// Writes a PDB out of a collected symbol table.
//
// A PDB is an MSF container, see UnitMSFWriter, whose streams have agreed
// meanings. The ones which matter for turning an address into a function and a
// source line are:
//
//   1  the information header, with the signature and age which have to match
//      the RSDS entry in the debug directory of the executable, otherwise a
//      debugger refuses the file as belonging to something else
//   2  types, left empty here, since none are described
//   3  the debug information, holding the module list, the section
//      contributions and the section map
//   4  identifiers, also left empty
//   5  the string table, named /names, which the line information indexes into
//      for file names
//   6  a copy of the section headers of the executable, which is how a
//      consumer turns a section and offset back into an address
//
// and then one stream per module, carrying its symbols and its line numbers.
//
// This is written from the published format description rather than from any
// existing implementation.
unit UnitPDBWriter;
{$ifdef fpc}
 {$mode delphi}
{$endif}

interface

uses SysUtils,
     Classes,
     PasVulkan.Types,
     UnitSymbolBuilder,
     UnitMSFWriter;

type TPDBSection=record
      Name:String;
      RVA:TpvUInt32;
      VirtualSize:TpvUInt32;
      RawSize:TpvUInt32;
      Characteristics:TpvUInt32;
     end;

     TPDBSections=array of TPDBSection;

     TPDBWriter=class
      private
       fBuilder:TSymbolBuilder;
       fMSF:TMSFWriter;
       fAge:TpvUInt32;
       fSignature:TpvUInt32;
       fGUID:array[0..15] of TpvUInt8;
       fSections:TPDBSections;
       fSectionCount:TpvSizeInt;
       // Turns an image relative address into the section and offset pair which
       // every record in a PDB is expressed in.
       function FindSection(const aRVA:TpvUInt64;out aSectionIndex:TpvUInt16;out aOffset:TpvUInt32):Boolean;
       procedure BuildSectionHeadersStream(const aStream:TMemoryStream);
       procedure WriteByte(const aStream:TMemoryStream;const aValue:TpvUInt8);
       procedure WriteUInt16(const aStream:TMemoryStream;const aValue:TpvUInt16);
       procedure WriteUInt32(const aStream:TMemoryStream;const aValue:TpvUInt32);
       procedure WriteInt32(const aStream:TMemoryStream;const aValue:TpvInt32);
       procedure BuildInfoStream(const aStream:TMemoryStream);
       procedure BuildTypeStream(const aStream:TMemoryStream);
       procedure BuildDebugInformationStream(const aStream:TMemoryStream;const aSectionHeadersStreamIndex:TpvUInt16;const aFirstModuleStreamIndex:TpvSizeInt);
      public
       constructor Create(const aBuilder:TSymbolBuilder);
       destructor Destroy; override;
       // The sections of the image, needed because a PDB addresses code as a
       // section and an offset rather than as an address.
       procedure AddSection(const aName:String;const aRVA,aVirtualSize,aRawSize,aCharacteristics:TpvUInt32);
       // The signature and age end up in the executable as well, so that the two
       // can be recognized as belonging together.
       procedure SetIdentity(const aSignature,aAge:TpvUInt32);
       procedure SaveToFile(const aFileName:String);
       property Age:TpvUInt32 read fAge;
       property Signature:TpvUInt32 read fSignature;
     end;

implementation

const PDBVersionVC70=TpvUInt32(20000404);

      TPIVersionV80=TpvUInt32(20040203);

      DBIVersionV70=TpvUInt32(19990903);

      DBIHeaderSize=64;

      TPIHeaderSize=56;

      IMAGE_FILE_MACHINE_AMD64=TpvUInt16($8664);

      // Tells a reader that this producer follows the conventions of the 14.x
      // toolchain, which is what every current consumer expects.
      FeatureCodeVC140=TpvUInt32(20140508);

constructor TPDBWriter.Create(const aBuilder:TSymbolBuilder);
var Index:TpvSizeInt;
begin
 inherited Create;
 fBuilder:=aBuilder;
 fMSF:=TMSFWriter.Create;
 fAge:=1;
 fSignature:=0;
 for Index:=0 to 15 do begin
  fGUID[Index]:=0;
 end;
end;

destructor TPDBWriter.Destroy;
begin
 FreeAndNil(fMSF);
 inherited Destroy;
end;

procedure TPDBWriter.AddSection(const aName:String;const aRVA,aVirtualSize,aRawSize,aCharacteristics:TpvUInt32);
begin
 if fSectionCount>=length(fSections) then begin
  SetLength(fSections,(fSectionCount+1)*2);
 end;
 fSections[fSectionCount].Name:=aName;
 fSections[fSectionCount].RVA:=aRVA;
 fSections[fSectionCount].VirtualSize:=aVirtualSize;
 fSections[fSectionCount].RawSize:=aRawSize;
 fSections[fSectionCount].Characteristics:=aCharacteristics;
 inc(fSectionCount);
end;

function TPDBWriter.FindSection(const aRVA:TpvUInt64;out aSectionIndex:TpvUInt16;out aOffset:TpvUInt32):Boolean;
var Index:TpvSizeInt;
begin
 result:=false;
 aSectionIndex:=0;
 aOffset:=0;
 for Index:=0 to fSectionCount-1 do begin
  if (aRVA>=fSections[Index].RVA) and
     (aRVA<(TpvUInt64(fSections[Index].RVA)+TpvUInt64(fSections[Index].VirtualSize))) then begin
   // Section numbers are one based everywhere in a PDB.
   aSectionIndex:=TpvUInt16(Index+1);
   aOffset:=TpvUInt32(aRVA-fSections[Index].RVA);
   result:=true;
   exit;
  end;
 end;
end;

// A verbatim copy of the section headers of the image. Without it a consumer
// cannot turn the section and offset pairs stored everywhere else back into an
// address.
procedure TPDBWriter.BuildSectionHeadersStream(const aStream:TMemoryStream);
var Index,NameIndex:TpvSizeInt;
    RawName:array[0..7] of AnsiChar;
    NameText:TpvRawByteString;
begin
 for Index:=0 to fSectionCount-1 do begin
  FillChar(RawName,SizeOf(RawName),#0);
  NameText:=TpvRawByteString(fSections[Index].Name);
  for NameIndex:=1 to length(NameText) do begin
   if NameIndex>8 then begin
    break;
   end;
   RawName[NameIndex-1]:=NameText[NameIndex];
  end;
  aStream.WriteBuffer(RawName,8);
  WriteUInt32(aStream,fSections[Index].VirtualSize);
  WriteUInt32(aStream,fSections[Index].RVA);
  WriteUInt32(aStream,fSections[Index].RawSize);
  WriteUInt32(aStream,0); // pointer to raw data, not meaningful here
  WriteUInt32(aStream,0); // pointer to relocations
  WriteUInt32(aStream,0); // pointer to line numbers
  WriteUInt16(aStream,0); // number of relocations
  WriteUInt16(aStream,0); // number of line numbers
  WriteUInt32(aStream,fSections[Index].Characteristics);
 end;
end;

procedure TPDBWriter.SetIdentity(const aSignature,aAge:TpvUInt32);
var Index:TpvSizeInt;
begin
 fSignature:=aSignature;
 fAge:=aAge;
 // A GUID derived from the signature, so that rebuilding the same input twice
 // yields the same identity and a stale PDB is recognized as stale.
 for Index:=0 to 15 do begin
  fGUID[Index]:=TpvUInt8((aSignature shr ((Index and 3) shl 3)) xor TpvUInt32(Index*37));
 end;
end;

procedure TPDBWriter.WriteByte(const aStream:TMemoryStream;const aValue:TpvUInt8);
begin
 aStream.WriteBuffer(aValue,SizeOf(TpvUInt8));
end;

procedure TPDBWriter.WriteUInt16(const aStream:TMemoryStream;const aValue:TpvUInt16);
begin
 aStream.WriteBuffer(aValue,SizeOf(TpvUInt16));
end;

procedure TPDBWriter.WriteUInt32(const aStream:TMemoryStream;const aValue:TpvUInt32);
begin
 aStream.WriteBuffer(aValue,SizeOf(TpvUInt32));
end;

procedure TPDBWriter.WriteInt32(const aStream:TMemoryStream;const aValue:TpvInt32);
begin
 aStream.WriteBuffer(aValue,SizeOf(TpvInt32));
end;

procedure TPDBWriter.BuildInfoStream(const aStream:TMemoryStream);
begin

 WriteUInt32(aStream,PDBVersionVC70);
 WriteUInt32(aStream,fSignature);
 WriteUInt32(aStream,fAge);
 aStream.WriteBuffer(fGUID,SizeOf(fGUID));

 // The named stream map, left empty for now. It is a string buffer followed by
 // a hash table, and an empty one is a zero length buffer with a hash table of
 // no entries.
 WriteUInt32(aStream,0); // string buffer size
 WriteUInt32(aStream,0); // hash table size
 WriteUInt32(aStream,1); // hash table capacity
 WriteUInt32(aStream,0); // present bit vector word count
 WriteUInt32(aStream,0); // deleted bit vector word count

 WriteUInt32(aStream,FeatureCodeVC140);

end;

procedure TPDBWriter.BuildTypeStream(const aStream:TMemoryStream);
begin
 WriteUInt32(aStream,TPIVersionV80);
 WriteUInt32(aStream,TPIHeaderSize);
 WriteUInt32(aStream,$1000); // first type index
 WriteUInt32(aStream,$1000); // one past the last, so none at all
 WriteUInt32(aStream,0);     // bytes of type records
 WriteUInt16(aStream,$ffff); // no hash stream
 WriteUInt16(aStream,$ffff); // no auxiliary hash stream
 WriteUInt32(aStream,4);     // hash key size
 WriteUInt32(aStream,$3ffff);// hash bucket count
 WriteInt32(aStream,0);      // hash value buffer offset
 WriteUInt32(aStream,0);     // hash value buffer length
 WriteInt32(aStream,0);      // index offset buffer offset
 WriteUInt32(aStream,0);     // index offset buffer length
 WriteInt32(aStream,0);      // hash adjustment buffer offset
 WriteUInt32(aStream,0);     // hash adjustment buffer length
end;

procedure TPDBWriter.BuildDebugInformationStream(const aStream:TMemoryStream;const aSectionHeadersStreamIndex:TpvUInt16;const aFirstModuleStreamIndex:TpvSizeInt);
var ModuleInfo,SectionContribution,SectionMap,SourceInfo,OptionalHeader:TMemoryStream;
    Index:TpvSizeInt;
    UnitRecord:TSymbolBuilder.TUnitRecord;
    SectionIndex:TpvUInt16;
    Offset:TpvUInt32;
    NameText:TpvRawByteString;
    Zero:AnsiChar;

 // Every record which describes a stretch of code uses this shape, both in the
 // module list and in the contribution list.
 procedure WriteContribution(const aTarget:TMemoryStream;const aSection:TpvUInt16;const aOffset,aSize:TpvUInt32;const aCharacteristics:TpvUInt32;const aModule:TpvUInt16);
 begin
  WriteUInt16(aTarget,aSection);
  WriteUInt16(aTarget,0);
  WriteUInt32(aTarget,aOffset);
  WriteUInt32(aTarget,aSize);
  WriteUInt32(aTarget,aCharacteristics);
  WriteUInt16(aTarget,aModule);
  WriteUInt16(aTarget,0);
  WriteUInt32(aTarget,0); // checksum of the data, not computed
  WriteUInt32(aTarget,0); // checksum of the relocations, not computed
 end;

 procedure WriteZeroTerminated(const aTarget:TMemoryStream;const aValue:String);
 begin
  NameText:=TpvRawByteString(aValue);
  if length(NameText)>0 then begin
   aTarget.WriteBuffer(NameText[1],length(NameText));
  end;
  Zero:=#0;
  aTarget.WriteBuffer(Zero,1);
 end;

 procedure PadToFour(const aTarget:TMemoryStream);
 begin
  Zero:=#0;
  while (aTarget.Size and 3)<>0 do begin
   aTarget.WriteBuffer(Zero,1);
  end;
 end;

begin

 ModuleInfo:=TMemoryStream.Create;
 SectionContribution:=TMemoryStream.Create;
 SectionMap:=TMemoryStream.Create;
 SourceInfo:=TMemoryStream.Create;
 OptionalHeader:=TMemoryStream.Create;
 try

  // One module per unit, each with its own stream carrying its symbols.
  for Index:=0 to fBuilder.UnitCount-1 do begin
   UnitRecord:=fBuilder.GetUnit(Index);
   if not FindSection(UnitRecord.StartRVA,SectionIndex,Offset) then begin
    SectionIndex:=0;
    Offset:=0;
   end;
   WriteUInt32(ModuleInfo,0); // unused
   WriteContribution(ModuleInfo,SectionIndex,Offset,TpvUInt32(UnitRecord.Size),0,TpvUInt16(Index));
   WriteUInt16(ModuleInfo,0); // flags
   WriteUInt16(ModuleInfo,TpvUInt16(aFirstModuleStreamIndex+Index));
   WriteUInt32(ModuleInfo,4); // bytes of symbols, just the signature so far
   WriteUInt32(ModuleInfo,0); // bytes of old style line information
   WriteUInt32(ModuleInfo,0); // bytes of current line information
   WriteUInt16(ModuleInfo,0); // number of source files
   WriteUInt16(ModuleInfo,0); // padding
   WriteUInt32(ModuleInfo,0); // unused
   WriteUInt32(ModuleInfo,0); // index of the source file name
   WriteUInt32(ModuleInfo,0); // index of the path of this pdb
   WriteZeroTerminated(ModuleInfo,String(UnitRecord.Name));
   WriteZeroTerminated(ModuleInfo,String(UnitRecord.Name));
   PadToFour(ModuleInfo);
  end;

  // The contributions repeat the same ranges, which is what a consumer scans to
  // find out which module an address belongs to.
  WriteUInt32(SectionContribution,$f12eba2d); // version marker
  for Index:=0 to fBuilder.UnitCount-1 do begin
   UnitRecord:=fBuilder.GetUnit(Index);
   if FindSection(UnitRecord.StartRVA,SectionIndex,Offset) then begin
    WriteContribution(SectionContribution,SectionIndex,Offset,TpvUInt32(UnitRecord.Size),0,TpvUInt16(Index));
   end;
  end;

  // The section map is a flat description of the segments, which predates the
  // section headers stream but is still expected to be there.
  WriteUInt16(SectionMap,TpvUInt16(fSectionCount));
  WriteUInt16(SectionMap,TpvUInt16(fSectionCount));
  for Index:=0 to fSectionCount-1 do begin
   WriteUInt16(SectionMap,$010d); // present, selector, addressable by 32 bits
   WriteUInt16(SectionMap,0);     // overlay
   WriteUInt16(SectionMap,0);     // group
   WriteUInt16(SectionMap,TpvUInt16(Index+1));
   WriteUInt16(SectionMap,$ffff); // no section name
   WriteUInt16(SectionMap,$ffff); // no class name
   WriteUInt32(SectionMap,0);     // offset within the group
   WriteUInt32(SectionMap,fSections[Index].VirtualSize);
  end;

  // No source file names are described yet, but the counts still have to be
  // there and consistent with the module count.
  WriteUInt16(SourceInfo,TpvUInt16(fBuilder.UnitCount));
  WriteUInt16(SourceInfo,0);
  for Index:=0 to fBuilder.UnitCount-1 do begin
   WriteUInt16(SourceInfo,TpvUInt16(Index));
  end;
  for Index:=0 to fBuilder.UnitCount-1 do begin
   WriteUInt16(SourceInfo,0);
  end;
  PadToFour(SourceInfo);

  // Eleven slots of stream indices, of which only the section headers, at index
  // five, is filled in here.
  for Index:=0 to 10 do begin
   if Index=5 then begin
    WriteUInt16(OptionalHeader,aSectionHeadersStreamIndex);
   end else begin
    WriteUInt16(OptionalHeader,$ffff);
   end;
  end;

  WriteInt32(aStream,-1);            // version signature
  WriteUInt32(aStream,DBIVersionV70);
  WriteUInt32(aStream,fAge);
  WriteUInt16(aStream,$ffff);        // global symbol stream, none yet
  WriteUInt16(aStream,$8e1d);        // toolchain build number
  WriteUInt16(aStream,$ffff);        // public symbol stream, none yet
  WriteUInt16(aStream,0);            // version of the producing dll
  WriteUInt16(aStream,$ffff);        // symbol record stream, none yet
  WriteUInt16(aStream,0);            // rebuild number of the producing dll

  WriteInt32(aStream,TpvInt32(ModuleInfo.Size));
  WriteInt32(aStream,TpvInt32(SectionContribution.Size));
  WriteInt32(aStream,TpvInt32(SectionMap.Size));
  WriteInt32(aStream,TpvInt32(SourceInfo.Size));
  WriteInt32(aStream,0);             // type server map substream
  WriteUInt32(aStream,0);            // index of the type server
  WriteInt32(aStream,TpvInt32(OptionalHeader.Size));
  WriteInt32(aStream,0);             // edit and continue substream

  WriteUInt16(aStream,0);            // flags
  WriteUInt16(aStream,IMAGE_FILE_MACHINE_AMD64);
  WriteUInt32(aStream,0);            // padding

  // The substreams follow the header in exactly this order.
  ModuleInfo.Position:=0;
  aStream.CopyFrom(ModuleInfo,ModuleInfo.Size);
  SectionContribution.Position:=0;
  aStream.CopyFrom(SectionContribution,SectionContribution.Size);
  SectionMap.Position:=0;
  aStream.CopyFrom(SectionMap,SectionMap.Size);
  SourceInfo.Position:=0;
  aStream.CopyFrom(SourceInfo,SourceInfo.Size);
  OptionalHeader.Position:=0;
  aStream.CopyFrom(OptionalHeader,OptionalHeader.Size);

 finally
  FreeAndNil(OptionalHeader);
  FreeAndNil(SourceInfo);
  FreeAndNil(SectionMap);
  FreeAndNil(SectionContribution);
  FreeAndNil(ModuleInfo);
 end;

end;

procedure TPDBWriter.SaveToFile(const aFileName:String);
var DebugInformation:TMemoryStream;
    SectionHeadersStreamIndex,FirstModuleStreamIndex:TpvSizeInt;
    Index:TpvSizeInt;
    ModuleStream:TMemoryStream;
begin

 // Stream zero is the previous stream directory, which a freshly written file
 // has none of, but the slot has to be there.
 fMSF.AddStream;

 BuildInfoStream(fMSF.AddStream);
 BuildTypeStream(fMSF.AddStream);

 // The debug information stream has to name the streams which follow it, so it
 // is created here and filled once their indices are known.
 DebugInformation:=fMSF.AddStream;

 BuildTypeStream(fMSF.AddStream); // identifiers, same shape as the types

 SectionHeadersStreamIndex:=fMSF.StreamCount;
 BuildSectionHeadersStream(fMSF.AddStream);

 FirstModuleStreamIndex:=fMSF.StreamCount;
 for Index:=0 to fBuilder.UnitCount-1 do begin
  ModuleStream:=fMSF.AddStream;
  // Only the marker saying that line information, where present, is in the
  // current format. Symbols and lines follow in the next step.
  WriteUInt32(ModuleStream,4);
  WriteUInt32(ModuleStream,0); // size of the global references block
 end;

 BuildDebugInformationStream(DebugInformation,TpvUInt16(SectionHeadersStreamIndex),FirstModuleStreamIndex);

 fMSF.SaveToFile(aFileName);

end;

end.
