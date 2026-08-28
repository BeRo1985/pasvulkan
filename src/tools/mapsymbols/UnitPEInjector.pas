// Adds sections to an existing PE executable.
//
// This is the alternative to writing a separate debug file: the DWARF sections
// go straight into the .exe, so that addr2line, gdb and the rest work on the
// executable itself with nothing beside it.
//
// It is the more invasive of the two, which is why it is optional. Three things
// have to be dealt with:
//
//   - The section header table needs room for the new entries. It sits between
//     the optional header and the start of the first section data, and that gap
//     is whatever the linker happened to leave. Delphi leaves 64 bytes, which is
//     not enough for three more sections, so the header area is grown and every
//     section moves along in the file by that amount. Only file offsets change,
//     the virtual addresses stay put, so the image as loaded is identical.
//   - Section names longer than eight characters live in the COFF string table
//     and the header only holds a slash and a decimal offset. Every DWARF
//     section name is too long, so a string table has to exist. A Delphi build
//     has none, so one is created.
//   - SizeOfImage covers the new sections, since they are given real addresses
//     rather than being left floating.
//
// A signed executable is refused rather than silently invalidated, and so is one
// which already carries a COFF symbol table, since rewriting an existing string
// table would mean moving whatever sits behind it.
unit UnitPEInjector;
{$ifdef fpc}
 {$mode delphi}
{$endif}

interface

uses SysUtils,
     Classes,
     PasVulkan.Types;

type TPEInjectorSection=record
      Name:String;
      Data:TMemoryStream;
      // A debug directory section is placed like any other, but afterwards the
      // addresses inside it and the data directory of the image have to be made
      // to point at it.
      IsDebugDirectory:Boolean;
     end;

     TPEInjectorSections=array of TPEInjectorSection;

     TPEInjector=class
      private
       fSections:TPEInjectorSections;
       fSectionCount:TpvSizeInt;
       fMessage:String;
      public
       constructor Create;
       destructor Destroy; override;
       procedure AddSection(const aName:String;const aData:TMemoryStream);
       // Adds the debug directory which names an accompanying PDB. Without it a
       // debugger has no way to tell that the PDB belongs to this image, and
       // will not load it. Delphi emits none of its own.
       procedure AddCodeViewDirectory(const aGUID:TpvPointer;const aAge:TpvUInt32;const aPDBFileName:String;const aTimeStamp:TpvUInt32);
       // Returns false and leaves the file untouched when the image is one this
       // must not touch. The reason is then in Message.
       function InjectInto(const aFileName:String):Boolean;
       property Message:String read fMessage;
     end;

implementation

const IMAGE_SCN_CNT_INITIALIZED_DATA=TpvUInt32($00000040);
      IMAGE_SCN_MEM_DISCARDABLE=TpvUInt32($02000000);
      IMAGE_SCN_MEM_READ=TpvUInt32($40000000);

      SectionHeaderSize=40;
      DebugDirectoryEntrySize=28;
      IMAGE_DEBUG_TYPE_CODEVIEW=TpvUInt32(2);

      // The magic of the symbol table this tool appends. Known here so that a
      // second run can tell the difference between a block it wrote itself and
      // something else somebody put behind the sections.
      AppendedTableMagic='PVSYMTAB';

type TExistingSection=record
      Name:String;
      // The forty bytes of the header as they stand, so that a section which is
      // kept is written back exactly as it was apart from its file offset.
      Raw:array[0..SectionHeaderSize-1] of TpvUInt8;
      VirtualAddress:TpvUInt32;
      VirtualSize:TpvUInt32;
      RawPointer:TpvUInt32;
      RawSize:TpvUInt32;
      // Set for a section which one of the new ones replaces, which is what
      // makes running this twice over the same executable work.
      Dropped:Boolean;
     end;

     TExistingSections=array of TExistingSection;

     // One entry of the debug directory the image already had. Kept so that
     // adding one of our own does not throw the others away.
     TExistingDebugEntry=record
      Raw:array[0..DebugDirectoryEntrySize-1] of TpvUInt8;
      EntryType:TpvUInt32;
      RawPointer:TpvUInt32;
      // Which slot of the directory this came out of. Kept because the entries
      // which survive are not the whole directory, so their position in this
      // list is not their position in the file.
      SourceIndex:TpvSizeInt;
     end;

     TExistingDebugEntries=array of TExistingDebugEntry;

// The name of a section as it stands in its header, following the slash and
// decimal offset into the string table where the name did not fit into the
// eight bytes of the header itself.
function SectionName(const aRaw:array of TpvUInt8;const aStringTable:TMemoryStream):String;
var Length_,Offset,Terminator:TpvSizeInt;
    Bytes:PpvUInt8Array;
begin
 Length_:=0;
 while (Length_<8) and (aRaw[Length_]<>0) do begin
  inc(Length_);
 end;
 SetString(result,PAnsiChar(@aRaw[0]),Length_);
 if (Length_>1) and (result[1]='/') and assigned(aStringTable) then begin
  Offset:=StrToIntDef(Copy(result,2,Length_-1),-1);
  if (Offset>=4) and (Offset<TpvSizeInt(aStringTable.Size)) then begin
   Bytes:=PpvUInt8Array(aStringTable.Memory);
   Terminator:=Offset;
   while (Terminator<TpvSizeInt(aStringTable.Size)) and (Bytes^[Terminator]<>0) do begin
    inc(Terminator);
   end;
   SetString(result,PAnsiChar(@Bytes^[Offset]),Terminator-Offset);
  end;
 end;
end;

// The ordinary reflected CRC32 over a stretch of a stream. Used to hold the
// bytes of a section against themselves across the rewrite.
function StreamCRC32(const aStream:TStream;const aOffset,aSize:TpvInt64):TpvUInt32;
const Polynomial=TpvUInt32($edb88320);
var Table:array[0..255] of TpvUInt32;
    Index,Bit:TpvInt32;
    Value:TpvUInt32;
    Buffer:array[0..65535] of TpvUInt8;
    Remaining:TpvInt64;
    Chunk,Position:TpvInt32;
begin
 for Index:=0 to 255 do begin
  Value:=TpvUInt32(Index);
  for Bit:=0 to 7 do begin
   if (Value and 1)<>0 then begin
    Value:=(Value shr 1) xor Polynomial;
   end else begin
    Value:=Value shr 1;
   end;
  end;
  Table[Index]:=Value;
 end;
 Value:=TpvUInt32($ffffffff);
 aStream.Seek(aOffset,soBeginning);
 Remaining:=aSize;
 while Remaining>0 do begin
  if Remaining>TpvInt64(SizeOf(Buffer)) then begin
   Chunk:=SizeOf(Buffer);
  end else begin
   Chunk:=TpvInt32(Remaining);
  end;
  Chunk:=aStream.Read(Buffer,Chunk);
  if Chunk<=0 then begin
   break;
  end;
  for Position:=0 to Chunk-1 do begin
   Value:=(Value shr 8) xor Table[(Value xor TpvUInt32(Buffer[Position])) and $ff];
  end;
  dec(Remaining,Chunk);
 end;
 result:=Value xor TpvUInt32($ffffffff);
end;

function AlignUp(const aValue,aAlignment:TpvUInt64):TpvUInt64;
begin
 if aAlignment<=1 then begin
  result:=aValue;
 end else begin
  result:=((aValue+(aAlignment-1)) div aAlignment)*aAlignment;
 end;
end;

constructor TPEInjector.Create;
begin
 inherited Create;
 fSections:=nil;
 fSectionCount:=0;
 fMessage:='';
end;

destructor TPEInjector.Destroy;
var Index:TpvSizeInt;
begin
 // Only the debug directory stream was created here, the rest belongs to the
 // caller.
 for Index:=0 to fSectionCount-1 do begin
  if fSections[Index].IsDebugDirectory then begin
   FreeAndNil(fSections[Index].Data);
  end;
 end;
 fSections:=nil;
 inherited Destroy;
end;

procedure TPEInjector.AddSection(const aName:String;const aData:TMemoryStream);
begin
 if assigned(aData) and (aData.Size>0) then begin
  if fSectionCount>=length(fSections) then begin
   SetLength(fSections,(fSectionCount+1)*2);
  end;
  fSections[fSectionCount].Name:=aName;
  fSections[fSectionCount].Data:=aData;
  fSections[fSectionCount].IsDebugDirectory:=false;
  inc(fSectionCount);
 end;
end;

procedure TPEInjector.AddCodeViewDirectory(const aGUID:TpvPointer;const aAge:TpvUInt32;const aPDBFileName:String;const aTimeStamp:TpvUInt32);
const IMAGE_DEBUG_TYPE_CODEVIEW=TpvUInt32(2);
var Data:TMemoryStream;
    Value32:TpvUInt32;
    Value16:TpvUInt16;
    NameText:TpvRawByteString;
    Terminator:AnsiChar;
    RecordSize:TpvUInt32;
begin

 NameText:=TpvRawByteString(aPDBFileName);
 // Four bytes of signature, the guid, the age, and the name with its terminator.
 RecordSize:=4+16+4+TpvUInt32(length(NameText))+1;

 Data:=TMemoryStream.Create;

 // The directory entry itself. The two addresses are filled in during injection,
 // once it is known where this lands.
 Value32:=0;
 Data.WriteBuffer(Value32,SizeOf(TpvUInt32)); // characteristics
 Data.WriteBuffer(aTimeStamp,SizeOf(TpvUInt32));
 Value16:=0;
 Data.WriteBuffer(Value16,SizeOf(TpvUInt16)); // major version
 Data.WriteBuffer(Value16,SizeOf(TpvUInt16)); // minor version
 Value32:=IMAGE_DEBUG_TYPE_CODEVIEW;
 Data.WriteBuffer(Value32,SizeOf(TpvUInt32));
 Data.WriteBuffer(RecordSize,SizeOf(TpvUInt32));
 Value32:=0;
 Data.WriteBuffer(Value32,SizeOf(TpvUInt32)); // address of the record, patched later
 Data.WriteBuffer(Value32,SizeOf(TpvUInt32)); // file offset of the record, patched later

 // The CodeView record which names the pdb.
 NameText:=TpvRawByteString('RSDS');
 Data.WriteBuffer(NameText[1],4);
 Data.WriteBuffer(aGUID^,16);
 Data.WriteBuffer(aAge,SizeOf(TpvUInt32));
 NameText:=TpvRawByteString(aPDBFileName);
 if length(NameText)>0 then begin
  Data.WriteBuffer(NameText[1],length(NameText));
 end;
 Terminator:=#0;
 Data.WriteBuffer(Terminator,1);

 if fSectionCount>=length(fSections) then begin
  SetLength(fSections,(fSectionCount+1)*2);
 end;
 fSections[fSectionCount].Name:='.debug';
 fSections[fSectionCount].Data:=Data;
 fSections[fSectionCount].IsDebugDirectory:=true;
 inc(fSectionCount);

end;

function TPEInjector.InjectInto(const aFileName:String):Boolean;
var Source,Target:TFileStream;
    TargetName,BackupName:String;
    NewHeaderOffset:TpvUInt32;
    Signature:array[0..3] of AnsiChar;
    NumberOfSections:TpvUInt16;
    SizeOfOptionalHeader:TpvUInt16;
    OptionalMagic:TpvUInt16;
    SymbolTablePointer,SymbolCount:TpvUInt32;
    SectionAlignment,FileAlignment,SizeOfImage,SizeOfHeaders:TpvUInt32;
    CertificateAddress,CertificateSize:TpvUInt32;
    SectionHeaderTableOffset:TpvInt64;
    NewSizeOfHeaders,Delta:TpvUInt32;
    Index:TpvSizeInt;
    VirtualAddress,VirtualSize,RawPointer:TpvUInt32;
    NextRVA,DataOffset,StringTableOffset:TpvUInt64;
    StringTable:TMemoryStream;
    NameOffsets:array of TpvUInt32;
    RawName:array[0..7] of AnsiChar;
    Value32:TpvUInt32;
    Value16:TpvUInt16;
    NameText:TpvRawByteString;
    Zero:AnsiChar;
    Padding:array[0..4095] of AnsiChar;
    SizeOfImageOffset,SizeOfHeadersOffset,DataDirectoryOffset:TpvInt64;
    NumberOfRvaAndSizes:TpvUInt32;
    ExistingSections:TExistingSections;
    KeptCount,DroppedCount,NewIndex:TpvSizeInt;
    OldStringTable:TMemoryStream;
    OldStringTableSize:TpvUInt32;
    BodyEnd,KnownEnd:TpvInt64;
    NeedStringTable,KeepOldStringTable:Boolean;
    NewSectionHeaderTableEnd:TpvInt64;
    DebugDirectoryAddress,DebugDirectorySize:TpvUInt32;
    ExistingDebugEntries:TExistingDebugEntries;
    ExistingDebugCount,LostDebugCount:TpvSizeInt;
    HaveCodeView:Boolean;
    FooterMagic:array[0..7] of AnsiChar;
    FooterOffset:TpvUInt64;
    DebugEntryCount:TpvSizeInt;
    EntryOffset:TpvInt64;
    MergedDirectory:TMemoryStream;
    Corrupted:String;

 // Where an address relative to the image base lands in the source file, going
 // by the sections as they were before anything moved.
 function SourceOffsetOfRVA(const aRVA:TpvUInt32):TpvInt64;
 var Scan:TpvSizeInt;
     Size:TpvUInt32;
 begin
  result:=0;
  for Scan:=0 to length(ExistingSections)-1 do begin
   if ExistingSections[Scan].RawSize=0 then begin
    continue;
   end;
   if ExistingSections[Scan].VirtualSize>0 then begin
    Size:=ExistingSections[Scan].VirtualSize;
   end else begin
    Size:=ExistingSections[Scan].RawSize;
   end;
   if (aRVA>=ExistingSections[Scan].VirtualAddress) and (aRVA<(ExistingSections[Scan].VirtualAddress+Size)) then begin
    result:=TpvInt64(ExistingSections[Scan].RawPointer)+TpvInt64(aRVA-ExistingSections[Scan].VirtualAddress);
    exit;
   end;
  end;
 end;

 // Whether a file offset falls inside a section which is being kept, which is
 // what decides whether whatever sits there survives this.
 function InsideKeptSection(const aOffset:TpvUInt32):Boolean;
 var Scan:TpvSizeInt;
 begin
  result:=false;
  for Scan:=0 to length(ExistingSections)-1 do begin
   if ExistingSections[Scan].Dropped or (ExistingSections[Scan].RawSize=0) then begin
    continue;
   end;
   if (aOffset>=ExistingSections[Scan].RawPointer) and
      (aOffset<(ExistingSections[Scan].RawPointer+ExistingSections[Scan].RawSize)) then begin
    result:=true;
    exit;
   end;
  end;
 end;

 procedure PadTo(const aStream:TFileStream;const aPosition:TpvInt64);
 var Remaining:TpvInt64;
 begin
  Remaining:=aPosition-aStream.Position;
  while Remaining>0 do begin
   if Remaining>TpvInt64(SizeOf(Padding)) then begin
    aStream.WriteBuffer(Padding,SizeOf(Padding));
    dec(Remaining,SizeOf(Padding));
   end else begin
    aStream.WriteBuffer(Padding,Remaining);
    Remaining:=0;
   end;
  end;
 end;

begin

 result:=false;
 fMessage:='';

 if fSectionCount=0 then begin
  fMessage:='Nothing to inject.';
  exit;
 end;

 FillChar(Padding,SizeOf(Padding),#0);
 Zero:=#0;
 Delta:=0;
 ExistingSections:=nil;
 ExistingDebugEntries:=nil;
 ExistingDebugCount:=0;
 LostDebugCount:=0;
 OldStringTable:=nil;

 HaveCodeView:=false;
 for Index:=0 to fSectionCount-1 do begin
  if fSections[Index].IsDebugDirectory then begin
   HaveCodeView:=true;
   break;
  end;
 end;

 StringTable:=TMemoryStream.Create;
 try

  Source:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  try

   Source.Seek(TpvInt64($3c),soBeginning);
   Source.ReadBuffer(NewHeaderOffset,SizeOf(TpvUInt32));
   Source.Seek(TpvInt64(NewHeaderOffset),soBeginning);
   Source.ReadBuffer(Signature,4);
   if (Signature[0]<>'P') or (Signature[1]<>'E') or (Signature[2]<>#0) or (Signature[3]<>#0) then begin
    fMessage:='Not a PE executable.';
    exit;
   end;

   Source.Seek(TpvInt64(NewHeaderOffset)+6,soBeginning);
   Source.ReadBuffer(NumberOfSections,SizeOf(TpvUInt16));
   Source.Seek(TpvInt64(NewHeaderOffset)+12,soBeginning);
   Source.ReadBuffer(SymbolTablePointer,SizeOf(TpvUInt32));
   Source.ReadBuffer(SymbolCount,SizeOf(TpvUInt32));
   Source.ReadBuffer(SizeOfOptionalHeader,SizeOf(TpvUInt16));

   // A real symbol table is refused: rebuilding one means understanding every
   // entry in it, and the string table behind it cannot be moved without that.
   // A pointer with no symbols behind it is a different thing entirely, it is a
   // string table on its own, which is what this tool leaves behind when it
   // writes section names longer than eight characters. That one is rebuilt.
   if (SymbolTablePointer<>0) and (SymbolCount<>0) then begin
    fMessage:='The executable has a COFF symbol table, which this does not rewrite.';
    exit;
   end;

   Source.Seek(TpvInt64(NewHeaderOffset)+24,soBeginning);
   Source.ReadBuffer(OptionalMagic,SizeOf(TpvUInt16));
   if (OptionalMagic<>$20b) and (OptionalMagic<>$10b) then begin
    fMessage:='Unknown optional header format.';
    exit;
   end;

   Source.Seek(TpvInt64(NewHeaderOffset)+24+32,soBeginning);
   Source.ReadBuffer(SectionAlignment,SizeOf(TpvUInt32));
   Source.ReadBuffer(FileAlignment,SizeOf(TpvUInt32));
   SizeOfImageOffset:=TpvInt64(NewHeaderOffset)+24+56;
   Source.Seek(SizeOfImageOffset,soBeginning);
   Source.ReadBuffer(SizeOfImage,SizeOf(TpvUInt32));
   SizeOfHeadersOffset:=TpvInt64(NewHeaderOffset)+24+60;
   Source.Seek(SizeOfHeadersOffset,soBeginning);
   Source.ReadBuffer(SizeOfHeaders,SizeOf(TpvUInt32));

   // The certificate table is the one data directory holding a file offset
   // rather than an address, so moving anything invalidates it. A signed binary
   // would lose its signature either way, so this refuses instead.
   if OptionalMagic=$20b then begin
    DataDirectoryOffset:=TpvInt64(NewHeaderOffset)+24+112;
   end else begin
    DataDirectoryOffset:=TpvInt64(NewHeaderOffset)+24+96;
   end;
   // How many of them there actually are. Sixteen is what every real linker
   // writes, but the count is a field rather than a constant, and reading the
   // seventh of six would be reading the section headers as if they were one.
   Source.Seek(DataDirectoryOffset-4,soBeginning);
   Source.ReadBuffer(NumberOfRvaAndSizes,SizeOf(TpvUInt32));
   if NumberOfRvaAndSizes>16 then begin
    NumberOfRvaAndSizes:=16;
   end;
   if NumberOfRvaAndSizes>4 then begin
    Source.Seek(DataDirectoryOffset+(4*8),soBeginning);
    Source.ReadBuffer(CertificateAddress,SizeOf(TpvUInt32));
    Source.ReadBuffer(CertificateSize,SizeOf(TpvUInt32));
   end else begin
    CertificateAddress:=0;
    CertificateSize:=0;
   end;
   if (CertificateAddress<>0) or (CertificateSize<>0) then begin
    fMessage:='The executable is signed, which this would invalidate.';
    exit;
   end;
   if NumberOfRvaAndSizes<7 then begin
    // Without a debug directory entry in the table there is nowhere to say that
    // this image has one, and growing the table means moving the section header
    // table, which is a different job than this one.
    if HaveCodeView then begin
     fMessage:='The optional header has no room for a debug directory entry.';
     exit;
    end;
    DebugDirectoryAddress:=0;
    DebugDirectorySize:=0;
   end else begin
    Source.Seek(DataDirectoryOffset+(6*8),soBeginning);
    Source.ReadBuffer(DebugDirectoryAddress,SizeOf(TpvUInt32));
    Source.ReadBuffer(DebugDirectorySize,SizeOf(TpvUInt32));
   end;

   SectionHeaderTableOffset:=TpvInt64(NewHeaderOffset)+24+TpvInt64(SizeOfOptionalHeader);

   // The string table which is already there, if there is one. It is needed to
   // read the names of the existing sections, since a name longer than eight
   // characters only lives there, and those are exactly the sections an earlier
   // run of this added.
   OldStringTableSize:=0;
   if SymbolTablePointer<>0 then begin
    if (TpvInt64(SymbolTablePointer)+4)>Source.Size then begin
     fMessage:='The string table of the executable points past its end.';
     exit;
    end;
    Source.Seek(TpvInt64(SymbolTablePointer),soBeginning);
    Source.ReadBuffer(OldStringTableSize,SizeOf(TpvUInt32));
    if (OldStringTableSize<4) or ((TpvInt64(SymbolTablePointer)+TpvInt64(OldStringTableSize))>Source.Size) then begin
     fMessage:='The string table of the executable states a size which does not fit.';
     exit;
    end;
    OldStringTable:=TMemoryStream.Create;
    Source.Seek(TpvInt64(SymbolTablePointer),soBeginning);
    OldStringTable.CopyFrom(Source,TpvInt64(OldStringTableSize));
   end;

   // Read the section table as it stands, names resolved, so that the ones this
   // run replaces can be told apart from the ones which have to survive.
   SetLength(ExistingSections,NumberOfSections);
   for Index:=0 to NumberOfSections-1 do begin
    Source.Seek(SectionHeaderTableOffset+(TpvInt64(Index)*SectionHeaderSize),soBeginning);
    Source.ReadBuffer(ExistingSections[Index].Raw[0],SectionHeaderSize);
    Move(ExistingSections[Index].Raw[8],ExistingSections[Index].VirtualSize,SizeOf(TpvUInt32));
    Move(ExistingSections[Index].Raw[12],ExistingSections[Index].VirtualAddress,SizeOf(TpvUInt32));
    Move(ExistingSections[Index].Raw[16],ExistingSections[Index].RawSize,SizeOf(TpvUInt32));
    Move(ExistingSections[Index].Raw[20],ExistingSections[Index].RawPointer,SizeOf(TpvUInt32));
    ExistingSections[Index].Name:=SectionName(ExistingSections[Index].Raw,OldStringTable);
    ExistingSections[Index].Dropped:=false;
   end;

   // A section which one of the new ones is called after is replaced rather
   // than added beside. That is what makes a second run over the same
   // executable do what the first one did instead of stacking another copy of
   // everything behind it.
   DroppedCount:=0;
   for Index:=0 to NumberOfSections-1 do begin
    for NewIndex:=0 to fSectionCount-1 do begin
     if ExistingSections[Index].Name=fSections[NewIndex].Name then begin
      ExistingSections[Index].Dropped:=true;
      inc(DroppedCount);
      break;
     end;
    end;
   end;
   KeptCount:=NumberOfSections-DroppedCount;

   // Where the body of the file ends once the replaced sections and the old
   // string table are gone. Everything from there on is reused space rather
   // than copied through, which is what keeps a second run from growing the
   // file by a copy of everything it wrote the first time.
   BodyEnd:=TpvInt64(SizeOfHeaders);
   for Index:=0 to NumberOfSections-1 do begin
    if (not ExistingSections[Index].Dropped) and (ExistingSections[Index].RawPointer<>0) and
       ((TpvInt64(ExistingSections[Index].RawPointer)+TpvInt64(ExistingSections[Index].RawSize))>BodyEnd) then begin
     BodyEnd:=TpvInt64(ExistingSections[Index].RawPointer)+TpvInt64(ExistingSections[Index].RawSize);
    end;
   end;

   // But only when everything being dropped really is behind it. What lies
   // past the last kept section has to be accounted for, or this would throw
   // away something somebody else put there. Known are the sections being
   // replaced, the old string table, and the table this tool appends, which is
   // recognized by the magic in its footer.
   KnownEnd:=BodyEnd;
   for Index:=0 to NumberOfSections-1 do begin
    if ExistingSections[Index].Dropped and (ExistingSections[Index].RawPointer<>0) then begin
     if TpvInt64(ExistingSections[Index].RawPointer)<BodyEnd then begin
      fMessage:='A section which would be replaced sits in front of one which has to stay.';
      exit;
     end;
     if (TpvInt64(ExistingSections[Index].RawPointer)+TpvInt64(ExistingSections[Index].RawSize))>KnownEnd then begin
      KnownEnd:=TpvInt64(ExistingSections[Index].RawPointer)+TpvInt64(ExistingSections[Index].RawSize);
     end;
    end;
   end;
   if SymbolTablePointer<>0 then begin
    if TpvInt64(SymbolTablePointer)<BodyEnd then begin
     fMessage:='The string table of the executable sits in front of its own sections.';
     exit;
    end;
    if (TpvInt64(SymbolTablePointer)+TpvInt64(OldStringTableSize))>KnownEnd then begin
     KnownEnd:=TpvInt64(SymbolTablePointer)+TpvInt64(OldStringTableSize);
    end;
   end;
   if Source.Size>KnownEnd then begin
    // Whatever is left has to be the appended symbol table, which the run this
    // is part of writes again anyway. Anything else and this stops rather than
    // discarding it.
    FooterOffset:=0;
    if Source.Size>=(KnownEnd+16) then begin
     Source.Seek(Source.Size-16,soBeginning);
     Source.ReadBuffer(FooterMagic,8);
     Source.ReadBuffer(FooterOffset,SizeOf(TpvUInt64));
    end else begin
     FillChar(FooterMagic,SizeOf(FooterMagic),#0);
    end;
    if (FooterMagic<>AppendedTableMagic) or (TpvInt64(FooterOffset)<KnownEnd) or
       (TpvInt64(FooterOffset)>=Source.Size) then begin
     fMessage:='There is something behind the sections of the executable which this does not recognize.';
     exit;
    end;
   end;

   // The debug directory the image already has. Its entries are kept, because
   // replacing the whole directory with one entry of our own would throw away
   // whatever else was in it, and because each of them holds a file offset to
   // its own payload which moves when the header area grows. Both of those are
   // silent losses otherwise: the image stays valid and says less than it did.
   //
   // An entry whose payload is not inside a section which survives is left out,
   // since what it points at is about to stop existing.
   ExistingDebugCount:=0;
   if (DebugDirectoryAddress<>0) and (DebugDirectorySize>=DebugDirectoryEntrySize) then begin
    EntryOffset:=SourceOffsetOfRVA(DebugDirectoryAddress);
    if (EntryOffset>0) and ((EntryOffset+TpvInt64(DebugDirectorySize))<=Source.Size) then begin
     DebugEntryCount:=DebugDirectorySize div DebugDirectoryEntrySize;
     SetLength(ExistingDebugEntries,DebugEntryCount);
     for Index:=0 to DebugEntryCount-1 do begin
      Source.Seek(EntryOffset+(TpvInt64(Index)*DebugDirectoryEntrySize),soBeginning);
      Source.ReadBuffer(ExistingDebugEntries[ExistingDebugCount].Raw[0],DebugDirectoryEntrySize);
      Move(ExistingDebugEntries[ExistingDebugCount].Raw[12],ExistingDebugEntries[ExistingDebugCount].EntryType,SizeOf(TpvUInt32));
      Move(ExistingDebugEntries[ExistingDebugCount].Raw[24],ExistingDebugEntries[ExistingDebugCount].RawPointer,SizeOf(TpvUInt32));
      ExistingDebugEntries[ExistingDebugCount].SourceIndex:=Index;
      // One of ours replaces any codeview entry which is there, since two of
      // them would leave a debugger to pick.
      if HaveCodeView and (ExistingDebugEntries[ExistingDebugCount].EntryType=IMAGE_DEBUG_TYPE_CODEVIEW) then begin
       continue;
      end;
      if (ExistingDebugEntries[ExistingDebugCount].RawPointer<>0) and
         not InsideKeptSection(ExistingDebugEntries[ExistingDebugCount].RawPointer) then begin
       // Its payload is in a section which is being replaced, so there is
       // nothing left for it to point at. Counted and said out loud rather than
       // dropped quietly, since it is the one thing here which is lost.
       inc(LostDebugCount);
       continue;
      end;
      inc(ExistingDebugCount);
     end;
    end;
   end;

   // An entry whose payload is going away can only be left out where the whole
   // directory is written again, which is what adding one of our own does.
   // Without that the directory stays where it is and is only nudged along, so
   // the entry would remain in it, still counted by its size, pointing at a
   // section which no longer exists. Saying it was dropped would then be untrue
   // twice over, so this stops instead of half doing it.
   if (LostDebugCount>0) and not HaveCodeView then begin
    fMessage:='The debug directory of the executable points into a section which would be replaced, and there is no way to rewrite it here.';
    exit;
   end;

   NewSectionHeaderTableEnd:=SectionHeaderTableOffset+(TpvInt64(KeptCount)*SectionHeaderSize);

   // Grow the header area when the new section headers do not fit. Delphi
   // leaves only 64 bytes of slack, so this is the normal case rather than the
   // exception, and every section then moves along in the file by that amount.
   NewSizeOfHeaders:=TpvUInt32(AlignUp(TpvUInt64(NewSectionHeaderTableEnd+(TpvInt64(fSectionCount)*SectionHeaderSize)),FileAlignment));
   if NewSizeOfHeaders<SizeOfHeaders then begin
    NewSizeOfHeaders:=SizeOfHeaders;
   end;
   Delta:=NewSizeOfHeaders-SizeOfHeaders;

   // Where the new sections go in the address space, which is behind everything
   // the image still covers once the replaced ones are gone. Their addresses
   // are given up with them, so a second run reuses the same range rather than
   // pushing the image further out every time.
   NextRVA:=0;
   for Index:=0 to NumberOfSections-1 do begin
    if ExistingSections[Index].Dropped then begin
     continue;
    end;
    if (TpvUInt64(ExistingSections[Index].VirtualAddress)+TpvUInt64(ExistingSections[Index].VirtualSize))>NextRVA then begin
     NextRVA:=TpvUInt64(ExistingSections[Index].VirtualAddress)+TpvUInt64(ExistingSections[Index].VirtualSize);
    end;
   end;
   NextRVA:=AlignUp(NextRVA,SectionAlignment);

   // A name of eight characters or fewer goes into the header itself, which is
   // what a PE is meant to look like. Only a longer one needs the string table,
   // which is a gnu extension that gdb relies on for the debug sections and
   // which nothing else here should have to pay for: adding only the entry
   // which names a pdb leaves an ordinary executable with no string table at
   // all, and one which can therefore be built over again without trouble.
   //
   // But a section which stays and whose name is already in the old table needs
   // that table to go on existing, and to go on saying the same thing at the
   // same offsets. Its header is written back as it stands, slash and decimal
   // offset included, so dropping the table would leave that name pointing at
   // nothing, and building a fresh one would leave it pointing at whichever
   // name happened to land on that offset instead. Neither is a section this
   // run was asked to touch.
   KeepOldStringTable:=false;
   for Index:=0 to NumberOfSections-1 do begin
    if (not ExistingSections[Index].Dropped) and (ExistingSections[Index].Raw[0]=TpvUInt8(Ord('/'))) then begin
     KeepOldStringTable:=true;
     break;
    end;
   end;
   if KeepOldStringTable and not assigned(OldStringTable) then begin
    fMessage:='A section of the executable names itself through a string table which is not there.';
    exit;
   end;

   NeedStringTable:=KeepOldStringTable;
   for Index:=0 to fSectionCount-1 do begin
    if length(fSections[Index].Name)>8 then begin
     NeedStringTable:=true;
     break;
    end;
   end;

   SetLength(NameOffsets,fSectionCount);
   for Index:=0 to fSectionCount-1 do begin
    NameOffsets[Index]:=0;
   end;
   if NeedStringTable then begin
    if KeepOldStringTable then begin
     // The old one first and unchanged, so every offset already written into a
     // header still lands on the name it landed on before. The new names go
     // behind it.
     OldStringTable.Position:=0;
     StringTable.CopyFrom(OldStringTable,OldStringTable.Size);
    end else begin
     // The table starts with its own size, and offsets are counted from its
     // beginning, so the first name sits at four.
     Value32:=0;
     StringTable.WriteBuffer(Value32,SizeOf(TpvUInt32));
    end;
    for Index:=0 to fSectionCount-1 do begin
     if length(fSections[Index].Name)>8 then begin
      NameOffsets[Index]:=TpvUInt32(StringTable.Position);
      NameText:=TpvRawByteString(fSections[Index].Name);
      StringTable.WriteBuffer(NameText[1],length(NameText));
      StringTable.WriteBuffer(Zero,1);
     end;
    end;
    Value32:=TpvUInt32(StringTable.Size);
    StringTable.Position:=0;
    StringTable.WriteBuffer(Value32,SizeOf(TpvUInt32));
   end;

   // Everything is written into a new file rather than in place, because the
   // header area may have to grow, which shifts every section behind it.
   TargetName:=aFileName+'.mapsymbols-tmp';
   Target:=TFileStream.Create(TargetName,fmCreate);
   try

    Source.Seek(0,soBeginning);
    Target.CopyFrom(Source,TpvInt64(SizeOfHeaders));

    Target.Seek(TpvInt64(NewHeaderOffset)+6,soBeginning);
    Value16:=TpvUInt16(TpvInt64(KeptCount)+fSectionCount);
    Target.WriteBuffer(Value16,SizeOf(TpvUInt16));
    Target.Seek(SizeOfHeadersOffset,soBeginning);
    Target.WriteBuffer(NewSizeOfHeaders,SizeOf(TpvUInt32));

    // Write the section table again from the ones which stay, so that the
    // entries of the replaced ones are gone rather than left pointing at bytes
    // nothing writes any more. Their virtual addresses are untouched and only
    // the file offset moves, by whatever the header area gained, so the image
    // in memory is the same one.
    NewIndex:=0;
    for Index:=0 to NumberOfSections-1 do begin
     if ExistingSections[Index].Dropped then begin
      continue;
     end;
     if ExistingSections[Index].RawPointer<>0 then begin
      Value32:=ExistingSections[Index].RawPointer+Delta;
      Move(Value32,ExistingSections[Index].Raw[20],SizeOf(TpvUInt32));
     end;
     Target.Seek(SectionHeaderTableOffset+(TpvInt64(NewIndex)*SectionHeaderSize),soBeginning);
     Target.WriteBuffer(ExistingSections[Index].Raw[0],SectionHeaderSize);
     inc(NewIndex);
    end;
    // The slots the dropped ones used are cleared. Most of them are written
    // over by the new sections below, but any which are not would otherwise
    // leave an old header standing in a table which no longer counts it.
    while NewIndex<NumberOfSections do begin
     Target.Seek(SectionHeaderTableOffset+(TpvInt64(NewIndex)*SectionHeaderSize),soBeginning);
     Target.WriteBuffer(Padding,SectionHeaderSize);
     inc(NewIndex);
    end;

    // Pad out to the new header size and copy the body, which is everything up
    // to the end of the last section which stays. What lies behind that is the
    // sections this run replaces, the old string table and the appended symbol
    // table, all of which are written again, so copying them through would only
    // make the file longer every time it is built.
    Target.Seek(0,soEnd);
    PadTo(Target,TpvInt64(NewSizeOfHeaders));
    if BodyEnd>TpvInt64(SizeOfHeaders) then begin
     Source.Seek(TpvInt64(SizeOfHeaders),soBeginning);
     Target.CopyFrom(Source,BodyEnd-TpvInt64(SizeOfHeaders));
    end;

    // The entries of a debug directory which stays hold a file offset to their
    // own payload, and that payload has just moved with its section. Nothing
    // else corrects it, and an entry which points at where its data used to be
    // is worse than none: the image is still valid and a debugger reads
    // whatever is there now.
    if (not HaveCodeView) and (Delta>0) and (ExistingDebugCount>0) then begin
     EntryOffset:=SourceOffsetOfRVA(DebugDirectoryAddress)+TpvInt64(Delta);
     for Index:=0 to ExistingDebugCount-1 do begin
      if ExistingDebugEntries[Index].RawPointer<>0 then begin
       Target.Seek(EntryOffset+(TpvInt64(ExistingDebugEntries[Index].SourceIndex)*DebugDirectoryEntrySize)+24,soBeginning);
       Value32:=ExistingDebugEntries[Index].RawPointer+Delta;
       Target.WriteBuffer(Value32,SizeOf(TpvUInt32));
      end;
     end;
    end;

    // Room in front of the entry which was built here for the ones which stay.
    // A debug directory is a run of entries and the payloads behind them, so
    // the kept ones have to come first and the section has to be that much
    // longer before anything about its placement is written down.
    if HaveCodeView and (ExistingDebugCount>0) then begin
     for Index:=0 to fSectionCount-1 do begin
      if fSections[Index].IsDebugDirectory then begin
       MergedDirectory:=TMemoryStream.Create;
       MergedDirectory.Size:=TpvInt64(ExistingDebugCount)*DebugDirectoryEntrySize;
       FillChar(MergedDirectory.Memory^,MergedDirectory.Size,#0);
       MergedDirectory.Position:=MergedDirectory.Size;
       fSections[Index].Data.Position:=0;
       MergedDirectory.CopyFrom(fSections[Index].Data,fSections[Index].Data.Size);
       FreeAndNil(fSections[Index].Data);
       fSections[Index].Data:=MergedDirectory;
       break;
      end;
     end;
    end;

    // Append the new sections, each starting on a file alignment boundary.
    for Index:=0 to fSectionCount-1 do begin

     Target.Seek(0,soEnd);
     PadTo(Target,TpvInt64(AlignUp(TpvUInt64(Target.Position),FileAlignment)));
     DataOffset:=TpvUInt64(Target.Position);

     fSections[Index].Data.Position:=0;
     Target.CopyFrom(fSections[Index].Data,fSections[Index].Data.Size);

     // Out to the full size the header is about to claim for it. Every section
     // but the last one got this for free from the padding in front of the next
     // one, and the last one did not: what followed it was the string table,
     // which then began inside the range this section says is its own.
     PadTo(Target,TpvInt64(DataOffset)+TpvInt64(AlignUp(TpvUInt64(fSections[Index].Data.Size),FileAlignment)));

     // Write the header entry for this section.
     Target.Seek(NewSectionHeaderTableEnd+(TpvInt64(Index)*SectionHeaderSize),soBeginning);
     FillChar(RawName,SizeOf(RawName),#0);
     // A name which fits goes in as it is. Only a longer one is put into the
     // string table and referred to by a slash and an offset, which is the gnu
     // extension gdb needs for the debug sections and which nothing else should
     // have to carry.
     if length(fSections[Index].Name)>8 then begin
      NameText:=TpvRawByteString('/'+IntToStr(NameOffsets[Index]));
      if length(NameText)>8 then begin
       SetLength(NameText,8);
      end;
     end else begin
      NameText:=TpvRawByteString(fSections[Index].Name);
     end;
     Move(NameText[1],RawName[0],length(NameText));
     Target.WriteBuffer(RawName,8);
     Value32:=TpvUInt32(fSections[Index].Data.Size);
     Target.WriteBuffer(Value32,SizeOf(TpvUInt32)); // virtual size
     Value32:=TpvUInt32(NextRVA);
     Target.WriteBuffer(Value32,SizeOf(TpvUInt32)); // virtual address
     Value32:=TpvUInt32(AlignUp(TpvUInt64(fSections[Index].Data.Size),FileAlignment));
     Target.WriteBuffer(Value32,SizeOf(TpvUInt32)); // size of raw data
     Value32:=TpvUInt32(DataOffset);
     Target.WriteBuffer(Value32,SizeOf(TpvUInt32)); // pointer to raw data
     Value32:=0;
     Target.WriteBuffer(Value32,SizeOf(TpvUInt32)); // relocations
     Target.WriteBuffer(Value32,SizeOf(TpvUInt32)); // line numbers
     Value16:=0;
     Target.WriteBuffer(Value16,SizeOf(TpvUInt16));
     Target.WriteBuffer(Value16,SizeOf(TpvUInt16));
     Value32:=IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_DISCARDABLE or IMAGE_SCN_MEM_READ;
     Target.WriteBuffer(Value32,SizeOf(TpvUInt32));

     // A debug directory has to say where its own payload sits, and the image
     // has to point at the directory, so both are filled in now that the
     // placement is known.
     if fSections[Index].IsDebugDirectory then begin
      // The entries which were already there come first, each with its own file
      // offset moved along by what the header area gained, and the one written
      // here follows them. The directory is therefore the old one with ours
      // added rather than ours instead of it.
      for NewIndex:=0 to ExistingDebugCount-1 do begin
       if ExistingDebugEntries[NewIndex].RawPointer<>0 then begin
        Value32:=ExistingDebugEntries[NewIndex].RawPointer+Delta;
        Move(Value32,ExistingDebugEntries[NewIndex].Raw[24],SizeOf(TpvUInt32));
       end;
       Target.Seek(TpvInt64(DataOffset)+(TpvInt64(NewIndex)*DebugDirectoryEntrySize),soBeginning);
       Target.WriteBuffer(ExistingDebugEntries[NewIndex].Raw[0],DebugDirectoryEntrySize);
      end;
      // Ours sits behind them, and the payload behind all of them. The two
      // address fields of an entry sit at twenty and twenty four, behind the
      // size of the data at sixteen, which must not be overwritten.
      EntryOffset:=TpvInt64(DataOffset)+(TpvInt64(ExistingDebugCount)*DebugDirectoryEntrySize);
      DebugEntryCount:=ExistingDebugCount+1;
      Target.Seek(EntryOffset+20,soBeginning);
      Value32:=TpvUInt32(NextRVA)+TpvUInt32(DebugEntryCount*DebugDirectoryEntrySize);
      Target.WriteBuffer(Value32,SizeOf(TpvUInt32)); // address of the record
      Value32:=TpvUInt32(DataOffset)+TpvUInt32(DebugEntryCount*DebugDirectoryEntrySize);
      Target.WriteBuffer(Value32,SizeOf(TpvUInt32)); // file offset of the record
      Target.Seek(DataDirectoryOffset+(6*8),soBeginning);
      Value32:=TpvUInt32(NextRVA);
      Target.WriteBuffer(Value32,SizeOf(TpvUInt32));
      Value32:=TpvUInt32(DebugEntryCount*DebugDirectoryEntrySize);
      Target.WriteBuffer(Value32,SizeOf(TpvUInt32));
     end;

     NextRVA:=AlignUp(NextRVA+TpvUInt64(fSections[Index].Data.Size),SectionAlignment);

    end;

    // The string table goes last, and the COFF header has to point at it. A
    // symbol count of zero is what tells a reader that only the string table is
    // there, which is exactly what FreePascal produces for a stripped build.
    //
    // Only when one of the names actually needed it. Where none did, the
    // pointer is cleared instead, so that adding nothing but a debug directory
    // leaves an executable which has no string table at all rather than one
    // which carries the machinery for a name that fitted anyway.
    Target.Seek(TpvInt64(NewHeaderOffset)+12,soBeginning);
    if NeedStringTable then begin
     Target.Seek(0,soEnd);
     StringTableOffset:=TpvUInt64(Target.Position);
     StringTable.Position:=0;
     Target.CopyFrom(StringTable,StringTable.Size);
     Target.Seek(TpvInt64(NewHeaderOffset)+12,soBeginning);
     Value32:=TpvUInt32(StringTableOffset);
    end else begin
     Value32:=0;
    end;
    Target.WriteBuffer(Value32,SizeOf(TpvUInt32));
    Value32:=0;
    Target.WriteBuffer(Value32,SizeOf(TpvUInt32));

    Target.Seek(SizeOfImageOffset,soBeginning);
    Value32:=TpvUInt32(NextRVA);
    Target.WriteBuffer(Value32,SizeOf(TpvUInt32));

    // Everything which was added here is read back by the caller. What is not
    // is everything which was already there, and this run moved all of it: each
    // section has been shifted by whatever the header area gained, its header
    // written again from its own bytes with a corrected file offset, and the
    // body copied only as far as the last section which stays. An error in any
    // of that gives an executable which does not start, and none of the checks
    // afterwards look at code, so the run would report success three times over
    // a program nobody can run any more.
    //
    // So every section which stays is held against itself, byte for byte,
    // before the original is replaced rather than after. A file which fails
    // here is thrown away and the original never moves.
    Corrupted:='';
    NewIndex:=0;
    for Index:=0 to NumberOfSections-1 do begin
     if ExistingSections[Index].Dropped then begin
      continue;
     end;
     if (ExistingSections[Index].RawPointer<>0) and (ExistingSections[Index].RawSize<>0) and
        ((TpvInt64(ExistingSections[Index].RawPointer)+TpvInt64(ExistingSections[Index].RawSize))<=Source.Size) then begin
      // Read where the header which was just written says the bytes are, rather
      // than where this expects them to be. An offset corrected wrongly moves
      // the answer and not the bytes, so comparing the bytes at the place they
      // were put would agree with itself and miss it entirely. What has to hold
      // is that a reader following the new header finds the old contents.
      Target.Seek(SectionHeaderTableOffset+(TpvInt64(NewIndex)*SectionHeaderSize)+20,soBeginning);
      Target.ReadBuffer(RawPointer,SizeOf(TpvUInt32));
      Value32:=StreamCRC32(Source,TpvInt64(ExistingSections[Index].RawPointer),TpvInt64(ExistingSections[Index].RawSize));
      if ((TpvInt64(RawPointer)+TpvInt64(ExistingSections[Index].RawSize))>Target.Size) or
         (StreamCRC32(Target,TpvInt64(RawPointer),TpvInt64(ExistingSections[Index].RawSize))<>Value32) then begin
       Corrupted:=ExistingSections[Index].Name;
       break;
      end;
     end;
     inc(NewIndex);
    end;
    if length(Corrupted)>0 then begin
     fMessage:='The contents of '+Corrupted+' did not survive being moved, so the executable was left alone.';
     FreeAndNil(Target);
     DeleteFile(TargetName);
     exit;
    end;

   finally
    FreeAndNil(Target);
   end;

  finally
   FreeAndNil(Source);
  end;

  // Everything up to here only read the original and built the replacement
  // beside it, and this is where that care would have been thrown away. The
  // original was deleted first and the replacement renamed into the free name
  // afterwards, so a rename which does not go through, a scanner holding the
  // name for a moment, a full volume, a target which turns out to be elsewhere,
  // left neither of them: the one good copy was already gone and its
  // replacement never took the name.
  //
  // So the original is set aside instead, and only thrown away once the
  // replacement is in place. Every way out of here now leaves either the
  // untouched original or the finished new file under that name, which is what
  // lets the caller say that a failure changed nothing.
  BackupName:=aFileName+'.mapsymbols-old';
  DeleteFile(BackupName);
  if not RenameFile(aFileName,BackupName) then begin
   DeleteFile(TargetName);
   fMessage:='Could not set '+aFileName+' aside, so it was left alone.';
   exit;
  end;
  if not RenameFile(TargetName,aFileName) then begin
   // Put back what was there. If even this does not work the original is still
   // on disk under the name in the message, which is worth more than a message
   // which does not say where it went.
   if not RenameFile(BackupName,aFileName) then begin
    fMessage:='Could not replace '+aFileName+', and it is now at '+BackupName+'.';
    exit;
   end;
   DeleteFile(TargetName);
   fMessage:='Could not replace '+aFileName+', so it was left alone.';
   exit;
  end;
  DeleteFile(BackupName);

  if DroppedCount>0 then begin
   fMessage:='Injected '+IntToStr(fSectionCount)+' sections, replacing '+IntToStr(DroppedCount)+' from an earlier run';
  end else begin
   fMessage:='Injected '+IntToStr(fSectionCount)+' sections';
  end;
  if Delta>0 then begin
   fMessage:=fMessage+', header area grown by '+IntToStr(Delta)+' bytes.';
  end else begin
   fMessage:=fMessage+'.';
  end;
  if LostDebugCount>0 then begin
   fMessage:=fMessage+' '+IntToStr(LostDebugCount)+' debug directory entries were dropped, because what they pointed at was in a section which was replaced.';
  end;
  result:=true;

 finally
  FreeAndNil(StringTable);
  FreeAndNil(OldStringTable);
  ExistingSections:=nil;
  ExistingDebugEntries:=nil;
 end;

end;

end.
