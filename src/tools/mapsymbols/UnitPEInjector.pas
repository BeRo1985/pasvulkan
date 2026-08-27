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
    TargetName:String;
    NewHeaderOffset:TpvUInt32;
    Signature:array[0..3] of AnsiChar;
    NumberOfSections:TpvUInt16;
    SizeOfOptionalHeader:TpvUInt16;
    OptionalMagic:TpvUInt16;
    SymbolTablePointer,SymbolCount:TpvUInt32;
    SectionAlignment,FileAlignment,SizeOfImage,SizeOfHeaders:TpvUInt32;
    CertificateAddress,CertificateSize:TpvUInt32;
    SectionHeaderTableOffset,SectionHeaderTableEnd:TpvInt64;
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

   if SymbolTablePointer<>0 then begin
    // Rewriting an existing COFF string table would mean moving whatever sits
    // behind it, so this case is refused rather than half handled. It does not
    // arise for Delphi builds, which carry no symbol table at all.
    fMessage:='The executable already has a COFF symbol table, which this does not rewrite.';
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
   Source.Seek(DataDirectoryOffset+(4*8),soBeginning);
   Source.ReadBuffer(CertificateAddress,SizeOf(TpvUInt32));
   Source.ReadBuffer(CertificateSize,SizeOf(TpvUInt32));
   if (CertificateAddress<>0) or (CertificateSize<>0) then begin
    fMessage:='The executable is signed, which this would invalidate.';
    exit;
   end;

   SectionHeaderTableOffset:=TpvInt64(NewHeaderOffset)+24+TpvInt64(SizeOfOptionalHeader);
   SectionHeaderTableEnd:=SectionHeaderTableOffset+(TpvInt64(NumberOfSections)*SectionHeaderSize);

   // Grow the header area when the new section headers do not fit. Delphi
   // leaves only 64 bytes of slack, so this is the normal case rather than the
   // exception, and every section then moves along in the file by that amount.
   NewSizeOfHeaders:=TpvUInt32(AlignUp(TpvUInt64(SectionHeaderTableEnd+(TpvInt64(fSectionCount)*SectionHeaderSize)),FileAlignment));
   if NewSizeOfHeaders<SizeOfHeaders then begin
    NewSizeOfHeaders:=SizeOfHeaders;
   end;
   Delta:=NewSizeOfHeaders-SizeOfHeaders;

   // Where the new sections go in the address space, which is behind everything
   // the image already covers.
   NextRVA:=0;
   for Index:=0 to NumberOfSections-1 do begin
    Source.Seek(SectionHeaderTableOffset+(TpvInt64(Index)*SectionHeaderSize)+8,soBeginning);
    Source.ReadBuffer(VirtualSize,SizeOf(TpvUInt32));
    Source.ReadBuffer(VirtualAddress,SizeOf(TpvUInt32));
    if (TpvUInt64(VirtualAddress)+TpvUInt64(VirtualSize))>NextRVA then begin
     NextRVA:=TpvUInt64(VirtualAddress)+TpvUInt64(VirtualSize);
    end;
   end;
   NextRVA:=AlignUp(NextRVA,SectionAlignment);

   // The string table starts with its own size, and offsets are counted from
   // its beginning, so the first name sits at four.
   Value32:=0;
   StringTable.WriteBuffer(Value32,SizeOf(TpvUInt32));
   SetLength(NameOffsets,fSectionCount);
   for Index:=0 to fSectionCount-1 do begin
    NameOffsets[Index]:=TpvUInt32(StringTable.Position);
    NameText:=TpvRawByteString(fSections[Index].Name);
    StringTable.WriteBuffer(NameText[1],length(NameText));
    StringTable.WriteBuffer(Zero,1);
   end;
   Value32:=TpvUInt32(StringTable.Size);
   StringTable.Position:=0;
   StringTable.WriteBuffer(Value32,SizeOf(TpvUInt32));

   // Everything is written into a new file rather than in place, because the
   // header area may have to grow, which shifts every section behind it.
   TargetName:=aFileName+'.mapsymbols-tmp';
   Target:=TFileStream.Create(TargetName,fmCreate);
   try

    Source.Seek(0,soBeginning);
    Target.CopyFrom(Source,TpvInt64(SizeOfHeaders));

    Target.Seek(TpvInt64(NewHeaderOffset)+6,soBeginning);
    Value16:=TpvUInt16(TpvInt64(NumberOfSections)+fSectionCount);
    Target.WriteBuffer(Value16,SizeOf(TpvUInt16));
    Target.Seek(SizeOfHeadersOffset,soBeginning);
    Target.WriteBuffer(NewSizeOfHeaders,SizeOf(TpvUInt32));

    // Move every existing section along by what the header area gained. Their
    // virtual addresses are untouched, so the image in memory is identical.
    if Delta>0 then begin
     for Index:=0 to NumberOfSections-1 do begin
      Target.Seek(SectionHeaderTableOffset+(TpvInt64(Index)*SectionHeaderSize)+20,soBeginning);
      Target.ReadBuffer(RawPointer,SizeOf(TpvUInt32));
      if RawPointer<>0 then begin
       Target.Seek(SectionHeaderTableOffset+(TpvInt64(Index)*SectionHeaderSize)+20,soBeginning);
       Value32:=RawPointer+Delta;
       Target.WriteBuffer(Value32,SizeOf(TpvUInt32));
      end;
     end;
    end;

    // Pad out to the new header size and copy the rest of the original file.
    Target.Seek(0,soEnd);
    PadTo(Target,TpvInt64(NewSizeOfHeaders));
    if Source.Size>TpvInt64(SizeOfHeaders) then begin
     Source.Seek(TpvInt64(SizeOfHeaders),soBeginning);
     Target.CopyFrom(Source,Source.Size-TpvInt64(SizeOfHeaders));
    end;

    // Append the new sections, each starting on a file alignment boundary.
    for Index:=0 to fSectionCount-1 do begin

     Target.Seek(0,soEnd);
     PadTo(Target,TpvInt64(AlignUp(TpvUInt64(Target.Position),FileAlignment)));
     DataOffset:=TpvUInt64(Target.Position);

     fSections[Index].Data.Position:=0;
     Target.CopyFrom(fSections[Index].Data,fSections[Index].Data.Size);

     // Write the header entry for this section.
     Target.Seek(SectionHeaderTableEnd+(TpvInt64(Index)*SectionHeaderSize),soBeginning);
     FillChar(RawName,SizeOf(RawName),#0);
     NameText:=TpvRawByteString('/'+IntToStr(NameOffsets[Index]));
     if length(NameText)>8 then begin
      SetLength(NameText,8);
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
      // The two address fields sit at twenty and twenty four, behind the size
      // of the data at sixteen, which must not be overwritten.
      Target.Seek(TpvInt64(DataOffset)+20,soBeginning);
      Value32:=TpvUInt32(NextRVA)+28;
      Target.WriteBuffer(Value32,SizeOf(TpvUInt32)); // address of the record
      Value32:=TpvUInt32(DataOffset)+28;
      Target.WriteBuffer(Value32,SizeOf(TpvUInt32)); // file offset of the record
      Target.Seek(DataDirectoryOffset+(6*8),soBeginning);
      Value32:=TpvUInt32(NextRVA);
      Target.WriteBuffer(Value32,SizeOf(TpvUInt32));
      Value32:=28;
      Target.WriteBuffer(Value32,SizeOf(TpvUInt32));
     end;

     NextRVA:=AlignUp(NextRVA+TpvUInt64(fSections[Index].Data.Size),SectionAlignment);

    end;

    // The string table goes last, and the COFF header has to point at it. A
    // symbol count of zero is what tells a reader that only the string table is
    // there, which is exactly what FreePascal produces for a stripped build.
    Target.Seek(0,soEnd);
    StringTableOffset:=TpvUInt64(Target.Position);
    StringTable.Position:=0;
    Target.CopyFrom(StringTable,StringTable.Size);

    Target.Seek(TpvInt64(NewHeaderOffset)+12,soBeginning);
    Value32:=TpvUInt32(StringTableOffset);
    Target.WriteBuffer(Value32,SizeOf(TpvUInt32));
    Value32:=0;
    Target.WriteBuffer(Value32,SizeOf(TpvUInt32));

    Target.Seek(SizeOfImageOffset,soBeginning);
    Value32:=TpvUInt32(NextRVA);
    Target.WriteBuffer(Value32,SizeOf(TpvUInt32));

   finally
    FreeAndNil(Target);
   end;

  finally
   FreeAndNil(Source);
  end;

  DeleteFile(aFileName);
  if not RenameFile(TargetName,aFileName) then begin
   fMessage:='Could not replace '+aFileName+'.';
   exit;
  end;

  if Delta>0 then begin
   fMessage:='Injected '+IntToStr(fSectionCount)+' sections, header area grown by '+IntToStr(Delta)+' bytes.';
  end else begin
   fMessage:='Injected '+IntToStr(fSectionCount)+' sections.';
  end;
  result:=true;

 finally
  FreeAndNil(StringTable);
 end;

end;

end.
