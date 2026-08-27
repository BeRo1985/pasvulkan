// Minimal reader for executable containers, currently PE.
//
// It provides only what building a symbol table needs: the image base, the
// section table, the contents of a named section, the COFF symbol table and the
// .gnu_debuglink entry.
//
// The debug link matters in practice: a FreePascal build which was linked with
// an external debug file, which is what Lazarus does by default, carries no
// DWARF at all in the executable. All of it sits in the accompanying .dbg, and
// the executable only holds a .gnu_debuglink section naming it. The same
// indirection is what the FreePascal runtime itself follows in
// rtl/inc/exeinfo.pp.
unit UnitImageFile;
{$ifdef fpc}
 {$mode delphi}
{$endif}

interface

uses SysUtils,
     Classes,
     PasVulkan.Types;

type TImageFileFormat=(iffUnknown,iffPE);

     TImageSection=record
      Name:String;
      VirtualAddress:TpvUInt64;
      VirtualSize:TpvUInt64;
      FileOffset:TpvUInt64;
      RawSize:TpvUInt64;
     end;

     TImageSections=array of TImageSection;

     TImageSymbolEvent=procedure(const aRVA:TpvUInt64;const aName:String) of object;

     TImageFile=class
      private
       fStream:TFileStream;
       fFileName:String;
       fFormat:TImageFileFormat;
       fImageBase:TpvUInt64;
       fSections:TImageSections;
       fSymbolTableOffset:TpvUInt64;
       fSymbolCount:TpvUInt32;
       function StringTableOffset:TpvInt64;
       function ReadStringTableEntry(const aOffset:TpvInt64):String;
       function ReadLongSectionName(const aOffset:TpvInt64):String;
       function ReadPE:Boolean;
      public
       constructor Create;
       destructor Destroy; override;
       function Open(const aFileName:String):Boolean;
       // Releases the file handle while keeping everything already read. The
       // caller has to do this before the image itself is written to.
       procedure Close;
       function FindSection(const aName:String;out aSection:TImageSection):Boolean;
       // Reads a whole section into memory. The caller owns the returned stream.
       function ReadSection(const aName:String):TMemoryStream;
       // Returns the file named by a .gnu_debuglink section, already resolved
       // against the directory of this image, or an empty string when there is
       // none or the named file does not exist.
       function DebugLinkFileName:String;
       // Walks the COFF symbol table and reports every symbol which lives in a
       // section, translated into an image relative address.
       procedure EnumerateSymbols(const aEvent:TImageSymbolEvent);
       property Format:TImageFileFormat read fFormat;
       property ImageBase:TpvUInt64 read fImageBase;
       property Sections:TImageSections read fSections;
     end;

implementation

constructor TImageFile.Create;
begin
 inherited Create;
 fStream:=nil;
 fFormat:=iffUnknown;
 fImageBase:=0;
 fSections:=nil;
 fSymbolTableOffset:=0;
 fSymbolCount:=0;
end;

destructor TImageFile.Destroy;
begin
 FreeAndNil(fStream);
 fSections:=nil;
 inherited Destroy;
end;

function TImageFile.Open(const aFileName:String):Boolean;
var Signature:TpvUInt16;
begin
 result:=false;
 FreeAndNil(fStream);
 fFileName:=aFileName;
 fFormat:=iffUnknown;
 if not FileExists(aFileName) then begin
  exit;
 end;
 fStream:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyNone);
 if fStream.Size<64 then begin
  exit;
 end;
 fStream.Seek(0,soBeginning);
 fStream.ReadBuffer(Signature,SizeOf(TpvUInt16));
 if Signature=$5a4d then begin
  // 'MZ'
  fFormat:=iffPE;
  result:=ReadPE;
 end;
end;

function TImageFile.StringTableOffset:TpvInt64;
begin
 // The COFF string table follows directly behind the symbol table. A symbol
 // count of zero does not mean there is no string table: an executable whose
 // debug information was linked out into a separate file keeps the pointer and
 // the string table, and needs it, because the long section names, including
 // .gnu_debuglink itself, are stored there.
 if fSymbolTableOffset=0 then begin
  result:=0;
 end else begin
  result:=TpvInt64(fSymbolTableOffset)+(TpvInt64(fSymbolCount)*18);
 end;
end;

function TImageFile.ReadStringTableEntry(const aOffset:TpvInt64):String;
var Base,Available:TpvInt64;
    Buffer:array[0..1023] of AnsiChar;
begin
 result:='';
 Base:=StringTableOffset;
 if (Base<=0) or (aOffset<0) or ((Base+aOffset)>=fStream.Size) then begin
  exit;
 end;
 Available:=fStream.Size-(Base+aOffset);
 if Available>TpvInt64(SizeOf(Buffer)-1) then begin
  Available:=SizeOf(Buffer)-1;
 end;
 FillChar(Buffer,SizeOf(Buffer),#0);
 fStream.Seek(Base+aOffset,soBeginning);
 fStream.ReadBuffer(Buffer,Available);
 result:=String(PAnsiChar(@Buffer[0]));
end;

function TImageFile.ReadLongSectionName(const aOffset:TpvInt64):String;
var Position:TpvInt64;
begin
 result:='';
 if aOffset<0 then begin
  exit;
 end;
 Position:=fStream.Position;
 try
  result:=ReadStringTableEntry(aOffset);
 finally
  fStream.Seek(Position,soBeginning);
 end;
end;

procedure TImageFile.Close;
begin
 FreeAndNil(fStream);
end;

function TImageFile.ReadPE:Boolean;
var NewHeaderOffset:TpvUInt32;
    PESignature:array[0..3] of AnsiChar;
    NumberOfSections:TpvUInt16;
    SizeOfOptionalHeader:TpvUInt16;
    OptionalMagic:TpvUInt16;
    Value32:TpvUInt32;
    SectionIndex:TpvInt32;
    RawName:array[0..7] of AnsiChar;
    VirtualSize,VirtualAddress,RawSize,RawOffset:TpvUInt32;
    NameLength:TpvInt32;
begin

 result:=false;

 fStream.Seek(TpvInt64($3c),soBeginning);
 fStream.ReadBuffer(NewHeaderOffset,SizeOf(TpvUInt32));
 if TpvInt64(NewHeaderOffset)+24>fStream.Size then begin
  exit;
 end;

 fStream.Seek(TpvInt64(NewHeaderOffset),soBeginning);
 fStream.ReadBuffer(PESignature,4);
 if (PESignature[0]<>'P') or (PESignature[1]<>'E') or (PESignature[2]<>#0) or (PESignature[3]<>#0) then begin
  exit;
 end;

 // COFF file header: machine, number of sections, time stamp, symbol table
 // pointer, symbol count, size of the optional header, characteristics.
 fStream.Seek(TpvInt64(NewHeaderOffset)+6,soBeginning);
 fStream.ReadBuffer(NumberOfSections,SizeOf(TpvUInt16));
 fStream.Seek(TpvInt64(NewHeaderOffset)+12,soBeginning);
 fStream.ReadBuffer(Value32,SizeOf(TpvUInt32));
 fSymbolTableOffset:=Value32;
 fStream.ReadBuffer(fSymbolCount,SizeOf(TpvUInt32));
 fStream.ReadBuffer(SizeOfOptionalHeader,SizeOf(TpvUInt16));

 fStream.Seek(TpvInt64(NewHeaderOffset)+24,soBeginning);
 fStream.ReadBuffer(OptionalMagic,SizeOf(TpvUInt16));
 if OptionalMagic=$20b then begin
  fStream.Seek(TpvInt64(NewHeaderOffset)+24+24,soBeginning);
  fStream.ReadBuffer(fImageBase,SizeOf(TpvUInt64));
 end else if OptionalMagic=$10b then begin
  fStream.Seek(TpvInt64(NewHeaderOffset)+24+28,soBeginning);
  fStream.ReadBuffer(Value32,SizeOf(TpvUInt32));
  fImageBase:=Value32;
 end else begin
  exit;
 end;

 SetLength(fSections,NumberOfSections);
 for SectionIndex:=0 to NumberOfSections-1 do begin
  fStream.Seek(TpvInt64(NewHeaderOffset)+24+TpvInt64(SizeOfOptionalHeader)+(TpvInt64(SectionIndex)*40),soBeginning);
  fStream.ReadBuffer(RawName,8);
  fStream.ReadBuffer(VirtualSize,SizeOf(TpvUInt32));
  fStream.ReadBuffer(VirtualAddress,SizeOf(TpvUInt32));
  fStream.ReadBuffer(RawSize,SizeOf(TpvUInt32));
  fStream.ReadBuffer(RawOffset,SizeOf(TpvUInt32));
  NameLength:=0;
  while (NameLength<8) and (RawName[NameLength]<>#0) do begin
   inc(NameLength);
  end;
  SetString(fSections[SectionIndex].Name,PAnsiChar(@RawName[0]),NameLength);
  // A name which does not fit into the eight byte field is stored as a slash
  // followed by a decimal offset into the COFF string table. Every DWARF
  // section is affected, since .debug_line alone is already eleven characters,
  // so without this the debug sections are simply invisible.
  if (NameLength>1) and (fSections[SectionIndex].Name[1]='/') then begin
   fSections[SectionIndex].Name:=ReadLongSectionName(StrToIntDef(Copy(fSections[SectionIndex].Name,2,NameLength-1),-1));
  end;
  fSections[SectionIndex].VirtualAddress:=VirtualAddress;
  fSections[SectionIndex].VirtualSize:=VirtualSize;
  fSections[SectionIndex].FileOffset:=RawOffset;
  fSections[SectionIndex].RawSize:=RawSize;
 end;

 result:=true;

end;

function TImageFile.FindSection(const aName:String;out aSection:TImageSection):Boolean;
var Index:TpvInt32;
begin
 result:=false;
 for Index:=0 to length(fSections)-1 do begin
  if fSections[Index].Name=aName then begin
   aSection:=fSections[Index];
   result:=true;
   exit;
  end;
 end;
end;

function TImageFile.ReadSection(const aName:String):TMemoryStream;
var Section:TImageSection;
    Size:TpvInt64;
begin
 result:=nil;
 if not FindSection(aName,Section) then begin
  exit;
 end;
 // The raw size is rounded up to the file alignment, while the virtual size is
 // the real one. Debug sections are usually not loaded, so the virtual size can
 // read as zero, in which case the raw size is all there is to go by.
 if (Section.VirtualSize>0) and (Section.VirtualSize<Section.RawSize) then begin
  Size:=TpvInt64(Section.VirtualSize);
 end else begin
  Size:=TpvInt64(Section.RawSize);
 end;
 if (Size<=0) or ((TpvInt64(Section.FileOffset)+Size)>fStream.Size) then begin
  exit;
 end;
 result:=TMemoryStream.Create;
 try
  fStream.Seek(TpvInt64(Section.FileOffset),soBeginning);
  result.CopyFrom(fStream,Size);
  result.Seek(0,soBeginning);
 except
  FreeAndNil(result);
  raise;
 end;
end;

function TImageFile.DebugLinkFileName:String;
var Data:TMemoryStream;
    Name:String;
    Directory:String;
    Candidate:String;
begin
 result:='';
 Data:=ReadSection('.gnu_debuglink');
 if not assigned(Data) then begin
  exit;
 end;
 try
  // The section holds a null terminated file name, padded to a four byte
  // boundary, followed by a CRC32 of the debug file. The name alone is enough
  // here, since the tool is pointed at a specific build by the caller.
  Name:=String(PAnsiChar(Data.Memory));
  if length(Name)=0 then begin
   exit;
  end;
  Directory:=ExtractFilePath(ExpandFileName(fFileName));
  Candidate:=Directory+Name;
  if FileExists(Candidate) then begin
   result:=Candidate;
  end else if FileExists(Name) then begin
   result:=Name;
  end;
 finally
  FreeAndNil(Data);
 end;
end;

procedure TImageFile.EnumerateSymbols(const aEvent:TImageSymbolEvent);
const SymbolRecordSize=18;
var Index:TpvUInt32;
    Base:TpvInt64;
    ShortName:array[0..7] of AnsiChar;
    Value:TpvUInt32;
    SectionNumber:TpvInt16;
    StorageClass:TpvUInt8;
    AuxiliaryCount:TpvUInt8;
    TypeValue:TpvUInt16;
    Zeroes,NameOffset:TpvUInt32;
    Name:String;
    NameLength:TpvInt32;
begin

 if (fSymbolTableOffset=0) or (fSymbolCount=0) or not assigned(aEvent) then begin
  exit;
 end;

 Base:=TpvInt64(fSymbolTableOffset);
 if StringTableOffset>fStream.Size then begin
  exit;
 end;

 Index:=0;
 while Index<fSymbolCount do begin

  fStream.Seek(Base+(TpvInt64(Index)*SymbolRecordSize),soBeginning);
  fStream.ReadBuffer(ShortName,8);
  fStream.ReadBuffer(Value,SizeOf(TpvUInt32));
  fStream.ReadBuffer(SectionNumber,SizeOf(TpvInt16));
  fStream.ReadBuffer(TypeValue,SizeOf(TpvUInt16));
  fStream.ReadBuffer(StorageClass,SizeOf(TpvUInt8));
  fStream.ReadBuffer(AuxiliaryCount,SizeOf(TpvUInt8));

  // A name whose first four bytes are zero is an offset into the string table
  // rather than an inline short name.
  Move(ShortName[0],Zeroes,4);
  if Zeroes=0 then begin
   Move(ShortName[4],NameOffset,4);
   Name:=ReadStringTableEntry(TpvInt64(NameOffset));
  end else begin
   NameLength:=0;
   while (NameLength<8) and (ShortName[NameLength]<>#0) do begin
    inc(NameLength);
   end;
   SetString(Name,PAnsiChar(@ShortName[0]),NameLength);
  end;

  // Section numbers are one based, anything else is an absolute or undefined
  // symbol and has no address in this image.
  if (length(Name)>0) and
     (SectionNumber>0) and
     (SectionNumber<=TpvInt16(length(fSections))) then begin
   aEvent(fSections[SectionNumber-1].VirtualAddress+TpvUInt64(Value),Name);
  end;

  inc(Index,1+TpvUInt32(AuxiliaryCount));

 end;

end;

end.
