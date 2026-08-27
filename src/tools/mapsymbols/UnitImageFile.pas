// Minimal reader for executable containers, PE and ELF.
//
// It provides only what building a symbol table needs: the image base, the
// section table, the range covered by executable sections, the contents of a
// named section, the symbol table and the .gnu_debuglink entry.
//
// The debug link matters in practice: a FreePascal build which was linked with
// an external debug file, which is what Lazarus does by default, carries no
// DWARF at all in the executable. All of it sits in the accompanying .dbg, and
// the executable only holds a .gnu_debuglink section naming it. The same
// indirection is what the FreePascal runtime itself follows in
// rtl/inc/exeinfo.pp.
//
// The image base is taken from the PE optional header, and for ELF from the
// lowest PT_LOAD segment address rather than from the section table. That
// distinction is deliberate: the first allocatable section of an ELF can start
// past the beginning of the mapping, so using it would shift every address by
// the difference.
unit UnitImageFile;
{$ifdef fpc}
 {$mode delphi}
{$endif}

interface

uses SysUtils,
     Classes,
     PasVulkan.Types;

type TImageFileFormat=(iffUnknown,iffPE,iffELF);

     TImageSection=record
      Name:String;
      VirtualAddress:TpvUInt64;
      VirtualSize:TpvUInt64;
      FileOffset:TpvUInt64;
      RawSize:TpvUInt64;
      Executable:Boolean;
      // The COFF section flags. Taken straight from a PE, and put together out
      // of the corresponding ELF section flags otherwise, so that a consumer
      // which speaks COFF gets an answer either way.
      Characteristics:TpvUInt32;
     end;

     TImageSections=array of TImageSection;

     TImageSymbolEvent=procedure(const aRVA:TpvUInt64;const aName:String) of object;

     TImageFile=class
      private
       fStream:TFileStream;
       fFileName:String;
       fFormat:TImageFileFormat;
       fImageBase:TpvUInt64;
       fCodeLow:TpvUInt64;
       fCodeHigh:TpvUInt64;
       fSections:TImageSections;
       fSymbolTableOffset:TpvUInt64;
       fSymbolCount:TpvUInt32;
       fELF64:Boolean;
       fMachine:TpvUInt16;
       function StringTableOffset:TpvInt64;
       function ReadStringTableEntry(const aOffset:TpvInt64):String;
       function ReadStringAt(const aAbsoluteOffset:TpvInt64):String;
       function ReadLongSectionName(const aOffset:TpvInt64):String;
       procedure ComputeCodeRange;
       function ReadPE:Boolean;
       function ReadELF:Boolean;
       procedure EnumeratePESymbols(const aEvent:TImageSymbolEvent);
       procedure EnumerateELFSymbols(const aEvent:TImageSymbolEvent);
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
       // Walks the symbol table and reports every symbol which lives in a
       // section, translated into a link time virtual address.
       procedure EnumerateSymbols(const aEvent:TImageSymbolEvent);
       property Format:TImageFileFormat read fFormat;
       property ImageBase:TpvUInt64 read fImageBase;
       // Lowest and highest link time address covered by executable sections.
       // Used to tell real line information apart from the leftovers which
       // FreePascal emits for code the linker has discarded.
       property CodeLow:TpvUInt64 read fCodeLow;
       property CodeHigh:TpvUInt64 read fCodeHigh;
       property Sections:TImageSections read fSections;
       // The processor the image was built for, as the value a COFF header
       // uses. An ELF machine is translated into the matching one, so that a
       // consumer does not have to know which container this came out of.
       property Machine:TpvUInt16 read fMachine;
     end;

implementation

const IMAGE_FILE_MACHINE_UNKNOWN=TpvUInt16($0000);
      IMAGE_FILE_MACHINE_I386=TpvUInt16($014c);
      IMAGE_FILE_MACHINE_ARMNT=TpvUInt16($01c4);
      IMAGE_FILE_MACHINE_AMD64=TpvUInt16($8664);
      IMAGE_FILE_MACHINE_ARM64=TpvUInt16($aa64);

      EM_386=TpvUInt16(3);
      EM_ARM=TpvUInt16(40);
      EM_X86_64=TpvUInt16(62);
      EM_AARCH64=TpvUInt16(183);

      PT_LOAD=TpvUInt32(1);

      SHF_EXECINSTR=TpvUInt64($4);
      SHF_ALLOC=TpvUInt64($2);

      SHF_WRITE=TpvUInt64($1);

      SHT_NOBITS=TpvUInt32(8);

      IMAGE_SCN_CNT_CODE=TpvUInt32($00000020);
      IMAGE_SCN_CNT_INITIALIZED_DATA=TpvUInt32($00000040);
      IMAGE_SCN_CNT_UNINITIALIZED_DATA=TpvUInt32($00000080);
      IMAGE_SCN_MEM_EXECUTE=TpvUInt32($20000000);
      IMAGE_SCN_MEM_READ=TpvUInt32($40000000);
      IMAGE_SCN_MEM_WRITE=TpvUInt32($80000000);

constructor TImageFile.Create;
begin
 inherited Create;
 fStream:=nil;
 fFormat:=iffUnknown;
 fImageBase:=0;
 fCodeLow:=0;
 fCodeHigh:=0;
 fSections:=nil;
 fSymbolTableOffset:=0;
 fSymbolCount:=0;
 fELF64:=true;
 fMachine:=IMAGE_FILE_MACHINE_UNKNOWN;
end;

destructor TImageFile.Destroy;
begin
 FreeAndNil(fStream);
 fSections:=nil;
 inherited Destroy;
end;

procedure TImageFile.Close;
begin
 FreeAndNil(fStream);
end;

function TImageFile.Open(const aFileName:String):Boolean;
var Signature:array[0..3] of AnsiChar;
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
 fStream.ReadBuffer(Signature,4);
 if (Signature[0]='M') and (Signature[1]='Z') then begin
  fFormat:=iffPE;
  result:=ReadPE;
 end else if (Signature[0]=#$7f) and (Signature[1]='E') and (Signature[2]='L') and (Signature[3]='F') then begin
  fFormat:=iffELF;
  result:=ReadELF;
 end;
 if result then begin
  ComputeCodeRange;
 end;
end;

procedure TImageFile.ComputeCodeRange;
var Index:TpvInt32;
    LowAddress,HighAddress,SectionEnd:TpvUInt64;
    Found:Boolean;
begin
 Found:=false;
 LowAddress:=0;
 HighAddress:=0;
 for Index:=0 to length(fSections)-1 do begin
  if fSections[Index].Executable and (fSections[Index].VirtualAddress>0) then begin
   SectionEnd:=fSections[Index].VirtualAddress+fSections[Index].VirtualSize;
   if not Found then begin
    LowAddress:=fSections[Index].VirtualAddress;
    HighAddress:=SectionEnd;
    Found:=true;
   end else begin
    if fSections[Index].VirtualAddress<LowAddress then begin
     LowAddress:=fSections[Index].VirtualAddress;
    end;
    if SectionEnd>HighAddress then begin
     HighAddress:=SectionEnd;
    end;
   end;
  end;
 end;
 if Found then begin
  fCodeLow:=LowAddress;
  fCodeHigh:=HighAddress;
 end else begin
  // Nothing recognizable, so no filtering rather than filtering everything out.
  fCodeLow:=0;
  fCodeHigh:=High(TpvUInt64);
 end;
end;

function TImageFile.StringTableOffset:TpvInt64;
begin
 // For PE the COFF string table follows directly behind the symbol table. A
 // symbol count of zero does not mean there is no string table: an executable
 // whose debug information was linked out into a separate file keeps the
 // pointer and the string table, and needs it, because the long section names,
 // including .gnu_debuglink itself, are stored there.
 if (fFormat<>iffPE) or (fSymbolTableOffset=0) then begin
  result:=0;
 end else begin
  result:=TpvInt64(fSymbolTableOffset)+(TpvInt64(fSymbolCount)*18);
 end;
end;

function TImageFile.ReadStringAt(const aAbsoluteOffset:TpvInt64):String;
var Available:TpvInt64;
    Buffer:array[0..1023] of AnsiChar;
begin
 result:='';
 if (aAbsoluteOffset<=0) or (aAbsoluteOffset>=fStream.Size) then begin
  exit;
 end;
 Available:=fStream.Size-aAbsoluteOffset;
 if Available>TpvInt64(SizeOf(Buffer)-1) then begin
  Available:=SizeOf(Buffer)-1;
 end;
 FillChar(Buffer,SizeOf(Buffer),#0);
 fStream.Seek(aAbsoluteOffset,soBeginning);
 fStream.ReadBuffer(Buffer,Available);
 result:=String(PAnsiChar(@Buffer[0]));
end;

function TImageFile.ReadStringTableEntry(const aOffset:TpvInt64):String;
var Base:TpvInt64;
begin
 Base:=StringTableOffset;
 if (Base<=0) or (aOffset<0) then begin
  result:='';
 end else begin
  result:=ReadStringAt(Base+aOffset);
 end;
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

function TImageFile.ReadPE:Boolean;
var NewHeaderOffset:TpvUInt32;
    PESignature:array[0..3] of AnsiChar;
    NumberOfSections:TpvUInt16;
    SizeOfOptionalHeader:TpvUInt16;
    OptionalMagic:TpvUInt16;
    Value32:TpvUInt32;
    SectionIndex:TpvInt32;
    RawName:array[0..7] of AnsiChar;
    VirtualSize,VirtualAddress,RawSize,RawOffset,Characteristics:TpvUInt32;
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
 fStream.Seek(TpvInt64(NewHeaderOffset)+4,soBeginning);
 fStream.ReadBuffer(fMachine,SizeOf(TpvUInt16));

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
  fStream.Seek(TpvInt64(NewHeaderOffset)+24+TpvInt64(SizeOfOptionalHeader)+(TpvInt64(SectionIndex)*40)+36,soBeginning);
  fStream.ReadBuffer(Characteristics,SizeOf(TpvUInt32));
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
  // PE section addresses are relative to the image base.
  fSections[SectionIndex].VirtualAddress:=fImageBase+VirtualAddress;
  fSections[SectionIndex].VirtualSize:=VirtualSize;
  fSections[SectionIndex].FileOffset:=RawOffset;
  fSections[SectionIndex].RawSize:=RawSize;
  fSections[SectionIndex].Executable:=(Characteristics and IMAGE_SCN_MEM_EXECUTE)<>0;
  fSections[SectionIndex].Characteristics:=Characteristics;
 end;

 result:=true;

end;

function TImageFile.ReadELF:Boolean;
var ELFClass,DataEncoding:TpvUInt8;
    ELFMachine:TpvUInt16;
    ProgramHeaderOffset,SectionHeaderOffset:TpvUInt64;
    ProgramHeaderEntrySize,ProgramHeaderCount:TpvUInt16;
    SectionHeaderEntrySize,SectionHeaderCount,SectionNameIndex:TpvUInt16;
    Index:TpvInt32;
    SegmentType:TpvUInt32;
    SegmentAddress:TpvUInt64;
    HaveBase:Boolean;
    NameOffset,SectionType:TpvUInt32;
    Flags,Address,Offset,Size:TpvUInt64;
    NameTableOffset:TpvUInt64;
begin

 result:=false;

 fStream.Seek(4,soBeginning);
 fStream.ReadBuffer(ELFClass,SizeOf(TpvUInt8));
 fStream.ReadBuffer(DataEncoding,SizeOf(TpvUInt8));

 // Only 64 bit little endian is handled, which is what every target PasVulkan
 // builds for on Linux uses.
 if (ELFClass<>2) or (DataEncoding<>1) then begin
  exit;
 end;
 fELF64:=true;

 fStream.Seek($12,soBeginning);
 fStream.ReadBuffer(ELFMachine,SizeOf(TpvUInt16));
 case ELFMachine of
  EM_386:begin
   fMachine:=IMAGE_FILE_MACHINE_I386;
  end;
  EM_ARM:begin
   fMachine:=IMAGE_FILE_MACHINE_ARMNT;
  end;
  EM_X86_64:begin
   fMachine:=IMAGE_FILE_MACHINE_AMD64;
  end;
  EM_AARCH64:begin
   fMachine:=IMAGE_FILE_MACHINE_ARM64;
  end;
  else begin
   fMachine:=IMAGE_FILE_MACHINE_UNKNOWN;
  end;
 end;

 fStream.Seek($20,soBeginning);
 fStream.ReadBuffer(ProgramHeaderOffset,SizeOf(TpvUInt64));
 fStream.ReadBuffer(SectionHeaderOffset,SizeOf(TpvUInt64));
 fStream.Seek($36,soBeginning);
 fStream.ReadBuffer(ProgramHeaderEntrySize,SizeOf(TpvUInt16));
 fStream.ReadBuffer(ProgramHeaderCount,SizeOf(TpvUInt16));
 fStream.ReadBuffer(SectionHeaderEntrySize,SizeOf(TpvUInt16));
 fStream.ReadBuffer(SectionHeaderCount,SizeOf(TpvUInt16));
 fStream.ReadBuffer(SectionNameIndex,SizeOf(TpvUInt16));

 // The image base is the lowest loadable segment address. For a position
 // independent executable that is zero, and the runtime side then has to add
 // the load bias it finds for itself.
 HaveBase:=false;
 fImageBase:=0;
 for Index:=0 to ProgramHeaderCount-1 do begin
  fStream.Seek(TpvInt64(ProgramHeaderOffset)+(TpvInt64(Index)*TpvInt64(ProgramHeaderEntrySize)),soBeginning);
  fStream.ReadBuffer(SegmentType,SizeOf(TpvUInt32));
  fStream.Seek(TpvInt64(ProgramHeaderOffset)+(TpvInt64(Index)*TpvInt64(ProgramHeaderEntrySize))+16,soBeginning);
  fStream.ReadBuffer(SegmentAddress,SizeOf(TpvUInt64));
  if SegmentType=PT_LOAD then begin
   if (not HaveBase) or (SegmentAddress<fImageBase) then begin
    fImageBase:=SegmentAddress;
    HaveBase:=true;
   end;
  end;
 end;

 if (SectionHeaderCount=0) or (SectionNameIndex>=SectionHeaderCount) then begin
  exit;
 end;

 // The section name table is itself a section, so its offset has to be read
 // before the names of the others can be resolved.
 fStream.Seek(TpvInt64(SectionHeaderOffset)+(TpvInt64(SectionNameIndex)*TpvInt64(SectionHeaderEntrySize))+24,soBeginning);
 fStream.ReadBuffer(NameTableOffset,SizeOf(TpvUInt64));

 SetLength(fSections,SectionHeaderCount);
 for Index:=0 to SectionHeaderCount-1 do begin
  fStream.Seek(TpvInt64(SectionHeaderOffset)+(TpvInt64(Index)*TpvInt64(SectionHeaderEntrySize)),soBeginning);
  fStream.ReadBuffer(NameOffset,SizeOf(TpvUInt32));
  fStream.ReadBuffer(SectionType,SizeOf(TpvUInt32));
  fStream.ReadBuffer(Flags,SizeOf(TpvUInt64));
  fStream.ReadBuffer(Address,SizeOf(TpvUInt64));
  fStream.ReadBuffer(Offset,SizeOf(TpvUInt64));
  fStream.ReadBuffer(Size,SizeOf(TpvUInt64));
  fSections[Index].Name:=ReadStringAt(TpvInt64(NameTableOffset)+TpvInt64(NameOffset));
  fSections[Index].VirtualAddress:=Address;
  fSections[Index].VirtualSize:=Size;
  fSections[Index].FileOffset:=Offset;
  fSections[Index].RawSize:=Size;
  fSections[Index].Executable:=((Flags and SHF_EXECINSTR)<>0) and ((Flags and SHF_ALLOC)<>0);
  fSections[Index].Characteristics:=0;
  if (Flags and SHF_ALLOC)<>0 then begin
   fSections[Index].Characteristics:=fSections[Index].Characteristics or IMAGE_SCN_MEM_READ;
   if SectionType=SHT_NOBITS then begin
    fSections[Index].Characteristics:=fSections[Index].Characteristics or IMAGE_SCN_CNT_UNINITIALIZED_DATA;
   end else begin
    fSections[Index].Characteristics:=fSections[Index].Characteristics or IMAGE_SCN_CNT_INITIALIZED_DATA;
   end;
  end;
  if (Flags and SHF_WRITE)<>0 then begin
   fSections[Index].Characteristics:=fSections[Index].Characteristics or IMAGE_SCN_MEM_WRITE;
  end;
  if fSections[Index].Executable then begin
   fSections[Index].Characteristics:=(fSections[Index].Characteristics or IMAGE_SCN_MEM_EXECUTE or IMAGE_SCN_CNT_CODE) and not IMAGE_SCN_CNT_INITIALIZED_DATA;
  end;
  // SHT_SYMTAB is 2, and its sh_link names the string table section holding
  // the symbol names.
  if SectionType=2 then begin
   fStream.Seek(TpvInt64(SectionHeaderOffset)+(TpvInt64(Index)*TpvInt64(SectionHeaderEntrySize))+40,soBeginning);
   fStream.ReadBuffer(fSymbolTableOffset,SizeOf(TpvUInt64));
   fSymbolTableOffset:=Offset;
   fSymbolCount:=TpvUInt32(Size div 24);
  end;
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
 // For PE the raw size is rounded up to the file alignment while the virtual
 // size is the real one. Debug sections are usually not loaded, so the virtual
 // size can read as zero, in which case the raw size is all there is to go by.
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
    Name,Directory,Candidate:String;
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
begin
 if assigned(aEvent) then begin
  case fFormat of
   iffPE:begin
    EnumeratePESymbols(aEvent);
   end;
   iffELF:begin
    EnumerateELFSymbols(aEvent);
   end;
   else begin
   end;
  end;
 end;
end;

procedure TImageFile.EnumeratePESymbols(const aEvent:TImageSymbolEvent);
const SymbolRecordSize=18;
var Index:TpvUInt32;
    Base:TpvInt64;
    ShortName:array[0..7] of AnsiChar;
    Value:TpvUInt32;
    SectionNumber:TpvInt16;
    StorageClass,AuxiliaryCount:TpvUInt8;
    TypeValue:TpvUInt16;
    Zeroes,NameOffset:TpvUInt32;
    Name:String;
    NameLength:TpvInt32;
begin

 if (fSymbolTableOffset=0) or (fSymbolCount=0) then begin
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

procedure TImageFile.EnumerateELFSymbols(const aEvent:TImageSymbolEvent);
const SymbolRecordSize=24;
      STT_FUNC=2;
var Index:TpvUInt32;
    StringSectionOffset:TpvUInt64;
    NameOffset:TpvUInt32;
    Info,Other:TpvUInt8;
    SectionIndex:TpvUInt16;
    Value,Size:TpvUInt64;
    Name:String;
    Section:TImageSection;
begin

 if (fSymbolTableOffset=0) or (fSymbolCount=0) then begin
  exit;
 end;

 // Symbol names live in .strtab, which is a plain string section.
 if not FindSection('.strtab',Section) then begin
  exit;
 end;
 StringSectionOffset:=Section.FileOffset;

 for Index:=0 to fSymbolCount-1 do begin

  fStream.Seek(TpvInt64(fSymbolTableOffset)+(TpvInt64(Index)*SymbolRecordSize),soBeginning);
  fStream.ReadBuffer(NameOffset,SizeOf(TpvUInt32));
  fStream.ReadBuffer(Info,SizeOf(TpvUInt8));
  fStream.ReadBuffer(Other,SizeOf(TpvUInt8));
  fStream.ReadBuffer(SectionIndex,SizeOf(TpvUInt16));
  fStream.ReadBuffer(Value,SizeOf(TpvUInt64));
  fStream.ReadBuffer(Size,SizeOf(TpvUInt64));

  // SHN_UNDEF is zero and SHN_ABS and above are not addresses in this image.
  if (NameOffset=0) or (SectionIndex=0) or (SectionIndex>=$ff00) then begin
   continue;
  end;

  // The low nibble of the info byte is the symbol type. Only routines are of
  // interest, which also keeps data labels out of stack traces.
  if (Info and $0f)<>STT_FUNC then begin
   continue;
  end;

  Name:=ReadStringAt(TpvInt64(StringSectionOffset)+TpvInt64(NameOffset));
  if length(Name)>0 then begin
   // ELF symbol values are already link time virtual addresses.
   aEvent(Value,Name);
  end;

 end;

end;

end.
