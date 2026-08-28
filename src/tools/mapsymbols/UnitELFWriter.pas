// Writes a standalone ELF file holding nothing but debug information.
//
// This is the companion of UnitDWARFWriter. The DWARF sections it produces need
// a container before any tool will look at them, and an ELF is the one every
// consumer of DWARF understands. The result is not runnable and not meant to
// be: it carries no code, only the sections which describe where the code of
// another binary came from.
//
// Addresses inside are absolute link time addresses of the original image, so
// addr2line and gdb resolve an address straight out of a crash log without
// being told a load base first.
unit UnitELFWriter;
{$ifdef fpc}
 {$mode delphi}
{$endif}

interface

uses SysUtils,
     Classes,
     PasVulkan.Types;

// The processor numbers an ELF header uses. Here rather than in the
// implementation, because the caller picks one.
const EM_386=TpvUInt16(3);
      EM_ARM=TpvUInt16(40);
      EM_X86_64=TpvUInt16(62);
      EM_AARCH64=TpvUInt16(183);

type TELFWriterSymbol=record
      Name:String;
      Address:TpvUInt64;
      Size:TpvUInt64;
     end;

     TELFWriterSymbols=array of TELFWriterSymbol;

     TELFWriter=class
      private
       type TSectionRecord=record
             Name:String;
             SectionType:TpvUInt32;
             Flags:TpvUInt64;
             Address:TpvUInt64;
             Data:TMemoryStream;
             OwnsData:Boolean;
             Link:TpvUInt32;
             Info:TpvUInt32;
             EntrySize:TpvUInt64;
             FileOffset:TpvUInt64;
             NameOffset:TpvUInt32;
            end;
            TSectionRecords=array of TSectionRecord;
      private
       fSections:TSectionRecords;
       fSymbols:TELFWriterSymbols;
       fSymbolCount:TpvSizeInt;
       fTextAddress:TpvUInt64;
       fTextSize:TpvUInt64;
       fMachine:TpvUInt16;
       fBits:TpvUInt8;
       fBigEndian:Boolean;
       fFlags:TpvUInt32;
       // Write one number in the byte order of the described image rather than
       // in the one of the machine this runs on.
       procedure WriteU16(const aStream:TStream;const aValue:TpvUInt16);
       procedure WriteU32(const aStream:TStream;const aValue:TpvUInt32);
       procedure WriteU64(const aStream:TStream;const aValue:TpvUInt64);
       function AddSection(const aName:String;const aSectionType:TpvUInt32;const aFlags:TpvUInt64;const aData:TMemoryStream;const aOwnsData:Boolean):TpvSizeInt;
      public
       constructor Create;
       destructor Destroy; override;
       procedure AddDebugSection(const aName:String;const aData:TMemoryStream);
       procedure AddSymbol(const aName:String;const aAddress,aSize:TpvUInt64);
       // Describes the code range of the original image, so that the written
       // file carries a matching, contentless text section. Without it a
       // consumer has no idea which addresses this file is about.
       procedure SetTextRange(const aAddress,aSize:TpvUInt64);
       // Which processor the described image is for, as an ELF machine number.
       // Defaults to x86-64, which is what a Delphi or FreePascal build for a
       // desktop is, and is set from the image where that is not the case.
       //
       property Machine:TpvUInt16 read fMachine write fMachine;
       // Whether the container is written in its thirty two or its sixty four
       // bit shape. That decides the size of the header, of a section header
       // and of a symbol entry, and for a symbol also the order of its fields,
       // which is not the same in the two.
       property Bits:TpvUInt8 read fBits write fBits;
       // The byte order of the described image. Goes into the identification
       // byte of the header and into every number of the file.
       property BigEndian:Boolean read fBigEndian write fBigEndian;
       // The processor specific header flags, carried over from the described
       // image. Nothing on x86 uses them, but on arm and on mips they state
       // which abi and which instruction set the image is for, and a file which
       // claims none of that while the image it belongs to does is a file two
       // readers can disagree about.
       property Flags:TpvUInt32 read fFlags write fFlags;
       procedure SaveToFile(const aFileName:String);
     end;

implementation

const SHT_PROGBITS=TpvUInt32(1);
      SHT_SYMTAB=TpvUInt32(2);
      SHT_STRTAB=TpvUInt32(3);
      SHT_NOBITS=TpvUInt32(8);

      SHF_ALLOC=TpvUInt64($2);
      SHF_EXECINSTR=TpvUInt64($4);

      STT_FUNC=$02;
      STB_GLOBAL=$01;

      // The two shapes of the container. A thirty two bit ELF is not a smaller
      // version of the same structures, its fields are narrower and a symbol
      // entry even orders them differently.
      ELFHeaderSize64=64;
      SectionHeaderSize64=64;
      SymbolSize64=24;

      ELFHeaderSize32=52;
      SectionHeaderSize32=40;
      SymbolSize32=16;

constructor TELFWriter.Create;
begin
 inherited Create;
 fSections:=nil;
 fSymbols:=nil;
 fSymbolCount:=0;
 fTextAddress:=0;
 fTextSize:=0;
 fMachine:=EM_X86_64;
 fBits:=64;
 fBigEndian:=false;
 fFlags:=0;
end;

procedure TELFWriter.WriteU16(const aStream:TStream;const aValue:TpvUInt16);
var Bytes:array[0..1] of TpvUInt8;
begin
 if fBigEndian then begin
  Bytes[0]:=TpvUInt8(aValue shr 8);
  Bytes[1]:=TpvUInt8(aValue and $ff);
 end else begin
  Bytes[0]:=TpvUInt8(aValue and $ff);
  Bytes[1]:=TpvUInt8(aValue shr 8);
 end;
 aStream.WriteBuffer(Bytes[0],2);
end;

procedure TELFWriter.WriteU32(const aStream:TStream;const aValue:TpvUInt32);
begin
 if fBigEndian then begin
  WriteU16(aStream,TpvUInt16(aValue shr 16));
  WriteU16(aStream,TpvUInt16(aValue and $ffff));
 end else begin
  WriteU16(aStream,TpvUInt16(aValue and $ffff));
  WriteU16(aStream,TpvUInt16(aValue shr 16));
 end;
end;

procedure TELFWriter.WriteU64(const aStream:TStream;const aValue:TpvUInt64);
begin
 if fBigEndian then begin
  WriteU32(aStream,TpvUInt32(aValue shr 32));
  WriteU32(aStream,TpvUInt32(aValue and TpvUInt64($ffffffff)));
 end else begin
  WriteU32(aStream,TpvUInt32(aValue and TpvUInt64($ffffffff)));
  WriteU32(aStream,TpvUInt32(aValue shr 32));
 end;
end;

destructor TELFWriter.Destroy;
var Index:TpvSizeInt;
begin
 for Index:=0 to length(fSections)-1 do begin
  if fSections[Index].OwnsData then begin
   FreeAndNil(fSections[Index].Data);
  end;
 end;
 fSections:=nil;
 fSymbols:=nil;
 inherited Destroy;
end;

function TELFWriter.AddSection(const aName:String;const aSectionType:TpvUInt32;const aFlags:TpvUInt64;const aData:TMemoryStream;const aOwnsData:Boolean):TpvSizeInt;
begin
 result:=length(fSections);
 SetLength(fSections,result+1);
 fSections[result].Name:=aName;
 fSections[result].SectionType:=aSectionType;
 fSections[result].Flags:=aFlags;
 fSections[result].Address:=0;
 fSections[result].Data:=aData;
 fSections[result].OwnsData:=aOwnsData;
 fSections[result].Link:=0;
 fSections[result].Info:=0;
 fSections[result].EntrySize:=0;
 fSections[result].FileOffset:=0;
 fSections[result].NameOffset:=0;
end;

procedure TELFWriter.AddDebugSection(const aName:String;const aData:TMemoryStream);
begin
 if assigned(aData) and (aData.Size>0) then begin
  AddSection(aName,SHT_PROGBITS,0,aData,false);
 end;
end;

procedure TELFWriter.AddSymbol(const aName:String;const aAddress,aSize:TpvUInt64);
begin
 if fSymbolCount>=length(fSymbols) then begin
  SetLength(fSymbols,(fSymbolCount+1)*2);
 end;
 fSymbols[fSymbolCount].Name:=aName;
 fSymbols[fSymbolCount].Address:=aAddress;
 fSymbols[fSymbolCount].Size:=aSize;
 inc(fSymbolCount);
end;

procedure TELFWriter.SetTextRange(const aAddress,aSize:TpvUInt64);
begin
 fTextAddress:=aAddress;
 fTextSize:=aSize;
end;

procedure TELFWriter.SaveToFile(const aFileName:String);
var Stream:TFileStream;
    StringTable,SymbolTable,SectionNames:TMemoryStream;
    Index,TextIndex,SymbolTableIndex,StringTableIndex,SectionNameIndex:TpvSizeInt;
    Offset,SectionHeaderOffset:TpvUInt64;
    HeaderSize,SectionHeaderSize:TpvUInt16;
    Value8,Value8Zero:TpvUInt8;
    Value16:TpvUInt16;
    Value32:TpvUInt32;
    Value64:TpvUInt64;
    Raw:TpvRawByteString;
    Zero:AnsiChar;
    NameOffset:TpvUInt32;

 function AppendString(const aStream:TMemoryStream;const aValue:String):TpvUInt32;
 var Bytes:TpvRawByteString;
     Terminator:AnsiChar;
 begin
  result:=TpvUInt32(aStream.Position);
{$ifdef fpc}
  Bytes:=TpvRawByteString(aValue);
{$else}
  Bytes:=TpvRawByteString(UTF8Encode(aValue));
{$endif}
  if length(Bytes)>0 then begin
   aStream.WriteBuffer(Bytes[1],length(Bytes));
  end;
  Terminator:=#0;
  aStream.WriteBuffer(Terminator,1);
 end;

begin

 // Everything below asks whether this is thirty two and treats anything else as
 // sixty four, so a value which is neither would quietly come out as the wider
 // one while the header says whatever was set.
 if (fBits<>32) and (fBits<>64) then begin
  raise Exception.Create('ELF class must be thirty two or sixty four bits');
 end;

 StringTable:=TMemoryStream.Create;
 SymbolTable:=TMemoryStream.Create;
 SectionNames:=TMemoryStream.Create;
 try

  // A contentless text section, so that a consumer sees which address range
  // this file describes.
  TextIndex:=-1;
  if fTextSize>0 then begin
   TextIndex:=AddSection('.text',SHT_NOBITS,SHF_ALLOC or SHF_EXECINSTR,nil,false);
   fSections[TextIndex].Address:=fTextAddress;
  end;

  // The symbol table, with its names in a string table of its own. Index zero
  // has to be the reserved null symbol.
  //
  // The two shapes do not only differ in width. A sixty four bit entry reads
  // name, info, other, section, value, size, while a thirty two bit one reads
  // name, value, size, info, other, section. Writing one layout with the other
  // one's widths gives a table which is the right length and says nothing.
  Zero:=#0;
  StringTable.WriteBuffer(Zero,1);
  Value8:=0;
  if fBits=32 then begin
   WriteU32(SymbolTable,0); // name
   WriteU32(SymbolTable,0); // value
   WriteU32(SymbolTable,0); // size
   SymbolTable.WriteBuffer(Value8,SizeOf(TpvUInt8)); // info
   SymbolTable.WriteBuffer(Value8,SizeOf(TpvUInt8)); // other
   WriteU16(SymbolTable,0); // section
  end else begin
   WriteU32(SymbolTable,0); // name
   SymbolTable.WriteBuffer(Value8,SizeOf(TpvUInt8)); // info
   SymbolTable.WriteBuffer(Value8,SizeOf(TpvUInt8)); // other
   WriteU16(SymbolTable,0); // section
   WriteU64(SymbolTable,0); // value
   WriteU64(SymbolTable,0); // size
  end;

  for Index:=0 to fSymbolCount-1 do begin

   NameOffset:=AppendString(StringTable,fSymbols[Index].Name);
   // Binding in the high nibble, type in the low one.
   Value8:=TpvUInt8((STB_GLOBAL shl 4) or STT_FUNC);
   // Section indices in the file are shifted by one against this list, because
   // ELF reserves index zero for the mandatory null section.
   if TextIndex>=0 then begin
    Value16:=TpvUInt16(TextIndex+1);
   end else begin
    Value16:=0;
   end;

   if fBits=32 then begin
    WriteU32(SymbolTable,NameOffset);
    WriteU32(SymbolTable,TpvUInt32(fSymbols[Index].Address));
    WriteU32(SymbolTable,TpvUInt32(fSymbols[Index].Size));
    SymbolTable.WriteBuffer(Value8,SizeOf(TpvUInt8));
    Value8Zero:=0;
    SymbolTable.WriteBuffer(Value8Zero,SizeOf(TpvUInt8)); // other
    WriteU16(SymbolTable,Value16);
   end else begin
    WriteU32(SymbolTable,NameOffset);
    SymbolTable.WriteBuffer(Value8,SizeOf(TpvUInt8));
    Value8Zero:=0;
    SymbolTable.WriteBuffer(Value8Zero,SizeOf(TpvUInt8)); // other
    WriteU16(SymbolTable,Value16);
    WriteU64(SymbolTable,fSymbols[Index].Address);
    WriteU64(SymbolTable,fSymbols[Index].Size);
   end;

  end;

  StringTableIndex:=AddSection('.strtab',SHT_STRTAB,0,StringTable,false);
  SymbolTableIndex:=AddSection('.symtab',SHT_SYMTAB,0,SymbolTable,false);
  // Again the shift by one for the reserved null section. Pointing this at the
  // wrong section makes every consumer read the symbol names out of whatever
  // happens to sit at that index.
  fSections[SymbolTableIndex].Link:=TpvUInt32(StringTableIndex+1);
  // Every symbol here is global, so the first one already is the first global.
  fSections[SymbolTableIndex].Info:=1;
  if fBits=32 then begin
   fSections[SymbolTableIndex].EntrySize:=SymbolSize32;
  end else begin
   fSections[SymbolTableIndex].EntrySize:=SymbolSize64;
  end;

  SectionNameIndex:=AddSection('.shstrtab',SHT_STRTAB,0,SectionNames,false);

  // The section name table has to contain the names of all sections including
  // its own, and index zero has to be the empty name of the null section.
  SectionNames.WriteBuffer(Zero,1);
  for Index:=0 to length(fSections)-1 do begin
   fSections[Index].NameOffset:=AppendString(SectionNames,fSections[Index].Name);
  end;

  // Lay the file out: header, then the contents, then the section headers.
  if fBits=32 then begin
   HeaderSize:=ELFHeaderSize32;
   SectionHeaderSize:=SectionHeaderSize32;
  end else begin
   HeaderSize:=ELFHeaderSize64;
   SectionHeaderSize:=SectionHeaderSize64;
  end;
  Offset:=HeaderSize;
  for Index:=0 to length(fSections)-1 do begin
   fSections[Index].FileOffset:=Offset;
   if (fSections[Index].SectionType<>SHT_NOBITS) and assigned(fSections[Index].Data) then begin
    inc(Offset,TpvUInt64(fSections[Index].Data.Size));
   end;
  end;
  SectionHeaderOffset:=Offset;

  Stream:=TFileStream.Create(aFileName,fmCreate);
  try

   // ELF header
   Raw:=#$7f'ELF';
   Stream.WriteBuffer(Raw[1],4);
   if fBits=32 then begin
    Value8:=1;
   end else begin
    Value8:=2;
   end;
   Stream.WriteBuffer(Value8,1); // class
   if fBigEndian then begin
    Value8:=2;
   end else begin
    Value8:=1;
   end;
   Stream.WriteBuffer(Value8,1); // data encoding
   Value8:=1; Stream.WriteBuffer(Value8,1); // header version
   Value8:=0; Stream.WriteBuffer(Value8,1); // System V ABI
   Value64:=0; Stream.WriteBuffer(Value64,8); // ABI version and padding
   WriteU16(Stream,2); // ET_EXEC
   WriteU16(Stream,fMachine);
   WriteU32(Stream,1); // version
   if fBits=32 then begin
    WriteU32(Stream,0); // entry
    WriteU32(Stream,0); // program header offset
    WriteU32(Stream,TpvUInt32(SectionHeaderOffset));
   end else begin
    WriteU64(Stream,0); // entry
    WriteU64(Stream,0); // program header offset
    WriteU64(Stream,SectionHeaderOffset);
   end;
   WriteU32(Stream,fFlags); // flags
   WriteU16(Stream,HeaderSize);
   WriteU16(Stream,0); // program header entry size
   WriteU16(Stream,0); // program header count
   WriteU16(Stream,SectionHeaderSize);
   WriteU16(Stream,TpvUInt16(length(fSections)+1));
   WriteU16(Stream,TpvUInt16(SectionNameIndex+1));

   // Section contents
   for Index:=0 to length(fSections)-1 do begin
    if (fSections[Index].SectionType<>SHT_NOBITS) and assigned(fSections[Index].Data) then begin
     fSections[Index].Data.Position:=0;
     Stream.CopyFrom(fSections[Index].Data,fSections[Index].Data.Size);
    end;
   end;

   // Section headers, starting with the mandatory null entry.
   Value8:=0;
   for Index:=0 to TpvSizeInt(SectionHeaderSize)-1 do begin
    Stream.WriteBuffer(Value8,1);
   end;

   for Index:=0 to length(fSections)-1 do begin

    if fSections[Index].SectionType=SHT_NOBITS then begin
     Value64:=fTextSize;
    end else if assigned(fSections[Index].Data) then begin
     Value64:=TpvUInt64(fSections[Index].Data.Size);
    end else begin
     Value64:=0;
    end;

    if fBits=32 then begin
     // Every field of a thirty two bit section header is four bytes, in the
     // same order as the wider one.
     WriteU32(Stream,fSections[Index].NameOffset);
     WriteU32(Stream,fSections[Index].SectionType);
     WriteU32(Stream,TpvUInt32(fSections[Index].Flags));
     WriteU32(Stream,TpvUInt32(fSections[Index].Address));
     WriteU32(Stream,TpvUInt32(fSections[Index].FileOffset));
     WriteU32(Stream,TpvUInt32(Value64));
     WriteU32(Stream,fSections[Index].Link);
     WriteU32(Stream,fSections[Index].Info);
     WriteU32(Stream,1); // alignment
     WriteU32(Stream,TpvUInt32(fSections[Index].EntrySize));
    end else begin
     WriteU32(Stream,fSections[Index].NameOffset);
     WriteU32(Stream,fSections[Index].SectionType);
     WriteU64(Stream,fSections[Index].Flags);
     WriteU64(Stream,fSections[Index].Address);
     WriteU64(Stream,fSections[Index].FileOffset);
     WriteU64(Stream,Value64);
     WriteU32(Stream,fSections[Index].Link);
     WriteU32(Stream,fSections[Index].Info);
     WriteU64(Stream,1); // alignment
     WriteU64(Stream,fSections[Index].EntrySize);
    end;

   end;

  finally
   FreeAndNil(Stream);
  end;

 finally
  FreeAndNil(SectionNames);
  FreeAndNil(SymbolTable);
  FreeAndNil(StringTable);
 end;

end;

end.
