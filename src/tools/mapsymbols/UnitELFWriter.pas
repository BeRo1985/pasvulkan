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

      ELFHeaderSize=64;
      SectionHeaderSize=64;
      SymbolSize=24;

constructor TELFWriter.Create;
begin
 inherited Create;
 fSections:=nil;
 fSymbols:=nil;
 fSymbolCount:=0;
 fTextAddress:=0;
 fTextSize:=0;
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
    Value8:TpvUInt8;
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
  Zero:=#0;
  StringTable.WriteBuffer(Zero,1);
  FillChar(Value64,SizeOf(TpvUInt64),#0);
  Value32:=0;
  SymbolTable.WriteBuffer(Value32,SizeOf(TpvUInt32));
  Value8:=0;
  SymbolTable.WriteBuffer(Value8,SizeOf(TpvUInt8));
  SymbolTable.WriteBuffer(Value8,SizeOf(TpvUInt8));
  Value16:=0;
  SymbolTable.WriteBuffer(Value16,SizeOf(TpvUInt16));
  Value64:=0;
  SymbolTable.WriteBuffer(Value64,SizeOf(TpvUInt64));
  SymbolTable.WriteBuffer(Value64,SizeOf(TpvUInt64));

  for Index:=0 to fSymbolCount-1 do begin
   NameOffset:=AppendString(StringTable,fSymbols[Index].Name);
   SymbolTable.WriteBuffer(NameOffset,SizeOf(TpvUInt32));
   // Binding in the high nibble, type in the low one.
   Value8:=TpvUInt8((STB_GLOBAL shl 4) or STT_FUNC);
   SymbolTable.WriteBuffer(Value8,SizeOf(TpvUInt8));
   Value8:=0;
   SymbolTable.WriteBuffer(Value8,SizeOf(TpvUInt8));
   // Section indices in the file are shifted by one against this list, because
   // ELF reserves index zero for the mandatory null section.
   if TextIndex>=0 then begin
    Value16:=TpvUInt16(TextIndex+1);
   end else begin
    Value16:=0;
   end;
   SymbolTable.WriteBuffer(Value16,SizeOf(TpvUInt16));
   Value64:=fSymbols[Index].Address;
   SymbolTable.WriteBuffer(Value64,SizeOf(TpvUInt64));
   Value64:=fSymbols[Index].Size;
   SymbolTable.WriteBuffer(Value64,SizeOf(TpvUInt64));
  end;

  StringTableIndex:=AddSection('.strtab',SHT_STRTAB,0,StringTable,false);
  SymbolTableIndex:=AddSection('.symtab',SHT_SYMTAB,0,SymbolTable,false);
  // Again the shift by one for the reserved null section. Pointing this at the
  // wrong section makes every consumer read the symbol names out of whatever
  // happens to sit at that index.
  fSections[SymbolTableIndex].Link:=TpvUInt32(StringTableIndex+1);
  // Every symbol here is global, so the first one already is the first global.
  fSections[SymbolTableIndex].Info:=1;
  fSections[SymbolTableIndex].EntrySize:=SymbolSize;

  SectionNameIndex:=AddSection('.shstrtab',SHT_STRTAB,0,SectionNames,false);

  // The section name table has to contain the names of all sections including
  // its own, and index zero has to be the empty name of the null section.
  SectionNames.WriteBuffer(Zero,1);
  for Index:=0 to length(fSections)-1 do begin
   fSections[Index].NameOffset:=AppendString(SectionNames,fSections[Index].Name);
  end;

  // Lay the file out: header, then the contents, then the section headers.
  Offset:=ELFHeaderSize;
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
   Value8:=2; Stream.WriteBuffer(Value8,1); // 64 bit
   Value8:=1; Stream.WriteBuffer(Value8,1); // little endian
   Value8:=1; Stream.WriteBuffer(Value8,1); // header version
   Value8:=0; Stream.WriteBuffer(Value8,1); // System V ABI
   Value64:=0; Stream.WriteBuffer(Value64,8); // ABI version and padding
   Value16:=2; Stream.WriteBuffer(Value16,2); // ET_EXEC
   Value16:=62; Stream.WriteBuffer(Value16,2); // EM_X86_64
   Value32:=1; Stream.WriteBuffer(Value32,4); // version
   Value64:=0; Stream.WriteBuffer(Value64,8); // entry
   Value64:=0; Stream.WriteBuffer(Value64,8); // program header offset
   Stream.WriteBuffer(SectionHeaderOffset,8);
   Value32:=0; Stream.WriteBuffer(Value32,4); // flags
   Value16:=ELFHeaderSize; Stream.WriteBuffer(Value16,2);
   Value16:=0; Stream.WriteBuffer(Value16,2); // program header entry size
   Value16:=0; Stream.WriteBuffer(Value16,2); // program header count
   Value16:=SectionHeaderSize; Stream.WriteBuffer(Value16,2);
   Value16:=TpvUInt16(length(fSections)+1); Stream.WriteBuffer(Value16,2);
   Value16:=TpvUInt16(SectionNameIndex+1); Stream.WriteBuffer(Value16,2);

   // Section contents
   for Index:=0 to length(fSections)-1 do begin
    if (fSections[Index].SectionType<>SHT_NOBITS) and assigned(fSections[Index].Data) then begin
     fSections[Index].Data.Position:=0;
     Stream.CopyFrom(fSections[Index].Data,fSections[Index].Data.Size);
    end;
   end;

   // Section headers, starting with the mandatory null entry.
   FillChar(Value64,SizeOf(TpvUInt64),#0);
   for Index:=0 to 7 do begin
    Value64:=0;
    Stream.WriteBuffer(Value64,8);
   end;

   for Index:=0 to length(fSections)-1 do begin
    Stream.WriteBuffer(fSections[Index].NameOffset,4);
    Stream.WriteBuffer(fSections[Index].SectionType,4);
    Stream.WriteBuffer(fSections[Index].Flags,8);
    Stream.WriteBuffer(fSections[Index].Address,8);
    Stream.WriteBuffer(fSections[Index].FileOffset,8);
    if fSections[Index].SectionType=SHT_NOBITS then begin
     Value64:=fTextSize;
    end else if assigned(fSections[Index].Data) then begin
     Value64:=TpvUInt64(fSections[Index].Data.Size);
    end else begin
     Value64:=0;
    end;
    Stream.WriteBuffer(Value64,8);
    Stream.WriteBuffer(fSections[Index].Link,4);
    Stream.WriteBuffer(fSections[Index].Info,4);
    Value64:=1;
    Stream.WriteBuffer(Value64,8); // alignment
    Stream.WriteBuffer(fSections[Index].EntrySize,8);
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
