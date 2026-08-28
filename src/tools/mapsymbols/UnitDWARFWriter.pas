// Writes DWARF debug information out of a collected symbol table.
//
// The point of this is the other direction from UnitDWARFLine: a Delphi build
// keeps its line numbers only in a .map file, which no standard tool reads, so
// nothing outside Delphi itself can turn a crash address into a source line.
// Emitting the same information as DWARF makes addr2line, gdb, objdump and
// everything else built on BFD or LLVM work on a Delphi binary.
//
// Three sections are produced:
//
//   .debug_line   the line number programs, one per unit
//   .debug_info   one compile unit DIE per unit, with the subprograms as
//                 children, which is what ties a line program to a source file
//   .debug_abbrev the two abbreviations those DIEs use
//
// A bare .debug_line would be useless on its own, since a consumer reaches the
// line programs through the DW_AT_stmt_list attribute of a compile unit rather
// than by scanning the section.
//
// DWARF version 2 is emitted, with inline strings rather than a .debug_str
// section, and with plain advance opcodes rather than special ones. That is a
// little larger than what a compiler would produce, and considerably easier to
// be sure about.
unit UnitDWARFWriter;
{$ifdef fpc}
 {$mode delphi}
{$endif}

interface

uses SysUtils,
     Classes,
     PasVulkan.Types,
     UnitSymbolBuilder;

type TDWARFWriter=class
      private
       fBuilder:TSymbolBuilder;
       fDebugLine:TMemoryStream;
       fDebugInfo:TMemoryStream;
       fDebugAbbrev:TMemoryStream;
       fAddressSize:TpvUInt8;
       fBigEndian:Boolean;
       fLineRowCount:TpvSizeInt;
       procedure WriteByte(const aStream:TMemoryStream;const aValue:TpvUInt8);
       procedure WriteUInt16(const aStream:TMemoryStream;const aValue:TpvUInt16);
       procedure WriteUInt32(const aStream:TMemoryStream;const aValue:TpvUInt32);
       procedure WriteUInt64(const aStream:TMemoryStream;const aValue:TpvUInt64);
       // An address in the width the described image uses, which is what
       // DW_FORM_addr means and what the address size in the header announces.
       procedure WriteAddress(const aStream:TMemoryStream;const aValue:TpvUInt64);
       procedure WriteULEB128(const aStream:TMemoryStream;const aValue:TpvUInt64);
       procedure WriteSLEB128(const aStream:TMemoryStream;const aValue:TpvInt64);
       procedure WriteZeroTerminated(const aStream:TMemoryStream;const aValue:String);
       procedure BuildAbbrev;
       function BuildLineProgram(const aUnitIndex:TpvSizeInt;const aFirstLine,aLastLine:TpvSizeInt):TpvUInt32;
       procedure BuildCompileUnit(const aUnitIndex:TpvSizeInt;const aStatementListOffset:TpvUInt32);
      public
       constructor Create(const aBuilder:TSymbolBuilder);
       destructor Destroy; override;
       procedure Build;
       // How wide an address of the described image is, four or eight bytes.
       // Has to be set before Build, since it goes into the header of every
       // compilation unit and decides how every address in them is written.
       property AddressSize:TpvUInt8 read fAddressSize write fAddressSize;
       // The byte order of the described image, which every number written
       // here follows. Has to be set before Build, like the address size.
       property BigEndian:Boolean read fBigEndian write fBigEndian;
       // How many rows the line programs came to. Not the number of collected
       // line records: a record which closes a sequence is not a row, and a
       // unit without line information contributes no program at all. Kept so
       // that a reader can be held against a number which was counted rather
       // than one which was worked out a second way.
       property LineRowCount:TpvSizeInt read fLineRowCount;
       property DebugLine:TMemoryStream read fDebugLine;
       property DebugInfo:TMemoryStream read fDebugInfo;
       property DebugAbbrev:TMemoryStream read fDebugAbbrev;
     end;

implementation

const DW_TAG_compile_unit=$11;
      DW_TAG_subprogram=$2e;

      DW_AT_name=$03;
      DW_AT_stmt_list=$10;
      DW_AT_low_pc=$11;
      DW_AT_high_pc=$12;
      DW_AT_language=$13;
      DW_AT_comp_dir=$1b;
      DW_AT_producer=$25;

      DW_FORM_addr=$01;
      DW_FORM_data4=$06;
      DW_FORM_string=$08;
      DW_FORM_data1=$0b;

      DW_LANG_Pascal83=$09;

      DW_LNS_copy=1;
      DW_LNS_advance_pc=2;
      DW_LNS_advance_line=3;
      DW_LNS_set_file=4;

      DW_LNE_end_sequence=1;
      DW_LNE_set_address=2;

      DWARFProducer='PasVulkan mapsymbols';

constructor TDWARFWriter.Create(const aBuilder:TSymbolBuilder);
begin
 inherited Create;
 fBuilder:=aBuilder;
 fDebugLine:=TMemoryStream.Create;
 fDebugInfo:=TMemoryStream.Create;
 fDebugAbbrev:=TMemoryStream.Create;
 // Eight and little endian unless the caller says otherwise, which is what a
 // desktop build is.
 fAddressSize:=8;
 fBigEndian:=false;
 fLineRowCount:=0;
end;

destructor TDWARFWriter.Destroy;
begin
 FreeAndNil(fDebugLine);
 FreeAndNil(fDebugInfo);
 FreeAndNil(fDebugAbbrev);
 inherited Destroy;
end;

procedure TDWARFWriter.WriteByte(const aStream:TMemoryStream;const aValue:TpvUInt8);
begin
 aStream.WriteBuffer(aValue,SizeOf(TpvUInt8));
end;

// Written out byte by byte, most significant first where the described image
// wants it that way. Writing the variable itself would give the order of the
// machine this tool runs on, which is not the question being asked.
procedure TDWARFWriter.WriteUInt16(const aStream:TMemoryStream;const aValue:TpvUInt16);
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

procedure TDWARFWriter.WriteUInt32(const aStream:TMemoryStream;const aValue:TpvUInt32);
begin
 if fBigEndian then begin
  WriteUInt16(aStream,TpvUInt16(aValue shr 16));
  WriteUInt16(aStream,TpvUInt16(aValue and $ffff));
 end else begin
  WriteUInt16(aStream,TpvUInt16(aValue and $ffff));
  WriteUInt16(aStream,TpvUInt16(aValue shr 16));
 end;
end;

procedure TDWARFWriter.WriteUInt64(const aStream:TMemoryStream;const aValue:TpvUInt64);
begin
 if fBigEndian then begin
  WriteUInt32(aStream,TpvUInt32(aValue shr 32));
  WriteUInt32(aStream,TpvUInt32(aValue and TpvUInt64($ffffffff)));
 end else begin
  WriteUInt32(aStream,TpvUInt32(aValue and TpvUInt64($ffffffff)));
  WriteUInt32(aStream,TpvUInt32(aValue shr 32));
 end;
end;

procedure TDWARFWriter.WriteAddress(const aStream:TMemoryStream;const aValue:TpvUInt64);
begin
 if fAddressSize=4 then begin
  WriteUInt32(aStream,TpvUInt32(aValue));
 end else begin
  WriteUInt64(aStream,aValue);
 end;
end;

procedure TDWARFWriter.WriteULEB128(const aStream:TMemoryStream;const aValue:TpvUInt64);
var Value:TpvUInt64;
    Current:TpvUInt8;
begin
 Value:=aValue;
 repeat
  Current:=TpvUInt8(Value and $7f);
  Value:=Value shr 7;
  if Value<>0 then begin
   Current:=Current or $80;
  end;
  WriteByte(aStream,Current);
 until Value=0;
end;

procedure TDWARFWriter.WriteSLEB128(const aStream:TMemoryStream;const aValue:TpvInt64);
var Value:TpvInt64;
    Current:TpvUInt8;
    More:Boolean;
begin
 Value:=aValue;
 More:=true;
 while More do begin
  Current:=TpvUInt8(Value and $7f);
  // Pascal shifts right logically even on a signed type, so a negative value
  // would never converge towards minus one and the encoding would run away.
  // The sign bits have to be put back by hand.
  if Value<0 then begin
   Value:=TpvInt64((TpvUInt64(Value) shr 7) or TpvUInt64($fe00000000000000));
  end else begin
   Value:=Value shr 7;
  end;
  if ((Value=0) and ((Current and $40)=0)) or
     ((Value=-1) and ((Current and $40)<>0)) then begin
   More:=false;
  end else begin
   Current:=Current or $80;
  end;
  WriteByte(aStream,Current);
 end;
end;

procedure TDWARFWriter.WriteZeroTerminated(const aStream:TMemoryStream;const aValue:String);
var Raw:TpvRawByteString;
    Zero:AnsiChar;
begin
{$ifdef fpc}
 Raw:=TpvRawByteString(aValue);
{$else}
 Raw:=TpvRawByteString(UTF8Encode(aValue));
{$endif}
 if length(Raw)>0 then begin
  aStream.WriteBuffer(Raw[1],length(Raw));
 end;
 Zero:=#0;
 aStream.WriteBuffer(Zero,1);
end;

procedure TDWARFWriter.BuildAbbrev;
begin

 // Abbreviation 1, the compile unit, which has children.
 WriteULEB128(fDebugAbbrev,1);
 WriteULEB128(fDebugAbbrev,DW_TAG_compile_unit);
 WriteByte(fDebugAbbrev,1);
 WriteULEB128(fDebugAbbrev,DW_AT_name);      WriteULEB128(fDebugAbbrev,DW_FORM_string);
 WriteULEB128(fDebugAbbrev,DW_AT_comp_dir);  WriteULEB128(fDebugAbbrev,DW_FORM_string);
 WriteULEB128(fDebugAbbrev,DW_AT_producer);  WriteULEB128(fDebugAbbrev,DW_FORM_string);
 WriteULEB128(fDebugAbbrev,DW_AT_language);  WriteULEB128(fDebugAbbrev,DW_FORM_data1);
 WriteULEB128(fDebugAbbrev,DW_AT_low_pc);    WriteULEB128(fDebugAbbrev,DW_FORM_addr);
 WriteULEB128(fDebugAbbrev,DW_AT_high_pc);   WriteULEB128(fDebugAbbrev,DW_FORM_addr);
 WriteULEB128(fDebugAbbrev,DW_AT_stmt_list); WriteULEB128(fDebugAbbrev,DW_FORM_data4);
 WriteULEB128(fDebugAbbrev,0);
 WriteULEB128(fDebugAbbrev,0);

 // Abbreviation 2, a subprogram, which has none.
 WriteULEB128(fDebugAbbrev,2);
 WriteULEB128(fDebugAbbrev,DW_TAG_subprogram);
 WriteByte(fDebugAbbrev,0);
 WriteULEB128(fDebugAbbrev,DW_AT_name);    WriteULEB128(fDebugAbbrev,DW_FORM_string);
 WriteULEB128(fDebugAbbrev,DW_AT_low_pc);  WriteULEB128(fDebugAbbrev,DW_FORM_addr);
 WriteULEB128(fDebugAbbrev,DW_AT_high_pc); WriteULEB128(fDebugAbbrev,DW_FORM_addr);
 WriteULEB128(fDebugAbbrev,0);
 WriteULEB128(fDebugAbbrev,0);

 // End of the abbreviation table.
 WriteULEB128(fDebugAbbrev,0);

end;

function TDWARFWriter.BuildLineProgram(const aUnitIndex:TpvSizeInt;const aFirstLine,aLastLine:TpvSizeInt):TpvUInt32;
var UnitRecord:TSymbolBuilder.TUnitRecord;
    LineRecord:TSymbolBuilder.TLineRecord;
    StartPosition,LengthPosition,HeaderLengthPosition,ProgramStartPosition,EndPosition:TpvInt64;
    Index:TpvSizeInt;
    CurrentAddress,RowAddress:TpvUInt64;
    CurrentLine:TpvInt64;
    ImageBase:TpvUInt64;
    InSequence:Boolean;
begin

 UnitRecord:=fBuilder.GetUnit(aUnitIndex);
 ImageBase:=fBuilder.ImageBase;

 result:=TpvUInt32(fDebugLine.Position);
 StartPosition:=fDebugLine.Position;

 // The unit length is only known at the end, so a placeholder goes in first.
 LengthPosition:=fDebugLine.Position;
 WriteUInt32(fDebugLine,0);

 WriteUInt16(fDebugLine,2);

 HeaderLengthPosition:=fDebugLine.Position;
 WriteUInt32(fDebugLine,0);

 WriteByte(fDebugLine,1); // minimum instruction length
 WriteByte(fDebugLine,1); // default is_stmt
 WriteByte(fDebugLine,TpvUInt8(TpvInt8(-5))); // line base
 WriteByte(fDebugLine,14); // line range
 WriteByte(fDebugLine,13); // opcode base

 // Operand counts of the twelve standard opcodes.
 WriteByte(fDebugLine,0);
 WriteByte(fDebugLine,1);
 WriteByte(fDebugLine,1);
 WriteByte(fDebugLine,1);
 WriteByte(fDebugLine,1);
 WriteByte(fDebugLine,0);
 WriteByte(fDebugLine,0);
 WriteByte(fDebugLine,0);
 WriteByte(fDebugLine,1);
 WriteByte(fDebugLine,0);
 WriteByte(fDebugLine,0);
 WriteByte(fDebugLine,1);

 // No include directories, so just the terminator.
 WriteByte(fDebugLine,0);

 // One file, the source of this unit, with a directory index of zero.
 WriteZeroTerminated(fDebugLine,ExtractFileName(UnitRecord.FileName));
 WriteULEB128(fDebugLine,0);
 WriteULEB128(fDebugLine,0);
 WriteULEB128(fDebugLine,0);
 WriteByte(fDebugLine,0);

 ProgramStartPosition:=fDebugLine.Position;

 // Patch the header length, which counts from behind its own field.
 EndPosition:=fDebugLine.Position;
 fDebugLine.Position:=HeaderLengthPosition;
 WriteUInt32(fDebugLine,TpvUInt32(ProgramStartPosition-(HeaderLengthPosition+4)));
 fDebugLine.Position:=EndPosition;

 CurrentAddress:=0;
 CurrentLine:=1;
 InSequence:=false;

 for Index:=aFirstLine to aLastLine do begin

  LineRecord:=fBuilder.GetLine(Index);
  RowAddress:=ImageBase+LineRecord.RVA;

  // A line number of zero is an end of sequence marker rather than a row. It is
  // written out as one, which is what keeps the last line of the sequence from
  // being carried on over the gap behind it.
  if LineRecord.LineNumber=0 then begin
   if InSequence then begin
    if RowAddress>CurrentAddress then begin
     WriteByte(fDebugLine,DW_LNS_advance_pc);
     WriteULEB128(fDebugLine,RowAddress-CurrentAddress);
     CurrentAddress:=RowAddress;
    end;
    WriteByte(fDebugLine,0);
    WriteULEB128(fDebugLine,1);
    WriteByte(fDebugLine,DW_LNE_end_sequence);
    // The state machine starts over behind an end of sequence.
    InSequence:=false;
    CurrentLine:=1;
   end;
   continue;
  end;

  if not InSequence then begin
   // A sequence has to start by stating where it is.
   WriteByte(fDebugLine,0);
   // The length of the extended opcode counts the opcode byte and the address
   // behind it, so it follows the address width as well.
   WriteULEB128(fDebugLine,TpvUInt64(fAddressSize)+1);
   WriteByte(fDebugLine,DW_LNE_set_address);
   WriteAddress(fDebugLine,RowAddress);
   CurrentAddress:=RowAddress;
   WriteByte(fDebugLine,DW_LNS_set_file);
   WriteULEB128(fDebugLine,1);
   InSequence:=true;
  end else if RowAddress>CurrentAddress then begin
   WriteByte(fDebugLine,DW_LNS_advance_pc);
   WriteULEB128(fDebugLine,RowAddress-CurrentAddress);
   CurrentAddress:=RowAddress;
  end;

  if TpvInt64(LineRecord.LineNumber)<>CurrentLine then begin
   WriteByte(fDebugLine,DW_LNS_advance_line);
   WriteSLEB128(fDebugLine,TpvInt64(LineRecord.LineNumber)-CurrentLine);
   CurrentLine:=LineRecord.LineNumber;
  end;

  WriteByte(fDebugLine,DW_LNS_copy);
  inc(fLineRowCount);

 end;

 // Close the last sequence one past the last byte of the unit, unless a marker
 // has closed it already.
 if InSequence then begin
  if (ImageBase+UnitRecord.StartRVA+UnitRecord.Size)>CurrentAddress then begin
   WriteByte(fDebugLine,DW_LNS_advance_pc);
   WriteULEB128(fDebugLine,(ImageBase+UnitRecord.StartRVA+UnitRecord.Size)-CurrentAddress);
  end;
  WriteByte(fDebugLine,0);
  WriteULEB128(fDebugLine,1);
  WriteByte(fDebugLine,DW_LNE_end_sequence);
 end;

 // Patch the unit length, which counts from behind its own field.
 EndPosition:=fDebugLine.Position;
 fDebugLine.Position:=LengthPosition;
 WriteUInt32(fDebugLine,TpvUInt32(EndPosition-(StartPosition+4)));
 fDebugLine.Position:=EndPosition;

end;

procedure TDWARFWriter.BuildCompileUnit(const aUnitIndex:TpvSizeInt;const aStatementListOffset:TpvUInt32);
var UnitRecord:TSymbolBuilder.TUnitRecord;
    SymbolRecord,NextSymbol:TSymbolBuilder.TSymbolRecord;
    StartPosition,LengthPosition,EndPosition:TpvInt64;
    Index:TpvSizeInt;
{$ifndef PasVulkanMapSymbolsLinearLookups}
    LowIndex,HighIndex,MiddleIndex:TpvSizeInt;
{$endif}
    ImageBase,UnitLow,UnitHigh,SymbolHigh:TpvUInt64;
    Directory:String;
begin

 UnitRecord:=fBuilder.GetUnit(aUnitIndex);
 ImageBase:=fBuilder.ImageBase;
 UnitLow:=ImageBase+UnitRecord.StartRVA;
 UnitHigh:=UnitLow+UnitRecord.Size;

 StartPosition:=fDebugInfo.Position;
 LengthPosition:=fDebugInfo.Position;
 WriteUInt32(fDebugInfo,0);

 WriteUInt16(fDebugInfo,2);  // version
 WriteUInt32(fDebugInfo,0);  // offset into .debug_abbrev
 WriteByte(fDebugInfo,fAddressSize);

 Directory:=ExtractFileDir(UnitRecord.FileName);
 if length(Directory)=0 then begin
  Directory:='.';
 end;

 WriteULEB128(fDebugInfo,1);
 WriteZeroTerminated(fDebugInfo,ExtractFileName(UnitRecord.FileName));
 WriteZeroTerminated(fDebugInfo,Directory);
 WriteZeroTerminated(fDebugInfo,DWARFProducer);
 WriteByte(fDebugInfo,DW_LANG_Pascal83);
 WriteAddress(fDebugInfo,UnitLow);
 WriteAddress(fDebugInfo,UnitHigh);
 WriteUInt32(fDebugInfo,aStatementListOffset);

 // Subprograms, taken from the symbols which fall inside this unit. They are
 // sorted by address, so the end of one is the start of the next.
{$ifdef PasVulkanMapSymbolsLinearLookups}
 for Index:=0 to fBuilder.SymbolCount-1 do begin
  SymbolRecord:=fBuilder.GetSymbol(Index);
  if ((ImageBase+SymbolRecord.RVA)<UnitLow) or ((ImageBase+SymbolRecord.RVA)>=UnitHigh) then begin
   continue;
  end;
{$else}
 // Being sorted by address also means the ones which fall inside this unit are
 // a run, so its beginning can be found instead of walked to. This is called
 // once per unit, and a build with thousands of units and a hundred thousand
 // symbols would otherwise spend hundreds of millions of iterations here, each
 // of them copying a record which carries a string.
 LowIndex:=0;
 HighIndex:=fBuilder.SymbolCount;
 while LowIndex<HighIndex do begin
  MiddleIndex:=LowIndex+((HighIndex-LowIndex) shr 1);
  if fBuilder.GetSymbol(MiddleIndex).RVA<UnitRecord.StartRVA then begin
   LowIndex:=MiddleIndex+1;
  end else begin
   HighIndex:=MiddleIndex;
  end;
 end;
 Index:=LowIndex;
 while Index<fBuilder.SymbolCount do begin
  SymbolRecord:=fBuilder.GetSymbol(Index);
  if (ImageBase+SymbolRecord.RVA)>=UnitHigh then begin
   break;
  end;
{$endif}
  SymbolHigh:=UnitHigh;
  if (Index+1)<fBuilder.SymbolCount then begin
   NextSymbol:=fBuilder.GetSymbol(Index+1);
   if ((ImageBase+NextSymbol.RVA)>(ImageBase+SymbolRecord.RVA)) and
      ((ImageBase+NextSymbol.RVA)<SymbolHigh) then begin
    SymbolHigh:=ImageBase+NextSymbol.RVA;
   end;
  end;
  WriteULEB128(fDebugInfo,2);
  WriteZeroTerminated(fDebugInfo,SymbolRecord.Name);
  WriteAddress(fDebugInfo,ImageBase+SymbolRecord.RVA);
  WriteAddress(fDebugInfo,SymbolHigh);
{$ifndef PasVulkanMapSymbolsLinearLookups}
  inc(Index);
{$endif}
 end;

 // End of the children of the compile unit.
 WriteULEB128(fDebugInfo,0);

 EndPosition:=fDebugInfo.Position;
 fDebugInfo.Position:=LengthPosition;
 WriteUInt32(fDebugInfo,TpvUInt32(EndPosition-(StartPosition+4)));
 fDebugInfo.Position:=EndPosition;

end;

procedure TDWARFWriter.Build;
var UnitIndex,LineIndex,FirstLine,LastLine:TpvSizeInt;
    StatementListOffset:TpvUInt32;
    LineRecord:TSymbolBuilder.TLineRecord;
{$ifndef PasVulkanMapSymbolsLinearLookups}
    FirstLines,LastLines:array of TpvSizeInt;
{$endif}
begin

 // Anything but these two would put a width into the header of every
 // compilation unit which the addresses behind it are then not written in, and
 // that is a file whose every address is off by however much the two differ.
 // Caught here rather than left to be noticed in a debugger.
 if (fAddressSize<>4) and (fAddressSize<>8) then begin
  raise Exception.Create('Address size must be four or eight bytes');
 end;

 fDebugLine.Clear;
 fDebugInfo.Clear;
 fDebugAbbrev.Clear;
 fLineRowCount:=0;

 BuildAbbrev;

{$ifndef PasVulkanMapSymbolsLinearLookups}
 // Where the rows of each unit begin and end. Found in one pass over the rows
 // rather than in one pass per unit: each row names its unit, so asking the
 // rows is enough, while asking the units means walking the whole row array
 // once for every one of them.
 SetLength(FirstLines,fBuilder.UnitCount);
 SetLength(LastLines,fBuilder.UnitCount);
 for UnitIndex:=0 to fBuilder.UnitCount-1 do begin
  FirstLines[UnitIndex]:=-1;
  LastLines[UnitIndex]:=-1;
 end;
 for LineIndex:=0 to fBuilder.LineCount-1 do begin
  LineRecord:=fBuilder.GetLine(LineIndex);
  if LineRecord.UnitIndex<TpvUInt32(fBuilder.UnitCount) then begin
   if FirstLines[LineRecord.UnitIndex]<0 then begin
    FirstLines[LineRecord.UnitIndex]:=LineIndex;
   end;
   LastLines[LineRecord.UnitIndex]:=LineIndex;
  end;
 end;
{$endif}

 for UnitIndex:=0 to fBuilder.UnitCount-1 do begin

  // The line records are sorted by address and each carries its unit, so the
  // rows of one unit form a contiguous run.
{$ifdef PasVulkanMapSymbolsLinearLookups}
  FirstLine:=-1;
  LastLine:=-1;
  for LineIndex:=0 to fBuilder.LineCount-1 do begin
   LineRecord:=fBuilder.GetLine(LineIndex);
   if LineRecord.UnitIndex=TpvUInt32(UnitIndex) then begin
    if FirstLine<0 then begin
     FirstLine:=LineIndex;
    end;
    LastLine:=LineIndex;
   end else if FirstLine>=0 then begin
    break;
   end;
  end;
{$else}
  FirstLine:=FirstLines[UnitIndex];
  LastLine:=LastLines[UnitIndex];
{$endif}

  if FirstLine<0 then begin
   // Nothing to say about a unit without line information.
   continue;
  end;

  StatementListOffset:=BuildLineProgram(UnitIndex,FirstLine,LastLine);
  BuildCompileUnit(UnitIndex,StatementListOffset);

 end;

 fDebugLine.Position:=0;
 fDebugInfo.Position:=0;
 fDebugAbbrev.Position:=0;

end;

end.
