// Reader for the DWARF .debug_line section.
//
// This is the part of DWARF which maps code addresses to source lines, and it is
// the only part a crash log needs. That distinction matters in practice: in a
// large debug file of about 510 MB, .debug_line is roughly 5.8 MB while
// .debug_info, which carries the types and variables, takes the remaining 488 MB.
// So reading only this section is what makes an appended symbol table small
// enough to ship.
//
// The line number program is a little state machine described in the DWARF
// standard. Versions 2, 3 and 4 differ only in one extra header field, and both
// FreePascal and Delphi emit version 2 here, so those are handled. Version 5
// replaced the directory and file tables with a form encoded variant, which is
// detected and skipped rather than guessed at.
unit UnitDwarfLine;
{$ifdef fpc}
 {$mode delphi}
{$endif}

interface

uses SysUtils,
     Classes,
     PasVulkan.Types;

type // A line number of zero marks the end of a sequence. Such a row has no
     // meaningful line, but its address still bounds the code of the unit, so
     // it is reported rather than dropped.
     TDwarfLineRowEvent=procedure(const aAddress:TpvUInt64;const aLineNumber:TpvUInt32) of object;

     // Signals that the rows of one compilation unit are complete. The address
     // range is deliberately not computed here, because deciding which rows are
     // real needs knowledge this reader does not have: FreePascal emits line
     // information for code the linker later discards, and those leftover
     // sequences keep addresses near zero. Letting the consumer bound the unit
     // over the rows it accepted keeps one stray row from poisoning the range
     // of an entire unit.
     TDwarfLineUnitEvent=procedure(const aFileName:String) of object;

     TDwarfLineReader=class
      private
       fData:PpvUInt8;
       fSize:TpvSizeInt;
       fPosition:TpvSizeInt;
       fUnitCount:TpvSizeInt;
       fRowCount:TpvSizeInt;
       fSkippedUnitCount:TpvSizeInt;
       function AtEnd:Boolean;
       function ReadUInt8:TpvUInt8;
       function ReadUInt16:TpvUInt16;
       function ReadUInt32:TpvUInt32;
       function ReadUInt64:TpvUInt64;
       function ReadULEB128:TpvUInt64;
       function ReadSLEB128:TpvInt64;
       function ReadString:String;
      public
       constructor Create(const aData:TpvPointer;const aSize:TpvSizeInt);
       function Parse(const aOnRow:TDwarfLineRowEvent;const aOnUnit:TDwarfLineUnitEvent):Boolean;
       property UnitCount:TpvSizeInt read fUnitCount;
       property RowCount:TpvSizeInt read fRowCount;
       property SkippedUnitCount:TpvSizeInt read fSkippedUnitCount;
     end;

implementation

const DW_LNS_copy=1;
      DW_LNS_advance_pc=2;
      DW_LNS_advance_line=3;
      DW_LNS_set_file=4;
      DW_LNS_set_column=5;
      DW_LNS_negate_stmt=6;
      DW_LNS_set_basic_block=7;
      DW_LNS_const_add_pc=8;
      DW_LNS_fixed_advance_pc=9;
      DW_LNS_set_prologue_end=10;
      DW_LNS_set_epilogue_begin=11;
      DW_LNS_set_isa=12;

      DW_LNE_end_sequence=1;
      DW_LNE_set_address=2;
      DW_LNE_define_file=3;

constructor TDwarfLineReader.Create(const aData:TpvPointer;const aSize:TpvSizeInt);
begin
 inherited Create;
 fData:=PpvUInt8(aData);
 fSize:=aSize;
 fPosition:=0;
 fUnitCount:=0;
 fRowCount:=0;
 fSkippedUnitCount:=0;
end;

function TDwarfLineReader.AtEnd:Boolean;
begin
 result:=fPosition>=fSize;
end;

function TDwarfLineReader.ReadUInt8:TpvUInt8;
begin
 if fPosition<fSize then begin
  result:=PpvUInt8(TpvPointer(TpvPtrUInt(TpvPtrUInt(fData)+TpvPtrUInt(fPosition))))^;
  inc(fPosition);
 end else begin
  result:=0;
  fPosition:=fSize;
 end;
end;

function TDwarfLineReader.ReadUInt16:TpvUInt16;
begin
 result:=TpvUInt16(ReadUInt8) or (TpvUInt16(ReadUInt8) shl 8);
end;

function TDwarfLineReader.ReadUInt32:TpvUInt32;
begin
 result:=TpvUInt32(ReadUInt16) or (TpvUInt32(ReadUInt16) shl 16);
end;

function TDwarfLineReader.ReadUInt64:TpvUInt64;
begin
 result:=TpvUInt64(ReadUInt32) or (TpvUInt64(ReadUInt32) shl 32);
end;

function TDwarfLineReader.ReadULEB128:TpvUInt64;
var Shift:TpvInt32;
    Value:TpvUInt8;
begin
 result:=0;
 Shift:=0;
 repeat
  Value:=ReadUInt8;
  if Shift<64 then begin
   result:=result or (TpvUInt64(Value and $7f) shl Shift);
  end;
  inc(Shift,7);
 until ((Value and $80)=0) or AtEnd;
end;

function TDwarfLineReader.ReadSLEB128:TpvInt64;
var Shift:TpvInt32;
    Value:TpvUInt8;
begin
 result:=0;
 Shift:=0;
 repeat
  Value:=ReadUInt8;
  if Shift<64 then begin
   result:=result or (TpvInt64(TpvUInt64(Value and $7f) shl Shift));
  end;
  inc(Shift,7);
 until ((Value and $80)=0) or AtEnd;
 // Sign extend when the last byte carried the sign bit.
 if (Shift<64) and ((Value and $40)<>0) then begin
  result:=result or -(TpvInt64(1) shl Shift);
 end;
end;

function TDwarfLineReader.ReadString:String;
var Start,Count:TpvSizeInt;
begin
 Start:=fPosition;
 while (fPosition<fSize) and
       (PpvUInt8(TpvPointer(TpvPtrUInt(TpvPtrUInt(fData)+TpvPtrUInt(fPosition))))^<>0) do begin
  inc(fPosition);
 end;
 Count:=fPosition-Start;
 if Count>0 then begin
  SetString(result,PAnsiChar(TpvPointer(TpvPtrUInt(TpvPtrUInt(fData)+TpvPtrUInt(Start)))),Count);
 end else begin
  result:='';
 end;
 if fPosition<fSize then begin
  inc(fPosition);
 end;
end;

function TDwarfLineReader.Parse(const aOnRow:TDwarfLineRowEvent;const aOnUnit:TDwarfLineUnitEvent):Boolean;
var UnitLength:TpvUInt64;
    UnitEnd,ProgramStart:TpvSizeInt;
    Version:TpvUInt16;
    HeaderLength:TpvUInt64;
    Is64Bit:Boolean;
    MinimumInstructionLength:TpvUInt8;
    DefaultIsStatement:TpvUInt8;
    LineBase:TpvInt8;
    LineRange:TpvUInt8;
    OpcodeBase:TpvUInt8;
    StandardOpcodeLengths:array[0..255] of TpvUInt8;
    Index:TpvInt32;
    Directories:TStringList;
    FileNames:TStringList;
    FileDirectories:array of TpvInt32;
    Name,PrimaryFileName,Directory:String;
    DirectoryIndex:TpvUInt64;
    Address:TpvUInt64;
    Line:TpvInt64;
    Opcode,SubOpcode:TpvUInt8;
    Adjusted:TpvUInt32;
    ExtendedLength,ExtendedEnd:TpvSizeInt;
    AddressSize:TpvSizeInt;
    OperandIndex:TpvInt32;

 procedure EmitRow;
 begin
  if (Line>0) and assigned(aOnRow) then begin
   aOnRow(Address,TpvUInt32(Line));
   inc(fRowCount);
  end;
 end;

 procedure EmitEndOfSequence;
 begin
  if assigned(aOnRow) then begin
   aOnRow(Address,0);
  end;
 end;

 procedure ResetState;
 begin
  Address:=0;
  Line:=1;
 end;

begin

 result:=false;

 if (not assigned(fData)) or (fSize<=0) then begin
  exit;
 end;

 fPosition:=0;

 while fPosition<fSize do begin

  // Compilation unit header
  UnitLength:=ReadUInt32;
  Is64Bit:=UnitLength=TpvUInt64($ffffffff);
  if Is64Bit then begin
   UnitLength:=ReadUInt64;
  end;
  if (UnitLength=0) or (UnitLength>TpvUInt64(fSize-fPosition)) then begin
   break;
  end;
  UnitEnd:=fPosition+TpvSizeInt(UnitLength);

  Version:=ReadUInt16;
  if (Version<2) or (Version>4) then begin
   // Version 5 encodes the directory and file tables through forms, which is a
   // different parser. Skipping is honest, guessing would silently produce
   // wrong line numbers.
   inc(fSkippedUnitCount);
   fPosition:=UnitEnd;
   continue;
  end;

  if Is64Bit then begin
   HeaderLength:=ReadUInt64;
  end else begin
   HeaderLength:=ReadUInt32;
  end;
  ProgramStart:=fPosition+TpvSizeInt(HeaderLength);

  MinimumInstructionLength:=ReadUInt8;
  if Version>=4 then begin
   // maximum_operations_per_instruction, only meaningful for VLIW targets
   ReadUInt8;
  end;
  DefaultIsStatement:=ReadUInt8;
  LineBase:=TpvInt8(ReadUInt8);
  LineRange:=ReadUInt8;
  OpcodeBase:=ReadUInt8;

  FillChar(StandardOpcodeLengths,SizeOf(StandardOpcodeLengths),#0);
  for Index:=1 to TpvInt32(OpcodeBase)-1 do begin
   StandardOpcodeLengths[Index]:=ReadUInt8;
  end;

  if LineRange=0 then begin
   // Would divide by zero further down, so this unit is unusable.
   inc(fSkippedUnitCount);
   fPosition:=UnitEnd;
   continue;
  end;

  Directories:=TStringList.Create;
  FileNames:=TStringList.Create;
  try

   repeat
    Name:=ReadString;
    if length(Name)>0 then begin
     Directories.Add(Name);
    end;
   until (length(Name)=0) or AtEnd;

   FileDirectories:=nil;
   repeat
    Name:=ReadString;
    if length(Name)>0 then begin
     DirectoryIndex:=ReadULEB128;
     ReadULEB128; // modification time
     ReadULEB128; // file length
     SetLength(FileDirectories,FileNames.Count+1);
     FileDirectories[FileNames.Count]:=TpvInt32(DirectoryIndex);
     FileNames.Add(Name);
    end;
   until (length(Name)=0) or AtEnd;

   // The first file entry is the primary source of the compilation unit, which
   // for Pascal is the unit itself. Line rows which point into an include file
   // are attributed to it as well, since the table stores one file per unit.
   PrimaryFileName:='';
   if FileNames.Count>0 then begin
    PrimaryFileName:=FileNames[0];
    if (FileDirectories[0]>0) and (FileDirectories[0]<=Directories.Count) then begin
     Directory:=Directories[FileDirectories[0]-1];
     if (length(Directory)>0) and
        (Directory[length(Directory)]<>'/') and
        (Directory[length(Directory)]<>'\') then begin
      Directory:=Directory+'/';
     end;
     PrimaryFileName:=Directory+PrimaryFileName;
    end;
   end;

   // The header length field is authoritative for where the program starts, so
   // any padding or unread table bytes cannot desynchronize the state machine.
   if (ProgramStart>fPosition) and (ProgramStart<=UnitEnd) then begin
    fPosition:=ProgramStart;
   end;

   ResetState;

   while fPosition<UnitEnd do begin

    Opcode:=ReadUInt8;

    if Opcode>=OpcodeBase then begin

     Adjusted:=TpvUInt32(Opcode)-TpvUInt32(OpcodeBase);
     inc(Address,TpvUInt64(Adjusted div LineRange)*TpvUInt64(MinimumInstructionLength));
     inc(Line,TpvInt64(LineBase)+TpvInt64(Adjusted mod LineRange));
     EmitRow;

    end else if Opcode=0 then begin

     ExtendedLength:=TpvSizeInt(ReadULEB128);
     ExtendedEnd:=fPosition+ExtendedLength;
     if ExtendedLength>0 then begin
      SubOpcode:=ReadUInt8;
      case SubOpcode of
       DW_LNE_end_sequence:begin
        // Marks one past the last instruction of a sequence. Its line value is
        // meaningless, but the address still bounds the code of the unit.
        EmitEndOfSequence;
        ResetState;
       end;
       DW_LNE_set_address:begin
        AddressSize:=ExtendedLength-1;
        if AddressSize=8 then begin
         Address:=ReadUInt64;
        end else if AddressSize=4 then begin
         Address:=ReadUInt32;
        end else if AddressSize=2 then begin
         Address:=ReadUInt16;
        end;
       end;
       DW_LNE_define_file:begin
        ReadString;
        ReadULEB128;
        ReadULEB128;
        ReadULEB128;
       end;
       else begin
        // Unknown vendor extension, the length tells how much to skip.
       end;
      end;
     end;
     if (ExtendedEnd>=fPosition) and (ExtendedEnd<=UnitEnd) then begin
      fPosition:=ExtendedEnd;
     end;

    end else begin

     case Opcode of
      DW_LNS_copy:begin
       EmitRow;
      end;
      DW_LNS_advance_pc:begin
       inc(Address,ReadULEB128*TpvUInt64(MinimumInstructionLength));
      end;
      DW_LNS_advance_line:begin
       inc(Line,ReadSLEB128);
      end;
      DW_LNS_set_file:begin
       ReadULEB128;
      end;
      DW_LNS_set_column:begin
       ReadULEB128;
      end;
      DW_LNS_negate_stmt,DW_LNS_set_basic_block,DW_LNS_set_prologue_end,DW_LNS_set_epilogue_begin:begin
      end;
      DW_LNS_const_add_pc:begin
       Adjusted:=255-TpvUInt32(OpcodeBase);
       inc(Address,TpvUInt64(Adjusted div LineRange)*TpvUInt64(MinimumInstructionLength));
      end;
      DW_LNS_fixed_advance_pc:begin
       // Deliberately not scaled by the minimum instruction length.
       inc(Address,TpvUInt64(ReadUInt16));
      end;
      DW_LNS_set_isa:begin
       ReadULEB128;
      end;
      else begin
       // An opcode this reader does not know, but whose operand count the
       // header declared, so it can still be stepped over safely.
       for OperandIndex:=1 to StandardOpcodeLengths[Opcode] do begin
        ReadULEB128;
       end;
      end;
     end;

    end;

   end;

   inc(fUnitCount);
   if assigned(aOnUnit) then begin
    aOnUnit(PrimaryFileName);
   end;

  finally
   FreeAndNil(FileNames);
   FreeAndNil(Directories);
  end;

  fPosition:=UnitEnd;

 end;

 result:=fUnitCount>0;

end;

end.
