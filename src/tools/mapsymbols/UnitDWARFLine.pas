// Reader for the DWARF .debug_line section.
//
// This is the part of DWARF which maps code addresses to source lines, and it is
// the only part a crash log needs. That distinction matters in practice: in a
// measured debug file of about 510 MB, .debug_line was roughly 5.8 MB while
// .debug_info, which carries the types and variables, took the remaining 488 MB.
// So reading only this section is what makes an appended symbol table small
// enough to ship.
//
// The line number program is a little state machine described in the DWARF
// standard. Versions 2, 3 and 4 differ only in one extra header field, and both
// FreePascal and Delphi emit version 2 here. Version 5 replaced the directory
// and file tables with a form encoded variant, and moved the strings out into
// their own sections, which is handled as well: linked in C objects bring it
// along even where the Pascal compiler does not emit it.
unit UnitDWARFLine;
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
     TDWARFLineRowEvent=procedure(const aAddress:TpvUInt64;const aLineNumber:TpvUInt32) of object;

     // Signals that the rows of one compilation unit are complete. The address
     // range is deliberately not computed here, because deciding which rows are
     // real needs knowledge this reader does not have: FreePascal emits line
     // information for code the linker later discards, and those leftover
     // sequences keep addresses near zero. Letting the consumer bound the unit
     // over the rows it accepted keeps one stray row from poisoning the range
     // of an entire unit.
     TDWARFLineUnitEvent=procedure(const aFileName:TpvUTF8String) of object;

     // The content types and forms which describe one version 5 table entry.
     TDWARFLineFormArray=array of TpvUInt64;

     TDWARFLineReader=class
      private
       fData:PpvUInt8;
       fSize:TpvSizeInt;
       fPosition:TpvSizeInt;
       fUnitCount:TpvSizeInt;
       fRowCount:TpvSizeInt;
       fSkippedUnitCount:TpvSizeInt;
       fBigEndian:Boolean;
       fLineStrData:PpvUInt8;
       fLineStrSize:TpvSizeInt;
       fStrData:PpvUInt8;
       fStrSize:TpvSizeInt;
       function AtEnd:Boolean;
       function ReadUInt8:TpvUInt8;
       function ReadUInt16:TpvUInt16;
       function ReadUInt32:TpvUInt32;
       function ReadUInt64:TpvUInt64;
       function ReadULEB128:TpvUInt64;
       function ReadSLEB128:TpvInt64;
       function ReadString:TpvUTF8String;
       function StringAt(const aData:PpvUInt8;const aSize:TpvSizeInt;const aOffset:TpvUInt64):TpvUTF8String;
       // Reads or steps over one value of the given form. Version 5 describes
       // its tables through forms, so both are needed to get at the entries.
       // Returns false for a form this does not know, which makes the unit
       // unusable, since the position in the stream is then lost.
       function SkipForm(const aForm:TpvUInt64;const aIs64Bit:Boolean):Boolean;
       function ReadFormString(const aForm:TpvUInt64;const aIs64Bit:Boolean;out aValue:TpvUTF8String):Boolean;
       function ReadFormUnsigned(const aForm:TpvUInt64;const aIs64Bit:Boolean;out aValue:TpvUInt64):Boolean;
       function ReadEntryFormat(out aTypes,aForms:TDWARFLineFormArray):Boolean;
       function ReadEntry(const aTypes,aForms:TDWARFLineFormArray;const aIs64Bit:Boolean;out aName:TpvUTF8String;out aDirectoryIndex:TpvInt64):Boolean;
      public
       constructor Create(const aData:TpvPointer;const aSize:TpvSizeInt);
       // Hands over the sections version 5 keeps its path strings in. Without
       // them a version 5 unit still yields its rows, only the file name stays
       // empty.
       procedure SetStringSections(const aLineStrData:TpvPointer;const aLineStrSize:TpvSizeInt;const aStrData:TpvPointer;const aStrSize:TpvSizeInt);
       function Parse(const aOnRow:TDWARFLineRowEvent;const aOnUnit:TDWARFLineUnitEvent):Boolean;
       // The byte order the section was written in, which is the one of the
       // image it came out of and has nothing to do with the machine reading
       // it here.
       property BigEndian:Boolean read fBigEndian write fBigEndian;
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

      // Content types of a version 5 directory or file entry.
      DW_LNCT_path=1;
      DW_LNCT_directory_index=2;

      DW_FORM_block2=$03;
      DW_FORM_block4=$04;
      DW_FORM_data2=$05;
      DW_FORM_data4=$06;
      DW_FORM_data8=$07;
      DW_FORM_string=$08;
      DW_FORM_block=$09;
      DW_FORM_block1=$0a;
      DW_FORM_data1=$0b;
      DW_FORM_flag=$0c;
      DW_FORM_sdata=$0d;
      DW_FORM_strp=$0e;
      DW_FORM_udata=$0f;
      DW_FORM_sec_offset=$17;
      DW_FORM_strx=$1a;
      DW_FORM_data16=$1e;
      DW_FORM_line_strp=$1f;
      DW_FORM_strx1=$25;
      DW_FORM_strx2=$26;
      DW_FORM_strx3=$27;
      DW_FORM_strx4=$28;

constructor TDWARFLineReader.Create(const aData:TpvPointer;const aSize:TpvSizeInt);
begin
 inherited Create;
 fData:=PpvUInt8(aData);
 fSize:=aSize;
 fPosition:=0;
 fUnitCount:=0;
 fRowCount:=0;
 fSkippedUnitCount:=0;
 fBigEndian:=false;
 fLineStrData:=nil;
 fLineStrSize:=0;
 fStrData:=nil;
 fStrSize:=0;
end;

procedure TDWARFLineReader.SetStringSections(const aLineStrData:TpvPointer;const aLineStrSize:TpvSizeInt;const aStrData:TpvPointer;const aStrSize:TpvSizeInt);
begin
 fLineStrData:=PpvUInt8(aLineStrData);
 fLineStrSize:=aLineStrSize;
 fStrData:=PpvUInt8(aStrData);
 fStrSize:=aStrSize;
end;

// Reads a zero terminated string out of one of the string sections.
function TDWARFLineReader.StringAt(const aData:PpvUInt8;const aSize:TpvSizeInt;const aOffset:TpvUInt64):TpvUTF8String;
var Position,Start:TpvSizeInt;
    Raw:TpvRawByteString;
begin
 result:='';
 if (not assigned(aData)) or (aOffset>=TpvUInt64(aSize)) then begin
  exit;
 end;
 Start:=TpvSizeInt(aOffset);
 Position:=Start;
 while (Position<aSize) and (PpvUInt8(TpvPointer(TpvPtrUInt(TpvPtrUInt(aData)+TpvPtrUInt(Position))))^<>0) do begin
  inc(Position);
 end;
 if Position>Start then begin
  SetLength(Raw,Position-Start);
  Move(PpvUInt8(TpvPointer(TpvPtrUInt(TpvPtrUInt(aData)+TpvPtrUInt(Start))))^,Raw[1],Position-Start);
  result:=String(Raw);
 end;
end;

function TDWARFLineReader.SkipForm(const aForm:TpvUInt64;const aIs64Bit:Boolean):Boolean;
var Length:TpvUInt64;
begin
 result:=true;
 case aForm of
  DW_FORM_string:begin
   ReadString;
  end;
  DW_FORM_flag,DW_FORM_data1,DW_FORM_strx1:begin
   ReadUInt8;
  end;
  DW_FORM_data2,DW_FORM_strx2:begin
   ReadUInt16;
  end;
  DW_FORM_strx3:begin
   ReadUInt8;
   ReadUInt16;
  end;
  DW_FORM_data4,DW_FORM_strx4:begin
   ReadUInt32;
  end;
  DW_FORM_data8:begin
   ReadUInt64;
  end;
  DW_FORM_data16:begin
   ReadUInt64;
   ReadUInt64;
  end;
  DW_FORM_udata,DW_FORM_strx:begin
   ReadULEB128;
  end;
  DW_FORM_sdata:begin
   ReadSLEB128;
  end;
  DW_FORM_strp,DW_FORM_line_strp,DW_FORM_sec_offset:begin
   if aIs64Bit then begin
    ReadUInt64;
   end else begin
    ReadUInt32;
   end;
  end;
  DW_FORM_block1:begin
   inc(fPosition,ReadUInt8);
  end;
  DW_FORM_block2:begin
   inc(fPosition,ReadUInt16);
  end;
  DW_FORM_block4:begin
   inc(fPosition,ReadUInt32);
  end;
  DW_FORM_block:begin
   Length:=ReadULEB128;
   inc(fPosition,TpvSizeInt(Length));
  end;
  else begin
   // Unknown, and since a form carries no length of its own there is no way to
   // step past it. Saying so is the only honest answer.
   result:=false;
  end;
 end;
end;

function TDWARFLineReader.ReadFormString(const aForm:TpvUInt64;const aIs64Bit:Boolean;out aValue:TpvUTF8String):Boolean;
var Offset:TpvUInt64;
begin
 result:=true;
 aValue:='';
 case aForm of
  DW_FORM_string:begin
   aValue:=ReadString;
  end;
  DW_FORM_line_strp:begin
   if aIs64Bit then begin
    Offset:=ReadUInt64;
   end else begin
    Offset:=ReadUInt32;
   end;
   aValue:=StringAt(fLineStrData,fLineStrSize,Offset);
  end;
  DW_FORM_strp:begin
   if aIs64Bit then begin
    Offset:=ReadUInt64;
   end else begin
    Offset:=ReadUInt32;
   end;
   aValue:=StringAt(fStrData,fStrSize,Offset);
  end;
  else begin
   // Anything else, the indexed string forms in particular, needs the string
   // offsets table of the compilation unit, which lives in a section this
   // reader deliberately does not read. The entry is stepped over instead, so
   // the unit still yields its rows without a file name.
   result:=SkipForm(aForm,aIs64Bit);
  end;
 end;
end;

function TDWARFLineReader.ReadFormUnsigned(const aForm:TpvUInt64;const aIs64Bit:Boolean;out aValue:TpvUInt64):Boolean;
begin
 result:=true;
 aValue:=0;
 case aForm of
  DW_FORM_data1:begin
   aValue:=ReadUInt8;
  end;
  DW_FORM_data2:begin
   aValue:=ReadUInt16;
  end;
  DW_FORM_data4:begin
   aValue:=ReadUInt32;
  end;
  DW_FORM_data8:begin
   aValue:=ReadUInt64;
  end;
  DW_FORM_udata:begin
   aValue:=ReadULEB128;
  end;
  else begin
   result:=SkipForm(aForm,aIs64Bit);
  end;
 end;
end;

function TDWARFLineReader.AtEnd:Boolean;
begin
 result:=fPosition>=fSize;
end;

function TDWARFLineReader.ReadUInt8:TpvUInt8;
begin
 if fPosition<fSize then begin
  result:=PpvUInt8(TpvPointer(TpvPtrUInt(TpvPtrUInt(fData)+TpvPtrUInt(fPosition))))^;
  inc(fPosition);
 end else begin
  result:=0;
  fPosition:=fSize;
 end;
end;

// Composed out of bytes rather than read as a whole, so the result depends only
// on the order the section was written in and not on the machine reading it.
// Which end comes first is what fBigEndian says.
function TDWARFLineReader.ReadUInt16:TpvUInt16;
var First,Second:TpvUInt8;
begin
 First:=ReadUInt8;
 Second:=ReadUInt8;
 if fBigEndian then begin
  result:=(TpvUInt16(First) shl 8) or TpvUInt16(Second);
 end else begin
  result:=TpvUInt16(First) or (TpvUInt16(Second) shl 8);
 end;
end;

function TDWARFLineReader.ReadUInt32:TpvUInt32;
var First,Second:TpvUInt16;
begin
 First:=ReadUInt16;
 Second:=ReadUInt16;
 if fBigEndian then begin
  result:=(TpvUInt32(First) shl 16) or TpvUInt32(Second);
 end else begin
  result:=TpvUInt32(First) or (TpvUInt32(Second) shl 16);
 end;
end;

function TDWARFLineReader.ReadUInt64:TpvUInt64;
var First,Second:TpvUInt32;
begin
 First:=ReadUInt32;
 Second:=ReadUInt32;
 if fBigEndian then begin
  result:=(TpvUInt64(First) shl 32) or TpvUInt64(Second);
 end else begin
  result:=TpvUInt64(First) or (TpvUInt64(Second) shl 32);
 end;
end;

function TDWARFLineReader.ReadULEB128:TpvUInt64;
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

function TDWARFLineReader.ReadSLEB128:TpvInt64;
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

function TDWARFLineReader.ReadString:TpvUTF8String;
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

// Reads the description of what each entry of a version 5 directory or file
// table holds, as a list of content type and form pairs.
function TDWARFLineReader.ReadEntryFormat(out aTypes,aForms:TDWARFLineFormArray):Boolean;
var Count,Index:TpvSizeInt;
begin
 Count:=ReadUInt8;
 SetLength(aTypes,Count);
 SetLength(aForms,Count);
 for Index:=0 to Count-1 do begin
  aTypes[Index]:=ReadULEB128;
  aForms[Index]:=ReadULEB128;
 end;
 result:=not AtEnd;
end;

// Reads one entry of such a table, keeping the path and the directory it
// belongs to and stepping over everything else.
function TDWARFLineReader.ReadEntry(const aTypes,aForms:TDWARFLineFormArray;const aIs64Bit:Boolean;out aName:TpvUTF8String;out aDirectoryIndex:TpvInt64):Boolean;
var Index:TpvSizeInt;
    Value:TpvUInt64;
begin
 result:=true;
 aName:='';
 // Nothing said means the compilation directory, which this reader has no name
 // for, so the path is left as it stands.
 aDirectoryIndex:=-1;
 for Index:=0 to length(aTypes)-1 do begin
  case aTypes[Index] of
   DW_LNCT_path:begin
    result:=ReadFormString(aForms[Index],aIs64Bit,aName);
   end;
   DW_LNCT_directory_index:begin
    result:=ReadFormUnsigned(aForms[Index],aIs64Bit,Value);
    aDirectoryIndex:=TpvInt64(Value);
   end;
   else begin
    result:=SkipForm(aForms[Index],aIs64Bit);
   end;
  end;
  if not result then begin
   break;
  end;
 end;
end;

function TDWARFLineReader.Parse(const aOnRow:TDWARFLineRowEvent;const aOnUnit:TDWARFLineUnitEvent):Boolean;
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
    Name,PrimaryFileName,Directory:TpvUTF8String;
    DirectoryIndex:TpvInt64;
    EntryTypes,EntryForms:TDWARFLineFormArray;
    EntryCount:TpvUInt64;
    Usable:Boolean;
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
  if (Version<2) or (Version>5) then begin
   // Anything outside this is a version whose header layout is unknown, and
   // guessing at it would silently produce wrong line numbers.
   inc(fSkippedUnitCount);
   fPosition:=UnitEnd;
   continue;
  end;

  if Version>=5 then begin
   // Two fields which the earlier versions do not have, sitting in front of the
   // header length rather than behind it.
   ReadUInt8; // address size
   ReadUInt8; // segment selector size
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

   FileDirectories:=nil;
   Usable:=true;

   if Version>=5 then begin

    // Both tables are preceded by a description of what each of their entries
    // holds, as a list of content type and form pairs, so the entries can only
    // be read by walking that description.
    Usable:=ReadEntryFormat(EntryTypes,EntryForms);
    if Usable then begin
     EntryCount:=ReadULEB128;
     for Index:=0 to TpvSizeInt(EntryCount)-1 do begin
      if not ReadEntry(EntryTypes,EntryForms,Is64Bit,Name,DirectoryIndex) then begin
       Usable:=false;
       break;
      end;
      Directories.Add(Name);
     end;
    end;

    if Usable then begin
     Usable:=ReadEntryFormat(EntryTypes,EntryForms);
    end;
    if Usable then begin
     EntryCount:=ReadULEB128;
     for Index:=0 to TpvSizeInt(EntryCount)-1 do begin
      if not ReadEntry(EntryTypes,EntryForms,Is64Bit,Name,DirectoryIndex) then begin
       Usable:=false;
       break;
      end;
      SetLength(FileDirectories,FileNames.Count+1);
      // Version 5 indexes the directory table from zero, where the earlier
      // versions reserve zero for the compilation directory. Stored normalized
      // here, so that everything below is version independent.
      FileDirectories[FileNames.Count]:=TpvInt32(DirectoryIndex);
      FileNames.Add(Name);
     end;
    end;

   end else begin

    repeat
     Name:=ReadString;
     if length(Name)>0 then begin
      Directories.Add(Name);
     end;
    until (length(Name)=0) or AtEnd;

    repeat
     Name:=ReadString;
     if length(Name)>0 then begin
      DirectoryIndex:=ReadULEB128;
      ReadULEB128; // modification time
      ReadULEB128; // file length
      SetLength(FileDirectories,FileNames.Count+1);
      // See above, made zero based to match what version 5 stores.
      FileDirectories[FileNames.Count]:=TpvInt32(DirectoryIndex)-1;
      FileNames.Add(Name);
     end;
    until (length(Name)=0) or AtEnd;

   end;

   if not Usable then begin
    inc(fSkippedUnitCount);
    fPosition:=UnitEnd;
    continue;
   end;

   // The first file entry is the primary source of the compilation unit, which
   // for Pascal is the unit itself. Line rows which point into an include file
   // are attributed to it as well, since the table stores one file per unit.
   PrimaryFileName:='';
   if FileNames.Count>0 then begin
    PrimaryFileName:=FileNames[0];
    if (FileDirectories[0]>=0) and (FileDirectories[0]<Directories.Count) then begin
     Directory:=Directories[FileDirectories[0]];
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
