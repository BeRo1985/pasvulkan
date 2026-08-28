// Reads back the compilation unit descriptions of a .debug_info section.
//
// This exists for one reason: .debug_line on its own says nothing about which
// source file a row belongs to, and nothing at all about which of its programs
// a consumer should even look at. Both of those come from here. A compile unit
// names its file, states the address range it covers, and points at its line
// program through DW_AT_stmt_list, and everything built on BFD or LLVM reaches
// the rows that way rather than by scanning the line section.
//
// So a written .debug_line can be perfect row for row while the file is still
// useless, because a compile unit points at the wrong program, states a range
// which does not contain its own rows, or announces an address width the
// addresses behind it are not written in. None of that is visible from the line
// section, and none of it is visible from the symbol table either.
//
// Only what the writer next door emits is understood, which is DWARF 2 with
// inline strings, and only the forms it uses. Anything else is reported as not
// understood rather than guessed at, since a wrong guess here would produce a
// check which passes for the wrong reason.
unit UnitDWARFInfo;
{$ifdef fpc}
 {$mode delphi}
{$endif}

interface

uses SysUtils,
     Classes,
     PasVulkan.Types;

type TDWARFInfoSubprogram=record
      Name:String;
      LowPC:TpvUInt64;
      HighPC:TpvUInt64;
     end;

     TDWARFInfoSubprograms=array of TDWARFInfoSubprogram;

     TDWARFInfoUnit=record
      Name:String;
      Directory:String;
      LowPC:TpvUInt64;
      HighPC:TpvUInt64;
      // Offset of the line program of this unit into .debug_line, which is what
      // DW_AT_stmt_list holds.
      StatementListOffset:TpvUInt64;
      HaveStatementList:Boolean;
      AddressSize:TpvUInt8;
      Subprograms:TDWARFInfoSubprograms;
      SubprogramCount:TpvSizeInt;
     end;
     PDWARFInfoUnit=^TDWARFInfoUnit;

     TDWARFInfoUnits=array of TDWARFInfoUnit;

     TDWARFInfoReader=class
      private
       type TAbbreviationAttribute=record
             Attribute:TpvUInt64;
             Form:TpvUInt64;
            end;
            PAbbreviationAttribute=^TAbbreviationAttribute;
            TAbbreviationAttributes=array of TAbbreviationAttribute;
            TAbbreviation=record
             Code:TpvUInt64;
             Tag:TpvUInt64;
             HasChildren:Boolean;
             Attributes:TAbbreviationAttributes;
             AttributeCount:TpvSizeInt;
            end;
            PAbbreviation=^TAbbreviation;
            TAbbreviations=array of TAbbreviation;
      private
       fInfoData:PpvUInt8;
       fInfoSize:TpvSizeInt;
       fAbbrevData:PpvUInt8;
       fAbbrevSize:TpvSizeInt;
       fBigEndian:Boolean;
       fPosition:TpvSizeInt;
       fSize:TpvSizeInt;
       fData:PpvUInt8;
       fAbbreviations:TAbbreviations;
       fAbbreviationCount:TpvSizeInt;
       fUnits:TDWARFInfoUnits;
       fUnitCount:TpvSizeInt;
       fMessage:String;
       function ReadUInt8:TpvUInt8;
       function ReadUInt16:TpvUInt16;
       function ReadUInt32:TpvUInt32;
       function ReadUInt64:TpvUInt64;
       function ReadULEB128:TpvUInt64;
       function ReadSLEB128:TpvInt64;
       function ReadString:String;
       function ReadAbbreviations(const aOffset:TpvUInt64):Boolean;
       function FindAbbreviation(const aCode:TpvUInt64;out aIndex:TpvSizeInt):Boolean;
       function ReadValue(const aForm:TpvUInt64;const aAddressSize:TpvUInt8;
                          out aUnsigned:TpvUInt64;out aString:String):Boolean;
      public
       constructor Create(const aInfoData:TpvPointer;const aInfoSize:TpvSizeInt;
                          const aAbbrevData:TpvPointer;const aAbbrevSize:TpvSizeInt);
       destructor Destroy; override;
       function Parse:Boolean;
       function GetUnit(const aIndex:TpvSizeInt):TDWARFInfoUnit;
       property BigEndian:Boolean read fBigEndian write fBigEndian;
       property UnitCount:TpvSizeInt read fUnitCount;
       // Why Parse said no, for a caller which wants to say more than that it
       // did.
       property Message:String read fMessage;
     end;

implementation

const DW_TAG_compile_unit=$11;
      DW_TAG_subprogram=$2e;

      DW_AT_name=$03;
      DW_AT_stmt_list=$10;
      DW_AT_low_pc=$11;
      DW_AT_high_pc=$12;
      DW_AT_comp_dir=$1b;

      DW_FORM_addr=$01;
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
      DW_FORM_udata=$0f;
      DW_FORM_ref_addr=$10;
      DW_FORM_ref1=$11;
      DW_FORM_ref2=$12;
      DW_FORM_ref4=$13;
      DW_FORM_ref8=$14;
      DW_FORM_ref_udata=$15;
      DW_FORM_sec_offset=$17;

constructor TDWARFInfoReader.Create(const aInfoData:TpvPointer;const aInfoSize:TpvSizeInt;
                                    const aAbbrevData:TpvPointer;const aAbbrevSize:TpvSizeInt);
begin
 inherited Create;
 fInfoData:=aInfoData;
 fInfoSize:=aInfoSize;
 fAbbrevData:=aAbbrevData;
 fAbbrevSize:=aAbbrevSize;
 fBigEndian:=false;
 fPosition:=0;
 fData:=nil;
 fSize:=0;
 fAbbreviations:=nil;
 fAbbreviationCount:=0;
 fUnits:=nil;
 fUnitCount:=0;
 fMessage:='';
end;

destructor TDWARFInfoReader.Destroy;
begin
 fAbbreviations:=nil;
 fUnits:=nil;
 inherited Destroy;
end;

// The same shape as in the line reader: composed out of bytes rather than read
// as a number, so that the byte order of the described image decides and not
// the one of the machine this runs on.
function TDWARFInfoReader.ReadUInt8:TpvUInt8;
begin
 if fPosition<fSize then begin
  result:=PpvUInt8Array(fData)^[fPosition];
  inc(fPosition);
 end else begin
  result:=0;
  fPosition:=fSize+1;
 end;
end;

function TDWARFInfoReader.ReadUInt16:TpvUInt16;
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

function TDWARFInfoReader.ReadUInt32:TpvUInt32;
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

function TDWARFInfoReader.ReadUInt64:TpvUInt64;
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

function TDWARFInfoReader.ReadULEB128:TpvUInt64;
var Shift:TpvInt32;
    Current:TpvUInt8;
begin
 result:=0;
 Shift:=0;
 repeat
  Current:=ReadUInt8;
  if Shift<64 then begin
   result:=result or (TpvUInt64(Current and $7f) shl Shift);
  end;
  inc(Shift,7);
 until ((Current and $80)=0) or (fPosition>fSize);
end;

function TDWARFInfoReader.ReadSLEB128:TpvInt64;
var Shift:TpvInt32;
    Current:TpvUInt8;
begin
 result:=0;
 Shift:=0;
 repeat
  Current:=ReadUInt8;
  if Shift<64 then begin
   result:=result or (TpvInt64(Current and $7f) shl Shift);
  end;
  inc(Shift,7);
 until ((Current and $80)=0) or (fPosition>fSize);
 // The sign lives in the second highest bit of the last byte, so what is above
 // it has to be filled in.
 if (Shift<64) and ((Current and $40)<>0) then begin
  result:=result or -(TpvInt64(1) shl Shift);
 end;
end;

function TDWARFInfoReader.ReadString:String;
var Start:TpvSizeInt;
    Length_:TpvSizeInt;
begin
 result:='';
 Start:=fPosition;
 while (fPosition<fSize) and (PpvUInt8Array(fData)^[fPosition]<>0) do begin
  inc(fPosition);
 end;
 Length_:=fPosition-Start;
 if Length_>0 then begin
  SetString(result,PAnsiChar(@PpvUInt8Array(fData)^[Start]),Length_);
 end;
 if fPosition<fSize then begin
  inc(fPosition);
 end;
end;

function TDWARFInfoReader.ReadAbbreviations(const aOffset:TpvUInt64):Boolean;
var Code,Tag,Attribute,Form:TpvUInt64;
    Index:TpvSizeInt;
    SavedData:PpvUInt8;
    SavedSize,SavedPosition:TpvSizeInt;
    Abbreviation:PAbbreviation;
    AbbreviationAttribute:PAbbreviationAttribute;
begin

 result:=false;

 SavedData:=fData;
 SavedSize:=fSize;
 SavedPosition:=fPosition;
 try

  fData:=fAbbrevData;
  fSize:=fAbbrevSize;
  if aOffset>TpvUInt64(fAbbrevSize) then begin
   fMessage:='the abbreviation offset of a compilation unit points past the abbreviation section';
   exit;
  end;
  fPosition:=TpvSizeInt(aOffset);

  fAbbreviationCount:=0;

  while fPosition<fSize do begin

   Code:=ReadULEB128;
   if Code=0 then begin
    // The terminator of this table.
    break;
   end;

   Tag:=ReadULEB128;

   if fAbbreviationCount>=length(fAbbreviations) then begin
    SetLength(fAbbreviations,(fAbbreviationCount+1)*2);
   end;
   Index:=fAbbreviationCount;
   inc(fAbbreviationCount);
   Abbreviation:=@fAbbreviations[Index];
   Abbreviation^.Code:=Code;
   Abbreviation^.Tag:=Tag;
   Abbreviation^.HasChildren:=ReadUInt8<>0;
   Abbreviation^.AttributeCount:=0;

   repeat
    Attribute:=ReadULEB128;
    Form:=ReadULEB128;
    if (Attribute=0) and (Form=0) then begin
     break;
    end;
    if Abbreviation^.AttributeCount>=length(Abbreviation^.Attributes) then begin
     SetLength(Abbreviation^.Attributes,(Abbreviation^.AttributeCount+1)*2);
    end;
    AbbreviationAttribute:=@Abbreviation^.Attributes[Abbreviation^.AttributeCount];
    AbbreviationAttribute^.Attribute:=Attribute;
    AbbreviationAttribute^.Form:=Form;
    inc(Abbreviation^.AttributeCount);
   until fPosition>fSize;

   if fPosition>fSize then begin
    fMessage:='the abbreviation table ends in the middle of an entry';
    exit;
   end;

  end;

  result:=fAbbreviationCount>0;
  if not result then begin
   fMessage:='the abbreviation table is empty';
  end;

 finally
  fData:=SavedData;
  fSize:=SavedSize;
  fPosition:=SavedPosition;
 end;

end;

function TDWARFInfoReader.FindAbbreviation(const aCode:TpvUInt64;out aIndex:TpvSizeInt):Boolean;
var Index:TpvSizeInt;
begin
 result:=false;
 aIndex:=-1;
 for Index:=0 to fAbbreviationCount-1 do begin
  if fAbbreviations[Index].Code=aCode then begin
   aIndex:=Index;
   result:=true;
   exit;
  end;
 end;
end;

// Reads one attribute value and hands back whatever of it is usable here. A
// form which is not understood is refused rather than skipped, since skipping
// the wrong number of bytes puts everything behind it out of step and the check
// built on this would then fail for a reason which has nothing to do with what
// it is checking.
function TDWARFInfoReader.ReadValue(const aForm:TpvUInt64;const aAddressSize:TpvUInt8;
                                    out aUnsigned:TpvUInt64;out aString:String):Boolean;
var Length_:TpvUInt64;
begin

 result:=true;
 aUnsigned:=0;
 aString:='';

 case aForm of
  DW_FORM_addr:begin
   if aAddressSize=4 then begin
    aUnsigned:=ReadUInt32;
   end else begin
    aUnsigned:=ReadUInt64;
   end;
  end;
  DW_FORM_string:begin
   aString:=ReadString;
  end;
  DW_FORM_data1,DW_FORM_flag,DW_FORM_ref1:begin
   aUnsigned:=ReadUInt8;
  end;
  DW_FORM_data2,DW_FORM_ref2:begin
   aUnsigned:=ReadUInt16;
  end;
  DW_FORM_data4,DW_FORM_ref4,DW_FORM_ref_addr,DW_FORM_sec_offset:begin
   aUnsigned:=ReadUInt32;
  end;
  DW_FORM_data8,DW_FORM_ref8:begin
   aUnsigned:=ReadUInt64;
  end;
  DW_FORM_udata,DW_FORM_ref_udata:begin
   aUnsigned:=ReadULEB128;
  end;
  DW_FORM_sdata:begin
   aUnsigned:=TpvUInt64(ReadSLEB128);
  end;
  DW_FORM_block1:begin
   Length_:=ReadUInt8;
   inc(fPosition,TpvSizeInt(Length_));
  end;
  DW_FORM_block2:begin
   Length_:=ReadUInt16;
   inc(fPosition,TpvSizeInt(Length_));
  end;
  DW_FORM_block4:begin
   Length_:=ReadUInt32;
   inc(fPosition,TpvSizeInt(Length_));
  end;
  DW_FORM_block:begin
   Length_:=ReadULEB128;
   inc(fPosition,TpvSizeInt(Length_));
  end;
  else begin
   fMessage:='a compilation unit uses attribute form $'+IntToHex(aForm,2)+', which this reader does not know';
   result:=false;
  end;
 end;

end;

function TDWARFInfoReader.Parse:Boolean;
var UnitLength,AbbrevOffset,Code,Value:TpvUInt64;
    UnitEnd:TpvSizeInt;
    Version:TpvUInt16;
    AddressSize:TpvUInt8;
    AbbreviationIndex,AttributeIndex,UnitIndex,Depth:TpvSizeInt;
    Text:String;
    Subprogram:TDWARFInfoSubprogram;
    HaveSubprogram,IsCompileUnit:Boolean;
    InfoUnit:PDWARFInfoUnit;
    Abbreviation:PAbbreviation;
    AbbreviationAttribute:PAbbreviationAttribute;
begin

 result:=false;
 fUnitCount:=0;

 if (not assigned(fInfoData)) or (fInfoSize<=0) then begin
  fMessage:='there is no compilation unit section';
  exit;
 end;

 fData:=fInfoData;
 fSize:=fInfoSize;
 fPosition:=0;

 while fPosition<fSize do begin

  UnitLength:=ReadUInt32;
  if UnitLength=TpvUInt64($ffffffff) then begin
   fMessage:='a compilation unit uses the sixty four bit section format, which this writer never emits';
   exit;
  end;
  if (UnitLength=0) or (UnitLength>TpvUInt64(fSize-fPosition)) then begin
   fMessage:='a compilation unit states a length which does not fit into the section';
   exit;
  end;
  UnitEnd:=fPosition+TpvSizeInt(UnitLength);

  Version:=ReadUInt16;
  if (Version<2) or (Version>4) then begin
   fMessage:='a compilation unit states version '+IntToStr(Version)+', which this reader does not know';
   exit;
  end;

  AbbrevOffset:=ReadUInt32;
  AddressSize:=ReadUInt8;
  if (AddressSize<>4) and (AddressSize<>8) then begin
   fMessage:='a compilation unit states an address size of '+IntToStr(AddressSize)+' bytes';
   exit;
  end;

  if not ReadAbbreviations(AbbrevOffset) then begin
   exit;
  end;

  if fUnitCount>=length(fUnits) then begin
   SetLength(fUnits,(fUnitCount+1)*2);
  end;
  UnitIndex:=fUnitCount;
  inc(fUnitCount);
  // Taken after the growth above and used for the whole of this unit. Nothing
  // below moves the array itself; what grows in there is the subprogram list of
  // this one entry, which lives somewhere else.
  InfoUnit:=@fUnits[UnitIndex];
  InfoUnit^.Name:='';
  InfoUnit^.Directory:='';
  InfoUnit^.LowPC:=0;
  InfoUnit^.HighPC:=0;
  InfoUnit^.StatementListOffset:=0;
  InfoUnit^.HaveStatementList:=false;
  InfoUnit^.AddressSize:=AddressSize;
  InfoUnit^.Subprograms:=nil;
  InfoUnit^.SubprogramCount:=0;

  // The compile unit itself and then its children. Only one level of them is
  // ever written here, but the nesting is followed rather than assumed: a
  // description which says it has children opens a level and a zero closes one,
  // and the unit is done when the level the compile unit opened is closed
  // again. The unit length bounds this as well, but only that would mean a file
  // with deeper nesting than expected is read to the end of the unit as if
  // everything in it sat at the top, which is not the same thing as reading it.
  Depth:=0;
  while fPosition<UnitEnd do begin

   Code:=ReadULEB128;
   if Code=0 then begin
    // The end of a list of children.
    if Depth>0 then begin
     dec(Depth);
     if Depth=0 then begin
      break;
     end;
    end;
    continue;
   end;

   if not FindAbbreviation(Code,AbbreviationIndex) then begin
    fMessage:='a description refers to abbreviation '+IntToStr(Code)+', which the abbreviation table does not have';
    exit;
   end;

   Abbreviation:=@fAbbreviations[AbbreviationIndex];
   HaveSubprogram:=Abbreviation^.Tag=DW_TAG_subprogram;
   IsCompileUnit:=Abbreviation^.Tag=DW_TAG_compile_unit;
   Subprogram.Name:='';
   Subprogram.LowPC:=0;
   Subprogram.HighPC:=0;

   for AttributeIndex:=0 to Abbreviation^.AttributeCount-1 do begin

    AbbreviationAttribute:=@Abbreviation^.Attributes[AttributeIndex];

    if not ReadValue(AbbreviationAttribute^.Form,AddressSize,Value,Text) then begin
     exit;
    end;

    case AbbreviationAttribute^.Attribute of
     DW_AT_name:begin
      if HaveSubprogram then begin
       Subprogram.Name:=Text;
      end else if IsCompileUnit then begin
       InfoUnit^.Name:=Text;
      end;
     end;
     DW_AT_comp_dir:begin
      if IsCompileUnit then begin
       InfoUnit^.Directory:=Text;
      end;
     end;
     DW_AT_low_pc:begin
      if HaveSubprogram then begin
       Subprogram.LowPC:=Value;
      end else if IsCompileUnit then begin
       InfoUnit^.LowPC:=Value;
      end;
     end;
     DW_AT_high_pc:begin
      if HaveSubprogram then begin
       Subprogram.HighPC:=Value;
      end else if IsCompileUnit then begin
       InfoUnit^.HighPC:=Value;
      end;
     end;
     DW_AT_stmt_list:begin
      if IsCompileUnit then begin
       InfoUnit^.StatementListOffset:=Value;
       InfoUnit^.HaveStatementList:=true;
      end;
     end;
     else begin
     end;
    end;

   end;

   if HaveSubprogram then begin
    if InfoUnit^.SubprogramCount>=length(InfoUnit^.Subprograms) then begin
     SetLength(InfoUnit^.Subprograms,(InfoUnit^.SubprogramCount+1)*2);
    end;
    InfoUnit^.Subprograms[InfoUnit^.SubprogramCount]:=Subprogram;
    inc(InfoUnit^.SubprogramCount);
   end;

   if Abbreviation^.HasChildren then begin
    inc(Depth);
   end;

   if fPosition>fSize then begin
    fMessage:='a description runs past the end of the section';
    exit;
   end;

  end;

  fPosition:=UnitEnd;

 end;

 result:=fUnitCount>0;
 if not result then begin
  fMessage:='the section holds no compilation units at all';
 end;

end;

function TDWARFInfoReader.GetUnit(const aIndex:TpvSizeInt):TDWARFInfoUnit;
begin
 if (aIndex>=0) and (aIndex<fUnitCount) then begin
  result:=fUnits[aIndex];
 end else begin
  FillChar(result,SizeOf(TDWARFInfoUnit),#0);
  result.Name:='';
  result.Directory:='';
 end;
end;

end.
