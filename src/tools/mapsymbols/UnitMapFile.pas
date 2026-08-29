// Reader for a Delphi .map file.
//
// The address arithmetic here is not obvious, and the compiler states it wrongly
// in one place:
//
// - The Start column of the segment table holds the virtual address of the
// segment, so .text typically reads 0000000000401000.
// - Every SSSS:OOOOOOOO elsewhere in the file, in the detailed segment map, in
// the publics and in the line numbers, is segment relative. The virtual
// address is therefore SegmentStartVA + Offset.
// - The "Program entry point at" line does not follow that rule. It prints
// ImageBase + Offset and is thus off by the distance between the image base
// and the segment start. It is ignored here.
//
// Verified against the PE header of a build: the line record of the main begin
// block resolves through SegmentStartVA + Offset to exactly the entry point the
// optional header names, while the map's own entry point line does not.
unit UnitMapFile;
{$ifdef fpc}
 {$mode delphi}
{$endif}

interface

uses SysUtils,
     Classes,
     PasVulkan.Types,
     UnitSymbolBuilder;

type TMapFileReader=class
      private
       type TSegment=record
             SegmentIndex:TpvUInt32;
             StartVA:TpvUInt64;
             Size:TpvUInt64;
            end;
            PSegment=^TSegment;
            TSegments=array of TSegment;
      private
       fSegments:TSegments;
       fImageBase:TpvUInt64;
       fBuilder:TSymbolBuilder;
       fWantSymbols:Boolean;
       fWantLines:Boolean;
       function ParseSegmentedAddress(const aToken:TpvUTF8String;out aVirtualAddress:TpvUInt64):Boolean;
      public
       constructor Create(const aBuilder:TSymbolBuilder;const aImageBase:TpvUInt64;const aWantSymbols,aWantLines:Boolean);
       procedure Parse(const aFileName:TpvUTF8String);
     end;

implementation

function ParseHex(const aValue:TpvUTF8String):TpvUInt64;
var Index:TpvSizeInt;
    Digit:TpvUInt32;
begin
 result:=0;
 for Index:=1 to length(aValue) do begin
  case aValue[Index] of
   '0'..'9':begin
    Digit:=TpvUInt32(ord(aValue[Index])-ord('0'));
   end;
   'a'..'f':begin
    Digit:=TpvUInt32(ord(aValue[Index])-ord('a'))+10;
   end;
   'A'..'F':begin
    Digit:=TpvUInt32(ord(aValue[Index])-ord('A'))+10;
   end;
   else begin
    break;
   end;
  end;
  result:=(result shl 4) or Digit;
 end;
end;

// The token list stays a TStringList, which is a list of the compiler's own
// string type, so the two touch points below convert. Everything this unit owns
// is utf-8; the container in between is the runtime's and is left as it is.
procedure Tokenize(const aLine:TpvUTF8String;const aTokens:TStringList);
var Index,Start:TpvSizeInt;
begin
 aTokens.Clear;
 Index:=1;
 while Index<=length(aLine) do begin
  while (Index<=length(aLine)) and (aLine[Index]<=' ') do begin
   inc(Index);
  end;
  if Index>length(aLine) then begin
   break;
  end;
  Start:=Index;
  while (Index<=length(aLine)) and (aLine[Index]>' ') do begin
   inc(Index);
  end;
  aTokens.Add(Copy(aLine,Start,Index-Start));
 end;
end;

constructor TMapFileReader.Create(const aBuilder:TSymbolBuilder;const aImageBase:TpvUInt64;const aWantSymbols,aWantLines:Boolean);
begin
 inherited Create;
 fBuilder:=aBuilder;
 fImageBase:=aImageBase;
 fWantSymbols:=aWantSymbols;
 fWantLines:=aWantLines;
 fSegments:=nil;
end;

function TMapFileReader.ParseSegmentedAddress(const aToken:TpvUTF8String;out aVirtualAddress:TpvUInt64):Boolean;
var ColonPosition:TpvSizeInt;
    SegmentIndex:TpvUInt32;
    Offset:TpvUInt64;
    Index:TpvSizeInt;
    Segment:PSegment;
begin
 result:=false;
 aVirtualAddress:=0;
 ColonPosition:=Pos(':',aToken);
 if (ColonPosition<2) or (ColonPosition>=length(aToken)) then begin
  exit;
 end;
 SegmentIndex:=TpvUInt32(ParseHex(Copy(aToken,1,ColonPosition-1)));
 Offset:=ParseHex(Copy(aToken,ColonPosition+1,length(aToken)-ColonPosition));
 for Index:=0 to length(fSegments)-1 do begin
  Segment:=@fSegments[Index];
  if Segment^.SegmentIndex=SegmentIndex then begin
   aVirtualAddress:=Segment^.StartVA+Offset;
   result:=true;
   exit;
  end;
 end;
end;

procedure TMapFileReader.Parse(const aFileName:TpvUTF8String);
type TSection=(scNone,scSegments,scDetailed,scPublicsByValue,scLineNumbers);
var Lines,Tokens:TStringList;
    LineIndex,TokenIndex,Index,MarkerPosition,ClosingPosition:TpvSizeInt;
    Line,Trimmed,UnitName,SourceFileName,Name:TpvUTF8String;
    Section:TSection;
    VirtualAddress,Size:TpvUInt64;
    Segment:TSegment;
    LineNumber:TpvUInt32;
    CurrentLineUnit:TpvUTF8String;
begin

 Section:=scNone;
 UnitName:='';
 SourceFileName:='';
 CurrentLineUnit:='';

 Lines:=TStringList.Create;
 Tokens:=TStringList.Create;
 try

  Lines.LoadFromFile(aFileName);

  for LineIndex:=0 to Lines.Count-1 do begin

   Line:=Lines[LineIndex];
   Trimmed:=Trim(Line);

   if (Pos('Start',Trimmed)=1) and (Pos('Length',Trimmed)>0) then begin
    Section:=scSegments;
    continue;
   end;
   if Trimmed='Detailed map of segments' then begin
    Section:=scDetailed;
    continue;
   end;
   if Pos('Publics by Value',Trimmed)>0 then begin
    Section:=scPublicsByValue;
    continue;
   end;
   if Pos('Publics by Name',Trimmed)>0 then begin
    // Same information as by value, so it is skipped to avoid duplicates.
    Section:=scNone;
    continue;
   end;
   if Pos('Line numbers for ',Trimmed)=1 then begin
    Section:=scLineNumbers;
    // The header reads: Line numbers for UnitName(SourceFile) segment .text
    Name:=Copy(Trimmed,length('Line numbers for ')+1,length(Trimmed));
    MarkerPosition:=Pos(' segment ',Name);
    if MarkerPosition>0 then begin
     Name:=Copy(Name,1,MarkerPosition-1);
    end;
    MarkerPosition:=Pos('(',Name);
    ClosingPosition:=0;
    for Index:=length(Name) downto 1 do begin
     if Name[Index]=')' then begin
      ClosingPosition:=Index;
      break;
     end;
    end;
    if (MarkerPosition>1) and (ClosingPosition>MarkerPosition) then begin
     UnitName:=Copy(Name,1,MarkerPosition-1);
     SourceFileName:=Copy(Name,MarkerPosition+1,ClosingPosition-MarkerPosition-1);
    end else begin
     UnitName:=Name;
     SourceFileName:='';
    end;
    CurrentLineUnit:=UnitName;
    // The detailed segment map only names the unit and never the file, so the
    // file becomes known here and is filled in afterwards.
    fBuilder.SetUnitFileName(UnitName,SourceFileName);
    continue;
   end;
   if (Trimmed='Bound resource files') or (Pos('Program entry point',Trimmed)=1) then begin
    Section:=scNone;
    continue;
   end;

   if length(Trimmed)=0 then begin
    continue;
   end;

   Tokenize(Line,Tokens);
   if Tokens.Count=0 then begin
    continue;
   end;

   case Section of

    scSegments:begin
     // 0001:0000000000401000 0002EF3CH .text  CODE
     if (Tokens.Count>=4) and (Pos(':',Tokens[0])>0) then begin
      Segment.SegmentIndex:=TpvUInt32(ParseHex(Copy(Tokens[0],1,Pos(':',Tokens[0])-1)));
      Segment.StartVA:=ParseHex(Copy(Tokens[0],Pos(':',Tokens[0])+1,length(Tokens[0])));
      Segment.Size:=ParseHex(Tokens[1]);
      Index:=length(fSegments);
      SetLength(fSegments,Index+1);
      fSegments[Index]:=Segment;
     end;
    end;

    scDetailed:begin
     // 0001:00000000 0001198C C=CODE  S=.text  G=(none)  M=System  ALIGN=4
     if (Tokens.Count>=4) and (Pos(':',Tokens[0])>0) then begin
      Name:='';
      for TokenIndex:=0 to Tokens.Count-1 do begin
       if Pos('M=',Tokens[TokenIndex])=1 then begin
        Name:=Copy(Tokens[TokenIndex],3,length(Tokens[TokenIndex]));
        break;
       end;
      end;
      if (length(Name)>0) and (Pos('C=CODE',Line)>0) and ParseSegmentedAddress(Tokens[0],VirtualAddress) then begin
       Size:=ParseHex(Tokens[1]);
       if Size>0 then begin
        fBuilder.AddUnit(Name,'',VirtualAddress-fImageBase,Size);
       end;
      end;
     end;
    end;

    scPublicsByValue:begin
     // 0001:000005B8       System..TObject
     if fWantSymbols and (Tokens.Count>=2) and (Pos(':',Tokens[0])>0) then begin
      if ParseSegmentedAddress(Tokens[0],VirtualAddress) then begin
       fBuilder.AddSymbol(VirtualAddress-fImageBase,Tokens[1]);
      end;
     end;
    end;

    scLineNumbers:begin
     // Pairs of a decimal line number and a segmented address.
     if fWantLines and (length(CurrentLineUnit)>0) then begin
      TokenIndex:=0;
      while (TokenIndex+1)<Tokens.Count do begin
       LineNumber:=TpvUInt32(StrToIntDef(Tokens[TokenIndex],0));
       if (LineNumber>0) and ParseSegmentedAddress(Tokens[TokenIndex+1],VirtualAddress) then begin
        fBuilder.AddLine(VirtualAddress-fImageBase,LineNumber);
       end;
       inc(TokenIndex,2);
      end;
     end;
    end;

    else begin
    end;

   end;

  end;

 finally
  FreeAndNil(Tokens);
  FreeAndNil(Lines);
 end;

end;

end.
