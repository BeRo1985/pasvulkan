program mapsymbols;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

// Converts a Delphi .map file into the compact PasVulkan symbol table format
// and appends it to the executable, so that a shipped build can symbolicate its
// own crash logs without any separate symbol file.
//
// This exists because Delphi has no counterpart to the lnfodwrf unit of
// FreePascal. Without it a Delphi built crash log can only ever show module
// plus offset, and resolving that afterwards needs the matching .map, which
// nobody has when a crash log arrives from a player.
//
// The address arithmetic of a Delphi .map, which is not obvious and is stated
// wrongly in one place by the compiler itself:
//
//   - The Start column of the segment table holds the virtual address of the
//     segment, so .text typically reads 0000000000401000.
//   - Every SSSS:OOOOOOOO elsewhere in the file, in the detailed segment map,
//     in the publics and in the line numbers, is segment relative. The virtual
//     address is therefore SegmentStartVA + Offset.
//   - The "Program entry point at" line does not follow that rule. It prints
//     ImageBase + Offset and is thus off by the distance between the image base
//     and the segment start. It is ignored here.
//
// Everything is stored relative to the image base, which is read out of the PE
// header of the executable, so that the reader can add the actual load address
// at runtime and stay correct under address space layout randomization.

uses SysUtils,
     Classes,
     PasVulkan.Types,
     PasVulkan.SymbolTable;

type TSegment=record
      SegmentIndex:TpvUInt32;
      StartVA:TpvUInt64;
      Size:TpvUInt64;
      SegmentClass:String;
     end;

     TSegments=array of TSegment;

     TUnitRecord=record
      Name:String;
      FileName:String;
      StartRVA:TpvUInt64;
      Size:TpvUInt64;
      NameOffset:TpvUInt32;
      FileNameOffset:TpvUInt32;
     end;

     TUnitRecords=array of TUnitRecord;

     TSymbolRecord=record
      RVA:TpvUInt64;
      Name:String;
      NameOffset:TpvUInt32;
     end;

     TSymbolRecords=array of TSymbolRecord;

     TLineRecord=record
      RVA:TpvUInt64;
      LineNumber:TpvUInt32;
      UnitIndex:TpvUInt32;
     end;

     TLineRecords=array of TLineRecord;

var Segments:TSegments;
    UnitRecords:TUnitRecords;
    SymbolRecords:TSymbolRecords;
    LineRecords:TLineRecords;
    UnitCount,SymbolCount,LineCount:TpvSizeInt;
    ImageBase:TpvUInt64;
    WantSymbols:Boolean=true;
    WantLines:Boolean=true;

function ParseHex(const aValue:String):TpvUInt64;
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

// Splits a line into whitespace separated tokens.
procedure Tokenize(const aLine:String;const aTokens:TStringList);
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

// Recognizes a SSSS:OOOOOOOO token and turns it into a virtual address.
function ParseSegmentedAddress(const aToken:String;out aVirtualAddress:TpvUInt64):Boolean;
var ColonPosition:TpvSizeInt;
    SegmentIndex:TpvUInt32;
    Offset:TpvUInt64;
    Index:TpvSizeInt;
begin
 result:=false;
 aVirtualAddress:=0;
 ColonPosition:=Pos(':',aToken);
 if (ColonPosition<2) or (ColonPosition>=length(aToken)) then begin
  exit;
 end;
 SegmentIndex:=TpvUInt32(ParseHex(Copy(aToken,1,ColonPosition-1)));
 Offset:=ParseHex(Copy(aToken,ColonPosition+1,length(aToken)-ColonPosition));
 for Index:=0 to length(Segments)-1 do begin
  if Segments[Index].SegmentIndex=SegmentIndex then begin
   aVirtualAddress:=Segments[Index].StartVA+Offset;
   result:=true;
   exit;
  end;
 end;
end;

function ReadPEImageBase(const aFileName:String;out aImageBase:TpvUInt64):Boolean;
var Stream:TFileStream;
    Signature:array[0..3] of AnsiChar;
    NewHeaderOffset:TpvUInt32;
    OptionalMagic:TpvUInt16;
    Value32:TpvUInt32;
begin
 result:=false;
 aImageBase:=0;
 Stream:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyNone);
 try
  if Stream.Size<64 then begin
   exit;
  end;
  Stream.Seek(TpvInt64($3c),soBeginning);
  Stream.ReadBuffer(NewHeaderOffset,SizeOf(TpvUInt32));
  if TpvInt64(NewHeaderOffset)+256>Stream.Size then begin
   exit;
  end;
  Stream.Seek(TpvInt64(NewHeaderOffset),soBeginning);
  Stream.ReadBuffer(Signature,4);
  if (Signature[0]<>'P') or (Signature[1]<>'E') or (Signature[2]<>#0) or (Signature[3]<>#0) then begin
   exit;
  end;
  // Skip the COFF file header, which is 20 bytes, to reach the optional header.
  Stream.Seek(TpvInt64(NewHeaderOffset)+24,soBeginning);
  Stream.ReadBuffer(OptionalMagic,SizeOf(TpvUInt16));
  if OptionalMagic=$20b then begin
   // PE32+, the image base is a 64 bit field at offset 24 of the optional header.
   Stream.Seek(TpvInt64(NewHeaderOffset)+24+24,soBeginning);
   Stream.ReadBuffer(aImageBase,SizeOf(TpvUInt64));
   result:=true;
  end else if OptionalMagic=$10b then begin
   // PE32, the image base is a 32 bit field at offset 28 of the optional header.
   Stream.Seek(TpvInt64(NewHeaderOffset)+24+28,soBeginning);
   Stream.ReadBuffer(Value32,SizeOf(TpvUInt32));
   aImageBase:=Value32;
   result:=true;
  end;
 finally
  FreeAndNil(Stream);
 end;
end;

procedure AddUnitRecord(const aName,aFileName:String;const aStartRVA,aSize:TpvUInt64);
begin
 if UnitCount>=length(UnitRecords) then begin
  SetLength(UnitRecords,(UnitCount+1)*2);
 end;
 UnitRecords[UnitCount].Name:=aName;
 UnitRecords[UnitCount].FileName:=aFileName;
 UnitRecords[UnitCount].StartRVA:=aStartRVA;
 UnitRecords[UnitCount].Size:=aSize;
 inc(UnitCount);
end;

procedure AddSymbolRecord(const aRVA:TpvUInt64;const aName:String);
begin
 if SymbolCount>=length(SymbolRecords) then begin
  SetLength(SymbolRecords,(SymbolCount+1)*2);
 end;
 SymbolRecords[SymbolCount].RVA:=aRVA;
 SymbolRecords[SymbolCount].Name:=aName;
 inc(SymbolCount);
end;

procedure AddLineRecord(const aRVA:TpvUInt64;const aLineNumber:TpvUInt32);
begin
 if LineCount>=length(LineRecords) then begin
  SetLength(LineRecords,(LineCount+1)*2);
 end;
 LineRecords[LineCount].RVA:=aRVA;
 LineRecords[LineCount].LineNumber:=aLineNumber;
 LineRecords[LineCount].UnitIndex:=High(TpvUInt32);
 inc(LineCount);
end;

procedure SortSymbols(const aLeft,aRight:TpvSizeInt);
var Low,High:TpvSizeInt;
    Pivot:TpvUInt64;
    Temporary:TSymbolRecord;
begin
 Low:=aLeft;
 High:=aRight;
 Pivot:=SymbolRecords[(aLeft+aRight) shr 1].RVA;
 repeat
  while SymbolRecords[Low].RVA<Pivot do begin
   inc(Low);
  end;
  while SymbolRecords[High].RVA>Pivot do begin
   dec(High);
  end;
  if Low<=High then begin
   Temporary:=SymbolRecords[Low];
   SymbolRecords[Low]:=SymbolRecords[High];
   SymbolRecords[High]:=Temporary;
   inc(Low);
   dec(High);
  end;
 until Low>High;
 if aLeft<High then begin
  SortSymbols(aLeft,High);
 end;
 if Low<aRight then begin
  SortSymbols(Low,aRight);
 end;
end;

procedure SortLines(const aLeft,aRight:TpvSizeInt);
var Low,High:TpvSizeInt;
    Pivot:TpvUInt64;
    Temporary:TLineRecord;
begin
 Low:=aLeft;
 High:=aRight;
 Pivot:=LineRecords[(aLeft+aRight) shr 1].RVA;
 repeat
  while LineRecords[Low].RVA<Pivot do begin
   inc(Low);
  end;
  while LineRecords[High].RVA>Pivot do begin
   dec(High);
  end;
  if Low<=High then begin
   Temporary:=LineRecords[Low];
   LineRecords[Low]:=LineRecords[High];
   LineRecords[High]:=Temporary;
   inc(Low);
   dec(High);
  end;
 until Low>High;
 if aLeft<High then begin
  SortLines(aLeft,High);
 end;
 if Low<aRight then begin
  SortLines(Low,aRight);
 end;
end;

procedure SortUnits(const aLeft,aRight:TpvSizeInt);
var Low,High:TpvSizeInt;
    Pivot:TpvUInt64;
    Temporary:TUnitRecord;
begin
 Low:=aLeft;
 High:=aRight;
 Pivot:=UnitRecords[(aLeft+aRight) shr 1].StartRVA;
 repeat
  while UnitRecords[Low].StartRVA<Pivot do begin
   inc(Low);
  end;
  while UnitRecords[High].StartRVA>Pivot do begin
   dec(High);
  end;
  if Low<=High then begin
   Temporary:=UnitRecords[Low];
   UnitRecords[Low]:=UnitRecords[High];
   UnitRecords[High]:=Temporary;
   inc(Low);
   dec(High);
  end;
 until Low>High;
 if aLeft<High then begin
  SortUnits(aLeft,High);
 end;
 if Low<aRight then begin
  SortUnits(Low,aRight);
 end;
end;

procedure ParseMapFile(const aFileName:String);
type TSection=(scNone,scSegments,scDetailed,scPublicsByValue,scLineNumbers);
var Lines,Tokens:TStringList;
    LineIndex,TokenIndex,Index,MarkerPosition,ClosingPosition:TpvSizeInt;
    Line,Trimmed,UnitName,SourceFileName,Name:String;
    Section:TSection;
    VirtualAddress,Size:TpvUInt64;
    Segment:TSegment;
    LineNumber:TpvUInt32;
    CurrentLineUnit:String;
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

   // Section headers
   if Pos('Start',Trimmed)=1 then begin
    if Pos('Length',Trimmed)>0 then begin
     Section:=scSegments;
     continue;
    end;
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
    // Remember the source file for the matching unit records, since the
    // detailed segment map only names the unit and never the file.
    for Index:=0 to UnitCount-1 do begin
     if SameText(UnitRecords[Index].Name,UnitName) and (length(UnitRecords[Index].FileName)=0) then begin
      UnitRecords[Index].FileName:=SourceFileName;
     end;
    end;
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
      Segment.SegmentClass:=Tokens[3];
      Index:=length(Segments);
      SetLength(Segments,Index+1);
      Segments[Index]:=Segment;
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
        AddUnitRecord(Name,'',VirtualAddress-ImageBase,Size);
       end;
      end;
     end;
    end;

    scPublicsByValue:begin
     // 0001:000005B8       System..TObject
     if WantSymbols and (Tokens.Count>=2) and (Pos(':',Tokens[0])>0) then begin
      if ParseSegmentedAddress(Tokens[0],VirtualAddress) then begin
       AddSymbolRecord(VirtualAddress-ImageBase,Tokens[1]);
      end;
     end;
    end;

    scLineNumbers:begin
     // Pairs of a decimal line number and a segmented address.
     if WantLines and (length(CurrentLineUnit)>0) then begin
      TokenIndex:=0;
      while (TokenIndex+1)<Tokens.Count do begin
       LineNumber:=TpvUInt32(StrToIntDef(Tokens[TokenIndex],0));
       if (LineNumber>0) and ParseSegmentedAddress(Tokens[TokenIndex+1],VirtualAddress) then begin
        AddLineRecord(VirtualAddress-ImageBase,LineNumber);
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

// Assigns every line record to the unit record whose address range covers it.
// Doing this by address rather than by name is what keeps it correct when one
// unit contributes several separate code ranges.
procedure AssignLinesToUnits;
var LineIndex,Low,High,Middle:TpvSizeInt;
begin
 for LineIndex:=0 to LineCount-1 do begin
  Low:=0;
  High:=UnitCount-1;
  while Low<=High do begin
   Middle:=Low+((High-Low) shr 1);
   if UnitRecords[Middle].StartRVA>LineRecords[LineIndex].RVA then begin
    High:=Middle-1;
   end else if (UnitRecords[Middle].StartRVA+UnitRecords[Middle].Size)<=LineRecords[LineIndex].RVA then begin
    Low:=Middle+1;
   end else begin
    LineRecords[LineIndex].UnitIndex:=TpvUInt32(Middle);
    break;
   end;
  end;
 end;
end;

var StringStream:TMemoryStream;
    UniqueStrings:TStringList;

// Unit and file names repeat a lot, so those go through a lookup. Symbol names
// are unique per address, so they are simply appended.
function AddString(const aValue:String;const aUnique:Boolean):TpvUInt32;
var Raw:TpvRawByteString;
    Index:TpvSizeInt;
    Zero:AnsiChar;
begin
 if aUnique then begin
  Index:=UniqueStrings.IndexOf(aValue);
  if Index>=0 then begin
   result:=TpvUInt32(TpvPtrUInt(UniqueStrings.Objects[Index]));
   exit;
  end;
 end;
{$ifdef fpc}
 Raw:=TpvRawByteString(aValue);
{$else}
 Raw:=TpvRawByteString(UTF8Encode(aValue));
{$endif}
 result:=TpvUInt32(StringStream.Position);
 if length(Raw)>0 then begin
  StringStream.WriteBuffer(Raw[1],length(Raw));
 end;
 Zero:=#0;
 StringStream.WriteBuffer(Zero,1);
 if aUnique then begin
  UniqueStrings.AddObject(aValue,TObject(TpvPtrUInt(result)));
 end;
end;

procedure WriteAndAppend(const aExecutableFileName:String);
var Stream:TFileStream;
    Header:TpvSymbolTableHeader;
    Footer:TpvSymbolTableFooter;
    ExistingFooter:TpvSymbolTableFooter;
    UnitEntry:TpvSymbolTableUnitEntry;
    SymbolEntry:TpvSymbolTableSymbolEntry;
    LineEntry:TpvSymbolTableLineEntry;
    Index,MatchIndex:TpvSizeInt;
    BlobOffset:TpvInt64;
    Matches:Boolean;
begin

 StringStream:=TMemoryStream.Create;
 UniqueStrings:=TStringList.Create;
 try

  UniqueStrings.Sorted:=false;
  // A leading terminator, so that offset zero always means the empty string.
  AddString('',false);

  for Index:=0 to UnitCount-1 do begin
   UnitRecords[Index].NameOffset:=AddString(UnitRecords[Index].Name,true);
   UnitRecords[Index].FileNameOffset:=AddString(UnitRecords[Index].FileName,true);
  end;

  for Index:=0 to SymbolCount-1 do begin
   SymbolRecords[Index].NameOffset:=AddString(SymbolRecords[Index].Name,false);
  end;

  Stream:=TFileStream.Create(aExecutableFileName,fmOpenReadWrite or fmShareExclusive);
  try

   // Re-running the tool on an already processed executable must replace the
   // previous table instead of stacking another one behind it.
   if Stream.Size>=TpvInt64(SizeOf(TpvSymbolTableFooter)) then begin
    Stream.Seek(-TpvInt64(SizeOf(TpvSymbolTableFooter)),soEnd);
    Stream.ReadBuffer(ExistingFooter,SizeOf(TpvSymbolTableFooter));
    Matches:=true;
    for MatchIndex:=0 to 7 do begin
     if ExistingFooter.Magic[MatchIndex]<>pvSymbolTableMagic[MatchIndex] then begin
      Matches:=false;
      break;
     end;
    end;
    if Matches and
       (ExistingFooter.Offset>0) and
       (ExistingFooter.Offset<TpvUInt64(Stream.Size)) then begin
     WriteLn('Replacing the symbol table which was already appended at offset ',ExistingFooter.Offset,'.');
     Stream.Size:=TpvInt64(ExistingFooter.Offset);
    end;
   end;

   BlobOffset:=Stream.Size;
   Stream.Seek(BlobOffset,soBeginning);

   FillChar(Header,SizeOf(TpvSymbolTableHeader),#0);
   Move(pvSymbolTableMagic[0],Header.Magic[0],8);
   Header.Version:=pvSymbolTableVersion;
   Header.Flags:=0;
   Header.UnitCount:=TpvUInt32(UnitCount);
   Header.SymbolCount:=TpvUInt32(SymbolCount);
   Header.LineCount:=TpvUInt32(LineCount);
   Header.StringSize:=TpvUInt32(StringStream.Size);
   Stream.WriteBuffer(Header,SizeOf(TpvSymbolTableHeader));

   for Index:=0 to UnitCount-1 do begin
    UnitEntry.StartRVA:=UnitRecords[Index].StartRVA;
    UnitEntry.Size:=UnitRecords[Index].Size;
    UnitEntry.NameOffset:=UnitRecords[Index].NameOffset;
    UnitEntry.FileNameOffset:=UnitRecords[Index].FileNameOffset;
    Stream.WriteBuffer(UnitEntry,SizeOf(TpvSymbolTableUnitEntry));
   end;

   for Index:=0 to SymbolCount-1 do begin
    SymbolEntry.RVA:=SymbolRecords[Index].RVA;
    SymbolEntry.NameOffset:=SymbolRecords[Index].NameOffset;
    SymbolEntry.Reserved:=0;
    Stream.WriteBuffer(SymbolEntry,SizeOf(TpvSymbolTableSymbolEntry));
   end;

   for Index:=0 to LineCount-1 do begin
    LineEntry.RVA:=LineRecords[Index].RVA;
    LineEntry.LineNumber:=LineRecords[Index].LineNumber;
    LineEntry.UnitIndex:=LineRecords[Index].UnitIndex;
    Stream.WriteBuffer(LineEntry,SizeOf(TpvSymbolTableLineEntry));
   end;

   StringStream.Seek(0,soBeginning);
   Stream.CopyFrom(StringStream,StringStream.Size);

   FillChar(Footer,SizeOf(TpvSymbolTableFooter),#0);
   Move(pvSymbolTableMagic[0],Footer.Magic[0],8);
   Footer.Offset:=TpvUInt64(BlobOffset);
   Stream.WriteBuffer(Footer,SizeOf(TpvSymbolTableFooter));

   WriteLn('Appended ',Stream.Size-BlobOffset,' bytes of symbol table at offset ',BlobOffset,'.');

  finally
   FreeAndNil(Stream);
  end;

 finally
  FreeAndNil(UniqueStrings);
  FreeAndNil(StringStream);
 end;

end;

procedure SelfCheck(const aExecutableFileName:String);
var SymbolTable:TpvSymbolTable;
    Location:TpvSymbolTableLocation;
    Index,Resolved,Probes:TpvSizeInt;
begin
 SymbolTable:=TpvSymbolTable.Create;
 try
  if not SymbolTable.LoadFromFile(aExecutableFileName) then begin
   WriteLn('Self check failed, the appended table could not be read back.');
   ExitCode:=1;
   exit;
  end;
  // Read a spread of line records back through the public interface, so that
  // both the layout and the lookups get exercised rather than just the writing.
  Resolved:=0;
  Probes:=0;
  Index:=0;
  while (Index<LineCount) and (Probes<64) do begin
   inc(Probes);
   if SymbolTable.Resolve(LineRecords[Index].RVA,Location) and
      (Location.LineNumber=LineRecords[Index].LineNumber) then begin
    inc(Resolved);
   end;
   if LineCount>64 then begin
    inc(Index,LineCount div 64);
   end else begin
    inc(Index);
   end;
  end;
  WriteLn('Self check: ',Resolved,' of ',Probes,' probes resolved to the expected line.');
  if Resolved<>Probes then begin
   ExitCode:=1;
  end;
 finally
  FreeAndNil(SymbolTable);
 end;
end;

var ExecutableFileName,MapFileName,Parameter:String;
    ParameterIndex:TpvSizeInt;

begin

 ExecutableFileName:='';
 MapFileName:='';

 ParameterIndex:=1;
 while ParameterIndex<=ParamCount do begin
  Parameter:=ParamStr(ParameterIndex);
  inc(ParameterIndex);
  if Parameter='--no-symbols' then begin
   WantSymbols:=false;
  end else if Parameter='--no-lines' then begin
   WantLines:=false;
  end else if length(ExecutableFileName)=0 then begin
   ExecutableFileName:=Parameter;
  end else if length(MapFileName)=0 then begin
   MapFileName:=Parameter;
  end;
 end;

 if length(ExecutableFileName)=0 then begin
  WriteLn('mapsymbols - appends a PasVulkan symbol table, built from a Delphi .map,');
  WriteLn('             to an executable, so that it can symbolicate its own crash logs.');
  WriteLn;
  WriteLn('  mapsymbols <executable> [mapfile] [--no-symbols] [--no-lines]');
  WriteLn;
  WriteLn('The map file defaults to the executable name with a .map extension.');
  WriteLn('Running it again on the same executable replaces the previous table.');
  ExitCode:=1;
  exit;
 end;

 if length(MapFileName)=0 then begin
  MapFileName:=ChangeFileExt(ExecutableFileName,'.map');
 end;

 if not FileExists(ExecutableFileName) then begin
  WriteLn('Executable not found: ',ExecutableFileName);
  ExitCode:=1;
  exit;
 end;

 if not FileExists(MapFileName) then begin
  WriteLn('Map file not found: ',MapFileName);
  WriteLn('Enable a detailed map file in the project options, which is DCC_MapFile set to 3.');
  ExitCode:=1;
  exit;
 end;

 if not ReadPEImageBase(ExecutableFileName,ImageBase) then begin
  WriteLn('Could not read the image base out of ',ExecutableFileName,', is that a PE executable?');
  ExitCode:=1;
  exit;
 end;

 WriteLn('Image base $',IntToHex(ImageBase,16));

 UnitCount:=0;
 SymbolCount:=0;
 LineCount:=0;

 ParseMapFile(MapFileName);

 if UnitCount=0 then begin
  WriteLn('No code segments were found in ',MapFileName,'.');
  ExitCode:=1;
  exit;
 end;

 SortUnits(0,UnitCount-1);
 if SymbolCount>0 then begin
  SortSymbols(0,SymbolCount-1);
 end;
 if LineCount>0 then begin
  SortLines(0,LineCount-1);
  AssignLinesToUnits;
 end;

 WriteLn(UnitCount,' units, ',SymbolCount,' symbols, ',LineCount,' line records.');

 if LineCount=0 then begin
  WriteLn('Warning: the map file carries no line numbers, so only symbol names will resolve.');
 end;

 WriteAndAppend(ExecutableFileName);

 SelfCheck(ExecutableFileName);

end.
