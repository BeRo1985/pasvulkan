// Collects units, symbols and line records from whichever frontend produced
// them, and writes them out in the PasVulkan symbol table format.
//
// This sits between the frontends, currently a Delphi .map reader and a DWARF
// .debug_line reader, and the on disk format, so that both of them describe what
// they found in the same terms and neither has to know anything about the
// layout, the sorting or the string table.
unit UnitSymbolBuilder;
{$ifdef fpc}
 {$mode delphi}
{$endif}

interface

uses SysUtils,
     Classes,
     PasVulkan.Types,
     PasVulkan.SymbolTable;

type TSymbolBuilder=class
      public
       type TUnitRecord=record
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
            // A fingerprint of everything which was collected, wide enough to
            // be used as a debug identity.
            TDigest=array[0..15] of TpvUInt8;
      private
       fUnits:TUnitRecords;
       fSymbols:TSymbolRecords;
       fLines:TLineRecords;
       fUnitCount:TpvSizeInt;
       fSymbolCount:TpvSizeInt;
       fLineCount:TpvSizeInt;
       fStringStream:TMemoryStream;
       fUniqueStrings:TStringList;
       fImageBase:TpvUInt64;
       fStripPaths:Boolean;
       function PreparePath(const aFileName:String):String;
       procedure SortUnits(const aLeft,aRight:TpvSizeInt);
       procedure SortSymbols(const aLeft,aRight:TpvSizeInt);
       procedure SortLines(const aLeft,aRight:TpvSizeInt);
       procedure AssignLinesToUnits;
       function AddString(const aValue:String;const aUnique:Boolean):TpvUInt32;
      public
       constructor Create;
       destructor Destroy; override;
       procedure AddUnit(const aName,aFileName:String;const aStartRVA,aSize:TpvUInt64);
       procedure AddSymbol(const aRVA:TpvUInt64;const aName:String);
       procedure AddLine(const aRVA:TpvUInt64;const aLineNumber:TpvUInt32);
       // Fills in the source file of a unit which was added without one, which
       // is what the Delphi .map frontend needs, since its segment map names
       // only the unit and the file shows up later in the line number section.
       procedure SetUnitFileName(const aUnitName,aFileName:String);
       // Sorts everything and ties the line records to their units. Must be
       // called once after the last Add and before writing.
       procedure Finish;
       procedure AppendToFile(const aFileName:String);
       // Reads the written table back through the public reader and checks that
       // a spread of line records resolves to the line it was built from.
       function SelfCheck(const aFileName:String;out aResolved,aProbes:TpvSizeInt):Boolean;
       // Digests the collected content, see the implementation for why a
       // counter would not do.
       procedure ComputeDigest(out aDigest:TDigest);
       // Link time base of the image, written into the header so that the
       // reader can turn a load bias back into a base at runtime.
       property ImageBase:TpvUInt64 read fImageBase write fImageBase;
       // When set, only the base name of a source file is kept. A shipped
       // binary otherwise carries the full build tree of whoever built it,
       // which says more about that machine than a crash log needs to.
       property StripPaths:Boolean read fStripPaths write fStripPaths;
       // Read access for the writers, which need the collected data again in a
       // different shape. Only valid after Finish, since that is what sorts it.
       function GetUnit(const aIndex:TpvSizeInt):TUnitRecord;
       function GetSymbol(const aIndex:TpvSizeInt):TSymbolRecord;
       function GetLine(const aIndex:TpvSizeInt):TLineRecord;
       property UnitCount:TpvSizeInt read fUnitCount;
       property SymbolCount:TpvSizeInt read fSymbolCount;
       property LineCount:TpvSizeInt read fLineCount;
     end;

implementation

constructor TSymbolBuilder.Create;
begin
 inherited Create;
 fUnits:=nil;
 fSymbols:=nil;
 fLines:=nil;
 fUnitCount:=0;
 fSymbolCount:=0;
 fLineCount:=0;
 fStringStream:=nil;
 fUniqueStrings:=nil;
 fImageBase:=0;
 fStripPaths:=false;
end;

function TSymbolBuilder.PreparePath(const aFileName:String):String;
begin
 if fStripPaths then begin
  result:=ExtractFileName(aFileName);
 end else begin
  result:=aFileName;
 end;
end;

destructor TSymbolBuilder.Destroy;
begin
 fUnits:=nil;
 fSymbols:=nil;
 fLines:=nil;
 FreeAndNil(fStringStream);
 FreeAndNil(fUniqueStrings);
 inherited Destroy;
end;

procedure TSymbolBuilder.AddUnit(const aName,aFileName:String;const aStartRVA,aSize:TpvUInt64);
begin
 if fUnitCount>=length(fUnits) then begin
  SetLength(fUnits,(fUnitCount+1)*2);
 end;
 fUnits[fUnitCount].Name:=aName;
 fUnits[fUnitCount].FileName:=PreparePath(aFileName);
 fUnits[fUnitCount].StartRVA:=aStartRVA;
 fUnits[fUnitCount].Size:=aSize;
 fUnits[fUnitCount].NameOffset:=0;
 fUnits[fUnitCount].FileNameOffset:=0;
 inc(fUnitCount);
end;

procedure TSymbolBuilder.AddSymbol(const aRVA:TpvUInt64;const aName:String);
begin
 if fSymbolCount>=length(fSymbols) then begin
  SetLength(fSymbols,(fSymbolCount+1)*2);
 end;
 fSymbols[fSymbolCount].RVA:=aRVA;
 fSymbols[fSymbolCount].Name:=aName;
 fSymbols[fSymbolCount].NameOffset:=0;
 inc(fSymbolCount);
end;

procedure TSymbolBuilder.AddLine(const aRVA:TpvUInt64;const aLineNumber:TpvUInt32);
begin
 if fLineCount>=length(fLines) then begin
  SetLength(fLines,(fLineCount+1)*2);
 end;
 fLines[fLineCount].RVA:=aRVA;
 fLines[fLineCount].LineNumber:=aLineNumber;
 fLines[fLineCount].UnitIndex:=High(TpvUInt32);
 inc(fLineCount);
end;

procedure TSymbolBuilder.SetUnitFileName(const aUnitName,aFileName:String);
var Index:TpvSizeInt;
begin
 for Index:=0 to fUnitCount-1 do begin
  if SameText(fUnits[Index].Name,aUnitName) and (length(fUnits[Index].FileName)=0) then begin
   fUnits[Index].FileName:=PreparePath(aFileName);
  end;
 end;
end;

// A fingerprint over everything which was collected, so that two builds which
// differ anywhere in their units, symbols or line records end up with different
// identities, while building the same input twice keeps giving the same one.
//
// A counter of any kind will not do here. Two builds with the same number of
// line records are the normal case for a small change, and anything which
// caches debug information by identity, a symbol server or a debugger, would
// then quietly hand out the symbols of the wrong build.
procedure TSymbolBuilder.ComputeDigest(out aDigest:TSymbolBuilder.TDigest);
var Index:TpvSizeInt;
    Low,High:TpvUInt64;

 procedure Feed(const aValue:TpvUInt64);
 begin
  Low:=Low xor aValue;
  Low:=Low*TpvUInt64($00000100000001b3);
  Low:=Low xor (Low shr 29);
  inc(High,Low xor TpvUInt64($9e3779b97f4a7c15));
  High:=High*TpvUInt64($ff51afd7ed558ccd);
  High:=High xor (High shr 32);
 end;

 procedure FeedString(const aValue:String);
 var Position:TpvSizeInt;
 begin
  Feed(TpvUInt64(length(aValue)));
  for Position:=1 to length(aValue) do begin
   Feed(TpvUInt64(Ord(aValue[Position])));
  end;
 end;

begin

 Low:=TpvUInt64($cbf29ce484222325);
 High:=TpvUInt64($9e3779b97f4a7c15);

 Feed(fImageBase);

 Feed(TpvUInt64(fUnitCount));
 for Index:=0 to fUnitCount-1 do begin
  Feed(fUnits[Index].StartRVA);
  Feed(fUnits[Index].Size);
  FeedString(fUnits[Index].Name);
  FeedString(fUnits[Index].FileName);
 end;

 Feed(TpvUInt64(fSymbolCount));
 for Index:=0 to fSymbolCount-1 do begin
  Feed(fSymbols[Index].RVA);
  FeedString(fSymbols[Index].Name);
 end;

 Feed(TpvUInt64(fLineCount));
 for Index:=0 to fLineCount-1 do begin
  Feed(fLines[Index].RVA);
  Feed(TpvUInt64(fLines[Index].LineNumber));
  Feed(TpvUInt64(fLines[Index].UnitIndex));
 end;

 for Index:=0 to 7 do begin
  aDigest[Index]:=TpvUInt8((Low shr (Index shl 3)) and $ff);
  aDigest[Index+8]:=TpvUInt8((High shr (Index shl 3)) and $ff);
 end;

end;

function TSymbolBuilder.GetUnit(const aIndex:TpvSizeInt):TUnitRecord;
begin
 result:=fUnits[aIndex];
end;

function TSymbolBuilder.GetSymbol(const aIndex:TpvSizeInt):TSymbolRecord;
begin
 result:=fSymbols[aIndex];
end;

function TSymbolBuilder.GetLine(const aIndex:TpvSizeInt):TLineRecord;
begin
 result:=fLines[aIndex];
end;

procedure TSymbolBuilder.SortUnits(const aLeft,aRight:TpvSizeInt);
var Low,High:TpvSizeInt;
    Pivot:TpvUInt64;
    Temporary:TUnitRecord;
begin
 Low:=aLeft;
 High:=aRight;
 Pivot:=fUnits[(aLeft+aRight) shr 1].StartRVA;
 repeat
  while fUnits[Low].StartRVA<Pivot do begin
   inc(Low);
  end;
  while fUnits[High].StartRVA>Pivot do begin
   dec(High);
  end;
  if Low<=High then begin
   Temporary:=fUnits[Low];
   fUnits[Low]:=fUnits[High];
   fUnits[High]:=Temporary;
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

procedure TSymbolBuilder.SortSymbols(const aLeft,aRight:TpvSizeInt);
var Low,High:TpvSizeInt;
    Pivot:TpvUInt64;
    Temporary:TSymbolRecord;
begin
 Low:=aLeft;
 High:=aRight;
 Pivot:=fSymbols[(aLeft+aRight) shr 1].RVA;
 repeat
  while fSymbols[Low].RVA<Pivot do begin
   inc(Low);
  end;
  while fSymbols[High].RVA>Pivot do begin
   dec(High);
  end;
  if Low<=High then begin
   Temporary:=fSymbols[Low];
   fSymbols[Low]:=fSymbols[High];
   fSymbols[High]:=Temporary;
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

procedure TSymbolBuilder.SortLines(const aLeft,aRight:TpvSizeInt);
var Low,High:TpvSizeInt;
    Pivot:TpvUInt64;
    Temporary:TLineRecord;
begin
 Low:=aLeft;
 High:=aRight;
 Pivot:=fLines[(aLeft+aRight) shr 1].RVA;
 repeat
  while fLines[Low].RVA<Pivot do begin
   inc(Low);
  end;
  while fLines[High].RVA>Pivot do begin
   dec(High);
  end;
  if Low<=High then begin
   Temporary:=fLines[Low];
   fLines[Low]:=fLines[High];
   fLines[High]:=Temporary;
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

// Ties every line record to the unit whose address range covers it. Doing this
// by address rather than by name is what keeps it correct when one unit
// contributes several separate code ranges.
procedure TSymbolBuilder.AssignLinesToUnits;
var LineIndex,Low,High,Middle:TpvSizeInt;
begin
 for LineIndex:=0 to fLineCount-1 do begin
  Low:=0;
  High:=fUnitCount-1;
  while Low<=High do begin
   Middle:=Low+((High-Low) shr 1);
   if fUnits[Middle].StartRVA>fLines[LineIndex].RVA then begin
    High:=Middle-1;
   end else if (fUnits[Middle].StartRVA+fUnits[Middle].Size)<=fLines[LineIndex].RVA then begin
    Low:=Middle+1;
   end else begin
    fLines[LineIndex].UnitIndex:=TpvUInt32(Middle);
    break;
   end;
  end;
 end;
end;

procedure TSymbolBuilder.Finish;
begin
 if fUnitCount>0 then begin
  SortUnits(0,fUnitCount-1);
 end;
 if fSymbolCount>0 then begin
  SortSymbols(0,fSymbolCount-1);
 end;
 if fLineCount>0 then begin
  SortLines(0,fLineCount-1);
  AssignLinesToUnits;
 end;
end;

// Unit and file names repeat a lot, so those go through a lookup. Symbol names
// are unique per address, so they are simply appended.
function TSymbolBuilder.AddString(const aValue:String;const aUnique:Boolean):TpvUInt32;
var Raw:TpvRawByteString;
    Index:TpvSizeInt;
    Zero:AnsiChar;
begin
 if aUnique then begin
  Index:=fUniqueStrings.IndexOf(aValue);
  if Index>=0 then begin
   result:=TpvUInt32(TpvPtrUInt(fUniqueStrings.Objects[Index]));
   exit;
  end;
 end;
{$ifdef fpc}
 Raw:=TpvRawByteString(aValue);
{$else}
 Raw:=TpvRawByteString(UTF8Encode(aValue));
{$endif}
 result:=TpvUInt32(fStringStream.Position);
 if length(Raw)>0 then begin
  fStringStream.WriteBuffer(Raw[1],length(Raw));
 end;
 Zero:=#0;
 fStringStream.WriteBuffer(Zero,1);
 if aUnique then begin
  fUniqueStrings.AddObject(aValue,TObject(TpvPtrUInt(result)));
 end;
end;

procedure TSymbolBuilder.AppendToFile(const aFileName:String);
var Stream:TFileStream;
    Header:TpvSymbolTableHeader;
    Footer,ExistingFooter:TpvSymbolTableFooter;
    UnitEntry:TpvSymbolTableUnitEntry;
    SymbolEntry:TpvSymbolTableSymbolEntry;
    LineEntry:TpvSymbolTableLineEntry;
    Index,MatchIndex:TpvSizeInt;
    BlobOffset:TpvInt64;
    Matches:Boolean;
begin

 FreeAndNil(fStringStream);
 FreeAndNil(fUniqueStrings);
 fStringStream:=TMemoryStream.Create;
 fUniqueStrings:=TStringList.Create;

 // A leading terminator, so that offset zero always means the empty string.
 AddString('',false);

 for Index:=0 to fUnitCount-1 do begin
  fUnits[Index].NameOffset:=AddString(fUnits[Index].Name,true);
  fUnits[Index].FileNameOffset:=AddString(fUnits[Index].FileName,true);
 end;

 for Index:=0 to fSymbolCount-1 do begin
  fSymbols[Index].NameOffset:=AddString(fSymbols[Index].Name,false);
 end;

 Stream:=TFileStream.Create(aFileName,fmOpenReadWrite or fmShareExclusive);
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
   if Matches and (ExistingFooter.Offset>0) and (ExistingFooter.Offset<TpvUInt64(Stream.Size)) then begin
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
  Header.ImageBase:=fImageBase;
  Header.UnitCount:=TpvUInt32(fUnitCount);
  Header.SymbolCount:=TpvUInt32(fSymbolCount);
  Header.LineCount:=TpvUInt32(fLineCount);
  Header.StringSize:=TpvUInt32(fStringStream.Size);
  Stream.WriteBuffer(Header,SizeOf(TpvSymbolTableHeader));

  for Index:=0 to fUnitCount-1 do begin
   UnitEntry.StartRVA:=fUnits[Index].StartRVA;
   UnitEntry.Size:=fUnits[Index].Size;
   UnitEntry.NameOffset:=fUnits[Index].NameOffset;
   UnitEntry.FileNameOffset:=fUnits[Index].FileNameOffset;
   Stream.WriteBuffer(UnitEntry,SizeOf(TpvSymbolTableUnitEntry));
  end;

  for Index:=0 to fSymbolCount-1 do begin
   SymbolEntry.RVA:=fSymbols[Index].RVA;
   SymbolEntry.NameOffset:=fSymbols[Index].NameOffset;
   SymbolEntry.Reserved:=0;
   Stream.WriteBuffer(SymbolEntry,SizeOf(TpvSymbolTableSymbolEntry));
  end;

  for Index:=0 to fLineCount-1 do begin
   LineEntry.RVA:=fLines[Index].RVA;
   LineEntry.LineNumber:=fLines[Index].LineNumber;
   LineEntry.UnitIndex:=fLines[Index].UnitIndex;
   Stream.WriteBuffer(LineEntry,SizeOf(TpvSymbolTableLineEntry));
  end;

  fStringStream.Seek(0,soBeginning);
  Stream.CopyFrom(fStringStream,fStringStream.Size);

  FillChar(Footer,SizeOf(TpvSymbolTableFooter),#0);
  Move(pvSymbolTableMagic[0],Footer.Magic[0],8);
  Footer.Offset:=TpvUInt64(BlobOffset);
  Stream.WriteBuffer(Footer,SizeOf(TpvSymbolTableFooter));

  WriteLn('Appended ',Stream.Size-BlobOffset,' bytes of symbol table at offset ',BlobOffset,'.');

 finally
  FreeAndNil(Stream);
 end;

end;

function TSymbolBuilder.SelfCheck(const aFileName:String;out aResolved,aProbes:TpvSizeInt):Boolean;
var SymbolTable:TpvSymbolTable;
    Location:TpvSymbolTableLocation;
    Index,Step:TpvSizeInt;
begin
 aResolved:=0;
 aProbes:=0;
 result:=false;
 SymbolTable:=TpvSymbolTable.Create;
 try
  if not SymbolTable.LoadFromFile(aFileName) then begin
   exit;
  end;
  if fLineCount>64 then begin
   Step:=fLineCount div 64;
  end else begin
   Step:=1;
  end;
  Index:=0;
  while (Index<fLineCount) and (aProbes<64) do begin
   // End of sequence markers are skipped rather than probed. They would match
   // trivially, since both sides are then zero, and would only water down what
   // the count says.
   if fLines[Index].LineNumber>0 then begin
    inc(aProbes);
    if SymbolTable.Resolve(fLines[Index].RVA,Location) and
       (Location.LineNumber=fLines[Index].LineNumber) then begin
     inc(aResolved);
    end;
   end;
   inc(Index,Step);
  end;
  result:=aResolved=aProbes;
 finally
  FreeAndNil(SymbolTable);
 end;
end;

end.
