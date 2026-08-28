// Collects units, symbols and line records from whichever frontend produced
// them, and writes them out in the PasVulkan symbol table format.
//
// This sits between the frontends, currently a Delphi .map reader and a DWARF
// .debug_line reader, and the on disk format, so that both of them describe what
// they found in the same terms and neither has to know anything about the
// layout, the sorting or the string table.
//
// The written table is little endian whatever machine it describes, which is
// what the reader in PasVulkan.SymbolTable states and undoes for itself. The
// writing here does no such thing: it puts records down as this machine holds
// them, which is the same thing only as long as this machine is little endian.
// So rather than leave the rule and the one implementation which has to keep it
// free to drift apart unnoticed, a big endian host does not build this at all.
// Writing it the other way round is a few lines, but they would be a few lines
// nobody could run.
unit UnitSymbolBuilder;
{$ifdef fpc}
 {$mode delphi}
{$endif}

{$if defined(ENDIAN_BIG) or defined(FPC_BIG_ENDIAN) or defined(BIG_ENDIAN)}
 {$error This tool writes its table in host byte order and the format is little endian, so it cannot be built on a big endian host}
{$ifend}

interface

uses SysUtils,
     Classes,
     PasVulkan.Types,
     PasVulkan.Compression.LZBRSF,
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
       fCompress:Boolean;
       fPackedFrom:TpvUInt64;
       fPackedTo:TpvUInt64;
       fTrimmedUnitCount:TpvSizeInt;
       function PreparePath(const aFileName:String):String;
{$ifndef PasVulkanMapSymbolsNoOverlapTrimming}
       function TrimOverlappingUnits:TpvSizeInt;
{$endif}
       procedure WritePayload(const aStream:TStream);
       procedure SortUnits(const aLeft,aRight:TpvSizeInt);
       procedure SortSymbols(const aLeft,aRight:TpvSizeInt);
       procedure SortLines(const aLeft,aRight:TpvSizeInt);
       procedure DropRedundantEndMarkers;
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
       // When set, only the base name of a source file goes into the appended
       // table. A shipped binary otherwise carries the full build tree of
       // whoever built it, which says more about that machine than a crash log
       // needs to. Does not affect the pdb or the standalone debug file, see
       // PreparePath.
       property StripPaths:Boolean read fStripPaths write fStripPaths;
       // When set, everything behind the header is packed. Measured at about a
       // third of what the executable would otherwise grow by, at the price of
       // the reader having to ask for memory while it is unpacking, which is
       // why the reading side of it is behind a define of its own.
       property Compress:Boolean read fCompress write fCompress;
       // What packing achieved, for the caller to report. Both zero when
       // nothing was packed, either because it was not asked for or because it
       // would not have come out smaller. Kept here rather than written out
       // from inside, so that every message of the tool comes from one place.
       property PackedFrom:TpvUInt64 read fPackedFrom;
       property PackedTo:TpvUInt64 read fPackedTo;
       // How many unit ranges Finish had to pull back off the one behind them.
       // Reported rather than kept quiet, since it is a repair of information
       // which came in slightly wrong and the caller should be able to see that
       // it happened at all.
       property TrimmedUnitCount:TpvSizeInt read fTrimmedUnitCount;
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
 fCompress:=false;
 fPackedFrom:=0;
 fPackedTo:=0;
 fTrimmedUnitCount:=0;
end;

// The contents behind the header, which is what is either written straight out
// or packed first. Split out so that both ways lay out exactly the same bytes.
procedure TSymbolBuilder.WritePayload(const aStream:TStream);
var Index:TpvSizeInt;
    UnitEntry:TpvSymbolTableUnitEntry;
    SymbolEntry:TpvSymbolTableSymbolEntry;
    LineEntry:TpvSymbolTableLineEntry;
begin

 for Index:=0 to fUnitCount-1 do begin
  UnitEntry.StartRVA:=fUnits[Index].StartRVA;
  UnitEntry.Size:=fUnits[Index].Size;
  UnitEntry.NameOffset:=fUnits[Index].NameOffset;
  UnitEntry.FileNameOffset:=fUnits[Index].FileNameOffset;
  aStream.WriteBuffer(UnitEntry,SizeOf(TpvSymbolTableUnitEntry));
 end;

 for Index:=0 to fSymbolCount-1 do begin
  SymbolEntry.RVA:=fSymbols[Index].RVA;
  SymbolEntry.NameOffset:=fSymbols[Index].NameOffset;
  SymbolEntry.Reserved:=0;
  aStream.WriteBuffer(SymbolEntry,SizeOf(TpvSymbolTableSymbolEntry));
 end;

 for Index:=0 to fLineCount-1 do begin
  LineEntry.RVA:=fLines[Index].RVA;
  LineEntry.LineNumber:=fLines[Index].LineNumber;
  LineEntry.UnitIndex:=fLines[Index].UnitIndex;
  aStream.WriteBuffer(LineEntry,SizeOf(TpvSymbolTableLineEntry));
 end;

 fStringStream.Seek(0,soBeginning);
 aStream.CopyFrom(fStringStream,fStringStream.Size);

end;

// Applied where the string table of the appended block is built, and nowhere
// else. That block is what goes out to whoever runs the program, so that is the
// one which must not carry the build tree. The pdb and the standalone debug
// file stay behind and live off the full paths, since a debugger uses them to
// find the source, so shortening those would only break them.
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
 fUnits[fUnitCount].FileName:=aFileName;
 fUnits[fUnitCount].StartRVA:=aStartRVA;
 fUnits[fUnitCount].Size:=aSize;
 fUnits[fUnitCount].NameOffset:=0;
 fUnits[fUnitCount].FileNameOffset:=0;
 inc(fUnitCount);
end;

procedure TSymbolBuilder.AddSymbol(const aRVA:TpvUInt64;const aName:String);
begin
 // A symbol without a name names nothing, so it has nothing to contribute to a
 // stack trace. Turned away here rather than carried along, which also settles
 // a question the check would otherwise have to ask: a reader is entitled to
 // skip a nameless entry, and the count coming back would then be short of the
 // count which went in and the file would be called broken over an entry which
 // never said anything.
 if length(aName)=0 then begin
  exit;
 end;
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
   fUnits[Index].FileName:=aFileName;
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

{$ifdef PasVulkanSymbolBuilderCharacterWiseDigest}
 // One character at a time, which is how this was first written. Kept because
 // the two do not produce the same digest, so a build which has to keep giving
 // the identity an older version of this tool gave it needs this one.
 procedure FeedString(const aValue:String);
 var Position:TpvSizeInt;
 begin
  Feed(TpvUInt64(length(aValue)));
  for Position:=1 to length(aValue) do begin
   Feed(TpvUInt64(Ord(aValue[Position])));
  end;
 end;
{$else}
 // Eight bytes at a time. A table of a million symbols would otherwise mean
 // tens of millions of rounds through the mixer for the names alone.
 //
 // The name as utf-8 bytes, eight at a time.
 //
 // Every byte of it, not just the lowest of each character: masking the rest
 // away would make two names which differ only above the low byte feed exactly
 // the same thing in, which is not a collision of the kind a digest of this
 // width always has but information thrown away before the mixer ever sees it.
 //
 // And utf-8 rather than whatever a character happens to be here, because this
 // digest is the identity of a pdb. A character is one byte under FreePascal
 // and two under Delphi, so the same table built by the same tool compiled two
 // ways would otherwise get two identities. For a name of plain ascii, which is
 // what a symbol name is, the bytes are the same either way, so this is also
 // the encoding which leaves existing identities where they are.
 procedure FeedString(const aValue:String);
 var Position,Count,ByteIndex:TpvSizeInt;
     Block:TpvUInt64;
     Raw:TpvRawByteString;
 begin
  Raw:=TpvRawByteString(UTF8Encode(aValue));
  Count:=length(Raw);
  Feed(TpvUInt64(Count));
  Block:=0;
  ByteIndex:=0;
  for Position:=1 to Count do begin
   Block:=(Block shl 8) or (TpvUInt64(TpvUInt8(Raw[Position])) and $ff);
   inc(ByteIndex);
   if ByteIndex=8 then begin
    Feed(Block);
    Block:=0;
    ByteIndex:=0;
   end;
  end;
  if ByteIndex>0 then begin
   Feed(Block);
  end;
 end;
{$endif}

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

// Drops the end of sequence markers which do not mark the end of anything.
//
// A marker says that the code described by the rows before it stops at its
// address. Where a real row starts at that same address the code does not stop
// there at all, the next sequence simply begins where the last one ended, and
// the marker describes a gap of zero length. Keeping it would do harm rather
// than nothing: the reader looks for the last record at or below an address,
// which would be the marker, and it would then report that address as having no
// line although the row right next to it says which one it is.
//
// Must run after the sort, since only then is a record next to the ones it
// shares an address with.
procedure TSymbolBuilder.DropRedundantEndMarkers;
var Index,Kept:TpvSizeInt;
    Redundant:Boolean;
begin
 Kept:=0;
 for Index:=0 to fLineCount-1 do begin
  Redundant:=false;
  if fLines[Index].LineNumber=0 then begin
   if (Index>0) and
      (fLines[Index-1].RVA=fLines[Index].RVA) and
      (fLines[Index-1].LineNumber>0) then begin
    Redundant:=true;
   end;
   if (Index<(fLineCount-1)) and
      (fLines[Index+1].RVA=fLines[Index].RVA) and
      (fLines[Index+1].LineNumber>0) then begin
    Redundant:=true;
   end;
  end;
  if not Redundant then begin
   if Kept<>Index then begin
    fLines[Kept]:=fLines[Index];
   end;
   inc(Kept);
  end;
 end;
 fLineCount:=Kept;
end;

{$ifndef PasVulkanMapSymbolsNoOverlapTrimming}
// Pulls a range which reaches into the one behind it back to where that one
// begins, and reports how many had to be pulled back.
//
// FreePascal really does emit this. The end of sequence marker which closes the
// line program of a unit can sit a handful of bytes past the first row of the
// next one, so the two ranges genuinely overlap in the information as it was
// read, by fourteen to thirty odd bytes on a build of this very tool. Which of
// the two owns those bytes is not in doubt: a unit saying where it begins is a
// stronger statement than another one saying where it stops, since the second
// is only ever one past the last byte and is what the compiler is loose about.
//
// Only a boundary which came out a little long is repaired, and three things
// have to hold for that to be what this is. The next range has to begin behind
// this one, or the two do not stand in that relation at all. It has to end at
// or behind this one, or it sits inside this one, which is the linker having
// woven two units together and not an end marker being off. And the overlap has
// to be small, since a large one is not a marker being off either whatever else
// it is. Anything which fails one of those is left exactly as it stands and
// walks straight into the check which stops the run.
//
// Trimming everything which merely overlaps would be worse than not trimming at
// all: a range enclosing another would be cut back to where that one begins, the
// tail it had behind it would silently disappear, and the check afterwards would
// then find nothing left to complain about.
function TSymbolBuilder.TrimOverlappingUnits:TpvSizeInt;
const cMaximalTrim=TpvUInt64(64);
var Index:TpvSizeInt;
    PreviousStart,PreviousEnd,CurrentStart,CurrentEnd:TpvUInt64;
begin
 result:=0;
 for Index:=1 to fUnitCount-1 do begin
  PreviousStart:=fUnits[Index-1].StartRVA;
  PreviousEnd:=PreviousStart+fUnits[Index-1].Size;
  CurrentStart:=fUnits[Index].StartRVA;
  CurrentEnd:=CurrentStart+fUnits[Index].Size;
  if (PreviousEnd>CurrentStart) and
     (CurrentStart>PreviousStart) and
     (CurrentEnd>=PreviousEnd) and
     ((PreviousEnd-CurrentStart)<=cMaximalTrim) then begin
   fUnits[Index-1].Size:=CurrentStart-PreviousStart;
   inc(result);
  end;
 end;
end;
{$endif}

procedure TSymbolBuilder.Finish;
begin
 if fUnitCount>0 then begin
  SortUnits(0,fUnitCount-1);
{$ifndef PasVulkanMapSymbolsNoOverlapTrimming}
  // Before the lines are handed out, so that a line in the disputed bytes goes
  // to the unit which keeps them.
  fTrimmedUnitCount:=TrimOverlappingUnits;
{$endif}
 end;
 if fSymbolCount>0 then begin
  SortSymbols(0,fSymbolCount-1);
 end;
 if fLineCount>0 then begin
  SortLines(0,fLineCount-1);
  DropRedundantEndMarkers;
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
    Index,MatchIndex:TpvSizeInt;
    BlobOffset:TpvInt64;
    Matches:Boolean;
    Payload:TMemoryStream;
    PackedData:TpvPointer;
    PackedSize:TpvUInt64;
begin

 FreeAndNil(fStringStream);
 FreeAndNil(fUniqueStrings);
 fStringStream:=TMemoryStream.Create;
 fUniqueStrings:=TStringList.Create;

 // A leading terminator, so that offset zero always means the empty string.
 AddString('',false);

 for Index:=0 to fUnitCount-1 do begin
  fUnits[Index].NameOffset:=AddString(fUnits[Index].Name,true);
  fUnits[Index].FileNameOffset:=AddString(PreparePath(fUnits[Index].FileName),true);
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

  Payload:=nil;
  PackedData:=nil;
  PackedSize:=0;
  try

   if fCompress then begin
    Payload:=TMemoryStream.Create;
    WritePayload(Payload);
    // Only kept when it actually came out smaller. A table which does not pack
    // is written plainly rather than larger than it was.
    if LZBRSFCompress(Payload.Memory,Payload.Size,PackedData,PackedSize) and
       (PackedSize<TpvUInt64(Payload.Size)) then begin
     Header.Flags:=Header.Flags or pvSymbolTableFlagCompressed;
    end else begin
     if assigned(PackedData) then begin
      FreeMem(PackedData);
      PackedData:=nil;
     end;
     PackedSize:=0;
    end;
   end;

   Stream.WriteBuffer(Header,SizeOf(TpvSymbolTableHeader));

   if (Header.Flags and pvSymbolTableFlagCompressed)<>0 then begin
    Stream.WriteBuffer(PackedData^,TpvSizeInt(PackedSize));
    fPackedFrom:=TpvUInt64(Payload.Size);
    fPackedTo:=PackedSize;
   end else if assigned(Payload) then begin
    Payload.Seek(0,soBeginning);
    Stream.CopyFrom(Payload,Payload.Size);
   end else begin
    WritePayload(Stream);
   end;

  finally
   if assigned(PackedData) then begin
    FreeMem(PackedData);
   end;
   FreeAndNil(Payload);
  end;

  FillChar(Footer,SizeOf(TpvSymbolTableFooter),#0);
  Move(pvSymbolTableMagic[0],Footer.Magic[0],8);
  Footer.Offset:=TpvUInt64(BlobOffset);
  Stream.WriteBuffer(Footer,SizeOf(TpvSymbolTableFooter));

  WriteLn('Appended ',Stream.Size-BlobOffset,' bytes of symbol table at offset ',BlobOffset,'.');

 finally
  FreeAndNil(Stream);
 end;

end;

// Reads the table back out of the file it was just appended to and asks it
// everything it was built to answer.
//
// This is the check which matters most and used to be the weakest. The debug
// file and the pdb are read by somebody sitting at a debugger; this table is
// the one which ships, and every line of every crash report from every machine
// comes out of it. It used to take sixty four samples of line numbers and never
// look at a single symbol, although the resolver hands the name back in the same
// call. And with no line numbers collected it took no samples at all and
// returned true, so a run could deliver a table nobody had looked at and report
// success.
//
// Everything is asked now, in both directions the resolver is used in. The
// lookups are binary searches over data which is already in memory, so this
// costs a fraction of what packing the same table costs.
function TSymbolBuilder.SelfCheck(const aFileName:String;out aResolved,aProbes:TpvSizeInt):Boolean;
var SymbolTable:TpvSymbolTable;
    Location:TpvSymbolTableLocation;
    Index,Last,Scan:TpvSizeInt;
    LowIndex,HighIndex,MiddleIndex:TpvSizeInt;
    Middle,UnitEnd:TpvUInt64;
    Found:Boolean;
begin
 aResolved:=0;
 aProbes:=0;
 result:=false;
 SymbolTable:=TpvSymbolTable.Create;
 try

  if not SymbolTable.LoadFromFile(aFileName) then begin
   exit;
  end;

  // Every unit range, at the address it begins. This is the third thing the
  // table holds and the only one which needs neither a symbol nor a line, so it
  // is also what a run which asked for neither still produces. Without it such
  // a run had nothing to probe and passed by having asked nothing.
  for Index:=0 to fUnitCount-1 do begin
   inc(aProbes);
   if SymbolTable.Resolve(fUnits[Index].StartRVA,Location) and
      (String(Location.UnitName)=fUnits[Index].Name) and
      (String(Location.FileName)=PreparePath(fUnits[Index].FileName)) then begin
    inc(aResolved);
   end;
  end;

  // Every address a routine starts at. That is the question a crash actually
  // asks, and it was not being asked here at all.
  //
  // Once per address rather than once per name: two names at one address are
  // ordinary, _start and the routine it is an alias of, main and PASCALMAIN,
  // SYSTEM.MOVE and FPC_MOVE, and a resolver can only hand one of them back.
  // Demanding a particular one would be demanding something the format does
  // not promise. What it does promise is that the answer is a routine which
  // really is there, and that is what is asked.
  Index:=0;
  while Index<fSymbolCount do begin
   Last:=Index;
   while ((Last+1)<fSymbolCount) and (fSymbols[Last+1].RVA=fSymbols[Index].RVA) do begin
    inc(Last);
   end;
   inc(aProbes);
   if SymbolTable.Resolve(fSymbols[Index].RVA,Location) then begin
    for Scan:=Index to Last do begin
     if Location.SymbolName=fSymbols[Scan].Name then begin
      inc(aResolved);
      break;
     end;
    end;
   end;
   // And once inside the routine rather than at its door. A crash address is
   // almost never the first byte of anything, and the way there is a different
   // one: an exact hit is found outright, while an address in the middle has to
   // be walked back to the nearest routine in front of it. That path was not
   // being taken by any of these probes.
   if (Last+1)<fSymbolCount then begin
    Middle:=fSymbols[Index].RVA+((fSymbols[Last+1].RVA-fSymbols[Index].RVA) shr 1);
    // Only where the middle is still inside the same unit as the routine.
    // Where the next routine belongs to the next unit, the space between them
    // does not belong to this one, and a resolver which says so is right: the
    // last routine of one unit must not be carried over into the next.
    UnitEnd:=0;
    LowIndex:=0;
    HighIndex:=fUnitCount-1;
    while LowIndex<=HighIndex do begin
     MiddleIndex:=LowIndex+((HighIndex-LowIndex) shr 1);
     if fSymbols[Index].RVA<fUnits[MiddleIndex].StartRVA then begin
      HighIndex:=MiddleIndex-1;
     end else if fSymbols[Index].RVA>=(fUnits[MiddleIndex].StartRVA+fUnits[MiddleIndex].Size) then begin
      LowIndex:=MiddleIndex+1;
     end else begin
      UnitEnd:=fUnits[MiddleIndex].StartRVA+fUnits[MiddleIndex].Size;
      break;
     end;
    end;
    if (Middle>fSymbols[Index].RVA) and (UnitEnd>0) and (Middle<UnitEnd) then begin
     inc(aProbes);
     if SymbolTable.Resolve(Middle,Location) then begin
      for Scan:=Index to Last do begin
       if Location.SymbolName=fSymbols[Scan].Name then begin
        inc(aResolved);
        break;
       end;
      end;
     end;
    end;
   end;
   Index:=Last+1;
  end;

  // And every address a line record stands at, the same way round. End of
  // sequence markers are not probed: they would match trivially, since both
  // sides are then zero, and would only water down what the count says.
  Index:=0;
  while Index<fLineCount do begin
   Last:=Index;
   while ((Last+1)<fLineCount) and (fLines[Last+1].RVA=fLines[Index].RVA) do begin
    inc(Last);
   end;
   Found:=false;
   for Scan:=Index to Last do begin
    if fLines[Scan].LineNumber>0 then begin
     Found:=true;
     break;
    end;
   end;
   if Found then begin
    inc(aProbes);
    if SymbolTable.Resolve(fLines[Index].RVA,Location) then begin
     for Scan:=Index to Last do begin
      if (fLines[Scan].LineNumber>0) and (Location.LineNumber=fLines[Scan].LineNumber) then begin
       inc(aResolved);
       break;
      end;
     end;
    end;
   end;
   Index:=Last+1;
  end;

  // Nothing asked is not the same as nothing wrong, and it must not read as a
  // pass. A table with neither a symbol nor a line in it is one which should
  // never have been written.
  result:=(aProbes>0) and (aResolved=aProbes);

 finally
  FreeAndNil(SymbolTable);
 end;
end;

end.
