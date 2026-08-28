program mapsymbols;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

// Builds a compact PasVulkan symbol table for an executable and appends it to
// that executable, so that a shipped build can symbolicate its own crash logs
// without any separate symbol file next to it.
//
// Two frontends feed the same table:
//
//   Delphi  : the .map file, which is the only place where a Delphi build keeps
//             line numbers at all.
//   FreePascal : the DWARF .debug_line section, either inside the executable or
//             inside the external debug file a .gnu_debuglink points at.
//
// The FreePascal case is worth spelling out. Lazarus links the debug information
// into a separate .dbg by default, and on a large project that file can reach
// several hundred megabytes, of which .debug_info takes almost all and
// .debug_line barely one percent. Since only the line information is needed to
// make a crash log readable, the table built here lands in a size which can
// actually be shipped, and the lnfodwrf unit no longer needs that file sitting
// next to the executable.

uses SysUtils,
     Classes,
     PasVulkan.Types,
     PasVulkan.SymbolTable,
     UnitSymbolBuilder,
     UnitImageFile,
     UnitDWARFLine,
     UnitDWARFInfo,
     UnitDWARFWriter,
     UnitELFWriter,
     UnitPEInjector,
     UnitPDBWriter,
     UnitDbgHelpCheck,
     UnitMapFile;

// How many symbols may be searched for in vain before the check stops looking
// and only keeps counting. A sound file never reaches one, since every symbol
// is found where it was put, so this only ever bites on a file which is already
// known to be wrong.
const cMaximalSearchedMismatches=64;

type
{$ifndef PasVulkanMapSymbolsSingleRangePerUnit}
     // One run of code of a compilation unit with no hole in it. A unit is not
     // necessarily one such run: the linker is free to put the code of two
     // units through one another, and a single range from the lowest to the
     // highest address of a unit would then cover the other one as well.
     TCollectorRange=record
      Low:TpvUInt64;
      // Exclusive, like everything else about ranges here.
      High:TpvUInt64;
     end;
     TCollectorRanges=array of TCollectorRange;
{$endif}

     // Counts the symbols which come back out of a written debug file and
     // whether each one is where it was put.
     TCheckCollector=class
      public
       Builder:TSymbolBuilder;
       ImageBase:TpvUInt64;
       Seen:TpvSizeInt;
       Mismatched:TpvSizeInt;
       function FindByName(const aAddress:TpvUInt64;const aName:String):Boolean;
       procedure OnSymbol(const aAddress:TpvUInt64;const aName:String);
     end;

     // The same for the line rows of a written debug file: how many came back
     // out of it and whether each one says about its address what the table
     // says about it.
     TDWARFCheckCollector=class
      public
       Builder:TSymbolBuilder;
       ImageBase:TpvUInt64;
       Rows:TpvSizeInt;
       Mismatched:TpvSizeInt;
       procedure OnRow(const aAddress:TpvUInt64;const aLineNumber:TpvUInt32);
     end;

     TCollector=class
      public
       Builder:TSymbolBuilder;
       ImageBase:TpvUInt64;
       WantSymbols:Boolean;
       // Link time window covered by executable sections. Anything outside is
       // not code of this image.
       CodeLow:TpvUInt64;
       CodeHigh:TpvUInt64;
       SymbolsAdded:TpvSizeInt;
       DiscardedRows:TpvSizeInt;
       // Range of the sequence currently being read, accumulated over the rows
       // which survived the plausibility check below. The end is exclusive, see
       // OnLineRow.
       CurrentLow:TpvUInt64;
       CurrentEnd:TpvUInt64;
       HaveCurrent:Boolean;
{$ifndef PasVulkanMapSymbolsSingleRangePerUnit}
       // The ranges of the compilation unit being read, one per run of code
       // which has no hole in it. Collected here rather than handed over as
       // they close, because the name of the file they belong to only arrives
       // at the end of the unit.
       Ranges:TCollectorRanges;
       RangeCount:TpvSizeInt;
       procedure CloseRange;
       procedure SortRanges(const aLeft,aRight:TpvSizeInt);
       procedure MergeRanges;
{$endif}
       procedure OnLineRow(const aAddress:TpvUInt64;const aLineNumber:TpvUInt32);
       procedure OnLineUnit(const aFileName:String);
       procedure OnSymbol(const aAddress:TpvUInt64;const aName:String);
     end;

// The answer which does not need the order to be anything in particular: every
// symbol of that name is tried, and one of them being at this address is enough.
function TCheckCollector.FindByName(const aAddress:TpvUInt64;const aName:String):Boolean;
var Index:TpvSizeInt;
    SymbolRecord:TSymbolBuilder.TSymbolRecord;
begin
 result:=false;
 for Index:=0 to Builder.SymbolCount-1 do begin
  SymbolRecord:=Builder.GetSymbol(Index);
  if SymbolRecord.Name=aName then begin
   result:=(ImageBase+SymbolRecord.RVA)=aAddress;
   if result then begin
    exit;
   end;
  end;
 end;
end;

procedure TCheckCollector.OnSymbol(const aAddress:TpvUInt64;const aName:String);
var Found:Boolean;
{$ifndef PasVulkanMapSymbolsLinearLookups}
    SymbolRecord:TSymbolBuilder.TSymbolRecord;
{$endif}
begin
 inc(Seen);
 // Once this many have failed, the file is wrong and the only thing still open
 // is how thoroughly. Every further search is a walk over the whole table for
 // an answer which is already known, and on a file whose names came out
 // scrambled not one of them succeeds, so the tool would appear to hang for
 // minutes before saying what it knew after the first handful.
 if Mismatched>=cMaximalSearchedMismatches then begin
  inc(Mismatched);
  exit;
 end;
 Found:=false;
{$ifndef PasVulkanMapSymbolsLinearLookups}
 // Nothing promises the order, but a reader which walks the table from front to
 // back does hand them back in it, and they were written in it. So the one at
 // this position is tried before anything is searched for, which turns a search
 // per symbol into a comparison per symbol. Without it the check is quadratic:
 // a build with a hundred thousand symbols would spend billions of iterations
 // here, each of them copying a record which carries a string, and a step which
 // took seconds would take minutes.
 if (Seen-1)<Builder.SymbolCount then begin
  SymbolRecord:=Builder.GetSymbol(Seen-1);
  Found:=(SymbolRecord.Name=aName) and ((ImageBase+SymbolRecord.RVA)=aAddress);
 end;
{$endif}
 if not Found then begin
  Found:=FindByName(aAddress,aName);
 end;
 if not Found then begin
  inc(Mismatched);
 end;
end;

procedure TDWARFCheckCollector.OnRow(const aAddress:TpvUInt64;const aLineNumber:TpvUInt32);
var LowIndex,HighIndex,MiddleIndex,Index:TpvSizeInt;
    LineRecord:TSymbolBuilder.TLineRecord;
    RVA:TpvUInt64;
    Found:Boolean;
begin

 // A line number of zero is the end of a sequence rather than a row.
 if aLineNumber=0 then begin
  exit;
 end;

 inc(Rows);

 if aAddress<ImageBase then begin
  inc(Mismatched);
  exit;
 end;
 RVA:=aAddress-ImageBase;

 // The records are sorted by address, so the ones at this one are found rather
 // than searched for. Held against by address rather than by position, since
 // what matters is that the file says the same thing about an address as the
 // table does, not that it says it in the same order.
 LowIndex:=0;
 HighIndex:=Builder.LineCount;
 while LowIndex<HighIndex do begin
  MiddleIndex:=LowIndex+((HighIndex-LowIndex) shr 1);
  if Builder.GetLine(MiddleIndex).RVA<RVA then begin
   LowIndex:=MiddleIndex+1;
  end else begin
   HighIndex:=MiddleIndex;
  end;
 end;

 // More than one record can sit at an address, so the whole run of them is
 // looked through rather than just the first.
 Found:=false;
 Index:=LowIndex;
 while Index<Builder.LineCount do begin
  LineRecord:=Builder.GetLine(Index);
  if LineRecord.RVA<>RVA then begin
   break;
  end;
  if LineRecord.LineNumber=aLineNumber then begin
   Found:=true;
   break;
  end;
  inc(Index);
 end;

 if not Found then begin
  inc(Mismatched);
 end;

end;

{$ifdef PasVulkanMapSymbolsMachineBasedAddressSize}
// Whether an image of this machine has thirty two bit addresses. Decides the
// width of everything which describes it: the ELF container, the address size
// in the DWARF, and the addresses inside both.
function Is32BitMachine(const aMachine:TpvUInt16):Boolean;
begin
 result:=(aMachine=IMAGE_FILE_MACHINE_I386) or (aMachine=IMAGE_FILE_MACHINE_ARMNT);
end;
{$endif}

// How wide an address of this image is. Decides the width of everything which
// describes it: the ELF container, the address size in the DWARF, and the
// addresses inside both.
//
// Asked of the image rather than worked out from the processor it is for. Both
// containers state it outright, in the class byte of an ELF and in the magic of
// a PE optional header, while the processor only implies it and only for the
// processors one happens to have a number for. A thirty two bit image for
// anything outside that handful, which is exactly what carrying the ELF machine
// number over was for, would fall through to the default and be described in
// sixty four bits: right processor, wrong width, every address in the file off.
function ImageAddressSize(const aImage:TImageFile):TpvUInt8;
begin
{$ifdef PasVulkanMapSymbolsMachineBasedAddressSize}
 if Is32BitMachine(aImage.Machine) then begin
  result:=4;
 end else begin
  result:=8;
 end;
{$else}
 result:=aImage.AddressSize;
{$endif}
end;

// Turns a FreePascal mangled symbol into something a reader recognizes.
//
//   SYSINIT_$$_SETUPENTRYINFORMATION              -> SYSINIT.SETUPENTRYINFORMATION
//   SYSINIT_$$_LINKIN$POINTER$POINTER$$POINTER    -> SYSINIT.LINKIN
//   PASVULKAN.TYPES_$$_TFOO_$__$$_BAR$LONGINT     -> PASVULKAN.TYPES.TFOO.BAR
//
// A name which does not carry the FreePascal unit marker is passed through, so
// that Delphi names and plain C symbols stay untouched.
function DemangleName(const aName:String):String;
var MarkerPosition,DollarPosition,Index:TpvSizeInt;
    UnitPart,RestPart:String;
begin

 result:=aName;

 MarkerPosition:=Pos('_$$_',aName);
 if MarkerPosition<2 then begin
  exit;
 end;

 UnitPart:=Copy(aName,1,MarkerPosition-1);
 RestPart:=Copy(aName,MarkerPosition+4,length(aName));

 // A method carries the class before the routine, separated by its own marker.
 MarkerPosition:=Pos('_$__$$_',RestPart);
 if MarkerPosition>0 then begin
  RestPart:=Copy(RestPart,1,MarkerPosition-1)+'.'+Copy(RestPart,MarkerPosition+7,length(RestPart));
 end;

 // Everything from the first remaining dollar sign on is the parameter and
 // result type encoding, or a generated hash suffix.
 DollarPosition:=0;
 for Index:=1 to length(RestPart) do begin
  if RestPart[Index]='$' then begin
   DollarPosition:=Index;
   break;
  end;
 end;
 if DollarPosition>0 then begin
  RestPart:=Copy(RestPart,1,DollarPosition-1);
 end;

 while (length(RestPart)>0) and (RestPart[length(RestPart)]='_') do begin
  Delete(RestPart,length(RestPart),1);
 end;

 if length(RestPart)=0 then begin
  exit;
 end;

 result:=UnitPart+'.'+RestPart;

end;

procedure TCollector.OnLineRow(const aAddress:TpvUInt64;const aLineNumber:TpvUInt32);
var RowEnd:TpvUInt64;
begin

 // FreePascal emits line information for code which the linker then discards.
 // The leftover sequences keep addresses near zero rather than a real virtual
 // address, and objdump shows exactly the same rows, so this is not a parsing
 // artefact but genuinely dead information which has to be dropped. Testing
 // against the executable sections rather than against the image base is what
 // makes this work for a position independent image too, where the base is
 // zero and a plain lower bound would let everything through.
 // An end of sequence marker is allowed to sit exactly one past the last byte
 // of code, which is where the last unit of the image ends, so it is not held
 // to the same upper bound as a row which stands for an instruction.
 if (aAddress<CodeLow) or
    ((aLineNumber>0) and (aAddress>=CodeHigh)) or
    ((aLineNumber=0) and (aAddress>CodeHigh)) then begin
  inc(DiscardedRows);
  exit;
 end;

 // Kept as an exclusive end rather than as the last address. A row which stands
 // for an instruction says that at least one byte at its address belongs to the
 // unit, while an end of sequence marker sits one past the last byte and says
 // the opposite. Treating both the same made a unit reach one byte into the one
 // behind it, which is enough to make the lookup answer with the wrong one.
 if aLineNumber>0 then begin
  RowEnd:=aAddress+1;
 end else begin
  RowEnd:=aAddress;
 end;

 if not HaveCurrent then begin
  CurrentLow:=aAddress;
  CurrentEnd:=RowEnd;
  HaveCurrent:=true;
 end else begin
  if aAddress<CurrentLow then begin
   CurrentLow:=aAddress;
  end;
  if RowEnd>CurrentEnd then begin
   CurrentEnd:=RowEnd;
  end;
 end;

 // A line number of zero is an end of sequence marker. It is not a line of its
 // own, but it is kept, because it is the only thing which says where the code
 // described by the rows before it stops. Dropping it would leave a hole inside
 // a unit, a routine without line information or padding between two sequences,
 // looking like a continuation of the line in front of it, and a reader would
 // then name that line with full confidence.
 Builder.AddLine(aAddress-ImageBase,aLineNumber);

{$ifndef PasVulkanMapSymbolsSingleRangePerUnit}
 // The marker also ends the run of code being measured, so that what the unit
 // covers is the runs themselves and not the span from the first to the last,
 // which would take in whatever else lies between them.
 if aLineNumber=0 then begin
  CloseRange;
 end;
{$endif}

end;

{$ifndef PasVulkanMapSymbolsSingleRangePerUnit}
// Ends the run of code being measured and keeps it.
procedure TCollector.CloseRange;
begin
 if HaveCurrent and (CurrentEnd>CurrentLow) then begin
  if RangeCount>=length(Ranges) then begin
   SetLength(Ranges,(RangeCount+1)*2);
  end;
  Ranges[RangeCount].Low:=CurrentLow;
  Ranges[RangeCount].High:=CurrentEnd;
  inc(RangeCount);
 end;
 HaveCurrent:=false;
 CurrentLow:=0;
 CurrentEnd:=0;
end;

procedure TCollector.SortRanges(const aLeft,aRight:TpvSizeInt);
var Left,Right:TpvSizeInt;
    Pivot:TpvUInt64;
    Temporary:TCollectorRange;
begin
 if aLeft>=aRight then begin
  exit;
 end;
 Left:=aLeft;
 Right:=aRight;
 Pivot:=Ranges[(aLeft+aRight) shr 1].Low;
 while Left<=Right do begin
  while Ranges[Left].Low<Pivot do begin
   inc(Left);
  end;
  while Ranges[Right].Low>Pivot do begin
   dec(Right);
  end;
  if Left<=Right then begin
   Temporary:=Ranges[Left];
   Ranges[Left]:=Ranges[Right];
   Ranges[Right]:=Temporary;
   inc(Left);
   dec(Right);
  end;
 end;
 SortRanges(aLeft,Right);
 SortRanges(Left,aRight);
end;

// Puts the runs in address order and joins the ones which are not separated by
// anything.
//
// Two things make this necessary. The sequences of a compilation unit do not
// arrive in address order, since the line program describes them in source
// order while the linker placed them as it saw fit, so a later run can begin
// below an earlier one and even inside it. And a run which begins exactly where
// the one before it ended is not a run of its own, there being no hole between
// them.
//
// What is left afterwards is one entry per actual hole, which is both the
// smallest correct answer and one the reader can binary search, since it
// assumes the ranges neither overlap nor come out of order.
procedure TCollector.MergeRanges;
const // A gap no larger than this is padding rather than a hole. Routines are
      // aligned, so the few bytes between the end of one and the start of the
      // next belong to neither, and nothing else can be placed there either,
      // since whatever came next would have to be aligned as well.
      //
      // Measured on a build of this project, every gap between two runs of one
      // unit was sixteen bytes or less, so without this every function boundary
      // would become a range of its own: three and a half thousand of them
      // instead of eight, for no gain.
      //
      // Turning the tolerance off with PasVulkanMapSymbolsNoPaddingTolerance
      // gives exactly that, one range per run, which is how the split case is
      // reproduced on a build whose linker did not actually split anything.
{$ifdef PasVulkanMapSymbolsNoPaddingTolerance}
      cMaximalPadding=0;
{$else}
      cMaximalPadding=16;
{$endif}
var Index,Kept:TpvSizeInt;
begin

 if RangeCount<2 then begin
  exit;
 end;

 SortRanges(0,RangeCount-1);

 Kept:=0;
 for Index:=1 to RangeCount-1 do begin
  if Ranges[Index].Low<=(Ranges[Kept].High+cMaximalPadding) then begin
   // Touching, overlapping, or separated by nothing but padding, so the two
   // describe one run between them.
   if Ranges[Index].High>Ranges[Kept].High then begin
    Ranges[Kept].High:=Ranges[Index].High;
   end;
  end else begin
   inc(Kept);
   if Kept<>Index then begin
    Ranges[Kept]:=Ranges[Index];
   end;
  end;
 end;

 RangeCount:=Kept+1;

end;
{$endif}

procedure TCollector.OnLineUnit(const aFileName:String);
var Name:String;
{$ifndef PasVulkanMapSymbolsSingleRangePerUnit}
    Index:TpvSizeInt;
{$endif}
begin

 // DWARF names the source file, not the Pascal unit, so the unit name is taken
 // from the file name, which is what a reader expects to see anyway.
 Name:=ChangeFileExt(ExtractFileName(aFileName),'');

{$ifdef PasVulkanMapSymbolsSingleRangePerUnit}

 // One range per compilation unit, from its lowest address to its highest,
 // which is how this was first written. Simpler, and right for a build whose
 // units the linker kept in one piece each, but where it did not, such a range
 // reaches over the code of another unit and answers for it.
 if HaveCurrent and (CurrentEnd>CurrentLow) then begin
  Builder.AddUnit(Name,aFileName,CurrentLow-ImageBase,CurrentEnd-CurrentLow);
 end;
 HaveCurrent:=false;
 CurrentLow:=0;
 CurrentEnd:=0;

{$else}

 // One entry per run of code instead. They share the name and the file, which
 // costs nothing, since the string table keeps one copy of each.
 CloseRange;
 MergeRanges;
 for Index:=0 to RangeCount-1 do begin
  Builder.AddUnit(Name,aFileName,Ranges[Index].Low-ImageBase,Ranges[Index].High-Ranges[Index].Low);
 end;
 RangeCount:=0;

{$endif}

end;

procedure TCollector.OnSymbol(const aAddress:TpvUInt64;const aName:String);
begin
 if not WantSymbols then begin
  exit;
 end;
 if (aAddress<CodeLow) or (aAddress>=CodeHigh) then begin
  exit;
 end;
 // The unit range markers are not routines and would only be noise in a stack
 // trace, since they sit exactly on the boundary between two units.
 if (Pos('DEBUGSTART_$',aName)=1) or (Pos('DEBUGEND_$',aName)=1) then begin
  exit;
 end;
 Builder.AddSymbol(aAddress-ImageBase,DemangleName(aName));
 inc(SymbolsAdded);
end;

{$ifndef PasVulkanMapSymbolsLinearLookups}
// The unit range this address falls in, or minus one when it falls into none of
// them. The ranges are sorted by start and are checked for overlap before
// anything is written, which is what makes a binary search over them the right
// answer rather than merely a fast one. It is also how the reader in the
// runtime looks a unit up.
function FindUnitRange(const aBuilder:TSymbolBuilder;const aRVA:TpvUInt64):TpvSizeInt;
var LowIndex,HighIndex,MiddleIndex:TpvSizeInt;
    UnitRecord:TSymbolBuilder.TUnitRecord;
begin
 result:=-1;
 LowIndex:=0;
 HighIndex:=aBuilder.UnitCount-1;
 while LowIndex<=HighIndex do begin
  MiddleIndex:=LowIndex+((HighIndex-LowIndex) shr 1);
  UnitRecord:=aBuilder.GetUnit(MiddleIndex);
  if aRVA<UnitRecord.StartRVA then begin
   HighIndex:=MiddleIndex-1;
  end else if aRVA>=(UnitRecord.StartRVA+UnitRecord.Size) then begin
   LowIndex:=MiddleIndex+1;
  end else begin
   result:=MiddleIndex;
   exit;
  end;
 end;
end;
{$endif}

// Collects the rows of one line program, to see where the program a compile
// unit points at actually lies.
type TDWARFProgramCollector=class
      public
       Low:TpvUInt64;
       High:TpvUInt64;
       Rows:TpvSizeInt;
       procedure OnRow(const aAddress:TpvUInt64;const aLineNumber:TpvUInt32);
     end;

procedure TDWARFProgramCollector.OnRow(const aAddress:TpvUInt64;const aLineNumber:TpvUInt32);
begin
 // The marker which closes a sequence sits one past the last byte, so it says
 // nothing about where the code is and is left out of the span.
 if aLineNumber=0 then begin
  exit;
 end;
 if Rows=0 then begin
  Low:=aAddress;
  High:=aAddress;
 end else begin
  if aAddress<Low then begin
   Low:=aAddress;
  end;
  if aAddress>High then begin
   High:=aAddress;
  end;
 end;
 inc(Rows);
end;

// Reads back the compilation unit descriptions and holds them against the unit
// ranges and against the line programs they point at.
//
// The line rows being right is not enough on its own. Nothing reaches them by
// scanning the line section: a consumer starts at a compile unit, takes the
// source file and the address range from it, and follows DW_AT_stmt_list to the
// rows. So a file whose every row is correct still resolves to the wrong source
// file, or to nothing at all, when a compile unit names the wrong range or
// points at another unit's program. Neither the symbol table nor the line rows
// say anything about that, which is why it is looked at separately.
procedure CheckCompileUnits(const aBuilder:TSymbolBuilder;const aCheck:TImageFile;const aLineSection:TMemoryStream);
var InfoSection,AbbrevSection:TMemoryStream;
    Reader:TDWARFInfoReader;
    ProgramCollector:TDWARFProgramCollector;
    ProgramReader:TDWARFLineReader;
    InfoUnit:TDWARFInfoUnit;
    UnitRecord:TSymbolBuilder.TUnitRecord;
    Index,RangeIndex,Complaints,WithLines:TpvSizeInt;
    LowIndex,HighIndex,MiddleIndex:TpvSizeInt;
    Offset,ProgramLength:TpvUInt64;
    ImageBase:TpvUInt64;
    Used:array of Boolean;
    ProgramOffsets:array of TpvUInt64;
    ProgramLengths:array of TpvUInt64;
    ProgramCount,Found:TpvSizeInt;
    Bytes:PpvUInt8Array;
begin

 InfoSection:=nil;
 AbbrevSection:=nil;
 Reader:=nil;
 ProgramCollector:=nil;
 ProgramReader:=nil;
 Used:=nil;
 ProgramOffsets:=nil;
 ProgramLengths:=nil;

 try

  InfoSection:=aCheck.ReadSection('.debug_info');
  AbbrevSection:=aCheck.ReadSection('.debug_abbrev');
  if (not assigned(InfoSection)) or (not assigned(AbbrevSection)) then begin
   WriteLn('Debug file check FAILED: the file which was just written has no readable compilation units.');
   ExitCode:=1;
   exit;
  end;

  Reader:=TDWARFInfoReader.Create(InfoSection.Memory,InfoSection.Size,AbbrevSection.Memory,AbbrevSection.Size);
  Reader.BigEndian:=aCheck.BigEndian;
  if not Reader.Parse then begin
   WriteLn('Debug file check FAILED: the compilation units cannot be read back, ',Reader.Message,'.');
   ExitCode:=1;
   exit;
  end;

  // Where the line programs lie. Walked here rather than taken from the writer,
  // since the point of this is whether a compile unit points at one of them.
  ProgramCount:=0;
  Bytes:=PpvUInt8Array(aLineSection.Memory);
  Offset:=0;
  while (Offset+4)<=TpvUInt64(aLineSection.Size) do begin
   if aCheck.BigEndian then begin
    ProgramLength:=(TpvUInt64(Bytes^[Offset]) shl 24) or (TpvUInt64(Bytes^[Offset+1]) shl 16) or
                   (TpvUInt64(Bytes^[Offset+2]) shl 8) or TpvUInt64(Bytes^[Offset+3]);
   end else begin
    ProgramLength:=TpvUInt64(Bytes^[Offset]) or (TpvUInt64(Bytes^[Offset+1]) shl 8) or
                   (TpvUInt64(Bytes^[Offset+2]) shl 16) or (TpvUInt64(Bytes^[Offset+3]) shl 24);
   end;
   if (ProgramLength=0) or ((Offset+4+ProgramLength)>TpvUInt64(aLineSection.Size)) then begin
    break;
   end;
   if ProgramCount>=length(ProgramOffsets) then begin
    SetLength(ProgramOffsets,(ProgramCount+1)*2);
    SetLength(ProgramLengths,(ProgramCount+1)*2);
   end;
   ProgramOffsets[ProgramCount]:=Offset;
   ProgramLengths[ProgramCount]:=4+ProgramLength;
   inc(ProgramCount);
   inc(Offset,4+ProgramLength);
  end;

  SetLength(Used,ProgramCount);
  for Index:=0 to ProgramCount-1 do begin
   Used[Index]:=false;
  end;

  ImageBase:=aBuilder.ImageBase;
  Complaints:=0;

  for Index:=0 to Reader.UnitCount-1 do begin

   InfoUnit:=Reader.GetUnit(Index);

   if not InfoUnit.HaveStatementList then begin
    WriteLn('  compilation unit ',InfoUnit.Name,' does not point at a line program at all.');
    inc(Complaints);
    continue;
   end;

   // The range it claims has to be one which was collected. Looked up by
   // address rather than by position, so that what is checked is the range and
   // not the order the units happen to come out in. The ranges are sorted by
   // start, so the search is the same binary one as everywhere else here.
   RangeIndex:=-1;
   if InfoUnit.LowPC>=ImageBase then begin
    LowIndex:=0;
    HighIndex:=aBuilder.UnitCount-1;
    while LowIndex<=HighIndex do begin
     MiddleIndex:=LowIndex+((HighIndex-LowIndex) shr 1);
     UnitRecord:=aBuilder.GetUnit(MiddleIndex);
     if (ImageBase+UnitRecord.StartRVA)<InfoUnit.LowPC then begin
      LowIndex:=MiddleIndex+1;
     end else if (ImageBase+UnitRecord.StartRVA)>InfoUnit.LowPC then begin
      HighIndex:=MiddleIndex-1;
     end else begin
      if (ImageBase+UnitRecord.StartRVA+UnitRecord.Size)=InfoUnit.HighPC then begin
       RangeIndex:=MiddleIndex;
      end;
      break;
     end;
    end;
   end;
   if RangeIndex<0 then begin
    WriteLn('  compilation unit ',InfoUnit.Name,' claims $',IntToHex(InfoUnit.LowPC,8),'..$',IntToHex(InfoUnit.HighPC,8),
            ', which is not a range which was collected.');
    inc(Complaints);
    continue;
   end;

   Found:=-1;
   for RangeIndex:=0 to ProgramCount-1 do begin
    if ProgramOffsets[RangeIndex]=InfoUnit.StatementListOffset then begin
     Found:=RangeIndex;
     break;
    end;
   end;
   if Found<0 then begin
    WriteLn('  compilation unit ',InfoUnit.Name,' points at offset ',InfoUnit.StatementListOffset,
            ', where no line program begins.');
    inc(Complaints);
    continue;
   end;
   if Used[Found] then begin
    WriteLn('  compilation unit ',InfoUnit.Name,' points at a line program which another one already points at.');
    inc(Complaints);
    continue;
   end;
   Used[Found]:=true;

   // And the rows of that program have to lie inside the range this unit
   // claims. That is what catches a stmt_list which is a valid offset but the
   // wrong one, which nothing above would notice.
   FreeAndNil(ProgramCollector);
   FreeAndNil(ProgramReader);
   ProgramCollector:=TDWARFProgramCollector.Create;
   ProgramReader:=TDWARFLineReader.Create(@Bytes^[ProgramOffsets[Found]],TpvSizeInt(ProgramLengths[Found]));
   ProgramReader.BigEndian:=aCheck.BigEndian;
   ProgramReader.Parse(ProgramCollector.OnRow,nil);
   if ProgramCollector.Rows=0 then begin
    WriteLn('  the line program of ',InfoUnit.Name,' has no rows.');
    inc(Complaints);
   end else if (ProgramCollector.Low<InfoUnit.LowPC) or (ProgramCollector.High>=InfoUnit.HighPC) then begin
    WriteLn('  the line program of ',InfoUnit.Name,' covers $',IntToHex(ProgramCollector.Low,8),'..$',IntToHex(ProgramCollector.High,8),
            ', which is outside the $',IntToHex(InfoUnit.LowPC,8),'..$',IntToHex(InfoUnit.HighPC,8),' it claims.');
    inc(Complaints);
   end;

  end;

  WithLines:=0;
  for Index:=0 to ProgramCount-1 do begin
   if Used[Index] then begin
    inc(WithLines);
   end;
  end;
  if WithLines<>ProgramCount then begin
   WriteLn('  ',ProgramCount-WithLines,' line programs are in the file which no compilation unit points at.');
   inc(Complaints);
  end;

  if Complaints>0 then begin
   WriteLn('Debug file check FAILED: ',Complaints,' complaints about the ',Reader.UnitCount,' compilation units.');
   ExitCode:=1;
  end else begin
   WriteLn('Debug file check: ',Reader.UnitCount,' compilation units name the ranges they were built from and point at their own line programs.');
  end;

 finally
  FreeAndNil(ProgramReader);
  FreeAndNil(ProgramCollector);
  FreeAndNil(Reader);
  FreeAndNil(AbbrevSection);
  FreeAndNil(InfoSection);
  Used:=nil;
  ProgramOffsets:=nil;
  ProgramLengths:=nil;
 end;

end;

// Reads the debug file which was just written back through the reader of this
// tool and holds what comes out against what went in.
//
// The symbols are one half of it. They carry an address and a name each and go
// through the whole of the container: the header says where the section table
// is, the section table says where the symbols and their names are, and a
// symbol entry has its fields in an order which differs between the two widths.
// Any of that written wrongly and the names or the addresses do not come back.
//
// The line programs are the other half, and the half a symbolizer actually
// reads. They go into a section as a block this tool never opens again, which
// is exactly where the address width in the header of a compilation unit, the
// length of the set address opcode and the byte order of every number in them
// sit. All of those can be wrong while every symbol still comes back perfectly,
// so the line programs are read back too, with the reader this same tool uses
// on somebody else's DWARF.
procedure CheckDebugFile(const aBuilder:TSymbolBuilder;const aDWARFWriter:TDWARFWriter;const aImage:TImageFile;const aFileName:String);
var Check:TImageFile;
    Collector:TCheckCollector;
    LineCollector:TDWARFCheckCollector;
    LineSection:TMemoryStream;
    LineReader:TDWARFLineReader;
    LineRecord:TSymbolBuilder.TLineRecord;
    Expected,Index:TpvSizeInt;
begin

 Collector:=TCheckCollector.Create;
 LineCollector:=TDWARFCheckCollector.Create;
 Check:=TImageFile.Create;
 LineSection:=nil;
 LineReader:=nil;
 try

  if not Check.Open(aFileName) then begin
   WriteLn('Debug file check failed: the file which was just written cannot be read back.');
   ExitCode:=1;
   exit;
  end;

  // The width and the processor as the file itself states them. A file written
  // in the wrong width is read back in the wrong width by this same reader and
  // agrees with itself all the way through, so nothing but the image it is
  // supposed to describe can catch it. And the COFF number alone says nothing
  // about a processor which has none: it is unknown on both sides and compares
  // equal, which is why the ELF one is held against it as well wherever the
  // image had one.
  if (Check.Machine<>aImage.Machine) or
     (Check.BigEndian<>aImage.BigEndian) or
     (Check.AddressSize<>ImageAddressSize(aImage)) or
     ((aImage.ELFMachine<>0) and
      ((Check.ELFMachine<>aImage.ELFMachine) or (Check.ELFFlags<>aImage.ELFFlags))) then begin
   WriteLn('Debug file check failed: it describes a different machine than the image does.');
   ExitCode:=1;
   exit;
  end;

  Collector.Builder:=aBuilder;
  Collector.ImageBase:=aBuilder.ImageBase;
  Check.EnumerateSymbols(Collector.OnSymbol);

  // Every symbol which went in has to come back, at the address it went in at.
  Expected:=aBuilder.SymbolCount;
  if (Collector.Seen<>Expected) or (Collector.Mismatched>0) then begin
   WriteLn('Debug file check FAILED: ',Collector.Seen,' of ',Expected,' symbols came back, ',Collector.Mismatched,' at the wrong address.');
   ExitCode:=1;
  end else begin
   WriteLn('Debug file check: ',Collector.Seen,' symbols came back at the addresses they went in at.');
  end;

  if assigned(aDWARFWriter) then begin

   LineSection:=Check.ReadSection('.debug_line');
   if not assigned(LineSection) then begin
    WriteLn('Debug file check FAILED: the file which was just written has no readable line programs.');
    ExitCode:=1;
    exit;
   end;

   LineReader:=TDWARFLineReader.Create(LineSection.Memory,LineSection.Size);
   LineReader.BigEndian:=Check.BigEndian;
   LineCollector.Builder:=aBuilder;
   LineCollector.ImageBase:=aBuilder.ImageBase;
   LineReader.Parse(LineCollector.OnRow,nil);

   // Counted out of the collected records rather than taken from the writer.
   // A record which closes a sequence is not a row and a record which fell
   // outside every unit range never reaches a line program, so the number is
   // not simply the record count, but it is arrived at without asking the side
   // being checked. The writer keeps its own count as well, and the two are
   // held against each other below: taking the writer's number as the target
   // would have let a writer which quietly dropped a whole unit agree with
   // itself, since the reader would then find exactly the fewer rows it wrote.
   Expected:=0;
   for Index:=0 to aBuilder.LineCount-1 do begin
    LineRecord:=aBuilder.GetLine(Index);
    if (LineRecord.LineNumber>0) and (LineRecord.UnitIndex<TpvUInt32(aBuilder.UnitCount)) then begin
     inc(Expected);
    end;
   end;

   if (LineCollector.Rows<>Expected) or (LineCollector.Mismatched>0) then begin
    WriteLn('Debug file check FAILED: ',LineCollector.Rows,' of ',Expected,' line rows came back, ',LineCollector.Mismatched,' with the wrong line number.');
    ExitCode:=1;
   end else begin
    WriteLn('Debug file check: ',LineCollector.Rows,' line rows came back with the line numbers they went in with.');
   end;

   if aDWARFWriter.LineRowCount<>Expected then begin
    WriteLn('Debug file check FAILED: the writer put down ',aDWARFWriter.LineRowCount,' line rows where the collected records come to ',Expected,'.');
    ExitCode:=1;
   end;

   CheckCompileUnits(aBuilder,Check,LineSection);

  end;

 finally
  FreeAndNil(LineReader);
  FreeAndNil(LineSection);
  FreeAndNil(Check);
  FreeAndNil(LineCollector);
  FreeAndNil(Collector);
 end;

end;

// Emits everything which was collected a second time, as DWARF inside a
// standalone ELF file. Nothing about the original executable changes.
procedure WriteDebugFile(const aBuilder:TSymbolBuilder;const aDWARFWriter:TDWARFWriter;const aImage:TImageFile;const aFileName:String);
var ELFWriter:TELFWriter;
    Index:TpvSizeInt;
    SymbolRecord,NextSymbol:TSymbolBuilder.TSymbolRecord;
    UnitRecord:TSymbolBuilder.TUnitRecord;
    ImageBase,Low,High,SymbolSize,SymbolEnd:TpvUInt64;
    RangeIndex:TpvSizeInt;
    Have:Boolean;
begin

 ELFWriter:=TELFWriter.Create;
 try

  // An ELF says which processor it is for in its own numbering, so that number
  // is carried straight over. Going through the COFF one and back would work
  // for the handful which have a number on both sides and turn everything else
  // into whatever the fallback is, which for a PowerPC image would be a file
  // claiming to be for x86-64.
  //
  // A PE has no such number, so there the COFF one is translated.
  if aImage.ELFMachine<>0 then begin
   ELFWriter.Machine:=aImage.ELFMachine;
  end else begin
   case aImage.Machine of
    IMAGE_FILE_MACHINE_I386:begin
     ELFWriter.Machine:=EM_386;
    end;
    IMAGE_FILE_MACHINE_ARMNT:begin
     ELFWriter.Machine:=EM_ARM;
    end;
    IMAGE_FILE_MACHINE_ARM64:begin
     ELFWriter.Machine:=EM_AARCH64;
    end;
    else begin
     ELFWriter.Machine:=EM_X86_64;
    end;
   end;
  end;
  ELFWriter.Bits:=ImageAddressSize(aImage)*8;
  ELFWriter.BigEndian:=aImage.BigEndian;
  // Nothing on the desktop targets uses these, but on arm and on mips they name
  // the abi and the instruction set, and a debug file which claims something
  // else about the image than the image does is one a reader can refuse.
  ELFWriter.Flags:=aImage.ELFFlags;

  ELFWriter.AddDebugSection('.debug_info',aDWARFWriter.DebugInfo);
  ELFWriter.AddDebugSection('.debug_abbrev',aDWARFWriter.DebugAbbrev);
  ELFWriter.AddDebugSection('.debug_line',aDWARFWriter.DebugLine);

  ImageBase:=aBuilder.ImageBase;

  // The overall code range, taken from the units, so that the file states which
  // addresses it is about.
  Have:=false;
  Low:=0;
  High:=0;
  for Index:=0 to aBuilder.UnitCount-1 do begin
   UnitRecord:=aBuilder.GetUnit(Index);
   if not Have then begin
    Low:=ImageBase+UnitRecord.StartRVA;
    High:=Low+UnitRecord.Size;
    Have:=true;
   end else begin
    if (ImageBase+UnitRecord.StartRVA)<Low then begin
     Low:=ImageBase+UnitRecord.StartRVA;
    end;
    if (ImageBase+UnitRecord.StartRVA+UnitRecord.Size)>High then begin
     High:=ImageBase+UnitRecord.StartRVA+UnitRecord.Size;
    end;
   end;
  end;
  if Have then begin
   ELFWriter.SetTextRange(Low,High-Low);
  end;

  for Index:=0 to aBuilder.SymbolCount-1 do begin

   SymbolRecord:=aBuilder.GetSymbol(Index);

   // How far a routine reaches is not stated anywhere, so the distance to the
   // next one is the best available answer. It is only an answer while the two
   // are in the same run of code: across a gap the next symbol can be a long
   // way off, and a size which reaches over that gap would claim ground which
   // belongs to something else. Where they are not, nothing is claimed, which
   // is what a size of zero means.
   SymbolSize:=0;
   if (Index+1)<aBuilder.SymbolCount then begin
    NextSymbol:=aBuilder.GetSymbol(Index+1);
    if NextSymbol.RVA>SymbolRecord.RVA then begin
     SymbolEnd:=NextSymbol.RVA;
{$ifdef PasVulkanMapSymbolsLinearLookups}
     for RangeIndex:=0 to aBuilder.UnitCount-1 do begin
      UnitRecord:=aBuilder.GetUnit(RangeIndex);
      if (SymbolRecord.RVA>=UnitRecord.StartRVA) and
         (SymbolRecord.RVA<(UnitRecord.StartRVA+UnitRecord.Size)) then begin
       if SymbolEnd>(UnitRecord.StartRVA+UnitRecord.Size) then begin
        SymbolEnd:=UnitRecord.StartRVA+UnitRecord.Size;
       end;
       break;
      end;
     end;
{$else}
     // The ranges are sorted by start and do not overlap, which is checked
     // before anything is written, so the one this symbol is in can be found
     // rather than looked for. Looking for it walks all of them once per
     // symbol, and this runs on every build which writes a debug file: with a
     // hundred thousand symbols and a few thousand ranges that is hundreds of
     // millions of iterations, each copying a record which carries two strings.
     RangeIndex:=FindUnitRange(aBuilder,SymbolRecord.RVA);
     if RangeIndex>=0 then begin
      UnitRecord:=aBuilder.GetUnit(RangeIndex);
      if SymbolEnd>(UnitRecord.StartRVA+UnitRecord.Size) then begin
       SymbolEnd:=UnitRecord.StartRVA+UnitRecord.Size;
      end;
     end;
{$endif}
     if SymbolEnd>SymbolRecord.RVA then begin
      SymbolSize:=SymbolEnd-SymbolRecord.RVA;
     end;
    end;
   end;

   ELFWriter.AddSymbol(SymbolRecord.Name,ImageBase+SymbolRecord.RVA,SymbolSize);

  end;

  ELFWriter.SaveToFile(aFileName);

  // And read it straight back with the reader of this same tool.
  //
  // Of the three things written here, this was the only one nobody ever looked
  // at again: the appended table has its self check and the pdb is held against
  // dbghelp, while the debug file was written and never opened. That was
  // defensible while there was one layout. There are now four, thirty two and
  // sixty four bits times the two byte orders, and the ones which are rarely
  // built are exactly the ones which are quietly broken half a year later. A
  // swapped pair of fields in a symbol entry gives a file of the right length
  // full of nonsense, and this is what notices.
  CheckDebugFile(aBuilder,aDWARFWriter,aImage,aFileName);

  WriteLn('Wrote ',aFileName,' with ',aDWARFWriter.DebugLine.Size,' bytes of line programs and ',
          aDWARFWriter.DebugInfo.Size,' bytes of compile units.');

 finally
  FreeAndNil(ELFWriter);
 end;

end;

// Puts the same DWARF sections into the executable itself, so that no separate
// file is needed at all. Has to happen before the symbol table is appended,
// since that has to stay at the very end of the file.
function InjectDebugSections(const aDWARFWriter:TDWARFWriter;const aPDBWriter:TPDBWriter;const aPDBFileName,aFileName:String):Boolean;
var Injector:TPEInjector;
begin
 Injector:=TPEInjector.Create;
 try
  if assigned(aDWARFWriter) then begin
   Injector.AddSection('.debug_info',aDWARFWriter.DebugInfo);
   Injector.AddSection('.debug_abbrev',aDWARFWriter.DebugAbbrev);
   Injector.AddSection('.debug_line',aDWARFWriter.DebugLine);
  end;
  if assigned(aPDBWriter) then begin
   Injector.AddCodeViewDirectory(aPDBWriter.GUIDPointer,aPDBWriter.Age,aPDBFileName,aPDBWriter.Signature);
  end;
  result:=Injector.InjectInto(aFileName);
  WriteLn(Injector.Message);
 finally
  FreeAndNil(Injector);
 end;
end;

// Emits the same information a third time, as a PDB.
function WritePDBFile(const aBuilder:TSymbolBuilder;const aImage:TImageFile;const aFileName:String):TPDBWriter;
var PDBWriter:TPDBWriter;
    Index:TpvSizeInt;
    Digest:TSymbolBuilder.TDigest;
begin
 PDBWriter:=TPDBWriter.Create(aBuilder);
 result:=PDBWriter;
 try
  for Index:=0 to length(aImage.Sections)-1 do begin
   PDBWriter.AddSection(aImage.Sections[Index].Name,
                        TpvUInt32(aImage.Sections[Index].VirtualAddress-aImage.ImageBase),
                        TpvUInt32(aImage.Sections[Index].VirtualSize),
                        TpvUInt32(aImage.Sections[Index].RawSize),
                        aImage.Sections[Index].Characteristics);
  end;
  // The identity is a digest of what was collected rather than the clock, so
  // that building the same input twice gives the same identity while two
  // builds which differ anywhere do not.
  PDBWriter.Machine:=aImage.Machine;
  aBuilder.ComputeDigest(Digest);
  PDBWriter.SetIdentity(Digest,1);
  PDBWriter.SaveToFile(aFileName);
  WriteLn('Wrote ',aFileName,'.');
 except
  FreeAndNil(result);
  raise;
 end;
end;

var ExecutableFileName,MapFileName,DebugFileName,Parameter:String;
    ParameterIndex:TpvSizeInt;
    WantSymbols,WantLines,ForceMap,ForceDWARF,StripPaths,Compress:Boolean;
    DebugOutputFileName:String;
    InjectIntoExecutable:Boolean;
    PDBOutputFileName:String;
    DWARFWriter:TDWARFWriter;
    PDBWriter:TPDBWriter;
    Image,DebugImage,SymbolImage:TImageFile;
    Section:TImageSection;
    LineData,LineStringData,StringData:TMemoryStream;
    OverlapCount,UnitIndex:TpvSizeInt;
    PreviousUnit,CurrentUnit:TSymbolBuilder.TUnitRecord;
    LineStringMemory,StringMemory:TpvPointer;
    LineStringSize,StringSize:TpvSizeInt;
    DWARFReader:TDWARFLineReader;
    MapReader:TMapFileReader;
    Builder:TSymbolBuilder;
    Collector:TCollector;
    Resolved,Probes:TpvSizeInt;
    DbgHelpResolved,DbgHelpProbes:TpvSizeInt;
    DbgHelpAvailable:Boolean;
    UsedDWARF:Boolean;

begin

 ExecutableFileName:='';
 MapFileName:='';
 WantSymbols:=true;
 StripPaths:=false;
 Compress:=false;
 WantLines:=true;
 ForceMap:=false;
 ForceDWARF:=false;
 DebugOutputFileName:='';
 InjectIntoExecutable:=false;
 PDBOutputFileName:='';

 ParameterIndex:=1;
 while ParameterIndex<=ParamCount do begin
  Parameter:=ParamStr(ParameterIndex);
  inc(ParameterIndex);
  if Parameter='--no-symbols' then begin
   WantSymbols:=false;
  end else if Parameter='--basenames' then begin
   StripPaths:=true;
  end else if Parameter='--compress' then begin
   Compress:=true;
  end else if Parameter='--no-lines' then begin
   WantLines:=false;
  end else if Parameter='--map' then begin
   ForceMap:=true;
  end else if Parameter='--dwarf' then begin
   ForceDWARF:=true;
  end else if (Parameter='--pdb') and (ParameterIndex<=ParamCount) then begin
   PDBOutputFileName:=ParamStr(ParameterIndex);
   inc(ParameterIndex);
  end else if Parameter='--pe-debug' then begin
   InjectIntoExecutable:=true;
  end else if (Parameter='--gdb') and (ParameterIndex<=ParamCount) then begin
   DebugOutputFileName:=ParamStr(ParameterIndex);
   inc(ParameterIndex);
  end else if length(ExecutableFileName)=0 then begin
   ExecutableFileName:=Parameter;
  end else if length(MapFileName)=0 then begin
   MapFileName:=Parameter;
  end;
 end;

 if length(ExecutableFileName)=0 then begin
  WriteLn('mapsymbols - appends a PasVulkan symbol table to an executable, so that it');
  WriteLn('             can symbolicate its own crash logs.');
  WriteLn;
  WriteLn('  mapsymbols <executable> [mapfile] [options]');
  WriteLn;
  WriteLn('It reads a Delphi .map file, or the DWARF line information of a FreePascal');
  WriteLn('build, whichever the executable actually offers. DWARF is also picked up out');
  WriteLn('of the external debug file which a .gnu_debuglink section points at.');
  WriteLn;
  WriteLn('  --map          force the Delphi map frontend');
  WriteLn('  --dwarf        force the DWARF frontend');
  WriteLn('  --no-symbols   omit routine names, which is by far the larger part');
  WriteLn('  --no-lines     omit line numbers');
  WriteLn('  --basenames    keep only the file name of a source, not the directory it');
  WriteLn('                 was built in, so that a shipped binary does not carry the');
  WriteLn('                 build tree of whoever built it');
  WriteLn('  --compress     pack the appended table, which measured at about a third of');
  WriteLn('                 its size. Needs the reading side built with the define');
  WriteLn('                 PasVulkanSymbolTableCompression, since unpacking asks for');
  WriteLn('                 memory at the worst moment for it. A build without that');
  WriteLn('                 define turns a packed table down rather than misreading it');
  WriteLn('  --gdb <file>   additionally write the same information as a standalone');
  WriteLn('                 ELF debug file, which addr2line, gdb and everything else');
  WriteLn('                 built on DWARF can read, also for a Delphi build');
  WriteLn('  --pdb <file>   additionally write a PDB, for the Microsoft debuggers');
  WriteLn('  --pe-debug     put those DWARF sections into the executable itself, so');
  WriteLn('                 that no separate file is needed. Needs room in the section');
  WriteLn('                 header table and says so when there is none');
  WriteLn;
  WriteLn('Running it again on the same executable replaces the previous table.');
  ExitCode:=1;
  exit;
 end;

 if not FileExists(ExecutableFileName) then begin
  WriteLn('Executable not found: ',ExecutableFileName);
  ExitCode:=1;
  exit;
 end;

 Image:=nil;
 DebugImage:=nil;
 Builder:=nil;
 Collector:=nil;
 LineData:=nil;
 LineStringData:=nil;
 StringData:=nil;
 DWARFReader:=nil;
 MapReader:=nil;
 try

  Image:=TImageFile.Create;
  if not Image.Open(ExecutableFileName) then begin
   WriteLn('Could not read ',ExecutableFileName,', is that a PE executable?');
   ExitCode:=1;
   exit;
  end;

  WriteLn('Image base $',IntToHex(Image.ImageBase,16));

  Builder:=TSymbolBuilder.Create;
  Builder.StripPaths:=StripPaths;
  Builder.Compress:=Compress;
  Collector:=TCollector.Create;
  Collector.Builder:=Builder;
  Collector.ImageBase:=Image.ImageBase;
  Collector.CodeLow:=Image.CodeLow;
  Collector.CodeHigh:=Image.CodeHigh;
  Builder.ImageBase:=Image.ImageBase;
  Collector.WantSymbols:=WantSymbols;
  Collector.SymbolsAdded:=0;
  Collector.DiscardedRows:=0;
  Collector.HaveCurrent:=false;

  // Decide which frontend applies. A Delphi build never carries DWARF, so
  // preferring DWARF when it is present is unambiguous.
  UsedDWARF:=false;
  if not ForceMap then begin
   SymbolImage:=Image;
   if not Image.FindSection('.debug_line',Section) then begin
    DebugFileName:=Image.DebugLinkFileName;
    if length(DebugFileName)>0 then begin
     WriteLn('Following the debug link to ',ExtractFileName(DebugFileName),'.');
     DebugImage:=TImageFile.Create;
     if DebugImage.Open(DebugFileName) and DebugImage.FindSection('.debug_line',Section) then begin
      SymbolImage:=DebugImage;
     end else begin
      FreeAndNil(DebugImage);
     end;
    end;
   end;

   if assigned(DebugImage) then begin
    LineData:=DebugImage.ReadSection('.debug_line');
    LineStringData:=DebugImage.ReadSection('.debug_line_str');
    StringData:=DebugImage.ReadSection('.debug_str');
   end else begin
    LineData:=Image.ReadSection('.debug_line');
    LineStringData:=Image.ReadSection('.debug_line_str');
    StringData:=Image.ReadSection('.debug_str');
   end;

   if assigned(LineData) and (LineData.Size>0) then begin
    if WantLines then begin
     DWARFReader:=TDWARFLineReader.Create(LineData.Memory,TpvSizeInt(LineData.Size));
     // The section was written in the order of the image it came out of, which
     // for a big endian target is not the order this tool runs in.
     DWARFReader.BigEndian:=SymbolImage.BigEndian;
     // Version 5 keeps its path strings in sections of their own, so those are
     // handed over as well where they exist.
     if assigned(LineStringData) or assigned(StringData) then begin
      if assigned(LineStringData) then begin
       LineStringMemory:=LineStringData.Memory;
       LineStringSize:=TpvSizeInt(LineStringData.Size);
      end else begin
       LineStringMemory:=nil;
       LineStringSize:=0;
      end;
      if assigned(StringData) then begin
       StringMemory:=StringData.Memory;
       StringSize:=TpvSizeInt(StringData.Size);
      end else begin
       StringMemory:=nil;
       StringSize:=0;
      end;
      DWARFReader.SetStringSections(LineStringMemory,LineStringSize,StringMemory,StringSize);
     end;
     DWARFReader.Parse(Collector.OnLineRow,Collector.OnLineUnit);
     WriteLn('DWARF: ',DWARFReader.UnitCount,' compilation units, ',DWARFReader.RowCount,' line rows, ',Collector.DiscardedRows,' of them for discarded code.');
     if DWARFReader.SkippedUnitCount>0 then begin
      // Version 5 used to be the usual reason and no longer is, since it is
      // read now. What is left are versions outside two to five, a line range
      // of zero, and an entry format this does not know how to step over.
      WriteLn('Warning: ',DWARFReader.SkippedUnitCount,' compilation units were skipped, because their line program header could not be read.');
     end;
    end;
    // The symbol table normally lives in whichever image carries the debug
    // information, since a stripped executable has none left.
    SymbolImage.EnumerateSymbols(Collector.OnSymbol);
    if (Collector.SymbolsAdded=0) and WantSymbols and (SymbolImage<>Image) then begin
     Image.EnumerateSymbols(Collector.OnSymbol);
    end;
    UsedDWARF:=true;
   end;
  end;

  if not UsedDWARF then begin

   if ForceDWARF then begin
    WriteLn('No DWARF line information found in ',ExecutableFileName,'.');
    ExitCode:=1;
    exit;
   end;

   if length(MapFileName)=0 then begin
    MapFileName:=ChangeFileExt(ExecutableFileName,'.map');
   end;

   if not FileExists(MapFileName) then begin
    WriteLn('Neither DWARF line information nor a map file was found.');
    WriteLn('Expected a map file at ',MapFileName,'.');
    WriteLn('For Delphi, enable a detailed map file, which is DCC_MapFile set to 3.');
    WriteLn('For FreePascal, build with debug information, which is -gw2 -gl.');
    ExitCode:=1;
    exit;
   end;

   MapReader:=TMapFileReader.Create(Builder,Image.ImageBase,WantSymbols,WantLines);
   MapReader.Parse(MapFileName);

  end;

  if Builder.UnitCount=0 then begin
   WriteLn('No code ranges were found, so there is nothing to write.');
   ExitCode:=1;
   exit;
  end;

  Builder.Finish;

  if Builder.TrimmedUnitCount>0 then begin
   WriteLn(Builder.TrimmedUnitCount,' unit ranges reached a few bytes into the one behind them and were pulled back to its start.');
  end;

  // The runtime resolver looks a unit up by binary search, which assumes the
  // ranges do not overlap. A compilation unit whose code the linker scattered
  // could break that assumption, so it is checked rather than hoped for.
  //
  // What is left here is what pulling a range back off the next one cannot
  // explain: a range which the following one begins inside of rather than
  // behind, which is two units genuinely woven through each other and not a
  // boundary which came in a few bytes long.
  OverlapCount:=0;
  for UnitIndex:=1 to Builder.UnitCount-1 do begin
   PreviousUnit:=Builder.GetUnit(UnitIndex-1);
   CurrentUnit:=Builder.GetUnit(UnitIndex);
   if (PreviousUnit.StartRVA+PreviousUnit.Size)>CurrentUnit.StartRVA then begin
    inc(OverlapCount);
    // Named rather than only counted, since the two files which overlap are the
    // whole of what somebody looking into this needs and are not recoverable
    // from anything the tool prints otherwise.
    if OverlapCount<=8 then begin
     WriteLn('  ',PreviousUnit.FileName,' [$',IntToHex(PreviousUnit.StartRVA,8),'..$',IntToHex(PreviousUnit.StartRVA+PreviousUnit.Size,8),
             ') overlaps ',CurrentUnit.FileName,' [$',IntToHex(CurrentUnit.StartRVA,8),'..$',IntToHex(CurrentUnit.StartRVA+CurrentUnit.Size,8),')');
    end;
   end;
  end;
  if OverlapCount>0 then begin
   // A warning was too soft. The reader looks a unit up by binary search over
   // these ranges and takes it for granted that they do not overlap, so a table
   // written in this state answers some addresses with the wrong unit and says
   // so with full confidence. A symbolizer which does not know is better than
   // one which is certain and wrong, and a build which asked for a table has to
   // be able to see that what it got is not one.
   //
   // If this ever fires, try PasVulkanMapSymbolsNoPaddingTolerance: the merging
   // of runs separated by padding is the only thing here which can create an
   // overlap out of ranges which had none.
   //
   // And nothing is written. An exit code beside a finished table would have
   // said one thing and done the other: the executable would have been changed,
   // the table appended and the debug file written, all of them in the state
   // this just called wrong, and every one of them still there after the build
   // system noticed. Stopping here is what makes the sentence above true.
   WriteLn('Error: ',OverlapCount,' unit ranges overlap, so some addresses would be attributed to the wrong unit.');
   // Not "nothing was written": an executable which already carried a table
   // from an earlier run still carries it, and saying nothing was written would
   // read as if it did not.
   WriteLn('Nothing was changed.');
   ExitCode:=1;
   exit;
  end;

  // Everything needed has been read, and the executable is about to be written
  // to, so the read handles have to go first.
  Image.Close;
  if assigned(DebugImage) then begin
   DebugImage.Close;
  end;

  // Ranges rather than units: a unit whose code the linker put down in several
  // pieces contributes one entry per piece, so this number is larger than the
  // number of source files and is meant to be.
  WriteLn(Builder.UnitCount,' unit ranges, ',Builder.SymbolCount,' symbols, ',Builder.LineCount,' line records.');

  if Builder.LineCount=0 then begin
   WriteLn('Warning: no line numbers were found, so only routine names will resolve.');
  end;

  DWARFWriter:=nil;
  PDBWriter:=nil;
  try

   if InjectIntoExecutable or (length(DebugOutputFileName)>0) then begin
    DWARFWriter:=TDWARFWriter.Create(Builder);
    // An address in the DWARF is as wide as an address of the image, which the
    // header of every compilation unit announces and every consumer reads it
    // by. Writing eight for a thirty two bit image would describe something
    // which is not there.
    DWARFWriter.AddressSize:=ImageAddressSize(Image);
    // And in the order the image is written in, since the sections describe
    // that image and are read alongside it.
    DWARFWriter.BigEndian:=Image.BigEndian;
    DWARFWriter.Build;
   end;

   // The pdb has to exist before the executable can name it, since the identity
   // in the debug directory has to be the one the pdb was written with.
   if length(PDBOutputFileName)>0 then begin
    PDBWriter:=WritePDBFile(Builder,Image,PDBOutputFileName);
   end;

   // Injection changes the size of the file, so it has to come first. The
   // appended table is found through a footer at the very end and would be
   // orphaned by anything written behind it.
   // The DWARF sections only go in when that was actually asked for, even if a
   // writer exists because a separate debug file was requested as well.
   if InjectIntoExecutable then begin
    InjectDebugSections(DWARFWriter,PDBWriter,ExtractFileName(PDBOutputFileName),ExecutableFileName);
   end else if assigned(PDBWriter) then begin
    InjectDebugSections(nil,PDBWriter,ExtractFileName(PDBOutputFileName),ExecutableFileName);
   end;

   Builder.AppendToFile(ExecutableFileName);

   if Builder.PackedTo>0 then begin
    WriteLn('Packed ',Builder.PackedFrom,' bytes of contents down to ',Builder.PackedTo,'.');
   end else if Compress then begin
    WriteLn('Not packed, since it would not have come out smaller.');
   end;

   if length(DebugOutputFileName)>0 then begin
    WriteDebugFile(Builder,DWARFWriter,Image,DebugOutputFileName);
   end;

  finally
   FreeAndNil(PDBWriter);
   FreeAndNil(DWARFWriter);
  end;

  // The pdb is checked through dbghelp, which is a different reader than the
  // one which wrote it, and which also has to accept the debug directory in the
  // executable before it will look at the pdb at all.
  if length(PDBOutputFileName)>0 then begin
   if CheckPDBWithDbgHelp(Builder,ExecutableFileName,DbgHelpResolved,DbgHelpProbes,DbgHelpAvailable) then begin
    WriteLn('Debugger check: ',DbgHelpResolved,' of ',DbgHelpProbes,' probes resolved to the expected line.');
   end else if DbgHelpAvailable then begin
    WriteLn('Debugger check FAILED: ',DbgHelpResolved,' of ',DbgHelpProbes,' probes resolved to the expected line.');
    ExitCode:=1;
   end else begin
    WriteLn('Debugger check skipped, dbghelp did not accept the executable.');
   end;
  end;

  if Builder.SelfCheck(ExecutableFileName,Resolved,Probes) then begin
   WriteLn('Self check: ',Resolved,' of ',Probes,' probes resolved to the expected line.');
  end else begin
   WriteLn('Self check FAILED: ',Resolved,' of ',Probes,' probes resolved to the expected line.');
   ExitCode:=1;
  end;

 finally
  FreeAndNil(MapReader);
  FreeAndNil(DWARFReader);
  FreeAndNil(LineData);
  FreeAndNil(LineStringData);
  FreeAndNil(StringData);
  FreeAndNil(Collector);
  FreeAndNil(Builder);
  FreeAndNil(DebugImage);
  FreeAndNil(Image);
 end;

end.
