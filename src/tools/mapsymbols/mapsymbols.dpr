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
{$ifdef Unix}
     // For finding out whether the name a run was given is a symbolic link,
     // which nothing in the portable part of the library answers.
     BaseUnix,
{$endif}
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
       // Whether the line numbers themselves are wanted. The rows are read
       // either way, because the ranges of a compilation unit are worked out
       // from where its rows are and there is nothing else in a DWARF which
       // says it. Not reading them at all left the run with no ranges, and a
       // switch which asks for less then produced nothing rather than less.
       WantLines:Boolean;
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
 //
 // Only the record is skipped when the caller does not want line numbers. The
 // measuring above and the closing below are what the ranges are made of and
 // happen either way.
 if WantLines then begin
  Builder.AddLine(aAddress-ImageBase,aLineNumber);
 end;

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

{$ifdef Windows}
// Declared here rather than taken from the windows unit, which brings its own
// DeleteFile and FileExists along and would take the place of the ones every
// other line in this file means.
const FILE_ATTRIBUTE_REPARSE_POINT_FLAG=TpvUInt32($00000400);
      INVALID_FILE_ATTRIBUTES_VALUE=TpvUInt32($ffffffff);
      FILE_READ_ATTRIBUTES_ACCESS=TpvUInt32($00000080);
      FILE_SHARE_ALL=TpvUInt32($00000007);
      OPEN_EXISTING_FILE=TpvUInt32(3);
      FILE_FLAG_BACKUP_SEMANTICS_FLAG=TpvUInt32($02000000);
      INVALID_FILE_HANDLE=THandle(-1);
      VOLUME_NAME_DOS_FORM=TpvUInt32(0);
function GetFileAttributesW(aFileName:PWideChar):TpvUInt32; stdcall; external 'kernel32.dll' name 'GetFileAttributesW';
function CreateFileW(aFileName:PWideChar;aAccess,aShareMode:TpvUInt32;aSecurity:TpvPointer;aDisposition,aFlags:TpvUInt32;aTemplate:THandle):THandle; stdcall; external 'kernel32.dll' name 'CreateFileW';
function CloseHandle(aHandle:THandle):LongBool; stdcall; external 'kernel32.dll' name 'CloseHandle';
function GetFinalPathNameByHandleW(aFile:THandle;aFilePath:PWideChar;aLength,aFlags:TpvUInt32):TpvUInt32; stdcall; external 'kernel32.dll' name 'GetFinalPathNameByHandleW';
{$endif}

// Follows a symbolic link to the file it really names.
//
// It matters because of how the executable is replaced at the end: what was
// there is renamed out of the way and the new file takes its name. Done to a
// link, that leaves a real file where the link was, the link is gone, and a
// layout which had one name pointing at the build of the day has quietly become
// two unrelated files. The one being read was the target all along, since
// reading a link reads through it, so the one being written has to be the
// target as well.
//
// Refusing links outright would be the other defensible answer. Following them
// is the one which keeps working for whoever set the link up, and it is said
// out loud rather than done quietly.
//
// Both sides, since both have them. Windows has had file links since Vista and
// junctions for longer than that, mklink is exactly what somebody reaches for
// to keep a game.exe pointing at the build of the day, and the damage there is
// the same one. That this was written for unix first says something about where
// links are common, not about where they do harm.
//
// aFailure is set instead when the name cannot be followed to anything, which
// in practice means a link which points at itself. A run which then went ahead
// would work on the link.
function ResolveSymbolicLink(const aFileName:String;out aResolvedFileName,aFailure:String):Boolean;
{$ifdef Unix}
var Information:stat;
    Target,Directory:String;
    Steps:TpvSizeInt;
{$endif}
{$ifdef Windows}
var Attributes:TpvUInt32;
    Handle:THandle;
    Buffer:array[0..32767] of WideChar;
    Length32:TpvUInt32;
    Final:String;
{$endif}
begin
 aResolvedFileName:=aFileName;
 aFailure:='';
 result:=false;
{$ifdef Unix}
 Steps:=0;
 // A link may point at a link. Bounded, because it may also point at itself.
 while Steps<32 do begin
  if (FpLStat(aResolvedFileName,Information)<>0) or not FpS_ISLNK(Information.st_mode) then begin
   exit;
  end;
  Target:=FpReadLink(aResolvedFileName);
  if length(Target)=0 then begin
   // A link which is there and cannot be read. On the first turn nothing has
   // been resolved yet and leaving quietly would be right, but from the second
   // on the name which is being held is itself a link, since that is the only
   // way the loop got here, and reporting success would hand the caller a link
   // to work on. Which is the one thing this exists to prevent.
   aFailure:=aFileName+' is a symbolic link which could not be read.';
   aResolvedFileName:=aFileName;
   result:=false;
   exit;
  end;
  // A link may name its target relative to the directory the link sits in,
  // which is not necessarily the directory this run was started in.
  if Target[1]<>'/' then begin
   Directory:=ExtractFilePath(ExpandFileName(aResolvedFileName));
   if length(Directory)>0 then begin
    Target:=IncludeTrailingPathDelimiter(Directory)+Target;
   end;
  end;
  aResolvedFileName:=Target;
  result:=true;
  inc(Steps);
 end;
 // Still a link after all of those, so it is a ring rather than a chain.
 aFailure:=aFileName+' is a symbolic link which does not lead to a file.';
 aResolvedFileName:=aFileName;
 result:=false;
{$endif}
{$ifdef Windows}
 // Asked first, so that an ordinary file is not put through the rest of this
 // only to come back under a spelling of its own name. What comes out of
 // GetFinalPathNameByHandleW is the canonical one, which differs from what was
 // typed for reasons which have nothing to do with links.
 Attributes:=GetFileAttributesW(PWideChar(WideString(aFileName)));
 if (Attributes=INVALID_FILE_ATTRIBUTES_VALUE) or
    ((Attributes and FILE_ATTRIBUTE_REPARSE_POINT_FLAG)=0) then begin
  exit;
 end;
 // Opened only to be asked about. The backup semantics flag is what lets this
 // work for a directory junction as well, and asking for attributes alone means
 // a file somebody else is writing can still be asked about.
 Handle:=CreateFileW(PWideChar(WideString(aFileName)),FILE_READ_ATTRIBUTES_ACCESS,FILE_SHARE_ALL,nil,
                     OPEN_EXISTING_FILE,FILE_FLAG_BACKUP_SEMANTICS_FLAG,0);
 if Handle=INVALID_FILE_HANDLE then begin
  aFailure:=aFileName+' is a link of some kind which could not be followed.';
  exit;
 end;
 try
  // One call, and it follows the whole chain including junctions.
  Length32:=GetFinalPathNameByHandleW(Handle,@Buffer[0],length(Buffer)-1,VOLUME_NAME_DOS_FORM);
  if (Length32=0) or (Length32>=TpvUInt32(length(Buffer))) then begin
   aFailure:=aFileName+' is a link of some kind which could not be followed.';
   exit;
  end;
  Buffer[Length32]:=#0;
  Final:=String(WideString(PWideChar(@Buffer[0])));
 finally
  CloseHandle(Handle);
 end;
 // It comes back in the form which is not subject to the ordinary path rules,
 // which most of the rest of the world does not take. A network path wears that
 // form too and has to be turned back into the one with the two slashes.
 if copy(Final,1,8)='\\?\UNC\' then begin
  Final:='\\'+copy(Final,9,length(Final)-8);
 end else if copy(Final,1,4)='\\?\' then begin
  Final:=copy(Final,5,length(Final)-4);
 end;
 if (length(Final)>0) and not SameFileName(Final,aFileName) then begin
  aResolvedFileName:=Final;
  result:=true;
 end;
{$endif}
end;

// Whether any two of the files a run reads or writes are one file.
//
// Two names for one file among them costs that file. Everything written here is
// built beside its name and then put under it, and the names it is built beside
// are worked out from the names given, so two which mean the same file means one
// piece of work being built on top of another and put in place as if it were the
// other. Saying --gdb game.exe used to end with the debug file under the name of
// the program, the program gone, and the copy which could have put it back
// thrown away as part of the same step.
//
// Every pair, and the inputs among themselves as well: a map file which is the
// executable is not going to end well either, and there is no reason to find
// that out halfway through parsing it.
//
// aLabels says what each name is for, so that the message names the two things
// which collided rather than only stating that two of them did.
function FilesCollide(const aFileNames,aLabels:array of String;out aMessage:String):Boolean;
var Left,Right:TpvSizeInt;
begin
 result:=false;
 aMessage:='';
 for Left:=0 to length(aFileNames)-2 do begin
  if length(aFileNames[Left])=0 then begin
   continue;
  end;
  for Right:=Left+1 to length(aFileNames)-1 do begin
   if length(aFileNames[Right])=0 then begin
    continue;
   end;
   if SameFileName(aFileNames[Left],aFileNames[Right]) then begin
    aMessage:=aLabels[Left]+' and '+aLabels[Right]+' are the same file, '+aFileNames[Left]+'.';
    result:=true;
    exit;
   end;
  end;
 end;
end;

// A copy of a file, which is where everything is written before any of it takes
// the place of what was there.
function CopyFileTo(const aFromFileName,aToFileName:String):Boolean;
var Source,Target:TFileStream;
begin
 result:=false;
 try
  Source:=TFileStream.Create(aFromFileName,fmOpenRead or fmShareDenyWrite);
  try
   Target:=TFileStream.Create(aToFileName,fmCreate);
   try
    if Source.Size>0 then begin
     Target.CopyFrom(Source,Source.Size);
    end;
    result:=true;
   finally
    FreeAndNil(Target);
   end;
  finally
   FreeAndNil(Source);
  end;
 except
  // A copy which did not come off leaves nothing behind to be mistaken for one.
  DeleteFile(aToFileName);
 end;
end;

// Says what a run which is giving up left behind. The executable is the thing
// which matters, and it really is untouched wherever this is called.
//
// The pdb used to be the exception: it is written before the executable can be
// asked to name it, so a run which gave up afterwards left a file behind which
// nothing pointed at, and the message said so because saying nothing would have
// been a lie about a file sitting right there. It is now put under its name
// with whatever was there kept beside it, and a run which ends this way puts
// that back, so the claim holds again. Still mentioned, because a file which is
// written and then unwritten is worth a word.
procedure ReportUnchanged(const aExecutableFileName,aPDBFileName:String;const aPDBWritten:Boolean);
begin
 if aPDBWritten and (length(aPDBFileName)>0) then begin
  WriteLn('Nothing was changed. ',aPDBFileName,' was written and is being put back the way it was.');
 end else begin
  WriteLn('Nothing was changed.');
 end;
end;

// Whether the compilation units of this image name this tool as their producer,
// which is to say whether the DWARF in it was put there by an earlier run.
//
// It matters because such DWARF must not be read back as a source. It was made
// out of a map file, so it holds nothing the map file does not, and it costs
// something the map file has: the routine names. A Delphi executable which went
// through the injector carries a COFF string table where a symbol table would
// be, and enumerating its symbols yields none, so a run which believed the
// DWARF would collect ranges and lines and no names at all, and would then
// replace a table which had them with one which does not.
//
// A plain scan for the string. It is written inline into every compilation unit
// with DW_FORM_string, so it stands there in the section as it is, and no
// compiler produces it.
function HasOwnDWARF(const aImage:TImageFile):Boolean;
var Section:TMemoryStream;
    Marker:TpvRawByteString;
    Bytes:PpvUInt8Array;
    Index,Position,Length_:TpvSizeInt;
begin
 result:=false;
 Section:=aImage.ReadSection('.debug_info');
 if not assigned(Section) then begin
  exit;
 end;
 try
  Marker:=TpvRawByteString(DWARFProducer);
  Length_:=length(Marker);
  if (Length_=0) or (Section.Size<Length_) then begin
   exit;
  end;
  Bytes:=PpvUInt8Array(Section.Memory);
  for Index:=0 to TpvSizeInt(Section.Size)-Length_ do begin
   Position:=0;
   while (Position<Length_) and (Bytes^[Index+Position]=TpvUInt8(Marker[Position+1])) do begin
    inc(Position);
   end;
   if Position=Length_ then begin
    result:=true;
    exit;
   end;
  end;
 finally
  FreeAndNil(Section);
 end;
end;

// The position of the first collected symbol at or above an address, or the
// symbol count when there is none. The symbols are sorted by address, so this
// and the same call for the end of a range give the run which falls inside it.
function FirstSymbolAtOrAbove(const aBuilder:TSymbolBuilder;const aAddress:TpvUInt64):TpvSizeInt;
var LowIndex,HighIndex,MiddleIndex:TpvSizeInt;
    ImageBase:TpvUInt64;
begin
 ImageBase:=aBuilder.ImageBase;
 LowIndex:=0;
 HighIndex:=aBuilder.SymbolCount;
 while LowIndex<HighIndex do begin
  MiddleIndex:=LowIndex+((HighIndex-LowIndex) shr 1);
  if (ImageBase+aBuilder.GetSymbol(MiddleIndex).RVA)<aAddress then begin
   LowIndex:=MiddleIndex+1;
  end else begin
   HighIndex:=MiddleIndex;
  end;
 end;
 result:=LowIndex;
end;

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
function CheckCompileUnits(const aBuilder:TSymbolBuilder;const aCheck:TImageFile;const aLineSection:TMemoryStream;
                           const aExpectedAddressSize:TpvUInt8):Boolean;
var InfoSection,AbbrevSection:TMemoryStream;
    Reader:TDWARFInfoReader;
    ProgramCollector:TDWARFProgramCollector;
    ProgramReader:TDWARFLineReader;
    InfoUnit:TDWARFInfoUnit;
    Subprogram:TDWARFInfoSubprogram;
    UnitRecord:TSymbolBuilder.TUnitRecord;
    Index,RangeIndex,Complaints,WithLines:TpvSizeInt;
    SubprogramIndex,SubprogramNext,SubprogramComplaints,SubprogramsSeen,SymbolIndex:TpvSizeInt;
    ExpectedSubprograms:TpvSizeInt;
    LowIndex,HighIndex,MiddleIndex:TpvSizeInt;
    Directory:String;
    Offset,ProgramLength:TpvUInt64;
    ImageBase,Expected:TpvUInt64;
    Used:array of Boolean;
    ProgramOffsets:array of TpvUInt64;
    ProgramLengths:array of TpvUInt64;
    ProgramCount,Found:TpvSizeInt;
    Bytes:PpvUInt8Array;
begin

 result:=false;

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
   exit;
  end;

  Reader:=TDWARFInfoReader.Create(InfoSection.Memory,InfoSection.Size,AbbrevSection.Memory,AbbrevSection.Size);
  Reader.BigEndian:=aCheck.BigEndian;
  if not Reader.Parse then begin
   WriteLn('Debug file check FAILED: the compilation units cannot be read back, ',Reader.Message,'.');
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
  SubprogramComplaints:=0;
  SubprogramsSeen:=0;

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
   UnitRecord:=aBuilder.GetUnit(RangeIndex);

   // The file it names, which is the whole reason a consumer comes here rather
   // than reading the line section directly: the rows say which line, and only
   // this says of which file. All of it can be right and this one wrong, and
   // the answer is then a correct line number of the wrong source.
   if InfoUnit.Name<>ExtractFileName(UnitRecord.FileName) then begin
    WriteLn('  the compilation unit at $',IntToHex(InfoUnit.LowPC,8),' names ',InfoUnit.Name,
            ' where the range it covers was collected for ',ExtractFileName(UnitRecord.FileName),'.');
    inc(Complaints);
   end;

   // And the directory it names, which is the other half of finding the source:
   // a debugger puts the two together, so a right file name under a wrong
   // directory gives a correct line number with nothing to show beside it. The
   // dot is what the writer puts down for a unit whose file name carries no
   // directory, so the same substitution is made here rather than comparing
   // against an empty string the writer never emits.
   Directory:=ExtractFileDir(UnitRecord.FileName);
   if length(Directory)=0 then begin
    Directory:='.';
   end;
   if InfoUnit.Directory<>Directory then begin
    WriteLn('  compilation unit ',InfoUnit.Name,' names the directory ',InfoUnit.Directory,
            ' where the range it covers was collected under ',Directory,'.');
    inc(Complaints);
   end;

   // And the width it announces, which every address behind it is written in.
   if InfoUnit.AddressSize<>aExpectedAddressSize then begin
    WriteLn('  compilation unit ',InfoUnit.Name,' announces ',InfoUnit.AddressSize,
            ' byte addresses where the image has ',aExpectedAddressSize,'.');
    inc(Complaints);
   end;

   // The offsets came out of walking the section from the front, so they are
   // ascending and the same binary search fits here as everywhere else.
   Found:=-1;
   LowIndex:=0;
   HighIndex:=ProgramCount-1;
   while LowIndex<=HighIndex do begin
    MiddleIndex:=LowIndex+((HighIndex-LowIndex) shr 1);
    if ProgramOffsets[MiddleIndex]<InfoUnit.StatementListOffset then begin
     LowIndex:=MiddleIndex+1;
    end else if ProgramOffsets[MiddleIndex]>InfoUnit.StatementListOffset then begin
     HighIndex:=MiddleIndex-1;
    end else begin
     Found:=MiddleIndex;
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

   // The subprogram descriptions, which are what a debugger takes the name of a
   // routine from. Not the symbol table: that one is read when there is no
   // DWARF, and where there is, these win. So a wrong address here survives
   // every check above, because the symbols it would contradict are in a part
   // of the file nothing compares them against.
   for SubprogramIndex:=0 to InfoUnit.SubprogramCount-1 do begin

    Subprogram:=InfoUnit.Subprograms[SubprogramIndex];

    if (Subprogram.LowPC<InfoUnit.LowPC) or (Subprogram.LowPC>=InfoUnit.HighPC) or
       (Subprogram.HighPC<Subprogram.LowPC) or (Subprogram.HighPC>InfoUnit.HighPC) then begin
     if SubprogramComplaints<8 then begin
      WriteLn('  ',Subprogram.Name,' covers $',IntToHex(Subprogram.LowPC,8),'..$',IntToHex(Subprogram.HighPC,8),
              ', which is not inside the $',IntToHex(InfoUnit.LowPC,8),'..$',IntToHex(InfoUnit.HighPC,8),' of ',InfoUnit.Name,'.');
     end;
     inc(SubprogramComplaints);
     continue;
    end;

    // No routine may reach over where another one begins. Being inside the
    // compilation unit is not enough: an end address taken from the wrong
    // neighbour stays inside it and still swallows everything behind. Checked
    // against the next routine at a different address, since routines which
    // share one are aliases of each other and cover the same ground.
    SubprogramNext:=SubprogramIndex+1;
    while (SubprogramNext<InfoUnit.SubprogramCount) and
          (InfoUnit.Subprograms[SubprogramNext].LowPC=Subprogram.LowPC) do begin
     inc(SubprogramNext);
    end;
    // Reaching past it is the failure which matters, but stopping short of it
    // is one too: the bytes in between then belong to no routine at all, and an
    // address in them resolves to nothing where it should have resolved to
    // this. Code is laid down without holes here, so the two boundaries meet.
    if SubprogramNext<InfoUnit.SubprogramCount then begin
     Expected:=InfoUnit.Subprograms[SubprogramNext].LowPC;
    end else begin
     Expected:=InfoUnit.HighPC;
    end;
    if Subprogram.HighPC<>Expected then begin
     if SubprogramComplaints<8 then begin
      WriteLn('  ',Subprogram.Name,' covers $',IntToHex(Subprogram.LowPC,8),'..$',IntToHex(Subprogram.HighPC,8),
              ' where the ground up to $',IntToHex(Expected,8),' is its own.');
     end;
     inc(SubprogramComplaints);
    end;

    // And it has to be a routine which was collected, at the address it was
    // collected at. Searched by address, since that is the direction a reader
    // uses it in. In its own variable rather than in the one which holds the
    // line program above, which is still needed after this loop.
    LowIndex:=0;
    HighIndex:=aBuilder.SymbolCount-1;
    SymbolIndex:=-1;
    while LowIndex<=HighIndex do begin
     MiddleIndex:=LowIndex+((HighIndex-LowIndex) shr 1);
     if (ImageBase+aBuilder.GetSymbol(MiddleIndex).RVA)<Subprogram.LowPC then begin
      LowIndex:=MiddleIndex+1;
     end else if (ImageBase+aBuilder.GetSymbol(MiddleIndex).RVA)>Subprogram.LowPC then begin
      HighIndex:=MiddleIndex-1;
     end else begin
      SymbolIndex:=MiddleIndex;
      break;
     end;
    end;
    // More than one name can sit at an address, so the run of them is walked.
    if SymbolIndex>=0 then begin
     while (SymbolIndex>0) and ((ImageBase+aBuilder.GetSymbol(SymbolIndex-1).RVA)=Subprogram.LowPC) do begin
      dec(SymbolIndex);
     end;
     while (SymbolIndex<aBuilder.SymbolCount) and ((ImageBase+aBuilder.GetSymbol(SymbolIndex).RVA)=Subprogram.LowPC) do begin
      if aBuilder.GetSymbol(SymbolIndex).Name=Subprogram.Name then begin
       break;
      end;
      inc(SymbolIndex);
     end;
     if (SymbolIndex>=aBuilder.SymbolCount) or ((ImageBase+aBuilder.GetSymbol(SymbolIndex).RVA)<>Subprogram.LowPC) then begin
      SymbolIndex:=-1;
     end;
    end;
    if SymbolIndex<0 then begin
     if SubprogramComplaints<8 then begin
      WriteLn('  ',Subprogram.Name,' is described at $',IntToHex(Subprogram.LowPC,8),
              ', where no routine of that name was collected.');
     end;
     inc(SubprogramComplaints);
    end else begin
     inc(SubprogramsSeen);
    end;

   end;

   // The other direction. Every subprogram being right says nothing about one
   // which is not there at all: a writer which drops a routine leaves the ones
   // it did write perfectly correct, and the loop above would pass. The
   // symbols are sorted by address, so what has to be described here is simply
   // the run which falls inside this range.
   ExpectedSubprograms:=FirstSymbolAtOrAbove(aBuilder,InfoUnit.HighPC)-
                        FirstSymbolAtOrAbove(aBuilder,InfoUnit.LowPC);
   if InfoUnit.SubprogramCount<>ExpectedSubprograms then begin
    WriteLn('  compilation unit ',InfoUnit.Name,' describes ',InfoUnit.SubprogramCount,
            ' routines where ',ExpectedSubprograms,' were collected in the range it covers.');
    inc(Complaints);
   end;

  end;

  if SubprogramComplaints>0 then begin
   WriteLn('  ',SubprogramComplaints,' subprogram descriptions do not match what was collected.');
   inc(Complaints);
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
  end else begin
   WriteLn('Debug file check: ',Reader.UnitCount,' compilation units name the ranges and files they were built from, point at their own line programs, and describe ',
           SubprogramsSeen,' routines which were collected.');
   result:=true;
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

// The DWARF of a file which was just written, held against what went in. Split
// out from the file check below because the same sections go into two different
// containers, the standalone debug file and the executable itself, and both of
// them are worth reading back.
function CheckDWARFSections(const aBuilder:TSymbolBuilder;const aDWARFWriter:TDWARFWriter;
                            const aCheck:TImageFile;const aExpectedAddressSize:TpvUInt8):Boolean;
var LineSection:TMemoryStream;
    LineReader:TDWARFLineReader;
    LineCollector:TDWARFCheckCollector;
    LineRecord:TSymbolBuilder.TLineRecord;
    Expected,Index:TpvSizeInt;
begin

 result:=false;

 LineSection:=nil;
 LineReader:=nil;
 LineCollector:=TDWARFCheckCollector.Create;
 try

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

  // Nothing was collected and nothing was written, which is what asking for no
  // line numbers looks like from here. Both halves are required: a writer which
  // put down nothing while records were there is a failure and falls through to
  // the checks below rather than into this.
  if (Expected=0) and (aDWARFWriter.LineRowCount=0) then begin
   WriteLn('Debug file check: no line numbers were collected, so there are no line programs to read back.');
   result:=true;
   exit;
  end;

  LineSection:=aCheck.ReadSection('.debug_line');
  if not assigned(LineSection) then begin
   WriteLn('Debug file check FAILED: the file which was just written has no readable line programs.');
   exit;
  end;

  LineReader:=TDWARFLineReader.Create(LineSection.Memory,LineSection.Size);
  LineReader.BigEndian:=aCheck.BigEndian;
  LineCollector.Builder:=aBuilder;
  LineCollector.ImageBase:=aBuilder.ImageBase;
  LineReader.Parse(LineCollector.OnRow,nil);

  result:=true;

  if (LineCollector.Rows<>Expected) or (LineCollector.Mismatched>0) then begin
   WriteLn('Debug file check FAILED: ',LineCollector.Rows,' of ',Expected,' line rows came back, ',LineCollector.Mismatched,' with the wrong line number.');
   result:=false;
  end else begin
   WriteLn('Debug file check: ',LineCollector.Rows,' line rows came back with the line numbers they went in with.');
  end;

  if aDWARFWriter.LineRowCount<>Expected then begin
   WriteLn('Debug file check FAILED: the writer put down ',aDWARFWriter.LineRowCount,' line rows where the collected records come to ',Expected,'.');
   result:=false;
  end;

  // Every part is asked, and one which failed does not stop the next: a run
  // which is going to be turned down anyway is worth as much detail as it can
  // give about why.
  if not CheckCompileUnits(aBuilder,aCheck,LineSection,aExpectedAddressSize) then begin
   result:=false;
  end;

 finally
  FreeAndNil(LineReader);
  FreeAndNil(LineSection);
  FreeAndNil(LineCollector);
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
function CheckDebugFile(const aBuilder:TSymbolBuilder;const aDWARFWriter:TDWARFWriter;const aImage:TImageFile;const aFileName:String):Boolean;
var Check:TImageFile;
    Collector:TCheckCollector;
    Expected:TpvSizeInt;
begin

 result:=false;

 Collector:=TCheckCollector.Create;
 Check:=TImageFile.Create;
 try

  if not Check.Open(aFileName) then begin
   WriteLn('Debug file check failed: the file which was just written cannot be read back.');
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
   exit;
  end;

  Collector.Builder:=aBuilder;
  Collector.ImageBase:=aBuilder.ImageBase;
  Check.EnumerateSymbols(Collector.OnSymbol);

  result:=true;

  // Every symbol which went in has to come back, at the address it went in at.
  Expected:=aBuilder.SymbolCount;
  if (Collector.Seen<>Expected) or (Collector.Mismatched>0) then begin
   WriteLn('Debug file check FAILED: ',Collector.Seen,' of ',Expected,' symbols came back, ',Collector.Mismatched,' at the wrong address.');
   result:=false;
  end else begin
   WriteLn('Debug file check: ',Collector.Seen,' symbols came back at the addresses they went in at.');
  end;

  if assigned(aDWARFWriter) and
     not CheckDWARFSections(aBuilder,aDWARFWriter,Check,ImageAddressSize(aImage)) then begin
   result:=false;
  end;

 finally
  FreeAndNil(Check);
  FreeAndNil(Collector);
 end;

end;

// Reads back the entry which names the pdb, out of the executable which was
// just written.
//
// This is the one thing a debugger looks at before anything else, and until now
// it was the one thing nobody looked at again. The pdb itself is held against
// dbghelp and comes back clean, but that says nothing about whether the
// executable leads there: a wrong identity and the debugger refuses the pdb it
// just found, a wrong name and it never finds it at all, and in both cases the
// symbols are perfect and unreachable.
function CheckCodeViewEntry(const aPDBWriter:TPDBWriter;const aPDBFileName,aFileName:String):Boolean;
var Check:TImageFile;
    Info:TImageCodeViewInfo;
    Written:PpvUInt8Array;
    Index:TpvSizeInt;
    Same:Boolean;
begin
 result:=false;
 Check:=TImageFile.Create;
 try
  if not Check.Open(aFileName) then begin
   WriteLn('Debug directory check FAILED: the executable cannot be read back.');
   exit;
  end;
  if not Check.CodeViewInfo(Info) then begin
   WriteLn('Debug directory check FAILED: the executable does not name a pdb at all.');
   exit;
  end;
  Written:=PpvUInt8Array(aPDBWriter.GUIDPointer);
  Same:=true;
  for Index:=0 to 15 do begin
   if Info.GUID[Index]<>Written^[Index] then begin
    Same:=false;
    break;
   end;
  end;
  if (not Same) or (Info.Age<>aPDBWriter.Age) then begin
   WriteLn('Debug directory check FAILED: the identity in the executable is not the one the pdb was written with.');
  end else if Info.FileName<>aPDBFileName then begin
   WriteLn('Debug directory check FAILED: the executable names ',Info.FileName,' where the pdb was written as ',aPDBFileName,'.');
  end else begin
   WriteLn('Debug directory check: the executable names ',Info.FileName,' with the identity it was written with.');
   result:=true;
  end;
 finally
  FreeAndNil(Check);
 end;
end;

// The same for the DWARF which was put into the executable itself. That is the
// same information in a different container, and the container is the part
// which differs: section names longer than eight characters live in the string
// table of a PE rather than in the section header, and the offsets are laid out
// by a different piece of code. So the sections coming back out of the file the
// user will actually ship is not something the standalone file can stand in for.
//
// The symbols are not looked at. They belong to the executable and have nothing
// to do with what was collected here.
function CheckInjectedDebugSections(const aBuilder:TSymbolBuilder;const aDWARFWriter:TDWARFWriter;
                                    const aImage:TImageFile;const aFileName:String):Boolean;
var Check:TImageFile;
begin
 result:=false;
 Check:=TImageFile.Create;
 try
  if not Check.Open(aFileName) then begin
   WriteLn('Injected sections check FAILED: the executable cannot be read back.');
   exit;
  end;
  result:=CheckDWARFSections(aBuilder,aDWARFWriter,Check,ImageAddressSize(aImage));
 finally
  FreeAndNil(Check);
 end;
end;

// Emits everything which was collected a second time, as DWARF inside a
// standalone ELF file. Nothing about the original executable changes.
//
// Writes the file it is given and reads it back. Whether that file then takes
// the name the run asked for is the caller's question, and it asks it by
// looking at what this returns.
function WriteDebugFile(const aBuilder:TSymbolBuilder;const aDWARFWriter:TDWARFWriter;const aImage:TImageFile;const aFileName:String):Boolean;
var ELFWriter:TELFWriter;
    Index:TpvSizeInt;
    SymbolRecord,NextSymbol:TSymbolBuilder.TSymbolRecord;
    UnitRecord:TSymbolBuilder.TUnitRecord;
    ImageBase,Low,High,SymbolSize,SymbolEnd:TpvUInt64;
    RangeIndex,NextIndex:TpvSizeInt;
{$ifdef PasVulkanMapSymbolsLinearLookups}
    ScanIndex:TpvSizeInt;
{$endif}
    Have:Boolean;
begin

 result:=false;

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

  // Only when there is something to describe. Without line numbers there are no
  // compilation units either, since the writer has nothing to say about a unit
  // it knows no rows for, and an abbreviation table on its own describes
  // nothing. What is left is a file of symbols, which is still worth having.
  if aDWARFWriter.DebugLine.Size>0 then begin
   ELFWriter.AddDebugSection('.debug_info',aDWARFWriter.DebugInfo);
   ELFWriter.AddDebugSection('.debug_abbrev',aDWARFWriter.DebugAbbrev);
   ELFWriter.AddDebugSection('.debug_line',aDWARFWriter.DebugLine);
  end;

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
   // The next one is the next at a different address. Two names at one address
   // are ordinary, and taking the one right behind meant comparing an address
   // with itself, finding it no larger, and leaving the size at zero for the
   // first of every such pair while its twin got the real one.
   SymbolSize:=0;

   // Where the routine ends. The end of the range it sits in to begin with, cut
   // back to the next routine when there is one in front of that. Built this
   // way round rather than out of the next routine alone, because the last
   // routine of the whole table has no next one and was left at a size of
   // nothing although the range it is in says perfectly well where it stops.
   // That is also how the dwarf and the pdb work it out, so all three now
   // describe the same thing.
   SymbolEnd:=SymbolRecord.RVA;
{$ifdef PasVulkanMapSymbolsLinearLookups}
   RangeIndex:=-1;
   for ScanIndex:=0 to aBuilder.UnitCount-1 do begin
    UnitRecord:=aBuilder.GetUnit(ScanIndex);
    if (SymbolRecord.RVA>=UnitRecord.StartRVA) and
       (SymbolRecord.RVA<(UnitRecord.StartRVA+UnitRecord.Size)) then begin
     RangeIndex:=ScanIndex;
     SymbolEnd:=UnitRecord.StartRVA+UnitRecord.Size;
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
    SymbolEnd:=UnitRecord.StartRVA+UnitRecord.Size;
   end;
{$endif}

   // And the next routine at a different address, which is where this one stops
   // when it comes before the end of the range. Two names at one address are
   // aliases of each other and cover the same ground.
   NextIndex:=Index+1;
   while (NextIndex<aBuilder.SymbolCount) and
         (aBuilder.GetSymbol(NextIndex).RVA=SymbolRecord.RVA) do begin
    inc(NextIndex);
   end;
   if NextIndex<aBuilder.SymbolCount then begin
    NextSymbol:=aBuilder.GetSymbol(NextIndex);
    // Where no range covers this at all, which is what a routine outside every
    // compilation unit is, the distance to the next one is the only answer
    // there is, and it is the one this gave before. Where a range does cover
    // it, the next routine only shortens what the range already said.
    if (NextSymbol.RVA>SymbolRecord.RVA) and
       ((RangeIndex<0) or (NextSymbol.RVA<SymbolEnd)) then begin
     SymbolEnd:=NextSymbol.RVA;
    end;
   end;

   if SymbolEnd>SymbolRecord.RVA then begin
    SymbolSize:=SymbolEnd-SymbolRecord.RVA;
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
  result:=CheckDebugFile(aBuilder,aDWARFWriter,aImage,aFileName);
  if not result then begin
   ExitCode:=1;
  end;

 finally
  FreeAndNil(ELFWriter);
 end;

end;

// Puts the same DWARF sections into the executable itself, so that no separate
// file is needed at all. Has to happen before the symbol table is appended,
// since that has to stay at the very end of the file.
function InjectDebugSections(const aBuilder:TSymbolBuilder;const aDWARFWriter:TDWARFWriter;const aPDBWriter:TPDBWriter;
                            const aImage:TImageFile;const aPDBFileName,aFileName:String):Boolean;
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

  // Built beside the original first, then looked at, and only put in its place
  // once it holds up. The injector already compares the sections it moved
  // before it commits; what is read back here is what it wrote, which nothing
  // had looked at until the executable had already been replaced. A failure now
  // leaves the file exactly as it was rather than leaving a finished executable
  // with an error code beside it.
  //
  // Nothing is said about having injected anything until it has happened. The
  // message the injector has at this point describes a file which is sitting
  // beside the original and may still be thrown away.
  result:=Injector.Prepare(aFileName);
  if not result then begin
   WriteLn(Injector.Message);
   exit;
  end;

  if assigned(aDWARFWriter) and
     not CheckInjectedDebugSections(aBuilder,aDWARFWriter,aImage,Injector.TemporaryFileName) then begin
   result:=false;
  end;
  if assigned(aPDBWriter) and
     not CheckCodeViewEntry(aPDBWriter,aPDBFileName,Injector.TemporaryFileName) then begin
   result:=false;
  end;

  if result then begin
   result:=Injector.Commit;
   WriteLn(Injector.Message);
  end else begin
   Injector.Discard;
   WriteLn('The sections which were written did not read back correctly, so the executable was left alone.');
  end;

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
  // Said by the caller rather than here, once the file is under the name it is
  // supposed to have. This writes it beside that name, and announcing a file
  // under a name it does not yet carry is a message which is not true when it
  // is printed and may never become true.
  PDBWriter.SaveToFile(aFileName);
 except
  FreeAndNil(result);
  raise;
 end;
end;

var ExecutableFileName,MapFileName,DebugFileName,DebugLinkMessage,Parameter:String;
    WorkFileName,ReplaceMessage,ResolvedFileName,ResolveFailure:String;
    // The pdb cannot wait for the end the way the executable does, because the
    // reader which checks it finds it by the name the executable gives. So it
    // goes under that name straight away and whatever was there is kept, until
    // the run knows whether it wants the new one or the old one back.
    PDBWorkFileName,PDBBackupFileName,StageMessage,CheckSumMessage,CollisionMessage:String;
    // The same for the separate debug file, which is decided on its own rather
    // than together with the executable: it describes the executable but
    // nothing about the executable points at it, so it is put in place as soon
    // as it has been read back.
    DebugWorkFileName,DebugBackupFileName:String;
    PDBStaged:Boolean;
    OutputOk,BothRemain,StageBothRemain:Boolean;
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
    DbgHelpAvailable,DbgHelpRefused:Boolean;
    UsedDWARF:Boolean;
    OwnDWARF:Boolean;
    ForeignDWARFInExecutable:Boolean;

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
 // Empty until there is one, since the way out of here deletes whatever it
 // names and every way out passes through it, including the early ones.
 WorkFileName:='';
 PDBWorkFileName:='';
 PDBBackupFileName:='';
 DebugWorkFileName:='';
 DebugBackupFileName:='';
 PDBStaged:=false;
 OutputOk:=false;

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
  end else if Parameter='--any-rights' then begin
   FileRightsAreOptional:=true;
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
  WriteLn('  --any-rights   go ahead even when the access rights of the executable');
  WriteLn('                 cannot be given to the file which replaces it. Without');
  WriteLn('                 this such a run stops and leaves the executable alone,');
  WriteLn('                 since a program which will not start is not a result. For');
  WriteLn('                 a volume which has no such rights to begin with');
  WriteLn('  --pe-debug     put those DWARF sections into the executable itself, so');
  WriteLn('                 that no separate file is needed. Needs room in the section');
  WriteLn('                 header table and says so when there is none');
  WriteLn;
  WriteLn('Running it again on the same executable replaces the previous table.');
  WriteLn;
  WriteLn('It belongs before the steps which describe the finished file rather than after');
  WriteLn('them. Everything here is written into the executable, so a signature taken over');
  WriteLn('it stops matching, and the file which ends up under the name is a new one, so');
  WriteLn('what setcap, an acl or an extended attribute put on the old one is not on it.');
  WriteLn('The access rights and the attributes are carried over, and nothing else can be');
  WriteLn('by a tool which is not root.');
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

  // Before anything is read, and long before anything is written, since what is
  // written at the end takes the name this run was given.
  if ResolveSymbolicLink(ExecutableFileName,ResolvedFileName,ResolveFailure) then begin
   WriteLn(ExecutableFileName,' is a link to ',ResolvedFileName,'.');
   WriteLn('That file is the one which is worked on, so the link stays a link and keeps pointing where it did.');
   ExecutableFileName:=ResolvedFileName;
  end else if length(ResolveFailure)>0 then begin
   WriteLn('Error: ',ResolveFailure);
   ExitCode:=1;
   exit;
  end;

  // The same for what is going to be written, for the same reason. A pdb or a
  // debug file under a name which is a link is put in place the same way the
  // executable is, by the old file being renamed aside and the new one taking
  // the name, and done to a link that is the link gone.
  //
  // Only when the name is already taken by a link. A name which is not there
  // yet is a name, and resolving it is a question about nothing.
  if length(PDBOutputFileName)>0 then begin
   if ResolveSymbolicLink(PDBOutputFileName,ResolvedFileName,ResolveFailure) then begin
    WriteLn(PDBOutputFileName,' is a link to ',ResolvedFileName,', which is where the pdb is written.');
    PDBOutputFileName:=ResolvedFileName;
   end else if length(ResolveFailure)>0 then begin
    WriteLn('Error: ',ResolveFailure);
    ExitCode:=1;
    exit;
   end;
  end;
  if length(DebugOutputFileName)>0 then begin
   if ResolveSymbolicLink(DebugOutputFileName,ResolvedFileName,ResolveFailure) then begin
    WriteLn(DebugOutputFileName,' is a link to ',ResolvedFileName,', which is where the debug file is written.');
    DebugOutputFileName:=ResolvedFileName;
   end else if length(ResolveFailure)>0 then begin
    WriteLn('Error: ',ResolveFailure);
    ExitCode:=1;
    exit;
   end;
  end;

  // Asked before anything is opened, of the names this run was given. Asked
  // again further down of the files it turned out to actually read, because two
  // of those are not named on the command line at all: the map file which is
  // found beside the executable when none was named, and the debug file a
  // .gnu_debuglink points at.
  if FilesCollide([ExecutableFileName,MapFileName,PDBOutputFileName,DebugOutputFileName],
                  ['the executable','the map file','the pdb','the debug file to write'],
                  CollisionMessage) then begin
   WriteLn('Error: ',CollisionMessage);
   WriteLn('Each of them has to be a file of its own, since what is written into one would otherwise take the place of another.');
   ExitCode:=1;
   exit;
  end;

  // What this run is going to write, so that no name it picks for itself to
  // build in can turn out to be one of them. Those files are not there yet,
  // which is exactly why looking at the disk does not answer it.
  KeepFileNamesClear([ExecutableFileName,PDBOutputFileName,DebugOutputFileName]);

  Image:=TImageFile.Create;
  if not Image.Open(ExecutableFileName) then begin
   WriteLn('Could not read ',ExecutableFileName,', is that a PE executable?');
   ExitCode:=1;
   exit;
  end;

  WriteLn('Image base $',IntToHex(Image.ImageBase,16));

  // A signature is taken over the file as it was signed, so anything written
  // into that file makes it stop matching, and everything this tool does is
  // written into that file. The injector turns such an image away outright, but
  // a run which only appends the table never reaches the injector, and that run
  // used to say nothing at all about what it was doing to the signature.
  //
  // Said rather than refused. Signing after this tool has run is the ordinary
  // order of a build, and a run which is part of such a build would be stopped
  // by a refusal for no reason.
  if ImageIsSigned(ExecutableFileName) then begin
   WriteLn('Note: this executable is signed, and everything written here is written into the file the signature was taken over.');
   WriteLn('The signature will no longer match afterwards, so this has to run before the signing rather than after it.');
  end;

  Builder:=TSymbolBuilder.Create;
  Builder.StripPaths:=StripPaths;
  Builder.Compress:=Compress;
  Collector:=TCollector.Create;
  Collector.Builder:=Builder;
  Collector.ImageBase:=Image.ImageBase;
  Collector.CodeLow:=Image.CodeLow;
  Collector.CodeHigh:=Image.CodeHigh;
  Builder.ImageBase:=Image.ImageBase;

  // Whether the executable itself carries debug sections which a compiler put
  // there. Asked of the image and of nothing else, before any of the deciding
  // about where the symbols are going to be read from: that is a separate
  // question, and reading them out of a map file instead does not make it any
  // more acceptable to write over what a compiler emitted. Asking it inside
  // that decision left it unanswered whenever the map was forced, which is
  // exactly the combination somebody reaches for when the map has the better
  // names, and the sections would then have been replaced after all.
  //
  // Any of the sections counts, not the line programs alone. What is protected
  // here is the compiler's description, and a file which has the types and the
  // abbreviations without the lines still has more in it than what would take
  // its place.
  ForeignDWARFInExecutable:=(Image.FindSection('.debug_info',Section) or
                             Image.FindSection('.debug_abbrev',Section) or
                             Image.FindSection('.debug_line',Section)) and
                            not HasOwnDWARF(Image);

  Collector.WantSymbols:=WantSymbols;
  Collector.WantLines:=WantLines;
  Collector.SymbolsAdded:=0;
  Collector.DiscardedRows:=0;
  Collector.HaveCurrent:=false;

  // Decide which frontend applies. A Delphi build never carries DWARF, so
  // preferring DWARF when it is present is unambiguous.
  UsedDWARF:=false;
  OwnDWARF:=false;
  if not ForceMap then begin
   SymbolImage:=Image;

   // Whether the DWARF in the executable is what an earlier run of this put
   // there. Settled before the debug link is considered, and not afterwards:
   // once sections have been injected the executable has a .debug_line of its
   // own, so a link to the real debug file beside it would never be followed
   // again, and a stripped build would go from working to having no source at
   // all on its second run.
   OwnDWARF:=Image.FindSection('.debug_line',Section) and HasOwnDWARF(Image);
   if OwnDWARF then begin
    WriteLn('The debug sections in this file were written by an earlier run of this tool, so they are not read back as a source.');
   end;

   if OwnDWARF or not Image.FindSection('.debug_line',Section) then begin
    DebugFileName:=Image.DebugLinkFileName(DebugLinkMessage);
    if length(DebugLinkMessage)>0 then begin
     WriteLn('Debug link: ',DebugLinkMessage,'.');
    end;
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
   end else if not OwnDWARF then begin
    LineData:=Image.ReadSection('.debug_line');
    LineStringData:=Image.ReadSection('.debug_line_str');
    StringData:=Image.ReadSection('.debug_str');
   end;

   if assigned(LineData) and (LineData.Size>0) then begin
    // Read whether or not the line numbers are wanted. What a compilation unit
    // covers is only stated by where its rows are, and nothing else in a DWARF
    // says it, so skipping the rows leaves no ranges and without ranges there
    // is nothing to write at all. Asking for fewer line numbers used to produce
    // nothing rather than fewer. The rows are dropped in the collector instead.
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
    if OwnDWARF then begin
     WriteLn('The only DWARF in ',ExecutableFileName,' is what an earlier run of this tool put there, which is not a source.');
     WriteLn('Build the executable again and run this on the fresh one.');
    end else begin
     WriteLn('No DWARF line information found in ',ExecutableFileName,'.');
    end;
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

  // Putting debug sections into an executable which already has its own is not
  // adding to it, it is writing over it. What a compiler emits carries types,
  // variables, scopes and much else; what is written here is a compilation unit,
  // its routines and its lines, which is what turns a crash address into a
  // place and no more. The injector replaces sections by name, so the richer
  // one would simply be gone, and for a build which is already debuggable that
  // is a loss with nothing gained: the executable already holds what this was
  // going to put in it.
  if InjectIntoExecutable and ForeignDWARFInExecutable then begin
   WriteLn(ExecutableFileName,' already carries debug sections of its own, so none are put into it.');
   WriteLn('The appended symbol table is written as usual.');
   InjectIntoExecutable:=false;
  end;

  // And the same question again, now that it is known which files were really
  // read. Two of them were not named on the command line: a map file is looked
  // for beside the executable when none was given, and a .gnu_debuglink names a
  // debug file which is opened without anybody having asked for it here.
  //
  // Both are ordinary things to write over by accident. A build which says
  // --gdb program.dbg on a stripped executable whose debug link points at
  // program.dbg has just read the compiler's own debug file, with its types and
  // its scopes and its variables, and is about to put this tool's much smaller
  // one in its place. That the run read it first only means the run finishes;
  // the file is gone either way.
  //
  // The names are resolved through links first, since two names for one file is
  // the question being asked and a link is one way for that to happen.
  if length(MapFileName)>0 then begin
   if ResolveSymbolicLink(MapFileName,ResolvedFileName,ResolveFailure) then begin
    MapFileName:=ResolvedFileName;
   end else if length(ResolveFailure)>0 then begin
    // A name which leads to a link which cannot be followed. The file behind it
    // was read a moment ago, so this is not about reading it; it is about not
    // being able to tell whether it is one of the files about to be written.
    WriteLn('Error: ',ResolveFailure);
    ExitCode:=1;
    exit;
   end;
  end;
  if length(DebugFileName)>0 then begin
   if ResolveSymbolicLink(DebugFileName,ResolvedFileName,ResolveFailure) then begin
    DebugFileName:=ResolvedFileName;
   end else if length(ResolveFailure)>0 then begin
    WriteLn('Error: ',ResolveFailure);
    ExitCode:=1;
    exit;
   end;
  end;
  if FilesCollide([ExecutableFileName,MapFileName,DebugFileName,PDBOutputFileName,DebugOutputFileName],
                  ['the executable','the map file which was read','the debug file which was read','the pdb','the debug file to write'],
                  CollisionMessage) then begin
   WriteLn('Error: ',CollisionMessage);
   WriteLn('What was read cannot be what is written, since the run would then replace the description it was built from.');
   ReportUnchanged(ExecutableFileName,PDBOutputFileName,false);
   ExitCode:=1;
   exit;
  end;

  // Everything from here on is written to a copy, and the copy takes the place
  // of the executable at the very end and only if every check of it passed.
  //
  // The injector already worked this way for its own part, but the part after
  // it did not: the table was appended to the executable itself, its own footer
  // truncated first, and only then was any of it read back. A failure there, or
  // a disk which filled up in the middle of it, left a program whose symbol
  // table was half rewritten. That the program still ran is not the point: the
  // run said it had failed and had already changed the file it was given.
  //
  // Now nothing is changed until everything holds. What is left behind on a
  // failure is a copy which is deleted, and the executable is the file it was.
  // A free name, taken by being created. The extension is kept, so the copy is
  // still an .exe while it is being worked on: dbghelp is handed this file, and
  // a reader which decides what a file is by its name has to keep deciding the
  // same thing.
  //
  // The name used to be worked out and whatever was there deleted, which made
  // every file called so beside an executable this tool's to throw away.
  if not AcquireTemporaryName(ExecutableFileName,'mapsymbols-work',WorkFileName,StageMessage) then begin
   WriteLn('Error: ',StageMessage);
   ReportUnchanged(ExecutableFileName,PDBOutputFileName,false);
   ExitCode:=1;
   exit;
  end;
  if not CopyFileTo(ExecutableFileName,WorkFileName) then begin
   WriteLn('Error: ',ExecutableFileName,' could not be copied to work on.');
   ReportUnchanged(ExecutableFileName,PDBOutputFileName,false);
   ExitCode:=1;
   exit;
  end;
  OutputOk:=true;

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
   //
   // It is written beside its name and then put under it, keeping whatever was
   // there. The executable can be left untouched until the very end because
   // nothing has to read it under its real name in the meantime; the pdb cannot,
   // because dbghelp looks for it by the name the executable gives, in the
   // directory the executable is in. So the two are staged differently and
   // decided together: a run which ends up not replacing the executable puts
   // the old pdb back, since the pair which was there worked and the pair it
   // would otherwise leave behind is an executable naming an identity which the
   // pdb next to it no longer has.
   if length(PDBOutputFileName)>0 then begin
    if not AcquireTemporaryName(PDBOutputFileName,'mapsymbols-work',PDBWorkFileName,StageMessage) then begin
     WriteLn('Error: ',StageMessage);
     ReportUnchanged(ExecutableFileName,PDBOutputFileName,false);
     ExitCode:=1;
     exit;
    end;
    PDBWriter:=WritePDBFile(Builder,Image,PDBWorkFileName);
    if not StageFileOver(PDBOutputFileName,PDBWorkFileName,PDBBackupFileName,StageMessage,StageBothRemain) then begin
     WriteLn('Error: ',StageMessage);
     if StageBothRemain then begin
      // Neither name holds what it should and both files are where the message
      // says. Nothing here throws either of them away.
      PDBWorkFileName:='';
      PDBBackupFileName:='';
     end;
     ReportUnchanged(ExecutableFileName,PDBOutputFileName,false);
     ExitCode:=1;
     exit;
    end;
    PDBWorkFileName:='';
    PDBStaged:=true;
    WriteLn('Wrote ',PDBOutputFileName,'.');
   end;

   // Injection changes the size of the file, so it has to come first. The
   // appended table is found through a footer at the very end and would be
   // orphaned by anything written behind it.
   //
   // The DWARF sections only go in when that was actually asked for, even if a
   // writer exists because a separate debug file was requested as well.
   //
   // Sections which describe nothing are not a smaller version of the job, they
   // are the job not done, and they would leave the executable changed for
   // nothing. Said before the executable is touched rather than after.
   if InjectIntoExecutable and (DWARFWriter.DebugLine.Size=0) then begin
    WriteLn('Error: there are no line numbers to put into the executable.');
    WriteLn('Debug sections in an executable are read through their compilation units, and there are none without line numbers.');
    ReportUnchanged(ExecutableFileName,PDBOutputFileName,assigned(PDBWriter));
    ExitCode:=1;
    exit;
   end;

   if InjectIntoExecutable then begin
    // The result is looked at. Refusing is a normal outcome here, an executable
    // which is signed or already carries a symbol table being turned away, and
    // the message says so, but the run asked for these sections and did not get
    // them. Carrying on and finishing with a zero would report success for an
    // executable which does not hold what was asked for, and the build which
    // asked would go on believing it does.
    if not InjectDebugSections(Builder,DWARFWriter,PDBWriter,Image,ExtractFileName(PDBOutputFileName),WorkFileName) then begin
     WriteLn('Error: the debug sections which were asked for could not be put into the executable.');
     ReportUnchanged(ExecutableFileName,PDBOutputFileName,assigned(PDBWriter));
     ExitCode:=1;
     exit;
    end;
   end else if assigned(PDBWriter) then begin
    // The same for the entry which names the pdb. Without it the pdb is on
    // disk and no debugger will ever look at it, which is a run that produced
    // a file and not the thing the file is for.
    if not InjectDebugSections(Builder,nil,PDBWriter,Image,ExtractFileName(PDBOutputFileName),WorkFileName) then begin
     WriteLn('Error: the executable could not be made to name the pdb which was just written.');
     ReportUnchanged(ExecutableFileName,PDBOutputFileName,true);
     ExitCode:=1;
     exit;
    end;
   end;

   Builder.AppendToFile(WorkFileName);

   // The file is finished now, so a checksum which the linker put into it can
   // be worked out again over what it actually is. This has to come last: the
   // checksum covers the whole file, and everything before this was still
   // adding to it.
   //
   // Only asked of a PE, since only a PE has such a field. The function says so
   // itself now, but a run on an ELF has no business asking a question about a
   // header which is not there.
   //
   // The answer is looked at. False means there was a checksum and it could not
   // be replaced, which is a file going out stating one which no longer
   // describes it, and windows turns such a file away for anything which is
   // loaded as a driver or early in the boot path. That was the one thing
   // written here which nobody read the outcome of.
   if Image.Format=iffPE then begin
    if not UpdateImageCheckSum(WorkFileName) then begin
     WriteLn('Error: the checksum in the header could not be worked out again.');
     ExitCode:=1;
     OutputOk:=false;
    end else begin
     // And read back out of the finished file by the code which computes it
     // rather than by the code which wrote it, which is what everything else
     // written here gets. Four bytes, but they are the four a loader compares.
     if not VerifyImageCheckSum(WorkFileName,CheckSumMessage) then begin
      WriteLn('Checksum check FAILED: ',CheckSumMessage);
      ExitCode:=1;
      OutputOk:=false;
     end;
    end;
   end;

   if Builder.PackedTo>0 then begin
    WriteLn('Packed ',Builder.PackedFrom,' bytes of contents down to ',Builder.PackedTo,'.');
   end else if Compress then begin
    WriteLn('Not packed, since it would not have come out smaller.');
   end;

   // The separate debug file, written beside its name and only put under it
   // once it has been read back. It replaces nothing about the executable, but
   // it may well replace a debug file of its own from an earlier run, and that
   // one described a build somebody may still have. The same rule as
   // everywhere else here: nothing takes the place of anything before it has
   // been looked at.
   if length(DebugOutputFileName)>0 then begin
    if not AcquireTemporaryName(DebugOutputFileName,'mapsymbols-work',DebugWorkFileName,StageMessage) then begin
     WriteLn('Error: ',StageMessage);
     ExitCode:=1;
    end else if WriteDebugFile(Builder,DWARFWriter,Image,DebugWorkFileName) then begin
     if StageFileOver(DebugOutputFileName,DebugWorkFileName,DebugBackupFileName,StageMessage,StageBothRemain) then begin
      CommitStagedFile(DebugBackupFileName);
      DebugBackupFileName:='';
      DebugWorkFileName:='';
      WriteLn('Wrote ',DebugOutputFileName,' with ',DWARFWriter.DebugLine.Size,' bytes of line programs and ',
              DWARFWriter.DebugInfo.Size,' bytes of compile units.');
     end else begin
      WriteLn('Error: ',StageMessage);
      if StageBothRemain then begin
       DebugWorkFileName:='';
       DebugBackupFileName:='';
      end;
      ExitCode:=1;
     end;
    end else begin
     WriteLn('What was written into ',DebugWorkFileName,' did not read back correctly, so ',DebugOutputFileName,' was left alone.');
    end;
   end;

  finally
   FreeAndNil(PDBWriter);
   FreeAndNil(DWARFWriter);
  end;

  // The pdb is checked through dbghelp, which is a different reader than the
  // one which wrote it, and which also has to accept the debug directory in the
  // executable before it will look at the pdb at all.
  if length(PDBOutputFileName)>0 then begin
   if CheckPDBWithDbgHelp(Builder,WorkFileName,DbgHelpResolved,DbgHelpProbes,DbgHelpAvailable,DbgHelpRefused) then begin
    WriteLn('Debugger check: ',DbgHelpResolved,' of ',DbgHelpProbes,' probes resolved to the expected line.');
   end else if DbgHelpAvailable then begin
    WriteLn('Debugger check FAILED: ',DbgHelpResolved,' of ',DbgHelpProbes,' probes resolved to the expected line.');
    ExitCode:=1;
    OutputOk:=false;
   end else if (Builder.SymbolCount=0) and (Builder.LineCount=0) then begin
    // A run which asked for neither names nor line numbers wrote a pdb which
    // describes the sections of the image and nothing else, and there is
    // nothing in it to ask a debugger about. Said as what it is rather than
    // being folded into one of the two below, both of which would be a claim
    // about dbghelp which is not true.
    WriteLn('Debugger check skipped, the pdb holds neither routine names nor line numbers to ask about.');
   end else if DbgHelpRefused then begin
    // dbghelp is there and would not take the file. This is the only check
    // which asks a reader this tool did not write, so losing it costs more than
    // losing any of the others, and it used to be lost with a line which read
    // like an aside. A run which asked for a pdb and got no word from the one
    // reader which could say anything about it has not been checked.
    WriteLn('Debugger check FAILED: dbghelp would not take ',WorkFileName,', so the pdb was never looked at.');
    ExitCode:=1;
    OutputOk:=false;
   end else begin
    WriteLn('Debugger check skipped, there is no dbghelp here to ask.');
   end;
  end;

  if Builder.SelfCheck(WorkFileName,Resolved,Probes) then begin
   WriteLn('Self check: ',Resolved,' of ',Probes,' probes resolved to the expected line.');
  end else begin
   WriteLn('Self check FAILED: ',Resolved,' of ',Probes,' probes resolved to the expected line.');
   ExitCode:=1;
   OutputOk:=false;
  end;

  // And only now, when everything which was written into the copy has been read
  // back out of it, does it become the executable. A run which got this far and
  // found something wrong leaves the file it was given exactly as it was.
  //
  // The separate debug file is not part of this. It is a file of its own which
  // replaces nothing, so its failure is reported and costs the run its exit
  // code without deciding anything about the executable.
  if OutputOk then begin
   if not ReplaceFileWith(ExecutableFileName,WorkFileName,ReplaceMessage,BothRemain) then begin
    WriteLn('Error: ',ReplaceMessage);
    if BothRemain then begin
     // Neither name holds what it should. Both files are on disk and somebody
     // is going to finish this by hand, so the pdb which goes with the finished
     // file stays where it is rather than being put back: the run which was
     // taken over is the one which was going to keep it, and a finished
     // executable whose pdb has just been deleted is worth nothing.
     //
     // Both pdbs are left, and which goes with which is said, since that is the
     // whole question at this point.
     if PDBStaged then begin
      WriteLn('The pdb which was written is left at ',PDBOutputFileName,', which is the one the finished new file names.');
      if length(PDBBackupFileName)>0 then begin
       WriteLn('The pdb which was there before is at ',PDBBackupFileName,', which is the one the original names.');
      end;
      PDBStaged:=false;
      PDBBackupFileName:='';
     end;
    end else begin
     DeleteFile(WorkFileName);
    end;
    WorkFileName:='';
    ExitCode:=1;
   end else begin
    WorkFileName:='';
    // The executable is in place and names this pdb, so the one which was there
    // before it can go. Only now: everything before this point could still have
    // ended with the old executable staying where it was.
    if PDBStaged then begin
     CommitStagedFile(PDBBackupFileName);
     PDBStaged:=false;
     PDBBackupFileName:='';
    end;
   end;
  end else begin
   WriteLn('The executable was not changed, because what was written into the copy of it did not read back correctly.');
  end;

 finally
  // Whatever is left of the copy goes, on every way out of here including the
  // ones which gave up early.
  if length(WorkFileName)>0 then begin
   DeleteFile(WorkFileName);
  end;
  if length(PDBWorkFileName)>0 then begin
   DeleteFile(PDBWorkFileName);
  end;
  if length(DebugWorkFileName)>0 then begin
   DeleteFile(DebugWorkFileName);
  end;
  // And a pdb which was put in place for a run which then did not replace the
  // executable is taken back out again, whichever way that run ended. What was
  // beside the executable before belongs to the executable which is still
  // there; leaving the new one would mean an executable naming an identity
  // which the pdb next to it does not have, which is worse than either of the
  // two states this run could have ended in.
  if PDBStaged then begin
   if RollbackStagedFile(PDBOutputFileName,PDBBackupFileName) then begin
    if length(PDBBackupFileName)>0 then begin
     WriteLn('The pdb which was there before is back in place, since the executable was not changed.');
    end;
   end else begin
    WriteLn('Warning: ',PDBOutputFileName,' could not be put back the way it was.');
   end;
   PDBStaged:=false;
  end;
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
