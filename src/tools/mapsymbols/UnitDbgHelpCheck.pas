// Checks a written PDB through dbghelp, which is the library the Microsoft
// debuggers themselves use.
//
// The other self checks in this tool read back what was written with the same
// code that wrote it, which proves the file is self consistent but not that
// anybody else agrees with it. This one hands the executable to the operating
// system symbol handler and asks it to name an address and give its source
// line, so it tests the whole chain: the debug directory in the executable, the
// identity matching between it and the pdb, and the contents of the pdb.
//
// Only available on Windows, since dbghelp is part of it. Elsewhere the check
// reports that it could not run rather than pretending to have passed.
unit UnitDbgHelpCheck;
{$ifdef fpc}
 {$mode delphi}
{$endif}

interface

uses SysUtils,
     Classes,
{$ifdef Windows}
     Windows,
{$endif}
     PasVulkan.Types,
     UnitSymbolBuilder;

// Resolves a spread of the collected line records and compares the answers with
// what they were built from. Returns false when dbghelp is not available, which
// aAvailable then reports separately from an actual failure.
function CheckPDBWithDbgHelp(const aBuilder:TSymbolBuilder;const aExecutable:String;out aResolved,aProbes:TpvSizeInt;out aAvailable:Boolean):Boolean;

implementation

{$ifdef Windows}

const SYMOPT_UNDNAME=TpvUInt32($00000002);
      SYMOPT_LOAD_LINES=TpvUInt32($00000010);

type TDbgHelpSymbolInfo=record
      SizeOfStruct:TpvUInt32;
      TypeIndex:TpvUInt32;
      Reserved:array[0..1] of TpvUInt64;
      Index:TpvUInt32;
      Size:TpvUInt32;
      ModBase:TpvUInt64;
      Flags:TpvUInt32;
      // The layout has an alignment gap here, which has to be stated so that
      // the fields behind it land where dbghelp expects them.
      Padding:TpvUInt32;
      Value:TpvUInt64;
      Address:TpvUInt64;
      Register_:TpvUInt32;
      Scope:TpvUInt32;
      Tag:TpvUInt32;
      NameLen:TpvUInt32;
      MaxNameLen:TpvUInt32;
      Name:array[0..1023] of AnsiChar;
     end;

     TDbgHelpLine=record
      SizeOfStruct:TpvUInt32;
      Padding:TpvUInt32;
      Key:TpvPointer;
      LineNumber:TpvUInt32;
      FileName:PAnsiChar;
      Address:TpvUInt64;
     end;

function SymSetOptions(aOptions:TpvUInt32):TpvUInt32; stdcall; external 'dbghelp.dll' name 'SymSetOptions';
function SymInitialize(aProcess:THandle;aSearchPath:PAnsiChar;aInvade:LongBool):LongBool; stdcall; external 'dbghelp.dll' name 'SymInitialize';
function SymCleanup(aProcess:THandle):LongBool; stdcall; external 'dbghelp.dll' name 'SymCleanup';
function SymLoadModuleEx(aProcess,aFile:THandle;aImageName,aModuleName:PAnsiChar;aBase:TpvUInt64;aSize:TpvUInt32;aData:TpvPointer;aFlags:TpvUInt32):TpvUInt64; stdcall; external 'dbghelp.dll' name 'SymLoadModuleEx';
function SymFromAddr(aProcess:THandle;aAddress:TpvUInt64;aDisplacement:TpvPointer;aSymbol:TpvPointer):LongBool; stdcall; external 'dbghelp.dll' name 'SymFromAddr';
function SymGetLineFromAddr64(aProcess:THandle;aAddress:TpvUInt64;aDisplacement:TpvPointer;aLine:TpvPointer):LongBool; stdcall; external 'dbghelp.dll' name 'SymGetLineFromAddr64';
function SymFromName(aProcess:THandle;aName:PAnsiChar;aSymbol:TpvPointer):LongBool; stdcall; external 'dbghelp.dll' name 'SymFromName';

function CheckPDBWithDbgHelp(const aBuilder:TSymbolBuilder;const aExecutable:String;out aResolved,aProbes:TpvSizeInt;out aAvailable:Boolean):Boolean;
const PreferredBase=TpvUInt64($400000);
var Process:THandle;
    Base:TpvUInt64;
    Symbol:TDbgHelpSymbolInfo;
    Line:TDbgHelpLine;
    Displacement64:TpvUInt64;
    Displacement32:TpvUInt32;
    Index,Step,SymbolStep,NextIndex,Probe,Low,High:TpvSizeInt;
    Middle,UnitEnd:TpvUInt64;
    LineRecord:TSymbolBuilder.TLineRecord;
    SymbolRecord,NextSymbol:TSymbolBuilder.TSymbolRecord;
    UnitRecord:TSymbolBuilder.TUnitRecord;
    ImageName,SymbolName:TpvRawByteString;
begin

 aResolved:=0;
 aProbes:=0;
 aAvailable:=false;
 result:=false;

 // Not turned away for having no line numbers. The lookup by name below tests
 // the publics hash of the pdb, which has nothing to do with line information,
 // and a run which asked for symbols only used to skip the one check a foreign
 // reader performs on what it wrote.
 if (aBuilder.LineCount=0) and (aBuilder.SymbolCount=0) then begin
  exit;
 end;

 ImageName:=TpvRawByteString(ExpandFileName(aExecutable));

 Process:=GetCurrentProcess;
 SymSetOptions(SYMOPT_LOAD_LINES or SYMOPT_UNDNAME);
 if not SymInitialize(Process,nil,false) then begin
  exit;
 end;
 try

  // The module is loaded at the address it was linked for, so the addresses in
  // the table can be used as they are.
  Base:=SymLoadModuleEx(Process,0,PAnsiChar(ImageName),nil,PreferredBase,0,nil,0);
  if Base=0 then begin
   exit;
  end;

  aAvailable:=true;

  if aBuilder.LineCount>32 then begin
   Step:=aBuilder.LineCount div 32;
  end else begin
   Step:=1;
  end;

  Index:=0;
  while (Index<aBuilder.LineCount) and (aProbes<32) do begin

   LineRecord:=aBuilder.GetLine(Index);
   if LineRecord.LineNumber=0 then begin
    // An end of sequence marker, which is not a line and is not written into
    // the pdb either.
    inc(Index,Step);
    continue;
   end;
   inc(aProbes);

   FillChar(Symbol,SizeOf(Symbol),0);
   Symbol.SizeOfStruct:=88;
   Symbol.MaxNameLen:=1000;
   Displacement64:=0;
   SymFromAddr(Process,Base+LineRecord.RVA,@Displacement64,@Symbol);

   FillChar(Line,SizeOf(Line),0);
   Line.SizeOfStruct:=SizeOf(TDbgHelpLine);
   Displacement32:=0;
   if SymGetLineFromAddr64(Process,Base+LineRecord.RVA,@Displacement32,@Line) and
      (Line.LineNumber=LineRecord.LineNumber) then begin
    inc(aResolved);
   end;

   inc(Index,Step);

  end;

  // A lookup by address in the middle of a routine rather than at its door.
  // That is what a debugger does with a crash address, and it goes by the range
  // a routine describes rather than by an exact match, so it is the only thing
  // here which reads that range at all. A routine which claims ground belonging
  // to the next one answers this with the wrong name.
  // Spread over the whole table rather than taken from the front, since asking
  // dbghelp is not free and a few dozen answers say as much as a few thousand.
  if aBuilder.SymbolCount>64 then begin
   SymbolStep:=aBuilder.SymbolCount div 64;
  end else begin
   SymbolStep:=1;
  end;

  Index:=0;
  while (Index+1)<aBuilder.SymbolCount do begin
   SymbolRecord:=aBuilder.GetSymbol(Index);
   NextIndex:=Index+1;
   while (NextIndex<aBuilder.SymbolCount) and (aBuilder.GetSymbol(NextIndex).RVA=SymbolRecord.RVA) do begin
    inc(NextIndex);
   end;
   if NextIndex>=aBuilder.SymbolCount then begin
    break;
   end;
   NextSymbol:=aBuilder.GetSymbol(NextIndex);
   Middle:=SymbolRecord.RVA+((NextSymbol.RVA-SymbolRecord.RVA) shr 1);
   // Only where both ends are in the same unit range. A collected symbol is not
   // always a routine: a Delphi map names its data as well, and those sit below
   // the code with nothing between them but more data. The pdb describes a
   // routine for what lies inside a unit and nothing for the rest, so asking
   // about the space between two variables asks about something which was
   // deliberately not written.
   UnitEnd:=0;
   Low:=0;
   High:=aBuilder.UnitCount-1;
   while Low<=High do begin
    Probe:=Low+((High-Low) shr 1);
    UnitRecord:=aBuilder.GetUnit(Probe);
    if SymbolRecord.RVA<UnitRecord.StartRVA then begin
     High:=Probe-1;
    end else if SymbolRecord.RVA>=(UnitRecord.StartRVA+UnitRecord.Size) then begin
     Low:=Probe+1;
    end else begin
     UnitEnd:=UnitRecord.StartRVA+UnitRecord.Size;
     break;
    end;
   end;
   // A group of more than one name at an address is always probed, whatever the
   // stride says. Those are the aliases, and the ground behind them is exactly
   // where a range taken from the wrong neighbour goes wrong. With a hundred
   // probes spread over twenty thousand symbols, leaving them to chance means
   // never looking at the two addresses which matter.
   if (((NextIndex-Index)>1) or ((Index mod SymbolStep)=0)) and
      (Middle>SymbolRecord.RVA) and (UnitEnd>0) and (Middle<UnitEnd) then begin
    inc(aProbes);
    FillChar(Symbol,SizeOf(Symbol),0);
    Symbol.SizeOfStruct:=88;
    Symbol.MaxNameLen:=1000;
    Displacement64:=0;
    // Held against the address it names rather than against the name itself.
    // dbghelp is asked to undecorate, so what comes back is not the string
    // which went in: a Delphi name loses its leading at sign on the way out.
    // The address is the thing being tested anyway, since what this probe is
    // about is which routine an address in the middle of one belongs to.
    if SymFromAddr(Process,Base+Middle,@Displacement64,@Symbol) and
       (Symbol.Address=(Base+SymbolRecord.RVA)) then begin
     inc(aResolved);
    end;
   end;
   Index:=NextIndex;
  end;

  // A lookup by name goes through the hash table of the publics stream rather
  // than the address map, so it tests a part nothing above touches. A wrong
  // bucket offset there fails exactly here and nowhere else.
  if aBuilder.SymbolCount>0 then begin
   SymbolRecord:=aBuilder.GetSymbol(aBuilder.SymbolCount div 2);
   SymbolName:=TpvRawByteString(String(SymbolRecord.Name));
   if length(SymbolName)>0 then begin
    inc(aProbes);
    FillChar(Symbol,SizeOf(Symbol),0);
    Symbol.SizeOfStruct:=88;
    Symbol.MaxNameLen:=1000;
    if SymFromName(Process,PAnsiChar(SymbolName),@Symbol) and
       (Symbol.Address=(Base+SymbolRecord.RVA)) then begin
     inc(aResolved);
    end;
   end;
  end;

  // Nothing asked is not a pass, the same as everywhere else here.
  result:=(aProbes>0) and (aResolved=aProbes);

 finally
  SymCleanup(Process);
 end;

end;

{$else}

function CheckPDBWithDbgHelp(const aBuilder:TSymbolBuilder;const aExecutable:String;out aResolved,aProbes:TpvSizeInt;out aAvailable:Boolean):Boolean;
begin
 aResolved:=0;
 aProbes:=0;
 aAvailable:=false;
 result:=false;
end;

{$endif}

end.
