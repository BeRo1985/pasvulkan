(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2024, Benjamin Rosseaux (benjamin@rosseaux.de)          *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 *                                                                            *
 ******************************************************************************
 *                  General guidelines for code contributors                  *
 *============================================================================*
 *                                                                            *
 * 1. Make sure you are legally allowed to make a contribution under the zlib *
 *    license.                                                                *
 * 2. The zlib license header goes at the top of each source file, with       *
 *    appropriate copyright notice.                                           *
 * 3. This PasVulkan wrapper may be used only with the PasVulkan-own Vulkan   *
 *    Pascal header.                                                          *
 * 4. After a pull request, check the status of your pull request on          *
      http://github.com/BeRo1985/pasvulkan                                    *
 * 5. Write code which's compatible with Delphi >= 2009 and FreePascal >=     *
 *    3.1.1                                                                   *
 * 6. Don't use Delphi-only, FreePascal-only or Lazarus-only libraries/units, *
 *    but if needed, make it out-ifdef-able.                                  *
 * 7. No use of third-party libraries/units as possible, but if needed, make  *
 *    it out-ifdef-able.                                                      *
 * 8. Try to use const when possible.                                         *
 * 9. Make sure to comment out writeln, used while debugging.                 *
 * 10. Make sure the code compiles on 32-bit and 64-bit platforms (x86-32,    *
 *     x86-64, ARM, ARM64, etc.).                                             *
 * 11. Make sure the code runs on all platforms with Vulkan support           *
 *                                                                            *
 ******************************************************************************)
unit PasVulkan.CrashReport;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

uses {$if defined(Windows)}
      Windows,
     {$ifend}
     {$if defined(fpc) and not (defined(PasVulkanWithoutLineInfo) or defined(Android))}
      // Without one of these units linked in, BackTraceStrFunc stays the default
      // SysBackTraceStr, which prints bare addresses and nothing else. They only
      // need to be in the uses list of any unit, since their initialization is
      // what replaces BackTraceStrFunc, and they degrade back to bare addresses
      // when the binary carries no debug information at all.
      {$ifdef PasVulkanUseStabsLineInfo}
       lineinfo,
      {$else}
       lnfodwrf,
      {$endif}
     {$ifend}
     SysUtils,
     Classes,
     PasVulkan.Types,
     PasVulkan.SymbolTable;

// This unit provides the low level crash capture backend of PasVulkan. It sits
// deliberately at the same dependency depth as PasVulkan.Utils, so that its
// initialization runs long before PasVulkan.Application is initialized, since
// PasVulkan.Application is at the very top of the unit dependency graph and
// would therefore miss every fault which happens while the units below it are
// still initializing themselves.
//
// The first chance mechanism differs per compiler and per operating system:
//
//   Delphi, all platforms  : Exception.GetExceptionStackInfoProc and friends.
//                            These fire at the raise point for language level
//                            raises and, through GetExceptionObject, also for
//                            hardware faults.
//   FreePascal on Windows  : System.RaiseProc is never called there, because
//                            rtl/win32/system.pp and rtl/win64/system.pp both
//                            define FPC_SYSTEM_HAS_RAISEEXCEPTION, which drops
//                            the only call site in rtl/inc/except.inc. So a
//                            vectored exception handler is the only option.
//   FreePascal elsewhere   : System.RaiseProc works, and it also covers
//                            hardware faults, since the POSIX signal handlers
//                            of the RTL turn those into ordinary exceptions.
//
// Everything lands in a lock free ring buffer, which is formatted only when a
// crash is actually being reported. That way the common case of an expected and
// immediately caught exception costs one ring buffer slot and nothing else.

const pvCrashReportRingBufferSize=64; // Must stay a power of two

      pvCrashReportEntryTextSize=1024;

      pvCrashReportKindRaise=TpvUInt32(0); // Language level raise, seen at the raise point

      pvCrashReportKindFault=TpvUInt32(1); // Operating system level fault, seen before any handler

      pvCrashReportKindNote=TpvUInt32(2); // Manually added note

type PpvCrashReportEntry=^TpvCrashReportEntry;
     TpvCrashReportEntry=record
      Sequence:TpvUInt64; // Zero while the entry is being written
      ThreadID:TpvUInt64;
      Address:TpvPointer;
      Kind:TpvUInt32;
      Code:TpvUInt32;
      TextLength:TpvInt32;
      Text:array[0..pvCrashReportEntryTextSize-1] of AnsiChar;
     end;

// Adds a manually formatted entry to the ring buffer. Safe to call at any time,
// but not from inside a vectored exception handler, since it works with managed
// strings.
procedure pvCrashReportNote(const aKind,aCode:TpvUInt32;const aAddress:TpvPointer;const aText:String);

// Formats a single code address as readable as the current build allows.
//
// aReturnAddress tells whether the address came back from a stack walk or is
// otherwise the address behind a call, which is the usual case. Such an address
// already belongs to the following statement, so the lookup has to be done one
// byte earlier to land on the line which actually made the call. Pass false for
// an address which is the faulting instruction itself, as reported for a
// hardware fault.
function pvCrashReportFormatAddress(const aAddress:TpvPointer;const aReturnAddress:Boolean=true):String;

// Captures the stack of the calling thread, skipping the given number of frames.
function pvCrashReportCaptureStackTrace(const aFramesToSkip:TpvInt32=1):String;

// Formats the recorded first chance history, oldest entry first.
function pvCrashReportHistory(const aMaximalCount:TpvInt32=pvCrashReportRingBufferSize):String;

// The single implementation behind every DumpExceptionCallStack in PasVulkan.
// The frame arguments are only used by FreePascal and are ignored by Delphi.
function pvCrashReportDumpException(const aException:Exception;const aAddress:TpvPointer=nil;const aFrameCount:TpvInt32=0;const aFrames:PPointer=nil):String;

procedure pvCrashReportInstall;

procedure pvCrashReportUninstall;

implementation

const LineEnding={$if defined(Windows)}#13#10{$else}#10{$ifend};

      HexDigits:array[0..15] of AnsiChar='0123456789abcdef';

      cDelphiException=TpvUInt32($0eedfade);

      cFPCException=TpvUInt32($e0465043);

      cAccessViolation=TpvUInt32($c0000005);

      cMaximalStackFrames=48;

type PpvCrashReportStackInfo=^TpvCrashReportStackInfo;
     // What the Delphi raise point hook hands back to the RTL. It deliberately
     // holds raw addresses only, so that the cost at the raise point stays a
     // stack walk and nothing more. Turning those into names happens in
     // GetStackInfoStringProc, so only for an exception whose stack somebody
     // actually reads, rather than for every single raise in the program.
     TpvCrashReportStackInfo=record
      Count:TpvInt32;
      Addresses:array[0..cMaximalStackFrames-1] of TpvPointer;
     end;

     PpvCrashReportNativeExceptionRecord=^TpvCrashReportNativeExceptionRecord;
     TpvCrashReportNativeExceptionRecord=record
      ExceptionCode:TpvUInt32;
      ExceptionFlags:TpvUInt32;
      ExceptionRecord:PpvCrashReportNativeExceptionRecord;
      ExceptionAddress:TpvPointer;
      NumberParameters:TpvUInt32;
      ExceptionInformation:array[0..14] of TpvPtrUInt;
     end;

     PpvCrashReportNativeExceptionPointers=^TpvCrashReportNativeExceptionPointers;
     TpvCrashReportNativeExceptionPointers=record
      ExceptionRecord:PpvCrashReportNativeExceptionRecord;
      ContextRecord:TpvPointer;
     end;

{$if defined(Windows)}
function RtlCaptureStackBackTrace(aFramesToSkip:TpvUInt32;aFramesToCapture:TpvUInt32;aBackTrace:PPointer;aBackTraceHash:PpvUInt32):TpvUInt16; stdcall; external 'kernel32.dll' name 'RtlCaptureStackBackTrace';
{$ifend}

{$if defined(fpc) and defined(Windows)}
function AddVectoredExceptionHandler(aFirst:TpvUInt32;aHandler:TpvPointer):TpvPointer; stdcall; external 'kernel32.dll' name 'AddVectoredExceptionHandler';

function RemoveVectoredExceptionHandler(aHandle:TpvPointer):TpvUInt32; stdcall; external 'kernel32.dll' name 'RemoveVectoredExceptionHandler';
{$ifend}

var CrashReportRingBuffer:array[0..pvCrashReportRingBufferSize-1] of TpvCrashReportEntry;
    CrashReportSequence:TpvInt64=0;
    CrashReportInstalled:Boolean=false;
    CrashReportSymbolTable:TpvSymbolTable=nil;
    CrashReportSymbolTableState:TpvInt32=0;
{$if defined(Windows)}
    CrashReportModuleBase:TpvPtrUInt=0;
{$ifend}
{$ifndef fpc}
    CrashReportOwnsStackInfoProcs:Boolean=false;
{$endif}
{$if defined(fpc) and defined(Windows)}
    CrashReportVectoredHandle:TpvPointer=nil;
{$elseif defined(fpc)}
    CrashReportOldRaiseProc:TExceptProc=nil;
{$ifend}

{$if not (defined(fpc) and defined(Windows))}
// Guards the managed string based handlers against reentering themselves, which
// would otherwise turn one failing handler into an endless cascade. Deliberately
// absent on FreePascal for Windows, where the only handler is the vectored one,
// which must never touch a threadvar, since it can run on threads the RTL has
// never seen and whose thread local storage is therefore not initialized.
threadvar CrashReportInsideHandler:Boolean;
{$ifend}

function CrashReportNextSequence:TpvUInt64;
begin
{$ifdef fpc}
 result:=TpvUInt64(InterLockedIncrement64(CrashReportSequence));
{$else}
 result:=TpvUInt64(AtomicIncrement(CrashReportSequence));
{$endif}
end;

function CrashReportCurrentThreadID:TpvUInt64;
begin
{$if defined(Windows)}
 result:=TpvUInt64(GetCurrentThreadId);
{$elseif defined(fpc)}
 result:=TpvUInt64(TpvPtrUInt(GetThreadID));
{$else}
 result:=TpvUInt64(TThread.CurrentThread.ThreadID);
{$ifend}
end;

procedure CrashReportEntryBegin(const aEntry:PpvCrashReportEntry;const aKind,aCode:TpvUInt32;const aAddress:TpvPointer;const aThreadID:TpvUInt64);
begin
 aEntry^.Sequence:=0;
 aEntry^.ThreadID:=aThreadID;
 aEntry^.Address:=aAddress;
 aEntry^.Kind:=aKind;
 aEntry^.Code:=aCode;
 aEntry^.TextLength:=0;
 aEntry^.Text[0]:=#0;
end;

procedure CrashReportEntryAppendChars(const aEntry:PpvCrashReportEntry;const aChars:PAnsiChar;const aCount:TpvInt32);
var Index:TpvInt32;
begin
 if assigned(aChars) then begin
  Index:=0;
  while (Index<aCount) and (aEntry^.TextLength<(pvCrashReportEntryTextSize-1)) do begin
   aEntry^.Text[aEntry^.TextLength]:=aChars[Index];
   inc(aEntry^.TextLength);
   inc(Index);
  end;
  aEntry^.Text[aEntry^.TextLength]:=#0;
 end;
end;

procedure CrashReportEntryAppendPAnsiChar(const aEntry:PpvCrashReportEntry;const aValue:PAnsiChar);
var Count:TpvInt32;
begin
 if assigned(aValue) then begin
  Count:=0;
  while (Count<pvCrashReportEntryTextSize) and (aValue[Count]<>#0) do begin
   inc(Count);
  end;
  CrashReportEntryAppendChars(aEntry,aValue,Count);
 end;
end;

procedure CrashReportEntryAppendHex(const aEntry:PpvCrashReportEntry;const aValue:TpvUInt64;const aDigits:TpvInt32);
var Buffer:array[0..15] of AnsiChar;
    Index,Digits:TpvInt32;
begin
 Digits:=aDigits;
 if Digits<1 then begin
  Digits:=1;
 end else if Digits>16 then begin
  Digits:=16;
 end;
 for Index:=0 to Digits-1 do begin
  Buffer[(Digits-1)-Index]:=HexDigits[(aValue shr (Index shl 2)) and $f];
 end;
 CrashReportEntryAppendChars(aEntry,@Buffer[0],Digits);
end;

procedure CrashReportEntryAppendString(const aEntry:PpvCrashReportEntry;const aValue:String);
{$ifdef fpc}
begin
 // Under FreePascal a String is an AnsiString which already holds UTF-8 bytes,
 // so they can be copied verbatim.
 if length(aValue)>0 then begin
  CrashReportEntryAppendChars(aEntry,PAnsiChar(aValue),length(aValue));
 end;
end;
{$else}
var Index:TpvInt32;
    CodeUnit:TpvUInt32;
begin
 // Under Delphi a String is a UnicodeString, so it needs to be converted to
 // UTF-8 here, without allocating anything on the way.
 for Index:=1 to length(aValue) do begin
  if aEntry^.TextLength>=(pvCrashReportEntryTextSize-4) then begin
   break;
  end;
  CodeUnit:=TpvUInt32(ord(aValue[Index]));
  if CodeUnit<$80 then begin
   aEntry^.Text[aEntry^.TextLength]:=AnsiChar(TpvUInt8(CodeUnit));
   inc(aEntry^.TextLength);
  end else if CodeUnit<$800 then begin
   aEntry^.Text[aEntry^.TextLength]:=AnsiChar(TpvUInt8($c0 or (CodeUnit shr 6)));
   aEntry^.Text[aEntry^.TextLength+1]:=AnsiChar(TpvUInt8($80 or (CodeUnit and $3f)));
   inc(aEntry^.TextLength,2);
  end else begin
   aEntry^.Text[aEntry^.TextLength]:=AnsiChar(TpvUInt8($e0 or (CodeUnit shr 12)));
   aEntry^.Text[aEntry^.TextLength+1]:=AnsiChar(TpvUInt8($80 or ((CodeUnit shr 6) and $3f)));
   aEntry^.Text[aEntry^.TextLength+2]:=AnsiChar(TpvUInt8($80 or (CodeUnit and $3f)));
   inc(aEntry^.TextLength,3);
  end;
 end;
 aEntry^.Text[aEntry^.TextLength]:=#0;
end;
{$endif}

procedure pvCrashReportNote(const aKind,aCode:TpvUInt32;const aAddress:TpvPointer;const aText:String);
var Entry:PpvCrashReportEntry;
    Sequence:TpvUInt64;
begin
 Sequence:=CrashReportNextSequence;
 Entry:=@CrashReportRingBuffer[(Sequence-1) and (pvCrashReportRingBufferSize-1)];
 CrashReportEntryBegin(Entry,aKind,aCode,aAddress,CrashReportCurrentThreadID);
 CrashReportEntryAppendString(Entry,aText);
 Entry^.Sequence:=Sequence;
end;

{$if defined(Windows)}
// The symbol table is only read when an address actually has to be formatted,
// which normally means a crash is already being reported. Loading it eagerly at
// startup would mean paying for a table which is many megabytes on a large
// project, in every run, for nothing.
function CrashReportGetSymbolTable:TpvSymbolTable;
var Previous:TpvInt32;
begin
{$ifdef fpc}
 Previous:=InterLockedExchange(CrashReportSymbolTableState,1);
{$else}
 Previous:=AtomicExchange(CrashReportSymbolTableState,1);
{$endif}
 if Previous=0 then begin
  try
   CrashReportModuleBase:=TpvPtrUInt(GetModuleHandle(nil));
   CrashReportSymbolTable:=TpvSymbolTable.Create;
   if not CrashReportSymbolTable.LoadFromFile(ParamStr(0)) then begin
    // No table appended, which is the normal case for a build the mapsymbols
    // tool has not been run on.
    FreeAndNil(CrashReportSymbolTable);
   end;
  except
   FreeAndNil(CrashReportSymbolTable);
  end;
 end;
 result:=CrashReportSymbolTable;
end;

// Returns false when there is no table, or when the address does not belong to
// the executable itself, for example because it points into a system library.
function CrashReportResolveAddress(const aAddress:TpvPointer;const aReturnAddress:Boolean;out aText:String):Boolean;
var SymbolTable:TpvSymbolTable;
    Location:TpvSymbolTableLocation;
    LookupAddress:TpvPtrUInt;
begin
 result:=false;
 aText:='';
 SymbolTable:=CrashReportGetSymbolTable;
 if not assigned(SymbolTable) then begin
  exit;
 end;
 if (CrashReportModuleBase=0) or (TpvPtrUInt(aAddress)<=CrashReportModuleBase) then begin
  exit;
 end;
 LookupAddress:=TpvPtrUInt(aAddress);
 if aReturnAddress then begin
  // One byte back, so that the lookup lands inside the calling instruction
  // rather than on the statement which follows it.
  dec(LookupAddress);
 end;
 // The table stores image relative addresses, so subtracting the actual load
 // address here is what makes this correct under address space layout
 // randomization.
 if not SymbolTable.Resolve(TpvUInt64(LookupAddress-CrashReportModuleBase),Location) then begin
  exit;
 end;
 aText:='$'+IntToHex(TpvPtrUInt(aAddress),SizeOf(TpvPointer) shl 1);
 if length(Location.SymbolName)>0 then begin
  aText:=aText+'  '+String(Location.SymbolName);
 end else if length(Location.UnitName)>0 then begin
  aText:=aText+'  '+String(Location.UnitName);
 end;
 if Location.LineNumber>0 then begin
  aText:=aText+', line '+IntToStr(Location.LineNumber);
  if length(Location.FileName)>0 then begin
   aText:=aText+' of '+String(Location.FileName);
  end;
 end;
 result:=true;
end;
{$ifend}

function CrashReportFormatAddressFallback(const aAddress:TpvPointer):String;
{$ifdef fpc}
begin
 // With the lnfodwrf unit linked in, this resolves to unit, file and line,
 // otherwise it degrades to a bare address. BackTraceStrFunc indents its result
 // for direct printing, which would collide with the indentation the callers
 // here apply themselves.
 result:=Trim(String(BackTraceStrFunc(aAddress)));
end;
{$else}
var MemoryInformation:TMemoryBasicInformation;
    ModuleFileName:array[0..MAX_PATH] of Char;
begin
 result:='$'+IntToHex(TpvPtrUInt(aAddress),SizeOf(TpvPointer) shl 1);
 FillChar(MemoryInformation,SizeOf(TMemoryBasicInformation),#0);
 if (VirtualQuery(aAddress,MemoryInformation,SizeOf(TMemoryBasicInformation))<>0) and
    assigned(MemoryInformation.AllocationBase) then begin
  FillChar(ModuleFileName,SizeOf(ModuleFileName),#0);
  if GetModuleFileName(HMODULE(TpvPtrUInt(MemoryInformation.AllocationBase)),@ModuleFileName[0],MAX_PATH)<>0 then begin
   result:=result+' ('+ExtractFileName(String(PChar(@ModuleFileName[0])))+'+$'+
           IntToHex(TpvPtrUInt(aAddress)-TpvPtrUInt(MemoryInformation.AllocationBase),8)+')';
  end;
 end;
end;
{$endif}

function pvCrashReportFormatAddress(const aAddress:TpvPointer;const aReturnAddress:Boolean):String;
begin
{$if defined(Windows)}
 // An appended symbol table wins, since it is the only source which can name a
 // source file and line in a Delphi build. Under FreePascal it is normally
 // absent and lnfodwrf does the job in the fallback below.
 if CrashReportResolveAddress(aAddress,aReturnAddress,result) then begin
  exit;
 end;
{$ifend}
 result:=CrashReportFormatAddressFallback(aAddress);
end;

function pvCrashReportCaptureStackTrace(const aFramesToSkip:TpvInt32):String;
{$if defined(Windows)}
var Frames:array[0..cMaximalStackFrames-1] of TpvPointer;
    Count,Index:TpvInt32;
begin
 result:='';
 FillChar(Frames,SizeOf(Frames),#0);
 Count:=RtlCaptureStackBackTrace(TpvUInt32(aFramesToSkip+1),cMaximalStackFrames,@Frames[0],nil);
 for Index:=0 to Count-1 do begin
  result:=result+'  '+pvCrashReportFormatAddress(Frames[Index])+LineEnding;
 end;
end;
{$elseif defined(fpc)}
var CurrentFrame,PreviousFrame:TpvPointer;
    Index:TpvInt32;
begin
 result:='';
 CurrentFrame:=get_caller_frame(get_frame);
 Index:=0;
 while assigned(CurrentFrame) do begin
  if Index>=aFramesToSkip then begin
   result:=result+'  '+pvCrashReportFormatAddress(get_caller_addr(CurrentFrame))+LineEnding;
  end;
  inc(Index);
  PreviousFrame:=CurrentFrame;
  CurrentFrame:=get_caller_frame(CurrentFrame);
  if (TpvPtrUInt(CurrentFrame)<=TpvPtrUInt(PreviousFrame)) or
     (TpvPtrUInt(CurrentFrame)>(TpvPtrUInt(StackBottom)+TpvPtrUInt(StackLength))) then begin
   break;
  end;
 end;
end;
{$else}
begin
 result:='';
end;
{$ifend}

function CrashReportKindToString(const aKind:TpvUInt32):String;
begin
 case aKind of
  pvCrashReportKindRaise:begin
   result:='raise';
  end;
  pvCrashReportKindFault:begin
   result:='fault';
  end;
  else begin
   result:='note ';
  end;
 end;
end;

function pvCrashReportHistory(const aMaximalCount:TpvInt32):String;
var Index,Count:TpvInt32;
    Newest,Wanted:TpvUInt64;
    Entry:PpvCrashReportEntry;
begin
 result:='';
 Newest:=TpvUInt64(CrashReportSequence);
 if Newest=0 then begin
  exit;
 end;
 Count:=aMaximalCount;
 if Count>pvCrashReportRingBufferSize then begin
  Count:=pvCrashReportRingBufferSize;
 end;
 if TpvUInt64(Count)>Newest then begin
  Count:=TpvInt32(Newest);
 end;
 result:='First chance exception history, oldest first, at most '+IntToStr(Count)+' entries:'+LineEnding;
 for Index:=Count-1 downto 0 do begin
  Wanted:=Newest-TpvUInt64(Index);
  Entry:=@CrashReportRingBuffer[(Wanted-1) and (pvCrashReportRingBufferSize-1)];
  // A mismatching sequence means the entry is either still being written or has
  // already been overwritten by a newer one while this was being formatted.
  if Entry^.Sequence<>Wanted then begin
   continue;
  end;
  result:=result+'  #'+IntToStr(Entry^.Sequence)+
                 ' thread $'+IntToHex(Entry^.ThreadID,8)+
                 ' '+CrashReportKindToString(Entry^.Kind);
  if Entry^.Code<>0 then begin
   result:=result+' code $'+IntToHex(Entry^.Code,8);
  end;
  if assigned(Entry^.Address) then begin
   // A hardware fault reports the faulting instruction itself, while the raise
   // address of a language level exception is the address behind the call.
   result:=result+' at '+pvCrashReportFormatAddress(Entry^.Address,Entry^.Kind<>pvCrashReportKindFault);
  end;
  if Entry^.TextLength>0 then begin
   result:=result+' : '+String(PAnsiChar(@Entry^.Text[0]));
  end;
  result:=result+LineEnding;
 end;
end;

function pvCrashReportDumpException(const aException:Exception;const aAddress:TpvPointer;const aFrameCount:TpvInt32;const aFrames:PPointer):String;
{$ifdef fpc}
var Index,FrameCount:TpvInt32;
    Frames:PPointer;
{$else}
var StackTrace:String;
{$endif}
begin
 result:='Program exception!'+LineEnding+'Stack trace:'+LineEnding+LineEnding;
 if assigned(aException) then begin
  result:=result+'Exception class: '+aException.ClassName+LineEnding+
                 'Message: '+aException.Message+LineEnding;
 end;
{$ifdef fpc}
 if assigned(aAddress) then begin
  result:=result+String(BackTraceStrFunc(aAddress))+LineEnding;
 end else begin
  result:=result+String(BackTraceStrFunc(ExceptAddr))+LineEnding;
 end;
 if assigned(aFrames) and (aFrameCount>0) then begin
  Frames:=aFrames;
  FrameCount:=aFrameCount;
 end else begin
  Frames:=ExceptFrames;
  FrameCount:=ExceptFrameCount;
 end;
 if assigned(Frames) then begin
  for Index:=0 to FrameCount-1 do begin
   // The frame value itself must be dereferenced here. Passing the slot pointer
   // instead, as the previous copies of this routine did, formats the address of
   // the frame array element rather than the return address it holds.
   result:=result+String(BackTraceStrFunc(Frames^))+LineEnding;
   inc(Frames);
  end;
 end;
{$else}
 if assigned(aException) then begin
  StackTrace:=aException.StackTrace;
 end else begin
  StackTrace:='';
 end;
 if length(StackTrace)>0 then begin
  result:=result+StackTrace;
  if StackTrace[length(StackTrace)]<>#10 then begin
   result:=result+LineEnding;
  end;
 end else begin
  // No stack was captured at the raise point, so this is the stack of the
  // handler rather than of the raise. Say so, instead of quietly pretending
  // otherwise.
  result:=result+'No stack trace was captured at the raise point, showing the handler stack instead:'+LineEnding+
                 pvCrashReportCaptureStackTrace(2);
 end;
{$endif}
end;

{$ifndef fpc}
procedure CrashReportNoteNativeExceptionRecord(const aExceptionRecord:PpvCrashReportNativeExceptionRecord);
var ExceptionObject:TObject;
    Text:String;
begin
 if not assigned(aExceptionRecord) then begin
  exit;
 end;
 if aExceptionRecord^.ExceptionCode=cDelphiException then begin
  Text:='';
  if aExceptionRecord^.NumberParameters>=2 then begin
   ExceptionObject:=TObject(aExceptionRecord^.ExceptionInformation[1]);
   if assigned(ExceptionObject) then begin
    Text:=ExceptionObject.ClassName;
    if ExceptionObject is Exception then begin
     Text:=Text+': '+Exception(ExceptionObject).Message;
    end;
   end;
  end;
  if length(Text)=0 then begin
   Text:='Unknown exception object';
  end;
  pvCrashReportNote(pvCrashReportKindRaise,0,aExceptionRecord^.ExceptionAddress,Text);
 end else begin
  pvCrashReportNote(pvCrashReportKindFault,
                    aExceptionRecord^.ExceptionCode,
                    aExceptionRecord^.ExceptionAddress,
                    'Operating system fault');
 end;
end;

function CrashReportGetExceptionStackInfoProc(P:PExceptionRecord):Pointer;
var StackInfo:PpvCrashReportStackInfo;
begin
 result:=nil;
 if CrashReportInsideHandler then begin
  exit;
 end;
 CrashReportInsideHandler:=true;
 try
  try
   CrashReportNoteNativeExceptionRecord(PpvCrashReportNativeExceptionRecord(P));
   GetMem(StackInfo,SizeOf(TpvCrashReportStackInfo));
   FillChar(StackInfo^,SizeOf(TpvCrashReportStackInfo),#0);
   StackInfo^.Count:=RtlCaptureStackBackTrace(2,cMaximalStackFrames,@StackInfo^.Addresses[0],nil);
   if StackInfo^.Count>0 then begin
    result:=StackInfo;
   end else begin
    FreeMem(StackInfo);
   end;
  except
   // A failing capture must never turn into a second exception on top of the
   // one which is currently being raised.
   result:=nil;
  end;
 finally
  CrashReportInsideHandler:=false;
 end;
end;

function CrashReportGetStackInfoStringProc(aInfo:Pointer):String;
var StackInfo:PpvCrashReportStackInfo;
    Index:TpvInt32;
begin
 result:='';
 StackInfo:=PpvCrashReportStackInfo(aInfo);
 if assigned(StackInfo) then begin
  for Index:=0 to StackInfo^.Count-1 do begin
   if assigned(StackInfo^.Addresses[Index]) then begin
    result:=result+'  '+pvCrashReportFormatAddress(StackInfo^.Addresses[Index])+LineEnding;
   end;
  end;
 end;
end;

procedure CrashReportCleanUpStackInfoProc(aInfo:Pointer);
begin
 if assigned(aInfo) then begin
  FreeMem(aInfo);
 end;
end;
{$endif}

{$if defined(fpc) and defined(Windows)}
function CrashReportVectoredExceptionHandler(aExceptionInformation:PpvCrashReportNativeExceptionPointers):TpvInt32; stdcall;
const EXCEPTION_CONTINUE_SEARCH=TpvInt32(0);
var ExceptionRecord:PpvCrashReportNativeExceptionRecord;
    Entry:PpvCrashReportEntry;
    Sequence:TpvUInt64;
    ExceptionObject:TObject;
    ClassNameString:PShortString;
begin
 // This runs before any SEH frame of the program gets a chance to look at the
 // exception, on any thread, including threads which were never created by the
 // RTL. So it must not allocate, must not raise and must not change the flow.
 result:=EXCEPTION_CONTINUE_SEARCH;
 if not assigned(aExceptionInformation) then begin
  exit;
 end;
 ExceptionRecord:=aExceptionInformation^.ExceptionRecord;
 if not assigned(ExceptionRecord) then begin
  exit;
 end;
 Sequence:=CrashReportNextSequence;
 Entry:=@CrashReportRingBuffer[(Sequence-1) and (pvCrashReportRingBufferSize-1)];
 if ExceptionRecord^.ExceptionCode=cFPCException then begin
  // fpc_RaiseException passes address, object, frame count and frames, see
  // rtl/win64/seh64.inc and rtl/win32/seh32.inc.
  CrashReportEntryBegin(Entry,pvCrashReportKindRaise,0,ExceptionRecord^.ExceptionAddress,TpvUInt64(GetCurrentThreadId));
  if ExceptionRecord^.NumberParameters>=2 then begin
   Entry^.Address:=TpvPointer(ExceptionRecord^.ExceptionInformation[0]);
   ExceptionObject:=TObject(ExceptionRecord^.ExceptionInformation[1]);
   if assigned(ExceptionObject) then begin
    ClassNameString:=PShortString(PPointer(TpvPtrUInt(TpvPtrUInt(ExceptionObject.ClassType)+TpvPtrUInt(vmtClassName)))^);
    if assigned(ClassNameString) then begin
     CrashReportEntryAppendChars(Entry,@ClassNameString^[1],length(ClassNameString^));
    end;
    if ExceptionObject is Exception then begin
     CrashReportEntryAppendPAnsiChar(Entry,': ');
     // Casting the string field to a pointer reads it without touching its
     // reference count and without allocating a temporary.
     CrashReportEntryAppendPAnsiChar(Entry,PAnsiChar(TpvPointer(Exception(ExceptionObject).Message)));
    end;
   end;
  end;
 end else begin
  CrashReportEntryBegin(Entry,pvCrashReportKindFault,ExceptionRecord^.ExceptionCode,ExceptionRecord^.ExceptionAddress,TpvUInt64(GetCurrentThreadId));
  CrashReportEntryAppendPAnsiChar(Entry,'Operating system fault code $');
  CrashReportEntryAppendHex(Entry,ExceptionRecord^.ExceptionCode,8);
  if (ExceptionRecord^.ExceptionCode=cAccessViolation) and (ExceptionRecord^.NumberParameters>=2) then begin
   case ExceptionRecord^.ExceptionInformation[0] of
    0:begin
     CrashReportEntryAppendPAnsiChar(Entry,', read of $');
    end;
    1:begin
     CrashReportEntryAppendPAnsiChar(Entry,', write of $');
    end;
    else begin
     CrashReportEntryAppendPAnsiChar(Entry,', execute of $');
    end;
   end;
   CrashReportEntryAppendHex(Entry,TpvUInt64(ExceptionRecord^.ExceptionInformation[1]),SizeOf(TpvPointer) shl 1);
  end;
 end;
 Entry^.Sequence:=Sequence;
end;
{$elseif defined(fpc)}
procedure CrashReportRaiseProc(aObject:TObject;aAddress:CodePointer;aFrameCount:Longint;aFrames:PCodePointer);
begin
 if not CrashReportInsideHandler then begin
  CrashReportInsideHandler:=true;
  try
   try
    if assigned(aObject) then begin
     if aObject is Exception then begin
      pvCrashReportNote(pvCrashReportKindRaise,0,aAddress,aObject.ClassName+': '+Exception(aObject).Message);
     end else begin
      pvCrashReportNote(pvCrashReportKindRaise,0,aAddress,aObject.ClassName);
     end;
    end else begin
     pvCrashReportNote(pvCrashReportKindRaise,0,aAddress,'Unknown exception object');
    end;
   except
    // See the Delphi handler above, a failing note must stay silent.
   end;
  finally
   CrashReportInsideHandler:=false;
  end;
 end;
 if assigned(CrashReportOldRaiseProc) then begin
  CrashReportOldRaiseProc(aObject,aAddress,aFrameCount,aFrames);
 end;
end;
{$ifend}

procedure pvCrashReportInstall;
begin
 if CrashReportInstalled then begin
  exit;
 end;
 CrashReportInstalled:=true;
 FillChar(CrashReportRingBuffer,SizeOf(CrashReportRingBuffer),#0);
{$ifndef fpc}
 // Do not fight over these with JCL, madExcept or anything else which may have
 // installed itself first, since those do a better job at symbol resolution.
 if not assigned(Exception.GetExceptionStackInfoProc) then begin
  Exception.GetExceptionStackInfoProc:=CrashReportGetExceptionStackInfoProc;
  Exception.GetStackInfoStringProc:=CrashReportGetStackInfoStringProc;
  Exception.CleanUpStackInfoProc:=CrashReportCleanUpStackInfoProc;
  CrashReportOwnsStackInfoProcs:=true;
 end;
{$endif}
{$if defined(fpc) and defined(Windows)}
 CrashReportVectoredHandle:=AddVectoredExceptionHandler(1,@CrashReportVectoredExceptionHandler);
{$elseif defined(fpc)}
 // Plain assignment, not Addr, since RaiseProc is a procedural variable and Addr
 // would yield the address of the variable itself rather than its value.
 CrashReportOldRaiseProc:=System.RaiseProc;
 System.RaiseProc:=@CrashReportRaiseProc;
{$ifend}
end;

procedure pvCrashReportUninstall;
begin
 if not CrashReportInstalled then begin
  exit;
 end;
 CrashReportInstalled:=false;
{$ifndef fpc}
 if CrashReportOwnsStackInfoProcs then begin
  Exception.GetExceptionStackInfoProc:=nil;
  Exception.GetStackInfoStringProc:=nil;
  Exception.CleanUpStackInfoProc:=nil;
  CrashReportOwnsStackInfoProcs:=false;
 end;
{$endif}
{$if defined(fpc) and defined(Windows)}
 if assigned(CrashReportVectoredHandle) then begin
  RemoveVectoredExceptionHandler(CrashReportVectoredHandle);
  CrashReportVectoredHandle:=nil;
 end;
{$elseif defined(fpc)}
 System.RaiseProc:=CrashReportOldRaiseProc;
 CrashReportOldRaiseProc:=nil;
{$ifend}
 if assigned(CrashReportSymbolTable) then begin
  FreeAndNil(CrashReportSymbolTable);
 end;
end;

initialization

 pvCrashReportInstall;

finalization

 pvCrashReportUninstall;

end.
