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

      // The recorded address sits behind a call rather than on the instruction
      // it belongs to, so a lookup has to be done one byte earlier. Which of the
      // two applies is known only where the address is captured, and it differs
      // per compiler: Delphi reports the return address of the raise, while
      // FreePascal reports the address of the raise statement itself.
      pvCrashReportFlagReturnAddress=TpvUInt32(1);

type PpvCrashReportEntry=^TpvCrashReportEntry;
     TpvCrashReportEntry=record
      // Zero while the entry is being written. Deliberately no wider than a
      // word, so that reading and writing it is a single indivisible access on
      // every supported platform, 32 bit ones included, where a 64 bit store
      // can be split in two and a reader could catch it halfway.
      Sequence:TpvUInt32;
      ThreadID:TpvUInt64;
      Address:TpvPointer;
      Kind:TpvUInt32;
      Code:TpvUInt32;
      Flags:TpvUInt32;
      TextLength:TpvInt32;
      Text:array[0..pvCrashReportEntryTextSize-1] of AnsiChar;
     end;

// Adds a manually formatted entry to the ring buffer. Safe to call at any time,
// but not from inside a vectored exception handler, since it works with managed
// strings.
procedure pvCrashReportNote(const aKind,aCode:TpvUInt32;const aAddress:TpvPointer;const aText:String;const aReturnAddress:Boolean=false);

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

// Formats the processor state of the last fault which was recorded. Empty when
// none has happened, and empty on a platform which does not hand one over.
function pvCrashReportRegisters:String;

// Lists the modules of the process with the address each was loaded at, so that
// an address which nothing could name can still be placed.
function pvCrashReportModules:String;

// Formats the call stack of every other thread of the process. When a job
// system is involved the thread which crashed is often only the one which
// noticed, and the thread which caused it is somewhere else entirely, which is
// what this is for.
//
// Currently answers on Windows only, see the implementation for why.
function pvCrashReportThreadStacks(const aMaximalThreads:TpvInt32=32):String;

procedure pvCrashReportInstall;

procedure pvCrashReportUninstall;

implementation

// Which instruction set this is being built for, as far as the stack walker is
// concerned. Asking for a 64 bit cpu is not the same question: an ARM64 build is
// 64 bit as well and has neither the registers nor the unwind tables this uses.
{$if defined(cpux86_64) or defined(cpuamd64) or defined(cpux64)}
 {$define PasVulkanCrashReportX64}
{$elseif defined(cpu386) or defined(cpui386) or defined(cpux86)}
 {$define PasVulkanCrashReportX86}
{$ifend}

const LineEnding={$if defined(Windows)}#13#10{$else}#10{$ifend};

      HexDigits:array[0..15] of AnsiChar='0123456789abcdef';

      cDelphiException=TpvUInt32($0eedfade);

      cFPCException=TpvUInt32($e0465043);

      // The code the runtime of this compiler raises its own exceptions with,
      // as opposed to a fault reported by the operating system.
      cLanguageException={$ifdef fpc}cFPCException{$else}cDelphiException{$endif};

      cAccessViolation=TpvUInt32($c0000005);

      cMaximalStackFrames=48;

type PpvCrashReportModuleEntry=^TpvCrashReportModuleEntry;
     TpvCrashReportModuleEntry=record
      // Where the module was loaded, the module handle on Windows and the
      // mapping address on anything else. Not enough on its own to identify it,
      // see the file name below.
      Key:TpvPtrUInt;
      // The file the module was loaded from. An address alone does not identify
      // a module over the life of a process: a library can be unloaded and a
      // different one can then land on exactly the same address, and the old
      // entry would name routines out of a library which is no longer there.
      FileName:String;
      // What has to be taken off a runtime address to get the image relative
      // one the table is keyed by, which is where the module begins in memory.
      RVABase:TpvPtrUInt;
      // Nil when the module carries no table. That is remembered too, so that a
      // module without one is not opened again for every single frame.
      Table:TpvSymbolTable;
     end;

     // The processor state at the moment of a fault. Filled straight out of the
     // context the operating system hands to the handler, which was there all
     // along and simply went unused. Half the questions an address only log
     // leaves open are answered by looking at what was in the registers.
     TpvCrashReportFaultState=record
      // Which history entry this belongs to. Zero means nothing was recorded.
      Sequence:TpvUInt32;
      Code:TpvUInt32;
{$if defined(Windows)}
{$ifdef PasVulkanCrashReportX64}
      Rax,Rbx,Rcx,Rdx,Rsi,Rdi,Rbp,Rsp:TpvUInt64;
      R8,R9,R10,R11,R12,R13,R14,R15:TpvUInt64;
      Rip:TpvUInt64;
      EFlags:TpvUInt32;
{$endif}
{$ifdef PasVulkanCrashReportX86}
      Eax,Ebx,Ecx,Edx,Esi,Edi,Ebp,Esp:TpvUInt32;
      Eip:TpvUInt32;
      EFlags:TpvUInt32;
{$endif}
{$ifend}
     end;

     PpvCrashReportStackInfo=^TpvCrashReportStackInfo;
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

{$if defined(Windows)}
function AddVectoredExceptionHandler(aFirst:TpvUInt32;aHandler:TpvPointer):TpvPointer; stdcall; external 'kernel32.dll' name 'AddVectoredExceptionHandler';

function RemoveVectoredExceptionHandler(aHandle:TpvPointer):TpvUInt32; stdcall; external 'kernel32.dll' name 'RemoveVectoredExceptionHandler';

// Declared here rather than taken from the Windows unit, which does not offer
// it on every supported compiler version.
function GetModuleHandleExW(aFlags:TpvUInt32;aName:PWideChar;var aModule:HMODULE):LongBool; stdcall; external 'kernel32.dll' name 'GetModuleHandleExW';

// What a thread snapshot hands back. Declared here for the same reason as
// above, so that this does not depend on which helper unit a given compiler
// happens to ship it in.
type TpvCrashReportThreadEntry32=record
      Size:TpvUInt32;
      Usage:TpvUInt32;
      ThreadID:TpvUInt32;
      OwnerProcessID:TpvUInt32;
      BasePriority:TpvInt32;
      DeltaPriority:TpvInt32;
      Flags:TpvUInt32;
     end;

function CreateToolhelp32Snapshot(aFlags,aProcessID:TpvUInt32):THandle; stdcall; external 'kernel32.dll' name 'CreateToolhelp32Snapshot';
function Thread32First(aSnapshot:THandle;var aEntry:TpvCrashReportThreadEntry32):LongBool; stdcall; external 'kernel32.dll' name 'Thread32First';
function Thread32Next(aSnapshot:THandle;var aEntry:TpvCrashReportThreadEntry32):LongBool; stdcall; external 'kernel32.dll' name 'Thread32Next';
function OpenThread(aAccess:TpvUInt32;aInherit:LongBool;aThreadID:TpvUInt32):THandle; stdcall; external 'kernel32.dll' name 'OpenThread';

// The module entry of the same snapshot mechanism, for the module list.
type TpvCrashReportModuleEntry32=record
      Size:TpvUInt32;
      ModuleID:TpvUInt32;
      ProcessID:TpvUInt32;
      GlblcntUsage:TpvUInt32;
      ProccntUsage:TpvUInt32;
      BaseAddress:TpvPointer;
      BaseSize:TpvUInt32;
      Handle:HMODULE;
      ModuleName:array[0..255] of WideChar;
      ExePath:array[0..259] of WideChar;
     end;

function Module32FirstW(aSnapshot:THandle;var aEntry:TpvCrashReportModuleEntry32):LongBool; stdcall; external 'kernel32.dll' name 'Module32FirstW';
function Module32NextW(aSnapshot:THandle;var aEntry:TpvCrashReportModuleEntry32):LongBool; stdcall; external 'kernel32.dll' name 'Module32NextW';

{$ifdef PasVulkanCrashReportX64}
// The unwinder of the operating system itself, which is what makes walking the
// stack of another thread possible without a frame pointer and without pulling
// in a symbol handler.
function RtlLookupFunctionEntry(aControlPC:TpvUInt64;var aImageBase:TpvUInt64;aHistoryTable:TpvPointer):TpvPointer; stdcall; external 'kernel32.dll' name 'RtlLookupFunctionEntry';
function RtlVirtualUnwind(aHandlerType:TpvUInt32;aImageBase,aControlPC:TpvUInt64;aFunctionEntry:TpvPointer;aContext:TpvPointer;var aHandlerData:TpvPointer;var aEstablisherFrame:TpvUInt64;aContextPointers:TpvPointer):TpvPointer; stdcall; external 'kernel32.dll' name 'RtlVirtualUnwind';
{$endif}
{$ifend}

{$if defined(Linux) or defined(Android)}
// What dladdr fills in. Only the first two fields are of interest here, but the
// whole record has to be there, since the library writes all of it.
type TpvCrashReportDlInfo=record
      FileName:PAnsiChar;
      BaseAddress:TpvPointer;
      SymbolName:PAnsiChar;
      SymbolAddress:TpvPointer;
     end;

function dladdr(aAddress:TpvPointer;var aInfo:TpvCrashReportDlInfo):TpvInt32; cdecl; external 'dl' name 'dladdr';

function dlopen(aFileName:PAnsiChar;aFlags:TpvInt32):TpvPointer; cdecl; external 'dl' name 'dlopen';
function dlsym(aHandle:TpvPointer;aName:PAnsiChar):TpvPointer; cdecl; external 'dl' name 'dlsym';

// The unwinder every compiled language on a unix already shares, which walks
// the frame descriptions in .eh_frame instead of a chain of saved frame
// pointers. That matters because FreePascal leaves the frame pointer out of a
// leaf function on x86-64, and a walk over the chain then either skips frames
// or loses its way entirely.
//
// Looked up at runtime rather than linked against, so that this stays a thing
// which is used when it is there and quietly not used when it is not, without
// making every build depend on it.
type TpvCrashReportUnwindTrace=function(aContext,aData:TpvPointer):TpvInt32; cdecl;
     TpvCrashReportUnwindBacktrace=function(aTrace:TpvCrashReportUnwindTrace;aData:TpvPointer):TpvInt32; cdecl;
     TpvCrashReportUnwindGetIP=function(aContext:TpvPointer):TpvPtrUInt; cdecl;

     PpvCrashReportUnwindWalk=^TpvCrashReportUnwindWalk;
     TpvCrashReportUnwindWalk=record
      Count:TpvInt32;
      Skip:TpvInt32;
      Frames:array[0..cMaximalStackFrames-1] of TpvPointer;
     end;
{$ifend}

var CrashReportRingBuffer:array[0..pvCrashReportRingBufferSize-1] of TpvCrashReportEntry;
    CrashReportSequence:TpvInt32=0;
{$ifndef fpc}
    // Only ever written by the fallback in the barriers below, on a compiler
    // which offers nothing better.
    CrashReportBarrierDummy:TpvInt32=0;
{$endif}
    CrashReportInstalled:Boolean=false;
    // One entry per module an address has been resolved in so far. A process
    // is not only its executable: shared libraries carry their own appended
    // table, and a frame inside one of them would otherwise be nothing but an
    // address, which is exactly the frame worth having a name for.
{$if defined(Windows)}
    CrashReportFaultState:TpvCrashReportFaultState;
{$ifend}
{$if defined(fpc) and (defined(Linux) or defined(Android))}
    CrashReportUnwindBacktraceProc:TpvCrashReportUnwindBacktrace=nil;
    CrashReportUnwindGetIPProc:TpvCrashReportUnwindGetIP=nil;
    CrashReportUnwinderState:TpvInt32=0;
{$ifend}
    CrashReportModules:array of TpvCrashReportModuleEntry;
    CrashReportModuleCount:TpvInt32=0;
    CrashReportModuleLock:TpvInt32=0;
{$ifndef fpc}
    CrashReportOwnsStackInfoProcs:Boolean=false;
{$endif}
{$if defined(Windows)}
    CrashReportVectoredHandle:TpvPointer=nil;
    // Index of an operating system thread local slot, used by the vectored
    // handler, which cannot use a threadvar. Not allocated is all ones, which
    // is what TlsAlloc itself reports on failure.
    CrashReportTLSIndex:TpvUInt32=$ffffffff;
{$ifend}
{$if defined(fpc) and not defined(Windows)}
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

function CrashReportNextSequence:TpvUInt32;
begin
 // Zero is the mark for an entry which is being written, so it must never be
 // handed out as a real sequence number. After four billion exceptions the
 // counter comes round to it, and one more step is all it takes to skip it.
 repeat
{$ifdef fpc}
  result:=TpvUInt32(InterLockedIncrement(CrashReportSequence));
{$else}
  result:=TpvUInt32(AtomicIncrement(CrashReportSequence));
{$endif}
 until result<>0;
end;

// Publishing an entry and reading one back need the stores and the loads around
// the sequence number to stay in the order they were written in. On x86 the
// hardware orders them anyway and these cost nothing, on ARM it does not and
// the ring buffer would be unsound without them: a reader could see the new
// sequence number of an entry next to the contents of the entry before it.
procedure CrashReportWriteBarrier;
begin
{$if defined(fpc)}
 WriteBarrier;
{$elseif declared(MemoryBarrier)}
 MemoryBarrier;
{$else}
 AtomicExchange(CrashReportBarrierDummy,0);
{$ifend}
end;

procedure CrashReportReadBarrier;
begin
{$if defined(fpc)}
 ReadBarrier;
{$elseif declared(MemoryBarrier)}
 MemoryBarrier;
{$else}
 AtomicExchange(CrashReportBarrierDummy,0);
{$ifend}
end;

// Takes the module cache. Only ever held while a report is being formatted,
// never in the vectored handler, so a plain spin is enough and saves bringing
// in anything which would have to be created and destroyed.
procedure CrashReportAcquireModuleLock;
begin
{$ifdef fpc}
 while InterLockedExchange(CrashReportModuleLock,1)<>0 do begin
{$else}
 while AtomicExchange(CrashReportModuleLock,1)<>0 do begin
{$endif}
{$if defined(Windows)}
  Sleep(0);
{$ifend}
 end;
end;

procedure CrashReportReleaseModuleLock;
begin
 CrashReportWriteBarrier;
 CrashReportModuleLock:=0;
end;

{$if defined(Windows)}
// Marks this thread as being inside the part of the vectored handler which
// dereferences the exception object, and reports whether it was already. A
// threadvar is deliberately not used, since the handler can run on threads the
// runtime never created and whose thread local storage is therefore not set up,
// where reading a threadvar is itself a fault.
function CrashReportEnterObjectInspection:Boolean;
begin
 if CrashReportTLSIndex=TpvUInt32($ffffffff) then begin
  // No slot, so no protection can be offered and the object is left alone
  // rather than risking the recursion.
  result:=false;
 end else if assigned(TlsGetValue(CrashReportTLSIndex)) then begin
  result:=false;
 end else begin
  TlsSetValue(CrashReportTLSIndex,TpvPointer(TpvPtrUInt(1)));
  result:=true;
 end;
end;

procedure CrashReportLeaveObjectInspection;
begin
 if CrashReportTLSIndex<>TpvUInt32($ffffffff) then begin
  TlsSetValue(CrashReportTLSIndex,nil);
 end;
end;
{$ifend}

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

procedure CrashReportEntryBegin(const aEntry:PpvCrashReportEntry;const aKind,aCode:TpvUInt32;const aAddress:TpvPointer;const aThreadID:TpvUInt64;const aFlags:TpvUInt32);
begin
 // Zero marks the slot as being written. It has to become visible before the
 // fields change, otherwise a reader could still see the old sequence number
 // next to fields which are already the new ones.
 aEntry^.Sequence:=0;
 CrashReportWriteBarrier;
 aEntry^.ThreadID:=aThreadID;
 aEntry^.Address:=aAddress;
 aEntry^.Kind:=aKind;
 aEntry^.Code:=aCode;
 aEntry^.Flags:=aFlags;
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

// Appends a UTF-16 string as UTF-8, walking it as raw memory up to its
// terminator. This is what a Delphi Message needs, and unlike anything which
// takes a String it neither allocates nor touches a reference count, which the
// vectored handler must not do.
procedure CrashReportEntryAppendPWideChar(const aEntry:PpvCrashReportEntry;const aValue:PWideChar);
var Index:TpvInt32;
    CodeUnit:TpvUInt32;
begin
 if assigned(aValue) then begin
  Index:=0;
  while (Index<pvCrashReportEntryTextSize) and (aValue[Index]<>#0) do begin
   if aEntry^.TextLength>=(pvCrashReportEntryTextSize-5) then begin
    break;
   end;
   CodeUnit:=TpvUInt32(Ord(aValue[Index]));
   // A character outside the basic plane arrives as two code units. Encoding
   // them one by one would give two three byte sequences, which is not UTF-8
   // but the older variant of it, and a strict reader rejects that. So the pair
   // is put back together into the one code point it stands for.
   if (CodeUnit>=$d800) and (CodeUnit<=$dbff) and
      (TpvUInt32(Ord(aValue[Index+1]))>=$dc00) and
      (TpvUInt32(Ord(aValue[Index+1]))<=$dfff) then begin
    CodeUnit:=$10000+((CodeUnit-$d800) shl 10)+(TpvUInt32(Ord(aValue[Index+1]))-$dc00);
    inc(Index);
   end;
   if CodeUnit<$80 then begin
    aEntry^.Text[aEntry^.TextLength]:=AnsiChar(TpvUInt8(CodeUnit));
    inc(aEntry^.TextLength);
   end else if CodeUnit<$800 then begin
    aEntry^.Text[aEntry^.TextLength]:=AnsiChar(TpvUInt8($c0 or (CodeUnit shr 6)));
    aEntry^.Text[aEntry^.TextLength+1]:=AnsiChar(TpvUInt8($80 or (CodeUnit and $3f)));
    inc(aEntry^.TextLength,2);
   end else if CodeUnit<$10000 then begin
    aEntry^.Text[aEntry^.TextLength]:=AnsiChar(TpvUInt8($e0 or (CodeUnit shr 12)));
    aEntry^.Text[aEntry^.TextLength+1]:=AnsiChar(TpvUInt8($80 or ((CodeUnit shr 6) and $3f)));
    aEntry^.Text[aEntry^.TextLength+2]:=AnsiChar(TpvUInt8($80 or (CodeUnit and $3f)));
    inc(aEntry^.TextLength,3);
   end else begin
    aEntry^.Text[aEntry^.TextLength]:=AnsiChar(TpvUInt8($f0 or (CodeUnit shr 18)));
    aEntry^.Text[aEntry^.TextLength+1]:=AnsiChar(TpvUInt8($80 or ((CodeUnit shr 12) and $3f)));
    aEntry^.Text[aEntry^.TextLength+2]:=AnsiChar(TpvUInt8($80 or ((CodeUnit shr 6) and $3f)));
    aEntry^.Text[aEntry^.TextLength+3]:=AnsiChar(TpvUInt8($80 or (CodeUnit and $3f)));
    inc(aEntry^.TextLength,4);
   end;
   inc(Index);
  end;
  aEntry^.Text[aEntry^.TextLength]:=#0;
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

procedure pvCrashReportNote(const aKind,aCode:TpvUInt32;const aAddress:TpvPointer;const aText:String;const aReturnAddress:Boolean);
var Entry:PpvCrashReportEntry;
    Sequence:TpvUInt32;
    Flags:TpvUInt32;
begin
 if aReturnAddress then begin
  Flags:=pvCrashReportFlagReturnAddress;
 end else begin
  Flags:=0;
 end;
 Sequence:=CrashReportNextSequence;
 Entry:=@CrashReportRingBuffer[(Sequence-1) and (pvCrashReportRingBufferSize-1)];
 CrashReportEntryBegin(Entry,aKind,aCode,aAddress,CrashReportCurrentThreadID,Flags);
 CrashReportEntryAppendString(Entry,aText);
 // Published last, and only once everything above is visible.
 CrashReportWriteBarrier;
 Entry^.Sequence:=Sequence;
end;

{$if defined(Windows) or defined(Linux) or defined(Android)}

{$if defined(Linux)}
// Reads the whole of a proc file. It cannot be read through a stream, because
// proc reports a size of zero for its files, so anything which trusts that size
// ends up with nothing.
function CrashReportReadProcFile(const aFileName:String):TpvRawByteString;
var Handle:THandle;
    Buffer:array[0..4095] of AnsiChar;
    Chunk:TpvRawByteString;
    Count:TpvSizeInt;
begin
 result:='';
 Handle:=FileOpen(aFileName,fmOpenRead or fmShareDenyNone);
 if Handle<>THandle(-1) then begin
  try
   repeat
    Count:=FileRead(Handle,Buffer[0],SizeOf(Buffer));
    if Count>0 then begin
     SetLength(Chunk,Count);
     Move(Buffer[0],Chunk[1],Count);
     result:=result+Chunk;
    end;
   until Count<=0;
  finally
   FileClose(Handle);
  end;
 end;
end;

// Finds the address the executable itself is mapped at, which for a position
// independent build is the load bias and for an ordinary one is simply the link
// time base again. Returns zero when it cannot be determined, and the caller
// then falls back to the base recorded in the table.
function CrashReportLinuxModuleBase:TpvPtrUInt;
var Maps,Line:TpvRawByteString;
    ExecutableName:TpvRawByteString;
    Start,Stop,SlashPosition,DashPosition,Index:TpvSizeInt;
    Value:TpvUInt64;
    Digit:TpvUInt32;
begin

 result:=0;
 ExecutableName:=TpvRawByteString(ParamStr(0));
 if length(ExecutableName)=0 then begin
  exit;
 end;

 Maps:=CrashReportReadProcFile('/proc/self/maps');

 Start:=1;
 while Start<=length(Maps) do begin

  Stop:=Start;
  while (Stop<=length(Maps)) and (Maps[Stop]<>#10) do begin
   inc(Stop);
  end;
  Line:=Copy(Maps,Start,Stop-Start);
  Start:=Stop+1;

  // A line reads: start-end perms offset dev inode path
  SlashPosition:=Pos(TpvRawByteString('/'),Line);
  if (SlashPosition=0) or (Copy(Line,SlashPosition,length(Line))<>ExecutableName) then begin
   continue;
  end;

  DashPosition:=Pos(TpvRawByteString('-'),Line);
  if DashPosition<2 then begin
   continue;
  end;

  Value:=0;
  for Index:=1 to DashPosition-1 do begin
   case Line[Index] of
    '0'..'9':begin
     Digit:=TpvUInt32(ord(Line[Index])-ord('0'));
    end;
    'a'..'f':begin
     Digit:=TpvUInt32(ord(Line[Index])-ord('a'))+10;
    end;
    'A'..'F':begin
     Digit:=TpvUInt32(ord(Line[Index])-ord('A'))+10;
    end;
    else begin
     Value:=0;
     break;
    end;
   end;
   Value:=(Value shl 4) or Digit;
  end;

  // The first mapping of the executable is the start of its image.
  result:=TpvPtrUInt(Value);
  exit;

 end;

end;
{$ifend}

// The symbol table is only read when an address actually has to be formatted,
// which normally means a crash is already being reported. Loading it eagerly at
// startup would mean paying for a table which is many megabytes on a large
// project, in every run, for nothing.
// Names the module an address belongs to, and where it was loaded. This is what
// makes a frame inside a shared library resolvable rather than a bare address.
function CrashReportModuleForAddress(const aAddress:TpvPointer;out aKey:TpvPtrUInt;out aFileName:String):Boolean;
{$if defined(Windows)}
const GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT=TpvUInt32($00000002);
      GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS=TpvUInt32($00000004);
var Module:HMODULE;
    Buffer:array[0..1023] of WideChar;
    Count:TpvUInt32;
begin
 result:=false;
 aKey:=0;
 aFileName:='';
 Module:=0;
 // Asking for the module by address rather than walking a list, and without
 // taking a reference, so that this cannot keep a library alive.
 if GetModuleHandleExW(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS or GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,PWideChar(aAddress),Module) and (Module<>0) then begin
  Count:=GetModuleFileNameW(Module,@Buffer[0],length(Buffer)-1);
  if (Count>0) and (Count<TpvUInt32(length(Buffer))) then begin
   Buffer[Count]:=#0;
   aKey:=TpvPtrUInt(Module);
   aFileName:=String(PWideChar(@Buffer[0]));
   result:=true;
  end;
 end;
end;
{$elseif defined(Linux) or defined(Android)}
var Info:TpvCrashReportDlInfo;
begin
 result:=false;
 aKey:=0;
 aFileName:='';
 FillChar(Info,SizeOf(TpvCrashReportDlInfo),#0);
 if (dladdr(aAddress,Info)<>0) and assigned(Info.FileName) then begin
  // The base reported here is the load bias, which is zero for an executable
  // linked to a fixed address and the mapping address for one which is not.
  aKey:=TpvPtrUInt(Info.BaseAddress);
  aFileName:=String(Info.FileName);
  result:=true;
 end;
end;
{$else}
begin
 result:=false;
 aKey:=0;
 aFileName:='';
end;
{$ifend}

// Returns the table covering an address, loading it on first use, together with
// what has to be subtracted from a runtime address to look something up in it.
function CrashReportTableForAddress(const aAddress:TpvPointer;out aRVABase:TpvPtrUInt):TpvSymbolTable;
var Index,Slot:TpvInt32;
    Key:TpvPtrUInt;
    FileName:String;
    Table:TpvSymbolTable;
    RVABase:TpvPtrUInt;
    Known:Boolean;
begin

 result:=nil;
 aRVABase:=0;

 if not CrashReportModuleForAddress(aAddress,Key,FileName) then begin
{$if defined(Windows)}
  exit;
{$else}
  // Without a way to ask, fall back to the executable itself, which is the
  // module every address belonged to before this could tell them apart.
  Key:=0;
  FileName:=ParamStr(0);
{$ifend}
 end;

 Known:=false;
 CrashReportAcquireModuleLock;
 try
  for Index:=0 to CrashReportModuleCount-1 do begin
   if (CrashReportModules[Index].Key=Key) and
      (CrashReportModules[Index].FileName=FileName) then begin
    result:=CrashReportModules[Index].Table;
    aRVABase:=CrashReportModules[Index].RVABase;
    Known:=true;
    break;
   end;
  end;
 finally
  CrashReportReleaseModuleLock;
 end;

 if Known then begin
  exit;
 end;

 // Loading happens outside the lock, since it reads a file and there is no
 // reason to hold every other thread up for that. Two threads may then load the
 // same module at once, which costs one wasted read and nothing else.
 Table:=nil;
 RVABase:=0;
 try
  Table:=TpvSymbolTable.Create;
  if Table.LoadFromFile(FileName) then begin
{$if defined(Windows)}
   // A module handle already is the image base as it ended up in memory, so it
   // covers address space layout randomization on its own.
   RVABase:=Key;
{$else}
   // What dladdr reports is documented as the address the object was loaded at,
   // and that is what it is: the start of the mapping, which for an executable
   // linked to a fixed address is its own link time base and for a position
   // independent one is wherever it ended up. Either way it is what has to come
   // off, exactly like a module handle on Windows.
   //
   // A library which reports the load bias instead, zero for a fixed address
   // image, would be off by the link time base, so that case is caught by
   // adding it back where the answer is plainly too low to be a mapping.
   RVABase:=Key;
   if RVABase<TpvPtrUInt(Table.ImageBase) then begin
    inc(RVABase,TpvPtrUInt(Table.ImageBase));
   end;
{$if defined(Linux)}
   if RVABase=0 then begin
    // Last resort, from the process map, for a runtime whose dladdr said
    // nothing useful.
    RVABase:=CrashReportLinuxModuleBase;
   end;
{$ifend}
{$ifend}
  end else begin
   // No table appended, which is the normal case for a module the mapsymbols
   // tool has not been run on, and for every system library.
   FreeAndNil(Table);
  end;
 except
  FreeAndNil(Table);
 end;

 CrashReportAcquireModuleLock;
 try
  Slot:=-1;
  for Index:=0 to CrashReportModuleCount-1 do begin
   if CrashReportModules[Index].Key=Key then begin
    if CrashReportModules[Index].FileName=FileName then begin
     // Another thread got here first while the load above was running.
     FreeAndNil(Table);
     result:=CrashReportModules[Index].Table;
     aRVABase:=CrashReportModules[Index].RVABase;
     exit;
    end;
    // Same address, different file, so the module which used to be here is
    // gone. Its entry is taken over rather than left to answer for its
    // successor.
    FreeAndNil(CrashReportModules[Index].Table);
    Slot:=Index;
    break;
   end;
  end;

  if Slot<0 then begin
   Slot:=CrashReportModuleCount;
   if Slot>=length(CrashReportModules) then begin
    SetLength(CrashReportModules,(Slot+1)*2);
   end;
   inc(CrashReportModuleCount);
  end;

  CrashReportModules[Slot].Key:=Key;
  CrashReportModules[Slot].FileName:=FileName;
  CrashReportModules[Slot].RVABase:=RVABase;
  CrashReportModules[Slot].Table:=Table;
  CrashReportWriteBarrier;
  result:=Table;
  aRVABase:=RVABase;

 finally
  CrashReportReleaseModuleLock;
 end;

end;

// Returns false when there is no table, or when the address does not belong to
// the executable itself, for example because it points into a system library.
function CrashReportResolveAddress(const aAddress:TpvPointer;const aReturnAddress:Boolean;out aText:String):Boolean;
var SymbolTable:TpvSymbolTable;
    Location:TpvSymbolTableLocation;
    LookupAddress,RVABase:TpvPtrUInt;
begin
 result:=false;
 aText:='';
 SymbolTable:=CrashReportTableForAddress(aAddress,RVABase);
 if not assigned(SymbolTable) then begin
  exit;
 end;
 if TpvPtrUInt(aAddress)<=RVABase then begin
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
 if not SymbolTable.Resolve(TpvUInt64(LookupAddress-RVABase),Location) then begin
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

{$if defined(fpc) and (defined(Linux) or defined(Android))}
// Called once per frame by the unwinder. Only collects, so that nothing which
// could raise happens inside a callback the library is driving.
function CrashReportUnwindTraceCallback(aContext,aData:TpvPointer):TpvInt32; cdecl;
const cUnwindNoReason=0;
      cUnwindEndOfStack=5;
var Walk:PpvCrashReportUnwindWalk;
    Address:TpvPtrUInt;
begin
 result:=cUnwindNoReason;
 Walk:=PpvCrashReportUnwindWalk(aData);
 if not assigned(Walk) then begin
  result:=cUnwindEndOfStack;
  exit;
 end;
 if Walk^.Skip>0 then begin
  dec(Walk^.Skip);
  exit;
 end;
 if Walk^.Count>=cMaximalStackFrames then begin
  result:=cUnwindEndOfStack;
  exit;
 end;
 Address:=CrashReportUnwindGetIPProc(aContext);
 if Address=0 then begin
  result:=cUnwindEndOfStack;
  exit;
 end;
 Walk^.Frames[Walk^.Count]:=TpvPointer(Address);
 inc(Walk^.Count);
end;

// Looks the unwinder up. Called once while installing, which is a quiet moment,
// so that the crash path itself only has two function pointers to check. A
// system which does not have it simply keeps the fallback.
procedure CrashReportLoadUnwinder;
const RTLD_NOW=2;
var Library_:TpvPointer;
begin
 if InterLockedExchange(CrashReportUnwinderState,1)<>0 then begin
  exit;
 end;
 // First wherever it already is, which covers a program linked against it and
 // a runtime which has it built in.
 Library_:=dlopen(nil,RTLD_NOW);
 if assigned(Library_) then begin
  CrashReportUnwindBacktraceProc:=TpvCrashReportUnwindBacktrace(dlsym(Library_,'_Unwind_Backtrace'));
  CrashReportUnwindGetIPProc:=TpvCrashReportUnwindGetIP(dlsym(Library_,'_Unwind_GetIP'));
 end;
 if not (assigned(CrashReportUnwindBacktraceProc) and assigned(CrashReportUnwindGetIPProc)) then begin
  Library_:=dlopen('libgcc_s.so.1',RTLD_NOW);
  if assigned(Library_) then begin
   CrashReportUnwindBacktraceProc:=TpvCrashReportUnwindBacktrace(dlsym(Library_,'_Unwind_Backtrace'));
   CrashReportUnwindGetIPProc:=TpvCrashReportUnwindGetIP(dlsym(Library_,'_Unwind_GetIP'));
  end;
 end;
end;
{$ifend}

function pvCrashReportFormatAddress(const aAddress:TpvPointer;const aReturnAddress:Boolean):String;
begin
{$if defined(Windows) or defined(Linux) or defined(Android)}
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
{$if defined(Linux) or defined(Android)}
    Walk:TpvCrashReportUnwindWalk;
{$ifend}
begin
 result:='';

{$if defined(Linux) or defined(Android)}
 // The unwinder of the platform first, which walks the frame descriptions in
 // .eh_frame and therefore does not care whether a frame pointer was kept.
 //
 // It only gets anywhere where those descriptions exist, and for FreePascal
 // code they do not: its exception handling does not need them, so it emits an
 // empty .eh_frame and the unwinder stops after the frame it started in. That
 // is why a single frame is not accepted as an answer here. It does work for
 // frames inside C libraries, which do carry the descriptions.
 if assigned(CrashReportUnwindBacktraceProc) and assigned(CrashReportUnwindGetIPProc) then begin
  Walk.Count:=0;
  // One more, for the frame of this function itself.
  Walk.Skip:=aFramesToSkip+1;
  FillChar(Walk.Frames,SizeOf(Walk.Frames),#0);
  CrashReportUnwindBacktraceProc(CrashReportUnwindTraceCallback,@Walk);
  if Walk.Count>1 then begin
   for Index:=0 to Walk.Count-1 do begin
    result:=result+'  '+pvCrashReportFormatAddress(Walk.Frames[Index])+LineEnding;
   end;
   exit;
  end;
 end;
{$ifend}

 // Otherwise the chain of saved frame pointers, which is all that is left. It
 // needs those frames to have been kept: FreePascal drops them under
 // optimization unless the build says otherwise, and the chain is then empty
 // from the start. Building with -OoNOSTACKFRAME keeps them.
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
 if length(result)=0 then begin
  // Nothing came of either way. Saying so beats an empty block, which reads
  // like the stack was empty rather than like it could not be walked.
  result:='  (no stack could be walked, the build kept neither frame pointers nor unwind information)'+LineEnding;
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
    Newest,Wanted:TpvUInt32;
    Entry:PpvCrashReportEntry;
    Snapshot:TpvCrashReportEntry;
begin
 result:='';
 Newest:=TpvUInt32(CrashReportSequence);
 if Newest=0 then begin
  exit;
 end;
 Count:=aMaximalCount;
 if Count>pvCrashReportRingBufferSize then begin
  Count:=pvCrashReportRingBufferSize;
 end;
 // Only relevant before the counter has gone round the first time, which is
 // also the only moment at which it says how many entries there are at all.
 if TpvUInt32(Count)>Newest then begin
  Count:=TpvInt32(Newest);
 end;
 result:='First chance exception history, oldest first, at most '+IntToStr(Count)+' entries:'+LineEnding;
 for Index:=Count-1 downto 0 do begin
  Wanted:=Newest-TpvUInt32(Index);
  Entry:=@CrashReportRingBuffer[(Wanted-1) and (pvCrashReportRingBufferSize-1)];
  // A mismatching sequence means the entry is either still being written or has
  // already been overwritten by a newer one.
  if Entry^.Sequence<>Wanted then begin
   continue;
  end;
  // Taken as a copy, and the sequence checked once more afterwards. Formatting
  // straight out of the ring buffer would leave a window in which a writer
  // takes the slot over halfway through, and the result would then be one half
  // of one entry next to one half of another, with nothing to show for it.
  CrashReportReadBarrier;
  Snapshot:=Entry^;
  CrashReportReadBarrier;
  if Entry^.Sequence<>Wanted then begin
   continue;
  end;
  result:=result+'  #'+IntToStr(Snapshot.Sequence)+
                 ' thread $'+IntToHex(Snapshot.ThreadID,8)+
                 ' '+CrashReportKindToString(Snapshot.Kind);
  if Snapshot.Code<>0 then begin
   result:=result+' code $'+IntToHex(Snapshot.Code,8);
  end;
  if assigned(Snapshot.Address) then begin
   result:=result+' at '+pvCrashReportFormatAddress(Snapshot.Address,(Snapshot.Flags and pvCrashReportFlagReturnAddress)<>0);
  end;
  if Snapshot.TextLength>0 then begin
   result:=result+' : '+String(PAnsiChar(@Snapshot.Text[0]));
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
  // Delphi hands over the return address of the raise site.
  pvCrashReportNote(pvCrashReportKindRaise,0,aExceptionRecord^.ExceptionAddress,Text,true);
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
{$ifndef Windows}
   // On Windows the vectored handler has already recorded this, and doing it
   // here as well would put every exception into the history twice.
   CrashReportNoteNativeExceptionRecord(PpvCrashReportNativeExceptionRecord(P));
{$endif}
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

{$if defined(Windows)}
// Copies the processor state out of the context the handler was given. Split
// out so that the handler itself stays readable, and kept to plain assignments
// so that it is as safe to call from there as everything else around it.
procedure CrashReportNoteFaultState(const aSequence,aCode:TpvUInt32;const aContext:TpvPointer);
{$if defined(Windows) and (defined(PasVulkanCrashReportX64) or defined(PasVulkanCrashReportX86))}
var Context:PContext;
begin
 if not assigned(aContext) then begin
  exit;
 end;
 Context:=PContext(aContext);
 CrashReportFaultState.Sequence:=0;
 CrashReportWriteBarrier;
 CrashReportFaultState.Code:=aCode;
{$ifdef PasVulkanCrashReportX64}
 CrashReportFaultState.Rax:=Context^.Rax;
 CrashReportFaultState.Rbx:=Context^.Rbx;
 CrashReportFaultState.Rcx:=Context^.Rcx;
 CrashReportFaultState.Rdx:=Context^.Rdx;
 CrashReportFaultState.Rsi:=Context^.Rsi;
 CrashReportFaultState.Rdi:=Context^.Rdi;
 CrashReportFaultState.Rbp:=Context^.Rbp;
 CrashReportFaultState.Rsp:=Context^.Rsp;
 CrashReportFaultState.R8:=Context^.R8;
 CrashReportFaultState.R9:=Context^.R9;
 CrashReportFaultState.R10:=Context^.R10;
 CrashReportFaultState.R11:=Context^.R11;
 CrashReportFaultState.R12:=Context^.R12;
 CrashReportFaultState.R13:=Context^.R13;
 CrashReportFaultState.R14:=Context^.R14;
 CrashReportFaultState.R15:=Context^.R15;
 CrashReportFaultState.Rip:=Context^.Rip;
 CrashReportFaultState.EFlags:=Context^.EFlags;
{$endif}
{$ifdef PasVulkanCrashReportX86}
 CrashReportFaultState.Eax:=Context^.Eax;
 CrashReportFaultState.Ebx:=Context^.Ebx;
 CrashReportFaultState.Ecx:=Context^.Ecx;
 CrashReportFaultState.Edx:=Context^.Edx;
 CrashReportFaultState.Esi:=Context^.Esi;
 CrashReportFaultState.Edi:=Context^.Edi;
 CrashReportFaultState.Ebp:=Context^.Ebp;
 CrashReportFaultState.Esp:=Context^.Esp;
 CrashReportFaultState.Eip:=Context^.Eip;
 CrashReportFaultState.EFlags:=Context^.EFlags;
{$endif}
 CrashReportWriteBarrier;
 CrashReportFaultState.Sequence:=aSequence;
end;
{$else}
begin
 // Nothing is recorded on an architecture whose registers this does not know,
 // rather than a handful of fields which would have to be guessed at.
end;
{$ifend}

function CrashReportVectoredExceptionHandler(aExceptionInformation:PpvCrashReportNativeExceptionPointers):TpvInt32; stdcall;
const EXCEPTION_CONTINUE_SEARCH=TpvInt32(0);
var ExceptionRecord:PpvCrashReportNativeExceptionRecord;
    Entry:PpvCrashReportEntry;
    Sequence:TpvUInt32;
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
 if ExceptionRecord^.ExceptionCode=cLanguageException then begin
{$ifdef fpc}
  // fpc_RaiseException passes address, object, frame count and frames, see
  // rtl/win64/seh64.inc and rtl/win32/seh32.inc.
  // fpc_RaiseException passes the address of the raise statement itself, not
  // the address behind it, so no adjustment is wanted here.
  CrashReportEntryBegin(Entry,pvCrashReportKindRaise,0,ExceptionRecord^.ExceptionAddress,TpvUInt64(GetCurrentThreadId),0);
{$else}
  // Delphi hands over the return address of the raise site instead.
  CrashReportEntryBegin(Entry,pvCrashReportKindRaise,0,ExceptionRecord^.ExceptionAddress,TpvUInt64(GetCurrentThreadId),pvCrashReportFlagReturnAddress);
{$endif}
  if ExceptionRecord^.NumberParameters>=2 then begin
   // Both runtimes pass the address of the raise as the first parameter. The
   // one in the record itself is the address inside RaiseException, which is
   // the same for every raise in the program and therefore of no use.
   Entry^.Address:=TpvPointer(ExceptionRecord^.ExceptionInformation[0]);
   ExceptionObject:=TObject(ExceptionRecord^.ExceptionInformation[1]);
   // Everything below this point dereferences a pointer which the raising code
   // supplied, and a corrupted one faults right here. That fault is dispatched
   // to the vectored handlers as well, which is to say back into this function,
   // and without a guard it would keep going until the stack runs out, at the
   // one moment where the report is wanted most. A thread local flag from the
   // runtime cannot be used for it, since this can run on threads whose thread
   // local storage the runtime has never set up, so the slot comes from the
   // operating system directly.
   if assigned(ExceptionObject) and CrashReportEnterObjectInspection then begin
    try
     // Read out of the virtual method table rather than through ClassName,
     // which builds a string and would therefore allocate.
     ClassNameString:=PShortString(PPointer(TpvPtrUInt(TpvPtrUInt(ExceptionObject.ClassType)+TpvPtrUInt(vmtClassName)))^);
     if assigned(ClassNameString) then begin
      CrashReportEntryAppendChars(Entry,@ClassNameString^[1],length(ClassNameString^));
     end;
     if ExceptionObject is Exception then begin
      CrashReportEntryAppendPAnsiChar(Entry,': ');
      // Casting the string field to a pointer reads it without touching its
      // reference count and without allocating a temporary.
{$if defined(fpc) and (SizeOf(Char)=1)}
      CrashReportEntryAppendPAnsiChar(Entry,PAnsiChar(TpvPointer(Exception(ExceptionObject).Message)));
{$else}
      CrashReportEntryAppendPWideChar(Entry,PWideChar(TpvPointer(Exception(ExceptionObject).Message)));
{$ifend}
     end;
    finally
     CrashReportLeaveObjectInspection;
    end;
   end;
  end;
 end else begin
  CrashReportEntryBegin(Entry,pvCrashReportKindFault,ExceptionRecord^.ExceptionCode,ExceptionRecord^.ExceptionAddress,TpvUInt64(GetCurrentThreadId),0);
  // The context has been sitting in the arguments all along. Only plain stores
  // here, no allocation and no call, so this is as safe as the rest.
  CrashReportNoteFaultState(Sequence,ExceptionRecord^.ExceptionCode,aExceptionInformation^.ContextRecord);
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
 CrashReportWriteBarrier;
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

{$if defined(Windows) and (defined(PasVulkanCrashReportX64) or defined(PasVulkanCrashReportX86))}
// Walks the stack of a thread which is already suspended and whose context has
// been read.
//
// On x86-64 this goes through the unwind information of the image, the same way
// the operating system itself unwinds, so it works regardless of whether a
// frame pointer was kept. On x86-32 there is no such information and the chain
// of saved frame pointers is all there is, which means a routine compiled
// without one is skipped over. Every address read out of the stack is read
// through ReadProcessMemory, so that a broken chain ends the walk instead of
// faulting.
function CrashReportWalkSuspendedThread(var aContext:TContext;const aFrames:PPointer;const aMaximalFrames:TpvInt32):TpvInt32;
{$ifdef PasVulkanCrashReportX64}
var ImageBase,EstablisherFrame,ReturnAddress,PreviousStackPointer:TpvUInt64;
    FunctionEntry,HandlerData:TpvPointer;
    Read:{$ifdef fpc}TpvSizeUInt{$else}NativeUInt{$endif};
begin
 result:=0;
 while result<aMaximalFrames do begin
  if aContext.Rip=0 then begin
   break;
  end;
  PPointerArray(aFrames)^[result]:=TpvPointer(TpvPtrUInt(aContext.Rip));
  inc(result);
  PreviousStackPointer:=aContext.Rsp;
  ImageBase:=0;
  FunctionEntry:=RtlLookupFunctionEntry(aContext.Rip,ImageBase,nil);
  if assigned(FunctionEntry) then begin
   HandlerData:=nil;
   EstablisherFrame:=0;
   RtlVirtualUnwind(0,ImageBase,aContext.Rip,FunctionEntry,@aContext,HandlerData,EstablisherFrame,nil);
   // Unwinding has to move up the stack. Damaged unwind information can leave
   // the stack pointer where it was, and the walk would then keep reporting the
   // same frame until it runs into the frame limit.
   if aContext.Rsp<=PreviousStackPointer then begin
    break;
   end;
  end else begin
   // A leaf function, which has no unwind information because it never moves
   // the stack pointer. Its return address is simply on top of the stack.
   ReturnAddress:=0;
   if not ReadProcessMemory(GetCurrentProcess,TpvPointer(TpvPtrUInt(aContext.Rsp)),@ReturnAddress,SizeOf(TpvUInt64),{$ifdef fpc}Read{$else}Read{$endif}) then begin
    break;
   end;
   aContext.Rip:=ReturnAddress;
   inc(aContext.Rsp,SizeOf(TpvUInt64));
  end;
 end;
end;
{$else}
var Frame,NextFrame,ReturnAddress:TpvUInt32;
    Read:{$ifdef fpc}TpvSizeUInt{$else}NativeUInt{$endif};
begin
 result:=0;
 if aContext.Eip<>0 then begin
  PPointerArray(aFrames)^[result]:=TpvPointer(TpvPtrUInt(aContext.Eip));
  inc(result);
 end;
 Frame:=aContext.Ebp;
 while result<aMaximalFrames do begin
  NextFrame:=0;
  ReturnAddress:=0;
  if not ReadProcessMemory(GetCurrentProcess,TpvPointer(TpvPtrUInt(Frame)),@NextFrame,SizeOf(TpvUInt32),Read) then begin
   break;
  end;
  if not ReadProcessMemory(GetCurrentProcess,TpvPointer(TpvPtrUInt(Frame)+SizeOf(TpvUInt32)),@ReturnAddress,SizeOf(TpvUInt32),Read) then begin
   break;
  end;
  if (ReturnAddress=0) or (NextFrame<=Frame) then begin
   // Either the end of the chain or something which is no longer a chain.
   break;
  end;
  PPointerArray(aFrames)^[result]:=TpvPointer(TpvPtrUInt(ReturnAddress));
  inc(result);
  Frame:=NextFrame;
 end;
end;
{$endif}
{$ifend}

function pvCrashReportRegisters:String;
// Only Windows, because the vectored handler is the only place a processor
// context is handed over. On a unix that would take a signal handler, which is
// the same decision the thread stacks are waiting on.
{$if defined(Windows) and (defined(PasVulkanCrashReportX64) or defined(PasVulkanCrashReportX86))}
var State:TpvCrashReportFaultState;

 function Reg(const aName:String;const aValue:TpvUInt64):String;
 begin
  result:=aName+'='+IntToHex(aValue,SizeOf(TpvPointer) shl 1)+' ';
 end;

begin
 result:='';
 if CrashReportFaultState.Sequence=0 then begin
  exit;
 end;
 // Taken as a copy and rechecked, for the same reason the history entries are.
 CrashReportReadBarrier;
 State:=CrashReportFaultState;
 CrashReportReadBarrier;
 if (State.Sequence=0) or (State.Sequence<>CrashReportFaultState.Sequence) then begin
  exit;
 end;
 result:='Processor state at fault #'+IntToStr(State.Sequence)+':'+LineEnding;
{$ifdef PasVulkanCrashReportX64}
 result:=result+'  '+Reg('rip',State.Rip)+Reg('rsp',State.Rsp)+Reg('rbp',State.Rbp)+LineEnding+
                '  '+Reg('rax',State.Rax)+Reg('rbx',State.Rbx)+Reg('rcx',State.Rcx)+LineEnding+
                '  '+Reg('rdx',State.Rdx)+Reg('rsi',State.Rsi)+Reg('rdi',State.Rdi)+LineEnding+
                '  '+Reg('r8 ',State.R8)+Reg('r9 ',State.R9)+Reg('r10',State.R10)+LineEnding+
                '  '+Reg('r11',State.R11)+Reg('r12',State.R12)+Reg('r13',State.R13)+LineEnding+
                '  '+Reg('r14',State.R14)+Reg('r15',State.R15)+'eflags='+IntToHex(State.EFlags,8)+LineEnding;
{$endif}
{$ifdef PasVulkanCrashReportX86}
 result:=result+'  '+Reg('eip',State.Eip)+Reg('esp',State.Esp)+Reg('ebp',State.Ebp)+LineEnding+
                '  '+Reg('eax',State.Eax)+Reg('ebx',State.Ebx)+Reg('ecx',State.Ecx)+LineEnding+
                '  '+Reg('edx',State.Edx)+Reg('esi',State.Esi)+Reg('edi',State.Edi)+LineEnding+
                '  eflags='+IntToHex(State.EFlags,8)+LineEnding;
{$endif}
end;
{$else}
begin
 result:='';
end;
{$ifend}

function pvCrashReportModules:String;
{$if defined(Windows)}
const TH32CS_SNAPMODULE=TpvUInt32($00000008);
      TH32CS_SNAPMODULE32=TpvUInt32($00000010);
var Snapshot:THandle;
    Entry:TpvCrashReportModuleEntry32;
    Count:TpvInt32;
begin

 result:='';

 Snapshot:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE or TH32CS_SNAPMODULE32,GetCurrentProcessId);
 if Snapshot=THandle(-1) then begin
  exit;
 end;

 try

  Count:=0;
  FillChar(Entry,SizeOf(TpvCrashReportModuleEntry32),#0);
  Entry.Size:=SizeOf(TpvCrashReportModuleEntry32);
  if not Module32FirstW(Snapshot,Entry) then begin
   exit;
  end;

  repeat
   inc(Count);
   result:=result+'  '+IntToHex(TpvPtrUInt(Entry.BaseAddress),SizeOf(TpvPointer) shl 1)+
                  '-'+IntToHex(TpvPtrUInt(Entry.BaseAddress)+Entry.BaseSize,SizeOf(TpvPointer) shl 1)+
                  '  '+String(PWideChar(@Entry.ExePath[0]))+LineEnding;
  until not Module32NextW(Snapshot,Entry);

  if Count>0 then begin
   result:='Loaded modules:'+LineEnding+result;
  end;

 finally
  CloseHandle(Snapshot);
 end;

end;
{$elseif defined(Linux) or defined(Android)}
var Maps,Line,Previous:TpvRawByteString;
    Start,Stop,SlashPosition:TpvSizeInt;
begin

 result:='';
 Previous:='';

 // The process map already lists every file backed mapping with the address it
 // sits at, so nothing else has to be asked. Only the first mapping of each
 // file is kept, which is where the module begins.
 Maps:=CrashReportReadProcFile('/proc/self/maps');

 Start:=1;
 while Start<=length(Maps) do begin
  Stop:=Start;
  while (Stop<=length(Maps)) and (Maps[Stop]<>#10) do begin
   inc(Stop);
  end;
  Line:=Copy(Maps,Start,Stop-Start);
  Start:=Stop+1;
  SlashPosition:=Pos(TpvRawByteString('/'),Line);
  if SlashPosition>0 then begin
   if Copy(Line,SlashPosition,length(Line))<>Previous then begin
    Previous:=Copy(Line,SlashPosition,length(Line));
    result:=result+'  '+String(Line)+LineEnding;
   end;
  end;
 end;

 if length(result)>0 then begin
  result:='Loaded modules:'+LineEnding+result;
 end;

end;
{$else}
begin
 result:='';
end;
{$ifend}

function pvCrashReportThreadStacks(const aMaximalThreads:TpvInt32):String;
{$if defined(Windows) and (defined(PasVulkanCrashReportX64) or defined(PasVulkanCrashReportX86))}
const TH32CS_SNAPTHREAD=TpvUInt32($00000004);
      THREAD_GET_CONTEXT=TpvUInt32($0008);
      THREAD_QUERY_INFORMATION=TpvUInt32($0040);
      THREAD_SUSPEND_RESUME=TpvUInt32($0002);
var Snapshot,Thread:THandle;
    Entry:TpvCrashReportThreadEntry32;
    // GetThreadContext wants a CONTEXT aligned to sixteen bytes, because of the
    // vector register area inside it, and refuses to fill in one which is not.
    // Whether a local variable of that type gets that alignment is up to the
    // compiler, and a refusal here is silent: the call simply returns false and
    // the thread looks unreadable. So the room is taken as plain bytes and the
    // alignment is done here, where it does not depend on anybody.
    ContextBuffer:array[0..SizeOf(TContext)+15] of TpvUInt8;
    Context:PContext;
    Frames:array[0..cMaximalStackFrames-1] of TpvPointer;
    Count,Index,Handled:TpvInt32;
    ProcessID,CurrentID:TpvUInt32;
    Text:String;
begin

 result:='';

 ProcessID:=GetCurrentProcessId;
 CurrentID:=GetCurrentThreadId;

 Context:=PContext(TpvPointer((TpvPtrUInt(@ContextBuffer[0])+15) and not TpvPtrUInt(15)));

 Snapshot:=CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD,0);
 if Snapshot=THandle(-1) then begin
  exit;
 end;

 try

  Handled:=0;

  FillChar(Entry,SizeOf(TpvCrashReportThreadEntry32),#0);
  Entry.Size:=SizeOf(TpvCrashReportThreadEntry32);
  if not Thread32First(Snapshot,Entry) then begin
   exit;
  end;

  repeat

   if (Entry.OwnerProcessID<>ProcessID) or (Entry.ThreadID=CurrentID) then begin
    continue;
   end;

   if Handled>=aMaximalThreads then begin
    result:=result+'  (more threads exist, stopped after '+IntToStr(aMaximalThreads)+')'+LineEnding;
    break;
   end;
   inc(Handled);

   Thread:=OpenThread(THREAD_GET_CONTEXT or THREAD_QUERY_INFORMATION or THREAD_SUSPEND_RESUME,false,Entry.ThreadID);
   if Thread=0 then begin
    result:=result+'Thread $'+IntToHex(Entry.ThreadID,8)+', could not be opened'+LineEnding;
    continue;
   end;

   try

    // A running thread has no stack worth reading, since it changes underneath.
    if SuspendThread(Thread)=TpvUInt32($ffffffff) then begin
     result:=result+'Thread $'+IntToHex(Entry.ThreadID,8)+', could not be suspended'+LineEnding;
     continue;
    end;

    // Nothing between here and the resume below may allocate, take a lock or
    // touch a file. The suspended thread can be holding the lock of the heap or
    // of anything else, and waiting for it while its owner is stopped is a
    // deadlock, in the one situation where the program has to keep going long
    // enough to write its report. So the frames are only collected here, by
    // reading memory and unwinding, and turned into text further down once the
    // thread is running again.
    Count:=-1;
    try
     FillChar(Context^,SizeOf(TContext),#0);
     Context^.ContextFlags:=CONTEXT_FULL;
     if GetThreadContext(Thread,Context^) then begin
      FillChar(Frames,SizeOf(Frames),#0);
      Count:=CrashReportWalkSuspendedThread(Context^,@Frames[0],cMaximalStackFrames);
     end;
    finally
     ResumeThread(Thread);
    end;

    if Count>=0 then begin
     result:=result+'Thread $'+IntToHex(Entry.ThreadID,8)+', '+IntToStr(Count)+' frames:'+LineEnding;
     for Index:=0 to Count-1 do begin
      // Every frame but the first is a return address, so it points behind the
      // call rather than at it.
      result:=result+'  '+pvCrashReportFormatAddress(Frames[Index],Index>0)+LineEnding;
     end;
    end else begin
     result:=result+'Thread $'+IntToHex(Entry.ThreadID,8)+', context not readable'+LineEnding;
    end;

   finally
    CloseHandle(Thread);
   end;

  until not Thread32Next(Snapshot,Entry);

  if Handled=0 then begin
   result:='No other threads were running.'+LineEnding;
  end else begin
   result:='Stacks of the other '+IntToStr(Handled)+' threads of this process:'+LineEnding+result;
  end;

 finally
  CloseHandle(Snapshot);
 end;

end;
{$else}
begin
 // Not answered elsewhere. Reading the stack of another thread means stopping
 // it first, and the ways of doing that on a unix, a signal handler which
 // captures its own context or a helper process attached through ptrace, both
 // reach a good deal further into the program than a crash logger should on its
 // own. Saying so is better than returning an empty string which reads like
 // there was nothing to report.
 result:='Stacks of other threads are not available on this platform.'+LineEnding;
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
 // All three, not just the first. Somebody who set only one of them is in a
 // mixed state which taking over halfway would make worse.
 if not (assigned(Exception.GetExceptionStackInfoProc) or
         assigned(Exception.GetStackInfoStringProc) or
         assigned(Exception.CleanUpStackInfoProc)) then begin
  Exception.GetExceptionStackInfoProc:=CrashReportGetExceptionStackInfoProc;
  Exception.GetStackInfoStringProc:=CrashReportGetStackInfoStringProc;
  Exception.CleanUpStackInfoProc:=CrashReportCleanUpStackInfoProc;
  CrashReportOwnsStackInfoProcs:=true;
 end;
{$endif}
{$if defined(Windows)}
 // The slot the vectored handler guards itself with. Allocated before the
 // handler goes in, so that it is there from the first exception on.
 CrashReportTLSIndex:=TlsAlloc;
 // Installed on both compilers, and on Delphi even when the stack info hooks
 // above went to somebody else. It is what fills the history, so giving way
 // there must not mean giving up the record of what led to the crash. It also
 // sees faults on threads the runtime has never heard of, which no hook of the
 // runtime can.
 CrashReportVectoredHandle:=AddVectoredExceptionHandler(1,@CrashReportVectoredExceptionHandler);
{$elseif defined(fpc)}
{$if defined(Linux) or defined(Android)}
 // Found now rather than in the middle of a crash, where opening a library is
 // the last thing worth doing.
 CrashReportLoadUnwinder;
{$ifend}
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
 // Only taken back when these are still the ones which were put in. Somebody
 // else may have installed themselves in the meantime, and clearing their
 // hooks would leave them without the ones they rely on.
 if CrashReportOwnsStackInfoProcs then begin
  // Each one on its own, and compared through a pointer, since these are plain
  // procedure variables and reading one in an expression would call it.
  if PPointer(@Exception.GetExceptionStackInfoProc)^=TpvPointer(@CrashReportGetExceptionStackInfoProc) then begin
   Exception.GetExceptionStackInfoProc:=nil;
  end;
  if PPointer(@Exception.GetStackInfoStringProc)^=TpvPointer(@CrashReportGetStackInfoStringProc) then begin
   Exception.GetStackInfoStringProc:=nil;
  end;
  if PPointer(@Exception.CleanUpStackInfoProc)^=TpvPointer(@CrashReportCleanUpStackInfoProc) then begin
   Exception.CleanUpStackInfoProc:=nil;
  end;
  CrashReportOwnsStackInfoProcs:=false;
 end;
{$endif}
{$if defined(Windows)}
 if assigned(CrashReportVectoredHandle) then begin
  RemoveVectoredExceptionHandler(CrashReportVectoredHandle);
  CrashReportVectoredHandle:=nil;
 end;
 if CrashReportTLSIndex<>TpvUInt32($ffffffff) then begin
  TlsFree(CrashReportTLSIndex);
  CrashReportTLSIndex:=TpvUInt32($ffffffff);
 end;
{$elseif defined(fpc)}
 // Same reasoning as above, only put back when nobody else has taken over.
 if PPointer(@System.RaiseProc)^=TpvPointer(@CrashReportRaiseProc) then begin
  System.RaiseProc:=CrashReportOldRaiseProc;
 end;
 CrashReportOldRaiseProc:=nil;
{$ifend}
 CrashReportAcquireModuleLock;
 try
  while CrashReportModuleCount>0 do begin
   dec(CrashReportModuleCount);
   FreeAndNil(CrashReportModules[CrashReportModuleCount].Table);
   CrashReportModules[CrashReportModuleCount].Key:=0;
   CrashReportModules[CrashReportModuleCount].FileName:='';
   CrashReportModules[CrashReportModuleCount].RVABase:=0;
  end;
  CrashReportModules:=nil;
 finally
  CrashReportReleaseModuleLock;
 end;
end;

initialization

 pvCrashReportInstall;

finalization

 pvCrashReportUninstall;

end.
