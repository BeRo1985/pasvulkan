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

// Which instruction set this is being built for, as far as reading registers
// and walking stacks is concerned. Asking for a 64 bit cpu is not the same
// question: an ARM64 build is 64 bit as well and has neither the registers nor
// the unwind tables any of this uses.
//
// Settled before the uses clause, since what is needed there depends on it.
{$if defined(cpux86_64) or defined(cpuamd64) or defined(cpux64)}
 {$define PasVulkanCrashReportX64}
{$elseif defined(cpu386) or defined(cpui386) or defined(cpux86)}
 {$define PasVulkanCrashReportX86}
{$ifend}

// Whether the signal based stacks of other threads are actually built. Asked
// for by PasVulkanCrashReportUnixThreadStacks, but only possible where the
// layout of the context a signal handler is given is known, which is to say on
// the two architectures above and nowhere else.
{$if defined(fpc) and defined(Linux) and defined(PasVulkanCrashReportUnixThreadStacks) and (defined(PasVulkanCrashReportX64) or defined(PasVulkanCrashReportX86))}
 {$define PasVulkanCrashReportUnixThreadStacksBuilt}
{$ifend}

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
     {$ifdef PasVulkanCrashReportUnixThreadStacksBuilt}
      // Only for the signal based thread stacks, which are the one thing here
      // which has to reach into the operating system past what the runtime
      // already offers.
      BaseUnix,
     {$endif}
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
//   Delphi on Windows      : Exception.GetExceptionStackInfoProc and friends.
//                            These fire at the raise point for language level
//                            raises and, through GetExceptionObject, also for
//                            hardware faults.
//                            Windows only, and deliberately so: the stack
//                            capture behind those hooks goes through
//                            RtlCaptureStackBackTrace, and the address
//                            formatting through VirtualQuery and module
//                            handles. A Delphi build for Linux or macOS would
//                            need a capture of its own, which nothing here
//                            asks for. FreePascal is where this unit is
//                            actually cross platform.
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

      // The entry carries the processor state of the fault it describes.
      pvCrashReportFlagRegisters=TpvUInt32(2);

type
{$if defined(Windows) and (defined(PasVulkanCrashReportX64) or defined(PasVulkanCrashReportX86))}
     // The processor state at the moment of a fault, taken straight out of the
     // context the operating system hands to the handler, which was there all
     // along and simply went unused. Half the questions an address only log
     // leaves open are answered by looking at what was in the registers.
     //
     // Part of the ring buffer entry rather than a place of its own, so that
     // the sequence number of the entry publishes the registers together with
     // everything else about that fault. Two threads which fault at the same
     // moment write into two different slots and cannot mix.
     TpvCrashReportFaultRegisters=record
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
     end;
{$ifend}

     PpvCrashReportEntry=^TpvCrashReportEntry;
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
{$if defined(Windows) and (defined(PasVulkanCrashReportX64) or defined(PasVulkanCrashReportX86))}
      // Only meaningful when Flags says so, see pvCrashReportFlagRegisters.
      Registers:TpvCrashReportFaultRegisters;
{$ifend}
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
//
// The frame arguments are used on both compilers. They used to be read only
// under FreePascal and ignored under Delphi, where the stack came from
// Exception.StackTrace instead; a caller which knew the addresses and said so
// therefore got a printed stack about one set of frames and an identifier below
// it about another. Where nothing is handed in, Delphi still uses the stack
// which was captured at the raise, which is the same one the identifier is
// built from.
//
// aAddressIsReturnAddress says which of the two kinds aAddress is, see
// pvCrashReportFormatAddress. A raise address handed over by Delphi is the
// address behind the raise; the one FreePascal reports and the one a hardware
// fault reports are the faulting instruction itself, which is the default here.
// Guessing it from the compiler would be wrong in both directions, since both
// kinds occur on both.
function pvCrashReportDumpException(const aException:Exception;const aAddress:TpvPointer=nil;const aFrameCount:TpvInt32=0;const aFrames:PPointer=nil;const aAddressIsReturnAddress:Boolean=false):String;

// A short identifier for where a crash happened, so that two reports of the
// same fault can be recognized as one thing seen twice rather than read one by
// one. Empty when nothing on the stack could be named, since an identifier made
// only of the exception class would be the same for every crash of that class
// and would say nothing at all.
//
// Built from the names of the topmost routines rather than from their
// addresses. An address moves with every run under address space layout
// randomization, and with every build which merely shifted code around, while
// the names stay where they are, so the same defect keeps its identifier across
// both.
//
// The frames are the ones of the exception being handled. On FreePascal that
// means ExceptAddr and ExceptFrames unless they are given here, and those are
// only meaningful inside an except block, so calling this from outside one
// yields the identifier of whatever was handled last. Pass the addresses in
// where they are known, which is what pvCrashReportDumpException does, so that
// the identifier and the printed stack are always about the same frames.
//
// An address handed in does not replace the frames which were captured at the
// raise, it is put in front of them: a caller who knows where the crash was and
// says so should not end up with a shorter identifier than one who said
// nothing. Only frames handed in replace captured frames, since those are two
// answers to the same question.
//
// aAddressIsReturnAddress, as in pvCrashReportDumpException, says which kind of
// address aAddress is, because that decides whether the name is looked up one
// byte earlier.
function pvCrashReportFingerprint(const aException:Exception=nil;const aMaximalNames:TpvInt32=5;const aFrameCount:TpvInt32=0;const aFrames:PPointer=nil;const aAddress:TpvPointer=nil;const aAddressIsReturnAddress:Boolean=false):String;

// Formats the processor state of the last fault of a thread. Empty when that
// thread had none, and empty on a platform which does not hand one over.
//
// Defaults to the calling thread, which is right where the thread which
// crashed writes its own report. An application which hands its crash logs to a
// logger thread of its own has to say whose state it wants, otherwise the
// logger asks about itself and gets nothing.
function pvCrashReportRegisters(const aThreadID:TpvUInt64=0):String;

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

// Everything around the exception itself: what led up to it, what was in the
// registers, what the other threads were doing, and what was loaded, in the
// order in which it is worth reading.
//
// For a caller which has already formatted the exception, which is what the
// crash log of PasVulkan.Application does.
//
// aThreadID says whose processor state to show, see pvCrashReportRegisters. It
// has to be reachable from here and from the full report as well, otherwise the
// one function which is meant to be the only one to call is the one which
// cannot answer it.
function pvCrashReportContext(const aThreadID:TpvUInt64=0):String;

// The exception and all of the above, in one piece.
//
// This exists so that there is one thing to call. The parts are public as well,
// but a shipping path which has to remember four of them is a shipping path
// which will be missing one of them, and it is the report from a player which
// then turns out to be missing it.
function pvCrashReportFullReport(const aException:Exception=nil;const aAddress:TpvPointer=nil;const aFrameCount:TpvInt32=0;const aFrames:PPointer=nil;const aThreadID:TpvUInt64=0;const aAddressIsReturnAddress:Boolean=false):String;

procedure pvCrashReportInstall;

procedure pvCrashReportUninstall;

implementation

const LineEnding={$if defined(Windows)}#13#10{$else}#10{$ifend};

      HexDigits:array[0..15] of AnsiChar='0123456789abcdef';

      cDelphiException=TpvUInt32($0eedfade);

      cFPCException=TpvUInt32($e0465043);

      // The code the runtime of this compiler raises its own exceptions with,
      // as opposed to a fault reported by the operating system.
      cLanguageException={$ifdef fpc}cFPCException{$else}cDelphiException{$endif};

      cAccessViolation=TpvUInt32($c0000005);

      cMaximalStackFrames=48;

      // How many readable ranges of the process the list starts out with. It
      // grows from there: a game with a graphics driver, a shader cache and a
      // few dozen shared libraries has well over a thousand of them, and a
      // range which did not fit would make the walk stop at the first frame in
      // it without saying why. Grown by the reporter, where allocating is
      // allowed, never by the signal handler.
      pvCrashReportInitialMappings=1024;

{$ifdef PasVulkanCrashReportFixedModuleCache}
const // How many modules the resolver remembers, when the cache is the fixed
      // size one. A process with more loaded than this stops resolving beyond
      // that point, which is why the growing cache is the default.
      pvCrashReportMaximalModules=32;
{$endif}

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

{$ifdef PasVulkanCrashReportUnixThreadStacksBuilt}
// Where a thread which was asked for its stack leaves the answer. One of these
// is enough, since the threads are asked one after another, and one is also all
// a signal handler can be given without a table it would have to look itself up
// in.
type TpvCrashReportUnixThreadSlot=record
      // Zero while nothing is being asked, one while a thread has been asked
      // and has not answered, two once it has.
      State:TpvInt32;
      // Which thread the open request is meant for, so that a handler which
      // was held up cannot answer a request which is no longer its own.
      WantedThreadID:TpvInt32;
      Count:TpvInt32;
      Frames:array[0..cMaximalStackFrames-1] of TpvPointer;
     end;

     // One readable range of the process, taken from the process map. Kept as
     // plain numbers so that the signal handler can look through them without
     // asking the operating system anything.
     TpvCrashReportMappingRange=record
      Low:TpvPtrUInt;
      High:TpvPtrUInt;
     end;

     // The registers inside the context a signal handler is given. Declared
     // wide enough for the largest of the two layouts, and only the few indices
     // which are actually read are ever touched.
     PpvCrashReportGeneralRegisters=^TpvCrashReportGeneralRegisters;
     TpvCrashReportGeneralRegisters=array[0..22] of TpvPtrUInt;

// Reaching past what the runtime offers: there is no wrapper for sending a
// signal to one thread rather than to the process, and none for asking for the
// own thread identifier, so both go through the system call gate directly.
function CrashReportSysCall(aNumber:TpvPtrInt):TpvPtrInt; cdecl; varargs; external name 'syscall';
{$endif}

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
{$if defined(fpc) and (defined(Linux) or defined(Android))}
    CrashReportUnwindBacktraceProc:TpvCrashReportUnwindBacktrace=nil;
    CrashReportUnwindGetIPProc:TpvCrashReportUnwindGetIP=nil;
    CrashReportUnwinderState:TpvInt32=0;
{$ifend}
{$ifdef PasVulkanCrashReportUnixThreadStacksBuilt}
    CrashReportUnixThreadSlot:TpvCrashReportUnixThreadSlot;
    CrashReportThreadSignalInstalled:Boolean=false;
    CrashReportMappings:array of TpvCrashReportMappingRange;
    CrashReportMappingCount:TpvInt32=0;
    // The action which was in place before the handler went in, so that an
    // uninstall can put it back rather than leave the signal claimed.
    CrashReportOldThreadSignalAction:SigActionRec;
{$endif}
{$ifdef PasVulkanCrashReportFixedModuleCache}
    CrashReportModules:array[0..pvCrashReportMaximalModules-1] of TpvCrashReportModuleEntry;
{$else}
    CrashReportModules:array of TpvCrashReportModuleEntry;
{$endif}
    CrashReportModuleCount:TpvInt32=0;
    CrashReportModuleLock:TpvInt32=0;
    // Non zero while somebody is collecting the stacks of other threads, see
    // CrashReportEnterThreadStacks.
    CrashReportThreadStacksBusy:TpvInt32=0;
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

// Android as well as Linux, because FreePascal does not define Linux for an
// Android target, it defines Android and Unix, and the module list below asks
// for this on both.
{$if defined(Linux) or defined(Android)}
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
{$if defined(Linux) or defined(Android)}
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
{$ifdef PasVulkanCrashReportFixedModuleCache}
   // A fixed number of entries, which is how this was first written. Once they
   // are used up nothing further is remembered, and since the table would then
   // be reloaded for every single frame it is not handed out either, so a
   // process with more modules than this loses the ones past the limit.
   if CrashReportModuleCount>=pvCrashReportMaximalModules then begin
    FreeAndNil(Table);
    result:=nil;
    aRVABase:=0;
    exit;
   end;
   Slot:=CrashReportModuleCount;
   inc(CrashReportModuleCount);
{$else}
   // Grown as needed instead, so that every module of the process can be named
   // however many there are. One entry is a handful of bytes, and a table is
   // only read for a module which actually turned up in a stack trace.
   Slot:=CrashReportModuleCount;
   if Slot>=length(CrashReportModules) then begin
    SetLength(CrashReportModules,(Slot+1)*2);
   end;
   inc(CrashReportModuleCount);
{$endif}
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
// The name of the routine an address sits in, on its own. The fingerprint is
// built from names rather than from formatted lines, so that neither the
// address nor the line number, both of which move for reasons which have
// nothing to do with the defect, end up in it.
function CrashReportSymbolNameOf(const aAddress:TpvPointer;const aReturnAddress:Boolean;out aName:String):Boolean;
var SymbolTable:TpvSymbolTable;
    Location:TpvSymbolTableLocation;
    LookupAddress,RVABase:TpvPtrUInt;
begin

 result:=false;
 aName:='';

 SymbolTable:=CrashReportTableForAddress(aAddress,RVABase);
 if not assigned(SymbolTable) then begin
  exit;
 end;
 if TpvPtrUInt(aAddress)<=RVABase then begin
  exit;
 end;

 LookupAddress:=TpvPtrUInt(aAddress);
 if aReturnAddress then begin
  dec(LookupAddress);
 end;

 if not SymbolTable.Resolve(TpvUInt64(LookupAddress-RVABase),Location) then begin
  exit;
 end;

 if length(Location.SymbolName)>0 then begin
  aName:=String(Location.SymbolName);
 end else if length(Location.UnitName)>0 then begin
  // No routine, but the unit still says more than nothing.
  aName:=String(Location.UnitName);
 end;

 result:=length(aName)>0;

end;

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

function pvCrashReportDumpException(const aException:Exception;const aAddress:TpvPointer;const aFrameCount:TpvInt32;const aFrames:PPointer;const aAddressIsReturnAddress:Boolean):String;
var Fingerprint:String;
    Index:TpvInt32;
    Frames:PPointer;
{$ifdef fpc}
    FrameCount:TpvInt32;
{$else}
    StackTrace:String;
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
 // What was handed in comes first, exactly as it does under FreePascal and
 // exactly as the identifier below reads it. An address does not replace the
 // captured stack, it stands in front of it; frames do replace it, since they
 // are another answer to the same question.
 if assigned(aAddress) then begin
  result:=result+'  '+pvCrashReportFormatAddress(aAddress,aAddressIsReturnAddress)+LineEnding;
 end;
 if assigned(aFrames) and (aFrameCount>0) then begin
  Frames:=aFrames;
  for Index:=0 to aFrameCount-1 do begin
   // The value in the slot, not the slot.
   result:=result+'  '+pvCrashReportFormatAddress(Frames^)+LineEnding;
   inc(Frames);
  end;
 end else begin
  if assigned(aException) then begin
   StackTrace:=aException.StackTrace;
  end else begin
   StackTrace:='';
  end;
  if length(StackTrace)>0 then begin
   // The stack which was captured at the raise. Where the hooks are the ones of
   // this unit, these are the very addresses the identifier below is built
   // from. Where they belong to another tool, this is that tool's text and the
   // identifier falls back to the recorded raise address, which is then the
   // only thing both sides can agree on.
   result:=result+StackTrace;
   if StackTrace[length(StackTrace)]<>#10 then begin
    result:=result+LineEnding;
   end;
  end else if not assigned(aAddress) then begin
   // No stack was captured at the raise point, so this is the stack of the
   // handler rather than of the raise. Say so, instead of quietly pretending
   // otherwise.
   result:=result+'No stack trace was captured at the raise point, showing the handler stack instead:'+LineEnding+
                  pvCrashReportCaptureStackTrace(2);
  end;
 end;
{$endif}
 // The same addresses the stack above was printed from, so that the two cannot
 // describe different frames.
 Fingerprint:=pvCrashReportFingerprint(aException,5,aFrameCount,aFrames,aAddress,aAddressIsReturnAddress);
 if length(Fingerprint)>0 then begin
  result:=result+'Crash fingerprint: '+Fingerprint+LineEnding;
 end;
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

// Whether the block behind an exception is one of ours, which is to say whether
// the hook which makes those blocks is still the one installed here.
//
// The flag on its own does not answer that. It says the hooks were free when
// this unit installed itself, which is a statement about that moment and not
// about now: this unit installs from its own initialization, and anything which
// installs later, from a unit further down the uses clause or at any point
// while the program runs, simply assigns over it. The flag then still says yes
// while every exception carries somebody else's block, and reading that block
// through the layout here means following whatever their first field happens to
// be as a count and an array of addresses.
//
// Which is worse than it sounds, because the one place this is asked is while a
// crash is being described. A crash reporter which faults on the way to naming
// the fault takes the report with it.
//
// The address operator does the reading here, since these are plain procedure
// variables and naming one in an expression would call it instead. What it
// yields is what the variable holds, not where the variable is, so there is
// nothing left to dereference: doing so anyway reads the first bytes of the
// routine as if they were a pointer, which compares equal to nothing.
function CrashReportStackInfoProcsAreOurs:Boolean;
begin
 result:=CrashReportOwnsStackInfoProcs and
         (TpvPointer(@Exception.GetExceptionStackInfoProc)=TpvPointer(@CrashReportGetExceptionStackInfoProc));
end;
{$endif}

{$if defined(Windows)}
// Copies the processor state out of the context the handler was given, into the
// ring buffer entry of the fault it belongs to. Split out so that the handler
// itself stays readable, and kept to plain assignments so that it is as safe to
// call from there as everything else around it.
//
// Writing into the entry rather than into a place of its own is what makes this
// sound when two threads fault at the same moment: each has a slot of its own,
// and the sequence number of that slot publishes the registers along with the
// rest of the entry. A single shared record would have let one thread overwrite
// half of another one's registers, and the reader had no way of telling.
procedure CrashReportNoteFaultState(const aEntry:PpvCrashReportEntry;const aContext:TpvPointer);
{$if defined(Windows) and (defined(PasVulkanCrashReportX64) or defined(PasVulkanCrashReportX86))}
var Context:PContext;
begin
 if not assigned(aContext) then begin
  exit;
 end;
 Context:=PContext(aContext);
{$ifdef PasVulkanCrashReportX64}
 aEntry^.Registers.Rax:=Context^.Rax;
 aEntry^.Registers.Rbx:=Context^.Rbx;
 aEntry^.Registers.Rcx:=Context^.Rcx;
 aEntry^.Registers.Rdx:=Context^.Rdx;
 aEntry^.Registers.Rsi:=Context^.Rsi;
 aEntry^.Registers.Rdi:=Context^.Rdi;
 aEntry^.Registers.Rbp:=Context^.Rbp;
 aEntry^.Registers.Rsp:=Context^.Rsp;
 aEntry^.Registers.R8:=Context^.R8;
 aEntry^.Registers.R9:=Context^.R9;
 aEntry^.Registers.R10:=Context^.R10;
 aEntry^.Registers.R11:=Context^.R11;
 aEntry^.Registers.R12:=Context^.R12;
 aEntry^.Registers.R13:=Context^.R13;
 aEntry^.Registers.R14:=Context^.R14;
 aEntry^.Registers.R15:=Context^.R15;
 aEntry^.Registers.Rip:=Context^.Rip;
 aEntry^.Registers.EFlags:=Context^.EFlags;
{$endif}
{$ifdef PasVulkanCrashReportX86}
 aEntry^.Registers.Eax:=Context^.Eax;
 aEntry^.Registers.Ebx:=Context^.Ebx;
 aEntry^.Registers.Ecx:=Context^.Ecx;
 aEntry^.Registers.Edx:=Context^.Edx;
 aEntry^.Registers.Esi:=Context^.Esi;
 aEntry^.Registers.Edi:=Context^.Edi;
 aEntry^.Registers.Ebp:=Context^.Ebp;
 aEntry^.Registers.Esp:=Context^.Esp;
 aEntry^.Registers.Eip:=Context^.Eip;
 aEntry^.Registers.EFlags:=Context^.EFlags;
{$endif}
 aEntry^.Flags:=aEntry^.Flags or pvCrashReportFlagRegisters;
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
    Flags:TpvUInt32;
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
 // A vectored handler sees every exception in the process, and a good many of
 // them are not trouble at all: every OutputDebugString raises one, so does
 // naming a thread for the debugger, and some libraries throw C++ exceptions as
 // ordinary control flow. Sixty four places fill up with those in no time, and
 // the fault which actually mattered is then long gone.
 //
 // What is kept is what the runtime itself raised, and what the operating
 // system reports as an error. Severity alone does not separate the two,
 // because a C++ throw is $e06d7363 and a managed one $e0434352, and both of
 // those carry severity error as well. What sets them apart is bit 29, the
 // customer bit, which the operating system never sets and a language runtime
 // always does. So that bit is what excludes them, and it has to be looked at
 // after the check for the own runtime, whose code has it set too.
 if (ExceptionRecord^.ExceptionCode<>cFPCException) and
    (ExceptionRecord^.ExceptionCode<>cDelphiException) and
    (((ExceptionRecord^.ExceptionCode and TpvUInt32($c0000000))<>TpvUInt32($c0000000)) or
     ((ExceptionRecord^.ExceptionCode and TpvUInt32($20000000))<>0)) then begin
  exit;
 end;
 Sequence:=CrashReportNextSequence;
 Entry:=@CrashReportRingBuffer[(Sequence-1) and (pvCrashReportRingBufferSize-1)];
 // Either runtime, not only the one this was built with. A FreePascal library
 // inside a Delphi host, or the other way round, raises with the code of its
 // own runtime, and that is still a raise and not a fault.
 //
 // What is not done for the foreign one is looking into the exception object.
 // Not only because the message is an eight bit string on one side and a
 // sixteen bit one on the other: the class name cannot be read either, since
 // the two runtimes lay out their virtual method tables differently, so the
 // offset used below lands somewhere arbitrary in an object of the other one
 // and is then dereferenced as a string. The rule is: an object is only ever
 // read by the runtime which made it.
 if (ExceptionRecord^.ExceptionCode=cFPCException) or
    (ExceptionRecord^.ExceptionCode=cDelphiException) then begin

  // Which of the two raised it decides this, not which of the two this was
  // built with. FreePascal passes the address of the raise statement itself,
  // see fpc_RaiseException in rtl/win64/seh64.inc and rtl/win32/seh32.inc,
  // while Delphi passes the return address of the raise site. Deciding it at
  // compile time would put the adjustment on the foreign one exactly where it
  // does not belong, and one byte in the wrong direction is a different line,
  // or a different routine where the call was the last statement of one.
  if ExceptionRecord^.ExceptionCode=cDelphiException then begin
   Flags:=pvCrashReportFlagReturnAddress;
  end else begin
   Flags:=0;
  end;
  CrashReportEntryBegin(Entry,pvCrashReportKindRaise,0,ExceptionRecord^.ExceptionAddress,TpvUInt64(GetCurrentThreadId),Flags);

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
   if assigned(ExceptionObject) and
      (ExceptionRecord^.ExceptionCode=cLanguageException) and
      CrashReportEnterObjectInspection then begin
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
  CrashReportNoteFaultState(Entry,aExceptionInformation^.ContextRecord);
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

function pvCrashReportFingerprint(const aException:Exception;const aMaximalNames:TpvInt32;const aFrameCount:TpvInt32;const aFrames:PPointer;const aAddress:TpvPointer;const aAddressIsReturnAddress:Boolean):String;
var Addresses:array[0..cMaximalStackFrames-1] of TpvPointer;
    // Whether each of them sits behind its call rather than on it, which decides
    // where the name is looked up. It used to be worked out from the position:
    // the first is the raise point and everything after it is a return address.
    // That stops being true as soon as one of them comes out of the ring buffer,
    // where which of the two it is was decided at the moment it was recorded and
    // written down alongside it.
    ReturnAddresses:array[0..cMaximalStackFrames-1] of Boolean;
    Count,Index,Named:TpvInt32;
    Hash:TpvUInt64;
    Name:String;
    Frames:PPointer;
    FrameCount:TpvInt32;
{$ifndef fpc}
    StackInfo:PpvCrashReportStackInfo;
{$endif}
    Snapshot:TpvCrashReportEntry;
    Entry:PpvCrashReportEntry;
    Newest,Wanted:TpvUInt32;
    OwnThreadID:TpvUInt64;
    Scan,ScanCount:TpvInt32;
    Found:Boolean;

 procedure Feed(const aValue:String);
 var Position:TpvSizeInt;
 begin
  for Position:=1 to length(aValue) do begin
   Hash:=Hash xor TpvUInt64(Ord(aValue[Position]));
   Hash:=Hash*TpvUInt64($00000100000001b3);
  end;
  // A separator between the names, so that two different splits of the same
  // letters cannot come out the same.
  Hash:=Hash xor (Hash shr 29);
 end;

begin

 result:='';
 Count:=0;
 FillChar(Addresses,SizeOf(Addresses),#0);
 FillChar(ReturnAddresses,SizeOf(ReturnAddresses),#0);

 // The raise point first, then the frames behind it. The ones which were handed
 // in win, so that this and the printed stack are about the same frames.
 //
 // Asked of both compilers now. The arguments are there on both, and
 // pvCrashReportDumpException hands the same ones to both, but this used to
 // read them only under FreePascal and to go straight to the stack info block
 // under Delphi. A caller which knew the address and the frames and said so was
 // therefore describing one stack in the report and a different one in the
 // fingerprint of that same report.
 if assigned(aAddress) then begin
  Addresses[Count]:=aAddress;
  // Said by the caller, because only the caller knows. A raise address from
  // Delphi is the one behind the raise, the one FreePascal reports and the one
  // a hardware fault reports are the instruction itself, and both kinds turn up
  // on both compilers.
  ReturnAddresses[Count]:=aAddressIsReturnAddress;
  inc(Count);
 end;
 Frames:=aFrames;
 FrameCount:=aFrameCount;
{$ifdef fpc}
 if (Count=0) and assigned(ExceptAddr) then begin
  Addresses[Count]:=ExceptAddr;
  // The address of the raise statement itself on this compiler.
  ReturnAddresses[Count]:=false;
  inc(Count);
 end;
 if not (assigned(Frames) and (FrameCount>0)) then begin
  Frames:=ExceptFrames;
  FrameCount:=ExceptFrameCount;
 end;
{$endif}
 if assigned(Frames) and (FrameCount>0) then begin
  for Index:=0 to FrameCount-1 do begin
   if Count>=cMaximalStackFrames then begin
    break;
   end;
   Addresses[Count]:=Frames^;
   // Everything in a frame list is where a call goes back to.
   ReturnAddresses[Count]:=true;
   inc(Count);
   inc(Frames);
  end;
 end;
{$ifndef fpc}
 // The stack info block, which only says anything when the hooks in front of it
 // are the ones installed here: where somebody else owns them their block has a
 // layout of its own and reading it would be a guess.
 //
 // Only when no frames were handed in, the same way the frames of the exception
 // are only used under FreePascal when none were handed in. An address alone
 // does not turn this off: it is where the crash was, the block is how it got
 // there, and a caller who says the first should not thereby lose the second.
 // Asking for Count=0 here did exactly that, so naming the crash site made the
 // identifier of that crash less specific than saying nothing.
 if not (assigned(aFrames) and (aFrameCount>0)) and
    assigned(aException) and CrashReportStackInfoProcsAreOurs then begin
  StackInfo:=PpvCrashReportStackInfo(aException.StackInfo);
  if assigned(StackInfo) then begin
   for Index:=0 to StackInfo^.Count-1 do begin
    if Count>=cMaximalStackFrames then begin
     break;
    end;
    Addresses[Count]:=StackInfo^.Addresses[Index];
    // The block is filled by CrashReportCaptureFrames, whose entries are return
    // addresses throughout, the first one included: it is where the raise went
    // back to, not the raise itself.
    ReturnAddresses[Count]:=true;
    inc(Count);
   end;
  end;
 end;
{$endif}

 // And the last resort: the newest raise or fault this thread recorded.
 //
 // Under Delphi with madExcept or anything else owning the stack info hooks
 // there was nothing at all up to here, and the fingerprint came out empty,
 // which is the one output of this unit which then said less than the log next
 // to it. The vectored handler and the raise hook write into the ring buffer
 // whoever owns those hooks, so the address of the crash is known even when the
 // frames behind it are somebody else's business.
 //
 // One address makes a coarse fingerprint. It is still the difference between
 // grouping crashes by where they happened and not grouping them at all.
 if (Count=0) and (TpvUInt32(CrashReportSequence)<>0) then begin
  Newest:=TpvUInt32(CrashReportSequence);
  ScanCount:=pvCrashReportRingBufferSize;
  if TpvUInt32(ScanCount)>Newest then begin
   ScanCount:=TpvInt32(Newest);
  end;
  OwnThreadID:=CrashReportCurrentThreadID;
  Found:=false;
  // Taken as a copy and rechecked afterwards, since the slot can be taken over
  // while it is being read. The same dance as everywhere else in here.
  for Scan:=0 to ScanCount-1 do begin
   Wanted:=Newest-TpvUInt32(Scan);
   Entry:=@CrashReportRingBuffer[(Wanted-1) and (pvCrashReportRingBufferSize-1)];
   if Entry^.Sequence<>Wanted then begin
    continue;
   end;
   CrashReportReadBarrier;
   Snapshot:=Entry^;
   CrashReportReadBarrier;
   if (Entry^.Sequence=Wanted) and
      (Snapshot.ThreadID=OwnThreadID) and
      assigned(Snapshot.Address) and
      ((Snapshot.Kind=pvCrashReportKindRaise) or (Snapshot.Kind=pvCrashReportKindFault)) then begin
    Found:=true;
    break;
   end;
  end;
  if Found then begin
   Addresses[Count]:=Snapshot.Address;
   // Written down when it was recorded, because only there was it known.
   ReturnAddresses[Count]:=(Snapshot.Flags and pvCrashReportFlagReturnAddress)<>0;
   inc(Count);
  end;
 end;

 Hash:=TpvUInt64($cbf29ce484222325);

 if assigned(aException) then begin
  Feed(aException.ClassName);
 end;

 Named:=0;
 for Index:=0 to Count-1 do begin
  if Named>=aMaximalNames then begin
   break;
  end;
  // Whether it points behind its call or at it was decided where it was
  // collected, since that is the only place which knows.
  if CrashReportSymbolNameOf(Addresses[Index],ReturnAddresses[Index],Name) then begin
   Feed(Name);
   inc(Named);
  end;
 end;

 if Named=0 then begin
  // Nothing could be named, so anything built here would only say which class
  // it was, which every crash of that class would say too.
  exit;
 end;

 result:=IntToHex(Hash,16);

end;

function pvCrashReportRegisters(const aThreadID:TpvUInt64):String;
// Only Windows, because the vectored handler is the only place a processor
// context is handed over. On a unix that would take a signal handler, which is
// the same decision the thread stacks are waiting on.
{$if defined(Windows) and (defined(PasVulkanCrashReportX64) or defined(PasVulkanCrashReportX86))}
var Snapshot:TpvCrashReportEntry;
    Entry:PpvCrashReportEntry;
    Newest,Wanted:TpvUInt32;
    OwnThreadID:TpvUInt64;
    Index,Count:TpvInt32;
    Found:Boolean;

 function Reg(const aName:String;const aValue:TpvUInt64):String;
 begin
  result:=aName+'='+IntToHex(aValue,SizeOf(TpvPointer) shl 1)+' ';
 end;

begin

 result:='';

 Newest:=TpvUInt32(CrashReportSequence);
 if Newest=0 then begin
  exit;
 end;

 Count:=pvCrashReportRingBufferSize;
 if TpvUInt32(Count)>Newest then begin
  Count:=TpvInt32(Newest);
 end;

 // The newest entry which carries a processor state and belongs to the thread
 // asking, searched from the newest backwards. Taken as a copy and rechecked
 // afterwards, exactly as the history does it, since the slot can be taken over
 // while it is being read.
 //
 // The thread matters. A report is written by the thread which crashed, and a
 // fault which some other thread caught and carried on from is not the state
 // this report is about. Without that condition a language level exception
 // being fatal here would show the registers of an unrelated fault elsewhere,
 // and nothing in the output would say so.
 if aThreadID=0 then begin
  OwnThreadID:=CrashReportCurrentThreadID;
 end else begin
  OwnThreadID:=aThreadID;
 end;
 Found:=false;
 for Index:=0 to Count-1 do begin
  Wanted:=Newest-TpvUInt32(Index);
  Entry:=@CrashReportRingBuffer[(Wanted-1) and (pvCrashReportRingBufferSize-1)];
  if Entry^.Sequence<>Wanted then begin
   continue;
  end;
  CrashReportReadBarrier;
  Snapshot:=Entry^;
  CrashReportReadBarrier;
  if (Entry^.Sequence=Wanted) and
     ((Snapshot.Flags and pvCrashReportFlagRegisters)<>0) and
     (Snapshot.ThreadID=OwnThreadID) then begin
   Found:=true;
   break;
  end;
 end;

 if not Found then begin
  exit;
 end;

 result:='Processor state at fault #'+IntToStr(Snapshot.Sequence)+':'+LineEnding;
{$ifdef PasVulkanCrashReportX64}
 result:=result+'  '+Reg('rip',Snapshot.Registers.Rip)+Reg('rsp',Snapshot.Registers.Rsp)+Reg('rbp',Snapshot.Registers.Rbp)+LineEnding+
                '  '+Reg('rax',Snapshot.Registers.Rax)+Reg('rbx',Snapshot.Registers.Rbx)+Reg('rcx',Snapshot.Registers.Rcx)+LineEnding+
                '  '+Reg('rdx',Snapshot.Registers.Rdx)+Reg('rsi',Snapshot.Registers.Rsi)+Reg('rdi',Snapshot.Registers.Rdi)+LineEnding+
                '  '+Reg('r8 ',Snapshot.Registers.R8)+Reg('r9 ',Snapshot.Registers.R9)+Reg('r10',Snapshot.Registers.R10)+LineEnding+
                '  '+Reg('r11',Snapshot.Registers.R11)+Reg('r12',Snapshot.Registers.R12)+Reg('r13',Snapshot.Registers.R13)+LineEnding+
                '  '+Reg('r14',Snapshot.Registers.R14)+Reg('r15',Snapshot.Registers.R15)+'eflags='+IntToHex(Snapshot.Registers.EFlags,8)+LineEnding;
{$endif}
{$ifdef PasVulkanCrashReportX86}
 result:=result+'  '+Reg('eip',Snapshot.Registers.Eip)+Reg('esp',Snapshot.Registers.Esp)+Reg('ebp',Snapshot.Registers.Ebp)+LineEnding+
                '  '+Reg('eax',Snapshot.Registers.Eax)+Reg('ebx',Snapshot.Registers.Ebx)+Reg('ecx',Snapshot.Registers.Ecx)+LineEnding+
                '  '+Reg('edx',Snapshot.Registers.Edx)+Reg('esi',Snapshot.Registers.Esi)+Reg('edi',Snapshot.Registers.Edi)+LineEnding+
                '  eflags='+IntToHex(Snapshot.Registers.EFlags,8)+LineEnding;
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
      // How often the snapshot is asked for again. Taking one is documented to
      // fail while the module list is changing, which a process which loads and
      // unloads libraries does all the time, and the documented answer to that
      // is to ask again rather than to give up.
      cSnapshotAttempts=8;
var Snapshot:THandle;
    Entry:TpvCrashReportModuleEntry32;
    Count,Attempt:TpvInt32;
begin

 result:='';

 Snapshot:=THandle(-1);
 for Attempt:=1 to cSnapshotAttempts do begin
  Snapshot:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE or TH32CS_SNAPMODULE32,GetCurrentProcessId);
  if Snapshot<>THandle(-1) then begin
   break;
  end;
  Sleep(1);
 end;
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
var Maps,Line,Name:TpvRawByteString;
    Seen:TStringList;
    Start,Stop,SlashPosition:TpvSizeInt;
begin

 result:='';

 // The process map already lists every file backed mapping with the address it
 // sits at, so nothing else has to be asked. Only the first mapping of each
 // file is kept, which is where the module begins.
 Maps:=CrashReportReadProcFile('/proc/self/maps');

 Seen:=TStringList.Create;
 try

  // Every name seen so far rather than only the one before it, since the
  // mappings of one library are not always next to each other, and comparing
  // against the previous line alone lists such a library several times.
  Seen.Sorted:=true;
  Seen.Duplicates:=dupIgnore;

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
    Name:=Copy(Line,SlashPosition,length(Line));
    if Seen.IndexOf(String(Name))<0 then begin
     Seen.Add(String(Name));
     result:=result+'  '+String(Line)+LineEnding;
    end;
   end;
  end;

 finally
  FreeAndNil(Seen);
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

{$ifdef PasVulkanCrashReportUnixThreadStacksBuilt}
// Reading the stack of another thread on a unix means stopping it first, and
// there is no equivalent of suspending it from the outside. What there is, is
// asking it to stop itself: a signal is delivered on the thread it is sent to,
// and the handler is given the processor state of whatever that thread was
// doing. So the thread walks its own stack and leaves the answer behind.
//
// This is behind a define because it takes a signal away from the program. A
// real time signal is used, which nothing in the runtime and nothing in the
// usual libraries claims, but only the program itself can know that for sure,
// which is why this is its decision and not the default.
const // The first real time signal on glibc, where the two below it belong to
      // the thread library. That is an assumption about the C library and not a
      // rule: another one, musl for instance, divides them up differently, so a
      // build against one of those should check that this number is free before
      // turning the feature on.
      cCrashReportThreadSignal=34;
{$ifdef PasVulkanCrashReportX64}
      cCrashReportSysTgkill=234;
      cCrashReportSysGetTid=186;
      // Offsets into the context the handler is given, which is a ucontext:
      // eight bytes of flags, a link, a stack description of twenty four bytes,
      // and then the registers as an array of long long.
      cCrashReportContextGRegs=40;
      cCrashReportRegFramePointer=10;
      cCrashReportRegStackPointer=15;
      cCrashReportRegInstructionPointer=16;
{$endif}
{$ifdef PasVulkanCrashReportX86}
      cCrashReportSysTgkill=270;
      cCrashReportSysGetTid=224;
      cCrashReportContextGRegs=20;
      cCrashReportRegFramePointer=6;
      cCrashReportRegStackPointer=7;
      cCrashReportRegInstructionPointer=14;
{$endif}

      // How far above the stack pointer a frame is still believed to be one.
      cCrashReportMaximalStackSpan=TpvPtrUInt(16) shl 20;

function CrashReportGetTid:TpvInt32;
begin
 result:=TpvInt32(CrashReportSysCall(cCrashReportSysGetTid));
end;

// Whether the whole of the given range is inside one mapping which the process
// may read. The list is taken from the process map before any thread is asked,
// so this is a lookup and not a system call, which is what a signal handler
// needs it to be.
//
// The plausibility rules on their own, aligned and above the stack pointer and
// rising, do not say that a page is there at all. A damaged frame pointer can
// easily land on a hole, and a fault taken here happens inside the signal
// handler and takes the process down at the one moment it is meant to survive.
function CrashReportMappedForReading(const aAddress:TpvPtrUInt;const aSize:TpvPtrUInt):Boolean;
var Low,High,Middle:TpvInt32;
begin
 // Binary, since the process map comes out sorted by address and a process can
 // easily have more than a thousand entries, which the walk would otherwise
 // look through once per frame.
 result:=false;
 Low:=0;
 High:=CrashReportMappingCount-1;
 while Low<=High do begin
  Middle:=(Low+High) shr 1;
  if aAddress<CrashReportMappings[Middle].Low then begin
   High:=Middle-1;
  end else if aAddress>=CrashReportMappings[Middle].High then begin
   Low:=Middle+1;
  end else begin
   // The whole of the range has to be inside the same mapping, not only where
   // it starts.
   result:=(aAddress+aSize)<=CrashReportMappings[Middle].High;
   exit;
  end;
 end;
end;

// Runs on the thread which is being asked. Everything here has to be safe to do
// from a signal handler, which rules out allocating, locking and anything which
// could raise, so it reads registers and memory and nothing else.
//
// The walk is over the chain of saved frame pointers, since that is all a unix
// build of FreePascal leaves behind, and every step of it is checked rather
// than trusted: the frame has to be aligned, has to lie above the stack pointer
// and below the end of the mapping it is in, has to be readable, and has to be
// above the frame before it.
procedure CrashReportThreadSignalHandler(aSignal:TpvInt32;aInfo:TpvPointer;aContext:TpvPointer); cdecl;
var Registers:PpvCrashReportGeneralRegisters;
    Frame,NextFrame,ReturnAddress,StackPointer:TpvPtrUInt;
    Count:TpvInt32;
begin

 // Both halves of this, because an answer is only wanted while one is being
 // asked for, and only from the thread it was asked of. A handler which was
 // held up long enough for its request to have timed out would otherwise
 // answer the request after it, and one thread would be reported under the
 // number of another.
 if (CrashReportUnixThreadSlot.State<>1) or
    (CrashReportUnixThreadSlot.WantedThreadID<>CrashReportGetTid) then begin
  exit;
 end;

 Count:=0;

 if assigned(aContext) then begin

  Registers:=PpvCrashReportGeneralRegisters(TpvPointer(TpvPtrUInt(TpvPtrUInt(aContext)+cCrashReportContextGRegs)));

  StackPointer:=Registers^[cCrashReportRegStackPointer];

  // The instruction which was interrupted, which is the frame the thread is
  // actually in and the only one not read out of the stack.
  CrashReportUnixThreadSlot.Frames[Count]:=TpvPointer(Registers^[cCrashReportRegInstructionPointer]);
  inc(Count);

  Frame:=Registers^[cCrashReportRegFramePointer];

  while Count<cMaximalStackFrames do begin
   if (Frame<StackPointer) or
      ((Frame and (SizeOf(TpvPtrUInt)-1))<>0) or
      not CrashReportMappedForReading(Frame,SizeOf(TpvPtrUInt)*2) then begin
    break;
   end;
   NextFrame:=PpvPtrUInt(TpvPointer(Frame))^;
   ReturnAddress:=PpvPtrUInt(TpvPointer(TpvPtrUInt(Frame+SizeOf(TpvPtrUInt))))^;
   if ReturnAddress=0 then begin
    break;
   end;
   CrashReportUnixThreadSlot.Frames[Count]:=TpvPointer(ReturnAddress);
   inc(Count);
   if NextFrame<=Frame then begin
    break;
   end;
   Frame:=NextFrame;
  end;

 end;

 // Checked once more, because the walk above takes time and the request may
 // have been given up on in the meantime.
 if (CrashReportUnixThreadSlot.State<>1) or
    (CrashReportUnixThreadSlot.WantedThreadID<>CrashReportGetTid) then begin
  exit;
 end;

 CrashReportUnixThreadSlot.Count:=Count;
 CrashReportWriteBarrier;
 CrashReportUnixThreadSlot.State:=2;

end;

// Reads the readable ranges of the process out of the process map. Done by the
// reporter before it asks anybody, where allocating is allowed, so that the
// handler has nothing left to do but look through the result.
procedure CrashReportLoadMappings;
var Maps,Line:TpvRawByteString;
    Start,Stop,Position,DashPosition,SpacePosition:TpvSizeInt;

 function ParseHex(const aFrom,aTo:TpvSizeInt;out aValue:TpvPtrUInt):Boolean;
 var Index:TpvSizeInt;
     Digit:TpvUInt32;
 begin
  result:=aTo>=aFrom;
  aValue:=0;
  for Index:=aFrom to aTo do begin
   case Line[Index] of
    '0'..'9':begin
     Digit:=TpvUInt32(Ord(Line[Index])-Ord('0'));
    end;
    'a'..'f':begin
     Digit:=TpvUInt32(Ord(Line[Index])-Ord('a'))+10;
    end;
    'A'..'F':begin
     Digit:=TpvUInt32(Ord(Line[Index])-Ord('A'))+10;
    end;
    else begin
     result:=false;
     exit;
    end;
   end;
   aValue:=(aValue shl 4) or Digit;
  end;
 end;

var Low,High:TpvPtrUInt;
begin

 CrashReportMappingCount:=0;
 if length(CrashReportMappings)<pvCrashReportInitialMappings then begin
  SetLength(CrashReportMappings,pvCrashReportInitialMappings);
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
  DashPosition:=Pos(TpvRawByteString('-'),Line);
  SpacePosition:=Pos(TpvRawByteString(' '),Line);
  if (DashPosition<2) or (SpacePosition<(DashPosition+2)) then begin
   continue;
  end;

  // Only what may be read, which leaves out the guard pages a stack is bounded
  // by, and those are exactly what a walk has to stop at.
  Position:=SpacePosition+1;
  if (Position>length(Line)) or (Line[Position]<>'r') then begin
   continue;
  end;

  if ParseHex(1,DashPosition-1,Low) and ParseHex(DashPosition+1,SpacePosition-1,High) and (High>Low) then begin
   if CrashReportMappingCount>=length(CrashReportMappings) then begin
    SetLength(CrashReportMappings,(CrashReportMappingCount+1)*2);
   end;
   CrashReportMappings[CrashReportMappingCount].Low:=Low;
   CrashReportMappings[CrashReportMappingCount].High:=High;
   inc(CrashReportMappingCount);
  end;

 end;

end;

// Puts the handler in place. Called from the install, so that the signal is
// claimed at a quiet moment rather than while a crash is being written up.
procedure CrashReportInstallThreadSignalHandler;
var Action:SigActionRec;
begin
 FillChar(Action,SizeOf(SigActionRec),#0);
 Action.sa_handler:=SigActionHandler(TpvPointer(@CrashReportThreadSignalHandler));
 FpSigEmptySet(Action.sa_mask);
 // Asking for the processor state, and for interrupted system calls to be
 // resumed rather than to fail, since the thread being asked is a bystander and
 // must not notice this beyond a moment of delay.
 Action.sa_flags:=SA_SIGINFO or SA_RESTART;
 // The previous one is kept, so that an uninstall can put it back instead of
 // leaving the signal claimed by a handler which is no longer wanted.
 FillChar(CrashReportOldThreadSignalAction,SizeOf(SigActionRec),#0);
 if FpSigAction(cCrashReportThreadSignal,@Action,@CrashReportOldThreadSignalAction)=0 then begin
  CrashReportThreadSignalInstalled:=true;
 end;
end;

procedure CrashReportUninstallThreadSignalHandler;
var Current:SigActionRec;
begin
 if CrashReportThreadSignalInstalled then begin
  // Only put back when the handler in place is still the one which was put
  // there. Somebody else may have claimed the signal in the meantime, and
  // restoring over them would leave them without the handler they installed.
  // Same reasoning as for the stack info hooks and for RaiseProc.
  //
  // Read with the address operator and not through it. sa_handler is a field of
  // a procedural type, and on such a thing the address operator already yields
  // what is stored rather than where it is stored, so a further dereference
  // takes the first bytes of the handler for a pointer and matches nothing.
  // Which meant this never put the signal back.
  FillChar(Current,SizeOf(SigActionRec),#0);
  if (FpSigAction(cCrashReportThreadSignal,nil,@Current)=0) and
     (TpvPointer(@Current.sa_handler)=TpvPointer(@CrashReportThreadSignalHandler)) then begin
   FpSigAction(cCrashReportThreadSignal,@CrashReportOldThreadSignalAction,nil);
  end;
  CrashReportThreadSignalInstalled:=false;
 end;
end;

function CrashReportUnixThreadStacks(const aMaximalThreads:TpvInt32):String;
const cWaitRounds=20000;
var SearchRec:TSearchRec;
    ThreadID,OwnThreadID,ProcessID:TpvInt32;
    Handled,Index,Round,Count:TpvInt32;
    Frames:array[0..cMaximalStackFrames-1] of TpvPointer;
    Code:TpvInt32;
begin

 result:='';

 if not CrashReportThreadSignalInstalled then begin
  result:='Stacks of other threads were not asked for, since the signal they need was not claimed.'+LineEnding;
  exit;
 end;

 OwnThreadID:=CrashReportGetTid;
 ProcessID:=FpGetPid;
 Handled:=0;

 // Read once, here, so that the handlers have the bounds they need without
 // having to ask for them themselves.
 CrashReportLoadMappings;

 if FindFirst('/proc/self/task/*',faAnyFile,SearchRec)<>0 then begin
  exit;
 end;

 try

  repeat

   Val(SearchRec.Name,ThreadID,Code);
   if (Code<>0) or (ThreadID<=0) or (ThreadID=OwnThreadID) then begin
    continue;
   end;

   if Handled>=aMaximalThreads then begin
    result:=result+'  (more threads exist, stopped after '+IntToStr(aMaximalThreads)+')'+LineEnding;
    break;
   end;
   inc(Handled);

   CrashReportUnixThreadSlot.Count:=0;
   CrashReportUnixThreadSlot.WantedThreadID:=ThreadID;
   CrashReportWriteBarrier;
   CrashReportUnixThreadSlot.State:=1;
   CrashReportWriteBarrier;

   if CrashReportSysCall(cCrashReportSysTgkill,ProcessID,ThreadID,cCrashReportThreadSignal)<>0 then begin
    CrashReportUnixThreadSlot.WantedThreadID:=0;
    CrashReportUnixThreadSlot.State:=0;
    result:=result+'Thread '+IntToStr(ThreadID)+', could not be reached'+LineEnding;
    continue;
   end;

   // Waiting rather than blocking on anything, since whatever this would block
   // on could be held by the very thread which is being asked. A thread which
   // never answers is left alone and reported as such.
   Round:=0;
   while (CrashReportUnixThreadSlot.State<>2) and (Round<cWaitRounds) do begin
    inc(Round);
    ThreadSwitch;
   end;

   if CrashReportUnixThreadSlot.State<>2 then begin
    CrashReportUnixThreadSlot.WantedThreadID:=0;
    CrashReportUnixThreadSlot.State:=0;
    result:=result+'Thread '+IntToStr(ThreadID)+', did not answer'+LineEnding;
    continue;
   end;

   CrashReportReadBarrier;
   Count:=CrashReportUnixThreadSlot.Count;
   if Count>cMaximalStackFrames then begin
    Count:=cMaximalStackFrames;
   end;
   for Index:=0 to Count-1 do begin
    Frames[Index]:=CrashReportUnixThreadSlot.Frames[Index];
   end;
   CrashReportUnixThreadSlot.WantedThreadID:=0;
   CrashReportUnixThreadSlot.State:=0;

   result:=result+'Thread '+IntToStr(ThreadID)+', '+IntToStr(Count)+' frames:'+LineEnding;
   for Index:=0 to Count-1 do begin
    // Everything but the interrupted instruction is a return address, so it
    // points behind its call rather than at it.
    result:=result+'  '+pvCrashReportFormatAddress(Frames[Index],Index>0)+LineEnding;
   end;

  until FindNext(SearchRec)<>0;

  if Handled=0 then begin
   result:='No other threads were running.'+LineEnding;
  end else begin
   result:='Stacks of the other '+IntToStr(Handled)+' threads of this process:'+LineEnding+result;
  end;

 finally
  FindClose(SearchRec);
 end;

end;
{$endif}

// Takes the right to collect the stacks of other threads, or reports that
// somebody else already has it. Never waits.
//
// Two threads writing a report at the same time, which is what two faults at
// the same time amount to, would otherwise walk into each other. On Windows
// each would suspend the other and neither would ever reach its resume, and the
// process would stop exactly where it was supposed to explain itself. On a unix
// they would both write into the one request slot and get each other's answers.
// Neither is worth a lock, since there is nothing useful for the second one to
// wait for: the first is already collecting the same stacks.
function CrashReportEnterThreadStacks:Boolean;
begin
{$ifdef fpc}
 result:=InterLockedExchange(CrashReportThreadStacksBusy,1)=0;
{$else}
 result:=AtomicExchange(CrashReportThreadStacksBusy,1)=0;
{$endif}
end;

procedure CrashReportLeaveThreadStacks;
begin
 CrashReportWriteBarrier;
 CrashReportThreadStacksBusy:=0;
end;

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

 if not CrashReportEnterThreadStacks then begin
  result:='Stacks of other threads are already being collected elsewhere.'+LineEnding;
  exit;
 end;
 try

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

 finally
  CrashReportLeaveThreadStacks;
 end;

end;
{$elseif defined(PasVulkanCrashReportUnixThreadStacksBuilt)}
begin
 if not CrashReportEnterThreadStacks then begin
  result:='Stacks of other threads are already being collected elsewhere.'+LineEnding;
  exit;
 end;
 try
  result:=CrashReportUnixThreadStacks(aMaximalThreads);
 finally
  CrashReportLeaveThreadStacks;
 end;
end;
{$else}
begin
 // Not answered here. Reading the stack of another thread on a unix means
 // asking it to stop itself through a signal, and taking a signal away from the
 // program is not something a crash logger should do on its own, so it is
 // behind PasVulkanCrashReportUnixThreadStacks. Saying so is better than
 // returning an empty string, which reads like there was nothing to report.
 result:='Stacks of other threads are not available on this platform.'+LineEnding;
end;
{$ifend}

function pvCrashReportContext(const aThreadID:TpvUInt64):String;
var Part:String;
begin

 result:=pvCrashReportHistory+LineEnding;

 // Each of these answers with an explanation of its own where it cannot answer,
 // so an empty one really does mean there was nothing, and is left out rather
 // than printed as a heading with nothing under it.
 Part:=pvCrashReportRegisters(aThreadID);
 if length(Part)>0 then begin
  result:=result+Part+LineEnding;
 end;

 // The thread which crashed is often only the one which noticed. With a job
 // system the one which caused it is somewhere else entirely, and its stack is
 // not in the report unless it is asked for.
 Part:=pvCrashReportThreadStacks;
 if length(Part)>0 then begin
  result:=result+Part+LineEnding;
 end;

 // Last, because it is the longest and the least often read, and because an
 // address which nothing could name is only placeable with it.
 Part:=pvCrashReportModules;
 if length(Part)>0 then begin
  result:=result+Part+LineEnding;
 end;

end;

function pvCrashReportFullReport(const aException:Exception;const aAddress:TpvPointer;const aFrameCount:TpvInt32;const aFrames:PPointer;const aThreadID:TpvUInt64;const aAddressIsReturnAddress:Boolean):String;
begin
 result:=pvCrashReportDumpException(aException,aAddress,aFrameCount,aFrames,aAddressIsReturnAddress)+LineEnding+
         pvCrashReportContext(aThreadID);
end;

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
{$ifdef PasVulkanCrashReportUnixThreadStacksBuilt}
 FillChar(CrashReportUnixThreadSlot,SizeOf(TpvCrashReportUnixThreadSlot),#0);
 CrashReportInstallThreadSignalHandler;
{$endif}
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
  // Each one on its own, and read with the address operator, since these are
  // plain procedure variables and naming one in an expression would call it.
  //
  // Without the dereference which used to be here. The address operator already
  // yields what the variable holds rather than where it is, so reading through
  // it once more took the first bytes of the routine for a pointer and compared
  // those against its address. That never matched, which means none of these
  // three was ever put back and an uninstall left the hooks pointing here. In a
  // library which is then unloaded, the runtime calls into the space where this
  // code used to be at the next exception.
  if TpvPointer(@Exception.GetExceptionStackInfoProc)=TpvPointer(@CrashReportGetExceptionStackInfoProc) then begin
   Exception.GetExceptionStackInfoProc:=nil;
  end;
  if TpvPointer(@Exception.GetStackInfoStringProc)=TpvPointer(@CrashReportGetStackInfoStringProc) then begin
   Exception.GetStackInfoStringProc:=nil;
  end;
  if TpvPointer(@Exception.CleanUpStackInfoProc)=TpvPointer(@CrashReportCleanUpStackInfoProc) then begin
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
 // Same reasoning as above, only put back when nobody else has taken over, and
 // read the same way: the address operator on a procedural variable is already
 // the reading. The dereference which used to be here made the comparison fail
 // always, so this never gave the raise hook back either.
 if TpvPointer(@System.RaiseProc)=TpvPointer(@CrashReportRaiseProc) then begin
  System.RaiseProc:=CrashReportOldRaiseProc;
 end;
 CrashReportOldRaiseProc:=nil;
{$ifend}
{$ifdef PasVulkanCrashReportUnixThreadStacksBuilt}
 CrashReportUninstallThreadSignalHandler;
{$endif}
 // The symbol tables are deliberately not given back here.
 //
 // CrashReportTableForAddress hands a table out and lets go of the lock, and
 // the caller then formats with it. Freeing them here would pull that out from
 // under a thread which is halfway through writing a report, which is exactly
 // the situation this runs in: an uninstall during shutdown after a crash. The
 // memory is a few megabytes at most and the process is on its way out, so
 // leaving it to the operating system is both cheaper and safer than a lock
 // which would have to be held for the whole of every report.
 //
 // A leak checker will therefore list them. That is the intended trade and not
 // an oversight: a report which is still being written is worth more than a
 // clean shutdown count.
end;

initialization

 pvCrashReportInstall;

finalization

 pvCrashReportUninstall;

end.
