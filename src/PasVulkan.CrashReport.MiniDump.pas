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
unit PasVulkan.CrashReport.MiniDump;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

// Which instruction set this is being built for, asked the same way and for the
// same reason as in PasVulkan.CrashReport: the register layouts below depend on
// it and on nothing else.
{$if defined(cpux86_64) or defined(cpuamd64) or defined(cpux64)}
 {$define PasVulkanCrashReportMiniDumpX64}
{$elseif defined(cpu386) or defined(cpui386) or defined(cpux86)}
 {$define PasVulkanCrashReportMiniDumpX86}
{$ifend}

// Whether the minidump is written by this unit itself rather than by a library.
//
// The condition is the same one PasVulkan.CrashReport uses to decide whether it
// keeps the state of a fault, and it has to be written out again here because a
// define belongs to the file it is made in. If the two ever disagree, this one
// asks for something the other one does not offer and the compiler says so at
// once, which is the failure worth having.
{$if defined(fpc) and defined(Linux) and not defined(PasVulkanCrashReportWithoutUnixFaultState) and (defined(PasVulkanCrashReportMiniDumpX64) or defined(PasVulkanCrashReportMiniDumpX86))}
 {$define PasVulkanCrashReportMiniDumpUnix}
{$ifend}

{$scopedenums on}

interface

uses {$if defined(Windows)}
      Windows,
     {$ifend}
     SysUtils,
     Classes,
     PasVulkan.Types,
     PasVulkan.CrashReport;

// This unit writes the binary counterpart of what PasVulkan.CrashReport writes
// as text: a minidump, the file format every debugger on Windows can open.
//
// It is a separate unit and not part of PasVulkan.CrashReport for two reasons.
// The first is that it needs a library, and a program which does not want dumps
// should not carry that library along. The second matters more: the unit next
// door runs inside a fault handler and has been gone over line by line with
// that in mind, and none of what is here belongs in that state of affairs. This
// allocates, opens files and calls into somebody else's code.
//
// What makes the dump worth having in this framework in particular is what the
// symbol tool next to it does to the executable. A minidump names the modules
// it saw and repeats their debug directory entry, and because that entry is a
// real one, pointing at a real program database, a debugger opening such a dump
// resolves the addresses to names and lines. Without that step a dump of an
// Object Pascal program is a list of numbers.
//
// The writing itself is a compromise which cannot be argued away: a process
// which dumps itself is asking a library to walk its own structures at the one
// moment those structures may be broken. Doing it from another process is the
// only real answer to that, which is why the well known crash reporters moved
// that way. What is done here instead is to remove every avoidable step from
// the crash path: the library is loaded beforehand, the file is created beside
// its final name and only moved into place once it is complete, and a failure
// leaves nothing behind.

type // How much of the process goes into the file.
     //
     // Small is the state of the threads, the list of modules and nothing else
     // worth mentioning, a few hundred kilobytes. It answers where and with
     // which registers, which is most of what a report is read for.
     //
     // Normal adds the memory the registers and the stacks point at, the layout
     // of the address space and the times of the threads. Still small, and it
     // is the one to ship.
     //
     // Full is the whole address space. For a program with a Vulkan renderer
     // that is gigabytes, since mapped device memory is part of it, so this is
     // for a reproducible fault on a machine one has access to and not for
     // something a player is asked to send in.
     TpvCrashReportMiniDumpKind=
      (
       Small,
       Normal,
       Full
      );

     // A writer which is used instead of the one built in here.
     //
     // This exists so that a program which is delivered through a service of
     // its own can hand the dump to that service rather than to a file, without
     // this unit having to know the service and without the service having to
     // be linked into everything which merely includes this unit. The one such
     // service which is worth naming is the one which takes the exception
     // pointers and a comment and does the rest itself, which is exactly the
     // two arguments below.
     //
     // Returning false means the dump was not written, and the caller of
     // pvCrashReportWriteMiniDump is told so. It does not fall back to the
     // built in writer, since a program which put a writer here meant it.
     TpvCrashReportMiniDumpWriter=function(const aFileName:String;
                                           const aExceptionPointers:TpvPointer;
                                           const aExceptionCode:TpvUInt32;
                                           const aThreadID:TpvUInt64;
                                           const aComment:String;
                                           const aKind:TpvCrashReportMiniDumpKind;
                                           const aUserData:TpvPointer):Boolean;

var // Unassigned means the built in writer, see the type above.
    pvCrashReportMiniDumpWriter:TpvCrashReportMiniDumpWriter=nil;

    // Handed to that writer unchanged, for whatever it needs to find its way
    // back to itself.
    pvCrashReportMiniDumpWriterUserData:TpvPointer=nil;

    // How long a thread waits for another one to be finished writing before it
    // gives up on its own dump.
    //
    // Two dumps at once cannot be allowed to happen, so they are written one
    // after the other, and this is what keeps the second thread from waiting
    // for a first one which is never going to finish. Waiting at all is worth
    // it, since two threads which fault at the same moment usually fault for
    // the same reason and either dump would do; waiting without end is not,
    // since a crash reporter which hangs has turned a crash into something
    // worse than a crash.
    //
    // The reason the two must not overlap is not this unit's doing. Writing a
    // dump stops every other thread of the process for the duration, so two
    // threads doing it at the same time stop one another, each waiting for the
    // other to be resumed by a thread which is itself stopped. Measured: eight
    // threads writing at once got through twenty six dumps of forty and then
    // stood still for good.
    pvCrashReportMiniDumpWaitMilliseconds:TpvUInt32=10000;

    // How long the thread which asked for a dump waits for the one which writes
    // it. Longer than the above on purpose: the first is a queue in front of a
    // job, this is the job.
    pvCrashReportMiniDumpWriteMilliseconds:TpvUInt32=60000;

    // Whether the install brings up a thread of its own to do the writing.
    //
    // Read once, by pvCrashReportMiniDumpInstall, so setting it afterwards
    // changes nothing.
    //
    // There are two reasons for the thread and they point the same way. The
    // first is that the operating system advises against a thread dumping
    // itself, and the reason it gives is the second one: a stack overflow. That
    // fault arrives with a few hundred bytes of stack left, which is not enough
    // for the library to work in, so the dump which is wanted most is the one
    // which cannot be written from where the fault happened. A thread which has
    // been asleep since startup has its whole stack.
    //
    // It also improves every other dump, for a reason which has nothing to do
    // with stacks: writing a dump stops all threads except the one writing it.
    // Written from the faulting thread, that thread is the one thread which is
    // still moving while its own state is being written down. Written from
    // somewhere else, it is stopped like all the others and what lands in the
    // file is what was actually there.
    pvCrashReportMiniDumpWantDumperThread:Boolean=true;

// Brings up whatever writing a dump needs, so that the crash path itself does
// none of it. Loads the library and resolves the one routine which is used.
//
// Deliberately not called from the initialization of this unit, which is where
// the sibling unit installs itself. Initialization of a unit inside a library
// runs while the operating system holds the loader lock, and loading another
// library at that point is the textbook way to deadlock a process at startup.
// The sibling unit gets away with installing itself there because everything it
// does is either arithmetic or a call into a library which is already loaded.
//
// So this is asked for, once, early, by whoever wants dumps. Skipping it is not
// fatal: the library is then loaded at the moment of the crash instead, which
// works far more often than not and is still better than no dump at all.
procedure pvCrashReportMiniDumpInstall;

procedure pvCrashReportMiniDumpUninstall;

// Whether a dump can be written at all. Loads the library if that has not
// happened yet, so this answers the question rather than guessing at it.
//
// False on everything which is not Windows, where there is no such thing as a
// minidump, and false on a Windows without the library, which does not happen
// in practice but is not worth crashing over.
function pvCrashReportMiniDumpAvailable:Boolean;

// A name for a dump which no file has yet, below the given directory, or beside
// the executable when none is given.
//
// Built from the name of the executable, the current time and the process
// identifier, so that a program which crashes twice keeps both files and two
// processes of the same program do not write over each other.
function pvCrashReportMiniDumpFileName(const aDirectory:String=''):String;

// Writes one, and reports whether it got there.
//
// aExceptionPointers is what an exception filter is handed. Pass what the
// filter was given whenever there is one, since a dump taken while the faulting
// thread still stands where it faulted is worth much more than one taken later.
// Passing nothing falls back to the state which PasVulkan.CrashReport kept
// aside at the moment of the fault, which is what makes a dump possible at all
// from a place which never saw those pointers, and which carries the warning
// written at pvCrashReportLastFault.
//
// aExceptionCode and aThreadID may be left at zero, in which case they are
// taken from the exception record and from the calling thread.
//
// aComment goes into the file as a comment stream, which is a part of the
// format itself rather than an invention of this unit, so a foreign debugger
// shows it. Handing pvCrashReportFullReport in makes the dump self contained:
// the identifier, the history, the registers, the stacks of every thread and
// the modules, in the same file as the machine state. It is not filled in from
// here, because assembling that report costs time and memory at a moment when
// the caller may want neither, and that is the caller's call to make.
//
// The file is written beside its final name and moved into place only once it
// is complete, so an interrupted attempt, which in a crash is a real
// possibility, leaves no file which looks like a dump and is not one.
function pvCrashReportWriteMiniDump(const aFileName:String;
                                    const aExceptionPointers:TpvPointer=nil;
                                    const aExceptionCode:TpvUInt32=0;
                                    const aThreadID:TpvUInt64=0;
                                    const aComment:String='';
                                    const aKind:TpvCrashReportMiniDumpKind=TpvCrashReportMiniDumpKind.Normal):Boolean;

implementation

// What one place hands the place which does the writing.
//
// The two strings are pointers into the caller and not copies, because the
// caller waits for the answer and its strings are therefore alive the whole
// time, and because copying a string on a thread which has just run out of
// stack is the sort of thing which turns one fault into two.
//
// Here rather than beside the rest of the machinery of either platform, since
// both of them write dumps and both of them describe one this way.
type PpvCrashReportMiniDumpString=^String;

     TpvCrashReportMiniDumpRequest=record
      FileName:PpvCrashReportMiniDumpString;
      Comment:PpvCrashReportMiniDumpString;
      Pointers:TpvPointer;
      Code:TpvUInt32;
      ThreadID:TpvUInt64;
      Kind:TpvCrashReportMiniDumpKind;
      Answer:Boolean;
     end;
     PpvCrashReportMiniDumpRequest=^TpvCrashReportMiniDumpRequest;

{$if defined(Windows)}

// The kinds of content a dump can hold, from the header of the library. Only
// the ones which are actually combined below are named, since a constant which
// nothing uses is a constant nobody checks.
const MiniDumpNormal=TpvUInt32($00000000);
      MiniDumpWithFullMemory=TpvUInt32($00000002);
      MiniDumpWithHandleData=TpvUInt32($00000004);
      MiniDumpWithUnloadedModules=TpvUInt32($00000020);
      MiniDumpWithIndirectlyReferencedMemory=TpvUInt32($00000040);
      MiniDumpWithProcessThreadData=TpvUInt32($00000100);
      MiniDumpWithFullMemoryInfo=TpvUInt32($00000800);
      MiniDumpWithThreadInfo=TpvUInt32($00001000);
      MiniDumpIgnoreInaccessibleMemory=TpvUInt32($00020000);

      // The right to delete a file and the flag which does it when the last
      // handle goes, both spelled out here. The name the header of the
      // operating system gives the first one is a word which is already taken
      // in this language, by the standard routine which shortens a string.
      cDeleteAccess=TpvUInt32($00010000);
      cDeleteOnClose=TpvUInt32($04000000);

      // The stream number the format reserves for a wide comment. A number of
      // its own would work as well, but this one is understood by every reader
      // of the format, and being readable by somebody else's tools is the whole
      // point of writing this format rather than one of our own.
      cCommentStreamW=TpvUInt32(11);

// The minidump structures of the header they come from stand inside a block
// which packs to four bytes, and that is not the alignment a record gets here by
// default. On a thirty two bit build the two agree and nothing shows. On a
// sixty four bit one they do not: a pointer which follows a four byte field
// belongs at offset four, and left to itself the compiler puts it at eight.
//
// The library then reads four bytes of the pointer together with four bytes of
// nothing next to it and follows the result. What that gives is not an error
// return but a fault inside the library, which is to say a crash while writing
// the report about a crash. Measured, not assumed: with the default alignment
// every call which passes one of these below faults, with this one every call
// returns true.
{$ifdef fpc}
 {$packrecords 4}
{$else}
 {$A4}
{$endif}
type // What the library is handed about the fault.
     TpvCrashReportMiniDumpExceptionInformation=record
      ThreadID:TpvUInt32;
      ExceptionPointers:TpvPointer;
      // False says the pointers above are in this process, which is what they
      // are, since this dumps itself.
      ClientPointers:LongBool;
     end;
     PpvCrashReportMiniDumpExceptionInformation=^TpvCrashReportMiniDumpExceptionInformation;

     TpvCrashReportMiniDumpUserStream=record
      StreamType:TpvUInt32;
      BufferSize:TpvUInt32;
      Buffer:TpvPointer;
     end;
     PpvCrashReportMiniDumpUserStream=^TpvCrashReportMiniDumpUserStream;

     TpvCrashReportMiniDumpUserStreamInformation=record
      UserStreamCount:TpvUInt32;
      UserStreamArray:PpvCrashReportMiniDumpUserStream;
     end;
     PpvCrashReportMiniDumpUserStreamInformation=^TpvCrashReportMiniDumpUserStreamInformation;

// And back, because what follows is not from that header. An exception record
// is an ordinary structure of the operating system with ordinary alignment, and
// packing that one to four would be the same mistake in the other direction.
{$ifdef fpc}
 {$packrecords default}
{$else}
 {$A8}
{$endif}
type // Only the first field is read here, so the rest is named but never
     // touched. It is the same layout the sibling unit mirrors, and it is
     // repeated rather than shared because a public type of that unit would
     // then be part of its interface forever for the sake of one field.
     //
     // The pointer to it has to come first here, which is the one case where it
     // has to: the record names it as one of its own fields.
     PpvCrashReportMiniDumpExceptionRecord=^TpvCrashReportMiniDumpExceptionRecord;
     TpvCrashReportMiniDumpExceptionRecord=record
      ExceptionCode:TpvUInt32;
      ExceptionFlags:TpvUInt32;
      ExceptionRecord:PpvCrashReportMiniDumpExceptionRecord;
      ExceptionAddress:TpvPointer;
      NumberParameters:TpvUInt32;
      ExceptionInformation:array[0..14] of TpvPtrUInt;
     end;

     TpvCrashReportMiniDumpPointers=record
      ExceptionRecord:PpvCrashReportMiniDumpExceptionRecord;
      ContextRecord:TpvPointer;
     end;
     PpvCrashReportMiniDumpPointers=^TpvCrashReportMiniDumpPointers;

     TpvCrashReportMiniDumpWriteDump=function(aProcess:THandle;
                                              aProcessID:TpvUInt32;
                                              aFile:THandle;
                                              aType:TpvUInt32;
                                              aExceptionParam:PpvCrashReportMiniDumpExceptionInformation;
                                              aUserStreamParam:PpvCrashReportMiniDumpUserStreamInformation;
                                              aCallbackParam:TpvPointer):LongBool; stdcall;

var CrashReportMiniDumpLibrary:HMODULE=0;
    CrashReportMiniDumpWriteDumpProc:TpvCrashReportMiniDumpWriteDump=nil;
    // Whether the load has been attempted at all, so that a machine without the
    // library is not asked for it again at every dump.
    CrashReportMiniDumpTried:Boolean=false;
    // Held while the library is being brought up, so that two threads crashing
    // at once do not load it twice.
    CrashReportMiniDumpLock:TpvInt32=0;
    // Held while a dump is being written, see pvCrashReportMiniDumpWaitMilliseconds.
    CrashReportMiniDumpWriteLock:TpvInt32=0;
    // The thread which does the writing and the two events it lives by, see
    // pvCrashReportMiniDumpWantDumperThread.
    CrashReportMiniDumpThreadID:TThreadID=0;
    CrashReportMiniDumpGoEvent:THandle=0;
    CrashReportMiniDumpDoneEvent:THandle=0;
    // Set by the thread itself on its way out, and waited for by the uninstall.
    //
    // A third event rather than waiting on the thread, because what BeginThread
    // hands back is a thread identifier on one compiler and a handle on the
    // other, and only one of those two is something to wait on. This asks the
    // thread instead of asking the operating system about it, and that answer
    // means the same thing everywhere. It stays set once set, since the one
    // thing waited for here happens once.
    CrashReportMiniDumpStoppedEvent:THandle=0;
    // Whether handing work over to it is still worth trying. Cleared for good
    // once a hand over has gone unanswered, since after that nothing is known
    // about what the thread is doing with the request below.
    CrashReportMiniDumpThreadReady:Boolean=false;
    CrashReportMiniDumpThreadQuit:Boolean=false;
    CrashReportMiniDumpRequest:TpvCrashReportMiniDumpRequest;

procedure CrashReportMiniDumpAcquireLock;
begin
{$ifdef fpc}
 while InterLockedExchange(CrashReportMiniDumpLock,1)<>0 do begin
{$else}
 while AtomicExchange(CrashReportMiniDumpLock,1)<>0 do begin
{$endif}
  Sleep(0);
 end;
end;

procedure CrashReportMiniDumpReleaseLock;
begin
 CrashReportMiniDumpLock:=0;
end;

// Takes the right to write a dump, or reports after a while that somebody else
// still has it. See pvCrashReportMiniDumpWaitMilliseconds for why both halves
// of that are needed.
//
// The difference of two tick counts is taken as an unsigned number, which is
// what makes this behave at the moment the counter of the operating system
// comes round rather than wait for another forty nine days.
function CrashReportMiniDumpAcquireWriteLock:Boolean;
var Start:TpvUInt32;
begin
 Start:=TpvUInt32(GetTickCount);
 repeat
{$ifdef fpc}
  if InterLockedExchange(CrashReportMiniDumpWriteLock,1)=0 then begin
{$else}
  if AtomicExchange(CrashReportMiniDumpWriteLock,1)=0 then begin
{$endif}
   result:=true;
   exit;
  end;
  // A whole millisecond rather than a yield, because the thread which holds
  // this is about to stop this one anyway, and spinning through the moment of
  // being stopped is time spent on nothing.
  Sleep(1);
 until TpvUInt32(TpvUInt32(GetTickCount)-Start)>pvCrashReportMiniDumpWaitMilliseconds;
 result:=false;
end;

procedure CrashReportMiniDumpReleaseWriteLock;
begin
 CrashReportMiniDumpWriteLock:=0;
end;

// Brings the library up if that has not been done, and reports whether the one
// routine which matters is there.
function CrashReportMiniDumpLoad:Boolean;
begin
 if not CrashReportMiniDumpTried then begin
  CrashReportMiniDumpAcquireLock;
  try
   if not CrashReportMiniDumpTried then begin
    // Set before the attempt rather than after it, so that a load which itself
    // faults is not repeated from inside the report about that fault.
    CrashReportMiniDumpTried:=true;
    CrashReportMiniDumpLibrary:=LoadLibraryW('dbghelp.dll');
    if CrashReportMiniDumpLibrary<>0 then begin
     CrashReportMiniDumpWriteDumpProc:=TpvCrashReportMiniDumpWriteDump(GetProcAddress(CrashReportMiniDumpLibrary,'MiniDumpWriteDump'));
    end;
   end;
  finally
   CrashReportMiniDumpReleaseLock;
  end;
 end;
 result:=assigned(CrashReportMiniDumpWriteDumpProc);
end;

// The content flags for a kind, richest first, each one a step poorer than the
// one before it.
//
// Three of them rather than one, because some of these flags are younger than
// some of the machines this may run on, and a library which does not know a
// flag refuses the whole call rather than ignoring the flag. Retrying with less
// turns a machine which would have produced no dump into one which produces a
// smaller one.
function CrashReportMiniDumpFlags(const aKind:TpvCrashReportMiniDumpKind;const aAttempt:TpvInt32):TpvUInt32;
begin
 case aAttempt of
  0:begin
   case aKind of
    TpvCrashReportMiniDumpKind.Small:begin
     result:=MiniDumpNormal or MiniDumpWithThreadInfo or MiniDumpWithUnloadedModules;
    end;
    TpvCrashReportMiniDumpKind.Full:begin
     result:=MiniDumpWithFullMemory or MiniDumpWithFullMemoryInfo or MiniDumpWithHandleData or
             MiniDumpWithThreadInfo or MiniDumpWithUnloadedModules or MiniDumpIgnoreInaccessibleMemory;
    end;
    else begin
     result:=MiniDumpNormal or MiniDumpWithThreadInfo or MiniDumpWithUnloadedModules or
             MiniDumpWithIndirectlyReferencedMemory or MiniDumpWithProcessThreadData or
             MiniDumpWithFullMemoryInfo or MiniDumpIgnoreInaccessibleMemory;
    end;
   end;
  end;
  1:begin
   result:=MiniDumpNormal or MiniDumpWithThreadInfo or MiniDumpWithUnloadedModules;
  end;
  else begin
   result:=MiniDumpNormal;
  end;
 end;
end;

// A name beside the wanted one which no file has, for the dump to be built
// under before it is moved into place.
//
// Exclusive creation is what decides it, not a look beforehand, so two threads
// or two processes cannot both come away believing the same name is theirs.
//
// One of these can be left behind, and by exactly the fault this unit is for: a
// process which is killed in the middle of writing a dump does not get to tidy
// up after itself. So a name which is taken is looked at once more before it is
// given up on. Everything here opens with no sharing at all, which turns the
// question of whether somebody is still writing that file into a question the
// operating system answers: an open which succeeds is proof that nobody holds
// it, and a file nobody holds under this name is the remains of a run which
// died. It is then deleted by the closing of the very handle which proved it,
// and the name is free again.
//
// Note what this does not do: it never looks at how old the file is or how big.
// The only thing it goes by is whether anyone still has it open, which is the
// one thing about it which cannot be wrong.
function CrashReportMiniDumpAcquireTemporaryFile(const aFileName:String;out aTemporaryName:String):THandle;
var Index:TpvInt32;
    Candidate:String;
    Stale:THandle;
begin
 result:=INVALID_HANDLE_VALUE;
 aTemporaryName:='';
 for Index:=0 to 63 do begin
  if Index=0 then begin
   Candidate:=aFileName+'.part';
  end else begin
   Candidate:=aFileName+'.part'+IntToStr(Index);
  end;
  result:=CreateFileW(PWideChar(UnicodeString(Candidate)),GENERIC_READ or GENERIC_WRITE,0,nil,CREATE_NEW,FILE_ATTRIBUTE_NORMAL,0);
  if result=INVALID_HANDLE_VALUE then begin
   Stale:=CreateFileW(PWideChar(UnicodeString(Candidate)),cDeleteAccess,0,nil,OPEN_EXISTING,cDeleteOnClose,0);
   if Stale<>INVALID_HANDLE_VALUE then begin
    CloseHandle(Stale);
    result:=CreateFileW(PWideChar(UnicodeString(Candidate)),GENERIC_READ or GENERIC_WRITE,0,nil,CREATE_NEW,FILE_ATTRIBUTE_NORMAL,0);
   end;
  end;
  if result<>INVALID_HANDLE_VALUE then begin
   aTemporaryName:=Candidate;
   exit;
  end;
 end;
end;

// The writing itself, on whichever thread ends up doing it.
//
// A function of its own so that the two ways in, straight from the caller and
// through the thread which was made for it, are one and the same piece of work
// and not two which have to be kept in step. It also keeps everything the
// library needs on the stack of whoever writes rather than on the stack of
// whoever asked, which for a caller whose stack has just run out is the whole
// difference.
//
// The right to write must already be held, and is not taken or given back here.
function CrashReportMiniDumpWriteHere(const aRequest:PpvCrashReportMiniDumpRequest):Boolean;
var Attempt:TpvInt32;
    ExceptionInformation:TpvCrashReportMiniDumpExceptionInformation;
    ExceptionParameter:PpvCrashReportMiniDumpExceptionInformation;
    UserStream:TpvCrashReportMiniDumpUserStream;
    UserStreamInformation:TpvCrashReportMiniDumpUserStreamInformation;
    UserStreamParameter:PpvCrashReportMiniDumpUserStreamInformation;
    CommentText:UnicodeString;
    Handle:THandle;
    TemporaryName:String;
    Written:Boolean;
begin
 result:=false;
 try

  Handle:=CrashReportMiniDumpAcquireTemporaryFile(aRequest^.FileName^,TemporaryName);
  if Handle=INVALID_HANDLE_VALUE then begin
   exit;
  end;
  Written:=false;
  // Two of these around one another, and both are needed. The inner one gives
  // the handle back, the outer one makes sure the half built file goes away no
  // matter which of the steps between here and the move gave up or faulted.
  try
   try

    ExceptionParameter:=nil;
    if assigned(aRequest^.Pointers) then begin
     ExceptionInformation.ThreadID:=TpvUInt32(aRequest^.ThreadID);
     ExceptionInformation.ExceptionPointers:=aRequest^.Pointers;
     ExceptionInformation.ClientPointers:=false;
     ExceptionParameter:=@ExceptionInformation;
    end;

    UserStreamParameter:=nil;
    CommentText:='';
    if length(aRequest^.Comment^)>0 then begin
     CommentText:=UnicodeString(aRequest^.Comment^);
     UserStream.StreamType:=cCommentStreamW;
     // With the terminator, which is what a reader of a comment stream expects
     // to find and what tells it where the text ends.
     UserStream.BufferSize:=TpvUInt32((length(CommentText)+1)*SizeOf(WideChar));
     UserStream.Buffer:=PWideChar(CommentText);
     UserStreamInformation.UserStreamCount:=1;
     UserStreamInformation.UserStreamArray:=@UserStream;
     UserStreamParameter:=@UserStreamInformation;
    end;

    for Attempt:=0 to 2 do begin
     if Attempt>0 then begin
      // Back to the start of the file, since a refused attempt may still have
      // put a header there, and a second header behind the first is not a dump.
      SetFilePointer(Handle,0,nil,FILE_BEGIN);
      SetEndOfFile(Handle);
     end;
     if CrashReportMiniDumpWriteDumpProc(GetCurrentProcess,
                                         GetCurrentProcessId,
                                         Handle,
                                         CrashReportMiniDumpFlags(aRequest^.Kind,Attempt),
                                         ExceptionParameter,
                                         UserStreamParameter,
                                         nil) then begin
      Written:=true;
      break;
     end;
    end;

    FlushFileBuffers(Handle);

   finally
    CloseHandle(Handle);
   end;

   if Written then begin
    // Into place only now that there is something whole to move. A dump which
    // was interrupted halfway never wore the name of a dump, so nobody is going
    // to open it, wonder why it stops in the middle, and mistrust the tool.
    result:=MoveFileExW(PWideChar(UnicodeString(TemporaryName)),PWideChar(UnicodeString(aRequest^.FileName^)),MOVEFILE_REPLACE_EXISTING);
   end;

  finally
   if not result then begin
    DeleteFileW(PWideChar(UnicodeString(TemporaryName)));
   end;
  end;

 except
  // The report about the crash outranks everything which happens while it is
  // being made.
  result:=false;
 end;
end;

// The thread which was made for the writing. Asleep from the install until it
// is woken, and asleep again afterwards.
function CrashReportMiniDumpThreadProc(aParameter:TpvPointer):{$ifdef fpc}TpvPtrInt{$else}Integer{$endif};
begin
 result:=0;
 while WaitForSingleObject(CrashReportMiniDumpGoEvent,INFINITE)=WAIT_OBJECT_0 do begin
  if CrashReportMiniDumpThreadQuit then begin
   break;
  end;
  try
   CrashReportMiniDumpRequest.Answer:=CrashReportMiniDumpWriteHere(@CrashReportMiniDumpRequest);
  except
   CrashReportMiniDumpRequest.Answer:=false;
  end;
  SetEvent(CrashReportMiniDumpDoneEvent);
 end;
 SetEvent(CrashReportMiniDumpStoppedEvent);
end;

// Hands one dump over to that thread and waits for it.
//
// The result says whether the thread dealt with the request at all, and
// aAnswer says whether the dump got written. The two are kept apart on purpose:
// a request the thread took and could not carry out must not be tried again by
// the caller, since it would fail for the same reason, and on a thread which
// has run out of stack the second attempt is the one which does the damage.
//
// A hand over which goes unanswered is the end of the thread as far as this
// unit is concerned. Not because it is certainly dead, but because it may still
// be reading the one request there is, and filling that in again while somebody
// reads it is worse than writing every further dump the slower way.
function CrashReportMiniDumpHandOver(const aFileName,aComment:PpvCrashReportMiniDumpString;
                                     const aPointers:TpvPointer;
                                     const aCode:TpvUInt32;
                                     const aThreadID:TpvUInt64;
                                     const aKind:TpvCrashReportMiniDumpKind;
                                     out aAnswer:Boolean):Boolean;
begin
 result:=false;
 aAnswer:=false;
 if not CrashReportMiniDumpThreadReady then begin
  exit;
 end;
 CrashReportMiniDumpRequest.FileName:=aFileName;
 CrashReportMiniDumpRequest.Comment:=aComment;
 CrashReportMiniDumpRequest.Pointers:=aPointers;
 CrashReportMiniDumpRequest.Code:=aCode;
 CrashReportMiniDumpRequest.ThreadID:=aThreadID;
 CrashReportMiniDumpRequest.Kind:=aKind;
 CrashReportMiniDumpRequest.Answer:=false;
 if not SetEvent(CrashReportMiniDumpGoEvent) then begin
  CrashReportMiniDumpThreadReady:=false;
  exit;
 end;
 if WaitForSingleObject(CrashReportMiniDumpDoneEvent,pvCrashReportMiniDumpWriteMilliseconds)=WAIT_OBJECT_0 then begin
  aAnswer:=CrashReportMiniDumpRequest.Answer;
  result:=true;
 end else begin
  CrashReportMiniDumpThreadReady:=false;
 end;
end;

procedure CrashReportMiniDumpCloseEvents;
begin
 if CrashReportMiniDumpGoEvent<>0 then begin
  CloseHandle(CrashReportMiniDumpGoEvent);
  CrashReportMiniDumpGoEvent:=0;
 end;
 if CrashReportMiniDumpDoneEvent<>0 then begin
  CloseHandle(CrashReportMiniDumpDoneEvent);
  CrashReportMiniDumpDoneEvent:=0;
 end;
 if CrashReportMiniDumpStoppedEvent<>0 then begin
  CloseHandle(CrashReportMiniDumpStoppedEvent);
  CrashReportMiniDumpStoppedEvent:=0;
 end;
end;

{$ifend}

{$ifdef PasVulkanCrashReportMiniDumpUnix}

// There is no library here which writes this format, so it is written out by
// hand. That sounds worse than it is: a minidump is a header, a table of
// contents and a handful of blocks, all of them plain records with no
// compression and no pointers other than offsets from the start of the file.
//
// What it is written for is the same set of readers the rest of this framework
// aims at. The format is not a Windows format by accident of where it came
// from, it is the format the crash reporters of the free world settled on for
// every platform, so a file written here is one the usual processing pipelines
// already understand.
//
// Everything is little endian, which on the two machines this is built for is
// also the machine order, so the numbers go in as they are.
const cMiniDumpSignature=TpvUInt32($504d444d); // MDMP
      cMiniDumpVersion=TpvUInt32($0000a793);

      cMiniDumpStreamThreadList=TpvUInt32(3);
      cMiniDumpStreamModuleList=TpvUInt32(4);
      cMiniDumpStreamMemoryList=TpvUInt32(5);
      cMiniDumpStreamException=TpvUInt32(6);
      cMiniDumpStreamSystemInfo=TpvUInt32(7);
      cMiniDumpStreamCommentW=TpvUInt32(11);
      // The one stream here which is not from the original format. The
      // processing pipelines of the free world added a few of their own for
      // this platform, and this is the useful one: the process map exactly as
      // the kernel prints it, which answers every question about the address
      // space which the module list rounds off.
      cMiniDumpStreamLinuxMaps=TpvUInt32($47670009);

      cMiniDumpPlatformLinux=TpvUInt32($8201);

      // What tells a reader that a module was identified by the note the linker
      // put in it rather than by a program database.
      cMiniDumpCVSignatureELF=TpvUInt32($4270454c); // BpEL

{$ifdef PasVulkanCrashReportMiniDumpX64}
      cMiniDumpArchitecture=TpvUInt16(9);
      cMiniDumpContextSize=1232;
      cMiniDumpContextFlags=TpvUInt32($0010000b);
      // Where the registers sit inside the context the operating system fills
      // in, which is the same offset PasVulkan.CrashReport reads them at.
      cMiniDumpContextGRegs=40;
{$else}
      cMiniDumpArchitecture=TpvUInt16(0);
      cMiniDumpContextSize=716;
      cMiniDumpContextFlags=TpvUInt32($00010007);
      cMiniDumpContextGRegs=20;
{$endif}

      // How much of a stack goes into the file. A thread which has just run out
      // of stack has a megabyte of it and all of it is interesting; a thread
      // with a hundred megabyte stack does not.
      cMiniDumpMaximalStackBytes=TpvSizeInt(1) shl 20;

      // And how much around the instruction which faulted, so that a
      // disassembler has something to work with even when the module it came
      // from is not at hand.
      cMiniDumpBytesAroundInstruction=256;

type TpvCrashReportMiniDumpBuffer=record
      Data:array of TpvUInt8;
      Size:TpvSizeInt;
     end;

     // One line of the process map, reduced to what is used.
     TpvCrashReportMiniDumpMapping=record
      Low:TpvPtrUInt;
      High:TpvPtrUInt;
      Offset:TpvUInt64;
      Executable:Boolean;
      Readable:Boolean;
      Path:TpvRawByteString;
     end;
     PpvCrashReportMiniDumpMapping=^TpvCrashReportMiniDumpMapping;

     TpvCrashReportMiniDumpMappings=array of TpvCrashReportMiniDumpMapping;

     TpvCrashReportMiniDumpModule=record
      Base:TpvPtrUInt;
      Size:TpvPtrUInt;
      Path:TpvRawByteString;
      BuildID:TpvRawByteString;
     end;
     PpvCrashReportMiniDumpModule=^TpvCrashReportMiniDumpModule;

     TpvCrashReportMiniDumpModules=array of TpvCrashReportMiniDumpModule;

// The state of the calling thread, for a dump which is asked for when nothing
// has faulted. The same thing the fault handler is handed, only about here and
// now instead of about a crash.
function CrashReportMiniDumpGetContext(aContext:TpvPointer):TpvInt32; cdecl; external name 'getcontext';

procedure CrashReportMiniDumpBufferNeed(var aBuffer:TpvCrashReportMiniDumpBuffer;const aExtra:TpvSizeInt);
var Wanted:TpvSizeInt;
begin
 Wanted:=aBuffer.Size+aExtra;
 if Wanted>length(aBuffer.Data) then begin
  // Doubling, so that building a file of a few megabytes out of many small
  // pieces does not become a copy for every piece.
  SetLength(aBuffer.Data,(Wanted+(Wanted shr 1))+4096);
 end;
end;

// Puts bytes at the end and answers where they landed, which in this format is
// what everything points at: an offset from the start of the file.
function CrashReportMiniDumpAppend(var aBuffer:TpvCrashReportMiniDumpBuffer;const aData;const aSize:TpvSizeInt):TpvUInt32;
begin
 result:=TpvUInt32(aBuffer.Size);
 if aSize>0 then begin
  CrashReportMiniDumpBufferNeed(aBuffer,aSize);
  Move(aData,aBuffer.Data[aBuffer.Size],aSize);
  inc(aBuffer.Size,aSize);
 end;
end;

function CrashReportMiniDumpAppendZeros(var aBuffer:TpvCrashReportMiniDumpBuffer;const aSize:TpvSizeInt):TpvUInt32;
begin
 result:=TpvUInt32(aBuffer.Size);
 if aSize>0 then begin
  CrashReportMiniDumpBufferNeed(aBuffer,aSize);
  FillChar(aBuffer.Data[aBuffer.Size],aSize,#0);
  inc(aBuffer.Size,aSize);
 end;
end;

procedure CrashReportMiniDumpPut32(var aBuffer:TpvCrashReportMiniDumpBuffer;const aAt:TpvUInt32;const aValue:TpvUInt32);
begin
 if (TpvSizeInt(aAt)+4)<=aBuffer.Size then begin
  PpvUInt32(TpvPointer(@aBuffer.Data[aAt]))^:=aValue;
 end;
end;

// A string the way this format keeps one: a length in bytes and then the
// characters, sixteen bits each, with a terminator which the length does not
// count.
function CrashReportMiniDumpAppendString(var aBuffer:TpvCrashReportMiniDumpBuffer;const aValue:String):TpvUInt32;
var Wide:UnicodeString;
    Bytes:TpvUInt32;
begin
 Wide:=UnicodeString(aValue);
 Bytes:=TpvUInt32(length(Wide)*SizeOf(WideChar));
 result:=CrashReportMiniDumpAppend(aBuffer,Bytes,SizeOf(TpvUInt32));
 if Bytes>0 then begin
  CrashReportMiniDumpAppend(aBuffer,PWideChar(Wide)^,Bytes);
 end;
 CrashReportMiniDumpAppendZeros(aBuffer,SizeOf(WideChar));
end;

// Reads one of the files the kernel makes up on the spot. They report no size,
// so this reads until it stops giving anything.
function CrashReportMiniDumpReadProcFile(const aFileName:String):TpvRawByteString;
var Handle:THandle;
    Got:TpvSizeInt;
    Chunk:array[0..8191] of AnsiChar;
begin
 result:='';
 Handle:=FileOpen(aFileName,fmOpenRead or fmShareDenyNone);
 if Handle=THandle(-1) then begin
  exit;
 end;
 try
  repeat
   Got:=FileRead(Handle,Chunk[0],SizeOf(Chunk));
   if Got>0 then begin
    SetLength(result,length(result)+Got);
    Move(Chunk[0],result[(length(result)-Got)+1],Got);
   end;
  until Got<=0;
 finally
  FileClose(Handle);
 end;
end;

// Takes the process map apart. One line looks like
//
//   7f3c1a000000-7f3c1a022000 r-xp 00000000 08:01 1234  /usr/lib/libc.so.6
//
// and everything this needs is in the first four fields and the last one.
function CrashReportMiniDumpReadMappings(out aMappings:TpvCrashReportMiniDumpMappings;out aRaw:TpvRawByteString):Boolean;
var Position,Start,Stop,Count:TpvSizeInt;
    Line:TpvRawByteString;

 function TakeHex(var aAt:TpvSizeInt;out aValue:TpvUInt64):Boolean;
 var Digit:TpvUInt32;
     Any:Boolean;
 begin
  aValue:=0;
  Any:=false;
  while aAt<=length(Line) do begin
   case Line[aAt] of
    '0'..'9':begin
     Digit:=TpvUInt32(ord(Line[aAt])-ord('0'));
    end;
    'a'..'f':begin
     Digit:=TpvUInt32(ord(Line[aAt])-ord('a'))+10;
    end;
    'A'..'F':begin
     Digit:=TpvUInt32(ord(Line[aAt])-ord('A'))+10;
    end;
    else begin
     break;
    end;
   end;
   aValue:=(aValue shl 4) or Digit;
   Any:=true;
   inc(aAt);
  end;
  result:=Any;
 end;

var At:TpvSizeInt;
    Low,High,Offset:TpvUInt64;
    Perms:TpvRawByteString;
    Fields:TpvInt32;
    Mapping:PpvCrashReportMiniDumpMapping;
begin
 aMappings:=nil;
 Count:=0;
 aRaw:=CrashReportMiniDumpReadProcFile('/proc/self/maps');
 result:=length(aRaw)>0;
 if not result then begin
  exit;
 end;
 Position:=1;
 while Position<=length(aRaw) do begin
  Start:=Position;
  Stop:=Position;
  while (Stop<=length(aRaw)) and (aRaw[Stop]<>#10) do begin
   inc(Stop);
  end;
  Line:=copy(aRaw,Start,Stop-Start);
  Position:=Stop+1;
  if length(Line)=0 then begin
   continue;
  end;
  At:=1;
  if not TakeHex(At,Low) then begin
   continue;
  end;
  if (At>length(Line)) or (Line[At]<>'-') then begin
   continue;
  end;
  inc(At);
  if not TakeHex(At,High) then begin
   continue;
  end;
  while (At<=length(Line)) and (Line[At]=' ') do begin
   inc(At);
  end;
  Perms:='';
  while (At<=length(Line)) and (Line[At]<>' ') do begin
   Perms:=Perms+Line[At];
   inc(At);
  end;
  while (At<=length(Line)) and (Line[At]=' ') do begin
   inc(At);
  end;
  if not TakeHex(At,Offset) then begin
   Offset:=0;
  end;
  // Past the device and the inode to the path, which is whatever is left after
  // the last run of spaces. A path with a space in it therefore has to be taken
  // from the first non space after the inode, which is what this does.
  Fields:=0;
  while (At<=length(Line)) and (Fields<2) do begin
   while (At<=length(Line)) and (Line[At]=' ') do begin
    inc(At);
   end;
   while (At<=length(Line)) and (Line[At]<>' ') do begin
    inc(At);
   end;
   inc(Fields);
  end;
  while (At<=length(Line)) and (Line[At]=' ') do begin
   inc(At);
  end;
  if Count>=length(aMappings) then begin
   SetLength(aMappings,(Count+1)*2);
  end;
  Mapping:=@aMappings[Count];
  Mapping^.Low:=TpvPtrUInt(Low);
  Mapping^.High:=TpvPtrUInt(High);
  Mapping^.Offset:=Offset;
  Mapping^.Readable:=(length(Perms)>0) and (Perms[1]='r');
  Mapping^.Executable:=(length(Perms)>2) and (Perms[3]='x');
  if At<=length(Line) then begin
   Mapping^.Path:=copy(Line,At,(length(Line)-At)+1);
  end else begin
   Mapping^.Path:='';
  end;
  inc(Count);
 end;
 SetLength(aMappings,Count);
 result:=Count>0;
end;

// The identity the linker left in a loaded object, read out of the object where
// it already sits in memory.
//
// An ELF file begins with a header which says where the program headers are;
// one of those describes a block of notes; and one of those notes is the build
// identifier. Every step is checked before it is followed, because this walks
// through memory which belongs to somebody else and is being read while the
// process is falling apart.
function CrashReportMiniDumpBuildID(const aBase:TpvPtrUInt;const aLimit:TpvPtrUInt):TpvRawByteString;
const cNoteGnuBuildID=3;
      cProgramHeaderNote=4;
var PhOffset,NoteAt,NoteEnd,Position:TpvPtrUInt;
    PhEntrySize,PhCount,Index:TpvUInt32;
    NameSize,DescriptionSize,NoteType,Padded:TpvUInt32;

 function Readable(const aAt:TpvPtrUInt;const aSize:TpvPtrUInt):Boolean;
 begin
  result:=(aAt>=aBase) and (aSize<=(aLimit-aBase)) and ((aAt+aSize)<=aLimit);
 end;

begin
 result:='';
 if not Readable(aBase,64) then begin
  exit;
 end;
 // The four bytes every ELF object starts with, and the class byte, which has
 // to be the one this build was made for since the offsets below differ.
 if (PpvUInt8(TpvPointer(aBase))^<>$7f) or
    (PpvUInt8(TpvPointer(aBase+1))^<>ord('E')) or
    (PpvUInt8(TpvPointer(aBase+2))^<>ord('L')) or
    (PpvUInt8(TpvPointer(aBase+3))^<>ord('F')) then begin
  exit;
 end;
{$ifdef PasVulkanCrashReportMiniDumpX64}
 if PpvUInt8(TpvPointer(aBase+4))^<>2 then begin
  exit;
 end;
 PhOffset:=TpvPtrUInt(PpvUInt64(TpvPointer(aBase+32))^);
 PhEntrySize:=PpvUInt16(TpvPointer(aBase+54))^;
 PhCount:=PpvUInt16(TpvPointer(aBase+56))^;
{$else}
 if PpvUInt8(TpvPointer(aBase+4))^<>1 then begin
  exit;
 end;
 PhOffset:=TpvPtrUInt(PpvUInt32(TpvPointer(aBase+28))^);
 PhEntrySize:=PpvUInt16(TpvPointer(aBase+42))^;
 PhCount:=PpvUInt16(TpvPointer(aBase+44))^;
{$endif}
 if (PhOffset=0) or (PhCount=0) or (PhCount>256) or (PhEntrySize<8) then begin
  exit;
 end;
 if not Readable(aBase+PhOffset,TpvPtrUInt(PhEntrySize)*PhCount) then begin
  exit;
 end;
 for Index:=0 to PhCount-1 do begin
  Position:=aBase+PhOffset+(TpvPtrUInt(PhEntrySize)*Index);
{$ifdef PasVulkanCrashReportMiniDumpX64}
  if PpvUInt32(TpvPointer(Position))^<>cProgramHeaderNote then begin
   continue;
  end;
  // The address the note block will have once the object is loaded, which is
  // relative to where the object landed.
  NoteAt:=aBase+TpvPtrUInt(PpvUInt64(TpvPointer(Position+16))^);
  NoteEnd:=NoteAt+TpvPtrUInt(PpvUInt64(TpvPointer(Position+40))^);
{$else}
  if PpvUInt32(TpvPointer(Position))^<>cProgramHeaderNote then begin
   continue;
  end;
  NoteAt:=aBase+TpvPtrUInt(PpvUInt32(TpvPointer(Position+8))^);
  NoteEnd:=NoteAt+TpvPtrUInt(PpvUInt32(TpvPointer(Position+20))^);
{$endif}
  if (NoteEnd<=NoteAt) or not Readable(NoteAt,NoteEnd-NoteAt) then begin
   continue;
  end;
  while (NoteAt+12)<=NoteEnd do begin
   NameSize:=PpvUInt32(TpvPointer(NoteAt))^;
   DescriptionSize:=PpvUInt32(TpvPointer(NoteAt+4))^;
   NoteType:=PpvUInt32(TpvPointer(NoteAt+8))^;
   // Both halves are padded out to four bytes, and neither length counts that.
   Padded:=((NameSize+3) and not TpvUInt32(3))+((DescriptionSize+3) and not TpvUInt32(3));
   if (Padded>(NoteEnd-NoteAt)) or (DescriptionSize>64) then begin
    break;
   end;
   if (NoteType=cNoteGnuBuildID) and (NameSize=4) and (DescriptionSize>0) and
      (PAnsiChar(TpvPointer(NoteAt+12))[0]='G') and
      (PAnsiChar(TpvPointer(NoteAt+12))[1]='N') and
      (PAnsiChar(TpvPointer(NoteAt+12))[2]='U') then begin
    SetLength(result,DescriptionSize);
    Move(TpvPointer(NoteAt+12+((NameSize+3) and not TpvUInt32(3)))^,result[1],DescriptionSize);
    exit;
   end;
   inc(NoteAt,12+Padded);
  end;
 end;
end;

// Turns the mappings into modules.
//
// A loaded object shows up as several mappings in a row, one per part of it
// with different rights, all naming the same file. The first of them, the one
// which is mapped from the beginning of the file, is where the object starts,
// and the last of the run is where it ends.
//
// Anything without a path is left out. Those are the stack, the heap and the
// anonymous mappings, which are not modules and which the process map stream
// describes anyway.
function CrashReportMiniDumpBuildModules(const aMappings:TpvCrashReportMiniDumpMappings):TpvCrashReportMiniDumpModules;
var Index,Count:TpvSizeInt;
    Mapping:PpvCrashReportMiniDumpMapping;
    Module:PpvCrashReportMiniDumpModule;
begin
 result:=nil;
 Count:=0;
 for Index:=0 to length(aMappings)-1 do begin
  Mapping:=@aMappings[Index];
  if (length(Mapping^.Path)=0) or (Mapping^.Path[1]<>'/') then begin
   continue;
  end;
  if Count>0 then begin
   Module:=@result[Count-1];
   if Module^.Path=Mapping^.Path then begin
    // Same file as the mapping before, so this is another part of the same
    // object and only moves its end.
    if Mapping^.High>(Module^.Base+Module^.Size) then begin
     Module^.Size:=Mapping^.High-Module^.Base;
    end;
    continue;
   end;
  end;
  if Mapping^.Offset<>0 then begin
   // Starts somewhere in the middle of a file, so it is a piece of a mapping
   // and not the head of an object.
   continue;
  end;
  if Count>=length(result) then begin
   SetLength(result,(Count+1)*2);
  end;
  Module:=@result[Count];
  Module^.Base:=Mapping^.Low;
  Module^.Size:=Mapping^.High-Mapping^.Low;
  Module^.Path:=Mapping^.Path;
  Module^.BuildID:='';
  inc(Count);
 end;
 SetLength(result,Count);
 for Index:=0 to Count-1 do begin
  Module:=@result[Index];
  // Read out of the copy which is already in memory, and only as far as the
  // object reaches, so a damaged header cannot walk out of it.
  Module^.BuildID:=CrashReportMiniDumpBuildID(Module^.Base,Module^.Base+Module^.Size);
 end;
end;

procedure CrashReportMiniDumpPutBytes32(var aData:array of TpvUInt8;const aAt:TpvInt32;const aValue:TpvUInt32);
begin
 if (aAt>=0) and ((aAt+4)<=length(aData)) then begin
  PpvUInt32(TpvPointer(@aData[aAt]))^:=aValue;
 end;
end;

procedure CrashReportMiniDumpPutBytes64(var aData:array of TpvUInt8;const aAt:TpvInt32;const aValue:TpvUInt64);
begin
 if (aAt>=0) and ((aAt+8)<=length(aData)) then begin
  PpvUInt64(TpvPointer(@aData[aAt]))^:=aValue;
 end;
end;

procedure CrashReportMiniDumpPutBytes16(var aData:array of TpvUInt8;const aAt:TpvInt32;const aValue:TpvUInt16);
begin
 if (aAt>=0) and ((aAt+2)<=length(aData)) then begin
  PpvUInt16(TpvPointer(@aData[aAt]))^:=aValue;
 end;
end;

// Copies a machine state from the shape the operating system keeps it in into
// the shape this format keeps it in.
//
// The two are unrelated layouts which happen to describe the same registers, so
// this is a table of offsets and nothing more. The numbers on the left are
// where the field sits in the record of the format; the numbers on the right
// are which entry of the register array of the operating system holds it, in
// the order the C library numbers them in.
procedure CrashReportMiniDumpFillContext(var aData:array of TpvUInt8;const aFault:TpvCrashReportUnixFault);
begin
 FillChar(aData[0],length(aData),#0);
{$ifdef PasVulkanCrashReportMiniDumpX64}
 CrashReportMiniDumpPutBytes32(aData,48,cMiniDumpContextFlags);
 // The segment registers arrive packed into one entry: the code segment in the
 // low sixteen bits, then gs, then fs.
 CrashReportMiniDumpPutBytes16(aData,56,TpvUInt16(aFault.Registers[18] and $ffff));
 CrashReportMiniDumpPutBytes16(aData,62,TpvUInt16((aFault.Registers[18] shr 32) and $ffff));
 CrashReportMiniDumpPutBytes16(aData,64,TpvUInt16((aFault.Registers[18] shr 16) and $ffff));
 CrashReportMiniDumpPutBytes32(aData,68,TpvUInt32(aFault.Registers[17]));
 CrashReportMiniDumpPutBytes64(aData,120,aFault.Registers[13]); // rax
 CrashReportMiniDumpPutBytes64(aData,128,aFault.Registers[14]); // rcx
 CrashReportMiniDumpPutBytes64(aData,136,aFault.Registers[12]); // rdx
 CrashReportMiniDumpPutBytes64(aData,144,aFault.Registers[11]); // rbx
 CrashReportMiniDumpPutBytes64(aData,152,aFault.Registers[15]); // rsp
 CrashReportMiniDumpPutBytes64(aData,160,aFault.Registers[10]); // rbp
 CrashReportMiniDumpPutBytes64(aData,168,aFault.Registers[9]);  // rsi
 CrashReportMiniDumpPutBytes64(aData,176,aFault.Registers[8]);  // rdi
 CrashReportMiniDumpPutBytes64(aData,184,aFault.Registers[0]);  // r8
 CrashReportMiniDumpPutBytes64(aData,192,aFault.Registers[1]);  // r9
 CrashReportMiniDumpPutBytes64(aData,200,aFault.Registers[2]);  // r10
 CrashReportMiniDumpPutBytes64(aData,208,aFault.Registers[3]);  // r11
 CrashReportMiniDumpPutBytes64(aData,216,aFault.Registers[4]);  // r12
 CrashReportMiniDumpPutBytes64(aData,224,aFault.Registers[5]);  // r13
 CrashReportMiniDumpPutBytes64(aData,232,aFault.Registers[6]);  // r14
 CrashReportMiniDumpPutBytes64(aData,240,aFault.Registers[7]);  // r15
 CrashReportMiniDumpPutBytes64(aData,248,aFault.Registers[16]); // rip
{$else}
 CrashReportMiniDumpPutBytes32(aData,0,cMiniDumpContextFlags);
 CrashReportMiniDumpPutBytes32(aData,140,TpvUInt32(aFault.Registers[0]));  // gs
 CrashReportMiniDumpPutBytes32(aData,144,TpvUInt32(aFault.Registers[1]));  // fs
 CrashReportMiniDumpPutBytes32(aData,148,TpvUInt32(aFault.Registers[2]));  // es
 CrashReportMiniDumpPutBytes32(aData,152,TpvUInt32(aFault.Registers[3]));  // ds
 CrashReportMiniDumpPutBytes32(aData,156,TpvUInt32(aFault.Registers[4]));  // edi
 CrashReportMiniDumpPutBytes32(aData,160,TpvUInt32(aFault.Registers[5]));  // esi
 CrashReportMiniDumpPutBytes32(aData,164,TpvUInt32(aFault.Registers[8]));  // ebx
 CrashReportMiniDumpPutBytes32(aData,168,TpvUInt32(aFault.Registers[9]));  // edx
 CrashReportMiniDumpPutBytes32(aData,172,TpvUInt32(aFault.Registers[10])); // ecx
 CrashReportMiniDumpPutBytes32(aData,176,TpvUInt32(aFault.Registers[11])); // eax
 CrashReportMiniDumpPutBytes32(aData,180,TpvUInt32(aFault.Registers[6]));  // ebp
 CrashReportMiniDumpPutBytes32(aData,184,TpvUInt32(aFault.Registers[14])); // eip
 CrashReportMiniDumpPutBytes32(aData,188,TpvUInt32(aFault.Registers[15])); // cs
 CrashReportMiniDumpPutBytes32(aData,192,TpvUInt32(aFault.Registers[16])); // eflags
 CrashReportMiniDumpPutBytes32(aData,196,TpvUInt32(aFault.Registers[7]));  // esp
 CrashReportMiniDumpPutBytes32(aData,200,TpvUInt32(aFault.Registers[18])); // ss
{$endif}
end;

// Builds the whole file in memory and then puts it on disk in one piece.
//
// In memory because the format is full of offsets which point forwards and
// backwards, and because it is a few hundred kilobytes. Writing it out only
// once it is complete also means the same discipline the other platform
// follows: a file appears under its name whole or not at all.
//
// No lock around any of this. Two threads writing dumps here do not disturb one
// another the way they do on the other platform, since nothing is stopped and
// nothing is shared: each builds its own file in its own memory and moves it
// into place under a name which carries the process number.
function CrashReportMiniDumpWriteHereUnix(const aRequest:PpvCrashReportMiniDumpRequest):Boolean;
const cMaximalStreams=7;
      cMaximalMemoryRanges=2;
type TpvCrashReportMiniDumpDirectoryEntry=record
      StreamType:TpvUInt32;
      Size:TpvUInt32;
      RVA:TpvUInt32;
     end;
     PpvCrashReportMiniDumpDirectoryEntry=^TpvCrashReportMiniDumpDirectoryEntry;
     TpvCrashReportMiniDumpRange=record
      Start:TpvUInt64;
      Size:TpvUInt32;
      RVA:TpvUInt32;
     end;
     PpvCrashReportMiniDumpRange=^TpvCrashReportMiniDumpRange;
var Buffer:TpvCrashReportMiniDumpBuffer;
    Fault:TpvCrashReportUnixFault;
    Mappings:TpvCrashReportMiniDumpMappings;
    Modules:TpvCrashReportMiniDumpModules;
    RawMaps:TpvRawByteString;
    ContextData:array[0..cMiniDumpContextSize-1] of TpvUInt8;
    UnixContext:array[0..4095] of TpvUInt8;
    Directory:array[0..cMaximalStreams-1] of TpvCrashReportMiniDumpDirectoryEntry;
    Ranges:array[0..cMaximalMemoryRanges-1] of TpvCrashReportMiniDumpRange;
    StreamCount,RangeCount:TpvInt32;
    Index,Inner:TpvSizeInt;
    ContextRVA,StreamRVA,DirectoryRVA,Value32:TpvUInt32;
    Value16:TpvUInt16;
    Value64:TpvUInt64;
    NameRVAs:array of TpvUInt32;
    CVRVAs,CVSizes:array of TpvUInt32;
    Handle:THandle;
    TemporaryName:String;
    CommentText:UnicodeString;
    Module:PpvCrashReportMiniDumpModule;
    Entry:PpvCrashReportMiniDumpDirectoryEntry;
    Range:PpvCrashReportMiniDumpRange;

 procedure AddStream(const aType,aSize,aRVA:TpvUInt32);
 var Where:PpvCrashReportMiniDumpDirectoryEntry;
 begin
  if StreamCount<cMaximalStreams then begin
   Where:=@Directory[StreamCount];
   Where^.StreamType:=aType;
   Where^.Size:=aSize;
   Where^.RVA:=aRVA;
   inc(StreamCount);
  end;
 end;

 // Copies a piece of the address space into the file, if it is really there.
 // Nothing is trusted: the range has to lie inside one readable mapping, which
 // is what the process map is read for.
 procedure AddRange(const aStart:TpvPtrUInt;const aWanted:TpvSizeInt);
 var Which,Size:TpvSizeInt;
     Where:PpvCrashReportMiniDumpMapping;
     Into:PpvCrashReportMiniDumpRange;
 begin
  if (RangeCount>=cMaximalMemoryRanges) or (aStart=0) or (aWanted<=0) then begin
   exit;
  end;
  for Which:=0 to length(Mappings)-1 do begin
   Where:=@Mappings[Which];
   if Where^.Readable and (aStart>=Where^.Low) and (aStart<Where^.High) then begin
    Size:=aWanted;
    if TpvPtrUInt(Size)>(Where^.High-aStart) then begin
     Size:=TpvSizeInt(Where^.High-aStart);
    end;
    if Size>0 then begin
     Into:=@Ranges[RangeCount];
     Into^.Start:=aStart;
     Into^.Size:=TpvUInt32(Size);
     Into^.RVA:=CrashReportMiniDumpAppend(Buffer,TpvPointer(aStart)^,Size);
     inc(RangeCount);
    end;
    exit;
   end;
  end;
 end;

begin
 result:=false;

 // What is being described. Either the fault which was kept aside, or, when
 // nothing has faulted, this thread as it is right now, which is what somebody
 // asking for a dump without a crash is asking about.
 if pvCrashReportLastFault(Fault)=0 then begin
  FillChar(Fault,SizeOf(TpvCrashReportUnixFault),#0);
  FillChar(UnixContext,SizeOf(UnixContext),#0);
  if CrashReportMiniDumpGetContext(@UnixContext[0])<>0 then begin
   exit;
  end;
  Move(UnixContext[cMiniDumpContextGRegs],Fault.Registers,SizeOf(Fault.Registers));
  Fault.ThreadID:=TpvInt32(GetProcessID);
 end;

 if not CrashReportMiniDumpReadMappings(Mappings,RawMaps) then begin
  exit;
 end;
 Modules:=CrashReportMiniDumpBuildModules(Mappings);

 Buffer.Data:=nil;
 Buffer.Size:=0;
 StreamCount:=0;
 RangeCount:=0;

 try

  // Room for the header, which is filled in last because half of what goes in
  // it is only known once everything else has been written.
  CrashReportMiniDumpAppendZeros(Buffer,32);

  CrashReportMiniDumpFillContext(ContextData,Fault);
  ContextRVA:=CrashReportMiniDumpAppend(Buffer,ContextData[0],cMiniDumpContextSize);

  // The stack of the thread which faulted, from where it stood up to the end of
  // the mapping it stands in, and a window around the instruction which
  // faulted.
{$ifdef PasVulkanCrashReportMiniDumpX64}
  AddRange(TpvPtrUInt(Fault.Registers[15]),cMiniDumpMaximalStackBytes);
  if Fault.Registers[16]>cMiniDumpBytesAroundInstruction then begin
   AddRange(TpvPtrUInt(Fault.Registers[16])-cMiniDumpBytesAroundInstruction,cMiniDumpBytesAroundInstruction*2);
  end;
{$else}
  AddRange(TpvPtrUInt(Fault.Registers[7]),cMiniDumpMaximalStackBytes);
  if Fault.Registers[14]>cMiniDumpBytesAroundInstruction then begin
   AddRange(TpvPtrUInt(Fault.Registers[14])-cMiniDumpBytesAroundInstruction,cMiniDumpBytesAroundInstruction*2);
  end;
{$endif}

  // One thread, the one this is about. The others are named in the text report
  // which travels in the comment stream, with their stacks, when the program
  // asked for that; what is not here is a machine state for each of them, which
  // would mean stopping every thread from the outside and is a piece of work of
  // its own.
  StreamRVA:=Buffer.Size;
  Value32:=1;
  CrashReportMiniDumpAppend(Buffer,Value32,SizeOf(TpvUInt32));
  Value32:=TpvUInt32(Fault.ThreadID);
  CrashReportMiniDumpAppend(Buffer,Value32,SizeOf(TpvUInt32));   // thread id
  CrashReportMiniDumpAppendZeros(Buffer,12);                     // suspend count, priorities
  Value64:=0;
  CrashReportMiniDumpAppend(Buffer,Value64,SizeOf(TpvUInt64));   // no thread block here
  if RangeCount>0 then begin
   Range:=@Ranges[0];
   CrashReportMiniDumpAppend(Buffer,Range^.Start,SizeOf(TpvUInt64));
   CrashReportMiniDumpAppend(Buffer,Range^.Size,SizeOf(TpvUInt32));
   CrashReportMiniDumpAppend(Buffer,Range^.RVA,SizeOf(TpvUInt32));
  end else begin
   CrashReportMiniDumpAppendZeros(Buffer,16);
  end;
  Value32:=cMiniDumpContextSize;
  CrashReportMiniDumpAppend(Buffer,Value32,SizeOf(TpvUInt32));
  CrashReportMiniDumpAppend(Buffer,ContextRVA,SizeOf(TpvUInt32));
  AddStream(cMiniDumpStreamThreadList,TpvUInt32(Buffer.Size)-StreamRVA,StreamRVA);

  // The names and the identities first, since the list itself is a run of
  // fixed size records which point at them.
  SetLength(NameRVAs,length(Modules));
  SetLength(CVRVAs,length(Modules));
  SetLength(CVSizes,length(Modules));
  for Index:=0 to length(Modules)-1 do begin
   Module:=@Modules[Index];
   NameRVAs[Index]:=CrashReportMiniDumpAppendString(Buffer,String(Module^.Path));
   if length(Module^.BuildID)>0 then begin
    Value32:=cMiniDumpCVSignatureELF;
    CVRVAs[Index]:=CrashReportMiniDumpAppend(Buffer,Value32,SizeOf(TpvUInt32));
    CrashReportMiniDumpAppend(Buffer,Module^.BuildID[1],length(Module^.BuildID));
    CVSizes[Index]:=TpvUInt32(SizeOf(TpvUInt32)+length(Module^.BuildID));
   end else begin
    CVRVAs[Index]:=0;
    CVSizes[Index]:=0;
   end;
  end;

  StreamRVA:=Buffer.Size;
  Value32:=TpvUInt32(length(Modules));
  CrashReportMiniDumpAppend(Buffer,Value32,SizeOf(TpvUInt32));
  for Index:=0 to length(Modules)-1 do begin
   Module:=@Modules[Index];
   Value64:=Module^.Base;
   CrashReportMiniDumpAppend(Buffer,Value64,SizeOf(TpvUInt64));
   Value32:=TpvUInt32(Module^.Size);
   CrashReportMiniDumpAppend(Buffer,Value32,SizeOf(TpvUInt32));
   CrashReportMiniDumpAppendZeros(Buffer,8);                       // checksum, time stamp
   CrashReportMiniDumpAppend(Buffer,NameRVAs[Index],SizeOf(TpvUInt32));
   CrashReportMiniDumpAppendZeros(Buffer,52);                      // version information
   CrashReportMiniDumpAppend(Buffer,CVSizes[Index],SizeOf(TpvUInt32));
   CrashReportMiniDumpAppend(Buffer,CVRVAs[Index],SizeOf(TpvUInt32));
   CrashReportMiniDumpAppendZeros(Buffer,8+16);                    // no misc record, two reserved
  end;
  AddStream(cMiniDumpStreamModuleList,TpvUInt32(Buffer.Size)-StreamRVA,StreamRVA);

  StreamRVA:=Buffer.Size;
  Value32:=TpvUInt32(RangeCount);
  CrashReportMiniDumpAppend(Buffer,Value32,SizeOf(TpvUInt32));
  for Index:=0 to RangeCount-1 do begin
   Range:=@Ranges[Index];
   CrashReportMiniDumpAppend(Buffer,Range^.Start,SizeOf(TpvUInt64));
   CrashReportMiniDumpAppend(Buffer,Range^.Size,SizeOf(TpvUInt32));
   CrashReportMiniDumpAppend(Buffer,Range^.RVA,SizeOf(TpvUInt32));
  end;
  AddStream(cMiniDumpStreamMemoryList,TpvUInt32(Buffer.Size)-StreamRVA,StreamRVA);

  // The fault itself. The number of the signal goes where a reader expects the
  // code of an exception, and what the kernel said about it goes into the
  // flags, which is the arrangement the crash reporters of this platform
  // settled on and therefore the one their tools read.
  StreamRVA:=Buffer.Size;
  Value32:=TpvUInt32(Fault.ThreadID);
  CrashReportMiniDumpAppend(Buffer,Value32,SizeOf(TpvUInt32));
  CrashReportMiniDumpAppendZeros(Buffer,4);
  Value32:=TpvUInt32(Fault.Signal);
  CrashReportMiniDumpAppend(Buffer,Value32,SizeOf(TpvUInt32));
  Value32:=TpvUInt32(Fault.Code);
  CrashReportMiniDumpAppend(Buffer,Value32,SizeOf(TpvUInt32));
  Value64:=0;
  CrashReportMiniDumpAppend(Buffer,Value64,SizeOf(TpvUInt64));    // no record behind this one
  Value64:=TpvUInt64(TpvPtrUInt(Fault.Address));
  CrashReportMiniDumpAppend(Buffer,Value64,SizeOf(TpvUInt64));
  CrashReportMiniDumpAppendZeros(Buffer,8+(15*8));                // no parameters
  Value32:=cMiniDumpContextSize;
  CrashReportMiniDumpAppend(Buffer,Value32,SizeOf(TpvUInt32));
  CrashReportMiniDumpAppend(Buffer,ContextRVA,SizeOf(TpvUInt32));
  AddStream(cMiniDumpStreamException,TpvUInt32(Buffer.Size)-StreamRVA,StreamRVA);

  StreamRVA:=Buffer.Size;
  Value16:=cMiniDumpArchitecture;
  CrashReportMiniDumpAppend(Buffer,Value16,SizeOf(TpvUInt16));
  CrashReportMiniDumpAppendZeros(Buffer,6);                       // level, revision, counts
  CrashReportMiniDumpAppendZeros(Buffer,12);                      // three version numbers
  Value32:=cMiniDumpPlatformLinux;
  CrashReportMiniDumpAppend(Buffer,Value32,SizeOf(TpvUInt32));
  // The name of the system version, which has to point at a real string even
  // when there is nothing to say. Written after the stream and pointed back at.
  Value32:=0;
  Inner:=Buffer.Size;
  CrashReportMiniDumpAppend(Buffer,Value32,SizeOf(TpvUInt32));
  CrashReportMiniDumpAppendZeros(Buffer,4);                       // suite mask, reserved
  CrashReportMiniDumpAppendZeros(Buffer,24);                      // what the processor can do
  AddStream(cMiniDumpStreamSystemInfo,TpvUInt32(Buffer.Size)-StreamRVA,StreamRVA);
  Value32:=CrashReportMiniDumpAppendString(Buffer,'');
  CrashReportMiniDumpPut32(Buffer,TpvUInt32(Inner),Value32);

  StreamRVA:=CrashReportMiniDumpAppend(Buffer,RawMaps[1],length(RawMaps));
  AddStream(cMiniDumpStreamLinuxMaps,TpvUInt32(length(RawMaps)),StreamRVA);

  if length(aRequest^.Comment^)>0 then begin
   CommentText:=UnicodeString(aRequest^.Comment^);
   StreamRVA:=CrashReportMiniDumpAppend(Buffer,PWideChar(CommentText)^,length(CommentText)*SizeOf(WideChar));
   CrashReportMiniDumpAppendZeros(Buffer,SizeOf(WideChar));
   AddStream(cMiniDumpStreamCommentW,TpvUInt32((length(CommentText)+1)*SizeOf(WideChar)),StreamRVA);
  end;

  DirectoryRVA:=Buffer.Size;
  for Index:=0 to StreamCount-1 do begin
   Entry:=@Directory[Index];
   CrashReportMiniDumpAppend(Buffer,Entry^.StreamType,SizeOf(TpvUInt32));
   CrashReportMiniDumpAppend(Buffer,Entry^.Size,SizeOf(TpvUInt32));
   CrashReportMiniDumpAppend(Buffer,Entry^.RVA,SizeOf(TpvUInt32));
  end;

  CrashReportMiniDumpPut32(Buffer,0,cMiniDumpSignature);
  CrashReportMiniDumpPut32(Buffer,4,cMiniDumpVersion);
  CrashReportMiniDumpPut32(Buffer,8,TpvUInt32(StreamCount));
  CrashReportMiniDumpPut32(Buffer,12,DirectoryRVA);
  CrashReportMiniDumpPut32(Buffer,16,0);
  CrashReportMiniDumpPut32(Buffer,20,0);

  // Beside the wanted name and moved onto it, the same way as on the other
  // platform. The process number is in the temporary name, so two processes
  // writing to one target cannot be building the same file.
  TemporaryName:=aRequest^.FileName^+'.part'+IntToStr(TpvInt64(GetProcessID));
  Handle:=FileCreate(TemporaryName);
  if Handle=THandle(-1) then begin
   exit;
  end;
  try
   result:=FileWrite(Handle,Buffer.Data[0],Buffer.Size)=Buffer.Size;
  finally
   FileClose(Handle);
  end;
  if result then begin
   result:=RenameFile(TemporaryName,aRequest^.FileName^);
  end;
  if not result then begin
   DeleteFile(TemporaryName);
  end;

 except
  // Same rule as everywhere here: the report about the crash outranks whatever
  // goes wrong while it is being made.
  result:=false;
 end;
end;

{$endif}

procedure pvCrashReportMiniDumpInstall;
begin
{$if defined(Windows)}
 CrashReportMiniDumpLoad;
 // The thread and its two events, all three of which have to be there before
 // the first fault rather than made at the moment of one.
 //
 // Both events reset themselves after one waiter, which is what makes the
 // handshake a handshake: one dump asked for is one dump woken, and an answer
 // which nobody was still waiting for does not stay behind for the next one.
 if pvCrashReportMiniDumpWantDumperThread and not CrashReportMiniDumpThreadReady then begin
  CrashReportMiniDumpThreadQuit:=false;
  CrashReportMiniDumpGoEvent:=CreateEvent(nil,false,false,nil);
  CrashReportMiniDumpDoneEvent:=CreateEvent(nil,false,false,nil);
  CrashReportMiniDumpStoppedEvent:=CreateEvent(nil,true,false,nil);
  if (CrashReportMiniDumpGoEvent<>0) and (CrashReportMiniDumpDoneEvent<>0) and (CrashReportMiniDumpStoppedEvent<>0) then begin
   // Through the runtime rather than through CreateThread, so that the thread
   // is one the runtime knows about. It works with strings and it catches
   // exceptions, and both of those want the per thread state which only this
   // way sets up.
   //
   // A megabyte of stack, said out loud rather than left to the header of the
   // executable, because the one thing this thread exists for is to have stack
   // when the thread which crashed has none.
   CrashReportMiniDumpThreadID:=0;
   CrashReportMiniDumpThreadReady:=BeginThread(nil,1024*1024,@CrashReportMiniDumpThreadProc,nil,0,CrashReportMiniDumpThreadID)<>0;
  end;
  if not CrashReportMiniDumpThreadReady then begin
   // No thread means every dump is written where it was asked for, which is
   // what this unit did before the thread existed and still does when the
   // program asks for it. So there is nothing to report here beyond giving the
   // handles back.
   CrashReportMiniDumpCloseEvents;
  end;
 end;
{$ifend}
end;

procedure pvCrashReportMiniDumpUninstall;
{$if defined(Windows)}
var Library_:HMODULE;
begin
 // The thread first, since it is the one which might still be using the
 // library. Told to stop and then waited for, but not waited for without end:
 // if it is in the middle of a dump which will not finish, this cannot be the
 // place where that becomes somebody else's hang.
 if CrashReportMiniDumpThreadReady then begin
  CrashReportMiniDumpThreadReady:=false;
  CrashReportMiniDumpThreadQuit:=true;
  SetEvent(CrashReportMiniDumpGoEvent);
  if WaitForSingleObject(CrashReportMiniDumpStoppedEvent,pvCrashReportMiniDumpWaitMilliseconds)<>WAIT_OBJECT_0 then begin
   // Still in there. Nothing is taken away from it: not the handles, since
   // closing one a thread is about to wait on is how a shutdown turns into a
   // fault, and not the library, since it is what that thread is inside of.
   // What is given up instead is three handles and a library in a process which
   // is on its way out.
   CrashReportMiniDumpThreadID:=0;
   exit;
  end;
 end;
 CrashReportMiniDumpCloseEvents;
 CrashReportMiniDumpThreadID:=0;
 CrashReportMiniDumpAcquireLock;
 try
  Library_:=CrashReportMiniDumpLibrary;
  CrashReportMiniDumpWriteDumpProc:=nil;
  CrashReportMiniDumpLibrary:=0;
  // Left as tried, so that a dump asked for after this does not quietly load
  // the library again behind the back of whoever just gave it back.
  if Library_<>0 then begin
   FreeLibrary(Library_);
  end;
 finally
  CrashReportMiniDumpReleaseLock;
 end;
end;
{$else}
begin
end;
{$ifend}

function pvCrashReportMiniDumpAvailable:Boolean;
begin
{$if defined(Windows)}
 result:=assigned(pvCrashReportMiniDumpWriter) or CrashReportMiniDumpLoad;
{$elseif defined(PasVulkanCrashReportMiniDumpUnix)}
 // Nothing to load here. The file is written by this unit itself, so the only
 // question is whether a writer of somebody else's has taken over, and the
 // answer either way is yes.
 result:=true;
{$else}
 // Neither a library which writes this nor the two things it would be written
 // out of: the state of a fault and a process map. Saying so is more use than
 // an empty file with the right extension. What this platform has instead is
 // the core dump of the operating system, which is the business of whoever runs
 // the program, and the text report of the sibling unit, which is complete.
 result:=assigned(pvCrashReportMiniDumpWriter);
{$ifend}
end;

function pvCrashReportMiniDumpFileName(const aDirectory:String):String;
var Directory,BaseName:String;
begin
 Directory:=aDirectory;
 if length(Directory)=0 then begin
  Directory:=ExtractFilePath(ParamStr(0));
 end;
 if length(Directory)>0 then begin
  Directory:=IncludeTrailingPathDelimiter(Directory);
 end;
 BaseName:=ChangeFileExt(ExtractFileName(ParamStr(0)),'');
 if length(BaseName)=0 then begin
  BaseName:='crash';
 end;
 // The time to tell two crashes of one process apart and the process
 // identifier to tell two processes apart. Both are needed: a program which
 // faults twice within the same second is exactly the program whose second
 // fault one wants to see.
 result:=Directory+BaseName+'-'+
         FormatDateTime('yyyymmdd-hhnnss',Now)+'-'+
{$if defined(Windows)}
         IntToStr(GetCurrentProcessId)+
{$else}
         IntToStr(TpvInt64(GetProcessID))+
{$ifend}
         '.dmp';
end;

function pvCrashReportWriteMiniDump(const aFileName:String;
                                    const aExceptionPointers:TpvPointer;
                                    const aExceptionCode:TpvUInt32;
                                    const aThreadID:TpvUInt64;
                                    const aComment:String;
                                    const aKind:TpvCrashReportMiniDumpKind):Boolean;
{$if defined(Windows)}
// Deliberately few of these, and none of them large. This is the frame which
// has to fit on the stack of the thread which crashed, and that thread may have
// crashed precisely because there was no more stack. Everything the library
// needs is a frame further on, in CrashReportMiniDumpWriteHere, which normally
// runs on a thread which has its whole stack left.
var Pointers:TpvPointer;
    Code:TpvUInt32;
    ThreadID:TpvUInt64;
    Sequence,KeptCode:TpvUInt32;
    KeptThreadID:TpvUInt64;
    Request:TpvCrashReportMiniDumpRequest;
begin
 result:=false;
 if length(aFileName)=0 then begin
  exit;
 end;
 Pointers:=aExceptionPointers;
 Code:=aExceptionCode;
 ThreadID:=aThreadID;
 Sequence:=0;
 if not assigned(Pointers) then begin
  // Nothing was handed in, so the state which the fault handler of the sibling
  // unit kept aside is used instead. Which thread it was is then known as well,
  // and is not the one asking, since a report is often written by another.
  Sequence:=pvCrashReportLastFault(Pointers,KeptCode,KeptThreadID);
  if Sequence<>0 then begin
   if Code=0 then begin
    Code:=KeptCode;
   end;
   if ThreadID=0 then begin
    ThreadID:=KeptThreadID;
   end;
  end;
 end;
 if ThreadID=0 then begin
  ThreadID:=TpvUInt64(GetCurrentThreadId);
 end;
 if (Code=0) and assigned(Pointers) and assigned(PpvCrashReportMiniDumpPointers(Pointers)^.ExceptionRecord) then begin
  Code:=PpvCrashReportMiniDumpPointers(Pointers)^.ExceptionRecord^.ExceptionCode;
 end;

 // A writer of somebody else's takes over the whole job from here, file name
 // and all, since the service behind such a writer usually has an opinion about
 // where a dump goes and this unit has no business overruling it.
 if assigned(pvCrashReportMiniDumpWriter) then begin
  try
   result:=pvCrashReportMiniDumpWriter(aFileName,Pointers,Code,ThreadID,aComment,aKind,pvCrashReportMiniDumpWriterUserData);
  except
   // A writer which fails is a dump which was not written, and nothing more.
   // Letting it out of here would replace the report about the crash with a
   // report about the reporting, which is the one exchange never worth making.
   result:=false;
  end;
  exit;
 end;

 if not CrashReportMiniDumpLoad then begin
  exit;
 end;

 // One at a time from here on, and the name is taken further in as well, so
 // that two threads cannot even be in the middle of choosing one at once.
 if not CrashReportMiniDumpAcquireWriteLock then begin
  exit;
 end;
 try

  // The thread which was made for this if there is one, and this thread itself
  // if there is not or if it did not answer. The right to write is held here
  // and not taken again over there, which is what keeps the two from waiting
  // for one another.
  if not CrashReportMiniDumpHandOver(@aFileName,@aComment,Pointers,Code,ThreadID,aKind,result) then begin
   Request.FileName:=@aFileName;
   Request.Comment:=@aComment;
   Request.Pointers:=Pointers;
   Request.Code:=Code;
   Request.ThreadID:=ThreadID;
   Request.Kind:=aKind;
   Request.Answer:=false;
   result:=CrashReportMiniDumpWriteHere(@Request);
  end;

 finally
  CrashReportMiniDumpReleaseWriteLock;
 end;
end;
{$else}
{$ifdef PasVulkanCrashReportMiniDumpUnix}
var Request:TpvCrashReportMiniDumpRequest;
{$endif}
begin
 result:=false;
 if assigned(pvCrashReportMiniDumpWriter) then begin
  try
   result:=pvCrashReportMiniDumpWriter(aFileName,aExceptionPointers,aExceptionCode,aThreadID,aComment,aKind,pvCrashReportMiniDumpWriterUserData);
  except
   result:=false;
  end;
  exit;
 end;
{$ifdef PasVulkanCrashReportMiniDumpUnix}
 if length(aFileName)=0 then begin
  exit;
 end;
 // The kind is taken and ignored on this platform. What a dump holds here is
 // decided by what can be got at without stopping the process, which is the
 // faulting thread and the layout of the address space, and there is no larger
 // and no smaller version of that to choose between.
 Request.FileName:=@aFileName;
 Request.Comment:=@aComment;
 Request.Pointers:=aExceptionPointers;
 Request.Code:=aExceptionCode;
 Request.ThreadID:=aThreadID;
 Request.Kind:=aKind;
 Request.Answer:=false;
 result:=CrashReportMiniDumpWriteHereUnix(@Request);
{$endif}
end;
{$ifend}

initialization

 // Nothing here on purpose, see pvCrashReportMiniDumpInstall.

finalization

 // Nothing here either, and for the mirror image of the same reason. Giving a
 // library back runs through the loader just as taking one does, and the
 // finalization of a unit inside a library runs while the operating system
 // holds the loader lock, so this is not the place for it.
 //
 // What is given up by leaving it is a page or two of address space in a
 // process which is on its way out anyway, and what is gained is that a fault
 // during shutdown, which is a common enough kind, still finds everything it
 // needs to be written down. A program which really wants the library gone
 // says so itself, from somewhere which is not a finalization.

end.
