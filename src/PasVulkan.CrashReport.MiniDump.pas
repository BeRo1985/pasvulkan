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
     PpvCrashReportMiniDumpExceptionInformation=^TpvCrashReportMiniDumpExceptionInformation;
     TpvCrashReportMiniDumpExceptionInformation=record
      ThreadID:TpvUInt32;
      ExceptionPointers:TpvPointer;
      // False says the pointers above are in this process, which is what they
      // are, since this dumps itself.
      ClientPointers:LongBool;
     end;

     PpvCrashReportMiniDumpUserStream=^TpvCrashReportMiniDumpUserStream;
     TpvCrashReportMiniDumpUserStream=record
      StreamType:TpvUInt32;
      BufferSize:TpvUInt32;
      Buffer:TpvPointer;
     end;

     PpvCrashReportMiniDumpUserStreamInformation=^TpvCrashReportMiniDumpUserStreamInformation;
     TpvCrashReportMiniDumpUserStreamInformation=record
      UserStreamCount:TpvUInt32;
      UserStreamArray:PpvCrashReportMiniDumpUserStream;
     end;

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
     PpvCrashReportMiniDumpExceptionRecord=^TpvCrashReportMiniDumpExceptionRecord;
     TpvCrashReportMiniDumpExceptionRecord=record
      ExceptionCode:TpvUInt32;
      ExceptionFlags:TpvUInt32;
      ExceptionRecord:PpvCrashReportMiniDumpExceptionRecord;
      ExceptionAddress:TpvPointer;
      NumberParameters:TpvUInt32;
      ExceptionInformation:array[0..14] of TpvPtrUInt;
     end;

     PpvCrashReportMiniDumpPointers=^TpvCrashReportMiniDumpPointers;
     TpvCrashReportMiniDumpPointers=record
      ExceptionRecord:PpvCrashReportMiniDumpExceptionRecord;
      ContextRecord:TpvPointer;
     end;

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
function CrashReportMiniDumpAcquireTemporaryFile(const aFileName:String;out aTemporaryName:String):THandle;
var Index:TpvInt32;
    Candidate:String;
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
  if result<>INVALID_HANDLE_VALUE then begin
   aTemporaryName:=Candidate;
   exit;
  end;
 end;
end;

{$ifend}

procedure pvCrashReportMiniDumpInstall;
begin
{$if defined(Windows)}
 CrashReportMiniDumpLoad;
{$ifend}
end;

procedure pvCrashReportMiniDumpUninstall;
{$if defined(Windows)}
var Library_:HMODULE;
begin
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
{$else}
 // Nothing of the sort exists here, and saying so is more use than an empty
 // file with the right extension. What this platform has instead is the core
 // dump of the operating system, which is the business of whoever runs the
 // program, and the text report of the sibling unit, which is complete.
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
var Attempt:TpvInt32;
    Pointers:TpvPointer;
    Code:TpvUInt32;
    ThreadID:TpvUInt64;
    Sequence,KeptCode:TpvUInt32;
    KeptThreadID:TpvUInt64;
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

 // One at a time from here on, and the name is taken inside as well, so that
 // two threads cannot even be in the middle of choosing one at once.
 if not CrashReportMiniDumpAcquireWriteLock then begin
  exit;
 end;
 // Again two of these around one another: the outer one gives the right to
 // write back under every circumstance including an exit in the middle, and the
 // inner one keeps whatever went wrong from leaving through this function.
 try
 try

  Handle:=CrashReportMiniDumpAcquireTemporaryFile(aFileName,TemporaryName);
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
    if assigned(Pointers) then begin
     ExceptionInformation.ThreadID:=TpvUInt32(ThreadID);
     ExceptionInformation.ExceptionPointers:=Pointers;
     ExceptionInformation.ClientPointers:=false;
     ExceptionParameter:=@ExceptionInformation;
    end;

    UserStreamParameter:=nil;
    CommentText:='';
    if length(aComment)>0 then begin
     CommentText:=UnicodeString(aComment);
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
                                         CrashReportMiniDumpFlags(aKind,Attempt),
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
    result:=MoveFileExW(PWideChar(UnicodeString(TemporaryName)),PWideChar(UnicodeString(aFileName)),MOVEFILE_REPLACE_EXISTING);
   end;

  finally
   if not result then begin
    DeleteFileW(PWideChar(UnicodeString(TemporaryName)));
   end;
  end;

 except
  // Same rule as above: the report about the crash outranks everything which
  // happens while it is being made.
  result:=false;
 end;
 finally
  CrashReportMiniDumpReleaseWriteLock;
 end;
end;
{$else}
begin
 result:=false;
 if assigned(pvCrashReportMiniDumpWriter) then begin
  try
   result:=pvCrashReportMiniDumpWriter(aFileName,aExceptionPointers,aExceptionCode,aThreadID,aComment,aKind,pvCrashReportMiniDumpWriterUserData);
  except
   result:=false;
  end;
 end;
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
