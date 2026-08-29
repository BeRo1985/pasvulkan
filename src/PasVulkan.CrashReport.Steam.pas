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
unit PasVulkan.CrashReport.Steam;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

uses SysUtils,
     Classes,
     PasVulkan.Types,
     PasVulkan.CrashReport,
     PasVulkan.CrashReport.MiniDump,
     PasVulkan.Steamworks;

// This unit hands a crash to the service the program is delivered through,
// instead of, or in addition to, leaving a file behind for somebody to find.
//
// It is separate from PasVulkan.CrashReport.MiniDump for the reason that unit
// gives for being separate from PasVulkan.CrashReport in turn: it needs a
// binding which a program that is not delivered that way should not have to
// carry along. Nothing else in the framework uses this unit, so the binding
// arrives only where somebody asked for it by name in a uses clause.
//
// There are two ways in, and they are less alternatives than different
// divisions of labour.
//
// The first is the one this framework is built around. The fault is caught
// here, the report is assembled here, and the dump is asked for here, with the
// report travelling along as its comment. All the service does is take the
// finished dump and get it off the machine. That is what pvCrashReportSteamInstall
// sets up, and it is the one worth using, because everything the sibling unit
// knows how to put into a report ends up in what the service receives.
//
// The second hands the whole job over: the service installs a fault handler of
// its own, catches the fault itself and uploads without asking anybody here.
// That is pvCrashReportSteamUseBreakpadCrashHandler. It is worth having for two
// reasons. It catches faults which arrive before this program has finished
// starting up, and it is the only one of the two which does anything at all
// away from Windows, where a minidump is not a thing the operating system has.
// What it costs is the report: that handler knows nothing of this framework and
// uploads the machine state alone.
//
// Nothing here loads the binding. A program which talks to the service has
// loaded it long before it crashes, and one which has not is not a program the
// service would accept a dump from anyway, so loading it at the moment of a
// crash would buy nothing and cost a trip through the loader.

// The build the dump belongs to, as the service counts builds, so that a dump
// which arrives can be lined up against the right one. Zero means unsaid.
var pvCrashReportSteamBuildID:TpvUInt32=0;

    // Whether the installed writer leaves the file behind as well, before it
    // hands the dump to the service.
    //
    // On by default, and the order is not arbitrary. The file is the copy which
    // exists no matter what the network, the client or the service does next,
    // and it is written first for exactly that reason. The upload is the part
    // which can fail quietly and be picked up again later by somebody else's
    // code.
    //
    // Off makes the service the only place a dump goes, which is the shape the
    // writer hook was made for and the tidier one for a build which ships.
    pvCrashReportSteamWritesFileToo:Boolean=true;

// Whether a dump can be handed over at all: the binding loaded, and the routine
// which takes one resolved in it.
function pvCrashReportSteamAvailable:Boolean;

// Hands one over, and reports whether it got that far.
//
// aExceptionPointers is what an exception filter is handed, and passing nothing
// falls back to the state PasVulkan.CrashReport kept aside at the fault, the
// same way pvCrashReportWriteMiniDump does. The service wants those pointers:
// it walks them itself rather than being handed a finished file, so a call with
// nothing to walk describes a thread which is not faulting.
//
// aComment travels with the dump and is meant to be pvCrashReportFullReport. It
// is copied into a buffer of this unit's own rather than passed straight on,
// because the routine which takes it wants a C string and a crash path is the
// wrong place to be building one. That buffer has a size, so a report longer
// than it is cut short at a line ending rather than in the middle of a
// character.
//
// This writes no file and does not touch the writer hook. It is the bare call,
// for a program which wants to decide for itself when the service hears about a
// crash.
function pvCrashReportSteamWriteMiniDump(const aExceptionPointers:TpvPointer=nil;
                                         const aExceptionCode:TpvUInt32=0;
                                         const aComment:TpvUTF8String=''):Boolean;

// Puts the above into pvCrashReportMiniDumpWriter, so that every dump the
// framework asks for goes to the service, and, for as long as
// pvCrashReportSteamWritesFileToo says so, into a file first.
//
// A writer which was already there is remembered and asked afterwards, so that
// installing this does not quietly unhook somebody else's.
procedure pvCrashReportSteamInstall;

procedure pvCrashReportSteamUninstall;

function pvCrashReportSteamInstalled:Boolean;

// The app a dump belongs to, for the service's own handler, which otherwise has
// no way of knowing: a fault can arrive before the API has been brought up, and
// then there is nothing left to ask.
//
// Meant to be set before the API is brought up. Setting it later is not an
// error, it is simply late for the faults it was supposed to cover.
//
// False where the binding has no such routine.
function pvCrashReportSteamSetBreakpadAppID(const aAppID:TpvUInt32):Boolean;

// Lets the service install a fault handler of its own, see the note at the top
// of this unit for what that trades away.
//
// The three strings name the build to the service and are kept alive here for
// as long as the process runs, since what is passed on is a pointer into them
// and there is no telling when it is read.
//
// aFullMemoryDumps asks for the whole address space rather than the stacks. For
// a program with a renderer that is gigabytes, most of it mapped device memory,
// so it is off unless somebody has a reason.
//
// False where the binding has no such routine.
function pvCrashReportSteamUseBreakpadCrashHandler(const aVersion:TpvUTF8String;
                                                   const aDate:TpvUTF8String;
                                                   const aTime:TpvUTF8String;
                                                   const aFullMemoryDumps:Boolean=false):Boolean;

implementation

const // How much of the report travels with the dump.
      //
      // A fixed buffer and not an allocation, because the one moment this is
      // used is the one moment allocating is worst, so the size is decided here
      // and not by whatever the report happened to come to. Wide enough for a
      // report with the registers, the history and the stacks of a few dozen
      // threads in it.
      cCommentSize=16384;

var // Filled by pvCrashReportSteamWriteMiniDump and read by the service before
    // that call returns, so it lives exactly as long as it has to. On the path
    // through the writer hook the write lock of the sibling unit keeps two
    // threads out of it at once; the bare call is the caller's own to
    // serialise, and the worst which comes of not doing so is a comment from
    // the other thread's report.
    CrashReportSteamComment:array[0..cCommentSize] of AnsiChar;

    // What stood in the writer hook before this unit was put into it, so that
    // uninstalling gives it back and installing does not lose it.
    CrashReportSteamPreviousWriter:TpvCrashReportMiniDumpWriter=nil;
    CrashReportSteamPreviousUserData:TpvPointer=nil;
    CrashReportSteamIsInstalled:Boolean=false;

    // Held for the lifetime of the process, see the note at
    // pvCrashReportSteamUseBreakpadCrashHandler.
    CrashReportSteamBreakpadVersion:TpvUTF8String='';
    CrashReportSteamBreakpadDate:TpvUTF8String='';
    CrashReportSteamBreakpadTime:TpvUTF8String='';

function pvCrashReportSteamAvailable:Boolean;
begin
 result:=assigned(SteamworksLibraryHandle) and assigned(SteamAPI_WriteMiniDump);
end;

// Copies as much of the report into the fixed buffer as fits, and says how much
// that was.
//
// Cutting a report short is better than sending none, but cutting it in the
// middle of a character would leave the service with something which is not
// text any more, and cutting it in the middle of a line would leave whoever
// reads it wondering whether the frame at the end is a whole one.
// Nothing is converted on the way in, which is the point of the report being a
// TpvUTF8String in the first place: what the service is handed is the bytes
// which were already there, and no copy of them is made on a path where making
// one is worst.
function CrashReportSteamFillComment(const aComment:TpvUTF8String):TpvSizeInt;
var Count,Index:TpvSizeInt;
begin

 result:=0;
 CrashReportSteamComment[0]:=#0;

 Count:=length(aComment);
 if Count=0 then begin
  exit;
 end;

 if Count>cCommentSize then begin

  Count:=cCommentSize;

  // A character boundary first, since that one is not negotiable. A byte which
  // continues a character has its two top bits set to one and no other byte
  // does, which is the whole of what has to be known here.
  while (Count>0) and ((ord(aComment[Count+1]) and $C0)=$80) do begin
   dec(Count);
  end;

  // And back to the last line ending on top of that, but only while that line
  // ending is near enough to the cut to be worth going back to. A report which
  // happens to hold one very long line would otherwise arrive as the handful of
  // bytes before it, which is the one outcome worse than a ragged last line.
  Index:=Count;
  while (Index>0) and (aComment[Index]<>#10) do begin
   dec(Index);
  end;
  if (Index*4)>=(Count*3) then begin
   Count:=Index;
  end;

 end;

 if Count>0 then begin
  Move(aComment[1],CrashReportSteamComment[0],Count);
 end;
 CrashReportSteamComment[Count]:=#0;
 result:=Count;

end;

function pvCrashReportSteamWriteMiniDump(const aExceptionPointers:TpvPointer;
                                         const aExceptionCode:TpvUInt32;
                                         const aComment:TpvUTF8String):Boolean;
var Pointers:TpvPointer;
    Code:TpvUInt32;
{$if defined(Windows)}
    Sequence,KeptCode:TpvUInt32;
    KeptThreadID:TpvUInt64;
{$ifend}
begin

 result:=false;

 if not pvCrashReportSteamAvailable then begin
  exit;
 end;

 Pointers:=aExceptionPointers;
 Code:=aExceptionCode;
{$if defined(Windows)}
 if not assigned(Pointers) then begin
  // The same fallback the sibling unit makes, and for the same reason: the
  // place which writes a report is often not the place which faulted, and the
  // state kept aside at the fault is the only thing left which describes it.
  //
  // Only here. What the routine below wants is a pointer to the two records an
  // exception filter is handed, and away from Windows there is no such thing to
  // hand it, whatever the library may still export under that name. The state
  // the sibling unit keeps on those platforms describes a signal instead, which
  // is not the same shape and would be read as the wrong one.
  Sequence:=pvCrashReportLastFault(Pointers,KeptCode,KeptThreadID);
  if (Sequence<>0) and (Code=0) then begin
   Code:=KeptCode;
  end;
 end;
{$ifend}

 try

  if assigned(SteamAPI_SetMiniDumpComment) and (CrashReportSteamFillComment(aComment)>0) then begin
   SteamAPI_SetMiniDumpComment(PSteamChar(@CrashReportSteamComment[0]));
  end;

  SteamAPI_WriteMiniDump(Code,Pointers,pvCrashReportSteamBuildID);

  // The routine says nothing about how it went, so the most which can honestly
  // be reported is that it was reached and came back. A dump which the client
  // takes and then fails to send is something which happens later and
  // elsewhere.
  result:=true;

 except
  // A service which throws is a dump which was not handed over and nothing
  // more, for the reason written out at the writer hook in the sibling unit.
  result:=false;
 end;

end;

function CrashReportSteamWriter(const aFileName:TpvUTF8String;
                                const aExceptionPointers:TpvPointer;
                                const aExceptionCode:TpvUInt32;
                                const aThreadID:TpvUInt64;
                                const aComment:TpvUTF8String;
                                const aKind:TpvCrashReportMiniDumpKind;
                                const aUserData:TpvPointer):Boolean;
var FileWritten,HandedOver:Boolean;
begin

 // The file first, while there is still a process to write it from, see
 // pvCrashReportSteamWritesFileToo.
 if pvCrashReportSteamWritesFileToo then begin
  FileWritten:=pvCrashReportWriteMiniDumpFile(aFileName,aExceptionPointers,aExceptionCode,aThreadID,aComment,aKind);
 end else begin
  FileWritten:=false;
 end;

 HandedOver:=pvCrashReportSteamWriteMiniDump(aExceptionPointers,aExceptionCode,aComment);

 // Whoever stood in the hook before this unit did still gets a turn, so that
 // installing this is an addition and not a replacement.
 if assigned(CrashReportSteamPreviousWriter) then begin
  try
   if CrashReportSteamPreviousWriter(aFileName,aExceptionPointers,aExceptionCode,aThreadID,aComment,aKind,CrashReportSteamPreviousUserData) then begin
    HandedOver:=true;
   end;
  except
   // As above.
  end;
 end;

 // True when the dump got somewhere, which is the question the caller asked.
 result:=FileWritten or HandedOver;

end;

procedure pvCrashReportSteamInstall;
begin
 if not CrashReportSteamIsInstalled then begin
  CrashReportSteamPreviousWriter:=pvCrashReportMiniDumpWriter;
  CrashReportSteamPreviousUserData:=pvCrashReportMiniDumpWriterUserData;
  CrashReportSteamIsInstalled:=true;
  pvCrashReportMiniDumpWriterUserData:=nil;
  // Last, so that a thread which faults in the middle of this finds either the
  // state before or the state after and never half of either.
  pvCrashReportMiniDumpWriter:=@CrashReportSteamWriter;
 end;
end;

procedure pvCrashReportSteamUninstall;
begin
 if CrashReportSteamIsInstalled then begin
  // First, for the mirror image of the reason above.
  pvCrashReportMiniDumpWriter:=CrashReportSteamPreviousWriter;
  pvCrashReportMiniDumpWriterUserData:=CrashReportSteamPreviousUserData;
  CrashReportSteamPreviousWriter:=nil;
  CrashReportSteamPreviousUserData:=nil;
  CrashReportSteamIsInstalled:=false;
 end;
end;

function pvCrashReportSteamInstalled:Boolean;
begin
 result:=CrashReportSteamIsInstalled;
end;

function pvCrashReportSteamSetBreakpadAppID(const aAppID:TpvUInt32):Boolean;
begin
 result:=assigned(SteamworksLibraryHandle) and assigned(SteamAPI_SetBreakpadAppID);
 if result then begin
  try
   SteamAPI_SetBreakpadAppID(aAppID);
  except
   result:=false;
  end;
 end;
end;

function pvCrashReportSteamUseBreakpadCrashHandler(const aVersion:TpvUTF8String;
                                                   const aDate:TpvUTF8String;
                                                   const aTime:TpvUTF8String;
                                                   const aFullMemoryDumps:Boolean):Boolean;
begin
 result:=assigned(SteamworksLibraryHandle) and assigned(SteamAPI_UseBreakpadCrashHandler);
 if result then begin
  CrashReportSteamBreakpadVersion:=aVersion;
  CrashReportSteamBreakpadDate:=aDate;
  CrashReportSteamBreakpadTime:=aTime;
  try
   SteamAPI_UseBreakpadCrashHandler(PSteamChar(CrashReportSteamBreakpadVersion),
                                    PSteamChar(CrashReportSteamBreakpadDate),
                                    PSteamChar(CrashReportSteamBreakpadTime),
                                    aFullMemoryDumps,
                                    nil,
                                    nil);
  except
   result:=false;
  end;
 end;
end;

initialization

// Nothing here on purpose. Naming this unit in a uses clause says that the
// program has the binding, not that it wants every crash sent, and those two
// are different enough that the second is asked for by name.

finalization

// And nothing here either. A fault during shutdown is a common enough kind, and
// unhooking the writer while one may still be on its way would turn a dump
// which was about to be sent into one which is not.

end.
