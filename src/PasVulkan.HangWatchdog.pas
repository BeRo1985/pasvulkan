(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2026, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.HangWatchdog;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

// A frozen program says nothing by itself: no exception, no log line and no exit. The picture
// stands still, the audio thread carries on with whatever loop it was in, and the process sits
// there until somebody ends it by hand - which on somebody else's machine usually means the run
// is simply lost, together with whatever it was about to say.
//
// So the program says it itself. The main loop leaves a beat behind on every turn, a thread of its
// own watches that beat, and once it has stopped moving for long enough that thread writes down for
// how long and what the program was doing at the time. After a few of those reports it ends the
// process, because a main loop which has not come back around in nearly a minute is not coming back
// at all, and a program which is only still making noise is worse than one which is gone.
//
// Costs one interlocked increment per turn of the loop and nothing at all besides while everything
// runs. Driven by TpvApplication, so every program built on it is watched without doing anything;
// one which is not can call the three procedures below itself.
//
// The reports are written with plain file handling rather than through a log, because a freeze
// which sits on a lock could just as well sit on the one a log takes, and then the report would be
// lost at exactly the moment it is needed.

interface

uses {$if defined(Windows)}
      Windows,
     {$ifend}
     {$if defined(Unix)}
      BaseUnix,
     {$ifend}
     SysUtils,
     Classes,
     PasMP,
     PasVulkan.Types;

type // Extra text for the report line, appended as it comes back. Both of these are called from the
     // watchdog thread while the program is stuck, so whatever they do must neither take a lock the
     // frozen part might be holding nor take long: reading a few interlocked variables and putting
     // them into a string is the shape this is meant for.
     TpvHangWatchdogDetailsHook=function:TpvUTF8String;

     // Where a report line goes besides the file, for whoever is watching a console. Called after
     // the file was written, never before, for the reason in the unit comment.
     TpvHangWatchdogLogHook=procedure(const aLine:TpvUTF8String);

var // Whether any of this happens at all. Off means the thread is not even started.
    pvHangWatchdogEnabled:Boolean=true;

    // How long the main loop may go without coming back around before this counts as frozen. Long
    // enough that a heavy load or a stall of several seconds does not produce a report, short
    // enough that somebody notices the freeze and the report at roughly the same time.
    pvHangWatchdogSeconds:TpvDouble=10.0;

    // Reported again every pvHangWatchdogSeconds while it stays frozen, so that a phase which does
    // move on can be told apart from one which does not. After the last of them the process is
    // ended, see below, and without that it is simply the point where the reporting stops rather
    // than filling a disk with a program left frozen over lunch.
    pvHangWatchdogMaxReports:TpvInt32=5;

    // Whether the process is ended once the last report is written. On, because that is the whole
    // point of it - the alternative is the program hanging around forever. Turned off where
    // somebody wants to attach a debugger to the frozen process instead, which is also what
    // TpvApplication does by itself when it is already running under one.
    pvHangWatchdogTerminate:Boolean=true;

    // What the process ends with. Only reaches the caller on Windows; elsewhere this is a signal
    // death and the caller sees that instead, while the written report says the same thing either
    // way.
    pvHangWatchdogExitCode:TpvInt32=3;

    // Where the reports are appended. Empty means none are written to a file and only the log hook
    // below sees them. TpvApplication puts it next to the crash log, since that is where somebody
    // will look for both.
    pvHangWatchdogReportFileName:TpvUTF8String='';

    // What the engine itself knows about the moment - the frame counter and the tag the update
    // thread left behind. Assigned by TpvApplication, so a program on top of it should use the one
    // below instead of overwriting this one.
    pvOnHangWatchdogEngineDetails:TpvHangWatchdogDetailsHook=nil;

    // And what the program on top of it knows: the phase words a game leaves behind on its way
    // through a frame, or whatever else names the place a freeze sits in.
    pvOnHangWatchdogDetails:TpvHangWatchdogDetailsHook=nil;

    pvOnHangWatchdogLog:TpvHangWatchdogLogHook=nil;

// One beat per turn of the main loop. Everything in that turn is allowed to take a while; it only
// has to come back.
procedure pvHangWatchdogHeartbeat;

// Starts respectively stops the watching thread. Called by TpvApplication around its main loop;
// starting twice or stopping something which never ran does nothing.
procedure pvHangWatchdogStart;
procedure pvHangWatchdogStop;

// Whether the beat is expected to keep moving at the moment. On to begin with, since a main loop
// which is running is a main loop which should be coming back around. Switched off around a stretch
// where frames are deliberately not drawn any more - a teardown on the way out of a screen, say.
procedure pvHangWatchdogSetArmed(const aArmed:Boolean);

// And on top of that a pause around everything which is allowed to take minutes without a turn of
// the loop: loading, and above all the shader pipelines compiled inside it, which in the worst case
// really do run that long. Nestable, and independent of the switch above, so that a load started
// from inside a running game is covered no matter in which order the screens change hands.
procedure pvHangWatchdogBeginPause;
procedure pvHangWatchdogEndPause;

implementation

const // How often the thread looks. Also how long a shutdown waits for it to notice that it is
      // over, which is the reason it is not longer.
      PollMilliseconds=250;

      PollSeconds=PollMilliseconds/1000.0;

type TpvHangWatchdogThread=class(TThread)
      protected
       procedure Execute; override;
     end;

var WatchdogThread:TpvHangWatchdogThread=nil;

    // Written by the main loop, read by the watchdog thread. Wrapping is fine and intended: the
    // watchdog only ever asks whether the value differs from the one it saw last time.
    HeartbeatCounter:TPasMPInt32=0;

    ArmedCounter:TPasMPInt32=1;
    PauseCounter:TPasMPInt32=0;

    // Watchdog-only, needs no protection.
    LastSeenHeartbeat:TpvInt32=0;
    HeartbeatEverSeen:Boolean=false;
    StallSeconds:TpvDouble=0.0;
    ReportCount:TpvInt32=0;

procedure pvHangWatchdogHeartbeat;
begin
 TPasMPInterlocked.Increment(HeartbeatCounter);
end;

procedure pvHangWatchdogSetArmed(const aArmed:Boolean);
begin
 TPasMPInterlocked.Write(ArmedCounter,TPasMPInt32(ord(aArmed) and 1));
end;

procedure pvHangWatchdogBeginPause;
begin
 TPasMPInterlocked.Increment(PauseCounter);
end;

procedure pvHangWatchdogEndPause;
begin
 if TPasMPInterlocked.Read(PauseCounter)>0 then begin
  TPasMPInterlocked.Decrement(PauseCounter);
 end;
end;

// Ends the process without running anything on the way out. Not Halt, which walks the finalization
// of every unit, and that is a walk through code which the freeze has already shown is not to be
// trusted - quite possibly through the very place the hang sits in.
procedure TerminateProcessHard(const aExitCode:TpvInt32);
begin
{$if defined(Windows)}
 TerminateProcess(GetCurrentProcess,aExitCode);
{$elseif defined(Unix)}
 FpKill(FpGetpid,SIGKILL);
{$else}
 Halt(aExitCode);
{$ifend}
end;

procedure WriteReportFileLine(const aLine:TpvUTF8String);
var FileName:String;
    FileLine:TpvUTF8String;
    Stream:TFileStream;
begin

 if length(pvHangWatchdogReportFileName)>0 then begin
  FileName:=String(pvHangWatchdogReportFileName);
  try
   if FileExists(FileName) then begin
    Stream:=TFileStream.Create(FileName,fmOpenWrite or fmShareDenyNone);
   end else begin
    Stream:=TFileStream.Create(FileName,fmCreate);
   end;
   try
    Stream.Position:=Stream.Size;
    FileLine:=aLine+#13#10;
    Stream.WriteBuffer(FileLine[1],length(FileLine));
   finally
    FreeAndNil(Stream);
   end;
  except
   // A report which cannot be written is not worth taking anything down for.
  end;
 end;

end;

// And into the ordinary log as well, for whoever is watching a console. Always after the file and
// never before it, and never at all on the way out: a log takes locks, and one of them may be the
// lock the freeze is sitting on, in which case this never comes back - which would cost exactly the
// termination it was written in front of.
procedure WriteReportLogLine(const aLine:TpvUTF8String);
begin
 try
  if assigned(pvOnHangWatchdogLog) then begin
   pvOnHangWatchdogLog(aLine);
  end;
 except
 end;
end;

function BuildHangReportLine(const aReportIndex:TpvInt32;const aStalledSeconds:TpvDouble):TpvUTF8String;
var Line,Details:TpvUTF8String;
begin

 Line:=TpvUTF8String(FormatDateTime('yyyy"-"mm"-"dd" "hh":"nn":"ss',Now))+
       ' hang #'+TpvUTF8String(IntToStr(aReportIndex))+
       ' no frame for '+TpvUTF8String(FormatFloat('0.0',aStalledSeconds))+' s';

 // Both wrapped, since a hook which raises must not cost the report it was supposed to enrich.
 try
  if assigned(pvOnHangWatchdogEngineDetails) then begin
   Details:=pvOnHangWatchdogEngineDetails();
   if length(Details)>0 then begin
    Line:=Line+' '+Details;
   end;
  end;
 except
 end;

 try
  if assigned(pvOnHangWatchdogDetails) then begin
   Details:=pvOnHangWatchdogDetails();
   if length(Details)>0 then begin
    Line:=Line+' '+Details;
   end;
  end;
 except
 end;

 result:=Line;

end;

// Called by the watchdog thread with the time its last sleep took. Says nothing for as long as the
// beat moves, and only starts counting once it has moved at least once, so that whatever happens
// before the first turn of the loop does not count as a freeze.
procedure CheckForHang(const aElapsedSeconds:TpvDouble);
var Beat:TpvInt32;
    Line:TpvUTF8String;
    Last:Boolean;
begin

 if (TPasMPInterlocked.Read(ArmedCounter)=0) or (TPasMPInterlocked.Read(PauseCounter)>0) then begin
  // Nobody owes a turn of the loop right now. Everything is reset rather than merely held, so that
  // switching the watch back on starts from a clean slate instead of from a stall which was never
  // one.
  HeartbeatEverSeen:=false;
  StallSeconds:=0.0;
  ReportCount:=0;
  exit;
 end;

 Beat:=TPasMPInterlocked.Read(HeartbeatCounter);
 if (not HeartbeatEverSeen) or (Beat<>LastSeenHeartbeat) then begin
  HeartbeatEverSeen:=true;
  LastSeenHeartbeat:=Beat;
  StallSeconds:=0.0;
  ReportCount:=0;
 end else begin
  StallSeconds:=StallSeconds+aElapsedSeconds;
  if (StallSeconds>=pvHangWatchdogSeconds) and (ReportCount<pvHangWatchdogMaxReports) then begin
   inc(ReportCount);
   Last:=(ReportCount>=pvHangWatchdogMaxReports) and pvHangWatchdogTerminate;
   Line:=BuildHangReportLine(ReportCount,pvHangWatchdogSeconds*ReportCount);
   // The report is written first, so that the verdict outlives the process, and the line after it
   // says why the process ends rather than leaving a report which simply stops.
   WriteReportFileLine(Line);
   if Last then begin
    WriteReportFileLine(TpvUTF8String(FormatDateTime('yyyy"-"mm"-"dd" "hh":"nn":"ss',Now))+
                        ' ending the process after '+TpvUTF8String(IntToStr(ReportCount))+
                        ' hang reports, exit code '+TpvUTF8String(IntToStr(pvHangWatchdogExitCode)));
    TerminateProcessHard(pvHangWatchdogExitCode);
   end;
   WriteReportLogLine(Line);
   StallSeconds:=0.0;
  end;
 end;

end;

procedure TpvHangWatchdogThread.Execute;
begin
 while not Terminated do begin
  // Polled rather than waited on, since the whole thread costs one wake-up every quarter second and
  // needs neither an operating system object nor a lock at the moment where both are least
  // reliable.
  Sleep(PollMilliseconds);
  if not Terminated then begin
   CheckForHang(PollSeconds);
  end;
 end;
end;

procedure pvHangWatchdogStart;
begin
 if pvHangWatchdogEnabled and not assigned(WatchdogThread) then begin
  HeartbeatEverSeen:=false;
  StallSeconds:=0.0;
  ReportCount:=0;
  WatchdogThread:=TpvHangWatchdogThread.Create(true);
  WatchdogThread.FreeOnTerminate:=false;
  WatchdogThread.Start;
 end;
end;

procedure pvHangWatchdogStop;
begin
 if assigned(WatchdogThread) then begin
  try
   WatchdogThread.Terminate;
   WatchdogThread.WaitFor;
  finally
   FreeAndNil(WatchdogThread);
  end;
 end;
end;

initialization
finalization
 pvHangWatchdogStop;
end.
