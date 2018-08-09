program projectmanager;
{$i ..\..\PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$if defined(Win32) or defined(Win64) or defined(Windows)}
 {$define Windows}
 {$apptype console}
{$ifend}

// {$if defined(fpc) and defined(Unix)}cthreads,{$ifend}

uses {$if defined(fpc) and defined(Unix)}cthreads,{$ifend}
     SysUtils,
     Classes,
     UnitVersion,
     UnitGlobals, UnitParameters;

{$if defined(fpc) and defined(Windows)}
function IsDebuggerPresent:longbool; stdcall; external 'kernel32.dll' name 'IsDebuggerPresent';
{$ifend}

procedure ShowTitle;
begin
 WriteLn('PasVulkan project manager version ',ProjectManagerVersion);
 WriteLn(ProjectManagerCopyright);
end;

procedure ShowUsage;
begin
 WriteLn('Usage: ',ExtractFileName(OwnExecutableFileName),' ([options] ...) ([project])');
end;

procedure ShowInfos;
begin
 WriteLn('Built with ',{$if defined(fpc)}
          'FreePascal compiler ',{$i %FPCVERSION%},' for ',{$i %FPCTARGETCPU%},' on ',{$i %FPCTARGETOS%},' at ',{$i %DATE%}
         {$else}
          'Delphi compiler'
         {$ifend});
 WriteLn('PasVulkan root path: ',PasVulkanRootPath);
end;

procedure ShowHelp;
begin
 ShowUsage;
 writeln('Options: -h / --help / -?           Show this help');
 writeln('         -i / --info                Show informations');
end;

begin
 ParseCommandLine;
 ShowTitle;
 if DoShowInfos then begin
  ShowInfos;
 end;
 if DoShowHelp then begin
  ShowHelp;
 end else if DoShowUsage then begin
  ShowUsage;
 end;
{$ifdef Windows}
 if {$ifdef fpc}IsDebuggerPresent{$else}DebugHook<>0{$endif} then begin
  writeln('Press return to exit . . . ');
  readln;
 end;
{$endif}
end.



