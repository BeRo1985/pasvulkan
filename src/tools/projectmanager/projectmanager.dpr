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

uses SysUtils,
     Classes,
     UnitVersion;

{$if defined(fpc) and defined(Windows)}
function IsDebuggerPresent:longbool; stdcall; external 'kernel32.dll' name 'IsDebuggerPresent';
{$ifend}

procedure ShowTitle;
begin
 WriteLn('PasVulkan project manager version ',ProjectManagerVersion);
 WriteLn(ProjectManagerCopyright);
 WriteLn('Built with ',{$if defined(fpc)}
          'FreePascal compiler ',{$i %FPCVERSION%},' for ',{$i %FPCTARGETCPU%},' on ',{$i %FPCTARGETOS%},' at ',{$i %DATE%}
         {$else}
          'Delphi compiler'
         {$ifend});
end;

begin
 ShowTitle;
{$ifdef Windows}
 if {$ifdef fpc}IsDebuggerPresent{$else}DebugHook<>0{$endif} then begin
  writeln('Press return to exit . . . ');
  readln;
 end;
{$endif}
end.



