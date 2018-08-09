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
     UnitGlobals,
     UnitParameters, UnitProject;

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
 WriteLn('Usage: ',ExtractFileName(OwnExecutableFileName),' ([options] ...) [command] ([command parameters])');
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
 writeln;
 writeln('Options: -h / --help / -?           Show this help');
 writeln('         -i / --info                Show informations');
 writeln;
 writeln('Commands: create [projectname]      Create a new project');
 writeln('          build [projectname]       Build an existent project');
//writeln('          delete [projectname]      Delete an existent project');
//writeln('          deploy [projectname]      Deploy an existent project');
 writeln('          run [projectname]         Run an existent project');
end;

begin

ParseCommandLine;

 ShowTitle;

 if DoShowInfos then begin
  ShowInfos;
 end;

 if DoShowHelp then begin
  ShowHelp;
 end else if DoShowUsage and (length(CurrentCommand)=0) then begin
  ShowUsage;
 end;

 if length(CurrentCommand)>0 then begin
  if CurrentCommand='create' then begin
   CreateProject;
  end else if CurrentCommand='build' then begin
   BuildProject;
  end else if CurrentCommand='run' then begin
   RunProject;
  end else begin
   WriteLn('Unknown command: ',CurrentCommand);
  end;
 end;

{$ifdef Windows}
 if {$ifdef fpc}IsDebuggerPresent{$else}DebugHook<>0{$endif} then begin
  writeln('Press return to exit . . . ');
  readln;
 end;
{$endif}
end.



