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
     UnitParameters, UnitProject, UnitExternalProcess;

{$if defined(fpc) and defined(Windows)}
function IsDebuggerPresent:longbool; stdcall; external 'kernel32.dll' name 'IsDebuggerPresent';
{$ifend}

procedure ShowTitle;
begin
 Writeln('PasVulkan project manager version ',ProjectManagerVersion);
 Writeln(ProjectManagerCopyright);
end;

procedure ShowUsage;
begin
 Writeln('Usage: ',ExtractFileName(OwnExecutableFileName),' ([options] ...) [command] ([command parameters])');
end;

procedure ShowInfos;
begin
 Writeln('Built with ',{$if defined(fpc)}
          'FreePascal compiler ',{$i %FPCVERSION%},' for ',{$i %FPCTARGETCPU%},' on ',{$i %FPCTARGETOS%},' at ',{$i %DATE%}
         {$else}
          'Delphi compiler'
         {$ifend});
 Writeln('PasVulkan root path: ',PasVulkanRootPath);
end;

procedure ShowHelp;
begin
 Writeln;
 ShowUsage;
 Writeln;
 Writeln('Options: -h / --help / -?                      Show this help');
 Writeln('         -i / --info                           Show informations');
 Writeln;
 Writeln('Commands: create [projectname]                 Create a new project (project name must be a valid lowercase pascal and java identifier)');
 Writeln('          build [projectname] ([target(s)])    Build an existent project');
 Writeln('          run [projectname]                    Run an existent project');
 Writeln('          update [projectname]                 Update the project base files of an existent project');
 Writeln;
 Writeln('Supported targets: allcpu-android');
 Writeln('                   x86_32-linux');
 Writeln('                   x86_64-linux');
 Writeln('                   x86_32-windows');
 Writeln('                   x86_64-windows');
 Writeln;
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
  end else if CurrentCommand='update' then begin
   UpdateProject;
  end else if CurrentCommand='build' then begin
   BuildProject;
  end else if CurrentCommand='run' then begin
   RunProject;
  end else begin
   Writeln('Unknown command: ',CurrentCommand);
  end;
 end;

{$ifdef Windows}
 if {$ifdef fpc}IsDebuggerPresent{$else}DebugHook<>0{$endif} then begin
  Writeln('Press return to exit . . . ');
  ReadLn;
 end;
{$endif}
end.



