unit UnitGlobals;
{$i ..\..\PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

uses SysUtils,Classes,UnitVersion;

var OwnExecutableFileName:UnicodeString='';
    PasVulkanRootPath:UnicodeString='';
    PasVulkanProjectsPath:UnicodeString='';
    PasVulkanProjectTemplatePath:UnicodeString='';

    DoShowUsage:boolean=true;
    DoShowHelp:boolean=false;
    DoShowInfos:boolean=false;

    CurrentCommand:UnicodeString='';
    CurrentProjectName:UnicodeString='';
    CurrentTarget:UnicodeString='';

implementation

procedure InitializeGlobals;
begin
 OwnExecutableFileName:=UnicodeString(UTF8String(ParamStr(0)));
 PasVulkanRootPath:=IncludeTrailingPathDelimiter(ExtractFilePath(OwnExecutableFileName));
 PasVulkanProjectsPath:=IncludeTrailingPathDelimiter(PasVulkanRootPath+'projects');
 PasVulkanProjectTemplatePath:=IncludeTrailingPathDelimiter(PasVulkanProjectsPath+'template');

{$if (defined(Win32) or defined(Win64) or defined(Windows)) and defined(cpu386)}
 CurrentTarget:='x86_32-windows';
{$elseif (defined(Win32) or defined(Win64) or defined(Windows)) and (defined(cpuamd64) or defined(cpux64))}
 CurrentTarget:='x86_64-windows';
{$elseif defined(Linux) and defined(cpu386)}
 CurrentTarget:='x86_32-linux';
{$elseif defined(Linux) and (defined(cpuamd64) or defined(cpux64))}
 CurrentTarget:='x86_64-linux';
{$elseif defined(Android)}
 CurrentTarget:='allcpu-android';
{$else}
 CurrentTarget:='';
{$ifend}

end;

procedure FinalizeGlobals;
begin
end;

initialization
 InitializeGlobals;
finalization
 FinalizeGlobals;
end.

