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

    DoShowUsage:boolean=true;
    DoShowHelp:boolean=false;
    DoShowInfos:boolean=false;

    CurrentCommand:UnicodeString='';
    CurrentProjectName:UnicodeString='';

implementation

procedure InitializeGlobals;
begin
 OwnExecutableFileName:=UnicodeString(UTF8String(ParamStr(0)));
 PasVulkanRootPath:=IncludeTrailingPathDelimiter(ExtractFilePath(OwnExecutableFileName));
end;

procedure FinalizeGlobals;
begin
end;

initialization
 InitializeGlobals;
finalization
 FinalizeGlobals;
end.

