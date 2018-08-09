unit UnitParameters;
{$i ..\..\PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

uses SysUtils,Classes,UnitVersion,UnitGlobals;

procedure ParseCommandLine;

implementation

procedure ParseCommandLine;
var Index,Count:Int32;
    Current:UnicodeString;
begin
 Index:=1;
 Count:=ParamCount;
 while Index<=Count do begin
  Current:=UnicodeString(UTF8String(ParamStr(Index)));
  if length(Current)>0 then begin
   if Current[1]='-' then begin
    if (Current='-h') or (Current='--help') or (Current='-?') then begin
     DoShowUsage:=false;
     DoShowHelp:=true;
    end else if (Current='-i') or (Current='--info') then begin
     DoShowUsage:=false;
     DoShowInfos:=true;
    end;
   end else begin

   end;
  end;
  inc(Index);
 end;
end;

initialization
finalization
end.
