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
var Index,Count,NormalParameterCounter:Int32;
    Current:UnicodeString;
begin
 Index:=1;
 Count:=ParamCount;
 NormalParameterCounter:=0;
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
    end else if (Current='--debug') then begin
     BuildMode:=TBuildMode.Debug;
    end else if (Current='--release') then begin
     BuildMode:=TBuildMode.Release;
    end;
   end else begin
    case NormalParameterCounter of
     0:begin
      CurrentCommand:=trim(Current);
     end;
     1:begin
      CurrentProjectName:=trim(Current);
     end;
     2:begin
      if CurrentCommand='build' then begin
       CurrentTarget:=trim(Current);
      end;
     end;
    end;
    inc(NormalParameterCounter);
   end;
  end;
  inc(Index);
 end;
end;

initialization
finalization
end.
