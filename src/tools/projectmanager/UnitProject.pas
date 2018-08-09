unit UnitProject;
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

procedure CreateProject;
procedure BuildProject;
procedure RunProject;

implementation

procedure CreateProject;
begin
end;

procedure BuildProject;
begin
end;

procedure RunProject;
begin
end;

end.

