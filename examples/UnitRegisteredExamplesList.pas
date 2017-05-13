unit UnitRegisteredExamplesList;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
{$endif}
{$if defined(Win32) or defined(Win64)}
 {$define Windows}
{$ifend}

interface

uses SysUtils,Classes;

var RegisterExamplesList:TStringList=nil;

procedure RegisterExample(const pTitle:string;const pClass:TClass);

implementation

procedure RegisterExample(const pTitle:string;const pClass:TClass);
begin
 RegisterExamplesList.AddObject(pTitle,pointer(pClass));
end;                                                 

initialization
 RegisterExamplesList:=TStringList.Create;
finalization
 FreeAndNil(RegisterExamplesList);
end.
 