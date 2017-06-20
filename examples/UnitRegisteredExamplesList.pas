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

var RegisteredExamplesList:TStringList=nil;

procedure RegisterExample(const aTitle:string;const aClass:TClass);

implementation

procedure RegisterExample(const aTitle:string;const aClass:TClass);
begin
 RegisteredExamplesList.AddObject(aTitle,pointer(aClass));
end;                                                 

initialization
 RegisteredExamplesList:=TStringList.Create;
finalization
 FreeAndNil(RegisteredExamplesList);
end.
 