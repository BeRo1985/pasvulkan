unit UnitMain;

{$mode delphi}

interface

uses SysUtils,Classes,PasVulkan.Types,PasVulkan.TextEditor,UnitConsole;

procedure Main;

implementation

function RepChar(const aChar:Char;const aCount:Int32):string;
var Index:Int32;
begin
 SetLength(result,aCount);
 for Index:=1 to aCount do begin
  result[Index]:=aChar;
 end;
end;

procedure DisplayKeys;
 procedure AddFKey(const aFKeyNr:byte;const aName:string);
 begin
  Console.TextBackground(TConsole.TColor.Black);
  Console.TextColor(TConsole.TColor.LightGray);
  Console.Write(IntToStr(aFKeyNr));
  Console.TextBackground(TConsole.TColor.Cyan);
  Console.TextColor(TConsole.TColor.Black);
  Console.Write(aName);
  Console.TextBackground(TConsole.TColor.Black);
  Console.TextColor(TConsole.TColor.LightGray);
  Console.Write(' ');
 end;
begin
 Console.GotoXY(1,Console.Height);
 AddFKey(1,'Help');
 AddFKey(2,'Save');
 AddFKey(3,'Find');
 AddFKey(4,'Replace');
 AddFKey(5,'Load');
 AddFKey(6,'Undo');
 AddFKey(7,'Redo');
 AddFKey(10,'Quit');
 while Console.WhereX<Console.Width do begin
  Console.Write(' ');
 end;
end;

procedure ClearEditScreen;
var Index:Int32;
begin
 Console.TextBackground(TConsole.TColor.Blue);
 Console.TextColor(TConsole.TColor.LightGray);
 for Index:=2 to Console.Height-1 do begin
  Console.GotoXY(1,Index);
  Console.Write(RepChar(#32,Console.Width));
 end;
 Console.TextBackground(TConsole.TColor.Black);
 Console.TextColor(TConsole.TColor.LightGray);
end;

procedure ResetScreen;
begin

 Console.TextBackground(TConsole.TColor.Blue);
 Console.TextColor(TConsole.TColor.LightGray);
 Console.ClrScr;
 Console.TextBackground(TConsole.TColor.Black);
 Console.TextColor(TConsole.TColor.LightGray);

 Console.CursorOff;

 Console.TextBackground(TConsole.TColor.Cyan);
 Console.TextColor(TConsole.TColor.Black);
 Console.GotoXY(1,1);
 Console.Write(RepChar(#32,Console.Width));

 Console.GotoXY(2,1);
 Console.Write('PasVulkan Test Text Editor');

 DisplayKeys;

 Console.TextBackground(TConsole.TColor.Black);
 Console.TextColor(TConsole.TColor.LightGray);

end;

procedure Main;
var c:char;
begin

 ResetScreen;

 //ClearEditScreen;

 Console.GotoXY(1,2);

 Console.CursorOn;

 Console.Flush;

{ repeat

//  c:=ReadKey;
  if c<>#0 then begin
  end;

 until false;}
 readln;
end;

end.

