unit UnitMain;

{$mode delphi}

interface

uses SysUtils,Classes,CRT,PasVulkan.Types,PasVulkan.TextEditor;

procedure Main;

implementation

var ScreenWidth,ScreenHeight:Int32;

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
  CRT.HighVideo;
  CRT.TextBackground(CRT.Black);
  CRT.TextColor(CRT.Mono);
  Write(aFKeyNr);
  CRT.LowVideo;
  CRT.TextBackground(CRT.Cyan);
  CRT.TextColor(CRT.Black);
  Write(aName);
  CRT.TextBackground(CRT.Black);
  CRT.TextColor(CRT.Mono);
  Write(' ');
 end;
begin
 CRT.GotoXY(1,ScreenHeight);
 AddFKey(1,'Help');
 AddFKey(2,'Save');
 AddFKey(3,'Find');
 AddFKey(4,'Replace');
 AddFKey(5,'Load');
 AddFKey(6,'Undo');
 AddFKey(7,'Redo');
 AddFKey(10,'Quit');
 while CRT.WhereX<ScreenWidth do begin
  Write(' ');
 end;
end;

procedure ClearEditScreen;
var Index:Int32;
begin
 CRT.TextBackground(CRT.Blue);
 CRT.TextColor(CRT.Mono);
 for Index:=2 to ScreenHeight-1 do begin
  CRT.GotoXY(1,Index);
  Write(RepChar(#32,ScreenWidth));
 end;
 CRT.TextBackground(CRT.Black);
 CRT.TextColor(CRT.Mono);
end;

procedure ResetScreen;
begin

 ScreenWidth:=(CRT.WindMaxX-CRT.WindMinX)+1;
 ScreenHeight:=(CRT.WindMaxY-CRT.WindMinY)+1;

 CRT.TextBackground(CRT.Blue);
 CRT.TextColor(CRT.Mono);
 CRT.ClrScr;
 CRT.TextBackground(CRT.Black);
 CRT.TextColor(CRT.Mono);

 CRT.CursorOff;

 CRT.LowVideo;
 CRT.GotoXY(1,1);
 Write(RepChar(#205,ScreenWidth));

 CRT.HighVideo;
 CRT.GotoXY(4,1);
 Write(' PasVulkan Test TextEditor ');

 CRT.LowVideo;
 CRT.GotoXY(1,ScreenHeight-1);
 Write(RepChar(#196,ScreenWidth));

 DisplayKeys;

end;

procedure Main;
var c:char;
begin

 ResetScreen;

 ClearEditScreen;

 CRT.GotoXY(1,2);

 CRT.CursorOn;

 repeat

  c:=ReadKey;
  if c<>#0 then begin
  end;

 until false;
end;

end.

