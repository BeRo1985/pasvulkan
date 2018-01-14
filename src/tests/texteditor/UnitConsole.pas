unit UnitConsole;

{$mode delphi}

{$scopedenums on}

interface

uses {$ifdef Windows}Windows,{$endif}SysUtils,Classes,Math,PUCU;

type PConsoleBufferItem=^TConsoleBufferItem;
     TConsoleBufferItem=record
      case boolean of
       false:(
        BackgroundColor:UInt8;
        ForegroundColor:UInt8;
        CodePoint:UInt32;
       );
       true:(
        Value:UInt64;
       );
     end;

     TConsoleBufferItems=array of TConsoleBufferItem;

     TConsole=class
      public
       type TColor=class
             public
              const Black=0;
                    Blue=1;
                    Green=2;
                    Cyan=3;
                    Red=4;
                    Magenta=5;
                    Brown=6;
                    LightGray=7;
                    DarkGray=8;
                    LightBlue=9;
                    LightGreen=10;
                    LightCyan=11;
                    LightRed=12;
                    LightMagenta=13;
                    Yellow=14;
                    White=15;
                    Blink=128;
            end;
            TCursorState=
             (
              Off,
              On,
              Big
             );
      private
       fWidth:Int32;
       fHeight:Int32;
       fDirty:boolean;
       fBuffer:TConsoleBufferItems;
       fLastBuffer:TConsoleBufferItems;
       fCursorX:Int32;
       fCursorY:Int32;
       fLastCursorX:Int32;
       fLastCursorY:Int32;
       fBackgroundColor:UInt8;
       fForegroundColor:UInt8;
       fScrollLock:boolean;
       fCursorState:TCursorState;
       fLastCursorState:TCursorState;
{$ifdef Windows}
       fConsoleHandle:Windows.THANDLE;
       fConsoleBuffer:array of CHAR_INFO;
       fConsoleInfo:TConsoleScreenBufferinfo;
       fCursorInfo:TConsoleCursorInfo;
{$endif}
       procedure SetWidth(const aWidth:Int32);
       procedure SetHeight(const aHeight:Int32);
       procedure UpdateBufferSize;
       procedure CheckCursorXY;
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       procedure ClrScr;
       procedure InsLine;
       procedure DelLine;
       procedure ClrEOL;
       procedure GotoXY(const aX,aY:Int32);
       procedure TextBackground(const aBackgroundColor:UInt8);
       procedure TextColor(const aForegroundColor:UInt8);
       procedure WriteCodePointToBuffer(const aX,aY:Int32;const aCodePoint:UInt32);
       procedure WriteCodePoint(const aCodePoint:UInt32);
       procedure Write(const aString:TPUCUUTF8String);
       procedure WriteLn(const aString:TPUCUUTF8String);
       procedure CursorOff;
       procedure CursorOn;
       procedure CursorBig;
       procedure Flush;
       function KeyPressed:boolean;
       function ReadKey:Char;
      published
       property Width:Int32 read fWidth write SetWidth;
       property Height:Int32 read fHeight write SetHeight;
       property BackgroundColor:UInt8 read fBackgroundColor write fBackgroundColor;
       property ForegroundColor:UInt8 read fForegroundColor write fForegroundColor;
       property WhereX:Int32 read fCursorX;
       property WhereY:Int32 read fCursorY;
       property ScrollLock:boolean read fScrollLock write fScrollLock;
       property CursorState:TCursorState read fCursorState write fCursorState;
     end;

var Console:TConsole=nil;

implementation

//{$ifndef Windows}
uses CRT;
//{$endif}

constructor TConsole.Create;
begin
 inherited Create;
{$ifdef Windows}
 fConsoleHandle:=GetStdHandle(STD_OUTPUT_HANDLE);
 fConsoleBuffer:=nil;
 if GetConsoleScreenBufferInfo(fConsoleHandle,fConsoleInfo) then begin
  fWidth:=(fConsoleInfo.srWindow.Right-fConsoleInfo.srWindow.Left)+1;
  fHeight:=(fConsoleInfo.srWindow.Bottom-fConsoleInfo.srWindow.Top)+1;
 end else begin
  fWidth:=80;
  fHeight:=25;
 end;
 GetConsoleCursorInfo(fConsoleHandle,fCursorInfo);
{$else}
 fWidth:=(CRT.WindMaxX-CRT.WindMinX)+1;
 fHeight:=(CRT.WindMaxY-CRT.WindMinY)+1;
{$endif}
 fBuffer:=nil;
 fLastBuffer:=nil;
 UpdateBufferSize;
 ClrScr;
 fDirty:=false;
 fScrollLock:=true;
 fBackgroundColor:=TColor.Black;
 fForegroundColor:=TColor.LightGray;
 fLastCursorX:=-1;
 fLastCursorY:=-1;
 GotoXY(1,1);
end;

destructor TConsole.Destroy;
begin
 fBuffer:=nil;
 fLastBuffer:=nil;
{$ifdef Windows}
 fConsoleBuffer:=nil;
{$endif}
 inherited Destroy;
end;

procedure TConsole.SetWidth(const aWidth:Int32);
begin
 if fWidth<>aWidth then begin
  fWidth:=aWidth;
  UpdateBufferSize;
  fDirty:=true;
 end;
end;

procedure TConsole.SetHeight(const aHeight:Int32);
begin
 if fHeight<>aHeight then begin
  fHeight:=aHeight;
  UpdateBufferSize;
  fDirty:=true;
 end;
end;

procedure TConsole.UpdateBufferSize;
var x,y:Int32;
    BufferItem:PConsoleBufferItem;
begin
 SetLength(fBuffer,fWidth*fHeight);
 SetLength(fLastBuffer,fWidth*fHeight);
{$ifdef Windows}
 SetLength(fConsoleBuffer,fWidth*fHeight);
{$endif}
 BufferItem:=@fLastBuffer[0];
 for y:=0 to fHeight-1 do begin
  for x:=0 to fWidth-1 do begin
   BufferItem^.BackgroundColor:=$ff;
   BufferItem^.ForegroundColor:=$ff;
   BufferItem^.CodePoint:=$ffffffff;
   inc(BufferItem);
  end;
 end;
end;

procedure TConsole.ClrScr;
var x,y:Int32;
    BufferItem:PConsoleBufferItem;
begin
 BufferItem:=@fBuffer[0];
 for y:=0 to fHeight-1 do begin
  for x:=0 to fWidth-1 do begin
   BufferItem^.BackgroundColor:=fBackgroundColor;
   BufferItem^.ForegroundColor:=fForegroundColor;
   BufferItem^.CodePoint:=32;
   inc(BufferItem);
  end;
 end;
end;

procedure TConsole.GotoXY(const aX,aY:Int32);
begin
 fCursorX:=Min(Max(aX,1),fWidth);
 fCursorY:=Min(Max(aY,1),fHeight);
end;

procedure TConsole.TextBackground(const aBackgroundColor:UInt8);
begin
 fBackgroundColor:=aBackgroundColor;
end;

procedure TConsole.TextColor(const aForegroundColor:UInt8);
begin
 fForegroundColor:=aForegroundColor;
end;

procedure TConsole.InsLine;
var Index:Int32;
begin
 for Index:=1 to Min(Max(fCursorY,0),fHeight)-1 do begin
  Move(fBuffer[Index*fWidth],fBuffer[(Index-1)*fWidth],fWidth*SizeOf(TConsoleBufferItem));
 end;
 ClrEOL;
end;

procedure TConsole.DelLine;
var Index,x,y:Int32;
    BufferItem:PConsoleBufferItem;
begin
 for Index:=Min(Max(fCursorY,0),fHeight)-1 to fHeight-1 do begin
  Move(fBuffer[Index*fWidth],fBuffer[(Index-1)*fWidth],fWidth*SizeOf(TConsoleBufferItem));
 end;
 y:=fHeight-1;
 BufferItem:=@fBuffer[y*fWidth];
 for x:=0 to fWidth-1 do begin
  BufferItem^.BackgroundColor:=fBackgroundColor;
  BufferItem^.ForegroundColor:=fForegroundColor;
  BufferItem^.CodePoint:=32;
  inc(BufferItem);
 end;
end;

procedure TConsole.ClrEOL;
var x,y:Int32;
    BufferItem:PConsoleBufferItem;
begin
 if ((fCursorX>0) and (fCursorX<=fWidth)) and
    ((fCursorY>0) and (fCursorY<=fHeight)) then begin
  y:=fCursorY-1;
  BufferItem:=@fBuffer[y*fWidth];
  for x:=0 to fWidth-1 do begin
   BufferItem^.BackgroundColor:=fBackgroundColor;
   BufferItem^.ForegroundColor:=fForegroundColor;
   BufferItem^.CodePoint:=32;
   inc(BufferItem);
  end;
 end;
end;

procedure TConsole.CheckCursorXY;
begin
 if fCursorX>fWidth then begin
  fCursorX:=1;
  inc(fCursorY);
 end;
 if fCursorY>fHeight then begin
  if fScrollLock then begin
   fCursorX:=1;
   fCursorY:=1;
  end else begin
   if fCursorY>fHeight then begin
    fCursorY:=fHeight;
   end;
   InsLine;
   fCursorX:=1;
   fCursorY:=fHeight;
  end;
 end;
end;

procedure TConsole.WriteCodePointToBuffer(const aX,aY:Int32;const aCodePoint:UInt32);
var BufferItem:PConsoleBufferItem;
begin
 if ((aX>0) and (aX<=fWidth)) and
    ((aY>0) and (aY<=fHeight)) then begin
  BufferItem:=@fBuffer[((aY-1)*fWidth)+(aX-1)];
  BufferItem^.BackgroundColor:=fBackgroundColor;
  BufferItem^.ForegroundColor:=fForegroundColor;
  BufferItem^.CodePoint:=aCodePoint;
 end;
end;

procedure TConsole.WriteCodePoint(const aCodePoint:UInt32);
var Index:Int32;
    BufferItem:PConsoleBufferItem;
begin
 case aCodePoint of
  0:begin
  end;
  7:begin
   //inc(PiepCount,11025);
  end;
  8:begin
   fCursorX:=Max(1,fCursorX-1);
  end;
  9:begin
   for Index:=1 to (((fCursorX+8) and not 7)-fCursorX)+1 do begin
    if ((fCursorX>0) and (fCursorX<=fWidth)) and
       ((fCursorY>0) and (fCursorY<=fHeight)) then begin
     BufferItem:=@fBuffer[((fCursorY-1)*fWidth)+(fCursorX-1)];
     BufferItem^.BackgroundColor:=fBackgroundColor;
     BufferItem^.ForegroundColor:=fForegroundColor;
     BufferItem^.CodePoint:=32;
    end;
    inc(fCursorX);
    CheckCursorXY;
   end;
  end;
  10:begin
   inc(fCursorY);
   CheckCursorXY;
  end;
  12:begin
   ClrScr;
  end;
  13:begin
   fCursorX:=1;
  end;
  255:begin
  end;
  else begin
   if ((fCursorX>0) and (fCursorX<=fWidth)) and
      ((fCursorY>0) and (fCursorY<=fHeight)) then begin
    BufferItem:=@fBuffer[((fCursorY-1)*fWidth)+(fCursorX-1)];
    BufferItem^.BackgroundColor:=fBackgroundColor;
    BufferItem^.ForegroundColor:=fForegroundColor;
    BufferItem^.CodePoint:=aCodePoint;
   end;
   inc(fCursorX);
   CheckCursorXY;
  end;
 end;
end;

procedure TConsole.Write(const aString:TPUCUUTF8String);
var Index,Len:Int32;
begin
 Index:=1;
 Len:=Length(aString);
 while Index<=Len do begin
  WriteCodePoint(PUCUUTF8CodeUnitGetCharAndIncFallback(aString,Index));
 end;
end;

procedure TConsole.WriteLn(const aString:TPUCUUTF8String);
begin
 Write(aString+#13#10);
end;

procedure TConsole.CursorOff;
begin
 fCursorState:=TConsole.TCursorState.Off;
end;

procedure TConsole.CursorOn;
begin
 fCursorState:=TConsole.TCursorState.On;
end;

procedure TConsole.CursorBig;
begin
 fCursorState:=TConsole.TCursorState.Big;
end;

procedure TConsole.Flush;
{$ifdef Windows}
var x,y:Int32;
    BufferItem,LastBufferItem:PConsoleBufferItem;
    CharBufSize,CharacterPos,CurInfo:Windows.TCOORD;
    WriteArea:Windows.TSMALL_RECT;
    p:PCHAR_INFO;
    HasChanges:boolean;
    CursorInfo:TConsoleCursorInfo;
begin
 HasChanges:=(fLastCursorX<>fCursorX) or (fLastCursorY<>fCursorY) or (fLastCursorState<>fCursorState);
 fLastCursorX:=fCursorX;
 fLastCursorY:=fCursorY;
 fLastCursorState:=fCursorState;
 BufferItem:=@fBuffer[0];
 LastBufferItem:=@fLastBuffer[0];
 p:=@fConsoleBuffer[0];
 for y:=1 to fHeight do begin
  for x:=1 to fWidth do begin
   if LastBufferItem^.Value<>BufferItem^.Value then begin
    LastBufferItem^.Value:=BufferItem^.Value;
    HasChanges:=true;
   end;
   p^.Attributes:=(BufferItem^.BackgroundColor shl 4) or BufferItem.ForegroundColor;
   p^.UnicodeChar:=WideChar(Word(BufferItem^.CodePoint));
   inc(LastBufferItem);
   inc(BufferItem);
   inc(p);
  end;
 end;
 if HasChanges then begin
  CharBufSize.x:=fWidth;
  CharBufSize.y:=fHeight;
  CharacterPos.x:=0;
  CharacterPos.y:=0;
  WriteArea.Left:=0;
  WriteArea.Top:=0;
  WriteArea.Right:=fWidth-1;
  WriteArea.Bottom:=fHeight-1;
  WriteConsoleOutputW(fConsoleHandle,@fConsoleBuffer[0],CharBufSize,CharacterPos,WriteArea);
  GetConsoleCursorInfo(fConsoleHandle,CursorInfo);
  case fCursorState of
   TConsole.TCursorState.Off:begin
    CursorInfo.bVisible:=false;
   end;
   TConsole.TCursorState.On:begin
    CursorInfo.dwSize:=fCursorInfo.dwSize;
    CursorInfo.bVisible:=true;
   end;
   TConsole.TCursorState.Big:begin
    CursorInfo.dwSize:=93;
    CursorInfo.bVisible:=true;
   end;
  end;
  CurInfo.X:=fCursorX-1;
  CurInfo.Y:=fCursorY-1;
  SetConsoleCursorPosition(fConsoleHandle,CurInfo);
  SetConsoleCursorInfo(fConsoleHandle,CursorInfo);
 end;
end;
{$else}
var x,y,LastBackgroundColor,LastForegroundColor,LastX,LastY:Int32;
    BufferItem,LastBufferItem:PConsoleBufferItem;
    HasChanges:boolean;
    StringBuffer:string;
 procedure FlushProcessedBuffer;
 begin
  if length(StringBuffer)>0 then begin
   CRT.GotoXY(LastX,LastY);
   System.Write(StringBuffer);
   StringBuffer:='';
  end;
 end;
begin
 HasChanges:=(fLastCursorX<>fCursorX) or (fLastCursorY<>fCursorY) or (fLastCursorState<>fCursorState);
 fLastCursorX:=fCursorX;
 fLastCursorY:=fCursorY;
 fLastCursorState:=fCursorState;
 BufferItem:=@fBuffer[0];
 LastBufferItem:=@fLastBuffer[0];
 for y:=1 to fHeight do begin
  for x:=1 to fWidth do begin
   if (x=fWidth) and (y=fHeight) then begin
    break;
   end;
   if LastBufferItem^.Value<>BufferItem^.Value then begin
    HasChanges:=true;
   end;
  end;
 end;
 if not HasChanges then begin
  exit;
 end;
 BufferItem:=@fBuffer[0];
 LastBufferItem:=@fLastBuffer[0];
 LastBackgroundColor:=-1;
 LastForegroundColor:=-1;
 LastX:=1;
 LastY:=1;
 CRT.cursoroff;
 BufferItem:=@fBuffer[0];
 LastBufferItem:=@fLastBuffer[0];
 StringBuffer:='';
 for y:=1 to fHeight do begin
  for x:=1 to fWidth do begin
   if (x=fWidth) and (y=fHeight) then begin
    break;
   end;
   if LastBufferItem^.Value<>BufferItem^.Value then begin
    LastBufferItem^.Value:=BufferItem^.Value;
    if LastBackgroundColor<>BufferItem^.BackgroundColor then begin
     FlushProcessedBuffer;
     LastBackgroundColor:=BufferItem^.BackgroundColor;
     CRT.TextBackground(LastBackgroundColor);
    end;
    if LastForegroundColor<>BufferItem^.ForegroundColor then begin
     FlushProcessedBuffer;
     LastForegroundColor:=BufferItem^.ForegroundColor;
     CRT.TextColor(LastForegroundColor);
    end;
    if length(StringBuffer)=0 then begin
     LastX:=x;
     LastY:=y;
    end;
    if BufferItem^.CodePoint<128 then begin
     StringBuffer:=StringBuffer+Chr(BufferItem^.CodePoint);
    end else begin
     StringBuffer:=StringBuffer+PUCUUTF32CharToUTF8(BufferItem^.CodePoint);
    end;
   end else begin
    FlushProcessedBuffer;
   end;
   inc(BufferItem);
   inc(LastBufferItem);
  end;
 end;
 FlushProcessedBuffer;
 CRT.GotoXY(fCursorX,fCursorY);
 case fCursorState of
  TConsole.TCursorState.Off:begin
   CRT.cursoroff;
  end;
  TConsole.TCursorState.On:begin
   CRT.cursoron;
  end;
  TConsole.TCursorState.Big:begin
   CRT.cursorbig;
  end;
 end;
end;
{$endif}

function TConsole.KeyPressed:boolean;
begin
 result:=CRT.KeyPressed;
end;

function TConsole.ReadKey:Char;
begin
 result:=CRT.ReadKey;
end;

initialization
 Console:=TConsole.Create;
finalization
 Console.Free;
end.

