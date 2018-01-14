unit UnitConsole;

{$mode delphi}

{$scopedenums on}

interface

uses {$ifdef Windows}Windows,{$endif}SysUtils,Classes,Math,PUCU,CRT;

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
       fBackgroundColor:UInt8;
       fForegroundColor:UInt8;
       fScrollLock:boolean;
       fCursorState:TCursorState;
{$ifdef Windows}
       fConsoleHandle:Windows.THANDLE;
       fConsoleBuffer:array of CHAR_INFO;
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

constructor TConsole.Create;
begin
 inherited Create;
{$ifdef Windows}
 fConsoleHandle:=GetStdHandle(STD_OUTPUT_HANDLE);
 fConsoleBuffer:=nil;
{$endif}
 fBuffer:=nil;
 fLastBuffer:=nil;
 fWidth:=(CRT.WindMaxX-CRT.WindMinX)+1;
 fHeight:=(CRT.WindMaxY-CRT.WindMinY)+1;
 UpdateBufferSize;
 ClrScr;
 fDirty:=false;
 fScrollLock:=true;
 fBackgroundColor:=TColor.Black;
 fForegroundColor:=TColor.LightGray;
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
    BufferItem:PConsoleBufferItem;
    CharBufSize,CharacterPos:Windows.TCOORD;
    WriteArea:Windows.TSMALL_RECT;
    p:PCHAR_INFO;
begin
 CRT.cursoroff;
 BufferItem:=@fBuffer[0];
 p:=@fConsoleBuffer[0];
 for y:=1 to fHeight do begin
  for x:=1 to fWidth do begin
   p^.Attributes:=(BufferItem^.BackgroundColor shl 4) or BufferItem.ForegroundColor;
   p^.UnicodeChar:=WideChar(Word(BufferItem^.CodePoint));
   inc(BufferItem);
   inc(p);
  end;
 end;
 CharBufSize.x:=fWidth;
 CharBufSize.y:=fHeight;
 CharacterPos.x:=0;
 CharacterPos.y:=0;
 WriteArea.Left:=0;
 WriteArea.Top:=0;
 WriteArea.Right:=fWidth-1;
 WriteArea.Bottom:=fHeight-1;
 WriteConsoleOutputW(fConsoleHandle,@fConsoleBuffer[0],CharBufSize,CharacterPos,WriteArea);
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
{$else}
var i,x,y,LastBackgroundColor,LastForegroundColor:Int32;
    BufferItem,LastBufferItem:PConsoleBufferItem;
begin
 LastBackgroundColor:=-1;
 LastForegroundColor:=-1;
 CRT.cursoroff;
 BufferItem:=@fBuffer[0];
 LastBufferItem:=@fLastBuffer[0];
 for y:=1 to fHeight do begin
  for x:=1 to fWidth do begin
   if (x=fWidth) and (y=fHeight) then begin
    break;
   end;
   if LastBufferItem^.Value<>BufferItem^.Value then begin
    LastBufferItem^.Value:=BufferItem^.Value;
    if (CRT.WhereX<>x) or (CRT.WhereY<>Y) then begin
     CRT.GotoXY(x,y);
    end;
    if LastBackgroundColor<>BufferItem^.BackgroundColor then begin
     LastBackgroundColor:=BufferItem^.BackgroundColor;
     CRT.TextBackground(BufferItem^.BackgroundColor);
    end;
    if LastForegroundColor<>BufferItem^.ForegroundColor then begin
     LastForegroundColor:=BufferItem^.ForegroundColor;
     CRT.TextColor(BufferItem^.ForegroundColor);
    end;
    if BufferItem^.CodePoint<128 then begin
     System.Write(Chr(BufferItem^.CodePoint));
    end else begin
     System.Write(PUCUUTF32CharToUTF8(BufferItem^.CodePoint));
    end;
   end;
   inc(BufferItem);
   inc(LastBufferItem);
  end;
 end;
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

initialization
 Console:=TConsole.Create;
finalization
 Console.Free;
end.

