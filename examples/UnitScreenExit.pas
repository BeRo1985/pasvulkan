unit UnitScreenExit;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$if defined(Win32) or defined(Win64)}
 {$define Windows}
{$ifend}

interface

uses SysUtils,
     Classes,
     UnitRegisteredExamplesList,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     UnitScreenBlank;

type TScreenExit=class(TScreenBlank)
      private
       fReady:boolean;
       fSelectedIndex:TpvInt32;
       fStartY:TpvFloat;
      public

       constructor Create; override;

       destructor Destroy; override;

       function KeyDown(const aKeyCode,aKeyModifier:TpvInt32):boolean; override;

       function KeyUp(const aKeyCode,aKeyModifier:TpvInt32):boolean; override;

       function KeyTyped(const aKeyCode,aKeyModifier:TpvInt32):boolean; override;

       function PointerDown(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean; override;

       function PointerUp(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean; override;

       function PointerMotion(const aPosition,aRelativePosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean; override;

       function Scrolled(const aAmount:TpvFloat):boolean; override;

       function CanBeParallelProcessed:boolean; override;

       procedure Update(const aDeltaTime:TpvDouble); override;

     end;

implementation

uses UnitExampleApplication,UnitTextOverlay,UnitScreenMainMenu;

const FontSize=3.0;

constructor TScreenExit.Create;
begin
 inherited Create;
 fSelectedIndex:=-1;
 fReady:=false;
end;

destructor TScreenExit.Destroy;
begin
 inherited Destroy;
end;

function TScreenExit.KeyDown(const aKeyCode,aKeyModifier:TpvInt32):boolean;
begin
 result:=false;
 if fReady then begin
  case aKeyCode of
   KEYCODE_AC_BACK,KEYCODE_ESCAPE:begin
    pvApplication.NextScreen:=TScreenMainMenu.Create;
   end;
   KEYCODE_UP:begin
    if fSelectedIndex<=0 then begin
     fSelectedIndex:=1;
    end else begin
     dec(fSelectedIndex);
    end;
   end;
   KEYCODE_DOWN:begin
    if fSelectedIndex>=1 then begin
     fSelectedIndex:=0;
    end else begin
     inc(fSelectedIndex);
    end;
   end;
   KEYCODE_PAGEUP:begin
    if fSelectedIndex<0 then begin
     fSelectedIndex:=1;
    end;
    dec(fSelectedIndex,5);
    while fSelectedIndex<0 do begin
     inc(fSelectedIndex,2);
    end;
   end;
   KEYCODE_PAGEDOWN:begin
    if fSelectedIndex<0 then begin
     fSelectedIndex:=0;
    end;
    inc(fSelectedIndex,5);
    while fSelectedIndex>=2 do begin
     dec(fSelectedIndex,2);
    end;
   end;
   KEYCODE_HOME:begin
    fSelectedIndex:=0;
   end;
   KEYCODE_END:begin
    fSelectedIndex:=1;
   end;
   KEYCODE_RETURN,KEYCODE_SPACE:begin
    if fSelectedIndex=0 then begin
     pvApplication.Terminate;
    end else begin
     pvApplication.NextScreen:=TScreenMainMenu.Create;
    end;
   end;
  end;
 end;
end;

function TScreenExit.KeyUp(const aKeyCode,aKeyModifier:TpvInt32):boolean;
begin
 result:=false;
end;

function TScreenExit.KeyTyped(const aKeyCode,aKeyModifier:TpvInt32):boolean;
begin
 result:=false;
end;

function TScreenExit.PointerDown(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean;
var Index:TpvInt32;
    cy:TpvFloat;
begin
 result:=false;
 if fReady then begin
  fSelectedIndex:=-1;
  cy:=fStartY;
  for Index:=0 to 1 do begin
   if (aPosition.y>=cy) and (aPosition.y<(cy+(ExampleApplication.TextOverlay.FontCharHeight*FontSize))) then begin
    fSelectedIndex:=Index;
    if fSelectedIndex=0 then begin
     pvApplication.Terminate;
    end else begin
     pvApplication.NextScreen:=TScreenMainMenu.Create;
    end;
   end;
   cy:=cy+((ExampleApplication.TextOverlay.FontCharHeight+4)*FontSize);
  end;
 end;
end;

function TScreenExit.PointerUp(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean;
begin
 result:=false;
end;

function TScreenExit.PointerMotion(const aPosition,aRelativePosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean;
var Index:TpvInt32;
    cy:TpvFloat;
begin
 result:=false;
 if fReady then begin
  fSelectedIndex:=-1;
  cy:=fStartY;
  for Index:=0 to 1 do begin
   if (aPosition.y>=cy) and (aPosition.y<(cy+(ExampleApplication.TextOverlay.FontCharHeight*FontSize))) then begin
    fSelectedIndex:=Index;
   end;
   cy:=cy+((ExampleApplication.TextOverlay.FontCharHeight+4)*FontSize);
  end;
 end;
end;

function TScreenExit.Scrolled(const aAmount:TpvFloat):boolean;
begin
 result:=false;
end;

function TScreenExit.CanBeParallelProcessed:boolean;
begin
 result:=true;
end;

procedure TScreenExit.Update(const aDeltaTime:TpvDouble);
const BoolToInt:array[boolean] of TpvInt32=(0,1);
      Options:array[0..1] of string=('Yes','No');
var Index:TpvInt32;
    cy:TpvFloat;
    s:string;
    IsSelected:boolean;
begin
 inherited Update(aDeltaTime);
 ExampleApplication.TextOverlay.AddText(pvApplication.Width*0.5,ExampleApplication.TextOverlay.FontCharHeight*1.0,2.0,toaCenter,'Are you sure to exit?');
 fStartY:=(pvApplication.Height-((((ExampleApplication.TextOverlay.FontCharHeight+4)*FontSize)*2)-(4*FontSize)))*0.5;
 cy:=fStartY;
 for Index:=0 to 1 do begin
  IsSelected:=fSelectedIndex=Index;
  s:=' '+Options[Index]+' ';
  if IsSelected then begin
   s:='>'+s+'<';
  end;
  ExampleApplication.TextOverlay.AddText(pvApplication.Width*0.5,cy,FontSize,toaCenter,TpvRawByteString(s),MenuColors[IsSelected,0,0],MenuColors[IsSelected,0,1],MenuColors[IsSelected,0,2],MenuColors[IsSelected,0,3],MenuColors[IsSelected,1,0],MenuColors[IsSelected,1,1],MenuColors[IsSelected,1,2],MenuColors[IsSelected,1,3]);
  cy:=cy+((ExampleApplication.TextOverlay.FontCharHeight+4)*FontSize);
 end;
 fReady:=true;
end;

end.
