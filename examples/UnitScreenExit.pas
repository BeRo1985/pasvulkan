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

uses SysUtils,Classes,Vulkan,PasVulkan,PasVulkanAndroid,PasVulkanApplication,UnitScreenBlank;

type TScreenExit=class(TScreenBlank)
      private
       fReady:boolean;
       fSelectedIndex:TVkInt32;
       fStartY:single;
      public

       constructor Create; override;

       destructor Destroy; override;

       function KeyDown(const aKeyCode,aKeyModifier:TVkInt32):boolean; override;

       function KeyUp(const aKeyCode,aKeyModifier:TVkInt32):boolean; override;

       function KeyTyped(const aKeyCode,aKeyModifier:TVkInt32):boolean; override;

       function TouchDown(const aScreenX,aScreenY,aPressure:single;const aPointerID,aButton:TVkInt32):boolean; override;

       function TouchUp(const aScreenX,aScreenY,aPressure:single;const aPointerID,aButton:TVkInt32):boolean; override;

       function TouchDragged(const aScreenX,aScreenY,aPressure:single;const aPointerID:TVkInt32):boolean; override;

       function MouseMoved(const aScreenX,aScreenY:TVkInt32):boolean; override;

       function Scrolled(const aAmount:TVkInt32):boolean; override;

       procedure Update(const aDeltaTime:double); override;

     end;

implementation

uses UnitExampleVulkanApplication,UnitTextOverlay,UnitScreenMainMenu;

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

function TScreenExit.KeyDown(const aKeyCode,aKeyModifier:TVkInt32):boolean;
begin
 result:=false;
 if fReady then begin
  case aKeyCode of
   KEYCODE_AC_BACK,KEYCODE_ESCAPE:begin
    VulkanApplication.NextScreen:=TScreenMainMenu.Create;
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
     VulkanApplication.Terminate;
    end else begin
     VulkanApplication.NextScreen:=TScreenMainMenu.Create;
    end;
   end;
  end;
 end;
end;

function TScreenExit.KeyUp(const aKeyCode,aKeyModifier:TVkInt32):boolean;
begin
 result:=false;
end;

function TScreenExit.KeyTyped(const aKeyCode,aKeyModifier:TVkInt32):boolean;
begin
 result:=false;
end;

function TScreenExit.TouchDown(const aScreenX,aScreenY,aPressure:single;const aPointerID,aButton:TVkInt32):boolean;
var Index:TVkInt32;
    cy:single;
begin
 result:=false;
 if fReady then begin
  fSelectedIndex:=-1;
  cy:=fStartY;
  for Index:=0 to 1 do begin
   if (aScreenY>=cy) and (aScreenY<=(cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*FontSize))) then begin
    fSelectedIndex:=Index;
    if fSelectedIndex=0 then begin
     VulkanApplication.Terminate;
    end else begin
     VulkanApplication.NextScreen:=TScreenMainMenu.Create;
    end;
   end;
   cy:=cy+((ExampleVulkanApplication.TextOverlay.FontCharHeight+4)*FontSize);
  end;
 end;
end;

function TScreenExit.TouchUp(const aScreenX,aScreenY,aPressure:single;const aPointerID,aButton:TVkInt32):boolean;
begin
 result:=false;
end;

function TScreenExit.TouchDragged(const aScreenX,aScreenY,aPressure:single;const aPointerID:TVkInt32):boolean;
var Index:TVkInt32;
    cy:single;
begin
 result:=false;
 if fReady then begin
  fSelectedIndex:=-1;
  cy:=fStartY;
  for Index:=0 to 1 do begin
   if (aScreenY>=cy) and (aScreenY<=(cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*FontSize))) then begin
    fSelectedIndex:=Index;
   end;
   cy:=cy+((ExampleVulkanApplication.TextOverlay.FontCharHeight+4)*FontSize);
  end;
 end;
end;

function TScreenExit.MouseMoved(const aScreenX,aScreenY:TVkInt32):boolean;
var Index:TVkInt32;
    cy:single;
begin
 result:=false;
 if fReady then begin
  fSelectedIndex:=-1;
  cy:=fStartY;
  for Index:=0 to 1 do begin
   if (aScreenY>=cy) and (aScreenY<=(cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*FontSize))) then begin
    fSelectedIndex:=Index;
   end;
   cy:=cy+((ExampleVulkanApplication.TextOverlay.FontCharHeight+4)*FontSize);
  end;
 end;
end;

function TScreenExit.Scrolled(const aAmount:TVkInt32):boolean;
begin
 result:=false;
end;

procedure TScreenExit.Update(const aDeltaTime:double);
const BoolToInt:array[boolean] of TVkInt32=(0,1);
      Options:array[0..1] of string=('Yes','No');
var Index:TVkInt32;
    cy:single;
    s:string;
    IsSelected:boolean;
begin
 inherited Update(aDeltaTime);
 ExampleVulkanApplication.TextOverlay.AddText(VulkanApplication.Width*0.5,ExampleVulkanApplication.TextOverlay.FontCharHeight*1.0,2.0,toaCenter,'Are you sure to exit?');
 fStartY:=(VulkanApplication.Height-((((ExampleVulkanApplication.TextOverlay.FontCharHeight+4)*FontSize)*2)-(4*FontSize)))*0.5;
 cy:=fStartY;
 for Index:=0 to 1 do begin
  IsSelected:=fSelectedIndex=Index;
  s:=' '+Options[Index]+' ';
  if IsSelected then begin
   s:='>'+s+'<';
  end;
  ExampleVulkanApplication.TextOverlay.AddText(VulkanApplication.Width*0.5,cy,FontSize,toaCenter,s,MenuColors[IsSelected,0,0],MenuColors[IsSelected,0,1],MenuColors[IsSelected,0,2],MenuColors[IsSelected,0,3],MenuColors[IsSelected,1,0],MenuColors[IsSelected,1,1],MenuColors[IsSelected,1,2],MenuColors[IsSelected,1,3]);
  cy:=cy+((ExampleVulkanApplication.TextOverlay.FontCharHeight+4)*FontSize);
 end;
 fReady:=true;
end;

end.
