unit UnitScreenMainMenu;
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

uses SysUtils,Classes,Vulkan,PasVulkan,PasVulkanAndroid,PasVulkanSDL2,PasVulkanApplication,UnitRegisteredExamplesList,UnitScreenBlank;

type TScreenMainMenu=class(TScreenBlank)
      private
       fReady:boolean;
       fSelectedIndex:TVkInt32;
      public

       constructor Create; override;

       destructor Destroy; override;

       function HandleEvent(const pEvent:TSDL_Event):boolean; override;

       function KeyDown(const pKeyCode,pKeyModifier:TVkInt32):boolean; override;

       function KeyUp(const pKeyCode,pKeyModifier:TVkInt32):boolean; override;

       function KeyTyped(const pKeyCode,pKeyModifier:TVkInt32):boolean; override;

       function TouchDown(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean; override;

       function TouchUp(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean; override;

       function TouchDragged(const pScreenX,pScreenY,pPressure:single;const pPointerID:TVkInt32):boolean; override;

       function MouseMoved(const pScreenX,pScreenY:TVkInt32):boolean; override;

       function Scrolled(const pAmount:TVkInt32):boolean; override;

       procedure Update(const pDeltaTime:double); override;

     end;

implementation

uses UnitExampleVulkanApplication,UnitTextOverlay;


constructor TScreenMainMenu.Create;
begin
 inherited Create;
 fReady:=false;
end;

destructor TScreenMainMenu.Destroy;
begin
 inherited Destroy;
end;

function TScreenMainMenu.HandleEvent(const pEvent:TSDL_Event):boolean;
const QuitMsgBoxButtons:array[0..1] of TSDL_MessageBoxButtonData=
{$ifdef Windows}
       ((flags:SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT;buttonid:0;text:'No'),
        (flags:SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT;buttonid:1;text:'Yes'));
{$else}
       ((flags:SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT;buttonid:1;text:'Yes'),
        (flags:SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT;buttonid:0;text:'No'));
{$endif}
      QuitMsgBoxColorScheme:TSDL_MessageBoxColorScheme=
       (colors:((r:0;g:0;b:0),
                (r:255;g:255;b:255),
                (r:192;g:192;b:192),
                (r:64;g:64;b:64),
                (r:128;g:128;b:128)));
      QuitMsgBoxData:TSDL_MessageBoxData=
       (
        flags:SDL_MESSAGEBOX_WARNING;
        window:nil;
        title:'PasVulkanApplication';
        message:'Are you sure to exit?';
        numbuttons:length(QuitMsgBoxButtons);
        buttons:@QuitMsgBoxButtons[0];
        colorScheme:@QuitMsgBoxColorScheme;
       );
var QuitMsgBoxDataButtonID:TSDLInt32;
begin
 result:=false;
 case pEvent.type_ of
  SDL_KEYDOWN:begin
{$if defined(fpc) and defined(android)}
   __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication',PAnsiChar(AnsiString('Keydown: '+IntToStr(pEvent.key.keysym.sym))));
{$ifend}
   case pEvent.key.keysym.sym of
    SDLK_AC_BACK,SDLK_ESCAPE:begin
     QuitMsgBoxDataButtonID:=-1;
     if (SDL_ShowMessageBox(@QuitMsgBoxData,@QuitMsgBoxDataButtonID)=0) and (QuitMsgBoxDataButtonID<>0) then begin
      VulkanApplication.Terminate;
     end;
     result:=true;
    end;
   end;
  end;
 end;
 if not result then begin
  result:=inherited HandleEvent(pEvent);
 end;
end;

function TScreenMainMenu.KeyDown(const pKeyCode,pKeyModifier:TVkInt32):boolean;
begin
 result:=false;
 if fReady then begin
  case pKeyCode of
   KEYCODE_UP:begin
    if fSelectedIndex<=0 then begin
     fSelectedIndex:=RegisteredExamplesList.Count-1;
    end else begin
     dec(fSelectedIndex);
    end;
   end;
   KEYCODE_DOWN:begin
    if fSelectedIndex>=(RegisteredExamplesList.Count-1) then begin
     fSelectedIndex:=0;
    end else begin
     inc(fSelectedIndex);
    end;
   end;
   KEYCODE_RETURN:begin
    VulkanApplication.NextScreen:=TVulkanScreenClass(RegisteredExamplesList.Objects[fSelectedIndex]).Create;
   end;
  end;
 end;
end;

function TScreenMainMenu.KeyUp(const pKeyCode,pKeyModifier:TVkInt32):boolean;
begin
 result:=false;
end;

function TScreenMainMenu.KeyTyped(const pKeyCode,pKeyModifier:TVkInt32):boolean;
begin
 result:=false;
end;

function TScreenMainMenu.TouchDown(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean;
var Index:TVkInt32;
    cy:single;
begin
 result:=false;
 if fReady then begin
  cy:=ExampleVulkanApplication.TextOverlay.FontCharHeight*5.0;
  for Index:=0 to RegisteredExamplesList.Count-1 do begin
   if (pScreenY>=cy) and (pScreenY<=(cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*2.0))) then begin
    fSelectedIndex:=Index;
    VulkanApplication.NextScreen:=TVulkanScreenClass(RegisteredExamplesList.Objects[fSelectedIndex]).Create;
   end;
   cy:=cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*2.0)+8;
  end;
 end;
end;

function TScreenMainMenu.TouchUp(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean;
begin
 result:=false;
end;

function TScreenMainMenu.TouchDragged(const pScreenX,pScreenY,pPressure:single;const pPointerID:TVkInt32):boolean;
var Index:TVkInt32;
    cy:single;
begin
 result:=false;
 if fReady then begin
  cy:=ExampleVulkanApplication.TextOverlay.FontCharHeight*5.0;
  for Index:=0 to RegisteredExamplesList.Count-1 do begin
   if (pScreenY>=cy) and (pScreenY<=(cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*2.0))) then begin
    fSelectedIndex:=Index;
   end;
   cy:=cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*2.0)+8;
  end;
 end;
end;

function TScreenMainMenu.MouseMoved(const pScreenX,pScreenY:TVkInt32):boolean;
var Index:TVkInt32;
    cy:single;
begin
 result:=false;
 if fReady then begin
  cy:=ExampleVulkanApplication.TextOverlay.FontCharHeight*5.0;
  for Index:=0 to RegisteredExamplesList.Count-1 do begin
   if (pScreenY>=cy) and (pScreenY<=(cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*2.0))) then begin
    fSelectedIndex:=Index;
   end;
   cy:=cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*2.0)+8;
  end;
 end;
end;

function TScreenMainMenu.Scrolled(const pAmount:TVkInt32):boolean;
begin
 result:=false;
end;

procedure TScreenMainMenu.Update(const pDeltaTime:double);
const BoolToInt:array[boolean] of TVkInt32=(0,1);
var Index:TVkInt32;
    cy:single;
    s:string;
    IsSelected:boolean;
begin
 inherited Update(pDeltaTime);
 cy:=ExampleVulkanApplication.TextOverlay.FontCharHeight*5.0;
 for Index:=0 to RegisteredExamplesList.Count-1 do begin
  IsSelected:=fSelectedIndex=Index;
  s:=' '+RegisteredExamplesList[Index]+' ';
  if IsSelected then begin
   s:='>'+s+'<';
  end;
  ExampleVulkanApplication.TextOverlay.AddText(VulkanApplication.Width*0.5,cy,2.0,toaCenter,s,1.0-BoolToInt[IsSelected],1.0-BoolToInt[IsSelected],1.0,(BoolToInt[IsSelected]*0.95)+0.05,1.0,1.0,1.0,1.0);
  cy:=cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*2.0)+8;
 end;
 fReady:=true;
end;

end.
