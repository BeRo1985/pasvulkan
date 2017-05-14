unit UnitExampleVulkanApplication;
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

uses SysUtils,Classes,UnitRegisteredExamplesList,Vulkan,PasVulkan,PasVulkanAndroid,PasVulkanSDL2,PasVulkanApplication,UnitTextOverlay;

const MenuColors:array[boolean,0..1,0..3] of single=
       (((0.0625,0.125,0.5,0.95),(1.0,1.0,1.0,0.95)),
        ((1.0,1.0,1.0,0.95),(0.0625,0.125,0.5,0.95)));

type TExampleVulkanApplication=class(TVulkanApplication)
      private
       fTextOverlay:TTextOverlay;
      public
       constructor Create; override;
       destructor Destroy; override;
//     function HandleEvent(const pEvent:TSDL_Event):boolean; override;
       procedure Start; override;
       procedure Stop; override;
       procedure Load; override;
       procedure Unload; override;
       procedure AfterCreateSwapChain; override;
       procedure BeforeDestroySwapChain; override;
       procedure Resume; override;
       procedure Pause; override;
       procedure Update(const pDeltaTime:double); override;
       procedure Draw(const pSwapChainImageIndex:TVkInt32;var pWaitSemaphore:TVulkanSemaphore;const pWaitFence:TVulkanFence=nil); override;
       class procedure Main; override;
      published
       property TextOverlay:TTextOverlay read fTextOverlay;
     end;

var ExampleVulkanApplication:TExampleVulkanApplication=nil;

implementation

uses UnitScreenMainMenu;

constructor TExampleVulkanApplication.Create;
begin
 inherited Create;
 ExampleVulkanApplication:=self;
 fTextOverlay:=nil;
end;

destructor TExampleVulkanApplication.Destroy;
begin
 ExampleVulkanApplication:=nil;
 inherited Destroy;
end;

(*
function TExampleVulkanApplication.HandleEvent(const pEvent:TSDL_Event):boolean;
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
{$ifdef Android}SDLK_AC_BACK,{$endif}SDLK_ESCAPE:begin
     QuitMsgBoxDataButtonID:=-1;
     if (SDL_ShowMessageBox(@QuitMsgBoxData,@QuitMsgBoxDataButtonID)=0) and (QuitMsgBoxDataButtonID<>0) then begin
      Terminate;
     end;
     result:=true;
    end;
   end;
  end;
 end;
 if not result then begin
  result:=inherited HandleEvent(pEvent);
 end;
end;(**)

procedure TExampleVulkanApplication.Start;
begin
 inherited Start;
 fTextOverlay:=TTextOverlay.Create;
end;

procedure TExampleVulkanApplication.Stop;
begin
 FreeAndNil(fTextOverlay);
 inherited Stop;
end;

procedure TExampleVulkanApplication.Load;
begin
 inherited Load;
 fTextOverlay.Load;
end;

procedure TExampleVulkanApplication.Unload;
begin
 fTextOverlay.Unload;
 inherited Unload;
end;

procedure TExampleVulkanApplication.AfterCreateSwapChain;
begin
 inherited AfterCreateSwapChain;
 fTextOverlay.Load;
 fTextOverlay.AfterCreateSwapChain;
end;

procedure TExampleVulkanApplication.BeforeDestroySwapChain;
begin
 inherited BeforeDestroySwapChain;
 fTextOverlay.BeforeDestroySwapChain;
end;

procedure TExampleVulkanApplication.Resume;
begin
 inherited Resume;
 fTextOverlay.Load;
end;

procedure TExampleVulkanApplication.Pause;
begin
 fTextOverlay.Unload;
 inherited Pause;
end;

procedure TExampleVulkanApplication.Update(const pDeltaTime:double);
begin
 fTextOverlay.PreUpdate(pDeltaTime);
 inherited Update(pDeltaTime);
 fTextOverlay.PostUpdate(pDeltaTime);
end;

procedure TExampleVulkanApplication.Draw(const pSwapChainImageIndex:TVkInt32;var pWaitSemaphore:TVulkanSemaphore;const pWaitFence:TVulkanFence=nil);
begin
 inherited Draw(pSwapChainImageIndex,pWaitSemaphore,nil);
 fTextOverlay.Draw(pSwapChainImageIndex,pWaitSemaphore,pWaitFence);
end;

class procedure TExampleVulkanApplication.Main;
begin
 VulkanApplication:=TExampleVulkanApplication.Create;
 try
{$ifndef Release}
  VulkanApplication.VulkanDebugging:=true;
  VulkanApplication.VulkanValidation:=true;
{$endif}
  VulkanApplication.Title:='SDL Vulkan Examples Application';
  VulkanApplication.PathName:='SDLVulkanExamplesApplication';
  VulkanApplication.StartScreen:=TScreenMainMenu;
  VulkanApplication.VisibleMouseCursor:=true;
  VulkanApplication.CatchMouse:=false;
  VulkanApplication.HideSystemBars:=true;
  VulkanApplication.AndroidSeparateMouseAndTouch:=false;
  VulkanApplication.Run;
 finally
  FreeAndNil(VulkanApplication);
 end;
end;

end.
