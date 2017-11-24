unit UnitExampleApplication;
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
     UnitTextOverlay;

const MenuColors:array[boolean,0..1,0..3] of TpvFloat=
       (((0.0051556683990326,0.0143498747068026,0.214041140482232,0.95),(1.0,1.0,1.0,0.95)),
        ((1.0,1.0,1.0,0.95),(0.0051556683990326,0.0143498747068026,0.214041140482232,0.95)));

{const MenuColors:array[boolean,0..1,0..3] of TpvFloat=
       (((0.0625,0.125,0.5,0.95),(1.0,1.0,1.0,0.95)),
        ((1.0,1.0,1.0,0.95),(0.0625,0.125,0.5,0.95)));}

type TExampleApplication=class(TpvApplication)
      private
       fMakeScreenshotJPEG:boolean;
       fMakeScreenshotPNG:boolean;
       fTextOverlay:TTextOverlay;
      public
       constructor Create; override;
       destructor Destroy; override;
       procedure Setup; override;
       procedure Start; override;
       procedure Stop; override;
       procedure Load; override;
       procedure Unload; override;
       procedure AfterCreateSwapChain; override;
       procedure BeforeDestroySwapChain; override;
       procedure Resume; override;
       procedure Pause; override;
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;
       procedure Update(const aDeltaTime:TpvDouble); override;
       procedure Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil); override;
      published
       property TextOverlay:TTextOverlay read fTextOverlay;
     end;

var ExampleApplication:TExampleApplication=nil;

implementation

uses UnitScreenMainMenu;

constructor TExampleApplication.Create;
begin
 inherited Create;
 ExampleApplication:=self;
 fMakeScreenshotJPEG:=false;
 fMakeScreenshotPNG:=false;
 fTextOverlay:=nil;
end;

destructor TExampleApplication.Destroy;
begin
 ExampleApplication:=nil;
 inherited Destroy;
end;

procedure TExampleApplication.Setup;
begin
 if Debugging then begin
  VulkanDebugging:=true;
  VulkanValidation:=true;
 end;
 Title:='SDL Vulkan Examples Application';
 PathName:='SDLVulkanExamplesApplication';
 StartScreen:=TScreenMainMenu;
 VisibleMouseCursor:=true;
 CatchMouse:=false;
 HideSystemBars:=true;
 AndroidSeparateMouseAndTouch:=true;
 UseAudio:=true;
//PresentMode:=TpvApplicationPresentMode.PRESENT_MODE_MAILBOX;
 PresentMode:={$ifdef NoVSync}TpvApplicationPresentMode.PRESENT_MODE_MAILBOX{TpvApplicationPresentMode.PRESENT_MODE_NO_VSYNC}{$else}TpvApplicationPresentMode.PRESENT_MODE_VSYNC{$endif};
end;

procedure TExampleApplication.Start;
begin
 inherited Start;
 fTextOverlay:=TTextOverlay.Create;
end;

procedure TExampleApplication.Stop;
begin
 FreeAndNil(fTextOverlay);
 inherited Stop;
end;

procedure TExampleApplication.Load;
begin
 inherited Load;
 fTextOverlay.Load;
end;

procedure TExampleApplication.Unload;
begin
 fTextOverlay.Unload;
 inherited Unload;
end;

procedure TExampleApplication.AfterCreateSwapChain;
begin
 inherited AfterCreateSwapChain;
 fTextOverlay.Load;
 fTextOverlay.AfterCreateSwapChain;
end;

procedure TExampleApplication.BeforeDestroySwapChain;
begin
 inherited BeforeDestroySwapChain;
 fTextOverlay.BeforeDestroySwapChain;
end;

procedure TExampleApplication.Resume;
begin
 inherited Resume;
 fTextOverlay.Load;
end;

procedure TExampleApplication.Pause;
begin
 fTextOverlay.Unload;
 inherited Pause;
end;

function TExampleApplication.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=inherited KeyEvent(aKeyEvent);
 if aKeyEvent.KeyEventType=KEYEVENT_DOWN then begin
  case aKeyEvent.KeyCode of
   KEYCODE_F10:begin
    fMakeScreenshotJPEG:=true;
   end;
   KEYCODE_F11:begin
    fMakeScreenshotPNG:=true;
   end;
  end;
 end;
end;

procedure TExampleApplication.Update(const aDeltaTime:TpvDouble);
begin
 fTextOverlay.PreUpdate(aDeltaTime);
 inherited Update(aDeltaTime);
 fTextOverlay.PostUpdate(aDeltaTime);
end;

procedure TExampleApplication.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
var Stream:TMemoryStream;
begin
 inherited Draw(aSwapChainImageIndex,aWaitSemaphore,nil);
 fTextOverlay.Draw(aSwapChainImageIndex,aWaitSemaphore,aWaitFence);
 if fMakeScreenshotJPEG then begin
  fMakeScreenshotJPEG:=false;
  Stream:=TMemoryStream.Create;
  try
   VulkanSwapChain.SaveScreenshotAsJPEGToStream(Stream);
   try
    Stream.SaveToFile('screenshot.jpeg');
   except
   end;
  finally
   Stream.Free;
  end;
 end else if fMakeScreenshotPNG then begin
  fMakeScreenshotPNG:=false;
  Stream:=TMemoryStream.Create;
  try
   VulkanSwapChain.SaveScreenshotAsPNGToStream(Stream);
   try
    Stream.SaveToFile('screenshot.png');
   except
   end;
  finally
   Stream.Free;
  end;
 end;
end;

end.
