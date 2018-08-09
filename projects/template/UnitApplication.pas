unit UnitApplication;
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
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application;

type TApplication=class(TpvApplication)
      private
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

var Application:TApplication=nil;

implementation

constructor TExampleApplication.Create;
begin
 inherited Create;
 Application:=self;
end;

destructor TExampleApplication.Destroy;
begin
 Application:=nil;
 inherited Destroy;
end;

procedure TExampleApplication.Setup;
begin
 if Debugging then begin
  VulkanDebugging:=true;
  VulkanValidation:=true;
 end;
 Title:='PasVulkan Application';
 PathName:='PasVulkanApplication';
 StartScreen:=TScreenMainMenu;
 VisibleMouseCursor:=true;
 CatchMouse:=false;
 HideSystemBars:=true;
 AndroidSeparateMouseAndTouch:=true;
 UseAudio:=true;
//DesiredCountSwapChainImages:=2;
 PresentMode:={$ifdef NoVSync}TpvApplicationPresentMode.Mailbox{TpvApplicationPresentMode.NoVSync}{$else}TpvApplicationPresentMode.VSync{$endif};
end;

procedure TExampleApplication.Start;
begin
 inherited Start;
end;

procedure TExampleApplication.Stop;
begin
 inherited Stop;
end;

procedure TExampleApplication.Load;
begin
 inherited Load;
end;

procedure TExampleApplication.Unload;
begin
 inherited Unload;
end;

procedure TExampleApplication.AfterCreateSwapChain;
begin
 inherited AfterCreateSwapChain;
end;

procedure TExampleApplication.BeforeDestroySwapChain;
begin
 inherited BeforeDestroySwapChain;
end;

procedure TExampleApplication.Resume;
begin
 inherited Resume;
end;

procedure TExampleApplication.Pause;
begin
 inherited Pause;
end;

function TExampleApplication.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=inherited KeyEvent(aKeyEvent);
end;

procedure TExampleApplication.Update(const aDeltaTime:TpvDouble);
begin
 inherited Update(aDeltaTime);
end;

procedure TExampleApplication.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
begin
 inherited Draw(aSwapChainImageIndex,aWaitSemaphore,nil);
end;

end.
