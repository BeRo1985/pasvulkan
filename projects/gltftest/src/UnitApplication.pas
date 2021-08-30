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

const ApplicationTag='gltftest';      

type TApplication=class(TpvApplication)
      public
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
     end;

var Application:TApplication=nil;

implementation

uses UnitScreenMain;

constructor TApplication.Create;
begin
 inherited Create;
 Application:=self;
end;

destructor TApplication.Destroy;
begin
 Application:=nil;
 inherited Destroy;
end;

procedure TApplication.Setup;
begin
 if Debugging then begin
  VulkanDebugging:=true;
  VulkanValidation:=true;
 end;
 VulkanDebugging:=true;
 VulkanValidation:=true;
 Title:='PasVulkan GLTF Test';
 PathName:='gltftest.pasvulkan';
 StartScreen:=TScreenMain;
 VisibleMouseCursor:=true;
 CatchMouse:=false;
 HideSystemBars:=true;
 AndroidSeparateMouseAndTouch:=true;
 UseAudio:=true;
 SwapChainColorSpace:=TpvApplicationSwapChainColorSpace.SRGB;
//DesiredCountSwapChainImages:=2;
 PresentMode:={$ifdef NoVSync}TpvApplicationPresentMode.Mailbox{TpvApplicationPresentMode.NoVSync}{$else}TpvApplicationPresentMode.VSync{$endif};
// VulkanAPIVersion:=VK_API_VERSION_1_0;
 VulkanAPIVersion:=0;//VK_API_VERSION_1_0;
end;

procedure TApplication.Start;
begin
 inherited Start;
end;

procedure TApplication.Stop;
begin
 inherited Stop;
end;

procedure TApplication.Load;
begin
 if not VulkanMultiviewSupportEnabled then begin
  raise EpvVulkanException.Create('Missing Vulkan multi-view support');
 end;
 inherited Load;
end;

procedure TApplication.Unload;
begin
 inherited Unload;
end;

procedure TApplication.AfterCreateSwapChain;
begin
 inherited AfterCreateSwapChain;
end;

procedure TApplication.BeforeDestroySwapChain;
begin
 inherited BeforeDestroySwapChain;
end;

procedure TApplication.Resume;
begin
 inherited Resume;
end;

procedure TApplication.Pause;
begin
 inherited Pause;
end;

function TApplication.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=inherited KeyEvent(aKeyEvent);
end;

procedure TApplication.Update(const aDeltaTime:TpvDouble);
begin
 inherited Update(aDeltaTime);
end;

procedure TApplication.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
begin
 inherited Draw(aSwapChainImageIndex,aWaitSemaphore,aWaitFence);
end;

end.
