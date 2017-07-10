unit UnitExampleVulkanApplication;
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
       procedure Start; override;
       procedure Stop; override;
       procedure Load; override;
       procedure Unload; override;
       procedure AfterCreateSwapChain; override;
       procedure BeforeDestroySwapChain; override;
       procedure Resume; override;
       procedure Pause; override;
       procedure Update(const aDeltaTime:double); override;
       procedure Draw(const aSwapChainImageIndex:TVkInt32;var aWaitSemaphore:TVulkanSemaphore;const aWaitFence:TVulkanFence=nil); override;
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

procedure TExampleVulkanApplication.Update(const aDeltaTime:double);
begin
 fTextOverlay.PreUpdate(aDeltaTime);
 inherited Update(aDeltaTime);
 fTextOverlay.PostUpdate(aDeltaTime);
end;

procedure TExampleVulkanApplication.Draw(const aSwapChainImageIndex:TVkInt32;var aWaitSemaphore:TVulkanSemaphore;const aWaitFence:TVulkanFence=nil);
begin
 inherited Draw(aSwapChainImageIndex,aWaitSemaphore,nil);
 fTextOverlay.Draw(aSwapChainImageIndex,aWaitSemaphore,aWaitFence);
end;

class procedure TExampleVulkanApplication.Main;
begin
 VulkanApplication:=TExampleVulkanApplication.Create;
 try
  if VulkanApplication.Debugging then begin
   VulkanApplication.VulkanDebugging:=true;
   VulkanApplication.VulkanValidation:=true;
  end;
  VulkanApplication.Title:='SDL Vulkan Examples Application';
  VulkanApplication.PathName:='SDLVulkanExamplesApplication';
  VulkanApplication.StartScreen:=TScreenMainMenu;
  VulkanApplication.VisibleMouseCursor:=true;
  VulkanApplication.CatchMouse:=false;
  VulkanApplication.HideSystemBars:=true;
  VulkanApplication.AndroidSeparateMouseAndTouch:=false;
  VulkanApplication.VSync:={$ifdef NoVSync}false{$else}true{$endif};
  VulkanApplication.Run;
 finally
  FreeAndNil(VulkanApplication);
 end;
end;

end.
