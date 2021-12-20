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
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.VirtualReality;

const ApplicationTag='gltftest';      

type TApplication=class(TpvApplication)
      public
      private
       fVirtualReality:TpvVirtualReality;
       fForceUseValidationLayers:boolean;
       fForceNoVSync:boolean;
       fMaxMSAA:TpvInt32;
      public
       constructor Create; override;
       destructor Destroy; override;
       procedure SetupVulkanInstance(const aVulkanInstance:TpvVulkanInstance); override;
       procedure ChooseVulkanPhysicalDevice(var aVulkanPhysicalDevice:TpvVulkanPhysicalDevice); override;
       procedure SetupVulkanDevice(const aVulkanDevice:TpvVulkanDevice); override;
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
       procedure Check(const aDeltaTime:TpvDouble); override;
       procedure Update(const aDeltaTime:TpvDouble); override;
       procedure BeginFrame(const aDeltaTime:TpvDouble); override;
       procedure Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil); override;
       procedure FinishFrame(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil); override;
       procedure PostPresent(const aSwapChainImageIndex:TpvInt32); override;
      published
       property VirtualReality:TpvVirtualReality read fVirtualReality;
       property MaxMSAA:TpvInt32 read fMaxMSAA;
     end;

var Application:TApplication=nil;

    GLTFFileName:UTF8String='test.glb';

implementation

uses UnitScreenMain;

constructor TApplication.Create;
var VirtualRealityMode:TpvVirtualReality.TMode;
{$if not (defined(Android) or defined(iOS))}
    Index:TpvInt32;
    Parameter:String;
{$ifend}
begin
 inherited Create;
 Application:=self;
 fForceUseValidationLayers:=false;
 fForceNoVSync:=false;
 fMaxMSAA:=8;
 VirtualRealityMode:=TpvVirtualReality.TMode.Disabled;
{$if not (defined(Android) or defined(iOS))}
 Index:=1;
 while Index<=ParamCount do begin
  Parameter:=LowerCase(ParamStr(Index));
  inc(Index);
  if (Parameter='--openvr') or
     (Parameter='/openvr') then begin
   VirtualRealityMode:=TpvVirtualReality.TMode.OpenVR;
  end else if (Parameter='--fakedvr') or
              (Parameter='/fakedvr') then begin
   VirtualRealityMode:=TpvVirtualReality.TMode.Faked;
  end else if (Parameter='--force-use-validation-layers') or
              (Parameter='/force-use-validation-layers') then begin
   fForceUseValidationLayers:=true;
  end else if (Parameter='--force-no-vsync') or
              (Parameter='/force-no-vsync') then begin
   fForceNoVSync:=true;
  end else if (Parameter='--prefer-dgpus') or
              (Parameter='/prefer-dgpus') then begin
   VulkanPreferDedicatedGPUs:=true;
  end else if (Parameter='--prefer-igpus') or
              (Parameter='/prefer-igpus') then begin
   VulkanPreferDedicatedGPUs:=false;
{ end else if (Parameter='--flush-update-data') or
              (Parameter='/flush-update-data') then begin
   FlushUpdateData:=true; //}
  end else if (Parameter='--max-msaa') or
              (Parameter='/max-msaa') then begin
   if Index<=ParamCount then begin
    fMaxMSAA:=StrToIntDef(ParamStr(Index),0);
    inc(Index);
   end;
  end else begin
   GLTFFileName:=Parameter;
  end;
 end;
{$ifend}
 fVirtualReality:=TpvVirtualReality.Create(VirtualRealityMode);
 fVirtualReality.ZNear:=0.1;
 fVirtualReality.ZFar:=-Infinity;
end;

destructor TApplication.Destroy;
begin
 FreeAndNil(fVirtualReality);
 Application:=nil;
 inherited Destroy;
end;

procedure TApplication.SetupVulkanInstance(const aVulkanInstance:TpvVulkanInstance);
begin
 inherited SetupVulkanInstance(aVulkanInstance);
 fVirtualReality.CheckVulkanInstanceExtensions(aVulkanInstance);
 aVulkanInstance.EnabledExtensionNames.Duplicates:=TDuplicates.dupIgnore;
 aVulkanInstance.EnabledExtensionNames.AddStrings(fVirtualReality.RequiredVulkanInstanceExtensions);
end;

procedure TApplication.ChooseVulkanPhysicalDevice(var aVulkanPhysicalDevice:TpvVulkanPhysicalDevice);
var PhysicalDevice:TVkPhysicalDevice;
begin
 inherited ChooseVulkanPhysicalDevice(aVulkanPhysicalDevice);
 if not (fVirtualReality.Mode in [TpvVirtualReality.TMode.Disabled,TpvVirtualReality.TMode.Faked]) then begin
  fVirtualReality.ChooseVulkanPhysicalDevice(VulkanInstance,PhysicalDevice);
  pvApplication.VulkanPhysicalDeviceHandle:=PhysicalDevice;
 end;
end;

procedure TApplication.SetupVulkanDevice(const aVulkanDevice:TpvVulkanDevice);
begin
 inherited SetupVulkanDevice(aVulkanDevice);
 fVirtualReality.CheckVulkanDeviceExtensions(pvApplication.VulkanPhysicalDeviceHandle);
 aVulkanDevice.EnabledExtensionNames.Duplicates:=TDuplicates.dupIgnore;
 aVulkanDevice.EnabledExtensionNames.AddStrings(fVirtualReality.RequiredVulkanDeviceExtensions);
 if aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME)>0 then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME);
 end;
 if aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME)>0 then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME);
 end;
end;

procedure TApplication.Setup;
begin
 if Debugging or fForceUseValidationLayers then begin
  VulkanDebugging:=true;
  VulkanValidation:=true;
 end;
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
 if fForceNoVSync or not (fVirtualReality.Mode in [TpvVirtualReality.TMode.Disabled,TpvVirtualReality.TMode.Faked]) then begin
  DesiredCountSwapChainImages:=2;
  PresentMode:=TpvApplicationPresentMode.Mailbox;
 end else begin
  PresentMode:=TpvApplicationPresentMode.VSync;
 end;
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

 fVirtualReality.Load;

end;

procedure TApplication.Unload;
begin

 fVirtualReality.Unload;

 inherited Unload;

end;

procedure TApplication.AfterCreateSwapChain;
begin

 fVirtualReality.AfterCreateSwapChain;

 inherited AfterCreateSwapChain;

end;

procedure TApplication.BeforeDestroySwapChain;
begin

 inherited BeforeDestroySwapChain;

 fVirtualReality.BeforeDestroySwapChain;

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
 if aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down then begin
  case aKeyEvent.KeyCode of
   KEYCODE_F9:begin
    VirtualReality.ResetOrientation;
   end;
   KEYCODE_F11:begin
   end;
  end;
 end;
end;

procedure TApplication.Check(const aDeltaTime:TpvDouble);
begin
 fVirtualReality.Check(aDeltaTime);
 inherited Check(aDeltaTime);
end;

procedure TApplication.Update(const aDeltaTime:TpvDouble);
begin
 fVirtualReality.Update(aDeltaTime);
 inherited Update(aDeltaTime);
end;

procedure TApplication.BeginFrame(const aDeltaTime:TpvDouble);
begin
 fVirtualReality.BeginFrame(aDeltaTime);
 inherited BeginFrame(aDeltaTime);
end;

procedure TApplication.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
begin
 inherited Draw(aSwapChainImageIndex,aWaitSemaphore,nil);
 fVirtualReality.Draw(aSwapChainImageIndex,aWaitSemaphore,aWaitFence);
end;

procedure TApplication.FinishFrame(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
begin
 inherited FinishFrame(aSwapChainImageIndex,aWaitSemaphore,nil);
 fVirtualReality.FinishFrame(aSwapChainImageIndex,aWaitSemaphore,aWaitFence);
end;

procedure TApplication.PostPresent(const aSwapChainImageIndex:TpvInt32);
begin
 inherited PostPresent(aSwapChainImageIndex);
 fVirtualReality.PostPresent(aSwapChainImageIndex);
end;

end.
