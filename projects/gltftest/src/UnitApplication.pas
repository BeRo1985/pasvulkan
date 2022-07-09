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
     PasVulkan.VirtualReality,
     UnitGlobals;

const ApplicationTag='gltftest';      

type TApplication=class(TpvApplication)
      public
      private
       fVirtualReality:TpvVirtualReality;
       fForceUseValidationLayers:boolean;
       fForceNoVSync:boolean;
       fMaxMSAA:TpvInt32;
       fMaxShadowMSAA:TpvInt32;
       fShadowMapSize:TpvInt32;
       fTransparencyMode:TTransparencyMode;
       fAntialiasingMode:TAntialiasingMode;
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
       property MaxShadowMSAA:TpvInt32 read fMaxShadowMSAA;
       property ShadowMapSize:TpvInt32 read fShadowMapSize;
       property TransparencyMode:TTransparencyMode read fTransparencyMode;
       property AntialiasingMode:TAntialiasingMode read fAntialiasingMode;
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
 VulkanNVIDIAAfterMath:=false;
 fMaxMSAA:=0;
 fMaxShadowMSAA:=0;
 fShadowMapSize:=512;
 fTransparencyMode:=TTransparencyMode.Auto;
 fAntialiasingMode:=TAntialiasingMode.Auto;
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
  end else if (Parameter='--nvidia-aftermath') or
              (Parameter='/nvidia-aftermath') then begin
   VulkanNVIDIAAfterMath:=true;
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
  end else if (Parameter='--max-shadow-msaa') or
              (Parameter='/max-shadow-msaa') then begin
   if Index<=ParamCount then begin
    fMaxShadowMSAA:=StrToIntDef(ParamStr(Index),0);
    inc(Index);
   end;
  end else if (Parameter='--shadow-map-size') or
              (Parameter='/shadow-map-size') then begin
   if Index<=ParamCount then begin
    fShadowMapSize:=StrToIntDef(ParamStr(Index),0);
    inc(Index);
   end;
  end else if (Parameter='--transparency-mode') or
              (Parameter='/transparency-mode') then begin
   if Index<=ParamCount then begin
    Parameter:=LowerCase(trim(ParamStr(Index)));
    inc(Index);
    if Parameter='direct' then begin
     fTransparencyMode:=TTransparencyMode.Direct;
    end else if Parameter='spinlockoit' then begin
     fTransparencyMode:=TTransparencyMode.SPINLOCKOIT;
    end else if Parameter='interlockoit' then begin
     fTransparencyMode:=TTransparencyMode.INTERLOCKOIT;
    end else if Parameter='wboit' then begin
     fTransparencyMode:=TTransparencyMode.WBOIT;
    end else if Parameter='mboit' then begin
     fTransparencyMode:=TTransparencyMode.MBOIT;
    end else begin
     fTransparencyMode:=TTransparencyMode.Auto;
    end;
   end;
  end else if (Parameter='--antialiasing-mode') or
              (Parameter='/antialiasing-mode') then begin
   if Index<=ParamCount then begin
    Parameter:=LowerCase(trim(ParamStr(Index)));
    inc(Index);
    if Parameter='none' then begin
     fAntialiasingMode:=TAntialiasingMode.None;
    end else if Parameter='dsaa' then begin
     fAntialiasingMode:=TAntialiasingMode.DSAA;
    end else if Parameter='fxaa' then begin
     fAntialiasingMode:=TAntialiasingMode.FXAA;
    end else if Parameter='smaa' then begin
     fAntialiasingMode:=TAntialiasingMode.SMAA;
    end else if Parameter='msaa' then begin
     fAntialiasingMode:=TAntialiasingMode.MSAA;
    end else begin
     fTransparencyMode:=TTransparencyMode.Auto;
    end;
   end;
  end else begin
   GLTFFileName:=Parameter;
  end;
 end;
{$ifend}
 if VirtualRealityMode=TpvVirtualReality.TMode.Disabled then begin
  fVirtualReality:=nil;
 end else begin
  fVirtualReality:=TpvVirtualReality.Create(VirtualRealityMode);
  fVirtualReality.ZNear:=0.1;
  fVirtualReality.ZFar:=-Infinity;
 end;
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
 if assigned(fVirtualReality) then begin
  fVirtualReality.CheckVulkanInstanceExtensions(aVulkanInstance);
  aVulkanInstance.EnabledExtensionNames.Duplicates:=TDuplicates.dupIgnore;
  aVulkanInstance.EnabledExtensionNames.AddStrings(fVirtualReality.RequiredVulkanInstanceExtensions);
 end;
end;

procedure TApplication.ChooseVulkanPhysicalDevice(var aVulkanPhysicalDevice:TpvVulkanPhysicalDevice);
var PhysicalDevice:TVkPhysicalDevice;
begin
 inherited ChooseVulkanPhysicalDevice(aVulkanPhysicalDevice);
 if assigned(fVirtualReality) and not (fVirtualReality.Mode in [TpvVirtualReality.TMode.Disabled,TpvVirtualReality.TMode.Faked]) then begin
  fVirtualReality.ChooseVulkanPhysicalDevice(VulkanInstance,PhysicalDevice);
  pvApplication.VulkanPhysicalDeviceHandle:=PhysicalDevice;
 end;
end;

procedure TApplication.SetupVulkanDevice(const aVulkanDevice:TpvVulkanDevice);
begin
 inherited SetupVulkanDevice(aVulkanDevice);
 if assigned(fVirtualReality) then begin
  fVirtualReality.CheckVulkanDeviceExtensions(pvApplication.VulkanPhysicalDeviceHandle);
  aVulkanDevice.EnabledExtensionNames.Duplicates:=TDuplicates.dupIgnore;
  aVulkanDevice.EnabledExtensionNames.AddStrings(fVirtualReality.RequiredVulkanDeviceExtensions);
 end;
 if (aVulkanDevice.PhysicalDevice.DescriptorIndexingFeaturesEXT.descriptorBindingPartiallyBound=VK_FALSE) or
    (aVulkanDevice.PhysicalDevice.DescriptorIndexingFeaturesEXT.runtimeDescriptorArray=VK_FALSE) or
    (aVulkanDevice.PhysicalDevice.DescriptorIndexingFeaturesEXT.shaderSampledImageArrayNonUniformIndexing=VK_FALSE) then begin
  raise EpvApplication.Create('Application','Support for VK_EXT_DESCRIPTOR_INDEXING (descriptorBindingPartiallyBound + runtimeDescriptorArray + shaderSampledImageArrayNonUniformIndexing) is needed',LOG_ERROR);
 end;
 if aVulkanDevice.PhysicalDevice.BufferDeviceAddressFeaturesKHR.bufferDeviceAddress=VK_FALSE then begin
  raise EpvApplication.Create('Application','Support for VK_KHR_buffer_device_address (bufferDeviceAddress) is needed',LOG_ERROR);
 end;
 if aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME)>=0 then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME);
 end;
 if aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_KHR_MAINTENANCE1_EXTENSION_NAME)>=0 then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_KHR_MAINTENANCE1_EXTENSION_NAME);
 end;
 if aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_KHR_MAINTENANCE2_EXTENSION_NAME)>=0 then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_KHR_MAINTENANCE2_EXTENSION_NAME);
 end;
 if aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_KHR_MAINTENANCE3_EXTENSION_NAME)>=0 then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_KHR_MAINTENANCE3_EXTENSION_NAME);
 end;
 if aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME)>=0 then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME);
 end;
 if aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME)>=0 then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME);
 end;
 if aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME)>=0 then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME);
 end;
 if aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME)>=0 then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME);
 end;
 if aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME)>=0 then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME);
 end;
 if ((aVulkanDevice.Instance.APIVersion and VK_API_VERSION_WITHOUT_PATCH_MASK)<VK_API_VERSION_1_2) and
    (aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_KHR_SPIRV_1_4_EXTENSION_NAME)>=0) then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_KHR_SPIRV_1_4_EXTENSION_NAME);
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
//Blocking:=false;
//DesiredCountSwapChainImages:=2;
 if fForceNoVSync or (assigned(fVirtualReality) and not (fVirtualReality.Mode in [TpvVirtualReality.TMode.Disabled,TpvVirtualReality.TMode.Faked])) then begin
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

 if assigned(fVirtualReality) then begin
  fVirtualReality.Load;
 end;

end;

procedure TApplication.Unload;
begin

 if assigned(fVirtualReality) then begin
  fVirtualReality.Unload;
 end;

 inherited Unload;

end;

procedure TApplication.AfterCreateSwapChain;
begin

 if assigned(fVirtualReality) then begin
  fVirtualReality.AfterCreateSwapChain;
 end;

 inherited AfterCreateSwapChain;

end;

procedure TApplication.BeforeDestroySwapChain;
begin

 inherited BeforeDestroySwapChain;

 if assigned(fVirtualReality) then begin
  fVirtualReality.BeforeDestroySwapChain;
 end;

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
 if assigned(fVirtualReality) then begin
  fVirtualReality.Check(aDeltaTime);
 end;
 inherited Check(aDeltaTime);
end;

procedure TApplication.Update(const aDeltaTime:TpvDouble);
begin
 if assigned(fVirtualReality) then begin
  fVirtualReality.Update(aDeltaTime);
 end;
 inherited Update(aDeltaTime);
end;

procedure TApplication.BeginFrame(const aDeltaTime:TpvDouble);
begin
 if assigned(fVirtualReality) then begin
  fVirtualReality.BeginFrame(aDeltaTime);
 end;
 inherited BeginFrame(aDeltaTime);
end;

procedure TApplication.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
begin
 if assigned(fVirtualReality) then begin
  inherited Draw(aSwapChainImageIndex,aWaitSemaphore,nil);
  fVirtualReality.Draw(aSwapChainImageIndex,aWaitSemaphore,aWaitFence);
 end else begin
  inherited Draw(aSwapChainImageIndex,aWaitSemaphore,aWaitFence);
 end;
end;

procedure TApplication.FinishFrame(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
begin
 if assigned(fVirtualReality) then begin
  inherited FinishFrame(aSwapChainImageIndex,aWaitSemaphore,nil);
  fVirtualReality.FinishFrame(aSwapChainImageIndex,aWaitSemaphore,aWaitFence);
 end else begin
  inherited FinishFrame(aSwapChainImageIndex,aWaitSemaphore,aWaitFence);
 end;
end;

procedure TApplication.PostPresent(const aSwapChainImageIndex:TpvInt32);
begin
 inherited PostPresent(aSwapChainImageIndex);
 if assigned(fVirtualReality) then begin
  fVirtualReality.PostPresent(aSwapChainImageIndex);
 end;
end;

end.
