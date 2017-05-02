unit UnitVulkanPresentationSurface;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$if defined(Win32) or defined(Win64)}
 {$define Windows}
{$ifend}

interface

uses {$if defined(Windows)}
      Windows,
     {$elseif defined(Unix)}
      BaseUnix,UnixType,dl,
     {$ifend}
     {$if defined(XLIB) and defined(VulkanUseXLIBUnits)}x,xlib,{$ifend}
     {$if defined(XCB) and defined(VulkanUseXCBUnits)}xcb,{$ifend}
     {$if defined(Mir) and defined(VulkanUseMirUnits)}Mir,{$ifend}
     {$if defined(Wayland) and defined(VulkanUseWaylandUnits)}Wayland,{$ifend}
     {$if defined(Android) and defined(VulkanUseAndroidUnits)}Android,{$ifend}
     SysUtils,Classes,Math,
     Vulkan,PasVulkan;

const MaxSwapChainImages=3;

type TVulkanPresentationSurface=class
      private
       fVulkanInstance:TVulkanInstance;
       fVulkanSurface:TVulkanSurface;
       fVulkanDevice:TVulkanDevice;
       fVulkanInitializationCommandBufferFence:TVulkanFence;
       fVulkanInitializationCommandPool:TVulkanCommandPool;
       fVulkanInitializationCommandBuffer:TVulkanCommandBuffer;
       fVulkanSwapChain:TVulkanSwapChain;
       fVulkanSwapChainImageFences:array[0..MaxSwapChainImages-1] of TVulkanFence;
       fVulkanSwapChainImageFencesReady:array[0..MaxSwapChainImages-1] of boolean;
       fVulkanSwapChainSimpleDirectRenderTarget:TVulkanSwapChainSimpleDirectRenderTarget;
       fVulkanCommandPool:TVulkanCommandPool;
       fVulkanCommandBuffers:array[0..MaxSwapChainImages-1] of TVulkanCommandBuffer;
       fVulkanCommandBufferFences:array[0..MaxSwapChainImages-1] of TVulkanFence;
       fVulkanCommandBufferFencesReady:array[0..MaxSwapChainImages-1] of boolean;
       fVulkanPresentCompleteSemaphores:array[0..MaxSwapChainImages-1] of TVulkanSemaphore;
       fVulkanDrawCompleteSemaphores:array[0..MaxSwapChainImages-1] of TVulkanSemaphore;
       fDoNeedToRecreateVulkanSwapChain:boolean;
       fGraphicsPipelinesReady:boolean;
       fWidth:TVkInt32;
       fHeight:TVkInt32;
       fLastImageIndex:TVkInt32;
       fCurrentImageIndex:TVkInt32;
       fReady:boolean;
       fVSync:boolean;
       procedure CreateGraphicsPipelines;
       procedure DestroyGraphicsPipelines;
      public
       constructor Create(const pWidth,pHeight:TVkInt32;
                          const pVSync:boolean;
                          const pSurfaceCreateInfo:TVulkanSurfaceCreateInfo);
       destructor Destroy; override;
       procedure SetSize(const pNewWidth,pNewHeight:TVkInt32);
       procedure SetVSync(const pVSync:boolean);
       procedure ClearAll;
       function AcquireBackBuffer(const pBlock:boolean):boolean;
       function PresentBackBuffer:boolean;
      published
       property Width:TVkInt32 read fWidth;
       property Height:TVkInt32 read fHeight;
       property LastImageIndex:TVkInt32 read fLastImageIndex;
       property CurrentImageIndex:TVkInt32 read fCurrentImageIndex;
       property Ready:boolean read fReady write fReady;
       property VSync:boolean read fVSync write SetVSync;
     end;

var VulkanPresentationSurface:TVulkanPresentationSurface=nil;

implementation

uses UnitGlobals,UnitSDL2Main;

constructor TVulkanPresentationSurface.Create(const pWidth,pHeight:TVkInt32;
                                              const pVSync:boolean;
                                              const pSurfaceCreateInfo:TVulkanSurfaceCreateInfo);
var Index:TVkInt32;
begin
 inherited Create;

 VulkanPresentationSurface:=self;

 fVulkanInstance:=VulkanInstance;

 SetSize(pWidth,pHeight);

 fVSync:=pVSync;

 fReady:=false;

 fLastImageIndex:=-2;

 fCurrentImageIndex:=-1;

 fGraphicsPipelinesReady:=false;

 try

  fVulkanSurface:=TVulkanSurface.Create(fVulkanInstance,pSurfaceCreateInfo);

  fVulkanDevice:=VulkanDevice;
  if not assigned(fVulkanDevice) then begin
   CreateVulkanDevice(fVulkanSurface);
   fVulkanDevice:=VulkanDevice;
   if not assigned(fVulkanDevice) then begin
    raise EVulkanSurfaceException.Create('Device does not support surface');
   end;
  end;

{ TVulkanDevice.Create(fVulkanInstance,nil,fVulkanSurface);
  fVulkanDevice.AddQueues;
  fVulkanDevice.EnabledExtensionNames.Add(VK_KHR_SWAPCHAIN_EXTENSION_NAME);
  fVulkanDevice.Initialize;{}

  fVulkanInitializationCommandBufferFence:=TVulkanFence.Create(fVulkanDevice);

  fVulkanInitializationCommandPool:=TVulkanCommandPool.Create(fVulkanDevice,fVulkanDevice.GraphicsQueueFamilyIndex,TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

  fVulkanInitializationCommandBuffer:=TVulkanCommandBuffer.Create(fVulkanInitializationCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

  fVulkanSwapChain:=TVulkanSwapChain.Create(fVulkanDevice,
                                            fVulkanSurface,
                                            nil,
                                            Width,
                                            Height,
                                            IfThen(pVSync,MaxSwapChainImages,1),
                                            1,
                                            VK_FORMAT_UNDEFINED,
                                            VK_COLOR_SPACE_SRGB_NONLINEAR_KHR,
                                            TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT),
                                            VK_SHARING_MODE_EXCLUSIVE,
                                            nil,
                                            VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
                                            TVkPresentModeKHR(integer(IfThen(pVSync,integer(VK_PRESENT_MODE_MAILBOX_KHR),integer(VK_PRESENT_MODE_IMMEDIATE_KHR)))));

  fVulkanSwapChainSimpleDirectRenderTarget:=TVulkanSwapChainSimpleDirectRenderTarget.Create(fVulkanDevice,fVulkanSwapChain,fVulkanInitializationCommandBuffer,fVulkanInitializationCommandBufferFence);

  fVulkanCommandPool:=TVulkanCommandPool.Create(fVulkanDevice,fVulkanDevice.GraphicsQueueFamilyIndex,TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

  for Index:=0 to MaxSwapChainImages-1 do begin
   fVulkanSwapChainImageFences[Index]:=TVulkanFence.Create(fVulkanDevice);
   fVulkanSwapChainImageFencesReady[Index]:=false;
   fVulkanCommandBuffers[Index]:=TVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
   fVulkanCommandBufferFences[Index]:=TVulkanFence.Create(fVulkanDevice);
   fVulkanCommandBufferFencesReady[Index]:=false;
   fVulkanPresentCompleteSemaphores[Index]:=TVulkanSemaphore.Create(fVulkanDevice);
   fVulkanDrawCompleteSemaphores[Index]:=TVulkanSemaphore.Create(fVulkanDevice);
  end;

  CreateGraphicsPipelines;

  fDoNeedToRecreateVulkanSwapChain:=false;

 except

  DestroyGraphicsPipelines;

  for Index:=0 to MaxSwapChainImages-1 do begin
   FreeAndNil(fVulkanSwapChainImageFences[Index]);
   FreeAndNil(fVulkanCommandBuffers[Index]);
   FreeAndNil(fVulkanCommandBufferFences[Index]);
   FreeAndNil(fVulkanPresentCompleteSemaphores[Index]);
   FreeAndNil(fVulkanDrawCompleteSemaphores[Index]);
  end;
  FreeAndNil(fVulkanCommandPool);
  FreeAndNil(fVulkanSwapChain);
  FreeAndNil(fVulkanInitializationCommandBuffer);
  FreeAndNil(fVulkanInitializationCommandPool);
  FreeAndNil(fVulkanInitializationCommandBufferFence);
//FreeAndNil(fVulkanDevice);
  FreeAndNil(fVulkanSurface);
  raise;
 end;
end;

destructor TVulkanPresentationSurface.Destroy;
var Index:TVkInt32;
begin
 if assigned(fVulkanDevice) then begin
  fVulkanDevice.WaitIdle;
  for Index:=0 to MaxSwapChainImages-1 do begin
   if fVulkanSwapChainImageFencesReady[Index] and assigned(fVulkanSwapChainImageFences[Index]) then begin
    fVulkanSwapChainImageFences[Index].WaitFor;
    fVulkanSwapChainImageFences[Index].Reset;
    fVulkanSwapChainImageFencesReady[Index]:=false;
   end;
   if fVulkanCommandBufferFencesReady[Index] and assigned(fVulkanCommandBufferFences[Index]) then begin
    fVulkanCommandBufferFences[Index].WaitFor;
    fVulkanCommandBufferFences[Index].Reset;
    fVulkanCommandBufferFencesReady[Index]:=false;
   end;
  end;
  fVulkanDevice.WaitIdle;
 end;
 ClearAll;
 DestroyGraphicsPipelines;
 if assigned(fVulkanDevice) then begin
  fVulkanDevice.WaitIdle;
 end;
 FreeAndNil(fVulkanSwapChainSimpleDirectRenderTarget);
 if assigned(fVulkanDevice) then begin
  fVulkanDevice.WaitIdle;
 end;
 VulkanPresentationSurface:=self;
 for Index:=0 to MaxSwapChainImages-1 do begin
  FreeAndNil(fVulkanSwapChainImageFences[Index]);
  FreeAndNil(fVulkanCommandBuffers[Index]);
  FreeAndNil(fVulkanCommandBufferFences[Index]);
  FreeAndNil(fVulkanPresentCompleteSemaphores[Index]);
  FreeAndNil(fVulkanDrawCompleteSemaphores[Index]);
 end;
 FreeAndNil(fVulkanCommandPool);
 FreeAndNil(fVulkanSwapChain);
 FreeAndNil(fVulkanInitializationCommandBuffer);
 FreeAndNil(fVulkanInitializationCommandPool);
 FreeAndNil(fVulkanInitializationCommandBufferFence);
//FreeAndNil(fVulkanDevice);
 FreeAndNil(fVulkanSurface);
 inherited Destroy;
end;

procedure TVulkanPresentationSurface.SetSize(const pNewWidth,pNewHeight:TVkInt32);
begin
 fWidth:=pNewWidth;
 fHeight:=pNewHeight;
end;

procedure TVulkanPresentationSurface.SetVSync(const pVSync:boolean);
begin
 if fVSync<>pVSync then begin
  fVSync:=pVSync;
  fDoNeedToRecreateVulkanSwapChain:=true;
 end;
end;

procedure TVulkanPresentationSurface.ClearAll;
begin
end;

procedure TVulkanPresentationSurface.CreateGraphicsPipelines;
begin
 if not fGraphicsPipelinesReady then begin
  fGraphicsPipelinesReady:=true;
 end;
end;

procedure TVulkanPresentationSurface.DestroyGraphicsPipelines;
begin
 if fGraphicsPipelinesReady then begin
  fGraphicsPipelinesReady:=false;
 end;
end;

function TVulkanPresentationSurface.AcquireBackBuffer(const pBlock:boolean):boolean;
var ImageIndex:TVkInt32;
    OldVulkanSwapChain:TVulkanSwapChain;
    TimeOut:TVkUInt64;
begin
 result:=false;

 fLastImageIndex:=fCurrentImageIndex;

 fCurrentImageIndex:=fVulkanSwapChain.CurrentImageIndex;

 if (fCurrentImageIndex<0) or (fCurrentImageIndex>=MaxSwapChainImages) then begin
  exit;
 end;

 if fVulkanSwapChainImageFencesReady[fCurrentImageIndex] then begin
  if fVulkanSwapChainImageFences[fCurrentImageIndex].GetStatus<>VK_SUCCESS then begin
   if pBlock then begin
    fVulkanSwapChainImageFences[fCurrentImageIndex].WaitFor;
   end else begin
    exit;
   end;
  end;
  fVulkanSwapChainImageFences[fCurrentImageIndex].Reset;
  fVulkanSwapChainImageFencesReady[fCurrentImageIndex]:=false;
 end;

 if fVulkanCommandBufferFencesReady[fCurrentImageIndex] then begin
  if fVulkanCommandBufferFences[fCurrentImageIndex].GetStatus<>VK_SUCCESS then begin
   if pBlock then begin
    fVulkanCommandBufferFences[fCurrentImageIndex].WaitFor;
   end else begin
    exit;
   end;
  end;
  fVulkanCommandBufferFences[fCurrentImageIndex].Reset;
  fVulkanCommandBufferFencesReady[fCurrentImageIndex]:=false;
 end;

 if (fVulkanSwapChain.Width<>Width) or (fVulkanSwapChain.Height<>Height) then begin
  fDoNeedToRecreateVulkanSwapChain:=true;
  VulkanDebugLn('New surface dimension size detected!');
 end else begin
  try
   if pBlock then begin
    TimeOut:=TVkUInt64(high(TVkUInt64));
   end else begin
    TimeOut:=0;
   end;
   case fVulkanSwapChain.AcquireNextImage(fVulkanPresentCompleteSemaphores[fCurrentImageIndex],fVulkanSwapChainImageFences[fCurrentImageIndex],TimeOut) of
    VK_SUCCESS:begin
     fVulkanSwapChainImageFencesReady[fCurrentImageIndex]:=true;
    end;
    VK_SUBOPTIMAL_KHR:begin
     fDoNeedToRecreateVulkanSwapChain:=true;
     VulkanDebugLn('Suboptimal surface detected!');
    end;
    else {VK_SUCCESS,VK_TIMEOUT:}begin
     exit;
    end;
   end;
  except
   on VulkanResultException:EVulkanResultException do begin
    case VulkanResultException.ResultCode of
     VK_ERROR_SURFACE_LOST_KHR,
     VK_ERROR_OUT_OF_DATE_KHR,
     VK_SUBOPTIMAL_KHR:begin
      fDoNeedToRecreateVulkanSwapChain:=true;
      VulkanDebugLn(VulkanResultException.ClassName+': '+VulkanResultException.Message);
     end;
     else begin
      raise;
     end;
    end;
   end;
  end;
 end;

 if fDoNeedToRecreateVulkanSwapChain then begin

  for ImageIndex:=0 to MaxSwapChainImages-1 do begin
   if fVulkanCommandBufferFencesReady[ImageIndex] then begin
    fVulkanCommandBufferFences[ImageIndex].WaitFor;
    fVulkanCommandBufferFences[ImageIndex].Reset;
    fVulkanCommandBufferFencesReady[ImageIndex]:=false;
   end;
  end;

  fVulkanDevice.WaitIdle;

  VulkanDebugLn('Recreating swap chain... ');
  fDoNeedToRecreateVulkanSwapChain:=false;
  OldVulkanSwapChain:=fVulkanSwapChain;
  try
   DestroyGraphicsPipelines;
   FreeAndNil(fVulkanSwapChainSimpleDirectRenderTarget);
   fVulkanSwapChain:=TVulkanSwapChain.Create(fVulkanDevice,
                                             fVulkanSurface,
                                             OldVulkanSwapChain,
                                             Width,
                                             Height,
                                             IfThen(fVSync,MaxSwapChainImages,1),
                                             1,
                                             VK_FORMAT_UNDEFINED,
                                             VK_COLOR_SPACE_SRGB_NONLINEAR_KHR,
                                             TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT),
                                             VK_SHARING_MODE_EXCLUSIVE,
                                             nil,
                                             VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
                                             TVkPresentModeKHR(integer(IfThen(fVSync,integer(VK_PRESENT_MODE_MAILBOX_KHR),integer(VK_PRESENT_MODE_IMMEDIATE_KHR)))));
   fVulkanSwapChainSimpleDirectRenderTarget:=TVulkanSwapChainSimpleDirectRenderTarget.Create(fVulkanDevice,fVulkanSwapChain,fVulkanInitializationCommandBuffer,fVulkanInitializationCommandBufferFence);
   CreateGraphicsPipelines;
  finally
   OldVulkanSwapChain.Free;
  end;
  VulkanDebugLn('Recreated swap chain... ');

  fCurrentImageIndex:=fVulkanSwapChain.CurrentImageIndex;

 end else begin

  result:=true;

 end;

end;

function TVulkanPresentationSurface.PresentBackBuffer:boolean;
var VulkanCommandBuffer:TVulkanCommandBuffer;
begin

 result:=false;

 VulkanCommandBuffer:=fVulkanCommandBuffers[fCurrentImageIndex];

 VulkanCommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));

 VulkanCommandBuffer.BeginRecording;

 VulkanCommandBuffer.MetaCmdPresentToDrawImageBarrier(fVulkanSwapChain.CurrentImage);

{}
 fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.ClearValues[0].color.float32[0]:=(cos(Now*86400.0*2.0*pi)*0.5)+0.5;
 fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.ClearValues[0].color.float32[1]:=(sin(Now*86400.0*2.0*pi)*0.5)+0.5;
 fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.ClearValues[0].color.float32[2]:=(cos(Now*86400.0*pi*0.731)*0.5)+0.5;{}

{
 fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.ClearValues[0].color.float32[0]:=(cos(Now*86400.0*2.0*pi)*0.5)+0.5;
 fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.ClearValues[0].color.float32[1]:=fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.ClearValues[0].color.float32[0];
 fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.ClearValues[0].color.float32[2]:=fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.ClearValues[0].color.float32[0];{}

 fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.BeginRenderPass(VulkanCommandBuffer,
                                                                     fVulkanSwapChainSimpleDirectRenderTarget.FrameBuffer,
                                                                     VK_SUBPASS_CONTENTS_INLINE,
                                                                     0,0,fVulkanSwapChain.Width,fVulkanSwapChain.Height);
// VulkanCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
// VulkanCommandBuffer.CmdDraw(3,1,0,0);
 fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.EndRenderPass(VulkanCommandBuffer);

 VulkanCommandBuffer.MetaCmdDrawToPresentImageBarrier(fVulkanSwapChain.CurrentImage);

 VulkanCommandBuffer.EndRecording;

 VulkanCommandBuffer.Execute(fVulkanDevice.GraphicsQueue,
                             TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                             fVulkanPresentCompleteSemaphores[fCurrentImageIndex],
                             fVulkanDrawCompleteSemaphores[fCurrentImageIndex],
                             fVulkanCommandBufferFences[fCurrentImageIndex],
                             false);
 fVulkanCommandBufferFencesReady[fCurrentImageIndex]:=true;

 try
  case fVulkanSwapChain.QueuePresent(fVulkanDevice.GraphicsQueue,fVulkanDrawCompleteSemaphores[fCurrentImageIndex]) of
   VK_SUCCESS:begin
    //VulkanDevice.WaitIdle; // A GPU/CPU frame synchronization point only for debug cases here, when something got run wrong
    result:=true;
   end;
   VK_SUBOPTIMAL_KHR:begin
    fDoNeedToRecreateVulkanSwapChain:=true;
   end;
  end;
 except
  on VulkanResultException:EVulkanResultException do begin
   case VulkanResultException.ResultCode of
    VK_ERROR_SURFACE_LOST_KHR,
    VK_ERROR_OUT_OF_DATE_KHR,
    VK_SUBOPTIMAL_KHR:begin
     fDoNeedToRecreateVulkanSwapChain:=true;
    end;
    else begin
     raise;
    end;
   end;
  end;
 end;

end;

initialization
end.
