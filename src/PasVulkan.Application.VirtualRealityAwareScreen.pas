unit PasVulkan.Application.VirtualRealityAwareScreen;
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
     PasMP,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.FrameGraph,
     PasVulkan.Application,
     PasVulkan.Audio,
     PasVulkan.Resources,
     PasVulkan.JSON,
     PasVulkan.Techniques,
     PasVulkan.Scene3D;

const ScreenGUIBaseWidth=2048;

      ScreenGUIBaseHeight=1152;

type PpvApplicationVirtualRealityAwareScreenSceneContentUniformBuffer=^TpvApplicationVirtualRealityAwareScreenSceneContentUniformBuffer;
     TpvApplicationVirtualRealityAwareScreenSceneContentUniformBuffer=packed record
      InverseViewProjectionMatrices:array[0..1] of TpvMatrix4x4;
      Resolution:TpvVector2;
     end;

     TpvApplicationVirtualRealityAwareScreen=class;

     TpvApplicationVirtualRealityAwareScreenContentRenderPass=class(TpvFrameGraph.TRenderPass)
      private
       fParent:TpvApplicationVirtualRealityAwareScreen;
       fVulkanRenderPass:TpvVulkanRenderPass;
       fResourceSurface:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceDepth:TpvFrameGraph.TPass.TUsedImageResource;
      public
       constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TpvApplicationVirtualRealityAwareScreen); reintroduce;
       destructor Destroy; override;
       procedure AcquirePersistentResources; override;
       procedure ReleasePersistentResources; override;
       procedure AcquireVolatileResources; override;
       procedure ReleaseVolatileResources; override;
       procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
       procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
     end;

     TpvApplicationVirtualRealityAwareScreenContentProjectionRenderPass=class(TpvFrameGraph.TRenderPass)
      private
       fVulkanRenderPass:TpvVulkanRenderPass;
       fWidth:TpvSizeInt;
       fHeight:TpvSizeInt;
       fParent:TpvApplicationVirtualRealityAwareScreen;
       fResourceContent:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceDepth:TpvFrameGraph.TPass.TUsedImageResource;
       fVulkanGraphicsCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanGraphicsCommandBufferFence:TpvVulkanFence;
       fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanTransferCommandBufferFence:TpvVulkanFence;
       fVertexShaderModule:TpvVulkanShaderModule;
       fFragmentShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
       fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
       fVulkanUniformBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
       fVulkanPipelineLayout:TpvVulkanPipelineLayout;
       fUniformBuffers:array[0..MaxInFlightFrames-1] of TpvApplicationVirtualRealityAwareScreenSceneContentUniformBuffer;
       fVulkanSampler:TpvVulkanSampler;
      public
       constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TpvApplicationVirtualRealityAwareScreen); reintroduce;
       destructor Destroy; override;
       procedure AcquirePersistentResources; override;
       procedure ReleasePersistentResources; override;
       procedure AcquireVolatileResources; override;
       procedure ReleaseVolatileResources; override;
       procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
       procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
     end;

     TpvApplicationVirtualRealityAwareScreenBlitRenderPass=class(TpvFrameGraph.TRenderPass)
      private
       fParent:TpvApplicationVirtualRealityAwareScreen;
       fVulkanRenderPass:TpvVulkanRenderPass;
       fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceSurface:TpvFrameGraph.TPass.TUsedImageResource;
       fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanTransferCommandBufferFence:TpvVulkanFence;
       fVulkanVertexShaderModule:TpvVulkanShaderModule;
       fVulkanFragmentShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
       fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
       fVulkanSampler:TpvVulkanSampler;
       fVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fVulkanImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
       fVulkanPipelineLayout:TpvVulkanPipelineLayout;
      public
       constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TpvApplicationVirtualRealityAwareScreen); reintroduce;
       destructor Destroy; override;
       procedure AcquirePersistentResources; override;
       procedure ReleasePersistentResources; override;
       procedure AcquireVolatileResources; override;
       procedure ReleaseVolatileResources; override;
       procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
       procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
     end;

     { TpvApplicationVirtualRealityAwareScreen }

     TpvApplicationVirtualRealityAwareScreen=class(TpvApplicationScreen)
      protected

       fFrameGraph:TpvFrameGraph;

       fExternalOutputImageData:TpvFrameGraph.TExternalImageData;

       fContentRenderPass:TpvApplicationVirtualRealityAwareScreenContentRenderPass;

       fContentProjectionRenderPass:TpvApplicationVirtualRealityAwareScreenContentProjectionRenderPass;

       fBlitRenderPass:TpvApplicationVirtualRealityAwareScreenBlitRenderPass;

       fVulkanSampleCountFlagBits:TVkSampleCountFlagBits;

       fVulkanGraphicsCommandPool:TpvVulkanCommandPool;
       fVulkanGraphicsCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanGraphicsCommandBufferFence:TpvVulkanFence;
       fVulkanTransferCommandPool:TpvVulkanCommandPool;
       fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanTransferCommandBufferFence:TpvVulkanFence;
       fVulkanRenderPass:TpvVulkanRenderPass;
       fVulkanCommandPool:TpvVulkanCommandPool;
       fVulkanRenderCommandBuffers:array[0..MaxInFlightFrames-1] of array of TpvVulkanCommandBuffer;

       fVulkanRenderSemaphores:array[0..MaxInFlightFrames-1] of TpvVulkanSemaphore;

       fReady:boolean;
       fSelectedIndex:TpvInt32;
       fStartY:TpvFloat;

       fTime:TpvDouble;

      public

       constructor Create; override;

       destructor Destroy; override;

       procedure Show; override;

       procedure Hide; override;

       procedure Resume; override;

       procedure Pause; override;

       procedure Resize(const aWidth,aHeight:TpvInt32); override;

       procedure AfterCreateSwapChain; override;

       procedure BeforeDestroySwapChain; override;

       procedure AcquirePersistentResources; virtual;
       procedure ReleasePersistentResources; virtual;
       procedure AcquireVolatileResources(const aRenderPass:TpvVulkanRenderPass;const aWidth,aHeight:TpvInt32); virtual;
       procedure ReleaseVolatileResources; virtual;
       procedure ContentUpdate(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); virtual;
       procedure ContentExecute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); virtual;

       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;

       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;

       function Scrolled(const aRelativeAmount:TpvVector2):boolean; override;

       function CanBeParallelProcessed:boolean; override;

       procedure Update(const aDeltaTime:TpvDouble); override;

       procedure Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil); override;

     end;

implementation

uses PasVulkan.Assets,PasVulkan.Streams;

const Offsets:array[0..0] of TVkDeviceSize=(0);

{ TpvApplicationVirtualRealityAwareScreenContentRenderPass }

constructor TpvApplicationVirtualRealityAwareScreenContentRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TpvApplicationVirtualRealityAwareScreen);
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='Content';

 MultiviewMask:=0;

 Queue:=aFrameGraph.UniversalQueue;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.Absolute,ScreenGUIBaseWidth,ScreenGUIBaseHeight);

 fResourceSurface:=AddImageOutput('resourcetype_content_color',
                                  'content_color',
                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                  TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                               TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );

 fResourceDepth:=AddImageDepthOutput('resourcetype_content_depth',
                                     'content_depth',
                                     VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                     TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                  TpvVector4.InlineableCreate(1.0,1.0,1.0,1.0)),
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

end;

destructor TpvApplicationVirtualRealityAwareScreenContentRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvApplicationVirtualRealityAwareScreenContentRenderPass.AcquirePersistentResources;
begin
 inherited AcquirePersistentResources;
 fParent.AcquirePersistentResources;
end;

procedure TpvApplicationVirtualRealityAwareScreenContentRenderPass.ReleasePersistentResources;
begin
 fParent.ReleasePersistentResources;
 inherited ReleasePersistentResources;
end;

procedure TpvApplicationVirtualRealityAwareScreenContentRenderPass.AcquireVolatileResources;
begin
 inherited AcquireVolatileResources;
 fVulkanRenderPass:=VulkanRenderPass;
 fParent.AcquireVolatileResources(fVulkanRenderPass,trunc(PhysicalRenderPass.Size.Size.x),trunc(PhysicalRenderPass.Size.Size.y));
end;

procedure TpvApplicationVirtualRealityAwareScreenContentRenderPass.ReleaseVolatileResources;
begin
 fParent.ReleaseVolatileResources;
 inherited ReleaseVolatileResources;
end;

procedure TpvApplicationVirtualRealityAwareScreenContentRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
 fParent.ContentUpdate(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvApplicationVirtualRealityAwareScreenContentRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
begin
 fParent.ContentExecute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
end;

{ TpvApplicationVirtualRealityAwareScreenContentProjectionRenderPass }

constructor TpvApplicationVirtualRealityAwareScreenContentProjectionRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TpvApplicationVirtualRealityAwareScreen);
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='ContentProjection';

 MultiviewMask:=pvApplication.VirtualReality.MultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       pvApplication.VirtualReality.CountImages);

 fResourceContent:=AddImageInput('resourcetype_content_color',
                                 'content_color',
                                 VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                 []
                                );

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin

  fResourceColor:=AddImageOutput('resourcetype_color',
                                 'scene_color',
                                 VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                              TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

  fResourceDepth:=AddImageDepthOutput('resourcetype_depth',
                                      'scene_depth',
                                      VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                      TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                   TpvVector4.InlineableCreate(1.0,1.0,1.0,1.0)),
                                      [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                     );

 end else begin

  fResourceColor:=AddImageOutput('resourcetype_msaa_color',
                                 'scene_msaa_color',
                                 VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                              TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

  fResourceColor:=AddImageResolveOutput('resourcetype_color',
                                        'scene_color',
                                        'scene_msaa_color',
                                        VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                        TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare,
                                                                     TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                        [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                       );

  fResourceDepth:=AddImageDepthOutput('resourcetype_msaa_depth',
                                      'scene_msaa_depth',
                                      VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                      TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                   TpvVector4.InlineableCreate(1.0,1.0,1.0,1.0)),
                                      [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                     );

 end;

end;

destructor TpvApplicationVirtualRealityAwareScreenContentProjectionRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvApplicationVirtualRealityAwareScreenContentProjectionRenderPass.AcquirePersistentResources;
var x,y:TpvSizeInt;
    Stream:TStream;
begin

 inherited AcquirePersistentResources;

 fVulkanGraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.GraphicsQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanGraphicsCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 begin

  Stream:=TpvDataStream.Create(@VirtualRealityFullscreenVertexSPIRVData,VirtualRealityFullscreenVertexSPIRVDataSize);
  try
   fVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
  finally
   Stream.Free;
  end;

  Stream:=TpvDataStream.Create(@VirtualRealityContentProjectionFragmentSPIRVData,VirtualRealityContentProjectionFragmentSPIRVDataSize);
  try
   fFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
  finally
   Stream.Free;
  end;

  fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVertexShaderModule,'main');

  fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fFragmentShaderModule,'main');

  fVulkanGraphicsPipeline:=nil;

 end;

 fVulkanSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                         0.0,
                                         false,
                                         0.0,
                                         false,
                                         VK_COMPARE_OP_ALWAYS,
                                         0.0,
                                         0.0,
                                         VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK,
                                         false);

end;

procedure TpvApplicationVirtualRealityAwareScreenContentProjectionRenderPass.ReleasePersistentResources;
begin
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fFragmentShaderModule);
 FreeAndNil(fVertexShaderModule);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 FreeAndNil(fVulkanGraphicsCommandBufferFence);
 FreeAndNil(fVulkanGraphicsCommandBuffer);
 FreeAndNil(fVulkanSampler);
 inherited ReleasePersistentResources;
end;

procedure TpvApplicationVirtualRealityAwareScreenContentProjectionRenderPass.AcquireVolatileResources;
var InFlightFrameIndex:TpvSizeInt;
begin
 inherited AcquireVolatileResources;

 fVulkanRenderPass:=VulkanRenderPass;

 fWidth:=trunc(PhysicalRenderPass.Size.Size.x);

 fHeight:=trunc(PhysicalRenderPass.Size.Size.y);

 begin

  for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
   fVulkanUniformBuffers[InFlightFrameIndex]:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                                               SizeOf(TpvApplicationVirtualRealityAwareScreenSceneContentUniformBuffer),
                                                                               TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT),
                                                                               TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                               FrameGraph.QueueFamilyIndices.Items,
                                                                               TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                               0,
                                                                               0,
                                                                               0,
                                                                               0,
                                                                               0,
                                                                               0,
                                                                               0,
                                                                               [TpvVulkanBufferFlag.PersistentMapped]
                                                                              );
   fVulkanUniformBuffers[InFlightFrameIndex].UploadData(FrameGraph.TransferQueue.PhysicalQueue,
                                                                  fVulkanTransferCommandBuffer,
                                                                  fVulkanTransferCommandBufferFence,
                                                                  fUniformBuffers[InFlightFrameIndex],
                                                                  0,
                                                                  SizeOf(TpvApplicationVirtualRealityAwareScreenSceneContentUniformBuffer),
                                                                  TpvVulkanBufferUseTemporaryStagingBufferMode.Yes);
  end;

  fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                        TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                        MaxInFlightFrames);
  fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,MaxInFlightFrames);
  fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,MaxInFlightFrames);
  fVulkanDescriptorPool.Initialize;

  fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
  fVulkanDescriptorSetLayout.AddBinding(0,
                                        VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                        1,
                                        TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                        []);
  fVulkanDescriptorSetLayout.AddBinding(1,
                                        VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                        1,
                                        TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                        []);
  fVulkanDescriptorSetLayout.Initialize;

  for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
   fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                            fVulkanDescriptorSetLayout);
   fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                  0,
                                                                  1,
                                                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                  [],
                                                                  [fVulkanUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                  [],
                                                                  false
                                                                 );
   fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                  0,
                                                                  1,
                                                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                  [TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                                 fResourceContent.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                 fResourceContent.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                  [],
                                                                  [],
                                                                  false
                                                                 );
   fVulkanDescriptorSets[InFlightFrameIndex].Flush;
  end;

  fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
  fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
  fVulkanPipelineLayout.Initialize;

  fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                                        pvApplication.VulkanPipelineCache,
                                                                        0,
                                                                        [],
                                                                        fVulkanPipelineLayout,
                                                                        fVulkanRenderPass,
                                                                        VulkanRenderPassSubpassIndex,
                                                                        nil,
                                                                        0);

  fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageVertex);
  fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageFragment);

  fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
  fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

  fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fWidth,fHeight,0.0,1.0);
  fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fWidth,fHeight);

  fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
  fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
  fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
  fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
 //fMainMenuSceneContentVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
  fVulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_CLOCKWISE;
  fVulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
  fVulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
  fVulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
  fVulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
  fVulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

  fVulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=fParent.fVulkanSampleCountFlagBits;
  fVulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=false;
  fVulkanGraphicsPipeline.MultisampleState.MinSampleShading:=0.0;
  fVulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
  fVulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=false;
  fVulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;

  fVulkanGraphicsPipeline.ColorBlendState.LogicOpEnable:=false;
  fVulkanGraphicsPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
  fVulkanGraphicsPipeline.ColorBlendState.BlendConstants[0]:=0.0;
  fVulkanGraphicsPipeline.ColorBlendState.BlendConstants[1]:=0.0;
  fVulkanGraphicsPipeline.ColorBlendState.BlendConstants[2]:=0.0;
  fVulkanGraphicsPipeline.ColorBlendState.BlendConstants[3]:=0.0;
  fVulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
                                                                                 VK_BLEND_FACTOR_SRC_ALPHA,
                                                                                 VK_BLEND_FACTOR_DST_ALPHA,
                                                                                 VK_BLEND_OP_ADD,
                                                                                 VK_BLEND_FACTOR_ONE,
                                                                                 VK_BLEND_FACTOR_ZERO,
                                                                                 VK_BLEND_OP_ADD,
                                                                                 TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                                 TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                                 TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                                 TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

  fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=false;
  fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=false;
  fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
  fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
  fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

  fVulkanGraphicsPipeline.Initialize;

  fVulkanGraphicsPipeline.FreeMemory;

 end;

end;

procedure TpvApplicationVirtualRealityAwareScreenContentProjectionRenderPass.ReleaseVolatileResources;
var InFlightFrameIndex:TpvSizeInt;
begin

 begin

  FreeAndNil(fVulkanGraphicsPipeline);

  FreeAndNil(fVulkanPipelineLayout);

  for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
   FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
  end;

  FreeAndNil(fVulkanDescriptorSetLayout);

  FreeAndNil(fVulkanDescriptorPool);

  for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
   FreeAndNil(fVulkanUniformBuffers[InFlightFrameIndex]);
  end;

 end;

 fVulkanRenderPass:=nil;

 inherited ReleaseVolatileResources;
end;

procedure TpvApplicationVirtualRealityAwareScreenContentProjectionRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
const ZNear=1.0;
      ZFar=1024.0;
var ModelMatrix,
    ViewMatrix:TpvMatrix4x4;
    ViewMatrices,
    ViewProjectionMatrices,
    InverseViewProjectionMatrices:array[0..1] of TpvMatrix4x4;
    SceneContentUniformBuffer:PpvApplicationVirtualRealityAwareScreenSceneContentUniformBuffer;
begin

 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);

 ViewMatrix:=TpvMatrix4x4.CreateFromQuaternion(TpvQuaternion.Create(0.0,0.0,0.0,-1.0).Normalize);

 ViewMatrices[0]:=ViewMatrix*pvApplication.VirtualReality.GetPositionMatrix(0);
 ViewMatrices[1]:=ViewMatrix*pvApplication.VirtualReality.GetPositionMatrix(1);

 ViewProjectionMatrices[0]:=ViewMatrices[0]*pvApplication.VirtualReality.GetProjectionMatrix(0);
 ViewProjectionMatrices[1]:=ViewMatrices[1]*pvApplication.VirtualReality.GetProjectionMatrix(1);

 InverseViewProjectionMatrices[0]:=ViewProjectionMatrices[0].Inverse;
 InverseViewProjectionMatrices[1]:=ViewProjectionMatrices[1].Inverse;

 SceneContentUniformBuffer:=@fUniformBuffers[aUpdateInFlightFrameIndex];
 SceneContentUniformBuffer^.InverseViewProjectionMatrices[0]:=InverseViewProjectionMatrices[0];
 SceneContentUniformBuffer^.InverseViewProjectionMatrices[1]:=InverseViewProjectionMatrices[1];
 SceneContentUniformBuffer^.Resolution:=TpvVector2.InlineableCreate(fWidth,fHeight);

end;

procedure TpvApplicationVirtualRealityAwareScreenContentProjectionRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var p:pointer;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 begin

  p:=fVulkanUniformBuffers[aInFlightFrameIndex].Memory.MapMemory(0,SizeOf(TpvApplicationVirtualRealityAwareScreenSceneContentUniformBuffer));
  if assigned(p) then begin
   try
    PpvApplicationVirtualRealityAwareScreenSceneContentUniformBuffer(p)^:=fUniformBuffers[aInFlightFrameIndex];
   finally
    fVulkanUniformBuffers[aInFlightFrameIndex].Memory.UnmapMemory;
   end;
  end;

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanPipelineLayout.Handle,0,1,@fVulkanDescriptorSets[aInFlightFrameIndex].Handle,0,nil);
  aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
  if assigned(pvApplication.VulkanDevice.BreadcrumbBuffer) then begin
   pvApplication.VulkanDevice.BreadcrumbBuffer.BeginBreadcrumb(aCommandBuffer.Handle,TpvVulkanBreadcrumbType.Draw,'ScreenGUIBaseSceneContentDraw');
  end;
  aCommandBuffer.CmdDraw(3,1,0,0);
  if assigned(pvApplication.VulkanDevice.BreadcrumbBuffer) then begin
   pvApplication.VulkanDevice.BreadcrumbBuffer.EndBreadcrumb(aCommandBuffer.Handle);
  end;

 end;//}

end;


constructor TpvApplicationVirtualRealityAwareScreenBlitRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TpvApplicationVirtualRealityAwareScreen);
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='BlitRenderPass';

 MultiviewMask:=pvApplication.VirtualReality.MultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

//SeparatePhysicalPass:=true;

//SeparateCommandBuffer:=true;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       pvApplication.VirtualReality.CountImages);

 fResourceColor:=AddImageInput('resourcetype_color',
                               'scene_color',
                               VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                               []
                              );

 fResourceSurface:=AddImageOutput('resourcetype_output_color',
                                  'resource_output',
                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                  TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                               TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment],
                                  TpvFrameGraph.TResourceInstanceType.InstancePerInFlightFrame,
                                  fParent.fExternalOutputImageData
                                 );

end;

destructor TpvApplicationVirtualRealityAwareScreenBlitRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvApplicationVirtualRealityAwareScreenBlitRenderPass.AcquirePersistentResources;
var Stream:TStream;
begin

 inherited AcquirePersistentResources;

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=TpvDataStream.Create(@VirtualRealityFullscreenVertexSPIRVData,VirtualRealityFullscreenVertexSPIRVDataSize);
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=TpvDataStream.Create(@VirtualRealityBlitFragmentSPIRVData,VirtualRealityBlitFragmentSPIRVDataSize);
 try
  fVulkanFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVulkanVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fVulkanFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

 fVulkanSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         0.0,
                                         false,
                                         0.0,
                                         false,
                                         VK_COMPARE_OP_ALWAYS,
                                         0.0,
                                         0.0,
                                         VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK,
                                         false);

end;

procedure TpvApplicationVirtualRealityAwareScreenBlitRenderPass.ReleasePersistentResources;
begin
 FreeAndNil(fVulkanSampler);
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 inherited ReleasePersistentResources;
end;

procedure TpvApplicationVirtualRealityAwareScreenBlitRenderPass.AcquireVolatileResources;
var InFlightFrameIndex,Width,Height:TpvSizeInt;
begin
 inherited AcquireVolatileResources;

 Width:=trunc(PhysicalRenderPass.Size.Size.x);

 Height:=trunc(PhysicalRenderPass.Size.Size.y);

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       MaxInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,MaxInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,MaxInFlightFrames);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(pvApplication.VulkanDevice,
                                                                   fResourceColor.VulkanImages[InFlightFrameIndex],
                                                                   VK_IMAGE_VIEW_TYPE_2D_ARRAY,
                                                                   TpvFrameGraph.TImageResourceType(fResourceColor.ResourceType).Format,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                   0,
                                                                   1,
                                                                   0,
                                                                   pvApplication.VirtualReality.CountImages
                                                                  );
  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                                fResourceColor.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceColor.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                           pvApplication.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           fVulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           VulkanRenderPassSubpassIndex,
                                                           nil,
                                                           0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,Width,Height,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,Width,Height);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
//fMainMenuSceneContentVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
 fVulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_CLOCKWISE;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
 fVulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

 fVulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=VK_SAMPLE_COUNT_1_BIT;
 fVulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=false;
 fVulkanGraphicsPipeline.MultisampleState.MinSampleShading:=0.0;
 fVulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
 fVulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=false;
 fVulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;

 fVulkanGraphicsPipeline.ColorBlendState.LogicOpEnable:=false;
 fVulkanGraphicsPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
 fVulkanGraphicsPipeline.ColorBlendState.BlendConstants[0]:=0.0;
 fVulkanGraphicsPipeline.ColorBlendState.BlendConstants[1]:=0.0;
 fVulkanGraphicsPipeline.ColorBlendState.BlendConstants[2]:=0.0;
 fVulkanGraphicsPipeline.ColorBlendState.BlendConstants[3]:=0.0;
 fVulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_DST_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_ONE,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_OP_ADD,
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

end;

procedure TpvApplicationVirtualRealityAwareScreenBlitRenderPass.ReleaseVolatileResources;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fVulkanImageViews[InFlightFrameIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 fVulkanRenderPass:=nil;

 inherited ReleaseVolatileResources;
end;

procedure TpvApplicationVirtualRealityAwareScreenBlitRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvApplicationVirtualRealityAwareScreenBlitRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanPipelineLayout.Handle,0,1,@fVulkanDescriptorSets[aInFlightFrameIndex].Handle,0,nil);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
 if assigned(pvApplication.VulkanDevice.BreadcrumbBuffer) then begin
  pvApplication.VulkanDevice.BreadcrumbBuffer.BeginBreadcrumb(aCommandBuffer.Handle,TpvVulkanBreadcrumbType.Draw,'ScreenGUIBaseBlitDraw');
 end;
 aCommandBuffer.CmdDraw(3,1,0,0);
 if assigned(pvApplication.VulkanDevice.BreadcrumbBuffer) then begin
  pvApplication.VulkanDevice.BreadcrumbBuffer.EndBreadcrumb(aCommandBuffer.Handle);
 end;
end;

{ TpvApplicationVirtualRealityAwareScreen }

constructor TpvApplicationVirtualRealityAwareScreen.Create;
var SampleCounts:TVkSampleCountFlags;
begin

 inherited Create;

 if assigned(pvApplication.VulkanDevice) then begin

  if assigned(pvApplication.VirtualReality) then begin

   fFrameGraph:=TpvFrameGraph.Create(pvApplication.VulkanDevice);

   fFrameGraph.SurfaceIsSwapchain:=false;

   fFrameGraph.DefaultResourceInstanceType:=TpvFrameGraph.TResourceInstanceType.SingleInstance;

   SampleCounts:=pvApplication.VulkanDevice.PhysicalDevice.Properties.limits.framebufferColorSampleCounts and
                 pvApplication.VulkanDevice.PhysicalDevice.Properties.limits.framebufferDepthSampleCounts and
                 pvApplication.VulkanDevice.PhysicalDevice.Properties.limits.framebufferStencilSampleCounts;

  (*if pvApplication.VulkanDevice.PhysicalDevice.Properties.deviceType=VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU then begin

   {if (SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_64_BIT))<>0 then begin
     fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_64_BIT);
    end else if (SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_32_BIT))<>0 then begin
     fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_32_BIT);
    end else if (SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_16_BIT))<>0 then begin
     fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_16_BIT);
    end else}if (SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_8_BIT))<>0 then begin
     fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_8_BIT);
    end else if (SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_4_BIT))<>0 then begin
     fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_4_BIT);
    end else if (SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_2_BIT))<>0 then begin
     fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_2_BIT);
    end else begin
     fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT);
    end;

   end else begin

    fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT);

   end;*)

   fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT);

   fExternalOutputImageData:=TpvFrameGraph.TExternalImageData.Create(fFrameGraph);

   fFrameGraph.AddImageResourceType('resourcetype_output_color',
                                    true,
                                    pvApplication.VirtualReality.ImageFormat,
                                    TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                    TpvFrameGraph.TImageType.Color,
                                    TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,pvApplication.VirtualReality.CountImages),
                                    TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT),
                                    1
                                   );

   fFrameGraph.AddImageResourceType('resourcetype_msaa_color',
                                    true,
                                    VK_FORMAT_R16G16B16A16_SFLOAT,
                                    fVulkanSampleCountFlagBits,
                                    TpvFrameGraph.TImageType.Color,
                                    TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,pvApplication.VirtualReality.CountImages),
                                    TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                    1
                                   );

   fFrameGraph.AddImageResourceType('resourcetype_msaa_depth',
                                    true,
                                    pvApplication.VulkanDepthImageFormat,
                                    fVulkanSampleCountFlagBits,
                                    TpvFrameGraph.TImageType.From(pvApplication.VulkanDepthImageFormat),
                                    TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,pvApplication.VirtualReality.CountImages),
                                    TVkImageUsageFlags(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT),
                                    1
                                   );

   fFrameGraph.AddImageResourceType('resourcetype_color',
                                    true,
                                    VK_FORMAT_R16G16B16A16_SFLOAT,
                                    TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                    TpvFrameGraph.TImageType.Color,
                                    TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,pvApplication.VirtualReality.CountImages),
                                    TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                    1
                                   );

   fFrameGraph.AddImageResourceType('resourcetype_srgb_color',
                                    true,
                                    VK_FORMAT_R8G8B8A8_SRGB,
                                    TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                    TpvFrameGraph.TImageType.Color,
                                    TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,pvApplication.VirtualReality.CountImages),
                                    TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                    1
                                   );

   fFrameGraph.AddImageResourceType('resourcetype_depth',
                                    true,
                                    pvApplication.VulkanDepthImageFormat,
                                    TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                    TpvFrameGraph.TImageType.From(pvApplication.VulkanDepthImageFormat),
                                    TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,pvApplication.VirtualReality.CountImages),
                                    TVkImageUsageFlags(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT),
                                    1
                                   );

   fFrameGraph.AddImageResourceType('resourcetype_content_color',
                                    true,
                                    VK_FORMAT_R16G16B16A16_SFLOAT,
                                    TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                    TpvFrameGraph.TImageType.Color,
                                    TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.Absolute,ScreenGUIBaseWidth,ScreenGUIBaseHeight),
                                    TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                    1
                                   );

   fFrameGraph.AddImageResourceType('resourcetype_content_depth',
                                    true,
                                    pvApplication.VulkanDepthImageFormat,
                                    TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                    TpvFrameGraph.TImageType.From(pvApplication.VulkanDepthImageFormat),
                                    TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.Absolute,ScreenGUIBaseWidth,ScreenGUIBaseHeight),
                                    TVkImageUsageFlags(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT),
                                    1
                                   );

   fContentRenderPass:=TpvApplicationVirtualRealityAwareScreenContentRenderPass.Create(fFrameGraph,self);

   fContentProjectionRenderPass:=TpvApplicationVirtualRealityAwareScreenContentProjectionRenderPass.Create(fFrameGraph,self);

   fBlitRenderPass:=TpvApplicationVirtualRealityAwareScreenBlitRenderPass.Create(fFrameGraph,self);

   fFrameGraph.RootPass:=fBlitRenderPass;

   fFrameGraph.DoWaitOnSemaphore:=true;

   fFrameGraph.DoSignalSemaphore:=true;

   fFrameGraph.Compile;

  end else begin

   fFrameGraph:=nil;

  end;

 end;

 fSelectedIndex:=-1;
 fReady:=false;

end;

destructor TpvApplicationVirtualRealityAwareScreen.Destroy;
begin
 if assigned(pvApplication.VulkanDevice) then begin
  if assigned(fFrameGraph) then begin
   FreeAndNil(fFrameGraph);
  end;
 end;
 inherited Destroy;
end;

procedure TpvApplicationVirtualRealityAwareScreen.Show;
var Index,SwapChainImageIndex:TpvInt32;
begin
 inherited Show;

 pvApplication.ApplyFPSLimit(false);

 if assigned(pvApplication.VulkanDevice) then begin

  for Index:=0 to MaxInFlightFrames-1 do begin
   fVulkanRenderSemaphores[Index]:=TpvVulkanSemaphore.Create(pvApplication.VulkanDevice);
  end;

  fVulkanGraphicsCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                          pvApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                                          TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

  fVulkanGraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(fVulkanGraphicsCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

  fVulkanGraphicsCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

  if assigned(fFrameGraph) then begin
   fFrameGraph.AcquirePersistentResources;
  end else begin

   fVulkanCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                   pvApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                                   TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
   for Index:=0 to MaxInFlightFrames-1 do begin
    SetLength(fVulkanRenderCommandBuffers[Index],pvApplication.CountSwapChainImages);
    for SwapChainImageIndex:=0 to pvApplication.CountSwapChainImages-1 do begin
     fVulkanRenderCommandBuffers[Index,SwapChainImageIndex]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
    end;
   end;

   fVulkanRenderPass:=nil;

   AcquirePersistentResources;

  end;

 end;

end;

procedure TpvApplicationVirtualRealityAwareScreen.Hide;
var Index,SwapChainImageIndex:TpvInt32;
begin
 if assigned(pvApplication.VulkanDevice) then begin
  if assigned(fFrameGraph) then begin
   fFrameGraph.ReleasePersistentResources;
  end else begin
   ReleasePersistentResources;
   FreeAndNil(fVulkanRenderPass);
   for Index:=0 to MaxInFlightFrames-1 do begin
    for SwapChainImageIndex:=0 to length(fVulkanRenderCommandBuffers[Index])-1 do begin
     FreeAndNil(fVulkanRenderCommandBuffers[Index,SwapChainImageIndex]);
    end;
    fVulkanRenderCommandBuffers[Index]:=nil;
   end;
   FreeAndNil(fVulkanCommandPool);
  end;
  FreeAndNil(fVulkanGraphicsCommandBufferFence);
  FreeAndNil(fVulkanGraphicsCommandBuffer);
  FreeAndNil(fVulkanGraphicsCommandPool);
  for Index:=0 to MaxInFlightFrames-1 do begin
   FreeAndNil(fVulkanRenderSemaphores[Index]);
  end;
 end;
 inherited Hide;
end;

procedure TpvApplicationVirtualRealityAwareScreen.Resume;
begin
 inherited Resume;
end;

procedure TpvApplicationVirtualRealityAwareScreen.Pause;
begin
 inherited Pause;
end;

procedure TpvApplicationVirtualRealityAwareScreen.Resize(const aWidth,aHeight:TpvInt32);
begin
 inherited Resize(aWidth,aHeight);
end;

procedure TpvApplicationVirtualRealityAwareScreen.AfterCreateSwapChain;
var Index,SwapChainImageIndex:TpvSizeInt;
    ColorAttachmentIndex:TpvUInt32;
    DepthAttachmentIndex:TpvUInt32;
begin
 inherited AfterCreateSwapChain;

 if assigned(pvApplication.VulkanDevice) then begin

  if assigned(fFrameGraph) then begin

   fFrameGraph.SetSwapChain(pvApplication.VulkanSwapChain,
                            pvApplication.VulkanDepthImageFormat);

   fFrameGraph.SurfaceWidth:=pvApplication.VirtualReality.Width;
   fFrameGraph.SurfaceHeight:=pvApplication.VirtualReality.Height;

   fExternalOutputImageData.VulkanImages.Clear;
   for Index:=0 to pvApplication.VirtualReality.VulkanImages.Count-1 do begin
    fExternalOutputImageData.VulkanImages.Add(pvApplication.VirtualReality.VulkanImages[Index]);
   end;

   (fFrameGraph.ResourceTypeByName['resourcetype_output_color'] as TpvFrameGraph.TImageResourceType).Format:=pvApplication.VirtualReality.ImageFormat;

   fFrameGraph.AcquireVolatileResources;

  end else begin

   FreeAndNil(fVulkanRenderPass);

   fVulkanRenderPass:=TpvVulkanRenderPass.Create(pvApplication.VulkanDevice);

   // AddAttachmentDescription appends to the render pass and returns the running attachment index, so the
   // order of these two calls decides which index the color and the depth attachment get. Called inline as
   // arguments of AddSubpassDescription that order would be the argument evaluation order of the compiler,
   // which Pascal leaves undefined and which does differ between compilers - and a swap here hands the
   // framebuffer's color image to the depth slot and vice versa. So they are pinned down here, in the same
   // order in which the framebuffers below pass their attachments.
   ColorAttachmentIndex:=fVulkanRenderPass.AddAttachmentDescription(0,
                                                                    pvApplication.VulkanSwapChain.ImageFormat,
                                                                    VK_SAMPLE_COUNT_1_BIT,
                                                                    VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                                    VK_ATTACHMENT_STORE_OP_STORE,
                                                                    VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                    VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                    VK_IMAGE_LAYOUT_UNDEFINED,
                                                                    VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
                                                                   );

   DepthAttachmentIndex:=fVulkanRenderPass.AddAttachmentDescription(0,
                                                                    pvApplication.VulkanDepthImageFormat,
                                                                    VK_SAMPLE_COUNT_1_BIT,
                                                                    VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                                    VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                    VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                    VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                    VK_IMAGE_LAYOUT_UNDEFINED,
                                                                    VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                                   );

   fVulkanRenderPass.AddSubpassDescription(0,
                                           VK_PIPELINE_BIND_POINT_GRAPHICS,
                                           [],
                                           [fVulkanRenderPass.AddAttachmentReference(ColorAttachmentIndex,
                                                                                     VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                                                                                    )],
                                           [],
                                           fVulkanRenderPass.AddAttachmentReference(DepthAttachmentIndex,
                                                                                    VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                                                   ),
                                           []);
   fVulkanRenderPass.AddSubpassDependency(VK_SUBPASS_EXTERNAL,
                                          0,
                                          TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                          TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT),
                                          TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT),
                                          TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT),
                                          TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT));
   fVulkanRenderPass.AddSubpassDependency(0,
                                          VK_SUBPASS_EXTERNAL,
                                          TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT),
                                          TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                          TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT),
                                          TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT),
                                          TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT));

   fVulkanRenderPass.Initialize;

   fVulkanRenderPass.ClearValues[0].color.float32[0]:=0.0;
   fVulkanRenderPass.ClearValues[0].color.float32[1]:=0.0;
   fVulkanRenderPass.ClearValues[0].color.float32[2]:=0.0;
   fVulkanRenderPass.ClearValues[0].color.float32[3]:=1.0;

   for Index:=0 to length(fVulkanRenderCommandBuffers)-1 do begin

    for SwapChainImageIndex:=0 to length(fVulkanRenderCommandBuffers[Index])-1 do begin
     FreeAndNil(fVulkanRenderCommandBuffers[Index,SwapChainImageIndex]);
    end;

    SetLength(fVulkanRenderCommandBuffers[Index],pvApplication.CountSwapChainImages);

    for SwapChainImageIndex:=0 to pvApplication.CountSwapChainImages-1 do begin

     fVulkanRenderCommandBuffers[Index,SwapChainImageIndex]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

    end;

   end;


   AcquireVolatileResources(fVulkanRenderPass,pvApplication.Width,pvApplication.Height);

  end;

 end;

end;

procedure TpvApplicationVirtualRealityAwareScreen.BeforeDestroySwapChain;
begin
 if assigned(pvApplication.VulkanDevice) then begin
  if assigned(fFrameGraph) then begin
   fFrameGraph.ReleaseVolatileResources;
   fExternalOutputImageData.VulkanImages.Clear;
  end else begin
   ReleaseVolatileResources;
   FreeAndNil(fVulkanRenderPass);
  end;
 end;
 inherited BeforeDestroySwapChain;
end;

procedure TpvApplicationVirtualRealityAwareScreen.AcquirePersistentResources;
begin

end;

procedure TpvApplicationVirtualRealityAwareScreen.ReleasePersistentResources;
begin

end;

procedure TpvApplicationVirtualRealityAwareScreen.AcquireVolatileResources(const aRenderPass:TpvVulkanRenderPass;const aWidth,aHeight:TpvInt32);
begin

end;

procedure TpvApplicationVirtualRealityAwareScreen.ReleaseVolatileResources;
begin

end;

procedure TpvApplicationVirtualRealityAwareScreen.ContentUpdate(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin

end;

procedure TpvApplicationVirtualRealityAwareScreen.ContentExecute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
begin

end;

function TpvApplicationVirtualRealityAwareScreen.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=false;
 if fReady and (aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down) then begin
  case aKeyEvent.KeyCode of
   KEYCODE_AC_BACK,KEYCODE_ESCAPE:begin
   // pvApplication.NextScreen:=TpvApplicationVirtualRealityAwareScreen.Create;
//  pvApplication.Terminate;
   end;
   KEYCODE_UP:begin
    if fSelectedIndex<=0 then begin
     fSelectedIndex:=0;
    end else begin
     dec(fSelectedIndex);
    end;
   end;
   KEYCODE_DOWN:begin
    if fSelectedIndex>=0 then begin
     fSelectedIndex:=0;
    end else begin
     inc(fSelectedIndex);
    end;
   end;
   KEYCODE_PAGEUP:begin
    if fSelectedIndex<0 then begin
     fSelectedIndex:=0;
    end;
   end;
   KEYCODE_PAGEDOWN:begin
    if fSelectedIndex<0 then begin
     fSelectedIndex:=0;
    end;
   end;
   KEYCODE_HOME:begin
    fSelectedIndex:=0;
   end;
   KEYCODE_END:begin
    fSelectedIndex:=0
   end;
   KEYCODE_RETURN,KEYCODE_SPACE:begin
    if fSelectedIndex=0 then begin
  //   pvApplication.NextScreen:=TpvApplicationVirtualRealityAwareScreen.Create;
    end;
   end;
  end;
 end;
end;

function TpvApplicationVirtualRealityAwareScreen.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
var Index:TpvInt32;
    cy:TpvFloat;
begin
 result:=false;
{if fReady then begin
  case aPointerEvent.PointerEventType of
   TpvApplicationInputPointerEventType.Down:begin
    fSelectedIndex:=-1;
    cy:=fStartY;
    for Index:=0 to 0 do begin
     if (aPointerEvent.Position.y>=cy) and (aPointerEvent.Position.y<(cy+(Application.TextOverlay.FontCharHeight*FontSize))) then begin
      fSelectedIndex:=Index;
      if fSelectedIndex=0 then begin
       pvApplication.NextScreen:=TpvApplicationVirtualRealityAwareScreen.Create;
      end;
     end;
     cy:=cy+((Application.TextOverlay.FontCharHeight+4)*FontSize);
    end;
   end;
   TpvApplicationInputPointerEventType.Up:begin
   end;
   TpvApplicationInputPointerEventType.Motion:begin
    fSelectedIndex:=-1;
    cy:=fStartY;
    for Index:=0 to 0 do begin
     if (aPointerEvent.Position.y>=cy) and (aPointerEvent.Position.y<(cy+(Application.TextOverlay.FontCharHeight*FontSize))) then begin
      fSelectedIndex:=Index;
     end;
     cy:=cy+((Application.TextOverlay.FontCharHeight+4)*FontSize);
    end;
   end;
   TpvApplicationInputPointerEventType.Drag:begin
   end;
  end;
 end;}
end;

function TpvApplicationVirtualRealityAwareScreen.Scrolled(const aRelativeAmount:TpvVector2):boolean;
begin
 result:=false;
end;

function TpvApplicationVirtualRealityAwareScreen.CanBeParallelProcessed:boolean;
begin
 result:=true;
end;

procedure TpvApplicationVirtualRealityAwareScreen.Update(const aDeltaTime:TpvDouble);
begin
 inherited Update(aDeltaTime);

 if assigned(fFrameGraph) then begin
  fFrameGraph.Update(pvApplication.UpdateInFlightFrameIndex,pvApplication.UpdateFrameCounter);
 end else begin
  ContentUpdate(pvApplication.UpdateInFlightFrameIndex,pvApplication.UpdateFrameCounter);
 end;

 fReady:=true;

 fTime:=fTime+aDeltaTime;

end;

procedure TpvApplicationVirtualRealityAwareScreen.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
var VulkanCommandBuffer:TpvVulkanCommandBuffer;
    VulkanSwapChain:TpvVulkanSwapChain;
begin
 inherited Draw(aSwapChainImageIndex,aWaitSemaphore,nil);
 if assigned(fFrameGraph) then begin
  fFrameGraph.Draw(pvApplication.SwapChainImageIndex,
                   pvApplication.DrawInFlightFrameIndex,
                   pvApplication.DrawFrameCounter,
                   aWaitSemaphore,
                   fVulkanRenderSemaphores[pvApplication.DrawInFlightFrameIndex],
                   aWaitFence);
 end else begin
  if assigned(fVulkanRenderPass) then begin

   VulkanCommandBuffer:=fVulkanRenderCommandBuffers[pvApplication.DrawInFlightFrameIndex,aSwapChainImageIndex];
   VulkanSwapChain:=pvApplication.VulkanSwapChain;

   VulkanCommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));

   VulkanCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));

   fVulkanRenderPass.BeginRenderPass(VulkanCommandBuffer,
                                     pvApplication.VulkanFrameBuffers[aSwapChainImageIndex],
                                     VK_SUBPASS_CONTENTS_INLINE,
                                     0,
                                     0,
                                     VulkanSwapChain.Width,
                                     VulkanSwapChain.Height);
   if assigned(pvApplication.VulkanDevice.BreadcrumbBuffer) then begin
    pvApplication.VulkanDevice.BreadcrumbBuffer.RenderPassHint(true);
   end;

   ContentExecute(VulkanCommandBuffer,
                  pvApplication.DrawInFlightFrameIndex,
                  pvApplication.DrawFrameCounter);

   if assigned(pvApplication.VulkanDevice.BreadcrumbBuffer) then begin
    pvApplication.VulkanDevice.BreadcrumbBuffer.RenderPassHint(false);
   end;
   fVulkanRenderPass.EndRenderPass(VulkanCommandBuffer);

   VulkanCommandBuffer.EndRecording;

   VulkanCommandBuffer.Execute(pvApplication.VulkanDevice.GraphicsQueue,
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                               aWaitSemaphore,
                               fVulkanRenderSemaphores[pvApplication.DrawInFlightFrameIndex],
                               aWaitFence,
                               false);

  end;
 end;

 aWaitSemaphore:=fVulkanRenderSemaphores[pvApplication.DrawInFlightFrameIndex];

end;

end.

