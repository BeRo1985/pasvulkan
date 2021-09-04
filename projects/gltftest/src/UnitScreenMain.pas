unit UnitScreenMain;
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
     PasMP,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Resources,
     PasVulkan.FrameGraph,
     PasVulkan.Scene3D,
     UnitSkyCubeMap,
     UnitGGXBRDF,
     UnitGGXEnvMapCubeMap,
     UnitCharlieBRDF,
     UnitCharlieEnvMapCubeMap,
     UnitLambertianEnvMapCubeMap,
     UnitSkyBox;

type { TScreenMain }
     TScreenMain=class(TpvApplicationScreen)
      public
        type { TForwardRenderingRenderPass }
             TForwardRenderingRenderPass=class(TpvFrameGraph.TRenderPass)
              public
               fVulkanRenderPass:TpvVulkanRenderPass;
               fWidth:TpvSizeInt;
               fHeight:TpvSizeInt;
               fParent:TScreenMain;
               fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceDepth:TpvFrameGraph.TPass.TUsedImageResource;
               fVulkanGraphicsCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanGraphicsCommandBufferFence:TpvVulkanFence;
               fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanTransferCommandBufferFence:TpvVulkanFence;
               fMeshVertexShaderModule:TpvVulkanShaderModule;
               fMeshFragmentShaderModule:TpvVulkanShaderModule;
               fMeshMaskedFragmentShaderModule:TpvVulkanShaderModule;
               fImageBasedLightingVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
               fImageBasedLightingVulkanDescriptorPool:TpvVulkanDescriptorPool;
               fImageBasedLightingVulkanDescriptorSet:TpvVulkanDescriptorSet;
               fVulkanPipelineShaderStageMeshVertex:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshFragment:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshMaskedFragment:TpvVulkanPipelineShaderStage;
               fVulkanGraphicsPipelines:array[TpvScene3D.TMaterial.TAlphaMode] of TpvScene3D.TGraphicsPipelines;
               fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               fSkyBox:TSkyBox;
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateSwapChainImageIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aSwapChainImageIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TTonemappingRenderPass }
             TTonemappingRenderPass=class(TpvFrameGraph.TRenderPass)
               private
                fParent:TScreenMain;
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
                fVulkanDescriptorPool:TpvVulkanDescriptorPool;
                fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
                fVulkanDescriptorSets:array[0..MaxSwapChainImages-1] of TpvVulkanDescriptorSet;
                fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               public
                constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
                destructor Destroy; override;
                procedure Show; override;
                procedure Hide; override;
                procedure AfterCreateSwapChain; override;
                procedure BeforeDestroySwapChain; override;
                procedure Update(const aUpdateSwapChainImageIndex,aUpdateFrameIndex:TpvSizeInt); override;
                procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aSwapChainImageIndex,aFrameIndex:TpvSizeInt); override;
              end;
              { TAntialiasingRenderPass }
              TAntialiasingRenderPass=class(TpvFrameGraph.TRenderPass)
               private
                fParent:TScreenMain;
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
                fVulkanImageViews:array[0..MaxSwapChainImages-1] of TpvVulkanImageView;
                fVulkanDescriptorSets:array[0..MaxSwapChainImages-1] of TpvVulkanDescriptorSet;
                fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               public
                constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
                destructor Destroy; override;
                procedure Show; override;
                procedure Hide; override;
                procedure AfterCreateSwapChain; override;
                procedure BeforeDestroySwapChain; override;
                procedure Update(const aUpdateSwapChainImageIndex,aUpdateFrameIndex:TpvSizeInt); override;
                procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aSwapChainImageIndex,aFrameIndex:TpvSizeInt); override;
              end;
      public
       type TSwapChainImageState=record
             Ready:TPasMPBool32;
             ViewMatrix:TpvMatrix4x4;
             ProjectionMatrix:TpvMatrix4x4;
            end;
            PSwapChainImageState=^TSwapChainImageState;
            TSwapChainImageStates=array[0..MaxSwapChainImages+1] of TSwapChainImageState;
      private
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fVulkanSampleCountFlagBits:TVkSampleCountFlagBits;
       fVulkanGraphicsCommandPool:TpvVulkanCommandPool;
       fVulkanGraphicsCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanGraphicsCommandBufferFence:TpvVulkanFence;
       fVulkanTransferCommandPool:TpvVulkanCommandPool;
       fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanTransferCommandBufferFence:TpvVulkanFence;
       fVulkanRenderSemaphores:array[0..MaxSwapChainImages-1] of TpvVulkanSemaphore;
       fSkyCubeMap:TSkyCubeMap;
       fGGXBRDF:TGGXBRDF;
       fGGXEnvMapCubeMap:TGGXEnvMapCubeMap;
       fCharlieBRDF:TCharlieBRDF;
       fCharlieEnvMapCubeMap:TCharlieEnvMapCubeMap;
       fLambertianEnvMapCubeMap:TLambertianEnvMapCubeMap;
       fSheenELUT:TpvVulkanTexture;
       fScene3D:TpvScene3D;
       fFrameGraph:TpvFrameGraph;
       fForwardRenderingRenderPass:TForwardRenderingRenderPass;
       fTonemappingRenderPass:TTonemappingRenderPass;
       fAntialiasingRenderPass:TAntialiasingRenderPass;
       fGroup:TpvScene3D.TGroup;
       fGroupInstance:TpvScene3D.TGroup.TInstance;
       fTime:Double;
       fCameraRotationX:TpvScalar;
       fCameraRotationY:TpvScalar;
       fZoom:TpvScalar;
       fSwapChainImageStates:TSwapChainImageStates;
       fUpdateLock:TPasMPCriticalSection;
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

       function CanBeParallelProcessed:boolean; override;

       procedure Update(const aDeltaTime:TpvDouble); override;

       function IsReadyForDrawOfSwapChainImageIndex(const aSwapChainImageIndex:TpvInt32):boolean; override;

       procedure Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil); override;

       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;

       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;

       function Scrolled(const aRelativeAmount:TpvVector2):boolean; override;

     end;

implementation

uses PasGLTF;

{ TScreenMain.TForwardRenderingRenderPass }

constructor TScreenMain.TForwardRenderingRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin
inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='ForwardRendering';

 MultiviewMask:=0;

 Queue:=aFrameGraph.UniversalQueue;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0);

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin

  fResourceColor:=AddImageOutput('resourcetype_color',
                                 'forwardrendering_color',
                                 VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                              TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

  fResourceDepth:=AddImageDepthOutput('resourcetype_depth',
                                      'forwardrendering_depth',
                                      VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                      TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                   TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                                      [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                     );

 end else begin

  fResourceColor:=AddImageOutput('resourcetype_msaa_color',
                                 'forwardrendering_msaa_color',
                                 VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                              TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

  fResourceColor:=AddImageResolveOutput('resourcetype_color',
                                        'forwardrendering_color',
                                        'forwardrendering_msaa_color',
                                        VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                        TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare,
                                                                     TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                        [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                       );

  fResourceDepth:=AddImageDepthOutput('resourcetype_msaa_depth',
                                      'forwardrendering_msaa_depth',
                                      VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                      TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                   TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                                      [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                     );

 end;


end;

destructor TScreenMain.TForwardRenderingRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TForwardRenderingRenderPass.Show;
var Stream:TStream;
begin
 inherited Show;

 fVulkanGraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.GraphicsQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanGraphicsCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_vert.spv');
 try
  fMeshVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_frag.spv');
 try
  fMeshFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_masked_frag.spv');
 try
  fMeshMaskedFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageMeshVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fMeshVertexShaderModule,'main');

 fVulkanPipelineShaderStageMeshFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshFragmentShaderModule,'main');

 fVulkanPipelineShaderStageMeshMaskedFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshMaskedFragmentShaderModule,'main');

 fImageBasedLightingVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fImageBasedLightingVulkanDescriptorSetLayout.AddBinding(0,
                                                         VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                         3,
                                                         TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                         []);
 fImageBasedLightingVulkanDescriptorSetLayout.AddBinding(1,
                                                         VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                         3,
                                                         TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                         []);
 fImageBasedLightingVulkanDescriptorSetLayout.Initialize;

 fImageBasedLightingVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),1);
 fImageBasedLightingVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,6);
 fImageBasedLightingVulkanDescriptorPool.Initialize;

 fImageBasedLightingVulkanDescriptorSet:=TpvVulkanDescriptorSet.Create(fImageBasedLightingVulkanDescriptorPool,
                                                                       fImageBasedLightingVulkanDescriptorSetLayout);
 fImageBasedLightingVulkanDescriptorSet.WriteToDescriptorSet(0,
                                                             0,
                                                             3,
                                                             TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                             [fParent.fGGXBRDF.DescriptorImageInfo,
                                                              fParent.fCharlieBRDF.DescriptorImageInfo,
                                                              fParent.fSheenELUT.DescriptorImageInfo],
                                                             [],
                                                             [],
                                                             false);
 fImageBasedLightingVulkanDescriptorSet.WriteToDescriptorSet(1,
                                                             0,
                                                             3,
                                                             TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                             [fParent.fGGXEnvMapCubeMap.DescriptorImageInfo,
                                                              fParent.fCharlieEnvMapCubeMap.DescriptorImageInfo,
                                                              fParent.fLambertianEnvMapCubeMap.DescriptorImageInfo],
                                                             [],
                                                             [],
                                                             false);
 fImageBasedLightingVulkanDescriptorSet.Flush;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddPushConstantRange(TVkPipelineStageFlags(VK_SHADER_STAGE_VERTEX_BIT),0,SizeOf(TpvScene3D.TVertexStagePushConstants));
 fVulkanPipelineLayout.AddDescriptorSetLayout(fParent.fScene3D.MeshVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fParent.fScene3D.MaterialVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fImageBasedLightingVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

end;

procedure TScreenMain.TForwardRenderingRenderPass.Hide;
begin

 FreeAndNil(fVulkanPipelineLayout);

 FreeAndNil(fImageBasedLightingVulkanDescriptorSet);
 FreeAndNil(fImageBasedLightingVulkanDescriptorPool);
 FreeAndNil(fImageBasedLightingVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanPipelineShaderStageMeshVertex);

 FreeAndNil(fVulkanPipelineShaderStageMeshFragment);

 FreeAndNil(fVulkanPipelineShaderStageMeshMaskedFragment);

 FreeAndNil(fMeshVertexShaderModule);

 FreeAndNil(fMeshFragmentShaderModule);

 FreeAndNil(fMeshMaskedFragmentShaderModule);

 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 FreeAndNil(fVulkanGraphicsCommandBufferFence);
 FreeAndNil(fVulkanGraphicsCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TForwardRenderingRenderPass.AfterCreateSwapChain;
var AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
    VulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
begin

 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fWidth:=pvApplication.VulkanSwapChain.Width;

 fHeight:=pvApplication.VulkanSwapChain.Height;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin

  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin

   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin

    VulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                             pvApplication.VulkanPipelineCache,
                                                             0,
                                                             [],
                                                             fVulkanPipelineLayout,
                                                             fVulkanRenderPass,
                                                             0,
                                                             nil,
                                                             0);

    try

     VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshVertex);
     if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask then begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshMaskedFragment);
     end else begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshFragment);
     end;

     VulkanGraphicsPipeline.InputAssemblyState.Topology:=TVkPrimitiveTopology(PrimitiveTopology);
     VulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

     VulkanGraphicsPipeline.VertexInputState.AddVertexInputBindingDescription(0,SizeOf(TpvScene3D.TVertex),VK_VERTEX_INPUT_RATE_VERTEX);
     VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(0,0,VK_FORMAT_R32G32B32_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PVertex(nil)^.Position)));
     VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(1,0,VK_FORMAT_R32_UINT,TVkPtrUInt(pointer(@TpvScene3D.PVertex(nil)^.NodeIndex)));
     VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(2,0,VK_FORMAT_R16G16_SNORM,TVkPtrUInt(pointer(@TpvScene3D.PVertex(nil)^.Normal)));
     VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(3,0,VK_FORMAT_R16G16_SNORM,TVkPtrUInt(pointer(@TpvScene3D.PVertex(nil)^.Tangent)));
     VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(4,0,VK_FORMAT_R32G32_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PVertex(nil)^.TexCoord0)));
     VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(5,0,VK_FORMAT_R32G32_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PVertex(nil)^.TexCoord1)));
     VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(6,0,VK_FORMAT_R16G16B16A16_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PVertex(nil)^.Color0)));
     VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(7,0,VK_FORMAT_R32_UINT,TVkPtrUInt(pointer(@TpvScene3D.PVertex(nil)^.MorphTargetVertexBaseIndex)));
     VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(8,0,VK_FORMAT_R32_UINT,TVkPtrUInt(pointer(@TpvScene3D.PVertex(nil)^.JointBlockBaseIndex)));
     VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(9,0,VK_FORMAT_R32_UINT,TVkPtrUInt(pointer(@TpvScene3D.PVertex(nil)^.CountJointBlocks)));
     VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(10,0,VK_FORMAT_R32_UINT,TVkPtrUInt(pointer(@TpvScene3D.PVertex(nil)^.Flags)));

     VulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,pvApplication.VulkanSwapChain.Width,pvApplication.VulkanSwapChain.Height,0.0,1.0);
     VulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,pvApplication.VulkanSwapChain.Width,pvApplication.VulkanSwapChain.Height);

     VulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
     if DoubleSided then begin
      VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
     end else begin
      VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
     end;
     VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

     VulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=fParent.fVulkanSampleCountFlagBits;
     VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=false;
     VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=0.0;
     VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
     VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=false;
     VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;

     VulkanGraphicsPipeline.ColorBlendState.LogicOpEnable:=false;
     VulkanGraphicsPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[0]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[1]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[2]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[3]:=0.0;
     if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Blend then begin
      VulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(true,
                                                                          VK_BLEND_FACTOR_SRC_ALPHA,
                                                                          VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                          VK_BLEND_OP_ADD,
                                                                          VK_BLEND_FACTOR_ONE,
                                                                          VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                          VK_BLEND_OP_ADD,
                                                                          TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                          TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                          TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                          TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));
     end else begin
      VulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
                                                                          VK_BLEND_FACTOR_ZERO,
                                                                          VK_BLEND_FACTOR_ZERO,
                                                                          VK_BLEND_OP_ADD,
                                                                          VK_BLEND_FACTOR_ZERO,
                                                                          VK_BLEND_FACTOR_ZERO,
                                                                          VK_BLEND_OP_ADD,
                                                                          TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                          TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                          TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                          TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));
     end;

     VulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
     VulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true;//AlphaMode<>TpvScene3D.TMaterial.TAlphaMode.Blend;
     VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_GREATER_OR_EQUAL;
     VulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
     VulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

     VulkanGraphicsPipeline.Initialize;

     VulkanGraphicsPipeline.FreeMemory;

    finally
     fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]:=VulkanGraphicsPipeline;
    end;

   end;

  end;

 end;

 fSkyBox:=TSkyBox.Create(fParent.fSkyCubeMap.DescriptorImageInfo,
                         fVulkanRenderPass,
                         fWidth,
                         fHeight,
                         fParent.fVulkanSampleCountFlagBits);

end;

procedure TScreenMain.TForwardRenderingRenderPass.BeforeDestroySwapChain;
var AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
begin
 FreeAndNil(fSkyBox);
 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;
 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TForwardRenderingRenderPass.Update(const aUpdateSwapChainImageIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateSwapChainImageIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TForwardRenderingRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                          const aSwapChainImageIndex,aFrameIndex:TpvSizeInt);
var SwapChainImageState:TScreenMain.PSwapChainImageState;
begin
 inherited Execute(aCommandBuffer,aSwapChainImageIndex,aFrameIndex);

 SwapChainImageState:=@fParent.fSwapChainImageStates[pvApplication.DrawSwapChainImageIndex];

 if TPasMPInterlocked.CompareExchange(SwapChainImageState^.Ready,false,true) then begin

  fSkyBox.Draw(aCommandBuffer,SwapChainImageState^.ViewMatrix,SwapChainImageState^.ProjectionMatrix);

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                       fVulkanPipelineLayout.Handle,
                                       2,
                                       1,
                                       @fImageBasedLightingVulkanDescriptorSet.Handle,
                                       0,
                                       nil);

  fParent.fScene3D.Prepare(pvApplication.DrawSwapChainImageIndex,
                           0,
                           SwapChainImageState^.ViewMatrix,
                           SwapChainImageState^.ProjectionMatrix,
                           fWidth,
                           fHeight,
                           true);

  fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Opaque],
                        pvApplication.DrawSwapChainImageIndex,
                        0,
                        SwapChainImageState^.ViewMatrix,
                        SwapChainImageState^.ProjectionMatrix,
                        aCommandBuffer,
                        fVulkanPipelineLayout,
                        [TpvScene3D.TMaterial.TAlphaMode.Opaque]);

  fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Mask],
                        pvApplication.DrawSwapChainImageIndex,
                        0,
                        SwapChainImageState^.ViewMatrix,
                        SwapChainImageState^.ProjectionMatrix,
                        aCommandBuffer,
                        fVulkanPipelineLayout,
                        [TpvScene3D.TMaterial.TAlphaMode.Mask]);

  fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Blend],
                        pvApplication.DrawSwapChainImageIndex,
                        0,
                        SwapChainImageState^.ViewMatrix,
                        SwapChainImageState^.ProjectionMatrix,
                        aCommandBuffer,
                        fVulkanPipelineLayout,
                        [TpvScene3D.TMaterial.TAlphaMode.Blend]);

 end;

end;

{ TScreenMain.TTonemappingRenderPass }

constructor TScreenMain.TTonemappingRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='Tonemapping';

 MultiviewMask:=0;

 Queue:=aFrameGraph.UniversalQueue;

//SeparatePhysicalPass:=true;

//SeparateCommandBuffer:=true;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0);

 fResourceColor:=AddImageInput('resourcetype_color',
                               'forwardrendering_color',
                               VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                               [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                              );

 fResourceSurface:=AddImageOutput('resourcetype_color',
                                  'tonemapping_color',
                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                  TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                               TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );

end;

destructor TScreenMain.TTonemappingRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TTonemappingRenderPass.Show;
var Stream:TStream;
begin

 inherited Show;

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/fullscreen_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/tonemapping_frag.spv');
 try
  fVulkanFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVulkanVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fVulkanFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

end;

procedure TScreenMain.TTonemappingRenderPass.Hide;
begin
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TTonemappingRenderPass.AfterCreateSwapChain;
var SwapChainImageIndex:TpvSizeInt;
begin
 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       MaxSwapChainImages);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,MaxSwapChainImages);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 for SwapChainImageIndex:=0 to FrameGraph.CountSwapChainImages-1 do begin
  fVulkanDescriptorSets[SwapChainImageIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                            fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[SwapChainImageIndex].WriteToDescriptorSet(0,
                                                                  0,
                                                                  1,
                                                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                  [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                 fResourceColor.VulkanImageViews[SwapChainImageIndex].Handle,
                                                                                                 fResourceColor.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                  [],
                                                                  [],
                                                                  false
                                                                 );
  fVulkanDescriptorSets[SwapChainImageIndex].Flush;
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

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
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

 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

end;

procedure TScreenMain.TTonemappingRenderPass.BeforeDestroySwapChain;
var SwapChainImageIndex:TpvSizeInt;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for SwapChainImageIndex:=0 to FrameGraph.CountSwapChainImages-1 do begin
  FreeAndNil(fVulkanDescriptorSets[SwapChainImageIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 fVulkanRenderPass:=nil;

 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TTonemappingRenderPass.Update(const aUpdateSwapChainImageIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateSwapChainImageIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TTonemappingRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aSwapChainImageIndex,aFrameIndex:TpvSizeInt);
begin
 inherited Execute(aCommandBuffer,aSwapChainImageIndex,aFrameIndex);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanPipelineLayout.Handle,0,1,@fVulkanDescriptorSets[aSwapChainImageIndex].Handle,0,nil);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
 aCommandBuffer.CmdDraw(3,1,0,0);
end;

{ TScreenMain.TAntialiasingRenderPass }

constructor TScreenMain.TAntialiasingRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='Antialiasing';

 MultiviewMask:=0;

 Queue:=aFrameGraph.UniversalQueue;

//SeparatePhysicalPass:=true;

//SeparateCommandBuffer:=true;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0);

 fResourceColor:=AddImageInput('resourcetype_color',
                               'tonemapping_color',
                               VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                               []
                              );

 fResourceSurface:=AddImageOutput('resourcetype_surface',
                                  'resource_surface',
                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                  TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                               TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );

{fResourceSurface:=AddImageOutput('resourcetype_output_color',
                                  'resource_output',
                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                  TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                               TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment],
                                  TpvFrameGraph.TResourceInstanceType.Default,
                                  fParent.fExternalOutputImageData
                                 );}

end;

destructor TScreenMain.TAntialiasingRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TAntialiasingRenderPass.Show;
var Stream:TStream;
begin

 inherited Show;

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/fullscreen_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/antialiasing_frag.spv');
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

procedure TScreenMain.TAntialiasingRenderPass.Hide;
begin
 FreeAndNil(fVulkanSampler);
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TAntialiasingRenderPass.AfterCreateSwapChain;
var SwapChainImageIndex:TpvSizeInt;
begin
 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       MaxSwapChainImages);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,MaxSwapChainImages);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 for SwapChainImageIndex:=0 to FrameGraph.CountSwapChainImages-1 do begin
  fVulkanImageViews[SwapChainImageIndex]:=TpvVulkanImageView.Create(pvApplication.VulkanDevice,
                                                                    fResourceColor.VulkanImages[SwapChainImageIndex],
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
                                                                    1
                                                                   );
  fVulkanDescriptorSets[SwapChainImageIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                            fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[SwapChainImageIndex].WriteToDescriptorSet(0,
                                                                  0,
                                                                  1,
                                                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                  [TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                                 fResourceColor.VulkanImageViews[SwapChainImageIndex].Handle,
                                                                                                 fResourceColor.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                  [],
                                                                  [],
                                                                  false
                                                                 );
  fVulkanDescriptorSets[SwapChainImageIndex].Flush;
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

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
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

 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

end;

procedure TScreenMain.TAntialiasingRenderPass.BeforeDestroySwapChain;
var SwapChainImageIndex:TpvSizeInt;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for SwapChainImageIndex:=0 to FrameGraph.CountSwapChainImages-1 do begin
  FreeAndNil(fVulkanDescriptorSets[SwapChainImageIndex]);
  FreeAndNil(fVulkanImageViews[SwapChainImageIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 fVulkanRenderPass:=nil;

 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TAntialiasingRenderPass.Update(const aUpdateSwapChainImageIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateSwapChainImageIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TAntialiasingRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aSwapChainImageIndex,aFrameIndex:TpvSizeInt);
begin
 inherited Execute(aCommandBuffer,aSwapChainImageIndex,aFrameIndex);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanPipelineLayout.Handle,0,1,@fVulkanDescriptorSets[aSwapChainImageIndex].Handle,0,nil);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
 aCommandBuffer.CmdDraw(3,1,0,0);
end;

{ TScreenMain }

constructor TScreenMain.Create;
var GLTF:TPasGLTF.TDocument;
    AssetStream:TStream;
    SampleCounts:TVkSampleCountFlags;
begin
 inherited Create;

 fUpdateLock:=TPasMPCriticalSection.Create;

 fScene3D:=TpvScene3D.Create(pvApplication.ResourceManager);

 fGroup:=TpvScene3D.TGroup.Create(pvApplication.ResourceManager,fScene3D);
 try
  fGroup.Culling:=false; // true for GLTFs with large scenes like landscapes, cities, etc.
  GLTF:=TPasGLTF.TDocument.Create;
  try
   AssetStream:=pvApplication.Assets.GetAssetStream('test2.glb');
   if assigned(AssetStream) then begin
    try
     GLTF.LoadFromStream(AssetStream);
    finally
     FreeAndNil(AssetStream);
    end;
   end;
   fGroup.AssignFromGLTF(GLTF);
  finally
   FreeAndNil(GLTF);
  end;
 finally
 end;

 fGroupInstance:=fGroup.CreateInstance;

 fFrameGraph:=TpvFrameGraph.Create(pvApplication.VulkanDevice);

 fFrameGraph.SurfaceIsSwapchain:=true;

 fFrameGraph.DefaultResourceInstanceType:=TpvFrameGraph.TResourceInstanceType.InstancePerSwapChainImage;

 SampleCounts:=pvApplication.VulkanDevice.PhysicalDevice.Properties.limits.framebufferColorSampleCounts and
               pvApplication.VulkanDevice.PhysicalDevice.Properties.limits.framebufferDepthSampleCounts and
               pvApplication.VulkanDevice.PhysicalDevice.Properties.limits.framebufferStencilSampleCounts;

 if pvApplication.VulkanDevice.PhysicalDevice.Properties.deviceType=VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU then begin

 {if (SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_64_BIT))<>0 then begin
   fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_64_BIT);
  end else if (SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_32_BIT))<>0 then begin
   fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_32_BIT);
  end else if (SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_16_BIT))<>0 then begin
   fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_16_BIT);
  end else if (SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_8_BIT))<>0 then begin
   fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_8_BIT);
  end else if (SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_4_BIT))<>0 then begin
   fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_4_BIT);
  end else if (SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_2_BIT))<>0 then begin
   fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_2_BIT);
  end else}begin
   fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT);
  end;

 end else begin

  fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT);

 end;

 fFrameGraph.AddImageResourceType('resourcetype_surface',
                                  true,
                                  TVkFormat(VK_FORMAT_UNDEFINED),
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Surface,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT),
                                  1
                                 );

{[fFrameGraph.AddImageResourceType('resourcetype_output_color',
                                  true,
                                  UnitApplication.Application.VirtualReality.ImageFormat,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,UnitApplication.Application.VirtualReality.CountImages),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT),
                                  1
                                 ); }

 fFrameGraph.AddImageResourceType('resourcetype_msaa_color',
                                  true,
                                  VK_FORMAT_R16G16B16A16_SFLOAT,
                                  fVulkanSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_msaa_depth',
                                  true,
                                  pvApplication.VulkanDepthImageFormat,
                                  fVulkanSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.From(pvApplication.VulkanDepthImageFormat),
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_color',
                                  true,
                                  VK_FORMAT_R16G16B16A16_SFLOAT,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_srgb_color',
                                  true,
                                  VK_FORMAT_R8G8B8A8_SRGB,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_depth',
                                  true,
                                  pvApplication.VulkanDepthImageFormat,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.From(pvApplication.VulkanDepthImageFormat),
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT),
                                  1
                                 );

 fForwardRenderingRenderPass:=TForwardRenderingRenderPass.Create(fFrameGraph,self);

 fTonemappingRenderPass:=TTonemappingRenderPass.Create(fFrameGraph,self);

 fAntialiasingRenderPass:=TAntialiasingRenderPass.Create(fFrameGraph,self);

 fFrameGraph.RootPass:=fAntialiasingRenderPass;

 fFrameGraph.DoWaitOnSemaphore:=true;

 fFrameGraph.DoSignalSemaphore:=true;

 fFrameGraph.Compile;

end;

destructor TScreenMain.Destroy;
begin
 FreeAndNil(fFrameGraph);
 FreeAndNil(fGroupInstance);
 FreeAndNil(fGroup);
 FreeAndNil(fScene3D);
 FreeAndNil(fUpdateLock);
 inherited Destroy;
end;

procedure TScreenMain.Show;
var Index:TpvInt32;
    Stream:TStream;
begin

 inherited Show;

 fSkyCubeMap:=TSkyCubeMap.Create;

 fGGXBRDF:=TGGXBRDF.Create;

 fGGXEnvMapCubeMap:=TGGXEnvMapCubeMap.Create(fSkyCubeMap.DescriptorImageInfo);

 fCharlieBRDF:=TCharlieBRDF.Create;

 fCharlieEnvMapCubeMap:=TCharlieEnvMapCubeMap.Create(fSkyCubeMap.DescriptorImageInfo);

 fLambertianEnvMapCubeMap:=TLambertianEnvMapCubeMap.Create(fSkyCubeMap.DescriptorImageInfo);

 fTime:=0.0;

 fCameraRotationX:=0.0;//frac(fTime*0.03125);

 fCameraRotationY:=0.0;

 fZoom:=1.0;

 fVulkanGraphicsCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                         pvApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                                         TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

 fVulkanGraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(fVulkanGraphicsCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanGraphicsCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fVulkanTransferCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                         pvApplication.VulkanDevice.TransferQueueFamilyIndex,
                                                         TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(fVulkanTransferCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fScene3D.Upload;

 Stream:=pvApplication.Assets.GetAssetStream('textures/sheenelut.png');
 try
  fSheenELUT:=TpvVulkanTexture.CreateFromImage(pvApplication.VulkanDevice,
                                                pvApplication.VulkanDevice.GraphicsQueue,
                                                fVulkanGraphicsCommandBuffer,
                                                fVulkanGraphicsCommandBufferFence,
                                                pvApplication.VulkanDevice.TransferQueue,
                                                fVulkanTransferCommandBuffer,
                                                fVulkanTransferCommandBufferFence,
                                                Stream,
                                                false,
                                                false);
  fSheenELUT.UpdateSampler;
 finally
  FreeAndNil(Stream);
 end;

 FillChar(fSwapChainImageStates,SizeOf(TSwapChainImageStates),#0);

 for Index:=0 to MaxSwapChainImages-1 do begin
  fVulkanRenderSemaphores[Index]:=TpvVulkanSemaphore.Create(pvApplication.VulkanDevice);
 end;

 fFrameGraph.Show;

 pvApplication.SkipNextDrawFrame:=true;

end;

procedure TScreenMain.Hide;
var Index:TpvInt32;
begin

 fFrameGraph.Hide;

 for Index:=0 to MaxSwapChainImages-1 do begin
  FreeAndNil(fVulkanRenderSemaphores[Index]);
 end;

 fScene3D.Unload;

 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 FreeAndNil(fVulkanTransferCommandPool);
 FreeAndNil(fVulkanGraphicsCommandBufferFence);
 FreeAndNil(fVulkanGraphicsCommandBuffer);
 FreeAndNil(fVulkanGraphicsCommandPool);

 FreeAndNil(fSheenELUT);

 FreeAndNil(fCharlieEnvMapCubeMap);

 FreeAndNil(fCharlieBRDF);

 FreeAndNil(fGGXEnvMapCubeMap);

 FreeAndNil(fGGXBRDF);

 FreeAndNil(fLambertianEnvMapCubeMap);

 FreeAndNil(fSkyCubeMap);

 inherited Hide;
end;

procedure TScreenMain.Resume;
begin
 inherited Resume;
 pvApplication.SkipNextDrawFrame:=true;
end;

procedure TScreenMain.Pause;
begin
 inherited Pause;
end;

procedure TScreenMain.Resize(const aWidth,aHeight:TpvInt32);
begin
 inherited Resize(aWidth,aHeight);
 pvApplication.SkipNextDrawFrame:=true;
end;

procedure TScreenMain.AfterCreateSwapChain;
begin
 inherited AfterCreateSwapChain;

 fWidth:=pvApplication.VulkanSwapChain.Width;

 fHeight:=pvApplication.VulkanSwapChain.Height;

//FillChar(fSwapChainImageStates,SizeOf(TSwapChainImageStates),#0);

{}fFrameGraph.SetSwapChain(pvApplication.VulkanSwapChain,
                           pvApplication.VulkanDepthImageFormat);{}

//(fFrameGraph.ResourceTypeByName['resourcetype_output_color'] as TpvFrameGraph.TImageResourceType).Format:=UnitApplication.Application.VirtualReality.ImageFormat;

 fFrameGraph.AfterCreateSwapChain;

 pvApplication.SkipNextDrawFrame:=true;

end;

procedure TScreenMain.BeforeDestroySwapChain;
begin
 fFrameGraph.BeforeDestroySwapChain;
 inherited BeforeDestroySwapChain;
end;

function TScreenMain.CanBeParallelProcessed:boolean;
begin
 result:=true;
end;

procedure TScreenMain.Update(const aDeltaTime:TpvDouble);
var ModelMatrix:TpvMatrix4x4;
    Center,Bounds:TpvVector3;
    t0,t1:Double;
    SwapChainImageState:PSwapChainImageState;
begin
 inherited Update(aDeltaTime);

 SwapChainImageState:=@fSwapChainImageStates[pvApplication.UpdateSwapChainImageIndex];

 begin

  fUpdateLock.Acquire;
  try

   ModelMatrix:=TpvMatrix4x4.Identity; // TpvMatrix4x4.CreateRotate(State^.AnglePhases[0]*TwoPI,TpvVector3.Create(0.0,0.0,1.0))*TpvMatrix4x4.CreateRotate(State^.AnglePhases[1]*TwoPI,TpvVector3.Create(0.0,1.0,0.0));

   fGroupInstance.ModelMatrix:=ModelMatrix;

   if fGroupInstance.Group.Animations.Count>0 then begin
    fGroupInstance.Automations[-1].Factor:=-1.0;
    fGroupInstance.Automations[-1].Time:=0.0;
    if fGroupInstance.Group.Animations.Count>4 then begin
     fGroupInstance.Automations[3].Factor:=1.0;
     t0:=fGroupInstance.Group.Animations[3].GetAnimationBeginTime;
     t1:=fGroupInstance.Group.Animations[3].GetAnimationEndTime;
     fGroupInstance.Automations[3].Time:=ModuloPos(fTime,t1-t0)+t0;
    end else begin
     fGroupInstance.Automations[0].Factor:=1.0;
     t0:=fGroupInstance.Group.Animations[0].GetAnimationBeginTime;
     t1:=fGroupInstance.Group.Animations[0].GetAnimationEndTime;
     fGroupInstance.Automations[0].Time:=ModuloPos(fTime,t1-t0)+t0;
    end;
   end else begin
    fGroupInstance.Automations[-1].Factor:=1.0;
    fGroupInstance.Automations[-1].Time:=0.0;
   end;

   fScene3D.Update(pvApplication.UpdateSwapChainImageIndex);

   Center:=(fGroup.BoundingBox.Min+fGroup.BoundingBox.Max)*0.5;
   Bounds:=(fGroup.BoundingBox.Max-fGroup.BoundingBox.Min)*0.5;
   SwapChainImageState^.ViewMatrix:=TpvMatrix4x4.CreateLookAt(Center+(TpvVector3.Create(sin(fCameraRotationX*PI*2.0)*cos(-fCameraRotationY*PI*2.0),
                                                                                        sin(-fCameraRotationY*PI*2.0),
                                                                                        cos(fCameraRotationX*PI*2.0)*cos(-fCameraRotationY*PI*2.0)).Normalize*
                                                                              (Max(Max(Bounds[0],Bounds[1]),Bounds[2])*2.0*fZoom)),
                                                              Center,
                                                              TpvVector3.Create(0.0,1.0,0.0))*TpvMatrix4x4.FlipYClipSpace;

   SwapChainImageState^.ProjectionMatrix:=TpvMatrix4x4.CreatePerspectiveReversedZ(60.0,pvApplication.VulkanSwapChain.Width/pvApplication.VulkanSwapChain.Height,0.1);

   TPasMPInterlocked.Write(SwapChainImageState^.Ready,true);

   fTime:=fTime+pvApplication.DeltaTime;

  finally
   fUpdateLock.Release;
  end;

 end;

 fFrameGraph.Update(pvApplication.UpdateSwapChainImageIndex,pvApplication.UpdateFrameCounter);

end;

function TScreenMain.IsReadyForDrawOfSwapChainImageIndex(const aSwapChainImageIndex:TpvInt32):boolean;
begin
 result:=TPasMPInterlocked.Read(fSwapChainImageStates[pvApplication.DrawSwapChainImageIndex].Ready);
end;

procedure TScreenMain.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
begin
 inherited Draw(aSwapChainImageIndex,aWaitSemaphore,nil);
 fFrameGraph.Draw(aSwapChainImageIndex,
                  pvApplication.DrawFrameCounter,
                  aWaitSemaphore,
                  fVulkanRenderSemaphores[aSwapChainImageIndex],
                  aWaitFence);
 aWaitSemaphore:=fVulkanRenderSemaphores[aSwapChainImageIndex];
end;

function TScreenMain.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=inherited KeyEvent(aKeyEvent);
 if aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down then begin
  if aKeyEvent.KeyCode=KEYCODE_ESCAPE then begin
   pvApplication.Terminate;
  end;
 end;
end;

function TScreenMain.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
begin
 result:=inherited PointerEvent(aPointerEvent);
 if not result then begin
  if (aPointerEvent.PointerEventType=TpvApplicationInputPointerEventType.Motion) and
     (TpvApplicationInputPointerButton.Left in aPointerEvent.Buttons) then begin
   fUpdateLock.Acquire;
   try
    fCameraRotationX:=frac(fCameraRotationX+(1.0-(aPointerEvent.RelativePosition.x*(1.0/pvApplication.VulkanSwapChain.Width))));
    fCameraRotationY:=frac(fCameraRotationY+(1.0-(aPointerEvent.RelativePosition.y*(1.0/pvApplication.VulkanSwapChain.Height))));
   finally
    fUpdateLock.Release;
   end;
   result:=true;
  end;
 end;
end;

function TScreenMain.Scrolled(const aRelativeAmount:TpvVector2):boolean;
begin
 result:=inherited Scrolled(aRelativeAmount);
 if not result then begin
  fUpdateLock.Acquire;
  try
   fZoom:=Max(1e-4,fZoom+((aRelativeAmount.x+aRelativeAmount.y)*0.1));
  finally
   fUpdateLock.Release;
  end;
  result:=true;
 end;
end;

end.
