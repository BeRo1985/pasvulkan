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
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Resources,
     PasVulkan.Scene3D,
     UnitGGXBRDF,
     UnitSkyCubeMap;

type { TScreenMain }
     TScreenMain=class(TpvApplicationScreen)
      private
       fVulkanGraphicsCommandPool:TpvVulkanCommandPool;
       fVulkanGraphicsCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanGraphicsCommandBufferFence:TpvVulkanFence;
       fVulkanTransferCommandPool:TpvVulkanCommandPool;
       fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanTransferCommandBufferFence:TpvVulkanFence;
       fMeshVertexShaderModule:TpvVulkanShaderModule;
       fMeshFragmentShaderModule:TpvVulkanShaderModule;
       fMeshMaskedFragmentShaderModule:TpvVulkanShaderModule;
       fImageBasedLightingBRDFLUTTexture:TpvVulkanTexture;
       fImageBasedLightingEnvMapTexture:TpvVulkanTexture;
       fImageBasedLightingVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fImageBasedLightingVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fImageBasedLightingVulkanDescriptorSet:TpvVulkanDescriptorSet;
       fVulkanPipelineShaderStageMeshVertex:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageMeshFragment:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageMeshMaskedFragment:TpvVulkanPipelineShaderStage;
       fVulkanRenderPass:TpvVulkanRenderPass;
       fVulkanGraphicsPipelines:array[TpvScene3D.TMaterial.TAlphaMode] of TpvScene3D.TGraphicsPipelines;
       fVulkanPipelineLayout:TpvVulkanPipelineLayout;
       fVulkanCommandPool:TpvVulkanCommandPool;
       fVulkanRenderCommandBuffers:array[0..MaxSwapChainImages-1] of TpvVulkanCommandBuffer;
       fVulkanRenderSemaphores:array[0..MaxSwapChainImages-1] of TpvVulkanSemaphore;
       fGGXBRDF:TGGXBRDF;
       fSkyCubeMap:TSkyCubeMap;
       fScene3D:TpvScene3D;
       fGroup:TpvScene3D.TGroup;
       fGroupInstance:TpvScene3D.TGroup.TInstance;
       fTime:Double;
       fCameraRotationX:TpvScalar;
       fCameraRotationY:TpvScalar;
       fZoom:TpvScalar;
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

       procedure Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil); override;

       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;

       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;

       function Scrolled(const aRelativeAmount:TpvVector2):boolean; override;

     end;

implementation

uses PasGLTF;

constructor TScreenMain.Create;
var GLTF:TPasGLTF.TDocument;
    AssetStream:TStream;
begin
 inherited Create;

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
//fGroup.AssignFromGLTF();
end;

destructor TScreenMain.Destroy;
begin
 FreeAndNil(fGroupInstance);
 FreeAndNil(fGroup);
 FreeAndNil(fScene3D);
 inherited Destroy;
end;

procedure TScreenMain.Show;
var Index:TpvInt32;
    Stream:TStream;
begin

 inherited Show;

 fGGXBRDF:=TGGXBRDF.Create;

 fSkyCubeMap:=TSkyCubeMap.Create;

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

 Stream:=pvApplication.Assets.GetAssetStream('textures/brdflut.png');
 try
  fImageBasedLightingBRDFLUTTexture:=TpvVulkanTexture.CreateFromImage(pvApplication.VulkanDevice,
                                                                      pvApplication.VulkanDevice.GraphicsQueue,
                                                                      fVulkanGraphicsCommandBuffer,
                                                                      fVulkanGraphicsCommandBufferFence,
                                                                      pvApplication.VulkanDevice.TransferQueue,
                                                                      fVulkanTransferCommandBuffer,
                                                                      fVulkanTransferCommandBufferFence,
                                                                      Stream,
                                                                      false,
                                                                      false);
  fImageBasedLightingBRDFLUTTexture.UpdateSampler;
 finally
  FreeAndNil(Stream);
 end;

 Stream:=pvApplication.Assets.GetAssetStream('textures/envmap.jpg');
 try
  fImageBasedLightingEnvMapTexture:=TpvVulkanTexture.CreateFromImage(pvApplication.VulkanDevice,
                                                                     pvApplication.VulkanDevice.GraphicsQueue,
                                                                     fVulkanGraphicsCommandBuffer,
                                                                     fVulkanGraphicsCommandBufferFence,
                                                                     pvApplication.VulkanDevice.TransferQueue,
                                                                     fVulkanTransferCommandBuffer,
                                                                     fVulkanTransferCommandBufferFence,
                                                                     Stream,
                                                                     true,
                                                                     false);
  fImageBasedLightingEnvMapTexture.UpdateSampler;
 finally
  FreeAndNil(Stream);
 end;

 fVulkanCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                 pvApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                                 TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
 for Index:=0 to MaxSwapChainImages-1 do begin
  fVulkanRenderCommandBuffers[Index]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  fVulkanRenderSemaphores[Index]:=TpvVulkanSemaphore.Create(pvApplication.VulkanDevice);
 end;

 fVulkanRenderPass:=nil;

 FillChar(fVulkanGraphicsPipelines,SizeOf(fVulkanGraphicsPipelines),#0);

 fScene3D.Upload;

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
                                                         2,
                                                         TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                         []);
 fImageBasedLightingVulkanDescriptorSetLayout.Initialize;

 fImageBasedLightingVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),1);
 fImageBasedLightingVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,2);
 fImageBasedLightingVulkanDescriptorPool.Initialize;

 fImageBasedLightingVulkanDescriptorSet:=TpvVulkanDescriptorSet.Create(fImageBasedLightingVulkanDescriptorPool,
                                                                       fImageBasedLightingVulkanDescriptorSetLayout);
 fImageBasedLightingVulkanDescriptorSet.WriteToDescriptorSet(0,
                                                             0,
                                                             2,
                                                             TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                             [fImageBasedLightingBRDFLUTTexture.DescriptorImageInfo,
                                                              fImageBasedLightingEnvMapTexture.DescriptorImageInfo],
                                                             [],
                                                             [],
                                                             false);
 fImageBasedLightingVulkanDescriptorSet.Flush;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddPushConstantRange(TVkPipelineStageFlags(VK_SHADER_STAGE_VERTEX_BIT),0,SizeOf(TpvScene3D.TVertexStagePushConstants));
 fVulkanPipelineLayout.AddDescriptorSetLayout(fScene3D.MeshVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fScene3D.MaterialVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fImageBasedLightingVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

end;

procedure TScreenMain.Hide;
var Index:TpvInt32;
begin

 fScene3D.Unload;

 FreeAndNil(fVulkanPipelineLayout);

 FreeAndNil(fImageBasedLightingVulkanDescriptorSet);
 FreeAndNil(fImageBasedLightingVulkanDescriptorPool);
 FreeAndNil(fImageBasedLightingVulkanDescriptorSetLayout);

 FreeAndNil(fImageBasedLightingEnvMapTexture);

 FreeAndNil(fImageBasedLightingBRDFLUTTexture);

 FreeAndNil(fVulkanPipelineShaderStageMeshVertex);

 FreeAndNil(fVulkanPipelineShaderStageMeshFragment);

 FreeAndNil(fVulkanPipelineShaderStageMeshMaskedFragment);

 FreeAndNil(fMeshVertexShaderModule);

 FreeAndNil(fMeshFragmentShaderModule);

 FreeAndNil(fMeshMaskedFragmentShaderModule);

 FreeAndNil(fVulkanRenderPass);
 for Index:=0 to MaxSwapChainImages-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[Index]);
  FreeAndNil(fVulkanRenderSemaphores[Index]);
 end;
 FreeAndNil(fVulkanCommandPool);

 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 FreeAndNil(fVulkanTransferCommandPool);
 FreeAndNil(fVulkanGraphicsCommandBufferFence);
 FreeAndNil(fVulkanGraphicsCommandBuffer);
 FreeAndNil(fVulkanGraphicsCommandPool);

 FreeAndNil(fSkyCubeMap);

 FreeAndNil(fGGXBRDF);

 inherited Hide;
end;

procedure TScreenMain.Resume;
begin
 inherited Resume;
end;

procedure TScreenMain.Pause;
begin
 inherited Pause;
end;

procedure TScreenMain.Resize(const aWidth,aHeight:TpvInt32);
begin
 inherited Resize(aWidth,aHeight);
end;

procedure TScreenMain.AfterCreateSwapChain;
var SwapChainImageIndex:TpvInt32;
    VulkanCommandBuffer:TpvVulkanCommandBuffer;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
    VulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
begin
 inherited AfterCreateSwapChain;

 FreeAndNil(fVulkanRenderPass);

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;

 fVulkanRenderPass:=TpvVulkanRenderPass.Create(pvApplication.VulkanDevice);

 fVulkanRenderPass.AddSubpassDescription(0,
                                         VK_PIPELINE_BIND_POINT_GRAPHICS,
                                         [],
                                         [fVulkanRenderPass.AddAttachmentReference(fVulkanRenderPass.AddAttachmentDescription(0,
                                                                                                                              pvApplication.VulkanSwapChain.ImageFormat,
                                                                                                                              VK_SAMPLE_COUNT_1_BIT,
                                                                                                                              VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                                                                                              VK_ATTACHMENT_STORE_OP_STORE,
                                                                                                                              VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                                                                              VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                              VK_IMAGE_LAYOUT_UNDEFINED, //VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, //VK_IMAGE_LAYOUT_UNDEFINED, // VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                                                                                                              VK_IMAGE_LAYOUT_PRESENT_SRC_KHR //VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL //VK_IMAGE_LAYOUT_PRESENT_SRC_KHR  // VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                                                                                                                             ),
                                                                             VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                                                                            )],
                                         [],
                                         fVulkanRenderPass.AddAttachmentReference(fVulkanRenderPass.AddAttachmentDescription(0,
                                                                                                                             pvApplication.VulkanDepthImageFormat,
                                                                                                                             VK_SAMPLE_COUNT_1_BIT,
                                                                                                                             VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                                                                                             VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                             VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                                                                             VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                             VK_IMAGE_LAYOUT_UNDEFINED, //VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, // VK_IMAGE_LAYOUT_UNDEFINED, // VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                                                                                                             VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                                                                                            ),
                                                                                  VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                                                 ),
                                         []);
 fVulkanRenderPass.AddSubpassDependency(VK_SUBPASS_EXTERNAL,
                                        0,
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                        TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT),
                                        TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
                                        TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT));
 fVulkanRenderPass.AddSubpassDependency(0,
                                        VK_SUBPASS_EXTERNAL,
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                        TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
                                        TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT),
                                        TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT));
 fVulkanRenderPass.Initialize;

 fVulkanRenderPass.ClearValues[0].color.float32[0]:=0.0;
 fVulkanRenderPass.ClearValues[0].color.float32[1]:=0.0;
 fVulkanRenderPass.ClearValues[0].color.float32[2]:=0.0;
 fVulkanRenderPass.ClearValues[0].color.float32[3]:=0.0;
 fVulkanRenderPass.ClearValues[1].depthStencil.depth:=0.0;

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

     VulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=VK_SAMPLE_COUNT_1_BIT;
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

 for SwapChainImageIndex:=0 to length(fVulkanRenderCommandBuffers)-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[SwapChainImageIndex]);
 end;

 for SwapChainImageIndex:=0 to pvApplication.CountSwapChainImages-1 do begin

  fVulkanRenderCommandBuffers[SwapChainImageIndex]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 end;

end;

procedure TScreenMain.BeforeDestroySwapChain;
var AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
begin
 FreeAndNil(fVulkanRenderPass);
 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;
 inherited BeforeDestroySwapChain;
end;

function TScreenMain.CanBeParallelProcessed:boolean;
begin
 result:=true;
end;

procedure TScreenMain.Update(const aDeltaTime:TpvDouble);
begin
 inherited Update(aDeltaTime);
end;

procedure TScreenMain.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
var VulkanCommandBuffer:TpvVulkanCommandBuffer;
    ModelMatrix:TpvMatrix4x4;
    ViewMatrix:TpvMatrix4x4;
    ProjectionMatrix:TpvMatrix4x4;
    Center,Bounds:TpvVector3;
    t0,t1:Double;
begin
 inherited Draw(aSwapChainImageIndex,aWaitSemaphore,nil);
 if assigned(fVulkanRenderPass) then begin

  ModelMatrix:=TpvMatrix4x4.Identity; // TpvMatrix4x4.CreateRotate(State^.AnglePhases[0]*TwoPI,TpvVector3.Create(0.0,0.0,1.0))*TpvMatrix4x4.CreateRotate(State^.AnglePhases[1]*TwoPI,TpvVector3.Create(0.0,1.0,0.0));

  fGroupInstance.ModelMatrix:=ModelMatrix;

  if fGroupInstance.Group.Animations.Count>0 then begin
   fGroupInstance.Automations[-1].Factor:=0.0;
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

  fScene3D.Update(aSwapChainImageIndex);

  Center:=(fGroup.BoundingBox.Min+fGroup.BoundingBox.Max)*0.5;
  Bounds:=(fGroup.BoundingBox.Max-fGroup.BoundingBox.Min)*0.5;
  ViewMatrix:=TpvMatrix4x4.CreateLookAt(Center+(TpvVector3.Create(sin(fCameraRotationX*PI*2.0)*cos(-fCameraRotationY*PI*2.0),
                                                                  sin(-fCameraRotationY*PI*2.0),
                                                                  cos(fCameraRotationX*PI*2.0)*cos(-fCameraRotationY*PI*2.0)).Normalize*
                                                        (Max(Max(Bounds[0],Bounds[1]),Bounds[2])*2.0*fZoom)),
                                        Center,
                                        TpvVector3.Create(0.0,1.0,0.0))*TpvMatrix4x4.FlipYClipSpace;

  ProjectionMatrix:=TpvMatrix4x4.CreatePerspectiveReversedZ(60.0,pvApplication.VulkanSwapChain.Width/pvApplication.VulkanSwapChain.Height,0.1);

  VulkanCommandBuffer:=fVulkanRenderCommandBuffers[aSwapChainImageIndex];

  VulkanCommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));

  VulkanCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));

  fVulkanRenderPass.BeginRenderPass(VulkanCommandBuffer,
                                    pvApplication.VulkanFrameBuffers[aSwapChainImageIndex],
                                    VK_SUBPASS_CONTENTS_INLINE,
                                    0,
                                    0,
                                    pvApplication.VulkanSwapChain.Width,
                                    pvApplication.VulkanSwapChain.Height);

  VulkanCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                            fVulkanPipelineLayout.Handle,
                                            2,
                                            1,
                                            @fImageBasedLightingVulkanDescriptorSet.Handle,
                                            0,
                                            nil);

  fScene3D.Prepare(aSwapChainImageIndex,
                   0,
                   ViewMatrix,
                   ProjectionMatrix,
                   true);

  fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Opaque],
                aSwapChainImageIndex,
                0,
                ViewMatrix,
                ProjectionMatrix,
                VulkanCommandBuffer,
                fVulkanPipelineLayout,
                [TpvScene3D.TMaterial.TAlphaMode.Opaque]);

  fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Mask],
                aSwapChainImageIndex,
                0,
                ViewMatrix,
                ProjectionMatrix,
                VulkanCommandBuffer,
                fVulkanPipelineLayout,
                [TpvScene3D.TMaterial.TAlphaMode.Mask]);

  fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Blend],
                aSwapChainImageIndex,
                0,
                ViewMatrix,
                ProjectionMatrix,
                VulkanCommandBuffer,
                fVulkanPipelineLayout,
                [TpvScene3D.TMaterial.TAlphaMode.Blend]);

  fVulkanRenderPass.EndRenderPass(VulkanCommandBuffer);

  VulkanCommandBuffer.EndRecording;

  VulkanCommandBuffer.Execute(pvApplication.VulkanDevice.GraphicsQueue,
                              TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                              aWaitSemaphore,
                              fVulkanRenderSemaphores[aSwapChainImageIndex],
                              aWaitFence,
                              false);

  aWaitSemaphore:=fVulkanRenderSemaphores[aSwapChainImageIndex];

  fTime:=fTime+pvApplication.DeltaTime;

 end;
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
   fCameraRotationX:=frac(fCameraRotationX+(1.0-(aPointerEvent.RelativePosition.x*(1.0/pvApplication.VulkanSwapChain.Width))));
   fCameraRotationY:=frac(fCameraRotationY+(1.0-(aPointerEvent.RelativePosition.y*(1.0/pvApplication.VulkanSwapChain.Height))));
   result:=true;
  end;
 end;
end;

function TScreenMain.Scrolled(const aRelativeAmount:TpvVector2):boolean;
begin
 result:=inherited Scrolled(aRelativeAmount);
 if not result then begin
  fZoom:=Max(1e-4,fZoom+((aRelativeAmount.x+aRelativeAmount.y)*0.1));
  result:=true;
 end;
end;

end.
