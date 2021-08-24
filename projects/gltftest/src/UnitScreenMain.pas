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
     PasVulkan.Scene3D;

type { TScreenMain }
     TScreenMain=class(TpvApplicationScreen)
      private
       fMeshVertexShaderModule:TpvVulkanShaderModule;
       fMeshFragmentShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStageMeshVertex:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageMeshFragment:TpvVulkanPipelineShaderStage;
       fVulkanRenderPass:TpvVulkanRenderPass;
       fVulkanGraphicsPipelines:TpvScene3D.TGraphicsPipelines;
       fVulkanPipelineLayout:TpvVulkanPipelineLayout;
       fVulkanCommandPool:TpvVulkanCommandPool;
       fVulkanRenderCommandBuffers:array[0..MaxSwapChainImages-1] of TpvVulkanCommandBuffer;
       fVulkanRenderSemaphores:array[0..MaxSwapChainImages-1] of TpvVulkanSemaphore;
       fScene3D:TpvScene3D;
       fGroup:TpvScene3D.TGroup;
       fGroupInstance:TpvScene3D.TGroup.TInstance;
       fTime:Double;
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
  GLTF:=TPasGLTF.TDocument.Create;
  try
   AssetStream:=pvApplication.Assets.GetAssetStream('test.glb');
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

 fTime:=0.0;

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

 fVulkanPipelineShaderStageMeshVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fMeshVertexShaderModule,'main');

 fVulkanPipelineShaderStageMeshFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshFragmentShaderModule,'main');

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddPushConstantRange(TVkPipelineStageFlags(VK_SHADER_STAGE_VERTEX_BIT),0,SizeOf(TpvScene3D.TVertexStagePushConstants));
 fVulkanPipelineLayout.AddDescriptorSetLayout(fScene3D.MeshVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fScene3D.MaterialVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

end;

procedure TScreenMain.Hide;
var Index:TpvInt32;
begin

 fScene3D.Unload;

 FreeAndNil(fVulkanPipelineLayout);

 FreeAndNil(fVulkanPipelineShaderStageMeshVertex);

 FreeAndNil(fVulkanPipelineShaderStageMeshFragment);

 FreeAndNil(fMeshVertexShaderModule);

 FreeAndNil(fMeshFragmentShaderModule);

 FreeAndNil(fVulkanRenderPass);
 for Index:=0 to MaxSwapChainImages-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[Index]);
  FreeAndNil(fVulkanRenderSemaphores[Index]);
 end;
 FreeAndNil(fVulkanCommandPool);

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
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
    VulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
begin
 inherited AfterCreateSwapChain;

 FreeAndNil(fVulkanRenderPass);

 for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
  for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
   FreeAndNil(fVulkanGraphicsPipelines[PrimitiveTopology,DoubleSided]);
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
 fVulkanRenderPass.ClearValues[0].color.float32[3]:=1.0;

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
    VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshFragment);

    VulkanGraphicsPipeline.InputAssemblyState.Topology:=TVkPrimitiveTopology(PrimitiveTopology);
    VulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

    VulkanGraphicsPipeline.VertexInputState.AddVertexInputBindingDescription(0,SizeOf(TpvScene3D.TVertex),VK_VERTEX_INPUT_RATE_VERTEX);
    VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(0,0,VK_FORMAT_R32G32B32_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PVertex(nil)^.Position)));
    VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(1,0,VK_FORMAT_R32_UINT,TVkPtrUInt(pointer(@TpvScene3D.PVertex(nil)^.NodeIndex)));
    VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(2,0,VK_FORMAT_R16G16B16A16_SNORM,TVkPtrUInt(pointer(@TpvScene3D.PVertex(nil)^.TangentSpace)));
    VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(3,0,VK_FORMAT_R32G32_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PVertex(nil)^.TexCoord0)));
    VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(4,0,VK_FORMAT_R32G32_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PVertex(nil)^.TexCoord1)));
    VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(5,0,VK_FORMAT_R16G16B16A16_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PVertex(nil)^.Color0)));
    VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(6,0,VK_FORMAT_R32_UINT,TVkPtrUInt(pointer(@TpvScene3D.PVertex(nil)^.MorphTargetVertexBaseIndex)));
    VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(7,0,VK_FORMAT_R32_UINT,TVkPtrUInt(pointer(@TpvScene3D.PVertex(nil)^.CountMorphTargetVertices)));
    VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(8,0,VK_FORMAT_R32_UINT,TVkPtrUInt(pointer(@TpvScene3D.PVertex(nil)^.JointBlockBaseIndex)));
    VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(9,0,VK_FORMAT_R32_UINT,TVkPtrUInt(pointer(@TpvScene3D.PVertex(nil)^.CountJointBlocks)));

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
    VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_CLOCKWISE;
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

    VulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
    VulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true;
    VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS;
    VulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
    VulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

    VulkanGraphicsPipeline.Initialize;

    VulkanGraphicsPipeline.FreeMemory;

   finally
    fVulkanGraphicsPipelines[PrimitiveTopology,DoubleSided]:=VulkanGraphicsPipeline;
   end;

  end;

 end;

 for SwapChainImageIndex:=0 to length(fVulkanRenderCommandBuffers)-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[SwapChainImageIndex]);
 end;

 for SwapChainImageIndex:=0 to pvApplication.CountSwapChainImages-1 do begin

  fVulkanRenderCommandBuffers[SwapChainImageIndex]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

{ VulkanCommandBuffer:=fVulkanRenderCommandBuffers[SwapChainImageIndex];

  VulkanCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT));

  fVulkanRenderPass.BeginRenderPass(VulkanCommandBuffer,
                                    pvApplication.VulkanFrameBuffers[SwapChainImageIndex],
                                    VK_SUBPASS_CONTENTS_INLINE,
                                    0,
                                    0,
                                    pvApplication.VulkanSwapChain.Width,
                                    pvApplication.VulkanSwapChain.Height);

  fVulkanRenderPass.EndRenderPass(VulkanCommandBuffer);

  VulkanCommandBuffer.EndRecording;
 }
 end;

end;

procedure TScreenMain.BeforeDestroySwapChain;
var PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
begin
 FreeAndNil(fVulkanRenderPass);
 for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
  for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
   FreeAndNil(fVulkanGraphicsPipelines[PrimitiveTopology,DoubleSided]);
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
    CameraRotationX,CameraRotationY,Zoom:TpvScalar;
    t0,t1:Double;
begin
 inherited Draw(aSwapChainImageIndex,aWaitSemaphore,nil);
 if assigned(fVulkanRenderPass) then begin

  ModelMatrix:=TpvMatrix4x4.Identity; // TpvMatrix4x4.CreateRotate(State^.AnglePhases[0]*TwoPI,TpvVector3.Create(0.0,0.0,1.0))*TpvMatrix4x4.CreateRotate(State^.AnglePhases[1]*TwoPI,TpvVector3.Create(0.0,1.0,0.0));

  CameraRotationX:=0.0;
  CameraRotationY:=0.0;
  Center:=(fGroup.BoundingBox.Min+fGroup.BoundingBox.Max)*0.5;
  Bounds:=(fGroup.BoundingBox.Max-fGroup.BoundingBox.Min)*0.5;
  Zoom:=1.0;
  ViewMatrix:=TpvMatrix4x4.CreateLookAt(Center+(TpvVector3.Create(sin(CameraRotationX*PI*2.0)*cos(-CameraRotationY*PI*2.0),
                                                                  sin(-CameraRotationY*PI*2.0),
                                                                  cos(CameraRotationX*PI*2.0)*cos(-CameraRotationY*PI*2.0)).Normalize*
                                                        (Max(Max(Bounds[0],Bounds[1]),Bounds[2])*3.0*Zoom)),
                                        Center,
                                        TpvVector3.Create(0.0,1.0,0.0));

//ViewMatrix:=TpvMatrix4x4.CreateTranslation(0.0,0.0,-6.0);
  ProjectionMatrix:=TpvMatrix4x4.CreatePerspective(45.0,pvApplication.VulkanSwapChain.Width/pvApplication.VulkanSwapChain.Height,1.0,1024.0);

  fGroupInstance.ModelMatrix:=ModelMatrix;

  if fGroupInstance.Group.Animations.Count>0 then begin
   fGroupInstance.Automations[-1].Factor:=0.0;
   fGroupInstance.Automations[-1].Time:=0.0;
   fGroupInstance.Automations[0].Factor:=1.0;
   t0:=fGroupInstance.Group.Animations[0].GetAnimationBeginTime;
   t1:=fGroupInstance.Group.Animations[0].GetAnimationEndTime;
   fGroupInstance.Automations[0].Time:=ModuloPos(fTime,t1-t0)+t0;
  end else begin
   fGroupInstance.Automations[-1].Factor:=1.0;
   fGroupInstance.Automations[-1].Time:=0.0;
  end;

  fScene3D.Update(aSwapChainImageIndex);

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

  fScene3D.Draw(fVulkanGraphicsPipelines,
                aSwapChainImageIndex,
                0,
                ViewMatrix,
                ProjectionMatrix,
                VulkanCommandBuffer,
                fVulkanPipelineLayout,
                [TpvScene3D.TMaterial.TAlphaMode.Opaque,
                 TpvScene3D.TMaterial.TAlphaMode.Blend,
                 TpvScene3D.TMaterial.TAlphaMode.Mask]);

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

end.
