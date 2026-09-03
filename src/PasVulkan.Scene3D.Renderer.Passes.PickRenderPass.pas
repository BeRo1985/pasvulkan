(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2026, Benjamin Rosseaux (benjamin@rosseaux.de)          *
 *                                                                            *
 ******************************************************************************)
unit PasVulkan.Scene3D.Renderer.Passes.PickRenderPass;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.FrameGraph,
     PasVulkan.Scene3D,
     PasVulkan.Scene3D.Renderer.Globals,
     PasVulkan.Scene3D.Renderer,
     PasVulkan.Scene3D.Renderer.Instance;

type { TpvScene3DRendererPassesPickRenderPass }
     // Object picking: draws the scene once into a single channel uint target, writing the mesh object
     // id of whatever is frontmost per pixel, so that what is under the cursor can be read back from
     // one pixel instead of being guessed from collision shapes.
     //
     // Why this draws for itself instead of reading the renderer's depth buffer, which would have been
     // free: that one is multisampled as soon as MSAA is on, and a multisampled image cannot be copied
     // out at all; and transparent surfaces never reach it, so a pane of glass would pick whatever
     // stands behind it. Its own target is single sampled and it draws blended materials as well.
     //
     // It runs only in the frame a pick was actually asked for (TpvScene3DRendererInstance.RequestPick,
     // which switches this pass and its read back on for exactly one frame). One extra scene pass per
     // click costs nothing anybody can feel, which is why there is no ray filtered draw list here: the
     // normal cull output is drawn, and that is the geometry that is visible anyway.
     TpvScene3DRendererPassesPickRenderPass=class(TpvFrameGraph.TRenderPass)
      private
       fOnSetRenderPassResourcesDone:boolean;
       procedure OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                          const aPipelineLayout:TpvVulkanPipelineLayout;
                                          const aRendererInstance:TObject;
                                          const aRenderPass:TpvScene3DRendererRenderPass;
                                          const aPreviousInFlightFrameIndex:TpvSizeInt;
                                          const aInFlightFrameIndex:TpvSizeInt);
      private
       fInstance:TpvScene3DRendererInstance;
       fResourceID:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceDepth:TpvFrameGraph.TPass.TUsedImageResource;
       fVulkanRenderPass:TpvVulkanRenderPass;
       fMeshVertexShaderModule:TpvVulkanShaderModule;
       fMeshFragmentShaderModule:TpvVulkanShaderModule;
       fMeshMaskedFragmentShaderModule:TpvVulkanShaderModule;
       fGlobalVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fGlobalVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fGlobalVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
       fVulkanPipelineShaderStageMeshVertex:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageMeshFragment:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageMeshMaskedFragment:TpvVulkanPipelineShaderStage;
       fMeshTaskShaderModule:TpvVulkanShaderModule;
       fMeshMeshShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStageMeshTask:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageMeshMesh:TpvVulkanPipelineShaderStage;
       fMeshShader:Boolean;
       fMeshShaderGraphicsPipelines:array[TpvScene3D.TMaterial.TAlphaMode] of TpvScene3D.TGraphicsPipelines;
       fVulkanGraphicsPipelines:array[TpvScene3D.TMaterial.TAlphaMode] of TpvScene3D.TGraphicsPipelines;
       fVulkanPipelineLayout:TpvVulkanPipelineLayout;
      public
       constructor Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance); reintroduce;
       destructor Destroy; override;
       procedure AcquirePersistentResources; override;
       procedure ReleasePersistentResources; override;
       procedure AcquireVolatileResources; override;
       procedure ReleaseVolatileResources; override;
       procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
     end;

implementation

{ TpvScene3DRendererPassesPickRenderPass }

constructor TpvScene3DRendererPassesPickRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin

 inherited Create(aFrameGraph);

 fInstance:=aInstance;

 Name:='PickRenderPass';

 MultiviewMask:=fInstance.SurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       fInstance.SizeFactor,
                                       fInstance.SizeFactor,
                                       1.0,
                                       fInstance.CountSurfaceViews);

 // Cleared to zero, which is the "nothing here" id: mesh object ids are handed out from one upwards,
 // so a read back zero says the cursor was over nothing without needing a second flag.
 fResourceID:=AddImageOutput('resourcetype_pick_id',
                             'resource_pick_id',
                             VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                             TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                          TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                             [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                            );

 fResourceDepth:=AddImageDepthOutput('resourcetype_pick_id_depth',
                                     'resource_pick_id_depth',
                                     VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                     TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                  // reverse-Z: clear depth to the far value (0.0 when ZFar<0)
                                                                  TpvVector4.InlineableCreate(IfThen(fInstance.ZFar<0.0,0.0,1.0),0.0,0.0,0.0)),
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

end;

destructor TpvScene3DRendererPassesPickRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesPickRenderPass.AcquirePersistentResources;
var Stream:TStream;
    MeshFragmentSpecializationConstants:TpvScene3DRendererInstance.TMeshFragmentSpecializationConstants;
begin

 inherited AcquirePersistentResources;

 fMeshShader:=fInstance.Renderer.Scene3D.MeshShaders;

 MeshFragmentSpecializationConstants:=fInstance.MeshFragmentSpecializationConstants;

 // Own vertex and mesh shader variants, unlike the selection mask pass which can reuse the plain
 // ones: the picking variants put the mesh object id into the flat output where the others put the
 // instance data index, because several objects can share one instance data slot.
 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_picking_vert.spv');
 try
  fMeshVertexShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_'+fInstance.Renderer.MeshFragTypeName+'picking_frag.spv');
 try
  fMeshFragmentShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fInstance.Renderer.UseDemote then begin
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_'+fInstance.Renderer.MeshFragTypeName+'picking_alphatest_demote_frag.spv');
 end else if fInstance.Renderer.UseNoDiscard then begin
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_'+fInstance.Renderer.MeshFragTypeName+'picking_alphatest_nodiscard_frag.spv');
 end else begin
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_'+fInstance.Renderer.MeshFragTypeName+'picking_alphatest_frag.spv');
 end;
 try
  fMeshMaskedFragmentShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageMeshVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fMeshVertexShaderModule,'main');

 fVulkanPipelineShaderStageMeshFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshFragmentShaderModule,'main');
 MeshFragmentSpecializationConstants.SetPipelineShaderStage(fVulkanPipelineShaderStageMeshFragment);

 fVulkanPipelineShaderStageMeshMaskedFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshMaskedFragmentShaderModule,'main');
 MeshFragmentSpecializationConstants.SetPipelineShaderStage(fVulkanPipelineShaderStageMeshMaskedFragment);

 if fMeshShader then begin

  if not fInstance.Renderer.UseMeshletExpand then begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_task_pass0.spv');
   try
    fMeshTaskShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
   finally
    Stream.Free;
   end;
   fVulkanPipelineShaderStageMeshTask:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_TASK_BIT_EXT,fMeshTaskShaderModule,'main');
  end else begin
   fMeshTaskShaderModule:=nil;
   fVulkanPipelineShaderStageMeshTask:=nil;
  end;

  if fInstance.Renderer.UseMeshletExpand then begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_picking_notask_mesh.spv');
  end else begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_picking_mesh.spv');
  end;
  try
   fMeshMeshShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
  finally
   Stream.Free;
  end;

  fVulkanPipelineShaderStageMeshMesh:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_MESH_BIT_EXT,fMeshMeshShaderModule,'main');

 end else begin
  fMeshTaskShaderModule:=nil;
  fMeshMeshShaderModule:=nil;
  fVulkanPipelineShaderStageMeshTask:=nil;
  fVulkanPipelineShaderStageMeshMesh:=nil;
 end;

end;

procedure TpvScene3DRendererPassesPickRenderPass.ReleasePersistentResources;
begin

 FreeAndNil(fVulkanPipelineShaderStageMeshVertex);

 FreeAndNil(fVulkanPipelineShaderStageMeshFragment);

 FreeAndNil(fVulkanPipelineShaderStageMeshMaskedFragment);

 FreeAndNil(fMeshVertexShaderModule);

 FreeAndNil(fMeshFragmentShaderModule);

 FreeAndNil(fMeshMaskedFragmentShaderModule);

 FreeAndNil(fVulkanPipelineShaderStageMeshTask);
 FreeAndNil(fVulkanPipelineShaderStageMeshMesh);
 FreeAndNil(fMeshTaskShaderModule);
 FreeAndNil(fMeshMeshShaderModule);

 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesPickRenderPass.AcquireVolatileResources;
var InFlightFrameIndex:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    FaceCullingMode:TpvScene3D.TFaceCullingMode;
    VulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
begin

 inherited AcquireVolatileResources;

 fVulkanRenderPass:=VulkanRenderPass;

 fMeshShader:=fInstance.Renderer.Scene3D.MeshShaders;

 fGlobalVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fInstance.Renderer.VulkanDevice);
 if fMeshShader then begin
  fGlobalVulkanDescriptorSetLayout.AddBinding(0,
                                              VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                              1,
                                              TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT) or TVkShaderStageFlags(VK_SHADER_STAGE_TASK_BIT_EXT) or TVkShaderStageFlags(VK_SHADER_STAGE_MESH_BIT_EXT),
                                              []);
 end else begin
  fGlobalVulkanDescriptorSetLayout.AddBinding(0,
                                              VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                              1,
                                              TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                              []);
 end;
 fGlobalVulkanDescriptorSetLayout.Initialize;

 fGlobalVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(fInstance.Renderer.VulkanDevice,TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),fInstance.Renderer.CountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,1*fInstance.Renderer.CountInFlightFrames);
 fGlobalVulkanDescriptorPool.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fGlobalVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fGlobalVulkanDescriptorPool,
                                                                                 fGlobalVulkanDescriptorSetLayout);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                       [],
                                                                       [fInstance.VulkanViewUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(fInstance.Renderer.VulkanDevice);
 if fMeshShader then begin
  fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT) or TVkShaderStageFlags(VK_SHADER_STAGE_TASK_BIT_EXT) or TVkShaderStageFlags(VK_SHADER_STAGE_MESH_BIT_EXT),0,SizeOf(TpvScene3D.TMeshStagePushConstants));
 end else begin
  fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),0,SizeOf(TpvScene3D.TMeshStagePushConstants));
 end;
 fVulkanPipelineLayout.AddDescriptorSetLayout(fInstance.Renderer.Scene3D.GlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for FaceCullingMode:=Low(TpvScene3D.TFaceCullingMode) to High(TpvScene3D.TFaceCullingMode) do begin
    fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,FaceCullingMode]:=nil;
    fMeshShaderGraphicsPipelines[AlphaMode,PrimitiveTopology,FaceCullingMode]:=nil;
   end;
  end;
 end;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin

  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin

   for FaceCullingMode:=Low(TpvScene3D.TFaceCullingMode) to High(TpvScene3D.TFaceCullingMode) do begin

    VulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(fInstance.Renderer.VulkanDevice,
                                                             fInstance.Renderer.VulkanPipelineCache,
                                                             0,
                                                             [],
                                                             fVulkanPipelineLayout,
                                                             fVulkanRenderPass,
                                                             VulkanRenderPassSubpassIndex,
                                                             nil,
                                                             0);

    try

     VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshVertex);
     // Every alpha mode needs its fragment stage here, unlike a depth prepass which can leave it out
     // for opaque geometry: an id has to be written, and nothing else writes it.
     if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask then begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshMaskedFragment);
     end else begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshFragment);
     end;

     VulkanGraphicsPipeline.InputAssemblyState.Topology:=TpvScene3D.VulkanPrimitiveTopologies[PrimitiveTopology];
     VulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

     fInstance.Renderer.Scene3D.InitializeGraphicsPipeline(VulkanGraphicsPipeline,false);

     VulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fResourceID.Width,fResourceID.Height,0.0,1.0);
     VulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fResourceID.Width,fResourceID.Height);

     VulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
     case FaceCullingMode of
      TpvScene3D.TFaceCullingMode.Normal:begin
       VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
       VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
      end;
      TpvScene3D.TFaceCullingMode.Inversed:begin
       VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
       VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_CLOCKWISE;
      end;
      else begin
       VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
       VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
      end;
     end;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

     // Single sampled whatever the scene runs at, see the class comment.
     VulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT);
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
     // No blending on an id: the frontmost surface wins outright, which is what the depth test decides.
     VulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
                                                                         VK_BLEND_FACTOR_ZERO,
                                                                         VK_BLEND_FACTOR_ZERO,
                                                                         VK_BLEND_OP_ADD,
                                                                         VK_BLEND_FACTOR_ZERO,
                                                                         VK_BLEND_FACTOR_ZERO,
                                                                         VK_BLEND_OP_ADD,
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT));

     // Depth written for every alpha mode, transparent ones included: a pick asks what one is
     // pointing at, and a pane of glass in front of a wall is the pane. That is the one place where
     // this deliberately differs from how the scene itself is drawn.
     VulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
     VulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true;
     if fInstance.ZFar<0.0 then begin
      VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_GREATER_OR_EQUAL;
     end else begin
      VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
     end;
     VulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
     VulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

     VulkanGraphicsPipeline.Initialize;

     VulkanGraphicsPipeline.FreeMemory;

    finally
     fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,FaceCullingMode]:=VulkanGraphicsPipeline;
    end;

   end;

  end;

 end;

 if fMeshShader and assigned(fVulkanPipelineShaderStageMeshMesh) then begin

  for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin

   for FaceCullingMode:=Low(TpvScene3D.TFaceCullingMode) to High(TpvScene3D.TFaceCullingMode) do begin

    VulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(fInstance.Renderer.VulkanDevice,
                                                             fInstance.Renderer.VulkanPipelineCache,
                                                             0,
                                                             [],
                                                             fVulkanPipelineLayout,
                                                             fVulkanRenderPass,
                                                             VulkanRenderPassSubpassIndex,
                                                             nil,
                                                             0);

    try

     if assigned(fVulkanPipelineShaderStageMeshTask) then begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshTask);
     end;
     VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshMesh);
     if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask then begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshMaskedFragment);
     end else begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshFragment);
     end;

     VulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
     VulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

     fInstance.Renderer.Scene3D.InitializeGraphicsPipeline(VulkanGraphicsPipeline,false);

     VulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fResourceID.Width,fResourceID.Height,0.0,1.0);
     VulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fResourceID.Width,fResourceID.Height);

     VulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
     case FaceCullingMode of
      TpvScene3D.TFaceCullingMode.Normal:begin
       VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
       VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
      end;
      TpvScene3D.TFaceCullingMode.Inversed:begin
       VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
       VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_CLOCKWISE;
      end;
      else begin
       VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
       VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
      end;
     end;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

     VulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT);
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
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT));

     VulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
     VulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true;
     if fInstance.ZFar<0.0 then begin
      VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_GREATER_OR_EQUAL;
     end else begin
      VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
     end;
     VulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
     VulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

     VulkanGraphicsPipeline.Initialize;

     VulkanGraphicsPipeline.FreeMemory;

    finally
     fMeshShaderGraphicsPipelines[AlphaMode,TpvScene3D.TPrimitiveTopology.Triangles,FaceCullingMode]:=VulkanGraphicsPipeline;
    end;

   end;

  end;

 end;

end;

procedure TpvScene3DRendererPassesPickRenderPass.ReleaseVolatileResources;
var Index:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    FaceCullingMode:TpvScene3D.TFaceCullingMode;
begin
 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for FaceCullingMode:=Low(TpvScene3D.TFaceCullingMode) to High(TpvScene3D.TFaceCullingMode) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,FaceCullingMode]);
    FreeAndNil(fMeshShaderGraphicsPipelines[AlphaMode,PrimitiveTopology,FaceCullingMode]);
   end;
  end;
 end;
 FreeAndNil(fVulkanPipelineLayout);
 for Index:=0 to fInstance.Renderer.CountInFlightFrames-1 do begin
  FreeAndNil(fGlobalVulkanDescriptorSets[Index]);
 end;
 FreeAndNil(fGlobalVulkanDescriptorPool);
 FreeAndNil(fGlobalVulkanDescriptorSetLayout);
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesPickRenderPass.OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                          const aPipelineLayout:TpvVulkanPipelineLayout;
                                                                          const aRendererInstance:TObject;
                                                                          const aRenderPass:TpvScene3DRendererRenderPass;
                                                                          const aPreviousInFlightFrameIndex:TpvSizeInt;
                                                                          const aInFlightFrameIndex:TpvSizeInt);
begin
 if not fOnSetRenderPassResourcesDone then begin
  fOnSetRenderPassResourcesDone:=true;
  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                       fVulkanPipelineLayout.Handle,
                                       1,
                                       1,
                                       @fGlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil);
 end;
end;

procedure TpvScene3DRendererPassesPickRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var InFlightFrameState:TpvScene3DRendererInstance.PInFlightFrameState;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 InFlightFrameState:=@fInstance.InFlightFrameStates^[aInFlightFrameIndex];

 if not InFlightFrameState^.Ready then begin
  exit;
 end;

 fOnSetRenderPassResourcesDone:=false;

 // Drawn per alpha mode so that each gets its own pipeline, and blended materials are included on
 // purpose - see the depth write above. Ordering within a mode does not matter here, the depth test
 // sorts out which id survives.
 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  fInstance.Renderer.Scene3D.Draw(fInstance,
                                  fVulkanGraphicsPipelines[AlphaMode],
                                  -1,
                                  aInFlightFrameIndex,
                                  TpvScene3DRendererRenderPass.View,
                                  InFlightFrameState^.FinalViewIndex,
                                  InFlightFrameState^.CountFinalViews,
                                  FrameGraph.DrawFrameIndex,
                                  aCommandBuffer,
                                  fVulkanPipelineLayout,
                                  OnSetRenderPassResources,
                                  [AlphaMode],
                                  // No jitter: a pick is not accumulated over frames like TAA, and a
                                  // sub pixel offset would only move the answer off the cursor.
                                  nil,
                                  true,
                                  false,
                                  @fMeshShaderGraphicsPipelines[AlphaMode]);
 end;

end;

end.
