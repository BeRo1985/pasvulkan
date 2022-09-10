(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2020, Benjamin Rosseaux (benjamin@rosseaux.de)          *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 *                                                                            *
 ******************************************************************************
 *                  General guidelines for code contributors                  *
 *============================================================================*
 *                                                                            *
 * 1. Make sure you are legally allowed to make a contribution under the zlib *
 *    license.                                                                *
 * 2. The zlib license header goes at the top of each source file, with       *
 *    appropriate copyright notice.                                           *
 * 3. This PasVulkan wrapper may be used only with the PasVulkan-own Vulkan   *
 *    Pascal header.                                                          *
 * 4. After a pull request, check the status of your pull request on          *
      http://github.com/BeRo1985/pasvulkan                                    *
 * 5. Write code which's compatible with Delphi >= 2009 and FreePascal >=     *
 *    3.1.1                                                                   *
 * 6. Don't use Delphi-only, FreePascal-only or Lazarus-only libraries/units, *
 *    but if needed, make it out-ifdef-able.                                  *
 * 7. No use of third-party libraries/units as possible, but if needed, make  *
 *    it out-ifdef-able.                                                      *
 * 8. Try to use const when possible.                                         *
 * 9. Make sure to comment out writeln, used while debugging.                 *
 * 10. Make sure the code compiles on 32-bit and 64-bit platforms (x86-32,    *
 *     x86-64, ARM, ARM64, etc.).                                             *
 * 11. Make sure the code runs on all platforms with Vulkan support           *
 *                                                                            *
 ******************************************************************************)
unit PasVulkan.Scene3D.Renderer.Passes.DepthVelocityNormalsRenderPass;
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

type { TpvScene3DRendererPassesDepthVelocityNormalsRenderPass }
     TpvScene3DRendererPassesDepthVelocityNormalsRenderPass=class(TpvFrameGraph.TRenderPass)
      private
       fOnSetRenderPassResourcesDone:boolean;
       procedure OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                          const aPipelineLayout:TpvVulkanPipelineLayout;
                                          const aRenderPassIndex:TpvSizeInt;
                                          const aPreviousInFlightFrameIndex:TpvSizeInt;
                                          const aInFlightFrameIndex:TpvSizeInt);
      private
       fInstance:TpvScene3DRendererInstance;
       fVulkanRenderPass:TpvVulkanRenderPass;
       fResourceVelocity:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceNormals:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceDepth:TpvFrameGraph.TPass.TUsedImageResource;
       fMeshVertexShaderModule:TpvVulkanShaderModule;
       fMeshDepthFragmentShaderModule:TpvVulkanShaderModule;
       fMeshDepthMaskedFragmentShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStageMeshVertex:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageMeshDepthFragment:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageMeshDepthMaskedFragment:TpvVulkanPipelineShaderStage;
       fVulkanGraphicsPipelines:array[TpvScene3D.TMaterial.TAlphaMode] of TpvScene3D.TGraphicsPipelines;
       fVulkanPipelineLayout:TpvVulkanPipelineLayout;
      public
       constructor Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance); reintroduce;
       destructor Destroy; override;
       procedure AcquirePersistentResources; override;
       procedure ReleasePersistentResources; override;
       procedure AcquireVolatileResources; override;
       procedure ReleaseVolatileResources; override;
       procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
       procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
     end;

implementation

{ TpvScene3DRendererPassesDepthVelocityNormalsRenderPass }

constructor TpvScene3DRendererPassesDepthVelocityNormalsRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin
inherited Create(aFrameGraph);

 fInstance:=aInstance;

 Name:='DepthVelocityNormalsRenderPass';

 MultiviewMask:=fInstance.SurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fInstance.CountSurfaceViews);

 if fInstance.Renderer.SurfaceSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin

  fResourceVelocity:=AddImageOutput('resourcetype_velocity',
                                    'resource_velocity_data',
                                    VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                    TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                 TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                    [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                   );

  fResourceNormals:=AddImageOutput('resourcetype_normals',
                                   'resource_normals_data',
                                   VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                   TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                   [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                  );

  fResourceDepth:=AddImageDepthOutput('resourcetype_depth',
                                      'resource_depth_data', // _temporary',
                                      VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                      TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                   TpvVector4.InlineableCreate(IfThen(fInstance.ZFar<0.0,0.0,1.0),0.0,0.0,0.0)),
                                      [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                     );

 end else begin

  fResourceVelocity:=AddImageOutput('resourcetype_msaa_velocity',
                                    'resource_forwardrendering_msaa_velocity',
                                    VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                    TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                 TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                    [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                   );

  fResourceVelocity:=AddImageResolveOutput('resourcetype_velocity',
                                           'resource_velocity_data',
                                           'resource_forwardrendering_msaa_velocity',
                                           VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                           TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare,
                                                                        TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                           [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                          );

  fResourceNormals:=AddImageOutput('resourcetype_msaa_normals',
                                   'resource_normals_data_msaa',
                                   VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                   TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                   [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                  );

  fResourceNormals:=AddImageResolveOutput('resourcetype_normals',
                                          'resource_normals_data',
                                          'resource_normals_data_msaa',
                                          VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                          TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare,
                                                                       TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                          [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                         );

  fResourceDepth:=AddImageDepthOutput('resourcetype_msaa_depth',
                                      'resource_msaa_depth_data', //'_temporary',
                                      VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                      TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                   TpvVector4.InlineableCreate(IfThen(fInstance.ZFar<0.0,0.0,1.0),0.0,0.0,0.0)),
                                      [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                     );

 end;

end;

destructor TpvScene3DRendererPassesDepthVelocityNormalsRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesDepthVelocityNormalsRenderPass.AcquirePersistentResources;
var Index:TpvSizeInt;
    Stream:TStream;
    MeshFragmentSpecializationConstants:TpvScene3DRendererInstance.TMeshFragmentSpecializationConstants;
begin
 inherited AcquirePersistentResources;

 MeshFragmentSpecializationConstants:=fInstance.MeshFragmentSpecializationConstants;

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_velocity_vert.spv');
 try
  fMeshVertexShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_'+fInstance.Renderer.MeshFragTypeName+'_depth_velocity_frag.spv');
 try
  fMeshDepthFragmentShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fInstance.Renderer.UseDemote then begin
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_'+fInstance.Renderer.MeshFragTypeName+'_depth_velocity_masked_demote_frag.spv');
 end else if fInstance.Renderer.UseNoDiscard then begin
  if fInstance.ZFar<0.0 then begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_'+fInstance.Renderer.MeshFragTypeName+'_depth_velocity_masked_nodiscard_reversedz_frag.spv');
  end else begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_'+fInstance.Renderer.MeshFragTypeName+'_depth_velocity_masked_nodiscard_frag.spv');
  end;
 end else begin
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_'+fInstance.Renderer.MeshFragTypeName+'_depth_velocity_masked_frag.spv');
 end;
 try
  fMeshDepthMaskedFragmentShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageMeshVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fMeshVertexShaderModule,'main');

 fVulkanPipelineShaderStageMeshDepthFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshDepthFragmentShaderModule,'main');
 MeshFragmentSpecializationConstants.SetPipelineShaderStage(fVulkanPipelineShaderStageMeshDepthFragment);

 fVulkanPipelineShaderStageMeshDepthMaskedFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshDepthMaskedFragmentShaderModule,'main');
 MeshFragmentSpecializationConstants.SetPipelineShaderStage(fVulkanPipelineShaderStageMeshDepthMaskedFragment);

end;

procedure TpvScene3DRendererPassesDepthVelocityNormalsRenderPass.ReleasePersistentResources;
begin

 FreeAndNil(fVulkanPipelineShaderStageMeshVertex);

 FreeAndNil(fVulkanPipelineShaderStageMeshDepthFragment);

 FreeAndNil(fVulkanPipelineShaderStageMeshDepthMaskedFragment);

 FreeAndNil(fMeshVertexShaderModule);

 FreeAndNil(fMeshDepthFragmentShaderModule);

 FreeAndNil(fMeshDepthMaskedFragmentShaderModule);

 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesDepthVelocityNormalsRenderPass.AcquireVolatileResources;
var InFlightFrameIndex:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
    VulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
begin

 inherited AcquireVolatileResources;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(fInstance.Renderer.VulkanDevice);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT),0,SizeOf(TpvScene3D.TVertexStagePushConstants));
 fVulkanPipelineLayout.AddDescriptorSetLayout(fInstance.Renderer.Scene3D.GlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

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
     if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask then begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshDepthMaskedFragment);
     end else begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshDepthFragment);
     end;

     VulkanGraphicsPipeline.InputAssemblyState.Topology:=TVkPrimitiveTopology(PrimitiveTopology);
     VulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

     fInstance.Renderer.Scene3D.InitializeGraphicsPipeline(VulkanGraphicsPipeline,true);

     VulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fInstance.Width,fInstance.Height,0.0,1.0);
     VulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fInstance.Width,fInstance.Height);

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

     VulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=fInstance.Renderer.SurfaceSampleCountFlagBits;
     begin
      VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=false;
      VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=0.0;
      VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
      VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=false;
      VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;
     end;

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
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT));
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
     VulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=AlphaMode<>TpvScene3D.TMaterial.TAlphaMode.Blend;
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
     fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]:=VulkanGraphicsPipeline;
    end;

   end;

  end;

 end;

end;

procedure TpvScene3DRendererPassesDepthVelocityNormalsRenderPass.ReleaseVolatileResources;
var Index:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
begin
 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;
 FreeAndNil(fVulkanPipelineLayout);
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesDepthVelocityNormalsRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvScene3DRendererPassesDepthVelocityNormalsRenderPass.OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                                          const aPipelineLayout:TpvVulkanPipelineLayout;
                                                                                          const aRenderPassIndex:TpvSizeInt;
                                                                                          const aPreviousInFlightFrameIndex:TpvSizeInt;
                                                                                          const aInFlightFrameIndex:TpvSizeInt);
begin
 if not fOnSetRenderPassResourcesDone then begin
  fOnSetRenderPassResourcesDone:=true;
 end;
end;

procedure TpvScene3DRendererPassesDepthVelocityNormalsRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                         const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var InFlightFrameState:TpvScene3DRendererInstance.PInFlightFrameState;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 InFlightFrameState:=@fInstance.InFlightFrameStates^[aInFlightFrameIndex];

 if InFlightFrameState^.Ready then begin

  fOnSetRenderPassResourcesDone:=false;

  fInstance.Renderer.Scene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Opaque],
                                  IfThen(aFrameIndex=0,aInFlightFrameIndex,FrameGraph.DrawPreviousInFlightFrameIndex),
                                  aInFlightFrameIndex,
                                  InFlightFrameState^.ViewRenderPassIndex,
                                  InFlightFrameState^.FinalViewIndex,
                                  InFlightFrameState^.CountViews,
                                  FrameGraph.DrawFrameIndex,
                                  aCommandBuffer,
                                  fVulkanPipelineLayout,
                                  OnSetRenderPassResources,
                                  [TpvScene3D.TMaterial.TAlphaMode.Opaque],
                                  @InFlightFrameState^.Jitter);

{ if (fInstance.Renderer.TransparencyMode=TTransparencyMode.Direct) or not (fInstance.Renderer.UseOITAlphaTest or fInstance.Renderer.Scene3D.HasTransmission) then begin
  fInstance.Renderer.Scene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Mask],
                                  IfThen(aFrameIndex=0,aInFlightFrameIndex,fFrameGraph.DrawPreviousInFlightFrameIndex),
                                  aInFlightFrameIndex,
                                  InFlightFrameState^.ViewRenderPassIndex,
                                  InFlightFrameState^.FinalViewIndex,
                                  InFlightFrameState^.CountViews,
                                  fFrameGraph.DrawFrameIndex,
                                  aCommandBuffer,
                                  fVulkanPipelineLayout,
                                  OnSetRenderPassResources,
                                  [TpvScene3D.TMaterial.TAlphaMode.Mask],
                                  @InFlightFrameState^.Jitter);
  end;}

 end;

end;

end.
