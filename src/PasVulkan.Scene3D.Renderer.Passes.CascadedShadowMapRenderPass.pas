(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2024, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.Scene3D.Renderer.Passes.CascadedShadowMapRenderPass;
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
     PasVulkan.Scene3D.Planet,
     PasVulkan.Scene3D.Renderer.Globals,
     PasVulkan.Scene3D.Renderer,
     PasVulkan.Scene3D.Renderer.Instance,
     PasVulkan.Scene3D.Renderer.SkyBox;

type { TpvScene3DRendererPassesCascadedShadowMapRenderPass }
     TpvScene3DRendererPassesCascadedShadowMapRenderPass=class(TpvFrameGraph.TRenderPass)
      private
       fOnSetRenderPassResourcesDone:boolean;
       procedure OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                          const aPipelineLayout:TpvVulkanPipelineLayout;
                                          const aRendererInstance:TObject;
                                          const aRenderPass:TpvScene3DRendererRenderPass;
                                          const aPreviousInFlightFrameIndex:TpvSizeInt;
                                          const aInFlightFrameIndex:TpvSizeInt);
      private
       fVulkanRenderPass:TpvVulkanRenderPass;
       fInstance:TpvScene3DRendererInstance;
       fResourceDepth:TpvFrameGraph.TPass.TUsedImageResource;
       fMeshVertexShaderModule:TpvVulkanShaderModule;
       fMeshFragmentShaderModule:TpvVulkanShaderModule;
       fMeshMaskedFragmentShaderModule:TpvVulkanShaderModule;
       fPassVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fPassVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fPassVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
       fVulkanPipelineShaderStageMeshVertex:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageMeshFragment:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageMeshMaskedFragment:TpvVulkanPipelineShaderStage;
       fVulkanGraphicsPipelines:array[TpvScene3D.TMaterial.TAlphaMode] of TpvScene3D.TGraphicsPipelines;
       fVulkanPipelineLayout:TpvVulkanPipelineLayout;
       fPlanetShadowMapPass:TpvScene3DPlanet.TRenderPass;
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

{ TpvScene3DRendererPassesCascadedShadowMapRenderPass }

constructor TpvScene3DRendererPassesCascadedShadowMapRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
var Index:TpvSizeInt;
begin
inherited Create(aFrameGraph);

 fInstance:=aInstance;

 Name:='CascadedShadowMapRenderPass';

 MultiviewMask:=0;
 for Index:=0 to TpvScene3DRendererInstance.CountCascadedShadowMapCascades-1 do begin
  MultiviewMask:=MultiviewMask or (1 shl Index);
 end;

 Queue:=aFrameGraph.UniversalQueue;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.Absolute,
                                       fInstance.CascadedShadowMapWidth,
                                       fInstance.CascadedShadowMapHeight,
                                       1.0,
                                       TpvScene3DRendererInstance.CountCascadedShadowMapCascades);

 case fInstance.Renderer.ShadowMode of
  TpvScene3DRendererShadowMode.MSM:begin
   if fInstance.Renderer.ShadowMapSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
    if fInstance.Renderer.GPUCulling and fInstance.Renderer.GPUShadowCulling then begin
     fResourceDepth:=AddImageDepthInput('resourcetype_cascadedshadowmap_depth',
                                        'resource_cascadedshadowmap_single_depth',
                                         VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                         [TpvFrameGraph.TResourceTransition.TFlag.Attachment,
                                          TpvFrameGraph.TResourceTransition.TFlag.ExplicitOutputAttachment]
                                        );
    end else begin
     fResourceDepth:=AddImageDepthOutput('resourcetype_cascadedshadowmap_depth',
                                         'resource_cascadedshadowmap_single_depth',
                                         VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                         TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                      TpvVector4.InlineableCreate(1.0,1.0,1.0,1.0)),
                                        [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                       );
    end;
   end else begin
    if fInstance.Renderer.GPUCulling and fInstance.Renderer.GPUShadowCulling then begin
     fResourceDepth:=AddImageDepthInput('resourcetype_cascadedshadowmap_msaa_depth',
                                        'resource_cascadedshadowmap_msaa_depth',
                                        VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                        [TpvFrameGraph.TResourceTransition.TFlag.Attachment,
                                         TpvFrameGraph.TResourceTransition.TFlag.ExplicitOutputAttachment]
                                       );
    end else begin
     fResourceDepth:=AddImageDepthOutput('resourcetype_cascadedshadowmap_msaa_depth',
                                         'resource_cascadedshadowmap_msaa_depth',
                                         VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                         TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                      TpvVector4.InlineableCreate(1.0,1.0,1.0,1.0)),
                                         [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                        );
    end;
   end;
  end
  else begin
   if fInstance.Renderer.GPUCulling and fInstance.Renderer.GPUShadowCulling then begin
    fResourceDepth:=AddImageDepthInput('resourcetype_cascadedshadowmap_data',
                                       'resource_cascadedshadowmap_data_final',
                                       VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                       [TpvFrameGraph.TResourceTransition.TFlag.Attachment,
                                        TpvFrameGraph.TResourceTransition.TFlag.ExplicitOutputAttachment]
                                      );
   end else begin
    fResourceDepth:=AddImageDepthOutput('resourcetype_cascadedshadowmap_data',
                                        'resource_cascadedshadowmap_data_final',
                                        VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                        TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                     TpvVector4.InlineableCreate(1.0,1.0,1.0,1.0)),
                                        [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                       );
   end;
  end;
 end;

end;

destructor TpvScene3DRendererPassesCascadedShadowMapRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesCascadedShadowMapRenderPass.AcquirePersistentResources;
var InFlightFrameIndex:TpvSizeInt;
    Stream:TStream;
begin
 inherited AcquirePersistentResources;

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_vert.spv');
 try
  fMeshVertexShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_'+fInstance.Renderer.MeshFragTypeName+'_depth_frag.spv');
 try
  fMeshFragmentShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fInstance.Renderer.UseDemote then begin
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_'+fInstance.Renderer.MeshFragTypeName+'_depth_alphatest_demote_frag.spv');
 end else if fInstance.Renderer.UseNoDiscard then begin
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_'+fInstance.Renderer.MeshFragTypeName+'_depth_alphatest_nodiscard_frag.spv');
 end else begin
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_'+fInstance.Renderer.MeshFragTypeName+'_depth_alphatest_frag.spv');
 end;
 try
  fMeshMaskedFragmentShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageMeshVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fMeshVertexShaderModule,'main');

 fVulkanPipelineShaderStageMeshFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshFragmentShaderModule,'main');

 fVulkanPipelineShaderStageMeshMaskedFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshMaskedFragmentShaderModule,'main');

 if fInstance.Renderer.GPUCulling and fInstance.Renderer.GPUShadowCulling then begin
  fPlanetShadowMapPass:=TpvScene3DPlanet.TRenderPass.Create(fInstance.Renderer,
                                                            fInstance,
                                                            fInstance.Renderer.Scene3D,
                                                            TpvScene3DPlanet.TRenderPass.TMode.ShadowMapDisocclusion,
                                                            nil,
                                                            nil);
 end else begin
  fPlanetShadowMapPass:=TpvScene3DPlanet.TRenderPass.Create(fInstance.Renderer,
                                                            fInstance,
                                                            fInstance.Renderer.Scene3D,
                                                            TpvScene3DPlanet.TRenderPass.TMode.ShadowMap,
                                                            nil,
                                                            nil);
 end;

end;

procedure TpvScene3DRendererPassesCascadedShadowMapRenderPass.ReleasePersistentResources;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fPlanetShadowMapPass);

 FreeAndNil(fVulkanPipelineShaderStageMeshVertex);

 FreeAndNil(fVulkanPipelineShaderStageMeshFragment);

 FreeAndNil(fVulkanPipelineShaderStageMeshMaskedFragment);

 FreeAndNil(fMeshVertexShaderModule);

 FreeAndNil(fMeshFragmentShaderModule);

 FreeAndNil(fMeshMaskedFragmentShaderModule);

 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesCascadedShadowMapRenderPass.AcquireVolatileResources;
var InFlightFrameIndex:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    FaceCullingMode:TpvScene3D.TFaceCullingMode;
    VulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
begin

 inherited AcquireVolatileResources;

 fVulkanRenderPass:=VulkanRenderPass;

 fPassVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fInstance.Renderer.VulkanDevice);
 fPassVulkanDescriptorSetLayout.AddBinding(0,
                                             VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fPassVulkanDescriptorSetLayout.Initialize;

 fPassVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(fInstance.Renderer.VulkanDevice,TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),fInstance.Renderer.CountInFlightFrames);
 fPassVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,1*fInstance.Renderer.CountInFlightFrames);
 fPassVulkanDescriptorPool.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fPassVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fPassVulkanDescriptorPool,
                                                                                 fPassVulkanDescriptorSetLayout);
  fPassVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                       [],
                                                                       [fInstance.VulkanViewUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                       [],
                                                                       false);
  fPassVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(fInstance.Renderer.VulkanDevice);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),0,SizeOf(TpvScene3D.TMeshStagePushConstants));
 fVulkanPipelineLayout.AddDescriptorSetLayout(fInstance.Renderer.Scene3D.GlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fPassVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for FaceCullingMode:=Low(TpvScene3D.TFaceCullingMode) to High(TpvScene3D.TFaceCullingMode) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,FaceCullingMode]);
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
     if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask then begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshMaskedFragment);
{    end else begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshFragment);}
     end;

     VulkanGraphicsPipeline.InputAssemblyState.Topology:=TpvScene3D.VulkanPrimitiveTopologies[PrimitiveTopology];
     VulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

     fInstance.Renderer.Scene3D.InitializeGraphicsPipeline(VulkanGraphicsPipeline);

     VulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fInstance.CascadedShadowMapWidth,fInstance.CascadedShadowMapHeight,0.0,1.0);
     VulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fInstance.CascadedShadowMapWidth,fInstance.CascadedShadowMapHeight);

     VulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
     case FaceCullingMode of
      TpvScene3D.TFaceCullingMode.Normal:begin
       VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
       VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_CLOCKWISE;
      end;
      TpvScene3D.TFaceCullingMode.Inversed:begin
       VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
       VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
      end;
      else begin
       VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
       VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_CLOCKWISE;
      end;
     end;
     if fInstance.Renderer.ShadowMode=TpvScene3DRendererShadowMode.MSM then begin
      // For MSM we are using no depth bias at all
      VulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
      VulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
      VulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
      VulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
     end else begin
      VulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=true;
      VulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.5; // Constant bias in depth-resolution units by which shadows are moved away from the light. The value of 0.5 is used to round depth values up.
      VulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
      VulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=2.0; // Bias based on the change in depth in depth-resolution units by which shadows are moved away from the light. The value of 2.0 works well with PCF and DPCF.
     end;
     VulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

     VulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=fInstance.Renderer.ShadowMapSampleCountFlagBits;
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
     VulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true; //AlphaMode<>TpvScene3D.TMaterial.TAlphaMode.Blend;
     VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
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

 fPlanetShadowMapPass.AllocateResources(fVulkanRenderPass,
                                        fInstance.CascadedShadowMapWidth,
                                        fInstance.CascadedShadowMapHeight,
                                        fInstance.Renderer.ShadowMapSampleCountFlagBits);

end;

procedure TpvScene3DRendererPassesCascadedShadowMapRenderPass.ReleaseVolatileResources;
var Index:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    FaceCullingMode:TpvScene3D.TFaceCullingMode;
begin
 fPlanetShadowMapPass.ReleaseResources;
 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for FaceCullingMode:=Low(TpvScene3D.TFaceCullingMode) to High(TpvScene3D.TFaceCullingMode) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,FaceCullingMode]);
   end;
  end;
 end;
 FreeAndNil(fVulkanPipelineLayout);
 for Index:=0 to fInstance.Renderer.CountInFlightFrames-1 do begin
  FreeAndNil(fPassVulkanDescriptorSets[Index]);
 end;
 FreeAndNil(fPassVulkanDescriptorPool);
 FreeAndNil(fPassVulkanDescriptorSetLayout);
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesCascadedShadowMapRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvScene3DRendererPassesCascadedShadowMapRenderPass.OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
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
                                       @fPassVulkanDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil);
 end;
end;

procedure TpvScene3DRendererPassesCascadedShadowMapRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                           const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var InFlightFrameState:TpvScene3DRendererInstance.PInFlightFrameState;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 InFlightFrameState:=@fInstance.InFlightFrameStates^[aInFlightFrameIndex];

 if InFlightFrameState^.Ready and (InFlightFrameState^.CascadedShadowMapViewIndex>=0) then begin

  fOnSetRenderPassResourcesDone:=false;

  if fInstance.Renderer.ShadowMode<>TpvScene3DRendererShadowMode.None then begin

   if not fInstance.Renderer.Scene3D.RaytracingActive then begin

    fPlanetShadowMapPass.Draw(aInFlightFrameIndex,
                              aFrameIndex,
                              TpvScene3DRendererRenderPass.CascadedShadowMap,
                              InFlightFrameState^.CascadedShadowMapViewIndex,
                              InFlightFrameState^.CountCascadedShadowMapViews,
                              aCommandBuffer);

    fInstance.Renderer.Scene3D.Draw(fInstance,
                                    fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Opaque],
                                    -1,
                                    aInFlightFrameIndex,
                                    TpvScene3DRendererRenderPass.CascadedShadowMap,
                                    InFlightFrameState^.CascadedShadowMapViewIndex,
                                    InFlightFrameState^.CountCascadedShadowMapViews,
                                    FrameGraph.DrawFrameIndex,
                                    aCommandBuffer,
                                    fVulkanPipelineLayout,
                                    OnSetRenderPassResources,
                                    [TpvScene3D.TMaterial.TAlphaMode.Opaque],
                                    nil,
                                    fInstance.Renderer.GPUCulling and fInstance.Renderer.GPUShadowCulling);

    fInstance.Renderer.Scene3D.Draw(fInstance,
                                    fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Mask],
                                    -1,
                                    aInFlightFrameIndex,
                                    TpvScene3DRendererRenderPass.CascadedShadowMap,
                                    InFlightFrameState^.CascadedShadowMapViewIndex,
                                    InFlightFrameState^.CountCascadedShadowMapViews,
                                    FrameGraph.DrawFrameIndex,
                                    aCommandBuffer,
                                    fVulkanPipelineLayout,
                                    OnSetRenderPassResources,
                                    [TpvScene3D.TMaterial.TAlphaMode.Mask],
                                    nil,
                                    fInstance.Renderer.GPUCulling and fInstance.Renderer.GPUShadowCulling);

  { fInstance.Renderer.Scene3D.Draw(fInstance,
                                    fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Blend],
                                    -1,
                                    aInFlightFrameIndex,
                                    TpvScene3DRendererRenderPass.CascadedShadowMap,
                                    InFlightFrameState^.CascadedShadowMapViewIndex,
                                    InFlightFrameState^.CountCascadedShadowMapViews,
                                    fFrameGraph.DrawFrameIndex,
                                    aCommandBuffer,
                                    fVulkanPipelineLayout,
                                    OnSetRenderPassResources,
                                    [TpvScene3D.TMaterial.TAlphaMode.Blend],
                                    nil,
                                    fInstance.Renderer.GPUCulling and fInstance.Renderer.GPUShadowCulling);}
    end;

  end;

 end;

end;

end.
