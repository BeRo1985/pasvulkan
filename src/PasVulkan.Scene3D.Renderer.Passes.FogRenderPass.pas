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
unit PasVulkan.Scene3D.Renderer.Passes.FogRenderPass;
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

type { TpvScene3DRendererPassesFogRenderPass }
     // Atmosphere-independent distance fog, applied at the start of the HDR post-process chain.
     // It reads the current chained scene colour (input attachment) plus scene depth, reconstructs
     // the view-space distance, and blends a fog colour over the pixel by an exponential distance
     // term with an optional world-height falloff. The fog colour is either a fixed colour
     // (FogMode = FixedColor) or the environment (IBL) cube map sampled in the view direction
     // (FogMode = EnvironmentColor). Background / sky pixels stay untouched. Under MSAA the raw
     // multisampled depth is read and the fog factor is computed per sample and averaged (the
     // FOG_MSAA shader variants), so silhouette pixels match their resolved coverage mix instead
     // of showing a rim line. The pass is only created when FogMode <> None, so projects that
     // never enable it pay nothing.
     TpvScene3DRendererPassesFogRenderPass=class(TpvFrameGraph.TRenderPass)
      public
       type TPushConstants=record
             FogColor:TpvVector4;
             ViewBaseIndex:TpvUInt32;
             Density:TpvFloat;
             DensityMultiplier:TpvFloat;
             HeightFalloff:TpvFloat;
             HeightBase:TpvFloat;
             EnvironmentLOD:TpvFloat;
             CountSamples:TpvUInt32;
            end;
      private
       fInstance:TpvScene3DRendererInstance;
       fSampleEnvironment:Boolean;
       // Under MSAA the fog reads the raw multisampled depth (per-sample fog factors, averaged in
       // the FOG_MSAA shader variants) instead of the reduced depth pyramid, whose single depth per
       // pixel mismatches the resolved colour's coverage mix at silhouettes (a visible rim line).
       fMSAA:Boolean;
       // AtmosphericCompositingBeforeResolve: the pass is placed before the multisample resolve and
       // runs at sample rate, so that each sample is fogged by its own depth and the resolve
       // afterwards averages finished results. That makes the averaging above unnecessary - it is
       // exact rather than an approximation - at the cost of shading every sample.
       fPerSample:Boolean;
       fVulkanRenderPass:TpvVulkanRenderPass;
       fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceMSAADepth:TpvFrameGraph.TPass.TUsedImageResource;
       fVulkanMSAADepthImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fResourceOutput:TpvFrameGraph.TPass.TUsedImageResource;
       fVulkanVertexShaderModule:TpvVulkanShaderModule;
       fVulkanFragmentShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
       fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
       fVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
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

uses PasVulkan.Scene3D.Renderer.GradientEnvironment;

{ TpvScene3DRendererPassesFogRenderPass }

constructor TpvScene3DRendererPassesFogRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin

 inherited Create(aFrameGraph);

 fInstance:=aInstance;

 fSampleEnvironment:=fInstance.FogMode=TpvScene3DRendererInstance.TFogMode.EnvironmentColor;

 fMSAA:=fInstance.Renderer.SurfaceSampleCountFlagBits<>TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT);

 // Only where there is something multisampled left to run on, and only where the instance asked for it.
 // The tracker has to hold something too - it is set by whichever pass last wrote a multisampled colour,
 // and if that never happened there is no such picture to composite onto.
 fPerSample:=fMSAA and
             fInstance.AtmosphericCompositingBeforeResolve and
             assigned(fInstance.LastMSAAOutputResource);

 Name:='FogRenderPass';

 MultiviewMask:=fInstance.SurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       fInstance.SizeFactor,
                                       fInstance.SizeFactor,
                                       1.0,
                                       fInstance.CountSurfaceViews);

 // Before the resolve the colour to read is the multisampled one, which is not what the chain's
 // LastOutputResource points at - that is the single-sample picture the resolve will eventually produce
 // and which does not exist yet at this point of the graph.
 if fPerSample then begin
  fResourceColor:=AddImageInput(fInstance.LastMSAAOutputResource.ResourceType.Name,
                                fInstance.LastMSAAOutputResource.Resource.Name,
                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                [TpvFrameGraph.TResourceTransition.TFlag.Attachment]);
 end else begin
  fResourceColor:=AddImageInput(fInstance.LastOutputResource.ResourceType.Name,
                                fInstance.LastOutputResource.Resource.Name,
                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                [TpvFrameGraph.TResourceTransition.TFlag.Attachment]);
 end;

 // Under MSAA the raw multisampled depth is read directly (see fMSAA above); without MSAA the
 // depth comes from the reduced depth pyramid, which needs no frame-graph input here (it is bound
 // directly, with the explicit DepthMipMapComputePass dependency set at pass creation).
 if fMSAA then begin
  fResourceMSAADepth:=AddImageInput('resourcetype_msaa_depth',
                                    'resource_msaa_depth_data',
                                    VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                    [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                   );
 end else begin
  fResourceMSAADepth:=nil;
 end;

 // The output takes the input's resource type when running before the resolve, so that it carries the
 // same sample count by construction rather than by a name chosen here having to agree with one chosen
 // elsewhere - the multisampled colour is called two different things depending on which pass last wrote
 // it, and both of them have to work.
 if fPerSample then begin
  fResourceOutput:=AddImageOutput(fInstance.LastMSAAOutputResource.ResourceType.Name,
                                  'resource_fog_msaa_color',
                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                  TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare),
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );
 end else begin
  fResourceOutput:=AddImageOutput('resourcetype_color',
                                  'resource_fog_color',
                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                  TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare),
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );
 end;

 // Continue the post-process chain: everything downstream now reads the fogged colour.
 fInstance.LastOutputResource:=fResourceOutput;

 // And before the resolve, the multisampled picture is this one now - the resolve that follows has to be
 // told, or it would resolve the colour this pass was handed instead of the one it produced.
 if fPerSample then begin
  fInstance.LastMSAAOutputResource:=fResourceOutput;
 end;

end;

destructor TpvScene3DRendererPassesFogRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesFogRenderPass.AcquirePersistentResources;
var Stream:TStream;
begin

 inherited AcquirePersistentResources;

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('fullscreen_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fPerSample then begin
  if fSampleEnvironment then begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('fog_environment_per_sample_frag.spv');
  end else begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('fog_per_sample_frag.spv');
  end;
 end else if fMSAA then begin
  if fSampleEnvironment then begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('fog_environment_msaa_frag.spv');
  end else begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('fog_msaa_frag.spv');
  end;
 end else begin
  if fSampleEnvironment then begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('fog_environment_frag.spv');
  end else begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('fog_frag.spv');
  end;
 end;
 try
  fVulkanFragmentShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVulkanVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fVulkanFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

end;

procedure TpvScene3DRendererPassesFogRenderPass.ReleasePersistentResources;
begin
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesFogRenderPass.AcquireVolatileResources;
var InFlightFrameIndex:TpvSizeInt;
    GradientEnvironment:TpvScene3DRendererGradientEnvironment;
    EnvironmentImageInfo:TVkDescriptorImageInfo;
begin

 inherited AcquireVolatileResources;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(fInstance.Renderer.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fInstance.Renderer.CountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,fInstance.Renderer.CountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fInstance.Renderer.CountInFlightFrames*2);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,fInstance.Renderer.CountInFlightFrames);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fInstance.Renderer.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(1,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(2,
                                       VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 if fSampleEnvironment then begin
  fVulkanDescriptorSetLayout.AddBinding(3,
                                        VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                        1,
                                        TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                        []);
 end;
 fVulkanDescriptorSetLayout.Initialize;

 // Resolve the environment cube map that the EnvironmentColor variant samples: the per-frame
 // gradient IBL when the gradient sky is active, otherwise the static environment map. Both are
 // valid once the renderer's persistent resources exist (before the frame graph goes volatile).
 EnvironmentImageInfo:=TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,VK_NULL_HANDLE,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
 if fSampleEnvironment then begin
  GradientEnvironment:=nil;
  if fInstance.Renderer.Scene3D.EnvironmentMode=TpvScene3DEnvironmentMode.Gradient then begin
   GradientEnvironment:=TpvScene3DRendererGradientEnvironment(fInstance.Renderer.GradientEnvironment);
  end;
  if assigned(GradientEnvironment) then begin
   EnvironmentImageInfo:=TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedSampler.Handle,
                                                       GradientEnvironment.GGXCubeMapTexture.VulkanImageView.Handle,
                                                       VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
  end else begin
   EnvironmentImageInfo:=fInstance.Renderer.ImageBasedLightingEnvMapCubeMaps.GGXDescriptorImageInfo;
  end;
 end;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                fResourceColor.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceColor.ResourceTransition.Layout)],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  if fMSAA then begin
   // The raw multisampled depth needs a depth-aspect 2D-array view of its own (the frame graph's
   // default view is a depth/stencil attachment view); the FOG_MSAA shader variant reads it as a
   // sampler2DMSArray via texelFetch, so the sampler itself is irrelevant.
   fVulkanMSAADepthImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                                                             fResourceMSAADepth.VulkanImages[InFlightFrameIndex],
                                                                             TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY),
                                                                             TpvFrameGraph.TImageResourceType(fResourceMSAADepth.ResourceType).Format,
                                                                             VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                             VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                             VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                             VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                             TVkImageAspectFlags(VK_IMAGE_ASPECT_DEPTH_BIT),
                                                                             0,
                                                                             1,
                                                                             0,
                                                                             fInstance.CountSurfaceViews
                                                                            );
   fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                  0,
                                                                  1,
                                                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                  [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedSampler.Handle,
                                                                                                 fVulkanMSAADepthImageViews[InFlightFrameIndex].Handle,
                                                                                                 fResourceMSAADepth.ResourceTransition.Layout)],
                                                                  [],
                                                                  [],
                                                                  false
                                                                 );
  end else begin
   fVulkanMSAADepthImageViews[InFlightFrameIndex]:=nil;
   fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                  0,
                                                                  1,
                                                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                  [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedSampler.Handle,
                                                                                                 fInstance.DepthMipmappedArray2DImages[InFlightFrameIndex].VulkanArrayImageView.Handle,
                                                                                                 TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                  [],
                                                                  [],
                                                                  false
                                                                 );
  end;
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                 [],
                                                                 [fInstance.VulkanViewUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                 [],
                                                                 false);
  if fSampleEnvironment then begin
   fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                  0,
                                                                  1,
                                                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                  [EnvironmentImageInfo],
                                                                  [],
                                                                  [],
                                                                  false);
  end;
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(fInstance.Renderer.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),0,SizeOf(TpvScene3DRendererPassesFogRenderPass.TPushConstants));
 fVulkanPipelineLayout.Initialize;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(fInstance.Renderer.VulkanDevice,
                                                           fInstance.Renderer.VulkanPipelineCache,
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

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fResourceOutput.Width,fResourceOutput.Height,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fResourceOutput.Width,fResourceOutput.Height);

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

 // Per sample, and every one of them: MinSampleShading below one lets the driver shade fewer invocations
 // than there are samples and hand the same result to several of them, which is precisely the averaging
 // this variant exists to avoid.
 if fPerSample then begin
  fVulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=fInstance.Renderer.SurfaceSampleCountFlagBits;
  fVulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=true;
  fVulkanGraphicsPipeline.MultisampleState.MinSampleShading:=1.0;
 end else begin
  fVulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=VK_SAMPLE_COUNT_1_BIT;
  fVulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=false;
  fVulkanGraphicsPipeline.MultisampleState.MinSampleShading:=0.0;
 end;
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

procedure TpvScene3DRendererPassesFogRenderPass.ReleaseVolatileResources;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fVulkanMSAADepthImageViews[InFlightFrameIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 fVulkanRenderPass:=nil;

 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesFogRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvScene3DRendererPassesFogRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var InFlightFrameState:TpvScene3DRendererInstance.PInFlightFrameState;
    PushConstants:TpvScene3DRendererPassesFogRenderPass.TPushConstants;
begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 InFlightFrameState:=@fInstance.InFlightFrameStates^[aInFlightFrameIndex];

 PushConstants.FogColor:=TpvVector4.InlineableCreate(fInstance.FogColor,1.0);
 PushConstants.ViewBaseIndex:=InFlightFrameState^.FinalViewIndex;
 PushConstants.Density:=fInstance.FogDensity;
 // FogMode = None only happens if the app disabled fog after the pass was already built; treat it
 // as a pass-through by zeroing the density (the pass itself is not created when None at build time).
 if fInstance.FogMode=TpvScene3DRendererInstance.TFogMode.None then begin
  PushConstants.DensityMultiplier:=0.0;
 end else begin
  PushConstants.DensityMultiplier:=fInstance.FogDensityMultiplier;
 end;
 PushConstants.HeightFalloff:=fInstance.FogHeightFalloff;
 PushConstants.HeightBase:=fInstance.FogHeightBase;
 PushConstants.EnvironmentLOD:=fInstance.FogEnvironmentLOD;
 PushConstants.CountSamples:=fInstance.Renderer.CountSurfaceMSAASamples;

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      fVulkanPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,0,nil);
 aCommandBuffer.CmdPushConstants(fVulkanPipelineLayout.Handle,
                                 TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_FRAGMENT_BIT),
                                 0,
                                 SizeOf(TpvScene3DRendererPassesFogRenderPass.TPushConstants),
                                 @PushConstants);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
 if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
  fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.BeginBreadcrumb(aCommandBuffer.Handle,TpvVulkanBreadcrumbType.Draw,'Fog');
 end;
 aCommandBuffer.CmdDraw(3,1,0,0);
 if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
  fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.EndBreadcrumb(aCommandBuffer.Handle);
 end;

end;

end.
