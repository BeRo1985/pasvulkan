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
unit PasVulkan.Scene3D.Renderer.Passes.WaterAerialPerspectiveRenderPass;
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
     PasVulkan.Scene3D.Atmosphere,
     PasVulkan.Scene3D.Renderer.Globals,
     PasVulkan.Scene3D.Renderer,
     PasVulkan.Scene3D.Renderer.Instance,
     PasVulkan.Scene3D.Planet;

type { TpvScene3DRendererPassesWaterAerialPerspectiveRenderPass }

     // The water surface is drawn into a layer of its own and is composited only in the transparency resolve,
     // so the atmosphere pass, which runs before it over the scene, never reaches it. Without this pass the
     // terrain at the horizon stands fully hazed next to water which carries no haze at all, which breaks the
     // picture exactly along the shore line and along the limb of the planet.
     // This pass hands the very same ray marching the same job for the water layer, so that both sides of
     // that seam are computed alike instead of being matched by eye. What keeps the background from being
     // hazed twice is the radiance weight mask which the water surface writes: it says, per pixel, how much
     // of what stands there is light of the water's own, as opposed to light from further away, which brought
     // its own, far longer atmosphere along already.
     TpvScene3DRendererPassesWaterAerialPerspectiveRenderPass=class(TpvFrameGraph.TRenderPass)
      public
      private
       fInstance:TpvScene3DRendererInstance;
       fVulkanRenderPass:TpvVulkanRenderPass;
       fResourceCascadedShadowMap:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceCloudsShadowMap:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceCloudsInscattering:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceCloudsTransmittance:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceCloudsDepth:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceDepth:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceOwnRadianceWeight:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceOutput:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceTransmittance:TpvFrameGraph.TPass.TUsedImageResource;
       fVulkanVertexShaderModule:TpvVulkanShaderModule;
       fVulkanFragmentShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
       fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
       fDualBlendSupport:Boolean;
       fMaskCarriesDepth:Boolean; // True where the water hands its surface depth over in the weight mask, which is the multisampled case
       fPushConstants:TpvScene3DAtmosphereGlobals.TRaymarchingPushConstants;
       function GetPlanetWaterSettings(out aScale:TpvFloat):TpvScene3DRendererWaterAerialPerspectiveMode;
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

{ TpvScene3DRendererPassesWaterAerialPerspectiveRenderPass }

constructor TpvScene3DRendererPassesWaterAerialPerspectiveRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin

 inherited Create(aFrameGraph);

 fInstance:=aInstance;

 Name:='WaterAerialPerspectiveRenderPass';

 MultiviewMask:=fInstance.SurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       fInstance.SizeFactor,
                                       fInstance.SizeFactor,
                                       1.0,
                                       fInstance.CountSurfaceViews);

 fDualBlendSupport:=(fInstance.Renderer.VulkanDevice.PhysicalDevice.Features.dualSrcBlend<>VK_FALSE) and
                    (fInstance.Renderer.VulkanDevice.PhysicalDevice.Properties.limits.maxFragmentDualSrcAttachments>=2);

 fResourceCascadedShadowMap:=AddImageInput('resourcetype_cascadedshadowmap_data',
                                           'resource_cascadedshadowmap_data_final',
                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                           []
                                          );

 fResourceCloudsShadowMap:=AddImageInput('resourcetype_clouds_shadowmap',
                                         'resource_clouds_shadowmap',
                                         VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                         []
                                        );

 fResourceCloudsInscattering:=AddImageInput('resourcetype_inscattering',
                                            'resource_clouds_inscattering',
                                            VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                            []
                                           );

 fResourceCloudsTransmittance:=AddImageInput('resourcetype_transmittance',
                                             'resource_clouds_transmittance',
                                             VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                             []
                                            );

 fResourceCloudsDepth:=AddImageInput('resourcetype_lineardepth',
                                     'resource_clouds_lineardepth',
                                     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                     []
                                    );

 // Where the water surface depth comes from. Without multisampling the depth buffer holds it, since the water
 // pass writes depth into it. With multisampling that buffer is multisampled while this pass works on the
 // resolved colour, so the water hands the depth over in the second channel of its weight mask instead, which
 // resolves along with it. The depth input is then not needed at all.
 fMaskCarriesDepth:=fInstance.Renderer.SurfaceSampleCountFlagBits<>TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT);

 if fMaskCarriesDepth then begin
  fResourceDepth:=nil;
  fResourceOwnRadianceWeight:=AddImageInput('resourcetype_water_own_radiance_weight_depth',
                                            'resource_water_own_radiance_weight',
                                            VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                            []
                                           );
 end else begin
  fResourceDepth:=AddImageDepthInput('resourcetype_depth',
                                     'resource_depth_data',
                                     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                     []
                                    );
  fResourceOwnRadianceWeight:=AddImageInput('resourcetype_water_own_radiance_weight',
                                            'resource_water_own_radiance_weight',
                                            VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                            []
                                           );
 end;

 fResourceOutput:=AddImageInput('resourcetype_color',
                                'resource_water_color',
                                VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                [TpvFrameGraph.TResourceTransition.TFlag.Attachment,
                                 TpvFrameGraph.TResourceTransition.TFlag.ExplicitOutputAttachment]
                               );

 if fDualBlendSupport then begin
  // Target of the second source colour of the dual source blend. Nothing ever reads it, it exists so that the
  // shader has somewhere to put the per channel transmittance which the blend multiplies the destination by.
  fResourceTransmittance:=AddImageOutput('resourcetype_transmittance',
                                         'resource_water_atmosphere_transmittance',
                                         VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                         TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                      TpvVector4.InlineableCreate(1.0,1.0,1.0,1.0)),
                                         [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                        );
 end else begin
  fResourceTransmittance:=nil;
 end;

end;

destructor TpvScene3DRendererPassesWaterAerialPerspectiveRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesWaterAerialPerspectiveRenderPass.AcquirePersistentResources;
var ShadowType:String;
    Stream:TStream;
begin

 inherited AcquirePersistentResources;

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('fullscreen_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fInstance.Renderer.RaytracingActive then begin
  ShadowType:='shadows_raytracing_';
 end else begin
  ShadowType:='shadows_';
 end;

 // No multisampled variants here, since this pass works on the resolved water colour in every configuration.
 // Should that turn out to show artefacts along the silhouette of the water, the multisampled layer is the
 // place to move to.
 if fDualBlendSupport then begin
  if fResourceOwnRadianceWeight.CountArrayLayers>0 then begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('atmosphere_raymarch_'+ShadowType+'dualblend_multiview_weightmask_frag.spv');
  end else begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('atmosphere_raymarch_'+ShadowType+'dualblend_weightmask_frag.spv');
  end;
 end else begin
  if fResourceOwnRadianceWeight.CountArrayLayers>0 then begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('atmosphere_raymarch_'+ShadowType+'multiview_weightmask_frag.spv');
  end else begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('atmosphere_raymarch_'+ShadowType+'weightmask_frag.spv');
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

procedure TpvScene3DRendererPassesWaterAerialPerspectiveRenderPass.ReleasePersistentResources;
begin
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesWaterAerialPerspectiveRenderPass.AcquireVolatileResources;
begin

 inherited AcquireVolatileResources;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(fInstance.Renderer.VulkanDevice,
                                                           fInstance.Renderer.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           TpvScene3DAtmosphereGlobals(fInstance.Scene3D.AtmosphereGlobals).RaymarchingPipelineLayout,
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

 fVulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT);
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
 if fDualBlendSupport then begin
  // Destination times the per channel transmittance of the second source colour, plus the inscattering, which
  // is the very same compositing which the atmosphere pass performs over the scene.
  fVulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(true,
                                                                       VK_BLEND_FACTOR_ONE,
                                                                       VK_BLEND_FACTOR_SRC1_COLOR,
                                                                       VK_BLEND_OP_ADD,
                                                                       VK_BLEND_FACTOR_ZERO,
                                                                       VK_BLEND_FACTOR_ONE,
                                                                       VK_BLEND_OP_ADD,
                                                                       TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                       TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                       TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                       TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));
  fVulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
                                                                       VK_BLEND_FACTOR_ONE,
                                                                       VK_BLEND_FACTOR_ONE,
                                                                       VK_BLEND_OP_ADD,
                                                                       VK_BLEND_FACTOR_ONE,
                                                                       VK_BLEND_FACTOR_ZERO,
                                                                       VK_BLEND_OP_ADD,
                                                                       0);
 end else begin
  fVulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(true,
                                                                       VK_BLEND_FACTOR_ONE,
                                                                       VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                       VK_BLEND_OP_ADD,
                                                                       VK_BLEND_FACTOR_ZERO,
                                                                       VK_BLEND_FACTOR_ONE,
                                                                       VK_BLEND_OP_ADD,
                                                                       TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                       TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                       TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                       TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));
 end;

 // The alpha of the water layer is its coverage for the transparency resolve, so it must survive this pass
 // untouched, which is what the zero and one alpha blend factors above are for. And there is no depth
 // attachment here at all, this is a full screen pass over what the water left behind.
 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

end;

procedure TpvScene3DRendererPassesWaterAerialPerspectiveRenderPass.ReleaseVolatileResources;
begin
 FreeAndNil(fVulkanGraphicsPipeline);
 fVulkanRenderPass:=nil;
 inherited ReleaseVolatileResources;
end;

function TpvScene3DRendererPassesWaterAerialPerspectiveRenderPass.GetPlanetWaterSettings(out aScale:TpvFloat):TpvScene3DRendererWaterAerialPerspectiveMode;
var Index:TpvSizeInt;
    Planets:TpvScene3DPlanets;
    Planet:TpvScene3DPlanet;
    MaxMode:TpvScene3DRendererWaterAerialPerspectiveMode;
begin
 aScale:=1.0;
 MaxMode:=TpvScene3DRendererWaterAerialPerspectiveMode.Off;
 Planets:=TpvScene3DPlanets(fInstance.Scene3D.Planets);
 if assigned(Planets) then begin
  for Index:=0 to Planets.Count-1 do begin
   Planet:=Planets[Index];
   if assigned(Planet) then begin
    // The first planet decides, since the water layer is a single one for the whole picture anyway.
    MaxMode:=Planet.WaterAerialPerspectiveMaxMode;
    aScale:=Planet.WaterAerialPerspectiveScale;
    break;
   end;
  end;
 end;
 // The player chooses, the planet caps. A world which cannot carry the expensive stage never hands it out,
 // and within what it does hand out the choice stays with the player.
 result:=fInstance.Renderer.WaterAerialPerspectiveMode;
 if result>MaxMode then begin
  result:=MaxMode;
 end;
end;

procedure TpvScene3DRendererPassesWaterAerialPerspectiveRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
var Scale:TpvFloat;
begin
 // The hard switch, which has to be set here and not while executing, since the frame graph takes it over
 // into its double buffered state at update time. Off skips the whole pass, barriers and load store aside.
 Enabled:=(GetPlanetWaterSettings(Scale)<>TpvScene3DRendererWaterAerialPerspectiveMode.Off) and (Scale>0.0);
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvScene3DRendererPassesWaterAerialPerspectiveRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var InFlightFrameState:TpvScene3DRendererInstance.PInFlightFrameState;
    Scale:TpvFloat;
    Mode:TpvScene3DRendererWaterAerialPerspectiveMode;
    DepthImageView:TVkImageView;
begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 Mode:=GetPlanetWaterSettings(Scale);

 // Where the mask carries the depth, the atmosphere's depth binding is never read by this shader variant, so
 // the weight mask itself stands in there as a valid view of the right kind.
 if assigned(fResourceDepth) then begin
  DepthImageView:=fResourceDepth.VulkanImageViews[aInFlightFrameIndex].Handle;
 end else begin
  DepthImageView:=fResourceOwnRadianceWeight.VulkanImageViews[aInFlightFrameIndex].Handle;
 end;

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);

 InFlightFrameState:=@TpvScene3DRendererInstance(fInstance).InFlightFrameStates[aInFlightFrameIndex];

 fPushConstants.BaseViewIndex:=InFlightFrameState^.FinalUnjitteredViewIndex;
 fPushConstants.CountViews:=InFlightFrameState^.CountFinalViews;

 if fInstance.Renderer.AnimatedAtmosphereNoise then begin
  fPushConstants.FrameIndex:=aFrameIndex;
 end else begin
  fPushConstants.FrameIndex:=0;
 end;

 fPushConstants.Flags:=0;
 if TpvScene3DRenderer(TpvScene3DRendererInstance(fInstance).Renderer).FastSky then begin
  fPushConstants.Flags:=fPushConstants.Flags or (TpvUInt32(1) shl 0);
 end;
 // The cheap stage is exactly the fast aerial perspective of the atmosphere, so it is requested here per pass
 // rather than being taken from the global setting: the player may well want the precomputed volume for the
 // water while the scene itself keeps the full ray marching, or the other way round.
 if (Mode=TpvScene3DRendererWaterAerialPerspectiveMode.CameraVolume) or
    TpvScene3DRenderer(TpvScene3DRendererInstance(fInstance).Renderer).FastAerialPerspective then begin
  fPushConstants.Flags:=fPushConstants.Flags or (TpvUInt32(1) shl 1);
 end;
 if TpvScene3DRenderer(TpvScene3DRendererInstance(fInstance).Renderer).AtmosphereBlueNoise then begin
  fPushConstants.Flags:=fPushConstants.Flags or (TpvUInt32(1) shl 2);
 end;
 if TpvScene3DRenderer(TpvScene3DRendererInstance(fInstance).Renderer).AtmosphereShadows then begin
  fPushConstants.Flags:=fPushConstants.Flags or (TpvUInt32(1) shl 3);
 end;
 if TpvScene3DRendererInstance(fInstance).ZFar<0.0 then begin
  fPushConstants.Flags:=fPushConstants.Flags or (TpvUInt32(1) shl 16);
 end;
 if fMaskCarriesDepth then begin
  fPushConstants.Flags:=fPushConstants.Flags or (TpvUInt32(1) shl 4); // FLAGS_RADIANCE_WEIGHT_MASK_DEPTH
 end;
 fPushConstants.CountSamples:=1;

 // Strength of this layer's aerial perspective on top of the atmosphere's own setting, which the shader
 // multiplies in as the base.
 fPushConstants.RadianceWeightMaskScale:=Scale;

 // Consumer 1 of the ray marching. Its own descriptor set, so that the views handed over here do not land
 // in the set the scene pass has already bound into its recorded draw. Getting that wrong painted the mask
 // into the scene pass's depth slot, which washed everything that was not water in sky inscattering.
 TpvScene3DAtmospheres(fInstance.Scene3D.Atmospheres).Draw(1,
                                                           aInFlightFrameIndex,
                                                           aCommandBuffer,
                                                           DepthImageView,
                                                           fResourceCascadedShadowMap.VulkanImageViews[aInFlightFrameIndex].Handle,
                                                           fResourceCloudsInscattering.VulkanImageViews[aInFlightFrameIndex].Handle,
                                                           fResourceCloudsTransmittance.VulkanImageViews[aInFlightFrameIndex].Handle,
                                                           fResourceCloudsDepth.VulkanImageViews[aInFlightFrameIndex].Handle,
                                                           fResourceCloudsShadowMap.VulkanImageViews[aInFlightFrameIndex].Handle,
                                                           fResourceOwnRadianceWeight.VulkanImageViews[aInFlightFrameIndex].Handle,
                                                           fInstance,
                                                           fPushConstants);

end;

end.
