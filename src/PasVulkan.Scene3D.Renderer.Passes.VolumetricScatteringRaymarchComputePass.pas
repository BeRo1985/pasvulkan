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
unit PasVulkan.Scene3D.Renderer.Passes.VolumetricScatteringRaymarchComputePass;
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
     PasVulkan.Raytracing,
     PasVulkan.Scene3D,
     PasVulkan.Scene3D.Atmosphere,
     PasVulkan.Scene3D.Renderer.Globals,
     PasVulkan.Scene3D.Renderer,
     PasVulkan.Scene3D.Renderer.Instance,
     PasVulkan.Scene3D.Renderer.Array2DImage;

type { TpvScene3DRendererPassesVolumetricScatteringRaymarchComputePass }
     // The first of the three volumetric scattering passes: the ray march, at half the surface size. See
     // volumetric_scattering_raymarch.comp for what it computes.
     //
     // It stands OUTSIDE the visible chain: it neither reads nor writes LastOutputResource, it only fills
     // the scattering buffer that the blur and the compose behind it work on. Its depth does not come out
     // of the frame graph either - it is FinalDepthArray2DImage, which the depth resolve leaves behind
     // right after the depth prepass, and that is exactly the opaque depth this march wants.
     TpvScene3DRendererPassesVolumetricScatteringRaymarchComputePass=class(TpvFrameGraph.TComputePass)
      public
       const // How far a ray is allowed to reach when nothing stops it. The projection is an infinite
             // one, so a sky ray has no far plane to march to and needs a length of its own; the same
             // 4096 metres is what the result is faded in over, so the two agree by construction.
             //
             // This is the MARCH's reach and nothing else. It is not a far plane and must never be used as
             // one: geometry routinely stands further away than this, and clamping a stored depth to it
             // makes every distant thing read as being exactly as far as the sky.
             VolumetricScatteringMaximumDistance=4096.0;
             // The depth the sky is given. Not a distance and not a bound - a SENTINEL, because under a
             // reversed infinite projection a sky pixel's raw depth is exactly zero and the distance it
             // stands for has no finite value to compute. Both this pass and the compose hand back this
             // very number for such a pixel, so the two agree by definition rather than by both happening
             // to clamp the same division the same way.
             //
             // Two to the twenty-fourth: exact in a 32-bit float, so it survives the depth image and comes
             // back bit for bit, and far beyond anything that will stand on a track. Geometry is held to
             // half of it (see below), which keeps the two apart by eight million metres - no real
             // distance can wander into the sky's value and be mistaken for it.
             VolumetricScatteringSkyDepth=16777216.0;
             // And what a geometry distance is held to, which is that sentinel halved. The distance itself
             // is abs(zNear)/rawDepth and therefore open at the top: a raw depth close enough to zero
             // yields something absurd, and absurd is exactly what must not collide with the sky's value.
             VolumetricScatteringMaximumGeometryDepth=8388608.0;
             // The shell the density is measured against: earth values, with the centre one earth radius
             // below the origin, so that near the ground it comes out as a plain falloff with height.
             // That is the GLOBAL case, and it is the one that works with a stylised skybox because it
             // asks the atmosphere for nothing. Bounding the march to a real atmosphere instead is a
             // later step; these stay as the fallback for when there is none.
             VolumetricScatteringBottomRadius=6360000.0;
             VolumetricScatteringTopRadius=6460000.0;
             VolumetricScatteringRayleighScaleHeight=8000.0;
             VolumetricScatteringMieScaleHeight=1800.0;
      public
       const // Bit zero of the flag word is free. It used to pick between two segment lengths, which was
             // really a statement about how thick the air is; that is now the mean free path below.
             // Bit one: which of the two density models the march works with. Clear is the physical
             // one - Rayleigh and Mie against the shell, with coloured extinction, which is what makes a
             // sky blue and a sunset red. Set is a drifting noise field instead, single-phase and grey,
             // which gives moving cloud shadows and banks of mist that no scale height can produce. The
             // shell's height falloff shapes both; they differ in what fills it.
             // Bit one is free again. It used to pick between the two density models, one or the other -
             // and they are now both computed and added, each with a weight of its own, so the question
             // it answered no longer has two answers: the noise field alone is (1,0), Rayleigh/Mie alone
             // is (0,1), and everything between is a real mixture rather than a choice.
             VolumetricScatteringFlagUnusedWasNoiseDensity=TpvUInt32(1) shl 1;
             // And bit two: the look this effect had before it grew a second buffer - the old distance
             // fade with its cap at a quarter, no aerial term, and no extinction written at all.
             VolumetricScatteringFlagLegacyLook=TpvUInt32(1) shl 2;
      public
       type TPushConstants=packed record
             // One hundred and twenty-eight bytes exactly, and that is the whole budget rather than a
             // coincidence: it is the smallest maxPushConstantsSize Vulkan guarantees and the figure AMD
             // drivers commonly report, so a ninth vector would be refused on hardware this must run on.
             //
             // Mirrors volumetric_scattering_raymarch.comp field for field. Floats grouped by what they
             // describe, and everything too small for a lane of its own gathered into ONE word at the end
             // instead of hiding in the spare corner of whatever vector happened to have room.

             // xyz = the direction the sunlight travels, w = the shaft gain, which is an admitted cheat on
             // the gathered light alone. See the shader for why that is not the same as the compose's
             // strength and must not be applied twice.
             SunDirectionShaftGain:TpvVector4;

             // xyz = what the primary light emits, colour times intensity, in the units the surface
             // shading uses. w = the physical model's mean free path in metres, its one density dial.
             SunRadianceMeanFreePath:TpvVector4;

             // x = the near distance (NEGATIVE here, which marks the reversed infinite projection),
             // y = how far a ray without geometry is marched, zw = what the noise model scatters and what
             // it swallows per unit of its density.
             ZNearMaximumDistanceNoiseCoefficients:TpvVector4;

             // xyz = the centre of the density shell, w = its bottom radius. Together they turn a position
             // into a height. The top radius is deliberately absent - nothing ever read it.
             ShellCentreBottomRadius:TpvVector4;

             // x = Rayleigh scale height, y = Mie scale height, z = how coarse the noise field is,
             // w = the time it drifts with.
             ScaleHeightsNoiseScaleTime:TpvVector4;

             // x = how much the aerial term weighs, y = the depth the sky is given (a sentinel, not a
             // distance), z = how thick the noise field is made - its own dial and NOT its share of the
             // mixture, w = spare, the only whole float left in the block.
             AerialWeightSkyDepthNoiseDensitySpare:TpvVector4;

             // x = the first view of this pass, y = the frame counter, z = the switches, w = the step
             // counts with the fewest in the low sixteen bits and the most in the high sixteen.
             ViewBaseIndexFrameIndexFlagsStepCounts:TpvUInt32Vector4;

             // Everything too small to be worth a lane, in one place: x = the tint's red and green,
             // y = its blue in the low half with the high half spare, z = the two models' shares with
             // Rayleigh/Mie low and the noise field high, w = the dust with Mie low and Rayleigh high.
             // Half precision throughout, which is ample for multipliers around one and shares between
             // nought and one.
             PackedTintSharesDust:TpvUInt32Vector4;

            end;
      private
       fInstance:TpvScene3DRendererInstance;
       // Whether this pass traces its shadows rather than looking them up in the cascaded shadow maps.
       // Decided once, when the shader is chosen, because the descriptor set has to match it.
       fRaytracing:Boolean;
       fPushConstants:TPushConstants;
       fResourceOutputInscattering:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceOutputExtinction:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceOutputDepth:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceCascadedShadowMap:TpvFrameGraph.TPass.TUsedImageResource;
       // Whether the march records its result twice along the one ray - once where it passes the geometry,
       // once out at the sky. Decided in the constructor, because two more outputs and two more bindings
       // hang on it, and the descriptor set has to match the shader that was chosen.
       fDualOutput:Boolean;
       fResourceOutputFarInscattering:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceOutputFarExtinction:TpvFrameGraph.TPass.TUsedImageResource;
       fOutputInscatteringImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fOutputExtinctionImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fOutputDepthImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fOutputFarInscatteringImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fOutputFarExtinctionImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fComputeShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStageCompute:TpvVulkanPipelineShaderStage;
       fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
       fPipelineLayout:TpvVulkanPipelineLayout;
       fPipeline:TpvVulkanComputePipeline;
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

{ TpvScene3DRendererPassesVolumetricScatteringRaymarchComputePass }

constructor TpvScene3DRendererPassesVolumetricScatteringRaymarchComputePass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin

 inherited Create(aFrameGraph);

 fInstance:=aInstance;

 fDualOutput:=fInstance.VolumetricScatteringDualOutputActive;

 Name:='VolumetricScatteringRaymarchComputePass';

 // The cascaded shadow maps, which is what this pass is for: without them every sample along the ray is
 // lit and the result is a wash of haze rather than shafts.
 fResourceCascadedShadowMap:=AddImageInput('resourcetype_cascadedshadowmap_data',
                                           'resource_cascadedshadowmap_data_final',
                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                           []
                                          );

 // LastOutputResource is deliberately left alone - what this writes is not the picture, it is the light
 // that will be added to the picture two passes further on.
 //
 // Two buffers rather than one, because the air does two things at once and they cannot be folded into
 // each other: it adds light along the way, and it swallows what comes through from behind. The first is
 // an addition, the second a multiplication, and the compose needs both to put a pixel together.
 fResourceOutputInscattering:=AddImageOutput('resourcetype_volumetric_scattering',
                                             'resource_volumetric_scattering',
                                             VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                             TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare),
                                             []
                                            );

 fResourceOutputExtinction:=AddImageOutput('resourcetype_volumetric_scattering',
                                           'resource_volumetric_scattering_extinction',
                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                           TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare),
                                           []
                                          );

 // And the depth, written here once and read by all three passes behind this one. It is not carried along
 // with the other two and never rewritten, so the four stages cannot come to hold different ideas of how
 // far away a texel is - which is the failure the alpha-packed version invited at every stage boundary.
 fResourceOutputDepth:=AddImageOutput('resourcetype_volumetric_scattering_depth',
                                      'resource_volumetric_scattering_depth',
                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                      TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare),
                                      []
                                     );

 // And the same two quantities again for the ray carried past the geometry, where the option asks for it.
 // No depth image of its own goes with them: the far pair describes one distance - the sky's - for every
 // texel alike, so there would be nothing in such an image but the same number repeated.
 if fDualOutput then begin

  fResourceOutputFarInscattering:=AddImageOutput('resourcetype_volumetric_scattering',
                                                 'resource_volumetric_scattering_far',
                                                 VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare),
                                                 []
                                                );

  fResourceOutputFarExtinction:=AddImageOutput('resourcetype_volumetric_scattering',
                                               'resource_volumetric_scattering_far_extinction',
                                               VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                               TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare),
                                               []
                                              );

 end else begin

  fResourceOutputFarInscattering:=nil;
  fResourceOutputFarExtinction:=nil;

 end;

end;

destructor TpvScene3DRendererPassesVolumetricScatteringRaymarchComputePass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesVolumetricScatteringRaymarchComputePass.AcquirePersistentResources;
var Stream:TStream;
begin

 inherited AcquirePersistentResources;

 // Which shadow the march asks for is decided here, once, and the descriptor set below follows it: with
 // raytracing the samples are traced against the acceleration structure, without it they are looked up in
 // the cascaded shadow maps. Traced shadows in a volume are the better answer - no cascade seams and no
 // bias to get wrong for points that have no surface - but they exist only where the hardware and the
 // scene both offer them.
 // Only on whether raytracing is in use at all, NOT on the top level structure already existing: it is
 // built as the scene comes up and may well not be there yet at this point, and choosing by it would
 // silently fall back to the cascades for the whole run.
 fRaytracing:=fInstance.Renderer.Scene3D.RaytracingActive and
              assigned(fInstance.Renderer.Scene3D.Raytracing);

 if fRaytracing then begin
  if fDualOutput then begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('volumetric_scattering_raymarch_raytracing_dual_comp.spv');
  end else begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('volumetric_scattering_raymarch_raytracing_comp.spv');
  end;
 end else begin
  if fDualOutput then begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('volumetric_scattering_raymarch_dual_comp.spv');
  end else begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('volumetric_scattering_raymarch_comp.spv');
  end;
 end;
 try
  fComputeShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageCompute:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

end;

procedure TpvScene3DRendererPassesVolumetricScatteringRaymarchComputePass.ReleasePersistentResources;
begin
 FreeAndNil(fVulkanPipelineShaderStageCompute);
 FreeAndNil(fComputeShaderModule);
 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesVolumetricScatteringRaymarchComputePass.AcquireVolatileResources;
var InFlightFrameIndex,CountViews:TpvInt32;
begin

 inherited AcquireVolatileResources;

 CountViews:=fInstance.CountSurfaceViews;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(fInstance.Renderer.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fInstance.Renderer.CountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fInstance.Renderer.CountInFlightFrames*2);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,fInstance.Renderer.CountInFlightFrames*2);
 // Three storage images normally - the two the air is written to and the depth - and five where the far
 // pair joins them.
 if fDualOutput then begin
  fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,fInstance.Renderer.CountInFlightFrames*5);
 end else begin
  fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,fInstance.Renderer.CountInFlightFrames*3);
 end;
 fVulkanDescriptorPool.Initialize;

 // This pass's own resources, as SET ONE. Set zero is the scene's global descriptor set, taken as it
 // stands, and it is where the acceleration structure comes from - see the note in the shader for why a
 // copy of that binding kept here was wrong. Every other raytracing consumer in the engine does the same.
 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fInstance.Renderer.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(1,
                                       VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(2,
                                       VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(3,
                                       VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(4,
                                       VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(5,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 // Six rather than four: the acceleration structure used to sit at six of this set, and moving it out to
 // the scene's global set left the slot free for the depth image.
 fVulkanDescriptorSetLayout.AddBinding(6,
                                       VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 if fDualOutput then begin
  fVulkanDescriptorSetLayout.AddBinding(7,
                                        VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                        1,
                                        TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                        []);
  fVulkanDescriptorSetLayout.AddBinding(8,
                                        VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                        1,
                                        TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                        []);
 end;
 fVulkanDescriptorSetLayout.Initialize;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(fInstance.Renderer.VulkanDevice);
 fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TpvScene3DRendererPassesVolumetricScatteringRaymarchComputePass.TPushConstants));
 // Set 0 = the scene's global set, which carries the acceleration structure at binding 8 among much else.
 // Declared whether or not this build traces, so that there is one layout and one binding call rather than
 // two of each; the shader simply does not name it when it has no rays to cast.
 fPipelineLayout.AddDescriptorSetLayout(fInstance.Renderer.Scene3D.GlobalVulkanDescriptorSetLayout);
 // Set 1 = this pass's own.
 fPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fPipelineLayout.Initialize;

 fPipeline:=TpvVulkanComputePipeline.Create(fInstance.Renderer.VulkanDevice,
                                            fInstance.Renderer.VulkanPipelineCache,
                                            0,
                                            fVulkanPipelineShaderStageCompute,
                                            fPipelineLayout,
                                            nil,
                                            0);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin

  fOutputInscatteringImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                                                               fResourceOutputInscattering.VulkanImages[InFlightFrameIndex],
                                                                               TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY),
                                                                               TpvFrameGraph.TImageResourceType(fResourceOutputInscattering.ResourceType).Format,
                                                                               VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                               VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                               VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                               VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                               TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                               0,
                                                                               1,
                                                                               0,
                                                                               CountViews
                                                                              );

  fOutputDepthImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                                                        fResourceOutputDepth.VulkanImages[InFlightFrameIndex],
                                                                        TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY),
                                                                        TpvFrameGraph.TImageResourceType(fResourceOutputDepth.ResourceType).Format,
                                                                        VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                        VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                        VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                        VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                        TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                        0,
                                                                        1,
                                                                        0,
                                                                        CountViews
                                                                       );

  fOutputExtinctionImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                                                             fResourceOutputExtinction.VulkanImages[InFlightFrameIndex],
                                                                             TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY),
                                                                             TpvFrameGraph.TImageResourceType(fResourceOutputExtinction.ResourceType).Format,
                                                                             VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                             VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                             VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                             VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                             TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                             0,
                                                                             1,
                                                                             0,
                                                                             CountViews
                                                                            );

  if fDualOutput then begin

   fOutputFarInscatteringImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                                                                    fResourceOutputFarInscattering.VulkanImages[InFlightFrameIndex],
                                                                                    TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY),
                                                                                    TpvFrameGraph.TImageResourceType(fResourceOutputFarInscattering.ResourceType).Format,
                                                                                    VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                    VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                    VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                    VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                    TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                    0,
                                                                                    1,
                                                                                    0,
                                                                                    CountViews
                                                                                   );

   fOutputFarExtinctionImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                                                                  fResourceOutputFarExtinction.VulkanImages[InFlightFrameIndex],
                                                                                  TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY),
                                                                                  TpvFrameGraph.TImageResourceType(fResourceOutputFarExtinction.ResourceType).Format,
                                                                                  VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                  VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                  VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                  VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                                  TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                  0,
                                                                                  1,
                                                                                  0,
                                                                                  CountViews
                                                                                 );

  end else begin

   fOutputFarInscatteringImageViews[InFlightFrameIndex]:=nil;
   fOutputFarExtinctionImageViews[InFlightFrameIndex]:=nil;

  end;

  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);

  // The ARRAY view of the resolved depth, not the plain one: a TpvScene3DRendererArray2DImage hands out a
  // 2D view when it holds a single layer, and the shader declares sampler2DArray either way.
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                                fInstance.FinalDepthArray2DImage.VulkanArrayImageView.Handle,
                                                                                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );

  // The views, for the inverse projection and the inverse view of whichever eye this is. The march needs
  // both: the ray is rebuilt through the projection first and only then taken out of view space, because
  // one combined inverse loses precision exactly where the ray is longest.
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                 [],
                                                                 [fInstance.VulkanViewUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                 [],
                                                                 false
                                                                );

  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                 [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                                fOutputInscatteringImageViews[InFlightFrameIndex].Handle,
                                                                                                VK_IMAGE_LAYOUT_GENERAL)],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );

  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                 [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                                fOutputExtinctionImageViews[InFlightFrameIndex].Handle,
                                                                                                VK_IMAGE_LAYOUT_GENERAL)],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );

  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(6,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                 [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                                fOutputDepthImageViews[InFlightFrameIndex].Handle,
                                                                                                VK_IMAGE_LAYOUT_GENERAL)],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );

  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                 [],
                                                                 [fInstance.CascadedShadowMapVulkanUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                 [],
                                                                 false
                                                                );

  // ShadowMapSampler is already the right one for the active shadow mode - the renderer builds it with
  // comparison enabled where the mode wants comparison - so the shader's two aliased views of this one
  // binding both get what they need.
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(5,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [TVkDescriptorImageInfo.Create(fInstance.Renderer.ShadowMapSampler.Handle,
                                                                                                fResourceCascadedShadowMap.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceCascadedShadowMap.ResourceTransition.Layout)],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );

  if fDualOutput then begin

   fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(7,
                                                                  0,
                                                                  1,
                                                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                  [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                                 fOutputFarInscatteringImageViews[InFlightFrameIndex].Handle,
                                                                                                 VK_IMAGE_LAYOUT_GENERAL)],
                                                                  [],
                                                                  [],
                                                                  false
                                                                 );

   fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(8,
                                                                  0,
                                                                  1,
                                                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                  [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                                 fOutputFarExtinctionImageViews[InFlightFrameIndex].Handle,
                                                                                                 VK_IMAGE_LAYOUT_GENERAL)],
                                                                  [],
                                                                  [],
                                                                  false
                                                                 );

  end;

  // The acceleration structure is not among these. It lives in the scene's global set, which this pass
  // binds beside its own and does not write to.
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;

 end;

end;

procedure TpvScene3DRendererPassesVolumetricScatteringRaymarchComputePass.ReleaseVolatileResources;
var InFlightFrameIndex:TpvInt32;
begin
 FreeAndNil(fPipeline);
 FreeAndNil(fPipelineLayout);
 for InFlightFrameIndex:=0 to fInstance.Renderer.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fOutputInscatteringImageViews[InFlightFrameIndex]);
  FreeAndNil(fOutputExtinctionImageViews[InFlightFrameIndex]);
  FreeAndNil(fOutputDepthImageViews[InFlightFrameIndex]);
  FreeAndNil(fOutputFarInscatteringImageViews[InFlightFrameIndex]);
  FreeAndNil(fOutputFarExtinctionImageViews[InFlightFrameIndex]);
 end;
 FreeAndNil(fVulkanDescriptorSetLayout);
 FreeAndNil(fVulkanDescriptorPool);
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesVolumetricScatteringRaymarchComputePass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvScene3DRendererPassesVolumetricScatteringRaymarchComputePass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var CountViews,Index:TpvInt32;
    InFlightFrameState:TpvScene3DRendererInstance.PInFlightFrameState;
    SunDirection,SunRadiance,ShellCentre:TpvVector3;
    ShellBottomRadius,ShellTopRadius:TpvFloat;
    Atmospheres:TpvScene3DAtmospheres;
    AtmosphereParameters:TpvScene3DAtmosphere.PAtmosphereParameters;
    DescriptorSets:array[0..1] of TVkDescriptorSet;
    ImageMemoryBarriers:array[0..4] of TVkImageMemoryBarrier;
    CountImageMemoryBarriers:TpvInt32;
begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 CountViews:=fInstance.CountSurfaceViews;

 // Three images to lay over, or five where the far pair is written as well.
 if fDualOutput then begin
  CountImageMemoryBarriers:=5;
 end else begin
  CountImageMemoryBarriers:=3;
 end;

 InFlightFrameState:=@fInstance.InFlightFrameStates^[aInFlightFrameIndex];

 // The frame graph hands the outputs in as SHADER_READ_ONLY_OPTIMAL, which is the one layout a storage
 // image may not be written in, so the pass lays them over themselves.
 begin
  FillChar(ImageMemoryBarriers,SizeOf(ImageMemoryBarriers),#0);
  for Index:=0 to CountImageMemoryBarriers-1 do begin
   ImageMemoryBarriers[Index].sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
   ImageMemoryBarriers[Index].srcAccessMask:=0;
   ImageMemoryBarriers[Index].dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
   ImageMemoryBarriers[Index].oldLayout:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
   ImageMemoryBarriers[Index].newLayout:=VK_IMAGE_LAYOUT_GENERAL;
   ImageMemoryBarriers[Index].srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
   ImageMemoryBarriers[Index].dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
   case Index of
    0:begin
     ImageMemoryBarriers[Index].image:=fResourceOutputInscattering.VulkanImages[aInFlightFrameIndex].Handle;
    end;
    1:begin
     ImageMemoryBarriers[Index].image:=fResourceOutputExtinction.VulkanImages[aInFlightFrameIndex].Handle;
    end;
    2:begin
     ImageMemoryBarriers[Index].image:=fResourceOutputDepth.VulkanImages[aInFlightFrameIndex].Handle;
    end;
    3:begin
     ImageMemoryBarriers[Index].image:=fResourceOutputFarInscattering.VulkanImages[aInFlightFrameIndex].Handle;
    end;
    else begin
     ImageMemoryBarriers[Index].image:=fResourceOutputFarExtinction.VulkanImages[aInFlightFrameIndex].Handle;
    end;
   end;
   ImageMemoryBarriers[Index].subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
   ImageMemoryBarriers[Index].subresourceRange.baseMipLevel:=0;
   ImageMemoryBarriers[Index].subresourceRange.levelCount:=1;
   ImageMemoryBarriers[Index].subresourceRange.baseArrayLayer:=0;
   ImageMemoryBarriers[Index].subresourceRange.layerCount:=CountViews;
  end;
  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    CountImageMemoryBarriers,@ImageMemoryBarriers[0]);
 end;

 // Switched off for this frame: the barriers above and below still run, so the image ends in the layout
 // the graph expects it in, but nothing is dispatched. What the buffer then holds is whatever was in it,
 // and that is safe only because the compose is told not to read it - see its own flag.
 if fInstance.VolumetricScatteringEnabled then begin

  aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

  // The light is the one the cascaded shadow maps are built for. There can be several directional lights
  // in a scene, but only that one has a shadow map, and scattering can only be shadowed where there is one
  // to ask - which is why this march has a single loop, and why a second light would mean a second cascade
  // set rather than a second loop.
  //
  // Taken as it stands. PrimaryShadowMapLightDirection IS the direction the light travels - measured, not
  // inferred: with the sun up it reads y = -0.67, which is downwards. The cascade builder negating it for
  // its own forward vector is not evidence of the opposite, and reading it as such put the sun under the
  // horizon for the guard in the shader, which skipped the whole march and returned exactly zero.
  SunDirection:=fInstance.Renderer.Scene3D.PrimaryShadowMapLightDirection.Normalize;

  fPushConstants.SunDirectionShaftGain:=TpvVector4.InlineableCreate(SunDirection.x,
                                                                    SunDirection.y,
                                                                    SunDirection.z,
                                                                    fInstance.VolumetricScatteringShaftGain);

  // And what that light actually emits, so the shafts belong to the same sun as the ground they fall on.
  // The scene records it beside the direction, from the light it recognised as the primary one.
  //
  // A scene that has no primary directional light leaves this black, and then the march gathers nothing -
  // which is the honest answer. Inventing a radiance here would mean shafts cast by a sun that is not in
  // the scene, and the old arrangement did exactly that: the gain alone WAS the brightness, so the shafts
  // stayed as bright and as white however the light changed.
  SunRadiance:=fInstance.Renderer.Scene3D.PrimaryShadowMapLightColorIntensity;

  fPushConstants.SunRadianceMeanFreePath:=TpvVector4.InlineableCreate(SunRadiance.x,
                                                                      SunRadiance.y,
                                                                      SunRadiance.z,
                                                                      fInstance.VolumetricScatteringMeanFreePath);

  fPushConstants.ZNearMaximumDistanceNoiseCoefficients:=TpvVector4.InlineableCreate(fInstance.ZNear,
                                                                                    VolumetricScatteringMaximumDistance,
                                                                                    fInstance.VolumetricScatteringNoiseScattering,
                                                                                    fInstance.VolumetricScatteringNoiseExtinction);

  // The shell the density is measured against, and the one place where having an atmosphere changes what
  // this effect does.
  //
  // WITHOUT one - the global case, and the one that works with a stylised skybox - the centre sits one
  // bottom radius below the origin, so that near the ground the density comes out as a plain falloff with
  // height and nothing here has to know about an atmosphere at all.
  ShellCentre:=TpvVector3.InlineableCreate(0.0,-VolumetricScatteringBottomRadius,0.0);
  ShellBottomRadius:=VolumetricScatteringBottomRadius;
  ShellTopRadius:=VolumetricScatteringTopRadius;

  // WITH one, the march is bounded by its placing and its size instead. Only the shell is taken; the
  // scale heights below stay this effect's own, because what is wanted here is light shafts and not a
  // second atmosphere beside the one already being rendered.
  //
  // UNVERIFIED, and it cannot be verified from this project - hoverrace1 creates no atmosphere at all, so
  // this branch never runs here. Two things to check on a project that does have one:
  //
  //  - THE UNITS. TAtmosphereParameters.InitializeEarthAtmosphere sets a bottom radius of 6360 and scale
  //    heights of 8.0 and 1.2, so the atmosphere counts in KILOMETRES, while the constants above are the
  //    same figures in metres. Whichever the world itself is in, the two have to agree, and the fallback
  //    above is the one that would then need changing.
  //  - THE CENTRE. Taken here as the translation of the transform plus the Center field. The atmosphere's
  //    own shaders work in a planet-centred space and never read either directly, so this reading comes
  //    from the field names and not from seeing it used.
  Atmospheres:=TpvScene3DAtmospheres(fInstance.Renderer.Scene3D.Atmospheres);
  if assigned(Atmospheres) and (Atmospheres.Count>0) and assigned(Atmospheres.Items[0]) then begin
   AtmosphereParameters:=Atmospheres.Items[0].AtmosphereParameters;
   if assigned(AtmosphereParameters) then begin
    ShellCentre:=TpvVector3.InlineableCreate(AtmosphereParameters^.Transform.Translation.x+AtmosphereParameters^.Center.x,
                                             AtmosphereParameters^.Transform.Translation.y+AtmosphereParameters^.Center.y,
                                             AtmosphereParameters^.Transform.Translation.z+AtmosphereParameters^.Center.z);
    ShellBottomRadius:=AtmosphereParameters^.BottomRadius;
    ShellTopRadius:=AtmosphereParameters^.TopRadius;
   end;
  end;

  fPushConstants.ShellCentreBottomRadius:=TpvVector4.InlineableCreate(ShellCentre.x,
                                                                      ShellCentre.y,
                                                                      ShellCentre.z,
                                                                      ShellBottomRadius);

  // ShellTopRadius is worked out above and deliberately not passed: the shader never read it. Left as a
  // local because the atmosphere it comes from may want it again, and recomputing it costs nothing here.
  //
  // The noise time is WRAPPED rather than handed over as it stands: the field is fetched at position plus
  // time, and a float that has been counting seconds since the game started loses the low bits the pattern
  // is made of. The wrap is at a power of two so that it does not show as a jump.
  fPushConstants.ScaleHeightsNoiseScaleTime:=TpvVector4.InlineableCreate(VolumetricScatteringRayleighScaleHeight,
                                                                         VolumetricScatteringMieScaleHeight,
                                                                         fInstance.VolumetricScatteringNoiseScale,
                                                                         Modulo(fInstance.VolumetricScatteringNoiseTime,4096.0));

  // The noise field's thickness is clamped at zero rather than passed through: a negative density is not
  // thin air, it is a medium that gains energy along the ray, and the exponential behind it turns that
  // into an overflow within a few steps. Left open at the top, where it only means thicker.
  fPushConstants.AerialWeightSkyDepthNoiseDensitySpare:=TpvVector4.InlineableCreate(fInstance.VolumetricScatteringAerialFactor,
                                                                                    VolumetricScatteringSkyDepth,
                                                                                    Max(fInstance.VolumetricScatteringNoiseDensityFactor,0.0),
                                                                                    0.0);

  fPushConstants.ViewBaseIndexFrameIndexFlagsStepCounts.x:=InFlightFrameState^.FinalViewIndex;
  fPushConstants.ViewBaseIndexFrameIndexFlagsStepCounts.y:=TpvUInt32(aFrameIndex);

  // The model bit is gone with the switch it stood for: both media are computed and added now, each with
  // its own share, so there is nothing left to select between.
  fPushConstants.ViewBaseIndexFrameIndexFlagsStepCounts.z:=0;
  if fInstance.VolumetricScatteringLegacyLook then begin
   fPushConstants.ViewBaseIndexFrameIndexFlagsStepCounts.z:=fPushConstants.ViewBaseIndexFrameIndexFlagsStepCounts.z or VolumetricScatteringFlagLegacyLook;
  end;

  // The step counts, fewest in the low sixteen bits and most in the high sixteen. At least one, or the
  // march divides by nothing and hands back infinities, and the most never below the fewest, or the
  // interpolation between them runs backwards. Both are held to what sixteen bits carry, which is far
  // above the 256 the shader caps at anyway.
  fPushConstants.ViewBaseIndexFrameIndexFlagsStepCounts.w:=TpvUInt32(Min(Max(1,fInstance.VolumetricScatteringMinimumStepCount),65535)) or
                                                            (TpvUInt32(Min(Max(Max(1,fInstance.VolumetricScatteringMinimumStepCount),
                                                                               fInstance.VolumetricScatteringMaximumStepCount),65535)) shl 16);

  // Everything half sized, in the order the shader's unpackHalf2x16 reads it: the low sixteen bits come
  // out as x.
  //
  // The tint is clamped at zero because a negative multiplier would take light out of the picture rather
  // than colour it, and left open at the top - above one is a legitimate way to ask for more of one
  // wavelength, and the buffers behind this are half floats that can carry it.
  //
  // The two shares are read only against each other; the shader divides them by their sum, so what is
  // written here is a ratio and not an amount. How thick each model is sits elsewhere, deliberately.
  //
  // The dust's x is the MIE shell - the one dust actually is. Above one is left open there too: a
  // variation wider than the density itself simply means the field reaches down to empty air in the
  // troughs, and the max() in the shader catches the rest.
  fPushConstants.PackedTintSharesDust.x:=TpvUInt32(TpvHalfFloat.FromFloat(Max(fInstance.VolumetricScatteringTint.x,0.0)).Value) or
                                          (TpvUInt32(TpvHalfFloat.FromFloat(Max(fInstance.VolumetricScatteringTint.y,0.0)).Value) shl 16);
  fPushConstants.PackedTintSharesDust.y:=TpvUInt32(TpvHalfFloat.FromFloat(Max(fInstance.VolumetricScatteringTint.z,0.0)).Value);
  fPushConstants.PackedTintSharesDust.z:=TpvUInt32(TpvHalfFloat.FromFloat(Max(fInstance.VolumetricScatteringRayleighMieFactor,0.0)).Value) or
                                          (TpvUInt32(TpvHalfFloat.FromFloat(Max(fInstance.VolumetricScatteringNoiseFieldFactor,0.0)).Value) shl 16);
  fPushConstants.PackedTintSharesDust.w:=TpvUInt32(TpvHalfFloat.FromFloat(Max(fInstance.VolumetricScatteringNoiseModulationMie,0.0)).Value) or
                                          (TpvUInt32(TpvHalfFloat.FromFloat(Max(fInstance.VolumetricScatteringNoiseModulationRayleigh,0.0)).Value) shl 16);

  aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                  TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_COMPUTE_BIT),
                                  0,
                                  SizeOf(TpvScene3DRendererPassesVolumetricScatteringRaymarchComputePass.TPushConstants),
                                  @fPushConstants);

  // Set 0 the scene's, set 1 this pass's own. The first is where the acceleration structure comes from,
  // maintained by the scene for this very in-flight frame.
  DescriptorSets[0]:=fInstance.Renderer.Scene3D.GlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle;
  DescriptorSets[1]:=fVulkanDescriptorSets[aInFlightFrameIndex].Handle;

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       fPipelineLayout.Handle,
                                       0,
                                       2,
                                       @DescriptorSets[0],
                                       0,
                                       nil);

  if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
   fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.BeginBreadcrumb(aCommandBuffer.Handle,TpvVulkanBreadcrumbType.Dispatch,'VolumetricScatteringRaymarch');
  end;

  // One invocation per pixel of the half-sized buffer, eight by eight to a workgroup.
  aCommandBuffer.CmdDispatch(Max(1,(fResourceOutputInscattering.Width+7) shr 3),
                             Max(1,(fResourceOutputInscattering.Height+7) shr 3),
                             CountViews);

  if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
   fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.EndBreadcrumb(aCommandBuffer.Handle);
  end;

 end;

 // And back, which is at the same time the barrier that makes the writes visible to the blur behind them.
 begin
  FillChar(ImageMemoryBarriers,SizeOf(ImageMemoryBarriers),#0);
  for Index:=0 to CountImageMemoryBarriers-1 do begin
   ImageMemoryBarriers[Index].sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
   ImageMemoryBarriers[Index].srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
   ImageMemoryBarriers[Index].dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT);
   ImageMemoryBarriers[Index].oldLayout:=VK_IMAGE_LAYOUT_GENERAL;
   ImageMemoryBarriers[Index].newLayout:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
   ImageMemoryBarriers[Index].srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
   ImageMemoryBarriers[Index].dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
   case Index of
    0:begin
     ImageMemoryBarriers[Index].image:=fResourceOutputInscattering.VulkanImages[aInFlightFrameIndex].Handle;
    end;
    1:begin
     ImageMemoryBarriers[Index].image:=fResourceOutputExtinction.VulkanImages[aInFlightFrameIndex].Handle;
    end;
    2:begin
     ImageMemoryBarriers[Index].image:=fResourceOutputDepth.VulkanImages[aInFlightFrameIndex].Handle;
    end;
    3:begin
     ImageMemoryBarriers[Index].image:=fResourceOutputFarInscattering.VulkanImages[aInFlightFrameIndex].Handle;
    end;
    else begin
     ImageMemoryBarriers[Index].image:=fResourceOutputFarExtinction.VulkanImages[aInFlightFrameIndex].Handle;
    end;
   end;
   ImageMemoryBarriers[Index].subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
   ImageMemoryBarriers[Index].subresourceRange.baseMipLevel:=0;
   ImageMemoryBarriers[Index].subresourceRange.levelCount:=1;
   ImageMemoryBarriers[Index].subresourceRange.baseArrayLayer:=0;
   ImageMemoryBarriers[Index].subresourceRange.layerCount:=CountViews;
  end;
  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    CountImageMemoryBarriers,@ImageMemoryBarriers[0]);
 end;

end;

end.
