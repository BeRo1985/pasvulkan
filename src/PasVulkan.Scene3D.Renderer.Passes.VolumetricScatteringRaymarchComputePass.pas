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
             VolumetricScatteringMaximumDistance=4096.0;
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
       const // Bit zero of the flag word: which step length the march uses. See the property of the same
             // name on the instance for what the two mean.
             VolumetricScatteringFlagRayLengthSegments=TpvUInt32(1) shl 0;
      public
       type TPushConstants=packed record
             SunDirectionStrength:TpvVector4;   // xyz = the way the light travels, w = strength
             // x = how depth becomes a distance, y = how far a ray without geometry reaches, z = the
             // strength of the light being scattered
             ZNearMaximumDistanceSunIntensitySpare:TpvVector4;
             CentreBottomRadiusTop:TpvVector4;  // xyz = the shell's centre, w = its bottom radius
             TopRadiusHeightsSpare:TpvVector4;  // x = top radius, y = Rayleigh height, z = Mie height
             // x = the first view of this pass, y = the frame counter the shadow noise is decorrelated by
             ViewBaseIndexFrameIndexSpare:TpvUInt32Vector4;
             // Everything that is a whole number or a switch, in a word of its own rather than squeezed
             // into the spare lanes of the float vectors: x = flags, y = the fewest steps a ray is walked
             // in, z = the most.
             FlagsStepCountsSpare:TpvUInt32Vector4;
            end;
      private
       fInstance:TpvScene3DRendererInstance;
       // Whether this pass traces its shadows rather than looking them up in the cascaded shadow maps.
       // Decided once, when the shader is chosen, because the descriptor set has to match it.
       fRaytracing:Boolean;
       fPushConstants:TPushConstants;
       fResourceOutput:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceCascadedShadowMap:TpvFrameGraph.TPass.TUsedImageResource;
       // Which acceleration structure this pass's descriptor set currently points at, per in-flight frame.
       // Kept because the scene rebuilds its top level structure as it changes, and a descriptor written
       // once at setup goes on pointing at whatever was there then - every ray misses and nothing casts a
       // shadow at all. Its own bookkeeping and not the scene's, whose slots belong to the global set.
       fBoundAccelerationStructures:array[0..MaxInFlightFrames-1] of TVkAccelerationStructureKHR;
       fBoundAccelerationStructureGenerations:array[0..MaxInFlightFrames-1] of TpvUInt64;
       fOutputImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
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
 fResourceOutput:=AddImageOutput('resourcetype_volumetric_scattering',
                                 'resource_volumetric_scattering',
                                 VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare),
                                 []
                                );

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
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('volumetric_scattering_raymarch_raytracing_comp.spv');
 end else begin
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('volumetric_scattering_raymarch_comp.spv');
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
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,fInstance.Renderer.CountInFlightFrames);
 if fRaytracing then begin
  fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR,fInstance.Renderer.CountInFlightFrames);
 end;
 fVulkanDescriptorPool.Initialize;

 // Everything in the one set: the shadow readers elsewhere use set 3 because they share a pipeline layout
 // with the atmosphere, and this pass has one of its own. See the note in the shader.
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
                                       VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(4,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 if fRaytracing then begin
  fVulkanDescriptorSetLayout.AddBinding(5,
                                        VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR,
                                        1,
                                        TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                        []);
 end;
 fVulkanDescriptorSetLayout.Initialize;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(fInstance.Renderer.VulkanDevice);
 fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TpvScene3DRendererPassesVolumetricScatteringRaymarchComputePass.TPushConstants));
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

  fOutputImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                                                   fResourceOutput.VulkanImages[InFlightFrameIndex],
                                                                   TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY),
                                                                   TpvFrameGraph.TImageResourceType(fResourceOutput.ResourceType).Format,
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
                                                                                                fOutputImageViews[InFlightFrameIndex].Handle,
                                                                                                VK_IMAGE_LAYOUT_GENERAL)],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );

  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
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
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
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

  // The acceleration structure is NOT written here. It is written in Execute, and rewritten whenever the
  // scene has rebuilt it - see the note at fBoundAccelerationStructures.
  fBoundAccelerationStructures[InFlightFrameIndex]:=VK_NULL_HANDLE;
  fBoundAccelerationStructureGenerations[InFlightFrameIndex]:=0;

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
  FreeAndNil(fOutputImageViews[InFlightFrameIndex]);
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
var CountViews:TpvInt32;
    InFlightFrameState:TpvScene3DRendererInstance.PInFlightFrameState;
    SunDirection,ShellCentre:TpvVector3;
    ShellBottomRadius,ShellTopRadius:TpvFloat;
    Atmospheres:TpvScene3DAtmospheres;
    AtmosphereParameters:TpvScene3DAtmosphere.PAtmosphereParameters;
    TopLevelAccelerationStructure:TpvRaytracingTopLevelAccelerationStructure;
    ImageMemoryBarrier:TVkImageMemoryBarrier;
begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 CountViews:=fInstance.CountSurfaceViews;

 InFlightFrameState:=@fInstance.InFlightFrameStates^[aInFlightFrameIndex];

 // The acceleration structure, re-pointed whenever the scene has built a new one. The scene does exactly
 // this for its own global set, and for the same reason: the handle changes as the world changes, and a
 // descriptor left pointing at the old one traces against nothing. Both the handle and its generation are
 // compared, because a structure can be rebuilt in place and keep its handle.
 if fRaytracing then begin
  TopLevelAccelerationStructure:=fInstance.Renderer.Scene3D.Raytracing.TopLevelAccelerationStructure;
  if assigned(TopLevelAccelerationStructure) and
     ((fBoundAccelerationStructures[aInFlightFrameIndex]<>TopLevelAccelerationStructure.AccelerationStructure) or
      (fBoundAccelerationStructureGenerations[aInFlightFrameIndex]<>TopLevelAccelerationStructure.Generation)) then begin
   fBoundAccelerationStructures[aInFlightFrameIndex]:=TopLevelAccelerationStructure.AccelerationStructure;
   fBoundAccelerationStructureGenerations[aInFlightFrameIndex]:=TopLevelAccelerationStructure.Generation;
   fVulkanDescriptorSets[aInFlightFrameIndex].WriteToDescriptorSet(5,
                                                                   0,
                                                                   1,
                                                                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR),
                                                                   [],
                                                                   [],
                                                                   [],
                                                                   [TopLevelAccelerationStructure.AccelerationStructure],
                                                                   true
                                                                  );
  end;
 end;

 // The frame graph hands the output in as SHADER_READ_ONLY_OPTIMAL, which is the one layout a storage
 // image may not be written in, so the pass lays it over itself.
 begin
  FillChar(ImageMemoryBarrier,SizeOf(TVkImageMemoryBarrier),#0);
  ImageMemoryBarrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
  ImageMemoryBarrier.srcAccessMask:=0;
  ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
  ImageMemoryBarrier.oldLayout:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
  ImageMemoryBarrier.newLayout:=VK_IMAGE_LAYOUT_GENERAL;
  ImageMemoryBarrier.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  ImageMemoryBarrier.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  ImageMemoryBarrier.image:=fResourceOutput.VulkanImages[aInFlightFrameIndex].Handle;
  ImageMemoryBarrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
  ImageMemoryBarrier.subresourceRange.baseMipLevel:=0;
  ImageMemoryBarrier.subresourceRange.levelCount:=1;
  ImageMemoryBarrier.subresourceRange.baseArrayLayer:=0;
  ImageMemoryBarrier.subresourceRange.layerCount:=CountViews;
  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    1,@ImageMemoryBarrier);
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

  fPushConstants.SunDirectionStrength:=TpvVector4.InlineableCreate(SunDirection.x,
                                                                   SunDirection.y,
                                                                   SunDirection.z,
                                                                   fInstance.VolumetricScatteringFactor);

  fPushConstants.ZNearMaximumDistanceSunIntensitySpare:=TpvVector4.InlineableCreate(fInstance.ZNear,
                                                                                    VolumetricScatteringMaximumDistance,
                                                                                    fInstance.VolumetricScatteringSunIntensity,
                                                                                    0.0);

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

  fPushConstants.CentreBottomRadiusTop:=TpvVector4.InlineableCreate(ShellCentre.x,
                                                                    ShellCentre.y,
                                                                    ShellCentre.z,
                                                                    ShellBottomRadius);

  fPushConstants.TopRadiusHeightsSpare:=TpvVector4.InlineableCreate(ShellTopRadius,
                                                                    VolumetricScatteringRayleighScaleHeight,
                                                                    VolumetricScatteringMieScaleHeight,
                                                                    0.0);

  fPushConstants.ViewBaseIndexFrameIndexSpare.x:=InFlightFrameState^.FinalViewIndex;
  fPushConstants.ViewBaseIndexFrameIndexSpare.y:=TpvUInt32(aFrameIndex);
  fPushConstants.ViewBaseIndexFrameIndexSpare.z:=0;
  fPushConstants.ViewBaseIndexFrameIndexSpare.w:=0;

  fPushConstants.FlagsStepCountsSpare.x:=0;
  if fInstance.VolumetricScatteringRayLengthSegments then begin
   fPushConstants.FlagsStepCountsSpare.x:=fPushConstants.FlagsStepCountsSpare.x or VolumetricScatteringFlagRayLengthSegments;
  end;
  // At least one step, or the march divides by nothing and hands back infinities, and the most never below
  // the fewest, or the interpolation between them runs backwards.
  fPushConstants.FlagsStepCountsSpare.y:=TpvUInt32(Max(1,fInstance.VolumetricScatteringMinimumStepCount));
  fPushConstants.FlagsStepCountsSpare.z:=TpvUInt32(Max(Max(1,fInstance.VolumetricScatteringMinimumStepCount),
                                                      fInstance.VolumetricScatteringMaximumStepCount));
  fPushConstants.FlagsStepCountsSpare.w:=0;

  aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                  TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_COMPUTE_BIT),
                                  0,
                                  SizeOf(TpvScene3DRendererPassesVolumetricScatteringRaymarchComputePass.TPushConstants),
                                  @fPushConstants);

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       fPipelineLayout.Handle,
                                       0,
                                       1,
                                       @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil);

  if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
   fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.BeginBreadcrumb(aCommandBuffer.Handle,TpvVulkanBreadcrumbType.Dispatch,'VolumetricScatteringRaymarch');
  end;

  // One invocation per pixel of the half-sized buffer, eight by eight to a workgroup.
  aCommandBuffer.CmdDispatch(Max(1,(fResourceOutput.Width+7) shr 3),
                             Max(1,(fResourceOutput.Height+7) shr 3),
                             CountViews);

  if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
   fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.EndBreadcrumb(aCommandBuffer.Handle);
  end;

 end;

 // And back, which is at the same time the barrier that makes the write visible to the blur behind it.
 begin
  FillChar(ImageMemoryBarrier,SizeOf(TVkImageMemoryBarrier),#0);
  ImageMemoryBarrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
  ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
  ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT);
  ImageMemoryBarrier.oldLayout:=VK_IMAGE_LAYOUT_GENERAL;
  ImageMemoryBarrier.newLayout:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
  ImageMemoryBarrier.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  ImageMemoryBarrier.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  ImageMemoryBarrier.image:=fResourceOutput.VulkanImages[aInFlightFrameIndex].Handle;
  ImageMemoryBarrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
  ImageMemoryBarrier.subresourceRange.baseMipLevel:=0;
  ImageMemoryBarrier.subresourceRange.levelCount:=1;
  ImageMemoryBarrier.subresourceRange.baseArrayLayer:=0;
  ImageMemoryBarrier.subresourceRange.layerCount:=CountViews;
  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    1,@ImageMemoryBarrier);
 end;

end;

end.
