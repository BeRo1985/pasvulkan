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
unit PasVulkan.Scene3D.Renderer.Passes.PlanetWaterCausticsComputePass;
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
     PasVulkan.Scene3D.Renderer.Instance,
     PasVulkan.Scene3D.Renderer.IBLDescriptor,
     PasVulkan.Scene3D.Planet;

type { TpvScene3DRendererPassesPlanetWaterCausticsComputePass }
     TpvScene3DRendererPassesPlanetWaterCausticsComputePass=class(TpvFrameGraph.TComputePass)
      private
       fInstance:TpvScene3DRendererInstance;
       fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceCascadedShadowMap:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceSSAO:TpvFrameGraph.TPass.TUsedImageResource;
       fPassVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fPassVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fPassVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
       fIBLDescriptors:array[0..MaxInFlightFrames-1] of TpvScene3DRendererIBLDescriptor;
       fVulkanStorageImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fVulkanPipelineLayout:TpvVulkanPipelineLayout;
       fVulkanComputeShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStageCompute:TpvVulkanPipelineShaderStage;
       fVulkanPipeline:TpvVulkanComputePipeline;
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

{ TpvScene3DRendererPassesPlanetWaterCausticsComputePass }

constructor TpvScene3DRendererPassesPlanetWaterCausticsComputePass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin
 inherited Create(aFrameGraph);

 fInstance:=aInstance;

 Name:='PlanetWaterCausticsComputePass';

 // Declare the scene colour buffer as a storage R/W output (Load = preserve
 // existing scene content so caustics are additively blended on top).
 fResourceColor:=AddImageOutput('resourcetype_color_optimized_non_alpha',
                                'resource_forwardrendering_color',
                                VK_IMAGE_LAYOUT_GENERAL,
                                TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Load),
                                []);

 fResourceCascadedShadowMap:=AddImageInput('resourcetype_cascadedshadowmap_data',
                                           'resource_cascadedshadowmap_data_final',
                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                           []);

 if fInstance.Renderer.ScreenSpaceAmbientOcclusion then begin
  fResourceSSAO:=AddImageInput('resourcetype_ambientocclusion_final',
                               'resource_ambientocclusion_data_final',
                               VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                               []);
 end else begin
  fResourceSSAO:=nil;
 end;

end;

destructor TpvScene3DRendererPassesPlanetWaterCausticsComputePass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesPlanetWaterCausticsComputePass.AcquirePersistentResources;
var Stream:TStream;
begin
 inherited AcquirePersistentResources;

 if TpvScene3DRendererInstance(fInstance).RaytracingActive then begin
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_water_caustics_raytracing_comp.spv');
 end else begin
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_water_caustics_bufref_comp.spv');
 end;
 try
  fVulkanComputeShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageCompute:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,
                                                                         fVulkanComputeShaderModule,
                                                                         'main');

end;

procedure TpvScene3DRendererPassesPlanetWaterCausticsComputePass.ReleasePersistentResources;
begin
 FreeAndNil(fVulkanPipelineShaderStageCompute);
 FreeAndNil(fVulkanComputeShaderModule);
 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesPlanetWaterCausticsComputePass.AcquireVolatileResources;
var InFlightFrameIndex:TpvSizeInt;
    CountViews:TpvInt32;
begin
 inherited AcquireVolatileResources;

 CountViews:=fInstance.CountSurfaceViews;

 // Descriptor set layout: mirrors WaterRenderPass bindings 0-8 plus storage image at 9.
 fPassVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fInstance.Renderer.VulkanDevice);
 fPassVulkanDescriptorSetLayout.AddBinding(0,
                                            VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                            1,
                                            TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                            []);
 fPassVulkanDescriptorSetLayout.AddBinding(1,
                                            VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                            3,
                                            TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                            []);
 fPassVulkanDescriptorSetLayout.AddBinding(2,
                                            VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                            6,
                                            TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                            []);
 fPassVulkanDescriptorSetLayout.AddBinding(3,
                                            VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                            1,
                                            TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                            []);
 fPassVulkanDescriptorSetLayout.AddBinding(4,
                                            VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                            1,
                                            TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                            []);
 fPassVulkanDescriptorSetLayout.AddBinding(5,
                                            VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                            3,
                                            TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                            []);
 fPassVulkanDescriptorSetLayout.AddBinding(6,
                                            VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                            1,
                                            TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                            []);
 fPassVulkanDescriptorSetLayout.AddBinding(7,
                                            VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                            1,
                                            TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                            []);
 fPassVulkanDescriptorSetLayout.AddBinding(8,
                                            VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                            1,
                                            TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                            []);
 fPassVulkanDescriptorSetLayout.AddBinding(9,
                                            VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                            1,
                                            TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                            []);
 fPassVulkanDescriptorSetLayout.Initialize;

 fPassVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(fInstance.Renderer.VulkanDevice,
                                                            TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                            fInstance.Renderer.CountInFlightFrames);
 fPassVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,13*fInstance.Renderer.CountInFlightFrames);
 fPassVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,3*fInstance.Renderer.CountInFlightFrames);
 fPassVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,2*fInstance.Renderer.CountInFlightFrames);
 fPassVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,fInstance.Renderer.CountInFlightFrames);
 fPassVulkanDescriptorPool.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin

  // Create a 2D-array image view covering all surface-view layers for the storage image.
  fVulkanStorageImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(
   fInstance.Renderer.VulkanDevice,
   fResourceColor.VulkanImages[InFlightFrameIndex],
   TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY),
   TpvFrameGraph.TImageResourceType(fResourceColor.ResourceType).Format,
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

  fPassVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fPassVulkanDescriptorPool,
                                                                                fPassVulkanDescriptorSetLayout);

  // Binding 0 – view UBO
  fPassVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                      0,
                                                                      1,
                                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                      [],
                                                                      [fInstance.VulkanViewUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                      [],
                                                                      false);

  // Binding 1 – BRDF look-up textures (GGX, Charlie, SheenE)
  fPassVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                      0,
                                                                      3,
                                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                      [fInstance.Renderer.GGXBRDF.DescriptorImageInfo,
                                                                       fInstance.Renderer.CharlieBRDF.DescriptorImageInfo,
                                                                       fInstance.Renderer.SheenEBRDF.DescriptorImageInfo],
                                                                      [],
                                                                      [],
                                                                      false);

  // Binding 2 – IBL env-map cube maps (6 slots; refreshed via IBLDescriptor each frame)
  fPassVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                      0,
                                                                      6,
                                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                      [fInstance.Renderer.ImageBasedLightingEnvMapCubeMaps.GGXDescriptorImageInfo,
                                                                       fInstance.Renderer.ImageBasedLightingEnvMapCubeMaps.CharlieDescriptorImageInfo,
                                                                       fInstance.Renderer.ImageBasedLightingEnvMapCubeMaps.LambertianDescriptorImageInfo,
                                                                       fInstance.Renderer.ImageBasedLightingEnvMapCubeMaps.GGXDescriptorImageInfo,
                                                                       fInstance.Renderer.ImageBasedLightingEnvMapCubeMaps.CharlieDescriptorImageInfo,
                                                                       fInstance.Renderer.ImageBasedLightingEnvMapCubeMaps.LambertianDescriptorImageInfo],
                                                                      [],
                                                                      [],
                                                                      false);

  // Binding 3 – cascaded shadow map UBO
  fPassVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                      0,
                                                                      1,
                                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                      [],
                                                                      [fInstance.CascadedShadowMapVulkanUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                      [],
                                                                      false);

  // Binding 4 – cascaded shadow map texture
  fPassVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                                      0,
                                                                      1,
                                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                      [TVkDescriptorImageInfo.Create(fInstance.Renderer.ShadowMapSampler.Handle,
                                                                                                     fResourceCascadedShadowMap.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                     fResourceCascadedShadowMap.ResourceTransition.Layout)],
                                                                      [],
                                                                      [],
                                                                      false);

  // Binding 5 – SSAO / scene colour mip / depth mip array
  if fInstance.Renderer.ScreenSpaceAmbientOcclusion then begin
   fPassVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(5,
                                                                       0,
                                                                       3,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [TVkDescriptorImageInfo.Create(fInstance.Renderer.AmbientOcclusionSampler.Handle,
                                                                                                      fResourceSSAO.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceSSAO.ResourceTransition.Layout),
                                                                        TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedSampler.Handle,
                                                                                                      fInstance.SceneMipmappedArray2DImage.VulkanArrayImageView.Handle,
                                                                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL),
                                                                        TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                                      fInstance.DepthMipmappedArray2DImages[InFlightFrameIndex].VulkanArrayImageView.Handle,
                                                                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                       [],
                                                                       [],
                                                                       false);
  end else begin
   fPassVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(5,
                                                                       0,
                                                                       3,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [TVkDescriptorImageInfo.Create(fInstance.Renderer.AmbientOcclusionSampler.Handle,
                                                                                                      fInstance.Renderer.EmptyAmbientOcclusionTexture.ImageView.Handle,
                                                                                                      TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)),
                                                                        TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedSampler.Handle,
                                                                                                      fInstance.SceneMipmappedArray2DImage.VulkanArrayImageView.Handle,
                                                                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL),
                                                                        TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                                      fInstance.DepthMipmappedArray2DImages[InFlightFrameIndex].VulkanArrayImageView.Handle,
                                                                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                       [],
                                                                       [],
                                                                       false);
  end;

  // Binding 6 – frustum cluster grid globals UBO
  fPassVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(6,
                                                                      0,
                                                                      1,
                                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                      [],
                                                                      [fInstance.FrustumClusterGridGlobalsVulkanBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                      [],
                                                                      false);

  // Binding 7 – frustum cluster grid index list SSBO
  fPassVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(7,
                                                                      0,
                                                                      1,
                                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                      [],
                                                                      [fInstance.FrustumClusterGridIndexListVulkanBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                      [],
                                                                      false);

  // Binding 8 – frustum cluster grid data SSBO
  fPassVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(8,
                                                                      0,
                                                                      1,
                                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                      [],
                                                                      [fInstance.FrustumClusterGridDataVulkanBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                      [],
                                                                      false);

  // Binding 9 – scene colour storage image (rgba16f, 2D array)
  fPassVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(9,
                                                                      0,
                                                                      1,
                                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                      [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                     fVulkanStorageImageViews[InFlightFrameIndex].Handle,
                                                                                                     VK_IMAGE_LAYOUT_GENERAL)],
                                                                      [],
                                                                      [],
                                                                      false);

  fPassVulkanDescriptorSets[InFlightFrameIndex].Flush;

  fIBLDescriptors[InFlightFrameIndex]:=TpvScene3DRendererIBLDescriptor.Create(fInstance.Renderer.VulkanDevice,
                                                                               fPassVulkanDescriptorSets[InFlightFrameIndex],
                                                                               2,
                                                                               fInstance.Renderer.ClampedSampler.Handle);

 end;

 // Pipeline layout: set 0 = global, set 1 = pass (bindings 0-9), set 2 = per-planet.
 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(fInstance.Renderer.VulkanDevice);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                             0,
                                             SizeOf(TpvScene3DPlanet.TWaterRenderPass.TPushConstants));
 fVulkanPipelineLayout.AddDescriptorSetLayout(fInstance.Renderer.Scene3D.GlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fPassVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddDescriptorSetLayout(TpvScene3D(fInstance.Renderer.Scene3D).PlanetDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 fVulkanPipeline:=TpvVulkanComputePipeline.Create(fInstance.Renderer.VulkanDevice,
                                                   pvApplication.VulkanPipelineCache,
                                                   TVkPipelineCreateFlags(0),
                                                   fVulkanPipelineShaderStageCompute,
                                                   fVulkanPipelineLayout,
                                                   nil,
                                                   0);

end;

procedure TpvScene3DRendererPassesPlanetWaterCausticsComputePass.ReleaseVolatileResources;
var InFlightFrameIndex:TpvSizeInt;
begin
 FreeAndNil(fVulkanPipeline);
 FreeAndNil(fVulkanPipelineLayout);
 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fIBLDescriptors[InFlightFrameIndex]);
  FreeAndNil(fPassVulkanDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fVulkanStorageImageViews[InFlightFrameIndex]);
 end;
 FreeAndNil(fPassVulkanDescriptorPool);
 FreeAndNil(fPassVulkanDescriptorSetLayout);
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesPlanetWaterCausticsComputePass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvScene3DRendererPassesPlanetWaterCausticsComputePass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var InFlightFrameState:TpvScene3DRendererInstance.PInFlightFrameState;
    PlanetIndex:TpvSizeInt;
    Planet:TpvScene3DPlanet;
    PlanetRendererInstance:TpvScene3DPlanet.TRendererInstance;
    PlanetRendererViewInstance:TpvScene3DPlanet.TRendererViewInstance;
    PushConstants:TpvScene3DPlanet.TWaterRenderPass.TPushConstants;
    DescriptorSets:array[0..2] of TVkDescriptorSet;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 InFlightFrameState:=@fInstance.InFlightFrameStates^[aInFlightFrameIndex];
 if not InFlightFrameState^.Ready then begin
  exit;
 end;

 fIBLDescriptors[aInFlightFrameIndex].SetFrom(fInstance.Renderer.Scene3D,fInstance,aInFlightFrameIndex);
 fIBLDescriptors[aInFlightFrameIndex].Update(true);

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fVulkanPipeline.Handle);

 // Set 0: global descriptor set
 DescriptorSets[0]:=fInstance.Renderer.Scene3D.GlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle;
 // Set 1: pass descriptor set (views, BRDF, env, shadows, depth, cluster, storage image)
 DescriptorSets[1]:=fPassVulkanDescriptorSets[aInFlightFrameIndex].Handle;

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       fVulkanPipelineLayout.Handle,
                                       0,
                                       2,
                                       @DescriptorSets[0],
                                       0,
                                       nil);

 TpvScene3DPlanets(TpvScene3D(fInstance.Renderer.Scene3D).Planets).Lock.AcquireRead;
 try
  for PlanetIndex:=0 to TpvScene3DPlanets(TpvScene3D(fInstance.Renderer.Scene3D).Planets).Count-1 do begin
   Planet:=TpvScene3DPlanets(TpvScene3D(fInstance.Renderer.Scene3D).Planets).Items[PlanetIndex];
   if assigned(Planet) and Planet.fReady and Planet.fInFlightFrameReady[aInFlightFrameIndex] then begin
    if Planet.fRendererInstanceHashMap.TryGet(fInstance,PlanetRendererInstance) then begin
     if Planet.fRendererViewInstanceHashMap.TryGet(TpvScene3DPlanet.TRendererViewInstance.TKey.Create(fInstance,TpvScene3DRendererRenderPass.View),PlanetRendererViewInstance) then begin

      // Set 2: per-planet descriptor set
      DescriptorSets[2]:=Planet.fPlanetDescriptorSets[aInFlightFrameIndex].Handle;
      aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                            fVulkanPipelineLayout.Handle,
                                            2,
                                            1,
                                            @DescriptorSets[2],
                                            0,
                                            nil);

      // Fill push constants identical to TWaterRenderPass.Draw
      FillChar(PushConstants,SizeOf(TpvScene3DPlanet.TWaterRenderPass.TPushConstants),#0);
      PushConstants.ViewBaseIndex:=InFlightFrameState^.FinalViewIndex;
      PushConstants.CountViews:=InFlightFrameState^.CountFinalViews;
      PushConstants.CountAllViews:=TpvScene3DRendererInstance(fInstance).Views[aInFlightFrameIndex].Count;
      PushConstants.CountQuadPointsInOneDirection:=64;
      PushConstants.ResolutionXY:=(fInstance.Width and $ffff) or ((fInstance.Height and $ffff) shl 16);
      PushConstants.TessellationFactor:=1.0/4.0;
      PushConstants.TileMapResolution:=Planet.TileMapResolution;
      PushConstants.Flags:=TpvScene3DRendererInstance(fInstance).RawRaytracingFlags;
      PushConstants.FrameIndex:=aFrameIndex;
      PushConstants.Time:=Modulo(TpvScene3D(Planet.Scene3D).SceneTimes^[aInFlightFrameIndex],65536.0);
      PushConstants.PlanetData:=Planet.fPlanetDataVulkanBuffers[aInFlightFrameIndex].DeviceAddress;
      PushConstants.Jitter:=TpvScene3DRendererInstance(fInstance).InFlightFrameStates[aInFlightFrameIndex].Jitter;

      aCommandBuffer.CmdPushConstants(fVulkanPipelineLayout.Handle,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       0,
                                       SizeOf(TpvScene3DPlanet.TWaterRenderPass.TPushConstants),
                                       @PushConstants);

      aCommandBuffer.CmdDispatch(Max(1,(fInstance.Width+15) shr 4),
                                  Max(1,(fInstance.Height+15) shr 4),
                                  InFlightFrameState^.CountFinalViews);

     end;
    end;
   end;
  end;
 finally
  TpvScene3DPlanets(TpvScene3D(fInstance.Renderer.Scene3D).Planets).Lock.ReleaseRead;
 end;

end;

end.
