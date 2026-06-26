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
unit PasVulkan.Scene3D.Renderer.Passes.GlobalIlluminationDDGIRSMSplatComputePass;
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
     PasVulkan.Scene3D.Renderer.IBLDescriptor;

type { TpvScene3DRendererPassesGlobalIlluminationDDGIRSMSplatComputePass }
     // Non-raytraced DDGI ray-data PRODUCER (Reflective Shadow Map fallback). For hardware without VK_KHR_ray_query it treats
     // a subset of the sun's RSM texels as virtual point lights and splats them into the same per-(ray, probe) ray-data slots
     // the ray-query trace pass would have written. It is a drop-in alternative to the trace producer: everything downstream
     // (irradiance / visibility / glossy blend, border) depends only on the ray-data, so those stages are unchanged.
     // Set 0 = the RSM source (color/normal/depth + the radiance-hints RSM UBO it shares); set 1 = the DDGI field (ddgiData
     // SSBO + the 6 environment cubemaps for sky-on-miss). Dispatch is identical to the trace: one thread per (ray, probe).
     TpvScene3DRendererPassesGlobalIlluminationDDGIRSMSplatComputePass=class(TpvFrameGraph.TComputePass)
      public
       type TPushConstants=record
             RandomRotation0:TpvVector4;         // mat3 column 0 in xyz
             RandomRotation1:TpvVector4;         // mat3 column 1 in xyz
             RandomRotation2:TpvVector4;         // mat3 column 2 in xyz
             Params:TpvUInt32Vector4;            // x = frameIndex, y = countCascades, z = probesPerCascade, w = raysPerProbe
             Blend:TpvVector4;                   // x = irradiance blend, y = multi-bounce feedback strength, z = first-frame relocation gate (< 0.5 applies the offset)
             EmissiveGIParticleCount:TpvVector4; // x = emissive GI scale, y = emissive GI maximum, z = alive particle count (0 disables particle injection)
             ParticleBVH:TpvUInt32Vector4;       // particle LBVH device addresses: xy = emitter buffer (uvec2), zw = node buffer (uvec2); 0 when inactive
            end;
            PPushConstants=^TPushConstants;
      private
       fInstance:TpvScene3DRendererInstance;
       fResourceRSMColor:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceRSMNormalUsed:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceRSMDepth:TpvFrameGraph.TPass.TUsedImageResource;
       fComputeShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStage:TpvVulkanPipelineShaderStage;
       fRSMDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fRSMDescriptorPool:TpvVulkanDescriptorPool;
       fRSMDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
       fDDGIDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fDDGIDescriptorPool:TpvVulkanDescriptorPool;
       fDDGIDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
       fIBLDescriptors:array[0..MaxInFlightFrames-1] of TpvScene3DRendererIBLDescriptor; // set 1 binding 4 (6 env cubemaps) for sky-on-miss
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
      published
     end;

implementation

{ TpvScene3DRendererPassesGlobalIlluminationDDGIRSMSplatComputePass }

constructor TpvScene3DRendererPassesGlobalIlluminationDDGIRSMSplatComputePass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin

 inherited Create(aFrameGraph);

 fInstance:=aInstance;

 Name:='GlobalIlluminationDDGIRSMSplatComputePass';

 // The sun's RSM (rendered by the ReflectiveShadowMapRenderPass): flux/color, encoded normal + used flag, light-space depth.
 // AddImageInput makes the frame graph transition them to shader-read and order this pass after the RSM render pass.
 fResourceRSMColor:=AddImageInput('resourcetype_reflectiveshadowmap_color',
                                  'resource_reflectiveshadowmap_color',
                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                  []
                                 );

 fResourceRSMNormalUsed:=AddImageInput('resourcetype_reflectiveshadowmap_normalused',
                                       'resource_reflectiveshadowmap_normalused',
                                       VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                       []
                                      );

 fResourceRSMDepth:=AddImageInput('resourcetype_reflectiveshadowmap_depth',
                                  'resource_reflectiveshadowmap_depth',
                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                  []
                                 );

end;

destructor TpvScene3DRendererPassesGlobalIlluminationDDGIRSMSplatComputePass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationDDGIRSMSplatComputePass.AcquirePersistentResources;
var Stream:TStream;
begin
 inherited AcquirePersistentResources;
 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('gi_ddgi_rsm_splat_comp.spv');
 try
  fComputeShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;
 fVulkanPipelineShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');
end;

procedure TpvScene3DRendererPassesGlobalIlluminationDDGIRSMSplatComputePass.ReleasePersistentResources;
begin
 FreeAndNil(fVulkanPipelineShaderStage);
 FreeAndNil(fComputeShaderModule);
 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationDDGIRSMSplatComputePass.AcquireVolatileResources;
var InFlightFrameIndex:TpvInt32;
begin

 inherited AcquireVolatileResources;

 // Set 0 = RSM source: 3 combined image samplers (color/normal/depth) + the radiance-hints RSM UBO (shared).
 fRSMDescriptorPool:=TpvVulkanDescriptorPool.Create(fInstance.Renderer.VulkanDevice,
                                                    TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                    fInstance.Renderer.CountInFlightFrames);
 fRSMDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fInstance.Renderer.CountInFlightFrames*3);
 fRSMDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,fInstance.Renderer.CountInFlightFrames);
 fRSMDescriptorPool.Initialize;

 fRSMDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fInstance.Renderer.VulkanDevice);
 fRSMDescriptorSetLayout.AddBinding(0,VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]); // RSM color (flux)
 fRSMDescriptorSetLayout.AddBinding(1,VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]); // RSM normal + used flag
 fRSMDescriptorSetLayout.AddBinding(2,VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]); // RSM depth
 fRSMDescriptorSetLayout.AddBinding(3,VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);         // RSM matrices UBO (shared radiance-hints buffer)
 fRSMDescriptorSetLayout.Initialize;

 // Set 1 = DDGI resources: ddgiData SSBO (cascade globals + sub-buffer pointers, incl. ray-data) + 6 env cubemaps (sky-on-miss).
 // Plus the previous-frame multi-bounce reads (mirrors the trace pass): octahedral irradiance at binding 2 (octahedral storage
 // only; SH irradiance is the master BDA buffer, no image) and the visibility moments at binding 3.
 fDDGIDescriptorPool:=TpvVulkanDescriptorPool.Create(fInstance.Renderer.VulkanDevice,
                                                     TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                     fInstance.Renderer.CountInFlightFrames);
 fDDGIDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,fInstance.Renderer.CountInFlightFrames); // binding 0 = ddgiData SSBO
 if TpvScene3DRendererInstance.GlobalIlluminationDDGIStorageOctahedral then begin
  fDDGIDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,fInstance.Renderer.CountInFlightFrames*2); // binding 2 = oct irradiance read + binding 3 = visibility read (multi-bounce)
 end else begin
  fDDGIDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,fInstance.Renderer.CountInFlightFrames); // binding 3 = visibility read only (SH irradiance is a BDA buffer via the master)
 end;
 fDDGIDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fInstance.Renderer.CountInFlightFrames*6);
 fDDGIDescriptorPool.Initialize;

 fDDGIDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fInstance.Renderer.VulkanDevice);
 fDDGIDescriptorSetLayout.AddBinding(0,VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);        // ddgiData SSBO
 if TpvScene3DRendererInstance.GlobalIlluminationDDGIStorageOctahedral then begin
  fDDGIDescriptorSetLayout.AddBinding(2,VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);       // oct irradiance read (multi-bounce)
 end;
 fDDGIDescriptorSetLayout.AddBinding(3,VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);        // visibility moments read (multi-bounce)
 fDDGIDescriptorSetLayout.AddBinding(4,VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,6,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]); // 6 env cubemaps (sky-on-miss)
 fDDGIDescriptorSetLayout.Initialize;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(fInstance.Renderer.VulkanDevice);
 fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
 fPipelineLayout.AddDescriptorSetLayout(fRSMDescriptorSetLayout);  // set 0 = RSM source
 fPipelineLayout.AddDescriptorSetLayout(fDDGIDescriptorSetLayout); // set 1 = DDGI field + env cubemaps
 fPipelineLayout.Initialize;

 fPipeline:=TpvVulkanComputePipeline.Create(fInstance.Renderer.VulkanDevice,fInstance.Renderer.VulkanPipelineCache,0,fVulkanPipelineShaderStage,fPipelineLayout,nil,0);

 for InFlightFrameIndex:=0 to fInstance.Renderer.CountInFlightFrames-1 do begin

  fRSMDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fRSMDescriptorPool,fRSMDescriptorSetLayout);
  fRSMDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,0,1,TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                              [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedSampler.Handle,
                                                                                             fResourceRSMColor.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                             fResourceRSMColor.ResourceTransition.Layout)],[],[],false);
  fRSMDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,0,1,TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                              [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedSampler.Handle,
                                                                                             fResourceRSMNormalUsed.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                             fResourceRSMNormalUsed.ResourceTransition.Layout)],[],[],false);
  fRSMDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,0,1,TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                              [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedSampler.Handle,
                                                                                             fResourceRSMDepth.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                             fResourceRSMDepth.ResourceTransition.Layout)],[],[],false);
  fRSMDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,0,1,TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                              [],[fInstance.GlobalIlluminationRadianceHintsRSMUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],[],false);
  fRSMDescriptorSets[InFlightFrameIndex].Flush;

  fDDGIDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fDDGIDescriptorPool,fDDGIDescriptorSetLayout);
  fDDGIDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,0,1,TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),[],[fInstance.GlobalIlluminationDDGIMasterBuffers[InFlightFrameIndex].DescriptorBufferInfo],[],false); // binding 0 = ddgiData SSBO
  if TpvScene3DRendererInstance.GlobalIlluminationDDGIStorageOctahedral then begin
   fDDGIDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,0,1,TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                               [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,fInstance.GlobalIlluminationDDGIIrradianceOctImages[InFlightFrameIndex].VulkanImageView.Handle,VK_IMAGE_LAYOUT_GENERAL)],[],[],false); // binding 2 = oct irradiance read (multi-bounce)
  end;
  fDDGIDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,0,1,TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                              [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,fInstance.GlobalIlluminationDDGIVisibilityMomentsImages[InFlightFrameIndex].VulkanImageView.Handle,VK_IMAGE_LAYOUT_GENERAL)],[],[],false); // binding 3 = visibility moments read (multi-bounce)
  fDDGIDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,0,6,TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                              [fInstance.Renderer.ImageBasedLightingEnvMapCubeMaps.GGXDescriptorImageInfo,
                                                               fInstance.Renderer.ImageBasedLightingEnvMapCubeMaps.CharlieDescriptorImageInfo,
                                                               fInstance.Renderer.ImageBasedLightingEnvMapCubeMaps.LambertianDescriptorImageInfo,
                                                               fInstance.Renderer.ImageBasedLightingEnvMapCubeMaps.GGXDescriptorImageInfo,
                                                               fInstance.Renderer.ImageBasedLightingEnvMapCubeMaps.CharlieDescriptorImageInfo,
                                                               fInstance.Renderer.ImageBasedLightingEnvMapCubeMaps.LambertianDescriptorImageInfo],[],[],false);
  fDDGIDescriptorSets[InFlightFrameIndex].Flush;

  // The IBL descriptor keeps set 1 binding 4 (the 6 env cubemaps) in sync with the active scene/atmosphere each frame, so
  // sky-on-miss matches the trace producer. Same wiring as the DDGI trace pass.
  fIBLDescriptors[InFlightFrameIndex]:=TpvScene3DRendererIBLDescriptor.Create(fInstance.Renderer.VulkanDevice,fDDGIDescriptorSets[InFlightFrameIndex],4,fInstance.Renderer.ClampedSampler.Handle);
  fIBLDescriptors[InFlightFrameIndex].SetFrom(fInstance.Scene3D,fInstance,InFlightFrameIndex);
  fIBLDescriptors[InFlightFrameIndex].Update(true);

 end;

end;

procedure TpvScene3DRendererPassesGlobalIlluminationDDGIRSMSplatComputePass.ReleaseVolatileResources;
var InFlightFrameIndex:TpvInt32;
begin
 FreeAndNil(fPipeline);
 FreeAndNil(fPipelineLayout);
 for InFlightFrameIndex:=0 to fInstance.Renderer.CountInFlightFrames-1 do begin
  FreeAndNil(fIBLDescriptors[InFlightFrameIndex]);
  FreeAndNil(fDDGIDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fRSMDescriptorSets[InFlightFrameIndex]);
 end;
 FreeAndNil(fDDGIDescriptorSetLayout);
 FreeAndNil(fDDGIDescriptorPool);
 FreeAndNil(fRSMDescriptorSetLayout);
 FreeAndNil(fRSMDescriptorPool);
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationDDGIRSMSplatComputePass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
 if assigned(fIBLDescriptors[aUpdateInFlightFrameIndex]) then begin
  fIBLDescriptors[aUpdateInFlightFrameIndex].SetFrom(fInstance.Scene3D,fInstance,aUpdateInFlightFrameIndex);
  fIBLDescriptors[aUpdateInFlightFrameIndex].Update(true);
 end;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationDDGIRSMSplatComputePass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
const TotalProbes=TpvScene3DRendererInstance.CountGlobalIlluminationDDGICascades*TpvScene3DRendererInstance.GlobalIlluminationDDGIProbesPerCascade;
var PushConstants:TPushConstants;
    DescriptorSets:array[0..1] of TVkDescriptorSet;
    Quaternion:TpvQuaternion;
    RotationMatrix:TpvMatrix3x3;
    BufferMemoryBarriers:array[0..1] of TVkBufferMemoryBarrier;
    FinalMemoryBarrier:TVkMemoryBarrier;
    ParticleEmitterAddress,ParticleNodeAddress:TVkDeviceAddress;
    ParticleCount:TpvUInt32;
begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 // A new pseudo-random rotation per frame so the spherical-Fibonacci ray set covers the whole sphere over several frames.
 // Deterministic from the frame index so the producer and the probe-update passes agree on it (both reconstruct directions).
 Quaternion:=TpvQuaternion.CreateFromAngleAxis((aFrameIndex*2.39996323)+0.5,TpvVector3.InlineableCreate(0.5774,0.5774,0.5774).Normalize);
 RotationMatrix:=TpvMatrix3x3.CreateFromQuaternion(Quaternion);

 PushConstants.RandomRotation0:=TpvVector4.InlineableCreate(RotationMatrix.RawComponents[0,0],RotationMatrix.RawComponents[0,1],RotationMatrix.RawComponents[0,2],0.0);
 PushConstants.RandomRotation1:=TpvVector4.InlineableCreate(RotationMatrix.RawComponents[1,0],RotationMatrix.RawComponents[1,1],RotationMatrix.RawComponents[1,2],0.0);
 PushConstants.RandomRotation2:=TpvVector4.InlineableCreate(RotationMatrix.RawComponents[2,0],RotationMatrix.RawComponents[2,1],RotationMatrix.RawComponents[2,2],0.0);

 PushConstants.Params.x:=TpvUInt32(aFrameIndex);
 PushConstants.Params.y:=TpvScene3DRendererInstance.CountGlobalIlluminationDDGICascades;
 PushConstants.Params.z:=TpvScene3DRendererInstance.GlobalIlluminationDDGIProbesPerCascade;
 PushConstants.Params.w:=TpvScene3DRendererInstance.GlobalIlluminationDDGIRaysPerProbe;

 // Multi-bounce feedback strength + relocation-offset gate: 0 / first-frame on this slot's first frame (the previous probe
 // field is uninitialized garbage and the relocation offset is not written yet), else full. Shared with the probe-update pass.
 if fInstance.GlobalIlluminationDDGIFirstFrames[aInFlightFrameIndex] then begin
  PushConstants.Blend:=TpvVector4.InlineableCreate(0.97,0.0,1.0,0.0);
 end else begin
  PushConstants.Blend:=TpvVector4.InlineableCreate(0.97,1.0,0.0,0.0);
 end;

 // Particle LBVH (software-traced, no hardware RT): alive count + emitter/node buffer addresses, zero when inactive. The splat
 // injects particles through the same shared gi_ddgi_particle_inject.glsl as the trace producer.
 ParticleEmitterAddress:=0;
 ParticleNodeAddress:=0;
 ParticleCount:=0;
 if assigned(fInstance.ParticleBVH) and fInstance.ParticleBVH.Active and assigned(fInstance.ParticleBVH.NodeBuffers[aInFlightFrameIndex]) and assigned(fInstance.ParticleBVH.EmitterBuffers[aInFlightFrameIndex]) then begin
  ParticleEmitterAddress:=fInstance.ParticleBVH.EmitterBuffers[aInFlightFrameIndex].DeviceAddress;
  ParticleNodeAddress:=fInstance.ParticleBVH.NodeBuffers[aInFlightFrameIndex].DeviceAddress;
  ParticleCount:=Min(TpvSizeInt(fInstance.Scene3D.CountInFlightFrameParticleVertices[aInFlightFrameIndex] div 3),TpvSizeInt(TpvScene3D.MaxParticles));
 end;

 PushConstants.EmissiveGIParticleCount:=TpvVector4.InlineableCreate(fInstance.Renderer.GlobalIlluminationEmissiveScale,
                                                       fInstance.Renderer.GlobalIlluminationEmissiveMaximum,
                                                       ParticleCount,
                                                       0.0);

 PushConstants.ParticleBVH.x:=TpvUInt32(ParticleEmitterAddress and TpvUInt64($ffffffff));
 PushConstants.ParticleBVH.y:=TpvUInt32(ParticleEmitterAddress shr 32);
 PushConstants.ParticleBVH.z:=TpvUInt32(ParticleNodeAddress and TpvUInt64($ffffffff));
 PushConstants.ParticleBVH.w:=TpvUInt32(ParticleNodeAddress shr 32);

 // Make the host/transfer writes visible to the compute shader: the ddgiData buffer's per-frame cascade globals (SSBO read)
 // and the shared RSM matrices UBO (uniform read).
 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,VK_QUEUE_FAMILY_IGNORED,
                                                        fInstance.GlobalIlluminationDDGIMasterBuffers[aInFlightFrameIndex].Handle,0,VK_WHOLE_SIZE);
 BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_UNIFORM_READ_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,VK_QUEUE_FAMILY_IGNORED,
                                                        fInstance.GlobalIlluminationRadianceHintsRSMUniformBuffers[aInFlightFrameIndex].Handle,0,VK_WHOLE_SIZE);
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,0,nil,2,@BufferMemoryBarriers[0],0,nil);

 DescriptorSets[0]:=fRSMDescriptorSets[aInFlightFrameIndex].Handle;
 DescriptorSets[1]:=fDDGIDescriptorSets[aInFlightFrameIndex].Handle;
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,fPipelineLayout.Handle,0,2,@DescriptorSets[0],0,nil);
 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants),@PushConstants);

 // Splat: one thread per (ray, probe). local_size_x = 32, same dispatch as the trace producer.
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);
 aCommandBuffer.CmdDispatch((TpvScene3DRendererInstance.GlobalIlluminationDDGIRaysPerProbe+31) shr 5,TotalProbes,1);

 // Publish the ray-data writes to the probe-update passes (they read the ray-data buffer). The frame graph orders the passes;
 // this memory barrier makes the writes visible.
 FinalMemoryBarrier:=TVkMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                             TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT));
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,1,@FinalMemoryBarrier,0,nil,0,nil);

end;

end.
