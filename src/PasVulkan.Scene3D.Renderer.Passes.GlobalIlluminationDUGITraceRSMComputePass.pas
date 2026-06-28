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
 *     x86-64, ARM, ARM64, etc.).                                            *
 * 11. Make sure the code runs on all platforms with Vulkan support           *
 *                                                                            *
 ******************************************************************************)
unit PasVulkan.Scene3D.Renderer.Passes.GlobalIlluminationDUGITraceRSMComputePass;
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

type { TpvScene3DRendererPassesGlobalIlluminationDUGITraceRSMComputePass }
     // Non-raytraced DUGI ray-data PRODUCER (Reflective Shadow Map fallback). This is the SAME gi_dugi_trace shader built with
     // the RSM backend (GI_TRACE_BACKEND=2): no hardware ray query — it reads the sun's reflective shadow map (rendered by the
     // ReflectiveShadowMapRenderPass) and gathers the lit RSM texels along each probe ray instead of tracing the TLAS. Probe
     // iteration / relocation / multi-bounce / particle injection / ray-data encode are byte-for-byte identical to the ray-query
     // trace, so the whole probe BLEND/update core downstream is unchanged. Only used when raytracing is unavailable, where the
     // engine's global descriptor set is the non-RT variant (lights without the TLAS), which the RSM shader matches.
     // Descriptor sets: 0 = global scene (lights/materials/textures, no TLAS), 1 = DUGI resources (dugiData + previous-frame
     // irradiance/visibility reads for multi-bounce + the 6 env cubemaps for sky-on-miss), 2 = the RSM source textures + matrices.
     TpvScene3DRendererPassesGlobalIlluminationDUGITraceRSMComputePass=class(TpvFrameGraph.TComputePass)
      public
       type TPushConstants=record
             RandomRotation0:TpvVector4;         // mat3 column 0 in xyz
             RandomRotation1:TpvVector4;         // mat3 column 1 in xyz
             RandomRotation2:TpvVector4;         // mat3 column 2 in xyz
             Params:TpvUInt32Vector4;            // x = frameIndex, y = countCascades, z = probesPerCascade, w = raysPerProbe
             Blend:TpvVector4;                   // y = multi-bounce feedback strength (0 on a slot's first frame); z = first-frame flag (relocation offset gate)
             EmissiveGIParticleCount:TpvVector4; // x = global GI emissive scale, y = global GI emissive max, z = particle count — must match gi_dugi_pushconstants.glsl
             ParticleBVH:TpvUInt32Vector4;       // particle LBVH device addresses: xy = emitter buffer (uvec2), zw = node buffer (uvec2); 0 when inactive
             Flags:TpvUInt32;                    // GI_DUGI_FLAG_* bitmask (see gi_dugi_pushconstants.glsl); 0 here (RSM fallback: classification keeps all probes active)
            end;
            PPushConstants=^TPushConstants;
      private
       fInstance:TpvScene3DRendererInstance;
       fResourceRSMColor:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceRSMNormalUsed:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceRSMDepth:TpvFrameGraph.TPass.TUsedImageResource;
       fComputeShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStage:TpvVulkanPipelineShaderStage;
       fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;       // set 1 = DUGI resources
       fVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
       fRSMDescriptorSetLayout:TpvVulkanDescriptorSetLayout;          // set 2 = RSM source
       fRSMDescriptorPool:TpvVulkanDescriptorPool;
       fRSMDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
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

{ TpvScene3DRendererPassesGlobalIlluminationDUGITraceRSMComputePass }

constructor TpvScene3DRendererPassesGlobalIlluminationDUGITraceRSMComputePass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin

 inherited Create(aFrameGraph);

 fInstance:=aInstance;

 Name:='GlobalIlluminationDUGITraceRSMComputePass';

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

destructor TpvScene3DRendererPassesGlobalIlluminationDUGITraceRSMComputePass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationDUGITraceRSMComputePass.AcquirePersistentResources;
var Stream:TStream;
begin
 inherited AcquirePersistentResources;
 // The RSM-backend build of gi_dugi_trace (GI_TRACE_BACKEND=2): same producer, no ray query.
 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('gi_dugi_trace_rsm_comp.spv');
 try
  fComputeShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;
 fVulkanPipelineShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');
end;

procedure TpvScene3DRendererPassesGlobalIlluminationDUGITraceRSMComputePass.ReleasePersistentResources;
begin
 FreeAndNil(fVulkanPipelineShaderStage);
 FreeAndNil(fComputeShaderModule);
 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationDUGITraceRSMComputePass.AcquireVolatileResources;
var InFlightFrameIndex:TpvInt32;
begin

 inherited AcquireVolatileResources;

 // Set 1 = DUGI resources (mirrors the trace pass): dugiData SSBO, previous-frame irradiance (octahedral storage only) +
 // visibility reads for multi-bounce, and the 6 environment cubemaps (sky-on-miss).
 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(fInstance.Renderer.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fInstance.Renderer.CountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,fInstance.Renderer.CountInFlightFrames); // binding 0 = dugiData SSBO
 if TpvScene3DRendererInstance.GlobalIlluminationDUGIStorageOctahedral then begin
  fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,fInstance.Renderer.CountInFlightFrames*2); // binding 2 = oct irradiance read + binding 3 = visibility read
 end else begin
  fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,fInstance.Renderer.CountInFlightFrames); // binding 3 = visibility read only (SH irradiance is a BDA buffer via the master)
 end;
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fInstance.Renderer.CountInFlightFrames*6);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fInstance.Renderer.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]); // binding 0 = dugiData SSBO
 if TpvScene3DRendererInstance.GlobalIlluminationDUGIStorageOctahedral then begin
  fVulkanDescriptorSetLayout.AddBinding(2,VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]); // oct irradiance read (multi-bounce)
 end;
 fVulkanDescriptorSetLayout.AddBinding(3,VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);
 fVulkanDescriptorSetLayout.AddBinding(4,VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,6,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);
 fVulkanDescriptorSetLayout.Initialize;

 // Set 2 = RSM source: 3 combined image samplers (color/normal/depth) + the radiance-hints RSM matrices UBO (shared).
 fRSMDescriptorPool:=TpvVulkanDescriptorPool.Create(fInstance.Renderer.VulkanDevice,
                                                    TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                    fInstance.Renderer.CountInFlightFrames);
 fRSMDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fInstance.Renderer.CountInFlightFrames*3);
 fRSMDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,fInstance.Renderer.CountInFlightFrames);
 fRSMDescriptorPool.Initialize;

 fRSMDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fInstance.Renderer.VulkanDevice);
 fRSMDescriptorSetLayout.AddBinding(0,VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]); // RSM color (flux/albedo)
 fRSMDescriptorSetLayout.AddBinding(1,VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]); // RSM normal + used flag
 fRSMDescriptorSetLayout.AddBinding(2,VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]); // RSM depth
 fRSMDescriptorSetLayout.AddBinding(3,VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);         // RSM matrices UBO (shared radiance-hints buffer)
 fRSMDescriptorSetLayout.Initialize;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(fInstance.Renderer.VulkanDevice);
 fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
 fPipelineLayout.AddDescriptorSetLayout(fInstance.Scene3D.GlobalVulkanDescriptorSetLayout); // set 0 = global scene (lights/materials/textures; no TLAS in the non-RT layout)
 fPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);                        // set 1 = DUGI resources
 fPipelineLayout.AddDescriptorSetLayout(fRSMDescriptorSetLayout);                           // set 2 = RSM source
 fPipelineLayout.Initialize;

 fPipeline:=TpvVulkanComputePipeline.Create(fInstance.Renderer.VulkanDevice,fInstance.Renderer.VulkanPipelineCache,0,fVulkanPipelineShaderStage,fPipelineLayout,nil,0);

 for InFlightFrameIndex:=0 to fInstance.Renderer.CountInFlightFrames-1 do begin

  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,0,1,TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),[],[fInstance.GlobalIlluminationDUGIMasterBuffers[InFlightFrameIndex].DescriptorBufferInfo],[],false); // binding 0 = dugiData SSBO
  if TpvScene3DRendererInstance.GlobalIlluminationDUGIStorageOctahedral then begin
   fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,0,1,TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                  [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,fInstance.GlobalIlluminationDUGIIrradianceOctImage.VulkanImageView.Handle,VK_IMAGE_LAYOUT_GENERAL)],[],[],false);
  end;
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,0,1,TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,fInstance.GlobalIlluminationDUGIVisibilityMomentsImage.VulkanImageView.Handle,VK_IMAGE_LAYOUT_GENERAL)],[],[],false);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,0,6,TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [fInstance.Renderer.ImageBasedLightingEnvMapCubeMaps.GGXDescriptorImageInfo,
                                                                  fInstance.Renderer.ImageBasedLightingEnvMapCubeMaps.CharlieDescriptorImageInfo,
                                                                  fInstance.Renderer.ImageBasedLightingEnvMapCubeMaps.LambertianDescriptorImageInfo,
                                                                  fInstance.Renderer.ImageBasedLightingEnvMapCubeMaps.GGXDescriptorImageInfo,
                                                                  fInstance.Renderer.ImageBasedLightingEnvMapCubeMaps.CharlieDescriptorImageInfo,
                                                                  fInstance.Renderer.ImageBasedLightingEnvMapCubeMaps.LambertianDescriptorImageInfo],[],[],false);
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;

  fIBLDescriptors[InFlightFrameIndex]:=TpvScene3DRendererIBLDescriptor.Create(fInstance.Renderer.VulkanDevice,fVulkanDescriptorSets[InFlightFrameIndex],4,fInstance.Renderer.ClampedSampler.Handle);
  fIBLDescriptors[InFlightFrameIndex].SetFrom(fInstance.Scene3D,fInstance,InFlightFrameIndex);
  fIBLDescriptors[InFlightFrameIndex].Update(true);

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

 end;

end;

procedure TpvScene3DRendererPassesGlobalIlluminationDUGITraceRSMComputePass.ReleaseVolatileResources;
var InFlightFrameIndex:TpvInt32;
begin
 FreeAndNil(fPipeline);
 FreeAndNil(fPipelineLayout);
 for InFlightFrameIndex:=0 to fInstance.Renderer.CountInFlightFrames-1 do begin
  FreeAndNil(fIBLDescriptors[InFlightFrameIndex]);
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fRSMDescriptorSets[InFlightFrameIndex]);
 end;
 FreeAndNil(fVulkanDescriptorSetLayout);
 FreeAndNil(fVulkanDescriptorPool);
 FreeAndNil(fRSMDescriptorSetLayout);
 FreeAndNil(fRSMDescriptorPool);
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationDUGITraceRSMComputePass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
 if assigned(fIBLDescriptors[aUpdateInFlightFrameIndex]) then begin
  fIBLDescriptors[aUpdateInFlightFrameIndex].SetFrom(fInstance.Scene3D,fInstance,aUpdateInFlightFrameIndex);
  fIBLDescriptors[aUpdateInFlightFrameIndex].Update(true);
 end;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationDUGITraceRSMComputePass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
const TotalProbes=TpvScene3DRendererInstance.CountGlobalIlluminationDUGICascades*TpvScene3DRendererInstance.GlobalIlluminationDUGIProbesPerCascade;
var PushConstants:TPushConstants;
    DescriptorSets:array[0..2] of TVkDescriptorSet;
    Quaternion:TpvQuaternion;
    RotationMatrix:TpvMatrix3x3;
    BufferMemoryBarriers:array[0..1] of TVkBufferMemoryBarrier;
    FinalMemoryBarrier:TVkMemoryBarrier;
    FieldMemoryBarrier:TVkMemoryBarrier;
    ParticleEmitterAddress,ParticleNodeAddress:TVkDeviceAddress;
    ParticleCount:TpvUInt32;
begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 // Deterministic per-frame rotation (same as the trace pass) so the producer and the probe-update passes agree on the
 // spherical-Fibonacci ray directions.
 Quaternion:=TpvQuaternion.CreateFromAngleAxis((aFrameIndex*2.39996323)+0.5,TpvVector3.InlineableCreate(0.5774,0.5774,0.5774).Normalize);
 RotationMatrix:=TpvMatrix3x3.CreateFromQuaternion(Quaternion);

 PushConstants.RandomRotation0:=TpvVector4.InlineableCreate(RotationMatrix.RawComponents[0,0],RotationMatrix.RawComponents[0,1],RotationMatrix.RawComponents[0,2],0.0);
 PushConstants.RandomRotation1:=TpvVector4.InlineableCreate(RotationMatrix.RawComponents[1,0],RotationMatrix.RawComponents[1,1],RotationMatrix.RawComponents[1,2],0.0);
 PushConstants.RandomRotation2:=TpvVector4.InlineableCreate(RotationMatrix.RawComponents[2,0],RotationMatrix.RawComponents[2,1],RotationMatrix.RawComponents[2,2],0.0);

 PushConstants.Params.x:=TpvUInt32(aFrameIndex);
 PushConstants.Params.y:=TpvScene3DRendererInstance.CountGlobalIlluminationDUGICascades;
 PushConstants.Params.z:=TpvScene3DRendererInstance.GlobalIlluminationDUGIProbesPerCascade;
 PushConstants.Params.w:=TpvScene3DRendererInstance.GlobalIlluminationDUGIRaysPerProbe;

 // Multi-bounce feedback strength + relocation-offset gate: 0 / first-frame on the first frame (the previous probe field is
 // uninitialized garbage and the relocation offset is not written yet), else full. Shared with the probe-update pass (single).
 if fInstance.GlobalIlluminationDUGIFirstFrame then begin
  PushConstants.Blend:=TpvVector4.InlineableCreate(0.97,0.0,1.0,0.0);
 end else begin
  PushConstants.Blend:=TpvVector4.InlineableCreate(0.97,1.0,0.0,0.0);
 end;

 // Particle LBVH (software-traced, no hardware RT): alive count + emitter/node buffer addresses, zero when inactive.
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

 PushConstants.Flags:=0; // RSM fallback: classification keeps all probes active (no fixed-ray geometry), so the early-out never applies

 // Cross-frame WAR: the probe field is now a single shared resource (one history). Before this frame's DUGI compute touches it,
 // the previous frame's reads (the fragment shaders that sampled the field + last frame's compute) must complete. Global memory
 // barrier; the field images stay in VK_IMAGE_LAYOUT_GENERAL, so no layout transition is needed.
 FieldMemoryBarrier:=TVkMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                             TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT));
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,1,@FieldMemoryBarrier,0,nil,0,nil);

 // Make the host/transfer writes visible to the compute shader: the dugiData cascade globals (SSBO read) and the shared RSM
 // matrices UBO (uniform read).
 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,VK_QUEUE_FAMILY_IGNORED,
                                                        fInstance.GlobalIlluminationDUGIMasterBuffers[aInFlightFrameIndex].Handle,0,VK_WHOLE_SIZE);
 BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_UNIFORM_READ_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,VK_QUEUE_FAMILY_IGNORED,
                                                        fInstance.GlobalIlluminationRadianceHintsRSMUniformBuffers[aInFlightFrameIndex].Handle,0,VK_WHOLE_SIZE);
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,0,nil,2,@BufferMemoryBarriers[0],0,nil);

 DescriptorSets[0]:=fInstance.Scene3D.GlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle;
 DescriptorSets[1]:=fVulkanDescriptorSets[aInFlightFrameIndex].Handle;
 DescriptorSets[2]:=fRSMDescriptorSets[aInFlightFrameIndex].Handle;
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,fPipelineLayout.Handle,0,3,@DescriptorSets[0],0,nil);
 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants),@PushConstants);

 // One thread per (ray, probe). local_size_x = 32, same dispatch as the trace producer.
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);
 aCommandBuffer.CmdDispatch((TpvScene3DRendererInstance.GlobalIlluminationDUGIRaysPerProbe+31) shr 5,TotalProbes,1);

 // Publish the ray-data writes to the probe-update passes (they read the ray-data buffer).
 FinalMemoryBarrier:=TVkMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                             TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT));
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,1,@FinalMemoryBarrier,0,nil,0,nil);

end;

end.
