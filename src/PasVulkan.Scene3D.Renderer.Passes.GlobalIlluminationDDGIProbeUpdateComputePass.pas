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
unit PasVulkan.Scene3D.Renderer.Passes.GlobalIlluminationDDGIProbeUpdateComputePass;
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

type { TpvScene3DRendererPassesGlobalIlluminationDDGIProbeUpdateComputePass }
     // Technique-agnostic DDGI probe BLEND/update CORE (RTXGI's ProbeBlendingCS analog): reads the ray-data image (produced
     // by the swappable trace pass) and integrates it temporally into the irradiance (L1/L2 SH or octahedral atlas) and
     // visibility (octahedral mean/mean^2 distance) probe data, then fills the octahedral guard bands. It depends only on
     // the ray-data + probe images, never on how the rays were traced. Three compute pipelines sharing one pipeline layout.
     TpvScene3DRendererPassesGlobalIlluminationDDGIProbeUpdateComputePass=class(TpvFrameGraph.TComputePass)
      public
       type TPushConstants=record
             RandomRotation0:TpvVector4; // mat3 column 0 — must match the trace's rotation (reconstructs the ray directions)
             RandomRotation1:TpvVector4;
             RandomRotation2:TpvVector4;
             Params:TpvUInt32Vector4;    // x = frameIndex, y = countCascades, z = probesPerCascade, w = raysPerProbe
             Blend:TpvVector4;           // x = hysteresis, z = firstFrame (1 = ignore the uninitialized previous probe data); y/w unused here
            end;
            PPushConstants=^TPushConstants;
      private
       fInstance:TpvScene3DRendererInstance;
       fComputeShaderModuleIrradianceUpdate:TpvVulkanShaderModule;
       fComputeShaderModuleVisibilityUpdate:TpvVulkanShaderModule;
       fComputeShaderModuleBorderUpdate:TpvVulkanShaderModule;
       fComputeShaderModuleRelocation:TpvVulkanShaderModule;       // relocation only (GlobalIlluminationDDGIProbeRelocation)
       fComputeShaderModuleClassification:TpvVulkanShaderModule;   // relocation only
       fVulkanPipelineShaderStageComputeIrradianceUpdate:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageComputeVisibilityUpdate:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageComputeBorderUpdate:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageComputeRelocation:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageComputeClassification:TpvVulkanPipelineShaderStage;
       fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
       fWarmupFrameCounts:array[0..MaxInFlightFrames-1] of TpvInt32; // per in-flight slot: frames since (re)init, for the convergence warmup hysteresis ramp
       fPipelineLayout:TpvVulkanPipelineLayout;
       fPipelineIrradianceUpdate:TpvVulkanComputePipeline;
       fPipelineVisibilityUpdate:TpvVulkanComputePipeline;
       fPipelineBorderUpdate:TpvVulkanComputePipeline;
       fPipelineRelocation:TpvVulkanComputePipeline;       // relocation only
       fPipelineClassification:TpvVulkanComputePipeline;   // relocation only
      public
       constructor Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance); reintroduce;
       destructor Destroy; override;
       procedure AcquirePersistentResources; override;
       procedure ReleasePersistentResources; override;
       procedure AcquireVolatileResources; override;
       procedure ReleaseVolatileResources; override;
       procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
      published
     end;

implementation

{ TpvScene3DRendererPassesGlobalIlluminationDDGIProbeUpdateComputePass }

constructor TpvScene3DRendererPassesGlobalIlluminationDDGIProbeUpdateComputePass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin
 inherited Create(aFrameGraph);
 fInstance:=aInstance;
 Name:='GlobalIlluminationDDGIProbeUpdateComputePass';
end;

destructor TpvScene3DRendererPassesGlobalIlluminationDDGIProbeUpdateComputePass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationDDGIProbeUpdateComputePass.AcquirePersistentResources;
 function Load(const aName:string):TpvVulkanShaderModule;
 var Stream:TStream;
 begin
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile(aName);
  try
   result:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
  finally
   Stream.Free;
  end;
 end;
begin
 inherited AcquirePersistentResources;
 fComputeShaderModuleIrradianceUpdate:=Load('gi_ddgi_irradiance_update_comp.spv');
 fComputeShaderModuleVisibilityUpdate:=Load('gi_ddgi_visibility_update_comp.spv');
 fComputeShaderModuleBorderUpdate:=Load('gi_ddgi_border_update_comp.spv');
 fVulkanPipelineShaderStageComputeIrradianceUpdate:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModuleIrradianceUpdate,'main');
 fVulkanPipelineShaderStageComputeVisibilityUpdate:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModuleVisibilityUpdate,'main');
 fVulkanPipelineShaderStageComputeBorderUpdate:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModuleBorderUpdate,'main');
 if TpvScene3DRendererInstance.GlobalIlluminationDDGIProbeRelocation then begin
  // RTXGI-style relocation + classification: read-only consumers of the trace's fixed rays, run after the blend/border.
  fComputeShaderModuleRelocation:=Load('gi_ddgi_relocation_comp.spv');
  fComputeShaderModuleClassification:=Load('gi_ddgi_classification_comp.spv');
  fVulkanPipelineShaderStageComputeRelocation:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModuleRelocation,'main');
  fVulkanPipelineShaderStageComputeClassification:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModuleClassification,'main');
 end;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationDDGIProbeUpdateComputePass.ReleasePersistentResources;
begin
 FreeAndNil(fVulkanPipelineShaderStageComputeIrradianceUpdate);
 FreeAndNil(fVulkanPipelineShaderStageComputeVisibilityUpdate);
 FreeAndNil(fVulkanPipelineShaderStageComputeBorderUpdate);
 FreeAndNil(fVulkanPipelineShaderStageComputeRelocation);
 FreeAndNil(fVulkanPipelineShaderStageComputeClassification);
 FreeAndNil(fComputeShaderModuleIrradianceUpdate);
 FreeAndNil(fComputeShaderModuleVisibilityUpdate);
 FreeAndNil(fComputeShaderModuleBorderUpdate);
 FreeAndNil(fComputeShaderModuleRelocation);
 FreeAndNil(fComputeShaderModuleClassification);
 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationDDGIProbeUpdateComputePass.AcquireVolatileResources;
var InFlightFrameIndex,SHImageIndex:TpvInt32;
    IrradianceImageInfos:TVkDescriptorImageInfoArray;
begin

 inherited AcquireVolatileResources;

 FillChar(fWarmupFrameCounts,SizeOf(fWarmupFrameCounts),#0); // every slot restarts the convergence warmup on (re)acquire

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(fInstance.Renderer.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fInstance.Renderer.CountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,fInstance.Renderer.CountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,fInstance.Renderer.CountInFlightFrames*(1+TpvScene3DRendererInstance.GlobalIlluminationDDGIIrradianceImageCount+1));
 if TpvScene3DRendererInstance.GlobalIlluminationDDGIProbeRelocation then begin
  fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,fInstance.Renderer.CountInFlightFrames); // binding 5 = probe-data (relocation/classification RMW)
 end;
 fVulkanDescriptorPool.Initialize;

 // Set 1 = DDGI resources used by the blend: UBO, ray-data (read), irradiance (write), visibility (write). Same bindings
 // the gi_ddgi_*_update.comp shaders declare (set 1).
 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fInstance.Renderer.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);
 fVulkanDescriptorSetLayout.AddBinding(1,VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);
 fVulkanDescriptorSetLayout.AddBinding(2,VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,TpvScene3DRendererInstance.GlobalIlluminationDDGIIrradianceImageCount,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);
 fVulkanDescriptorSetLayout.AddBinding(3,VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);
 if TpvScene3DRendererInstance.GlobalIlluminationDDGIProbeRelocation then begin
  // Binding 5 = probe-data (xyz = relocation offset, w = state), read-modify-written by the relocation + classification
  // pipelines. Matches gi_ddgi_relocation.comp / gi_ddgi_classification.comp (set 1 binding 5).
  fVulkanDescriptorSetLayout.AddBinding(5,VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);
 end;
 fVulkanDescriptorSetLayout.Initialize;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(fInstance.Renderer.VulkanDevice);
 fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
 // The update shaders address their resources at set 1 (shared layout with the trace shaders). Set 0 is unused here, so
 // the global scene set layout fills the slot and no descriptor set is bound there (the shaders never touch set 0).
 fPipelineLayout.AddDescriptorSetLayout(fInstance.Scene3D.GlobalVulkanDescriptorSetLayout); // set 0 = unused placeholder slot
 fPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);                        // set 1 = DDGI update resources
 fPipelineLayout.Initialize;

 fPipelineIrradianceUpdate:=TpvVulkanComputePipeline.Create(fInstance.Renderer.VulkanDevice,fInstance.Renderer.VulkanPipelineCache,0,fVulkanPipelineShaderStageComputeIrradianceUpdate,fPipelineLayout,nil,0);
 fPipelineVisibilityUpdate:=TpvVulkanComputePipeline.Create(fInstance.Renderer.VulkanDevice,fInstance.Renderer.VulkanPipelineCache,0,fVulkanPipelineShaderStageComputeVisibilityUpdate,fPipelineLayout,nil,0);
 fPipelineBorderUpdate:=TpvVulkanComputePipeline.Create(fInstance.Renderer.VulkanDevice,fInstance.Renderer.VulkanPipelineCache,0,fVulkanPipelineShaderStageComputeBorderUpdate,fPipelineLayout,nil,0);
 if TpvScene3DRendererInstance.GlobalIlluminationDDGIProbeRelocation then begin
  fPipelineRelocation:=TpvVulkanComputePipeline.Create(fInstance.Renderer.VulkanDevice,fInstance.Renderer.VulkanPipelineCache,0,fVulkanPipelineShaderStageComputeRelocation,fPipelineLayout,nil,0);
  fPipelineClassification:=TpvVulkanComputePipeline.Create(fInstance.Renderer.VulkanDevice,fInstance.Renderer.VulkanPipelineCache,0,fVulkanPipelineShaderStageComputeClassification,fPipelineLayout,nil,0);
 end;

 for InFlightFrameIndex:=0 to fInstance.Renderer.CountInFlightFrames-1 do begin

  IrradianceImageInfos:=nil;
  SetLength(IrradianceImageInfos,TpvScene3DRendererInstance.GlobalIlluminationDDGIIrradianceImageCount);
  if TpvScene3DRendererInstance.GlobalIlluminationDDGIStorageOctahedral then begin
   IrradianceImageInfos[0]:=TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,fInstance.GlobalIlluminationDDGIIrradianceOctImages[InFlightFrameIndex].VulkanImageView.Handle,VK_IMAGE_LAYOUT_GENERAL);
  end else begin
   for SHImageIndex:=0 to TpvScene3DRendererInstance.GlobalIlluminationDDGISHImageCount-1 do begin
    IrradianceImageInfos[SHImageIndex]:=TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,fInstance.GlobalIlluminationDDGIIrradianceImages[InFlightFrameIndex,SHImageIndex].VulkanImageView.Handle,VK_IMAGE_LAYOUT_GENERAL);
   end;
  end;

  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,0,1,TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),[],[fInstance.GlobalIlluminationDDGIUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],[],false);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,0,1,TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,fInstance.GlobalIlluminationDDGIRayDataImages[InFlightFrameIndex].VulkanImageView.Handle,VK_IMAGE_LAYOUT_GENERAL)],[],[],false);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,0,length(IrradianceImageInfos),TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),IrradianceImageInfos,[],[],false);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,0,1,TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,fInstance.GlobalIlluminationDDGIVisibilityImages[InFlightFrameIndex].VulkanImageView.Handle,VK_IMAGE_LAYOUT_GENERAL)],[],[],false);
  if TpvScene3DRendererInstance.GlobalIlluminationDDGIProbeRelocation then begin
   fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(5,0,1,TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                  [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,fInstance.GlobalIlluminationDDGIProbeDataImages[InFlightFrameIndex].VulkanImageView.Handle,VK_IMAGE_LAYOUT_GENERAL)],[],[],false);
  end;
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;

  IrradianceImageInfos:=nil;

 end;

end;

procedure TpvScene3DRendererPassesGlobalIlluminationDDGIProbeUpdateComputePass.ReleaseVolatileResources;
var InFlightFrameIndex:TpvInt32;
begin
 FreeAndNil(fPipelineIrradianceUpdate);
 FreeAndNil(fPipelineVisibilityUpdate);
 FreeAndNil(fPipelineBorderUpdate);
 FreeAndNil(fPipelineRelocation);
 FreeAndNil(fPipelineClassification);
 FreeAndNil(fPipelineLayout);
 for InFlightFrameIndex:=0 to fInstance.Renderer.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
 end;
 FreeAndNil(fVulkanDescriptorSetLayout);
 FreeAndNil(fVulkanDescriptorPool);
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationDDGIProbeUpdateComputePass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
const TotalProbes=TpvScene3DRendererInstance.CountGlobalIlluminationDDGICascades*TpvScene3DRendererInstance.GlobalIlluminationDDGIProbesPerCascade;
      // Convergence warmup: for a slot's first WarmupFrames updates, ramp the temporal hysteresis from WarmupStartHysteresis
      // up to SteadyHysteresis, so freshly (re)initialized probes settle in a few frames instead of ~100 (less startup flicker).
      WarmupFrames=16;
      WarmupStartHysteresis=0.7;
      SteadyHysteresis=0.97;
var PushConstants:TPushConstants;
    DescriptorSet:TVkDescriptorSet;
    Quaternion:TpvQuaternion;
    RotationMatrix:TpvMatrix3x3;
    FinalMemoryBarrier:TVkMemoryBarrier;
    WarmupT,Hysteresis:TpvFloat;
 procedure FullMemoryBarrier;
 var MemoryBarrier:TVkMemoryBarrier;
 begin
  MemoryBarrier:=TVkMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT));
  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,1,@MemoryBarrier,0,nil,0,nil);
 end;
begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 // Reconstruct the same per-frame rotation the trace used, so the directions the blend weights against match the traced
 // rays (deterministic from the frame index).
 Quaternion:=TpvQuaternion.CreateFromAngleAxis((aFrameIndex*2.39996323)+0.5,TpvVector3.InlineableCreate(0.5774,0.5774,0.5774).Normalize);
 RotationMatrix:=TpvMatrix3x3.CreateFromQuaternion(Quaternion);
 PushConstants.RandomRotation0:=TpvVector4.InlineableCreate(RotationMatrix.RawComponents[0,0],RotationMatrix.RawComponents[0,1],RotationMatrix.RawComponents[0,2],0.0);
 PushConstants.RandomRotation1:=TpvVector4.InlineableCreate(RotationMatrix.RawComponents[1,0],RotationMatrix.RawComponents[1,1],RotationMatrix.RawComponents[1,2],0.0);
 PushConstants.RandomRotation2:=TpvVector4.InlineableCreate(RotationMatrix.RawComponents[2,0],RotationMatrix.RawComponents[2,1],RotationMatrix.RawComponents[2,2],0.0);
 PushConstants.Params.x:=TpvUInt32(aFrameIndex);
 PushConstants.Params.y:=TpvScene3DRendererInstance.CountGlobalIlluminationDDGICascades;
 PushConstants.Params.z:=TpvScene3DRendererInstance.GlobalIlluminationDDGIProbesPerCascade;
 PushConstants.Params.w:=TpvScene3DRendererInstance.GlobalIlluminationDDGIRaysPerProbe;
 // x = temporal hysteresis; z = firstFrame flag (this slot's probe images are still uninitialized -> discard the previous
 // data in the temporal blend this frame). Shared first-frame state with the trace pass; flipped false after writing.
 if fInstance.GlobalIlluminationDDGIFirstFrames[aInFlightFrameIndex] then begin
  // First frame of this slot: take the raw value (z=1, hysteresis irrelevant) and (re)start the convergence warmup.
  fWarmupFrameCounts[aInFlightFrameIndex]:=0;
  PushConstants.Blend:=TpvVector4.InlineableCreate(SteadyHysteresis,0.0,1.0,0.0);
 end else begin
  // Warmup ramp: low hysteresis right after init (probes converge fast) easing up to the steady value over WarmupFrames.
  WarmupT:=Min(fWarmupFrameCounts[aInFlightFrameIndex]/WarmupFrames,1.0);
  Hysteresis:=(WarmupStartHysteresis*(1.0-WarmupT))+(SteadyHysteresis*WarmupT);
  PushConstants.Blend:=TpvVector4.InlineableCreate(Hysteresis,1.0,0.0,0.0);
 end;
 if fWarmupFrameCounts[aInFlightFrameIndex]<WarmupFrames then begin
  inc(fWarmupFrameCounts[aInFlightFrameIndex]);
 end;

 // The ray-data was published by the trace pass (its final barrier + the frame-graph ordering make the writes visible).
 DescriptorSet:=fVulkanDescriptorSets[aInFlightFrameIndex].Handle;
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,fPipelineLayout.Handle,1,1,@DescriptorSet,0,nil); // bind set 1 only (set 0 unused)
 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants),@PushConstants);

 // Irradiance integration: one thread per probe (SH storage). local_size_x = 64.
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipelineIrradianceUpdate.Handle);
 aCommandBuffer.CmdDispatch((TotalProbes+63) shr 6,1,1);

 // With the per-probe warmup the irradiance pass reads the visibility image's w (per-probe age); serialize so that read
 // completes before the visibility pass below overwrites it (WAR). Off by default -> no extra barrier for the planet game.
 if TpvScene3DRendererInstance.GlobalIlluminationDDGIProbeAgeWarmup then begin
  FullMemoryBarrier;
 end;

 // Visibility integration: one workgroup per probe (octahedral tile). local_size = 16x16.
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipelineVisibilityUpdate.Handle);
 aCommandBuffer.CmdDispatch(TotalProbes,1,1);
 FullMemoryBarrier;

 // Border / guard band copy for the octahedral atlas(es). One workgroup per probe.
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipelineBorderUpdate.Handle);
 aCommandBuffer.CmdDispatch(TotalProbes,1,1);

 // RTXGI-style probe relocation + classification (read-only consumers of the trace's fixed rays; no re-tracing). They run
 // after the blend/border and read-modify-write the probe-data image: relocation writes the offset (xyz), classification the
 // active state (w). They touch a different resource than the border (atlases), so no barrier is needed before relocation;
 // a barrier between the two serializes the probe-data RMW. The final publish barrier below also makes the probe-data
 // visible to the shading samplers (binding 3) and to next frame's trace (relocated ray origin).
 if TpvScene3DRendererInstance.GlobalIlluminationDDGIProbeRelocation then begin
  // Probe relocation: one thread per probe (local_size_x = 64). Pushes each probe out of any geometry it is embedded in.
  aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipelineRelocation.Handle);
  aCommandBuffer.CmdDispatch((TotalProbes+63) shr 6,1,1);
  FullMemoryBarrier; // classification reads the offset (xyz) the relocation pass just wrote and sets the state (w)
  // Probe classification: one thread per probe. Marks probes mostly seeing backfaces (inside geometry) as INACTIVE.
  aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipelineClassification.Handle);
  aCommandBuffer.CmdDispatch((TotalProbes+63) shr 6,1,1);
 end;

 // Publish the probe writes to every later shader stage that samples them (mesh/planet fragment shaders).
 FinalMemoryBarrier:=TVkMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                             TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT));
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   FrameGraph.VulkanDevice.PhysicalDevice.PipelineStageAllShaderBits,
                                   0,1,@FinalMemoryBarrier,0,nil,0,nil);

 // This slot's probe images have now been written once -> subsequent frames blend against them normally. Shared flag the
 // trace pass reads (it ran before this pass this frame, so it saw the pre-flip value).
 fInstance.GlobalIlluminationDDGIFirstFrames[aInFlightFrameIndex]:=false;

end;

end.
