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
unit PasVulkan.Scene3D.Renderer.Passes.GlobalIlluminationDDGIComputePass;
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

type { TpvScene3DRendererPassesGlobalIlluminationDDGIComputePass }
     // Single combined compute pass that runs the whole per-frame DDGI probe update: trace rays against the scene TLAS,
     // integrate irradiance (L1 SH) and visibility (octahedral mean/mean^2 distance), and fill the octahedral guard
     // bands. The four stages are separate compute pipelines sharing one pipeline layout, separated by memory barriers.
     TpvScene3DRendererPassesGlobalIlluminationDDGIComputePass=class(TpvFrameGraph.TComputePass)
      public
       type TPushConstants=record
             RandomRotation0:TpvVector4; // mat3 column 0 in xyz
             RandomRotation1:TpvVector4; // mat3 column 1 in xyz
             RandomRotation2:TpvVector4; // mat3 column 2 in xyz
             Params:TpvUInt32Vector4;    // x = frameIndex, y = countCascades, z = probesPerCascade, w = raysPerProbe
             Blend:TpvVector4;           // x = hysteresis
            end;
            PPushConstants=^TPushConstants;
      private
       fInstance:TpvScene3DRendererInstance;
       fComputeShaderModuleTrace:TpvVulkanShaderModule;
       fComputeShaderModuleIrradianceUpdate:TpvVulkanShaderModule;
       fComputeShaderModuleVisibilityUpdate:TpvVulkanShaderModule;
       fComputeShaderModuleBorderUpdate:TpvVulkanShaderModule;
       fVulkanPipelineShaderStageComputeTrace:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageComputeIrradianceUpdate:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageComputeVisibilityUpdate:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageComputeBorderUpdate:TpvVulkanPipelineShaderStage;
       fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
       fPipelineLayout:TpvVulkanPipelineLayout;
       fPipelineTrace:TpvVulkanComputePipeline;
       fPipelineIrradianceUpdate:TpvVulkanComputePipeline;
       fPipelineVisibilityUpdate:TpvVulkanComputePipeline;
       fPipelineBorderUpdate:TpvVulkanComputePipeline;
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

{ TpvScene3DRendererPassesGlobalIlluminationDDGIComputePass }

constructor TpvScene3DRendererPassesGlobalIlluminationDDGIComputePass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin
 inherited Create(aFrameGraph);
 fInstance:=aInstance;
 Name:='GlobalIlluminationDDGIComputePass';
end;

destructor TpvScene3DRendererPassesGlobalIlluminationDDGIComputePass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationDDGIComputePass.AcquirePersistentResources;
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
 fComputeShaderModuleTrace:=Load('gi_ddgi_trace_comp.spv');
 fComputeShaderModuleIrradianceUpdate:=Load('gi_ddgi_irradiance_update_comp.spv');
 fComputeShaderModuleVisibilityUpdate:=Load('gi_ddgi_visibility_update_comp.spv');
 fComputeShaderModuleBorderUpdate:=Load('gi_ddgi_border_update_comp.spv');
 fVulkanPipelineShaderStageComputeTrace:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModuleTrace,'main');
 fVulkanPipelineShaderStageComputeIrradianceUpdate:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModuleIrradianceUpdate,'main');
 fVulkanPipelineShaderStageComputeVisibilityUpdate:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModuleVisibilityUpdate,'main');
 fVulkanPipelineShaderStageComputeBorderUpdate:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModuleBorderUpdate,'main');
end;

procedure TpvScene3DRendererPassesGlobalIlluminationDDGIComputePass.ReleasePersistentResources;
begin
 FreeAndNil(fVulkanPipelineShaderStageComputeTrace);
 FreeAndNil(fVulkanPipelineShaderStageComputeIrradianceUpdate);
 FreeAndNil(fVulkanPipelineShaderStageComputeVisibilityUpdate);
 FreeAndNil(fVulkanPipelineShaderStageComputeBorderUpdate);
 FreeAndNil(fComputeShaderModuleTrace);
 FreeAndNil(fComputeShaderModuleIrradianceUpdate);
 FreeAndNil(fComputeShaderModuleVisibilityUpdate);
 FreeAndNil(fComputeShaderModuleBorderUpdate);
 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationDDGIComputePass.AcquireVolatileResources;
var InFlightFrameIndex,SHImageIndex:TpvInt32;
    IrradianceImageInfos:TVkDescriptorImageInfoArray;
begin

 inherited AcquireVolatileResources;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(fInstance.Renderer.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fInstance.Renderer.CountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,fInstance.Renderer.CountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,fInstance.Renderer.CountInFlightFrames*(1+TpvScene3DRendererInstance.GlobalIlluminationDDGISHImageCount+1));
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fInstance.Renderer.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);
 fVulkanDescriptorSetLayout.AddBinding(1,VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);
 fVulkanDescriptorSetLayout.AddBinding(2,VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,TpvScene3DRendererInstance.GlobalIlluminationDDGISHImageCount,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);
 fVulkanDescriptorSetLayout.AddBinding(3,VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);
 fVulkanDescriptorSetLayout.Initialize;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(fInstance.Renderer.VulkanDevice);
 fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
 // Set 0 = the scene's global descriptor set (TLAS, lights, materials, textures) for the ray query in the trace stage.
 fPipelineLayout.AddDescriptorSetLayout(fInstance.Scene3D.GlobalVulkanDescriptorSetLayout);
 // Set 1 = the DDGI probe resources.
 fPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fPipelineLayout.Initialize;

 fPipelineTrace:=TpvVulkanComputePipeline.Create(fInstance.Renderer.VulkanDevice,fInstance.Renderer.VulkanPipelineCache,0,fVulkanPipelineShaderStageComputeTrace,fPipelineLayout,nil,0);
 fPipelineIrradianceUpdate:=TpvVulkanComputePipeline.Create(fInstance.Renderer.VulkanDevice,fInstance.Renderer.VulkanPipelineCache,0,fVulkanPipelineShaderStageComputeIrradianceUpdate,fPipelineLayout,nil,0);
 fPipelineVisibilityUpdate:=TpvVulkanComputePipeline.Create(fInstance.Renderer.VulkanDevice,fInstance.Renderer.VulkanPipelineCache,0,fVulkanPipelineShaderStageComputeVisibilityUpdate,fPipelineLayout,nil,0);
 fPipelineBorderUpdate:=TpvVulkanComputePipeline.Create(fInstance.Renderer.VulkanDevice,fInstance.Renderer.VulkanPipelineCache,0,fVulkanPipelineShaderStageComputeBorderUpdate,fPipelineLayout,nil,0);

 for InFlightFrameIndex:=0 to fInstance.Renderer.CountInFlightFrames-1 do begin

  IrradianceImageInfos:=nil;
  SetLength(IrradianceImageInfos,TpvScene3DRendererInstance.GlobalIlluminationDDGISHImageCount);
  for SHImageIndex:=0 to TpvScene3DRendererInstance.GlobalIlluminationDDGISHImageCount-1 do begin
   IrradianceImageInfos[SHImageIndex]:=TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                     fInstance.GlobalIlluminationDDGIIrradianceImages[InFlightFrameIndex,SHImageIndex].VulkanImageView.Handle,
                                                                     VK_IMAGE_LAYOUT_GENERAL);
  end;

  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,0,1,TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),[],[fInstance.GlobalIlluminationDDGIUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],[],false);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,0,1,TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,fInstance.GlobalIlluminationDDGIRayDataImages[InFlightFrameIndex].VulkanImageView.Handle,VK_IMAGE_LAYOUT_GENERAL)],[],[],false);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,0,length(IrradianceImageInfos),TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),IrradianceImageInfos,[],[],false);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,0,1,TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,fInstance.GlobalIlluminationDDGIVisibilityImages[InFlightFrameIndex].VulkanImageView.Handle,VK_IMAGE_LAYOUT_GENERAL)],[],[],false);
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;

  IrradianceImageInfos:=nil;

 end;

end;

procedure TpvScene3DRendererPassesGlobalIlluminationDDGIComputePass.ReleaseVolatileResources;
var InFlightFrameIndex:TpvInt32;
begin
 FreeAndNil(fPipelineTrace);
 FreeAndNil(fPipelineIrradianceUpdate);
 FreeAndNil(fPipelineVisibilityUpdate);
 FreeAndNil(fPipelineBorderUpdate);
 FreeAndNil(fPipelineLayout);
 for InFlightFrameIndex:=0 to fInstance.Renderer.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
 end;
 FreeAndNil(fVulkanDescriptorSetLayout);
 FreeAndNil(fVulkanDescriptorPool);
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationDDGIComputePass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvScene3DRendererPassesGlobalIlluminationDDGIComputePass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
const TotalProbes=TpvScene3DRendererInstance.CountGlobalIlluminationDDGICascades*TpvScene3DRendererInstance.GlobalIlluminationDDGIProbesPerCascade;
var PushConstants:TPushConstants;
    DescriptorSets:array[0..1] of TVkDescriptorSet;
    Quaternion:TpvQuaternion;
    RotationMatrix:TpvMatrix3x3;
    BufferMemoryBarrier:TVkBufferMemoryBarrier;
    FinalMemoryBarrier:TVkMemoryBarrier;
 procedure FullMemoryBarrier;
 var MemoryBarrier:TVkMemoryBarrier;
 begin
  // Conservative all-shader-write -> all-shader-read/write memory barrier between the compute stages (all DDGI images
  // stay in VK_IMAGE_LAYOUT_GENERAL, so no image layout transitions are needed, only memory/execution dependencies).
  MemoryBarrier:=TVkMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT));
  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,1,@MemoryBarrier,0,nil,0,nil);
 end;
begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 // A new pseudo-random rotation per frame so that, over several frames, the spherical-Fibonacci ray set covers the
 // whole sphere. Deterministic from the frame index so the trace and update stages agree on it.
 Quaternion:=TpvQuaternion.CreateFromAngleAxis((aFrameIndex*2.39996323)+0.5,
                                               TpvVector3.InlineableCreate(0.5774,0.5774,0.5774).Normalize);
 RotationMatrix:=TpvMatrix3x3.CreateFromQuaternion(Quaternion);
 PushConstants.RandomRotation0:=TpvVector4.InlineableCreate(RotationMatrix.RawComponents[0,0],RotationMatrix.RawComponents[0,1],RotationMatrix.RawComponents[0,2],0.0);
 PushConstants.RandomRotation1:=TpvVector4.InlineableCreate(RotationMatrix.RawComponents[1,0],RotationMatrix.RawComponents[1,1],RotationMatrix.RawComponents[1,2],0.0);
 PushConstants.RandomRotation2:=TpvVector4.InlineableCreate(RotationMatrix.RawComponents[2,0],RotationMatrix.RawComponents[2,1],RotationMatrix.RawComponents[2,2],0.0);
 PushConstants.Params.x:=TpvUInt32(aFrameIndex);
 PushConstants.Params.y:=TpvScene3DRendererInstance.CountGlobalIlluminationDDGICascades;
 PushConstants.Params.z:=TpvScene3DRendererInstance.GlobalIlluminationDDGIProbesPerCascade;
 PushConstants.Params.w:=TpvScene3DRendererInstance.GlobalIlluminationDDGIRaysPerProbe;
 PushConstants.Blend:=TpvVector4.InlineableCreate(0.97,0.0,0.0,0.0);

 // Make sure the host/transfer write of the uniform buffer is visible to the compute shaders.
 BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                    TVkAccessFlags(VK_ACCESS_UNIFORM_READ_BIT),
                                                    VK_QUEUE_FAMILY_IGNORED,VK_QUEUE_FAMILY_IGNORED,
                                                    fInstance.GlobalIlluminationDDGIUniformBuffers[aInFlightFrameIndex].Handle,0,VK_WHOLE_SIZE);
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,0,nil,1,@BufferMemoryBarrier,0,nil);

 DescriptorSets[0]:=fInstance.Scene3D.GlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle;
 DescriptorSets[1]:=fVulkanDescriptorSets[aInFlightFrameIndex].Handle;
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,fPipelineLayout.Handle,0,2,@DescriptorSets[0],0,nil);
 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants),@PushConstants);

 // 1) Trace: one thread per (ray, probe). local_size_x = 32.
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipelineTrace.Handle);
 aCommandBuffer.CmdDispatch((TpvScene3DRendererInstance.GlobalIlluminationDDGIRaysPerProbe+31) shr 5,TotalProbes,1);
 FullMemoryBarrier;

 // 2) Irradiance integration: one thread per probe (SH storage). local_size_x = 64.
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipelineIrradianceUpdate.Handle);
 aCommandBuffer.CmdDispatch((TotalProbes+63) shr 6,1,1);

 // 3) Visibility integration: one workgroup per probe (octahedral tile). local_size = 16x16.
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipelineVisibilityUpdate.Handle);
 aCommandBuffer.CmdDispatch(TotalProbes,1,1);
 FullMemoryBarrier;

 // 4) Border / guard band copy for the octahedral atlas(es). One workgroup per probe.
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipelineBorderUpdate.Handle);
 aCommandBuffer.CmdDispatch(TotalProbes,1,1);

 // Final barrier: publish the probe writes to every later shader stage that samples them (the mesh fragment shader
 // samples the irradiance and visibility atlases), matching how the radiance hints passes publish their volumes.
 FinalMemoryBarrier:=TVkMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                             TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT));
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   FrameGraph.VulkanDevice.PhysicalDevice.PipelineStageAllShaderBits,
                                   0,1,@FinalMemoryBarrier,0,nil,0,nil);

end;

end.
