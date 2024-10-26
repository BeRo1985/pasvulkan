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
unit PasVulkan.Scene3D.Renderer.Passes.MeshCullPass1ComputePass;
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
     PasVulkan.Scene3D.Renderer.Instance;

type { TpvScene3DRendererPassesMeshCullPass1ComputePass }
     TpvScene3DRendererPassesMeshCullPass1ComputePass=class(TpvFrameGraph.TComputePass)
      public
       type TPushConstants=packed record
             BaseDrawIndexedIndirectCommandIndex:TpvUInt32;
             CountDrawIndexedIndirectCommands:TpvUInt32;
             DrawCallIndex:TpvUInt32;
             CountObjectIndices:TpvUInt32;
             SkipCulling:TpvUInt32;
             VisibilityBufferOffset:TpvUInt32;
             TextureDepthIndex:TpvUInt32;
             BaseViewIndex:TpvUInt32;
             CountViews:TpvUInt32;
             BaseDrawIndexedIndirectCommandIndexForDisocclusions:TpvUInt32;
             DrawCallIndexForDisocclusions:TpvUInt32;
            end;
            PPushConstants=^TPushConstants;
      private
       fInstance:TpvScene3DRendererInstance;
       fCullRenderPass:TpvScene3DRendererCullRenderPass;
       fComputeShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStageCompute:TpvVulkanPipelineShaderStage;
       fPipelineLayout:TpvVulkanPipelineLayout;
       fPipeline:TpvVulkanComputePipeline;
       fPlanetCullPass:TpvScene3DPlanet.TCullPass;
      public
       constructor Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance;const aCullRenderPass:TpvScene3DRendererCullRenderPass); reintroduce;
       destructor Destroy; override;
       procedure AcquirePersistentResources; override;
       procedure ReleasePersistentResources; override;
       procedure AcquireVolatileResources; override;
       procedure ReleaseVolatileResources; override;
       procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
       procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
     end;

implementation

{ TpvScene3DRendererPassesMeshCullPass1ComputePass }

constructor TpvScene3DRendererPassesMeshCullPass1ComputePass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance;const aCullRenderPass:TpvScene3DRendererCullRenderPass);
begin
 inherited Create(aFrameGraph);

 fInstance:=aInstance;

 fCullRenderPass:=aCullRenderPass;

 case fCullRenderPass of
  TpvScene3DRendererCullRenderPass.FinalView:begin
   Name:='FinalViewMeshCullPass1ComputePass';
  end;
  TpvScene3DRendererCullRenderPass.CascadedShadowMap:begin
   Name:='CascadedShadowMapMeshCullPass1ComputePass';
  end;
  else begin
   Name:='MeshCullPass1ComputePass';
  end;
 end;

end;

destructor TpvScene3DRendererPassesMeshCullPass1ComputePass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesMeshCullPass1ComputePass.AcquirePersistentResources;
var Stream:TStream;
begin

 inherited AcquirePersistentResources;

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_cull_pass1_comp.spv');
 try
  fComputeShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
  fInstance.Renderer.VulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DRendererPassesMeshCullPass1ComputePass.fComputeShaderModule');
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageCompute:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

 fPlanetCullPass:=TpvScene3DPlanet.TCullPass.Create(fInstance.Renderer,
                                                    fInstance,
                                                    fInstance.Renderer.Scene3D,
                                                    fCullRenderPass,
                                                    1);

end;

procedure TpvScene3DRendererPassesMeshCullPass1ComputePass.ReleasePersistentResources;
begin
 FreeAndNil(fPlanetCullPass);
 FreeAndNil(fVulkanPipelineShaderStageCompute);
 FreeAndNil(fComputeShaderModule);
 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesMeshCullPass1ComputePass.AcquireVolatileResources;
var Index:TpvSizeInt;
begin

 inherited AcquireVolatileResources;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(fInstance.Renderer.VulkanDevice);
 fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TpvScene3DRendererPassesMeshCullPass1ComputePass.TPushConstants));
 fPipelineLayout.AddDescriptorSetLayout(fInstance.MeshCullPass1ComputeVulkanDescriptorSetLayout);
 fPipelineLayout.AddDescriptorSetLayout(fInstance.Scene3D.GlobalVulkanDescriptorSetLayout);
 fPipelineLayout.Initialize;

 fInstance.Renderer.VulkanDevice.DebugUtils.SetObjectName(fPipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DRendererPassesMeshCullPass1ComputePass.fPipelineLayout');

 fPipeline:=TpvVulkanComputePipeline.Create(fInstance.Renderer.VulkanDevice,
                                            fInstance.Renderer.VulkanPipelineCache,
                                            0,
                                            fVulkanPipelineShaderStageCompute,
                                            fPipelineLayout,
                                            nil,
                                            0);
 fInstance.Renderer.VulkanDevice.DebugUtils.SetObjectName(fPipeline.Handle,VK_OBJECT_TYPE_PIPELINE,'TpvScene3DRendererPassesMeshCullPass1ComputePass.fPipeline');

 fPlanetCullPass.AllocateResources;

end;

procedure TpvScene3DRendererPassesMeshCullPass1ComputePass.ReleaseVolatileResources;
var Index:TpvSizeInt;
begin
 fPlanetCullPass.ReleaseResources;
 FreeAndNil(fPipeline);
 FreeAndNil(fPipelineLayout);
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesMeshCullPass1ComputePass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvScene3DRendererPassesMeshCullPass1ComputePass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var RenderPass:TpvScene3DRendererRenderPass;
    DrawChoreographyBatchRangeIndex,
    FirstDrawCallIndex,
    CountDrawCallIndices,
    Part:TpvSizeInt;
    DrawChoreographyBatchRangeDynamicArray:TpvScene3D.PDrawChoreographyBatchRangeDynamicArray;
    DrawChoreographyBatchRangeIndexDynamicArray:TpvScene3D.PDrawChoreographyBatchRangeIndexDynamicArray;
    DrawChoreographyBatchRange:TpvScene3D.PDrawChoreographyBatchRange;
    BufferMemoryBarriers:array[0..3] of TVkBufferMemoryBarrier;
    PushConstants:TpvScene3DRendererPassesMeshCullPass1ComputePass.TPushConstants;
    DescriptorSets:array[0..1] of TVkDescriptorSet;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 case fCullRenderPass of
  TpvScene3DRendererCullRenderPass.FinalView:begin
   RenderPass:=TpvScene3DRendererRenderPass.View;
   Part:=0;
  end;
  TpvScene3DRendererCullRenderPass.CascadedShadowMap:begin
   RenderPass:=TpvScene3DRendererRenderPass.CascadedShadowMap;
   Part:=1;
  end;
  else begin
   exit;
  end;
 end;

 begin

  fPlanetCullPass.Execute(aCommandBuffer,aInFlightFrameIndex);

  fInstance.Renderer.VulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3D.Mesh',[0.5,0.25,0.75,1.0]);

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandInputBuffers[aInFlightFrameIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandVisibilityBuffers[aInFlightFrameIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[2]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandOutputBuffers[aInFlightFrameIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[3]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                                         TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandCounterBuffers[aInFlightFrameIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT) or
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                    0,
                                    0,nil,
                                    4,@BufferMemoryBarriers[0],
                                    0,nil);

  if fInstance.PerInFlightFrameGPUCulledArray[aInFlightFrameIndex,RenderPass] then begin

   FirstDrawCallIndex:=0;
   CountDrawCallIndices:=0;

   DrawChoreographyBatchRangeDynamicArray:=@fInstance.DrawChoreographyBatchRangeFrameBuckets[aInFlightFrameIndex];

   DrawChoreographyBatchRangeIndexDynamicArray:=@fInstance.DrawChoreographyBatchRangeFrameRenderPassBuckets[aInFlightFrameIndex,RenderPass];

   for DrawChoreographyBatchRangeIndex:=0 to DrawChoreographyBatchRangeIndexDynamicArray^.Count-1 do begin

    DrawChoreographyBatchRange:=@DrawChoreographyBatchRangeDynamicArray^.Items[DrawChoreographyBatchRangeIndexDynamicArray^.Items[DrawChoreographyBatchRangeIndex]];

    if DrawChoreographyBatchRange^.CountCommands>0 then begin

     if (CountDrawCallIndices=0) or ((FirstDrawCallIndex+CountDrawCallIndices)<>DrawChoreographyBatchRange^.DrawCallIndex) then begin
      if CountDrawCallIndices>0 then begin
       aCommandBuffer.CmdFillBuffer(fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandCounterBuffers[aInFlightFrameIndex].Handle,
                                    FirstDrawCallIndex*SizeOf(TVkUInt32),
                                    CountDrawCallIndices*SizeOf(TVkUInt32),
                                    0);
       aCommandBuffer.CmdFillBuffer(fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandCounterBuffers[aInFlightFrameIndex].Handle,
                                    (TpvScene3DRendererInstance.MaxMultiIndirectDrawCalls+FirstDrawCallIndex)*SizeOf(TVkUInt32),
                                    CountDrawCallIndices*SizeOf(TVkUInt32),
                                    0);
      end;
      FirstDrawCallIndex:=DrawChoreographyBatchRange^.DrawCallIndex;
      CountDrawCallIndices:=0;
     end;

     inc(CountDrawCallIndices);

    end;

   end;

   if CountDrawCallIndices>0 then begin
    aCommandBuffer.CmdFillBuffer(fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandCounterBuffers[aInFlightFrameIndex].Handle,
                                 FirstDrawCallIndex*SizeOf(TVkUInt32),
                                 CountDrawCallIndices*SizeOf(TVkUInt32),
                                 0);
    aCommandBuffer.CmdFillBuffer(fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandCounterBuffers[aInFlightFrameIndex].Handle,
                                 (TpvScene3DRendererInstance.MaxMultiIndirectDrawCalls+FirstDrawCallIndex)*SizeOf(TVkUInt32),
                                 CountDrawCallIndices*SizeOf(TVkUInt32),
                                 0);
   end;

  end;

{ aCommandBuffer.CmdFillBuffer(fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandCounterBuffers[aInFlightFrameIndex].Handle,
                               0,
                               VK_WHOLE_SIZE,
                               0);}

  aCommandBuffer.CmdFillBuffer(fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandVisibilityBuffers[aInFlightFrameIndex].Handle,
                               fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandVisibilityBufferPartSizes[aInFlightFrameIndex]*TpvUInt32(Part)*SizeOf(TVkUInt32),
                               fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandVisibilityBufferPartSizes[aInFlightFrameIndex]*SizeOf(TVkUInt32),
                               0);

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandVisibilityBuffers[aInFlightFrameIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandCounterBuffers[aInFlightFrameIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    2,@BufferMemoryBarriers[0],
                                    0,nil);

  aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

  DescriptorSets[0]:=fInstance.MeshCullPass1ComputeVulkanDescriptorSets[aInFlightFrameIndex].Handle;
  DescriptorSets[1]:=fInstance.Scene3D.GlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle;

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       fPipelineLayout.Handle,
                                       0,
                                       2,
                                       @DescriptorSets[0],
                                       0,
                                       nil);

  if fInstance.PerInFlightFrameGPUCulledArray[aInFlightFrameIndex,RenderPass] then begin

   DrawChoreographyBatchRangeDynamicArray:=@fInstance.DrawChoreographyBatchRangeFrameBuckets[aInFlightFrameIndex];

   DrawChoreographyBatchRangeIndexDynamicArray:=@fInstance.DrawChoreographyBatchRangeFrameRenderPassBuckets[aInFlightFrameIndex,RenderPass];

   for DrawChoreographyBatchRangeIndex:=0 to DrawChoreographyBatchRangeIndexDynamicArray^.Count-1 do begin

    DrawChoreographyBatchRange:=@DrawChoreographyBatchRangeDynamicArray^.Items[DrawChoreographyBatchRangeIndexDynamicArray^.Items[DrawChoreographyBatchRangeIndex]];

    if DrawChoreographyBatchRange^.CountCommands>0 then begin

     PushConstants.BaseDrawIndexedIndirectCommandIndex:=DrawChoreographyBatchRange^.FirstCommand;
     PushConstants.CountDrawIndexedIndirectCommands:=DrawChoreographyBatchRange^.CountCommands;
     PushConstants.CountObjectIndices:=fInstance.PerInFlightFrameGPUCountObjectIndicesArray[aInFlightFrameIndex];
     PushConstants.DrawCallIndex:=DrawChoreographyBatchRange^.DrawCallIndex;
     PushConstants.SkipCulling:=0;
     PushConstants.VisibilityBufferOffset:=fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandVisibilityBufferPartSizes[aInFlightFrameIndex]*TpvUInt32(Part);
     PushConstants.TextureDepthIndex:=Part;
     case fCullRenderPass of
      TpvScene3DRendererCullRenderPass.FinalView:begin
       PushConstants.BaseViewIndex:=fInstance.InFlightFrameStates^[aInFlightFrameIndex].FinalViewIndex;
       PushConstants.CountViews:=fInstance.InFlightFrameStates^[aInFlightFrameIndex].CountFinalViews;
      end;
      TpvScene3DRendererCullRenderPass.CascadedShadowMap:begin
       PushConstants.BaseViewIndex:=fInstance.InFlightFrameStates^[aInFlightFrameIndex].CascadedShadowMapViewIndex;
       PushConstants.CountViews:=fInstance.InFlightFrameStates^[aInFlightFrameIndex].CountCascadedShadowMapViews;
      end;
      else begin
       Assert(false);
      end;
     end;
     PushConstants.BaseDrawIndexedIndirectCommandIndexForDisocclusions:=fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandDisocclusionOffsets[aInFlightFrameIndex]+DrawChoreographyBatchRange^.FirstCommand;
     PushConstants.DrawCallIndexForDisocclusions:=TpvScene3DRendererInstance.MaxMultiIndirectDrawCalls+DrawChoreographyBatchRange^.DrawCallIndex;

     aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                     TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_COMPUTE_BIT),
                                     0,
                                     SizeOf(TpvScene3DRendererPassesMeshCullPass1ComputePass.TPushConstants),
                                     @PushConstants);

     aCommandBuffer.CmdDispatch((DrawChoreographyBatchRange^.CountCommands+255) shr 8,1,1);

    end;

   end;

  end;

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandInputBuffers[aInFlightFrameIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandVisibilityBuffers[aInFlightFrameIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[2]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandOutputBuffers[aInFlightFrameIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[3]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandCounterBuffers[aInFlightFrameIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT) or
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    4,@BufferMemoryBarriers[0],
                                    0,nil);

  fInstance.Renderer.VulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

 end;

end;

end.
