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
unit PasVulkan.Scene3D.Renderer.Passes.MeshCullPass0ComputePass;
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

type { TpvScene3DRendererPassesMeshCullPass0ComputePass }
     TpvScene3DRendererPassesMeshCullPass0ComputePass=class(TpvFrameGraph.TComputePass)
      public
       type TPushConstants=packed record
             LODLevelCurrentBDA:TVkDeviceAddress;
             LODLevelPreviousBDA:TVkDeviceAddress;
             CountRanges:TpvUInt32;
             TotalCommands:TpvUInt32;
             CountMeshObjectIDs:TpvUInt32;
             SkipCulling:TpvUInt32;
             BatchRangeOffset:TpvUInt32;
             PrefixSumOffset:TpvUInt32;
             VisibilityBufferOffset:TpvUInt32;
             RenderPassMask:TpvUInt32;
             BaseViewIndex:TpvUInt32;
             CountViews:TpvUInt32;
             RendererInstanceIndex:TpvUInt32;
             Flags:TpvUInt32;
             BatchRangeIndex:TpvInt32;
            end;
            PPushConstants=^TPushConstants;
            TMeshCullResetPushConstants=packed record
             CountRanges:TpvUInt32;
             MaxMultiIndirectDrawCalls:TpvUInt32;
             BatchRangeOffset:TpvUInt32;
             PrefixSumOffset:TpvUInt32;
             CullDispatchIndex:TpvUInt32;
            end;
            PMeshCullResetPushConstants=^TMeshCullResetPushConstants;
      private
       fInstance:TpvScene3DRendererInstance;
       fCullRenderPass:TpvScene3DRendererCullRenderPass;
       fComputeShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStageCompute:TpvVulkanPipelineShaderStage;
       fPipelineLayout:TpvVulkanPipelineLayout;
       fPipeline:TpvVulkanComputePipeline;
       fMeshShaderComputeShaderModule:TpvVulkanShaderModule;
       fMeshShaderVulkanPipelineShaderStageCompute:TpvVulkanPipelineShaderStage;
       fMeshShaderPipeline:TpvVulkanComputePipeline;
       fPlanetCullPass:TpvScene3DPlanet.TCullPass;
//     fPlanetCullPass2:TpvScene3DPlanet.TCullPass;
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

{ TpvScene3DRendererPassesMeshCullPass0ComputePass }

constructor TpvScene3DRendererPassesMeshCullPass0ComputePass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance;const aCullRenderPass:TpvScene3DRendererCullRenderPass);
begin
 inherited Create(aFrameGraph);

 fInstance:=aInstance;

 fCullRenderPass:=aCullRenderPass;

 case fCullRenderPass of
  TpvScene3DRendererCullRenderPass.FinalView:begin
   Name:='FinalViewMeshCullPass0ComputePass';
  end;
  TpvScene3DRendererCullRenderPass.CascadedShadowMap:begin
   Name:='CascadedShadowMapMeshCullPass0ComputePass';
  end;
  else begin
   Name:='MeshCullPass0ComputePass';
  end;
 end;

end;

destructor TpvScene3DRendererPassesMeshCullPass0ComputePass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesMeshCullPass0ComputePass.AcquirePersistentResources;
var Stream:TStream;
begin

 inherited AcquirePersistentResources;

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_cull_pass0_comp.spv');
 try
  fComputeShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
  fInstance.Renderer.VulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DRendererPassesMeshCullPass0ComputePass.fComputeShaderModule');
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageCompute:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

 if fInstance.Scene3D.MeshShaderSupport then begin
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_cull_meshshader_pass0_comp.spv');
  try
   fMeshShaderComputeShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
   fInstance.Renderer.VulkanDevice.DebugUtils.SetObjectName(fMeshShaderComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DRendererPassesMeshCullPass0ComputePass.fMeshShaderComputeShaderModule');
  finally
   Stream.Free;
  end;
  fMeshShaderVulkanPipelineShaderStageCompute:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fMeshShaderComputeShaderModule,'main');
 end else begin
  fMeshShaderComputeShaderModule:=nil;
  fMeshShaderVulkanPipelineShaderStageCompute:=nil;
 end;

 fPlanetCullPass:=TpvScene3DPlanet.TCullPass.Create(fInstance.Renderer,
                                                    fInstance,
                                                    fInstance.Renderer.Scene3D,
                                                    fCullRenderPass,
                                                    0);

{fPlanetCullPass2:=TpvScene3DPlanet.TCullPass.Create(fInstance.Renderer,
                                                     fInstance,
                                                     fInstance.Renderer.Scene3D,
                                                     TpvScene3DRendererCullRenderPass.CascadedShadowMap,
                                                     -1);}

end;

procedure TpvScene3DRendererPassesMeshCullPass0ComputePass.ReleasePersistentResources;
begin
 FreeAndNil(fPlanetCullPass);
//FreeAndNil(fPlanetCullPass2);
 FreeAndNil(fMeshShaderVulkanPipelineShaderStageCompute);
 FreeAndNil(fMeshShaderComputeShaderModule);
 FreeAndNil(fVulkanPipelineShaderStageCompute);
 FreeAndNil(fComputeShaderModule);
 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesMeshCullPass0ComputePass.AcquireVolatileResources;
var Index:TpvSizeInt;
begin

 inherited AcquireVolatileResources;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(fInstance.Renderer.VulkanDevice);
 fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TpvScene3DRendererPassesMeshCullPass0ComputePass.TPushConstants));
 fPipelineLayout.AddDescriptorSetLayout(fInstance.MeshCullPass0ComputeVulkanDescriptorSetLayout);
 fPipelineLayout.AddDescriptorSetLayout(fInstance.Scene3D.GlobalVulkanDescriptorSetLayout);
 fPipelineLayout.AddDescriptorSetLayout(fInstance.Scene3D.GlobalBoundingSphereVulkanDescriptorSetLayout);
 fPipelineLayout.Initialize;

 fInstance.Renderer.VulkanDevice.DebugUtils.SetObjectName(fPipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DRendererPassesMeshCullPass0ComputePass.fPipelineLayout');

 fPipeline:=TpvVulkanComputePipeline.Create(fInstance.Renderer.VulkanDevice,
                                            fInstance.Renderer.VulkanPipelineCache,
                                            0,
                                            fVulkanPipelineShaderStageCompute,
                                            fPipelineLayout,
                                            nil,
                                            0);
 fInstance.Renderer.VulkanDevice.DebugUtils.SetObjectName(fPipeline.Handle,VK_OBJECT_TYPE_PIPELINE,'TpvScene3DRendererPassesMeshCullPass0ComputePass.fPipeline');

 if assigned(fMeshShaderVulkanPipelineShaderStageCompute) then begin
  fMeshShaderPipeline:=TpvVulkanComputePipeline.Create(fInstance.Renderer.VulkanDevice,
                                                       fInstance.Renderer.VulkanPipelineCache,
                                                       0,
                                                       fMeshShaderVulkanPipelineShaderStageCompute,
                                                       fPipelineLayout,
                                                       nil,
                                                       0);
  fInstance.Renderer.VulkanDevice.DebugUtils.SetObjectName(fMeshShaderPipeline.Handle,VK_OBJECT_TYPE_PIPELINE,'TpvScene3DRendererPassesMeshCullPass0ComputePass.fMeshShaderPipeline');
 end else begin
  fMeshShaderPipeline:=nil;
 end;

 fPlanetCullPass.AllocateResources;

//fPlanetCullPass2.AllocateResources;

end;

procedure TpvScene3DRendererPassesMeshCullPass0ComputePass.ReleaseVolatileResources;
var Index:TpvSizeInt;
begin
 fPlanetCullPass.ReleaseResources;
//fPlanetCullPass2.ReleaseResources;
 FreeAndNil(fMeshShaderPipeline);
 FreeAndNil(fPipeline);
 FreeAndNil(fPipelineLayout);
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesMeshCullPass0ComputePass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvScene3DRendererPassesMeshCullPass0ComputePass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var RenderPass:TpvScene3DRendererRenderPass;
    PreviousInFlightFrameIndex,
    Part:TpvSizeInt;
    BufferMemoryBarriers:array[0..3] of TVkBufferMemoryBarrier;
    PushConstants:TpvScene3DRendererPassesMeshCullPass0ComputePass.TPushConstants;
    ResetPushConstants:TMeshCullResetPushConstants;
    DescriptorSets:array[0..2] of TVkDescriptorSet;
    CountRanges,TotalCommands:TpvUInt32;
    RangeIndex,BatchRangeOffset,RangeCountCommands:TpvUInt32;
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

  PreviousInFlightFrameIndex:=FrameGraph.DrawPreviousInFlightFrameIndex;

  fPlanetCullPass.Execute(aCommandBuffer,aInFlightFrameIndex);

//fPlanetCullPass2.Execute(aCommandBuffer,aInFlightFrameIndex);

  fInstance.Renderer.VulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3D.Mesh',[0.5,0.25,0.75,1.0]);

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandInputBuffers[aInFlightFrameIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandVisibilityBuffers[PreviousInFlightFrameIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[2]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandOutputBuffers[aInFlightFrameIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[3]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandCounterBuffers[aInFlightFrameIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT) or
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT) or
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or
                                    TVkPipelineStageFlags(IfThen(fInstance.Scene3D.MeshShaderSupport,
                                                                 TVkFlags(VK_PIPELINE_STAGE_TASK_SHADER_BIT_EXT) or
                                                                 TVkFlags(VK_PIPELINE_STAGE_MESH_SHADER_BIT_EXT),
                                                                 0)),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    4,@BufferMemoryBarriers[0],
                                    0,nil);

  if fInstance.PerInFlightFrameGPUCulledArray[aInFlightFrameIndex,RenderPass] then begin

   CountRanges:=fInstance.PerInFlightFrameMeshCullBatchRangeCounts[aInFlightFrameIndex,fCullRenderPass];
   TotalCommands:=fInstance.PerInFlightFrameMeshCullTotalCommands[aInFlightFrameIndex,fCullRenderPass];

   if (CountRanges>0) and (TotalCommands>0) then begin

    aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fInstance.MeshCullReset.Pipeline.Handle);

    DescriptorSets[0]:=fInstance.MeshCullReset.VulkanDescriptorSets[aInFlightFrameIndex].Handle;

    aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                         fInstance.MeshCullReset.PipelineLayout.Handle,
                                         0,
                                         1,
                                         @DescriptorSets[0],
                                         0,
                                         nil);

    ResetPushConstants.CountRanges:=CountRanges;
    ResetPushConstants.MaxMultiIndirectDrawCalls:=TpvScene3DRendererInstance.MaxMultiIndirectDrawCalls;
    ResetPushConstants.BatchRangeOffset:=fInstance.PerInFlightFrameMeshCullBatchRangeOffsets[aInFlightFrameIndex,fCullRenderPass];
    ResetPushConstants.PrefixSumOffset:=fInstance.PerInFlightFrameMeshCullPrefixSumOffsets[aInFlightFrameIndex,fCullRenderPass];
    ResetPushConstants.CullDispatchIndex:=TpvUInt32(Part);

    aCommandBuffer.CmdPushConstants(fInstance.MeshCullReset.PipelineLayout.Handle,
                                    TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_COMPUTE_BIT),
                                    0,
                                    SizeOf(ResetPushConstants),
                                    @ResetPushConstants);

    if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
     fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.BeginBreadcrumb(aCommandBuffer.Handle,TpvVulkanBreadcrumbType.Dispatch,'MeshCullPass0ComputePass.ResetDispatch');
    end;
    aCommandBuffer.CmdDispatch((CountRanges+255) shr 8,1,1);
    if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
     fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.EndBreadcrumb(aCommandBuffer.Handle);
    end;

   end;

  end;

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandCounterBuffers[aInFlightFrameIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.PerInFlightFrameMeshCullIndirectDispatchBuffers[aInFlightFrameIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT),
                                    0,
                                    0,nil,
                                    2,@BufferMemoryBarriers[0],
                                    0,nil);

  if fInstance.Renderer.UseMeshShaderPipeline and assigned(fMeshShaderPipeline) then begin
{$ifdef MeshShaderDebug}
   WriteLn('[DEBUG-MS] MeshCullPass0: Using MESH_SHADER_PATH pipeline');
{$endif}
   aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fMeshShaderPipeline.Handle);
  end else begin
   aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);
  end;

  DescriptorSets[0]:=fInstance.MeshCullPass0ComputeVulkanDescriptorSets[aInFlightFrameIndex].Handle;
  DescriptorSets[1]:=fInstance.Scene3D.GlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle;
  DescriptorSets[2]:=fInstance.Scene3D.GlobalBoundingSphereVulkanDescriptorSets[aInFlightFrameIndex].Handle;

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       fPipelineLayout.Handle,
                                       0,
                                       3,
                                       @DescriptorSets[0],
                                       0,
                                       nil);

  if fInstance.PerInFlightFrameGPUCulledArray[aInFlightFrameIndex,RenderPass] then begin

   CountRanges:=fInstance.PerInFlightFrameMeshCullBatchRangeCounts[aInFlightFrameIndex,fCullRenderPass];
   TotalCommands:=fInstance.PerInFlightFrameMeshCullTotalCommands[aInFlightFrameIndex,fCullRenderPass];

   if (CountRanges>0) and (TotalCommands>0) then begin

    PushConstants.CountRanges:=CountRanges;
    PushConstants.TotalCommands:=TotalCommands;
    PushConstants.CountMeshObjectIDs:=fInstance.PerInFlightFrameGPUCountMeshObjectIDsArray[PreviousInFlightFrameIndex];
    PushConstants.SkipCulling:=IfThen((aFrameIndex=0) or
                                      fInstance.InFlightFrameStates[aInFlightFrameIndex].CameraReset or
                                      (fInstance.PerInFlightFrameGPUCountMeshObjectIDsArray[PreviousInFlightFrameIndex]=0),1,0);
    PushConstants.BatchRangeOffset:=fInstance.PerInFlightFrameMeshCullBatchRangeOffsets[aInFlightFrameIndex,fCullRenderPass];
    PushConstants.PrefixSumOffset:=fInstance.PerInFlightFrameMeshCullPrefixSumOffsets[aInFlightFrameIndex,fCullRenderPass];
    PushConstants.VisibilityBufferOffset:=fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandVisibilityBufferPartSizes[PreviousInFlightFrameIndex]*TpvUInt32(Part);

    case fCullRenderPass of
     TpvScene3DRendererCullRenderPass.FinalView:begin
      PushConstants.BaseViewIndex:=fInstance.InFlightFrameStates^[aInFlightFrameIndex].FinalViewIndex;
      PushConstants.CountViews:=fInstance.InFlightFrameStates^[aInFlightFrameIndex].CountFinalViews;
      PushConstants.RenderPassMask:=TpvUInt32(1) shl TpvUInt32(ord(TpvScene3DRendererRenderPass.View));
     end;
     TpvScene3DRendererCullRenderPass.CascadedShadowMap:begin
      PushConstants.BaseViewIndex:=fInstance.InFlightFrameStates^[aInFlightFrameIndex].CascadedShadowMapViewIndex;
      PushConstants.CountViews:=fInstance.InFlightFrameStates^[aInFlightFrameIndex].CountCascadedShadowMapViews;
      PushConstants.RenderPassMask:=TpvUInt32(1) shl TpvUInt32(ord(TpvScene3DRendererRenderPass.CascadedShadowMap));
     end;
     else begin
      PushConstants.BaseViewIndex:=0;
      PushConstants.CountViews:=0;
      PushConstants.RenderPassMask:=$ffff;
     end;
    end;

    PushConstants.RendererInstanceIndex:=TpvUInt32(fInstance.RendererInstanceIndex);
    PushConstants.Flags:=0;
    if fInstance.Scene3D.GPULODEnabled then begin
     case fCullRenderPass of
      TpvScene3DRendererCullRenderPass.FinalView:begin
       PushConstants.Flags:=PushConstants.Flags or TpvUInt32(1 shl 0); // FLAG_LOD_ENABLED
      end;
      else begin
       // LOD selection only for final view pass for now
      end;
     end;
     if not fInstance.Scene3D.LODTransformAllLevels then begin
      PushConstants.Flags:=PushConstants.Flags or TpvUInt32(1 shl 1); // FLAG_LOD_TEMPORAL
     end;
     if fInstance.Scene3D.LODFrameCounter<fInstance.Scene3D.CountInFlightFrames then begin
      PushConstants.Flags:=PushConstants.Flags or TpvUInt32(1 shl 2); // FLAG_LOD_RESET_FRAME
     end;
     if assigned(fInstance.LODLevelBuffers[aInFlightFrameIndex]) then begin
      PushConstants.LODLevelCurrentBDA:=fInstance.LODLevelBuffers[aInFlightFrameIndex].DeviceAddress;
     end else begin
      PushConstants.LODLevelCurrentBDA:=0;
     end;
     if assigned(fInstance.LODLevelBuffers[PreviousInFlightFrameIndex]) then begin
      PushConstants.LODLevelPreviousBDA:=fInstance.LODLevelBuffers[PreviousInFlightFrameIndex].DeviceAddress;
     end else begin
      PushConstants.LODLevelPreviousBDA:=0;
     end;
    end else begin 
     PushConstants.LODLevelCurrentBDA:=0;
     PushConstants.LODLevelPreviousBDA:=0;
    end;

    if fInstance.Renderer.UseMeshShaderPipeline and assigned(fMeshShaderPipeline) then begin
     PushConstants.Flags:=PushConstants.Flags or TpvUInt32(1 shl 3); // FLAG_MESHLET_CULLING_ENABLED
    end;

    if fInstance.Scene3D.UseMegaDispatch then begin

     PushConstants.BatchRangeIndex:=-1;

     aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                     TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_COMPUTE_BIT),
                                     0,
                                     SizeOf(TpvScene3DRendererPassesMeshCullPass0ComputePass.TPushConstants),
                                     @PushConstants);

     if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
      fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.BeginBreadcrumb(aCommandBuffer.Handle,TpvVulkanBreadcrumbType.Dispatch,'MeshCullPass0ComputePass.Dispatch');
     end;
     aCommandBuffer.CmdDispatchIndirect(fInstance.PerInFlightFrameMeshCullIndirectDispatchBuffers[aInFlightFrameIndex].Handle,
                                        TpvUInt32(Part)*SizeOf(TVkDispatchIndirectCommand));
     if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
      fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.EndBreadcrumb(aCommandBuffer.Handle);
     end;

    end else begin

     BatchRangeOffset:=fInstance.PerInFlightFrameMeshCullBatchRangeOffsets[aInFlightFrameIndex,fCullRenderPass];

     for RangeIndex:=0 to CountRanges-1 do begin

      RangeCountCommands:=fInstance.GPUBatchRanges[BatchRangeOffset+RangeIndex].CountCommands;

      PushConstants.BatchRangeIndex:=TpvInt32(RangeIndex);

      aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                      TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_COMPUTE_BIT),
                                      0,
                                      SizeOf(TpvScene3DRendererPassesMeshCullPass0ComputePass.TPushConstants),
                                      @PushConstants);

      if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
       fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.BeginBreadcrumb(aCommandBuffer.Handle,TpvVulkanBreadcrumbType.Dispatch,'MeshCullPass0ComputePass.Dispatch');
      end;
      aCommandBuffer.CmdDispatch((RangeCountCommands+255) shr 8,1,1);
      if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
       fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.EndBreadcrumb(aCommandBuffer.Handle);
      end;

     end;

    end;

   end;

  end;

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandInputBuffers[aInFlightFrameIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandVisibilityBuffers[PreviousInFlightFrameIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[2]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandOutputBuffers[aInFlightFrameIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[3]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.PerInFlightFrameGPUDrawIndexedIndirectCommandCounterBuffers[aInFlightFrameIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT) or
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or
                                    TVkPipelineStageFlags(IfThen(fInstance.Scene3D.MeshShaderSupport,
                                                                 TVkFlags(VK_PIPELINE_STAGE_TASK_SHADER_BIT_EXT) or
                                                                 TVkFlags(VK_PIPELINE_STAGE_MESH_SHADER_BIT_EXT),
                                                                 0)),
                                    0,
                                    0,nil,
                                    4,@BufferMemoryBarriers[0],
                                    0,nil);

  fInstance.Renderer.VulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

 end;

end;


end.
