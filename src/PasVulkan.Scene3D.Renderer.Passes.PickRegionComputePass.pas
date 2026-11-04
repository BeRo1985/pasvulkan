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
unit PasVulkan.Scene3D.Renderer.Passes.PickRegionComputePass;
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

type { TpvScene3DRendererPassesPickRegionComputePass }
     // Region picking: answers which mesh object ids are visible inside a region of the id target, as a
     // bitset over the ids that is copied into a host visible buffer, one per in-flight frame. The region
     // comes as row spans (TpvScene3DRendererInstance.RequestPickRegion), so a rectangle, a lasso or any
     // other shape is all the same here. Reading the answer is what the renderer instance does a full
     // round of in-flight frames later, exactly as for the single pixel of the point pick.
     //
     // It reads the very id target the pick render pass draws, so a region costs no second scene pass,
     // and it records something only in a frame a region pick was asked for - in every other one it does
     // nothing at all. A point pick in the same frame is not touched by it, nor the other way round: the
     // two have their own requests, their own buffers and their own answers, and share nothing but the
     // drawn target.
     //
     // Ordered between the pick render pass and the point pick's read back on purpose (see the frame graph
     // wiring in the renderer instance): the target has to be shader readable here and TRANSFER_SRC there,
     // and with a fixed order the frame graph moves it from the one layout to the other once per frame.
     TpvScene3DRendererPassesPickRegionComputePass=class(TpvFrameGraph.TComputePass)
      public
       type TPushConstants=packed record
             CountSpans:TpvUInt32; // spans in the span buffer
             CountWords:TpvUInt32; // words in the bitset, ids beyond them are dropped
             Width:TpvInt32;       // size of the id target, the shader clamps against it once more
             Height:TpvInt32;
            end;
            PPushConstants=^TPushConstants;
      private
       fInstance:TpvScene3DRendererInstance;
       fResourceID:TpvFrameGraph.TPass.TUsedImageResource;
       fComputeShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStageCompute:TpvVulkanPipelineShaderStage;
       fVulkanImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
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
       procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
     end;

implementation

{ TpvScene3DRendererPassesPickRegionComputePass }

constructor TpvScene3DRendererPassesPickRegionComputePass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin

 inherited Create(aFrameGraph);

 fInstance:=aInstance;

 Name:='PickRegionComputePass';

 Queue:=aFrameGraph.UniversalQueue;

 // Asking for SHADER_READ_ONLY_OPTIMAL is what makes the frame graph transition the id target for us,
 // just as the point pick's read back gets it as a copy source by asking for TRANSFER_SRC_OPTIMAL.
 fResourceID:=AddImageInput('resourcetype_pick_id',
                            'resource_pick_id',
                            VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                            []
                           );

end;

destructor TpvScene3DRendererPassesPickRegionComputePass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesPickRegionComputePass.AcquirePersistentResources;
var Stream:TStream;
begin

 inherited AcquirePersistentResources;

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('pick_region_comp.spv');
 try
  fComputeShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fInstance.Renderer.VulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DRendererPassesPickRegionComputePass.fComputeShaderModule');

 fVulkanPipelineShaderStageCompute:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

end;

procedure TpvScene3DRendererPassesPickRegionComputePass.ReleasePersistentResources;
begin
 FreeAndNil(fVulkanPipelineShaderStageCompute);
 FreeAndNil(fComputeShaderModule);
 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesPickRegionComputePass.AcquireVolatileResources;
var InFlightFrameIndex:TpvInt32;
begin

 inherited AcquireVolatileResources;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(fInstance.Renderer.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fInstance.Renderer.CountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fInstance.Renderer.CountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,fInstance.Renderer.CountInFlightFrames*2); // spans + bitset
 fVulkanDescriptorPool.Initialize;

 // 0 = the id target, 1 = the spans (read), 2 = the bitset (written).
 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fInstance.Renderer.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(1,
                                       VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(2,
                                       VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(fInstance.Renderer.VulkanDevice);
 fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
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

  // Only the first layer, and as a plain 2D view: picking happens through the main eye, and a 2D view of
  // one layer is just as valid on the array image of a multiview target, so one shader serves both.
  fVulkanImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                                                   fResourceID.VulkanImages[InFlightFrameIndex],
                                                                   TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D),
                                                                   TpvFrameGraph.TImageResourceType(fResourceID.ResourceType).Format,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                   0,
                                                                   1,
                                                                   0,
                                                                   1
                                                                  );

  // Only the id target is written here. The two buffers are written in Execute, every time the pass
  // dispatches, because the renderer instance grows them on demand - see there.
  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);
  // Nearest, since the ids are fetched texel by texel and an integer format has nothing to filter anyway.
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                                fVulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceID.ResourceTransition.Layout)],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;

 end;

end;

procedure TpvScene3DRendererPassesPickRegionComputePass.ReleaseVolatileResources;
var InFlightFrameIndex:TpvInt32;
begin
 FreeAndNil(fPipeline);
 FreeAndNil(fPipelineLayout);
 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fVulkanImageViews[InFlightFrameIndex]);
 end;
 FreeAndNil(fVulkanDescriptorSetLayout);
 FreeAndNil(fVulkanDescriptorPool);
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesPickRegionComputePass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var CountSpans,CountWords,CountGroupsX,CountGroupsY:TpvSizeInt;
    MaxSpanWidth:TpvInt32;
    SpanBuffer,BitSetBuffer,ReadBackBuffer:TpvVulkanBuffer;
    PushConstants:TPushConstants;
    MemoryBarrier:TVkMemoryBarrier;
    BufferMemoryBarrier:TVkBufferMemoryBarrier;
    BufferCopy:TVkBufferCopy;
begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 // Nothing to look for in a frame nobody asked about - and clearing and copying anyway would overwrite
 // the answer to the region pick that is still being waited for.
 if not fInstance.PickRegionActive(aInFlightFrameIndex) then begin
  exit;
 end;

 // The instance clamps the spans to the target, makes its buffers large enough for them and for the
 // mesh object ids there are now, and fills the span buffer - so what is left here is recording.
 if not fInstance.PreparePickRegion(aInFlightFrameIndex,fResourceID.Width,fResourceID.Height,CountSpans,CountWords,MaxSpanWidth) then begin
  exit;
 end;

 SpanBuffer:=fInstance.PickRegionSpanBuffers[aInFlightFrameIndex];
 BitSetBuffer:=fInstance.PickRegionBitSetBuffers[aInFlightFrameIndex];
 ReadBackBuffer:=fInstance.PickRegionReadBackBuffers[aInFlightFrameIndex];

 if not (assigned(SpanBuffer) and assigned(BitSetBuffer) and assigned(ReadBackBuffer)) then begin
  exit;
 end;

 // Written anew in every frame that dispatches rather than once: the instance replaces these buffers by
 // larger ones whenever a request or the scene outgrows them, and a set written once would keep pointing
 // at the old ones. Safe right here - this in-flight frame's command buffer is being recorded anew, which
 // means the frame that used the set last is through on the GPU, and the write lands before the bind.
 fVulkanDescriptorSets[aInFlightFrameIndex].WriteToDescriptorSet(1,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                 [],
                                                                 [SpanBuffer.DescriptorBufferInfo],
                                                                 [],
                                                                 false
                                                                );
 fVulkanDescriptorSets[aInFlightFrameIndex].WriteToDescriptorSet(2,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                 [],
                                                                 [BitSetBuffer.DescriptorBufferInfo],
                                                                 [],
                                                                 false
                                                                );
 fVulkanDescriptorSets[aInFlightFrameIndex].Flush;

 // Cleared over exactly the words this frame reads back, so that a bit left set is one the dispatch set.
 aCommandBuffer.CmdFillBuffer(BitSetBuffer.Handle,0,CountWords*SizeOf(TpvUInt32),0);

 // The clear for the dispatch, and for the copy as well when there is no dispatch - plus the spans the
 // host has just written, which the dispatch reads.
 FillChar(MemoryBarrier,SizeOf(TVkMemoryBarrier),#0);
 MemoryBarrier.sType:=VK_STRUCTURE_TYPE_MEMORY_BARRIER;
 MemoryBarrier.pNext:=nil;
 MemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT);
 MemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT);
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   0,
                                   1,@MemoryBarrier,
                                   0,nil,
                                   0,nil);

 // Without a single span left the cleared bitset is the answer: nothing is in the region.
 if CountSpans>0 then begin

  PushConstants.CountSpans:=CountSpans;
  PushConstants.CountWords:=CountWords;
  PushConstants.Width:=fResourceID.Width;
  PushConstants.Height:=fResourceID.Height;

  // One workgroup row per span and as many 64 wide workgroups as the widest span needs, both clamped to
  // what every device must support. The shader walks spans and columns by the dispatched size, so a
  // clamped dispatch still covers everything, just with more work per invocation.
  CountGroupsX:=(MaxSpanWidth+63) shr 6;
  if CountGroupsX<1 then begin
   CountGroupsX:=1;
  end else if CountGroupsX>65535 then begin
   CountGroupsX:=65535;
  end;
  CountGroupsY:=CountSpans;
  if CountGroupsY>65535 then begin
   CountGroupsY:=65535;
  end;

  aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       fPipelineLayout.Handle,
                                       0,
                                       1,
                                       @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil);

  aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  0,
                                  SizeOf(TPushConstants),
                                  @PushConstants);

  if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
   fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.BeginBreadcrumb(aCommandBuffer.Handle,TpvVulkanBreadcrumbType.Dispatch,'PickRegionComputePass');
  end;
  aCommandBuffer.CmdDispatch(CountGroupsX,CountGroupsY,1);
  if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
   fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.EndBreadcrumb(aCommandBuffer.Handle);
  end;

  // The bits the dispatch has set, for the copy below.
  FillChar(MemoryBarrier,SizeOf(TVkMemoryBarrier),#0);
  MemoryBarrier.sType:=VK_STRUCTURE_TYPE_MEMORY_BARRIER;
  MemoryBarrier.pNext:=nil;
  MemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
  MemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT);
  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                    0,
                                    1,@MemoryBarrier,
                                    0,nil,
                                    0,nil);

 end;

 // Copied out of device local memory rather than set there directly: atomics on host visible memory are
 // slow where they work at all, and only these few words have to cross over.
 BufferCopy.srcOffset:=0;
 BufferCopy.dstOffset:=0;
 BufferCopy.size:=CountWords*SizeOf(TpvUInt32);
 aCommandBuffer.CmdCopyBuffer(BitSetBuffer.Handle,ReadBackBuffer.Handle,1,@BufferCopy);

 // The host reads this buffer a full round of in-flight frames later, so the copy has to be visible to
 // host reads.
 FillChar(BufferMemoryBarrier,SizeOf(TVkBufferMemoryBarrier),#0);
 BufferMemoryBarrier.sType:=VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER;
 BufferMemoryBarrier.pNext:=nil;
 BufferMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT);
 BufferMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_HOST_READ_BIT);
 BufferMemoryBarrier.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 BufferMemoryBarrier.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 BufferMemoryBarrier.buffer:=ReadBackBuffer.Handle;
 BufferMemoryBarrier.offset:=0;
 BufferMemoryBarrier.size:=VK_WHOLE_SIZE;
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                   0,
                                   0,nil,
                                   1,@BufferMemoryBarrier,
                                   0,nil);

end;

end.
