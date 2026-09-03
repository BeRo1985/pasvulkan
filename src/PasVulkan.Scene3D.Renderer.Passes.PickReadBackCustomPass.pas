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
 ******************************************************************************)
unit PasVulkan.Scene3D.Renderer.Passes.PickReadBackCustomPass;
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

type { TpvScene3DRendererPassesPickReadBackCustomPass }
     // Object picking: copies the single pixel under the cursor out of the id target into a host
     // visible buffer, one per in-flight frame. Reading it is what the renderer instance does a frame
     // later (TpvScene3DRendererInstance.PickResult) - the copy of frame N is waited for by the fence
     // that frame N's slot is reused behind anyway, so nothing here ever blocks on the running frame.
     //
     // Consuming the id target is also what keeps the pick pass in the compiled graph at all: the
     // frame graph walks from the root pass and drops whatever nobody reads.
     TpvScene3DRendererPassesPickReadBackCustomPass=class(TpvFrameGraph.TCustomPass)
      private
       fInstance:TpvScene3DRendererInstance;
       fResourceID:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceDepth:TpvFrameGraph.TPass.TUsedImageResource;
      public
       constructor Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance); reintroduce;
       destructor Destroy; override;
       procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
     end;

implementation

{ TpvScene3DRendererPassesPickReadBackCustomPass }

constructor TpvScene3DRendererPassesPickReadBackCustomPass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin

 inherited Create(aFrameGraph);

 fInstance:=aInstance;

 Name:='PickReadBackCustomPass';

 Queue:=aFrameGraph.UniversalQueue;

 // Asking for TRANSFER_SRC_OPTIMAL here is what makes the frame graph transition the id target for
 // us, the same way the TAA post pass gets its inputs ready for a blit.
 fResourceID:=AddImageInput('resourcetype_pick_id',
                            'resource_pick_id',
                            VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                            []
                           );

 // The depth of the same pixel, unprojected the point on the visible surface under the cursor, which
 // is what the collision shapes cannot say when one of them does not cover its model.
 //
 // Deliberately the pick pass's OWN depth attachment and not the renderer's depth buffer, for two
 // reasons that both make the latter unusable: with MSAA on it is a multisampled image, which
 // vkCmdCopyImageToBuffer cannot read at all, and transparent surfaces never reach it, so a pane of
 // glass would pick whatever stands behind it. The pick pass draws for itself, so it also decides
 // what counts as visible here.
 fResourceDepth:=AddImageInput('resourcetype_pick_id_depth',
                               'resource_pick_id_depth',
                               VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                               []
                              );

end;

destructor TpvScene3DRendererPassesPickReadBackCustomPass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesPickReadBackCustomPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var BufferMemoryBarriers:array[0..1] of TVkBufferMemoryBarrier;
    BufferImageCopy:TVkBufferImageCopy;
    IDBuffer,DepthBuffer:TpvVulkanBuffer;
    X,Y,CountBarriers:TpvInt32;

 procedure CopyPixel(const aResource:TpvFrameGraph.TPass.TUsedImageResource;const aAspectMask:TVkImageAspectFlags;const aBuffer:TpvVulkanBuffer);
 begin
  // Clamped rather than skipped when out of range: a copy that always happens keeps the buffer's
  // content defined, so the reader never has to tell "no copy this frame" from "nothing was hit".
  X:=Min(Max(fInstance.PickPositionX[aInFlightFrameIndex],0),Max(aResource.Width-1,0));
  Y:=Min(Max(fInstance.PickPositionY[aInFlightFrameIndex],0),Max(aResource.Height-1,0));
  FillChar(BufferImageCopy,SizeOf(TVkBufferImageCopy),#0);
  BufferImageCopy.bufferOffset:=0;
  BufferImageCopy.bufferRowLength:=0;
  BufferImageCopy.bufferImageHeight:=0;
  BufferImageCopy.imageSubresource.aspectMask:=aAspectMask;
  BufferImageCopy.imageSubresource.mipLevel:=0;
  BufferImageCopy.imageSubresource.baseArrayLayer:=0;
  BufferImageCopy.imageSubresource.layerCount:=1; // one view is enough, picking happens through the main eye
  BufferImageCopy.imageOffset.x:=X;
  BufferImageCopy.imageOffset.y:=Y;
  BufferImageCopy.imageOffset.z:=0;
  BufferImageCopy.imageExtent.width:=1;
  BufferImageCopy.imageExtent.height:=1;
  BufferImageCopy.imageExtent.depth:=1;
  aCommandBuffer.CmdCopyImageToBuffer(aResource.VulkanImages[aInFlightFrameIndex].Handle,
                                      VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                      aBuffer.Handle,
                                      1,
                                      @BufferImageCopy);
 end;

 procedure AddHostReadBarrier(const aBuffer:TpvVulkanBuffer);
 begin
  // The host reads this buffer a frame later, so the write has to be visible to host reads.
  FillChar(BufferMemoryBarriers[CountBarriers],SizeOf(TVkBufferMemoryBarrier),#0);
  BufferMemoryBarriers[CountBarriers].sType:=VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER;
  BufferMemoryBarriers[CountBarriers].pNext:=nil;
  BufferMemoryBarriers[CountBarriers].srcAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT);
  BufferMemoryBarriers[CountBarriers].dstAccessMask:=TVkAccessFlags(VK_ACCESS_HOST_READ_BIT);
  BufferMemoryBarriers[CountBarriers].srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  BufferMemoryBarriers[CountBarriers].dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  BufferMemoryBarriers[CountBarriers].buffer:=aBuffer.Handle;
  BufferMemoryBarriers[CountBarriers].offset:=0;
  BufferMemoryBarriers[CountBarriers].size:=VK_WHOLE_SIZE;
  inc(CountBarriers);
 end;

begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 IDBuffer:=fInstance.PickReadBackBuffers[aInFlightFrameIndex];
 DepthBuffer:=fInstance.PickDepthReadBackBuffers[aInFlightFrameIndex];

 CountBarriers:=0;

 if assigned(IDBuffer) then begin
  CopyPixel(fResourceID,TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),IDBuffer);
  AddHostReadBarrier(IDBuffer);
 end;

 if assigned(DepthBuffer) then begin
  CopyPixel(fResourceDepth,TVkImageAspectFlags(VK_IMAGE_ASPECT_DEPTH_BIT),DepthBuffer);
  AddHostReadBarrier(DepthBuffer);
 end;

 if CountBarriers>0 then begin
  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                    0,
                                    0,nil,
                                    CountBarriers,@BufferMemoryBarriers[0],
                                    0,nil);
 end;

end;

end.
