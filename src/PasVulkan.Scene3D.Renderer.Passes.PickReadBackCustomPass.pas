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

end;

destructor TpvScene3DRendererPassesPickReadBackCustomPass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesPickReadBackCustomPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var BufferMemoryBarrier:TVkBufferMemoryBarrier;
    BufferImageCopy:TVkBufferImageCopy;
    Buffer:TpvVulkanBuffer;
    X,Y:TpvInt32;
begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 Buffer:=fInstance.PickReadBackBuffers[aInFlightFrameIndex];
 if not assigned(Buffer) then begin
  exit;
 end;

 // Clamped rather than skipped when out of range: a copy that always happens keeps the buffer's
 // content defined, so the reader never has to tell "no copy this frame" from "nothing was hit".
 X:=Min(Max(fInstance.PickPositionX[aInFlightFrameIndex],0),Max(fResourceID.Width-1,0));
 Y:=Min(Max(fInstance.PickPositionY[aInFlightFrameIndex],0),Max(fResourceID.Height-1,0));

 FillChar(BufferImageCopy,SizeOf(TVkBufferImageCopy),#0);
 BufferImageCopy.bufferOffset:=0;
 BufferImageCopy.bufferRowLength:=0;
 BufferImageCopy.bufferImageHeight:=0;
 BufferImageCopy.imageSubresource.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 BufferImageCopy.imageSubresource.mipLevel:=0;
 BufferImageCopy.imageSubresource.baseArrayLayer:=0;
 BufferImageCopy.imageSubresource.layerCount:=1; // one view is enough, picking happens through the main eye
 BufferImageCopy.imageOffset.x:=X;
 BufferImageCopy.imageOffset.y:=Y;
 BufferImageCopy.imageOffset.z:=0;
 BufferImageCopy.imageExtent.width:=1;
 BufferImageCopy.imageExtent.height:=1;
 BufferImageCopy.imageExtent.depth:=1;

 aCommandBuffer.CmdCopyImageToBuffer(fResourceID.VulkanImages[aInFlightFrameIndex].Handle,
                                     VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                     Buffer.Handle,
                                     1,
                                     @BufferImageCopy);

 // The host reads this buffer a frame later, so the write has to be visible to host reads.
 FillChar(BufferMemoryBarrier,SizeOf(TVkBufferMemoryBarrier),#0);
 BufferMemoryBarrier.sType:=VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER;
 BufferMemoryBarrier.pNext:=nil;
 BufferMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT);
 BufferMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_HOST_READ_BIT);
 BufferMemoryBarrier.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 BufferMemoryBarrier.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 BufferMemoryBarrier.buffer:=Buffer.Handle;
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
