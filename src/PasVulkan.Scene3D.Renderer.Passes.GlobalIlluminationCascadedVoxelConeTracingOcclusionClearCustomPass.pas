(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2020, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.Scene3D.Renderer.Passes.GlobalIlluminationCascadedVoxelConeTracingOcclusionClearCustomPass;
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
     PasVulkan.Scene3D.Renderer.SkyBox;

type { TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingOcclusionClearCustomPass }
     TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingOcclusionClearCustomPass=class(TpvFrameGraph.TCustomPass)
      private
       fInstance:TpvScene3DRendererInstance;
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

{ TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingOcclusionClearCustomPass }

constructor TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingOcclusionClearCustomPass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin
 inherited Create(aFrameGraph);
 fInstance:=aInstance;
 Name:='GlobalIlluminationCascadedVoxelConeTracingOcclusionClearCustomPass';
end;

destructor TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingOcclusionClearCustomPass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingOcclusionClearCustomPass.AcquirePersistentResources;
begin
 inherited AcquirePersistentResources;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingOcclusionClearCustomPass.ReleasePersistentResources;
begin
 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingOcclusionClearCustomPass.AcquireVolatileResources;
begin
 inherited AcquireVolatileResources;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingOcclusionClearCustomPass.ReleaseVolatileResources;
begin
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingOcclusionClearCustomPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvScene3DRendererPassesGlobalIlluminationCascadedVoxelConeTracingOcclusionClearCustomPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var PreviousInFlightFrameIndex,Index,CascadeIndex,ImageIndex,ImageCount:TpvInt32;
    ImageMemoryBarriers:array[0..(4*(6+1+1))-1] of TVkImageMemoryBarrier;
    BufferMemoryBarriers:array[0..1] of TVkBufferMemoryBarrier;
    PreviousAccessFlags:TVkAccessFlags;
    PreviousImageLayout:TVkImageLayout;
    ClearColor:TVkClearColorValue;
    ImageSubresourceRange:TVkImageSubresourceRange;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 PreviousInFlightFrameIndex:=FrameGraph.DrawPreviousInFlightFrameIndex;

{if fInstance.fGlobalIlluminationCascadedVoxelConeTracingFirst[PreviousInFlightFrameIndex] then begin
  PreviousAccessFlags:=0;
  PreviousImageLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
 end else begin
  PreviousAccessFlags:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT);
  PreviousImageLayout:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
 end;}

 PreviousAccessFlags:=0;
 PreviousImageLayout:=VK_IMAGE_LAYOUT_UNDEFINED;

 ImageCount:=0;
 for CascadeIndex:=0 to fInstance.Renderer.GlobalIlluminationVoxelCountClipMaps-1 do begin
 {if assigned(fInstance.GlobalIlluminationCascadedVoxelConeTracingAtomicImages[CascadeIndex]) then begin
   ImageMemoryBarriers[ImageCount]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                                 TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                 VK_IMAGE_LAYOUT_GENERAL,
                                                                 VK_IMAGE_LAYOUT_GENERAL,
                                                                 VK_QUEUE_FAMILY_IGNORED,
                                                                 VK_QUEUE_FAMILY_IGNORED,
                                                                 fInstance.GlobalIlluminationCascadedVoxelConeTracingAtomicImages[CascadeIndex].VulkanImage.Handle,
                                                                 TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                                 0,
                                                                                                 1,
                                                                                                 0,
                                                                                                 1));
   inc(ImageCount);
  end;//}
  ImageMemoryBarriers[ImageCount]:=TVkImageMemoryBarrier.Create(PreviousAccessFlags,
                                                                TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                PreviousImageLayout,
                                                                VK_IMAGE_LAYOUT_GENERAL,
                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                fInstance.GlobalIlluminationCascadedVoxelConeTracingOcclusionImages[CascadeIndex].VulkanImage.Handle,
                                                                TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                                0,
                                                                                                fInstance.GlobalIlluminationCascadedVoxelConeTracingOcclusionImages[CascadeIndex].MipMapLevels,
                                                                                                0,
                                                                                                1));
  inc(ImageCount);
  for ImageIndex:=0 to 5 do begin
   ImageMemoryBarriers[ImageCount]:=TVkImageMemoryBarrier.Create(PreviousAccessFlags,
                                                                 TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                 PreviousImageLayout,
                                                                 VK_IMAGE_LAYOUT_GENERAL,
                                                                 VK_QUEUE_FAMILY_IGNORED,
                                                                 VK_QUEUE_FAMILY_IGNORED,
                                                                 fInstance.GlobalIlluminationCascadedVoxelConeTracingImages[CascadeIndex,ImageIndex].VulkanImage.Handle,
                                                                 TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                                 0,
                                                                                                 fInstance.GlobalIlluminationCascadedVoxelConeTracingImages[CascadeIndex,ImageIndex].MipMapLevels,
                                                                                                 0,
                                                                                                 1));
   inc(ImageCount);
  end;
 end;

{if fInstance.fGlobalIlluminationCascadedVoxelConeTracingFirst[PreviousInFlightFrameIndex] then}begin

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(0,
                                                         TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.GlobalIlluminationCascadedVoxelConeTracingColorBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(0,
                                                         TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.GlobalIlluminationCascadedVoxelConeTracingCounterBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

{end else begin

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.GlobalIlluminationCascadedVoxelConeTracingColorBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.GlobalIlluminationCascadedVoxelConeTracingCounterBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);    //}

 end;

 if (aInFlightFrameIndex<>PreviousInFlightFrameIndex) and fInstance.fGlobalIlluminationCascadedVoxelConeTracingEventReady[PreviousInFlightFrameIndex] then begin
  fInstance.fGlobalIlluminationCascadedVoxelConeTracingEventReady[PreviousInFlightFrameIndex]:=false;
  aCommandBuffer.CmdWaitEvents(1,
                               @fInstance.fGlobalIlluminationCascadedVoxelConeTracingEvents[PreviousInFlightFrameIndex].Handle,
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT){
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT) or
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) or
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) or
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT) or
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT)},
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT){
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT) or
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) or
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) or
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT) or
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT)},
                               0,nil,
                               length(BufferMemoryBarriers),@BufferMemoryBarriers[0],
                               ImageCount,@ImageMemoryBarriers[0]);
  aCommandBuffer.CmdResetEvent(fInstance.fGlobalIlluminationCascadedVoxelConeTracingEvents[PreviousInFlightFrameIndex].Handle,
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT){
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT) or
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) or
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) or
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT) or
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT)});
 end else begin
  aCommandBuffer.CmdPipelineBarrier(FrameGraph.VulkanDevice.PhysicalDevice.PipelineStageAllShaderBits or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                    FrameGraph.VulkanDevice.PhysicalDevice.PipelineStageAllShaderBits or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                    0,
                                    0,nil,
                                    length(BufferMemoryBarriers),@BufferMemoryBarriers[0],
                                    ImageCount,@ImageMemoryBarriers[0]);
 end;

 //if fInstance.fGlobalIlluminationCascadedVoxelConeTracingFirst[PreviousInFlightFrameIndex] then
 begin

  aCommandBuffer.CmdFillBuffer(fInstance.GlobalIlluminationCascadedVoxelConeTracingColorBuffer.Handle,0,VK_WHOLE_SIZE,0);

  aCommandBuffer.CmdFillBuffer(fInstance.GlobalIlluminationCascadedVoxelConeTracingCounterBuffer.Handle,0,VK_WHOLE_SIZE,0);

{ ClearColor.int32[0]:=0;
  ClearColor.int32[1]:=0;
  ClearColor.int32[2]:=0;
  ClearColor.int32[3]:=0;

  ImageSubresourceRange:=TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                         0,
                                                         1,
                                                         0,
                                                         1);

  for CascadeIndex:=0 to fInstance.Renderer.GlobalIlluminationVoxelCountClipMaps-1 do begin
   if assigned(fInstance.GlobalIlluminationCascadedVoxelConeTracingAtomicImages[CascadeIndex]) then begin
    aCommandBuffer.CmdClearColorImage(fInstance.GlobalIlluminationCascadedVoxelConeTracingAtomicImages[CascadeIndex].VulkanImage.Handle,
                                      VK_IMAGE_LAYOUT_GENERAL,
                                      @ClearColor,
                                      1,
                                      @ImageSubresourceRange);
   end;
  end;}

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.GlobalIlluminationCascadedVoxelConeTracingColorBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fInstance.GlobalIlluminationCascadedVoxelConeTracingCounterBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

{ ImageCount:=0;
  for CascadeIndex:=0 to fInstance.Renderer.GlobalIlluminationVoxelCountClipMaps-1 do begin
   ImageMemoryBarriers[ImageCount]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                                 TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                 VK_IMAGE_LAYOUT_GENERAL,
                                                                 VK_IMAGE_LAYOUT_GENERAL,
                                                                 VK_QUEUE_FAMILY_IGNORED,
                                                                 VK_QUEUE_FAMILY_IGNORED,
                                                                 fInstance.GlobalIlluminationCascadedVoxelConeTracingAtomicImages[CascadeIndex].VulkanImage.Handle,
                                                                 TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                                 0,
                                                                                                 1,
                                                                                                 0,
                                                                                                 1));
   inc(ImageCount);
  end;//}

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                    FrameGraph.VulkanDevice.PhysicalDevice.PipelineStageAllShaderBits,
                                    0,
                                    0,nil,
                                    length(BufferMemoryBarriers),@BufferMemoryBarriers[0],
                                    0,nil);//ImageCount,@ImageMemoryBarriers[0]);

 end;

 fInstance.fGlobalIlluminationCascadedVoxelConeTracingFirst[PreviousInFlightFrameIndex]:=false;

end;

end.
