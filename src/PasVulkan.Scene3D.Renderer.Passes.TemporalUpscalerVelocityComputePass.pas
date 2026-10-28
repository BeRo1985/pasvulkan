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
unit PasVulkan.Scene3D.Renderer.Passes.TemporalUpscalerVelocityComputePass;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

// The motion vectors the temporal upscaler works from: a copy of the velocity buffer in which every pixel
// without anything drawn into it gets the motion of the camera, see temporal_upscaler_velocity.comp. The
// sky, the atmosphere and the clouds write no velocity, and with none a temporal upscaler takes them for
// standing still on the screen and smears them whenever the camera turns.
//
// It writes a resource of its own instead of changing the velocity buffer, so that nothing else that reads
// the velocity buffer sees any difference.

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

type { TpvScene3DRendererPassesTemporalUpscalerVelocityComputePass }
     TpvScene3DRendererPassesTemporalUpscalerVelocityComputePass=class(TpvFrameGraph.TComputePass)
      private
       type TPushConstants=packed record
             ViewBaseIndex:TpvUInt32;
             CountAllViews:TpvUInt32;
             FarDepth:TpvFloat;
             Unused:TpvFloat;
             Jitter:TpvVector4;
            end;
      private
       fInstance:TpvScene3DRendererInstance;
       fResourceVelocity:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceOutput:TpvFrameGraph.TPass.TUsedImageResource;
       fShaderModule:TpvVulkanShaderModule;
       fShaderStage:TpvVulkanPipelineShaderStage;
       fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fPipelineLayout:TpvVulkanPipelineLayout;
       fPipeline:TpvVulkanComputePipeline;
       fDescriptorPool:TpvVulkanDescriptorPool;
       fVelocityImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fOutputImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
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

{ TpvScene3DRendererPassesTemporalUpscalerVelocityComputePass }

constructor TpvScene3DRendererPassesTemporalUpscalerVelocityComputePass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin

 inherited Create(aFrameGraph);

 fInstance:=aInstance;

 Name:='TemporalUpscalerVelocityComputePass';

 fResourceVelocity:=AddImageInput('resourcetype_velocity',
                                  'resource_velocity_data',
                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                  []
                                 );

 // The depth is not taken from the graph: it comes from FinalDepthArray2DImage, which the depth resolve
 // leaves behind, and which the instance makes this pass wait for.

 fResourceOutput:=AddImageOutput('resourcetype_temporal_upscaler_velocity',
                                 'resource_temporal_upscaler_velocity',
                                 VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare),
                                 []
                                );

end;

destructor TpvScene3DRendererPassesTemporalUpscalerVelocityComputePass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesTemporalUpscalerVelocityComputePass.AcquirePersistentResources;
var Stream:TStream;
begin

 inherited AcquirePersistentResources;

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('temporal_upscaler_velocity_comp.spv');
 try
  fShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;
 fInstance.Renderer.VulkanDevice.DebugUtils.SetObjectName(fShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TemporalUpscalerVelocity.Shader');

 fShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fShaderModule,'main');

end;

procedure TpvScene3DRendererPassesTemporalUpscalerVelocityComputePass.ReleasePersistentResources;
begin
 FreeAndNil(fShaderStage);
 FreeAndNil(fShaderModule);
 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesTemporalUpscalerVelocityComputePass.AcquireVolatileResources;
var InFlightFrameIndex:TpvSizeInt;
    CountViews:TpvInt32;
begin

 inherited AcquireVolatileResources;

 CountViews:=Max(1,fInstance.CountSurfaceViews);

 fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fInstance.Renderer.VulkanDevice);
 fDescriptorSetLayout.AddBinding(0,
                                 VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                 1,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 []);
 fDescriptorSetLayout.AddBinding(1,
                                 VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                 1,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 []);
 fDescriptorSetLayout.AddBinding(2,
                                 VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                 1,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 []);
 fDescriptorSetLayout.AddBinding(3,
                                 VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                 1,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 []);
 fDescriptorSetLayout.Initialize;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(fInstance.Renderer.VulkanDevice);
 fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
 fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
 fPipelineLayout.Initialize;

 fPipeline:=TpvVulkanComputePipeline.Create(fInstance.Renderer.VulkanDevice,
                                            fInstance.Renderer.VulkanPipelineCache,
                                            0,
                                            fShaderStage,
                                            fPipelineLayout,
                                            nil,
                                            0);
 fInstance.Renderer.VulkanDevice.DebugUtils.SetObjectName(fPipeline.Handle,VK_OBJECT_TYPE_PIPELINE,'TemporalUpscalerVelocity.Pipeline');

 fDescriptorPool:=TpvVulkanDescriptorPool.Create(fInstance.Renderer.VulkanDevice,
                                                 TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                 FrameGraph.CountInFlightFrames);
 fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,FrameGraph.CountInFlightFrames);
 fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,FrameGraph.CountInFlightFrames*2);
 fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,FrameGraph.CountInFlightFrames);
 fDescriptorPool.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin

  fVelocityImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                                                     fResourceVelocity.VulkanImages[InFlightFrameIndex],
                                                                     VK_IMAGE_VIEW_TYPE_2D_ARRAY,
                                                                     TpvFrameGraph.TImageResourceType(fResourceVelocity.ResourceType).Format,
                                                                     VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                     VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                     VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                     VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                     TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                     0,
                                                                     1,
                                                                     0,
                                                                     CountViews);

  fOutputImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                                                   fResourceOutput.VulkanImages[InFlightFrameIndex],
                                                                   VK_IMAGE_VIEW_TYPE_2D_ARRAY,
                                                                   TpvFrameGraph.TImageResourceType(fResourceOutput.ResourceType).Format,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                   0,
                                                                   1,
                                                                   0,
                                                                   CountViews);

  fDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                           0,
                                                           1,
                                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                           [],
                                                           [fInstance.VulkanViewUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                           [],
                                                           false);
  fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                           0,
                                                           1,
                                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                           [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                          fVelocityImageViews[InFlightFrameIndex].Handle,
                                                                                          VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                           [],
                                                           [],
                                                           false);
  fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                           0,
                                                           1,
                                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                           [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                          fInstance.FinalDepthArray2DImage.VulkanArrayImageView.Handle,
                                                                                          VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                           [],
                                                           [],
                                                           false);
  fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                           0,
                                                           1,
                                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                           [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                          fOutputImageViews[InFlightFrameIndex].Handle,
                                                                                          VK_IMAGE_LAYOUT_GENERAL)],
                                                           [],
                                                           [],
                                                           false);
  fDescriptorSets[InFlightFrameIndex].Flush;

 end;

end;

procedure TpvScene3DRendererPassesTemporalUpscalerVelocityComputePass.ReleaseVolatileResources;
var InFlightFrameIndex:TpvSizeInt;
begin
 for InFlightFrameIndex:=0 to fInstance.Renderer.CountInFlightFrames-1 do begin
  FreeAndNil(fDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fOutputImageViews[InFlightFrameIndex]);
  FreeAndNil(fVelocityImageViews[InFlightFrameIndex]);
 end;
 FreeAndNil(fDescriptorPool);
 FreeAndNil(fPipeline);
 FreeAndNil(fPipelineLayout);
 FreeAndNil(fDescriptorSetLayout);
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesTemporalUpscalerVelocityComputePass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvScene3DRendererPassesTemporalUpscalerVelocityComputePass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var CountViews:TpvInt32;
    InFlightFrameState:TpvScene3DRendererInstance.PInFlightFrameState;
    PushConstants:TPushConstants;
    ImageMemoryBarrier:TVkImageMemoryBarrier;
begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 CountViews:=Max(1,fInstance.CountSurfaceViews);

 InFlightFrameState:=@fInstance.InFlightFrameStates^[aInFlightFrameIndex];

 // The output is written in full, so whatever it held and whichever layout the last pass of the frame
 // before left it in, as the capture of frame generation does, does not matter.
 FillChar(ImageMemoryBarrier,SizeOf(TVkImageMemoryBarrier),#0);
 ImageMemoryBarrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
 ImageMemoryBarrier.srcAccessMask:=0;
 ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
 ImageMemoryBarrier.oldLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
 ImageMemoryBarrier.newLayout:=VK_IMAGE_LAYOUT_GENERAL;
 ImageMemoryBarrier.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 ImageMemoryBarrier.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 ImageMemoryBarrier.image:=fResourceOutput.VulkanImages[aInFlightFrameIndex].Handle;
 ImageMemoryBarrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 ImageMemoryBarrier.subresourceRange.baseMipLevel:=0;
 ImageMemoryBarrier.subresourceRange.levelCount:=1;
 ImageMemoryBarrier.subresourceRange.baseArrayLayer:=0;
 ImageMemoryBarrier.subresourceRange.layerCount:=CountViews;
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@ImageMemoryBarrier);

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                      fPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fDescriptorSets[aInFlightFrameIndex].Handle,
                                      0,
                                      nil);

 PushConstants.ViewBaseIndex:=InFlightFrameState^.FinalViewIndex;
 PushConstants.CountAllViews:=fInstance.Views[aInFlightFrameIndex].Count;
 if fInstance.ZFar<0.0 then begin
  PushConstants.FarDepth:=0.0;
 end else begin
  PushConstants.FarDepth:=1.0;
 end;
 PushConstants.Unused:=0.0;
 PushConstants.Jitter:=InFlightFrameState^.Jitter;

 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 0,
                                 SizeOf(TPushConstants),
                                 @PushConstants);

 if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
  fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.BeginBreadcrumb(aCommandBuffer.Handle,TpvVulkanBreadcrumbType.Dispatch,'TemporalUpscalerVelocity');
 end;
 aCommandBuffer.CmdDispatch(Max(1,(fInstance.ScaledWidth+7) shr 3),
                            Max(1,(fInstance.ScaledHeight+7) shr 3),
                            CountViews);
 if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
  fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.EndBreadcrumb(aCommandBuffer.Handle);
 end;

 FillChar(ImageMemoryBarrier,SizeOf(TVkImageMemoryBarrier),#0);
 ImageMemoryBarrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
 ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
 ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT);
 ImageMemoryBarrier.oldLayout:=VK_IMAGE_LAYOUT_GENERAL;
 ImageMemoryBarrier.newLayout:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
 ImageMemoryBarrier.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 ImageMemoryBarrier.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 ImageMemoryBarrier.image:=fResourceOutput.VulkanImages[aInFlightFrameIndex].Handle;
 ImageMemoryBarrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 ImageMemoryBarrier.subresourceRange.baseMipLevel:=0;
 ImageMemoryBarrier.subresourceRange.levelCount:=1;
 ImageMemoryBarrier.subresourceRange.baseArrayLayer:=0;
 ImageMemoryBarrier.subresourceRange.layerCount:=CountViews;
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@ImageMemoryBarrier);

end;

end.
