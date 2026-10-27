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
unit PasVulkan.Scene3D.Renderer.Passes.TemporalUpscalerReactiveComputePass;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

// How much each pixel should follow the current frame over its history, as the temporal upscaler gets it,
// see temporal_upscaler_reactive.comp: from the transparency, which the transparency resolves leave in the
// alpha channel of the picture, and from the depth of field, where its resolve takes the blurred picture.
//
// The picture is the one from before the depth of field, which is handed in, since the depth of field
// writes its own alpha channel. The circle of confusion comes from the depth of field's prepare pass.

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

type { TpvScene3DRendererPassesTemporalUpscalerReactiveComputePass }
     TpvScene3DRendererPassesTemporalUpscalerReactiveComputePass=class(TpvFrameGraph.TComputePass)
      private
       type TPushConstants=packed record
             TransparencyReactiveness:TpvFloat;
             DepthOfFieldReactiveness:TpvFloat;
             Unused0:TpvFloat;
             Unused1:TpvFloat;
            end;
      private
       fInstance:TpvScene3DRendererInstance;
       fDepthOfField:boolean;
       fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceDepthOfField:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceOutput:TpvFrameGraph.TPass.TUsedImageResource;
       fShaderModule:TpvVulkanShaderModule;
       fShaderStage:TpvVulkanPipelineShaderStage;
       fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fPipelineLayout:TpvVulkanPipelineLayout;
       fPipeline:TpvVulkanComputePipeline;
       fDescriptorPool:TpvVulkanDescriptorPool;
       fColorImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fDepthOfFieldImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fOutputImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
       function CreateArrayImageView(const aResource:TpvFrameGraph.TPass.TUsedImageResource;const aInFlightFrameIndex:TpvSizeInt):TpvVulkanImageView;
      public
       constructor Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance;const aColorResource:TpvFrameGraph.TPass.TUsedImageResource;const aDepthOfField:boolean); reintroduce;
       destructor Destroy; override;
       procedure AcquirePersistentResources; override;
       procedure ReleasePersistentResources; override;
       procedure AcquireVolatileResources; override;
       procedure ReleaseVolatileResources; override;
       procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
       procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
     end;

implementation

{ TpvScene3DRendererPassesTemporalUpscalerReactiveComputePass }

constructor TpvScene3DRendererPassesTemporalUpscalerReactiveComputePass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance;const aColorResource:TpvFrameGraph.TPass.TUsedImageResource;const aDepthOfField:boolean);
begin

 inherited Create(aFrameGraph);

 fInstance:=aInstance;

 fDepthOfField:=aDepthOfField;

 Name:='TemporalUpscalerReactiveComputePass';

 fResourceColor:=AddImageInput(aColorResource.ResourceType.Name,
                               aColorResource.Resource.Name,
                               VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                               []
                              );

 if fDepthOfField then begin
  fResourceDepthOfField:=AddImageInput('resourcetype_color',
                                       'resource_depthoffield_prepare_data',
                                       VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                       []
                                      );
 end else begin
  fResourceDepthOfField:=nil;
 end;

 fResourceOutput:=AddImageOutput('resourcetype_temporal_upscaler_reactive',
                                 'resource_temporal_upscaler_reactive',
                                 VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare),
                                 []
                                );

end;

destructor TpvScene3DRendererPassesTemporalUpscalerReactiveComputePass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesTemporalUpscalerReactiveComputePass.AcquirePersistentResources;
var Stream:TStream;
begin

 inherited AcquirePersistentResources;

 if fDepthOfField then begin
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('temporal_upscaler_reactive_depthoffield_comp.spv');
 end else begin
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('temporal_upscaler_reactive_comp.spv');
 end;
 try
  fShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;
 fInstance.Renderer.VulkanDevice.DebugUtils.SetObjectName(fShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TemporalUpscalerReactive.Shader');

 fShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fShaderModule,'main');

end;

procedure TpvScene3DRendererPassesTemporalUpscalerReactiveComputePass.ReleasePersistentResources;
begin
 FreeAndNil(fShaderStage);
 FreeAndNil(fShaderModule);
 inherited ReleasePersistentResources;
end;

function TpvScene3DRendererPassesTemporalUpscalerReactiveComputePass.CreateArrayImageView(const aResource:TpvFrameGraph.TPass.TUsedImageResource;const aInFlightFrameIndex:TpvSizeInt):TpvVulkanImageView;
begin
 result:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                   aResource.VulkanImages[aInFlightFrameIndex],
                                   VK_IMAGE_VIEW_TYPE_2D_ARRAY,
                                   TpvFrameGraph.TImageResourceType(aResource.ResourceType).Format,
                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                   TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                   0,
                                   1,
                                   0,
                                   Max(1,fInstance.CountSurfaceViews));
end;

procedure TpvScene3DRendererPassesTemporalUpscalerReactiveComputePass.AcquireVolatileResources;
var InFlightFrameIndex,OutputBinding:TpvSizeInt;
begin

 inherited AcquireVolatileResources;

 if fDepthOfField then begin
  OutputBinding:=2;
 end else begin
  OutputBinding:=1;
 end;

 fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fInstance.Renderer.VulkanDevice);
 fDescriptorSetLayout.AddBinding(0,
                                 VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                 1,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 []);
 if fDepthOfField then begin
  fDescriptorSetLayout.AddBinding(1,
                                  VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  []);
 end;
 fDescriptorSetLayout.AddBinding(OutputBinding,
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
 fInstance.Renderer.VulkanDevice.DebugUtils.SetObjectName(fPipeline.Handle,VK_OBJECT_TYPE_PIPELINE,'TemporalUpscalerReactive.Pipeline');

 fDescriptorPool:=TpvVulkanDescriptorPool.Create(fInstance.Renderer.VulkanDevice,
                                                 TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                 FrameGraph.CountInFlightFrames);
 fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,FrameGraph.CountInFlightFrames*2);
 fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,FrameGraph.CountInFlightFrames);
 fDescriptorPool.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin

  fColorImageViews[InFlightFrameIndex]:=CreateArrayImageView(fResourceColor,InFlightFrameIndex);

  if fDepthOfField then begin
   fDepthOfFieldImageViews[InFlightFrameIndex]:=CreateArrayImageView(fResourceDepthOfField,InFlightFrameIndex);
  end else begin
   fDepthOfFieldImageViews[InFlightFrameIndex]:=nil;
  end;

  fOutputImageViews[InFlightFrameIndex]:=CreateArrayImageView(fResourceOutput,InFlightFrameIndex);

  fDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                           0,
                                                           1,
                                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                           [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                          fColorImageViews[InFlightFrameIndex].Handle,
                                                                                          VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                           [],
                                                           [],
                                                           false);
  if fDepthOfField then begin
   fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                            0,
                                                            1,
                                                            TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                            [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                           fDepthOfFieldImageViews[InFlightFrameIndex].Handle,
                                                                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                            [],
                                                            [],
                                                            false);
  end;
  fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(OutputBinding,
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

procedure TpvScene3DRendererPassesTemporalUpscalerReactiveComputePass.ReleaseVolatileResources;
var InFlightFrameIndex:TpvSizeInt;
begin
 for InFlightFrameIndex:=0 to fInstance.Renderer.CountInFlightFrames-1 do begin
  FreeAndNil(fDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fOutputImageViews[InFlightFrameIndex]);
  FreeAndNil(fDepthOfFieldImageViews[InFlightFrameIndex]);
  FreeAndNil(fColorImageViews[InFlightFrameIndex]);
 end;
 FreeAndNil(fDescriptorPool);
 FreeAndNil(fPipeline);
 FreeAndNil(fPipelineLayout);
 FreeAndNil(fDescriptorSetLayout);
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesTemporalUpscalerReactiveComputePass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvScene3DRendererPassesTemporalUpscalerReactiveComputePass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var CountViews:TpvInt32;
    PushConstants:TPushConstants;
    ImageMemoryBarrier:TVkImageMemoryBarrier;
begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 CountViews:=Max(1,fInstance.CountSurfaceViews);

 FillChar(ImageMemoryBarrier,SizeOf(TVkImageMemoryBarrier),#0);
 ImageMemoryBarrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
 ImageMemoryBarrier.srcAccessMask:=0;
 ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
 ImageMemoryBarrier.oldLayout:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
 ImageMemoryBarrier.newLayout:=VK_IMAGE_LAYOUT_GENERAL;
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

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                      fPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fDescriptorSets[aInFlightFrameIndex].Handle,
                                      0,
                                      nil);

 PushConstants.TransparencyReactiveness:=Min(Max(fInstance.Renderer.TemporalUpscalerTransparencyReactiveness,0.0),1.0);
 PushConstants.DepthOfFieldReactiveness:=Min(Max(fInstance.Renderer.TemporalUpscalerDepthOfFieldReactiveness,0.0),1.0);
 PushConstants.Unused0:=0.0;
 PushConstants.Unused1:=0.0;

 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 0,
                                 SizeOf(TPushConstants),
                                 @PushConstants);

 if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
  fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.BeginBreadcrumb(aCommandBuffer.Handle,TpvVulkanBreadcrumbType.Dispatch,'TemporalUpscalerReactive');
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
