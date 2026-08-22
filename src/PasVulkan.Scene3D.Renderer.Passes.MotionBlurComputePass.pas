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
unit PasVulkan.Scene3D.Renderer.Passes.MotionBlurComputePass;
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
     PasVulkan.Scene3D.Renderer.Array2DImage;

type { TpvScene3DRendererPassesMotionBlurComputePass }
     TpvScene3DRendererPassesMotionBlurComputePass=class(TpvFrameGraph.TComputePass)
      public
       type TPushConstants=packed record
             StrengthTileSizeSoftZDepthUnproject:TpvVector4;
             FrameSpare:TpvVector4;
            end;
      private
       fInstance:TpvScene3DRendererInstance;
       fResourceInput:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceVelocity:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceNeighbourMax:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceOutput:TpvFrameGraph.TPass.TUsedImageResource;
       fVelocityImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fNeighbourMaxImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fOutputImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fComputeShaderModule:TpvVulkanShaderModule;
       fVulkanImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fVulkanPipelineShaderStageCompute:TpvVulkanPipelineShaderStage;
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
       procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
       procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
     end;

implementation

{ TpvScene3DRendererPassesMotionBlurComputePass  }

constructor TpvScene3DRendererPassesMotionBlurComputePass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin
 inherited Create(aFrameGraph);

 fInstance:=aInstance;

 Name:='MotionBlurComputePass';

 begin

  begin

   fResourceInput:=AddImageInput(fInstance.LastOutputResource.ResourceType.Name,
                                 fInstance.LastOutputResource.Resource.Name,
                                 VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                 []
                                );

   fResourceVelocity:=AddImageInput('resourcetype_velocity',
                                    'resource_velocity_data',
                                    VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                    []
                                   );

   fResourceNeighbourMax:=AddImageInput('resourcetype_motionblur_tile',
                                        'resource_motionblur_neighbourmax',
                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                        []
                                       );

   fResourceOutput:=AddImageOutput('resourcetype_color_motionblur',
                                   'resource_motionblur_color',
                                   VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                   TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare),
                                   []
                                  );

   // The depth is not taken from the graph: it comes from FinalDepthArray2DImage, which the depth resolve
   // fills and which is one sample per pixel whether or not the surface is multisampled.

   fInstance.LastOutputResource:=fResourceOutput;

  end;

 end;

end;

destructor TpvScene3DRendererPassesMotionBlurComputePass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesMotionBlurComputePass.AcquirePersistentResources;
var Stream:TStream;
begin

 inherited AcquirePersistentResources;

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('motionblur_comp.spv');
 try
  fComputeShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageCompute:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

end;

procedure TpvScene3DRendererPassesMotionBlurComputePass.ReleasePersistentResources;
begin
 FreeAndNil(fVulkanPipelineShaderStageCompute);
 FreeAndNil(fComputeShaderModule);
 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesMotionBlurComputePass.AcquireVolatileResources;
var InFlightFrameIndex,CountViews,BindingIndex:TpvInt32;
    ImageViewType:TVkImageViewType;
begin

 inherited AcquireVolatileResources;

 CountViews:=fInstance.CountSurfaceViews;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(fInstance.Renderer.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fInstance.Renderer.CountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fInstance.Renderer.CountInFlightFrames*4);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,fInstance.Renderer.CountInFlightFrames);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fInstance.Renderer.VulkanDevice);
 for BindingIndex:=0 to 3 do begin
  fVulkanDescriptorSetLayout.AddBinding(BindingIndex,
                                        VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                        1,
                                        TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                        []);
 end;
 fVulkanDescriptorSetLayout.AddBinding(4,
                                       VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(fInstance.Renderer.VulkanDevice);
 fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TpvScene3DRendererPassesMotionBlurComputePass.TPushConstants));
 fPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fPipelineLayout.Initialize;

 fPipeline:=TpvVulkanComputePipeline.Create(fInstance.Renderer.VulkanDevice,
                                                  fInstance.Renderer.VulkanPipelineCache,
                                                  0,
                                                  fVulkanPipelineShaderStageCompute,
                                                  fPipelineLayout,
                                                  nil,
                                                  0);

 // Always an array view - see the note in the tile-max pass.
 ImageViewType:=TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                                                   fResourceInput.VulkanImages[InFlightFrameIndex],
                                                                   ImageViewType,
                                                                   TpvFrameGraph.TImageResourceType(fResourceInput.ResourceType).Format,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                   0,
                                                                   1,
                                                                   0,
                                                                   CountViews
                                                                  );
  fVelocityImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                                                     fResourceVelocity.VulkanImages[InFlightFrameIndex],
                                                                     TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY),
                                                                     TpvFrameGraph.TImageResourceType(fResourceVelocity.ResourceType).Format,
                                                                     VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                     VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                     VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                     VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                     TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                     0,
                                                                     1,
                                                                     0,
                                                                     CountViews
                                                                    );
  fNeighbourMaxImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                                                         fResourceNeighbourMax.VulkanImages[InFlightFrameIndex],
                                                                         TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY),
                                                                         TpvFrameGraph.TImageResourceType(fResourceNeighbourMax.ResourceType).Format,
                                                                         VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                         VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                         VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                         VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                         TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                         0,
                                                                         1,
                                                                         0,
                                                                         CountViews
                                                                        );
  fOutputImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                                                   fResourceOutput.VulkanImages[InFlightFrameIndex],
                                                                   TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY),
                                                                   TpvFrameGraph.TImageResourceType(fResourceOutput.ResourceType).Format,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                   0,
                                                                   1,
                                                                   0,
                                                                   CountViews
                                                                  );
  begin
   fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                            fVulkanDescriptorSetLayout);
   fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                  0,
                                                                  1,
                                                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                  [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                                 fVulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                 fResourceInput.ResourceTransition.Layout)],
                                                                  [],
                                                                  [],
                                                                  false
                                                                 );
   fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                  0,
                                                                  1,
                                                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                  [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                                 fVelocityImageViews[InFlightFrameIndex].Handle,
                                                                                                 fResourceVelocity.ResourceTransition.Layout)],
                                                                  [],
                                                                  [],
                                                                  false
                                                                 );
   // The depth, from the image the resolve pass leaves behind rather than from the graph. The ARRAY view,
   // not the plain one: the plain one is a 2D view whenever there is only one view of the surface, and the
   // shader declares a sampler2DArray whatever the count.
   fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                  0,
                                                                  1,
                                                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                  [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                                 fInstance.FinalDepthArray2DImage.VulkanArrayImageView.Handle,
                                                                                                 VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                  [],
                                                                  [],
                                                                  false
                                                                 );
   // The one input taken with a linear sampler rather than a nearest one: it holds one velocity per
   // 16x16 tile, and read tile by tile that quantisation is plainly visible as blocks in the picture -
   // a pixel is smeared or not depending on which tile it happens to fall in. Read smoothly between
   // tiles, the reach grows and shrinks across the picture instead of stepping.
   fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                  0,
                                                                  1,
                                                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                  [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedSampler.Handle,
                                                                                                 fNeighbourMaxImageViews[InFlightFrameIndex].Handle,
                                                                                                 fResourceNeighbourMax.ResourceTransition.Layout)],
                                                                  [],
                                                                  [],
                                                                  false
                                                                 );
   fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                                  0,
                                                                  1,
                                                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                  [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                                 fOutputImageViews[InFlightFrameIndex].Handle,
                                                                                                 VK_IMAGE_LAYOUT_GENERAL)],
                                                                  [],
                                                                  [],
                                                                  false
                                                                 );
   fVulkanDescriptorSets[InFlightFrameIndex].Flush;
  end;
 end;

end;

procedure TpvScene3DRendererPassesMotionBlurComputePass.ReleaseVolatileResources;
var InFlightFrameIndex:TpvInt32;
begin
 FreeAndNil(fPipeline);
 FreeAndNil(fPipelineLayout);
 for InFlightFrameIndex:=0 to fInstance.Renderer.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fVulkanImageViews[InFlightFrameIndex]);
  FreeAndNil(fVelocityImageViews[InFlightFrameIndex]);
  FreeAndNil(fNeighbourMaxImageViews[InFlightFrameIndex]);
  FreeAndNil(fOutputImageViews[InFlightFrameIndex]);
 end;
 FreeAndNil(fVulkanDescriptorSetLayout);
 FreeAndNil(fVulkanDescriptorPool);
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesMotionBlurComputePass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvScene3DRendererPassesMotionBlurComputePass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var InFlightFrameIndex,CountViews:TpvInt32;
    PushConstants:TpvScene3DRendererPassesMotionBlurComputePass.TPushConstants;
    ImageMemoryBarrier:TVkImageMemoryBarrier;
begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 InFlightFrameIndex:=aInFlightFrameIndex;

 CountViews:=fInstance.CountSurfaceViews;

 // The frame graph hands the output over in SHADER_READ_ONLY_OPTIMAL, which is the one layout a storage
 // image may NOT be written in. So it is taken to GENERAL here and given back afterwards, to whoever
 // carries the picture on from here.

 begin

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

 end;

 begin

  aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

  // The last of the four is what turns a raw depth value into a distance that grows away from the camera:
  // the projection is reversed-Z, so the raw depth does the opposite, and the filter's "which of the two
  // is in front" test needs it the right way round to mean anything. The frame counter carries the jitter.
  PushConstants.StrengthTileSizeSoftZDepthUnproject:=TpvVector4.InlineableCreate(fInstance.MotionBlurFactor,
                                                                                 TpvScene3DRendererInstance.MotionBlurTileSize,
                                                                                 fInstance.MotionBlurSoftZExtent,
                                                                                 fInstance.ZNear);
  PushConstants.FrameSpare:=TpvVector4.InlineableCreate(aFrameIndex,0.0,0.0,0.0);

  aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                  TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_COMPUTE_BIT),
                                  0,
                                  SizeOf(TpvScene3DRendererPassesMotionBlurComputePass.TPushConstants),
                                  @PushConstants);

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       fPipelineLayout.Handle,
                                       0,
                                       1,
                                       @fVulkanDescriptorSets[InFlightFrameIndex].Handle,
                                       0,
                                       nil);

  if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
   fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.BeginBreadcrumb(aCommandBuffer.Handle,TpvVulkanBreadcrumbType.Dispatch,'MotionBlur');
  end;
  // One invocation per pixel, eight by eight to a workgroup.
  aCommandBuffer.CmdDispatch(Max(1,(fResourceOutput.Width+7) shr 3),
                             Max(1,(fResourceOutput.Height+7) shr 3),
                             CountViews);
  if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
   fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.EndBreadcrumb(aCommandBuffer.Handle);
  end;

 end;

 begin

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
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    1,@ImageMemoryBarrier);

 end;

end;

end.
