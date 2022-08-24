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
unit PasVulkan.Scene3D.Renderer.Passes.LensUpsampleComputePass;
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

type { TpvScene3DRendererPassesLensUpsampleComputePass }
     TpvScene3DRendererPassesLensUpsampleComputePass=class(TpvFrameGraph.TComputePass)
      private
       fInstance:TpvScene3DRendererInstance;
       fUpsampleComputeShaderModule:TpvVulkanShaderModule;
       fVulkanSampler:TpvVulkanSampler;
       fVulkanPipelineShaderStageUpsampleCompute:TpvVulkanPipelineShaderStage;
       fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fVulkanDescriptorSets:array[0..MaxInFlightFrames-1,0..15] of TpvVulkanDescriptorSet;
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

{ TpvScene3DRendererPassesLensUpsampleComputePass }

constructor TpvScene3DRendererPassesLensUpsampleComputePass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin
 inherited Create(aFrameGraph);

 fInstance:=aInstance;

 Name:='LensUpsampleComputePass';

end;

destructor TpvScene3DRendererPassesLensUpsampleComputePass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesLensUpsampleComputePass.AcquirePersistentResources;
var Stream:TStream;
    Format:string;
begin

 inherited AcquirePersistentResources;

 case fInstance.Renderer.OptimizedNonAlphaFormat of
  VK_FORMAT_B10G11R11_UFLOAT_PACK32:begin
   Format:='r11g11b10f';
  end;
  VK_FORMAT_R16G16B16A16_SFLOAT:begin
   Format:='rgba16f';
  end;
  else begin
   Assert(false);
   Format:='';
  end;
 end;

 if fInstance.CountSurfaceViews>1 then begin
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('lens_upsample_'+Format+'_multiview_comp.spv');
 end else begin
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('lens_upsample_'+Format+'_comp.spv');
 end;
 try
  fUpsampleComputeShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageUpsampleCompute:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fUpsampleComputeShaderModule,'main');

end;

procedure TpvScene3DRendererPassesLensUpsampleComputePass.ReleasePersistentResources;
begin
 FreeAndNil(fVulkanPipelineShaderStageUpsampleCompute);
 FreeAndNil(fUpsampleComputeShaderModule);
 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesLensUpsampleComputePass.AcquireVolatileResources;
var InFlightFrameIndex,MipMapLevelIndex:TpvInt32;
    ImageViewType:TVkImageViewType;
begin

 inherited AcquireVolatileResources;

 fVulkanSampler:=TpvVulkanSampler.Create(fInstance.Renderer.VulkanDevice,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         0.0,
                                         false,
                                         0.0,
                                         false,
                                         VK_COMPARE_OP_ALWAYS,
                                         0.0,
                                         0.0,
                                         VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK,
                                         false);

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(fInstance.Renderer.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fInstance.Renderer.CountInFlightFrames*(fInstance.SceneMipmappedArray2DImages[0].MipMapLevels-1));
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fInstance.Renderer.CountInFlightFrames*(fInstance.SceneMipmappedArray2DImages[0].MipMapLevels-1));
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,fInstance.Renderer.CountInFlightFrames*(fInstance.SceneMipmappedArray2DImages[0].MipMapLevels-1));
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fInstance.Renderer.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(1,
                                       VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(fInstance.Renderer.VulkanDevice);
 fPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fPipelineLayout.Initialize;

 fPipeline:=TpvVulkanComputePipeline.Create(fInstance.Renderer.VulkanDevice,
                                            fInstance.Renderer.VulkanPipelineCache,
                                            0,
                                            fVulkanPipelineShaderStageUpsampleCompute,
                                            fPipelineLayout,
                                            nil,
                                            0);

 if fInstance.CountSurfaceViews>1 then begin
  ImageViewType:=TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY);
 end else begin
  ImageViewType:=TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D);
 end;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  for MipMapLevelIndex:=0 to fInstance.SceneMipmappedArray2DImages[InFlightFrameIndex].MipMapLevels-2 do begin
   fVulkanDescriptorSets[InFlightFrameIndex,MipMapLevelIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                                             fVulkanDescriptorSetLayout);
   fVulkanDescriptorSets[InFlightFrameIndex,MipMapLevelIndex].WriteToDescriptorSet(0,
                                                                                   0,
                                                                                   1,
                                                                                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                                   [TVkDescriptorImageInfo.Create(fInstance.SceneMipmappedArray2DImages[InFlightFrameIndex].VulkanMipMapSampler.Handle,
                                                                                                                  fInstance.SceneMipmappedArray2DImages[InFlightFrameIndex].DescriptorImageInfos[MipMapLevelIndex+1].imageView,
                                                                                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                                   [],
                                                                                   [],
                                                                                   false
                                                                                  );
   fVulkanDescriptorSets[InFlightFrameIndex,MipMapLevelIndex].WriteToDescriptorSet(1,
                                                                                   0,
                                                                                   1,
                                                                                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                                   [TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                                                  fInstance.SceneMipmappedArray2DImages[InFlightFrameIndex].DescriptorImageInfos[MipMapLevelIndex].imageView,
                                                                                                                  VK_IMAGE_LAYOUT_GENERAL)],
                                                                                   [],
                                                                                   [],
                                                                                   false
                                                                                  );
   fVulkanDescriptorSets[InFlightFrameIndex,MipMapLevelIndex].Flush;
  end;
 end;

end;

procedure TpvScene3DRendererPassesLensUpsampleComputePass.ReleaseVolatileResources;
var InFlightFrameIndex,MipMapLevelIndex:TpvInt32;
begin
 FreeAndNil(fPipeline);
 FreeAndNil(fPipelineLayout);
 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  for MipMapLevelIndex:=0 to fInstance.SceneMipmappedArray2DImages[InFlightFrameIndex].MipMapLevels-2 do begin
   FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex,MipMapLevelIndex]);
  end;
 end;
 FreeAndNil(fVulkanDescriptorSetLayout);
 FreeAndNil(fVulkanDescriptorPool);
 FreeAndNil(fVulkanSampler);
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesLensUpsampleComputePass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvScene3DRendererPassesLensUpsampleComputePass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var InFlightFrameIndex,MipMapLevelIndex:TpvInt32;
    Pipeline:TpvVulkanComputePipeline;
    ImageMemoryBarrier:TVkImageMemoryBarrier;
begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 InFlightFrameIndex:=aInFlightFrameIndex;

 Pipeline:=fPipeline;
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,Pipeline.Handle);

 FillChar(ImageMemoryBarrier,SizeOf(TVkImageMemoryBarrier),#0);
 ImageMemoryBarrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
 ImageMemoryBarrier.pNext:=nil;
 ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT);
 ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
 ImageMemoryBarrier.oldLayout:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
 ImageMemoryBarrier.newLayout:=VK_IMAGE_LAYOUT_GENERAL;
 ImageMemoryBarrier.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 ImageMemoryBarrier.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 ImageMemoryBarrier.image:=fInstance.SceneMipmappedArray2DImages[InFlightFrameIndex].VulkanImage.Handle;
 ImageMemoryBarrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 ImageMemoryBarrier.subresourceRange.baseMipLevel:=0;
 ImageMemoryBarrier.subresourceRange.levelCount:=fInstance.SceneMipmappedArray2DImages[InFlightFrameIndex].MipMapLevels-1;
 ImageMemoryBarrier.subresourceRange.baseArrayLayer:=0;
 ImageMemoryBarrier.subresourceRange.layerCount:=fInstance.CountSurfaceViews;
 aCommandBuffer.CmdPipelineBarrier(fFrameGraph.VulkanDevice.PhysicalDevice.PipelineStageAllShaderBits,
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@ImageMemoryBarrier);

 for MipMapLevelIndex:=fInstance.SceneMipmappedArray2DImages[InFlightFrameIndex].MipMapLevels-2 downto 0 do begin

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       fPipelineLayout.Handle,
                                       0,
                                       1,
                                       @fVulkanDescriptorSets[InFlightFrameIndex,MipMapLevelIndex].Handle,
                                       0,
                                       nil);

  aCommandBuffer.CmdDispatch(Max(1,(fInstance.Width+((1 shl (4+MipMapLevelIndex))-1)) shr (4+MipMapLevelIndex)),
                             Max(1,(fInstance.Height+((1 shl (4+MipMapLevelIndex))-1)) shr (4+MipMapLevelIndex)),
                             fInstance.CountSurfaceViews);

  FillChar(ImageMemoryBarrier,SizeOf(TVkImageMemoryBarrier),#0);
  ImageMemoryBarrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
  ImageMemoryBarrier.pNext:=nil;
  ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
  ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT);
  ImageMemoryBarrier.oldLayout:=VK_IMAGE_LAYOUT_GENERAL;
  ImageMemoryBarrier.newLayout:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
  ImageMemoryBarrier.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  ImageMemoryBarrier.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  ImageMemoryBarrier.image:=fInstance.SceneMipmappedArray2DImages[InFlightFrameIndex].VulkanImage.Handle;
  ImageMemoryBarrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
  ImageMemoryBarrier.subresourceRange.baseMipLevel:=MipMapLevelIndex;
  ImageMemoryBarrier.subresourceRange.levelCount:=1;
  ImageMemoryBarrier.subresourceRange.baseArrayLayer:=0;
  ImageMemoryBarrier.subresourceRange.layerCount:=fInstance.CountSurfaceViews;
  if MipMapLevelIndex>0 then begin
   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                     0,
                                     0,nil,
                                     0,nil,
                                     1,@ImageMemoryBarrier);
  end else begin
   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                     fFrameGraph.VulkanDevice.PhysicalDevice.PipelineStageAllShaderBits,
                                     0,
                                     0,nil,
                                     0,nil,
                                     1,@ImageMemoryBarrier);
  end;

 end;

end;

end.
