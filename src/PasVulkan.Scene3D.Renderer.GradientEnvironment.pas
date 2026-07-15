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
unit PasVulkan.Scene3D.Renderer.GradientEnvironment;
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
     PasVulkan.Scene3D.Renderer.MipmapImageCubeMap,
     PasVulkan.Scene3D.Renderer.CubeMapMipMapGenerator,
     PasVulkan.Scene3D.Renderer.CubeMapIBLFilter;

type { TpvScene3DRendererGradientEnvironment }
     // Per-frame rebaked IBL source for the stylized gradient sky. It fills a raw
     // radiance cube map from the scene's gradient palette (cubemap_gradient.comp,
     // the same ramp plus horizon-tinted sun disk as the visible skybox), then reuses
     // the shared cube-map mip-map generator and the GGX/Charlie/Lambertian IBL filters
     // to prefilter it. Mirrors the atmosphere's cube-map IBL chain, but the source is
     // camera-independent, so a single object on the renderer feeds every view.
     TpvScene3DRendererGradientEnvironment=class
      public
       const CubeMapTextureSize=32;
       type TPushConstants=packed record
             GradientTopColor:TpvVector4;      // rgb = top colour,     w = star intensity (ignored for IBL)
             GradientHorizonColor:TpvVector4;  // rgb = horizon colour, w = sun size
             GradientBottomColor:TpvVector4;   // rgb = bottom colour,  w = sun brightness
             LightDirection:TpvVector4;        // xyz = primary light direction, w = radiance intensity factor
            end;
            PPushConstants=^TPushConstants;
      private
       fScene3D:TpvScene3D;
       fRenderer:TObject;
       fVulkanDevice:TpvVulkanDevice;
       fCubeMapTexture:TpvScene3DRendererMipmapImageCubeMap;
       fGGXCubeMapTexture:TpvScene3DRendererMipmapImageCubeMap;
       fCharlieCubeMapTexture:TpvScene3DRendererMipmapImageCubeMap;
       fLambertianCubeMapTexture:TpvScene3DRendererMipmapImageCubeMap;
       fCubeMapMipMapGenerator:TpvScene3DRendererCubeMapMipMapGenerator;
       fGGXCubeMapIBLFilter:TpvScene3DRendererCubeMapIBLFilter;
       fCharlieCubeMapIBLFilter:TpvScene3DRendererCubeMapIBLFilter;
       fLambertianCubeMapIBLFilter:TpvScene3DRendererCubeMapIBLFilter;
       fComputeShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStageCompute:TpvVulkanPipelineShaderStage;
       fCubeMapImageView:TpvVulkanImageView;
       fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fVulkanDescriptorSet:TpvVulkanDescriptorSet;
       fPipelineLayout:TpvVulkanPipelineLayout;
       fPipeline:TpvVulkanComputePipeline;
       fPushConstants:TPushConstants;
       fReady:Boolean;
      public
       constructor Create(const aScene3D:TpvScene3D;const aRenderer:TObject); reintroduce;
       destructor Destroy; override;
       procedure AcquirePersistentResources;
       procedure ReleasePersistentResources;
       procedure AcquireVolatileResources;
       procedure ReleaseVolatileResources;
       procedure Execute(const aInFlightFrameIndex:TpvSizeInt;const aCommandBuffer:TpvVulkanCommandBuffer);
      published
       property CubeMapTexture:TpvScene3DRendererMipmapImageCubeMap read fCubeMapTexture;
       property GGXCubeMapTexture:TpvScene3DRendererMipmapImageCubeMap read fGGXCubeMapTexture;
       property CharlieCubeMapTexture:TpvScene3DRendererMipmapImageCubeMap read fCharlieCubeMapTexture;
       property LambertianCubeMapTexture:TpvScene3DRendererMipmapImageCubeMap read fLambertianCubeMapTexture;
       property Ready:Boolean read fReady;
     end;

implementation

uses PasVulkan.Scene3D.Renderer;

{ TpvScene3DRendererGradientEnvironment }

constructor TpvScene3DRendererGradientEnvironment.Create(const aScene3D:TpvScene3D;const aRenderer:TObject);
begin

 inherited Create;

 fScene3D:=aScene3D;

 fRenderer:=aRenderer;

 fVulkanDevice:=fScene3D.VulkanDevice;

 fCubeMapTexture:=nil;
 fGGXCubeMapTexture:=nil;
 fCharlieCubeMapTexture:=nil;
 fLambertianCubeMapTexture:=nil;

 fCubeMapMipMapGenerator:=nil;
 fGGXCubeMapIBLFilter:=nil;
 fCharlieCubeMapIBLFilter:=nil;
 fLambertianCubeMapIBLFilter:=nil;

 fComputeShaderModule:=nil;
 fVulkanPipelineShaderStageCompute:=nil;
 fCubeMapImageView:=nil;
 fVulkanDescriptorSetLayout:=nil;
 fVulkanDescriptorPool:=nil;
 fVulkanDescriptorSet:=nil;
 fPipelineLayout:=nil;
 fPipeline:=nil;

 fReady:=false;

end;

destructor TpvScene3DRendererGradientEnvironment.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererGradientEnvironment.AcquirePersistentResources;
var Stream:TStream;
begin

 // The raw radiance cube map is a 32x32x6 RGBA16F image, matching the atmosphere's IBL source,
 // so the shared mip-map generator and IBL filters accept it unchanged.
 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('cubemap_gradient_rgba16f_comp.spv');
 try
  fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
 finally
  Stream.Free;
 end;
 fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DRendererGradientEnvironment.fComputeShaderModule');

 fVulkanPipelineShaderStageCompute:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

end;

procedure TpvScene3DRendererGradientEnvironment.ReleasePersistentResources;
begin
 FreeAndNil(fVulkanPipelineShaderStageCompute);
 FreeAndNil(fComputeShaderModule);
end;

procedure TpvScene3DRendererGradientEnvironment.AcquireVolatileResources;
begin

 fCubeMapTexture:=TpvScene3DRendererMipmapImageCubeMap.Create(fVulkanDevice,
                                                              CubeMapTextureSize,
                                                              CubeMapTextureSize,
                                                              VK_FORMAT_R16G16B16A16_SFLOAT,
                                                              true,
                                                              VK_SAMPLE_COUNT_1_BIT,
                                                              VK_IMAGE_LAYOUT_GENERAL,
                                                              TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                              nil,
                                                              0,
                                                              'TpvScene3DRendererGradientEnvironment.CubeMapTexture');

 fGGXCubeMapTexture:=TpvScene3DRendererMipmapImageCubeMap.Create(fVulkanDevice,
                                                                 CubeMapTextureSize,
                                                                 CubeMapTextureSize,
                                                                 VK_FORMAT_R16G16B16A16_SFLOAT,
                                                                 true,
                                                                 VK_SAMPLE_COUNT_1_BIT,
                                                                 VK_IMAGE_LAYOUT_GENERAL,
                                                                 TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                 nil,
                                                                 0,
                                                                 'TpvScene3DRendererGradientEnvironment.GGXCubeMapTexture');

 fCharlieCubeMapTexture:=TpvScene3DRendererMipmapImageCubeMap.Create(fVulkanDevice,
                                                                     CubeMapTextureSize,
                                                                     CubeMapTextureSize,
                                                                     VK_FORMAT_R16G16B16A16_SFLOAT,
                                                                     true,
                                                                     VK_SAMPLE_COUNT_1_BIT,
                                                                     VK_IMAGE_LAYOUT_GENERAL,
                                                                     TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                     nil,
                                                                     0,
                                                                     'TpvScene3DRendererGradientEnvironment.CharlieCubeMapTexture');

 fLambertianCubeMapTexture:=TpvScene3DRendererMipmapImageCubeMap.Create(fVulkanDevice,
                                                                        CubeMapTextureSize,
                                                                        CubeMapTextureSize,
                                                                        VK_FORMAT_R16G16B16A16_SFLOAT,
                                                                        true,
                                                                        VK_SAMPLE_COUNT_1_BIT,
                                                                        VK_IMAGE_LAYOUT_GENERAL,
                                                                        TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                        nil,
                                                                        0,
                                                                        'TpvScene3DRendererGradientEnvironment.LambertianCubeMapTexture');

 // The mip-map generator and the three IBL filters are shared engine helpers; they are
 // reused verbatim from the atmosphere path, only the raw source cube map differs.
 fCubeMapMipMapGenerator:=TpvScene3DRendererCubeMapMipMapGenerator.Create(fScene3D,fCubeMapTexture);
 fCubeMapMipMapGenerator.AcquirePersistentResources;
 fCubeMapMipMapGenerator.AcquireVolatileResources;

 fGGXCubeMapIBLFilter:=TpvScene3DRendererCubeMapIBLFilter.Create(fScene3D,TpvScene3DRenderer(fRenderer),fCubeMapTexture,fGGXCubeMapTexture,TpvScene3DRendererCubeMapIBLFilter.GGX);
 fGGXCubeMapIBLFilter.AcquirePersistentResources;
 fGGXCubeMapIBLFilter.AcquireVolatileResources;

 fCharlieCubeMapIBLFilter:=TpvScene3DRendererCubeMapIBLFilter.Create(fScene3D,TpvScene3DRenderer(fRenderer),fCubeMapTexture,fCharlieCubeMapTexture,TpvScene3DRendererCubeMapIBLFilter.Charlie);
 fCharlieCubeMapIBLFilter.AcquirePersistentResources;
 fCharlieCubeMapIBLFilter.AcquireVolatileResources;

 fLambertianCubeMapIBLFilter:=TpvScene3DRendererCubeMapIBLFilter.Create(fScene3D,TpvScene3DRenderer(fRenderer),fCubeMapTexture,fLambertianCubeMapTexture,TpvScene3DRendererCubeMapIBLFilter.Lambertian);
 fLambertianCubeMapIBLFilter.AcquirePersistentResources;
 fLambertianCubeMapIBLFilter.AcquireVolatileResources;

 // A dedicated cube (6-layer) view of mip level 0 of the raw cube map, bound as the
 // storage-image write target of the gradient fill compute pass.
 fCubeMapImageView:=TpvVulkanImageView.Create(fVulkanDevice,
                                              fCubeMapTexture.VulkanImage,
                                              TVkImageViewType(VK_IMAGE_VIEW_TYPE_CUBE),
                                              fCubeMapTexture.Format,
                                              VK_COMPONENT_SWIZZLE_IDENTITY,
                                              VK_COMPONENT_SWIZZLE_IDENTITY,
                                              VK_COMPONENT_SWIZZLE_IDENTITY,
                                              VK_COMPONENT_SWIZZLE_IDENTITY,
                                              TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                              0,
                                              1,
                                              0,
                                              6);
 fVulkanDevice.DebugUtils.SetObjectName(fCubeMapImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'TpvScene3DRendererGradientEnvironment.fCubeMapImageView');

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       1);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,1);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSet:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,fVulkanDescriptorSetLayout);
 fVulkanDescriptorSet.WriteToDescriptorSet(0,
                                           0,
                                           1,
                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                           [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                          fCubeMapImageView.Handle,
                                                                          VK_IMAGE_LAYOUT_GENERAL)],
                                           [],
                                           [],
                                           false);
 fVulkanDescriptorSet.Flush;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
 fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TpvScene3DRendererGradientEnvironment.TPushConstants));
 fPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fPipelineLayout.Initialize;

 fPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                            TpvScene3DRenderer(fRenderer).VulkanPipelineCache,
                                            0,
                                            fVulkanPipelineShaderStageCompute,
                                            fPipelineLayout,
                                            nil,
                                            0);

 fReady:=true;

end;

procedure TpvScene3DRendererGradientEnvironment.ReleaseVolatileResources;
begin

 fReady:=false;

 FreeAndNil(fPipeline);
 FreeAndNil(fPipelineLayout);
 FreeAndNil(fVulkanDescriptorSet);
 FreeAndNil(fVulkanDescriptorPool);
 FreeAndNil(fVulkanDescriptorSetLayout);
 FreeAndNil(fCubeMapImageView);

 if assigned(fLambertianCubeMapIBLFilter) then begin
  fLambertianCubeMapIBLFilter.ReleaseVolatileResources;
  fLambertianCubeMapIBLFilter.ReleasePersistentResources;
  FreeAndNil(fLambertianCubeMapIBLFilter);
 end;

 if assigned(fCharlieCubeMapIBLFilter) then begin
  fCharlieCubeMapIBLFilter.ReleaseVolatileResources;
  fCharlieCubeMapIBLFilter.ReleasePersistentResources;
  FreeAndNil(fCharlieCubeMapIBLFilter);
 end;

 if assigned(fGGXCubeMapIBLFilter) then begin
  fGGXCubeMapIBLFilter.ReleaseVolatileResources;
  fGGXCubeMapIBLFilter.ReleasePersistentResources;
  FreeAndNil(fGGXCubeMapIBLFilter);
 end;

 if assigned(fCubeMapMipMapGenerator) then begin
  fCubeMapMipMapGenerator.ReleaseVolatileResources;
  fCubeMapMipMapGenerator.ReleasePersistentResources;
  FreeAndNil(fCubeMapMipMapGenerator);
 end;

 FreeAndNil(fLambertianCubeMapTexture);
 FreeAndNil(fCharlieCubeMapTexture);
 FreeAndNil(fGGXCubeMapTexture);
 FreeAndNil(fCubeMapTexture);

end;

procedure TpvScene3DRendererGradientEnvironment.Execute(const aInFlightFrameIndex:TpvSizeInt;const aCommandBuffer:TpvVulkanCommandBuffer);
var ImageMemoryBarriers:array[0..0] of TVkImageMemoryBarrier;
begin

 if not fReady then begin
  exit;
 end;

 // The gradient palette lives on the scene and may be shifted per session or morphed per
 // frame; it is uploaded fresh into the push constants each time, so no dirty tracking is
 // needed. The star intensity is packed for layout parity but ignored by the IBL shader.
 fPushConstants.GradientTopColor:=TpvVector4.InlineableCreate(fScene3D.SkyGradientTopColor,fScene3D.SkyGradientStarIntensity);
 fPushConstants.GradientHorizonColor:=TpvVector4.InlineableCreate(fScene3D.SkyGradientHorizonColor,fScene3D.SkyGradientSunSize);
 fPushConstants.GradientBottomColor:=TpvVector4.InlineableCreate(fScene3D.SkyGradientBottomColor,fScene3D.SkyGradientSunBrightness);
 fPushConstants.LightDirection:=TpvVector4.InlineableCreate(fScene3D.PrimaryLightDirection,fScene3D.EnvironmentIntensityFactor);

 // Gradient fill pass: write the raw radiance cube map (mip 0, all six faces).

 fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DRendererGradientEnvironment.GradientCubeMapPass',[1.0,0.75,0.25,1.0]);

 ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(0,
                                                      TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      VK_IMAGE_LAYOUT_UNDEFINED,
                                                      VK_IMAGE_LAYOUT_GENERAL,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      fCubeMapTexture.VulkanImage.Handle,
                                                      TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                      0,
                                                                                      1,
                                                                                      0,
                                                                                      6));

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@ImageMemoryBarriers[0]);

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                      fPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSet.Handle,
                                      0,
                                      nil);

 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                 TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_COMPUTE_BIT),
                                 0,
                                 SizeOf(TpvScene3DRendererGradientEnvironment.TPushConstants),
                                 @fPushConstants);

 aCommandBuffer.CmdDispatch((fCubeMapTexture.Width+15) shr 4,
                            (fCubeMapTexture.Height+15) shr 4,
                            6);

 ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                                      VK_IMAGE_LAYOUT_GENERAL,
                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      fCubeMapTexture.VulkanImage.Handle,
                                                      TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                      0,
                                                                                      1,
                                                                                      0,
                                                                                      6));

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@ImageMemoryBarriers[0]);

 fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

 // Prefilter the freshly baked radiance into the mip chain and the three IBL cube maps.

 fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DRendererGradientEnvironment.CubeMapMipMapPass',[1.0,0.5,0.75,1.0]);
 fCubeMapMipMapGenerator.Execute(aCommandBuffer);
 fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

 fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DRendererGradientEnvironment.GGXCubeMapIBLFilterPass',[0.5,1.0,0.75,1.0]);
 fGGXCubeMapIBLFilter.Execute(aCommandBuffer);
 fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

 fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DRendererGradientEnvironment.CharlieCubeMapIBLFilterPass',[0.5,0.75,1.0,1.0]);
 fCharlieCubeMapIBLFilter.Execute(aCommandBuffer);
 fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

 fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DRendererGradientEnvironment.LambertianCubeMapIBLFilterPass',[0.75,1.0,0.5,1.0]);
 fLambertianCubeMapIBLFilter.Execute(aCommandBuffer);
 fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

end;

end.
