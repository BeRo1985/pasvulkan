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
unit PasVulkan.Scene3D.Renderer.Passes.VolumetricScatteringComposeComputePass;
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

type { TpvScene3DRendererPassesVolumetricScatteringComposeComputePass }
     // The last of the three, and the only one of them that stands in the visible chain: it takes the
     // picture as it is, thins it by what the air swallowed and adds what the air scattered in, and hands
     // the result on as the new LastOutputResource. The upsample from half size happens in the same
     // dispatch - see volumetric_scattering_compose.comp for why that does not want a pass of its own, and
     // for why both halves of the air have to be applied and not just the one that adds light.
     TpvScene3DRendererPassesVolumetricScatteringComposeComputePass=class(TpvFrameGraph.TComputePass)
      public
       const // How hard the upsample separates two depths: the weight of a tap is 1/(1+(difference*this)),
             // so a bigger number keeps the four taps more strictly apart and a smaller one lets them
             // blend. A thousand is very nearly a hard choice rather than a blend - half a metre of
             // disagreement already puts a tap at a five-hundredth of the weight of one that agrees. That
             // is deliberate: this branch is only reached at a depth edge in the first place, and blending
             // gently across one is the halo the whole arrangement exists to avoid.
             VolumetricScatteringUpsampleDepthWeight=1000.0;
             // And what the old compose used, which is a thousand times softer: at that weight a metre of
             // disagreement still leaves a tap at half, so where the depths agree at all the four come out
             // as a plain average. That averaging IS the old look, so the legacy switch needs the old
             // number and not only the old code path.
             VolumetricScatteringUpsampleDepthWeightLegacy=1.0;
      public
       const // Bit zero of the flag word: the scattering over a picture dimmed almost to nothing, for
             // telling this effect apart from everything else in a finished frame.
             VolumetricScatteringComposeFlagShowScatteringOnly=TpvUInt32(1) shl 0;
             // And bit one: whether the effect does anything this frame at all. Clear hands the picture
             // through untouched, which is what this pass does in place of the skipping the three passes
             // before it can afford - it owns the resource the rest of the chain reads.
             VolumetricScatteringComposeFlagEnabled=TpvUInt32(1) shl 1;
             // And bit two: the extinction alone, as a colour. White = the air takes nothing away, black =
             // the world is being multiplied by nothing.
             VolumetricScatteringComposeFlagShowExtinctionOnly=TpvUInt32(1) shl 2;
             // And bit three: the upsample the old compose did - all four taps weighted every time and a
             // depth weight soft enough that agreeing depths come out as a plain 2x2 average.
             VolumetricScatteringComposeFlagLegacyLook=TpvUInt32(1) shl 3;
             // And bit four: where not one of the four taps is of the pixel's own kind - sky against
             // geometry - look outwards for one that is, rather than weigh four wrong answers against
             // each other. That is the rim along every silhouette against the sky, and it cannot be
             // fixed by compositing per sample: the air itself was only ever computed once per texel,
             // at the resolved depth, which at a silhouette is the geometry's.
             VolumetricScatteringComposeFlagSkyTapSearch=TpvUInt32(1) shl 4;
      public
       type TPushConstants=packed record
             // x = strength, y = how depth becomes a distance, z = how hard the upsample separates two
             // depths, w = the depth the sky is given, which this pass has to hand back for a sky pixel
             // itself if the two are to be one quantity
             StrengthZNearDepthWeightSkyDepth:TpvVector4;
             // x = switches, in a word of their own rather than squeezed into a spare float lane.
             // y = how many samples the raw depth has, which only the MSAA variant reads.
             FlagsSampleCountSpare:TpvUInt32Vector4;
            end;
      private
       fInstance:TpvScene3DRendererInstance;
       fResourceInput:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceScattering:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceExtinction:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceScatteringDepth:TpvFrameGraph.TPass.TUsedImageResource;
       // Under MSAA this pass reads the RAW multisampled depth and answers once per sample, because the
       // resolved depth has already thrown away what it needs: the resolve keeps the nearest sample, so a
       // silhouette pixel whose colour is nine tenths sky reports the sliver of geometry in front and then
       // takes that geometry's scattering over its whole area. The fog pass beside us does the same thing
       // for the same reason - see the note at the top of fog.frag.
       fMSAA:Boolean;
       fResourceMSAADepth:TpvFrameGraph.TPass.TUsedImageResource;
       fMSAADepthImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fResourceOutput:TpvFrameGraph.TPass.TUsedImageResource;
       fPushConstants:TPushConstants;
       fInputImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fScatteringImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fExtinctionImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fScatteringDepthImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fOutputImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fComputeShaderModule:TpvVulkanShaderModule;
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

// In the implementation rather than the interface, and for one constant only: the distance the march caps
// its stored depth at. This pass has to cap its own reading at the same value or the two are not the same
// quantity, and taking the number from the march itself is what keeps them from drifting apart. The other
// direction does not exist - the march knows nothing of the compose - so there is no cycle here.
uses PasVulkan.Scene3D.Renderer.Passes.VolumetricScatteringRaymarchComputePass;

{ TpvScene3DRendererPassesVolumetricScatteringComposeComputePass }

constructor TpvScene3DRendererPassesVolumetricScatteringComposeComputePass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin

 inherited Create(aFrameGraph);

 fInstance:=aInstance;

 fMSAA:=fInstance.Renderer.SurfaceSampleCountFlagBits<>TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT);

 Name:='VolumetricScatteringComposeComputePass';

 fResourceInput:=AddImageInput(fInstance.LastOutputResource.ResourceType.Name,
                               fInstance.LastOutputResource.Resource.Name,
                               VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                               []
                              );

 fResourceScattering:=AddImageInput('resourcetype_volumetric_scattering',
                                    'resource_volumetric_scattering_blurred_xy',
                                    VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                    []
                                   );

 fResourceExtinction:=AddImageInput('resourcetype_volumetric_scattering',
                                    'resource_volumetric_scattering_extinction_blurred_xy',
                                    VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                    []
                                   );

 // The depth the march measured, straight from the march - not from the blurred buffers, which no longer
 // carry it. It is the same image the two blur steps read, so what this pass weighs its taps against is
 // what they smoothed by, exactly.
 fResourceScatteringDepth:=AddImageInput('resourcetype_volumetric_scattering_depth',
                                         'resource_volumetric_scattering_depth',
                                         VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                         []
                                        );

 // And under MSAA the raw multisampled depth as well, which is the only place the per-sample coverage of
 // a silhouette pixel still exists. The resolved depth this pass otherwise reads has already been reduced
 // to the nearest sample.
 if fMSAA then begin
  fResourceMSAADepth:=AddImageInput('resourcetype_msaa_depth',
                                    'resource_msaa_depth_data',
                                    VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                    [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                   );
 end else begin
  fResourceMSAADepth:=nil;
 end;

 fResourceOutput:=AddImageOutput('resourcetype_color_volumetric_scattering',
                                 'resource_volumetric_scattering_color',
                                 VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare),
                                 []
                                );

 fInstance.LastOutputResource:=fResourceOutput;

end;

destructor TpvScene3DRendererPassesVolumetricScatteringComposeComputePass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesVolumetricScatteringComposeComputePass.AcquirePersistentResources;
var Stream:TStream;
begin

 inherited AcquirePersistentResources;

 // Two variants, and which one is chosen is settled here rather than per frame: the MSAA one declares a
 // sampler2DMSArray and a binding the other does not have, so the descriptor set layout differs with it.
 if fMSAA then begin
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('volumetric_scattering_compose_msaa_comp.spv');
 end else begin
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('volumetric_scattering_compose_comp.spv');
 end;
 try
  fComputeShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageCompute:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

end;

procedure TpvScene3DRendererPassesVolumetricScatteringComposeComputePass.ReleasePersistentResources;
begin
 FreeAndNil(fVulkanPipelineShaderStageCompute);
 FreeAndNil(fComputeShaderModule);
 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesVolumetricScatteringComposeComputePass.AcquireVolatileResources;
var InFlightFrameIndex,CountViews:TpvInt32;
begin

 inherited AcquireVolatileResources;

 CountViews:=fInstance.CountSurfaceViews;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(fInstance.Renderer.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fInstance.Renderer.CountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fInstance.Renderer.CountInFlightFrames*6);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,fInstance.Renderer.CountInFlightFrames);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fInstance.Renderer.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(1,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(2,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(3,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(4,
                                       VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(5,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 if fMSAA then begin
  fVulkanDescriptorSetLayout.AddBinding(6,
                                        VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                        1,
                                        TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                        []);
 end;
 fVulkanDescriptorSetLayout.Initialize;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(fInstance.Renderer.VulkanDevice);
 fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TpvScene3DRendererPassesVolumetricScatteringComposeComputePass.TPushConstants));
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

  fInputImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                                                  fResourceInput.VulkanImages[InFlightFrameIndex],
                                                                  TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY),
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

  fScatteringImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                                                       fResourceScattering.VulkanImages[InFlightFrameIndex],
                                                                       TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY),
                                                                       TpvFrameGraph.TImageResourceType(fResourceScattering.ResourceType).Format,
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

  fScatteringDepthImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                                                            fResourceScatteringDepth.VulkanImages[InFlightFrameIndex],
                                                                            TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY),
                                                                            TpvFrameGraph.TImageResourceType(fResourceScatteringDepth.ResourceType).Format,
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

  fExtinctionImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                                                       fResourceExtinction.VulkanImages[InFlightFrameIndex],
                                                                       TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY),
                                                                       TpvFrameGraph.TImageResourceType(fResourceExtinction.ResourceType).Format,
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

  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);
  // The picture is fetched by texel, so the nearest sampler is the honest one for it.
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                                fInputImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceInput.ResourceTransition.Layout)],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  // The two half-sized buffers the blur left behind: the light the air adds, and what it takes away. Both
  // by texel and therefore nearest - the upsample picks and weighs its four taps itself, precisely so that
  // it can leave out the ones belonging to a different distance, which a linear fetch could not.
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                                fScatteringImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceScattering.ResourceTransition.Layout)],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                                fExtinctionImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceExtinction.ResourceTransition.Layout)],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  // The resolved opaque depth, which is what the upsample weighs its four taps against. The ARRAY view of
  // it, for the same reason as everywhere else here.
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
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
  // The half-resolution depth the march measured. Not to be confused with binding 2 above, which is the
  // full-resolution opaque depth of the pixel being shaded - the upsample is the comparison of the two.
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(5,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                                fScatteringDepthImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceScatteringDepth.ResourceTransition.Layout)],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );

  if fMSAA then begin

   // The raw multisampled depth needs a depth-aspect array view of its own - the frame graph's default
   // view is a depth/stencil attachment view - exactly as the fog pass builds one. The shader reads it
   // with texelFetch per sample, so the sampler attached to it never comes into play.
   fMSAADepthImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                                                       fResourceMSAADepth.VulkanImages[InFlightFrameIndex],
                                                                       TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY),
                                                                       TpvFrameGraph.TImageResourceType(fResourceMSAADepth.ResourceType).Format,
                                                                       VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                       VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                       VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                       VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                       TVkImageAspectFlags(VK_IMAGE_ASPECT_DEPTH_BIT),
                                                                       0,
                                                                       1,
                                                                       0,
                                                                       CountViews
                                                                      );

   fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(6,
                                                                  0,
                                                                  1,
                                                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                  [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                                 fMSAADepthImageViews[InFlightFrameIndex].Handle,
                                                                                                 fResourceMSAADepth.ResourceTransition.Layout)],
                                                                  [],
                                                                  [],
                                                                  false
                                                                 );

  end;

  fVulkanDescriptorSets[InFlightFrameIndex].Flush;

 end;

end;

procedure TpvScene3DRendererPassesVolumetricScatteringComposeComputePass.ReleaseVolatileResources;
var InFlightFrameIndex:TpvInt32;
begin
 FreeAndNil(fPipeline);
 FreeAndNil(fPipelineLayout);
 for InFlightFrameIndex:=0 to fInstance.Renderer.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fInputImageViews[InFlightFrameIndex]);
  FreeAndNil(fScatteringImageViews[InFlightFrameIndex]);
  FreeAndNil(fExtinctionImageViews[InFlightFrameIndex]);
  FreeAndNil(fScatteringDepthImageViews[InFlightFrameIndex]);
  FreeAndNil(fMSAADepthImageViews[InFlightFrameIndex]);
  FreeAndNil(fOutputImageViews[InFlightFrameIndex]);
 end;
 FreeAndNil(fVulkanDescriptorSetLayout);
 FreeAndNil(fVulkanDescriptorPool);
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesVolumetricScatteringComposeComputePass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvScene3DRendererPassesVolumetricScatteringComposeComputePass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var CountViews:TpvInt32;
    ImageMemoryBarrier:TVkImageMemoryBarrier;
begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 CountViews:=fInstance.CountSurfaceViews;

 // As in the blur pass: the graph hands the output in as SHADER_READ_ONLY_OPTIMAL, and a storage image
 // may not be written in that layout.
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
  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    1,@ImageMemoryBarrier);
 end;

 begin

  aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

  // Read here rather than in Update, so a strength the game changes between frames lands on the frame it
  // was meant for.
  // ZNear turns the stored depth back into a distance, exactly as the march did when it wrote it - the
  // two have to be the same quantity or the upsample would be comparing nothing with nothing.
  // The depth weight goes with the code path, not beside it: the legacy upsample is the old one only if it
  // also gets the old, thousand-times-softer number.
  // The sky's depth is the march's own number, taken from the march's own constant. It is a sentinel, not
  // a distance: a sky pixel gets it on both sides rather than each side clamping the same division and
  // hoping to land on the same result.
  if fInstance.VolumetricScatteringLegacyLook then begin
   fPushConstants.StrengthZNearDepthWeightSkyDepth:=TpvVector4.InlineableCreate(fInstance.VolumetricScatteringFactor,
                                                                                fInstance.ZNear,
                                                                                VolumetricScatteringUpsampleDepthWeightLegacy,
                                                                                TpvScene3DRendererPassesVolumetricScatteringRaymarchComputePass.VolumetricScatteringSkyDepth);
  end else begin
   fPushConstants.StrengthZNearDepthWeightSkyDepth:=TpvVector4.InlineableCreate(fInstance.VolumetricScatteringFactor,
                                                                                fInstance.ZNear,
                                                                                VolumetricScatteringUpsampleDepthWeight,
                                                                                TpvScene3DRendererPassesVolumetricScatteringRaymarchComputePass.VolumetricScatteringSkyDepth);
  end;

  fPushConstants.FlagsSampleCountSpare.x:=0;
  if fInstance.VolumetricScatteringLegacyLook then begin
   fPushConstants.FlagsSampleCountSpare.x:=fPushConstants.FlagsSampleCountSpare.x or VolumetricScatteringComposeFlagLegacyLook;
  end;
  if fInstance.VolumetricScatteringShowScatteringOnly then begin
   fPushConstants.FlagsSampleCountSpare.x:=fPushConstants.FlagsSampleCountSpare.x or VolumetricScatteringComposeFlagShowScatteringOnly;
  end;
  if fInstance.VolumetricScatteringShowExtinctionOnly then begin
   fPushConstants.FlagsSampleCountSpare.x:=fPushConstants.FlagsSampleCountSpare.x or VolumetricScatteringComposeFlagShowExtinctionOnly;
  end;
  if fInstance.VolumetricScatteringEnabled then begin
   fPushConstants.FlagsSampleCountSpare.x:=fPushConstants.FlagsSampleCountSpare.x or VolumetricScatteringComposeFlagEnabled;
  end;
  if fInstance.VolumetricScatteringSkyTapSearch then begin
   fPushConstants.FlagsSampleCountSpare.x:=fPushConstants.FlagsSampleCountSpare.x or VolumetricScatteringComposeFlagSkyTapSearch;
  end;
  // How many samples the raw depth carries. Only the MSAA variant reads it, and only it is given a raw
  // depth to read; one everywhere else, so a stray read could not divide by nothing.
  fPushConstants.FlagsSampleCountSpare.y:=TpvUInt32(Max(1,fInstance.Renderer.CountSurfaceMSAASamples));
  fPushConstants.FlagsSampleCountSpare.z:=0;
  fPushConstants.FlagsSampleCountSpare.w:=0;

  aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                  TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_COMPUTE_BIT),
                                  0,
                                  SizeOf(TpvScene3DRendererPassesVolumetricScatteringComposeComputePass.TPushConstants),
                                  @fPushConstants);

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       fPipelineLayout.Handle,
                                       0,
                                       1,
                                       @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil);

  if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
   fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.BeginBreadcrumb(aCommandBuffer.Handle,TpvVulkanBreadcrumbType.Dispatch,'VolumetricScatteringCompose');
  end;

  // One invocation per pixel of the full-sized picture, eight by eight to a workgroup.
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
