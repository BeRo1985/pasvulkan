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
unit PasVulkan.Scene3D.Renderer.Passes.VolumetricScatteringComposeRenderPass;
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

type { TpvScene3DRendererPassesVolumetricScatteringComposeRenderPass }
     // The compose step of the volumetric scattering, in the form that runs BEFORE the multisample
     // resolve - the AtmosphericCompositingBeforeResolve option. Same arithmetic as the compute pass
     // beside it, and literally the same code: both include volumetric_scattering_compose.glsl, so
     // the two cannot drift apart.
     //
     // What differs is where it sits. The compute pass works on the resolved picture, and at a
     // silhouette that is a picture whose colour is already a coverage mix of two surfaces at two
     // distances while its depth names only one of them. Applying one transmittance to a blended
     // colour is not the same as blending two transmitted colours, and the difference is a thin rim
     // along every sky-to-geometry boundary in the frame. This pass runs at sample rate on the
     // unresolved colour instead: one invocation per sample, each with its own depth, and the resolve
     // afterwards averages finished results. There is then nothing left to approximate.
     //
     // It costs a full shading pass per sample, and it forces the fog pass to move with it - the two
     // have to be applied to the same picture - which is why this is a create-time option and not the
     // default.
     TpvScene3DRendererPassesVolumetricScatteringComposeRenderPass=class(TpvFrameGraph.TRenderPass)
      public
       type TPushConstants=packed record
             // x = strength, y = how depth becomes a distance, z = how hard the upsample separates two
             // depths, w = the depth the sky is given, which this pass has to hand back for a sky pixel
             // itself if the two are to be one quantity
             StrengthZNearDepthWeightSkyDepth:TpvVector4;
             // x = switches, in a word of their own rather than squeezed into a spare float lane.
             // y = how many samples the raw depth has.
             FlagsSampleCountSpare:TpvUInt32Vector4;
            end;
      private
       fInstance:TpvScene3DRendererInstance;
       fVulkanRenderPass:TpvVulkanRenderPass;
       fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceScattering:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceExtinction:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceScatteringDepth:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceMSAADepth:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceOutput:TpvFrameGraph.TPass.TUsedImageResource;
       // Whether the march produced a second answer for the sky's distance. With it, a sample looking past
       // a silhouette reads that instead of weighing four taps that all belong to the geometry in front.
       fDualOutput:Boolean;
       fResourceFarScattering:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceFarExtinction:TpvFrameGraph.TPass.TUsedImageResource;
       fPushConstants:TPushConstants;
       fScatteringImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fExtinctionImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fScatteringDepthImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fMSAADepthImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fFarScatteringImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fFarExtinctionImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
       fVulkanVertexShaderModule:TpvVulkanShaderModule;
       fVulkanFragmentShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
       fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
       fVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
       fVulkanPipelineLayout:TpvVulkanPipelineLayout;
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

// For the march's own sky-depth constant and for the two upsample depth weights: this pass has to use the
// very same numbers as the compute variant, and taking them from where they already live is what keeps the
// two from drifting apart. Neither of those units knows anything of this one, so there is no cycle.
uses PasVulkan.Scene3D.Renderer.Passes.VolumetricScatteringRaymarchComputePass,
     PasVulkan.Scene3D.Renderer.Passes.VolumetricScatteringComposeComputePass;

{ TpvScene3DRendererPassesVolumetricScatteringComposeRenderPass }

constructor TpvScene3DRendererPassesVolumetricScatteringComposeRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin

 inherited Create(aFrameGraph);

 fInstance:=aInstance;

 fDualOutput:=fInstance.VolumetricScatteringDualOutputActive;

 Name:='VolumetricScatteringComposeRenderPass';

 MultiviewMask:=fInstance.SurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       fInstance.SizeFactor,
                                       fInstance.SizeFactor,
                                       1.0,
                                       fInstance.CountSurfaceViews);

 // The still-multisampled picture, as an input attachment: the pass reads and writes the same pixel of the
 // same attachment, which is the one thing an input attachment is allowed to do and a sampler is not.
 fResourceColor:=AddImageInput(fInstance.LastMSAAOutputResource.ResourceType.Name,
                               fInstance.LastMSAAOutputResource.Resource.Name,
                               VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                               [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
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

 // What the air does over the SKY's distance, through the same two blur steps as the pair above. With
 // these bound, a sample that looks at the sky reads the sky's own air at its own pixel - nothing is
 // borrowed from a neighbour and nothing is estimated.
 if fDualOutput then begin

  fResourceFarScattering:=AddImageInput('resourcetype_volumetric_scattering',
                                        'resource_volumetric_scattering_far_blurred_xy',
                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                        []
                                       );

  fResourceFarExtinction:=AddImageInput('resourcetype_volumetric_scattering',
                                        'resource_volumetric_scattering_far_extinction_blurred_xy',
                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                        []
                                       );

 end else begin

  fResourceFarScattering:=nil;
  fResourceFarExtinction:=nil;

 end;

 // And the raw multisampled depth, which is the whole reason this variant exists: it is the only place
 // where each sample still has a distance of its own.
 fResourceMSAADepth:=AddImageInput('resourcetype_msaa_depth',
                                   'resource_msaa_depth_data',
                                   VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                   [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                  );

 // The output takes the input's resource type, so that it carries the same sample count by construction
 // rather than by a name chosen here having to agree with one chosen elsewhere.
 fResourceOutput:=AddImageOutput(fInstance.LastMSAAOutputResource.ResourceType.Name,
                                 'resource_volumetric_scattering_msaa_color',
                                 VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare),
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

 // Both trackers, because both are now this: the chain's picture, and the multisampled one the resolve
 // that follows has to be pointed at.
 fInstance.LastOutputResource:=fResourceOutput;
 fInstance.LastMSAAOutputResource:=fResourceOutput;

end;

destructor TpvScene3DRendererPassesVolumetricScatteringComposeRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesVolumetricScatteringComposeRenderPass.AcquirePersistentResources;
var Stream:TStream;
begin

 inherited AcquirePersistentResources;

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('fullscreen_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fDualOutput then begin
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('volumetric_scattering_compose_per_sample_dual_frag.spv');
 end else begin
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('volumetric_scattering_compose_per_sample_frag.spv');
 end;
 try
  fVulkanFragmentShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVulkanVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fVulkanFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

end;

procedure TpvScene3DRendererPassesVolumetricScatteringComposeRenderPass.ReleasePersistentResources;
begin
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesVolumetricScatteringComposeRenderPass.AcquireVolatileResources;
var InFlightFrameIndex,CountViews:TpvInt32;
begin

 inherited AcquireVolatileResources;

 CountViews:=fInstance.CountSurfaceViews;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(fInstance.Renderer.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fInstance.Renderer.CountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,fInstance.Renderer.CountInFlightFrames);
 // Four sampled images normally, six with the far pair among them.
 if fDualOutput then begin
  fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fInstance.Renderer.CountInFlightFrames*6);
 end else begin
  fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fInstance.Renderer.CountInFlightFrames*4);
 end;
 fVulkanDescriptorPool.Initialize;

 // The binding numbers are the shared include's, gaps and all: bindings three and four belong to the
 // compute variant's resolved depth and storage image, neither of which exists here. A layout may declare
 // fewer bindings than the numbering suggests, and leaving the numbers alone is what lets both stages
 // include the same file.
 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fInstance.Renderer.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(1,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(2,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(5,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(6,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 if fDualOutput then begin
  fVulkanDescriptorSetLayout.AddBinding(7,
                                        VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                        1,
                                        TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                        []);
  fVulkanDescriptorSetLayout.AddBinding(8,
                                        VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                        1,
                                        TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                        []);
 end;
 fVulkanDescriptorSetLayout.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin

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

  // The raw multisampled depth needs a depth-aspect array view of its own - the frame graph's default view
  // is a depth/stencil attachment view - exactly as the fog pass builds one. The shader reads it with
  // texelFetch at one sample index, so the sampler attached to it never comes into play.
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

  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);

  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                fResourceColor.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceColor.ResourceTransition.Layout)],
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

  // The half-resolution depth the march measured, which the upsample weighs its four taps against.
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

  if fDualOutput then begin

   fFarScatteringImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                                                            fResourceFarScattering.VulkanImages[InFlightFrameIndex],
                                                                            TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY),
                                                                            TpvFrameGraph.TImageResourceType(fResourceFarScattering.ResourceType).Format,
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

   fFarExtinctionImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                                                            fResourceFarExtinction.VulkanImages[InFlightFrameIndex],
                                                                            TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY),
                                                                            TpvFrameGraph.TImageResourceType(fResourceFarExtinction.ResourceType).Format,
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

   fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(7,
                                                                  0,
                                                                  1,
                                                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                  [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                                 fFarScatteringImageViews[InFlightFrameIndex].Handle,
                                                                                                 fResourceFarScattering.ResourceTransition.Layout)],
                                                                  [],
                                                                  [],
                                                                  false
                                                                 );

   fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(8,
                                                                  0,
                                                                  1,
                                                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                  [TVkDescriptorImageInfo.Create(fInstance.Renderer.ClampedNearestSampler.Handle,
                                                                                                 fFarExtinctionImageViews[InFlightFrameIndex].Handle,
                                                                                                 fResourceFarExtinction.ResourceTransition.Layout)],
                                                                  [],
                                                                  [],
                                                                  false
                                                                 );

  end else begin

   fFarScatteringImageViews[InFlightFrameIndex]:=nil;
   fFarExtinctionImageViews[InFlightFrameIndex]:=nil;

  end;

  fVulkanDescriptorSets[InFlightFrameIndex].Flush;

 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(fInstance.Renderer.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),0,SizeOf(TpvScene3DRendererPassesVolumetricScatteringComposeRenderPass.TPushConstants));
 fVulkanPipelineLayout.Initialize;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(fInstance.Renderer.VulkanDevice,
                                                           fInstance.Renderer.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           fVulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           VulkanRenderPassSubpassIndex,
                                                           nil,
                                                           0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fResourceOutput.Width,fResourceOutput.Height,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fResourceOutput.Width,fResourceOutput.Height);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
 fVulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_CLOCKWISE;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
 fVulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

 // Per sample, and every one of them: MinSampleShading below one lets the driver shade fewer invocations
 // than there are samples and hand the same result to several of them, which is precisely the averaging
 // this pass exists to avoid.
 fVulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=fInstance.Renderer.SurfaceSampleCountFlagBits;
 fVulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=true;
 fVulkanGraphicsPipeline.MultisampleState.MinSampleShading:=1.0;
 fVulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
 fVulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=false;
 fVulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;

 fVulkanGraphicsPipeline.ColorBlendState.LogicOpEnable:=false;
 fVulkanGraphicsPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
 fVulkanGraphicsPipeline.ColorBlendState.BlendConstants[0]:=0.0;
 fVulkanGraphicsPipeline.ColorBlendState.BlendConstants[1]:=0.0;
 fVulkanGraphicsPipeline.ColorBlendState.BlendConstants[2]:=0.0;
 fVulkanGraphicsPipeline.ColorBlendState.BlendConstants[3]:=0.0;
 fVulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_DST_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_ONE,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_OP_ADD,
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

end;

procedure TpvScene3DRendererPassesVolumetricScatteringComposeRenderPass.ReleaseVolatileResources;
var InFlightFrameIndex:TpvInt32;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fScatteringImageViews[InFlightFrameIndex]);
  FreeAndNil(fExtinctionImageViews[InFlightFrameIndex]);
  FreeAndNil(fScatteringDepthImageViews[InFlightFrameIndex]);
  FreeAndNil(fMSAADepthImageViews[InFlightFrameIndex]);
  FreeAndNil(fFarScatteringImageViews[InFlightFrameIndex]);
  FreeAndNil(fFarExtinctionImageViews[InFlightFrameIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 fVulkanRenderPass:=nil;

 inherited ReleaseVolatileResources;

end;

procedure TpvScene3DRendererPassesVolumetricScatteringComposeRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvScene3DRendererPassesVolumetricScatteringComposeRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 // Read here rather than in Update, so a strength the game changes between frames lands on the frame it
 // was meant for. Every one of these numbers is the compute variant's - the constants come from the same
 // two units, so a change made there reaches both.
 if fInstance.VolumetricScatteringLegacyLook then begin
  fPushConstants.StrengthZNearDepthWeightSkyDepth:=TpvVector4.InlineableCreate(fInstance.VolumetricScatteringFactor,
                                                                               fInstance.ZNear,
                                                                               TpvScene3DRendererPassesVolumetricScatteringComposeComputePass.VolumetricScatteringUpsampleDepthWeightLegacy,
                                                                               TpvScene3DRendererPassesVolumetricScatteringRaymarchComputePass.VolumetricScatteringSkyDepth);
 end else begin
  fPushConstants.StrengthZNearDepthWeightSkyDepth:=TpvVector4.InlineableCreate(fInstance.VolumetricScatteringFactor,
                                                                               fInstance.ZNear,
                                                                               TpvScene3DRendererPassesVolumetricScatteringComposeComputePass.VolumetricScatteringUpsampleDepthWeight,
                                                                               TpvScene3DRendererPassesVolumetricScatteringRaymarchComputePass.VolumetricScatteringSkyDepth);
 end;

 fPushConstants.FlagsSampleCountSpare.x:=0;
 if fInstance.VolumetricScatteringLegacyLook then begin
  fPushConstants.FlagsSampleCountSpare.x:=fPushConstants.FlagsSampleCountSpare.x or TpvScene3DRendererPassesVolumetricScatteringComposeComputePass.VolumetricScatteringComposeFlagLegacyLook;
 end;
 if fInstance.VolumetricScatteringShowScatteringOnly then begin
  fPushConstants.FlagsSampleCountSpare.x:=fPushConstants.FlagsSampleCountSpare.x or TpvScene3DRendererPassesVolumetricScatteringComposeComputePass.VolumetricScatteringComposeFlagShowScatteringOnly;
 end;
 if fInstance.VolumetricScatteringShowExtinctionOnly then begin
  fPushConstants.FlagsSampleCountSpare.x:=fPushConstants.FlagsSampleCountSpare.x or TpvScene3DRendererPassesVolumetricScatteringComposeComputePass.VolumetricScatteringComposeFlagShowExtinctionOnly;
 end;
 if fInstance.VolumetricScatteringEnabled then begin
  fPushConstants.FlagsSampleCountSpare.x:=fPushConstants.FlagsSampleCountSpare.x or TpvScene3DRendererPassesVolumetricScatteringComposeComputePass.VolumetricScatteringComposeFlagEnabled;
 end;
 if fInstance.VolumetricScatteringSkyTapSearch then begin
  fPushConstants.FlagsSampleCountSpare.x:=fPushConstants.FlagsSampleCountSpare.x or TpvScene3DRendererPassesVolumetricScatteringComposeComputePass.VolumetricScatteringComposeFlagSkyTapSearch;
 end;
 // How many samples the raw depth carries. This variant reads one of them per invocation rather than all
 // of them at once, so it never needs the count - it is filled in all the same, because the push constant
 // block is shared with the compute variant and a lane that means one thing in one of them and nothing in
 // the other is how the two start to drift apart.
 fPushConstants.FlagsSampleCountSpare.y:=TpvUInt32(Max(1,fInstance.Renderer.CountSurfaceMSAASamples));
 fPushConstants.FlagsSampleCountSpare.z:=0;
 fPushConstants.FlagsSampleCountSpare.w:=0;

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      fVulkanPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,0,nil);

 aCommandBuffer.CmdPushConstants(fVulkanPipelineLayout.Handle,
                                 TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_FRAGMENT_BIT),
                                 0,
                                 SizeOf(TpvScene3DRendererPassesVolumetricScatteringComposeRenderPass.TPushConstants),
                                 @fPushConstants);

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);

 if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
  fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.BeginBreadcrumb(aCommandBuffer.Handle,TpvVulkanBreadcrumbType.Draw,'VolumetricScatteringComposePerSample');
 end;

 aCommandBuffer.CmdDraw(3,1,0,0);

 if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
  fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.EndBreadcrumb(aCommandBuffer.Handle);
 end;

end;

end.
