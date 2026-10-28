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
unit PasVulkan.Scene3D.Renderer.FrameGenerationBackend.FSR3;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

// AMD FidelityFX Super Resolution 3.1 frame generation as a frame generation backend.
//
// The GPU side are AMD's own GLSL passes of the optical flow and of the frame interpolation from the
// FidelityFX SDK v1.1.4, unchanged, in assets/shaders/scene3d/fsr3 (see the README.txt there). What is here
// is the host side, a port of the SDK's sdk/src/components/opticalflow/ffx_opticalflow.cpp and
// sdk/src/components/frameinterpolation/ffx_frameinterpolation.cpp, together with the part of
// sdk/src/components/fsr3/ffx_fsr3.cpp that ties the two together: the internal resources, the constants and
// the order of the dispatches. It stays close to those files on purpose, in names and in the order things
// happen, so that a newer FSR 3.1.x can be followed by comparing them.
//
// What differs from the SDK is what its backend and its swapchain do, and the way the frames come in:
//
// - Every internal image lives in VK_IMAGE_LAYOUT_GENERAL for good, a global memory barrier stands between
//   each two dispatches, and nothing is aliased.
// - The SDK copies every presented frame into an internal image to have it as the previous one for the next
//   interval. The ring of the compositor already holds the previous rendered frame, which is taken directly.
// - The SDK prepares a rendered frame for the interpolation (ffxFrameInterpolationPrepare: the dilated
//   depth and motion vectors) while it is rendered. Here it is done right before the interpolation, from the
//   copies of depth and motion vectors in the ring, which comes to the same.
// - The color is linear, at 1.0 for diffuse white, and goes to FSR as scRGB, with the peak brightness of
//   the output as the maximum luminance, so that both the optical flow and the interpolation see it as the
//   luminance from zero to one they expect.
// - The motion vectors are taken at render resolution and without jitter, and with the frame's jitter given
//   separately, as for the temporal upscaler backends.
//
// FSR 3.1 interpolates exactly one frame, halfway between two rendered ones.

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Scene3D.Renderer.Globals,
     PasVulkan.Scene3D.Renderer.FrameGenerationBackend;

type { TpvScene3DRendererFrameGenerationBackendFSR3 }
     TpvScene3DRendererFrameGenerationBackendFSR3=class(TpvScene3DRendererFrameGenerationBackend)
      public
       const CountOpticalFlowPyramidLevels=7;
             CountInpaintingPyramidMipViews=13;
             OpticalFlowBlockSize=8;
             HistogramBins=256;
             HistogramsPerDim=3;
             HistogramShifts=3;
             // The constant buffer of an in-flight frame holds every set of constants of the frame, one in
             // each of these slots: the optical flow has one per pyramid level, as the SDK stages them anew
             // for each level.
             ConstantBufferSlotSize=256;
             ConstantSlotFrameInterpolation=0;
             ConstantSlotGameVectorFieldInpaintingPyramid=1;
             ConstantSlotInpaintingPyramid=2;
             ConstantSlotOpticalFlowSPD=3;
             ConstantSlotOpticalFlow=4;
             CountConstantSlots=ConstantSlotOpticalFlow+CountOpticalFlowPyramidLevels;
             // FFX_BACKBUFFER_TRANSFER_FUNCTION_SCRGB
             TransferFunctionSCRGB=2;
             // FFX_FRAMEINTERPOLATION_DISPATCH_DRAW_DEBUG_VIEW
             DispatchDrawDebugView=4;
             // The most times a pass is dispatched in one interval: the search and the filter of the optical
             // flow once per pyramid level.
             MaximumCountPassUses=CountOpticalFlowPyramidLevels;
       type // The passes as they are dispatched, first those of the optical flow, then those of the frame
            // interpolation. DebugView draws, when asked for, views of the internal state over the output.
            TPass=
             (
              OpticalFlowPrepareLuma,
              OpticalFlowComputeLuminancePyramid,
              OpticalFlowGenerateSCDHistogram,
              OpticalFlowComputeSCDDivergence,
              OpticalFlowComputeOpticalFlowAdvancedV5,
              OpticalFlowFilterOpticalFlowV5,
              OpticalFlowScaleOpticalFlowAdvancedV5,
              ReconstructAndDilate,
              Setup,
              ReconstructPreviousDepth,
              GameMotionVectorField,
              OpticalFlowVectorField,
              DisocclusionMask,
              Interpolation,
              InpaintingPyramid,
              Inpainting,
              GameVectorFieldInpaintingPyramid,
              DebugView
             );
            // The internal images apart from the two pyramids of the optical flow, which are arrays of their
            // own. DilatedDepth, DilatedMotionVectors and ReconstructedPreviousNearestDepth are the three the
            // SDK shares with its upscaler, OpticalFlowVector and OpticalFlowSCDOutput the two it shares
            // between optical flow and frame interpolation. OpticalFlowConfidence stands in for the
            // confidence the SDK leaves unset, which its backend then reads as zero.
            TInternalImage=
             (
              DilatedDepth,
              DilatedMotionVectors,
              ReconstructedPreviousNearestDepth,
              ReconstructedDepthInterpolatedFrame,
              GameMotionVectorFieldX,
              GameMotionVectorFieldY,
              OpticalFlowMotionVectorFieldX,
              OpticalFlowMotionVectorFieldY,
              DisocclusionMask,
              InpaintingPyramid,
              DefaultDistortionField,
              OpticalFlowVector,
              OpticalFlowSCDOutput,
              OpticalFlowConfidence,
              OpticalFlowSCDHistogram,
              OpticalFlowSCDPreviousHistogram,
              OpticalFlowSCDTemp
             );
            // What a binding of a pass refers to, as the SDK's resource and binding identifiers. Which image
            // that is, is set before each dispatch, as the SDK does with its srvBindings and uavBindings.
            TResource=
             (
              InputColor,
              InputDepth,
              InputMotionVectors,
              PreviousInterpolationSource,
              CurrentInterpolationSource,
              PresentBackbuffer,
              Output,
              DilatedDepth,
              DilatedMotionVectors,
              ReconstructedDepthPreviousFrame,
              ReconstructedDepthInterpolatedFrame,
              DisocclusionMask,
              GameMotionVectorFieldX,
              GameMotionVectorFieldY,
              OpticalFlowMotionVectorFieldX,
              OpticalFlowMotionVectorFieldY,
              InpaintingPyramid,
              InpaintingPyramidMip,
              DistortionField,
              Counters,
              OpticalFlowVector,
              OpticalFlowConfidence,
              OpticalFlowSceneChangeDetection,
              OpticalFlowInput,
              OpticalFlowInputLevel,
              OpticalFlowPreviousInput,
              OpticalFlowSCDHistogram,
              OpticalFlowSCDPreviousHistogram,
              OpticalFlowSCDTemp,
              OpticalFlowSCDOutput,
              OpticalFlow,
              OpticalFlowNextLevel,
              OpticalFlowPrevious,
              ConstantsFrameInterpolation,
              ConstantsInpaintingPyramid,
              ConstantsOpticalFlow,
              ConstantsOpticalFlowSPD,
              LinearClampSampler
             );
            TBindingKind=
             (
              SampledImage,
              StorageImage,
              UniformBuffer,
              StorageBuffer,
              Sampler
             );
            // Index is the mip level of InpaintingPyramidMip and the pyramid level of OpticalFlowInputLevel.
            TBinding=record
             Binding:TpvUInt32;
             Kind:TBindingKind;
             Resource:TResource;
             Index:TpvInt32;
            end;
            TBindings=array of TBinding;
            // FrameInterpolationConstants of the SDK, and cbFI of the shaders, which is std140 with the same
            // layout.
            TFrameInterpolationConstants=packed record
             RenderSize:array[0..1] of TpvInt32;
             DisplaySize:array[0..1] of TpvInt32;
             DisplaySizeRcp:array[0..1] of TpvFloat;
             CameraNear:TpvFloat;
             CameraFar:TpvFloat;
             UpscalerTargetSize:array[0..1] of TpvInt32;
             Mode:TpvInt32;
             Reset:TpvInt32;
             DeviceToViewDepth:array[0..3] of TpvFloat;
             DeltaTime:TpvFloat;
             HUDLessAttachedFactor:TpvInt32;
             DistortionFieldSize:array[0..1] of TpvInt32;
             OpticalFlowScale:array[0..1] of TpvFloat;
             OpticalFlowBlockSize:TpvInt32;
             DispatchFlags:TpvUInt32;
             MaxRenderSize:array[0..1] of TpvInt32;
             OpticalFlowHalfResMode:TpvInt32;
             NumInstances:TpvInt32;
             InterpolationRectBase:array[0..1] of TpvInt32;
             InterpolationRectSize:array[0..1] of TpvInt32;
             DebugBarColor:array[0..2] of TpvFloat;
             BackBufferTransferFunction:TpvUInt32;
             MinMaxLuminance:array[0..1] of TpvFloat;
             TanHalfFOV:TpvFloat;
             Pad1:TpvFloat;
             Jitter:array[0..1] of TpvFloat;
             MotionVectorScale:array[0..1] of TpvFloat;
            end;
            PFrameInterpolationConstants=^TFrameInterpolationConstants;
            // InpaintingPyramidConstants and cbInpaintingPyramid.
            TInpaintingPyramidConstants=packed record
             Mips:TpvUInt32;
             NumWorkGroups:TpvUInt32;
             WorkGroupOffset:array[0..1] of TpvUInt32;
            end;
            PInpaintingPyramidConstants=^TInpaintingPyramidConstants;
            // OpticalflowConstants and cbOF.
            TOpticalFlowConstants=packed record
             InputLumaResolution:array[0..1] of TpvInt32;
             OpticalFlowPyramidLevel:TpvUInt32;
             OpticalFlowPyramidLevelCount:TpvUInt32;
             FrameIndex:TpvInt32;
             BackbufferTransferFunction:TpvUInt32;
             MinMaxLuminance:array[0..1] of TpvFloat;
            end;
            POpticalFlowConstants=^TOpticalFlowConstants;
            // OpticalFlowSpdConstants and cbOF_SPD.
            TOpticalFlowSPDConstants=packed record
             Mips:TpvUInt32;
             NumWorkGroups:TpvUInt32;
             WorkGroupOffset:array[0..1] of TpvUInt32;
             NumWorkGroupsOpticalFlowInputPyramid:TpvUInt32;
             Pad0:TpvUInt32;
             Pad1:TpvUInt32;
             Pad2:TpvUInt32;
            end;
            POpticalFlowSPDConstants=^TOpticalFlowSPDConstants;
            TInternalImageItem=record
             Image:TpvVulkanImage;
             MemoryBlock:TpvVulkanDeviceMemoryBlock;
             ImageView:TpvVulkanImageView;
             Format:TVkFormat;
             Width:TpvInt32;
             Height:TpvInt32;
             CountMipLevels:TpvInt32;
            end;
            PInternalImageItem=^TInternalImageItem;
            TPassItem=record
             Bindings:TBindings;
             ShaderModule:TpvVulkanShaderModule;
             ShaderStage:TpvVulkanPipelineShaderStage;
             DescriptorSetLayout:TpvVulkanDescriptorSetLayout;
             PipelineLayout:TpvVulkanPipelineLayout;
             Pipeline:TpvVulkanComputePipeline;
            end;
            PPassItem=^TPassItem;
            // The two halves of each optical flow pyramid, which swap from frame to frame, by pyramid level.
            TOpticalFlowPyramid=array[0..1,0..CountOpticalFlowPyramidLevels-1] of TInternalImageItem;
            TOpticalFlowTextureSizes=array[0..CountOpticalFlowPyramidLevels-1,0..1] of TpvInt32;
      private
       fPasses:array[TPass] of TPassItem;
       fInternalImages:array[TInternalImage] of TInternalImageItem;
       fOpticalFlowInputs:TOpticalFlowPyramid;
       fOpticalFlows:TOpticalFlowPyramid;
       fOpticalFlowTextureSizes:TOpticalFlowTextureSizes;
       fInpaintingPyramidMipViews:array[0..CountInpaintingPyramidMipViews-1] of TpvVulkanImageView;
       fLinearClampSampler:TpvVulkanSampler;
       fCounters:TpvVulkanBuffer;
       fConstantBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fDescriptorPool:TpvVulkanDescriptorPool;
       fDescriptorSets:array[0..MaxInFlightFrames-1,TPass,0..MaximumCountPassUses-1] of TpvVulkanDescriptorSet;
       fPassUses:array[TPass] of TpvInt32;
       fBoundImageViews:array[TResource] of TVkImageView;
       fBoundImageLayouts:array[TResource] of TVkImageLayout;
       fBoundConstantSlots:array[TResource] of TpvInt32;
       fBoundOpticalFlowInputHalf:TpvInt32;
       fFrameInterpolationConstants:TFrameInterpolationConstants;
       fOpticalFlowConstants:TOpticalFlowConstants;
       fFirstExecution:boolean;
       fPreviousFrameIndex:TpvInt64;
       fOpticalFlowResourceFrameIndex:TpvUInt32;
       fDebugBarColorIndex:TpvInt32;
       procedure CreateInternalImage(const aItem:PInternalImageItem;
                                     const aFormat:TVkFormat;
                                     const aWidth:TpvInt32;
                                     const aHeight:TpvInt32;
                                     const aCountMipLevels:TpvInt32;
                                     const aUsage:TVkImageUsageFlags;
                                     const aName:TpvUTF8String);
       procedure DestroyInternalImage(const aItem:PInternalImageItem);
       class function GetPassBindings(const aPass:TPass):TBindings; static;
       class function GetPassMaximumCountUses(const aPass:TPass):TpvInt32; static;
       function GetShaderFileName(const aPass:TPass):TpvUTF8String;
       function GetImageView(const aBinding:TBinding):TVkImageView;
       procedure BindImage(const aResource:TResource;const aImageView:TVkImageView;const aImageLayout:TVkImageLayout);
       procedure BindInternalImage(const aResource:TResource;const aInternalImage:TInternalImage);
       procedure SetupDeviceDepthToViewSpaceDepthParams(const aProjectionMatrix:TpvMatrix4x4);
       procedure GlobalBarrier(const aCommandBuffer:TpvVulkanCommandBuffer;
                               const aSrcStageMask:TVkPipelineStageFlags;
                               const aDstStageMask:TVkPipelineStageFlags;
                               const aSrcAccessMask:TVkAccessFlags;
                               const aDstAccessMask:TVkAccessFlags);
       procedure ClearImage(const aCommandBuffer:TpvVulkanCommandBuffer;const aItem:PInternalImageItem;const aR,aG,aB,aA:TpvFloat);
       procedure ClearImageUInt(const aCommandBuffer:TpvVulkanCommandBuffer;const aItem:PInternalImageItem;const aValue:TpvUInt32);
       procedure ScheduleDispatch(const aCommandBuffer:TpvVulkanCommandBuffer;
                                  const aInFlightFrameIndex:TpvSizeInt;
                                  const aPass:TPass;
                                  const aDispatchX:TpvUInt32;
                                  const aDispatchY:TpvUInt32;
                                  const aDispatchZ:TpvUInt32=1);
       procedure DispatchOpticalFlow(const aCommandBuffer:TpvVulkanCommandBuffer;
                                     const aInFlightFrameIndex:TpvSizeInt;
                                     const aDescription:TGenerateDescription;
                                     const aReset:boolean;
                                     const aMinLuminance:TpvFloat;
                                     const aMaxLuminance:TpvFloat);
       procedure DispatchFrameInterpolation(const aCommandBuffer:TpvVulkanCommandBuffer;
                                            const aInFlightFrameIndex:TpvSizeInt;
                                            const aDescription:TGenerateDescription;
                                            const aReset:boolean;
                                            const aMinLuminance:TpvFloat;
                                            const aMaxLuminance:TpvFloat);
      public
       // Whether a device can run FSR 3.1 frame generation at all: the extended storage formats the passes
       // declare, storage images read and written without a format, the subgroup arithmetic of the optical
       // flow's search on a subgroup of 32 or 64 lanes, which is all the search knows, and storage support
       // for every format of the internal images.
       class function IsSupported(const aDevice:TpvVulkanDevice):boolean; override;
       class function GetMaximumCountGeneratedFrames(const aDevice:TpvVulkanDevice):TpvInt32; override;
       class function NeedsDepthAndMotionVectors:boolean; override;
       constructor Create(const aDevice:TpvVulkanDevice;
                          const aPipelineCache:TpvVulkanPipelineCache;
                          const aCountInFlightFrames:TpvSizeInt;
                          const aWidth:TpvInt32;
                          const aHeight:TpvInt32;
                          const aRenderWidth:TpvInt32;
                          const aRenderHeight:TpvInt32;
                          const aColorFormat:TVkFormat;
                          const aInvertedDepth:boolean;
                          const aName:TpvUTF8String); override;
       destructor Destroy; override;
       function Generate(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex:TpvSizeInt;const aDescription:TGenerateDescription):boolean; override;
     end;

implementation

const FLT_EPSILON=1.192092896e-7;

      // debugBarColorSequence of the SDK.
      DebugBarColorSequence:array[0..6,0..2] of TpvFloat=
       (
        (0.0,1.0,1.0),   // teal
        (1.0,0.42,0.0),  // orange
        (0.0,0.16,1.0),  // blue
        (0.74,1.0,0.0),  // lime
        (0.68,0.0,1.0),  // purple
        (0.0,1.0,0.1),   // green
        (1.0,1.0,0.48)   // bright yellow
       );

// ffxSpdSetup of the SDK, for a rectangle at the origin. A negative mip count means as many as the
// rectangle has, at most twelve.
procedure SPDSetup(const aWidth,aHeight:TpvInt32;
                   const aMips:TpvInt32;
                   out aDispatchThreadGroupCountX:TpvUInt32;
                   out aDispatchThreadGroupCountY:TpvUInt32;
                   out aNumWorkGroups:TpvUInt32;
                   out aCountMips:TpvUInt32);
begin
 aDispatchThreadGroupCountX:=TpvUInt32(((Max(aWidth,1)-1) div 64)+1);
 aDispatchThreadGroupCountY:=TpvUInt32(((Max(aHeight,1)-1) div 64)+1);
 aNumWorkGroups:=aDispatchThreadGroupCountX*aDispatchThreadGroupCountY;
 if aMips>=0 then begin
  aCountMips:=TpvUInt32(aMips);
 end else begin
  aCountMips:=TpvUInt32(Min(TpvInt32(IntLog2(TpvUInt32(Max(Max(aWidth,aHeight),1)))),12));
 end;
end;

{ TpvScene3DRendererFrameGenerationBackendFSR3 }

class function TpvScene3DRendererFrameGenerationBackendFSR3.IsSupported(const aDevice:TpvVulkanDevice):boolean;
const StorageFormats:array[0..6] of TVkFormat=
       (
        VK_FORMAT_R8_UINT,
        VK_FORMAT_R8G8_UNORM,
        VK_FORMAT_R16G16_SINT,
        VK_FORMAT_R16G16_SFLOAT,
        VK_FORMAT_R16G16B16A16_SFLOAT,
        VK_FORMAT_R32_UINT,
        VK_FORMAT_R32_SFLOAT
       );
var Index:TpvSizeInt;
    FormatProperties:TVkFormatProperties;
    RequiredFeatures:TVkFormatFeatureFlags;
    SubgroupSize:TpvUInt32;
begin
 result:=assigned(aDevice) and
         (aDevice.PhysicalDevice.Features.shaderStorageImageExtendedFormats<>VK_FALSE) and
         (aDevice.PhysicalDevice.Features.shaderStorageImageReadWithoutFormat<>VK_FALSE) and
         (aDevice.PhysicalDevice.Features.shaderStorageImageWriteWithoutFormat<>VK_FALSE) and
         ((aDevice.PhysicalDevice.Vulkan11Properties.subgroupSupportedStages and TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT))<>0) and
         ((aDevice.PhysicalDevice.Vulkan11Properties.subgroupSupportedOperations and TVkSubgroupFeatureFlags(VK_SUBGROUP_FEATURE_BASIC_BIT))<>0) and
         ((aDevice.PhysicalDevice.Vulkan11Properties.subgroupSupportedOperations and TVkSubgroupFeatureFlags(VK_SUBGROUP_FEATURE_ARITHMETIC_BIT))<>0);
 if result then begin
  SubgroupSize:=aDevice.PhysicalDevice.Vulkan11Properties.subgroupSize;
  result:=(SubgroupSize=32) or (SubgroupSize=64);
 end;
 if result then begin
  RequiredFeatures:=TVkFormatFeatureFlags(VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT) or TVkFormatFeatureFlags(VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT);
  for Index:=Low(StorageFormats) to High(StorageFormats) do begin
   FormatProperties:=aDevice.PhysicalDevice.GetFormatProperties(StorageFormats[Index]);
   if (FormatProperties.optimalTilingFeatures and RequiredFeatures)<>RequiredFeatures then begin
    result:=false;
    break;
   end;
  end;
 end;
end;

class function TpvScene3DRendererFrameGenerationBackendFSR3.GetMaximumCountGeneratedFrames(const aDevice:TpvVulkanDevice):TpvInt32;
begin
 result:=1;
end;

class function TpvScene3DRendererFrameGenerationBackendFSR3.NeedsDepthAndMotionVectors:boolean;
begin
 result:=true;
end;

constructor TpvScene3DRendererFrameGenerationBackendFSR3.Create(const aDevice:TpvVulkanDevice;
                                                                const aPipelineCache:TpvVulkanPipelineCache;
                                                                const aCountInFlightFrames:TpvSizeInt;
                                                                const aWidth:TpvInt32;
                                                                const aHeight:TpvInt32;
                                                                const aRenderWidth:TpvInt32;
                                                                const aRenderHeight:TpvInt32;
                                                                const aColorFormat:TVkFormat;
                                                                const aInvertedDepth:boolean;
                                                                const aName:TpvUTF8String);
const StorageUsage=TVkImageUsageFlags(VK_IMAGE_USAGE_STORAGE_BIT) or
                   TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT) or
                   TVkImageUsageFlags(VK_IMAGE_USAGE_TRANSFER_DST_BIT);
      ReadOnlyUsage=TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT) or
                    TVkImageUsageFlags(VK_IMAGE_USAGE_TRANSFER_DST_BIT);
var Pass:TPass;
    PassItem:PPassItem;
    BindingIndex,InFlightFrameIndex,MipIndex,Half,Level,UseIndex:TpvSizeInt;
    Stream:TStream;
    CountSampledImages,CountStorageImages,CountUniformBuffers,CountStorageBuffers,CountSamplers,CountSets,CountUses:TpvUInt32;
    DescriptorType:TVkDescriptorType;
    HistogramWidth:TpvInt32;
begin

 inherited Create(aDevice,
                  aPipelineCache,
                  aCountInFlightFrames,
                  aWidth,
                  aHeight,
                  aRenderWidth,
                  aRenderHeight,
                  aColorFormat,
                  aInvertedDepth,
                  aName);

 FillChar(fPasses,SizeOf(fPasses),#0);
 FillChar(fInternalImages,SizeOf(fInternalImages),#0);
 FillChar(fOpticalFlowInputs,SizeOf(fOpticalFlowInputs),#0);
 FillChar(fOpticalFlows,SizeOf(fOpticalFlows),#0);
 FillChar(fInpaintingPyramidMipViews,SizeOf(fInpaintingPyramidMipViews),#0);
 FillChar(fConstantBuffers,SizeOf(fConstantBuffers),#0);
 FillChar(fDescriptorSets,SizeOf(fDescriptorSets),#0);
 FillChar(fPassUses,SizeOf(fPassUses),#0);
 FillChar(fBoundImageViews,SizeOf(fBoundImageViews),#0);
 FillChar(fBoundImageLayouts,SizeOf(fBoundImageLayouts),#0);
 FillChar(fBoundConstantSlots,SizeOf(fBoundConstantSlots),#0);
 FillChar(fFrameInterpolationConstants,SizeOf(TFrameInterpolationConstants),#0);
 FillChar(fOpticalFlowConstants,SizeOf(TOpticalFlowConstants),#0);

 fBoundOpticalFlowInputHalf:=0;

 fFirstExecution:=true;
 fPreviousFrameIndex:=-1;
 fOpticalFlowResourceFrameIndex:=0;
 fDebugBarColorIndex:=0;

 // The defaults the SDK sets at context creation.
 fFrameInterpolationConstants.MaxRenderSize[0]:=fRenderWidth;
 fFrameInterpolationConstants.MaxRenderSize[1]:=fRenderHeight;
 fFrameInterpolationConstants.DisplaySize[0]:=fWidth;
 fFrameInterpolationConstants.DisplaySize[1]:=fHeight;
 fFrameInterpolationConstants.DisplaySizeRcp[0]:=1.0/fWidth;
 fFrameInterpolationConstants.DisplaySizeRcp[1]:=1.0/fHeight;
 fFrameInterpolationConstants.InterpolationRectBase[0]:=0;
 fFrameInterpolationConstants.InterpolationRectBase[1]:=0;
 fFrameInterpolationConstants.InterpolationRectSize[0]:=fWidth;
 fFrameInterpolationConstants.InterpolationRectSize[1]:=fHeight;

 fOpticalFlowConstants.InputLumaResolution[0]:=fWidth;
 fOpticalFlowConstants.InputLumaResolution[1]:=fHeight;

 // The internal resources of the frame interpolation, with the formats and sizes of the SDK's
 // internalSurfaceDesc and of its shared resource descriptions.
 CreateInternalImage(@fInternalImages[TInternalImage.DilatedDepth],VK_FORMAT_R32_SFLOAT,fRenderWidth,fRenderHeight,1,StorageUsage,'DilatedDepth');
 CreateInternalImage(@fInternalImages[TInternalImage.DilatedMotionVectors],VK_FORMAT_R16G16_SFLOAT,fRenderWidth,fRenderHeight,1,StorageUsage,'DilatedMotionVectors');
 CreateInternalImage(@fInternalImages[TInternalImage.ReconstructedPreviousNearestDepth],VK_FORMAT_R32_UINT,fRenderWidth,fRenderHeight,1,StorageUsage,'ReconstructedPreviousNearestDepth');
 CreateInternalImage(@fInternalImages[TInternalImage.ReconstructedDepthInterpolatedFrame],VK_FORMAT_R32_UINT,fRenderWidth,fRenderHeight,1,StorageUsage,'ReconstructedDepthInterpolatedFrame');
 CreateInternalImage(@fInternalImages[TInternalImage.GameMotionVectorFieldX],VK_FORMAT_R32_UINT,fRenderWidth,fRenderHeight,1,StorageUsage,'GameMotionVectorFieldX');
 CreateInternalImage(@fInternalImages[TInternalImage.GameMotionVectorFieldY],VK_FORMAT_R32_UINT,fRenderWidth,fRenderHeight,1,StorageUsage,'GameMotionVectorFieldY');
 CreateInternalImage(@fInternalImages[TInternalImage.OpticalFlowMotionVectorFieldX],VK_FORMAT_R32_UINT,fRenderWidth,fRenderHeight,1,StorageUsage,'OpticalFlowMotionVectorFieldX');
 CreateInternalImage(@fInternalImages[TInternalImage.OpticalFlowMotionVectorFieldY],VK_FORMAT_R32_UINT,fRenderWidth,fRenderHeight,1,StorageUsage,'OpticalFlowMotionVectorFieldY');
 CreateInternalImage(@fInternalImages[TInternalImage.DisocclusionMask],VK_FORMAT_R8G8_UNORM,fRenderWidth,fRenderHeight,1,StorageUsage,'DisocclusionMask');
 CreateInternalImage(@fInternalImages[TInternalImage.InpaintingPyramid],VK_FORMAT_R16G16B16A16_SFLOAT,fWidth div 2,fHeight div 2,0,StorageUsage,'InpaintingPyramid');
 CreateInternalImage(@fInternalImages[TInternalImage.DefaultDistortionField],VK_FORMAT_R8G8_UNORM,1,1,1,ReadOnlyUsage,'DefaultDistortionField');

 // The internal resources of the optical flow, with the formats and sizes of its internalSurfaceDesc and of
 // its shared resource descriptions. The optical flow works at display resolution, its vectors are for blocks
 // of eight by eight pixels, and every level of both pyramids halves the one before.
 fOpticalFlowTextureSizes[0,0]:=(fWidth+(OpticalFlowBlockSize-1)) div OpticalFlowBlockSize;
 fOpticalFlowTextureSizes[0,1]:=(fHeight+(OpticalFlowBlockSize-1)) div OpticalFlowBlockSize;
 for Level:=1 to CountOpticalFlowPyramidLevels-1 do begin
  fOpticalFlowTextureSizes[Level,0]:=(fOpticalFlowTextureSizes[Level-1,0]+1) div 2;
  fOpticalFlowTextureSizes[Level,1]:=(fOpticalFlowTextureSizes[Level-1,1]+1) div 2;
 end;
 for Half:=0 to 1 do begin
  for Level:=0 to CountOpticalFlowPyramidLevels-1 do begin
   CreateInternalImage(@fOpticalFlowInputs[Half,Level],
                       VK_FORMAT_R8_UINT,
                       fWidth shr Level,
                       fHeight shr Level,
                       1,
                       StorageUsage,
                       'OpticalFlowInput'+IntToStr(Half+1)+'Level'+IntToStr(Level));
   CreateInternalImage(@fOpticalFlows[Half,Level],
                       VK_FORMAT_R16G16_SINT,
                       fOpticalFlowTextureSizes[Level,0],
                       fOpticalFlowTextureSizes[Level,1],
                       1,
                       StorageUsage,
                       'OpticalFlow'+IntToStr(Half+1)+'Level'+IntToStr(Level));
  end;
 end;
 HistogramWidth:=HistogramBins*(HistogramsPerDim*HistogramsPerDim);
 CreateInternalImage(@fInternalImages[TInternalImage.OpticalFlowSCDHistogram],VK_FORMAT_R32_UINT,HistogramWidth,1,1,StorageUsage,'OpticalFlowSCDHistogram');
 CreateInternalImage(@fInternalImages[TInternalImage.OpticalFlowSCDPreviousHistogram],VK_FORMAT_R32_SFLOAT,HistogramWidth,1,1,StorageUsage,'OpticalFlowSCDPreviousHistogram');
 CreateInternalImage(@fInternalImages[TInternalImage.OpticalFlowSCDTemp],VK_FORMAT_R32_UINT,3,1,1,StorageUsage,'OpticalFlowSCDTemp');
 CreateInternalImage(@fInternalImages[TInternalImage.OpticalFlowVector],VK_FORMAT_R16G16_SINT,fOpticalFlowTextureSizes[0,0],fOpticalFlowTextureSizes[0,1],1,StorageUsage,'OpticalFlowVector');
 CreateInternalImage(@fInternalImages[TInternalImage.OpticalFlowSCDOutput],VK_FORMAT_R32_UINT,3,1,1,StorageUsage,'OpticalFlowSCDOutput');
 CreateInternalImage(@fInternalImages[TInternalImage.OpticalFlowConfidence],VK_FORMAT_R32_UINT,fOpticalFlowTextureSizes[0,0],fOpticalFlowTextureSizes[0,1],1,StorageUsage,'OpticalFlowConfidence');

 // The storage views of the inpainting pyramid's mip levels. Where the pyramid has fewer than thirteen,
 // the last one stands in for the missing ones, which SPD then never writes, since it stops at the mip
 // count.
 for MipIndex:=0 to CountInpaintingPyramidMipViews-1 do begin
  fInpaintingPyramidMipViews[MipIndex]:=TpvVulkanImageView.Create(fDevice,
                                                                  fInternalImages[TInternalImage.InpaintingPyramid].Image,
                                                                  VK_IMAGE_VIEW_TYPE_2D,
                                                                  VK_FORMAT_R16G16B16A16_SFLOAT,
                                                                  VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                  VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                  VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                  VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                  TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                  Min(MipIndex,fInternalImages[TInternalImage.InpaintingPyramid].CountMipLevels-1),
                                                                  1,
                                                                  0,
                                                                  1);
  fDevice.DebugUtils.SetObjectName(fInpaintingPyramidMipViews[MipIndex].Handle,VK_OBJECT_TYPE_IMAGE_VIEW,fName+'.InpaintingPyramidMipView['+IntToStr(MipIndex)+']');
 end;

 // s_LinearClamp, which every pass of the frame interpolation declares; the optical flow has no samplers.
 fLinearClampSampler:=TpvVulkanSampler.Create(fDevice,
                                              TVkFilter.VK_FILTER_LINEAR,
                                              TVkFilter.VK_FILTER_LINEAR,
                                              TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                              VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                              VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                              VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                              0.0,
                                              false,
                                              1.0,
                                              false,
                                              VK_COMPARE_OP_ALWAYS,
                                              0.0,
                                              VK_LOD_CLAMP_NONE,
                                              VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK,
                                              false);
 fDevice.DebugUtils.SetObjectName(fLinearClampSampler.Handle,VK_OBJECT_TYPE_SAMPLER,fName+'.LinearClampSampler');

 // The counters of the frame interpolation, a structured buffer of two unsigned integers in the SDK: the
 // atomic counter of SPD and the count of frames since the last reset.
 fCounters:=TpvVulkanBuffer.Create(fDevice,
                                   16,
                                   TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT),
                                   TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                   [],
                                   TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                   0,
                                   0,
                                   0,
                                   0,
                                   0,
                                   0,
                                   0,
                                   [],
                                   0,
                                   pvAllocationGroupIDScene3DDynamic,
                                   fName+'.Counters');

 // The constants, one buffer per in-flight frame, since a frame's are still read by the GPU while the next
 // frame's are written.
 for InFlightFrameIndex:=0 to fCountInFlightFrames-1 do begin
  fConstantBuffers[InFlightFrameIndex]:=TpvVulkanBuffer.Create(fDevice,
                                                               CountConstantSlots*ConstantBufferSlotSize,
                                                               TVkBufferUsageFlags(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT),
                                                               TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                               [],
                                                               TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                               TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                               0,
                                                               0,
                                                               0,
                                                               0,
                                                               0,
                                                               0,
                                                               [TpvVulkanBufferFlag.PersistentMappedIfPossible],
                                                               0,
                                                               pvAllocationGroupIDScene3DDynamic,
                                                               fName+'.ConstantBuffers['+IntToStr(InFlightFrameIndex)+']');
 end;

 // The passes: bindings, shader, descriptor set layout, pipeline. The bindings and their numbers are the
 // FFX_FRAMEINTERPOLATION_BIND_* and FFX_OPTICALFLOW_BIND_* defines at the top of each pass's GLSL file.
 CountSampledImages:=0;
 CountStorageImages:=0;
 CountUniformBuffers:=0;
 CountStorageBuffers:=0;
 CountSamplers:=0;
 CountSets:=0;
 for Pass:=Low(TPass) to High(TPass) do begin

  PassItem:=@fPasses[Pass];

  PassItem^.Bindings:=GetPassBindings(Pass);

  CountUses:=GetPassMaximumCountUses(Pass);

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile(GetShaderFileName(Pass));
  try
   PassItem^.ShaderModule:=TpvVulkanShaderModule.Create(fDevice,Stream);
  finally
   Stream.Free;
  end;
  fDevice.DebugUtils.SetObjectName(PassItem^.ShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,fName+'.ShaderModule['+IntToStr(ord(Pass))+']');

  PassItem^.ShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,PassItem^.ShaderModule,'main');

  PassItem^.DescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fDevice);
  for BindingIndex:=0 to length(PassItem^.Bindings)-1 do begin
   case PassItem^.Bindings[BindingIndex].Kind of
    TBindingKind.SampledImage:begin
     DescriptorType:=VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE;
     inc(CountSampledImages,CountUses);
    end;
    TBindingKind.StorageImage:begin
     DescriptorType:=VK_DESCRIPTOR_TYPE_STORAGE_IMAGE;
     inc(CountStorageImages,CountUses);
    end;
    TBindingKind.UniformBuffer:begin
     DescriptorType:=VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
     inc(CountUniformBuffers,CountUses);
    end;
    TBindingKind.StorageBuffer:begin
     DescriptorType:=VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
     inc(CountStorageBuffers,CountUses);
    end;
    else {TBindingKind.Sampler:}begin
     DescriptorType:=VK_DESCRIPTOR_TYPE_SAMPLER;
     inc(CountSamplers,CountUses);
    end;
   end;
   PassItem^.DescriptorSetLayout.AddBinding(PassItem^.Bindings[BindingIndex].Binding,
                                            DescriptorType,
                                            1,
                                            TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                            []);
  end;
  PassItem^.DescriptorSetLayout.Initialize;
  fDevice.DebugUtils.SetObjectName(PassItem^.DescriptorSetLayout.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,fName+'.DescriptorSetLayout['+IntToStr(ord(Pass))+']');

  PassItem^.PipelineLayout:=TpvVulkanPipelineLayout.Create(fDevice);
  PassItem^.PipelineLayout.AddDescriptorSetLayout(PassItem^.DescriptorSetLayout);
  PassItem^.PipelineLayout.Initialize;
  fDevice.DebugUtils.SetObjectName(PassItem^.PipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,fName+'.PipelineLayout['+IntToStr(ord(Pass))+']');

  PassItem^.Pipeline:=TpvVulkanComputePipeline.Create(fDevice,
                                                      fPipelineCache,
                                                      0,
                                                      PassItem^.ShaderStage,
                                                      PassItem^.PipelineLayout,
                                                      nil,
                                                      0);
  fDevice.DebugUtils.SetObjectName(PassItem^.Pipeline.Handle,VK_OBJECT_TYPE_PIPELINE,fName+'.Pipeline['+IntToStr(ord(Pass))+']');

  inc(CountSets,CountUses);

 end;

 // One descriptor set per in-flight frame and dispatch of a pass, written while the command buffer of that
 // in-flight frame is recorded, which is after its previous use has finished, since the images from the
 // ring and the halves of the optical flow pyramids differ from interval to interval.
 fDescriptorPool:=TpvVulkanDescriptorPool.Create(fDevice,
                                                 TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                 CountSets*TpvUInt32(fCountInFlightFrames));
 if CountSampledImages>0 then begin
  fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE,CountSampledImages*TpvUInt32(fCountInFlightFrames));
 end;
 if CountStorageImages>0 then begin
  fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,CountStorageImages*TpvUInt32(fCountInFlightFrames));
 end;
 if CountUniformBuffers>0 then begin
  fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,CountUniformBuffers*TpvUInt32(fCountInFlightFrames));
 end;
 if CountStorageBuffers>0 then begin
  fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,CountStorageBuffers*TpvUInt32(fCountInFlightFrames));
 end;
 if CountSamplers>0 then begin
  fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_SAMPLER,CountSamplers*TpvUInt32(fCountInFlightFrames));
 end;
 fDescriptorPool.Initialize;
 fDevice.DebugUtils.SetObjectName(fDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,fName+'.DescriptorPool');

 for InFlightFrameIndex:=0 to fCountInFlightFrames-1 do begin
  for Pass:=Low(TPass) to High(TPass) do begin
   for UseIndex:=0 to GetPassMaximumCountUses(Pass)-1 do begin
    fDescriptorSets[InFlightFrameIndex,Pass,UseIndex]:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fPasses[Pass].DescriptorSetLayout);
    fDevice.DebugUtils.SetObjectName(fDescriptorSets[InFlightFrameIndex,Pass,UseIndex].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,fName+'.DescriptorSet['+IntToStr(InFlightFrameIndex)+','+IntToStr(ord(Pass))+','+IntToStr(UseIndex)+']');
   end;
  end;
 end;

end;

destructor TpvScene3DRendererFrameGenerationBackendFSR3.Destroy;
var Pass:TPass;
    InternalImage:TInternalImage;
    InFlightFrameIndex,MipIndex,Half,Level,UseIndex:TpvSizeInt;
begin

 for InFlightFrameIndex:=0 to fCountInFlightFrames-1 do begin
  for Pass:=Low(TPass) to High(TPass) do begin
   for UseIndex:=0 to MaximumCountPassUses-1 do begin
    FreeAndNil(fDescriptorSets[InFlightFrameIndex,Pass,UseIndex]);
   end;
  end;
 end;
 FreeAndNil(fDescriptorPool);

 for Pass:=Low(TPass) to High(TPass) do begin
  FreeAndNil(fPasses[Pass].Pipeline);
  FreeAndNil(fPasses[Pass].PipelineLayout);
  FreeAndNil(fPasses[Pass].DescriptorSetLayout);
  FreeAndNil(fPasses[Pass].ShaderStage);
  FreeAndNil(fPasses[Pass].ShaderModule);
  fPasses[Pass].Bindings:=nil;
 end;

 for InFlightFrameIndex:=0 to fCountInFlightFrames-1 do begin
  FreeAndNil(fConstantBuffers[InFlightFrameIndex]);
 end;

 FreeAndNil(fCounters);

 FreeAndNil(fLinearClampSampler);

 for MipIndex:=0 to CountInpaintingPyramidMipViews-1 do begin
  FreeAndNil(fInpaintingPyramidMipViews[MipIndex]);
 end;

 for Half:=0 to 1 do begin
  for Level:=0 to CountOpticalFlowPyramidLevels-1 do begin
   DestroyInternalImage(@fOpticalFlows[Half,Level]);
   DestroyInternalImage(@fOpticalFlowInputs[Half,Level]);
  end;
 end;

 for InternalImage:=Low(TInternalImage) to High(TInternalImage) do begin
  DestroyInternalImage(@fInternalImages[InternalImage]);
 end;

 inherited Destroy;

end;

procedure TpvScene3DRendererFrameGenerationBackendFSR3.CreateInternalImage(const aItem:PInternalImageItem;
                                                                           const aFormat:TVkFormat;
                                                                           const aWidth:TpvInt32;
                                                                           const aHeight:TpvInt32;
                                                                           const aCountMipLevels:TpvInt32;
                                                                           const aUsage:TVkImageUsageFlags;
                                                                           const aName:TpvUTF8String);
var MemoryRequirements:TVkMemoryRequirements;
    CountMipLevels:TpvInt32;
begin

 aItem^.Format:=aFormat;
 aItem^.Width:=Max(1,aWidth);
 aItem^.Height:=Max(1,aHeight);

 // A mip count of zero means the full chain down to one texel, as in the SDK.
 if aCountMipLevels<=0 then begin
  CountMipLevels:=IntLog2(Max(aItem^.Width,aItem^.Height))+1;
 end else begin
  CountMipLevels:=aCountMipLevels;
 end;
 aItem^.CountMipLevels:=CountMipLevels;

 aItem^.Image:=TpvVulkanImage.Create(fDevice,
                                     0,
                                     VK_IMAGE_TYPE_2D,
                                     aFormat,
                                     aItem^.Width,
                                     aItem^.Height,
                                     1,
                                     CountMipLevels,
                                     1,
                                     VK_SAMPLE_COUNT_1_BIT,
                                     VK_IMAGE_TILING_OPTIMAL,
                                     aUsage,
                                     VK_SHARING_MODE_EXCLUSIVE,
                                     0,
                                     nil,
                                     VK_IMAGE_LAYOUT_UNDEFINED);
 fDevice.DebugUtils.SetObjectName(aItem^.Image.Handle,VK_OBJECT_TYPE_IMAGE,fName+'.'+aName);

 fDevice.Commands.GetImageMemoryRequirements(fDevice.Handle,aItem^.Image.Handle,@MemoryRequirements);

 aItem^.MemoryBlock:=fDevice.MemoryManager.AllocateMemoryBlock([],
                                                               MemoryRequirements.size,
                                                               MemoryRequirements.alignment,
                                                               MemoryRequirements.memoryTypeBits,
                                                               TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                               0,
                                                               0,
                                                               0,
                                                               0,
                                                               0,
                                                               0,
                                                               0,
                                                               TpvVulkanDeviceMemoryAllocationType.ImageOptimal,
                                                               nil,
                                                               pvAllocationGroupIDScene3DDynamic,
                                                               fName+'.'+aName+'.Memory');

 VulkanCheckResult(fDevice.Commands.BindImageMemory(fDevice.Handle,
                                                    aItem^.Image.Handle,
                                                    aItem^.MemoryBlock.MemoryChunk.Handle,
                                                    aItem^.MemoryBlock.Offset));

 aItem^.ImageView:=TpvVulkanImageView.Create(fDevice,
                                             aItem^.Image,
                                             VK_IMAGE_VIEW_TYPE_2D,
                                             aFormat,
                                             VK_COMPONENT_SWIZZLE_IDENTITY,
                                             VK_COMPONENT_SWIZZLE_IDENTITY,
                                             VK_COMPONENT_SWIZZLE_IDENTITY,
                                             VK_COMPONENT_SWIZZLE_IDENTITY,
                                             TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                             0,
                                             CountMipLevels,
                                             0,
                                             1);
 fDevice.DebugUtils.SetObjectName(aItem^.ImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,fName+'.'+aName+'.ImageView');

end;

procedure TpvScene3DRendererFrameGenerationBackendFSR3.DestroyInternalImage(const aItem:PInternalImageItem);
begin
 FreeAndNil(aItem^.ImageView);
 FreeAndNil(aItem^.Image);
 if assigned(aItem^.MemoryBlock) then begin
  aItem^.MemoryBlock.Free;
  aItem^.MemoryBlock:=nil;
 end;
end;

class function TpvScene3DRendererFrameGenerationBackendFSR3.GetPassBindings(const aPass:TPass):TBindings;
var Count,MipIndex:TpvSizeInt;
 procedure Add(const aBinding:TpvUInt32;const aKind:TBindingKind;const aResource:TResource;const aIndex:TpvInt32=0);
 begin
  if Count>=length(result) then begin
   SetLength(result,(Count+1)*2);
  end;
  result[Count].Binding:=aBinding;
  result[Count].Kind:=aKind;
  result[Count].Resource:=aResource;
  result[Count].Index:=aIndex;
  inc(Count);
 end;
begin
 result:=nil;
 Count:=0;
 case aPass of
  TPass.OpticalFlowPrepareLuma:begin
   Add(0,TBindingKind.SampledImage,TResource.InputColor);
   Add(1,TBindingKind.StorageImage,TResource.OpticalFlowInput);
   Add(2,TBindingKind.UniformBuffer,TResource.ConstantsOpticalFlow);
  end;
  TPass.OpticalFlowComputeLuminancePyramid:begin
   Add(0,TBindingKind.StorageImage,TResource.OpticalFlowInput);
   Add(1,TBindingKind.StorageImage,TResource.OpticalFlowInputLevel,1);
   Add(2,TBindingKind.StorageImage,TResource.OpticalFlowInputLevel,2);
   Add(3,TBindingKind.StorageImage,TResource.OpticalFlowInputLevel,3);
   Add(4,TBindingKind.StorageImage,TResource.OpticalFlowInputLevel,4);
   Add(5,TBindingKind.StorageImage,TResource.OpticalFlowInputLevel,5);
   Add(6,TBindingKind.StorageImage,TResource.OpticalFlowInputLevel,6);
   Add(7,TBindingKind.UniformBuffer,TResource.ConstantsOpticalFlow);
   Add(8,TBindingKind.UniformBuffer,TResource.ConstantsOpticalFlowSPD);
  end;
  TPass.OpticalFlowGenerateSCDHistogram:begin
   Add(0,TBindingKind.SampledImage,TResource.OpticalFlowInput);
   Add(1,TBindingKind.StorageImage,TResource.OpticalFlowSCDHistogram);
   Add(2,TBindingKind.UniformBuffer,TResource.ConstantsOpticalFlow);
  end;
  TPass.OpticalFlowComputeSCDDivergence:begin
   Add(0,TBindingKind.StorageImage,TResource.OpticalFlowSCDHistogram);
   Add(1,TBindingKind.StorageImage,TResource.OpticalFlowSCDPreviousHistogram);
   Add(2,TBindingKind.StorageImage,TResource.OpticalFlowSCDTemp);
   Add(3,TBindingKind.StorageImage,TResource.OpticalFlowSCDOutput);
   Add(4,TBindingKind.UniformBuffer,TResource.ConstantsOpticalFlow);
  end;
  TPass.OpticalFlowComputeOpticalFlowAdvancedV5:begin
   Add(0,TBindingKind.SampledImage,TResource.OpticalFlowInput);
   Add(1,TBindingKind.SampledImage,TResource.OpticalFlowPreviousInput);
   Add(2,TBindingKind.StorageImage,TResource.OpticalFlow);
   Add(3,TBindingKind.StorageImage,TResource.OpticalFlowSCDOutput);
   Add(4,TBindingKind.UniformBuffer,TResource.ConstantsOpticalFlow);
  end;
  TPass.OpticalFlowFilterOpticalFlowV5:begin
   Add(0,TBindingKind.SampledImage,TResource.OpticalFlowPrevious);
   Add(1,TBindingKind.StorageImage,TResource.OpticalFlow);
   Add(2,TBindingKind.UniformBuffer,TResource.ConstantsOpticalFlow);
  end;
  TPass.OpticalFlowScaleOpticalFlowAdvancedV5:begin
   Add(0,TBindingKind.SampledImage,TResource.OpticalFlowInput);
   Add(1,TBindingKind.SampledImage,TResource.OpticalFlowPreviousInput);
   Add(2,TBindingKind.SampledImage,TResource.OpticalFlow);
   Add(3,TBindingKind.StorageImage,TResource.OpticalFlowNextLevel);
   Add(4,TBindingKind.StorageImage,TResource.OpticalFlowSCDOutput);
   Add(5,TBindingKind.UniformBuffer,TResource.ConstantsOpticalFlow);
  end;
  TPass.ReconstructAndDilate:begin
   Add(0,TBindingKind.SampledImage,TResource.InputMotionVectors);
   Add(1,TBindingKind.SampledImage,TResource.InputDepth);
   Add(2,TBindingKind.StorageImage,TResource.ReconstructedDepthPreviousFrame);
   Add(3,TBindingKind.StorageImage,TResource.DilatedMotionVectors);
   Add(4,TBindingKind.StorageImage,TResource.DilatedDepth);
   Add(5,TBindingKind.UniformBuffer,TResource.ConstantsFrameInterpolation);
  end;
  TPass.Setup:begin
   Add(0,TBindingKind.SampledImage,TResource.OpticalFlowSceneChangeDetection);
   Add(1,TBindingKind.StorageImage,TResource.GameMotionVectorFieldX);
   Add(2,TBindingKind.StorageImage,TResource.GameMotionVectorFieldY);
   Add(3,TBindingKind.StorageImage,TResource.OpticalFlowMotionVectorFieldX);
   Add(4,TBindingKind.StorageImage,TResource.OpticalFlowMotionVectorFieldY);
   Add(5,TBindingKind.StorageImage,TResource.DisocclusionMask);
   Add(6,TBindingKind.StorageBuffer,TResource.Counters);
   Add(7,TBindingKind.UniformBuffer,TResource.ConstantsFrameInterpolation);
  end;
  TPass.ReconstructPreviousDepth:begin
   Add(0,TBindingKind.SampledImage,TResource.DilatedMotionVectors);
   Add(1,TBindingKind.SampledImage,TResource.DilatedDepth);
   Add(2,TBindingKind.SampledImage,TResource.CurrentInterpolationSource);
   Add(3,TBindingKind.SampledImage,TResource.DistortionField);
   Add(4,TBindingKind.StorageImage,TResource.ReconstructedDepthInterpolatedFrame);
   Add(5,TBindingKind.UniformBuffer,TResource.ConstantsFrameInterpolation);
  end;
  TPass.GameMotionVectorField:begin
   Add(0,TBindingKind.SampledImage,TResource.DilatedMotionVectors);
   Add(1,TBindingKind.SampledImage,TResource.DilatedDepth);
   Add(2,TBindingKind.SampledImage,TResource.PreviousInterpolationSource);
   Add(3,TBindingKind.SampledImage,TResource.CurrentInterpolationSource);
   Add(4,TBindingKind.SampledImage,TResource.DistortionField);
   Add(5,TBindingKind.StorageImage,TResource.GameMotionVectorFieldX);
   Add(6,TBindingKind.StorageImage,TResource.GameMotionVectorFieldY);
   Add(7,TBindingKind.UniformBuffer,TResource.ConstantsFrameInterpolation);
  end;
  TPass.OpticalFlowVectorField:begin
   Add(0,TBindingKind.SampledImage,TResource.OpticalFlowVector);
   Add(1,TBindingKind.SampledImage,TResource.OpticalFlowConfidence);
   Add(2,TBindingKind.SampledImage,TResource.DilatedDepth);
   Add(3,TBindingKind.SampledImage,TResource.PreviousInterpolationSource);
   Add(4,TBindingKind.SampledImage,TResource.CurrentInterpolationSource);
   Add(6,TBindingKind.StorageImage,TResource.OpticalFlowMotionVectorFieldX);
   Add(7,TBindingKind.StorageImage,TResource.OpticalFlowMotionVectorFieldY);
   Add(8,TBindingKind.UniformBuffer,TResource.ConstantsFrameInterpolation);
  end;
  TPass.DisocclusionMask:begin
   Add(0,TBindingKind.SampledImage,TResource.GameMotionVectorFieldX);
   Add(1,TBindingKind.SampledImage,TResource.GameMotionVectorFieldY);
   Add(2,TBindingKind.SampledImage,TResource.ReconstructedDepthPreviousFrame);
   Add(3,TBindingKind.SampledImage,TResource.DilatedDepth);
   Add(4,TBindingKind.SampledImage,TResource.ReconstructedDepthInterpolatedFrame);
   Add(5,TBindingKind.SampledImage,TResource.InpaintingPyramid);
   Add(6,TBindingKind.SampledImage,TResource.DistortionField);
   Add(7,TBindingKind.StorageImage,TResource.DisocclusionMask);
   Add(8,TBindingKind.UniformBuffer,TResource.ConstantsFrameInterpolation);
  end;
  TPass.Interpolation:begin
   Add(0,TBindingKind.SampledImage,TResource.GameMotionVectorFieldX);
   Add(1,TBindingKind.SampledImage,TResource.GameMotionVectorFieldY);
   Add(2,TBindingKind.SampledImage,TResource.OpticalFlowMotionVectorFieldX);
   Add(3,TBindingKind.SampledImage,TResource.OpticalFlowMotionVectorFieldY);
   Add(4,TBindingKind.SampledImage,TResource.PreviousInterpolationSource);
   Add(5,TBindingKind.SampledImage,TResource.CurrentInterpolationSource);
   Add(6,TBindingKind.SampledImage,TResource.DisocclusionMask);
   Add(7,TBindingKind.SampledImage,TResource.InpaintingPyramid);
   Add(8,TBindingKind.StorageBuffer,TResource.Counters);
   Add(9,TBindingKind.StorageImage,TResource.Output);
   Add(10,TBindingKind.UniformBuffer,TResource.ConstantsFrameInterpolation);
  end;
  TPass.InpaintingPyramid:begin
   Add(0,TBindingKind.SampledImage,TResource.Output);
   Add(1,TBindingKind.StorageBuffer,TResource.Counters);
   for MipIndex:=0 to CountInpaintingPyramidMipViews-1 do begin
    Add(2+MipIndex,TBindingKind.StorageImage,TResource.InpaintingPyramidMip,MipIndex);
   end;
   Add(15,TBindingKind.UniformBuffer,TResource.ConstantsFrameInterpolation);
   Add(16,TBindingKind.UniformBuffer,TResource.ConstantsInpaintingPyramid);
  end;
  TPass.Inpainting:begin
   Add(0,TBindingKind.SampledImage,TResource.OpticalFlowSceneChangeDetection);
   Add(1,TBindingKind.SampledImage,TResource.InpaintingPyramid);
   Add(2,TBindingKind.SampledImage,TResource.PresentBackbuffer);
   Add(3,TBindingKind.SampledImage,TResource.CurrentInterpolationSource);
   Add(7,TBindingKind.StorageImage,TResource.Output);
   Add(8,TBindingKind.UniformBuffer,TResource.ConstantsFrameInterpolation);
  end;
  TPass.GameVectorFieldInpaintingPyramid:begin
   Add(0,TBindingKind.SampledImage,TResource.GameMotionVectorFieldX);
   Add(1,TBindingKind.SampledImage,TResource.GameMotionVectorFieldY);
   Add(2,TBindingKind.StorageBuffer,TResource.Counters);
   for MipIndex:=0 to CountInpaintingPyramidMipViews-1 do begin
    Add(3+MipIndex,TBindingKind.StorageImage,TResource.InpaintingPyramidMip,MipIndex);
   end;
   Add(16,TBindingKind.UniformBuffer,TResource.ConstantsFrameInterpolation);
   Add(17,TBindingKind.UniformBuffer,TResource.ConstantsInpaintingPyramid);
  end;
  TPass.DebugView:begin
   Add(0,TBindingKind.SampledImage,TResource.GameMotionVectorFieldX);
   Add(1,TBindingKind.SampledImage,TResource.GameMotionVectorFieldY);
   Add(2,TBindingKind.SampledImage,TResource.OpticalFlowMotionVectorFieldX);
   Add(3,TBindingKind.SampledImage,TResource.OpticalFlowMotionVectorFieldY);
   Add(4,TBindingKind.SampledImage,TResource.DisocclusionMask);
   Add(5,TBindingKind.SampledImage,TResource.PresentBackbuffer);
   Add(6,TBindingKind.SampledImage,TResource.InpaintingPyramid);
   Add(7,TBindingKind.SampledImage,TResource.CurrentInterpolationSource);
   Add(8,TBindingKind.SampledImage,TResource.DistortionField);
   Add(9,TBindingKind.StorageImage,TResource.Output);
   Add(10,TBindingKind.UniformBuffer,TResource.ConstantsFrameInterpolation);
  end;
 end;
 // s_LinearClamp, declared by the shared callbacks of every pass of the frame interpolation.
 if aPass>=TPass.ReconstructAndDilate then begin
  Add(1000,TBindingKind.Sampler,TResource.LinearClampSampler);
 end;
 SetLength(result,Count);
end;

class function TpvScene3DRendererFrameGenerationBackendFSR3.GetPassMaximumCountUses(const aPass:TPass):TpvInt32;
begin
 case aPass of
  TPass.OpticalFlowComputeOpticalFlowAdvancedV5,
  TPass.OpticalFlowFilterOpticalFlowV5:begin
   // Once per pyramid level.
   result:=CountOpticalFlowPyramidLevels;
  end;
  TPass.OpticalFlowScaleOpticalFlowAdvancedV5:begin
   // Once per pyramid level but the finest, which has none finer to scale to.
   result:=CountOpticalFlowPyramidLevels-1;
  end;
  TPass.GameVectorFieldInpaintingPyramid:begin
   // Once for the interpolation and once more for the debug view.
   result:=2;
  end;
  else begin
   result:=1;
  end;
 end;
end;

function TpvScene3DRendererFrameGenerationBackendFSR3.GetShaderFileName(const aPass:TPass):TpvUTF8String;
begin
 case aPass of
  TPass.OpticalFlowPrepareLuma:begin
   result:='fsr3opticalflow_prepare_luma';
  end;
  TPass.OpticalFlowComputeLuminancePyramid:begin
   result:='fsr3opticalflow_compute_luminance_pyramid';
  end;
  TPass.OpticalFlowGenerateSCDHistogram:begin
   result:='fsr3opticalflow_generate_scd_histogram';
  end;
  TPass.OpticalFlowComputeSCDDivergence:begin
   result:='fsr3opticalflow_compute_scd_divergence';
  end;
  TPass.OpticalFlowComputeOpticalFlowAdvancedV5:begin
   result:='fsr3opticalflow_compute_optical_flow_advanced_v5';
  end;
  TPass.OpticalFlowFilterOpticalFlowV5:begin
   result:='fsr3opticalflow_filter_optical_flow_v5';
  end;
  TPass.OpticalFlowScaleOpticalFlowAdvancedV5:begin
   result:='fsr3opticalflow_scale_optical_flow_advanced_v5';
  end;
  TPass.ReconstructAndDilate:begin
   result:='fsr3frameinterpolation_reconstruct_and_dilate';
  end;
  TPass.Setup:begin
   result:='fsr3frameinterpolation_setup';
  end;
  TPass.ReconstructPreviousDepth:begin
   result:='fsr3frameinterpolation_reconstruct_previous_depth';
  end;
  TPass.GameMotionVectorField:begin
   result:='fsr3frameinterpolation_game_motion_vector_field';
  end;
  TPass.OpticalFlowVectorField:begin
   result:='fsr3frameinterpolation_optical_flow_vector_field';
  end;
  TPass.DisocclusionMask:begin
   result:='fsr3frameinterpolation_disocclusion_mask';
  end;
  TPass.Interpolation:begin
   result:='fsr3frameinterpolation_interpolation';
  end;
  TPass.InpaintingPyramid:begin
   result:='fsr3frameinterpolation_compute_inpainting_pyramid';
  end;
  TPass.Inpainting:begin
   result:='fsr3frameinterpolation_inpainting';
  end;
  TPass.GameVectorFieldInpaintingPyramid:begin
   result:='fsr3frameinterpolation_compute_game_vector_field_inpainting_pyramid';
  end;
  else {TPass.DebugView:}begin
   result:='fsr3frameinterpolation_debug_view';
  end;
 end;
 // The optical flow knows nothing of depth, the frame interpolation is built for either convention.
 if fInvertedDepth and (aPass>=TPass.ReconstructAndDilate) then begin
  result:=result+'_inverteddepth';
 end;
 result:=result+'_comp.spv';
end;

function TpvScene3DRendererFrameGenerationBackendFSR3.GetImageView(const aBinding:TBinding):TVkImageView;
begin
 case aBinding.Resource of
  TResource.InpaintingPyramidMip:begin
   result:=fInpaintingPyramidMipViews[aBinding.Index].Handle;
  end;
  TResource.OpticalFlowInputLevel:begin
   result:=fOpticalFlowInputs[fBoundOpticalFlowInputHalf,aBinding.Index].ImageView.Handle;
  end;
  else begin
   result:=fBoundImageViews[aBinding.Resource];
  end;
 end;
end;

procedure TpvScene3DRendererFrameGenerationBackendFSR3.BindImage(const aResource:TResource;const aImageView:TVkImageView;const aImageLayout:TVkImageLayout);
begin
 fBoundImageViews[aResource]:=aImageView;
 fBoundImageLayouts[aResource]:=aImageLayout;
end;

procedure TpvScene3DRendererFrameGenerationBackendFSR3.BindInternalImage(const aResource:TResource;const aInternalImage:TInternalImage);
begin
 BindImage(aResource,fInternalImages[aInternalImage].ImageView.Handle,VK_IMAGE_LAYOUT_GENERAL);
end;

// setupDeviceDepthToViewSpaceDepthParams of the SDK, taken from the projection matrix itself, as in
// TpvScene3DRendererTemporalUpscalerBackendFSR3.SetupDeviceDepthToViewSpaceDepthParams, where the reasoning
// is written down.
procedure TpvScene3DRendererFrameGenerationBackendFSR3.SetupDeviceDepthToViewSpaceDepthParams(const aProjectionMatrix:TpvMatrix4x4);
begin
 fFrameInterpolationConstants.DeviceToViewDepth[0]:=-aProjectionMatrix.RawComponents[2,2];
 if abs(fFrameInterpolationConstants.DeviceToViewDepth[0])<FLT_EPSILON then begin
  fFrameInterpolationConstants.DeviceToViewDepth[0]:=-FLT_EPSILON;
 end;
 fFrameInterpolationConstants.DeviceToViewDepth[1]:=aProjectionMatrix.RawComponents[3,2];
 fFrameInterpolationConstants.DeviceToViewDepth[2]:=1.0/Max(FLT_EPSILON,abs(aProjectionMatrix.RawComponents[0,0]));
 fFrameInterpolationConstants.DeviceToViewDepth[3]:=1.0/Max(FLT_EPSILON,abs(aProjectionMatrix.RawComponents[1,1]));
end;

procedure TpvScene3DRendererFrameGenerationBackendFSR3.GlobalBarrier(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                     const aSrcStageMask:TVkPipelineStageFlags;
                                                                     const aDstStageMask:TVkPipelineStageFlags;
                                                                     const aSrcAccessMask:TVkAccessFlags;
                                                                     const aDstAccessMask:TVkAccessFlags);
var MemoryBarrier:TVkMemoryBarrier;
begin
 FillChar(MemoryBarrier,SizeOf(TVkMemoryBarrier),#0);
 MemoryBarrier.sType:=VK_STRUCTURE_TYPE_MEMORY_BARRIER;
 MemoryBarrier.srcAccessMask:=aSrcAccessMask;
 MemoryBarrier.dstAccessMask:=aDstAccessMask;
 aCommandBuffer.CmdPipelineBarrier(aSrcStageMask,
                                   aDstStageMask,
                                   0,
                                   1,@MemoryBarrier,
                                   0,nil,
                                   0,nil);
end;

procedure TpvScene3DRendererFrameGenerationBackendFSR3.ClearImage(const aCommandBuffer:TpvVulkanCommandBuffer;const aItem:PInternalImageItem;const aR,aG,aB,aA:TpvFloat);
var ClearColorValue:TVkClearColorValue;
    ImageSubresourceRange:TVkImageSubresourceRange;
begin
 FillChar(ClearColorValue,SizeOf(TVkClearColorValue),#0);
 ClearColorValue.float32[0]:=aR;
 ClearColorValue.float32[1]:=aG;
 ClearColorValue.float32[2]:=aB;
 ClearColorValue.float32[3]:=aA;
 FillChar(ImageSubresourceRange,SizeOf(TVkImageSubresourceRange),#0);
 ImageSubresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 ImageSubresourceRange.baseMipLevel:=0;
 ImageSubresourceRange.levelCount:=aItem^.CountMipLevels;
 ImageSubresourceRange.baseArrayLayer:=0;
 ImageSubresourceRange.layerCount:=1;
 aCommandBuffer.CmdClearColorImage(aItem^.Image.Handle,
                                   VK_IMAGE_LAYOUT_GENERAL,
                                   @ClearColorValue,
                                   1,
                                   @ImageSubresourceRange);
end;

procedure TpvScene3DRendererFrameGenerationBackendFSR3.ClearImageUInt(const aCommandBuffer:TpvVulkanCommandBuffer;const aItem:PInternalImageItem;const aValue:TpvUInt32);
var ClearColorValue:TVkClearColorValue;
    ImageSubresourceRange:TVkImageSubresourceRange;
begin
 FillChar(ClearColorValue,SizeOf(TVkClearColorValue),#0);
 ClearColorValue.uint32[0]:=aValue;
 ClearColorValue.uint32[1]:=aValue;
 ClearColorValue.uint32[2]:=aValue;
 ClearColorValue.uint32[3]:=aValue;
 FillChar(ImageSubresourceRange,SizeOf(TVkImageSubresourceRange),#0);
 ImageSubresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 ImageSubresourceRange.baseMipLevel:=0;
 ImageSubresourceRange.levelCount:=aItem^.CountMipLevels;
 ImageSubresourceRange.baseArrayLayer:=0;
 ImageSubresourceRange.layerCount:=1;
 aCommandBuffer.CmdClearColorImage(aItem^.Image.Handle,
                                   VK_IMAGE_LAYOUT_GENERAL,
                                   @ClearColorValue,
                                   1,
                                   @ImageSubresourceRange);
end;

// scheduleDispatch of the SDK: writes the next descriptor set of the pass with what is bound at the moment,
// and records the dispatch.
procedure TpvScene3DRendererFrameGenerationBackendFSR3.ScheduleDispatch(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                        const aInFlightFrameIndex:TpvSizeInt;
                                                                        const aPass:TPass;
                                                                        const aDispatchX:TpvUInt32;
                                                                        const aDispatchY:TpvUInt32;
                                                                        const aDispatchZ:TpvUInt32);
var BindingIndex:TpvSizeInt;
    Binding:TBinding;
    DescriptorSet:TpvVulkanDescriptorSet;
begin

 if fPassUses[aPass]>=GetPassMaximumCountUses(aPass) then begin
  exit;
 end;
 DescriptorSet:=fDescriptorSets[aInFlightFrameIndex,aPass,fPassUses[aPass]];
 inc(fPassUses[aPass]);

 for BindingIndex:=0 to length(fPasses[aPass].Bindings)-1 do begin
  Binding:=fPasses[aPass].Bindings[BindingIndex];
  case Binding.Kind of
   TBindingKind.SampledImage:begin
    DescriptorSet.WriteToDescriptorSet(Binding.Binding,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                                       [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                      GetImageView(Binding),
                                                                      fBoundImageLayouts[Binding.Resource])],
                                       [],
                                       [],
                                       false);
   end;
   TBindingKind.StorageImage:begin
    DescriptorSet.WriteToDescriptorSet(Binding.Binding,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                       [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                      GetImageView(Binding),
                                                                      VK_IMAGE_LAYOUT_GENERAL)],
                                       [],
                                       [],
                                       false);
   end;
   TBindingKind.UniformBuffer:begin
    DescriptorSet.WriteToDescriptorSet(Binding.Binding,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                       [],
                                       [TVkDescriptorBufferInfo.Create(fConstantBuffers[aInFlightFrameIndex].Handle,
                                                                       fBoundConstantSlots[Binding.Resource]*ConstantBufferSlotSize,
                                                                       ConstantBufferSlotSize)],
                                       [],
                                       false);
   end;
   TBindingKind.StorageBuffer:begin
    DescriptorSet.WriteToDescriptorSet(Binding.Binding,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                       [],
                                       [fCounters.DescriptorBufferInfo],
                                       [],
                                       false);
   end;
   else {TBindingKind.Sampler:}begin
    DescriptorSet.WriteToDescriptorSet(Binding.Binding,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLER),
                                       [TVkDescriptorImageInfo.Create(fLinearClampSampler.Handle,
                                                                      VK_NULL_HANDLE,
                                                                      VK_IMAGE_LAYOUT_UNDEFINED)],
                                       [],
                                       [],
                                       false);
   end;
  end;
 end;
 DescriptorSet.Flush;

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPasses[aPass].Pipeline.Handle);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                      fPasses[aPass].PipelineLayout.Handle,
                                      0,
                                      1,
                                      @DescriptorSet.Handle,
                                      0,
                                      nil);
 aCommandBuffer.CmdDispatch(Max(1,aDispatchX),Max(1,aDispatchY),Max(1,aDispatchZ));

 // Every pass reads what the ones before it wrote, so each one is followed by a barrier.
 GlobalBarrier(aCommandBuffer,
               TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
               TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
               TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
               TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT));

end;

// dispatch of ffx_opticalflow.cpp, for the color of the current frame. The luma pyramid of the frame before
// is still in the other half of the input pyramid.
procedure TpvScene3DRendererFrameGenerationBackendFSR3.DispatchOpticalFlow(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                           const aInFlightFrameIndex:TpvSizeInt;
                                                                           const aDescription:TGenerateDescription;
                                                                           const aReset:boolean;
                                                                           const aMinLuminance:TpvFloat;
                                                                           const aMaxLuminance:TpvFloat);
var Level,Half:TpvSizeInt;
    IsOddFrame,IsOddLevel:boolean;
    InputHalfA,InputHalfB,FlowHalfA,FlowHalfB:TpvInt32;
    ThreadGroupSizeOpticalFlowInputPyramidX,ThreadGroupSizeOpticalFlowInputPyramidY,NumWorkGroups,CountMips:TpvUInt32;
    InputLumaWidth,InputLumaHeight,StrataWidth:TpvInt32;
    SPDConstants:TOpticalFlowSPDConstants;
begin

 fOpticalFlowConstants.BackbufferTransferFunction:=TransferFunctionSCRGB;
 fOpticalFlowConstants.MinMaxLuminance[0]:=aMinLuminance;
 fOpticalFlowConstants.MinMaxLuminance[1]:=aMaxLuminance;

 if aReset then begin
  fOpticalFlowConstants.FrameIndex:=0;
 end else begin
  inc(fOpticalFlowConstants.FrameIndex);
 end;

 if aReset then begin
  ClearImageUInt(aCommandBuffer,@fInternalImages[TInternalImage.OpticalFlowSCDTemp],0);
  ClearImageUInt(aCommandBuffer,@fInternalImages[TInternalImage.OpticalFlowSCDOutput],0);
  ClearImageUInt(aCommandBuffer,@fInternalImages[TInternalImage.OpticalFlowSCDHistogram],0);
  ClearImage(aCommandBuffer,@fInternalImages[TInternalImage.OpticalFlowSCDPreviousHistogram],0.0,0.0,0.0,0.0);
  for Half:=0 to 1 do begin
   for Level:=0 to CountOpticalFlowPyramidLevels-1 do begin
    ClearImageUInt(aCommandBuffer,@fOpticalFlowInputs[Half,Level],0);
   end;
  end;
  GlobalBarrier(aCommandBuffer,
                TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT));
 end;

 SPDSetup(fWidth,
          fHeight,
          4,
          ThreadGroupSizeOpticalFlowInputPyramidX,
          ThreadGroupSizeOpticalFlowInputPyramidY,
          NumWorkGroups,
          CountMips);

 FillChar(SPDConstants,SizeOf(TOpticalFlowSPDConstants),#0);
 SPDConstants.NumWorkGroups:=NumWorkGroups;
 SPDConstants.Mips:=CountMips;
 SPDConstants.WorkGroupOffset[0]:=0;
 SPDConstants.WorkGroupOffset[1]:=0;
 SPDConstants.NumWorkGroupsOpticalFlowInputPyramid:=NumWorkGroups;

 // The constants of every pyramid level go into a slot of their own; the passes before the pyramid see
 // those of the finest level, as they do in the SDK, where the last level of the frame before is still
 // staged.
 fOpticalFlowConstants.OpticalFlowPyramidLevelCount:=CountOpticalFlowPyramidLevels;
 for Level:=0 to CountOpticalFlowPyramidLevels-1 do begin
  fOpticalFlowConstants.OpticalFlowPyramidLevel:=Level;
  fConstantBuffers[aInFlightFrameIndex].UpdateData(fOpticalFlowConstants,(ConstantSlotOpticalFlow+Level)*ConstantBufferSlotSize,SizeOf(TOpticalFlowConstants));
 end;
 fConstantBuffers[aInFlightFrameIndex].UpdateData(SPDConstants,ConstantSlotOpticalFlowSPD*ConstantBufferSlotSize,SizeOf(TOpticalFlowSPDConstants));
 fBoundConstantSlots[TResource.ConstantsOpticalFlow]:=ConstantSlotOpticalFlow;
 fBoundConstantSlots[TResource.ConstantsOpticalFlowSPD]:=ConstantSlotOpticalFlowSPD;

 BindInternalImage(TResource.OpticalFlowSCDHistogram,TInternalImage.OpticalFlowSCDHistogram);
 BindInternalImage(TResource.OpticalFlowSCDPreviousHistogram,TInternalImage.OpticalFlowSCDPreviousHistogram);
 BindInternalImage(TResource.OpticalFlowSCDTemp,TInternalImage.OpticalFlowSCDTemp);
 BindInternalImage(TResource.OpticalFlowSCDOutput,TInternalImage.OpticalFlowSCDOutput);

 IsOddFrame:=(fOpticalFlowResourceFrameIndex and 1)<>0;

 if IsOddFrame then begin
  InputHalfA:=1;
  InputHalfB:=0;
 end else begin
  InputHalfA:=0;
  InputHalfB:=1;
 end;

 fBoundOpticalFlowInputHalf:=InputHalfA;
 BindImage(TResource.InputColor,aDescription.Current.Color.ImageView,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
 BindImage(TResource.OpticalFlowInput,fOpticalFlowInputs[InputHalfA,0].ImageView.Handle,VK_IMAGE_LAYOUT_GENERAL);
 BindImage(TResource.OpticalFlowPreviousInput,fOpticalFlowInputs[InputHalfB,0].ImageView.Handle,VK_IMAGE_LAYOUT_GENERAL);

 // Two by two pixels per thread, in groups of sixteen by sixteen.
 ScheduleDispatch(aCommandBuffer,
                  aInFlightFrameIndex,
                  TPass.OpticalFlowPrepareLuma,
                  ((((fWidth+1) div 2)+15) div 16),
                  ((((fHeight+1) div 2)+15) div 16));

 ScheduleDispatch(aCommandBuffer,
                  aInFlightFrameIndex,
                  TPass.OpticalFlowComputeLuminancePyramid,
                  ThreadGroupSizeOpticalFlowInputPyramidX,
                  ThreadGroupSizeOpticalFlowInputPyramidY);

 StrataWidth:=(fWidth div 4) div HistogramsPerDim;
 ScheduleDispatch(aCommandBuffer,
                  aInFlightFrameIndex,
                  TPass.OpticalFlowGenerateSCDHistogram,
                  (StrataWidth+31) div 32,
                  16,
                  HistogramsPerDim*HistogramsPerDim);

 ScheduleDispatch(aCommandBuffer,
                  aInFlightFrameIndex,
                  TPass.OpticalFlowComputeSCDDivergence,
                  HistogramsPerDim*HistogramsPerDim,
                  HistogramShifts);

 for Level:=CountOpticalFlowPyramidLevels-1 downto 0 do begin

  IsOddLevel:=(Level and 1)<>0;

  if IsOddFrame<>IsOddLevel then begin
   FlowHalfA:=1;
   FlowHalfB:=0;
  end else begin
   FlowHalfA:=0;
   FlowHalfB:=1;
  end;

  fBoundConstantSlots[TResource.ConstantsOpticalFlow]:=ConstantSlotOpticalFlow+Level;

  BindImage(TResource.OpticalFlowInput,fOpticalFlowInputs[InputHalfA,Level].ImageView.Handle,VK_IMAGE_LAYOUT_GENERAL);
  BindImage(TResource.OpticalFlowPreviousInput,fOpticalFlowInputs[InputHalfB,Level].ImageView.Handle,VK_IMAGE_LAYOUT_GENERAL);
  BindImage(TResource.OpticalFlow,fOpticalFlows[FlowHalfA,Level].ImageView.Handle,VK_IMAGE_LAYOUT_GENERAL);

  // Search, four pixels per thread in rows of sixteen threads.
  InputLumaWidth:=Max(fWidth shr Level,1);
  InputLumaHeight:=Max(fHeight shr Level,1);
  ScheduleDispatch(aCommandBuffer,
                   aInFlightFrameIndex,
                   TPass.OpticalFlowComputeOpticalFlowAdvancedV5,
                   ((((InputLumaWidth+3) div 4)*16)+63) div 64,
                   (InputLumaHeight+15) div 16);

  // Filter, into the shared optical flow vectors at the finest level.
  BindImage(TResource.OpticalFlowPrevious,fOpticalFlows[FlowHalfA,Level].ImageView.Handle,VK_IMAGE_LAYOUT_GENERAL);
  if Level=0 then begin
   BindInternalImage(TResource.OpticalFlow,TInternalImage.OpticalFlowVector);
  end else begin
   BindImage(TResource.OpticalFlow,fOpticalFlows[FlowHalfB,Level].ImageView.Handle,VK_IMAGE_LAYOUT_GENERAL);
  end;
  ScheduleDispatch(aCommandBuffer,
                   aInFlightFrameIndex,
                   TPass.OpticalFlowFilterOpticalFlowV5,
                   (fOpticalFlowTextureSizes[Level,0]+15) div 16,
                   (fOpticalFlowTextureSizes[Level,1]+3) div 4);

  // Scale, as the start of the search at the next finer level.
  if Level>0 then begin
   BindImage(TResource.OpticalFlow,fOpticalFlows[FlowHalfB,Level].ImageView.Handle,VK_IMAGE_LAYOUT_GENERAL);
   BindImage(TResource.OpticalFlowNextLevel,fOpticalFlows[FlowHalfB,Level-1].ImageView.Handle,VK_IMAGE_LAYOUT_GENERAL);
   ScheduleDispatch(aCommandBuffer,
                    aInFlightFrameIndex,
                    TPass.OpticalFlowScaleOpticalFlowAdvancedV5,
                    (fOpticalFlowTextureSizes[Level-1,0]+3) div 4,
                    (fOpticalFlowTextureSizes[Level-1,1]+3) div 4,
                    1);
  end;

 end;

 fOpticalFlowResourceFrameIndex:=(fOpticalFlowResourceFrameIndex+1) mod 16;

end;

// ffxFrameInterpolationPrepare and ffxFrameInterpolationDispatch of ffx_frameinterpolation.cpp.
procedure TpvScene3DRendererFrameGenerationBackendFSR3.DispatchFrameInterpolation(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                                  const aInFlightFrameIndex:TpvSizeInt;
                                                                                  const aDescription:TGenerateDescription;
                                                                                  const aReset:boolean;
                                                                                  const aMinLuminance:TpvFloat;
                                                                                  const aMaxLuminance:TpvFloat);
var RenderWidth,RenderHeight:TpvInt32;
    RenderDispatchSizeX,RenderDispatchSizeY,DisplayDispatchSizeX,DisplayDispatchSizeY:TpvUInt32;
    OpticalFlowDispatchSizeX,OpticalFlowDispatchSizeY:TpvUInt32;
    SPDDispatchX,SPDDispatchY,SPDNumWorkGroups,SPDCountMips:TpvUInt32;
    InpaintingPyramidConstants:TInpaintingPyramidConstants;
    DepthClearValue:TpvFloat;
    ExecutePreparationPasses:boolean;
begin

 RenderWidth:=Min(Max(1,aDescription.Current.Depth.Width),fRenderWidth);
 RenderHeight:=Min(Max(1,aDescription.Current.Depth.Height),fRenderHeight);

 // ffxFrameInterpolationPrepare
 fFrameInterpolationConstants.RenderSize[0]:=RenderWidth;
 fFrameInterpolationConstants.RenderSize[1]:=RenderHeight;
 fFrameInterpolationConstants.Jitter[0]:=aDescription.Current.JitterOffset.x;
 fFrameInterpolationConstants.Jitter[1]:=aDescription.Current.JitterOffset.y;

 // The motion vectors are at render resolution.
 fFrameInterpolationConstants.MotionVectorScale[0]:=aDescription.Current.MotionVectorScale.x/RenderWidth;
 fFrameInterpolationConstants.MotionVectorScale[1]:=aDescription.Current.MotionVectorScale.y/RenderHeight;

 // ffxFrameInterpolationDispatch
 fFrameInterpolationConstants.DisplaySize[0]:=fWidth;
 fFrameInterpolationConstants.DisplaySize[1]:=fHeight;
 fFrameInterpolationConstants.DisplaySizeRcp[0]:=1.0/fWidth;
 fFrameInterpolationConstants.DisplaySizeRcp[1]:=1.0/fHeight;
 fFrameInterpolationConstants.UpscalerTargetSize[0]:=fWidth;
 fFrameInterpolationConstants.UpscalerTargetSize[1]:=fHeight;
 fFrameInterpolationConstants.Mode:=0;
 fFrameInterpolationConstants.Reset:=ord(aReset) and 1;
 fFrameInterpolationConstants.DeltaTime:=aDescription.Current.FrameTimeDelta;
 // The HUD comes on top afterwards, see TpvScene3DRendererFrameGenerationCompositor, so the picture without
 // it is all there is.
 fFrameInterpolationConstants.HUDLessAttachedFactor:=0;

 fFrameInterpolationConstants.OpticalFlowScale[0]:=1.0/fWidth;
 fFrameInterpolationConstants.OpticalFlowScale[1]:=1.0/fHeight;
 fFrameInterpolationConstants.OpticalFlowBlockSize:=OpticalFlowBlockSize;
 if aDescription.DebugView then begin
  fFrameInterpolationConstants.DispatchFlags:=DispatchDrawDebugView;
 end else begin
  fFrameInterpolationConstants.DispatchFlags:=0;
 end;

 fFrameInterpolationConstants.CameraNear:=aDescription.Current.ZNear;
 fFrameInterpolationConstants.CameraFar:=aDescription.Current.ZFar;

 fFrameInterpolationConstants.InterpolationRectBase[0]:=0;
 fFrameInterpolationConstants.InterpolationRectBase[1]:=0;
 fFrameInterpolationConstants.InterpolationRectSize[0]:=fWidth;
 fFrameInterpolationConstants.InterpolationRectSize[1]:=fHeight;

 // Debug bar
 fFrameInterpolationConstants.DebugBarColor[0]:=DebugBarColorSequence[fDebugBarColorIndex,0];
 fFrameInterpolationConstants.DebugBarColor[1]:=DebugBarColorSequence[fDebugBarColorIndex,1];
 fFrameInterpolationConstants.DebugBarColor[2]:=DebugBarColorSequence[fDebugBarColorIndex,2];
 fDebugBarColorIndex:=(fDebugBarColorIndex+1) mod length(DebugBarColorSequence);

 fFrameInterpolationConstants.BackBufferTransferFunction:=TransferFunctionSCRGB;
 fFrameInterpolationConstants.MinMaxLuminance[0]:=aMinLuminance;
 fFrameInterpolationConstants.MinMaxLuminance[1]:=aMaxLuminance;

 // The tangent of half the horizontal field of view, which is the inverse of the projection's x scale.
 fFrameInterpolationConstants.TanHalfFOV:=1.0/Max(FLT_EPSILON,abs(aDescription.Current.ProjectionMatrix.RawComponents[0,0]));

 // No distortion field, the default one of a single texel without any offset stands in for it.
 fFrameInterpolationConstants.DistortionFieldSize[0]:=1;
 fFrameInterpolationConstants.DistortionFieldSize[1]:=1;

 SetupDeviceDepthToViewSpaceDepthParams(aDescription.Current.ProjectionMatrix);

 fConstantBuffers[aInFlightFrameIndex].UpdateData(fFrameInterpolationConstants,ConstantSlotFrameInterpolation*ConstantBufferSlotSize,SizeOf(TFrameInterpolationConstants));
 fBoundConstantSlots[TResource.ConstantsFrameInterpolation]:=ConstantSlotFrameInterpolation;

 // The bindings, which are the same for all the passes but the inpainting pyramid constants.
 BindImage(TResource.InputDepth,aDescription.Current.Depth.ImageView,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
 BindImage(TResource.InputMotionVectors,aDescription.Current.MotionVectors.ImageView,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
 BindImage(TResource.PreviousInterpolationSource,aDescription.Previous.Color.ImageView,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
 BindImage(TResource.CurrentInterpolationSource,aDescription.Current.Color.ImageView,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
 BindImage(TResource.PresentBackbuffer,aDescription.Current.Color.ImageView,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
 BindImage(TResource.Output,aDescription.Output.ImageView,VK_IMAGE_LAYOUT_GENERAL);
 BindInternalImage(TResource.DilatedDepth,TInternalImage.DilatedDepth);
 BindInternalImage(TResource.DilatedMotionVectors,TInternalImage.DilatedMotionVectors);
 BindInternalImage(TResource.ReconstructedDepthPreviousFrame,TInternalImage.ReconstructedPreviousNearestDepth);
 BindInternalImage(TResource.ReconstructedDepthInterpolatedFrame,TInternalImage.ReconstructedDepthInterpolatedFrame);
 BindInternalImage(TResource.DisocclusionMask,TInternalImage.DisocclusionMask);
 BindInternalImage(TResource.GameMotionVectorFieldX,TInternalImage.GameMotionVectorFieldX);
 BindInternalImage(TResource.GameMotionVectorFieldY,TInternalImage.GameMotionVectorFieldY);
 BindInternalImage(TResource.OpticalFlowMotionVectorFieldX,TInternalImage.OpticalFlowMotionVectorFieldX);
 BindInternalImage(TResource.OpticalFlowMotionVectorFieldY,TInternalImage.OpticalFlowMotionVectorFieldY);
 BindInternalImage(TResource.InpaintingPyramid,TInternalImage.InpaintingPyramid);
 BindInternalImage(TResource.DistortionField,TInternalImage.DefaultDistortionField);
 BindInternalImage(TResource.OpticalFlowVector,TInternalImage.OpticalFlowVector);
 BindInternalImage(TResource.OpticalFlowConfidence,TInternalImage.OpticalFlowConfidence);
 BindInternalImage(TResource.OpticalFlowSceneChangeDetection,TInternalImage.OpticalFlowSCDOutput);

 RenderDispatchSizeX:=(TpvUInt32(RenderWidth)+7) div 8;
 RenderDispatchSizeY:=(TpvUInt32(RenderHeight)+7) div 8;
 DisplayDispatchSizeX:=(TpvUInt32(fWidth)+7) div 8;
 DisplayDispatchSizeY:=(TpvUInt32(fHeight)+7) div 8;
 OpticalFlowDispatchSizeX:=TpvUInt32(trunc((fWidth/OpticalFlowBlockSize)+7)) div 8;
 OpticalFlowDispatchSizeY:=TpvUInt32(trunc((fHeight/OpticalFlowBlockSize)+7)) div 8;

 // The reconstructed depths hold the bits of a float as an unsigned integer, so they are cleared with the
 // bits of the farthest depth.
 if fInvertedDepth then begin
  DepthClearValue:=0.0;
 end else begin
  DepthClearValue:=1.0;
 end;

 // ffxFrameInterpolationPrepare: clear estimated depth resources, then dilate.
 ClearImageUInt(aCommandBuffer,@fInternalImages[TInternalImage.ReconstructedPreviousNearestDepth],PpvUInt32(@DepthClearValue)^);
 GlobalBarrier(aCommandBuffer,
               TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
               TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
               TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
               TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT));
 ScheduleDispatch(aCommandBuffer,aInFlightFrameIndex,TPass.ReconstructAndDilate,RenderDispatchSizeX,RenderDispatchSizeY);

 // ffxFrameInterpolationDispatch
 ExecutePreparationPasses:=not aReset;

 ScheduleDispatch(aCommandBuffer,aInFlightFrameIndex,TPass.Setup,RenderDispatchSizeX,RenderDispatchSizeY);

 // game vector field inpainting pyramid, as SPD over the render area
 SPDSetup(RenderWidth,RenderHeight,-1,SPDDispatchX,SPDDispatchY,SPDNumWorkGroups,SPDCountMips);
 FillChar(InpaintingPyramidConstants,SizeOf(TInpaintingPyramidConstants),#0);
 InpaintingPyramidConstants.NumWorkGroups:=SPDNumWorkGroups;
 InpaintingPyramidConstants.Mips:=SPDCountMips;
 InpaintingPyramidConstants.WorkGroupOffset[0]:=0;
 InpaintingPyramidConstants.WorkGroupOffset[1]:=0;
 fConstantBuffers[aInFlightFrameIndex].UpdateData(InpaintingPyramidConstants,ConstantSlotGameVectorFieldInpaintingPyramid*ConstantBufferSlotSize,SizeOf(TInpaintingPyramidConstants));

 // only execute FG data preparation passes when reset wasnt triggered
 if ExecutePreparationPasses then begin

  // clear estimated depth resources
  ClearImageUInt(aCommandBuffer,@fInternalImages[TInternalImage.ReconstructedDepthInterpolatedFrame],PpvUInt32(@DepthClearValue)^);
  GlobalBarrier(aCommandBuffer,
                TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT));

  ScheduleDispatch(aCommandBuffer,aInFlightFrameIndex,TPass.ReconstructPreviousDepth,RenderDispatchSizeX,RenderDispatchSizeY);
  ScheduleDispatch(aCommandBuffer,aInFlightFrameIndex,TPass.GameMotionVectorField,RenderDispatchSizeX,RenderDispatchSizeY);

  fBoundConstantSlots[TResource.ConstantsInpaintingPyramid]:=ConstantSlotGameVectorFieldInpaintingPyramid;
  ScheduleDispatch(aCommandBuffer,aInFlightFrameIndex,TPass.GameVectorFieldInpaintingPyramid,SPDDispatchX,SPDDispatchY);

  ScheduleDispatch(aCommandBuffer,aInFlightFrameIndex,TPass.OpticalFlowVectorField,OpticalFlowDispatchSizeX,OpticalFlowDispatchSizeY);

  ScheduleDispatch(aCommandBuffer,aInFlightFrameIndex,TPass.DisocclusionMask,RenderDispatchSizeX,RenderDispatchSizeY);

 end;

 ScheduleDispatch(aCommandBuffer,aInFlightFrameIndex,TPass.Interpolation,DisplayDispatchSizeX,DisplayDispatchSizeY);

 // inpainting pyramid, as SPD over the display area
 SPDSetup(fWidth,fHeight,-1,SPDDispatchX,SPDDispatchY,SPDNumWorkGroups,SPDCountMips);
 FillChar(InpaintingPyramidConstants,SizeOf(TInpaintingPyramidConstants),#0);
 InpaintingPyramidConstants.NumWorkGroups:=SPDNumWorkGroups;
 InpaintingPyramidConstants.Mips:=SPDCountMips;
 InpaintingPyramidConstants.WorkGroupOffset[0]:=0;
 InpaintingPyramidConstants.WorkGroupOffset[1]:=0;
 fConstantBuffers[aInFlightFrameIndex].UpdateData(InpaintingPyramidConstants,ConstantSlotInpaintingPyramid*ConstantBufferSlotSize,SizeOf(TInpaintingPyramidConstants));
 fBoundConstantSlots[TResource.ConstantsInpaintingPyramid]:=ConstantSlotInpaintingPyramid;
 ScheduleDispatch(aCommandBuffer,aInFlightFrameIndex,TPass.InpaintingPyramid,SPDDispatchX,SPDDispatchY);

 ScheduleDispatch(aCommandBuffer,aInFlightFrameIndex,TPass.Inpainting,DisplayDispatchSizeX,DisplayDispatchSizeY);

 if aDescription.DebugView then begin
  SPDSetup(RenderWidth,RenderHeight,-1,SPDDispatchX,SPDDispatchY,SPDNumWorkGroups,SPDCountMips);
  fBoundConstantSlots[TResource.ConstantsInpaintingPyramid]:=ConstantSlotGameVectorFieldInpaintingPyramid;
  ScheduleDispatch(aCommandBuffer,aInFlightFrameIndex,TPass.GameVectorFieldInpaintingPyramid,SPDDispatchX,SPDDispatchY);
  ScheduleDispatch(aCommandBuffer,aInFlightFrameIndex,TPass.DebugView,DisplayDispatchSizeX,DisplayDispatchSizeY);
 end;

end;

function TpvScene3DRendererFrameGenerationBackendFSR3.Generate(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex:TpvSizeInt;const aDescription:TGenerateDescription):boolean;
var Index,Half,Level:TpvSizeInt;
    InternalImage:TInternalImage;
    ImageMemoryBarriers:array of TVkImageMemoryBarrier;
    Reset:boolean;
    MaxLuminance:TpvFloat;
 procedure AddImageMemoryBarrier(const aItem:PInternalImageItem);
 begin
  FillChar(ImageMemoryBarriers[Index],SizeOf(TVkImageMemoryBarrier),#0);
  ImageMemoryBarriers[Index].sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
  ImageMemoryBarriers[Index].srcAccessMask:=0;
  ImageMemoryBarriers[Index].dstAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
  ImageMemoryBarriers[Index].oldLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
  ImageMemoryBarriers[Index].newLayout:=VK_IMAGE_LAYOUT_GENERAL;
  ImageMemoryBarriers[Index].srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  ImageMemoryBarriers[Index].dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  ImageMemoryBarriers[Index].image:=aItem^.Image.Handle;
  ImageMemoryBarriers[Index].subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
  ImageMemoryBarriers[Index].subresourceRange.baseMipLevel:=0;
  ImageMemoryBarriers[Index].subresourceRange.levelCount:=aItem^.CountMipLevels;
  ImageMemoryBarriers[Index].subresourceRange.baseArrayLayer:=0;
  ImageMemoryBarriers[Index].subresourceRange.layerCount:=1;
  inc(Index);
 end;
begin

 result:=false;

 // FSR 3.1 makes the one frame halfway between the two rendered ones, and that needs the depth and the
 // motion vectors of the current one.
 if (aDescription.Index<>1) or
    (aDescription.Current.Depth.ImageView=VK_NULL_HANDLE) or
    (aDescription.Current.MotionVectors.ImageView=VK_NULL_HANDLE) then begin
  exit;
 end;

 FillChar(fPassUses,SizeOf(fPassUses),#0);

 // First of all, wait for everything that wrote or read the internal images before - the passes of the
 // interval before.
 GlobalBarrier(aCommandBuffer,
               TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
               TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
               TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
               TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT));

 if fFirstExecution then begin

  // The internal images go into GENERAL once and stay there.
  ImageMemoryBarriers:=nil;
  SetLength(ImageMemoryBarriers,(ord(High(TInternalImage))-ord(Low(TInternalImage))+1)+(2*2*CountOpticalFlowPyramidLevels));
  Index:=0;
  for InternalImage:=Low(TInternalImage) to High(TInternalImage) do begin
   AddImageMemoryBarrier(@fInternalImages[InternalImage]);
  end;
  for Half:=0 to 1 do begin
   for Level:=0 to CountOpticalFlowPyramidLevels-1 do begin
    AddImageMemoryBarrier(@fOpticalFlowInputs[Half,Level]);
    AddImageMemoryBarrier(@fOpticalFlows[Half,Level]);
   end;
  end;
  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    Index,@ImageMemoryBarriers[0]);
  ImageMemoryBarriers:=nil;

  // The read-only data, which the SDK hands its backend as initial data at creation, and the counters,
  // which the setup pass keeps from then on.
  ClearImage(aCommandBuffer,@fInternalImages[TInternalImage.DefaultDistortionField],0.0,0.0,0.0,0.0);
  ClearImageUInt(aCommandBuffer,@fInternalImages[TInternalImage.OpticalFlowConfidence],0);
  aCommandBuffer.CmdFillBuffer(fCounters.Handle,0,VK_WHOLE_SIZE,0);

  GlobalBarrier(aCommandBuffer,
                TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT));

 end;

 // A reset at the start, when asked for, and where the rendered frame before this interval is not the one
 // the frame interpolation saw last, which the SDK detects as disjoint frame IDs: then neither the luma
 // pyramid of the optical flow nor its history fit any more.
 Reset:=fFirstExecution or
        aDescription.Reset or
        (aDescription.Previous.FrameIndex<>fPreviousFrameIndex);
 fPreviousFrameIndex:=aDescription.Current.FrameIndex;
 fFirstExecution:=false;

 // scRGB, where 1.0 is 80 nits, with the peak of the output as the maximum, so that the color goes from
 // zero to one in the luminance FSR works with.
 if aDescription.Current.PeakBrightness>0.0 then begin
  MaxLuminance:=80.0*aDescription.Current.PeakBrightness;
 end else begin
  MaxLuminance:=80.0;
 end;

 DispatchOpticalFlow(aCommandBuffer,aInFlightFrameIndex,aDescription,Reset,0.0,MaxLuminance);

 DispatchFrameInterpolation(aCommandBuffer,aInFlightFrameIndex,aDescription,Reset,0.0,MaxLuminance);

 result:=true;

end;

initialization
 TpvScene3DRendererFrameGenerationBackendFSR3.RegisterBackend(TpvScene3DRendererFrameGeneration.FSR);
end.
