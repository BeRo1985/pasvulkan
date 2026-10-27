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
unit PasVulkan.Scene3D.Renderer.TemporalUpscalerBackend.FSR3;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

// AMD FidelityFX Super Resolution 3.1 upscaling as a temporal upscaler backend.
//
// The GPU side are AMD's own GLSL passes from the FidelityFX SDK v1.1.4, unchanged, in
// assets/shaders/scene3d/fsr3 (see the README.txt there). What is here is the host side, a port of the
// SDK's sdk/src/components/fsr3upscaler/ffx_fsr3upscaler.cpp: the internal resources, the constants, and
// the order of the dispatches. It stays close to that file on purpose, in names and in the order things
// happen, so that a newer FSR 3.1.x can be followed by comparing the two.
//
// What differs from the SDK is only what the SDK leaves to its backend: here every internal image lives
// in VK_IMAGE_LAYOUT_GENERAL for good, a global memory barrier stands between each two dispatches, and
// nothing is aliased, which costs some memory but keeps the history images out of harm's way. Exposure is
// always the automatic one FSR works out itself, the same as the SDK's FFX_FSR3UPSCALER_ENABLE_AUTO_EXPOSURE.
// The motion vectors are taken at render resolution and without jitter, and the input is HDR.

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
     PasVulkan.Scene3D.Renderer.TemporalUpscalerBackend;

type { TpvScene3DRendererTemporalUpscalerBackendFSR3 }
     TpvScene3DRendererTemporalUpscalerBackendFSR3=class(TpvScene3DRendererTemporalUpscalerBackend)
      public
       const CountParities=2;
             LanczosLUTWidth=128;
             CountSPDMipViews=6;
       type // The passes as they are dispatched. AccumulateSharpen is Accumulate built for the RCAS pass
            // behind it, which then writes the output instead. DebugView draws, when asked for, six views of
            // the inputs and the internal state over the output, in a grid of three by three with the
            // picture itself left in the middle row.
            TPass=
             (
              PrepareInputs,
              LumaPyramid,
              ShadingChangePyramid,
              ShadingChange,
              PrepareReactivity,
              LumaInstability,
              Accumulate,
              AccumulateSharpen,
              RCAS,
              DebugView
             );
            // The internal images, as the SDK's internal resource identifiers. DilatedDepth,
            // DilatedMotionVectors and ReconstructedPreviousNearestDepth are the three the SDK leaves to the
            // application as shared resources; here the backend owns them like the others.
            TInternalImage=
             (
              Accumulation1,
              Accumulation2,
              Luma1,
              Luma2,
              IntermediateFP16x1,
              ShadingChange,
              NewLocks,
              InternalUpscaled1,
              InternalUpscaled2,
              SPDMips,
              FarthestDepthMip1,
              LumaHistory1,
              LumaHistory2,
              SPDAtomicCount,
              DilatedReactiveMasks,
              LanczosLUT,
              DefaultReactivity,
              FrameInfo,
              DilatedDepth,
              DilatedMotionVectors,
              ReconstructedPreviousNearestDepth
             );
            // What a binding of a pass refers to. The ...Read and ...Write pairs are the two halves of a
            // ping-pong pair, which swap from frame to frame.
            TResource=
             (
              InputColor,
              InputDepth,
              InputMotionVectors,
              InputExposure,
              ReactiveMask,
              TransparencyAndCompositionMask,
              DilatedMotionVectors,
              DilatedDepth,
              ReconstructedPreviousNearestDepth,
              FarthestDepth,
              FarthestDepthMip1,
              CurrentLuma,
              PreviousLuma,
              SPDGlobalAtomic,
              FrameInfo,
              SPDMip0,
              SPDMip1,
              SPDMip2,
              SPDMip3,
              SPDMip4,
              SPDMip5,
              SPDMips,
              ShadingChange,
              DilatedReactiveMasks,
              NewLocks,
              AccumulationRead,
              AccumulationWrite,
              LumaHistoryRead,
              LumaHistoryWrite,
              LumaInstability,
              InternalUpscaledRead,
              InternalUpscaledWrite,
              RCASInput,
              LanczosLUT,
              UpscaledOutput,
              ConstantsFSR3Upscaler,
              ConstantsSPD,
              ConstantsRCAS,
              PointClampSampler,
              LinearClampSampler
             );
            TBindingKind=
             (
              SampledImage,
              StorageImage,
              UniformBuffer,
              Sampler
             );
            TBinding=record
             Binding:TpvUInt32;
             Kind:TBindingKind;
             Resource:TResource;
            end;
            TBindings=array of TBinding;
            // Fsr3UpscalerConstants of the SDK, and cbFSR3Upscaler of the shaders, which is std140 with the
            // same layout.
            TConstants=packed record
             RenderSize:array[0..1] of TpvInt32;
             PreviousFrameRenderSize:array[0..1] of TpvInt32;
             UpscaleSize:array[0..1] of TpvInt32;
             PreviousFrameUpscaleSize:array[0..1] of TpvInt32;
             MaxRenderSize:array[0..1] of TpvInt32;
             MaxUpscaleSize:array[0..1] of TpvInt32;
             DeviceToViewDepth:array[0..3] of TpvFloat;
             JitterOffset:array[0..1] of TpvFloat;
             PreviousFrameJitterOffset:array[0..1] of TpvFloat;
             MotionVectorScale:array[0..1] of TpvFloat;
             DownscaleFactor:array[0..1] of TpvFloat;
             MotionVectorJitterCancellation:array[0..1] of TpvFloat;
             TanHalfFOV:TpvFloat;
             JitterPhaseCount:TpvFloat;
             DeltaTime:TpvFloat;
             DeltaPreExposure:TpvFloat;
             ViewSpaceToMetersFactor:TpvFloat;
             FrameIndex:TpvFloat;
             VelocityFactor:TpvFloat;
             ReactivenessScale:TpvFloat;
             ShadingChangeScale:TpvFloat;
             AccumulationAddedPerFrame:TpvFloat;
             MinDisocclusionAccumulation:TpvFloat;
            end;
            PConstants=^TConstants;
            // Fsr3UpscalerSpdConstants and cbSPD.
            TSPDConstants=packed record
             Mips:TpvUInt32;
             NumWorkGroups:TpvUInt32;
             WorkGroupOffset:array[0..1] of TpvUInt32;
             RenderSize:array[0..1] of TpvUInt32;
            end;
            PSPDConstants=^TSPDConstants;
            // Fsr3UpscalerRcasConstants and cbRCAS.
            TRCASConstants=packed record
             RCASConfig:array[0..3] of TpvUInt32;
            end;
            PRCASConstants=^TRCASConstants;
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
            TConstantBuffers=array[0..2] of TpvVulkanBuffer;
      private
       fPasses:array[TPass] of TPassItem;
       fInternalImages:array[TInternalImage] of TInternalImageItem;
       fSPDMipViews:array[0..CountSPDMipViews-1] of TpvVulkanImageView;
       fPointClampSampler:TpvVulkanSampler;
       fLinearClampSampler:TpvVulkanSampler;
       fLanczosLUTStagingBuffer:TpvVulkanBuffer;
       fConstantBuffers:array[0..MaxInFlightFrames-1] of TConstantBuffers;
       fDescriptorPool:TpvVulkanDescriptorPool;
       fDescriptorSets:array[0..MaxInFlightFrames-1,0..CountParities-1,TPass] of TpvVulkanDescriptorSet;
       fInputs:array[0..MaxInFlightFrames-1] of TInputs;
       fConstants:TConstants;
       fFirstExecution:boolean;
       fResourceFrameIndex:TpvUInt32;
       fPreExposure:TpvFloat;
       fPreviousFramePreExposure:TpvFloat;
       fUseLanczosLUT:boolean;
       procedure CreateInternalImage(const aInternalImage:TInternalImage;
                                     const aFormat:TVkFormat;
                                     const aWidth:TpvInt32;
                                     const aHeight:TpvInt32;
                                     const aCountMipLevels:TpvInt32;
                                     const aUsage:TVkImageUsageFlags);
       procedure DestroyInternalImage(const aInternalImage:TInternalImage);
       class function GetPassBindings(const aPass:TPass):TBindings; static;
       function GetShaderFileName(const aPass:TPass):TpvUTF8String;
       function GetImageView(const aResource:TResource;const aInFlightFrameIndex,aParity:TpvSizeInt):TVkImageView;
       function GetImageLayout(const aResource:TResource):TVkImageLayout;
       procedure WriteDescriptorSets(const aInFlightFrameIndex:TpvSizeInt);
       procedure SetupDeviceDepthToViewSpaceDepthParams(const aProjectionMatrix:TpvMatrix4x4);
       procedure GlobalBarrier(const aCommandBuffer:TpvVulkanCommandBuffer;
                               const aSrcStageMask:TVkPipelineStageFlags;
                               const aDstStageMask:TVkPipelineStageFlags;
                               const aSrcAccessMask:TVkAccessFlags;
                               const aDstAccessMask:TVkAccessFlags);
       procedure ClearImage(const aCommandBuffer:TpvVulkanCommandBuffer;const aInternalImage:TInternalImage;const aR,aG,aB,aA:TpvFloat);
       procedure ClearImageUInt(const aCommandBuffer:TpvVulkanCommandBuffer;const aInternalImage:TInternalImage;const aValue:TpvUInt32);
       procedure ScheduleDispatch(const aCommandBuffer:TpvVulkanCommandBuffer;
                                  const aInFlightFrameIndex:TpvSizeInt;
                                  const aParity:TpvSizeInt;
                                  const aPass:TPass;
                                  const aDispatchX:TpvUInt32;
                                  const aDispatchY:TpvUInt32);
      public
       constructor Create(const aDevice:TpvVulkanDevice;
                          const aPipelineCache:TpvVulkanPipelineCache;
                          const aCountInFlightFrames:TpvSizeInt;
                          const aRenderWidth:TpvInt32;
                          const aRenderHeight:TpvInt32;
                          const aUpscaleWidth:TpvInt32;
                          const aUpscaleHeight:TpvInt32;
                          const aInvertedDepth:boolean;
                          const aInfiniteDepth:boolean;
                          const aName:TpvUTF8String); override;
       destructor Destroy; override;
       // Whether a device can run FSR 3.1 at all: the formatless storage image writes and the extended
       // storage formats the passes declare, the quad subgroup operations of the two pyramid passes, and
       // storage support for every format of the internal images.
       class function IsSupported(const aDevice:TpvVulkanDevice):boolean; static;
       class function GetJitterPhaseCount(const aRenderWidth,aDisplayWidth:TpvInt32):TpvInt32; static;
       procedure SetInputs(const aInFlightFrameIndex:TpvSizeInt;const aInputs:TInputs); override;
       procedure Dispatch(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex:TpvSizeInt;const aDescription:TDispatchDescription); override;
     end;

implementation

const FLT_EPSILON=1.192092896e-7;

// The weight of a Lanczos2 filter at the given distance, as the SDK computes its LUT from.
function Lanczos2(const aValue:TpvDouble):TpvDouble;
begin
 if abs(aValue)<FLT_EPSILON then begin
  result:=1.0;
 end else begin
  result:=(sin(PI*aValue)/(PI*aValue))*(sin(0.5*PI*aValue)/(0.5*PI*aValue));
 end;
end;

{ TpvScene3DRendererTemporalUpscalerBackendFSR3 }

constructor TpvScene3DRendererTemporalUpscalerBackendFSR3.Create(const aDevice:TpvVulkanDevice;
                                                                 const aPipelineCache:TpvVulkanPipelineCache;
                                                                 const aCountInFlightFrames:TpvSizeInt;
                                                                 const aRenderWidth:TpvInt32;
                                                                 const aRenderHeight:TpvInt32;
                                                                 const aUpscaleWidth:TpvInt32;
                                                                 const aUpscaleHeight:TpvInt32;
                                                                 const aInvertedDepth:boolean;
                                                                 const aInfiniteDepth:boolean;
                                                                 const aName:TpvUTF8String);
const StorageUsage=TVkImageUsageFlags(VK_IMAGE_USAGE_STORAGE_BIT) or
                   TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT) or
                   TVkImageUsageFlags(VK_IMAGE_USAGE_TRANSFER_DST_BIT);
      ReadOnlyUsage=TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT) or
                    TVkImageUsageFlags(VK_IMAGE_USAGE_TRANSFER_DST_BIT);
var Pass:TPass;
    PassItem:PPassItem;
    BindingIndex,InFlightFrameIndex,Parity,MipIndex,Index:TpvSizeInt;
    Stream:TStream;
    CountSampledImages,CountStorageImages,CountUniformBuffers,CountSamplers,CountSets:TpvUInt32;
    DescriptorType:TVkDescriptorType;
    RenderWidthDiv2,RenderHeightDiv2:TpvInt32;
    LanczosWeights:array[0..LanczosLUTWidth-1] of TpvInt16;
    x:TpvDouble;
begin

 inherited Create(aDevice,
                  aPipelineCache,
                  aCountInFlightFrames,
                  aRenderWidth,
                  aRenderHeight,
                  aUpscaleWidth,
                  aUpscaleHeight,
                  aInvertedDepth,
                  aInfiniteDepth,
                  aName);

 FillChar(fPasses,SizeOf(fPasses),#0);
 FillChar(fInternalImages,SizeOf(fInternalImages),#0);
 FillChar(fSPDMipViews,SizeOf(fSPDMipViews),#0);
 FillChar(fConstantBuffers,SizeOf(fConstantBuffers),#0);
 FillChar(fDescriptorSets,SizeOf(fDescriptorSets),#0);
 FillChar(fInputs,SizeOf(fInputs),#0);
 FillChar(fConstants,SizeOf(TConstants),#0);

 fFirstExecution:=true;
 fResourceFrameIndex:=0;
 fPreExposure:=0.0;
 fPreviousFramePreExposure:=0.0;

 // The SDK takes the LUT on GPUs that run both wave32 and wave64, which is AMD's RDNA, and the reference
 // Lanczos everywhere else.
 fUseLanczosLUT:=TpvVulkanVendorID(fDevice.PhysicalDevice.Properties.vendorID)=TpvVulkanVendorID.AMD;

 // The defaults the SDK sets at context creation.
 fConstants.MaxUpscaleSize[0]:=fUpscaleWidth;
 fConstants.MaxUpscaleSize[1]:=fUpscaleHeight;
 fConstants.VelocityFactor:=1.0;
 fConstants.ReactivenessScale:=1.0;
 fConstants.ShadingChangeScale:=1.0;
 fConstants.AccumulationAddedPerFrame:=1.0/3.0;
 fConstants.MinDisocclusionAccumulation:=-1.0/3.0;

 RenderWidthDiv2:=Max(1,fRenderWidth div 2);
 RenderHeightDiv2:=Max(1,fRenderHeight div 2);

 // The internal resources, with the formats and sizes of the SDK's internalSurfaceDesc and of its shared
 // resource descriptions.
 CreateInternalImage(TInternalImage.Accumulation1,VK_FORMAT_R8_UNORM,fRenderWidth,fRenderHeight,1,StorageUsage);
 CreateInternalImage(TInternalImage.Accumulation2,VK_FORMAT_R8_UNORM,fRenderWidth,fRenderHeight,1,StorageUsage);
 CreateInternalImage(TInternalImage.Luma1,VK_FORMAT_R16_SFLOAT,fRenderWidth,fRenderHeight,1,StorageUsage);
 CreateInternalImage(TInternalImage.Luma2,VK_FORMAT_R16_SFLOAT,fRenderWidth,fRenderHeight,1,StorageUsage);
 CreateInternalImage(TInternalImage.IntermediateFP16x1,VK_FORMAT_R16_SFLOAT,fRenderWidth,fRenderHeight,1,StorageUsage);
 CreateInternalImage(TInternalImage.ShadingChange,VK_FORMAT_R8_UNORM,RenderWidthDiv2,RenderHeightDiv2,1,StorageUsage);
 CreateInternalImage(TInternalImage.NewLocks,VK_FORMAT_R8_UNORM,fUpscaleWidth,fUpscaleHeight,1,StorageUsage);
 CreateInternalImage(TInternalImage.InternalUpscaled1,VK_FORMAT_R16G16B16A16_SFLOAT,fUpscaleWidth,fUpscaleHeight,1,StorageUsage);
 CreateInternalImage(TInternalImage.InternalUpscaled2,VK_FORMAT_R16G16B16A16_SFLOAT,fUpscaleWidth,fUpscaleHeight,1,StorageUsage);
 CreateInternalImage(TInternalImage.SPDMips,VK_FORMAT_R16G16_SFLOAT,RenderWidthDiv2,RenderHeightDiv2,0,StorageUsage);
 CreateInternalImage(TInternalImage.FarthestDepthMip1,VK_FORMAT_R16_SFLOAT,RenderWidthDiv2,RenderHeightDiv2,1,StorageUsage);
 CreateInternalImage(TInternalImage.LumaHistory1,VK_FORMAT_R16G16B16A16_SFLOAT,fRenderWidth,fRenderHeight,1,StorageUsage);
 CreateInternalImage(TInternalImage.LumaHistory2,VK_FORMAT_R16G16B16A16_SFLOAT,fRenderWidth,fRenderHeight,1,StorageUsage);
 CreateInternalImage(TInternalImage.SPDAtomicCount,VK_FORMAT_R32_UINT,1,1,1,StorageUsage);
 CreateInternalImage(TInternalImage.DilatedReactiveMasks,VK_FORMAT_R8G8B8A8_UNORM,fRenderWidth,fRenderHeight,1,StorageUsage);
 CreateInternalImage(TInternalImage.LanczosLUT,VK_FORMAT_R16_SNORM,LanczosLUTWidth,1,1,ReadOnlyUsage);
 CreateInternalImage(TInternalImage.DefaultReactivity,VK_FORMAT_R8_UNORM,1,1,1,ReadOnlyUsage);
 CreateInternalImage(TInternalImage.FrameInfo,VK_FORMAT_R32G32B32A32_SFLOAT,1,1,1,StorageUsage);
 CreateInternalImage(TInternalImage.DilatedDepth,VK_FORMAT_R32_SFLOAT,fRenderWidth,fRenderHeight,1,StorageUsage);
 CreateInternalImage(TInternalImage.DilatedMotionVectors,VK_FORMAT_R16G16_SFLOAT,fRenderWidth,fRenderHeight,1,StorageUsage);
 CreateInternalImage(TInternalImage.ReconstructedPreviousNearestDepth,VK_FORMAT_R32_UINT,fRenderWidth,fRenderHeight,1,StorageUsage);

 // The storage views of the luma pyramid's first six mip levels. Where the pyramid has fewer, the last
 // one stands in for the missing ones, which SPD then never writes, since it stops at the mip count.
 for MipIndex:=0 to CountSPDMipViews-1 do begin
  fSPDMipViews[MipIndex]:=TpvVulkanImageView.Create(fDevice,
                                                   fInternalImages[TInternalImage.SPDMips].Image,
                                                   VK_IMAGE_VIEW_TYPE_2D,
                                                   VK_FORMAT_R16G16_SFLOAT,
                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                   TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                   Min(MipIndex,fInternalImages[TInternalImage.SPDMips].CountMipLevels-1),
                                                   1,
                                                   0,
                                                   1);
  fDevice.DebugUtils.SetObjectName(fSPDMipViews[MipIndex].Handle,VK_OBJECT_TYPE_IMAGE_VIEW,fName+'.SPDMipView['+IntToStr(MipIndex)+']');
 end;

 // The two samplers every pass declares, s_PointClamp and s_LinearClamp.
 fPointClampSampler:=TpvVulkanSampler.Create(fDevice,
                                             TVkFilter.VK_FILTER_NEAREST,
                                             TVkFilter.VK_FILTER_NEAREST,
                                             TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_NEAREST,
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
 fDevice.DebugUtils.SetObjectName(fPointClampSampler.Handle,VK_OBJECT_TYPE_SAMPLER,fName+'.PointClampSampler');

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

 // The Lanczos2 LUT, the same 128 signed-normalized weights over the distance 0..2 as in the SDK. It goes
 // into its image with the first dispatch, from this staging buffer.
 for Index:=0 to LanczosLUTWidth-1 do begin
  x:=(2.0*Index)/(LanczosLUTWidth-1);
  LanczosWeights[Index]:=TpvInt16(round(Lanczos2(x)*32767.0));
 end;
 fLanczosLUTStagingBuffer:=TpvVulkanBuffer.Create(fDevice,
                                                  SizeOf(LanczosWeights),
                                                  TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT),
                                                  TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                  [],
                                                  TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  [],
                                                  0,
                                                  pvAllocationGroupIDScene3DStatic,
                                                  fName+'.LanczosLUTStagingBuffer');
 fLanczosLUTStagingBuffer.UpdateData(LanczosWeights,0,SizeOf(LanczosWeights));

 // The constants, one set per in-flight frame, since a frame's are still read by the GPU while the next
 // frame's are written.
 for InFlightFrameIndex:=0 to fCountInFlightFrames-1 do begin
  for Index:=0 to 2 do begin
   fConstantBuffers[InFlightFrameIndex,Index]:=TpvVulkanBuffer.Create(fDevice,
                                                                      256,
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
                                                                      fName+'.ConstantBuffers['+IntToStr(InFlightFrameIndex)+','+IntToStr(Index)+']');
  end;
 end;

 // The passes: bindings, shader, descriptor set layout, pipeline. The bindings and their numbers are the
 // FSR3UPSCALER_BIND_* defines at the top of each pass's GLSL file.
 CountSampledImages:=0;
 CountStorageImages:=0;
 CountUniformBuffers:=0;
 CountSamplers:=0;
 CountSets:=0;
 for Pass:=Low(TPass) to High(TPass) do begin

  PassItem:=@fPasses[Pass];

  PassItem^.Bindings:=GetPassBindings(Pass);

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
     inc(CountSampledImages);
    end;
    TBindingKind.StorageImage:begin
     DescriptorType:=VK_DESCRIPTOR_TYPE_STORAGE_IMAGE;
     inc(CountStorageImages);
    end;
    TBindingKind.UniformBuffer:begin
     DescriptorType:=VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
     inc(CountUniformBuffers);
    end;
    else {TBindingKind.Sampler:}begin
     DescriptorType:=VK_DESCRIPTOR_TYPE_SAMPLER;
     inc(CountSamplers);
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

  inc(CountSets);

 end;

 // One descriptor set per pass, in-flight frame and parity, since both the images from outside and the
 // ping-pong halves differ between them.
 fDescriptorPool:=TpvVulkanDescriptorPool.Create(fDevice,
                                                 TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                 CountSets*TpvUInt32(fCountInFlightFrames*CountParities));
 if CountSampledImages>0 then begin
  fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE,CountSampledImages*TpvUInt32(fCountInFlightFrames*CountParities));
 end;
 if CountStorageImages>0 then begin
  fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,CountStorageImages*TpvUInt32(fCountInFlightFrames*CountParities));
 end;
 if CountUniformBuffers>0 then begin
  fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,CountUniformBuffers*TpvUInt32(fCountInFlightFrames*CountParities));
 end;
 if CountSamplers>0 then begin
  fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_SAMPLER,CountSamplers*TpvUInt32(fCountInFlightFrames*CountParities));
 end;
 fDescriptorPool.Initialize;
 fDevice.DebugUtils.SetObjectName(fDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,fName+'.DescriptorPool');

 for InFlightFrameIndex:=0 to fCountInFlightFrames-1 do begin
  for Parity:=0 to CountParities-1 do begin
   for Pass:=Low(TPass) to High(TPass) do begin
    fDescriptorSets[InFlightFrameIndex,Parity,Pass]:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fPasses[Pass].DescriptorSetLayout);
    fDevice.DebugUtils.SetObjectName(fDescriptorSets[InFlightFrameIndex,Parity,Pass].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,fName+'.DescriptorSet['+IntToStr(InFlightFrameIndex)+','+IntToStr(Parity)+','+IntToStr(ord(Pass))+']');
   end;
  end;
 end;

end;

destructor TpvScene3DRendererTemporalUpscalerBackendFSR3.Destroy;
var Pass:TPass;
    InternalImage:TInternalImage;
    InFlightFrameIndex,Parity,MipIndex,Index:TpvSizeInt;
begin

 for InFlightFrameIndex:=0 to fCountInFlightFrames-1 do begin
  for Parity:=0 to CountParities-1 do begin
   for Pass:=Low(TPass) to High(TPass) do begin
    FreeAndNil(fDescriptorSets[InFlightFrameIndex,Parity,Pass]);
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
  for Index:=0 to 2 do begin
   FreeAndNil(fConstantBuffers[InFlightFrameIndex,Index]);
  end;
 end;

 FreeAndNil(fLanczosLUTStagingBuffer);

 FreeAndNil(fLinearClampSampler);
 FreeAndNil(fPointClampSampler);

 for MipIndex:=0 to CountSPDMipViews-1 do begin
  FreeAndNil(fSPDMipViews[MipIndex]);
 end;

 for InternalImage:=Low(TInternalImage) to High(TInternalImage) do begin
  DestroyInternalImage(InternalImage);
 end;

 inherited Destroy;

end;

class function TpvScene3DRendererTemporalUpscalerBackendFSR3.IsSupported(const aDevice:TpvVulkanDevice):boolean;
const StorageFormats:array[0..8] of TVkFormat=
       (
        VK_FORMAT_R8_UNORM,
        VK_FORMAT_R8G8B8A8_UNORM,
        VK_FORMAT_R16_SFLOAT,
        VK_FORMAT_R16G16_SFLOAT,
        VK_FORMAT_R16G16B16A16_SFLOAT,
        VK_FORMAT_R32_UINT,
        VK_FORMAT_R32_SFLOAT,
        VK_FORMAT_R32G32B32A32_SFLOAT,
        VK_FORMAT_R16_SNORM
       );
var Index:TpvSizeInt;
    FormatProperties:TVkFormatProperties;
    RequiredFeatures:TVkFormatFeatureFlags;
begin
 result:=assigned(aDevice) and
         (aDevice.PhysicalDevice.Features.shaderStorageImageExtendedFormats<>VK_FALSE) and
         (aDevice.PhysicalDevice.Features.shaderStorageImageWriteWithoutFormat<>VK_FALSE) and
         ((aDevice.PhysicalDevice.Vulkan11Properties.subgroupSupportedStages and TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT))<>0) and
         ((aDevice.PhysicalDevice.Vulkan11Properties.subgroupSupportedOperations and TVkSubgroupFeatureFlags(VK_SUBGROUP_FEATURE_BASIC_BIT))<>0) and
         ((aDevice.PhysicalDevice.Vulkan11Properties.subgroupSupportedOperations and TVkSubgroupFeatureFlags(VK_SUBGROUP_FEATURE_QUAD_BIT))<>0);
 if result then begin
  for Index:=Low(StorageFormats) to High(StorageFormats) do begin
   FormatProperties:=aDevice.PhysicalDevice.GetFormatProperties(StorageFormats[Index]);
   // The LUT is only ever sampled, everything else is written as a storage image as well.
   if StorageFormats[Index]=VK_FORMAT_R16_SNORM then begin
    RequiredFeatures:=TVkFormatFeatureFlags(VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT);
   end else begin
    RequiredFeatures:=TVkFormatFeatureFlags(VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT) or TVkFormatFeatureFlags(VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT);
   end;
   if (FormatProperties.optimalTilingFeatures and RequiredFeatures)<>RequiredFeatures then begin
    result:=false;
    break;
   end;
  end;
 end;
end;

// ffxFsr3UpscalerGetJitterPhaseCount of the SDK.
class function TpvScene3DRendererTemporalUpscalerBackendFSR3.GetJitterPhaseCount(const aRenderWidth,aDisplayWidth:TpvInt32):TpvInt32;
begin
 result:=Max(1,trunc(8.0*sqr(Max(1,aDisplayWidth)/Max(1,aRenderWidth))));
end;

procedure TpvScene3DRendererTemporalUpscalerBackendFSR3.CreateInternalImage(const aInternalImage:TInternalImage;
                                                                            const aFormat:TVkFormat;
                                                                            const aWidth:TpvInt32;
                                                                            const aHeight:TpvInt32;
                                                                            const aCountMipLevels:TpvInt32;
                                                                            const aUsage:TVkImageUsageFlags);
var Item:PInternalImageItem;
    MemoryRequirements:TVkMemoryRequirements;
    CountMipLevels:TpvInt32;
begin

 Item:=@fInternalImages[aInternalImage];

 // A mip count of zero means the full chain down to one texel, as in the SDK.
 if aCountMipLevels<=0 then begin
  CountMipLevels:=IntLog2(Max(1,Max(aWidth,aHeight)))+1;
 end else begin
  CountMipLevels:=aCountMipLevels;
 end;

 Item^.Format:=aFormat;
 Item^.Width:=Max(1,aWidth);
 Item^.Height:=Max(1,aHeight);
 Item^.CountMipLevels:=CountMipLevels;

 Item^.Image:=TpvVulkanImage.Create(fDevice,
                                    0,
                                    VK_IMAGE_TYPE_2D,
                                    aFormat,
                                    Item^.Width,
                                    Item^.Height,
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
 fDevice.DebugUtils.SetObjectName(Item^.Image.Handle,VK_OBJECT_TYPE_IMAGE,fName+'.InternalImage['+IntToStr(ord(aInternalImage))+']');

 fDevice.Commands.GetImageMemoryRequirements(fDevice.Handle,Item^.Image.Handle,@MemoryRequirements);

 Item^.MemoryBlock:=fDevice.MemoryManager.AllocateMemoryBlock([],
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
                                                              fName+'.InternalImageMemory['+IntToStr(ord(aInternalImage))+']');

 VulkanCheckResult(fDevice.Commands.BindImageMemory(fDevice.Handle,
                                                    Item^.Image.Handle,
                                                    Item^.MemoryBlock.MemoryChunk.Handle,
                                                    Item^.MemoryBlock.Offset));

 Item^.ImageView:=TpvVulkanImageView.Create(fDevice,
                                            Item^.Image,
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
 fDevice.DebugUtils.SetObjectName(Item^.ImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,fName+'.InternalImageView['+IntToStr(ord(aInternalImage))+']');

end;

procedure TpvScene3DRendererTemporalUpscalerBackendFSR3.DestroyInternalImage(const aInternalImage:TInternalImage);
var Item:PInternalImageItem;
begin
 Item:=@fInternalImages[aInternalImage];
 FreeAndNil(Item^.ImageView);
 FreeAndNil(Item^.Image);
 if assigned(Item^.MemoryBlock) then begin
  Item^.MemoryBlock.Free;
  Item^.MemoryBlock:=nil;
 end;
end;

class function TpvScene3DRendererTemporalUpscalerBackendFSR3.GetPassBindings(const aPass:TPass):TBindings;
var Count:TpvSizeInt;
 procedure Add(const aBinding:TpvUInt32;const aKind:TBindingKind;const aResource:TResource);
 begin
  if Count>=length(result) then begin
   SetLength(result,(Count+1)*2);
  end;
  result[Count].Binding:=aBinding;
  result[Count].Kind:=aKind;
  result[Count].Resource:=aResource;
  inc(Count);
 end;
begin
 result:=nil;
 Count:=0;
 case aPass of
  TPass.PrepareInputs:begin
   Add(0,TBindingKind.SampledImage,TResource.InputMotionVectors);
   Add(1,TBindingKind.SampledImage,TResource.InputDepth);
   Add(2,TBindingKind.SampledImage,TResource.InputColor);
   Add(3,TBindingKind.StorageImage,TResource.DilatedMotionVectors);
   Add(4,TBindingKind.StorageImage,TResource.DilatedDepth);
   Add(5,TBindingKind.StorageImage,TResource.ReconstructedPreviousNearestDepth);
   Add(6,TBindingKind.StorageImage,TResource.FarthestDepth);
   Add(7,TBindingKind.StorageImage,TResource.CurrentLuma);
   Add(8,TBindingKind.UniformBuffer,TResource.ConstantsFSR3Upscaler);
  end;
  TPass.LumaPyramid:begin
   Add(0,TBindingKind.SampledImage,TResource.CurrentLuma);
   Add(1,TBindingKind.SampledImage,TResource.FarthestDepth);
   Add(2,TBindingKind.StorageImage,TResource.SPDGlobalAtomic);
   Add(3,TBindingKind.StorageImage,TResource.FrameInfo);
   Add(4,TBindingKind.StorageImage,TResource.SPDMip0);
   Add(5,TBindingKind.StorageImage,TResource.SPDMip1);
   Add(6,TBindingKind.StorageImage,TResource.SPDMip2);
   Add(7,TBindingKind.StorageImage,TResource.SPDMip3);
   Add(8,TBindingKind.StorageImage,TResource.SPDMip4);
   Add(9,TBindingKind.StorageImage,TResource.SPDMip5);
   Add(10,TBindingKind.StorageImage,TResource.FarthestDepthMip1);
   Add(11,TBindingKind.UniformBuffer,TResource.ConstantsFSR3Upscaler);
   Add(12,TBindingKind.UniformBuffer,TResource.ConstantsSPD);
  end;
  TPass.ShadingChangePyramid:begin
   Add(0,TBindingKind.SampledImage,TResource.CurrentLuma);
   Add(1,TBindingKind.SampledImage,TResource.PreviousLuma);
   Add(2,TBindingKind.SampledImage,TResource.DilatedMotionVectors);
   Add(3,TBindingKind.SampledImage,TResource.InputExposure);
   Add(4,TBindingKind.StorageImage,TResource.SPDGlobalAtomic);
   Add(5,TBindingKind.StorageImage,TResource.SPDMip0);
   Add(6,TBindingKind.StorageImage,TResource.SPDMip1);
   Add(7,TBindingKind.StorageImage,TResource.SPDMip2);
   Add(8,TBindingKind.StorageImage,TResource.SPDMip3);
   Add(9,TBindingKind.StorageImage,TResource.SPDMip4);
   Add(10,TBindingKind.StorageImage,TResource.SPDMip5);
   Add(11,TBindingKind.UniformBuffer,TResource.ConstantsFSR3Upscaler);
   Add(12,TBindingKind.UniformBuffer,TResource.ConstantsSPD);
  end;
  TPass.ShadingChange:begin
   Add(0,TBindingKind.SampledImage,TResource.SPDMips);
   Add(1,TBindingKind.StorageImage,TResource.ShadingChange);
   Add(2,TBindingKind.UniformBuffer,TResource.ConstantsFSR3Upscaler);
  end;
  TPass.PrepareReactivity:begin
   Add(0,TBindingKind.SampledImage,TResource.ReconstructedPreviousNearestDepth);
   Add(1,TBindingKind.SampledImage,TResource.DilatedMotionVectors);
   Add(2,TBindingKind.SampledImage,TResource.DilatedDepth);
   Add(3,TBindingKind.SampledImage,TResource.ReactiveMask);
   Add(4,TBindingKind.SampledImage,TResource.TransparencyAndCompositionMask);
   Add(5,TBindingKind.SampledImage,TResource.AccumulationRead);
   Add(6,TBindingKind.SampledImage,TResource.ShadingChange);
   Add(7,TBindingKind.SampledImage,TResource.CurrentLuma);
   Add(8,TBindingKind.SampledImage,TResource.InputExposure);
   Add(9,TBindingKind.StorageImage,TResource.DilatedReactiveMasks);
   Add(10,TBindingKind.StorageImage,TResource.NewLocks);
   Add(11,TBindingKind.StorageImage,TResource.AccumulationWrite);
   Add(12,TBindingKind.UniformBuffer,TResource.ConstantsFSR3Upscaler);
  end;
  TPass.LumaInstability:begin
   Add(0,TBindingKind.SampledImage,TResource.InputExposure);
   Add(1,TBindingKind.SampledImage,TResource.DilatedReactiveMasks);
   Add(2,TBindingKind.SampledImage,TResource.DilatedMotionVectors);
   Add(3,TBindingKind.SampledImage,TResource.FrameInfo);
   Add(4,TBindingKind.SampledImage,TResource.LumaHistoryRead);
   Add(5,TBindingKind.SampledImage,TResource.FarthestDepthMip1);
   Add(6,TBindingKind.SampledImage,TResource.CurrentLuma);
   Add(7,TBindingKind.StorageImage,TResource.LumaHistoryWrite);
   Add(8,TBindingKind.StorageImage,TResource.LumaInstability);
   Add(9,TBindingKind.UniformBuffer,TResource.ConstantsFSR3Upscaler);
  end;
  TPass.Accumulate,
  TPass.AccumulateSharpen:begin
   Add(0,TBindingKind.SampledImage,TResource.InputExposure);
   Add(1,TBindingKind.SampledImage,TResource.DilatedReactiveMasks);
   Add(2,TBindingKind.SampledImage,TResource.DilatedMotionVectors);
   Add(3,TBindingKind.SampledImage,TResource.InternalUpscaledRead);
   Add(4,TBindingKind.SampledImage,TResource.LanczosLUT);
   Add(5,TBindingKind.SampledImage,TResource.FarthestDepthMip1);
   Add(6,TBindingKind.SampledImage,TResource.CurrentLuma);
   Add(7,TBindingKind.SampledImage,TResource.LumaInstability);
   Add(8,TBindingKind.SampledImage,TResource.InputColor);
   Add(9,TBindingKind.StorageImage,TResource.InternalUpscaledWrite);
   Add(10,TBindingKind.StorageImage,TResource.UpscaledOutput);
   Add(11,TBindingKind.StorageImage,TResource.NewLocks);
   Add(12,TBindingKind.UniformBuffer,TResource.ConstantsFSR3Upscaler);
  end;
  TPass.RCAS:begin
   Add(0,TBindingKind.SampledImage,TResource.InputExposure);
   Add(1,TBindingKind.SampledImage,TResource.RCASInput);
   Add(2,TBindingKind.StorageImage,TResource.UpscaledOutput);
   Add(3,TBindingKind.UniformBuffer,TResource.ConstantsFSR3Upscaler);
   Add(4,TBindingKind.UniformBuffer,TResource.ConstantsRCAS);
  end;
  TPass.DebugView:begin
   Add(0,TBindingKind.SampledImage,TResource.DilatedReactiveMasks);
   Add(1,TBindingKind.SampledImage,TResource.DilatedMotionVectors);
   Add(2,TBindingKind.SampledImage,TResource.DilatedDepth);
   Add(3,TBindingKind.SampledImage,TResource.InternalUpscaledWrite);
   Add(4,TBindingKind.SampledImage,TResource.InputExposure);
   Add(5,TBindingKind.StorageImage,TResource.UpscaledOutput);
   Add(6,TBindingKind.UniformBuffer,TResource.ConstantsFSR3Upscaler);
  end;
 end;
 // s_PointClamp and s_LinearClamp, declared by the shared callbacks of every pass.
 Add(1000,TBindingKind.Sampler,TResource.PointClampSampler);
 Add(1001,TBindingKind.Sampler,TResource.LinearClampSampler);
 SetLength(result,Count);
end;

function TpvScene3DRendererTemporalUpscalerBackendFSR3.GetShaderFileName(const aPass:TPass):TpvUTF8String;
begin
 case aPass of
  TPass.PrepareInputs:begin
   result:='fsr3upscaler_prepare_inputs';
  end;
  TPass.LumaPyramid:begin
   result:='fsr3upscaler_luma_pyramid';
  end;
  TPass.ShadingChangePyramid:begin
   result:='fsr3upscaler_shading_change_pyramid';
  end;
  TPass.ShadingChange:begin
   result:='fsr3upscaler_shading_change';
  end;
  TPass.PrepareReactivity:begin
   result:='fsr3upscaler_prepare_reactivity';
  end;
  TPass.LumaInstability:begin
   result:='fsr3upscaler_luma_instability';
  end;
  TPass.Accumulate,
  TPass.AccumulateSharpen:begin
   result:='fsr3upscaler_accumulate';
   if fUseLanczosLUT then begin
    result:=result+'_lut';
   end;
   if aPass=TPass.AccumulateSharpen then begin
    result:=result+'_sharpen';
   end;
  end;
  TPass.RCAS:begin
   result:='fsr3upscaler_rcas';
  end;
  else {TPass.DebugView:}begin
   result:='fsr3upscaler_debug_view';
  end;
 end;
 if fInvertedDepth then begin
  result:=result+'_inverteddepth';
 end;
 result:=result+'_comp.spv';
end;

function TpvScene3DRendererTemporalUpscalerBackendFSR3.GetImageView(const aResource:TResource;const aInFlightFrameIndex,aParity:TpvSizeInt):TVkImageView;
var IsOddFrame:boolean;
begin
 // The ping-pong halves as the SDK picks them: in an odd frame the second one of each pair is read and
 // the first one written, in an even frame the other way round.
 IsOddFrame:=aParity<>0;
 case aResource of
  TResource.InputColor:begin
   result:=fInputs[aInFlightFrameIndex].Color.ImageView;
  end;
  TResource.InputDepth:begin
   result:=fInputs[aInFlightFrameIndex].Depth.ImageView;
  end;
  TResource.InputMotionVectors:begin
   result:=fInputs[aInFlightFrameIndex].MotionVectors.ImageView;
  end;
  TResource.InputExposure,
  TResource.FrameInfo:begin
   result:=fInternalImages[TInternalImage.FrameInfo].ImageView.Handle;
  end;
  TResource.ReactiveMask:begin
   if fInputs[aInFlightFrameIndex].Reactive.ImageView<>VK_NULL_HANDLE then begin
    result:=fInputs[aInFlightFrameIndex].Reactive.ImageView;
   end else begin
    result:=fInternalImages[TInternalImage.DefaultReactivity].ImageView.Handle;
   end;
  end;
  TResource.TransparencyAndCompositionMask:begin
   result:=fInternalImages[TInternalImage.DefaultReactivity].ImageView.Handle;
  end;
  TResource.DilatedMotionVectors:begin
   result:=fInternalImages[TInternalImage.DilatedMotionVectors].ImageView.Handle;
  end;
  TResource.DilatedDepth:begin
   result:=fInternalImages[TInternalImage.DilatedDepth].ImageView.Handle;
  end;
  TResource.ReconstructedPreviousNearestDepth:begin
   result:=fInternalImages[TInternalImage.ReconstructedPreviousNearestDepth].ImageView.Handle;
  end;
  TResource.FarthestDepth,
  TResource.LumaInstability:begin
   // The farthest depth and the luma instability share one image, the one is done with before the other
   // is written, as in the SDK.
   result:=fInternalImages[TInternalImage.IntermediateFP16x1].ImageView.Handle;
  end;
  TResource.FarthestDepthMip1:begin
   result:=fInternalImages[TInternalImage.FarthestDepthMip1].ImageView.Handle;
  end;
  TResource.CurrentLuma:begin
   if IsOddFrame then begin
    result:=fInternalImages[TInternalImage.Luma2].ImageView.Handle;
   end else begin
    result:=fInternalImages[TInternalImage.Luma1].ImageView.Handle;
   end;
  end;
  TResource.PreviousLuma:begin
   if IsOddFrame then begin
    result:=fInternalImages[TInternalImage.Luma1].ImageView.Handle;
   end else begin
    result:=fInternalImages[TInternalImage.Luma2].ImageView.Handle;
   end;
  end;
  TResource.SPDGlobalAtomic:begin
   result:=fInternalImages[TInternalImage.SPDAtomicCount].ImageView.Handle;
  end;
  TResource.SPDMip0..TResource.SPDMip5:begin
   result:=fSPDMipViews[ord(aResource)-ord(TResource.SPDMip0)].Handle;
  end;
  TResource.SPDMips:begin
   result:=fInternalImages[TInternalImage.SPDMips].ImageView.Handle;
  end;
  TResource.ShadingChange:begin
   result:=fInternalImages[TInternalImage.ShadingChange].ImageView.Handle;
  end;
  TResource.DilatedReactiveMasks:begin
   result:=fInternalImages[TInternalImage.DilatedReactiveMasks].ImageView.Handle;
  end;
  TResource.NewLocks:begin
   result:=fInternalImages[TInternalImage.NewLocks].ImageView.Handle;
  end;
  TResource.AccumulationRead:begin
   if IsOddFrame then begin
    result:=fInternalImages[TInternalImage.Accumulation2].ImageView.Handle;
   end else begin
    result:=fInternalImages[TInternalImage.Accumulation1].ImageView.Handle;
   end;
  end;
  TResource.AccumulationWrite:begin
   if IsOddFrame then begin
    result:=fInternalImages[TInternalImage.Accumulation1].ImageView.Handle;
   end else begin
    result:=fInternalImages[TInternalImage.Accumulation2].ImageView.Handle;
   end;
  end;
  TResource.LumaHistoryRead:begin
   if IsOddFrame then begin
    result:=fInternalImages[TInternalImage.LumaHistory2].ImageView.Handle;
   end else begin
    result:=fInternalImages[TInternalImage.LumaHistory1].ImageView.Handle;
   end;
  end;
  TResource.LumaHistoryWrite:begin
   if IsOddFrame then begin
    result:=fInternalImages[TInternalImage.LumaHistory1].ImageView.Handle;
   end else begin
    result:=fInternalImages[TInternalImage.LumaHistory2].ImageView.Handle;
   end;
  end;
  TResource.InternalUpscaledRead:begin
   if IsOddFrame then begin
    result:=fInternalImages[TInternalImage.InternalUpscaled2].ImageView.Handle;
   end else begin
    result:=fInternalImages[TInternalImage.InternalUpscaled1].ImageView.Handle;
   end;
  end;
  TResource.InternalUpscaledWrite,
  TResource.RCASInput:begin
   // RCAS sharpens the upscaled color just written, so its input is the write half of the pair.
   if IsOddFrame then begin
    result:=fInternalImages[TInternalImage.InternalUpscaled1].ImageView.Handle;
   end else begin
    result:=fInternalImages[TInternalImage.InternalUpscaled2].ImageView.Handle;
   end;
  end;
  TResource.LanczosLUT:begin
   result:=fInternalImages[TInternalImage.LanczosLUT].ImageView.Handle;
  end;
  TResource.UpscaledOutput:begin
   result:=fInputs[aInFlightFrameIndex].Output.ImageView;
  end;
  else begin
   result:=VK_NULL_HANDLE;
  end;
 end;
end;

function TpvScene3DRendererTemporalUpscalerBackendFSR3.GetImageLayout(const aResource:TResource):TVkImageLayout;
begin
 // The inputs come from the frame graph, which leaves them ready to be sampled; the reactive mask falls
 // back to an internal image, which is in GENERAL like all of them.
 case aResource of
  TResource.InputColor,
  TResource.InputDepth,
  TResource.InputMotionVectors:begin
   result:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
  end;
  else begin
   result:=VK_IMAGE_LAYOUT_GENERAL;
  end;
 end;
end;

procedure TpvScene3DRendererTemporalUpscalerBackendFSR3.WriteDescriptorSets(const aInFlightFrameIndex:TpvSizeInt);
var Parity,BindingIndex:TpvSizeInt;
    Pass:TPass;
    Binding:TBinding;
    DescriptorSet:TpvVulkanDescriptorSet;
    ImageLayout:TVkImageLayout;
    Buffer:TpvVulkanBuffer;
begin
 for Parity:=0 to CountParities-1 do begin
  for Pass:=Low(TPass) to High(TPass) do begin
   DescriptorSet:=fDescriptorSets[aInFlightFrameIndex,Parity,Pass];
   for BindingIndex:=0 to length(fPasses[Pass].Bindings)-1 do begin
    Binding:=fPasses[Pass].Bindings[BindingIndex];
    case Binding.Kind of
     TBindingKind.SampledImage,
     TBindingKind.StorageImage:begin
      ImageLayout:=GetImageLayout(Binding.Resource);
      if (Binding.Resource=TResource.ReactiveMask) and (fInputs[aInFlightFrameIndex].Reactive.ImageView<>VK_NULL_HANDLE) then begin
       ImageLayout:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
      end;
      if Binding.Kind=TBindingKind.SampledImage then begin
       DescriptorSet.WriteToDescriptorSet(Binding.Binding,
                                          0,
                                          1,
                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                                          [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                         GetImageView(Binding.Resource,aInFlightFrameIndex,Parity),
                                                                         ImageLayout)],
                                          [],
                                          [],
                                          false);
      end else begin
       DescriptorSet.WriteToDescriptorSet(Binding.Binding,
                                          0,
                                          1,
                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                          [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                         GetImageView(Binding.Resource,aInFlightFrameIndex,Parity),
                                                                         VK_IMAGE_LAYOUT_GENERAL)],
                                          [],
                                          [],
                                          false);
      end;
     end;
     TBindingKind.UniformBuffer:begin
      case Binding.Resource of
       TResource.ConstantsSPD:begin
        Buffer:=fConstantBuffers[aInFlightFrameIndex,1];
       end;
       TResource.ConstantsRCAS:begin
        Buffer:=fConstantBuffers[aInFlightFrameIndex,2];
       end;
       else {TResource.ConstantsFSR3Upscaler:}begin
        Buffer:=fConstantBuffers[aInFlightFrameIndex,0];
       end;
      end;
      DescriptorSet.WriteToDescriptorSet(Binding.Binding,
                                         0,
                                         1,
                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                         [],
                                         [Buffer.DescriptorBufferInfo],
                                         [],
                                         false);
     end;
     else {TBindingKind.Sampler:}begin
      if Binding.Resource=TResource.PointClampSampler then begin
       DescriptorSet.WriteToDescriptorSet(Binding.Binding,
                                          0,
                                          1,
                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLER),
                                          [TVkDescriptorImageInfo.Create(fPointClampSampler.Handle,
                                                                         VK_NULL_HANDLE,
                                                                         VK_IMAGE_LAYOUT_UNDEFINED)],
                                          [],
                                          [],
                                          false);
      end else begin
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
   end;
   DescriptorSet.Flush;
  end;
 end;
end;

procedure TpvScene3DRendererTemporalUpscalerBackendFSR3.SetInputs(const aInFlightFrameIndex:TpvSizeInt;const aInputs:TInputs);
begin
 fInputs[aInFlightFrameIndex]:=aInputs;
 WriteDescriptorSets(aInFlightFrameIndex);
end;

// setupDeviceDepthToViewSpaceDepthParams of the SDK, though taken from the projection matrix itself rather
// than rebuilt from near, far and field of view, so that it follows the renderer's projection exactly,
// whichever depth convention and near plane it uses in a frame. The depth is d=(P22*z+P32)/(-z) for the
// view space z in front of the camera, which makes the distance P32/(d+P22), and FSR's view depth is
// DeviceToViewDepth[1]/(d-DeviceToViewDepth[0]).
procedure TpvScene3DRendererTemporalUpscalerBackendFSR3.SetupDeviceDepthToViewSpaceDepthParams(const aProjectionMatrix:TpvMatrix4x4);
begin
 fConstants.DeviceToViewDepth[0]:=-aProjectionMatrix.RawComponents[2,2];
 // An infinite far plane leaves P22 at zero in the reversed case, where the SDK puts FLT_EPSILON instead,
 // to keep the farthest depth finite.
 if abs(fConstants.DeviceToViewDepth[0])<FLT_EPSILON then begin
  fConstants.DeviceToViewDepth[0]:=-FLT_EPSILON;
 end;
 fConstants.DeviceToViewDepth[1]:=aProjectionMatrix.RawComponents[3,2];
 fConstants.DeviceToViewDepth[2]:=1.0/Max(FLT_EPSILON,abs(aProjectionMatrix.RawComponents[0,0]));
 fConstants.DeviceToViewDepth[3]:=1.0/Max(FLT_EPSILON,abs(aProjectionMatrix.RawComponents[1,1]));
end;

procedure TpvScene3DRendererTemporalUpscalerBackendFSR3.GlobalBarrier(const aCommandBuffer:TpvVulkanCommandBuffer;
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

procedure TpvScene3DRendererTemporalUpscalerBackendFSR3.ClearImage(const aCommandBuffer:TpvVulkanCommandBuffer;const aInternalImage:TInternalImage;const aR,aG,aB,aA:TpvFloat);
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
 ImageSubresourceRange.levelCount:=fInternalImages[aInternalImage].CountMipLevels;
 ImageSubresourceRange.baseArrayLayer:=0;
 ImageSubresourceRange.layerCount:=1;
 aCommandBuffer.CmdClearColorImage(fInternalImages[aInternalImage].Image.Handle,
                                   VK_IMAGE_LAYOUT_GENERAL,
                                   @ClearColorValue,
                                   1,
                                   @ImageSubresourceRange);
end;

procedure TpvScene3DRendererTemporalUpscalerBackendFSR3.ClearImageUInt(const aCommandBuffer:TpvVulkanCommandBuffer;const aInternalImage:TInternalImage;const aValue:TpvUInt32);
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
 ImageSubresourceRange.levelCount:=fInternalImages[aInternalImage].CountMipLevels;
 ImageSubresourceRange.baseArrayLayer:=0;
 ImageSubresourceRange.layerCount:=1;
 aCommandBuffer.CmdClearColorImage(fInternalImages[aInternalImage].Image.Handle,
                                   VK_IMAGE_LAYOUT_GENERAL,
                                   @ClearColorValue,
                                   1,
                                   @ImageSubresourceRange);
end;

procedure TpvScene3DRendererTemporalUpscalerBackendFSR3.ScheduleDispatch(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                         const aInFlightFrameIndex:TpvSizeInt;
                                                                         const aParity:TpvSizeInt;
                                                                         const aPass:TPass;
                                                                         const aDispatchX:TpvUInt32;
                                                                         const aDispatchY:TpvUInt32);
begin
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPasses[aPass].Pipeline.Handle);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                      fPasses[aPass].PipelineLayout.Handle,
                                      0,
                                      1,
                                      @fDescriptorSets[aInFlightFrameIndex,aParity,aPass].Handle,
                                      0,
                                      nil);
 aCommandBuffer.CmdDispatch(Max(1,aDispatchX),Max(1,aDispatchY),1);
 // Every pass reads what the ones before it wrote, so each one is followed by a barrier.
 GlobalBarrier(aCommandBuffer,
               TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
               TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
               TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
               TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT));
end;

procedure TpvScene3DRendererTemporalUpscalerBackendFSR3.Dispatch(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex:TpvSizeInt;const aDescription:TDispatchDescription);
const ThreadGroupWorkRegionDim=8;
      ThreadGroupWorkRegionDimRCAS=16;
var InternalImage:TInternalImage;
    ImageMemoryBarriers:array[TInternalImage] of TVkImageMemoryBarrier;
    BufferImageCopy:TVkBufferImageCopy;
    Parity:TpvSizeInt;
    IsOddFrame,ResetAccumulation,EnableSharpening:boolean;
    AccumulationReadImage:TInternalImage;
    JitterPhaseCount,JitterPhaseCountDelta:TpvInt32;
    DispatchSrcX,DispatchSrcY,DispatchDstX,DispatchDstY:TpvInt32;
    DispatchShadingChangePassX,DispatchShadingChangePassY:TpvInt32;
    SPDDispatchThreadGroupCountX,SPDDispatchThreadGroupCountY,SPDEndIndexX,SPDEndIndexY:TpvUInt32;
    SPDConstants:TSPDConstants;
    RCASConstants:TRCASConstants;
    Sharpness:TpvFloat;
    HalfSharpness:TpvHalfFloat;
    DepthClearValue:TpvFloat;
begin

 // First of all, wait for everything that wrote or read the internal images before - the previous
 // frame's passes - and for the inputs, which the frame graph has made ready to be sampled.
 GlobalBarrier(aCommandBuffer,
               TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
               TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
               TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
               TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT));

 if fFirstExecution then begin

  // The internal images go into GENERAL once and stay there.
  for InternalImage:=Low(TInternalImage) to High(TInternalImage) do begin
   FillChar(ImageMemoryBarriers[InternalImage],SizeOf(TVkImageMemoryBarrier),#0);
   ImageMemoryBarriers[InternalImage].sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
   ImageMemoryBarriers[InternalImage].srcAccessMask:=0;
   ImageMemoryBarriers[InternalImage].dstAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
   ImageMemoryBarriers[InternalImage].oldLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
   ImageMemoryBarriers[InternalImage].newLayout:=VK_IMAGE_LAYOUT_GENERAL;
   ImageMemoryBarriers[InternalImage].srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
   ImageMemoryBarriers[InternalImage].dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
   ImageMemoryBarriers[InternalImage].image:=fInternalImages[InternalImage].Image.Handle;
   ImageMemoryBarriers[InternalImage].subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
   ImageMemoryBarriers[InternalImage].subresourceRange.baseMipLevel:=0;
   ImageMemoryBarriers[InternalImage].subresourceRange.levelCount:=fInternalImages[InternalImage].CountMipLevels;
   ImageMemoryBarriers[InternalImage].subresourceRange.baseArrayLayer:=0;
   ImageMemoryBarriers[InternalImage].subresourceRange.layerCount:=1;
  end;
  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    length(ImageMemoryBarriers),@ImageMemoryBarriers[Low(TInternalImage)]);

  // The read-only data, which the SDK hands its backend as initial data at creation.
  FillChar(BufferImageCopy,SizeOf(TVkBufferImageCopy),#0);
  BufferImageCopy.bufferOffset:=0;
  BufferImageCopy.bufferRowLength:=0;
  BufferImageCopy.bufferImageHeight:=0;
  BufferImageCopy.imageSubresource.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
  BufferImageCopy.imageSubresource.mipLevel:=0;
  BufferImageCopy.imageSubresource.baseArrayLayer:=0;
  BufferImageCopy.imageSubresource.layerCount:=1;
  BufferImageCopy.imageExtent.width:=LanczosLUTWidth;
  BufferImageCopy.imageExtent.height:=1;
  BufferImageCopy.imageExtent.depth:=1;
  aCommandBuffer.CmdCopyBufferToImage(fLanczosLUTStagingBuffer.Handle,
                                      fInternalImages[TInternalImage.LanczosLUT].Image.Handle,
                                      VK_IMAGE_LAYOUT_GENERAL,
                                      1,
                                      @BufferImageCopy);
  ClearImage(aCommandBuffer,TInternalImage.DefaultReactivity,0.0,0.0,0.0,0.0);

  ClearImage(aCommandBuffer,TInternalImage.Accumulation1,0.0,0.0,0.0,0.0);
  ClearImage(aCommandBuffer,TInternalImage.Accumulation2,0.0,0.0,0.0,0.0);
  ClearImage(aCommandBuffer,TInternalImage.Luma1,0.0,0.0,0.0,0.0);
  ClearImage(aCommandBuffer,TInternalImage.Luma2,0.0,0.0,0.0,0.0);

 end;

 // Prepare per frame descriptor tables
 IsOddFrame:=(fResourceFrameIndex and 1)<>0;
 if IsOddFrame then begin
  Parity:=1;
  AccumulationReadImage:=TInternalImage.Accumulation2;
 end else begin
  Parity:=0;
  AccumulationReadImage:=TInternalImage.Accumulation1;
 end;

 ResetAccumulation:=aDescription.Reset or fFirstExecution;
 fFirstExecution:=false;

 EnableSharpening:=aDescription.Sharpness>0.0;

 fConstants.PreviousFrameJitterOffset[0]:=fConstants.JitterOffset[0];
 fConstants.PreviousFrameJitterOffset[1]:=fConstants.JitterOffset[1];
 fConstants.JitterOffset[0]:=aDescription.JitterOffset.x;
 fConstants.JitterOffset[1]:=aDescription.JitterOffset.y;

 fConstants.PreviousFrameRenderSize[0]:=fConstants.RenderSize[0];
 fConstants.PreviousFrameRenderSize[1]:=fConstants.RenderSize[1];
 fConstants.RenderSize[0]:=Max(1,aDescription.RenderWidth);
 fConstants.RenderSize[1]:=Max(1,aDescription.RenderHeight);
 fConstants.MaxRenderSize[0]:=fRenderWidth;
 fConstants.MaxRenderSize[1]:=fRenderHeight;

 // The tangent of half the horizontal field of view, which is the inverse of the projection's x scale.
 fConstants.TanHalfFOV:=1.0/Max(FLT_EPSILON,abs(aDescription.ProjectionMatrix.RawComponents[0,0]));
 fConstants.ViewSpaceToMetersFactor:=1.0;

 // compute params to enable device depth to view space depth computation in shader
 SetupDeviceDepthToViewSpaceDepthParams(aDescription.ProjectionMatrix);

 fConstants.PreviousFrameUpscaleSize[0]:=fConstants.UpscaleSize[0];
 fConstants.PreviousFrameUpscaleSize[1]:=fConstants.UpscaleSize[1];
 fConstants.UpscaleSize[0]:=fUpscaleWidth;
 fConstants.UpscaleSize[1]:=fUpscaleHeight;

 fConstants.DownscaleFactor[0]:=fConstants.RenderSize[0]/fConstants.UpscaleSize[0];
 fConstants.DownscaleFactor[1]:=fConstants.RenderSize[1]/fConstants.UpscaleSize[1];

 // calculate pre-exposure relevant factors
 fConstants.DeltaPreExposure:=1.0;
 fPreviousFramePreExposure:=fPreExposure;
 if aDescription.PreExposure<>0.0 then begin
  fPreExposure:=aDescription.PreExposure;
 end else begin
  fPreExposure:=1.0;
 end;
 if fPreviousFramePreExposure>0.0 then begin
  fConstants.DeltaPreExposure:=fPreExposure/fPreviousFramePreExposure;
 end;

 // motion vector data, at render resolution
 fConstants.MotionVectorScale[0]:=aDescription.MotionVectorScale.x/fConstants.RenderSize[0];
 fConstants.MotionVectorScale[1]:=aDescription.MotionVectorScale.y/fConstants.RenderSize[1];

 // The motion vectors come without jitter, so there is nothing to cancel.
 fConstants.MotionVectorJitterCancellation[0]:=0.0;
 fConstants.MotionVectorJitterCancellation[1]:=0.0;

 // lock data, assuming jitter sequence length computation for now
 JitterPhaseCount:=GetJitterPhaseCount(fConstants.RenderSize[0],fConstants.UpscaleSize[0]);

 // init on first frame
 if ResetAccumulation or (fConstants.JitterPhaseCount=0.0) then begin
  fConstants.JitterPhaseCount:=JitterPhaseCount;
 end else begin
  JitterPhaseCountDelta:=trunc(JitterPhaseCount-fConstants.JitterPhaseCount);
  if JitterPhaseCountDelta>0 then begin
   fConstants.JitterPhaseCount:=fConstants.JitterPhaseCount+1.0;
  end else if JitterPhaseCountDelta<0 then begin
   fConstants.JitterPhaseCount:=fConstants.JitterPhaseCount-1.0;
  end;
 end;

 // delta time in seconds, clamped to [0, 1]
 fConstants.DeltaTime:=Max(0.0,Min(1.0,aDescription.FrameTimeDelta));

 if ResetAccumulation then begin
  fConstants.FrameIndex:=0.0;
 end else begin
  fConstants.FrameIndex:=fConstants.FrameIndex+1.0;
 end;

 DispatchSrcX:=(fConstants.RenderSize[0]+(ThreadGroupWorkRegionDim-1)) div ThreadGroupWorkRegionDim;
 DispatchSrcY:=(fConstants.RenderSize[1]+(ThreadGroupWorkRegionDim-1)) div ThreadGroupWorkRegionDim;
 DispatchDstX:=(fConstants.UpscaleSize[0]+(ThreadGroupWorkRegionDim-1)) div ThreadGroupWorkRegionDim;
 DispatchDstY:=(fConstants.UpscaleSize[1]+(ThreadGroupWorkRegionDim-1)) div ThreadGroupWorkRegionDim;
 DispatchShadingChangePassX:=(trunc(fConstants.RenderSize[0]*0.5)+(ThreadGroupWorkRegionDim-1)) div ThreadGroupWorkRegionDim;
 DispatchShadingChangePassY:=(trunc(fConstants.RenderSize[1]*0.5)+(ThreadGroupWorkRegionDim-1)) div ThreadGroupWorkRegionDim;

 // Clear reconstructed depth for max depth store.
 if ResetAccumulation then begin
  ClearImage(aCommandBuffer,AccumulationReadImage,0.0,0.0,0.0,0.0);
  ClearImage(aCommandBuffer,TInternalImage.SPDMips,0.0,0.0,0.0,0.0);
  // Auto exposure always used to track luma changes in locking logic
  ClearImage(aCommandBuffer,TInternalImage.FrameInfo,-1.0,1.0,0.0,0.0);
 end;

 // The reconstructed previous nearest depth holds the bits of a float as an unsigned integer, so it is
 // cleared with the bits of the farthest depth.
 if fInvertedDepth then begin
  DepthClearValue:=0.0;
 end else begin
  DepthClearValue:=1.0;
 end;
 ClearImageUInt(aCommandBuffer,TInternalImage.ReconstructedPreviousNearestDepth,PpvUInt32(@DepthClearValue)^);

 // The SPD atomic counter and the SPD mips, cleared every frame as the SDK does.
 ClearImageUInt(aCommandBuffer,TInternalImage.SPDAtomicCount,0);
 ClearImage(aCommandBuffer,TInternalImage.SPDMips,0.0,0.0,0.0,0.0);

 GlobalBarrier(aCommandBuffer,
               TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
               TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
               TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
               TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT));

 // ffxSpdSetup of the SDK, for the whole render area and the mip count it works out by itself.
 SPDEndIndexX:=(TpvUInt32(fConstants.RenderSize[0])-1) div 64;
 SPDEndIndexY:=(TpvUInt32(fConstants.RenderSize[1])-1) div 64;
 SPDDispatchThreadGroupCountX:=SPDEndIndexX+1;
 SPDDispatchThreadGroupCountY:=SPDEndIndexY+1;
 SPDConstants.NumWorkGroups:=SPDDispatchThreadGroupCountX*SPDDispatchThreadGroupCountY;
 SPDConstants.Mips:=TpvUInt32(Min(floor(Log2(Max(fConstants.RenderSize[0],fConstants.RenderSize[1]))),12));
 SPDConstants.WorkGroupOffset[0]:=0;
 SPDConstants.WorkGroupOffset[1]:=0;
 SPDConstants.RenderSize[0]:=fConstants.RenderSize[0];
 SPDConstants.RenderSize[1]:=fConstants.RenderSize[1];

 // FsrRcasCon of the SDK, with the sharpness remapped the way the SDK does.
 Sharpness:=exp2(-((-2.0*Min(Max(aDescription.Sharpness,0.0),1.0))+2.0));
 HalfSharpness:=TpvHalfFloat.FromFloat(Sharpness);
 RCASConstants.RCASConfig[0]:=PpvUInt32(@Sharpness)^;
 RCASConstants.RCASConfig[1]:=TpvUInt32(HalfSharpness.Value) or (TpvUInt32(HalfSharpness.Value) shl 16);
 RCASConstants.RCASConfig[2]:=0;
 RCASConstants.RCASConfig[3]:=0;

 // initialize constantBuffers data
 fConstantBuffers[aInFlightFrameIndex,0].UpdateData(fConstants,0,SizeOf(TConstants));
 fConstantBuffers[aInFlightFrameIndex,1].UpdateData(SPDConstants,0,SizeOf(TSPDConstants));
 fConstantBuffers[aInFlightFrameIndex,2].UpdateData(RCASConstants,0,SizeOf(TRCASConstants));

 ScheduleDispatch(aCommandBuffer,aInFlightFrameIndex,Parity,TPass.PrepareInputs,DispatchSrcX,DispatchSrcY);
 ScheduleDispatch(aCommandBuffer,aInFlightFrameIndex,Parity,TPass.LumaPyramid,SPDDispatchThreadGroupCountX,SPDDispatchThreadGroupCountY);
 ScheduleDispatch(aCommandBuffer,aInFlightFrameIndex,Parity,TPass.ShadingChangePyramid,SPDDispatchThreadGroupCountX,SPDDispatchThreadGroupCountY);
 ScheduleDispatch(aCommandBuffer,aInFlightFrameIndex,Parity,TPass.ShadingChange,DispatchShadingChangePassX,DispatchShadingChangePassY);
 ScheduleDispatch(aCommandBuffer,aInFlightFrameIndex,Parity,TPass.PrepareReactivity,DispatchSrcX,DispatchSrcY);
 ScheduleDispatch(aCommandBuffer,aInFlightFrameIndex,Parity,TPass.LumaInstability,DispatchSrcX,DispatchSrcY);

 if EnableSharpening then begin
  ScheduleDispatch(aCommandBuffer,aInFlightFrameIndex,Parity,TPass.AccumulateSharpen,DispatchDstX,DispatchDstY);
  // dispatch RCAS
  ScheduleDispatch(aCommandBuffer,
                   aInFlightFrameIndex,
                   Parity,
                   TPass.RCAS,
                   (fConstants.UpscaleSize[0]+(ThreadGroupWorkRegionDimRCAS-1)) div ThreadGroupWorkRegionDimRCAS,
                   (fConstants.UpscaleSize[1]+(ThreadGroupWorkRegionDimRCAS-1)) div ThreadGroupWorkRegionDimRCAS);
 end else begin
  ScheduleDispatch(aCommandBuffer,aInFlightFrameIndex,Parity,TPass.Accumulate,DispatchDstX,DispatchDstY);
 end;

 if aDescription.DebugView then begin
  ScheduleDispatch(aCommandBuffer,aInFlightFrameIndex,Parity,TPass.DebugView,DispatchDstX,DispatchDstY);
 end;

 fResourceFrameIndex:=(fResourceFrameIndex+1) and 15;

end;

end.
