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
unit PasVulkan.Scene3D.Renderer.FrameGenerationCompositor;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

// What a renderer instance needs for frame generation besides the backend itself.
//
// Every rendered frame is captured into a ring outside of the frame graph, which aliases and reuses its own
// images: the picture without the HUD, as it goes into the HUD composition at the end of the frame, and the
// HUD, and for backends that ask for them the depth and the motion vectors as well. That happens in the frame
// graph, see TpvScene3DRendererPassesFrameGenerationCaptureCustomPass, while the rendered frame goes on into
// its swapchain image as without frame generation.
//
// Right after the rendered frame, the frames generated between the previous one and it are made: for each,
// the application hands out a further swapchain image (see TpvApplication.AcquireGeneratedFrameImage), the
// backend writes the generated picture, and a small render pass puts the HUD of the rendered frame on top
// and converts it into the color space of the swapchain the way the end of every rendered frame does. All of
// that goes into one submit, which waits on the rendered frame and lets its present wait in turn, since the
// application presents the generated frames first.

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.TimerQuery,
     PasVulkan.Application,
     PasVulkan.Scene3D.Renderer.Globals,
     PasVulkan.Scene3D.Renderer.Image2D,
     PasVulkan.Scene3D.Renderer.FrameGenerationBackend;

type { TpvScene3DRendererFrameGenerationCompositor }
     TpvScene3DRendererFrameGenerationCompositor=class
      public
       const CountRingSlots=3;
             ColorSpaceSRGBNonLinearSDR=0;
             ColorSpaceExtendedSRGBLinear=1;
             FlagHUD=1;
             FlagDebug=2;
             // The formats of the resolved depth and of the motion vectors in the renderer, which the copies
             // into the ring keep.
             DepthFormat=VK_FORMAT_R32_SFLOAT;
             MotionVectorsFormat=VK_FORMAT_R16G16_SFLOAT;
       type TPushConstants=packed record
             ColorSpace:TpvUInt32;
             FrameCounter:TpvUInt32;
             Flags:TpvUInt32;
             Index:TpvUInt32;
             Count:TpvUInt32;
            end;
            PPushConstants=^TPushConstants;
            // One rendered frame as captured.
            TRingSlot=record
             Color:TpvScene3DRendererImage2D;
             HUD:TpvScene3DRendererImage2D;
             Depth:TpvScene3DRendererImage2D;
             MotionVectors:TpvScene3DRendererImage2D;
             Frame:TpvScene3DRendererFrameGenerationBackend.TFrame;
             Captured:boolean;
             Reset:boolean;
            end;
            PRingSlot=^TRingSlot;
      private
       fDevice:TpvVulkanDevice;
       fPipelineCache:TpvVulkanPipelineCache;
       fCountInFlightFrames:TpvSizeInt;
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fRenderWidth:TpvInt32;
       fRenderHeight:TpvInt32;
       fColorFormat:TVkFormat;
       fNeedsDepthAndMotionVectors:boolean;
       fHasHUD:boolean;
       fHUDWidth:TpvInt32;
       fHUDHeight:TpvInt32;
       fHUDFormat:TVkFormat;
       fSampler:TpvVulkanSampler;
       fBackend:TpvScene3DRendererFrameGenerationBackend;
       fCountGeneratedFrames:TpvInt32;
       fDebugView:boolean;
       fSuspended:boolean;
       fMinimumFrameRate:TpvFloat;
       fGPUFrameTime:TpvDouble;
       fGenerationGPUTime:TpvDouble;
       fBelowMinimumFrameRate:boolean;
       fName:TpvUTF8String;
       fRing:array[0..CountRingSlots-1] of TRingSlot;
       fCurrentSlot:TpvInt32;
       fPreviousSlot:TpvInt32;
       fFrameCounter:TpvInt64;
       fOutput:TpvScene3DRendererImage2D;
       fCommandPool:TpvVulkanCommandPool;
       fTimerQueries:array[0..MaxInFlightFrames-1] of TpvTimerQuery;
       fCommandBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanCommandBuffer;
       fFences:array[0..MaxInFlightFrames-1] of TpvVulkanFence;
       fFencesReady:array[0..MaxInFlightFrames-1] of boolean;
       fPresentSemaphores:array[0..MaxInFlightFrames-1] of TpvVulkanSemaphore;
       fSwapChain:TpvVulkanSwapChain;
       fColorSpace:TpvUInt32;
       fRenderPass:TpvVulkanRenderPass;
       fSwapChainImageViews:array of TpvVulkanImageView;
       fFrameBuffers:array of TpvVulkanFrameBuffer;
       fVertexShaderModule:TpvVulkanShaderModule;
       fFragmentShaderModule:TpvVulkanShaderModule;
       fVertexShaderStage:TpvVulkanPipelineShaderStage;
       fFragmentShaderStage:TpvVulkanPipelineShaderStage;
       fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fDescriptorPool:TpvVulkanDescriptorPool;
       fDescriptorSets:array[0..MaxInFlightFrames-1,0..MaxGeneratedFramesPerFrame-1] of TpvVulkanDescriptorSet;
       fPipelineLayout:TpvVulkanPipelineLayout;
       fPipeline:TpvVulkanGraphicsPipeline;
       procedure CreateSwapChainResources;
       procedure DestroySwapChainResources;
      public
       constructor Create(const aDevice:TpvVulkanDevice;
                          const aPipelineCache:TpvVulkanPipelineCache;
                          const aCountInFlightFrames:TpvSizeInt;
                          const aWidth:TpvInt32;
                          const aHeight:TpvInt32;
                          const aRenderWidth:TpvInt32;
                          const aRenderHeight:TpvInt32;
                          const aColorFormat:TVkFormat;
                          const aHasHUD:boolean;
                          const aHUDWidth:TpvInt32;
                          const aHUDHeight:TpvInt32;
                          const aHUDFormat:TVkFormat;
                          const aSampler:TpvVulkanSampler;
                          const aBackendClass:TpvScene3DRendererFrameGenerationBackendClass;
                          const aCountGeneratedFrames:TpvInt32;
                          const aInvertedDepth:boolean;
                          const aName:TpvUTF8String); reintroduce;
       destructor Destroy; override;
       // Starts a rendered frame: picks its ring slot and keeps what is known of it on the CPU side.
       procedure BeginFrame(const aFrameIndex:TpvInt64;const aReset:boolean;const aFrameTimeDelta:TpvFloat);
       // The camera of the current rendered frame, see TpvScene3DRendererFrameGenerationBackend.TFrame.
       procedure SetFrameView(const aViewMatrix:TpvMatrix4x4;
                              const aProjectionMatrix:TpvMatrix4x4;
                              const aJitterOffset:TpvVector2;
                              const aMotionVectorScale:TpvVector2;
                              const aZNear:TpvFloat;
                              const aZFar:TpvFloat;
                              const aPeakBrightness:TpvFloat);
       // Records the capture of the current rendered frame into its ring slot, inside the frame graph. The
       // color is a layer of an array image and the HUD a 2D image, both in TRANSFER_SRC_OPTIMAL; the HUD
       // is VK_NULL_HANDLE without one. The depth and the motion vectors, at render resolution, are only
       // taken where the backend asks for them: the depth is a layer of an array image that stays in
       // SHADER_READ_ONLY_OPTIMAL around the capture, the motion vectors a layer of one in
       // TRANSFER_SRC_OPTIMAL.
       procedure Capture(const aCommandBuffer:TpvVulkanCommandBuffer;
                         const aColorImage:TVkImage;
                         const aColorLayer:TpvInt32;
                         const aHUDImage:TVkImage;
                         const aDepthImage:TVkImage;
                         const aDepthLayer:TpvInt32;
                         const aMotionVectorsImage:TVkImage;
                         const aMotionVectorsLayer:TpvInt32);
       // Makes, submits and queues for presentation the frames generated between the previous rendered frame
       // and the current one, right after the current one has been submitted. aWaitSemaphore is what the
       // rendered frame signalled; afterwards it is what its present has to wait on instead.
       procedure DrawGeneratedFrames(const aInFlightFrameIndex:TpvSizeInt;var aWaitSemaphore:TpvVulkanSemaphore);
      published
       property Backend:TpvScene3DRendererFrameGenerationBackend read fBackend;
       property NeedsDepthAndMotionVectors:boolean read fNeedsDepthAndMotionVectors;
       property CountGeneratedFrames:TpvInt32 read fCountGeneratedFrames write fCountGeneratedFrames;
       property DebugView:boolean read fDebugView write fDebugView;
       // No generated frames while set, see TpvScene3DRenderer.FrameGenerationSuspended.
       property Suspended:boolean read fSuspended write fSuspended;
       // See TpvScene3DRenderer.FrameGenerationMinimumFrameRate, together with the GPU time of the last
       // rendered frame that it goes by, in seconds, zero where there is none.
       property MinimumFrameRate:TpvFloat read fMinimumFrameRate write fMinimumFrameRate;
       property GPUFrameTime:TpvDouble read fGPUFrameTime write fGPUFrameTime;
       // The GPU time of the last measured submit of generated frames, backend and composition together,
       // in seconds, zero where there is none.
       property GenerationGPUTime:TpvDouble read fGenerationGPUTime;
     end;

implementation

{ TpvScene3DRendererFrameGenerationCompositor }

constructor TpvScene3DRendererFrameGenerationCompositor.Create(const aDevice:TpvVulkanDevice;
                                                               const aPipelineCache:TpvVulkanPipelineCache;
                                                               const aCountInFlightFrames:TpvSizeInt;
                                                               const aWidth:TpvInt32;
                                                               const aHeight:TpvInt32;
                                                               const aRenderWidth:TpvInt32;
                                                               const aRenderHeight:TpvInt32;
                                                               const aColorFormat:TVkFormat;
                                                               const aHasHUD:boolean;
                                                               const aHUDWidth:TpvInt32;
                                                               const aHUDHeight:TpvInt32;
                                                               const aHUDFormat:TVkFormat;
                                                               const aSampler:TpvVulkanSampler;
                                                               const aBackendClass:TpvScene3DRendererFrameGenerationBackendClass;
                                                               const aCountGeneratedFrames:TpvInt32;
                                                               const aInvertedDepth:boolean;
                                                               const aName:TpvUTF8String);
var SlotIndex:TpvInt32;
    InFlightFrameIndex,Index:TpvSizeInt;
    Stream:TStream;
begin

 inherited Create;

 fDevice:=aDevice;
 fPipelineCache:=aPipelineCache;
 fCountInFlightFrames:=aCountInFlightFrames;
 fWidth:=Max(1,aWidth);
 fHeight:=Max(1,aHeight);
 fRenderWidth:=Max(1,aRenderWidth);
 fRenderHeight:=Max(1,aRenderHeight);
 fColorFormat:=aColorFormat;
 fNeedsDepthAndMotionVectors:=aBackendClass.NeedsDepthAndMotionVectors;
 fHasHUD:=aHasHUD;
 fHUDWidth:=Max(1,aHUDWidth);
 fHUDHeight:=Max(1,aHUDHeight);
 fHUDFormat:=aHUDFormat;
 fSampler:=aSampler;
 fCountGeneratedFrames:=Min(Max(aCountGeneratedFrames,1),MaxGeneratedFramesPerFrame);
 fDebugView:=false;
 fSuspended:=false;
 fMinimumFrameRate:=0.0;
 fGPUFrameTime:=0.0;
 fGenerationGPUTime:=0.0;
 fBelowMinimumFrameRate:=false;
 fName:=aName;

 fCurrentSlot:=0;
 fPreviousSlot:=-1;
 fFrameCounter:=0;

 fBackend:=aBackendClass.Create(fDevice,
                                fPipelineCache,
                                fCountInFlightFrames,
                                fWidth,
                                fHeight,
                                aRenderWidth,
                                aRenderHeight,
                                fColorFormat,
                                aInvertedDepth,
                                fName+'.Backend');

 for SlotIndex:=0 to CountRingSlots-1 do begin
  FillChar(fRing[SlotIndex],SizeOf(TRingSlot),#0);
  fRing[SlotIndex].Color:=TpvScene3DRendererImage2D.Create(fDevice,
                                                           fWidth,
                                                           fHeight,
                                                           fColorFormat,
                                                           false,
                                                           TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                                           TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL),
                                                           TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                           nil,
                                                           0,
                                                           fName+'.Ring['+IntToStr(SlotIndex)+'].Color');
  if fHasHUD then begin
   fRing[SlotIndex].HUD:=TpvScene3DRendererImage2D.Create(fDevice,
                                                          fHUDWidth,
                                                          fHUDHeight,
                                                          fHUDFormat,
                                                          false,
                                                          TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                                          TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL),
                                                          TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                          nil,
                                                          0,
                                                          fName+'.Ring['+IntToStr(SlotIndex)+'].HUD');
  end else begin
   fRing[SlotIndex].HUD:=nil;
  end;
  if fNeedsDepthAndMotionVectors then begin
   fRing[SlotIndex].Depth:=TpvScene3DRendererImage2D.Create(fDevice,
                                                            fRenderWidth,
                                                            fRenderHeight,
                                                            DepthFormat,
                                                            false,
                                                            TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                                            TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL),
                                                            TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                            nil,
                                                            0,
                                                            fName+'.Ring['+IntToStr(SlotIndex)+'].Depth');
   fRing[SlotIndex].MotionVectors:=TpvScene3DRendererImage2D.Create(fDevice,
                                                                    fRenderWidth,
                                                                    fRenderHeight,
                                                                    MotionVectorsFormat,
                                                                    false,
                                                                    TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                                                    TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL),
                                                                    TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                    nil,
                                                                    0,
                                                                    fName+'.Ring['+IntToStr(SlotIndex)+'].MotionVectors');
  end else begin
   fRing[SlotIndex].Depth:=nil;
   fRing[SlotIndex].MotionVectors:=nil;
  end;
 end;

 fOutput:=TpvScene3DRendererImage2D.Create(fDevice,
                                           fWidth,
                                           fHeight,
                                           fColorFormat,
                                           true,
                                           TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                           TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL),
                                           TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                           nil,
                                           0,
                                           fName+'.Output');

 fCommandPool:=TpvVulkanCommandPool.Create(fDevice,
                                           fDevice.UniversalQueueFamilyIndex,
                                           TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
 for InFlightFrameIndex:=0 to MaxInFlightFrames-1 do begin
  fCommandBuffers[InFlightFrameIndex]:=TpvVulkanCommandBuffer.Create(fCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  fDevice.DebugUtils.SetObjectName(fCommandBuffers[InFlightFrameIndex].Handle,VK_OBJECT_TYPE_COMMAND_BUFFER,fName+'.CommandBuffer['+IntToStr(InFlightFrameIndex)+']');
  fFences[InFlightFrameIndex]:=TpvVulkanFence.Create(fDevice);
  fFencesReady[InFlightFrameIndex]:=false;
  fTimerQueries[InFlightFrameIndex]:=TpvTimerQuery.Create(fDevice,1);
  fPresentSemaphores[InFlightFrameIndex]:=TpvVulkanSemaphore.Create(fDevice);
  fDevice.DebugUtils.SetObjectName(fPresentSemaphores[InFlightFrameIndex].Handle,VK_OBJECT_TYPE_SEMAPHORE,fName+'.PresentSemaphore['+IntToStr(InFlightFrameIndex)+']');
 end;

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('fullscreen_vert.spv');
 try
  fVertexShaderModule:=TpvVulkanShaderModule.Create(fDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('framegen_compose_frag.spv');
 try
  fFragmentShaderModule:=TpvVulkanShaderModule.Create(fDevice,Stream);
 finally
  Stream.Free;
 end;

 fVertexShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVertexShaderModule,'main');

 fFragmentShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fFragmentShaderModule,'main');

 fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fDevice);
 fDescriptorSetLayout.AddBinding(0,VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE,1,TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),[]);
 fDescriptorSetLayout.AddBinding(1,VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,1,TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),[]);
 fDescriptorSetLayout.Initialize;

 fDescriptorPool:=TpvVulkanDescriptorPool.Create(fDevice,
                                                 TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                 MaxInFlightFrames*MaxGeneratedFramesPerFrame);
 fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE,MaxInFlightFrames*MaxGeneratedFramesPerFrame);
 fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,MaxInFlightFrames*MaxGeneratedFramesPerFrame);
 fDescriptorPool.Initialize;

 for InFlightFrameIndex:=0 to MaxInFlightFrames-1 do begin
  for Index:=0 to MaxGeneratedFramesPerFrame-1 do begin
   fDescriptorSets[InFlightFrameIndex,Index]:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  end;
 end;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(fDevice);
 fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
 fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),0,SizeOf(TPushConstants));
 fPipelineLayout.Initialize;

 fSwapChain:=nil;
 fRenderPass:=nil;
 fPipeline:=nil;
 fSwapChainImageViews:=nil;
 fFrameBuffers:=nil;

end;

destructor TpvScene3DRendererFrameGenerationCompositor.Destroy;
var SlotIndex:TpvInt32;
    InFlightFrameIndex,Index:TpvSizeInt;
begin
 for InFlightFrameIndex:=0 to MaxInFlightFrames-1 do begin
  if fFencesReady[InFlightFrameIndex] then begin
   fFences[InFlightFrameIndex].WaitFor;
   fFencesReady[InFlightFrameIndex]:=false;
  end;
 end;
 DestroySwapChainResources;
 FreeAndNil(fPipelineLayout);
 for InFlightFrameIndex:=0 to MaxInFlightFrames-1 do begin
  for Index:=0 to MaxGeneratedFramesPerFrame-1 do begin
   FreeAndNil(fDescriptorSets[InFlightFrameIndex,Index]);
  end;
 end;
 FreeAndNil(fDescriptorPool);
 FreeAndNil(fDescriptorSetLayout);
 FreeAndNil(fFragmentShaderStage);
 FreeAndNil(fVertexShaderStage);
 FreeAndNil(fFragmentShaderModule);
 FreeAndNil(fVertexShaderModule);
 for InFlightFrameIndex:=0 to MaxInFlightFrames-1 do begin
  FreeAndNil(fPresentSemaphores[InFlightFrameIndex]);
  FreeAndNil(fTimerQueries[InFlightFrameIndex]);
  FreeAndNil(fFences[InFlightFrameIndex]);
  FreeAndNil(fCommandBuffers[InFlightFrameIndex]);
 end;
 FreeAndNil(fCommandPool);
 FreeAndNil(fOutput);
 for SlotIndex:=0 to CountRingSlots-1 do begin
  FreeAndNil(fRing[SlotIndex].MotionVectors);
  FreeAndNil(fRing[SlotIndex].Depth);
  FreeAndNil(fRing[SlotIndex].HUD);
  FreeAndNil(fRing[SlotIndex].Color);
 end;
 FreeAndNil(fBackend);
 inherited Destroy;
end;

// The render pass, the image views and the framebuffers over the swapchain images and the pipeline, made
// for the swapchain of the moment and made again when it has changed.
procedure TpvScene3DRendererFrameGenerationCompositor.CreateSwapChainResources;
var ImageIndex:TpvSizeInt;
    ColorAttachmentIndex:TpvUInt32;
begin

 fSwapChain:=pvApplication.VulkanSwapChain;

 if fSwapChain.ImageColorSpace=VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT then begin
  fColorSpace:=ColorSpaceExtendedSRGBLinear;
 end else begin
  fColorSpace:=ColorSpaceSRGBNonLinearSDR;
 end;

 fRenderPass:=TpvVulkanRenderPass.Create(fDevice);
 ColorAttachmentIndex:=fRenderPass.AddAttachmentDescription(0,
                                                            fSwapChain.ImageFormat,
                                                            VK_SAMPLE_COUNT_1_BIT,
                                                            VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                            VK_ATTACHMENT_STORE_OP_STORE,
                                                            VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                            VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                            VK_IMAGE_LAYOUT_UNDEFINED,
                                                            VK_IMAGE_LAYOUT_PRESENT_SRC_KHR);
 fRenderPass.AddSubpassDescription(0,
                                   VK_PIPELINE_BIND_POINT_GRAPHICS,
                                   [],
                                   [fRenderPass.AddAttachmentReference(ColorAttachmentIndex,VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL)],
                                   [],
                                   -1,
                                   []);
 // The acquire semaphore is waited on at the color attachment output stage, so the layout transition at
 // the start of the render pass has to wait there as well.
 fRenderPass.AddSubpassDependency(VK_SUBPASS_EXTERNAL,
                                  0,
                                  TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                  TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                  0,
                                  TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
                                  0);
 fRenderPass.AddSubpassDependency(0,
                                  VK_SUBPASS_EXTERNAL,
                                  TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                  TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                  TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
                                  0,
                                  0);
 fRenderPass.Initialize;
 fDevice.DebugUtils.SetObjectName(fRenderPass.Handle,VK_OBJECT_TYPE_RENDER_PASS,fName+'.RenderPass');

 SetLength(fSwapChainImageViews,fSwapChain.CountImages);
 SetLength(fFrameBuffers,fSwapChain.CountImages);
 for ImageIndex:=0 to fSwapChain.CountImages-1 do begin
  fSwapChainImageViews[ImageIndex]:=TpvVulkanImageView.Create(fDevice,
                                                              fSwapChain.Images[ImageIndex],
                                                              VK_IMAGE_VIEW_TYPE_2D,
                                                              fSwapChain.ImageFormat,
                                                              VK_COMPONENT_SWIZZLE_IDENTITY,
                                                              VK_COMPONENT_SWIZZLE_IDENTITY,
                                                              VK_COMPONENT_SWIZZLE_IDENTITY,
                                                              VK_COMPONENT_SWIZZLE_IDENTITY,
                                                              TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                              0,
                                                              1,
                                                              0,
                                                              1);
  fFrameBuffers[ImageIndex]:=TpvVulkanFrameBuffer.Create(fDevice,
                                                         fRenderPass,
                                                         fSwapChain.Width,
                                                         fSwapChain.Height,
                                                         1,
                                                         [TpvVulkanFrameBufferAttachment.Create(fDevice,
                                                                                                fSwapChain.Images[ImageIndex],
                                                                                                fSwapChainImageViews[ImageIndex],
                                                                                                fSwapChain.Width,
                                                                                                fSwapChain.Height,
                                                                                                fSwapChain.ImageFormat,
                                                                                                false)],
                                                         true,
                                                         fName+'.FrameBuffer['+IntToStr(ImageIndex)+']');
 end;

 fPipeline:=TpvVulkanGraphicsPipeline.Create(fDevice,
                                             fPipelineCache,
                                             0,
                                             [],
                                             fPipelineLayout,
                                             fRenderPass,
                                             0,
                                             nil,
                                             0);
 fPipeline.AddStage(fVertexShaderStage);
 fPipeline.AddStage(fFragmentShaderStage);
 fPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;
 fPipeline.ViewPortState.AddViewPort(0.0,0.0,fSwapChain.Width,fSwapChain.Height,0.0,1.0);
 fPipeline.ViewPortState.AddScissor(0,0,fSwapChain.Width,fSwapChain.Height);
 fPipeline.RasterizationState.DepthClampEnable:=false;
 fPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
 fPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_CLOCKWISE;
 fPipeline.RasterizationState.DepthBiasEnable:=false;
 fPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
 fPipeline.RasterizationState.DepthBiasClamp:=0.0;
 fPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
 fPipeline.RasterizationState.LineWidth:=1.0;
 fPipeline.MultisampleState.RasterizationSamples:=VK_SAMPLE_COUNT_1_BIT;
 fPipeline.MultisampleState.SampleShadingEnable:=false;
 fPipeline.MultisampleState.MinSampleShading:=0.0;
 fPipeline.MultisampleState.CountSampleMasks:=0;
 fPipeline.MultisampleState.AlphaToCoverageEnable:=false;
 fPipeline.MultisampleState.AlphaToOneEnable:=false;
 fPipeline.ColorBlendState.LogicOpEnable:=false;
 fPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
 fPipeline.ColorBlendState.BlendConstants[0]:=0.0;
 fPipeline.ColorBlendState.BlendConstants[1]:=0.0;
 fPipeline.ColorBlendState.BlendConstants[2]:=0.0;
 fPipeline.ColorBlendState.BlendConstants[3]:=0.0;
 fPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
                                                        VK_BLEND_FACTOR_ONE,
                                                        VK_BLEND_FACTOR_ZERO,
                                                        VK_BLEND_OP_ADD,
                                                        VK_BLEND_FACTOR_ONE,
                                                        VK_BLEND_FACTOR_ZERO,
                                                        VK_BLEND_OP_ADD,
                                                        TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                        TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                        TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                        TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));
 fPipeline.DepthStencilState.DepthTestEnable:=false;
 fPipeline.DepthStencilState.DepthWriteEnable:=false;
 fPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fPipeline.DepthStencilState.StencilTestEnable:=false;
 fPipeline.Initialize;
 fPipeline.FreeMemory;
 fDevice.DebugUtils.SetObjectName(fPipeline.Handle,VK_OBJECT_TYPE_PIPELINE,fName+'.Pipeline');

end;

procedure TpvScene3DRendererFrameGenerationCompositor.DestroySwapChainResources;
var ImageIndex:TpvSizeInt;
begin
 FreeAndNil(fPipeline);
 for ImageIndex:=0 to length(fFrameBuffers)-1 do begin
  FreeAndNil(fFrameBuffers[ImageIndex]);
 end;
 for ImageIndex:=0 to length(fSwapChainImageViews)-1 do begin
  FreeAndNil(fSwapChainImageViews[ImageIndex]);
 end;
 fFrameBuffers:=nil;
 fSwapChainImageViews:=nil;
 FreeAndNil(fRenderPass);
 fSwapChain:=nil;
end;

procedure TpvScene3DRendererFrameGenerationCompositor.BeginFrame(const aFrameIndex:TpvInt64;const aReset:boolean;const aFrameTimeDelta:TpvFloat);
var Slot:PRingSlot;
begin
 fFrameCounter:=aFrameIndex;
 fCurrentSlot:=TpvInt32(TpvUInt64(aFrameIndex) mod CountRingSlots);
 if fCurrentSlot=fPreviousSlot then begin
  // The same frame index twice in a row, as after a skipped frame: there is no previous frame to go from.
  fPreviousSlot:=-1;
 end;
 Slot:=@fRing[fCurrentSlot];
 Slot^.Captured:=false;
 Slot^.Reset:=aReset;
 FillChar(Slot^.Frame,SizeOf(TpvScene3DRendererFrameGenerationBackend.TFrame),#0);
 Slot^.Frame.Color.Image:=Slot^.Color.VulkanImage.Handle;
 Slot^.Frame.Color.ImageView:=Slot^.Color.VulkanImageView.Handle;
 Slot^.Frame.Color.Format:=fColorFormat;
 Slot^.Frame.Color.Width:=fWidth;
 Slot^.Frame.Color.Height:=fHeight;
 if fNeedsDepthAndMotionVectors then begin
  Slot^.Frame.Depth.Image:=Slot^.Depth.VulkanImage.Handle;
  Slot^.Frame.Depth.ImageView:=Slot^.Depth.VulkanImageView.Handle;
  Slot^.Frame.Depth.Format:=DepthFormat;
  Slot^.Frame.Depth.Width:=fRenderWidth;
  Slot^.Frame.Depth.Height:=fRenderHeight;
  Slot^.Frame.MotionVectors.Image:=Slot^.MotionVectors.VulkanImage.Handle;
  Slot^.Frame.MotionVectors.ImageView:=Slot^.MotionVectors.VulkanImageView.Handle;
  Slot^.Frame.MotionVectors.Format:=MotionVectorsFormat;
  Slot^.Frame.MotionVectors.Width:=fRenderWidth;
  Slot^.Frame.MotionVectors.Height:=fRenderHeight;
 end;
 Slot^.Frame.ViewMatrix:=TpvMatrix4x4.Identity;
 Slot^.Frame.ProjectionMatrix:=TpvMatrix4x4.Identity;
 Slot^.Frame.PeakBrightness:=1.0;
 Slot^.Frame.FrameTimeDelta:=aFrameTimeDelta;
 Slot^.Frame.FrameIndex:=aFrameIndex;
end;

procedure TpvScene3DRendererFrameGenerationCompositor.SetFrameView(const aViewMatrix:TpvMatrix4x4;
                                                                   const aProjectionMatrix:TpvMatrix4x4;
                                                                   const aJitterOffset:TpvVector2;
                                                                   const aMotionVectorScale:TpvVector2;
                                                                   const aZNear:TpvFloat;
                                                                   const aZFar:TpvFloat;
                                                                   const aPeakBrightness:TpvFloat);
var Slot:PRingSlot;
begin
 Slot:=@fRing[fCurrentSlot];
 Slot^.Frame.ViewMatrix:=aViewMatrix;
 Slot^.Frame.ProjectionMatrix:=aProjectionMatrix;
 Slot^.Frame.JitterOffset:=aJitterOffset;
 Slot^.Frame.MotionVectorScale:=aMotionVectorScale;
 Slot^.Frame.ZNear:=aZNear;
 Slot^.Frame.ZFar:=aZFar;
 Slot^.Frame.PeakBrightness:=aPeakBrightness;
end;

procedure TpvScene3DRendererFrameGenerationCompositor.Capture(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                              const aColorImage:TVkImage;
                                                              const aColorLayer:TpvInt32;
                                                              const aHUDImage:TVkImage;
                                                              const aDepthImage:TVkImage;
                                                              const aDepthLayer:TpvInt32;
                                                              const aMotionVectorsImage:TVkImage;
                                                              const aMotionVectorsLayer:TpvInt32);
var Slot:PRingSlot;
    ImageMemoryBarriers:array[0..4] of TVkImageMemoryBarrier;
    CountImageMemoryBarriers:TpvInt32;
    ImageCopy:TVkImageCopy;
    WithHUD,WithDepthAndMotionVectors:boolean;
 procedure AddImageMemoryBarrier(const aImage:TVkImage;
                                 const aBaseArrayLayer:TpvInt32;
                                 const aSrcAccessMask:TVkAccessFlags;
                                 const aDstAccessMask:TVkAccessFlags;
                                 const aOldLayout:TVkImageLayout;
                                 const aNewLayout:TVkImageLayout);
 begin
  FillChar(ImageMemoryBarriers[CountImageMemoryBarriers],SizeOf(TVkImageMemoryBarrier),#0);
  ImageMemoryBarriers[CountImageMemoryBarriers].sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
  ImageMemoryBarriers[CountImageMemoryBarriers].srcAccessMask:=aSrcAccessMask;
  ImageMemoryBarriers[CountImageMemoryBarriers].dstAccessMask:=aDstAccessMask;
  ImageMemoryBarriers[CountImageMemoryBarriers].oldLayout:=aOldLayout;
  ImageMemoryBarriers[CountImageMemoryBarriers].newLayout:=aNewLayout;
  ImageMemoryBarriers[CountImageMemoryBarriers].srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  ImageMemoryBarriers[CountImageMemoryBarriers].dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  ImageMemoryBarriers[CountImageMemoryBarriers].image:=aImage;
  ImageMemoryBarriers[CountImageMemoryBarriers].subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
  ImageMemoryBarriers[CountImageMemoryBarriers].subresourceRange.baseMipLevel:=0;
  ImageMemoryBarriers[CountImageMemoryBarriers].subresourceRange.levelCount:=1;
  ImageMemoryBarriers[CountImageMemoryBarriers].subresourceRange.baseArrayLayer:=aBaseArrayLayer;
  ImageMemoryBarriers[CountImageMemoryBarriers].subresourceRange.layerCount:=1;
  inc(CountImageMemoryBarriers);
 end;
begin

 Slot:=@fRing[fCurrentSlot];

 WithHUD:=fHasHUD and (aHUDImage<>VK_NULL_HANDLE) and assigned(Slot^.HUD);

 WithDepthAndMotionVectors:=fNeedsDepthAndMotionVectors and
                            (aDepthImage<>VK_NULL_HANDLE) and
                            (aMotionVectorsImage<>VK_NULL_HANDLE) and
                            assigned(Slot^.Depth) and
                            assigned(Slot^.MotionVectors);

 // The slot was last read by the generated frames of two rendered frames ago, which came earlier in
 // submission order on the same queue, so waiting for every stage before is enough; what it holds is
 // overwritten anyway. The depth is not a resource of the frame graph and stays ready to be sampled
 // between the passes, so it goes into TRANSFER_SRC_OPTIMAL here and back afterwards, after whatever wrote
 // it or still reads it.
 CountImageMemoryBarriers:=0;
 AddImageMemoryBarrier(Slot^.Color.VulkanImage.Handle,
                       0,
                       0,
                       TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                       VK_IMAGE_LAYOUT_UNDEFINED,
                       VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);
 if WithHUD then begin
  AddImageMemoryBarrier(Slot^.HUD.VulkanImage.Handle,
                        0,
                        0,
                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                        VK_IMAGE_LAYOUT_UNDEFINED,
                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);
 end;
 if WithDepthAndMotionVectors then begin
  AddImageMemoryBarrier(Slot^.Depth.VulkanImage.Handle,
                        0,
                        0,
                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                        VK_IMAGE_LAYOUT_UNDEFINED,
                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);
  AddImageMemoryBarrier(Slot^.MotionVectors.VulkanImage.Handle,
                        0,
                        0,
                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                        VK_IMAGE_LAYOUT_UNDEFINED,
                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);
  AddImageMemoryBarrier(aDepthImage,
                        Max(0,aDepthLayer),
                        TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                        TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL);
 end;
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   CountImageMemoryBarriers,@ImageMemoryBarriers[0]);

 FillChar(ImageCopy,SizeOf(TVkImageCopy),#0);
 ImageCopy.srcSubresource.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 ImageCopy.srcSubresource.mipLevel:=0;
 ImageCopy.srcSubresource.baseArrayLayer:=Max(0,aColorLayer);
 ImageCopy.srcSubresource.layerCount:=1;
 ImageCopy.dstSubresource.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 ImageCopy.dstSubresource.mipLevel:=0;
 ImageCopy.dstSubresource.baseArrayLayer:=0;
 ImageCopy.dstSubresource.layerCount:=1;
 ImageCopy.extent.width:=fWidth;
 ImageCopy.extent.height:=fHeight;
 ImageCopy.extent.depth:=1;
 aCommandBuffer.CmdCopyImage(aColorImage,
                             VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                             Slot^.Color.VulkanImage.Handle,
                             VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                             1,
                             @ImageCopy);

 if WithHUD then begin
  ImageCopy.srcSubresource.baseArrayLayer:=0;
  ImageCopy.extent.width:=fHUDWidth;
  ImageCopy.extent.height:=fHUDHeight;
  aCommandBuffer.CmdCopyImage(aHUDImage,
                              VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                              Slot^.HUD.VulkanImage.Handle,
                              VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                              1,
                              @ImageCopy);
 end;

 if WithDepthAndMotionVectors then begin
  ImageCopy.srcSubresource.baseArrayLayer:=Max(0,aDepthLayer);
  ImageCopy.extent.width:=fRenderWidth;
  ImageCopy.extent.height:=fRenderHeight;
  aCommandBuffer.CmdCopyImage(aDepthImage,
                              VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                              Slot^.Depth.VulkanImage.Handle,
                              VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                              1,
                              @ImageCopy);
  ImageCopy.srcSubresource.baseArrayLayer:=Max(0,aMotionVectorsLayer);
  aCommandBuffer.CmdCopyImage(aMotionVectorsImage,
                              VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                              Slot^.MotionVectors.VulkanImage.Handle,
                              VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                              1,
                              @ImageCopy);
 end;

 // Everything into the ring is read by shaders from now on; the depth goes back to where it was, for the
 // passes after this one.
 CountImageMemoryBarriers:=0;
 AddImageMemoryBarrier(Slot^.Color.VulkanImage.Handle,
                       0,
                       TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                       TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                       VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                       VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
 if WithHUD then begin
  AddImageMemoryBarrier(Slot^.HUD.VulkanImage.Handle,
                        0,
                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
 end;
 if WithDepthAndMotionVectors then begin
  AddImageMemoryBarrier(Slot^.Depth.VulkanImage.Handle,
                        0,
                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
  AddImageMemoryBarrier(Slot^.MotionVectors.VulkanImage.Handle,
                        0,
                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
 end;
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   CountImageMemoryBarriers,@ImageMemoryBarriers[0]);

 if WithDepthAndMotionVectors then begin
  CountImageMemoryBarriers:=0;
  AddImageMemoryBarrier(aDepthImage,
                        Max(0,aDepthLayer),
                        TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    CountImageMemoryBarriers,@ImageMemoryBarriers[0]);
 end;

 Slot^.Captured:=true;

end;

procedure TpvScene3DRendererFrameGenerationCompositor.DrawGeneratedFrames(const aInFlightFrameIndex:TpvSizeInt;var aWaitSemaphore:TpvVulkanSemaphore);
var CurrentSlot,PreviousSlot:PRingSlot;
    CountFrames,CountAcquired,Index:TpvInt32;
    ImageIndices:array[0..MaxGeneratedFramesPerFrame-1] of TpvInt32;
    AcquireSemaphores,RenderDoneSemaphores:array[0..MaxGeneratedFramesPerFrame-1] of TpvVulkanSemaphore;
    WaitSemaphores:array[0..MaxGeneratedFramesPerFrame] of TVkSemaphore;
    WaitDstStageMasks:array[0..MaxGeneratedFramesPerFrame] of TVkPipelineStageFlags;
    SignalSemaphores:array[0..MaxGeneratedFramesPerFrame] of TVkSemaphore;
    CountWaitSemaphores,CountSignalSemaphores:TpvUInt32;
    CommandBuffer:TpvVulkanCommandBuffer;
    Description:TpvScene3DRendererFrameGenerationBackend.TGenerateDescription;
    ImageMemoryBarrier:TVkImageMemoryBarrier;
    DescriptorSet:TpvVulkanDescriptorSet;
    PushConstants:TPushConstants;
    SubmitInfo:TVkSubmitInfo;
begin

 CurrentSlot:=@fRing[fCurrentSlot];

 // The minimum frame rate with a tenth of hysteresis, so that a GPU time right at the limit does not turn
 // frame generation on and off from frame to frame.
 if (fMinimumFrameRate>0.0) and (fGPUFrameTime>0.0) then begin
  if fBelowMinimumFrameRate then begin
   fBelowMinimumFrameRate:=(1.0/fGPUFrameTime)<(fMinimumFrameRate*1.1);
  end else begin
   fBelowMinimumFrameRate:=(1.0/fGPUFrameTime)<fMinimumFrameRate;
  end;
 end else begin
  fBelowMinimumFrameRate:=false;
 end;

 try

  if fSuspended or
     fBelowMinimumFrameRate or
     (not CurrentSlot^.Captured) or
     CurrentSlot^.Reset or
     (fPreviousSlot<0) or
     (not fRing[fPreviousSlot].Captured) or
     (not assigned(pvApplication)) or
     (not assigned(pvApplication.VulkanSwapChain)) then begin
   exit;
  end;

  PreviousSlot:=@fRing[fPreviousSlot];

  if fSwapChain<>pvApplication.VulkanSwapChain then begin
   DestroySwapChainResources;
   CreateSwapChainResources;
  end;

  // A swapchain of another size than the frames, as while a resize is under way, gets no generated frames.
  if (fSwapChain.Width<>fWidth) or (fSwapChain.Height<>fHeight) then begin
   exit;
  end;

  if fFencesReady[aInFlightFrameIndex] then begin
   fFences[aInFlightFrameIndex].WaitFor;
   fFences[aInFlightFrameIndex].Reset;
   fFencesReady[aInFlightFrameIndex]:=false;
   // The submit of this in-flight frame before has finished, so its time can be read.
   if assigned(fTimerQueries[aInFlightFrameIndex]) and fTimerQueries[aInFlightFrameIndex].Update then begin
    fGenerationGPUTime:=fTimerQueries[aInFlightFrameIndex].Results[0].Duration;
   end;
  end;

  CountFrames:=Min(Min(fCountGeneratedFrames,fBackend.GetMaximumCountGeneratedFrames(fDevice)),MaxGeneratedFramesPerFrame);

  CountAcquired:=0;
  while CountAcquired<CountFrames do begin
   if pvApplication.AcquireGeneratedFrameImage(ImageIndices[CountAcquired],AcquireSemaphores[CountAcquired],RenderDoneSemaphores[CountAcquired]) then begin
    inc(CountAcquired);
   end else begin
    break;
   end;
  end;

  if CountAcquired=0 then begin
   exit;
  end;

  CommandBuffer:=fCommandBuffers[aInFlightFrameIndex];
  CommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));
  CommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));

  if assigned(fTimerQueries[aInFlightFrameIndex]) then begin
   fTimerQueries[aInFlightFrameIndex].Reset;
   fTimerQueries[aInFlightFrameIndex].Start(fDevice.UniversalQueue,CommandBuffer,'FrameGeneration');
  end;

  fDevice.DebugUtils.CmdBufLabelBegin(CommandBuffer,'FrameGeneration',[0.8,0.2,0.8,1.0]);

  FillChar(Description,SizeOf(TpvScene3DRendererFrameGenerationBackend.TGenerateDescription),#0);
  Description.Previous:=PreviousSlot^.Frame;
  Description.Current:=CurrentSlot^.Frame;
  Description.Output.Image:=fOutput.VulkanImage.Handle;
  Description.Output.ImageView:=fOutput.VulkanImageView.Handle;
  Description.Output.Format:=fColorFormat;
  Description.Output.Width:=fWidth;
  Description.Output.Height:=fHeight;
  Description.Count:=CountAcquired;
  Description.Reset:=false;
  Description.DebugView:=fDebugView;

  FillChar(ImageMemoryBarrier,SizeOf(TVkImageMemoryBarrier),#0);
  ImageMemoryBarrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
  ImageMemoryBarrier.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  ImageMemoryBarrier.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  ImageMemoryBarrier.image:=fOutput.VulkanImage.Handle;
  ImageMemoryBarrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
  ImageMemoryBarrier.subresourceRange.baseMipLevel:=0;
  ImageMemoryBarrier.subresourceRange.levelCount:=1;
  ImageMemoryBarrier.subresourceRange.baseArrayLayer:=0;
  ImageMemoryBarrier.subresourceRange.layerCount:=1;

  for Index:=1 to CountAcquired do begin

   // The output was read by the composition of the generated frame before, or of the interval before.
   ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT);
   ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
   ImageMemoryBarrier.oldLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
   ImageMemoryBarrier.newLayout:=VK_IMAGE_LAYOUT_GENERAL;
   CommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    1,@ImageMemoryBarrier);

   Description.Index:=Index;
   fBackend.Generate(CommandBuffer,aInFlightFrameIndex,Description);

   ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
   ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT);
   ImageMemoryBarrier.oldLayout:=VK_IMAGE_LAYOUT_GENERAL;
   ImageMemoryBarrier.newLayout:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
   CommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    1,@ImageMemoryBarrier);

   DescriptorSet:=fDescriptorSets[aInFlightFrameIndex,Index-1];
   DescriptorSet.WriteToDescriptorSet(0,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                                      [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                     fOutput.VulkanImageView.Handle,
                                                                     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                      [],
                                      [],
                                      false);
   if fHasHUD and assigned(CurrentSlot^.HUD) then begin
    DescriptorSet.WriteToDescriptorSet(1,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                       [TVkDescriptorImageInfo.Create(fSampler.Handle,
                                                                      CurrentSlot^.HUD.VulkanImageView.Handle,
                                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                       [],
                                       [],
                                       false);
   end else begin
    // Without a HUD, the binding still has to point at something valid; the shader does not read it.
    DescriptorSet.WriteToDescriptorSet(1,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                       [TVkDescriptorImageInfo.Create(fSampler.Handle,
                                                                      CurrentSlot^.Color.VulkanImageView.Handle,
                                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                       [],
                                       [],
                                       false);
   end;
   DescriptorSet.Flush;

   fRenderPass.BeginRenderPass(CommandBuffer,
                               fFrameBuffers[ImageIndices[Index-1]],
                               VK_SUBPASS_CONTENTS_INLINE,
                               0,
                               0,
                               fSwapChain.Width,
                               fSwapChain.Height);

   CommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fPipeline.Handle);
   CommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                       fPipelineLayout.Handle,
                                       0,
                                       1,
                                       @DescriptorSet.Handle,
                                       0,
                                       nil);

   PushConstants.ColorSpace:=fColorSpace;
   PushConstants.FrameCounter:=TpvUInt32((fFrameCounter*(MaxGeneratedFramesPerFrame+1))+Index);
   PushConstants.Flags:=0;
   if fHasHUD and assigned(CurrentSlot^.HUD) then begin
    PushConstants.Flags:=PushConstants.Flags or FlagHUD;
   end;
   if fDebugView then begin
    PushConstants.Flags:=PushConstants.Flags or FlagDebug;
   end;
   PushConstants.Index:=Index;
   PushConstants.Count:=CountAcquired;
   CommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                  0,
                                  SizeOf(TPushConstants),
                                  @PushConstants);

   CommandBuffer.CmdDraw(3,1,0,0);

   fRenderPass.EndRenderPass(CommandBuffer);

  end;

  fDevice.DebugUtils.CmdBufLabelEnd(CommandBuffer);

  if assigned(fTimerQueries[aInFlightFrameIndex]) then begin
   fTimerQueries[aInFlightFrameIndex].Stop(fDevice.UniversalQueue,CommandBuffer);
  end;

  CommandBuffer.EndRecording;

  // Waits on the rendered frame, which wrote the ring slot, and on every acquired image; signals every
  // generated frame for its present and the rendered frame for its own.
  CountWaitSemaphores:=0;
  if assigned(aWaitSemaphore) then begin
   WaitSemaphores[CountWaitSemaphores]:=aWaitSemaphore.Handle;
   WaitDstStageMasks[CountWaitSemaphores]:=TVkPipelineStageFlags(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT);
   inc(CountWaitSemaphores);
  end;
  CountSignalSemaphores:=0;
  for Index:=0 to CountAcquired-1 do begin
   WaitSemaphores[CountWaitSemaphores]:=AcquireSemaphores[Index].Handle;
   WaitDstStageMasks[CountWaitSemaphores]:=TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT);
   inc(CountWaitSemaphores);
   SignalSemaphores[CountSignalSemaphores]:=RenderDoneSemaphores[Index].Handle;
   inc(CountSignalSemaphores);
  end;
  SignalSemaphores[CountSignalSemaphores]:=fPresentSemaphores[aInFlightFrameIndex].Handle;
  inc(CountSignalSemaphores);

  FillChar(SubmitInfo,SizeOf(TVkSubmitInfo),#0);
  SubmitInfo.sType:=VK_STRUCTURE_TYPE_SUBMIT_INFO;
  // With NVIDIA Reflex, the submit belongs to the rendered frame, as its own submits do.
  SubmitInfo.pNext:=pvApplication.LowLatencySubmissionPresentIDNext(aInFlightFrameIndex,fFrameCounter);
  SubmitInfo.waitSemaphoreCount:=CountWaitSemaphores;
  SubmitInfo.pWaitSemaphores:=@WaitSemaphores[0];
  SubmitInfo.pWaitDstStageMask:=@WaitDstStageMasks[0];
  SubmitInfo.commandBufferCount:=1;
  SubmitInfo.pCommandBuffers:=@CommandBuffer.Handle;
  SubmitInfo.signalSemaphoreCount:=CountSignalSemaphores;
  SubmitInfo.pSignalSemaphores:=@SignalSemaphores[0];
  fDevice.UniversalQueue.Submit(1,@SubmitInfo,fFences[aInFlightFrameIndex]);
  fFencesReady[aInFlightFrameIndex]:=true;

  aWaitSemaphore:=fPresentSemaphores[aInFlightFrameIndex];

 finally
  // The current frame is the previous one of the next interval, if it was captured at all.
  if CurrentSlot^.Captured then begin
   fPreviousSlot:=fCurrentSlot;
  end else begin
   fPreviousSlot:=-1;
  end;
 end;

end;

end.
