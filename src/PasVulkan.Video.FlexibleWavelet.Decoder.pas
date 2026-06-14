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
unit PasVulkan.Video.FlexibleWavelet.Decoder;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$rangechecks off}
{$overflowchecks off}

// GPU decoder for the Flexible Wavelet Video (FWV) codec, the resource-system-independent engine-side reader
// of an FWVC container. Vulkan-compute on top of the shared CPU core in PasVulkan.Video.FlexibleWavelet.
//
// Stage B (this step) is the GPU resource SETUP only: Create parses the container header + frame index, then
// builds the intra-decode compute pipelines (from the embedded FlexibleWaveletVideo*SPIRV constants), the
// descriptor set layouts / pool / sets, the working buffers and the output image — no decoding yet. The
// per-frame decode dispatch, the poll API (DecodeTime/Decode/Seek) and the inter / 3D-DWT / B-frame
// resources are added in the following stages.
//
// The caller owns the TStream and the TpvVulkanDevice and must keep them alive at least as long as the
// decoder; the decoder never frees them.

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Assets,
     PasVulkan.Video.FlexibleWavelet;

type EpvFlexibleWaveletVideoDecoder=class(EpvFlexibleWaveletVideo);

     { TpvFlexibleWaveletVideoDecoder }
     TpvFlexibleWaveletVideoDecoder=class(TpvFlexibleWaveletVideo)
      public
       type TBidiPlan=record // mode-B decode-ahead plan captured by PrepareFrameBidi, replayed by RecordFrameBidi
             RingSlot:TpvInt32;
             IsPredicted:TpvInt32;
             Ref1Slot:TpvInt32;
             Weight0:TpvInt32;
             Weight1:TpvInt32;
            end;
            TBidiPlans=array of TBidiPlan;
      private
       fStream:TStream;
       fDevice:TpvVulkanDevice;
       fHeader:THeader;
       fFrameEntries:TFrameEntries;
       fFrameCount:TpvInt64;
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fLevels:TpvInt32;
       fQuality:TpvInt32;
       fBlockSize:TpvInt32;
       fChromaFormat:TpvInt32;
       fPredictionMethod:TpvInt32;
       fGOP:TpvInt32;
       fMotionBlock:TpvInt32;
       fMVCodec:TpvInt32;
       fMotionVariable:boolean;
       fChromaQuant:TpvFloat;
       fSampleWhite:TpvInt32;
       fIsHDR:boolean;
       fTransferFunction:TpvInt32;
       fHDRExposure:TpvFloat;
       fLossless:boolean;
       fPreferSCRGB:boolean;
       fGainsComputed:boolean;
       // --- hierarchical B-frames (Stage E3) ---
       fHasBFrames:boolean;
       fHasPerBlockMode:boolean;
       fSubmitMode:TpvInt32; // 0 = mode A (decoder self-submits the decode-ahead), 1 = mode B (caller command buffer)
       fGDecodePeriod:TpvInt32;
       fGDecodeLead:TpvInt32;
       fGDPBSlots:TpvInt32;
       fGCursor:TpvInt32; // next coding-order frame to decode-ahead
       fGDPBLastUse:array of TpvInt32; // per coding frame: last coding index that references it (eviction trigger)
       fGDPBPOCToSlot:array of TpvInt32; // display POC -> DPB slot
       fGDPBCodingToSlot:array of TpvInt32; // coding index -> DPB slot
       fGDPBSlotCoding:array of TpvInt32; // DPB slot -> coding index occupying it (-1 = free)
       fGDecStepIndex:TpvInt32; // currently-bound dequant step quality (Stage E3 rebuilds per frame)
       // --- 3D-DWT / MCTF temporal mode (Stage E4) ---
       fMode3DDWT:boolean;
       fMCTF:boolean;
       fTemporalLevels:TpvInt32;
       fTemporalWavelet:TpvInt32;
       fGOPCapacity:TpvInt32;
       fCur3DGopStart:TpvInt32; // coding/display index of the currently-decoded GOP (-1 = none)
       fGopBuffer:array[0..2] of TpvVulkanBuffer; // the whole reconstructed GOP (subbands -> frames), device-local
       fPipeTDWTInt:TpvVulkanComputePipeline;
       fPipeTDWTFloat:TpvVulkanComputePipeline;
       fPLTemporal:TpvVulkanPipelineLayout; // DSL1 + 20-byte push
       fSetTemporal:array[0..2] of TpvVulkanDescriptorSet; // {gop_buffer[plane]}
       fMCTFPred:array[0..2] of TpvVulkanBuffer; // MCTF: the per-pair MC-warped low frame, device-local
       fMCTFScratch:array[0..2] of TpvVulkanBuffer; // MCTF: the per-level interleaved frame workspace, device-local
       fMCTFMVScratch:array of TpvInt32; // MCTF: every GOP frame's luma MV field (CPU side), by deinterleaved slot
       fSetMCTFMC:array[0..2] of TpvVulkanDescriptorSet; // MCTF (rebound per pair, with byte offsets): {gop@low, mv, pred}
       fSetMCTFAdd:array[0..2] of TpvVulkanDescriptorSet; // MCTF: {scratch@odd, pred}
       // mode B per-frame input ring: -1 = the shared buffers/sets (mode A self-submit, mode C caller step-loop),
       // >=0 = the active ring slot (mode B records the whole decode-ahead into ONE caller command buffer).
       fBufferRingSlot:TpvInt32;
       fBufferRingSize:TpvInt32;
       fRingDataBuffer:array of TpvVulkanBuffer;
       fRingOffsetBuffer:array of array[0..2] of TpvVulkanBuffer;
       fRingStepBuffer:array of array[0..2] of TpvVulkanBuffer;
       fRingMVBuffer:array of TpvVulkanBuffer;
       fRingMV1Buffer:array of TpvVulkanBuffer;
       fRingModeBuffer:array of TpvVulkanBuffer;
       fRingSetUnpack:array of array[0..2] of TpvVulkanDescriptorSet; // bound once: {ring data, ring offset, shared coeff}
       fRingSetDequant:array of array[0..2] of TpvVulkanDescriptorSet; // bound once: {shared coeff, ring step}
       fRingSetGMC0:array of array[0..2] of TpvVulkanDescriptorSet; // rebound per frame
       fRingSetGMC1:array of array[0..2] of TpvVulkanDescriptorSet;
       fRingSetGBlend:array of array[0..2] of TpvVulkanDescriptorSet;
       fRingSetGBlendMode:array of array[0..2] of TpvVulkanDescriptorSet;
       fRingSetGAdd:array of array[0..2] of TpvVulkanDescriptorSet;
       // two-phase decode (the poll-API split): PrepareFrame does the CPU side, RecordFrame the GPU side.
       fPreparedIndex:TpvInt32; // display index the last PrepareFrame staged (-1 = none)
       fPreparedIsPredicted:boolean; // intra/P: is_predicted of fPreparedIndex
       fBidiPlan:TBidiPlans; // mode-B decode-ahead plan captured by PrepareFrameBidi, replayed by RecordFrameBidi
       fBidiPlanCount:TpvInt32;
       fBidiDisplayPOC:TpvInt32;
       fHFGain:TSynthesisGains;
       fLLGain:TpvFloat;
       fUseSCRGB:boolean;
       fOutputFormat:TVkFormat;
       fPipelineCache:TpvVulkanPipelineCache;
       fDSL1:TpvVulkanDescriptorSetLayout; // 1 storage buffer
       fDSL2:TpvVulkanDescriptorSetLayout; // 2 storage buffers
       fDSL3:TpvVulkanDescriptorSetLayout; // 3 storage buffers
       fDSLColour:TpvVulkanDescriptorSetLayout; // 3 storage buffers + 1 storage image
       fPLUnpack:TpvVulkanPipelineLayout;
       fPLDequant:TpvVulkanPipelineLayout;
       fPLTranspose:TpvVulkanPipelineLayout;
       fPLRow:TpvVulkanPipelineLayout;
       fPLRound:TpvVulkanPipelineLayout;
       fPLCoeffAdd:TpvVulkanPipelineLayout;
       fPLColour:TpvVulkanPipelineLayout;
       fPLColourHDR:TpvVulkanPipelineLayout;
       fPipeUnpack:TpvVulkanComputePipeline;
       fPipeDequant:TpvVulkanComputePipeline;
       fPipeTranspose:TpvVulkanComputePipeline;
       fPipeIDWT97:TpvVulkanComputePipeline;
       fPipeIDWT53:TpvVulkanComputePipeline;
       fPipeRound:TpvVulkanComputePipeline;
       fPipeCoeffAdd:TpvVulkanComputePipeline;
       fPipeMC:TpvVulkanComputePipeline;
       fPipeMotionAdd:TpvVulkanComputePipeline;
       fPipeBidiBlend:TpvVulkanComputePipeline; // B-frames: weighted L0/L1 blend
       fPipeBlendMode:TpvVulkanComputePipeline; // B-frames: per-block L0/L1/BI mode blend
       fPLBlendMode:TpvVulkanPipelineLayout; // DSL3 + 20-byte push
       fPipeColour:TpvVulkanComputePipeline;
       fPipeColourHDR:TpvVulkanComputePipeline;
       fPipeColourHDRSCRGB:TpvVulkanComputePipeline;
       fDescriptorPool:TpvVulkanDescriptorPool;
       fDataBuffer:TpvVulkanBuffer;
       fOffsetBuffer:array[0..2] of TpvVulkanBuffer;
       fStepBuffer:array[0..2] of TpvVulkanBuffer;
       fCoeffBuffer:array[0..2] of TpvVulkanBuffer;
       fPreviousBuffer:array[0..2] of TpvVulkanBuffer; // P-frame reference (coefficients / reconstructed YCoCg), GPU-resident across frames
       fMVBuffer:TpvVulkanBuffer; // colordiff (B): per-block [mv_x, mv_y] (half-pel), host-visible
       fMV1Buffer:TpvVulkanBuffer; // B-frames: the L1 motion-vector field, host-visible
       fModeBuffer:TpvVulkanBuffer; // B-frames: per-block L0/L1/BI mode, host-visible
       fDPBBuffer:array of array[0..2] of TpvVulkanBuffer; // B-frame decoded-picture-buffer slots (YCoCg), device-local
       fGMCBuffer:array[0..1] of array[0..2] of TpvVulkanBuffer; // B-frames: the L0/L1 motion-compensated references, device-local
       fScratchBuffer:TpvVulkanBuffer;
       fOutputImage:TpvVulkanImage;
       fOutputImageMemory:TpvVulkanDeviceMemoryBlock;
       fOutputImageView:TpvVulkanImageView;        // sample / present view (sRGB for SDR -> samples to linear; FP16 for HDR)
       fOutputImageStorageView:TpvVulkanImageView; // compute storage view (UNORM for SDR -> stores raw gamma bytes; FP16 for HDR)
       fOutputStorageFormat:TVkFormat;
       fOutputImageFlags:TVkImageCreateFlags;
       fSetUnpack:array[0..2] of TpvVulkanDescriptorSet;
       fSetDequant:array[0..2] of TpvVulkanDescriptorSet;
       fSetAdd:array[0..2] of TpvVulkanDescriptorSet; // coefdiff (A): {coeff, previous}
       fSetMCPlay:array[0..2] of TpvVulkanDescriptorSet; // colordiff (B): {previous, mv, scratch=mc_prev}
       fSetMotionAddPlay:array[0..2] of TpvVulkanDescriptorSet; // colordiff (B): {coeff, scratch=mc_prev, previous}
       fSetGMC0:array[0..2] of TpvVulkanDescriptorSet; // B-frames (rewritten per frame): {dpb[ref0], mv, gmc0}
       fSetGMC1:array[0..2] of TpvVulkanDescriptorSet; // B-frames: {dpb[ref1], mv1, gmc1}
       fSetGBlend:array[0..2] of TpvVulkanDescriptorSet; // B-frames: {gmc0, gmc1, scratch}
       fSetGBlendMode:array[0..2] of TpvVulkanDescriptorSet; // B-frames: {gmc0, gmc1, mode}
       fSetGAdd:array[0..2] of TpvVulkanDescriptorSet; // B-frames: {coeff, prediction, dpb[dst]}
       fSetCoeffToScratch:array[0..2] of TpvVulkanDescriptorSet;
       fSetScratchToCoeff:array[0..2] of TpvVulkanDescriptorSet;
       fSetRow:array[0..2] of TpvVulkanDescriptorSet;
       fSetRowScratch:TpvVulkanDescriptorSet;
       fSetColour:TpvVulkanDescriptorSet;
       fFrameScratch:array of TpvUInt8; // decompressed frame payload, grown on demand
       fCompressedScratch:array of TpvUInt8; // raw container bytes of the current frame
       fOffsetScratch:array[0..2] of array of TpvUInt32; // per-plane block offset prefix sums (CPU side)
       fStepScratch:array of TpvInt32; // per-plane quantization step map (CPU side), grown on demand
       // Quantization-step cache: the step map depends only on (quality, levels, gains, sample-white), NOT on frame
       // content, so it is built ONCE per distinct quality and reused (the C fwvplay does the same via step_cache).
       // Without this the per-pixel rebuild every frame is the CPU bottleneck at 1080p.
       fStepCacheQuality:array of TpvInt32;       // quality value held by each cache slot
       fStepCacheData:array of array of TpvInt32; // [(slot*3)+plane] -> the prebuilt step map for that quality/plane
       fMVScratch:array of TpvInt32; // decoded motion vectors (CPU side) before upload to fMVBuffer
       fMV1Scratch:array of TpvInt32; // B-frames: decoded L1 motion vectors (CPU side)
       fModeScratch:array of TpvInt32; // B-frames: decoded per-block modes (CPU side)
       fBDecodeCommandPool:TpvVulkanCommandPool; // mode A: the decode-ahead self-submit objects
       fBDecodeCommandBuffer:TpvVulkanCommandBuffer;
       fBDecodeFence:TpvVulkanFence;
       function PlaneWidth(const aPlane:TpvInt32):TpvInt32;
       function PlaneHeight(const aPlane:TpvInt32):TpvInt32;
       function BlockCountX(const aWidth:TpvInt32):TpvInt32;
       function BlockCountY(const aHeight:TpvInt32):TpvInt32;
       function MotionBlocksX(const aWidth:TpvInt32):TpvInt32;
       function MotionBlocksY(const aHeight:TpvInt32):TpvInt32;
       procedure ParseContainer;
       function CreateDescriptorSetLayout(const aBufferCount:TpvInt32;const aHasImage:boolean):TpvVulkanDescriptorSetLayout;
       function CreatePipelineLayout(const aSetLayout:TpvVulkanDescriptorSetLayout;const aPushSize:TpvUInt32):TpvVulkanPipelineLayout;
       function CreateComputePipeline(const aData;const aDataSize:TVkSize;const aLayout:TpvVulkanPipelineLayout;const aBlockSizeSpec:boolean;const aMotionBlockSpec:boolean=false):TpvVulkanComputePipeline;
       function CreateStorageBuffer(const aSize:TVkDeviceSize;const aDeviceLocal:boolean;const aName:TpvUTF8String):TpvVulkanBuffer;
       function AllocateSet(const aLayout:TpvVulkanDescriptorSetLayout):TpvVulkanDescriptorSet;
       procedure BindStorageBuffer(const aSet:TpvVulkanDescriptorSet;const aBinding:TpvUInt32;const aBuffer:TpvVulkanBuffer);
       procedure BindStorageBufferOffset(const aSet:TpvVulkanDescriptorSet;const aBinding:TpvUInt32;const aBuffer:TpvVulkanBuffer;const aByteOffset,aRange:TVkDeviceSize);
       procedure BindStorageImage(const aSet:TpvVulkanDescriptorSet;const aBinding:TpvUInt32);
       procedure BuildPipelines;
       procedure BuildBuffersAndImage;
       procedure BuildDescriptorSets;
       function ChromaShiftX:TpvInt32;
       function ChromaShiftY:TpvInt32;
       procedure RecordComputeBarrier(const aCommandBuffer:TpvVulkanCommandBuffer);
       procedure RecordImageBarrier(const aCommandBuffer:TpvVulkanCommandBuffer;const aOldLayout,aNewLayout:TVkImageLayout;const aSrcAccess,aDstAccess:TVkAccessFlags;const aSrcStage,aDstStage:TVkPipelineStageFlags);
       procedure RecordDispatch(const aCommandBuffer:TpvVulkanCommandBuffer;const aPipeline:TpvVulkanComputePipeline;const aLayout:TpvVulkanPipelineLayout;const aSet:TpvVulkanDescriptorSet;const aPushConstants:Pointer;const aPushSize:TpvUInt32;const aGroupsX,aGroupsY,aGroupsZ:TpvUInt32);
       function EnsureStepCacheSlot(const aQuality:TpvInt32):TpvInt32; // build (once) + return the step-map cache slot for a quality
       procedure UploadFrame(const aFrameIndex:TpvInt32);
       procedure RecordDecode(const aCommandBuffer:TpvVulkanCommandBuffer;const aIsPredicted:boolean);
       // hierarchical B-frames (Stage E3). The Active* helpers return the shared buffer/set (fBufferRingSlot<0,
       // modes A/C) or the active ring slot's (fBufferRingSlot>=0, mode B), so UploadBidiFrame / RecordBidiDecode
       // are shared by all submit modes.
       function ActiveDataBuffer:TpvVulkanBuffer;
       function ActiveOffsetBuffer(const aPlane:TpvInt32):TpvVulkanBuffer;
       function ActiveStepBuffer(const aPlane:TpvInt32):TpvVulkanBuffer;
       function ActiveMVBuffer:TpvVulkanBuffer;
       function ActiveMV1Buffer:TpvVulkanBuffer;
       function ActiveModeBuffer:TpvVulkanBuffer;
       function ActiveSetUnpack(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
       function ActiveSetDequant(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
       function ActiveSetGMC0(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
       function ActiveSetGMC1(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
       function ActiveSetGBlend(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
       function ActiveSetGBlendMode(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
       function ActiveSetGAdd(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
       procedure BuildBidiRing; // allocate the per-frame input ring (mode B only)
       procedure UploadBidiFrame(const aCodingIndex:TpvInt32); // CPU: read + parse + MV/mode decode + upload for a coding frame
       function PrepareBidiFrame(const aDisplayPOC:TpvInt32;out aIsPredicted,aRef1Slot,aWeight0,aWeight1:TpvInt32):boolean; // upload fGCursor + DPB slot mgmt + rebind; False = pool full but display ready
       procedure RecordBidiDecode(const aCommandBuffer:TpvVulkanCommandBuffer;const aIsPredicted,aRef1Slot,aWeight0,aWeight1:TpvInt32);
       procedure RecordBidiDisplay(const aCommandBuffer:TpvVulkanCommandBuffer;const aDisplayPOC:TpvInt32);
       procedure DecodeFrameBidi(const aCommandBuffer:TpvVulkanCommandBuffer;const aDisplayPOC:TpvInt32);
       procedure PrepareFrameBidi(const aDisplayPOC:TpvInt32); // CPU half of DecodeFrameBidi (mode B): decode-ahead uploads + plan
       procedure RecordFrameBidi(const aCommandBuffer:TpvVulkanCommandBuffer); // GPU half: replay the plan + display into the caller CB
       // 3D-DWT / MCTF temporal mode (Stage E4)
       function GopCountFrom(const aStart:TpvInt32):TpvInt32;
       procedure Upload3DFrame(const aCodingIndex,aSlot,aGOPCount:TpvInt32); // read + parse + temporally-scaled step upload
       procedure RecordSpatial3D(const aCommandBuffer:TpvVulkanCommandBuffer;const aSlot:TpvInt32); // spatial inverse -> gop[slot]
       procedure RecordTemporal3D(const aCommandBuffer:TpvVulkanCommandBuffer;const aGOPCount:TpvInt32); // open-loop temporal inverse over the GOP
       procedure DecodeMCTFInverse(const aGOPCount:TpvInt32); // MCTF MC-Haar temporal inverse (self-submits per pair)
       procedure RecordDisplay3D(const aCommandBuffer:TpvVulkanCommandBuffer;const aSlot:TpvInt32); // gop[slot] -> coeff -> [round] -> colour
       procedure DecodeFrame3D(const aCommandBuffer:TpvVulkanCommandBuffer;const aDisplayIndex:TpvInt32);
      public
       // aPreferSCRGBForHDR: for HDR streams, output scRGB FP16 (R16G16B16A16) for a real HDR display instead of the
       // SDR-tonemapped sRGB8 fallback. Ignored for SDR streams (always R8G8B8A8). The output format is fixed here.
       // aBSubmitMode: 0 = the decoder self-submits the B-frame decode-ahead (reference/debug; uses its own queue/fence),
       // 1 = engine-friendly (the decode-ahead is recorded into the caller's command buffer; no self-submit).
       constructor Create(const aStream:TStream;const aDevice:TpvVulkanDevice;const aPreferSCRGBForHDR:boolean=false;const aBSubmitMode:TpvInt32=0);
       destructor Destroy; override;
       // Record a decode of aFrameIndex (I or P) into the caller's command buffer; on submit it leaves the
       // reconstructed RGB in OutputImage (VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL). The CPU upload (read /
       // decompress / parse / buffer fill) happens here too, so the caller must submit before decoding another
       // frame. P-frames reference the previous frame's GPU-resident state, so frames must be decoded in coding
       // order. The poll API (DecodeTime / Decode) wrapping this lands in a later stage.
       procedure DecodeFrame(const aCommandBuffer:TpvVulkanCommandBuffer;const aFrameIndex:TpvInt32);
       // two-phase poll-API split: PrepareFrame = CPU-side work (Update thread), RecordFrame = GPU recording into the caller
       // command buffer (Draw thread). For 3D-DWT/MCTF the multi-pass GOP rebuild self-submits, so its CPU work cannot be
       // separated and stays inside RecordFrame (the GOP rebuild runs on the Draw thread when the displayed GOP changes).
       procedure PrepareFrame(const aDisplayIndex:TpvInt32);
       procedure RecordFrame(const aCommandBuffer:TpvVulkanCommandBuffer);
       // Mode C (submit mode 2): record ONE B-frame decode-ahead step (or the final display) into the caller's
       // command buffer; the caller submits + WAITS, then calls again, until this returns False (display recorded).
       function DecodeFrameStep(const aCommandBuffer:TpvVulkanCommandBuffer;const aDisplayPOC:TpvInt32):boolean;
       property Width:TpvInt32 read fWidth;
       property Height:TpvInt32 read fHeight;
       property FrameCount:TpvInt64 read fFrameCount;
       property OutputImage:TpvVulkanImage read fOutputImage;
       property OutputImageView:TpvVulkanImageView read fOutputImageView;
       property OutputFormat:TVkFormat read fOutputFormat; // R8G8B8A8_UNORM (SDR) or R16G16B16A16_SFLOAT (scRGB HDR)
       property IsHDR:boolean read fIsHDR;
      end;

implementation

{ TpvFlexibleWaveletVideoDecoder }

function TpvFlexibleWaveletVideoDecoder.PlaneWidth(const aPlane:TpvInt32):TpvInt32;
var Shift:TpvInt32;
begin
 if (aPlane=0) or (fChromaFormat=0) then begin
  Shift:=0; // chroma_shift_x: 4:2:2 / 4:2:0 halve horizontally, 4:4:4 does not
 end else begin
  Shift:=1;
 end;
 result:=(fWidth+((1 shl Shift)-1)) shr Shift; // ceil(width / 2^shift)
end;

function TpvFlexibleWaveletVideoDecoder.PlaneHeight(const aPlane:TpvInt32):TpvInt32;
var Shift:TpvInt32;
begin
 if (aPlane=0) or (fChromaFormat<>2) then begin
  Shift:=0; // chroma_shift_y: only 4:2:0 halves vertically
 end else begin
  Shift:=1;
 end;
 result:=(fHeight+((1 shl Shift)-1)) shr Shift;
end;

function TpvFlexibleWaveletVideoDecoder.BlockCountX(const aWidth:TpvInt32):TpvInt32;
begin
 result:=(aWidth+(fBlockSize-1)) div fBlockSize;
end;

function TpvFlexibleWaveletVideoDecoder.BlockCountY(const aHeight:TpvInt32):TpvInt32;
begin
 result:=(aHeight+(fBlockSize-1)) div fBlockSize;
end;

function TpvFlexibleWaveletVideoDecoder.MotionBlocksX(const aWidth:TpvInt32):TpvInt32;
begin
 result:=((aWidth+fMotionBlock)-1) div fMotionBlock;
end;

function TpvFlexibleWaveletVideoDecoder.MotionBlocksY(const aHeight:TpvInt32):TpvInt32;
begin
 result:=((aHeight+fMotionBlock)-1) div fMotionBlock;
end;

procedure TpvFlexibleWaveletVideoDecoder.ParseContainer;
var BFrameIndex,SlotIndex:TpvInt32;
begin

 // The 126-byte packed header (ReadBuffer-compatible, little-endian)
 fStream.ReadBuffer(fHeader,SizeOf(fHeader));
 if (CompareByte(fHeader.Magic[0],Magic[0],4)<>0) or (fHeader.Version<>FormatVersion) then begin
  raise EpvFlexibleWaveletVideoDecoder.Create('Not a FWVC stream');
 end;

 // The coding-order frame index
 fFrameCount:=TpvInt64(fHeader.FrameCount);
 SetLength(fFrameEntries,fHeader.FrameCount);
 if fHeader.FrameCount>0 then begin
  fStream.Position:=TpvInt64(fHeader.IndexOffset);
  fStream.ReadBuffer(fFrameEntries[0],Int64(fHeader.FrameCount)*SizeOf(TFrameEntry));
 end;

 // Derive the decode parameters
 fWidth:=TpvInt32(fHeader.Width);
 fHeight:=TpvInt32(fHeader.Height);
 fLevels:=TpvInt32(fHeader.Levels);
 fQuality:=TpvInt32(fHeader.Quality);
 fChromaFormat:=fHeader.ChromaFormat;
 fPredictionMethod:=fHeader.PredictionMethod;
 fGOP:=fHeader.GOP;

 // motion config: mv entropy coder, motion block size + the variable-quadtree flag (reserved2[5]==1)
 fMVCodec:=fHeader.MVCodec;
 fMotionVariable:=false;
 fMotionBlock:=16;
 if (fHeader.Reserved2[5]=8) or (fHeader.Reserved2[5]=16) or (fHeader.Reserved2[5]=32) then begin
  fMotionBlock:=fHeader.Reserved2[5];
 end else if fHeader.Reserved2[5]=1 then begin
  fMotionVariable:=true; // variable quadtree motion (root 32 -> 8); the fine grid is 8
  fMotionBlock:=8;
 end;
 if fHeader.ChromaQuantX16<>0 then begin
  fChromaQuant:=fHeader.ChromaQuantX16/16.0; // chroma quant weighting from the container
 end else begin
  fChromaQuant:=1.0; // 0 (old file) -> off
 end;
 // HDR signalling: colour_flags bit0 = HDR (12-bit BT.2020, PQ/HLG). HDR scales the reference white to 4096
 // (Q x16) and selects the HDR colour shader; the SDR fallback path tonemaps to sRGB8 (exposure default 100).
 fIsHDR:=(fHeader.ColourFlags and 1)<>0;
 fTransferFunction:=fHeader.TransferFunction;
 fHDRExposure:=100.0;
 if fIsHDR then begin
  fSampleWhite:=4096;
 end else begin
  fSampleWhite:=256;
 end;
 fLossless:=fQuality=0;
 fGainsComputed:=false;
 fBlockSize:=fHeader.Reserved2[4];
 if not ((fBlockSize=32) or (fBlockSize=64) or (fBlockSize=128)) then begin
  fBlockSize:=32;
 end;
 // Output format: scRGB FP16 (real HDR display) only when the caller asked for it AND the stream is HDR;
 // otherwise SDR R8G8B8A8 (HDR streams then tonemap to sRGB8 via the SDR-fallback colour shader).
 // The output is "linear-s(c)RGB true truth": HDR = linear scRGB FP16 directly; SDR = an sRGB-format image that the
 // compute fills with gamma bytes via a UNORM storage view, so sampling / blitting decodes to linear (the engine's
 // sRGB swapchain then re-encodes for display, and it is directly usable as an sRGB texture in 3D). The sRGB image
 // needs MUTABLE_FORMAT + EXTENDED_USAGE so STORAGE usage is allowed through the UNORM view.
 fUseSCRGB:=fPreferSCRGB and fIsHDR;
 if fUseSCRGB then begin
  fOutputFormat:=VK_FORMAT_R16G16B16A16_SFLOAT;
  fOutputStorageFormat:=VK_FORMAT_R16G16B16A16_SFLOAT;
  fOutputImageFlags:=0;
 end else begin
  fOutputFormat:=VK_FORMAT_R8G8B8A8_SRGB;
  fOutputStorageFormat:=VK_FORMAT_R8G8B8A8_UNORM;
  fOutputImageFlags:=TVkImageCreateFlags(VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT) or TVkImageCreateFlags(VK_IMAGE_CREATE_EXTENDED_USAGE_BIT);
 end;

 // Hierarchical B-frames: the colordiff stream carries a coding-order B-hierarchy when reserved2[2] > 0; the
 // DPB pool holds the decode-lead + the live references (3*period + spare). Initialise the slot-management
 // arrays (each frame's last-use coding index drives DPB eviction).
 fHasBFrames:=(fPredictionMethod=1) and (fHeader.Reserved2[2]>0);
 fHasPerBlockMode:=fHasBFrames and (fHeader.Reserved2[3]<>0);
 fGCursor:=0;
 fGDecStepIndex:=-1;
 fBufferRingSlot:=-1; // default = the shared input buffers/sets (modes A + C)
 fBufferRingSize:=0;
 if fHasBFrames then begin
  fGDecodePeriod:=fHeader.Reserved2[2]+1;
  fGDecodeLead:=2*fGDecodePeriod;
  fGDPBSlots:=(3*fGDecodePeriod)+6;
  if fGDPBSlots>64 then begin
   fGDPBSlots:=64;
  end;
  fBufferRingSize:=fGDecodeLead+fGDecodePeriod; // mode B: max coding frames recorded into one command buffer
  SetLength(fGDPBLastUse,fFrameCount);
  SetLength(fGDPBPOCToSlot,fFrameCount);
  SetLength(fGDPBCodingToSlot,fFrameCount);
  SetLength(fGDPBSlotCoding,fGDPBSlots);
  for BFrameIndex:=0 to fFrameCount-1 do begin
   fGDPBLastUse[BFrameIndex]:=BFrameIndex;
   fGDPBPOCToSlot[BFrameIndex]:=-1;
   fGDPBCodingToSlot[BFrameIndex]:=-1;
  end;
  for BFrameIndex:=0 to fFrameCount-1 do begin
   if (fFrameEntries[BFrameIndex].Ref0>=0) and (BFrameIndex>fGDPBLastUse[fFrameEntries[BFrameIndex].Ref0]) then begin
    fGDPBLastUse[fFrameEntries[BFrameIndex].Ref0]:=BFrameIndex;
   end;
   if (fFrameEntries[BFrameIndex].Ref1>=0) and (BFrameIndex>fGDPBLastUse[fFrameEntries[BFrameIndex].Ref1]) then begin
    fGDPBLastUse[fFrameEntries[BFrameIndex].Ref1]:=BFrameIndex;
   end;
  end;
  for SlotIndex:=0 to fGDPBSlots-1 do begin
   fGDPBSlotCoding[SlotIndex]:=-1;
  end;
 end else begin
  fGDecodePeriod:=0;
  fGDecodeLead:=0;
  fGDPBSlots:=0;
 end;

 // 3D-DWT temporal mode: prediction_method 2 = open-loop temporal DWT, 3 = motion-compensated (MCTF). A whole
 // GOP is decoded at once (spatial inverse per subband frame -> temporal inverse), then display frames are slot copies.
 fMode3DDWT:=(fPredictionMethod=2) or (fPredictionMethod=3);
 fMCTF:=fPredictionMethod=3;
 fCur3DGopStart:=-1;
 if fMode3DDWT then begin
  if fHeader.Reserved2[0]<>0 then begin
   fTemporalLevels:=fHeader.Reserved2[0];
  end else begin
   fTemporalLevels:=2;
  end;
  fTemporalWavelet:=fHeader.Reserved2[1];
  fGOPCapacity:=fGOP;
  if fGOPCapacity<16 then begin
   fGOPCapacity:=16;
  end else if fGOPCapacity>64 then begin
   fGOPCapacity:=64;
  end;
 end;

end;

function TpvFlexibleWaveletVideoDecoder.CreateDescriptorSetLayout(const aBufferCount:TpvInt32;const aHasImage:boolean):TpvVulkanDescriptorSetLayout;
var Index:TpvInt32;
begin
 result:=TpvVulkanDescriptorSetLayout.Create(fDevice);
 for Index:=0 to aBufferCount-1 do begin
  result.AddBinding(Index,VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);
 end;
 if aHasImage then begin
  result.AddBinding(aBufferCount,VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);
 end;
 result.Initialize;
end;

function TpvFlexibleWaveletVideoDecoder.CreatePipelineLayout(const aSetLayout:TpvVulkanDescriptorSetLayout;const aPushSize:TpvUInt32):TpvVulkanPipelineLayout;
begin
 result:=TpvVulkanPipelineLayout.Create(fDevice);
 result.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,aPushSize);
 result.AddDescriptorSetLayout(aSetLayout);
 result.Initialize;
end;

function TpvFlexibleWaveletVideoDecoder.CreateComputePipeline(const aData;const aDataSize:TVkSize;const aLayout:TpvVulkanPipelineLayout;const aBlockSizeSpec:boolean;const aMotionBlockSpec:boolean):TpvVulkanComputePipeline;
var ShaderModule:TpvVulkanShaderModule;
    Stage:TpvVulkanPipelineShaderStage;
    SpecValues:array[0..1] of TpvInt32;
begin

 // Module + stage are transient: the compiled pipeline keeps what it needs, so both are freed afterwards
 ShaderModule:=TpvVulkanShaderModule.Create(fDevice,aData,aDataSize);
 try
  Stage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,ShaderModule,'main');
  try

   // bitplane_unpack bakes the coding block size into spec constant 0 (BS) + the COOP flag into spec 1
   if aBlockSizeSpec then begin
    SpecValues[0]:=fBlockSize;
    if fBlockSize=128 then begin
     SpecValues[1]:=1;
    end else begin
     SpecValues[1]:=0;
    end;
    Stage.AddSpecializationMapEntry(0,0,SizeOf(TpvInt32));
    Stage.AddSpecializationMapEntry(1,SizeOf(TpvInt32),SizeOf(TpvInt32));
    Stage.AddSpecializationDataFromMemory(@SpecValues[0],SizeOf(SpecValues),true);
   end else if aMotionBlockSpec then begin
    // mc.comp bakes the motion block size into spec constant 0 (MB)
    SpecValues[0]:=fMotionBlock;
    Stage.AddSpecializationMapEntry(0,0,SizeOf(TpvInt32));
    Stage.AddSpecializationDataFromMemory(@SpecValues[0],SizeOf(TpvInt32),true);
   end;

   result:=TpvVulkanComputePipeline.Create(fDevice,fPipelineCache,0,Stage,aLayout,nil,0);

  finally
   Stage.Free;
  end;
 finally
  ShaderModule.Free;
 end;
end;

function TpvFlexibleWaveletVideoDecoder.CreateStorageBuffer(const aSize:TVkDeviceSize;const aDeviceLocal:boolean;const aName:TpvUTF8String):TpvVulkanBuffer;
begin
 if aDeviceLocal then begin
  // GPU-only role (coeff / scratch / previous — never CPU-mapped): strictly device-local memory. TRANSFER_DST/SRC
  // are added so later stages can vkCmdCopyBuffer a DPB / GOP slot into the coefficient buffers.
  result:=TpvVulkanBuffer.Create(fDevice,
                                 aSize,
                                 TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT),
                                 VK_SHARING_MODE_EXCLUSIVE,
                                 [],
                                 TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                 0,0,0,
                                 0,0,0,0,
                                 [],
                                 0,
                                 0,
                                 aName);
 end else begin
  // host-visible|coherent role (data / offset / step / mv — CPU-mapped each frame for upload); the short
  // overload's required = HOST_VISIBLE | HOST_COHERENT, preferred = DEVICE_LOCAL.
  result:=TpvVulkanBuffer.Create(fDevice,aSize,TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),0);
 end;
end;

function TpvFlexibleWaveletVideoDecoder.AllocateSet(const aLayout:TpvVulkanDescriptorSetLayout):TpvVulkanDescriptorSet;
begin
 result:=TpvVulkanDescriptorSet.Create(fDescriptorPool,aLayout);
end;

procedure TpvFlexibleWaveletVideoDecoder.BindStorageBuffer(const aSet:TpvVulkanDescriptorSet;const aBinding:TpvUInt32;const aBuffer:TpvVulkanBuffer);
var BufferInfo:TVkDescriptorBufferInfo;
begin
 BufferInfo.buffer:=aBuffer.Handle;
 BufferInfo.offset:=0;
 BufferInfo.range:=VK_WHOLE_SIZE;
 aSet.WriteToDescriptorSet(aBinding,0,1,TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),[],[BufferInfo],[],false);
end;

procedure TpvFlexibleWaveletVideoDecoder.BindStorageBufferOffset(const aSet:TpvVulkanDescriptorSet;const aBinding:TpvUInt32;const aBuffer:TpvVulkanBuffer;const aByteOffset,aRange:TVkDeviceSize);
var BufferInfo:TVkDescriptorBufferInfo;
begin
 // MCTF binds a single GOP frame at a byte offset (the buffer holds gop_capacity frames). The offset must be a
 // multiple of minStorageBufferOffsetAlignment, which holds here since plane_pixels*4 is a large power-of-two-ish span.
 BufferInfo.buffer:=aBuffer.Handle;
 BufferInfo.offset:=aByteOffset;
 BufferInfo.range:=aRange;
 aSet.WriteToDescriptorSet(aBinding,0,1,TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),[],[BufferInfo],[],false);
end;

procedure TpvFlexibleWaveletVideoDecoder.BindStorageImage(const aSet:TpvVulkanDescriptorSet;const aBinding:TpvUInt32);
var ImageInfo:TVkDescriptorImageInfo;
begin
 ImageInfo.sampler:=VK_NULL_HANDLE;
 ImageInfo.imageView:=fOutputImageStorageView.Handle; // UNORM view of the sRGB image -> imageStore writes raw gamma bytes
 ImageInfo.imageLayout:=VK_IMAGE_LAYOUT_GENERAL;
 aSet.WriteToDescriptorSet(aBinding,0,1,TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),[ImageInfo],[],[],false);
end;

procedure TpvFlexibleWaveletVideoDecoder.BuildPipelines;
begin

 // 4 descriptor set layouts shared by the decode pipelines
 fDSL1:=CreateDescriptorSetLayout(1,false);
 fDSL2:=CreateDescriptorSetLayout(2,false);
 fDSL3:=CreateDescriptorSetLayout(3,false);
 fDSLColour:=CreateDescriptorSetLayout(3,true);

 // pipeline layouts (push-constant sizes match the C shaders)
 fPLUnpack:=CreatePipelineLayout(fDSL3,16);
 fPLDequant:=CreatePipelineLayout(fDSL2,8);
 fPLTranspose:=CreatePipelineLayout(fDSL2,16);
 fPLRow:=CreatePipelineLayout(fDSL1,16);
 fPLRound:=CreatePipelineLayout(fDSL1,4);
 fPLCoeffAdd:=CreatePipelineLayout(fDSL2,8); // coefdiff (A) P-frame coeff_add: {coeff, previous}, push [pixel_count, is_predicted]
 fPLColour:=CreatePipelineLayout(fDSLColour,24);
 fPLColourHDR:=CreatePipelineLayout(fDSLColour,32);

 // intra-decode compute pipelines from the embedded SPIR-V
 fPipeUnpack:=CreateComputePipeline(FlexibleWaveletVideoBitplaneUnpackSPIRVData,FlexibleWaveletVideoBitplaneUnpackSPIRVDataSize,fPLUnpack,true);
 fPipeDequant:=CreateComputePipeline(FlexibleWaveletVideoDequant97SPIRVData,FlexibleWaveletVideoDequant97SPIRVDataSize,fPLDequant,false);
 fPipeTranspose:=CreateComputePipeline(FlexibleWaveletVideoTransposeFSPIRVData,FlexibleWaveletVideoTransposeFSPIRVDataSize,fPLTranspose,false);
 fPipeIDWT97:=CreateComputePipeline(FlexibleWaveletVideoIdwt97rowSPIRVData,FlexibleWaveletVideoIdwt97rowSPIRVDataSize,fPLRow,false);
 fPipeIDWT53:=CreateComputePipeline(FlexibleWaveletVideoIdwt53rowSPIRVData,FlexibleWaveletVideoIdwt53rowSPIRVDataSize,fPLRow,false);
 fPipeRound:=CreateComputePipeline(FlexibleWaveletVideoRound97SPIRVData,FlexibleWaveletVideoRound97SPIRVDataSize,fPLRound,false);
 fPipeCoeffAdd:=CreateComputePipeline(FlexibleWaveletVideoCoeffAddSPIRVData,FlexibleWaveletVideoCoeffAddSPIRVDataSize,fPLCoeffAdd,false);
 // colordiff (B): mc (3 buffers + motion-block spec const, push 12) + motion_add (3 buffers, push 8), both on the unpack layout
 fPipeMC:=CreateComputePipeline(FlexibleWaveletVideoMcSPIRVData,FlexibleWaveletVideoMcSPIRVDataSize,fPLUnpack,false,true);
 fPipeMotionAdd:=CreateComputePipeline(FlexibleWaveletVideoMotionAddSPIRVData,FlexibleWaveletVideoMotionAddSPIRVDataSize,fPLUnpack,false);
 // B-frames: the weighted L0/L1 blend (on the unpack layout, push 12) + the per-block-mode blend (own layout, push 20)
 if fHasBFrames then begin
  fPipeBidiBlend:=CreateComputePipeline(FlexibleWaveletVideoBidiBlendSPIRVData,FlexibleWaveletVideoBidiBlendSPIRVDataSize,fPLUnpack,false);
  if fHasPerBlockMode then begin
   fPLBlendMode:=CreatePipelineLayout(fDSL3,20);
   fPipeBlendMode:=CreateComputePipeline(FlexibleWaveletVideoBlendModeSPIRVData,FlexibleWaveletVideoBlendModeSPIRVDataSize,fPLBlendMode,false,true);
  end;
 end;
 // 3D-DWT temporal-axis inverse: tdwt_int (lossless) / tdwt_float (lossy), 1 buffer (the whole GOP), push 20
 if fMode3DDWT then begin
  fPLTemporal:=CreatePipelineLayout(fDSL1,20);
  fPipeTDWTInt:=CreateComputePipeline(FlexibleWaveletVideoTdwtIntSPIRVData,FlexibleWaveletVideoTdwtIntSPIRVDataSize,fPLTemporal,false);
  fPipeTDWTFloat:=CreateComputePipeline(FlexibleWaveletVideoTdwtFloatSPIRVData,FlexibleWaveletVideoTdwtFloatSPIRVDataSize,fPLTemporal,false);
 end;
 fPipeColour:=CreateComputePipeline(FlexibleWaveletVideoColorSPIRVData,FlexibleWaveletVideoColorSPIRVDataSize,fPLColour,false);
 fPipeColourHDR:=CreateComputePipeline(FlexibleWaveletVideoColorHdrSPIRVData,FlexibleWaveletVideoColorHdrSPIRVDataSize,fPLColourHDR,false);
 fPipeColourHDRSCRGB:=CreateComputePipeline(FlexibleWaveletVideoColorHdrScrgbSPIRVData,FlexibleWaveletVideoColorHdrScrgbSPIRVDataSize,fPLColourHDR,false);

end;

procedure TpvFlexibleWaveletVideoDecoder.BuildBuffersAndImage;
var Plane,SlotIndex:TpvInt32;
    DataCapacity,PlaneBytes,ScratchSide:TVkDeviceSize;
    LumaBlockCount:TpvInt32;
    MemoryRequirements:TVkMemoryRequirements;
    RequiresDedicated,PrefersDedicated:boolean;
    MemoryBlockFlags:TpvVulkanDeviceMemoryBlockFlags;
    ImageHandle:TVkImage;
begin

 // host-visible payload buffer (bitplane data + per-block offsets) + per-plane host-visible offset buffers
 LumaBlockCount:=BlockCountX(fWidth)*BlockCountY(fHeight);
 DataCapacity:=(TVkDeviceSize(fWidth)*TVkDeviceSize(fHeight)*4)+(TVkDeviceSize(LumaBlockCount)*16);
 fDataBuffer:=CreateStorageBuffer(DataCapacity,false,'FWV.data');

 // per-plane working buffers
 for Plane:=0 to 2 do begin
  PlaneBytes:=TVkDeviceSize(PlaneWidth(Plane))*TVkDeviceSize(PlaneHeight(Plane))*4;
  fOffsetBuffer[Plane]:=CreateStorageBuffer(TVkDeviceSize(LumaBlockCount)*4,false,'FWV.offset');
  fStepBuffer[Plane]:=CreateStorageBuffer(PlaneBytes,false,'FWV.step');
  fCoeffBuffer[Plane]:=CreateStorageBuffer(PlaneBytes,true,'FWV.coeff');
  fPreviousBuffer[Plane]:=CreateStorageBuffer(PlaneBytes,true,'FWV.previous'); // P-frame reference, GPU-resident across frames
 end;

 // device-local DWT transpose scratch (a W x H plane transposes through stride max(W,H))
 if fWidth>fHeight then begin
  ScratchSide:=fWidth;
 end else begin
  ScratchSide:=fHeight;
 end;
 fScratchBuffer:=CreateStorageBuffer(ScratchSide*ScratchSide*4,true,'FWV.scratch');

 // colordiff (B) motion-vector field: per luma motion block [mv_x, mv_y] (half-pel), host-visible
 fMVBuffer:=CreateStorageBuffer(TVkDeviceSize(MotionBlocksX(fWidth))*TVkDeviceSize(MotionBlocksY(fHeight))*2*4,false,'FWV.mv');

 // hierarchical B-frames: the DPB slot pool (device-local YCoCg) + the L0/L1 MC references + the L1 MV field
 // and per-block mode (host-visible).
 if fHasBFrames then begin
  fMV1Buffer:=CreateStorageBuffer(TVkDeviceSize(MotionBlocksX(fWidth))*TVkDeviceSize(MotionBlocksY(fHeight))*2*4,false,'FWV.mv1');
  fModeBuffer:=CreateStorageBuffer(TVkDeviceSize(MotionBlocksX(fWidth))*TVkDeviceSize(MotionBlocksY(fHeight))*4,false,'FWV.mode');
  SetLength(fDPBBuffer,fGDPBSlots);
  for SlotIndex:=0 to fGDPBSlots-1 do begin
   for Plane:=0 to 2 do begin
    PlaneBytes:=TVkDeviceSize(PlaneWidth(Plane))*TVkDeviceSize(PlaneHeight(Plane))*4;
    fDPBBuffer[SlotIndex][Plane]:=CreateStorageBuffer(PlaneBytes,true,'FWV.dpb');
   end;
  end;
  for Plane:=0 to 2 do begin
   PlaneBytes:=TVkDeviceSize(PlaneWidth(Plane))*TVkDeviceSize(PlaneHeight(Plane))*4;
   fGMCBuffer[0][Plane]:=CreateStorageBuffer(PlaneBytes,true,'FWV.gmc0');
   fGMCBuffer[1][Plane]:=CreateStorageBuffer(PlaneBytes,true,'FWV.gmc1');
  end;
 end;

 // 3D-DWT: the whole GOP (gop_capacity frames per plane, contiguous slots), device-local
 if fMode3DDWT then begin
  for Plane:=0 to 2 do begin
   PlaneBytes:=TVkDeviceSize(PlaneWidth(Plane))*TVkDeviceSize(PlaneHeight(Plane))*4;
   fGopBuffer[Plane]:=CreateStorageBuffer(TVkDeviceSize(fGOPCapacity)*PlaneBytes,true,'FWV.gop');
  end;
  // MCTF: the per-pair MC prediction + the per-level interleaved-frame workspace + the GOP's luma MV fields
  if fMCTF then begin
   for Plane:=0 to 2 do begin
    PlaneBytes:=TVkDeviceSize(PlaneWidth(Plane))*TVkDeviceSize(PlaneHeight(Plane))*4;
    fMCTFPred[Plane]:=CreateStorageBuffer(PlaneBytes,true,'FWV.mctfpred');
    fMCTFScratch[Plane]:=CreateStorageBuffer(TVkDeviceSize(fGOPCapacity)*PlaneBytes,true,'FWV.mctfscratch');
   end;
   SetLength(fMCTFMVScratch,fGOPCapacity*MotionBlocksX(fWidth)*MotionBlocksY(fHeight)*2);
  end;
 end;

 // the output RGB image (compute-written, then transfer-read for present/readback)
 fOutputImage:=TpvVulkanImage.Create(fDevice,
                                     fOutputImageFlags, // SDR: MUTABLE_FORMAT|EXTENDED_USAGE so the sRGB image takes a UNORM storage view
                                     VK_IMAGE_TYPE_2D,
                                     fOutputFormat,
                                     fWidth,fHeight,1,
                                     1,1,
                                     VK_SAMPLE_COUNT_1_BIT,
                                     VK_IMAGE_TILING_OPTIMAL,
                                     TVkImageUsageFlags(VK_IMAGE_USAGE_STORAGE_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_TRANSFER_SRC_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT), // STORAGE=compute write, TRANSFER_SRC=readback/blit-present, SAMPLED=textured-quad present (+ video-on-3D-textures)
                                     VK_SHARING_MODE_EXCLUSIVE,
                                     [],
                                     VK_IMAGE_LAYOUT_UNDEFINED);

 // The image create overload above only makes the VkImage handle; back it with device-local memory + bind.
 MemoryRequirements:=fDevice.MemoryManager.GetImageMemoryRequirements(fOutputImage.Handle,RequiresDedicated,PrefersDedicated);
 MemoryBlockFlags:=[];
 if RequiresDedicated then begin
  Include(MemoryBlockFlags,TpvVulkanDeviceMemoryBlockFlag.DedicatedAllocation);
 end else if PrefersDedicated then begin
  Include(MemoryBlockFlags,TpvVulkanDeviceMemoryBlockFlag.PreferDedicatedAllocation);
 end;
 ImageHandle:=fOutputImage.Handle;
 fOutputImageMemory:=fDevice.MemoryManager.AllocateMemoryBlock(MemoryBlockFlags,
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
                                                               @ImageHandle,
                                                               0,
                                                               'FWV.output');
 if not assigned(fOutputImageMemory) then begin
  raise EpvFlexibleWaveletVideoDecoder.Create('Output image memory allocation failed');
 end;
 VulkanCheckResult(fDevice.Commands.BindImageMemory(fDevice.Handle,fOutputImage.Handle,fOutputImageMemory.MemoryChunk.Handle,fOutputImageMemory.Offset));

 // sample / present view: fOutputFormat (sRGB for SDR -> samples to linear; FP16 for HDR)
 fOutputImageView:=TpvVulkanImageView.Create(fDevice,
                                             fOutputImage,
                                             VK_IMAGE_VIEW_TYPE_2D,
                                             fOutputFormat,
                                             VK_COMPONENT_SWIZZLE_IDENTITY,
                                             VK_COMPONENT_SWIZZLE_IDENTITY,
                                             VK_COMPONENT_SWIZZLE_IDENTITY,
                                             VK_COMPONENT_SWIZZLE_IDENTITY,
                                             TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                             0,1,0,1);

 // compute storage view: fOutputStorageFormat (UNORM for SDR so imageStore writes raw gamma bytes; FP16 for HDR)
 fOutputImageStorageView:=TpvVulkanImageView.Create(fDevice,
                                                    fOutputImage,
                                                    VK_IMAGE_VIEW_TYPE_2D,
                                                    fOutputStorageFormat,
                                                    VK_COMPONENT_SWIZZLE_IDENTITY,
                                                    VK_COMPONENT_SWIZZLE_IDENTITY,
                                                    VK_COMPONENT_SWIZZLE_IDENTITY,
                                                    VK_COMPONENT_SWIZZLE_IDENTITY,
                                                    TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                    0,1,0,1);

end;

procedure TpvFlexibleWaveletVideoDecoder.BuildDescriptorSets;
var Plane,MaxSets,MaxBuffers:TpvInt32;
begin

 MaxSets:=64;
 MaxBuffers:=256;
 if fHasBFrames and (fSubmitMode=1) then begin // mode B: + the per-frame input ring (~21 sets / ~60 buffers per slot)
  MaxSets:=MaxSets+(fBufferRingSize*24);
  MaxBuffers:=MaxBuffers+(fBufferRingSize*70);
 end;
 fDescriptorPool:=TpvVulkanDescriptorPool.Create(fDevice,TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),MaxSets);
 fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,MaxBuffers);
 fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,2);
 fDescriptorPool.Initialize;

 // per-plane sets: unpack (data, offset, coeff), dequant (coeff, step), the two transpose directions, the row pass
 for Plane:=0 to 2 do begin

  fSetUnpack[Plane]:=AllocateSet(fDSL3);
  BindStorageBuffer(fSetUnpack[Plane],0,fDataBuffer);
  BindStorageBuffer(fSetUnpack[Plane],1,fOffsetBuffer[Plane]);
  BindStorageBuffer(fSetUnpack[Plane],2,fCoeffBuffer[Plane]);
  fSetUnpack[Plane].Flush;

  fSetDequant[Plane]:=AllocateSet(fDSL2);
  BindStorageBuffer(fSetDequant[Plane],0,fCoeffBuffer[Plane]);
  BindStorageBuffer(fSetDequant[Plane],1,fStepBuffer[Plane]);
  fSetDequant[Plane].Flush;

  fSetAdd[Plane]:=AllocateSet(fDSL2); // coefdiff (A): {coeff, previous}
  BindStorageBuffer(fSetAdd[Plane],0,fCoeffBuffer[Plane]);
  BindStorageBuffer(fSetAdd[Plane],1,fPreviousBuffer[Plane]);
  fSetAdd[Plane].Flush;

  fSetMCPlay[Plane]:=AllocateSet(fDSL3); // colordiff (B) mc: {previous, mv, scratch=mc_prev}
  BindStorageBuffer(fSetMCPlay[Plane],0,fPreviousBuffer[Plane]);
  BindStorageBuffer(fSetMCPlay[Plane],1,fMVBuffer);
  BindStorageBuffer(fSetMCPlay[Plane],2,fScratchBuffer);
  fSetMCPlay[Plane].Flush;

  fSetMotionAddPlay[Plane]:=AllocateSet(fDSL3); // colordiff (B) motion_add: {coeff, scratch=mc_prev, previous}
  BindStorageBuffer(fSetMotionAddPlay[Plane],0,fCoeffBuffer[Plane]);
  BindStorageBuffer(fSetMotionAddPlay[Plane],1,fScratchBuffer);
  BindStorageBuffer(fSetMotionAddPlay[Plane],2,fPreviousBuffer[Plane]);
  fSetMotionAddPlay[Plane].Flush;

  fSetCoeffToScratch[Plane]:=AllocateSet(fDSL2);
  BindStorageBuffer(fSetCoeffToScratch[Plane],0,fCoeffBuffer[Plane]);
  BindStorageBuffer(fSetCoeffToScratch[Plane],1,fScratchBuffer);
  fSetCoeffToScratch[Plane].Flush;

  fSetScratchToCoeff[Plane]:=AllocateSet(fDSL2);
  BindStorageBuffer(fSetScratchToCoeff[Plane],0,fScratchBuffer);
  BindStorageBuffer(fSetScratchToCoeff[Plane],1,fCoeffBuffer[Plane]);
  fSetScratchToCoeff[Plane].Flush;

  fSetRow[Plane]:=AllocateSet(fDSL1);
  BindStorageBuffer(fSetRow[Plane],0,fCoeffBuffer[Plane]);
  fSetRow[Plane].Flush;

 end;

 // the scratch-buffer row pass, and the colour set (3 coeff planes + the output image)
 fSetRowScratch:=AllocateSet(fDSL1);
 BindStorageBuffer(fSetRowScratch,0,fScratchBuffer);
 fSetRowScratch.Flush;

 fSetColour:=AllocateSet(fDSLColour);
 BindStorageBuffer(fSetColour,0,fCoeffBuffer[0]);
 BindStorageBuffer(fSetColour,1,fCoeffBuffer[1]);
 BindStorageBuffer(fSetColour,2,fCoeffBuffer[2]);
 BindStorageImage(fSetColour,3);
 fSetColour.Flush;

 // hierarchical B-frames: the 5 per-plane sets are allocated once but REWRITTEN per decode-ahead frame (their
 // DPB ref slots change), so they are bound in DecodeAheadFrame, not here.
 if fHasBFrames then begin
  for Plane:=0 to 2 do begin
   fSetGMC0[Plane]:=AllocateSet(fDSL3);
   fSetGMC1[Plane]:=AllocateSet(fDSL3);
   fSetGBlend[Plane]:=AllocateSet(fDSL3);
   fSetGAdd[Plane]:=AllocateSet(fDSL3);
   if fHasPerBlockMode then begin
    fSetGBlendMode[Plane]:=AllocateSet(fDSL3);
   end;
  end;
 end;

 // 3D-DWT temporal set: {gop_buffer[plane]}; MCTF mc/add sets are rebound per pair (with byte offsets)
 if fMode3DDWT then begin
  for Plane:=0 to 2 do begin
   fSetTemporal[Plane]:=AllocateSet(fDSL1);
   BindStorageBuffer(fSetTemporal[Plane],0,fGopBuffer[Plane]);
   fSetTemporal[Plane].Flush;
   if fMCTF then begin
    fSetMCTFMC[Plane]:=AllocateSet(fDSL3);
    fSetMCTFAdd[Plane]:=AllocateSet(fDSL2);
   end;
  end;
 end;

end;

function TpvFlexibleWaveletVideoDecoder.ChromaShiftX:TpvInt32;
begin
 if fChromaFormat=0 then begin
  result:=0; // 4:4:4 keeps the full chroma width
 end else begin
  result:=1; // 4:2:2 / 4:2:0 halve horizontally
 end;
end;

function TpvFlexibleWaveletVideoDecoder.ChromaShiftY:TpvInt32;
begin
 if fChromaFormat=2 then begin
  result:=1; // only 4:2:0 halves vertically
 end else begin
  result:=0;
 end;
end;

procedure TpvFlexibleWaveletVideoDecoder.RecordComputeBarrier(const aCommandBuffer:TpvVulkanCommandBuffer);
var Barrier:TVkMemoryBarrier;
begin
 // The shared compute->compute barrier between decode passes (the C decoder's memory_barrier()).
 FillChar(Barrier,SizeOf(Barrier),#0);
 Barrier.sType:=VK_STRUCTURE_TYPE_MEMORY_BARRIER;
 Barrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
 Barrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   1,@Barrier,
                                   0,nil,
                                   0,nil);
end;

procedure TpvFlexibleWaveletVideoDecoder.RecordImageBarrier(const aCommandBuffer:TpvVulkanCommandBuffer;const aOldLayout,aNewLayout:TVkImageLayout;const aSrcAccess,aDstAccess:TVkAccessFlags;const aSrcStage,aDstStage:TVkPipelineStageFlags);
var Barrier:TVkImageMemoryBarrier;
begin
 FillChar(Barrier,SizeOf(Barrier),#0);
 Barrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
 Barrier.srcAccessMask:=aSrcAccess;
 Barrier.dstAccessMask:=aDstAccess;
 Barrier.oldLayout:=aOldLayout;
 Barrier.newLayout:=aNewLayout;
 Barrier.srcQueueFamilyIndex:=TVkUInt32(VK_QUEUE_FAMILY_IGNORED);
 Barrier.dstQueueFamilyIndex:=TVkUInt32(VK_QUEUE_FAMILY_IGNORED);
 Barrier.image:=fOutputImage.Handle;
 Barrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 Barrier.subresourceRange.baseMipLevel:=0;
 Barrier.subresourceRange.levelCount:=1;
 Barrier.subresourceRange.baseArrayLayer:=0;
 Barrier.subresourceRange.layerCount:=1;
 aCommandBuffer.CmdPipelineBarrier(aSrcStage,aDstStage,
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@Barrier);
end;

procedure TpvFlexibleWaveletVideoDecoder.RecordDispatch(const aCommandBuffer:TpvVulkanCommandBuffer;const aPipeline:TpvVulkanComputePipeline;const aLayout:TpvVulkanPipelineLayout;const aSet:TpvVulkanDescriptorSet;const aPushConstants:Pointer;const aPushSize:TpvUInt32;const aGroupsX,aGroupsY,aGroupsZ:TpvUInt32);
var SetHandle:TVkDescriptorSet;
begin
 SetHandle:=aSet.Handle;
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,aPipeline.Handle);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,aLayout.Handle,0,1,@SetHandle,0,nil);
 aCommandBuffer.CmdPushConstants(aLayout.Handle,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,aPushSize,aPushConstants);
 aCommandBuffer.CmdDispatch(aGroupsX,aGroupsY,aGroupsZ);
end;

function TpvFlexibleWaveletVideoDecoder.EnsureStepCacheSlot(const aQuality:TpvInt32):TpvInt32;
var Slot,Plane,PlanePixels:TpvInt32;
begin

 // already built for this quality -> reuse (the step map is content-independent, so it is identical every frame)
 for Slot:=0 to length(fStepCacheQuality)-1 do begin
  if fStepCacheQuality[Slot]=aQuality then begin
   result:=Slot;
   exit;
  end;
 end;

 // first time this quality appears -> build all three planes once (caller has ensured the synthesis gains are ready)
 Slot:=length(fStepCacheQuality);
 SetLength(fStepCacheQuality,Slot+1);
 SetLength(fStepCacheData,(Slot+1)*3);
 fStepCacheQuality[Slot]:=aQuality;
 for Plane:=0 to 2 do begin
  PlanePixels:=PlaneWidth(Plane)*PlaneHeight(Plane);
  SetLength(fStepCacheData[(Slot*3)+Plane],PlanePixels);
  BuildQuantizationSteps(PpvInt32Array(@fStepCacheData[(Slot*3)+Plane][0]),PlaneWidth(Plane),PlaneHeight(Plane),fLevels,aQuality,fSampleWhite,fHFGain,fLLGain);
 end;

 result:=Slot;
end;

procedure TpvFlexibleWaveletVideoDecoder.UploadFrame(const aFrameIndex:TpvInt32);
var Plane,PlanePixels,StepSlot:TpvInt32;
    Entry:PFrameEntry;
    CompressedLength:TpvSizeUInt;
    RawLength,DataLength:TpvUInt32;
    BlockCount:TBlockCounts;
    Offsets:TPlaneOffsets;
    LeadingBlockCount:TpvInt32;
    MVDataOffset,BlockDataOffset:TpvSizeUInt;
    MVLength:TpvUInt32;
    DataPointer:PpvUInt8Array;
    IsPredicted:boolean;
    MotionBlockCountX,MotionBlockCountY,MVComponentCount:TpvInt32;
    MVReader:TBitReader;
    MVRangeDecoder:TMVRangeDecoder;
begin

 if (aFrameIndex<0) or (aFrameIndex>=fFrameCount) then begin
  raise EpvFlexibleWaveletVideoDecoder.Create('Frame index out of range');
 end;
 Entry:=@fFrameEntries[aFrameIndex];

 // Read the compressed container payload for this frame.
 CompressedLength:=Entry^.Size;
 if TpvSizeUInt(Length(fCompressedScratch))<(CompressedLength+8) then begin
  SetLength(fCompressedScratch,CompressedLength+8); // +8 slack guards a final-token input over-read
 end;
 fStream.Position:=TpvInt64(Entry^.Offset);
 fStream.ReadBuffer(fCompressedScratch[0],CompressedLength);

 // Decompress (method 0 = raw, 1 = LZSS, 2 = LZBRRC) into the frame scratch. The raw length sits right
 // after the method byte in every framing variant, so peek it to size the destination.
 RawLength:=ReadU32LE(PpvUInt8Array(@fCompressedScratch[0]),1);
 if TpvSizeUInt(Length(fFrameScratch))<RawLength then begin
  SetLength(fFrameScratch,RawLength);
 end;
 if not DecompressFrame(PpvUInt8Array(@fCompressedScratch[0]),CompressedLength,PpvUInt8Array(@fFrameScratch[0]),TpvSizeUInt(Length(fFrameScratch)),RawLength) then begin
  raise EpvFlexibleWaveletVideoDecoder.Create('Frame decompression failed');
 end;

 // Lossy: upload the per-plane integer quant step maps the GPU dequant reads. The per-subband synthesis gains
 // depend only on the level count (measured once), and the step map itself depends only on the quality, so it is
 // built once per quality and cached (rebuilding it per frame was the 1080p CPU bottleneck).
 if not fLossless then begin
  if not fGainsComputed then begin
   MeasureSynthesisGains(fLevels,fHFGain,fLLGain);
   fGainsComputed:=true;
  end;
  StepSlot:=EnsureStepCacheSlot(Entry^.Quality);
  for Plane:=0 to 2 do begin
   PlanePixels:=PlaneWidth(Plane)*PlaneHeight(Plane);
   DataPointer:=PpvUInt8Array(ActiveStepBuffer(Plane).Memory.MapMemory);
   try
    Move(fStepCacheData[(StepSlot*3)+Plane][0],DataPointer^[0],TpvSizeUInt(PlanePixels)*4);
   finally
    ActiveStepBuffer(Plane).Memory.UnmapMemory;
   end;
  end;
 end;

 // Per-plane block counts for the size-table prefix sum (4:4:4 -> all equal to the luma count); the offsets
 // are prefix-summed into the CPU scratch (the GPU offset buffers are filled from it below).
 for Plane:=0 to 2 do begin
  BlockCount[Plane]:=BlockCountX(PlaneWidth(Plane))*BlockCountY(PlaneHeight(Plane));
  if TpvSizeUInt(Length(fOffsetScratch[Plane]))<TpvSizeUInt(BlockCount[Plane]) then begin
   SetLength(fOffsetScratch[Plane],BlockCount[Plane]);
  end;
  Offsets[Plane]:=PpvUInt32Array(@fOffsetScratch[Plane][0]);
 end;
 ParseFrameHeader(PpvUInt8Array(@fFrameScratch[0]),BlockCount,Offsets,LeadingBlockCount,MVDataOffset,MVLength,BlockDataOffset);

 // Upload the per-plane offset tables, then the packed bitplane bytes (data_length is the u32 right before
 // the block data). The host-visible buffers are pooled into shared memory chunks where only one map per
 // chunk may be live at a time, so each buffer is mapped / copied / unmapped on its own.
 for Plane:=0 to 2 do begin
  DataPointer:=PpvUInt8Array(ActiveOffsetBuffer(Plane).Memory.MapMemory);
  try
   Move(fOffsetScratch[Plane][0],DataPointer^[0],TpvSizeUInt(BlockCount[Plane])*4);
  finally
   ActiveOffsetBuffer(Plane).Memory.UnmapMemory;
  end;
 end;

 DataLength:=ReadU32LE(PpvUInt8Array(@fFrameScratch[0]),BlockDataOffset-4);
 DataPointer:=PpvUInt8Array(ActiveDataBuffer.Memory.MapMemory);
 try
  Move(fFrameScratch[BlockDataOffset],DataPointer^[0],DataLength);
 finally
  ActiveDataBuffer.Memory.UnmapMemory;
 end;

 // colordiff (B) P-frame: decode the motion-vector field and upload it for mc.comp. mv_length=0 means the
 // encoder coded no motion -> the field stays all-zero (mc is then the identity == plain colordiff).
 IsPredicted:=fFrameEntries[aFrameIndex].FrameType<>0;
 if (fPredictionMethod=1) and IsPredicted then begin
  // fixed grid: motion_blocks at fMotionBlock; variable: fMotionBlock=8 -> this is the fine 8-grid (fgx x fgy)
  MotionBlockCountX:=MotionBlocksX(fWidth);
  MotionBlockCountY:=MotionBlocksY(fHeight);
  MVComponentCount:=(MotionBlockCountX*MotionBlockCountY)*2;
  if TpvSizeUInt(Length(fMVScratch))<TpvSizeUInt(MVComponentCount) then begin
   SetLength(fMVScratch,MVComponentCount);
  end;
  FillChar(fMVScratch[0],TpvSizeUInt(MVComponentCount)*4,#0); // quadtree fills via leaf-expansion -> clear first (edges)
  if MVLength>0 then begin
   if fMVCodec=1 then begin // range-coded (mv_codec=1)
    MVRangeDecoder.Init(PpvUInt8Array(@fFrameScratch[MVDataOffset]),MVLength);
    if fMotionVariable then begin
     DecodeMotionQuadtreeRange(MVRangeDecoder,PpvInt32Array(@fMVScratch[0]),MotionBlockCountX,MotionBlockCountY);
    end else begin
     DecodeMotionVectorsRange(MVRangeDecoder,PpvInt32Array(@fMVScratch[0]),MotionBlockCountX,MotionBlockCountY);
    end;
   end else begin // signed Exp-Golomb (mv_codec=0)
    MVReader.Init(PpvUInt8Array(@fFrameScratch[MVDataOffset]),MVLength);
    if fMotionVariable then begin
     DecodeMotionQuadtree(MVReader,PpvInt32Array(@fMVScratch[0]),MotionBlockCountX,MotionBlockCountY);
    end else begin
     DecodeMotionVectors(MVReader,PpvInt32Array(@fMVScratch[0]),MotionBlockCountX,MotionBlockCountY);
    end;
   end;
  end;
  DataPointer:=PpvUInt8Array(ActiveMVBuffer.Memory.MapMemory);
  try
   Move(fMVScratch[0],DataPointer^[0],TpvSizeUInt(MVComponentCount)*4);
  finally
   ActiveMVBuffer.Memory.UnmapMemory;
  end;
 end;

end;

procedure TpvFlexibleWaveletVideoDecoder.RecordDecode(const aCommandBuffer:TpvVulkanCommandBuffer;const aIsPredicted:boolean);
var Plane,Level,LevelCount:TpvInt32;
    PlaneW,PlaneH,ScratchStride,PlanePixels:TpvInt32;
    PlaneBlocksX,PlaneBlocksY,PlaneBlockCount:TpvInt32;
    PlanePixelWorkgroups,PlaneUnpackWorkgroups:TpvInt32;
    CurrentWidth,CurrentHeight,LevelW,LevelH:TpvInt32;
    PixelWorkgroups:TpvInt32;
    LevelWidth,LevelHeight:array[0..15] of TpvInt32;
    RowPipeline:TpvVulkanComputePipeline;
    UnpackPush:array[0..3] of TpvInt32;
    DequantPush:array[0..1] of TpvInt32;
    AddPush:array[0..1] of TpvInt32;
    MCPush:array[0..2] of TpvInt32;
    TransposePush1,TransposePush2,RowPush1,RowPush2:array[0..3] of TpvInt32;
    ColourPush:array[0..5] of TpvInt32;
    HDRPush:array[0..7] of TpvInt32;
    PixelCountPush:TpvInt32;
    ChromaMultiplier,ExposureBits:TpvFloat;
    Predictive:boolean;
begin

 if fLossless then begin
  RowPipeline:=fPipeIDWT53; // reversible 5/3 inverse transform
 end else begin
  RowPipeline:=fPipeIDWT97; // 9/7 inverse transform
 end;

 // Stream-level prediction flag (C: predictive = (gop>1) && (method==1 ? 1 : lossless)). For an all-intra
 // stream (gop=1) this is false, so the predictive passes below never run. When it is set, even the I-frame
 // (is_predicted=0) runs the add pass to SEED the reference (it just stores, without adding).
 Predictive:=(fGOP>1) and ((fPredictionMethod=1) or fLossless);

 // The output image goes UNDEFINED -> GENERAL so the colour shader can store into it.
 RecordImageBarrier(aCommandBuffer,
                    VK_IMAGE_LAYOUT_UNDEFINED,VK_IMAGE_LAYOUT_GENERAL,
                    0,TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT));

 for Plane:=0 to 2 do begin

  // Per-plane dimensions: luma is full-res, chroma is subsampled when the chroma format is not 4:4:4.
  PlaneW:=PlaneWidth(Plane);
  PlaneH:=PlaneHeight(Plane);
  if PlaneW>PlaneH then begin
   ScratchStride:=PlaneW; // wide plane: transpose scratch keeps the row stride
  end else begin
   ScratchStride:=PlaneH; // tall plane: grow the stride to avoid row overlap
  end;
  PlanePixels:=PlaneW*PlaneH;
  PlaneBlocksX:=BlockCountX(PlaneW);
  PlaneBlocksY:=BlockCountY(PlaneH);
  PlaneBlockCount:=PlaneBlocksX*PlaneBlocksY;
  PlanePixelWorkgroups:=(PlanePixels+255) div 256;
  if fBlockSize=128 then begin
   PlaneUnpackWorkgroups:=PlaneBlockCount; // coop: one workgroup per block
  end else begin
   PlaneUnpackWorkgroups:=(PlaneBlockCount+63) div 64;
  end;

  // unpack: packed bytes + per-block offsets -> coefficients
  UnpackPush[0]:=PlaneW;
  UnpackPush[1]:=PlaneH;
  UnpackPush[2]:=PlaneBlocksX;
  UnpackPush[3]:=PlaneBlocksY;
  RecordDispatch(aCommandBuffer,fPipeUnpack,fPLUnpack,fSetUnpack[Plane],@UnpackPush[0],16,PlaneUnpackWorkgroups,1,1);
  RecordComputeBarrier(aCommandBuffer);

  // coefdiff (A) P-frame: add the previous coefficients to the unpacked difference here (BEFORE the inverse),
  // and save the reconstructed coefficients as the next reference. (colordiff adds AFTER the inverse instead.)
  if Predictive and (fPredictionMethod=0) then begin
   AddPush[0]:=PlanePixels;
   AddPush[1]:=Ord(aIsPredicted);
   RecordDispatch(aCommandBuffer,fPipeCoeffAdd,fPLCoeffAdd,fSetAdd[Plane],@AddPush[0],8,PlanePixelWorkgroups,1,1);
   RecordComputeBarrier(aCommandBuffer);
  end;

  // lossy: dequantize the coefficients (chroma uses the chroma quant multiplier; raw float bits in the push)
  if not fLossless then begin
   DequantPush[0]:=PlanePixels;
   if Plane=0 then begin
    ChromaMultiplier:=1.0;
   end else begin
    ChromaMultiplier:=fChromaQuant;
   end;
   DequantPush[1]:=PpvInt32(@ChromaMultiplier)^;
   RecordDispatch(aCommandBuffer,fPipeDequant,fPLDequant,fSetDequant[Plane],@DequantPush[0],8,PlanePixelWorkgroups,1,1);
   RecordComputeBarrier(aCommandBuffer);
  end;

  // Inverse 2D wavelet, coarsest level first. The level pyramid is per-plane (from PlaneW/PlaneH).
  LevelCount:=0;
  CurrentWidth:=PlaneW;
  CurrentHeight:=PlaneH;
  Level:=0;
  while ((Level<fLevels) and (CurrentWidth>=2)) and (CurrentHeight>=2) do begin
   LevelWidth[LevelCount]:=CurrentWidth;
   LevelHeight[LevelCount]:=CurrentHeight;
   inc(LevelCount);
   CurrentWidth:=(CurrentWidth+1) div 2;
   CurrentHeight:=(CurrentHeight+1) div 2;
   inc(Level);
  end;
  for Level:=LevelCount-1 downto 0 do begin
   LevelW:=LevelWidth[Level];
   LevelH:=LevelHeight[Level];

   // coeff (stride PlaneW) -> scratch (stride ScratchStride)
   TransposePush1[0]:=PlaneW;
   TransposePush1[1]:=LevelW;
   TransposePush1[2]:=LevelH;
   TransposePush1[3]:=ScratchStride;
   RecordDispatch(aCommandBuffer,fPipeTranspose,fPLTranspose,fSetCoeffToScratch[Plane],@TransposePush1[0],16,(LevelW+15) div 16,(LevelH+15) div 16,1);
   RecordComputeBarrier(aCommandBuffer);

   // row pass on scratch
   RowPush1[0]:=ScratchStride;
   RowPush1[1]:=LevelH;
   RowPush1[2]:=LevelW;
   RowPush1[3]:=1;
   RecordDispatch(aCommandBuffer,RowPipeline,fPLRow,fSetRowScratch,@RowPush1[0],16,LevelW,1,1);
   RecordComputeBarrier(aCommandBuffer);

   // scratch (stride ScratchStride) -> coeff (stride PlaneW)
   TransposePush2[0]:=ScratchStride;
   TransposePush2[1]:=LevelH;
   TransposePush2[2]:=LevelW;
   TransposePush2[3]:=PlaneW;
   RecordDispatch(aCommandBuffer,fPipeTranspose,fPLTranspose,fSetScratchToCoeff[Plane],@TransposePush2[0],16,(LevelH+15) div 16,(LevelW+15) div 16,1);
   RecordComputeBarrier(aCommandBuffer);

   // row pass on coeff
   RowPush2[0]:=PlaneW;
   RowPush2[1]:=LevelW;
   RowPush2[2]:=LevelH;
   RowPush2[3]:=1;
   RecordDispatch(aCommandBuffer,RowPipeline,fPLRow,fSetRow[Plane],@RowPush2[0],16,LevelH,1,1);
   RecordComputeBarrier(aCommandBuffer);
  end;

  // lossy: round the float-scaled coefficients back to integer pixels
  if not fLossless then begin
   PixelCountPush:=PlanePixels;
   RecordDispatch(aCommandBuffer,fPipeRound,fPLRound,fSetRow[Plane],@PixelCountPush,4,PlanePixelWorkgroups,1,1);
   RecordComputeBarrier(aCommandBuffer);
  end;

  // colordiff (B) P-frame: motion-compensate the previous reconstructed YCoCg into scratch (mc_prev), then add
  // it to the residual AFTER the inverse, saving the result as the next reference. mv=0 -> mc_prev == previous.
  if Predictive and (fPredictionMethod=1) then begin
   if aIsPredicted then begin
    MCPush[0]:=PlaneW;
    MCPush[1]:=PlaneH;
    MCPush[2]:=MotionBlocksX(PlaneW);
    RecordDispatch(aCommandBuffer,fPipeMC,fPLUnpack,fSetMCPlay[Plane],@MCPush[0],12,PlanePixelWorkgroups,1,1);
    RecordComputeBarrier(aCommandBuffer);
   end;
   AddPush[0]:=PlanePixels;
   AddPush[1]:=Ord(aIsPredicted);
   RecordDispatch(aCommandBuffer,fPipeMotionAdd,fPLUnpack,fSetMotionAddPlay[Plane],@AddPush[0],8,PlanePixelWorkgroups,1,1);
   RecordComputeBarrier(aCommandBuffer);
  end;

 end;

 // colour: YCoCg(-R) -> RGB into the output image. Chroma upsample params: shift + the stored Co/Cg dims
 // (4:4:4 -> shift 0 + small dims == frame dims, so the upsample reduces to identity).
 PixelWorkgroups:=((fWidth*fHeight)+255) div 256;
 if fIsHDR then begin
  // HDR: scRGB FP16 (real HDR display) or PQ/HLG -> tonemap -> sRGB8 (the SDR fallback). Same 32-byte push + set.
  HDRPush[0]:=fWidth;
  HDRPush[1]:=fHeight;
  ExposureBits:=fHDRExposure;
  HDRPush[2]:=PpvInt32(@ExposureBits)^; // raw float bits (the shader reads a float exposure)
  HDRPush[3]:=fTransferFunction;
  HDRPush[4]:=ChromaShiftX;
  HDRPush[5]:=ChromaShiftY;
  HDRPush[6]:=PlaneWidth(1);
  HDRPush[7]:=PlaneHeight(1);
  if fUseSCRGB then begin
   RecordDispatch(aCommandBuffer,fPipeColourHDRSCRGB,fPLColourHDR,fSetColour,@HDRPush[0],32,PixelWorkgroups,1,1);
  end else begin
   RecordDispatch(aCommandBuffer,fPipeColourHDR,fPLColourHDR,fSetColour,@HDRPush[0],32,PixelWorkgroups,1,1);
  end;
 end else begin
  ColourPush[0]:=fWidth;
  ColourPush[1]:=fHeight;
  ColourPush[2]:=ChromaShiftX;
  ColourPush[3]:=ChromaShiftY;
  ColourPush[4]:=PlaneWidth(1);
  ColourPush[5]:=PlaneHeight(1);
  RecordDispatch(aCommandBuffer,fPipeColour,fPLColour,fSetColour,@ColourPush[0],24,PixelWorkgroups,1,1);
 end;

 // Hand the decoded image to the transfer stage for present / readback.
 RecordImageBarrier(aCommandBuffer,
                    VK_IMAGE_LAYOUT_GENERAL,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                    TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT));

end;

function TpvFlexibleWaveletVideoDecoder.ActiveDataBuffer:TpvVulkanBuffer;
begin
 if fBufferRingSlot<0 then begin result:=fDataBuffer; end else begin result:=fRingDataBuffer[fBufferRingSlot]; end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveOffsetBuffer(const aPlane:TpvInt32):TpvVulkanBuffer;
begin
 if fBufferRingSlot<0 then begin result:=fOffsetBuffer[aPlane]; end else begin result:=fRingOffsetBuffer[fBufferRingSlot][aPlane]; end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveStepBuffer(const aPlane:TpvInt32):TpvVulkanBuffer;
begin
 if fBufferRingSlot<0 then begin result:=fStepBuffer[aPlane]; end else begin result:=fRingStepBuffer[fBufferRingSlot][aPlane]; end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveMVBuffer:TpvVulkanBuffer;
begin
 if fBufferRingSlot<0 then begin result:=fMVBuffer; end else begin result:=fRingMVBuffer[fBufferRingSlot]; end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveMV1Buffer:TpvVulkanBuffer;
begin
 if fBufferRingSlot<0 then begin result:=fMV1Buffer; end else begin result:=fRingMV1Buffer[fBufferRingSlot]; end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveModeBuffer:TpvVulkanBuffer;
begin
 if fBufferRingSlot<0 then begin result:=fModeBuffer; end else begin result:=fRingModeBuffer[fBufferRingSlot]; end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveSetUnpack(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
begin
 if fBufferRingSlot<0 then begin result:=fSetUnpack[aPlane]; end else begin result:=fRingSetUnpack[fBufferRingSlot][aPlane]; end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveSetDequant(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
begin
 if fBufferRingSlot<0 then begin result:=fSetDequant[aPlane]; end else begin result:=fRingSetDequant[fBufferRingSlot][aPlane]; end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveSetGMC0(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
begin
 if fBufferRingSlot<0 then begin result:=fSetGMC0[aPlane]; end else begin result:=fRingSetGMC0[fBufferRingSlot][aPlane]; end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveSetGMC1(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
begin
 if fBufferRingSlot<0 then begin result:=fSetGMC1[aPlane]; end else begin result:=fRingSetGMC1[fBufferRingSlot][aPlane]; end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveSetGBlend(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
begin
 if fBufferRingSlot<0 then begin result:=fSetGBlend[aPlane]; end else begin result:=fRingSetGBlend[fBufferRingSlot][aPlane]; end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveSetGBlendMode(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
begin
 if fBufferRingSlot<0 then begin result:=fSetGBlendMode[aPlane]; end else begin result:=fRingSetGBlendMode[fBufferRingSlot][aPlane]; end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveSetGAdd(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
begin
 if fBufferRingSlot<0 then begin result:=fSetGAdd[aPlane]; end else begin result:=fRingSetGAdd[fBufferRingSlot][aPlane]; end;
end;

// Mode B: allocate the per-frame input ring (data / offset / step / mv / mv1 / mode buffers + their sets) so the
// whole decode-ahead can be recorded into ONE caller command buffer. The unpack/dequant sets bind their ring
// inputs + the SHARED coeff once here; the gmc/blend/add sets are rebound per frame (DPB ref slots).
procedure TpvFlexibleWaveletVideoDecoder.BuildBidiRing;
var Slot,Plane:TpvInt32;
    DataCapacity,PlaneBytes:TVkDeviceSize;
    LumaBlockCount,MotionCells:TpvInt32;
begin
 LumaBlockCount:=BlockCountX(fWidth)*BlockCountY(fHeight);
 DataCapacity:=(TVkDeviceSize(fWidth)*TVkDeviceSize(fHeight)*4)+(TVkDeviceSize(LumaBlockCount)*16);
 MotionCells:=MotionBlocksX(fWidth)*MotionBlocksY(fHeight);
 SetLength(fBidiPlan,fBufferRingSize); // PrepareFrameBidi captures one entry per decode-ahead frame
 SetLength(fRingDataBuffer,fBufferRingSize);
 SetLength(fRingOffsetBuffer,fBufferRingSize);
 SetLength(fRingStepBuffer,fBufferRingSize);
 SetLength(fRingMVBuffer,fBufferRingSize);
 SetLength(fRingMV1Buffer,fBufferRingSize);
 SetLength(fRingModeBuffer,fBufferRingSize);
 SetLength(fRingSetUnpack,fBufferRingSize);
 SetLength(fRingSetDequant,fBufferRingSize);
 SetLength(fRingSetGMC0,fBufferRingSize);
 SetLength(fRingSetGMC1,fBufferRingSize);
 SetLength(fRingSetGBlend,fBufferRingSize);
 SetLength(fRingSetGBlendMode,fBufferRingSize);
 SetLength(fRingSetGAdd,fBufferRingSize);
 for Slot:=0 to fBufferRingSize-1 do begin
  fRingDataBuffer[Slot]:=CreateStorageBuffer(DataCapacity,false,'FWV.ring.data');
  fRingMVBuffer[Slot]:=CreateStorageBuffer(TVkDeviceSize(MotionCells)*2*4,false,'FWV.ring.mv');
  fRingMV1Buffer[Slot]:=CreateStorageBuffer(TVkDeviceSize(MotionCells)*2*4,false,'FWV.ring.mv1');
  fRingModeBuffer[Slot]:=CreateStorageBuffer(TVkDeviceSize(MotionCells)*4,false,'FWV.ring.mode');
  for Plane:=0 to 2 do begin
   PlaneBytes:=TVkDeviceSize(PlaneWidth(Plane))*TVkDeviceSize(PlaneHeight(Plane))*4;
   fRingOffsetBuffer[Slot][Plane]:=CreateStorageBuffer(TVkDeviceSize(LumaBlockCount)*4,false,'FWV.ring.offset');
   fRingStepBuffer[Slot][Plane]:=CreateStorageBuffer(PlaneBytes,false,'FWV.ring.step');
   // unpack set: {ring data, ring offset, shared coeff}; dequant set: {shared coeff, ring step}
   fRingSetUnpack[Slot][Plane]:=AllocateSet(fDSL3);
   BindStorageBuffer(fRingSetUnpack[Slot][Plane],0,fRingDataBuffer[Slot]);
   BindStorageBuffer(fRingSetUnpack[Slot][Plane],1,fRingOffsetBuffer[Slot][Plane]);
   BindStorageBuffer(fRingSetUnpack[Slot][Plane],2,fCoeffBuffer[Plane]);
   fRingSetUnpack[Slot][Plane].Flush;
   fRingSetDequant[Slot][Plane]:=AllocateSet(fDSL2);
   BindStorageBuffer(fRingSetDequant[Slot][Plane],0,fCoeffBuffer[Plane]);
   BindStorageBuffer(fRingSetDequant[Slot][Plane],1,fRingStepBuffer[Slot][Plane]);
   fRingSetDequant[Slot][Plane].Flush;
   fRingSetGMC0[Slot][Plane]:=AllocateSet(fDSL3);
   fRingSetGMC1[Slot][Plane]:=AllocateSet(fDSL3);
   fRingSetGBlend[Slot][Plane]:=AllocateSet(fDSL3);
   fRingSetGAdd[Slot][Plane]:=AllocateSet(fDSL3);
   if fHasPerBlockMode then begin
    fRingSetGBlendMode[Slot][Plane]:=AllocateSet(fDSL3);
   end;
  end;
 end;
end;

procedure TpvFlexibleWaveletVideoDecoder.UploadBidiFrame(const aCodingIndex:TpvInt32);
var Plane,PlanePixels,StepSlot:TpvInt32;
    Entry:PFrameEntry;
    CompressedLength:TpvSizeUInt;
    RawLength,DataLength:TpvUInt32;
    BlockCount:TBlockCounts;
    Offsets:TPlaneOffsets;
    LeadingBlockCount:TpvInt32;
    MVDataOffset,BlockDataOffset:TpvSizeUInt;
    MVLength:TpvUInt32;
    DataPointer:PpvUInt8Array;
    HasMode,HasMV1,IsPredicted:boolean;
    MotionBlockCountX,MotionBlockCountY,MVComponentCount,ModeCount,BlockIndex:TpvInt32;
    MVReader:TBitReader;
    MVRangeDecoder:TMVRangeDecoder;
begin

 Entry:=@fFrameEntries[aCodingIndex];

 // Read + decompress the coding frame (same framing as the non-B path).
 CompressedLength:=Entry^.Size;
 if TpvSizeUInt(Length(fCompressedScratch))<(CompressedLength+8) then begin
  SetLength(fCompressedScratch,CompressedLength+8);
 end;
 fStream.Position:=TpvInt64(Entry^.Offset);
 fStream.ReadBuffer(fCompressedScratch[0],CompressedLength);
 RawLength:=ReadU32LE(PpvUInt8Array(@fCompressedScratch[0]),1);
 if TpvSizeUInt(Length(fFrameScratch))<RawLength then begin
  SetLength(fFrameScratch,RawLength);
 end;
 if not DecompressFrame(PpvUInt8Array(@fCompressedScratch[0]),CompressedLength,PpvUInt8Array(@fFrameScratch[0]),TpvSizeUInt(Length(fFrameScratch)),RawLength) then begin
  raise EpvFlexibleWaveletVideoDecoder.Create('Frame decompression failed');
 end;

 // Lossy: upload the per-plane quant step maps at THIS frame's (temporal-id-cascaded) quality, cached per quality.
 if not fLossless then begin
  if not fGainsComputed then begin
   MeasureSynthesisGains(fLevels,fHFGain,fLLGain);
   fGainsComputed:=true;
  end;
  StepSlot:=EnsureStepCacheSlot(Entry^.Quality);
  for Plane:=0 to 2 do begin
   PlanePixels:=PlaneWidth(Plane)*PlaneHeight(Plane);
   DataPointer:=PpvUInt8Array(ActiveStepBuffer(Plane).Memory.MapMemory);
   try
    Move(fStepCacheData[(StepSlot*3)+Plane][0],DataPointer^[0],TpvSizeUInt(PlanePixels)*4);
   finally
    ActiveStepBuffer(Plane).Memory.UnmapMemory;
   end;
  end;
 end;

 // Offsets + data upload.
 for Plane:=0 to 2 do begin
  BlockCount[Plane]:=BlockCountX(PlaneWidth(Plane))*BlockCountY(PlaneHeight(Plane));
  if TpvSizeUInt(Length(fOffsetScratch[Plane]))<TpvSizeUInt(BlockCount[Plane]) then begin
   SetLength(fOffsetScratch[Plane],BlockCount[Plane]);
  end;
  Offsets[Plane]:=PpvUInt32Array(@fOffsetScratch[Plane][0]);
 end;
 ParseFrameHeader(PpvUInt8Array(@fFrameScratch[0]),BlockCount,Offsets,LeadingBlockCount,MVDataOffset,MVLength,BlockDataOffset);
 for Plane:=0 to 2 do begin
  DataPointer:=PpvUInt8Array(ActiveOffsetBuffer(Plane).Memory.MapMemory);
  try
   Move(fOffsetScratch[Plane][0],DataPointer^[0],TpvSizeUInt(BlockCount[Plane])*4);
  finally
   ActiveOffsetBuffer(Plane).Memory.UnmapMemory;
  end;
 end;
 DataLength:=ReadU32LE(PpvUInt8Array(@fFrameScratch[0]),BlockDataOffset-4);
 DataPointer:=PpvUInt8Array(ActiveDataBuffer.Memory.MapMemory);
 try
  Move(fFrameScratch[BlockDataOffset],DataPointer^[0],DataLength);
 finally
  ActiveDataBuffer.Memory.UnmapMemory;
 end;

 // B-frame motion: the blob carries the optional per-block mode, then L0 MVs, then (for a B-frame) L1 MVs.
 IsPredicted:=Entry^.Ref0>=0;
 HasMV1:=Entry^.Ref1>=0;
 HasMode:=fHasPerBlockMode and (Entry^.Ref1>=0);
 if IsPredicted and (MVLength>0) then begin
  MotionBlockCountX:=MotionBlocksX(fWidth);
  MotionBlockCountY:=MotionBlocksY(fHeight);
  MVComponentCount:=(MotionBlockCountX*MotionBlockCountY)*2;
  ModeCount:=MotionBlockCountX*MotionBlockCountY;
  if TpvSizeUInt(Length(fMVScratch))<TpvSizeUInt(MVComponentCount) then begin
   SetLength(fMVScratch,MVComponentCount);
  end;
  if TpvSizeUInt(Length(fMV1Scratch))<TpvSizeUInt(MVComponentCount) then begin
   SetLength(fMV1Scratch,MVComponentCount);
  end;
  if TpvSizeUInt(Length(fModeScratch))<TpvSizeUInt(ModeCount) then begin
   SetLength(fModeScratch,ModeCount);
  end;
  if fMotionVariable then begin // quadtrees fill via leaf-expansion -> clear first (partial edges)
   if HasMode then begin
    FillChar(fModeScratch[0],TpvSizeUInt(ModeCount)*4,#0);
   end;
   FillChar(fMVScratch[0],TpvSizeUInt(MVComponentCount)*4,#0);
   if HasMV1 then begin
    FillChar(fMV1Scratch[0],TpvSizeUInt(MVComponentCount)*4,#0);
   end;
  end;
  if fMVCodec=1 then begin // range codec: one stream covering [mode] -> mv0 -> [mv1]
   MVRangeDecoder.Init(PpvUInt8Array(@fFrameScratch[MVDataOffset]),MVLength);
   if HasMode then begin
    if fMotionVariable then begin
     DecodeModeQuadtreeRange(MVRangeDecoder,PpvInt32Array(@fModeScratch[0]),MotionBlockCountX,MotionBlockCountY);
    end else begin
     for BlockIndex:=0 to ModeCount-1 do begin
      fModeScratch[BlockIndex]:=MVRangeDecoder.DecodeMode;
     end;
    end;
   end;
   if fMotionVariable then begin
    DecodeMotionQuadtreeRange(MVRangeDecoder,PpvInt32Array(@fMVScratch[0]),MotionBlockCountX,MotionBlockCountY);
   end else begin
    DecodeMotionVectorsRange(MVRangeDecoder,PpvInt32Array(@fMVScratch[0]),MotionBlockCountX,MotionBlockCountY);
   end;
   if HasMV1 then begin
    if fMotionVariable then begin
     DecodeMotionQuadtreeRange(MVRangeDecoder,PpvInt32Array(@fMV1Scratch[0]),MotionBlockCountX,MotionBlockCountY);
    end else begin
     DecodeMotionVectorsRange(MVRangeDecoder,PpvInt32Array(@fMV1Scratch[0]),MotionBlockCountX,MotionBlockCountY);
    end;
   end;
  end else begin // signed Exp-Golomb: [mode] -> mv0 -> [mv1]
   MVReader.Init(PpvUInt8Array(@fFrameScratch[MVDataOffset]),MVLength);
   if HasMode then begin
    if fMotionVariable then begin
     DecodeModeQuadtree(MVReader,PpvInt32Array(@fModeScratch[0]),MotionBlockCountX,MotionBlockCountY);
    end else begin
     for BlockIndex:=0 to ModeCount-1 do begin
      fModeScratch[BlockIndex]:=TpvInt32(MVReader.GetBits(2)); // 2-bit mode (0=L0, 1=L1, 2=BI)
     end;
    end;
   end;
   if fMotionVariable then begin
    DecodeMotionQuadtree(MVReader,PpvInt32Array(@fMVScratch[0]),MotionBlockCountX,MotionBlockCountY);
   end else begin
    DecodeMotionVectors(MVReader,PpvInt32Array(@fMVScratch[0]),MotionBlockCountX,MotionBlockCountY);
   end;
   if HasMV1 then begin
    if fMotionVariable then begin
     DecodeMotionQuadtree(MVReader,PpvInt32Array(@fMV1Scratch[0]),MotionBlockCountX,MotionBlockCountY);
    end else begin
     DecodeMotionVectors(MVReader,PpvInt32Array(@fMV1Scratch[0]),MotionBlockCountX,MotionBlockCountY);
    end;
   end;
  end;
  DataPointer:=PpvUInt8Array(ActiveMVBuffer.Memory.MapMemory);
  try
   Move(fMVScratch[0],DataPointer^[0],TpvSizeUInt(MVComponentCount)*4);
  finally
   ActiveMVBuffer.Memory.UnmapMemory;
  end;
  if HasMV1 then begin
   DataPointer:=PpvUInt8Array(ActiveMV1Buffer.Memory.MapMemory);
   try
    Move(fMV1Scratch[0],DataPointer^[0],TpvSizeUInt(MVComponentCount)*4);
   finally
    ActiveMV1Buffer.Memory.UnmapMemory;
   end;
  end;
  if HasMode then begin
   DataPointer:=PpvUInt8Array(ActiveModeBuffer.Memory.MapMemory);
   try
    Move(fModeScratch[0],DataPointer^[0],TpvSizeUInt(ModeCount)*4);
   finally
    ActiveModeBuffer.Memory.UnmapMemory;
   end;
  end;
 end;

end;

procedure TpvFlexibleWaveletVideoDecoder.RecordBidiDecode(const aCommandBuffer:TpvVulkanCommandBuffer;const aIsPredicted,aRef1Slot,aWeight0,aWeight1:TpvInt32);
var Plane,Level,LevelCount:TpvInt32;
    PlaneW,PlaneH,ScratchStride,PlanePixels:TpvInt32;
    PlaneBlocksX,PlaneBlocksY,PlaneBlockCount:TpvInt32;
    PlanePixelWorkgroups,PlaneUnpackWorkgroups,PlaneMotionBlocksX:TpvInt32;
    CurrentWidth,CurrentHeight,LevelW,LevelH:TpvInt32;
    LevelWidth,LevelHeight:array[0..15] of TpvInt32;
    RowPipeline:TpvVulkanComputePipeline;
    UnpackPush:array[0..3] of TpvInt32;
    DequantPush:array[0..1] of TpvInt32;
    TransposePush1,TransposePush2,RowPush1,RowPush2:array[0..3] of TpvInt32;
    PixelCountPush:TpvInt32;
    ChromaMultiplier:TpvFloat;
    MCPush:array[0..2] of TpvInt32;
    AddPush:array[0..1] of TpvInt32;
    BlendPush:array[0..2] of TpvInt32;
    ModeBlendPush:array[0..4] of TpvInt32;
begin

 if fLossless then begin
  RowPipeline:=fPipeIDWT53;
 end else begin
  RowPipeline:=fPipeIDWT97;
 end;

 for Plane:=0 to 2 do begin

  PlaneW:=PlaneWidth(Plane);
  PlaneH:=PlaneHeight(Plane);
  if PlaneW>PlaneH then begin
   ScratchStride:=PlaneW;
  end else begin
   ScratchStride:=PlaneH;
  end;
  PlanePixels:=PlaneW*PlaneH;
  PlaneBlocksX:=BlockCountX(PlaneW);
  PlaneBlocksY:=BlockCountY(PlaneH);
  PlaneBlockCount:=PlaneBlocksX*PlaneBlocksY;
  PlanePixelWorkgroups:=(PlanePixels+255) div 256;
  if fBlockSize=128 then begin
   PlaneUnpackWorkgroups:=PlaneBlockCount;
  end else begin
   PlaneUnpackWorkgroups:=(PlaneBlockCount+63) div 64;
  end;

  // residual: unpack -> [dequant] -> iDWT -> [round] (same as the non-B path, no coeff_add on the B/colordiff path).
  // The unpack/dequant SETS come from Active* (the per-frame ring slot for mode B, the shared set for modes A/C).
  UnpackPush[0]:=PlaneW;
  UnpackPush[1]:=PlaneH;
  UnpackPush[2]:=PlaneBlocksX;
  UnpackPush[3]:=PlaneBlocksY;
  RecordDispatch(aCommandBuffer,fPipeUnpack,fPLUnpack,ActiveSetUnpack(Plane),@UnpackPush[0],16,PlaneUnpackWorkgroups,1,1);
  RecordComputeBarrier(aCommandBuffer);

  if not fLossless then begin
   DequantPush[0]:=PlanePixels;
   if Plane=0 then begin
    ChromaMultiplier:=1.0;
   end else begin
    ChromaMultiplier:=fChromaQuant;
   end;
   DequantPush[1]:=PpvInt32(@ChromaMultiplier)^;
   RecordDispatch(aCommandBuffer,fPipeDequant,fPLDequant,ActiveSetDequant(Plane),@DequantPush[0],8,PlanePixelWorkgroups,1,1);
   RecordComputeBarrier(aCommandBuffer);
  end;

  LevelCount:=0;
  CurrentWidth:=PlaneW;
  CurrentHeight:=PlaneH;
  Level:=0;
  while ((Level<fLevels) and (CurrentWidth>=2)) and (CurrentHeight>=2) do begin
   LevelWidth[LevelCount]:=CurrentWidth;
   LevelHeight[LevelCount]:=CurrentHeight;
   inc(LevelCount);
   CurrentWidth:=(CurrentWidth+1) div 2;
   CurrentHeight:=(CurrentHeight+1) div 2;
   inc(Level);
  end;
  for Level:=LevelCount-1 downto 0 do begin
   LevelW:=LevelWidth[Level];
   LevelH:=LevelHeight[Level];
   TransposePush1[0]:=PlaneW;
   TransposePush1[1]:=LevelW;
   TransposePush1[2]:=LevelH;
   TransposePush1[3]:=ScratchStride;
   RecordDispatch(aCommandBuffer,fPipeTranspose,fPLTranspose,fSetCoeffToScratch[Plane],@TransposePush1[0],16,(LevelW+15) div 16,(LevelH+15) div 16,1);
   RecordComputeBarrier(aCommandBuffer);
   RowPush1[0]:=ScratchStride;
   RowPush1[1]:=LevelH;
   RowPush1[2]:=LevelW;
   RowPush1[3]:=1;
   RecordDispatch(aCommandBuffer,RowPipeline,fPLRow,fSetRowScratch,@RowPush1[0],16,LevelW,1,1);
   RecordComputeBarrier(aCommandBuffer);
   TransposePush2[0]:=ScratchStride;
   TransposePush2[1]:=LevelH;
   TransposePush2[2]:=LevelW;
   TransposePush2[3]:=PlaneW;
   RecordDispatch(aCommandBuffer,fPipeTranspose,fPLTranspose,fSetScratchToCoeff[Plane],@TransposePush2[0],16,(LevelH+15) div 16,(LevelW+15) div 16,1);
   RecordComputeBarrier(aCommandBuffer);
   RowPush2[0]:=PlaneW;
   RowPush2[1]:=LevelW;
   RowPush2[2]:=LevelH;
   RowPush2[3]:=1;
   RecordDispatch(aCommandBuffer,RowPipeline,fPLRow,fSetRow[Plane],@RowPush2[0],16,LevelH,1,1);
   RecordComputeBarrier(aCommandBuffer);
  end;

  if not fLossless then begin
   PixelCountPush:=PlanePixels;
   RecordDispatch(aCommandBuffer,fPipeRound,fPLRound,fSetRow[Plane],@PixelCountPush,4,PlanePixelWorkgroups,1,1);
   RecordComputeBarrier(aCommandBuffer);
  end;

  // bidirectional prediction: mc(L0) [+ mc(L1)] -> blend -> motion_add -> dpb[dst]. The sets are pre-bound by
  // DecodeFrameBidi. For a P-anchor (ref1<0) only mc0 runs and the bidi blend (w0=256, w1=0) reduces to gmc0.
  if aIsPredicted<>0 then begin
   PlaneMotionBlocksX:=MotionBlocksX(PlaneW);
   MCPush[0]:=PlaneW;
   MCPush[1]:=PlaneH;
   MCPush[2]:=PlaneMotionBlocksX;
   RecordDispatch(aCommandBuffer,fPipeMC,fPLUnpack,ActiveSetGMC0(Plane),@MCPush[0],12,PlanePixelWorkgroups,1,1);
   RecordComputeBarrier(aCommandBuffer);
   if aRef1Slot>=0 then begin
    RecordDispatch(aCommandBuffer,fPipeMC,fPLUnpack,ActiveSetGMC1(Plane),@MCPush[0],12,PlanePixelWorkgroups,1,1);
    RecordComputeBarrier(aCommandBuffer);
   end;
   if fHasPerBlockMode and (aRef1Slot>=0) then begin // Phase 2: per-block L0/L1/BI mode into gmc0 in place
    ModeBlendPush[0]:=PlaneW;
    ModeBlendPush[1]:=PlaneH;
    ModeBlendPush[2]:=PlaneMotionBlocksX;
    ModeBlendPush[3]:=aWeight0;
    ModeBlendPush[4]:=aWeight1;
    RecordDispatch(aCommandBuffer,fPipeBlendMode,fPLBlendMode,ActiveSetGBlendMode(Plane),@ModeBlendPush[0],20,PlanePixelWorkgroups,1,1);
    RecordComputeBarrier(aCommandBuffer);
   end else begin // uniform weighted BI blend into scratch (P-anchor: w1=0 -> = gmc0)
    BlendPush[0]:=PlanePixels;
    BlendPush[1]:=aWeight0;
    BlendPush[2]:=aWeight1;
    RecordDispatch(aCommandBuffer,fPipeBidiBlend,fPLUnpack,ActiveSetGBlend(Plane),@BlendPush[0],12,PlanePixelWorkgroups,1,1);
    RecordComputeBarrier(aCommandBuffer);
   end;
  end;

  AddPush[0]:=PlanePixels;
  AddPush[1]:=aIsPredicted;
  RecordDispatch(aCommandBuffer,fPipeMotionAdd,fPLUnpack,ActiveSetGAdd(Plane),@AddPush[0],8,PlanePixelWorkgroups,1,1);
  RecordComputeBarrier(aCommandBuffer);

 end;

end;

function TpvFlexibleWaveletVideoDecoder.PrepareBidiFrame(const aDisplayPOC:TpvInt32;out aIsPredicted,aRef1Slot,aWeight0,aWeight1:TpvInt32):boolean;
var CodingIndex,SlotIndex,Occupant,DstSlot,Ref0Slot,IsPhase2,BlendR1,Plane:TpvInt32;
    POCSelf,POC0,POC1:TpvInt32;
    Entry:PFrameEntry;
begin

 CodingIndex:=fGCursor;
 Entry:=@fFrameEntries[CodingIndex];

 UploadBidiFrame(CodingIndex); // uses the Active* buffers (the per-frame ring slot for mode B, set by the caller)

 // evict slots no longer referenced (last_use passed) AND already displayed (poc < this display POC)
 for SlotIndex:=0 to fGDPBSlots-1 do begin
  Occupant:=fGDPBSlotCoding[SlotIndex];
  if ((Occupant>=0) and (fGDPBLastUse[Occupant]<CodingIndex)) and (fFrameEntries[Occupant].POC<TpvUInt32(aDisplayPOC)) then begin
   fGDPBCodingToSlot[Occupant]:=-1;
   fGDPBPOCToSlot[fFrameEntries[Occupant].POC]:=-1;
   fGDPBSlotCoding[SlotIndex]:=-1;
  end;
 end;

 DstSlot:=-1;
 for SlotIndex:=0 to fGDPBSlots-1 do begin
  if fGDPBSlotCoding[SlotIndex]<0 then begin
   DstSlot:=SlotIndex;
   break;
  end;
 end;
 if DstSlot<0 then begin
  if fGDPBPOCToSlot[aDisplayPOC]>=0 then begin
   result:=false; // pool full but the display frame is ready -> stop the decode-ahead
   exit;
  end;
  raise EpvFlexibleWaveletVideoDecoder.Create('B-frame DPB pool exhausted');
 end;

 if Entry^.Ref0>=0 then begin
  Ref0Slot:=fGDPBCodingToSlot[Entry^.Ref0];
 end else begin
  Ref0Slot:=-1;
 end;
 if Entry^.Ref1>=0 then begin
  aRef1Slot:=fGDPBCodingToSlot[Entry^.Ref1];
 end else begin
  aRef1Slot:=-1;
 end;
 aIsPredicted:=Ord(Entry^.Ref0>=0);
 aWeight0:=0;
 aWeight1:=0;
 if (Ref0Slot>=0) and (aRef1Slot>=0) then begin
  POCSelf:=TpvInt32(Entry^.POC);
  POC0:=TpvInt32(fFrameEntries[Entry^.Ref0].POC);
  POC1:=TpvInt32(fFrameEntries[Entry^.Ref1].POC);
  aWeight0:=(256*(POC1-POCSelf)) div (POC1-POC0);
  aWeight1:=256-aWeight0;
 end else if Ref0Slot>=0 then begin
  aWeight0:=256;
 end;
 IsPhase2:=Ord(fHasPerBlockMode and (aRef1Slot>=0));

 // rebind the dynamic B sets (Active* = the ring slot's sets for mode B) to this frame's DPB ref slots + target slot
 if aIsPredicted<>0 then begin
  for Plane:=0 to 2 do begin
   BindStorageBuffer(ActiveSetGMC0(Plane),0,fDPBBuffer[Ref0Slot][Plane]);
   BindStorageBuffer(ActiveSetGMC0(Plane),1,ActiveMVBuffer);
   BindStorageBuffer(ActiveSetGMC0(Plane),2,fGMCBuffer[0][Plane]);
   ActiveSetGMC0(Plane).Flush;
   if aRef1Slot>=0 then begin
    BindStorageBuffer(ActiveSetGMC1(Plane),0,fDPBBuffer[aRef1Slot][Plane]);
    BindStorageBuffer(ActiveSetGMC1(Plane),1,ActiveMV1Buffer);
    BindStorageBuffer(ActiveSetGMC1(Plane),2,fGMCBuffer[1][Plane]);
    ActiveSetGMC1(Plane).Flush;
   end;
   if IsPhase2<>0 then begin
    BindStorageBuffer(ActiveSetGBlendMode(Plane),0,fGMCBuffer[0][Plane]);
    BindStorageBuffer(ActiveSetGBlendMode(Plane),1,fGMCBuffer[1][Plane]);
    BindStorageBuffer(ActiveSetGBlendMode(Plane),2,ActiveModeBuffer);
    ActiveSetGBlendMode(Plane).Flush;
   end else begin
    if aRef1Slot>=0 then begin
     BlendR1:=1;
    end else begin
     BlendR1:=0; // P-anchor: blend gmc0 with itself (w1=0)
    end;
    BindStorageBuffer(ActiveSetGBlend(Plane),0,fGMCBuffer[0][Plane]);
    BindStorageBuffer(ActiveSetGBlend(Plane),1,fGMCBuffer[BlendR1][Plane]);
    BindStorageBuffer(ActiveSetGBlend(Plane),2,fScratchBuffer);
    ActiveSetGBlend(Plane).Flush;
   end;
  end;
 end;
 for Plane:=0 to 2 do begin
  BindStorageBuffer(ActiveSetGAdd(Plane),0,fCoeffBuffer[Plane]);
  if IsPhase2<>0 then begin
   BindStorageBuffer(ActiveSetGAdd(Plane),1,fGMCBuffer[0][Plane]);
  end else begin
   BindStorageBuffer(ActiveSetGAdd(Plane),1,fScratchBuffer);
  end;
  BindStorageBuffer(ActiveSetGAdd(Plane),2,fDPBBuffer[DstSlot][Plane]);
  ActiveSetGAdd(Plane).Flush;
 end;

 // register this frame in the DPB (CPU bookkeeping; the GPU writes dpb[dst] on the upcoming submit)
 fGDPBSlotCoding[DstSlot]:=CodingIndex;
 fGDPBCodingToSlot[CodingIndex]:=DstSlot;
 fGDPBPOCToSlot[TpvInt32(Entry^.POC)]:=DstSlot;
 inc(fGCursor);
 result:=true;
end;

procedure TpvFlexibleWaveletVideoDecoder.RecordBidiDisplay(const aCommandBuffer:TpvVulkanCommandBuffer;const aDisplayPOC:TpvInt32);
var Plane,PlanePixels,DisplaySlot,PixelWorkgroups:TpvInt32;
    BufferCopy:TVkBufferCopy;
    Barrier:TVkMemoryBarrier;
    ColourPush:array[0..5] of TpvInt32;
    HDRPush:array[0..7] of TpvInt32;
    ExposureBits:TpvFloat;
begin

 // copy the display POC's reconstructed YCoCg DPB slot into coeff, then colour-convert into the output image
 DisplaySlot:=fGDPBPOCToSlot[aDisplayPOC];

 RecordImageBarrier(aCommandBuffer,
                    VK_IMAGE_LAYOUT_UNDEFINED,VK_IMAGE_LAYOUT_GENERAL,
                    0,TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT));

 for Plane:=0 to 2 do begin
  PlanePixels:=PlaneWidth(Plane)*PlaneHeight(Plane);
  FillChar(BufferCopy,SizeOf(BufferCopy),#0);
  BufferCopy.srcOffset:=0;
  BufferCopy.dstOffset:=0;
  BufferCopy.size:=TVkDeviceSize(PlanePixels)*4;
  aCommandBuffer.CmdCopyBuffer(fDPBBuffer[DisplaySlot][Plane].Handle,fCoeffBuffer[Plane].Handle,1,@BufferCopy);
 end;
 FillChar(Barrier,SizeOf(Barrier),#0);
 Barrier.sType:=VK_STRUCTURE_TYPE_MEMORY_BARRIER;
 Barrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT);
 Barrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,1,@Barrier,0,nil,0,nil);

 PixelWorkgroups:=((fWidth*fHeight)+255) div 256;
 if fIsHDR then begin
  HDRPush[0]:=fWidth;
  HDRPush[1]:=fHeight;
  ExposureBits:=fHDRExposure;
  HDRPush[2]:=PpvInt32(@ExposureBits)^;
  HDRPush[3]:=fTransferFunction;
  HDRPush[4]:=ChromaShiftX;
  HDRPush[5]:=ChromaShiftY;
  HDRPush[6]:=PlaneWidth(1);
  HDRPush[7]:=PlaneHeight(1);
  if fUseSCRGB then begin
   RecordDispatch(aCommandBuffer,fPipeColourHDRSCRGB,fPLColourHDR,fSetColour,@HDRPush[0],32,PixelWorkgroups,1,1);
  end else begin
   RecordDispatch(aCommandBuffer,fPipeColourHDR,fPLColourHDR,fSetColour,@HDRPush[0],32,PixelWorkgroups,1,1);
  end;
 end else begin
  ColourPush[0]:=fWidth;
  ColourPush[1]:=fHeight;
  ColourPush[2]:=ChromaShiftX;
  ColourPush[3]:=ChromaShiftY;
  ColourPush[4]:=PlaneWidth(1);
  ColourPush[5]:=PlaneHeight(1);
  RecordDispatch(aCommandBuffer,fPipeColour,fPLColour,fSetColour,@ColourPush[0],24,PixelWorkgroups,1,1);
 end;

 RecordImageBarrier(aCommandBuffer,
                    VK_IMAGE_LAYOUT_GENERAL,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                    TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT));

end;

procedure TpvFlexibleWaveletVideoDecoder.DecodeFrameBidi(const aCommandBuffer:TpvVulkanCommandBuffer;const aDisplayPOC:TpvInt32);
var IsPredicted,Ref1Slot,Weight0,Weight1,RingIndex:TpvInt32;
begin

 // decode-ahead: reconstruct coding-order frames into the DPB until this display POC is ready + the lead is full
 RingIndex:=0;
 while fGCursor<fFrameCount do begin
  if (fGDPBPOCToSlot[aDisplayPOC]>=0) and (fGCursor>=(aDisplayPOC+fGDecodeLead)) then begin
   break;
  end;
  if fSubmitMode=1 then begin // mode B: this coding frame uses the next per-frame ring slot
   if RingIndex>=fBufferRingSize then begin
    raise EpvFlexibleWaveletVideoDecoder.Create('B-frame mode B input ring overflow');
   end;
   fBufferRingSlot:=RingIndex;
  end else begin
   fBufferRingSlot:=-1;
  end;
  if not PrepareBidiFrame(aDisplayPOC,IsPredicted,Ref1Slot,Weight0,Weight1) then begin
   break;
  end;
  if fSubmitMode=0 then begin // mode A: self-submit + wait on the internal command buffer (shared input buffers)
   fBDecodeCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));
   RecordBidiDecode(fBDecodeCommandBuffer,IsPredicted,Ref1Slot,Weight0,Weight1);
   fBDecodeCommandBuffer.EndRecording;
   fBDecodeCommandBuffer.Execute(fDevice.UniversalQueue,TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),nil,nil,fBDecodeFence,true);
  end else begin // mode B: record into the caller's command buffer (submitted once with the display below)
   RecordBidiDecode(aCommandBuffer,IsPredicted,Ref1Slot,Weight0,Weight1);
   inc(RingIndex);
  end;
 end;
 fBufferRingSlot:=-1;

 RecordBidiDisplay(aCommandBuffer,aDisplayPOC);

end;

function TpvFlexibleWaveletVideoDecoder.DecodeFrameStep(const aCommandBuffer:TpvVulkanCommandBuffer;const aDisplayPOC:TpvInt32):boolean;
var IsPredicted,Ref1Slot,Weight0,Weight1:TpvInt32;
begin
 // mode C: record ONE decode-ahead coding frame into the caller's command buffer (the caller submits + WAITS,
 // then calls again, reusing the shared input buffers), or record the display when the frame is ready -> False.
 fBufferRingSlot:=-1;
 if (fGCursor<fFrameCount) and not ((fGDPBPOCToSlot[aDisplayPOC]>=0) and (fGCursor>=(aDisplayPOC+fGDecodeLead))) then begin
  if PrepareBidiFrame(aDisplayPOC,IsPredicted,Ref1Slot,Weight0,Weight1) then begin
   RecordBidiDecode(aCommandBuffer,IsPredicted,Ref1Slot,Weight0,Weight1);
   result:=true;
   exit;
  end;
 end;
 RecordBidiDisplay(aCommandBuffer,aDisplayPOC);
 result:=false;
end;

function TpvFlexibleWaveletVideoDecoder.GopCountFrom(const aStart:TpvInt32):TpvInt32;
begin
 // a 3D-DWT GOP starts at a type-0 subband frame and runs while the following frames are type != 0
 result:=1;
 while ((aStart+result)<fFrameCount) and (fFrameEntries[aStart+result].FrameType<>0) do begin
  inc(result);
 end;
end;

procedure TpvFlexibleWaveletVideoDecoder.Upload3DFrame(const aCodingIndex,aSlot,aGOPCount:TpvInt32);
var Plane,PlanePixels,TemporalLevel,EffectiveQuality,StepSlot:TpvInt32;
    Entry:PFrameEntry;
    CompressedLength:TpvSizeUInt;
    RawLength,DataLength:TpvUInt32;
    BlockCount:TBlockCounts;
    Offsets:TPlaneOffsets;
    LeadingBlockCount:TpvInt32;
    MVDataOffset,BlockDataOffset:TpvSizeUInt;
    MVLength:TpvUInt32;
    DataPointer:PpvUInt8Array;
    QValue:TpvFloat;
    MotionBlockCountX,MotionBlockCountY,LumaBlocks:TpvInt32;
    MVReader:TBitReader;
    MVRangeDecoder:TMVRangeDecoder;
begin

 Entry:=@fFrameEntries[aCodingIndex];

 CompressedLength:=Entry^.Size;
 if TpvSizeUInt(Length(fCompressedScratch))<(CompressedLength+8) then begin
  SetLength(fCompressedScratch,CompressedLength+8);
 end;
 fStream.Position:=TpvInt64(Entry^.Offset);
 fStream.ReadBuffer(fCompressedScratch[0],CompressedLength);
 RawLength:=ReadU32LE(PpvUInt8Array(@fCompressedScratch[0]),1);
 if TpvSizeUInt(Length(fFrameScratch))<RawLength then begin
  SetLength(fFrameScratch,RawLength);
 end;
 if not DecompressFrame(PpvUInt8Array(@fCompressedScratch[0]),CompressedLength,PpvUInt8Array(@fFrameScratch[0]),TpvSizeUInt(Length(fFrameScratch)),RawLength) then begin
  raise EpvFlexibleWaveletVideoDecoder.Create('Frame decompression failed');
 end;

 // Lossy: the temporal subband's quant is scaled by its temporal level (coarser for higher temporal frequencies).
 if not fLossless then begin
  if not fGainsComputed then begin
   MeasureSynthesisGains(fLevels,fHFGain,fLLGain);
   fGainsComputed:=true;
  end;
  TemporalLevel:=TemporalQuantLevel(aSlot,aGOPCount,fTemporalLevels);
  QValue:=(TpvFloat(Entry^.Quality)*TemporalQuantScale(TemporalLevel))+0.5; // single store -> rounds like C's +0.5f
  EffectiveQuality:=Trunc(QValue);
  if EffectiveQuality<1 then begin
   EffectiveQuality:=1;
  end;
  StepSlot:=EnsureStepCacheSlot(EffectiveQuality);
  for Plane:=0 to 2 do begin
   PlanePixels:=PlaneWidth(Plane)*PlaneHeight(Plane);
   DataPointer:=PpvUInt8Array(fStepBuffer[Plane].Memory.MapMemory);
   try
    Move(fStepCacheData[(StepSlot*3)+Plane][0],DataPointer^[0],TpvSizeUInt(PlanePixels)*4);
   finally
    fStepBuffer[Plane].Memory.UnmapMemory;
   end;
  end;
 end;

 // offsets + data upload
 for Plane:=0 to 2 do begin
  BlockCount[Plane]:=BlockCountX(PlaneWidth(Plane))*BlockCountY(PlaneHeight(Plane));
  if TpvSizeUInt(Length(fOffsetScratch[Plane]))<TpvSizeUInt(BlockCount[Plane]) then begin
   SetLength(fOffsetScratch[Plane],BlockCount[Plane]);
  end;
  Offsets[Plane]:=PpvUInt32Array(@fOffsetScratch[Plane][0]);
 end;
 ParseFrameHeader(PpvUInt8Array(@fFrameScratch[0]),BlockCount,Offsets,LeadingBlockCount,MVDataOffset,MVLength,BlockDataOffset);
 for Plane:=0 to 2 do begin
  DataPointer:=PpvUInt8Array(fOffsetBuffer[Plane].Memory.MapMemory);
  try
   Move(fOffsetScratch[Plane][0],DataPointer^[0],TpvSizeUInt(BlockCount[Plane])*4);
  finally
   fOffsetBuffer[Plane].Memory.UnmapMemory;
  end;
 end;
 DataLength:=ReadU32LE(PpvUInt8Array(@fFrameScratch[0]),BlockDataOffset-4);
 DataPointer:=PpvUInt8Array(fDataBuffer.Memory.MapMemory);
 try
  Move(fFrameScratch[BlockDataOffset],DataPointer^[0],DataLength);
 finally
  fDataBuffer.Memory.UnmapMemory;
 end;

 // MCTF: a high-pass subband frame carries the luma MV field for its MC-Haar temporal pair. Decode it into this
 // GOP slot's MV store (used per pair by the temporal inverse). Deepest temporal-low frames have no MVs.
 if fMCTF and (MVLength>0) then begin
  MotionBlockCountX:=MotionBlocksX(fWidth);
  MotionBlockCountY:=MotionBlocksY(fHeight);
  LumaBlocks:=MotionBlockCountX*MotionBlockCountY;
  if fMVCodec=1 then begin
   MVRangeDecoder.Init(PpvUInt8Array(@fFrameScratch[MVDataOffset]),MVLength);
   DecodeMotionVectorsRange(MVRangeDecoder,PpvInt32Array(@fMCTFMVScratch[aSlot*LumaBlocks*2]),MotionBlockCountX,MotionBlockCountY);
  end else begin
   MVReader.Init(PpvUInt8Array(@fFrameScratch[MVDataOffset]),MVLength);
   DecodeMotionVectors(MVReader,PpvInt32Array(@fMCTFMVScratch[aSlot*LumaBlocks*2]),MotionBlockCountX,MotionBlockCountY);
  end;
 end;

end;

procedure TpvFlexibleWaveletVideoDecoder.RecordSpatial3D(const aCommandBuffer:TpvVulkanCommandBuffer;const aSlot:TpvInt32);
var Plane,Level,LevelCount:TpvInt32;
    PlaneW,PlaneH,ScratchStride,PlanePixels:TpvInt32;
    PlaneBlocksX,PlaneBlocksY,PlaneBlockCount:TpvInt32;
    PlanePixelWorkgroups,PlaneUnpackWorkgroups:TpvInt32;
    CurrentWidth,CurrentHeight,LevelW,LevelH:TpvInt32;
    LevelWidth,LevelHeight:array[0..15] of TpvInt32;
    RowPipeline:TpvVulkanComputePipeline;
    UnpackPush:array[0..3] of TpvInt32;
    DequantPush:array[0..1] of TpvInt32;
    TransposePush1,TransposePush2,RowPush1,RowPush2:array[0..3] of TpvInt32;
    PixelCountPush:TpvInt32;
    ChromaMultiplier:TpvFloat;
    Barrier:TVkMemoryBarrier;
    BufferCopy:TVkBufferCopy;
begin

 if fLossless then begin
  RowPipeline:=fPipeIDWT53;
 end else begin
  RowPipeline:=fPipeIDWT97;
 end;

 for Plane:=0 to 2 do begin

  PlaneW:=PlaneWidth(Plane);
  PlaneH:=PlaneHeight(Plane);
  if PlaneW>PlaneH then begin
   ScratchStride:=PlaneW;
  end else begin
   ScratchStride:=PlaneH;
  end;
  PlanePixels:=PlaneW*PlaneH;
  PlaneBlocksX:=BlockCountX(PlaneW);
  PlaneBlocksY:=BlockCountY(PlaneH);
  PlaneBlockCount:=PlaneBlocksX*PlaneBlocksY;
  PlanePixelWorkgroups:=(PlanePixels+255) div 256;
  if fBlockSize=128 then begin
   PlaneUnpackWorkgroups:=PlaneBlockCount;
  end else begin
   PlaneUnpackWorkgroups:=(PlaneBlockCount+63) div 64;
  end;

  // spatial inverse: unpack -> [dequant] -> iDWT (same as intra), reusing the shared sets that bind fCoeffBuffer
  UnpackPush[0]:=PlaneW;
  UnpackPush[1]:=PlaneH;
  UnpackPush[2]:=PlaneBlocksX;
  UnpackPush[3]:=PlaneBlocksY;
  RecordDispatch(aCommandBuffer,fPipeUnpack,fPLUnpack,fSetUnpack[Plane],@UnpackPush[0],16,PlaneUnpackWorkgroups,1,1);
  RecordComputeBarrier(aCommandBuffer);

  if not fLossless then begin
   DequantPush[0]:=PlanePixels;
   if Plane=0 then begin
    ChromaMultiplier:=1.0;
   end else begin
    ChromaMultiplier:=fChromaQuant;
   end;
   DequantPush[1]:=PpvInt32(@ChromaMultiplier)^;
   RecordDispatch(aCommandBuffer,fPipeDequant,fPLDequant,fSetDequant[Plane],@DequantPush[0],8,PlanePixelWorkgroups,1,1);
   RecordComputeBarrier(aCommandBuffer);
  end;

  LevelCount:=0;
  CurrentWidth:=PlaneW;
  CurrentHeight:=PlaneH;
  Level:=0;
  while ((Level<fLevels) and (CurrentWidth>=2)) and (CurrentHeight>=2) do begin
   LevelWidth[LevelCount]:=CurrentWidth;
   LevelHeight[LevelCount]:=CurrentHeight;
   inc(LevelCount);
   CurrentWidth:=(CurrentWidth+1) div 2;
   CurrentHeight:=(CurrentHeight+1) div 2;
   inc(Level);
  end;
  for Level:=LevelCount-1 downto 0 do begin
   LevelW:=LevelWidth[Level];
   LevelH:=LevelHeight[Level];
   TransposePush1[0]:=PlaneW;
   TransposePush1[1]:=LevelW;
   TransposePush1[2]:=LevelH;
   TransposePush1[3]:=ScratchStride;
   RecordDispatch(aCommandBuffer,fPipeTranspose,fPLTranspose,fSetCoeffToScratch[Plane],@TransposePush1[0],16,(LevelW+15) div 16,(LevelH+15) div 16,1);
   RecordComputeBarrier(aCommandBuffer);
   RowPush1[0]:=ScratchStride;
   RowPush1[1]:=LevelH;
   RowPush1[2]:=LevelW;
   RowPush1[3]:=1;
   RecordDispatch(aCommandBuffer,RowPipeline,fPLRow,fSetRowScratch,@RowPush1[0],16,LevelW,1,1);
   RecordComputeBarrier(aCommandBuffer);
   TransposePush2[0]:=ScratchStride;
   TransposePush2[1]:=LevelH;
   TransposePush2[2]:=LevelW;
   TransposePush2[3]:=PlaneW;
   RecordDispatch(aCommandBuffer,fPipeTranspose,fPLTranspose,fSetScratchToCoeff[Plane],@TransposePush2[0],16,(LevelH+15) div 16,(LevelW+15) div 16,1);
   RecordComputeBarrier(aCommandBuffer);
   RowPush2[0]:=PlaneW;
   RowPush2[1]:=LevelW;
   RowPush2[2]:=LevelH;
   RowPush2[3]:=1;
   RecordDispatch(aCommandBuffer,RowPipeline,fPLRow,fSetRow[Plane],@RowPush2[0],16,LevelH,1,1);
   RecordComputeBarrier(aCommandBuffer);
  end;

  // MCTF gop is integer: round the float 9/7 result before the integer MC-Haar inverse (open-loop stays float)
  if fMCTF and not fLossless then begin
   PixelCountPush:=PlanePixels;
   RecordDispatch(aCommandBuffer,fPipeRound,fPLRound,fSetRow[Plane],@PixelCountPush,4,PlanePixelWorkgroups,1,1);
   RecordComputeBarrier(aCommandBuffer);
  end;

  // hand the reconstructed plane to the transfer stage, then copy it into this frame's GOP slot
  FillChar(Barrier,SizeOf(Barrier),#0);
  Barrier.sType:=VK_STRUCTURE_TYPE_MEMORY_BARRIER;
  Barrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
  Barrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT);
  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                    0,1,@Barrier,0,nil,0,nil);
  FillChar(BufferCopy,SizeOf(BufferCopy),#0);
  BufferCopy.srcOffset:=0;
  BufferCopy.dstOffset:=TVkDeviceSize(aSlot)*TVkDeviceSize(PlanePixels)*4;
  BufferCopy.size:=TVkDeviceSize(PlanePixels)*4;
  aCommandBuffer.CmdCopyBuffer(fCoeffBuffer[Plane].Handle,fGopBuffer[Plane].Handle,1,@BufferCopy);

 end;

end;

procedure TpvFlexibleWaveletVideoDecoder.RecordTemporal3D(const aCommandBuffer:TpvVulkanCommandBuffer;const aGOPCount:TpvInt32);
var Plane,PlanePixels,Wavelet:TpvInt32;
    TemporalPush:array[0..4] of TpvInt32;
    Pipeline:TpvVulkanComputePipeline;
begin

 Wavelet:=fTemporalWavelet;
 if fLossless and (Wavelet=2) then begin
  Wavelet:=1; // the integer temporal path has no 9/7
 end;
 if fLossless then begin
  Pipeline:=fPipeTDWTInt;
 end else begin
  Pipeline:=fPipeTDWTFloat;
 end;

 for Plane:=0 to 2 do begin
  PlanePixels:=PlaneWidth(Plane)*PlaneHeight(Plane);
  TemporalPush[0]:=PlanePixels;
  TemporalPush[1]:=aGOPCount;
  TemporalPush[2]:=fTemporalLevels;
  TemporalPush[3]:=Wavelet;
  TemporalPush[4]:=1; // inverse
  RecordDispatch(aCommandBuffer,Pipeline,fPLTemporal,fSetTemporal[Plane],@TemporalPush[0],20,(PlanePixels+255) div 256,1,1);
  RecordComputeBarrier(aCommandBuffer);
 end;

end;

procedure TpvFlexibleWaveletVideoDecoder.DecodeMCTFInverse(const aGOPCount:TpvInt32);
var LumaBlocks,Plane,Level,Count,Len,LevelLen,LowCount,k,Even,Odd:TpvInt32;
    PlaneW,PlaneH,PlanePP,PlaneMBX:array[0..2] of TpvInt32;
    Lengths:array[0..15] of TpvInt32;
    PlanePixels:TpvInt32;
    LowOff,HighOff,EvenOff,OddOff:TVkDeviceSize;
    MCPush:array[0..2] of TpvInt32;
    AddPush:array[0..1] of TpvInt32;
    BufferCopy:TVkBufferCopy;
    Barrier:TVkMemoryBarrier;
    DataPointer:PpvUInt8Array;
begin

 LumaBlocks:=MotionBlocksX(fWidth)*MotionBlocksY(fHeight);
 for Plane:=0 to 2 do begin
  PlaneW[Plane]:=PlaneWidth(Plane);
  PlaneH[Plane]:=PlaneHeight(Plane);
  PlanePP[Plane]:=PlaneW[Plane]*PlaneH[Plane];
  PlaneMBX[Plane]:=((PlaneW[Plane]+fMotionBlock)-1) div fMotionBlock;
 end;

 Count:=0;
 Len:=aGOPCount;
 for Level:=0 to fTemporalLevels-1 do begin
  if Len<2 then begin
   break;
  end;
  Lengths[Count]:=Len;
  inc(Count);
  Len:=(Len+1) div 2;
 end;

 for Level:=Count-1 downto 0 do begin
  LevelLen:=Lengths[Level];
  LowCount:=(LevelLen+1) div 2;
  for k:=0 to LowCount-1 do begin
   Even:=2*k;
   fBDecodeCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));
   if ((2*k)+1)<LevelLen then begin // pair: odd = high + OBMC(low)
    Odd:=(2*k)+1;
    DataPointer:=PpvUInt8Array(fMVBuffer.Memory.MapMemory); // this high-pass frame's luma MVs
    try
     Move(fMCTFMVScratch[(LowCount+k)*LumaBlocks*2],DataPointer^[0],TpvSizeUInt(LumaBlocks)*2*4);
    finally
     fMVBuffer.Memory.UnmapMemory;
    end;
    for Plane:=0 to 2 do begin
     PlanePixels:=PlanePP[Plane];
     LowOff:=TVkDeviceSize(k)*PlanePixels*4;
     HighOff:=TVkDeviceSize(LowCount+k)*PlanePixels*4;
     EvenOff:=TVkDeviceSize(Even)*PlanePixels*4;
     OddOff:=TVkDeviceSize(Odd)*PlanePixels*4;
     // mc: warp gop@low(k) by this pair's MVs -> mctf_pred
     BindStorageBufferOffset(fSetMCTFMC[Plane],0,fGopBuffer[Plane],LowOff,TVkDeviceSize(PlanePixels)*4);
     BindStorageBuffer(fSetMCTFMC[Plane],1,fMVBuffer);
     BindStorageBuffer(fSetMCTFMC[Plane],2,fMCTFPred[Plane]);
     fSetMCTFMC[Plane].Flush;
     MCPush[0]:=PlaneW[Plane];
     MCPush[1]:=PlaneH[Plane];
     MCPush[2]:=PlaneMBX[Plane];
     RecordDispatch(fBDecodeCommandBuffer,fPipeMC,fPLUnpack,fSetMCTFMC[Plane],@MCPush[0],12,(PlanePixels+255) div 256,1,1);
     // even = low passthrough -> scratch@even; high -> scratch@odd (coeff_add adds pred in place)
     FillChar(BufferCopy,SizeOf(BufferCopy),#0);
     BufferCopy.srcOffset:=LowOff;
     BufferCopy.dstOffset:=EvenOff;
     BufferCopy.size:=TVkDeviceSize(PlanePixels)*4;
     fBDecodeCommandBuffer.CmdCopyBuffer(fGopBuffer[Plane].Handle,fMCTFScratch[Plane].Handle,1,@BufferCopy);
     BufferCopy.srcOffset:=HighOff;
     BufferCopy.dstOffset:=OddOff;
     fBDecodeCommandBuffer.CmdCopyBuffer(fGopBuffer[Plane].Handle,fMCTFScratch[Plane].Handle,1,@BufferCopy);
     // mc (compute) + the two copies (transfer) -> coeff_add (compute)
     FillChar(Barrier,SizeOf(Barrier),#0);
     Barrier.sType:=VK_STRUCTURE_TYPE_MEMORY_BARRIER;
     Barrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT);
     Barrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
     fBDecodeCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                              TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                              0,1,@Barrier,0,nil,0,nil);
     // coeff_add: scratch@odd (= high) += pred -> odd
     BindStorageBufferOffset(fSetMCTFAdd[Plane],0,fMCTFScratch[Plane],OddOff,TVkDeviceSize(PlanePixels)*4);
     BindStorageBuffer(fSetMCTFAdd[Plane],1,fMCTFPred[Plane]);
     fSetMCTFAdd[Plane].Flush;
     AddPush[0]:=PlanePixels;
     AddPush[1]:=1;
     RecordDispatch(fBDecodeCommandBuffer,fPipeCoeffAdd,fPLCoeffAdd,fSetMCTFAdd[Plane],@AddPush[0],8,(PlanePixels+255) div 256,1,1);
    end;
   end else begin // odd tail (no partner): even = low passthrough -> scratch@even
    for Plane:=0 to 2 do begin
     PlanePixels:=PlanePP[Plane];
     FillChar(BufferCopy,SizeOf(BufferCopy),#0);
     BufferCopy.srcOffset:=TVkDeviceSize(k)*PlanePixels*4;
     BufferCopy.dstOffset:=TVkDeviceSize(Even)*PlanePixels*4;
     BufferCopy.size:=TVkDeviceSize(PlanePixels)*4;
     fBDecodeCommandBuffer.CmdCopyBuffer(fGopBuffer[Plane].Handle,fMCTFScratch[Plane].Handle,1,@BufferCopy);
    end;
   end;
   fBDecodeCommandBuffer.EndRecording;
   fBDecodeCommandBuffer.Execute(fDevice.UniversalQueue,TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),nil,nil,fBDecodeFence,true);
  end;
  // copy scratch[0..level_len) back into gop_buffer (the interleaved frames for this level)
  fBDecodeCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));
  for Plane:=0 to 2 do begin
   FillChar(BufferCopy,SizeOf(BufferCopy),#0);
   BufferCopy.srcOffset:=0;
   BufferCopy.dstOffset:=0;
   BufferCopy.size:=TVkDeviceSize(LevelLen)*TVkDeviceSize(PlanePP[Plane])*4;
   fBDecodeCommandBuffer.CmdCopyBuffer(fMCTFScratch[Plane].Handle,fGopBuffer[Plane].Handle,1,@BufferCopy);
  end;
  fBDecodeCommandBuffer.EndRecording;
  fBDecodeCommandBuffer.Execute(fDevice.UniversalQueue,TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),nil,nil,fBDecodeFence,true);
 end;

end;

procedure TpvFlexibleWaveletVideoDecoder.RecordDisplay3D(const aCommandBuffer:TpvVulkanCommandBuffer;const aSlot:TpvInt32);
var Plane,PlanePixels,PixelWorkgroups:TpvInt32;
    BufferCopy:TVkBufferCopy;
    Barrier:TVkMemoryBarrier;
    PixelCountPush:TpvInt32;
    ColourPush:array[0..5] of TpvInt32;
    HDRPush:array[0..7] of TpvInt32;
    ExposureBits:TpvFloat;
begin

 RecordImageBarrier(aCommandBuffer,
                    VK_IMAGE_LAYOUT_UNDEFINED,VK_IMAGE_LAYOUT_GENERAL,
                    0,TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT));

 // copy this display frame's GOP slot into coeff, then (lossy open-loop only) round the float to int
 for Plane:=0 to 2 do begin
  PlanePixels:=PlaneWidth(Plane)*PlaneHeight(Plane);
  FillChar(BufferCopy,SizeOf(BufferCopy),#0);
  BufferCopy.srcOffset:=TVkDeviceSize(aSlot)*TVkDeviceSize(PlanePixels)*4;
  BufferCopy.dstOffset:=0;
  BufferCopy.size:=TVkDeviceSize(PlanePixels)*4;
  aCommandBuffer.CmdCopyBuffer(fGopBuffer[Plane].Handle,fCoeffBuffer[Plane].Handle,1,@BufferCopy);
 end;
 FillChar(Barrier,SizeOf(Barrier),#0);
 Barrier.sType:=VK_STRUCTURE_TYPE_MEMORY_BARRIER;
 Barrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT);
 Barrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,1,@Barrier,0,nil,0,nil);

 if (not fLossless) and (not fMCTF) then begin // open-loop lossy gop is float -> round to int before colour
  for Plane:=0 to 2 do begin
   PlanePixels:=PlaneWidth(Plane)*PlaneHeight(Plane);
   PixelCountPush:=PlanePixels;
   RecordDispatch(aCommandBuffer,fPipeRound,fPLRound,fSetRow[Plane],@PixelCountPush,4,(PlanePixels+255) div 256,1,1);
   RecordComputeBarrier(aCommandBuffer);
  end;
 end;

 PixelWorkgroups:=((fWidth*fHeight)+255) div 256;
 if fIsHDR then begin
  HDRPush[0]:=fWidth;
  HDRPush[1]:=fHeight;
  ExposureBits:=fHDRExposure;
  HDRPush[2]:=PpvInt32(@ExposureBits)^;
  HDRPush[3]:=fTransferFunction;
  HDRPush[4]:=ChromaShiftX;
  HDRPush[5]:=ChromaShiftY;
  HDRPush[6]:=PlaneWidth(1);
  HDRPush[7]:=PlaneHeight(1);
  if fUseSCRGB then begin
   RecordDispatch(aCommandBuffer,fPipeColourHDRSCRGB,fPLColourHDR,fSetColour,@HDRPush[0],32,PixelWorkgroups,1,1);
  end else begin
   RecordDispatch(aCommandBuffer,fPipeColourHDR,fPLColourHDR,fSetColour,@HDRPush[0],32,PixelWorkgroups,1,1);
  end;
 end else begin
  ColourPush[0]:=fWidth;
  ColourPush[1]:=fHeight;
  ColourPush[2]:=ChromaShiftX;
  ColourPush[3]:=ChromaShiftY;
  ColourPush[4]:=PlaneWidth(1);
  ColourPush[5]:=PlaneHeight(1);
  RecordDispatch(aCommandBuffer,fPipeColour,fPLColour,fSetColour,@ColourPush[0],24,PixelWorkgroups,1,1);
 end;

 RecordImageBarrier(aCommandBuffer,
                    VK_IMAGE_LAYOUT_GENERAL,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                    TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT));

end;

procedure TpvFlexibleWaveletVideoDecoder.DecodeFrame3D(const aCommandBuffer:TpvVulkanCommandBuffer;const aDisplayIndex:TpvInt32);
var GopStart,GopCount,SlotIndex:TpvInt32;
begin

 fBufferRingSlot:=-1; // 3D-DWT uses the shared input buffers

 // the GOP containing this display frame starts at the nearest preceding type-0 subband frame
 GopStart:=aDisplayIndex;
 while (GopStart>0) and (fFrameEntries[GopStart].FrameType<>0) do begin
  dec(GopStart);
 end;

 if fCur3DGopStart<>GopStart then begin
  GopCount:=GopCountFrom(GopStart);
  // spatial-inverse each subband frame into its GOP slot (self-submit per frame: the shared input buffers are reused)
  for SlotIndex:=0 to GopCount-1 do begin
   Upload3DFrame(GopStart+SlotIndex,SlotIndex,GopCount);
   fBDecodeCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));
   RecordSpatial3D(fBDecodeCommandBuffer,SlotIndex);
   fBDecodeCommandBuffer.EndRecording;
   fBDecodeCommandBuffer.Execute(fDevice.UniversalQueue,TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),nil,nil,fBDecodeFence,true);
  end;
  // temporal inverse over the whole GOP: MCTF MC-Haar (self-submits per pair) or the open-loop temporal DWT
  if fMCTF then begin
   DecodeMCTFInverse(GopCount);
  end else begin
   fBDecodeCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));
   RecordTemporal3D(fBDecodeCommandBuffer,GopCount);
   fBDecodeCommandBuffer.EndRecording;
   fBDecodeCommandBuffer.Execute(fDevice.UniversalQueue,TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),nil,nil,fBDecodeFence,true);
  end;
  fCur3DGopStart:=GopStart;
 end;

 RecordDisplay3D(aCommandBuffer,aDisplayIndex-GopStart);

end;

procedure TpvFlexibleWaveletVideoDecoder.DecodeFrame(const aCommandBuffer:TpvVulkanCommandBuffer;const aFrameIndex:TpvInt32);
begin
 if fMode3DDWT then begin
  DecodeFrame3D(aCommandBuffer,aFrameIndex);
 end else if fHasBFrames then begin
  // B-stream: aFrameIndex is the DISPLAY POC; the decode-ahead reconstructs coding-order frames into the DPB.
  DecodeFrameBidi(aCommandBuffer,aFrameIndex);
 end else begin
  UploadFrame(aFrameIndex);
  RecordDecode(aCommandBuffer,fFrameEntries[aFrameIndex].FrameType<>0); // is_predicted = frame type is not I
 end;
end;

procedure TpvFlexibleWaveletVideoDecoder.PrepareFrameBidi(const aDisplayPOC:TpvInt32);
var IsPredicted,Ref1Slot,Weight0,Weight1,RingIndex:TpvInt32;
begin

 // CPU decode-ahead (mode B): upload + DPB-manage each coding-order frame into its own ring slot until this
 // display POC is ready + the lead is full, capturing a per-frame record plan for RecordFrameBidi to replay.
 fBidiPlanCount:=0;
 RingIndex:=0;
 while fGCursor<fFrameCount do begin
  if (fGDPBPOCToSlot[aDisplayPOC]>=0) and (fGCursor>=(aDisplayPOC+fGDecodeLead)) then begin
   break;
  end;
  if RingIndex>=fBufferRingSize then begin
   raise EpvFlexibleWaveletVideoDecoder.Create('B-frame mode B input ring overflow');
  end;
  fBufferRingSlot:=RingIndex;
  if not PrepareBidiFrame(aDisplayPOC,IsPredicted,Ref1Slot,Weight0,Weight1) then begin
   break;
  end;
  fBidiPlan[fBidiPlanCount].RingSlot:=RingIndex;
  fBidiPlan[fBidiPlanCount].IsPredicted:=IsPredicted;
  fBidiPlan[fBidiPlanCount].Ref1Slot:=Ref1Slot;
  fBidiPlan[fBidiPlanCount].Weight0:=Weight0;
  fBidiPlan[fBidiPlanCount].Weight1:=Weight1;
  inc(fBidiPlanCount);
  inc(RingIndex);
 end;
 fBufferRingSlot:=-1;
 fBidiDisplayPOC:=aDisplayPOC;

end;

procedure TpvFlexibleWaveletVideoDecoder.RecordFrameBidi(const aCommandBuffer:TpvVulkanCommandBuffer);
var PlanIndex:TpvInt32;
begin

 // GPU: replay the captured decode-ahead plan into the caller command buffer (each frame's ring slot keeps the
 // descriptor bindings PrepareFrameBidi made), then record the display.
 for PlanIndex:=0 to fBidiPlanCount-1 do begin
  fBufferRingSlot:=fBidiPlan[PlanIndex].RingSlot;
  RecordBidiDecode(aCommandBuffer,fBidiPlan[PlanIndex].IsPredicted,fBidiPlan[PlanIndex].Ref1Slot,fBidiPlan[PlanIndex].Weight0,fBidiPlan[PlanIndex].Weight1);
 end;
 fBufferRingSlot:=-1;

 RecordBidiDisplay(aCommandBuffer,fBidiDisplayPOC);

end;

procedure TpvFlexibleWaveletVideoDecoder.PrepareFrame(const aDisplayIndex:TpvInt32);
begin

 // CPU side of the poll-API split (Update thread): parse + decompress + MV/mode decode + host-buffer upload.
 fPreparedIndex:=aDisplayIndex;
 if fMode3DDWT then begin
  // 3D-DWT/MCTF: the multi-pass GOP rebuild self-submits, so its CPU uploads stay coupled inside RecordFrame.
 end else if fHasBFrames then begin
  PrepareFrameBidi(aDisplayIndex);
 end else begin
  UploadFrame(aDisplayIndex);
  fPreparedIsPredicted:=fFrameEntries[aDisplayIndex].FrameType<>0; // is_predicted = frame type is not I
 end;

end;

procedure TpvFlexibleWaveletVideoDecoder.RecordFrame(const aCommandBuffer:TpvVulkanCommandBuffer);
begin

 // GPU side of the poll-API split (Draw thread): record the prepared frame into the caller command buffer.
 if fPreparedIndex<0 then begin
  exit;
 end;
 if fMode3DDWT then begin
  DecodeFrame3D(aCommandBuffer,fPreparedIndex);
 end else if fHasBFrames then begin
  RecordFrameBidi(aCommandBuffer);
 end else begin
  RecordDecode(aCommandBuffer,fPreparedIsPredicted);
 end;
 fPreparedIndex:=-1;

end;

constructor TpvFlexibleWaveletVideoDecoder.Create(const aStream:TStream;const aDevice:TpvVulkanDevice;const aPreferSCRGBForHDR:boolean;const aBSubmitMode:TpvInt32);
begin
 inherited Create;

 fPreparedIndex:=-1;

 fStream:=aStream;
 fDevice:=aDevice;
 fPreferSCRGB:=aPreferSCRGBForHDR; // consumed by ParseContainer (output format) below
 fSubmitMode:=aBSubmitMode;

 ParseContainer;

 fPipelineCache:=TpvVulkanPipelineCache.Create(fDevice);

 BuildPipelines;
 BuildBuffersAndImage;
 BuildDescriptorSets;

 // the B-frame mode-A decode-ahead and the 3D-DWT GOP decode self-submit on their own command buffer + fence
 if (fHasBFrames and (fSubmitMode=0)) or fMode3DDWT then begin
  fBDecodeCommandPool:=TpvVulkanCommandPool.Create(fDevice,fDevice.UniversalQueueFamilyIndex);
  fBDecodeCommandBuffer:=TpvVulkanCommandBuffer.Create(fBDecodeCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  fBDecodeFence:=TpvVulkanFence.Create(fDevice);
 end;

 // mode B only: the per-frame input ring (the whole decode-ahead in one caller command buffer)
 if fHasBFrames and (fSubmitMode=1) then begin
  BuildBidiRing;
 end;

end;

destructor TpvFlexibleWaveletVideoDecoder.Destroy;
var Plane,SlotIndex:TpvInt32;
begin

 FreeAndNil(fBDecodeFence);
 FreeAndNil(fBDecodeCommandBuffer);
 FreeAndNil(fBDecodeCommandPool);

 FreeAndNil(fSetColour);
 FreeAndNil(fSetRowScratch);
 for Plane:=0 to 2 do begin
  FreeAndNil(fSetUnpack[Plane]);
  FreeAndNil(fSetDequant[Plane]);
  FreeAndNil(fSetAdd[Plane]);
  FreeAndNil(fSetMCPlay[Plane]);
  FreeAndNil(fSetMotionAddPlay[Plane]);
  FreeAndNil(fSetGMC0[Plane]);
  FreeAndNil(fSetGMC1[Plane]);
  FreeAndNil(fSetGBlend[Plane]);
  FreeAndNil(fSetGBlendMode[Plane]);
  FreeAndNil(fSetGAdd[Plane]);
  FreeAndNil(fSetTemporal[Plane]);
  FreeAndNil(fSetMCTFMC[Plane]);
  FreeAndNil(fSetMCTFAdd[Plane]);
  FreeAndNil(fSetCoeffToScratch[Plane]);
  FreeAndNil(fSetScratchToCoeff[Plane]);
  FreeAndNil(fSetRow[Plane]);
 end;
 for SlotIndex:=0 to length(fRingDataBuffer)-1 do begin // mode B per-frame input ring
  for Plane:=0 to 2 do begin
   FreeAndNil(fRingSetUnpack[SlotIndex][Plane]);
   FreeAndNil(fRingSetDequant[SlotIndex][Plane]);
   FreeAndNil(fRingSetGMC0[SlotIndex][Plane]);
   FreeAndNil(fRingSetGMC1[SlotIndex][Plane]);
   FreeAndNil(fRingSetGBlend[SlotIndex][Plane]);
   FreeAndNil(fRingSetGBlendMode[SlotIndex][Plane]);
   FreeAndNil(fRingSetGAdd[SlotIndex][Plane]);
   FreeAndNil(fRingOffsetBuffer[SlotIndex][Plane]);
   FreeAndNil(fRingStepBuffer[SlotIndex][Plane]);
  end;
  FreeAndNil(fRingDataBuffer[SlotIndex]);
  FreeAndNil(fRingMVBuffer[SlotIndex]);
  FreeAndNil(fRingMV1Buffer[SlotIndex]);
  FreeAndNil(fRingModeBuffer[SlotIndex]);
 end;
 FreeAndNil(fDescriptorPool);

 FreeAndNil(fOutputImageStorageView);
 FreeAndNil(fOutputImageView);
 FreeAndNil(fOutputImage);
 if assigned(fOutputImageMemory) then begin
  fDevice.MemoryManager.FreeMemoryBlock(fOutputImageMemory);
  fOutputImageMemory:=nil;
 end;
 for Plane:=0 to 2 do begin
  FreeAndNil(fGopBuffer[Plane]);
  FreeAndNil(fMCTFPred[Plane]);
  FreeAndNil(fMCTFScratch[Plane]);
 end;
 FreeAndNil(fMV1Buffer);
 FreeAndNil(fModeBuffer);
 for SlotIndex:=0 to length(fDPBBuffer)-1 do begin
  for Plane:=0 to 2 do begin
   FreeAndNil(fDPBBuffer[SlotIndex][Plane]);
  end;
 end;
 for Plane:=0 to 2 do begin
  FreeAndNil(fGMCBuffer[0][Plane]);
  FreeAndNil(fGMCBuffer[1][Plane]);
 end;
 FreeAndNil(fMVBuffer);
 FreeAndNil(fScratchBuffer);
 for Plane:=0 to 2 do begin
  FreeAndNil(fPreviousBuffer[Plane]);
  FreeAndNil(fCoeffBuffer[Plane]);
  FreeAndNil(fStepBuffer[Plane]);
  FreeAndNil(fOffsetBuffer[Plane]);
 end;
 FreeAndNil(fDataBuffer);

 FreeAndNil(fPipeTDWTFloat);
 FreeAndNil(fPipeTDWTInt);
 FreeAndNil(fPipeBlendMode);
 FreeAndNil(fPipeBidiBlend);
 FreeAndNil(fPipeColourHDRSCRGB);
 FreeAndNil(fPipeColourHDR);
 FreeAndNil(fPipeColour);
 FreeAndNil(fPipeMotionAdd);
 FreeAndNil(fPipeMC);
 FreeAndNil(fPipeCoeffAdd);
 FreeAndNil(fPipeRound);
 FreeAndNil(fPipeIDWT53);
 FreeAndNil(fPipeIDWT97);
 FreeAndNil(fPipeTranspose);
 FreeAndNil(fPipeDequant);
 FreeAndNil(fPipeUnpack);

 FreeAndNil(fPLTemporal);
 FreeAndNil(fPLBlendMode);
 FreeAndNil(fPLColourHDR);
 FreeAndNil(fPLColour);
 FreeAndNil(fPLCoeffAdd);
 FreeAndNil(fPLRound);
 FreeAndNil(fPLRow);
 FreeAndNil(fPLTranspose);
 FreeAndNil(fPLDequant);
 FreeAndNil(fPLUnpack);

 FreeAndNil(fDSLColour);
 FreeAndNil(fDSL3);
 FreeAndNil(fDSL2);
 FreeAndNil(fDSL1);

 FreeAndNil(fPipelineCache);

 inherited Destroy;
end;

end.
