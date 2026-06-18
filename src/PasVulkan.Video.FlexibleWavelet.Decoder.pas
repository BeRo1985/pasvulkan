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
       fNumPlanes:TpvInt32; // 3 (Y,Co,Cg) by default, 4 when the stream carries an optional alpha channel (plane 3 = full-res like luma)
       fPredictionMethod:TpvInt32;
       fGOP:TpvInt32;
       fMotionBlock:TpvInt32;
       fMVCodec:TpvInt32;
       fMotionMode:TpvInt32;
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
       fCur3DGopStart:TpvInt32; // display index of the GOP currently being presented (-1 = none)
       fGopBuffer:array[0..1,0..2] of TpvVulkanBuffer; // double-buffered reconstructed GOP (cur vs prefetched), device-local
       fPipeTDWTInt:TpvVulkanComputePipeline;
       fPipeTDWTFloat:TpvVulkanComputePipeline;
       fPLTemporal:TpvVulkanPipelineLayout; // DSL1 + 20-byte push
       fSetTemporal:array[0..1,0..2] of TpvVulkanDescriptorSet; // per GOP buffer: {gop_buffer[buf][plane]}
       fMCTFPred:array[0..3] of TpvVulkanBuffer; // MCTF: the per-pair MC-warped low frame, device-local
       fMCTFScratch:array[0..3] of TpvVulkanBuffer; // MCTF: the per-level interleaved frame workspace, device-local
       fMCTFMVScratch:array of TpvInt32; // MCTF: every GOP frame's luma MV field (CPU side), by deinterleaved slot
       fSetMCTFMC:array[0..3] of TpvVulkanDescriptorSet; // MCTF (rebound per pair, with byte offsets): {gop@low, mv, pred}
       fSetMCTFAdd:array[0..3] of TpvVulkanDescriptorSet; // MCTF: {scratch@odd, pred}
       // 3D-DWT GOP prefetch: the next GOP is decoded one subband per displayed frame on a SEPARATE command buffer +
       // fence (overlaps the present) into the OTHER gop buffer, then swapped in — so no whole-GOP burst stalls a frame.
       // The prefetch spatial inverse writes its own fPrefetchCoeff (the present's display only touches fCoeffBuffer, so
       // data/offset/step/scratch stay free for the prefetch to reuse).
       fPrefetchCoeff:array[0..3] of TpvVulkanBuffer;
       fSetUnpackPF:array[0..3] of TpvVulkanDescriptorSet; // {data, offset, prefetch_coeff}
       fSetDequantPF:array[0..3] of TpvVulkanDescriptorSet; // {prefetch_coeff, step}
       fSetCoeffToScratchPF:array[0..3] of TpvVulkanDescriptorSet; // {prefetch_coeff, scratch}
       fSetScratchToCoeffPF:array[0..3] of TpvVulkanDescriptorSet; // {scratch, prefetch_coeff}
       fSetRowPF:array[0..3] of TpvVulkanDescriptorSet; // {prefetch_coeff} (iDWT row + MCTF round)
       fPrefetchCommandBuffer:TpvVulkanCommandBuffer; // async GOP-prefetch steps (overlaps present)
       fPrefetchFence:TpvVulkanFence;
       f3DCurBuf:TpvInt32; // gop_buffer index currently presented
       f3DCurGopCount:TpvInt32; // frame count of the presented GOP
       f3DPfBuf:TpvInt32; // gop_buffer index being prefetched into
       f3DPfGopStart:TpvInt32; // first frame index of the prefetched GOP
       f3DPfGopCount:TpvInt32; // frame count of the prefetched GOP
       f3DPfStep:TpvInt32; // next subband to prefetch (0..f3DPfGopCount)
       f3DPfDone:boolean; // the prefetched GOP is fully decoded (spatial + temporal/MCTF)
       f3DPfPending:boolean; // a prefetch submit is in flight on fPrefetchFence (gates PrefetchWait so it never waits empty)
       f3DInitialized:boolean; // GOP 0 has been decoded up-front
       // Per-frame input ring: -1 = the shared buffers/sets (mode A self-submit, mode C caller step-loop, 3D-DWT),
       // >=0 = the active ring slot. Used by mode B (the whole decode-ahead into ONE caller command buffer) AND by the
       // pure intra/I-P path, which cycles one slot per displayed frame so a pipelined Update can stage frame N+1 without
       // clobbering the input buffers frame N's still-running GPU decode reads (the washed-out / pale-fade race).
       fBufferRingSlot:TpvInt32;
       fBufferRingSize:TpvInt32;
       fIPInputRing:boolean; // the intra/I-P path uses the ring (set when not B-frames and not 3D-DWT)
       fIPRingSlot:TpvInt32; // next ring slot the intra/I-P PrepareFrame will use (cycles 0..fBufferRingSize-1)
       fPreparedRingSlot:TpvInt32; // ring slot PrepareFrame chose; RecordFrame restores it (Update + Draw are separate calls)
       fRingDataBuffer:array of TpvVulkanBuffer;
       fRingOffsetBuffer:array of array[0..3] of TpvVulkanBuffer;
       fRingStepBuffer:array of array[0..3] of TpvVulkanBuffer;
       fRingTileCodesBuffer:array of TpvVulkanBuffer; // AQ (GPU): per-ring-slot tile codes (the shared buffer would clobber under pipelining)
       fRingMVBuffer:array of TpvVulkanBuffer;
       fRingMV1Buffer:array of TpvVulkanBuffer;
       fRingModeBuffer:array of TpvVulkanBuffer;
       fRingSetUnpack:array of array[0..3] of TpvVulkanDescriptorSet; // bound once: {ring data, ring offset, shared coeff}
       fRingSetDequant:array of array[0..3] of TpvVulkanDescriptorSet; // bound once: {shared coeff, ring step}
       fRingSetApplyAQ:array of array[0..3] of TpvVulkanDescriptorSet; // AQ apply (B-frame ring): {ring step, tile codes, weight LUT, ring step (in-place)}
       fRingSetGMC0:array of array[0..3] of TpvVulkanDescriptorSet; // rebound per frame
       fRingSetGMC1:array of array[0..3] of TpvVulkanDescriptorSet;
       fRingSetGBlend:array of array[0..3] of TpvVulkanDescriptorSet;
       fRingSetGBlendMode:array of array[0..3] of TpvVulkanDescriptorSet;
       fRingSetGAdd:array of array[0..3] of TpvVulkanDescriptorSet;
       fRingSetMCPlay:array of array[0..3] of TpvVulkanDescriptorSet; // intra/I-P motion comp: {shared previous, ring mv, shared scratch}
       // two-phase decode (the poll-API split): PrepareFrame does the CPU side, RecordFrame the GPU side.
       fPreparedIndex:TpvInt32; // display index the last PrepareFrame staged (-1 = none)
       fPreparedIsPredicted:boolean; // intra/P: is_predicted of fPreparedIndex
       fBidiPlan:TBidiPlans; // mode-B decode-ahead plan captured by PrepareFrameBidi, replayed by RecordFrameBidi
       fBidiPlanCount:TpvInt32;
       fBidiDisplayPOC:TpvInt32;
       fBidiRingCursor:TpvInt32; // mode B: free-running ring-slot cursor; does NOT reset per display frame, so consecutive
                                 // (pipelined, in-flight) frames take DISJOINT input slots instead of clobbering each other
       fHFGain:TSynthesisGains;
       fLLGain:TpvFloat;
       fHasAlpha:boolean;     // color_flags bit2: an optional appended 8-bit alpha section per frame (intra, full-res)
       fAlphaPremultiplied:boolean; // color_flags bit3: the RGB is premultiplied by alpha
       fAlphaQP:TpvInt32;     // the alpha section's own quantization level (parsed per frame; 0 = lossless 5/3)
       fAlphaLossless:boolean; // fAlphaQP=0 -> reversible 5/3 alpha inverse transform
       fUseSCRGB:boolean;
       fOutputFormat:TVkFormat;
       fPipelineCache:TpvVulkanPipelineCache;
       fDSL1:TpvVulkanDescriptorSetLayout; // 1 storage buffer
       fDSL2:TpvVulkanDescriptorSetLayout; // 2 storage buffers
       fDSL3:TpvVulkanDescriptorSetLayout; // 3 storage buffers
       fDSL4:TpvVulkanDescriptorSetLayout; // 4 storage buffers (AQ apply: base step, tile codes, weight LUT, modulated step)
       fDSLColor:TpvVulkanDescriptorSetLayout; // 3 storage buffers + 1 storage image
       fDSLColorAlpha:TpvVulkanDescriptorSetLayout; // alpha variant: 3 buffers + 1 image + 1 alpha buffer (bindings 0,1,2,3=image,4=alpha)
       fPLUnpack:TpvVulkanPipelineLayout;
       fPLDequant:TpvVulkanPipelineLayout;
       fPLApplyAQ:TpvVulkanPipelineLayout; // AQ apply: fDSL4 + 20-byte push {width,height,levels,tile_cols,tile_rows}
       fPLTranspose:TpvVulkanPipelineLayout;
       fPLRow:TpvVulkanPipelineLayout;
       fPLRound:TpvVulkanPipelineLayout;
       fPLCoeffAdd:TpvVulkanPipelineLayout;
       fPLColor:TpvVulkanPipelineLayout;
       fPLColorAlpha:TpvVulkanPipelineLayout;
       fPLColorHDR:TpvVulkanPipelineLayout;
       fPLColorHDRAlpha:TpvVulkanPipelineLayout; // HDR alpha variant: fDSLColorAlpha (5 bindings) + the 32-byte HDR push
       fPipeUnpack:TpvVulkanComputePipeline;
       fPipeDequant:TpvVulkanComputePipeline;
       fPipeApplyAQ:TpvVulkanComputePipeline; // apply_tile_aq.comp: GPU per-tile AQ step modulation (replaces the per-frame CPU ApplyAQ)
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
       fPipeColor:TpvVulkanComputePipeline;
       fPipeColorAlpha:TpvVulkanComputePipeline; // color_alpha.spv: also writes the decoded alpha plane (binding 4) into output A
       fPipeColorHDR:TpvVulkanComputePipeline;
       fPipeColorHDRSCRGB:TpvVulkanComputePipeline;
       fPipeColorHDRAlpha:TpvVulkanComputePipeline; // color_hdr_alpha.spv: HDR/SDR-tonemap output + decoded alpha into A
       fPipeColorHDRSCRGBAlpha:TpvVulkanComputePipeline; // color_hdr_scrgb_alpha.spv: FP16 scRGB output + decoded alpha into A
       fDescriptorPool:TpvVulkanDescriptorPool;
       fDataBuffer:TpvVulkanBuffer;
       // alpha host-input ring (data/offset/step) — pipelining-safety: the CPU writes these each displayed frame while
       // an earlier frame's GPU alpha decode may still be reading them, so they cycle through fAlphaRingSize slots (a
       // free-running cursor) exactly like the color I/P input ring. The decoded coeff[3] (device-local) stays shared.
       fAlphaRingSize:TpvInt32;
       fAlphaRingCursor:TpvInt32;  // next free-running slot the upload will use (cycles 0..fAlphaRingSize-1)
       fAlphaCurrentSlot:TpvInt32; // the slot the last UploadAlpha* wrote; RecordAlphaDecode reads it (Upload + Record are separate calls)
       fAlphaRingData:array of TpvVulkanBuffer;   // [slot] packed bitplane bytes (own buffer -> alpha offsets are prefix-sums-from-0, uniform across modes)
       fAlphaRingOffset:array of TpvVulkanBuffer;  // [slot] alpha block offsets
       fAlphaRingStep:array of TpvVulkanBuffer;    // [slot] alpha quant steps (lossy)
       fAlphaRingSetUnpack:array of TpvVulkanDescriptorSet;  // [slot] {ring data, ring offset, shared coeff[3]}
       fAlphaRingSetDequant:array of TpvVulkanDescriptorSet; // [slot] {shared coeff[3], ring step}
       fOffsetBuffer:array[0..3] of TpvVulkanBuffer;
       fStepBuffer:array[0..3] of TpvVulkanBuffer;
       fWeightLUTBuffer:TpvVulkanBuffer;  // AQ (GPU): 256 log-spaced weights (= aq_weight_from_code), uploaded once
       fTileCodesBuffer:TpvVulkanBuffer;  // AQ (GPU): this frame's raw qpmap tile codes (4 codes per uint), uploaded per frame
       fAQPush:array[0..4] of TpvInt32;   // AQ (GPU): apply_tile_aq push {width,height,levels,tile_cols,tile_rows} (recording is sequential)
       fCoeffBuffer:array[0..3] of TpvVulkanBuffer;
       fPreviousBuffer:array[0..3] of TpvVulkanBuffer; // P-frame reference (coefficients / reconstructed YCoCg), GPU-resident across frames
       fMVBuffer:TpvVulkanBuffer; // colordiff (B): per-block [mv_x, mv_y] (half-pel), host-visible
       fMV1Buffer:TpvVulkanBuffer; // B-frames: the L1 motion-vector field, host-visible
       fModeBuffer:TpvVulkanBuffer; // B-frames: per-block L0/L1/BI mode, host-visible
       fDPBBuffer:array of array[0..3] of TpvVulkanBuffer; // B-frame decoded-picture-buffer slots (YCoCg), device-local
       fGMCBuffer:array[0..1] of array[0..3] of TpvVulkanBuffer; // B-frames: the L0/L1 motion-compensated references, device-local
       fScratchBuffer:TpvVulkanBuffer;
       fOutputImage:TpvVulkanImage;
       fOutputImageMemory:TpvVulkanDeviceMemoryBlock;
       fOutputImageView:TpvVulkanImageView;        // sample / present view (sRGB for SDR -> samples to linear; FP16 for HDR)
       fOutputImageStorageView:TpvVulkanImageView; // compute storage view (UNORM for SDR -> stores raw gamma bytes; FP16 for HDR)
       fOutputStorageFormat:TVkFormat;
       fOutputImageFlags:TVkImageCreateFlags;
       fSetUnpack:array[0..3] of TpvVulkanDescriptorSet;
       fSetDequant:array[0..3] of TpvVulkanDescriptorSet;
       fSetApplyAQ:array[0..3] of TpvVulkanDescriptorSet; // AQ apply (I/P + 3D-DWT prefetch): {fStepBuffer, tile codes, weight LUT, fStepBuffer (in-place)}
       fSetAdd:array[0..3] of TpvVulkanDescriptorSet; // coefdiff (A): {coeff, previous}
       fSetMCPlay:array[0..3] of TpvVulkanDescriptorSet; // colordiff (B): {previous, mv, scratch=mc_prev}
       fSetMotionAddPlay:array[0..3] of TpvVulkanDescriptorSet; // colordiff (B): {coeff, scratch=mc_prev, previous}
       fSetGMC0:array[0..3] of TpvVulkanDescriptorSet; // B-frames (rewritten per frame): {dpb[ref0], mv, gmc0}
       fSetGMC1:array[0..3] of TpvVulkanDescriptorSet; // B-frames: {dpb[ref1], mv1, gmc1}
       fSetGBlend:array[0..3] of TpvVulkanDescriptorSet; // B-frames: {gmc0, gmc1, scratch}
       fSetGBlendMode:array[0..3] of TpvVulkanDescriptorSet; // B-frames: {gmc0, gmc1, mode}
       fSetGAdd:array[0..3] of TpvVulkanDescriptorSet; // B-frames: {coeff, prediction, dpb[dst]}
       fSetCoeffToScratch:array[0..3] of TpvVulkanDescriptorSet;
       fSetScratchToCoeff:array[0..3] of TpvVulkanDescriptorSet;
       fSetRow:array[0..3] of TpvVulkanDescriptorSet;
       fSetRowScratch:TpvVulkanDescriptorSet;
       fSetColor:TpvVulkanDescriptorSet;
       fSetColorAlpha:TpvVulkanDescriptorSet; // color_alpha: {coeff0,coeff1,coeff2, image, coeff3=alpha}
       fFrameScratch:array of TpvUInt8; // decompressed frame payload, grown on demand
       fCompressedScratch:array of TpvUInt8; // raw container bytes of the current frame
       fAlphaCompressedScratch:array of TpvUInt8; // alpha re-read (B / 3D-DWT): a displayed frame's raw container bytes (kept off fCompressedScratch, which may hold an in-flight color frame)
       fAlphaFrameScratch:array of TpvUInt8; // alpha re-read: the decompressed displayed frame payload
       fAlphaOffsetScratch:array of TpvUInt32; // the alpha plane's block-offset prefix sums (CPU side)
       fOffsetScratch:array[0..3] of array of TpvUInt32; // per-plane block offset prefix sums (CPU side)
       fStepScratch:array of TpvInt32; // per-plane quantization step map (CPU side), grown on demand
       // Quantization-step cache: the step map depends only on (quality, levels, gains, sample-white), NOT on frame
       // content, so it is built ONCE per distinct quality and reused (the C fwvplay does the same via step_cache).
       // Without this the per-pixel rebuild every frame is the CPU bottleneck at 1080p.
       fStepCacheQuality:array of TpvInt32;       // quality value held by each cache slot
       fStepCacheData:array of array of TpvInt32; // [(slot*3)+plane] -> the prebuilt step map for that quality/plane
       // AQ (per-tile QP): the per-frame per-tile QP maps (qpmap container section, coding order). When present the
       // base (per-quality) step is modulated by the current frame's map in-place AFTER the cache Move, so the cache
       // stays the content-independent base and the GPU dequant is untouched (matches the C fwvplay decoder).
       fAQEnabled:boolean;
       fAQMaps:array of TpvUInt8; // all frames' maps concatenated by coding index (frame_count * fAQCols * fAQRows)
       fAQCols:TpvInt32;
       fAQRows:TpvInt32;
       fAQMapBytes:TpvInt32; // fAQCols * fAQRows
       fAQCurrentMap:PpvUInt8Array; // the current frame's map (set per frame by SetAQCurrentMap), or nil
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
       procedure SetAQCurrentMap(const aCodingIndex:TpvInt32); // AQ: select this coding frame's per-tile QP map (no-op if AQ off)
       procedure UploadTileCodes; // AQ (GPU): copy the current frame's raw qpmap tile codes into fTileCodesBuffer for apply_tile_aq.comp
       procedure UploadFrame(const aFrameIndex:TpvInt32);
       procedure RecordDecode(const aCommandBuffer:TpvVulkanCommandBuffer;const aIsPredicted:boolean);
       // Optional alpha (color_flags bit2): one intra, full-res 8-bit plane appended per frame. The color is decoded
       // as usual into coeff[0..2]; ParseAlphaSection + UploadAlpha* stage the appended section, RecordAlphaDecode GPU-
       // decodes it into coeff[3], and the color pass swaps to fPipeColorAlpha (which writes coeff[3] into output A).
       function ParseAlphaSection(const aFrameBuffer:PpvUInt8Array;const aFrameLength:TpvSizeUInt;const aSectionOffset:TpvSizeUInt;const aBlockCount:TpvInt32;out aAlphaQP:TpvInt32;out aAlphaDataOffset:TpvSizeUInt;out aAlphaDataLength:TpvUInt32):boolean; // False = corrupt / truncated alpha section
       procedure UploadAlphaFromBuffer(const aFrameBuffer:PpvUInt8Array;const aFrameLength:TpvSizeUInt;const aSectionOffset:TpvSizeUInt);
       procedure UploadAlphaForDisplayedFrame(const aCodingIndex:TpvInt32);
       procedure RecordAlphaDecode(const aCommandBuffer:TpvVulkanCommandBuffer);
       // hierarchical B-frames (Stage E3). The Active* helpers return the shared buffer/set (fBufferRingSlot<0,
       // modes A/C) or the active ring slot's (fBufferRingSlot>=0, mode B), so UploadBidiFrame / RecordBidiDecode
       // are shared by all submit modes.
       function ActiveDataBuffer:TpvVulkanBuffer;
       function ActiveOffsetBuffer(const aPlane:TpvInt32):TpvVulkanBuffer;
       function ActiveStepBuffer(const aPlane:TpvInt32):TpvVulkanBuffer;
       function ActiveTileCodesBuffer:TpvVulkanBuffer; // AQ apply: the tile-codes buffer matching the active step buffer (shared vs ring slot)
       function ActiveSetApplyAQ(const aPlane:TpvInt32):TpvVulkanDescriptorSet; // AQ apply set bound to the active step buffer (shared vs ring slot)
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
       function ActiveSetMCPlay(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
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
       procedure RecordSpatial3D(const aCommandBuffer:TpvVulkanCommandBuffer;const aBuf,aSlot:TpvInt32); // spatial inverse -> gop[buf][slot]
       procedure RecordTemporal3D(const aCommandBuffer:TpvVulkanCommandBuffer;const aBuf,aGOPCount:TpvInt32); // open-loop temporal inverse over the GOP buffer
       procedure DecodeMCTFInverse(const aBuf,aGOPCount:TpvInt32); // MCTF MC-Haar temporal inverse on the prefetch CB (self-submits per pair, async)
       procedure PrefetchWait; // wait + reset the prefetch fence (the previous async prefetch step finished reading the shared buffers)
       procedure PrefetchFinishGop(const aBuf,aGOPCount:TpvInt32); // temporal / MCTF inverse of a prefetched GOP (async, last submit left pending)
       procedure PrepareFrame3D(const aDisplayIndex:TpvInt32); // GOP-prefetch orchestration: GOP 0 up-front, one subband per frame, swap
       procedure RecordDisplay3D(const aCommandBuffer:TpvVulkanCommandBuffer;const aBuf,aSlot:TpvInt32); // gop[buf][slot] -> coeff -> [round] -> color
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
       // Reset the decode-ahead / DPB bookkeeping so the next PrepareFrame replays from frame 0 (seek-to-start without
       // recreating the decoder). Frame 0 is intra, so the reference chain rebuilds cleanly from there.
       procedure ResetForReplay;
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
       property HasAlpha:boolean read fHasAlpha; // color_flags bit2: the output image's A channel carries a decoded alpha plane
       property AlphaPremultiplied:boolean read fAlphaPremultiplied; // color_flags bit3: the RGB is premultiplied by alpha
      end;

implementation

{ TpvFlexibleWaveletVideoDecoder }

function TpvFlexibleWaveletVideoDecoder.PlaneWidth(const aPlane:TpvInt32):TpvInt32;
var Shift:TpvInt32;
begin
 if (aPlane=0) or (aPlane=3) or (fChromaFormat=0) then begin
  Shift:=0; // luma + alpha (plane 3) full res; chroma_shift_x: 4:2:2 / 4:2:0 halve horizontally, 4:4:4 does not
 end else begin
  Shift:=1;
 end;
 result:=(fWidth+((1 shl Shift)-1)) shr Shift; // ceil(width / 2^shift)
end;

function TpvFlexibleWaveletVideoDecoder.PlaneHeight(const aPlane:TpvInt32):TpvInt32;
var Shift:TpvInt32;
begin
 if (aPlane=0) or (aPlane=3) or (fChromaFormat<>2) then begin
  Shift:=0; // luma + alpha (plane 3) full res; chroma_shift_y: only 4:2:0 halves vertically
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

 // The 142-byte packed header (ReadBuffer-compatible, little-endian)
 fStream.ReadBuffer(fHeader,SizeOf(fHeader));
 if (CompareByte(fHeader.Magic[0],Magic[0],4)<>0) or (fHeader.Version<>FormatVersion) then begin
  raise EpvFlexibleWaveletVideoDecoder.Create('Not a FWVC stream');
 end;

 // Older containers predate the appended qpmap fields (HeaderSize < the current record): the 16 over-read bytes are
 // index/payload, not header, so ignore them and leave AQ off for those streams. Every later read seeks to an absolute
 // on-disk offset (which was read correctly from the leading fields), so the over-read does not affect anything else.
 if fHeader.HeaderSize<SizeOf(fHeader) then begin
  fHeader.QPMapOffset:=0;
  fHeader.QPMapSize:=0;
 end;

 // The coding-order frame index
 fFrameCount:=TpvInt64(fHeader.FrameCount);
 SetLength(fFrameEntries,fHeader.FrameCount);
 if fHeader.FrameCount>0 then begin
  fStream.Position:=TpvInt64(fHeader.IndexOffset);
  fStream.ReadBuffer(fFrameEntries[0],Int64(fHeader.FrameCount)*SizeOf(TFrameEntry));
 end;

 // Validate the (untrusted) index before any entry is used as an array index, a stream offset, or a POC-distance
 // divisor: POC in range, Ref0/Ref1 are -1 or a valid coding index, the frame data stays within the stream, and a
 // bidirectional frame's two references have distinct POCs (the blend weight divides by their POC distance).
 for BFrameIndex:=0 to fFrameCount-1 do begin
  if TpvInt64(fFrameEntries[BFrameIndex].POC)>=fFrameCount then begin
   raise EpvFlexibleWaveletVideoDecoder.Create('Corrupt frame index (POC out of range)');
  end;
  if (fFrameEntries[BFrameIndex].Ref0<>-1) and ((fFrameEntries[BFrameIndex].Ref0<0) or (fFrameEntries[BFrameIndex].Ref0>=fFrameCount)) then begin
   raise EpvFlexibleWaveletVideoDecoder.Create('Corrupt frame index (Ref0 out of range)');
  end;
  if (fFrameEntries[BFrameIndex].Ref1<>-1) and ((fFrameEntries[BFrameIndex].Ref1<0) or (fFrameEntries[BFrameIndex].Ref1>=fFrameCount)) then begin
   raise EpvFlexibleWaveletVideoDecoder.Create('Corrupt frame index (Ref1 out of range)');
  end;
  if (TpvUInt64(fFrameEntries[BFrameIndex].Offset)>TpvUInt64(fStream.Size)) or
     (TpvUInt64(fFrameEntries[BFrameIndex].Size)>(TpvUInt64(fStream.Size)-TpvUInt64(fFrameEntries[BFrameIndex].Offset))) then begin
   raise EpvFlexibleWaveletVideoDecoder.Create('Corrupt frame index (frame past end of stream)');
  end;
  if ((fFrameEntries[BFrameIndex].Ref0<>-1) and (fFrameEntries[BFrameIndex].Ref1<>-1)) and
     (fFrameEntries[fFrameEntries[BFrameIndex].Ref0].POC=fFrameEntries[fFrameEntries[BFrameIndex].Ref1].POC) then begin
   raise EpvFlexibleWaveletVideoDecoder.Create('Corrupt frame index (zero-distance B references)');
  end;
 end;

 // Derive the decode parameters
 fWidth:=TpvInt32(fHeader.Width);
 fHeight:=TpvInt32(fHeader.Height);
 fLevels:=TpvInt32(fHeader.Levels);
 fQuality:=TpvInt32(fHeader.Quality);
 fChromaFormat:=fHeader.ChromaFormat;
 fNumPlanes:=3; // stays 3 unless a has-alpha header sets it; non-alpha streams are byte-for-byte unchanged
 fPredictionMethod:=fHeader.PredictionMethod;
 fGOP:=fHeader.GOP;

 // AQ (per-tile QP): read the per-frame per-tile QP-map section once. The decode then applies map[coding index] to the
 // step before each dequant (SetAQCurrentMap), identically to the encoder, so the round trip is exact. The map is
 // normalised, so the same per-frame map fits every plane (luma full-res, chroma subsampled).
 fAQCols:=AQTileCols(fWidth);
 fAQRows:=AQTileRows(fHeight);
 fAQMapBytes:=fAQCols*fAQRows;
 fAQCurrentMap:=nil;
 fAQEnabled:=fHeader.QPMapSize>0;
 if fAQEnabled then begin
  SetLength(fAQMaps,fHeader.QPMapSize);
  fStream.Position:=TpvInt64(fHeader.QPMapOffset);
  fStream.ReadBuffer(fAQMaps[0],TpvInt64(fHeader.QPMapSize));
 end;

 // motion config: mv entropy coder (bit0), sub-pel motion_mode (bits1-2), motion block size + variable flag.
 // Old files have MVCodec=0 -> coder=golomb, motion_mode=0 (bilinear + half-pel) = exactly how they were encoded.
 fMVCodec:=fHeader.MVCodec and 1;
 fMotionMode:=(fHeader.MVCodec shr 1) and 7;   // bits0-1 = interpolation filter (0 bilinear, 1 6-tap, 2 8-tap DCTIF), bit2 = quarter-pel; mirrors the MOTION_MODE shader spec constant
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
 // HDR signalling: color_flags bit0 = HDR (12-bit BT.2020, PQ/HLG). HDR scales the reference white to 4096
 // (Q x16) and selects the HDR color shader; the SDR fallback path tonemaps to sRGB8 (exposure default 100).
 fIsHDR:=(fHeader.ColorFlags and 1)<>0;
 // Optional alpha: color_flags bit2 = an 8-bit alpha section appended per frame (bit3 = the RGB is premultiplied).
 // A color-only decoder ignores the section (it sits past the color payload within FrameEntry.Size).
 fHasAlpha:=(fHeader.ColorFlags and 4)<>0;
 fAlphaPremultiplied:=(fHeader.ColorFlags and 8)<>0;
 fAlphaQP:=0;
 fAlphaLossless:=true;
 fAlphaRingSize:=4; // MaxInFlightFrames (3) + 1 -> frame N's alpha slot is not reused until N+4, by when N's decode is done
 fAlphaRingCursor:=0;
 fAlphaCurrentSlot:=0;
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
 // otherwise SDR R8G8B8A8 (HDR streams then tonemap to sRGB8 via the SDR-fallback color shader).
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
 f3DInitialized:=false; // GOP 0 decoded on the first PrepareFrame3D
 f3DPfPending:=false;
 f3DCurBuf:=0;
 f3DCurGopCount:=0;
 f3DPfBuf:=1;
 f3DPfGopStart:=-1;
 f3DPfGopCount:=0;
 f3DPfStep:=0;
 f3DPfDone:=true;
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

 // Intra / I-P (not B-frames, not 3D-DWT) records its decode into the CALLER command buffer, which the engine submits
 // and pipelines. With a single shared set of input buffers, Update stages frame N+1 (UploadFrame overwrites them)
 // before the engine waits on frame N's GPU decode -> a clobber-while-reading race. Give this path its own input ring,
 // cycled one slot per displayed frame (see PrepareFrame / RecordFrame), so each in-flight frame reads its own slot.
 fIPInputRing:=(not fHasBFrames) and (not fMode3DDWT);
 fIPRingSlot:=0;
 fPreparedRingSlot:=-1;
 if fIPInputRing then begin
  fBufferRingSize:=4; // MaxInFlightFrames (3) + 1: frame N's slot is not reused until frame N+4, by when N's decode is done
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
    // mc.comp bakes the motion block size into spec constant 0 (MB) and the sub-pel mode into spec 3 (MOTION_MODE)
    SpecValues[0]:=fMotionBlock;
    SpecValues[1]:=fMotionMode;
    Stage.AddSpecializationMapEntry(0,0,SizeOf(TpvInt32));
    Stage.AddSpecializationMapEntry(3,SizeOf(TpvInt32),SizeOf(TpvInt32));
    Stage.AddSpecializationDataFromMemory(@SpecValues[0],SizeOf(TpvInt32)*2,true);
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
 if fAQEnabled then begin
  fDSL4:=CreateDescriptorSetLayout(4,false); // AQ apply: base step, tile codes, weight LUT, modulated step
 end;
 fDSLColor:=CreateDescriptorSetLayout(3,true);

 // color_alpha layout: bindings 0,1,2 = coeff buffers, 3 = output image, 4 = alpha buffer (the image is NOT last, so
 // this can't go through CreateDescriptorSetLayout — it always appends the image after the buffers; build it inline).
 if fHasAlpha then begin
  fDSLColorAlpha:=TpvVulkanDescriptorSetLayout.Create(fDevice);
  fDSLColorAlpha.AddBinding(0,VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);
  fDSLColorAlpha.AddBinding(1,VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);
  fDSLColorAlpha.AddBinding(2,VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);
  fDSLColorAlpha.AddBinding(3,VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);
  fDSLColorAlpha.AddBinding(4,VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);
  fDSLColorAlpha.Initialize;
 end;

 // pipeline layouts (push-constant sizes match the C shaders)
 fPLUnpack:=CreatePipelineLayout(fDSL3,16);
 fPLDequant:=CreatePipelineLayout(fDSL2,8);
 if fAQEnabled then begin
  fPLApplyAQ:=CreatePipelineLayout(fDSL4,20); // push {width,height,levels,tile_cols,tile_rows}
 end;
 fPLTranspose:=CreatePipelineLayout(fDSL2,16);
 fPLRow:=CreatePipelineLayout(fDSL1,16);
 fPLRound:=CreatePipelineLayout(fDSL1,4);
 fPLCoeffAdd:=CreatePipelineLayout(fDSL2,8); // coefdiff (A) P-frame coeff_add: {coeff, previous}, push [pixel_count, is_predicted]
 fPLColor:=CreatePipelineLayout(fDSLColor,24);
 if fHasAlpha then begin
  fPLColorAlpha:=CreatePipelineLayout(fDSLColorAlpha,24); // same 24-byte push as fPipeColor
 end;
 fPLColorHDR:=CreatePipelineLayout(fDSLColor,32);
 if fHasAlpha then begin
  fPLColorHDRAlpha:=CreatePipelineLayout(fDSLColorAlpha,32); // the HDR 32-byte push on the 5-binding alpha layout
 end;

 // intra-decode compute pipelines from the embedded SPIR-V
 fPipeUnpack:=CreateComputePipeline(FlexibleWaveletVideoBitplaneUnpackSPIRVData,FlexibleWaveletVideoBitplaneUnpackSPIRVDataSize,fPLUnpack,true);
 fPipeDequant:=CreateComputePipeline(FlexibleWaveletVideoDequant97SPIRVData,FlexibleWaveletVideoDequant97SPIRVDataSize,fPLDequant,false);
 if fAQEnabled then begin
  fPipeApplyAQ:=CreateComputePipeline(FlexibleWaveletVideoApplyTileAqSPIRVData,FlexibleWaveletVideoApplyTileAqSPIRVDataSize,fPLApplyAQ,false);
 end;
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
 fPipeColor:=CreateComputePipeline(FlexibleWaveletVideoColorSPIRVData,FlexibleWaveletVideoColorSPIRVDataSize,fPLColor,false);
 if fHasAlpha then begin
  fPipeColorAlpha:=CreateComputePipeline(FlexibleWaveletVideoColorAlphaSPIRVData,FlexibleWaveletVideoColorAlphaSPIRVDataSize,fPLColorAlpha,false);
 end;
 fPipeColorHDR:=CreateComputePipeline(FlexibleWaveletVideoColorHdrSPIRVData,FlexibleWaveletVideoColorHdrSPIRVDataSize,fPLColorHDR,false);
 fPipeColorHDRSCRGB:=CreateComputePipeline(FlexibleWaveletVideoColorHdrScrgbSPIRVData,FlexibleWaveletVideoColorHdrScrgbSPIRVDataSize,fPLColorHDR,false);
 if fHasAlpha then begin
  fPipeColorHDRAlpha:=CreateComputePipeline(FlexibleWaveletVideoColorHdrAlphaSPIRVData,FlexibleWaveletVideoColorHdrAlphaSPIRVDataSize,fPLColorHDRAlpha,false);
  fPipeColorHDRSCRGBAlpha:=CreateComputePipeline(FlexibleWaveletVideoColorHdrScrgbAlphaSPIRVData,FlexibleWaveletVideoColorHdrScrgbAlphaSPIRVDataSize,fPLColorHDRAlpha,false);
 end;

end;

procedure TpvFlexibleWaveletVideoDecoder.BuildBuffersAndImage;
var Plane,SlotIndex:TpvInt32;
    DataCapacity,PlaneBytes,ScratchSide:TVkDeviceSize;
    LumaBlockCount:TpvInt32;
    MemoryRequirements:TVkMemoryRequirements;
    RequiresDedicated,PrefersDedicated:boolean;
    MemoryBlockFlags:TpvVulkanDeviceMemoryBlockFlags;
    ImageHandle:TVkImage;
    AQWeightPointer:PpvFloatArray; // AQ: the weight LUT host mapping while filling it
    AQLogSpan:TpvFloat;
    AQCodeIndex:TpvInt32;
begin

 // host-visible payload buffer (bitplane data + per-block offsets) + per-plane host-visible offset buffers
 LumaBlockCount:=BlockCountX(fWidth)*BlockCountY(fHeight);
 DataCapacity:=(TVkDeviceSize(fWidth)*TVkDeviceSize(fHeight)*4)+(TVkDeviceSize(LumaBlockCount)*16);
 fDataBuffer:=CreateStorageBuffer(DataCapacity,false,'FWV.data');

 // per-plane working buffers
 for Plane:=0 to fNumPlanes-1 do begin
  PlaneBytes:=TVkDeviceSize(PlaneWidth(Plane))*TVkDeviceSize(PlaneHeight(Plane))*4;
  fOffsetBuffer[Plane]:=CreateStorageBuffer(TVkDeviceSize(LumaBlockCount)*4,false,'FWV.offset');
  fStepBuffer[Plane]:=CreateStorageBuffer(PlaneBytes,false,'FWV.step');
  fCoeffBuffer[Plane]:=CreateStorageBuffer(PlaneBytes,true,'FWV.coeff');
  fPreviousBuffer[Plane]:=CreateStorageBuffer(PlaneBytes,true,'FWV.previous'); // P-frame reference, GPU-resident across frames
 end;

 // AQ (GPU): the one-time weight LUT (code -> log-spaced weight, IDENTICAL to ApplyTileAQ / aq_weight_from_code) + the
 // per-frame raw qpmap tile codes, so apply_tile_aq.comp modulates the base step on the GPU (no per-pixel CPU work).
 if fAQEnabled then begin
  fWeightLUTBuffer:=CreateStorageBuffer(256*SizeOf(TpvFloat),false,'FWV.aqlut');
  AQLogSpan:=Ln(TpvFlexibleWaveletVideo.AQWeightMax/TpvFlexibleWaveletVideo.AQWeightMin);
  AQWeightPointer:=PpvFloatArray(fWeightLUTBuffer.Memory.MapMemory);
  try
   for AQCodeIndex:=0 to 255 do begin
    AQWeightPointer^[AQCodeIndex]:=TpvFlexibleWaveletVideo.AQWeightMin*Exp((TpvFloat(AQCodeIndex)/255.0)*AQLogSpan);
   end;
  finally
   fWeightLUTBuffer.Memory.UnmapMemory;
  end;
  // tile codes: rounded up to a multiple of 4 so apply_tile_aq's packed-uint reads (4 codes/uint) stay in bounds
  fTileCodesBuffer:=CreateStorageBuffer(TVkDeviceSize(((fAQMapBytes+3) div 4)*4),false,'FWV.aqcodes');
 end;

 // optional alpha plane (index 3): a full-res (luma-sized) intra plane. Its own data buffer keeps the alpha block
 // offsets prefix-sums-from-0 (independent of where the color data lives), so the GPU alpha decode is uniform for
 // every mode (I/P, B, 3D-DWT, MCTF). No previous-buffer: the alpha is intra, never predicted.
 if fHasAlpha then begin
  PlaneBytes:=TVkDeviceSize(fWidth)*TVkDeviceSize(fHeight)*4;
  fCoeffBuffer[3]:=CreateStorageBuffer(PlaneBytes,true,'FWV.alphacoeff'); // decoded alpha plane (device-local, GPU-written -> shared, like coeff[0..2])
  SetLength(fAlphaRingData,fAlphaRingSize);
  SetLength(fAlphaRingOffset,fAlphaRingSize);
  SetLength(fAlphaRingStep,fAlphaRingSize);
  for SlotIndex:=0 to fAlphaRingSize-1 do begin // host-visible input ring (one set of buffers per in-flight frame slot)
   fAlphaRingData[SlotIndex]:=CreateStorageBuffer(DataCapacity,false,'FWV.alphadata');
   fAlphaRingOffset[SlotIndex]:=CreateStorageBuffer(TVkDeviceSize(LumaBlockCount)*4,false,'FWV.alphaoffset');
   fAlphaRingStep[SlotIndex]:=CreateStorageBuffer(PlaneBytes,false,'FWV.alphastep');
  end;
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
   for Plane:=0 to fNumPlanes-1 do begin
    PlaneBytes:=TVkDeviceSize(PlaneWidth(Plane))*TVkDeviceSize(PlaneHeight(Plane))*4;
    fDPBBuffer[SlotIndex][Plane]:=CreateStorageBuffer(PlaneBytes,true,'FWV.dpb');
   end;
  end;
  for Plane:=0 to fNumPlanes-1 do begin
   PlaneBytes:=TVkDeviceSize(PlaneWidth(Plane))*TVkDeviceSize(PlaneHeight(Plane))*4;
   fGMCBuffer[0][Plane]:=CreateStorageBuffer(PlaneBytes,true,'FWV.gmc0');
   fGMCBuffer[1][Plane]:=CreateStorageBuffer(PlaneBytes,true,'FWV.gmc1');
  end;
 end;

 // 3D-DWT: the whole GOP (gop_capacity frames per plane, contiguous slots), DOUBLE-buffered (present vs prefetch),
 // device-local; plus a prefetch coeff buffer (one plane) so the prefetch spatial inverse never touches fCoeffBuffer.
 if fMode3DDWT then begin
  for Plane:=0 to fNumPlanes-1 do begin
   PlaneBytes:=TVkDeviceSize(PlaneWidth(Plane))*TVkDeviceSize(PlaneHeight(Plane))*4;
   fGopBuffer[0][Plane]:=CreateStorageBuffer(TVkDeviceSize(fGOPCapacity)*PlaneBytes,true,'FWV.gop0');
   fGopBuffer[1][Plane]:=CreateStorageBuffer(TVkDeviceSize(fGOPCapacity)*PlaneBytes,true,'FWV.gop1');
   fPrefetchCoeff[Plane]:=CreateStorageBuffer(PlaneBytes,true,'FWV.pfcoeff');
  end;
  // MCTF: the per-pair MC prediction + the per-level interleaved-frame workspace + the GOP's luma MV fields
  if fMCTF then begin
   for Plane:=0 to fNumPlanes-1 do begin
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
var Plane,MaxSets,MaxBuffers,SlotIndex:TpvInt32;
begin

 MaxSets:=64;
 MaxBuffers:=256;
 if fHasAlpha then begin // alpha: per-slot unpack+dequant ring sets (fAlphaRingSize*2) + 4 shared sets (coeff<->scratch/row + color_alpha)
  MaxSets:=MaxSets+(fAlphaRingSize*2)+6;
  MaxBuffers:=MaxBuffers+(fAlphaRingSize*5)+12;
 end;
 if (fHasBFrames and (fSubmitMode=1)) or fIPInputRing then begin // the per-frame input ring (~24 sets / ~80 buffers per slot)
  MaxSets:=MaxSets+(fBufferRingSize*30);
  MaxBuffers:=MaxBuffers+(fBufferRingSize*96);
 end;
 if fMode3DDWT then begin // + the GOP-prefetch spatial sets (5/plane) + the second gop buffer's temporal sets
  MaxSets:=MaxSets+24;
  MaxBuffers:=MaxBuffers+48;
 end;
 if fAQEnabled then begin // apply_tile_aq sets: 3 shared (I/P + 3D-DWT prefetch) + 3 per B-frame ring slot, 4 buffers each
  MaxSets:=MaxSets+3+(fBufferRingSize*3);
  MaxBuffers:=MaxBuffers+12+(fBufferRingSize*12);
 end;
 fDescriptorPool:=TpvVulkanDescriptorPool.Create(fDevice,TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),MaxSets);
 fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,MaxBuffers);
 fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,3); // fSetColor + (optional) fSetColorAlpha
 fDescriptorPool.Initialize;

 // per-plane sets: unpack (data, offset, coeff), dequant (coeff, step), the two transpose directions, the row pass
 for Plane:=0 to fNumPlanes-1 do begin

  fSetUnpack[Plane]:=AllocateSet(fDSL3);
  BindStorageBuffer(fSetUnpack[Plane],0,fDataBuffer);
  BindStorageBuffer(fSetUnpack[Plane],1,fOffsetBuffer[Plane]);
  BindStorageBuffer(fSetUnpack[Plane],2,fCoeffBuffer[Plane]);
  fSetUnpack[Plane].Flush;

  fSetDequant[Plane]:=AllocateSet(fDSL2);
  BindStorageBuffer(fSetDequant[Plane],0,fCoeffBuffer[Plane]);
  BindStorageBuffer(fSetDequant[Plane],1,fStepBuffer[Plane]);
  fSetDequant[Plane].Flush;

  if fAQEnabled then begin // AQ apply (I/P + 3D-DWT prefetch): base + tile codes + weight LUT -> the same step buffer (in-place)
   fSetApplyAQ[Plane]:=AllocateSet(fDSL4);
   BindStorageBuffer(fSetApplyAQ[Plane],0,fStepBuffer[Plane]);   // base step (the per-frame copy of the cached base lands here)
   BindStorageBuffer(fSetApplyAQ[Plane],1,fTileCodesBuffer);
   BindStorageBuffer(fSetApplyAQ[Plane],2,fWeightLUTBuffer);
   BindStorageBuffer(fSetApplyAQ[Plane],3,fStepBuffer[Plane]);   // modulated step the dequant reads (same buffer, in-place)
   fSetApplyAQ[Plane].Flush;
  end;

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

 // the scratch-buffer row pass, and the color set (3 coeff planes + the output image)
 fSetRowScratch:=AllocateSet(fDSL1);
 BindStorageBuffer(fSetRowScratch,0,fScratchBuffer);
 fSetRowScratch.Flush;

 fSetColor:=AllocateSet(fDSLColor);
 BindStorageBuffer(fSetColor,0,fCoeffBuffer[0]);
 BindStorageBuffer(fSetColor,1,fCoeffBuffer[1]);
 BindStorageBuffer(fSetColor,2,fCoeffBuffer[2]);
 BindStorageImage(fSetColor,3);
 fSetColor.Flush;

 // alpha plane-3 decode sets (same pipelines as the color planes, but the unpack reads the SEPARATE alpha data
 // buffer) + the color_alpha set (coeff0..2 + image + coeff3=alpha). The alpha is intra: no add / mc / motion_add sets.
 if fHasAlpha then begin
  // unpack + dequant sets are PER RING SLOT (they bind the per-slot host input buffers); the rest bind only the
  // shared coeff[3] / scratch / image, so one shared set each suffices.
  SetLength(fAlphaRingSetUnpack,fAlphaRingSize);
  SetLength(fAlphaRingSetDequant,fAlphaRingSize);
  for SlotIndex:=0 to fAlphaRingSize-1 do begin
   fAlphaRingSetUnpack[SlotIndex]:=AllocateSet(fDSL3);
   BindStorageBuffer(fAlphaRingSetUnpack[SlotIndex],0,fAlphaRingData[SlotIndex]);
   BindStorageBuffer(fAlphaRingSetUnpack[SlotIndex],1,fAlphaRingOffset[SlotIndex]);
   BindStorageBuffer(fAlphaRingSetUnpack[SlotIndex],2,fCoeffBuffer[3]);
   fAlphaRingSetUnpack[SlotIndex].Flush;

   fAlphaRingSetDequant[SlotIndex]:=AllocateSet(fDSL2);
   BindStorageBuffer(fAlphaRingSetDequant[SlotIndex],0,fCoeffBuffer[3]);
   BindStorageBuffer(fAlphaRingSetDequant[SlotIndex],1,fAlphaRingStep[SlotIndex]);
   fAlphaRingSetDequant[SlotIndex].Flush;
  end;

  fSetCoeffToScratch[3]:=AllocateSet(fDSL2);
  BindStorageBuffer(fSetCoeffToScratch[3],0,fCoeffBuffer[3]);
  BindStorageBuffer(fSetCoeffToScratch[3],1,fScratchBuffer);
  fSetCoeffToScratch[3].Flush;

  fSetScratchToCoeff[3]:=AllocateSet(fDSL2);
  BindStorageBuffer(fSetScratchToCoeff[3],0,fScratchBuffer);
  BindStorageBuffer(fSetScratchToCoeff[3],1,fCoeffBuffer[3]);
  fSetScratchToCoeff[3].Flush;

  fSetRow[3]:=AllocateSet(fDSL1);
  BindStorageBuffer(fSetRow[3],0,fCoeffBuffer[3]);
  fSetRow[3].Flush;

  fSetColorAlpha:=AllocateSet(fDSLColorAlpha);
  BindStorageBuffer(fSetColorAlpha,0,fCoeffBuffer[0]);
  BindStorageBuffer(fSetColorAlpha,1,fCoeffBuffer[1]);
  BindStorageBuffer(fSetColorAlpha,2,fCoeffBuffer[2]);
  BindStorageImage(fSetColorAlpha,3);
  BindStorageBuffer(fSetColorAlpha,4,fCoeffBuffer[3]);
  fSetColorAlpha.Flush;
 end;

 // hierarchical B-frames: the 5 per-plane sets are allocated once but REWRITTEN per decode-ahead frame (their
 // DPB ref slots change), so they are bound in DecodeAheadFrame, not here.
 if fHasBFrames then begin
  for Plane:=0 to fNumPlanes-1 do begin
   fSetGMC0[Plane]:=AllocateSet(fDSL3);
   fSetGMC1[Plane]:=AllocateSet(fDSL3);
   fSetGBlend[Plane]:=AllocateSet(fDSL3);
   fSetGAdd[Plane]:=AllocateSet(fDSL3);
   if fHasPerBlockMode then begin
    fSetGBlendMode[Plane]:=AllocateSet(fDSL3);
   end;
  end;
 end;

 // 3D-DWT temporal set: {gop_buffer[buf][plane]}, one per GOP buffer; MCTF mc/add sets are rebound per pair (with byte
 // offsets). Plus the GOP-prefetch spatial sets, identical to the present's spatial sets but bound to fPrefetchCoeff
 // (so the prefetch's spatial inverse never touches fCoeffBuffer, which the present's display copy + color use).
 if fMode3DDWT then begin
  for Plane:=0 to fNumPlanes-1 do begin
   fSetTemporal[0][Plane]:=AllocateSet(fDSL1);
   BindStorageBuffer(fSetTemporal[0][Plane],0,fGopBuffer[0][Plane]);
   fSetTemporal[0][Plane].Flush;
   fSetTemporal[1][Plane]:=AllocateSet(fDSL1);
   BindStorageBuffer(fSetTemporal[1][Plane],0,fGopBuffer[1][Plane]);
   fSetTemporal[1][Plane].Flush;

   fSetUnpackPF[Plane]:=AllocateSet(fDSL3);
   BindStorageBuffer(fSetUnpackPF[Plane],0,fDataBuffer);
   BindStorageBuffer(fSetUnpackPF[Plane],1,fOffsetBuffer[Plane]);
   BindStorageBuffer(fSetUnpackPF[Plane],2,fPrefetchCoeff[Plane]);
   fSetUnpackPF[Plane].Flush;

   fSetDequantPF[Plane]:=AllocateSet(fDSL2);
   BindStorageBuffer(fSetDequantPF[Plane],0,fPrefetchCoeff[Plane]);
   BindStorageBuffer(fSetDequantPF[Plane],1,fStepBuffer[Plane]);
   fSetDequantPF[Plane].Flush;

   fSetCoeffToScratchPF[Plane]:=AllocateSet(fDSL2);
   BindStorageBuffer(fSetCoeffToScratchPF[Plane],0,fPrefetchCoeff[Plane]);
   BindStorageBuffer(fSetCoeffToScratchPF[Plane],1,fScratchBuffer);
   fSetCoeffToScratchPF[Plane].Flush;

   fSetScratchToCoeffPF[Plane]:=AllocateSet(fDSL2);
   BindStorageBuffer(fSetScratchToCoeffPF[Plane],0,fScratchBuffer);
   BindStorageBuffer(fSetScratchToCoeffPF[Plane],1,fPrefetchCoeff[Plane]);
   fSetScratchToCoeffPF[Plane].Flush;

   fSetRowPF[Plane]:=AllocateSet(fDSL1);
   BindStorageBuffer(fSetRowPF[Plane],0,fPrefetchCoeff[Plane]);
   fSetRowPF[Plane].Flush;

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
 SetLength(fStepCacheData,(Slot+1)*fNumPlanes);
 fStepCacheQuality[Slot]:=aQuality;
 for Plane:=0 to fNumPlanes-1 do begin
  PlanePixels:=PlaneWidth(Plane)*PlaneHeight(Plane);
  SetLength(fStepCacheData[(Slot*fNumPlanes)+Plane],PlanePixels);
  BuildQuantizationSteps(PpvInt32Array(@fStepCacheData[(Slot*fNumPlanes)+Plane][0]),PlaneWidth(Plane),PlaneHeight(Plane),fLevels,aQuality,fSampleWhite,fHFGain,fLLGain);
 end;

 result:=Slot;
end;

procedure TpvFlexibleWaveletVideoDecoder.SetAQCurrentMap(const aCodingIndex:TpvInt32);
begin
 if fAQEnabled and ((aCodingIndex>=0) and (((aCodingIndex+1)*fAQMapBytes)<=length(fAQMaps))) then begin
  fAQCurrentMap:=PpvUInt8Array(@fAQMaps[aCodingIndex*fAQMapBytes]);
 end else begin
  fAQCurrentMap:=nil;
 end;
end;

// AQ (GPU): upload the current frame's raw qpmap tile codes; apply_tile_aq.comp then modulates the base step (copied
// into the step buffer each frame) IN PLACE on the GPU, so there is no per-pixel CPU work (the old ApplyAQ is gone).
procedure TpvFlexibleWaveletVideoDecoder.UploadTileCodes;
var DataPointer:PpvUInt8Array;
    TileCodesBuffer:TpvVulkanBuffer;
begin
 if fAQEnabled and assigned(fAQCurrentMap) then begin
  TileCodesBuffer:=ActiveTileCodesBuffer; // shared (I/P, 3D-DWT) or the active B-frame ring slot's, matching the step buffer
  DataPointer:=PpvUInt8Array(TileCodesBuffer.Memory.MapMemory);
  try
   Move(fAQCurrentMap^[0],DataPointer^[0],TpvSizeUInt(fAQMapBytes));
  finally
   TileCodesBuffer.Memory.UnmapMemory;
  end;
 end;
end;

// The apply_tile_aq set bound to the ACTIVE step buffer (shared fStepBuffer when fBufferRingSlot<0, else the B ring
// slot's). base and modulated are the SAME buffer (in-place) — safe because each invocation only touches its own gid
// and the base is freshly copied into the step buffer each frame, so there is no cross-frame accumulation.
function TpvFlexibleWaveletVideoDecoder.ActiveSetApplyAQ(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
begin
 if fBufferRingSlot<0 then begin
  result:=fSetApplyAQ[aPlane];
 end else begin
  result:=fRingSetApplyAQ[fBufferRingSlot][aPlane];
 end;
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
 SetAQCurrentMap(aFrameIndex); // AQ: this coding frame's per-tile QP map (no-op when AQ off)
 UploadTileCodes; // AQ (GPU): stage the raw tile codes for apply_tile_aq.comp

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
  for Plane:=0 to fNumPlanes-1 do begin
   PlanePixels:=PlaneWidth(Plane)*PlaneHeight(Plane);
   DataPointer:=PpvUInt8Array(ActiveStepBuffer(Plane).Memory.MapMemory);
   try
    Move(fStepCacheData[(StepSlot*fNumPlanes)+Plane][0],DataPointer^[0],TpvSizeUInt(PlanePixels)*4);
    // AQ: the base step lands here; the GPU apply_tile_aq pass (recorded below) modulates it in place (no CPU work)
   finally
    ActiveStepBuffer(Plane).Memory.UnmapMemory;
   end;
  end;
 end;

 // Per-plane block counts for the size-table prefix sum (4:4:4 -> all equal to the luma count); the offsets
 // are prefix-summed into the CPU scratch (the GPU offset buffers are filled from it below).
 for Plane:=0 to fNumPlanes-1 do begin
  BlockCount[Plane]:=BlockCountX(PlaneWidth(Plane))*BlockCountY(PlaneHeight(Plane));
  if TpvSizeUInt(Length(fOffsetScratch[Plane]))<TpvSizeUInt(BlockCount[Plane]) then begin
   SetLength(fOffsetScratch[Plane],BlockCount[Plane]);
  end;
  Offsets[Plane]:=PpvUInt32Array(@fOffsetScratch[Plane][0]);
 end;
 if not ParseFrameHeader(PpvUInt8Array(@fFrameScratch[0]),RawLength,BlockCount,Offsets,LeadingBlockCount,MVDataOffset,MVLength,BlockDataOffset) then begin
  raise EpvFlexibleWaveletVideoDecoder.Create('Corrupt frame header');
 end;

 // Upload the per-plane offset tables, then the packed bitplane bytes (data_length is the u32 right before
 // the block data). The host-visible buffers are pooled into shared memory chunks where only one map per
 // chunk may be live at a time, so each buffer is mapped / copied / unmapped on its own.
 for Plane:=0 to fNumPlanes-1 do begin
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

 // optional alpha (I/P colordiff/coefdiff path): the appended alpha section sits right after the color block data,
 // already in fFrameScratch -> stage it for RecordAlphaDecode (no re-read; the color and alpha are the same frame).
 if fHasAlpha then begin
  UploadAlphaFromBuffer(PpvUInt8Array(@fFrameScratch[0]),RawLength,BlockDataOffset+DataLength);
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
    ColorPush:array[0..5] of TpvInt32;
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

 // The output image goes UNDEFINED -> GENERAL so the color shader can store into it.
 RecordImageBarrier(aCommandBuffer,
                    VK_IMAGE_LAYOUT_UNDEFINED,VK_IMAGE_LAYOUT_GENERAL,
                    0,TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT));

 for Plane:=0 to fNumPlanes-1 do begin

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
  RecordDispatch(aCommandBuffer,fPipeUnpack,fPLUnpack,ActiveSetUnpack(Plane),@UnpackPush[0],16,PlaneUnpackWorkgroups,1,1);
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
   if fAQEnabled then begin // GPU AQ: modulate the base step (in the step buffer) by this frame's tile map, in place, before dequant
    fAQPush[0]:=PlaneWidth(Plane);
    fAQPush[1]:=PlaneHeight(Plane);
    fAQPush[2]:=fLevels;
    fAQPush[3]:=fAQCols;
    fAQPush[4]:=fAQRows;
    RecordDispatch(aCommandBuffer,fPipeApplyAQ,fPLApplyAQ,ActiveSetApplyAQ(Plane),@fAQPush[0],20,PlanePixelWorkgroups,1,1);
    RecordComputeBarrier(aCommandBuffer);
   end;
   RecordDispatch(aCommandBuffer,fPipeDequant,fPLDequant,ActiveSetDequant(Plane),@DequantPush[0],8,PlanePixelWorkgroups,1,1);
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
    RecordDispatch(aCommandBuffer,fPipeMC,fPLUnpack,ActiveSetMCPlay(Plane),@MCPush[0],12,PlanePixelWorkgroups,1,1);
    RecordComputeBarrier(aCommandBuffer);
   end;
   AddPush[0]:=PlanePixels;
   AddPush[1]:=Ord(aIsPredicted);
   RecordDispatch(aCommandBuffer,fPipeMotionAdd,fPLUnpack,fSetMotionAddPlay[Plane],@AddPush[0],8,PlanePixelWorkgroups,1,1);
   RecordComputeBarrier(aCommandBuffer);
  end;

 end;

 // optional alpha: GPU-decode the intra alpha plane into coeff[3] (the color pass below then writes it into output A).
 if fHasAlpha then begin
  RecordAlphaDecode(aCommandBuffer);
 end;

 // color: YCoCg(-R) -> RGB into the output image. Chroma upsample params: shift + the stored Co/Cg dims
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
  if fHasAlpha then begin // HDR + alpha: write the decoded alpha plane into the HDR / scRGB swapchain A (fSetColorAlpha)
   if fUseSCRGB then begin
    RecordDispatch(aCommandBuffer,fPipeColorHDRSCRGBAlpha,fPLColorHDRAlpha,fSetColorAlpha,@HDRPush[0],32,PixelWorkgroups,1,1);
   end else begin
    RecordDispatch(aCommandBuffer,fPipeColorHDRAlpha,fPLColorHDRAlpha,fSetColorAlpha,@HDRPush[0],32,PixelWorkgroups,1,1);
   end;
  end else begin
   if fUseSCRGB then begin
    RecordDispatch(aCommandBuffer,fPipeColorHDRSCRGB,fPLColorHDR,fSetColor,@HDRPush[0],32,PixelWorkgroups,1,1);
   end else begin
    RecordDispatch(aCommandBuffer,fPipeColorHDR,fPLColorHDR,fSetColor,@HDRPush[0],32,PixelWorkgroups,1,1);
   end;
  end;
 end else begin
  ColorPush[0]:=fWidth;
  ColorPush[1]:=fHeight;
  ColorPush[2]:=ChromaShiftX;
  ColorPush[3]:=ChromaShiftY;
  ColorPush[4]:=PlaneWidth(1);
  ColorPush[5]:=PlaneHeight(1);
  if fHasAlpha then begin
   RecordDispatch(aCommandBuffer,fPipeColorAlpha,fPLColorAlpha,fSetColorAlpha,@ColorPush[0],24,PixelWorkgroups,1,1);
  end else begin
   RecordDispatch(aCommandBuffer,fPipeColor,fPLColor,fSetColor,@ColorPush[0],24,PixelWorkgroups,1,1);
  end;
 end;

 // Hand the decoded image to the transfer stage for present / readback.
 RecordImageBarrier(aCommandBuffer,
                    VK_IMAGE_LAYOUT_GENERAL,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                    TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT));

end;

function TpvFlexibleWaveletVideoDecoder.ParseAlphaSection(const aFrameBuffer:PpvUInt8Array;const aFrameLength:TpvSizeUInt;const aSectionOffset:TpvSizeUInt;const aBlockCount:TpvInt32;out aAlphaQP:TpvInt32;out aAlphaDataOffset:TpvSizeUInt;out aAlphaDataLength:TpvUInt32):boolean;
var Cursor:TpvSizeUInt;
    SizeBlobLength,Running:TpvUInt32;
    SizeReader:TBitReader;
    Block:TpvInt32;
begin

 result:=false;
 aAlphaQP:=0;
 aAlphaDataOffset:=0;
 aAlphaDataLength:=0;

 // The appended section (mirrors C parse_alpha_section): [u8 alpha_qp][u32 size_blob_length][size blob: unsigned
 // Exp-Golomb per-block sizes][u32 alpha_data_length][alpha block data]. The alpha is full-res, so block_count is the
 // luma block count. The per-block sizes prefix-sum into the alpha offset table (fAlphaOffsetScratch). Returns False
 // (without dereferencing past the frame) if any field would read beyond aFrameLength.
 if (aSectionOffset+5)>aFrameLength then begin // alpha_qp (1) + size_blob_length (4)
  exit;
 end;
 Cursor:=aSectionOffset;
 aAlphaQP:=aFrameBuffer^[Cursor];
 inc(Cursor,1);

 SizeBlobLength:=ReadU32LE(aFrameBuffer,Cursor);
 inc(Cursor,4);
 if (Cursor+TpvSizeUInt(SizeBlobLength))>aFrameLength then begin
  exit;
 end;
 if TpvSizeUInt(Length(fAlphaOffsetScratch))<TpvSizeUInt(aBlockCount) then begin
  SetLength(fAlphaOffsetScratch,aBlockCount);
 end;
 SizeReader.Init(PpvUInt8Array(@aFrameBuffer^[Cursor]),SizeBlobLength);
 Running:=0;
 for Block:=0 to aBlockCount-1 do begin
  fAlphaOffsetScratch[Block]:=Running;
  inc(Running,SizeReader.GetUnsignedExpGolomb);
 end;
 inc(Cursor,SizeBlobLength);

 if (Cursor+4)>aFrameLength then begin
  exit;
 end;
 aAlphaDataLength:=ReadU32LE(aFrameBuffer,Cursor);
 inc(Cursor,4);
 aAlphaDataOffset:=Cursor;
 if (Cursor+TpvSizeUInt(aAlphaDataLength))>aFrameLength then begin // the alpha block data itself must fit
  exit;
 end;

 result:=true;

end;

procedure TpvFlexibleWaveletVideoDecoder.UploadAlphaFromBuffer(const aFrameBuffer:PpvUInt8Array;const aFrameLength:TpvSizeUInt;const aSectionOffset:TpvSizeUInt);
var BlockCount,StepSlot:TpvInt32;
    AlphaDataOffset:TpvSizeUInt;
    AlphaDataLength:TpvUInt32;
    DataPointer:PpvUInt8Array;
begin

 // pick the next free-running ring slot so this frame's host buffers are NOT the ones an in-flight earlier frame's
 // GPU alpha decode may still be reading (the color I/P input ring does the same). RecordAlphaDecode reads back the
 // captured fAlphaCurrentSlot. Upload and Record are paired CPU-sequentially (Update then Draw), so the capture is safe.
 fAlphaCurrentSlot:=fAlphaRingCursor;
 inc(fAlphaRingCursor);
 if fAlphaRingCursor>=fAlphaRingSize then begin
  fAlphaRingCursor:=0;
 end;

 BlockCount:=BlockCountX(fWidth)*BlockCountY(fHeight); // alpha is full-res -> the luma block count
 if not ParseAlphaSection(aFrameBuffer,aFrameLength,aSectionOffset,BlockCount,fAlphaQP,AlphaDataOffset,AlphaDataLength) then begin
  raise EpvFlexibleWaveletVideoDecoder.Create('Corrupt alpha section');
 end;
 fAlphaLossless:=fAlphaQP=0;

 // alpha block offsets -> this slot's offset buffer
 DataPointer:=PpvUInt8Array(fAlphaRingOffset[fAlphaCurrentSlot].Memory.MapMemory);
 try
  Move(fAlphaOffsetScratch[0],DataPointer^[0],TpvSizeUInt(BlockCount)*4);
 finally
  fAlphaRingOffset[fAlphaCurrentSlot].Memory.UnmapMemory;
 end;

 // alpha packed bitplane bytes -> this slot's data buffer (offsets are prefix-sums-from-0)
 DataPointer:=PpvUInt8Array(fAlphaRingData[fAlphaCurrentSlot].Memory.MapMemory);
 try
  Move(aFrameBuffer^[AlphaDataOffset],DataPointer^[0],AlphaDataLength);
 finally
  fAlphaRingData[fAlphaCurrentSlot].Memory.UnmapMemory;
 end;

 // lossy alpha: the quant step map for alpha_qp. The alpha is full-res, so it equals the LUMA (plane 0) step map for
 // quality = alpha_qp; reuse the per-quality step cache (a distinct alpha_qp just adds its own cache slot).
 if not fAlphaLossless then begin
  if not fGainsComputed then begin
   MeasureSynthesisGains(fLevels,fHFGain,fLLGain);
   fGainsComputed:=true;
  end;
  StepSlot:=EnsureStepCacheSlot(fAlphaQP);
  DataPointer:=PpvUInt8Array(fAlphaRingStep[fAlphaCurrentSlot].Memory.MapMemory);
  try
   Move(fStepCacheData[(StepSlot*fNumPlanes)+0][0],DataPointer^[0],TpvSizeUInt(fWidth)*TpvSizeUInt(fHeight)*4);
   // NOTE: the alpha plane is intentionally NOT AQ-modulated — the encoder quantises it with plain alpha_qp steps
   // (step_map[3] is built once, before any per-frame tile map exists, and is never rebuilt under --aq), so the
   // decode must dequantise alpha with the plain steps too. Applying the tile map here would mismatch the encoder.
  finally
   fAlphaRingStep[fAlphaCurrentSlot].Memory.UnmapMemory;
  end;
 end;

end;

procedure TpvFlexibleWaveletVideoDecoder.UploadAlphaForDisplayedFrame(const aCodingIndex:TpvInt32);
var Entry:PFrameEntry;
    CompressedLength:TpvSizeUInt;
    RawLength,DataLength,MVLength:TpvUInt32;
    BlockCount:TBlockCounts;
    Offsets:TPlaneOffsets;
    Plane,LeadingBlockCount:TpvInt32;
    MVDataOffset,BlockDataOffset:TpvSizeUInt;
begin

 // B / 3D-DWT: the displayed frame's color was reconstructed earlier (from a DPB slot / GOP buffer), so its payload
 // is NOT in fFrameScratch. Re-read + decompress THAT frame into the alpha scratch (kept off fFrameScratch, which may
 // hold an in-flight color frame under engine pipelining), parse the color header only to locate the appended alpha
 // section (= color block-data end), then stage it like the colordiff path.
 Entry:=@fFrameEntries[aCodingIndex];
 CompressedLength:=Entry^.Size;
 if TpvSizeUInt(Length(fAlphaCompressedScratch))<(CompressedLength+8) then begin
  SetLength(fAlphaCompressedScratch,CompressedLength+8);
 end;
 fStream.Position:=TpvInt64(Entry^.Offset);
 fStream.ReadBuffer(fAlphaCompressedScratch[0],CompressedLength);
 RawLength:=ReadU32LE(PpvUInt8Array(@fAlphaCompressedScratch[0]),1);
 if TpvSizeUInt(Length(fAlphaFrameScratch))<RawLength then begin
  SetLength(fAlphaFrameScratch,RawLength);
 end;
 if not DecompressFrame(PpvUInt8Array(@fAlphaCompressedScratch[0]),CompressedLength,PpvUInt8Array(@fAlphaFrameScratch[0]),TpvSizeUInt(Length(fAlphaFrameScratch)),RawLength) then begin
  raise EpvFlexibleWaveletVideoDecoder.Create('Alpha frame decompression failed');
 end;

 for Plane:=0 to fNumPlanes-1 do begin
  BlockCount[Plane]:=BlockCountX(PlaneWidth(Plane))*BlockCountY(PlaneHeight(Plane));
  if TpvSizeUInt(Length(fOffsetScratch[Plane]))<TpvSizeUInt(BlockCount[Plane]) then begin
   SetLength(fOffsetScratch[Plane],BlockCount[Plane]);
  end;
  Offsets[Plane]:=PpvUInt32Array(@fOffsetScratch[Plane][0]);
 end;
 if not ParseFrameHeader(PpvUInt8Array(@fAlphaFrameScratch[0]),RawLength,BlockCount,Offsets,LeadingBlockCount,MVDataOffset,MVLength,BlockDataOffset) then begin
  raise EpvFlexibleWaveletVideoDecoder.Create('Corrupt frame header');
 end;
 DataLength:=ReadU32LE(PpvUInt8Array(@fAlphaFrameScratch[0]),BlockDataOffset-4);

 UploadAlphaFromBuffer(PpvUInt8Array(@fAlphaFrameScratch[0]),RawLength,BlockDataOffset+DataLength);

end;

procedure TpvFlexibleWaveletVideoDecoder.RecordAlphaDecode(const aCommandBuffer:TpvVulkanCommandBuffer);
var Level,LevelCount:TpvInt32;
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
begin

 // The alpha is intra + full-res (luma dims): the same per-plane decode as a color plane (unpack -> dequant -> iDWT
 // -> round) into coeff[3], but WITHOUT any predictive pass, and the lossless choice follows the alpha's OWN quant
 // (fAlphaLossless), not the color stream's fLossless. The unpack set reads the SEPARATE alpha data buffer.
 if fAlphaLossless then begin
  RowPipeline:=fPipeIDWT53; // reversible 5/3
 end else begin
  RowPipeline:=fPipeIDWT97; // irreversible 9/7
 end;

 PlaneW:=fWidth;
 PlaneH:=fHeight;
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

 // unpack: packed alpha bytes + per-block offsets -> coefficients
 UnpackPush[0]:=PlaneW;
 UnpackPush[1]:=PlaneH;
 UnpackPush[2]:=PlaneBlocksX;
 UnpackPush[3]:=PlaneBlocksY;
 RecordDispatch(aCommandBuffer,fPipeUnpack,fPLUnpack,fAlphaRingSetUnpack[fAlphaCurrentSlot],@UnpackPush[0],16,PlaneUnpackWorkgroups,1,1);
 RecordComputeBarrier(aCommandBuffer);

 // lossy: dequantize (single luma-like plane -> chroma multiplier 1.0)
 if not fAlphaLossless then begin
  DequantPush[0]:=PlanePixels;
  ChromaMultiplier:=1.0;
  DequantPush[1]:=PpvInt32(@ChromaMultiplier)^;
  RecordDispatch(aCommandBuffer,fPipeDequant,fPLDequant,fAlphaRingSetDequant[fAlphaCurrentSlot],@DequantPush[0],8,PlanePixelWorkgroups,1,1);
  RecordComputeBarrier(aCommandBuffer);
 end;

 // inverse 2D wavelet, coarsest level first (the level pyramid is the full-res pyramid)
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
  RecordDispatch(aCommandBuffer,fPipeTranspose,fPLTranspose,fSetCoeffToScratch[3],@TransposePush1[0],16,(LevelW+15) div 16,(LevelH+15) div 16,1);
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
  RecordDispatch(aCommandBuffer,fPipeTranspose,fPLTranspose,fSetScratchToCoeff[3],@TransposePush2[0],16,(LevelH+15) div 16,(LevelW+15) div 16,1);
  RecordComputeBarrier(aCommandBuffer);

  RowPush2[0]:=PlaneW;
  RowPush2[1]:=LevelW;
  RowPush2[2]:=LevelH;
  RowPush2[3]:=1;
  RecordDispatch(aCommandBuffer,RowPipeline,fPLRow,fSetRow[3],@RowPush2[0],16,LevelH,1,1);
  RecordComputeBarrier(aCommandBuffer);
 end;

 // lossy: round the float-scaled coefficients back to integer alpha bytes
 if not fAlphaLossless then begin
  PixelCountPush:=PlanePixels;
  RecordDispatch(aCommandBuffer,fPipeRound,fPLRound,fSetRow[3],@PixelCountPush,4,PlanePixelWorkgroups,1,1);
  RecordComputeBarrier(aCommandBuffer);
 end;

end;

function TpvFlexibleWaveletVideoDecoder.ActiveDataBuffer:TpvVulkanBuffer;
begin
 if fBufferRingSlot<0 then begin
  result:=fDataBuffer;
 end else begin
  result:=fRingDataBuffer[fBufferRingSlot];
 end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveOffsetBuffer(const aPlane:TpvInt32):TpvVulkanBuffer;
begin
 if fBufferRingSlot<0 then begin
  result:=fOffsetBuffer[aPlane];
 end else begin
  result:=fRingOffsetBuffer[fBufferRingSlot][aPlane];
 end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveStepBuffer(const aPlane:TpvInt32):TpvVulkanBuffer;
begin
 if fBufferRingSlot<0 then begin
  result:=fStepBuffer[aPlane];
 end else begin
  result:=fRingStepBuffer[fBufferRingSlot][aPlane];
 end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveTileCodesBuffer:TpvVulkanBuffer;
begin
 if fBufferRingSlot<0 then begin
  result:=fTileCodesBuffer;
 end else begin
  result:=fRingTileCodesBuffer[fBufferRingSlot];
 end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveMVBuffer:TpvVulkanBuffer;
begin
 if fBufferRingSlot<0 then begin
  result:=fMVBuffer;
 end else begin
  result:=fRingMVBuffer[fBufferRingSlot];
 end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveMV1Buffer:TpvVulkanBuffer;
begin
 if fBufferRingSlot<0 then begin
  result:=fMV1Buffer;
 end else begin
  result:=fRingMV1Buffer[fBufferRingSlot];
 end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveModeBuffer:TpvVulkanBuffer;
begin
 if fBufferRingSlot<0 then begin
  result:=fModeBuffer;
 end else begin
  result:=fRingModeBuffer[fBufferRingSlot];
 end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveSetUnpack(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
begin
 if fBufferRingSlot<0 then begin
  result:=fSetUnpack[aPlane];
 end else begin
  result:=fRingSetUnpack[fBufferRingSlot][aPlane];
 end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveSetDequant(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
begin
 if fBufferRingSlot<0 then begin
  result:=fSetDequant[aPlane];
 end else begin
  result:=fRingSetDequant[fBufferRingSlot][aPlane];
 end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveSetGMC0(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
begin
 if fBufferRingSlot<0 then begin
  result:=fSetGMC0[aPlane];
 end else begin
  result:=fRingSetGMC0[fBufferRingSlot][aPlane];
 end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveSetGMC1(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
begin
 if fBufferRingSlot<0 then begin
  result:=fSetGMC1[aPlane];
 end else begin
  result:=fRingSetGMC1[fBufferRingSlot][aPlane];
 end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveSetGBlend(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
begin
 if fBufferRingSlot<0 then begin
  result:=fSetGBlend[aPlane];
 end else begin
  result:=fRingSetGBlend[fBufferRingSlot][aPlane];
 end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveSetGBlendMode(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
begin
 if fBufferRingSlot<0 then begin
  result:=fSetGBlendMode[aPlane];
 end else begin
  result:=fRingSetGBlendMode[fBufferRingSlot][aPlane];
 end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveSetGAdd(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
begin
 if fBufferRingSlot<0 then begin
  result:=fSetGAdd[aPlane];
 end else begin
  result:=fRingSetGAdd[fBufferRingSlot][aPlane];
 end;
end;

function TpvFlexibleWaveletVideoDecoder.ActiveSetMCPlay(const aPlane:TpvInt32):TpvVulkanDescriptorSet;
begin
 if fBufferRingSlot<0 then begin
  result:=fSetMCPlay[aPlane];
 end else begin
  result:=fRingSetMCPlay[fBufferRingSlot][aPlane];
 end;
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
 SetLength(fRingTileCodesBuffer,fBufferRingSize);
 SetLength(fRingMVBuffer,fBufferRingSize);
 SetLength(fRingMV1Buffer,fBufferRingSize);
 SetLength(fRingModeBuffer,fBufferRingSize);
 SetLength(fRingSetUnpack,fBufferRingSize);
 SetLength(fRingSetDequant,fBufferRingSize);
 SetLength(fRingSetApplyAQ,fBufferRingSize); // AQ apply set per ring slot (bound only when AQ is enabled)
 SetLength(fRingSetGMC0,fBufferRingSize);
 SetLength(fRingSetGMC1,fBufferRingSize);
 SetLength(fRingSetGBlend,fBufferRingSize);
 SetLength(fRingSetGBlendMode,fBufferRingSize);
 SetLength(fRingSetGAdd,fBufferRingSize);
 SetLength(fRingSetMCPlay,fBufferRingSize);
 for Slot:=0 to fBufferRingSize-1 do begin
  fRingDataBuffer[Slot]:=CreateStorageBuffer(DataCapacity,false,'FWV.ring.data');
  fRingMVBuffer[Slot]:=CreateStorageBuffer(TVkDeviceSize(MotionCells)*2*4,false,'FWV.ring.mv');
  fRingMV1Buffer[Slot]:=CreateStorageBuffer(TVkDeviceSize(MotionCells)*2*4,false,'FWV.ring.mv1');
  fRingModeBuffer[Slot]:=CreateStorageBuffer(TVkDeviceSize(MotionCells)*4,false,'FWV.ring.mode');
  if fAQEnabled then begin // AQ: per-slot tile codes (sized like the shared buffer, rounded up to a multiple of 4)
   fRingTileCodesBuffer[Slot]:=CreateStorageBuffer(TVkDeviceSize(((fAQMapBytes+3) div 4)*4),false,'FWV.ring.aqcodes');
  end;
  for Plane:=0 to fNumPlanes-1 do begin
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
   if fAQEnabled then begin // AQ apply: this ring slot's step buffer is base + modulated (in-place)
    fRingSetApplyAQ[Slot][Plane]:=AllocateSet(fDSL4);
    BindStorageBuffer(fRingSetApplyAQ[Slot][Plane],0,fRingStepBuffer[Slot][Plane]);
    BindStorageBuffer(fRingSetApplyAQ[Slot][Plane],1,fRingTileCodesBuffer[Slot]);
    BindStorageBuffer(fRingSetApplyAQ[Slot][Plane],2,fWeightLUTBuffer);
    BindStorageBuffer(fRingSetApplyAQ[Slot][Plane],3,fRingStepBuffer[Slot][Plane]);
    fRingSetApplyAQ[Slot][Plane].Flush;
   end;
   fRingSetGMC0[Slot][Plane]:=AllocateSet(fDSL3);
   fRingSetGMC1[Slot][Plane]:=AllocateSet(fDSL3);
   fRingSetGBlend[Slot][Plane]:=AllocateSet(fDSL3);
   fRingSetGAdd[Slot][Plane]:=AllocateSet(fDSL3);
   if fHasPerBlockMode then begin
    fRingSetGBlendMode[Slot][Plane]:=AllocateSet(fDSL3);
   end;
   // intra/I-P motion comp set, bound once: {shared previous, ring mv, shared scratch} (mirrors fSetMCPlay)
   fRingSetMCPlay[Slot][Plane]:=AllocateSet(fDSL3);
   BindStorageBuffer(fRingSetMCPlay[Slot][Plane],0,fPreviousBuffer[Plane]);
   BindStorageBuffer(fRingSetMCPlay[Slot][Plane],1,fRingMVBuffer[Slot]);
   BindStorageBuffer(fRingSetMCPlay[Slot][Plane],2,fScratchBuffer);
   fRingSetMCPlay[Slot][Plane].Flush;
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
 SetAQCurrentMap(aCodingIndex); // AQ: this coding frame's per-tile QP map (no-op when AQ off)
 UploadTileCodes; // AQ (GPU): stage the raw tile codes for apply_tile_aq.comp

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
  for Plane:=0 to fNumPlanes-1 do begin
   PlanePixels:=PlaneWidth(Plane)*PlaneHeight(Plane);
   DataPointer:=PpvUInt8Array(ActiveStepBuffer(Plane).Memory.MapMemory);
   try
    Move(fStepCacheData[(StepSlot*fNumPlanes)+Plane][0],DataPointer^[0],TpvSizeUInt(PlanePixels)*4);
    // AQ: the base step lands here; the GPU apply_tile_aq pass (recorded below) modulates it in place (no CPU work)
   finally
    ActiveStepBuffer(Plane).Memory.UnmapMemory;
   end;
  end;
 end;

 // Offsets + data upload.
 for Plane:=0 to fNumPlanes-1 do begin
  BlockCount[Plane]:=BlockCountX(PlaneWidth(Plane))*BlockCountY(PlaneHeight(Plane));
  if TpvSizeUInt(Length(fOffsetScratch[Plane]))<TpvSizeUInt(BlockCount[Plane]) then begin
   SetLength(fOffsetScratch[Plane],BlockCount[Plane]);
  end;
  Offsets[Plane]:=PpvUInt32Array(@fOffsetScratch[Plane][0]);
 end;
 if not ParseFrameHeader(PpvUInt8Array(@fFrameScratch[0]),RawLength,BlockCount,Offsets,LeadingBlockCount,MVDataOffset,MVLength,BlockDataOffset) then begin
  raise EpvFlexibleWaveletVideoDecoder.Create('Corrupt frame header');
 end;
 for Plane:=0 to fNumPlanes-1 do begin
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

 for Plane:=0 to fNumPlanes-1 do begin

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
   if fAQEnabled then begin // GPU AQ: modulate the base step (in the step buffer) by this frame's tile map, in place, before dequant
    fAQPush[0]:=PlaneWidth(Plane);
    fAQPush[1]:=PlaneHeight(Plane);
    fAQPush[2]:=fLevels;
    fAQPush[3]:=fAQCols;
    fAQPush[4]:=fAQRows;
    RecordDispatch(aCommandBuffer,fPipeApplyAQ,fPLApplyAQ,ActiveSetApplyAQ(Plane),@fAQPush[0],20,PlanePixelWorkgroups,1,1);
    RecordComputeBarrier(aCommandBuffer);
   end;
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
  for Plane:=0 to fNumPlanes-1 do begin
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
 for Plane:=0 to fNumPlanes-1 do begin
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
    ColorPush:array[0..5] of TpvInt32;
    HDRPush:array[0..7] of TpvInt32;
    ExposureBits:TpvFloat;
begin

 // copy the display POC's reconstructed YCoCg DPB slot into coeff, then color-convert into the output image
 DisplaySlot:=fGDPBPOCToSlot[aDisplayPOC];

 // optional alpha: re-read + stage the displayed frame's appended alpha section. Its container entry is the coding
 // index occupying this display POC's DPB slot (fGDPBSlotCoding[slot]) — the SAME map the color copy below uses.
 if fHasAlpha then begin
  UploadAlphaForDisplayedFrame(fGDPBSlotCoding[DisplaySlot]);
 end;

 RecordImageBarrier(aCommandBuffer,
                    VK_IMAGE_LAYOUT_UNDEFINED,VK_IMAGE_LAYOUT_GENERAL,
                    0,TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT));

 for Plane:=0 to fNumPlanes-1 do begin
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

 // optional alpha: GPU-decode the displayed frame's intra alpha plane into coeff[3] (staged above for this POC).
 if fHasAlpha then begin
  RecordAlphaDecode(aCommandBuffer);
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
  if fHasAlpha then begin // HDR + alpha: write the decoded alpha plane into the HDR / scRGB swapchain A (fSetColorAlpha)
   if fUseSCRGB then begin
    RecordDispatch(aCommandBuffer,fPipeColorHDRSCRGBAlpha,fPLColorHDRAlpha,fSetColorAlpha,@HDRPush[0],32,PixelWorkgroups,1,1);
   end else begin
    RecordDispatch(aCommandBuffer,fPipeColorHDRAlpha,fPLColorHDRAlpha,fSetColorAlpha,@HDRPush[0],32,PixelWorkgroups,1,1);
   end;
  end else begin
   if fUseSCRGB then begin
    RecordDispatch(aCommandBuffer,fPipeColorHDRSCRGB,fPLColorHDR,fSetColor,@HDRPush[0],32,PixelWorkgroups,1,1);
   end else begin
    RecordDispatch(aCommandBuffer,fPipeColorHDR,fPLColorHDR,fSetColor,@HDRPush[0],32,PixelWorkgroups,1,1);
   end;
  end;
 end else begin
  ColorPush[0]:=fWidth;
  ColorPush[1]:=fHeight;
  ColorPush[2]:=ChromaShiftX;
  ColorPush[3]:=ChromaShiftY;
  ColorPush[4]:=PlaneWidth(1);
  ColorPush[5]:=PlaneHeight(1);
  if fHasAlpha then begin
   RecordDispatch(aCommandBuffer,fPipeColorAlpha,fPLColorAlpha,fSetColorAlpha,@ColorPush[0],24,PixelWorkgroups,1,1);
  end else begin
   RecordDispatch(aCommandBuffer,fPipeColor,fPLColor,fSetColor,@ColorPush[0],24,PixelWorkgroups,1,1);
  end;
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
   fBufferRingSlot:=fBidiRingCursor; // free-running cursor (not RingIndex): disjoint slots across pipelined frames
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
   inc(fBidiRingCursor);
   if fBidiRingCursor>=fBufferRingSize then begin
    fBidiRingCursor:=0;
   end;
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
 SetAQCurrentMap(aCodingIndex); // AQ: this coding frame's per-tile QP map (no-op when AQ off)
 UploadTileCodes; // AQ (GPU): stage the raw tile codes for apply_tile_aq.comp

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
  for Plane:=0 to fNumPlanes-1 do begin
   PlanePixels:=PlaneWidth(Plane)*PlaneHeight(Plane);
   DataPointer:=PpvUInt8Array(fStepBuffer[Plane].Memory.MapMemory);
   try
    Move(fStepCacheData[(StepSlot*fNumPlanes)+Plane][0],DataPointer^[0],TpvSizeUInt(PlanePixels)*4);
    // AQ: the base step lands here; the GPU apply_tile_aq pass (recorded below) modulates it in place (no CPU work)
   finally
    fStepBuffer[Plane].Memory.UnmapMemory;
   end;
  end;
 end;

 // offsets + data upload
 for Plane:=0 to fNumPlanes-1 do begin
  BlockCount[Plane]:=BlockCountX(PlaneWidth(Plane))*BlockCountY(PlaneHeight(Plane));
  if TpvSizeUInt(Length(fOffsetScratch[Plane]))<TpvSizeUInt(BlockCount[Plane]) then begin
   SetLength(fOffsetScratch[Plane],BlockCount[Plane]);
  end;
  Offsets[Plane]:=PpvUInt32Array(@fOffsetScratch[Plane][0]);
 end;
 if not ParseFrameHeader(PpvUInt8Array(@fFrameScratch[0]),RawLength,BlockCount,Offsets,LeadingBlockCount,MVDataOffset,MVLength,BlockDataOffset) then begin
  raise EpvFlexibleWaveletVideoDecoder.Create('Corrupt frame header');
 end;
 for Plane:=0 to fNumPlanes-1 do begin
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

procedure TpvFlexibleWaveletVideoDecoder.RecordSpatial3D(const aCommandBuffer:TpvVulkanCommandBuffer;const aBuf,aSlot:TpvInt32);
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

 for Plane:=0 to fNumPlanes-1 do begin

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

  // spatial inverse: unpack -> [dequant] -> iDWT, into the PREFETCH coeff (so it never touches fCoeffBuffer, which the
  // present's display copy + color use) and the chosen GOP buffer; the scratch + row-scratch set are shared (the
  // present's display does not touch scratch).
  UnpackPush[0]:=PlaneW;
  UnpackPush[1]:=PlaneH;
  UnpackPush[2]:=PlaneBlocksX;
  UnpackPush[3]:=PlaneBlocksY;
  RecordDispatch(aCommandBuffer,fPipeUnpack,fPLUnpack,fSetUnpackPF[Plane],@UnpackPush[0],16,PlaneUnpackWorkgroups,1,1);
  RecordComputeBarrier(aCommandBuffer);

  if not fLossless then begin
   DequantPush[0]:=PlanePixels;
   if Plane=0 then begin
    ChromaMultiplier:=1.0;
   end else begin
    ChromaMultiplier:=fChromaQuant;
   end;
   DequantPush[1]:=PpvInt32(@ChromaMultiplier)^;
   if fAQEnabled then begin // GPU AQ: the prefetch decodes into fStepBuffer (fSetDequantPF), so modulate it in place via fSetApplyAQ
    fAQPush[0]:=PlaneWidth(Plane);
    fAQPush[1]:=PlaneHeight(Plane);
    fAQPush[2]:=fLevels;
    fAQPush[3]:=fAQCols;
    fAQPush[4]:=fAQRows;
    RecordDispatch(aCommandBuffer,fPipeApplyAQ,fPLApplyAQ,fSetApplyAQ[Plane],@fAQPush[0],20,PlanePixelWorkgroups,1,1);
    RecordComputeBarrier(aCommandBuffer);
   end;
   RecordDispatch(aCommandBuffer,fPipeDequant,fPLDequant,fSetDequantPF[Plane],@DequantPush[0],8,PlanePixelWorkgroups,1,1);
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
   RecordDispatch(aCommandBuffer,fPipeTranspose,fPLTranspose,fSetCoeffToScratchPF[Plane],@TransposePush1[0],16,(LevelW+15) div 16,(LevelH+15) div 16,1);
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
   RecordDispatch(aCommandBuffer,fPipeTranspose,fPLTranspose,fSetScratchToCoeffPF[Plane],@TransposePush2[0],16,(LevelH+15) div 16,(LevelW+15) div 16,1);
   RecordComputeBarrier(aCommandBuffer);
   RowPush2[0]:=PlaneW;
   RowPush2[1]:=LevelW;
   RowPush2[2]:=LevelH;
   RowPush2[3]:=1;
   RecordDispatch(aCommandBuffer,RowPipeline,fPLRow,fSetRowPF[Plane],@RowPush2[0],16,LevelH,1,1);
   RecordComputeBarrier(aCommandBuffer);
  end;

  // MCTF gop is integer: round the float 9/7 result before the integer MC-Haar inverse (open-loop stays float)
  if fMCTF and not fLossless then begin
   PixelCountPush:=PlanePixels;
   RecordDispatch(aCommandBuffer,fPipeRound,fPLRound,fSetRowPF[Plane],@PixelCountPush,4,PlanePixelWorkgroups,1,1);
   RecordComputeBarrier(aCommandBuffer);
  end;

  // hand the reconstructed plane to the transfer stage, then copy it into this frame's GOP slot of the chosen buffer
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
  aCommandBuffer.CmdCopyBuffer(fPrefetchCoeff[Plane].Handle,fGopBuffer[aBuf][Plane].Handle,1,@BufferCopy);

 end;

end;

procedure TpvFlexibleWaveletVideoDecoder.RecordTemporal3D(const aCommandBuffer:TpvVulkanCommandBuffer;const aBuf,aGOPCount:TpvInt32);
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

 for Plane:=0 to fNumPlanes-1 do begin
  PlanePixels:=PlaneWidth(Plane)*PlaneHeight(Plane);
  TemporalPush[0]:=PlanePixels;
  TemporalPush[1]:=aGOPCount;
  TemporalPush[2]:=fTemporalLevels;
  TemporalPush[3]:=Wavelet;
  TemporalPush[4]:=1; // inverse
  RecordDispatch(aCommandBuffer,Pipeline,fPLTemporal,fSetTemporal[aBuf][Plane],@TemporalPush[0],20,(PlanePixels+255) div 256,1,1);
  RecordComputeBarrier(aCommandBuffer);
 end;

end;

procedure TpvFlexibleWaveletVideoDecoder.DecodeMCTFInverse(const aBuf,aGOPCount:TpvInt32);
var LumaBlocks,Plane,Level,Count,Len,LevelLen,LowCount,k,Even,Odd:TpvInt32;
    PlaneW,PlaneH,PlanePP,PlaneMBX:array[0..3] of TpvInt32;
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
 for Plane:=0 to fNumPlanes-1 do begin
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

 // Each pair / back-copy submits ASYNC on the prefetch command buffer + fence; PrefetchWait (idempotent via f3DPfPending)
 // before reusing the CB / fMVBuffer waits the previous step, leaving the LAST submit pending for the caller (the GOP
 // swap) to wait on. The caller already drained the fence before calling, so the first PrefetchWait here is a no-op.
 for Level:=Count-1 downto 0 do begin
  LevelLen:=Lengths[Level];
  LowCount:=(LevelLen+1) div 2;
  for k:=0 to LowCount-1 do begin
   Even:=2*k;
   PrefetchWait;
   fPrefetchCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));
   if ((2*k)+1)<LevelLen then begin // pair: odd = high + OBMC(low)
    Odd:=(2*k)+1;
    DataPointer:=PpvUInt8Array(fMVBuffer.Memory.MapMemory); // this high-pass frame's luma MVs
    try
     Move(fMCTFMVScratch[(LowCount+k)*LumaBlocks*2],DataPointer^[0],TpvSizeUInt(LumaBlocks)*2*4);
    finally
     fMVBuffer.Memory.UnmapMemory;
    end;
    for Plane:=0 to fNumPlanes-1 do begin
     PlanePixels:=PlanePP[Plane];
     LowOff:=TVkDeviceSize(k)*PlanePixels*4;
     HighOff:=TVkDeviceSize(LowCount+k)*PlanePixels*4;
     EvenOff:=TVkDeviceSize(Even)*PlanePixels*4;
     OddOff:=TVkDeviceSize(Odd)*PlanePixels*4;
     // mc: warp gop@low(k) by this pair's MVs -> mctf_pred
     BindStorageBufferOffset(fSetMCTFMC[Plane],0,fGopBuffer[aBuf][Plane],LowOff,TVkDeviceSize(PlanePixels)*4);
     BindStorageBuffer(fSetMCTFMC[Plane],1,fMVBuffer);
     BindStorageBuffer(fSetMCTFMC[Plane],2,fMCTFPred[Plane]);
     fSetMCTFMC[Plane].Flush;
     MCPush[0]:=PlaneW[Plane];
     MCPush[1]:=PlaneH[Plane];
     MCPush[2]:=PlaneMBX[Plane];
     RecordDispatch(fPrefetchCommandBuffer,fPipeMC,fPLUnpack,fSetMCTFMC[Plane],@MCPush[0],12,(PlanePixels+255) div 256,1,1);
     // even = low passthrough -> scratch@even; high -> scratch@odd (coeff_add adds pred in place)
     FillChar(BufferCopy,SizeOf(BufferCopy),#0);
     BufferCopy.srcOffset:=LowOff;
     BufferCopy.dstOffset:=EvenOff;
     BufferCopy.size:=TVkDeviceSize(PlanePixels)*4;
     fPrefetchCommandBuffer.CmdCopyBuffer(fGopBuffer[aBuf][Plane].Handle,fMCTFScratch[Plane].Handle,1,@BufferCopy);
     BufferCopy.srcOffset:=HighOff;
     BufferCopy.dstOffset:=OddOff;
     fPrefetchCommandBuffer.CmdCopyBuffer(fGopBuffer[aBuf][Plane].Handle,fMCTFScratch[Plane].Handle,1,@BufferCopy);
     // mc (compute) + the two copies (transfer) -> coeff_add (compute)
     FillChar(Barrier,SizeOf(Barrier),#0);
     Barrier.sType:=VK_STRUCTURE_TYPE_MEMORY_BARRIER;
     Barrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT);
     Barrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
     fPrefetchCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                               0,1,@Barrier,0,nil,0,nil);
     // coeff_add: scratch@odd (= high) += pred -> odd
     BindStorageBufferOffset(fSetMCTFAdd[Plane],0,fMCTFScratch[Plane],OddOff,TVkDeviceSize(PlanePixels)*4);
     BindStorageBuffer(fSetMCTFAdd[Plane],1,fMCTFPred[Plane]);
     fSetMCTFAdd[Plane].Flush;
     AddPush[0]:=PlanePixels;
     AddPush[1]:=1;
     RecordDispatch(fPrefetchCommandBuffer,fPipeCoeffAdd,fPLCoeffAdd,fSetMCTFAdd[Plane],@AddPush[0],8,(PlanePixels+255) div 256,1,1);
    end;
   end else begin // odd tail (no partner): even = low passthrough -> scratch@even
    for Plane:=0 to fNumPlanes-1 do begin
     PlanePixels:=PlanePP[Plane];
     FillChar(BufferCopy,SizeOf(BufferCopy),#0);
     BufferCopy.srcOffset:=TVkDeviceSize(k)*PlanePixels*4;
     BufferCopy.dstOffset:=TVkDeviceSize(Even)*PlanePixels*4;
     BufferCopy.size:=TVkDeviceSize(PlanePixels)*4;
     fPrefetchCommandBuffer.CmdCopyBuffer(fGopBuffer[aBuf][Plane].Handle,fMCTFScratch[Plane].Handle,1,@BufferCopy);
    end;
   end;
   fPrefetchCommandBuffer.EndRecording;
   fPrefetchCommandBuffer.Execute(fDevice.UniversalQueue,TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),nil,nil,fPrefetchFence,false);
   f3DPfPending:=true;
  end;
  // copy scratch[0..level_len) back into gop_buffer (the interleaved frames for this level)
  PrefetchWait;
  fPrefetchCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));
  for Plane:=0 to fNumPlanes-1 do begin
   FillChar(BufferCopy,SizeOf(BufferCopy),#0);
   BufferCopy.srcOffset:=0;
   BufferCopy.dstOffset:=0;
   BufferCopy.size:=TVkDeviceSize(LevelLen)*TVkDeviceSize(PlanePP[Plane])*4;
   fPrefetchCommandBuffer.CmdCopyBuffer(fMCTFScratch[Plane].Handle,fGopBuffer[aBuf][Plane].Handle,1,@BufferCopy);
  end;
  fPrefetchCommandBuffer.EndRecording;
  fPrefetchCommandBuffer.Execute(fDevice.UniversalQueue,TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),nil,nil,fPrefetchFence,false);
  f3DPfPending:=true;
 end;

end;

procedure TpvFlexibleWaveletVideoDecoder.RecordDisplay3D(const aCommandBuffer:TpvVulkanCommandBuffer;const aBuf,aSlot:TpvInt32);
var Plane,PlanePixels,PixelWorkgroups:TpvInt32;
    BufferCopy:TVkBufferCopy;
    Barrier:TVkMemoryBarrier;
    PixelCountPush:TpvInt32;
    ColorPush:array[0..5] of TpvInt32;
    HDRPush:array[0..7] of TpvInt32;
    ExposureBits:TpvFloat;
begin

 RecordImageBarrier(aCommandBuffer,
                    VK_IMAGE_LAYOUT_UNDEFINED,VK_IMAGE_LAYOUT_GENERAL,
                    0,TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT));

 // copy this display frame's GOP slot into coeff, then (lossy open-loop only) round the float to int
 for Plane:=0 to fNumPlanes-1 do begin
  PlanePixels:=PlaneWidth(Plane)*PlaneHeight(Plane);
  FillChar(BufferCopy,SizeOf(BufferCopy),#0);
  BufferCopy.srcOffset:=TVkDeviceSize(aSlot)*TVkDeviceSize(PlanePixels)*4;
  BufferCopy.dstOffset:=0;
  BufferCopy.size:=TVkDeviceSize(PlanePixels)*4;
  aCommandBuffer.CmdCopyBuffer(fGopBuffer[aBuf][Plane].Handle,fCoeffBuffer[Plane].Handle,1,@BufferCopy);
 end;
 FillChar(Barrier,SizeOf(Barrier),#0);
 Barrier.sType:=VK_STRUCTURE_TYPE_MEMORY_BARRIER;
 Barrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT);
 Barrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,1,@Barrier,0,nil,0,nil);

 if (not fLossless) and (not fMCTF) then begin // open-loop lossy gop is float -> round to int before color
  for Plane:=0 to fNumPlanes-1 do begin
   PlanePixels:=PlaneWidth(Plane)*PlaneHeight(Plane);
   PixelCountPush:=PlanePixels;
   RecordDispatch(aCommandBuffer,fPipeRound,fPLRound,fSetRow[Plane],@PixelCountPush,4,(PlanePixels+255) div 256,1,1);
   RecordComputeBarrier(aCommandBuffer);
  end;
 end;

 // optional alpha: GPU-decode the displayed frame's intra alpha plane into coeff[3] (staged by PrepareFrame3D).
 if fHasAlpha then begin
  RecordAlphaDecode(aCommandBuffer);
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
  if fHasAlpha then begin // HDR + alpha: write the decoded alpha plane into the HDR / scRGB swapchain A (fSetColorAlpha)
   if fUseSCRGB then begin
    RecordDispatch(aCommandBuffer,fPipeColorHDRSCRGBAlpha,fPLColorHDRAlpha,fSetColorAlpha,@HDRPush[0],32,PixelWorkgroups,1,1);
   end else begin
    RecordDispatch(aCommandBuffer,fPipeColorHDRAlpha,fPLColorHDRAlpha,fSetColorAlpha,@HDRPush[0],32,PixelWorkgroups,1,1);
   end;
  end else begin
   if fUseSCRGB then begin
    RecordDispatch(aCommandBuffer,fPipeColorHDRSCRGB,fPLColorHDR,fSetColor,@HDRPush[0],32,PixelWorkgroups,1,1);
   end else begin
    RecordDispatch(aCommandBuffer,fPipeColorHDR,fPLColorHDR,fSetColor,@HDRPush[0],32,PixelWorkgroups,1,1);
   end;
  end;
 end else begin
  ColorPush[0]:=fWidth;
  ColorPush[1]:=fHeight;
  ColorPush[2]:=ChromaShiftX;
  ColorPush[3]:=ChromaShiftY;
  ColorPush[4]:=PlaneWidth(1);
  ColorPush[5]:=PlaneHeight(1);
  if fHasAlpha then begin
   RecordDispatch(aCommandBuffer,fPipeColorAlpha,fPLColorAlpha,fSetColorAlpha,@ColorPush[0],24,PixelWorkgroups,1,1);
  end else begin
   RecordDispatch(aCommandBuffer,fPipeColor,fPLColor,fSetColor,@ColorPush[0],24,PixelWorkgroups,1,1);
  end;
 end;

 RecordImageBarrier(aCommandBuffer,
                    VK_IMAGE_LAYOUT_GENERAL,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                    TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT));

end;

procedure TpvFlexibleWaveletVideoDecoder.PrefetchWait;
begin
 // idempotent: only wait+reset when a prefetch submit is actually in flight, so a wait is never issued on an
 // already-drained fence (which would block forever).
 if f3DPfPending then begin
  fPrefetchFence.WaitFor;
  fPrefetchFence.Reset;
  f3DPfPending:=false;
 end;
end;

procedure TpvFlexibleWaveletVideoDecoder.PrefetchFinishGop(const aBuf,aGOPCount:TpvInt32);
begin
 // temporal axis: MCTF MC-Haar inverse (multiple async submits, last left pending) or the open-loop temporal DWT (one
 // async submit). The caller drained the fence (last spatial done) before calling.
 if fMCTF then begin
  DecodeMCTFInverse(aBuf,aGOPCount);
 end else begin
  fPrefetchCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));
  RecordTemporal3D(fPrefetchCommandBuffer,aBuf,aGOPCount);
  fPrefetchCommandBuffer.EndRecording;
  fPrefetchCommandBuffer.Execute(fDevice.UniversalQueue,TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),nil,nil,fPrefetchFence,false);
  f3DPfPending:=true;
 end;
end;

// CPU side of the poll-API split for 3D-DWT/MCTF: the GOP-prefetch state machine (ported from fwvplay.c). The whole
// GOP is decoded one subband per displayed frame on fPrefetchCommandBuffer (overlapping the present) into the OTHER
// gop buffer; when the presented GOP runs out, the prefetched buffer is swapped in. No whole-GOP burst stalls a frame.
procedure TpvFlexibleWaveletVideoDecoder.PrepareFrame3D(const aDisplayIndex:TpvInt32);
 procedure SpatialStep(const aBuf,aGopStart,aSlot,aGopCount:TpvInt32); // upload + async spatial inverse of one subband
 begin
  PrefetchWait;
  Upload3DFrame(aGopStart+aSlot,aSlot,aGopCount);
  fPrefetchCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));
  RecordSpatial3D(fPrefetchCommandBuffer,aBuf,aSlot);
  fPrefetchCommandBuffer.EndRecording;
  fPrefetchCommandBuffer.Execute(fDevice.UniversalQueue,TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),nil,nil,fPrefetchFence,false);
  f3DPfPending:=true;
 end;
 procedure StartPrefetch(const aBuf,aGopStart:TpvInt32); // arm the prefetch state for the GOP after the current one
 begin
  f3DPfBuf:=aBuf;
  f3DPfGopStart:=aGopStart;
  if aGopStart<fFrameCount then begin
   f3DPfGopCount:=GopCountFrom(aGopStart);
   f3DPfStep:=0;
   f3DPfDone:=false;
  end else begin
   f3DPfDone:=true; // no further GOP to prefetch (end of stream)
  end;
 end;
var TargetGopStart,Slot:TpvInt32;
begin

 fBufferRingSlot:=-1; // 3D-DWT uses the shared input buffers

 // the GOP containing this display frame starts at the nearest preceding type-0 subband frame
 TargetGopStart:=aDisplayIndex;
 while (TargetGopStart>0) and (fFrameEntries[TargetGopStart].FrameType<>0) do begin
  dec(TargetGopStart);
 end;

 if (not f3DInitialized) or ((TargetGopStart<>fCur3DGopStart) and (TargetGopStart<>f3DPfGopStart)) then begin

  // start-up OR a seek to an unrelated GOP: decode it synchronously into buffer 0 (a one-off stall is fine here)
  f3DCurBuf:=0;
  fCur3DGopStart:=TargetGopStart;
  f3DCurGopCount:=GopCountFrom(TargetGopStart);
  for Slot:=0 to f3DCurGopCount-1 do begin
   SpatialStep(0,TargetGopStart,Slot,f3DCurGopCount);
  end;
  PrefetchWait; // last spatial done
  PrefetchFinishGop(0,f3DCurGopCount);
  PrefetchWait; // temporal/MCTF done -> the GOP is fully decoded
  f3DInitialized:=true;
  StartPrefetch(1,fCur3DGopStart+f3DCurGopCount);

 end else if (TargetGopStart=f3DPfGopStart) and (TargetGopStart<>fCur3DGopStart) then begin

  // advanced into the prefetched GOP: finish it if it lagged, then swap it in
  while not f3DPfDone do begin
   SpatialStep(f3DPfBuf,f3DPfGopStart,f3DPfStep,f3DPfGopCount);
   inc(f3DPfStep);
   if f3DPfStep>=f3DPfGopCount then begin
    PrefetchWait;
    PrefetchFinishGop(f3DPfBuf,f3DPfGopCount);
    f3DPfDone:=true;
   end;
  end;
  PrefetchWait; // the prefetched GOP's finish submit -> fully decoded before we present from it
  f3DCurBuf:=f3DPfBuf;
  fCur3DGopStart:=f3DPfGopStart;
  f3DCurGopCount:=f3DPfGopCount;
  StartPrefetch(1-f3DCurBuf,fCur3DGopStart+f3DCurGopCount);

 end;

 // advance the prefetch one subband per displayed frame (overlaps the present)
 if not f3DPfDone then begin
  SpatialStep(f3DPfBuf,f3DPfGopStart,f3DPfStep,f3DPfGopCount);
  inc(f3DPfStep);
  if f3DPfStep>=f3DPfGopCount then begin
   PrefetchWait;
   PrefetchFinishGop(f3DPfBuf,f3DPfGopCount); // async; the swap waits its completion
   f3DPfDone:=true;
  end;
 end;

 // optional alpha: re-read + stage the displayed frame's appended alpha section (3D-DWT container entries are
 // positional, so the display index IS the coding index). RecordDisplay3D's RecordAlphaDecode GPU-decodes it.
 if fHasAlpha then begin
  UploadAlphaForDisplayedFrame(aDisplayIndex);
 end;

end;

procedure TpvFlexibleWaveletVideoDecoder.DecodeFrame3D(const aCommandBuffer:TpvVulkanCommandBuffer;const aDisplayIndex:TpvInt32);
begin
 // combined (non-poll-API) path: prepare the GOP then record the display from the current buffer.
 PrepareFrame3D(aDisplayIndex);
 RecordDisplay3D(aCommandBuffer,f3DCurBuf,aDisplayIndex-fCur3DGopStart);
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
 RingIndex:=0; // per-call coding-frame counter; only guards that ONE display frame's decode-ahead fits the ring
 while fGCursor<fFrameCount do begin
  if (fGDPBPOCToSlot[aDisplayPOC]>=0) and (fGCursor>=(aDisplayPOC+fGDecodeLead)) then begin
   break;
  end;
  if RingIndex>=fBufferRingSize then begin
   raise EpvFlexibleWaveletVideoDecoder.Create('B-frame mode B input ring overflow');
  end;
  // Use the FREE-RUNNING cursor (not RingIndex) as the slot: it does not reset per display frame, so this frame's
  // input slots stay disjoint from the previous, still-in-flight frame's slots (the engine pipelines display frames,
  // and UploadBidiFrame here would otherwise clobber buffers the prior frame's GPU decode is still reading -> the pale
  // fade). The ring's +fGDecodePeriod margin over the lead-burst depth covers the in-flight window before a slot wraps.
  fBufferRingSlot:=fBidiRingCursor;
  if not PrepareBidiFrame(aDisplayPOC,IsPredicted,Ref1Slot,Weight0,Weight1) then begin
   break;
  end;
  fBidiPlan[fBidiPlanCount].RingSlot:=fBidiRingCursor;
  fBidiPlan[fBidiPlanCount].IsPredicted:=IsPredicted;
  fBidiPlan[fBidiPlanCount].Ref1Slot:=Ref1Slot;
  fBidiPlan[fBidiPlanCount].Weight0:=Weight0;
  fBidiPlan[fBidiPlanCount].Weight1:=Weight1;
  inc(fBidiPlanCount);
  inc(RingIndex);
  inc(fBidiRingCursor);
  if fBidiRingCursor>=fBufferRingSize then begin
   fBidiRingCursor:=0;
  end;
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
  // 3D-DWT/MCTF: run the GOP-prefetch state machine (decode-ahead on the prefetch CB); RecordFrame only copies+colors.
  PrepareFrame3D(aDisplayIndex);
 end else if fHasBFrames then begin
  PrepareFrameBidi(aDisplayIndex);
 end else begin
  if fIPInputRing then begin
   // cycle to a fresh ring slot so this frame's input buffers differ from those the previous (still in-flight) frames' GPU
   // decode is reading; RecordFrame restores this slot for the GPU side (Update and Draw are separate calls).
   fBufferRingSlot:=fIPRingSlot;
   inc(fIPRingSlot);
   if fIPRingSlot>=fBufferRingSize then begin
    fIPRingSlot:=0;
   end;
  end;
  fPreparedRingSlot:=fBufferRingSlot;
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
  // PrepareFrame3D already decoded/prefetched the GOP; just copy this frame's slot out of the current buffer + color.
  RecordDisplay3D(aCommandBuffer,f3DCurBuf,fPreparedIndex-fCur3DGopStart);
 end else if fHasBFrames then begin
  RecordFrameBidi(aCommandBuffer);
 end else begin
  fBufferRingSlot:=fPreparedRingSlot; // GPU side reads the same ring slot PrepareFrame's UploadFrame wrote (-1 = shared)
  RecordDecode(aCommandBuffer,fPreparedIsPredicted);
  fBufferRingSlot:=-1;
 end;
 fPreparedIndex:=-1;

end;

procedure TpvFlexibleWaveletVideoDecoder.ResetForReplay;
var Index:TpvInt32;
begin

 // Decode-ahead / DPB / 3D-GOP bookkeeping back to the start so the next PrepareFrame(0) replays cleanly from the
 // intra frame 0 (which needs no references). Reuses the same reset values as Create; the buffers are kept.
 fGCursor:=0;
 fGDecStepIndex:=-1;
 fBufferRingSlot:=-1;
 fIPRingSlot:=0;
 fPreparedRingSlot:=-1;
 fBidiRingCursor:=0;
 fAlphaRingCursor:=0; // free-running alpha host-buffer ring cursor
 fAlphaCurrentSlot:=0;
 fPreparedIndex:=-1;
 fBidiDisplayPOC:=-1;
 fCur3DGopStart:=-1;
 if fMode3DDWT then begin
  // drain any in-flight prefetch, then force GOP 0 to be re-decoded up-front on the next PrepareFrame3D
  PrefetchWait;
  f3DInitialized:=false;
  f3DCurBuf:=0;
  f3DCurGopCount:=0;
  f3DPfBuf:=1;
  f3DPfGopStart:=-1;
  f3DPfGopCount:=0;
  f3DPfStep:=0;
  f3DPfDone:=true;
 end;
 if fHasBFrames then begin
  for Index:=0 to fFrameCount-1 do begin
   fGDPBPOCToSlot[Index]:=-1;
   fGDPBCodingToSlot[Index]:=-1;
  end;
  for Index:=0 to fGDPBSlots-1 do begin
   fGDPBSlotCoding[Index]:=-1;
  end;
 end;

end;

constructor TpvFlexibleWaveletVideoDecoder.Create(const aStream:TStream;const aDevice:TpvVulkanDevice;const aPreferSCRGBForHDR:boolean;const aBSubmitMode:TpvInt32);
begin
 inherited Create;

 fPreparedIndex:=-1;

 fStream:=aStream;
 fDevice:=aDevice;
 fPreferSCRGB:=aPreferSCRGBForHDR; // consumed by ParseContainer (output format) below
 fSubmitMode:=aBSubmitMode;

 // GPU capability floor: the row IDWT shaders run 256 invocations and cache a full 4096-sample line (16 KiB) in shared
 // memory. Every desktop GPU clears this (Vulkan's guaranteed minimum is only 128 / 16 KiB), so fail early with a clear
 // message instead of a cryptic pipeline-creation crash on a strict-minimum mobile / iGPU.
 if (fDevice.PhysicalDevice.Properties.limits.maxComputeWorkGroupInvocations<256) or
    (fDevice.PhysicalDevice.Properties.limits.maxComputeSharedMemorySize<16384) then begin
  raise EpvFlexibleWaveletVideoDecoder.CreateFmt('GPU below the FWV decode floor: needs >=256 compute invocations and >=16 KiB shared memory (this device: %d invocations / %d bytes)',
                                                 [fDevice.PhysicalDevice.Properties.limits.maxComputeWorkGroupInvocations,
                                                  fDevice.PhysicalDevice.Properties.limits.maxComputeSharedMemorySize]);
 end;

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

 // 3D-DWT GOP prefetch: a SECOND command buffer + fence so the next GOP's subbands decode async (overlapping the
 // present) instead of stalling a single frame with the whole-GOP burst. The fence starts signaled (no prefetch in
 // flight yet) so the first wait-before-reuse is a no-op.
 if fMode3DDWT then begin
  fPrefetchCommandBuffer:=TpvVulkanCommandBuffer.Create(fBDecodeCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  fPrefetchFence:=TpvVulkanFence.Create(fDevice); // unsignaled; f3DPfPending gates the wait so it is never waited empty
 end;

 // the per-frame input ring: mode B (the whole decode-ahead in one caller CB) + the intra/I-P path (one slot per frame)
 if (fHasBFrames and (fSubmitMode=1)) or fIPInputRing then begin
  BuildBidiRing;
 end;

end;

destructor TpvFlexibleWaveletVideoDecoder.Destroy;
var Plane,SlotIndex:TpvInt32;
begin

 // Drain any in-flight 3D-DWT/MCTF GOP-prefetch or B-decode-ahead submit before freeing the fences, command buffers and
 // GPU resources they may still be reading (closing the player mid-prefetch otherwise frees a buffer under an active read).
 if assigned(fDevice) then begin
  fDevice.WaitIdle;
 end;

 FreeAndNil(fPrefetchFence);
 FreeAndNil(fPrefetchCommandBuffer); // allocated from fBDecodeCommandPool -> free before it
 FreeAndNil(fBDecodeFence);
 FreeAndNil(fBDecodeCommandBuffer);
 FreeAndNil(fBDecodeCommandPool);

 FreeAndNil(fSetColor);
 FreeAndNil(fSetColorAlpha);
 FreeAndNil(fSetRowScratch);
 // optional alpha decode sets (not covered by the 0..fNumPlanes-1 loop below): the per-slot unpack+dequant ring sets
 // plus the shared idwt sets bound to coeff[3].
 for SlotIndex:=0 to length(fAlphaRingSetUnpack)-1 do begin
  FreeAndNil(fAlphaRingSetUnpack[SlotIndex]);
  FreeAndNil(fAlphaRingSetDequant[SlotIndex]);
 end;
 FreeAndNil(fSetCoeffToScratch[3]);
 FreeAndNil(fSetScratchToCoeff[3]);
 FreeAndNil(fSetRow[3]);
 for Plane:=0 to fNumPlanes-1 do begin
  FreeAndNil(fSetUnpack[Plane]);
  FreeAndNil(fSetDequant[Plane]);
  FreeAndNil(fSetApplyAQ[Plane]);
  FreeAndNil(fSetAdd[Plane]);
  FreeAndNil(fSetMCPlay[Plane]);
  FreeAndNil(fSetMotionAddPlay[Plane]);
  FreeAndNil(fSetGMC0[Plane]);
  FreeAndNil(fSetGMC1[Plane]);
  FreeAndNil(fSetGBlend[Plane]);
  FreeAndNil(fSetGBlendMode[Plane]);
  FreeAndNil(fSetGAdd[Plane]);
  FreeAndNil(fSetTemporal[0][Plane]);
  FreeAndNil(fSetTemporal[1][Plane]);
  FreeAndNil(fSetUnpackPF[Plane]);
  FreeAndNil(fSetDequantPF[Plane]);
  FreeAndNil(fSetCoeffToScratchPF[Plane]);
  FreeAndNil(fSetScratchToCoeffPF[Plane]);
  FreeAndNil(fSetRowPF[Plane]);
  FreeAndNil(fSetMCTFMC[Plane]);
  FreeAndNil(fSetMCTFAdd[Plane]);
  FreeAndNil(fSetCoeffToScratch[Plane]);
  FreeAndNil(fSetScratchToCoeff[Plane]);
  FreeAndNil(fSetRow[Plane]);
 end;
 for SlotIndex:=0 to length(fRingDataBuffer)-1 do begin // mode B per-frame input ring
  for Plane:=0 to fNumPlanes-1 do begin
   FreeAndNil(fRingSetUnpack[SlotIndex][Plane]);
   FreeAndNil(fRingSetDequant[SlotIndex][Plane]);
   FreeAndNil(fRingSetApplyAQ[SlotIndex][Plane]);
   FreeAndNil(fRingSetGMC0[SlotIndex][Plane]);
   FreeAndNil(fRingSetGMC1[SlotIndex][Plane]);
   FreeAndNil(fRingSetGBlend[SlotIndex][Plane]);
   FreeAndNil(fRingSetGBlendMode[SlotIndex][Plane]);
   FreeAndNil(fRingSetGAdd[SlotIndex][Plane]);
   FreeAndNil(fRingSetMCPlay[SlotIndex][Plane]);
   FreeAndNil(fRingOffsetBuffer[SlotIndex][Plane]);
   FreeAndNil(fRingStepBuffer[SlotIndex][Plane]);
  end;
  FreeAndNil(fRingDataBuffer[SlotIndex]);
  FreeAndNil(fRingTileCodesBuffer[SlotIndex]);
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
 for Plane:=0 to fNumPlanes-1 do begin
  FreeAndNil(fGopBuffer[0][Plane]);
  FreeAndNil(fGopBuffer[1][Plane]);
  FreeAndNil(fPrefetchCoeff[Plane]);
  FreeAndNil(fMCTFPred[Plane]);
  FreeAndNil(fMCTFScratch[Plane]);
 end;
 FreeAndNil(fMV1Buffer);
 FreeAndNil(fModeBuffer);
 for SlotIndex:=0 to length(fDPBBuffer)-1 do begin
  for Plane:=0 to fNumPlanes-1 do begin
   FreeAndNil(fDPBBuffer[SlotIndex][Plane]);
  end;
 end;
 for Plane:=0 to fNumPlanes-1 do begin
  FreeAndNil(fGMCBuffer[0][Plane]);
  FreeAndNil(fGMCBuffer[1][Plane]);
 end;
 FreeAndNil(fMVBuffer);
 FreeAndNil(fScratchBuffer);
 for Plane:=0 to fNumPlanes-1 do begin
  FreeAndNil(fPreviousBuffer[Plane]);
  FreeAndNil(fCoeffBuffer[Plane]);
  FreeAndNil(fStepBuffer[Plane]);
  FreeAndNil(fOffsetBuffer[Plane]);
 end;
 FreeAndNil(fCoeffBuffer[3]); // the shared decoded-alpha plane (not covered by the loop above)
 for SlotIndex:=0 to length(fAlphaRingData)-1 do begin // the alpha host-input ring
  FreeAndNil(fAlphaRingData[SlotIndex]);
  FreeAndNil(fAlphaRingOffset[SlotIndex]);
  FreeAndNil(fAlphaRingStep[SlotIndex]);
 end;
 FreeAndNil(fDataBuffer);

 FreeAndNil(fPipeTDWTFloat);
 FreeAndNil(fPipeTDWTInt);
 FreeAndNil(fPipeBlendMode);
 FreeAndNil(fPipeBidiBlend);
 FreeAndNil(fPipeColorHDRSCRGBAlpha);
 FreeAndNil(fPipeColorHDRAlpha);
 FreeAndNil(fPipeColorHDRSCRGB);
 FreeAndNil(fPipeColorHDR);
 FreeAndNil(fPipeColorAlpha);
 FreeAndNil(fPipeColor);
 FreeAndNil(fPipeMotionAdd);
 FreeAndNil(fPipeMC);
 FreeAndNil(fPipeCoeffAdd);
 FreeAndNil(fPipeRound);
 FreeAndNil(fPipeIDWT53);
 FreeAndNil(fPipeIDWT97);
 FreeAndNil(fPipeTranspose);
 FreeAndNil(fPipeApplyAQ);
 FreeAndNil(fWeightLUTBuffer);
 FreeAndNil(fTileCodesBuffer);
 FreeAndNil(fPipeDequant);
 FreeAndNil(fPipeUnpack);

 FreeAndNil(fPLTemporal);
 FreeAndNil(fPLBlendMode);
 FreeAndNil(fPLColorHDRAlpha);
 FreeAndNil(fPLColorHDR);
 FreeAndNil(fPLColorAlpha);
 FreeAndNil(fPLColor);
 FreeAndNil(fPLCoeffAdd);
 FreeAndNil(fPLRound);
 FreeAndNil(fPLRow);
 FreeAndNil(fPLTranspose);
 FreeAndNil(fPLApplyAQ);
 FreeAndNil(fPLDequant);
 FreeAndNil(fPLUnpack);

 FreeAndNil(fDSLColorAlpha);
 FreeAndNil(fDSLColor);
 FreeAndNil(fDSL4);
 FreeAndNil(fDSL3);
 FreeAndNil(fDSL2);
 FreeAndNil(fDSL1);

 FreeAndNil(fPipelineCache);

 inherited Destroy;
end;

end.
