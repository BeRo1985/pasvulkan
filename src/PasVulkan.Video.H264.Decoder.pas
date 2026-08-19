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
 * See the file COPYING.PasVulkan in the source distribution for the full     *
 * zlib license text.                                                         *
 *                                                                            *
 ******************************************************************************)
unit PasVulkan.Video.H264.Decoder;
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

// VK hardware H.264 decode backend for the Flexible Wavelet video player (the engine-side port of the C
// fwv_h264.c run_h264_player). The container can carry an H.264 Annex-B elementary stream alongside the
// wavelet stream; where the GPU supports VK_KHR_video_decode_h264 (TpvApplication.VideoDecodeSupport), the
// player decodes H.264 in hardware, otherwise it falls back to the wavelet decoder.
//
// Stage F3a (this revision) = the CPU bitstream front-end only: Annex-B NAL splitting, RBSP emulation-prevention
// removal, and SPS / PPS / slice-header / POC parsing (a faithful port of fwv_h264.c lines 108-311). The VK video
// session + DPB + decode submission + NV12->RGBA come in the following sub-stages.

interface

uses SysUtils,
     Classes,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Framework,
     PasVulkan.Assets.Video.FlexibleVideo;

const H264ReorderDepth=8; // matches the C reference REORDER: bump the lowest-key frame once more than this are buffered

type EpvVideoH264=class(Exception);

     { TpvVideoH264Decoder }
     TpvVideoH264Decoder=class
      public
       type { TBitReader }
            // MSB-first bit reader over a (de-emulation-prevented) RBSP, matching the C BitReader
            TBitReader=record
             Data:PpvUInt8Array;
             Length:TpvSizeUInt; // in bytes
             BitPosition:TpvSizeUInt;
             procedure Init(const aData:PpvUInt8Array;const aLength:TpvSizeUInt);
             function ReadBit:TpvInt32;
             function ReadBits(const aCount:TpvInt32):TpvUInt32;
             function ReadUnsignedExpGolomb:TpvUInt32;
             function ReadSignedExpGolomb:TpvInt32;
             procedure SkipScalingLists(const aListCount:TpvInt32);
            end;
            { TSPS }
            TSPS=record
             ProfileIDC:TpvInt32;
             LevelIDC:TpvInt32;
             SPSId:TpvInt32;
             ChromaFormatIDC:TpvInt32;
             Log2MaxFrameNumMinus4:TpvInt32;
             POCType:TpvInt32;
             Log2MaxPOCLSBMinus4:TpvInt32;
             MaxNumRef:TpvInt32;
             WidthInMBsMinus1:TpvInt32;
             HeightInMapUnitsMinus1:TpvInt32;
             FrameMBsOnly:TpvInt32;
             Direct8x8:TpvInt32;
             MBAdaptive:TpvInt32;
             Gaps:TpvInt32;
             CropLeft:TpvInt32;
             CropRight:TpvInt32;
             CropTop:TpvInt32;
             CropBottom:TpvInt32;
             Width:TpvInt32;
             Height:TpvInt32;
            end;
            { TPPS }
            TPPS=record
             PPSId:TpvInt32;
             SPSId:TpvInt32;
             EntropyCodingMode:TpvInt32;
             BottomFieldPOC:TpvInt32;
             NumRefL0Minus1:TpvInt32;
             NumRefL1Minus1:TpvInt32;
             WeightedPred:TpvInt32;
             WeightedBipred:TpvInt32;
             PicInitQPMinus26:TpvInt32;
             PicInitQSMinus26:TpvInt32;
             ChromaQPOffset:TpvInt32;
             DeblockingControl:TpvInt32;
             ConstrainedIntra:TpvInt32;
             Redundant:TpvInt32;
             Transform8x8:TpvInt32;
             SecondChromaQPOffset:TpvInt32;
            end;
            { TMmcoOperation }
            // one parsed memory_management_control_operation from dec_ref_pic_marking
            TMmcoOperation=record
             Operation:TpvInt32; // 1..6 ; 0 terminates the list
             Argument1:TpvInt32; // op1/op3: difference_of_pic_nums_minus1 ; op2: long_term_pic_num ; op4: max_long_term_frame_idx_plus1
             Argument2:TpvInt32; // op3/op6: long_term_frame_idx
            end;
            TMmcoOperations=array[0..15] of TMmcoOperation;
            { TSlice }
            TSlice=record
             SliceType:TpvInt32;
             FrameNum:TpvInt32;
             IDR:TpvInt32;
             POCLSB:TpvInt32;
             IDRPicId:TpvInt32;
             LongTermReferenceFlag:TpvInt32; // IDR only
             AdaptiveMarking:TpvInt32; // non-IDR reference: explicit MMCO commands present
             MmcoCount:TpvInt32;
             Mmco:TMmcoOperations;
            end;
            { TFrame }
            // one decodable picture (a coded slice NAL incl. its Annex-B start code) in decode order
            TFrame=record
             NALOffset:TpvSizeUInt; // byte offset of the NAL (including the 00 00 01 start code) in the blob
             NALLength:TpvSizeUInt; // length from the start code to the next start code (the unit handed to the GPU decoder)
             IDR:TpvInt32;
             FrameNum:TpvInt32;
             POC:TpvInt32; // display order key
             SliceType:TpvInt32; // 0=P 1=B 2=I ...
             RefIdc:TpvInt32; // nal_ref_idc: 0 = non-reference picture
             LongTermReferenceFlag:TpvInt32; // IDR only: mark as a long-term reference
             AdaptiveMarking:TpvInt32; // non-IDR reference: explicit MMCO commands present
             MmcoCount:TpvInt32;
             Mmco:TMmcoOperations;
            end;
            TFrames=array of TFrame;
       class function ToRBSP(const aNAL:PpvUInt8Array;const aLength:TpvSizeUInt;const aOut:PpvUInt8Array):TpvSizeUInt; static;
       class procedure ParseSPS(const aRBSP:PpvUInt8Array;const aLength:TpvSizeUInt;out aSPS:TSPS); static;
       class procedure ParsePPS(const aRBSP:PpvUInt8Array;const aLength:TpvSizeUInt;out aPPS:TPPS); static;
       class procedure ParseSlice(const aRBSP:PpvUInt8Array;const aLength:TpvSizeUInt;const aSPS:TSPS;const aPPS:TPPS;const aNALType,aNALRefIdc:TpvInt32;out aSlice:TSlice); static;
       class function ComputePOC(const aSPS:TSPS;const aSlice:TSlice;var aPreviousMSB,aPreviousLSB:TpvInt32):TpvInt32; static;
       // split an Annex-B blob into the SPS/PPS + the decode-order frame list (NAL split + slice parse + POC); returns frame count
       class function ParseAnnexB(const aBlob:PpvUInt8Array;const aSize:TpvSizeUInt;out aSPS:TSPS;out aPPS:TPPS;out aFrames:TFrames):TpvInt32; static;
      private
       fDevice:TpvVulkanDevice;
       fBlob:array of TpvUInt8;
       fFrames:TFrames;
       fFrameCount:TpvInt32;
       fSPS:TSPS;
       fPPS:TPPS;
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fCodedWidth:TpvInt32;
       fCodedHeight:TpvInt32;
       // display-order bookkeeping (precomputed from the parsed decode-order frame list); shared by the
       // CPU and the GPU side, so it is kept outside the {$ifdef VkVideo} GPU machinery
       fDisplayOrder:array of TpvInt32; // display position -> coded (decode-order) frame index
       fFrameKey:array of TpvUInt64;    // per coded frame: (gop_index shl 24) or (poc biased positive)
       fFrameGOP:array of TpvInt32;     // per coded frame: GOP index (increments at every non-first IDR)
{$ifdef VkVideo}
       fStdSPS:TStdVideoH264SequenceParameterSet;
       fStdPPS:TStdVideoH264PictureParameterSet;
       fSession:TVkVideoSessionKHR;
       fSessionParameters:TVkVideoSessionParametersKHR;
       fSessionMemories:array of TVkDeviceMemory;
       fMaxDpbSlots:TpvInt32;
       fMaxActiveReferences:TpvInt32;
       // profile chain kept alive for the DPB-image / bitstream-buffer pNext (VkVideoProfileListInfoKHR)
       fProfile:TVkVideoProfileInfoKHR;
       fH264Profile:TVkVideoDecodeH264ProfileInfoKHR;
       fProfileList:TVkVideoProfileListInfoKHR;
       fNv12Format:TVkFormat;
       fBitstreamAlignment:TVkDeviceSize;
       fMaxFrameNum:TpvInt32;
       // DPB pool: NV12 reference / decode-target images + per-slot picture state
       fSlotCount:TpvInt32;
       fMaxRef:TpvInt32;
       fDPBImages:array of TVkImage;
       fDPBViews:array of TVkImageView;
       fDPBMemories:array of TVkDeviceMemory;
       fDPBLayouts:array of TVkImageLayout;
       fPictureResources:array of TVkVideoPictureResourceInfoKHR;
       fSlotUsed:array of TpvInt32;
       fSlotPOC:array of TpvInt32;
       fSlotFrameNum:array of TpvInt32;
       fSlotLongTerm:array of TpvInt32;          // 0 = short-term reference, 1 = long-term reference
       fSlotLongTermFrameIdx:array of TpvInt32;  // valid when fSlotLongTerm[]<>0
       fMaxLongTermFrameIdx:TpvInt32;            // H.264 reference marking state (MMCO op4); -1 = none
       // host-visible bitstream upload buffer (mapped persistently)
       fBitstreamBuffer:TVkBuffer;
       fBitstreamMemory:TVkDeviceMemory;
       fBitstreamMap:Pointer;
       fBitstreamLength:TVkDeviceSize;
       // sampled NV12 stage image (MUTABLE_FORMAT) + R8 / R8G8 plane views + a NEAREST sampler
       fStageImage:TVkImage;
       fStageMemory:TVkDeviceMemory;
       fStageLumaView:TVkImageView;
       fStageChromaView:TVkImageView;
       fSampler:TVkSampler;
       fStageLayout:TVkImageLayout;
       // RGBA output pool (one slot per in-flight decoded frame, all kept in VK_IMAGE_LAYOUT_GENERAL).
       // engine-managed TpvVulkanImage so the facade can expose a uniform OutputImage:TpvVulkanImage for both codecs
       fPoolCount:TpvInt32;
       fPoolImages:array of TpvVulkanImage;
       fPoolViews:array of TpvVulkanImageView;
       fPoolMemories:array of TpvVulkanDeviceMemoryBlock;
       fPoolFree:array of TpvInt32;
       fFreeCount:TpvInt32;
       // single stable display image the current rotating pool slot is copied into after each decode, so the facade
       // sees one persistent SAMPLED image in TRANSFER_SRC_OPTIMAL (identical to the wavelet OutputImage contract)
       fDisplayImage:TpvVulkanImage;
       fDisplayImageView:TpvVulkanImageView;
       fDisplayMemory:TpvVulkanDeviceMemoryBlock;
       fDisplayLayout:TVkImageLayout;
       // nv12rgb compute pipeline + per-pool descriptor sets (luma/chroma sampled, pool image as storage dst)
       fComputeModule:TVkShaderModule;
       fSetLayout:TVkDescriptorSetLayout;
       fComputeLayout:TVkPipelineLayout;
       fComputePipeline:TVkPipeline;
       fDescriptorPool:TVkDescriptorPool;
       fDescriptorSets:array of TVkDescriptorSet;
       // command buffers + fence (video-decode queue for the decode, universal queue for the compute)
       fVideoCommandPool:TVkCommandPool;
       fDecodeCommand:TVkCommandBuffer;
       fComputeCommandPool:TVkCommandPool;
       fComputeCommand:TVkCommandBuffer;
       fComputeFence:TVkFence;
       fResetDone:TpvInt32;
       // online reorder buffer (decoded RGBA pool slots keyed by display key, bumped lowest-first once full)
       fReorderKey:array of TpvUInt64;
       fReorderPool:array of TpvInt32;
       fReorderCount:TpvInt32;
       fNextCoded:TpvInt32;           // decode cursor: next coded (decode-order) frame to decode
       fCurrentDisplayIndex:TpvInt32; // display position of the current output frame (-1 = none yet)
       fCurrentPoolIndex:TpvInt32;    // pool slot holding the current output frame
       function FindMemoryType(const aTypeBits:TVkUInt32;const aWanted:TVkMemoryPropertyFlags):TpvUInt32;
       procedure BuildStdParameterSets;
       procedure CreateVideoSession;
       procedure CreateDecodeResources;
       procedure DestroyDecodeResources;
       procedure RecordImageBarrier(const aCommandBuffer:TVkCommandBuffer;const aImage:TVkImage;
                                    const aOldLayout,aNewLayout:TVkImageLayout;
                                    const aSrcAccess,aDstAccess:TVkAccessFlags2;
                                    const aSrcStage,aDstStage:TVkPipelineStageFlags2);
       function DecodeCodedFrame(const aCodedIndex:TpvInt32):TpvInt32;
       procedure CopyPoolToDisplay(const aPoolIndex:TpvInt32); // copy a GENERAL pool slot into fDisplayImage (-> TRANSFER_SRC_OPTIMAL)
       procedure ResetDecodeState(const aFromCodedIndex:TpvInt32);
{$endif}
       procedure PrepareDisplayOrder;
      public
       constructor Create(const aStream:TStream;const aDevice:TpvVulkanDevice); reintroduce;
       destructor Destroy; override;
{$ifdef VkVideo}
       // GPU side, called on the Draw thread only: make sure the frame at the given DISPLAY position is decoded
       // (decoding however many decode-order frames it takes, with full DPB reference handling) and expose it as
       // the current output. Returns true when an output frame is available. aDisplayIndex is clamped to range.
       function EnsureDisplayFrame(const aDisplayIndex:TpvInt32):boolean;
       // the current output frame as an RGBA8 image (kept in VK_IMAGE_LAYOUT_GENERAL); the facade blits from it
       function OutputImage:TpvVulkanImage;
       function OutputImageView:TpvVulkanImageView;
{$endif}
       property Width:TpvInt32 read fWidth;
       property Height:TpvInt32 read fHeight;
       property FrameCount:TpvInt32 read fFrameCount;
     end;

implementation

uses PasVulkan.Application;

{ TpvVideoH264Decoder.TBitReader }

procedure TpvVideoH264Decoder.TBitReader.Init(const aData:PpvUInt8Array;const aLength:TpvSizeUInt);
begin
 Data:=aData;
 Length:=aLength;
 BitPosition:=0;
end;

function TpvVideoH264Decoder.TBitReader.ReadBit:TpvInt32;
begin
 if (BitPosition shr 3)>=Length then begin
  result:=0;
 end else begin
  result:=(Data^[BitPosition shr 3] shr (7-(BitPosition and 7))) and 1;
  inc(BitPosition);
 end;
end;

function TpvVideoH264Decoder.TBitReader.ReadBits(const aCount:TpvInt32):TpvUInt32;
var Index:TpvInt32;
begin
 result:=0;
 for Index:=0 to aCount-1 do begin
  result:=(result shl 1) or TpvUInt32(ReadBit);
 end;
end;

function TpvVideoH264Decoder.TBitReader.ReadUnsignedExpGolomb:TpvUInt32;
var LeadingZeros,Index:TpvInt32;
begin
 LeadingZeros:=0;
 while (ReadBit=0) and (LeadingZeros<32) do begin
  inc(LeadingZeros);
 end;
 result:=(TpvUInt32(1) shl LeadingZeros)-1;
 for Index:=0 to LeadingZeros-1 do begin
  inc(result,TpvUInt32(ReadBit) shl (LeadingZeros-1-Index));
 end;
end;

function TpvVideoH264Decoder.TBitReader.ReadSignedExpGolomb:TpvInt32;
var Code:TpvUInt32;
begin
 Code:=ReadUnsignedExpGolomb;
 if (Code and 1)<>0 then begin
  result:=TpvInt32((Code+1) shr 1);
 end else begin
  result:=-TpvInt32(Code shr 1);
 end;
end;

procedure TpvVideoH264Decoder.TBitReader.SkipScalingLists(const aListCount:TpvInt32);
var Index,Last,Next,Size,SubIndex:TpvInt32;
begin
 for Index:=0 to aListCount-1 do begin
  if ReadBit<>0 then begin
   Last:=8;
   Next:=8;
   if Index<6 then begin
    Size:=16;
   end else begin
    Size:=64;
   end;
   for SubIndex:=0 to Size-1 do begin
    if Next<>0 then begin
     Next:=((Last+ReadSignedExpGolomb)+256) mod 256;
    end;
    if Next<>0 then begin
     Last:=Next;
    end;
   end;
  end;
 end;
end;

{ TpvVideoH264Decoder }

class function TpvVideoH264Decoder.ToRBSP(const aNAL:PpvUInt8Array;const aLength:TpvSizeUInt;const aOut:PpvUInt8Array):TpvSizeUInt;
var Index,OutLength:TpvSizeUInt;
begin
 // remove the emulation-prevention bytes (00 00 03 -> 00 00) to recover the raw byte sequence payload
 OutLength:=0;
 Index:=0;
 while Index<aLength do begin
  if (Index>=2) and (aNAL^[Index]=3) and (aNAL^[Index-1]=0) and (aNAL^[Index-2]=0) and ((Index+1)<aLength) and (aNAL^[Index+1]<=3) then begin
   inc(Index);
   continue;
  end;
  aOut^[OutLength]:=aNAL^[Index];
  inc(OutLength);
  inc(Index);
 end;
 result:=OutLength;
end;

class procedure TpvVideoH264Decoder.ParseSPS(const aRBSP:PpvUInt8Array;const aLength:TpvSizeUInt;out aSPS:TSPS);
var Reader:TBitReader;
    Cycle,Index:TpvInt32;
begin
 Reader.Init(aRBSP,aLength);
 FillChar(aSPS,SizeOf(TSPS),#0);
 aSPS.ProfileIDC:=TpvInt32(Reader.ReadBits(8));
 Reader.ReadBits(8);
 aSPS.LevelIDC:=TpvInt32(Reader.ReadBits(8));
 aSPS.SPSId:=TpvInt32(Reader.ReadUnsignedExpGolomb);
 aSPS.ChromaFormatIDC:=1;
 if (aSPS.ProfileIDC=100) or (aSPS.ProfileIDC=110) or (aSPS.ProfileIDC=122) or (aSPS.ProfileIDC=244) or
    (aSPS.ProfileIDC=44) or (aSPS.ProfileIDC=83) or (aSPS.ProfileIDC=86) or (aSPS.ProfileIDC=118) or (aSPS.ProfileIDC=128) then begin
  aSPS.ChromaFormatIDC:=TpvInt32(Reader.ReadUnsignedExpGolomb);
  if aSPS.ChromaFormatIDC=3 then begin
   Reader.ReadBit;
  end;
  Reader.ReadUnsignedExpGolomb;
  Reader.ReadUnsignedExpGolomb;
  Reader.ReadBit;
  if Reader.ReadBit<>0 then begin
   if aSPS.ChromaFormatIDC<>3 then begin
    Reader.SkipScalingLists(8);
   end else begin
    Reader.SkipScalingLists(12);
   end;
  end;
 end;
 aSPS.Log2MaxFrameNumMinus4:=TpvInt32(Reader.ReadUnsignedExpGolomb);
 aSPS.POCType:=TpvInt32(Reader.ReadUnsignedExpGolomb);
 if aSPS.POCType=0 then begin
  aSPS.Log2MaxPOCLSBMinus4:=TpvInt32(Reader.ReadUnsignedExpGolomb);
 end else if aSPS.POCType=1 then begin
  Reader.ReadBit;
  Reader.ReadSignedExpGolomb;
  Reader.ReadSignedExpGolomb;
  Cycle:=TpvInt32(Reader.ReadUnsignedExpGolomb);
  for Index:=0 to Cycle-1 do begin
   Reader.ReadSignedExpGolomb;
  end;
 end;
 aSPS.MaxNumRef:=TpvInt32(Reader.ReadUnsignedExpGolomb);
 aSPS.Gaps:=Reader.ReadBit;
 aSPS.WidthInMBsMinus1:=TpvInt32(Reader.ReadUnsignedExpGolomb);
 aSPS.HeightInMapUnitsMinus1:=TpvInt32(Reader.ReadUnsignedExpGolomb);
 aSPS.FrameMBsOnly:=Reader.ReadBit;
 if aSPS.FrameMBsOnly=0 then begin
  aSPS.MBAdaptive:=Reader.ReadBit;
 end;
 aSPS.Direct8x8:=Reader.ReadBit;
 if Reader.ReadBit<>0 then begin
  aSPS.CropLeft:=TpvInt32(Reader.ReadUnsignedExpGolomb);
  aSPS.CropRight:=TpvInt32(Reader.ReadUnsignedExpGolomb);
  aSPS.CropTop:=TpvInt32(Reader.ReadUnsignedExpGolomb);
  aSPS.CropBottom:=TpvInt32(Reader.ReadUnsignedExpGolomb);
 end;
 aSPS.Width:=(((aSPS.WidthInMBsMinus1+1)*16)-((aSPS.CropLeft+aSPS.CropRight)*2));
 aSPS.Height:=((((aSPS.HeightInMapUnitsMinus1+1)*16)*(2-aSPS.FrameMBsOnly))-(((aSPS.CropTop+aSPS.CropBottom)*2)*(2-aSPS.FrameMBsOnly)));
end;

class procedure TpvVideoH264Decoder.ParsePPS(const aRBSP:PpvUInt8Array;const aLength:TpvSizeUInt;out aPPS:TPPS);
var Reader:TBitReader;
    LastSetBit,Bit:TpvSizeUInt;
begin
 Reader.Init(aRBSP,aLength);
 FillChar(aPPS,SizeOf(TPPS),#0);
 aPPS.PPSId:=TpvInt32(Reader.ReadUnsignedExpGolomb);
 aPPS.SPSId:=TpvInt32(Reader.ReadUnsignedExpGolomb);
 aPPS.EntropyCodingMode:=Reader.ReadBit;
 aPPS.BottomFieldPOC:=Reader.ReadBit;
 Reader.ReadUnsignedExpGolomb;
 aPPS.NumRefL0Minus1:=TpvInt32(Reader.ReadUnsignedExpGolomb);
 aPPS.NumRefL1Minus1:=TpvInt32(Reader.ReadUnsignedExpGolomb);
 aPPS.WeightedPred:=Reader.ReadBit;
 aPPS.WeightedBipred:=TpvInt32(Reader.ReadBits(2));
 aPPS.PicInitQPMinus26:=Reader.ReadSignedExpGolomb;
 aPPS.PicInitQSMinus26:=Reader.ReadSignedExpGolomb;
 aPPS.ChromaQPOffset:=Reader.ReadSignedExpGolomb;
 aPPS.DeblockingControl:=Reader.ReadBit;
 aPPS.ConstrainedIntra:=Reader.ReadBit;
 aPPS.Redundant:=Reader.ReadBit;
 aPPS.SecondChromaQPOffset:=aPPS.ChromaQPOffset;
 // the optional transform_8x8 / scaling / second chroma offset tail is present only before the rbsp stop bit
 LastSetBit:=0;
 Bit:=aLength*8;
 while Bit>0 do begin
  if ((aRBSP^[(Bit-1) shr 3] shr (7-((Bit-1) and 7))) and 1)<>0 then begin
   LastSetBit:=Bit-1;
   break;
  end;
  dec(Bit);
 end;
 if Reader.BitPosition<LastSetBit then begin
  aPPS.Transform8x8:=Reader.ReadBit;
  if Reader.ReadBit<>0 then begin
   if aPPS.Transform8x8<>0 then begin
    Reader.SkipScalingLists(6+2);
   end else begin
    Reader.SkipScalingLists(6);
   end;
  end;
  aPPS.SecondChromaQPOffset:=Reader.ReadSignedExpGolomb;
 end;
end;

class procedure TpvVideoH264Decoder.ParseSlice(const aRBSP:PpvUInt8Array;const aLength:TpvSizeUInt;const aSPS:TSPS;const aPPS:TPPS;const aNALType,aNALRefIdc:TpvInt32;out aSlice:TSlice);
var Reader:TBitReader;
    FieldPicFlag,NumRefL0,NumRefL1,ChromaArrayType,HasWeights,Idc,Operation,Index:TpvInt32;
begin

 // Parse the whole slice header up to and including dec_ref_pic_marking() - everything before it is read and
 // discarded just to stay bit-aligned (the GPU re-reads it from the bitstream); the reference picture marking
 // (sliding window vs. MMCO) is the only part the application must track itself.
 FillChar(aSlice,SizeOf(TSlice),#0);
 Reader.Init(aRBSP,aLength);
 Reader.ReadUnsignedExpGolomb; // first_mb_in_slice
 aSlice.SliceType:=TpvInt32(Reader.ReadUnsignedExpGolomb mod 5);
 Reader.ReadUnsignedExpGolomb; // pic_parameter_set_id
 aSlice.FrameNum:=TpvInt32(Reader.ReadBits(aSPS.Log2MaxFrameNumMinus4+4));
 FieldPicFlag:=0;
 if aSPS.FrameMBsOnly=0 then begin
  FieldPicFlag:=Reader.ReadBit;
  if FieldPicFlag<>0 then begin
   Reader.ReadBit; // bottom_field_flag
  end;
 end;
 if aNALType=5 then begin
  aSlice.IDR:=1;
 end else begin
  aSlice.IDR:=0;
 end;
 if aSlice.IDR<>0 then begin
  aSlice.IDRPicId:=TpvInt32(Reader.ReadUnsignedExpGolomb);
 end;
 if aSPS.POCType=0 then begin
  aSlice.POCLSB:=TpvInt32(Reader.ReadBits(aSPS.Log2MaxPOCLSBMinus4+4));
  if (aPPS.BottomFieldPOC<>0) and (FieldPicFlag=0) then begin
   Reader.ReadSignedExpGolomb; // delta_pic_order_cnt_bottom
  end;
 end;
 // poc_type=1 (delta-based POC) is not emitted by x264 here and the SPS parser does not retain
 // delta_pic_order_always_zero_flag, so that branch is intentionally not consumed.
 if aPPS.Redundant<>0 then begin
  Reader.ReadUnsignedExpGolomb; // redundant_pic_cnt
 end;
 if aSlice.SliceType=1 then begin // B
  Reader.ReadBit; // direct_spatial_mv_pred_flag
 end;
 NumRefL0:=aPPS.NumRefL0Minus1;
 NumRefL1:=aPPS.NumRefL1Minus1;
 if (aSlice.SliceType=0) or (aSlice.SliceType=3) or (aSlice.SliceType=1) then begin // P / SP / B
  if Reader.ReadBit<>0 then begin // num_ref_idx_active_override_flag
   NumRefL0:=TpvInt32(Reader.ReadUnsignedExpGolomb);
   if aSlice.SliceType=1 then begin
    NumRefL1:=TpvInt32(Reader.ReadUnsignedExpGolomb);
   end;
  end;
 end;

 // ref_pic_list_modification() - parse + discard
 if (aSlice.SliceType<>2) and (aSlice.SliceType<>4) then begin // not I / SI
  if Reader.ReadBit<>0 then begin // ref_pic_list_modification_flag_l0
   repeat
    Idc:=TpvInt32(Reader.ReadUnsignedExpGolomb);
    if (Idc=0) or (Idc=1) or (Idc=2) then begin
     Reader.ReadUnsignedExpGolomb;
    end;
   until Idc=3;
  end;
 end;
 if aSlice.SliceType=1 then begin // B
  if Reader.ReadBit<>0 then begin // ref_pic_list_modification_flag_l1
   repeat
    Idc:=TpvInt32(Reader.ReadUnsignedExpGolomb);
    if (Idc=0) or (Idc=1) or (Idc=2) then begin
     Reader.ReadUnsignedExpGolomb;
    end;
   until Idc=3;
  end;
 end;

 // pred_weight_table() - parse + discard (chroma_array_type = chroma_format_idc; no separate colour planes)
 ChromaArrayType:=aSPS.ChromaFormatIDC;
 if ((((aSlice.SliceType=0) or (aSlice.SliceType=3)) and (aPPS.WeightedPred<>0)) or
     ((aSlice.SliceType=1) and (aPPS.WeightedBipred=1))) then begin
  HasWeights:=1;
 end else begin
  HasWeights:=0;
 end;
 if HasWeights<>0 then begin
  Reader.ReadUnsignedExpGolomb; // luma_log2_weight_denom
  if ChromaArrayType<>0 then begin
   Reader.ReadUnsignedExpGolomb; // chroma_log2_weight_denom
  end;
  for Index:=0 to NumRefL0 do begin
   if Reader.ReadBit<>0 then begin // luma_weight_l0_flag
    Reader.ReadSignedExpGolomb;
    Reader.ReadSignedExpGolomb;
   end;
   if ChromaArrayType<>0 then begin
    if Reader.ReadBit<>0 then begin // chroma_weight_l0_flag
     Reader.ReadSignedExpGolomb;
     Reader.ReadSignedExpGolomb;
     Reader.ReadSignedExpGolomb;
     Reader.ReadSignedExpGolomb;
    end;
   end;
  end;
  if aSlice.SliceType=1 then begin
   for Index:=0 to NumRefL1 do begin
    if Reader.ReadBit<>0 then begin // luma_weight_l1_flag
     Reader.ReadSignedExpGolomb;
     Reader.ReadSignedExpGolomb;
    end;
    if ChromaArrayType<>0 then begin
     if Reader.ReadBit<>0 then begin // chroma_weight_l1_flag
      Reader.ReadSignedExpGolomb;
      Reader.ReadSignedExpGolomb;
      Reader.ReadSignedExpGolomb;
      Reader.ReadSignedExpGolomb;
     end;
    end;
   end;
  end;
 end;

 // dec_ref_pic_marking() - the part we keep
 if aNALRefIdc<>0 then begin
  if aSlice.IDR<>0 then begin
   Reader.ReadBit; // no_output_of_prior_pics_flag
   aSlice.LongTermReferenceFlag:=Reader.ReadBit;
  end;
  if aSlice.IDR=0 then begin
   aSlice.AdaptiveMarking:=Reader.ReadBit; // adaptive_ref_pic_marking_mode_flag
   if aSlice.AdaptiveMarking<>0 then begin
    repeat
     Operation:=TpvInt32(Reader.ReadUnsignedExpGolomb);
     if (Operation<>0) and (aSlice.MmcoCount<16) then begin
      aSlice.Mmco[aSlice.MmcoCount].Operation:=Operation;
      aSlice.Mmco[aSlice.MmcoCount].Argument1:=0;
      aSlice.Mmco[aSlice.MmcoCount].Argument2:=0;
      if (Operation=1) or (Operation=3) then begin
       aSlice.Mmco[aSlice.MmcoCount].Argument1:=TpvInt32(Reader.ReadUnsignedExpGolomb); // difference_of_pic_nums_minus1
      end;
      if Operation=2 then begin
       aSlice.Mmco[aSlice.MmcoCount].Argument1:=TpvInt32(Reader.ReadUnsignedExpGolomb); // long_term_pic_num
      end;
      if (Operation=3) or (Operation=6) then begin
       aSlice.Mmco[aSlice.MmcoCount].Argument2:=TpvInt32(Reader.ReadUnsignedExpGolomb); // long_term_frame_idx
      end;
      if Operation=4 then begin
       aSlice.Mmco[aSlice.MmcoCount].Argument1:=TpvInt32(Reader.ReadUnsignedExpGolomb); // max_long_term_frame_idx_plus1
      end;
      inc(aSlice.MmcoCount);
     end;
    until Operation=0;
   end;
  end;
 end;

end;

class function TpvVideoH264Decoder.ComputePOC(const aSPS:TSPS;const aSlice:TSlice;var aPreviousMSB,aPreviousLSB:TpvInt32):TpvInt32;
var MaxLSB,MSB:TpvInt32;
begin
 if aSlice.IDR<>0 then begin
  aPreviousMSB:=0;
  aPreviousLSB:=0;
  result:=0;
  exit;
 end;
 MaxLSB:=1 shl (aSPS.Log2MaxPOCLSBMinus4+4);
 if (aSlice.POCLSB<aPreviousLSB) and ((aPreviousLSB-aSlice.POCLSB)>=(MaxLSB div 2)) then begin
  MSB:=aPreviousMSB+MaxLSB;
 end else if (aSlice.POCLSB>aPreviousLSB) and ((aSlice.POCLSB-aPreviousLSB)>(MaxLSB div 2)) then begin
  MSB:=aPreviousMSB-MaxLSB;
 end else begin
  MSB:=aPreviousMSB;
 end;
 aPreviousMSB:=MSB;
 aPreviousLSB:=aSlice.POCLSB;
 result:=MSB+aSlice.POCLSB;
end;

class function TpvVideoH264Decoder.ParseAnnexB(const aBlob:PpvUInt8Array;const aSize:TpvSizeUInt;out aSPS:TSPS;out aPPS:TPPS;out aFrames:TFrames):TpvInt32;
var Position,Start,Stop,RBSPLength:TpvSizeUInt;
    NALType,RefIdc,PreviousMSB,PreviousLSB,FrameCount,POC:TpvInt32;
    HaveSPS,HavePPS:boolean;
    RBSP:array of TpvUInt8;
    Slice:TSlice;
begin
 FillChar(aSPS,SizeOf(TSPS),#0);
 FillChar(aPPS,SizeOf(TPPS),#0);
 aFrames:=nil;
 SetLength(aFrames,256);
 SetLength(RBSP,aSize+16);
 HaveSPS:=false;
 HavePPS:=false;
 PreviousMSB:=0;
 PreviousLSB:=0;
 FrameCount:=0;
 Position:=0;
 while (Position+3)<aSize do begin
  if (aBlob^[Position]=0) and (aBlob^[Position+1]=0) and (aBlob^[Position+2]=1) then begin
   Start:=Position+3;
   Stop:=Start;
   while ((Stop+3)<aSize) and not ((aBlob^[Stop]=0) and (aBlob^[Stop+1]=0) and (aBlob^[Stop+2]=1)) do begin
    inc(Stop);
   end;
   if (Stop+3)>=aSize then begin
    Stop:=aSize;
   end;
   NALType:=aBlob^[Start] and $1f;
   RefIdc:=(aBlob^[Start] shr 5) and 3;
   RBSPLength:=ToRBSP(PpvUInt8Array(@aBlob^[Start+1]),Stop-Start-1,PpvUInt8Array(@RBSP[0]));
   case NALType of
    7:begin
     ParseSPS(PpvUInt8Array(@RBSP[0]),RBSPLength,aSPS);
     HaveSPS:=true;
    end;
    8:begin
     ParsePPS(PpvUInt8Array(@RBSP[0]),RBSPLength,aPPS);
     HavePPS:=true;
    end;
    1,5:begin
     if HaveSPS and HavePPS then begin
      ParseSlice(PpvUInt8Array(@RBSP[0]),RBSPLength,aSPS,aPPS,NALType,RefIdc,Slice);
      POC:=ComputePOC(aSPS,Slice,PreviousMSB,PreviousLSB);
      if FrameCount>=length(aFrames) then begin
       SetLength(aFrames,length(aFrames)*2);
      end;
      aFrames[FrameCount].NALOffset:=Position; // from the start code
      aFrames[FrameCount].NALLength:=Stop-Position;
      aFrames[FrameCount].IDR:=Slice.IDR;
      aFrames[FrameCount].FrameNum:=Slice.FrameNum;
      aFrames[FrameCount].POC:=POC;
      aFrames[FrameCount].SliceType:=Slice.SliceType;
      aFrames[FrameCount].RefIdc:=RefIdc;
      aFrames[FrameCount].LongTermReferenceFlag:=Slice.LongTermReferenceFlag;
      aFrames[FrameCount].AdaptiveMarking:=Slice.AdaptiveMarking;
      aFrames[FrameCount].MmcoCount:=Slice.MmcoCount;
      aFrames[FrameCount].Mmco:=Slice.Mmco;
      inc(FrameCount);
     end;
    end;
   end;
   Position:=Stop;
  end else begin
   inc(Position);
  end;
 end;
 if not (HaveSPS and HavePPS and (FrameCount>0)) then begin
  raise EpvVideoH264.Create('missing SPS/PPS/slices in the embedded H.264 stream');
 end;
 SetLength(aFrames,FrameCount);
 result:=FrameCount;
end;

constructor TpvVideoH264Decoder.Create(const aStream:TStream;const aDevice:TpvVulkanDevice);
var Size:TpvSizeInt;
begin
 inherited Create;
 fDevice:=aDevice;
 Size:=aStream.Size-aStream.Position;
 if Size<=0 then begin
  raise EpvVideoH264.Create('empty H.264 stream');
 end;
 SetLength(fBlob,Size);
 aStream.ReadBuffer(fBlob[0],Size);
 fFrameCount:=ParseAnnexB(PpvUInt8Array(@fBlob[0]),TpvSizeUInt(Size),fSPS,fPPS,fFrames);
 fWidth:=fSPS.Width;
 fHeight:=fSPS.Height;
 fCodedWidth:=(fSPS.WidthInMBsMinus1+1)*16;
 fCodedHeight:=(fSPS.HeightInMapUnitsMinus1+1)*16;
 PrepareDisplayOrder;
{$ifdef VkVideo}
 fSession:=VK_NULL_HANDLE;
 fSessionParameters:=VK_NULL_HANDLE;
 fSessionMemories:=nil;
 fMaxDpbSlots:=0;
 fMaxActiveReferences:=0;
 BuildStdParameterSets;
 CreateVideoSession;
 CreateDecodeResources;
{$endif}
end;

destructor TpvVideoH264Decoder.Destroy;
{$ifdef VkVideo}
var Index:TpvInt32;
{$endif}
begin
{$ifdef VkVideo}
 DestroyDecodeResources;
 if fSessionParameters<>VK_NULL_HANDLE then begin
  fDevice.Commands.DestroyVideoSessionParametersKHR(fDevice.Handle,fSessionParameters,nil);
 end;
 if fSession<>VK_NULL_HANDLE then begin
  fDevice.Commands.DestroyVideoSessionKHR(fDevice.Handle,fSession,nil);
 end;
 for Index:=0 to length(fSessionMemories)-1 do begin
  if fSessionMemories[Index]<>VK_NULL_HANDLE then begin
   fDevice.Commands.FreeMemory(fDevice.Handle,fSessionMemories[Index],nil);
  end;
 end;
 fSessionMemories:=nil;
{$endif}
 fDisplayOrder:=nil;
 fFrameKey:=nil;
 fFrameGOP:=nil;
 fFrames:=nil;
 fBlob:=nil;
 inherited Destroy;
end;

procedure TpvVideoH264Decoder.PrepareDisplayOrder;
var Index,GOPIndex:TpvInt32;
begin

 // Per coded (decode-order) frame: assign a GOP index (increments at every non-first IDR) and a display key
 // (gop_index in the high bits, biased POC in the low bits) that totally orders frames across GOP boundaries.
 // The online reorder buffer in EnsureDisplayFrame bumps frames in ascending key order - exactly the C reference.
 SetLength(fFrameGOP,fFrameCount);
 SetLength(fFrameKey,fFrameCount);
 GOPIndex:=0;
 for Index:=0 to fFrameCount-1 do begin
  if (fFrames[Index].IDR<>0) and (Index>0) then begin
   inc(GOPIndex);
  end;
  fFrameGOP[Index]:=GOPIndex;
  fFrameKey[Index]:=(TpvUInt64(TpvUInt32(GOPIndex)) shl 24) or TpvUInt64(TpvUInt32(fFrames[Index].POC+$800000));
 end;

end;

{$ifdef VkVideo}

function TpvVideoH264Decoder.FindMemoryType(const aTypeBits:TVkUInt32;const aWanted:TVkMemoryPropertyFlags):TpvUInt32;
var Index:TpvUInt32;
begin

 // First pass: a memory type allowed by aTypeBits that also satisfies the wanted property flags.
 for Index:=0 to fDevice.PhysicalDevice.MemoryProperties.memoryTypeCount-1 do begin
  if ((aTypeBits and (TVkUInt32(1) shl Index))<>0) and
     ((fDevice.PhysicalDevice.MemoryProperties.memoryTypes[Index].propertyFlags and aWanted)=aWanted) then begin
   result:=Index;
   exit;
  end;
 end;

 // Fallback (matches the C reference): some video-session bind slots only allow host-cached memory and
 // never DEVICE_LOCAL, so when nothing satisfies the wanted flags just take the first allowed type.
 for Index:=0 to fDevice.PhysicalDevice.MemoryProperties.memoryTypeCount-1 do begin
  if (aTypeBits and (TVkUInt32(1) shl Index))<>0 then begin
   result:=Index;
   exit;
  end;
 end;

 raise EpvVideoH264.Create('no suitable memory type for H.264 decode');
end;

procedure TpvVideoH264Decoder.BuildStdParameterSets;
begin
 FillChar(fStdSPS,SizeOf(fStdSPS),#0);
 fStdSPS.profile_idc:=STD_VIDEO_H264_PROFILE_IDC_HIGH;
 fStdSPS.level_idc:=STD_VIDEO_H264_LEVEL_IDC_4_0;
 fStdSPS.seq_parameter_set_id:=fSPS.SPSId;
 fStdSPS.chroma_format_idc:=TStdVideoH264ChromaFormatIdc(fSPS.ChromaFormatIDC);
 fStdSPS.log2_max_frame_num_minus4:=fSPS.Log2MaxFrameNumMinus4;
 fStdSPS.pic_order_cnt_type:=TStdVideoH264PocType(fSPS.POCType);
 fStdSPS.log2_max_pic_order_cnt_lsb_minus4:=fSPS.Log2MaxPOCLSBMinus4;
 fStdSPS.max_num_ref_frames:=fSPS.MaxNumRef;
 fStdSPS.pic_width_in_mbs_minus1:=fSPS.WidthInMBsMinus1;
 fStdSPS.pic_height_in_map_units_minus1:=fSPS.HeightInMapUnitsMinus1;
 fStdSPS.frame_crop_left_offset:=fSPS.CropLeft;
 fStdSPS.frame_crop_right_offset:=fSPS.CropRight;
 fStdSPS.frame_crop_top_offset:=fSPS.CropTop;
 fStdSPS.frame_crop_bottom_offset:=fSPS.CropBottom;
 fStdSPS.flags.frame_mbs_only_flag:=fSPS.FrameMBsOnly;
 fStdSPS.flags.direct_8x8_inference_flag:=fSPS.Direct8x8;
 fStdSPS.flags.frame_cropping_flag:=ord((fSPS.CropLeft<>0) or (fSPS.CropRight<>0) or (fSPS.CropTop<>0) or (fSPS.CropBottom<>0));
 FillChar(fStdPPS,SizeOf(fStdPPS),#0);
 fStdPPS.seq_parameter_set_id:=fPPS.SPSId;
 fStdPPS.pic_parameter_set_id:=fPPS.PPSId;
 fStdPPS.num_ref_idx_l0_default_active_minus1:=fPPS.NumRefL0Minus1;
 fStdPPS.num_ref_idx_l1_default_active_minus1:=fPPS.NumRefL1Minus1;
 fStdPPS.weighted_bipred_idc:=TStdVideoH264WeightedBipredIdc(fPPS.WeightedBipred);
 fStdPPS.pic_init_qp_minus26:=fPPS.PicInitQPMinus26;
 fStdPPS.pic_init_qs_minus26:=fPPS.PicInitQSMinus26;
 fStdPPS.chroma_qp_index_offset:=fPPS.ChromaQPOffset;
 fStdPPS.second_chroma_qp_index_offset:=fPPS.SecondChromaQPOffset;
 fStdPPS.flags.entropy_coding_mode_flag:=fPPS.EntropyCodingMode;
 fStdPPS.flags.weighted_pred_flag:=fPPS.WeightedPred;
 fStdPPS.flags.deblocking_filter_control_present_flag:=fPPS.DeblockingControl;
 fStdPPS.flags.constrained_intra_pred_flag:=fPPS.ConstrainedIntra;
 fStdPPS.flags.bottom_field_pic_order_in_frame_present_flag:=fPPS.BottomFieldPOC;
 fStdPPS.flags.transform_8x8_mode_flag:=fPPS.Transform8x8;
 fStdPPS.flags.redundant_pic_cnt_present_flag:=fPPS.Redundant;
end;

procedure TpvVideoH264Decoder.CreateVideoSession;
var H264Profile:TVkVideoDecodeH264ProfileInfoKHR;
    Profile:TVkVideoProfileInfoKHR;
    H264Caps:TVkVideoDecodeH264CapabilitiesKHR;
    DecodeCaps:TVkVideoDecodeCapabilitiesKHR;
    Caps:TVkVideoCapabilitiesKHR;
    StdHeader:TVkExtensionProperties;
    SessionInfo:TVkVideoSessionCreateInfoKHR;
    AddInfo:TVkVideoDecodeH264SessionParametersAddInfoKHR;
    H264Params:TVkVideoDecodeH264SessionParametersCreateInfoKHR;
    ParametersInfo:TVkVideoSessionParametersCreateInfoKHR;
    MemoryRequirementCount,Index:TpvUInt32;
    MemoryRequirements:array of TVkVideoSessionMemoryRequirementsKHR;
    Binds:array of TVkBindVideoSessionMemoryInfoKHR;
    AllocateInfo:TVkMemoryAllocateInfo;
    Memory:TVkDeviceMemory;
    ExtensionName:TpvRawByteString;
    Coded:TVkExtent2D;
begin

 FillChar(H264Profile,SizeOf(H264Profile),#0);
 H264Profile.sType:=VK_STRUCTURE_TYPE_VIDEO_DECODE_H264_PROFILE_INFO_KHR;
 H264Profile.stdProfileIdc:=STD_VIDEO_H264_PROFILE_IDC_HIGH;
 H264Profile.pictureLayout:=VK_VIDEO_DECODE_H264_PICTURE_LAYOUT_PROGRESSIVE_KHR;

 FillChar(Profile,SizeOf(Profile),#0);
 Profile.sType:=VK_STRUCTURE_TYPE_VIDEO_PROFILE_INFO_KHR;
 Profile.pNext:=@H264Profile;
 Profile.videoCodecOperation:=VK_VIDEO_CODEC_OPERATION_DECODE_H264_BIT_KHR;
 Profile.chromaSubsampling:=TVkVideoChromaSubsamplingFlagsKHR(VK_VIDEO_CHROMA_SUBSAMPLING_420_BIT_KHR);
 Profile.lumaBitDepth:=TVkVideoComponentBitDepthFlagsKHR(VK_VIDEO_COMPONENT_BIT_DEPTH_8_BIT_KHR);
 Profile.chromaBitDepth:=TVkVideoComponentBitDepthFlagsKHR(VK_VIDEO_COMPONENT_BIT_DEPTH_8_BIT_KHR);

 FillChar(H264Caps,SizeOf(H264Caps),#0);
 H264Caps.sType:=VK_STRUCTURE_TYPE_VIDEO_DECODE_H264_CAPABILITIES_KHR;
 FillChar(DecodeCaps,SizeOf(DecodeCaps),#0);
 DecodeCaps.sType:=VK_STRUCTURE_TYPE_VIDEO_DECODE_CAPABILITIES_KHR;
 DecodeCaps.pNext:=@H264Caps;
 FillChar(Caps,SizeOf(Caps),#0);
 Caps.sType:=VK_STRUCTURE_TYPE_VIDEO_CAPABILITIES_KHR;
 Caps.pNext:=@DecodeCaps;
 if fDevice.Instance.Commands.GetPhysicalDeviceVideoCapabilitiesKHR(fDevice.PhysicalDevice.Handle,@Profile,@Caps)<>VK_SUCCESS then begin
  raise EpvVideoH264.Create('vkGetPhysicalDeviceVideoCapabilitiesKHR failed');
 end;

 Coded.width:=fCodedWidth;
 Coded.height:=fCodedHeight;
 fMaxDpbSlots:=Caps.maxDpbSlots;
 if fMaxDpbSlots>17 then begin
  fMaxDpbSlots:=17;
 end;
 fMaxActiveReferences:=Caps.maxActiveReferencePictures;

 ExtensionName:=VK_STD_VULKAN_VIDEO_CODEC_H264_DECODE_EXTENSION_NAME;
 FillChar(StdHeader,SizeOf(StdHeader),#0);
 if length(ExtensionName)>0 then begin
  Move(ExtensionName[1],StdHeader.extensionName[0],length(ExtensionName));
 end;
 StdHeader.specVersion:=VK_STD_VULKAN_VIDEO_CODEC_H264_DECODE_SPEC_VERSION;

 FillChar(SessionInfo,SizeOf(SessionInfo),#0);
 SessionInfo.sType:=VK_STRUCTURE_TYPE_VIDEO_SESSION_CREATE_INFO_KHR;
 SessionInfo.queueFamilyIndex:=fDevice.VideoDecodeQueueFamilyIndex;
 SessionInfo.pVideoProfile:=@Profile;
 SessionInfo.pictureFormat:=VK_FORMAT_G8_B8R8_2PLANE_420_UNORM;
 SessionInfo.maxCodedExtent:=Coded;
 SessionInfo.referencePictureFormat:=VK_FORMAT_G8_B8R8_2PLANE_420_UNORM;
 SessionInfo.maxDpbSlots:=fMaxDpbSlots;
 SessionInfo.maxActiveReferencePictures:=fMaxActiveReferences;
 SessionInfo.pStdHeaderVersion:=@StdHeader;
 if fDevice.Commands.CreateVideoSessionKHR(fDevice.Handle,@SessionInfo,nil,@fSession)<>VK_SUCCESS then begin
  raise EpvVideoH264.Create('vkCreateVideoSessionKHR failed');
 end;

 MemoryRequirementCount:=0;
 fDevice.Commands.GetVideoSessionMemoryRequirementsKHR(fDevice.Handle,fSession,@MemoryRequirementCount,nil);
 SetLength(MemoryRequirements,MemoryRequirementCount);
 SetLength(Binds,MemoryRequirementCount);
 SetLength(fSessionMemories,MemoryRequirementCount);
 for Index:=0 to MemoryRequirementCount-1 do begin
  FillChar(MemoryRequirements[Index],SizeOf(TVkVideoSessionMemoryRequirementsKHR),#0);
  MemoryRequirements[Index].sType:=VK_STRUCTURE_TYPE_VIDEO_SESSION_MEMORY_REQUIREMENTS_KHR;
 end;
 fDevice.Commands.GetVideoSessionMemoryRequirementsKHR(fDevice.Handle,fSession,@MemoryRequirementCount,@MemoryRequirements[0]);
{$ifdef FWVH264Debug}
 pvApplication.Log(LOG_VERBOSE,'TpvVideoH264Decoder','  [h264dbg] SizeOf(TVkVideoSessionMemoryRequirementsKHR)='+IntToStr(SizeOf(TVkVideoSessionMemoryRequirementsKHR))+' memReqCount='+IntToStr(MemoryRequirementCount));
 for Index:=0 to MemoryRequirementCount-1 do begin
  pvApplication.Log(LOG_VERBOSE,'TpvVideoH264Decoder','  [h264dbg] req['+IntToStr(Index)+'] bindIndex='+IntToStr(MemoryRequirements[Index].memoryBindIndex)+
                    ' typeBits=$'+IntToHex(MemoryRequirements[Index].memoryRequirements.memoryTypeBits,8)+
                    ' size='+IntToStr(MemoryRequirements[Index].memoryRequirements.size));
 end;
 pvApplication.Log(LOG_VERBOSE,'TpvVideoH264Decoder','  [h264dbg] memoryTypeCount='+IntToStr(fDevice.PhysicalDevice.MemoryProperties.memoryTypeCount));
 for Index:=0 to fDevice.PhysicalDevice.MemoryProperties.memoryTypeCount-1 do begin
  pvApplication.Log(LOG_VERBOSE,'TpvVideoH264Decoder','  [h264dbg] memType['+IntToStr(Index)+'] propertyFlags=$'+IntToHex(fDevice.PhysicalDevice.MemoryProperties.memoryTypes[Index].propertyFlags,8)+
                    ' heapIndex='+IntToStr(fDevice.PhysicalDevice.MemoryProperties.memoryTypes[Index].heapIndex));
 end;
{$endif}
 for Index:=0 to MemoryRequirementCount-1 do begin
  FillChar(AllocateInfo,SizeOf(AllocateInfo),#0);
  AllocateInfo.sType:=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
  AllocateInfo.allocationSize:=MemoryRequirements[Index].memoryRequirements.size;
  AllocateInfo.memoryTypeIndex:=FindMemoryType(MemoryRequirements[Index].memoryRequirements.memoryTypeBits,TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT));
  Memory:=VK_NULL_HANDLE;
  if fDevice.Commands.AllocateMemory(fDevice.Handle,@AllocateInfo,nil,@Memory)<>VK_SUCCESS then begin
   raise EpvVideoH264.Create('video session memory allocation failed');
  end;
  fSessionMemories[Index]:=Memory;
  FillChar(Binds[Index],SizeOf(TVkBindVideoSessionMemoryInfoKHR),#0);
  Binds[Index].sType:=VK_STRUCTURE_TYPE_BIND_VIDEO_SESSION_MEMORY_INFO_KHR;
  Binds[Index].memoryBindIndex:=MemoryRequirements[Index].memoryBindIndex;
  Binds[Index].memory:=Memory;
  Binds[Index].memorySize:=AllocateInfo.allocationSize;
 end;
 if MemoryRequirementCount>0 then begin
  if fDevice.Commands.BindVideoSessionMemoryKHR(fDevice.Handle,fSession,MemoryRequirementCount,@Binds[0])<>VK_SUCCESS then begin
   raise EpvVideoH264.Create('vkBindVideoSessionMemoryKHR failed');
  end;
 end;

 FillChar(AddInfo,SizeOf(AddInfo),#0);
 AddInfo.sType:=VK_STRUCTURE_TYPE_VIDEO_DECODE_H264_SESSION_PARAMETERS_ADD_INFO_KHR;
 AddInfo.stdSPSCount:=1;
 AddInfo.pStdSPSs:=@fStdSPS;
 AddInfo.stdPPSCount:=1;
 AddInfo.pStdPPSs:=@fStdPPS;
 FillChar(H264Params,SizeOf(H264Params),#0);
 H264Params.sType:=VK_STRUCTURE_TYPE_VIDEO_DECODE_H264_SESSION_PARAMETERS_CREATE_INFO_KHR;
 H264Params.maxStdSPSCount:=1;
 H264Params.maxStdPPSCount:=1;
 H264Params.pParametersAddInfo:=@AddInfo;
 FillChar(ParametersInfo,SizeOf(ParametersInfo),#0);
 ParametersInfo.sType:=VK_STRUCTURE_TYPE_VIDEO_SESSION_PARAMETERS_CREATE_INFO_KHR;
 ParametersInfo.pNext:=@H264Params;
 ParametersInfo.videoSession:=fSession;
 if fDevice.Commands.CreateVideoSessionParametersKHR(fDevice.Handle,@ParametersInfo,nil,@fSessionParameters)<>VK_SUCCESS then begin
  raise EpvVideoH264.Create('vkCreateVideoSessionParametersKHR failed');
 end;

 // Keep a persistent copy of the profile chain (the DPB images + bitstream buffer reference it via pNext) and
 // capture the caps-derived sizing the decode loop needs (NV12 format, bitstream alignment, slot / ref counts).
 fProfile:=Profile;
 fH264Profile:=H264Profile;
 fProfile.pNext:=@fH264Profile;
 FillChar(fProfileList,SizeOf(fProfileList),#0);
 fProfileList.sType:=VK_STRUCTURE_TYPE_VIDEO_PROFILE_LIST_INFO_KHR;
 fProfileList.profileCount:=1;
 fProfileList.pProfiles:=@fProfile;
 fNv12Format:=VK_FORMAT_G8_B8R8_2PLANE_420_UNORM;
 fBitstreamAlignment:=Caps.minBitstreamBufferSizeAlignment;
 if fBitstreamAlignment<1 then begin
  fBitstreamAlignment:=1;
 end;
 fSlotCount:=fMaxDpbSlots;
 if fSlotCount>8 then begin
  fSlotCount:=8;
 end;
 fMaxRef:=fSPS.MaxNumRef;
 if fMaxRef<1 then begin
  fMaxRef:=1;
 end;
 if fMaxRef>(fSlotCount-1) then begin
  fMaxRef:=fSlotCount-1;
 end;
 fMaxFrameNum:=TpvInt32(1) shl (fSPS.Log2MaxFrameNumMinus4+4);

end;

procedure TpvVideoH264Decoder.RecordImageBarrier(const aCommandBuffer:TVkCommandBuffer;const aImage:TVkImage;
                                                 const aOldLayout,aNewLayout:TVkImageLayout;
                                                 const aSrcAccess,aDstAccess:TVkAccessFlags2;
                                                 const aSrcStage,aDstStage:TVkPipelineStageFlags2);
var Barrier:TVkImageMemoryBarrier2;
    Dependency:TVkDependencyInfo;
begin
 FillChar(Barrier,SizeOf(Barrier),#0);
 Barrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2;
 Barrier.srcStageMask:=aSrcStage;
 Barrier.srcAccessMask:=aSrcAccess;
 Barrier.dstStageMask:=aDstStage;
 Barrier.dstAccessMask:=aDstAccess;
 Barrier.oldLayout:=aOldLayout;
 Barrier.newLayout:=aNewLayout;
 Barrier.srcQueueFamilyIndex:=TVkUInt32(VK_QUEUE_FAMILY_IGNORED);
 Barrier.dstQueueFamilyIndex:=TVkUInt32(VK_QUEUE_FAMILY_IGNORED);
 Barrier.image:=aImage;
 Barrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 Barrier.subresourceRange.baseMipLevel:=0;
 Barrier.subresourceRange.levelCount:=1;
 Barrier.subresourceRange.baseArrayLayer:=0;
 Barrier.subresourceRange.layerCount:=1;
 FillChar(Dependency,SizeOf(Dependency),#0);
 Dependency.sType:=VK_STRUCTURE_TYPE_DEPENDENCY_INFO;
 Dependency.imageMemoryBarrierCount:=1;
 Dependency.pImageMemoryBarriers:=@Barrier;
 fDevice.Commands.CmdPipelineBarrier2(aCommandBuffer,@Dependency);
end;

procedure TpvVideoH264Decoder.CreateDecodeResources;
var Index,MaxFrameLength,Concurrent,UniversalFamily,VideoFamily:TpvInt32;
    Coded:TVkExtent2D;
    SharedFamilies:array[0..1] of TVkUInt32;
    ImageInfo:TVkImageCreateInfo;
    ViewInfo:TVkImageViewCreateInfo;
    ViewUsage:TVkImageViewUsageCreateInfo;
    PlaneView:TVkImageViewCreateInfo;
    MemoryRequirements:TVkMemoryRequirements;
    AllocateInfo:TVkMemoryAllocateInfo;
    Memory:TVkDeviceMemory;
    BufferInfo:TVkBufferCreateInfo;
    SamplerInfo:TVkSamplerCreateInfo;
    ModuleInfo:TVkShaderModuleCreateInfo;
    Bindings:array[0..2] of TVkDescriptorSetLayoutBinding;
    SetLayoutInfo:TVkDescriptorSetLayoutCreateInfo;
    PushRange:TVkPushConstantRange;
    PipelineLayoutInfo:TVkPipelineLayoutCreateInfo;
    PipelineInfo:TVkComputePipelineCreateInfo;
    DescriptorSizes:array[0..1] of TVkDescriptorPoolSize;
    DescriptorPoolInfo:TVkDescriptorPoolCreateInfo;
    SetAllocateInfo:TVkDescriptorSetAllocateInfo;
    LumaInfo,ChromaInfo,DestinationInfo:TVkDescriptorImageInfo;
    Writes:array[0..2] of TVkWriteDescriptorSet;
    CommandPoolInfo:TVkCommandPoolCreateInfo;
    CommandBufferAllocateInfo:TVkCommandBufferAllocateInfo;
    FenceInfo:TVkFenceCreateInfo;
    BeginInfo:TVkCommandBufferBeginInfo;
    SubmitInfo:TVkSubmitInfo;
    QueueFamilyIndex:TVkUInt32;
    RequiresDedicated,PrefersDedicated:boolean;
    MemoryBlockFlags:TpvVulkanDeviceMemoryBlockFlags;
    ImageHandle:TVkImage;
begin

 Coded.width:=fCodedWidth;
 Coded.height:=fCodedHeight;
 VideoFamily:=fDevice.VideoDecodeQueueFamilyIndex;
 UniversalFamily:=fDevice.UniversalQueueFamilyIndex;
 Concurrent:=ord(VideoFamily<>UniversalFamily);

 // ---- DPB pool: slot_count NV12 reference / decode-target images ----
 SetLength(fDPBImages,fSlotCount);
 SetLength(fDPBViews,fSlotCount);
 SetLength(fDPBMemories,fSlotCount);
 SetLength(fDPBLayouts,fSlotCount);
 SetLength(fPictureResources,fSlotCount);
 SetLength(fSlotUsed,fSlotCount);
 SetLength(fSlotPOC,fSlotCount);
 SetLength(fSlotFrameNum,fSlotCount);
 SetLength(fSlotLongTerm,fSlotCount);
 SetLength(fSlotLongTermFrameIdx,fSlotCount);
 fMaxLongTermFrameIdx:=-1;
 QueueFamilyIndex:=VideoFamily;
 for Index:=0 to fSlotCount-1 do begin
  FillChar(ImageInfo,SizeOf(ImageInfo),#0);
  ImageInfo.sType:=VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
  ImageInfo.pNext:=@fProfileList;
  ImageInfo.imageType:=VK_IMAGE_TYPE_2D;
  ImageInfo.format:=fNv12Format;
  ImageInfo.extent.width:=fCodedWidth;
  ImageInfo.extent.height:=fCodedHeight;
  ImageInfo.extent.depth:=1;
  ImageInfo.mipLevels:=1;
  ImageInfo.arrayLayers:=1;
  ImageInfo.samples:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT);
  ImageInfo.tiling:=VK_IMAGE_TILING_OPTIMAL;
  ImageInfo.usage:=TVkImageUsageFlags(VK_IMAGE_USAGE_VIDEO_DECODE_DPB_BIT_KHR) or
                   TVkImageUsageFlags(VK_IMAGE_USAGE_VIDEO_DECODE_DST_BIT_KHR) or
                   TVkImageUsageFlags(VK_IMAGE_USAGE_TRANSFER_SRC_BIT);
  ImageInfo.queueFamilyIndexCount:=1;
  ImageInfo.pQueueFamilyIndices:=@QueueFamilyIndex;
  if fDevice.Commands.CreateImage(fDevice.Handle,@ImageInfo,nil,@fDPBImages[Index])<>VK_SUCCESS then begin
   raise EpvVideoH264.Create('DPB image creation failed');
  end;
  fDevice.Commands.GetImageMemoryRequirements(fDevice.Handle,fDPBImages[Index],@MemoryRequirements);
  FillChar(AllocateInfo,SizeOf(AllocateInfo),#0);
  AllocateInfo.sType:=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
  AllocateInfo.allocationSize:=MemoryRequirements.size;
  AllocateInfo.memoryTypeIndex:=FindMemoryType(MemoryRequirements.memoryTypeBits,TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT));
  Memory:=VK_NULL_HANDLE;
  if fDevice.Commands.AllocateMemory(fDevice.Handle,@AllocateInfo,nil,@Memory)<>VK_SUCCESS then begin
   raise EpvVideoH264.Create('DPB image memory allocation failed');
  end;
  fDPBMemories[Index]:=Memory;
  fDevice.Commands.BindImageMemory(fDevice.Handle,fDPBImages[Index],Memory,0);
  FillChar(ViewUsage,SizeOf(ViewUsage),#0);
  ViewUsage.sType:=VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO;
  ViewUsage.usage:=TVkImageUsageFlags(VK_IMAGE_USAGE_VIDEO_DECODE_DPB_BIT_KHR) or
                   TVkImageUsageFlags(VK_IMAGE_USAGE_VIDEO_DECODE_DST_BIT_KHR);
  FillChar(ViewInfo,SizeOf(ViewInfo),#0);
  ViewInfo.sType:=VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
  ViewInfo.pNext:=@ViewUsage;
  ViewInfo.image:=fDPBImages[Index];
  ViewInfo.viewType:=VK_IMAGE_VIEW_TYPE_2D;
  ViewInfo.format:=fNv12Format;
  ViewInfo.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
  ViewInfo.subresourceRange.baseMipLevel:=0;
  ViewInfo.subresourceRange.levelCount:=1;
  ViewInfo.subresourceRange.baseArrayLayer:=0;
  ViewInfo.subresourceRange.layerCount:=1;
  if fDevice.Commands.CreateImageView(fDevice.Handle,@ViewInfo,nil,@fDPBViews[Index])<>VK_SUCCESS then begin
   raise EpvVideoH264.Create('DPB image view creation failed');
  end;
  fDPBLayouts[Index]:=VK_IMAGE_LAYOUT_UNDEFINED;
  fSlotUsed[Index]:=0;
  FillChar(fPictureResources[Index],SizeOf(TVkVideoPictureResourceInfoKHR),#0);
  fPictureResources[Index].sType:=VK_STRUCTURE_TYPE_VIDEO_PICTURE_RESOURCE_INFO_KHR;
  fPictureResources[Index].codedExtent:=Coded;
  fPictureResources[Index].imageViewBinding:=fDPBViews[Index];
 end;

 // ---- host-visible bitstream upload buffer (sized to the largest frame, alignment-rounded) ----
 MaxFrameLength:=0;
 for Index:=0 to fFrameCount-1 do begin
  if TpvInt32(fFrames[Index].NALLength+1)>MaxFrameLength then begin
   MaxFrameLength:=TpvInt32(fFrames[Index].NALLength+1);
  end;
 end;
 fBitstreamLength:=((TVkDeviceSize(MaxFrameLength)+fBitstreamAlignment-1) div fBitstreamAlignment)*fBitstreamAlignment;
 FillChar(BufferInfo,SizeOf(BufferInfo),#0);
 BufferInfo.sType:=VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
 BufferInfo.pNext:=@fProfileList;
 BufferInfo.size:=fBitstreamLength;
 BufferInfo.usage:=TVkBufferUsageFlags(VK_BUFFER_USAGE_VIDEO_DECODE_SRC_BIT_KHR);
 if fDevice.Commands.CreateBuffer(fDevice.Handle,@BufferInfo,nil,@fBitstreamBuffer)<>VK_SUCCESS then begin
  raise EpvVideoH264.Create('bitstream buffer creation failed');
 end;
 fDevice.Commands.GetBufferMemoryRequirements(fDevice.Handle,fBitstreamBuffer,@MemoryRequirements);
 FillChar(AllocateInfo,SizeOf(AllocateInfo),#0);
 AllocateInfo.sType:=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
 AllocateInfo.allocationSize:=MemoryRequirements.size;
 AllocateInfo.memoryTypeIndex:=FindMemoryType(MemoryRequirements.memoryTypeBits,
                                              TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or
                                              TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT));
 fBitstreamMemory:=VK_NULL_HANDLE;
 if fDevice.Commands.AllocateMemory(fDevice.Handle,@AllocateInfo,nil,@fBitstreamMemory)<>VK_SUCCESS then begin
  raise EpvVideoH264.Create('bitstream buffer memory allocation failed');
 end;
 fDevice.Commands.BindBufferMemory(fDevice.Handle,fBitstreamBuffer,fBitstreamMemory,0);
 fBitstreamMap:=nil;
 if fDevice.Commands.MapMemory(fDevice.Handle,fBitstreamMemory,0,TVkDeviceSize(VK_WHOLE_SIZE),0,@fBitstreamMap)<>VK_SUCCESS then begin
  raise EpvVideoH264.Create('bitstream buffer map failed');
 end;

 // ---- sampled NV12 stage image (MUTABLE_FORMAT for the R8 / R8G8 plane views) + sampler ----
 SharedFamilies[0]:=VideoFamily;
 SharedFamilies[1]:=UniversalFamily;
 FillChar(ImageInfo,SizeOf(ImageInfo),#0);
 ImageInfo.sType:=VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
 ImageInfo.flags:=TVkImageCreateFlags(VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT);
 ImageInfo.imageType:=VK_IMAGE_TYPE_2D;
 ImageInfo.format:=fNv12Format;
 ImageInfo.extent.width:=fCodedWidth;
 ImageInfo.extent.height:=fCodedHeight;
 ImageInfo.extent.depth:=1;
 ImageInfo.mipLevels:=1;
 ImageInfo.arrayLayers:=1;
 ImageInfo.samples:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT);
 ImageInfo.tiling:=VK_IMAGE_TILING_OPTIMAL;
 ImageInfo.usage:=TVkImageUsageFlags(VK_IMAGE_USAGE_TRANSFER_DST_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT);
 if Concurrent<>0 then begin
  ImageInfo.sharingMode:=VK_SHARING_MODE_CONCURRENT;
  ImageInfo.queueFamilyIndexCount:=2;
  ImageInfo.pQueueFamilyIndices:=@SharedFamilies[0];
 end;
 if fDevice.Commands.CreateImage(fDevice.Handle,@ImageInfo,nil,@fStageImage)<>VK_SUCCESS then begin
  raise EpvVideoH264.Create('stage image creation failed');
 end;
 fDevice.Commands.GetImageMemoryRequirements(fDevice.Handle,fStageImage,@MemoryRequirements);
 FillChar(AllocateInfo,SizeOf(AllocateInfo),#0);
 AllocateInfo.sType:=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
 AllocateInfo.allocationSize:=MemoryRequirements.size;
 AllocateInfo.memoryTypeIndex:=FindMemoryType(MemoryRequirements.memoryTypeBits,TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT));
 fStageMemory:=VK_NULL_HANDLE;
 if fDevice.Commands.AllocateMemory(fDevice.Handle,@AllocateInfo,nil,@fStageMemory)<>VK_SUCCESS then begin
  raise EpvVideoH264.Create('stage image memory allocation failed');
 end;
 fDevice.Commands.BindImageMemory(fDevice.Handle,fStageImage,fStageMemory,0);
 fStageLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
 FillChar(PlaneView,SizeOf(PlaneView),#0);
 PlaneView.sType:=VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
 PlaneView.image:=fStageImage;
 PlaneView.viewType:=VK_IMAGE_VIEW_TYPE_2D;
 PlaneView.format:=VK_FORMAT_R8_UNORM;
 PlaneView.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_PLANE_0_BIT);
 PlaneView.subresourceRange.baseMipLevel:=0;
 PlaneView.subresourceRange.levelCount:=1;
 PlaneView.subresourceRange.baseArrayLayer:=0;
 PlaneView.subresourceRange.layerCount:=1;
 if fDevice.Commands.CreateImageView(fDevice.Handle,@PlaneView,nil,@fStageLumaView)<>VK_SUCCESS then begin
  raise EpvVideoH264.Create('stage luma view creation failed');
 end;
 PlaneView.format:=VK_FORMAT_R8G8_UNORM;
 PlaneView.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_PLANE_1_BIT);
 if fDevice.Commands.CreateImageView(fDevice.Handle,@PlaneView,nil,@fStageChromaView)<>VK_SUCCESS then begin
  raise EpvVideoH264.Create('stage chroma view creation failed');
 end;
 FillChar(SamplerInfo,SizeOf(SamplerInfo),#0);
 SamplerInfo.sType:=VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO;
 SamplerInfo.magFilter:=VK_FILTER_NEAREST; // nv12rgb.comp box-upsamples chroma via texelFetch (matches ffmpeg's default)
 SamplerInfo.minFilter:=VK_FILTER_NEAREST;
 SamplerInfo.addressModeU:=VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
 SamplerInfo.addressModeV:=VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
 SamplerInfo.addressModeW:=VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
 if fDevice.Commands.CreateSampler(fDevice.Handle,@SamplerInfo,nil,@fSampler)<>VK_SUCCESS then begin
  raise EpvVideoH264.Create('sampler creation failed');
 end;

 // ---- RGBA output pool (REORDER + spare); all images live permanently in VK_IMAGE_LAYOUT_GENERAL ----
 // engine-managed TpvVulkanImage (handle-only ctor + a device-local memory block, mirroring the wavelet decoder).
 fPoolCount:=8+8; // REORDER(8) held + a few in-flight present slots + spare; matches the C pool budget
 SetLength(fPoolImages,fPoolCount);
 SetLength(fPoolViews,fPoolCount);
 SetLength(fPoolMemories,fPoolCount);
 SetLength(fPoolFree,fPoolCount);
 for Index:=0 to fPoolCount-1 do begin
  fPoolImages[Index]:=TpvVulkanImage.Create(fDevice,
                                            0,
                                            VK_IMAGE_TYPE_2D,
                                            VK_FORMAT_R8G8B8A8_UNORM,
                                            fWidth,fHeight,1,
                                            1,1,
                                            VK_SAMPLE_COUNT_1_BIT,
                                            VK_IMAGE_TILING_OPTIMAL,
                                            TVkImageUsageFlags(VK_IMAGE_USAGE_STORAGE_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_TRANSFER_SRC_BIT),
                                            VK_SHARING_MODE_EXCLUSIVE,
                                            [],
                                            VK_IMAGE_LAYOUT_UNDEFINED);
  MemoryRequirements:=fDevice.MemoryManager.GetImageMemoryRequirements(fPoolImages[Index].Handle,RequiresDedicated,PrefersDedicated);
  MemoryBlockFlags:=[];
  if RequiresDedicated then begin
   Include(MemoryBlockFlags,TpvVulkanDeviceMemoryBlockFlag.DedicatedAllocation);
  end else if PrefersDedicated then begin
   Include(MemoryBlockFlags,TpvVulkanDeviceMemoryBlockFlag.PreferDedicatedAllocation);
  end;
  ImageHandle:=fPoolImages[Index].Handle;
  fPoolMemories[Index]:=fDevice.MemoryManager.AllocateMemoryBlock(MemoryBlockFlags,
                                                                 MemoryRequirements.size,
                                                                 MemoryRequirements.alignment,
                                                                 MemoryRequirements.memoryTypeBits,
                                                                 TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                 0,0,0,0,0,0,0,
                                                                 TpvVulkanDeviceMemoryAllocationType.ImageOptimal,
                                                                 @ImageHandle,
                                                                 0,
                                                                 'FWV.h264.pool');
  if not assigned(fPoolMemories[Index]) then begin
   raise EpvVideoH264.Create('pool image memory allocation failed');
  end;
  VulkanCheckResult(fDevice.Commands.BindImageMemory(fDevice.Handle,fPoolImages[Index].Handle,fPoolMemories[Index].MemoryChunk.Handle,fPoolMemories[Index].Offset));
  fPoolViews[Index]:=TpvVulkanImageView.Create(fDevice,
                                               fPoolImages[Index],
                                               VK_IMAGE_VIEW_TYPE_2D,
                                               VK_FORMAT_R8G8B8A8_UNORM,
                                               VK_COMPONENT_SWIZZLE_IDENTITY,
                                               VK_COMPONENT_SWIZZLE_IDENTITY,
                                               VK_COMPONENT_SWIZZLE_IDENTITY,
                                               VK_COMPONENT_SWIZZLE_IDENTITY,
                                               TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                               0,1,0,1);
  fPoolFree[Index]:=Index;
 end;
 fFreeCount:=fPoolCount;

 // ---- single stable display image (SAMPLED for present-A texturing + TRANSFER_SRC for the present-B blit) ----
 // R8G8B8A8_SRGB so sampling / blitting yields linear values (the nv12rgb output is gamma-encoded display RGB);
 // the pool->display copy is a raw byte copy (UNORM and SRGB are copy-compatible), so the stored bytes are the
 // same gamma values - only the format's read-time decode differs. This makes the output linear-sRGB so the
 // engine's sRGB swapchain re-encodes it correctly (and it is directly usable as an sRGB texture in 3D).
 fDisplayImage:=TpvVulkanImage.Create(fDevice,
                                      0,
                                      VK_IMAGE_TYPE_2D,
                                      VK_FORMAT_R8G8B8A8_SRGB,
                                      fWidth,fHeight,1,
                                      1,1,
                                      VK_SAMPLE_COUNT_1_BIT,
                                      VK_IMAGE_TILING_OPTIMAL,
                                      TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_TRANSFER_SRC_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_TRANSFER_DST_BIT),
                                      VK_SHARING_MODE_EXCLUSIVE,
                                      [],
                                      VK_IMAGE_LAYOUT_UNDEFINED);
 MemoryRequirements:=fDevice.MemoryManager.GetImageMemoryRequirements(fDisplayImage.Handle,RequiresDedicated,PrefersDedicated);
 MemoryBlockFlags:=[];
 if RequiresDedicated then begin
  Include(MemoryBlockFlags,TpvVulkanDeviceMemoryBlockFlag.DedicatedAllocation);
 end else if PrefersDedicated then begin
  Include(MemoryBlockFlags,TpvVulkanDeviceMemoryBlockFlag.PreferDedicatedAllocation);
 end;
 ImageHandle:=fDisplayImage.Handle;
 fDisplayMemory:=fDevice.MemoryManager.AllocateMemoryBlock(MemoryBlockFlags,
                                                          MemoryRequirements.size,
                                                          MemoryRequirements.alignment,
                                                          MemoryRequirements.memoryTypeBits,
                                                          TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                          0,0,0,0,0,0,0,
                                                          TpvVulkanDeviceMemoryAllocationType.ImageOptimal,
                                                          @ImageHandle,
                                                          0,
                                                          'FWV.h264.display');
 if not assigned(fDisplayMemory) then begin
  raise EpvVideoH264.Create('display image memory allocation failed');
 end;
 VulkanCheckResult(fDevice.Commands.BindImageMemory(fDevice.Handle,fDisplayImage.Handle,fDisplayMemory.MemoryChunk.Handle,fDisplayMemory.Offset));
 fDisplayImageView:=TpvVulkanImageView.Create(fDevice,
                                              fDisplayImage,
                                              VK_IMAGE_VIEW_TYPE_2D,
                                              VK_FORMAT_R8G8B8A8_SRGB,
                                              VK_COMPONENT_SWIZZLE_IDENTITY,
                                              VK_COMPONENT_SWIZZLE_IDENTITY,
                                              VK_COMPONENT_SWIZZLE_IDENTITY,
                                              VK_COMPONENT_SWIZZLE_IDENTITY,
                                              TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                              0,1,0,1);
 fDisplayLayout:=VK_IMAGE_LAYOUT_UNDEFINED;

 // ---- nv12rgb compute pipeline ----
 FillChar(ModuleInfo,SizeOf(ModuleInfo),#0);
 ModuleInfo.sType:=VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
 ModuleInfo.codeSize:=FlexibleVideoNv12rgbSPIRVDataSize;
 ModuleInfo.pCode:=@FlexibleVideoNv12rgbSPIRVData[0];
 if fDevice.Commands.CreateShaderModule(fDevice.Handle,@ModuleInfo,nil,@fComputeModule)<>VK_SUCCESS then begin
  raise EpvVideoH264.Create('nv12rgb shader module creation failed');
 end;
 FillChar(Bindings,SizeOf(Bindings),#0);
 Bindings[0].binding:=0;
 Bindings[0].descriptorType:=VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
 Bindings[0].descriptorCount:=1;
 Bindings[0].stageFlags:=TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT);
 Bindings[1].binding:=1;
 Bindings[1].descriptorType:=VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
 Bindings[1].descriptorCount:=1;
 Bindings[1].stageFlags:=TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT);
 Bindings[2].binding:=2;
 Bindings[2].descriptorType:=VK_DESCRIPTOR_TYPE_STORAGE_IMAGE;
 Bindings[2].descriptorCount:=1;
 Bindings[2].stageFlags:=TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT);
 FillChar(SetLayoutInfo,SizeOf(SetLayoutInfo),#0);
 SetLayoutInfo.sType:=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
 SetLayoutInfo.bindingCount:=3;
 SetLayoutInfo.pBindings:=@Bindings[0];
 if fDevice.Commands.CreateDescriptorSetLayout(fDevice.Handle,@SetLayoutInfo,nil,@fSetLayout)<>VK_SUCCESS then begin
  raise EpvVideoH264.Create('nv12rgb descriptor set layout creation failed');
 end;
 FillChar(PushRange,SizeOf(PushRange),#0);
 PushRange.stageFlags:=TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT);
 PushRange.offset:=0;
 PushRange.size:=8;
 FillChar(PipelineLayoutInfo,SizeOf(PipelineLayoutInfo),#0);
 PipelineLayoutInfo.sType:=VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
 PipelineLayoutInfo.setLayoutCount:=1;
 PipelineLayoutInfo.pSetLayouts:=@fSetLayout;
 PipelineLayoutInfo.pushConstantRangeCount:=1;
 PipelineLayoutInfo.pPushConstantRanges:=@PushRange;
 if fDevice.Commands.CreatePipelineLayout(fDevice.Handle,@PipelineLayoutInfo,nil,@fComputeLayout)<>VK_SUCCESS then begin
  raise EpvVideoH264.Create('nv12rgb pipeline layout creation failed');
 end;
 FillChar(PipelineInfo,SizeOf(PipelineInfo),#0);
 PipelineInfo.sType:=VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO;
 PipelineInfo.stage.sType:=VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
 PipelineInfo.stage.stage:=TVkShaderStageFlagBits(VK_SHADER_STAGE_COMPUTE_BIT);
 PipelineInfo.stage.module:=fComputeModule;
 PipelineInfo.stage.pName:=PVkChar('main');
 PipelineInfo.layout:=fComputeLayout;
 if fDevice.Commands.CreateComputePipelines(fDevice.Handle,VK_NULL_HANDLE,1,@PipelineInfo,nil,@fComputePipeline)<>VK_SUCCESS then begin
  raise EpvVideoH264.Create('nv12rgb compute pipeline creation failed');
 end;

 // ---- per-pool descriptor sets (the same stage plane views as sources, that pool slot as the storage dst) ----
 FillChar(DescriptorSizes,SizeOf(DescriptorSizes),#0);
 DescriptorSizes[0].type_:=VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
 DescriptorSizes[0].descriptorCount:=TVkUInt32(fPoolCount)*2;
 DescriptorSizes[1].type_:=VK_DESCRIPTOR_TYPE_STORAGE_IMAGE;
 DescriptorSizes[1].descriptorCount:=TVkUInt32(fPoolCount);
 FillChar(DescriptorPoolInfo,SizeOf(DescriptorPoolInfo),#0);
 DescriptorPoolInfo.sType:=VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
 DescriptorPoolInfo.maxSets:=TVkUInt32(fPoolCount);
 DescriptorPoolInfo.poolSizeCount:=2;
 DescriptorPoolInfo.pPoolSizes:=@DescriptorSizes[0];
 if fDevice.Commands.CreateDescriptorPool(fDevice.Handle,@DescriptorPoolInfo,nil,@fDescriptorPool)<>VK_SUCCESS then begin
  raise EpvVideoH264.Create('nv12rgb descriptor pool creation failed');
 end;
 SetLength(fDescriptorSets,fPoolCount);
 for Index:=0 to fPoolCount-1 do begin
  FillChar(SetAllocateInfo,SizeOf(SetAllocateInfo),#0);
  SetAllocateInfo.sType:=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
  SetAllocateInfo.descriptorPool:=fDescriptorPool;
  SetAllocateInfo.descriptorSetCount:=1;
  SetAllocateInfo.pSetLayouts:=@fSetLayout;
  if fDevice.Commands.AllocateDescriptorSets(fDevice.Handle,@SetAllocateInfo,@fDescriptorSets[Index])<>VK_SUCCESS then begin
   raise EpvVideoH264.Create('nv12rgb descriptor set allocation failed');
  end;
  LumaInfo.sampler:=fSampler;
  LumaInfo.imageView:=fStageLumaView;
  LumaInfo.imageLayout:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
  ChromaInfo.sampler:=fSampler;
  ChromaInfo.imageView:=fStageChromaView;
  ChromaInfo.imageLayout:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
  DestinationInfo.sampler:=VK_NULL_HANDLE;
  DestinationInfo.imageView:=fPoolViews[Index].Handle;
  DestinationInfo.imageLayout:=VK_IMAGE_LAYOUT_GENERAL;
  FillChar(Writes,SizeOf(Writes),#0);
  Writes[0].sType:=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
  Writes[0].dstSet:=fDescriptorSets[Index];
  Writes[0].dstBinding:=0;
  Writes[0].descriptorCount:=1;
  Writes[0].descriptorType:=VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
  Writes[0].pImageInfo:=@LumaInfo;
  Writes[1].sType:=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
  Writes[1].dstSet:=fDescriptorSets[Index];
  Writes[1].dstBinding:=1;
  Writes[1].descriptorCount:=1;
  Writes[1].descriptorType:=VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
  Writes[1].pImageInfo:=@ChromaInfo;
  Writes[2].sType:=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
  Writes[2].dstSet:=fDescriptorSets[Index];
  Writes[2].dstBinding:=2;
  Writes[2].descriptorCount:=1;
  Writes[2].descriptorType:=VK_DESCRIPTOR_TYPE_STORAGE_IMAGE;
  Writes[2].pImageInfo:=@DestinationInfo;
  fDevice.Commands.UpdateDescriptorSets(fDevice.Handle,3,@Writes[0],0,nil);
 end;

 // ---- command pools + buffers (decode on the video family, compute on the universal family) + a fence ----
 FillChar(CommandPoolInfo,SizeOf(CommandPoolInfo),#0);
 CommandPoolInfo.sType:=VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
 CommandPoolInfo.flags:=TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT);
 CommandPoolInfo.queueFamilyIndex:=VideoFamily;
 if fDevice.Commands.CreateCommandPool(fDevice.Handle,@CommandPoolInfo,nil,@fVideoCommandPool)<>VK_SUCCESS then begin
  raise EpvVideoH264.Create('video command pool creation failed');
 end;
 FillChar(CommandBufferAllocateInfo,SizeOf(CommandBufferAllocateInfo),#0);
 CommandBufferAllocateInfo.sType:=VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
 CommandBufferAllocateInfo.commandPool:=fVideoCommandPool;
 CommandBufferAllocateInfo.level:=VK_COMMAND_BUFFER_LEVEL_PRIMARY;
 CommandBufferAllocateInfo.commandBufferCount:=1;
 if fDevice.Commands.AllocateCommandBuffers(fDevice.Handle,@CommandBufferAllocateInfo,@fDecodeCommand)<>VK_SUCCESS then begin
  raise EpvVideoH264.Create('decode command buffer allocation failed');
 end;
 FillChar(CommandPoolInfo,SizeOf(CommandPoolInfo),#0);
 CommandPoolInfo.sType:=VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
 CommandPoolInfo.flags:=TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT);
 CommandPoolInfo.queueFamilyIndex:=UniversalFamily;
 if fDevice.Commands.CreateCommandPool(fDevice.Handle,@CommandPoolInfo,nil,@fComputeCommandPool)<>VK_SUCCESS then begin
  raise EpvVideoH264.Create('compute command pool creation failed');
 end;
 FillChar(CommandBufferAllocateInfo,SizeOf(CommandBufferAllocateInfo),#0);
 CommandBufferAllocateInfo.sType:=VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
 CommandBufferAllocateInfo.commandPool:=fComputeCommandPool;
 CommandBufferAllocateInfo.level:=VK_COMMAND_BUFFER_LEVEL_PRIMARY;
 CommandBufferAllocateInfo.commandBufferCount:=1;
 if fDevice.Commands.AllocateCommandBuffers(fDevice.Handle,@CommandBufferAllocateInfo,@fComputeCommand)<>VK_SUCCESS then begin
  raise EpvVideoH264.Create('compute command buffer allocation failed');
 end;
 FillChar(FenceInfo,SizeOf(FenceInfo),#0);
 FenceInfo.sType:=VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
 if fDevice.Commands.CreateFence(fDevice.Handle,@FenceInfo,nil,@fComputeFence)<>VK_SUCCESS then begin
  raise EpvVideoH264.Create('compute fence creation failed');
 end;

 // ---- one-time: move every pool image into GENERAL (the compute shader stores into them) ----
 FillChar(BeginInfo,SizeOf(BeginInfo),#0);
 BeginInfo.sType:=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
 BeginInfo.flags:=TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT);
 fDevice.Commands.BeginCommandBuffer(fComputeCommand,@BeginInfo);
 for Index:=0 to fPoolCount-1 do begin
  RecordImageBarrier(fComputeCommand,fPoolImages[Index].Handle,VK_IMAGE_LAYOUT_UNDEFINED,VK_IMAGE_LAYOUT_GENERAL,
                     0,TVkAccessFlags2(VK_ACCESS_2_SHADER_WRITE_BIT),
                     TVkPipelineStageFlags2(VK_PIPELINE_STAGE_2_NONE),TVkPipelineStageFlags2(VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT));
 end;
 fDevice.Commands.EndCommandBuffer(fComputeCommand);
 FillChar(SubmitInfo,SizeOf(SubmitInfo),#0);
 SubmitInfo.sType:=VK_STRUCTURE_TYPE_SUBMIT_INFO;
 SubmitInfo.commandBufferCount:=1;
 SubmitInfo.pCommandBuffers:=@fComputeCommand;
 fDevice.UniversalQueue.Submit(1,@SubmitInfo,nil);
 fDevice.UniversalQueue.WaitIdle;

 // ---- online reorder buffer + decode cursor ----
 SetLength(fReorderKey,H264ReorderDepth+2);
 SetLength(fReorderPool,H264ReorderDepth+2);
 fReorderCount:=0;
 fNextCoded:=0;
 fResetDone:=0;
 fCurrentDisplayIndex:=-1;
 fCurrentPoolIndex:=-1;

end;

procedure TpvVideoH264Decoder.DestroyDecodeResources;
var Index:TpvInt32;
begin

 if fDevice.VideoDecodeQueue<>nil then begin
  fDevice.VideoDecodeQueue.WaitIdle;
 end;
 fDevice.UniversalQueue.WaitIdle;

 if fComputeFence<>VK_NULL_HANDLE then begin
  fDevice.Commands.DestroyFence(fDevice.Handle,fComputeFence,nil);
  fComputeFence:=VK_NULL_HANDLE;
 end;
 if fComputeCommandPool<>VK_NULL_HANDLE then begin
  fDevice.Commands.DestroyCommandPool(fDevice.Handle,fComputeCommandPool,nil);
  fComputeCommandPool:=VK_NULL_HANDLE;
 end;
 if fVideoCommandPool<>VK_NULL_HANDLE then begin
  fDevice.Commands.DestroyCommandPool(fDevice.Handle,fVideoCommandPool,nil);
  fVideoCommandPool:=VK_NULL_HANDLE;
 end;
 if fDescriptorPool<>VK_NULL_HANDLE then begin
  fDevice.Commands.DestroyDescriptorPool(fDevice.Handle,fDescriptorPool,nil);
  fDescriptorPool:=VK_NULL_HANDLE;
 end;
 fDescriptorSets:=nil;
 if fComputePipeline<>VK_NULL_HANDLE then begin
  fDevice.Commands.DestroyPipeline(fDevice.Handle,fComputePipeline,nil);
  fComputePipeline:=VK_NULL_HANDLE;
 end;
 if fComputeLayout<>VK_NULL_HANDLE then begin
  fDevice.Commands.DestroyPipelineLayout(fDevice.Handle,fComputeLayout,nil);
  fComputeLayout:=VK_NULL_HANDLE;
 end;
 if fSetLayout<>VK_NULL_HANDLE then begin
  fDevice.Commands.DestroyDescriptorSetLayout(fDevice.Handle,fSetLayout,nil);
  fSetLayout:=VK_NULL_HANDLE;
 end;
 if fComputeModule<>VK_NULL_HANDLE then begin
  fDevice.Commands.DestroyShaderModule(fDevice.Handle,fComputeModule,nil);
  fComputeModule:=VK_NULL_HANDLE;
 end;
 FreeAndNil(fDisplayImageView);
 FreeAndNil(fDisplayImage);
 FreeAndNil(fDisplayMemory);
 for Index:=0 to length(fPoolViews)-1 do begin
  FreeAndNil(fPoolViews[Index]);
 end;
 for Index:=0 to length(fPoolImages)-1 do begin
  FreeAndNil(fPoolImages[Index]);
 end;
 for Index:=0 to length(fPoolMemories)-1 do begin
  FreeAndNil(fPoolMemories[Index]);
 end;
 fPoolViews:=nil;
 fPoolImages:=nil;
 fPoolMemories:=nil;
 fPoolFree:=nil;
 if fSampler<>VK_NULL_HANDLE then begin
  fDevice.Commands.DestroySampler(fDevice.Handle,fSampler,nil);
  fSampler:=VK_NULL_HANDLE;
 end;
 if fStageChromaView<>VK_NULL_HANDLE then begin
  fDevice.Commands.DestroyImageView(fDevice.Handle,fStageChromaView,nil);
  fStageChromaView:=VK_NULL_HANDLE;
 end;
 if fStageLumaView<>VK_NULL_HANDLE then begin
  fDevice.Commands.DestroyImageView(fDevice.Handle,fStageLumaView,nil);
  fStageLumaView:=VK_NULL_HANDLE;
 end;
 if fStageImage<>VK_NULL_HANDLE then begin
  fDevice.Commands.DestroyImage(fDevice.Handle,fStageImage,nil);
  fStageImage:=VK_NULL_HANDLE;
 end;
 if fStageMemory<>VK_NULL_HANDLE then begin
  fDevice.Commands.FreeMemory(fDevice.Handle,fStageMemory,nil);
  fStageMemory:=VK_NULL_HANDLE;
 end;
 if fBitstreamMap<>nil then begin
  fDevice.Commands.UnmapMemory(fDevice.Handle,fBitstreamMemory);
  fBitstreamMap:=nil;
 end;
 if fBitstreamBuffer<>VK_NULL_HANDLE then begin
  fDevice.Commands.DestroyBuffer(fDevice.Handle,fBitstreamBuffer,nil);
  fBitstreamBuffer:=VK_NULL_HANDLE;
 end;
 if fBitstreamMemory<>VK_NULL_HANDLE then begin
  fDevice.Commands.FreeMemory(fDevice.Handle,fBitstreamMemory,nil);
  fBitstreamMemory:=VK_NULL_HANDLE;
 end;
 for Index:=0 to length(fDPBViews)-1 do begin
  if fDPBViews[Index]<>VK_NULL_HANDLE then begin
   fDevice.Commands.DestroyImageView(fDevice.Handle,fDPBViews[Index],nil);
  end;
 end;
 for Index:=0 to length(fDPBImages)-1 do begin
  if fDPBImages[Index]<>VK_NULL_HANDLE then begin
   fDevice.Commands.DestroyImage(fDevice.Handle,fDPBImages[Index],nil);
  end;
 end;
 for Index:=0 to length(fDPBMemories)-1 do begin
  if fDPBMemories[Index]<>VK_NULL_HANDLE then begin
   fDevice.Commands.FreeMemory(fDevice.Handle,fDPBMemories[Index],nil);
  end;
 end;
 fDPBViews:=nil;
 fDPBImages:=nil;
 fDPBMemories:=nil;
 fDPBLayouts:=nil;
 fPictureResources:=nil;
 fSlotUsed:=nil;
 fSlotPOC:=nil;
 fSlotFrameNum:=nil;
 fSlotLongTerm:=nil;
 fSlotLongTermFrameIdx:=nil;
 fReorderKey:=nil;
 fReorderPool:=nil;

end;

procedure TpvVideoH264Decoder.ResetDecodeState(const aFromCodedIndex:TpvInt32);
var Index:TpvInt32;
begin

 // Flush DPB + reorder buffer + free-list so a backward seek can re-decode from a clean IDR at aFromCodedIndex.
 for Index:=0 to fSlotCount-1 do begin
  fSlotUsed[Index]:=0;
  fSlotLongTerm[Index]:=0;
  fDPBLayouts[Index]:=VK_IMAGE_LAYOUT_UNDEFINED;
 end;
 for Index:=0 to fPoolCount-1 do begin
  fPoolFree[Index]:=Index;
 end;
 fFreeCount:=fPoolCount;
 fMaxLongTermFrameIdx:=-1;
 fReorderCount:=0;
 fNextCoded:=aFromCodedIndex;
 fResetDone:=0;
 fCurrentDisplayIndex:=-1;
 fCurrentPoolIndex:=-1;

end;

function TpvVideoH264Decoder.DecodeCodedFrame(const aCodedIndex:TpvInt32):TpvInt32;
var Slot,Current,ReferenceCount,Oldest,OldestWrap,Wrap,UsedCount,PoolIndex,Push0,Push1:TpvInt32;
    CurrPicNum,MmcoIndex,Operation,PicNumX,PicNum,LongTermIdx:TpvInt32;
    Range:TVkDeviceSize;
    References:array[0..7] of TVkVideoReferenceSlotInfoKHR;
    BeginSlots:array[0..8] of TVkVideoReferenceSlotInfoKHR;
    ReferenceInfo:array[0..7] of TStdVideoDecodeH264ReferenceInfo;
    ReferenceDpb:array[0..7] of TVkVideoDecodeH264DpbSlotInfoKHR;
    SetupInfo:TStdVideoDecodeH264ReferenceInfo;
    SetupDpb:TVkVideoDecodeH264DpbSlotInfoKHR;
    SetupSlot:TVkVideoReferenceSlotInfoKHR;
    StdPicture:TStdVideoDecodeH264PictureInfo;
    H264Picture:TVkVideoDecodeH264PictureInfoKHR;
    DecodeInfo:TVkVideoDecodeInfoKHR;
    BeginCoding:TVkVideoBeginCodingInfoKHR;
    Control:TVkVideoCodingControlInfoKHR;
    EndCoding:TVkVideoEndCodingInfoKHR;
    SliceOffset:TVkUInt32;
    PlaneCopies:array[0..1] of TVkImageCopy;
    BeginInfo:TVkCommandBufferBeginInfo;
    SubmitInfo:TVkSubmitInfo;
    Push:array[0..1] of TVkInt32;
begin

 // IDR flushes the whole DPB (no surviving references across an IDR).
 if fFrames[aCodedIndex].IDR<>0 then begin
  for Slot:=0 to fSlotCount-1 do begin
   fSlotUsed[Slot]:=0;
   fSlotLongTerm[Slot]:=0;
  end;
  fMaxLongTermFrameIdx:=-1;
 end;

 // Pick a free DPB slot; if none, evict the lowest-frame_num (wrapped) reference.
 Current:=-1;
 for Slot:=0 to fSlotCount-1 do begin
  if fSlotUsed[Slot]=0 then begin
   Current:=Slot;
   break;
  end;
 end;
 if Current<0 then begin
  Oldest:=-1;
  OldestWrap:=0;
  for Slot:=0 to fSlotCount-1 do begin
   if fSlotUsed[Slot]<>0 then begin
    if fSlotFrameNum[Slot]>fFrames[aCodedIndex].FrameNum then begin
     Wrap:=fSlotFrameNum[Slot]-fMaxFrameNum;
    end else begin
     Wrap:=fSlotFrameNum[Slot];
    end;
    if (Oldest<0) or (Wrap<OldestWrap) then begin
     Oldest:=Slot;
     OldestWrap:=Wrap;
    end;
   end;
  end;
  fSlotUsed[Oldest]:=0;
  Current:=Oldest;
 end;

 // Build the active reference-slot list from the currently-used DPB slots.
 ReferenceCount:=0;
 for Slot:=0 to fSlotCount-1 do begin
  if fSlotUsed[Slot]<>0 then begin
   FillChar(ReferenceInfo[ReferenceCount],SizeOf(TStdVideoDecodeH264ReferenceInfo),#0);
   if fSlotLongTerm[Slot]<>0 then begin
    // for a long-term reference the std header carries LongTermFrameIdx in FrameNum + the flag
    ReferenceInfo[ReferenceCount].flags.used_for_long_term_reference:=1;
    ReferenceInfo[ReferenceCount].FrameNum:=fSlotLongTermFrameIdx[Slot];
   end else begin
    ReferenceInfo[ReferenceCount].FrameNum:=fSlotFrameNum[Slot];
   end;
   ReferenceInfo[ReferenceCount].PicOrderCnt[0]:=fSlotPOC[Slot];
   ReferenceInfo[ReferenceCount].PicOrderCnt[1]:=fSlotPOC[Slot];
   FillChar(ReferenceDpb[ReferenceCount],SizeOf(TVkVideoDecodeH264DpbSlotInfoKHR),#0);
   ReferenceDpb[ReferenceCount].sType:=VK_STRUCTURE_TYPE_VIDEO_DECODE_H264_DPB_SLOT_INFO_KHR;
   ReferenceDpb[ReferenceCount].pStdReferenceInfo:=@ReferenceInfo[ReferenceCount];
   FillChar(References[ReferenceCount],SizeOf(TVkVideoReferenceSlotInfoKHR),#0);
   References[ReferenceCount].sType:=VK_STRUCTURE_TYPE_VIDEO_REFERENCE_SLOT_INFO_KHR;
   References[ReferenceCount].pNext:=@ReferenceDpb[ReferenceCount];
   References[ReferenceCount].slotIndex:=Slot;
   References[ReferenceCount].pPictureResource:=@fPictureResources[Slot];
   inc(ReferenceCount);
  end;
 end;

 // The setup (reconstructed-picture) slot for the frame being decoded.
 FillChar(SetupInfo,SizeOf(SetupInfo),#0);
 SetupInfo.FrameNum:=fFrames[aCodedIndex].FrameNum;
 SetupInfo.PicOrderCnt[0]:=fFrames[aCodedIndex].POC;
 SetupInfo.PicOrderCnt[1]:=fFrames[aCodedIndex].POC;
 FillChar(SetupDpb,SizeOf(SetupDpb),#0);
 SetupDpb.sType:=VK_STRUCTURE_TYPE_VIDEO_DECODE_H264_DPB_SLOT_INFO_KHR;
 SetupDpb.pStdReferenceInfo:=@SetupInfo;
 FillChar(SetupSlot,SizeOf(SetupSlot),#0);
 SetupSlot.sType:=VK_STRUCTURE_TYPE_VIDEO_REFERENCE_SLOT_INFO_KHR;
 SetupSlot.pNext:=@SetupDpb;
 SetupSlot.slotIndex:=Current;
 SetupSlot.pPictureResource:=@fPictureResources[Current];
 for Slot:=0 to ReferenceCount-1 do begin
  BeginSlots[Slot]:=References[Slot];
 end;
 BeginSlots[ReferenceCount]:=SetupSlot;
 BeginSlots[ReferenceCount].slotIndex:=-1;

 // Upload the NAL: a leading zero byte then the Annex-B NAL (the C does the same -> 00 00 00 01 ...).
 FillChar(PpvUInt8Array(fBitstreamMap)^[0],fBitstreamLength,0);
 Move(fBlob[fFrames[aCodedIndex].NALOffset],PpvUInt8Array(fBitstreamMap)^[1],fFrames[aCodedIndex].NALLength);
 Range:=((TVkDeviceSize(fFrames[aCodedIndex].NALLength+1)+fBitstreamAlignment-1) div fBitstreamAlignment)*fBitstreamAlignment;

 // ---- record + submit the decode on the video-decode queue ----
 fDevice.Commands.ResetCommandBuffer(fDecodeCommand,0);
 FillChar(BeginInfo,SizeOf(BeginInfo),#0);
 BeginInfo.sType:=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
 BeginInfo.flags:=TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT);
 fDevice.Commands.BeginCommandBuffer(fDecodeCommand,@BeginInfo);
 RecordImageBarrier(fDecodeCommand,fDPBImages[Current],fDPBLayouts[Current],VK_IMAGE_LAYOUT_VIDEO_DECODE_DPB_KHR,
                    0,TVkAccessFlags2(VK_ACCESS_2_VIDEO_DECODE_WRITE_BIT_KHR) or TVkAccessFlags2(VK_ACCESS_2_VIDEO_DECODE_READ_BIT_KHR),
                    TVkPipelineStageFlags2(VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT),TVkPipelineStageFlags2(VK_PIPELINE_STAGE_2_VIDEO_DECODE_BIT_KHR));
 fDPBLayouts[Current]:=VK_IMAGE_LAYOUT_VIDEO_DECODE_DPB_KHR;
 FillChar(BeginCoding,SizeOf(BeginCoding),#0);
 BeginCoding.sType:=VK_STRUCTURE_TYPE_VIDEO_BEGIN_CODING_INFO_KHR;
 BeginCoding.videoSession:=fSession;
 BeginCoding.videoSessionParameters:=fSessionParameters;
 BeginCoding.referenceSlotCount:=ReferenceCount+1;
 BeginCoding.pReferenceSlots:=@BeginSlots[0];
 fDevice.Commands.CmdBeginVideoCodingKHR(fDecodeCommand,@BeginCoding);
 if fResetDone=0 then begin
  FillChar(Control,SizeOf(Control),#0);
  Control.sType:=VK_STRUCTURE_TYPE_VIDEO_CODING_CONTROL_INFO_KHR;
  Control.flags:=TVkVideoCodingControlFlagsKHR(VK_VIDEO_CODING_CONTROL_RESET_BIT_KHR);
  fDevice.Commands.CmdControlVideoCodingKHR(fDecodeCommand,@Control);
  fResetDone:=1;
 end;
 FillChar(StdPicture,SizeOf(StdPicture),#0);
 StdPicture.flags.IdrPicFlag:=fFrames[aCodedIndex].IDR;
 StdPicture.flags.is_reference:=ord(fFrames[aCodedIndex].RefIdc<>0);
 StdPicture.flags.is_intra:=ord((fFrames[aCodedIndex].SliceType=2) or (fFrames[aCodedIndex].IDR<>0));
 StdPicture.seq_parameter_set_id:=fSPS.SPSId;
 StdPicture.pic_parameter_set_id:=fPPS.PPSId;
 StdPicture.frame_num:=fFrames[aCodedIndex].FrameNum;
 StdPicture.PicOrderCnt[0]:=fFrames[aCodedIndex].POC;
 StdPicture.PicOrderCnt[1]:=fFrames[aCodedIndex].POC;
 SliceOffset:=0;
 FillChar(H264Picture,SizeOf(H264Picture),#0);
 H264Picture.sType:=VK_STRUCTURE_TYPE_VIDEO_DECODE_H264_PICTURE_INFO_KHR;
 H264Picture.pStdPictureInfo:=@StdPicture;
 H264Picture.sliceCount:=1;
 H264Picture.pSliceOffsets:=@SliceOffset;
 FillChar(DecodeInfo,SizeOf(DecodeInfo),#0);
 DecodeInfo.sType:=VK_STRUCTURE_TYPE_VIDEO_DECODE_INFO_KHR;
 DecodeInfo.pNext:=@H264Picture;
 DecodeInfo.srcBuffer:=fBitstreamBuffer;
 DecodeInfo.srcBufferOffset:=0;
 DecodeInfo.srcBufferRange:=Range;
 DecodeInfo.dstPictureResource:=fPictureResources[Current];
 DecodeInfo.pSetupReferenceSlot:=@SetupSlot;
 DecodeInfo.referenceSlotCount:=ReferenceCount;
 if ReferenceCount>0 then begin
  DecodeInfo.pReferenceSlots:=@References[0];
 end else begin
  DecodeInfo.pReferenceSlots:=nil;
 end;
 fDevice.Commands.CmdDecodeVideoKHR(fDecodeCommand,@DecodeInfo);
 FillChar(EndCoding,SizeOf(EndCoding),#0);
 EndCoding.sType:=VK_STRUCTURE_TYPE_VIDEO_END_CODING_INFO_KHR;
 fDevice.Commands.CmdEndVideoCodingKHR(fDecodeCommand,@EndCoding);

 // Copy the decoded NV12 (both planes) into the sampled stage image.
 RecordImageBarrier(fDecodeCommand,fDPBImages[Current],VK_IMAGE_LAYOUT_VIDEO_DECODE_DPB_KHR,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                    TVkAccessFlags2(VK_ACCESS_2_VIDEO_DECODE_WRITE_BIT_KHR),TVkAccessFlags2(VK_ACCESS_2_TRANSFER_READ_BIT),
                    TVkPipelineStageFlags2(VK_PIPELINE_STAGE_2_VIDEO_DECODE_BIT_KHR),TVkPipelineStageFlags2(VK_PIPELINE_STAGE_2_COPY_BIT));
 RecordImageBarrier(fDecodeCommand,fStageImage,fStageLayout,VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                    0,TVkAccessFlags2(VK_ACCESS_2_TRANSFER_WRITE_BIT),
                    TVkPipelineStageFlags2(VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT),TVkPipelineStageFlags2(VK_PIPELINE_STAGE_2_COPY_BIT));
 fStageLayout:=VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
 FillChar(PlaneCopies,SizeOf(PlaneCopies),#0);
 PlaneCopies[0].srcSubresource.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_PLANE_0_BIT);
 PlaneCopies[0].srcSubresource.layerCount:=1;
 PlaneCopies[0].dstSubresource.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_PLANE_0_BIT);
 PlaneCopies[0].dstSubresource.layerCount:=1;
 PlaneCopies[0].extent.width:=fCodedWidth;
 PlaneCopies[0].extent.height:=fCodedHeight;
 PlaneCopies[0].extent.depth:=1;
 PlaneCopies[1].srcSubresource.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_PLANE_1_BIT);
 PlaneCopies[1].srcSubresource.layerCount:=1;
 PlaneCopies[1].dstSubresource.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_PLANE_1_BIT);
 PlaneCopies[1].dstSubresource.layerCount:=1;
 PlaneCopies[1].extent.width:=fCodedWidth div 2;
 PlaneCopies[1].extent.height:=fCodedHeight div 2;
 PlaneCopies[1].extent.depth:=1;
 fDevice.Commands.CmdCopyImage(fDecodeCommand,fDPBImages[Current],VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                               fStageImage,VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,2,@PlaneCopies[0]);
 RecordImageBarrier(fDecodeCommand,fDPBImages[Current],VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,VK_IMAGE_LAYOUT_VIDEO_DECODE_DPB_KHR,
                    TVkAccessFlags2(VK_ACCESS_2_TRANSFER_READ_BIT),TVkAccessFlags2(VK_ACCESS_2_VIDEO_DECODE_READ_BIT_KHR),
                    TVkPipelineStageFlags2(VK_PIPELINE_STAGE_2_COPY_BIT),TVkPipelineStageFlags2(VK_PIPELINE_STAGE_2_VIDEO_DECODE_BIT_KHR));
 fDPBLayouts[Current]:=VK_IMAGE_LAYOUT_VIDEO_DECODE_DPB_KHR;
 fDevice.Commands.EndCommandBuffer(fDecodeCommand);
 FillChar(SubmitInfo,SizeOf(SubmitInfo),#0);
 SubmitInfo.sType:=VK_STRUCTURE_TYPE_SUBMIT_INFO;
 SubmitInfo.commandBufferCount:=1;
 SubmitInfo.pCommandBuffers:=@fDecodeCommand;
 fDevice.VideoDecodeQueue.Submit(1,@SubmitInfo,nil);
 fDevice.VideoDecodeQueue.WaitIdle;

 // ---- nv12rgb compute into a free pool slot (universal queue, fenced) ----
 if fFreeCount<=0 then begin
  raise EpvVideoH264.Create('RGBA pool exhausted at coded frame '+IntToStr(aCodedIndex)+' (nextCoded='+IntToStr(fNextCoded)+')');
 end;
 dec(fFreeCount);
 PoolIndex:=fPoolFree[fFreeCount];
 fDevice.Commands.ResetCommandBuffer(fComputeCommand,0);
 FillChar(BeginInfo,SizeOf(BeginInfo),#0);
 BeginInfo.sType:=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
 BeginInfo.flags:=TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT);
 fDevice.Commands.BeginCommandBuffer(fComputeCommand,@BeginInfo);
 RecordImageBarrier(fComputeCommand,fStageImage,VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                    TVkAccessFlags2(VK_ACCESS_2_TRANSFER_WRITE_BIT),TVkAccessFlags2(VK_ACCESS_2_SHADER_READ_BIT),
                    TVkPipelineStageFlags2(VK_PIPELINE_STAGE_2_COPY_BIT),TVkPipelineStageFlags2(VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT));
 fStageLayout:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
 fDevice.Commands.CmdBindPipeline(fComputeCommand,VK_PIPELINE_BIND_POINT_COMPUTE,fComputePipeline);
 fDevice.Commands.CmdBindDescriptorSets(fComputeCommand,VK_PIPELINE_BIND_POINT_COMPUTE,fComputeLayout,0,1,@fDescriptorSets[PoolIndex],0,nil);
 Push[0]:=fWidth;
 Push[1]:=fHeight;
 fDevice.Commands.CmdPushConstants(fComputeCommand,fComputeLayout,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,8,@Push[0]);
 Push0:=(fWidth+7) div 8;
 Push1:=(fHeight+7) div 8;
 fDevice.Commands.CmdDispatch(fComputeCommand,Push0,Push1,1);
 // make the compute store available to the later present blit (keep the pool image in GENERAL)
 RecordImageBarrier(fComputeCommand,fPoolImages[PoolIndex].Handle,VK_IMAGE_LAYOUT_GENERAL,VK_IMAGE_LAYOUT_GENERAL,
                    TVkAccessFlags2(VK_ACCESS_2_SHADER_WRITE_BIT),TVkAccessFlags2(VK_ACCESS_2_TRANSFER_READ_BIT),
                    TVkPipelineStageFlags2(VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT),TVkPipelineStageFlags2(VK_PIPELINE_STAGE_2_BLIT_BIT));
 fDevice.Commands.EndCommandBuffer(fComputeCommand);
 fDevice.Commands.ResetFences(fDevice.Handle,1,@fComputeFence);
 FillChar(SubmitInfo,SizeOf(SubmitInfo),#0);
 SubmitInfo.sType:=VK_STRUCTURE_TYPE_SUBMIT_INFO;
 SubmitInfo.commandBufferCount:=1;
 SubmitInfo.pCommandBuffers:=@fComputeCommand;
 fDevice.UniversalQueue.Lock.Acquire;
 try
  fDevice.Commands.QueueSubmit(fDevice.UniversalQueue.Handle,1,@SubmitInfo,fComputeFence);
 finally
  fDevice.UniversalQueue.Lock.Release;
 end;
 fDevice.Commands.WaitForFences(fDevice.Handle,1,@fComputeFence,VK_TRUE,TpvUInt64(high(TpvUInt64)));

 // H.264 reference picture marking (spec 8.2.5): keep this picture's slot if it is a reference, then update the
 // short-term / long-term reference set per IDR / MMCO / sliding-window so the next pictures get exactly the right
 // DPB. (Without applying MMCO, x264 b-pyramid streams hand the GPU a slightly wrong reference set and the
 // motion-compensated prediction drifts over the GOP.)
 if fFrames[aCodedIndex].RefIdc<>0 then begin
  CurrPicNum:=fFrames[aCodedIndex].FrameNum;
  if fFrames[aCodedIndex].IDR<>0 then begin
   // the DPB was already cleared above; mark the IDR picture itself (short- or long-term)
   fSlotUsed[Current]:=1;
   fSlotPOC[Current]:=fFrames[aCodedIndex].POC;
   fSlotFrameNum[Current]:=fFrames[aCodedIndex].FrameNum;
   fSlotLongTerm[Current]:=fFrames[aCodedIndex].LongTermReferenceFlag;
   fSlotLongTermFrameIdx[Current]:=0;
   if fFrames[aCodedIndex].LongTermReferenceFlag<>0 then begin
    fMaxLongTermFrameIdx:=0;
   end else begin
    fMaxLongTermFrameIdx:=-1;
   end;
  end else begin
   // a non-IDR reference picture is first added to the DPB as a short-term reference
   fSlotUsed[Current]:=1;
   fSlotPOC[Current]:=fFrames[aCodedIndex].POC;
   fSlotFrameNum[Current]:=fFrames[aCodedIndex].FrameNum;
   fSlotLongTerm[Current]:=0;
   fSlotLongTermFrameIdx[Current]:=0;
   if fFrames[aCodedIndex].AdaptiveMarking<>0 then begin
    // adaptive memory control (8.2.5.4): apply the MMCO command list
    for MmcoIndex:=0 to fFrames[aCodedIndex].MmcoCount-1 do begin
     Operation:=fFrames[aCodedIndex].Mmco[MmcoIndex].Operation;
     if Operation=1 then begin
      // mark a short-term picture as unused for reference
      PicNumX:=CurrPicNum-(fFrames[aCodedIndex].Mmco[MmcoIndex].Argument1+1);
      for Slot:=0 to fSlotCount-1 do begin
       if (fSlotUsed[Slot]<>0) and (fSlotLongTerm[Slot]=0) then begin
        if fSlotFrameNum[Slot]>CurrPicNum then begin
         PicNum:=fSlotFrameNum[Slot]-fMaxFrameNum;
        end else begin
         PicNum:=fSlotFrameNum[Slot];
        end;
        if PicNum=PicNumX then begin
         fSlotUsed[Slot]:=0;
        end;
       end;
      end;
     end;
     if Operation=2 then begin
      // mark a long-term picture as unused for reference
      for Slot:=0 to fSlotCount-1 do begin
       if (fSlotUsed[Slot]<>0) and (fSlotLongTerm[Slot]<>0) and (fSlotLongTermFrameIdx[Slot]=fFrames[aCodedIndex].Mmco[MmcoIndex].Argument1) then begin
        fSlotUsed[Slot]:=0;
       end;
      end;
     end;
     if Operation=3 then begin
      // turn a short-term picture into a long-term picture with the given index
      PicNumX:=CurrPicNum-(fFrames[aCodedIndex].Mmco[MmcoIndex].Argument1+1);
      LongTermIdx:=fFrames[aCodedIndex].Mmco[MmcoIndex].Argument2;
      for Slot:=0 to fSlotCount-1 do begin
       if (fSlotUsed[Slot]<>0) and (fSlotLongTerm[Slot]<>0) and (fSlotLongTermFrameIdx[Slot]=LongTermIdx) then begin
        fSlotUsed[Slot]:=0;
       end;
      end;
      for Slot:=0 to fSlotCount-1 do begin
       if (fSlotUsed[Slot]<>0) and (fSlotLongTerm[Slot]=0) then begin
        if fSlotFrameNum[Slot]>CurrPicNum then begin
         PicNum:=fSlotFrameNum[Slot]-fMaxFrameNum;
        end else begin
         PicNum:=fSlotFrameNum[Slot];
        end;
        if PicNum=PicNumX then begin
         fSlotLongTerm[Slot]:=1;
         fSlotLongTermFrameIdx[Slot]:=LongTermIdx;
        end;
       end;
      end;
     end;
     if Operation=4 then begin
      // set MaxLongTermFrameIdx and drop any long-term picture above it
      fMaxLongTermFrameIdx:=fFrames[aCodedIndex].Mmco[MmcoIndex].Argument1-1;
      for Slot:=0 to fSlotCount-1 do begin
       if (fSlotUsed[Slot]<>0) and (fSlotLongTerm[Slot]<>0) and (fSlotLongTermFrameIdx[Slot]>fMaxLongTermFrameIdx) then begin
        fSlotUsed[Slot]:=0;
       end;
      end;
     end;
     if Operation=5 then begin
      // mark all reference pictures unused and reset; the current picture keeps frame_num 0
      for Slot:=0 to fSlotCount-1 do begin
       fSlotUsed[Slot]:=0;
      end;
      fMaxLongTermFrameIdx:=-1;
      CurrPicNum:=0;
      fSlotUsed[Current]:=1;
      fSlotPOC[Current]:=fFrames[aCodedIndex].POC;
      fSlotFrameNum[Current]:=0;
      fSlotLongTerm[Current]:=0;
      fSlotLongTermFrameIdx[Current]:=0;
     end;
     if Operation=6 then begin
      // assign a long-term frame index to the current picture
      LongTermIdx:=fFrames[aCodedIndex].Mmco[MmcoIndex].Argument2;
      for Slot:=0 to fSlotCount-1 do begin
       if (fSlotUsed[Slot]<>0) and (fSlotLongTerm[Slot]<>0) and (fSlotLongTermFrameIdx[Slot]=LongTermIdx) and (Slot<>Current) then begin
        fSlotUsed[Slot]:=0;
       end;
      end;
      fSlotLongTerm[Current]:=1;
      fSlotLongTermFrameIdx[Current]:=LongTermIdx;
     end;
    end;
   end else begin
    // sliding window (8.2.5.3): remove the short-term reference with the smallest FrameNumWrap once the total
    // reference count exceeds the limit
    UsedCount:=0;
    for Slot:=0 to fSlotCount-1 do begin
     inc(UsedCount,fSlotUsed[Slot]);
    end;
    if UsedCount>fMaxRef then begin
     Oldest:=-1;
     OldestWrap:=0;
     for Slot:=0 to fSlotCount-1 do begin
      if (fSlotUsed[Slot]<>0) and (fSlotLongTerm[Slot]=0) and (Slot<>Current) then begin
       if fSlotFrameNum[Slot]>CurrPicNum then begin
        Wrap:=fSlotFrameNum[Slot]-fMaxFrameNum;
       end else begin
        Wrap:=fSlotFrameNum[Slot];
       end;
       if (Oldest<0) or (Wrap<OldestWrap) then begin
        Oldest:=Slot;
        OldestWrap:=Wrap;
       end;
      end;
     end;
     if Oldest>=0 then begin
      fSlotUsed[Oldest]:=0;
     end;
    end;
   end;
  end;
 end;

 result:=PoolIndex;
end;

procedure TpvVideoH264Decoder.CopyPoolToDisplay(const aPoolIndex:TpvInt32);
var BeginInfo:TVkCommandBufferBeginInfo;
    SubmitInfo:TVkSubmitInfo;
    Copy:TVkImageCopy;
begin

 fDevice.Commands.ResetCommandBuffer(fComputeCommand,0);
 FillChar(BeginInfo,SizeOf(BeginInfo),#0);
 BeginInfo.sType:=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
 BeginInfo.flags:=TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT);
 fDevice.Commands.BeginCommandBuffer(fComputeCommand,@BeginInfo);

 // the source pool slot stays in GENERAL (just make its prior compute store visible to the copy read)
 RecordImageBarrier(fComputeCommand,fPoolImages[aPoolIndex].Handle,VK_IMAGE_LAYOUT_GENERAL,VK_IMAGE_LAYOUT_GENERAL,
                    TVkAccessFlags2(VK_ACCESS_2_SHADER_WRITE_BIT),TVkAccessFlags2(VK_ACCESS_2_TRANSFER_READ_BIT),
                    TVkPipelineStageFlags2(VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT),TVkPipelineStageFlags2(VK_PIPELINE_STAGE_2_COPY_BIT));
 RecordImageBarrier(fComputeCommand,fDisplayImage.Handle,fDisplayLayout,VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                    0,TVkAccessFlags2(VK_ACCESS_2_TRANSFER_WRITE_BIT),
                    TVkPipelineStageFlags2(VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT),TVkPipelineStageFlags2(VK_PIPELINE_STAGE_2_COPY_BIT));

 FillChar(Copy,SizeOf(Copy),#0);
 Copy.srcSubresource.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 Copy.srcSubresource.layerCount:=1;
 Copy.dstSubresource.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 Copy.dstSubresource.layerCount:=1;
 Copy.extent.width:=fWidth;
 Copy.extent.height:=fHeight;
 Copy.extent.depth:=1;
 fDevice.Commands.CmdCopyImage(fComputeCommand,fPoolImages[aPoolIndex].Handle,VK_IMAGE_LAYOUT_GENERAL,
                               fDisplayImage.Handle,VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,1,@Copy);

 // leave the display image in TRANSFER_SRC_OPTIMAL (the wavelet OutputImage contract the facade/present path expects)
 RecordImageBarrier(fComputeCommand,fDisplayImage.Handle,VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                    TVkAccessFlags2(VK_ACCESS_2_TRANSFER_WRITE_BIT),TVkAccessFlags2(VK_ACCESS_2_TRANSFER_READ_BIT),
                    TVkPipelineStageFlags2(VK_PIPELINE_STAGE_2_COPY_BIT),TVkPipelineStageFlags2(VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT));
 fDisplayLayout:=VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;

 fDevice.Commands.EndCommandBuffer(fComputeCommand);
 fDevice.Commands.ResetFences(fDevice.Handle,1,@fComputeFence);
 FillChar(SubmitInfo,SizeOf(SubmitInfo),#0);
 SubmitInfo.sType:=VK_STRUCTURE_TYPE_SUBMIT_INFO;
 SubmitInfo.commandBufferCount:=1;
 SubmitInfo.pCommandBuffers:=@fComputeCommand;
 fDevice.UniversalQueue.Lock.Acquire;
 try
  fDevice.Commands.QueueSubmit(fDevice.UniversalQueue.Handle,1,@SubmitInfo,fComputeFence);
 finally
  fDevice.UniversalQueue.Lock.Release;
 end;
 fDevice.Commands.WaitForFences(fDevice.Handle,1,@fComputeFence,VK_TRUE,TpvUInt64(high(TpvUInt64)));

end;

function TpvVideoH264Decoder.EnsureDisplayFrame(const aDisplayIndex:TpvInt32):boolean;
var Target,Lowest,Index,EmittedPool:TpvInt32;
begin

 result:=false;
 if fFrameCount<=0 then begin
  exit;
 end;

 Target:=aDisplayIndex;
 if Target<0 then begin
  Target:=0;
 end;
 if Target>(fFrameCount-1) then begin
  Target:=fFrameCount-1;
 end;

 // Already the current output frame -> idempotent (repeated DecodeTime within one tick, or a held frame).
 if (Target=fCurrentDisplayIndex) and (fCurrentPoolIndex>=0) then begin
  result:=true;
  exit;
 end;

 // Backward seek: replay from the very first IDR (cutscene seeking is rare; correctness first).
 if Target<fCurrentDisplayIndex then begin
  ResetDecodeState(0);
 end;

 // Emit frames in key (display) order until the current output is the wanted display position. This is the C
 // reference's reorder buffer pulled on demand: decode in coded order, bump the lowest-key frame once the
 // buffer holds more than H264ReorderDepth (so that lowest key is final) or the stream has ended.
 while fCurrentDisplayIndex<Target do begin

  while (fReorderCount<=H264ReorderDepth) and (fNextCoded<fFrameCount) do begin
   fReorderPool[fReorderCount]:=DecodeCodedFrame(fNextCoded);
   fReorderKey[fReorderCount]:=fFrameKey[fNextCoded];
   inc(fReorderCount);
   inc(fNextCoded);
  end;

  if fReorderCount=0 then begin
   break; // stream exhausted, nothing more to emit
  end;

  Lowest:=0;
  for Index:=1 to fReorderCount-1 do begin
   if fReorderKey[Index]<fReorderKey[Lowest] then begin
    Lowest:=Index;
   end;
  end;
  EmittedPool:=fReorderPool[Lowest];

  // The previously-emitted output frame has been consumed by now -> recycle its pool slot.
  if fCurrentPoolIndex>=0 then begin
   fPoolFree[fFreeCount]:=fCurrentPoolIndex;
   inc(fFreeCount);
  end;
  fCurrentPoolIndex:=EmittedPool;
  inc(fCurrentDisplayIndex);

  for Index:=Lowest to fReorderCount-2 do begin
   fReorderKey[Index]:=fReorderKey[Index+1];
   fReorderPool[Index]:=fReorderPool[Index+1];
  end;
  dec(fReorderCount);

 end;

 // publish the new current frame into the single stable display image (-> TRANSFER_SRC_OPTIMAL)
 if fCurrentPoolIndex>=0 then begin
  CopyPoolToDisplay(fCurrentPoolIndex);
 end;

 result:=fCurrentPoolIndex>=0;
end;

function TpvVideoH264Decoder.OutputImage:TpvVulkanImage;
begin
 // the single stable display image (always valid; undefined content until the first EnsureDisplayFrame)
 result:=fDisplayImage;
end;

function TpvVideoH264Decoder.OutputImageView:TpvVulkanImageView;
begin
 result:=fDisplayImageView;
end;

{$endif}

end.
