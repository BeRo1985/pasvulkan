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
unit PasVulkan.Video.FlexibleWavelet;
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

// Shared CPU reference core of the Flexible Wavelet Video (FWV) codec, the engine-side sister of the C
// "fwv" tools / the "FWVC" container. This base unit holds only the parts the decoder and the encoder have
// in common: the on-disk container records (the byte-exact packed ContainerHeader / FrameEntry layout), the
// format consts/enums, the MSB-first CPU bit reader, the LZSS frame-payload (de)compression, and the
// per-frame payload framing. PasVulkan.Video.FlexibleWavelet.Decoder and .Encoder build the Vulkan-compute
// codec classes on top of it; the motion-vector entropy coders are added here as the inter-frame stages
// need them.
//
// The arithmetic is kept bit-for-bit identical to the C reference. The .fwv frame payloads use one of three
// methods: 0 = raw, 1 = LZSS (here), 2 = LZBRRC (reused from PasVulkan.Compression.LZBRRC).

interface

uses SysUtils,
     Classes,
     Math,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Math.Utils,
     PasVulkan.Compression.LZBRRC;

type EpvFlexibleWaveletVideo=class(Exception);

     { TpvFlexibleWaveletVideo }
     TpvFlexibleWaveletVideo=class // plain TObject base, resource-system-independent (NOT TpvResource)
      public
       const Magic:array[0..3] of AnsiChar=('F','W','V','C'); // the .fwv container magic
             FormatVersion=1;
             MotionRoot=32; // motion quadtree root block (px)
             MotionLeaf=8; // motion quadtree finest leaf (px) = the fine MV-field cell
             MaxMotionBlock=32;
             MaxBlockSize=128; // coding/bitplane block size cap (32/64/128 are valid)
             MVRCContextCap=20; // motion-vector range coder: magnitude-class truncated-unary cap
             MVRCBuckets=6; // motion-vector range coder: neighbour-context bucket count
             LZMaxOffset=65535;
             LZMinMatch=4;
             LZMaxMatch=259;
             FrameMethodRaw=0; // per-frame payload method byte: uncompressed
             FrameMethodLZSS=1; // LZSS-compressed
             FrameMethodLZBRRC=2; // LZBRRC-compressed
             AQTile=64; // AQ (per-tile QP): tile edge in luma pixels
             AQWeightMin=0.5; // AQ: finest step scale (most bits) -> code 0
             AQWeightMax=2.0; // AQ: coarsest step scale (least bits) -> code 255
       type TPredictionMethod=(CoefDiff=0,ColorDiff=1,OpenLoop3DDWT=2,MCTF3DDWT=3);
            TChromaFormat=(YCbCr444=0,YCbCr422=1,YCbCr420=2);
            TMVCodec=(ExpGolomb=0,Range=1);
            TFrameType=(Intra=0,Predicted=1,Bidirectional=2);
            PHeader=^THeader;
            { THeader }
            THeader=packed record // 142 bytes, byte-for-byte the packed C ContainerHeader (ReadBuffer-compatible, little-endian)
             Magic:array[0..3] of AnsiChar;
             Version:TpvUInt16;
             HeaderSize:TpvUInt16;
             Width:TpvUInt32;
             Height:TpvUInt32;
             FpsNum:TpvUInt32;
             FpsDen:TpvUInt32;
             Levels:TpvUInt32;
             Quality:TpvUInt32;
             FrameCount:TpvUInt32;
             BitDepth:TpvUInt8;
             ColourPrimaries:TpvUInt8;
             TransferFunction:TpvUInt8;
             Matrix:TpvUInt8;
             FullRange:TpvUInt8;
             ColourFlags:TpvUInt8;
             GOP:TpvUInt16;
             MasteringPrimariesX:array[0..2] of TpvUInt16;
             MasteringPrimariesY:array[0..2] of TpvUInt16;
             MasteringWhiteX:TpvUInt16;
             MasteringWhiteY:TpvUInt16;
             MasteringMaxLuminance:TpvUInt32;
             MasteringMinLuminance:TpvUInt32;
             MaxContentLightLevel:TpvUInt16;
             MaxFrameAvgLightLevel:TpvUInt16;
             AudioOffset:TpvUInt64;
             AudioSize:TpvUInt64;
             IndexOffset:TpvUInt64;
             PredictionMethod:TpvUInt8;
             ChromaQuantX16:TpvUInt8;
             ChromaFormat:TpvUInt8;
             Reserved2:array[0..5] of TpvUInt8;
             AudioCodec:array[0..3] of AnsiChar;
             MVCodec:TpvUInt8;
             H264Offset:TpvUInt64;
             H264Size:TpvUInt64;
             QPMapOffset:TpvUInt64; // AQ (per-tile QP): byte offset of the per-frame per-tile QP-map section
             QPMapSize:TpvUInt64; // total bytes of the qpmap section (frame_count * tile_cols * tile_rows u8); 0 = no AQ
            end;
            PFrameEntry=^TFrameEntry;
            { TFrameEntry }
            TFrameEntry=packed record // 28 bytes, byte-for-byte the packed C FrameEntry (coding order; POC = display order)
             Offset:TpvUInt64;
             Size:TpvUInt32;
             POC:TpvUInt32;
             Ref0:TpvInt32; // coding-order index of the L0 reference (-1 = none)
             Ref1:TpvInt32; // coding-order index of the L1 reference (-1 = none)
             FrameType:TpvUInt8; // 0 = I, 1 = P, 2 = B
             Quality:TpvUInt8;
             TemporalID:TpvUInt8;
             Pad:TpvUInt8;
            end;
            TFrameEntries=array of TFrameEntry;
            TBlockCounts=array[0..2] of TpvInt32;
            TPlaneOffsets=array[0..2] of PpvUInt32Array;
            TSynthesisGains=array[0..15,0..2] of TpvFloat; // per-level HL/LH/HH inverse-transform L2 gains
            { TBitReader }
            TBitReader=record // MSB-first CPU bit reader, window-based (the C BitReader)
             Bytes:PpvUInt8Array;
             ByteLength:TpvSizeUInt;
             Position:TpvSizeUInt;
             Window:TpvUInt64; // next bits, left-aligned (next bit at bit 63), zero-filled below WindowBits
             WindowBits:TpvInt32; // valid bits currently in the window (0..64)
             procedure Init(const aBytes:PpvUInt8Array;const aByteLength:TpvSizeUInt);
             procedure Refill;
             function GetBit:TpvInt32;
             function GetBits(const aBitCount:TpvInt32):TpvUInt32;
             function GetUnsignedExpGolomb:TpvUInt32;
             function GetSignedExpGolomb:TpvInt32;
            end;
            { TMVRangeDecoder }
            TMVRangeDecoder=record // LZMA-style adaptive binary range decoder for motion vectors (the C MVRangeDec)
             Input:PpvUInt8Array;
             Position:TpvSizeUInt;
             ByteLength:TpvSizeUInt;
             Code:TpvUInt32;
             Range:TpvUInt32;
             Res:array[0..1,0..MVRCBuckets-1,0..MVRCContextCap+1] of TpvUInt16; // [component][bucket][magnitude-class] residual contexts
             Flag:TpvUInt16; // quadtree split flag context
             Mode:array[0..1] of TpvUInt16; // L0/L1/BI mode contexts
             procedure Init(const aInput:PpvUInt8Array;const aByteLength:TpvSizeUInt);
             function DecodeBit(var aProbability:TpvUInt16):TpvInt32;
             function DecodeBypass:TpvInt32;
             function DecodeResidual(const aComponent,aBucket:TpvInt32):TpvInt32;
             function DecodeMode:TpvInt32;
            end;
      public
       // little-endian byte access into a payload buffer.
       class function ReadU32LE(const aBytes:PpvUInt8Array;const aOffset:TpvSizeUInt):TpvUInt32; static; {$ifdef caninline}inline;{$endif}
       // frame-payload (de)compression: [u8 method][u32 raw_len][raw / LZSS / LZBRRC bytes].
       class function LZSSDecompress(const aInput:PpvUInt8Array;const aInputLength:TpvSizeUInt;const aOutput:PpvUInt8Array;const aOutputSize:TpvSizeUInt):boolean; static;
       class function DecompressFrame(const aCompressed:PpvUInt8Array;const aCompressedLength:TpvSizeUInt;const aOutput:PpvUInt8Array;const aOutputCapacity:TpvSizeUInt;out aRawLength:TpvUInt32):boolean; static;
       // split a decompressed frame payload into the per-plane block offset arrays, the MV blob and the block data.
       class function ParseFrameHeader(const aFrame:PpvUInt8Array;const aFrameLength:TpvSizeUInt;const aBlockCount:TBlockCounts;const aOffsets:TPlaneOffsets;out aLeadingBlockCount:TpvInt32;out aMVDataOffset:TpvSizeUInt;out aMVLength:TpvUInt32;out aBlockDataOffset:TpvSizeUInt):boolean; static; // False = corrupt / truncated header
       // lossy quantization: measure the per-subband inverse-transform gains (once per level count), then build
       // the per-pixel integer quant step map (must match the C encoder/decoder bit-for-bit so the GPU dequant agrees).
       class procedure MeasureSynthesisGains(const aLevels:TpvInt32;out aHFGain:TSynthesisGains;out aLLGain:TpvFloat); static;
       class procedure BuildQuantizationSteps(const aStep:PpvInt32Array;const aWidth,aHeight,aLevels,aBaseQuality,aSampleWhite:TpvInt32;const aHFGain:TSynthesisGains;const aLLGain:TpvFloat); static;
       // AQ (per-tile QP / adaptive quantization): a per-frame per-tile weight map (transmitted in the qpmap container
       // section) modulates the per-coefficient quant steps IDENTICALLY on encoder + decoder, so the GPU dequant is
       // untouched and the round trip stays exact. The map is normalised (a coefficient's tile = its position within
       // its own subband), so one map fits every plane (luma full-res, chroma subsampled).
       class function AQTileCols(const aWidth:TpvInt32):TpvInt32; static;
       class function AQTileRows(const aHeight:TpvInt32):TpvInt32; static;
       class procedure ApplyTileAQ(const aStep:PpvInt32Array;const aWidth,aHeight,aLevels:TpvInt32;const aTileCode:PpvUInt8Array;const aTileCols,aTileRows:TpvInt32); static;
       // colordiff (B) motion vectors: golomb-coded, causal-median predicted, raster order, per fixed-grid block.
       class procedure DecodeMotionVectors(var aReader:TBitReader;const aMV:PpvInt32Array;const aBlocksX,aBlocksY:TpvInt32); static;
       // variable motion: a quadtree of 32->8 px leaves (RLE split flags + golomb leaf deltas) expanded into the fine 8-grid.
       class procedure DecodeMotionQuadtree(var aReader:TBitReader;const aFineMV:PpvInt32Array;const aFineGridX,aFineGridY:TpvInt32); static;
       // range-coded motion vectors (mv_codec=1): fixed grid (with neighbour-class context) or variable quadtree.
       class procedure DecodeMotionVectorsRange(var aDecoder:TMVRangeDecoder;const aMV:PpvInt32Array;const aBlocksX,aBlocksY:TpvInt32); static;
       class procedure DecodeMotionQuadtreeRange(var aDecoder:TMVRangeDecoder;const aFineMV:PpvInt32Array;const aFineGridX,aFineGridY:TpvInt32); static;
       // B-frame per-block L0/L1/BI mode field coded as its own quadtree (2-bit leaf, no predictor).
       class procedure DecodeModeQuadtree(var aReader:TBitReader;const aMode:PpvInt32Array;const aFineGridX,aFineGridY:TpvInt32); static;
       class procedure DecodeModeQuadtreeRange(var aDecoder:TMVRangeDecoder;const aMode:PpvInt32Array;const aFineGridX,aFineGridY:TpvInt32); static;
       // 3D-DWT temporal-mode quant: the temporal subband level of a GOP frame, and its per-level quant multiplier
       // (TAV: coarser quant for higher temporal frequencies).
       class function TemporalQuantLevel(const aFrame,aGOPCount,aTemporalLevels:TpvInt32):TpvInt32; static;
       class function TemporalQuantScale(const aLevel:TpvInt32):TpvFloat; static;
      end;

implementation

{ TpvFlexibleWaveletVideo.TBitReader }

procedure TpvFlexibleWaveletVideo.TBitReader.Init(const aBytes:PpvUInt8Array;const aByteLength:TpvSizeUInt);
begin
 Bytes:=aBytes;
 ByteLength:=aByteLength;
 Position:=0;
 Window:=0;
 WindowBits:=0;
end;

procedure TpvFlexibleWaveletVideo.TBitReader.Refill;
begin
 while (WindowBits<=56) and (Position<ByteLength) do begin
  Window:=Window or (TpvUInt64(Bytes^[Position]) shl (56-WindowBits));
  inc(Position);
  inc(WindowBits,8);
 end;
end;

function TpvFlexibleWaveletVideo.TBitReader.GetBit:TpvInt32;
begin
 if WindowBits=0 then begin
  Refill;
  if WindowBits=0 then begin
   result:=0; // past end of stream: read as zeros
   exit;
  end;
 end;
 result:=TpvInt32(Window shr 63);
 Window:=Window shl 1;
 dec(WindowBits);
end;

function TpvFlexibleWaveletVideo.TBitReader.GetBits(const aBitCount:TpvInt32):TpvUInt32;
begin
 if aBitCount<=0 then begin
  result:=0;
  exit;
 end;
 if WindowBits<aBitCount then begin
  Refill;
 end;
 result:=TpvUInt32(Window shr (64-aBitCount)); // top aBitCount bits (zero-filled past end)
 if WindowBits>=aBitCount then begin
  Window:=Window shl aBitCount;
  dec(WindowBits,aBitCount);
 end else begin
  Window:=0;
  WindowBits:=0;
 end;
end;

function TpvFlexibleWaveletVideo.TBitReader.GetUnsignedExpGolomb:TpvUInt32;
var BitCount,Index:TpvInt32;
    Mantissa:TpvUInt32;
begin

 // Unary prefix: count the leading zeros (capped at 31)
 BitCount:=0;
 while GetBit=0 do begin
  inc(BitCount);
  if BitCount>31 then begin
   break;
  end;
 end;

 // Then BitCount mantissa bits below the implicit leading 1
 Mantissa:=1;
 for Index:=0 to BitCount-1 do begin
  Mantissa:=(Mantissa shl 1) or TpvUInt32(GetBit);
 end;
 result:=Mantissa-1;

end;

function TpvFlexibleWaveletVideo.TBitReader.GetSignedExpGolomb:TpvInt32;
var Sentinel:TpvUInt64;
    BitCount,Consume:TpvInt32;
    Mantissa,Mapped:TpvUInt32;
begin

 // The unary prefix length = leading-zero count of the window; a sentinel 1 just past the valid bits
 // bounds the count so it never runs into the zero-fill (or past the end).
 Refill;
 Sentinel:=Window;
 if WindowBits<64 then begin
  Sentinel:=Sentinel or (TpvUInt64(1) shl (63-WindowBits));
 end;
 if Sentinel=0 then begin
  BitCount:=31;
 end else begin
  BitCount:=0;
  while (Sentinel shr 63)=0 do begin
   Sentinel:=Sentinel shl 1;
   inc(BitCount);
  end;
 end;
 if BitCount>31 then begin
  BitCount:=31;
 end;

 // Consume the prefix zeros plus the terminating 1, then read the BitCount mantissa bits
 Consume:=BitCount+1;
 if Consume>WindowBits then begin
  Consume:=WindowBits;
 end;
 Window:=Window shl Consume;
 dec(WindowBits,Consume);
 Mantissa:=TpvUInt32(1) shl BitCount;
 if BitCount>0 then begin
  Mantissa:=Mantissa or GetBits(BitCount);
 end;

 // Un-zigzag back to a signed residual
 Mapped:=Mantissa-1;
 result:=TpvInt32((Mapped shr 1) xor (TpvUInt32(0)-(Mapped and 1)));

end;

{ TpvFlexibleWaveletVideo }

class function TpvFlexibleWaveletVideo.ReadU32LE(const aBytes:PpvUInt8Array;const aOffset:TpvSizeUInt):TpvUInt32;
begin
 result:=TpvUInt32(aBytes^[aOffset]) or
         (TpvUInt32(aBytes^[aOffset+1]) shl 8) or
         (TpvUInt32(aBytes^[aOffset+2]) shl 16) or
         (TpvUInt32(aBytes^[aOffset+3]) shl 24);
end;

class function TpvFlexibleWaveletVideo.LZSSDecompress(const aInput:PpvUInt8Array;const aInputLength:TpvSizeUInt;const aOutput:PpvUInt8Array;const aOutputSize:TpvSizeUInt):boolean;
var InPosition,OutPosition,Source:TpvSizeUInt;
    ControlBit,Offset,MatchLength,Index,RunLength:TpvInt32;
    Control:TpvUInt32;
begin
 InPosition:=0;
 OutPosition:=0;
 ControlBit:=32;
 Control:=0;
 while (InPosition<aInputLength) and (OutPosition<aOutputSize) do begin

  // A fresh 32-bit control word every 32 tokens (1 flag bit per token, LSB first)
  if ControlBit=32 then begin
   if (InPosition+4)>aInputLength then begin
    result:=false; // truncated control word
    exit;
   end;
   Control:=ReadU32LE(aInput,InPosition);
   inc(InPosition,4);
   ControlBit:=0;
  end;

  if (Control and (TpvUInt32(1) shl ControlBit))<>0 then begin

   // Match: [u16 offset][u8 length-4]
   if (InPosition+3)>aInputLength then begin
    result:=false; // truncated match token
    exit;
   end;
   Offset:=aInput^[InPosition] or (TpvInt32(aInput^[InPosition+1]) shl 8);
   MatchLength:=aInput^[InPosition+2]+LZMinMatch;
   inc(InPosition,3);
   // A final token may run past the frame end (the trailing control-word bits are zero padding, decoded as
   // phantom literals/matches); clamp it to the output bound. The C decoder instead relies on an oversized
   // frame buffer and ignores the slack — equivalent for the meaningful (first aOutputSize) bytes.
   if (OutPosition+TpvSizeUInt(MatchLength))>aOutputSize then begin
    MatchLength:=TpvInt32(aOutputSize-OutPosition);
   end;
   if (Offset=0) or (TpvSizeUInt(Offset)>OutPosition) then begin
    result:=false; // back-reference before the output start (invalid offset)
    exit;
   end;
   Source:=OutPosition-TpvSizeUInt(Offset);
   if TpvSizeUInt(Offset)>=TpvSizeUInt(MatchLength) then begin
    Move(aOutput^[Source],aOutput^[OutPosition],MatchLength); // non-overlapping
   end else if Offset=1 then begin
    FillChar(aOutput^[OutPosition],MatchLength,aOutput^[Source]); // 1-byte run
   end else begin
    for Index:=0 to MatchLength-1 do begin
     aOutput^[OutPosition+TpvSizeUInt(Index)]:=aOutput^[Source+TpvSizeUInt(Index)]; // small-offset pattern fill
    end;
   end;
   inc(OutPosition,TpvSizeUInt(MatchLength));
   inc(ControlBit);

  end else begin

   // A run of consecutive literals (control bits 0) -> one bulk copy
   RunLength:=0;
   while ((ControlBit+RunLength)<32) and (((Control shr (ControlBit+RunLength)) and 1)=0) do begin
    inc(RunLength);
   end;
   if (OutPosition+TpvSizeUInt(RunLength))>aOutputSize then begin
    RunLength:=TpvInt32(aOutputSize-OutPosition); // clamp the final literal run to the frame end (see the match case)
   end;
   if (InPosition+TpvSizeUInt(RunLength))>aInputLength then begin
    RunLength:=TpvInt32(aInputLength-InPosition); // ...and to the input bound (parity with the C decoder's literal clamp)
   end;
   Move(aInput^[InPosition],aOutput^[OutPosition],RunLength);
   inc(InPosition,TpvSizeUInt(RunLength));
   inc(OutPosition,TpvSizeUInt(RunLength));
   inc(ControlBit,RunLength);

  end;
 end;
 result:=OutPosition>=aOutputSize;
end;

class function TpvFlexibleWaveletVideo.DecompressFrame(const aCompressed:PpvUInt8Array;const aCompressedLength:TpvSizeUInt;const aOutput:PpvUInt8Array;const aOutputCapacity:TpvSizeUInt;out aRawLength:TpvUInt32):boolean;
var Method:TpvUInt8;
    DestData:TpvPointer;
    DestLength:TpvUInt64;
begin
 result:=false;
 aRawLength:=0;
 if aCompressedLength<5 then begin
  exit;
 end;

 // [u8 method][u32 raw_len][payload]
 Method:=aCompressed^[0];
 aRawLength:=ReadU32LE(aCompressed,1);
 if aRawLength>aOutputCapacity then begin
  exit;
 end;

 case Method of
  FrameMethodRaw:begin
   if aRawLength>(aCompressedLength-5) then begin // a raw payload cannot exceed its compressed container entry
    exit;
   end;
   Move(aCompressed^[5],aOutput^[0],aRawLength);
   result:=true;
  end;
  FrameMethodLZSS:begin
   result:=LZSSDecompress(PpvUInt8Array(@aCompressed^[5]),aCompressedLength-5,aOutput,aRawLength);
  end;
  FrameMethodLZBRRC:begin
   // pre-allocated DestData -> decompress in place; the frame's LZBRRC blob carries the redundant 8-byte
   // size header (aWithSize=true: the size + initial code register are read from it, like the C decoder).
   DestData:=aOutput;
   result:=LZBRRCDecompress(@aCompressed^[5],aCompressedLength-5,DestData,DestLength,TpvInt64(aRawLength),true);
  end;
 end;
end;

// aFrameLength = the decompressed frame payload length. Returns False (without dereferencing past the frame) if any
// field would read beyond it, so a corrupt / truncated container frame is rejected instead of walking out of bounds.
class function TpvFlexibleWaveletVideo.ParseFrameHeader(const aFrame:PpvUInt8Array;const aFrameLength:TpvSizeUInt;const aBlockCount:TBlockCounts;const aOffsets:TPlaneOffsets;out aLeadingBlockCount:TpvInt32;out aMVDataOffset:TpvSizeUInt;out aMVLength:TpvUInt32;out aBlockDataOffset:TpvSizeUInt):boolean;
var Cursor:TpvSizeUInt;
    SizeBlobLength,Running,DataLength:TpvUInt32;
    SizeReader:TBitReader;
    Plane,Block:TpvInt32;
begin

 result:=false;
 aLeadingBlockCount:=0;
 aMVDataOffset:=0;
 aMVLength:=0;
 aBlockDataOffset:=0;

 // [u32 luma_block_count] (sanity; the per-plane counts come from the caller) + [u32 size_blob_length]
 if aFrameLength<8 then begin
  exit;
 end;
 aLeadingBlockCount:=TpvInt32(ReadU32LE(aFrame,0));
 Cursor:=4;

 // [u32 size_blob_length][size blob] -> prefix-sum the unsigned-Exp-Golomb per-block sizes into per-plane offsets
 SizeBlobLength:=ReadU32LE(aFrame,Cursor);
 inc(Cursor,4);
 if (Cursor+TpvSizeUInt(SizeBlobLength))>aFrameLength then begin
  exit;
 end;
 SizeReader.Init(PpvUInt8Array(@aFrame^[Cursor]),SizeBlobLength);
 Running:=0;
 for Plane:=0 to 2 do begin
  for Block:=0 to aBlockCount[Plane]-1 do begin
   aOffsets[Plane]^[Block]:=Running;
   inc(Running,SizeReader.GetUnsignedExpGolomb);
  end;
 end;
 inc(Cursor,SizeBlobLength);

 // [u32 mv_length][mv blob]
 if (Cursor+4)>aFrameLength then begin
  exit;
 end;
 aMVLength:=ReadU32LE(aFrame,Cursor);
 inc(Cursor,4);
 if (Cursor+TpvSizeUInt(aMVLength))>aFrameLength then begin
  exit;
 end;
 aMVDataOffset:=Cursor;
 inc(Cursor,aMVLength);

 // [u32 data_length][block data]
 if (Cursor+4)>aFrameLength then begin
  exit;
 end;
 DataLength:=ReadU32LE(aFrame,Cursor);
 inc(Cursor,4);
 aBlockDataOffset:=Cursor;
 if (Cursor+TpvSizeUInt(DataLength))>aFrameLength then begin // the block data itself must fit within the frame
  exit;
 end;

 result:=true;

end;

// --- CPU float 9/7 (CDF 9/7) inverse transform, used only to measure the per-subband synthesis gains for the
//     quantization step map (the real frame iDWT runs on the GPU). Ported byte-faithfully from fwvwave.c. ---

const CDF97Alpha:TpvFloat=-1.586134342; // the four lifting coefficients + scale of the irreversible CDF 9/7
      CDF97Beta:TpvFloat=-0.052980118;
      CDF97Gamma:TpvFloat=0.882911076;
      CDF97Delta:TpvFloat=0.443506852;
      CDF97Scale:TpvFloat=1.230174105;
      QuantCoarseFactor:TpvFloat=2.0; // step of the finest-detail HF subband, in base_quality units
      QuantCSFHH:TpvFloat=1.4; // diagonal (HH) band tolerates a coarser step
      QuantCSFFine:TpvFloat=1.25; // the finest level's highest frequencies tolerate a coarser step
      MaxLineLength=8192; // longest DWT row/column the CPU helper supports

// Reflect an index back into [0,length) with whole-sample symmetric border extension.
function FWVMirrorIndex(const aIndex,aLength:TpvInt32):TpvInt32;
var Period,Index:TpvInt32;
begin
 if aLength=1 then begin
  result:=0;
 end else begin
  Period:=2*(aLength-1);
  Index:=aIndex mod Period;
  if Index<0 then begin
   inc(Index,Period);
  end;
  if Index>=aLength then begin
   Index:=Period-Index;
  end;
  result:=Index;
 end;
end;

function FWVSampleMirrored(const aLine:PpvFloatArray;const aIndex,aLength:TpvInt32):TpvFloat; {$ifdef caninline}inline;{$endif}
begin
 result:=aLine^[FWVMirrorIndex(aIndex,aLength)];
end;

// Inverse 1D CDF 9/7: interleave [low | high] back to even/odd, then undo the lifting in reverse order.
procedure FWVInverseCDF97(const aLine:PpvFloatArray;const aLength:TpvInt32);
var Scratch:array[0..MaxLineLength-1] of TpvFloat;
    k,LowCount:TpvInt32;
begin
 if aLength<2 then begin
  exit;
 end;
 LowCount:=(aLength+1) div 2;
 for k:=0 to aLength-1 do begin
  if (k and 1)<>0 then begin
   Scratch[k]:=aLine^[LowCount+(k shr 1)];
  end else begin
   Scratch[k]:=aLine^[k shr 1];
  end;
 end;
 Move(Scratch[0],aLine^[0],TpvSizeUInt(aLength)*SizeOf(TpvFloat));

 k:=0;
 while k<aLength do begin
  aLine^[k]:=aLine^[k]*CDF97Scale;
  inc(k,2);
 end;
 k:=1;
 while k<aLength do begin
  aLine^[k]:=aLine^[k]*(1.0/CDF97Scale);
  inc(k,2);
 end;
 k:=0;
 while k<aLength do begin
  aLine^[k]:=aLine^[k]-(CDF97Delta*(FWVSampleMirrored(aLine,k-1,aLength)+FWVSampleMirrored(aLine,k+1,aLength)));
  inc(k,2);
 end;
 k:=1;
 while k<aLength do begin
  aLine^[k]:=aLine^[k]-(CDF97Gamma*(FWVSampleMirrored(aLine,k-1,aLength)+FWVSampleMirrored(aLine,k+1,aLength)));
  inc(k,2);
 end;
 k:=0;
 while k<aLength do begin
  aLine^[k]:=aLine^[k]-(CDF97Beta*(FWVSampleMirrored(aLine,k-1,aLength)+FWVSampleMirrored(aLine,k+1,aLength)));
  inc(k,2);
 end;
 k:=1;
 while k<aLength do begin
  aLine^[k]:=aLine^[k]-(CDF97Alpha*(FWVSampleMirrored(aLine,k-1,aLength)+FWVSampleMirrored(aLine,k+1,aLength)));
  inc(k,2);
 end;
end;

// Strided variant: gather a column (stride = image width) into a contiguous line, transform, scatter back.
procedure FWVInverseCDF97Strided(const aBase:PpvFloatArray;const aLength,aStride:TpvInt32);
var Line:array[0..MaxLineLength-1] of TpvFloat;
    i:TpvInt32;
begin
 for i:=0 to aLength-1 do begin
  Line[i]:=aBase^[i*aStride];
 end;
 FWVInverseCDF97(PpvFloatArray(@Line[0]),aLength);
 for i:=0 to aLength-1 do begin
  aBase^[i*aStride]:=Line[i];
 end;
end;

// Inverse 2D wavelet, coarsest level first: per level the columns (strided) then the rows.
procedure FWVInverseDWT2D(const aPlane:PpvFloatArray;const aWidth,aHeight,aLevels:TpvInt32);
var LevelWidth,LevelHeight:array[0..15] of TpvInt32;
    LevelCount,Level,CurrentWidth,CurrentHeight,x,y:TpvInt32;
begin
 LevelCount:=0;
 CurrentWidth:=aWidth;
 CurrentHeight:=aHeight;
 Level:=0;
 while ((Level<aLevels) and (CurrentWidth>=2)) and (CurrentHeight>=2) do begin
  LevelWidth[LevelCount]:=CurrentWidth;
  LevelHeight[LevelCount]:=CurrentHeight;
  inc(LevelCount);
  CurrentWidth:=(CurrentWidth+1) div 2;
  CurrentHeight:=(CurrentHeight+1) div 2;
  inc(Level);
 end;
 for Level:=LevelCount-1 downto 0 do begin
  CurrentWidth:=LevelWidth[Level];
  CurrentHeight:=LevelHeight[Level];
  for x:=0 to CurrentWidth-1 do begin
   FWVInverseCDF97Strided(PpvFloatArray(@aPlane^[x]),CurrentHeight,aWidth);
  end;
  for y:=0 to CurrentHeight-1 do begin
   FWVInverseCDF97(PpvFloatArray(@aPlane^[TpvSizeUInt(y)*TpvSizeUInt(aWidth)]),CurrentWidth);
  end;
 end;
end;

class procedure TpvFlexibleWaveletVideo.MeasureSynthesisGains(const aLevels:TpvInt32;out aHFGain:TSynthesisGains;out aLLGain:TpvFloat);
var Grid,Level,QuadWidth,QuadHeight,HalfWidth,HalfHeight,j,Orientation,i,LowLowWidth,LowLowHeight:TpvInt32;
    PositionX,PositionY:array[0..2] of TpvInt32;
    Plane:array of TpvFloat;
    Energy:TpvDouble;
begin

 FillChar(aHFGain,SizeOf(aHFGain),#0);

 Grid:=1 shl (aLevels+3);
 if Grid<64 then begin
  Grid:=64;
 end;
 if Grid>512 then begin
  Grid:=512;
 end;
 SetLength(Plane,Grid*Grid);

 for Level:=0 to aLevels-1 do begin
  QuadWidth:=Grid;
  QuadHeight:=Grid;
  for j:=0 to Level-1 do begin
   QuadWidth:=(QuadWidth+1) div 2;
   QuadHeight:=(QuadHeight+1) div 2;
  end;
  HalfWidth:=(QuadWidth+1) div 2;
  HalfHeight:=(QuadHeight+1) div 2;
  PositionX[0]:=HalfWidth+((QuadWidth-HalfWidth) div 2);
  PositionX[1]:=HalfWidth div 2;
  PositionX[2]:=HalfWidth+((QuadWidth-HalfWidth) div 2);
  PositionY[0]:=HalfHeight div 2;
  PositionY[1]:=HalfHeight+((QuadHeight-HalfHeight) div 2);
  PositionY[2]:=HalfHeight+((QuadHeight-HalfHeight) div 2);
  for Orientation:=0 to 2 do begin
   FillChar(Plane[0],TpvSizeUInt(Grid)*TpvSizeUInt(Grid)*SizeOf(TpvFloat),#0);
   Plane[(PositionY[Orientation]*Grid)+PositionX[Orientation]]:=1.0;
   FWVInverseDWT2D(PpvFloatArray(@Plane[0]),Grid,Grid,aLevels);
   Energy:=0.0;
   for i:=0 to (Grid*Grid)-1 do begin
    Energy:=Energy+((Plane[i]+0.0)*(Plane[i]+0.0)); // +0.0 promotes single -> double losslessly (C's (double) cast)
   end;
   aHFGain[Level,Orientation]:=Sqrt(Energy);
  end;
 end;

 LowLowWidth:=Grid;
 LowLowHeight:=Grid;
 for j:=0 to aLevels-1 do begin
  LowLowWidth:=(LowLowWidth+1) div 2;
  LowLowHeight:=(LowLowHeight+1) div 2;
 end;
 FillChar(Plane[0],TpvSizeUInt(Grid)*TpvSizeUInt(Grid)*SizeOf(TpvFloat),#0);
 Plane[((LowLowHeight div 2)*Grid)+(LowLowWidth div 2)]:=1.0;
 FWVInverseDWT2D(PpvFloatArray(@Plane[0]),Grid,Grid,aLevels);
 Energy:=0.0;
 for i:=0 to (Grid*Grid)-1 do begin
  Energy:=Energy+((Plane[i]+0.0)*(Plane[i]+0.0));
 end;
 aLLGain:=Sqrt(Energy);

end;

class procedure TpvFlexibleWaveletVideo.BuildQuantizationSteps(const aStep:PpvInt32Array;const aWidth,aHeight,aLevels,aBaseQuality,aSampleWhite:TpvInt32;const aHFGain:TSynthesisGains;const aLLGain:TpvFloat);
var BaseQuality,CurrentWidth,CurrentHeight,Level,HalfWidth,HalfHeight,GainLevel,x,y,Orientation,q,LowPassQ,i:TpvInt32;
    InRight,InBottom:boolean;
    GainReference,Csf,RelativeGain,RelativeLowLow,QValue:TpvFloat;
begin

 // Scale Q to the reference-white level so a given Q means the same coarseness at any bit depth (SDR -> x1).
 BaseQuality:=aBaseQuality*(aSampleWhite div 256);

 if aHFGain[0,0]>0.0 then begin
  GainReference:=aHFGain[0,0]; // finest HF (HL) — the anchor
 end else begin
  GainReference:=1.0;
 end;

 for i:=0 to (aWidth*aHeight)-1 do begin
  aStep^[i]:=BaseQuality;
 end;

 CurrentWidth:=aWidth;
 CurrentHeight:=aHeight;
 Level:=0;
 while ((Level<aLevels) and (CurrentWidth>=2)) and (CurrentHeight>=2) do begin
  HalfWidth:=(CurrentWidth+1) div 2;
  HalfHeight:=(CurrentHeight+1) div 2;
  if Level<16 then begin
   GainLevel:=Level;
  end else begin
   GainLevel:=15;
  end;
  for y:=0 to CurrentHeight-1 do begin
   for x:=0 to CurrentWidth-1 do begin
    InRight:=x>=HalfWidth;
    InBottom:=y>=HalfHeight;
    if (not InRight) and (not InBottom) then begin
     continue; // the LL of this level -> set by the next level (or the final LL below)
    end;
    if InRight and (not InBottom) then begin
     Orientation:=0; // HL
    end else if (not InRight) and InBottom then begin
     Orientation:=1; // LH
    end else begin
     Orientation:=2; // HH
    end;
    if Orientation=2 then begin
     Csf:=QuantCSFHH;
    end else begin
     Csf:=1.0;
    end;
    if Level=0 then begin
     Csf:=Csf*QuantCSFFine;
    end;
    RelativeGain:=aHFGain[GainLevel,Orientation]/GainReference;
    if RelativeGain<=0.0 then begin
     RelativeGain:=1.0;
    end;
    QValue:=(((TpvFloat(BaseQuality)*QuantCoarseFactor)*Csf)/RelativeGain)+0.5; // single store -> rounds like C's +0.5f
    q:=Trunc(QValue);
    if q<1 then begin
     q:=1;
    end;
    aStep^[(y*aWidth)+x]:=q;
   end;
  end;
  CurrentWidth:=HalfWidth;
  CurrentHeight:=HalfHeight;
  inc(Level);
 end;

 RelativeLowLow:=aLLGain/GainReference;
 if RelativeLowLow<=0.0 then begin
  RelativeLowLow:=1.0;
 end;
 QValue:=((TpvFloat(BaseQuality)*QuantCoarseFactor)/RelativeLowLow)+0.5;
 LowPassQ:=Trunc(QValue);
 if LowPassQ<1 then begin
  LowPassQ:=1;
 end;
 for y:=0 to CurrentHeight-1 do begin
  for x:=0 to CurrentWidth-1 do begin
   aStep^[(y*aWidth)+x]:=LowPassQ;
  end;
 end;

end;

{ TpvFlexibleWaveletVideo — AQ (per-tile QP / adaptive quantization) }

class function TpvFlexibleWaveletVideo.AQTileCols(const aWidth:TpvInt32):TpvInt32;
begin
 result:=(aWidth+(AQTile-1)) div AQTile;
end;

class function TpvFlexibleWaveletVideo.AQTileRows(const aHeight:TpvInt32):TpvInt32;
begin
 result:=(aHeight+(AQTile-1)) div AQTile;
end;

// Modulate step[] by the per-tile weight map, IDENTICALLY to the C encoder/decoder (apply_tile_aq), walking the same
// Mallat layout BuildQuantizationSteps uses. Each coefficient's tile = its normalised position within its own subband.
class procedure TpvFlexibleWaveletVideo.ApplyTileAQ(const aStep:PpvInt32Array;const aWidth,aHeight,aLevels:TpvInt32;const aTileCode:PpvUInt8Array;const aTileCols,aTileRows:TpvInt32);
var WeightLUT:array[0..255] of TpvFloat;
    LogSpan,Weight,QuantValue:TpvFloat;
    CurrentWidth,CurrentHeight,Level,HalfWidth,HalfHeight,PixelX,PixelY:TpvInt32;
    SubbandX,SubbandY,SubbandWidth,SubbandHeight,TileColumn,TileRow,QuantStep,CodeIndex:TpvInt32;
    InRight,InBottom:boolean;
begin

 // u8 code -> weight lookup, log-spaced so 1.0 sits at the middle (IDENTICAL to aq_weight_from_code on the encoder).
 LogSpan:=Ln(AQWeightMax/AQWeightMin);
 for CodeIndex:=0 to 255 do begin
  WeightLUT[CodeIndex]:=AQWeightMin*Exp((TpvFloat(CodeIndex)/255.0)*LogSpan);
 end;

 CurrentWidth:=aWidth;
 CurrentHeight:=aHeight;
 Level:=0;
 while ((Level<aLevels) and (CurrentWidth>=2)) and (CurrentHeight>=2) do begin
  HalfWidth:=(CurrentWidth+1) div 2;
  HalfHeight:=(CurrentHeight+1) div 2;
  for PixelY:=0 to CurrentHeight-1 do begin
   for PixelX:=0 to CurrentWidth-1 do begin
    InRight:=PixelX>=HalfWidth;
    InBottom:=PixelY>=HalfHeight;
    if (not InRight) and (not InBottom) then begin
     continue; // the LL of this level -> handled at the final level below
    end;
    if InRight then begin
     SubbandX:=PixelX-HalfWidth;
     SubbandWidth:=CurrentWidth-HalfWidth;
    end else begin
     SubbandX:=PixelX;
     SubbandWidth:=HalfWidth;
    end;
    if InBottom then begin
     SubbandY:=PixelY-HalfHeight;
     SubbandHeight:=CurrentHeight-HalfHeight;
    end else begin
     SubbandY:=PixelY;
     SubbandHeight:=HalfHeight;
    end;
    if SubbandWidth>0 then begin
     TileColumn:=(SubbandX*aTileCols) div SubbandWidth;
    end else begin
     TileColumn:=0;
    end;
    if SubbandHeight>0 then begin
     TileRow:=(SubbandY*aTileRows) div SubbandHeight;
    end else begin
     TileRow:=0;
    end;
    if TileColumn>=aTileCols then begin
     TileColumn:=aTileCols-1;
    end;
    if TileRow>=aTileRows then begin
     TileRow:=aTileRows-1;
    end;
    Weight:=WeightLUT[aTileCode^[(TileRow*aTileCols)+TileColumn]];
    QuantValue:=(TpvFloat(aStep^[(PixelY*aWidth)+PixelX])*Weight)+0.5; // single store -> rounds like C's +0.5f
    QuantStep:=Trunc(QuantValue);
    if QuantStep<1 then begin
     QuantStep:=1;
    end;
    aStep^[(PixelY*aWidth)+PixelX]:=QuantStep;
   end;
  end;
  CurrentWidth:=HalfWidth;
  CurrentHeight:=HalfHeight;
  inc(Level);
 end;

 // the final LL
 for PixelY:=0 to CurrentHeight-1 do begin
  for PixelX:=0 to CurrentWidth-1 do begin
   if CurrentWidth>0 then begin
    TileColumn:=(PixelX*aTileCols) div CurrentWidth;
   end else begin
    TileColumn:=0;
   end;
   if CurrentHeight>0 then begin
    TileRow:=(PixelY*aTileRows) div CurrentHeight;
   end else begin
    TileRow:=0;
   end;
   if TileColumn>=aTileCols then begin
    TileColumn:=aTileCols-1;
   end;
   if TileRow>=aTileRows then begin
    TileRow:=aTileRows-1;
   end;
   Weight:=WeightLUT[aTileCode^[(TileRow*aTileCols)+TileColumn]];
   QuantValue:=(TpvFloat(aStep^[(PixelY*aWidth)+PixelX])*Weight)+0.5;
   QuantStep:=Trunc(QuantValue);
   if QuantStep<1 then begin
    QuantStep:=1;
   end;
   aStep^[(PixelY*aWidth)+PixelX]:=QuantStep;
  end;
 end;

end;

// Median of three (the MV predictor's combiner), exactly as the C median3.
function FWVMedian3(const a,b,c:TpvInt32):TpvInt32;
begin
 if a<b then begin
  if b<c then begin
   result:=b;
  end else if a<c then begin
   result:=c;
  end else begin
   result:=a;
  end;
 end else begin
  if a<c then begin
   result:=a;
  end else if b<c then begin
   result:=c;
  end else begin
   result:=b;
  end;
 end;
end;

// Causal median predictor: median of the left, up and up-right already-decoded neighbour MVs.
function FWVPredictMotionComponent(const aMV:PpvInt32Array;const aBlocksX,aBlockX,aBlockY,aComponent:TpvInt32):TpvInt32;
var Left,Up,UpRight:TpvInt32;
begin
 if aBlockX>0 then begin
  Left:=aMV^[(((aBlockY*aBlocksX)+(aBlockX-1))*2)+aComponent];
 end else begin
  Left:=0;
 end;
 if aBlockY>0 then begin
  Up:=aMV^[((((aBlockY-1)*aBlocksX)+aBlockX)*2)+aComponent];
 end else begin
  Up:=0;
 end;
 if (aBlockY>0) and ((aBlockX+1)<aBlocksX) then begin
  UpRight:=aMV^[((((aBlockY-1)*aBlocksX)+(aBlockX+1))*2)+aComponent];
 end else begin
  UpRight:=0;
 end;
 result:=FWVMedian3(Left,Up,UpRight);
end;

class procedure TpvFlexibleWaveletVideo.DecodeMotionVectors(var aReader:TBitReader;const aMV:PpvInt32Array;const aBlocksX,aBlocksY:TpvInt32);
var BlockX,BlockY,Component,Prediction:TpvInt32;
begin
 for BlockY:=0 to aBlocksY-1 do begin
  for BlockX:=0 to aBlocksX-1 do begin
   for Component:=0 to 1 do begin
    Prediction:=FWVPredictMotionComponent(aMV,aBlocksX,BlockX,BlockY,Component);
    aMV^[(((BlockY*aBlocksX)+BlockX)*2)+Component]:=Prediction+aReader.GetSignedExpGolomb;
   end;
  end;
 end;
end;

// Causal median predictor on the fine grid: left, up and up-LEFT (all already-decoded under root-raster + child
// z-order; the fixed grid's up-RIGHT is NOT available here).
function FWVPredictFineMV(const aFineMV:PpvInt32Array;const aFineGridX,aX,aY,aComponent:TpvInt32):TpvInt32;
var Left,Up,UpLeft:TpvInt32;
begin
 if aX>0 then begin
  Left:=aFineMV^[(((aY*aFineGridX)+(aX-1))*2)+aComponent];
 end else begin
  Left:=0;
 end;
 if aY>0 then begin
  Up:=aFineMV^[((((aY-1)*aFineGridX)+aX)*2)+aComponent];
 end else begin
  Up:=0;
 end;
 if (aX>0) and (aY>0) then begin
  UpLeft:=aFineMV^[((((aY-1)*aFineGridX)+(aX-1))*2)+aComponent];
 end else begin
  UpLeft:=0;
 end;
 result:=FWVMedian3(Left,Up,UpLeft);
end;

// The split flags are one RLE stream: [n_flags][run_of_0s][run_of_1s]... (the alternating runs start with 0s).
function FWVReadFlagRLE(var aReader:TpvFlexibleWaveletVideo.TBitReader;const aFlags:PpvUInt8Array):TpvInt32;
var NFlags,i,Run,k,Current:TpvInt32;
begin
 NFlags:=TpvInt32(aReader.GetUnsignedExpGolomb);
 i:=0;
 Current:=0;
 while i<NFlags do begin
  Run:=TpvInt32(aReader.GetUnsignedExpGolomb);
  k:=0;
  while (k<Run) and (i<NFlags) do begin
   aFlags^[i]:=TpvUInt8(Current);
   inc(i);
   inc(k);
  end;
  Current:=Current xor 1;
 end;
 result:=NFlags;
end;

// Walk the quadtree driven by the pre-read flag array (root-raster + child z-order), expanding each leaf MV.
procedure FWVDecodeQuadtreeMVs(var aReader:TpvFlexibleWaveletVideo.TBitReader;const aFineMV:PpvInt32Array;const aFineGridX,aFineGridY,aX0,aY0,aCells:TpvInt32;const aFlags:PpvUInt8Array;var aFlagIndex:TpvInt32);
var Split,Component,Prediction,Half,dx,dy,Index:TpvInt32;
    MV:array[0..1] of TpvInt32;
begin
 if aCells<>1 then begin
  Split:=aFlags^[aFlagIndex];
  inc(aFlagIndex);
 end else begin
  Split:=0; // an 8-leaf has no flag
 end;
 if Split=0 then begin
  for Component:=0 to 1 do begin
   Prediction:=FWVPredictFineMV(aFineMV,aFineGridX,aX0,aY0,Component);
   MV[Component]:=Prediction+aReader.GetSignedExpGolomb;
  end;
  dy:=0;
  while (dy<aCells) and ((aY0+dy)<aFineGridY) do begin // expand the leaf MV to all its fine cells
   dx:=0;
   while (dx<aCells) and ((aX0+dx)<aFineGridX) do begin
    Index:=(((aY0+dy)*aFineGridX)+(aX0+dx))*2;
    aFineMV^[Index]:=MV[0];
    aFineMV^[Index+1]:=MV[1];
    inc(dx);
   end;
   inc(dy);
  end;
 end else begin
  Half:=aCells div 2;
  FWVDecodeQuadtreeMVs(aReader,aFineMV,aFineGridX,aFineGridY,aX0,aY0,Half,aFlags,aFlagIndex);
  FWVDecodeQuadtreeMVs(aReader,aFineMV,aFineGridX,aFineGridY,aX0+Half,aY0,Half,aFlags,aFlagIndex);
  FWVDecodeQuadtreeMVs(aReader,aFineMV,aFineGridX,aFineGridY,aX0,aY0+Half,Half,aFlags,aFlagIndex);
  FWVDecodeQuadtreeMVs(aReader,aFineMV,aFineGridX,aFineGridY,aX0+Half,aY0+Half,Half,aFlags,aFlagIndex);
 end;
end;

class procedure TpvFlexibleWaveletVideo.DecodeMotionQuadtree(var aReader:TBitReader;const aFineMV:PpvInt32Array;const aFineGridX,aFineGridY:TpvInt32);
var RootCells,Grid32X,Grid32Y,Capacity,FlagIndex,RootX,RootY:TpvInt32;
    Flags:array of TpvUInt8;
begin
 RootCells:=MotionRoot div MotionLeaf; // 32 / 8 = 4 fine cells per root axis
 Grid32X:=((aFineGridX+RootCells)-1) div RootCells;
 Grid32Y:=((aFineGridY+RootCells)-1) div RootCells;
 Capacity:=((Grid32X*Grid32Y)*5)+16; // a fully-split root emits 1 [32] + 4 [16] flags
 SetLength(Flags,Capacity);
 FWVReadFlagRLE(aReader,PpvUInt8Array(@Flags[0]));
 FlagIndex:=0;
 RootY:=0;
 while RootY<aFineGridY do begin
  RootX:=0;
  while RootX<aFineGridX do begin
   FWVDecodeQuadtreeMVs(aReader,aFineMV,aFineGridX,aFineGridY,RootX,RootY,RootCells,PpvUInt8Array(@Flags[0]),FlagIndex);
   inc(RootX,RootCells);
  end;
  inc(RootY,RootCells);
 end;
end;

{ TpvFlexibleWaveletVideo.TMVRangeDecoder }

procedure TpvFlexibleWaveletVideo.TMVRangeDecoder.Init(const aInput:PpvUInt8Array;const aByteLength:TpvSizeUInt);
var i,Component,Bucket,j:TpvInt32;
begin
 Input:=aInput;
 ByteLength:=aByteLength;
 Position:=0;
 Range:=$ffffffff;
 Code:=0;
 for i:=0 to 3 do begin // prime the code register with the first 4 bytes (big-endian)
  if Position<ByteLength then begin
   Code:=(Code shl 8) or Input^[Position];
  end else begin
   Code:=Code shl 8;
  end;
  inc(Position);
 end;
 for Component:=0 to 1 do begin
  for Bucket:=0 to MVRCBuckets-1 do begin
   for j:=0 to MVRCContextCap+1 do begin
    Res[Component,Bucket,j]:=2048;
   end;
  end;
 end;
 Flag:=2048;
 Mode[0]:=2048;
 Mode[1]:=2048;
end;

function TpvFlexibleWaveletVideo.TMVRangeDecoder.DecodeBit(var aProbability:TpvUInt16):TpvInt32;
var Bound:TpvUInt32;
begin
 Bound:=(Range shr 12)*aProbability;
 if Code<Bound then begin
  Range:=Bound;
  aProbability:=aProbability+((4096-aProbability) shr 5);
  result:=0;
 end else begin
  Code:=Code-Bound;
  Range:=Range-Bound;
  aProbability:=aProbability-(aProbability shr 5);
  result:=1;
 end;
 while Range<$1000000 do begin
  if Position<ByteLength then begin
   Code:=(Code shl 8) or Input^[Position];
  end else begin
   Code:=Code shl 8;
  end;
  inc(Position);
  Range:=Range shl 8;
 end;
end;

function TpvFlexibleWaveletVideo.TMVRangeDecoder.DecodeBypass:TpvInt32;
var Bound:TpvUInt32;
begin
 Bound:=Range shr 1;
 if Code<Bound then begin
  Range:=Bound;
  result:=0;
 end else begin
  Code:=Code-Bound;
  Range:=Range-Bound;
  result:=1;
 end;
 while Range<$1000000 do begin
  if Position<ByteLength then begin
   Code:=(Code shl 8) or Input^[Position];
  end else begin
   Code:=Code shl 8;
  end;
  inc(Position);
  Range:=Range shl 8;
 end;
end;

function TpvFlexibleWaveletVideo.TMVRangeDecoder.DecodeResidual(const aComponent,aBucket:TpvInt32):TpvInt32;
var k,b,ContextIndex:TpvInt32;
    u:TpvUInt32;
begin
 // truncated-unary magnitude class (adaptive), then k-1 bypass mantissa bits, then zig-zag back to signed.
 k:=0;
 while true do begin
  if k<MVRCContextCap then begin
   ContextIndex:=k;
  end else begin
   ContextIndex:=MVRCContextCap;
  end;
  if DecodeBit(Res[aComponent and 1,aBucket,ContextIndex])=1 then begin
   inc(k);
  end else begin
   break;
  end;
 end;
 if k=0 then begin
  result:=0;
 end else begin
  u:=TpvUInt32(1) shl (k-1);
  for b:=k-2 downto 0 do begin
   u:=u or (TpvUInt32(DecodeBypass) shl b);
  end;
  if (u and 1)<>0 then begin
   result:=-TpvInt32((u+1) shr 1);
  end else begin
   result:=TpvInt32(u shr 1);
  end;
 end;
end;

function TpvFlexibleWaveletVideo.TMVRangeDecoder.DecodeMode:TpvInt32;
begin
 if DecodeBit(Mode[0])=0 then begin
  result:=0;
 end else if DecodeBit(Mode[1])<>0 then begin
  result:=2;
 end else begin
  result:=1;
 end;
end;

// Neighbour-magnitude context bucket + magnitude class (bit length), exactly as the C mvrc_bucket / mvrc_classof.
function FWVMVRCBucket(const aSum:TpvInt32):TpvInt32;
begin
 if aSum=0 then begin
  result:=0;
 end else if aSum<=2 then begin
  result:=1;
 end else if aSum<=4 then begin
  result:=2;
 end else if aSum<=6 then begin
  result:=3;
 end else if aSum<=10 then begin
  result:=4;
 end else begin
  result:=5;
 end;
end;

function FWVMVRCClassOf(const aValue:TpvUInt32):TpvInt32;
var v:TpvUInt32;
begin
 result:=0; // 32 - clz(value) = bit length
 v:=aValue;
 while v<>0 do begin
  inc(result);
  v:=v shr 1;
 end;
end;

// Zig-zag encode a signed residual to the unsigned magnitude the class/bucket model uses: (v<<1) ^ (v>>31).
function FWVZigZag(const aValue:TpvInt32):TpvUInt32;
begin
 result:=TpvUInt32(aValue) shl 1;
 if aValue<0 then begin
  result:=result xor $ffffffff;
 end;
end;

class procedure TpvFlexibleWaveletVideo.DecodeMotionVectorsRange(var aDecoder:TMVRangeDecoder;const aMV:PpvInt32Array;const aBlocksX,aBlocksY:TpvInt32);
var BlockX,BlockY,Component,Prediction,Left,Up,v,Index:TpvInt32;
    Cls:array of TpvInt32;
begin
 SetLength(Cls,aBlocksX*aBlocksY*2); // neighbour-class store, zero-initialised
 for BlockY:=0 to aBlocksY-1 do begin
  for BlockX:=0 to aBlocksX-1 do begin
   for Component:=0 to 1 do begin
    Prediction:=FWVPredictMotionComponent(aMV,aBlocksX,BlockX,BlockY,Component);
    if BlockX>0 then begin
     Left:=Cls[(((BlockY*aBlocksX)+(BlockX-1))*2)+Component];
    end else begin
     Left:=0;
    end;
    if BlockY>0 then begin
     Up:=Cls[((((BlockY-1)*aBlocksX)+BlockX)*2)+Component];
    end else begin
     Up:=0;
    end;
    v:=aDecoder.DecodeResidual(Component,FWVMVRCBucket(Left+Up));
    Index:=((BlockY*aBlocksX)+BlockX)*2;
    aMV^[Index+Component]:=Prediction+v;
    Cls[Index+Component]:=FWVMVRCClassOf(FWVZigZag(v));
   end;
  end;
 end;
end;

// Range-coded variable quadtree (z-order leaves use neighbour-context bucket 0, matching the encoder).
procedure FWVDecodeQuadtreeMVsRange(var aDecoder:TpvFlexibleWaveletVideo.TMVRangeDecoder;const aFineMV:PpvInt32Array;const aFineGridX,aFineGridY,aX0,aY0,aCells:TpvInt32);
var Split,Component,Prediction,Half,dx,dy,Index:TpvInt32;
    MV:array[0..1] of TpvInt32;
begin
 if aCells<>1 then begin
  Split:=aDecoder.DecodeBit(aDecoder.Flag);
 end else begin
  Split:=0;
 end;
 if Split=0 then begin
  for Component:=0 to 1 do begin
   Prediction:=FWVPredictFineMV(aFineMV,aFineGridX,aX0,aY0,Component);
   MV[Component]:=Prediction+aDecoder.DecodeResidual(Component,0);
  end;
  dy:=0;
  while (dy<aCells) and ((aY0+dy)<aFineGridY) do begin
   dx:=0;
   while (dx<aCells) and ((aX0+dx)<aFineGridX) do begin
    Index:=(((aY0+dy)*aFineGridX)+(aX0+dx))*2;
    aFineMV^[Index]:=MV[0];
    aFineMV^[Index+1]:=MV[1];
    inc(dx);
   end;
   inc(dy);
  end;
 end else begin
  Half:=aCells div 2;
  FWVDecodeQuadtreeMVsRange(aDecoder,aFineMV,aFineGridX,aFineGridY,aX0,aY0,Half);
  FWVDecodeQuadtreeMVsRange(aDecoder,aFineMV,aFineGridX,aFineGridY,aX0+Half,aY0,Half);
  FWVDecodeQuadtreeMVsRange(aDecoder,aFineMV,aFineGridX,aFineGridY,aX0,aY0+Half,Half);
  FWVDecodeQuadtreeMVsRange(aDecoder,aFineMV,aFineGridX,aFineGridY,aX0+Half,aY0+Half,Half);
 end;
end;

class procedure TpvFlexibleWaveletVideo.DecodeMotionQuadtreeRange(var aDecoder:TMVRangeDecoder;const aFineMV:PpvInt32Array;const aFineGridX,aFineGridY:TpvInt32);
var RootCells,RootX,RootY:TpvInt32;
begin
 RootCells:=MotionRoot div MotionLeaf;
 RootY:=0;
 while RootY<aFineGridY do begin
  RootX:=0;
  while RootX<aFineGridX do begin
   FWVDecodeQuadtreeMVsRange(aDecoder,aFineMV,aFineGridX,aFineGridY,RootX,RootY,RootCells);
   inc(RootX,RootCells);
  end;
  inc(RootY,RootCells);
 end;
end;

// Walk the mode quadtree (golomb): leaf split from the pre-read flags, a 2-bit mode per leaf, expanded to its cells.
procedure FWVDecodeModeLeaves(var aReader:TpvFlexibleWaveletVideo.TBitReader;const aMode:PpvInt32Array;const aFineGridX,aFineGridY,aX0,aY0,aCells:TpvInt32;const aFlags:PpvUInt8Array;var aFlagIndex:TpvInt32);
var Split,Mode,Half,dx,dy:TpvInt32;
begin
 if aCells<>1 then begin
  Split:=aFlags^[aFlagIndex];
  inc(aFlagIndex);
 end else begin
  Split:=0;
 end;
 if Split=0 then begin
  Mode:=TpvInt32(aReader.GetBits(2));
  dy:=0;
  while (dy<aCells) and ((aY0+dy)<aFineGridY) do begin
   dx:=0;
   while (dx<aCells) and ((aX0+dx)<aFineGridX) do begin
    aMode^[((aY0+dy)*aFineGridX)+(aX0+dx)]:=Mode;
    inc(dx);
   end;
   inc(dy);
  end;
 end else begin
  Half:=aCells div 2;
  FWVDecodeModeLeaves(aReader,aMode,aFineGridX,aFineGridY,aX0,aY0,Half,aFlags,aFlagIndex);
  FWVDecodeModeLeaves(aReader,aMode,aFineGridX,aFineGridY,aX0+Half,aY0,Half,aFlags,aFlagIndex);
  FWVDecodeModeLeaves(aReader,aMode,aFineGridX,aFineGridY,aX0,aY0+Half,Half,aFlags,aFlagIndex);
  FWVDecodeModeLeaves(aReader,aMode,aFineGridX,aFineGridY,aX0+Half,aY0+Half,Half,aFlags,aFlagIndex);
 end;
end;

class procedure TpvFlexibleWaveletVideo.DecodeModeQuadtree(var aReader:TBitReader;const aMode:PpvInt32Array;const aFineGridX,aFineGridY:TpvInt32);
var RootCells,Grid32X,Grid32Y,Capacity,FlagIndex,RootX,RootY:TpvInt32;
    Flags:array of TpvUInt8;
begin
 RootCells:=MotionRoot div MotionLeaf;
 Grid32X:=((aFineGridX+RootCells)-1) div RootCells;
 Grid32Y:=((aFineGridY+RootCells)-1) div RootCells;
 Capacity:=((Grid32X*Grid32Y)*5)+16;
 SetLength(Flags,Capacity);
 FWVReadFlagRLE(aReader,PpvUInt8Array(@Flags[0]));
 FlagIndex:=0;
 RootY:=0;
 while RootY<aFineGridY do begin
  RootX:=0;
  while RootX<aFineGridX do begin
   FWVDecodeModeLeaves(aReader,aMode,aFineGridX,aFineGridY,RootX,RootY,RootCells,PpvUInt8Array(@Flags[0]),FlagIndex);
   inc(RootX,RootCells);
  end;
  inc(RootY,RootCells);
 end;
end;

// Walk the mode quadtree (range): split via the shared flag context, a range-coded mode per leaf, expanded.
procedure FWVDecodeModeLeavesRange(var aDecoder:TpvFlexibleWaveletVideo.TMVRangeDecoder;const aMode:PpvInt32Array;const aFineGridX,aFineGridY,aX0,aY0,aCells:TpvInt32);
var Split,Mode,Half,dx,dy:TpvInt32;
begin
 if aCells<>1 then begin
  Split:=aDecoder.DecodeBit(aDecoder.Flag);
 end else begin
  Split:=0;
 end;
 if Split=0 then begin
  Mode:=aDecoder.DecodeMode;
  dy:=0;
  while (dy<aCells) and ((aY0+dy)<aFineGridY) do begin
   dx:=0;
   while (dx<aCells) and ((aX0+dx)<aFineGridX) do begin
    aMode^[((aY0+dy)*aFineGridX)+(aX0+dx)]:=Mode;
    inc(dx);
   end;
   inc(dy);
  end;
 end else begin
  Half:=aCells div 2;
  FWVDecodeModeLeavesRange(aDecoder,aMode,aFineGridX,aFineGridY,aX0,aY0,Half);
  FWVDecodeModeLeavesRange(aDecoder,aMode,aFineGridX,aFineGridY,aX0+Half,aY0,Half);
  FWVDecodeModeLeavesRange(aDecoder,aMode,aFineGridX,aFineGridY,aX0,aY0+Half,Half);
  FWVDecodeModeLeavesRange(aDecoder,aMode,aFineGridX,aFineGridY,aX0+Half,aY0+Half,Half);
 end;
end;

class procedure TpvFlexibleWaveletVideo.DecodeModeQuadtreeRange(var aDecoder:TMVRangeDecoder;const aMode:PpvInt32Array;const aFineGridX,aFineGridY:TpvInt32);
var RootCells,RootX,RootY:TpvInt32;
begin
 RootCells:=MotionRoot div MotionLeaf;
 RootY:=0;
 while RootY<aFineGridY do begin
  RootX:=0;
  while RootX<aFineGridX do begin
   FWVDecodeModeLeavesRange(aDecoder,aMode,aFineGridX,aFineGridY,RootX,RootY,RootCells);
   inc(RootX,RootCells);
  end;
  inc(RootY,RootCells);
 end;
end;

class function TpvFlexibleWaveletVideo.TemporalQuantLevel(const aFrame,aGOPCount,aTemporalLevels:TpvInt32):TpvInt32;
var Lengths:array[0..15] of TpvInt32;
    Count,Len,Level,i,HighBegin:TpvInt32;
begin
 Count:=0;
 Len:=aGOPCount;
 for Level:=0 to aTemporalLevels-1 do begin
  if Len<2 then begin
   break;
  end;
  Lengths[Count]:=Len;
  inc(Count);
  Len:=(Len+1) div 2;
 end;
 if aFrame<Len then begin
  result:=0; // the deepest temporal low band
  exit;
 end;
 for i:=0 to Count-1 do begin
  if (i+1)<Count then begin
   HighBegin:=Lengths[i+1];
  end else begin
   HighBegin:=Len;
  end;
  if (aFrame>=HighBegin) and (aFrame<Lengths[i]) then begin
   result:=Count-i;
   exit;
  end;
 end;
 result:=Count;
end;

class function TpvFlexibleWaveletVideo.TemporalQuantScale(const aLevel:TpvInt32):TpvFloat;
const Beta=0.6;
      Kappa=1.14;
begin
 result:=Power(2.0,Beta*Power(aLevel,Kappa)); // coarser quant for higher temporal frequencies
end;

end.
