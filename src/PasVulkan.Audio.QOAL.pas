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
unit PasVulkan.Audio.QOAL;
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

// Decoder for the "qoal" sub-codec: a little-endian reimplementation of the QOA audio codec (LMS order-4
// predictor, 20-sample slices grouped into 5120-sample frames, 3-bit residuals), the byte-exact engine-side
// reader of the C `qoal_encode` output (FWV container audio_codec = 'QOAL'). It is decode-only and pure CPU:
// Create reads the whole blob from the caller-owned stream and decodes it to interleaved s16; Decode then
// pulls interleaved float32 (normalized by 1/32768), matching the FWA decoder's pull interface so the video
// player facade can feed any audio sub-codec through one TpvAudioSoundVideo read callback. s16 in, f32 out.

interface

uses SysUtils,
     Classes,
     PasVulkan.Types,
     PasVulkan.Math.Utils;

type EpvAudioQOAL=class(Exception);

     { TpvAudioQOALDecoder }
     TpvAudioQOALDecoder=class
      public
       const QOASliceLength=20;
             QOASlicesPerFrame=256;
             QOAFrameLength=QOASlicesPerFrame*QOASliceLength; // 5120
             QOALMSLength=4;
             QOAMaxChannels=8;
      private
       type TQoaLMS=record
             History:array[0..QOALMSLength-1] of TpvInt32;
             Weights:array[0..QOALMSLength-1] of TpvInt32;
            end;
      private
       fChannels:TpvInt32;
       fSampleRate:TpvInt32;
       fFrameCount:TpvInt64; // total per-channel sample frames
       fPCM:array of TpvInt16; // decoded interleaved s16 (fFrameCount*fChannels)
       fCursor:TpvInt64; // current read frame position (Decode/Seek)
       class function LMSPredict(const aLMS:TQoaLMS):TpvInt32; static;
       class procedure LMSUpdate(var aLMS:TQoaLMS;const aSample,aResidual:TpvInt32); static;
       class function ClampS16(const aValue:TpvInt32):TpvInt32; static;
       procedure DecodeBlob(const aBlob:PpvUInt8Array;const aSize:TpvSizeUInt);
      public
       constructor Create(const aStream:TStream);
       destructor Destroy; override;
       procedure Seek(const aSamplePosition:TpvUInt64);
       function Decode(const aBuffer:Pointer;const aCount:TpvSizeInt):TpvSizeInt; // interleaved f32, returns frames produced
       property Channels:TpvInt32 read fChannels;
       property SampleRate:TpvInt32 read fSampleRate;
       property FrameCount:TpvInt64 read fFrameCount;
     end;

implementation

const QOADequantTable:array[0..15,0..7] of TpvInt32=
       (
        (    1,    -1,    3,    -3,    5,    -5,     7,     -7),
        (    5,    -5,   18,   -18,   32,   -32,    49,    -49),
        (   16,   -16,   53,   -53,   95,   -95,   147,   -147),
        (   34,   -34,  113,  -113,  203,  -203,   315,   -315),
        (   63,   -63,  210,  -210,  378,  -378,   588,   -588),
        (  104,  -104,  345,  -345,  621,  -621,   966,   -966),
        (  158,  -158,  528,  -528,  950,  -950,  1477,  -1477),
        (  228,  -228,  760,  -760, 1368, -1368,  2128,  -2128),
        (  316,  -316, 1053, -1053, 1895, -1895,  2947,  -2947),
        (  422,  -422, 1405, -1405, 2529, -2529,  3934,  -3934),
        (  548,  -548, 1828, -1828, 3290, -3290,  5117,  -5117),
        (  696,  -696, 2320, -2320, 4176, -4176,  6496,  -6496),
        (  868,  -868, 2893, -2893, 5207, -5207,  8099,  -8099),
        ( 1064, -1064, 3548, -3548, 6386, -6386,  9933,  -9933),
        ( 1286, -1286, 4288, -4288, 7718, -7718, 12005, -12005),
        ( 1536, -1536, 5120, -5120, 9216, -9216, 14336, -14336)
       );

{ TpvAudioQOALDecoder }

class function TpvAudioQOALDecoder.LMSPredict(const aLMS:TQoaLMS):TpvInt32;
var Index,Prediction:TpvInt32;
begin
 Prediction:=0;
 for Index:=0 to QOALMSLength-1 do begin
  inc(Prediction,aLMS.Weights[Index]*aLMS.History[Index]);
 end;
 result:=SARLongint(Prediction,13); // C: prediction >> 13 (arithmetic; Object-Pascal shr is logical)
end;

class procedure TpvAudioQOALDecoder.LMSUpdate(var aLMS:TQoaLMS;const aSample,aResidual:TpvInt32);
var Index,Delta:TpvInt32;
begin
 Delta:=SARLongint(aResidual,4);
 for Index:=0 to QOALMSLength-1 do begin
  if aLMS.History[Index]<0 then begin
   dec(aLMS.Weights[Index],Delta);
  end else begin
   inc(aLMS.Weights[Index],Delta);
  end;
 end;
 for Index:=0 to QOALMSLength-2 do begin
  aLMS.History[Index]:=aLMS.History[Index+1];
 end;
 aLMS.History[QOALMSLength-1]:=aSample;
end;

class function TpvAudioQOALDecoder.ClampS16(const aValue:TpvInt32):TpvInt32;
begin
 if aValue<-32768 then begin
  result:=-32768;
 end else if aValue>32767 then begin
  result:=32767;
 end else begin
  result:=aValue;
 end;
end;

procedure TpvAudioQOALDecoder.DecodeBlob(const aBlob:PpvUInt8Array;const aSize:TpvSizeUInt);
var Position:TpvSizeUInt;
    TotalSamples,FileChannels,FileRate,Written:TpvInt32;
    FrameHeader,History,Weights,Slice:TpvUInt64;
    FrameChannels,FrameRate,FrameLength,Slices:TpvInt32;
    Channel,Index,SampleIndex,SampleStep,SliceLength,Base,ScaleFactor,Quantized,Dequantized,Predicted,Reconstructed:TpvInt32;
    LMS:array[0..QOAMaxChannels-1] of TQoaLMS;
 function LoadU32:TpvUInt32;
 begin
  result:=TpvUInt32(aBlob^[Position]) or
          (TpvUInt32(aBlob^[Position+1]) shl 8) or
          (TpvUInt32(aBlob^[Position+2]) shl 16) or
          (TpvUInt32(aBlob^[Position+3]) shl 24);
  inc(Position,4);
 end;
 function LoadU64:TpvUInt64;
 var SubIndex:TpvInt32;
     Value:TpvUInt64;
 begin
  Value:=0;
  for SubIndex:=0 to 7 do begin
   Value:=Value or (TpvUInt64(aBlob^[Position+TpvSizeUInt(SubIndex)]) shl (SubIndex*8));
  end;
  inc(Position,8);
  result:=Value;
 end;
begin

 fChannels:=0;
 fSampleRate:=0;
 fFrameCount:=0;
 fPCM:=nil;

 if aSize<12 then begin
  raise EpvAudioQOAL.Create('Not a QOAL blob (too small)');
 end;
 if (aBlob^[0]<>ord('q')) or (aBlob^[1]<>ord('o')) or (aBlob^[2]<>ord('a')) or (aBlob^[3]<>ord('l')) then begin
  raise EpvAudioQOAL.Create('Not a QOAL blob (bad magic)');
 end;

 Position:=4;
 TotalSamples:=TpvInt32(LoadU32); // total per-channel samples
 Position:=8;

 FileChannels:=0;
 FileRate:=0;
 Written:=0;

 while (Position+8)<=aSize do begin

  FrameHeader:=LoadU64;
  FrameChannels:=TpvInt32((FrameHeader shr 56) and $ff);
  FrameRate:=TpvInt32((FrameHeader shr 32) and $ffffff);
  FrameLength:=TpvInt32((FrameHeader shr 16) and $ffff);
  if (FrameChannels<1) or (FrameChannels>QOAMaxChannels) then begin
   raise EpvAudioQOAL.Create('QOAL frame channel count out of range');
  end;

  if FileChannels=0 then begin
   FileChannels:=FrameChannels;
   FileRate:=FrameRate;
   SetLength(fPCM,TpvSizeInt(TotalSamples)*TpvSizeInt(FileChannels));
  end;

  Slices:=(FrameLength+(QOASliceLength-1)) div QOASliceLength;
  if (Position+TpvSizeUInt((16*FrameChannels)+((Slices*FrameChannels)*8)))>aSize then begin
   raise EpvAudioQOAL.Create('QOAL frame truncated');
  end;

  // Per-channel LMS predictor state (4 history + 4 weights, each as packed s16 in two big-endian-within-u64 words)
  for Channel:=0 to FrameChannels-1 do begin
   History:=LoadU64;
   Weights:=LoadU64;
   for Index:=0 to QOALMSLength-1 do begin
    LMS[Channel].History[Index]:=TpvInt16(History shr 48);
    History:=History shl 16;
    LMS[Channel].Weights[Index]:=TpvInt16(Weights shr 48);
    Weights:=Weights shl 16;
   end;
  end;

  // Per-channel slices: 4-bit scalefactor + 20 x 3-bit quantized residuals, MSB-first in the u64
  for Channel:=0 to FrameChannels-1 do begin
   SampleIndex:=0;
   while SampleIndex<FrameLength do begin
    Slice:=LoadU64;
    ScaleFactor:=TpvInt32((Slice shr 60) and $f);
    SliceLength:=QOASliceLength;
    if SliceLength>(FrameLength-SampleIndex) then begin
     SliceLength:=FrameLength-SampleIndex;
    end;
    Base:=((Written+SampleIndex)*FrameChannels)+Channel;
    Slice:=Slice shl 4; // drop the 4 scalefactor bits; residuals now in the top bits
    for SampleStep:=0 to SliceLength-1 do begin
     Predicted:=LMSPredict(LMS[Channel]);
     Quantized:=TpvInt32((Slice shr 61) and $7);
     Dequantized:=QOADequantTable[ScaleFactor,Quantized];
     Reconstructed:=ClampS16(Predicted+Dequantized);
     fPCM[Base+(SampleStep*FrameChannels)]:=TpvInt16(Reconstructed);
     Slice:=Slice shl 3;
     LMSUpdate(LMS[Channel],Reconstructed,Dequantized);
    end;
    inc(SampleIndex,QOASliceLength);
   end;
  end;

  inc(Written,FrameLength);
 end;

 fChannels:=FileChannels;
 fSampleRate:=FileRate;
 fFrameCount:=TotalSamples;
 if fChannels<1 then begin
  raise EpvAudioQOAL.Create('Empty QOAL blob');
 end;

end;

constructor TpvAudioQOALDecoder.Create(const aStream:TStream);
var Blob:array of TpvUInt8;
    Size:TpvSizeInt;
begin
 inherited Create;
 fCursor:=0;
 Size:=aStream.Size-aStream.Position;
 if Size<=0 then begin
  raise EpvAudioQOAL.Create('Empty QOAL stream');
 end;
 SetLength(Blob,Size);
 aStream.ReadBuffer(Blob[0],Size);
 DecodeBlob(PpvUInt8Array(@Blob[0]),TpvSizeUInt(Size));
end;

destructor TpvAudioQOALDecoder.Destroy;
begin
 fPCM:=nil;
 inherited Destroy;
end;

procedure TpvAudioQOALDecoder.Seek(const aSamplePosition:TpvUInt64);
begin
 if TpvInt64(aSamplePosition)>fFrameCount then begin
  fCursor:=fFrameCount;
 end else begin
  fCursor:=TpvInt64(aSamplePosition);
 end;
end;

function TpvAudioQOALDecoder.Decode(const aBuffer:Pointer;const aCount:TpvSizeInt):TpvSizeInt;
var Destination:PpvFloatArray;
    Frame,Channel:TpvSizeInt;
    Produced:TpvSizeInt;
begin
 Destination:=PpvFloatArray(aBuffer);
 Produced:=aCount;
 if (fCursor+Produced)>fFrameCount then begin
  Produced:=fFrameCount-fCursor;
 end;
 if Produced<0 then begin
  Produced:=0;
 end;
 for Frame:=0 to Produced-1 do begin
  for Channel:=0 to fChannels-1 do begin
   Destination^[(Frame*fChannels)+Channel]:=fPCM[((fCursor+Frame)*fChannels)+Channel]/32768.0;
  end;
 end;
 inc(fCursor,Produced);
 result:=Produced;
end;

end.
