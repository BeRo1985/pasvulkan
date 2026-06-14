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
unit PasVulkan.Audio.RPCM;
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

// Decoder for the "RPCM" sub-codec: raw linear PCM with a 12-byte little-endian header (sample_rate, channels, bits),
// the engine-side reader of the C `rpcm_encode_s16` output (FWV container audio_codec = 'RPCM'). bits = +16/+8/+32 for
// unsigned-8 / signed-16 / signed-32 integer PCM and -32 for 32-bit float; all are converted to s16 then pulled as
// float32 (/32768), matching the FWA / QOAL decoders' Decode interface for the video player facade's read callback.

interface

uses SysUtils,
     Classes,
     PasVulkan.Types,
     PasVulkan.Math.Utils;

type EpvAudioRPCM=class(Exception);

     { TpvAudioRPCMDecoder }
     TpvAudioRPCMDecoder=class
      private
       fChannels:TpvInt32;
       fSampleRate:TpvInt32;
       fFrameCount:TpvInt64; // total per-channel sample frames
       fPCM:array of TpvInt16; // decoded interleaved s16 (fFrameCount*fChannels)
       fCursor:TpvInt64;
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

{ TpvAudioRPCMDecoder }

class function TpvAudioRPCMDecoder.ClampS16(const aValue:TpvInt32):TpvInt32;
begin
 if aValue<-32768 then begin
  result:=-32768;
 end else if aValue>32767 then begin
  result:=32767;
 end else begin
  result:=aValue;
 end;
end;

procedure TpvAudioRPCMDecoder.DecodeBlob(const aBlob:PpvUInt8Array;const aSize:TpvSizeUInt);
var Rate,Bits,BytesPerSample,Value:TpvInt32;
    DataBytes,TotalValues,Index,Base:TpvSizeUInt;
    RawU32:TpvUInt32;
    FloatValue:TpvFloat;
 function LoadU32(const aOffset:TpvSizeUInt):TpvUInt32;
 begin
  result:=TpvUInt32(aBlob^[aOffset]) or
          (TpvUInt32(aBlob^[aOffset+1]) shl 8) or
          (TpvUInt32(aBlob^[aOffset+2]) shl 16) or
          (TpvUInt32(aBlob^[aOffset+3]) shl 24);
 end;
begin

 fChannels:=0;
 fSampleRate:=0;
 fFrameCount:=0;
 fPCM:=nil;

 if aSize<12 then begin
  raise EpvAudioRPCM.Create('Not an RPCM blob (too small)');
 end;

 Rate:=TpvInt32(LoadU32(0));
 fChannels:=TpvInt32(LoadU32(4));
 Bits:=TpvInt32(LoadU32(8)); // +16 signed s16, +8 unsigned u8, +32 signed s32, -32 float32
 if (fChannels<1) or (fChannels>8) then begin
  raise EpvAudioRPCM.Create('RPCM channel count out of range');
 end;

 if Bits<0 then begin
  BytesPerSample:=(-Bits) div 8;
 end else begin
  BytesPerSample:=Bits div 8;
 end;
 DataBytes:=aSize-12;
 if (BytesPerSample<1) or ((DataBytes mod TpvSizeUInt(BytesPerSample))<>0) then begin
  raise EpvAudioRPCM.Create('RPCM unsupported sample depth');
 end;

 TotalValues:=DataBytes div TpvSizeUInt(BytesPerSample);
 SetLength(fPCM,TotalValues);
 Base:=12;
 for Index:=0 to TotalValues-1 do begin
  case Bits of
   16:begin
    Value:=TpvInt16(TpvUInt16(aBlob^[Base]) or (TpvUInt16(aBlob^[Base+1]) shl 8));
   end;
   8:begin
    Value:=(TpvInt32(aBlob^[Base])-128) shl 8; // unsigned 8-bit -> centred s16
   end;
   32:begin
    Value:=ClampS16(SARLongint(TpvInt32(LoadU32(Base)),16)); // s32 -> s16
   end;
   -32:begin
    RawU32:=LoadU32(Base);
    Move(RawU32,FloatValue,SizeOf(TpvFloat));
    Value:=ClampS16(Round(FloatValue*32767.0));
   end;
   else begin
    raise EpvAudioRPCM.Create('RPCM unsupported sample depth');
   end;
  end;
  fPCM[Index]:=TpvInt16(Value);
  inc(Base,TpvSizeUInt(BytesPerSample));
 end;

 fSampleRate:=Rate;
 fFrameCount:=TpvInt64(TotalValues div TpvSizeUInt(fChannels));

end;

constructor TpvAudioRPCMDecoder.Create(const aStream:TStream);
var Blob:array of TpvUInt8;
    Size:TpvSizeInt;
begin
 inherited Create;
 fCursor:=0;
 Size:=aStream.Size-aStream.Position;
 if Size<=0 then begin
  raise EpvAudioRPCM.Create('Empty RPCM stream');
 end;
 SetLength(Blob,Size);
 aStream.ReadBuffer(Blob[0],Size);
 DecodeBlob(PpvUInt8Array(@Blob[0]),TpvSizeUInt(Size));
end;

destructor TpvAudioRPCMDecoder.Destroy;
begin
 fPCM:=nil;
 inherited Destroy;
end;

procedure TpvAudioRPCMDecoder.Seek(const aSamplePosition:TpvUInt64);
begin
 if TpvInt64(aSamplePosition)>fFrameCount then begin
  fCursor:=fFrameCount;
 end else begin
  fCursor:=TpvInt64(aSamplePosition);
 end;
end;

function TpvAudioRPCMDecoder.Decode(const aBuffer:Pointer;const aCount:TpvSizeInt):TpvSizeInt;
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
