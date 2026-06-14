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
unit PasVulkan.Audio.OGGVorbis;
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

// A thin pull-decoder over the engine's OGG/Vorbis (Tremor, integer) backend, the engine-side reader of an "OGGV"
// audio blob (FWV container audio_codec = 'OGGV'). It opens the blob via ov_open_callbacks on an in-memory copy and
// exposes the same Decode (interleaved float32, /32768) / Seek / Channels / SampleRate / FrameCount interface as the
// FWA / QOAL / RPCM decoders, so the video player facade can feed any audio sub-codec through one read callback.

interface

uses SysUtils,
     Classes,
     PasVulkan.Types,
     PasVulkan.Audio.OGGVorbisTremor;

type EpvAudioOGGVorbis=class(Exception);

     { TpvAudioOGGVorbisDecoder }
     TpvAudioOGGVorbisDecoder=class
      private
       fData:TMemoryStream; // owns the blob; Tremor reads from it on demand via the callbacks
       fVorbisFile:OggVorbis_File;
       fOpened:boolean;
       fChannels:TpvInt32;
       fSampleRate:TpvInt32;
       fFrameCount:TpvInt64;
       fCursor:TpvInt64;
       fScratch:array of TpvInt16; // ov_read target (interleaved host-endian s16)
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

const SeekSet=0;
      SeekCur=1;
      SeekEnd=2;

function OGGVRead(ptr:pointer;size,nmemb:PasAudioOGGPtrUInt;datasource:pointer):PasAudioOGGPtrUInt; {$ifdef UseExternalOGGVorbisTremorLibrary}cdecl;{$endif}
begin
 result:=TStream(datasource).Read(ptr^,nmemb*size);
end;

function OGGVSeek(datasource:pointer;offset:int64;whence:longint):longint; {$ifdef UseExternalOGGVorbisTremorLibrary}cdecl;{$endif}
begin
 case whence of
  SeekSet:begin
   TStream(datasource).Seek(offset,soFromBeginning);
  end;
  SeekCur:begin
   TStream(datasource).Seek(offset,soFromCurrent);
  end;
  SeekEnd:begin
   TStream(datasource).Seek(offset,soFromEnd);
  end;
 end;
 result:=TStream(datasource).Position;
end;

function OGGVClose(datasource:pointer):longint; {$ifdef UseExternalOGGVorbisTremorLibrary}cdecl;{$endif}
begin
 result:=0;
end;

function OGGVTell(datasource:pointer):longint; {$ifdef UseExternalOGGVorbisTremorLibrary}cdecl;{$endif}
begin
 result:=TStream(datasource).Position;
end;

const OGGVCallbacks:ov_callbacks=(read_func:OGGVRead;seek_func:OGGVSeek;close_func:OGGVClose;tell_func:OGGVTell);

{ TpvAudioOGGVorbisDecoder }

constructor TpvAudioOGGVorbisDecoder.Create(const aStream:TStream);
var Size:TpvSizeInt;
    Info:Pvorbis_info;
begin
 inherited Create;
 fOpened:=false;
 fChannels:=0;
 fSampleRate:=0;
 fFrameCount:=0;
 fCursor:=0;

 Size:=aStream.Size-aStream.Position;
 if Size<=0 then begin
  raise EpvAudioOGGVorbis.Create('Empty OGGV stream');
 end;
 fData:=TMemoryStream.Create;
 fData.CopyFrom(aStream,Size);
 fData.Seek(0,soFromBeginning);

 if ov_open_callbacks(Pointer(fData),@fVorbisFile,nil,0,OGGVCallbacks)<>0 then begin
  raise EpvAudioOGGVorbis.Create('Not a valid OGG/Vorbis stream');
 end;
 fOpened:=true;

 Info:=ov_info(@fVorbisFile,-1);
 if not assigned(Info) then begin
  raise EpvAudioOGGVorbis.Create('OGG/Vorbis info missing');
 end;
 fChannels:=Info^.channels;
 fSampleRate:=Info^.rate;
 fFrameCount:=ov_pcm_total(@fVorbisFile,-1);
 if fChannels<1 then begin
  raise EpvAudioOGGVorbis.Create('OGG/Vorbis channel count invalid');
 end;
end;

destructor TpvAudioOGGVorbisDecoder.Destroy;
begin
 if fOpened then begin
  ov_clear(@fVorbisFile);
  fOpened:=false;
 end;
 FreeAndNil(fData);
 fScratch:=nil;
 inherited Destroy;
end;

procedure TpvAudioOGGVorbisDecoder.Seek(const aSamplePosition:TpvUInt64);
begin
 if fOpened then begin
  ov_pcm_seek(@fVorbisFile,ogg_int64_t(aSamplePosition));
 end;
 if TpvInt64(aSamplePosition)>fFrameCount then begin
  fCursor:=fFrameCount;
 end else begin
  fCursor:=TpvInt64(aSamplePosition);
 end;
end;

function TpvAudioOGGVorbisDecoder.Decode(const aBuffer:Pointer;const aCount:TpvSizeInt):TpvSizeInt;
var Destination:PpvFloatArray;
    BytesNeeded,BytesGot,Returned,SampleCount,Index,Produced:TpvSizeInt;
    BitStream:longint;
begin
 Destination:=PpvFloatArray(aBuffer);
 if (aCount<=0) or (fChannels<1) then begin
  result:=0;
  exit;
 end;
 if length(fScratch)<(aCount*fChannels) then begin
  SetLength(fScratch,aCount*fChannels);
 end;

 // Tremor's ov_read returns one packet's worth of host-endian interleaved s16 per call, so loop until the request is
 // filled or the stream ends.
 BytesNeeded:=(aCount*fChannels)*SizeOf(TpvInt16);
 BytesGot:=0;
 BitStream:=0;
 while BytesGot<BytesNeeded do begin
  Returned:=ov_read(@fVorbisFile,Pointer(@PpvUInt8Array(@fScratch[0])^[BytesGot]),BytesNeeded-BytesGot,@BitStream);
  if Returned<=0 then begin
   break; // end of stream or error
  end;
  inc(BytesGot,Returned);
 end;

 SampleCount:=BytesGot div SizeOf(TpvInt16);
 for Index:=0 to SampleCount-1 do begin
  Destination^[Index]:=fScratch[Index]/32768.0;
 end;
 Produced:=SampleCount div fChannels;
 inc(fCursor,Produced);
 result:=Produced;
end;

end.
