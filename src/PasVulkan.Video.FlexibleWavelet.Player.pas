unit PasVulkan.Video.FlexibleWavelet.Player;
{$i PasVulkan.inc}

// Poll-based playback facade over the Flexible Wavelet video decoders. The engine drives it with a clock (e.g. the
// FWA audio clock for A/V sync): DecodeTime(t) advances the CPU side on the Update thread (parse + decompress + MV/mode
// decode + host upload, one frame toward the time's target), Decode(cmd) records the GPU side into the caller's command
// buffer on the Draw thread, then the decoded frame is available as OutputImage / blitted into a present target. The
// facade owns the backend choice: a container carrying an H.264 stream decodes via the dedicated VK-H.264 hardware path
// where the GPU supports it (Stage F3, PasVulkan.Video.H264.Decoder), otherwise via the self-contained wavelet GPU path
// (the lossless / HDR / 3D-DWT / downscalable fallback). For now only the wavelet backend is wired; an H.264-only request
// raises, an auto request with an H.264 stream logs and falls back to wavelet.

interface

uses SysUtils,
     Classes,
     PasMP,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Framework,
     PasVulkan.Audio,
     PasVulkan.Audio.FlexibleWavelet.Decoder,
     PasVulkan.Audio.QOAL,
     PasVulkan.Audio.RPCM,
     PasVulkan.Audio.OGGVorbis,
     PasVulkan.Video.FlexibleWavelet,
     PasVulkan.Video.FlexibleWavelet.Decoder,
     PasVulkan.Video.H264.Decoder;

type EpvFlexibleWaveletVideoPlayer=class(EpvFlexibleWaveletVideo);

     { TpvFlexibleWaveletVideoPlayer }
     TpvFlexibleWaveletVideoPlayer=class(TObject)
      public
       type TDecoderChoice=
             (
              Auto,         // H.264 where HW-decodable, else wavelet
              ForceH264,    // require the H.264 hardware path (raises if unavailable)
              ForceWavelet  // always the wavelet GPU-compute path
             );
            TAudioKind=
             (
              None,
              FWAC, // wavelet audio  (TpvFlexibleWaveletAudioDecoder)
              QOAL, // little-endian QOA  (TpvAudioQOALDecoder)
              RPCM, // raw PCM  (TpvAudioRPCMDecoder)
              OGGV  // OGG/Vorbis  (TpvAudioOGGVorbisDecoder)
             );
      private
       fStream:TStream;
       fDevice:TpvVulkanDevice;
       fDecoder:TpvFlexibleWaveletVideoDecoder; // the wavelet backend (nil when the H.264 backend is active)
       fH264:TpvVideoH264Decoder; // the VK-H.264 HW-decode backend (Stage F3); nil unless fUsingH264
       // audio (A/V sync): one container audio sub-codec decoder feeds a TpvAudioSoundVideo whose PlaybackTime is the clock
       fAudio:TpvAudio;
       fAudioKind:TAudioKind;
       fAudioStream:TpvAudioSoundVideo;
       fAudioBlobStream:TMemoryStream; // owns the audio sub-blob; kept alive >= the decoder (FWA reads it on demand)
       fAudioFWADecoder:TpvFlexibleWaveletAudioDecoder;
       fAudioQOALDecoder:TpvAudioQOALDecoder;
       fAudioRPCMDecoder:TpvAudioRPCMDecoder;
       fAudioOGGVDecoder:TpvAudioOGGVorbisDecoder;
       fAudioChannels:TpvInt32;
       fAudioSampleRate:TpvInt32;
       fHeader:TpvFlexibleWaveletVideo.THeader;
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fFrameCount:TpvInt64;
       fFpsNum:TpvUInt32;
       fFpsDen:TpvUInt32;
       fFrameRate:TpvDouble;
       fDuration:TpvDouble;
       fHasH264Stream:boolean;
       fUsingH264:boolean;
       fTargetIndex:TpvInt32; // display frame index the current time maps to
       fPreparedIndex:TpvInt32; // index PrepareFrame staged for the next Decode (-1 = none)
       fLastDecodedIndex:TpvInt32; // last index RecordFrame produced (-1 = none)
       fPendingAudioSeekFrame:TpvInt64; // queued audio-source seek; applied by the audio thread in AudioReadCallback (-1 = none)
       fAudioFinished:boolean; // set on the audio thread when the audio sub-codec decoder reaches EOF (source exhausted)
{$ifdef VkVideo}
       procedure TryCreateH264Backend; // build the VK-H.264 backend from the container's H.264 sub-blob; on success sets fUsingH264
{$endif}
       function GetOutputImage:TpvVulkanImage;
       function GetOutputImageView:TpvVulkanImageView;
       function GetOutputFormat:TVkFormat;
       function GetIsHDR:boolean;
       function GetHasAlpha:boolean;
       function GetAlphaPremultiplied:boolean;
       function GetHasAudio:boolean;
       function GetAudioFinished:boolean; // True when there is no audio, or the audio decoder has reached EOF
       function GetPlaybackFinished:boolean; // True only when the LAST video frame has been decoded AND the audio has finished
       function AudioReadCallback(const aFloatBuffer:Pointer;const aFrameCount:TpvInt32):TpvInt32; // TpvAudioSoundVideoReadCallback
      public
       constructor Create(const aStream:TStream;const aDevice:TpvVulkanDevice;const aDecoderChoice:TDecoderChoice=TDecoderChoice.Auto;const aPreferSCRGBForHDR:boolean=false);
       destructor Destroy; override;
       // map a presentation time to a display-order frame index (clamped to the stream)
       function TimeToFrameIndex(const aTimeInSeconds:TpvDouble):TpvInt32;
       // CPU advance (Update thread): stage the next frame toward the time's target; True if a frame is staged for Decode.
       // Idempotent across repeated calls before a Decode (a fixed-timestep loop may call Update several times per Draw).
       function DecodeTime(const aTimeInSeconds:TpvDouble):boolean;
       // GPU record (Draw thread): record the staged frame's decode into the caller command buffer; True if it recorded one
       function Decode(const aCommandBuffer:TpvVulkanCommandBuffer):boolean;
       // Restart playback from the beginning by SEEKING (reuses the player + its GPU resources; no free/recreate).
       procedure Restart;
       // blit the last decoded frame (OutputImage, left in TRANSFER_SRC_OPTIMAL) into a present target image
       procedure BlitLastDecodedFrame(const aCommandBuffer:TpvVulkanCommandBuffer;const aTargetImage:TpvVulkanImage;const aTargetWidth,aTargetHeight:TpvInt32;const aTargetOldLayout,aTargetNewLayout:TVkImageLayout;const aLetterbox:boolean=false);
       // audio: open the container's audio sub-codec into a TpvAudioSoundVideo on the given engine audio system (A/V sync)
       procedure OpenAudio(const aAudio:TpvAudio);
       procedure StartAudio; // begin audio playback (anchors the master clock)
       procedure PauseAudio;
       procedure ResumeAudio;
       procedure SeekAudio(const aTimeInSeconds:TpvDouble);
       procedure CloseAudio;
       function MasterClockSeconds:TpvDouble; // the audio stream's PlaybackTime (only meaningful when HasAudio)
       property HasAudio:boolean read GetHasAudio;
       property AudioFinished:boolean read GetAudioFinished; // no audio, or the audio reached EOF (the audible tail may lag a mixer buffer)
       property PlaybackFinished:boolean read GetPlaybackFinished; // BOTH the video (last frame decoded) AND the audio are done — a game polls this to dismiss the cutscene
       property AudioChannels:TpvInt32 read fAudioChannels;
       property AudioSampleRate:TpvInt32 read fAudioSampleRate;
       property Width:TpvInt32 read fWidth;
       property Height:TpvInt32 read fHeight;
       property FrameCount:TpvInt64 read fFrameCount;
       property FrameRate:TpvDouble read fFrameRate;
       property Duration:TpvDouble read fDuration;
       property CurrentFrameIndex:TpvInt32 read fLastDecodedIndex;
       property HasH264Stream:boolean read fHasH264Stream;
       property UsingH264:boolean read fUsingH264;
       property OutputImage:TpvVulkanImage read GetOutputImage;
       property OutputImageView:TpvVulkanImageView read GetOutputImageView;
       property OutputFormat:TVkFormat read GetOutputFormat;
       property IsHDR:boolean read GetIsHDR;
       property HasAlpha:boolean read GetHasAlpha; // the decoded OutputImage's A channel carries a real alpha plane
       property AlphaPremultiplied:boolean read GetAlphaPremultiplied; // the decoded RGB is premultiplied by alpha
     end;

implementation

{ TpvFlexibleWaveletVideoPlayer }

constructor TpvFlexibleWaveletVideoPlayer.Create(const aStream:TStream;const aDevice:TpvVulkanDevice;const aDecoderChoice:TDecoderChoice;const aPreferSCRGBForHDR:boolean);
begin
 inherited Create;

 fStream:=aStream;
 fDevice:=aDevice;
 fDecoder:=nil;
 fH264:=nil;
 fTargetIndex:=-1;
 fPreparedIndex:=-1;
 fLastDecodedIndex:=-1;
 fPendingAudioSeekFrame:=-1;
 fAudioFinished:=false;
 fAudio:=nil;
 fAudioKind:=TAudioKind.None;
 fAudioStream:=nil;
 fAudioBlobStream:=nil;
 fAudioFWADecoder:=nil;
 fAudioQOALDecoder:=nil;
 fAudioRPCMDecoder:=nil;
 fAudioOGGVDecoder:=nil;
 fAudioChannels:=0;
 fAudioSampleRate:=0;

 // peek the 126-byte container header for the time base + the backend decision, then rewind for the decoder
 fStream.Seek(0,soBeginning);
 fStream.ReadBuffer(fHeader,SizeOf(fHeader));
 if (CompareByte(fHeader.Magic[0],TpvFlexibleWaveletVideo.Magic[0],4)<>0) or (fHeader.Version<>TpvFlexibleWaveletVideo.FormatVersion) then begin
  raise EpvFlexibleWaveletVideoPlayer.Create('Not a FWVC stream');
 end;

 fWidth:=TpvInt32(fHeader.Width);
 fHeight:=TpvInt32(fHeader.Height);
 fFrameCount:=TpvInt64(fHeader.FrameCount);
 fFpsNum:=fHeader.FpsNum;
 fFpsDen:=fHeader.FpsDen;
 if fFpsNum=0 then begin
  fFpsNum:=30;
 end;
 if fFpsDen=0 then begin
  fFpsDen:=1;
 end;
 fFrameRate:=fFpsNum/fFpsDen;
 if fFrameRate>0.0 then begin
  fDuration:=fFrameCount/fFrameRate;
 end else begin
  fDuration:=0.0;
 end;

 // backend decision (mirrors the C fwvplay: use_h264 = (choice<>force-wavelet) && h264_size>0 && gpu_h264)
 fHasH264Stream:=fHeader.H264Size>0;
 fUsingH264:=false;
{$ifdef VkVideo}
 if (aDecoderChoice<>TDecoderChoice.ForceWavelet) and fHasH264Stream and assigned(fDevice.VideoDecodeQueue) then begin
  // copy the H.264 Annex-B sub-blob out of the container; TpvVideoH264Decoder copies it again internally, so a local
  // stream is enough (unlike the audio decoders, which read on demand and need their blob kept alive).
  TryCreateH264Backend; // sets fUsingH264 + fH264 + the H.264 width/height/frame-count on success
 end;
 if (not fUsingH264) and (aDecoderChoice=TDecoderChoice.ForceH264) then begin
  raise EpvFlexibleWaveletVideoPlayer.Create('H.264 hardware decode requested but unavailable (no stream, no VK video-decode queue, or session creation failed)');
 end;
{$else}
 if aDecoderChoice=TDecoderChoice.ForceH264 then begin
  raise EpvFlexibleWaveletVideoPlayer.Create('H.264 hardware decode requested but the engine was built without VkVideo');
 end;
{$endif}

 // the wavelet backend is the default + the fallback; only built when the H.264 backend is not active
 if not fUsingH264 then begin
  fStream.Seek(0,soBeginning);
  fDecoder:=TpvFlexibleWaveletVideoDecoder.Create(fStream,fDevice,aPreferSCRGBForHDR,1); // submit mode B: whole decode-ahead into ONE caller CB
 end;

end;

{$ifdef VkVideo}
procedure TpvFlexibleWaveletVideoPlayer.TryCreateH264Backend;
var BlobStream:TMemoryStream;
begin
 BlobStream:=TMemoryStream.Create;
 try
  fStream.Seek(TpvInt64(fHeader.H264Offset),soBeginning);
  BlobStream.CopyFrom(fStream,TpvInt64(fHeader.H264Size));
  BlobStream.Seek(0,soBeginning);
  try
   fH264:=TpvVideoH264Decoder.Create(BlobStream,fDevice); // parses + creates the VkVideoSession (raises if unsupported)
   fUsingH264:=true;
   // the H.264 stream is full-resolution; the container header width/height may be the wavelet's down-scaled size
   fWidth:=fH264.Width;
   fHeight:=fH264.Height;
   fFrameCount:=fH264.FrameCount;
   if fFrameRate>0.0 then begin
    fDuration:=fFrameCount/fFrameRate;
   end;
  except
   on e:Exception do begin
    // auto-fallback to wavelet: the GPU advertised a video-decode queue but the actual session/profile was rejected
    FreeAndNil(fH264);
    fUsingH264:=false;
   end;
  end;
 finally
  BlobStream.Free;
 end;
end;
{$endif}

destructor TpvFlexibleWaveletVideoPlayer.Destroy;
begin
 CloseAudio;
 FreeAndNil(fDecoder);
{$ifdef VkVideo}
 FreeAndNil(fH264);
{$endif}
 inherited Destroy;
end;

function TpvFlexibleWaveletVideoPlayer.GetHasAudio:boolean;
begin
 result:=(fAudioKind<>TAudioKind.None) and assigned(fAudioStream);
end;

function TpvFlexibleWaveletVideoPlayer.GetAudioFinished:boolean;
begin
 // No audio -> nothing to wait for; otherwise the audio thread sets fAudioFinished when the source hits EOF.
 result:=(not GetHasAudio) or fAudioFinished;
end;

function TpvFlexibleWaveletVideoPlayer.GetPlaybackFinished:boolean;
begin
 // Finished only when the LAST video frame has been decoded AND the audio has finished — so a game holding a cutscene
 // on this player keeps it up until BOTH streams are done, instead of cutting off whichever finishes first.
 result:=((fFrameCount>0) and (fLastDecodedIndex>=(fFrameCount-1))) and GetAudioFinished;
end;

function TpvFlexibleWaveletVideoPlayer.AudioReadCallback(const aFloatBuffer:Pointer;const aFrameCount:TpvInt32):TpvInt32;
var PendingSeekFrame:TpvInt64;
begin
 // called from the audio thread (TpvAudioSoundVideo.GetNextInBuffer, under the mixer critical section): pull interleaved
 // f32 from the active sub-codec decoder.
 // A queued seek (SeekAudio) is applied HERE, on this thread, rather than on the caller thread, so the decoder is only
 // ever touched from one thread - the source seek can never run concurrently with this same thread's read on the same
 // source. This is the deadlock-free alternative to locking the mixer's critical section across the seek.
 PendingSeekFrame:=TPasMPInterlocked.Exchange(fPendingAudioSeekFrame,TpvInt64(-1));
 if PendingSeekFrame>=0 then begin
  case fAudioKind of
   TAudioKind.FWAC:begin
    fAudioFWADecoder.Seek(TpvUInt64(PendingSeekFrame));
   end;
   TAudioKind.QOAL:begin
    fAudioQOALDecoder.Seek(TpvUInt64(PendingSeekFrame));
   end;
   TAudioKind.RPCM:begin
    fAudioRPCMDecoder.Seek(TpvUInt64(PendingSeekFrame));
   end;
   TAudioKind.OGGV:begin
    fAudioOGGVDecoder.Seek(TpvUInt64(PendingSeekFrame));
   end;
   else begin
   end;
  end;
  fAudioFinished:=false; // a queued seek re-armed the audio source — it is no longer at EOF
 end;
 case fAudioKind of
  TAudioKind.FWAC:begin
   result:=fAudioFWADecoder.Decode(aFloatBuffer,aFrameCount);
  end;
  TAudioKind.QOAL:begin
   result:=fAudioQOALDecoder.Decode(aFloatBuffer,aFrameCount);
  end;
  TAudioKind.RPCM:begin
   result:=fAudioRPCMDecoder.Decode(aFloatBuffer,aFrameCount);
  end;
  TAudioKind.OGGV:begin
   result:=fAudioOGGVDecoder.Decode(aFloatBuffer,aFrameCount);
  end;
  else begin
   result:=0;
  end;
 end;
 // The sub-codec decoders fill the buffer completely until EOF, so a short read marks the audio source exhausted.
 // (The audible tail still in the mixer buffer lags by up to one buffer — PlaybackFinished also gates on the video.)
 if result<aFrameCount then begin
  fAudioFinished:=true;
 end;
end;

procedure TpvFlexibleWaveletVideoPlayer.OpenAudio(const aAudio:TpvAudio);
 function CodecIs(const a,b,c,d:AnsiChar):boolean;
 begin
  result:=(fHeader.AudioCodec[0]=a) and (fHeader.AudioCodec[1]=b) and (fHeader.AudioCodec[2]=c) and (fHeader.AudioCodec[3]=d);
 end;
begin

 CloseAudio;
 if (fHeader.AudioSize=0) or not assigned(aAudio) then begin
  exit; // no audio track (or no audio engine)
 end;
 fAudio:=aAudio;

 // copy the audio sub-blob out of the container stream (kept alive >= the decoder)
 fAudioBlobStream:=TMemoryStream.Create;
 fStream.Seek(TpvInt64(fHeader.AudioOffset),soBeginning);
 fAudioBlobStream.CopyFrom(fStream,TpvInt64(fHeader.AudioSize));
 fAudioBlobStream.Seek(0,soBeginning);

 if CodecIs('F','W','A','C') then begin
  fAudioFWADecoder:=TpvFlexibleWaveletAudioDecoder.Create(fAudioBlobStream);
  fAudioKind:=TAudioKind.FWAC;
  fAudioChannels:=fAudioFWADecoder.Channels;
  fAudioSampleRate:=fAudioFWADecoder.SampleRate;
 end else if CodecIs('Q','O','A','L') then begin
  fAudioQOALDecoder:=TpvAudioQOALDecoder.Create(fAudioBlobStream);
  fAudioKind:=TAudioKind.QOAL;
  fAudioChannels:=fAudioQOALDecoder.Channels;
  fAudioSampleRate:=fAudioQOALDecoder.SampleRate;
 end else if CodecIs('R','P','C','M') then begin
  fAudioRPCMDecoder:=TpvAudioRPCMDecoder.Create(fAudioBlobStream);
  fAudioKind:=TAudioKind.RPCM;
  fAudioChannels:=fAudioRPCMDecoder.Channels;
  fAudioSampleRate:=fAudioRPCMDecoder.SampleRate;
 end else if CodecIs('O','G','G','V') then begin
  fAudioOGGVDecoder:=TpvAudioOGGVorbisDecoder.Create(fAudioBlobStream);
  fAudioKind:=TAudioKind.OGGV;
  fAudioChannels:=fAudioOGGVDecoder.Channels;
  fAudioSampleRate:=fAudioOGGVDecoder.SampleRate;
 end else begin
  // unknown audio sub-codec -> no audio
  FreeAndNil(fAudioBlobStream);
  fAudio:=nil;
  exit;
 end;

 fAudioStream:=TpvAudioSoundVideo.Create(fAudio,fAudio.Videos,fAudioChannels,fAudioSampleRate,AudioReadCallback);

end;

procedure TpvFlexibleWaveletVideoPlayer.StartAudio;
begin
 if assigned(fAudioStream) then begin
  fAudioStream.Play(1.0,0.0,1.0,false);
 end;
end;

procedure TpvFlexibleWaveletVideoPlayer.PauseAudio;
begin
 if assigned(fAudioStream) then begin
  fAudioStream.Pause;
 end;
end;

procedure TpvFlexibleWaveletVideoPlayer.ResumeAudio;
begin
 if assigned(fAudioStream) then begin
  fAudioStream.Resume;
 end;
end;

procedure TpvFlexibleWaveletVideoPlayer.SeekAudio(const aTimeInSeconds:TpvDouble);
var Frame:TpvInt64;
begin

 if not GetHasAudio then begin
  exit;
 end;

 if aTimeInSeconds<=0.0 then begin
  Frame:=0;
 end else begin
  Frame:=Round(aTimeInSeconds*fAudioSampleRate);
 end;

 // Hand the source seek to the audio thread instead of doing it here. The decoder is then only ever touched from
 // AudioReadCallback (which the mixer calls under its own critical section), so the seek can never run concurrently
 // with the mixer's ov_read on the same source - which is exactly what hung/raced when seeking from this thread.
 // Lock-free: a single producer (this thread) hands one frame to a single consumer (the audio thread).
 TPasMPInterlocked.Exchange(fPendingAudioSeekFrame,Frame);

 // Force a fresh GetNextInBuffer (so the queued seek is picked up promptly), flush the resampler and re-anchor the
 // per-stream clock to this time. ResetForSeek only briefly enters the mixer lock to write fields (no long work under
 // it), and also clears a latched end-of-stream Stopped state so the read callback runs again.
 fAudioStream.ResetForSeek(aTimeInSeconds);

end;

procedure TpvFlexibleWaveletVideoPlayer.CloseAudio;
begin

 FreeAndNil(fAudioStream); // removes itself from fAudio.Videos under the engine lock

 FreeAndNil(fAudioFWADecoder);
 FreeAndNil(fAudioQOALDecoder);
 FreeAndNil(fAudioRPCMDecoder);
 FreeAndNil(fAudioOGGVDecoder);
 FreeAndNil(fAudioBlobStream);

 fAudioKind:=TAudioKind.None;
 fAudio:=nil;
 fAudioChannels:=0;
 fAudioSampleRate:=0;

end;

function TpvFlexibleWaveletVideoPlayer.MasterClockSeconds:TpvDouble;
begin
 if GetHasAudio then begin
  result:=fAudioStream.PlaybackTime;
 end else begin
  result:=0.0;
 end;
end;

function TpvFlexibleWaveletVideoPlayer.GetOutputImage:TpvVulkanImage;
begin

{$ifdef VkVideo}
 if fUsingH264 then begin
  result:=fH264.OutputImage;
  exit;
 end;
{$endif}

 result:=fDecoder.OutputImage;

end;

function TpvFlexibleWaveletVideoPlayer.GetOutputImageView:TpvVulkanImageView;
begin

{$ifdef VkVideo}
 if fUsingH264 then begin
  result:=fH264.OutputImageView;
  exit;
 end;
{$endif}

 result:=fDecoder.OutputImageView;

end;

function TpvFlexibleWaveletVideoPlayer.GetOutputFormat:TVkFormat;
begin
 if fUsingH264 then begin
  result:=VK_FORMAT_R8G8B8A8_SRGB; // H.264 display image is sRGB -> samples/blits to linear (sRGB swapchain + in-game correct)
  exit;
 end;
 result:=fDecoder.OutputFormat;
end;

function TpvFlexibleWaveletVideoPlayer.GetIsHDR:boolean;
begin

 if fUsingH264 then begin
  result:=false; // the H.264 backend is 8-bit SDR
  exit;
 end;

 result:=fDecoder.IsHDR;
end;

function TpvFlexibleWaveletVideoPlayer.GetHasAlpha:boolean;
begin
 if fUsingH264 then begin
  result:=false; // the H.264 backend has no alpha channel
  exit;
 end;
 result:=fDecoder.HasAlpha;
end;

function TpvFlexibleWaveletVideoPlayer.GetAlphaPremultiplied:boolean;
begin
 if fUsingH264 then begin
  result:=false;
  exit;
 end;
 result:=fDecoder.AlphaPremultiplied;
end;

function TpvFlexibleWaveletVideoPlayer.TimeToFrameIndex(const aTimeInSeconds:TpvDouble):TpvInt32;
var FrameNumber:TpvInt64;
begin

 if aTimeInSeconds<=0.0 then begin
  result:=0;
  exit;
 end;

 // display frame = floor(t * fps) = floor(t * FpsNum / FpsDen)
 FrameNumber:=TpvInt64(Trunc((aTimeInSeconds*fFpsNum)/fFpsDen));
 if FrameNumber<0 then begin
  FrameNumber:=0;
 end else if FrameNumber>=fFrameCount then begin
  FrameNumber:=fFrameCount-1;
 end;

 result:=TpvInt32(FrameNumber);
end;

function TpvFlexibleWaveletVideoPlayer.DecodeTime(const aTimeInSeconds:TpvDouble):boolean;
begin
 fTargetIndex:=TimeToFrameIndex(aTimeInSeconds);

{$ifdef VkVideo}
 if fUsingH264 then begin
  // H.264 path: CPU-only on the Update thread (just note the target). The actual HW decode is self-submitting and
  // runs entirely in Decode() on the Draw thread, so Update+Draw stay parallel-safe. EnsureDisplayFrame absorbs any
  // forward jump (or a backward seek) in one Decode() call, so no per-frame staging is needed here.
  result:=fTargetIndex<>fLastDecodedIndex;
  exit;
 end;
{$endif}

 // backward seek (wavelet path): the decode chain is forward-only (intra / P / B references), so to land on an earlier
 // display frame we reset the decoder to the start of the stream and re-decode forward to the target — one frame per
 // tick, exactly like a forward catch-up. (The H.264 backend handles a backward target inside EnsureDisplayFrame above,
 // so this only applies here.) Audio re-anchoring is the caller's master-clock / SeekAudio concern, not this video poll.
 if fTargetIndex<fLastDecodedIndex then begin
  if assigned(fDecoder) then begin
   fDecoder.ResetForReplay;
  end;
  fLastDecodedIndex:=-1;
  fPreparedIndex:=-1;
 end;

 // already at (or past) the target -> nothing new to decode this tick
 if fLastDecodedIndex>=fTargetIndex then begin
  result:=false;
  exit;
 end;

 // already staged this exact frame (a previous DecodeTime call this tick) -> idempotent, don't re-prepare
 if fPreparedIndex=(fLastDecodedIndex+1) then begin
  result:=true;
  exit;
 end;

 // stage exactly one display frame toward the target (keeps the intra/P reference chain; the B-frame decode-ahead is
 // incremental, so stepping POC by POC only uploads newly-needed coding frames). A caller behind by several frames
 // calls DecodeTime/Decode again to catch up.
 fPreparedIndex:=fLastDecodedIndex+1;
 fDecoder.PrepareFrame(fPreparedIndex);
 result:=true;
end;

function TpvFlexibleWaveletVideoPlayer.Decode(const aCommandBuffer:TpvVulkanCommandBuffer):boolean;
begin

 {$ifdef VkVideo}
 if fUsingH264 then begin

  // GPU side (Draw thread): the H.264 decode is fully self-submitting (own video + universal queue submits), so the
  // caller command buffer is only used later by BlitLastDecodedFrame. EnsureDisplayFrame brings the output to fTargetIndex.
  if (fTargetIndex<0) or (fTargetIndex=fLastDecodedIndex) then begin
   result:=false;
   exit;
  end;

  fH264.EnsureDisplayFrame(fTargetIndex);
  fLastDecodedIndex:=fTargetIndex;
  result:=true;
  exit;

 end;

{$endif}

 if fPreparedIndex<0 then begin
  result:=false;
  exit;
 end;
 fDecoder.RecordFrame(aCommandBuffer);
 fLastDecodedIndex:=fPreparedIndex;
 fPreparedIndex:=-1;
 result:=true;
end;

procedure TpvFlexibleWaveletVideoPlayer.Restart;
begin
 // Seek back to the start without recreating the decoder/GPU resources: reset the poll-API cursors + the decoder's
 // reference bookkeeping, then re-anchor the audio clock. The H.264 backend resets itself on the next Decode (its
 // EnsureDisplayFrame sees the target is behind the current display index and replays from the IDR).
 fLastDecodedIndex:=-1;
 fPreparedIndex:=-1;
 fTargetIndex:=-1;
 fAudioFinished:=false; // replay re-arms the audio source (the queued seek below also clears it on the audio thread)
 if assigned(fDecoder) then begin
  fDecoder.ResetForReplay;
 end;
 SeekAudio(0.0); // seek the audio decoder back + flush + (atomically, under the audio lock) resume the stream from 0
end;

procedure TpvFlexibleWaveletVideoPlayer.BlitLastDecodedFrame(const aCommandBuffer:TpvVulkanCommandBuffer;const aTargetImage:TpvVulkanImage;const aTargetWidth,aTargetHeight:TpvInt32;const aTargetOldLayout,aTargetNewLayout:TVkImageLayout;const aLetterbox:boolean);
var Blit:TVkImageBlit;
    Barrier:TVkImageMemoryBarrier;
    SourceLayout:TVkImageLayout;
    ClearColor:TVkClearColorValue;
    ClearRange:TVkImageSubresourceRange;
    MemBarrier:TVkMemoryBarrier;
    DstX,DstY,DstW,DstH:TpvInt32;

 procedure TransitionTarget(const aOldLayout,aNewLayout:TVkImageLayout;const aSrcAccess,aDstAccess:TVkAccessFlags;const aSrcStage,aDstStage:TVkPipelineStageFlags);
 begin
  FillChar(Barrier,SizeOf(Barrier),#0);
  Barrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
  Barrier.srcAccessMask:=aSrcAccess;
  Barrier.dstAccessMask:=aDstAccess;
  Barrier.oldLayout:=aOldLayout;
  Barrier.newLayout:=aNewLayout;
  Barrier.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  Barrier.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  Barrier.image:=aTargetImage.Handle;
  Barrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
  Barrier.subresourceRange.levelCount:=1;
  Barrier.subresourceRange.layerCount:=1;
  aCommandBuffer.CmdPipelineBarrier(aSrcStage,aDstStage,0,0,nil,0,nil,1,@Barrier);
 end;

begin

 // both backends leave OutputImage as a single persistent image in TRANSFER_SRC_OPTIMAL (the H.264 decoder copies its
 // current rotating pool slot into a stable display image), so the present path is identical for both
 SourceLayout:=VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;

 // aspect-fit + centre the decoded frame in the target when letterboxing (else stretch to fill the whole target)
 if (((aLetterbox and (fWidth>0)) and (fHeight>0)) and (aTargetWidth>0)) and (aTargetHeight>0) then begin
  if (aTargetWidth*fHeight)>(aTargetHeight*fWidth) then begin // target wider than the video -> pillarbox
   DstH:=aTargetHeight;
   DstW:=Round((aTargetHeight*fWidth)/fHeight);
  end else begin // target taller (or equal) -> letterbox
   DstW:=aTargetWidth;
   DstH:=Round((aTargetWidth*fHeight)/fWidth);
  end;
  DstX:=(aTargetWidth-DstW) div 2;
  DstY:=(aTargetHeight-DstH) div 2;
 end else begin
  DstX:=0;
  DstY:=0;
  DstW:=aTargetWidth;
  DstH:=aTargetHeight;
 end;

 // bring the target to TRANSFER_DST
 TransitionTarget(aTargetOldLayout,VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                  0,TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                  TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT));

 // letterbox: clear the whole target to black so the bars are black, then a barrier before the centred blit
 if (DstX>0) or (DstY>0) then begin
  FillChar(ClearColor,SizeOf(ClearColor),#0);
  ClearColor.float32[3]:=1.0;
  FillChar(ClearRange,SizeOf(ClearRange),#0);
  ClearRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
  ClearRange.levelCount:=1;
  ClearRange.layerCount:=1;
  aCommandBuffer.CmdClearColorImage(aTargetImage.Handle,VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,@ClearColor,1,@ClearRange);
  FillChar(MemBarrier,SizeOf(MemBarrier),#0);
  MemBarrier.sType:=VK_STRUCTURE_TYPE_MEMORY_BARRIER;
  MemBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT);
  MemBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT);
  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),0,1,@MemBarrier,0,nil,0,nil);
 end;

 FillChar(Blit,SizeOf(Blit),#0);
 Blit.srcSubresource.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 Blit.srcSubresource.layerCount:=1;
 Blit.srcOffsets[1].x:=fWidth;
 Blit.srcOffsets[1].y:=fHeight;
 Blit.srcOffsets[1].z:=1;
 Blit.dstSubresource.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 Blit.dstSubresource.layerCount:=1;
 Blit.dstOffsets[0].x:=DstX;
 Blit.dstOffsets[0].y:=DstY;
 Blit.dstOffsets[0].z:=0;
 Blit.dstOffsets[1].x:=DstX+DstW;
 Blit.dstOffsets[1].y:=DstY+DstH;
 Blit.dstOffsets[1].z:=1;
 aCommandBuffer.CmdBlitImage(GetOutputImage.Handle,SourceLayout,
                             aTargetImage.Handle,VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                             1,@Blit,VK_FILTER_LINEAR);

 TransitionTarget(VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,aTargetNewLayout,
                  TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),0,
                  TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT));

end;

end.
