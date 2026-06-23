unit UnitTestHarness;
{$i PasVulkan.inc}

// FVD decoder validation harness, built as part of videoexample so it gets the engine's exact (lazbuild)
// compile settings + ppu. The .dpr runs it (and exits) when invoked with `--fvdtest <file.fvd> [...]`, so
// it never opens a window. It creates a headless Vulkan compute device (validation layers on) and, per given
// .fvd, builds the decoder, decodes its intra frame 0 on the GPU, reads the reconstructed RGB back and writes
// it next to the file as <file>.pas.ppm. The C reference (fvdplay --dump -> /tmp/fvd_frame0.ppm, same SPIR-V)
// is then compared byte-for-byte externally. Lossy files raise (Stage C2) and are reported as skipped.

interface

procedure TestHarness;

implementation

uses SysUtils,
     Classes,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Framework,
     PasVulkan.Video.FlexibleVideo,
     PasVulkan.Video.FlexibleVideo.Decoder,
     PasVulkan.Video.FlexibleVideo.Player,
     PasVulkan.Video.H264.Decoder,
     PasVulkan.Audio.QOAL,
     PasVulkan.Audio.RPCM,
     PasVulkan.Audio.OGGVorbis,
     PasVulkan.Audio.FlexibleWavelet,
     PasVulkan.Audio.FlexibleWavelet.Decoder,
     PasVulkan.Audio.FlexibleWavelet.Encoder;

// Decode frame aFrameIndex, read the output image back and write it to aPath: an 8-bit RGB PPM for the SDR
// R8G8B8A8 format (matches the C --dump), or the raw RGBA16F bytes for the scRGB FP16 format. aSubmitMode 2
// (mode C) drives the caller-step-loop: each B-frame decode-ahead step is its own submit+wait.
procedure DecodeFrameToFile(const aDevice:TpvVulkanDevice;const aDecoder:TpvFlexibleVideoDecoder;const aFrameIndex:TpvInt32;const aPath:string;const aSubmitMode:TpvInt32);
var CommandPool:TpvVulkanCommandPool;
    CommandBuffer:TpvVulkanCommandBuffer;
    Fence:TpvVulkanFence;
    ReadbackBuffer:TpvVulkanBuffer;
    Region:TVkBufferImageCopy;
    PixelCount,Index,BytesPerPixel:TpvSizeInt;
    Mapped:PpvUInt8Array;
    OutFile:TFileStream;
    AlphaFile:TFileStream;
    HeaderText:TpvRawByteString;
    IsFP16,MoreSteps:boolean;
begin
 CommandPool:=nil;
 CommandBuffer:=nil;
 Fence:=nil;
 ReadbackBuffer:=nil;
 try

  IsFP16:=aDecoder.OutputFormat=VK_FORMAT_R16G16B16A16_SFLOAT;
  if IsFP16 then begin
   BytesPerPixel:=8;
  end else begin
   BytesPerPixel:=4;
  end;
  PixelCount:=TpvSizeInt(aDecoder.Width)*TpvSizeInt(aDecoder.Height);

  ReadbackBuffer:=TpvVulkanBuffer.Create(aDevice,PixelCount*BytesPerPixel,TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT),0);

  CommandPool:=TpvVulkanCommandPool.Create(aDevice,aDevice.UniversalQueueFamilyIndex);
  CommandBuffer:=TpvVulkanCommandBuffer.Create(CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  Fence:=TpvVulkanFence.Create(aDevice);

  FillChar(Region,SizeOf(Region),#0);
  Region.imageSubresource.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
  Region.imageSubresource.mipLevel:=0;
  Region.imageSubresource.baseArrayLayer:=0;
  Region.imageSubresource.layerCount:=1;
  Region.imageExtent.width:=aDecoder.Width;
  Region.imageExtent.height:=aDecoder.Height;
  Region.imageExtent.depth:=1;

  if aSubmitMode=2 then begin
   // mode C: loop the decode-ahead, submitting + waiting per coding-frame step; the final step records the display
   repeat
    CommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));
    MoreSteps:=aDecoder.DecodeFrameStep(CommandBuffer,aFrameIndex);
    if not MoreSteps then begin
     CommandBuffer.CmdCopyImageToBuffer(aDecoder.OutputImage.Handle,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,ReadbackBuffer.Handle,1,@Region);
    end;
    CommandBuffer.EndRecording;
    CommandBuffer.Execute(aDevice.UniversalQueue,TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),nil,nil,Fence,true);
   until not MoreSteps;
  end else begin
   // modes A (self-submit decode-ahead) and B (whole decode-ahead in this command buffer): one submit
   CommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));
   aDecoder.DecodeFrame(CommandBuffer,aFrameIndex);
   CommandBuffer.CmdCopyImageToBuffer(aDecoder.OutputImage.Handle,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,ReadbackBuffer.Handle,1,@Region);
   CommandBuffer.EndRecording;
   CommandBuffer.Execute(aDevice.UniversalQueue,TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),nil,nil,Fence,true);
  end;

  Mapped:=PpvUInt8Array(ReadbackBuffer.Memory.MapMemory);
  try
   OutFile:=TFileStream.Create(aPath,fmCreate);
   try
    if IsFP16 then begin
     OutFile.WriteBuffer(Mapped^[0],PixelCount*8); // raw RGBA16F (the C scRGB readback layout)
    end else begin
     HeaderText:=TpvRawByteString('P6'#10+IntToStr(aDecoder.Width)+' '+IntToStr(aDecoder.Height)+#10'255'#10);
     OutFile.WriteBuffer(HeaderText[1],Length(HeaderText));
     for Index:=0 to PixelCount-1 do begin
      OutFile.WriteBuffer(Mapped^[Index*4],3); // R,G,B (drop A) -> 8-bit PPM
     end;
    end;
   finally
    OutFile.Free;
   end;
   // optional alpha: dump the decoded A lane as a raw 8-bit grayscale sidecar (matches the C fvdplay --dump alpha lane),
   // so the alpha round-trip can be verified externally (the PPM itself drops A).
   if aDecoder.HasAlpha and not IsFP16 then begin
    AlphaFile:=TFileStream.Create(aPath+'.a.gray',fmCreate);
    try
     for Index:=0 to PixelCount-1 do begin
      AlphaFile.WriteBuffer(Mapped^[(Index*4)+3],1);
     end;
    finally
     AlphaFile.Free;
    end;
   end;
  finally
   ReadbackBuffer.Memory.UnmapMemory;
  end;

 finally
  FreeAndNil(Fence);
  FreeAndNil(CommandBuffer);
  FreeAndNil(CommandPool);
  FreeAndNil(ReadbackBuffer);
 end;
end;

procedure CheckOne(const aDevice:TpvVulkanDevice;const aPath:string);
var Stream:TFileStream;
    Decoder:TpvFlexibleVideoDecoder;
    FrameIndex,SubmitMode:TpvInt32;
    PreferSCRGB:boolean;
    Extension:string;
begin
 PreferSCRGB:=GetEnvironmentVariable('FVD_SCRGB')='1'; // scRGB FP16 output for HDR streams (.pasN.f16 instead of .ppm)
 SubmitMode:=StrToIntDef(GetEnvironmentVariable('FVD_BMODE'),0); // 0=A self-submit, 1=B caller-CB, 2=C step-loop
 Stream:=TFileStream.Create(aPath,fmOpenRead);
 try
  Decoder:=TpvFlexibleVideoDecoder.Create(Stream,aDevice,PreferSCRGB,SubmitMode);
  try
   if Decoder.OutputFormat=VK_FORMAT_R16G16B16A16_SFLOAT then begin
    Extension:='.f16';
   end else begin
    Extension:='.ppm';
   end;
   try
    // decode every frame in coding order (P-frames reference GPU-resident state from the previous frame),
    // writing each reconstructed frame to <path>.pasN.{ppm|f16} for the byte-diff against the C reference
    for FrameIndex:=0 to Decoder.FrameCount-1 do begin
     DecodeFrameToFile(aDevice,Decoder,FrameIndex,aPath+'.pas'+IntToStr(FrameIndex)+Extension,SubmitMode);
    end;
    writeln(Format('  %-40s %dx%d frames=%d hdr=%d bmode=%d -> %s.pas{0..%d}%s',[aPath,Decoder.Width,Decoder.Height,Decoder.FrameCount,Ord(Decoder.IsHDR),SubmitMode,aPath,Decoder.FrameCount-1,Extension]));
   except
    on e:Exception do begin
     writeln(Format('  %-40s %dx%d frames=%d (decode skipped: %s)',[aPath,Decoder.Width,Decoder.Height,Decoder.FrameCount,e.Message]));
    end;
   end;
  finally
   Decoder.Free;
  end;
 finally
  Stream.Free;
 end;
end;

// Drive the poll-API facade exactly like a player would: walk the presentation timeline one display frame at a time
// (t = mid-frame so it maps cleanly to that index), DecodeTime (CPU) then Decode (GPU) into one command buffer, read the
// reconstructed frame back and write <path>.playerN.{ppm|f16}. Bit-diffing those against the C display frames validates
// the facade's time->frame mapping AND its two-phase split produce exactly the decoder's already-validated output.
procedure CheckOnePlayer(const aDevice:TpvVulkanDevice;const aPath:string);
var Stream:TFileStream;
    Player:TpvFlexibleVideoPlayer;
    CommandPool:TpvVulkanCommandPool;
    CommandBuffer:TpvVulkanCommandBuffer;
    Fence:TpvVulkanFence;
    ReadbackBuffer:TpvVulkanBuffer;
    Region:TVkBufferImageCopy;
    FrameIndex,Index,BytesPerPixel:TpvSizeInt;
    PixelCount:TpvSizeInt;
    Mapped:PpvUInt8Array;
    OutFile:TFileStream;
    HeaderText:TpvRawByteString;
    IsFP16,Staged:boolean;
    Extension,OutPath:string;
    Time:TpvDouble;
begin
 Stream:=TFileStream.Create(aPath,fmOpenRead);
 try
  Player:=TpvFlexibleVideoPlayer.Create(Stream,aDevice);
  try
   IsFP16:=Player.OutputFormat=VK_FORMAT_R16G16B16A16_SFLOAT;
   if IsFP16 then begin
    BytesPerPixel:=8;
    Extension:='.f16';
   end else begin
    BytesPerPixel:=4;
    Extension:='.ppm';
   end;
   PixelCount:=TpvSizeInt(Player.Width)*TpvSizeInt(Player.Height);

   ReadbackBuffer:=TpvVulkanBuffer.Create(aDevice,PixelCount*BytesPerPixel,TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT),0);
   CommandPool:=TpvVulkanCommandPool.Create(aDevice,aDevice.UniversalQueueFamilyIndex);
   CommandBuffer:=TpvVulkanCommandBuffer.Create(CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
   Fence:=TpvVulkanFence.Create(aDevice);
   try

    FillChar(Region,SizeOf(Region),#0);
    Region.imageSubresource.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
    Region.imageSubresource.layerCount:=1;
    Region.imageExtent.width:=Player.Width;
    Region.imageExtent.height:=Player.Height;
    Region.imageExtent.depth:=1;

    for FrameIndex:=0 to Player.FrameCount-1 do begin
     Time:=(FrameIndex+0.5)/Player.FrameRate; // mid-frame -> maps to exactly this display index
     CommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));
     Staged:=Player.DecodeTime(Time); // CPU advance one frame toward the target
     if Staged then begin
      Player.Decode(CommandBuffer); // GPU record into this command buffer
     end;
     CommandBuffer.CmdCopyImageToBuffer(Player.OutputImage.Handle,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,ReadbackBuffer.Handle,1,@Region);
     CommandBuffer.EndRecording;
     CommandBuffer.Execute(aDevice.UniversalQueue,TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),nil,nil,Fence,true);

     OutPath:=aPath+'.player'+IntToStr(FrameIndex)+Extension;
     Mapped:=PpvUInt8Array(ReadbackBuffer.Memory.MapMemory);
     try
      OutFile:=TFileStream.Create(OutPath,fmCreate);
      try
       if IsFP16 then begin
        OutFile.WriteBuffer(Mapped^[0],PixelCount*8);
       end else begin
        HeaderText:=TpvRawByteString('P6'#10+IntToStr(Player.Width)+' '+IntToStr(Player.Height)+#10'255'#10);
        OutFile.WriteBuffer(HeaderText[1],Length(HeaderText));
        for Index:=0 to PixelCount-1 do begin
         OutFile.WriteBuffer(Mapped^[Index*4],3);
        end;
       end;
      finally
       OutFile.Free;
      end;
     finally
      ReadbackBuffer.Memory.UnmapMemory;
     end;
    end;

    writeln(Format('  %-40s %dx%d frames=%d fps=%.3f dur=%.2fs h264=%d using_h264=%d -> %s.player{0..%d}%s',
                   [aPath,Player.Width,Player.Height,Player.FrameCount,Player.FrameRate,Player.Duration,
                    Ord(Player.HasH264Stream),Ord(Player.UsingH264),aPath,Player.FrameCount-1,Extension]));

   finally
    FreeAndNil(Fence);
    FreeAndNil(CommandBuffer);
    FreeAndNil(CommandPool);
    FreeAndNil(ReadbackBuffer);
   end;
  finally
   Player.Free;
  end;
 finally
  Stream.Free;
 end;
end;

// Decode a standalone "qoal" blob via TpvAudioQOALDecoder (pure CPU, no Vulkan) and write the reconstructed interleaved
// s16 next to it as <path>.pas.s16, for a byte-diff against the C qoal_decode reference. f32 = s16/32768 is exact, so
// round(f32*32768) recovers the s16 bit-exactly.
procedure CheckQOAL(const aPath:string);
var Stream:TFileStream;
    Decoder:TpvAudioQOALDecoder;
    OutFile:TFileStream;
    FloatBuffer:array of TpvFloat;
    Samples:array of TpvInt16;
    Total,WriteCursor,SeekFrame:TpvInt64;
    Got,SampleIndex:TpvSizeInt;
begin
 Stream:=TFileStream.Create(aPath,fmOpenRead);
 try
  Decoder:=TpvAudioQOALDecoder.Create(Stream);
  try
   SeekFrame:=StrToInt64Def(GetEnvironmentVariable('FVD_QOALSEEK'),0); // optional: seek before decoding (seek validation)
   if SeekFrame>0 then begin
    Decoder.Seek(SeekFrame);
   end;
   Total:=(Decoder.FrameCount-SeekFrame)*Decoder.Channels;
   if Total<0 then begin
    Total:=0;
   end;
   SetLength(FloatBuffer,Decoder.Channels*4096);
   SetLength(Samples,Total);
   WriteCursor:=0;
   repeat
    Got:=Decoder.Decode(@FloatBuffer[0],4096);
    for SampleIndex:=0 to (Got*Decoder.Channels)-1 do begin
     Samples[WriteCursor+SampleIndex]:=Round(FloatBuffer[SampleIndex]*32768.0);
    end;
    inc(WriteCursor,Got*Decoder.Channels);
   until Got<=0;
   OutFile:=TFileStream.Create(aPath+'.pas.s16',fmCreate);
   try
    if Total>0 then begin
     OutFile.WriteBuffer(Samples[0],Total*SizeOf(TpvInt16));
    end;
   finally
    OutFile.Free;
   end;
   writeln(Format('  %-40s qoal ch=%d rate=%d frames=%d -> %s.pas.s16',[aPath,Decoder.Channels,Decoder.SampleRate,Decoder.FrameCount,aPath]));
  finally
   Decoder.Free;
  end;
 finally
  Stream.Free;
 end;
end;

// FWA self-test: decode the whole stream (incl. cross-fade overlap) + write <path>.pas.s16 for a byte/SNR diff vs C fwa dec.
procedure CheckFWA(const aPath:string);
var Stream:TFileStream;
    Decoder:TpvFlexibleWaveletAudioDecoder;
    OutFile:TFileStream;
    FloatBuffer:array of TpvFloat;
    Samples:array of TpvInt16;
    Total,WriteCursor:TpvInt64;
    Got,SampleIndex:TpvSizeInt;
begin
 Stream:=TFileStream.Create(aPath,fmOpenRead);
 try
  Decoder:=TpvFlexibleWaveletAudioDecoder.Create(Stream);
  try
   Total:=Decoder.FrameCount*Decoder.Channels;
   if Total<0 then begin
    Total:=0;
   end;
   SetLength(FloatBuffer,Decoder.Channels*4096);
   SetLength(Samples,Total);
   WriteCursor:=0;
   repeat
    Got:=Decoder.Decode(@FloatBuffer[0],4096);
    for SampleIndex:=0 to (Got*Decoder.Channels)-1 do begin
     Samples[WriteCursor+SampleIndex]:=Round(FloatBuffer[SampleIndex]*32768.0);
    end;
    inc(WriteCursor,Got*Decoder.Channels);
   until Got<=0;
   OutFile:=TFileStream.Create(aPath+'.pas.s16',fmCreate);
   try
    if Total>0 then begin
     OutFile.WriteBuffer(Samples[0],Total*SizeOf(TpvInt16));
    end;
   finally
    OutFile.Free;
   end;
   writeln(Format('  %-40s fwa ch=%d rate=%d frames=%d -> %s.pas.s16',[aPath,Decoder.Channels,Decoder.SampleRate,Decoder.FrameCount,aPath]));
  finally
   Decoder.Free;
  end;
 finally
  Stream.Free;
 end;
end;

// FWA seek self-test (FVD_FWASEEK=1): full-decode a reference, then Seek+Decode at several positions (incl. hard
// backward seeks) and confirm the samples match the reference bit-for-bit. Validates the bounded LMS-state rebuild
// (EnsureLMSStateAt replays only from the nearest reset boundary, not from block 0).
procedure CheckFWASeek(const aPath:string);
const Positions:array[0..7] of TpvDouble=(0.97,0.10,0.55,0.02,0.99,0.33,0.80,0.005);
var Stream:TFileStream;
    Decoder:TpvFlexibleWaveletAudioDecoder;
    Reference,Chunk:array of TpvFloat;
    Total,SeekFrame:TpvInt64;
    Got,Channels,SampleIndex,WorstDiff,Diff,ChunkFrames,k:TpvSizeInt;
    WriteCursor:TpvInt64;
    Verdict:TpvUTF8String;
begin
 Stream:=TFileStream.Create(aPath,fmOpenRead);
 try
  Decoder:=TpvFlexibleWaveletAudioDecoder.Create(Stream);
  try
   Channels:=Decoder.Channels;
   ChunkFrames:=2000;
   SetLength(Reference,Decoder.FrameCount*Channels);
   SetLength(Chunk,ChunkFrames*Channels);
   // reference = a clean sequential full decode
   WriteCursor:=0;
   repeat
    Got:=Decoder.Decode(@Chunk[0],ChunkFrames);
    for SampleIndex:=0 to (Got*Channels)-1 do begin
     Reference[WriteCursor+SampleIndex]:=Chunk[SampleIndex];
    end;
    inc(WriteCursor,Got*Channels);
   until Got<=0;
   // seek to each position and compare the decoded chunk to the reference there
   WorstDiff:=0;
   for k:=0 to High(Positions) do begin
    SeekFrame:=Trunc(Positions[k]*Decoder.FrameCount);
    if SeekFrame<0 then begin
     SeekFrame:=0;
    end;
    Decoder.Seek(TpvUInt64(SeekFrame));
    Got:=Decoder.Decode(@Chunk[0],ChunkFrames);
    for SampleIndex:=0 to (Got*Channels)-1 do begin
     Diff:=Round(Abs(Chunk[SampleIndex]-Reference[(SeekFrame*Channels)+SampleIndex])*32768.0);
     if Diff>WorstDiff then begin
      WorstDiff:=Diff;
     end;
    end;
   end;
   Total:=Decoder.FrameCount;
   if WorstDiff=0 then begin
    Verdict:='SEEK BIT-EXACT';
   end else begin
    Verdict:='MISMATCH';
   end;
   writeln(Format('  %-40s fwa-seek ch=%d frames=%d lms-reset=%d -> worst|diff|=%d (%s)',
                  [aPath,Channels,Total,Decoder.LMSResetBlocks,WorstDiff,Verdict]));
  finally
   Decoder.Free;
  end;
 finally
  Stream.Free;
 end;
end;

// FWA encoder round-trip self-test: synthesize a tone, Pascal-encode (cross-fade overlap from FVD_FWAENC_OVERLAP),
// write <path>.pas.fwa for an external C `fwa dec` cross-check, then round-trip via the Pascal decoder + report the diff.
procedure CheckFWAEncode(const aPath:string);
const SampleRate=48000;
      FrameTotal=48000;
var Encoder:TpvFlexibleWaveletAudioEncoder;
    Decoder:TpvFlexibleWaveletAudioDecoder;
    Params:TpvFlexibleWaveletAudioEncoder.TParams;
    Buffers:TpvFlexibleWaveletAudio.TAudioBuffers;
    MemStream:TMemoryStream;
    OutFile:TFileStream;
    Reference:array of TpvInt16;
    FloatBuffer:array of TpvFloat;
    Decoded:array of TpvInt16;
    Frame,SampleIndex,WriteCursor,Total,Limit,Difference,MaxDifference:TpvInt64;
    Got:TpvSizeInt;
    SumSquares:TpvDouble;
begin
 SetLength(Buffers,2);
 SetLength(Buffers[0],FrameTotal);
 SetLength(Buffers[1],FrameTotal);
 SetLength(Reference,FrameTotal*2);
 for Frame:=0 to FrameTotal-1 do begin
  Buffers[0][Frame]:=0.5*Sin((2.0*Pi*440.0*Frame)/SampleRate);
  Buffers[1][Frame]:=0.5*Sin((2.0*Pi*660.0*Frame)/SampleRate);
  Reference[(Frame*2)+0]:=Round(Buffers[0][Frame]*32768.0); // 0.5*sine stays well within int16, no clamp needed
  Reference[(Frame*2)+1]:=Round(Buffers[1][Frame]*32768.0);
 end;
 FillChar(Params,SizeOf(Params),0);
 Params.Quality:=8;
 Params.Perceptual:=true;
 Params.Joint:=true;
 Params.PairEnabled:=true;
 Params.Adapt:=true;
 Params.Overlap:=StrToIntDef(GetEnvironmentVariable('FVD_FWAENC_OVERLAP'),1024);
 MemStream:=TMemoryStream.Create;
 try
  Encoder:=TpvFlexibleWaveletAudioEncoder.Create;
  try
   Encoder.Encode(Buffers,SampleRate,Params,MemStream);
  finally
   Encoder.Free;
  end;
  MemStream.Position:=0;
  OutFile:=TFileStream.Create(aPath+'.pas.fwa',fmCreate);
  try
   OutFile.CopyFrom(MemStream,MemStream.Size);
  finally
   OutFile.Free;
  end;
  MemStream.Position:=0;
  Decoder:=TpvFlexibleWaveletAudioDecoder.Create(MemStream);
  try
   Total:=Decoder.FrameCount*Decoder.Channels;
   SetLength(Decoded,Total);
   SetLength(FloatBuffer,Decoder.Channels*4096);
   WriteCursor:=0;
   repeat
    Got:=Decoder.Decode(@FloatBuffer[0],4096);
    for SampleIndex:=0 to (Got*Decoder.Channels)-1 do begin
     Decoded[WriteCursor+SampleIndex]:=Round(FloatBuffer[SampleIndex]*32768.0);
    end;
    inc(WriteCursor,Got*Decoder.Channels);
   until Got<=0;
   Limit:=Total;
   if Limit>(FrameTotal*2) then begin
    Limit:=FrameTotal*2;
   end;
   SumSquares:=0.0;
   MaxDifference:=0;
   for SampleIndex:=0 to Limit-1 do begin
    Difference:=Abs(Decoded[SampleIndex]-Reference[SampleIndex]);
    if Difference>MaxDifference then begin
     MaxDifference:=Difference;
    end;
    SumSquares:=SumSquares+(Difference*Difference);
   end;
   writeln(Format('  FWA encode round-trip overlap=%d: frames=%d maxdiff=%d rms=%.2f -> %s.pas.fwa',[Params.Overlap,Decoder.FrameCount,MaxDifference,Sqrt(SumSquares/Limit),aPath]));
  finally
   Decoder.Free;
  end;
 finally
  MemStream.Free;
 end;
end;

// RPCM self-test (lossless raw PCM): decode + write <path>.pas.s16 for a byte-diff against the C rpcm_decode_s16 ref.
procedure CheckRPCM(const aPath:string);
var Stream:TFileStream;
    Decoder:TpvAudioRPCMDecoder;
    OutFile:TFileStream;
    FloatBuffer:array of TpvFloat;
    Samples:array of TpvInt16;
    Total,WriteCursor:TpvInt64;
    Got,SampleIndex:TpvSizeInt;
begin
 Stream:=TFileStream.Create(aPath,fmOpenRead);
 try
  Decoder:=TpvAudioRPCMDecoder.Create(Stream);
  try
   Total:=Decoder.FrameCount*Decoder.Channels;
   SetLength(FloatBuffer,Decoder.Channels*4096);
   SetLength(Samples,Total);
   WriteCursor:=0;
   repeat
    Got:=Decoder.Decode(@FloatBuffer[0],4096);
    for SampleIndex:=0 to (Got*Decoder.Channels)-1 do begin
     Samples[WriteCursor+SampleIndex]:=Round(FloatBuffer[SampleIndex]*32768.0);
    end;
    inc(WriteCursor,Got*Decoder.Channels);
   until Got<=0;
   OutFile:=TFileStream.Create(aPath+'.pas.s16',fmCreate);
   try
    if Total>0 then begin
     OutFile.WriteBuffer(Samples[0],Total*SizeOf(TpvInt16));
    end;
   finally
    OutFile.Free;
   end;
   writeln(Format('  %-40s rpcm ch=%d rate=%d frames=%d -> %s.pas.s16',[aPath,Decoder.Channels,Decoder.SampleRate,Decoder.FrameCount,aPath]));
  finally
   Decoder.Free;
  end;
 finally
  Stream.Free;
 end;
end;

// OGGV self-test (lossy Vorbis): decode via Tremor + write <path>.pas.s16 for a tolerance compare against an ffmpeg
// decode (Tremor != ffmpeg's Vorbis, so not bit-exact). Mainly confirms channels/rate/frame-count + sane output.
procedure CheckOGGV(const aPath:string);
var Stream:TFileStream;
    Decoder:TpvAudioOGGVorbisDecoder;
    OutFile:TFileStream;
    FloatBuffer:array of TpvFloat;
    Sample:TpvInt16;
    Got,SampleIndex:TpvSizeInt;
    Produced:TpvInt64;
begin
 Stream:=TFileStream.Create(aPath,fmOpenRead);
 try
  Decoder:=TpvAudioOGGVorbisDecoder.Create(Stream);
  try
   SetLength(FloatBuffer,Decoder.Channels*4096);
   OutFile:=TFileStream.Create(aPath+'.pas.s16',fmCreate);
   try
    Produced:=0;
    repeat
     Got:=Decoder.Decode(@FloatBuffer[0],4096);
     for SampleIndex:=0 to (Got*Decoder.Channels)-1 do begin
      Sample:=Round(FloatBuffer[SampleIndex]*32768.0);
      OutFile.WriteBuffer(Sample,SizeOf(TpvInt16));
     end;
     inc(Produced,Got);
    until Got<=0;
   finally
    OutFile.Free;
   end;
   writeln(Format('  %-40s oggv ch=%d rate=%d frames=%d (decoded %d) -> %s.pas.s16',[aPath,Decoder.Channels,Decoder.SampleRate,Decoder.FrameCount,Produced,aPath]));
  finally
   Decoder.Free;
  end;
 finally
  Stream.Free;
 end;
end;

// H.264 parse self-test (Stage F3a): extract the container's Annex-B blob, split NALs, parse SPS/PPS/slice/POC via
// TpvVideoH264Decoder and print them in the exact format of the C /tmp/h264ref reference, for a byte-diff.
procedure CheckH264Parse(const aPath:string);
var FileStream:TFileStream;
    Header:TpvFlexibleVideo.THeader;
    H264:array of TpvUInt8;
    RBSP:array of TpvUInt8;
    SPS:TpvVideoH264Decoder.TSPS;
    PPS:TpvVideoH264Decoder.TPPS;
    Slice:TpvVideoH264Decoder.TSlice;
    HaveSPS,HavePPS:boolean;
    PreviousMSB,PreviousLSB,SliceCount,POC,NALType,RefIdc:TpvInt32;
    Position,Start,Stop,RBSPLength:TpvSizeUInt;
begin
 FileStream:=TFileStream.Create(aPath,fmOpenRead);
 try
  FileStream.ReadBuffer(Header,SizeOf(Header));
  if Header.H264Size=0 then begin
   writeln('  ',aPath,': no H.264 stream');
   exit;
  end;
  SetLength(H264,Header.H264Size);
  FileStream.Seek(TpvInt64(Header.H264Offset),soBeginning);
  FileStream.ReadBuffer(H264[0],Header.H264Size);
 finally
  FileStream.Free;
 end;

 SetLength(RBSP,Header.H264Size+16);
 HaveSPS:=false;
 HavePPS:=false;
 PreviousMSB:=0;
 PreviousLSB:=0;
 SliceCount:=0;
 Position:=0;
 while (Position+3)<Header.H264Size do begin
  if (H264[Position]=0) and (H264[Position+1]=0) and (H264[Position+2]=1) then begin
   Start:=Position+3;
   Stop:=Start;
   while ((Stop+3)<Header.H264Size) and not ((H264[Stop]=0) and (H264[Stop+1]=0) and (H264[Stop+2]=1)) do begin
    inc(Stop);
   end;
   if (Stop+3)>=Header.H264Size then begin
    Stop:=Header.H264Size;
   end;
   NALType:=H264[Start] and $1f;
   RefIdc:=(H264[Start] shr 5) and 3;
   RBSPLength:=TpvVideoH264Decoder.ToRBSP(PpvUInt8Array(@H264[Start+1]),Stop-Start-1,PpvUInt8Array(@RBSP[0]));
   case NALType of
    7:begin
     TpvVideoH264Decoder.ParseSPS(PpvUInt8Array(@RBSP[0]),RBSPLength,SPS);
     HaveSPS:=true;
     writeln(Format('SPS profile=%d level=%d sps_id=%d chroma=%d log2fn=%d poc_type=%d log2poc=%d maxref=%d w=%d h=%d',
                    [SPS.ProfileIDC,SPS.LevelIDC,SPS.SPSId,SPS.ChromaFormatIDC,SPS.Log2MaxFrameNumMinus4,SPS.POCType,SPS.Log2MaxPOCLSBMinus4,SPS.MaxNumRef,SPS.Width,SPS.Height]));
    end;
    8:begin
     TpvVideoH264Decoder.ParsePPS(PpvUInt8Array(@RBSP[0]),RBSPLength,PPS);
     HavePPS:=true;
     writeln(Format('PPS pps_id=%d sps_id=%d entropy=%d nref0=%d nref1=%d wpred=%d initqp=%d t8x8=%d',
                    [PPS.PPSId,PPS.SPSId,PPS.EntropyCodingMode,PPS.NumRefL0Minus1,PPS.NumRefL1Minus1,PPS.WeightedPred,PPS.PicInitQPMinus26,PPS.Transform8x8]));
    end;
    1,5:begin
     if HaveSPS and HavePPS and (SliceCount<8) then begin
      TpvVideoH264Decoder.ParseSlice(PpvUInt8Array(@RBSP[0]),RBSPLength,SPS,PPS,NALType,RefIdc,Slice);
      POC:=TpvVideoH264Decoder.ComputePOC(SPS,Slice,PreviousMSB,PreviousLSB);
      writeln(Format('SLICE type=%d frame_num=%d idr=%d poc_lsb=%d poc=%d',[Slice.SliceType,Slice.FrameNum,Slice.IDR,Slice.POCLSB,POC]));
      inc(SliceCount);
     end;
    end;
   end;
   Position:=Stop;
  end else begin
   inc(Position);
  end;
 end;
end;

// ABI check (Stage F3b): print SizeOf the generated VK-video / StdVideoH264 records, to byte-diff against the C
// sizeof reference (validates the generator's bitfield = backing-uint representation matches the C ABI).
procedure CheckVKVideoSizes;
begin
{$ifdef VkStdVideo}
 writeln('StdVideoH264SpsFlags ',SizeOf(TStdVideoH264SpsFlags));
 writeln('StdVideoH264PpsFlags ',SizeOf(TStdVideoH264PpsFlags));
 writeln('StdVideoH264SequenceParameterSet ',SizeOf(TStdVideoH264SequenceParameterSet));
 writeln('StdVideoH264PictureParameterSet ',SizeOf(TStdVideoH264PictureParameterSet));
 writeln('StdVideoDecodeH264PictureInfo ',SizeOf(TStdVideoDecodeH264PictureInfo));
 writeln('StdVideoDecodeH264ReferenceInfo ',SizeOf(TStdVideoDecodeH264ReferenceInfo));
{$endif}
{$ifdef VkVideo}
 writeln('VkVideoSessionCreateInfoKHR ',SizeOf(TVkVideoSessionCreateInfoKHR));
 writeln('VkVideoDecodeInfoKHR ',SizeOf(TVkVideoDecodeInfoKHR));
 writeln('VkVideoDecodeH264PictureInfoKHR ',SizeOf(TVkVideoDecodeH264PictureInfoKHR));
{$endif}
end;

// H.264 frame-list self-test (Stage F3b-1): ParseAnnexB the whole container blob into the decode-order frame list and
// dump it in the exact format of the C /tmp/h264frames reference, for a byte-diff over all frames (not just 8 slices).
procedure CheckH264Frames(const aPath:string);
var FileStream:TFileStream;
    Header:TpvFlexibleVideo.THeader;
    H264:array of TpvUInt8;
    SPS:TpvVideoH264Decoder.TSPS;
    PPS:TpvVideoH264Decoder.TPPS;
    Frames:TpvVideoH264Decoder.TFrames;
    FrameCount,Index:TpvInt32;
begin
 FileStream:=TFileStream.Create(aPath,fmOpenRead);
 try
  FileStream.ReadBuffer(Header,SizeOf(Header));
  if Header.H264Size=0 then begin
   writeln('  ',aPath,': no H.264 stream');
   exit;
  end;
  SetLength(H264,Header.H264Size);
  FileStream.Seek(TpvInt64(Header.H264Offset),soBeginning);
  FileStream.ReadBuffer(H264[0],Header.H264Size);
 finally
  FileStream.Free;
 end;
 FrameCount:=TpvVideoH264Decoder.ParseAnnexB(PpvUInt8Array(@H264[0]),Header.H264Size,SPS,PPS,Frames);
 for Index:=0 to FrameCount-1 do begin
  writeln(Format('F%d type=%d poc=%d idr=%d fn=%d ref=%d len=%d',
                 [Index,Frames[Index].SliceType,Frames[Index].POC,Frames[Index].IDR,Frames[Index].FrameNum,Frames[Index].RefIdc,Frames[Index].NALLength]));
 end;
 writeln(Format('frames=%d w=%d h=%d',[FrameCount,SPS.Width,SPS.Height]));
end;

procedure TestHarness;
var Instance:TpvVulkanInstance;
    Device:TpvVulkanDevice;
    Index:TpvInt32;
    UsePlayer:boolean;
    Extension:string;
begin
 if GetEnvironmentVariable('FVD_H264SIZES')='1' then begin // ABI self-test: dump VK-video/StdVideo record sizes, then exit
  CheckVKVideoSizes;
  writeln('RESULT: VK video sizes dumped');
  exit;
 end;
 Instance:=TpvVulkanInstance.Create('fvdtest',1,'PasVulkan',1,VK_API_VERSION_1_1,true,nil);
 try
  Instance.Initialize;
  Device:=TpvVulkanDevice.Create(Instance,nil,nil,nil,true);
  try
   Device.AddQueues(nil); // headless: no surface -> no present queue
   Device.Initialize;
   writeln('GPU: ',string(Device.PhysicalDevice.DeviceName));
   UsePlayer:=GetEnvironmentVariable('FVD_PLAYER')='1'; // drive the poll-API facade instead of the decoder directly
   for Index:=2 to ParamCount do begin
    Extension:=LowerCase(ExtractFileExt(ParamStr(Index)));
    if GetEnvironmentVariable('FVD_H264FRAMES')='1' then begin
     CheckH264Frames(ParamStr(Index)); // Stage F3b-1 H.264 frame-list self-test
    end else if GetEnvironmentVariable('FVD_H264PARSE')='1' then begin
     CheckH264Parse(ParamStr(Index)); // Stage F3a H.264 bitstream parse self-test
    end else if GetEnvironmentVariable('FVD_FWASEEK')='1' then begin
     CheckFWASeek(ParamStr(Index)); // FWA seek self-test: backward seeks must match the sequential decode (bounded LMS rebuild)
    end else if GetEnvironmentVariable('FVD_FWAENC')='1' then begin
     CheckFWAEncode(ParamStr(Index)); // FWA encoder round-trip self-test (Pascal-encode -> .pas.fwa for a C fwa-dec cross-check)
    end else if Extension='.qoal' then begin
     CheckQOAL(ParamStr(Index)); // pure-CPU audio sub-codec self-tests
    end else if Extension='.rpcm' then begin
     CheckRPCM(ParamStr(Index));
    end else if (Extension='.ogg') or (Extension='.oggv') then begin
     CheckOGGV(ParamStr(Index));
    end else if Extension='.fwa' then begin
     CheckFWA(ParamStr(Index)); // FWA audio decode self-test (incl. cross-fade overlap) -> byte/SNR diff vs C fwa dec
    end else if UsePlayer then begin
     CheckOnePlayer(Device,ParamStr(Index));
    end else begin
     CheckOne(Device,ParamStr(Index));
    end;
   end;
   Device.WaitIdle;
  finally
   Device.Free;
  end;
 finally
  Instance.Free;
 end;
 writeln('RESULT: FVD decode harness done');
end;

end.
