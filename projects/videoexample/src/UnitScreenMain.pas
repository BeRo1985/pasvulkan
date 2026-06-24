unit UnitScreenMain;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$if defined(Win32) or defined(Win64)}
 {$define Windows}
{$ifend}

// Stage F2a: video-only window playback of a .fvd via the poll-API facade. Update() advances a wall-clock and calls
// DecodeTime (CPU); Draw() records the player's Decode (GPU) plus a present pass into the swapchain. Two present paths,
// toggled with B: (A, default) a fullscreen textured triangle that SAMPLES the decoded image (reuses the engine's
// ToScreenBlit shaders; this is also the path that generalises to drawing video onto textures in 3D scenes), and (B) a
// straight vkCmdBlitImage of the decoded image into the swapchain. SPACE pauses, R restarts. The .fvd path is ParamStr(1).

interface

uses SysUtils,
     Classes,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Assets,
     PasVulkan.Streams,
     PasVulkan.Video.FlexibleVideo,
     PasVulkan.Video.FlexibleVideo.Decoder,
     PasVulkan.Video.FlexibleVideo.Player,
     PasVulkan.Video.H264.Decoder;

type TScreenMain=class(TpvApplicationScreen)
      private
       fVulkanRenderPass:TpvVulkanRenderPass;
       fVulkanCommandPool:TpvVulkanCommandPool;
       fVulkanRenderCommandBuffers:array[0..MaxInFlightFrames-1] of array of TpvVulkanCommandBuffer;
       fVulkanRenderSemaphores:array[0..MaxInFlightFrames-1] of TpvVulkanSemaphore;
       // the video player + its backing stream
       fVideoPath:string;
       fStream:TFileStream;
       fPlayer:TpvFlexibleVideoPlayer;
       fPlaybackTime:TpvDouble;
       fAVLogTick:TpvInt32; // FVD_AVLOG diagnostic counter
       fDecTimeAccumUS,fDecTimeMaxUS:TpvInt64; // FVD_DECTIME: CPU DecodeTime cost accumulator
       fDecTimeCount:TpvInt32;
       fPaused:boolean;
       fPresentBlit:boolean; // False = textured-quad (A, default), True = blit (B)
       fBlendBackgroundMode:TpvInt32; // present path A composite background: 0 = checkerboard, 1 = solid color ('G' toggles)
       fOutputImageLayout:TVkImageLayout; // tracked layout of the player's OutputImage between passes
       fPlayerInitPending:boolean; // defer the heavy CreatePlayer (~0.5 s of compute-pipeline compiling) out of Show
       fBlackFramePresented:boolean; // set once Draw has cleared the swapchain to black while no player exists yet
       fAudioStartPending:boolean; // A/V start-sync: audio is opened but held until the FIRST video frame is decoded, then started together (no startup lag)
       // present path A (fullscreen textured quad sampling the decoded image)
       fSampler:TpvVulkanSampler;
       fVertexShaderModule:TpvVulkanShaderModule;
       fFragmentShaderModule:TpvVulkanShaderModule;
       fVertexShaderStage:TpvVulkanPipelineShaderStage;
       fFragmentShaderStage:TpvVulkanPipelineShaderStage;
       fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fDescriptorPool:TpvVulkanDescriptorPool;
       fDescriptorSet:TpvVulkanDescriptorSet;
       fPipelineLayout:TpvVulkanPipelineLayout;
       fGraphicsPipeline:TpvVulkanGraphicsPipeline;
       procedure CreatePlayer;
       procedure DestroyPlayer;
       procedure TestH264Session;
       procedure TestH264Decode;
       procedure BindPlayerToDescriptor;
       procedure LetterboxRect(const aTargetW,aTargetH:TpvInt32;out aX,aY,aW,aH:TpvInt32); // aspect-fit, centred video rect
       procedure TransitionOutputImage(const aCommandBuffer:TpvVulkanCommandBuffer;const aNewLayout:TVkImageLayout;const aSrcAccess,aDstAccess:TVkAccessFlags;const aSrcStage,aDstStage:TVkPipelineStageFlags);
      public

       constructor Create; override;

       destructor Destroy; override;

       procedure Show; override;

       procedure Hide; override;

       procedure Resume; override;

       procedure Pause; override;

       procedure Resize(const aWidth,aHeight:TpvInt32); override;

       procedure AfterCreateSwapChain; override;

       procedure BeforeDestroySwapChain; override;

       function CanBeParallelProcessed:boolean; override;

       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;

       procedure Update(const aDeltaTime:TpvDouble); override;

       procedure Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil); override;

     end;

implementation

// push constants for fvd_composite.frag (must match the shader's PushConstants block). The compiled shader itself is
// a RUNTIME asset (assets/shaders/fvd_composite.frag.spv, built by src/assets/shaders/compileshaders.sh) loaded via
// pvApplication.Assets, NOT embedded.
type TFVDCompositePush=packed record
      Mode:TpvInt32;          // 0 = checkerboard, 1 = solid color
      Premultiplied:TpvInt32; // 1 = video RGB premultiplied by alpha
      CheckerSize:TpvFloat;   // checkerboard cell size in pixels
      Pad:TpvFloat;
      SolidColor:array[0..3] of TpvFloat;
     end;

constructor TScreenMain.Create;
begin
 inherited Create;
 fVideoPath:=ParamStr(1); // the .fvd to play (the harness path '--fvdtest' already exits in the .dpr before the app runs)
 fStream:=nil;
 fPlayer:=nil;
 fPlaybackTime:=0.0;
 fPaused:=false;
 fPresentBlit:=GetEnvironmentVariable('FVD_BLIT')='1'; // start in present path B (else A); toggle live with B
 if GetEnvironmentVariable('FVD_BLENDBG')='1' then begin
  fBlendBackgroundMode:=1; // start on the solid-color background ('G' toggles to/from the checkerboard)
 end else begin
  fBlendBackgroundMode:=0; // default = checkerboard (classic transparency view)
 end;
 fOutputImageLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
 fPlayerInitPending:=false;
 fBlackFramePresented:=false;
 fAudioStartPending:=false;
end;

destructor TScreenMain.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.CreatePlayer;
var ForceSCRGB:boolean;
    ParamIndex:TpvInt32;
begin
 // --scrgb command-line flag: force scRGB FP16 decode even on an SDR swapchain — a sanity check that the FP16 decode +
 // present path runs without crashing on a non-HDR machine (the colors look clipped/washed on SDR, but no crash = ok).
 ForceSCRGB:=false;
 for ParamIndex:=1 to ParamCount do begin
  if ParamStr(ParamIndex)='--scrgb' then begin
   ForceSCRGB:=true;
  end;
 end;
 if (length(fVideoPath)>0) and FileExists(fVideoPath) then begin
  fStream:=TFileStream.Create(fVideoPath,fmOpenRead or fmShareDenyWrite);
  // If the engine got a real HDR swapchain (scRGB-linear FP16, see SwapChainHDR in Setup), let HDR streams output scRGB
  // FP16 for true HDR display; otherwise (SDR swapchain) the decoder stays on the rgba8 SDR / SDR-tonemap path.
  fPlayer:=TpvFlexibleVideoPlayer.Create(fStream,pvApplication.VulkanDevice,
                                                TpvFlexibleVideoPlayer.TDecoderChoice.Auto,
                                                (pvApplication.VulkanSwapChain.ImageFormat=VK_FORMAT_R16G16B16A16_SFLOAT) or ForceSCRGB,
                                                pvApplication.VulkanPipelineCache, // engine's disk-persisted cache -> warm-start the ~2 s pipeline build
                                                true); // aBlitUsage: this screen presents via BlitLastDecodedFrame -> needs the sRGB blit image
  fPlaybackTime:=0.0;
  fOutputImageLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
  // wire the container audio into the engine audio system as the A/V master clock
  if assigned(pvApplication.Audio) then begin
   fPlayer.OpenAudio(pvApplication.Audio);
   if fPlayer.HasAudio then begin
    pvApplication.Audio.OutputLatencyFrames:=pvApplication.Audio.BufferSamples; // ~one device buffer past our ring
    // Don't start the audio clock yet: the first frame's compute-pipeline warmup + GOP decode-ahead burst takes ~0.5 s,
    // and starting audio now would leave the video that far behind. Hold it until the first frame is actually decoded
    // (Draw), then start audio + video together -> no startup A/V lag.
    fAudioStartPending:=true;
   end;
  end;
  writeln(Format('videoexample: playing %s  %dx%d  %d frames  %.3f fps  %.2fs  hdr=%d  h264=%d/using=%d  audio=%d ch=%d/%dHz',
                 [fVideoPath,fPlayer.Width,fPlayer.Height,fPlayer.FrameCount,fPlayer.FrameRate,fPlayer.Duration,
                  Ord(fPlayer.IsHDR),Ord(fPlayer.HasH264Stream),Ord(fPlayer.UsingH264),
                  Ord(fPlayer.HasAudio),fPlayer.AudioChannels,fPlayer.AudioSampleRate]));
 end else begin
  if length(fVideoPath)>0 then begin
   writeln('videoexample: file not found: ',fVideoPath);
  end else begin
   writeln('videoexample: no .fvd given (usage: videoexample <file.fvd>)');
  end;
 end;
end;

procedure TScreenMain.DestroyPlayer;
begin
 FreeAndNil(fPlayer);
 FreeAndNil(fStream);
end;

procedure TScreenMain.TestH264Session;
var FileStream:TFileStream;
    Header:TpvFlexibleVideo.THeader;
    BlobStream:TMemoryStream;
    Decoder:TpvVideoH264Decoder;
begin
 FileStream:=TFileStream.Create(fVideoPath,fmOpenRead);
 try
  FileStream.ReadBuffer(Header,SizeOf(Header));
  if Header.H264Size=0 then begin
   writeln('  h264 session test: no H.264 stream');
   exit;
  end;
  BlobStream:=TMemoryStream.Create;
  try
   FileStream.Seek(TpvInt64(Header.H264Offset),soBeginning);
   BlobStream.CopyFrom(FileStream,TpvInt64(Header.H264Size));
   BlobStream.Seek(0,soBeginning);
   try
    Decoder:=TpvVideoH264Decoder.Create(BlobStream,pvApplication.VulkanDevice);
    try
     writeln(Format('  h264 session test: VkVideoSession created OK  %dx%d  %d frames',[Decoder.Width,Decoder.Height,Decoder.FrameCount]));
    finally
     Decoder.Free;
    end;
   except
    on e:Exception do begin
     writeln('  h264 session test FAILED: ',e.Message);
    end;
   end;
  finally
   BlobStream.Free;
  end;
 finally
  FileStream.Free;
 end;
end;

procedure TScreenMain.TestH264Decode;
var FileStream:TFileStream;
    Header:TpvFlexibleVideo.THeader;
    BlobStream:TMemoryStream;
    Decoder:TpvVideoH264Decoder;
    ReadbackBuffer:TpvVulkanBuffer;
    CommandBuffer:TpvVulkanCommandBuffer;
    Fence:TpvVulkanFence;
    Region:TVkBufferImageCopy;
    DumpFile:TFileStream;
    MappedPointer:PpvUInt8Array;
    RGBData:array of TpvUInt8;
    Width,Height,FrameCount,DisplayIndex,PixelIndex,ByteCount:TpvInt32;
begin
 FileStream:=TFileStream.Create(fVideoPath,fmOpenRead);
 try
  FileStream.ReadBuffer(Header,SizeOf(Header));
  if Header.H264Size=0 then begin
   writeln('  h264 decode test: no H.264 stream');
   exit;
  end;
  BlobStream:=TMemoryStream.Create;
  try
   FileStream.Seek(TpvInt64(Header.H264Offset),soBeginning);
   BlobStream.CopyFrom(FileStream,TpvInt64(Header.H264Size));
   BlobStream.SaveToFile('/tmp/fvd_pas.h264'); // the raw Annex-B blob -> the ffmpeg reference decode
   BlobStream.Seek(0,soBeginning);
   try
    Decoder:=TpvVideoH264Decoder.Create(BlobStream,pvApplication.VulkanDevice);
    try
     Width:=Decoder.Width;
     Height:=Decoder.Height;
     FrameCount:=Decoder.FrameCount;
     ByteCount:=(Width*Height)*4;
     ReadbackBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,ByteCount,
                                            TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT),0,'h264verify');
     CommandBuffer:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
     Fence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);
     DumpFile:=TFileStream.Create('/tmp/fvd_pas.rgb',fmCreate);
     SetLength(RGBData,(Width*Height)*3);
     try
      for DisplayIndex:=0 to FrameCount-1 do begin

       // HW-decode (with full DPB reference handling) the frame at this display position into OutputImageHandle.
       Decoder.EnsureDisplayFrame(DisplayIndex);

       // Read the RGBA pool image (it lives in GENERAL) back into a host-visible buffer.
       FillChar(Region,SizeOf(Region),#0);
       Region.imageSubresource.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
       Region.imageSubresource.layerCount:=1;
       Region.imageExtent.width:=Width;
       Region.imageExtent.height:=Height;
       Region.imageExtent.depth:=1;
       CommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));
       CommandBuffer.CmdCopyImageToBuffer(Decoder.OutputImage.Handle,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,ReadbackBuffer.Handle,1,@Region);
       CommandBuffer.EndRecording;
       CommandBuffer.Execute(pvApplication.VulkanDevice.UniversalQueue,
                             TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),nil,nil,Fence,true);

       // RGBA -> RGB, append to the dump file (kept on disk, never all in RAM).
       MappedPointer:=PpvUInt8Array(ReadbackBuffer.Memory.MapMemory);
       for PixelIndex:=0 to (Width*Height)-1 do begin
        RGBData[(PixelIndex*3)+0]:=MappedPointer^[(PixelIndex*4)+0];
        RGBData[(PixelIndex*3)+1]:=MappedPointer^[(PixelIndex*4)+1];
        RGBData[(PixelIndex*3)+2]:=MappedPointer^[(PixelIndex*4)+2];
       end;
       ReadbackBuffer.Memory.UnmapMemory;
       DumpFile.WriteBuffer(RGBData[0],(Width*Height)*3);

      end;
      writeln(Format('  h264 decode test: dumped %d frames %dx%d to /tmp/fvd_pas.rgb (ref blob /tmp/fvd_pas.h264)',
                     [FrameCount,Width,Height]));
     finally
      DumpFile.Free;
      Fence.Free;
      CommandBuffer.Free;
      ReadbackBuffer.Free;
     end;
    finally
     Decoder.Free;
    end;
   except
    on e:Exception do begin
     writeln('  h264 decode test FAILED: ',e.Message);
    end;
   end;
  finally
   BlobStream.Free;
  end;
 finally
  FileStream.Free;
 end;
end;

procedure TScreenMain.BindPlayerToDescriptor;
begin
 if not (assigned(fDescriptorSet) and assigned(fPlayer)) then begin
  exit;
 end;
 fDescriptorSet.WriteToDescriptorSet(0,
                                     0,
                                     1,
                                     TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                     [TVkDescriptorImageInfo.Create(fSampler.Handle,
                                                                    fPlayer.OutputImageView.Handle,
                                                                    VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                     [],
                                     [],
                                     false);
 fDescriptorSet.Flush;
end;

procedure TScreenMain.LetterboxRect(const aTargetW,aTargetH:TpvInt32;out aX,aY,aW,aH:TpvInt32);
var VideoW,VideoH:TpvInt32;
begin
 // aspect-fit the video into the target, centred: pillarbox (bars left/right) if the target is wider than the video,
 // letterbox (bars top/bottom) if it is taller. Falls back to the full target if a size is unknown.
 VideoW:=0;
 VideoH:=0;
 if assigned(fPlayer) then begin
  VideoW:=fPlayer.Width;
  VideoH:=fPlayer.Height;
 end;
 if ((VideoW<=0) or (VideoH<=0)) or ((aTargetW<=0) or (aTargetH<=0)) then begin
  aX:=0;
  aY:=0;
  aW:=aTargetW;
  aH:=aTargetH;
  exit;
 end;
 if (aTargetW*VideoH)>(aTargetH*VideoW) then begin // target wider than the video -> pillarbox
  aH:=aTargetH;
  aW:=Round((aTargetH*VideoW)/VideoH);
 end else begin // target taller (or equal) -> letterbox
  aW:=aTargetW;
  aH:=Round((aTargetW*VideoH)/VideoW);
 end;
 aX:=(aTargetW-aW) div 2;
 aY:=(aTargetH-aH) div 2;
end;

procedure TScreenMain.TransitionOutputImage(const aCommandBuffer:TpvVulkanCommandBuffer;const aNewLayout:TVkImageLayout;const aSrcAccess,aDstAccess:TVkAccessFlags;const aSrcStage,aDstStage:TVkPipelineStageFlags);
var Barrier:TVkImageMemoryBarrier;
begin
 if fOutputImageLayout=aNewLayout then begin
  exit;
 end;
 FillChar(Barrier,SizeOf(Barrier),#0);
 Barrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
 Barrier.srcAccessMask:=aSrcAccess;
 Barrier.dstAccessMask:=aDstAccess;
 Barrier.oldLayout:=fOutputImageLayout;
 Barrier.newLayout:=aNewLayout;
 Barrier.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 Barrier.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 Barrier.image:=fPlayer.OutputImage.Handle;
 Barrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 Barrier.subresourceRange.levelCount:=1;
 Barrier.subresourceRange.layerCount:=1;
 aCommandBuffer.CmdPipelineBarrier(aSrcStage,aDstStage,0,0,nil,0,nil,1,@Barrier);
 fOutputImageLayout:=aNewLayout;
end;

procedure TScreenMain.Show;
var Index,SwapChainImageIndex:TpvInt32;
    Stream:TStream;
begin
 inherited Show;

 fVulkanCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                 pvApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                                 TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
 for Index:=0 to MaxInFlightFrames-1 do begin
  SetLength(fVulkanRenderCommandBuffers[Index],pvApplication.CountSwapChainImages);
  for SwapChainImageIndex:=0 to pvApplication.CountSwapChainImages-1 do begin
   fVulkanRenderCommandBuffers[Index,SwapChainImageIndex]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  end;
  fVulkanRenderSemaphores[Index]:=TpvVulkanSemaphore.Create(pvApplication.VulkanDevice);
 end;

 fVulkanRenderPass:=nil;

 // report whether the engine got a VK video-decode queue (VideoDecodeSupport opt-in; foundation for the H.264 backend)
 writeln(Format('videoexample: video-decode queue family=%d assigned=%d',
                [pvApplication.VulkanDevice.VideoDecodeQueueFamilyIndex,Ord(assigned(pvApplication.VulkanDevice.VideoDecodeQueue))]));

 // F3b session self-test: build a VkVideoSession from the container's H.264 stream (validates the bindings at runtime)
 if (GetEnvironmentVariable('FVD_H264SESSION')='1') and assigned(pvApplication.VulkanDevice.VideoDecodeQueue) and
    (length(fVideoPath)>0) and FileExists(fVideoPath) then begin
  TestH264Session;
 end;

 // F3b-3 decode self-test: HW-decode the whole H.264 stream in DISPLAY order, dump RGB for an external ffmpeg PSNR check
 if (GetEnvironmentVariable('FVD_H264DECODE')='1') and assigned(pvApplication.VulkanDevice.VideoDecodeQueue) and
    (length(fVideoPath)>0) and FileExists(fVideoPath) then begin
  TestH264Decode;
 end;

 // The video player's Create compiles ~30 compute pipelines (~0.5 s) and blocks the render loop while it runs. Doing it
 // here in Show would mean ~0.5 s with no Draw at all -> the OS shows the still-undefined swapchain surface (a pale /
 // white flash) until the first frame. Defer it to the first Update AFTER one black frame has been presented instead.
 fBlackFramePresented:=false;
 fPlayerInitPending:=true;

 // present path A: sampler + the engine's fullscreen ToScreenBlit shaders + a single combined-image-sampler set
 fSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                   VK_FILTER_LINEAR,VK_FILTER_LINEAR,VK_SAMPLER_MIPMAP_MODE_NEAREST,
                                   VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                   0.0,false,0.0,false,VK_COMPARE_OP_ALWAYS,0.0,0.0,VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK,false);

 Stream:=TpvDataStream.Create(@PasVulkan.Assets.VRDisabledToScreenBlitVertSPIRVData[0],PasVulkan.Assets.VRDisabledToScreenBlitVertSPIRVDataSize);
 try
  fVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;
 // present path A frag = the FVD composite shader (blends the decoded alpha over the chosen background) instead of the
 // plain ToScreenBlit frag; it keeps the SAME fullscreen-triangle vertex shader + binding-0 sampler, and for an opaque
 // (non-alpha) stream (A=1) it shows the video unchanged -> a safe drop-in. Loaded as a runtime asset.
 Stream:=pvApplication.Assets.GetAssetStream('shaders/fvd_composite.frag.spv');
 try
  fFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;
 fVertexShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVertexShaderModule,'main');
 fFragmentShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fFragmentShaderModule,'main');

 fDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),1);
 fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,1);
 fDescriptorPool.Initialize;

 fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fDescriptorSetLayout.AddBinding(0,VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,1,TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),[]);
 fDescriptorSetLayout.Initialize;

 fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
 BindPlayerToDescriptor;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
 fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),0,SizeOf(TFVDCompositePush)); // fvd_composite.frag background/blend params
 fPipelineLayout.Initialize;

 fGraphicsPipeline:=nil;

end;

procedure TScreenMain.Hide;
var Index,SwapChainImageIndex:TpvInt32;
begin
 FreeAndNil(fGraphicsPipeline);
 FreeAndNil(fPipelineLayout);
 FreeAndNil(fDescriptorSet);
 FreeAndNil(fDescriptorSetLayout);
 FreeAndNil(fDescriptorPool);
 FreeAndNil(fFragmentShaderStage);
 FreeAndNil(fVertexShaderStage);
 FreeAndNil(fFragmentShaderModule);
 FreeAndNil(fVertexShaderModule);
 FreeAndNil(fSampler);
 DestroyPlayer;
 FreeAndNil(fVulkanRenderPass);
 for Index:=0 to MaxInFlightFrames-1 do begin
  for SwapChainImageIndex:=0 to length(fVulkanRenderCommandBuffers[Index])-1 do begin
   FreeAndNil(fVulkanRenderCommandBuffers[Index,SwapChainImageIndex]);
  end;
  fVulkanRenderCommandBuffers[Index]:=nil;
  FreeAndNil(fVulkanRenderSemaphores[Index]);
 end;
 FreeAndNil(fVulkanCommandPool);
 inherited Hide;
end;

procedure TScreenMain.Resume;
begin
 inherited Resume;
end;

procedure TScreenMain.Pause;
begin
 inherited Pause;
end;

procedure TScreenMain.Resize(const aWidth,aHeight:TpvInt32);
begin
 inherited Resize(aWidth,aHeight);
end;

procedure TScreenMain.AfterCreateSwapChain;
begin
 inherited AfterCreateSwapChain;

 FreeAndNil(fVulkanRenderPass);

 fVulkanRenderPass:=TpvVulkanRenderPass.Create(pvApplication.VulkanDevice);

 fVulkanRenderPass.AddSubpassDescription(0,
                                         VK_PIPELINE_BIND_POINT_GRAPHICS,
                                         [],
                                         [fVulkanRenderPass.AddAttachmentReference(fVulkanRenderPass.AddAttachmentDescription(0,
                                                                                                                              pvApplication.VulkanSwapChain.ImageFormat,
                                                                                                                              VK_SAMPLE_COUNT_1_BIT,
                                                                                                                              VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                                                                                              VK_ATTACHMENT_STORE_OP_STORE,
                                                                                                                              VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                                                                              VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                              VK_IMAGE_LAYOUT_UNDEFINED,
                                                                                                                              VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
                                                                                                                             ),
                                                                             VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                                                                            )],
                                         [],
                                         fVulkanRenderPass.AddAttachmentReference(fVulkanRenderPass.AddAttachmentDescription(0,
                                                                                                                             pvApplication.VulkanDepthImageFormat,
                                                                                                                             VK_SAMPLE_COUNT_1_BIT,
                                                                                                                             VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                                                                                             VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                             VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                                                                             VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                             VK_IMAGE_LAYOUT_UNDEFINED,
                                                                                                                             VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                                                                                            ),
                                                                                  VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                                                 ),
                                         []);
 fVulkanRenderPass.AddSubpassDependency(VK_SUBPASS_EXTERNAL,
                                        0,
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                        TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT),
                                        TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
                                        TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT));
 fVulkanRenderPass.AddSubpassDependency(0,
                                        VK_SUBPASS_EXTERNAL,
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                        TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
                                        TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT),
                                        TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT));
 fVulkanRenderPass.Initialize;

 fVulkanRenderPass.ClearValues[0].color.float32[0]:=0.0;
 fVulkanRenderPass.ClearValues[0].color.float32[1]:=0.0;
 fVulkanRenderPass.ClearValues[0].color.float32[2]:=0.0;
 fVulkanRenderPass.ClearValues[0].color.float32[3]:=1.0;

 // fullscreen textured-quad pipeline (present path A), built against the swapchain render pass
 FreeAndNil(fGraphicsPipeline);
 fGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                     pvApplication.VulkanPipelineCache,
                                                     0,
                                                     [],
                                                     fPipelineLayout,
                                                     fVulkanRenderPass,
                                                     0,
                                                     nil,
                                                     0);
 fGraphicsPipeline.AddStage(fVertexShaderStage);
 fGraphicsPipeline.AddStage(fFragmentShaderStage);
 fGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;
 fGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,pvApplication.VulkanSwapChain.Width,pvApplication.VulkanSwapChain.Height,0.0,1.0);
 fGraphicsPipeline.ViewPortState.AddScissor(0,0,pvApplication.VulkanSwapChain.Width,pvApplication.VulkanSwapChain.Height);
 // viewport + scissor are set per-frame (Draw) to the aspect-fit, centred letterbox rect of the video in the window
 fGraphicsPipeline.DynamicState.AddDynamicState(TVkDynamicState(VK_DYNAMIC_STATE_VIEWPORT));
 fGraphicsPipeline.DynamicState.AddDynamicState(TVkDynamicState(VK_DYNAMIC_STATE_SCISSOR));
 fGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
 fGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
 fGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
 fGraphicsPipeline.RasterizationState.LineWidth:=1.0;
 fGraphicsPipeline.MultisampleState.RasterizationSamples:=VK_SAMPLE_COUNT_1_BIT;
 fGraphicsPipeline.ColorBlendState.LogicOpEnable:=false;
 fGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
                                                                VK_BLEND_FACTOR_SRC_ALPHA,
                                                                VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                VK_BLEND_OP_ADD,
                                                                VK_BLEND_FACTOR_SRC_ALPHA,
                                                                VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                VK_BLEND_OP_ADD,
                                                                TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));
 fGraphicsPipeline.DepthStencilState.DepthTestEnable:=false;
 fGraphicsPipeline.DepthStencilState.DepthWriteEnable:=false;
 fGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;
 fGraphicsPipeline.Initialize;
 fGraphicsPipeline.FreeMemory;

end;

procedure TScreenMain.BeforeDestroySwapChain;
begin
 FreeAndNil(fGraphicsPipeline);
 FreeAndNil(fVulkanRenderPass);
 inherited BeforeDestroySwapChain;
end;

function TScreenMain.CanBeParallelProcessed:boolean;
begin
 result:=false; // F2a: serialize Update (DecodeTime) and Draw (Decode) over one set of staging buffers (double-buffering = later)
end;

function TScreenMain.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=false;
 if aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down then begin
  case aKeyEvent.KeyCode of
   KEYCODE_SPACE:begin
    fPaused:=not fPaused;
    if assigned(fPlayer) and fPlayer.HasAudio then begin
     if fPaused then begin
      fPlayer.PauseAudio; // freezes the master clock -> the video freezes too
     end else begin
      fPlayer.ResumeAudio;
     end;
    end;
    result:=true;
   end;
   KEYCODE_B:begin
    fPresentBlit:=not fPresentBlit; // toggle present path A <-> B
    result:=true;
   end;
   KEYCODE_G:begin
    fBlendBackgroundMode:=1-fBlendBackgroundMode; // toggle present path A composite background: checkerboard <-> solid color
    result:=true;
   end;
   KEYCODE_R:begin
    // restart from the beginning by SEEKING - reuse the player + its GPU resources (no free/recreate, no device wait)
    if assigned(fPlayer) then begin
     fPlaybackTime:=0.0;
     fPlayer.Restart;
    end;
    result:=true;
   end;
  end;
 end;
 if not result then begin
  result:=inherited KeyEvent(aKeyEvent);
 end;
end;

procedure TScreenMain.Update(const aDeltaTime:TpvDouble);
var DecTimeT0,DecTimeUS:TpvInt64;
begin
 inherited Update(aDeltaTime);

 // Deferred, one-frame-delayed player creation (see Show): wait until Draw has put one black frame on screen, THEN run
 // the ~0.5 s CreatePlayer, so the compile blocks over black instead of over the undefined surface.
 if fPlayerInitPending then begin
  if not fBlackFramePresented then begin
   exit; // no player yet and no black frame shown yet -> let Draw present black first
  end;
  CreatePlayer;
  BindPlayerToDescriptor;
  fPlayerInitPending:=false;
 end;

 if assigned(fPlayer) then begin
  if fAudioStartPending then begin
   fPlaybackTime:=0.0; // pre-roll: hold on frame 0 (decode + pipeline warmup over black) until Draw starts the audio
  end else if fPlayer.HasAudio and (not fPlayer.AudioFinished) then begin
   fPlaybackTime:=fPlayer.MasterClockSeconds; // A/V sync: the audio playback clock drives the video while audio plays (freezes on pause)
  end else if (not fPaused) and (fPlayer.Duration>0.0) and (fPlaybackTime<fPlayer.Duration) then begin
   // no audio, OR the audio finished before the video: advance on the wall clock so the video still plays out to its
   // end instead of freezing mid-stream; clamps at the end (hold the last frame until the window is closed).
   fPlaybackTime:=fPlaybackTime+aDeltaTime;
  end;
  if GetEnvironmentVariable('FVD_DECTIME')='1' then begin // CPU DecodeTime cost diagnostic (worst-case per frame vs 33.3 ms)
   DecTimeT0:=pvApplication.HighResolutionTimer.GetTime;
   fPlayer.DecodeTime(fPlaybackTime);
   DecTimeUS:=pvApplication.HighResolutionTimer.ToMicroseconds(pvApplication.HighResolutionTimer.GetTime-DecTimeT0);
   inc(fDecTimeAccumUS,DecTimeUS);
   if DecTimeUS>fDecTimeMaxUS then begin
    fDecTimeMaxUS:=DecTimeUS;
   end;
   inc(fDecTimeCount);
   if fDecTimeCount>=60 then begin
    writeln(Format('  dectime: avg %.2f ms / MAX %.2f ms over %d ticks (realtime budget 33.3 ms)',
                   [(fDecTimeAccumUS/fDecTimeCount)/1000.0,fDecTimeMaxUS/1000.0,fDecTimeCount]));
    fDecTimeAccumUS:=0;
    fDecTimeMaxUS:=0;
    fDecTimeCount:=0;
   end;
  end else begin
   fPlayer.DecodeTime(fPlaybackTime); // CPU advance: stage the frame this time maps to (idempotent within a tick)
  end;
  if GetEnvironmentVariable('FVD_AVLOG')='1' then begin // A/V sync diagnostic: master clock vs displayed frame
   inc(fAVLogTick);
   if (fAVLogTick mod 30)=0 then begin
    writeln(Format('  av: clock=%.3fs  frame=%d  (expected ~%.0f)',[fPlaybackTime,fPlayer.CurrentFrameIndex,fPlaybackTime*fPlayer.FrameRate]));
   end;
  end;
 end;
end;

procedure TScreenMain.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
var CommandBuffer:TpvVulkanCommandBuffer;
    WaitStage:TVkPipelineStageFlags;
    HasFrame:boolean;
    Barrier:TVkImageMemoryBarrier;
    ClearColor:TVkClearColorValue;
    ClearRange:TVkImageSubresourceRange;
    FitX,FitY,FitW,FitH:TpvInt32;
    Viewport:TVkViewport;
    ScissorRect:TVkRect2D;
    CompositePush:TFVDCompositePush;
begin
 inherited Draw(aSwapChainImageIndex,aWaitSemaphore,nil);

 if not assigned(fVulkanRenderPass) then begin
  exit;
 end;

 CommandBuffer:=fVulkanRenderCommandBuffers[pvApplication.DrawInFlightFrameIndex,aSwapChainImageIndex];
 CommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));

 // GPU decode the frame Update staged (leaves the decoded image in TRANSFER_SRC_OPTIMAL when it records one)
 if assigned(fPlayer) and fPlayer.Decode(CommandBuffer) then begin
  fOutputImageLayout:=VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
  if fAudioStartPending then begin
   // first frame is decoded -> start the audio clock now so audio + video begin together (no startup lag)
   fPlayer.StartAudio;
   fAudioStartPending:=false;
  end;
 end;

 // No valid frame to show when: the player does not exist yet (deferred creation, see Show/Update) or it has not decoded
 // its first frame. Sampling / blitting the OutputImage then shows undefined memory (a pale frame), so present plain
 // black instead. The first such black frame also releases the deferred CreatePlayer (fBlackFramePresented).
 HasFrame:=assigned(fPlayer) and (fPlayer.CurrentFrameIndex>=0);
 if not assigned(fPlayer) then begin
  fBlackFramePresented:=true;
 end;

 if fPresentBlit then begin

  if HasFrame then begin
   // present path B: blit the decoded image straight into the swapchain image
   TransitionOutputImage(CommandBuffer,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                         TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT));
   fPlayer.BlitLastDecodedFrame(CommandBuffer,
                                pvApplication.VulkanSwapChain.Images[aSwapChainImageIndex],
                                pvApplication.VulkanSwapChain.Width,pvApplication.VulkanSwapChain.Height,
                                VK_IMAGE_LAYOUT_UNDEFINED,VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
                                true); // aspect-fit + centre (letterbox), bars cleared black
  end else begin
   // no decoded frame yet -> clear the swapchain image to black
   FillChar(ClearColor,SizeOf(ClearColor),#0);
   ClearColor.float32[3]:=1.0;
   FillChar(ClearRange,SizeOf(ClearRange),#0);
   ClearRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
   ClearRange.levelCount:=1;
   ClearRange.layerCount:=1;
   FillChar(Barrier,SizeOf(Barrier),#0);
   Barrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
   Barrier.srcAccessMask:=0;
   Barrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT);
   Barrier.oldLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
   Barrier.newLayout:=VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
   Barrier.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
   Barrier.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
   Barrier.image:=pvApplication.VulkanSwapChain.Images[aSwapChainImageIndex].Handle;
   Barrier.subresourceRange:=ClearRange;
   CommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),0,0,nil,0,nil,1,@Barrier);
   CommandBuffer.CmdClearColorImage(pvApplication.VulkanSwapChain.Images[aSwapChainImageIndex].Handle,VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,@ClearColor,1,@ClearRange);
   Barrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT);
   Barrier.dstAccessMask:=0;
   Barrier.oldLayout:=VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
   Barrier.newLayout:=VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
   CommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),0,0,nil,0,nil,1,@Barrier);
  end;
  WaitStage:=TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT);

 end else begin

  // present path A: the render pass clears the swapchain to black; only sample the decoded image when one exists
  if HasFrame then begin
   TransitionOutputImage(CommandBuffer,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                         TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                         TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT));
  end;
  fVulkanRenderPass.BeginRenderPass(CommandBuffer,
                                    pvApplication.VulkanFrameBuffers[aSwapChainImageIndex],
                                    VK_SUBPASS_CONTENTS_INLINE,
                                    0,0,
                                    pvApplication.VulkanSwapChain.Width,pvApplication.VulkanSwapChain.Height);
  if HasFrame then begin
   // letterbox: the render pass already cleared the swapchain to black; draw the fullscreen quad only into the
   // aspect-fit, centred rect (dynamic viewport + scissor) so the bars stay black instead of stretching the video.
   LetterboxRect(pvApplication.VulkanSwapChain.Width,pvApplication.VulkanSwapChain.Height,FitX,FitY,FitW,FitH);
   Viewport.x:=FitX;
   Viewport.y:=FitY;
   Viewport.width:=FitW;
   Viewport.height:=FitH;
   Viewport.minDepth:=0.0;
   Viewport.maxDepth:=1.0;
   CommandBuffer.CmdSetViewport(0,1,@Viewport);
   ScissorRect.offset.x:=FitX;
   ScissorRect.offset.y:=FitY;
   ScissorRect.extent.width:=FitW;
   ScissorRect.extent.height:=FitH;
   CommandBuffer.CmdSetScissor(0,1,@ScissorRect);
   CommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fGraphicsPipeline.Handle);
   CommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,fPipelineLayout.Handle,0,1,@fDescriptorSet.Handle,0,nil);
   // composite params: the background ('G' toggles checker<->color) + the stream's premultiplied-alpha flag. For an
   // opaque stream (A=1) the shader shows the video unchanged regardless, so this is harmless for non-alpha streams.
   CompositePush.Mode:=fBlendBackgroundMode;
   if fPlayer.AlphaPremultiplied then begin
    CompositePush.Premultiplied:=1;
   end else begin
    CompositePush.Premultiplied:=0;
   end;
   CompositePush.CheckerSize:=16.0;
   CompositePush.Pad:=0.0;
   CompositePush.SolidColor[0]:=0.10; // background color for mode 1 (a muted blue-grey)
   CompositePush.SolidColor[1]:=0.12;
   CompositePush.SolidColor[2]:=0.18;
   CompositePush.SolidColor[3]:=1.0;
   CommandBuffer.CmdPushConstants(fPipelineLayout.Handle,TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),0,SizeOf(TFVDCompositePush),@CompositePush);
   CommandBuffer.CmdDraw(3,1,0,0);
  end;
  fVulkanRenderPass.EndRenderPass(CommandBuffer);
  WaitStage:=TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT);

 end;

 CommandBuffer.EndRecording;
 CommandBuffer.Execute(pvApplication.VulkanDevice.GraphicsQueue,
                       WaitStage,
                       aWaitSemaphore,
                       fVulkanRenderSemaphores[pvApplication.DrawInFlightFrameIndex],
                       aWaitFence,
                       false);
 aWaitSemaphore:=fVulkanRenderSemaphores[pvApplication.DrawInFlightFrameIndex];

end;

end.
