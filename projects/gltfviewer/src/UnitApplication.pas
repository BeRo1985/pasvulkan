unit UnitApplication;
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

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasDblStrUtils,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Resources,
     PasVulkan.VirtualReality,
     PasVulkan.Scene3D.Renderer.Globals;

const ApplicationTag='gltftest';      

type TApplication=class(TpvApplication)
      public
      private
       fVirtualReality:TpvVirtualReality;
       fForceNoVSync:boolean;
       fMaxMSAA:TpvInt32;
       fMaxShadowMSAA:TpvInt32;
       fShadowMapSize:TpvInt32;
       fTransparencyMode:TpvScene3DRendererTransparencyMode;
       fAntialiasingMode:TpvScene3DRendererAntialiasingMode;
       fDepthOfFieldMode:TpvScene3DRendererDepthOfFieldMode;
       fShadowMode:TpvScene3DRendererShadowMode;
       fLensMode:TpvScene3DRendererLensMode;
       fResamplingMode:TpvScene3DRendererResamplingMode;
       fRCASSharpness:TpvDouble;
       fAIUpscaleMode:TpvScene3DRendererAIUpscaleMode;
       fAIUpscaleQuality:TpvScene3DRendererAIUpscaleQuality;
       fTemporalUpscaler:TpvScene3DRendererTemporalUpscaler;
       fTemporalUpscalerQuality:TpvScene3DRendererTemporalUpscalerQuality;
       fTemporalUpscalerSharpness:TpvDouble;
       fTemporalUpscalerDebugView:boolean;
       fFrameGenerationProbe:boolean;
       fFrameGeneration:TpvScene3DRendererFrameGeneration;
       fCountGeneratedFrames:TpvInt32;
       fFrameGenerationDebugView:boolean;
       fFrameGenerationMinimumFrameRate:TpvFloat;
       fCountSwapChainImages:TpvInt32;
       fForcedPresentMode:TpvInt32;
       fSizeFactor:TpvDouble;
       fTransparentColorKey:boolean;
       fMakeScreenshotJPEG:boolean;
       fMakeScreenshotPNG:boolean;
       fMakeScreenshotQOI:boolean;
       fScreenshotPNGFileName:String;
       fAutoScreenshotTime:TpvDouble;
       fAutoScreenshotElapsed:TpvDouble;
       fAutoScreenshotRequested:boolean;
       fAutoScreenshotWritten:boolean;
       // --auto-screenshot-frames: that many pictures of frames in a row instead of one, with _0, _1 and so
       // on before the extension. Each picture asks for the next one once it is written, in Draw.
       fAutoScreenshotCountFrames:TpvInt32;
       fAutoScreenshotFrameIndex:TpvInt32;
       fAutoOrbitSpeed:TpvDouble;
       function GetAutoScreenshotFileName(const aFrameIndex:TpvInt32):String;
      public
       constructor Create; override;
       destructor Destroy; override;
       procedure SetupVulkanInstance(const aVulkanInstance:TpvVulkanInstance); override;
       procedure ChooseVulkanPhysicalDevice(var aVulkanPhysicalDevice:TpvVulkanPhysicalDevice); override;
       procedure SetupVulkanDevice(const aVulkanDevice:TpvVulkanDevice); override;
       procedure Setup; override;
       procedure Start; override;
       procedure Stop; override;
       procedure Load; override;
       procedure Unload; override;
       procedure AfterCreateSwapChain; override;
       procedure BeforeDestroySwapChain; override;
       procedure Resume; override;
       procedure Pause; override;
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;
       procedure Check(const aDeltaTime:TpvDouble); override;
       procedure Update(const aDeltaTime:TpvDouble); override;
       procedure BeginFrame(const aDeltaTime:TpvDouble); override;
       procedure Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil); override;
       procedure FinishFrame(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil); override;
       procedure PostPresent(const aSwapChainImageIndex:TpvInt32); override;
      published
       property VirtualReality:TpvVirtualReality read fVirtualReality;
       property MaxMSAA:TpvInt32 read fMaxMSAA;
       property MaxShadowMSAA:TpvInt32 read fMaxShadowMSAA;
       property ShadowMapSize:TpvInt32 read fShadowMapSize;
       property TransparencyMode:TpvScene3DRendererTransparencyMode read fTransparencyMode;
       property AntialiasingMode:TpvScene3DRendererAntialiasingMode read fAntialiasingMode;
       property DepthOfFieldMode:TpvScene3DRendererDepthOfFieldMode read fDepthOfFieldMode;
       property ShadowMode:TpvScene3DRendererShadowMode read fShadowMode;
       property LensMode:TpvScene3DRendererLensMode read fLensMode;
       property ResamplingMode:TpvScene3DRendererResamplingMode read fResamplingMode;
       property RCASSharpness:TpvDouble read fRCASSharpness;
       property AIUpscaleMode:TpvScene3DRendererAIUpscaleMode read fAIUpscaleMode;
       property AIUpscaleQuality:TpvScene3DRendererAIUpscaleQuality read fAIUpscaleQuality;
       property TemporalUpscaler:TpvScene3DRendererTemporalUpscaler read fTemporalUpscaler;
       property TemporalUpscalerQuality:TpvScene3DRendererTemporalUpscalerQuality read fTemporalUpscalerQuality;
       property TemporalUpscalerSharpness:TpvDouble read fTemporalUpscalerSharpness;
       property TemporalUpscalerDebugView:boolean read fTemporalUpscalerDebugView;
       property FrameGenerationProbe:boolean read fFrameGenerationProbe;
       property FrameGeneration:TpvScene3DRendererFrameGeneration read fFrameGeneration;
       property CountGeneratedFrames:TpvInt32 read fCountGeneratedFrames;
       property FrameGenerationDebugView:boolean read fFrameGenerationDebugView;
       property FrameGenerationMinimumFrameRate:TpvFloat read fFrameGenerationMinimumFrameRate;
       property SizeFactor:TpvDouble read fSizeFactor;
       property AutoOrbitSpeed:TpvDouble read fAutoOrbitSpeed;
       property TransparentColorKey:boolean read fTransparentColorKey;
     end;

var Application:TApplication=nil;

    GLTFFileName:UTF8String='';

implementation

uses PasVulkan.Scene3D.Renderer,
     UnitScreenMain;

constructor TApplication.Create;
var VirtualRealityMode:TpvVirtualReality.TMode;
{$if not (defined(Android) or defined(iOS))}
    Index:TpvInt32;
    OriginalParameter,Parameter:String;
    OK:TPasDblStrUtilsBoolean;
    LowLatencyModeGiven:boolean;
{$ifend}
begin
 inherited Create;
 Application:=self;
 PasVulkan.Resources.AllowExternalResources:=true;
 fMakeScreenshotJPEG:=false;
 fMakeScreenshotPNG:=false;
 fMakeScreenshotQOI:=false;
 fScreenshotPNGFileName:='screenshot.png';
 fAutoScreenshotTime:=-1.0;
 fAutoScreenshotElapsed:=0.0;
 fAutoScreenshotRequested:=false;
 fAutoScreenshotWritten:=false;
 fAutoScreenshotCountFrames:=1;
 fAutoScreenshotFrameIndex:=0;
 fAutoOrbitSpeed:=0.0;
 ExclusiveFullScreenMode:=TpvVulkanExclusiveFullScreenMode.Allowed;
 fForceNoVSync:=false;
 VulkanNVIDIAAfterMath:=false;
 //WaitOnPreviousFrame:=true;
 fMaxMSAA:=0;
 fMaxShadowMSAA:=1;
 fShadowMapSize:=2048;
 fTransparencyMode:=TpvScene3DRendererTransparencyMode.Auto;
 fAntialiasingMode:=TpvScene3DRendererAntialiasingMode.Auto;
 fDepthOfFieldMode:=TpvScene3DRendererDepthOfFieldMode.None;
 fShadowMode:=TpvScene3DRendererShadowMode.Auto;
 fLensMode:=TpvScene3DRendererLensMode.Auto;
 fResamplingMode:=TpvScene3DRendererResamplingMode.Lanczos;
 fRCASSharpness:=0.25;
 fAIUpscaleMode:=TpvScene3DRendererAIUpscaleMode.None;
 fAIUpscaleQuality:=TpvScene3DRendererAIUpscaleQuality.Low;
 fTemporalUpscaler:=TpvScene3DRendererTemporalUpscaler.None;
 fTemporalUpscalerQuality:=TpvScene3DRendererTemporalUpscalerQuality.Quality;
 fTemporalUpscalerSharpness:=0.0;
 fTemporalUpscalerDebugView:=false;
 fFrameGenerationProbe:=false;
 fFrameGeneration:=TpvScene3DRendererFrameGeneration.None;
 fCountGeneratedFrames:=1;
 fFrameGenerationDebugView:=false;
 fFrameGenerationMinimumFrameRate:=0.0;
 fCountSwapChainImages:=0;
 fForcedPresentMode:=-1;
 fSizeFactor:=1.0;
 fTransparentColorKey:=false;
 VirtualRealityMode:=TpvVirtualReality.TMode.Disabled;
 AcceptDragDropFiles:=true;
{$if not (defined(Android) or defined(iOS))}
 LowLatencyModeGiven:=false;
 Index:=1;
 while Index<=ParamCount do begin
  OriginalParameter:=ParamStr(Index);
  Parameter:=OriginalParameter;
  inc(Index);
  if (Parameter='--openvr') or
     (Parameter='/openvr') then begin
   VirtualRealityMode:=TpvVirtualReality.TMode.OpenVR;
  end else if (Parameter='--fakedvr') or
              (Parameter='/fakedvr') then begin
   VirtualRealityMode:=TpvVirtualReality.TMode.Faked;
  end else if (Parameter='--force-no-vsync') or
              (Parameter='/force-no-vsync') then begin
   fForceNoVSync:=true;
  end else if (Parameter='--nvidia-aftermath') or
              (Parameter='/nvidia-aftermath') then begin
   VulkanNVIDIAAfterMath:=true;
  end else if (Parameter='--prefer-dgpus') or
              (Parameter='/prefer-dgpus') then begin
   VulkanPreferDedicatedGPUs:=true;
  end else if (Parameter='--prefer-igpus') or
              (Parameter='/prefer-igpus') then begin
   VulkanPreferDedicatedGPUs:=false;
{ end else if (Parameter='--flush-update-data') or
              (Parameter='/flush-update-data') then begin
   FlushUpdateData:=true; //}
  end else if (Parameter='--max-msaa') or
              (Parameter='/max-msaa') then begin
   if Index<=ParamCount then begin
    fMaxMSAA:=StrToIntDef(ParamStr(Index),0);
    inc(Index);
   end;
  end else if (Parameter='--max-shadow-msaa') or
              (Parameter='/max-shadow-msaa') then begin
   if Index<=ParamCount then begin
    fMaxShadowMSAA:=StrToIntDef(ParamStr(Index),0);
    inc(Index);
   end;
  end else if (Parameter='--shadow-map-size') or
              (Parameter='/shadow-map-size') then begin
   if Index<=ParamCount then begin
    fShadowMapSize:=StrToIntDef(ParamStr(Index),0);
    inc(Index);
   end;
  end else if (Parameter='--transparency-mode') or
              (Parameter='/transparency-mode') then begin
   if Index<=ParamCount then begin
    Parameter:=LowerCase(trim(ParamStr(Index)));
    inc(Index);
    if Parameter='direct' then begin
     fTransparencyMode:=TpvScene3DRendererTransparencyMode.Direct;
    end else if Parameter='spinlockoit' then begin
     fTransparencyMode:=TpvScene3DRendererTransparencyMode.SPINLOCKOIT;
    end else if Parameter='interlockoit' then begin
     fTransparencyMode:=TpvScene3DRendererTransparencyMode.INTERLOCKOIT;
    end else if Parameter='loopoit' then begin
     fTransparencyMode:=TpvScene3DRendererTransparencyMode.LOOPOIT;
    end else if Parameter='wboit' then begin
     fTransparencyMode:=TpvScene3DRendererTransparencyMode.WBOIT;
    end else if Parameter='mboit' then begin
     fTransparencyMode:=TpvScene3DRendererTransparencyMode.MBOIT;
    end else if Parameter='spinlockdfaoit' then begin
     fTransparencyMode:=TpvScene3DRendererTransparencyMode.SPINLOCKDFAOIT;
    end else if Parameter='interlockdfaoit' then begin
     fTransparencyMode:=TpvScene3DRendererTransparencyMode.INTERLOCKDFAOIT;
    end else begin
     fTransparencyMode:=TpvScene3DRendererTransparencyMode.Auto;
    end;
   end;
  end else if (Parameter='--antialiasing-mode') or
              (Parameter='/antialiasing-mode') then begin
   if Index<=ParamCount then begin
    Parameter:=LowerCase(trim(ParamStr(Index)));
    inc(Index);
    if Parameter='none' then begin
     fAntialiasingMode:=TpvScene3DRendererAntialiasingMode.None;
    end else if Parameter='dsaa' then begin
     fAntialiasingMode:=TpvScene3DRendererAntialiasingMode.DSAA;
    end else if Parameter='fxaa' then begin
     fAntialiasingMode:=TpvScene3DRendererAntialiasingMode.FXAA;
    end else if Parameter='smaa' then begin
     fAntialiasingMode:=TpvScene3DRendererAntialiasingMode.SMAA;
    end else if Parameter='msaa' then begin
     fAntialiasingMode:=TpvScene3DRendererAntialiasingMode.MSAA;
    end else if Parameter='msaasmaa' then begin
     fAntialiasingMode:=TpvScene3DRendererAntialiasingMode.MSAASMAA;
    end else if Parameter='taa' then begin
     fAntialiasingMode:=TpvScene3DRendererAntialiasingMode.TAA;
    end else begin
     fTransparencyMode:=TpvScene3DRendererTransparencyMode.Auto;
    end;
   end;
  end else if (Parameter='--shadow-mode') or
              (Parameter='/shadow-mode') then begin
   if Index<=ParamCount then begin
    Parameter:=LowerCase(trim(ParamStr(Index)));
    inc(Index);
    if Parameter='none' then begin
     fShadowMode:=TpvScene3DRendererShadowMode.None;
    end else if Parameter='pcf' then begin
     fShadowMode:=TpvScene3DRendererShadowMode.PCF;
    end else if Parameter='dpcf' then begin
     fShadowMode:=TpvScene3DRendererShadowMode.DPCF;
    end else if Parameter='pcss' then begin
     fShadowMode:=TpvScene3DRendererShadowMode.PCSS;
    end else if Parameter='msm' then begin
     fShadowMode:=TpvScene3DRendererShadowMode.MSM;
    end else begin
     fShadowMode:=TpvScene3DRendererShadowMode.Auto;
    end;
   end;
  end else if (Parameter='--depth-of-field-mode') or
              (Parameter='/depth-of-field-mode') then begin
   if Index<=ParamCount then begin
    Parameter:=LowerCase(trim(ParamStr(Index)));
    inc(Index);
    if Parameter='none' then begin
     fDepthOfFieldMode:=TpvScene3DRendererDepthOfFieldMode.None;
    end else if Parameter='halfresseparatenearfar' then begin
     fDepthOfFieldMode:=TpvScene3DRendererDepthOfFieldMode.HalfResSeparateNearFar;
    end else if Parameter='halfresbruteforce' then begin
     fDepthOfFieldMode:=TpvScene3DRendererDepthOfFieldMode.HalfResBruteforce;
    end else if Parameter='fullreshexagon' then begin
     fDepthOfFieldMode:=TpvScene3DRendererDepthOfFieldMode.FullResHexagon;
    end else if Parameter='fullresbruteforce' then begin
     fDepthOfFieldMode:=TpvScene3DRendererDepthOfFieldMode.FullResBruteforce;
    end else begin
     fDepthOfFieldMode:=TpvScene3DRendererDepthOfFieldMode.Auto;
    end;
   end;
  end else if (Parameter='--lens-mode') or
              (Parameter='/lens-mode') then begin
   if Index<=ParamCount then begin
    Parameter:=LowerCase(trim(ParamStr(Index)));
    inc(Index);
    if Parameter='none' then begin
     fLensMode:=TpvScene3DRendererLensMode.None;
    end else if Parameter='downupsample' then begin
     fLensMode:=TpvScene3DRendererLensMode.DownUpsample;
    end else begin
     fLensMode:=TpvScene3DRendererLensMode.Auto;
    end;
   end;
  end else if (Parameter='--transparent-color-key') or
              (Parameter='/transparent-color-key') then begin
   fTransparentColorKey:=true;
  end else if (Parameter='--resampling-mode') or
              (Parameter='/resampling-mode') then begin
   if Index<=ParamCount then begin
    fResamplingMode.FromString(ParamStr(Index));
    inc(Index);
   end;
  end else if (Parameter='--rcas-sharpness') or
              (Parameter='/rcas-sharpness') then begin
   if Index<=ParamCount then begin
    OK:=false;
    fRCASSharpness:=ConvertStringToDouble(ParamStr(Index),rmNearest,@OK);
    if not OK then begin
     fRCASSharpness:=0.25;
    end;
    inc(Index);
   end;
  end else if (Parameter='--ai-upscale-mode') or
              (Parameter='/ai-upscale-mode') then begin
   if Index<=ParamCount then begin
    fAIUpscaleMode.FromString(ParamStr(Index));
    inc(Index);
   end;
  end else if (Parameter='--ai-upscale-quality') or
              (Parameter='/ai-upscale-quality') then begin
   if Index<=ParamCount then begin
    fAIUpscaleQuality.FromString(ParamStr(Index));
    inc(Index);
   end;
  end else if (Parameter='--temporal-upscaler') or
              (Parameter='/temporal-upscaler') or
              (Parameter='--upscaler') or
              (Parameter='/upscaler') then begin
   if Index<=ParamCount then begin
    fTemporalUpscaler.FromString(ParamStr(Index));
    inc(Index);
   end;
  end else if (Parameter='--temporal-upscaler-quality') or
              (Parameter='/temporal-upscaler-quality') or
              (Parameter='--upscale-quality') or
              (Parameter='/upscale-quality') then begin
   if Index<=ParamCount then begin
    fTemporalUpscalerQuality.FromString(ParamStr(Index));
    inc(Index);
   end;
  end else if (Parameter='--temporal-upscaler-sharpness') or
              (Parameter='/temporal-upscaler-sharpness') or
              (Parameter='--upscale-sharpness') or
              (Parameter='/upscale-sharpness') then begin
   if Index<=ParamCount then begin
    OK:=false;
    fTemporalUpscalerSharpness:=ConvertStringToDouble(ParamStr(Index),rmNearest,@OK);
    if not OK then begin
     fTemporalUpscalerSharpness:=0.0;
    end;
    fTemporalUpscalerSharpness:=Min(Max(fTemporalUpscalerSharpness,0.0),1.0);
    inc(Index);
   end;
  end else if (Parameter='--temporal-upscaler-debug-view') or
              (Parameter='/temporal-upscaler-debug-view') or
              (Parameter='--upscale-debug') or
              (Parameter='/upscale-debug') then begin
   // The upscaler's own debug view over the picture, see TpvScene3DRenderer.TemporalUpscalerDebugView
   fTemporalUpscalerDebugView:=true;
  end else if (Parameter='--frame-generation-probe') or
              (Parameter='/frame-generation-probe') or
              (Parameter='--framegen-probe') or
              (Parameter='/framegen-probe') then begin
   // Whether frame generation runs here at all, see TpvScene3DRenderer.FrameGenerationProbe; it needs a
   // temporal upscaler to sit behind
   fFrameGenerationProbe:=true;
  end else if (Parameter='--frame-generation') or
              (Parameter='/frame-generation') or
              (Parameter='--framegen') or
              (Parameter='/framegen') then begin
   if Index<=ParamCount then begin
    fFrameGeneration.FromString(ParamStr(Index));
    inc(Index);
   end;
  end else if (Parameter='--frame-generation-count') or
              (Parameter='/frame-generation-count') or
              (Parameter='--framegen-count') or
              (Parameter='/framegen-count') then begin
   if Index<=ParamCount then begin
    fCountGeneratedFrames:=Max(1,StrToIntDef(ParamStr(Index),1));
    inc(Index);
   end;
  end else if (Parameter='--frame-generation-debug') or
              (Parameter='/frame-generation-debug') or
              (Parameter='--framegen-debug') or
              (Parameter='/framegen-debug') then begin
   // Generated frames tinted and numbered, see TpvScene3DRenderer.FrameGenerationDebugView
   fFrameGenerationDebugView:=true;
  end else if (Parameter='--frame-generation-min-fps') or
              (Parameter='/frame-generation-min-fps') or
              (Parameter='--framegen-min-fps') or
              (Parameter='/framegen-min-fps') then begin
   // See TpvScene3DRenderer.FrameGenerationMinimumFrameRate
   if Index<=ParamCount then begin
    fFrameGenerationMinimumFrameRate:=Max(0,StrToIntDef(ParamStr(Index),0));
    inc(Index);
   end;
  end else if (Parameter='--frame-generation-pacing') or
              (Parameter='/frame-generation-pacing') or
              (Parameter='--framegen-pacing') or
              (Parameter='/framegen-pacing') then begin
   // How the presents of frame generation are spaced: auto, targettimes or cpu, see
   // TpvApplication.GeneratedFramePacing
   if Index<=ParamCount then begin
    if LowerCase(ParamStr(Index))='cpu' then begin
     GeneratedFramePacing:=TpvApplicationGeneratedFramePacing.CPU;
    end else if LowerCase(ParamStr(Index))='targettimes' then begin
     GeneratedFramePacing:=TpvApplicationGeneratedFramePacing.TargetTimes;
    end else begin
     GeneratedFramePacing:=TpvApplicationGeneratedFramePacing.Auto;
    end;
    inc(Index);
   end;
  end else if (Parameter='--present-stats') or
              (Parameter='/present-stats') then begin
   // The intervals between the presented frames in the log, see TpvApplication.PresentIntervalStatistics
   PresentIntervalStatistics:=true;
  end else if (Parameter='--max-fps') or
              (Parameter='/max-fps') then begin
   // An upper limit for the rendered frames per second, for trying frame generation at rates of all kinds
   if Index<=ParamCount then begin
    MaximumFramesPerSecond:=Max(0,StrToIntDef(ParamStr(Index),0));
    inc(Index);
   end;
  end else if (Parameter='--present-mode') or
              (Parameter='/present-mode') then begin
   // The present mode, over whatever the rest would pick: immediate, mailbox, fifo, fiforelaxed or
   // fifolatestready, for holding them against one another
   if Index<=ParamCount then begin
    if LowerCase(ParamStr(Index))='immediate' then begin
     fForcedPresentMode:=TpvInt32(TpvApplicationPresentMode.Immediate);
    end else if LowerCase(ParamStr(Index))='mailbox' then begin
     fForcedPresentMode:=TpvInt32(TpvApplicationPresentMode.Mailbox);
    end else if LowerCase(ParamStr(Index))='fifo' then begin
     fForcedPresentMode:=TpvInt32(TpvApplicationPresentMode.FIFO);
    end else if LowerCase(ParamStr(Index))='fiforelaxed' then begin
     fForcedPresentMode:=TpvInt32(TpvApplicationPresentMode.FIFORelaxed);
    end else if LowerCase(ParamStr(Index))='fifolatestready' then begin
     fForcedPresentMode:=TpvInt32(TpvApplicationPresentMode.FIFOLatestReady);
    end;
    inc(Index);
   end;
  end else if (Parameter='--swapchain-images') or
              (Parameter='/swapchain-images') then begin
   // How many swapchain images to ask for, at least as many as frame generation needs
   if Index<=ParamCount then begin
    fCountSwapChainImages:=Max(0,StrToIntDef(ParamStr(Index),0));
    inc(Index);
   end;
  end else if (Parameter='--low-latency') or
              (Parameter='/low-latency') then begin
   // The low latency mode: none, auto, reflex or antilag, see TpvApplication.LowLatencyMode
   if Index<=ParamCount then begin
    if LowerCase(ParamStr(Index))='reflex' then begin
     LowLatencyMode:=TpvApplicationLowLatencyMode.NVReflex;
    end else if LowerCase(ParamStr(Index))='antilag' then begin
     LowLatencyMode:=TpvApplicationLowLatencyMode.AMDAntiLag;
    end else if LowerCase(ParamStr(Index))='auto' then begin
     LowLatencyMode:=TpvApplicationLowLatencyMode.Auto;
    end else begin
     LowLatencyMode:=TpvApplicationLowLatencyMode.None;
    end;
    LowLatencyModeGiven:=true;
    inc(Index);
   end;
  end else if (Parameter='--auto-screenshot') or
              (Parameter='/auto-screenshot') then begin
   // For automated comparisons: after this many seconds a PNG screenshot is written and the viewer quits.
   if Index<=ParamCount then begin
    OK:=false;
    fAutoScreenshotTime:=ConvertStringToDouble(ParamStr(Index),rmNearest,@OK);
    if not OK then begin
     fAutoScreenshotTime:=-1.0;
    end;
    inc(Index);
   end;
  end else if (Parameter='--auto-orbit') or
              (Parameter='/auto-orbit') then begin
   // Lets the orbit camera turn by itself, in turns per second.
   if Index<=ParamCount then begin
    OK:=false;
    fAutoOrbitSpeed:=ConvertStringToDouble(ParamStr(Index),rmNearest,@OK);
    if not OK then begin
     fAutoOrbitSpeed:=0.0;
    end;
    inc(Index);
   end;
  end else if (Parameter='--auto-screenshot-file') or
              (Parameter='/auto-screenshot-file') then begin
   if Index<=ParamCount then begin
    fScreenshotPNGFileName:=ParamStr(Index);
    inc(Index);
   end;
  end else if (Parameter='--auto-screenshot-frames') or
              (Parameter='/auto-screenshot-frames') then begin
   // How many frames in a row --auto-screenshot takes, each a picture of its own, 1 to 64
   if Index<=ParamCount then begin
    fAutoScreenshotCountFrames:=Min(Max(StrToIntDef(trim(ParamStr(Index)),1),1),64);
    inc(Index);
   end;
  end else if (Parameter='--size-factor') or
              (Parameter='/size-factor') then begin
   if Index<=ParamCount then begin
    OK:=false;
    fSizeFactor:=ConvertStringToDouble(ParamStr(Index),rmNearest,@OK);
    if not OK then begin
     fSizeFactor:=1.0;
    end;
    inc(Index);
   end;
  end else if (Parameter='--loglevel') or
              (Parameter='/loglevel') or
              (Parameter='--crashlog') or
              (Parameter='/crashlog') or
              (Parameter='--breadcrumbtracefile') or
              (Parameter='/breadcrumbtracefile') or
              (Parameter='--recordinput') or
              (Parameter='/recordinput') or
              (Parameter='--replayinput') or
              (Parameter='/replayinput') or
              (Parameter='--hangseconds') or
              (Parameter='/hangseconds') or
              (Parameter='--hangreports') or
              (Parameter='/hangreports') or
              (Parameter='--hangshutdownseconds') or
              (Parameter='/hangshutdownseconds') then begin
   // Options the engine takes itself, together with a value, which must not end up as the file name
   if Index<=ParamCount then begin
    inc(Index);
   end;
  end else if (length(Parameter)>2) and (Parameter[1]='-') and (Parameter[2]='-') then begin
   // Further options the engine takes itself, such as --force-use-validation-layers, and no file name either
  end else begin
   GLTFFileName:=OriginalParameter;
  end;
 end;
 // DLSS frame generation goes with NVIDIA Reflex, unless asked otherwise.
 if (fFrameGeneration=TpvScene3DRendererFrameGeneration.DLSS) and not LowLatencyModeGiven then begin
  LowLatencyMode:=TpvApplicationLowLatencyMode.Auto;
 end;
{$ifend}
 if VirtualRealityMode=TpvVirtualReality.TMode.Disabled then begin
  fVirtualReality:=nil;
 end else begin
  fVirtualReality:=TpvVirtualReality.Create(VirtualRealityMode);
  fVirtualReality.ZNear:=0.1;
  fVirtualReality.ZFar:=-Infinity;
 end;
end;

// The name of the given picture of --auto-screenshot, with _0, _1 and so on before the extension when
// --auto-screenshot-frames asks for more than one.
function TApplication.GetAutoScreenshotFileName(const aFrameIndex:TpvInt32):String;
begin
 if fAutoScreenshotCountFrames>1 then begin
  result:=ChangeFileExt(fScreenshotPNGFileName,'')+'_'+IntToStr(aFrameIndex)+ExtractFileExt(fScreenshotPNGFileName);
 end else begin
  result:=fScreenshotPNGFileName;
 end;
end;

destructor TApplication.Destroy;
begin
 FreeAndNil(fVirtualReality);
 Application:=nil;
 inherited Destroy;
end;

procedure TApplication.SetupVulkanInstance(const aVulkanInstance:TpvVulkanInstance);
begin
 inherited SetupVulkanInstance(aVulkanInstance);
 if assigned(fVirtualReality) then begin
  fVirtualReality.CheckVulkanInstanceExtensions(aVulkanInstance);
  aVulkanInstance.EnabledExtensionNames.Duplicates:=TDuplicates.dupIgnore;
  aVulkanInstance.EnabledExtensionNames.AddStrings(fVirtualReality.RequiredVulkanInstanceExtensions);
 end;
end;

procedure TApplication.ChooseVulkanPhysicalDevice(var aVulkanPhysicalDevice:TpvVulkanPhysicalDevice);
var PhysicalDevice:TVkPhysicalDevice;
begin
 inherited ChooseVulkanPhysicalDevice(aVulkanPhysicalDevice);
 if assigned(fVirtualReality) and not (fVirtualReality.Mode in [TpvVirtualReality.TMode.Disabled,TpvVirtualReality.TMode.Faked]) then begin
  PhysicalDevice:=VK_NULL_HANDLE;
  fVirtualReality.ChooseVulkanPhysicalDevice(VulkanInstance,PhysicalDevice);
  pvApplication.VulkanPhysicalDeviceHandle:=PhysicalDevice;
 end;
end;

procedure TApplication.SetupVulkanDevice(const aVulkanDevice:TpvVulkanDevice);
begin
 inherited SetupVulkanDevice(aVulkanDevice);
 if assigned(fVirtualReality) then begin
  fVirtualReality.CheckVulkanDeviceExtensions(pvApplication.VulkanPhysicalDeviceHandle);
  aVulkanDevice.EnabledExtensionNames.Duplicates:=TDuplicates.dupIgnore;
  aVulkanDevice.EnabledExtensionNames.AddStrings(fVirtualReality.RequiredVulkanDeviceExtensions);
 end;
 TpvScene3DRenderer.SetupVulkanDevice(aVulkanDevice);
end;

procedure TApplication.Setup;
begin
 // Validation layers: --force-use-validation-layers, --sync-validation, --debug-labels, all handled
 // by TpvApplication.ParseCommandLine. Nothing is switched on here on its own any more.
 Title:='PasVulkan GLTF Viewer';
 PathName:='gltftest.pasvulkan';
 StartScreen:=TScreenMain;
 VisibleMouseCursor:=true;
 CatchMouse:=false;
 HideSystemBars:=true;
 AndroidSeparateMouseAndTouch:=true;
 UseAudio:=true;
 UpdateWaitsForGPU:=true;
 UseExtraUpdateThread:=false;
 SwapChainColorSpace:=TpvApplicationSwapChainColorSpace.SRGB;
//Blocking:=false;
//DesiredCountSwapChainImages:=2;
 DesiredCountInFlightFrames:=2;
 if fForceNoVSync or (assigned(fVirtualReality) and not (fVirtualReality.Mode in [TpvVirtualReality.TMode.Disabled,TpvVirtualReality.TMode.Faked])) then begin
  DesiredCountSwapChainImages:=2;
  PresentMode:=TpvApplicationPresentMode.Mailbox;
 end else begin
  PresentMode:=TpvApplicationPresentMode.FIFO;
 end;
 // Generated frames take swapchain images of their own, see TpvApplication.AcquireGeneratedFrameImage
 if fFrameGeneration<>TpvScene3DRendererFrameGeneration.None then begin
  DesiredCountSwapChainImages:=Max(DesiredCountSwapChainImages,3+fCountGeneratedFrames);
  // A display that cannot show twice as many frames as are rendered would hold the rendered frames to
  // half its refresh rate with FIFO, see TpvApplicationPresentMode.FIFOLatestReady.
  if PresentMode=TpvApplicationPresentMode.FIFO then begin
   PresentMode:=TpvApplicationPresentMode.FIFOLatestReady;
  end;
 end;
 if fCountSwapChainImages>0 then begin
  DesiredCountSwapChainImages:=Max(DesiredCountSwapChainImages,fCountSwapChainImages);
 end;
 if fForcedPresentMode>=0 then begin
  PresentMode:=TpvApplicationPresentMode(fForcedPresentMode);
 end;
// VulkanAPIVersion:=VK_API_VERSION_1_0;
 VulkanAPIVersion:=0;//VK_API_VERSION_1_0;
end;

procedure TApplication.Start;
begin
 inherited Start;
end;

procedure TApplication.Stop;
begin
 inherited Stop;
end;

procedure TApplication.Load;
begin

 if not VulkanMultiviewSupportEnabled then begin
  raise EpvVulkanException.Create('Missing Vulkan multi-view support');
 end;

 inherited Load;

 if assigned(fVirtualReality) then begin
  fVirtualReality.Load;
 end;

end;

procedure TApplication.Unload;
begin

 if assigned(fVirtualReality) then begin
  fVirtualReality.Unload;
 end;

 inherited Unload;

end;

procedure TApplication.AfterCreateSwapChain;
begin

 if assigned(fVirtualReality) then begin
  fVirtualReality.AfterCreateSwapChain;
 end;

 inherited AfterCreateSwapChain;

end;

procedure TApplication.BeforeDestroySwapChain;
begin

 inherited BeforeDestroySwapChain;

 if assigned(fVirtualReality) then begin
  fVirtualReality.BeforeDestroySwapChain;
 end;

end;

procedure TApplication.Resume;
begin
 inherited Resume;
end;

procedure TApplication.Pause;
begin
 inherited Pause;
end;

function TApplication.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=inherited KeyEvent(aKeyEvent);
 if aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down then begin
  case aKeyEvent.KeyCode of
   KEYCODE_F8:begin
    if assigned(VirtualReality) then begin
     VirtualReality.ResetOrientation;
    end;
   end;
   KEYCODE_F9:begin
    fMakeScreenshotQOI:=true;
   end;
   KEYCODE_F10:begin
    fMakeScreenshotJPEG:=true;
   end;
   KEYCODE_F11:begin
    fMakeScreenshotPNG:=true;
   end;
  end;
 end;
end;

procedure TApplication.Check(const aDeltaTime:TpvDouble);
begin
 if assigned(fVirtualReality) then begin
  fVirtualReality.Check(aDeltaTime);
 end;
 inherited Check(aDeltaTime);
end;

procedure TApplication.Update(const aDeltaTime:TpvDouble);
begin
 if assigned(fVirtualReality) then begin
  fVirtualReality.Update(aDeltaTime);
 end;
 if fAutoScreenshotTime>=0.0 then begin
  if fAutoScreenshotWritten then begin
   Terminate;
  end else if not fAutoScreenshotRequested then begin
   fAutoScreenshotElapsed:=fAutoScreenshotElapsed+aDeltaTime;
   if fAutoScreenshotElapsed>=fAutoScreenshotTime then begin
    fAutoScreenshotRequested:=true;
    fMakeScreenshotPNG:=true;
   end;
  end;
 end;
 inherited Update(aDeltaTime);
end;

procedure TApplication.BeginFrame(const aDeltaTime:TpvDouble);
begin
 if assigned(fVirtualReality) then begin
  fVirtualReality.BeginFrame(aDeltaTime);
 end;
 inherited BeginFrame(aDeltaTime);
end;

procedure TApplication.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
var Stream:TMemoryStream;
    GeneratedFrameIndex:TpvInt32;
    FileName:String;
begin
 if assigned(fVirtualReality) then begin
  inherited Draw(aSwapChainImageIndex,aWaitSemaphore,nil);
  fVirtualReality.Draw(aSwapChainImageIndex,DrawInFlightFrameIndex,aWaitSemaphore,aWaitFence);
 end else begin
  inherited Draw(aSwapChainImageIndex,aWaitSemaphore,aWaitFence);
 end;
 if fMakeScreenshotJPEG then begin
  fMakeScreenshotJPEG:=false;
  Stream:=TMemoryStream.Create;
  try
   VulkanSwapChain.SaveScreenshotAsJPEGToStream(Stream);
   try
    Stream.SaveToFile('screenshot.jpeg');
   except
   end;
  finally
   Stream.Free;
  end;
 end else if fMakeScreenshotPNG then begin
  fMakeScreenshotPNG:=false;
  Stream:=TMemoryStream.Create;
  try
   VulkanSwapChain.SaveScreenshotAsPNGToStream(Stream);
   try
    if fAutoScreenshotRequested then begin
     Stream.SaveToFile(GetAutoScreenshotFileName(fAutoScreenshotFrameIndex));
    end else begin
     Stream.SaveToFile(fScreenshotPNGFileName);
    end;
   except
   end;
  finally
   Stream.Free;
  end;
  // With frame generation, the frames generated ahead of this one as well, as <name>_generated<n>.<ext>
  for GeneratedFrameIndex:=0 to CountQueuedGeneratedFrames-1 do begin
   Stream:=TMemoryStream.Create;
   try
    VulkanSwapChain.SaveScreenshotAsPNGToStream(Stream,VulkanSwapChain.Images[GetQueuedGeneratedFrameImageIndex(GeneratedFrameIndex)]);
    try
     if fAutoScreenshotRequested then begin
      FileName:=GetAutoScreenshotFileName(fAutoScreenshotFrameIndex);
     end else begin
      FileName:=fScreenshotPNGFileName;
     end;
     Stream.SaveToFile(ChangeFileExt(FileName,'')+'_generated'+IntToStr(GeneratedFrameIndex+1)+ExtractFileExt(FileName));
    except
    end;
   finally
    Stream.Free;
   end;
  end;
  if fAutoScreenshotRequested then begin
   if (fAutoScreenshotFrameIndex+1)<fAutoScreenshotCountFrames then begin
    // The next frame in a row
    inc(fAutoScreenshotFrameIndex);
    fMakeScreenshotPNG:=true;
   end else begin
    fAutoScreenshotWritten:=true;
   end;
  end;
 end else if fMakeScreenshotQOI then begin
  fMakeScreenshotQOI:=false;
  Stream:=TMemoryStream.Create;
  try
   VulkanSwapChain.SaveScreenshotAsQOIToStream(Stream);
   try
    Stream.SaveToFile('screenshot.qoi');
   except
   end;
  finally
   Stream.Free;
  end;
 end;
end;

procedure TApplication.FinishFrame(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
begin
 if assigned(fVirtualReality) then begin
  inherited FinishFrame(aSwapChainImageIndex,aWaitSemaphore,nil);
  fVirtualReality.FinishFrame(aSwapChainImageIndex,DrawInFlightFrameIndex,aWaitSemaphore,aWaitFence);
 end else begin
  inherited FinishFrame(aSwapChainImageIndex,aWaitSemaphore,aWaitFence);
 end;
end;

procedure TApplication.PostPresent(const aSwapChainImageIndex:TpvInt32);
begin
 inherited PostPresent(aSwapChainImageIndex);
 if assigned(fVirtualReality) then begin
  fVirtualReality.PostPresent(aSwapChainImageIndex,DrawInFlightFrameIndex);
 end;
end;

end.
