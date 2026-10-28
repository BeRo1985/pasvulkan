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
unit PasVulkan.Scene3D.Renderer.TemporalUpscalerBackend;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

// What every temporal upscaler backend has in common, whichever vendor technique sits behind it: it takes
// the jittered picture at render resolution together with its depth and motion vectors, keeps its own
// history from frame to frame, and writes the picture at output resolution. One backend instance serves
// one view, so a renderer instance with several views - VR, multiview - has one per view, each with a
// history of its own.
//
// The backend records plain Vulkan commands into the command buffer it is handed and manages its own
// internal images, pipelines and descriptor sets. The images it is given from outside are expected in
// these layouts during the dispatch: the inputs in VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL, the output in
// VK_IMAGE_LAYOUT_GENERAL. Moving the output into and out of GENERAL is the caller's business.

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Scene3D.Renderer.Globals;

type TpvScene3DRendererTemporalUpscalerBackend=class;

     TpvScene3DRendererTemporalUpscalerBackendClass=class of TpvScene3DRendererTemporalUpscalerBackend;

     { TpvScene3DRendererTemporalUpscalerBackend }
     TpvScene3DRendererTemporalUpscalerBackend=class
      public
       type // One image as the backend sees it: a single 2D layer, already viewed as such. BaseArrayLayer
            // is the layer of Image the view shows, for the backends that need the image itself as well.
            TImage=record
             Image:TVkImage;
             ImageView:TVkImageView;
             Format:TVkFormat;
             Width:TpvInt32;
             Height:TpvInt32;
             BaseArrayLayer:TpvInt32;
            end;
            PImage=^TImage;
            // What the backend reads and writes in one in-flight frame. Reactive is optional and left with
            // a VK_NULL_HANDLE image when there is none, which counts as nothing being reactive.
            TInputs=record
             Color:TImage;         // the jittered HDR picture at render resolution
             Depth:TImage;         // the depth at render resolution as R32_SFLOAT
             MotionVectors:TImage; // the motion vectors at render resolution
             Reactive:TImage;      // how much each pixel should follow the current frame over its history, 0..1
             Output:TImage;        // the picture at output resolution, a storage image
            end;
            PInputs=^TInputs;
            // What changes from frame to frame.
            TDispatchDescription=record
             // The jitter of this frame in pixels of the render resolution, as the shift the picture got
             // from it: positive x to the right, positive y downwards.
             JitterOffset:TpvVector2;
             // Turns a motion vector as stored into pixels of the render resolution, pointing from where a
             // pixel is now to where it was in the previous frame.
             MotionVectorScale:TpvVector2;
             RenderWidth:TpvInt32;
             RenderHeight:TpvInt32;
             // The projection the frame was rendered with, the jittered one, from which the depth
             // conversion and the field of view are taken.
             ProjectionMatrix:TpvMatrix4x4;
             // The world to view matrix of the frame, for the backends that need the camera itself.
             ViewMatrix:TpvMatrix4x4;
             // The distances of the near and the far plane, both positive, the far one possibly infinite.
             ZNear:TpvFloat;
             ZFar:TpvFloat;
             FrameTimeDelta:TpvFloat; // in seconds
             PreExposure:TpvFloat;    // what the input color was already multiplied with, 1.0 for nothing
             Sharpness:TpvFloat;      // 0.0 for none, up to 1.0
             Reset:boolean;           // the camera jumped, the history must not be used
             DebugView:boolean;       // draw the backend's own view of its inputs and internals over the output, where it has one
            end;
            PDispatchDescription=^TDispatchDescription;
      protected
       fDevice:TpvVulkanDevice;
       fPipelineCache:TpvVulkanPipelineCache;
       fCountInFlightFrames:TpvSizeInt;
       fRenderWidth:TpvInt32;
       fRenderHeight:TpvInt32;
       fUpscaleWidth:TpvInt32;
       fUpscaleHeight:TpvInt32;
       fInvertedDepth:boolean;
       fInfiniteDepth:boolean;
       fName:TpvUTF8String;
       fQuality:TpvScene3DRendererTemporalUpscalerQuality;
       fFrameGenerationProbe:boolean;
      public
       // The backend class that takes care of a temporal upscaler, nil when none is registered for it.
       class function FindBackend(const aTemporalUpscaler:TpvScene3DRendererTemporalUpscaler):TpvScene3DRendererTemporalUpscalerBackendClass; static;
       // Registers the class it is called on for a temporal upscaler, typically in the initialization
       // section of its unit. A backend may live outside PasVulkan this way, with nothing of it in here.
       class procedure RegisterBackend(const aTemporalUpscaler:TpvScene3DRendererTemporalUpscaler);
       // Whether the backend can run on a device at all; the renderer falls back to no temporal upscaler
       // when not.
       class function IsSupported(const aDevice:TpvVulkanDevice):boolean; virtual;
       // How many jitter phases one cycle has for the given render and output widths. Eight for every
       // output pixel one render pixel covers, unless the backend wants otherwise.
       class function GetJitterPhaseCount(const aRenderWidth,aOutputWidth:TpvInt32):TpvInt32; virtual;
       // The texture LOD bias the final view is drawn with for the given render and output widths, log2
       // of their ratio unless the backend wants otherwise.
       class function GetTextureLODBias(const aRenderWidth,aOutputWidth:TpvInt32):TpvFloat; virtual;
       constructor Create(const aDevice:TpvVulkanDevice;
                          const aPipelineCache:TpvVulkanPipelineCache;
                          const aCountInFlightFrames:TpvSizeInt;
                          const aRenderWidth:TpvInt32;
                          const aRenderHeight:TpvInt32;
                          const aUpscaleWidth:TpvInt32;
                          const aUpscaleHeight:TpvInt32;
                          const aInvertedDepth:boolean;
                          const aInfiniteDepth:boolean;
                          const aName:TpvUTF8String); reintroduce; virtual;
       destructor Destroy; override;
       // Hands over the images of one in-flight frame. Called once per in-flight frame after creation,
       // and again whenever those images change.
       procedure SetInputs(const aInFlightFrameIndex:TpvSizeInt;const aInputs:TInputs); virtual; abstract;
       // Records the whole upscaling of one frame. False when the output was not written, as when the
       // backend failed, and the pass then scales the picture up by itself, see temporal_upscaler_fallback.comp.
       function Dispatch(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex:TpvSizeInt;const aDescription:TDispatchDescription):boolean; virtual; abstract;
      published
       property RenderWidth:TpvInt32 read fRenderWidth;
       property RenderHeight:TpvInt32 read fRenderHeight;
       property UpscaleWidth:TpvInt32 read fUpscaleWidth;
       property UpscaleHeight:TpvInt32 read fUpscaleHeight;
       property InvertedDepth:boolean read fInvertedDepth;
       property InfiniteDepth:boolean read fInfiniteDepth;
       // The quality the renderer asked for, set before the first dispatch, for the backends that choose
       // something by it; the render size itself is the renderer's.
       property Quality:TpvScene3DRendererTemporalUpscalerQuality read fQuality write fQuality;
       // TpvScene3DRenderer.FrameGenerationProbe, set before the first dispatch, for the backends that can
       // probe a frame generation of their vendor.
       property FrameGenerationProbe:boolean read fFrameGenerationProbe write fFrameGenerationProbe;
     end;

implementation

var BackendClasses:array[TpvScene3DRendererTemporalUpscaler] of TpvScene3DRendererTemporalUpscalerBackendClass;

{ TpvScene3DRendererTemporalUpscalerBackend }

class function TpvScene3DRendererTemporalUpscalerBackend.FindBackend(const aTemporalUpscaler:TpvScene3DRendererTemporalUpscaler):TpvScene3DRendererTemporalUpscalerBackendClass;
begin
 result:=BackendClasses[aTemporalUpscaler];
end;

class procedure TpvScene3DRendererTemporalUpscalerBackend.RegisterBackend(const aTemporalUpscaler:TpvScene3DRendererTemporalUpscaler);
begin
 BackendClasses[aTemporalUpscaler]:=self;
end;

class function TpvScene3DRendererTemporalUpscalerBackend.IsSupported(const aDevice:TpvVulkanDevice):boolean;
begin
 result:=false;
end;

// Eight phases for every output pixel that one render pixel covers, so that within one cycle every output
// pixel gets samples from all over it at every upscaling ratio - eight at native resolution, 18 at 1.5
// times, 32 at twice and 72 at three times the render size. Rounded down, as FSR counts them.
class function TpvScene3DRendererTemporalUpscalerBackend.GetJitterPhaseCount(const aRenderWidth,aOutputWidth:TpvInt32):TpvInt32;
var Ratio:TpvDouble;
begin
 Ratio:=Max(1,aOutputWidth)/Max(1,aRenderWidth);
 result:=Max(1,trunc(8.0*sqr(Ratio)));
end;

class function TpvScene3DRendererTemporalUpscalerBackend.GetTextureLODBias(const aRenderWidth,aOutputWidth:TpvInt32):TpvFloat;
begin
 result:=ln(Max(1,aRenderWidth)/Max(1,aOutputWidth))/ln(2.0);
end;

constructor TpvScene3DRendererTemporalUpscalerBackend.Create(const aDevice:TpvVulkanDevice;
                                                             const aPipelineCache:TpvVulkanPipelineCache;
                                                             const aCountInFlightFrames:TpvSizeInt;
                                                             const aRenderWidth:TpvInt32;
                                                             const aRenderHeight:TpvInt32;
                                                             const aUpscaleWidth:TpvInt32;
                                                             const aUpscaleHeight:TpvInt32;
                                                             const aInvertedDepth:boolean;
                                                             const aInfiniteDepth:boolean;
                                                             const aName:TpvUTF8String);
begin
 inherited Create;
 fDevice:=aDevice;
 fPipelineCache:=aPipelineCache;
 fCountInFlightFrames:=aCountInFlightFrames;
 fRenderWidth:=Max(1,aRenderWidth);
 fRenderHeight:=Max(1,aRenderHeight);
 fUpscaleWidth:=Max(1,aUpscaleWidth);
 fUpscaleHeight:=Max(1,aUpscaleHeight);
 fInvertedDepth:=aInvertedDepth;
 fInfiniteDepth:=aInfiniteDepth;
 fName:=aName;
 fQuality:=TpvScene3DRendererTemporalUpscalerQuality.Quality;
 fFrameGenerationProbe:=false;
end;

destructor TpvScene3DRendererTemporalUpscalerBackend.Destroy;
begin
 inherited Destroy;
end;

end.
