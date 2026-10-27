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
     PasVulkan.Framework;

type { TpvScene3DRendererTemporalUpscalerBackend }
     TpvScene3DRendererTemporalUpscalerBackend=class
      public
       type // One image as the backend sees it: a single 2D layer, already viewed as such.
            TImage=record
             Image:TVkImage;
             ImageView:TVkImageView;
             Format:TVkFormat;
             Width:TpvInt32;
             Height:TpvInt32;
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
             FrameTimeDelta:TpvFloat; // in seconds
             PreExposure:TpvFloat;    // what the input color was already multiplied with, 1.0 for nothing
             Sharpness:TpvFloat;      // 0.0 for none, up to 1.0
             Reset:boolean;           // the camera jumped, the history must not be used
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
      public
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
       // Records the whole upscaling of one frame.
       procedure Dispatch(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex:TpvSizeInt;const aDescription:TDispatchDescription); virtual; abstract;
      published
       property RenderWidth:TpvInt32 read fRenderWidth;
       property RenderHeight:TpvInt32 read fRenderHeight;
       property UpscaleWidth:TpvInt32 read fUpscaleWidth;
       property UpscaleHeight:TpvInt32 read fUpscaleHeight;
       property InvertedDepth:boolean read fInvertedDepth;
       property InfiniteDepth:boolean read fInfiniteDepth;
     end;

implementation

{ TpvScene3DRendererTemporalUpscalerBackend }

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
end;

destructor TpvScene3DRendererTemporalUpscalerBackend.Destroy;
begin
 inherited Destroy;
end;

end.
