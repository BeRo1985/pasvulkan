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
unit PasVulkan.Scene3D.Renderer.FrameGenerationBackend;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

// The interface of a frame generation behind the renderer: it makes the frames that are presented between
// two rendered ones. A backend is handed each rendered frame once, as the picture without the HUD at output
// resolution, plus the depth, the motion vectors and the camera of the frame where it asks for them, and it
// writes the generated frames of that interval into an output image one after the other. Putting the HUD on
// top and presenting them is not its job, see TpvScene3DRendererFrameGenerationCompositor.
//
// Backends register themselves for a TpvScene3DRendererFrameGeneration, the way the temporal upscaler
// backends do, so a backend may live outside PasVulkan with nothing of it in here.

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Scene3D.Renderer.Globals;

type TpvScene3DRendererFrameGenerationBackend=class;

     TpvScene3DRendererFrameGenerationBackendClass=class of TpvScene3DRendererFrameGenerationBackend;

     { TpvScene3DRendererFrameGenerationBackend }
     TpvScene3DRendererFrameGenerationBackend=class
      public
       type // One image as the backend sees it: a single 2D layer, already viewed as such, in the layout
            // given with it.
            TImage=record
             Image:TVkImage;
             ImageView:TVkImageView;
             Format:TVkFormat;
             Width:TpvInt32;
             Height:TpvInt32;
            end;
            PImage=^TImage;
            // One rendered frame. Depth and MotionVectors are only filled in for backends that ask for them
            // with NeedsDepthAndMotionVectors, and are otherwise left with a VK_NULL_HANDLE image.
            TFrame=record
             Color:TImage;         // the picture without the HUD at output resolution, linear, SHADER_READ_ONLY_OPTIMAL
             Depth:TImage;         // the resolved depth at render resolution as R32_SFLOAT, SHADER_READ_ONLY_OPTIMAL
             MotionVectors:TImage; // the motion vectors at render resolution, SHADER_READ_ONLY_OPTIMAL
             // The jitter of this frame in pixels of the render resolution and the scale that turns a motion
             // vector as stored into pixels of the render resolution, pointing from where a pixel is now to
             // where it was in the previous frame, as for the temporal upscaler backends.
             JitterOffset:TpvVector2;
             MotionVectorScale:TpvVector2;
             // The camera of the frame, the projection without its jitter.
             ViewMatrix:TpvMatrix4x4;
             ProjectionMatrix:TpvMatrix4x4;
             // The distances of the near and the far plane, both positive, the far one possibly infinite.
             ZNear:TpvFloat;
             ZFar:TpvFloat;
             // The brightest value the color takes: 1.0 for SDR output, for HDR output the peak relative to
             // diffuse white, which is at 1.0 in both.
             PeakBrightness:TpvFloat;
             FrameTimeDelta:TpvFloat; // in seconds, since the rendered frame before
             FrameIndex:TpvInt64;     // counts the rendered frames
            end;
            PFrame=^TFrame;
            // One generated frame to make, the Index-th of Count between Previous and Current, that is at
            // Index/(Count+1) of the way from Previous to Current.
            TGenerateDescription=record
             Previous:TFrame;
             Current:TFrame;
             Output:TImage;   // a storage image at output resolution in the format of the color, GENERAL
             Index:TpvInt32;  // 1 .. Count
             Count:TpvInt32;
             // The history must not be used: the camera jumped between Previous and Current, or Previous
             // is the first frame after a reset.
             Reset:boolean;
             DebugView:boolean; // the backend's own view of its internals over the output, where it has one
            end;
            PGenerateDescription=^TGenerateDescription;
      protected
       fDevice:TpvVulkanDevice;
       fPipelineCache:TpvVulkanPipelineCache;
       fCountInFlightFrames:TpvSizeInt;
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fRenderWidth:TpvInt32;
       fRenderHeight:TpvInt32;
       fColorFormat:TVkFormat;
       fInvertedDepth:boolean;
       fName:TpvUTF8String;
      public
       // The backend class that takes care of a frame generation, nil when none is registered for it.
       class function FindBackend(const aFrameGeneration:TpvScene3DRendererFrameGeneration):TpvScene3DRendererFrameGenerationBackendClass; static;
       // Registers the class it is called on for a frame generation, typically in the initialization section
       // of its unit.
       class procedure RegisterBackend(const aFrameGeneration:TpvScene3DRendererFrameGeneration);
       // Whether the backend can run on a device at all; the renderer falls back to no frame generation
       // when not.
       class function IsSupported(const aDevice:TpvVulkanDevice):boolean; virtual;
       // How many frames the backend can generate between two rendered ones on a device, at least one.
       class function GetMaximumCountGeneratedFrames(const aDevice:TpvVulkanDevice):TpvInt32; virtual;
       // Whether the backend reads the depth and the motion vectors of the frames.
       class function NeedsDepthAndMotionVectors:boolean; virtual;
       constructor Create(const aDevice:TpvVulkanDevice;
                          const aPipelineCache:TpvVulkanPipelineCache;
                          const aCountInFlightFrames:TpvSizeInt;
                          const aWidth:TpvInt32;
                          const aHeight:TpvInt32;
                          const aRenderWidth:TpvInt32;
                          const aRenderHeight:TpvInt32;
                          const aColorFormat:TVkFormat;
                          const aInvertedDepth:boolean;
                          const aName:TpvUTF8String); reintroduce; virtual;
       destructor Destroy; override;
       // Records the making of one generated frame. For one interval it is called for Index 1 to Count in
       // this order, all in the same command buffer.
       procedure Generate(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex:TpvSizeInt;const aDescription:TGenerateDescription); virtual; abstract;
      published
       property Width:TpvInt32 read fWidth;
       property Height:TpvInt32 read fHeight;
       property RenderWidth:TpvInt32 read fRenderWidth;
       property RenderHeight:TpvInt32 read fRenderHeight;
       property ColorFormat:TVkFormat read fColorFormat;
       property InvertedDepth:boolean read fInvertedDepth;
     end;

implementation

var BackendClasses:array[TpvScene3DRendererFrameGeneration] of TpvScene3DRendererFrameGenerationBackendClass;

{ TpvScene3DRendererFrameGenerationBackend }

class function TpvScene3DRendererFrameGenerationBackend.FindBackend(const aFrameGeneration:TpvScene3DRendererFrameGeneration):TpvScene3DRendererFrameGenerationBackendClass;
begin
 result:=BackendClasses[aFrameGeneration];
end;

class procedure TpvScene3DRendererFrameGenerationBackend.RegisterBackend(const aFrameGeneration:TpvScene3DRendererFrameGeneration);
begin
 BackendClasses[aFrameGeneration]:=self;
end;

class function TpvScene3DRendererFrameGenerationBackend.IsSupported(const aDevice:TpvVulkanDevice):boolean;
begin
 result:=false;
end;

class function TpvScene3DRendererFrameGenerationBackend.GetMaximumCountGeneratedFrames(const aDevice:TpvVulkanDevice):TpvInt32;
begin
 result:=1;
end;

class function TpvScene3DRendererFrameGenerationBackend.NeedsDepthAndMotionVectors:boolean;
begin
 result:=true;
end;

constructor TpvScene3DRendererFrameGenerationBackend.Create(const aDevice:TpvVulkanDevice;
                                                            const aPipelineCache:TpvVulkanPipelineCache;
                                                            const aCountInFlightFrames:TpvSizeInt;
                                                            const aWidth:TpvInt32;
                                                            const aHeight:TpvInt32;
                                                            const aRenderWidth:TpvInt32;
                                                            const aRenderHeight:TpvInt32;
                                                            const aColorFormat:TVkFormat;
                                                            const aInvertedDepth:boolean;
                                                            const aName:TpvUTF8String);
begin
 inherited Create;
 fDevice:=aDevice;
 fPipelineCache:=aPipelineCache;
 fCountInFlightFrames:=aCountInFlightFrames;
 fWidth:=Max(1,aWidth);
 fHeight:=Max(1,aHeight);
 fRenderWidth:=Max(1,aRenderWidth);
 fRenderHeight:=Max(1,aRenderHeight);
 fColorFormat:=aColorFormat;
 fInvertedDepth:=aInvertedDepth;
 fName:=aName;
end;

destructor TpvScene3DRendererFrameGenerationBackend.Destroy;
begin
 inherited Destroy;
end;

end.
