(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2017, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.Canvas;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

uses SysUtils,
     Classes,
     Math,
     PUCU,
     PasMP,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Collections,
     PasVulkan.Framework,
     PasVulkan.Sprites;

type TpvCanvasColor=class(TPersistent)
      private
       fRed:TpvFloat;
       fGreen:TpvFloat;
       fBlue:TpvFloat;
       fAlpha:TpvFloat;
      public
       constructor Create;
       destructor Destroy; override;
       procedure Assign(Source:TPersistent); override;
      published
       property Red:TpvFloat read fRed write fRed;
       property Green:TpvFloat read fGreen write fGreen;
       property Blue:TpvFloat read fBlue write fBlue;
       property Alpha:TpvFloat read fAlpha write fAlpha;
     end;

     TpvCanvasPenStyle=(vcpsClear,vcpsSolid);

     TpvCanvasPenLineJoin=(vcpljBevel,vcpljMiter,vcpljRound);

     TpvCanvasPenLineCap=(vcplcButt,vcplcSquare,vcplcRound);

     TpvCanvasPen=class(TPersistent)
      private
       fColor:TpvCanvasColor;
       fWidth:TpvFloat;
       fAntialiasingWidth:TpvFloat;
       fMiterLimit:TpvFloat;
       fStyle:TpvCanvasPenStyle;
       fLineJoin:TpvCanvasPenLineJoin;
       fLineCap:TpvCanvasPenLineCap;
      public
       constructor Create;
       destructor Destroy; override;
       procedure Assign(Source:TPersistent); override;
      published
       property Color:TpvCanvasColor read fColor;
       property Width:TpvFloat read fWidth write fWidth;
       property AntialiasingWidth:TpvFloat read fAntialiasingWidth write fAntialiasingWidth;
       property MiterLimit:TpvFloat read fMiterLimit write fMiterLimit;
       property Style:TpvCanvasPenStyle read fStyle write fStyle default vcpsSolid;
       property LineJoin:TpvCanvasPenLineJoin read fLineJoin write fLineJoin default vcpljRound;
       property LineCap:TpvCanvasPenLineCap read fLineCap write fLineCap default vcplcRound;
     end;

     PpvCanvasPoint=^TpvCanvasPoint;
     TpvCanvasPoint=TpvVector2;

     PpvCanvasInternalPoint=^TpvCanvasInternalPoint;
     TpvCanvasInternalPoint=record
      Position:TpvVector2;
      Middle:TpvVector2;
      Color:TpvVector4;
     end;

     TpvCanvasInternalPoints=array of TpvCanvasInternalPoint;

     TpvCanvasMode=(vcmNormal,vcmLine);

     PpvCanvasVertexState=^TpvCanvasVertexState;
     TpvCanvasVertexState=record
      BlendingMode:TpvHalfFloat;
      RenderingMode:TpvHalfFloat;
     end;

     PpvCanvasVertex=^TpvCanvasVertex;
     TpvCanvasVertex=packed record
      Position:TpvVector2;                       // +  8 (2x 32-bit floats)       = 0
      Color:TpvHalfFloatVector4;                 // +  8 (4x 16-bit half-floats)  = 8  (=> 8 byte aligned)
      TextureCoord:TpvVector3;                   // + 12 (3x 32-bit floats)       = 16 (=> 16 byte aligned)
      State:TpvCanvasVertexState;                // +  4 (2x 16-bit half-floats)  = 28 (=> 4 byte aligned)
      ClipRect:TpvRect;                          // + 16 (4x 32-bit floats)       = 32 (=> 32 byte aligned)
      MetaInfo:TpvVector4;                       // + 16 (4x 32-bit floats)       = 48 (=> 32 byte aligned)
     end;                                        // = 64 per vertex

     TpvCanvasVertices=array of TpvCanvasVertex;

     TpvCanvasVulkanBuffers=array of TpvVulkanBuffer;

     PpvCanvasVertexBuffer=^TpvCanvasVertexBuffer;
     TpvCanvasVertexBuffer=array[0..(32768*4)-1] of TpvCanvasVertex;

     TpvCanvasVertexBuffers=array of TpvCanvasVertexBuffer;

     TpvCanvasVertexBufferSizes=array of TVkSizeInt;

     PpvCanvasIndexBuffer=^TpvCanvasIndexBuffer;
     TpvCanvasIndexBuffer=array[0..((SizeOf(TpvCanvasVertexBuffer) div (SizeOf(TpvCanvasVertex)*4))*6)-1] of TpvUInt32;

     TpvCanvasIndexBuffers=array of TpvCanvasIndexBuffer;

     TpvCanvasIndexBufferSizes=array of TVkSizeInt;

     PpvCanvasRenderingMode=^TpvCanvasRenderingMode;
     TpvCanvasRenderingMode=
      (
       vsbrmNormal,
       vsbrmFont
      );

     PpvCanvasBlendingMode=^TpvCanvasBlendingMode;
     TpvCanvasBlendingMode=
      (
       vsbbmNone,
       vsbbmAlphaBlending,
       vsbbmAdditiveBlending
      );

     TpvCanvasHook=procedure(const aData:TpvPointer) of object;

     TpvCanvasQueueItemKind=
      (
       vcqikNone,
       vcqikNormal,
       vcqikVector,
       vcqikHook
      );

     PpvCanvasQueueItem=^TpvCanvasQueueItem;
     TpvCanvasQueueItem=record
      Kind:TpvCanvasQueueItemKind;
      BufferIndex:TpvInt32;
      DescriptorIndex:TpvInt32;
      StartVertexIndex:TpvInt32;
      StartIndexIndex:TpvInt32;
      CountVertices:TpvInt32;
      CountIndices:TpvInt32;
      RenderingMode:TpvCanvasRenderingMode;
      BlendingMode:TpvCanvasBlendingMode;
      Scissor:TVkRect2D;
      Hook:TpvCanvasHook;
      HookData:TVkPointer;
     end;

     TpvCanvasQueueItems=array of TpvCanvasQueueItem;

     TpvCanvasDescriptorPools=array of TpvVulkanDescriptorPool;

     TpvCanvasDescriptorSets=array of TpvVulkanDescriptorSet;

     PpvCanvasBuffer=^TpvCanvasBuffer;
     TpvCanvasBuffer=record
      fSpinLock:TpvInt32;
      fVulkanVertexBuffers:TpvCanvasVulkanBuffers;
      fVulkanIndexBuffers:TpvCanvasVulkanBuffers;
      fVertexBuffers:TpvCanvasVertexBuffers;
      fVertexBufferSizes:TpvCanvasVertexBufferSizes;
      fIndexBuffers:TpvCanvasIndexBuffers;
      fIndexBufferSizes:TpvCanvasIndexBufferSizes;
      fCountAllocatedBuffers:TpvInt32;
      fCountUsedBuffers:TpvInt32;
      fQueueItems:TpvCanvasQueueItems;
      fCountQueueItems:TpvInt32;
     end;

     TpvCanvasBuffers=array of TpvCanvasBuffer;

     TpvCanvasAtlasArrayTextureDescriptorSetHashMap=class(TpvHashMap<TpvSpriteAtlasArrayTexture,TpvInt32>);

     TpvCanvasPathCommandType=
      (
       pcpctMoveTo,
       pcpctLineTo,
       pcpctQuadraticCurveTo,
       pcpctCubicCurveTo,
       pcpctClose
      );

     PpvCanvasPathCommandPoints=^TpvCanvasPathCommandPoints;
     TpvCanvasPathCommandPoints=array[0..2] of TpvVector2;

     PpvCanvasPathCommand=^TpvCanvasPathCommand;
     TpvCanvasPathCommand=record
      CommandType:TpvCanvasPathCommandType;
      Points:TpvCanvasPathCommandPoints;
     end;

     TpvCanvasPathCommands=array of TpvCanvasPathCommand;

     TpvCanvasPath=class
      private
       fCommands:TpvCanvasPathCommands;
       fCountCommands:TpvInt32;
       function NewCommand:PpvCanvasPathCommand;
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       function Reset:TpvCanvasPath;
       function MoveTo(const aP0:TpvVector2):TpvCanvasPath; overload;
       function MoveTo(const aX,aY:TpvFloat):TpvCanvasPath; overload;
       function LineTo(const aP0:TpvVector2):TpvCanvasPath; overload;
       function LineTo(const aX,aY:TpvFloat):TpvCanvasPath; overload;
       function QuadraticCurveTo(const aC0,aA0:TpvVector2):TpvCanvasPath; overload;
       function QuadraticCurveTo(const aCX,aCY,aAX,aAY:TpvFloat):TpvCanvasPath; overload;
       function CubicCurveTo(const aC0,aC1,aA0:TpvVector2):TpvCanvasPath; overload;
       function CubicCurveTo(const aC0X,aC0Y,aC1X,aC1Y,aAX,aAY:TpvFloat):TpvCanvasPath; overload;
       function Close:TpvCanvasPath;
     end;

     TpvCanvas=class
      private
       fDevice:TpvVulkanDevice;
       fGraphicsQueue:TpvVulkanQueue;
       fGraphicsCommandBuffer:TpvVulkanCommandBuffer;
       fGraphicsFence:TpvVulkanFence;
       fTransferQueue:TpvVulkanQueue;
       fTransferCommandBuffer:TpvVulkanCommandBuffer;
       fTransferFence:TpvVulkanFence;
       fPipelineCache:TpvVulkanPipelineCache;
       fSpriteBatchVertexShaderModule:TpvVulkanShaderModule;
       fSpriteBatchFragmentShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineSpriteBatchShaderStageVertex:TpvVulkanPipelineShaderStage;
       fVulkanPipelineSpriteBatchShaderStageFragment:TpvVulkanPipelineShaderStage;
       fVulkanDescriptorPools:TpvCanvasDescriptorPools;
       fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fVulkanDescriptorSets:TpvCanvasDescriptorSets;
       fCountVulkanDescriptors:TpvInt32;
       fVulkanTextureDescriptorSetHashMap:TpvCanvasAtlasArrayTextureDescriptorSetHashMap;
       fVulkanRenderPass:TpvVulkanRenderPass;
       fVulkanPipelineLayout:TpvVulkanPipelineLayout;
       fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
       fVulkanCanvasBuffers:TpvCanvasBuffers;
       fCountBuffers:TpvInt32;
       fCurrentFillBuffer:PpvCanvasBuffer;
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fViewPort:TVkViewport;
       fPointerToViewport:PVkViewport;
       fCurrentVulkanBufferIndex:TpvInt32;
       fCurrentVulkanVertexBufferOffset:TpvInt32;
       fCurrentVulkanIndexBufferOffset:TpvInt32;
       fState:TpvCanvasVertexState;
       fClipRect:TpvRect;
       fUnscaledClipRect:TpvRect;
       fRenderingMode:TpvCanvasRenderingMode;
       fBlendingMode:TpvCanvasBlendingMode;
       fLastArrayTexture:TpvSpriteAtlasArrayTexture;
       fInverseWidth:TpvFloat;
       fInverseHeight:TpvFloat;
       fInverseTextureWidth:TpvFloat;
       fInverseTextureHeight:TpvFloat;
       fCurrentCountVertices:TVkSizeInt;
       fCurrentCountIndices:TVkSizeInt;
       fCurrentDestinationVertexBufferPointer:PpvCanvasVertexBuffer;
       fCurrentDestinationIndexBufferPointer:PpvCanvasIndexBuffer;
       fScissor:TVkRect2D;
       fMatrix:TpvMatrix4x4;
       fPen:TpvCanvasPen;
       function RotatePoint(const PointToRotate,AroundPoint:TpvVector2;Cosinus,Sinus:TpvFloat):TpvVector2;
       procedure SetArrayTexture(const ArrayTexture:TpvSpriteAtlasArrayTexture);
       procedure SetRenderingMode(const aRenderingMode:TpvCanvasRenderingMode);
       procedure SetBlendingMode(const aBlendingMode:TpvCanvasBlendingMode);
       procedure SetMatrix(const aMatrix:TpvMatrix4x4);
       procedure GetNextDestinationVertexBuffer;
       procedure FlushAndGetNewDestinationVertexBufferIfNeeded(const aCountVerticesToCheck,aCountIndicesToCheck:TpvInt32);
       function ClipCheck(const aX0,aY0,aX1,aY1:TpvFloat):boolean;
      public
       constructor Create(const aDevice:TpvVulkanDevice;
                          const aGraphicsQueue:TpvVulkanQueue;
                          const aGraphicsCommandBuffer:TpvVulkanCommandBuffer;
                          const aGraphicsFence:TpvVulkanFence;
                          const aTransferQueue:TpvVulkanQueue;
                          const aTransferCommandBuffer:TpvVulkanCommandBuffer;
                          const aTransferFence:TpvVulkanFence;
                          const aPipelineCache:TpvVulkanPipelineCache;
                          const aRenderPass:TpvVulkanRenderPass;
                          const aCountBuffers:TpvInt32); reintroduce;
       destructor Destroy; override;
       procedure Start(const aBufferIndex:TpvInt32);
       procedure Stop;
       procedure Flush;
       procedure SetScissor(const aScissor:TVkRect2D); overload;
       procedure SetScissor(const aLeft,aTop,aWidth,aHeight:TpvInt32); overload;
       procedure SetClipRect(const aClipRect:TVkRect2D); overload;
       procedure SetClipRect(const aClipRect:TpvRect); overload;
       procedure SetClipRect(const aLeft,aTop,aWidth,aHeight:TpvInt32); overload;
       procedure Hook(const aHook:TpvCanvasHook;const aData:TpvPointer); overload;
       procedure DrawSprite(const Sprite:TpvSprite;const Src,Dest:TpvRect;const Color:TpvVector4); overload;
       procedure DrawSprite(const Sprite:TpvSprite;const Src,Dest:TpvRect;const Origin:TpvVector2;Rotation:TpvFloat;const Color:TpvVector4); overload;
       procedure DrawSprite(const Sprite:TpvSprite;const x,y:TpvFloat;const Color:TpvVector4); overload;
       procedure DrawSprite(const Sprite:TpvSprite;const x,y:TpvFloat); overload;
       procedure DrawSprite(const Sprite:TpvSprite;const sx1,sy1,sx2,sy2,dx1,dy1,dx2,dy2,Alpha:TpvFloat); overload;
       procedure ExecuteUpload(const aVulkanCommandBuffer:TpvVulkanCommandBuffer;const aBufferIndex:TpvInt32);
       procedure ExecuteDraw(const aVulkanCommandBuffer:TpvVulkanCommandBuffer;const aBufferIndex:TpvInt32);
      public
       property Viewport:PVkViewport read fPointerToViewport;
      published
       property Device:TpvVulkanDevice read fDevice;
       property Width:TpvInt32 read fWidth write fWidth;
       property Height:TpvInt32 read fHeight write fHeight;
       property RenderingMode:TpvCanvasRenderingMode read fRenderingMode write SetRenderingMode;
       property BlendingMode:TpvCanvasBlendingMode read fBlendingMode write SetBlendingMode;
       property Matrix:TpvMatrix4x4 read fMatrix write SetMatrix;
       property Pen:TpvCanvasPen read fPen;
     end;

implementation

uses PasVulkan.Assets,
     PasVulkan.Streams;

constructor TpvCanvasColor.Create;
begin
 inherited Create;
 fRed:=1.0;
 fGreen:=1.0;
 fBlue:=1.0;
 fAlpha:=1.0;
end;

destructor TpvCanvasColor.Destroy;
begin
 inherited Destroy;
end;

procedure TpvCanvasColor.Assign(Source:TPersistent);
begin
 Assert(Source is TpvCanvasColor);
 fRed:=TpvCanvasColor(Source).fRed;
 fGreen:=TpvCanvasColor(Source).fGreen;
 fBlue:=TpvCanvasColor(Source).fBlue;
 fAlpha:=TpvCanvasColor(Source).fAlpha;
end;

constructor TpvCanvasPen.Create;
begin
 inherited Create;
 fColor:=TpvCanvasColor.Create;
 fWidth:=1.0;
 fAntialiasingWidth:=2.0;
 fMiterLimit:=3.0;
 fStyle:=vcpsSolid;
 fLineJoin:=vcpljRound;
 fLineCap:=vcplcRound;
end;

destructor TpvCanvasPen.Destroy;
begin
 FreeAndNil(fColor);
 inherited Destroy;
end;

procedure TpvCanvasPen.Assign(Source:TPersistent);
begin
 Assert(Source is TpvCanvasPen);
 fColor.Assign(TpvCanvasPen(Source).fColor);
 fWidth:=TpvCanvasPen(Source).fWidth;
 fAntialiasingWidth:=TpvCanvasPen(Source).fAntialiasingWidth;
 fMiterLimit:=TpvCanvasPen(Source).fMiterLimit;
 fStyle:=TpvCanvasPen(Source).fStyle;
 fLineJoin:=TpvCanvasPen(Source).fLineJoin;
 fLineCap:=TpvCanvasPen(Source).fLineCap;
end;

constructor TpvCanvasPath.Create;
begin
 inherited Create;
 fCommands:=nil;
 fCountCommands:=0;
end;

destructor TpvCanvasPath.Destroy;
begin
 fCommands:=nil;
 inherited Destroy;
end;

function TpvCanvasPath.NewCommand:PpvCanvasPathCommand;
var Index:TpvInt32;
begin
 Index:=fCountCommands;
 inc(fCountCommands);
 if length(fCommands)<fCountCommands then begin
  SetLength(fCommands,fCountCommands*2);
 end;
 result:=@fCommands[Index];
end;

function TpvCanvasPath.Reset:TpvCanvasPath;
begin
 fCountCommands:=0;
 result:=self;
end;

function TpvCanvasPath.MoveTo(const aP0:TpvVector2):TpvCanvasPath;
var Command:PpvCanvasPathCommand;
begin
 Command:=NewCommand;
 Command^.CommandType:=pcpctMoveTo;
 Command^.Points[0]:=aP0;
 result:=self;
end;

function TpvCanvasPath.MoveTo(const aX,aY:TpvFloat):TpvCanvasPath;
begin
 MoveTo(TpvVector2.Create(aX,aY));
 result:=self;
end;

function TpvCanvasPath.LineTo(const aP0:TpvVector2):TpvCanvasPath;
var Command:PpvCanvasPathCommand;
begin
 Command:=NewCommand;
 Command^.CommandType:=pcpctLineTo;
 Command^.Points[0]:=aP0;
 result:=self;
end;

function TpvCanvasPath.LineTo(const aX,aY:TpvFloat):TpvCanvasPath;
begin
 LineTo(TpvVector2.Create(aX,aY));
 result:=self;
end;

function TpvCanvasPath.QuadraticCurveTo(const aC0,aA0:TpvVector2):TpvCanvasPath;
var Command:PpvCanvasPathCommand;
begin
 Command:=NewCommand;
 Command^.CommandType:=pcpctQuadraticCurveTo;
 Command^.Points[0]:=aC0;
 Command^.Points[1]:=aA0;
 result:=self;
end;

function TpvCanvasPath.QuadraticCurveTo(const aCX,aCY,aAX,aAY:TpvFloat):TpvCanvasPath;
begin
 QuadraticCurveTo(TpvVector2.Create(aCX,aCY),TpvVector2.Create(aAX,aAY));
 result:=self;
end;

function TpvCanvasPath.CubicCurveTo(const aC0,aC1,aA0:TpvVector2):TpvCanvasPath;
var Command:PpvCanvasPathCommand;
begin
 Command:=NewCommand;
 Command^.CommandType:=pcpctCubicCurveTo;
 Command^.Points[0]:=aC0;
 Command^.Points[1]:=aC1;
 Command^.Points[2]:=aA0;
 result:=self;
end;

function TpvCanvasPath.CubicCurveTo(const aC0X,aC0Y,aC1X,aC1Y,aAX,aAY:TpvFloat):TpvCanvasPath;
begin
 CubicCurveTo(TpvVector2.Create(aC0X,aC0Y),TpvVector2.Create(aC1X,aC1Y),TpvVector2.Create(aAX,aAY));
 result:=self;
end;

function TpvCanvasPath.Close:TpvCanvasPath;
var Command:PpvCanvasPathCommand;
begin
 Command:=NewCommand;
 Command^.CommandType:=pcpctClose;
 result:=self;
end;

constructor TpvCanvas.Create(const aDevice:TpvVulkanDevice;
                                      const aGraphicsQueue:TpvVulkanQueue;
                                      const aGraphicsCommandBuffer:TpvVulkanCommandBuffer;
                                      const aGraphicsFence:TpvVulkanFence;
                                      const aTransferQueue:TpvVulkanQueue;
                                      const aTransferCommandBuffer:TpvVulkanCommandBuffer;
                                      const aTransferFence:TpvVulkanFence;
                                      const aPipelineCache:TpvVulkanPipelineCache;
                                      const aRenderPass:TpvVulkanRenderPass;
                                      const aCountBuffers:TpvInt32);
var Index:TpvInt32;
    RenderingModeIndex:TpvCanvasRenderingMode;
    BlendingModeIndex:TpvCanvasBlendingMode;
    VulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
    VulkanCanvasBuffer:PpvCanvasBuffer;
    Stream:TStream;
begin
 inherited Create;

 fDevice:=aDevice;

 fGraphicsQueue:=aGraphicsQueue;
 fGraphicsCommandBuffer:=aGraphicsCommandBuffer;
 fGraphicsFence:=aGraphicsFence;

 fTransferQueue:=aTransferQueue;
 fTransferCommandBuffer:=aTransferCommandBuffer;
 fTransferFence:=aTransferFence;

 fPipelineCache:=aPipelineCache;

 fCountBuffers:=aCountBuffers;

 fVulkanCanvasBuffers:=nil;

 SetLength(fVulkanCanvasBuffers,aCountBuffers);

 fLastArrayTexture:=nil;

 fCurrentCountVertices:=0;
 fCurrentCountIndices:=0;

 fRenderingMode:=vsbrmNormal;

 fBlendingMode:=vsbbmAlphaBlending;

 fWidth:=1280;
 fHeight:=720;

 fViewPort.x:=0.0;
 fViewPort.y:=0.0;
 fViewPort.Width:=1280.0;
 fViewPort.Height:=720.0;
 fViewPort.minDepth:=0.0;
 fViewPort.maxDepth:=1.0;

 fPointerToViewport:=@fViewport;

 fCurrentDestinationVertexBufferPointer:=nil;
 fCurrentDestinationIndexBufferPointer:=nil;

 for Index:=0 to length(fVulkanCanvasBuffers)-1 do begin
  VulkanCanvasBuffer:=@fVulkanCanvasBuffers[Index];
  VulkanCanvasBuffer^.fSpinLock:=0;
  VulkanCanvasBuffer^.fVulkanVertexBuffers:=nil;
  VulkanCanvasBuffer^.fVulkanIndexBuffers:=nil;
  VulkanCanvasBuffer^.fVertexBuffers:=nil;
  VulkanCanvasBuffer^.fVertexBufferSizes:=nil;
  VulkanCanvasBuffer^.fIndexBuffers:=nil;
  VulkanCanvasBuffer^.fIndexBufferSizes:=nil;
  VulkanCanvasBuffer^.fCountAllocatedBuffers:=0;
  VulkanCanvasBuffer^.fCountUsedBuffers:=0;
  VulkanCanvasBuffer^.fQueueItems:=nil;
  VulkanCanvasBuffer^.fCountQueueItems:=0;
 end;

 fCurrentVulkanBufferIndex:=0;
 fCurrentVulkanVertexBufferOffset:=0;
 fCurrentVulkanIndexBufferOffset:=0;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 fVulkanDescriptorPools:=nil;
 fVulkanDescriptorSets:=nil;
 fCountVulkanDescriptors:=0;

 fVulkanTextureDescriptorSetHashMap:=TpvCanvasAtlasArrayTextureDescriptorSetHashMap.Create(-1);

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(fDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 fVulkanRenderPass:=aRenderPass;

 Stream:=TpvDataStream.Create(@SpriteBatchVertexSPIRVData,SpriteBatchVertexSPIRVDataSize);
 try
  fSpriteBatchVertexShaderModule:=TpvVulkanShaderModule.Create(fDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=TpvDataStream.Create(@SpriteBatchFragmentSPIRVData,SpriteBatchFragmentSPIRVDataSize);
 try
  fSpriteBatchFragmentShaderModule:=TpvVulkanShaderModule.Create(fDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineSpriteBatchShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fSpriteBatchVertexShaderModule,'main');

 fVulkanPipelineSpriteBatchShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fSpriteBatchFragmentShaderModule,'main');

 VulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(fDevice,
                                                        fPipelineCache,
                                                        0,
                                                        [],
                                                        fVulkanPipelineLayout,
                                                        fVulkanRenderPass,
                                                        0,
                                                        nil,
                                                        0);
 fVulkanGraphicsPipeline:=VulkanGraphicsPipeline;

 VulkanGraphicsPipeline.AddStage(fVulkanPipelineSpriteBatchShaderStageVertex);
 VulkanGraphicsPipeline.AddStage(fVulkanPipelineSpriteBatchShaderStageFragment);

 VulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 VulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 VulkanGraphicsPipeline.VertexInputState.AddVertexInputBindingDescription(0,SizeOf(TpvCanvasVertex),VK_VERTEX_INPUT_RATE_VERTEX);
 VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(0,0,VK_FORMAT_R32G32_SFLOAT,TpvPtrUInt(TpvPointer(@PpvCanvasVertex(nil)^.Position)));
 VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(1,0,VK_FORMAT_R16G16B16A16_SFLOAT,TpvPtrUInt(TpvPointer(@PpvCanvasVertex(nil)^.Color)));
 VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(2,0,VK_FORMAT_R32G32B32_SFLOAT,TpvPtrUInt(TpvPointer(@PpvCanvasVertex(nil)^.TextureCoord)));
 VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(3,0,VK_FORMAT_R16G16_SFLOAT,TpvPtrUInt(TpvPointer(@PpvCanvasVertex(nil)^.State)));
 VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(4,0,VK_FORMAT_R32G32B32A32_SFLOAT,TpvPtrUInt(TpvPointer(@PpvCanvasVertex(nil)^.ClipRect)));

 VulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fWidth,fHeight,0.0,1.0);
 VulkanGraphicsPipeline.ViewPortState.DynamicViewPorts:=true;

 VulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fWidth,fHeight);
 VulkanGraphicsPipeline.ViewPortState.DynamicScissors:=true;

 VulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 VulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 VulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
 VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
 VulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
 VulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
 VulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
 VulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
 VulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

 VulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=VK_SAMPLE_COUNT_1_BIT;
 VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=false;
 VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=0.0;
 VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
 VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=false;
 VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;

 VulkanGraphicsPipeline.ColorBlendState.LogicOpEnable:=false;
 VulkanGraphicsPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
 VulkanGraphicsPipeline.ColorBlendState.BlendConstants[0]:=0.0;
 VulkanGraphicsPipeline.ColorBlendState.BlendConstants[1]:=0.0;
 VulkanGraphicsPipeline.ColorBlendState.BlendConstants[2]:=0.0;
 VulkanGraphicsPipeline.ColorBlendState.BlendConstants[3]:=0.0;
 VulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(true,
                                                                     VK_BLEND_FACTOR_ONE,
                                                                     VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                     VK_BLEND_OP_ADD,
                                                                     VK_BLEND_FACTOR_ONE,
                                                                     VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                     VK_BLEND_OP_ADD,
                                                                     TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                     TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                     TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                     TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));
 VulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=false;
 VulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=false;
 VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 VulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 VulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 VulkanGraphicsPipeline.DynamicState.AddDynamicStates([VK_DYNAMIC_STATE_VIEWPORT,
                                                       VK_DYNAMIC_STATE_SCISSOR]);

 VulkanGraphicsPipeline.Initialize;

 VulkanGraphicsPipeline.FreeMemory;

 fCurrentFillBuffer:=nil;

 fPen:=TpvCanvasPen.Create;

end;

destructor TpvCanvas.Destroy;
var Index,SubIndex:TpvInt32;
    RenderingModeIndex:TpvCanvasRenderingMode;
    BlendingModeIndex:TpvCanvasBlendingMode;
    VulkanCanvasBuffer:PpvCanvasBuffer;
begin

 FreeAndNil(fPen);

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

//FreeAndNil(fVulkanRenderPass);

 for Index:=0 to fCountVulkanDescriptors-1 do begin
  FreeAndNil(fVulkanDescriptorSets[Index]);
 end;

 fVulkanDescriptorSets:=nil;

 FreeAndNil(fVulkanDescriptorSetLayout);

 for Index:=0 to fCountVulkanDescriptors-1 do begin
  FreeAndNil(fVulkanDescriptorPools[Index]);
 end;

 fVulkanDescriptorPools:=nil;

 FreeAndNil(fVulkanTextureDescriptorSetHashMap);

 FreeAndNil(fVulkanPipelineSpriteBatchShaderStageVertex);

 FreeAndNil(fVulkanPipelineSpriteBatchShaderStageFragment);

 FreeAndNil(fSpriteBatchVertexShaderModule);

 FreeAndNil(fSpriteBatchFragmentShaderModule);

 for Index:=0 to length(fVulkanCanvasBuffers)-1 do begin
  VulkanCanvasBuffer:=@fVulkanCanvasBuffers[Index];
  for SubIndex:=0 to VulkanCanvasBuffer^.fCountAllocatedBuffers-1 do begin
   FreeAndNil(VulkanCanvasBuffer^.fVulkanVertexBuffers[SubIndex]);
   FreeAndNil(VulkanCanvasBuffer^.fVulkanIndexBuffers[SubIndex]);
  end;
  VulkanCanvasBuffer^.fVulkanVertexBuffers:=nil;
  VulkanCanvasBuffer^.fVulkanIndexBuffers:=nil;
  VulkanCanvasBuffer^.fVertexBuffers:=nil;
  VulkanCanvasBuffer^.fVertexBufferSizes:=nil;
  VulkanCanvasBuffer^.fIndexBuffers:=nil;
  VulkanCanvasBuffer^.fIndexBufferSizes:=nil;
  VulkanCanvasBuffer^.fQueueItems:=nil;
 end;

 fCurrentDestinationVertexBufferPointer:=nil;
 fCurrentDestinationIndexBufferPointer:=nil;

 inherited Destroy;
end;

function TpvCanvas.RotatePoint(const PointToRotate,AroundPoint:TpvVector2;Cosinus,Sinus:TpvFloat):TpvVector2;
var x,y:TpvFloat;
begin
 x:=PointToRotate.x-AroundPoint.x;
 y:=PointToRotate.y-AroundPoint.y;
 result.x:=(((((x*Cosinus)-(y*Sinus))+AroundPoint.x)*fInverseWidth)-0.5)*2;
 result.y:=(((((x*Sinus)+(y*Cosinus))+AroundPoint.y)*fInverseHeight)-0.5)*2;
end;

procedure TpvCanvas.SetRenderingMode(const aRenderingMode:TpvCanvasRenderingMode);
begin
 if fRenderingMode<>aRenderingMode then begin
  fRenderingMode:=aRenderingMode;
  case aRenderingMode of
   vsbrmNormal:begin
    fState.RenderingMode:=0.0;
   end;
   else {vsbrmFont:}begin
    fState.RenderingMode:=1.0;
   end;
  end;
 end;
end;

procedure TpvCanvas.SetBlendingMode(const aBlendingMode:TpvCanvasBlendingMode);
begin
 if fBlendingMode<>aBlendingMode then begin
  fBlendingMode:=aBlendingMode;
  case fBlendingMode of
   vsbbmNone:begin
    fState.BlendingMode:=-1.0;
   end;
   vsbbmAlphaBlending:begin
    fState.BlendingMode:=1.0;
   end;
   else{vsbbmAdditiveBlending:}begin
    fState.BlendingMode:=0.0;
   end;
  end;
 end;
end;

procedure TpvCanvas.SetMatrix(const aMatrix:TpvMatrix4x4);
begin
 if fMatrix<>aMatrix then begin
  Flush;
  fMatrix:=aMatrix;
 end;
end;

procedure TpvCanvas.Start(const aBufferIndex:TpvInt32);
begin

 fInverseWidth:=1.0/fWidth;
 fInverseHeight:=1.0/fHeight;

 fLastArrayTexture:=nil;

 fCurrentCountVertices:=0;
 fCurrentCountIndices:=0;

 fRenderingMode:=vsbrmNormal;

 fBlendingMode:=vsbbmAlphaBlending;

 fCurrentFillBuffer:=@fVulkanCanvasBuffers[aBufferIndex];
 fCurrentFillBuffer^.fCountQueueItems:=0;
 fCurrentFillBuffer^.fCountUsedBuffers:=0;

 fScissor.offset.x:=trunc(floor(fViewport.x));
 fScissor.offset.y:=trunc(floor(fViewport.y));
 fScissor.extent.Width:=trunc(ceil(fViewport.Width));
 fScissor.extent.Height:=trunc(ceil(fViewport.Height));

 fMatrix:=TpvMatrix4x4.Identity;

 fCurrentVulkanBufferIndex:=-1;
 fCurrentVulkanVertexBufferOffset:=0;
 fCurrentVulkanIndexBufferOffset:=0;

 GetNextDestinationVertexBuffer;

 fState.BlendingMode:=1.0;
 fState.RenderingMode:=0.0;

 fClipRect:=TpvRect.Create(-1.0,-1.0,1.0,1.0);

 fUnscaledClipRect:=TpvRect.Create(0.0,0.0,fWidth,fHeight);

end;

procedure TpvCanvas.Stop;
begin

 Flush;

 fCurrentFillBuffer:=nil;

end;

procedure TpvCanvas.Flush;
var CurrentVulkanBufferIndex,OldCount,NewCount,QueueItemIndex,DescriptorIndex:TpvInt32;
    QueueItem:PpvCanvasQueueItem;
    VulkanDescriptorPool:TpvVulkanDescriptorPool;
    VulkanDescriptorSet:TpvVulkanDescriptorSet;
begin
 if assigned(fCurrentFillBuffer) and (fCurrentCountVertices>0) then begin

  while TPasMPInterlocked.CompareExchange(fCurrentFillBuffer^.fSpinLock,-1,0)<>0 do begin
  end;
  try

   CurrentVulkanBufferIndex:=fCurrentVulkanBufferIndex;

   fCurrentFillBuffer^.fCountUsedBuffers:=Max(fCurrentFillBuffer^.fCountUsedBuffers,CurrentVulkanBufferIndex+1);

   OldCount:=fCurrentFillBuffer^.fCountAllocatedBuffers;
   if OldCount<=CurrentVulkanBufferIndex then begin
    NewCount:=(CurrentVulkanBufferIndex+1)*2;
    SetLength(fCurrentFillBuffer^.fVulkanVertexBuffers,NewCount);
    SetLength(fCurrentFillBuffer^.fVulkanIndexBuffers,NewCount);
    SetLength(fCurrentFillBuffer^.fVertexBuffers,NewCount);
    SetLength(fCurrentFillBuffer^.fVertexBufferSizes,NewCount);
    SetLength(fCurrentFillBuffer^.fIndexBuffers,NewCount);
    SetLength(fCurrentFillBuffer^.fIndexBufferSizes,NewCount);
    FillChar(fCurrentFillBuffer^.fVulkanVertexBuffers[OldCount],(NewCount-OldCount)*SizeOf(TpvVulkanBuffer),#0);
    FillChar(fCurrentFillBuffer^.fVulkanIndexBuffers[OldCount],(NewCount-OldCount)*SizeOf(TpvVulkanBuffer),#0);
    FillChar(fCurrentFillBuffer^.fVertexBufferSizes[OldCount],(NewCount-OldCount)*SizeOf(TVkSizeInt),#0);
    FillChar(fCurrentFillBuffer^.fIndexBufferSizes[OldCount],(NewCount-OldCount)*SizeOf(TVkSizeInt),#0);
    fCurrentFillBuffer^.fCountAllocatedBuffers:=NewCount;
   end;

   inc(fCurrentFillBuffer^.fVertexBufferSizes[CurrentVulkanBufferIndex],fCurrentCountVertices*SizeOf(TpvCanvasVertex));

   inc(fCurrentFillBuffer^.fIndexBufferSizes[CurrentVulkanBufferIndex],fCurrentCountIndices*SizeOf(TpvUInt32));

   if not fVulkanTextureDescriptorSetHashMap.TryGet(fLastArrayTexture,DescriptorIndex) then begin
    DescriptorIndex:=fCountVulkanDescriptors;
    inc(fCountVulkanDescriptors);
    if length(fVulkanDescriptorPools)<fCountVulkanDescriptors then begin
     SetLength(fVulkanDescriptorPools,fCountVulkanDescriptors*2);
    end;
    if length(fVulkanDescriptorSets)<fCountVulkanDescriptors then begin
     SetLength(fVulkanDescriptorSets,fCountVulkanDescriptors*2);
    end;
    VulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(fDevice,
                                                         TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                         2);
    fVulkanDescriptorPools[DescriptorIndex]:=VulkanDescriptorPool;
    VulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,1);
    VulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,1);
    VulkanDescriptorPool.Initialize;
    VulkanDescriptorSet:=TpvVulkanDescriptorSet.Create(VulkanDescriptorPool,
                                                       fVulkanDescriptorSetLayout);
    fVulkanDescriptorSets[DescriptorIndex]:=VulkanDescriptorSet;
    VulkanDescriptorSet.WriteToDescriptorSet(0,
                                             0,
                                             1,
                                             TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                             [fLastArrayTexture.Texture.DescriptorImageInfo],
                                             [],
                                             [],
                                             false
                                            );
    VulkanDescriptorSet.Flush;
    fVulkanTextureDescriptorSetHashMap.Add(fLastArrayTexture,DescriptorIndex);
   end;

   QueueItemIndex:=fCurrentFillBuffer^.fCountQueueItems;
   inc(fCurrentFillBuffer^.fCountQueueItems);
   if length(fCurrentFillBuffer^.fQueueItems)<fCurrentFillBuffer^.fCountQueueItems then begin
    SetLength(fCurrentFillBuffer^.fQueueItems,fCurrentFillBuffer^.fCountQueueItems*2);
   end;
   QueueItem:=@fCurrentFillBuffer^.fQueueItems[QueueItemIndex];
   QueueItem^.Kind:=vcqikNormal;
   QueueItem^.BufferIndex:=CurrentVulkanBufferIndex;
   QueueItem^.DescriptorIndex:=DescriptorIndex;
   QueueItem^.StartVertexIndex:=fCurrentVulkanVertexBufferOffset;
   QueueItem^.StartIndexIndex:=fCurrentVulkanIndexBufferOffset;
   QueueItem^.CountVertices:=fCurrentCountVertices;
   QueueItem^.CountIndices:=fCurrentCountIndices;
   QueueItem^.RenderingMode:=fRenderingMode;
   QueueItem^.BlendingMode:=fBlendingMode;
   QueueItem^.Scissor:=fScissor;

  finally
   TPasMPInterlocked.Exchange(fCurrentFillBuffer^.fSpinLock,0);
  end;

  inc(fCurrentVulkanVertexBufferOffset,fCurrentCountVertices);
  inc(fCurrentVulkanIndexBufferOffset,fCurrentCountIndices);

  fCurrentCountVertices:=0;
  fCurrentCountIndices:=0;

  fCurrentDestinationVertexBufferPointer:=@fCurrentFillBuffer^.fVertexBuffers[fCurrentVulkanBufferIndex][fCurrentVulkanVertexBufferOffset];
  fCurrentDestinationIndexBufferPointer:=@fCurrentFillBuffer^.fIndexBuffers[fCurrentVulkanBufferIndex][fCurrentVulkanIndexBufferOffset];

 end;
end;

procedure TpvCanvas.GetNextDestinationVertexBuffer;
var OldCount,NewCount:TpvInt32;
begin

 inc(fCurrentVulkanBufferIndex);

 fCurrentVulkanVertexBufferOffset:=0;
 fCurrentVulkanIndexBufferOffset:=0;

 OldCount:=fCurrentFillBuffer^.fCountAllocatedBuffers;
 if OldCount<=fCurrentVulkanBufferIndex then begin
  NewCount:=RoundUpToPowerOfTwo(fCurrentVulkanBufferIndex+1);
  SetLength(fCurrentFillBuffer^.fVulkanVertexBuffers,NewCount);
  SetLength(fCurrentFillBuffer^.fVulkanIndexBuffers,NewCount);
  SetLength(fCurrentFillBuffer^.fVertexBuffers,NewCount);
  SetLength(fCurrentFillBuffer^.fVertexBufferSizes,NewCount);
  SetLength(fCurrentFillBuffer^.fIndexBuffers,NewCount);
  SetLength(fCurrentFillBuffer^.fIndexBufferSizes,NewCount);
  FillChar(fCurrentFillBuffer^.fVulkanVertexBuffers[OldCount],(NewCount-OldCount)*SizeOf(TpvVulkanBuffer),#0);
  FillChar(fCurrentFillBuffer^.fVulkanIndexBuffers[OldCount],(NewCount-OldCount)*SizeOf(TpvVulkanBuffer),#0);
  FillChar(fCurrentFillBuffer^.fVertexBufferSizes[OldCount],(NewCount-OldCount)*SizeOf(TVkSizeInt),#0);
  FillChar(fCurrentFillBuffer^.fIndexBufferSizes[OldCount],(NewCount-OldCount)*SizeOf(TVkSizeInt),#0);
  fCurrentFillBuffer^.fCountAllocatedBuffers:=NewCount;
 end;

 fCurrentDestinationVertexBufferPointer:=@fCurrentFillBuffer^.fVertexBuffers[fCurrentVulkanBufferIndex][0];

 fCurrentDestinationIndexBufferPointer:=@fCurrentFillBuffer^.fIndexBuffers[fCurrentVulkanBufferIndex][0];

 fCurrentFillBuffer^.fVertexBufferSizes[fCurrentVulkanBufferIndex]:=0;

 fCurrentFillBuffer^.fIndexBufferSizes[fCurrentVulkanBufferIndex]:=0;

end;

procedure TpvCanvas.FlushAndGetNewDestinationVertexBufferIfNeeded(const aCountVerticesToCheck,aCountIndicesToCheck:TpvInt32);
const UntilCountVertices=SizeOf(TpvCanvasVertexBuffer) div SizeOf(TpvCanvasVertex);
      UntilCountIndices=SizeOf(TpvCanvasIndexBuffer) div SizeOf(TpvUInt32);
begin
 if ((fCurrentVulkanVertexBufferOffset+fCurrentCountVertices+aCountVerticesToCheck)>=UntilCountVertices) or
    ((fCurrentVulkanIndexBufferOffset+fCurrentCountIndices+aCountIndicesToCheck)>=UntilCountIndices) then begin
  Flush;
  GetNextDestinationVertexBuffer;
 end;
end;

function TpvCanvas.ClipCheck(const aX0,aY0,aX1,aY1:TpvFloat):boolean;
const Threshold=1e-6;
begin
 result:=(fUnscaledClipRect.LeftTop.x<=(aX1+Threshold)) and
         (aX0<=(fUnscaledClipRect.RightBottom.x+Threshold)) and
         (fUnscaledClipRect.LeftTop.y<=(aY1+Threshold)) and
         (aY0<=(fUnscaledClipRect.RightBottom.y+Threshold));
end;

procedure TpvCanvas.SetArrayTexture(const ArrayTexture:TpvSpriteAtlasArrayTexture);
begin
 if fLastArrayTexture<>ArrayTexture then begin
  Flush;
  fLastArrayTexture:=ArrayTexture;
  fInverseTextureWidth:=1.0/ArrayTexture.Width;
  fInverseTextureHeight:=1.0/ArrayTexture.Height;
 end;
end;

procedure TpvCanvas.SetScissor(const aScissor:TVkRect2D);
begin
 if (fScissor.offset.x<>aScissor.offset.x) or
    (fScissor.offset.y<>aScissor.offset.y) or
    (fScissor.extent.Width<>aScissor.extent.Width) or
    (fScissor.extent.Height<>aScissor.extent.Height) then begin
  Flush;
  fScissor:=aScissor;
 end;
end;

procedure TpvCanvas.SetScissor(const aLeft,aTop,aWidth,aHeight:TpvInt32);
var NewScissor:TVkRect2D;
begin
 NewScissor.offset.x:=aLeft;
 NewScissor.offset.y:=aTop;
 NewScissor.extent.Width:=aWidth;
 NewScissor.extent.Height:=aHeight;
 SetScissor(NewScissor);
end;

procedure TpvCanvas.SetClipRect(const aClipRect:TVkRect2D);
begin
 fUnscaledClipRect.LeftTop:=TpvVector2.Create(aClipRect.offset.x,aClipRect.offset.y);
 fUnscaledClipRect.RightBottom:=TpvVector2.Create(aClipRect.offset.x+TpvFloat(aClipRect.extent.width),aClipRect.offset.y+TpvFloat(aClipRect.extent.height));
 fClipRect.LeftTop:=TpvVector2.Create(((aClipRect.offset.x*fInverseWidth)-0.5)*2.0,((aClipRect.offset.y*fInverseHeight)-0.5)*2.0);
 fClipRect.RightBottom:=TpvVector2.Create((((aClipRect.offset.x+TpvFloat(aClipRect.extent.width))*fInverseWidth)-0.5)*2.0,(((aClipRect.offset.y+TpvFloat(aClipRect.extent.height))*fInverseHeight)-0.5)*2.0);
end;

procedure TpvCanvas.SetClipRect(const aClipRect:TpvRect);
begin
 fUnscaledClipRect:=aClipRect;
 fClipRect.LeftTop:=((aClipRect.LeftTop*TpvVector2.Create(fInverseWidth,fInverseHeight))-TpvVector2.Create(0.5,0.5))*2.0;
 fClipRect.RightBottom:=((aClipRect.RightBottom*TpvVector2.Create(fInverseWidth,fInverseHeight))-TpvVector2.Create(0.5,0.5))*2.0;
end;

procedure TpvCanvas.SetClipRect(const aLeft,aTop,aWidth,aHeight:TpvInt32);
begin
 fUnscaledClipRect.LeftTop:=TpvVector2.Create(aLeft,aTop);
 fUnscaledClipRect.RightBottom:=TpvVector2.Create(aLeft+aWidth,aTop+aHeight);
 fClipRect.LeftTop:=TpvVector2.Create(((aLeft*fInverseWidth)-0.5)*2.0,((aTop*fInverseHeight)-0.5)*2.0);
 fClipRect.RightBottom:=TpvVector2.Create((((aLeft+aWidth)*fInverseWidth)-0.5)*2.0,(((aTop+aHeight)*fInverseHeight)-0.5)*2.0);
end;

procedure TpvCanvas.Hook(const aHook:TpvCanvasHook;const aData:TpvPointer);
var QueueItemIndex:TpvInt32;
    QueueItem:PpvCanvasQueueItem;
begin
 if assigned(aHook) then begin

  Flush;

  QueueItemIndex:=fCurrentFillBuffer^.fCountQueueItems;
  inc(fCurrentFillBuffer^.fCountQueueItems);
  if length(fCurrentFillBuffer^.fQueueItems)<fCurrentFillBuffer^.fCountQueueItems then begin
   SetLength(fCurrentFillBuffer^.fQueueItems,fCurrentFillBuffer^.fCountQueueItems*2);
  end;
  QueueItem:=@fCurrentFillBuffer^.fQueueItems[QueueItemIndex];
  QueueItem^.Kind:=vcqikHook;
  QueueItem^.Hook:=aHook;
  QueueItem^.HookData:=aData;

 end;
end;

procedure TpvCanvas.DrawSprite(const Sprite:TpvSprite;const Src,Dest:TpvRect;const Color:TpvVector4);
const MinA=1.0/1024.0;
var tx1,ty1,tx2,ty2,xf,yf,sX0,sY0,sX1,sY1:TpvFloat;
    TempDest,TempSrc:TpvRect;
    VertexColor:TpvHalfFloatVector4;
begin
 if (abs(Color.a)>MinA) and
    ClipCheck(Dest.Left,Dest.Top,Dest.Right,Dest.Bottom) and
    (((Src.Right>=Sprite.TrimmedX) and (Src.Bottom>=Sprite.TrimmedY)) and
    (((not Sprite.Rotated) and (((Sprite.TrimmedX+Sprite.TrimmedWidth)>=Src.Left) and ((Sprite.TrimmedY+Sprite.TrimmedHeight)>=Src.Top))) or
     (Sprite.Rotated and (((Sprite.TrimmedX+Sprite.TrimmedHeight)>=Src.Left) and ((Sprite.TrimmedY+Sprite.TrimmedWidth)>=Src.Top))))) then begin
  VertexColor.r:=Color.r;
  VertexColor.g:=Color.g;
  VertexColor.b:=Color.b;
  VertexColor.a:=Color.a;
  SetArrayTexture(Sprite.ArrayTexture);
  FlushAndGetNewDestinationVertexBufferIfNeeded(4,6);
  if Sprite.Rotated then begin
   tx1:=Max(Sprite.TrimmedX,Src.Left);
   ty1:=Max(Sprite.TrimmedY,Src.Top);
   tx2:=Min((Sprite.TrimmedX+Sprite.TrimmedHeight),Src.Right);
   ty2:=Min((Sprite.TrimmedY+Sprite.TrimmedWidth),Src.Bottom);
   xf:=abs(Dest.Right-Dest.Left)/(Src.Right-Src.Left);
   yf:=abs(Dest.Bottom-Dest.Top)/(Src.Bottom-Src.Top);
   TempDest.Left:=Dest.Left+((tx1-Src.Left)*xf);
   TempDest.Right:=Dest.Right+((tx2-Src.Right)*xf);
   TempDest.Top:=Dest.Top+((ty1-Src.Top)*yf);
   TempDest.Bottom:=Dest.Bottom+((ty2-Src.Bottom)*yf);
{  if Dest.Left<=Dest.Right then begin
    TempDest.Left:=Dest.Left+((tx1-Src.Left)*xf);
    TempDest.Right:=Dest.Right+((tx2-Src.Right)*xf);
   end else begin
    TempDest.Left:=Dest.Left+((tx2-Src.Right)*xf);
    TempDest.Right:=Dest.Right+((tx1-Src.Left)*xf);
   end;
   if Dest.Top<=Dest.Bottom then begin
    TempDest.Top:=Dest.Top+((ty1-Src.Top)*yf);
    TempDest.Bottom:=Dest.Bottom+((ty2-Src.Bottom)*yf);
   end else begin
    TempDest.Top:=Dest.Top+((ty2-Src.Bottom)*yf);
    TempDest.Bottom:=Dest.Bottom+((ty1-Src.Top)*yf);
   end;}
   TempSrc.Left:=(tx1-Sprite.TrimmedX)+Sprite.x;
   TempSrc.Top:=(ty1-Sprite.TrimmedY)+Sprite.y;
   TempSrc.Right:=TempSrc.Left+(ty2-ty1);
   TempSrc.Bottom:=TempSrc.Top+(tx2-tx1);
   sX0:=TempSrc.Left*fInverseTextureWidth;
   sY0:=TempSrc.Top*fInverseTextureHeight;
   sX1:=TempSrc.Right*fInverseTextureWidth;
   sY1:=TempSrc.Bottom*fInverseTextureHeight;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+0]:=fCurrentCountVertices+0;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+1]:=fCurrentCountVertices+1;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+2]:=fCurrentCountVertices+2;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+3]:=fCurrentCountVertices+0;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+4]:=fCurrentCountVertices+2;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+5]:=fCurrentCountVertices+3;
   inc(fCurrentCountIndices,6);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position.x:=((TempDest.Left*fInverseWidth)-0.5)*2;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position.y:=((TempDest.Top*fInverseHeight)-0.5)*2;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=Sprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=fState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fClipRect;
   inc(fCurrentCountVertices);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position.x:=((TempDest.Right*fInverseWidth)-0.5)*2;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position.y:=((TempDest.Top*fInverseHeight)-0.5)*2;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=Sprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=fState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fClipRect;
   inc(fCurrentCountVertices);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position.x:=((TempDest.Right*fInverseWidth)-0.5)*2;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position.y:=((TempDest.Bottom*fInverseHeight)-0.5)*2;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=Sprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=fState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fClipRect;
   inc(fCurrentCountVertices);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position.x:=((TempDest.Left*fInverseWidth)-0.5)*2;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position.y:=((TempDest.Bottom*fInverseHeight)-0.5)*2;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=Sprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=fState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fClipRect;
   inc(fCurrentCountVertices);
   inc(fCurrentCountIndices,6);
  end else begin
   tx1:=Max(Sprite.TrimmedX,Src.Left);
   ty1:=Max(Sprite.TrimmedY,Src.Top);
   tx2:=Min((Sprite.TrimmedX+Sprite.TrimmedWidth),Src.Right);
   ty2:=Min((Sprite.TrimmedY+Sprite.TrimmedHeight),Src.Bottom);
   xf:=abs(Dest.Right-Dest.Left)/(Src.Right-Src.Left);
   yf:=abs(Dest.Bottom-Dest.Top)/(Src.Bottom-Src.Top);
   TempDest.Left:=Dest.Left+((tx1-Src.Left)*xf);
   TempDest.Right:=Dest.Right+((tx2-Src.Right)*xf);
   TempDest.Top:=Dest.Top+((ty1-Src.Top)*yf);
   TempDest.Bottom:=Dest.Bottom+((ty2-Src.Bottom)*yf);
{  if Dest.Left<=Dest.Right then begin
    TempDest.Left:=Dest.Left+((tx1-Src.Left)*xf);
    TempDest.Right:=Dest.Right+((tx2-Src.Right)*xf);
   end else begin
    TempDest.Left:=Dest.Left+((tx2-Src.Right)*xf);
    TempDest.Right:=Dest.Right+((tx1-Src.Left)*xf);
   end;
   if Dest.Top<=Dest.Bottom then begin
    TempDest.Top:=Dest.Top+((ty1-Src.Top)*yf);
    TempDest.Bottom:=Dest.Bottom+((ty2-Src.Bottom)*yf);
   end else begin
    TempDest.Top:=Dest.Bottom+((ty2-Src.Bottom)*yf);
    TempDest.Bottom:=Dest.Top+((ty1-Src.Top)*yf);
   end;}
   TempSrc.Left:=(tx1-Sprite.TrimmedX)+Sprite.x;
   TempSrc.Top:=(ty1-Sprite.TrimmedY)+Sprite.y;
   TempSrc.Right:=TempSrc.Left+(tx2-tx1);
   TempSrc.Bottom:=TempSrc.Top+(ty2-ty1);
   if TempDest.Left<fUnscaledClipRect.LeftTop.x then begin
    TempSrc.Left:=TempSrc.Left+((TempSrc.Right-TempSrc.Left)*((fUnscaledClipRect.LeftTop.x-TempDest.Left)/(TempDest.Right-TempDest.Left)));
    TempDest.Left:=fUnscaledClipRect.LeftTop.x;
   end;
   if TempDest.Top<fUnscaledClipRect.LeftTop.y then begin
    TempSrc.Top:=TempSrc.Top+((TempSrc.Bottom-TempSrc.Top)*((fUnscaledClipRect.LeftTop.y-TempDest.Top)/(TempDest.Bottom-TempDest.Top)));
    TempDest.Top:=fUnscaledClipRect.LeftTop.y;
   end;
   if TempDest.Right>fUnscaledClipRect.RightBottom.x then begin
    TempSrc.Right:=TempSrc.Left+((TempSrc.Right-TempSrc.Left)*((fUnscaledClipRect.RightBottom.x-TempDest.Left)/(TempDest.Right-TempDest.Left)));
    TempDest.Right:=fUnscaledClipRect.RightBottom.x;
   end;
   if TempDest.Bottom>fUnscaledClipRect.RightBottom.y then begin
    TempSrc.Bottom:=TempSrc.Top+((TempSrc.Bottom-TempSrc.Top)*((fUnscaledClipRect.RightBottom.y-TempDest.Top)/(TempDest.Bottom-TempDest.Top)));
    TempDest.Bottom:=fUnscaledClipRect.RightBottom.y;
   end;
   sX0:=TempSrc.Left*fInverseTextureWidth;
   sY0:=TempSrc.Top*fInverseTextureHeight;
   sX1:=TempSrc.Right*fInverseTextureWidth;
   sY1:=TempSrc.Bottom*fInverseTextureHeight;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+0]:=fCurrentCountVertices+0;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+1]:=fCurrentCountVertices+1;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+2]:=fCurrentCountVertices+2;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+3]:=fCurrentCountVertices+0;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+4]:=fCurrentCountVertices+2;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+5]:=fCurrentCountVertices+3;
   inc(fCurrentCountIndices,6);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position.x:=((TempDest.Left*fInverseWidth)-0.5)*2;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position.y:=((TempDest.Top*fInverseHeight)-0.5)*2;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=Sprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=fState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fClipRect;
   inc(fCurrentCountVertices);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position.x:=((TempDest.Right*fInverseWidth)-0.5)*2;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position.y:=((TempDest.Top*fInverseHeight)-0.5)*2;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=Sprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=fState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fClipRect;
   inc(fCurrentCountVertices);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position.x:=((TempDest.Right*fInverseWidth)-0.5)*2;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position.y:=((TempDest.Bottom*fInverseHeight)-0.5)*2;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=Sprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=fState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fClipRect;
   inc(fCurrentCountVertices);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position.x:=((TempDest.Left*fInverseWidth)-0.5)*2;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position.y:=((TempDest.Bottom*fInverseHeight)-0.5)*2;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=Sprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=fState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fClipRect;
   inc(fCurrentCountVertices);
   inc(fCurrentCountIndices,6);
  end;
 end;
end;

procedure TpvCanvas.DrawSprite(const Sprite:TpvSprite;const Src,Dest:TpvRect;const Origin:TpvVector2;Rotation:TpvFloat;const Color:TpvVector4);
const MinA=1.0/1024.0;
var Cosinus,Sinus,tx1,ty1,tx2,ty2,xf,yf,sX0,sY0,sX1,sY1:TpvFloat;
    AroundPoint:TpvVector2;
    Points:array[0..3] of TpvVector2;
    TempDest,TempSrc:TpvRect;
    VertexColor:TpvHalfFloatVector4;
begin
 if (abs(Color.a)>MinA) and
    (((Src.Right>=Sprite.TrimmedX) and (Src.Bottom>=Sprite.TrimmedY)) and
    (((not Sprite.Rotated) and (((Sprite.TrimmedX+Sprite.TrimmedWidth)>=Src.Left) and ((Sprite.TrimmedY+Sprite.TrimmedHeight)>=Src.Top))) or
     (Sprite.Rotated and (((Sprite.TrimmedX+Sprite.TrimmedHeight)>=Src.Left) and ((Sprite.TrimmedY+Sprite.TrimmedWidth)>=Src.Top))))) then begin
  VertexColor.r:=Color.r;
  VertexColor.g:=Color.g;
  VertexColor.b:=Color.b;
  VertexColor.a:=Color.a;
  Cosinus:=cos(Rotation);
  Sinus:=sin(Rotation);
  SetArrayTexture(Sprite.ArrayTexture);
  FlushAndGetNewDestinationVertexBufferIfNeeded(4,6);
  if Sprite.Rotated then begin
   tx1:=Max(Sprite.TrimmedX,Src.Left);
   ty1:=Max(Sprite.TrimmedY,Src.Top);
   tx2:=Min((Sprite.TrimmedX+Sprite.TrimmedHeight),Src.Right);
   ty2:=Min((Sprite.TrimmedY+Sprite.TrimmedWidth),Src.Bottom);
   xf:=(Dest.Right-Dest.Left)/(Src.Right-Src.Left);
   yf:=(Dest.Bottom-Dest.Top)/(Src.Bottom-Src.Top);
   TempDest.Left:=Dest.Left+((tx1-Src.Left)*xf);
   TempDest.Top:=Dest.Top+((ty1-Src.Top)*yf);
   TempDest.Right:=Dest.Right+((tx2-Src.Right)*xf);
   TempDest.Bottom:=Dest.Bottom+((ty2-Src.Bottom)*yf);
   TempSrc.Left:=(tx1-Sprite.TrimmedX)+Sprite.x;
   TempSrc.Top:=(ty1-Sprite.TrimmedY)+Sprite.y;
   TempSrc.Right:=TempSrc.Left+(ty2-ty1);
   TempSrc.Bottom:=TempSrc.Top+(tx2-tx1);
   AroundPoint.x:=TempDest.Left+Origin.x;
   AroundPoint.y:=TempDest.Top+Origin.y;
   Points[0].x:=TempDest.Left;
   Points[0].y:=TempDest.Top;
   Points[1].x:=TempDest.Right;
   Points[1].y:=TempDest.Top;
   Points[2].x:=TempDest.Right;
   Points[2].y:=TempDest.Bottom;
   Points[3].x:=TempDest.Left;
   Points[3].y:=TempDest.Bottom;
   sX0:=TempSrc.Left*fInverseTextureWidth;
   sY0:=TempSrc.Top*fInverseTextureHeight;
   sX1:=TempSrc.Right*fInverseTextureWidth;
   sY1:=TempSrc.Bottom*fInverseTextureHeight;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+0]:=fCurrentCountVertices+0;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+1]:=fCurrentCountVertices+1;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+2]:=fCurrentCountVertices+2;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+3]:=fCurrentCountVertices+0;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+4]:=fCurrentCountVertices+2;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+5]:=fCurrentCountVertices+3;
   inc(fCurrentCountIndices,6);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position:=RotatePoint(Points[0],AroundPoint,Cosinus,Sinus);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=Sprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=fState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fClipRect;
   inc(fCurrentCountVertices);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position:=RotatePoint(Points[1],AroundPoint,Cosinus,Sinus);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=Sprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=fState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fClipRect;
   inc(fCurrentCountVertices);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position:=RotatePoint(Points[2],AroundPoint,Cosinus,Sinus);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=Sprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=fState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fClipRect;
   inc(fCurrentCountVertices);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position:=RotatePoint(Points[3],AroundPoint,Cosinus,Sinus);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=Sprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=fState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fClipRect;
   inc(fCurrentCountVertices);
   inc(fCurrentCountIndices,6);
  end else begin
   tx1:=Max(Sprite.TrimmedX,Src.Left);
   ty1:=Max(Sprite.TrimmedY,Src.Top);
   tx2:=Min((Sprite.TrimmedX+Sprite.TrimmedWidth),Src.Right);
   ty2:=Min((Sprite.TrimmedY+Sprite.TrimmedHeight),Src.Bottom);
   xf:=(Dest.Right-Dest.Left)/(Src.Right-Src.Left);
   yf:=(Dest.Bottom-Dest.Top)/(Src.Bottom-Src.Top);
   TempDest.Left:=Dest.Left+((tx1-Src.Left)*xf);
   TempDest.Top:=Dest.Top+((ty1-Src.Top)*yf);
   TempDest.Right:=Dest.Right+((tx2-Src.Right)*xf);
   TempDest.Bottom:=Dest.Bottom+((ty2-Src.Bottom)*yf);
   TempSrc.Left:=(tx1-Sprite.TrimmedX)+Sprite.x;
   TempSrc.Top:=(ty1-Sprite.TrimmedY)+Sprite.y;
   TempSrc.Right:=TempSrc.Left+(tx2-tx1);
   TempSrc.Bottom:=TempSrc.Top+(ty2-ty1);
   if Rotation=0.0 then begin
    if TempDest.Left<fUnscaledClipRect.LeftTop.x then begin
     TempSrc.Left:=TempSrc.Left+((TempSrc.Right-TempSrc.Left)*((fUnscaledClipRect.LeftTop.x-TempDest.Left)/(TempDest.Right-TempDest.Left)));
     TempDest.Left:=fUnscaledClipRect.LeftTop.x;
    end;
    if TempDest.Top<fUnscaledClipRect.LeftTop.y then begin
     TempSrc.Top:=TempSrc.Top+((TempSrc.Bottom-TempSrc.Top)*((fUnscaledClipRect.LeftTop.y-TempDest.Top)/(TempDest.Bottom-TempDest.Top)));
     TempDest.Top:=fUnscaledClipRect.LeftTop.y;
    end;
    if TempDest.Right>fUnscaledClipRect.RightBottom.x then begin
     TempSrc.Right:=TempSrc.Left+((TempSrc.Right-TempSrc.Left)*((fUnscaledClipRect.RightBottom.x-TempDest.Left)/(TempDest.Right-TempDest.Left)));
     TempDest.Right:=fUnscaledClipRect.RightBottom.x;
    end;
    if TempDest.Bottom>fUnscaledClipRect.RightBottom.y then begin
     TempSrc.Bottom:=TempSrc.Top+((TempSrc.Bottom-TempSrc.Top)*((fUnscaledClipRect.RightBottom.y-TempDest.Top)/(TempDest.Bottom-TempDest.Top)));
     TempDest.Bottom:=fUnscaledClipRect.RightBottom.y;
    end;
   end;
   AroundPoint.x:=TempDest.Left+Origin.x;
   AroundPoint.y:=TempDest.Top+Origin.y;
   Points[0].x:=TempDest.Left;
   Points[0].y:=TempDest.Top;
   Points[1].x:=TempDest.Right;
   Points[1].y:=TempDest.Top;
   Points[2].x:=TempDest.Right;
   Points[2].y:=TempDest.Bottom;
   Points[3].x:=TempDest.Left;
   Points[3].y:=TempDest.Bottom;
   sX0:=TempSrc.Left*fInverseTextureWidth;
   sY0:=TempSrc.Top*fInverseTextureHeight;
   sX1:=TempSrc.Right*fInverseTextureWidth;
   sY1:=TempSrc.Bottom*fInverseTextureHeight;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+0]:=fCurrentCountVertices+0;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+1]:=fCurrentCountVertices+1;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+2]:=fCurrentCountVertices+2;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+3]:=fCurrentCountVertices+0;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+4]:=fCurrentCountVertices+2;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+5]:=fCurrentCountVertices+3;
   inc(fCurrentCountIndices,6);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position:=RotatePoint(Points[0],AroundPoint,Cosinus,Sinus);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=Sprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=fState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fClipRect;
   inc(fCurrentCountVertices);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position:=RotatePoint(Points[1],AroundPoint,Cosinus,Sinus);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=Sprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=fState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fClipRect;
   inc(fCurrentCountVertices);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position:=RotatePoint(Points[2],AroundPoint,Cosinus,Sinus);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=Sprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=fState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fClipRect;
   inc(fCurrentCountVertices);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position:=RotatePoint(Points[3],AroundPoint,Cosinus,Sinus);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=Sprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=fState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fClipRect;
   inc(fCurrentCountVertices);
   inc(fCurrentCountIndices,6);
  end;
 end;
end;

procedure TpvCanvas.DrawSprite(const Sprite:TpvSprite;const x,y:TpvFloat;const Color:TpvVector4);
var Src,Dest:TpvRect;
begin
 Src.Left:=0;
 Src.Top:=0;
 Src.Right:=Sprite.Width;
 Src.Bottom:=Sprite.Height;
 Dest.Left:=x;
 Dest.Top:=y;
 Dest.Right:=x+Sprite.Width;
 Dest.Bottom:=y+Sprite.Height;
 DrawSprite(Sprite,Src,Dest,Color);
end;

procedure TpvCanvas.DrawSprite(const Sprite:TpvSprite;const x,y:TpvFloat);
var Color:TpvVector4;
begin
 Color.r:=1;
 Color.g:=1;
 Color.b:=1;
 Color.a:=1;
 DrawSprite(Sprite,x,y,Color);
end;

procedure TpvCanvas.DrawSprite(const Sprite:TpvSprite;const sx1,sy1,sx2,sy2,dx1,dy1,dx2,dy2,Alpha:TpvFloat);
var Src,Dest:TpvRect;
    Color:TpvVector4;
begin
 Dest.Left:=dx1;
 Dest.Top:=dy1;
 Dest.Right:=dx2;
 Dest.Bottom:=dy2;
 Src.Left:=sx1;
 Src.Top:=sy1;
 Src.Right:=sx2;
 Src.Bottom:=sy2;
 Color.r:=1;
 Color.g:=1;
 Color.b:=1;
 Color.a:=Alpha;
 DrawSprite(Sprite,Src,Dest,Color);
end;

procedure TpvCanvas.ExecuteUpload(const aVulkanCommandBuffer:TpvVulkanCommandBuffer;const aBufferIndex:TpvInt32);
var Index:TpvInt32;
    CurrentDrawSpriteBatchBuffer:PpvCanvasBuffer;
    VulkanBuffer:TpvVulkanBuffer;
begin
 CurrentDrawSpriteBatchBuffer:=@fVulkanCanvasBuffers[aBufferIndex];
 if assigned(CurrentDrawSpriteBatchBuffer) and (CurrentDrawSpriteBatchBuffer^.fCountUsedBuffers>0) then begin
  while TPasMPInterlocked.CompareExchange(CurrentDrawSpriteBatchBuffer^.fSpinLock,-1,0)<>0 do begin
  end;
  try
   for Index:=0 to CurrentDrawSpriteBatchBuffer^.fCountUsedBuffers-1 do begin
    if CurrentDrawSpriteBatchBuffer^.fVertexBufferSizes[Index]>0 then begin
     VulkanBuffer:=CurrentDrawSpriteBatchBuffer^.fVulkanVertexBuffers[Index];
     if not assigned(VulkanBuffer) then begin
      VulkanBuffer:=TpvVulkanBuffer.Create(fDevice,
                                         SizeOf(TpvCanvasVertexBuffer),
                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT),
                                         TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                         nil,
                                         TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) {or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)},
                                         0,
                                         0,
                                         0,
                                         0,
                                         0,
                                         [vbfPersistentMapped]
                                        );
      CurrentDrawSpriteBatchBuffer^.fVulkanVertexBuffers[Index]:=VulkanBuffer;
     end;
     if assigned(VulkanBuffer) then begin
      VulkanBuffer.UploadData(fTransferQueue,
                              fTransferCommandBuffer,
                              fTransferFence,
                              CurrentDrawSpriteBatchBuffer^.fVertexBuffers[Index,0],
                              0,
                              CurrentDrawSpriteBatchBuffer^.fVertexBufferSizes[Index],
                              vbutsbmNo);
     end;
    end;
    if CurrentDrawSpriteBatchBuffer^.fIndexBufferSizes[Index]>0 then begin
     VulkanBuffer:=CurrentDrawSpriteBatchBuffer^.fVulkanIndexBuffers[Index];
     if not assigned(VulkanBuffer) then begin
      VulkanBuffer:=TpvVulkanBuffer.Create(fDevice,
                                         SizeOf(TpvCanvasIndexBuffer),
                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT),
                                         TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                         nil,
                                         TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) {or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)},
                                         0,
                                         0,
                                         0,
                                         0,
                                         0,
                                         [vbfPersistentMapped]
                                        );
      CurrentDrawSpriteBatchBuffer^.fVulkanIndexBuffers[Index]:=VulkanBuffer;
     end;
     if assigned(VulkanBuffer) then begin
      VulkanBuffer.UploadData(fTransferQueue,
                              fTransferCommandBuffer,
                              fTransferFence,
                              CurrentDrawSpriteBatchBuffer^.fIndexBuffers[Index,0],
                              0,
                              CurrentDrawSpriteBatchBuffer^.fIndexBufferSizes[Index],
                              vbutsbmNo);
     end;
    end;
   end;
  finally
   TPasMPInterlocked.Exchange(CurrentDrawSpriteBatchBuffer^.fSpinLock,0);
  end;
{ aVulkanCommandBuffer.MetaCmdMemoryBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
                                            TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT),
                                            TVkAccessFlags(VK_ACCESS_UNIFORM_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT));}
 end;
end;

procedure TpvCanvas.ExecuteDraw(const aVulkanCommandBuffer:TpvVulkanCommandBuffer;const aBufferIndex:TpvInt32);
var Index,DescriptorIndex,StartVertexIndex:TpvInt32;
    QueueItem:PpvCanvasQueueItem;
    OldQueueItemKind:TpvCanvasQueueItemKind;
    CurrentDrawSpriteBatchBuffer:PpvCanvasBuffer;
    VulkanVertexBuffer,VulkanIndexBuffer:TpvVulkanBuffer;
    OldScissor:TVkRect2D;
    ForceUpdate:boolean;
    Offsets:array[0..0] of TVkDeviceSize;
begin
 CurrentDrawSpriteBatchBuffer:=@fVulkanCanvasBuffers[aBufferIndex];
 if assigned(CurrentDrawSpriteBatchBuffer) and (CurrentDrawSpriteBatchBuffer^.fCountQueueItems>0) then begin

  OldScissor.offset.x:=-$7fffffff;
  OldScissor.offset.y:=-$7fffffff;
  OldScissor.extent.Width:=$7fffffff;
  OldScissor.extent.Height:=$7fffffff;

  DescriptorIndex:=-1;

  OldQueueItemKind:=vcqikNone;

  ForceUpdate:=true;

  for Index:=0 to CurrentDrawSpriteBatchBuffer^.fCountQueueItems-1 do begin

   QueueItem:=@CurrentDrawSpriteBatchBuffer^.fQueueItems[Index];

   if OldQueueItemKind<>QueueItem^.Kind then begin
    OldQueueItemKind:=QueueItem^.Kind;
    ForceUpdate:=true;
   end;

   case QueueItem^.Kind of
    vcqikNormal:begin

     VulkanVertexBuffer:=CurrentDrawSpriteBatchBuffer^.fVulkanVertexBuffers[QueueItem^.BufferIndex];

     VulkanIndexBuffer:=CurrentDrawSpriteBatchBuffer^.fVulkanIndexBuffers[QueueItem^.BufferIndex];

     if ForceUpdate then begin
      aVulkanCommandBuffer.CmdSetViewport(0,1,fPointerToViewport);
     end;

     if ForceUpdate or
        (DescriptorIndex<>QueueItem^.DescriptorIndex) then begin
      DescriptorIndex:=QueueItem^.DescriptorIndex;
      aVulkanCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanPipelineLayout.Handle,0,1,@fVulkanDescriptorSets[DescriptorIndex].Handle,0,nil);
     end;

     if ForceUpdate then begin
      aVulkanCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
      OldScissor.offset.x:=-$7fffffff;
      OldScissor.offset.y:=-$7fffffff;
      OldScissor.extent.Width:=$7fffffff;
      OldScissor.extent.Height:=$7fffffff;
     end;

     if ForceUpdate or
        (OldScissor.offset.x<>QueueItem^.Scissor.offset.x) or
        (OldScissor.offset.y<>QueueItem^.Scissor.offset.y) or
        (OldScissor.extent.Width<>QueueItem^.Scissor.extent.Width) or
        (OldScissor.extent.Height<>QueueItem^.Scissor.extent.Height) then begin
      OldScissor:=QueueItem^.Scissor;
      aVulkanCommandBuffer.CmdSetScissor(0,1,@QueueItem^.Scissor);
     end;

     Offsets[0]:=QueueItem^.StartVertexIndex*SizeOf(TpvCanvasVertex);
     aVulkanCommandBuffer.CmdBindVertexBuffers(0,1,@VulkanVertexBuffer.Handle,@Offsets);

     aVulkanCommandBuffer.CmdBindIndexBuffer(VulkanIndexBuffer.Handle,QueueItem^.StartIndexIndex*SizeOf(TpvUInt32),VK_INDEX_TYPE_UINT32);
     aVulkanCommandBuffer.CmdDrawIndexed(QueueItem^.CountIndices,1,0,0,0);

     ForceUpdate:=false;

    end;
    vcqikVector:begin
     // TODO
    end;
    vcqikHook:begin
     if assigned(QueueItem^.Hook) then begin
      QueueItem^.Hook(QueueItem^.HookData);
     end;
     ForceUpdate:=true;
    end;
    else {vcqikNone:}begin
     ForceUpdate:=true;
    end;
   end;

  end;

  CurrentDrawSpriteBatchBuffer^.fCountQueueItems:=0;
  CurrentDrawSpriteBatchBuffer^.fCountUsedBuffers:=0;

 end;
end;

end.
