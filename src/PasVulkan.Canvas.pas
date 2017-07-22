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
     Generics.Collections,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Collections,
     PasVulkan.Framework,
     PasVulkan.Sprites,
     PasVulkan.Font;

type PpvCanvasRenderingMode=^TpvCanvasRenderingMode;
     TpvCanvasRenderingMode=
      (
       pvcrmNormal,
       pvcrmFont
      );

     PpvCanvasBlendingMode=^TpvCanvasBlendingMode;
     TpvCanvasBlendingMode=
      (
       pvcbmNone,
       pvcbmAlphaBlending,
       pvcbmAdditiveBlending
      );

     PpvCanvasLineJoin=^TpvCanvasLineJoin;
     TpvCanvasLineJoin=
      (
       pvcljBevel,
       pvcljMiter,
       pvcljRound
      );

     PpvCanvasLineCap=^TpvCanvasLineCap;
     TpvCanvasLineCap=
      (
       pvclcButt,
       pvclcSquare,
       pvclcRound
      );

     PpvCanvasFillRule=^TpvCanvasFillRule;
     TpvCanvasFillRule=
      (
       pvcfrDoNotMatter, // for pure raw speed, where is no guarantee winding fill rule correctness of triangulation
       pvcfrNonZero,
       pvcfrEvenOdd
      );

     PpvCanvasFillStyle=^TpvCanvasFillStyle;
     TpvCanvasFillStyle=
      (
       pvcfsClear,
       pvcfsSolid
      );

     PpvCanvasStrokeStyle=^TpvCanvasStrokeStyle;
     TpvCanvasStrokeStyle=
      (
       pvcssClear,
       pvcssSolid
      );

     PpvCanvasTextHorizontalAlignment=^TpvCanvasTextHorizontalAlignment;
     TpvCanvasTextHorizontalAlignment=
      (
       pvcthaLeft,
       pvcthaCenter,
       pvcthaRight
      );

     ppvCanvasTextVerticalAlignment=^TpvCanvasTextVerticalAlignment;
     TpvCanvasTextVerticalAlignment=
      (
       pvctvaTop,
       pvctvaMiddle,
       pvctvaBottom
      );

     PpvCanvasPathCommandType=^TpvCanvasPathCommandType;
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

     TpvCanvasPath=class(TPersistent)
      private
       fCommands:TpvCanvasPathCommands;
       fCountCommands:TpvInt32;
       function NewCommand:PpvCanvasPathCommand;
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       procedure Assign(aSource:TPersistent); override;
       function BeginPath:TpvCanvasPath;
       function EndPath:TpvCanvasPath;
       function ClosePath:TpvCanvasPath;
       function MoveTo(const aP0:TpvVector2):TpvCanvasPath;
       function LineTo(const aP0:TpvVector2):TpvCanvasPath;
       function QuadraticCurveTo(const aC0,aA0:TpvVector2):TpvCanvasPath;
       function CubicCurveTo(const aC0,aC1,aA0:TpvVector2):TpvCanvasPath;
     end;

     TpvCanvasState=class(TPersistent)
      private
       fBlendingMode:TpvCanvasBlendingMode;
       fLineWidth:TpvFloat;
       fMiterLimit:TpvFloat;
       fLineJoin:TpvCanvasLineJoin;
       fLineCap:TpvCanvasLineCap;
       fFillRule:TpvCanvasFillRule;
       fColor:TpvVector4;
       fClipRect:TpvRect;
       fScissor:TVkRect2D;
       fProjectionMatrix:TpvMatrix4x4;
       fViewMatrix:TpvMatrix4x4;
       fModelMatrix:TpvMatrix3x3;
       fFont:TpvFont;
       fFontSize:TpvFloat;
       fTextHorizontalAlignment:TpvCanvasTextHorizontalAlignment;
       fTextVerticalAlignment:TpvCanvasTextVerticalAlignment;
       fPath:TpvCanvasPath;
       procedure Reset;
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       procedure Assign(aSource:TPersistent); override;
      public
       property Color:TpvVector4 read fColor write fColor;
       property ClipRect:TpvRect read fClipRect write fClipRect;
       property Scissor:TVkRect2D read fScissor write fScissor;
       property ProjectionMatrix:TpvMatrix4x4 read fProjectionMatrix write fProjectionMatrix;
       property ViewMatrix:TpvMatrix4x4 read fViewMatrix write fViewMatrix;
       property ModelMatrix:TpvMatrix3x3 read fModelMatrix write fModelMatrix;
      published
       property BlendingMode:TpvCanvasBlendingMode read fBlendingMode write fBlendingMode;
       property LineWidth:TpvFloat read fLineWidth write fLineWidth;
       property MiterLimit:TpvFloat read fMiterLimit write fMiterLimit;
       property LineJoin:TpvCanvasLineJoin read fLineJoin write fLineJoin;
       property LineCap:TpvCanvasLineCap read fLineCap write fLineCap;
       property FillRule:TpvCanvasFillRule read fFillRule write fFillRule;
       property Font:TpvFont read fFont write fFont;
       property FontSize:TpvFloat read fFontSize write fFontSize;
       property TextHorizontalAlignment:TpvCanvasTextHorizontalAlignment read fTextHorizontalAlignment write fTextHorizontalAlignment;
       property TextVerticalAlignment:TpvCanvasTextVerticalAlignment read fTextVerticalAlignment write fTextVerticalAlignment;
       property Path:TpvCanvasPath read fPath write fPath;
     end;

     TpvCanvasStateStack=class(TObjectStack<TpvCanvasState>);

     PpvCanvasShapeCacheVertex=^TpvCanvasShapeCacheVertex;
     TpvCanvasShapeCacheVertex=record
      ObjectMode:TVkUInt32;
      Position:TpvVector2;
      MetaInfo:TpvVector4;
     end;

     PpvCanvasShapeCacheLinePoint=^TpvCanvasShapeCacheLinePoint;
     TpvCanvasShapeCacheLinePoint=record
      Position:TpvVector2;
      Middle:TpvVector2;
     end;

     TpvCanvasShapeCacheLinePoints=array of TpvCanvasShapeCacheLinePoint;

     PpvCanvasShapeCacheVertices=^TpvCanvasShapeCacheVertices;
     TpvCanvasShapeCacheVertices=array of TpvCanvasShapeCacheVertex;

     TpvCanvasShapeCacheIndices=array of TpvUInt32;

     PpvCanvasShapeCacheGroup=^TpvCanvasShapeCacheGroup;
     TpvCanvasShapeCacheGroup=record
      Vertices:TpvCanvasShapeCacheVertices;
      Indices:TpvCanvasShapeCacheIndices;
      CountVertices:TpvInt32;
      CountIndices:TpvInt32;
     end;

     TpvCanvasShapeCacheGroups=array of TpvCanvasShapeCacheGroup;

     TpvCanvasShape=class
      private
       fCacheGroups:TpvCanvasShapeCacheGroups;
       fCountCacheGroups:TpvInt32;
       fCacheLinePoints:TpvCanvasShapeCacheLinePoints;
       fCountCacheLinePoints:TpvInt32;
       function NewCacheGroup(const aStartAllocationCountVertices:TpvInt32=0;const aStartAllocationCountIndices:TpvInt32=0):PpvCanvasShapeCacheGroup;
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       procedure StrokeFromPath(const aPath:TpvCanvasPath;const aState:TpvCanvasState);
       procedure FillFromPath(const aPath:TpvCanvasPath;const aState:TpvCanvasState);
     end;

     PpvCanvasVertex=^TpvCanvasVertex;
     TpvCanvasVertex=packed record               //   Size                                    Offset
      Position:TpvVector2;                       //    8 bytes (2x 32-bit floats)           =  0
      Color:TpvHalfFloatVector4;                 // +  8 bytes (4x 16-bit half-floats)      =  8 (=> 8 byte aligned)
      TextureCoord:TpvVector3;                   // + 12 bytes (3x 32-bit floats)           = 16 (=> 16 byte aligned)
      State:TpvUInt32;                           // +  4 bytes (1x 32-bit unsigned integer) = 28 (=> 4 byte aligned)
      ClipRect:TpvRect;                          // + 16 bytes (4x 32-bit floats)           = 32 (=> 32 byte aligned)
      MetaInfo:TpvVector4;                       // + 16 bytes (4x 32-bit floats)           = 48 (=> 32 byte aligned)
     end;                                        // = 64 bytes per vertex

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

     TpvCanvasHook=procedure(const aData:TpvPointer) of object;

     TpvCanvasQueueItemKind=
      (
       pvcqikNone,
       pvcqikNormal,
       pvcqikVector,
       pvcqikHook
      );

     PpvCanvasQueueItem=^TpvCanvasQueueItem;
     TpvCanvasQueueItem=record
      Kind:TpvCanvasQueueItemKind;
      BufferIndex:TpvInt32;
      DescriptorIndex:TpvInt32;
      TextureMode:TPasMPBool32;
      StartVertexIndex:TpvInt32;
      StartIndexIndex:TpvInt32;
      CountVertices:TpvInt32;
      CountIndices:TpvInt32;
      Scissor:TVkRect2D;
      Matrix:TpvMatrix4x4;
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

     TpvCanvasTextureDescriptorSetHashMap=class(TpvHashMap<TObject,TpvInt32>);

     PpvCanvasRenderingModeValues=^TpvCanvasRenderingModeValues;
     TpvCanvasRenderingModeValues=array[TpvCanvasRenderingMode] of TpvUInt32;

     PpvCanvasBlendingModeValues=^TpvCanvasBlendingModeValues;
     TpvCanvasBlendingModeValues=array[TpvCanvasBlendingMode] of TpvUInt32;

     TpvCanvasCommon=class
      private
       class var fLock:TPasMPInt32;
      private
       fDevice:TpvVulkanDevice;
       fReferenceCounter:TpvInt32;
       fCanvasVertexShaderModule:TpvVulkanShaderModule;
       fCanvasFragmentAtlasTextureShaderModule:TpvVulkanShaderModule;
       fCanvasFragmentNoTextureShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineCanvasShaderStageVertex:TpvVulkanPipelineShaderStage;
       fVulkanPipelineCanvasShaderStageFragmentAtlasTexture:TpvVulkanPipelineShaderStage;
       fVulkanPipelineCanvasShaderStageFragmentNoTexture:TpvVulkanPipelineShaderStage;
      public
       constructor Create(const aDevice:TpvVulkanDevice); reintroduce;
       destructor Destroy; override;
       class function Acquire(const aDevice:TpvVulkanDevice):TpvCanvasCommon;
       class procedure Release(const aDevice:TpvVulkanDevice);
     end;

     TpvCanvasVulkanPipelineLayouts=array[boolean] of TpvVulkanPipelineLayout;

     TpvCanvasVulkanGraphicsPipelines=array[boolean] of TpvVulkanGraphicsPipeline;

     TpvCanvas=class
      private
       fDevice:TpvVulkanDevice;
       fCanvasCommon:TpvCanvasCommon;
       fGraphicsQueue:TpvVulkanQueue;
       fGraphicsCommandBuffer:TpvVulkanCommandBuffer;
       fGraphicsFence:TpvVulkanFence;
       fTransferQueue:TpvVulkanQueue;
       fTransferCommandBuffer:TpvVulkanCommandBuffer;
       fTransferFence:TpvVulkanFence;
       fPipelineCache:TpvVulkanPipelineCache;
       fVulkanDescriptorPools:TpvCanvasDescriptorPools;
       fVulkanDescriptorSetTextureLayout:TpvVulkanDescriptorSetLayout;
       fVulkanDescriptorSetNoTextureLayout:TpvVulkanDescriptorSetLayout;
       fVulkanDescriptorSets:TpvCanvasDescriptorSets;
       fCountVulkanDescriptors:TpvInt32;
       fVulkanTextureDescriptorSetHashMap:TpvCanvasTextureDescriptorSetHashMap;
       fVulkanRenderPass:TpvVulkanRenderPass;
       fVulkanPipelineLayouts:TpvCanvasVulkanPipelineLayouts;
       fVulkanGraphicsPipelines:TpvCanvasVulkanGraphicsPipelines;
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
       fCurrentCountVertices:TVkSizeInt;
       fCurrentCountIndices:TVkSizeInt;
       fCurrentDestinationVertexBufferPointer:PpvCanvasVertexBuffer;
       fCurrentDestinationIndexBufferPointer:PpvCanvasIndexBuffer;
       fInternalRenderingMode:TpvCanvasRenderingMode;
       fInternalTexture:TObject;
       fShape:TpvCanvasShape;
       fState:TpvCanvasState;
       fStateStack:TpvCanvasStateStack;
       procedure SetInternalTexture(const aTexture:TpvSpriteAtlasArrayTexture);
       function GetBlendingMode:TpvCanvasBlendingMode; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetBlendingMode(const aBlendingMode:TpvCanvasBlendingMode);
       function GetLineWidth:TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetLineWidth(const aLineWidth:TpvFloat); {$ifdef CAN_INLINE}inline;{$endif}
       function GetMiterLimit:TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetMiterLimit(const aMiterLimit:TpvFloat); {$ifdef CAN_INLINE}inline;{$endif}
       function GetLineJoin:TpvCanvasLineJoin; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetLineJoin(const aLineJoin:TpvCanvasLineJoin); {$ifdef CAN_INLINE}inline;{$endif}
       function GetLineCap:TpvCanvasLineCap; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetLineCap(const aLineCap:TpvCanvasLineCap); {$ifdef CAN_INLINE}inline;{$endif}
       function GetFillRule:TpvCanvasFillRule; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetFillRule(const aFillRule:TpvCanvasFillRule); {$ifdef CAN_INLINE}inline;{$endif}
       function GetColor:TpvVector4; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetColor(const aColor:TpvVector4); {$ifdef CAN_INLINE}inline;{$endif}
       function GetProjectionMatrix:TpvMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetProjectionMatrix(const aProjectionMatrix:TpvMatrix4x4);
       function GetViewMatrix:TpvMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetViewMatrix(const aViewMatrix:TpvMatrix4x4);
       function GetModelMatrix:TpvMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetModelMatrix(const aModelMatrix:TpvMatrix3x3); {$ifdef CAN_INLINE}inline;{$endif}
       function GetFont:TpvFont; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetFont(const aFont:TpvFont); {$ifdef CAN_INLINE}inline;{$endif}
       function GetFontSize:TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetFontSize(const aFontSize:TpvFloat); {$ifdef CAN_INLINE}inline;{$endif}
       function GetTextHorizontalAlignment:TpvCanvasTextHorizontalAlignment; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetTextHorizontalAlignment(aTextHorizontalAlignment:TpvCanvasTextHorizontalAlignment); {$ifdef CAN_INLINE}inline;{$endif}
       function GetTextVerticalAlignment:TpvCanvasTextVerticalAlignment; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetTextVerticalAlignment(aTextVerticalAlignment:TpvCanvasTextVerticalAlignment); {$ifdef CAN_INLINE}inline;{$endif}
       procedure GetNextDestinationVertexBuffer;
       procedure FlushAndGetNewDestinationVertexBufferIfNeeded(const aCountVerticesToCheck,aCountIndicesToCheck:TpvInt32);
       function ClipCheck(const aX0,aY0,aX1,aY1:TpvFloat):boolean;
       function GetVertexState:TpvUInt32; {$ifdef CAN_INLINE}inline;{$endif}
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
      public
       procedure ExecuteUpload(const aVulkanCommandBuffer:TpvVulkanCommandBuffer;const aBufferIndex:TpvInt32);
       procedure ExecuteDraw(const aVulkanCommandBuffer:TpvVulkanCommandBuffer;const aBufferIndex:TpvInt32);
      public
       function Push:TpvCanvas;
       function Pop:TpvCanvas;
      public
       function Hook(const aHook:TpvCanvasHook;const aData:TpvPointer):TpvCanvas; overload;
      private
       function DrawSprite(const aSprite:TpvSprite;const aSrc,aDest:TpvRect;const aRenderingMode:TpvCanvasRenderingMode):TpvCanvas; overload;
      public
       function DrawFontGlyphSprite(const aSprite:TpvSprite;const aSrc,aDest:TpvRect):TpvCanvas;
      public
       function DrawSprite(const aSprite:TpvSprite;const aSrc,aDest:TpvRect):TpvCanvas; overload;
       function DrawSprite(const aSprite:TpvSprite;const aSrc,aDest:TpvRect;const aOrigin:TpvVector2;const aRotation:TpvFloat):TpvCanvas; overload;
       function DrawSprite(const aSprite:TpvSprite;const aPosition:TpvVector2):TpvCanvas; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function DrawSprite(const aSprite:TpvSprite):TpvCanvas; overload; {$ifdef CAN_INLINE}inline;{$endif}
      public
       function TextWidth(const aText:TpvUTF8String):TpvFloat;
       function TextHeight(const aText:TpvUTF8String):TpvFloat;
       function TextSize(const aText:TpvUTF8String):TpvVector2;
       function TextRowHeight(const aPercent:TpvFloat):TpvFloat;
       function DrawText(const aText:TpvUTF8String;const aPosition:TpvVector2):TpvCanvas; overload;
       function DrawText(const aText:TpvUTF8String;const aX,aY:TpvFloat):TpvCanvas; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function DrawText(const aText:TpvUTF8String):TpvCanvas; overload; {$ifdef CAN_INLINE}inline;{$endif}
      public
       function DrawShape(const aShape:TpvCanvasShape):TpvCanvas; overload; {$ifdef CAN_INLINE}inline;{$endif}
      public
       function BeginPath:TpvCanvas; {$ifdef CAN_INLINE}inline;{$endif}
       function EndPath:TpvCanvas; {$ifdef CAN_INLINE}inline;{$endif}
       function ClosePath:TpvCanvas; {$ifdef CAN_INLINE}inline;{$endif}
      public
       function MoveTo(const aP0:TpvVector2):TpvCanvas; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function MoveTo(const aX,aY:TpvFloat):TpvCanvas; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function LineTo(const aP0:TpvVector2):TpvCanvas; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function LineTo(const aX,aY:TpvFloat):TpvCanvas; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function QuadraticCurveTo(const aC0,aA0:TpvVector2):TpvCanvas; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function QuadraticCurveTo(const aCX,aCY,aAX,aAY:TpvFloat):TpvCanvas; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function CubicCurveTo(const aC0,aC1,aA0:TpvVector2):TpvCanvas; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function CubicCurveTo(const aC0X,aC0Y,aC1X,aC1Y,aAX,aAY:TpvFloat):TpvCanvas; overload; {$ifdef CAN_INLINE}inline;{$endif}
      public
       function Stroke:TpvCanvas;
       function Fill:TpvCanvas;
      public
       property Viewport:PVkViewport read fPointerToViewport;
       property Color:TpvVector4 read GetColor write SetColor;
       property ProjectionMatrix:TpvMatrix4x4 read GetProjectionMatrix write SetProjectionMatrix;
       property ViewMatrix:TpvMatrix4x4 read GetViewMatrix write SetViewMatrix;
       property ModelMatrix:TpvMatrix3x3 read GetModelMatrix write SetModelMatrix;
       property Font:TpvFont read GetFont write SetFont;
       property FontSize:TpvFloat read GetFontSize write SetFontSize;
       property TextHorizontalAlignment:TpvCanvasTextHorizontalAlignment read GetTextHorizontalAlignment write SetTextHorizontalAlignment;
       property TextVerticalAlignment:TpvCanvasTextVerticalAlignment read GetTextVerticalAlignment write SetTextVerticalAlignment;
      published
       property Device:TpvVulkanDevice read fDevice;
       property Width:TpvInt32 read fWidth write fWidth;
       property Height:TpvInt32 read fHeight write fHeight;
       property BlendingMode:TpvCanvasBlendingMode read GetBlendingMode write SetBlendingMode;
       property LineWidth:TpvFloat read GetLineWidth write SetLineWidth;
       property MiterLimit:TpvFloat read GetMiterLimit write SetMiterLimit;
       property LineJoin:TpvCanvasLineJoin read GetLineJoin write SetLineJoin;
       property LineCap:TpvCanvasLineCap read GetLineCap write SetLineCap;
       property FillRule:TpvCanvasFillRule read GetFillRule write SetFillRule;
       property State:TpvCanvasState read fState;
     end;

const pvCanvasRenderingModeValues:TpvCanvasRenderingModeValues=
       (
        0, // pvcrmNormal
        1  // pvcrmFont
       );

      pvCanvasBlendingModeValues:TpvCanvasBlendingModeValues=
       (
         0, // pvcbmNone
         1, // pvcbmAlphaBlending
         2  // pvcbmAdditiveBlending
        );

implementation

uses PasVulkan.Assets,
     PasVulkan.Streams;

const pcvvaomLineEdge=1;
      pcvvaomRoundLineCapCircle=2;

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

procedure TpvCanvasPath.Assign(aSource:TPersistent);
begin
 if assigned(aSource) and (aSource is TpvCanvasPath) then begin
  fCountCommands:=TpvCanvasPath(aSource).fCountCommands;
  if length(fCommands)<fCountCommands then begin
   SetLength(fCommands,fCountCommands*2);
  end;
  if fCountCommands>0 then begin
   Move(TpvCanvasPath(aSource).fCommands[0],fCommands[0],fCountCommands*SizeOf(TpvCanvasPathCommand));
  end;
 end;
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

function TpvCanvasPath.BeginPath:TpvCanvasPath;
begin
 fCountCommands:=0;
 result:=self;
end;

function TpvCanvasPath.EndPath:TpvCanvasPath;
begin
 fCountCommands:=0;
 result:=self;
end;

function TpvCanvasPath.ClosePath:TpvCanvasPath;
var Command:PpvCanvasPathCommand;
begin
 Command:=NewCommand;
 Command^.CommandType:=pcpctClose;
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

function TpvCanvasPath.LineTo(const aP0:TpvVector2):TpvCanvasPath;
var Command:PpvCanvasPathCommand;
begin
 Command:=NewCommand;
 Command^.CommandType:=pcpctLineTo;
 Command^.Points[0]:=aP0;
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

constructor TpvCanvasState.Create;
begin
 inherited Create;
 fClipRect:=TpvRect.Create(-MaxSingle,-MaxSingle,MaxSingle,MaxSingle);
 fScissor:=TVkRect2D.Create(TVkOffset2D.Create(0,0),TVkExtent2D.Create($7fffffff,$7fffffff));
 fProjectionMatrix:=TpvMatrix4x4.Identity;
 fPath:=TpvCanvasPath.Create;
 Reset;
end;

destructor TpvCanvasState.Destroy;
begin
 FreeAndNil(fPath);
 inherited Destroy;
end;

procedure TpvCanvasState.Reset;
begin
 fBlendingMode:=pvcbmAlphaBlending;
 fLineWidth:=1.0;
 fMiterLimit:=3.0;
 fLineJoin:=TpvCanvasLineJoin.pvcljRound;
 fLineCap:=TpvCanvasLineCap.pvclcRound;
 fFillRule:=TpvCanvasFillRule.pvcfrNonZero;
 fColor:=TpvVector4.Create(1.0,1.0,1.0,1.0);
 fFont:=nil;
 fFontSize:=-12;
 fTextHorizontalAlignment:=TpvCanvasTextHorizontalAlignment.pvcthaLeft;
 fTextVerticalAlignment:=TpvCanvasTextVerticalAlignment.pvctvaTop;
 fViewMatrix:=TpvMatrix4x4.Identity;
 fModelMatrix:=TpvMatrix3x3.Identity;
 fPath.fCountCommands:=0;
end;

procedure TpvCanvasState.Assign(aSource:TPersistent);
begin
 if assigned(aSource) and (aSource is TpvCanvasState) then begin
  fBlendingMode:=TpvCanvasState(aSource).fBlendingMode;
  fLineWidth:=TpvCanvasState(aSource).fLineWidth;
  fMiterLimit:=TpvCanvasState(aSource).fMiterLimit;
  fLineJoin:=TpvCanvasState(aSource).fLineJoin;
  fLineCap:=TpvCanvasState(aSource).fLineCap;
  fFillRule:=TpvCanvasState(aSource).fFillRule;
  fColor:=TpvCanvasState(aSource).fColor;
  fClipRect:=TpvCanvasState(aSource).fClipRect;
  fScissor:=TpvCanvasState(aSource).fScissor;
  fProjectionMatrix:=TpvCanvasState(aSource).fProjectionMatrix;
  fViewMatrix:=TpvCanvasState(aSource).fViewMatrix;
  fModelMatrix:=TpvCanvasState(aSource).fModelMatrix;
  fFont:=TpvCanvasState(aSource).fFont;
  fFontSize:=TpvCanvasState(aSource).fFontSize;
  fTextHorizontalAlignment:=TpvCanvasState(aSource).fTextHorizontalAlignment;
  fTextVerticalAlignment:=TpvCanvasState(aSource).fTextVerticalAlignment;
  fPath.Assign(TpvCanvasState(aSource).fPath);
 end;
end;

constructor TpvCanvasShape.Create;
begin
 inherited Create;
 fCacheGroups:=nil;
 fCountCacheGroups:=0;
 fCacheLinePoints:=nil;
 fCountCacheLinePoints:=0;
end;

destructor TpvCanvasShape.Destroy;
begin
 fCacheGroups:=nil;
 fCacheLinePoints:=nil;
 inherited Destroy;
end;

function TpvCanvasShape.NewCacheGroup(const aStartAllocationCountVertices:TpvInt32=0;const aStartAllocationCountIndices:TpvInt32=0):PpvCanvasShapeCacheGroup;
begin
 inc(fCountCacheGroups);
 if length(fCacheGroups)<fCountCacheGroups then begin
  SetLength(fCacheGroups,fCountCacheGroups*2);
 end;
 result:=@fCacheGroups[fCountCacheGroups-1];
 if Length(result^.Vertices)<aStartAllocationCountVertices then begin
  SetLength(result^.Vertices,aStartAllocationCountVertices);
 end;
 if length(result^.Indices)<aStartAllocationCountIndices then begin
  SetLength(result^.Indices,aStartAllocationCountIndices);
 end;
 result^.CountVertices:=0;
 result^.CountIndices:=0;
end;

procedure TpvCanvasShape.StrokeFromPath(const aPath:TpvCanvasPath;const aState:TpvCanvasState);
var StartPoint,LastPoint:TpvVector2;
    CacheGroup:PpvCanvasShapeCacheGroup;
 procedure StrokeAddPoint(const aP0:TpvVector2);
 var Index:TpvInt32;
     ShapeCacheLinePoint:PpvCanvasShapeCacheLinePoint;
 begin
  if (fCountCacheLinePoints=0) or
     (fCacheLinePoints[fCountCacheLinePoints-1].Position<>aP0) then begin
   Index:=fCountCacheLinePoints;
   inc(fCountCacheLinePoints);
   if length(fCacheLinePoints)<fCountCacheLinePoints then begin
    SetLength(fCacheLinePoints,fCountCacheLinePoints*2);
   end;
   ShapeCacheLinePoint:=@fCacheLinePoints[Index];
   ShapeCacheLinePoint^.Position:=aP0;
  end;
 end;
 procedure StrokeFlush;
 var VertexColor:TpvHalfFloatVector4;
     VertexState:TpvUInt32;
     Closed:boolean;
     Width:TpvFloat;
     v0,v1,v2,v3:TpvVector2;
     First:boolean;
  function AddVertex(const Position:TpvVector2;const ObjectMode:TpvUInt8;const MetaInfo:TpvVector4):TpvInt32;
  var v:PpvCanvasShapeCacheVertex;
  begin
   result:=CacheGroup^.CountVertices;
   inc(CacheGroup^.CountVertices);
   if length(CacheGroup^.Vertices)<CacheGroup^.CountVertices then begin
    SetLength(CacheGroup^.Vertices,CacheGroup^.CountVertices*2);
   end;
   v:=@CacheGroup^.Vertices[result];
   v^.Position:=Position;
   v^.ObjectMode:=ObjectMode;
   v^.MetaInfo:=MetaInfo;
  end;
  function AddIndex(const VertexIndex:TpvInt32):TpvInt32;
  begin
   result:=CacheGroup^.CountIndices;
   inc(CacheGroup^.CountIndices);
   if length(CacheGroup^.Indices)<CacheGroup^.CountIndices then begin
    SetLength(CacheGroup^.Indices,CacheGroup^.CountIndices*2);
   end;
   CacheGroup^.Indices[result]:=VertexIndex;
  end;
{$define FasterRoundCap}
{$ifdef FasterRoundCap}
  procedure CreateRoundCap(const Center,p0,p1,NextPointInLine:TpvVector2); // On the CPU faster but with more overdraw on the GPU
  const Offsets:array[0..2] of TpvVector2=
         ((x:1.414213562373095;y:0.0),
          (x:-1.414213562373095;y:-2.82842712474619),
          (x:-1.414213562373095;y:2.82842712474619));
{ const Offsets:array[0..2] of TpvVector2=
         ((x:-1.0;y:-1.0),
          (x:-1.0;y:3.0),
          (x:3.0;y:-1.0)); }
  var Radius:TpvFloat;
      MetaInfo:TpvVector4;
      Normal:TpvVector2;
      Matrix:TpvMatrix2x2;
  begin
   Radius:=p0.DistanceTo(Center);
   MetaInfo:=TpvVector4.Create(Center.x,Center.y,Radius,-1.0);
   CacheGroup:=NewCacheGroup(3,3);
   Normal:=(Center-NextPointInLine).Normalize;
   Matrix:=TpvMatrix2x2.Create(Normal,TpvVector2.Create(-Normal.y,Normal.x));
   AddIndex(AddVertex(Center+((Matrix*Offsets[0])*Radius),pcvvaomRoundLineCapCircle,MetaInfo));
   AddIndex(AddVertex(Center+((Matrix*Offsets[1])*Radius),pcvvaomRoundLineCapCircle,MetaInfo));
   AddIndex(AddVertex(Center+((Matrix*Offsets[2])*Radius),pcvvaomRoundLineCapCircle,MetaInfo));
 end;
{$else}
  procedure CreateRoundCap(const Center,p0,p1,NextPointInLine:TpvVector2); // On the CPU slower but with less overdraw on the GPU
  var Radius,Angle0,Angle1,AngleDifference,AngleIncrement,Phase:TpvFloat;
      Segments,iCenter,iLast,iCurrent,i:TpvInt32;
      MetaInfo:TpvVector4;
  begin
   Radius:=Center.DistanceTo(p0);
   Angle0:=AngleClamp(ArcTan2(p1.y-Center.y,p1.x-Center.x));
   Angle1:=AngleClamp(ArcTan2(p0.y-Center.y,p0.x-Center.x));
   AngleDifference:=AngleDiff(Angle0,Angle1);{}
{  Angle0:=ModuloPos(ModuloPos(ArcTan2(p1.y-Center.y,p1.x-Center.x)+pi,pi2)+pi2,pi2)-pi;
   Angle1:=ModuloPos(ModuloPos(ArcTan2(p0.y-Center.y,p0.x-Center.x)+pi,pi2)+pi2,pi2)-pi;
   AngleDifference:=ModuloPos((Angle1-Angle0)+pi,pi2)-pi;{}
{  if (abs(pi-abs(AngleDifference))<1e-4) and
      (Vector2Dot(Vector2Norm(Vector2Sub(Center,NextPointInLine)),Vector2AngleDirection(Angle0+(AngleDifference*0.5)))<0.0) then begin
    AngleDifference:=-AngleDifference;
   end;}
   if (abs(pi-abs(AngleDifference))<1e-4) then begin
    Angle0:=ArcTan2(NextPointInLine.x-Center.x,Center.y-NextPointInLine.y);
    AngleDifference:=pi;
   end;
   Segments:=Max(4,trunc(ceil((abs(AngleDifference)*Radius)/8.0)));
   AngleIncrement:=AngleDifference/Segments;
   MetaInfo:=TpvVector4.Create(Center.x,Center.y,Radius,-1.0);
   Radius:=ceil(Radius);
   if Segments<1024 then begin
    CacheGroup:=NewCacheGroup(2+Segments,Segments*3);
    iCenter:=AddVertex(Center,pcvvaomRoundLineCapCircle,MetaInfo);
    Phase:=Angle0;
    iCurrent:=AddVertex(Center+(TpvVector2.Create(cos(Phase),sin(Phase))*Radius),pcvvaomRoundLineCapCircle,MetaInfo);
    for i:=0 to Segments-1 do begin
     iLast:=iCurrent;
     Phase:=Phase+AngleIncrement;
     iCurrent:=AddVertex(Center+(TpvVector2.Create(cos(Phase),sin(Phase))*Radius),pcvvaomRoundLineCapCircle,MetaInfo);
     AddIndex(iCenter);
     AddIndex(iLast);
     AddIndex(iCurrent);
    end;
   end else begin
    Phase:=Angle0;
    for i:=0 to Segments-1 do begin
     CacheGroup:=NewCacheGroup(3,3);
     iCenter:=AddVertex(Center,pcvvaomRoundLineCapCircle,MetaInfo);
     iLast:=AddVertex(Center+(TpvVector2.Create(cos(Phase),sin(Phase))*Radius),pcvvaomRoundLineCapCircle,MetaInfo);
     Phase:=Phase+AngleIncrement;
     iCurrent:=AddVertex(Center+(TpvVector2.Create(cos(Phase),sin(Phase))*Radius),pcvvaomRoundLineCapCircle,MetaInfo);
     AddIndex(iCenter);
     AddIndex(iLast);
     AddIndex(iCurrent);
    end;
   end;
  end;
{$endif}
  function LineIntersection(out p:TpvVector2;const v0,v1,v2,v3:TpvVector2):boolean;
  const EPSILON=1e-8;
  var a0,a1,b0,b1,c0,c1,Determinant:TpvFloat;
  begin
   a0:=v1.y-v0.y;
   b0:=v0.x-v1.x;
   a1:=v3.y-v2.y;
   b1:=v2.x-v3.x;
   Determinant:=(a0*b1)-(a1*b0);
   result:=abs(Determinant)>EPSILON;
   if result then begin
    c0:=(a0*v0.x)+(b0*v0.y);
    c1:=(a1*v2.x)+(b1*v2.y);
    p.x:=((b1*c0)-(b0*c1))/Determinant;
    p.y:=((a0*c1)-(a1*c0))/Determinant;
   end;
  end;
  function SignedArea(const v1,v2,v3:TpvVector2):TpvFloat;
  begin
   result:=((v2.x-v1.x)*(v3.y-v1.y))-((v3.x-v1.x)*(v2.y-v1.y));
  end;
  procedure CreateSquareCap(const p0,p1,d:TpvVector2);
  var ip0,ip0d,ip1d,ip1:TpvInt32;
  begin
   CacheGroup:=NewCacheGroup(4,6);
   ip0:=AddVertex(p0,pcvvaomLineEdge,TpvVector4.Create(-Width,0.0,Width,Width));
   ip0d:=AddVertex(p0+d,pcvvaomLineEdge,TpvVector4.Create(-Width,Width,Width,Width));
   ip1d:=AddVertex(p1+d,pcvvaomLineEdge,TpvVector4.Create(Width,Width,Width,Width));
   ip1:=AddVertex(p1,pcvvaomLineEdge,TpvVector4.Create(Width,0.0,Width,Width));
   AddIndex(ip0);
   AddIndex(ip0d);
   AddIndex(ip1d);
   AddIndex(ip1);
   AddIndex(ip1d);
   AddIndex(ip0);
  end;
  procedure CreateTriangles(const p0,p1,p2:TpvVector2;const LineJoin:TpvCanvasLineJoin;MiterLimit:TpvFloat;const IsFirst,IsLast:boolean);
  var CountVerticesToAdd,CountIndicesToAdd,LineJoinCase,
      ip0at0,ip0st0,ip1sAnchor,ip1at0,ip1st0,ip2at2,ip1st2,ip1at2,ip2st2,ip1,iIntersectionPoint,iCenter:TpvInt32;
      t0,t2,IntersectionPoint,Anchor,p0p1,p1p2:TpvVector2;
      AnchorLength,dd,p0p1Length,p1p2Length,l0,l2,s0,s2:TpvFloat;
      DoIntersect:boolean;
  begin
   t0:=(p1-p0).Perpendicular.Normalize*Width;
   t2:=(p2-p1).Perpendicular.Normalize*Width;
   if SignedArea(p0,p1,p2)>0.0 then begin
    t0:=-t0;
    t2:=-t2;
   end;
   DoIntersect:=LineIntersection(IntersectionPoint,p0+t0,p1+t0,p2+t2,p1+t2);
   if DoIntersect and not (IsFirst and IsLast) then begin
    Anchor:=IntersectionPoint-p1;
    AnchorLength:=Anchor.Length;
    dd:=AnchorLength/Width;
   end else begin
    Anchor:=Vector2Origin;
    AnchorLength:=3.4e+28;
    dd:=0.0;
   end;
   p0p1:=p0-p1;
   p1p2:=p1-p2;
   p0p1Length:=p0p1.Length;
   p1p2Length:=p1p2.Length;
   if First then begin
    v0:=p0+t0;
    v1:=p0-t0;
   end;
   v2:=p2-t2;
   v3:=p2+t2;
   if Closed or (aState.fLineCap<>pvclcButt) then begin
    l0:=0.0;
    l2:=0.0;
    s0:=Width;
    s2:=Width;
   end else begin
    if IsFirst then begin
     l0:=p0.DistanceTo(p1);
     s0:=l0;
    end else begin
     l0:=0.0;
     s0:=Width;
    end;
    if IsLast then begin
     l2:=p1.DistanceTo(p2);
     s2:=l2;
    end else begin
     l2:=0.0;
     s2:=Width;
    end;
   end;
   if (AnchorLength>p0p1Length) or (AnchorLength>p1p2Length) then begin
    // The cross point exceeds any of the segments dimension.
    // Do not use cross point as reference.
    // This case deserves more attention to avoid redraw, currently works by overdrawing large parts.
    CountVerticesToAdd:=8;
    CountIndicesToAdd:=12;
    if LineJoin=pvcljRound then begin
     LineJoinCase:=0;
    end else if (LineJoin=pvcljBevel) or ((LineJoin=pvcljMiter) and (dd>=MiterLimit)) then begin
     LineJoinCase:=1;
     inc(CountVerticesToAdd,3);
     inc(CountIndicesToAdd,3);
    end else if (LineJoin=pvcljMiter) and (dd<MiterLimit) and DoIntersect then begin
     LineJoinCase:=2;
     inc(CountVerticesToAdd,4);
     inc(CountIndicesToAdd,6);
    end else begin
     LineJoinCase:=3;
    end;
    CacheGroup:=NewCacheGroup(CountVerticesToAdd,CountIndicesToAdd);
    ip0at0:=AddVertex(p0+t0,pcvvaomLineEdge,TpvVector4.Create(Width,l0,Width,s0));
    ip0st0:=AddVertex(p0-t0,pcvvaomLineEdge,TpvVector4.Create(-Width,l0,Width,s0));
    ip1at0:=AddVertex(p1+t0,pcvvaomLineEdge,TpvVector4.Create(Width,0.0,Width,s0));
    ip1st0:=AddVertex(p1-t0,pcvvaomLineEdge,TpvVector4.Create(-Width,0.0,Width,s0));
    AddIndex(ip0at0);
    AddIndex(ip0st0);
    AddIndex(ip1at0);
    AddIndex(ip0st0);
    AddIndex(ip1at0);
    AddIndex(ip1st0);
    ip1at2:=AddVertex(p1+t2,pcvvaomLineEdge,TpvVector4.Create(Width,0.0,Width,s2));
    ip2at2:=AddVertex(p2+t2,pcvvaomLineEdge,TpvVector4.Create(Width,l2,Width,s2));
    ip1st2:=AddVertex(p1-t2,pcvvaomLineEdge,TpvVector4.Create(-Width,0.0,Width,s2));
    ip2st2:=AddVertex(p2-t2,pcvvaomLineEdge,TpvVector4.Create(-Width,l2,Width,s2));
    AddIndex(ip2at2);
    AddIndex(ip1st2);
    AddIndex(ip1at2);
    AddIndex(ip2at2);
    AddIndex(ip1st2);
    AddIndex(ip2st2);
    case LineJoinCase of
     0:begin
      // Round join
      CreateRoundCap(p1,p1+t0,p1+t2,p2);
     end;
     1:begin
      // Bevel join
      ip1:=AddVertex(p1,pcvvaomLineEdge,TpvVector4.Create(0.0,0.0,Width,Width));
      ip1at0:=AddVertex(p1+t0,pcvvaomLineEdge,TpvVector4.Create(Width,Width,Width,Width));
      ip1at2:=AddVertex(p1+t2,pcvvaomLineEdge,TpvVector4.Create(Width,Width,Width,Width));
      AddIndex(ip1);
      AddIndex(ip1at0);
      AddIndex(ip1at2);
     end;
     2:begin
      // Miter join
      ip1:=AddVertex(p1,pcvvaomLineEdge,TpvVector4.Create(0.0,0.0,Width,Width));
      iIntersectionPoint:=AddVertex(IntersectionPoint,pcvvaomLineEdge,TpvVector4.Create(Width,0.0,Width,Width));
      ip1at0:=AddVertex(p1+t0,pcvvaomLineEdge,TpvVector4.Create(Width,0.0,Width,Width));
      ip1at2:=AddVertex(p1+t2,pcvvaomLineEdge,TpvVector4.Create(Width,0.0,Width,Width));
      AddIndex(ip1at0);
      AddIndex(ip1);
      AddIndex(iIntersectionPoint);
      AddIndex(ip1at2);
      AddIndex(ip1);
      AddIndex(iIntersectionPoint);
     end;
     else begin
      // Nothing
     end;
    end;
   end else begin
    CountVerticesToAdd:=8;
    CountIndicesToAdd:=12;
    if LineJoin=pvcljRound then begin
     LineJoinCase:=0;
     inc(CountVerticesToAdd,4);
     inc(CountIndicesToAdd,6);
    end else if (LineJoin=pvcljBevel) or ((LineJoin=pvcljMiter) and (dd>=MiterLimit)) then begin
     LineJoinCase:=1;
     inc(CountVerticesToAdd,3);
     inc(CountIndicesToAdd,3);
    end else if (LineJoin=pvcljMiter) and (dd<MiterLimit) and DoIntersect then begin
     LineJoinCase:=2;
     inc(CountVerticesToAdd,4);
     inc(CountIndicesToAdd,6);
    end else begin
     LineJoinCase:=3;
    end;
    CacheGroup:=NewCacheGroup(CountVerticesToAdd,CountIndicesToAdd);
    ip0at0:=AddVertex(p0+t0,pcvvaomLineEdge,TpvVector4.Create(Width,l0,Width,s0));
    ip0st0:=AddVertex(p0-t0,pcvvaomLineEdge,TpvVector4.Create(-Width,l0,Width,s0));
    ip1sAnchor:=AddVertex(p1-Anchor,pcvvaomLineEdge,TpvVector4.Create(-Width,0.0,Width,s0));
    ip1at0:=AddVertex(p1+t0,pcvvaomLineEdge,TpvVector4.Create(Width,0.0,Width,s0));
    AddIndex(ip0at0);
    AddIndex(ip0st0);
    AddIndex(ip1sAnchor);
    AddIndex(ip0at0);
    AddIndex(ip1sAnchor);
    AddIndex(ip1at0);
    ip1sAnchor:=AddVertex(p1-Anchor,pcvvaomLineEdge,TpvVector4.Create(-Width,0.0,Width,s2));
    ip2at2:=AddVertex(p2+t2,pcvvaomLineEdge,TpvVector4.Create(Width,l2,Width,s2));
    ip1at2:=AddVertex(p1+t2,pcvvaomLineEdge,TpvVector4.Create(Width,0.0,Width,s2));
    ip2st2:=AddVertex(p2-t2,pcvvaomLineEdge,TpvVector4.Create(-Width,l2,Width,s2));
    AddIndex(ip2at2);
    AddIndex(ip1sAnchor);
    AddIndex(ip1at2);
    AddIndex(ip2at2);
    AddIndex(ip1sAnchor);
    AddIndex(ip2st2);
    case LineJoinCase of
     0:begin
      // Round join
      ip1at0:=AddVertex(p1+t0,pcvvaomLineEdge,TpvVector4.Create(Width,0.0,Width,Width));
      ip1:=AddVertex(p1,pcvvaomLineEdge,TpvVector4.Create(0.0,0.0,Width,Width));
      ip1sAnchor:=AddVertex(p1-Anchor,pcvvaomLineEdge,TpvVector4.Create(-Width,0.0,Width,Width));
      ip1at2:=AddVertex(p1+t2,pcvvaomLineEdge,TpvVector4.Create(Width,0.0,Width,Width));
      AddIndex(ip1at0);
      AddIndex(ip1);
      AddIndex(ip1sAnchor);
      AddIndex(ip1);
      AddIndex(ip1at2);
      AddIndex(ip1sAnchor);
      CreateRoundCap(p1,p1+t0,p1+t2,p1-Anchor);
     end;
     1:begin
      // Bevel join
      ip1at0:=AddVertex(p1+t0,pcvvaomLineEdge,TpvVector4.Create(Width,Width,Width,Width));
      ip1at2:=AddVertex(p1+t2,pcvvaomLineEdge,TpvVector4.Create(Width,Width,Width,Width));
      ip1sAnchor:=AddVertex(p1-Anchor,pcvvaomLineEdge,TpvVector4.Create(-Width,0.0,Width,Width));
      AddIndex(ip1at0);
      AddIndex(ip1at2);
      AddIndex(ip1sAnchor);
     end;
     2:begin
      // Miter join
      ip1at0:=AddVertex(p1+t0,pcvvaomLineEdge,TpvVector4.Create(Width,0.0,Width,Width));
      ip1at2:=AddVertex(p1+t2,pcvvaomLineEdge,TpvVector4.Create(Width,0.0,Width,Width));
      iCenter:=AddVertex(p1-Anchor,pcvvaomLineEdge,TpvVector4.Create(-Width,0.0,Width,Width));
      iIntersectionPoint:=AddVertex(IntersectionPoint,pcvvaomLineEdge,TpvVector4.Create(Width,0.0,Width,Width));
      AddIndex(ip1at0);
      AddIndex(iCenter);
      AddIndex(iIntersectionPoint);
      AddIndex(iCenter);
      AddIndex(ip1at2);
      AddIndex(iIntersectionPoint);
     end;
    end;
   end;
   First:=false;
  end;
 var i:TpvInt32;
 begin
  if fCountCacheLinePoints>2 then begin
   for i:=fCountCacheLinePoints-2 downto 0 do begin
    if fCacheLinePoints[i].Position=fCacheLinePoints[i+1].Position then begin
     dec(fCountCacheLinePoints);
     Move(fCacheLinePoints[i-1],fCacheLinePoints[i],fCountCacheLinePoints*SizeOf(TpvCanvasShapeCacheLinePoint));
    end;
   end;
   if fCountCacheLinePoints>2 then begin
    Width:=abs(aState.fLineWidth)*0.5;
    First:=true;
    if fCountCacheLinePoints=2 then begin
     Closed:=false;
     CreateTriangles(fCacheLinePoints[0].Position,
                     fCacheLinePoints[0].Position.Lerp(fCacheLinePoints[1].Position,0.5),
                     fCacheLinePoints[1].Position,
                     pvcljBevel,
                     aState.fMiterLimit,
                     true,
                     true);
    end else if fCountCacheLinePoints>2 then begin
     Closed:=fCacheLinePoints[0].Position.DistanceTo(fCacheLinePoints[fCountCacheLinePoints-1].Position)<EPSILON;
     if Closed then begin
      fCacheLinePoints[0].Position:=(fCacheLinePoints[0].Position+fCacheLinePoints[1].Position)*0.5;
      inc(fCountCacheLinePoints);
      if fCountCacheLinePoints>length(fCacheLinePoints) then begin
       SetLength(fCacheLinePoints,fCountCacheLinePoints*2);
      end;
      fCacheLinePoints[fCountCacheLinePoints-1]:=fCacheLinePoints[0];
     end;
     fCacheLinePoints[0].Middle:=fCacheLinePoints[0].Position;
     for i:=1 to fCountCacheLinePoints-3 do begin
      fCacheLinePoints[i].Middle:=(fCacheLinePoints[i].Position+fCacheLinePoints[i+1].Position)*0.5;
     end;
     fCacheLinePoints[fCountCacheLinePoints-2].Middle:=fCacheLinePoints[fCountCacheLinePoints-1].Position;
     for i:=1 to fCountCacheLinePoints-2 do begin
      CreateTriangles(fCacheLinePoints[i-1].Middle,
                      fCacheLinePoints[i].Position,
                      fCacheLinePoints[i].Middle,
                      aState.fLineJoin,
                      aState.fMiterLimit,
                      i=1,
                      i=(fCountCacheLinePoints-2));
     end;
    end;
    if not Closed then begin
     case aState.fLineCap of
      pvclcRound:begin
       CreateRoundCap(fCacheLinePoints[0].Position,v0,v1,fCacheLinePoints[1].Position);
       CreateRoundCap(fCacheLinePoints[fCountCacheLinePoints-1].Position,v2,v3,fCacheLinePoints[fCountCacheLinePoints-2].Position);
      end;
      pvclcSquare:begin
       CreateSquareCap(v0,
                       v1,
                       (fCacheLinePoints[0].Position-fCacheLinePoints[1].Position).Normalize*fCacheLinePoints[0].Position.DistanceTo(v0));
       CreateSquareCap(v2,
                       v3,
                       (fCacheLinePoints[fCountCacheLinePoints-1].Position-fCacheLinePoints[fCountCacheLinePoints-2].Position).Normalize*fCacheLinePoints[fCountCacheLinePoints-1].Position.DistanceTo(v3));
      end;
     end;
    end;
   end;
  end;
  fCountCacheLinePoints:=0;
 end;
 procedure StrokeMoveTo(const aP0:TpvVector2);
 begin
  StrokeFlush;
  StrokeAddPoint(aP0);
  StartPoint:=aP0;
  LastPoint:=aP0;
 end;
 procedure StrokeLineTo(const aP0:TpvVector2);
 begin
  StrokeAddPoint(aP0);
  LastPoint:=aP0;
 end;
 procedure StrokeQuadraticCurveTo(const aC0,aA0:TpvVector2;const Tolerance:TpvDouble=1.0/256.0;const MaxLevel:TpvInt32=32);
  procedure Recursive(const x1,y1,x2,y2,x3,y3:TpvFloat;const Level:TpvInt32);
  var x12,y12,x23,y23,x123,y123,mx,my,d:TpvFloat;
      Point:TpvVector2;
  begin
   x12:=(x1+x2)*0.5;
   y12:=(y1+y2)*0.5;
   x23:=(x2+x3)*0.5;
   y23:=(y2+y3)*0.5;
   x123:=(x12+x23)*0.5;
   y123:=(y12+y23)*0.5;
   mx:=(x1+x3)*0.5;
   my:=(y1+y3)*0.5;
   d:=abs(mx-x123)+abs(my-y123);
   if (Level>MaxLevel) or (d<Tolerance) then begin
    Point.x:=x123;
    Point.y:=y123;
    StrokeAddPoint(Point);
   end else begin
    Recursive(x1,y1,x12,y12,x123,y123,level+1);
    Recursive(x123,y123,x23,y23,x3,y3,level+1);
   end;
  end;
 begin
  Recursive(LastPoint.x,LastPoint.y,aC0.x,aC0.y,aA0.x,aA0.y,0);
  StrokeAddPoint(aA0);
 end;
 procedure StrokeCubicCurveTo(const aC0,aC1,aA0:TpvVector2;const Tolerance:TpvDouble=1.0/256.0;const MaxLevel:TpvInt32=32);
  procedure Recursive(const x1,y1,x2,y2,x3,y3,x4,y4:TpvDouble;const Level:TpvInt32);
  var x12,y12,x23,y23,x34,y34,x123,y123,x234,y234,x1234,y1234,mx,my,d:TpvDouble;
      Point:TpvVector2;
  begin
   x12:=(x1+x2)*0.5;
   y12:=(y1+y2)*0.5;
   x23:=(x2+x3)*0.5;
   y23:=(y2+y3)*0.5;
   x34:=(x3+x4)*0.5;
   y34:=(y3+y4)*0.5;
   x123:=(x12+x23)*0.5;
   y123:=(y12+y23)*0.5;
   x234:=(x23+x34)*0.5;
   y234:=(y23+y34)*0.5;
   x1234:=(x123+x234)*0.5;
   y1234:=(y123+y234)*0.5;
   mx:=(x1+x4)*0.5;
   my:=(y1+y4)*0.5;
   d:=abs(mx-x1234)+abs(my-y1234);
   if (Level>MaxLevel) or (d<Tolerance) then begin
    Point.x:=x1234;
    Point.y:=y1234;
    StrokeAddPoint(Point);
   end else begin
    Recursive(x1,y1,x12,y12,x123,y123,x1234,y1234,Level+1);
    Recursive(x1234,y1234,x234,y234,x34,y34,x4,y4,Level+1);
   end;
  end;
 begin
  Recursive(LastPoint.x,LastPoint.y,aC0.x,aC0.y,aC1.x,aC1.y,aA0.x,aA0.y,0);
  StrokeAddPoint(aA0);
 end;
 procedure StrokeClose;
 begin
  if fCountCacheLinePoints>0 then begin
   StrokeAddPoint(StartPoint);
   StrokeFlush;
  end;
 end;
var CommandIndex:TpvInt32;
    Command:PpvCanvasPathCommand;
begin
 CacheGroup:=nil;
 fCountCacheGroups:=0;
 fCountCacheLinePoints:=0;
 for CommandIndex:=0 to aPath.fCountCommands-1 do begin
  Command:=@aPath.fCommands[CommandIndex];
  case Command^.CommandType of
   pcpctMoveTo:begin
    StrokeMoveTo(Command.Points[0]);
   end;
   pcpctLineTo:begin
    StrokeLineTo(Command.Points[0]);
   end;
   pcpctQuadraticCurveTo:begin
    StrokeQuadraticCurveTo(Command.Points[0],Command.Points[1]);
   end;
   pcpctCubicCurveTo:begin
    StrokeCubicCurveTo(Command.Points[0],Command.Points[1],Command.Points[2]);
   end;
   pcpctClose:begin
    StrokeClose;
   end;
  end;
 end;
 StrokeFlush;
end;

procedure TpvCanvasShape.FillFromPath(const aPath:TpvCanvasPath;const aState:TpvCanvasState);
begin
 fCountCacheGroups:=0;
 fCountCacheLinePoints:=0;
end;

constructor TpvCanvasCommon.Create(const aDevice:TpvVulkanDevice);
var Stream:TStream;
begin
 inherited Create;

 fDevice:=aDevice;

 fDevice.CanvasCommon:=self;

 fReferenceCounter:=0;

 Stream:=TpvDataStream.Create(@CanvasVertexSPIRVData,CanvasVertexSPIRVDataSize);
 try
  fCanvasVertexShaderModule:=TpvVulkanShaderModule.Create(fDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=TpvDataStream.Create(@CanvasFragmentAtlasTextureSPIRVData,CanvasFragmentAtlasTextureSPIRVDataSize);
 try
  fCanvasFragmentAtlasTextureShaderModule:=TpvVulkanShaderModule.Create(fDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=TpvDataStream.Create(@CanvasFragmentNoTextureSPIRVData,CanvasFragmentNoTextureSPIRVDataSize);
 try
  fCanvasFragmentNoTextureShaderModule:=TpvVulkanShaderModule.Create(fDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineCanvasShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fCanvasVertexShaderModule,'main');

 fVulkanPipelineCanvasShaderStageFragmentAtlasTexture:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fCanvasFragmentAtlasTextureShaderModule,'main');

 fVulkanPipelineCanvasShaderStageFragmentNoTexture:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fCanvasFragmentNoTextureShaderModule,'main');

end;

destructor TpvCanvasCommon.Destroy;
begin
 fDevice.CanvasCommon:=nil;
 FreeAndNil(fVulkanPipelineCanvasShaderStageVertex);
 FreeAndNil(fVulkanPipelineCanvasShaderStageFragmentAtlasTexture);
 FreeAndNil(fVulkanPipelineCanvasShaderStageFragmentNoTexture);
 FreeAndNil(fCanvasVertexShaderModule);
 FreeAndNil(fCanvasFragmentAtlasTextureShaderModule);
 FreeAndNil(fCanvasFragmentNoTextureShaderModule);
 inherited Destroy;
end;

class function TpvCanvasCommon.Acquire(const aDevice:TpvVulkanDevice):TpvCanvasCommon;
begin
 while TPasMPInterlocked.CompareExchange(fLock,-1,0)<>0 do begin
  TPasMP.Yield;
 end;
 try
  result:=TpvCanvasCommon(aDevice.CanvasCommon);
  if not assigned(result) then begin
   result:=TpvCanvasCommon.Create(aDevice);
  end;
  TPasMPInterlocked.Increment(result.fReferenceCounter);
 finally
  TPasMPInterlocked.Write(fLock,0);
 end;
end;

class procedure TpvCanvasCommon.Release(const aDevice:TpvVulkanDevice);
var CanvasCommon:TpvCanvasCommon;
begin
 while TPasMPInterlocked.CompareExchange(fLock,-1,0)<>0 do begin
  TPasMP.Yield;
 end;
 try
  if assigned(aDevice) then begin
   CanvasCommon:=TpvCanvasCommon(aDevice.CanvasCommon);
   if assigned(CanvasCommon) then begin
    if TPasMPInterlocked.Decrement(CanvasCommon.fReferenceCounter)=0 then begin
     CanvasCommon.Free;
    end;
   end;
  end;
 finally
  TPasMPInterlocked.Write(fLock,0);
 end;
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
    VulkanPipelineLayout:TpvVulkanPipelineLayout;
    VulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
    VulkanCanvasBuffer:PpvCanvasBuffer;
    Stream:TStream;
    TextureModeIndex:boolean;
begin
 inherited Create;

 fDevice:=aDevice;

 fCanvasCommon:=TpvCanvasCommon.Acquire(fDevice);

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

 fCurrentCountVertices:=0;
 fCurrentCountIndices:=0;

 fInternalTexture:=nil;

 fShape:=TpvCanvasShape.Create;

 fState:=TpvCanvasState.Create;

 fStateStack:=TpvCanvasStateStack.Create(true);

 fState.Reset;

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

 fVulkanDescriptorSetTextureLayout:=TpvVulkanDescriptorSetLayout.Create(fDevice);
 fVulkanDescriptorSetTextureLayout.AddBinding(0,
                                              VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                              1,
                                              TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                              []);
 fVulkanDescriptorSetTextureLayout.Initialize;

 fVulkanDescriptorSetNoTextureLayout:=TpvVulkanDescriptorSetLayout.Create(fDevice);
 fVulkanDescriptorSetNoTextureLayout.Initialize;

 fVulkanDescriptorPools:=nil;
 fVulkanDescriptorSets:=nil;
 fCountVulkanDescriptors:=0;

 fVulkanTextureDescriptorSetHashMap:=TpvCanvasTextureDescriptorSetHashMap.Create(-1);

 fVulkanRenderPass:=aRenderPass;

 for TextureModeIndex:=false to true do begin

  VulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(fDevice);
  fVulkanPipelineLayouts[TextureModeIndex]:=VulkanPipelineLayout;
  if TextureModeIndex then begin
   VulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetTextureLayout);
  end else begin
   VulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetNoTextureLayout);
  end;
  VulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT),0,SizeOf(TpvMatrix4x4));
  VulkanPipelineLayout.Initialize;

  VulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(fDevice,
                                                           fPipelineCache,
                                                           0,
                                                           [],
                                                           VulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           0,
                                                           nil,
                                                           0);
  fVulkanGraphicsPipelines[TextureModeIndex]:=VulkanGraphicsPipeline;

  VulkanGraphicsPipeline.AddStage(fCanvasCommon.fVulkanPipelineCanvasShaderStageVertex);

  if TextureModeIndex then begin
   VulkanGraphicsPipeline.AddStage(fCanvasCommon.fVulkanPipelineCanvasShaderStageFragmentAtlasTexture);
  end else begin
   VulkanGraphicsPipeline.AddStage(fCanvasCommon.fVulkanPipelineCanvasShaderStageFragmentNoTexture);
  end;

  VulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
  VulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

  VulkanGraphicsPipeline.VertexInputState.AddVertexInputBindingDescription(0,SizeOf(TpvCanvasVertex),VK_VERTEX_INPUT_RATE_VERTEX);
  VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(0,0,VK_FORMAT_R32G32_SFLOAT,TpvPtrUInt(TpvPointer(@PpvCanvasVertex(nil)^.Position)));
  VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(1,0,VK_FORMAT_R16G16B16A16_SFLOAT,TpvPtrUInt(TpvPointer(@PpvCanvasVertex(nil)^.Color)));
  VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(2,0,VK_FORMAT_R32G32B32_SFLOAT,TpvPtrUInt(TpvPointer(@PpvCanvasVertex(nil)^.TextureCoord)));
  VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(3,0,VK_FORMAT_R32_UINT,TpvPtrUInt(TpvPointer(@PpvCanvasVertex(nil)^.State)));
  VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(4,0,VK_FORMAT_R32G32B32A32_SFLOAT,TpvPtrUInt(TpvPointer(@PpvCanvasVertex(nil)^.ClipRect)));
  VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(5,0,VK_FORMAT_R32G32B32A32_SFLOAT,TpvPtrUInt(TpvPointer(@PpvCanvasVertex(nil)^.MetaInfo)));

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

 end;

 fCurrentFillBuffer:=nil;

end;

destructor TpvCanvas.Destroy;
var Index,SubIndex:TpvInt32;
    RenderingModeIndex:TpvCanvasRenderingMode;
    BlendingModeIndex:TpvCanvasBlendingMode;
    VulkanCanvasBuffer:PpvCanvasBuffer;
    TextureModeIndex:boolean;
begin

 FreeAndNil(fStateStack);

 FreeAndNil(fState);

 FreeAndNil(fShape);

 for TextureModeIndex:=false to true do begin
  FreeAndNil(fVulkanGraphicsPipelines[TextureModeIndex]);
  FreeAndNil(fVulkanPipelineLayouts[TextureModeIndex]);
 end;

//FreeAndNil(fVulkanRenderPass);

 for Index:=0 to fCountVulkanDescriptors-1 do begin
  FreeAndNil(fVulkanDescriptorSets[Index]);
 end;

 fVulkanDescriptorSets:=nil;

 FreeAndNil(fVulkanDescriptorSetTextureLayout);
 FreeAndNil(fVulkanDescriptorSetNoTextureLayout);

 for Index:=0 to fCountVulkanDescriptors-1 do begin
  FreeAndNil(fVulkanDescriptorPools[Index]);
 end;

 fVulkanDescriptorPools:=nil;

 FreeAndNil(fVulkanTextureDescriptorSetHashMap);

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

 fCanvasCommon:=nil;

 TpvCanvasCommon.Release(fDevice);

 inherited Destroy;
end;

procedure TpvCanvas.SetInternalTexture(const aTexture:TpvSpriteAtlasArrayTexture);
begin
 if fInternalTexture<>aTexture then begin
  Flush;
  fInternalTexture:=aTexture;
 end;
end;

procedure TpvCanvas.SetScissor(const aScissor:TVkRect2D);
begin
 if (fState.fScissor.offset.x<>aScissor.offset.x) or
    (fState.fScissor.offset.y<>aScissor.offset.y) or
    (fState.fScissor.extent.Width<>aScissor.extent.Width) or
    (fState.fScissor.extent.Height<>aScissor.extent.Height) then begin
  Flush;
  fState.fScissor:=aScissor;
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
 fState.fClipRect.LeftTop:=TpvVector2.Create(aClipRect.offset.x,aClipRect.offset.y);
 fState.fClipRect.RightBottom:=TpvVector2.Create(aClipRect.offset.x+TpvFloat(aClipRect.extent.width),aClipRect.offset.y+TpvFloat(aClipRect.extent.height));
end;

procedure TpvCanvas.SetClipRect(const aClipRect:TpvRect);
begin
 fState.fClipRect:=aClipRect;
end;

procedure TpvCanvas.SetClipRect(const aLeft,aTop,aWidth,aHeight:TpvInt32);
begin
 fState.fClipRect.LeftTop:=TpvVector2.Create(aLeft,aTop);
 fState.fClipRect.RightBottom:=TpvVector2.Create(aLeft+aWidth,aTop+aHeight);
end;

function TpvCanvas.GetBlendingMode:TpvCanvasBlendingMode;
begin
 result:=fState.fBlendingMode;
end;

procedure TpvCanvas.SetBlendingMode(const aBlendingMode:TpvCanvasBlendingMode);
begin
 fState.fBlendingMode:=aBlendingMode;
end;

function TpvCanvas.GetLineWidth:TpvFloat;
begin
 result:=fState.fLineWidth;
end;

procedure TpvCanvas.SetLineWidth(const aLineWidth:TpvFloat);
begin
 fState.fLineWidth:=aLineWidth;
end;

function TpvCanvas.GetMiterLimit:TpvFloat;
begin
 result:=fState.fMiterLimit;
end;

procedure TpvCanvas.SetMiterLimit(const aMiterLimit:TpvFloat);
begin
 fState.fMiterLimit:=aMiterLimit;
end;

function TpvCanvas.GetLineJoin:TpvCanvasLineJoin;
begin
 result:=fState.fLineJoin;
end;

procedure TpvCanvas.SetLineJoin(const aLineJoin:TpvCanvasLineJoin);
begin
 fState.fLineJoin:=aLineJoin;
end;

function TpvCanvas.GetLineCap:TpvCanvasLineCap;
begin
 result:=fState.fLineCap;
end;

procedure TpvCanvas.SetLineCap(const aLineCap:TpvCanvasLineCap);
begin
 fState.fLineCap:=aLineCap;
end;

function TpvCanvas.GetFillRule:TpvCanvasFillRule;
begin
 result:=fState.fFillRule;
end;

procedure TpvCanvas.SetFillRule(const aFillRule:TpvCanvasFillRule);
begin
 fState.fFillRule:=aFillRule;
end;

function TpvCanvas.GetColor:TpvVector4;
begin
 result:=fState.fColor;
end;

procedure TpvCanvas.SetColor(const aColor:TpvVector4);
begin
 fState.fColor:=aColor;
end;

function TpvCanvas.GetProjectionMatrix:TpvMatrix4x4;
begin
 result:=fState.fProjectionMatrix;
end;

procedure TpvCanvas.SetProjectionMatrix(const aProjectionMatrix:TpvMatrix4x4);
begin
 if fState.fProjectionMatrix<>aProjectionMatrix then begin
  Flush;
  fState.fProjectionMatrix:=aProjectionMatrix;
 end;
end;

function TpvCanvas.GetViewMatrix:TpvMatrix4x4;
begin
 result:=fState.fViewMatrix;
end;

procedure TpvCanvas.SetViewMatrix(const aViewMatrix:TpvMatrix4x4);
begin
 if fState.fViewMatrix<>aViewMatrix then begin
  Flush;
  fState.fViewMatrix:=aViewMatrix;
 end;
end;

function TpvCanvas.GetModelMatrix:TpvMatrix3x3;
begin
 result:=fState.fModelMatrix;
end;

procedure TpvCanvas.SetModelMatrix(const aModelMatrix:TpvMatrix3x3);
begin
 fState.fModelMatrix:=aModelMatrix;
end;

function TpvCanvas.GetFont:TpvFont;
begin
 result:=fState.fFont;
end;

procedure TpvCanvas.SetFont(const aFont:TpvFont);
begin
 fState.fFont:=aFont;
end;

function TpvCanvas.GetFontSize:TpvFloat;
begin
 result:=fState.fFontSize;
end;

procedure TpvCanvas.SetFontSize(const aFontSize:TpvFloat);
begin
 fState.fFontSize:=aFontSize;
end;

function TpvCanvas.GetTextHorizontalAlignment:TpvCanvasTextHorizontalAlignment;
begin
 result:=fState.fTextHorizontalAlignment;
end;

procedure TpvCanvas.SetTextHorizontalAlignment(aTextHorizontalAlignment:TpvCanvasTextHorizontalAlignment);
begin
 fState.fTextHorizontalAlignment:=aTextHorizontalAlignment;
end;

function TpvCanvas.GetTextVerticalAlignment:TpvCanvasTextVerticalAlignment;
begin
 result:=fState.fTextVerticalAlignment;
end;

procedure TpvCanvas.SetTextVerticalAlignment(aTextVerticalAlignment:TpvCanvasTextVerticalAlignment);
begin
 fState.fTextVerticalAlignment:=aTextVerticalAlignment;
end;

function TpvCanvas.GetVertexState:TpvUInt32;
begin
 result:=(pvCanvasBlendingModeValues[fState.fBlendingMode] shl 0) or
         (pvCanvasRenderingModeValues[fInternalRenderingMode] shl 2);
end;

procedure TpvCanvas.Start(const aBufferIndex:TpvInt32);
begin

 fCurrentCountVertices:=0;
 fCurrentCountIndices:=0;

 fCurrentFillBuffer:=@fVulkanCanvasBuffers[aBufferIndex];
 fCurrentFillBuffer^.fCountQueueItems:=0;
 fCurrentFillBuffer^.fCountUsedBuffers:=0;

 fInternalTexture:=nil;

 fState.Reset;

 fState.fScissor.offset.x:=trunc(floor(fViewport.x));
 fState.fScissor.offset.y:=trunc(floor(fViewport.y));
 fState.fScissor.extent.Width:=trunc(ceil(fViewport.Width));
 fState.fScissor.extent.Height:=trunc(ceil(fViewport.Height));

 fState.fClipRect:=TpvRect.Create(0.0,0.0,fWidth,fHeight);

 fState.fProjectionMatrix:=TpvMatrix4x4.CreateOrtho(0.0,fWidth,0.0,fHeight,-100.0,100.0);

 fCurrentVulkanBufferIndex:=-1;
 fCurrentVulkanVertexBufferOffset:=0;
 fCurrentVulkanIndexBufferOffset:=0;

 GetNextDestinationVertexBuffer;

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

   if not fVulkanTextureDescriptorSetHashMap.TryGet(fInternalTexture,DescriptorIndex) then begin
    DescriptorIndex:=fCountVulkanDescriptors;
    inc(fCountVulkanDescriptors);
    if length(fVulkanDescriptorPools)<fCountVulkanDescriptors then begin
     SetLength(fVulkanDescriptorPools,fCountVulkanDescriptors*2);
    end;
    if length(fVulkanDescriptorSets)<fCountVulkanDescriptors then begin
     SetLength(fVulkanDescriptorSets,fCountVulkanDescriptors*2);
    end;
    if assigned(fInternalTexture) then begin
     VulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(fDevice,
                                                          TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                          1);
     fVulkanDescriptorPools[DescriptorIndex]:=VulkanDescriptorPool;
     VulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,1);
     VulkanDescriptorPool.Initialize;
     VulkanDescriptorSet:=TpvVulkanDescriptorSet.Create(VulkanDescriptorPool,
                                                        fVulkanDescriptorSetTextureLayout);
     fVulkanDescriptorSets[DescriptorIndex]:=VulkanDescriptorSet;
     if fInternalTexture is TpvSpriteAtlasArrayTexture then begin
      VulkanDescriptorSet.WriteToDescriptorSet(0,
                                               0,
                                               1,
                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                               [TpvSpriteAtlasArrayTexture(fInternalTexture).Texture.DescriptorImageInfo],
                                               [],
                                               [],
                                               false
                                              );
     end else begin
      VulkanDescriptorSet.WriteToDescriptorSet(0,
                                               0,
                                               1,
                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                               [TpvVulkanTexture(fInternalTexture).DescriptorImageInfo],
                                               [],
                                               [],
                                               false
                                              );
     end;
     VulkanDescriptorSet.Flush;
    end else begin
     VulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(fDevice,
                                                          TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                          1);
     fVulkanDescriptorPools[DescriptorIndex]:=VulkanDescriptorPool;
     VulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,1);
     VulkanDescriptorPool.Initialize;
     VulkanDescriptorSet:=TpvVulkanDescriptorSet.Create(VulkanDescriptorPool,
                                                        fVulkanDescriptorSetNoTextureLayout);
     fVulkanDescriptorSets[DescriptorIndex]:=VulkanDescriptorSet;
     VulkanDescriptorSet.Flush;
    end;
    fVulkanTextureDescriptorSetHashMap.Add(fInternalTexture,DescriptorIndex);
   end;

   QueueItemIndex:=fCurrentFillBuffer^.fCountQueueItems;
   inc(fCurrentFillBuffer^.fCountQueueItems);
   if length(fCurrentFillBuffer^.fQueueItems)<fCurrentFillBuffer^.fCountQueueItems then begin
    SetLength(fCurrentFillBuffer^.fQueueItems,fCurrentFillBuffer^.fCountQueueItems*2);
   end;
   QueueItem:=@fCurrentFillBuffer^.fQueueItems[QueueItemIndex];
   QueueItem^.Kind:=pvcqikNormal;
   QueueItem^.BufferIndex:=CurrentVulkanBufferIndex;
   QueueItem^.DescriptorIndex:=DescriptorIndex;
   QueueItem^.TextureMode:=assigned(fInternalTexture);
   QueueItem^.StartVertexIndex:=fCurrentVulkanVertexBufferOffset;
   QueueItem^.StartIndexIndex:=fCurrentVulkanIndexBufferOffset;
   QueueItem^.CountVertices:=fCurrentCountVertices;
   QueueItem^.CountIndices:=fCurrentCountIndices;
   QueueItem^.Scissor:=fState.fScissor;
   QueueItem^.Matrix:=fState.fViewMatrix*fState.fProjectionMatrix;

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
 result:=(fState.fClipRect.LeftTop.x<=(aX1+Threshold)) and
         (aX0<=(fState.fClipRect.RightBottom.x+Threshold)) and
         (fState.fClipRect.LeftTop.y<=(aY1+Threshold)) and
         (aY0<=(fState.fClipRect.RightBottom.y+Threshold));
end;

procedure TpvCanvas.ExecuteUpload(const aVulkanCommandBuffer:TpvVulkanCommandBuffer;const aBufferIndex:TpvInt32);
var Index:TpvInt32;
    CurrentBuffer:PpvCanvasBuffer;
    VulkanBuffer:TpvVulkanBuffer;
begin
 CurrentBuffer:=@fVulkanCanvasBuffers[aBufferIndex];
 if assigned(CurrentBuffer) and (CurrentBuffer^.fCountUsedBuffers>0) then begin
  while TPasMPInterlocked.CompareExchange(CurrentBuffer^.fSpinLock,-1,0)<>0 do begin
  end;
  try
   for Index:=0 to CurrentBuffer^.fCountUsedBuffers-1 do begin
    if CurrentBuffer^.fVertexBufferSizes[Index]>0 then begin
     VulkanBuffer:=CurrentBuffer^.fVulkanVertexBuffers[Index];
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
      CurrentBuffer^.fVulkanVertexBuffers[Index]:=VulkanBuffer;
     end;
     if assigned(VulkanBuffer) then begin
      VulkanBuffer.UploadData(fTransferQueue,
                              fTransferCommandBuffer,
                              fTransferFence,
                              CurrentBuffer^.fVertexBuffers[Index,0],
                              0,
                              CurrentBuffer^.fVertexBufferSizes[Index],
                              vbutsbmNo);
     end;
    end;
    if CurrentBuffer^.fIndexBufferSizes[Index]>0 then begin
     VulkanBuffer:=CurrentBuffer^.fVulkanIndexBuffers[Index];
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
      CurrentBuffer^.fVulkanIndexBuffers[Index]:=VulkanBuffer;
     end;
     if assigned(VulkanBuffer) then begin
      VulkanBuffer.UploadData(fTransferQueue,
                              fTransferCommandBuffer,
                              fTransferFence,
                              CurrentBuffer^.fIndexBuffers[Index,0],
                              0,
                              CurrentBuffer^.fIndexBufferSizes[Index],
                              vbutsbmNo);
     end;
    end;
   end;
  finally
   TPasMPInterlocked.Exchange(CurrentBuffer^.fSpinLock,0);
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
    CurrentBuffer:PpvCanvasBuffer;
    VulkanVertexBuffer,VulkanIndexBuffer:TpvVulkanBuffer;
    OldScissor:TVkRect2D;
    Matrix:TpvMatrix4x4;
    ForceUpdate,TextureMode:boolean;
    Offsets:array[0..0] of TVkDeviceSize;
begin
 CurrentBuffer:=@fVulkanCanvasBuffers[aBufferIndex];
 if assigned(CurrentBuffer) and (CurrentBuffer^.fCountQueueItems>0) then begin

  OldScissor.offset.x:=-$7fffffff;
  OldScissor.offset.y:=-$7fffffff;
  OldScissor.extent.Width:=$7fffffff;
  OldScissor.extent.Height:=$7fffffff;

  DescriptorIndex:=-1;

  Matrix:=TpvMatrix4x4.Null;

  OldQueueItemKind:=pvcqikNone;

  ForceUpdate:=true;

  TextureMode:=true;

  for Index:=0 to CurrentBuffer^.fCountQueueItems-1 do begin

   QueueItem:=@CurrentBuffer^.fQueueItems[Index];

   if OldQueueItemKind<>QueueItem^.Kind then begin
    OldQueueItemKind:=QueueItem^.Kind;
    ForceUpdate:=true;
   end;

   case QueueItem^.Kind of
    pvcqikNormal:begin

     VulkanVertexBuffer:=CurrentBuffer^.fVulkanVertexBuffers[QueueItem^.BufferIndex];

     VulkanIndexBuffer:=CurrentBuffer^.fVulkanIndexBuffers[QueueItem^.BufferIndex];

     if ForceUpdate then begin
      aVulkanCommandBuffer.CmdSetViewport(0,1,fPointerToViewport);
     end;

     if ForceUpdate or
        (TextureMode<>QueueItem^.TextureMode) then begin
      TextureMode:=QueueItem^.TextureMode;
      aVulkanCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipelines[QueueItem^.TextureMode].Handle);
      OldScissor.offset.x:=-$7fffffff;
      OldScissor.offset.y:=-$7fffffff;
      OldScissor.extent.Width:=$7fffffff;
      OldScissor.extent.Height:=$7fffffff;
      DescriptorIndex:=-1;
     end;

     if ForceUpdate or
        (DescriptorIndex<>QueueItem^.DescriptorIndex) then begin
      DescriptorIndex:=QueueItem^.DescriptorIndex;
      aVulkanCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanPipelineLayouts[QueueItem^.TextureMode].Handle,0,1,@fVulkanDescriptorSets[DescriptorIndex].Handle,0,nil);
     end;

     if ForceUpdate or
        (Matrix<>QueueItem^.Matrix) then begin
      aVulkanCommandBuffer.CmdPushConstants(fVulkanPipelineLayouts[TextureMode].Handle,TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT),0,SizeOf(TpvMatrix4x4),@QueueItem^.Matrix);
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
    pvcqikVector:begin
     // TODO
    end;
    pvcqikHook:begin
     if assigned(QueueItem^.Hook) then begin
      QueueItem^.Hook(QueueItem^.HookData);
     end;
     ForceUpdate:=true;
    end;
    else {pvcqikNone:}begin
     ForceUpdate:=true;
    end;
   end;

  end;

  CurrentBuffer^.fCountQueueItems:=0;
  CurrentBuffer^.fCountUsedBuffers:=0;

 end;
end;

function TpvCanvas.Push:TpvCanvas;
var NewState:TpvCanvasState;
begin
 NewState:=TpvCanvasState.Create;
 NewState.Assign(fState);
 fStateStack.Push(TpvCanvasState(TObject(TPasMPInterlocked.Exchange(TObject(fState),TObject(NewState)))));
 result:=self;
end;

function TpvCanvas.Pop:TpvCanvas;
begin
 Flush;
 TpvCanvasState(TObject(TPasMPInterlocked.Exchange(TObject(fState),TObject(fStateStack.Extract)))).Free;
 result:=self;
end;

function TpvCanvas.Hook(const aHook:TpvCanvasHook;const aData:TpvPointer):TpvCanvas;
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
  QueueItem^.Kind:=pvcqikHook;
  QueueItem^.Hook:=aHook;
  QueueItem^.HookData:=aData;

 end;
 result:=self;
end;

function TpvCanvas.DrawSprite(const aSprite:TpvSprite;const aSrc,aDest:TpvRect;const aRenderingMode:TpvCanvasRenderingMode):TpvCanvas;
const MinA=1.0/65536.0;
var tx1,ty1,tx2,ty2,xf,yf,sX0,sY0,sX1,sY1:TpvFloat;
    TempDest,TempSrc:TpvRect;
    VertexColor:TpvHalfFloatVector4;
    VertexState:TpvUInt32;
begin
 if ((fState.fBlendingMode=pvcbmNone) or (abs(fState.fColor.a)>MinA)) and
    ClipCheck(aDest.Left,aDest.Top,aDest.Right,aDest.Bottom) and
    (((aSrc.Right>=aSprite.TrimmedX) and (aSrc.Bottom>=aSprite.TrimmedY)) and
    (((not aSprite.Rotated) and (((aSprite.TrimmedX+aSprite.TrimmedWidth)>=aSrc.Left) and ((aSprite.TrimmedY+aSprite.TrimmedHeight)>=aSrc.Top))) or
     (aSprite.Rotated and (((aSprite.TrimmedX+aSprite.TrimmedHeight)>=aSrc.Left) and ((aSprite.TrimmedY+aSprite.TrimmedWidth)>=aSrc.Top))))) then begin
  fInternalRenderingMode:=aRenderingMode;
  VertexColor.r:=fState.fColor.r;
  VertexColor.g:=fState.fColor.g;
  VertexColor.b:=fState.fColor.b;
  VertexColor.a:=fState.fColor.a;
  VertexState:=GetVertexState;
  SetInternalTexture(aSprite.ArrayTexture);
  FlushAndGetNewDestinationVertexBufferIfNeeded(4,6);
  if aSprite.Rotated then begin
   tx1:=Max(aSprite.TrimmedX,aSrc.Left);
   ty1:=Max(aSprite.TrimmedY,aSrc.Top);
   tx2:=Min((aSprite.TrimmedX+aSprite.TrimmedHeight),aSrc.Right);
   ty2:=Min((aSprite.TrimmedY+aSprite.TrimmedWidth),aSrc.Bottom);
   xf:=abs(aDest.Right-aDest.Left)/(aSrc.Right-aSrc.Left);
   yf:=abs(aDest.Bottom-aDest.Top)/(aSrc.Bottom-aSrc.Top);
   TempDest.Left:=aDest.Left+((tx1-aSrc.Left)*xf);
   TempDest.Right:=aDest.Right+((tx2-aSrc.Right)*xf);
   TempDest.Top:=aDest.Top+((ty1-aSrc.Top)*yf);
   TempDest.Bottom:=aDest.Bottom+((ty2-aSrc.Bottom)*yf);
{  if aDest.Left<=aDest.Right then begin
    TempDest.Left:=aDest.Left+((tx1-aSrc.Left)*xf);
    TempDest.Right:=aDest.Right+((tx2-aSrc.Right)*xf);
   end else begin
    TempDest.Left:=aDest.Left+((tx2-aSrc.Right)*xf);
    TempDest.Right:=aDest.Right+((tx1-aSrc.Left)*xf);
   end;
   if aDest.Top<=aDest.Bottom then begin
    TempDest.Top:=aDest.Top+((ty1-aSrc.Top)*yf);
    TempDest.Bottom:=aDest.Bottom+((ty2-aSrc.Bottom)*yf);
   end else begin
    TempDest.Top:=aDest.Top+((ty2-aSrc.Bottom)*yf);
    TempDest.Bottom:=aDest.Bottom+((ty1-aSrc.Top)*yf);
   end;}
   TempSrc.Left:=(tx1-aSprite.TrimmedX)+aSprite.x;
   TempSrc.Top:=(ty1-aSprite.TrimmedY)+aSprite.y;
   TempSrc.Right:=TempSrc.Left+(ty2-ty1);
   TempSrc.Bottom:=TempSrc.Top+(tx2-tx1);
   sX0:=TempSrc.Left*TpvSpriteAtlasArrayTexture(fInternalTexture).InverseWidth;
   sY0:=TempSrc.Top*TpvSpriteAtlasArrayTexture(fInternalTexture).InverseHeight;
   sX1:=TempSrc.Right*TpvSpriteAtlasArrayTexture(fInternalTexture).InverseWidth;
   sY1:=TempSrc.Bottom*TpvSpriteAtlasArrayTexture(fInternalTexture).InverseHeight;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+0]:=fCurrentCountVertices+0;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+1]:=fCurrentCountVertices+1;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+2]:=fCurrentCountVertices+2;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+3]:=fCurrentCountVertices+0;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+4]:=fCurrentCountVertices+2;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+5]:=fCurrentCountVertices+3;
   inc(fCurrentCountIndices,6);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position:=fState.fModelMatrix*TpvVector2.Create(TempDest.Left,TempDest.Top);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=aSprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=VertexState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fState.fClipRect;
   inc(fCurrentCountVertices);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position:=fState.fModelMatrix*TpvVector2.Create(TempDest.Right,TempDest.Top);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=aSprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=VertexState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fState.fClipRect;
   inc(fCurrentCountVertices);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position:=fState.fModelMatrix*TpvVector2.Create(TempDest.Right,TempDest.Bottom);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=aSprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=VertexState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fState.fClipRect;
   inc(fCurrentCountVertices);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position:=fState.fModelMatrix*TpvVector2.Create(TempDest.Left,TempDest.Bottom);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=aSprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=VertexState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fState.fClipRect;
   inc(fCurrentCountVertices);
   inc(fCurrentCountIndices,6);
  end else begin
   tx1:=Max(aSprite.TrimmedX,aSrc.Left);
   ty1:=Max(aSprite.TrimmedY,aSrc.Top);
   tx2:=Min((aSprite.TrimmedX+aSprite.TrimmedWidth),aSrc.Right);
   ty2:=Min((aSprite.TrimmedY+aSprite.TrimmedHeight),aSrc.Bottom);
   xf:=abs(aDest.Right-aDest.Left)/(aSrc.Right-aSrc.Left);
   yf:=abs(aDest.Bottom-aDest.Top)/(aSrc.Bottom-aSrc.Top);
   TempDest.Left:=aDest.Left+((tx1-aSrc.Left)*xf);
   TempDest.Right:=aDest.Right+((tx2-aSrc.Right)*xf);
   TempDest.Top:=aDest.Top+((ty1-aSrc.Top)*yf);
   TempDest.Bottom:=aDest.Bottom+((ty2-aSrc.Bottom)*yf);
{  if aDest.Left<=aDest.Right then begin
    TempDest.Left:=aDest.Left+((tx1-aSrc.Left)*xf);
    TempDest.Right:=aDest.Right+((tx2-aSrc.Right)*xf);
   end else begin
    TempDest.Left:=aDest.Left+((tx2-aSrc.Right)*xf);
    TempDest.Right:=aDest.Right+((tx1-aSrc.Left)*xf);
   end;
   if aDest.Top<=aDest.Bottom then begin
    TempDest.Top:=aDest.Top+((ty1-aSrc.Top)*yf);
    TempDest.Bottom:=aDest.Bottom+((ty2-aSrc.Bottom)*yf);
   end else begin
    TempDest.Top:=aDest.Bottom+((ty2-aSrc.Bottom)*yf);
    TempDest.Bottom:=aDest.Top+((ty1-aSrc.Top)*yf);
   end;}
   TempSrc.Left:=(tx1-aSprite.TrimmedX)+aSprite.x;
   TempSrc.Top:=(ty1-aSprite.TrimmedY)+aSprite.y;
   TempSrc.Right:=TempSrc.Left+(tx2-tx1);
   TempSrc.Bottom:=TempSrc.Top+(ty2-ty1);
   if fState.fModelMatrix=Matrix3x3Identity then begin
    if TempDest.Left<fState.fClipRect.LeftTop.x then begin
     TempSrc.Left:=TempSrc.Left+((TempSrc.Right-TempSrc.Left)*((fState.fClipRect.LeftTop.x-TempDest.Left)/(TempDest.Right-TempDest.Left)));
     TempDest.Left:=fState.fClipRect.LeftTop.x;
    end;
    if TempDest.Top<fState.fClipRect.LeftTop.y then begin
     TempSrc.Top:=TempSrc.Top+((TempSrc.Bottom-TempSrc.Top)*((fState.fClipRect.LeftTop.y-TempDest.Top)/(TempDest.Bottom-TempDest.Top)));
     TempDest.Top:=fState.fClipRect.LeftTop.y;
    end;
    if TempDest.Right>fState.fClipRect.RightBottom.x then begin
     TempSrc.Right:=TempSrc.Left+((TempSrc.Right-TempSrc.Left)*((fState.fClipRect.RightBottom.x-TempDest.Left)/(TempDest.Right-TempDest.Left)));
     TempDest.Right:=fState.fClipRect.RightBottom.x;
    end;
    if TempDest.Bottom>fState.fClipRect.RightBottom.y then begin
     TempSrc.Bottom:=TempSrc.Top+((TempSrc.Bottom-TempSrc.Top)*((fState.fClipRect.RightBottom.y-TempDest.Top)/(TempDest.Bottom-TempDest.Top)));
     TempDest.Bottom:=fState.fClipRect.RightBottom.y;
    end;
   end;
   sX0:=TempSrc.Left*TpvSpriteAtlasArrayTexture(fInternalTexture).InverseWidth;
   sY0:=TempSrc.Top*TpvSpriteAtlasArrayTexture(fInternalTexture).InverseHeight;
   sX1:=TempSrc.Right*TpvSpriteAtlasArrayTexture(fInternalTexture).InverseWidth;
   sY1:=TempSrc.Bottom*TpvSpriteAtlasArrayTexture(fInternalTexture).InverseHeight;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+0]:=fCurrentCountVertices+0;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+1]:=fCurrentCountVertices+1;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+2]:=fCurrentCountVertices+2;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+3]:=fCurrentCountVertices+0;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+4]:=fCurrentCountVertices+2;
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices+5]:=fCurrentCountVertices+3;
   inc(fCurrentCountIndices,6);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position:=fState.fModelMatrix*TpvVector2.Create(TempDest.Left,TempDest.Top);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=aSprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=VertexState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fState.fClipRect;
   inc(fCurrentCountVertices);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position:=fState.fModelMatrix*TpvVector2.Create(TempDest.Right,TempDest.Top);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=aSprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=VertexState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fState.fClipRect;
   inc(fCurrentCountVertices);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position:=fState.fModelMatrix*TpvVector2.Create(TempDest.Right,TempDest.Bottom);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=aSprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=VertexState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fState.fClipRect;
   inc(fCurrentCountVertices);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Position:=fState.fModelMatrix*TpvVector2.Create(TempDest.Left,TempDest.Bottom);
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.x:=sX0;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.y:=sY1;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].TextureCoord.z:=aSprite.Layer;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].Color:=VertexColor;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].State:=VertexState;
   fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices].ClipRect:=fState.fClipRect;
   inc(fCurrentCountVertices);
   inc(fCurrentCountIndices,6);
  end;
 end;
 result:=self;
end;

function TpvCanvas.DrawFontGlyphSprite(const aSprite:TpvSprite;const aSrc,aDest:TpvRect):TpvCanvas;
begin
 result:=DrawSprite(aSprite,aSrc,aDest,pvcrmFont);
end;

function TpvCanvas.DrawSprite(const aSprite:TpvSprite;const aSrc,aDest:TpvRect):TpvCanvas;
begin
 result:=DrawSprite(aSprite,aSrc,aDest,pvcrmNormal);
end;

function TpvCanvas.DrawSprite(const aSprite:TpvSprite;const aSrc,aDest:TpvRect;const aOrigin:TpvVector2;const aRotation:TpvFloat):TpvCanvas;
var OldMatrix:TpvMatrix3x3;
    AroundPoint:TpvVector2;
begin
 OldMatrix:=fState.fModelMatrix;
 try
  AroundPoint:=aDest.LeftTop+aOrigin;
  fState.fModelMatrix:=((TpvMatrix3x3.CreateTranslation(-AroundPoint)*
                         TpvMatrix3x3.CreateRotateZ(aRotation))*
                        TpvMatrix3x3.CreateTranslation(AroundPoint))*
                        fState.fModelMatrix;
  result:=DrawSprite(aSprite,aSrc,aDest,pvcrmNormal);
 finally
  fState.fModelMatrix:=OldMatrix;
 end;
end;

function TpvCanvas.DrawSprite(const aSprite:TpvSprite;const aPosition:TpvVector2):TpvCanvas;
begin
 DrawSprite(aSprite,
            TpvRect.Create(0.0,0.0,aSprite.Width,aSprite.Height),
            TpvRect.Create(aPosition.x,aPosition.y,aPosition.x+aSprite.Width,aPosition.y+aSprite.Height));
 result:=self;
end;

function TpvCanvas.DrawSprite(const aSprite:TpvSprite):TpvCanvas;
begin
 DrawSprite(aSprite,
            TpvVector2.Create(0.0,0.0));
 result:=self;
end;

function TpvCanvas.TextWidth(const aText:TpvUTF8String):TpvFloat;
begin
 if assigned(fState.fFont) then begin
  result:=fState.fFont.TextWidth(aText,fState.fFontSize);
 end else begin
  result:=0.0;
 end;
end;

function TpvCanvas.TextHeight(const aText:TpvUTF8String):TpvFloat;
begin
 if assigned(fState.fFont) then begin
  result:=fState.fFont.TextHeight(aText,fState.fFontSize);
 end else begin
  result:=0.0;
 end;
end;

function TpvCanvas.TextSize(const aText:TpvUTF8String):TpvVector2;
begin
 if assigned(fState.fFont) then begin
  result:=fState.fFont.TextSize(aText,fState.fFontSize);
 end else begin
  result:=TpvVector2.Create(0.0,0.0);
 end;
end;

function TpvCanvas.TextRowHeight(const aPercent:TpvFloat):TpvFloat;
begin
 if assigned(fState.fFont) then begin
  result:=fState.fFont.RowHeight(aPercent);
 end else begin
  result:=0.0;
 end;
end;

function TpvCanvas.DrawText(const aText:TpvUTF8String;const aPosition:TpvVector2):TpvCanvas;
var Position,Size:TpvVector2;
begin
 if assigned(fState.fFont) then begin
  Position:=aPosition;
  if fState.fTextHorizontalAlignment<>pvcthaLeft then begin
   if fState.fTextVerticalAlignment<>pvctvaTop then begin
    Size:=TextSize(aText);
   end else begin
    Size:=TpvVector2.Create(TextWidth(aText),0.0);
   end;
  end else begin
   if fState.fTextVerticalAlignment<>pvctvaTop then begin
    Size:=TpvVector2.Create(0.0,TextHeight(aText));
   end else begin
    Size:=TpvVector2.Create(0.0,0.0);
   end;
  end;
  case fState.fTextHorizontalAlignment of
   pvcthaLeft:begin
    // Do nothing
   end;
   pvcthaCenter:begin
    Position.x:=Position.x-(Size.x*0.5);
   end;
   pvcthaRight:begin
    Position.x:=Position.x-Size.x;
   end;
  end;
  case fState.fTextVerticalAlignment of
   pvctvaTop:begin
    // Do nothing
   end;
   pvctvaMiddle:begin
    Position.y:=Position.y-(Size.y*0.5);
   end;
   pvctvaBottom:begin
    Position.y:=Position.y-Size.y;
   end;
  end;
  fState.fFont.Draw(self,aText,Position,fState.fFontSize);
 end;
 result:=self;
end;

function TpvCanvas.DrawText(const aText:TpvUTF8String;const aX,aY:TpvFloat):TpvCanvas;
begin
 result:=DrawText(aText,TpvVector2.Create(aX,aY));
end;

function TpvCanvas.DrawText(const aText:TpvUTF8String):TpvCanvas;
begin
 result:=DrawText(aText,TpvVector2.Create(0.0,0.0));
end;

function TpvCanvas.DrawShape(const aShape:TpvCanvasShape):TpvCanvas;
var CacheGroupIndex,VertexIndex,IndexIndex:TpvInt32;
    CacheGroup:PpvCanvasShapeCacheGroup;
    CacheVertex:PpvCanvasShapeCacheVertex;
    CanvasVertex:PpvCanvasVertex;
    VertexColor:TpvHalfFloatVector4;
    VertexState,BaseVertexIndex:TpvUInt32;
begin
 fInternalRenderingMode:=pvcrmNormal;
 SetInternalTexture(nil);
 VertexColor.r:=fState.fColor.r;
 VertexColor.g:=fState.fColor.g;
 VertexColor.b:=fState.fColor.b;
 VertexColor.a:=fState.fColor.a;
 VertexState:=GetVertexState;
 for CacheGroupIndex:=0 to aShape.fCountCacheGroups-1 do begin
  CacheGroup:=@aShape.fCacheGroups[CacheGroupIndex];
  FlushAndGetNewDestinationVertexBufferIfNeeded(CacheGroup^.CountVertices,CacheGroup^.CountIndices);
  BaseVertexIndex:=fCurrentCountVertices;
  for VertexIndex:=0 to CacheGroup^.CountVertices-1 do begin
   CacheVertex:=@CacheGroup^.Vertices[VertexIndex];
   CanvasVertex:=@fCurrentDestinationVertexBufferPointer^[fCurrentCountVertices];
   inc(fCurrentCountVertices);
   CanvasVertex^.Position:=fState.fModelMatrix*CacheVertex^.Position;
   CanvasVertex^.Color:=VertexColor;
   CanvasVertex^.TextureCoord:=Vector3Origin;
   CanvasVertex^.State:=VertexState or ((CacheVertex^.ObjectMode and $ff) shl 4);
   CanvasVertex^.ClipRect:=fState.fClipRect;
   CanvasVertex^.MetaInfo:=CacheVertex^.MetaInfo;
   case CacheVertex^.ObjectMode of
    pcvvaomRoundLineCapCircle:begin
     CanvasVertex^.MetaInfo.xy:=fState.fModelMatrix*CanvasVertex^.MetaInfo.xy;
    end;
   end;
  end;
  for IndexIndex:=0 to CacheGroup^.CountIndices-1 do begin
   fCurrentDestinationIndexBufferPointer^[fCurrentCountIndices]:=BaseVertexIndex+CacheGroup^.Indices[IndexIndex];
   inc(fCurrentCountIndices);
  end;
 end;
 result:=self;
end;

function TpvCanvas.BeginPath:TpvCanvas;
begin
 fState.fPath.BeginPath;
 result:=self;
end;

function TpvCanvas.EndPath:TpvCanvas;
begin
 fState.fPath.EndPath;
 result:=self;
end;

function TpvCanvas.ClosePath:TpvCanvas;
begin
 fState.fPath.ClosePath;
 result:=self;
end;

function TpvCanvas.MoveTo(const aP0:TpvVector2):TpvCanvas;
begin
 fState.fPath.MoveTo(aP0);
 result:=self;
end;

function TpvCanvas.MoveTo(const aX,aY:TpvFloat):TpvCanvas;
begin
 fState.fPath.MoveTo(TpvVector2.Create(aX,aY));
 result:=self;
end;

function TpvCanvas.LineTo(const aP0:TpvVector2):TpvCanvas;
begin
 fState.fPath.LineTo(aP0);
 result:=self;
end;

function TpvCanvas.LineTo(const aX,aY:TpvFloat):TpvCanvas;
begin
 fState.fPath.LineTo(TpvVector2.Create(aX,aY));
 result:=self;
end;

function TpvCanvas.QuadraticCurveTo(const aC0,aA0:TpvVector2):TpvCanvas;
begin
 fState.fPath.QuadraticCurveTo(aC0,aA0);
 result:=self;
end;

function TpvCanvas.QuadraticCurveTo(const aCX,aCY,aAX,aAY:TpvFloat):TpvCanvas;
begin
 fState.fPath.QuadraticCurveTo(TpvVector2.Create(aCX,aCY),TpvVector2.Create(aAX,aAY));
 result:=self;
end;

function TpvCanvas.CubicCurveTo(const aC0,aC1,aA0:TpvVector2):TpvCanvas;
begin
 fState.fPath.CubicCurveTo(aC0,aC1,aA0);
 result:=self;
end;

function TpvCanvas.CubicCurveTo(const aC0X,aC0Y,aC1X,aC1Y,aAX,aAY:TpvFloat):TpvCanvas;
begin
 fState.fPath.CubicCurveTo(TpvVector2.Create(aC0X,aC0Y),TpvVector2.Create(aC1X,aC1Y),TpvVector2.Create(aAX,aAY));
 result:=self;
end;

function TpvCanvas.Stroke:TpvCanvas;
begin
 fShape.StrokeFromPath(fState.fPath,fState);
 result:=DrawShape(fShape);
end;

function TpvCanvas.Fill:TpvCanvas;
begin
 fShape.FillFromPath(fState.fPath,fState);
 result:=self;
end;

end.
