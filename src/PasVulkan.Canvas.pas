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

type TpvVulkanCanvasColor=class(TPersistent)
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

     TpvVulkanCanvasPenStyle=(vcpsClear,vcpsSolid);

     TpvVulkanCanvasPenLineJoin=(vcpljBevel,vcpljMiter,vcpljRound);

     TpvVulkanCanvasPenLineCap=(vcplcButt,vcplcSquare,vcplcRound);

     TpvVulkanCanvasPen=class(TPersistent)
      private
       fColor:TpvVulkanCanvasColor;
       fWidth:TpvFloat;
       fAntialiasingWidth:TpvFloat;
       fMiterLimit:TpvFloat;
       fStyle:TpvVulkanCanvasPenStyle;
       fLineJoin:TpvVulkanCanvasPenLineJoin;
       fLineCap:TpvVulkanCanvasPenLineCap;
      public
       constructor Create;
       destructor Destroy; override;
       procedure Assign(Source:TPersistent); override;
      published
       property Color:TpvVulkanCanvasColor read fColor;
       property Width:TpvFloat read fWidth write fWidth;
       property AntialiasingWidth:TpvFloat read fAntialiasingWidth write fAntialiasingWidth;
       property MiterLimit:TpvFloat read fMiterLimit write fMiterLimit;
       property Style:TpvVulkanCanvasPenStyle read fStyle write fStyle default vcpsSolid;
       property LineJoin:TpvVulkanCanvasPenLineJoin read fLineJoin write fLineJoin default vcpljRound;
       property LineCap:TpvVulkanCanvasPenLineCap read fLineCap write fLineCap default vcplcRound;
     end;

     PpvVulkanCanvasPoint=^TpvVulkanCanvasPoint;
     TpvVulkanCanvasPoint=TpvVector2;

     PpvVulkanCanvasInternalPoint=^TpvVulkanCanvasInternalPoint;
     TpvVulkanCanvasInternalPoint=record
      Position:TpvVector2;
      Middle:TpvVector2;
      Color:TpvVector4;
     end;

     TpvVulkanCanvasInternalPoints=array of TpvVulkanCanvasInternalPoint;

     TpvVulkanCanvasMode=(vcmNormal,vcmLine);

     PpvVulkanCanvasVertex=^TpvVulkanCanvasVertex;
     TpvVulkanCanvasVertex=packed record
      Position:TpvVulkanSpriteVertexPoint;             // +  8 (2x 32-bit floats)       = 0
      Color:TpvVulkanSpriteVertexColor;                // +  8 (4x 16-bit half-floats)  = 8  (=> 8 byte aligned)
      TextureCoord:TpvVulkanSpriteVertexTextureCoord;  // + 12 (3x 32-bit floats)       = 16 (=> 16 byte aligned)
      State:TpvVulkanSpriteVertexState;                // +  4 (2x 16-bit half-floats)  = 28 (=> 4 byte aligned)
      ClipRect:TpvVulkanSpriteVertexClipRect;          // + 16 (4x 32-bit floats)       = 32 (=> 32 byte aligned)
      MetaInfo:TpvVector4;                             // + 16 (4x 32-bit floats)       = 48 (=> 32 byte aligned)
     end;                                            // = 64 per vertex

     TpvVulkanCanvasVertices=array of TpvVulkanCanvasVertex;

     TpvVulkanCanvasVulkanBuffers=array of TpvVulkanBuffer;

     PpvVulkanCanvasVertexBuffer=^TpvVulkanCanvasVertexBuffer;
     TpvVulkanCanvasVertexBuffer=array[0..(32768*4)-1] of TpvVulkanCanvasVertex;

     TpvVulkanCanvasVertexBuffers=array of TpvVulkanCanvasVertexBuffer;

     TpvVulkanCanvasVertexBufferSizes=array of TVkSizeInt;

     PpvVulkanCanvasIndexBuffer=^TpvVulkanCanvasIndexBuffer;
     TpvVulkanCanvasIndexBuffer=array[0..((SizeOf(TpvVulkanCanvasVertexBuffer) div (SizeOf(TpvVulkanCanvasVertex)*4))*6)-1] of TpvUInt32;

     TpvVulkanCanvasIndexBuffers=array of TpvVulkanCanvasIndexBuffer;

     TpvVulkanCanvasIndexBufferSizes=array of TVkSizeInt;

     PpvVulkanCanvasRenderingMode=^TpvVulkanCanvasRenderingMode;
     TpvVulkanCanvasRenderingMode=
      (
       vsbrmNormal,
       vsbrmFont
      );

     PpvVulkanCanvasBlendingMode=^TpvVulkanCanvasBlendingMode;
     TpvVulkanCanvasBlendingMode=
      (
       vsbbmNone,
       vsbbmAlphaBlending,
       vsbbmAdditiveBlending
      );

     TpvVulkanCanvasHook=procedure(const aData:TpvPointer) of object;

     TpvVulkanCanvasQueueItemKind=
      (
       vcqikNone,
       vcqikNormal,
       vcqikVector,
       vcqikHook
      );

     PpvVulkanCanvasQueueItem=^TpvVulkanCanvasQueueItem;
     TpvVulkanCanvasQueueItem=record
      Kind:TpvVulkanCanvasQueueItemKind;
      BufferIndex:TpvInt32;
      DescriptorSetIndex:TpvInt32;
      StartVertexIndex:TpvInt32;
      StartIndexIndex:TpvInt32;
      CountVertices:TpvInt32;
      CountIndices:TpvInt32;
      RenderingMode:TpvVulkanCanvasRenderingMode;
      BlendingMode:TpvVulkanCanvasBlendingMode;
      Scissor:TVkRect2D;
      Hook:TpvVulkanCanvasHook;
      HookData:TVkPointer;
     end;

     TpvVulkanCanvasQueueItems=array of TpvVulkanCanvasQueueItem;

     TpvVulkanCanvasDescriptorSets=array of TpvVulkanDescriptorSet;

     PpvVulkanCanvasBuffer=^TpvVulkanCanvasBuffer;
     TpvVulkanCanvasBuffer=record
      fSpinLock:TpvInt32;
      fVulkanVertexBuffers:TpvVulkanCanvasVulkanBuffers;
      fVulkanIndexBuffers:TpvVulkanCanvasVulkanBuffers;
      fVertexBuffers:TpvVulkanCanvasVertexBuffers;
      fVertexBufferSizes:TpvVulkanCanvasVertexBufferSizes;
      fIndexBuffers:TpvVulkanCanvasIndexBuffers;
      fIndexBufferSizes:TpvVulkanCanvasIndexBufferSizes;
      fCountAllocatedBuffers:TpvInt32;
      fCountUsedBuffers:TpvInt32;
      fQueueItems:TpvVulkanCanvasQueueItems;
      fCountQueueItems:TpvInt32;
     end;

     TpvVulkanCanvasBuffers=array of TpvVulkanCanvasBuffer;

     TpvVulkanCanvasAtlasArrayTextureDescriptorSetHashMap=class(TpvHashMap<TpvVulkanSpriteAtlasArrayTexture,TpvInt32>);

     TpvVulkanCanvas=class
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
       fVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fVulkanDescriptorSets:TpvVulkanCanvasDescriptorSets;
       fCountVulkanDescriptorSets:TpvInt32;
       fVulkanTextureDescriptorSetHashMap:TpvVulkanCanvasAtlasArrayTextureDescriptorSetHashMap;
       fVulkanRenderPass:TpvVulkanRenderPass;
       fVulkanPipelineLayout:TpvVulkanPipelineLayout;
       fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
       fVulkanCanvasBuffers:TpvVulkanCanvasBuffers;
       fCountBuffers:TpvInt32;
       fCurrentFillBuffer:PpvVulkanCanvasBuffer;
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fViewPort:TVkViewport;
       fPointerToViewport:PVkViewport;
       fCurrentVulkanBufferIndex:TpvInt32;
       fCurrentVulkanVertexBufferOffset:TpvInt32;
       fCurrentVulkanIndexBufferOffset:TpvInt32;
       fState:TpvVulkanSpriteVertexState;
       fClipRect:TpvVulkanSpriteVertexClipRect;
       fUnscaledClipRect:TpvVulkanSpriteVertexClipRect;
       fRenderingMode:TpvVulkanCanvasRenderingMode;
       fBlendingMode:TpvVulkanCanvasBlendingMode;
       fLastArrayTexture:TpvVulkanSpriteAtlasArrayTexture;
       fInverseWidth:TpvFloat;
       fInverseHeight:TpvFloat;
       fInverseTextureWidth:TpvFloat;
       fInverseTextureHeight:TpvFloat;
       fCurrentCountVertices:TVkSizeInt;
       fCurrentCountIndices:TVkSizeInt;
       fCurrentDestinationVertexBufferPointer:PpvVulkanCanvasVertexBuffer;
       fCurrentDestinationIndexBufferPointer:PpvVulkanCanvasIndexBuffer;
       fScissor:TVkRect2D;
       fPen:TpvVulkanCanvasPen;
       function RotatePoint(const PointToRotate,AroundPoint:TpvVulkanSpritePoint;Cosinus,Sinus:TpvFloat):TpvVulkanSpritePoint;
       procedure SetArrayTexture(const ArrayTexture:TpvVulkanSpriteAtlasArrayTexture);
       procedure SetRenderingMode(aRenderingMode:TpvVulkanCanvasRenderingMode);
       procedure SetBlendingMode(aBlendingMode:TpvVulkanCanvasBlendingMode);
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
       procedure SetClipRect(const aLeft,aTop,aWidth,aHeight:TpvInt32); overload;
       procedure Hook(const aHook:TpvVulkanCanvasHook;const aData:TpvPointer); overload;
       procedure DrawSprite(const Sprite:TpvVulkanSprite;const Src,Dest:TpvVulkanSpriteRect;const Color:TpvVulkanSpriteColor); overload;
       procedure DrawSprite(const Sprite:TpvVulkanSprite;const Src,Dest:TpvVulkanSpriteRect;const Origin:TpvVulkanSpritePoint;Rotation:TpvFloat;const Color:TpvVulkanSpriteColor); overload;
       procedure DrawSprite(const Sprite:TpvVulkanSprite;const x,y:TpvFloat;const Color:TpvVulkanSpriteColor); overload;
       procedure DrawSprite(const Sprite:TpvVulkanSprite;const x,y:TpvFloat); overload;
       procedure DrawSprite(const Sprite:TpvVulkanSprite;const sx1,sy1,sx2,sy2,dx1,dy1,dx2,dy2,Alpha:TpvFloat); overload;
       procedure ExecuteUpload(const aVulkanCommandBuffer:TpvVulkanCommandBuffer;const aBufferIndex:TpvInt32);
       procedure ExecuteDraw(const aVulkanCommandBuffer:TpvVulkanCommandBuffer;const aBufferIndex:TpvInt32);
      public
       property Viewport:PVkViewport read fPointerToViewport;
      published
       property Device:TpvVulkanDevice read fDevice;
       property Width:TpvInt32 read fWidth write fWidth;
       property Height:TpvInt32 read fHeight write fHeight;
       property RenderingMode:TpvVulkanCanvasRenderingMode read fRenderingMode write SetRenderingMode;
       property BlendingMode:TpvVulkanCanvasBlendingMode read fBlendingMode write SetBlendingMode;
       property Pen:TpvVulkanCanvasPen read fPen;
     end;

implementation

uses PasVulkan.Assets;

constructor TpvVulkanCanvasColor.Create;
begin
 inherited Create;
 fRed:=1.0;
 fGreen:=1.0;
 fBlue:=1.0;
 fAlpha:=1.0;
end;

destructor TpvVulkanCanvasColor.Destroy;
begin
 inherited Destroy;
end;

procedure TpvVulkanCanvasColor.Assign(Source:TPersistent);
begin
 Assert(Source is TpvVulkanCanvasColor);
 fRed:=TpvVulkanCanvasColor(Source).fRed;
 fGreen:=TpvVulkanCanvasColor(Source).fGreen;
 fBlue:=TpvVulkanCanvasColor(Source).fBlue;
 fAlpha:=TpvVulkanCanvasColor(Source).fAlpha;
end;

constructor TpvVulkanCanvasPen.Create;
begin
 inherited Create;
 fColor:=TpvVulkanCanvasColor.Create;
 fWidth:=1.0;
 fAntialiasingWidth:=2.0;
 fMiterLimit:=3.0;
 fStyle:=vcpsSolid;
 fLineJoin:=vcpljRound;
 fLineCap:=vcplcRound;
end;

destructor TpvVulkanCanvasPen.Destroy;
begin
 FreeAndNil(fColor);
 inherited Destroy;
end;

procedure TpvVulkanCanvasPen.Assign(Source:TPersistent);
begin
 Assert(Source is TpvVulkanCanvasPen);
 fColor.Assign(TpvVulkanCanvasPen(Source).fColor);
 fWidth:=TpvVulkanCanvasPen(Source).fWidth;
 fAntialiasingWidth:=TpvVulkanCanvasPen(Source).fAntialiasingWidth;
 fMiterLimit:=TpvVulkanCanvasPen(Source).fMiterLimit;
 fStyle:=TpvVulkanCanvasPen(Source).fStyle;
 fLineJoin:=TpvVulkanCanvasPen(Source).fLineJoin;
 fLineCap:=TpvVulkanCanvasPen(Source).fLineCap;
end;

constructor TpvVulkanCanvas.Create(const aDevice:TpvVulkanDevice;
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
    RenderingModeIndex:TpvVulkanCanvasRenderingMode;
    BlendingModeIndex:TpvVulkanCanvasBlendingMode;
    VulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
    VulkanCanvasBuffer:PpvVulkanCanvasBuffer;
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

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(fDevice,
                                                     TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                     2);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,1);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,1);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 fVulkanDescriptorSets:=nil;
 fCountVulkanDescriptorSets:=0;

 fVulkanTextureDescriptorSetHashMap:=TpvVulkanCanvasAtlasArrayTextureDescriptorSetHashMap.Create(-1);

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(fDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 fVulkanRenderPass:=aRenderPass;

 Stream:=TpvVulkanDataStream.Create(@SpriteBatchVertexSPIRVData,SpriteBatchVertexSPIRVDataSize);
 try
  fSpriteBatchVertexShaderModule:=TpvVulkanShaderModule.Create(fDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=TpvVulkanDataStream.Create(@SpriteBatchFragmentSPIRVData,SpriteBatchFragmentSPIRVDataSize);
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

 VulkanGraphicsPipeline.VertexInputState.AddVertexInputBindingDescription(0,SizeOf(TpvVulkanCanvasVertex),VK_VERTEX_INPUT_RATE_VERTEX);
 VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(0,0,VK_FORMAT_R32G32_SFLOAT,TpvPtrUInt(TpvPointer(@PpvVulkanCanvasVertex(nil)^.Position)));
 VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(1,0,VK_FORMAT_R16G16B16A16_SFLOAT,TpvPtrUInt(TpvPointer(@PpvVulkanCanvasVertex(nil)^.Color)));
 VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(2,0,VK_FORMAT_R32G32B32_SFLOAT,TpvPtrUInt(TpvPointer(@PpvVulkanCanvasVertex(nil)^.TextureCoord)));
 VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(3,0,VK_FORMAT_R16G16_SFLOAT,TpvPtrUInt(TpvPointer(@PpvVulkanCanvasVertex(nil)^.State)));
 VulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(4,0,VK_FORMAT_R32G32B32A32_SFLOAT,TpvPtrUInt(TpvPointer(@PpvVulkanCanvasVertex(nil)^.ClipRect)));

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

 fPen:=TpvVulkanCanvasPen.Create;

end;

destructor TpvVulkanCanvas.Destroy;
var Index,SubIndex:TpvInt32;
    RenderingModeIndex:TpvVulkanCanvasRenderingMode;
    BlendingModeIndex:TpvVulkanCanvasBlendingMode;
    VulkanCanvasBuffer:PpvVulkanCanvasBuffer;
begin

 FreeAndNil(fPen);

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

//FreeAndNil(fVulkanRenderPass);

 for Index:=0 to fCountVulkanDescriptorSets-1 do begin
  FreeAndNil(fVulkanDescriptorSets[Index]);
 end;

 fVulkanDescriptorSets:=nil;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

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

function TpvVulkanCanvas.RotatePoint(const PointToRotate,AroundPoint:TpvVulkanSpritePoint;Cosinus,Sinus:TpvFloat):TpvVulkanSpritePoint;
var x,y:TpvFloat;
begin
 x:=PointToRotate.x-AroundPoint.x;
 y:=PointToRotate.y-AroundPoint.y;
 result.x:=(((((x*Cosinus)-(y*Sinus))+AroundPoint.x)*fInverseWidth)-0.5)*2;
 result.y:=(((((x*Sinus)+(y*Cosinus))+AroundPoint.y)*fInverseHeight)-0.5)*2;
end;

procedure TpvVulkanCanvas.SetRenderingMode(aRenderingMode:TpvVulkanCanvasRenderingMode);
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

procedure TpvVulkanCanvas.SetBlendingMode(aBlendingMode:TpvVulkanCanvasBlendingMode);
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

procedure TpvVulkanCanvas.Start(const aBufferIndex:TpvInt32);
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

 fCurrentVulkanBufferIndex:=-1;
 fCurrentVulkanVertexBufferOffset:=0;
 fCurrentVulkanIndexBufferOffset:=0;
 GetNextDestinationVertexBuffer;

 fState.BlendingMode:=1.0;
 fState.RenderingMode:=0.0;

 fClipRect.x0:=-1.0;
 fClipRect.y0:=-1.0;
 fClipRect.x1:=1.0;
 fClipRect.y1:=1.0;

 fUnscaledClipRect.x0:=0.0;
 fUnscaledClipRect.y0:=0.0;
 fUnscaledClipRect.x1:=fWidth;
 fUnscaledClipRect.y1:=fHeight;

end;

procedure TpvVulkanCanvas.Stop;
begin

 Flush;

 fCurrentFillBuffer:=nil;

end;

procedure TpvVulkanCanvas.Flush;
var CurrentVulkanBufferIndex,OldCount,NewCount,QueueItemIndex,DescriptorSetIndex:TpvInt32;
    QueueItem:PpvVulkanCanvasQueueItem;
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

   inc(fCurrentFillBuffer^.fVertexBufferSizes[CurrentVulkanBufferIndex],fCurrentCountVertices*SizeOf(TpvVulkanCanvasVertex));

   inc(fCurrentFillBuffer^.fIndexBufferSizes[CurrentVulkanBufferIndex],fCurrentCountIndices*SizeOf(TpvUInt32));

   if not fVulkanTextureDescriptorSetHashMap.TryGet(fLastArrayTexture,DescriptorSetIndex) then begin
    DescriptorSetIndex:=fCountVulkanDescriptorSets;
    inc(fCountVulkanDescriptorSets);
    if length(fVulkanDescriptorSets)<fCountVulkanDescriptorSets then begin
     SetLength(fVulkanDescriptorSets,fCountVulkanDescriptorSets*2);
    end;
    VulkanDescriptorSet:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                     fVulkanDescriptorSetLayout);
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
    fVulkanDescriptorSets[DescriptorSetIndex]:=VulkanDescriptorSet;
    fVulkanTextureDescriptorSetHashMap.Add(fLastArrayTexture,DescriptorSetIndex);
   end;

   QueueItemIndex:=fCurrentFillBuffer^.fCountQueueItems;
   inc(fCurrentFillBuffer^.fCountQueueItems);
   if length(fCurrentFillBuffer^.fQueueItems)<fCurrentFillBuffer^.fCountQueueItems then begin
    SetLength(fCurrentFillBuffer^.fQueueItems,fCurrentFillBuffer^.fCountQueueItems*2);
   end;
   QueueItem:=@fCurrentFillBuffer^.fQueueItems[QueueItemIndex];
   QueueItem^.Kind:=vcqikNormal;
   QueueItem^.BufferIndex:=CurrentVulkanBufferIndex;
   QueueItem^.DescriptorSetIndex:=DescriptorSetIndex;
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
  fCurrentDestinationIndexBufferPointer:=@fCurrentFillBuffer^.fVertexBuffers[fCurrentVulkanBufferIndex][fCurrentVulkanIndexBufferOffset];

 end;
end;

procedure TpvVulkanCanvas.GetNextDestinationVertexBuffer;
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

procedure TpvVulkanCanvas.FlushAndGetNewDestinationVertexBufferIfNeeded(const aCountVerticesToCheck,aCountIndicesToCheck:TpvInt32);
const UntilCountVertices=SizeOf(TpvVulkanCanvasVertexBuffer) div SizeOf(TpvVulkanCanvasVertex);
      UntilCountIndices=SizeOf(TpvVulkanCanvasIndexBuffer) div SizeOf(TpvUInt32);
begin
 if ((fCurrentVulkanVertexBufferOffset+fCurrentCountVertices+aCountVerticesToCheck)>=UntilCountVertices) or
    ((fCurrentVulkanIndexBufferOffset+fCurrentCountIndices+aCountIndicesToCheck)>=UntilCountIndices) then begin
  Flush;
  GetNextDestinationVertexBuffer;
 end;
end;

function TpvVulkanCanvas.ClipCheck(const aX0,aY0,aX1,aY1:TpvFloat):boolean;
const Threshold=1e-6;
begin
 result:=(fUnscaledClipRect.x0<=(aX1+Threshold)) and
         (aX0<=(fUnscaledClipRect.x1+Threshold)) and
         (fUnscaledClipRect.y0<=(aY1+Threshold)) and
         (aY0<=(fUnscaledClipRect.y1+Threshold));
end;

procedure TpvVulkanCanvas.SetArrayTexture(const ArrayTexture:TpvVulkanSpriteAtlasArrayTexture);
begin
 if fLastArrayTexture<>ArrayTexture then begin
  Flush;
  fLastArrayTexture:=ArrayTexture;
  fInverseTextureWidth:=1.0/ArrayTexture.Width;
  fInverseTextureHeight:=1.0/ArrayTexture.Height;
 end;
end;

procedure TpvVulkanCanvas.SetScissor(const aScissor:TVkRect2D);
begin
 if (fScissor.offset.x<>aScissor.offset.x) or
    (fScissor.offset.y<>aScissor.offset.y) or
    (fScissor.extent.Width<>aScissor.extent.Width) or
    (fScissor.extent.Height<>aScissor.extent.Height) then begin
  Flush;
  fScissor:=aScissor;
 end;
end;

procedure TpvVulkanCanvas.SetScissor(const aLeft,aTop,aWidth,aHeight:TpvInt32);
var NewScissor:TVkRect2D;
begin
 NewScissor.offset.x:=aLeft;
 NewScissor.offset.y:=aTop;
 NewScissor.extent.Width:=aWidth;
 NewScissor.extent.Height:=aHeight;
 SetScissor(NewScissor);
end;

procedure TpvVulkanCanvas.SetClipRect(const aClipRect:TVkRect2D);
begin
 fUnscaledClipRect.x0:=aClipRect.offset.x;
 fUnscaledClipRect.y0:=aClipRect.offset.y;
 fUnscaledClipRect.x1:=aClipRect.offset.x+(aClipRect.extent.width+0.0);
 fUnscaledClipRect.y1:=aClipRect.offset.y+(aClipRect.extent.height+0.0);
 fClipRect.x0:=((aClipRect.offset.x*fInverseWidth)-0.5)*2.0;
 fClipRect.y0:=((aClipRect.offset.y*fInverseHeight)-0.5)*2.0;
 fClipRect.x1:=(((aClipRect.offset.x+(aClipRect.extent.width+0.0))*fInverseWidth)-0.5)*2.0;
 fClipRect.y1:=(((aClipRect.offset.y+(aClipRect.extent.height+0.0))*fInverseHeight)-0.5)*2.0;
end;

procedure TpvVulkanCanvas.SetClipRect(const aLeft,aTop,aWidth,aHeight:TpvInt32);
begin
 fUnscaledClipRect.x0:=aLeft;
 fUnscaledClipRect.y0:=aTop;
 fUnscaledClipRect.x1:=aLeft+aWidth;
 fUnscaledClipRect.y1:=aTop+aHeight;
 fClipRect.x0:=((aLeft*fInverseWidth)-0.5)*2.0;
 fClipRect.y0:=((aTop*fInverseHeight)-0.5)*2.0;
 fClipRect.x1:=(((aLeft+aWidth)*fInverseWidth)-0.5)*2.0;
 fClipRect.y1:=(((aTop+aHeight)*fInverseHeight)-0.5)*2.0;
end;

procedure TpvVulkanCanvas.Hook(const aHook:TpvVulkanCanvasHook;const aData:TpvPointer);
var QueueItemIndex:TpvInt32;
    QueueItem:PpvVulkanCanvasQueueItem;
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

procedure TpvVulkanCanvas.DrawSprite(const Sprite:TpvVulkanSprite;const Src,Dest:TpvVulkanSpriteRect;const Color:TpvVulkanSpriteColor);
const MinA=1.0/1024.0;
var tx1,ty1,tx2,ty2,xf,yf,sX0,sY0,sX1,sY1:TpvFloat;
    TempDest,TempSrc:TpvVulkanSpriteRect;
    VertexColor:TpvVulkanSpriteVertexColor;
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
   if TempDest.Left<fUnscaledClipRect.x0 then begin
    TempSrc.Left:=TempSrc.Left+((TempSrc.Right-TempSrc.Left)*((fUnscaledClipRect.x0-TempDest.Left)/(TempDest.Right-TempDest.Left)));
    TempDest.Left:=fUnscaledClipRect.x0;
   end;
   if TempDest.Top<fUnscaledClipRect.y0 then begin
    TempSrc.Top:=TempSrc.Top+((TempSrc.Bottom-TempSrc.Top)*((fUnscaledClipRect.y0-TempDest.Top)/(TempDest.Bottom-TempDest.Top)));
    TempDest.Top:=fUnscaledClipRect.y0;
   end;
   if TempDest.Right>fUnscaledClipRect.x1 then begin
    TempSrc.Right:=TempSrc.Left+((TempSrc.Right-TempSrc.Left)*((fUnscaledClipRect.x1-TempDest.Left)/(TempDest.Right-TempDest.Left)));
    TempDest.Right:=fUnscaledClipRect.x1;
   end;
   if TempDest.Bottom>fUnscaledClipRect.y1 then begin
    TempSrc.Bottom:=TempSrc.Top+((TempSrc.Bottom-TempSrc.Top)*((fUnscaledClipRect.y1-TempDest.Top)/(TempDest.Bottom-TempDest.Top)));
    TempDest.Bottom:=fUnscaledClipRect.y1;
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

procedure TpvVulkanCanvas.DrawSprite(const Sprite:TpvVulkanSprite;const Src,Dest:TpvVulkanSpriteRect;const Origin:TpvVulkanSpritePoint;Rotation:TpvFloat;const Color:TpvVulkanSpriteColor);
const MinA=1.0/1024.0;
var Cosinus,Sinus,tx1,ty1,tx2,ty2,xf,yf,sX0,sY0,sX1,sY1:TpvFloat;
    AroundPoint:TpvVulkanSpritePoint;
    Points:array[0..3] of TpvVulkanSpritePoint;
    TempDest,TempSrc:TpvVulkanSpriteRect;
    VertexColor:TpvVulkanSpriteVertexColor;
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
    if TempDest.Left<fUnscaledClipRect.x0 then begin
     TempSrc.Left:=TempSrc.Left+((TempSrc.Right-TempSrc.Left)*((fUnscaledClipRect.x0-TempDest.Left)/(TempDest.Right-TempDest.Left)));
     TempDest.Left:=fUnscaledClipRect.x0;
    end;
    if TempDest.Top<fUnscaledClipRect.y0 then begin
     TempSrc.Top:=TempSrc.Top+((TempSrc.Bottom-TempSrc.Top)*((fUnscaledClipRect.y0-TempDest.Top)/(TempDest.Bottom-TempDest.Top)));
     TempDest.Top:=fUnscaledClipRect.y0;
    end;
    if TempDest.Right>fUnscaledClipRect.x1 then begin
     TempSrc.Right:=TempSrc.Left+((TempSrc.Right-TempSrc.Left)*((fUnscaledClipRect.x1-TempDest.Left)/(TempDest.Right-TempDest.Left)));
     TempDest.Right:=fUnscaledClipRect.x1;
    end;
    if TempDest.Bottom>fUnscaledClipRect.y1 then begin
     TempSrc.Bottom:=TempSrc.Top+((TempSrc.Bottom-TempSrc.Top)*((fUnscaledClipRect.y1-TempDest.Top)/(TempDest.Bottom-TempDest.Top)));
     TempDest.Bottom:=fUnscaledClipRect.y1;
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

procedure TpvVulkanCanvas.DrawSprite(const Sprite:TpvVulkanSprite;const x,y:TpvFloat;const Color:TpvVulkanSpriteColor);
var Src,Dest:TpvVulkanSpriteRect;
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

procedure TpvVulkanCanvas.DrawSprite(const Sprite:TpvVulkanSprite;const x,y:TpvFloat);
var Color:TpvVulkanSpriteColor;
begin
 Color.r:=1;
 Color.g:=1;
 Color.b:=1;
 Color.a:=1;
 DrawSprite(Sprite,x,y,Color);
end;

procedure TpvVulkanCanvas.DrawSprite(const Sprite:TpvVulkanSprite;const sx1,sy1,sx2,sy2,dx1,dy1,dx2,dy2,Alpha:TpvFloat);
var Src,Dest:TpvVulkanSpriteRect;
    Color:TpvVulkanSpriteColor;
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

procedure TpvVulkanCanvas.ExecuteUpload(const aVulkanCommandBuffer:TpvVulkanCommandBuffer;const aBufferIndex:TpvInt32);
var Index:TpvInt32;
    CurrentDrawSpriteBatchBuffer:PpvVulkanCanvasBuffer;
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
                                         SizeOf(TpvVulkanCanvasVertexBuffer),
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
                                         SizeOf(TpvVulkanCanvasIndexBuffer),
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

procedure TpvVulkanCanvas.ExecuteDraw(const aVulkanCommandBuffer:TpvVulkanCommandBuffer;const aBufferIndex:TpvInt32);
var Index,DescriptorSetIndex,StartVertexIndex:TpvInt32;
    QueueItem:PpvVulkanCanvasQueueItem;
    OldQueueItemKind:TpvVulkanCanvasQueueItemKind;
    CurrentDrawSpriteBatchBuffer:PpvVulkanCanvasBuffer;
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

  DescriptorSetIndex:=-1;

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
        (DescriptorSetIndex<>QueueItem^.DescriptorSetIndex) then begin
      DescriptorSetIndex:=QueueItem^.DescriptorSetIndex;
      aVulkanCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanPipelineLayout.Handle,0,1,@fVulkanDescriptorSets[DescriptorSetIndex].Handle,0,nil);
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

     Offsets[0]:=QueueItem^.StartVertexIndex*SizeOf(TpvVulkanCanvasVertex);
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
