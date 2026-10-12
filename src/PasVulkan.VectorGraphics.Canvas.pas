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
unit PasVulkan.VectorGraphics.Canvas;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

{$define CANINLINE}

// Brings a vector graphics document onto a TpvCanvas.
//
// For now this goes through a texture: the document is rasterized by the software rasterizer and
// the result becomes a sprite, which the canvas then draws like any other. That is the one route
// where everything a document can hold really arrives, filters, blend modes, gradients with as
// many stops as they like, patterns, masks and the timeline included, because it is the same
// renderer the reference comparison is run against.
//
// The price is a fixed resolution: zooming in means rasterizing again at the new size. For
// artwork that is drawn at a known size, which is what an icon or a user interface is, that costs
// nothing. The resolution independent route over the vector path shaders of the canvas is the
// next step and will reuse the recorder backend rather than this unit.

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Collections,
     PasVulkan.Framework,
     PasVulkan.Sprites,
     PasVulkan.Canvas,
     PasVulkan.VectorPath,
     PasVulkan.VectorGraphics.Rasterizer,
     PasVulkan.VectorGraphics.Rasterizer.Analytical,
     PasVulkan.VectorGraphics,
     PasVulkan.VectorGraphics.SVG;

type EpvVectorGraphicsCanvas=class(Exception);

     { TpvVectorGraphicsCanvasRasterizer }

     // Keeps a rasterizer and its pixel buffer between runs, so that rendering the same document
     // again, at another point in time or after something changed, costs no allocation at all.
     // The pixels come out in the order the sprite atlas and the Vulkan textures want them.
     TpvVectorGraphicsCanvasRasterizer=class
      private
       fRasterizer:TpvVectorGraphicsRasterizerAnalytical;
       fPixelBuffer:TpvVectorGraphicsPixelBuffer;
       // The same picture once more with red and blue the other way round, which is what the
       // atlas expects and what the rasterizer does not store in.
       fConvertedPixels:array of TpvVectorGraphicsColor;
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       function GetPixelPointer:TpvPointer;
      public
       constructor Create(const aWidth,aHeight:TpvInt32;
                          const aColorSpaceMode:TpvVectorGraphicsColorSpaceMode=TpvVectorGraphicsColorSpaceMode.Integer); reintroduce;
       destructor Destroy; override;
       procedure Resize(const aWidth,aHeight:TpvInt32);
       // Draws the whole document, fitted into the buffer the way its preserveAspectRatio asks
       // for. aClearColor is what the buffer starts out as, transparent by default.
       procedure Render(const aDocument:TpvVectorGraphicsDocument;
                        const aTick:TpvInt64=0;
                        const aTickFraction:TpvDouble=0.0;
                        const aClearColor:TpvVectorGraphicsColor=0);
       // Draws one object out of the document on its own, which is what a user interface wants
       // when it takes a single icon out of a sheet. Returns false when no object carries that
       // name.
       function RenderObjectByID(const aDocument:TpvVectorGraphicsDocument;
                                 const aID:TpvUTF8String;
                                 const aTick:TpvInt64=0;
                                 const aTickFraction:TpvDouble=0.0;
                                 const aClearColor:TpvVectorGraphicsColor=0):boolean;
       // Hands what was drawn to a sprite atlas, which uploads it and hands back the sprite.
       function ToSprite(const aSpriteAtlas:TpvSpriteAtlas;const aName:TpvRawByteString;
                         const aAutomaticTrim:boolean=true;
                         const aPadding:TpvInt32=2):TpvSprite;
      public
       property Rasterizer:TpvVectorGraphicsRasterizerAnalytical read fRasterizer;
       property PixelBuffer:TpvVectorGraphicsPixelBuffer read fPixelBuffer;
       // The pixels as R8G8B8A8, which is what the sprite atlas and a Vulkan texture take.
       property PixelPointer:TpvPointer read GetPixelPointer;
      published
       property Width:TpvInt32 read fWidth;
       property Height:TpvInt32 read fHeight;
     end;

     { TpvVectorGraphicsCanvasTexture }

     // A document together with a texture of its own, which is the way to go whenever the picture
     // changes: a timeline that runs, a recolored icon, a document being edited.
     //
     // A sprite would not do here, because a sprite atlas never gives a place back, so handing it
     // a new picture every frame would fill it up. The texture is created as a streaming one, so
     // that every further upload reuses the same staging buffer.
     TpvVectorGraphicsCanvasTexture=class
      private
       fDevice:TpvVulkanDevice;
       fDocument:TpvVectorGraphicsDocument;
       fOwnsDocument:boolean;
       fRasterizer:TpvVectorGraphicsCanvasRasterizer;
       fTexture:TpvVulkanTexture;
       fSRGB:boolean;
       fTick:TpvInt64;
       fTickFraction:TpvDouble;
       fDirty:boolean;
       procedure SetTick(const aTick:TpvInt64);
       procedure SetTickFraction(const aTickFraction:TpvDouble);
      public
       // aWidth and aHeight of zero take the size the document itself states. The document is
       // taken over unless aOwnsDocument says otherwise. Nothing on the Vulkan side happens here,
       // because that needs the transfer objects, which only Update gets.
       constructor Create(const aDevice:TpvVulkanDevice;
                          const aDocument:TpvVectorGraphicsDocument;
                          const aWidth:TpvInt32=0;
                          const aHeight:TpvInt32=0;
                          const aOwnsDocument:boolean=true;
                          const aSRGB:boolean=false); reintroduce;
       destructor Destroy; override;
       // Another size for the picture, which marks it as needing a new rasterization. The texture
       // is thrown away, because its size is fixed once it exists.
       procedure Resize(const aWidth,aHeight:TpvInt32);
       // Rasterizes again when anything changed and uploads the result. The texture is created on
       // the first call. Doing nothing when nothing changed is the whole point, so calling this
       // every frame is fine.
       procedure Update(const aGraphicsQueue:TpvVulkanQueue;
                        const aGraphicsCommandBuffer:TpvVulkanCommandBuffer;
                        const aGraphicsFence:TpvVulkanFence;
                        const aTransferQueue:TpvVulkanQueue;
                        const aTransferCommandBuffer:TpvVulkanCommandBuffer;
                        const aTransferFence:TpvVulkanFence);
       // Draws what was last uploaded. Update has to have run at least once, which is why it is
       // not called from here: it needs the transfer objects and a draw does not have them.
       procedure Draw(const aCanvas:TpvCanvas;const aDestination:TpvRect); overload;
       procedure Draw(const aCanvas:TpvCanvas;const aPosition:TpvVector2); overload;
      public
       property Document:TpvVectorGraphicsDocument read fDocument;
       property Texture:TpvVulkanTexture read fTexture;
       property Rasterizer:TpvVectorGraphicsCanvasRasterizer read fRasterizer;
      published
       // Moving the playhead only marks the picture as needing a new rasterization, it does not
       // run one, so that several changes before the next Update cost only one.
       property Tick:TpvInt64 read fTick write SetTick;
       property TickFraction:TpvDouble read fTickFraction write SetTickFraction;
       // Set this after changing the document itself, which nothing here can notice on its own.
       property Dirty:boolean read fDirty write fDirty;
     end;

// The short way for the common case: an SVG from a file, a stream or a string, straight into a
// sprite of the given atlas. A size of zero takes the one the document states.
function pvVectorGraphicsCreateSpriteFromDocument(const aSpriteAtlas:TpvSpriteAtlas;
                                                  const aName:TpvRawByteString;
                                                  const aDocument:TpvVectorGraphicsDocument;
                                                  const aWidth:TpvInt32=0;
                                                  const aHeight:TpvInt32=0;
                                                  const aTick:TpvInt64=0;
                                                  const aAutomaticTrim:boolean=true):TpvSprite;

function pvVectorGraphicsCreateSpriteFromSVGFile(const aSpriteAtlas:TpvSpriteAtlas;
                                                 const aName:TpvRawByteString;
                                                 const aFileName:string;
                                                 const aWidth:TpvInt32=0;
                                                 const aHeight:TpvInt32=0;
                                                 const aTick:TpvInt64=0;
                                                 const aAutomaticTrim:boolean=true):TpvSprite;

function pvVectorGraphicsCreateSpriteFromSVGStream(const aSpriteAtlas:TpvSpriteAtlas;
                                                   const aName:TpvRawByteString;
                                                   const aStream:TStream;
                                                   const aWidth:TpvInt32=0;
                                                   const aHeight:TpvInt32=0;
                                                   const aTick:TpvInt64=0;
                                                   const aAutomaticTrim:boolean=true):TpvSprite;

function pvVectorGraphicsCreateSpriteFromSVGString(const aSpriteAtlas:TpvSpriteAtlas;
                                                   const aName:TpvRawByteString;
                                                   const aText:TpvRawByteString;
                                                   const aWidth:TpvInt32=0;
                                                   const aHeight:TpvInt32=0;
                                                   const aTick:TpvInt64=0;
                                                   const aAutomaticTrim:boolean=true):TpvSprite;

implementation

{ TpvVectorGraphicsCanvasRasterizer }

constructor TpvVectorGraphicsCanvasRasterizer.Create(const aWidth,aHeight:TpvInt32;
                                                     const aColorSpaceMode:TpvVectorGraphicsColorSpaceMode);
begin

 inherited Create;

 fWidth:=Max(1,aWidth);
 fHeight:=Max(1,aHeight);

 fPixelBuffer:=TpvVectorGraphicsPixelBuffer.Create(fWidth,fHeight,aColorSpaceMode);
 fRasterizer:=TpvVectorGraphicsRasterizerAnalytical.Create(fPixelBuffer);

 fConvertedPixels:=nil;

end;

destructor TpvVectorGraphicsCanvasRasterizer.Destroy;
begin

 // The rasterizer only borrows the buffer, so it goes first and the buffer afterwards.
 FreeAndNil(fRasterizer);
 FreeAndNil(fPixelBuffer);

 fConvertedPixels:=nil;

 inherited Destroy;

end;

procedure TpvVectorGraphicsCanvasRasterizer.Resize(const aWidth,aHeight:TpvInt32);
begin

 if (fWidth<>Max(1,aWidth)) or (fHeight<>Max(1,aHeight)) then begin

  fWidth:=Max(1,aWidth);
  fHeight:=Max(1,aHeight);

  // Going through the rasterizer rather than the buffer, because the scanline bookkeeping of the
  // rasterizer is sized to the old one as well.
  fRasterizer.Resize(fWidth,fHeight);

  fConvertedPixels:=nil;

 end;

end;

function TpvVectorGraphicsCanvasRasterizer.GetPixelPointer:TpvPointer;
var Count:TpvSizeInt;
begin

 Count:=TpvSizeInt(fWidth)*TpvSizeInt(fHeight);
 if Count<=0 then begin
  result:=nil;
  exit;
 end;

 // The rasterizer stores blue in the lowest byte, which maps onto a B8G8R8A8 texture without a
 // swizzle, while the sprite atlas works in R8G8B8A8, so the two have to change places here.
 if Length(fConvertedPixels)<Count then begin
  SetLength(fConvertedPixels,Count);
 end;

 fPixelBuffer.ConvertToRGBA8(@fConvertedPixels[0]);

 result:=@fConvertedPixels[0];

end;

procedure TpvVectorGraphicsCanvasRasterizer.Render(const aDocument:TpvVectorGraphicsDocument;
                                                   const aTick:TpvInt64;
                                                   const aTickFraction:TpvDouble;
                                                   const aClearColor:TpvVectorGraphicsColor);
begin

 if not assigned(aDocument) then begin
  exit;
 end;

 fPixelBuffer.Clear(aClearColor);

 aDocument.Update;
 aDocument.Render(fRasterizer,
                  aDocument.GetViewBoxMatrix(fWidth,fHeight),
                  aTick,
                  aTickFraction);

end;

function TpvVectorGraphicsCanvasRasterizer.RenderObjectByID(const aDocument:TpvVectorGraphicsDocument;
                                                            const aID:TpvUTF8String;
                                                            const aTick:TpvInt64;
                                                            const aTickFraction:TpvDouble;
                                                            const aClearColor:TpvVectorGraphicsColor):boolean;
begin

 result:=false;

 if not assigned(aDocument) then begin
  exit;
 end;

 fPixelBuffer.Clear(aClearColor);

 aDocument.Update;
 result:=aDocument.RenderObjectByID(aID,
                                    fRasterizer,
                                    aDocument.GetViewBoxMatrix(fWidth,fHeight),
                                    TpvVectorGraphicsRenderOverride.CreateNone,
                                    aTick,
                                    aTickFraction);

end;

function TpvVectorGraphicsCanvasRasterizer.ToSprite(const aSpriteAtlas:TpvSpriteAtlas;const aName:TpvRawByteString;
                                                    const aAutomaticTrim:boolean;
                                                    const aPadding:TpvInt32):TpvSprite;
var Pixels:TpvPointer;
begin

 result:=nil;

 if not assigned(aSpriteAtlas) then begin
  exit;
 end;

 Pixels:=GetPixelPointer;
 if not assigned(Pixels) then begin
  exit;
 end;

 // The rasterizer works in straight alpha, and the atlas converts to whatever it keeps itself
 // once it is told which of the two it is being handed.
 result:=aSpriteAtlas.LoadRawSprite(aName,Pixels,fWidth,fHeight,aAutomaticTrim,aPadding,1,
                                    false,nil,false,false);

end;

{ TpvVectorGraphicsCanvasTexture }

constructor TpvVectorGraphicsCanvasTexture.Create(const aDevice:TpvVulkanDevice;
                                                  const aDocument:TpvVectorGraphicsDocument;
                                                  const aWidth:TpvInt32;
                                                  const aHeight:TpvInt32;
                                                  const aOwnsDocument:boolean;
                                                  const aSRGB:boolean);
var Width_,Height_:TpvInt32;
begin

 inherited Create;

 if not (assigned(aDevice) and assigned(aDocument)) then begin
  raise EpvVectorGraphicsCanvas.Create('A canvas texture needs both a device and a document');
 end;

 fDevice:=aDevice;
 fDocument:=aDocument;
 fOwnsDocument:=aOwnsDocument;
 fSRGB:=aSRGB;

 fTexture:=nil;

 fTick:=0;
 fTickFraction:=0.0;

 // A size of zero means whatever the document states, rounded up so that nothing is cut off.
 if aWidth>0 then begin
  Width_:=aWidth;
 end else begin
  Width_:=Max(1,trunc(Ceil(aDocument.Width)));
 end;
 if aHeight>0 then begin
  Height_:=aHeight;
 end else begin
  Height_:=Max(1,trunc(Ceil(aDocument.Height)));
 end;

 fRasterizer:=TpvVectorGraphicsCanvasRasterizer.Create(Width_,Height_);

 // Nothing is drawn yet, the first Update does that.
 fDirty:=true;

end;

destructor TpvVectorGraphicsCanvasTexture.Destroy;
begin

 FreeAndNil(fTexture);
 FreeAndNil(fRasterizer);

 if fOwnsDocument then begin
  FreeAndNil(fDocument);
 end else begin
  fDocument:=nil;
 end;

 inherited Destroy;

end;

procedure TpvVectorGraphicsCanvasTexture.SetTick(const aTick:TpvInt64);
begin
 if fTick<>aTick then begin
  fTick:=aTick;
  fDirty:=true;
 end;
end;

procedure TpvVectorGraphicsCanvasTexture.SetTickFraction(const aTickFraction:TpvDouble);
begin
 if not SameValue(fTickFraction,aTickFraction) then begin
  fTickFraction:=aTickFraction;
  fDirty:=true;
 end;
end;

procedure TpvVectorGraphicsCanvasTexture.Resize(const aWidth,aHeight:TpvInt32);
begin

 if (fRasterizer.Width<>Max(1,aWidth)) or (fRasterizer.Height<>Max(1,aHeight)) then begin

  fRasterizer.Resize(aWidth,aHeight);

  // The size of a texture is fixed once it exists, so the next Update makes a new one.
  FreeAndNil(fTexture);
  fDirty:=true;

 end;

end;

procedure TpvVectorGraphicsCanvasTexture.Update(const aGraphicsQueue:TpvVulkanQueue;
                                                const aGraphicsCommandBuffer:TpvVulkanCommandBuffer;
                                                const aGraphicsFence:TpvVulkanFence;
                                                const aTransferQueue:TpvVulkanQueue;
                                                const aTransferCommandBuffer:TpvVulkanCommandBuffer;
                                                const aTransferFence:TpvVulkanFence);
var Pixels:TpvPointer;
    DataSize:TVkSizeInt;
    Format:TVkFormat;
begin

 if (not fDirty) and assigned(fTexture) then begin
  exit;
 end;

 fRasterizer.Render(fDocument,fTick,fTickFraction);

 Pixels:=fRasterizer.PixelPointer;
 if not assigned(Pixels) then begin
  exit;
 end;

 DataSize:=TVkSizeInt(fRasterizer.Width)*TVkSizeInt(fRasterizer.Height)*SizeOf(TpvVectorGraphicsColor);

 if fSRGB then begin
  Format:=VK_FORMAT_R8G8B8A8_SRGB;
 end else begin
  Format:=VK_FORMAT_R8G8B8A8_UNORM;
 end;

 if assigned(fTexture) then begin

  // A streaming texture keeps its staging buffer, so every further upload is only the copy.
  fTexture.Upload(aGraphicsQueue,aGraphicsCommandBuffer,aGraphicsFence,
                  aTransferQueue,aTransferCommandBuffer,aTransferFence,
                  Pixels,DataSize,false,false,1,true);

 end else begin

  fTexture:=TpvVulkanTexture.CreateFromMemory(fDevice,
                                              aGraphicsQueue,aGraphicsCommandBuffer,aGraphicsFence,
                                              aTransferQueue,aTransferCommandBuffer,aTransferFence,
                                              Format,
                                              VK_SAMPLE_COUNT_1_BIT,
                                              fRasterizer.Width,
                                              fRasterizer.Height,
                                              0,
                                              0,
                                              1,
                                              1,
                                              [TpvVulkanTextureUsageFlag.TransferDst,
                                               TpvVulkanTextureUsageFlag.Sampled],
                                              Pixels,
                                              DataSize,
                                              false,
                                              false,
                                              1,
                                              true,
                                              false,
                                              true);

  // Clamped to a transparent border, so that drawing the picture scaled does not smear its edge
  // pixels outwards, the same way the sprite atlas does it.
  fTexture.WrapModeU:=TpvVulkanTextureWrapMode.ClampToBorder;
  fTexture.WrapModeV:=TpvVulkanTextureWrapMode.ClampToBorder;
  fTexture.WrapModeW:=TpvVulkanTextureWrapMode.ClampToBorder;
  fTexture.BorderColor:=VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK;
  fTexture.UpdateSampler;

 end;

 fDirty:=false;

end;

procedure TpvVectorGraphicsCanvasTexture.Draw(const aCanvas:TpvCanvas;const aDestination:TpvRect);
begin
 if assigned(fTexture) then begin
  aCanvas.DrawTexturedRectangle(fTexture,aDestination);
 end;
end;

procedure TpvVectorGraphicsCanvasTexture.Draw(const aCanvas:TpvCanvas;const aPosition:TpvVector2);
begin
 if assigned(fTexture) then begin
  aCanvas.DrawTexturedRectangle(fTexture,
                                TpvRect.CreateRelative(aPosition.x,aPosition.y,
                                                       fRasterizer.Width,fRasterizer.Height));
 end;
end;

function pvVectorGraphicsCreateSpriteFromDocument(const aSpriteAtlas:TpvSpriteAtlas;
                                                  const aName:TpvRawByteString;
                                                  const aDocument:TpvVectorGraphicsDocument;
                                                  const aWidth:TpvInt32;
                                                  const aHeight:TpvInt32;
                                                  const aTick:TpvInt64;
                                                  const aAutomaticTrim:boolean):TpvSprite;
var Rasterizer:TpvVectorGraphicsCanvasRasterizer;
    Width_,Height_:TpvInt32;
begin

 result:=nil;

 if not (assigned(aSpriteAtlas) and assigned(aDocument)) then begin
  exit;
 end;

 if aWidth>0 then begin
  Width_:=aWidth;
 end else begin
  Width_:=Max(1,trunc(Ceil(aDocument.Width)));
 end;
 if aHeight>0 then begin
  Height_:=aHeight;
 end else begin
  Height_:=Max(1,trunc(Ceil(aDocument.Height)));
 end;

 Rasterizer:=TpvVectorGraphicsCanvasRasterizer.Create(Width_,Height_);
 try
  Rasterizer.Render(aDocument,aTick);
  result:=Rasterizer.ToSprite(aSpriteAtlas,aName,aAutomaticTrim);
 finally
  FreeAndNil(Rasterizer);
 end;

end;

function pvVectorGraphicsCreateSpriteFromSVGFile(const aSpriteAtlas:TpvSpriteAtlas;
                                                 const aName:TpvRawByteString;
                                                 const aFileName:string;
                                                 const aWidth:TpvInt32;
                                                 const aHeight:TpvInt32;
                                                 const aTick:TpvInt64;
                                                 const aAutomaticTrim:boolean):TpvSprite;
var Document:TpvVectorGraphicsDocument;
begin

 result:=nil;

 Document:=pvVectorGraphicsLoadSVGFromFile(aFileName);
 if not assigned(Document) then begin
  exit;
 end;

 try
  result:=pvVectorGraphicsCreateSpriteFromDocument(aSpriteAtlas,aName,Document,
                                                   aWidth,aHeight,aTick,aAutomaticTrim);
 finally
  FreeAndNil(Document);
 end;

end;

function pvVectorGraphicsCreateSpriteFromSVGStream(const aSpriteAtlas:TpvSpriteAtlas;
                                                   const aName:TpvRawByteString;
                                                   const aStream:TStream;
                                                   const aWidth:TpvInt32;
                                                   const aHeight:TpvInt32;
                                                   const aTick:TpvInt64;
                                                   const aAutomaticTrim:boolean):TpvSprite;
var Document:TpvVectorGraphicsDocument;
begin

 result:=nil;

 Document:=pvVectorGraphicsLoadSVGFromStream(aStream);
 if not assigned(Document) then begin
  exit;
 end;

 try
  result:=pvVectorGraphicsCreateSpriteFromDocument(aSpriteAtlas,aName,Document,
                                                   aWidth,aHeight,aTick,aAutomaticTrim);
 finally
  FreeAndNil(Document);
 end;

end;

function pvVectorGraphicsCreateSpriteFromSVGString(const aSpriteAtlas:TpvSpriteAtlas;
                                                   const aName:TpvRawByteString;
                                                   const aText:TpvRawByteString;
                                                   const aWidth:TpvInt32;
                                                   const aHeight:TpvInt32;
                                                   const aTick:TpvInt64;
                                                   const aAutomaticTrim:boolean):TpvSprite;
var Document:TpvVectorGraphicsDocument;
begin

 result:=nil;

 Document:=pvVectorGraphicsLoadSVGFromString(aText);
 if not assigned(Document) then begin
  exit;
 end;

 try
  result:=pvVectorGraphicsCreateSpriteFromDocument(aSpriteAtlas,aName,Document,
                                                   aWidth,aHeight,aTick,aAutomaticTrim);
 finally
  FreeAndNil(Document);
 end;

end;

end.
