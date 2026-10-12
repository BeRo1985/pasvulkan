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

// Brings a vector graphics document onto a TpvCanvas, by either of two routes.
//
// The first goes through a texture: the document is rasterized by the software rasterizer and the
// result becomes a sprite or a texture, which the canvas then draws like any other. That is the
// one route where everything a document can hold really arrives, filters, blend modes, patterns,
// masks and the timeline included, because it is the same renderer the reference comparison is
// run against. The price is a fixed resolution: zooming in means rasterizing again at the new
// size. For artwork drawn at a known size, which is what an icon or a user interface is, that
// costs nothing.
//
// The second hands the outlines to the vector path shader of the canvas, which resolves them per
// pixel, so the same set of shapes stays sharp at any zoom. Plain colors and the two kinds of SVG
// gradient arrive that way; a pattern, an image fill, a mask, a blend mode or a filter does not,
// and for a document that uses one of those the second route says so and the first takes over.

interface

uses SysUtils,
     Classes,
     Math,
     PasMP,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Collections,
     PasVulkan.Framework,
     PasVulkan.TimerQuery,
     PasVulkan.Sprites,
     PasVulkan.Canvas,
     PasVulkan.VectorPath,
     PasVulkan.VectorGraphics.Rasterizer,
     PasVulkan.VectorGraphics.Paints,
     PasVulkan.VectorGraphics.Rasterizer.Analytical,
     PasVulkan.VectorGraphics.Rasterizer.Recorder,
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
       // Whether the colour is multiplied by its own alpha before the upload, which is what makes
       // the filtering of the hardware right at every edge, in every mip level and at the border.
       // On by default, and only worth turning off where something reads the texture back and wants
       // the colours as they were drawn.
       fPremultipliedAlpha:boolean;
       // Whether the colour of the picture is carried out into the transparent area before the
       // upload instead, which is the weaker of the two ways and only used when the one above is
       // off.
       fDilateTransparentColors:boolean;
       fTick:TpvInt64;
       fTickFraction:TpvDouble;
       fDirty:boolean;
       procedure SetTick(const aTick:TpvInt64);
       procedure SetTickFraction(const aTickFraction:TpvDouble);
      public
       // aWidth and aHeight of zero take the size the document itself states. The document is
       // taken over unless aOwnsDocument says otherwise. Nothing on the Vulkan side happens here,
       // because that needs the transfer objects, which only Update gets.
       // aSRGB says that the bytes the rasterizer produces are sRGB values, which they always are:
       // the rasterizer keeps its picture in eight bit sRGB whichever color space it blends in, and
       // a linear run writes that back when it is done. Turning it off hands the bytes to the
       // canvas as if they were linear light, which comes out visibly washed out, and is only
       // right where the picture is not a picture at all but data, such as a mask.
       constructor Create(const aDevice:TpvVulkanDevice;
                          const aDocument:TpvVectorGraphicsDocument;
                          const aWidth:TpvInt32=0;
                          const aHeight:TpvInt32=0;
                          const aOwnsDocument:boolean=true;
                          const aSRGB:boolean=true); reintroduce;
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
       // Whether the colour is multiplied by its own alpha before every upload, which is what makes
       // the filtering of the hardware right at the edges of the picture.
       property PremultipliedAlpha:boolean read fPremultipliedAlpha write fPremultipliedAlpha;
       // Whether the colour is carried out into the transparent area instead, which only applies
       // when the one above is off.
       property DilateTransparentColors:boolean read fDilateTransparentColors write fDilateTransparentColors;
     end;

     // What is done about the seam two shapes leave between them where they share an edge. Each of
     // the two covers half of the pixel row the edge runs along, and drawing them one over the
     // other leaves a quarter of the background showing through. Every renderer has this, since it
     // follows from drawing each shape on its own with antialiasing, but it only catches the eye
     // where the canvas blends in linear light: a quarter of the background is then the value 137
     // of 255 rather than 64, and what looks like a hairline crack appears between two shapes that
     // were meant to read as one. librsvg, Inkscape and the software rasterizer of this very unit
     // all blend in sRGB and all show the same seam, only far fainter.
     PpvVectorGraphicsCanvasSeamMode=^TpvVectorGraphicsCanvasSeamMode;
     TpvVectorGraphicsCanvasSeamMode=
      (
       // Nothing, which is right wherever the document has no two shapes sharing an edge, and
       // which is the only one that leaves both the geometry and the blending exactly as they are.
       None=0,
       // Raise the coverage of every outline a little, which pushes its edge outwards until two
       // shapes that share one overlap instead. Costs nothing and works whatever the colors are,
       // and in return every outline thinner than a pixel is drawn by the same share too solid.
       WidenedCoverage=1,
       // Draw the whole document into a buffer of its own that blends in sRGB, then put that over
       // the canvas in one go. The seam is then exactly as faint as in every other renderer, and
       // no geometry is touched. Costs a buffer and a render pass of its own.
       SRGBCompositingBuffer=2
      );

     { TpvVectorGraphicsCanvasShapes }

     // A document turned into GPU vector path shapes, which the canvas then draws through its
     // vector path fragment shader. This is the resolution independent route: one and the same
     // set of shapes stays sharp at any zoom, because the outline is evaluated per pixel instead
     // of being flattened for one size beforehand.
     //
     // Shapes filled with a plain color and with the two kinds of SVG gradient arrive this way.
     // The gradients go through a ramp atlas, one row of 256 entries per distinct ramp, which the
     // shader samples. What is left over is a pattern or an image fill, a coverage mask, a blend
     // mode and a filter, none of which can be expressed as a filled outline. Recording notices
     // that, Build then returns false and Supported stays false, which is the signal to draw that
     // document through TpvVectorGraphicsCanvasTexture instead.
     TpvVectorGraphicsCanvasShapes=class
      private
       type { TCompositingRingEntry }
            // One slot of the ring the compositing buffer is drawn through. There is more than one
            // of them because the submit is not waited for any more: while the GPU is still
            // drawing one frame and the canvas is still sampling what came out of it, the next
            // frame must have a texture, a command buffer and a fence that nobody else is on.
            TCompositingRingEntry=record
             Texture:TpvVulkanTexture;
             ImageView:TpvVulkanImageView;
             FrameBuffer:TpvVulkanFrameBuffer;
             CommandBuffer:TpvVulkanCommandBuffer;
             Fence:TpvVulkanFence;
             // A pair of timestamps around the pass, so that what it costs the device can be had
             // without waiting for it: by the time this slot comes round again, its last frame is
             // long done and the numbers are simply there to be read.
             TimerQuery:TpvTimerQuery;
             // Whether a submit of this slot is still on its way, which is what says that the
             // fence has to be waited for before the slot is taken again.
             InFlight:boolean;
            end;
            PCompositingRingEntry=^TCompositingRingEntry;
            TCompositingRingEntries=array of TCompositingRingEntry;
      private
       fDevice:TpvVulkanDevice;
       fBufferPool:TpvVectorPathGPUBufferPool;
       fOwnsBufferPool:boolean;
       fRecorder:TpvVectorGraphicsRasterizerRecorder;
       fRecords:TpvVectorGraphicsRasterizerRecorder.TRecords;
       fSupported:boolean;
       fBoundingBox:TpvVectorPathBoundingBox;
       // One row of 256 entries per distinct ramp, in the order the rows were first met, together
       // with the row each record ends up sampling.
       fRampRows:array of TpvVectorGraphicsRasterizerRecorder.TGradientRamp;
       fCountRampRows:TpvSizeInt;
       fRecordRampRows:array of TpvSizeInt;
       fRampTexture:TpvVulkanTexture;
       fRampTextureDirty:boolean;
       // What is done about the seam between two shapes that share an edge, and how far the
       // coverage is raised when that is what was chosen.
       fSeamMode:TpvVectorGraphicsCanvasSeamMode;
       fSeamCoverageWidening:TpvFloat;
       // The buffer the SRGBCompositingBuffer mode draws the whole document into before putting it
       // over the canvas in one go. It is the size of the canvas it is drawn onto, so that both
       // share one coordinate system and what lands in it needs no transform of its own.
       //
       // The texture carries an sRGB format, which is the view the canvas samples it through, while
       // the image view the render pass writes into carries the plain one over the very same
       // memory. So the shader encodes to sRGB on the way in, the hardware blends on those encoded
       // values the way every other 2D renderer does, and the hardware decodes again on the way
       // out. VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT, which CreateSimple2DTarget sets whenever it is
       // given a second format, is what lets one image be seen as both.
       // The render pass and the canvas are the same for every slot, since all slots carry the
       // same format and size; only the attachment and what records into it go round.
       fCompositingRenderPass:TpvVulkanRenderPass;
       fCompositingCanvas:TpvCanvas;
       fCompositingCommandPool:TpvVulkanCommandPool;
       fCompositingRing:TCompositingRingEntries;
       fCompositingRingIndex:TpvSizeInt;
       fCompositingRingLength:TpvSizeInt;
       fCompositingGPUTime:TpvDouble;
       fCompositingWidth:TpvInt32;
       fCompositingHeight:TpvInt32;
       procedure ReleaseCompositingBuffer;
       procedure SetCompositingRingLength(const aCompositingRingLength:TpvSizeInt);
       function EnsureCompositingBuffer(const aGraphicsQueue:TpvVulkanQueue;
                                        const aGraphicsCommandBuffer:TpvVulkanCommandBuffer;
                                        const aGraphicsFence:TpvVulkanFence;
                                        const aWidth,aHeight:TpvInt32):boolean;
       procedure DrawThroughCompositingBuffer(const aCanvas:TpvCanvas;
                                              const aGraphicsQueue:TpvVulkanQueue;
                                              const aGraphicsCommandBuffer:TpvVulkanCommandBuffer;
                                              const aGraphicsFence:TpvVulkanFence);
       procedure DrawShapes(const aCanvas:TpvCanvas);
       // Takes every shape out of the pool again before the records that own them go away, so
       // that the pool is not left with entries pointing at freed shapes.
       procedure ReleaseRecords;
       // Hands the recorded shapes to the pool, which builds their grids and uploads them, so
       // that the frame which draws them finds everything in place.
       procedure RegisterShapes;
       // Collects the ramps of the recorded gradients, folding identical ones onto one row, and
       // notes per record which row it sampled.
       procedure CollectRampRows;
       function GetCountShapes:TpvSizeInt;
      public
       // aBufferPool nil creates a pool of this object's own, which is the simple case for one
       // document on its own. Several of these should share one pool, so that they end up in the
       // same buffers and the canvas does not have to swap its descriptor set between them.
       // aPasMPInstance is what a pool created here spreads the grid building of its shapes over,
       // which is where nearly all of the time of a Build goes. It is ignored when a pool is handed
       // in, since that one brings its own.
       constructor Create(const aDevice:TpvVulkanDevice;
                          const aBufferPool:TpvVectorPathGPUBufferPool=nil;
                          const aPasMPInstance:TPasMP=nil); reintroduce;
       destructor Destroy; override;
       // Uploads the gradient ramps, which the first draw after a Build needs. Does nothing when
       // nothing changed and nothing when the document has no gradient at all, so calling it
       // every frame is fine.
       procedure UpdateRampTexture(const aGraphicsQueue:TpvVulkanQueue;
                                   const aGraphicsCommandBuffer:TpvVulkanCommandBuffer;
                                   const aGraphicsFence:TpvVulkanFence;
                                   const aTransferQueue:TpvVulkanQueue;
                                   const aTransferCommandBuffer:TpvVulkanCommandBuffer;
                                   const aTransferFence:TpvVulkanFence);
       // Records aDocument at the given point in time. aMatrix is the space the shapes end up in,
       // so the identity keeps them in the units of the document while a view box matrix puts
       // them straight into target pixels. Returns whether the document really fits through this
       // route; when it does not, nothing is kept and the caller falls back to the texture route.
       //
       // The space matters for more than placement: curves are turned into quadratic ones within
       // aCurveTolerance of the shape's own units, and the stroker flattens its outlines at the
       // scale of aMatrix. Building in target pixels and leaving the tolerance at one therefore
       // gives the right amount of detail. Drawing the result much larger than that through the
       // canvas model matrix still stays sharp along the filled edges, because those are curves
       // the shader resolves, but the stroked ones eventually show their line segments, so a big
       // zoom is worth another Build.
       function Build(const aDocument:TpvVectorGraphicsDocument;
                      const aMatrix:TpvVectorPathMatrix;
                      const aTick:TpvInt64=0;
                      const aTickFraction:TpvDouble=0.0;
                      const aCurveTolerance:TpvDouble=1.0):boolean;
       // Same for a single object or subtree out of the document, by id. Returns false both when
       // no object carries that id and when the subtree does not fit through this route.
       function BuildObjectByID(const aDocument:TpvVectorGraphicsDocument;
                                const aID:TpvUTF8String;
                                const aMatrix:TpvVectorPathMatrix;
                                const aTick:TpvInt64=0;
                                const aTickFraction:TpvDouble=0.0;
                                const aCurveTolerance:TpvDouble=1.0):boolean;
       // Throws the recorded shapes away and releases them from the pool.
       procedure Clear;
       // Draws what Build recorded. The canvas has to have this object's pool set as its
       // VectorPathBufferPool, which Draw does itself, since one canvas draws one pool at a time
       // anyway.
       //
       // The three transfer objects are only needed by the SRGBCompositingBuffer seam mode, which
       // renders into a buffer of its own first. Left out, that mode falls back to drawing
       // straight onto the canvas. They have to be ones that may be submitted right here, which
       // holds while the canvas is still collecting geometry and its own render pass has not begun.
       procedure Draw(const aCanvas:TpvCanvas;
                      const aGraphicsQueue:TpvVulkanQueue=nil;
                      const aGraphicsCommandBuffer:TpvVulkanCommandBuffer=nil;
                      const aGraphicsFence:TpvVulkanFence=nil);
      public
       property BufferPool:TpvVectorPathGPUBufferPool read fBufferPool;
       property Records:TpvVectorGraphicsRasterizerRecorder.TRecords read fRecords;
       // Union of the bounding boxes of everything recorded, in the space aMatrix mapped into.
       property BoundingBox:TpvVectorPathBoundingBox read fBoundingBox;
       // The ramp atlas, or nil as long as no gradient was recorded or nothing was uploaded yet.
       property RampTexture:TpvVulkanTexture read fRampTexture;
      published
       property Supported:boolean read fSupported;
       property CountShapes:TpvSizeInt read GetCountShapes;
       // How many distinct gradient ramps the recorded document uses.
       property CountRampRows:TpvSizeInt read fCountRampRows;
       // What is done about the seam two shapes leave between them where they share an edge. None
       // by default, so that a document without such a pair draws exactly as it did before.
       property SeamMode:TpvVectorGraphicsCanvasSeamMode read fSeamMode write fSeamMode;
       // By what share the coverage is raised when SeamMode asks for that. A half pushes every
       // edge a quarter of a pixel outwards, which closes most of the seam, and a one closes it
       // completely at the price of drawing everything thinner than a pixel twice as solid.
       property SeamCoverageWidening:TpvFloat read fSeamCoverageWidening write fSeamCoverageWidening;
       // How many frames the SRGBCompositingBuffer mode keeps in the air at once. It needs to be
       // at least as many as the application lets run before it waits, since the whole point of
       // the ring is that nothing is overwritten while the GPU is still on it. Setting it throws
       // the buffer away, so the next draw builds the ring anew.
       property CompositingRingLength:TpvSizeInt read fCompositingRingLength write SetCompositingRingLength;
       // What the pass of the SRGBCompositingBuffer mode last cost the device, in milliseconds, or
       // zero while that mode is not the one in use. It is the measurement of a frame a few back,
       // since reading the one of this frame would mean waiting for it.
       property CompositingGPUTime:TpvDouble read fCompositingGPUTime;
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

 fPremultipliedAlpha:=true;

 fDilateTransparentColors:=true;

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

 // The rasterizer clears to transparent black, so every texel outside the picture holds black, and
 // the hardware's bilinear filter mixes that black into every edge texel it touches, which it does
 // before the fragment shader ever multiplies by the alpha. The result is a dark fringe all the way
 // around, and the same happens again in every mip level and at the transparent black border the
 // sampler reads past the edge of the picture.
 //
 // Multiplying the colour by its own alpha here is what makes all of that right rather than only
 // less wrong: the average of premultiplied texels is the premultiplied average, so whatever the
 // filter mixes in from a transparent texel is zero and contributes nothing, at every mip level and
 // at the border alike. The shader is told not to multiply a second time.
 //
 // Carrying the colour of the nearest opaque texel outwards is the other way, the one a sprite
 // atlas takes, and it only approximates this: it gets the colour right where the picture has one
 // colour along the edge, and not where two meet. It is left here for a caller that needs the
 // texture to keep straight alpha for something else.
 if fPremultipliedAlpha then begin
  ConvertImageAlphaMode(Pixels,fRasterizer.Width,fRasterizer.Height,false,fSRGB,true);
 end else if fDilateTransparentColors then begin
  DilateTransparentColoursInImage(Pixels,fRasterizer.Width,fRasterizer.Height,false);
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
var OldTexturePremultipliedAlpha:boolean;
begin
 if assigned(fTexture) then begin
  // The shader has to know which of the two the texture holds, or it multiplies by the alpha a
  // second time and every half transparent edge comes out too faint.
  OldTexturePremultipliedAlpha:=aCanvas.TexturePremultipliedAlpha;
  try
   aCanvas.TexturePremultipliedAlpha:=fPremultipliedAlpha;
   aCanvas.DrawTexturedRectangle(fTexture,aDestination);
  finally
   aCanvas.TexturePremultipliedAlpha:=OldTexturePremultipliedAlpha;
  end;
 end;
end;

procedure TpvVectorGraphicsCanvasTexture.Draw(const aCanvas:TpvCanvas;const aPosition:TpvVector2);
begin
 if assigned(fTexture) then begin
  // Through the one above, so that it tells the shader about the premultiplied colours here too.
  Draw(aCanvas,TpvRect.CreateRelative(aPosition.x,aPosition.y,
                                      fRasterizer.Width,fRasterizer.Height));
 end;
end;

{ TpvVectorGraphicsCanvasShapes }

constructor TpvVectorGraphicsCanvasShapes.Create(const aDevice:TpvVulkanDevice;
                                                 const aBufferPool:TpvVectorPathGPUBufferPool;
                                                 const aPasMPInstance:TPasMP);
begin

 inherited Create;

 fDevice:=aDevice;

 if assigned(aBufferPool) then begin
  fBufferPool:=aBufferPool;
  fOwnsBufferPool:=false;
 end else begin
  fBufferPool:=TpvVectorPathGPUBufferPool.Create(aDevice);
  fBufferPool.PasMPInstance:=aPasMPInstance;
  fOwnsBufferPool:=true;
 end;

 // The recorder never touches a pixel, so it gets no buffer of its own.
 fRecorder:=TpvVectorGraphicsRasterizerRecorder.Create;

 fRecords:=nil;
 fSupported:=false;
 fBoundingBox:=TpvVectorPathBoundingBox.Create(TpvVectorPathVector.Create(0.0,0.0),
                                               TpvVectorPathVector.Create(0.0,0.0));

 fRampRows:=nil;
 fCountRampRows:=0;
 fRecordRampRows:=nil;
 fRampTexture:=nil;
 fRampTextureDirty:=false;

 fSeamMode:=TpvVectorGraphicsCanvasSeamMode.None;
 fSeamCoverageWidening:=0.5;

 fCompositingRenderPass:=nil;
 fCompositingCanvas:=nil;
 fCompositingCommandPool:=nil;
 fCompositingRing:=nil;
 fCompositingRingIndex:=0;
 fCompositingGPUTime:=0.0;
 // Three is what a swap chain usually keeps in the air, and one more than that costs only another
 // texture while one less would put the wait back in.
 fCompositingRingLength:=3;
 fCompositingWidth:=0;
 fCompositingHeight:=0;

end;

destructor TpvVectorGraphicsCanvasShapes.Destroy;
begin

 ReleaseRecords;

 ReleaseCompositingBuffer;

 FreeAndNil(fRecorder);

 FreeAndNil(fRampTexture);
 fRampRows:=nil;
 fRecordRampRows:=nil;

 if fOwnsBufferPool then begin
  FreeAndNil(fBufferPool);
 end else begin
  fBufferPool:=nil;
 end;

 inherited Destroy;

end;

procedure TpvVectorGraphicsCanvasShapes.ReleaseRecords;
var Index:TpvSizeInt;
    CurrentRecord:TpvVectorGraphicsRasterizerRecorder.TRecord_;
begin

 if not assigned(fRecords) then begin
  exit;
 end;

 // The records own their shapes, and the pool keys its entries on the shape objects, so the pool
 // has to let go first or it would keep entries pointing at freed memory.
 if assigned(fBufferPool) then begin
  for Index:=0 to fRecords.Count-1 do begin
   CurrentRecord:=fRecords[Index];
   if assigned(CurrentRecord) and assigned(CurrentRecord.Shape) then begin
    fBufferPool.RemoveShape(CurrentRecord.Shape);
   end;
  end;
 end;

 FreeAndNil(fRecords);

end;

function TpvVectorGraphicsCanvasShapes.GetCountShapes:TpvSizeInt;
begin
 if assigned(fRecords) then begin
  result:=fRecords.Count;
 end else begin
  result:=0;
 end;
end;

procedure TpvVectorGraphicsCanvasShapes.ReleaseCompositingBuffer;
var Index:TpvSizeInt;
    Entry:PCompositingRingEntry;
begin
 // Nothing may be torn down while the GPU is still reading it, and since the submits are no
 // longer waited for, that has to happen here.
 for Index:=0 to length(fCompositingRing)-1 do begin
  Entry:=@fCompositingRing[Index];
  if Entry^.InFlight and assigned(Entry^.Fence) then begin
   Entry^.Fence.WaitFor;
   Entry^.Fence.Reset;
  end;
  Entry^.InFlight:=false;
 end;
 FreeAndNil(fCompositingCanvas);
 for Index:=0 to length(fCompositingRing)-1 do begin
  Entry:=@fCompositingRing[Index];
  FreeAndNil(Entry^.FrameBuffer);
  FreeAndNil(Entry^.ImageView);
  FreeAndNil(Entry^.Texture);
  FreeAndNil(Entry^.TimerQuery);
  FreeAndNil(Entry^.CommandBuffer);
  FreeAndNil(Entry^.Fence);
 end;
 fCompositingRing:=nil;
 fCompositingRingIndex:=0;
 FreeAndNil(fCompositingCommandPool);
 FreeAndNil(fCompositingRenderPass);
 fCompositingWidth:=0;
 fCompositingHeight:=0;
end;

procedure TpvVectorGraphicsCanvasShapes.SetCompositingRingLength(const aCompositingRingLength:TpvSizeInt);
var CompositingRingLength:TpvSizeInt;
begin
 CompositingRingLength:=Min(Max(aCompositingRingLength,1),16);
 if fCompositingRingLength<>CompositingRingLength then begin
  fCompositingRingLength:=CompositingRingLength;
  ReleaseCompositingBuffer;
 end;
end;

// Builds the buffer the document is drawn into, or keeps the one there is when it already has the
// size asked for. Returns whether there is one to draw into afterwards.
function TpvVectorGraphicsCanvasShapes.EnsureCompositingBuffer(const aGraphicsQueue:TpvVulkanQueue;
                                                               const aGraphicsCommandBuffer:TpvVulkanCommandBuffer;
                                                               const aGraphicsFence:TpvVulkanFence;
                                                               const aWidth,aHeight:TpvInt32):boolean;
var Index:TpvSizeInt;
    Entry:PCompositingRingEntry;
begin

 if (aWidth<1) or (aHeight<1) then begin
  result:=false;
  exit;
 end;

 if assigned(fCompositingCanvas) and (fCompositingWidth=aWidth) and (fCompositingHeight=aHeight) then begin
  result:=true;
  exit;
 end;

 ReleaseCompositingBuffer;

 try

  // One attachment, cleared to nothing at the start and left in the layout the canvas samples it
  // from at the end, so that no barrier of its own is needed around it.
  fCompositingRenderPass:=TpvVulkanRenderPass.Create(fDevice);
  fCompositingRenderPass.AddSubpassDescription(0,
                                               VK_PIPELINE_BIND_POINT_GRAPHICS,
                                               [],
                                               [fCompositingRenderPass.AddAttachmentReference(fCompositingRenderPass.AddAttachmentDescription(0,
                                                                                                                                              VK_FORMAT_R8G8B8A8_UNORM,
                                                                                                                                              VK_SAMPLE_COUNT_1_BIT,
                                                                                                                                              VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                                                                                                              VK_ATTACHMENT_STORE_OP_STORE,
                                                                                                                                              VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                                                                                              VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                                              VK_IMAGE_LAYOUT_UNDEFINED,
                                                                                                                                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL),
                                                                                              VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL)],
                                               [],
                                               VK_ATTACHMENT_UNUSED,
                                               []);
  fCompositingRenderPass.AddSubpassDependency(VK_SUBPASS_EXTERNAL,
                                              0,
                                              TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                              TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                              TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                              TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
                                              TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT));
  fCompositingRenderPass.AddSubpassDependency(0,
                                              VK_SUBPASS_EXTERNAL,
                                              TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                              TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                              TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
                                              TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                              TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT));
  fCompositingRenderPass.Initialize;

  fCompositingRenderPass.ClearValues[0].color.float32[0]:=0.0;
  fCompositingRenderPass.ClearValues[0].color.float32[1]:=0.0;
  fCompositingRenderPass.ClearValues[0].color.float32[2]:=0.0;
  fCompositingRenderPass.ClearValues[0].color.float32[3]:=0.0;

  fCompositingCommandPool:=TpvVulkanCommandPool.Create(fDevice,
                                                       aGraphicsQueue.QueueFamilyIndex,
                                                       TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

  SetLength(fCompositingRing,fCompositingRingLength);
  for Index:=0 to fCompositingRingLength-1 do begin
   Entry:=@fCompositingRing[Index];
   Entry^.Texture:=nil;
   Entry^.ImageView:=nil;
   Entry^.FrameBuffer:=nil;
   Entry^.CommandBuffer:=nil;
   Entry^.Fence:=nil;
   Entry^.TimerQuery:=nil;
   Entry^.InFlight:=false;
  end;

  for Index:=0 to fCompositingRingLength-1 do begin

   Entry:=@fCompositingRing[Index];

   // The sRGB format is the one the texture keeps and therefore the one the canvas samples it
   // through. The plain view over the same memory comes right after, for the render pass to write
   // into.
   Entry^.Texture:=TpvVulkanTexture.CreateSimple2DTarget(fDevice,
                                                         aGraphicsQueue,
                                                         aGraphicsCommandBuffer,
                                                         aGraphicsFence,
                                                         true,
                                                         VK_FORMAT_R8G8B8A8_SRGB,
                                                         VK_FORMAT_R8G8B8A8_SRGB,
                                                         aWidth,
                                                         aHeight,
                                                         [TpvVulkanTextureUsageFlag.ColorAttachment,
                                                          TpvVulkanTextureUsageFlag.Sampled,
                                                          TpvVulkanTextureUsageFlag.TransferDst],
                                                         VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                         pvAllocationGroupIDCanvas,
                                                         'TpvVectorGraphicsCanvasShapes.fCompositingRing.Texture');

   Entry^.Texture.WrapModeU:=TpvVulkanTextureWrapMode.ClampToBorder;
   Entry^.Texture.WrapModeV:=TpvVulkanTextureWrapMode.ClampToBorder;
   Entry^.Texture.WrapModeW:=TpvVulkanTextureWrapMode.ClampToBorder;
   Entry^.Texture.BorderColor:=VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK;
   Entry^.Texture.UpdateSampler;

   Entry^.ImageView:=TpvVulkanImageView.Create(fDevice,
                                               Entry^.Texture.Image,
                                               VK_IMAGE_VIEW_TYPE_2D,
                                               VK_FORMAT_R8G8B8A8_UNORM,
                                               VK_COMPONENT_SWIZZLE_IDENTITY,
                                               VK_COMPONENT_SWIZZLE_IDENTITY,
                                               VK_COMPONENT_SWIZZLE_IDENTITY,
                                               VK_COMPONENT_SWIZZLE_IDENTITY,
                                               TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                               0,
                                               1,
                                               0,
                                               1);

   Entry^.FrameBuffer:=TpvVulkanFrameBuffer.Create(fDevice,
                                                   fCompositingRenderPass,
                                                   aWidth,
                                                   aHeight,
                                                   1,
                                                   [TpvVulkanFrameBufferAttachment.Create(fDevice,
                                                                                          Entry^.Texture.Image,
                                                                                          Entry^.ImageView,
                                                                                          aWidth,
                                                                                          aHeight,
                                                                                          VK_FORMAT_R8G8B8A8_UNORM,
                                                                                          false)],
                                                   true);

   Entry^.CommandBuffer:=TpvVulkanCommandBuffer.Create(fCompositingCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

   Entry^.Fence:=TpvVulkanFence.Create(fDevice);

   Entry^.TimerQuery:=TpvTimerQuery.Create(fDevice,1);

  end;

  fCompositingCanvas:=TpvCanvas.Create(fDevice,nil,1);
  // One buffer per slot of the ring, since the one a submit still reads must not be written into
  // by the frame after it. This is the count Start, ExecuteUpload and ExecuteDraw index into, and
  // it is not the count of processing buffers the constructor takes.
  fCompositingCanvas.CountBuffers:=fCompositingRingLength;
  fCompositingCanvas.VulkanRenderPass:=fCompositingRenderPass;
  fCompositingCanvas.Resize(aWidth,aHeight);
  // The attachment stores plain numbers, so the shader encodes to sRGB itself and the hardware
  // then blends on those encoded values, which is the whole point of this route.
  fCompositingCanvas.EncodeOutputToSRGB:=true;

  fCompositingWidth:=aWidth;
  fCompositingHeight:=aHeight;

  result:=true;

 except
  ReleaseCompositingBuffer;
  raise;
 end;

end;

// Draws the whole document into the buffer of its own, which blends in sRGB, and then puts that
// over the canvas as one picture. Only the part of it the document really covers is drawn back,
// since the buffer is the size of the whole canvas and the rest of it is empty.
procedure TpvVectorGraphicsCanvasShapes.DrawThroughCompositingBuffer(const aCanvas:TpvCanvas;
                                                                     const aGraphicsQueue:TpvVulkanQueue;
                                                                     const aGraphicsCommandBuffer:TpvVulkanCommandBuffer;
                                                                     const aGraphicsFence:TpvVulkanFence);
var Width,Height:TpvInt32;
    Corners:array[0..3] of TpvVector2;
    CornerIndex:TpvSizeInt;
    MinX,MinY,MaxX,MaxY:TpvFloat;
    DestinationRect:TpvRect;
    OldTexturePremultipliedAlpha:boolean;
    Entry:PCompositingRingEntry;
begin

 Width:=aCanvas.Width;
 Height:=aCanvas.Height;

 if not EnsureCompositingBuffer(aGraphicsQueue,aGraphicsCommandBuffer,aGraphicsFence,Width,Height) then begin
  DrawShapes(aCanvas);
  exit;
 end;

 Entry:=@fCompositingRing[fCompositingRingIndex];

 // The slot comes round again only after the whole ring has been through, so by now the GPU is
 // long done with it and this waits for nothing. It is still needed, because a ring that is
 // shorter than what the application keeps in the air would otherwise overwrite what is being
 // read.
 if Entry^.InFlight then begin
  Entry^.Fence.WaitFor;
  Entry^.Fence.Reset;
  Entry^.InFlight:=false;
 end;

 // Now that this slot is known to be done with, what it cost the device last time round can be
 // read off it, which is a few frames back but costs nothing to have.
 if assigned(Entry^.TimerQuery) and Entry^.TimerQuery.Update then begin
  fCompositingGPUTime:=Entry^.TimerQuery.Total*1000.0;
 end;

 // The buffer is the size of the canvas and shares its coordinate system, so everything the
 // drawing side set up carries over unchanged and what lands in the buffer lands where it belongs.
 fCompositingCanvas.Start(fCompositingRingIndex);
 try
  fCompositingCanvas.ViewMatrix:=aCanvas.ViewMatrix;
  fCompositingCanvas.ProjectionMatrix:=aCanvas.ProjectionMatrix;
  fCompositingCanvas.ModelMatrix:=aCanvas.ModelMatrix;
  fCompositingCanvas.BlendingMode:=TpvCanvasBlendingMode.AlphaBlending;
  fCompositingCanvas.VectorPathCoverageMode:=aCanvas.VectorPathCoverageMode;
  fCompositingCanvas.VectorPathCoverageSampleCount:=aCanvas.VectorPathCoverageSampleCount;
  DrawShapes(fCompositingCanvas);
 finally
  fCompositingCanvas.Stop;
 end;

 fCompositingCanvas.ExecuteUpload(aGraphicsQueue,aGraphicsCommandBuffer,aGraphicsFence,fCompositingRingIndex);

 Entry^.CommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));
 Entry^.CommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));

 if assigned(Entry^.TimerQuery) then begin
  Entry^.TimerQuery.Reset;
  Entry^.TimerQuery.Start(aGraphicsQueue,Entry^.CommandBuffer,'compositing');
 end;

 fCompositingRenderPass.BeginRenderPass(Entry^.CommandBuffer,
                                        Entry^.FrameBuffer,
                                        VK_SUBPASS_CONTENTS_INLINE,
                                        0,
                                        0,
                                        Width,
                                        Height);
 fCompositingCanvas.ExecuteDraw(Entry^.CommandBuffer,fCompositingRingIndex);
 fCompositingRenderPass.EndRenderPass(Entry^.CommandBuffer);

 if assigned(Entry^.TimerQuery) then begin
  Entry^.TimerQuery.Stop(aGraphicsQueue,Entry^.CommandBuffer);
 end;

 Entry^.CommandBuffer.EndRecording;

 // Submitted without waiting for it. What the canvas draws afterwards goes to the same queue and
 // therefore runs after this, and the render pass already ends in the layout the canvas samples
 // from with a dependency onto the fragment stage, so there is nothing left for the processor to
 // stand around for. Waiting here is what used to cost three to four milliseconds a frame at a
 // strong zoom: not more work, just the whole of the drawing done before anything else began.
 Entry^.CommandBuffer.Execute(aGraphicsQueue,TVkPipelineStageFlags(VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT),nil,nil,Entry^.Fence,false);
 Entry^.InFlight:=true;

 // On to the next slot right away, so that the way out below, where the document covers nothing of
 // the canvas, leaves the ring turning as well.
 fCompositingRingIndex:=(fCompositingRingIndex+1) mod fCompositingRingLength;

 // Where the document ended up on the canvas, which is the only part of the buffer worth putting
 // back. The bounding box is in the space the shapes were recorded in, so the model matrix of the
 // canvas is what takes it to where it was drawn, and all four corners are needed since that
 // matrix may turn it.
 Corners[0]:=TpvVector2.InlineableCreate(fBoundingBox.Min.x,fBoundingBox.Min.y);
 Corners[1]:=TpvVector2.InlineableCreate(fBoundingBox.Max.x,fBoundingBox.Min.y);
 Corners[2]:=TpvVector2.InlineableCreate(fBoundingBox.Max.x,fBoundingBox.Max.y);
 Corners[3]:=TpvVector2.InlineableCreate(fBoundingBox.Min.x,fBoundingBox.Max.y);
 MinX:=Infinity;
 MinY:=Infinity;
 MaxX:=-Infinity;
 MaxY:=-Infinity;
 for CornerIndex:=0 to 3 do begin
  Corners[CornerIndex]:=aCanvas.ModelMatrix*Corners[CornerIndex];
  MinX:=Min(MinX,Corners[CornerIndex].x);
  MinY:=Min(MinY,Corners[CornerIndex].y);
  MaxX:=Max(MaxX,Corners[CornerIndex].x);
  MaxY:=Max(MaxY,Corners[CornerIndex].y);
 end;

 // Whole texels, and never outside the buffer, since a fetch past its edge would read the border.
 MinX:=Max(Floor(MinX),0.0);
 MinY:=Max(Floor(MinY),0.0);
 MaxX:=Min(Ceil(MaxX),Width);
 MaxY:=Min(Ceil(MaxY),Height);
 if (MaxX<=MinX) or (MaxY<=MinY) then begin
  exit;
 end;

 DestinationRect:=TpvRect.CreateAbsolute(0.0,0.0,Width,Height);

 OldTexturePremultipliedAlpha:=aCanvas.TexturePremultipliedAlpha;
 try
  // One to one over the whole canvas, so the model matrix has to be out of the way: the picture is
  // already where it belongs. The scissor is what keeps the work to the part the document really
  // covers, since the rest of the buffer holds nothing. What comes out of it carries its alpha in
  // its colors already.
  aCanvas.Push;
  try
   aCanvas.SetScissor(trunc(MinX),trunc(MinY),trunc(MaxX-MinX),trunc(MaxY-MinY));
   aCanvas.ModelMatrix:=TpvMatrix4x4.Identity;
   aCanvas.Color:=TpvVector4.InlineableCreate(1.0,1.0,1.0,1.0);
   aCanvas.BlendingMode:=TpvCanvasBlendingMode.AlphaBlending;
   aCanvas.TexturePremultipliedAlpha:=true;
   aCanvas.DrawTexturedRectangle(Entry^.Texture,DestinationRect);
  finally
   aCanvas.Pop;
  end;
 finally
  aCanvas.TexturePremultipliedAlpha:=OldTexturePremultipliedAlpha;
 end;

end;

procedure TpvVectorGraphicsCanvasShapes.Clear;
begin
 ReleaseRecords;
 fSupported:=false;
 fBoundingBox:=TpvVectorPathBoundingBox.Create(TpvVectorPathVector.Create(0.0,0.0),
                                               TpvVectorPathVector.Create(0.0,0.0));
 // The rows go, the texture stays until the next upload, because throwing it away here would
 // leave a draw between a Build and an UpdateRampTexture with nothing to sample.
 fCountRampRows:=0;
 fRecordRampRows:=nil;
end;

procedure TpvVectorGraphicsCanvasShapes.CollectRampRows;
var Index,RowIndex,FoundRow:TpvSizeInt;
    CurrentRecord:TpvVectorGraphicsRasterizerRecorder.TRecord_;
begin

 fCountRampRows:=0;
 fRecordRampRows:=nil;

 if not assigned(fRecords) then begin
  exit;
 end;

 SetLength(fRecordRampRows,fRecords.Count);
 for Index:=0 to fRecords.Count-1 do begin
  fRecordRampRows[Index]:=-1;
 end;

 for Index:=0 to fRecords.Count-1 do begin

  CurrentRecord:=fRecords[Index];
  if (not assigned(CurrentRecord)) or
     (CurrentRecord.FillKind=TpvVectorGraphicsRasterizerRecorder.TFillKind.Solid) then begin
   continue;
  end;

  // Several shapes usually share one gradient, and a document that reuses one across a dozen
  // elements would otherwise get a dozen identical rows.
  FoundRow:=-1;
  for RowIndex:=0 to fCountRampRows-1 do begin
   if CompareMem(@fRampRows[RowIndex],
                 CurrentRecord.GradientRamp,
                 SizeOf(TpvVectorGraphicsRasterizerRecorder.TGradientRamp)) then begin
    FoundRow:=RowIndex;
    break;
   end;
  end;

  if FoundRow<0 then begin
   if length(fRampRows)<=fCountRampRows then begin
    SetLength(fRampRows,(fCountRampRows+1)*2);
   end;
   Move(CurrentRecord.GradientRamp^,
        fRampRows[fCountRampRows],
        SizeOf(TpvVectorGraphicsRasterizerRecorder.TGradientRamp));
   FoundRow:=fCountRampRows;
   inc(fCountRampRows);
  end;

  fRecordRampRows[Index]:=FoundRow;

 end;

 fRampTextureDirty:=fCountRampRows>0;

end;

procedure TpvVectorGraphicsCanvasShapes.UpdateRampTexture(const aGraphicsQueue:TpvVulkanQueue;
                                                          const aGraphicsCommandBuffer:TpvVulkanCommandBuffer;
                                                          const aGraphicsFence:TpvVulkanFence;
                                                          const aTransferQueue:TpvVulkanQueue;
                                                          const aTransferCommandBuffer:TpvVulkanCommandBuffer;
                                                          const aTransferFence:TpvVulkanFence);
const RampWidth=256;
var DataSize:TVkSizeInt;
    Rows:TpvSizeInt;
begin

 if (not fRampTextureDirty) or (fCountRampRows=0) then begin
  exit;
 end;

 // The texture is as tall as the rows it holds, so a document that gained or lost a gradient
 // needs a new one rather than an upload into the old one.
 if assigned(fRampTexture) and (fRampTexture.Height<>fCountRampRows) then begin
  FreeAndNil(fRampTexture);
 end;

 Rows:=fCountRampRows;
 DataSize:=TVkSizeInt(RampWidth)*TVkSizeInt(Rows)*SizeOf(TpvVectorGraphicsColor);

 if assigned(fRampTexture) then begin

  fRampTexture.Upload(aGraphicsQueue,aGraphicsCommandBuffer,aGraphicsFence,
                      aTransferQueue,aTransferCommandBuffer,aTransferFence,
                      @fRampRows[0],DataSize,false,false,1,true);

 end else begin

  // The ramp entries are sRGB values, and the canvas works on linear ones, so the format does
  // that conversion on every fetch and the shader gets what it expects.
  fRampTexture:=TpvVulkanTexture.CreateFromMemory(fDevice,
                                                  aGraphicsQueue,aGraphicsCommandBuffer,aGraphicsFence,
                                                  aTransferQueue,aTransferCommandBuffer,aTransferFence,
                                                  VK_FORMAT_R8G8B8A8_SRGB,
                                                  VK_SAMPLE_COUNT_1_BIT,
                                                  RampWidth,
                                                  Rows,
                                                  0,
                                                  0,
                                                  1,
                                                  1,
                                                  [TpvVulkanTextureUsageFlag.TransferDst,
                                                   TpvVulkanTextureUsageFlag.Sampled],
                                                  @fRampRows[0],
                                                  DataSize,
                                                  false,
                                                  false,
                                                  1,
                                                  true,
                                                  false,
                                                  true);

  // Nearest, because one filter setting has to serve both directions and interpolating along the
  // rows would mean interpolating between neighbouring gradients as well. It also makes the
  // sampled entry the very one the software rasterizer would have picked.
  fRampTexture.FilterMode:=TpvVulkanTextureFilterMode.Nearest;
  fRampTexture.WrapModeU:=TpvVulkanTextureWrapMode.ClampToEdge;
  fRampTexture.WrapModeV:=TpvVulkanTextureWrapMode.ClampToEdge;
  fRampTexture.WrapModeW:=TpvVulkanTextureWrapMode.ClampToEdge;
  fRampTexture.UpdateSampler;

 end;

 fRampTextureDirty:=false;

end;

function TpvVectorGraphicsCanvasShapes.Build(const aDocument:TpvVectorGraphicsDocument;
                                             const aMatrix:TpvVectorPathMatrix;
                                             const aTick:TpvInt64;
                                             const aTickFraction:TpvDouble;
                                             const aCurveTolerance:TpvDouble):boolean;
begin

 Clear;

 result:=false;

 if not assigned(aDocument) then begin
  exit;
 end;

 fRecorder.CubicToQuadraticTolerance:=aCurveTolerance;
 fRecorder.ClearRecords;

 aDocument.Update;
 aDocument.Render(fRecorder,aMatrix,aTick,aTickFraction);

 result:=fRecorder.FullyRecorded;
 if not result then begin
  // Nothing of this is of any use to the caller, and holding on to it would only tie up pool
  // entries for shapes that never get drawn.
  fRecorder.ClearRecords;
  exit;
 end;

 fBoundingBox:=fRecorder.GetBoundingBox;
 fRecords:=fRecorder.ExtractRecords;
 fSupported:=true;

 // Registering the shapes here rather than on the first draw keeps the upload out of the frame
 // that draws them.
 RegisterShapes;
 CollectRampRows;

end;

function TpvVectorGraphicsCanvasShapes.BuildObjectByID(const aDocument:TpvVectorGraphicsDocument;
                                                       const aID:TpvUTF8String;
                                                       const aMatrix:TpvVectorPathMatrix;
                                                       const aTick:TpvInt64;
                                                       const aTickFraction:TpvDouble;
                                                       const aCurveTolerance:TpvDouble):boolean;
begin

 Clear;

 result:=false;

 if not assigned(aDocument) then begin
  exit;
 end;

 fRecorder.CubicToQuadraticTolerance:=aCurveTolerance;
 fRecorder.ClearRecords;

 aDocument.Update;
 if not aDocument.RenderObjectByID(aID,
                                   fRecorder,
                                   aMatrix,
                                   TpvVectorGraphicsRenderOverride.CreateNone,
                                   aTick,
                                   aTickFraction) then begin
  fRecorder.ClearRecords;
  exit;
 end;

 result:=fRecorder.FullyRecorded;
 if not result then begin
  fRecorder.ClearRecords;
  exit;
 end;

 fBoundingBox:=fRecorder.GetBoundingBox;
 fRecords:=fRecorder.ExtractRecords;
 fSupported:=true;

 RegisterShapes;
 CollectRampRows;

end;

procedure TpvVectorGraphicsCanvasShapes.RegisterShapes;
var Index:TpvSizeInt;
    CurrentRecord:TpvVectorGraphicsRasterizerRecorder.TRecord_;
begin

 if not (assigned(fRecords) and assigned(fBufferPool)) then begin
  exit;
 end;

 for Index:=0 to fRecords.Count-1 do begin
  CurrentRecord:=fRecords[Index];
  if assigned(CurrentRecord) and
     assigned(CurrentRecord.Shape) and
     (CurrentRecord.Shape.Contours.Count>0) then begin
   CurrentRecord.Shape.FillRule:=CurrentRecord.FillRule;
   fBufferPool.GetOrCreateShape(CurrentRecord.Shape);
  end;
 end;

 fBufferPool.Update;

end;

procedure TpvVectorGraphicsCanvasShapes.Draw(const aCanvas:TpvCanvas;
                                             const aGraphicsQueue:TpvVulkanQueue;
                                             const aGraphicsCommandBuffer:TpvVulkanCommandBuffer;
                                             const aGraphicsFence:TpvVulkanFence);
begin

 if not (fSupported and assigned(fRecords) and assigned(aCanvas)) then begin
  exit;
 end;

 if (fSeamMode=TpvVectorGraphicsCanvasSeamMode.SRGBCompositingBuffer) and
    assigned(aGraphicsQueue) and
    assigned(aGraphicsCommandBuffer) and
    assigned(aGraphicsFence) then begin
  DrawThroughCompositingBuffer(aCanvas,aGraphicsQueue,aGraphicsCommandBuffer,aGraphicsFence);
 end else begin
  DrawShapes(aCanvas);
 end;

end;

procedure TpvVectorGraphicsCanvasShapes.DrawShapes(const aCanvas:TpvCanvas);
var Index:TpvSizeInt;
    CurrentRecord:TpvVectorGraphicsRasterizerRecorder.TRecord_;
    Color:TpvVectorGraphicsColor;
    OldColor:TpvVector4;
    OldTexture:TObject;
    OldBufferPool:TpvVectorPathGPUBufferPool;
    OldCoverageWidening:TpvFloat;
    GradientKind:TpvCanvasVectorPathGradientKind;
    GradientSpreadMode:TpvCanvasVectorPathGradientSpreadMode;
    RampRow:TpvFloat;
begin

 if not (fSupported and assigned(fRecords) and assigned(aCanvas)) then begin
  exit;
 end;

 OldColor:=aCanvas.Color;
 OldTexture:=aCanvas.Texture;
 OldBufferPool:=aCanvas.VectorPathBufferPool;
 OldCoverageWidening:=aCanvas.VectorPathCoverageWidening;
 try

  aCanvas.VectorPathBufferPool:=fBufferPool;

  // Widening the coverage is the one of the three that the shader does by itself, so it is nothing
  // but a number handed over here.
  if fSeamMode=TpvVectorGraphicsCanvasSeamMode.WidenedCoverage then begin
   aCanvas.VectorPathCoverageWidening:=fSeamCoverageWidening;
  end else begin
   aCanvas.VectorPathCoverageWidening:=0.0;
  end;

  // The ramp atlas is what the shader samples a gradient out of, and it is bound the ordinary
  // way, as the texture of the canvas.
  if assigned(fRampTexture) then begin
   aCanvas.Texture:=fRampTexture;
  end;

  for Index:=0 to fRecords.Count-1 do begin

   CurrentRecord:=fRecords[Index];
   if (not assigned(CurrentRecord)) or
      (not assigned(CurrentRecord.Shape)) or
      (CurrentRecord.Shape.Contours.Count=0) then begin
    continue;
   end;

   // The recorded color is an sRGB value, while the canvas works on linear ones and leaves the
   // way back to sRGB to the color format of the target, so it is converted here. The alpha is
   // a plain coverage factor and stays as it is, and the shader premultiplies by itself, so it
   // is handed over straight.
   Color:=CurrentRecord.Color;
   aCanvas.Color:=TpvVector4.InlineableCreate(pvVectorGraphicsSRGBToLinear(((Color shr 16) and $ff)/255.0),
                                              pvVectorGraphicsSRGBToLinear(((Color shr 8) and $ff)/255.0),
                                              pvVectorGraphicsSRGBToLinear((Color and $ff)/255.0),
                                              ((Color shr 24) and $ff)/255.0);

   case CurrentRecord.FillKind of
    TpvVectorGraphicsRasterizerRecorder.TFillKind.LinearGradient,
    TpvVectorGraphicsRasterizerRecorder.TFillKind.RadialGradient:begin

     if CurrentRecord.FillKind=TpvVectorGraphicsRasterizerRecorder.TFillKind.RadialGradient then begin
      GradientKind:=TpvCanvasVectorPathGradientKind.Radial;
     end else begin
      GradientKind:=TpvCanvasVectorPathGradientKind.Linear;
     end;

     case CurrentRecord.GradientSpreadMethod of
      TpvVectorGraphicsSpreadMethod.Reflect:begin
       GradientSpreadMode:=TpvCanvasVectorPathGradientSpreadMode.MirroredRepeat;
      end;
      TpvVectorGraphicsSpreadMethod.Repeat_:begin
       GradientSpreadMode:=TpvCanvasVectorPathGradientSpreadMode.WrappedRepeat;
      end;
      else begin
       GradientSpreadMode:=TpvCanvasVectorPathGradientSpreadMode.Pad;
      end;
     end;

     // The row is addressed by its middle, so that nearest sampling cannot land on a neighbour.
     if (Index<length(fRecordRampRows)) and (fRecordRampRows[Index]>=0) and (fCountRampRows>0) then begin
      RampRow:=(fRecordRampRows[Index]+0.5)/fCountRampRows;
     end else begin
      RampRow:=0.5;
     end;

     aCanvas.DrawVectorPathShape(CurrentRecord.Shape,
                                 GradientKind,
                                 CurrentRecord.GradientMatrix,
                                 TpvVector2.InlineableCreate(CurrentRecord.GradientFocus.x,
                                                             CurrentRecord.GradientFocus.y),
                                 GradientSpreadMode,
                                 RampRow);

    end;
    else {TpvVectorGraphicsRasterizerRecorder.TFillKind.Solid:}begin
     aCanvas.DrawVectorPathShape(CurrentRecord.Shape);
    end;
   end;

  end;

 finally

  // The shapes collected above are only turned into a draw call on the next flush, and that
  // reads the buffer pool of the canvas to find the descriptor set they are to be drawn with.
  // Putting the old pool back first would leave them without one, so they go out now.
  aCanvas.Flush;

  aCanvas.Color:=OldColor;
  aCanvas.Texture:=OldTexture;
  aCanvas.VectorPathBufferPool:=OldBufferPool;
  aCanvas.VectorPathCoverageWidening:=OldCoverageWidening;

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
