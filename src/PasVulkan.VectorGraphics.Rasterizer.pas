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
unit PasVulkan.VectorGraphics.Rasterizer;
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

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasMP,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.VectorPath;

const // Subpixel precision of the coordinates that reach the rasterizer. Eight bits mean that the
      // rasterizer works on a 1/256th-of-a-pixel grid, which is what the analytical coverage
      // computation was written for.
      pvVectorGraphicsPixelBits=8;
      pvVectorGraphicsPixelFactor=1 shl pvVectorGraphicsPixelBits;
      pvVectorGraphicsHalfPixel=1 shl (pvVectorGraphicsPixelBits-1);
      pvVectorGraphicsOnePixel=1 shl pvVectorGraphicsPixelBits;
      pvVectorGraphicsPixelMask=(1 shl pvVectorGraphicsPixelBits)-1;

type EpvVectorGraphicsRasterizer=class(Exception);

     // Color in the $aarrggbb layout, so blue in the lowest byte and alpha in the highest one,
     // which on a little endian machine is BGRA in memory and therefore maps onto
     // VK_FORMAT_B8G8R8A8_UNORM without a swizzle. ConvertToRGBA8 exists for the other case.
     PpvVectorGraphicsColor=^TpvVectorGraphicsColor;
     TpvVectorGraphicsColor=TpvUInt32;

     TpvVectorGraphicsColorArray=array of TpvVectorGraphicsColor;

     PpvVectorGraphicsRawColorArray=^TpvVectorGraphicsRawColorArray;
     TpvVectorGraphicsRawColorArray=array[0..65535] of TpvVectorGraphicsColor;

     PpvVectorGraphicsRawByteArray=^TpvVectorGraphicsRawByteArray;
     TpvVectorGraphicsRawByteArray=array[0..65535] of TpvUInt8;

     { TpvVectorGraphicsRGBA }

     TpvVectorGraphicsRGBA=record
      public
       constructor Create(const aR,aG,aB,aA:TpvUInt8);
       class function CreateFromColor(const aColor:TpvVectorGraphicsColor):TpvVectorGraphicsRGBA; static; {$ifdef CANINLINE}inline;{$endif}
       function ToColor:TpvVectorGraphicsColor; {$ifdef CANINLINE}inline;{$endif}
      public
       b:TpvUInt8;
       g:TpvUInt8;
       r:TpvUInt8;
       a:TpvUInt8;
     end;

     PpvVectorGraphicsRGBA=^TpvVectorGraphicsRGBA;

     // Which arithmetic the span blending uses. Integer is bit-for-bit what the old BeRoVector
     // software renderer did, so output can be compared against it; Linear converts to linear
     // light, blends there and converts back, which is correct but no longer comparable.
     PpvVectorGraphicsColorSpaceMode=^TpvVectorGraphicsColorSpaceMode;
     TpvVectorGraphicsColorSpaceMode=
      (
       Integer=0,
       Linear
      );

     { TpvVectorGraphicsByteMulTable }

     // 256x256 table of (a*b)/255, which the old renderer used throughout and which this port
     // keeps, both for speed and so that the integer path rounds identically.
     TpvVectorGraphicsByteMulTable=array[TpvUInt8,TpvUInt8] of TpvUInt8;

     TpvVectorGraphicsGammaLookUpTable=array[TpvUInt8] of TpvUInt8;

     { TpvVectorGraphicsPixelBuffer }

     // The destination of a rasterization. Holds the $aarrggbb pixels and, when the color space
     // mode asks for it, a parallel buffer of linear light values that the blending works on.
     TpvVectorGraphicsPixelBuffer=class
      private
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fColorSpaceMode:TpvVectorGraphicsColorSpaceMode;
       fPixels:TpvVectorGraphicsColorArray;
       fLinearPixels:TpvFloatDynamicArray; // four consecutive floats per pixel, r,g,b,a
       fOwnsPixels:boolean;
       fExternalPixels:PpvVectorGraphicsRawColorArray;
       procedure SetColorSpaceMode(const aColorSpaceMode:TpvVectorGraphicsColorSpaceMode);
       function GetPixelPointer:PpvVectorGraphicsRawColorArray; {$ifdef CANINLINE}inline;{$endif}
      public
       constructor Create(const aWidth:TpvInt32=0;
                          const aHeight:TpvInt32=0;
                          const aColorSpaceMode:TpvVectorGraphicsColorSpaceMode=TpvVectorGraphicsColorSpaceMode.Integer); reintroduce;
       // Wraps memory that somebody else owns, for instance a mapped Vulkan staging buffer. The
       // buffer must stay alive and keep its size for as long as this object uses it.
       constructor CreateFromExternalMemory(const aPixels:pointer;const aWidth,aHeight:TpvInt32;
                                            const aColorSpaceMode:TpvVectorGraphicsColorSpaceMode=TpvVectorGraphicsColorSpaceMode.Integer); reintroduce;
       destructor Destroy; override;
       procedure Resize(const aWidth,aHeight:TpvInt32);
       procedure Clear(const aColor:TpvVectorGraphicsColor=0);
       function GetPixel(const aX,aY:TpvInt32):TpvVectorGraphicsColor;
       procedure SetPixel(const aX,aY:TpvInt32;const aColor:TpvVectorGraphicsColor);
       // Writes the pixels out as R8G8B8A8, so with the red and blue bytes swapped relative to
       // the internal layout. aDestination must have room for Width*Height*4 bytes.
       procedure ConvertToRGBA8(const aDestination:pointer);
       // Blends aSource over this buffer with the given alpha, which is how the off-image group
       // opacity of the scene graph gets composited.
       procedure BlendFrom(const aSource:TpvVectorGraphicsPixelBuffer;const aAlpha:TpvUInt8);
       // Copies the linear light buffer back into the integer pixels, which the Linear color
       // space mode needs before the result is read or uploaded.
       procedure FlushLinearToIntegerPixels;
       // And the other way round, after somebody wrote integer pixels directly.
       procedure FlushIntegerToLinearPixels;
      public
       property PixelPointer:PpvVectorGraphicsRawColorArray read GetPixelPointer;
      published
       property Width:TpvInt32 read fWidth;
       property Height:TpvInt32 read fHeight;
       property ColorSpaceMode:TpvVectorGraphicsColorSpaceMode read fColorSpaceMode write SetColorSpaceMode;
     end;

     { TpvVectorGraphicsMask }

     // A full RGBA coverage buffer that multiplies into the paint color before blending, so it
     // serves both the alpha-only clip paths and the per-channel color masks of SVG.
     TpvVectorGraphicsMask=class
      private
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fPixels:TpvVectorGraphicsColorArray;
      public
       constructor Create(const aWidth:TpvInt32=0;const aHeight:TpvInt32=0); reintroduce;
       destructor Destroy; override;
       procedure Resize(const aWidth,aHeight:TpvInt32);
       procedure Clear(const aColor:TpvVectorGraphicsColor=0);
       procedure Assign(const aFrom:TpvVectorGraphicsMask);
       // Multiplies aWith into this mask channel by channel, which is how a nested clip path or
       // mask narrows the one already in effect.
       procedure IntersectWith(const aFrom:TpvVectorGraphicsMask);
       // Replaces every pixel by its luminance in all four channels, which is what the
       // luminance mask type of SVG asks for.
       procedure ConvertToLuminance;
       function GetPixel(const aX,aY:TpvInt32):TpvVectorGraphicsColor;
       function GetPixelPointer:PpvVectorGraphicsRawColorArray;
      public
       property PixelPointer:PpvVectorGraphicsRawColorArray read GetPixelPointer;
      published
       property Width:TpvInt32 read fWidth;
       property Height:TpvInt32 read fHeight;
     end;

     TpvVectorGraphicsMaskList=array of TpvVectorGraphicsMask;

     { TpvVectorGraphicsMaskStack }

     // Keeps the nested clip paths and masks of a scene graph. Pushing a mask intersects it with
     // whatever is already in effect, so the top of the stack is always the mask to use.
     TpvVectorGraphicsMaskStack=class
      private
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fMasks:TpvVectorGraphicsMaskList;
       fCount:TpvSizeInt;
       fFreeMasks:TpvVectorGraphicsMaskList;
       fCountFreeMasks:TpvSizeInt;
       function AcquireMask:TpvVectorGraphicsMask;
       procedure ReleaseMask(const aMask:TpvVectorGraphicsMask);
      public
       constructor Create(const aWidth:TpvInt32=0;const aHeight:TpvInt32=0); reintroduce;
       destructor Destroy; override;
       procedure Resize(const aWidth,aHeight:TpvInt32);
       procedure Clear;
       // Pushes aMask, after intersecting it with the current top, and returns the new top.
       function Push(const aMask:TpvVectorGraphicsMask):TpvVectorGraphicsMask;
       procedure Pop;
       function Top:TpvVectorGraphicsMask;
      published
       property Count:TpvSizeInt read fCount;
     end;

     TpvVectorGraphicsRasterizer=class;

     { TpvVectorGraphicsPaint }

     // A source of color for the pixels a rasterized shape covers. The base class answers a
     // single color everywhere; gradients, images and patterns override GetColorAt, and sources
     // that can do better than one pixel at a time override GetSpan too.
     TpvVectorGraphicsPaint=class
      private
       fColor:TpvVectorGraphicsColor;
      public
       constructor Create(const aColor:TpvVectorGraphicsColor=$ff000000); reintroduce; virtual;
       destructor Destroy; override;
       // aX and aY are in the subpixel fixed point grid of the rasterizer, not in whole pixels,
       // so that gradients and patterns can sample at the pixel center.
       function GetColorAt(const aX,aY:TpvInt32):TpvVectorGraphicsColor; virtual;
       procedure GetSpan(const aX,aY:TpvInt32;const aCount:TpvSizeInt;const aColors:PpvVectorGraphicsRawColorArray); virtual;
       // True when every pixel gets the same color, which lets the rasterizer take the much
       // cheaper solid span paths.
       function IsSolid:boolean; virtual;
      published
       property Color:TpvVectorGraphicsColor read fColor write fColor;
     end;

     { TpvVectorGraphicsRasterizer }

     // Base class of the software rasterizers. Takes a path as a stream of subpixel MoveTo and
     // LineTo commands, and on Render turns the accumulated coverage into blended spans.
     TpvVectorGraphicsRasterizer=class
      protected
       fPixelBuffer:TpvVectorGraphicsPixelBuffer;
       // The buffer this rasterizer created for itself, freed on destruction no matter which
       // buffer is currently active, so that handing another one in temporarily cannot leak it.
       fOwnedPixelBuffer:TpvVectorGraphicsPixelBuffer;
       fPaint:TpvVectorGraphicsPaint;
       fOwnsPaint:boolean;
       fSolidPaint:TpvVectorGraphicsPaint;
       fMask:TpvVectorGraphicsMask;
       fFillRule:TpvVectorPathFillRule;
       fGamma:TpvDouble;
       fGammaLookUpTable:TpvVectorGraphicsGammaLookUpTable;
       fFlipY:boolean;
       // One scratch buffer per worker thread, because the scanlines of a single Render may be
       // blended in parallel and a shared buffer would be a data race.
       fSpanColors:array of TpvVectorGraphicsColorArray;
       procedure SetGamma(const aGamma:TpvDouble);
       procedure UpdateSpanBuffers;
       procedure SetColor(const aColor:TpvVectorGraphicsColor);
       function GetColor:TpvVectorGraphicsColor;
       function GetWidth:TpvInt32; {$ifdef CANINLINE}inline;{$endif}
       function GetHeight:TpvInt32; {$ifdef CANINLINE}inline;{$endif}
       // Blends one horizontal run of aCount pixels at (aX,aY) with the given coverage, after
       // clipping it to the buffer and running it through the gamma table and the mask.
       // aThreadIndex picks the scratch buffer, so a subclass that blends scanlines in parallel
       // has to pass the index of the worker it is running on.
       procedure RenderSpanCoverage(const aY,aX,aCount,aCoverage:TpvInt32;const aThreadIndex:TpvInt32=0);
      public
       constructor Create(const aPixelBuffer:TpvVectorGraphicsPixelBuffer=nil); reintroduce; virtual;
       destructor Destroy; override;
       procedure Resize(const aWidth,aHeight:TpvInt32); virtual;
       procedure Clear(const aColor:TpvVectorGraphicsColor=0); virtual;
       // Throws away the accumulated path without drawing it.
       procedure Reset; virtual;
       // Path input in the subpixel fixed point grid, so a whole pixel is
       // pvVectorGraphicsPixelFactor units wide.
       procedure MoveTo(const aX,aY:TpvInt32); virtual;
       procedure LineTo(const aX,aY:TpvInt32); virtual;
       procedure Rectangle(const aX1,aY1,aX2,aY2:TpvInt32); virtual;
       procedure Quad(const aX1,aY1,aX2,aY2,aX3,aY3,aX4,aY4:TpvInt32); virtual;
       procedure Close; virtual;
       // Draws everything accumulated since the last Render or Reset, then resets.
       procedure Render; virtual;
       // Feeds an already flattened shape through MoveTo/LineTo/Close and renders it. Curved
       // segments are ignored, so the caller flattens first, where the target scale is known.
       // aUseShapeFillRule false keeps whatever fill rule is currently set instead of taking the
       // one the shape carries.
       procedure FillShape(const aShape:TpvVectorPathShape;const aMatrix:TpvVectorPathMatrix;
                           const aUseShapeFillRule:boolean=true);
       // Rasterizes aShape into aMask instead of into the pixel buffer, which is how clip paths
       // and masks are turned into coverage.
       procedure RasterizeShapeToMask(const aShape:TpvVectorPathShape;
                                      const aMatrix:TpvVectorPathMatrix;
                                      const aMask:TpvVectorGraphicsMask;
                                      const aFillRule:TpvVectorPathFillRule;
                                      const aColor:TpvVectorGraphicsColor=$ffffffff);
       // Sets the paint to use. The rasterizer does not take ownership.
       procedure SetPaint(const aPaint:TpvVectorGraphicsPaint);
       // Redirects drawing into another buffer and returns the one that was in effect, so the
       // caller can put it back. Ownership of neither buffer changes, which is what off-image
       // group compositing, masks and pattern tiles all need.
       function ReplacePixelBuffer(const aPixelBuffer:TpvVectorGraphicsPixelBuffer):TpvVectorGraphicsPixelBuffer;
      public
       property PixelBuffer:TpvVectorGraphicsPixelBuffer read fPixelBuffer;
       property GammaLookUpTable:TpvVectorGraphicsGammaLookUpTable read fGammaLookUpTable;
      published
       property Width:TpvInt32 read GetWidth;
       property Height:TpvInt32 read GetHeight;
       property Color:TpvVectorGraphicsColor read GetColor write SetColor;
       property Gamma:TpvDouble read fGamma write SetGamma;
       property FillRule:TpvVectorPathFillRule read fFillRule write fFillRule;
       property Mask:TpvVectorGraphicsMask read fMask write fMask;
       property Paint:TpvVectorGraphicsPaint read fPaint;
       property FlipY:boolean read fFlipY write fFlipY;
     end;

var pvVectorGraphicsByteMulTable:TpvVectorGraphicsByteMulTable;

// Converts a floating point coordinate into the subpixel fixed point grid of the rasterizer.
function pvVectorGraphicsConvertCoordinate(const aValue:TpvDouble):TpvInt32; {$ifdef CANINLINE}inline;{$endif}

function pvVectorGraphicsBlendColor(const aDestination,aSource:TpvVectorGraphicsColor):TpvVectorGraphicsColor; {$ifdef CANINLINE}inline;{$endif}

function pvVectorGraphicsModulateColor(const aColor:TpvVectorGraphicsColor;const aAlpha:TpvUInt8):TpvVectorGraphicsColor; {$ifdef CANINLINE}inline;{$endif}

function pvVectorGraphicsMultiplyColors(const aColor,aWith:TpvVectorGraphicsColor):TpvVectorGraphicsColor; {$ifdef CANINLINE}inline;{$endif}

function pvVectorGraphicsSRGBToLinear(const aValue:TpvFloat):TpvFloat; {$ifdef CANINLINE}inline;{$endif}

function pvVectorGraphicsLinearToSRGB(const aValue:TpvFloat):TpvFloat; {$ifdef CANINLINE}inline;{$endif}

implementation

var pvVectorGraphicsSRGBToLinearTable:array[TpvUInt8] of TpvFloat;

function pvVectorGraphicsConvertCoordinate(const aValue:TpvDouble):TpvInt32;
begin
 result:=round(aValue*pvVectorGraphicsPixelFactor);
end;

// Straight, non-premultiplied source-over blending, with the same rounding the old renderer
// used, so that the integer path stays comparable down to the last bit.
function pvVectorGraphicsBlendColor(const aDestination,aSource:TpvVectorGraphicsColor):TpvVectorGraphicsColor;
var SourceAlpha,DestinationAlpha:TpvUInt32;
begin

 SourceAlpha:=aSource shr 24;

 // The two fully transparent and fully opaque cases need no arithmetic at all.
 if SourceAlpha=0 then begin

  result:=aDestination;

 end else if SourceAlpha=255 then begin

  result:=aSource;

 end else begin

  // Red and blue are interpolated in one go and green in a second one, so that the two halves
  // never carry into each other.
  DestinationAlpha:=aDestination shr 24;
  result:=(((aDestination and TpvUInt32($00ff00ff))+
            (((((aSource and TpvUInt32($00ff00ff))-(aDestination and TpvUInt32($00ff00ff)))*SourceAlpha)+TpvUInt32($00800080)) shr 8)) and TpvUInt32($00ff00ff)) or
          (((aDestination and TpvUInt32($0000ff00))+
            (((((aSource and TpvUInt32($0000ff00))-(aDestination and TpvUInt32($0000ff00)))*SourceAlpha)+TpvUInt32($00008000)) shr 8)) and TpvUInt32($0000ff00)) or
          (TpvUInt32((SourceAlpha+DestinationAlpha)-(((SourceAlpha*DestinationAlpha)+255) shr 8)) shl 24);

 end;

end;

function pvVectorGraphicsModulateColor(const aColor:TpvVectorGraphicsColor;const aAlpha:TpvUInt8):TpvVectorGraphicsColor;
begin
 result:=(TpvVectorGraphicsColor(pvVectorGraphicsByteMulTable[(aColor shr 24) and $ff,aAlpha]) shl 24) or
         (aColor and TpvUInt32($00ffffff));
end;

function pvVectorGraphicsMultiplyColors(const aColor,aWith:TpvVectorGraphicsColor):TpvVectorGraphicsColor;
begin
 result:=(TpvVectorGraphicsColor(pvVectorGraphicsByteMulTable[(aColor shr 24) and $ff,(aWith shr 24) and $ff]) shl 24) or
         (TpvVectorGraphicsColor(pvVectorGraphicsByteMulTable[(aColor shr 16) and $ff,(aWith shr 16) and $ff]) shl 16) or
         (TpvVectorGraphicsColor(pvVectorGraphicsByteMulTable[(aColor shr 8) and $ff,(aWith shr 8) and $ff]) shl 8) or
         TpvVectorGraphicsColor(pvVectorGraphicsByteMulTable[aColor and $ff,aWith and $ff]);
end;

function pvVectorGraphicsSRGBToLinear(const aValue:TpvFloat):TpvFloat;
begin
 if aValue<=0.04045 then begin
  result:=aValue/12.92;
 end else begin
  result:=Power((aValue+0.055)/1.055,2.4);
 end;
end;

function pvVectorGraphicsLinearToSRGB(const aValue:TpvFloat):TpvFloat;
begin
 if aValue<=0.0031308 then begin
  result:=aValue*12.92;
 end else begin
  result:=(1.055*Power(aValue,1.0/2.4))-0.055;
 end;
end;

{ TpvVectorGraphicsRGBA }

constructor TpvVectorGraphicsRGBA.Create(const aR,aG,aB,aA:TpvUInt8);
begin
 r:=aR;
 g:=aG;
 b:=aB;
 a:=aA;
end;

class function TpvVectorGraphicsRGBA.CreateFromColor(const aColor:TpvVectorGraphicsColor):TpvVectorGraphicsRGBA;
begin
 result.b:=aColor and $ff;
 result.g:=(aColor shr 8) and $ff;
 result.r:=(aColor shr 16) and $ff;
 result.a:=(aColor shr 24) and $ff;
end;

function TpvVectorGraphicsRGBA.ToColor:TpvVectorGraphicsColor;
begin
 result:=(TpvVectorGraphicsColor(a) shl 24) or
         (TpvVectorGraphicsColor(r) shl 16) or
         (TpvVectorGraphicsColor(g) shl 8) or
         TpvVectorGraphicsColor(b);
end;

{ TpvVectorGraphicsPixelBuffer }

constructor TpvVectorGraphicsPixelBuffer.Create(const aWidth,aHeight:TpvInt32;
                                                const aColorSpaceMode:TpvVectorGraphicsColorSpaceMode);
begin

 inherited Create;

 fWidth:=0;
 fHeight:=0;
 fColorSpaceMode:=aColorSpaceMode;

 fPixels:=nil;
 fLinearPixels:=nil;

 fOwnsPixels:=true;
 fExternalPixels:=nil;

 // The zero size above makes this the one place where the storage gets allocated.
 Resize(aWidth,aHeight);

end;

constructor TpvVectorGraphicsPixelBuffer.CreateFromExternalMemory(const aPixels:pointer;const aWidth,aHeight:TpvInt32;
                                                                  const aColorSpaceMode:TpvVectorGraphicsColorSpaceMode);
begin

 inherited Create;

 fWidth:=aWidth;
 fHeight:=aHeight;
 fColorSpaceMode:=aColorSpaceMode;

 // The caller keeps ownership of the memory, so nothing is allocated and Resize is refused later.
 fPixels:=nil;
 fLinearPixels:=nil;
 fOwnsPixels:=false;
 fExternalPixels:=aPixels;

 // In linear mode the float shadow is seeded from whatever the foreign memory already holds.
 if fColorSpaceMode=TpvVectorGraphicsColorSpaceMode.Linear then begin
  SetLength(fLinearPixels,TpvSizeInt(fWidth)*TpvSizeInt(fHeight)*4);
  FlushIntegerToLinearPixels;
 end;

end;

destructor TpvVectorGraphicsPixelBuffer.Destroy;
begin

 fPixels:=nil;
 fLinearPixels:=nil;

 inherited Destroy;

end;

function TpvVectorGraphicsPixelBuffer.GetPixelPointer:PpvVectorGraphicsRawColorArray;
begin

 if fOwnsPixels then begin

  if length(fPixels)>0 then begin
   result:=@fPixels[0];
  end else begin
   result:=nil;
  end;

 end else begin

  result:=fExternalPixels;

 end;

end;

procedure TpvVectorGraphicsPixelBuffer.SetColorSpaceMode(const aColorSpaceMode:TpvVectorGraphicsColorSpaceMode);
begin

 if fColorSpaceMode<>aColorSpaceMode then begin

  // Leaving linear mode means the float buffer is the authoritative one and has to be written back.
  if fColorSpaceMode=TpvVectorGraphicsColorSpaceMode.Linear then begin
   FlushLinearToIntegerPixels;
  end;

  fColorSpaceMode:=aColorSpaceMode;

  // Entering linear mode means the float buffer has to be created and seeded instead.
  if fColorSpaceMode=TpvVectorGraphicsColorSpaceMode.Linear then begin
   SetLength(fLinearPixels,TpvSizeInt(fWidth)*TpvSizeInt(fHeight)*4);
   FlushIntegerToLinearPixels;
  end else begin
   fLinearPixels:=nil;
  end;

 end;

end;

procedure TpvVectorGraphicsPixelBuffer.Resize(const aWidth,aHeight:TpvInt32);
begin

 if (fWidth<>aWidth) or (fHeight<>aHeight) then begin

  // Foreign memory has a size the caller decided on, which this class must not change.
  if not fOwnsPixels then begin
   raise EpvVectorGraphicsRasterizer.Create('Cannot resize a pixel buffer that wraps external memory');
  end;

  fWidth:=aWidth;
  fHeight:=aHeight;

  // An empty size is a legal request and simply drops the storage.
  if (fWidth>0) and (fHeight>0) then begin

   SetLength(fPixels,TpvSizeInt(fWidth)*TpvSizeInt(fHeight));

   if fColorSpaceMode=TpvVectorGraphicsColorSpaceMode.Linear then begin
    SetLength(fLinearPixels,TpvSizeInt(fWidth)*TpvSizeInt(fHeight)*4);
   end else begin
    fLinearPixels:=nil;
   end;

  end else begin

   fPixels:=nil;
   fLinearPixels:=nil;

  end;

 end;

end;

procedure TpvVectorGraphicsPixelBuffer.Clear(const aColor:TpvVectorGraphicsColor);
var Index,Count:TpvSizeInt;
    Pixels:PpvVectorGraphicsRawColorArray;
    LinearRed,LinearGreen,LinearBlue,LinearAlpha:TpvFloat;
begin

 Count:=TpvSizeInt(fWidth)*TpvSizeInt(fHeight);
 if Count<=0 then begin
  exit;
 end;

 Pixels:=GetPixelPointer;
 if assigned(Pixels) then begin
  for Index:=0 to Count-1 do begin
   Pixels^[Index]:=aColor;
  end;
 end;

 // The linear buffer is kept in step, so that the two never disagree.
 if fColorSpaceMode=TpvVectorGraphicsColorSpaceMode.Linear then begin

  LinearBlue:=pvVectorGraphicsSRGBToLinearTable[aColor and $ff];
  LinearGreen:=pvVectorGraphicsSRGBToLinearTable[(aColor shr 8) and $ff];
  LinearRed:=pvVectorGraphicsSRGBToLinearTable[(aColor shr 16) and $ff];
  LinearAlpha:=((aColor shr 24) and $ff)/255.0;

  for Index:=0 to Count-1 do begin
   fLinearPixels[(Index*4)+0]:=LinearRed;
   fLinearPixels[(Index*4)+1]:=LinearGreen;
   fLinearPixels[(Index*4)+2]:=LinearBlue;
   fLinearPixels[(Index*4)+3]:=LinearAlpha;
  end;

 end;

end;

function TpvVectorGraphicsPixelBuffer.GetPixel(const aX,aY:TpvInt32):TpvVectorGraphicsColor;
var Pixels:PpvVectorGraphicsRawColorArray;
begin

 // Everything outside reads as fully transparent, which spares every caller its own clipping.
 if (aX>=0) and (aX<fWidth) and (aY>=0) and (aY<fHeight) then begin

  Pixels:=GetPixelPointer;
  if assigned(Pixels) then begin
   result:=Pixels^[(TpvSizeInt(aY)*TpvSizeInt(fWidth))+TpvSizeInt(aX)];
  end else begin
   result:=0;
  end;

 end else begin

  result:=0;

 end;

end;

procedure TpvVectorGraphicsPixelBuffer.SetPixel(const aX,aY:TpvInt32;const aColor:TpvVectorGraphicsColor);
var Pixels:PpvVectorGraphicsRawColorArray;
    Index:TpvSizeInt;
begin

 // Writes outside are dropped, for the same reason reads outside are answered with transparency.
 if (aX>=0) and (aX<fWidth) and (aY>=0) and (aY<fHeight) then begin

  Pixels:=GetPixelPointer;
  if assigned(Pixels) then begin

   Index:=(TpvSizeInt(aY)*TpvSizeInt(fWidth))+TpvSizeInt(aX);
   Pixels^[Index]:=aColor;

   // In linear mode the float shadow has to be kept in step, otherwise the next flush would
   // undo this write again.
   if fColorSpaceMode=TpvVectorGraphicsColorSpaceMode.Linear then begin
    fLinearPixels[(Index*4)+0]:=pvVectorGraphicsSRGBToLinearTable[(aColor shr 16) and $ff];
    fLinearPixels[(Index*4)+1]:=pvVectorGraphicsSRGBToLinearTable[(aColor shr 8) and $ff];
    fLinearPixels[(Index*4)+2]:=pvVectorGraphicsSRGBToLinearTable[aColor and $ff];
    fLinearPixels[(Index*4)+3]:=((aColor shr 24) and $ff)/255.0;
   end;

  end;

 end;

end;

procedure TpvVectorGraphicsPixelBuffer.ConvertToRGBA8(const aDestination:pointer);
var Index,Count:TpvSizeInt;
    Source:PpvVectorGraphicsRawColorArray;
    Destination:PpvVectorGraphicsRawColorArray;
    Color:TpvVectorGraphicsColor;
begin

 Count:=TpvSizeInt(fWidth)*TpvSizeInt(fHeight);
 if (Count<=0) or not assigned(aDestination) then begin
  exit;
 end;

 // The integer pixels are the ones being read, so in linear mode they have to be current first.
 if fColorSpaceMode=TpvVectorGraphicsColorSpaceMode.Linear then begin
  FlushLinearToIntegerPixels;
 end;

 Source:=GetPixelPointer;
 Destination:=aDestination;

 // Only red and blue change places, alpha and green stay where they are.
 if assigned(Source) then begin
  for Index:=0 to Count-1 do begin
   Color:=Source^[Index];
   Destination^[Index]:=(Color and TpvUInt32($ff00ff00)) or
                        ((Color and TpvUInt32($00ff0000)) shr 16) or
                        ((Color and TpvUInt32($000000ff)) shl 16);
  end;
 end;

end;

procedure TpvVectorGraphicsPixelBuffer.BlendFrom(const aSource:TpvVectorGraphicsPixelBuffer;const aAlpha:TpvUInt8);
var Index,Count:TpvSizeInt;
    SourcePixels,DestinationPixels:PpvVectorGraphicsRawColorArray;
begin

 if not assigned(aSource) then begin
  exit;
 end;

 if (aSource.fWidth<>fWidth) or (aSource.fHeight<>fHeight) then begin
  raise EpvVectorGraphicsRasterizer.Create('Pixel buffer size mismatch while blending');
 end;

 Count:=TpvSizeInt(fWidth)*TpvSizeInt(fHeight);
 if Count<=0 then begin
  exit;
 end;

 // Blending happens on the integer pixels, so a linear source has to be brought up to date.
 if aSource.fColorSpaceMode=TpvVectorGraphicsColorSpaceMode.Linear then begin
  aSource.FlushLinearToIntegerPixels;
 end;

 SourcePixels:=aSource.GetPixelPointer;
 DestinationPixels:=GetPixelPointer;
 if not (assigned(SourcePixels) and assigned(DestinationPixels)) then begin
  exit;
 end;

 // Full opacity is a plain copy, which is what the off-image passes start out with.
 if aAlpha=255 then begin
  for Index:=0 to Count-1 do begin
   DestinationPixels^[Index]:=SourcePixels^[Index];
  end;
 end else begin
  for Index:=0 to Count-1 do begin
   DestinationPixels^[Index]:=pvVectorGraphicsBlendColor(DestinationPixels^[Index],
                                                         pvVectorGraphicsModulateColor(SourcePixels^[Index],aAlpha));
  end;
 end;

 if fColorSpaceMode=TpvVectorGraphicsColorSpaceMode.Linear then begin
  FlushIntegerToLinearPixels;
 end;

end;

procedure TpvVectorGraphicsPixelBuffer.FlushLinearToIntegerPixels;
var Index,Count:TpvSizeInt;
    Pixels:PpvVectorGraphicsRawColorArray;
    Value:TpvFloat;
    Channel:TpvInt32;
    Color:TpvVectorGraphicsColor;
    Shifts:array[0..3] of TpvInt32;
begin

 if fColorSpaceMode<>TpvVectorGraphicsColorSpaceMode.Linear then begin
  exit;
 end;

 Count:=TpvSizeInt(fWidth)*TpvSizeInt(fHeight);
 Pixels:=GetPixelPointer;
 if (Count<=0) or not assigned(Pixels) then begin
  exit;
 end;

 // Where each channel lands in the $aarrggbb word.
 Shifts[0]:=16;
 Shifts[1]:=8;
 Shifts[2]:=0;
 Shifts[3]:=24;

 // The three color channels go back through the transfer function, alpha stays linear.
 for Index:=0 to Count-1 do begin

  Color:=0;
  for Channel:=0 to 3 do begin

   Value:=fLinearPixels[(Index*4)+Channel];
   if Channel<3 then begin
    Value:=pvVectorGraphicsLinearToSRGB(Value);
   end;

   if Value<=0.0 then begin
    Value:=0.0;
   end else if Value>=1.0 then begin
    Value:=1.0;
   end;

   Color:=Color or (TpvVectorGraphicsColor(round(Value*255.0)) shl Shifts[Channel]);

  end;

  Pixels^[Index]:=Color;

 end;

end;

procedure TpvVectorGraphicsPixelBuffer.FlushIntegerToLinearPixels;
var Index,Count:TpvSizeInt;
    Pixels:PpvVectorGraphicsRawColorArray;
    Color:TpvVectorGraphicsColor;
begin

 if fColorSpaceMode<>TpvVectorGraphicsColorSpaceMode.Linear then begin
  exit;
 end;

 Count:=TpvSizeInt(fWidth)*TpvSizeInt(fHeight);
 Pixels:=GetPixelPointer;
 if (Count<=0) or not assigned(Pixels) then begin
  exit;
 end;

 // External memory can have been handed over before the float buffer existed.
 if length(fLinearPixels)<>(Count*4) then begin
  SetLength(fLinearPixels,Count*4);
 end;

 // The table undoes the sRGB transfer curve, alpha is linear to begin with.
 for Index:=0 to Count-1 do begin
  Color:=Pixels^[Index];
  fLinearPixels[(Index*4)+0]:=pvVectorGraphicsSRGBToLinearTable[(Color shr 16) and $ff];
  fLinearPixels[(Index*4)+1]:=pvVectorGraphicsSRGBToLinearTable[(Color shr 8) and $ff];
  fLinearPixels[(Index*4)+2]:=pvVectorGraphicsSRGBToLinearTable[Color and $ff];
  fLinearPixels[(Index*4)+3]:=((Color shr 24) and $ff)/255.0;
 end;

end;

{ TpvVectorGraphicsMask }

constructor TpvVectorGraphicsMask.Create(const aWidth,aHeight:TpvInt32);
begin
 inherited Create;
 fWidth:=0;
 fHeight:=0;
 fPixels:=nil;
 Resize(aWidth,aHeight);
end;

destructor TpvVectorGraphicsMask.Destroy;
begin
 fPixels:=nil;
 inherited Destroy;
end;

procedure TpvVectorGraphicsMask.Resize(const aWidth,aHeight:TpvInt32);
begin

 if (fWidth<>aWidth) or (fHeight<>aHeight) then begin

  fWidth:=aWidth;
  fHeight:=aHeight;

  // The old contents are not carried over, every user clears the mask before filling it anyway.
  if (fWidth>0) and (fHeight>0) then begin
   SetLength(fPixels,TpvSizeInt(fWidth)*TpvSizeInt(fHeight));
  end else begin
   fPixels:=nil;
  end;

 end;

end;

procedure TpvVectorGraphicsMask.Clear(const aColor:TpvVectorGraphicsColor);
var Index,Count:TpvSizeInt;
begin
 Count:=length(fPixels);
 for Index:=0 to Count-1 do begin
  fPixels[Index]:=aColor;
 end;
end;

procedure TpvVectorGraphicsMask.Assign(const aFrom:TpvVectorGraphicsMask);
var Index,Count:TpvSizeInt;
begin

 if not assigned(aFrom) then begin
  exit;
 end;

 Resize(aFrom.fWidth,aFrom.fHeight);

 Count:=length(fPixels);
 for Index:=0 to Count-1 do begin
  fPixels[Index]:=aFrom.fPixels[Index];
 end;

end;

procedure TpvVectorGraphicsMask.IntersectWith(const aFrom:TpvVectorGraphicsMask);
var Index,Count:TpvSizeInt;
begin

 if not assigned(aFrom) then begin
  exit;
 end;

 // Unlike Assign this one does not resize, since intersecting masks of different sizes has no
 // meaningful answer.
 if (aFrom.fWidth<>fWidth) or (aFrom.fHeight<>fHeight) then begin
  raise EpvVectorGraphicsRasterizer.Create('Mask size mismatch while intersecting');
 end;

 // Nested clip paths and masks multiply, which is what makes the stack work.
 Count:=length(fPixels);
 for Index:=0 to Count-1 do begin
  fPixels[Index]:=pvVectorGraphicsMultiplyColors(fPixels[Index],aFrom.fPixels[Index]);
 end;

end;

procedure TpvVectorGraphicsMask.ConvertToLuminance;
var Index,Count:TpvSizeInt;
    Color,Luminance:TpvUInt32;
begin

 Count:=length(fPixels);
 for Index:=0 to Count-1 do begin

  Color:=fPixels[Index];

  // The coefficients are the ones the SVG masking specification gives for linearRGB luminance,
  // scaled to 16 bit so that the whole thing stays in integers.
  Luminance:=((((Color shr 16) and $ff)*13933)+
              (((Color shr 8) and $ff)*46871)+
              ((Color and $ff)*4732)) shr 16;
  if Luminance>255 then begin
   Luminance:=255;
  end;

  // A transparent pixel masks nothing, whatever its color says.
  Luminance:=pvVectorGraphicsByteMulTable[Luminance,(Color shr 24) and $ff];

  fPixels[Index]:=(Luminance shl 24) or (Luminance shl 16) or (Luminance shl 8) or Luminance;

 end;

end;

function TpvVectorGraphicsMask.GetPixel(const aX,aY:TpvInt32):TpvVectorGraphicsColor;
begin
 if (aX>=0) and (aX<fWidth) and (aY>=0) and (aY<fHeight) then begin
  result:=fPixels[(TpvSizeInt(aY)*TpvSizeInt(fWidth))+TpvSizeInt(aX)];
 end else begin
  result:=0;
 end;
end;

function TpvVectorGraphicsMask.GetPixelPointer:PpvVectorGraphicsRawColorArray;
begin
 if length(fPixels)>0 then begin
  result:=@fPixels[0];
 end else begin
  result:=nil;
 end;
end;

{ TpvVectorGraphicsMaskStack }

constructor TpvVectorGraphicsMaskStack.Create(const aWidth,aHeight:TpvInt32);
begin

 inherited Create;

 fWidth:=aWidth;
 fHeight:=aHeight;

 fMasks:=nil;
 fCount:=0;

 // Popped masks land here instead of being freed, since a deeply nested document otherwise
 // allocates and frees a full-size buffer per group.
 fFreeMasks:=nil;
 fCountFreeMasks:=0;

end;

destructor TpvVectorGraphicsMaskStack.Destroy;
var Index:TpvSizeInt;
begin

 // Both the live stack and the pool own their masks.
 for Index:=0 to length(fMasks)-1 do begin
  FreeAndNil(fMasks[Index]);
 end;
 fMasks:=nil;

 for Index:=0 to length(fFreeMasks)-1 do begin
  FreeAndNil(fFreeMasks[Index]);
 end;
 fFreeMasks:=nil;

 inherited Destroy;

end;

function TpvVectorGraphicsMaskStack.AcquireMask:TpvVectorGraphicsMask;
begin

 // A pooled mask still carries the size it had when it was released, hence the resize.
 if fCountFreeMasks>0 then begin

  dec(fCountFreeMasks);
  result:=fFreeMasks[fCountFreeMasks];
  fFreeMasks[fCountFreeMasks]:=nil;
  result.Resize(fWidth,fHeight);

 end else begin

  result:=TpvVectorGraphicsMask.Create(fWidth,fHeight);

 end;

end;

procedure TpvVectorGraphicsMaskStack.ReleaseMask(const aMask:TpvVectorGraphicsMask);
begin

 if not assigned(aMask) then begin
  exit;
 end;

 if fCountFreeMasks>=length(fFreeMasks) then begin
  SetLength(fFreeMasks,(fCountFreeMasks+1)*2);
 end;

 fFreeMasks[fCountFreeMasks]:=aMask;
 inc(fCountFreeMasks);

end;

procedure TpvVectorGraphicsMaskStack.Resize(const aWidth,aHeight:TpvInt32);
begin

 // Whatever is on the stack belongs to the old size and cannot be kept.
 if (fWidth<>aWidth) or (fHeight<>aHeight) then begin
  fWidth:=aWidth;
  fHeight:=aHeight;
  Clear;
 end;

end;

procedure TpvVectorGraphicsMaskStack.Clear;
begin
 while fCount>0 do begin
  Pop;
 end;
end;

function TpvVectorGraphicsMaskStack.Push(const aMask:TpvVectorGraphicsMask):TpvVectorGraphicsMask;
begin

 result:=AcquireMask;
 result.Assign(aMask);
 result.Resize(fWidth,fHeight);

 // The new entry already carries everything the enclosing groups clip away, so drawing only ever
 // has to look at the topmost mask.
 if fCount>0 then begin
  result.IntersectWith(fMasks[fCount-1]);
 end;

 if fCount>=length(fMasks) then begin
  SetLength(fMasks,(fCount+1)*2);
 end;

 fMasks[fCount]:=result;
 inc(fCount);

end;

procedure TpvVectorGraphicsMaskStack.Pop;
begin

 // The mask goes back into the pool rather than being freed.
 if fCount>0 then begin
  dec(fCount);
  ReleaseMask(fMasks[fCount]);
  fMasks[fCount]:=nil;
 end;

end;

function TpvVectorGraphicsMaskStack.Top:TpvVectorGraphicsMask;
begin
 if fCount>0 then begin
  result:=fMasks[fCount-1];
 end else begin
  result:=nil;
 end;
end;

{ TpvVectorGraphicsPaint }

constructor TpvVectorGraphicsPaint.Create(const aColor:TpvVectorGraphicsColor);
begin
 inherited Create;
 fColor:=aColor;
end;

destructor TpvVectorGraphicsPaint.Destroy;
begin
 inherited Destroy;
end;

function TpvVectorGraphicsPaint.GetColorAt(const aX,aY:TpvInt32):TpvVectorGraphicsColor;
begin
 result:=fColor;
end;

procedure TpvVectorGraphicsPaint.GetSpan(const aX,aY:TpvInt32;const aCount:TpvSizeInt;const aColors:PpvVectorGraphicsRawColorArray);
var Index:TpvSizeInt;
begin
 for Index:=0 to aCount-1 do begin
  aColors^[Index]:=GetColorAt(aX+TpvInt32(Index*pvVectorGraphicsPixelFactor),aY);
 end;
end;

function TpvVectorGraphicsPaint.IsSolid:boolean;
begin
 result:=true;
end;

{ TpvVectorGraphicsRasterizer }

constructor TpvVectorGraphicsRasterizer.Create(const aPixelBuffer:TpvVectorGraphicsPixelBuffer);
begin

 inherited Create;

 // A buffer handed in stays the caller's, only a self-made one is tracked for freeing.
 if assigned(aPixelBuffer) then begin
  fPixelBuffer:=aPixelBuffer;
  fOwnedPixelBuffer:=nil;
 end else begin
  fOwnedPixelBuffer:=TpvVectorGraphicsPixelBuffer.Create;
  fPixelBuffer:=fOwnedPixelBuffer;
 end;

 // The solid paint is the fallback whenever no gradient or image paint is set.
 fSolidPaint:=TpvVectorGraphicsPaint.Create($ff000000);
 fPaint:=fSolidPaint;
 fOwnsPaint:=false;

 fMask:=nil;
 fFillRule:=TpvVectorPathFillRule.NonZero;
 fFlipY:=false;
 fSpanColors:=nil;

 // The zero above makes SetGamma actually build the table instead of taking its early exit.
 fGamma:=0.0;
 SetGamma(1.0);

 UpdateSpanBuffers;

end;

destructor TpvVectorGraphicsRasterizer.Destroy;
begin

 // fPixelBuffer is only a reference, the owned one is the one that gets freed.
 fPixelBuffer:=nil;
 FreeAndNil(fOwnedPixelBuffer);

 FreeAndNil(fSolidPaint);
 fPaint:=nil;

 fSpanColors:=nil;

 inherited Destroy;

end;

procedure TpvVectorGraphicsRasterizer.SetGamma(const aGamma:TpvDouble);
const OneOver255=1.0/255.0;
var Index,Value:TpvInt32;
begin

 if SameValue(fGamma,aGamma) then begin
  exit;
 end;

 fGamma:=aGamma;

 // Coverage goes through this table before it modulates the paint alpha, which is what lets a
 // document be rendered darker or lighter at the edges without touching the geometry.
 for Index:=Low(TpvVectorGraphicsGammaLookUpTable) to High(TpvVectorGraphicsGammaLookUpTable) do begin

  if Index=0 then begin
   Value:=0;
  end else if aGamma<1e-8 then begin
   Value:=255;
  end else begin
   Value:=trunc(Power(Index*OneOver255,aGamma)*255.0);
   if Value<0 then begin
    Value:=0;
   end else if Value>255 then begin
    Value:=255;
   end;
  end;

  fGammaLookUpTable[Index]:=Value;

 end;

end;

procedure TpvVectorGraphicsRasterizer.SetColor(const aColor:TpvVectorGraphicsColor);
begin
 fSolidPaint.fColor:=aColor;
 fPaint:=fSolidPaint;
end;

function TpvVectorGraphicsRasterizer.GetColor:TpvVectorGraphicsColor;
begin
 result:=fPaint.fColor;
end;

function TpvVectorGraphicsRasterizer.GetWidth:TpvInt32;
begin
 result:=fPixelBuffer.fWidth;
end;

function TpvVectorGraphicsRasterizer.GetHeight:TpvInt32;
begin
 result:=fPixelBuffer.fHeight;
end;

procedure TpvVectorGraphicsRasterizer.SetPaint(const aPaint:TpvVectorGraphicsPaint);
begin
 if assigned(aPaint) then begin
  fPaint:=aPaint;
 end else begin
  fPaint:=fSolidPaint;
 end;
end;

function TpvVectorGraphicsRasterizer.ReplacePixelBuffer(const aPixelBuffer:TpvVectorGraphicsPixelBuffer):TpvVectorGraphicsPixelBuffer;
begin

 result:=fPixelBuffer;

 if assigned(aPixelBuffer) and (aPixelBuffer<>fPixelBuffer) then begin

  // Anything half drawn belongs to the old buffer and must not spill into the new one.
  Reset;

  fPixelBuffer:=aPixelBuffer;

  // The scanline bookkeeping of a subclass is sized to the old buffer, so it gets resized here.
  Resize(aPixelBuffer.Width,aPixelBuffer.Height);

  // Resize only does anything when the size actually changed, but the scratch buffers have to
  // fit the new width either way.
  UpdateSpanBuffers;

 end;

end;

procedure TpvVectorGraphicsRasterizer.UpdateSpanBuffers;
var Index,Count,Width_:TpvSizeInt;
begin

 // One more than the worker count, since the thread that invokes a parallel job takes part in
 // it as well and gets index zero.
 Count:=1;
 if assigned(TPasMP.GetGlobalInstance) then begin
  Count:=Max(1,TPasMP.GetGlobalInstance.CountJobWorkerThreads+1);
 end;

 if Length(fSpanColors)<Count then begin
  SetLength(fSpanColors,Count);
 end;

 Width_:=Max(1,fPixelBuffer.fWidth);

 // Sized up front, so that no allocation ever happens inside the parallel blending.
 for Index:=0 to Length(fSpanColors)-1 do begin
  if Length(fSpanColors[Index])<Width_ then begin
   SetLength(fSpanColors[Index],Width_);
  end;
 end;

end;

procedure TpvVectorGraphicsRasterizer.Resize(const aWidth,aHeight:TpvInt32);
begin
 fPixelBuffer.Resize(aWidth,aHeight);
 UpdateSpanBuffers;
end;

procedure TpvVectorGraphicsRasterizer.Clear(const aColor:TpvVectorGraphicsColor);
begin
 fPixelBuffer.Clear(aColor);
end;

procedure TpvVectorGraphicsRasterizer.Reset;
begin
end;

procedure TpvVectorGraphicsRasterizer.MoveTo(const aX,aY:TpvInt32);
begin
end;

procedure TpvVectorGraphicsRasterizer.LineTo(const aX,aY:TpvInt32);
begin
end;

procedure TpvVectorGraphicsRasterizer.Rectangle(const aX1,aY1,aX2,aY2:TpvInt32);
begin
 MoveTo(aX1,aY1);
 LineTo(aX2,aY1);
 LineTo(aX2,aY2);
 LineTo(aX1,aY2);
 LineTo(aX1,aY1);
end;

procedure TpvVectorGraphicsRasterizer.Quad(const aX1,aY1,aX2,aY2,aX3,aY3,aX4,aY4:TpvInt32);
begin
 MoveTo(aX1,aY1);
 LineTo(aX2,aY2);
 LineTo(aX3,aY3);
 LineTo(aX4,aY4);
 LineTo(aX1,aY1);
end;

procedure TpvVectorGraphicsRasterizer.Close;
begin
end;

procedure TpvVectorGraphicsRasterizer.Render;
begin
end;

procedure TpvVectorGraphicsRasterizer.RenderSpanCoverage(const aY,aX,aCount,aCoverage:TpvInt32;const aThreadIndex:TpvInt32);
var X,Count,Index,ThreadIndex:TpvInt32;
    Row:TpvInt32;
    Alpha:TpvUInt8;
    Pixels:PpvVectorGraphicsRawColorArray;
    SpanColors:PpvVectorGraphicsRawColorArray;
    Color,SourceColor:TpvVectorGraphicsColor;
    IsSolidPaint:boolean;
    LinearSource:array[0..3] of TpvFloat;
    LinearIndex:TpvSizeInt;
    SourceAlpha,InverseSourceAlpha,ResultAlpha:TpvFloat;
    Channel:TpvInt32;
begin

 if (aCoverage=0) or (aY<0) or (aY>=fPixelBuffer.fHeight) then begin
  exit;
 end;

 // Clip the run to the buffer.
 X:=aX;
 Count:=aCount;
 if ((X+Count)<=0) or (X>=fPixelBuffer.fWidth) then begin
  exit;
 end;
 if X<0 then begin
  inc(Count,X);
  if Count<=0 then begin
   exit;
  end;
  X:=0;
 end;
 if (X+Count)>fPixelBuffer.fWidth then begin
  Count:=fPixelBuffer.fWidth-X;
 end;
 if Count<=0 then begin
  exit;
 end;

 Pixels:=fPixelBuffer.PixelPointer;
 if not assigned(Pixels) then begin
  exit;
 end;

 // Coverage becomes opacity through the gamma table.
 Alpha:=fGammaLookUpTable[aCoverage and $ff];
 if Alpha=0 then begin
  exit;
 end;

 if fFlipY then begin
  Row:=fPixelBuffer.fHeight-(aY+1);
 end else begin
  Row:=aY;
 end;

 // A paint that varies across the run is asked for the whole span in one go, into the scratch
 // buffer belonging to this worker.
 IsSolidPaint:=fPaint.IsSolid;
 if IsSolidPaint then begin

  SpanColors:=nil;

 end else begin

  ThreadIndex:=aThreadIndex;
  if (ThreadIndex<0) or (ThreadIndex>=Length(fSpanColors)) then begin
   ThreadIndex:=0;
  end;
  if Length(fSpanColors)=0 then begin
   UpdateSpanBuffers;
  end;
  if Length(fSpanColors[ThreadIndex])<Count then begin
   SetLength(fSpanColors[ThreadIndex],Count);
  end;

  SpanColors:=@fSpanColors[ThreadIndex][0];
  fPaint.GetSpan((X*pvVectorGraphicsPixelFactor)+pvVectorGraphicsHalfPixel,
                 (aY*pvVectorGraphicsPixelFactor)+pvVectorGraphicsHalfPixel,
                 Count,
                 SpanColors);

 end;

 if fPixelBuffer.fColorSpaceMode=TpvVectorGraphicsColorSpaceMode.Linear then begin

  // Linear light path, which blends correctly but no longer matches the old renderer.
  for Index:=0 to Count-1 do begin

   if IsSolidPaint then begin
    SourceColor:=fPaint.fColor;
   end else begin
    SourceColor:=SpanColors^[Index];
   end;

   SourceColor:=pvVectorGraphicsModulateColor(SourceColor,Alpha);
   if assigned(fMask) then begin
    SourceColor:=pvVectorGraphicsMultiplyColors(SourceColor,fMask.GetPixel(X+Index,aY));
   end;

   SourceAlpha:=((SourceColor shr 24) and $ff)/255.0;
   if SourceAlpha<=0.0 then begin
    continue;
   end;

   LinearSource[0]:=pvVectorGraphicsSRGBToLinearTable[(SourceColor shr 16) and $ff];
   LinearSource[1]:=pvVectorGraphicsSRGBToLinearTable[(SourceColor shr 8) and $ff];
   LinearSource[2]:=pvVectorGraphicsSRGBToLinearTable[SourceColor and $ff];

   LinearIndex:=(((TpvSizeInt(Row)*TpvSizeInt(fPixelBuffer.fWidth))+TpvSizeInt(X+Index))*4);
   InverseSourceAlpha:=1.0-SourceAlpha;
   ResultAlpha:=SourceAlpha+(fPixelBuffer.fLinearPixels[LinearIndex+3]*InverseSourceAlpha);

   // Straight alpha compositing, so the destination contribution has to be weighted by its own
   // alpha and the result divided by the combined alpha again.
   for Channel:=0 to 2 do begin
    if ResultAlpha>0.0 then begin
     fPixelBuffer.fLinearPixels[LinearIndex+Channel]:=
      ((LinearSource[Channel]*SourceAlpha)+
       (fPixelBuffer.fLinearPixels[LinearIndex+Channel]*fPixelBuffer.fLinearPixels[LinearIndex+3]*InverseSourceAlpha))/ResultAlpha;
    end else begin
     fPixelBuffer.fLinearPixels[LinearIndex+Channel]:=0.0;
    end;
   end;

   fPixelBuffer.fLinearPixels[LinearIndex+3]:=ResultAlpha;

  end;

 end else begin

  Row:=(Row*fPixelBuffer.fWidth)+X;

  if IsSolidPaint and not assigned(fMask) then begin

   // The cheap case: one color for the whole run, and a fully opaque one can be stored
   // outright instead of blended.
   Color:=pvVectorGraphicsModulateColor(fPaint.fColor,Alpha);
   if (Color shr 24)=0 then begin
    exit;
   end;

   if (Color shr 24)=255 then begin
    for Index:=0 to Count-1 do begin
     Pixels^[Row+Index]:=Color;
    end;
   end else begin
    for Index:=0 to Count-1 do begin
     Pixels^[Row+Index]:=pvVectorGraphicsBlendColor(Pixels^[Row+Index],Color);
    end;
   end;

  end else begin

   // Per pixel, because the paint varies, a mask is in effect, or both.
   for Index:=0 to Count-1 do begin

    if IsSolidPaint then begin
     SourceColor:=fPaint.fColor;
    end else begin
     SourceColor:=SpanColors^[Index];
    end;

    SourceColor:=pvVectorGraphicsModulateColor(SourceColor,Alpha);
    if assigned(fMask) then begin
     SourceColor:=pvVectorGraphicsMultiplyColors(SourceColor,fMask.GetPixel(X+Index,aY));
    end;

    Pixels^[Row+Index]:=pvVectorGraphicsBlendColor(Pixels^[Row+Index],SourceColor);

   end;

  end;

 end;

end;

procedure TpvVectorGraphicsRasterizer.FillShape(const aShape:TpvVectorPathShape;const aMatrix:TpvVectorPathMatrix;
                                                const aUseShapeFillRule:boolean);
var Contour:TpvVectorPathContour;
    Segment:TpvVectorPathSegment;
    SegmentLine:TpvVectorPathSegmentLine;
    Position:TpvVectorPathVector;
    First:boolean;
begin

 if not assigned(aShape) then begin
  exit;
 end;

 Reset;

 if aUseShapeFillRule then begin
  fFillRule:=aShape.FillRule;
 end;

 // Each contour becomes one subpath, transformed on the way into the subpixel grid.
 for Contour in aShape.Contours do begin

  if not assigned(Contour) then begin
   continue;
  end;

  First:=true;
  for Segment in Contour.Segments do begin

   // Curved segments are skipped, so the caller flattens first, where the target scale is known.
   if not (Segment is TpvVectorPathSegmentLine) then begin
    continue;
   end;

   SegmentLine:=TpvVectorPathSegmentLine(Segment);
   if First then begin
    First:=false;
    Position:=aMatrix.Transform(SegmentLine.Points[0]);
    MoveTo(pvVectorGraphicsConvertCoordinate(Position.x),pvVectorGraphicsConvertCoordinate(Position.y));
   end;

   Position:=aMatrix.Transform(SegmentLine.Points[1]);
   LineTo(pvVectorGraphicsConvertCoordinate(Position.x),pvVectorGraphicsConvertCoordinate(Position.y));

  end;

  if not First then begin
   Close;
  end;

 end;

 Render;

end;

procedure TpvVectorGraphicsRasterizer.RasterizeShapeToMask(const aShape:TpvVectorPathShape;
                                                           const aMatrix:TpvVectorPathMatrix;
                                                           const aMask:TpvVectorGraphicsMask;
                                                           const aFillRule:TpvVectorPathFillRule;
                                                           const aColor:TpvVectorGraphicsColor);
var MaskPixelBuffer:TpvVectorGraphicsPixelBuffer;
    OldPixelBuffer:TpvVectorGraphicsPixelBuffer;
    OldMask:TpvVectorGraphicsMask;
    OldPaint:TpvVectorGraphicsPaint;
    OldFillRule:TpvVectorPathFillRule;
begin

 if not (assigned(aShape) and assigned(aMask)) then begin
  exit;
 end;

 if length(aMask.fPixels)=0 then begin
  exit;
 end;

 // The mask is just another $aarrggbb buffer, so the same rasterizer draws into it, only with
 // the mask itself switched off while it does.
 MaskPixelBuffer:=TpvVectorGraphicsPixelBuffer.CreateFromExternalMemory(@aMask.fPixels[0],
                                                                        aMask.fWidth,
                                                                        aMask.fHeight,
                                                                        TpvVectorGraphicsColorSpaceMode.Integer);
 try

  OldMask:=fMask;
  OldPaint:=fPaint;
  OldFillRule:=fFillRule;
  OldPixelBuffer:=ReplacePixelBuffer(MaskPixelBuffer);
  try

   fMask:=nil;
   SetColor(aColor);
   fFillRule:=aFillRule;
   FillShape(aShape,aMatrix,false);

  finally
   ReplacePixelBuffer(OldPixelBuffer);
   fMask:=OldMask;
   fPaint:=OldPaint;
   fFillRule:=OldFillRule;
  end;

 finally
  FreeAndNil(MaskPixelBuffer);
 end;

end;

procedure InitializeByteMulTable;
var ValueA,ValueB:TpvInt32;
begin

 // The three special cases are spelled out rather than rounded, so that multiplying by one or by
 // zero is exact and a fully opaque source never loses a step.
 for ValueA:=Low(TpvUInt8) to High(TpvUInt8) do begin
  for ValueB:=Low(TpvUInt8) to High(TpvUInt8) do begin
   if ValueA=255 then begin
    pvVectorGraphicsByteMulTable[ValueA,ValueB]:=ValueB;
   end else if ValueB=255 then begin
    pvVectorGraphicsByteMulTable[ValueA,ValueB]:=ValueA;
   end else if (ValueA=0) or (ValueB=0) then begin
    pvVectorGraphicsByteMulTable[ValueA,ValueB]:=0;
   end else begin
    pvVectorGraphicsByteMulTable[ValueA,ValueB]:=round((ValueA/255.0)*ValueB);
   end;
  end;
 end;

 // The two corners where both special cases overlap.
 pvVectorGraphicsByteMulTable[0,0]:=0;
 pvVectorGraphicsByteMulTable[255,255]:=255;

end;

procedure InitializeSRGBToLinearTable;
var Index:TpvInt32;
begin

 for Index:=Low(TpvUInt8) to High(TpvUInt8) do begin
  pvVectorGraphicsSRGBToLinearTable[Index]:=pvVectorGraphicsSRGBToLinear(Index/255.0);
 end;

end;

initialization
 InitializeByteMulTable;
 InitializeSRGBToLinearTable;
end.
