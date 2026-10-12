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
unit PasVulkan.VectorGraphics.Filters;
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
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Collections,
     PasVulkan.VectorPath,
     PasVulkan.VectorGraphics.Rasterizer;

type EpvVectorGraphicsFilter=class(Exception);

     // Whether the numbers of a filter are fractions of the bounding box of what is being
     // filtered, or lengths in the user space that was in effect.
     PpvVectorGraphicsFilterUnits=^TpvVectorGraphicsFilterUnits;
     TpvVectorGraphicsFilterUnits=
      (
       UserSpaceOnUse=0,
       ObjectBoundingBox
      );

     // The space the arithmetic of the primitives happens in. The specification makes linear
     // light the default, which is why a filter rendered on the sRGB values alone comes out too
     // dark in the blurred edges.
     PpvVectorGraphicsFilterColorSpace=^TpvVectorGraphicsFilterColorSpace;
     TpvVectorGraphicsFilterColorSpace=
      (
       LinearRGB=0,
       SRGB
      );

     // What a primitive does. One enum rather than one class per kind, because the seventeen of
     // them share almost all of their machinery and differ only in one loop each.
     PpvVectorGraphicsFilterPrimitiveKind=^TpvVectorGraphicsFilterPrimitiveKind;
     TpvVectorGraphicsFilterPrimitiveKind=
      (
       None=0,
       Blend,
       ColorMatrix,
       ComponentTransfer,
       Composite,
       ConvolveMatrix,
       DiffuseLighting,
       DisplacementMap,
       DropShadow,
       Flood,
       GaussianBlur,
       Image,
       Merge,
       Morphology,
       Offset,
       SpecularLighting,
       Tile,
       Turbulence
      );

     // The Porter and Duff operators of feComposite, plus the arithmetic form that takes four
     // factors instead of naming a rule.
     PpvVectorGraphicsFilterCompositeOperator=^TpvVectorGraphicsFilterCompositeOperator;
     TpvVectorGraphicsFilterCompositeOperator=
      (
       Over=0,
       In_,
       Out_,
       Atop,
       Xor_,
       Arithmetic
      );

     PpvVectorGraphicsFilterColorMatrixKind=^TpvVectorGraphicsFilterColorMatrixKind;
     TpvVectorGraphicsFilterColorMatrixKind=
      (
       Matrix=0,
       Saturate,
       HueRotate,
       LuminanceToAlpha
      );

     PpvVectorGraphicsFilterTransferKind=^TpvVectorGraphicsFilterTransferKind;
     TpvVectorGraphicsFilterTransferKind=
      (
       Identity=0,
       Table,
       Discrete,
       Linear,
       Gamma
      );

     PpvVectorGraphicsFilterEdgeMode=^TpvVectorGraphicsFilterEdgeMode;
     TpvVectorGraphicsFilterEdgeMode=
      (
       Duplicate=0,
       Wrap,
       NoneEdge
      );

     PpvVectorGraphicsFilterMorphologyOperator=^TpvVectorGraphicsFilterMorphologyOperator;
     TpvVectorGraphicsFilterMorphologyOperator=
      (
       Erode=0,
       Dilate
      );

     PpvVectorGraphicsFilterChannel=^TpvVectorGraphicsFilterChannel;
     TpvVectorGraphicsFilterChannel=
      (
       ChannelR=0,
       ChannelG,
       ChannelB,
       ChannelA
      );

     PpvVectorGraphicsFilterLightKind=^TpvVectorGraphicsFilterLightKind;
     TpvVectorGraphicsFilterLightKind=
      (
       Distant=0,
       Point,
       Spot
      );

     TpvVectorGraphicsFilterTransferValues=array of TpvDouble;

     { TpvVectorGraphicsFilterTransferFunction }

     // One channel of an feComponentTransfer, which is a curve from zero to one given in one of
     // five shapes.
     TpvVectorGraphicsFilterTransferFunction=record
      public
       Kind:TpvVectorGraphicsFilterTransferKind;
       TableValues:TpvVectorGraphicsFilterTransferValues;
       Slope:TpvDouble;
       Intercept:TpvDouble;
       Amplitude:TpvDouble;
       Exponent:TpvDouble;
       Offset:TpvDouble;
      public
       class function CreateDefault:TpvVectorGraphicsFilterTransferFunction; static;
       function Apply(const aValue:TpvDouble):TpvDouble;
     end;

     PpvVectorGraphicsFilterTransferFunction=^TpvVectorGraphicsFilterTransferFunction;

     { TpvVectorGraphicsFilterImage }

     // A floating point RGBA image in the working color space of the filter, with the colors
     // premultiplied by their alpha, which is the form the specification assumes everywhere
     // except in the two primitives that say otherwise.
     TpvVectorGraphicsFilterImage=class
      private
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fPixels:array of TpvFloat;
       function GetPixelPointer:PpvFloat; {$ifdef CANINLINE}inline;{$endif}
      public
       constructor Create(const aWidth,aHeight:TpvInt32); reintroduce;
       destructor Destroy; override;
       procedure Clear;
       procedure Assign(const aFrom:TpvVectorGraphicsFilterImage);
       // Reads one pixel, with everything outside counting as fully transparent, which is what
       // the specification says a filter sees beyond its input.
       procedure GetPixel(const aX,aY:TpvInt32;out aR,aG,aB,aA:TpvFloat); {$ifdef CANINLINE}inline;{$endif}
       procedure SetPixel(const aX,aY:TpvInt32;const aR,aG,aB,aA:TpvFloat); {$ifdef CANINLINE}inline;{$endif}
       // Takes the region at (aX,aY) out of a pixel buffer and converts it into this image.
       procedure LoadFrom(const aBuffer:TpvVectorGraphicsPixelBuffer;const aX,aY:TpvInt32;
                          const aColorSpace:TpvVectorGraphicsFilterColorSpace);
       // And writes it back, replacing rather than blending, since the caller composites the
       // whole buffer afterwards.
       procedure StoreTo(const aBuffer:TpvVectorGraphicsPixelBuffer;const aX,aY:TpvInt32;
                         const aColorSpace:TpvVectorGraphicsFilterColorSpace);
       // Zeroes everything outside the given rectangle, which is how the subregion of a
       // primitive narrows what it produced.
       procedure ClipToRectangle(const aX0,aY0,aX1,aY1:TpvInt32);
      public
       property Width:TpvInt32 read fWidth;
       property Height:TpvInt32 read fHeight;
       property PixelPointer:PpvFloat read GetPixelPointer;
     end;

     TpvVectorGraphicsFilter=class;

     { TpvVectorGraphicsFilterPrimitive }

     // One step of a filter. The parameters of all seventeen kinds live side by side here, since
     // a primitive carries only a handful of them and a class per kind would be far more
     // boilerplate than it is worth.
     TpvVectorGraphicsFilterPrimitive=class
      private
       fKind:TpvVectorGraphicsFilterPrimitiveKind;
       fInput1:TpvUTF8String;
       fInput2:TpvUTF8String;
       fResultName:TpvUTF8String;
       // The subregion the primitive writes to, as given, together with the flags that say which
       // of the four numbers were stated at all.
       fX:TpvDouble;
       fY:TpvDouble;
       fWidth:TpvDouble;
       fHeight:TpvDouble;
       fHasX:boolean;
       fHasY:boolean;
       fHasWidth:boolean;
       fHasHeight:boolean;
       // feBlend
       fBlendMode:TpvVectorGraphicsBlendMode;
       // feColorMatrix
       fColorMatrixKind:TpvVectorGraphicsFilterColorMatrixKind;
       fColorMatrixValues:array[0..19] of TpvDouble;
       // feComponentTransfer
       fTransferFunctions:array[0..3] of TpvVectorGraphicsFilterTransferFunction;
       // feComposite
       fCompositeOperator:TpvVectorGraphicsFilterCompositeOperator;
       fK1:TpvDouble;
       fK2:TpvDouble;
       fK3:TpvDouble;
       fK4:TpvDouble;
       // feGaussianBlur and feDropShadow
       fStandardDeviationX:TpvDouble;
       fStandardDeviationY:TpvDouble;
       // feOffset and feDropShadow
       fDeltaX:TpvDouble;
       fDeltaY:TpvDouble;
       // feFlood and feDropShadow
       fFloodColor:TpvVectorGraphicsColor;
       fFloodOpacity:TpvDouble;
       // feMerge
       fMergeInputs:array of TpvUTF8String;
       // feMorphology
       fMorphologyOperator:TpvVectorGraphicsFilterMorphologyOperator;
       fRadiusX:TpvDouble;
       fRadiusY:TpvDouble;
       // feConvolveMatrix
       fOrderX:TpvInt32;
       fOrderY:TpvInt32;
       fKernel:array of TpvDouble;
       fDivisor:TpvDouble;
       fBias:TpvDouble;
       fTargetX:TpvInt32;
       fTargetY:TpvInt32;
       fHasTargetX:boolean;
       fHasTargetY:boolean;
       fEdgeMode:TpvVectorGraphicsFilterEdgeMode;
       fPreserveAlpha:boolean;
       // feDisplacementMap
       fScale:TpvDouble;
       fChannelSelectorX:TpvVectorGraphicsFilterChannel;
       fChannelSelectorY:TpvVectorGraphicsFilterChannel;
       // feTurbulence
       fBaseFrequencyX:TpvDouble;
       fBaseFrequencyY:TpvDouble;
       fNumOctaves:TpvInt32;
       fSeed:TpvInt32;
       fStitchTiles:boolean;
       fFractalNoise:boolean;
       // feDiffuseLighting and feSpecularLighting
       fSurfaceScale:TpvDouble;
       fDiffuseConstant:TpvDouble;
       fSpecularConstant:TpvDouble;
       fSpecularExponent:TpvDouble;
       fLightColor:TpvVectorGraphicsColor;
       fLightKind:TpvVectorGraphicsFilterLightKind;
       fLightAzimuth:TpvDouble;
       fLightElevation:TpvDouble;
       fLightX:TpvDouble;
       fLightY:TpvDouble;
       fLightZ:TpvDouble;
       fLightPointsAtX:TpvDouble;
       fLightPointsAtY:TpvDouble;
       fLightPointsAtZ:TpvDouble;
       fLightLimitingConeAngle:TpvDouble;
       fHasLightLimitingConeAngle:boolean;
       // feImage. The pixels are kept here rather than as a reference into the document, so that
       // this unit does not have to know about the scene graph at all; the loader fills it.
       fImage:TpvVectorGraphicsPixelBuffer;
       fOwnsImage:boolean;
      public
       constructor Create(const aKind:TpvVectorGraphicsFilterPrimitiveKind); reintroduce;
       destructor Destroy; override;
       procedure AddMergeInput(const aName:TpvUTF8String);
       // The three things a loader has to fill in that are not single values, and which are kept
       // private so that nothing outside can get their sizes wrong.
       procedure SetColorMatrixValues(const aValues:array of TpvDouble);
       procedure SetColorMatrixValue(const aIndex:TpvSizeInt;const aValue:TpvDouble);
       procedure SetTransferFunction(const aChannel:TpvSizeInt;const aFunction:TpvVectorGraphicsFilterTransferFunction);
       procedure SetKernel(const aValues:array of TpvDouble);
       // And the matching way back out, which a writer needs in order to put a filter into a
       // file again without anything going missing.
       function GetColorMatrixValue(const aIndex:TpvSizeInt):TpvDouble;
       function GetTransferFunction(const aChannel:TpvSizeInt):TpvVectorGraphicsFilterTransferFunction;
       function GetMergeInputCount:TpvSizeInt;
       function GetMergeInput(const aIndex:TpvSizeInt):TpvUTF8String;
       function GetKernelCount:TpvSizeInt;
       function GetKernelValue(const aIndex:TpvSizeInt):TpvDouble;
      public
       property Kind:TpvVectorGraphicsFilterPrimitiveKind read fKind;
       property Input1:TpvUTF8String read fInput1 write fInput1;
       property Input2:TpvUTF8String read fInput2 write fInput2;
       property ResultName:TpvUTF8String read fResultName write fResultName;
       property x:TpvDouble read fX write fX;
       property y:TpvDouble read fY write fY;
       property Width:TpvDouble read fWidth write fWidth;
       property Height:TpvDouble read fHeight write fHeight;
       property HasX:boolean read fHasX write fHasX;
       property HasY:boolean read fHasY write fHasY;
       property HasWidth:boolean read fHasWidth write fHasWidth;
       property HasHeight:boolean read fHasHeight write fHasHeight;
       property BlendMode:TpvVectorGraphicsBlendMode read fBlendMode write fBlendMode;
       property ColorMatrixKind:TpvVectorGraphicsFilterColorMatrixKind read fColorMatrixKind write fColorMatrixKind;
       property CompositeOperator:TpvVectorGraphicsFilterCompositeOperator read fCompositeOperator write fCompositeOperator;
       property K1:TpvDouble read fK1 write fK1;
       property K2:TpvDouble read fK2 write fK2;
       property K3:TpvDouble read fK3 write fK3;
       property K4:TpvDouble read fK4 write fK4;
       property StandardDeviationX:TpvDouble read fStandardDeviationX write fStandardDeviationX;
       property StandardDeviationY:TpvDouble read fStandardDeviationY write fStandardDeviationY;
       property DeltaX:TpvDouble read fDeltaX write fDeltaX;
       property DeltaY:TpvDouble read fDeltaY write fDeltaY;
       property FloodColor:TpvVectorGraphicsColor read fFloodColor write fFloodColor;
       property FloodOpacity:TpvDouble read fFloodOpacity write fFloodOpacity;
       property MorphologyOperator:TpvVectorGraphicsFilterMorphologyOperator read fMorphologyOperator write fMorphologyOperator;
       property RadiusX:TpvDouble read fRadiusX write fRadiusX;
       property RadiusY:TpvDouble read fRadiusY write fRadiusY;
       property OrderX:TpvInt32 read fOrderX write fOrderX;
       property OrderY:TpvInt32 read fOrderY write fOrderY;
       property Divisor:TpvDouble read fDivisor write fDivisor;
       property Bias:TpvDouble read fBias write fBias;
       property TargetX:TpvInt32 read fTargetX write fTargetX;
       property TargetY:TpvInt32 read fTargetY write fTargetY;
       property HasTargetX:boolean read fHasTargetX write fHasTargetX;
       property HasTargetY:boolean read fHasTargetY write fHasTargetY;
       property EdgeMode:TpvVectorGraphicsFilterEdgeMode read fEdgeMode write fEdgeMode;
       property PreserveAlpha:boolean read fPreserveAlpha write fPreserveAlpha;
       property Scale:TpvDouble read fScale write fScale;
       property ChannelSelectorX:TpvVectorGraphicsFilterChannel read fChannelSelectorX write fChannelSelectorX;
       property ChannelSelectorY:TpvVectorGraphicsFilterChannel read fChannelSelectorY write fChannelSelectorY;
       property BaseFrequencyX:TpvDouble read fBaseFrequencyX write fBaseFrequencyX;
       property BaseFrequencyY:TpvDouble read fBaseFrequencyY write fBaseFrequencyY;
       property NumOctaves:TpvInt32 read fNumOctaves write fNumOctaves;
       property Seed:TpvInt32 read fSeed write fSeed;
       property StitchTiles:boolean read fStitchTiles write fStitchTiles;
       property FractalNoise:boolean read fFractalNoise write fFractalNoise;
       property SurfaceScale:TpvDouble read fSurfaceScale write fSurfaceScale;
       property DiffuseConstant:TpvDouble read fDiffuseConstant write fDiffuseConstant;
       property SpecularConstant:TpvDouble read fSpecularConstant write fSpecularConstant;
       property SpecularExponent:TpvDouble read fSpecularExponent write fSpecularExponent;
       property LightColor:TpvVectorGraphicsColor read fLightColor write fLightColor;
       property LightKind:TpvVectorGraphicsFilterLightKind read fLightKind write fLightKind;
       property LightAzimuth:TpvDouble read fLightAzimuth write fLightAzimuth;
       property LightElevation:TpvDouble read fLightElevation write fLightElevation;
       property LightX:TpvDouble read fLightX write fLightX;
       property LightY:TpvDouble read fLightY write fLightY;
       property LightZ:TpvDouble read fLightZ write fLightZ;
       property LightPointsAtX:TpvDouble read fLightPointsAtX write fLightPointsAtX;
       property LightPointsAtY:TpvDouble read fLightPointsAtY write fLightPointsAtY;
       property LightPointsAtZ:TpvDouble read fLightPointsAtZ write fLightPointsAtZ;
       property LightLimitingConeAngle:TpvDouble read fLightLimitingConeAngle write fLightLimitingConeAngle;
       property HasLightLimitingConeAngle:boolean read fHasLightLimitingConeAngle write fHasLightLimitingConeAngle;
       // Setting this hands the buffer over, so the primitive frees it along with itself.
       property Image:TpvVectorGraphicsPixelBuffer read fImage write fImage;
       property OwnsImage:boolean read fOwnsImage write fOwnsImage;
     end;

     TpvVectorGraphicsFilterPrimitiveList=TpvObjectGenericList<TpvVectorGraphicsFilterPrimitive>;

     { TpvVectorGraphicsFilter }

     // A whole filter, so the region it covers and the chain of primitives that fills it.
     TpvVectorGraphicsFilter=class
      private
       fID:TpvUTF8String;
       fPrimitives:TpvVectorGraphicsFilterPrimitiveList;
       fFilterUnits:TpvVectorGraphicsFilterUnits;
       fPrimitiveUnits:TpvVectorGraphicsFilterUnits;
       fColorSpace:TpvVectorGraphicsFilterColorSpace;
       fX:TpvDouble;
       fY:TpvDouble;
       fWidth:TpvDouble;
       fHeight:TpvDouble;
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
      published
       property ID:TpvUTF8String read fID write fID;
       property Primitives:TpvVectorGraphicsFilterPrimitiveList read fPrimitives;
       property FilterUnits:TpvVectorGraphicsFilterUnits read fFilterUnits write fFilterUnits;
       property PrimitiveUnits:TpvVectorGraphicsFilterUnits read fPrimitiveUnits write fPrimitiveUnits;
       property ColorSpace:TpvVectorGraphicsFilterColorSpace read fColorSpace write fColorSpace;
       property x:TpvDouble read fX write fX;
       property y:TpvDouble read fY write fY;
       property Width:TpvDouble read fWidth write fWidth;
       property Height:TpvDouble read fHeight write fHeight;
     end;

     TpvVectorGraphicsFilterList=TpvObjectGenericList<TpvVectorGraphicsFilter>;

     { TpvVectorGraphicsFilterContext }

     // Everything one run of a filter needs to know beyond the filter itself: where the thing
     // being filtered sits, how big a user unit is on screen, and the buffer the source was
     // drawn into.
     TpvVectorGraphicsFilterContext=record
      public
       // The bounding box of what is being filtered, in device pixels.
       BoundingBoxX:TpvDouble;
       BoundingBoxY:TpvDouble;
       BoundingBoxWidth:TpvDouble;
       BoundingBoxHeight:TpvDouble;
       // How many device pixels one user unit is, which is what turns the lengths of the filter
       // into something the pixel loops can use.
       ScaleX:TpvDouble;
       ScaleY:TpvDouble;
      public
       class function Create(const aBoundingBoxX,aBoundingBoxY,aBoundingBoxWidth,aBoundingBoxHeight:TpvDouble;
                             const aScaleX,aScaleY:TpvDouble):TpvVectorGraphicsFilterContext; static;
     end;

     PpvVectorGraphicsFilterContext=^TpvVectorGraphicsFilterContext;

// Runs a filter over what was drawn into aBuffer and writes the result back into the same buffer.
// aBuffer holds the element on its own and nothing else, which is what makes SourceGraphic
// meaningful; the caller composites the outcome afterwards.
procedure pvVectorGraphicsApplyFilter(const aFilter:TpvVectorGraphicsFilter;
                                      const aBuffer:TpvVectorGraphicsPixelBuffer;
                                      const aContext:TpvVectorGraphicsFilterContext);

// The name of a primitive kind and the way back, which is what a format that stores filters needs
// in order to write them out and read them in again.
function pvVectorGraphicsFilterPrimitiveKindToName(const aKind:TpvVectorGraphicsFilterPrimitiveKind):TpvUTF8String;
function pvVectorGraphicsFilterPrimitiveKindFromName(const aName:TpvUTF8String):TpvVectorGraphicsFilterPrimitiveKind;

implementation

function pvVectorGraphicsFilterPrimitiveKindToName(const aKind:TpvVectorGraphicsFilterPrimitiveKind):TpvUTF8String;
begin

 // The names of the SVG elements without the fe, which keeps them readable in a file and short.
 case aKind of
  TpvVectorGraphicsFilterPrimitiveKind.Blend:begin
   result:='blend';
  end;
  TpvVectorGraphicsFilterPrimitiveKind.ColorMatrix:begin
   result:='colormatrix';
  end;
  TpvVectorGraphicsFilterPrimitiveKind.ComponentTransfer:begin
   result:='componenttransfer';
  end;
  TpvVectorGraphicsFilterPrimitiveKind.Composite:begin
   result:='composite';
  end;
  TpvVectorGraphicsFilterPrimitiveKind.ConvolveMatrix:begin
   result:='convolvematrix';
  end;
  TpvVectorGraphicsFilterPrimitiveKind.DiffuseLighting:begin
   result:='diffuselighting';
  end;
  TpvVectorGraphicsFilterPrimitiveKind.DisplacementMap:begin
   result:='displacementmap';
  end;
  TpvVectorGraphicsFilterPrimitiveKind.DropShadow:begin
   result:='dropshadow';
  end;
  TpvVectorGraphicsFilterPrimitiveKind.Flood:begin
   result:='flood';
  end;
  TpvVectorGraphicsFilterPrimitiveKind.GaussianBlur:begin
   result:='gaussianblur';
  end;
  TpvVectorGraphicsFilterPrimitiveKind.Image:begin
   result:='image';
  end;
  TpvVectorGraphicsFilterPrimitiveKind.Merge:begin
   result:='merge';
  end;
  TpvVectorGraphicsFilterPrimitiveKind.Morphology:begin
   result:='morphology';
  end;
  TpvVectorGraphicsFilterPrimitiveKind.Offset:begin
   result:='offset';
  end;
  TpvVectorGraphicsFilterPrimitiveKind.SpecularLighting:begin
   result:='specularlighting';
  end;
  TpvVectorGraphicsFilterPrimitiveKind.Tile:begin
   result:='tile';
  end;
  TpvVectorGraphicsFilterPrimitiveKind.Turbulence:begin
   result:='turbulence';
  end;
  else begin
   result:='none';
  end;
 end;

end;

function pvVectorGraphicsFilterPrimitiveKindFromName(const aName:TpvUTF8String):TpvVectorGraphicsFilterPrimitiveKind;
var Name:TpvUTF8String;
begin

 Name:=TpvUTF8String(LowerCase(Trim(String(aName))));

 if Name='blend' then begin
  result:=TpvVectorGraphicsFilterPrimitiveKind.Blend;
 end else if Name='colormatrix' then begin
  result:=TpvVectorGraphicsFilterPrimitiveKind.ColorMatrix;
 end else if Name='componenttransfer' then begin
  result:=TpvVectorGraphicsFilterPrimitiveKind.ComponentTransfer;
 end else if Name='composite' then begin
  result:=TpvVectorGraphicsFilterPrimitiveKind.Composite;
 end else if Name='convolvematrix' then begin
  result:=TpvVectorGraphicsFilterPrimitiveKind.ConvolveMatrix;
 end else if Name='diffuselighting' then begin
  result:=TpvVectorGraphicsFilterPrimitiveKind.DiffuseLighting;
 end else if Name='displacementmap' then begin
  result:=TpvVectorGraphicsFilterPrimitiveKind.DisplacementMap;
 end else if Name='dropshadow' then begin
  result:=TpvVectorGraphicsFilterPrimitiveKind.DropShadow;
 end else if Name='flood' then begin
  result:=TpvVectorGraphicsFilterPrimitiveKind.Flood;
 end else if Name='gaussianblur' then begin
  result:=TpvVectorGraphicsFilterPrimitiveKind.GaussianBlur;
 end else if Name='image' then begin
  result:=TpvVectorGraphicsFilterPrimitiveKind.Image;
 end else if Name='merge' then begin
  result:=TpvVectorGraphicsFilterPrimitiveKind.Merge;
 end else if Name='morphology' then begin
  result:=TpvVectorGraphicsFilterPrimitiveKind.Morphology;
 end else if Name='offset' then begin
  result:=TpvVectorGraphicsFilterPrimitiveKind.Offset;
 end else if Name='specularlighting' then begin
  result:=TpvVectorGraphicsFilterPrimitiveKind.SpecularLighting;
 end else if Name='tile' then begin
  result:=TpvVectorGraphicsFilterPrimitiveKind.Tile;
 end else if Name='turbulence' then begin
  result:=TpvVectorGraphicsFilterPrimitiveKind.Turbulence;
 end else begin
  result:=TpvVectorGraphicsFilterPrimitiveKind.None;
 end;

end;

{ TpvVectorGraphicsFilterTransferFunction }

class function TpvVectorGraphicsFilterTransferFunction.CreateDefault:TpvVectorGraphicsFilterTransferFunction;
begin
 result.Kind:=TpvVectorGraphicsFilterTransferKind.Identity;
 result.TableValues:=nil;
 result.Slope:=1.0;
 result.Intercept:=0.0;
 result.Amplitude:=1.0;
 result.Exponent:=1.0;
 result.Offset:=0.0;
end;

function TpvVectorGraphicsFilterTransferFunction.Apply(const aValue:TpvDouble):TpvDouble;
var Count,Index:TpvSizeInt;
    Position:TpvDouble;
begin

 case Kind of

  TpvVectorGraphicsFilterTransferKind.Table:begin

   // The table values sit at evenly spaced points and the value is read off the straight line
   // between the two it falls between.
   Count:=Length(TableValues);
   if Count=0 then begin
    result:=aValue;
   end else if Count=1 then begin
    result:=TableValues[0];
   end else begin
    Position:=aValue*(Count-1);
    Index:=trunc(Position);
    if Index<0 then begin
     Index:=0;
    end else if Index>(Count-2) then begin
     Index:=Count-2;
    end;
    Position:=Position-Index;
    result:=TableValues[Index]+((TableValues[Index+1]-TableValues[Index])*Position);
   end;

  end;

  TpvVectorGraphicsFilterTransferKind.Discrete:begin

   // Here the values are steps rather than corners, so there are as many slices as values.
   Count:=Length(TableValues);
   if Count=0 then begin
    result:=aValue;
   end else begin
    Index:=trunc(aValue*Count);
    if Index<0 then begin
     Index:=0;
    end else if Index>(Count-1) then begin
     Index:=Count-1;
    end;
    result:=TableValues[Index];
   end;

  end;

  TpvVectorGraphicsFilterTransferKind.Linear:begin
   result:=(Slope*aValue)+Intercept;
  end;

  TpvVectorGraphicsFilterTransferKind.Gamma:begin
   result:=(Amplitude*Power(aValue,Exponent))+Offset;
  end;

  else {TpvVectorGraphicsFilterTransferKind.Identity:}begin
   result:=aValue;
  end;

 end;

end;

{ TpvVectorGraphicsFilterImage }

constructor TpvVectorGraphicsFilterImage.Create(const aWidth,aHeight:TpvInt32);
begin

 inherited Create;

 fWidth:=Max(1,aWidth);
 fHeight:=Max(1,aHeight);

 SetLength(fPixels,TpvSizeInt(fWidth)*TpvSizeInt(fHeight)*4);
 Clear;

end;

destructor TpvVectorGraphicsFilterImage.Destroy;
begin
 fPixels:=nil;
 inherited Destroy;
end;

function TpvVectorGraphicsFilterImage.GetPixelPointer:PpvFloat;
begin
 if Length(fPixels)>0 then begin
  result:=@fPixels[0];
 end else begin
  result:=nil;
 end;
end;

procedure TpvVectorGraphicsFilterImage.Clear;
var Index:TpvSizeInt;
begin
 for Index:=0 to Length(fPixels)-1 do begin
  fPixels[Index]:=0.0;
 end;
end;

procedure TpvVectorGraphicsFilterImage.Assign(const aFrom:TpvVectorGraphicsFilterImage);
var Index:TpvSizeInt;
begin

 if not assigned(aFrom) then begin
  exit;
 end;

 if (aFrom.fWidth<>fWidth) or (aFrom.fHeight<>fHeight) then begin
  raise EpvVectorGraphicsFilter.Create('Filter image size mismatch');
 end;

 for Index:=0 to Length(fPixels)-1 do begin
  fPixels[Index]:=aFrom.fPixels[Index];
 end;

end;

procedure TpvVectorGraphicsFilterImage.GetPixel(const aX,aY:TpvInt32;out aR,aG,aB,aA:TpvFloat);
var Index:TpvSizeInt;
begin

 // Everything beyond the image counts as fully transparent, which is what the specification says
 // a primitive finds outside its input.
 if (aX<0) or (aX>=fWidth) or (aY<0) or (aY>=fHeight) then begin
  aR:=0.0;
  aG:=0.0;
  aB:=0.0;
  aA:=0.0;
  exit;
 end;

 Index:=((TpvSizeInt(aY)*TpvSizeInt(fWidth))+TpvSizeInt(aX))*4;
 aR:=fPixels[Index];
 aG:=fPixels[Index+1];
 aB:=fPixels[Index+2];
 aA:=fPixels[Index+3];

end;

procedure TpvVectorGraphicsFilterImage.SetPixel(const aX,aY:TpvInt32;const aR,aG,aB,aA:TpvFloat);
var Index:TpvSizeInt;
begin

 if (aX<0) or (aX>=fWidth) or (aY<0) or (aY>=fHeight) then begin
  exit;
 end;

 Index:=((TpvSizeInt(aY)*TpvSizeInt(fWidth))+TpvSizeInt(aX))*4;
 fPixels[Index]:=aR;
 fPixels[Index+1]:=aG;
 fPixels[Index+2]:=aB;
 fPixels[Index+3]:=aA;

end;

procedure TpvVectorGraphicsFilterImage.LoadFrom(const aBuffer:TpvVectorGraphicsPixelBuffer;const aX,aY:TpvInt32;
                                                const aColorSpace:TpvVectorGraphicsFilterColorSpace);
const OneOver255=1.0/255.0;
var x,y:TpvInt32;
    Index:TpvSizeInt;
    Color:TpvVectorGraphicsColor;
    Alpha,Red,Green,Blue:TpvFloat;
begin

 for y:=0 to fHeight-1 do begin
  for x:=0 to fWidth-1 do begin

   Color:=aBuffer.GetPixel(aX+x,aY+y);
   Alpha:=((Color shr 24) and $ff)*OneOver255;

   // The stored pixels are straight, the filter works premultiplied, and the default working
   // space is linear light rather than the sRGB the buffer holds.
   if aColorSpace=TpvVectorGraphicsFilterColorSpace.LinearRGB then begin
    Red:=pvVectorGraphicsSRGBToLinear(((Color shr 16) and $ff)*OneOver255);
    Green:=pvVectorGraphicsSRGBToLinear(((Color shr 8) and $ff)*OneOver255);
    Blue:=pvVectorGraphicsSRGBToLinear((Color and $ff)*OneOver255);
   end else begin
    Red:=((Color shr 16) and $ff)*OneOver255;
    Green:=((Color shr 8) and $ff)*OneOver255;
    Blue:=(Color and $ff)*OneOver255;
   end;

   Index:=((TpvSizeInt(y)*TpvSizeInt(fWidth))+TpvSizeInt(x))*4;
   fPixels[Index]:=Red*Alpha;
   fPixels[Index+1]:=Green*Alpha;
   fPixels[Index+2]:=Blue*Alpha;
   fPixels[Index+3]:=Alpha;

  end;
 end;

end;

procedure TpvVectorGraphicsFilterImage.StoreTo(const aBuffer:TpvVectorGraphicsPixelBuffer;const aX,aY:TpvInt32;
                                               const aColorSpace:TpvVectorGraphicsFilterColorSpace);
var x,y:TpvInt32;
    Index:TpvSizeInt;
    Alpha,Red,Green,Blue,InverseAlpha:TpvFloat;
    Color:TpvVectorGraphicsColor;

 // Rounds one channel back into a byte.
 function ToByte(const aValue:TpvFloat):TpvVectorGraphicsColor;
 var Value:TpvFloat;
 begin
  Value:=aValue;
  if Value<=0.0 then begin
   Value:=0.0;
  end else if Value>=1.0 then begin
   Value:=1.0;
  end;
  result:=round(Value*255.0);
 end;

begin

 for y:=0 to fHeight-1 do begin
  for x:=0 to fWidth-1 do begin

   Index:=((TpvSizeInt(y)*TpvSizeInt(fWidth))+TpvSizeInt(x))*4;
   Alpha:=fPixels[Index+3];
   if Alpha<=0.0 then begin
    aBuffer.SetPixel(aX+x,aY+y,0);
    continue;
   end;
   if Alpha>1.0 then begin
    Alpha:=1.0;
   end;

   // Back to straight colors, and out of the working space if it was not the one the buffer
   // stores in.
   InverseAlpha:=1.0/Alpha;
   Red:=fPixels[Index]*InverseAlpha;
   Green:=fPixels[Index+1]*InverseAlpha;
   Blue:=fPixels[Index+2]*InverseAlpha;

   if aColorSpace=TpvVectorGraphicsFilterColorSpace.LinearRGB then begin
    Red:=pvVectorGraphicsLinearToSRGB(Max(0.0,Min(1.0,Red)));
    Green:=pvVectorGraphicsLinearToSRGB(Max(0.0,Min(1.0,Green)));
    Blue:=pvVectorGraphicsLinearToSRGB(Max(0.0,Min(1.0,Blue)));
   end;

   Color:=(ToByte(Alpha) shl 24) or (ToByte(Red) shl 16) or (ToByte(Green) shl 8) or ToByte(Blue);
   aBuffer.SetPixel(aX+x,aY+y,Color);

  end;
 end;

end;

procedure TpvVectorGraphicsFilterImage.ClipToRectangle(const aX0,aY0,aX1,aY1:TpvInt32);
var x,y:TpvInt32;
    Index:TpvSizeInt;
begin

 for y:=0 to fHeight-1 do begin
  for x:=0 to fWidth-1 do begin
   if (x<aX0) or (x>=aX1) or (y<aY0) or (y>=aY1) then begin
    Index:=((TpvSizeInt(y)*TpvSizeInt(fWidth))+TpvSizeInt(x))*4;
    fPixels[Index]:=0.0;
    fPixels[Index+1]:=0.0;
    fPixels[Index+2]:=0.0;
    fPixels[Index+3]:=0.0;
   end;
  end;
 end;

end;

{ TpvVectorGraphicsFilterPrimitive }

constructor TpvVectorGraphicsFilterPrimitive.Create(const aKind:TpvVectorGraphicsFilterPrimitiveKind);
var Index:TpvSizeInt;
begin

 inherited Create;

 fKind:=aKind;

 fInput1:='';
 fInput2:='';
 fResultName:='';

 // All four numbers of the subregion are optional, and what is missing comes from the filter
 // region instead.
 fX:=0.0;
 fY:=0.0;
 fWidth:=0.0;
 fHeight:=0.0;
 fHasX:=false;
 fHasY:=false;
 fHasWidth:=false;
 fHasHeight:=false;

 fBlendMode:=TpvVectorGraphicsBlendMode.Normal;

 fColorMatrixKind:=TpvVectorGraphicsFilterColorMatrixKind.Matrix;
 for Index:=0 to 19 do begin
  fColorMatrixValues[Index]:=0.0;
 end;
 fColorMatrixValues[0]:=1.0;
 fColorMatrixValues[6]:=1.0;
 fColorMatrixValues[12]:=1.0;
 fColorMatrixValues[18]:=1.0;

 for Index:=0 to 3 do begin
  fTransferFunctions[Index]:=TpvVectorGraphicsFilterTransferFunction.CreateDefault;
 end;

 fCompositeOperator:=TpvVectorGraphicsFilterCompositeOperator.Over;
 fK1:=0.0;
 fK2:=0.0;
 fK3:=0.0;
 fK4:=0.0;

 fStandardDeviationX:=0.0;
 fStandardDeviationY:=0.0;

 fDeltaX:=0.0;
 fDeltaY:=0.0;

 fFloodColor:=$ff000000;
 fFloodOpacity:=1.0;

 fMergeInputs:=nil;

 fMorphologyOperator:=TpvVectorGraphicsFilterMorphologyOperator.Erode;
 fRadiusX:=0.0;
 fRadiusY:=0.0;

 fOrderX:=3;
 fOrderY:=3;
 fKernel:=nil;
 fDivisor:=0.0;
 fBias:=0.0;
 fTargetX:=0;
 fTargetY:=0;
 fHasTargetX:=false;
 fHasTargetY:=false;
 fEdgeMode:=TpvVectorGraphicsFilterEdgeMode.Duplicate;
 fPreserveAlpha:=false;

 fScale:=0.0;
 fChannelSelectorX:=TpvVectorGraphicsFilterChannel.ChannelA;
 fChannelSelectorY:=TpvVectorGraphicsFilterChannel.ChannelA;

 fBaseFrequencyX:=0.0;
 fBaseFrequencyY:=0.0;
 fNumOctaves:=1;
 fSeed:=0;
 fStitchTiles:=false;
 fFractalNoise:=false;

 fSurfaceScale:=1.0;
 fDiffuseConstant:=1.0;
 fSpecularConstant:=1.0;
 fSpecularExponent:=1.0;
 fLightColor:=$ffffffff;
 fLightKind:=TpvVectorGraphicsFilterLightKind.Distant;
 fLightAzimuth:=0.0;
 fLightElevation:=0.0;
 fLightX:=0.0;
 fLightY:=0.0;
 fLightZ:=0.0;
 fLightPointsAtX:=0.0;
 fLightPointsAtY:=0.0;
 fLightPointsAtZ:=0.0;
 fLightLimitingConeAngle:=0.0;
 fHasLightLimitingConeAngle:=false;

 fImage:=nil;
 fOwnsImage:=true;

end;

destructor TpvVectorGraphicsFilterPrimitive.Destroy;
begin

 fMergeInputs:=nil;
 fKernel:=nil;

 if fOwnsImage then begin
  FreeAndNil(fImage);
 end else begin
  fImage:=nil;
 end;

 inherited Destroy;

end;

procedure TpvVectorGraphicsFilterPrimitive.AddMergeInput(const aName:TpvUTF8String);
var Count:TpvSizeInt;
begin
 Count:=Length(fMergeInputs);
 SetLength(fMergeInputs,Count+1);
 fMergeInputs[Count]:=aName;
end;

procedure TpvVectorGraphicsFilterPrimitive.SetColorMatrixValues(const aValues:array of TpvDouble);
var Index:TpvSizeInt;
begin

 // Anything shorter than the twenty the matrix needs is ignored outright, since a half filled
 // matrix would be worse than the identity that is already there.
 if Length(aValues)<20 then begin
  exit;
 end;

 for Index:=0 to 19 do begin
  fColorMatrixValues[Index]:=aValues[Index];
 end;

end;

procedure TpvVectorGraphicsFilterPrimitive.SetColorMatrixValue(const aIndex:TpvSizeInt;const aValue:TpvDouble);
begin
 if (aIndex>=0) and (aIndex<=19) then begin
  fColorMatrixValues[aIndex]:=aValue;
 end;
end;

procedure TpvVectorGraphicsFilterPrimitive.SetTransferFunction(const aChannel:TpvSizeInt;
                                                               const aFunction:TpvVectorGraphicsFilterTransferFunction);
begin
 if (aChannel>=0) and (aChannel<=3) then begin
  fTransferFunctions[aChannel]:=aFunction;
 end;
end;

procedure TpvVectorGraphicsFilterPrimitive.SetKernel(const aValues:array of TpvDouble);
var Index:TpvSizeInt;
begin
 SetLength(fKernel,Length(aValues));
 for Index:=0 to Length(aValues)-1 do begin
  fKernel[Index]:=aValues[Index];
 end;
end;

function TpvVectorGraphicsFilterPrimitive.GetColorMatrixValue(const aIndex:TpvSizeInt):TpvDouble;
begin
 if (aIndex>=0) and (aIndex<=19) then begin
  result:=fColorMatrixValues[aIndex];
 end else begin
  result:=0.0;
 end;
end;

function TpvVectorGraphicsFilterPrimitive.GetTransferFunction(const aChannel:TpvSizeInt):TpvVectorGraphicsFilterTransferFunction;
begin
 if (aChannel>=0) and (aChannel<=3) then begin
  result:=fTransferFunctions[aChannel];
 end else begin
  result:=TpvVectorGraphicsFilterTransferFunction.CreateDefault;
 end;
end;

function TpvVectorGraphicsFilterPrimitive.GetMergeInputCount:TpvSizeInt;
begin
 result:=Length(fMergeInputs);
end;

function TpvVectorGraphicsFilterPrimitive.GetMergeInput(const aIndex:TpvSizeInt):TpvUTF8String;
begin
 if (aIndex>=0) and (aIndex<Length(fMergeInputs)) then begin
  result:=fMergeInputs[aIndex];
 end else begin
  result:='';
 end;
end;

function TpvVectorGraphicsFilterPrimitive.GetKernelCount:TpvSizeInt;
begin
 result:=Length(fKernel);
end;

function TpvVectorGraphicsFilterPrimitive.GetKernelValue(const aIndex:TpvSizeInt):TpvDouble;
begin
 if (aIndex>=0) and (aIndex<Length(fKernel)) then begin
  result:=fKernel[aIndex];
 end else begin
  result:=0.0;
 end;
end;

{ TpvVectorGraphicsFilter }

constructor TpvVectorGraphicsFilter.Create;
begin

 inherited Create;

 fID:='';
 fPrimitives:=TpvVectorGraphicsFilterPrimitiveList.Create;
 fPrimitives.OwnsObjects:=true;

 fFilterUnits:=TpvVectorGraphicsFilterUnits.ObjectBoundingBox;
 fPrimitiveUnits:=TpvVectorGraphicsFilterUnits.UserSpaceOnUse;
 fColorSpace:=TpvVectorGraphicsFilterColorSpace.LinearRGB;

 // The defaults of the specification, so a tenth of the bounding box of margin on every side.
 fX:=-0.1;
 fY:=-0.1;
 fWidth:=1.2;
 fHeight:=1.2;

end;

destructor TpvVectorGraphicsFilter.Destroy;
begin
 FreeAndNil(fPrimitives);
 inherited Destroy;
end;

{ TpvVectorGraphicsFilterContext }

class function TpvVectorGraphicsFilterContext.Create(const aBoundingBoxX,aBoundingBoxY,aBoundingBoxWidth,aBoundingBoxHeight:TpvDouble;
                                                     const aScaleX,aScaleY:TpvDouble):TpvVectorGraphicsFilterContext;
begin
 result.BoundingBoxX:=aBoundingBoxX;
 result.BoundingBoxY:=aBoundingBoxY;
 result.BoundingBoxWidth:=aBoundingBoxWidth;
 result.BoundingBoxHeight:=aBoundingBoxHeight;
 result.ScaleX:=aScaleX;
 result.ScaleY:=aScaleY;
end;

// One pass of a box blur along x, which is the building block the specification approximates a
// Gaussian with.
procedure FilterBoxBlurHorizontal(const aSource,aDestination:TpvVectorGraphicsFilterImage;
                                  const aSize,aOffset:TpvInt32);
var x,y,Channel,Left,Right:TpvInt32;
    Sums:array[0..3] of TpvDouble;
    Scale:TpvDouble;
    R,G,B,A:TpvFloat;
begin

 if aSize<=1 then begin
  aDestination.Assign(aSource);
  exit;
 end;

 Scale:=1.0/aSize;

 for y:=0 to aSource.Height-1 do begin

  for Channel:=0 to 3 do begin
   Sums[Channel]:=0.0;
  end;

  // Prime the running sum with the window as it stands at the left edge.
  for x:=-aOffset to (aSize-aOffset)-1 do begin
   aSource.GetPixel(x,y,R,G,B,A);
   Sums[0]:=Sums[0]+R;
   Sums[1]:=Sums[1]+G;
   Sums[2]:=Sums[2]+B;
   Sums[3]:=Sums[3]+A;
  end;

  for x:=0 to aSource.Width-1 do begin

   aDestination.SetPixel(x,y,Sums[0]*Scale,Sums[1]*Scale,Sums[2]*Scale,Sums[3]*Scale);

   // Slide the window on by one, so the whole row costs two lookups per pixel rather than the
   // size of the window.
   Left:=x-aOffset;
   Right:=(x+aSize)-aOffset;

   aSource.GetPixel(Left,y,R,G,B,A);
   Sums[0]:=Sums[0]-R;
   Sums[1]:=Sums[1]-G;
   Sums[2]:=Sums[2]-B;
   Sums[3]:=Sums[3]-A;

   aSource.GetPixel(Right,y,R,G,B,A);
   Sums[0]:=Sums[0]+R;
   Sums[1]:=Sums[1]+G;
   Sums[2]:=Sums[2]+B;
   Sums[3]:=Sums[3]+A;

  end;

 end;

end;

// The same along y. The two are kept apart rather than transposing, because a transpose of a
// large image costs more than the duplication here saves.
procedure FilterBoxBlurVertical(const aSource,aDestination:TpvVectorGraphicsFilterImage;
                                const aSize,aOffset:TpvInt32);
var x,y,Channel,Top,Bottom:TpvInt32;
    Sums:array[0..3] of TpvDouble;
    Scale:TpvDouble;
    R,G,B,A:TpvFloat;
begin

 if aSize<=1 then begin
  aDestination.Assign(aSource);
  exit;
 end;

 Scale:=1.0/aSize;

 for x:=0 to aSource.Width-1 do begin

  for Channel:=0 to 3 do begin
   Sums[Channel]:=0.0;
  end;

  for y:=-aOffset to (aSize-aOffset)-1 do begin
   aSource.GetPixel(x,y,R,G,B,A);
   Sums[0]:=Sums[0]+R;
   Sums[1]:=Sums[1]+G;
   Sums[2]:=Sums[2]+B;
   Sums[3]:=Sums[3]+A;
  end;

  for y:=0 to aSource.Height-1 do begin

   aDestination.SetPixel(x,y,Sums[0]*Scale,Sums[1]*Scale,Sums[2]*Scale,Sums[3]*Scale);

   Top:=y-aOffset;
   Bottom:=(y+aSize)-aOffset;

   aSource.GetPixel(x,Top,R,G,B,A);
   Sums[0]:=Sums[0]-R;
   Sums[1]:=Sums[1]-G;
   Sums[2]:=Sums[2]-B;
   Sums[3]:=Sums[3]-A;

   aSource.GetPixel(x,Bottom,R,G,B,A);
   Sums[0]:=Sums[0]+R;
   Sums[1]:=Sums[1]+G;
   Sums[2]:=Sums[2]+B;
   Sums[3]:=Sums[3]+A;

  end;

 end;

end;

// The Gaussian of the specification, which is three box blurs whose sizes come out of the
// standard deviation by a formula it gives outright.
procedure FilterGaussianBlur(const aSource,aDestination:TpvVectorGraphicsFilterImage;
                             const aDeviationX,aDeviationY:TpvDouble);
var SizeX,SizeY:TpvInt32;
    Scratch,Other:TpvVectorGraphicsFilterImage;
begin

 SizeX:=trunc(((aDeviationX*3.0*sqrt(2.0*PI))/4.0)+0.5);
 SizeY:=trunc(((aDeviationY*3.0*sqrt(2.0*PI))/4.0)+0.5);

 if (SizeX<1) and (SizeY<1) then begin
  aDestination.Assign(aSource);
  exit;
 end;

 Scratch:=TpvVectorGraphicsFilterImage.Create(aSource.Width,aSource.Height);
 Other:=TpvVectorGraphicsFilterImage.Create(aSource.Width,aSource.Height);
 try

  // An odd size centres on the pixel, an even one cannot, so the specification shifts the first
  // two passes against each other and widens the third by one.
  if SizeX>=1 then begin
   if (SizeX and 1)<>0 then begin
    FilterBoxBlurHorizontal(aSource,Scratch,SizeX,SizeX div 2);
    FilterBoxBlurHorizontal(Scratch,Other,SizeX,SizeX div 2);
    FilterBoxBlurHorizontal(Other,Scratch,SizeX,SizeX div 2);
   end else begin
    FilterBoxBlurHorizontal(aSource,Scratch,SizeX,SizeX div 2);
    FilterBoxBlurHorizontal(Scratch,Other,SizeX,(SizeX div 2)-1);
    FilterBoxBlurHorizontal(Other,Scratch,SizeX+1,SizeX div 2);
   end;
  end else begin
   Scratch.Assign(aSource);
  end;

  if SizeY>=1 then begin
   if (SizeY and 1)<>0 then begin
    FilterBoxBlurVertical(Scratch,Other,SizeY,SizeY div 2);
    FilterBoxBlurVertical(Other,Scratch,SizeY,SizeY div 2);
    FilterBoxBlurVertical(Scratch,aDestination,SizeY,SizeY div 2);
   end else begin
    FilterBoxBlurVertical(Scratch,Other,SizeY,SizeY div 2);
    FilterBoxBlurVertical(Other,Scratch,SizeY,(SizeY div 2)-1);
    FilterBoxBlurVertical(Scratch,aDestination,SizeY+1,SizeY div 2);
   end;
  end else begin
   aDestination.Assign(Scratch);
  end;

 finally
  FreeAndNil(Scratch);
  FreeAndNil(Other);
 end;

end;

// feOffset, which is a plain shift by whole pixels.
procedure FilterOffset(const aSource,aDestination:TpvVectorGraphicsFilterImage;const aDeltaX,aDeltaY:TpvInt32);
var x,y:TpvInt32;
    R,G,B,A:TpvFloat;
begin
 for y:=0 to aDestination.Height-1 do begin
  for x:=0 to aDestination.Width-1 do begin
   aSource.GetPixel(x-aDeltaX,y-aDeltaY,R,G,B,A);
   aDestination.SetPixel(x,y,R,G,B,A);
  end;
 end;
end;

// feFlood, which fills its subregion with one color and leaves the rest alone, since the caller
// clips to the subregion afterwards anyway.
procedure FilterFlood(const aDestination:TpvVectorGraphicsFilterImage;
                      const aColor:TpvVectorGraphicsColor;const aOpacity:TpvDouble;
                      const aColorSpace:TpvVectorGraphicsFilterColorSpace);
const OneOver255=1.0/255.0;
var x,y:TpvInt32;
    Alpha,Red,Green,Blue:TpvFloat;
begin

 Alpha:=(((aColor shr 24) and $ff)*OneOver255)*aOpacity;

 if aColorSpace=TpvVectorGraphicsFilterColorSpace.LinearRGB then begin
  Red:=pvVectorGraphicsSRGBToLinear(((aColor shr 16) and $ff)*OneOver255);
  Green:=pvVectorGraphicsSRGBToLinear(((aColor shr 8) and $ff)*OneOver255);
  Blue:=pvVectorGraphicsSRGBToLinear((aColor and $ff)*OneOver255);
 end else begin
  Red:=((aColor shr 16) and $ff)*OneOver255;
  Green:=((aColor shr 8) and $ff)*OneOver255;
  Blue:=(aColor and $ff)*OneOver255;
 end;

 for y:=0 to aDestination.Height-1 do begin
  for x:=0 to aDestination.Width-1 do begin
   aDestination.SetPixel(x,y,Red*Alpha,Green*Alpha,Blue*Alpha,Alpha);
  end;
 end;

end;

// feBlend, which is the same set of modes mix-blend-mode uses, only between two filter inputs.
procedure FilterBlend(const aFirst,aSecond,aDestination:TpvVectorGraphicsFilterImage;
                      const aMode:TpvVectorGraphicsBlendMode);
var x,y,Channel:TpvInt32;
    SourceR,SourceG,SourceB,SourceA:TpvFloat;
    BackdropR,BackdropG,BackdropB,BackdropA:TpvFloat;
    Source,Backdrop,Blended:TpvVectorGraphicsFloatTriple;
    ResultAlpha,InverseSource:TpvFloat;
    Straight:array[0..2] of TpvFloat;
begin

 for y:=0 to aDestination.Height-1 do begin
  for x:=0 to aDestination.Width-1 do begin

   // The first input is the source drawn over the second, which the specification calls the
   // backdrop here just as compositing does.
   aFirst.GetPixel(x,y,SourceR,SourceG,SourceB,SourceA);
   aSecond.GetPixel(x,y,BackdropR,BackdropG,BackdropB,BackdropA);

   if (SourceA<=0.0) and (BackdropA<=0.0) then begin
    aDestination.SetPixel(x,y,0.0,0.0,0.0,0.0);
    continue;
   end;

   // The blend functions work on straight colors, so the premultiplied ones are divided out and
   // multiplied back in afterwards.
   if SourceA>0.0 then begin
    Source[0]:=SourceR/SourceA;
    Source[1]:=SourceG/SourceA;
    Source[2]:=SourceB/SourceA;
   end else begin
    Source[0]:=0.0;
    Source[1]:=0.0;
    Source[2]:=0.0;
   end;

   if BackdropA>0.0 then begin
    Backdrop[0]:=BackdropR/BackdropA;
    Backdrop[1]:=BackdropG/BackdropA;
    Backdrop[2]:=BackdropB/BackdropA;
   end else begin
    Backdrop[0]:=0.0;
    Backdrop[1]:=0.0;
    Backdrop[2]:=0.0;
   end;

   Blended:=pvVectorGraphicsBlendTriple(aMode,Backdrop,Source,BackdropA);

   // Plain source over with the blended color standing in for the source.
   InverseSource:=1.0-SourceA;
   ResultAlpha:=SourceA+(BackdropA*InverseSource);
   for Channel:=0 to 2 do begin
    Straight[Channel]:=(Blended[Channel]*SourceA)+(Backdrop[Channel]*BackdropA*InverseSource);
   end;

   aDestination.SetPixel(x,y,Straight[0],Straight[1],Straight[2],ResultAlpha);

  end;
 end;

end;

// feComposite, so the Porter and Duff rules plus the arithmetic form.
procedure FilterComposite(const aFirst,aSecond,aDestination:TpvVectorGraphicsFilterImage;
                          const aOperator:TpvVectorGraphicsFilterCompositeOperator;
                          const aK1,aK2,aK3,aK4:TpvDouble);
var x,y,Channel:TpvInt32;
    Source:array[0..3] of TpvFloat;
    Backdrop:array[0..3] of TpvFloat;
    Output:array[0..3] of TpvFloat;
    FactorSource,FactorBackdrop,Value:TpvFloat;
begin

 for y:=0 to aDestination.Height-1 do begin
  for x:=0 to aDestination.Width-1 do begin

   aFirst.GetPixel(x,y,Source[0],Source[1],Source[2],Source[3]);
   aSecond.GetPixel(x,y,Backdrop[0],Backdrop[1],Backdrop[2],Backdrop[3]);

   if aOperator=TpvVectorGraphicsFilterCompositeOperator.Arithmetic then begin

    // The arithmetic form works on every channel alike, alpha included, and can leave the unit
    // range, which is why it is clamped here.
    for Channel:=0 to 3 do begin
     Value:=((aK1*Source[Channel]*Backdrop[Channel])+(aK2*Source[Channel]))+
            ((aK3*Backdrop[Channel])+aK4);
     if Value<0.0 then begin
      Value:=0.0;
     end else if Value>1.0 then begin
      Value:=1.0;
     end;
     Output[Channel]:=Value;
    end;

   end else begin

    // The five rules differ only in the two factors the two inputs are weighted with.
    case aOperator of
     TpvVectorGraphicsFilterCompositeOperator.In_:begin
      FactorSource:=Backdrop[3];
      FactorBackdrop:=0.0;
     end;
     TpvVectorGraphicsFilterCompositeOperator.Out_:begin
      FactorSource:=1.0-Backdrop[3];
      FactorBackdrop:=0.0;
     end;
     TpvVectorGraphicsFilterCompositeOperator.Atop:begin
      FactorSource:=Backdrop[3];
      FactorBackdrop:=1.0-Source[3];
     end;
     TpvVectorGraphicsFilterCompositeOperator.Xor_:begin
      FactorSource:=1.0-Backdrop[3];
      FactorBackdrop:=1.0-Source[3];
     end;
     else {TpvVectorGraphicsFilterCompositeOperator.Over:}begin
      FactorSource:=1.0;
      FactorBackdrop:=1.0-Source[3];
     end;
    end;

    for Channel:=0 to 3 do begin
     Output[Channel]:=(Source[Channel]*FactorSource)+(Backdrop[Channel]*FactorBackdrop);
    end;

   end;

   aDestination.SetPixel(x,y,Output[0],Output[1],Output[2],Output[3]);

  end;
 end;

end;

// feColorMatrix, which is the one place besides the component transfer where the specification
// works on straight rather than premultiplied colors.
procedure FilterColorMatrix(const aSource,aDestination:TpvVectorGraphicsFilterImage;
                            const aValues:array of TpvDouble);
var x,y,Row,Column:TpvInt32;
    Input:array[0..3] of TpvFloat;
    Straight:array[0..4] of TpvFloat;
    Output:array[0..3] of TpvFloat;
    Value:TpvFloat;
begin

 Straight[4]:=1.0;

 for y:=0 to aDestination.Height-1 do begin
  for x:=0 to aDestination.Width-1 do begin

   aSource.GetPixel(x,y,Input[0],Input[1],Input[2],Input[3]);

   if Input[3]>0.0 then begin
    Straight[0]:=Input[0]/Input[3];
    Straight[1]:=Input[1]/Input[3];
    Straight[2]:=Input[2]/Input[3];
   end else begin
    Straight[0]:=0.0;
    Straight[1]:=0.0;
    Straight[2]:=0.0;
   end;
   Straight[3]:=Input[3];

   // Twenty values, so four rows of five, the fifth column being the constant term.
   for Row:=0 to 3 do begin
    Value:=0.0;
    for Column:=0 to 4 do begin
     Value:=Value+(aValues[(Row*5)+Column]*Straight[Column]);
    end;
    if Value<0.0 then begin
     Value:=0.0;
    end else if Value>1.0 then begin
     Value:=1.0;
    end;
    Output[Row]:=Value;
   end;

   aDestination.SetPixel(x,y,Output[0]*Output[3],Output[1]*Output[3],Output[2]*Output[3],Output[3]);

  end;
 end;

end;

// feComponentTransfer, one curve per channel, likewise on straight colors.
procedure FilterComponentTransfer(const aSource,aDestination:TpvVectorGraphicsFilterImage;
                                  const aFunctions:array of TpvVectorGraphicsFilterTransferFunction);
var x,y,Channel:TpvInt32;
    Input:array[0..3] of TpvFloat;
    Straight:array[0..3] of TpvFloat;
    Value:TpvFloat;
begin

 for y:=0 to aDestination.Height-1 do begin
  for x:=0 to aDestination.Width-1 do begin

   aSource.GetPixel(x,y,Input[0],Input[1],Input[2],Input[3]);

   if Input[3]>0.0 then begin
    Straight[0]:=Input[0]/Input[3];
    Straight[1]:=Input[1]/Input[3];
    Straight[2]:=Input[2]/Input[3];
   end else begin
    Straight[0]:=0.0;
    Straight[1]:=0.0;
    Straight[2]:=0.0;
   end;
   Straight[3]:=Input[3];

   for Channel:=0 to 3 do begin
    Value:=aFunctions[Channel].Apply(Straight[Channel]);
    if Value<0.0 then begin
     Value:=0.0;
    end else if Value>1.0 then begin
     Value:=1.0;
    end;
    Straight[Channel]:=Value;
   end;

   aDestination.SetPixel(x,y,Straight[0]*Straight[3],Straight[1]*Straight[3],Straight[2]*Straight[3],Straight[3]);

  end;
 end;

end;

// feTile, which repeats the input subregion across the whole output.
procedure FilterTile(const aSource,aDestination:TpvVectorGraphicsFilterImage;
                     const aX0,aY0,aX1,aY1:TpvInt32);
var x,y,TileWidth,TileHeight,SourceX,SourceY:TpvInt32;
    R,G,B,A:TpvFloat;
begin

 TileWidth:=aX1-aX0;
 TileHeight:=aY1-aY0;
 if (TileWidth<=0) or (TileHeight<=0) then begin
  exit;
 end;

 for y:=0 to aDestination.Height-1 do begin
  for x:=0 to aDestination.Width-1 do begin

   // The modulo has to work for negative numbers too, since the output reaches above and to the
   // left of the tile.
   SourceX:=(x-aX0) mod TileWidth;
   if SourceX<0 then begin
    inc(SourceX,TileWidth);
   end;
   SourceY:=(y-aY0) mod TileHeight;
   if SourceY<0 then begin
    inc(SourceY,TileHeight);
   end;

   aSource.GetPixel(aX0+SourceX,aY0+SourceY,R,G,B,A);
   aDestination.SetPixel(x,y,R,G,B,A);

  end;
 end;

end;

// feMorphology, so the smallest or the largest value found inside a rectangle around each pixel.
procedure FilterMorphology(const aSource,aDestination:TpvVectorGraphicsFilterImage;
                           const aRadiusX,aRadiusY:TpvInt32;
                           const aOperator:TpvVectorGraphicsFilterMorphologyOperator);
var x,y,OffsetX,OffsetY,Channel:TpvInt32;
    Best:array[0..3] of TpvFloat;
    Current:array[0..3] of TpvFloat;
    Dilate:boolean;
begin

 if (aRadiusX<=0) and (aRadiusY<=0) then begin
  aDestination.Assign(aSource);
  exit;
 end;

 Dilate:=aOperator=TpvVectorGraphicsFilterMorphologyOperator.Dilate;

 for y:=0 to aDestination.Height-1 do begin
  for x:=0 to aDestination.Width-1 do begin

   // Erode starts from the largest possible value and keeps the smallest it meets, dilate the
   // other way round.
   for Channel:=0 to 3 do begin
    if Dilate then begin
     Best[Channel]:=0.0;
    end else begin
     Best[Channel]:=1.0;
    end;
   end;

   for OffsetY:=-aRadiusY to aRadiusY do begin
    for OffsetX:=-aRadiusX to aRadiusX do begin
     aSource.GetPixel(x+OffsetX,y+OffsetY,Current[0],Current[1],Current[2],Current[3]);
     for Channel:=0 to 3 do begin
      if Dilate then begin
       if Current[Channel]>Best[Channel] then begin
        Best[Channel]:=Current[Channel];
       end;
      end else begin
       if Current[Channel]<Best[Channel] then begin
        Best[Channel]:=Current[Channel];
       end;
      end;
     end;
    end;
   end;

   aDestination.SetPixel(x,y,Best[0],Best[1],Best[2],Best[3]);

  end;
 end;

end;

// feConvolveMatrix. The kernel of the specification is applied rotated by half a turn, which is
// why the lookup below subtracts rather than adds the offsets.
procedure FilterConvolveMatrix(const aSource,aDestination:TpvVectorGraphicsFilterImage;
                               const aOrderX,aOrderY:TpvInt32;
                               const aKernel:array of TpvDouble;
                               const aDivisor,aBias:TpvDouble;
                               const aTargetX,aTargetY:TpvInt32;
                               const aEdgeMode:TpvVectorGraphicsFilterEdgeMode;
                               const aPreserveAlpha:boolean);
var x,y,KernelX,KernelY,SampleX,SampleY,Channel:TpvInt32;
    Sums:array[0..3] of TpvDouble;
    Sample:array[0..3] of TpvFloat;
    Weight,Divisor,Value,Alpha:TpvDouble;

 // Reads one pixel the way the edge mode asks for, which is what makes the three of them differ.
 procedure SampleAt(const aX,aY:TpvInt32);
 var SourceX,SourceY:TpvInt32;
 begin

  SourceX:=aX;
  SourceY:=aY;

  case aEdgeMode of
   TpvVectorGraphicsFilterEdgeMode.Duplicate:begin
    if SourceX<0 then begin
     SourceX:=0;
    end else if SourceX>=aSource.Width then begin
     SourceX:=aSource.Width-1;
    end;
    if SourceY<0 then begin
     SourceY:=0;
    end else if SourceY>=aSource.Height then begin
     SourceY:=aSource.Height-1;
    end;
   end;
   TpvVectorGraphicsFilterEdgeMode.Wrap:begin
    SourceX:=SourceX mod aSource.Width;
    if SourceX<0 then begin
     inc(SourceX,aSource.Width);
    end;
    SourceY:=SourceY mod aSource.Height;
    if SourceY<0 then begin
     inc(SourceY,aSource.Height);
    end;
   end;
   else {TpvVectorGraphicsFilterEdgeMode.NoneEdge:}begin
    // Outside counts as transparent, which GetPixel already does on its own.
   end;
  end;

  aSource.GetPixel(SourceX,SourceY,Sample[0],Sample[1],Sample[2],Sample[3]);

 end;

begin

 if (aOrderX<=0) or (aOrderY<=0) or (Length(aKernel)<(aOrderX*aOrderY)) then begin
  aDestination.Assign(aSource);
  exit;
 end;

 // A divisor of zero means the sum of the kernel, and a kernel that sums to zero means one.
 Divisor:=aDivisor;
 if abs(Divisor)<1e-12 then begin
  Divisor:=0.0;
  for KernelY:=0 to (aOrderX*aOrderY)-1 do begin
   Divisor:=Divisor+aKernel[KernelY];
  end;
  if abs(Divisor)<1e-12 then begin
   Divisor:=1.0;
  end;
 end;

 for y:=0 to aDestination.Height-1 do begin
  for x:=0 to aDestination.Width-1 do begin

   for Channel:=0 to 3 do begin
    Sums[Channel]:=0.0;
   end;

   for KernelY:=0 to aOrderY-1 do begin
    for KernelX:=0 to aOrderX-1 do begin

     Weight:=aKernel[((aOrderY-1-KernelY)*aOrderX)+(aOrderX-1-KernelX)];
     if Weight=0.0 then begin
      continue;
     end;

     SampleX:=(x-aTargetX)+KernelX;
     SampleY:=(y-aTargetY)+KernelY;
     SampleAt(SampleX,SampleY);

     // With preserveAlpha the colors are taken straight rather than premultiplied, since the
     // alpha is then left untouched and must not be folded into them.
     if aPreserveAlpha and (Sample[3]>0.0) then begin
      Sums[0]:=Sums[0]+((Sample[0]/Sample[3])*Weight);
      Sums[1]:=Sums[1]+((Sample[1]/Sample[3])*Weight);
      Sums[2]:=Sums[2]+((Sample[2]/Sample[3])*Weight);
     end else if not aPreserveAlpha then begin
      Sums[0]:=Sums[0]+(Sample[0]*Weight);
      Sums[1]:=Sums[1]+(Sample[1]*Weight);
      Sums[2]:=Sums[2]+(Sample[2]*Weight);
     end;
     Sums[3]:=Sums[3]+(Sample[3]*Weight);

    end;
   end;

   if aPreserveAlpha then begin
    aSource.GetPixel(x,y,Sample[0],Sample[1],Sample[2],Sample[3]);
    Alpha:=Sample[3];
   end else begin
    Alpha:=(Sums[3]/Divisor)+aBias;
    if Alpha<0.0 then begin
     Alpha:=0.0;
    end else if Alpha>1.0 then begin
     Alpha:=1.0;
    end;
   end;

   for Channel:=0 to 2 do begin
    Value:=(Sums[Channel]/Divisor)+(aBias*Alpha);
    if Value<0.0 then begin
     Value:=0.0;
    end else if Value>Alpha then begin
     Value:=Alpha;
    end;
    Sums[Channel]:=Value;
   end;

   if aPreserveAlpha then begin
    aDestination.SetPixel(x,y,Sums[0]*Alpha,Sums[1]*Alpha,Sums[2]*Alpha,Alpha);
   end else begin
    aDestination.SetPixel(x,y,Sums[0],Sums[1],Sums[2],Alpha);
   end;

  end;
 end;

end;

// feDisplacementMap, which moves every pixel of the first input by what two channels of the
// second one say.
procedure FilterDisplacementMap(const aFirst,aSecond,aDestination:TpvVectorGraphicsFilterImage;
                                const aScaleX,aScaleY:TpvDouble;
                                const aChannelX,aChannelY:TpvVectorGraphicsFilterChannel);
var x,y:TpvInt32;
    Map:array[0..3] of TpvFloat;
    Straight:array[0..3] of TpvFloat;
    R,G,B,A,ShiftX,ShiftY:TpvFloat;
begin

 for y:=0 to aDestination.Height-1 do begin
  for x:=0 to aDestination.Width-1 do begin

   aSecond.GetPixel(x,y,Map[0],Map[1],Map[2],Map[3]);

   // The channels are read straight, since a premultiplied color would make the displacement
   // depend on the opacity of the map.
   if Map[3]>0.0 then begin
    Straight[0]:=Map[0]/Map[3];
    Straight[1]:=Map[1]/Map[3];
    Straight[2]:=Map[2]/Map[3];
   end else begin
    Straight[0]:=0.0;
    Straight[1]:=0.0;
    Straight[2]:=0.0;
   end;
   Straight[3]:=Map[3];

   // A channel value of one half means no shift at all.
   ShiftX:=aScaleX*(Straight[Ord(aChannelX)]-0.5);
   ShiftY:=aScaleY*(Straight[Ord(aChannelY)]-0.5);

   aFirst.GetPixel(x+round(ShiftX),y+round(ShiftY),R,G,B,A);
   aDestination.SetPixel(x,y,R,G,B,A);

  end;
 end;

end;

const // The lattice of the turbulence generator, with the sizes the specification fixes so that
      // two implementations agree pixel for pixel.
      TurbulenceBSize=256;
      TurbulenceBM=$ff;
      TurbulenceN=4096;

type { TpvVectorGraphicsFilterTurbulenceState }

     // The tables feTurbulence draws its noise from. They depend only on the seed, so they are
     // built once per primitive rather than per pixel.
     TpvVectorGraphicsFilterTurbulenceState=record
      public
       LatticeSelector:array[0..(TurbulenceBSize*2)+1] of TpvInt32;
       Gradient:array[0..3,0..(TurbulenceBSize*2)+1,0..1] of TpvDouble;
     end;

// The random number generator the specification prescribes, which has to be exactly this one for
// the noise to come out the same as everywhere else.
function TurbulenceRandom(var aSeed:TpvInt32):TpvInt32;
const RandomM=2147483647;
      RandomA=16807;
      RandomQ=127773;
      RandomR=2836;
var Value:TpvInt32;
begin

 Value:=(RandomA*(aSeed mod RandomQ))-(RandomR*(aSeed div RandomQ));
 if Value<=0 then begin
  inc(Value,RandomM);
 end;

 aSeed:=Value;
 result:=Value;

end;

procedure TurbulenceInitialize(out aState:TpvVectorGraphicsFilterTurbulenceState;const aSeed:TpvInt32);
var Channel,Index,OtherIndex,Swap:TpvInt32;
    Seed:TpvInt32;
    Length_:TpvDouble;
begin

 Seed:=aSeed;
 if Seed<=0 then begin
  Seed:=-(Seed mod (2147483647-1))+1;
 end;
 if Seed>(2147483647-1) then begin
  Seed:=2147483647-1;
 end;

 // The channel is the outer loop and the lattice point the inner one, which is the order the
 // specification draws its random numbers in. Turning the two around gives noise that is just as
 // plausible to look at and agrees with nothing.
 for Channel:=0 to 3 do begin
  for Index:=0 to TurbulenceBSize-1 do begin

   aState.LatticeSelector[Index]:=Index;

   // Two numbers between minus one and one, put onto the unit circle.
   aState.Gradient[Channel,Index,0]:=((TurbulenceRandom(Seed) mod (TurbulenceBSize+TurbulenceBSize))-TurbulenceBSize)/TurbulenceBSize;
   aState.Gradient[Channel,Index,1]:=((TurbulenceRandom(Seed) mod (TurbulenceBSize+TurbulenceBSize))-TurbulenceBSize)/TurbulenceBSize;
   Length_:=sqrt(sqr(aState.Gradient[Channel,Index,0])+sqr(aState.Gradient[Channel,Index,1]));
   if Length_>1e-12 then begin
    aState.Gradient[Channel,Index,0]:=aState.Gradient[Channel,Index,0]/Length_;
    aState.Gradient[Channel,Index,1]:=aState.Gradient[Channel,Index,1]/Length_;
   end;

  end;
 end;

 // Shuffle the lattice, then repeat the whole thing so that the lookups never have to wrap. The
 // loop of the specification decrements before it tests, and the counter it inherits stands at
 // the size of the lattice, so the first entry it touches is the last one.
 for Index:=TurbulenceBSize-1 downto 1 do begin
  OtherIndex:=TurbulenceRandom(Seed) mod TurbulenceBSize;
  Swap:=aState.LatticeSelector[Index];
  aState.LatticeSelector[Index]:=aState.LatticeSelector[OtherIndex];
  aState.LatticeSelector[OtherIndex]:=Swap;
 end;

 for Index:=0 to TurbulenceBSize+1 do begin
  aState.LatticeSelector[TurbulenceBSize+Index]:=aState.LatticeSelector[Index];
  for Channel:=0 to 3 do begin
   aState.Gradient[Channel,TurbulenceBSize+Index,0]:=aState.Gradient[Channel,Index,0];
   aState.Gradient[Channel,TurbulenceBSize+Index,1]:=aState.Gradient[Channel,Index,1];
  end;
 end;

end;

// One octave of Perlin noise at a point, for one channel.
function TurbulenceNoise2(const aState:TpvVectorGraphicsFilterTurbulenceState;
                          const aChannel:TpvInt32;const aX,aY:TpvDouble):TpvDouble;
var bx0,bx1,by0,by1,b00,b10,b01,b11,Index:TpvInt32;
    rx0,rx1,ry0,ry1,sx,sy,a,b,t,u,v:TpvDouble;
begin

 t:=aX+TurbulenceN;
 bx0:=trunc(t) and TurbulenceBM;
 bx1:=(bx0+1) and TurbulenceBM;
 rx0:=t-trunc(t);
 rx1:=rx0-1.0;

 t:=aY+TurbulenceN;
 by0:=trunc(t) and TurbulenceBM;
 by1:=(by0+1) and TurbulenceBM;
 ry0:=t-trunc(t);
 ry1:=ry0-1.0;

 Index:=aState.LatticeSelector[bx0];
 b00:=aState.LatticeSelector[Index+by0];
 b10:=aState.LatticeSelector[Index+by1];
 Index:=aState.LatticeSelector[bx1];
 b01:=aState.LatticeSelector[Index+by0];
 b11:=aState.LatticeSelector[Index+by1];

 // The smoothstep the specification uses, so that the lattice does not show as a grid.
 sx:=rx0*rx0*(3.0-(2.0*rx0));
 sy:=ry0*ry0*(3.0-(2.0*ry0));

 u:=(rx0*aState.Gradient[aChannel,b00,0])+(ry0*aState.Gradient[aChannel,b00,1]);
 v:=(rx1*aState.Gradient[aChannel,b01,0])+(ry0*aState.Gradient[aChannel,b01,1]);
 a:=u+(sx*(v-u));

 u:=(rx0*aState.Gradient[aChannel,b10,0])+(ry1*aState.Gradient[aChannel,b10,1]);
 v:=(rx1*aState.Gradient[aChannel,b11,0])+(ry1*aState.Gradient[aChannel,b11,1]);
 b:=u+(sx*(v-u));

 result:=a+(sy*(b-a));

end;

// The sum over the octaves, in either of the two shapes feTurbulence offers.
function TurbulenceSum(const aState:TpvVectorGraphicsFilterTurbulenceState;
                       const aChannel:TpvInt32;const aX,aY:TpvDouble;
                       const aOctaves:TpvInt32;const aFractalNoise:boolean):TpvDouble;
var Octave:TpvInt32;
    Sum,Ratio,PointX,PointY:TpvDouble;
begin

 Sum:=0.0;
 PointX:=aX;
 PointY:=aY;
 Ratio:=1.0;

 for Octave:=1 to aOctaves do begin

  // The fractal form keeps the sign, the turbulence form folds it, which is what gives the
  // latter its creased look.
  if aFractalNoise then begin
   Sum:=Sum+(TurbulenceNoise2(aState,aChannel,PointX,PointY)/Ratio);
  end else begin
   Sum:=Sum+(abs(TurbulenceNoise2(aState,aChannel,PointX,PointY))/Ratio);
  end;

  PointX:=PointX*2.0;
  PointY:=PointY*2.0;
  Ratio:=Ratio*2.0;

 end;

 result:=Sum;

end;

// feTurbulence, which fills its subregion with noise rather than reading any input at all.
procedure FilterTurbulence(const aDestination:TpvVectorGraphicsFilterImage;
                           const aBaseFrequencyX,aBaseFrequencyY:TpvDouble;
                           const aOctaves,aSeed:TpvInt32;const aFractalNoise:boolean;
                           const aOriginX,aOriginY:TpvDouble;
                           const aScaleX,aScaleY:TpvDouble);
var State:TpvVectorGraphicsFilterTurbulenceState;
    x,y,Channel:TpvInt32;
    PointX,PointY,Value:TpvDouble;
    Channels:array[0..3] of TpvFloat;
begin

 if (aBaseFrequencyX<=0.0) and (aBaseFrequencyY<=0.0) then begin
  exit;
 end;

 TurbulenceInitialize(State,aSeed);

 for y:=0 to aDestination.Height-1 do begin
  for x:=0 to aDestination.Width-1 do begin

   // The noise is defined in user space, so the device pixel has to be taken back into it first.
   PointX:=(aOriginX+(x/aScaleX))*aBaseFrequencyX;
   PointY:=(aOriginY+(y/aScaleY))*aBaseFrequencyY;

   for Channel:=0 to 3 do begin

    Value:=TurbulenceSum(State,Channel,PointX,PointY,aOctaves,aFractalNoise);

    // The fractal form runs from minus one to one and is moved into the unit range, the other
    // one is already there.
    if aFractalNoise then begin
     Value:=(Value+1.0)*0.5;
    end;

    if Value<0.0 then begin
     Value:=0.0;
    end else if Value>1.0 then begin
     Value:=1.0;
    end;

    Channels[Channel]:=Value;

   end;

   // The result is given as straight colors, so it is premultiplied before being stored.
   aDestination.SetPixel(x,y,
                         Channels[0]*Channels[3],
                         Channels[1]*Channels[3],
                         Channels[2]*Channels[3],
                         Channels[3]);

  end;
 end;

end;

// The two lighting primitives, which differ only in how they turn the light into a color.
procedure FilterLighting(const aSource,aDestination:TpvVectorGraphicsFilterImage;
                         const aPrimitive:TpvVectorGraphicsFilterPrimitive;
                         const aSpecular:boolean;
                         const aOriginX,aOriginY:TpvDouble;
                         const aScaleX,aScaleY:TpvDouble;
                         const aColorSpace:TpvVectorGraphicsFilterColorSpace);
const OneOver255=1.0/255.0;
var x,y,Channel:TpvInt32;
    NormalX,NormalY,NormalZ,Length_:TpvDouble;
    LightX,LightY,LightZ,HalfwayZ,HalfwayLength:TpvDouble;
    SurfaceZ,DotProduct,Factor,Value:TpvDouble;
    LightRed,LightGreen,LightBlue:TpvDouble;
    Colors:array[0..3] of TpvFloat;
    ConeCosine,SpotFactor:TpvDouble;
    PointX,PointY:TpvDouble;

 // The alpha channel is the height map, so this reads it with everything outside counting as
 // flat ground.
 function AlphaAt(const aX,aY:TpvInt32):TpvDouble;
 var R,G,B,A:TpvFloat;
 begin
  aSource.GetPixel(aX,aY,R,G,B,A);
  result:=A;
 end;

begin

 // Unlike a flood color, the light color is taken as it stands rather than being brought out of
 // sRGB first, so its numbers are read as values of whatever space the filter works in. That is
 // what the reference renderers do, and since an author picks the color by eye against one of
 // them, matching it matters more here than the other reading of the specification would.
 LightRed:=((aPrimitive.LightColor shr 16) and $ff)*OneOver255;
 LightGreen:=((aPrimitive.LightColor shr 8) and $ff)*OneOver255;
 LightBlue:=(aPrimitive.LightColor and $ff)*OneOver255;

 for y:=0 to aDestination.Height-1 do begin
  for x:=0 to aDestination.Width-1 do begin

   // The surface normal comes from the slope of the alpha channel, with the Sobel pair the
   // specification gives for the inner pixels.
   NormalX:=-aPrimitive.SurfaceScale*0.25*(((AlphaAt(x+1,y-1)+(2.0*AlphaAt(x+1,y)))+AlphaAt(x+1,y+1))-
                                           ((AlphaAt(x-1,y-1)+(2.0*AlphaAt(x-1,y)))+AlphaAt(x-1,y+1)));
   NormalY:=-aPrimitive.SurfaceScale*0.25*(((AlphaAt(x-1,y+1)+(2.0*AlphaAt(x,y+1)))+AlphaAt(x+1,y+1))-
                                           ((AlphaAt(x-1,y-1)+(2.0*AlphaAt(x,y-1)))+AlphaAt(x+1,y-1)));
   NormalZ:=1.0;

   Length_:=sqrt((sqr(NormalX)+sqr(NormalY))+sqr(NormalZ));
   if Length_>1e-12 then begin
    NormalX:=NormalX/Length_;
    NormalY:=NormalY/Length_;
    NormalZ:=NormalZ/Length_;
   end;

   SurfaceZ:=aPrimitive.SurfaceScale*AlphaAt(x,y);

   // The point in user space, which is where the lights are given.
   PointX:=aOriginX+(x/aScaleX);
   PointY:=aOriginY+(y/aScaleY);

   SpotFactor:=1.0;

   case aPrimitive.LightKind of

    TpvVectorGraphicsFilterLightKind.Point,
    TpvVectorGraphicsFilterLightKind.Spot:begin

     LightX:=aPrimitive.LightX-PointX;
     LightY:=aPrimitive.LightY-PointY;
     LightZ:=aPrimitive.LightZ-SurfaceZ;
     Length_:=sqrt((sqr(LightX)+sqr(LightY))+sqr(LightZ));
     if Length_>1e-12 then begin
      LightX:=LightX/Length_;
      LightY:=LightY/Length_;
      LightZ:=LightZ/Length_;
     end;

     // A spot additionally falls off towards the edge of its cone.
     if aPrimitive.LightKind=TpvVectorGraphicsFilterLightKind.Spot then begin

      DotProduct:=aPrimitive.LightPointsAtX-aPrimitive.LightX;
      Factor:=aPrimitive.LightPointsAtY-aPrimitive.LightY;
      Value:=aPrimitive.LightPointsAtZ-aPrimitive.LightZ;
      Length_:=sqrt((sqr(DotProduct)+sqr(Factor))+sqr(Value));
      if Length_>1e-12 then begin
       DotProduct:=DotProduct/Length_;
       Factor:=Factor/Length_;
       Value:=Value/Length_;
      end;

      // The cosine between the direction of the spot and the one back towards the surface.
      ConeCosine:=((-LightX)*DotProduct)+((-LightY)*Factor)+((-LightZ)*Value);
      if ConeCosine<=0.0 then begin
       SpotFactor:=0.0;
      end else begin
       SpotFactor:=Power(ConeCosine,aPrimitive.SpecularExponent);
       if aPrimitive.HasLightLimitingConeAngle and
          (ConeCosine<cos(aPrimitive.LightLimitingConeAngle*(PI/180.0))) then begin
        SpotFactor:=0.0;
       end;
      end;

     end;

    end;

    else {TpvVectorGraphicsFilterLightKind.Distant:}begin
     // A distant light has one direction for the whole surface.
     LightX:=cos(aPrimitive.LightAzimuth*(PI/180.0))*cos(aPrimitive.LightElevation*(PI/180.0));
     LightY:=sin(aPrimitive.LightAzimuth*(PI/180.0))*cos(aPrimitive.LightElevation*(PI/180.0));
     LightZ:=sin(aPrimitive.LightElevation*(PI/180.0));
    end;

   end;

   if aSpecular then begin

    // The halfway vector between the light and the eye, which looks straight down.
    HalfwayZ:=LightZ+1.0;
    HalfwayLength:=sqrt((sqr(LightX)+sqr(LightY))+sqr(HalfwayZ));
    if HalfwayLength>1e-12 then begin
     DotProduct:=(((NormalX*LightX)+(NormalY*LightY))+(NormalZ*HalfwayZ))/HalfwayLength;
    end else begin
     DotProduct:=0.0;
    end;
    if DotProduct<0.0 then begin
     DotProduct:=0.0;
    end;

    Factor:=aPrimitive.SpecularConstant*Power(DotProduct,aPrimitive.SpecularExponent)*SpotFactor;

    // The specular form carries its own alpha, which is the largest of the three channels.
    Colors[0]:=Max(0.0,Min(1.0,Factor*LightRed));
    Colors[1]:=Max(0.0,Min(1.0,Factor*LightGreen));
    Colors[2]:=Max(0.0,Min(1.0,Factor*LightBlue));
    Colors[3]:=Max(Colors[0],Max(Colors[1],Colors[2]));

    // Already premultiplied in effect, since the alpha is the largest channel.
    aDestination.SetPixel(x,y,Colors[0],Colors[1],Colors[2],Colors[3]);

   end else begin

    DotProduct:=((NormalX*LightX)+(NormalY*LightY))+(NormalZ*LightZ);
    if DotProduct<0.0 then begin
     DotProduct:=0.0;
    end;

    Factor:=aPrimitive.DiffuseConstant*DotProduct*SpotFactor;

    // The diffuse form is fully opaque, so straight and premultiplied are the same thing.
    for Channel:=0 to 2 do begin
     case Channel of
      0:begin
       Value:=Factor*LightRed;
      end;
      1:begin
       Value:=Factor*LightGreen;
      end;
      else begin
       Value:=Factor*LightBlue;
      end;
     end;
     Colors[Channel]:=Max(0.0,Min(1.0,Value));
    end;

    aDestination.SetPixel(x,y,Colors[0],Colors[1],Colors[2],1.0);

   end;

  end;
 end;

end;

procedure pvVectorGraphicsApplyFilter(const aFilter:TpvVectorGraphicsFilter;
                                      const aBuffer:TpvVectorGraphicsPixelBuffer;
                                      const aContext:TpvVectorGraphicsFilterContext);
var RegionX0,RegionY0,RegionX1,RegionY1,RegionWidth,RegionHeight:TpvInt32;
    PrimitiveIndex,Index:TpvSizeInt;
    Primitive:TpvVectorGraphicsFilterPrimitive;
    SourceGraphic,SourceAlpha,Last,First,Second,Output:TpvVectorGraphicsFilterImage;
    ResultNames:array of TpvUTF8String;
    ResultImages:array of TpvVectorGraphicsFilterImage;
    // The subregion each result was clipped to, which feTile needs in order to know what its
    // input offers as a tile.
    ResultRects:array of array[0..3] of TpvInt32;
    CountResults:TpvSizeInt;
    TileX0,TileY0,TileX1,TileY1:TpvInt32;
    SubX0,SubY0,SubX1,SubY1:TpvInt32;
    Deviation,Delta:TpvDouble;
    x,y:TpvInt32;
    R,G,B,A:TpvFloat;
    Scratch:TpvVectorGraphicsFilterImage;

 // Turns a length of the filter region into device pixels. For bounding box units the numbers
 // are fractions of the box, otherwise they are user units.
 function RegionValue(const aValue:TpvDouble;const aIsX,aIsSize:boolean):TpvDouble;
 begin
  if aFilter.FilterUnits=TpvVectorGraphicsFilterUnits.ObjectBoundingBox then begin
   if aIsX then begin
    result:=aValue*aContext.BoundingBoxWidth;
    if not aIsSize then begin
     result:=result+aContext.BoundingBoxX;
    end;
   end else begin
    result:=aValue*aContext.BoundingBoxHeight;
    if not aIsSize then begin
     result:=result+aContext.BoundingBoxY;
    end;
   end;
  end else begin
   if aIsX then begin
    result:=aValue*aContext.ScaleX;
   end else begin
    result:=aValue*aContext.ScaleY;
   end;
  end;
 end;

 // The same for the subregion of a primitive, which follows primitiveUnits instead.
 function PrimitiveValue(const aValue:TpvDouble;const aIsX,aIsSize:boolean):TpvDouble;
 begin
  if aFilter.PrimitiveUnits=TpvVectorGraphicsFilterUnits.ObjectBoundingBox then begin
   if aIsX then begin
    if aIsSize then begin
     result:=aValue*aContext.BoundingBoxWidth;
    end else begin
     result:=aContext.BoundingBoxX+(aValue*aContext.BoundingBoxWidth);
    end;
   end else begin
    if aIsSize then begin
     result:=aValue*aContext.BoundingBoxHeight;
    end else begin
     result:=aContext.BoundingBoxY+(aValue*aContext.BoundingBoxHeight);
    end;
   end;
  end else begin
   if aIsX then begin
    result:=aValue*aContext.ScaleX;
   end else begin
    result:=aValue*aContext.ScaleY;
   end;
  end;
 end;

 // A length that is not tied to an axis, such as a standard deviation, in primitive units.
 function PrimitiveLengthX(const aValue:TpvDouble):TpvDouble;
 begin
  if aFilter.PrimitiveUnits=TpvVectorGraphicsFilterUnits.ObjectBoundingBox then begin
   result:=aValue*aContext.BoundingBoxWidth;
  end else begin
   result:=aValue*aContext.ScaleX;
  end;
 end;

 function PrimitiveLengthY(const aValue:TpvDouble):TpvDouble;
 begin
  if aFilter.PrimitiveUnits=TpvVectorGraphicsFilterUnits.ObjectBoundingBox then begin
   result:=aValue*aContext.BoundingBoxHeight;
  end else begin
   result:=aValue*aContext.ScaleY;
  end;
 end;

 // Looks up what an in attribute names, falling back to what the previous primitive produced.
 function ResolveInput(const aName:TpvUTF8String):TpvVectorGraphicsFilterImage;
 var Search:TpvSizeInt;
 begin

  if Length(aName)=0 then begin
   result:=Last;
   exit;
  end;

  if aName='SourceGraphic' then begin
   result:=SourceGraphic;
   exit;
  end;

  if aName='SourceAlpha' then begin
   result:=SourceAlpha;
   exit;
  end;

  // BackgroundImage, BackgroundAlpha, FillPaint and StrokePaint would need the page behind the
  // element and an endless plane of paint. No renderer in the wild implements them either, and
  // the specification allows treating them as transparent black, which is what happens here.
  if (aName='BackgroundImage') or (aName='BackgroundAlpha') or
     (aName='FillPaint') or (aName='StrokePaint') then begin
   result:=nil;
   exit;
  end;

  for Search:=0 to CountResults-1 do begin
   if ResultNames[Search]=aName then begin
    result:=ResultImages[Search];
    exit;
  end;
  end;

  // A name that was never produced counts as the previous result, which is what browsers do.
  result:=Last;

 end;

 // Keeps an image alive until the end of the run, under the name a later primitive can ask for it
 // by. A name of one null character belongs to nothing that can be referenced and only says that
 // the image has to be freed with the rest.
 procedure StoreResult(const aName:TpvUTF8String;const aImage:TpvVectorGraphicsFilterImage;
                       const aX0,aY0,aX1,aY1:TpvInt32);
 begin
  SetLength(ResultNames,CountResults+1);
  SetLength(ResultImages,CountResults+1);
  SetLength(ResultRects,CountResults+1);
  ResultNames[CountResults]:=aName;
  ResultImages[CountResults]:=aImage;
  ResultRects[CountResults,0]:=aX0;
  ResultRects[CountResults,1]:=aY0;
  ResultRects[CountResults,2]:=aX1;
  ResultRects[CountResults,3]:=aY1;
  inc(CountResults);
 end;

 // The subregion that belongs to an input, which is the whole filter region for everything that
 // did not come out of a primitive.
 procedure ResolveInputRect(const aName:TpvUTF8String;out aX0,aY0,aX1,aY1:TpvInt32);
 var Search:TpvSizeInt;
 begin

  aX0:=0;
  aY0:=0;
  aX1:=RegionWidth;
  aY1:=RegionHeight;

  if Length(aName)=0 then begin
   // The previous result, which is the last entry that was put away.
   if CountResults>0 then begin
    aX0:=ResultRects[CountResults-1,0];
    aY0:=ResultRects[CountResults-1,1];
    aX1:=ResultRects[CountResults-1,2];
    aY1:=ResultRects[CountResults-1,3];
   end;
   exit;
  end;

  for Search:=0 to CountResults-1 do begin
   if ResultNames[Search]=aName then begin
    aX0:=ResultRects[Search,0];
    aY0:=ResultRects[Search,1];
    aX1:=ResultRects[Search,2];
    aY1:=ResultRects[Search,3];
    exit;
   end;
  end;

 end;

begin

 if (not assigned(aFilter)) or (not assigned(aBuffer)) or (aFilter.Primitives.Count=0) then begin
  exit;
 end;

 // The filter region in device pixels, clipped to what the buffer actually holds, since nothing
 // outside it could be shown anyway.
 RegionX0:=trunc(Floor(RegionValue(aFilter.x,true,false)));
 RegionY0:=trunc(Floor(RegionValue(aFilter.y,false,false)));
 RegionX1:=RegionX0+trunc(Ceil(RegionValue(aFilter.Width,true,true)));
 RegionY1:=RegionY0+trunc(Ceil(RegionValue(aFilter.Height,false,true)));

 if RegionX0<0 then begin
  RegionX0:=0;
 end;
 if RegionY0<0 then begin
  RegionY0:=0;
 end;
 if RegionX1>aBuffer.Width then begin
  RegionX1:=aBuffer.Width;
 end;
 if RegionY1>aBuffer.Height then begin
  RegionY1:=aBuffer.Height;
 end;

 RegionWidth:=RegionX1-RegionX0;
 RegionHeight:=RegionY1-RegionY0;

 // An empty region means the filter produces nothing at all, which is still something: whatever
 // was drawn has to go.
 if (RegionWidth<=0) or (RegionHeight<=0) then begin
  aBuffer.Clear(0);
  exit;
 end;

 SourceGraphic:=nil;
 SourceAlpha:=nil;
 Scratch:=nil;
 ResultNames:=nil;
 ResultImages:=nil;
 ResultRects:=nil;
 CountResults:=0;

 try

  SourceGraphic:=TpvVectorGraphicsFilterImage.Create(RegionWidth,RegionHeight);
  SourceGraphic.LoadFrom(aBuffer,RegionX0,RegionY0,aFilter.ColorSpace);

  // SourceAlpha is the same thing with the colors thrown away, which is what makes a drop shadow
  // take the shape of the element and not its colors.
  SourceAlpha:=TpvVectorGraphicsFilterImage.Create(RegionWidth,RegionHeight);
  for y:=0 to RegionHeight-1 do begin
   for x:=0 to RegionWidth-1 do begin
    SourceGraphic.GetPixel(x,y,R,G,B,A);
    SourceAlpha.SetPixel(x,y,0.0,0.0,0.0,A);
   end;
  end;

  Last:=SourceGraphic;

  for PrimitiveIndex:=0 to aFilter.Primitives.Count-1 do begin

   Primitive:=aFilter.Primitives[PrimitiveIndex];

   // The first primitive without an in attribute reads the source, every later one reads what
   // came before it, which ResolveInput already does through Last.
   if (PrimitiveIndex=0) and (Length(Primitive.Input1)=0) then begin
    First:=SourceGraphic;
   end else begin
    First:=ResolveInput(Primitive.Input1);
   end;
   Second:=ResolveInput(Primitive.Input2);

   // An input that stands for something not supported is an empty image rather than nothing, so
   // that the loops below never have to test for it. It goes into the list under no name, which
   // keeps it alive until the end without anything being able to ask for it again.
   // The subregion of the first input, which only feTile looks at.
   ResolveInputRect(Primitive.Input1,TileX0,TileY0,TileX1,TileY1);

   if not assigned(First) then begin
    First:=TpvVectorGraphicsFilterImage.Create(RegionWidth,RegionHeight);
    StoreResult(#0,First,0,0,RegionWidth,RegionHeight);
   end;
   if not assigned(Second) then begin
    Second:=First;
   end;

   Output:=TpvVectorGraphicsFilterImage.Create(RegionWidth,RegionHeight);

   // The subregion, which both narrows what the primitive writes and, for the ones that make
   // their own pixels, says where they go.
   if Primitive.HasX then begin
    SubX0:=trunc(Floor(PrimitiveValue(Primitive.x,true,false)))-RegionX0;
   end else begin
    SubX0:=0;
   end;
   if Primitive.HasY then begin
    SubY0:=trunc(Floor(PrimitiveValue(Primitive.y,false,false)))-RegionY0;
   end else begin
    SubY0:=0;
   end;
   if Primitive.HasWidth then begin
    SubX1:=SubX0+trunc(Ceil(PrimitiveValue(Primitive.Width,true,true)));
   end else begin
    SubX1:=RegionWidth;
   end;
   if Primitive.HasHeight then begin
    SubY1:=SubY0+trunc(Ceil(PrimitiveValue(Primitive.Height,false,true)));
   end else begin
    SubY1:=RegionHeight;
   end;

   // The output goes into the list, under its own name when it has one, so that everything this
   // run created is freed in one place at the end and feTile can find what it needs.
   if Length(Primitive.ResultName)>0 then begin
    StoreResult(Primitive.ResultName,Output,SubX0,SubY0,SubX1,SubY1);
   end else begin
    StoreResult(#0,Output,SubX0,SubY0,SubX1,SubY1);
   end;

   case Primitive.Kind of

    TpvVectorGraphicsFilterPrimitiveKind.Blend:begin
     FilterBlend(First,Second,Output,Primitive.BlendMode);
    end;

    TpvVectorGraphicsFilterPrimitiveKind.ColorMatrix:begin
     FilterColorMatrix(First,Output,Primitive.fColorMatrixValues);
    end;

    TpvVectorGraphicsFilterPrimitiveKind.ComponentTransfer:begin
     FilterComponentTransfer(First,Output,Primitive.fTransferFunctions);
    end;

    TpvVectorGraphicsFilterPrimitiveKind.Composite:begin
     FilterComposite(First,Second,Output,Primitive.CompositeOperator,
                     Primitive.K1,Primitive.K2,Primitive.K3,Primitive.K4);
    end;

    TpvVectorGraphicsFilterPrimitiveKind.ConvolveMatrix:begin
     FilterConvolveMatrix(First,Output,Primitive.OrderX,Primitive.OrderY,Primitive.fKernel,
                          Primitive.Divisor,Primitive.Bias,
                          Primitive.TargetX,Primitive.TargetY,
                          Primitive.EdgeMode,Primitive.PreserveAlpha);
    end;

    TpvVectorGraphicsFilterPrimitiveKind.DiffuseLighting:begin
     FilterLighting(First,Output,Primitive,false,
                    RegionX0/aContext.ScaleX,RegionY0/aContext.ScaleY,
                    aContext.ScaleX,aContext.ScaleY,aFilter.ColorSpace);
    end;

    TpvVectorGraphicsFilterPrimitiveKind.DisplacementMap:begin
     FilterDisplacementMap(First,Second,Output,
                           PrimitiveLengthX(Primitive.Scale),PrimitiveLengthY(Primitive.Scale),
                           Primitive.ChannelSelectorX,Primitive.ChannelSelectorY);
    end;

    TpvVectorGraphicsFilterPrimitiveKind.DropShadow:begin

     // A drop shadow is a blur of the alpha, moved, colored and put behind the original, which
     // the specification spells out as exactly that sequence.
     Scratch:=TpvVectorGraphicsFilterImage.Create(RegionWidth,RegionHeight);
     try

      for y:=0 to RegionHeight-1 do begin
       for x:=0 to RegionWidth-1 do begin
        First.GetPixel(x,y,R,G,B,A);
        Scratch.SetPixel(x,y,0.0,0.0,0.0,A);
       end;
      end;

      FilterGaussianBlur(Scratch,Output,
                         PrimitiveLengthX(Primitive.StandardDeviationX),
                         PrimitiveLengthY(Primitive.StandardDeviationY));
      FilterOffset(Output,Scratch,
                   round(PrimitiveLengthX(Primitive.DeltaX)),
                   round(PrimitiveLengthY(Primitive.DeltaY)));

      // The blurred alpha becomes the shape the flood color is poured into.
      FilterFlood(Output,Primitive.FloodColor,Primitive.FloodOpacity,aFilter.ColorSpace);
      FilterComposite(Output,Scratch,Output,TpvVectorGraphicsFilterCompositeOperator.In_,0.0,0.0,0.0,0.0);

      // And the original goes on top of it.
      FilterComposite(First,Output,Output,TpvVectorGraphicsFilterCompositeOperator.Over,0.0,0.0,0.0,0.0);

     finally
      FreeAndNil(Scratch);
     end;

    end;

    TpvVectorGraphicsFilterPrimitiveKind.Flood:begin
     FilterFlood(Output,Primitive.FloodColor,Primitive.FloodOpacity,aFilter.ColorSpace);
    end;

    TpvVectorGraphicsFilterPrimitiveKind.GaussianBlur:begin
     Deviation:=PrimitiveLengthX(Primitive.StandardDeviationX);
     Delta:=PrimitiveLengthY(Primitive.StandardDeviationY);
     FilterGaussianBlur(First,Output,Deviation,Delta);
    end;

    TpvVectorGraphicsFilterPrimitiveKind.Image:begin
     if assigned(Primitive.Image) then begin
      Output.LoadFrom(Primitive.Image,-SubX0,-SubY0,aFilter.ColorSpace);
     end;
    end;

    TpvVectorGraphicsFilterPrimitiveKind.Merge:begin

     // Every input in turn, each one drawn over what is there already.
     Scratch:=TpvVectorGraphicsFilterImage.Create(RegionWidth,RegionHeight);
     try
      for Index:=0 to Length(Primitive.fMergeInputs)-1 do begin
       Second:=ResolveInput(Primitive.fMergeInputs[Index]);
       if assigned(Second) then begin
        FilterComposite(Second,Output,Scratch,TpvVectorGraphicsFilterCompositeOperator.Over,0.0,0.0,0.0,0.0);
        Output.Assign(Scratch);
       end;
      end;
     finally
      FreeAndNil(Scratch);
     end;

    end;

    TpvVectorGraphicsFilterPrimitiveKind.Morphology:begin
     FilterMorphology(First,Output,
                      round(PrimitiveLengthX(Primitive.RadiusX)),
                      round(PrimitiveLengthY(Primitive.RadiusY)),
                      Primitive.MorphologyOperator);
    end;

    TpvVectorGraphicsFilterPrimitiveKind.Offset:begin
     FilterOffset(First,Output,
                  round(PrimitiveLengthX(Primitive.DeltaX)),
                  round(PrimitiveLengthY(Primitive.DeltaY)));
    end;

    TpvVectorGraphicsFilterPrimitiveKind.SpecularLighting:begin
     FilterLighting(First,Output,Primitive,true,
                    RegionX0/aContext.ScaleX,RegionY0/aContext.ScaleY,
                    aContext.ScaleX,aContext.ScaleY,aFilter.ColorSpace);
    end;

    TpvVectorGraphicsFilterPrimitiveKind.Tile:begin
     // What gets repeated is the subregion of the input, not the one of this primitive, which is
     // where the tile goes.
     FilterTile(First,Output,Max(0,TileX0),Max(0,TileY0),Min(RegionWidth,TileX1),Min(RegionHeight,TileY1));
    end;

    TpvVectorGraphicsFilterPrimitiveKind.Turbulence:begin
     FilterTurbulence(Output,Primitive.BaseFrequencyX,Primitive.BaseFrequencyY,
                      Primitive.NumOctaves,Primitive.Seed,Primitive.FractalNoise,
                      RegionX0/aContext.ScaleX,RegionY0/aContext.ScaleY,
                      aContext.ScaleX,aContext.ScaleY);
    end;

    else begin
     Output.Assign(First);
    end;

   end;

   Output.ClipToRectangle(SubX0,SubY0,SubX1,SubY1);

   Last:=Output;

  end;

  // Nothing outside the filter region is drawn at all, so the buffer is cleared before the
  // result goes back in.
  aBuffer.Clear(0);
  if assigned(Last) then begin
   Last.StoreTo(aBuffer,RegionX0,RegionY0,aFilter.ColorSpace);
  end;

 finally

  for Index:=0 to CountResults-1 do begin
   ResultImages[Index].Free;
  end;
  ResultImages:=nil;
  ResultNames:=nil;
  ResultRects:=nil;

  FreeAndNil(SourceGraphic);
  FreeAndNil(SourceAlpha);

 end;

end;

end.
