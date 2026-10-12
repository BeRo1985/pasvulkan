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
unit PasVulkan.VectorGraphics;
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
     PasVulkan.Collections,
     PasVulkan.TrueTypeFont,
     PasVulkan.VectorPath,
     PasVulkan.VectorGraphics.Rasterizer,
     PasVulkan.VectorGraphics.Paints;

const // Nesting depth beyond which a group that contains itself, directly or through a use
      // element, stops recursing instead of running out of stack.
      pvVectorGraphicsMaximumNestingLevel=3;

type EpvVectorGraphics=class(Exception);

     TpvVectorGraphicsDocument=class;

     TpvVectorGraphicsObject=class;

     TpvVectorGraphicsGroup=class;

     { TpvVectorGraphicsSimpleMatrix }

     // The transform of an object, kept in its parts rather than as a baked matrix, so that an
     // editor and the timeline can interpolate rotation and scale separately. Matrix holds an
     // extra free-form transform that SVG's transform attribute lands in.
     TpvVectorGraphicsSimpleMatrix=record
      public
       Matrix:TpvVectorPathMatrix;
       TranslateX:TpvDouble;
       TranslateY:TpvDouble;
       ScaleX:TpvDouble;
       ScaleY:TpvDouble;
       RotationDegrees:TpvDouble;
       SkewX:TpvDouble;
       SkewY:TpvDouble;
      public
       class function CreateDefault:TpvVectorGraphicsSimpleMatrix; static;
       // Composes the parts into one matrix. aOffsetX and aOffsetY move the pivot that rotation,
       // skew and scale act around.
       function ToMatrix(const aOffsetX:TpvDouble=0.0;const aOffsetY:TpvDouble=0.0):TpvVectorPathMatrix;
     end;

     PpvVectorGraphicsSimpleMatrix=^TpvVectorGraphicsSimpleMatrix;

     PpvVectorGraphicsInterpolationMode=^TpvVectorGraphicsInterpolationMode;
     TpvVectorGraphicsInterpolationMode=
      (
       Nearest=0,
       Linear,
       Cosine,
       Cubic,
       Hermite
      );

     PpvVectorGraphicsFillMode=^TpvVectorGraphicsFillMode;
     TpvVectorGraphicsFillMode=
      (
       None=0,
       Color,
       Image,
       Pattern,
       RadialGradient,
       LinearGradient
      );

     PpvVectorGraphicsMaskMode=^TpvVectorGraphicsMaskMode;
     TpvVectorGraphicsMaskMode=
      (
       None=0,
       // The object is a clip path for its siblings: its coverage, not its color, narrows them.
       Clip,
       // The object is a mask, taken as plain alpha.
       Mono,
       // The object is a mask, taken per color channel.
       Color
      );

     PpvVectorGraphicsTimeLineAction=^TpvVectorGraphicsTimeLineAction;
     TpvVectorGraphicsTimeLineAction=
      (
       None=0,
       Show,
       Hide,
       Transform,
       Move,
       Morph
      );

     { TpvVectorGraphicsBoundingBox }

     TpvVectorGraphicsBoundingBox=record
      public
       x1:TpvDouble;
       y1:TpvDouble;
       x2:TpvDouble;
       y2:TpvDouble;
      public
       class function CreateEmpty:TpvVectorGraphicsBoundingBox; static;
       procedure Extend(const aX,aY:TpvDouble);
       procedure Combine(const aWith:TpvVectorGraphicsBoundingBox);
       function IsEmpty:boolean;
       function Width:TpvDouble; {$ifdef CANINLINE}inline;{$endif}
       function Height:TpvDouble; {$ifdef CANINLINE}inline;{$endif}
     end;

     PpvVectorGraphicsBoundingBox=^TpvVectorGraphicsBoundingBox;

     { TpvVectorGraphicsImageResource }

     // A decoded raster image in the $aarrggbb layout, shared by every object that references
     // it. Owned by the document, not by the objects.
     TpvVectorGraphicsImageResource=class
      private
       fDocument:TpvVectorGraphicsDocument;
       fID:TpvInt32;
       fName:TpvUTF8String;
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fPixels:TpvVectorGraphicsColorArray;
      public
       constructor Create(const aDocument:TpvVectorGraphicsDocument); reintroduce;
       destructor Destroy; override;
       procedure SetSize(const aWidth,aHeight:TpvInt32);
       // Takes pixels that are already in the $aarrggbb layout.
       procedure SetPixels(const aPixels:pointer;const aWidth,aHeight:TpvInt32);
       // Takes R8G8B8A8 pixels, so with red in the lowest byte, and swaps them into place.
       procedure SetPixelsFromRGBA8(const aPixels:pointer;const aWidth,aHeight:TpvInt32);
       function GetPixelPointer:PpvVectorGraphicsRawColorArray;
      published
       property ID:TpvInt32 read fID write fID;
       property Name:TpvUTF8String read fName write fName;
       property Width:TpvInt32 read fWidth;
       property Height:TpvInt32 read fHeight;
     end;

     TpvVectorGraphicsImageResourceList=TpvObjectGenericList<TpvVectorGraphicsImageResource>;

     { TpvVectorGraphicsFillData }

     // Everything that decides what color the inside, or the stroke, of a shape gets. A single
     // record serves both, because a stroke in SVG can carry any paint a fill can.
     TpvVectorGraphicsFillData=record
      public
       Mode:TpvVectorGraphicsFillMode;
       Color:TpvVectorGraphicsColor;
       Image:TpvVectorGraphicsImageResource;
       SimpleMatrix:TpvVectorGraphicsSimpleMatrix;
       GradientFocusSimpleMatrix:TpvVectorGraphicsSimpleMatrix;
       GradientFocusX:TpvDouble;
       GradientFocusY:TpvDouble;
       GradientRadius:TpvDouble;
       GradientStops:TpvVectorGraphicsGradientStops;
       SpreadMethod:TpvVectorGraphicsSpreadMethod;
       FillRule:TpvVectorPathFillRule;
       ClipRule:TpvVectorPathFillRule;
       Pattern:TpvVectorGraphicsGroup;
      public
       class function CreateDefault:TpvVectorGraphicsFillData; static;
       procedure Assign(const aFrom:TpvVectorGraphicsFillData);
     end;

     PpvVectorGraphicsFillData=^TpvVectorGraphicsFillData;

     { TpvVectorGraphicsStyle }

     TpvVectorGraphicsStyle=record
      public
       FillData:TpvVectorGraphicsFillData;
       StrokeFillData:TpvVectorGraphicsFillData;
       StrokeCapMode:TpvVectorPathStrokeCapMode;
       StrokeJoinMode:TpvVectorPathStrokeJoinMode;
       StrokeInnerJoinMode:TpvVectorPathStrokeInnerJoinMode;
       StrokeWidth:TpvDouble;
       // When set, the stroke keeps its width in device pixels instead of scaling with the
       // object, which is what the non-scaling-stroke vector effect asks for.
       StrokeWidthFixed:boolean;
       StrokeMiterLimit:TpvDouble;
       StrokeInnerMiterLimit:TpvDouble;
       StrokeDashArray:TpvVectorPathStrokeDashArray;
       StrokeDashOffset:TpvDouble;
       // Renders the object into its own buffer first and composites that with the object's
       // opacity, so that overlapping fill and stroke do not show through each other.
       OffImage:boolean;
      public
       class function CreateDefault:TpvVectorGraphicsStyle; static;
       procedure Assign(const aFrom:TpvVectorGraphicsStyle);
     end;

     PpvVectorGraphicsStyle=^TpvVectorGraphicsStyle;

     { TpvVectorGraphicsTimeLineItem }

     TpvVectorGraphicsTimeLineItem=record
      public
       StartTick:TpvInt64;
       EndTick:TpvInt64;
       Action:TpvVectorGraphicsTimeLineAction;
       InterpolationMode:TpvVectorGraphicsInterpolationMode;
       TransformSimpleMatrix:TpvVectorGraphicsSimpleMatrix;
       HasAlpha:boolean;
       Alpha:TpvUInt8;
       HasFillColor:boolean;
       FillColor:TpvVectorGraphicsColor;
       HasStrokeColor:boolean;
       StrokeColor:TpvVectorGraphicsColor;
       HasStrokeWidth:boolean;
       StrokeWidth:TpvDouble;
       HasFillSimpleMatrix:boolean;
       FillSimpleMatrix:TpvVectorGraphicsSimpleMatrix;
       HasStrokeFillSimpleMatrix:boolean;
       StrokeFillSimpleMatrix:TpvVectorGraphicsSimpleMatrix;
       // For a morph, the shape the object has at EndTick.
       MorphPath:TpvVectorPath;
     end;

     PpvVectorGraphicsTimeLineItem=^TpvVectorGraphicsTimeLineItem;

     TpvVectorGraphicsTimeLine=array of TpvVectorGraphicsTimeLineItem;

     { TpvVectorGraphicsRenderOverride }

     // Lets a caller recolor an object for one draw call without touching the document, which
     // is what a user interface needs when it draws the same artwork in several states within
     // one frame.
     TpvVectorGraphicsRenderOverride=record
      public
       HasFillColor:boolean;
       FillColor:TpvVectorGraphicsColor;
       HasStrokeColor:boolean;
       StrokeColor:TpvVectorGraphicsColor;
       HasStrokeWidth:boolean;
       StrokeWidth:TpvDouble;
       HasOpacity:boolean;
       Opacity:TpvUInt8;
       // When set, the override applies to the whole subtree, otherwise only to the object it is
       // handed to.
       Recursive:boolean;
      public
       class function CreateNone:TpvVectorGraphicsRenderOverride; static;
       class function CreateFillColor(const aColor:TpvVectorGraphicsColor;const aRecursive:boolean=true):TpvVectorGraphicsRenderOverride; static;
       class function CreateFillAndStrokeColor(const aFillColor,aStrokeColor:TpvVectorGraphicsColor;const aRecursive:boolean=true):TpvVectorGraphicsRenderOverride; static;
       function IsActive:boolean;
     end;

     PpvVectorGraphicsRenderOverride=^TpvVectorGraphicsRenderOverride;

     { TpvVectorGraphicsRenderContext }

     // Everything one render pass needs to carry down the tree, in one place instead of a dozen
     // parameters.
     TpvVectorGraphicsRenderContext=record
      public
       Rasterizer:TpvVectorGraphicsRasterizer;
       Tick:TpvInt64;
       TickFraction:TpvDouble;
       ParentMatrix:TpvVectorPathMatrix;
       ParentAlpha:TpvUInt8;
       Mask:TpvVectorGraphicsMask;
       Override_:TpvVectorGraphicsRenderOverride;
       // Set while a clip path or mask subtree is being rasterized, which suppresses paint and
       // draws plain coverage instead.
       MaskPass:boolean;
       // Set while an outline pass is drawing hairline outlines of everything, for an editor.
       OutlinePass:boolean;
       OutlineColor:TpvVectorGraphicsColor;
      public
       class function Create(const aRasterizer:TpvVectorGraphicsRasterizer):TpvVectorGraphicsRenderContext; static;
     end;

     PpvVectorGraphicsRenderContext=^TpvVectorGraphicsRenderContext;

     { TpvVectorGraphicsObject }

     TpvVectorGraphicsObject=class
      protected
       fDocument:TpvVectorGraphicsDocument;
       fParent:TpvVectorGraphicsObject;
       fID:TpvUTF8String;
       fName:TpvUTF8String;
       fSimpleMatrix:TpvVectorGraphicsSimpleMatrix;
       fAlpha:TpvUInt8;
       fZ:TpvDouble;
       fOffsetX:TpvDouble;
       fOffsetY:TpvDouble;
       fVisible:boolean;
       fLocked:boolean;
       fOutline:boolean;
       fMaskMode:TpvVectorGraphicsMaskMode;
       fBlendMode:TpvVectorGraphicsBlendMode;
       fIsolate:boolean;
       fTimeLine:TpvVectorGraphicsTimeLine;
       fBoundingBox:TpvVectorGraphicsBoundingBox;
       fCurrentMatrix:TpvVectorPathMatrix;
       fCurrentMatrixInverse:TpvVectorPathMatrix;
       fCurrentAlpha:TpvUInt8;
       // The buffer this object composites itself through, kept around between frames because it
       // is the size of the whole target. Created on first use, which most objects never reach.
       fOffImageBuffer:TpvVectorGraphicsPixelBuffer;
       procedure SetID(const aID:TpvUTF8String);
       // Resolves the transform at a point in time, including whatever the timeline interpolates.
       function GetMatrixAt(const aTick:TpvInt64;const aTickFraction:TpvDouble):TpvVectorPathMatrix; virtual;
       function GetAlphaAt(const aTick:TpvInt64;const aTickFraction:TpvDouble):TpvUInt8; virtual;
       function GetVisibleAt(const aTick:TpvInt64):boolean; virtual;
       // Sets up fCurrentMatrix, its inverse and fCurrentAlpha, which every subclass needs before
       // it draws anything.
       procedure PrepareRender(var aContext:TpvVectorGraphicsRenderContext); virtual;
       // Redirects the rasterizer into a buffer of this object's own and returns the buffer that
       // was in effect, which the caller has to hand back to EndOffImage. aIsolated starts the
       // buffer out empty, which a blend mode needs because the blend function has to see the
       // backdrop as a separate thing; without one it starts from a copy of the backdrop, which
       // keeps a semi transparent object over existing content blending correctly.
       function BeginOffImage(var aContext:TpvVectorGraphicsRenderContext;
                              const aIsolated:boolean):TpvVectorGraphicsPixelBuffer;
       // Puts aOldBuffer back and composites into it what was drawn in the meantime.
       procedure EndOffImage(var aContext:TpvVectorGraphicsRenderContext;
                             const aOldBuffer:TpvVectorGraphicsPixelBuffer;
                             const aAlpha:TpvUInt8;
                             const aBlendMode:TpvVectorGraphicsBlendMode;
                             const aIsolated:boolean);
       // Which blend mode actually applies, and whether the object therefore has to go through a
       // buffer of its own. A mask pass carries no color, so a blend mode is meaningless there.
       procedure ResolveBlending(const aContext:TpvVectorGraphicsRenderContext;
                                 out aBlendMode:TpvVectorGraphicsBlendMode;
                                 out aIsolated:boolean);
      public
       constructor Create(const aDocument:TpvVectorGraphicsDocument;const aParent:TpvVectorGraphicsObject); reintroduce; virtual;
       destructor Destroy; override;
       procedure Assign(const aFrom:TpvVectorGraphicsObject); virtual;
       procedure CalculateBoundingBox(const aTick:TpvInt64;const aMatrix:TpvVectorPathMatrix); virtual;
       procedure Update; virtual;
       procedure Render(var aContext:TpvVectorGraphicsRenderContext); virtual;
       // Depth first search for an object with the given id, self included.
       function FindByID(const aID:TpvUTF8String):TpvVectorGraphicsObject; virtual;
       // Collects self and, for a group, everything below it.
       procedure CollectObjects(const aList:TpvObjectGenericList<TpvVectorGraphicsObject>); virtual;
      public
       property BoundingBox:TpvVectorGraphicsBoundingBox read fBoundingBox write fBoundingBox;
       property SimpleMatrix:TpvVectorGraphicsSimpleMatrix read fSimpleMatrix write fSimpleMatrix;
       property TimeLine:TpvVectorGraphicsTimeLine read fTimeLine write fTimeLine;
       property CurrentMatrix:TpvVectorPathMatrix read fCurrentMatrix;
       property CurrentMatrixInverse:TpvVectorPathMatrix read fCurrentMatrixInverse;
      published
       property Document:TpvVectorGraphicsDocument read fDocument;
       property Parent:TpvVectorGraphicsObject read fParent;
       property ID:TpvUTF8String read fID write SetID;
       property Name:TpvUTF8String read fName write fName;
       property Alpha:TpvUInt8 read fAlpha write fAlpha;
       property Z:TpvDouble read fZ write fZ;
       property OffsetX:TpvDouble read fOffsetX write fOffsetX;
       property OffsetY:TpvDouble read fOffsetY write fOffsetY;
       property Visible:boolean read fVisible write fVisible;
       property Locked:boolean read fLocked write fLocked;
       property Outline:boolean read fOutline write fOutline;
       property MaskMode:TpvVectorGraphicsMaskMode read fMaskMode write fMaskMode;
       // Anything but Normal makes the object draw into a buffer of its own and composite that
       // once through the blend function, which is what the CSS compositing specification means
       // by an element rather than each of its parts blending with the backdrop.
       property BlendMode:TpvVectorGraphicsBlendMode read fBlendMode write fBlendMode;
       // Keeps the blend modes of the children inside, which is what isolation:isolate asks for
       // and what every stacking context implies anyway.
       property Isolate:boolean read fIsolate write fIsolate;
       property CurrentAlpha:TpvUInt8 read fCurrentAlpha;
     end;

     TpvVectorGraphicsObjectList=TpvObjectGenericList<TpvVectorGraphicsObject>;

     { TpvVectorGraphicsGraphic }

     // Base of everything that has an outline to fill and to stroke. Subclasses only have to
     // build the path in UpdatePath; filling, stroking, dashing and painting happen here.
     TpvVectorGraphicsGraphic=class(TpvVectorGraphicsObject)
      protected
       fPath:TpvVectorPath;
       fStyle:TpvVectorGraphicsStyle;
       fPathDirty:boolean;
       fFlattenedShape:TpvVectorPathShape;
       fFlattenedScale:TpvDouble;
       fStroker:TpvVectorPathStroker;
       fFillPaint:TpvVectorGraphicsPaint;
       fStrokePaint:TpvVectorGraphicsPaint;
       fSolidPaint:TpvVectorGraphicsSolidPaint;
       fLinearGradientPaint:TpvVectorGraphicsLinearGradientPaint;
       fRadialGradientPaint:TpvVectorGraphicsRadialGradientPaint;
       fImagePaint:TpvVectorGraphicsImagePaint;
       // Rebuilds fPath from the parameters of the subclass. The base class does nothing, which
       // is right for the free form shape that owns its path outright.
       procedure UpdatePath; virtual;
       // Returns the shape flattened for the given scale, rebuilding it when the scale moved far
       // enough for the old flattening to show.
       function GetFlattenedShape(const aScale:TpvDouble;const aTick:TpvInt64;const aTickFraction:TpvDouble):TpvVectorPathShape;
       function GetFillColorAt(const aTick:TpvInt64;const aTickFraction:TpvDouble):TpvVectorGraphicsColor;
       function GetStrokeColorAt(const aTick:TpvInt64;const aTickFraction:TpvDouble):TpvVectorGraphicsColor;
       function GetStrokeWidthAt(const aTick:TpvInt64;const aTickFraction:TpvDouble):TpvDouble;
       // Picks and configures the paint for one pass. Returns nil when the pass draws nothing.
       function PreparePaint(const aFillData:TpvVectorGraphicsFillData;
                             const aColor:TpvVectorGraphicsColor;
                             const aAlpha:TpvUInt8;
                             var aContext:TpvVectorGraphicsRenderContext):TpvVectorGraphicsPaint;
       procedure RenderPass(const aShape:TpvVectorPathShape;
                            const aFillData:TpvVectorGraphicsFillData;
                            const aColor:TpvVectorGraphicsColor;
                            var aContext:TpvVectorGraphicsRenderContext);
      public
       constructor Create(const aDocument:TpvVectorGraphicsDocument;const aParent:TpvVectorGraphicsObject); override;
       destructor Destroy; override;
       procedure Assign(const aFrom:TpvVectorGraphicsObject); override;
       procedure CalculateBoundingBox(const aTick:TpvInt64;const aMatrix:TpvVectorPathMatrix); override;
       procedure Update; override;
       procedure Render(var aContext:TpvVectorGraphicsRenderContext); override;
       // Marks the path as needing a rebuild, which any change to the subclass parameters has to
       // do.
       procedure MarkPathDirty;
      public
       property Style:TpvVectorGraphicsStyle read fStyle write fStyle;
       property Path:TpvVectorPath read fPath;
     end;

     { TpvVectorGraphicsShape }

     // A free form path, which is what an SVG path, polyline or polygon element becomes.
     TpvVectorGraphicsShape=class(TpvVectorGraphicsGraphic)
      public
       constructor Create(const aDocument:TpvVectorGraphicsDocument;const aParent:TpvVectorGraphicsObject); override;
     end;

     { TpvVectorGraphicsLine }

     TpvVectorGraphicsLine=class(TpvVectorGraphicsGraphic)
      private
       fX1:TpvDouble;
       fY1:TpvDouble;
       fX2:TpvDouble;
       fY2:TpvDouble;
       procedure SetCoordinate(const aIndex:TpvInt32;const aValue:TpvDouble);
      protected
       procedure UpdatePath; override;
      public
       constructor Create(const aDocument:TpvVectorGraphicsDocument;const aParent:TpvVectorGraphicsObject); override;
       procedure Assign(const aFrom:TpvVectorGraphicsObject); override;
      published
       property x1:TpvDouble read fX1 write fX1;
       property y1:TpvDouble read fY1 write fY1;
       property x2:TpvDouble read fX2 write fX2;
       property y2:TpvDouble read fY2 write fY2;
     end;

     { TpvVectorGraphicsRectangle }

     TpvVectorGraphicsRectangle=class(TpvVectorGraphicsGraphic)
      private
       fX:TpvDouble;
       fY:TpvDouble;
       fWidth:TpvDouble;
       fHeight:TpvDouble;
       fRadiusX:TpvDouble;
       fRadiusY:TpvDouble;
      protected
       procedure UpdatePath; override;
      public
       constructor Create(const aDocument:TpvVectorGraphicsDocument;const aParent:TpvVectorGraphicsObject); override;
       procedure Assign(const aFrom:TpvVectorGraphicsObject); override;
      published
       property x:TpvDouble read fX write fX;
       property y:TpvDouble read fY write fY;
       property Width:TpvDouble read fWidth write fWidth;
       property Height:TpvDouble read fHeight write fHeight;
       property RadiusX:TpvDouble read fRadiusX write fRadiusX;
       property RadiusY:TpvDouble read fRadiusY write fRadiusY;
     end;

     { TpvVectorGraphicsEllipse }

     TpvVectorGraphicsEllipse=class(TpvVectorGraphicsGraphic)
      private
       fCenterX:TpvDouble;
       fCenterY:TpvDouble;
       fRadiusX:TpvDouble;
       fRadiusY:TpvDouble;
      protected
       procedure UpdatePath; override;
      public
       constructor Create(const aDocument:TpvVectorGraphicsDocument;const aParent:TpvVectorGraphicsObject); override;
       procedure Assign(const aFrom:TpvVectorGraphicsObject); override;
      published
       property CenterX:TpvDouble read fCenterX write fCenterX;
       property CenterY:TpvDouble read fCenterY write fCenterY;
       property RadiusX:TpvDouble read fRadiusX write fRadiusX;
       property RadiusY:TpvDouble read fRadiusY write fRadiusY;
     end;

     PpvVectorGraphicsTextAnchor=^TpvVectorGraphicsTextAnchor;
     TpvVectorGraphicsTextAnchor=
      (
       Start=0,
       Middle,
       End_
      );

     PpvVectorGraphicsTextBaseline=^TpvVectorGraphicsTextBaseline;
     TpvVectorGraphicsTextBaseline=
      (
       Alphabetic=0,
       Central,
       Middle,
       Hanging,
       Top,
       Bottom
      );

     { TpvVectorGraphicsFontProvider }

     // Hands out fonts by family name and style. The engine has no business knowing where an
     // application keeps its fonts, so a caller plugs this in; without one, text elements simply
     // produce no geometry.
     TpvVectorGraphicsFontProvider=class
      public
       // May return nil. The returned font stays owned by the provider.
       function GetFont(const aFamily:TpvUTF8String;const aBold,aItalic:boolean):TObject; virtual;
     end;

     { TpvVectorGraphicsText }

     // A run of text, turned into an outline path through the font provider. SVG lays text out
     // on one line here; tspan with its own position becomes a text object of its own.
     TpvVectorGraphicsText=class(TpvVectorGraphicsGraphic)
      private
       fX:TpvDouble;
       fY:TpvDouble;
       fText:TpvUTF8String;
       fFontFamily:TpvUTF8String;
       fFontSize:TpvDouble;
       fBold:boolean;
       fItalic:boolean;
       fAnchor:TpvVectorGraphicsTextAnchor;
       fBaseline:TpvVectorGraphicsTextBaseline;
       fLetterSpacing:TpvDouble;
       fAdvanceWidth:TpvDouble;
      protected
       procedure UpdatePath; override;
      public
       constructor Create(const aDocument:TpvVectorGraphicsDocument;const aParent:TpvVectorGraphicsObject); override;
       procedure Assign(const aFrom:TpvVectorGraphicsObject); override;
      published
       property x:TpvDouble read fX write fX;
       property y:TpvDouble read fY write fY;
       property Text:TpvUTF8String read fText write fText;
       property FontFamily:TpvUTF8String read fFontFamily write fFontFamily;
       property FontSize:TpvDouble read fFontSize write fFontSize;
       property Bold:boolean read fBold write fBold;
       property Italic:boolean read fItalic write fItalic;
       property Anchor:TpvVectorGraphicsTextAnchor read fAnchor write fAnchor;
       property Baseline:TpvVectorGraphicsTextBaseline read fBaseline write fBaseline;
       property LetterSpacing:TpvDouble read fLetterSpacing write fLetterSpacing;
       // The width the laid out run came to, valid after the path was built.
       property AdvanceWidth:TpvDouble read fAdvanceWidth;
     end;

     { TpvVectorGraphicsImageObject }

     // Draws a raster image into the unit square of its own coordinate system, scaled by Width
     // and Height, which is what the SVG image element asks for.
     TpvVectorGraphicsImageObject=class(TpvVectorGraphicsGraphic)
      private
       fX:TpvDouble;
       fY:TpvDouble;
       fWidth:TpvDouble;
       fHeight:TpvDouble;
      protected
       procedure UpdatePath; override;
      public
       constructor Create(const aDocument:TpvVectorGraphicsDocument;const aParent:TpvVectorGraphicsObject); override;
       procedure Assign(const aFrom:TpvVectorGraphicsObject); override;
       procedure SetImage(const aImage:TpvVectorGraphicsImageResource);
      published
       property x:TpvDouble read fX write fX;
       property y:TpvDouble read fY write fY;
       property Width:TpvDouble read fWidth write fWidth;
       property Height:TpvDouble read fHeight write fHeight;
     end;

     { TpvVectorGraphicsGroup }

     // A container, which is at once the SVG g element, the root of a document, the body of a
     // clipPath, mask, marker, symbol or pattern, and the target of a use element.
     TpvVectorGraphicsGroup=class(TpvVectorGraphicsObject)
      private
       fChildren:TpvVectorGraphicsObjectList;
       fSortedChildren:TpvVectorGraphicsObjectList;
       fWidth:TpvDouble;
       fHeight:TpvDouble;
       fBackgroundColor:TpvVectorGraphicsColor;
       fGamma:TpvDouble;
       fLoopTime:TpvInt64;
       fOffImage:boolean;
       fNestingLevel:TpvInt32;
       // Pattern tiles get rendered into this once per frame and then sampled by the image paint.
       fPatternBuffer:TpvVectorGraphicsPixelBuffer;
       procedure SortChildren;
       function GetRealTick(const aTick:TpvInt64):TpvInt64;
       // Rasterizes every child that is marked as a clip path or mask into aMask.
       function BuildMask(var aContext:TpvVectorGraphicsRenderContext;const aMaskMode:TpvVectorGraphicsMaskMode):TpvVectorGraphicsMask;
      public
       constructor Create(const aDocument:TpvVectorGraphicsDocument;const aParent:TpvVectorGraphicsObject); override;
       destructor Destroy; override;
       procedure Assign(const aFrom:TpvVectorGraphicsObject); override;
       procedure CalculateBoundingBox(const aTick:TpvInt64;const aMatrix:TpvVectorPathMatrix); override;
       procedure Update; override;
       procedure Render(var aContext:TpvVectorGraphicsRenderContext); override;
       function FindByID(const aID:TpvUTF8String):TpvVectorGraphicsObject; override;
       procedure CollectObjects(const aList:TpvObjectGenericList<TpvVectorGraphicsObject>); override;
       function Add(const aChild:TpvVectorGraphicsObject):TpvVectorGraphicsObject;
       // Removes and destroys the child.
       procedure Remove(const aChild:TpvVectorGraphicsObject);
       // Removes the child and hands it back without destroying it, for moving it elsewhere.
       function Extract(const aChild:TpvVectorGraphicsObject):TpvVectorGraphicsObject;
       // Renders this group into aBuffer, which the pattern paint then tiles.
       procedure RenderToPatternBuffer(var aContext:TpvVectorGraphicsRenderContext;const aWidth,aHeight:TpvInt32);
      public
       property PatternBuffer:TpvVectorGraphicsPixelBuffer read fPatternBuffer;
      published
       property Children:TpvVectorGraphicsObjectList read fChildren;
       property Width:TpvDouble read fWidth write fWidth;
       property Height:TpvDouble read fHeight write fHeight;
       property BackgroundColor:TpvVectorGraphicsColor read fBackgroundColor write fBackgroundColor;
       property Gamma:TpvDouble read fGamma write fGamma;
       property LoopTime:TpvInt64 read fLoopTime write fLoopTime;
       property OffImage:boolean read fOffImage write fOffImage;
     end;

     TpvVectorGraphicsGroupList=TpvObjectGenericList<TpvVectorGraphicsGroup>;

     { TpvVectorGraphicsGroupInstance }

     // A reference to another object that is drawn in place, which is the SVG use element. The
     // target is owned elsewhere, usually by the defs of the document, and may be any object,
     // not only a group, since use may point at a single shape just as well.
     TpvVectorGraphicsGroupInstance=class(TpvVectorGraphicsObject)
      private
       fTarget:TpvVectorGraphicsObject;
       fRecursionGuard:TpvInt32;
       function GetGroup:TpvVectorGraphicsGroup;
       procedure SetGroup(const aGroup:TpvVectorGraphicsGroup);
      public
       constructor Create(const aDocument:TpvVectorGraphicsDocument;const aParent:TpvVectorGraphicsObject); override;
       procedure Assign(const aFrom:TpvVectorGraphicsObject); override;
       procedure CalculateBoundingBox(const aTick:TpvInt64;const aMatrix:TpvVectorPathMatrix); override;
       procedure Render(var aContext:TpvVectorGraphicsRenderContext); override;
      published
       property Target:TpvVectorGraphicsObject read fTarget write fTarget;
       // Kept for the common case where the target really is a group; nil when it is not.
       property Group:TpvVectorGraphicsGroup read GetGroup write SetGroup;
     end;

     TpvVectorGraphicsIDHashMap=TpvStringHashMap<TpvVectorGraphicsObject>;

     { TpvVectorGraphicsDocument }

     // Owns the whole tree, the shared images and the groups that live in defs, and is the entry
     // point for rendering and for looking objects up by id.
     TpvVectorGraphicsDocument=class
      private
       fRoot:TpvVectorGraphicsGroup;
       fDefs:TpvVectorGraphicsGroup;
       fImages:TpvVectorGraphicsImageResourceList;
       fIDHashMap:TpvVectorGraphicsIDHashMap;
       fImageIDCounter:TpvInt32;
       fDrawBackground:boolean;
       fViewBox:TpvVectorGraphicsBoundingBox;
       fHasViewBox:boolean;
       fFontProvider:TpvVectorGraphicsFontProvider;
       fPreserveAspectRatioUniform:boolean;
       fPreserveAspectRatioMeet:boolean;
       fPreserveAspectRatioAlignX:TpvDouble;
       fPreserveAspectRatioAlignY:TpvDouble;
       function GetWidth:TpvDouble;
       function GetHeight:TpvDouble;
       procedure SetWidth(const aWidth:TpvDouble);
       procedure SetHeight(const aHeight:TpvDouble);
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       procedure Update;
       procedure CalculateBoundingBox(const aTick:TpvInt64=0);
       // The one call a user of this unit normally needs: draws the whole document into the
       // rasterizer, scaled and placed by aMatrix.
       procedure Render(const aRasterizer:TpvVectorGraphicsRasterizer;
                        const aMatrix:TpvVectorPathMatrix;
                        const aTick:TpvInt64=0;
                        const aTickFraction:TpvDouble=0.0);
       // Draws one object, or one subtree, on its own, with an optional recolor that leaves the
       // document untouched. The matrix of every ancestor up to the root is applied, so the part
       // lands where it sits in the whole picture.
       procedure RenderObject(const aObject:TpvVectorGraphicsObject;
                              const aRasterizer:TpvVectorGraphicsRasterizer;
                              const aMatrix:TpvVectorPathMatrix;
                              const aOverride:TpvVectorGraphicsRenderOverride;
                              const aTick:TpvInt64=0;
                              const aTickFraction:TpvDouble=0.0);
       // Same, by id. Returns false when no object carries that id.
       function RenderObjectByID(const aID:TpvUTF8String;
                                 const aRasterizer:TpvVectorGraphicsRasterizer;
                                 const aMatrix:TpvVectorPathMatrix;
                                 const aOverride:TpvVectorGraphicsRenderOverride;
                                 const aTick:TpvInt64=0;
                                 const aTickFraction:TpvDouble=0.0):boolean;
       function FindByID(const aID:TpvUTF8String):TpvVectorGraphicsObject;
       procedure RegisterID(const aID:TpvUTF8String;const aObject:TpvVectorGraphicsObject);
       procedure UnregisterID(const aID:TpvUTF8String);
       function NewImage:TpvVectorGraphicsImageResource;
       // Sets the provider that text elements of this document resolve their fonts through. The
       // document does not take ownership.
       procedure SetFontProvider(const aFontProvider:TpvVectorGraphicsFontProvider);
       // Builds the matrix that maps the view box onto a target of the given size, honouring the
       // preserveAspectRatio style of fitting that SVG describes. Called without the four
       // optional arguments it uses whatever the document's own preserveAspectRatio said.
       function GetViewBoxMatrix(const aTargetWidth,aTargetHeight:TpvDouble):TpvVectorPathMatrix; overload;
       function GetViewBoxMatrix(const aTargetWidth,aTargetHeight:TpvDouble;
                                 const aUniform:boolean;
                                 const aMeet:boolean;
                                 const aAlignX:TpvDouble;
                                 const aAlignY:TpvDouble):TpvVectorPathMatrix; overload;
       // Parses a preserveAspectRatio attribute value into the four settings above.
       procedure SetPreserveAspectRatio(const aValue:TpvUTF8String);
      public
       property ViewBox:TpvVectorGraphicsBoundingBox read fViewBox write fViewBox;
      published
       property PreserveAspectRatioUniform:boolean read fPreserveAspectRatioUniform write fPreserveAspectRatioUniform;
       property PreserveAspectRatioMeet:boolean read fPreserveAspectRatioMeet write fPreserveAspectRatioMeet;
       property PreserveAspectRatioAlignX:TpvDouble read fPreserveAspectRatioAlignX write fPreserveAspectRatioAlignX;
       property PreserveAspectRatioAlignY:TpvDouble read fPreserveAspectRatioAlignY write fPreserveAspectRatioAlignY;
       property Root:TpvVectorGraphicsGroup read fRoot;
       property Defs:TpvVectorGraphicsGroup read fDefs;
       property Images:TpvVectorGraphicsImageResourceList read fImages;
       property FontProvider:TpvVectorGraphicsFontProvider read fFontProvider write fFontProvider;
       property DrawBackground:boolean read fDrawBackground write fDrawBackground;
       property HasViewBox:boolean read fHasViewBox write fHasViewBox;
       property Width:TpvDouble read GetWidth write SetWidth;
       property Height:TpvDouble read GetHeight write SetHeight;
     end;

implementation

type TpvVectorGraphicsInterpolateFunction=function(const aX,aY0,aY1,aY2,aY3:TpvDouble):TpvDouble;

function NearestInterpolate(const aX,aY0,aY1,aY2,aY3:TpvDouble):TpvDouble;
begin
 result:=aY1;
end;

function LinearInterpolate(const aX,aY0,aY1,aY2,aY3:TpvDouble):TpvDouble;
begin
 result:=(aY1*(1.0-aX))+(aY2*aX);
end;

function CosineInterpolate(const aX,aY0,aY1,aY2,aY3:TpvDouble):TpvDouble;
var Factor:TpvDouble;
begin
 Factor:=(1.0-sin((aX+0.5)*PI))*0.5;
 result:=(aY1*(1.0-Factor))+(aY2*Factor);
end;

function CubicInterpolate(const aX,aY0,aY1,aY2,aY3:TpvDouble):TpvDouble;
var Squared,Value:TpvDouble;
begin
 Squared:=aX*aX;
 Value:=((aY3-aY2)-aY0)+aY1;
 result:=((((Value*aX)*Squared)+(((aY0-aY1)-Value)*Squared))+((aY2-aY0)*aX))+aY1;
end;

function HermiteInterpolate(const aX,aY0,aY1,aY2,aY3:TpvDouble):TpvDouble;
var c0,c1,c2,c3:TpvDouble;
begin
 c0:=aY1;
 c1:=0.5*(aY2-aY0);
 c2:=((aY0-(2.5*aY1))+(2.0*aY2))-(0.5*aY3);
 c3:=(1.5*(aY1-aY2))+(0.5*(aY3-aY0));
 result:=(((((c3*aX)+c2)*aX)+c1)*aX)+c0;
end;

function Interpolate(const aMode:TpvVectorGraphicsInterpolationMode;const aX,aY0,aY1,aY2,aY3:TpvDouble):TpvDouble;
begin

 case aMode of
  TpvVectorGraphicsInterpolationMode.Linear:begin
   result:=LinearInterpolate(aX,aY0,aY1,aY2,aY3);
  end;
  TpvVectorGraphicsInterpolationMode.Cosine:begin
   result:=CosineInterpolate(aX,aY0,aY1,aY2,aY3);
  end;
  TpvVectorGraphicsInterpolationMode.Cubic:begin
   result:=CubicInterpolate(aX,aY0,aY1,aY2,aY3);
  end;
  TpvVectorGraphicsInterpolationMode.Hermite:begin
   result:=HermiteInterpolate(aX,aY0,aY1,aY2,aY3);
  end;
  else {TpvVectorGraphicsInterpolationMode.Nearest:}begin
   result:=NearestInterpolate(aX,aY0,aY1,aY2,aY3);
  end;
 end;

end;

function InterpolateColor(const aMode:TpvVectorGraphicsInterpolationMode;
                          const aX:TpvDouble;
                          const aFrom,aTo:TpvVectorGraphicsColor):TpvVectorGraphicsColor;
var FromRGBA,ToRGBA,Mixed:TpvVectorGraphicsRGBA;
    Value:TpvInt32;
begin

 FromRGBA:=TpvVectorGraphicsRGBA.CreateFromColor(aFrom);
 ToRGBA:=TpvVectorGraphicsRGBA.CreateFromColor(aTo);

 // The two keys are passed twice each, so that the cubic and Hermite modes see a flat tangent at
 // both ends instead of reaching for neighbours that a two-key blend does not have.
 Value:=round(Interpolate(aMode,aX,FromRGBA.r,FromRGBA.r,ToRGBA.r,ToRGBA.r));
 Mixed.r:=Min(Max(Value,0),255);

 Value:=round(Interpolate(aMode,aX,FromRGBA.g,FromRGBA.g,ToRGBA.g,ToRGBA.g));
 Mixed.g:=Min(Max(Value,0),255);

 Value:=round(Interpolate(aMode,aX,FromRGBA.b,FromRGBA.b,ToRGBA.b,ToRGBA.b));
 Mixed.b:=Min(Max(Value,0),255);

 Value:=round(Interpolate(aMode,aX,FromRGBA.a,FromRGBA.a,ToRGBA.a,ToRGBA.a));
 Mixed.a:=Min(Max(Value,0),255);

 result:=Mixed.ToColor;

end;

{ TpvVectorGraphicsSimpleMatrix }

class function TpvVectorGraphicsSimpleMatrix.CreateDefault:TpvVectorGraphicsSimpleMatrix;
begin
 result.Matrix:=TpvVectorPathMatrix.CreateIdentity;
 result.TranslateX:=0.0;
 result.TranslateY:=0.0;
 result.ScaleX:=1.0;
 result.ScaleY:=1.0;
 result.RotationDegrees:=0.0;
 result.SkewX:=0.0;
 result.SkewY:=0.0;
end;

function TpvVectorGraphicsSimpleMatrix.ToMatrix(const aOffsetX,aOffsetY:TpvDouble):TpvVectorPathMatrix;
const DegreesToRadians=PI/180.0;
begin

 result:=TpvVectorPathMatrix.CreateTranslate(TranslateX,TranslateY);

 // Rotation, skew and scale all happen around the given offset, which is what makes an object
 // turn about its own centre rather than about the origin of the document.
 result:=TpvVectorPathMatrix.CreateTranslate(aOffsetX,aOffsetY)*result;

 result:=TpvVectorPathMatrix.CreateRotate(RotationDegrees*DegreesToRadians)*result;

 if abs(SkewX)>1e-12 then begin
  result:=TpvVectorPathMatrix.CreateSkewX(SkewX*DegreesToRadians)*result;
 end;

 if abs(SkewY)>1e-12 then begin
  result:=TpvVectorPathMatrix.CreateSkewY(SkewY*DegreesToRadians)*result;
 end;

 result:=TpvVectorPathMatrix.CreateScale(ScaleX,ScaleY)*result;

 result:=TpvVectorPathMatrix.CreateTranslate(-aOffsetX,-aOffsetY)*result;

 // The free-form matrix is applied last, so that it sees the whole simple transform as its input.
 result:=Matrix*result;

end;

{ TpvVectorGraphicsBoundingBox }

class function TpvVectorGraphicsBoundingBox.CreateEmpty:TpvVectorGraphicsBoundingBox;
begin
 result.x1:=MaxDouble;
 result.y1:=MaxDouble;
 result.x2:=-MaxDouble;
 result.y2:=-MaxDouble;
end;

procedure TpvVectorGraphicsBoundingBox.Extend(const aX,aY:TpvDouble);
begin

 // The inverted bounds of an empty box make the first point set all four sides at once.
 if x1>aX then begin
  x1:=aX;
 end;

 if y1>aY then begin
  y1:=aY;
 end;

 if x2<aX then begin
  x2:=aX;
 end;

 if y2<aY then begin
  y2:=aY;
 end;

end;

procedure TpvVectorGraphicsBoundingBox.Combine(const aWith:TpvVectorGraphicsBoundingBox);
begin
 if not aWith.IsEmpty then begin
  Extend(aWith.x1,aWith.y1);
  Extend(aWith.x2,aWith.y2);
 end;
end;

function TpvVectorGraphicsBoundingBox.IsEmpty:boolean;
begin
 result:=(x1>x2) or (y1>y2);
end;

function TpvVectorGraphicsBoundingBox.Width:TpvDouble;
begin
 result:=x2-x1;
end;

function TpvVectorGraphicsBoundingBox.Height:TpvDouble;
begin
 result:=y2-y1;
end;

{ TpvVectorGraphicsImageResource }

constructor TpvVectorGraphicsImageResource.Create(const aDocument:TpvVectorGraphicsDocument);
begin

 inherited Create;

 fDocument:=aDocument;
 fID:=0;
 fName:='';

 fWidth:=0;
 fHeight:=0;
 fPixels:=nil;

end;

destructor TpvVectorGraphicsImageResource.Destroy;
begin

 fPixels:=nil;

 inherited Destroy;

end;

procedure TpvVectorGraphicsImageResource.SetSize(const aWidth,aHeight:TpvInt32);
begin

 fWidth:=aWidth;
 fHeight:=aHeight;

 if (fWidth>0) and (fHeight>0) then begin
  SetLength(fPixels,TpvSizeInt(fWidth)*TpvSizeInt(fHeight));
 end else begin
  fPixels:=nil;
 end;

end;

procedure TpvVectorGraphicsImageResource.SetPixels(const aPixels:pointer;const aWidth,aHeight:TpvInt32);
var Index,Count:TpvSizeInt;
    Source:PpvVectorGraphicsRawColorArray;
begin

 SetSize(aWidth,aHeight);

 // The incoming pixels already have the layout this class stores, so they are only copied.
 Count:=TpvSizeInt(fWidth)*TpvSizeInt(fHeight);
 if (Count>0) and assigned(aPixels) then begin
  Source:=aPixels;
  for Index:=0 to Count-1 do begin
   fPixels[Index]:=Source^[Index];
  end;
 end;

end;

procedure TpvVectorGraphicsImageResource.SetPixelsFromRGBA8(const aPixels:pointer;const aWidth,aHeight:TpvInt32);
var Index,Count:TpvSizeInt;
    Source:PpvVectorGraphicsRawColorArray;
    Color:TpvVectorGraphicsColor;
begin

 SetSize(aWidth,aHeight);

 // Here red and blue have to change places, since the storage order is the one the rasterizer
 // blends in and not the one an image loader hands out.
 Count:=TpvSizeInt(fWidth)*TpvSizeInt(fHeight);
 if (Count>0) and assigned(aPixels) then begin
  Source:=aPixels;
  for Index:=0 to Count-1 do begin
   Color:=Source^[Index];
   fPixels[Index]:=(Color and TpvUInt32($ff00ff00)) or
                   ((Color and TpvUInt32($00ff0000)) shr 16) or
                   ((Color and TpvUInt32($000000ff)) shl 16);
  end;
 end;

end;

function TpvVectorGraphicsImageResource.GetPixelPointer:PpvVectorGraphicsRawColorArray;
begin
 if length(fPixels)>0 then begin
  result:=@fPixels[0];
 end else begin
  result:=nil;
 end;
end;

{ TpvVectorGraphicsFillData }

class function TpvVectorGraphicsFillData.CreateDefault:TpvVectorGraphicsFillData;
begin
 result.Mode:=TpvVectorGraphicsFillMode.None;
 result.Color:=$ff000000;
 result.Image:=nil;
 result.SimpleMatrix:=TpvVectorGraphicsSimpleMatrix.CreateDefault;
 result.GradientFocusSimpleMatrix:=TpvVectorGraphicsSimpleMatrix.CreateDefault;
 result.GradientFocusX:=0.0;
 result.GradientFocusY:=0.0;
 result.GradientRadius:=1.0;
 result.GradientStops:=nil;
 result.SpreadMethod:=TpvVectorGraphicsSpreadMethod.Pad;
 result.FillRule:=TpvVectorPathFillRule.NonZero;
 result.ClipRule:=TpvVectorPathFillRule.NonZero;
 result.Pattern:=nil;
end;

procedure TpvVectorGraphicsFillData.Assign(const aFrom:TpvVectorGraphicsFillData);
begin

 Mode:=aFrom.Mode;
 Color:=aFrom.Color;

 // Image and pattern are references into the document and are shared, not duplicated.
 Image:=aFrom.Image;

 SimpleMatrix:=aFrom.SimpleMatrix;

 GradientFocusSimpleMatrix:=aFrom.GradientFocusSimpleMatrix;
 GradientFocusX:=aFrom.GradientFocusX;
 GradientFocusY:=aFrom.GradientFocusY;
 GradientRadius:=aFrom.GradientRadius;

 // The stops are a real copy, so that editing one object's gradient leaves the other alone.
 GradientStops:=copy(aFrom.GradientStops);
 SpreadMethod:=aFrom.SpreadMethod;

 FillRule:=aFrom.FillRule;
 ClipRule:=aFrom.ClipRule;

 Pattern:=aFrom.Pattern;

end;

{ TpvVectorGraphicsStyle }

class function TpvVectorGraphicsStyle.CreateDefault:TpvVectorGraphicsStyle;
begin
 result.FillData:=TpvVectorGraphicsFillData.CreateDefault;
 result.FillData.Mode:=TpvVectorGraphicsFillMode.Color;
 result.StrokeFillData:=TpvVectorGraphicsFillData.CreateDefault;
 result.StrokeCapMode:=TpvVectorPathStrokeCapMode.Butt;
 result.StrokeJoinMode:=TpvVectorPathStrokeJoinMode.Miter;
 result.StrokeInnerJoinMode:=TpvVectorPathStrokeInnerJoinMode.Miter;
 result.StrokeWidth:=1.0;
 result.StrokeWidthFixed:=false;
 result.StrokeMiterLimit:=4.0;
 result.StrokeInnerMiterLimit:=1.01;
 result.StrokeDashArray:=nil;
 result.StrokeDashOffset:=0.0;
 result.OffImage:=false;
end;

procedure TpvVectorGraphicsStyle.Assign(const aFrom:TpvVectorGraphicsStyle);
begin

 FillData.Assign(aFrom.FillData);
 StrokeFillData.Assign(aFrom.StrokeFillData);

 StrokeCapMode:=aFrom.StrokeCapMode;
 StrokeJoinMode:=aFrom.StrokeJoinMode;
 StrokeInnerJoinMode:=aFrom.StrokeInnerJoinMode;

 StrokeWidth:=aFrom.StrokeWidth;
 StrokeWidthFixed:=aFrom.StrokeWidthFixed;
 StrokeMiterLimit:=aFrom.StrokeMiterLimit;
 StrokeInnerMiterLimit:=aFrom.StrokeInnerMiterLimit;

 // As with the gradient stops, the dash pattern is copied rather than shared.
 StrokeDashArray:=copy(aFrom.StrokeDashArray);
 StrokeDashOffset:=aFrom.StrokeDashOffset;

 OffImage:=aFrom.OffImage;

end;

{ TpvVectorGraphicsRenderOverride }

class function TpvVectorGraphicsRenderOverride.CreateNone:TpvVectorGraphicsRenderOverride;
begin
 result.HasFillColor:=false;
 result.FillColor:=0;
 result.HasStrokeColor:=false;
 result.StrokeColor:=0;
 result.HasStrokeWidth:=false;
 result.StrokeWidth:=0.0;
 result.HasOpacity:=false;
 result.Opacity:=255;
 result.Recursive:=true;
end;

class function TpvVectorGraphicsRenderOverride.CreateFillColor(const aColor:TpvVectorGraphicsColor;const aRecursive:boolean):TpvVectorGraphicsRenderOverride;
begin
 result:=TpvVectorGraphicsRenderOverride.CreateNone;
 result.HasFillColor:=true;
 result.FillColor:=aColor;
 result.Recursive:=aRecursive;
end;

class function TpvVectorGraphicsRenderOverride.CreateFillAndStrokeColor(const aFillColor,aStrokeColor:TpvVectorGraphicsColor;const aRecursive:boolean):TpvVectorGraphicsRenderOverride;
begin
 result:=TpvVectorGraphicsRenderOverride.CreateNone;
 result.HasFillColor:=true;
 result.FillColor:=aFillColor;
 result.HasStrokeColor:=true;
 result.StrokeColor:=aStrokeColor;
 result.Recursive:=aRecursive;
end;

function TpvVectorGraphicsRenderOverride.IsActive:boolean;
begin
 result:=HasFillColor or HasStrokeColor or HasStrokeWidth or HasOpacity;
end;

{ TpvVectorGraphicsRenderContext }

class function TpvVectorGraphicsRenderContext.Create(const aRasterizer:TpvVectorGraphicsRasterizer):TpvVectorGraphicsRenderContext;
begin
 result.Rasterizer:=aRasterizer;
 result.Tick:=0;
 result.TickFraction:=0.0;
 result.ParentMatrix:=TpvVectorPathMatrix.CreateIdentity;
 result.ParentAlpha:=255;
 result.Mask:=nil;
 result.Override_:=TpvVectorGraphicsRenderOverride.CreateNone;
 result.MaskPass:=false;
 result.OutlinePass:=false;
 result.OutlineColor:=$ff000000;
end;

{ TpvVectorGraphicsObject }

constructor TpvVectorGraphicsObject.Create(const aDocument:TpvVectorGraphicsDocument;const aParent:TpvVectorGraphicsObject);
begin

 inherited Create;

 fDocument:=aDocument;
 fParent:=aParent;

 fID:='';
 fName:='';

 fSimpleMatrix:=TpvVectorGraphicsSimpleMatrix.CreateDefault;
 fAlpha:=255;
 fZ:=0.0;
 fOffsetX:=0.0;
 fOffsetY:=0.0;

 fVisible:=true;
 fLocked:=false;
 fOutline:=false;
 fMaskMode:=TpvVectorGraphicsMaskMode.None;

 // Normal and not isolated means the object composites straight into whatever it is drawn over,
 // with no buffer of its own.
 fBlendMode:=TpvVectorGraphicsBlendMode.Normal;
 fIsolate:=false;

 fTimeLine:=nil;

 // Everything below is filled in by PrepareRender and only holds defaults until then.
 fBoundingBox:=TpvVectorGraphicsBoundingBox.CreateEmpty;
 fCurrentMatrix:=TpvVectorPathMatrix.CreateIdentity;
 fCurrentMatrixInverse:=TpvVectorPathMatrix.CreateIdentity;
 fCurrentAlpha:=255;

 fOffImageBuffer:=nil;

end;

destructor TpvVectorGraphicsObject.Destroy;
var Index:TpvSizeInt;
begin

 // The document's identifier table points at this object and must not outlive it.
 if assigned(fDocument) and (length(fID)>0) then begin
  fDocument.UnregisterID(fID);
 end;

 // The timeline owns the morph paths of its keys.
 for Index:=0 to length(fTimeLine)-1 do begin
  FreeAndNil(fTimeLine[Index].MorphPath);
 end;
 fTimeLine:=nil;

 FreeAndNil(fOffImageBuffer);

 inherited Destroy;

end;

procedure TpvVectorGraphicsObject.SetID(const aID:TpvUTF8String);
begin

 // The document's identifier table is kept in step, so that FindByID never hands out an object
 // under a name it no longer carries.
 if fID<>aID then begin

  if assigned(fDocument) and (length(fID)>0) then begin
   fDocument.UnregisterID(fID);
  end;

  fID:=aID;

  if assigned(fDocument) and (length(fID)>0) then begin
   fDocument.RegisterID(fID,self);
  end;

 end;

end;

procedure TpvVectorGraphicsObject.Assign(const aFrom:TpvVectorGraphicsObject);
begin

 if not assigned(aFrom) then begin
  exit;
 end;

 fSimpleMatrix:=aFrom.fSimpleMatrix;
 fAlpha:=aFrom.fAlpha;
 fZ:=aFrom.fZ;
 fOffsetX:=aFrom.fOffsetX;
 fOffsetY:=aFrom.fOffsetY;

 fVisible:=aFrom.fVisible;
 fLocked:=aFrom.fLocked;
 fOutline:=aFrom.fOutline;
 fMaskMode:=aFrom.fMaskMode;
 fBlendMode:=aFrom.fBlendMode;
 fIsolate:=aFrom.fIsolate;

 // The identifier is deliberately left out, since it has to stay unique within the document.
 fName:=aFrom.fName;

end;

function TpvVectorGraphicsObject.GetMatrixAt(const aTick:TpvInt64;const aTickFraction:TpvDouble):TpvVectorPathMatrix;
var Current:TpvVectorGraphicsSimpleMatrix;
    Keys:array[0..3] of TpvVectorGraphicsSimpleMatrix;
    Index,OtherIndex,ThirdIndex,ValueIndex:TpvSizeInt;
    Factor:TpvDouble;
    Mode:TpvVectorGraphicsInterpolationMode;
begin

 Current:=fSimpleMatrix;

 for Index:=0 to length(fTimeLine)-1 do begin

  if fTimeLine[Index].Action<>TpvVectorGraphicsTimeLineAction.Transform then begin
   continue;
  end;

  // Past the end of a span, or a span of no length at all, the target simply applies.
  if (fTimeLine[Index].EndTick<=aTick) or
     ((fTimeLine[Index].StartTick<=aTick) and
      (fTimeLine[Index].EndTick=fTimeLine[Index].StartTick)) then begin
   Current:=fTimeLine[Index].TransformSimpleMatrix;
   continue;
  end;

  if fTimeLine[Index].StartTick>aTick then begin
   continue;
  end;

  // The cubic and hermite modes need the key before and the key after the span too, so they get
  // gathered here, falling back to the neighbours when there is none.
  Keys[1]:=Current;
  Keys[0]:=Keys[1];
  Keys[2]:=fTimeLine[Index].TransformSimpleMatrix;
  Keys[3]:=Keys[2];

  for OtherIndex:=Index+1 to length(fTimeLine)-1 do begin
   if fTimeLine[OtherIndex].Action=TpvVectorGraphicsTimeLineAction.Transform then begin
    Keys[3]:=fTimeLine[OtherIndex].TransformSimpleMatrix;
    break;
   end;
  end;

  for OtherIndex:=Index-1 downto 0 do begin
   if fTimeLine[OtherIndex].Action=TpvVectorGraphicsTimeLineAction.Transform then begin
    Keys[1]:=fTimeLine[OtherIndex].TransformSimpleMatrix;
    Keys[0]:=Keys[1];
    for ThirdIndex:=OtherIndex-1 downto 0 do begin
     if fTimeLine[ThirdIndex].Action=TpvVectorGraphicsTimeLineAction.Transform then begin
      Keys[0]:=fTimeLine[ThirdIndex].TransformSimpleMatrix;
      break;
     end;
    end;
    break;
   end;
  end;

  // Every part of the transform is interpolated on its own, which is the point of keeping them
  // apart instead of blending baked matrices.
  Factor:=((aTick-fTimeLine[Index].StartTick)+aTickFraction)/(fTimeLine[Index].EndTick-fTimeLine[Index].StartTick);
  Mode:=fTimeLine[Index].InterpolationMode;

  for ValueIndex:=0 to 5 do begin
   Current.Matrix.Values[ValueIndex]:=Interpolate(Mode,Factor,
                                                  Keys[0].Matrix.Values[ValueIndex],
                                                  Keys[1].Matrix.Values[ValueIndex],
                                                  Keys[2].Matrix.Values[ValueIndex],
                                                  Keys[3].Matrix.Values[ValueIndex]);
  end;

  Current.TranslateX:=Interpolate(Mode,Factor,Keys[0].TranslateX,Keys[1].TranslateX,Keys[2].TranslateX,Keys[3].TranslateX);
  Current.TranslateY:=Interpolate(Mode,Factor,Keys[0].TranslateY,Keys[1].TranslateY,Keys[2].TranslateY,Keys[3].TranslateY);
  Current.ScaleX:=Interpolate(Mode,Factor,Keys[0].ScaleX,Keys[1].ScaleX,Keys[2].ScaleX,Keys[3].ScaleX);
  Current.ScaleY:=Interpolate(Mode,Factor,Keys[0].ScaleY,Keys[1].ScaleY,Keys[2].ScaleY,Keys[3].ScaleY);
  Current.RotationDegrees:=Interpolate(Mode,Factor,Keys[0].RotationDegrees,Keys[1].RotationDegrees,Keys[2].RotationDegrees,Keys[3].RotationDegrees);
  Current.SkewX:=Interpolate(Mode,Factor,Keys[0].SkewX,Keys[1].SkewX,Keys[2].SkewX,Keys[3].SkewX);
  Current.SkewY:=Interpolate(Mode,Factor,Keys[0].SkewY,Keys[1].SkewY,Keys[2].SkewY,Keys[3].SkewY);

 end;

 result:=Current.ToMatrix(fOffsetX,fOffsetY);

end;

function TpvVectorGraphicsObject.GetAlphaAt(const aTick:TpvInt64;const aTickFraction:TpvDouble):TpvUInt8;
var Index:TpvSizeInt;
    Current,Target:TpvInt32;
    Factor:TpvDouble;
begin

 Current:=fAlpha;

 // The keys are walked in order and each one carries the value forward, so that the last span
 // that has already started decides the result.
 for Index:=0 to length(fTimeLine)-1 do begin

  if (fTimeLine[Index].Action=TpvVectorGraphicsTimeLineAction.Move) and fTimeLine[Index].HasAlpha then begin

   if fTimeLine[Index].EndTick<=aTick then begin

    Current:=fTimeLine[Index].Alpha;

   end else if fTimeLine[Index].StartTick<=aTick then begin

    // A span of no length is a jump, anything else blends from what came before towards the key.
    if fTimeLine[Index].EndTick=fTimeLine[Index].StartTick then begin
     Current:=fTimeLine[Index].Alpha;
    end else begin
     Target:=fTimeLine[Index].Alpha;
     Factor:=((aTick-fTimeLine[Index].StartTick)+aTickFraction)/(fTimeLine[Index].EndTick-fTimeLine[Index].StartTick);
     Current:=round(Interpolate(fTimeLine[Index].InterpolationMode,Factor,Current,Current,Target,Target));
    end;

   end;

  end;

 end;

 result:=Min(Max(Current,0),255);

end;

function TpvVectorGraphicsObject.GetVisibleAt(const aTick:TpvInt64):boolean;
var Index:TpvSizeInt;
begin

 result:=fVisible;

 // Show and hide are instant, hence no end tick and no interpolation, and the last one that has
 // been reached wins.
 for Index:=0 to length(fTimeLine)-1 do begin
  if fTimeLine[Index].StartTick<=aTick then begin
   case fTimeLine[Index].Action of
    TpvVectorGraphicsTimeLineAction.Show:begin
     result:=true;
    end;
    TpvVectorGraphicsTimeLineAction.Hide:begin
     result:=false;
    end;
    else begin
    end;
   end;
  end;
 end;

end;

procedure TpvVectorGraphicsObject.PrepareRender(var aContext:TpvVectorGraphicsRenderContext);
var Opacity:TpvUInt8;
begin

 // The inverse is kept alongside, since paints in object bounding box units need to map a device
 // pixel back into the object's own space.
 fCurrentMatrix:=GetMatrixAt(aContext.Tick,aContext.TickFraction)*aContext.ParentMatrix;
 fCurrentMatrixInverse:=fCurrentMatrix.Inverse;

 // An override replaces the object's own opacity but not the one it inherits.
 if aContext.Override_.HasOpacity then begin
  Opacity:=aContext.Override_.Opacity;
 end else begin
  Opacity:=GetAlphaAt(aContext.Tick,aContext.TickFraction);
 end;

 fCurrentAlpha:=pvVectorGraphicsByteMulTable[aContext.ParentAlpha,Opacity];

end;

procedure TpvVectorGraphicsObject.ResolveBlending(const aContext:TpvVectorGraphicsRenderContext;
                                                  out aBlendMode:TpvVectorGraphicsBlendMode;
                                                  out aIsolated:boolean);
begin

 if aContext.MaskPass then begin
  aBlendMode:=TpvVectorGraphicsBlendMode.Normal;
  aIsolated:=false;
 end else begin
  aBlendMode:=fBlendMode;
  aIsolated:=(aBlendMode<>TpvVectorGraphicsBlendMode.Normal) or fIsolate;
 end;

end;

function TpvVectorGraphicsObject.BeginOffImage(var aContext:TpvVectorGraphicsRenderContext;
                                               const aIsolated:boolean):TpvVectorGraphicsPixelBuffer;
begin

 // The buffer covers the whole target, so it is only rebuilt when that changes size.
 if (not assigned(fOffImageBuffer)) or
    (fOffImageBuffer.Width<>aContext.Rasterizer.Width) or
    (fOffImageBuffer.Height<>aContext.Rasterizer.Height) then begin
  FreeAndNil(fOffImageBuffer);
  fOffImageBuffer:=TpvVectorGraphicsPixelBuffer.Create(aContext.Rasterizer.Width,
                                                       aContext.Rasterizer.Height,
                                                       aContext.Rasterizer.PixelBuffer.ColorSpaceMode);
 end;

 if aIsolated then begin
  fOffImageBuffer.Clear(0);
 end else begin
  fOffImageBuffer.BlendFrom(aContext.Rasterizer.PixelBuffer,255);
 end;

 result:=aContext.Rasterizer.ReplacePixelBuffer(fOffImageBuffer);

end;

procedure TpvVectorGraphicsObject.EndOffImage(var aContext:TpvVectorGraphicsRenderContext;
                                              const aOldBuffer:TpvVectorGraphicsPixelBuffer;
                                              const aAlpha:TpvUInt8;
                                              const aBlendMode:TpvVectorGraphicsBlendMode;
                                              const aIsolated:boolean);
begin

 if not (assigned(aOldBuffer) and assigned(fOffImageBuffer)) then begin
  exit;
 end;

 aContext.Rasterizer.ReplacePixelBuffer(aOldBuffer);

 // An isolated buffer is transparent where nothing was drawn, so it has to be composited rather
 // than copied, and that is also where a blend mode gets applied.
 if aIsolated then begin
  aOldBuffer.BlendOverFrom(fOffImageBuffer,aAlpha,aBlendMode,aContext.Rasterizer.LinearBlending);
 end else begin
  aOldBuffer.BlendFrom(fOffImageBuffer,aAlpha);
 end;

end;

procedure TpvVectorGraphicsObject.CalculateBoundingBox(const aTick:TpvInt64;const aMatrix:TpvVectorPathMatrix);
begin
 fBoundingBox:=TpvVectorGraphicsBoundingBox.CreateEmpty;
end;

procedure TpvVectorGraphicsObject.Update;
begin
end;

procedure TpvVectorGraphicsObject.Render(var aContext:TpvVectorGraphicsRenderContext);
begin
 PrepareRender(aContext);
end;

function TpvVectorGraphicsObject.FindByID(const aID:TpvUTF8String):TpvVectorGraphicsObject;
begin
 if fID=aID then begin
  result:=self;
 end else begin
  result:=nil;
 end;
end;

procedure TpvVectorGraphicsObject.CollectObjects(const aList:TpvObjectGenericList<TpvVectorGraphicsObject>);
begin
 aList.Add(self);
end;

{ TpvVectorGraphicsGraphic }

constructor TpvVectorGraphicsGraphic.Create(const aDocument:TpvVectorGraphicsDocument;const aParent:TpvVectorGraphicsObject);
begin

 inherited Create(aDocument,aParent);

 fPath:=TpvVectorPath.Create;
 fStyle:=TpvVectorGraphicsStyle.CreateDefault;

 // The flattening is rebuilt on demand, the scale of zero makes sure the first request does it.
 fPathDirty:=true;
 fFlattenedShape:=nil;
 fFlattenedScale:=0.0;

 // The stroker and all paints but the solid one are created only when something actually needs
 // them, since most objects need none of them.
 fStroker:=nil;
 fSolidPaint:=TpvVectorGraphicsSolidPaint.Create;
 fLinearGradientPaint:=nil;
 fRadialGradientPaint:=nil;
 fImagePaint:=nil;
 fFillPaint:=nil;
 fStrokePaint:=nil;

end;

destructor TpvVectorGraphicsGraphic.Destroy;
begin

 FreeAndNil(fPath);
 FreeAndNil(fFlattenedShape);
 FreeAndNil(fStroker);

 // fFillPaint and fStrokePaint only point at one of the paints below and own nothing.
 FreeAndNil(fSolidPaint);
 FreeAndNil(fLinearGradientPaint);
 FreeAndNil(fRadialGradientPaint);
 FreeAndNil(fImagePaint);

 inherited Destroy;

end;

procedure TpvVectorGraphicsGraphic.Assign(const aFrom:TpvVectorGraphicsObject);
begin

 inherited Assign(aFrom);

 if aFrom is TpvVectorGraphicsGraphic then begin
  fStyle.Assign(TpvVectorGraphicsGraphic(aFrom).fStyle);
  fPath.Assign(TpvVectorGraphicsGraphic(aFrom).fPath);
  MarkPathDirty;
 end;

end;

procedure TpvVectorGraphicsGraphic.MarkPathDirty;
begin

 // The flattening belongs to the old path and has to go with it.
 fPathDirty:=true;
 fFlattenedScale:=0.0;

end;

procedure TpvVectorGraphicsGraphic.UpdatePath;
begin
end;

procedure TpvVectorGraphicsGraphic.Update;
begin

 inherited Update;

 if fPathDirty then begin
  fPathDirty:=false;
  UpdatePath;
 end;

end;

function TpvVectorGraphicsGraphic.GetFlattenedShape(const aScale:TpvDouble;const aTick:TpvInt64;const aTickFraction:TpvDouble):TpvVectorPathShape;
var Scale:TpvDouble;
begin

 if fPathDirty then begin
  fPathDirty:=false;
  UpdatePath;
  fFlattenedScale:=0.0;
 end;

 // A floor on the scale keeps a heavily shrunk object from being flattened into nothing.
 Scale:=aScale;
 if Scale<0.125 then begin
  Scale:=0.125;
 end;

 // Reflattening on every tiny scale change would throw the work away for nothing, so the old
 // flattening is kept while it is within a quarter of the current scale.
 if (not assigned(fFlattenedShape)) or
    (fFlattenedScale<=0.0) or
    (Scale>(fFlattenedScale*1.25)) or
    (Scale<(fFlattenedScale*0.8)) then begin

  FreeAndNil(fFlattenedShape);
  fFlattenedShape:=TpvVectorPathShape.Create(fPath);
  fFlattenedShape.FillRule:=fStyle.FillData.FillRule;
  fFlattenedShape.ConvertCurvesToLines(1.0/Scale);
  fFlattenedScale:=Scale;

 end else begin

  // The fill rule is cheap to set and can have changed without the geometry changing.
  fFlattenedShape.FillRule:=fStyle.FillData.FillRule;

 end;

 result:=fFlattenedShape;

end;

function TpvVectorGraphicsGraphic.GetFillColorAt(const aTick:TpvInt64;const aTickFraction:TpvDouble):TpvVectorGraphicsColor;
var Index:TpvSizeInt;
    Factor:TpvDouble;
begin

 result:=fStyle.FillData.Color;

 // Same shape as GetAlphaAt: the keys are walked in order, a finished span sets the value, a
 // running one blends towards it, and a span of no length is a jump.
 for Index:=0 to length(fTimeLine)-1 do begin
  if (fTimeLine[Index].Action=TpvVectorGraphicsTimeLineAction.Move) and fTimeLine[Index].HasFillColor then begin
   if fTimeLine[Index].EndTick<=aTick then begin
    result:=fTimeLine[Index].FillColor;
   end else if fTimeLine[Index].StartTick<=aTick then begin
    if fTimeLine[Index].EndTick=fTimeLine[Index].StartTick then begin
     result:=fTimeLine[Index].FillColor;
    end else begin
     Factor:=((aTick-fTimeLine[Index].StartTick)+aTickFraction)/(fTimeLine[Index].EndTick-fTimeLine[Index].StartTick);
     result:=InterpolateColor(fTimeLine[Index].InterpolationMode,Factor,result,fTimeLine[Index].FillColor);
    end;
   end;
  end;
 end;

end;

function TpvVectorGraphicsGraphic.GetStrokeColorAt(const aTick:TpvInt64;const aTickFraction:TpvDouble):TpvVectorGraphicsColor;
var Index:TpvSizeInt;
    Factor:TpvDouble;
begin

 result:=fStyle.StrokeFillData.Color;

 for Index:=0 to length(fTimeLine)-1 do begin
  if (fTimeLine[Index].Action=TpvVectorGraphicsTimeLineAction.Move) and fTimeLine[Index].HasStrokeColor then begin
   if fTimeLine[Index].EndTick<=aTick then begin
    result:=fTimeLine[Index].StrokeColor;
   end else if fTimeLine[Index].StartTick<=aTick then begin
    if fTimeLine[Index].EndTick=fTimeLine[Index].StartTick then begin
     result:=fTimeLine[Index].StrokeColor;
    end else begin
     Factor:=((aTick-fTimeLine[Index].StartTick)+aTickFraction)/(fTimeLine[Index].EndTick-fTimeLine[Index].StartTick);
     result:=InterpolateColor(fTimeLine[Index].InterpolationMode,Factor,result,fTimeLine[Index].StrokeColor);
    end;
   end;
  end;
 end;

end;

function TpvVectorGraphicsGraphic.GetStrokeWidthAt(const aTick:TpvInt64;const aTickFraction:TpvDouble):TpvDouble;
var Index:TpvSizeInt;
    Factor:TpvDouble;
begin

 result:=fStyle.StrokeWidth;

 for Index:=0 to length(fTimeLine)-1 do begin
  if (fTimeLine[Index].Action=TpvVectorGraphicsTimeLineAction.Move) and fTimeLine[Index].HasStrokeWidth then begin
   if fTimeLine[Index].EndTick<=aTick then begin
    result:=fTimeLine[Index].StrokeWidth;
   end else if fTimeLine[Index].StartTick<=aTick then begin
    if fTimeLine[Index].EndTick=fTimeLine[Index].StartTick then begin
     result:=fTimeLine[Index].StrokeWidth;
    end else begin
     Factor:=((aTick-fTimeLine[Index].StartTick)+aTickFraction)/(fTimeLine[Index].EndTick-fTimeLine[Index].StartTick);
     result:=Interpolate(fTimeLine[Index].InterpolationMode,Factor,result,result,fTimeLine[Index].StrokeWidth,fTimeLine[Index].StrokeWidth);
    end;
   end;
  end;
 end;

end;

procedure TpvVectorGraphicsGraphic.CalculateBoundingBox(const aTick:TpvInt64;const aMatrix:TpvVectorPathMatrix);
var Matrix:TpvVectorPathMatrix;
    Shape:TpvVectorPathShape;
    Contour:TpvVectorPathContour;
    Segment:TpvVectorPathSegment;
    SegmentLine:TpvVectorPathSegmentLine;
    Position:TpvVectorPathVector;
begin

 fBoundingBox:=TpvVectorGraphicsBoundingBox.CreateEmpty;

 Matrix:=GetMatrixAt(aTick,0.0)*aMatrix;

 // The flattened shape rather than the path, so that a curve's bulge is covered by its line
 // segments instead of only by its control points.
 Shape:=GetFlattenedShape(Max(0.125,Matrix.ExpansionFactor),aTick,0.0);

 if assigned(Shape) then begin
  for Contour in Shape.Contours do begin
   for Segment in Contour.Segments do begin
    if Segment is TpvVectorPathSegmentLine then begin
     SegmentLine:=TpvVectorPathSegmentLine(Segment);
     Position:=Matrix.Transform(SegmentLine.Points[0]);
     fBoundingBox.Extend(Position.x,Position.y);
     Position:=Matrix.Transform(SegmentLine.Points[1]);
     fBoundingBox.Extend(Position.x,Position.y);
    end;
   end;
  end;
 end;

end;

function TpvVectorGraphicsGraphic.PreparePaint(const aFillData:TpvVectorGraphicsFillData;
                                               const aColor:TpvVectorGraphicsColor;
                                               const aAlpha:TpvUInt8;
                                               var aContext:TpvVectorGraphicsRenderContext):TpvVectorGraphicsPaint;
var PaintMatrix:TpvVectorPathMatrix;
    PatternWidth,PatternHeight:TpvInt32;
begin

 result:=nil;

 if aFillData.Mode=TpvVectorGraphicsFillMode.None then begin
  exit;
 end;

 // Inside a mask pass only coverage matters, so every paint collapses to plain white times the
 // alpha, which keeps the clip path of a gradient filled shape from being tinted by it.
 if aContext.MaskPass then begin
  fSolidPaint.Color:=pvVectorGraphicsModulateColor($ffffffff,aAlpha);
  result:=fSolidPaint;
  exit;
 end;

 case aFillData.Mode of

  TpvVectorGraphicsFillMode.LinearGradient:begin

   if not assigned(fLinearGradientPaint) then begin
    fLinearGradientPaint:=TpvVectorGraphicsLinearGradientPaint.Create;
   end;

   // A device pixel first goes back into the object's own space and from there into the space
   // the gradient was defined in.
   PaintMatrix:=fCurrentMatrixInverse*aFillData.SimpleMatrix.ToMatrix;

   fLinearGradientPaint.Matrix:=PaintMatrix;
   fLinearGradientPaint.Stops:=aFillData.GradientStops;
   fLinearGradientPaint.SpreadMethod:=aFillData.SpreadMethod;
   fLinearGradientPaint.Radius:=aFillData.GradientRadius;
   fLinearGradientPaint.Opacity:=aAlpha;
   fLinearGradientPaint.Update;

   result:=fLinearGradientPaint;

  end;

  TpvVectorGraphicsFillMode.RadialGradient:begin

   if not assigned(fRadialGradientPaint) then begin
    fRadialGradientPaint:=TpvVectorGraphicsRadialGradientPaint.Create;
   end;

   PaintMatrix:=fCurrentMatrixInverse*aFillData.SimpleMatrix.ToMatrix;

   fRadialGradientPaint.Matrix:=PaintMatrix;
   fRadialGradientPaint.FocusMatrix:=aFillData.GradientFocusSimpleMatrix.ToMatrix;
   fRadialGradientPaint.FocusX:=aFillData.GradientFocusX;
   fRadialGradientPaint.FocusY:=aFillData.GradientFocusY;
   fRadialGradientPaint.Stops:=aFillData.GradientStops;
   fRadialGradientPaint.SpreadMethod:=aFillData.SpreadMethod;
   fRadialGradientPaint.Radius:=aFillData.GradientRadius;
   fRadialGradientPaint.Opacity:=aAlpha;
   fRadialGradientPaint.Update;

   result:=fRadialGradientPaint;

  end;

  TpvVectorGraphicsFillMode.Image:begin

   if assigned(aFillData.Image) and (aFillData.Image.Width>0) and (aFillData.Image.Height>0) then begin

    if not assigned(fImagePaint) then begin
     fImagePaint:=TpvVectorGraphicsImagePaint.Create;
    end;

    fImagePaint.SetImage(aFillData.Image.GetPixelPointer,aFillData.Image.Width,aFillData.Image.Height);
    fImagePaint.Matrix:=fCurrentMatrixInverse*aFillData.SimpleMatrix.ToMatrix;
    fImagePaint.SpreadMethod:=aFillData.SpreadMethod;
    fImagePaint.Opacity:=aAlpha;

    result:=fImagePaint;

   end;

  end;

  TpvVectorGraphicsFillMode.Pattern:begin

   if assigned(aFillData.Pattern) then begin

    // The tile is rendered at the size the target is being drawn at, so that it stays sharp
    // however far the document is zoomed in.
    PatternWidth:=Max(1,Min(round(aFillData.Pattern.Width*Max(1.0,fCurrentMatrix.ExpansionFactor)),4096));
    PatternHeight:=Max(1,Min(round(aFillData.Pattern.Height*Max(1.0,fCurrentMatrix.ExpansionFactor)),4096));
    aFillData.Pattern.RenderToPatternBuffer(aContext,PatternWidth,PatternHeight);

    if assigned(aFillData.Pattern.PatternBuffer) then begin

     if not assigned(fImagePaint) then begin
      fImagePaint:=TpvVectorGraphicsImagePaint.Create;
     end;

     fImagePaint.SetImage(aFillData.Pattern.PatternBuffer.PixelPointer,
                          aFillData.Pattern.PatternBuffer.Width,
                          aFillData.Pattern.PatternBuffer.Height);

     // The tile buffer is in pixels, the pattern is in user units, so the sample position has
     // to be scaled by the ratio between the two.
     PaintMatrix:=fCurrentMatrixInverse*aFillData.SimpleMatrix.ToMatrix;
     PaintMatrix:=PaintMatrix*TpvVectorPathMatrix.CreateScale(PatternWidth/Max(1e-12,aFillData.Pattern.Width),
                                                              PatternHeight/Max(1e-12,aFillData.Pattern.Height));

     fImagePaint.Matrix:=PaintMatrix;
     fImagePaint.SpreadMethod:=aFillData.SpreadMethod;
     fImagePaint.Opacity:=aAlpha;

     result:=fImagePaint;

    end;

   end;

  end;

  else {TpvVectorGraphicsFillMode.Color:}begin

   fSolidPaint.Color:=pvVectorGraphicsModulateColor(aColor,aAlpha);
   result:=fSolidPaint;

  end;

 end;

end;

procedure TpvVectorGraphicsGraphic.RenderPass(const aShape:TpvVectorPathShape;
                                              const aFillData:TpvVectorGraphicsFillData;
                                              const aColor:TpvVectorGraphicsColor;
                                              var aContext:TpvVectorGraphicsRenderContext);
var CurrentPaint:TpvVectorGraphicsPaint;
begin

 if not assigned(aShape) then begin
  exit;
 end;

 // No paint means the fill mode is none, which is not an error but simply nothing to draw.
 CurrentPaint:=PreparePaint(aFillData,aColor,fCurrentAlpha,aContext);
 if not assigned(CurrentPaint) then begin
  exit;
 end;

 // The fill rule comes from the paint's own data rather than from the shape, which is what lets
 // one and the same outline be filled and stroked under different rules.
 aContext.Rasterizer.SetPaint(CurrentPaint);
 aContext.Rasterizer.FillRule:=aFillData.FillRule;
 aContext.Rasterizer.Mask:=aContext.Mask;

 // Only a recording backend reads this; the software path has the element in a buffer of its own
 // at this point and applies the mode when that buffer is composited.
 aContext.Rasterizer.ElementBlendMode:=fBlendMode;

 aContext.Rasterizer.FillShape(aShape,fCurrentMatrix,false);

 // The paint and the blend mode belong to this object and must not stay behind for the next one.
 aContext.Rasterizer.SetPaint(nil);
 aContext.Rasterizer.ElementBlendMode:=TpvVectorGraphicsBlendMode.Normal;

end;

procedure TpvVectorGraphicsGraphic.Render(var aContext:TpvVectorGraphicsRenderContext);
var Shape,StrokedShape:TpvVectorPathShape;
    Scale:TpvDouble;
    StrokeSettings:TpvVectorPathStrokeSettings;
    FillColor,StrokeColor:TpvVectorGraphicsColor;
    StrokeWidth:TpvDouble;
    FillData,StrokeFillData:TpvVectorGraphicsFillData;
    OldBuffer:TpvVectorGraphicsPixelBuffer;
    UseOffImage,UseIsolation:boolean;
    OffImageAlpha:TpvUInt8;
    BlendMode:TpvVectorGraphicsBlendMode;
    StrokeMatrix:TpvVectorPathMatrix;
begin

 inherited Render(aContext);

 if (not GetVisibleAt(aContext.Tick)) or (fCurrentAlpha=0) then begin
  exit;
 end;

 Scale:=Max(0.125,fCurrentMatrix.ExpansionFactor);
 Shape:=GetFlattenedShape(Scale,aContext.Tick,aContext.TickFraction);
 if not assigned(Shape) then begin
  exit;
 end;

 FillData.Assign(fStyle.FillData);
 StrokeFillData.Assign(fStyle.StrokeFillData);
 FillColor:=GetFillColorAt(aContext.Tick,aContext.TickFraction);
 StrokeColor:=GetStrokeColorAt(aContext.Tick,aContext.TickFraction);
 StrokeWidth:=GetStrokeWidthAt(aContext.Tick,aContext.TickFraction);

 // The per-call override replaces the colors without ever touching the stored style, so the
 // same object can be drawn in several states within one frame.
 if aContext.Override_.HasFillColor then begin
  FillColor:=aContext.Override_.FillColor;
  if FillData.Mode<>TpvVectorGraphicsFillMode.None then begin
   FillData.Mode:=TpvVectorGraphicsFillMode.Color;
  end;
 end;
 if aContext.Override_.HasStrokeColor then begin
  StrokeColor:=aContext.Override_.StrokeColor;
  if StrokeFillData.Mode<>TpvVectorGraphicsFillMode.None then begin
   StrokeFillData.Mode:=TpvVectorGraphicsFillMode.Color;
  end;
 end;
 if aContext.Override_.HasStrokeWidth then begin
  StrokeWidth:=aContext.Override_.StrokeWidth;
 end;

 // An outline pass draws a hairline around everything and nothing else, which is what an editor
 // wants for a selection or a wireframe view.
 if aContext.OutlinePass or fOutline then begin
  StrokeFillData.Mode:=TpvVectorGraphicsFillMode.Color;
  StrokeColor:=aContext.OutlineColor;
  StrokeWidth:=1.0/Scale;
  FillData.Mode:=TpvVectorGraphicsFillMode.None;
 end;

 // Fill and stroke overlap along the outline, so a semi transparent object is drawn into a
 // buffer of its own and composited once, rather than showing a darker seam there. A blend mode
 // needs the same buffer, because it applies to the element as a whole and not to each of its
 // two passes separately.
 ResolveBlending(aContext,BlendMode,UseIsolation);
 UseOffImage:=UseIsolation or (fStyle.OffImage and (fCurrentAlpha<>255) and not aContext.MaskPass);

 OldBuffer:=nil;
 OffImageAlpha:=fCurrentAlpha;

 try

  if UseOffImage then begin

   OldBuffer:=BeginOffImage(aContext,UseIsolation);

   // The opacity belongs to the one composite at the end, so the passes themselves draw at full
   // strength; applying it in both places would square it.
   fCurrentAlpha:=255;

  end;

  if FillData.Mode<>TpvVectorGraphicsFillMode.None then begin
   RenderPass(Shape,FillData,FillColor,aContext);
  end;

  if (StrokeFillData.Mode<>TpvVectorGraphicsFillMode.None) and (StrokeWidth>0.0) then begin

   if not assigned(fStroker) then begin
    fStroker:=TpvVectorPathStroker.Create;
   end;

   StrokeSettings:=TpvVectorPathStrokeSettings.CreateDefault;
   StrokeSettings.Width:=StrokeWidth;
   StrokeSettings.CapMode:=fStyle.StrokeCapMode;
   StrokeSettings.JoinMode:=fStyle.StrokeJoinMode;
   StrokeSettings.InnerJoinMode:=fStyle.StrokeInnerJoinMode;
   StrokeSettings.MiterLimit:=fStyle.StrokeMiterLimit;
   StrokeSettings.InnerMiterLimit:=fStyle.StrokeInnerMiterLimit;
   StrokeSettings.DashArray:=fStyle.StrokeDashArray;
   StrokeSettings.DashOffset:=fStyle.StrokeDashOffset;

   if fStyle.StrokeWidthFixed then begin

    // A non-scaling stroke has to be built after the transform, so the outline is generated in
    // device space and then drawn without a further transform.
    StrokedShape:=nil;
    Shape:=Shape.Clone;
    try
     Shape.Transform(fCurrentMatrix);
     StrokedShape:=fStroker.Stroke(Shape,StrokeSettings,Scale);
    finally
     FreeAndNil(Shape);
    end;
    StrokeMatrix:=TpvVectorPathMatrix.CreateIdentity;

   end else begin

    StrokedShape:=fStroker.Stroke(Shape,StrokeSettings,Scale);
    StrokeMatrix:=fCurrentMatrix;

   end;

   if assigned(StrokedShape) then begin
    try

     // The stroke outline is always a non-zero shape, whatever the fill rule of the original.
     StrokedShape.FillRule:=TpvVectorPathFillRule.NonZero;
     StrokeFillData.FillRule:=TpvVectorPathFillRule.NonZero;

     if fStyle.StrokeWidthFixed then begin
      // The transform already sits in the geometry, so it must not be applied a second time.
      fCurrentMatrix:=StrokeMatrix;
      try
       RenderPass(StrokedShape,StrokeFillData,StrokeColor,aContext);
      finally
       fCurrentMatrix:=GetMatrixAt(aContext.Tick,aContext.TickFraction)*aContext.ParentMatrix;
      end;
     end else begin
      RenderPass(StrokedShape,StrokeFillData,StrokeColor,aContext);
     end;

    finally
     FreeAndNil(StrokedShape);
    end;
   end;

  end;

 finally

  if UseOffImage then begin
   fCurrentAlpha:=OffImageAlpha;
   EndOffImage(aContext,OldBuffer,OffImageAlpha,BlendMode,UseIsolation);
  end;

 end;

end;

{ TpvVectorGraphicsShape }

constructor TpvVectorGraphicsShape.Create(const aDocument:TpvVectorGraphicsDocument;const aParent:TpvVectorGraphicsObject);
begin
 inherited Create(aDocument,aParent);
 // The path is the authority here, so it never gets regenerated from parameters.
 fPathDirty:=false;
end;

{ TpvVectorGraphicsLine }

constructor TpvVectorGraphicsLine.Create(const aDocument:TpvVectorGraphicsDocument;const aParent:TpvVectorGraphicsObject);
begin

 inherited Create(aDocument,aParent);

 fX1:=0.0;
 fY1:=0.0;
 fX2:=0.0;
 fY2:=0.0;

 // A line has no interior to fill, so unlike every other graphic it starts out stroked.
 fStyle.FillData.Mode:=TpvVectorGraphicsFillMode.None;
 fStyle.StrokeFillData.Mode:=TpvVectorGraphicsFillMode.Color;

end;

procedure TpvVectorGraphicsLine.SetCoordinate(const aIndex:TpvInt32;const aValue:TpvDouble);
begin

 // The indexed access exists for the editor side, which addresses the four values as one array.
 case aIndex of
  0:begin
   fX1:=aValue;
  end;
  1:begin
   fY1:=aValue;
  end;
  2:begin
   fX2:=aValue;
  end;
  else begin
   fY2:=aValue;
  end;
 end;

 MarkPathDirty;

end;

procedure TpvVectorGraphicsLine.Assign(const aFrom:TpvVectorGraphicsObject);
begin

 inherited Assign(aFrom);

 if aFrom is TpvVectorGraphicsLine then begin
  fX1:=TpvVectorGraphicsLine(aFrom).fX1;
  fY1:=TpvVectorGraphicsLine(aFrom).fY1;
  fX2:=TpvVectorGraphicsLine(aFrom).fX2;
  fY2:=TpvVectorGraphicsLine(aFrom).fY2;
  MarkPathDirty;
 end;

end;

procedure TpvVectorGraphicsLine.UpdatePath;
begin

 // Deliberately left open, since a closed contour would stroke the line twice.
 fPath.Commands.Clear;
 fPath.MoveTo(fX1,fY1);
 fPath.LineTo(fX2,fY2);

end;

{ TpvVectorGraphicsRectangle }

constructor TpvVectorGraphicsRectangle.Create(const aDocument:TpvVectorGraphicsDocument;const aParent:TpvVectorGraphicsObject);
begin

 inherited Create(aDocument,aParent);

 fX:=0.0;
 fY:=0.0;
 fWidth:=0.0;
 fHeight:=0.0;

 // Zero radii mean square corners.
 fRadiusX:=0.0;
 fRadiusY:=0.0;

end;

procedure TpvVectorGraphicsRectangle.Assign(const aFrom:TpvVectorGraphicsObject);
begin

 inherited Assign(aFrom);

 if aFrom is TpvVectorGraphicsRectangle then begin
  fX:=TpvVectorGraphicsRectangle(aFrom).fX;
  fY:=TpvVectorGraphicsRectangle(aFrom).fY;
  fWidth:=TpvVectorGraphicsRectangle(aFrom).fWidth;
  fHeight:=TpvVectorGraphicsRectangle(aFrom).fHeight;
  fRadiusX:=TpvVectorGraphicsRectangle(aFrom).fRadiusX;
  fRadiusY:=TpvVectorGraphicsRectangle(aFrom).fRadiusY;
  MarkPathDirty;
 end;

end;

procedure TpvVectorGraphicsRectangle.UpdatePath;
const KappaComplement=1.0-0.5522847498307933; // the usual four-arc circle approximation
var RadiusX,RadiusY:TpvDouble;
begin

 fPath.Commands.Clear;

 // SVG says a missing radius copies the other one, and that neither may exceed half the side.
 RadiusX:=fRadiusX;
 RadiusY:=fRadiusY;
 if (RadiusX<=0.0) and (RadiusY>0.0) then begin
  RadiusX:=RadiusY;
 end else if (RadiusY<=0.0) and (RadiusX>0.0) then begin
  RadiusY:=RadiusX;
 end;
 if RadiusX>(fWidth*0.5) then begin
  RadiusX:=fWidth*0.5;
 end;
 if RadiusY>(fHeight*0.5) then begin
  RadiusY:=fHeight*0.5;
 end;

 // Rounded: four sides with a quarter ellipse at each corner.
 if (RadiusX>0.0) and (RadiusY>0.0) then begin

  fPath.MoveTo(fX+RadiusX,fY);
  fPath.LineTo((fX+fWidth)-RadiusX,fY);
  fPath.CubicCurveTo((fX+fWidth)-(RadiusX*KappaComplement),fY,
                     fX+fWidth,fY+(RadiusY*KappaComplement),
                     fX+fWidth,fY+RadiusY);
  fPath.LineTo(fX+fWidth,(fY+fHeight)-RadiusY);
  fPath.CubicCurveTo(fX+fWidth,(fY+fHeight)-(RadiusY*KappaComplement),
                     (fX+fWidth)-(RadiusX*KappaComplement),fY+fHeight,
                     (fX+fWidth)-RadiusX,fY+fHeight);
  fPath.LineTo(fX+RadiusX,fY+fHeight);
  fPath.CubicCurveTo(fX+(RadiusX*KappaComplement),fY+fHeight,
                     fX,(fY+fHeight)-(RadiusY*KappaComplement),
                     fX,(fY+fHeight)-RadiusY);
  fPath.LineTo(fX,fY+RadiusY);
  fPath.CubicCurveTo(fX,fY+(RadiusY*KappaComplement),
                     fX+(RadiusX*KappaComplement),fY,
                     fX+RadiusX,fY);
  fPath.Close;

 end else begin

  fPath.MoveTo(fX,fY);
  fPath.LineTo(fX+fWidth,fY);
  fPath.LineTo(fX+fWidth,fY+fHeight);
  fPath.LineTo(fX,fY+fHeight);
  fPath.Close;

 end;

end;

{ TpvVectorGraphicsEllipse }

constructor TpvVectorGraphicsEllipse.Create(const aDocument:TpvVectorGraphicsDocument;const aParent:TpvVectorGraphicsObject);
begin

 inherited Create(aDocument,aParent);

 fCenterX:=0.0;
 fCenterY:=0.0;
 fRadiusX:=0.0;
 fRadiusY:=0.0;

end;

procedure TpvVectorGraphicsEllipse.Assign(const aFrom:TpvVectorGraphicsObject);
begin

 inherited Assign(aFrom);

 if aFrom is TpvVectorGraphicsEllipse then begin
  fCenterX:=TpvVectorGraphicsEllipse(aFrom).fCenterX;
  fCenterY:=TpvVectorGraphicsEllipse(aFrom).fCenterY;
  fRadiusX:=TpvVectorGraphicsEllipse(aFrom).fRadiusX;
  fRadiusY:=TpvVectorGraphicsEllipse(aFrom).fRadiusY;
  MarkPathDirty;
 end;

end;

procedure TpvVectorGraphicsEllipse.UpdatePath;
begin

 // Circle, ellipse and the SVG circle element all end up here, only with different radii.
 fPath.Commands.Clear;
 fPath.Ellipse(fCenterX,fCenterY,fRadiusX,fRadiusY);

end;

{ TpvVectorGraphicsFontProvider }

function TpvVectorGraphicsFontProvider.GetFont(const aFamily:TpvUTF8String;const aBold,aItalic:boolean):TObject;
begin
 result:=nil;
end;

{ TpvVectorGraphicsText }

constructor TpvVectorGraphicsText.Create(const aDocument:TpvVectorGraphicsDocument;const aParent:TpvVectorGraphicsObject);
begin

 inherited Create(aDocument,aParent);

 fX:=0.0;
 fY:=0.0;
 fText:='';

 // An empty family means whatever the document's font provider hands out as its default.
 fFontFamily:='';
 fFontSize:=12.0;
 fBold:=false;
 fItalic:=false;

 fAnchor:=TpvVectorGraphicsTextAnchor.Start;
 fBaseline:=TpvVectorGraphicsTextBaseline.Alphabetic;
 fLetterSpacing:=0.0;

 // Filled in by UpdatePath and read by whoever lays out a following run.
 fAdvanceWidth:=0.0;

end;

procedure TpvVectorGraphicsText.Assign(const aFrom:TpvVectorGraphicsObject);
begin

 inherited Assign(aFrom);

 if aFrom is TpvVectorGraphicsText then begin
  fX:=TpvVectorGraphicsText(aFrom).fX;
  fY:=TpvVectorGraphicsText(aFrom).fY;
  fText:=TpvVectorGraphicsText(aFrom).fText;
  fFontFamily:=TpvVectorGraphicsText(aFrom).fFontFamily;
  fFontSize:=TpvVectorGraphicsText(aFrom).fFontSize;
  fBold:=TpvVectorGraphicsText(aFrom).fBold;
  fItalic:=TpvVectorGraphicsText(aFrom).fItalic;
  fAnchor:=TpvVectorGraphicsText(aFrom).fAnchor;
  fBaseline:=TpvVectorGraphicsText(aFrom).fBaseline;
  fLetterSpacing:=TpvVectorGraphicsText(aFrom).fLetterSpacing;
  MarkPathDirty;
 end;

end;

procedure TpvVectorGraphicsText.UpdatePath;
var Font:TpvTrueTypeFont;
    PolygonBuffer:TpvTrueTypeFontPolygonBuffer;
    Scale,OffsetX,OffsetY,Ascender,Descender:TpvDouble;
    UnitsPerEm:TpvInt32;
    Matrix:TpvVectorPathMatrix;
begin

 fPath.Commands.Clear;
 fAdvanceWidth:=0.0;

 // Without a provider there is no font, and therefore no geometry at all.
 if (Length(fText)=0) or (not assigned(fDocument)) or (not assigned(fDocument.fFontProvider)) then begin
  exit;
 end;

 Font:=TpvTrueTypeFont(fDocument.fFontProvider.GetFont(fFontFamily,fBold,fItalic));
 if not assigned(Font) then begin
  exit;
 end;

 UnitsPerEm:=Font.GetUnitsPerEm;
 if UnitsPerEm<=0 then begin
  exit;
 end;

 // A negative size means the scale factor comes out as size over units per em, so asking for
 // exactly minus the units per em gives a factor of one and therefore raw design units, which
 // keeps the result independent of whatever target resolution the font was set up with.
 Font.Size:=-UnitsPerEm;

 FillChar(PolygonBuffer,SizeOf(TpvTrueTypeFontPolygonBuffer),#0);
 try
  Font.ResetPolygonBuffer(PolygonBuffer);
  Font.FillTextPolygonBuffer(PolygonBuffer,fText,0,0);
  if PolygonBuffer.CountCommands=0 then begin
   exit;
  end;
  PolygonBuffer.ConvertToVectorPath(fPath);
 finally
  Font.ResetPolygonBuffer(PolygonBuffer);
 end;

 Scale:=fFontSize/UnitsPerEm;
 fAdvanceWidth:=Font.TextWidth(fText)*Scale;

 // The letter spacing is not fed into the font, so it only shows up in the advance width that
 // the anchor is computed from.
 if abs(fLetterSpacing)>1e-12 then begin
  fAdvanceWidth:=fAdvanceWidth+(fLetterSpacing*Max(0,Length(fText)-1));
 end;

 // The anchor shifts the whole run horizontally around the given position.
 OffsetX:=fX;
 case fAnchor of
  TpvVectorGraphicsTextAnchor.Middle:begin
   OffsetX:=OffsetX-(fAdvanceWidth*0.5);
  end;
  TpvVectorGraphicsTextAnchor.End_:begin
   OffsetX:=OffsetX-fAdvanceWidth;
  end;
  else {TpvVectorGraphicsTextAnchor.Start:}begin
  end;
 end;

 // The horizontal metrics out of hhea are the ones that describe a line of text; OS/2 carries
 // its own pair that fonts do not always keep consistent with them.
 Ascender:=Font.HorizontalAscender*Scale;
 Descender:=Font.HorizontalDescender*Scale;

 // And the baseline shifts it vertically.
 OffsetY:=fY;
 case fBaseline of
  TpvVectorGraphicsTextBaseline.Central,
  TpvVectorGraphicsTextBaseline.Middle:begin
   OffsetY:=OffsetY+((Ascender+Descender)*0.5);
  end;
  TpvVectorGraphicsTextBaseline.Hanging,
  TpvVectorGraphicsTextBaseline.Top:begin
   OffsetY:=OffsetY+Ascender;
  end;
  TpvVectorGraphicsTextBaseline.Bottom:begin
   OffsetY:=OffsetY+Descender;
  end;
  else {TpvVectorGraphicsTextBaseline.Alphabetic:}begin
  end;
 end;

 // The glyph outlines are in design units with y pointing up, so they get scaled to the
 // requested font size and flipped into the y-down space SVG uses.
 Matrix:=TpvVectorPathMatrix.CreateScale(Scale,-Scale)*
         TpvVectorPathMatrix.CreateTranslate(OffsetX,OffsetY);
 fPath.Transform(Matrix);

end;

{ TpvVectorGraphicsImageObject }

constructor TpvVectorGraphicsImageObject.Create(const aDocument:TpvVectorGraphicsDocument;const aParent:TpvVectorGraphicsObject);
begin

 inherited Create(aDocument,aParent);

 fX:=0.0;
 fY:=0.0;
 fWidth:=0.0;
 fHeight:=0.0;

 // The object is a rectangle filled with its image, so the image is its fill from the start.
 fStyle.FillData.Mode:=TpvVectorGraphicsFillMode.Image;
 fStyle.StrokeFillData.Mode:=TpvVectorGraphicsFillMode.None;

end;

procedure TpvVectorGraphicsImageObject.Assign(const aFrom:TpvVectorGraphicsObject);
begin

 inherited Assign(aFrom);

 if aFrom is TpvVectorGraphicsImageObject then begin
  fX:=TpvVectorGraphicsImageObject(aFrom).fX;
  fY:=TpvVectorGraphicsImageObject(aFrom).fY;
  fWidth:=TpvVectorGraphicsImageObject(aFrom).fWidth;
  fHeight:=TpvVectorGraphicsImageObject(aFrom).fHeight;
  MarkPathDirty;
 end;

end;

procedure TpvVectorGraphicsImageObject.SetImage(const aImage:TpvVectorGraphicsImageResource);
begin
 fStyle.FillData.Image:=aImage;
 fStyle.FillData.Mode:=TpvVectorGraphicsFillMode.Image;
end;

procedure TpvVectorGraphicsImageObject.UpdatePath;
var ScaleX,ScaleY:TpvDouble;
begin

 fPath.Commands.Clear;
 fPath.MoveTo(fX,fY);
 fPath.LineTo(fX+fWidth,fY);
 fPath.LineTo(fX+fWidth,fY+fHeight);
 fPath.LineTo(fX,fY+fHeight);
 fPath.Close;

 // The image paint samples in image pixels, so the fill matrix has to map the rectangle in user
 // units onto the pixel grid of the image.
 if assigned(fStyle.FillData.Image) and (fWidth>1e-12) and (fHeight>1e-12) then begin
  ScaleX:=fStyle.FillData.Image.Width/fWidth;
  ScaleY:=fStyle.FillData.Image.Height/fHeight;
 end else begin
  ScaleX:=1.0;
  ScaleY:=1.0;
 end;

 fStyle.FillData.SimpleMatrix:=TpvVectorGraphicsSimpleMatrix.CreateDefault;
 fStyle.FillData.SimpleMatrix.Matrix:=TpvVectorPathMatrix.CreateTranslate(-fX,-fY)*
                                      TpvVectorPathMatrix.CreateScale(ScaleX,ScaleY);

end;

{ TpvVectorGraphicsGroup }

constructor TpvVectorGraphicsGroup.Create(const aDocument:TpvVectorGraphicsDocument;const aParent:TpvVectorGraphicsObject);
begin

 inherited Create(aDocument,aParent);

 // The children belong to the group, the sorted view only borrows them.
 fChildren:=TpvVectorGraphicsObjectList.Create;
 fChildren.OwnsObjects:=true;
 fSortedChildren:=TpvVectorGraphicsObjectList.Create;
 fSortedChildren.OwnsObjects:=false;

 fWidth:=0.0;
 fHeight:=0.0;
 fBackgroundColor:=0;
 fGamma:=1.0;
 fLoopTime:=0;

 // The nesting level guards against a group that reaches itself through a group instance.
 fOffImage:=false;
 fNestingLevel:=0;

 // Created on first use, like the off-image buffer of the base class.
 fPatternBuffer:=nil;

end;

destructor TpvVectorGraphicsGroup.Destroy;
begin

 // The sorted view goes first, since freeing the owning list takes its entries with it.
 FreeAndNil(fSortedChildren);
 FreeAndNil(fChildren);

 FreeAndNil(fPatternBuffer);

 inherited Destroy;

end;

procedure TpvVectorGraphicsGroup.Assign(const aFrom:TpvVectorGraphicsObject);
begin

 inherited Assign(aFrom);

 // Only the group's own settings, the children are not duplicated.
 if aFrom is TpvVectorGraphicsGroup then begin
  fWidth:=TpvVectorGraphicsGroup(aFrom).fWidth;
  fHeight:=TpvVectorGraphicsGroup(aFrom).fHeight;
  fBackgroundColor:=TpvVectorGraphicsGroup(aFrom).fBackgroundColor;
  fGamma:=TpvVectorGraphicsGroup(aFrom).fGamma;
  fLoopTime:=TpvVectorGraphicsGroup(aFrom).fLoopTime;
  fOffImage:=TpvVectorGraphicsGroup(aFrom).fOffImage;
 end;

end;

function TpvVectorGraphicsGroup.Add(const aChild:TpvVectorGraphicsObject):TpvVectorGraphicsObject;
begin
 result:=aChild;
 if assigned(aChild) then begin
  fChildren.Add(aChild);
  aChild.fParent:=self;
 end;
end;

procedure TpvVectorGraphicsGroup.Remove(const aChild:TpvVectorGraphicsObject);
begin
 if assigned(aChild) then begin
  fSortedChildren.RemoveWithoutFree(aChild);
  fChildren.Remove(aChild);
 end;
end;

function TpvVectorGraphicsGroup.Extract(const aChild:TpvVectorGraphicsObject):TpvVectorGraphicsObject;
begin
 result:=aChild;
 if assigned(aChild) then begin
  fSortedChildren.RemoveWithoutFree(aChild);
  fChildren.RemoveWithoutFree(aChild);
  aChild.fParent:=nil;
 end;
end;

procedure TpvVectorGraphicsGroup.SortChildren;
var Index,OtherIndex,TargetIndex:TpvSizeInt;
    Current:TpvVectorGraphicsObject;
begin

 fSortedChildren.Clear;
 for Index:=0 to fChildren.Count-1 do begin
  fSortedChildren.Add(fChildren[Index]);
 end;

 // Insertion sort by z, which has to be stable so that objects with equal z keep the document
 // order that decides what covers what.
 for Index:=0 to fSortedChildren.Count-2 do begin

  // How far back the entry has to travel, where minus one means it already sits right.
  Current:=fSortedChildren[Index+1];
  TargetIndex:=-1;
  for OtherIndex:=Index downto 0 do begin
   if (fSortedChildren[OtherIndex].fZ>Current.fZ) and
      not SameValue(fSortedChildren[OtherIndex].fZ,Current.fZ) then begin
    TargetIndex:=OtherIndex;
   end else begin
    break;
   end;
  end;

  if TargetIndex>=0 then begin
   for OtherIndex:=Index downto TargetIndex do begin
    fSortedChildren[OtherIndex+1]:=fSortedChildren[OtherIndex];
   end;
   fSortedChildren[TargetIndex]:=Current;
  end;

 end;

end;

function TpvVectorGraphicsGroup.GetRealTick(const aTick:TpvInt64):TpvInt64;
begin

 // A loop time makes the group's own timeline wrap around independently of its parent's.
 if fLoopTime>0 then begin
  result:=aTick mod fLoopTime;
 end else begin
  result:=aTick;
 end;

end;

procedure TpvVectorGraphicsGroup.Update;
var Index:TpvSizeInt;
begin

 inherited Update;

 for Index:=0 to fChildren.Count-1 do begin
  fChildren[Index].Update;
 end;

end;

procedure TpvVectorGraphicsGroup.CalculateBoundingBox(const aTick:TpvInt64;const aMatrix:TpvVectorPathMatrix);
var Index:TpvSizeInt;
    Matrix:TpvVectorPathMatrix;
begin
 fBoundingBox:=TpvVectorGraphicsBoundingBox.CreateEmpty;
 Matrix:=GetMatrixAt(aTick,0.0)*aMatrix;
 for Index:=0 to fChildren.Count-1 do begin
  fChildren[Index].CalculateBoundingBox(aTick,Matrix);
  fBoundingBox.Combine(fChildren[Index].fBoundingBox);
 end;
end;

function TpvVectorGraphicsGroup.FindByID(const aID:TpvUTF8String):TpvVectorGraphicsObject;
var Index:TpvSizeInt;
begin

 // The group itself first, then its children in document order.
 result:=inherited FindByID(aID);
 if not assigned(result) then begin
  for Index:=0 to fChildren.Count-1 do begin
   result:=fChildren[Index].FindByID(aID);
   if assigned(result) then begin
    break;
   end;
  end;
 end;

end;

procedure TpvVectorGraphicsGroup.CollectObjects(const aList:TpvObjectGenericList<TpvVectorGraphicsObject>);
var Index:TpvSizeInt;
begin

 inherited CollectObjects(aList);

 for Index:=0 to fChildren.Count-1 do begin
  fChildren[Index].CollectObjects(aList);
 end;

end;

function TpvVectorGraphicsGroup.BuildMask(var aContext:TpvVectorGraphicsRenderContext;const aMaskMode:TpvVectorGraphicsMaskMode):TpvVectorGraphicsMask;
var Index:TpvSizeInt;
    MaskContext:TpvVectorGraphicsRenderContext;
    MaskBuffer:TpvVectorGraphicsPixelBuffer;
    OldBuffer:TpvVectorGraphicsPixelBuffer;
    Found:boolean;
begin

 result:=nil;

 // Nothing to build unless at least one child is marked as this kind of mask.
 Found:=false;
 for Index:=0 to fSortedChildren.Count-1 do begin
  if fSortedChildren[Index].fMaskMode=aMaskMode then begin
   Found:=true;
   break;
  end;
 end;
 if not Found then begin
  exit;
 end;

 result:=TpvVectorGraphicsMask.Create(aContext.Rasterizer.Width,aContext.Rasterizer.Height);
 result.Clear(0);

 // The mask is just a pixel buffer as far as the rasterizer is concerned, so the children get
 // drawn into it through the same path, only with paint switched off.
 MaskBuffer:=TpvVectorGraphicsPixelBuffer.CreateFromExternalMemory(result.PixelPointer,
                                                                   result.Width,
                                                                   result.Height,
                                                                   TpvVectorGraphicsColorSpaceMode.Integer);
 try

  OldBuffer:=aContext.Rasterizer.ReplacePixelBuffer(MaskBuffer);
  try

   // A mask is drawn on its own terms: no outer mask, no recoloring, full opacity.
   MaskContext:=aContext;
   MaskContext.Mask:=nil;
   MaskContext.MaskPass:=true;
   MaskContext.ParentMatrix:=fCurrentMatrix;
   MaskContext.ParentAlpha:=255;
   MaskContext.Override_:=TpvVectorGraphicsRenderOverride.CreateNone;

   for Index:=0 to fSortedChildren.Count-1 do begin
    if fSortedChildren[Index].fMaskMode=aMaskMode then begin
     fSortedChildren[Index].Render(MaskContext);
    end;
   end;

  finally
   aContext.Rasterizer.ReplacePixelBuffer(OldBuffer);
  end;

 finally
  FreeAndNil(MaskBuffer);
 end;

 // A luminance mask takes the brightness of what was drawn, not its alpha.
 if aMaskMode=TpvVectorGraphicsMaskMode.Mono then begin
  result.ConvertToLuminance;
 end;

end;

procedure TpvVectorGraphicsGroup.Render(var aContext:TpvVectorGraphicsRenderContext);
var Index:TpvSizeInt;
    ChildContext:TpvVectorGraphicsRenderContext;
    BuiltMask,PushedMask:TpvVectorGraphicsMask;
    OldBuffer:TpvVectorGraphicsPixelBuffer;
    UseOffImage,UseIsolation:boolean;
    BlendMode:TpvVectorGraphicsBlendMode;
    Corner:array[0..3] of TpvVectorPathVector;
begin

 inherited Render(aContext);

 // The nesting guard catches a group that contains itself through a use element.
 if (fNestingLevel>=pvVectorGraphicsMaximumNestingLevel) or
    (not GetVisibleAt(aContext.Tick)) or
    (fCurrentAlpha=0) then begin
  exit;
 end;

 inc(fNestingLevel);
 BuiltMask:=nil;
 PushedMask:=nil;
 OldBuffer:=nil;
 try

  SortChildren;

  // Only the root group sets the gamma, since it applies to the whole picture.
  if (not assigned(fParent)) and (fGamma>0.0) then begin
   aContext.Rasterizer.Gamma:=fGamma;
  end;

  ChildContext:=aContext;
  ChildContext.Tick:=GetRealTick(aContext.Tick);
  ChildContext.ParentMatrix:=fCurrentMatrix;
  ChildContext.ParentAlpha:=fCurrentAlpha;
  if not ChildContext.Override_.Recursive then begin
   ChildContext.Override_:=TpvVectorGraphicsRenderOverride.CreateNone;
  end;

  // Clip paths narrow by coverage, masks by color, and both end up as the same multiplying
  // buffer, which is why there is no polygon clipper anywhere in here.
  BuiltMask:=BuildMask(aContext,TpvVectorGraphicsMaskMode.Clip);
  if not assigned(BuiltMask) then begin
   BuiltMask:=BuildMask(aContext,TpvVectorGraphicsMaskMode.Mono);
  end;
  if not assigned(BuiltMask) then begin
   BuiltMask:=BuildMask(aContext,TpvVectorGraphicsMaskMode.Color);
  end;

  // A mask of its own narrows whatever was already in effect rather than replacing it.
  if assigned(BuiltMask) then begin
   if assigned(aContext.Mask) then begin
    BuiltMask.IntersectWith(aContext.Mask);
   end;
   PushedMask:=BuiltMask;
   ChildContext.Mask:=BuiltMask;
  end;

  // A group with its own opacity is composited as a whole, so that overlapping children do not
  // show through each other. A blend mode on the group itself, and an isolation that keeps the
  // blend modes of the children from reaching past the group, need the same buffer, only one
  // that starts out empty.
  ResolveBlending(aContext,BlendMode,UseIsolation);
  UseOffImage:=UseIsolation or (fOffImage and (fCurrentAlpha<>255) and not aContext.MaskPass);

  if UseOffImage then begin

   OldBuffer:=BeginOffImage(aContext,UseIsolation);

   // The children then draw at full opacity and the group opacity is applied once, at the end.
   ChildContext.ParentAlpha:=255;

  end;

  // Only the root paints a background, and only when the document asks for one.
  if assigned(fDocument) and fDocument.DrawBackground and (fDocument.Root=self) and
     ((fBackgroundColor shr 24)<>0) and not aContext.MaskPass then begin

   Corner[0]:=fCurrentMatrix.Transform(0.0,0.0);
   Corner[1]:=fCurrentMatrix.Transform(0.0,fHeight);
   Corner[2]:=fCurrentMatrix.Transform(fWidth,fHeight);
   Corner[3]:=fCurrentMatrix.Transform(fWidth,0.0);

   aContext.Rasterizer.SetPaint(nil);
   aContext.Rasterizer.Color:=fBackgroundColor;
   aContext.Rasterizer.FillRule:=TpvVectorPathFillRule.NonZero;
   aContext.Rasterizer.Mask:=ChildContext.Mask;
   aContext.Rasterizer.Quad(pvVectorGraphicsConvertCoordinate(Corner[0].x),pvVectorGraphicsConvertCoordinate(Corner[0].y),
                            pvVectorGraphicsConvertCoordinate(Corner[1].x),pvVectorGraphicsConvertCoordinate(Corner[1].y),
                            pvVectorGraphicsConvertCoordinate(Corner[2].x),pvVectorGraphicsConvertCoordinate(Corner[2].y),
                            pvVectorGraphicsConvertCoordinate(Corner[3].x),pvVectorGraphicsConvertCoordinate(Corner[3].y));
   aContext.Rasterizer.Render;

  end;

  // The children that are masks were already consumed above, so only the rest gets drawn.
  for Index:=0 to fSortedChildren.Count-1 do begin
   if (fSortedChildren[Index].fMaskMode=TpvVectorGraphicsMaskMode.None) and
      fSortedChildren[Index].GetVisibleAt(ChildContext.Tick) then begin
    fSortedChildren[Index].Render(ChildContext);
   end;
  end;

  if UseOffImage and assigned(OldBuffer) then begin
   EndOffImage(aContext,OldBuffer,fCurrentAlpha,BlendMode,UseIsolation);
   OldBuffer:=nil;
  end;

 finally

  // Should anything above have thrown, the buffer still has to go back.
  if assigned(OldBuffer) then begin
   aContext.Rasterizer.ReplacePixelBuffer(OldBuffer);
  end;

  FreeAndNil(PushedMask);
  dec(fNestingLevel);

 end;

end;

procedure TpvVectorGraphicsGroup.RenderToPatternBuffer(var aContext:TpvVectorGraphicsRenderContext;const aWidth,aHeight:TpvInt32);
var PatternContext:TpvVectorGraphicsRenderContext;
    OldBuffer:TpvVectorGraphicsPixelBuffer;
    OldVisible:boolean;
begin

 if (aWidth<=0) or (aHeight<=0) or (fWidth<=1e-12) or (fHeight<=1e-12) then begin
  exit;
 end;

 if fNestingLevel>=pvVectorGraphicsMaximumNestingLevel then begin
  exit;
 end;

 if (not assigned(fPatternBuffer)) or
    (fPatternBuffer.Width<>aWidth) or
    (fPatternBuffer.Height<>aHeight) then begin
  FreeAndNil(fPatternBuffer);
  fPatternBuffer:=TpvVectorGraphicsPixelBuffer.Create(aWidth,aHeight,TpvVectorGraphicsColorSpaceMode.Integer);
 end;
 fPatternBuffer.Clear(0);

 OldBuffer:=aContext.Rasterizer.PixelBuffer;
 aContext.Rasterizer.ReplacePixelBuffer(fPatternBuffer);

 // A pattern lives in defs and is therefore marked invisible, which must not stop it from being
 // drawn into its own tile.
 OldVisible:=fVisible;
 fVisible:=true;
 try

  PatternContext:=TpvVectorGraphicsRenderContext.Create(aContext.Rasterizer);
  PatternContext.Tick:=aContext.Tick;
  PatternContext.TickFraction:=aContext.TickFraction;

  // The tile is drawn into its own buffer, so the matrix only has to scale the pattern's user
  // unit box onto those pixels.
  PatternContext.ParentMatrix:=TpvVectorPathMatrix.CreateScale(aWidth/fWidth,aHeight/fHeight);
  Render(PatternContext);

 finally
  fVisible:=OldVisible;
  aContext.Rasterizer.ReplacePixelBuffer(OldBuffer);
 end;

end;

{ TpvVectorGraphicsGroupInstance }

constructor TpvVectorGraphicsGroupInstance.Create(const aDocument:TpvVectorGraphicsDocument;const aParent:TpvVectorGraphicsObject);
begin
 inherited Create(aDocument,aParent);
 fTarget:=nil;
 fRecursionGuard:=0;
end;

function TpvVectorGraphicsGroupInstance.GetGroup:TpvVectorGraphicsGroup;
begin
 if fTarget is TpvVectorGraphicsGroup then begin
  result:=TpvVectorGraphicsGroup(fTarget);
 end else begin
  result:=nil;
 end;
end;

procedure TpvVectorGraphicsGroupInstance.SetGroup(const aGroup:TpvVectorGraphicsGroup);
begin
 fTarget:=aGroup;
end;

procedure TpvVectorGraphicsGroupInstance.Assign(const aFrom:TpvVectorGraphicsObject);
begin
 inherited Assign(aFrom);
 if aFrom is TpvVectorGraphicsGroupInstance then begin
  fTarget:=TpvVectorGraphicsGroupInstance(aFrom).fTarget;
 end;
end;

procedure TpvVectorGraphicsGroupInstance.CalculateBoundingBox(const aTick:TpvInt64;const aMatrix:TpvVectorPathMatrix);
begin

 fBoundingBox:=TpvVectorGraphicsBoundingBox.CreateEmpty;

 // Same guard as in Render, since a cycle would recurse here just as well.
 if assigned(fTarget) and (fRecursionGuard<pvVectorGraphicsMaximumNestingLevel) then begin
  inc(fRecursionGuard);
  try
   fTarget.CalculateBoundingBox(aTick,GetMatrixAt(aTick,0.0)*aMatrix);
   fBoundingBox:=fTarget.fBoundingBox;
  finally
   dec(fRecursionGuard);
  end;
 end;

end;

procedure TpvVectorGraphicsGroupInstance.Render(var aContext:TpvVectorGraphicsRenderContext);
var ChildContext:TpvVectorGraphicsRenderContext;
    OldBuffer:TpvVectorGraphicsPixelBuffer;
    BlendMode:TpvVectorGraphicsBlendMode;
    UseIsolation:boolean;
begin

 inherited Render(aContext);

 if (not assigned(fTarget)) or not GetVisibleAt(aContext.Tick) then begin
  exit;
 end;

 if fCurrentAlpha=0 then begin
  exit;
 end;

 // A use element that ends up pointing at itself, directly or around a loop, would otherwise
 // recurse until the stack runs out.
 if fRecursionGuard>=pvVectorGraphicsMaximumNestingLevel then begin
  exit;
 end;

 // A blend mode on a use element applies to the whole referenced subtree, so it goes through a
 // buffer of its own just like one on a group would.
 ResolveBlending(aContext,BlendMode,UseIsolation);
 OldBuffer:=nil;

 inc(fRecursionGuard);
 try

  // The instance's own transform and opacity become the ones the target inherits.
  ChildContext:=aContext;
  ChildContext.ParentMatrix:=fCurrentMatrix;
  ChildContext.ParentAlpha:=fCurrentAlpha;

  if UseIsolation then begin
   OldBuffer:=BeginOffImage(aContext,true);
   ChildContext.ParentAlpha:=255;
  end;

  // The target may be hidden because it lives in defs, which is not a reason not to draw it
  // here, so visibility of the target itself is bypassed for a group.
  if (fTarget is TpvVectorGraphicsGroup) and not fTarget.fVisible then begin
   fTarget.fVisible:=true;
   try
    fTarget.Render(ChildContext);
   finally
    fTarget.fVisible:=false;
   end;
  end else begin
   fTarget.Render(ChildContext);
  end;

 finally

  if assigned(OldBuffer) then begin
   EndOffImage(aContext,OldBuffer,fCurrentAlpha,BlendMode,true);
  end;

  dec(fRecursionGuard);

 end;

end;

{ TpvVectorGraphicsDocument }

constructor TpvVectorGraphicsDocument.Create;
begin

 inherited Create;

 fIDHashMap:=TpvVectorGraphicsIDHashMap.Create(nil);

 fImages:=TpvVectorGraphicsImageResourceList.Create;
 fImages.OwnsObjects:=true;
 fImageIDCounter:=0;

 fRoot:=TpvVectorGraphicsGroup.Create(self,nil);
 fDefs:=TpvVectorGraphicsGroup.Create(self,nil);

 // The defs tree is never drawn on its own, only through use elements and paint servers.
 fDefs.Visible:=false;

 fDrawBackground:=false;

 fViewBox:=TpvVectorGraphicsBoundingBox.CreateEmpty;
 fHasViewBox:=false;

 fFontProvider:=nil;

 // The SVG default is xMidYMid meet, so uniform scaling, centered, fitted inside.
 fPreserveAspectRatioUniform:=true;
 fPreserveAspectRatioMeet:=true;
 fPreserveAspectRatioAlignX:=0.5;
 fPreserveAspectRatioAlignY:=0.5;

end;

destructor TpvVectorGraphicsDocument.Destroy;
begin

 // The trees go before the identifier table, since every object unregisters itself on the way out.
 FreeAndNil(fRoot);
 FreeAndNil(fDefs);

 FreeAndNil(fImages);
 FreeAndNil(fIDHashMap);

 inherited Destroy;

end;

function TpvVectorGraphicsDocument.GetWidth:TpvDouble;
begin
 result:=fRoot.fWidth;
end;

function TpvVectorGraphicsDocument.GetHeight:TpvDouble;
begin
 result:=fRoot.fHeight;
end;

procedure TpvVectorGraphicsDocument.SetWidth(const aWidth:TpvDouble);
begin
 fRoot.fWidth:=aWidth;
end;

procedure TpvVectorGraphicsDocument.SetHeight(const aHeight:TpvDouble);
begin
 fRoot.fHeight:=aHeight;
end;

procedure TpvVectorGraphicsDocument.Update;
begin
 fDefs.Update;
 fRoot.Update;
end;

procedure TpvVectorGraphicsDocument.CalculateBoundingBox(const aTick:TpvInt64);
begin
 fRoot.CalculateBoundingBox(aTick,TpvVectorPathMatrix.CreateIdentity);
end;

procedure TpvVectorGraphicsDocument.RegisterID(const aID:TpvUTF8String;const aObject:TpvVectorGraphicsObject);
begin
 if length(aID)>0 then begin
  fIDHashMap.Add(aID,aObject);
 end;
end;

procedure TpvVectorGraphicsDocument.UnregisterID(const aID:TpvUTF8String);
begin
 if length(aID)>0 then begin
  fIDHashMap.Delete(aID);
 end;
end;

function TpvVectorGraphicsDocument.FindByID(const aID:TpvUTF8String):TpvVectorGraphicsObject;
begin

 if not fIDHashMap.TryGet(aID,result) then begin

  // The hash map is the fast path; a tree walk still finds objects whose id was set before they
  // were attached to this document.
  result:=fRoot.FindByID(aID);
  if not assigned(result) then begin
   result:=fDefs.FindByID(aID);
  end;

 end;

end;

function TpvVectorGraphicsDocument.NewImage:TpvVectorGraphicsImageResource;
begin

 // The counter gives every image a number of its own, which the Berosh format refers to.
 result:=TpvVectorGraphicsImageResource.Create(self);
 result.ID:=fImageIDCounter;
 inc(fImageIDCounter);

 fImages.Add(result);

end;

procedure TpvVectorGraphicsDocument.SetFontProvider(const aFontProvider:TpvVectorGraphicsFontProvider);
var List:TpvVectorGraphicsObjectList;
    Index:TpvSizeInt;
begin

 if fFontProvider=aFontProvider then begin
  exit;
 end;

 fFontProvider:=aFontProvider;

 // Every text object has to rebuild its outline, since a different provider may well hand back
 // a different font.
 List:=TpvVectorGraphicsObjectList.Create;
 try

  List.OwnsObjects:=false;
  fRoot.CollectObjects(List);
  fDefs.CollectObjects(List);

  for Index:=0 to List.Count-1 do begin
   if List[Index] is TpvVectorGraphicsText then begin
    TpvVectorGraphicsText(List[Index]).MarkPathDirty;
   end;
  end;

 finally
  FreeAndNil(List);
 end;

end;

procedure TpvVectorGraphicsDocument.Render(const aRasterizer:TpvVectorGraphicsRasterizer;
                                           const aMatrix:TpvVectorPathMatrix;
                                           const aTick:TpvInt64;
                                           const aTickFraction:TpvDouble);
var Context:TpvVectorGraphicsRenderContext;
begin
 Update;
 Context:=TpvVectorGraphicsRenderContext.Create(aRasterizer);
 Context.Tick:=aTick;
 Context.TickFraction:=aTickFraction;
 Context.ParentMatrix:=aMatrix;
 fRoot.Render(Context);
end;

procedure TpvVectorGraphicsDocument.RenderObject(const aObject:TpvVectorGraphicsObject;
                                                 const aRasterizer:TpvVectorGraphicsRasterizer;
                                                 const aMatrix:TpvVectorPathMatrix;
                                                 const aOverride:TpvVectorGraphicsRenderOverride;
                                                 const aTick:TpvInt64;
                                                 const aTickFraction:TpvDouble);
var Context:TpvVectorGraphicsRenderContext;
    Ancestor:TpvVectorGraphicsObject;
    AncestorMatrix:TpvVectorPathMatrix;
begin

 if not assigned(aObject) then begin
  exit;
 end;

 Update;

 // Every ancestor's transform still applies, otherwise the part would be drawn at the origin
 // instead of where it sits in the picture. The object's own transform is left to its Render.
 AncestorMatrix:=TpvVectorPathMatrix.CreateIdentity;
 Ancestor:=aObject.Parent;
 while assigned(Ancestor) do begin
  AncestorMatrix:=AncestorMatrix*Ancestor.GetMatrixAt(aTick,aTickFraction);
  Ancestor:=Ancestor.Parent;
 end;

 Context:=TpvVectorGraphicsRenderContext.Create(aRasterizer);
 Context.Tick:=aTick;
 Context.TickFraction:=aTickFraction;
 Context.ParentMatrix:=AncestorMatrix*aMatrix;
 Context.Override_:=aOverride;

 aObject.Render(Context);

end;

function TpvVectorGraphicsDocument.RenderObjectByID(const aID:TpvUTF8String;
                                                    const aRasterizer:TpvVectorGraphicsRasterizer;
                                                    const aMatrix:TpvVectorPathMatrix;
                                                    const aOverride:TpvVectorGraphicsRenderOverride;
                                                    const aTick:TpvInt64;
                                                    const aTickFraction:TpvDouble):boolean;
var Found:TpvVectorGraphicsObject;
begin
 Found:=FindByID(aID);
 result:=assigned(Found);
 if result then begin
  RenderObject(Found,aRasterizer,aMatrix,aOverride,aTick,aTickFraction);
 end;
end;

procedure TpvVectorGraphicsDocument.SetPreserveAspectRatio(const aValue:TpvUTF8String);
var Text:TpvUTF8String;
begin

 // The optional defer keyword only matters for referenced images and is therefore ignored, as is
 // the exact spelling, which is why everything is matched in lower case.
 Text:=TpvUTF8String(LowerCase(Trim(String(aValue))));
 if Length(Text)=0 then begin
  exit;
 end;

 fPreserveAspectRatioMeet:=Pos(TpvUTF8String('slice'),Text)=0;

 // none scales both axes on their own, so there is no leftover space to align.
 if Pos(TpvUTF8String('none'),Text)>0 then begin
  fPreserveAspectRatioUniform:=false;
  fPreserveAspectRatioAlignX:=0.0;
  fPreserveAspectRatioAlignY:=0.0;
  exit;
 end;

 fPreserveAspectRatioUniform:=true;

 // Min, mid and max become the fraction of the leftover space put before the content.
 if Pos(TpvUTF8String('xmin'),Text)>0 then begin
  fPreserveAspectRatioAlignX:=0.0;
 end else if Pos(TpvUTF8String('xmax'),Text)>0 then begin
  fPreserveAspectRatioAlignX:=1.0;
 end else begin
  fPreserveAspectRatioAlignX:=0.5;
 end;

 if Pos(TpvUTF8String('ymin'),Text)>0 then begin
  fPreserveAspectRatioAlignY:=0.0;
 end else if Pos(TpvUTF8String('ymax'),Text)>0 then begin
  fPreserveAspectRatioAlignY:=1.0;
 end else begin
  fPreserveAspectRatioAlignY:=0.5;
 end;

end;

function TpvVectorGraphicsDocument.GetViewBoxMatrix(const aTargetWidth,aTargetHeight:TpvDouble):TpvVectorPathMatrix;
begin
 result:=GetViewBoxMatrix(aTargetWidth,aTargetHeight,
                          fPreserveAspectRatioUniform,
                          fPreserveAspectRatioMeet,
                          fPreserveAspectRatioAlignX,
                          fPreserveAspectRatioAlignY);
end;

function TpvVectorGraphicsDocument.GetViewBoxMatrix(const aTargetWidth,aTargetHeight:TpvDouble;
                                                    const aUniform:boolean;
                                                    const aMeet:boolean;
                                                    const aAlignX,aAlignY:TpvDouble):TpvVectorPathMatrix;
var SourceWidth,SourceHeight,ScaleX,ScaleY,Scale:TpvDouble;
    SourceX,SourceY:TpvDouble;
begin

 // Without a view box the document's own size is the source rectangle.
 if fHasViewBox and not fViewBox.IsEmpty then begin
  SourceX:=fViewBox.x1;
  SourceY:=fViewBox.y1;
  SourceWidth:=fViewBox.Width;
  SourceHeight:=fViewBox.Height;
 end else begin
  SourceX:=0.0;
  SourceY:=0.0;
  SourceWidth:=Width;
  SourceHeight:=Height;
 end;

 if (SourceWidth<=1e-12) or (SourceHeight<=1e-12) then begin
  result:=TpvVectorPathMatrix.CreateIdentity;
  exit;
 end;

 ScaleX:=aTargetWidth/SourceWidth;
 ScaleY:=aTargetHeight/SourceHeight;

 // meet fits the whole view box inside the target, slice fills the target and crops.
 if aUniform then begin
  if aMeet then begin
   Scale:=Min(ScaleX,ScaleY);
  end else begin
   Scale:=Max(ScaleX,ScaleY);
  end;
  ScaleX:=Scale;
  ScaleY:=Scale;
 end;

 // Source origin to zero, then the scale, then the leftover space distributed by the alignment.
 result:=TpvVectorPathMatrix.CreateTranslate(-SourceX,-SourceY)*
         TpvVectorPathMatrix.CreateScale(ScaleX,ScaleY)*
         TpvVectorPathMatrix.CreateTranslate((aTargetWidth-(SourceWidth*ScaleX))*aAlignX,
                                             (aTargetHeight-(SourceHeight*ScaleY))*aAlignY);

end;

end.
