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
unit PasVulkan.VectorGraphics.SVG;
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
     PasDblStrUtils,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Collections,
     PasVulkan.XML,
     PasVulkan.Base64,
     PasVulkan.Image.PNG,
     PasVulkan.Image.JPEG,
     PasVulkan.VectorPath,
     PasVulkan.VectorGraphics.Rasterizer,
     PasVulkan.VectorGraphics.Paints,
     PasVulkan.VectorGraphics.Filters,
     PasVulkan.VectorGraphics;

type EpvVectorGraphicsSVG=class(Exception);

     { TpvVectorGraphicsSVGLoader }

     // Turns an SVG document tree into a vector graphics scene graph. Paint servers, clip paths,
     // masks, markers, symbols and patterns are collected in a first pass and resolved in a
     // second one, because a document may well reference them before it defines them.
     TpvVectorGraphicsSVGLoader=class
      public
       type { TState }
            // The inherited presentation state at one point in the tree. SVG inheritance is
            // simply this record being copied down into every child.
            TState=record
             public
              Opacity:TpvUInt8;
              CurrentColor:TpvVectorGraphicsColor;
              FillColor:TpvVectorGraphicsColor;
              FillPaintID:TpvUTF8String;
              FillHasPaint:boolean;
              FillOpacity:TpvUInt8;
              FillRule:TpvVectorPathFillRule;
              ClipRule:TpvVectorPathFillRule;
              StrokeColor:TpvVectorGraphicsColor;
              StrokePaintID:TpvUTF8String;
              StrokeHasPaint:boolean;
              StrokeOpacity:TpvUInt8;
              StrokeWidth:TpvDouble;
              StrokeWidthFixed:boolean;
              StrokeMiterLimit:TpvDouble;
              StrokeCapMode:TpvVectorPathStrokeCapMode;
              StrokeJoinMode:TpvVectorPathStrokeJoinMode;
              StrokeInnerJoinMode:TpvVectorPathStrokeInnerJoinMode;
              StrokeDashArray:TpvUTF8String;
              StrokeDashOffset:TpvDouble;
              Matrix:TpvVectorPathMatrix;
              FontFamily:TpvUTF8String;
              FontSize:TpvDouble;
              FontBold:boolean;
              FontItalic:boolean;
              TextAnchor:TpvInt32; // 0 start, 1 middle, 2 end
              LetterSpacing:TpvDouble;
              ClipPathID:TpvUTF8String;
              MaskID:TpvUTF8String;
              MarkerStartID:TpvUTF8String;
              MarkerMidID:TpvUTF8String;
              MarkerEndID:TpvUTF8String;
              FilterID:TpvUTF8String;
              Display:boolean;
              // Neither of these two inherits in CSS, so unlike everything else in this record
              // they are reset for every element rather than passed down.
              BlendMode:TpvVectorGraphicsBlendMode;
              Isolate:boolean;
             public
              class function CreateDefault:TState; static;
            end;
            PState=^TState;
            TGradientKind=
             (
              Linear=0,
              Radial
             );
            TUnits=
             (
              UserSpaceOnUse=0,
              ObjectBoundingBox
             );
            { TGradient }
            // A paint server as it stands in the file, with its coordinates still unresolved,
            // because whether a percentage means the viewport or the object's own box only
            // becomes clear once it is applied to a shape.
            TGradient=record
             public
              ID:TpvUTF8String;
              HRef:TpvUTF8String;
              Kind:TGradientKind;
              SpreadMethod:TpvVectorGraphicsSpreadMethod;
              Units:TUnits;
              Matrix:TpvVectorPathMatrix;
              Stops:TpvVectorGraphicsGradientStops;
              x1:TpvUTF8String;
              y1:TpvUTF8String;
              x2:TpvUTF8String;
              y2:TpvUTF8String;
              cx:TpvUTF8String;
              cy:TpvUTF8String;
              fx:TpvUTF8String;
              fy:TpvUTF8String;
              r:TpvUTF8String;
              HasKind:boolean;
              HasSpreadMethod:boolean;
              HasUnits:boolean;
              HasMatrix:boolean;
              HasStops:boolean;
            end;
            PGradient=^TGradient;
            TGradients=array of TGradient;
            { TPatternDefinition }
            TPatternDefinition=record
             public
              ID:TpvUTF8String;
              Group:TpvVectorGraphicsGroup;
              Units:TUnits;
              ContentUnits:TUnits;
              Matrix:TpvVectorPathMatrix;
              x:TpvDouble;
              y:TpvDouble;
              Width:TpvDouble;
              Height:TpvDouble;
            end;
            TPatternDefinitions=array of TPatternDefinition;
            { TMarkerDefinition }
            TMarkerDefinition=record
             public
              ID:TpvUTF8String;
              Group:TpvVectorGraphicsGroup;
              RefX:TpvDouble;
              RefY:TpvDouble;
              MarkerWidth:TpvDouble;
              MarkerHeight:TpvDouble;
              // True when orient is auto, so the marker turns with the path; otherwise Orient
              // holds the fixed angle in degrees.
              OrientAuto:boolean;
              // True when orient is auto-start-reverse, which flips the start marker.
              OrientAutoStartReverse:boolean;
              Orient:TpvDouble;
              // True when markerUnits is strokeWidth, which scales the marker with the line.
              ScaleWithStrokeWidth:boolean;
              HasViewBox:boolean;
              ViewBoxX:TpvDouble;
              ViewBoxY:TpvDouble;
              ViewBoxWidth:TpvDouble;
              ViewBoxHeight:TpvDouble;
              PreserveAspectRatioUniform:boolean;
              PreserveAspectRatioMeet:boolean;
              PreserveAspectRatioAlignX:TpvDouble;
              PreserveAspectRatioAlignY:TpvDouble;
            end;
            TMarkerDefinitions=array of TMarkerDefinition;
            { TStyleRule }
            // One CSS rule from a style element, kept as the plain selector text and the
            // declaration block, which is all the subset SVG files actually use needs.
            TStyleRule=record
             public
              Selector:TpvUTF8String;
              Declarations:TpvUTF8String;
            end;
            TStyleRules=array of TStyleRule;
      private
       fDocument:TpvVectorGraphicsDocument;
       fGradients:TGradients;
       fPatterns:TPatternDefinitions;
       fMarkers:TMarkerDefinitions;
       fStyleRules:TStyleRules;
       fViewportWidth:TpvDouble;
       fViewportHeight:TpvDouble;
       // Directory that relative image references are resolved against; empty means only data
       // URIs are accepted, which is what a loader fed from a stream can honour.
       fBaseDirectory:string;
       fRootViewBox:TpvVectorGraphicsBoundingBox;
       fHasRootViewBox:boolean;
       // Shapes whose paint still has to be resolved once every bounding box is known.
       fPendingFillPaints:TpvObjectGenericList<TpvVectorGraphicsObject>;
       fPendingFillPaintIDs:TpvGenericList<TpvUTF8String>;
       fPendingFillPaintMatrices:TpvGenericList<TpvVectorPathMatrix>;
       fPendingStrokePaints:TpvObjectGenericList<TpvVectorGraphicsObject>;
       fPendingStrokePaintIDs:TpvGenericList<TpvUTF8String>;
       fPendingStrokePaintMatrices:TpvGenericList<TpvVectorPathMatrix>;
       function ParseValue(const aValue:TpvUTF8String;const aBase:TpvDouble=1.0):TpvDouble;
       function ParseColor(const aValue:TpvUTF8String;const aCurrentColor:TpvVectorGraphicsColor;out aIsNone:boolean):TpvVectorGraphicsColor;
       function ParseTransform(const aValue:TpvUTF8String):TpvVectorPathMatrix;
       function ParsePaintReference(const aValue:TpvUTF8String):TpvUTF8String;
       // The mix-blend-mode keyword, where anything unknown counts as normal rather than as an
       // error, which is what a CSS property with an unsupported value does.
       function ParseBlendMode(const aValue:TpvUTF8String):TpvVectorGraphicsBlendMode;
       procedure ParseStyleDeclarations(const aDeclarations:TpvUTF8String;var aState:TState);
       procedure ParseCSS(const aText:TpvUTF8String);
       function GetMatchingDeclarations(const aTag:TpvXMLTag):TpvUTF8String;
       procedure ApplyPresentationAttributes(const aTag:TpvXMLTag;var aState:TState);
       // aBakedMatrix is the transform that was written into the geometry rather than left on the
       // object, which is what the shape elements do and what text and images do not. A paint
       // server given in user space is specified in the element's own coordinates, so it has to
       // be brought back out of the baked transform again.
       procedure ApplyStateToGraphic(const aGraphic:TpvVectorGraphicsGraphic;const aState:TState;
                                     const aBakedMatrix:TpvVectorPathMatrix);
       function FindGradient(const aID:TpvUTF8String):TpvSizeInt;
       function FindPattern(const aID:TpvUTF8String):TpvSizeInt;
       function FindMarker(const aID:TpvUTF8String):TpvSizeInt;
       // Places the start, mid and end markers of aState along the path of aGraphic, each one as
       // a reference to the marker group with its own transform.
       procedure ApplyMarkers(const aGraphic:TpvVectorGraphicsGraphic;
                              const aGroup:TpvVectorGraphicsGroup;
                              const aState:TState);
       // Walks the xlink:href chain so that a gradient that only carries stops, or only
       // coordinates, ends up complete.
       function ResolveGradient(const aIndex:TpvSizeInt;const aDepth:TpvInt32=0):TGradient;
       procedure CollectDefinitions(const aItem:TpvXMLItem;const aState:TState);
       procedure CollectGradient(const aTag:TpvXMLTag);
       procedure CollectPattern(const aTag:TpvXMLTag;const aState:TState);
       procedure CollectGroupDefinition(const aTag:TpvXMLTag;const aState:TState;const aKind:TpvUTF8String);
       function ProcessTag(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState):TpvVectorGraphicsObject;
       procedure ProcessChildren(const aItem:TpvXMLItem;const aGroup:TpvVectorGraphicsGroup;const aState:TState);
       function ProcessPath(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState):TpvVectorGraphicsObject;
       function ProcessRectangle(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState):TpvVectorGraphicsObject;
       function ProcessCircle(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState):TpvVectorGraphicsObject;
       function ProcessEllipse(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState):TpvVectorGraphicsObject;
       function ProcessLine(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState):TpvVectorGraphicsObject;
       function ProcessPolygon(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState;const aClosed:boolean):TpvVectorGraphicsObject;
       function ProcessImage(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState):TpvVectorGraphicsObject;
       // Decodes a data URI or, when aBaseDirectory is set, a relative file reference into an
       // image resource of the document. Returns nil when the data cannot be decoded.
       function LoadImageReference(const aReference:TpvUTF8String):TpvVectorGraphicsImageResource;
       function ProcessText(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState;
                            const aOffsetX:TpvDouble=0.0;const aOffsetY:TpvDouble=0.0;
                            const aLevel:TpvInt32=0):TpvVectorGraphicsObject;
       function ProcessGroup(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState):TpvVectorGraphicsObject;
       function ProcessUse(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState):TpvVectorGraphicsObject;
       // Wraps aObject in a group together with a reference to the clip path or mask, which is
       // how the scene graph expresses clipping.
       function ApplyClipAndMask(const aObject:TpvVectorGraphicsObject;
                                 const aGroup:TpvVectorGraphicsGroup;
                                 const aState:TState):TpvVectorGraphicsObject;
       procedure ResolvePendingPaints;
       procedure ResolvePaint(const aObject:TpvVectorGraphicsObject;const aID:TpvUTF8String;const aStroke:boolean;
                              const aBakedMatrix:TpvVectorPathMatrix);
       // An SMIL clock value in ticks, so "2s", "250ms", "1.5" or a full "01:02:33.5". Returns
       // false for "indefinite" and for anything it cannot make sense of.
       function ParseClockValue(const aValue:TpvUTF8String;out aTicks:TpvInt64):boolean;
       // Reads a filter element and everything below it into a filter of the document.
       procedure CollectFilter(const aTag:TpvXMLTag;const aState:TState);
       // The pixels behind an image reference as a buffer of their own, which is what feImage
       // needs, since the filter unit knows nothing about the image resources of the document.
       function LoadImageBuffer(const aReference:TpvUTF8String):TpvVectorGraphicsPixelBuffer;
       // Whether an element carries animation children, which decides whether its transform goes
       // into its geometry or stays on the object, since an animated length or rotation is given
       // in the element's own user space and only works out there.
       function HasAnimationChildren(const aTag:TpvXMLTag):boolean;
       // Finishes a shape element: without animations the transform goes into the geometry as
       // usual, with them it stays on the object instead.
       procedure FinishGraphic(const aTag:TpvXMLTag;const aGraphic:TpvVectorGraphicsGraphic;const aState:TState);
       // Turns the animation elements below aTag into timeline keys on aObject.
       procedure ProcessAnimations(const aTag:TpvXMLTag;const aObject:TpvVectorGraphicsObject;
                                   const aState:TState);
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       // Fills aDocument from the parsed XML tree. The document is cleared of nothing, so a
       // fresh one should be handed in.
       function Load(const aXML:TpvXML;const aDocument:TpvVectorGraphicsDocument):boolean;
      published
       property BaseDirectory:string read fBaseDirectory write fBaseDirectory;
     end;

     { TpvVectorGraphicsSVGWriter }

     // Writes a scene graph back out as SVG. Every shape becomes a path element, which keeps the
     // geometry exact at the price of losing the fact that something started out as a circle.
     TpvVectorGraphicsSVGWriter=class
      private
       fDocument:TpvVectorGraphicsDocument;
       fXML:TpvXML;
       fDefinitionsTag:TpvXMLTag;
       fPaintServerCounter:TpvInt32;
       function FormatValue(const aValue:TpvDouble):TpvRawByteString;
       function FormatColor(const aColor:TpvVectorGraphicsColor):TpvRawByteString;
       function FormatMatrix(const aMatrix:TpvVectorPathMatrix):TpvRawByteString;
       // The mix-blend-mode keyword for a mode, and an empty string for Normal, which is the
       // initial value and therefore never written out.
       function FormatBlendMode(const aBlendMode:TpvVectorGraphicsBlendMode):TpvRawByteString;
       function FormatPathData(const aPath:TpvVectorPath):TpvRawByteString;
       function EnsureDefinitionsTag:TpvXMLTag;
       // Writes a gradient or pattern into defs and returns the url(#id) reference for it, or an
       // empty string when the fill is a plain color or nothing at all.
       function WritePaintServer(const aFillData:TpvVectorGraphicsFillData):TpvRawByteString;
       procedure WriteStyle(const aTag:TpvXMLTag;const aGraphic:TpvVectorGraphicsGraphic);
       procedure WriteObject(const aObject:TpvVectorGraphicsObject;const aParentTag:TpvXMLTag);
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       // Builds the XML tree for aDocument into aXML, which the caller then writes out.
       function Save(const aDocument:TpvVectorGraphicsDocument;const aXML:TpvXML):boolean;
     end;

// Convenience entry points. Each returns a freshly created document that the caller owns, or nil
// when the input could not be parsed. aBaseDirectory is where relative image references are
// looked up; left empty, only data URIs are accepted.
function pvVectorGraphicsLoadSVGFromStream(const aStream:TStream;const aBaseDirectory:string=''):TpvVectorGraphicsDocument;

function pvVectorGraphicsLoadSVGFromFile(const aFileName:string):TpvVectorGraphicsDocument;

function pvVectorGraphicsLoadSVGFromString(const aText:TpvRawByteString):TpvVectorGraphicsDocument;

// Writes aDocument out as SVG. The caller keeps ownership of the document.
function pvVectorGraphicsSaveSVGToStream(const aDocument:TpvVectorGraphicsDocument;const aStream:TStream):boolean;

function pvVectorGraphicsSaveSVGToFile(const aDocument:TpvVectorGraphicsDocument;const aFileName:string):boolean;

// Looks up one of the 147 color keywords of SVG. Returns false when the name is not one of them.
function pvVectorGraphicsFindNamedColor(const aName:TpvUTF8String;out aColor:TpvVectorGraphicsColor):boolean;

implementation

type TNamedColor=record
      Name:TpvUTF8String;
      Color:TpvVectorGraphicsColor;
     end;

const NamedColors:array[0..146] of TNamedColor=
       ((Name:'aliceblue';           Color:$fff0f8ff),
        (Name:'antiquewhite';        Color:$fffaebd7),
        (Name:'aqua';                Color:$ff00ffff),
        (Name:'aquamarine';          Color:$ff7fffd4),
        (Name:'azure';               Color:$fff0ffff),
        (Name:'beige';               Color:$fff5f5dc),
        (Name:'bisque';              Color:$ffffe4c4),
        (Name:'black';               Color:$ff000000),
        (Name:'blanchedalmond';      Color:$ffffebcd),
        (Name:'blue';                Color:$ff0000ff),
        (Name:'blueviolet';          Color:$ff8a2be2),
        (Name:'brown';               Color:$ffa52a2a),
        (Name:'burlywood';           Color:$ffdeb887),
        (Name:'cadetblue';           Color:$ff5f9ea0),
        (Name:'chartreuse';          Color:$ff7fff00),
        (Name:'chocolate';           Color:$ffd2691e),
        (Name:'coral';               Color:$ffff7f50),
        (Name:'cornflowerblue';      Color:$ff6495ed),
        (Name:'cornsilk';            Color:$fffff8dc),
        (Name:'crimson';             Color:$ffdc143c),
        (Name:'cyan';                Color:$ff00ffff),
        (Name:'darkblue';            Color:$ff00008b),
        (Name:'darkcyan';            Color:$ff008b8b),
        (Name:'darkgoldenrod';       Color:$ffb8860b),
        (Name:'darkgray';            Color:$ffa9a9a9),
        (Name:'darkgreen';           Color:$ff006400),
        (Name:'darkgrey';            Color:$ffa9a9a9),
        (Name:'darkkhaki';           Color:$ffbdb76b),
        (Name:'darkmagenta';         Color:$ff8b008b),
        (Name:'darkolivegreen';      Color:$ff556b2f),
        (Name:'darkorange';          Color:$ffff8c00),
        (Name:'darkorchid';          Color:$ff9932cc),
        (Name:'darkred';             Color:$ff8b0000),
        (Name:'darksalmon';          Color:$ffe9967a),
        (Name:'darkseagreen';        Color:$ff8fbc8f),
        (Name:'darkslateblue';       Color:$ff483d8b),
        (Name:'darkslategray';       Color:$ff2f4f4f),
        (Name:'darkslategrey';       Color:$ff2f4f4f),
        (Name:'darkturquoise';       Color:$ff00ced1),
        (Name:'darkviolet';          Color:$ff9400d3),
        (Name:'deeppink';            Color:$ffff1493),
        (Name:'deepskyblue';         Color:$ff00bfff),
        (Name:'dimgray';             Color:$ff696969),
        (Name:'dimgrey';             Color:$ff696969),
        (Name:'dodgerblue';          Color:$ff1e90ff),
        (Name:'firebrick';           Color:$ffb22222),
        (Name:'floralwhite';         Color:$fffffaf0),
        (Name:'forestgreen';         Color:$ff228b22),
        (Name:'fuchsia';             Color:$ffff00ff),
        (Name:'gainsboro';           Color:$ffdcdcdc),
        (Name:'ghostwhite';          Color:$fff8f8ff),
        (Name:'gold';                Color:$ffffd700),
        (Name:'goldenrod';           Color:$ffdaa520),
        (Name:'gray';                Color:$ff808080),
        (Name:'green';               Color:$ff008000),
        (Name:'greenyellow';         Color:$ffadff2f),
        (Name:'grey';                Color:$ff808080),
        (Name:'honeydew';            Color:$fff0fff0),
        (Name:'hotpink';             Color:$ffff69b4),
        (Name:'indianred';           Color:$ffcd5c5c),
        (Name:'indigo';              Color:$ff4b0082),
        (Name:'ivory';               Color:$fffffff0),
        (Name:'khaki';               Color:$fff0e68c),
        (Name:'lavender';            Color:$ffe6e6fa),
        (Name:'lavenderblush';       Color:$fffff0f5),
        (Name:'lawngreen';           Color:$ff7cfc00),
        (Name:'lemonchiffon';        Color:$fffffacd),
        (Name:'lightblue';           Color:$ffadd8e6),
        (Name:'lightcoral';          Color:$fff08080),
        (Name:'lightcyan';           Color:$ffe0ffff),
        (Name:'lightgoldenrodyellow';Color:$fffafad2),
        (Name:'lightgray';           Color:$ffd3d3d3),
        (Name:'lightgreen';          Color:$ff90ee90),
        (Name:'lightgrey';           Color:$ffd3d3d3),
        (Name:'lightpink';           Color:$ffffb6c1),
        (Name:'lightsalmon';         Color:$ffffa07a),
        (Name:'lightseagreen';       Color:$ff20b2aa),
        (Name:'lightskyblue';        Color:$ff87cefa),
        (Name:'lightslategray';      Color:$ff778899),
        (Name:'lightslategrey';      Color:$ff778899),
        (Name:'lightsteelblue';      Color:$ffb0c4de),
        (Name:'lightyellow';         Color:$ffffffe0),
        (Name:'lime';                Color:$ff00ff00),
        (Name:'limegreen';           Color:$ff32cd32),
        (Name:'linen';               Color:$fffaf0e6),
        (Name:'magenta';             Color:$ffff00ff),
        (Name:'maroon';              Color:$ff800000),
        (Name:'mediumaquamarine';    Color:$ff66cdaa),
        (Name:'mediumblue';          Color:$ff0000cd),
        (Name:'mediumorchid';        Color:$ffba55d3),
        (Name:'mediumpurple';        Color:$ff9370db),
        (Name:'mediumseagreen';      Color:$ff3cb371),
        (Name:'mediumslateblue';     Color:$ff7b68ee),
        (Name:'mediumspringgreen';   Color:$ff00fa9a),
        (Name:'mediumturquoise';     Color:$ff48d1cc),
        (Name:'mediumvioletred';     Color:$ffc71585),
        (Name:'midnightblue';        Color:$ff191970),
        (Name:'mintcream';           Color:$fff5fffa),
        (Name:'mistyrose';           Color:$ffffe4e1),
        (Name:'moccasin';            Color:$ffffe4b5),
        (Name:'navajowhite';         Color:$ffffdead),
        (Name:'navy';                Color:$ff000080),
        (Name:'oldlace';             Color:$fffdf5e6),
        (Name:'olive';               Color:$ff808000),
        (Name:'olivedrab';           Color:$ff6b8e23),
        (Name:'orange';              Color:$ffffa500),
        (Name:'orangered';           Color:$ffff4500),
        (Name:'orchid';              Color:$ffda70d6),
        (Name:'palegoldenrod';       Color:$ffeee8aa),
        (Name:'palegreen';           Color:$ff98fb98),
        (Name:'paleturquoise';       Color:$ffafeeee),
        (Name:'palevioletred';       Color:$ffdb7093),
        (Name:'papayawhip';          Color:$ffffefd5),
        (Name:'peachpuff';           Color:$ffffdab9),
        (Name:'peru';                Color:$ffcd853f),
        (Name:'pink';                Color:$ffffc0cb),
        (Name:'plum';                Color:$ffdda0dd),
        (Name:'powderblue';          Color:$ffb0e0e6),
        (Name:'purple';              Color:$ff800080),
        (Name:'red';                 Color:$ffff0000),
        (Name:'rosybrown';           Color:$ffbc8f8f),
        (Name:'royalblue';           Color:$ff4169e1),
        (Name:'saddlebrown';         Color:$ff8b4513),
        (Name:'salmon';              Color:$fffa8072),
        (Name:'sandybrown';          Color:$fff4a460),
        (Name:'seagreen';            Color:$ff2e8b57),
        (Name:'seashell';            Color:$fffff5ee),
        (Name:'sienna';              Color:$ffa0522d),
        (Name:'silver';              Color:$ffc0c0c0),
        (Name:'skyblue';             Color:$ff87ceeb),
        (Name:'slateblue';           Color:$ff6a5acd),
        (Name:'slategray';           Color:$ff708090),
        (Name:'slategrey';           Color:$ff708090),
        (Name:'snow';                Color:$fffffafa),
        (Name:'springgreen';         Color:$ff00ff7f),
        (Name:'steelblue';           Color:$ff4682b4),
        (Name:'tan';                 Color:$ffd2b48c),
        (Name:'teal';                Color:$ff008080),
        (Name:'thistle';             Color:$ffd8bfd8),
        (Name:'tomato';              Color:$ffff6347),
        (Name:'turquoise';           Color:$ff40e0d0),
        (Name:'violet';              Color:$ffee82ee),
        (Name:'wheat';               Color:$fff5deb3),
        (Name:'white';               Color:$ffffffff),
        (Name:'whitesmoke';          Color:$fff5f5f5),
        (Name:'yellow';              Color:$ffffff00),
        (Name:'yellowgreen';         Color:$ff9acd32));

function pvVectorGraphicsFindNamedColor(const aName:TpvUTF8String;out aColor:TpvVectorGraphicsColor):boolean;
var LowIndex,HighIndex,MiddleIndex,Comparison:TpvSizeInt;
    Name:TpvUTF8String;
begin

 result:=false;
 aColor:=0;

 // SVG color names are case insensitive, the table holds them in lower case.
 Name:=LowerCase(aName);

 // The table is sorted, so a binary search answers in eight steps instead of a hundred and fifty.
 LowIndex:=0;
 HighIndex:=Length(NamedColors)-1;
 while LowIndex<=HighIndex do begin
  MiddleIndex:=(LowIndex+HighIndex) shr 1;
  Comparison:=CompareStr(String(NamedColors[MiddleIndex].Name),String(Name));
  if Comparison=0 then begin
   aColor:=NamedColors[MiddleIndex].Color;
   result:=true;
   exit;
  end else if Comparison<0 then begin
   LowIndex:=MiddleIndex+1;
  end else begin
   HighIndex:=MiddleIndex-1;
  end;
 end;

end;

// Splits off the text up to the first character out of aDelimiters, removes it from aText and
// returns it. The common string walk of all the little SVG value parsers.
function ParseToken(var aText:TpvUTF8String;const aDelimiters:TpvUTF8String;const aSkipRepeated:boolean=true):TpvUTF8String;
var Index,Len:TpvSizeInt;
begin

 result:='';
 Len:=Length(aText);

 // Forward to the first delimiter, which is where the token ends.
 Index:=1;
 while (Index<=Len) and (Pos(TpvUTF8String(aText[Index]),aDelimiters)=0) do begin
  inc(Index);
 end;
 result:=Copy(aText,1,Index-1);

 // Whitespace runs count as one separator, whereas a comma separated list has to keep its empty
 // entries, hence the two ways of stepping over what was found.
 if aSkipRepeated then begin
  while (Index<=Len) and (Pos(TpvUTF8String(aText[Index]),aDelimiters)>0) do begin
   inc(Index);
  end;
 end else if Index<=Len then begin
  inc(Index);
 end;

 Delete(aText,1,Index-1);

end;

function TrimText(const aText:TpvUTF8String):TpvUTF8String;
begin
 result:=TpvUTF8String(Trim(String(aText)));
end;

{ TpvVectorGraphicsSVGLoader.TState }

class function TpvVectorGraphicsSVGLoader.TState.CreateDefault:TState;
begin
 result.Opacity:=255;
 result.CurrentColor:=$ff000000;
 // SVG starts out filling black and not stroking at all.
 result.FillColor:=$ff000000;
 result.FillPaintID:='';
 result.FillHasPaint:=true;
 result.FillOpacity:=255;
 result.FillRule:=TpvVectorPathFillRule.NonZero;
 result.ClipRule:=TpvVectorPathFillRule.NonZero;
 result.StrokeColor:=$ff000000;
 result.StrokePaintID:='';
 result.StrokeHasPaint:=false;
 result.StrokeOpacity:=255;
 result.StrokeWidth:=1.0;
 result.StrokeWidthFixed:=false;
 result.StrokeMiterLimit:=4.0;
 result.StrokeCapMode:=TpvVectorPathStrokeCapMode.Butt;
 result.StrokeJoinMode:=TpvVectorPathStrokeJoinMode.Miter;
 result.StrokeInnerJoinMode:=TpvVectorPathStrokeInnerJoinMode.Miter;
 result.StrokeDashArray:='';
 result.StrokeDashOffset:=0.0;
 result.Matrix:=TpvVectorPathMatrix.CreateIdentity;
 result.FontFamily:='';
 result.FontSize:=12.0;
 result.FontBold:=false;
 result.FontItalic:=false;
 result.TextAnchor:=0;
 result.LetterSpacing:=0.0;
 result.ClipPathID:='';
 result.MaskID:='';
 result.MarkerStartID:='';
 result.MarkerMidID:='';
 result.MarkerEndID:='';
 result.FilterID:='';
 result.Display:=true;
 result.BlendMode:=TpvVectorGraphicsBlendMode.Normal;
 result.Isolate:=false;
end;

{ TpvVectorGraphicsSVGLoader }

constructor TpvVectorGraphicsSVGLoader.Create;
begin

 inherited Create;

 fDocument:=nil;

 // Gathered in the definitions pass that runs before the tree is built.
 fGradients:=nil;
 fPatterns:=nil;
 fMarkers:=nil;
 fStyleRules:=nil;

 // The viewport is what percentage values resolve against.
 fViewportWidth:=0.0;
 fViewportHeight:=0.0;
 fRootViewBox:=TpvVectorGraphicsBoundingBox.CreateEmpty;
 fHasRootViewBox:=false;

 fBaseDirectory:='';

 // A paint server can be referred to before it has been read, so references are collected here
 // and resolved once the whole document is in.
 fPendingFillPaints:=TpvObjectGenericList<TpvVectorGraphicsObject>.Create;
 fPendingFillPaints.OwnsObjects:=false;
 fPendingFillPaintIDs:=TpvGenericList<TpvUTF8String>.Create;
 fPendingFillPaintMatrices:=TpvGenericList<TpvVectorPathMatrix>.Create;
 fPendingStrokePaints:=TpvObjectGenericList<TpvVectorGraphicsObject>.Create;
 fPendingStrokePaints.OwnsObjects:=false;
 fPendingStrokePaintIDs:=TpvGenericList<TpvUTF8String>.Create;
 fPendingStrokePaintMatrices:=TpvGenericList<TpvVectorPathMatrix>.Create;

end;

destructor TpvVectorGraphicsSVGLoader.Destroy;
begin

 fGradients:=nil;
 fPatterns:=nil;
 fMarkers:=nil;
 fStyleRules:=nil;

 // The lists hold borrowed references, the objects belong to the document.
 FreeAndNil(fPendingFillPaints);
 FreeAndNil(fPendingFillPaintIDs);
 FreeAndNil(fPendingFillPaintMatrices);
 FreeAndNil(fPendingStrokePaints);
 FreeAndNil(fPendingStrokePaintIDs);
 FreeAndNil(fPendingStrokePaintMatrices);

 inherited Destroy;

end;

function TpvVectorGraphicsSVGLoader.ParseValue(const aValue:TpvUTF8String;const aBase:TpvDouble):TpvDouble;
var Text,Number:TpvUTF8String;
    Index,Len:TpvSizeInt;
    OK:TPasDblStrUtilsBoolean;
begin

 result:=0.0;

 Text:=TrimText(aValue);
 Len:=Length(Text);
 if Len=0 then begin
  exit;
 end;

 // Walk off the number: optional sign, digits, optional fraction, optional exponent. What is
 // left after it is the unit.
 Index:=1;
 if (Index<=Len) and (Text[Index] in ['-','+']) then begin
  inc(Index);
 end;
 while (Index<=Len) and (Text[Index] in ['0'..'9']) do begin
  inc(Index);
 end;
 if (Index<=Len) and (Text[Index] in ['.',',']) then begin
  inc(Index);
  while (Index<=Len) and (Text[Index] in ['0'..'9']) do begin
   inc(Index);
  end;
 end;
 if (Index<=Len) and (Text[Index] in ['e','E']) then begin
  inc(Index);
  if (Index<=Len) and (Text[Index] in ['-','+']) then begin
   inc(Index);
  end;
  while (Index<=Len) and (Text[Index] in ['0'..'9']) do begin
   inc(Index);
  end;
 end;

 // A decimal comma reaches here from locales that write one, so it is normalized away.
 Number:=Copy(Text,1,Index-1);
 for Len:=1 to Length(Number) do begin
  if Number[Len]=',' then begin
   Number[Len]:='.';
  end;
 end;
 if Length(Number)=0 then begin
  exit;
 end;

 result:=ConvertStringToDouble(TPasDblStrUtilsRawByteString(Number),rmNearest,@OK);
 if not OK then begin
  result:=0.0;
  exit;
 end;

 // The unit suffixes use the ninety pixels per inch that SVG 1.1 defines.
 Text:=TrimText(Copy(Text,Index,Length(Text)));
 if Length(Text)>0 then begin
  if Text[1]='%' then begin
   result:=result*0.01*aBase;
  end else if Copy(Text,1,2)='px' then begin
  end else if Copy(Text,1,2)='pt' then begin
   result:=result*1.25;
  end else if Copy(Text,1,2)='pc' then begin
   result:=result*15.0;
  end else if Copy(Text,1,2)='mm' then begin
   result:=result*3.543307;
  end else if Copy(Text,1,2)='cm' then begin
   result:=result*35.43307;
  end else if Copy(Text,1,2)='in' then begin
   result:=result*90.0;
  end else if Copy(Text,1,2)='em' then begin
   // Relative to the font size, which this parser does not track per element, so the usual
   // sixteen pixel default stands in.
   result:=result*16.0;
  end else if Copy(Text,1,2)='ex' then begin
   result:=result*8.0;
  end;

 end;

end;

function TpvVectorGraphicsSVGLoader.ParseColor(const aValue:TpvUTF8String;const aCurrentColor:TpvVectorGraphicsColor;out aIsNone:boolean):TpvVectorGraphicsColor;
var Text,Component:TpvUTF8String;
    Values:array[0..3] of TpvDouble;
    Count,Index:TpvSizeInt;
    Digits:TpvUInt32;
    r,g,b,a:TpvUInt32;
    HasPercent:boolean;
begin

 aIsNone:=false;
 result:=$ff000000;

 Text:=TpvUTF8String(LowerCase(Trim(String(aValue))));

 // The keywords that do not name a color at all.
 if Length(Text)=0 then begin
  aIsNone:=true;
  exit;
 end;

 if (Text='none') or (Text='transparent') then begin
  aIsNone:=true;
  result:=0;
  exit;
 end;

 if Text='currentcolor' then begin
  result:=aCurrentColor;
  exit;
 end;

 // Hexadecimal, in the three, four, six or eight digit form.
 if Text[1]='#' then begin

  Delete(Text,1,1);
  Digits:=0;
  for Index:=1 to Length(Text) do begin
   case Text[Index] of
    '0'..'9':begin
     Digits:=(Digits shl 4) or TpvUInt32(Ord(Text[Index])-Ord('0'));
    end;
    'a'..'f':begin
     Digits:=(Digits shl 4) or TpvUInt32((Ord(Text[Index])-Ord('a'))+10);
    end;
    else begin
     // Anything else ends the hex value, which keeps a stray suffix from poisoning it.
     Text:=Copy(Text,1,Index-1);
     break;
    end;
   end;
  end;
  case Length(Text) of
   3:begin
    r:=(Digits shr 8) and $f;
    g:=(Digits shr 4) and $f;
    b:=Digits and $f;
    result:=TpvUInt32($ff000000) or ((r or (r shl 4)) shl 16) or ((g or (g shl 4)) shl 8) or (b or (b shl 4));
   end;
   4:begin
    a:=(Digits shr 12) and $f;
    r:=(Digits shr 8) and $f;
    g:=(Digits shr 4) and $f;
    b:=Digits and $f;
    result:=((a or (a shl 4)) shl 24) or ((r or (r shl 4)) shl 16) or ((g or (g shl 4)) shl 8) or (b or (b shl 4));
   end;
   6:begin
    result:=Digits or TpvUInt32($ff000000);
   end;
   8:begin
    // Eight digits are red green blue alpha in CSS color 4, not alpha first.
    result:=((Digits and $ff) shl 24) or ((Digits shr 8) and TpvUInt32($00ffffff));
   end;
   else begin
    result:=TpvUInt32($ff000000);
   end;
  end;

  exit;

 end;

 // The functional form, with either plain numbers or percentages.
 if (Copy(Text,1,4)='rgb(') or (Copy(Text,1,5)='rgba(') then begin

  if Copy(Text,1,5)='rgba(' then begin
   Delete(Text,1,5);
  end else begin
   Delete(Text,1,4);
  end;
  Text:=ParseToken(Text,')',false);

  Count:=0;
  Values[0]:=0.0;
  Values[1]:=0.0;
  Values[2]:=0.0;
  Values[3]:=1.0;

  while (Length(Text)>0) and (Count<4) do begin
   Component:=TrimText(ParseToken(Text,#0#1#2#3#4#5#6#7#8#9#10#11#12#13#14#15#16#17#18#19#20#21#22#23#24#25#26#27#28#29#30#31' ,/'));
   if Length(Component)=0 then begin
    break;
   end;
   HasPercent:=Pos(TpvUTF8String('%'),Component)>0;
   Values[Count]:=ParseValue(Component,1.0);
   if HasPercent then begin
    // A percentage component means a fraction of the full range, which ParseValue already
    // turned into a hundredth, so it only has to be scaled up.
    if Count<3 then begin
     Values[Count]:=Values[Count]*255.0;
    end;
   end;
   inc(Count);
  end;

  if Count>=3 then begin
   r:=Min(Max(round(Values[0]),0),255);
   g:=Min(Max(round(Values[1]),0),255);
   b:=Min(Max(round(Values[2]),0),255);
   if Count>=4 then begin
    a:=Min(Max(round(Values[3]*255.0),0),255);
   end else begin
    a:=255;
   end;
   result:=(a shl 24) or (r shl 16) or (g shl 8) or b;
  end else begin
   aIsNone:=true;
   result:=0;
  end;

  exit;

 end;

 // Anything left has to be one of the color keywords. An unknown one is not a reason to drop
 // the element, so it falls back to black, which is the initial value of fill anyway.
 if not pvVectorGraphicsFindNamedColor(Text,result) then begin
  result:=$ff000000;
 end;

end;

function TpvVectorGraphicsSVGLoader.ParseTransform(const aValue:TpvUTF8String):TpvVectorPathMatrix;
const DegreesToRadians=PI/180.0;
var Text,Command,Parameters:TpvUTF8String;
    Values:array[0..5] of TpvDouble;
    Count:TpvSizeInt;
    Matrix:TpvVectorPathMatrix;
 function NextValue:TpvDouble;
 var Token:TpvUTF8String;
 begin
  Token:=TrimText(ParseToken(Parameters,#0#1#2#3#4#5#6#7#8#9#10#11#12#13#14#15#16#17#18#19#20#21#22#23#24#25#26#27#28#29#30#31' ,'));
  result:=ParseValue(Token);
  Parameters:=TrimText(Parameters);
 end;
begin

 result:=TpvVectorPathMatrix.CreateIdentity;

 Text:=TrimText(aValue);
 while Length(Text)>0 do begin

  // One entry of the list: the name up to the opening bracket, the arguments up to the closing
  // one, and whatever separates it from the next entry.
  Command:=TpvUTF8String(LowerCase(Trim(String(ParseToken(Text,'(',false)))));
  Parameters:=TrimText(ParseToken(Text,')',false));
  Text:=TrimText(Text);
  while (Length(Text)>0) and (Text[1] in [';',',']) do begin
   Delete(Text,1,1);
   Text:=TrimText(Text);
  end;

  if Length(Command)=0 then begin
   break;
  end;

  // Six is as many as the longest form takes.
  Count:=0;
  while (Length(Parameters)>0) and (Count<6) do begin
   Values[Count]:=NextValue;
   inc(Count);
  end;

  // Each transform in the list is applied before everything to its left, which is the order the
  // SVG transform attribute prescribes.
  if Command='matrix' then begin
   if Count>=6 then begin
    Matrix:=TpvVectorPathMatrix.Create(Values[0],Values[1],Values[2],Values[3],Values[4],Values[5]);
    result:=Matrix*result;
   end;
  end else if Command='translate' then begin
   if Count>=2 then begin
    result:=TpvVectorPathMatrix.CreateTranslate(Values[0],Values[1])*result;
   end else if Count=1 then begin
    result:=TpvVectorPathMatrix.CreateTranslate(Values[0],0.0)*result;
   end;
  end else if Command='scale' then begin
   if Count>=2 then begin
    result:=TpvVectorPathMatrix.CreateScale(Values[0],Values[1])*result;
   end else if Count=1 then begin
    result:=TpvVectorPathMatrix.CreateScale(Values[0],Values[0])*result;
   end;
  end else if Command='rotate' then begin
   if Count>=3 then begin
    result:=TpvVectorPathMatrix.CreateRotate(Values[0]*DegreesToRadians,Values[1],Values[2])*result;
   end else if Count>=1 then begin
    result:=TpvVectorPathMatrix.CreateRotate(Values[0]*DegreesToRadians)*result;
   end;
  end else if Command='skewx' then begin
   if Count>=1 then begin
    result:=TpvVectorPathMatrix.CreateSkewX(Values[0]*DegreesToRadians)*result;
   end;
  end else if Command='skewy' then begin
   if Count>=1 then begin
    result:=TpvVectorPathMatrix.CreateSkewY(Values[0]*DegreesToRadians)*result;
   end;
  end;

 end;

end;

function TpvVectorGraphicsSVGLoader.ParsePaintReference(const aValue:TpvUTF8String):TpvUTF8String;
var Text:TpvUTF8String;
    Index:TpvSizeInt;
begin

 result:='';

 // Only a local reference of the form url(#name) is understood, anything else yields nothing.
 Text:=TrimText(aValue);
 if Copy(LowerCase(String(Text)),1,4)<>'url(' then begin
  exit;
 end;

 Delete(Text,1,4);
 Text:=TrimText(Text);

 // The name may be quoted.
 while (Length(Text)>0) and (Text[1] in ['''','"']) do begin
  Delete(Text,1,1);
  Text:=TrimText(Text);
 end;

 if (Length(Text)>0) and (Text[1]='#') then begin
  Delete(Text,1,1);
  Text:=TrimText(Text);
 end;

 // Everything from the closing bracket onwards is not part of the name, and neither is a closing
 // quote that the opening one left behind.
 Index:=Pos(TpvUTF8String(')'),Text);
 if Index>0 then begin
  Text:=Copy(Text,1,Index-1);
 end;
 Text:=TrimText(Text);
 while (Length(Text)>0) and (Text[Length(Text)] in ['''','"']) do begin
  Delete(Text,Length(Text),1);
 end;

 result:=TrimText(Text);

end;

function TpvVectorGraphicsSVGLoader.ParseBlendMode(const aValue:TpvUTF8String):TpvVectorGraphicsBlendMode;
var Text:string;
begin

 Text:=LowerCase(Trim(String(aValue)));

 // The twelve separable modes first, in the order the specification lists them.
 if Text='multiply' then begin
  result:=TpvVectorGraphicsBlendMode.Multiply;
 end else if Text='screen' then begin
  result:=TpvVectorGraphicsBlendMode.Screen;
 end else if Text='overlay' then begin
  result:=TpvVectorGraphicsBlendMode.Overlay;
 end else if Text='darken' then begin
  result:=TpvVectorGraphicsBlendMode.Darken;
 end else if Text='lighten' then begin
  result:=TpvVectorGraphicsBlendMode.Lighten;
 end else if Text='color-dodge' then begin
  result:=TpvVectorGraphicsBlendMode.ColorDodge;
 end else if Text='color-burn' then begin
  result:=TpvVectorGraphicsBlendMode.ColorBurn;
 end else if Text='hard-light' then begin
  result:=TpvVectorGraphicsBlendMode.HardLight;
 end else if Text='soft-light' then begin
  result:=TpvVectorGraphicsBlendMode.SoftLight;
 end else if Text='difference' then begin
  result:=TpvVectorGraphicsBlendMode.Difference;
 end else if Text='exclusion' then begin
  result:=TpvVectorGraphicsBlendMode.Exclusion;

 // And then the four that need all three channels at once.
 end else if Text='hue' then begin
  result:=TpvVectorGraphicsBlendMode.Hue;
 end else if Text='saturation' then begin
  result:=TpvVectorGraphicsBlendMode.Saturation;
 end else if Text='color' then begin
  result:=TpvVectorGraphicsBlendMode.Color;
 end else if Text='luminosity' then begin
  result:=TpvVectorGraphicsBlendMode.Luminosity;
 end else begin
  result:=TpvVectorGraphicsBlendMode.Normal;
 end;

end;

procedure TpvVectorGraphicsSVGLoader.ParseStyleDeclarations(const aDeclarations:TpvUTF8String;var aState:TState);
var Text,Declaration,Name,Value,Reference:TpvUTF8String;
    IsNone:boolean;
    Color:TpvVectorGraphicsColor;
begin

 Text:=aDeclarations;

 // One name and value pair at a time, each of which simply overwrites that part of the state;
 // inherit means leaving whatever came down from the parent alone.
 while Length(Text)>0 do begin

  Declaration:=TrimText(ParseToken(Text,';',false));
  if Length(Declaration)=0 then begin
   continue;
  end;

  Name:=TpvUTF8String(LowerCase(Trim(String(ParseToken(Declaration,':',false)))));
  Value:=TrimText(Declaration);
  if (Length(Name)=0) or (Length(Value)=0) or (Value='inherit') then begin
   continue;
  end;

  if Name='fill' then begin
   Reference:=ParsePaintReference(Value);
   if Length(Reference)>0 then begin
    aState.FillPaintID:=Reference;
    aState.FillHasPaint:=true;
    aState.FillColor:=$ffffffff;
   end else begin
    aState.FillPaintID:='';
    aState.FillColor:=ParseColor(Value,aState.CurrentColor,IsNone);
    aState.FillHasPaint:=not IsNone;
   end;
  end else if Name='stroke' then begin
   Reference:=ParsePaintReference(Value);
   if Length(Reference)>0 then begin
    aState.StrokePaintID:=Reference;
    aState.StrokeHasPaint:=true;
    aState.StrokeColor:=$ffffffff;
   end else begin
    aState.StrokePaintID:='';
    aState.StrokeColor:=ParseColor(Value,aState.CurrentColor,IsNone);
    aState.StrokeHasPaint:=not IsNone;
   end;
  end else if Name='color' then begin
   Color:=ParseColor(Value,aState.CurrentColor,IsNone);
   if not IsNone then begin
    aState.CurrentColor:=Color;
   end;
  end else if Name='opacity' then begin
   aState.Opacity:=Min(Max(round(ParseValue(Value,1.0)*255.0),0),255);
  end else if Name='fill-opacity' then begin
   aState.FillOpacity:=Min(Max(round(ParseValue(Value,1.0)*255.0),0),255);
  end else if Name='stroke-opacity' then begin
   aState.StrokeOpacity:=Min(Max(round(ParseValue(Value,1.0)*255.0),0),255);
  end else if Name='fill-rule' then begin
   if LowerCase(String(Value))='evenodd' then begin
    aState.FillRule:=TpvVectorPathFillRule.EvenOdd;
   end else if LowerCase(String(Value))='nonzero' then begin
    aState.FillRule:=TpvVectorPathFillRule.NonZero;
   end;
  end else if Name='clip-rule' then begin
   if LowerCase(String(Value))='evenodd' then begin
    aState.ClipRule:=TpvVectorPathFillRule.EvenOdd;
   end else if LowerCase(String(Value))='nonzero' then begin
    aState.ClipRule:=TpvVectorPathFillRule.NonZero;
   end;
  end else if Name='stroke-width' then begin
   aState.StrokeWidth:=ParseValue(Value,Max(fViewportWidth,fViewportHeight));
  end else if Name='stroke-miterlimit' then begin
   aState.StrokeMiterLimit:=Max(1.0,ParseValue(Value));
  end else if Name='stroke-linecap' then begin
   Value:=TpvUTF8String(LowerCase(String(Value)));
   if Value='round' then begin
    aState.StrokeCapMode:=TpvVectorPathStrokeCapMode.Round;
   end else if Value='square' then begin
    aState.StrokeCapMode:=TpvVectorPathStrokeCapMode.Square;
   end else if Value='butt' then begin
    aState.StrokeCapMode:=TpvVectorPathStrokeCapMode.Butt;
   end;
  end else if Name='stroke-linejoin' then begin
   Value:=TpvUTF8String(LowerCase(String(Value)));
   if Value='round' then begin
    aState.StrokeJoinMode:=TpvVectorPathStrokeJoinMode.Round;
    aState.StrokeInnerJoinMode:=TpvVectorPathStrokeInnerJoinMode.Round;
   end else if Value='bevel' then begin
    aState.StrokeJoinMode:=TpvVectorPathStrokeJoinMode.Bevel;
    aState.StrokeInnerJoinMode:=TpvVectorPathStrokeInnerJoinMode.Bevel;
   end else if Value='miter' then begin
    aState.StrokeJoinMode:=TpvVectorPathStrokeJoinMode.Miter;
    aState.StrokeInnerJoinMode:=TpvVectorPathStrokeInnerJoinMode.Miter;
   end else if Value='miter-clip' then begin
    aState.StrokeJoinMode:=TpvVectorPathStrokeJoinMode.MiterRevert;
   end else if Value='arcs' then begin
    aState.StrokeJoinMode:=TpvVectorPathStrokeJoinMode.MiterRound;
   end;
  end else if Name='stroke-dasharray' then begin
   if LowerCase(String(Value))='none' then begin
    aState.StrokeDashArray:='';
   end else begin
    aState.StrokeDashArray:=Value;
   end;
  end else if Name='stroke-dashoffset' then begin
   aState.StrokeDashOffset:=ParseValue(Value);
  end else if Name='vector-effect' then begin
   aState.StrokeWidthFixed:=LowerCase(String(Value))='non-scaling-stroke';
  end else if Name='font-family' then begin
   aState.FontFamily:=Value;
  end else if Name='font-size' then begin
   aState.FontSize:=ParseValue(Value,fViewportHeight);
  end else if Name='font-weight' then begin
   Value:=TpvUTF8String(LowerCase(String(Value)));
   aState.FontBold:=(Value='bold') or (Value='bolder') or (ParseValue(Value)>=600.0);
  end else if Name='font-style' then begin
   Value:=TpvUTF8String(LowerCase(String(Value)));
   aState.FontItalic:=(Value='italic') or (Value='oblique');
  end else if Name='letter-spacing' then begin
   aState.LetterSpacing:=ParseValue(Value);
  end else if Name='text-anchor' then begin
   Value:=TpvUTF8String(LowerCase(String(Value)));
   if Value='middle' then begin
    aState.TextAnchor:=1;
   end else if Value='end' then begin
    aState.TextAnchor:=2;
   end else begin
    aState.TextAnchor:=0;
   end;
  end else if Name='clip-path' then begin
   aState.ClipPathID:=ParsePaintReference(Value);
  end else if Name='mask' then begin
   aState.MaskID:=ParsePaintReference(Value);
  end else if Name='filter' then begin
   aState.FilterID:=ParsePaintReference(Value);
  end else if Name='marker-start' then begin
   aState.MarkerStartID:=ParsePaintReference(Value);
  end else if Name='marker-mid' then begin
   aState.MarkerMidID:=ParsePaintReference(Value);
  end else if Name='marker-end' then begin
   aState.MarkerEndID:=ParsePaintReference(Value);
  end else if Name='display' then begin
   aState.Display:=LowerCase(String(Value))<>'none';
  end else if Name='visibility' then begin
   Value:=TpvUTF8String(LowerCase(String(Value)));
   aState.Display:=(Value<>'hidden') and (Value<>'collapse');
  end else if Name='mix-blend-mode' then begin
   aState.BlendMode:=ParseBlendMode(Value);
  end else if Name='isolation' then begin
   aState.Isolate:=LowerCase(Trim(String(Value)))='isolate';
  end;

 end;

end;

procedure TpvVectorGraphicsSVGLoader.ParseCSS(const aText:TpvUTF8String);
var Text,Selector,Declarations,SingleSelector:TpvUTF8String;
    Index,Count:TpvSizeInt;
begin

 Text:=aText;

 // Strip comments first, so that a selector inside one cannot be taken for a real rule.
 repeat
  Index:=Pos(TpvUTF8String('/*'),Text);
  if Index=0 then begin
   break;
  end;
  Count:=Pos(TpvUTF8String('*/'),Copy(Text,Index+2,Length(Text)));
  if Count=0 then begin
   Text:=Copy(Text,1,Index-1);
   break;
  end;
  Delete(Text,Index,Count+3);
 until false;

 // Then the rules, each one a selector up to the opening brace and a body up to the closing one.
 while Length(Text)>0 do begin

  Selector:=TrimText(ParseToken(Text,'{',false));
  if Length(Text)=0 then begin
   break;
  end;

  Declarations:=TrimText(ParseToken(Text,'}',false));
  if Length(Selector)=0 then begin
   continue;
  end;

  // A rule may list several selectors, each of which gets its own entry.
  while Length(Selector)>0 do begin
   SingleSelector:=TrimText(ParseToken(Selector,',',false));
   if Length(SingleSelector)>0 then begin
    Count:=Length(fStyleRules);
    SetLength(fStyleRules,Count+1);
    fStyleRules[Count].Selector:=SingleSelector;
    fStyleRules[Count].Declarations:=Declarations;
   end;
  end;

 end;

end;

function TpvVectorGraphicsSVGLoader.GetMatchingDeclarations(const aTag:TpvXMLTag):TpvUTF8String;
var Index:TpvSizeInt;
    Selector,ClassNames,SingleClass,TagID,TagName:TpvUTF8String;
begin

 result:='';
 if Length(fStyleRules)=0 then begin
  exit;
 end;

 TagName:=TpvUTF8String(LowerCase(String(aTag.Name)));
 TagID:=TpvUTF8String(aTag.GetParameter('id',''));
 ClassNames:=TpvUTF8String(aTag.GetParameter('class',''));

 // Only the three selector shapes that SVG files really use get matched: type, class and id.
 // Later rules win over earlier ones, which the concatenation order takes care of.
 for Index:=0 to Length(fStyleRules)-1 do begin

  Selector:=TrimText(fStyleRules[Index].Selector);
  if Length(Selector)=0 then begin
   continue;
  end;

  if Selector[1]='.' then begin
   SingleClass:=Copy(Selector,2,Length(Selector));
   if Length(SingleClass)>0 then begin
    Selector:=ClassNames;
    while Length(Selector)>0 do begin
     if TrimText(ParseToken(Selector,#9#10#13' '))=SingleClass then begin
      result:=result+fStyleRules[Index].Declarations+';';
      break;
     end;
    end;
   end;
  end else if Selector[1]='#' then begin
   if Copy(Selector,2,Length(Selector))=TagID then begin
    result:=result+fStyleRules[Index].Declarations+';';
   end;
  end else if Selector='*' then begin
   result:=result+fStyleRules[Index].Declarations+';';
  end else if TpvUTF8String(LowerCase(String(Selector)))=TagName then begin
   result:=result+fStyleRules[Index].Declarations+';';
  end;

 end;

end;

procedure TpvVectorGraphicsSVGLoader.ApplyPresentationAttributes(const aTag:TpvXMLTag;var aState:TState);
var Declarations,Value:TpvUTF8String;
 procedure Collect(const aName:TpvUTF8String);
 var Attribute:TpvUTF8String;
 begin
  Attribute:=TpvUTF8String(aTag.GetParameter(TpvRawByteString(aName),''));
  if Length(Attribute)>0 then begin
   Declarations:=Declarations+aName+':'+Attribute+';';
  end;
 end;
begin

 Declarations:='';

 // The order matters: CSS from a style element first, then presentation attributes, then the
 // style attribute, which is what the SVG cascade prescribes for the subset supported here.
 Declarations:=GetMatchingDeclarations(aTag);

 Collect('color');
 Collect('fill');
 Collect('fill-opacity');
 Collect('fill-rule');
 Collect('clip-rule');
 Collect('stroke');
 Collect('stroke-opacity');
 Collect('stroke-width');
 Collect('stroke-miterlimit');
 Collect('stroke-linecap');
 Collect('stroke-linejoin');
 Collect('stroke-dasharray');
 Collect('stroke-dashoffset');
 Collect('vector-effect');
 Collect('opacity');
 Collect('font-family');
 Collect('font-size');
 Collect('font-weight');
 Collect('font-style');
 Collect('letter-spacing');
 Collect('text-anchor');
 Collect('clip-path');
 Collect('mask');
 Collect('filter');
 Collect('marker-start');
 Collect('marker-mid');
 Collect('marker-end');
 Collect('display');
 Collect('visibility');
 Collect('mix-blend-mode');
 Collect('isolation');

 Value:=TpvUTF8String(aTag.GetParameter('style',''));
 if Length(Value)>0 then begin
  Declarations:=Declarations+Value+';';
 end;

 if Length(Declarations)>0 then begin
  ParseStyleDeclarations(Declarations,aState);
 end;

 // The transform is not a style declaration and therefore handled on its own.
 Value:=TpvUTF8String(aTag.GetParameter('transform',''));
 if Length(Value)>0 then begin
  aState.Matrix:=ParseTransform(Value)*aState.Matrix;
 end;

end;

procedure TpvVectorGraphicsSVGLoader.ApplyStateToGraphic(const aGraphic:TpvVectorGraphicsGraphic;const aState:TState;
                                                         const aBakedMatrix:TpvVectorPathMatrix);
var Style:TpvVectorGraphicsStyle;
    DashText,Token:TpvUTF8String;
    DashCount:TpvSizeInt;
    DashValue,BakedScale:TpvDouble;
begin

 Style:=aGraphic.Style;

 // A paint server reference becomes a plain color for now and is replaced later, once the
 // bounding boxes that object bounding box units need are known.
 if aState.FillHasPaint then begin
  Style.FillData.Mode:=TpvVectorGraphicsFillMode.Color;
  Style.FillData.Color:=pvVectorGraphicsModulateColor(aState.FillColor,aState.FillOpacity);
 end else begin
  Style.FillData.Mode:=TpvVectorGraphicsFillMode.None;
 end;
 Style.FillData.FillRule:=aState.FillRule;
 Style.FillData.ClipRule:=aState.ClipRule;

 if aState.StrokeHasPaint then begin
  Style.StrokeFillData.Mode:=TpvVectorGraphicsFillMode.Color;
  Style.StrokeFillData.Color:=pvVectorGraphicsModulateColor(aState.StrokeColor,aState.StrokeOpacity);
 end else begin
  Style.StrokeFillData.Mode:=TpvVectorGraphicsFillMode.None;
 end;

 // The stroke outline is always filled with the non-zero rule, whatever the shape itself uses.
 Style.StrokeFillData.FillRule:=TpvVectorPathFillRule.NonZero;
 Style.StrokeWidth:=aState.StrokeWidth;
 Style.StrokeWidthFixed:=aState.StrokeWidthFixed;
 Style.StrokeMiterLimit:=aState.StrokeMiterLimit;
 Style.StrokeCapMode:=aState.StrokeCapMode;
 Style.StrokeJoinMode:=aState.StrokeJoinMode;
 Style.StrokeInnerJoinMode:=aState.StrokeInnerJoinMode;
 Style.StrokeDashOffset:=aState.StrokeDashOffset;

 Style.StrokeDashArray:=nil;
 if Length(aState.StrokeDashArray)>0 then begin

  DashText:=aState.StrokeDashArray;
  DashCount:=0;

  while Length(DashText)>0 do begin

   Token:=TrimText(ParseToken(DashText,#9#10#13' ,'));
   if Length(Token)=0 then begin
    continue;
   end;

   // A negative dash is an error in SVG and turns the whole list off.
   DashValue:=ParseValue(Token,Max(fViewportWidth,fViewportHeight));
   if DashValue<0.0 then begin
    DashCount:=0;
    break;
   end;

   SetLength(Style.StrokeDashArray,DashCount+1);
   Style.StrokeDashArray[DashCount]:=DashValue;
   inc(DashCount);

  end;

  SetLength(Style.StrokeDashArray,DashCount);

 end;

 // Every stroke length above is given in the element's own user space, but the element transform
 // went into the geometry on loading, so they have to be scaled along with it. A non-scaling
 // stroke is the exception: it is in device units and is built after the transform anyway.
 if not Style.StrokeWidthFixed then begin
  BakedScale:=aBakedMatrix.ExpansionFactor;
  if BakedScale>1e-12 then begin
   Style.StrokeWidth:=Style.StrokeWidth*BakedScale;
   Style.StrokeDashOffset:=Style.StrokeDashOffset*BakedScale;
   for DashCount:=0 to Length(Style.StrokeDashArray)-1 do begin
    Style.StrokeDashArray[DashCount]:=Style.StrokeDashArray[DashCount]*BakedScale;
   end;
  end;
 end;

 // Fill and stroke of one shape overlap along the outline, so a semi transparent shape has to
 // be composited as a whole to avoid a darker seam there.
 Style.OffImage:=(aState.Opacity<>255) and
                 (Style.FillData.Mode<>TpvVectorGraphicsFillMode.None) and
                 (Style.StrokeFillData.Mode<>TpvVectorGraphicsFillMode.None);

 aGraphic.Style:=Style;
 aGraphic.Alpha:=aState.Opacity;
 aGraphic.SimpleMatrix:=TpvVectorGraphicsSimpleMatrix.CreateDefault;
 aGraphic.Visible:=aState.Display;

 // Isolation is left out, since a shape has no children whose blend modes it could keep inside.
 aGraphic.BlendMode:=aState.BlendMode;

 // The paint servers can only be resolved once every bounding box is known, so the shapes that
 // want one get queued here.
 if Length(aState.FillPaintID)>0 then begin
  fPendingFillPaints.Add(aGraphic);
  fPendingFillPaintIDs.Add(aState.FillPaintID);
  fPendingFillPaintMatrices.Add(aBakedMatrix);
 end;
 if Length(aState.StrokePaintID)>0 then begin
  fPendingStrokePaints.Add(aGraphic);
  fPendingStrokePaintIDs.Add(aState.StrokePaintID);
  fPendingStrokePaintMatrices.Add(aBakedMatrix);
 end;

end;

function TpvVectorGraphicsSVGLoader.FindGradient(const aID:TpvUTF8String):TpvSizeInt;
var Index:TpvSizeInt;
begin

 // A linear walk, since a document with enough paint servers for this to matter does not exist
 // in practice.
 result:=-1;
 for Index:=0 to Length(fGradients)-1 do begin
  if fGradients[Index].ID=aID then begin
   result:=Index;
   exit;
  end;
 end;

end;

function TpvVectorGraphicsSVGLoader.FindPattern(const aID:TpvUTF8String):TpvSizeInt;
var Index:TpvSizeInt;
begin

 result:=-1;
 for Index:=0 to Length(fPatterns)-1 do begin
  if fPatterns[Index].ID=aID then begin
   result:=Index;
   exit;
  end;
 end;

end;

function TpvVectorGraphicsSVGLoader.FindMarker(const aID:TpvUTF8String):TpvSizeInt;
var Index:TpvSizeInt;
begin

 result:=-1;
 for Index:=0 to Length(fMarkers)-1 do begin
  if fMarkers[Index].ID=aID then begin
   result:=Index;
   exit;
  end;
 end;

end;

procedure TpvVectorGraphicsSVGLoader.ApplyMarkers(const aGraphic:TpvVectorGraphicsGraphic;
                                                  const aGroup:TpvVectorGraphicsGroup;
                                                  const aState:TState);
const DegreesToRadians=PI/180.0;
type TVertex=record
      Position:TpvVectorPathVector;
      // Direction of the segment arriving at this vertex and of the one leaving it; either may
      // be zero when the vertex is an endpoint.
      Incoming:TpvVectorPathVector;
      Outgoing:TpvVectorPathVector;
      StartsSubPath:boolean;
      EndsSubPath:boolean;
     end;
var Vertices:array of TVertex;
    CountVertices:TpvSizeInt;
    Index,MarkerIndex:TpvSizeInt;
    Command:TpvVectorPathCommand;
    Current,Start,Anchor,ControlPoint:TpvVectorPathVector;
    HasCurrent:boolean;
 procedure AddVertex(const aPosition,aIncoming,aOutgoing:TpvVectorPathVector;const aStarts,aEnds:boolean);
 begin
  if CountVertices>=Length(Vertices) then begin
   SetLength(Vertices,(CountVertices+1)*2);
  end;
  Vertices[CountVertices].Position:=aPosition;
  Vertices[CountVertices].Incoming:=aIncoming;
  Vertices[CountVertices].Outgoing:=aOutgoing;
  Vertices[CountVertices].StartsSubPath:=aStarts;
  Vertices[CountVertices].EndsSubPath:=aEnds;
  inc(CountVertices);
 end;
 procedure SetOutgoing(const aDirection:TpvVectorPathVector);
 begin
  if CountVertices>0 then begin
   Vertices[CountVertices-1].Outgoing:=aDirection;
  end;
 end;
 function DirectionAngle(const aVertex:TVertex):TpvDouble;
 var Direction:TpvVectorPathVector;
 begin
  // The mid markers bisect the two directions, which is what makes a chain of markers follow a
  // corner smoothly instead of jumping.
  Direction.x:=aVertex.Incoming.x+aVertex.Outgoing.x;
  Direction.y:=aVertex.Incoming.y+aVertex.Outgoing.y;
  if (abs(Direction.x)<1e-12) and (abs(Direction.y)<1e-12) then begin
   Direction:=aVertex.Outgoing;
   if (abs(Direction.x)<1e-12) and (abs(Direction.y)<1e-12) then begin
    Direction:=aVertex.Incoming;
   end;
  end;
  if (abs(Direction.x)<1e-12) and (abs(Direction.y)<1e-12) then begin
   result:=0.0;
  end else begin
   result:=ArcTan2(Direction.y,Direction.x);
  end;
 end;
 procedure PlaceMarker(const aMarkerIndex:TpvSizeInt;const aVertex:TVertex;const aReverse:boolean);
 var Instance:TpvVectorGraphicsGroupInstance;
     Matrix:TpvVectorPathMatrix;
     SimpleMatrix:TpvVectorGraphicsSimpleMatrix;
     Angle,ScaleX,ScaleY,OffsetX,OffsetY,Scale:TpvDouble;
     Marker:^TMarkerDefinition;
 begin
  Marker:=@fMarkers[aMarkerIndex];
  if not assigned(Marker^.Group) then begin
   exit;
  end;
  // The marker is built in its own space and then moved onto the vertex: first the reference
  // point goes to the origin, then the view box mapping, then the stroke width scale, then the
  // rotation, and finally the translation onto the vertex itself.
  Matrix:=TpvVectorPathMatrix.CreateTranslate(-Marker^.RefX,-Marker^.RefY);
  if Marker^.HasViewBox and (Marker^.ViewBoxWidth>1e-12) and (Marker^.ViewBoxHeight>1e-12) then begin
   ScaleX:=Marker^.MarkerWidth/Marker^.ViewBoxWidth;
   ScaleY:=Marker^.MarkerHeight/Marker^.ViewBoxHeight;
   if Marker^.PreserveAspectRatioUniform then begin
    if Marker^.PreserveAspectRatioMeet then begin
     Scale:=Min(ScaleX,ScaleY);
    end else begin
     Scale:=Max(ScaleX,ScaleY);
    end;
    ScaleX:=Scale;
    ScaleY:=Scale;
   end;
   OffsetX:=(Marker^.MarkerWidth-(Marker^.ViewBoxWidth*ScaleX))*Marker^.PreserveAspectRatioAlignX;
   OffsetY:=(Marker^.MarkerHeight-(Marker^.ViewBoxHeight*ScaleY))*Marker^.PreserveAspectRatioAlignY;
   // With a view box the reference point is given in view box units, so the shift has to happen
   // after the mapping rather than before it.
   Matrix:=TpvVectorPathMatrix.CreateTranslate(-Marker^.ViewBoxX,-Marker^.ViewBoxY)*
           TpvVectorPathMatrix.CreateScale(ScaleX,ScaleY)*
           TpvVectorPathMatrix.CreateTranslate(OffsetX-(Marker^.RefX*ScaleX),OffsetY-(Marker^.RefY*ScaleY));
  end;
  if Marker^.ScaleWithStrokeWidth then begin
   Matrix:=Matrix*TpvVectorPathMatrix.CreateScale(aState.StrokeWidth,aState.StrokeWidth);
  end;
  if Marker^.OrientAuto or Marker^.OrientAutoStartReverse then begin
   Angle:=DirectionAngle(aVertex);
   if aReverse and Marker^.OrientAutoStartReverse then begin
    Angle:=Angle+PI;
   end;
  end else begin
   Angle:=Marker^.Orient*DegreesToRadians;
  end;
  Matrix:=Matrix*TpvVectorPathMatrix.CreateRotate(Angle);
  Matrix:=Matrix*TpvVectorPathMatrix.CreateTranslate(aVertex.Position.x,aVertex.Position.y);
  Instance:=TpvVectorGraphicsGroupInstance.Create(fDocument,aGroup);
  Instance.Target:=Marker^.Group;
  Instance.Alpha:=aState.Opacity;
  SimpleMatrix:=TpvVectorGraphicsSimpleMatrix.CreateDefault;
  SimpleMatrix.Matrix:=Matrix;
  Instance.SimpleMatrix:=SimpleMatrix;
  aGroup.Add(Instance);
 end;
 function Direction(const aFrom,aTo:TpvVectorPathVector):TpvVectorPathVector;
 var Length_:TpvDouble;
 begin
  result.x:=aTo.x-aFrom.x;
  result.y:=aTo.y-aFrom.y;
  Length_:=sqrt(sqr(result.x)+sqr(result.y));
  if Length_>1e-12 then begin
   result.x:=result.x/Length_;
   result.y:=result.y/Length_;
  end else begin
   result.x:=0.0;
   result.y:=0.0;
  end;
 end;
begin

 if (Length(aState.MarkerStartID)=0) and
    (Length(aState.MarkerMidID)=0) and
    (Length(aState.MarkerEndID)=0) then begin
  exit;
 end;

 if Length(fMarkers)=0 then begin
  exit;
 end;

 Vertices:=nil;
 CountVertices:=0;
 Current:=TpvVectorPathVector.Create(0.0,0.0);
 Start:=Current;
 HasCurrent:=false;

 // The anchor points of the path commands are the marker positions, with the control points
 // giving the tangents at the ends of a curve.
 for Command in aGraphic.Path.Commands do begin
  if not assigned(Command) then begin
   continue;
  end;
  case Command.CommandType of
   TpvVectorPathCommandType.MoveTo:begin
    Anchor:=TpvVectorPathVector.Create(Command.x0,Command.y0);
    if HasCurrent and (CountVertices>0) then begin
     Vertices[CountVertices-1].EndsSubPath:=true;
    end;
    AddVertex(Anchor,TpvVectorPathVector.Create(0.0,0.0),TpvVectorPathVector.Create(0.0,0.0),true,false);
    Current:=Anchor;
    Start:=Anchor;
    HasCurrent:=true;
   end;
   TpvVectorPathCommandType.LineTo:begin
    Anchor:=TpvVectorPathVector.Create(Command.x0,Command.y0);
    SetOutgoing(Direction(Current,Anchor));
    AddVertex(Anchor,Direction(Current,Anchor),TpvVectorPathVector.Create(0.0,0.0),false,false);
    Current:=Anchor;
   end;
   TpvVectorPathCommandType.QuadraticCurveTo:begin
    ControlPoint:=TpvVectorPathVector.Create(Command.x0,Command.y0);
    Anchor:=TpvVectorPathVector.Create(Command.x1,Command.y1);
    SetOutgoing(Direction(Current,ControlPoint));
    AddVertex(Anchor,Direction(ControlPoint,Anchor),TpvVectorPathVector.Create(0.0,0.0),false,false);
    Current:=Anchor;
   end;
   TpvVectorPathCommandType.CubicCurveTo:begin
    ControlPoint:=TpvVectorPathVector.Create(Command.x0,Command.y0);
    SetOutgoing(Direction(Current,ControlPoint));
    ControlPoint:=TpvVectorPathVector.Create(Command.x1,Command.y1);
    Anchor:=TpvVectorPathVector.Create(Command.x2,Command.y2);
    AddVertex(Anchor,Direction(ControlPoint,Anchor),TpvVectorPathVector.Create(0.0,0.0),false,false);
    Current:=Anchor;
   end;
   TpvVectorPathCommandType.Close:begin
    if HasCurrent then begin
     SetOutgoing(Direction(Current,Start));
     if CountVertices>0 then begin
      Vertices[CountVertices-1].EndsSubPath:=true;
     end;
     Current:=Start;
    end;
   end;
   else begin
   end;
  end;
 end;
 if CountVertices=0 then begin
  exit;
 end;
 Vertices[CountVertices-1].EndsSubPath:=true;
 for Index:=0 to CountVertices-1 do begin
  MarkerIndex:=-1;
  if (Index=0) and (Length(aState.MarkerStartID)>0) then begin
   MarkerIndex:=FindMarker(aState.MarkerStartID);
   if MarkerIndex>=0 then begin
    PlaceMarker(MarkerIndex,Vertices[Index],true);
    continue;
   end;
  end;
  if (Index=(CountVertices-1)) and (Length(aState.MarkerEndID)>0) then begin
   MarkerIndex:=FindMarker(aState.MarkerEndID);
   if MarkerIndex>=0 then begin
    PlaceMarker(MarkerIndex,Vertices[Index],false);
    continue;
   end;
  end;
  if Length(aState.MarkerMidID)>0 then begin
   MarkerIndex:=FindMarker(aState.MarkerMidID);
   if MarkerIndex>=0 then begin
    PlaceMarker(MarkerIndex,Vertices[Index],false);
   end;
  end;

 end;

end;

function TpvVectorGraphicsSVGLoader.ResolveGradient(const aIndex:TpvSizeInt;const aDepth:TpvInt32):TGradient;
var Parent:TGradient;
    ParentIndex:TpvSizeInt;
begin

 // The depth limit catches a chain of references that leads back into itself.
 result:=fGradients[aIndex];
 if (Length(result.HRef)=0) or (aDepth>=16) then begin
  exit;
 end;

 ParentIndex:=FindGradient(result.HRef);
 if (ParentIndex<0) or (ParentIndex=aIndex) then begin
  exit;
 end;

 Parent:=ResolveGradient(ParentIndex,aDepth+1);

 // Only what this gradient does not state itself is taken over from the one it references.
 if not result.HasStops then begin
  result.Stops:=Parent.Stops;
  result.HasStops:=Parent.HasStops;
 end;
 if not result.HasSpreadMethod then begin
  result.SpreadMethod:=Parent.SpreadMethod;
  result.HasSpreadMethod:=Parent.HasSpreadMethod;
 end;
 if not result.HasUnits then begin
  result.Units:=Parent.Units;
  result.HasUnits:=Parent.HasUnits;
 end;
 if not result.HasMatrix then begin
  result.Matrix:=Parent.Matrix;
  result.HasMatrix:=Parent.HasMatrix;
 end;
 if Length(result.x1)=0 then begin
  result.x1:=Parent.x1;
 end;
 if Length(result.y1)=0 then begin
  result.y1:=Parent.y1;
 end;
 if Length(result.x2)=0 then begin
  result.x2:=Parent.x2;
 end;
 if Length(result.y2)=0 then begin
  result.y2:=Parent.y2;
 end;
 if Length(result.cx)=0 then begin
  result.cx:=Parent.cx;
 end;
 if Length(result.cy)=0 then begin
  result.cy:=Parent.cy;
 end;
 if Length(result.fx)=0 then begin
  result.fx:=Parent.fx;
 end;
 if Length(result.fy)=0 then begin
  result.fy:=Parent.fy;
 end;
 if Length(result.r)=0 then begin
  result.r:=Parent.r;
 end;

end;

procedure TpvVectorGraphicsSVGLoader.CollectGradient(const aTag:TpvXMLTag);
var Index,Count,StopCount:TpvSizeInt;
    Gradient:PGradient;
    Value,TagName:TpvUTF8String;
    SubTag:TpvXMLTag;
    StopState:TState;
    StopColor:TpvVectorGraphicsColor;
    StopOpacity:TpvUInt8;
    Offset:TpvDouble;
    Declarations:TpvUTF8String;
begin

 // A paint server without an identifier cannot be referred to and is therefore of no use.
 Value:=TpvUTF8String(aTag.GetParameter('id',''));
 if Length(Value)=0 then begin
  exit;
 end;

 Count:=Length(fGradients);
 SetLength(fGradients,Count+1);
 Gradient:=@fGradients[Count];
 Gradient^.ID:=Value;
 Gradient^.HRef:='';

 Gradient^.SpreadMethod:=TpvVectorGraphicsSpreadMethod.Pad;
 Gradient^.Units:=TUnits.ObjectBoundingBox;
 Gradient^.Matrix:=TpvVectorPathMatrix.CreateIdentity;
 Gradient^.Stops:=nil;

 // The coordinates are kept as text, since whether a number is a fraction or a user unit depends
 // on the gradient units, which a referenced gradient may well be the one to state.
 Gradient^.x1:='';
 Gradient^.y1:='';
 Gradient^.x2:='';
 Gradient^.y2:='';
 Gradient^.cx:='';
 Gradient^.cy:='';
 Gradient^.fx:='';
 Gradient^.fy:='';
 Gradient^.r:='';

 // The flags record what this element states itself, as opposed to what it inherits.
 Gradient^.HasKind:=true;
 Gradient^.HasSpreadMethod:=false;
 Gradient^.HasUnits:=false;
 Gradient^.HasMatrix:=false;
 Gradient^.HasStops:=false;

 if LowerCase(String(aTag.Name))='radialgradient' then begin
  Gradient^.Kind:=TGradientKind.Radial;
 end else begin
  Gradient^.Kind:=TGradientKind.Linear;
 end;

 // Both the namespaced and the plain spelling of the reference are accepted.
 Value:=TpvUTF8String(aTag.GetParameter('xlink:href',TpvRawByteString(aTag.GetParameter('href',''))));
 if (Length(Value)>0) and (Value[1]='#') then begin
  Gradient^.HRef:=Copy(Value,2,Length(Value));
 end;

 Value:=TpvUTF8String(aTag.GetParameter('gradientUnits',''));
 if Length(Value)>0 then begin
  Gradient^.HasUnits:=true;
  if LowerCase(String(Value))='userspaceonuse' then begin
   Gradient^.Units:=TUnits.UserSpaceOnUse;
  end else begin
   Gradient^.Units:=TUnits.ObjectBoundingBox;
  end;
 end;

 Value:=TpvUTF8String(aTag.GetParameter('spreadMethod',''));
 if Length(Value)>0 then begin
  Gradient^.HasSpreadMethod:=true;
  Value:=TpvUTF8String(LowerCase(String(Value)));
  if Value='reflect' then begin
   Gradient^.SpreadMethod:=TpvVectorGraphicsSpreadMethod.Reflect;
  end else if Value='repeat' then begin
   Gradient^.SpreadMethod:=TpvVectorGraphicsSpreadMethod.Repeat_;
  end else begin
   Gradient^.SpreadMethod:=TpvVectorGraphicsSpreadMethod.Pad;
  end;
 end;

 Value:=TpvUTF8String(aTag.GetParameter('gradientTransform',''));
 if Length(Value)>0 then begin
  Gradient^.HasMatrix:=true;
  Gradient^.Matrix:=ParseTransform(Value);
 end;

 Gradient^.x1:=TpvUTF8String(aTag.GetParameter('x1',''));
 Gradient^.y1:=TpvUTF8String(aTag.GetParameter('y1',''));
 Gradient^.x2:=TpvUTF8String(aTag.GetParameter('x2',''));
 Gradient^.y2:=TpvUTF8String(aTag.GetParameter('y2',''));
 Gradient^.cx:=TpvUTF8String(aTag.GetParameter('cx',''));
 Gradient^.cy:=TpvUTF8String(aTag.GetParameter('cy',''));
 Gradient^.fx:=TpvUTF8String(aTag.GetParameter('fx',''));
 Gradient^.fy:=TpvUTF8String(aTag.GetParameter('fy',''));
 Gradient^.r:=TpvUTF8String(aTag.GetParameter('r',''));

 // And then the stop elements, which are the gradient's own children.
 StopCount:=0;
 for Index:=0 to aTag.Items.Count-1 do begin
  if aTag.Items[Index] is TpvXMLTag then begin
   SubTag:=TpvXMLTag(aTag.Items[Index]);
   TagName:=TpvUTF8String(LowerCase(String(SubTag.Name)));
   if TagName='stop' then begin

    StopState:=TState.CreateDefault;
    StopColor:=$ff000000;
    StopOpacity:=255;
    Offset:=ParseValue(TpvUTF8String(SubTag.GetParameter('offset','0')),1.0);

    // stop-color and stop-opacity may come as attributes or inside a style attribute, so both
    // are gathered into one declaration list and read back out of it.
    Declarations:='';
    Value:=TpvUTF8String(SubTag.GetParameter('stop-color',''));
    if Length(Value)>0 then begin
     Declarations:=Declarations+'fill:'+Value+';';
    end;
    Value:=TpvUTF8String(SubTag.GetParameter('stop-opacity',''));
    if Length(Value)>0 then begin
     Declarations:=Declarations+'fill-opacity:'+Value+';';
    end;
    Value:=TpvUTF8String(SubTag.GetParameter('style',''));
    if Length(Value)>0 then begin
     Value:=TpvUTF8String(StringReplace(StringReplace(String(Value),'stop-color','fill',[rfReplaceAll,rfIgnoreCase]),
                                        'stop-opacity','fill-opacity',[rfReplaceAll,rfIgnoreCase]));
     Declarations:=Declarations+Value+';';
    end;

    if Length(Declarations)>0 then begin
     ParseStyleDeclarations(Declarations,StopState);
     if StopState.FillHasPaint then begin
      StopColor:=StopState.FillColor;
     end else begin
      StopColor:=0;
     end;
     StopOpacity:=StopState.FillOpacity;
    end;

    SetLength(Gradient^.Stops,StopCount+1);
    Gradient^.Stops[StopCount].Offset:=Offset;
    Gradient^.Stops[StopCount].Color:=pvVectorGraphicsModulateColor(StopColor,StopOpacity);
    inc(StopCount);

   end;
  end;
 end;

 // No stops of its own means the ones of a referenced gradient still apply.
 if StopCount>0 then begin
  Gradient^.HasStops:=true;
 end;

end;

procedure TpvVectorGraphicsSVGLoader.CollectPattern(const aTag:TpvXMLTag;const aState:TState);
var Count:TpvSizeInt;
    Value:TpvUTF8String;
    Group:TpvVectorGraphicsGroup;
    State:TState;
begin

 Value:=TpvUTF8String(aTag.GetParameter('id',''));
 if Length(Value)=0 then begin
  exit;
 end;

 // The two kinds of units are independent of each other and default the other way round: the
 // tile rectangle in fractions of the bounding box, its contents in user units.
 Count:=Length(fPatterns);
 SetLength(fPatterns,Count+1);
 fPatterns[Count].ID:=Value;
 fPatterns[Count].Units:=TUnits.ObjectBoundingBox;
 fPatterns[Count].ContentUnits:=TUnits.UserSpaceOnUse;
 fPatterns[Count].Matrix:=TpvVectorPathMatrix.CreateIdentity;

 if LowerCase(String(aTag.GetParameter('patternUnits','objectBoundingBox')))='userspaceonuse' then begin
  fPatterns[Count].Units:=TUnits.UserSpaceOnUse;
 end;

 if LowerCase(String(aTag.GetParameter('patternContentUnits','userSpaceOnUse')))='objectboundingbox' then begin
  fPatterns[Count].ContentUnits:=TUnits.ObjectBoundingBox;
 end;

 Value:=TpvUTF8String(aTag.GetParameter('patternTransform',''));
 if Length(Value)>0 then begin
  fPatterns[Count].Matrix:=ParseTransform(Value);
 end;

 fPatterns[Count].x:=ParseValue(TpvUTF8String(aTag.GetParameter('x','0')),fViewportWidth);
 fPatterns[Count].y:=ParseValue(TpvUTF8String(aTag.GetParameter('y','0')),fViewportHeight);
 fPatterns[Count].Width:=ParseValue(TpvUTF8String(aTag.GetParameter('width','0')),fViewportWidth);
 fPatterns[Count].Height:=ParseValue(TpvUTF8String(aTag.GetParameter('height','0')),fViewportHeight);

 // The tile's contents become a hidden group in defs, which is later rendered into the buffer
 // the image paint samples from.
 Group:=TpvVectorGraphicsGroup.Create(fDocument,fDocument.Defs);
 fDocument.Defs.Add(Group);
 Group.Width:=fPatterns[Count].Width;
 Group.Height:=fPatterns[Count].Height;
 Group.Visible:=false;
 fPatterns[Count].Group:=Group;

 // The tile starts from a clean slate: neither the transform nor the clipping of whatever
 // referred to the pattern applies inside it.
 State:=aState;
 State.Matrix:=TpvVectorPathMatrix.CreateIdentity;
 State.ClipPathID:='';
 State.MaskID:='';

 ProcessChildren(aTag,Group,State);

end;

procedure TpvVectorGraphicsSVGLoader.CollectGroupDefinition(const aTag:TpvXMLTag;const aState:TState;const aKind:TpvUTF8String);
var Value,ViewBoxText,Token:TpvUTF8String;
    Group:TpvVectorGraphicsGroup;
    State:TState;
    Count,ViewBoxCount:TpvSizeInt;
    ViewBoxValues:array[0..3] of TpvDouble;
    Marker:^TMarkerDefinition;
begin

 Value:=TpvUTF8String(aTag.GetParameter('id',''));
 if Length(Value)=0 then begin
  exit;
 end;

 // Clip paths, masks, markers and symbols all become a hidden group in defs; what tells them
 // apart is only how they are referred to later.
 Group:=TpvVectorGraphicsGroup.Create(fDocument,fDocument.Defs);
 fDocument.Defs.Add(Group);
 Group.Visible:=false;
 Group.ID:=Value;

 // A marker additionally needs its own placement data, which nothing else uses.
 if aKind='marker' then begin

  Count:=Length(fMarkers);
  SetLength(fMarkers,Count+1);
  Marker:=@fMarkers[Count];
  Marker^.ID:=Value;
  Marker^.Group:=Group;

  Marker^.RefX:=ParseValue(TpvUTF8String(aTag.GetParameter('refX','0')),fViewportWidth);
  Marker^.RefY:=ParseValue(TpvUTF8String(aTag.GetParameter('refY','0')),fViewportHeight);
  Marker^.MarkerWidth:=ParseValue(TpvUTF8String(aTag.GetParameter('markerWidth','3')),fViewportWidth);
  Marker^.MarkerHeight:=ParseValue(TpvUTF8String(aTag.GetParameter('markerHeight','3')),fViewportHeight);
  Marker^.ScaleWithStrokeWidth:=LowerCase(String(aTag.GetParameter('markerUnits','strokeWidth')))<>'userspaceonuse';

  // auto turns the marker along the path, auto-start-reverse additionally flips the one at the
  // start, and anything else is a fixed angle.
  Value:=TpvUTF8String(LowerCase(Trim(String(aTag.GetParameter('orient','0')))));
  Marker^.OrientAuto:=Value='auto';
  Marker^.OrientAutoStartReverse:=Value='auto-start-reverse';
  if Marker^.OrientAuto or Marker^.OrientAutoStartReverse then begin
   Marker^.Orient:=0.0;
  end else begin
   Marker^.Orient:=ParseValue(Value);
  end;

  Marker^.HasViewBox:=false;
  Marker^.ViewBoxX:=0.0;
  Marker^.ViewBoxY:=0.0;
  Marker^.ViewBoxWidth:=0.0;
  Marker^.ViewBoxHeight:=0.0;

  // Four numbers, or the view box counts as absent.
  ViewBoxText:=TpvUTF8String(aTag.GetParameter('viewBox',''));
  ViewBoxCount:=0;
  while (Length(ViewBoxText)>0) and (ViewBoxCount<4) do begin
   Token:=TrimText(ParseToken(ViewBoxText,#9#10#13' ,'));
   if Length(Token)=0 then begin
    continue;
   end;
   ViewBoxValues[ViewBoxCount]:=ParseValue(Token);
   inc(ViewBoxCount);
  end;

  if ViewBoxCount=4 then begin
   Marker^.HasViewBox:=true;
   Marker^.ViewBoxX:=ViewBoxValues[0];
   Marker^.ViewBoxY:=ViewBoxValues[1];
   Marker^.ViewBoxWidth:=ViewBoxValues[2];
   Marker^.ViewBoxHeight:=ViewBoxValues[3];
  end;

  // Same keywords as on the document itself, only kept per marker.
  Marker^.PreserveAspectRatioUniform:=true;
  Marker^.PreserveAspectRatioMeet:=true;
  Marker^.PreserveAspectRatioAlignX:=0.5;
  Marker^.PreserveAspectRatioAlignY:=0.5;
  Value:=TpvUTF8String(LowerCase(Trim(String(aTag.GetParameter('preserveAspectRatio','')))));
  if Length(Value)>0 then begin
   Marker^.PreserveAspectRatioMeet:=Pos(TpvUTF8String('slice'),Value)=0;
   if Pos(TpvUTF8String('none'),Value)>0 then begin
    Marker^.PreserveAspectRatioUniform:=false;
    Marker^.PreserveAspectRatioAlignX:=0.0;
    Marker^.PreserveAspectRatioAlignY:=0.0;
   end else begin
    if Pos(TpvUTF8String('xmin'),Value)>0 then begin
     Marker^.PreserveAspectRatioAlignX:=0.0;
    end else if Pos(TpvUTF8String('xmax'),Value)>0 then begin
     Marker^.PreserveAspectRatioAlignX:=1.0;
    end;
    if Pos(TpvUTF8String('ymin'),Value)>0 then begin
     Marker^.PreserveAspectRatioAlignY:=0.0;
    end else if Pos(TpvUTF8String('ymax'),Value)>0 then begin
     Marker^.PreserveAspectRatioAlignY:=1.0;
    end;
   end;
  end;

 end;

 // As with a pattern tile, the definition starts from a clean slate.
 State:=aState;
 State.Matrix:=TpvVectorPathMatrix.CreateIdentity;
 State.ClipPathID:='';
 State.MaskID:='';

 if aKind='clippath' then begin
  // A clip path paints plain coverage, so the fill of its children is forced on and any stroke
  // is dropped, which is what the clipping rules of SVG say.
  State.FillHasPaint:=true;
  State.FillColor:=$ffffffff;
  State.FillOpacity:=255;
  State.FillPaintID:='';
  State.StrokeHasPaint:=false;
  State.StrokePaintID:='';
  State.FillRule:=aState.ClipRule;
 end;

 ApplyPresentationAttributes(aTag,State);

 // The attributes of the element itself could have set a paint again, which for a clip path has
 // to be overridden once more.
 if aKind='clippath' then begin
  State.FillHasPaint:=true;
  State.FillColor:=$ffffffff;
  State.FillOpacity:=255;
  State.FillPaintID:='';
  State.StrokeHasPaint:=false;
  State.StrokePaintID:='';
 end;

 ProcessChildren(aTag,Group,State);

end;

procedure TpvVectorGraphicsSVGLoader.CollectDefinitions(const aItem:TpvXMLItem;const aState:TState);
var Index:TpvSizeInt;
    Tag:TpvXMLTag;
    TagName:TpvUTF8String;
begin

 for Index:=0 to aItem.Items.Count-1 do begin
  if aItem.Items[Index] is TpvXMLTag then begin

   Tag:=TpvXMLTag(aItem.Items[Index]);
   TagName:=TpvUTF8String(LowerCase(String(Tag.Name)));

   if (TagName='lineargradient') or (TagName='radialgradient') then begin
    CollectGradient(Tag);
   end else if TagName='pattern' then begin
    CollectPattern(Tag,aState);
   end else if TagName='filter' then begin
    CollectFilter(Tag,aState);
   end else if (TagName='clippath') or (TagName='mask') or (TagName='marker') or (TagName='symbol') then begin
    CollectGroupDefinition(Tag,aState,TagName);
   end else begin
    // Definitions may sit anywhere in the tree, not just inside a defs element, so the whole
    // document gets walked.
    CollectDefinitions(Tag,aState);
   end;

  end;
 end;

end;

function TpvVectorGraphicsSVGLoader.ProcessPath(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState):TpvVectorGraphicsObject;
var Shape:TpvVectorGraphicsShape;
    PathData:TpvRawByteString;
    ParsedPath:TpvVectorPath;
begin

 result:=nil;

 PathData:=aTag.GetParameter('d','');
 if Length(PathData)=0 then begin
  exit;
 end;

 Shape:=TpvVectorGraphicsShape.Create(fDocument,aGroup);
 try

  // The engine's own path parser already understands the whole of the d attribute.
  ParsedPath:=TpvVectorPath.CreateFromSVGPath(PathData);
  try
   Shape.Path.Assign(ParsedPath);
  finally
   FreeAndNil(ParsedPath);
  end;

  FinishGraphic(aTag,Shape,aState);

 except
  FreeAndNil(Shape);
  raise;
 end;

 result:=Shape;

end;

function TpvVectorGraphicsSVGLoader.ProcessRectangle(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState):TpvVectorGraphicsObject;
var Rectangle:TpvVectorGraphicsRectangle;
begin

 result:=nil;

 Rectangle:=TpvVectorGraphicsRectangle.Create(fDocument,aGroup);
 try

  Rectangle.x:=ParseValue(TpvUTF8String(aTag.GetParameter('x','0')),fViewportWidth);
  Rectangle.y:=ParseValue(TpvUTF8String(aTag.GetParameter('y','0')),fViewportHeight);
  Rectangle.Width:=ParseValue(TpvUTF8String(aTag.GetParameter('width','0')),fViewportWidth);
  Rectangle.Height:=ParseValue(TpvUTF8String(aTag.GetParameter('height','0')),fViewportHeight);
  Rectangle.RadiusX:=ParseValue(TpvUTF8String(aTag.GetParameter('rx','0')),fViewportWidth);
  Rectangle.RadiusY:=ParseValue(TpvUTF8String(aTag.GetParameter('ry','0')),fViewportHeight);

  // SVG says a rectangle without extent is simply not drawn.
  if (Rectangle.Width<=0.0) or (Rectangle.Height<=0.0) then begin
   FreeAndNil(Rectangle);
   exit;
  end;

  Rectangle.MarkPathDirty;
  Rectangle.Update;

  // The element parameters stay in their own coordinate system, so the transform is baked into
  // the generated path instead, which keeps a later non-uniform scale from distorting the
  // corner radii differently than SVG would.
  FinishGraphic(aTag,Rectangle,aState);

 except
  FreeAndNil(Rectangle);
  raise;
 end;

 result:=Rectangle;

end;

function TpvVectorGraphicsSVGLoader.ProcessCircle(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState):TpvVectorGraphicsObject;
var Ellipse:TpvVectorGraphicsEllipse;
    Radius:TpvDouble;
begin

 result:=nil;

 // A percentage radius resolves against the diagonal of the viewport, which is the reference
 // SVG gives for a length that belongs to neither axis.
 Radius:=ParseValue(TpvUTF8String(aTag.GetParameter('r','0')),
                    sqrt(sqr(fViewportWidth)+sqr(fViewportHeight))/sqrt(2.0));
 if Radius<=0.0 then begin
  exit;
 end;

 Ellipse:=TpvVectorGraphicsEllipse.Create(fDocument,aGroup);
 try

  Ellipse.CenterX:=ParseValue(TpvUTF8String(aTag.GetParameter('cx','0')),fViewportWidth);
  Ellipse.CenterY:=ParseValue(TpvUTF8String(aTag.GetParameter('cy','0')),fViewportHeight);
  Ellipse.RadiusX:=Radius;
  Ellipse.RadiusY:=Radius;

  Ellipse.MarkPathDirty;
  Ellipse.Update;

  FinishGraphic(aTag,Ellipse,aState);

 except
  FreeAndNil(Ellipse);
  raise;
 end;

 result:=Ellipse;

end;

function TpvVectorGraphicsSVGLoader.ProcessEllipse(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState):TpvVectorGraphicsObject;
var Ellipse:TpvVectorGraphicsEllipse;
begin

 result:=nil;

 Ellipse:=TpvVectorGraphicsEllipse.Create(fDocument,aGroup);
 try

  Ellipse.CenterX:=ParseValue(TpvUTF8String(aTag.GetParameter('cx','0')),fViewportWidth);
  Ellipse.CenterY:=ParseValue(TpvUTF8String(aTag.GetParameter('cy','0')),fViewportHeight);
  Ellipse.RadiusX:=ParseValue(TpvUTF8String(aTag.GetParameter('rx','0')),fViewportWidth);
  Ellipse.RadiusY:=ParseValue(TpvUTF8String(aTag.GetParameter('ry','0')),fViewportHeight);

  if (Ellipse.RadiusX<=0.0) or (Ellipse.RadiusY<=0.0) then begin
   FreeAndNil(Ellipse);
   exit;
  end;

  Ellipse.MarkPathDirty;
  Ellipse.Update;

  FinishGraphic(aTag,Ellipse,aState);

 except
  FreeAndNil(Ellipse);
  raise;
 end;

 result:=Ellipse;

end;

function TpvVectorGraphicsSVGLoader.ProcessLine(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState):TpvVectorGraphicsObject;
var Line:TpvVectorGraphicsLine;
begin

 result:=nil;

 Line:=TpvVectorGraphicsLine.Create(fDocument,aGroup);
 try

  Line.x1:=ParseValue(TpvUTF8String(aTag.GetParameter('x1','0')),fViewportWidth);
  Line.y1:=ParseValue(TpvUTF8String(aTag.GetParameter('y1','0')),fViewportHeight);
  Line.x2:=ParseValue(TpvUTF8String(aTag.GetParameter('x2','0')),fViewportWidth);
  Line.y2:=ParseValue(TpvUTF8String(aTag.GetParameter('y2','0')),fViewportHeight);

  Line.MarkPathDirty;
  Line.Update;

  FinishGraphic(aTag,Line,aState);

 except
  FreeAndNil(Line);
  raise;
 end;

 result:=Line;

end;

function TpvVectorGraphicsSVGLoader.ProcessPolygon(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState;const aClosed:boolean):TpvVectorGraphicsObject;
var Shape:TpvVectorGraphicsShape;
    Points,Token:TpvUTF8String;
    Values:array of TpvDouble;
    Count,Index:TpvSizeInt;
begin

 result:=nil;

 Points:=TpvUTF8String(aTag.GetParameter('points',''));
 if Length(Points)=0 then begin
  exit;
 end;

 // The coordinates come as one flat list, where whitespace and commas separate alike.
 Values:=nil;
 Count:=0;
 while Length(Points)>0 do begin
  Token:=TrimText(ParseToken(Points,#9#10#13' ,'));
  if Length(Token)=0 then begin
   continue;
  end;
  if Count>=Length(Values) then begin
   SetLength(Values,(Count+1)*2);
  end;
  Values[Count]:=ParseValue(Token);
  inc(Count);
 end;

 // Fewer than two points is not a polygon.
 if Count<4 then begin
  exit;
 end;

 Shape:=TpvVectorGraphicsShape.Create(fDocument,aGroup);
 try

  // A trailing lone coordinate is dropped, which is what the loop condition takes care of.
  Shape.Path.MoveTo(Values[0],Values[1]);
  Index:=2;
  while (Index+1)<Count do begin
   Shape.Path.LineTo(Values[Index],Values[Index+1]);
   inc(Index,2);
  end;

  // This is the only difference between polygon and polyline.
  if aClosed then begin
   Shape.Path.Close;
  end;

  FinishGraphic(aTag,Shape,aState);

 except
  FreeAndNil(Shape);
  raise;
 end;

 result:=Shape;

end;

function TpvVectorGraphicsSVGLoader.LoadImageReference(const aReference:TpvUTF8String):TpvVectorGraphicsImageResource;
var Reference,Header,Payload:TpvUTF8String;
    Stream:TMemoryStream;
    FileStream:TFileStream;
    FileName:string;
    ImageData:TpvPointer;
    ImageWidth,ImageHeight:TpvInt32;
    PixelFormat:TpvPNGPixelFormat;
    Index:TpvSizeInt;
    Decoded:boolean;
begin

 result:=nil;

 Reference:=TrimText(aReference);
 if Length(Reference)=0 then begin
  exit;
 end;

 Stream:=TMemoryStream.Create;
 try

  // Either the bytes come inline as a data URI, or they come out of a file beside the document.
  if LowerCase(Copy(String(Reference),1,5))='data:' then begin

   Index:=Pos(TpvUTF8String(','),Reference);
   if Index=0 then begin
    exit;
   end;

   Header:=TpvUTF8String(LowerCase(Copy(String(Reference),1,Index-1)));
   Payload:=Copy(Reference,Index+1,Length(Reference));
   if Pos(TpvUTF8String(';base64'),Header)=0 then begin
    // Only the base64 form of a data URI carries binary image data in practice.
    exit;
   end;

   // Whitespace inside the payload is common when the attribute got line wrapped.
   Payload:=TpvUTF8String(StringReplace(StringReplace(StringReplace(String(Payload),#13,'',[rfReplaceAll]),
                                                      #10,'',[rfReplaceAll]),
                                        ' ','',[rfReplaceAll]));
   if not TpvBase64.Decode(TpvRawByteString(Payload),Stream) then begin
    exit;
   end;

  end else begin

   // Without a base directory there is nothing a relative reference could resolve against, which
   // is the case when the document came from a stream rather than from a file.
   if Length(fBaseDirectory)=0 then begin
    exit;
   end;

   FileName:=IncludeTrailingPathDelimiter(fBaseDirectory)+String(Reference);
   if not FileExists(FileName) then begin
    exit;
   end;

   FileStream:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite);
   try
    Stream.CopyFrom(FileStream,FileStream.Size);
   finally
    FreeAndNil(FileStream);
   end;

  end;

  if Stream.Size<=0 then begin
   exit;
  end;

  // The format is not taken from the data URI's media type but simply tried out, since the type
  // is missing or wrong often enough.
  ImageData:=nil;
  ImageWidth:=0;
  ImageHeight:=0;
  PixelFormat:=TpvPNGPixelFormat.Unknown;
  Decoded:=LoadPNGImage(Stream.Memory,Stream.Size,ImageData,ImageWidth,ImageHeight,false,PixelFormat);
  if not Decoded then begin
   Decoded:=LoadJPEGImage(Stream.Memory,Stream.Size,ImageData,ImageWidth,ImageHeight,false);
  end;

  if Decoded and assigned(ImageData) and (ImageWidth>0) and (ImageHeight>0) then begin
   try
    result:=fDocument.NewImage;
    // Both decoders hand back R8G8B8A8, which has red and blue the other way round from the
    // layout the rasterizer works in.
    result.SetPixelsFromRGBA8(ImageData,ImageWidth,ImageHeight);
   finally
    FreeMem(ImageData);
   end;
  end else if assigned(ImageData) then begin
   FreeMem(ImageData);
  end;

 finally
  FreeAndNil(Stream);
 end;

end;

function TpvVectorGraphicsSVGLoader.ProcessImage(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState):TpvVectorGraphicsObject;
var ImageObject:TpvVectorGraphicsImageObject;
    Resource:TpvVectorGraphicsImageResource;
    Reference:TpvUTF8String;
    SimpleMatrix:TpvVectorGraphicsSimpleMatrix;
begin

 result:=nil;

 Reference:=TpvUTF8String(aTag.GetParameter('xlink:href',TpvRawByteString(aTag.GetParameter('href',''))));
 Resource:=LoadImageReference(Reference);
 if not assigned(Resource) then begin
  exit;
 end;

 ImageObject:=TpvVectorGraphicsImageObject.Create(fDocument,aGroup);
 try

  ImageObject.x:=ParseValue(TpvUTF8String(aTag.GetParameter('x','0')),fViewportWidth);
  ImageObject.y:=ParseValue(TpvUTF8String(aTag.GetParameter('y','0')),fViewportHeight);
  ImageObject.Width:=ParseValue(TpvUTF8String(aTag.GetParameter('width','0')),fViewportWidth);
  ImageObject.Height:=ParseValue(TpvUTF8String(aTag.GetParameter('height','0')),fViewportHeight);

  // A missing extent means the image's own pixel size.
  if ImageObject.Width<=0.0 then begin
   ImageObject.Width:=Resource.Width;
  end;
  if ImageObject.Height<=0.0 then begin
   ImageObject.Height:=Resource.Height;
  end;

  // The transform of an image stays on the object rather than going into the geometry, so there
  // is nothing baked in that a paint server would have to be brought back out of.
  ImageObject.SetImage(Resource);
  ApplyStateToGraphic(ImageObject,aState,TpvVectorPathMatrix.CreateIdentity);

  // The fill mode has to stay on the image, which ApplyStateToGraphic would have turned into a
  // plain color because the state says nothing about images.
  ImageObject.SetImage(Resource);
  ImageObject.MarkPathDirty;

  // The image samples through its own fill matrix, so the inherited transform goes onto the
  // object rather than into the path, which would break that mapping.
  SimpleMatrix:=TpvVectorGraphicsSimpleMatrix.CreateDefault;
  SimpleMatrix.Matrix:=aState.Matrix;
  ImageObject.SimpleMatrix:=SimpleMatrix;

 except
  FreeAndNil(ImageObject);
  raise;
 end;

 result:=ImageObject;

end;

function TpvVectorGraphicsSVGLoader.ProcessText(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState;
                                                const aOffsetX,aOffsetY:TpvDouble;
                                                const aLevel:TpvInt32):TpvVectorGraphicsObject;
var Text:TpvVectorGraphicsText;
    Content,Value:TpvUTF8String;
    Index:TpvSizeInt;
    PositionX,PositionY:TpvDouble;
    SimpleMatrix:TpvVectorGraphicsSimpleMatrix;
    State:TState;
    SubTag:TpvXMLTag;
begin

 result:=nil;

 // Nested tspans end somewhere, whatever the document claims.
 if aLevel>8 then begin
  exit;
 end;

 // Without its own position a run continues where the previous one left off.
 PositionX:=aOffsetX;
 PositionY:=aOffsetY;
 Value:=TpvUTF8String(aTag.GetParameter('x',''));
 if Length(Value)>0 then begin
  PositionX:=ParseValue(Value,fViewportWidth);
 end;
 Value:=TpvUTF8String(aTag.GetParameter('y',''));
 if Length(Value)>0 then begin
  PositionY:=ParseValue(Value,fViewportHeight);
 end;

 // Direct text content becomes one run; a tspan with its own position becomes a run of its own,
 // which is as far as the text layout of this port goes.
 Content:='';
 for Index:=0 to aTag.Items.Count-1 do begin
  if aTag.Items[Index] is TpvXMLText then begin
   Content:=Content+TpvUTF8String(TpvXMLText(aTag.Items[Index]).Text);
  end else if aTag.Items[Index] is TpvXMLCDataTag then begin
   Content:=Content+TpvUTF8String(TpvXMLCDataTag(aTag.Items[Index]).Text);
  end;
 end;
 Content:=TrimText(Content);

 if Length(Content)>0 then begin

  Text:=TpvVectorGraphicsText.Create(fDocument,aGroup);
  try

   Text.Text:=Content;
   Text.x:=PositionX;
   Text.y:=PositionY;

   Text.FontFamily:=aState.FontFamily;
   Text.FontSize:=aState.FontSize;
   Text.Bold:=aState.FontBold;
   Text.Italic:=aState.FontItalic;
   Text.LetterSpacing:=aState.LetterSpacing;

   case aState.TextAnchor of
    1:begin
     Text.Anchor:=TpvVectorGraphicsTextAnchor.Middle;
    end;
    2:begin
     Text.Anchor:=TpvVectorGraphicsTextAnchor.End_;
    end;
    else begin
     Text.Anchor:=TpvVectorGraphicsTextAnchor.Start;
    end;
   end;

   Value:=TpvUTF8String(LowerCase(String(aTag.GetParameter('dominant-baseline',''))));
   if (Value='central') or (Value='middle') then begin
    Text.Baseline:=TpvVectorGraphicsTextBaseline.Central;
   end else if (Value='hanging') or (Value='text-before-edge') then begin
    Text.Baseline:=TpvVectorGraphicsTextBaseline.Hanging;
   end else if (Value='text-after-edge') or (Value='ideographic') then begin
    Text.Baseline:=TpvVectorGraphicsTextBaseline.Bottom;
   end else begin
    Text.Baseline:=TpvVectorGraphicsTextBaseline.Alphabetic;
   end;

   // As with an image, the transform of a text run stays on the object.
   ApplyStateToGraphic(Text,aState,TpvVectorPathMatrix.CreateIdentity);

   // The outline is built in the text's own space, so the inherited transform goes onto the
   // object rather than into the path, which keeps the glyphs from being rebuilt on every
   // change of the transform.
   SimpleMatrix:=TpvVectorGraphicsSimpleMatrix.CreateDefault;
   SimpleMatrix.Matrix:=aState.Matrix;
   Text.SimpleMatrix:=SimpleMatrix;
   Text.MarkPathDirty;

   aGroup.Add(Text);
   result:=Text;

  except
   FreeAndNil(Text);
   raise;
  end;

 end;

 // And then the nested runs, each of which inherits this one's state.
 for Index:=0 to aTag.Items.Count-1 do begin
  if aTag.Items[Index] is TpvXMLTag then begin
   SubTag:=TpvXMLTag(aTag.Items[Index]);
   if LowerCase(String(SubTag.Name))='tspan' then begin
    State:=aState;
    ApplyPresentationAttributes(SubTag,State);
    if State.Display then begin
     ProcessText(SubTag,aGroup,State,PositionX,PositionY,aLevel+1);
    end;
   end;
  end;
 end;

end;

function TpvVectorGraphicsSVGLoader.ProcessGroup(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState):TpvVectorGraphicsObject;
var Group:TpvVectorGraphicsGroup;
    State:TState;
    SimpleMatrix:TpvVectorGraphicsSimpleMatrix;
begin

 Group:=TpvVectorGraphicsGroup.Create(fDocument,aGroup);

 // The caller adds it to the parent, but the children below need it to be there already, so
 // that an id inside it resolves while the subtree is still being built.
 aGroup.Add(Group);

 Group.Alpha:=aState.Opacity;
 Group.Visible:=aState.Display;

 // A group with its own opacity has to be composited as a whole, otherwise overlapping children
 // would show through each other.
 Group.OffImage:=aState.Opacity<>255;

 Group.BlendMode:=aState.BlendMode;
 Group.Isolate:=aState.Isolate;

 State:=aState;

 // The opacity is applied once, on the group, so the children must not apply it again.
 State.Opacity:=255;

 // Clip path and mask likewise belong to the group and not to each of its children.
 State.ClipPathID:='';
 State.MaskID:='';

 // And the two that do not inherit in the first place.
 State.BlendMode:=TpvVectorGraphicsBlendMode.Normal;
 State.Isolate:=false;

 // A group with animations keeps its transform instead of passing it down into the geometry of
 // its children, for the same reason a shape does: an animated transform is given in the group's
 // own user space and only means the right thing there.
 if HasAnimationChildren(aTag) then begin
  SimpleMatrix:=TpvVectorGraphicsSimpleMatrix.CreateDefault;
  SimpleMatrix.Matrix:=aState.Matrix;
  Group.SimpleMatrix:=SimpleMatrix;
  State.Matrix:=TpvVectorPathMatrix.CreateIdentity;
 end;

 ProcessChildren(aTag,Group,State);

 result:=Group;

end;

function TpvVectorGraphicsSVGLoader.ProcessUse(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState):TpvVectorGraphicsObject;
var Instance:TpvVectorGraphicsGroupInstance;
    Reference:TpvUTF8String;
    Target:TpvVectorGraphicsObject;
    OffsetX,OffsetY:TpvDouble;
    Matrix:TpvVectorPathMatrix;
    SimpleMatrix:TpvVectorGraphicsSimpleMatrix;
begin

 result:=nil;

 Reference:=TpvUTF8String(aTag.GetParameter('xlink:href',TpvRawByteString(aTag.GetParameter('href',''))));
 if (Length(Reference)>0) and (Reference[1]='#') then begin
  Reference:=Copy(Reference,2,Length(Reference));
 end;
 if Length(Reference)=0 then begin
  exit;
 end;

 // Only a target that is already in the document can be referred to, which is why the
 // definitions pass runs first.
 Target:=fDocument.FindByID(Reference);
 if not assigned(Target) then begin
  exit;
 end;

 OffsetX:=ParseValue(TpvUTF8String(aTag.GetParameter('x','0')),fViewportWidth);
 OffsetY:=ParseValue(TpvUTF8String(aTag.GetParameter('y','0')),fViewportHeight);

 Instance:=TpvVectorGraphicsGroupInstance.Create(fDocument,aGroup);
 Instance.Target:=Target;
 Instance.Alpha:=aState.Opacity;
 Instance.Visible:=aState.Display;
 Instance.BlendMode:=aState.BlendMode;
 Instance.Isolate:=aState.Isolate;

 Matrix:=aState.Matrix;
 if (abs(OffsetX)>1e-12) or (abs(OffsetY)>1e-12) then begin
  // The x and y of a use element shift the referenced content, before whatever transform the
  // element itself carries.
  Matrix:=TpvVectorPathMatrix.CreateTranslate(OffsetX,OffsetY)*Matrix;
 end;

 SimpleMatrix:=TpvVectorGraphicsSimpleMatrix.CreateDefault;
 SimpleMatrix.Matrix:=Matrix;
 Instance.SimpleMatrix:=SimpleMatrix;

 result:=Instance;

end;

function TpvVectorGraphicsSVGLoader.ApplyClipAndMask(const aObject:TpvVectorGraphicsObject;
                                                     const aGroup:TpvVectorGraphicsGroup;
                                                     const aState:TState):TpvVectorGraphicsObject;
var Wrapper:TpvVectorGraphicsGroup;
    MaskTarget:TpvVectorGraphicsObject;
    MaskInstance:TpvVectorGraphicsGroupInstance;
begin

 result:=aObject;
 if (not assigned(aObject)) or ((Length(aState.ClipPathID)=0) and (Length(aState.MaskID)=0)) then begin
  exit;
 end;

 // A clip path takes precedence over a mask when both are given, and a reference that leads
 // nowhere leaves the object unclipped.
 MaskTarget:=nil;
 if Length(aState.ClipPathID)>0 then begin
  MaskTarget:=fDocument.FindByID(aState.ClipPathID);
 end;
 if (not assigned(MaskTarget)) and (Length(aState.MaskID)>0) then begin
  MaskTarget:=fDocument.FindByID(aState.MaskID);
 end;
 if not assigned(MaskTarget) then begin
  exit;
 end;

 // The scene graph says that a child marked as a clip path narrows its siblings, so the clipped
 // object and a reference to the clip path go into a wrapper group together.
 Wrapper:=TpvVectorGraphicsGroup.Create(fDocument,aGroup);

 // Extract, not Remove, since the object is moving into the wrapper rather than going away.
 aGroup.Extract(aObject);
 aGroup.Add(Wrapper);

 // Clip takes the coverage of the shape, mono its luminance.
 MaskInstance:=TpvVectorGraphicsGroupInstance.Create(fDocument,Wrapper);
 MaskInstance.Target:=MaskTarget;
 if Length(aState.ClipPathID)>0 then begin
  MaskInstance.MaskMode:=TpvVectorGraphicsMaskMode.Clip;
 end else begin
  MaskInstance.MaskMode:=TpvVectorGraphicsMaskMode.Mono;
 end;

 // The mask has to come first, since the scene graph only clips the siblings that follow it.
 Wrapper.Add(MaskInstance);
 Wrapper.Add(aObject);

 result:=Wrapper;

end;

function TpvVectorGraphicsSVGLoader.ProcessTag(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup;const aState:TState):TpvVectorGraphicsObject;
var TagName,Value:TpvUTF8String;
    State:TState;
begin

 result:=nil;
 TagName:=TpvUTF8String(LowerCase(String(aTag.Name)));

 // Everything that is only a definition, metadata or unsupported is skipped here; the paint
 // servers and clip paths were already collected in the first pass.
 if (TagName='defs') or (TagName='lineargradient') or (TagName='radialgradient') or
    (TagName='pattern') or (TagName='clippath') or (TagName='mask') or (TagName='marker') or
    (TagName='symbol') or (TagName='style') or (TagName='title') or (TagName='desc') or
    (TagName='metadata') or (TagName='script') or (TagName='filter') or
    (TagName='font') or (TagName='font-face') or (TagName='animate') or
    (TagName='animatecolor') or (TagName='animatetransform') or
    (TagName='animatemotion') or (TagName='set') or
    // A tspan only has meaning inside a text element, which handles it itself.
    (TagName='tspan') or (TagName='textpath') then begin
  exit;
 end;

 State:=aState;

 // Unlike the rest of the state these two do not inherit, so whatever the parent carried is
 // dropped before this element's own attributes are read.
 State.BlendMode:=TpvVectorGraphicsBlendMode.Normal;
 State.Isolate:=false;

 ApplyPresentationAttributes(aTag,State);
 if not State.Display then begin
  exit;
 end;

 if TagName='path' then begin
  result:=ProcessPath(aTag,aGroup,State);
 end else if TagName='rect' then begin
  result:=ProcessRectangle(aTag,aGroup,State);
 end else if TagName='circle' then begin
  result:=ProcessCircle(aTag,aGroup,State);
 end else if TagName='ellipse' then begin
  result:=ProcessEllipse(aTag,aGroup,State);
 end else if TagName='line' then begin
  result:=ProcessLine(aTag,aGroup,State);
 end else if TagName='polyline' then begin
  result:=ProcessPolygon(aTag,aGroup,State,false);
 end else if TagName='polygon' then begin
  result:=ProcessPolygon(aTag,aGroup,State,true);
 end else if TagName='use' then begin
  result:=ProcessUse(aTag,aGroup,State);
 end else if TagName='text' then begin
  result:=ProcessText(aTag,aGroup,State);
 end else if TagName='image' then begin
  result:=ProcessImage(aTag,aGroup,State);
 end else if (TagName='g') or (TagName='a') or (TagName='switch') or (TagName='svg') then begin
  result:=ProcessGroup(aTag,aGroup,State);
 end;

 if assigned(result) then begin

  // A group and a text run have already been attached, because their children needed a place to
  // go; everything else is attached here.
  if not ((result is TpvVectorGraphicsGroup) or (result is TpvVectorGraphicsText)) then begin
   aGroup.Add(result);
  end;

  Value:=TpvUTF8String(aTag.GetParameter('id',''));
  if Length(Value)>0 then begin
   result.ID:=Value;
  end;

  // The animation elements sit as children of whatever they animate, and they belong on the
  // object itself rather than on a wrapper, so they are read before the clipping below.
  ProcessAnimations(aTag,result,State);

  // The filters were all collected before the tree was built, so the reference resolves here.
  if Length(State.FilterID)>0 then begin
   result.Filter:=fDocument.FindFilter(State.FilterID);
  end;

  // Markers go alongside the shape, so they have to be placed before the whole thing possibly
  // moves into a clipping wrapper.
  if result is TpvVectorGraphicsGraphic then begin
   ApplyMarkers(TpvVectorGraphicsGraphic(result),aGroup,State);
  end;

  result:=ApplyClipAndMask(result,aGroup,State);

 end;

end;

procedure TpvVectorGraphicsSVGLoader.ProcessChildren(const aItem:TpvXMLItem;const aGroup:TpvVectorGraphicsGroup;const aState:TState);
var Index:TpvSizeInt;
begin

 for Index:=0 to aItem.Items.Count-1 do begin
  if aItem.Items[Index] is TpvXMLTag then begin
   ProcessTag(TpvXMLTag(aItem.Items[Index]),aGroup,aState);
  end;
 end;

end;

procedure TpvVectorGraphicsSVGLoader.ResolvePaint(const aObject:TpvVectorGraphicsObject;const aID:TpvUTF8String;const aStroke:boolean;
                                                  const aBakedMatrix:TpvVectorPathMatrix);
var GradientIndex,PatternIndex,Index:TpvSizeInt;
    Gradient:TGradient;
    Graphic:TpvVectorGraphicsGraphic;
    Style:TpvVectorGraphicsStyle;
    FillData:PpvVectorGraphicsFillData;
    Matrix,Canonical:TpvVectorPathMatrix;
    BoundingBox:TpvVectorGraphicsBoundingBox;
    x1,y1,x2,y2,Radius,BoxWidth,BoxHeight:TpvDouble;
    Opacity:TpvUInt8;
 function ParseAxisX(const aValue:TpvUTF8String;const aDefault:TpvDouble):TpvDouble;
 begin
  if Length(aValue)=0 then begin
   result:=aDefault;
  end else if Gradient.Units=TUnits.ObjectBoundingBox then begin
   result:=ParseValue(aValue,1.0);
  end else begin
   result:=ParseValue(aValue,fViewportWidth);
  end;
 end;
 function ParseAxisY(const aValue:TpvUTF8String;const aDefault:TpvDouble):TpvDouble;
 begin
  if Length(aValue)=0 then begin
   result:=aDefault;
  end else if Gradient.Units=TUnits.ObjectBoundingBox then begin
   result:=ParseValue(aValue,1.0);
  end else begin
   result:=ParseValue(aValue,fViewportHeight);
  end;
 end;
 function ParseRadius(const aValue:TpvUTF8String;const aDefault:TpvDouble):TpvDouble;
 begin
  if Length(aValue)=0 then begin
   result:=aDefault;
  end else if Gradient.Units=TUnits.ObjectBoundingBox then begin
   result:=ParseValue(aValue,1.0);
  end else begin
   result:=ParseValue(aValue,sqrt(sqr(fViewportWidth)+sqr(fViewportHeight))/sqrt(2.0));
  end;
 end;
begin

 // Only a graphic has a paint; a group that carried the attribute passed it to its children.
 if not (aObject is TpvVectorGraphicsGraphic) then begin
  exit;
 end;

 Graphic:=TpvVectorGraphicsGraphic(aObject);
 Style:=Graphic.Style;
 if aStroke then begin
  FillData:=@Style.StrokeFillData;
 end else begin
  FillData:=@Style.FillData;
 end;

 // The bounding box is what object bounding box units resolve against, and a degenerate one
 // would otherwise turn the paint matrix singular.
 BoundingBox:=Graphic.BoundingBox;
 if BoundingBox.IsEmpty then begin
  BoxWidth:=1.0;
  BoxHeight:=1.0;
  BoundingBox.x1:=0.0;
  BoundingBox.y1:=0.0;
 end else begin
  BoxWidth:=BoundingBox.Width;
  BoxHeight:=BoundingBox.Height;
  if abs(BoxWidth)<1e-12 then begin
   BoxWidth:=1e-12;
  end;
  if abs(BoxHeight)<1e-12 then begin
   BoxHeight:=1e-12;
  end;
 end;

 // A pattern is the simpler case, since its tile is rendered separately and only sampled here.
 PatternIndex:=FindPattern(aID);
 if PatternIndex>=0 then begin

  if FillData^.Mode<>TpvVectorGraphicsFillMode.None then begin
   FillData^.Mode:=TpvVectorGraphicsFillMode.Pattern;
  end;
  FillData^.Pattern:=fPatterns[PatternIndex].Group;
  FillData^.SpreadMethod:=TpvVectorGraphicsSpreadMethod.Repeat_;

  // The paint matrix maps a device pixel into the tile, so the steps come in the reverse order
  // of how the pattern was built up, exactly as for a gradient below.
  if fPatterns[PatternIndex].Units=TUnits.ObjectBoundingBox then begin
   Matrix:=(TpvVectorPathMatrix.CreateScale(BoxWidth,BoxHeight)*
            TpvVectorPathMatrix.CreateTranslate(BoundingBox.x1,BoundingBox.y1)).Inverse;
  end else begin
   Matrix:=aBakedMatrix.Inverse;
  end;

  FillData^.SimpleMatrix:=TpvVectorGraphicsSimpleMatrix.CreateDefault;
  FillData^.SimpleMatrix.Matrix:=Matrix*fPatterns[PatternIndex].Matrix.Inverse;
  Graphic.Style:=Style;
  exit;

 end;

 GradientIndex:=FindGradient(aID);
 if GradientIndex<0 then begin
  exit;
 end;

 Gradient:=ResolveGradient(GradientIndex);

 if Length(Gradient.Stops)=0 then begin
  // A paint server without stops paints nothing at all, which SVG spells out explicitly.
  FillData^.Mode:=TpvVectorGraphicsFillMode.None;
  Graphic.Style:=Style;
  exit;
 end;

 if Length(Gradient.Stops)=1 then begin
  // A single stop is a flat color, and taking that shortcut avoids a pointless gradient ramp.
  FillData^.Mode:=TpvVectorGraphicsFillMode.Color;
  FillData^.Color:=Gradient.Stops[0].Color;
  Graphic.Style:=Style;
  exit;
 end;

 // A fill mode of none means the element had no paint to begin with, which a paint server
 // reference does not change.
 if FillData^.Mode<>TpvVectorGraphicsFillMode.None then begin
  if Gradient.Kind=TGradientKind.Linear then begin
   FillData^.Mode:=TpvVectorGraphicsFillMode.LinearGradient;
  end else begin
   FillData^.Mode:=TpvVectorGraphicsFillMode.RadialGradient;
  end;
 end;
 x1:=0.0;
 y1:=0.0;
 x2:=0.0;
 y2:=0.0;
 Radius:=0.0;

 // The canonical form a gradient paint samples in: for a linear one the gradient vector lies on
 // the positive x axis starting at the origin, for a radial one the center sits at the origin.
 if Gradient.Kind=TGradientKind.Linear then begin

  // The defaults of a linear gradient run left to right across the object box.
  x1:=ParseAxisX(Gradient.x1,0.0);
  y1:=ParseAxisY(Gradient.y1,0.0);
  x2:=ParseAxisX(Gradient.x2,1.0);
  y2:=ParseAxisY(Gradient.y2,0.0);

  Canonical:=TpvVectorPathMatrix.CreateTranslate(-x1,-y1)*
             TpvVectorPathMatrix.CreateRotate(-ArcTan2(y2-y1,x2-x1));

  Radius:=sqrt(sqr(x2-x1)+sqr(y2-y1));
  if Radius<1e-9 then begin
   // A degenerate gradient vector paints the last stop everywhere, per the specification.
   FillData^.Mode:=TpvVectorGraphicsFillMode.Color;
   FillData^.Color:=Gradient.Stops[Length(Gradient.Stops)-1].Color;
   Graphic.Style:=Style;
   exit;
  end;

 end else begin

  Radius:=ParseRadius(Gradient.r,0.5);
  if abs(Radius)<1e-5 then begin
   Radius:=1e-5;
  end;

  x1:=ParseAxisX(Gradient.cx,0.5);
  y1:=ParseAxisY(Gradient.cy,0.5);

  // A missing focus coincides with the center, which is the usual centered highlight.
  x2:=ParseAxisX(Gradient.fx,x1);
  y2:=ParseAxisY(Gradient.fy,y1);

  Canonical:=TpvVectorPathMatrix.CreateTranslate(-x1,-y1);

 end;

 // The paint matrix takes a point in document space and has to end up in that canonical space,
 // so the three steps come in the reverse order of how the gradient was built up.
 if Gradient.Units=TUnits.ObjectBoundingBox then begin

  // The coordinates are fractions of the bounding box, which is already in document space, so
  // the first step is simply out of that box.
  Matrix:=(TpvVectorPathMatrix.CreateScale(BoxWidth,BoxHeight)*
           TpvVectorPathMatrix.CreateTranslate(BoundingBox.x1,BoundingBox.y1)).Inverse;

 end else begin

  // The coordinates are in the user space the element was in, and that element's own transform
  // went into its geometry on loading, so it has to be undone here.
  Matrix:=aBakedMatrix.Inverse;

 end;

 // Then the gradient transform, and only then the step into the canonical form, because that is
 // the space the gradient coordinates were given in.
 Matrix:=Matrix*Gradient.Matrix.Inverse*Canonical;

 FillData^.SimpleMatrix:=TpvVectorGraphicsSimpleMatrix.CreateDefault;
 FillData^.SimpleMatrix.Matrix:=Matrix;
 FillData^.GradientFocusSimpleMatrix:=TpvVectorGraphicsSimpleMatrix.CreateDefault;
 FillData^.GradientFocusX:=x2-x1;
 FillData^.GradientFocusY:=y2-y1;
 FillData^.GradientRadius:=Radius;
 FillData^.SpreadMethod:=Gradient.SpreadMethod;

 // The fill-opacity or stroke-opacity of the element sits in the alpha of the color that was put
 // here before the paint server was resolved, and once the gradient takes over, that color has no
 // say any more. So it goes into the stops, which is where it ends up having the same effect.
 Opacity:=(FillData^.Color shr 24) and $ff;

 SetLength(FillData^.GradientStops,Length(Gradient.Stops));
 for Index:=0 to Length(Gradient.Stops)-1 do begin
  FillData^.GradientStops[Index]:=Gradient.Stops[Index];
  if Opacity<>255 then begin
   FillData^.GradientStops[Index].Color:=pvVectorGraphicsModulateColor(Gradient.Stops[Index].Color,Opacity);
  end;
 end;

 Graphic.Style:=Style;

end;

function TpvVectorGraphicsSVGLoader.ParseClockValue(const aValue:TpvUTF8String;out aTicks:TpvInt64):boolean;
var Text,Part:TpvUTF8String;
    Seconds:TpvDouble;
    Parts:array[0..2] of TpvDouble;
    Count:TpvSizeInt;
begin

 aTicks:=0;

 Text:=TpvUTF8String(LowerCase(Trim(String(aValue))));
 if (Length(Text)=0) or (Text='indefinite') or (Text='media') then begin
  result:=false;
  exit;
 end;

 result:=true;

 // The full clock form, so hours, minutes and seconds separated by colons, with the hours
 // optional. Anything without a colon is a plain number with an optional unit.
 if Pos(TpvUTF8String(':'),Text)>0 then begin

  Count:=0;
  while (Length(Text)>0) and (Count<3) do begin
   Part:=TrimText(ParseToken(Text,':',false));
   Parts[Count]:=ParseValue(Part);
   inc(Count);
  end;

  if Count>=3 then begin
   Seconds:=((Parts[0]*3600.0)+(Parts[1]*60.0))+Parts[2];
  end else if Count=2 then begin
   Seconds:=(Parts[0]*60.0)+Parts[1];
  end else begin
   Seconds:=Parts[0];
  end;

 end else begin

  // The unit comes after the number, and seconds are what is meant when none is given.
  if Copy(Text,Length(Text)-1,2)='ms' then begin
   Seconds:=ParseValue(Copy(Text,1,Length(Text)-2))/1000.0;
  end else if Copy(Text,Length(Text),1)='s' then begin
   Seconds:=ParseValue(Copy(Text,1,Length(Text)-1));
  end else if Copy(Text,Length(Text),1)='h' then begin
   Seconds:=ParseValue(Copy(Text,1,Length(Text)-1))*3600.0;
  end else if Copy(Text,Length(Text)-2,3)='min' then begin
   Seconds:=ParseValue(Copy(Text,1,Length(Text)-3))*60.0;
  end else begin
   Seconds:=ParseValue(Text);
  end;

 end;

 aTicks:=round(Seconds*pvVectorGraphicsTicksPerSecond);

end;

function TpvVectorGraphicsSVGLoader.LoadImageBuffer(const aReference:TpvUTF8String):TpvVectorGraphicsPixelBuffer;
var Resource:TpvVectorGraphicsImageResource;
    x,y:TpvInt32;
    Pixels:PpvVectorGraphicsRawColorArray;
begin

 result:=nil;

 Resource:=LoadImageReference(aReference);
 if (not assigned(Resource)) or (Resource.Width<=0) or (Resource.Height<=0) then begin
  exit;
 end;

 // A copy rather than a view, because the primitive owns what it is handed and the resource
 // belongs to the document.
 result:=TpvVectorGraphicsPixelBuffer.Create(Resource.Width,Resource.Height,
                                             TpvVectorGraphicsColorSpaceMode.Integer);
 Pixels:=Resource.GetPixelPointer;
 if assigned(Pixels) then begin
  for y:=0 to Resource.Height-1 do begin
   for x:=0 to Resource.Width-1 do begin
    result.SetPixel(x,y,Pixels^[(TpvSizeInt(y)*TpvSizeInt(Resource.Width))+TpvSizeInt(x)]);
   end;
  end;
 end;

end;

procedure TpvVectorGraphicsSVGLoader.CollectFilter(const aTag:TpvXMLTag;const aState:TState);
var Filter:TpvVectorGraphicsFilter;
    Index,SubIndex,ValueIndex:TpvSizeInt;
    SubTag,FunctionTag:TpvXMLTag;
    Value,TagName,Text,Token:TpvUTF8String;
    Primitive:TpvVectorGraphicsFilterPrimitive;
    Kind:TpvVectorGraphicsFilterPrimitiveKind;
    IsNone:boolean;
    Numbers:array of TpvDouble;
    CountNumbers:TpvSizeInt;
    Saturation,Angle,Cosine,Sine:TpvDouble;
    TransferFunction:TpvVectorGraphicsFilterTransferFunction;

 // The two numbers that most of the frequency and deviation attributes carry, where a single one
 // stands for both axes.
 procedure ParsePair(const aText:TpvUTF8String;out aX,aY:TpvDouble);
 var Rest,Part:TpvUTF8String;
     Count:TpvSizeInt;
     Values:array[0..1] of TpvDouble;
 begin
  Rest:=TrimText(aText);
  Count:=0;
  while (Length(Rest)>0) and (Count<2) do begin
   Part:=TrimText(ParseToken(Rest,#9#10#13' ,'));
   if Length(Part)=0 then begin
    continue;
   end;
   Values[Count]:=ParseValue(Part);
   inc(Count);
  end;
  if Count=0 then begin
   aX:=0.0;
   aY:=0.0;
  end else if Count=1 then begin
   aX:=Values[0];
   aY:=Values[0];
  end else begin
   aX:=Values[0];
   aY:=Values[1];
  end;
 end;

 // A whitespace or comma separated list of numbers, which several primitives carry.
 procedure ParseNumberList(const aText:TpvUTF8String);
 var Rest,Part:TpvUTF8String;
 begin
  Numbers:=nil;
  CountNumbers:=0;
  Rest:=TrimText(aText);
  while Length(Rest)>0 do begin
   Part:=TrimText(ParseToken(Rest,#9#10#13' ,'));
   if Length(Part)=0 then begin
    continue;
   end;
   SetLength(Numbers,CountNumbers+1);
   Numbers[CountNumbers]:=ParseValue(Part);
   inc(CountNumbers);
  end;
 end;

 // The four optional numbers of a subregion, which every primitive may carry. In bounding box
 // units a percentage is a fraction of the box rather than of the viewport, which is why the
 // base a percentage resolves against depends on the units.
 procedure ParseSubregion(const aPrimitiveTag:TpvXMLTag;const aPrimitive:TpvVectorGraphicsFilterPrimitive);
 var BaseX,BaseY:TpvDouble;
 begin

  if Filter.PrimitiveUnits=TpvVectorGraphicsFilterUnits.ObjectBoundingBox then begin
   BaseX:=1.0;
   BaseY:=1.0;
  end else begin
   BaseX:=fViewportWidth;
   BaseY:=fViewportHeight;
  end;

  Text:=TrimText(TpvUTF8String(aPrimitiveTag.GetParameter('x','')));
  if Length(Text)>0 then begin
   aPrimitive.HasX:=true;
   aPrimitive.x:=ParseValue(Text,BaseX);
  end;

  Text:=TrimText(TpvUTF8String(aPrimitiveTag.GetParameter('y','')));
  if Length(Text)>0 then begin
   aPrimitive.HasY:=true;
   aPrimitive.y:=ParseValue(Text,BaseY);
  end;

  Text:=TrimText(TpvUTF8String(aPrimitiveTag.GetParameter('width','')));
  if Length(Text)>0 then begin
   aPrimitive.HasWidth:=true;
   aPrimitive.Width:=ParseValue(Text,BaseX);
  end;

  Text:=TrimText(TpvUTF8String(aPrimitiveTag.GetParameter('height','')));
  if Length(Text)>0 then begin
   aPrimitive.HasHeight:=true;
   aPrimitive.Height:=ParseValue(Text,BaseY);
  end;

 end;

 // One of the four transfer functions of an feComponentTransfer.
 procedure ParseTransferFunction(const aFunctionTag:TpvXMLTag;out aFunction:TpvVectorGraphicsFilterTransferFunction);
 var FunctionType:TpvUTF8String;
     ListIndex:TpvSizeInt;
 begin

  aFunction:=TpvVectorGraphicsFilterTransferFunction.CreateDefault;

  FunctionType:=TpvUTF8String(LowerCase(Trim(String(aFunctionTag.GetParameter('type','identity')))));
  if FunctionType='table' then begin
   aFunction.Kind:=TpvVectorGraphicsFilterTransferKind.Table;
  end else if FunctionType='discrete' then begin
   aFunction.Kind:=TpvVectorGraphicsFilterTransferKind.Discrete;
  end else if FunctionType='linear' then begin
   aFunction.Kind:=TpvVectorGraphicsFilterTransferKind.Linear;
  end else if FunctionType='gamma' then begin
   aFunction.Kind:=TpvVectorGraphicsFilterTransferKind.Gamma;
  end;

  ParseNumberList(TpvUTF8String(aFunctionTag.GetParameter('tableValues','')));
  SetLength(aFunction.TableValues,CountNumbers);
  for ListIndex:=0 to CountNumbers-1 do begin
   aFunction.TableValues[ListIndex]:=Numbers[ListIndex];
  end;

  aFunction.Slope:=ParseValue(TpvUTF8String(aFunctionTag.GetParameter('slope','1')),1.0);
  aFunction.Intercept:=ParseValue(TpvUTF8String(aFunctionTag.GetParameter('intercept','0')),1.0);
  aFunction.Amplitude:=ParseValue(TpvUTF8String(aFunctionTag.GetParameter('amplitude','1')),1.0);
  aFunction.Exponent:=ParseValue(TpvUTF8String(aFunctionTag.GetParameter('exponent','1')),1.0);
  aFunction.Offset:=ParseValue(TpvUTF8String(aFunctionTag.GetParameter('offset','0')),1.0);

 end;

 // The light source of one of the two lighting primitives, which is a child element rather than
 // an attribute.
 procedure ParseLight(const aPrimitiveTag:TpvXMLTag;const aPrimitive:TpvVectorGraphicsFilterPrimitive);
 var LightIndex:TpvSizeInt;
     LightTag:TpvXMLTag;
     LightName:TpvUTF8String;
 begin

  for LightIndex:=0 to aPrimitiveTag.Items.Count-1 do begin

   if not (aPrimitiveTag.Items[LightIndex] is TpvXMLTag) then begin
    continue;
   end;

   LightTag:=TpvXMLTag(aPrimitiveTag.Items[LightIndex]);
   LightName:=TpvUTF8String(LowerCase(String(LightTag.Name)));

   if LightName='fedistantlight' then begin

    aPrimitive.LightKind:=TpvVectorGraphicsFilterLightKind.Distant;
    aPrimitive.LightAzimuth:=ParseValue(TpvUTF8String(LightTag.GetParameter('azimuth','0')));
    aPrimitive.LightElevation:=ParseValue(TpvUTF8String(LightTag.GetParameter('elevation','0')));
    break;

   end else if LightName='fepointlight' then begin

    aPrimitive.LightKind:=TpvVectorGraphicsFilterLightKind.Point;
    aPrimitive.LightX:=ParseValue(TpvUTF8String(LightTag.GetParameter('x','0')),fViewportWidth);
    aPrimitive.LightY:=ParseValue(TpvUTF8String(LightTag.GetParameter('y','0')),fViewportHeight);
    aPrimitive.LightZ:=ParseValue(TpvUTF8String(LightTag.GetParameter('z','0')),fViewportWidth);
    break;

   end else if LightName='fespotlight' then begin

    aPrimitive.LightKind:=TpvVectorGraphicsFilterLightKind.Spot;
    aPrimitive.LightX:=ParseValue(TpvUTF8String(LightTag.GetParameter('x','0')),fViewportWidth);
    aPrimitive.LightY:=ParseValue(TpvUTF8String(LightTag.GetParameter('y','0')),fViewportHeight);
    aPrimitive.LightZ:=ParseValue(TpvUTF8String(LightTag.GetParameter('z','0')),fViewportWidth);
    aPrimitive.LightPointsAtX:=ParseValue(TpvUTF8String(LightTag.GetParameter('pointsAtX','0')),fViewportWidth);
    aPrimitive.LightPointsAtY:=ParseValue(TpvUTF8String(LightTag.GetParameter('pointsAtY','0')),fViewportHeight);
    aPrimitive.LightPointsAtZ:=ParseValue(TpvUTF8String(LightTag.GetParameter('pointsAtZ','0')),fViewportWidth);
    aPrimitive.SpecularExponent:=ParseValue(TpvUTF8String(LightTag.GetParameter('specularExponent','1')),1.0);
    Text:=TrimText(TpvUTF8String(LightTag.GetParameter('limitingConeAngle','')));
    if Length(Text)>0 then begin
     aPrimitive.HasLightLimitingConeAngle:=true;
     aPrimitive.LightLimitingConeAngle:=ParseValue(Text);
    end;
    break;

   end;

  end;

 end;

begin

 Value:=TpvUTF8String(aTag.GetParameter('id',''));
 if Length(Value)=0 then begin
  exit;
 end;

 Filter:=fDocument.NewFilter(Value);

 if LowerCase(String(aTag.GetParameter('filterUnits','objectBoundingBox')))='userspaceonuse' then begin
  Filter.FilterUnits:=TpvVectorGraphicsFilterUnits.UserSpaceOnUse;
 end;

 if LowerCase(String(aTag.GetParameter('primitiveUnits','userSpaceOnUse')))='objectboundingbox' then begin
  Filter.PrimitiveUnits:=TpvVectorGraphicsFilterUnits.ObjectBoundingBox;
 end;

 // The working space defaults to linear light, which is the single most common reason a filter
 // comes out looking different from what a browser shows.
 if LowerCase(Trim(String(aTag.GetParameter('color-interpolation-filters','linearRGB'))))='srgb' then begin
  Filter.ColorSpace:=TpvVectorGraphicsFilterColorSpace.SRGB;
 end;

 // In bounding box units the four numbers of the region are fractions of the box, so a percentage
 // there resolves against one and not against the viewport.
 if Filter.FilterUnits=TpvVectorGraphicsFilterUnits.ObjectBoundingBox then begin
  Saturation:=1.0;
  Angle:=1.0;
 end else begin
  Saturation:=fViewportWidth;
  Angle:=fViewportHeight;
 end;

 Text:=TrimText(TpvUTF8String(aTag.GetParameter('x','')));
 if Length(Text)>0 then begin
  Filter.x:=ParseValue(Text,Saturation);
 end;
 Text:=TrimText(TpvUTF8String(aTag.GetParameter('y','')));
 if Length(Text)>0 then begin
  Filter.y:=ParseValue(Text,Angle);
 end;
 Text:=TrimText(TpvUTF8String(aTag.GetParameter('width','')));
 if Length(Text)>0 then begin
  Filter.Width:=ParseValue(Text,Saturation);
 end;
 Text:=TrimText(TpvUTF8String(aTag.GetParameter('height','')));
 if Length(Text)>0 then begin
  Filter.Height:=ParseValue(Text,Angle);
 end;

 for Index:=0 to aTag.Items.Count-1 do begin

  if not (aTag.Items[Index] is TpvXMLTag) then begin
   continue;
  end;

  SubTag:=TpvXMLTag(aTag.Items[Index]);
  TagName:=TpvUTF8String(LowerCase(String(SubTag.Name)));

  if TagName='feblend' then begin
   Kind:=TpvVectorGraphicsFilterPrimitiveKind.Blend;
  end else if TagName='fecolormatrix' then begin
   Kind:=TpvVectorGraphicsFilterPrimitiveKind.ColorMatrix;
  end else if TagName='fecomponenttransfer' then begin
   Kind:=TpvVectorGraphicsFilterPrimitiveKind.ComponentTransfer;
  end else if TagName='fecomposite' then begin
   Kind:=TpvVectorGraphicsFilterPrimitiveKind.Composite;
  end else if TagName='feconvolvematrix' then begin
   Kind:=TpvVectorGraphicsFilterPrimitiveKind.ConvolveMatrix;
  end else if TagName='fediffuselighting' then begin
   Kind:=TpvVectorGraphicsFilterPrimitiveKind.DiffuseLighting;
  end else if TagName='fedisplacementmap' then begin
   Kind:=TpvVectorGraphicsFilterPrimitiveKind.DisplacementMap;
  end else if TagName='fedropshadow' then begin
   Kind:=TpvVectorGraphicsFilterPrimitiveKind.DropShadow;
  end else if TagName='feflood' then begin
   Kind:=TpvVectorGraphicsFilterPrimitiveKind.Flood;
  end else if TagName='fegaussianblur' then begin
   Kind:=TpvVectorGraphicsFilterPrimitiveKind.GaussianBlur;
  end else if TagName='feimage' then begin
   Kind:=TpvVectorGraphicsFilterPrimitiveKind.Image;
  end else if TagName='femerge' then begin
   Kind:=TpvVectorGraphicsFilterPrimitiveKind.Merge;
  end else if TagName='femorphology' then begin
   Kind:=TpvVectorGraphicsFilterPrimitiveKind.Morphology;
  end else if TagName='feoffset' then begin
   Kind:=TpvVectorGraphicsFilterPrimitiveKind.Offset;
  end else if TagName='fespecularlighting' then begin
   Kind:=TpvVectorGraphicsFilterPrimitiveKind.SpecularLighting;
  end else if TagName='fetile' then begin
   Kind:=TpvVectorGraphicsFilterPrimitiveKind.Tile;
  end else if TagName='feturbulence' then begin
   Kind:=TpvVectorGraphicsFilterPrimitiveKind.Turbulence;
  end else begin
   continue;
  end;

  Primitive:=TpvVectorGraphicsFilterPrimitive.Create(Kind);
  Filter.Primitives.Add(Primitive);

  Primitive.Input1:=TrimText(TpvUTF8String(SubTag.GetParameter('in','')));
  Primitive.Input2:=TrimText(TpvUTF8String(SubTag.GetParameter('in2','')));
  Primitive.ResultName:=TrimText(TpvUTF8String(SubTag.GetParameter('result','')));

  ParseSubregion(SubTag,Primitive);

  case Kind of

   TpvVectorGraphicsFilterPrimitiveKind.Blend:begin
    Primitive.BlendMode:=ParseBlendMode(TpvUTF8String(SubTag.GetParameter('mode','normal')));
   end;

   TpvVectorGraphicsFilterPrimitiveKind.ColorMatrix:begin

    Text:=TpvUTF8String(LowerCase(Trim(String(SubTag.GetParameter('type','matrix')))));
    ParseNumberList(TpvUTF8String(SubTag.GetParameter('values','')));

    if Text='saturate' then begin

     // The specification gives the matrix of a saturation outright, so it is filled in here and
     // the primitive itself only ever sees a plain matrix.
     if CountNumbers>0 then begin
      Saturation:=Numbers[0];
     end else begin
      Saturation:=1.0;
     end;
     for ValueIndex:=0 to 19 do begin
      Primitive.SetColorMatrixValue(ValueIndex,0.0);
     end;
     Primitive.SetColorMatrixValue(0,0.213+(0.787*Saturation));
     Primitive.SetColorMatrixValue(1,0.715-(0.715*Saturation));
     Primitive.SetColorMatrixValue(2,0.072-(0.072*Saturation));
     Primitive.SetColorMatrixValue(5,0.213-(0.213*Saturation));
     Primitive.SetColorMatrixValue(6,0.715+(0.285*Saturation));
     Primitive.SetColorMatrixValue(7,0.072-(0.072*Saturation));
     Primitive.SetColorMatrixValue(10,0.213-(0.213*Saturation));
     Primitive.SetColorMatrixValue(11,0.715-(0.715*Saturation));
     Primitive.SetColorMatrixValue(12,0.072+(0.928*Saturation));
     Primitive.SetColorMatrixValue(18,1.0);

    end else if Text='huerotate' then begin

     if CountNumbers>0 then begin
      Angle:=Numbers[0]*(PI/180.0);
     end else begin
      Angle:=0.0;
     end;
     Cosine:=cos(Angle);
     Sine:=sin(Angle);
     for ValueIndex:=0 to 19 do begin
      Primitive.SetColorMatrixValue(ValueIndex,0.0);
     end;
     Primitive.SetColorMatrixValue(0,0.213+(Cosine*0.787)-(Sine*0.213));
     Primitive.SetColorMatrixValue(1,0.715-(Cosine*0.715)-(Sine*0.715));
     Primitive.SetColorMatrixValue(2,0.072-(Cosine*0.072)+(Sine*0.928));
     Primitive.SetColorMatrixValue(5,0.213-(Cosine*0.213)+(Sine*0.143));
     Primitive.SetColorMatrixValue(6,0.715+(Cosine*0.285)+(Sine*0.140));
     Primitive.SetColorMatrixValue(7,0.072-(Cosine*0.072)-(Sine*0.283));
     Primitive.SetColorMatrixValue(10,0.213-(Cosine*0.213)-(Sine*0.787));
     Primitive.SetColorMatrixValue(11,0.715-(Cosine*0.715)+(Sine*0.715));
     Primitive.SetColorMatrixValue(12,0.072+(Cosine*0.928)+(Sine*0.072));
     Primitive.SetColorMatrixValue(18,1.0);

    end else if Text='luminancetoalpha' then begin

     for ValueIndex:=0 to 19 do begin
      Primitive.SetColorMatrixValue(ValueIndex,0.0);
     end;
     Primitive.SetColorMatrixValue(15,0.2125);
     Primitive.SetColorMatrixValue(16,0.7154);
     Primitive.SetColorMatrixValue(17,0.0721);

    end else if CountNumbers>=20 then begin

     for ValueIndex:=0 to 19 do begin
      Primitive.SetColorMatrixValue(ValueIndex,Numbers[ValueIndex]);
     end;

    end;

   end;

   TpvVectorGraphicsFilterPrimitiveKind.ComponentTransfer:begin

    for SubIndex:=0 to SubTag.Items.Count-1 do begin
     if SubTag.Items[SubIndex] is TpvXMLTag then begin
      FunctionTag:=TpvXMLTag(SubTag.Items[SubIndex]);
      Token:=TpvUTF8String(LowerCase(String(FunctionTag.Name)));
      if Token='fefuncr' then begin
       ParseTransferFunction(FunctionTag,TransferFunction);
       Primitive.SetTransferFunction(0,TransferFunction);
      end else if Token='fefuncg' then begin
       ParseTransferFunction(FunctionTag,TransferFunction);
       Primitive.SetTransferFunction(1,TransferFunction);
      end else if Token='fefuncb' then begin
       ParseTransferFunction(FunctionTag,TransferFunction);
       Primitive.SetTransferFunction(2,TransferFunction);
      end else if Token='fefunca' then begin
       ParseTransferFunction(FunctionTag,TransferFunction);
       Primitive.SetTransferFunction(3,TransferFunction);
      end;
     end;
    end;

   end;

   TpvVectorGraphicsFilterPrimitiveKind.Composite:begin

    Text:=TpvUTF8String(LowerCase(Trim(String(SubTag.GetParameter('operator','over')))));
    if Text='in' then begin
     Primitive.CompositeOperator:=TpvVectorGraphicsFilterCompositeOperator.In_;
    end else if Text='out' then begin
     Primitive.CompositeOperator:=TpvVectorGraphicsFilterCompositeOperator.Out_;
    end else if Text='atop' then begin
     Primitive.CompositeOperator:=TpvVectorGraphicsFilterCompositeOperator.Atop;
    end else if Text='xor' then begin
     Primitive.CompositeOperator:=TpvVectorGraphicsFilterCompositeOperator.Xor_;
    end else if Text='arithmetic' then begin
     Primitive.CompositeOperator:=TpvVectorGraphicsFilterCompositeOperator.Arithmetic;
    end;

    Primitive.K1:=ParseValue(TpvUTF8String(SubTag.GetParameter('k1','0')),1.0);
    Primitive.K2:=ParseValue(TpvUTF8String(SubTag.GetParameter('k2','0')),1.0);
    Primitive.K3:=ParseValue(TpvUTF8String(SubTag.GetParameter('k3','0')),1.0);
    Primitive.K4:=ParseValue(TpvUTF8String(SubTag.GetParameter('k4','0')),1.0);

   end;

   TpvVectorGraphicsFilterPrimitiveKind.ConvolveMatrix:begin

    ParsePair(TpvUTF8String(SubTag.GetParameter('order','3')),Saturation,Angle);
    Primitive.OrderX:=Max(1,round(Saturation));
    Primitive.OrderY:=Max(1,round(Angle));

    ParseNumberList(TpvUTF8String(SubTag.GetParameter('kernelMatrix','')));
    SetLength(Numbers,CountNumbers);
    Primitive.SetKernel(Numbers);

    Primitive.Divisor:=ParseValue(TpvUTF8String(SubTag.GetParameter('divisor','0')),1.0);
    Primitive.Bias:=ParseValue(TpvUTF8String(SubTag.GetParameter('bias','0')),1.0);

    // A missing target sits in the middle of the kernel.
    Text:=TrimText(TpvUTF8String(SubTag.GetParameter('targetX','')));
    if Length(Text)>0 then begin
     Primitive.TargetX:=round(ParseValue(Text));
    end else begin
     Primitive.TargetX:=Primitive.OrderX div 2;
    end;
    Text:=TrimText(TpvUTF8String(SubTag.GetParameter('targetY','')));
    if Length(Text)>0 then begin
     Primitive.TargetY:=round(ParseValue(Text));
    end else begin
     Primitive.TargetY:=Primitive.OrderY div 2;
    end;

    Text:=TpvUTF8String(LowerCase(Trim(String(SubTag.GetParameter('edgeMode','duplicate')))));
    if Text='wrap' then begin
     Primitive.EdgeMode:=TpvVectorGraphicsFilterEdgeMode.Wrap;
    end else if Text='none' then begin
     Primitive.EdgeMode:=TpvVectorGraphicsFilterEdgeMode.NoneEdge;
    end;

    Primitive.PreserveAlpha:=LowerCase(Trim(String(SubTag.GetParameter('preserveAlpha','false'))))='true';

   end;

   TpvVectorGraphicsFilterPrimitiveKind.DiffuseLighting,
   TpvVectorGraphicsFilterPrimitiveKind.SpecularLighting:begin

    Primitive.SurfaceScale:=ParseValue(TpvUTF8String(SubTag.GetParameter('surfaceScale','1')),1.0);
    Primitive.DiffuseConstant:=ParseValue(TpvUTF8String(SubTag.GetParameter('diffuseConstant','1')),1.0);
    Primitive.SpecularConstant:=ParseValue(TpvUTF8String(SubTag.GetParameter('specularConstant','1')),1.0);
    Primitive.SpecularExponent:=ParseValue(TpvUTF8String(SubTag.GetParameter('specularExponent','1')),1.0);
    Primitive.LightColor:=ParseColor(TpvUTF8String(SubTag.GetParameter('lighting-color','white')),aState.CurrentColor,IsNone);
    ParseLight(SubTag,Primitive);

   end;

   TpvVectorGraphicsFilterPrimitiveKind.DisplacementMap:begin

    Primitive.Scale:=ParseValue(TpvUTF8String(SubTag.GetParameter('scale','0')),1.0);

    Text:=TpvUTF8String(UpperCase(Trim(String(SubTag.GetParameter('xChannelSelector','A')))));
    if Text='R' then begin
     Primitive.ChannelSelectorX:=TpvVectorGraphicsFilterChannel.ChannelR;
    end else if Text='G' then begin
     Primitive.ChannelSelectorX:=TpvVectorGraphicsFilterChannel.ChannelG;
    end else if Text='B' then begin
     Primitive.ChannelSelectorX:=TpvVectorGraphicsFilterChannel.ChannelB;
    end;

    Text:=TpvUTF8String(UpperCase(Trim(String(SubTag.GetParameter('yChannelSelector','A')))));
    if Text='R' then begin
     Primitive.ChannelSelectorY:=TpvVectorGraphicsFilterChannel.ChannelR;
    end else if Text='G' then begin
     Primitive.ChannelSelectorY:=TpvVectorGraphicsFilterChannel.ChannelG;
    end else if Text='B' then begin
     Primitive.ChannelSelectorY:=TpvVectorGraphicsFilterChannel.ChannelB;
    end;

   end;

   TpvVectorGraphicsFilterPrimitiveKind.DropShadow:begin

    ParsePair(TpvUTF8String(SubTag.GetParameter('stdDeviation','2')),Saturation,Angle);
    Primitive.StandardDeviationX:=Saturation;
    Primitive.StandardDeviationY:=Angle;
    Primitive.DeltaX:=ParseValue(TpvUTF8String(SubTag.GetParameter('dx','2')),1.0);
    Primitive.DeltaY:=ParseValue(TpvUTF8String(SubTag.GetParameter('dy','2')),1.0);
    Primitive.FloodColor:=ParseColor(TpvUTF8String(SubTag.GetParameter('flood-color','black')),aState.CurrentColor,IsNone);
    Primitive.FloodOpacity:=ParseValue(TpvUTF8String(SubTag.GetParameter('flood-opacity','1')),1.0);

   end;

   TpvVectorGraphicsFilterPrimitiveKind.Flood:begin

    Primitive.FloodColor:=ParseColor(TpvUTF8String(SubTag.GetParameter('flood-color','black')),aState.CurrentColor,IsNone);
    if IsNone then begin
     Primitive.FloodColor:=0;
    end;
    Primitive.FloodOpacity:=ParseValue(TpvUTF8String(SubTag.GetParameter('flood-opacity','1')),1.0);

   end;

   TpvVectorGraphicsFilterPrimitiveKind.GaussianBlur:begin
    ParsePair(TpvUTF8String(SubTag.GetParameter('stdDeviation','0')),Saturation,Angle);
    Primitive.StandardDeviationX:=Saturation;
    Primitive.StandardDeviationY:=Angle;
   end;

   TpvVectorGraphicsFilterPrimitiveKind.Image:begin
    // The image is loaded through the same path an image element takes, and the pixels are kept
    // in a buffer of their own because the filter unit knows nothing about the document.
    Text:=TpvUTF8String(SubTag.GetParameter('xlink:href',TpvRawByteString(SubTag.GetParameter('href',''))));
    Primitive.Image:=LoadImageBuffer(Text);
   end;

   TpvVectorGraphicsFilterPrimitiveKind.Merge:begin

    for SubIndex:=0 to SubTag.Items.Count-1 do begin
     if SubTag.Items[SubIndex] is TpvXMLTag then begin
      FunctionTag:=TpvXMLTag(SubTag.Items[SubIndex]);
      if LowerCase(String(FunctionTag.Name))='femergenode' then begin
       Primitive.AddMergeInput(TpvUTF8String(FunctionTag.GetParameter('in','')));
      end;
     end;
    end;

   end;

   TpvVectorGraphicsFilterPrimitiveKind.Morphology:begin

    if LowerCase(Trim(String(SubTag.GetParameter('operator','erode'))))='dilate' then begin
     Primitive.MorphologyOperator:=TpvVectorGraphicsFilterMorphologyOperator.Dilate;
    end;
    ParsePair(TpvUTF8String(SubTag.GetParameter('radius','0')),Saturation,Angle);
    Primitive.RadiusX:=Saturation;
    Primitive.RadiusY:=Angle;

   end;

   TpvVectorGraphicsFilterPrimitiveKind.Offset:begin
    Primitive.DeltaX:=ParseValue(TpvUTF8String(SubTag.GetParameter('dx','0')),1.0);
    Primitive.DeltaY:=ParseValue(TpvUTF8String(SubTag.GetParameter('dy','0')),1.0);
   end;

   TpvVectorGraphicsFilterPrimitiveKind.Turbulence:begin

    ParsePair(TpvUTF8String(SubTag.GetParameter('baseFrequency','0')),Saturation,Angle);
    Primitive.BaseFrequencyX:=Saturation;
    Primitive.BaseFrequencyY:=Angle;
    Primitive.NumOctaves:=Max(1,round(ParseValue(TpvUTF8String(SubTag.GetParameter('numOctaves','1')))));
    Primitive.Seed:=round(ParseValue(TpvUTF8String(SubTag.GetParameter('seed','0'))));
    Primitive.StitchTiles:=LowerCase(Trim(String(SubTag.GetParameter('stitchTiles','noStitch'))))='stitch';
    Primitive.FractalNoise:=LowerCase(Trim(String(SubTag.GetParameter('type','turbulence'))))='fractalnoise';

   end;

   else begin
   end;

  end;

 end;

end;

function TpvVectorGraphicsSVGLoader.HasAnimationChildren(const aTag:TpvXMLTag):boolean;
var Index:TpvSizeInt;
    TagName:TpvUTF8String;
begin

 result:=false;

 for Index:=0 to aTag.Items.Count-1 do begin
  if aTag.Items[Index] is TpvXMLTag then begin
   TagName:=TpvUTF8String(LowerCase(String(TpvXMLTag(aTag.Items[Index]).Name)));
   if (TagName='animate') or (TagName='animatecolor') or (TagName='animatetransform') or
      (TagName='animatemotion') or (TagName='set') then begin
    result:=true;
    exit;
   end;
  end;
 end;

end;

procedure TpvVectorGraphicsSVGLoader.FinishGraphic(const aTag:TpvXMLTag;const aGraphic:TpvVectorGraphicsGraphic;
                                                   const aState:TState);
var SimpleMatrix:TpvVectorGraphicsSimpleMatrix;
begin

 if HasAnimationChildren(aTag) then begin

  // The transform has to stay on the object here, because an animated translation or rotation is
  // given in the element's own user space and would mean something else once the geometry had
  // been moved into document space. ApplyStateToGraphic resets the matrix, so it goes on after.
  ApplyStateToGraphic(aGraphic,aState,TpvVectorPathMatrix.CreateIdentity);
  SimpleMatrix:=TpvVectorGraphicsSimpleMatrix.CreateDefault;
  SimpleMatrix.Matrix:=aState.Matrix;
  aGraphic.SimpleMatrix:=SimpleMatrix;

 end else begin

  aGraphic.Path.Transform(aState.Matrix);
  ApplyStateToGraphic(aGraphic,aState,aState.Matrix);

 end;

end;

procedure TpvVectorGraphicsSVGLoader.ProcessAnimations(const aTag:TpvXMLTag;const aObject:TpvVectorGraphicsObject;
                                                       const aState:TState);
var Index:TpvSizeInt;
    SubTag:TpvXMLTag;
    TagName:TpvUTF8String;
    TimeLine:TpvVectorGraphicsTimeLine;

 // Appends a key and hands back a pointer to it, with everything that is not set afterwards
 // already at the value that means "this key does not touch it".
 function AddKey(const aAction:TpvVectorGraphicsTimeLineAction;
                 const aStartTick,aEndTick:TpvInt64):PpvVectorGraphicsTimeLineItem;
 var Count:TpvSizeInt;
 begin
  Count:=Length(TimeLine);
  SetLength(TimeLine,Count+1);
  result:=@TimeLine[Count];
  result^.StartTick:=aStartTick;
  result^.EndTick:=aEndTick;
  result^.Action:=aAction;
  result^.InterpolationMode:=TpvVectorGraphicsInterpolationMode.Linear;
  result^.TransformSimpleMatrix:=TpvVectorGraphicsSimpleMatrix.CreateDefault;
  result^.HasAlpha:=false;
  result^.Alpha:=255;
  result^.HasFillColor:=false;
  result^.FillColor:=0;
  result^.HasStrokeColor:=false;
  result^.StrokeColor:=0;
  result^.HasStrokeWidth:=false;
  result^.StrokeWidth:=0.0;
  result^.HasFillSimpleMatrix:=false;
  result^.FillSimpleMatrix:=TpvVectorGraphicsSimpleMatrix.CreateDefault;
  result^.HasStrokeFillSimpleMatrix:=false;
  result^.StrokeFillSimpleMatrix:=TpvVectorGraphicsSimpleMatrix.CreateDefault;
  result^.HasValue:=false;
  result^.ValueName:='';
  result^.Value:=0.0;
  result^.SplineControlPoints[0]:=0.0;
  result^.SplineControlPoints[1]:=0.0;
  result^.SplineControlPoints[2]:=1.0;
  result^.SplineControlPoints[3]:=1.0;
  result^.RepeatCount:=0;
  result^.RepeatOrigin:=0;
  result^.RepeatPeriod:=0;
  result^.Freeze:=true;
  result^.MorphPath:=nil;
 end;

 // One animation element, turned into however many keys its list of values needs.
 procedure ProcessOne(const aAnimationTag:TpvXMLTag;const aKind:TpvUTF8String);
 var AttributeName,TypeName,Text,Token,CalcMode:TpvUTF8String;
     Values:array of TpvUTF8String;
     KeyTimes,Splines:array of TpvDouble;
     CountValues,CountKeyTimes,CountSplines,ValueIndex,SplineBase:TpvSizeInt;
     BeginTick,DurationTicks,StartTick:TpvInt64;
     RepeatCount:TpvInt32;
     Freeze,IsColor,IsTransform:boolean;
     Mode:TpvVectorGraphicsInterpolationMode;
     Item:PpvVectorGraphicsTimeLineItem;

  // Reads one of the transform forms into the parts of a simple matrix, since animateTransform
  // names the kind rather than handing over a matrix.
  procedure ApplyTransformValue(const aItem:PpvVectorGraphicsTimeLineItem;const aText:TpvUTF8String);
  var Rest,Number:TpvUTF8String;
      Numbers:array[0..2] of TpvDouble;
      NumberCount:TpvSizeInt;
  begin

   Rest:=TrimText(aText);
   NumberCount:=0;
   while (Length(Rest)>0) and (NumberCount<3) do begin
    Number:=TrimText(ParseToken(Rest,#9#10#13' ,'));
    if Length(Number)=0 then begin
     continue;
    end;
    Numbers[NumberCount]:=ParseValue(Number);
    inc(NumberCount);
   end;

   if NumberCount=0 then begin
    exit;
   end;

   // Keeping the parts of the transform apart is exactly what makes this work: a rotation
   // interpolates as an angle rather than as six matrix entries.
   if TypeName='scale' then begin
    aItem^.TransformSimpleMatrix.ScaleX:=Numbers[0];
    if NumberCount>=2 then begin
     aItem^.TransformSimpleMatrix.ScaleY:=Numbers[1];
    end else begin
     aItem^.TransformSimpleMatrix.ScaleY:=Numbers[0];
    end;
   end else if TypeName='rotate' then begin
    aItem^.TransformSimpleMatrix.RotationDegrees:=Numbers[0];
    // The optional center of rotation becomes the offset the parts turn around.
    if NumberCount>=3 then begin
     aObject.OffsetX:=Numbers[1];
     aObject.OffsetY:=Numbers[2];
    end;
   end else if TypeName='skewx' then begin
    aItem^.TransformSimpleMatrix.SkewX:=Numbers[0];
   end else if TypeName='skewy' then begin
    aItem^.TransformSimpleMatrix.SkewY:=Numbers[0];
   end else begin
    // translate, which is also what an unknown kind falls back to.
    aItem^.TransformSimpleMatrix.TranslateX:=Numbers[0];
    if NumberCount>=2 then begin
     aItem^.TransformSimpleMatrix.TranslateY:=Numbers[1];
    end;
   end;

  end;

  // Writes one value of the list into a key, according to what is being animated.
  procedure ApplyValue(const aItem:PpvVectorGraphicsTimeLineItem;const aText:TpvUTF8String);
  var IsNone:boolean;
  begin

   if IsTransform then begin

    ApplyTransformValue(aItem,aText);

   end else if AttributeName='opacity' then begin

    aItem^.HasAlpha:=true;
    aItem^.Alpha:=Min(Max(round(ParseValue(aText,1.0)*255.0),0),255);

   end else if AttributeName='fill' then begin

    aItem^.HasFillColor:=true;
    aItem^.FillColor:=ParseColor(aText,aState.CurrentColor,IsNone);
    if IsNone then begin
     aItem^.FillColor:=0;
    end;

   end else if AttributeName='stroke' then begin

    aItem^.HasStrokeColor:=true;
    aItem^.StrokeColor:=ParseColor(aText,aState.CurrentColor,IsNone);
    if IsNone then begin
     aItem^.StrokeColor:=0;
    end;

   end else if AttributeName='stroke-width' then begin

    aItem^.HasStrokeWidth:=true;
    aItem^.StrokeWidth:=ParseValue(aText,Max(fViewportWidth,fViewportHeight));

   end else begin

    // Everything else goes through the numeric parameter key, which the geometry reads back by
    // name when it rebuilds itself.
    aItem^.HasValue:=true;
    aItem^.ValueName:=AttributeName;
    aItem^.Value:=ParseValue(aText,Max(fViewportWidth,fViewportHeight));

   end;

  end;

  // Sets everything that every key of this animation shares.
  procedure ApplyTiming(const aItem:PpvVectorGraphicsTimeLineItem);
  begin
   if IsTransform then begin
    aItem^.Action:=TpvVectorGraphicsTimeLineAction.Transform;
    // A transform key replaces the whole simple matrix, so it has to start out as the one the
    // object already carries, otherwise the inherited transform sitting in its matrix part
    // would be dropped the moment the animation takes over.
    aItem^.TransformSimpleMatrix:=aObject.SimpleMatrix;
   end;
   aItem^.Freeze:=Freeze;
   aItem^.RepeatCount:=RepeatCount;
   aItem^.RepeatOrigin:=StartTick;
   aItem^.RepeatPeriod:=DurationTicks;
  end;

 begin

  AttributeName:=TpvUTF8String(LowerCase(Trim(String(aAnimationTag.GetParameter('attributeName','')))));
  TypeName:=TpvUTF8String(LowerCase(Trim(String(aAnimationTag.GetParameter('type','translate')))));
  IsTransform:=aKind='animatetransform';
  IsColor:=(AttributeName='fill') or (AttributeName='stroke');

  // Without a name there is nothing to animate, except for a transform, which says what it
  // changes through its type instead.
  if (Length(AttributeName)=0) and not IsTransform then begin
   exit;
  end;

  // Only the declarative form of begin is understood. An event or a syncbase reference means the
  // animation never starts here, which is better than starting it at the wrong moment.
  Text:=TrimText(TpvUTF8String(aAnimationTag.GetParameter('begin','0')));
  if Length(Text)=0 then begin
   BeginTick:=0;
  end else if not ParseClockValue(Text,BeginTick) then begin
   exit;
  end;

  // A set element does not blend at all, it lands on its value and stays there, which is a span
  // of no length whatever duration it may carry.
  if (aKind='set') or
     not ParseClockValue(TpvUTF8String(aAnimationTag.GetParameter('dur','')),DurationTicks) then begin
   DurationTicks:=0;
  end;
  if DurationTicks<0 then begin
   DurationTicks:=0;
  end;

  Text:=TpvUTF8String(LowerCase(Trim(String(aAnimationTag.GetParameter('repeatCount','1')))));
  if Text='indefinite' then begin
   RepeatCount:=-1;
  end else begin
   RepeatCount:=Max(1,round(ParseValue(Text)))-1;
  end;

  // A set element holds its value by nature, everything else drops it again unless told to
  // freeze, which is what the SMIL default of remove means.
  Freeze:=(aKind='set') or
          (LowerCase(Trim(String(aAnimationTag.GetParameter('fill','remove'))))='freeze');

  // additive="sum" would have to add to the base rather than replace it, which the timeline does
  // not express, so such an animation is left out instead of being shown wrongly.
  if LowerCase(Trim(String(aAnimationTag.GetParameter('additive','replace'))))='sum' then begin
   exit;
  end;

  CalcMode:=TpvUTF8String(LowerCase(Trim(String(aAnimationTag.GetParameter('calcMode','linear')))));
  if CalcMode='discrete' then begin
   Mode:=TpvVectorGraphicsInterpolationMode.Nearest;
  end else if CalcMode='spline' then begin
   Mode:=TpvVectorGraphicsInterpolationMode.CubicBezier;
  end else begin
   // paced lands here as well: it only differs from linear for values with several components,
   // and for the single numbers and colors handled here the two agree.
   Mode:=TpvVectorGraphicsInterpolationMode.Linear;
  end;

  // The list of values, either given outright or built from the from, to and by attributes.
  Values:=nil;
  CountValues:=0;
  Text:=TpvUTF8String(aAnimationTag.GetParameter('values',''));
  if Length(TrimText(Text))>0 then begin

   while Length(Text)>0 do begin
    Token:=TrimText(ParseToken(Text,';',false));
    if Length(Token)=0 then begin
     continue;
    end;
    SetLength(Values,CountValues+1);
    Values[CountValues]:=Token;
    inc(CountValues);
   end;

  end else begin

   Text:=TrimText(TpvUTF8String(aAnimationTag.GetParameter('from','')));
   if Length(Text)>0 then begin
    SetLength(Values,CountValues+1);
    Values[CountValues]:=Text;
    inc(CountValues);
   end;

   Text:=TrimText(TpvUTF8String(aAnimationTag.GetParameter('to','')));
   if Length(Text)=0 then begin
    // by is a relative offset, which only means anything for plain numbers, and there the value
    // it departs from is already known at this point.
    Token:=TrimText(TpvUTF8String(aAnimationTag.GetParameter('by','')));
    if (Length(Token)>0) and (CountValues>0) and not (IsColor or IsTransform) then begin
     Text:=TpvUTF8String(ConvertDoubleToString(ParseValue(Values[0])+ParseValue(Token)));
    end;
   end;
   if Length(Text)>0 then begin
    SetLength(Values,CountValues+1);
    Values[CountValues]:=Text;
    inc(CountValues);
   end;

  end;

  if CountValues=0 then begin
   exit;
  end;

  // The times the values sit at, as fractions of the duration, evenly spaced unless stated.
  KeyTimes:=nil;
  CountKeyTimes:=0;
  Text:=TpvUTF8String(aAnimationTag.GetParameter('keyTimes',''));
  while Length(Text)>0 do begin
   Token:=TrimText(ParseToken(Text,';',false));
   if Length(Token)=0 then begin
    continue;
   end;
   SetLength(KeyTimes,CountKeyTimes+1);
   KeyTimes[CountKeyTimes]:=ParseValue(Token);
   inc(CountKeyTimes);
  end;
  if CountKeyTimes<>CountValues then begin
   SetLength(KeyTimes,CountValues);
   CountKeyTimes:=CountValues;
   for ValueIndex:=0 to CountValues-1 do begin
    if Mode=TpvVectorGraphicsInterpolationMode.Nearest then begin
     // Discrete gives each value a slice of its own, so the values divide the duration rather
     // than sitting at the ends of the spans between them.
     KeyTimes[ValueIndex]:=ValueIndex/CountValues;
    end else if CountValues>1 then begin
     KeyTimes[ValueIndex]:=ValueIndex/(CountValues-1);
    end else begin
     KeyTimes[ValueIndex]:=0.0;
    end;
   end;
  end;

  // Four numbers per interval between two values, so one easing curve for each span. The
  // semicolons between the groups are turned into plain separators, since the grouping is
  // already given by there being four of them.
  Splines:=nil;
  CountSplines:=0;
  Text:=TpvUTF8String(StringReplace(String(aAnimationTag.GetParameter('keySplines','')),';',' ',[rfReplaceAll]));
  while Length(Text)>0 do begin
   Token:=TrimText(ParseToken(Text,#9#10#13' ,'));
   if Length(Token)=0 then begin
    continue;
   end;
   SetLength(Splines,CountSplines+1);
   Splines[CountSplines]:=ParseValue(Token);
   inc(CountSplines);
  end;

  StartTick:=BeginTick;

  // A single value is a jump to it, which is what a set element is and what an animation with
  // only a to attribute comes down to as well.
  if CountValues=1 then begin
   Item:=AddKey(TpvVectorGraphicsTimeLineAction.Move,StartTick,StartTick+DurationTicks);
   ApplyTiming(Item);
   Item^.InterpolationMode:=Mode;
   ApplyValue(Item,Values[0]);
   exit;
  end;

  // Discrete does not blend, so every value becomes a jump of its own at the moment its slice
  // begins, rather than one span reaching towards the next value.
  if Mode=TpvVectorGraphicsInterpolationMode.Nearest then begin
   for ValueIndex:=0 to CountValues-1 do begin
    Item:=AddKey(TpvVectorGraphicsTimeLineAction.Move,
                 StartTick+round(KeyTimes[ValueIndex]*DurationTicks),
                 StartTick+round(KeyTimes[ValueIndex]*DurationTicks));
    ApplyTiming(Item);
    ApplyValue(Item,Values[ValueIndex]);
   end;
   exit;
  end;

  // The first value lands the moment the animation starts, so that it departs from there and not
  // from whatever the object happens to carry.
  Item:=AddKey(TpvVectorGraphicsTimeLineAction.Move,StartTick,StartTick);
  ApplyTiming(Item);
  ApplyValue(Item,Values[0]);

  // And then one span per interval between two values. They all share the origin and the period,
  // so that a repeat takes the whole sequence rather than each span on its own.
  for ValueIndex:=1 to CountValues-1 do begin

   Item:=AddKey(TpvVectorGraphicsTimeLineAction.Move,
                StartTick+round(KeyTimes[ValueIndex-1]*DurationTicks),
                StartTick+round(KeyTimes[ValueIndex]*DurationTicks));
   ApplyTiming(Item);
   Item^.InterpolationMode:=Mode;

   // One easing curve per interval, with the first one standing in when there are too few.
   if (Mode=TpvVectorGraphicsInterpolationMode.CubicBezier) and (CountSplines>=4) then begin
    SplineBase:=(ValueIndex-1)*4;
    if (SplineBase+3)>=CountSplines then begin
     SplineBase:=0;
    end;
    Item^.SplineControlPoints[0]:=Splines[SplineBase];
    Item^.SplineControlPoints[1]:=Splines[SplineBase+1];
    Item^.SplineControlPoints[2]:=Splines[SplineBase+2];
    Item^.SplineControlPoints[3]:=Splines[SplineBase+3];
   end;

   ApplyValue(Item,Values[ValueIndex]);

  end;

 end;

 // An animateMotion moves the object along a path, which becomes a run of transform keys with the
 // position sampled along it and, where asked for, the direction of travel as the rotation.
 procedure ProcessMotion(const aAnimationTag:TpvXMLTag);
 var PathData,Rotate,Reference,Text:TpvUTF8String;
     Path:TpvVectorPath;
     Shape:TpvVectorPathShape;
     Contour:TpvVectorPathContour;
     Segment:TpvVectorPathSegment;
     SegmentLine:TpvVectorPathSegmentLine;
     Points:array of TpvVectorPathVector;
     Lengths:array of TpvDouble;
     CountPoints,SampleCount,SampleIndex,PointIndex,SubIndex:TpvSizeInt;
     TotalLength,Target,SegmentLength,Fraction,Angle:TpvDouble;
     Position:TpvVectorPathVector;
     BeginTick,DurationTicks,SampleTick,PreviousTick:TpvInt64;
     RepeatCount:TpvInt32;
     Freeze,AutoRotate,ReverseRotate:boolean;
     Item:PpvVectorGraphicsTimeLineItem;
     ReferencedObject:TpvVectorGraphicsObject;
     SubTag:TpvXMLTag;

  // Fills in one key from the sample at the given fraction along the path.
  procedure PlaceSample(const aItem:PpvVectorGraphicsTimeLineItem;const aFraction:TpvDouble);
  var Walked:TpvDouble;
  begin

   Target:=aFraction*TotalLength;

   // Walk to the segment the sample falls on and place it along that one.
   PointIndex:=1;
   while (PointIndex<(CountPoints-1)) and (Lengths[PointIndex]<Target) do begin
    inc(PointIndex);
   end;

   SegmentLength:=Lengths[PointIndex]-Lengths[PointIndex-1];
   if SegmentLength>1e-12 then begin
    Walked:=(Target-Lengths[PointIndex-1])/SegmentLength;
   end else begin
    Walked:=0.0;
   end;

   Position.x:=Points[PointIndex-1].x+((Points[PointIndex].x-Points[PointIndex-1].x)*Walked);
   Position.y:=Points[PointIndex-1].y+((Points[PointIndex].y-Points[PointIndex-1].y)*Walked);

   // The motion comes on top of whatever the object already carries, so the key starts out as
   // its simple matrix and only the translation and the rotation are replaced.
   aItem^.TransformSimpleMatrix:=aObject.SimpleMatrix;
   aItem^.TransformSimpleMatrix.TranslateX:=Position.x;
   aItem^.TransformSimpleMatrix.TranslateY:=Position.y;

   if AutoRotate then begin
    Angle:=ArcTan2(Points[PointIndex].y-Points[PointIndex-1].y,
                   Points[PointIndex].x-Points[PointIndex-1].x)*(180.0/PI);
    if ReverseRotate then begin
     Angle:=Angle+180.0;
    end;
    aItem^.TransformSimpleMatrix.RotationDegrees:=Angle;
   end else begin
    aItem^.TransformSimpleMatrix.RotationDegrees:=ParseValue(Rotate);
   end;

   aItem^.Freeze:=Freeze;
   aItem^.RepeatCount:=RepeatCount;
   aItem^.RepeatOrigin:=BeginTick;
   aItem^.RepeatPeriod:=DurationTicks;

  end;

 begin

  // The path either stands on the element itself or is pointed at through an mpath child.
  Path:=nil;
  Shape:=nil;
  try

   PathData:=TrimText(TpvUTF8String(aAnimationTag.GetParameter('path','')));
   if Length(PathData)>0 then begin

    Path:=TpvVectorPath.CreateFromSVGPath(TpvRawByteString(PathData));

   end else begin

    for SubIndex:=0 to aAnimationTag.Items.Count-1 do begin
     if aAnimationTag.Items[SubIndex] is TpvXMLTag then begin
      SubTag:=TpvXMLTag(aAnimationTag.Items[SubIndex]);
      if LowerCase(String(SubTag.Name))='mpath' then begin
       Reference:=TpvUTF8String(SubTag.GetParameter('xlink:href',TpvRawByteString(SubTag.GetParameter('href',''))));
       if (Length(Reference)>0) and (Reference[1]='#') then begin
        ReferencedObject:=fDocument.FindByID(Copy(Reference,2,Length(Reference)));
        if ReferencedObject is TpvVectorGraphicsGraphic then begin
         Path:=TpvVectorPath.Create;
         Path.Assign(TpvVectorGraphicsGraphic(ReferencedObject).Path);
        end;
       end;
       break;
      end;
     end;
    end;

   end;

   if not assigned(Path) then begin
    exit;
   end;

   // Everything below works on the flattened outline, so the sampling only has to walk straight
   // segments and the distance along the path is a plain sum.
   Shape:=TpvVectorPathShape.Create(Path);
   Shape.ConvertCurvesToLines(0.25);

   Points:=nil;
   CountPoints:=0;
   for Contour in Shape.Contours do begin
    for Segment in Contour.Segments do begin
     if Segment is TpvVectorPathSegmentLine then begin
      SegmentLine:=TpvVectorPathSegmentLine(Segment);
      if CountPoints=0 then begin
       SetLength(Points,CountPoints+1);
       Points[CountPoints]:=SegmentLine.Points[0];
       inc(CountPoints);
      end;
      SetLength(Points,CountPoints+1);
      Points[CountPoints]:=SegmentLine.Points[1];
      inc(CountPoints);
     end;
    end;
   end;

   if CountPoints<2 then begin
    exit;
   end;

   SetLength(Lengths,CountPoints);
   Lengths[0]:=0.0;
   TotalLength:=0.0;
   for PointIndex:=1 to CountPoints-1 do begin
    TotalLength:=TotalLength+sqrt(sqr(Points[PointIndex].x-Points[PointIndex-1].x)+
                                  sqr(Points[PointIndex].y-Points[PointIndex-1].y));
    Lengths[PointIndex]:=TotalLength;
   end;

   if TotalLength<=1e-12 then begin
    exit;
   end;

   Text:=TrimText(TpvUTF8String(aAnimationTag.GetParameter('begin','0')));
   if Length(Text)=0 then begin
    BeginTick:=0;
   end else if not ParseClockValue(Text,BeginTick) then begin
    exit;
   end;

   if not ParseClockValue(TpvUTF8String(aAnimationTag.GetParameter('dur','')),DurationTicks) then begin
    DurationTicks:=0;
   end;
   if DurationTicks<=0 then begin
    exit;
   end;

   Text:=TpvUTF8String(LowerCase(Trim(String(aAnimationTag.GetParameter('repeatCount','1')))));
   if Text='indefinite' then begin
    RepeatCount:=-1;
   end else begin
    RepeatCount:=Max(1,round(ParseValue(Text)))-1;
   end;

   Freeze:=LowerCase(Trim(String(aAnimationTag.GetParameter('fill','remove'))))='freeze';

   Rotate:=TpvUTF8String(LowerCase(Trim(String(aAnimationTag.GetParameter('rotate','0')))));
   AutoRotate:=(Rotate='auto') or (Rotate='auto-reverse');
   ReverseRotate:=Rotate='auto-reverse';

   // The path becomes a run of keys rather than something sampled at render time, since the
   // timeline only knows how to blend between keys. Enough of them that the corners are kept,
   // and not so many that a long path fills the timeline with noise.
   SampleCount:=Min(256,Max(16,CountPoints));

   // The first sample lands the moment the motion starts, the rest are spans that blend from one
   // to the next, which is the same shape every other animation here has.
   Item:=AddKey(TpvVectorGraphicsTimeLineAction.Transform,BeginTick,BeginTick);
   PlaceSample(Item,0.0);

   PreviousTick:=BeginTick;
   for SampleIndex:=1 to SampleCount-1 do begin
    Fraction:=SampleIndex/(SampleCount-1);
    SampleTick:=BeginTick+round(Fraction*DurationTicks);
    Item:=AddKey(TpvVectorGraphicsTimeLineAction.Transform,PreviousTick,SampleTick);
    Item^.InterpolationMode:=TpvVectorGraphicsInterpolationMode.Linear;
    PlaceSample(Item,Fraction);
    PreviousTick:=SampleTick;
   end;

  finally
   FreeAndNil(Shape);
   FreeAndNil(Path);
  end;

 end;

begin

 if not assigned(aObject) then begin
  exit;
 end;

 TimeLine:=aObject.TimeLine;

 for Index:=0 to aTag.Items.Count-1 do begin

  if not (aTag.Items[Index] is TpvXMLTag) then begin
   continue;
  end;

  SubTag:=TpvXMLTag(aTag.Items[Index]);
  TagName:=TpvUTF8String(LowerCase(String(SubTag.Name)));

  // animateColor is the deprecated spelling of an animate on a paint attribute and behaves the
  // same way, so it goes through the same path.
  if (TagName='animate') or (TagName='animatecolor') or
     (TagName='animatetransform') or (TagName='set') then begin
   ProcessOne(SubTag,TagName);
  end else if TagName='animatemotion' then begin
   ProcessMotion(SubTag);
  end;

 end;

 if Length(TimeLine)>0 then begin

  aObject.TimeLine:=TimeLine;

  // An animated parameter changes the geometry, so whatever was built before no longer stands.
  if aObject is TpvVectorGraphicsGraphic then begin
   TpvVectorGraphicsGraphic(aObject).MarkPathDirty;
  end;

 end;

end;

procedure TpvVectorGraphicsSVGLoader.ResolvePendingPaints;
var Index:TpvSizeInt;
begin

 // Object bounding box units need the boxes, and those need the whole tree, which is why this
 // runs only after everything has been built.
 fDocument.Update;
 fDocument.Defs.CalculateBoundingBox(0,TpvVectorPathMatrix.CreateIdentity);
 fDocument.CalculateBoundingBox(0);

 for Index:=0 to fPendingFillPaints.Count-1 do begin
  ResolvePaint(fPendingFillPaints[Index],fPendingFillPaintIDs[Index],false,fPendingFillPaintMatrices[Index]);
 end;

 for Index:=0 to fPendingStrokePaints.Count-1 do begin
  ResolvePaint(fPendingStrokePaints[Index],fPendingStrokePaintIDs[Index],true,fPendingStrokePaintMatrices[Index]);
 end;

end;

function TpvVectorGraphicsSVGLoader.Load(const aXML:TpvXML;const aDocument:TpvVectorGraphicsDocument):boolean;
var RootTag,Tag:TpvXMLTag;
    Index:TpvSizeInt;
    State:TState;
    Value:TpvUTF8String;
    ViewBoxText,Token:TpvUTF8String;
    ViewBoxValues:array[0..3] of TpvDouble;
    ViewBoxCount:TpvSizeInt;
 procedure CollectStyles(const aItem:TpvXMLItem);
 var SubIndex,TextIndex:TpvSizeInt;
     SubTag:TpvXMLTag;
     Text:TpvUTF8String;
 begin
  for SubIndex:=0 to aItem.Items.Count-1 do begin
   if aItem.Items[SubIndex] is TpvXMLTag then begin
    SubTag:=TpvXMLTag(aItem.Items[SubIndex]);
    if LowerCase(String(SubTag.Name))='style' then begin
     Text:='';
     for TextIndex:=0 to SubTag.Items.Count-1 do begin
      if SubTag.Items[TextIndex] is TpvXMLText then begin
       Text:=Text+TpvUTF8String(TpvXMLText(SubTag.Items[TextIndex]).Text);
      end else if SubTag.Items[TextIndex] is TpvXMLCDataTag then begin
       Text:=Text+TpvUTF8String(TpvXMLCDataTag(SubTag.Items[TextIndex]).Text);
      end;
     end;
     if Length(Text)>0 then begin
      ParseCSS(Text);
     end;
    end else begin
     CollectStyles(SubTag);
    end;
   end;
  end;
 end;
begin

 result:=false;

 if not (assigned(aXML) and assigned(aDocument)) then begin
  exit;
 end;

 fDocument:=aDocument;

 RootTag:=nil;
 for Index:=0 to aXML.Root.Items.Count-1 do begin
  if aXML.Root.Items[Index] is TpvXMLTag then begin
   Tag:=TpvXMLTag(aXML.Root.Items[Index]);
   if LowerCase(String(Tag.Name))='svg' then begin
    RootTag:=Tag;
    break;
   end;
  end;
 end;

 if not assigned(RootTag) then begin
  exit;
 end;

 // The view box has to be read before anything else, since every percentage in the document is
 // relative to the viewport it establishes.
 ViewBoxText:=TpvUTF8String(RootTag.GetParameter('viewBox',''));
 ViewBoxCount:=0;
 while (Length(ViewBoxText)>0) and (ViewBoxCount<4) do begin
  Token:=TrimText(ParseToken(ViewBoxText,#9#10#13' ,'));
  if Length(Token)=0 then begin
   continue;
  end;
  ViewBoxValues[ViewBoxCount]:=ParseValue(Token);
  inc(ViewBoxCount);
 end;

 if ViewBoxCount=4 then begin
  fHasRootViewBox:=true;
  fRootViewBox.x1:=ViewBoxValues[0];
  fRootViewBox.y1:=ViewBoxValues[1];
  fRootViewBox.x2:=ViewBoxValues[0]+ViewBoxValues[2];
  fRootViewBox.y2:=ViewBoxValues[1]+ViewBoxValues[3];
  fViewportWidth:=ViewBoxValues[2];
  fViewportHeight:=ViewBoxValues[3];
 end else begin
  fHasRootViewBox:=false;
  fViewportWidth:=0.0;
  fViewportHeight:=0.0;
 end;

 Value:=TpvUTF8String(RootTag.GetParameter('width',''));
 if Length(Value)>0 then begin
  fDocument.Width:=ParseValue(Value,fViewportWidth);
 end;
 Value:=TpvUTF8String(RootTag.GetParameter('height',''));
 if Length(Value)>0 then begin
  fDocument.Height:=ParseValue(Value,fViewportHeight);
 end;

 // Either of the two may be missing, in which case the other one stands in for it.
 if fViewportWidth<=0.0 then begin
  fViewportWidth:=fDocument.Width;
 end;
 if fViewportHeight<=0.0 then begin
  fViewportHeight:=fDocument.Height;
 end;
 if fDocument.Width<=0.0 then begin
  fDocument.Width:=fViewportWidth;
 end;
 if fDocument.Height<=0.0 then begin
  fDocument.Height:=fViewportHeight;
 end;
 fDocument.SetPreserveAspectRatio(TpvUTF8String(RootTag.GetParameter('preserveAspectRatio','')));

 if fHasRootViewBox then begin
  fDocument.ViewBox:=fRootViewBox;
  fDocument.HasViewBox:=true;
 end else begin
  fDocument.ViewBox:=TpvVectorGraphicsBoundingBox.CreateEmpty;
  fDocument.ViewBox.Extend(0.0,0.0);
  fDocument.ViewBox.Extend(fDocument.Width,fDocument.Height);
  fDocument.HasViewBox:=false;
 end;

 State:=TState.CreateDefault;

 // Style sheets, then the definitions, then the drawing itself: a document may well reference
 // a gradient or a clip path before it defines one.
 CollectStyles(RootTag);
 CollectDefinitions(RootTag,State);
 ProcessChildren(RootTag,fDocument.Root,State);

 // And the paint servers last, since object bounding box units need the boxes.
 ResolvePendingPaints;

 result:=true;

end;

function pvVectorGraphicsLoadSVGFromStream(const aStream:TStream;const aBaseDirectory:string):TpvVectorGraphicsDocument;
var XML:TpvXML;
    Loader:TpvVectorGraphicsSVGLoader;
begin

 result:=nil;

 XML:=TpvXML.Create;
 try

  if not XML.Parse(aStream) then begin
   exit;
  end;

  result:=TpvVectorGraphicsDocument.Create;
  try

   Loader:=TpvVectorGraphicsSVGLoader.Create;
   try
    Loader.BaseDirectory:=aBaseDirectory;
    if not Loader.Load(XML,result) then begin
     FreeAndNil(result);
    end;
   finally
    FreeAndNil(Loader);
   end;

  except
   FreeAndNil(result);
   raise;
  end;

 finally
  FreeAndNil(XML);
 end;

end;

function pvVectorGraphicsLoadSVGFromFile(const aFileName:string):TpvVectorGraphicsDocument;
var Stream:TFileStream;
begin

 Stream:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
 try
  // Relative image references in the file are resolved next to the file itself.
  result:=pvVectorGraphicsLoadSVGFromStream(Stream,ExtractFilePath(ExpandFileName(aFileName)));
 finally
  FreeAndNil(Stream);
 end;

end;

function pvVectorGraphicsLoadSVGFromString(const aText:TpvRawByteString):TpvVectorGraphicsDocument;
var Stream:TMemoryStream;
begin

 Stream:=TMemoryStream.Create;
 try

  if Length(aText)>0 then begin
   Stream.WriteBuffer(aText[1],Length(aText));
  end;
  Stream.Seek(0,soBeginning);

  // No base directory, since a document out of a string has no place to resolve against.
  result:=pvVectorGraphicsLoadSVGFromStream(Stream);

 finally
  FreeAndNil(Stream);
 end;

end;

{ TpvVectorGraphicsSVGWriter }

constructor TpvVectorGraphicsSVGWriter.Create;
begin

 inherited Create;

 fDocument:=nil;
 fXML:=nil;

 // The definitions tag is created on first use, so that a document without paint servers does
 // not get an empty defs element.
 fDefinitionsTag:=nil;
 fPaintServerCounter:=0;

end;

destructor TpvVectorGraphicsSVGWriter.Destroy;
begin
 inherited Destroy;
end;

function TpvVectorGraphicsSVGWriter.FormatValue(const aValue:TpvDouble):TpvRawByteString;
var Text:string;
    Index:TpvSizeInt;
begin

 // Six digits are plenty for coordinates and keep the output from exploding in size, and the
 // trailing zeros go away so that whole numbers stay whole numbers.
 Text:=ConvertDoubleToString(aValue);
 if Pos('.',Text)>0 then begin
  Index:=Length(Text);
  while (Index>0) and (Text[Index]='0') do begin
   dec(Index);
  end;
  if (Index>0) and (Text[Index]='.') then begin
   dec(Index);
  end;
  Text:=Copy(Text,1,Index);
 end;

 // Trimming a bare zero away leaves nothing behind.
 if Length(Text)=0 then begin
  Text:='0';
 end;

 result:=TpvRawByteString(Text);

end;

function TpvVectorGraphicsSVGWriter.FormatColor(const aColor:TpvVectorGraphicsColor):TpvRawByteString;
begin
 result:=TpvRawByteString(LowerCase(Format('#%.2x%.2x%.2x',[(aColor shr 16) and $ff,
                                                            (aColor shr 8) and $ff,
                                                            aColor and $ff])));
end;

function TpvVectorGraphicsSVGWriter.FormatMatrix(const aMatrix:TpvVectorPathMatrix):TpvRawByteString;
begin
 result:='matrix('+FormatValue(aMatrix.m00)+','+FormatValue(aMatrix.m01)+','+
                   FormatValue(aMatrix.m10)+','+FormatValue(aMatrix.m11)+','+
                   FormatValue(aMatrix.m20)+','+FormatValue(aMatrix.m21)+')';
end;

function TpvVectorGraphicsSVGWriter.FormatBlendMode(const aBlendMode:TpvVectorGraphicsBlendMode):TpvRawByteString;
begin

 case aBlendMode of
  TpvVectorGraphicsBlendMode.Multiply:begin
   result:='multiply';
  end;
  TpvVectorGraphicsBlendMode.Screen:begin
   result:='screen';
  end;
  TpvVectorGraphicsBlendMode.Overlay:begin
   result:='overlay';
  end;
  TpvVectorGraphicsBlendMode.Darken:begin
   result:='darken';
  end;
  TpvVectorGraphicsBlendMode.Lighten:begin
   result:='lighten';
  end;
  TpvVectorGraphicsBlendMode.ColorDodge:begin
   result:='color-dodge';
  end;
  TpvVectorGraphicsBlendMode.ColorBurn:begin
   result:='color-burn';
  end;
  TpvVectorGraphicsBlendMode.HardLight:begin
   result:='hard-light';
  end;
  TpvVectorGraphicsBlendMode.SoftLight:begin
   result:='soft-light';
  end;
  TpvVectorGraphicsBlendMode.Difference:begin
   result:='difference';
  end;
  TpvVectorGraphicsBlendMode.Exclusion:begin
   result:='exclusion';
  end;
  TpvVectorGraphicsBlendMode.Hue:begin
   result:='hue';
  end;
  TpvVectorGraphicsBlendMode.Saturation:begin
   result:='saturation';
  end;
  TpvVectorGraphicsBlendMode.Color:begin
   result:='color';
  end;
  TpvVectorGraphicsBlendMode.Luminosity:begin
   result:='luminosity';
  end;
  else {TpvVectorGraphicsBlendMode.Normal:}begin
   result:='';
  end;
 end;

end;

function TpvVectorGraphicsSVGWriter.FormatPathData(const aPath:TpvVectorPath):TpvRawByteString;
var Command:TpvVectorPathCommand;
begin

 result:='';
 if not assigned(aPath) then begin
  exit;
 end;

 // Only the absolute forms are written, since the relative ones save space but gain nothing in
 // readability, and the path parser understands both anyway.
 for Command in aPath.Commands do begin

  if not assigned(Command) then begin
   continue;
  end;

  if Length(result)>0 then begin
   result:=result+' ';
  end;

  case Command.CommandType of
   TpvVectorPathCommandType.MoveTo:begin
    result:=result+'M '+FormatValue(Command.x0)+' '+FormatValue(Command.y0);
   end;
   TpvVectorPathCommandType.LineTo:begin
    result:=result+'L '+FormatValue(Command.x0)+' '+FormatValue(Command.y0);
   end;
   TpvVectorPathCommandType.QuadraticCurveTo:begin
    result:=result+'Q '+FormatValue(Command.x0)+' '+FormatValue(Command.y0)+' '+
                        FormatValue(Command.x1)+' '+FormatValue(Command.y1);
   end;
   TpvVectorPathCommandType.CubicCurveTo:begin
    result:=result+'C '+FormatValue(Command.x0)+' '+FormatValue(Command.y0)+' '+
                        FormatValue(Command.x1)+' '+FormatValue(Command.y1)+' '+
                        FormatValue(Command.x2)+' '+FormatValue(Command.y2);
   end;
   TpvVectorPathCommandType.Close:begin
    result:=result+'Z';
   end;
   else begin
   end;
  end;

 end;

end;

function TpvVectorGraphicsSVGWriter.EnsureDefinitionsTag:TpvXMLTag;
var RootTag:TpvXMLTag;
begin

 if not assigned(fDefinitionsTag) then begin

  RootTag:=TpvXMLTag(fXML.Root.Items[fXML.Root.Items.Count-1]);
  fDefinitionsTag:=TpvXMLTag.Create;
  fDefinitionsTag.Name:='defs';

  // The defs have to come first, since a paint server must be defined before it is referenced
  // for the file to be readable by tools that do not look ahead.
  RootTag.Items.Insert(0,fDefinitionsTag);

 end;

 result:=fDefinitionsTag;

end;

function TpvVectorGraphicsSVGWriter.WritePaintServer(const aFillData:TpvVectorGraphicsFillData):TpvRawByteString;
var Tag,StopTag:TpvXMLTag;
    Identifier:TpvRawByteString;
    Inverse:TpvVectorPathMatrix;
    StartPoint,EndPoint,Center,Focus:TpvVectorPathVector;
    Index:TpvSizeInt;
begin

 result:='';

 // Patterns and images are not written as paint servers, so an empty result means the caller has
 // to fall back to a plain color.
 case aFillData.Mode of

  TpvVectorGraphicsFillMode.LinearGradient,
  TpvVectorGraphicsFillMode.RadialGradient:begin

   if Length(aFillData.GradientStops)=0 then begin
    exit;
   end;

   inc(fPaintServerCounter);
   Identifier:=TpvRawByteString('pvpaint'+IntToStr(fPaintServerCounter));
   Tag:=TpvXMLTag.Create;

   // The stored matrix maps user space into the gradient's canonical space, so inverting it
   // gives back the axis or the circle the file has to state.
   Inverse:=aFillData.SimpleMatrix.ToMatrix.Inverse;

   if aFillData.Mode=TpvVectorGraphicsFillMode.LinearGradient then begin
    Tag.Name:='linearGradient';
    StartPoint:=Inverse.Transform(0.0,0.0);
    EndPoint:=Inverse.Transform(aFillData.GradientRadius,0.0);
    Tag.AddParameter('x1',FormatValue(StartPoint.x));
    Tag.AddParameter('y1',FormatValue(StartPoint.y));
    Tag.AddParameter('x2',FormatValue(EndPoint.x));
    Tag.AddParameter('y2',FormatValue(EndPoint.y));
   end else begin
    Tag.Name:='radialGradient';
    Center:=Inverse.Transform(0.0,0.0);
    Focus:=Inverse.Transform(aFillData.GradientFocusX,aFillData.GradientFocusY);
    Tag.AddParameter('cx',FormatValue(Center.x));
    Tag.AddParameter('cy',FormatValue(Center.y));
    Tag.AddParameter('fx',FormatValue(Focus.x));
    Tag.AddParameter('fy',FormatValue(Focus.y));
    // The radius is in the gradient's own space, where the inverse is not a plain scale in
    // general, so the length of the transformed unit x axis gives the matching user space one.
    EndPoint:=Inverse.Transform(aFillData.GradientRadius,0.0);
    Tag.AddParameter('r',FormatValue(sqrt(sqr(EndPoint.x-Center.x)+sqr(EndPoint.y-Center.y))));
   end;

   // The coordinates above are user units, which is what makes the object bounding box units of
   // the original file unnecessary here.
   Tag.AddParameter('id',Identifier);
   Tag.AddParameter('gradientUnits','userSpaceOnUse');

   case aFillData.SpreadMethod of
    TpvVectorGraphicsSpreadMethod.Reflect:begin
     Tag.AddParameter('spreadMethod','reflect');
    end;
    TpvVectorGraphicsSpreadMethod.Repeat_:begin
     Tag.AddParameter('spreadMethod','repeat');
    end;
    else begin
    end;
   end;

   // The stop alpha was folded into the color on loading and is split out again here.
   for Index:=0 to Length(aFillData.GradientStops)-1 do begin
    StopTag:=TpvXMLTag.Create;
    StopTag.Name:='stop';
    StopTag.AddParameter('offset',FormatValue(aFillData.GradientStops[Index].Offset));
    StopTag.AddParameter('stop-color',FormatColor(aFillData.GradientStops[Index].Color));
    if ((aFillData.GradientStops[Index].Color shr 24) and $ff)<>255 then begin
     StopTag.AddParameter('stop-opacity',FormatValue(((aFillData.GradientStops[Index].Color shr 24) and $ff)/255.0));
    end;
    Tag.Add(StopTag);
   end;

   EnsureDefinitionsTag.Add(Tag);
   result:='url(#'+Identifier+')';

  end;

  else begin
  end;

 end;

end;

procedure TpvVectorGraphicsSVGWriter.WriteStyle(const aTag:TpvXMLTag;const aGraphic:TpvVectorGraphicsGraphic);
var Style:TpvVectorGraphicsStyle;
    Reference:TpvRawByteString;
    DashText:TpvRawByteString;
    Index:TpvSizeInt;
begin

 Style:=aGraphic.Style;

 // Only what differs from the SVG default is written out, which keeps the files small and the
 // diff between a loaded and a rewritten document readable.
 if Style.FillData.Mode=TpvVectorGraphicsFillMode.None then begin

  aTag.AddParameter('fill','none');

 end else begin

  Reference:=WritePaintServer(Style.FillData);
  if Length(Reference)>0 then begin
   aTag.AddParameter('fill',Reference);
  end else begin
   aTag.AddParameter('fill',FormatColor(Style.FillData.Color));
   if ((Style.FillData.Color shr 24) and $ff)<>255 then begin
    aTag.AddParameter('fill-opacity',FormatValue(((Style.FillData.Color shr 24) and $ff)/255.0));
   end;
  end;

  if Style.FillData.FillRule=TpvVectorPathFillRule.EvenOdd then begin
   aTag.AddParameter('fill-rule','evenodd');
  end;

 end;

 if Style.StrokeFillData.Mode<>TpvVectorGraphicsFillMode.None then begin

  Reference:=WritePaintServer(Style.StrokeFillData);
  if Length(Reference)>0 then begin
   aTag.AddParameter('stroke',Reference);
  end else begin
   aTag.AddParameter('stroke',FormatColor(Style.StrokeFillData.Color));
   if ((Style.StrokeFillData.Color shr 24) and $ff)<>255 then begin
    aTag.AddParameter('stroke-opacity',FormatValue(((Style.StrokeFillData.Color shr 24) and $ff)/255.0));
   end;
  end;

  aTag.AddParameter('stroke-width',FormatValue(Style.StrokeWidth));

  case Style.StrokeCapMode of
   TpvVectorPathStrokeCapMode.Round:begin
    aTag.AddParameter('stroke-linecap','round');
   end;
   TpvVectorPathStrokeCapMode.Square:begin
    aTag.AddParameter('stroke-linecap','square');
   end;
   else begin
   end;
  end;

  // The miter limit only matters for a miter join, which is why it lives in that branch.
  case Style.StrokeJoinMode of
   TpvVectorPathStrokeJoinMode.Round:begin
    aTag.AddParameter('stroke-linejoin','round');
   end;
   TpvVectorPathStrokeJoinMode.Bevel:begin
    aTag.AddParameter('stroke-linejoin','bevel');
   end;
   else begin
    if not SameValue(Style.StrokeMiterLimit,4.0) then begin
     aTag.AddParameter('stroke-miterlimit',FormatValue(Style.StrokeMiterLimit));
    end;
   end;
  end;

  if Length(Style.StrokeDashArray)>0 then begin
   DashText:='';
   for Index:=0 to Length(Style.StrokeDashArray)-1 do begin
    if Index>0 then begin
     DashText:=DashText+',';
    end;
    DashText:=DashText+FormatValue(Style.StrokeDashArray[Index]);
   end;
   aTag.AddParameter('stroke-dasharray',DashText);
   if not SameValue(Style.StrokeDashOffset,0.0) then begin
    aTag.AddParameter('stroke-dashoffset',FormatValue(Style.StrokeDashOffset));
   end;
  end;

  if Style.StrokeWidthFixed then begin
   aTag.AddParameter('vector-effect','non-scaling-stroke');
  end;

 end;

 if aGraphic.Alpha<>255 then begin
  aTag.AddParameter('opacity',FormatValue(aGraphic.Alpha/255.0));
 end;

end;

procedure TpvVectorGraphicsSVGWriter.WriteObject(const aObject:TpvVectorGraphicsObject;const aParentTag:TpvXMLTag);
var Tag:TpvXMLTag;
    Group:TpvVectorGraphicsGroup;
    Matrix:TpvVectorPathMatrix;
    Index:TpvSizeInt;
    PathData,BlendModeText,StyleText:TpvRawByteString;
begin

 if not assigned(aObject) then begin
  exit;
 end;

 // Every kind of graphic is written as a path, since its own parameters were baked into the
 // outline on loading and a rectangle or an ellipse could no longer be reconstructed from it.
 Tag:=nil;
 if aObject is TpvVectorGraphicsGroup then begin

  Group:=TpvVectorGraphicsGroup(aObject);
  Tag:=TpvXMLTag.Create;
  Tag.Name:='g';
  aParentTag.Add(Tag);
  for Index:=0 to Group.Children.Count-1 do begin
   WriteObject(Group.Children[Index],Tag);
  end;

 end else if aObject is TpvVectorGraphicsGroupInstance then begin

  // An instance without a target, or one whose target carries no identifier, cannot be written
  // as a use element and is therefore dropped.
  if assigned(TpvVectorGraphicsGroupInstance(aObject).Target) and
     (Length(TpvVectorGraphicsGroupInstance(aObject).Target.ID)>0) then begin
   Tag:=TpvXMLTag.Create;
   Tag.Name:='use';
   Tag.AddParameter('xlink:href',TpvRawByteString('#'+String(TpvVectorGraphicsGroupInstance(aObject).Target.ID)));
   aParentTag.Add(Tag);
  end;

 end else if aObject is TpvVectorGraphicsGraphic then begin

  PathData:=FormatPathData(TpvVectorGraphicsGraphic(aObject).Path);
  if Length(PathData)>0 then begin
   Tag:=TpvXMLTag.Create;
   Tag.Name:='path';
   Tag.AddParameter('d',PathData);
   WriteStyle(Tag,TpvVectorGraphicsGraphic(aObject));
   aParentTag.Add(Tag);
  end;

 end;

 // What every kind of object has in common goes on afterwards.
 if assigned(Tag) then begin

  if Length(aObject.ID)>0 then begin
   Tag.AddParameter('id',TpvRawByteString(aObject.ID));
  end;

  Matrix:=aObject.SimpleMatrix.ToMatrix(aObject.OffsetX,aObject.OffsetY);
  if Matrix<>TpvVectorPathMatrix.CreateIdentity then begin
   Tag.AddParameter('transform',FormatMatrix(Matrix));
  end;

  if not aObject.Visible then begin
   Tag.AddParameter('display','none');
  end;

  // A graphic's opacity was already written by WriteStyle, a group's was not.
  if (aObject is TpvVectorGraphicsGroup) and (aObject.Alpha<>255) then begin
   Tag.AddParameter('opacity',FormatValue(aObject.Alpha/255.0));
  end;

  // Both of these are CSS properties rather than SVG attributes, so they go into the style
  // attribute, which is also where they came from when the document was read.
  StyleText:='';

  BlendModeText:=FormatBlendMode(aObject.BlendMode);
  if Length(BlendModeText)>0 then begin
   StyleText:='mix-blend-mode:'+BlendModeText;
  end;

  if aObject.Isolate then begin
   if Length(StyleText)>0 then begin
    StyleText:=StyleText+';';
   end;
   StyleText:=StyleText+'isolation:isolate';
  end;

  if Length(StyleText)>0 then begin
   Tag.AddParameter('style',StyleText);
  end;

 end;

end;

function TpvVectorGraphicsSVGWriter.Save(const aDocument:TpvVectorGraphicsDocument;const aXML:TpvXML):boolean;
var ProcessTag:TpvXMLProcessTag;
    RootTag:TpvXMLTag;
    Index:TpvSizeInt;
begin

 result:=false;
 if not (assigned(aDocument) and assigned(aXML)) then begin
  exit;
 end;

 fDocument:=aDocument;
 fXML:=aXML;
 fDefinitionsTag:=nil;
 fPaintServerCounter:=0;

 aXML.Root.Clear;

 ProcessTag:=TpvXMLProcessTag.Create;
 ProcessTag.Name:='xml';
 ProcessTag.AddParameter('version','1.0');
 ProcessTag.AddParameter('encoding','UTF-8');
 aXML.Root.Add(ProcessTag);

 RootTag:=TpvXMLTag.Create;
 RootTag.Name:='svg';
 RootTag.AddParameter('xmlns','http://www.w3.org/2000/svg');
 RootTag.AddParameter('xmlns:xlink','http://www.w3.org/1999/xlink');
 RootTag.AddParameter('version','1.1');
 RootTag.AddParameter('width',FormatValue(aDocument.Width));
 RootTag.AddParameter('height',FormatValue(aDocument.Height));

 if aDocument.HasViewBox and not aDocument.ViewBox.IsEmpty then begin
  RootTag.AddParameter('viewBox',FormatValue(aDocument.ViewBox.x1)+' '+
                                 FormatValue(aDocument.ViewBox.y1)+' '+
                                 FormatValue(aDocument.ViewBox.Width)+' '+
                                 FormatValue(aDocument.ViewBox.Height));
 end;

 aXML.Root.Add(RootTag);

 // The defs tree holds the clip paths, markers and pattern tiles, which have to be written
 // before anything can reference them.
 for Index:=0 to aDocument.Defs.Children.Count-1 do begin
  WriteObject(aDocument.Defs.Children[Index],EnsureDefinitionsTag);
 end;

 for Index:=0 to aDocument.Root.Children.Count-1 do begin
  WriteObject(aDocument.Root.Children[Index],RootTag);
 end;

 result:=true;

end;

function pvVectorGraphicsSaveSVGToStream(const aDocument:TpvVectorGraphicsDocument;const aStream:TStream):boolean;
var XML:TpvXML;
    Writer:TpvVectorGraphicsSVGWriter;
begin

 result:=false;

 XML:=TpvXML.Create;
 try

  Writer:=TpvVectorGraphicsSVGWriter.Create;
  try
   if not Writer.Save(aDocument,XML) then begin
    exit;
   end;
  finally
   FreeAndNil(Writer);
  end;

  XML.FormatIndent:=true;
  result:=XML.Write(aStream);

 finally
  FreeAndNil(XML);
 end;

end;

function pvVectorGraphicsSaveSVGToFile(const aDocument:TpvVectorGraphicsDocument;const aFileName:string):boolean;
var Stream:TFileStream;
begin

 Stream:=TFileStream.Create(aFileName,fmCreate);
 try
  result:=pvVectorGraphicsSaveSVGToStream(aDocument,Stream);
 finally
  FreeAndNil(Stream);
 end;

end;

initialization
end.
