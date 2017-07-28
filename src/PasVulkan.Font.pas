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
unit PasVulkan.Font;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

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
     PasVulkan.TrueTypeFont,
     PasVulkan.Sprites;

type TpvFontCodePointBitmap=array of TpvUInt32;

     PpvFontCharacterRange=^TpvFontCharacterRange;
     TpvFontCharacterRange=set of AnsiChar;

     PpvFontCodePointRange=^TpvFontCodePointRange;
     TpvFontCodePointRange=record
      FromCodePoint:TpvUInt32;
      ToCodePoint:TpvUInt32;
     end;

     TpvFontCodePointRanges=array of TpvFontCodePointRange;

     PpvFontGlyph=^TpvFontGlyph;
     TpvFontGlyph=record
      AdvanceWidth:TpvFloat;
      AdvanceHeight:TpvFloat;
      LeftSideBearing:TpvFloat;
      RightSideBearing:TpvFloat;
      TopSideBearing:TpvFloat;
      BottomSideBearing:TpvFloat;
      BoundsMinX:TpvFloat;
      BoundsMinY:TpvFloat;
      BoundsMaxX:TpvFloat;
      BoundsMaxY:TpvFloat;
      OffsetX:TpvFloat;
      OffsetY:TpvFloat;
      Width:TpvInt32;
      Height:TpvInt32;
      Sprite:TpvSprite;
     end;

     TPVulkanFontGlyphs=array of PpvFontGlyph;

     TpvFontGlyphs=array of TpvFontGlyph;

     PpvFontCodePointGlyphPair=^TpvFontCodePointGlyphPair;
     TpvFontCodePointGlyphPair=record
      CodePoint:TpvUInt32;
      Glyph:TpvInt32;
     end;

     TpvFontCodePointGlyphPairs=array of TpvFontCodePointGlyphPair;

     PpvFontKerningPair=^TpvFontKerningPair;
     TpvFontKerningPair=record
      Left:TpvUInt32;
      Right:TpvUInt32;
      Horizontal:TpvInt32;
      Vertical:TpvInt32;
     end;

     TpvFontKerningPairs=array of TpvFontKerningPair;

     PpvFontDistanceFieldPixel=^TpvFontDistanceFieldPixel;
     TpvFontDistanceFieldPixel=packed record
      r,g,b,a:byte;
     end;

     TpvFontDistanceFieldPixels=array of TpvFontDistanceFieldPixel;

     PpvFontDistanceField=^TpvFontDistanceField;
     TpvFontDistanceField=record
      OffsetX:TpvDouble;
      OffsetY:TpvDouble;
      Width:TpvInt32;
      Height:TpvInt32;
      Pixels:TpvFontDistanceFieldPixels;
     end;

     TpvFontDistanceFields=array of TpvFontDistanceField;

     PpvFontDistanceFieldJob=^TpvFontDistanceFieldJob;
     TpvFontDistanceFieldJob=record
      DistanceField:PpvFontDistanceField;
      MultiChannel:boolean;
      PolygonBuffer:TpvTrueTypeFontPolygonBuffer;
     end;

     TpvFontDistanceFieldJobs=array of TpvFontDistanceFieldJob;

     TpvFontInt64HashMap=class(TpvHashMap<TpvInt64,TpvInt64>);

     TpvFont=class
      private
       fDevice:TpvVulkanDevice;
       fSpriteAtlas:TpvSpriteAtlas;
       fTargetPPI:TpvInt32;
       fUnitsPerEm:TpvInt32;
       fBaseScaleFactor:TpvFloat;
       fInverseBaseScaleFactor:TpvFloat;
       fBaseSize:TpvFloat;
       fInverseBaseSize:TpvFloat;
       fMinX:TpvFloat;
       fMinY:TpvFloat;
       fMaxX:TpvFloat;
       fMaxY:TpvFloat;
       fMinimumCodePoint:TpvUInt32;
       fMaximumCodePoint:TpvUInt32;
       fCodePointBitmap:TpvFontCodePointBitmap;
       fGlyphs:TpvFontGlyphs;
       fCodePointGlyphPairs:TpvFontCodePointGlyphPairs;
       fKerningPairs:TpvFontKerningPairs;
       fCodePointToGlyphHashMap:TpvFontInt64HashMap;
       fKerningPairHashMap:TpvFontInt64HashMap;
       fDistanceFieldJobs:TpvFontDistanceFieldJobs;
       procedure GenerateSignedDistanceField(var DistanceField:TpvFontDistanceField;const MultiChannel:boolean;const PolygonBuffer:TpvTrueTypeFontPolygonBuffer;const FillRule:TpvInt32);
       procedure GenerateSignedDistanceFieldParallelForJobFunction(const Job:PPasMPJob;const ThreadIndex:TPasMPInt32;const Data:TVkPointer;const FromIndex,ToIndex:TPasMPNativeInt);
      public
       constructor Create(const aDevice:TpvVulkanDevice;const aSpriteAtlas:TpvSpriteAtlas;const aTargetPPI:TpvInt32=72;const aBaseSize:TpvFloat=12.0); reintroduce;
       constructor CreateFromTrueTypeFont(const aDevice:TpvVulkanDevice;const aSpriteAtlas:TpvSpriteAtlas;const aTrueTypeFont:TpvTrueTypeFont;const aCodePointRanges:array of TpvFontCodePointRange);
       destructor Destroy; override;
       class function CodePointRange(const aFromCodePoint,aToCodePoint:TpvUInt32):TpvFontCodePointRange; overload;
       class function CodePointRange(const aFromCodePoint,aToCodePoint:WideChar):TpvFontCodePointRange; overload;
       class function CodePointRange(const aCharacterRange:TpvFontCharacterRange):TpvFontCodePointRange; overload;
       function GetScaleFactor(const aSize:TpvFloat):TpvFloat;
       function TextWidth(const aText:TpvUTF8String;const aSize:TpvFloat):TpvFloat;
       function TextHeight(const aText:TpvUTF8String;const aSize:TpvFloat):TpvFloat;
       function TextSize(const aText:TpvUTF8String;const aSize:TpvFloat):TpvVector2;
       function RowHeight(const Percent:TpvFloat):TpvFloat;
       procedure Draw(const aCanvas:TObject;const aText:TpvUTF8String;const aPosition:TpvVector2;const aSize:TpvFloat);
      published
       property BaseSize:TpvFloat read fBaseSize;
     end;

implementation

uses PasVulkan.Utils,
     PasVulkan.Canvas;

function CompareVulkanFontGlyphsByArea(const a,b:TpvPointer):TpvInt32;
begin
 result:=(PpvFontGlyph(b)^.Width*PpvFontGlyph(b)^.Height)-(PpvFontGlyph(a)^.Width*PpvFontGlyph(a)^.Height);
end;

function CompareVulkanFontKerningPairs(const a,b:TpvPointer):TpvInt32;
begin
 result:=TpvInt64(PpvFontKerningPair(a)^.Left)-TpvInt64(PpvFontKerningPair(b)^.Left);
 if result=0 then begin
  result:=TpvInt64(PpvFontKerningPair(a)^.Right)-TpvInt64(PpvFontKerningPair(b)^.Right);
 end;
end;

const VulkanFontDistanceFieldMagnitudeValue=VulkanFontDistanceFieldSpreadValue;
      VulkanFontDistanceFieldPadValue=VulkanFontDistanceFieldSpreadValue;
      VulkanFontScalar1Value=1.0;
      VulkanFontCloseValue=VulkanFontScalar1Value/16.0;
      VulkanFontCloseSquaredValue=VulkanFontCloseValue*VulkanFontCloseValue;
      VulkanFontNearlyZeroValue=VulkanFontScalar1Value/int64(1 shl 18);
      VulkanFontTangentToleranceValue=VulkanFontScalar1Value/int64(1 shl 11);
      VulkanFontRasterizerToScreenScale=1.0/256.0;

      CurveTessellationTolerance=0.125;

      CurveTessellationToleranceSquared=CurveTessellationTolerance*CurveTessellationTolerance;

      CurveRecursionLimit=32;

type PpvFontPathSegmentSide=^TpvFontPathSegmentSide;
     TpvFontPathSegmentSide=
      (
       pssLeft=-1,
       pssOn=0,
       pssRight=1,
       pssNone=2
      );

     PpvFontDistanceFieldDataItem=^TpvFontDistanceFieldDataItem;
     TpvFontDistanceFieldDataItem=record
      SquaredDistance:TpvFloat;
      SquaredDistanceR:TpvFloat;
      SquaredDistanceG:TpvFloat;
      SquaredDistanceB:TpvFloat;
      PseudoSquaredDistanceR:TpvFloat;
      PseudoSquaredDistanceG:TpvFloat;
      PseudoSquaredDistanceB:TpvFloat;
      DeltaWindingScore:TpvInt32;
     end;

     TpvFontDistanceFieldData=array of TpvFontDistanceFieldDataItem;

     PpvFontDoublePrecisionPoint=^TpvFontDoublePrecisionPoint;
     TpvFontDoublePrecisionPoint=record
      x:TpvDouble;
      y:TpvDouble;
     end;

     PpvFontDoublePrecisionAffineMatrix=^TpvFontDoublePrecisionAffineMatrix;
     TpvFontDoublePrecisionAffineMatrix=array[0..5] of TpvDouble;

     PpvFontPathSegmentType=^TpvFontPathSegmentType;
     TpvFontPathSegmentType=
      (
       pstLine,
       pstQuadraticBezierCurve
      );

     PpvFontBoundingBox=^TpvFontBoundingBox;
     TpvFontBoundingBox=record
      Min:TpvFontDoublePrecisionPoint;
      Max:TpvFontDoublePrecisionPoint;
     end;

     PpvFontPathSegmentColor=^TpvFontPathSegmentColor;
     TpvFontPathSegmentColor=
      (
       pscBlack=0,
       pscRed=1,
       pscGreen=2,
       pscYellow=3,
       pscBlue=4,
       pscMagenta=5,
       pscCyan=6,
       pscWhite=7
      );

     PpvFontPathSegmentPoints=^TpvFontPathSegmentPoints;
     TpvFontPathSegmentPoints=array[0..2] of TpvFontDoublePrecisionPoint;

     PpvFontPathSegment=^TpvFontPathSegment;
     TpvFontPathSegment=record
      Type_:TpvFontPathSegmentType;
      Color:TpvFontPathSegmentColor;
      Points:TpvFontPathSegmentPoints;
      P0T,P2T:TpvFontDoublePrecisionPoint;
      XFormMatrix:TpvFontDoublePrecisionAffineMatrix;
      ScalingFactor:TpvDouble;
      SquaredScalingFactor:TpvDouble;
      NearlyZeroScaled:TpvDouble;
      SquaredTangentToleranceScaled:TpvDouble;
      BoundingBox:TpvFontBoundingBox;
     end;

     TpvFontPathSegments=array of TpvFontPathSegment;

     PpvFontPathContour=^TpvFontPathContour;
     TpvFontPathContour=record
      PathSegments:TpvFontPathSegments;
      CountPathSegments:TpvInt32;
     end;

     TpvFontPathContours=array of TpvFontPathContour;

     PpvFontShape=^TpvFontShape;
     TpvFontShape=record
      Contours:TpvFontPathContours;
      CountContours:TpvInt32;
     end;

     PpvFontRowDataIntersectionType=^TpvFontRowDataIntersectionType;
     TpvFontRowDataIntersectionType=
      (
       rditNoIntersection,
       rditVerticalLine,
       rditTangentLine,
       rditTwoPointsIntersect
      );

     PpvFontRowData=^TpvFontRowData;
     TpvFontRowData=record
      IntersectionType:TpvFontRowDataIntersectionType;
      QuadraticXDirection:TpvInt32;
      ScanlineXDirection:TpvInt32;
      YAtIntersection:TpvFloat;
      XAtIntersection:array[0..1] of TpvFloat;
     end;

     PpvFontPointInPolygonPathSegment=^TpvFontPointInPolygonPathSegment;
     TpvFontPointInPolygonPathSegment=record
      Points:array[0..1] of TpvFontDoublePrecisionPoint;
     end;

     TpvFontPointInPolygonPathSegments=array of TpvFontPointInPolygonPathSegment;

     TpvFontDataGenerator=class
      private
       PointInPolygonPathSegments:TpvFontPointInPolygonPathSegments;
       PolygonBuffer:PpvTrueTypeFontPolygonBuffer;
       DistanceField:PpvFontDistanceField;
       MultiChannel:boolean;
       FillRule:TpvInt32;
       Shape:TpvFontShape;
       DistanceFieldData:TpvFontDistanceFieldData;
      protected
       function Clamp(const Value,MinValue,MaxValue:TpvInt64):TpvInt64; overload;
       function Clamp(const Value,MinValue,MaxValue:TpvDouble):TpvDouble; overload;
       function DoublePrecisionPointAdd(const a,b:TpvFontDoublePrecisionPoint):TpvFontDoublePrecisionPoint;
       function DoublePrecisionPointSub(const a,b:TpvFontDoublePrecisionPoint):TpvFontDoublePrecisionPoint;
       function DoublePrecisionPointLength(const p:TpvFontDoublePrecisionPoint):TpvDouble;
       function DoublePrecisionPointDistance(const a,b:TpvFontDoublePrecisionPoint):TpvDouble;
       function DoublePrecisionPointLengthSquared(const v:TpvFontDoublePrecisionPoint):TpvDouble;
       function DoublePrecisionPointDistanceSquared(const a,b:TpvFontDoublePrecisionPoint):TpvDouble;
       function DoublePrecisionPointCrossProduct(const a,b:TpvFontDoublePrecisionPoint):TpvDouble;
       function DoublePrecisionPointIsLeft(const a,b,c:TpvFontDoublePrecisionPoint):TpvDouble;
       function DoublePrecisionPointDotProduct(const a,b:TpvFontDoublePrecisionPoint):TpvDouble;
       function DoublePrecisionPointNormalize(const v:TpvFontDoublePrecisionPoint):TpvFontDoublePrecisionPoint;
       function DoublePrecisionPointLerp(const a,b:TpvFontDoublePrecisionPoint;const t:TpvDouble):TpvFontDoublePrecisionPoint;
       function DoublePrecisionPointMap(const p:TpvFontDoublePrecisionPoint;const m:TpvFontDoublePrecisionAffineMatrix):TpvFontDoublePrecisionPoint;
       function BetweenClosedOpen(const a,b,c:TpvDouble;const Tolerance:TpvDouble=0.0;const XFormToleranceToX:boolean=false):boolean;
       function BetweenClosed(const a,b,c:TpvDouble;const Tolerance:TpvDouble=0.0;const XFormToleranceToX:boolean=false):boolean;
       function NearlyZero(const Value:TpvDouble;const Tolerance:TpvDouble=VulkanFontNearlyZeroValue):boolean;
       function NearlyEqual(const x,y:TpvDouble;const Tolerance:TpvDouble=VulkanFontNearlyZeroValue;const XFormToleranceToX:boolean=false):boolean;
       function SignOf(const Value:TpvDouble):TpvInt32;
       function IsColinear(const Points:array of TpvFontDoublePrecisionPoint):boolean;
       function PathSegmentDirection(const PathSegment:TpvFontPathSegment;const Which:TpvInt32):TpvFontDoublePrecisionPoint;
       function PathSegmentCountPoints(const PathSegment:TpvFontPathSegment):TpvInt32;
       function PathSegmentEndPoint(const PathSegment:TpvFontPathSegment):PpvFontDoublePrecisionPoint;
       function PathSegmentCornerPoint(const PathSegment:TpvFontPathSegment;const WhichA,WhichB:TpvInt32):PpvFontDoublePrecisionPoint;
       procedure InitializePathSegment(var PathSegment:TpvFontPathSegment);
       procedure InitializeDistances;
       function AddLineToPathSegmentArray(var Contour:TpvFontPathContour;const Points:array of TpvFontDoublePrecisionPoint):TpvInt32;
       function AddQuadraticBezierCurveToPathSegmentArray(var Contour:TpvFontPathContour;const Points:array of TpvFontDoublePrecisionPoint):TpvInt32;
       function AddQuadraticBezierCurveAsSubdividedLinesToPathSegmentArray(var Contour:TpvFontPathContour;const Points:array of TpvFontDoublePrecisionPoint):TpvInt32;
       function AddCubicBezierCurveAsSubdividedQuadraticBezierCurvesToPathSegmentArray(var Contour:TpvFontPathContour;const Points:array of TpvFontDoublePrecisionPoint):TpvInt32;
       function AddCubicBezierCurveAsSubdividedLinesToPathSegmentArray(var Contour:TpvFontPathContour;const Points:array of TpvFontDoublePrecisionPoint):TpvInt32;
       function CubeRoot(Value:TpvDouble):TpvDouble;
       function CalculateNearestPointForQuadraticBezierCurve(const PathSegment:TpvFontPathSegment;const XFormPoint:TpvFontDoublePrecisionPoint):TpvDouble;
       procedure PrecomputationForRow(out RowData:TpvFontRowData;const PathSegment:TpvFontPathSegment;const PointLeft,PointRight:TpvFontDoublePrecisionPoint);
       function CalculateSideOfQuadraticBezierCurve(const PathSegment:TpvFontPathSegment;const Point,XFormPoint:TpvFontDoublePrecisionPoint;const RowData:TpvFontRowData):TpvFontPathSegmentSide;
       function DistanceToPathSegment(const Point:TpvFontDoublePrecisionPoint;const PathSegment:TpvFontPathSegment;const RowData:TpvFontRowData;out PathSegmentSide:TpvFontPathSegmentSide):TpvDouble;
       procedure ConvertShape(const DoSubdivideCurvesIntoLines:boolean);
       procedure SplitPathSegmentIntoThreePartsInsideContour(var Contour:TpvFontPathContour;const BasePathSegmentIndex:TpvInt32);
       procedure SplitPathSegmentIntoThreePartsToContour(var Contour:TpvFontPathContour;const BasePathSegmentIndex:TpvInt32;const BasePathSegment:TpvFontPathSegment);
       procedure NormalizeShape;
       procedure PathSegmentColorizeShape;
       function GetLineNonClippedTime(const p,p0,p1:TpvFontDoublePrecisionPoint):TpvDouble;
       function GetQuadraticBezierCurveNonClippedTime(const p,p0,p1,p2:TpvFontDoublePrecisionPoint):TpvDouble;
       function GetNonClampedSignedLineDistance(const p,p0,p1:TpvFontDoublePrecisionPoint):TpvDouble;
       procedure CalculateDistanceFieldDataLineRange(const FromY,ToY:TpvInt32);
       procedure CalculateDistanceFieldDataLineRangeParallelForJobFunction(const Job:PPasMPJob;const ThreadIndex:TPasMPInt32;const Data:TpvPointer;const FromIndex,ToIndex:TPasMPNativeInt);
       function PackDistanceFieldValue(Distance:TpvDouble):TpvUInt8;
       function PackPseudoDistanceFieldValue(Distance:TpvDouble):TpvUInt8;
       procedure ConvertToPointInPolygonPathSegments;
       function GetWindingNumberAtPointInPolygon(const Point:TpvFontDoublePrecisionPoint):TpvInt32;
       function GenerateDistanceFieldPicture(const DistanceFieldData:TpvFontDistanceFieldData;const Width,Height,TryIteration:TpvInt32):boolean;
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       procedure DoIt;
     end;

const VulkanFontDoublePrecisionAffineMatrixIdentity:TpvFontDoublePrecisionAffineMatrix=(1.0,0.0,0.0,0.0,1.0,0.0);

constructor TpvFontDataGenerator.Create;
begin
 inherited Create;
 PointInPolygonPathSegments:=nil;
 PolygonBuffer:=nil;
 DistanceField:=nil;
 MultiChannel:=false;
 FillRule:=pvTTF_PolygonWindingRule_NONZERO;
end;

destructor TpvFontDataGenerator.Destroy;
begin
 PointInPolygonPathSegments:=nil;
 PolygonBuffer:=nil;
 DistanceField:=nil;
 inherited Destroy;
end;

function TpvFontDataGenerator.Clamp(const Value,MinValue,MaxValue:TpvInt64):TpvInt64;
begin
 if Value<=MinValue then begin
  result:=MinValue;
 end else if Value>=MaxValue then begin
  result:=MaxValue;
 end else begin
  result:=Value;
 end;
end;

function TpvFontDataGenerator.Clamp(const Value,MinValue,MaxValue:TpvDouble):TpvDouble;
begin
 if Value<=MinValue then begin
  result:=MinValue;
 end else if Value>=MaxValue then begin
  result:=MaxValue;
 end else begin
  result:=Value;
 end;
end;

function TpvFontDataGenerator.DoublePrecisionPointAdd(const a,b:TpvFontDoublePrecisionPoint):TpvFontDoublePrecisionPoint;
begin
 result.x:=a.x+b.x;
 result.y:=a.y+b.y;
end;

function TpvFontDataGenerator.DoublePrecisionPointSub(const a,b:TpvFontDoublePrecisionPoint):TpvFontDoublePrecisionPoint;
begin
 result.x:=a.x-b.x;
 result.y:=a.y-b.y;
end;

function TpvFontDataGenerator.DoublePrecisionPointLength(const p:TpvFontDoublePrecisionPoint):TpvDouble;
begin
 result:=sqrt(sqr(p.x)+sqr(p.y));
end;

function TpvFontDataGenerator.DoublePrecisionPointDistance(const a,b:TpvFontDoublePrecisionPoint):TpvDouble;
begin
 result:=sqrt(sqr(a.x-b.x)+sqr(a.y-b.y));
end;

function TpvFontDataGenerator.DoublePrecisionPointLengthSquared(const v:TpvFontDoublePrecisionPoint):TpvDouble;
begin
 result:=sqr(v.x)+sqr(v.y);
end;

function TpvFontDataGenerator.DoublePrecisionPointDistanceSquared(const a,b:TpvFontDoublePrecisionPoint):TpvDouble;
begin
 result:=sqr(a.x-b.x)+sqr(a.y-b.y);
end;

function TpvFontDataGenerator.DoublePrecisionPointCrossProduct(const a,b:TpvFontDoublePrecisionPoint):TpvDouble;
begin
 result:=(a.x*b.y)-(a.y*b.x);
end;

function TpvFontDataGenerator.DoublePrecisionPointIsLeft(const a,b,c:TpvFontDoublePrecisionPoint):TpvDouble;
begin
 result:=((b.x*a.x)*(c.y*a.y))-((c.x*a.x)*(b.y*a.y));
end;

function TpvFontDataGenerator.DoublePrecisionPointDotProduct(const a,b:TpvFontDoublePrecisionPoint):TpvDouble;
begin
 result:=(a.x*b.x)+(a.y*b.y);
end;

function TpvFontDataGenerator.DoublePrecisionPointNormalize(const v:TpvFontDoublePrecisionPoint):TpvFontDoublePrecisionPoint;
var f:TpvDouble;
begin
 f:=sqr(v.x)+sqr(v.y);
 if IsZero(f) then begin
  result.x:=0.0;
  result.y:=0.0;
 end else begin
  result.x:=v.x/f;
  result.y:=v.y/f;
 end;
end;

function TpvFontDataGenerator.DoublePrecisionPointLerp(const a,b:TpvFontDoublePrecisionPoint;const t:TpvDouble):TpvFontDoublePrecisionPoint;
begin
 if t<=0.0 then begin
  result:=a;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  result.x:=(a.x*(1.0-t))+(b.x*t);
  result.y:=(a.y*(1.0-t))+(b.y*t);
 end;
end;

function TpvFontDataGenerator.DoublePrecisionPointMap(const p:TpvFontDoublePrecisionPoint;const m:TpvFontDoublePrecisionAffineMatrix):TpvFontDoublePrecisionPoint;
begin
 result.x:=(p.x*m[0])+(p.y*m[1])+m[2];
 result.y:=(p.x*m[3])+(p.y*m[4])+m[5];
end;

function TpvFontDataGenerator.BetweenClosedOpen(const a,b,c:TpvDouble;const Tolerance:TpvDouble=0.0;const XFormToleranceToX:boolean=false):boolean;
var ToleranceB,ToleranceC:TpvDouble;
begin
 Assert(Tolerance>=0.0);
 if XFormToleranceToX then begin
  ToleranceB:=Tolerance/sqrt((sqr(b)*4.0)+1.0);
  ToleranceC:=Tolerance/sqrt((sqr(c)*4.0)+1.0);
 end else begin
  ToleranceB:=Tolerance;
  ToleranceC:=Tolerance;
 end;
 if b<c then begin
  result:=(a>=(b-ToleranceB)) and (a<(c-ToleranceC));
 end else begin
  result:=(a>=(c-ToleranceC)) and (a<(b-ToleranceB));
 end;
end;

function TpvFontDataGenerator.BetweenClosed(const a,b,c:TpvDouble;const Tolerance:TpvDouble=0.0;const XFormToleranceToX:boolean=false):boolean;
var ToleranceB,ToleranceC:TpvDouble;
begin
 Assert(Tolerance>=0.0);
 if XFormToleranceToX then begin
  ToleranceB:=Tolerance/sqrt((sqr(b)*4.0)+1.0);
  ToleranceC:=Tolerance/sqrt((sqr(c)*4.0)+1.0);
 end else begin
  ToleranceB:=Tolerance;
  ToleranceC:=Tolerance;
 end;
 if b<c then begin
  result:=(a>=(b-ToleranceB)) and (a<=(c+ToleranceC));
 end else begin
  result:=(a>=(c-ToleranceC)) and (a<=(b+ToleranceB));
 end;
end;

function TpvFontDataGenerator.NearlyZero(const Value:TpvDouble;const Tolerance:TpvDouble=VulkanFontNearlyZeroValue):boolean;
begin
 Assert(Tolerance>=0.0);
 result:=abs(Value)<=Tolerance;
end;

function TpvFontDataGenerator.NearlyEqual(const x,y:TpvDouble;const Tolerance:TpvDouble=VulkanFontNearlyZeroValue;const XFormToleranceToX:boolean=false):boolean;
begin
 Assert(Tolerance>=0.0);
 if XFormToleranceToX then begin
  result:=abs(x-y)<=(Tolerance/sqrt((sqr(y)*4.0)+1.0));
 end else begin
  result:=abs(x-y)<=Tolerance;
 end;
end;

function TpvFontDataGenerator.SignOf(const Value:TpvDouble):TpvInt32;
begin
 if Value<0.0 then begin
  result:=-1;
 end else begin
  result:=1;
 end;
end;

function TpvFontDataGenerator.IsColinear(const Points:array of TpvFontDoublePrecisionPoint):boolean;
begin
 Assert(length(Points)=3);
 result:=abs(((Points[1].y-Points[0].y)*(Points[1].x-Points[2].x))-
             ((Points[1].y-Points[2].y)*(Points[1].x-Points[0].x)))<=VulkanFontCloseSquaredValue;
end;

function TpvFontDataGenerator.PathSegmentDirection(const PathSegment:TpvFontPathSegment;const Which:TpvInt32):TpvFontDoublePrecisionPoint;
begin
 case PathSegment.Type_ of
  pstLine:begin
   result.x:=PathSegment.Points[1].x-PathSegment.Points[0].x;
   result.y:=PathSegment.Points[1].y-PathSegment.Points[0].y;
  end;
  pstQuadraticBezierCurve:begin
   case Which of
    0:begin
     result.x:=PathSegment.Points[1].x-PathSegment.Points[0].x;
     result.y:=PathSegment.Points[1].y-PathSegment.Points[0].y;
    end;
    1:begin
     result.x:=PathSegment.Points[2].x-PathSegment.Points[1].x;
     result.y:=PathSegment.Points[2].y-PathSegment.Points[1].y;
    end;
    else begin
     result.x:=0.0;
     result.y:=0.0;
     Assert(false);
    end;
   end;
  end;
  else begin
   result.x:=0.0;
   result.y:=0.0;
   Assert(false);
  end;
 end;
end;

function TpvFontDataGenerator.PathSegmentCountPoints(const PathSegment:TpvFontPathSegment):TpvInt32;
begin
 case PathSegment.Type_ of
  pstLine:begin
   result:=2;
  end;
  pstQuadraticBezierCurve:begin
   result:=3;
  end;
  else begin
   result:=0;
   Assert(false);
  end;
 end;
end;

function TpvFontDataGenerator.PathSegmentEndPoint(const PathSegment:TpvFontPathSegment):PpvFontDoublePrecisionPoint;
begin
 case PathSegment.Type_ of
  pstLine:begin
   result:=@PathSegment.Points[1];
  end;
  pstQuadraticBezierCurve:begin
   result:=@PathSegment.Points[2];
  end;
  else begin
   result:=nil;
   Assert(false);
  end;
 end;
end;

function TpvFontDataGenerator.PathSegmentCornerPoint(const PathSegment:TpvFontPathSegment;const WhichA,WhichB:TpvInt32):PpvFontDoublePrecisionPoint;
begin
 case PathSegment.Type_ of
  pstLine:begin
   result:=@PathSegment.Points[WhichB and 1];
  end;
  pstQuadraticBezierCurve:begin
   result:=@PathSegment.Points[(WhichA and 1)+(WhichB and 1)];
  end;
  else begin
   result:=nil;
   Assert(false);
  end;
 end;
end;

procedure TpvFontDataGenerator.InitializePathSegment(var PathSegment:TpvFontPathSegment);
var p0,p1,p2,p1mp0,d,t,sp0,sp1,sp2,p01p,p02p,p12p:TpvFontDoublePrecisionPoint;
    Hypotenuse,CosTheta,SinTheta,a,b,h,c,g,f,gd,fd,x,y,Lambda:TpvDouble;
begin
 case PathSegment.Type_ of
  pstLine:begin
   p0:=PathSegment.Points[0];
   p2:=PathSegment.Points[1];
   PathSegment.BoundingBox.Min.x:=Min(p0.x,p2.x);
   PathSegment.BoundingBox.Min.y:=Min(p0.y,p2.y);
   PathSegment.BoundingBox.Max.x:=Max(p0.x,p2.x);
   PathSegment.BoundingBox.Max.y:=Max(p0.y,p2.y);
   PathSegment.ScalingFactor:=1.0;
   PathSegment.SquaredScalingFactor:=1.0;
   Hypotenuse:=DoublePrecisionPointDistance(p0,p2);
   CosTheta:=(p2.x-p0.x)/Hypotenuse;
   SinTheta:=(p2.y-p0.y)/Hypotenuse;
   PathSegment.XFormMatrix[0]:=CosTheta;
   PathSegment.XFormMatrix[1]:=SinTheta;
   PathSegment.XFormMatrix[2]:=(-(CosTheta*p0.x))-(SinTheta*p0.y);
   PathSegment.XFormMatrix[3]:=-SinTheta;
   PathSegment.XFormMatrix[4]:=CosTheta;
   PathSegment.XFormMatrix[5]:=(SinTheta*p0.x)-(CosTheta*p0.y);
  end;
  else {pstQuad:}begin
   p0:=PathSegment.Points[0];
   p1:=PathSegment.Points[1];
   p2:=PathSegment.Points[2];
   PathSegment.BoundingBox.Min.x:=Min(p0.x,p2.x);
   PathSegment.BoundingBox.Min.y:=Min(p0.y,p2.y);
   PathSegment.BoundingBox.Max.x:=Max(p0.x,p2.x);
   PathSegment.BoundingBox.Max.y:=Max(p0.y,p2.y);
   p1mp0.x:=p1.x-p0.x;
   p1mp0.y:=p1.y-p0.y;
   d.x:=(p1mp0.x-p2.x)+p1.x;
   d.y:=(p1mp0.y-p2.y)+p1.y;
   if IsZero(d.x) then begin
    t.x:=p0.x;
   end else begin
    t.x:=p0.x+(Clamp(p1mp0.x/d.x,0.0,1.0)*p1mp0.x);
   end;
   if IsZero(d.y) then begin
    t.y:=p0.y;
   end else begin
    t.y:=p0.y+(Clamp(p1mp0.y/d.y,0.0,1.0)*p1mp0.y);
   end;
   PathSegment.BoundingBox.Min.x:=Min(PathSegment.BoundingBox.Min.x,t.x);
   PathSegment.BoundingBox.Min.y:=Min(PathSegment.BoundingBox.Min.y,t.y);
   PathSegment.BoundingBox.Max.x:=Max(PathSegment.BoundingBox.Max.x,t.x);
   PathSegment.BoundingBox.Max.y:=Max(PathSegment.BoundingBox.Max.y,t.y);
   sp0.x:=sqr(p0.x);
   sp0.y:=sqr(p0.y);
   sp1.x:=sqr(p1.x);
   sp1.y:=sqr(p1.y);
   sp2.x:=sqr(p2.x);
   sp2.y:=sqr(p2.y);
   p01p.x:=p0.x*p1.x;
   p01p.y:=p0.y*p1.y;
   p02p.x:=p0.x*p2.x;
   p02p.y:=p0.y*p2.y;
   p12p.x:=p1.x*p2.x;
   p12p.y:=p1.y*p2.y;
   a:=sqr((p0.y-(2.0*p1.y))+p2.y);
   h:=-(((p0.y-(2.0*p1.y))+p2.y)*((p0.x-(2.0*p1.x))+p2.x));
   b:=sqr((p0.x-(2.0*p1.x))+p2.x);
   c:=((((((sp0.x*sp2.y)-(4.0*p01p.x*p12p.y))-(2.0*p02p.x*p02p.y))+(4.0*p02p.x*sp1.y))+(4.0*sp1.x*p02p.y))-(4.0*p12p.x*p01p.y))+(sp2.x*sp0.y);
   g:=((((((((((p0.x*p02p.y)-(2.0*p0.x*sp1.y))+(2.0*p0.x*p12p.y))-(p0.x*sp2.y))+(2.0*p1.x*p01p.y))-(4.0*p1.x*p02p.y))+(2.0*p1.x*p12p.y))-(p2.x*sp0.y))+(2.0*p2.x*p01p.y))+(p2.x*p02p.y))-(2.0*p2.x*sp1.y);
   f:=-(((((((((((sp0.x*p2.y)-(2.0*p01p.x*p1.y))-(2.0*p01p.x*p2.y))-(p02p.x*p0.y))+(4.0*p02p.x*p1.y))-(p02p.x*p2.y))+(2.0*sp1.x*p0.y))+(2.0*sp1.x*p2.y))-(2.0*p12p.x*p0.y))-(2.0*p12p.x*p1.y))+(sp2.x*p0.y));
   CosTheta:=sqrt(a/(a+b));
   SinTheta:=(-SignOf((a+b)*h))*sqrt(b/(a+b));
   gd:=(CosTheta*g)-(SinTheta*f);
   fd:=(SinTheta*g)+(CosTheta*f);
   x:=gd/(a+b);
   y:=(1.0/(2.0*fd))*(c-(sqr(gd)/(a+b)));
   Lambda:=-((a+b)/(2.0*fd));
   PathSegment.ScalingFactor:=abs(1.0/Lambda);
   PathSegment.SquaredScalingFactor:=sqr(PathSegment.ScalingFactor);
   CosTheta:=CosTheta*Lambda;
   SinTheta:=SinTheta*Lambda;
   PathSegment.XFormMatrix[0]:=CosTheta;
   PathSegment.XFormMatrix[1]:=-SinTheta;
   PathSegment.XFormMatrix[2]:=x*Lambda;
   PathSegment.XFormMatrix[3]:=SinTheta;
   PathSegment.XFormMatrix[4]:=CosTheta;
   PathSegment.XFormMatrix[5]:=y*Lambda;
  end;
 end;
 PathSegment.NearlyZeroScaled:=VulkanFontNearlyZeroValue/PathSegment.ScalingFactor;
 PathSegment.SquaredTangentToleranceScaled:=sqr(VulkanFontTangentToleranceValue)/PathSegment.SquaredScalingFactor;
 PathSegment.P0T:=DoublePrecisionPointMap(p0,PathSegment.XFormMatrix);
 PathSegment.P2T:=DoublePrecisionPointMap(p2,PathSegment.XFormMatrix);
end;

procedure TpvFontDataGenerator.InitializeDistances;
var Index:TpvInt32;
begin
 for Index:=0 to length(DistanceFieldData)-1 do begin
  DistanceFieldData[Index].SquaredDistance:=sqr(VulkanFontDistanceFieldMagnitudeValue);
  DistanceFieldData[Index].SquaredDistanceR:=sqr(VulkanFontDistanceFieldMagnitudeValue);
  DistanceFieldData[Index].SquaredDistanceG:=sqr(VulkanFontDistanceFieldMagnitudeValue);
  DistanceFieldData[Index].SquaredDistanceB:=sqr(VulkanFontDistanceFieldMagnitudeValue);
  DistanceFieldData[Index].PseudoSquaredDistanceR:=sqr(VulkanFontDistanceFieldMagnitudeValue);
  DistanceFieldData[Index].PseudoSquaredDistanceG:=sqr(VulkanFontDistanceFieldMagnitudeValue);
  DistanceFieldData[Index].PseudoSquaredDistanceB:=sqr(VulkanFontDistanceFieldMagnitudeValue);
  DistanceFieldData[Index].DeltaWindingScore:=0;
 end;
end;

function TpvFontDataGenerator.AddLineToPathSegmentArray(var Contour:TpvFontPathContour;const Points:array of TpvFontDoublePrecisionPoint):TpvInt32;
var PathSegment:PpvFontPathSegment;
begin
 Assert(length(Points)=2);
 result:=Contour.CountPathSegments;
 inc(Contour.CountPathSegments);
 if length(Contour.PathSegments)<=Contour.CountPathSegments then begin
  SetLength(Contour.PathSegments,Contour.CountPathSegments*2);
 end;
 PathSegment:=@Contour.PathSegments[result];
 PathSegment^.Type_:=pstLine;
 PathSegment^.Color:=pscBlack;
 PathSegment^.Points[0]:=Points[0];
 PathSegment^.Points[1]:=Points[1];
 InitializePathSegment(PathSegment^);
end;

function TpvFontDataGenerator.AddQuadraticBezierCurveToPathSegmentArray(var Contour:TpvFontPathContour;const Points:array of TpvFontDoublePrecisionPoint):TpvInt32;
var PathSegment:PpvFontPathSegment;
begin
 Assert(length(Points)=3);
 result:=Contour.CountPathSegments;
 if (DoublePrecisionPointDistanceSquared(Points[0],Points[1])<VulkanFontCloseSquaredValue) or
    (DoublePrecisionPointDistanceSquared(Points[1],Points[2])<VulkanFontCloseSquaredValue) or
    IsColinear(Points) then begin
  if not (SameValue(Points[0].x,Points[2].x) and SameValue(Points[0].y,Points[2].y)) then begin
   inc(Contour.CountPathSegments);
   if length(Contour.PathSegments)<=Contour.CountPathSegments then begin
    SetLength(Contour.PathSegments,Contour.CountPathSegments*2);
   end;
   PathSegment:=@Contour.PathSegments[result];
   PathSegment^.Type_:=pstLine;
   PathSegment^.Color:=pscBlack;
   PathSegment^.Points[0]:=Points[0];
   PathSegment^.Points[1]:=Points[2];
   InitializePathSegment(PathSegment^);
  end;
 end else begin
  inc(Contour.CountPathSegments);
  if length(Contour.PathSegments)<=Contour.CountPathSegments then begin
   SetLength(Contour.PathSegments,Contour.CountPathSegments*2);
  end;
  PathSegment:=@Contour.PathSegments[result];
  PathSegment^.Type_:=pstQuadraticBezierCurve;
  PathSegment^.Color:=pscBlack;
  PathSegment^.Points[0]:=Points[0];
  PathSegment^.Points[1]:=Points[1];
  PathSegment^.Points[2]:=Points[2];
  InitializePathSegment(PathSegment^);
 end;
end;

function TpvFontDataGenerator.AddQuadraticBezierCurveAsSubdividedLinesToPathSegmentArray(var Contour:TpvFontPathContour;const Points:array of TpvFontDoublePrecisionPoint):TpvInt32;
var LastPoint:TpvFontDoublePrecisionPoint;
 procedure LineToPointAt(const Point:TpvFontDoublePrecisionPoint);
 begin
  AddLineToPathSegmentArray(Contour,[LastPoint,Point]);
  LastPoint:=Point;
 end;
 procedure Recursive(const x1,y1,x2,y2,x3,y3:TpvDouble;const Level:TpvInt32);
 var x12,y12,x23,y23,x123,y123,dx,dy:TpvDouble;
     Point:TpvFontDoublePrecisionPoint;
 begin
  x12:=(x1+x2)*0.5;
  y12:=(y1+y2)*0.5;
  x23:=(x2+x3)*0.5;
  y23:=(y2+y3)*0.5;
  x123:=(x12+x23)*0.5;
  y123:=(y12+y23)*0.5;
  dx:=x3-x1;
  dy:=y3-y1;
  if (Level>CurveRecursionLimit) or
     ((Level>0) and
      (sqr(((x2-x3)*dy)-((y2-y3)*dx))<((sqr(dx)+sqr(dy))*CurveTessellationToleranceSquared))) then begin
   Point.x:=x3;
   Point.y:=y3;
   LineToPointAt(Point);
  end else begin
   Recursive(x1,y1,x12,y12,x123,y123,Level+1);
   Recursive(x123,y123,x23,y23,x3,y3,Level+1);
  end;
 end;
begin
 Assert(length(Points)=3);
 result:=Contour.CountPathSegments;
 LastPoint:=Points[0];
 Recursive(Points[0].x,Points[0].y,Points[1].x,Points[1].y,Points[2].x,Points[2].y,0);
 LineToPointAt(Points[2]);
end;

function TpvFontDataGenerator.AddCubicBezierCurveAsSubdividedQuadraticBezierCurvesToPathSegmentArray(var Contour:TpvFontPathContour;const Points:array of TpvFontDoublePrecisionPoint):TpvInt32;
type TLine=record
      a,b,c:TpvDouble;
      Exist,Vertical:boolean;
     end;
     TPointLine=record
      p:TpvFontDoublePrecisionPoint;
      l:TLine;
     end;
var LastPoint:TpvFontDoublePrecisionPoint;
 procedure MoveTo(const p:TpvFontDoublePrecisionPoint);
 begin
  LastPoint:=p;
 end;
 procedure LineTo(const p:TpvFontDoublePrecisionPoint);
 begin
  AddLineToPathSegmentArray(Contour,[LastPoint,p]);
  LastPoint:=p;
 end;
 procedure CurveTo(const p0,p1:TpvFontDoublePrecisionPoint);
 begin
  AddQuadraticBezierCurveToPathSegmentArray(Contour,[LastPoint,p0,p1]);
  LastPoint:=p1;
 end;
 function GetLine(const P0,P1:TpvFontDoublePrecisionPoint):TLine;
 begin
  FillChar(result,SizeOf(TLine),#0);
  if SameValue(P0.x,P1.x) then begin
   if SameValue(P0.y,P1.y) then begin
    // P0 and P1 are same point, return null
    result.Exist:=false;
    result.Vertical:=false;
   end else begin
    // Otherwise, the line is a vertical line
    result.Exist:=true;
    result.Vertical:=true;
    result.c:=P0.x;
   end;
  end else begin
   result.Exist:=true;
   result.Vertical:=false;
   result.a:=(P0.y-P1.y)/(P0.x-P1.x);
   result.b:=P0.y-(result.a*P0.x);
  end;
 end;
 function GetLine2(const P0,v0:TpvFontDoublePrecisionPoint):TLine;
 begin
  FillChar(result,SizeOf(TLine),#0);
  result.Exist:=true;
  if IsZero(v0.x) then begin
   // The line is vertical
   result.Vertical:=true;
   result.c:=p0.x;
  end else begin
   result.Vertical:=false;
   result.a:=v0.y/v0.x;
   result.b:=P0.y-(result.a*P0.x);
  end;
 end;
 function GetLineCross(const l0,l1:TLine;var b:boolean):TpvFontDoublePrecisionPoint;
 var u:TpvDouble;
 begin

  result.x:=0.0;
  result.y:=0.0;

  // Make sure both line exists
  b:=false;
  if (not l0.exist) or (not l1.exist) then begin
   exit;
  end;

  // Checks whether both lines are vertical
  if (not l0.vertical) and (not l1.vertical) then begin

   // Lines are not verticals but parallel, intersection does not exist
   if l0.a=l1.a then begin
    exit;
   end;

   // Calculate common x value.
   u:=(l1.b-l0.b)/(l0.a-l1.a);

   // Return the new point
   result.x:=u;
   result.y:=(l0.a*u)+l0.b;
  end else begin
   if l0.Vertical then begin
    if l1.Vertical then begin
     // Both lines vertical, intersection does not exist
     exit;
    end else begin
     // Return the point on l1 with x = c0
     result.x:=l0.c;
     result.y:=(l1.a*l0.c)+l1.b;
    end;
   end else if l1.Vertical then begin
    // No need to test c0 as it was tested above, return the point on l0 with x = c1
    result.x:=l1.c;
    result.y:=(l0.a*l1.c)+l0.b;
   end;
  end;

  // We're done!
  b:=true;
 end;
 function GetCubicPoint(const c0,c1,c2,c3,t:TpvDouble):TpvDouble;
 var ts,g,b,a:TpvDouble;
 begin
  ts:=t*t;
  g:=3*(c1-c0);
  b:=(3*(c2-c1))-g;
  a:=((c3-c0)-b)-g;
  result:=(a*ts*t)+(b*ts)+(g*t)+c0;
 end;
 function GetCubicDerivative(const c0,c1,c2,c3,t:TpvDouble):TpvDouble;
 var g,b,a:TpvDouble;
 begin
  g:=3*(c1-c0);
  b:=(3*(c2-c1))-g;
  a:=((c3-c0)-b)-g;
  result:=(3*a*t*t)+(2*b*t)+g;
 end;
 function GetCubicTangent(const P0,P1,P2,P3:TpvFontDoublePrecisionPoint;t:TpvDouble):TPointLine;
 var P,V:TpvFontDoublePrecisionPoint;
     l:TLine;
 begin

  // Calculates the position of the cubic bezier at t
  P.x:=GetCubicPoint(P0.x,P1.x,P2.x,P3.x,t);
  P.y:=GetCubicPoint(P0.y,P1.y,P2.y,P3.y,t);

  // Calculates the tangent values of the cubic bezier at t
  V.x:=GetCubicDerivative(P0.x,P1.x,P2.x,P3.x,t);
  V.y:=GetCubicDerivative(P0.y,P1.y,P2.y,P3.y,t);

  // Calculates the line equation for the tangent at t
  l:=GetLine2(P,V);

  // Return the Point/Tangent object
  result.P:=P;
  result.l:=l;

 end;
 procedure CubicCurveToTangent(const P0,P1,P2,P3:TpvFontDoublePrecisionPoint);
 const NumberOfSegments=8;
  function SliceCubicBezierSegment(const p0,p1,p2,p3:TpvFontDoublePrecisionPoint;const u1,u2:TpvDouble;const Tu1,Tu2:TPointLine;Recursion:TpvInt32):TpvInt32;
  var P,ControlPoint:TpvFontDoublePrecisionPoint;
      b:boolean;
      d,uMid:TpvDouble;
      TuMid:TPointLine;
  begin

   // Prevents infinite recursion (no more than 10 levels) if 10 levels are reached the latest subsegment is approximated with a line (no quadratic curve). It should be good enough.
   if Recursion>10 then begin
    P:=Tu2.P;
    LineTo(P);
    result:=1;
    exit;
   end;

   // Recursion level is OK, process current segment
   ControlPoint:=GetLineCross(Tu1.l,Tu2.l,b);

   // A control point is considered misplaced if its distance from one of the anchor is greater
   // than the distance between the two anchors.
   d:=DoublePrecisionPointDistance(Tu1.P,Tu2.P);
   if (not b) or (DoublePrecisionPointDistance(Tu1.P,ControlPoint)>d) or (DoublePrecisionPointDistance(Tu2.P,ControlPoint)>d) then begin

    // Total for this subsegment starts at 0
    result:=0;

    // If the Control Point is misplaced, slice the segment more
    uMid:=(u1+u2)*0.5;
    TuMid:=GetCubicTangent(P0,P1,P2,P3,uMid);
    inc(result,SliceCubicBezierSegment(P0,P1,P2,P3,u1,uMid,Tu1,TuMid,Recursion+1));
    inc(result,SliceCubicBezierSegment(P0,P1,P2,P3,uMid,u2,TuMid,Tu2,Recursion+1));

   end else begin

    // If everything is OK draw curve
    P:=Tu2.P;
    CurveTo(ControlPoint,P);
    result:=1;

   end;
  end;
 var CurrentTime,NextTime:TPointLine;
     TimeStep:TpvDouble;
     i:TpvInt32;
 begin

  // Get the time step from number of output segments
  TimeStep:=1.0/NumberOfSegments;

  // Get the first tangent Object
  CurrentTime.P:=P0;
  CurrentTime.l:=GetLine(P0,P1);

  MoveTo(P0);

  // Get tangent objects for all intermediate segments and draw the segments
  for i:=1 to NumberOfSegments do begin

   // Get tangent object for next point
   NextTime:=GetCubicTangent(P0,P1,P2,P3,i*TimeStep);

   // Get segment data for the current segment
   SliceCubicBezierSegment(P0,P1,P2,P3,(i-1)*TimeStep,i*TimeStep,CurrentTime,NextTime,0);

   // Prepare for next round
   CurrentTime:=NextTime;

  end;

 end;
begin
 Assert(length(Points)=4);
 result:=Contour.CountPathSegments;
 CubicCurveToTangent(Points[0],Points[1],Points[2],Points[3]);
end;

function TpvFontDataGenerator.AddCubicBezierCurveAsSubdividedLinesToPathSegmentArray(var Contour:TpvFontPathContour;const Points:array of TpvFontDoublePrecisionPoint):TpvInt32;
var LastPoint:TpvFontDoublePrecisionPoint;
 procedure LineToPointAt(const Point:TpvFontDoublePrecisionPoint);
 begin
  AddLineToPathSegmentArray(Contour,[LastPoint,Point]);
  LastPoint:=Point;
 end;
 procedure Recursive(const x1,y1,x2,y2,x3,y3,x4,y4:TpvDouble;const Level:TpvInt32);
 var x12,y12,x23,y23,x34,y34,x123,y123,x234,y234,x1234,y1234,dx,dy:TpvDouble;
     Point:TpvFontDoublePrecisionPoint;
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
  dx:=x4-x1;
  dy:=y4-y1;
  if (Level>CurveRecursionLimit) or
     ((Level>0) and
      (sqr(abs(((x2-x4)*dy)-((y2-y4)*dx))+
           abs(((x3-x4)*dy)-((y3-y4)*dx)))<((sqr(dx)+sqr(dy))*CurveTessellationToleranceSquared))) then begin
   Point.x:=x4;
   Point.y:=y4;
   LineToPointAt(Point);
  end else begin
   Recursive(x1,y1,x12,y12,x123,y123,x1234,y1234,Level+1);
   Recursive(x1234,y1234,x234,y234,x34,y34,x4,y4,Level+1);
  end;
 end;
begin
 Assert(length(Points)=4);
 result:=Contour.CountPathSegments;
 LastPoint:=Points[0];
 Recursive(Points[0].x,Points[0].y,Points[1].x,Points[1].y,Points[2].x,Points[2].y,Points[3].x,Points[3].y,0);
 LineToPointAt(Points[3]);
end;

function TpvFontDataGenerator.CubeRoot(Value:TpvDouble):TpvDouble;
begin
 if IsZero(Value) then begin
  result:=0.0;
 end else begin
  result:=exp(ln(abs(Value))/3.0);
  if Value<0.0 then begin
   result:=-result;
  end;
 end;
end;

function TpvFontDataGenerator.CalculateNearestPointForQuadraticBezierCurve(const PathSegment:TpvFontPathSegment;const XFormPoint:TpvFontDoublePrecisionPoint):TpvDouble;
const OneDiv3=1.0/3.0;
      OneDiv27=1.0/27.0;
var a,b,a3,b2,c,SqrtC,CosPhi,Phi:TpvDouble;
begin
 a:=0.5-XFormPoint.y;
 b:=(-0.5)*XFormPoint.x;
 a3:=sqr(a)*a;
 b2:=sqr(b);
 c:=(b2*0.25)+(a3*OneDiv27);
 if c>=0.0 then begin
  SqrtC:=sqrt(c);
  b:=b*(-0.5);
  result:=CubeRoot(b+SqrtC)+CubeRoot(b-SqrtC);
 end else begin
  CosPhi:=sqrt((b2*0.25)*((-27.0)/a3));
  if b>0.0 then begin
   CosPhi:=-CosPhi;
  end;
  Phi:=ArcCos(CosPhi);
  if XFormPoint.x>0.0 then begin
   result:=2.0*sqrt(a*(-OneDiv3))*cos(Phi*OneDiv3);
   if not BetweenClosed(result,PathSegment.P0T.x,PathSegment.P2T.x) then begin
    result:=2.0*sqrt(a*(-OneDiv3))*cos((Phi*OneDiv3)+(pi*2.0*OneDiv3));
   end;
  end else begin
   result:=2.0*sqrt(a*(-OneDiv3))*cos((Phi*OneDiv3)+(pi*2.0*OneDiv3));
   if not BetweenClosed(result,PathSegment.P0T.x,PathSegment.P2T.x) then begin
    result:=2.0*sqrt(a*(-OneDiv3))*cos(Phi*OneDiv3);
   end;
  end;
 end;
end;

procedure TpvFontDataGenerator.PrecomputationForRow(out RowData:TpvFontRowData;const PathSegment:TpvFontPathSegment;const PointLeft,PointRight:TpvFontDoublePrecisionPoint);
var XFormPointLeft,XFormPointRight:TpvFontDoublePrecisionPoint;
    x0,y0,x1,y1,m,b,m2,c,Tolerance,d:TpvDouble;
begin
 if PathSegment.Type_=pstQuadraticBezierCurve then begin
  XFormPointLeft:=DoublePrecisionPointMap(PointLeft,PathSegment.XFormMatrix);
  XFormPointRight:=DoublePrecisionPointMap(PointRight,PathSegment.XFormMatrix);
  RowData.QuadraticXDirection:=SignOf(PathSegment.P2T.x-PathSegment.P0T.x);
  RowData.ScanlineXDirection:=SignOf(XFormPointRight.x-XFormPointLeft.x);
  x0:=XFormPointLeft.x;
  y0:=XFormPointLeft.y;
  x1:=XFormPointRight.x;
  y1:=XFormPointRight.y;
  if NearlyEqual(x0,x1,PathSegment.NearlyZeroScaled,true) then begin
   RowData.IntersectionType:=rditVerticalLine;
   RowData.YAtIntersection:=sqr(x0);
   RowData.ScanlineXDirection:=0;
  end else begin
   m:=(y1-y0)/(x1-x0);
   b:=y0-(m*x0);
   m2:=sqr(m);
   c:=m2+(4.0*b);
   Tolerance:=(4.0*PathSegment.SquaredTangentToleranceScaled)/(m2+1.0);
   if (RowData.ScanlineXDirection=1) and
      (SameValue(PathSegment.Points[0].y,PointLeft.y) or
       SameValue(PathSegment.Points[2].y,PointLeft.y)) and
       NearlyZero(c,Tolerance) then begin
    RowData.IntersectionType:=rditTangentLine;
    RowData.XAtIntersection[0]:=m*0.5;
    RowData.XAtIntersection[1]:=m*0.5;
   end else if c<=0.0 then begin
    RowData.IntersectionType:=rditNoIntersection;
   end else begin
    RowData.IntersectionType:=rditTwoPointsIntersect;
    d:=sqrt(c);
    RowData.XAtIntersection[0]:=(m+d)*0.5;
    RowData.XAtIntersection[1]:=(m-d)*0.5;
   end;
  end;
 end;
end;

function TpvFontDataGenerator.CalculateSideOfQuadraticBezierCurve(const PathSegment:TpvFontPathSegment;const Point,XFormPoint:TpvFontDoublePrecisionPoint;const RowData:TpvFontRowData):TpvFontPathSegmentSide;
var p0,p1:TpvDouble;
    sp0,sp1:TpvInt32;
    ip0,ip1:boolean;
begin
 case RowData.IntersectionType of
  rditVerticalLine:begin
   result:=TpvFontPathSegmentSide(TpvInt32(SignOf(XFormPoint.y-RowData.YAtIntersection)*RowData.QuadraticXDirection));
  end;
  rditTwoPointsIntersect:begin
   result:=pssNone;
   p0:=RowData.XAtIntersection[0];
   p1:=RowData.XAtIntersection[1];
   sp0:=SignOf(p0-XFormPoint.x);
   ip0:=true;
   ip1:=true;
   if RowData.ScanlineXDirection=1 then begin
    if ((RowData.QuadraticXDirection=-1) and
        (PathSegment.Points[0].y<=Point.y) and
        NearlyEqual(PathSegment.P0T.x,p0,PathSegment.NearlyZeroScaled,true)) or
       ((RowData.QuadraticXDirection=1) and
        (PathSegment.Points[2].y<=Point.y) and
        NearlyEqual(PathSegment.P2T.x,p0,PathSegment.NearlyZeroScaled,true)) then begin
     ip0:=false;
    end;
    if ((RowData.QuadraticXDirection=-1) and
        (PathSegment.Points[2].y<=Point.y) and
        NearlyEqual(PathSegment.P2T.x,p1,PathSegment.NearlyZeroScaled,true)) or
       ((RowData.QuadraticXDirection=1) and
        (PathSegment.Points[0].y<=Point.y) and
        NearlyEqual(PathSegment.P0T.x,p1,PathSegment.NearlyZeroScaled,true)) then begin
     ip1:=false;
    end;
   end;
   if ip0 and BetweenClosed(p0,PathSegment.P0T.x,PathSegment.P2T.x,PathSegment.NearlyZeroScaled,true) then begin
    result:=TpvFontPathSegmentSide(TpvInt32(sp0*RowData.QuadraticXDirection));
   end;
   if ip1 and BetweenClosed(p1,PathSegment.P0T.x,PathSegment.P2T.x,PathSegment.NearlyZeroScaled,true) then begin
    sp1:=SignOf(p1-XFormPoint.x);
    if (result=pssNone) or (sp1=1) then begin
     result:=TpvFontPathSegmentSide(TpvInt32(-sp1*RowData.QuadraticXDirection));
    end;
   end;
  end;
  rditTangentLine:begin
   result:=pssNone;
   if RowData.ScanlineXDirection=1 then begin
    if SameValue(PathSegment.Points[0].y,Point.y) then begin
     result:=TpvFontPathSegmentSide(TpvInt32(SignOf(RowData.XAtIntersection[0]-XFormPoint.x)));
    end else if SameValue(PathSegment.Points[2].y,Point.y) then begin
     result:=TpvFontPathSegmentSide(TpvInt32(SignOf(XFormPoint.x-RowData.XAtIntersection[0])));
    end;
   end;
  end;
  else begin
   result:=pssNone;
  end;
 end;
end;

function TpvFontDataGenerator.DistanceToPathSegment(const Point:TpvFontDoublePrecisionPoint;const PathSegment:TpvFontPathSegment;const RowData:TpvFontRowData;out PathSegmentSide:TpvFontPathSegmentSide):TpvDouble;
var XFormPoint,x:TpvFontDoublePrecisionPoint;
    NearestPoint:TpvDouble;
begin
 XFormPoint:=DoublePrecisionPointMap(Point,PathSegment.XFormMatrix);
 case PathSegment.Type_ of
  pstLine:begin
   if BetweenClosed(XFormPoint.x,PathSegment.P0T.x,PathSegment.P2T.x) then begin
    result:=sqr(XFormPoint.y);
   end else if XFormPoint.x<PathSegment.P0T.x then begin
    result:=sqr(XFormPoint.x)+sqr(XFormPoint.y);
   end else begin
    result:=sqr(XFormPoint.x-PathSegment.P2T.x)+sqr(XFormPoint.y);
   end;
   if BetweenClosedOpen(Point.y,PathSegment.BoundingBox.Min.y,PathSegment.BoundingBox.Max.y) then begin
    PathSegmentSide:=TpvFontPathSegmentSide(TpvInt32(SignOf(XFormPoint.y)));
   end else begin
    PathSegmentSide:=pssNone;
   end;
  end;
  pstQuadraticBezierCurve:begin
   NearestPoint:=CalculateNearestPointForQuadraticBezierCurve(PathSegment,XFormPoint);
   if BetweenClosed(NearestPoint,PathSegment.P0T.x,PathSegment.P2T.x) then begin
    x.x:=NearestPoint;
    x.y:=sqr(NearestPoint);
    result:=DoublePrecisionPointDistanceSquared(XFormPoint,x)*PathSegment.SquaredScalingFactor;
   end else begin
    result:=Min(DoublePrecisionPointDistanceSquared(XFormPoint,PathSegment.P0T),
                DoublePrecisionPointDistanceSquared(XFormPoint,PathSegment.P2T))*PathSegment.SquaredScalingFactor;
   end;
   if BetweenClosedOpen(Point.y,PathSegment.BoundingBox.Min.y,PathSegment.BoundingBox.Max.y) then begin
    PathSegmentSide:=CalculateSideOfQuadraticBezierCurve(PathSegment,Point,XFormPoint,RowData);
   end else begin
    PathSegmentSide:=pssNone;
   end;
  end;
  else begin
   PathSegmentSide:=pssNone;
   result:=0.0;
  end;
 end;
end;

procedure TpvFontDataGenerator.ConvertShape(const DoSubdivideCurvesIntoLines:boolean);
var CommandIndex:TpvInt32;
    Contour:PpvFontPathContour;
    StartPoint,LastPoint,ControlPoint,OtherControlPoint,Point:TpvFontDoublePrecisionPoint;
begin
 Shape.Contours:=nil;
 Shape.CountContours:=0;
 try
  Contour:=nil;
  try
   StartPoint.x:=0.0;
   StartPoint.y:=0.0;
   LastPoint.x:=0.0;
   LastPoint.y:=0.0;
   for CommandIndex:=0 to PolygonBuffer^.CountCommands-1 do begin
    case PolygonBuffer^.Commands[CommandIndex].CommandType of
     pvTTF_PolygonCommandType_MOVETO:begin
      if assigned(Contour) then begin
       SetLength(Contour^.PathSegments,Contour^.CountPathSegments);
      end;
      if length(Shape.Contours)<(Shape.CountContours+1) then begin
       SetLength(Shape.Contours,(Shape.CountContours+1)*2);
      end;
      Contour:=@Shape.Contours[Shape.CountContours];
      inc(Shape.CountContours);
      LastPoint.x:=(PolygonBuffer^.Commands[CommandIndex].Points[0].x*VulkanFontRasterizerToScreenScale)+DistanceField^.OffsetX;
      LastPoint.y:=(PolygonBuffer^.Commands[CommandIndex].Points[0].y*VulkanFontRasterizerToScreenScale)+DistanceField^.OffsetY;
      StartPoint:=LastPoint;
     end;
     pvTTF_PolygonCommandType_LINETO:begin
      Point.x:=(PolygonBuffer^.Commands[CommandIndex].Points[0].x*VulkanFontRasterizerToScreenScale)+DistanceField^.OffsetX;
      Point.y:=(PolygonBuffer^.Commands[CommandIndex].Points[0].y*VulkanFontRasterizerToScreenScale)+DistanceField^.OffsetY;
      if assigned(Contour) and not (SameValue(LastPoint.x,Point.x) and SameValue(LastPoint.y,Point.y)) then begin
       AddLineToPathSegmentArray(Contour^,[LastPoint,Point]);
      end;
      LastPoint:=Point;
     end;
     pvTTF_PolygonCommandType_QUADRATICCURVETO:begin
      ControlPoint.x:=(PolygonBuffer^.Commands[CommandIndex].Points[0].x*VulkanFontRasterizerToScreenScale)+DistanceField^.OffsetX;
      ControlPoint.y:=(PolygonBuffer^.Commands[CommandIndex].Points[0].y*VulkanFontRasterizerToScreenScale)+DistanceField^.OffsetY;
      Point.x:=(PolygonBuffer^.Commands[CommandIndex].Points[1].x*VulkanFontRasterizerToScreenScale)+DistanceField^.OffsetX;
      Point.y:=(PolygonBuffer^.Commands[CommandIndex].Points[1].y*VulkanFontRasterizerToScreenScale)+DistanceField^.OffsetY;
      if assigned(Contour) and not ((SameValue(LastPoint.x,Point.x) and SameValue(LastPoint.y,Point.y)) and
                                    (SameValue(LastPoint.x,ControlPoint.x) and SameValue(LastPoint.y,ControlPoint.y))) then begin
       if DoSubdivideCurvesIntoLines then begin
        AddQuadraticBezierCurveAsSubdividedLinesToPathSegmentArray(Contour^,[LastPoint,ControlPoint,Point]);
       end else begin
        AddQuadraticBezierCurveToPathSegmentArray(Contour^,[LastPoint,ControlPoint,Point]);
       end;
      end;
      LastPoint:=Point;
     end;
     pvTTF_PolygonCommandType_CUBICCURVETO:begin
      ControlPoint.x:=(PolygonBuffer^.Commands[CommandIndex].Points[0].x*VulkanFontRasterizerToScreenScale)+DistanceField^.OffsetX;
      ControlPoint.y:=(PolygonBuffer^.Commands[CommandIndex].Points[0].y*VulkanFontRasterizerToScreenScale)+DistanceField^.OffsetY;
      OtherControlPoint.x:=(PolygonBuffer^.Commands[CommandIndex].Points[1].x*VulkanFontRasterizerToScreenScale)+DistanceField^.OffsetX;
      OtherControlPoint.y:=(PolygonBuffer^.Commands[CommandIndex].Points[1].y*VulkanFontRasterizerToScreenScale)+DistanceField^.OffsetY;
      Point.x:=(PolygonBuffer^.Commands[CommandIndex].Points[2].x*VulkanFontRasterizerToScreenScale)+DistanceField^.OffsetX;
      Point.y:=(PolygonBuffer^.Commands[CommandIndex].Points[2].y*VulkanFontRasterizerToScreenScale)+DistanceField^.OffsetY;
      if assigned(Contour) and not ((SameValue(LastPoint.x,Point.x) and SameValue(LastPoint.y,Point.y)) and
                                    (SameValue(LastPoint.x,OtherControlPoint.x) and SameValue(LastPoint.y,OtherControlPoint.y)) and
                                    (SameValue(LastPoint.x,ControlPoint.x) and SameValue(LastPoint.y,ControlPoint.y))) then begin
       if DoSubdivideCurvesIntoLines then begin
        AddCubicBezierCurveAsSubdividedLinesToPathSegmentArray(Contour^,[LastPoint,ControlPoint,OtherControlPoint,Point]);
       end else begin
        AddCubicBezierCurveAsSubdividedLinesToPathSegmentArray(Contour^,[LastPoint,ControlPoint,OtherControlPoint,Point]);
//       AddCubicBezierCurveAsSubdividedQuadraticBezierCurvesToPathSegmentArray(Contour^,[LastPoint,ControlPoint,OtherControlPoint,Point]);
       end;
      end;
      LastPoint:=Point;
     end;
     pvTTF_PolygonCommandType_CLOSE:begin
      if assigned(Contour) then begin
       if not (SameValue(LastPoint.x,StartPoint.x) and SameValue(LastPoint.y,StartPoint.y)) then begin
        AddLineToPathSegmentArray(Contour^,[LastPoint,StartPoint]);
       end;
       SetLength(Contour^.PathSegments,Contour^.CountPathSegments);
      end;
      Contour:=nil;
     end;
    end;
   end;
  finally
   if assigned(Contour) then begin
    SetLength(Contour^.PathSegments,Contour^.CountPathSegments);
   end;
  end;
 finally
  SetLength(Shape.Contours,Shape.CountContours);
 end;
end;

procedure TpvFontDataGenerator.SplitPathSegmentIntoThreePartsInsideContour(var Contour:TpvFontPathContour;const BasePathSegmentIndex:TpvInt32);
var BasePathSegment:TpvFontPathSegment;
begin
 if (BasePathSegmentIndex>=0) and (BasePathSegmentIndex<Contour.CountPathSegments) then begin
  BasePathSegment:=Contour.PathSegments[BasePathSegmentIndex];
  if BasePathSegment.Type_ in [pstLine,pstQuadraticBezierCurve] then begin
   inc(Contour.CountPathSegments,2);
   if length(Contour.PathSegments)<=Contour.CountPathSegments then begin
    SetLength(Contour.PathSegments,Contour.CountPathSegments*2);
   end;
   Move(Contour.PathSegments[BasePathSegmentIndex+1],Contour.PathSegments[BasePathSegmentIndex+3],(Contour.CountPathSegments-(BasePathSegmentIndex+3))*SizeOf(TpvFontPathSegment));
   FillChar(Contour.PathSegments[BasePathSegmentIndex],SizeOf(TpvFontPathSegment)*3,#0);
  end else begin
   Assert(false);
  end;
  case BasePathSegment.Type_ of
   pstLine:begin
    Contour.PathSegments[BasePathSegmentIndex+0].Type_:=pstLine;
    Contour.PathSegments[BasePathSegmentIndex+0].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+0].Points[0]:=BasePathSegment.Points[0];
    Contour.PathSegments[BasePathSegmentIndex+0].Points[1]:=DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],1.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+1].Type_:=pstLine;
    Contour.PathSegments[BasePathSegmentIndex+1].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+1].Points[0]:=Contour.PathSegments[BasePathSegmentIndex+0].Points[1];
    Contour.PathSegments[BasePathSegmentIndex+1].Points[1]:=DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],2.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+2].Type_:=pstLine;
    Contour.PathSegments[BasePathSegmentIndex+2].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+2].Points[0]:=Contour.PathSegments[BasePathSegmentIndex+1].Points[1];
    Contour.PathSegments[BasePathSegmentIndex+2].Points[1]:=BasePathSegment.Points[1];
   end;
   pstQuadraticBezierCurve:begin
    Contour.PathSegments[BasePathSegmentIndex+0].Type_:=pstQuadraticBezierCurve;
    Contour.PathSegments[BasePathSegmentIndex+0].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+0].Points[0]:=BasePathSegment.Points[0];
    Contour.PathSegments[BasePathSegmentIndex+0].Points[1]:=DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],1.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+0].Points[2]:=DoublePrecisionPointLerp(DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],1.0/3.0),DoublePrecisionPointLerp(BasePathSegment.Points[1],BasePathSegment.Points[2],1.0/3.0),1.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+1].Type_:=pstQuadraticBezierCurve;
    Contour.PathSegments[BasePathSegmentIndex+1].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+1].Points[0]:=Contour.PathSegments[BasePathSegmentIndex+0].Points[2];
    Contour.PathSegments[BasePathSegmentIndex+1].Points[1]:=DoublePrecisionPointLerp(DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],5.0/9.0),DoublePrecisionPointLerp(BasePathSegment.Points[1],BasePathSegment.Points[2],4.0/9.0),0.5);
    Contour.PathSegments[BasePathSegmentIndex+1].Points[2]:=DoublePrecisionPointLerp(DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],2.0/3.0),DoublePrecisionPointLerp(BasePathSegment.Points[1],BasePathSegment.Points[2],2.0/3.0),2.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+2].Type_:=pstQuadraticBezierCurve;
    Contour.PathSegments[BasePathSegmentIndex+2].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+2].Points[0]:=Contour.PathSegments[BasePathSegmentIndex+1].Points[2];
    Contour.PathSegments[BasePathSegmentIndex+2].Points[1]:=DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],2.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+2].Points[2]:=BasePathSegment.Points[2];
   end;
   else begin
    Assert(false);
   end;
  end;
  InitializePathSegment(Contour.PathSegments[BasePathSegmentIndex+0]);
  InitializePathSegment(Contour.PathSegments[BasePathSegmentIndex+1]);
  InitializePathSegment(Contour.PathSegments[BasePathSegmentIndex+2]);
 end;
end;

procedure TpvFontDataGenerator.SplitPathSegmentIntoThreePartsToContour(var Contour:TpvFontPathContour;const BasePathSegmentIndex:TpvInt32;const BasePathSegment:TpvFontPathSegment);
begin
 if (BasePathSegmentIndex>=0) and (BasePathSegmentIndex<Contour.CountPathSegments) then begin
  case BasePathSegment.Type_ of
   pstLine:begin
    Contour.PathSegments[BasePathSegmentIndex+0].Type_:=pstLine;
    Contour.PathSegments[BasePathSegmentIndex+0].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+0].Points[0]:=BasePathSegment.Points[0];
    Contour.PathSegments[BasePathSegmentIndex+0].Points[1]:=DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],1.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+1].Type_:=pstLine;
    Contour.PathSegments[BasePathSegmentIndex+1].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+1].Points[0]:=Contour.PathSegments[BasePathSegmentIndex+0].Points[1];
    Contour.PathSegments[BasePathSegmentIndex+1].Points[1]:=DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],2.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+2].Type_:=pstLine;
    Contour.PathSegments[BasePathSegmentIndex+2].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+2].Points[0]:=Contour.PathSegments[BasePathSegmentIndex+1].Points[1];
    Contour.PathSegments[BasePathSegmentIndex+2].Points[1]:=BasePathSegment.Points[1];
   end;
   pstQuadraticBezierCurve:begin
    Contour.PathSegments[BasePathSegmentIndex+0].Type_:=pstQuadraticBezierCurve;
    Contour.PathSegments[BasePathSegmentIndex+0].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+0].Points[0]:=BasePathSegment.Points[0];
    Contour.PathSegments[BasePathSegmentIndex+0].Points[1]:=DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],1.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+0].Points[2]:=DoublePrecisionPointLerp(DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],1.0/3.0),DoublePrecisionPointLerp(BasePathSegment.Points[1],BasePathSegment.Points[2],1.0/3.0),1.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+1].Type_:=pstQuadraticBezierCurve;
    Contour.PathSegments[BasePathSegmentIndex+1].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+1].Points[0]:=Contour.PathSegments[BasePathSegmentIndex+0].Points[2];
    Contour.PathSegments[BasePathSegmentIndex+1].Points[1]:=DoublePrecisionPointLerp(DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],5.0/9.0),DoublePrecisionPointLerp(BasePathSegment.Points[1],BasePathSegment.Points[2],4.0/9.0),0.5);
    Contour.PathSegments[BasePathSegmentIndex+1].Points[2]:=DoublePrecisionPointLerp(DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],2.0/3.0),DoublePrecisionPointLerp(BasePathSegment.Points[1],BasePathSegment.Points[2],2.0/3.0),2.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+2].Type_:=pstQuadraticBezierCurve;
    Contour.PathSegments[BasePathSegmentIndex+2].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+2].Points[0]:=Contour.PathSegments[BasePathSegmentIndex+1].Points[2];
    Contour.PathSegments[BasePathSegmentIndex+2].Points[1]:=DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],2.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+2].Points[2]:=BasePathSegment.Points[2];
   end;
   else begin
    Assert(false);
   end;
  end;
  InitializePathSegment(Contour.PathSegments[BasePathSegmentIndex+0]);
  InitializePathSegment(Contour.PathSegments[BasePathSegmentIndex+1]);
  InitializePathSegment(Contour.PathSegments[BasePathSegmentIndex+2]);
 end;
end;

procedure TpvFontDataGenerator.NormalizeShape;
var ContourIndex:TpvInt32;
    Contour:PpvFontPathContour;
begin
 for ContourIndex:=0 to Shape.CountContours-1 do begin
  Contour:=@Shape.Contours[ContourIndex];
  if Contour^.CountPathSegments=1 then begin
   try
    SplitPathSegmentIntoThreePartsInsideContour(Contour^,0);
   finally
    SetLength(Contour^.PathSegments,Contour^.CountPathSegments);
   end;
  end;
 end;
end;

procedure TpvFontDataGenerator.PathSegmentColorizeShape;
const AngleThreshold=3.0;
      EdgeThreshold=1.00000001;
type PCorner=^TCorner;
     TCorner=TpvInt32;
     TCorners=array of TCorner;
var ContourIndex,PathSegmentIndex,CountCorners,CornerIndex,SplineIndex,StartIndex,
    OtherPathSegmentIndex:TpvInt32;
    Seed:TpvUInt64;
    Contour:PpvFontPathContour;
    PathSegment:PpvFontPathSegment;
    Corners:TCorners;
    CurrentDirection,PreviousDirection,a,b:TpvFontDoublePrecisionPoint;
    CrossThreshold:TpvDouble;
    Color,InitialColor:TpvFontPathSegmentColor;
    Colors:array[0..2] of TpvFontPathSegmentColor;
    PathSegments:TpvFontPathSegments;
 procedure SwitchColor(var Color:TpvFontPathSegmentColor;const BannedColor:TpvFontPathSegmentColor=pscBlack);
 const StartColors:array[0..2] of TpvFontPathSegmentColor=(pscCyan,pscMagenta,pscYellow);
 var CombinedColor:TpvFontPathSegmentColor;
     Shifted:TpvUInt64;
 begin
  CombinedColor:=TpvFontPathSegmentColor(TpvInt32(TpvInt32(Color) and TpvInt32(BannedColor)));
  if CombinedColor in [pscRed,pscGreen,pscBlue] then begin
   Color:=TpvFontPathSegmentColor(TpvInt32(TpvInt32(CombinedColor) xor TpvInt32(TpvFontPathSegmentColor(pscWhite))));
  end else if CombinedColor in [pscBlack,pscWhite] then begin
   Color:=StartColors[Seed mod 3];
   Seed:=Seed div 3;
  end else begin
   Shifted:=TpvInt32(Color) shl (1+(Seed and 1));
   Color:=TpvFontPathSegmentColor(TpvInt32((Shifted or (Shifted shr 3)) and TpvInt32(TpvFontPathSegmentColor(pscWhite))));
   Seed:=Seed shr 1;
  end;
 end;
begin

 Seed:=$7ffffffffffffff;

 CrossThreshold:=sin(AngleThreshold);

 for ContourIndex:=0 to Shape.CountContours-1 do begin

  Contour:=@Shape.Contours[ContourIndex];
  try

   Corners:=nil;
   CountCorners:=0;
   try

    if Contour^.CountPathSegments>0 then begin

     PreviousDirection:=PathSegmentDirection(Contour^.PathSegments[Contour^.CountPathSegments-1],1);

     for PathSegmentIndex:=0 to Contour^.CountPathSegments-1 do begin

      PathSegment:=@Contour^.PathSegments[PathSegmentIndex];

      CurrentDirection:=PathSegmentDirection(PathSegment^,0);

      a:=DoublePrecisionPointNormalize(PreviousDirection);
      b:=DoublePrecisionPointNormalize(CurrentDirection);

      if (((a.x*b.x)+(a.y*b.y))<=0.0) or (abs((a.x*b.y)-(a.y*b.x))>CrossThreshold) then begin

       if length(Corners)<(CountCorners+1) then begin
        SetLength(Corners,(CountCorners+1)*2);
       end;
       Corners[CountCorners]:=PathSegmentIndex;
       inc(CountCorners);

      end;

      PreviousDirection:=PathSegmentDirection(PathSegment^,1);

     end;

    end;

    case CountCorners of
     0:begin
      for PathSegmentIndex:=0 to Contour^.CountPathSegments-1 do begin
       PathSegment:=@Contour^.PathSegments[PathSegmentIndex];
       PathSegment^.Color:=pscWhite;
      end;
     end;
     1:begin
      Colors[0]:=pscWhite;
      Colors[1]:=pscWhite;
      SwitchColor(Colors[0]);
      Colors[2]:=Colors[0];
      SwitchColor(Colors[2]);
      CornerIndex:=Corners[0];
      if Contour^.CountPathSegments>2 then begin
       for PathSegmentIndex:=0 to Contour^.CountPathSegments-1 do begin
        PathSegment:=@Contour^.PathSegments[CornerIndex];
        PathSegment^.Color:=Colors[abs((trunc(((3+((2.875*PathSegmentIndex)/(Contour^.CountPathSegments-1)))-1.4375)+0.5)-3)+1) mod 3];
        inc(CornerIndex);
        if CornerIndex>=Contour^.CountPathSegments then begin
         CornerIndex:=0;
        end;
       end;
      end else if Contour^.CountPathSegments=2 then begin
       PathSegments:=copy(Contour^.PathSegments,0,Contour^.CountPathSegments);
       try
        SetLength(Contour^.PathSegments,6);
        try
         Contour^.CountPathSegments:=6;
         SplitPathSegmentIntoThreePartsToContour(Contour^,CornerIndex*3,PathSegments[0]);
         SplitPathSegmentIntoThreePartsToContour(Contour^,3-(CornerIndex*3),PathSegments[1]);
         Contour^.PathSegments[0].Color:=Colors[0];
         Contour^.PathSegments[1].Color:=Colors[0];
         Contour^.PathSegments[2].Color:=Colors[1];
         Contour^.PathSegments[3].Color:=Colors[1];
         Contour^.PathSegments[4].Color:=Colors[2];
         Contour^.PathSegments[5].Color:=Colors[2];
        finally
         SetLength(Contour^.PathSegments,Contour^.CountPathSegments);
        end;
       finally
        PathSegments:=nil;
       end;
      end else if Contour^.CountPathSegments=1 then begin
       PathSegments:=copy(Contour^.PathSegments,0,Contour^.CountPathSegments);
       try
        SetLength(Contour^.PathSegments,3);
        try
         Contour^.CountPathSegments:=3;
         SplitPathSegmentIntoThreePartsToContour(Contour^,0,PathSegments[0]);
         Contour^.PathSegments[0].Color:=Colors[0];
         Contour^.PathSegments[1].Color:=Colors[1];
         Contour^.PathSegments[2].Color:=Colors[2];
        finally
         SetLength(Contour^.PathSegments,Contour^.CountPathSegments);
        end;
       finally
        PathSegments:=nil;
       end;
      end;
     end;
     else begin
      SplineIndex:=0;
      StartIndex:=Corners[0];
      Color:=pscWhite;
      SwitchColor(Color);
      InitialColor:=Color;
      for PathSegmentIndex:=0 to Contour^.CountPathSegments-1 do begin
       OtherPathSegmentIndex:=StartIndex+PathSegmentIndex;
       if OtherPathSegmentIndex>=Contour^.CountPathSegments then begin
        dec(OtherPathSegmentIndex,Contour^.CountPathSegments);
       end;
       if ((SplineIndex+1)<CountCorners) and (Corners[SplineIndex+1]=OtherPathSegmentIndex) then begin
        inc(SplineIndex);
        SwitchColor(Color,TpvFontPathSegmentColor(TpvInt32(IfThen(SplineIndex=(CountCorners-1),TpvInt32(InitialColor),TpvInt32(TpvFontPathSegmentColor(pscBlack))))));
       end;
       Contour^.PathSegments[OtherPathSegmentIndex].Color:=Color;
      end;
     end;
    end;

   finally
    Corners:=nil;
   end;

  finally
   SetLength(Contour^.PathSegments,Contour^.CountPathSegments);
  end;

 end;

end;

function TpvFontDataGenerator.GetLineNonClippedTime(const p,p0,p1:TpvFontDoublePrecisionPoint):TpvDouble;
var pAP,pAB:TpvFontDoublePrecisionPoint;
begin
 pAP.x:=p.x-p0.x;
 pAP.y:=p.y-p0.y;
 pAB.x:=p1.x-p0.x;
 pAB.y:=p1.y-p0.y;
 result:=((pAP.x*pAB.x)+(pAP.y*pAB.y))/(sqr(pAB.x)+sqr(pAB.y));
end;

function TpvFontDataGenerator.GetQuadraticBezierCurveNonClippedTime(const p,p0,p1,p2:TpvFontDoublePrecisionPoint):TpvDouble;
var b0,b1,b2,d21,d10,d20,gf,pp,d0p:TpvFontDoublePrecisionPoint;
    a,b,d,f,ap,bp,v,c:TpvDouble;
begin
 b0.x:=p0.x-p.x;
 b0.y:=p0.y-p.y;
 b1.x:=p1.x-p.x;
 b1.y:=p1.y-p.y;
 b2.x:=p2.x-p.x;
 b2.y:=p2.y-p.y;
 a:=((b0.x*b2.y)-(b0.y*b2.x))*2.0;
 b:=((b1.x*b0.y)-(b1.y*b0.x))*2.0;
 d:=((b2.x*b1.y)-(b2.y*b1.x))*2.0;
 c:=(2.0*a)+b+d;
 if IsZero(c) then begin
  result:=GetLineNonClippedTime(p,p0,p2);
 end else begin
  f:=(b*d)-sqr(a);
  d21.x:=b2.x-b1.x;
  d21.y:=b2.y-b1.y;
  d10.x:=b1.x-b0.x;
  d10.y:=b1.y-b0.y;
  d20.x:=b2.x-b0.x;
  d20.y:=b2.y-b0.y;
  gf.x:=((d21.y*b)+(d10.y*d)+(d20.y*a))*2.0;
  gf.y:=((d21.x*b)+(d10.x*d)+(d20.x*a))*(-2.0);
  v:=-(f/(sqr(gf.x)+sqr(gf.y)));
  pp.x:=gf.x*v;
  pp.y:=gf.y*v;
  d0p.x:=b0.x-pp.x;
  d0p.y:=b0.y-pp.y;
  ap:=(d0p.x*d20.y)-(d0p.y*d20.x);
  bp:=((d10.x*d0p.y)-(d10.y*d0p.x))*2.0;
  result:=(ap+bp)/c;
 end;
end;

function TpvFontDataGenerator.GetNonClampedSignedLineDistance(const p,p0,p1:TpvFontDoublePrecisionPoint):TpvDouble;
begin
 result:=((p.x*(p0.y-p1.y))+(p0.x*(p1.y-p.y))+(p1.x*(p.y-p0.y)))/sqrt(sqr(p1.x-p0.x)+sqr(p1.y-p0.y));
end;

procedure TpvFontDataGenerator.CalculateDistanceFieldDataLineRange(const FromY,ToY:TpvInt32);
var ContourIndex,PathSegmentIndex,x0,y0,x1,y1,x,y,PixelIndex,Dilation,DeltaWindingScore:TpvInt32;
    Contour:PpvFontPathContour;
    PathSegment:PpvFontPathSegment;
    PathSegmentBoundingBox:TpvFontBoundingBox;
    PreviousPathSegmentSide,PathSegmentSide:TpvFontPathSegmentSide;
    RowData:TpvFontRowData;
    DistanceFieldDataItem:PpvFontDistanceFieldDataItem;
    PointLeft,PointRight,Point,p0,p1,Direction,OriginPointDifference:TpvFontDoublePrecisionPoint;
    pX,pY,CurrentSquaredDistance,CurrentSquaredPseudoDistance,Time,Value:TpvDouble;
begin
 RowData.QuadraticXDirection:=0;
 for ContourIndex:=0 to Shape.CountContours-1 do begin
  Contour:=@Shape.Contours[ContourIndex];
  for PathSegmentIndex:=0 to Contour^.CountPathSegments-1 do begin
   PathSegment:=@Contour^.PathSegments[PathSegmentIndex];
   PathSegmentBoundingBox.Min.x:=PathSegment.BoundingBox.Min.x-VulkanFontDistanceFieldPadValue;
   PathSegmentBoundingBox.Min.y:=PathSegment.BoundingBox.Min.y-VulkanFontDistanceFieldPadValue;
   PathSegmentBoundingBox.Max.x:=PathSegment.BoundingBox.Max.x+VulkanFontDistanceFieldPadValue;
   PathSegmentBoundingBox.Max.y:=PathSegment.BoundingBox.Max.y+VulkanFontDistanceFieldPadValue;
   x0:=Clamp(Trunc(Floor(PathSegmentBoundingBox.Min.x)),0,DistanceField.Width-1);
   y0:=Clamp(Trunc(Floor(PathSegmentBoundingBox.Min.y)),0,DistanceField.Height-1);
   x1:=Clamp(Trunc(Ceil(PathSegmentBoundingBox.Max.x)),0,DistanceField.Width-1);
   y1:=Clamp(Trunc(Ceil(PathSegmentBoundingBox.Max.y)),0,DistanceField.Height-1);
{  x0:=0;
   y0:=0;
   x1:=DistanceField.Width-1;
   y1:=DistanceField.Height-1;}
   for y:=Max(FromY,y0) to Min(ToY,y1) do begin
    PreviousPathSegmentSide:=pssNone;
    pY:=y+0.5;
    PointLeft.x:=x0;
    PointLeft.y:=pY;
    PointRight.x:=x1;
    PointRight.y:=pY;
    if BetweenClosedOpen(pY,PathSegment.BoundingBox.Min.y,PathSegment.BoundingBox.Max.y) then begin
     PrecomputationForRow(RowData,PathSegment^,PointLeft,PointRight);
    end;
    for x:=x0 to x1 do begin
     PixelIndex:=(y*DistanceField.Width)+x;
     pX:=x+0.5;
     Point.x:=pX;
     Point.y:=pY;
     DistanceFieldDataItem:=@DistanceFieldData[PixelIndex];
     Dilation:=Clamp(Floor(sqrt(Max(1,DistanceFieldDataItem^.SquaredDistance))+0.5),1,VulkanFontDistanceFieldPadValue);
     PathSegmentBoundingBox.Min.x:=Floor(PathSegment.BoundingBox.Min.x)-VulkanFontDistanceFieldPadValue;
     PathSegmentBoundingBox.Min.y:=Floor(PathSegment.BoundingBox.Min.y)-VulkanFontDistanceFieldPadValue;
     PathSegmentBoundingBox.Max.x:=Ceil(PathSegment.BoundingBox.Max.x)+VulkanFontDistanceFieldPadValue;
     PathSegmentBoundingBox.Max.y:=Ceil(PathSegment.BoundingBox.Max.y)+VulkanFontDistanceFieldPadValue;
     if (Dilation<>VulkanFontDistanceFieldPadValue) and not
        (((x>=PathSegmentBoundingBox.Min.x) and (x<=PathSegmentBoundingBox.Max.x)) and
         ((y>=PathSegmentBoundingBox.Min.y) and (y<=PathSegmentBoundingBox.Max.y))) then begin
      continue;
     end else begin
      PathSegmentSide:=pssNone;
      CurrentSquaredDistance:=DistanceToPathSegment(Point,PathSegment^,RowData,PathSegmentSide);
      CurrentSquaredPseudoDistance:=CurrentSquaredDistance;
(**)  if MultiChannel then begin
       case PathSegment^.Type_ of
        pstLine:begin
         Time:=GetLineNonClippedTime(Point,PathSegment^.Points[0],PathSegment^.Points[1]);
        end;
        pstQuadraticBezierCurve:begin
         Time:=GetQuadraticBezierCurveNonClippedTime(Point,PathSegment^.Points[0],PathSegment^.Points[1],PathSegment^.Points[2]);
        end;
        else begin
         Time:=0.5;
        end;
       end;
       if Time<=0.0 then begin
        p0:=PathSegmentCornerPoint(PathSegment^,0,0)^;
        p1:=PathSegmentCornerPoint(PathSegment^,0,1)^;
        Direction:=DoublePrecisionPointNormalize(DoublePrecisionPointSub(p1,p0));
        OriginPointDifference:=DoublePrecisionPointSub(Point,p0);
        if DoublePrecisionPointDotProduct(OriginPointDifference,Direction)<0.0 then begin
         Value:=DoublePrecisionPointCrossProduct(OriginPointDifference,Direction);
//         Value:=GetNonClampedSignedLineDistance(Point,p0,p1);
         if abs(Value)<=abs(CurrentSquaredPseudoDistance) then begin
          CurrentSquaredPseudoDistance:=abs(Value);
         end;
        end;
{       Value:=GetNonClampedSignedLineDistance(Point,PathSegmentCornerPoint(PathSegment^,0,0)^,PathSegmentCornerPoint(PathSegment^,0,1)^);
        if Value<0.0 then begin
         Value:=sqr(Value);
         if abs(Value)<=abs(CurrentSquaredPseudoDistance) then begin
          CurrentSquaredPseudoDistance:=abs(Value);
         end;
        end;}
       end else if Time>=1.0 then begin
        p0:=PathSegmentCornerPoint(PathSegment^,1,0)^;
        p1:=PathSegmentCornerPoint(PathSegment^,1,1)^;
        Direction:=DoublePrecisionPointNormalize(DoublePrecisionPointSub(p1,p0));
        OriginPointDifference:=DoublePrecisionPointSub(Point,p1);
        if DoublePrecisionPointDotProduct(OriginPointDifference,Direction)>0.0 then begin
         Value:=DoublePrecisionPointCrossProduct(OriginPointDifference,Direction);
//         Value:=GetNonClampedSignedLineDistance(Point,p0,p1);
         if abs(Value)<=abs(CurrentSquaredPseudoDistance) then begin
          CurrentSquaredPseudoDistance:=abs(Value);
         end;
        end;
{       Value:=GetNonClampedSignedLineDistance(Point,PathSegmentCornerPoint(PathSegment^,1,0)^,PathSegmentCornerPoint(PathSegment^,1,1)^);
        if Value>0.0 then begin
         Value:=sqr(Value);
         if abs(Value)<=abs(CurrentSquaredPseudoDistance) then begin
          CurrentSquaredPseudoDistance:=abs(Value);
         end;
        end;}
       end;
      end;(**)
      if (PreviousPathSegmentSide=pssLeft) and (PathSegmentSide=pssRight) then begin
       DeltaWindingScore:=-1;
      end else if (PreviousPathSegmentSide=pssRight) and (PathSegmentSide=pssLeft) then begin
       DeltaWindingScore:=1;
      end else begin
       DeltaWindingScore:=0;
      end;
      PreviousPathSegmentSide:=PathSegmentSide;
      if CurrentSquaredDistance<DistanceFieldDataItem^.SquaredDistance then begin
       DistanceFieldDataItem^.SquaredDistance:=CurrentSquaredDistance;
      end;
      if MultiChannel then begin
       if (((TpvInt32(PathSegment^.Color) and TpvInt32(TpvFontPathSegmentColor(pscRed)))<>0)) and
          (CurrentSquaredDistance<DistanceFieldDataItem^.SquaredDistanceR) then begin
        DistanceFieldDataItem^.SquaredDistanceR:=CurrentSquaredDistance;
        DistanceFieldDataItem^.PseudoSquaredDistanceR:=CurrentSquaredPseudoDistance;
       end;
       if (((TpvInt32(PathSegment^.Color) and TpvInt32(TpvFontPathSegmentColor(pscGreen)))<>0)) and
          (CurrentSquaredDistance<DistanceFieldDataItem^.SquaredDistanceG) then begin
        DistanceFieldDataItem^.SquaredDistanceG:=CurrentSquaredDistance;
        DistanceFieldDataItem^.PseudoSquaredDistanceG:=CurrentSquaredPseudoDistance;
       end;
       if (((TpvInt32(PathSegment^.Color) and TpvInt32(TpvFontPathSegmentColor(pscBlue)))<>0)) and
          (CurrentSquaredDistance<DistanceFieldDataItem^.SquaredDistanceB) then begin
        DistanceFieldDataItem^.SquaredDistanceB:=CurrentSquaredDistance;
        DistanceFieldDataItem^.PseudoSquaredDistanceB:=CurrentSquaredPseudoDistance;
       end;
      end;
      inc(DistanceFieldDataItem^.DeltaWindingScore,DeltaWindingScore);
     end;
    end;
   end;
  end;
 end;
end;

procedure TpvFontDataGenerator.CalculateDistanceFieldDataLineRangeParallelForJobFunction(const Job:PPasMPJob;const ThreadIndex:TPasMPInt32;const Data:TpvPointer;const FromIndex,ToIndex:TPasMPNativeInt);
begin
 CalculateDistanceFieldDataLineRange(FromIndex,ToIndex);
end;

function TpvFontDataGenerator.PackDistanceFieldValue(Distance:TpvDouble):TpvUInt8;
begin
 result:=Clamp(Round((Distance*(128.0/VulkanFontDistanceFieldMagnitudeValue))+128.0),0,255);
end;

function TpvFontDataGenerator.PackPseudoDistanceFieldValue(Distance:TpvDouble):TpvUInt8;
begin
 result:=Clamp(Round((Distance*(128.0/VulkanFontDistanceFieldMagnitudeValue))+128.0),0,255);
end;

procedure TpvFontDataGenerator.ConvertToPointInPolygonPathSegments;
var ContourIndex,PathSegmentIndex,CountPathSegments:TpvInt32;
    Contour:PpvFontPathContour;
    PathSegment:PpvFontPathSegment;
    StartPoint,LastPoint:TpvFontDoublePrecisionPoint;
 procedure AddPathSegment(const p0,p1:TpvFontDoublePrecisionPoint);
 var Index:TpvInt32;
     PointInPolygonPathSegment:PpvFontPointInPolygonPathSegment;
 begin
  Index:=CountPathSegments;
  inc(CountPathSegments);
  if length(PointInPolygonPathSegments)<CountPathSegments then begin
   SetLength(PointInPolygonPathSegments,CountPathSegments*2);
  end;
  PointInPolygonPathSegment:=@PointInPolygonPathSegments[Index];
  PointInPolygonPathSegment^.Points[0]:=p0;
  PointInPolygonPathSegment^.Points[1]:=p1;
 end;
 procedure AddQuadraticBezierCurveAsSubdividedLinesToPathSegmentArray(const p0,p1,p2:TpvFontDoublePrecisionPoint);
 var LastPoint:TpvFontDoublePrecisionPoint;
  procedure LineToPointAt(const Point:TpvFontDoublePrecisionPoint);
  begin
   AddPathSegment(LastPoint,Point);
   LastPoint:=Point;
  end;
  procedure Recursive(const x1,y1,x2,y2,x3,y3:TpvDouble;const Level:TpvInt32);
  var x12,y12,x23,y23,x123,y123,dx,dy:TpvDouble;
      Point:TpvFontDoublePrecisionPoint;
  begin
   x12:=(x1+x2)*0.5;
   y12:=(y1+y2)*0.5;
   x23:=(x2+x3)*0.5;
   y23:=(y2+y3)*0.5;
   x123:=(x12+x23)*0.5;
   y123:=(y12+y23)*0.5;
   dx:=x3-x1;
   dy:=y3-y1;
   if (Level>CurveRecursionLimit) or
      ((Level>0) and
       (sqr(((x2-x3)*dy)-((y2-y3)*dx))<((sqr(dx)+sqr(dy))*CurveTessellationToleranceSquared))) then begin
    Point.x:=x3;
    Point.y:=y3;
    LineToPointAt(Point);
   end else begin
    Recursive(x1,y1,x12,y12,x123,y123,Level+1);
    Recursive(x123,y123,x23,y23,x3,y3,Level+1);
   end;
  end;
 begin
  LastPoint:=p0;
  Recursive(p0.x,p0.y,p1.x,p1.y,p2.x,p2.y,0);
  LineToPointAt(p2);
 end;
begin
 PointInPolygonPathSegments:=nil;
 CountPathSegments:=0;
 try
  for ContourIndex:=0 to Shape.CountContours-1 do begin
   Contour:=@Shape.Contours[ContourIndex];
   if Contour^.CountPathSegments>0 then begin
    StartPoint.x:=0.0;
    StartPoint.y:=0.0;
    LastPoint.x:=0.0;
    LastPoint.y:=0.0;
    for PathSegmentIndex:=0 to Contour^.CountPathSegments-1 do begin
     PathSegment:=@Contour^.PathSegments[PathSegmentIndex];
     case PathSegment^.Type_ of
      pstLine:begin
       if PathSegmentIndex=0 then begin
        StartPoint:=PathSegment^.Points[0];
       end;
       LastPoint:=PathSegment^.Points[1];
       AddPathSegment(PathSegment^.Points[0],PathSegment^.Points[1]);
      end;
      pstQuadraticBezierCurve:begin
       if PathSegmentIndex=0 then begin
        StartPoint:=PathSegment^.Points[0];
       end;
       LastPoint:=PathSegment^.Points[2];
       AddQuadraticBezierCurveAsSubdividedLinesToPathSegmentArray(PathSegment^.Points[0],PathSegment^.Points[1],PathSegment^.Points[2]);
      end;
     end;
    end;
    if not (SameValue(LastPoint.x,StartPoint.x) and SameValue(LastPoint.y,StartPoint.y)) then begin
     AddPathSegment(LastPoint,StartPoint);
    end;
   end;
  end;
 finally
  SetLength(PointInPolygonPathSegments,CountPathSegments);
 end;
end;

function TpvFontDataGenerator.GetWindingNumberAtPointInPolygon(const Point:TpvFontDoublePrecisionPoint):TpvInt32;
var Index,CaseIndex:TpvInt32;
    PointInPolygonPathSegment:PpvFontPointInPolygonPathSegment;
    x0,y0,x1,y1:TpvDouble;
begin
 result:=0;
 for Index:=0 to length(PointInPolygonPathSegments)-1 do begin
  PointInPolygonPathSegment:=@PointInPolygonPathSegments[Index];
  y0:=PointInPolygonPathSegment^.Points[0].y-Point.y;
  y1:=PointInPolygonPathSegment^.Points[1].y-Point.y;
  if y0<0.0 then begin
   CaseIndex:=0;
  end else if y0>0.0 then begin
   CaseIndex:=2;
  end else begin
   CaseIndex:=1;
  end;
  if y1<0.0 then begin
   inc(CaseIndex,0);
  end else if y1>0.0 then begin
   inc(CaseIndex,6);
  end else begin
   inc(CaseIndex,3);
  end;
  if CaseIndex in [1,2,3,6] then begin
   x0:=PointInPolygonPathSegment^.Points[0].x-Point.x;
   x1:=PointInPolygonPathSegment^.Points[1].x-Point.x;
   if not (((x0>0.0) and (x1>0.0)) or ((not ((x0<=0.0) and (x1<=0.0))) and ((x0-(y0*((x1-x0)/(y1-y0))))>0.0))) then begin
    if CaseIndex in [1,2] then begin
     inc(result);
    end else begin
     dec(result);
    end;
   end;
  end;
 end;
end;

function TpvFontDataGenerator.GenerateDistanceFieldPicture(const DistanceFieldData:TpvFontDistanceFieldData;const Width,Height,TryIteration:TpvInt32):boolean;
var x,y,PixelIndex,DistanceFieldSign,WindingNumber,Value:TpvInt32;
    DistanceFieldDataItem:PpvFontDistanceFieldDataItem;
    DistanceFieldPixel:PpvFontDistanceFieldPixel;
    p:TpvFontDoublePrecisionPoint;
begin

 result:=true;

 PixelIndex:=0;
 for y:=0 to Height-1 do begin
  WindingNumber:=0;
  for x:=0 to Width-1 do begin
   DistanceFieldDataItem:=@DistanceFieldData[PixelIndex];
   if TryIteration=2 then begin
    p.x:=x+0.5;
    p.y:=y+0.5;
    WindingNumber:=GetWindingNumberAtPointInPolygon(p);
   end else begin
    inc(WindingNumber,DistanceFieldDataItem^.DeltaWindingScore);
    if (x=(Width-1)) and (WindingNumber<>0) then begin
     result:=false;
     break;
    end;
   end;
   case FillRule of
    pvTTF_PolygonWindingRule_NONZERO:begin
     if WindingNumber<>0 then begin
      DistanceFieldSign:=1;
     end else begin
      DistanceFieldSign:=-1;
     end;
    end;
    else {pvTTF_PolygonWindingRule_EVENODD:}begin
     if (WindingNumber and 1)<>0 then begin
      DistanceFieldSign:=1;
     end else begin
      DistanceFieldSign:=-1;
     end;
    end;
   end;
   DistanceFieldPixel:=@DistanceField^.Pixels[PixelIndex];
   if MultiChannel then begin
    DistanceFieldPixel^.r:=PackPseudoDistanceFieldValue(sqrt(DistanceFieldDataItem^.PseudoSquaredDistanceR)*DistanceFieldSign);
    DistanceFieldPixel^.g:=PackPseudoDistanceFieldValue(sqrt(DistanceFieldDataItem^.PseudoSquaredDistanceG)*DistanceFieldSign);
    DistanceFieldPixel^.b:=PackPseudoDistanceFieldValue(sqrt(DistanceFieldDataItem^.PseudoSquaredDistanceB)*DistanceFieldSign);
    DistanceFieldPixel^.a:=PackDistanceFieldValue(sqrt(DistanceFieldDataItem^.SquaredDistance)*DistanceFieldSign);
   end else begin
    Value:=PackDistanceFieldValue(sqrt(DistanceFieldDataItem^.SquaredDistance)*DistanceFieldSign);
    DistanceFieldPixel^.r:=Value;
    DistanceFieldPixel^.g:=Value;
    DistanceFieldPixel^.b:=Value;
    DistanceFieldPixel^.a:=Value;
   end;
   inc(PixelIndex);
  end;
  if not result then begin
   break;
  end;
 end;

end;

procedure TpvFontDataGenerator.DoIt;
var TryIteration:TpvInt32;
    PasMPInstance:TPasMP;
begin

 PasMPInstance:=TPasMP.GetGlobalInstance;

 Initialize(Shape);
 try

  DistanceFieldData:=nil;
  try

   SetLength(DistanceFieldData,DistanceField.Width*DistanceField.Height);

   PointInPolygonPathSegments:=nil;
   try

    for TryIteration:=0 to 2 do begin
     case TryIteration of
      0,1:begin
       InitializeDistances;
       ConvertShape(TryIteration in [1,2]);
       if MultiChannel then begin
        NormalizeShape;
        PathSegmentColorizeShape;
        NormalizeShape;
       end;
      end;
      else {2:}begin
       ConvertToPointInPolygonPathSegments;
      end;
     end;
     PasMPInstance.Invoke(PasMPInstance.ParallelFor(nil,0,DistanceField.Height-1,CalculateDistanceFieldDataLineRangeParallelForJobFunction,1,10,nil,0));
     if GenerateDistanceFieldPicture(DistanceFieldData,DistanceField.Width,DistanceField.Height,TryIteration) then begin
      break;
     end else begin
      // Try it again, after all quadratic bezier curves were subdivided into lines at the next try iteration
     end;
    end;

   finally
    PointInPolygonPathSegments:=nil;
   end;

  finally
   DistanceFieldData:=nil;
  end;

 finally
  Finalize(Shape);
 end;

end;

constructor TpvFont.Create(const aDevice:TpvVulkanDevice;const aSpriteAtlas:TpvSpriteAtlas;const aTargetPPI:TpvInt32=72;const aBaseSize:TpvFloat=12.0);
begin

 inherited Create;

 fDevice:=aDevice;

 fSpriteAtlas:=aSpriteAtlas;

 fTargetPPI:=aTargetPPI;

 fUnitsPerEm:=72;

 fBaseScaleFactor:=1.0;

 fInverseBaseScaleFactor:=1.0;

 fBaseSize:=aBaseSize;

 fInverseBaseSize:=1.0/fBaseSize;

 fMinX:=0.0;
 fMinY:=0.0;
 fMaxX:=0.0;
 fMaxY:=0.0;

 fMinimumCodePoint:=High(TpvUInt32);
 fMaximumCodePoint:=Low(TpvUInt32);

 fCodePointBitmap:=nil;

 fGlyphs:=nil;

 fCodePointGlyphPairs:=nil;

 fKerningPairs:=nil;

 fCodePointToGlyphHashMap:=TpvFontInt64HashMap.Create(-1);

 fKerningPairHashMap:=TpvFontInt64HashMap.Create(-1);

 fDistanceFieldJobs:=nil;

end;

constructor TpvFont.CreateFromTrueTypeFont(const aDevice:TpvVulkanDevice;const aSpriteAtlas:TpvSpriteAtlas;const aTrueTypeFont:TpvTrueTypeFont;const aCodePointRanges:array of TpvFontCodePointRange);
const GlyphMetaDataScaleFactor=1.0;
      GlyphRasterizationScaleFactor=1.0/256.0;
var Index,TTFGlyphIndex,GlyphIndex,OtherGlyphIndex,CountGlyphs,
    CodePointGlyphPairIndex,CountCodePointGlyphPairs,
    TrueTypeFontKerningIndex,TrueTypeFontKerningPairIndex,
    KerningPairIndex,CountKerningPairs:TpvInt32;
    x0,y0,x1,y1:TpvDouble;
    KerningPairDoubleIndex:TpvUInt64;
    Int64Value:TpvInt64;
    CodePointRange:PpvFontCodePointRange;
    CodePointIndex,BitmapCodePointIndex:TpvUInt32;
    CodePointBitmap:TpvFontCodePointBitmap;
    CodePointToTTFGlyphHashMap:TpvFontInt64HashMap;
    TTFGlyphToGlyphHashMap:TpvFontInt64HashMap;
    GlyphToTTFGlyphHashMap:TpvFontInt64HashMap;
    KerningPairHashMap:TpvFontInt64HashMap;
    Glyph:PpvFontGlyph;
    GlyphBuffer:TpvTrueTypeFontGlyphBuffer;
    PolygonBuffers:TpvTrueTypeFontPolygonBuffers;
    SortedGlyphs:TPVulkanFontGlyphs;
    DistanceField:TpvFontDistanceField;
    TrueTypeFontKerningTable:PpvTrueTypeFontKerningTable;
    TrueTypeFontKerningPair:PpvTrueTypeFontKerningPair;
    CodePointGlyphPair:PpvFontCodePointGlyphPair;
    KerningPair:PpvFontKerningPair;
    GlyphDistanceField:PpvFontDistanceField;
    GlyphDistanceFields:TpvFontDistanceFields;
    PasMPInstance:TPasMP;
    GlyphDistanceFieldJob:PpvFontDistanceFieldJob;
begin

 Create(aDevice,aSpriteAtlas,aTrueTypeFont.TargetPPI,aTrueTypeFont.Size);

 PasMPInstance:=TPasMP.GetGlobalInstance;

 fUnitsPerEm:=aTrueTypeFont.GetUnitsPerEm;

 fBaseScaleFactor:=aTrueTypeFont.GetScaleFactor;

 fInverseBaseScaleFactor:=1.0/fBaseScaleFactor;

 fMinX:=aTrueTypeFont.MinX;
 fMinY:=aTrueTypeFont.MinY;
 fMaxX:=aTrueTypeFont.MaxX;
 fMaxY:=aTrueTypeFont.MaxY;

 for Index:=low(aCodePointRanges) to high(aCodePointRanges) do begin
  CodePointRange:=@aCodePointRanges[Index];
  fMinimumCodePoint:=Min(fMinimumCodePoint,Min(CodePointRange^.FromCodePoint,CodePointRange^.ToCodePoint));
  fMaximumCodePoint:=Max(fMaximumCodePoint,Max(CodePointRange^.FromCodePoint,CodePointRange^.ToCodePoint));
 end;

 if fMinimumCodePoint<=fMaximumCodePoint then begin

  SetLength(CodePointBitmap,((fMaximumCodePoint-fMinimumCodePoint)+32) shr 5);

  FillChar(CodePointBitmap[0],length(CodePointBitmap)*SizeOf(TpvUInt32),#0);

  for Index:=low(aCodePointRanges) to high(aCodePointRanges) do begin
   CodePointRange:=@aCodePointRanges[Index];
   for CodePointIndex:=Min(CodePointRange^.FromCodePoint,CodePointRange^.ToCodePoint) to Max(CodePointRange^.FromCodePoint,CodePointRange^.ToCodePoint) do begin
    BitmapCodePointIndex:=CodePointIndex-fMinimumCodePoint;
    CodePointBitmap[BitmapCodePointIndex shr 5]:=CodePointBitmap[BitmapCodePointIndex shr 5] or (TpvUInt32(1) shl (BitmapCodePointIndex and 31));
   end;
  end;

  TTFGlyphToGlyphHashMap:=TpvFontInt64HashMap.Create(-1);
  try

   GlyphToTTFGlyphHashMap:=TpvFontInt64HashMap.Create(-1);
   try

    // Collect used glyphs
    CodePointToTTFGlyphHashMap:=TpvFontInt64HashMap.Create(-1);
    try
     CountGlyphs:=0;
     CountCodePointGlyphPairs:=0;
     try
      for CodePointIndex:=fMinimumCodePoint to fMaximumCodePoint do begin
       BitmapCodePointIndex:=CodePointIndex-fMinimumCodePoint;
       if (CodePointBitmap[BitmapCodePointIndex shr 5] and (TpvUInt32(1) shl (BitmapCodePointIndex and 31)))<>0 then begin
        TTFGlyphIndex:=aTrueTypeFont.GetGlyphIndex(CodePointIndex);
        if (TTFGlyphIndex>=0) and (TTFGlyphIndex<aTrueTypeFont.CountGlyphs) then begin
         if not CodePointToTTFGlyphHashMap.ExistKey(CodePointIndex) then begin
          CodePointToTTFGlyphHashMap.Add(CodePointIndex,TTFGlyphIndex);
          if TTFGlyphToGlyphHashMap.TryGet(TTFGlyphIndex,Int64Value) then begin
           GlyphIndex:=Int64Value;
          end else begin
           GlyphIndex:=CountGlyphs;
           inc(CountGlyphs);
           TTFGlyphToGlyphHashMap.Add(TTFGlyphIndex,GlyphIndex);
           GlyphToTTFGlyphHashMap.Add(GlyphIndex,TTFGlyphIndex);
          end;
          fCodePointToGlyphHashMap.Add(CodePointIndex,GlyphIndex);
          CodePointGlyphPairIndex:=CountCodePointGlyphPairs;
          inc(CountCodePointGlyphPairs);
          if length(fCodePointGlyphPairs)<CountCodePointGlyphPairs then begin
           SetLength(fCodePointGlyphPairs,CountCodePointGlyphPairs*2);
          end;
          CodePointGlyphPair:=@fCodePointGlyphPairs[CodePointGlyphPairIndex];
          CodePointGlyphPair^.CodePoint:=CodePointIndex;
          CodePointGlyphPair^.Glyph:=GlyphIndex;
         end;
        end;
       end;
      end;
     finally
      SetLength(fGlyphs,CountGlyphs);
      SetLength(fCodePointGlyphPairs,CountCodePointGlyphPairs);
     end;
    finally
     CodePointToTTFGlyphHashMap.Free;
    end;

    // Convert glyph data and get polygon data
    PolygonBuffers:=nil;
    try

     SetLength(PolygonBuffers,CountGlyphs);

     for GlyphIndex:=0 to CountGlyphs-1 do begin

      Glyph:=@fGlyphs[GlyphIndex];

      FillChar(Glyph^,SizeOf(TpvFontGlyph),#0);

      if GlyphToTTFGlyphHashMap.TryGet(GlyphIndex,Int64Value) then begin

       TTFGlyphIndex:=Int64Value;

       Glyph^.AdvanceWidth:=aTrueTypeFont.GetGlyphAdvanceWidth(TTFGlyphIndex)*GlyphMetaDataScaleFactor;
       Glyph^.AdvanceHeight:=aTrueTypeFont.GetGlyphAdvanceHeight(TTFGlyphIndex)*GlyphMetaDataScaleFactor;
       Glyph^.LeftSideBearing:=aTrueTypeFont.GetGlyphLeftSideBearing(TTFGlyphIndex)*GlyphMetaDataScaleFactor;
       Glyph^.RightSideBearing:=aTrueTypeFont.GetGlyphRightSideBearing(TTFGlyphIndex)*GlyphMetaDataScaleFactor;
       Glyph^.TopSideBearing:=aTrueTypeFont.GetGlyphTopSideBearing(TTFGlyphIndex)*GlyphMetaDataScaleFactor;
       Glyph^.BottomSideBearing:=aTrueTypeFont.GetGlyphBottomSideBearing(TTFGlyphIndex)*GlyphMetaDataScaleFactor;
       Glyph^.BoundsMinX:=aTrueTypeFont.Glyphs[TTFGlyphIndex].Bounds.XMin*GlyphMetaDataScaleFactor;
       Glyph^.BoundsMinY:=aTrueTypeFont.Glyphs[TTFGlyphIndex].Bounds.YMin*GlyphMetaDataScaleFactor;
       Glyph^.BoundsMaxX:=aTrueTypeFont.Glyphs[TTFGlyphIndex].Bounds.XMax*GlyphMetaDataScaleFactor;
       Glyph^.BoundsMaxY:=aTrueTypeFont.Glyphs[TTFGlyphIndex].Bounds.YMax*GlyphMetaDataScaleFactor;

       GlyphBuffer.Points:=nil;
       PolygonBuffers[GlyphIndex].Commands:=nil;
       try

        if aTrueTypeFont.IsPostScriptGlyph(TTFGlyphIndex) then begin

         aTrueTypeFont.ResetPolygonBuffer(PolygonBuffers[GlyphIndex]);
         aTrueTypeFont.FillPostScriptPolygonBuffer(PolygonBuffers[GlyphIndex],TTFGlyphIndex);

        end else begin

         aTrueTypeFont.ResetGlyphBuffer(GlyphBuffer);
         aTrueTypeFont.FillGlyphBuffer(GlyphBuffer,TTFGlyphIndex);

         aTrueTypeFont.ResetPolygonBuffer(PolygonBuffers[GlyphIndex]);
         aTrueTypeFont.FillPolygonBuffer(PolygonBuffers[GlyphIndex],GlyphBuffer);

        end;

        aTrueTypeFont.GetPolygonBufferBounds(PolygonBuffers[GlyphIndex],x0,y0,x1,y1);

        Glyph^.OffsetX:=(x0*GlyphRasterizationScaleFactor)-(VulkanFontDistanceFieldSpreadValue*2.0);
        Glyph^.OffsetY:=(y0*GlyphRasterizationScaleFactor)-(VulkanFontDistanceFieldSpreadValue*2.0);
        Glyph^.Width:=Max(1,ceil(((x1-x0)*GlyphRasterizationScaleFactor)+(VulkanFontDistanceFieldSpreadValue*4.0)));
        Glyph^.Height:=Max(1,ceil(((y1-y0)*GlyphRasterizationScaleFactor)+(VulkanFontDistanceFieldSpreadValue*4.0)));

       finally
        GlyphBuffer.Points:=nil;
       end;

      end;
     end;

     GlyphDistanceFields:=nil;
     try

      SetLength(GlyphDistanceFields,CountGlyphs);

      fDistanceFieldJobs:=nil;
      try

       SetLength(fDistanceFieldJobs,CountGlyphs);

       // Rasterize glyph signed distance field sprites
       for GlyphIndex:=0 to CountGlyphs-1 do begin
        Glyph:=@fGlyphs[GlyphIndex];
        GlyphDistanceField:=@GlyphDistanceFields[GlyphIndex];
        GlyphDistanceField^.OffsetX:=-Glyph^.OffsetX;
        GlyphDistanceField^.OffsetY:=-Glyph^.OffsetY;
        GlyphDistanceField^.Width:=Max(1,Glyph^.Width);
        GlyphDistanceField^.Height:=Max(1,Glyph^.Height);
        GlyphDistanceField^.Pixels:=nil;
        SetLength(GlyphDistanceField^.Pixels,GlyphDistanceField^.Width*GlyphDistanceField^.Height);
        GlyphDistanceFieldJob:=@fDistanceFieldJobs[GlyphIndex];
        GlyphDistanceFieldJob^.DistanceField:=GlyphDistanceField;
        GlyphDistanceFieldJob^.MultiChannel:=false;
        GlyphDistanceFieldJob^.PolygonBuffer:=PolygonBuffers[GlyphIndex];
//      GenerateSignedDistanceField(GlyphDistanceField^,false,PolygonBuffers[GlyphIndex],pvTTF_PolygonWindingRule_NONZERO);
       end;

       if CountGlyphs>0 then begin
        PasMPInstance.Invoke(PasMPInstance.ParallelFor(@fDistanceFieldJobs[0],0,CountGlyphs-1,GenerateSignedDistanceFieldParallelForJobFunction,1,10,nil,0));
       end;

      finally
       fDistanceFieldJobs:=nil;
      end;

      // Insert glyph signed distance field sprites by sorted area size order
      SortedGlyphs:=nil;
      try

       SetLength(SortedGlyphs,length(fGlyphs));

       for GlyphIndex:=0 to length(fGlyphs)-1 do begin
        SortedGlyphs[GlyphIndex]:=@fGlyphs[GlyphIndex];
       end;

       if length(SortedGlyphs)>1 then begin
        IndirectIntroSort(@SortedGlyphs[0],0,length(SortedGlyphs)-1,CompareVulkanFontGlyphsByArea);
       end;

       for GlyphIndex:=0 to length(SortedGlyphs)-1 do begin
        Glyph:=SortedGlyphs[GlyphIndex];
        if (Glyph^.Width>0) and (Glyph^.Height>0) then begin
         OtherGlyphIndex:={%H-}((TpvPtrUInt(TpvPointer(Glyph))-TpvPtrUInt(TpvPointer(@fGlyphs[0])))) div SizeOf(TpvFontGlyph);
         Glyph^.Sprite:=aSpriteAtlas.LoadRawSprite(TpvRawByteString(String('glyph'+IntToStr(OtherGlyphIndex))),
                                                   @GlyphDistanceFields[OtherGlyphIndex].Pixels[0],
                                                   Glyph^.Width,
                                                   Glyph^.Height,
                                                   false);
        end;
       end;

      finally
       SortedGlyphs:=nil;
      end;

     finally
      GlyphDistanceFields:=nil;
     end;

    finally
     PolygonBuffers:=nil;
    end;

   finally
    GlyphToTTFGlyphHashMap.Free;
   end;

   // Convert kerning pair lookup data
   fKerningPairs:=nil;
   CountKerningPairs:=0;
   try
    KerningPairHashMap:=TpvFontInt64HashMap.Create(-1);
    try
     for TrueTypeFontKerningIndex:=0 to length(aTrueTypeFont.KerningTables)-1 do begin
      TrueTypeFontKerningTable:=@aTrueTypeFont.KerningTables[TrueTypeFontKerningIndex];
      for TrueTypeFontKerningPairIndex:=0 to length(TrueTypeFontKerningTable^.KerningPairs)-1 do begin
       TrueTypeFontKerningPair:=@TrueTypeFontKerningTable^.KerningPairs[TrueTypeFontKerningPairIndex];
       if TTFGlyphToGlyphHashMap.TryGet(TrueTypeFontKerningPair^.Left,Int64Value) then begin
        GlyphIndex:=Int64Value;
        if TTFGlyphToGlyphHashMap.TryGet(TrueTypeFontKerningPair^.Right,Int64Value) then begin
         OtherGlyphIndex:=Int64Value;
         KerningPairDoubleIndex:=CombineTwoUInt32IntoOneUInt64(GlyphIndex,OtherGlyphIndex);
         if not KerningPairHashMap.ExistKey(KerningPairDoubleIndex) then begin
          KerningPairIndex:=CountKerningPairs;
          inc(CountKerningPairs);
          if length(fKerningPairs)<CountKerningPairs then begin
           SetLength(fKerningPairs,CountKerningPairs*2);
          end;
          KerningPairHashMap.Add(KerningPairDoubleIndex,KerningPairIndex);
          KerningPair:=@fKerningPairs[KerningPairIndex];
          KerningPair^.Left:=TrueTypeFontKerningPair^.Left;
          KerningPair^.Right:=TrueTypeFontKerningPair^.Right;
          KerningPair^.Horizontal:=aTrueTypeFont.GetKerning(KerningPair^.Left,KerningPair^.Right,true);
          KerningPair^.Vertical:=aTrueTypeFont.GetKerning(KerningPair^.Left,KerningPair^.Right,false);
         end;
        end;
       end;
      end;
     end;
    finally
     KerningPairHashMap.Free;
    end;
   finally
    SetLength(fKerningPairs,CountKerningPairs);
    if length(fKerningPairs)>1 then begin
     UntypedDirectIntroSort(@fKerningPairs[0],0,length(fKerningPairs)-1,SizeOf(TpvFontKerningPair),CompareVulkanFontKerningPairs);
    end;
    for KerningPairIndex:=0 to length(fKerningPairs)-1 do begin
     KerningPair:=@fKerningPairs[KerningPairIndex];
     KerningPairDoubleIndex:=CombineTwoUInt32IntoOneUInt64(KerningPair^.Left,KerningPair^.Right);
     fKerningPairHashMap.Add(KerningPairDoubleIndex,KerningPairIndex);
    end;
   end;

  finally
   TTFGlyphToGlyphHashMap.Free;
  end;

 end;

end;

destructor TpvFont.Destroy;
begin

 fCodePointBitmap:=nil;

 fGlyphs:=nil;

 fCodePointGlyphPairs:=nil;

 fKerningPairs:=nil;

 fCodePointToGlyphHashMap.Free;

 fKerningPairHashMap.Free;

 fDistanceFieldJobs:=nil;

 inherited Destroy;
end;

class function TpvFont.CodePointRange(const aFromCodePoint,aToCodePoint:TpvUInt32): TpvFontCodePointRange;
begin
 result.FromCodePoint:=Min(aFromCodePoint,aToCodePoint);
 result.ToCodePoint:=Max(aFromCodePoint,aToCodePoint);
end;

class function TpvFont.CodePointRange(const aFromCodePoint,aToCodePoint:WideChar):TpvFontCodePointRange;
begin
 result.FromCodePoint:=Min(TpvUInt16(WideChar(aFromCodePoint)),TpvUInt16(WideChar(aToCodePoint)));
 result.ToCodePoint:=Max(TpvUInt16(WideChar(aFromCodePoint)),TpvUInt16(WideChar(aToCodePoint)));
end;

class function TpvFont.CodePointRange(const aCharacterRange:TpvFontCharacterRange):TpvFontCodePointRange;
var Index:AnsiChar;
begin
 result.FromCodePoint:=High(TpvUInt32);
 result.ToCodePoint:=Low(TpvUInt32);
 for Index:=Low(AnsiChar) to High(AnsiChar) do begin
  if Index in aCharacterRange then begin
   result.FromCodePoint:=TpvUInt8(AnsiChar(Index));
   break;
  end;
 end;
 for Index:=High(AnsiChar) downto Low(AnsiChar) do begin
  if Index in aCharacterRange then begin
   result.ToCodePoint:=TpvUInt8(AnsiChar(Index));
   break;
  end;
 end;
end;

function TpvFont.GetScaleFactor(const aSize:TpvFloat):TpvFloat;
begin
 if aSize<0.0 then begin
  result:=(-aSize)/fUnitsPerEm;
 end else begin
  result:=(aSize*fTargetPPI)/(fUnitsPerEm*72);
 end;
end;

procedure TpvFont.GenerateSignedDistanceField(var DistanceField:TpvFontDistanceField;const MultiChannel:boolean;const PolygonBuffer:TpvTrueTypeFontPolygonBuffer;const FillRule:TpvInt32);
var DataGenerator:TpvFontDataGenerator;
begin
 DataGenerator:=TpvFontDataGenerator.Create;
 try
  DataGenerator.PointInPolygonPathSegments:=nil;
  DataGenerator.PolygonBuffer:=@PolygonBuffer;
  DataGenerator.DistanceField:=@DistanceField;
  DataGenerator.MultiChannel:=MultiChannel;
  DataGenerator.FillRule:=FillRule;
  DataGenerator.DoIt;
 finally
  DataGenerator.Free;
 end;
end;

procedure TpvFont.GenerateSignedDistanceFieldParallelForJobFunction(const Job:PPasMPJob;const ThreadIndex:TPasMPInt32;const Data:TpvPointer;const FromIndex,ToIndex:TPasMPNativeInt);
var Index:TPasMPNativeInt;
    JobData:PpvFontDistanceFieldJob;
begin
 Index:=FromIndex;
 while Index<=ToIndex do begin
  JobData:=@fDistanceFieldJobs[Index];
  GenerateSignedDistanceField(JobData^.DistanceField^,JobData^.MultiChannel,JobData^.PolygonBuffer,pvTTF_PolygonWindingRule_NONZERO);
  inc(Index);
 end;
end;

function TpvFont.TextWidth(const aText:TpvUTF8String;const aSize:TpvFloat):TpvFloat;
var TextIndex,CurrentGlyph,LastGlyph:TpvInt32;
    CurrentCodePoint:TpvUInt32;
    Width,NewWidth:TpvFloat;
    Int64Value:TpvInt64;
    Glyph:PpvFontGlyph;
begin
 result:=0.0;
 Width:=0.0;
 TextIndex:=1;
 LastGlyph:=-1;
 while TextIndex<=length(aText) do begin
  CurrentCodePoint:=PUCUUTF8CodeUnitGetCharAndIncFallback(aText,TextIndex);
  if fCodePointToGlyphHashMap.TryGet(CurrentCodePoint,Int64Value) then begin
   CurrentGlyph:=Int64Value;
   if (CurrentGlyph>=0) or (CurrentGlyph<length(fGlyphs)) then begin
    if ((LastGlyph>=0) and (LastGlyph<length(fGlyphs))) and
       fKerningPairHashMap.TryGet(CombineTwoUInt32IntoOneUInt64(LastGlyph,CurrentGlyph),Int64Value) then begin
     result:=result+fKerningPairs[Int64Value].Horizontal;
    end;
    Glyph:=@fGlyphs[CurrentGlyph];
    if LastGlyph<0 then begin
     result:=result+Glyph^.LeftSideBearing;
    end;
    NewWidth:=result+(Glyph^.BoundsMaxX-Glyph^.BoundsMinX);
    if Width<NewWidth then begin
     Width:=NewWidth;
    end;
    result:=result+Glyph^.AdvanceWidth;
   end;
  end else begin
   CurrentGlyph:=0;
  end;
  LastGlyph:=CurrentGlyph;
 end;
 if result=0 then begin
  result:=fMaxX-fMinX;
 end;
 if result<Width then begin
  result:=Width;
 end;
 result:=result*GetScaleFactor(aSize);
end;

function TpvFont.TextHeight(const aText:TpvUTF8String;const aSize:TpvFloat):TpvFloat;
var TextIndex,CurrentGlyph,LastGlyph:TpvInt32;
    CurrentCodePoint:TpvUInt32;
    Height,NewHeight:TpvFloat;
    Int64Value:TpvInt64;
    Glyph:PpvFontGlyph;
begin
 result:=0.0;
 Height:=0.0;
 TextIndex:=1;
 LastGlyph:=-1;
 while TextIndex<=length(aText) do begin
  CurrentCodePoint:=PUCUUTF8CodeUnitGetCharAndIncFallback(aText,TextIndex);
  if fCodePointToGlyphHashMap.TryGet(CurrentCodePoint,Int64Value) then begin
   CurrentGlyph:=Int64Value;
   if (CurrentGlyph>=0) or (CurrentGlyph<length(fGlyphs)) then begin
    if ((LastGlyph>=0) and (LastGlyph<length(fGlyphs))) and
       fKerningPairHashMap.TryGet(CombineTwoUInt32IntoOneUInt64(LastGlyph,CurrentGlyph),Int64Value) then begin
     result:=result+fKerningPairs[Int64Value].Vertical;
    end;
    Glyph:=@fGlyphs[CurrentGlyph];
    if LastGlyph<0 then begin
     result:=result+Glyph^.TopSideBearing;
    end;
    NewHeight:=result+(Glyph^.BoundsMaxY-Glyph^.BoundsMinY);
    if Height<NewHeight then begin
     Height:=NewHeight;
    end;
    result:=result+Glyph^.AdvanceHeight;
   end;
  end else begin
   CurrentGlyph:=0;
  end;
  LastGlyph:=CurrentGlyph;
 end;
 if result=0 then begin
  result:=fMaxY-fMinY;
 end;
 if result<Height then begin
  result:=Height;
 end;
 result:=result*GetScaleFactor(aSize);
end;

function TpvFont.TextSize(const aText:TpvUTF8String;const aSize:TpvFloat):TpvVector2;
var TextIndex,CurrentGlyph,LastGlyph:TpvInt32;
    CurrentCodePoint:TpvUInt32;
    Width,NewWidth,Height,NewHeight:TpvFloat;
    Int64Value:TpvInt64;
    Glyph:PpvFontGlyph;
    KerningPair:PpvFontKerningPair;
begin
 result.x:=0.0;
 result.y:=0.0;
 Width:=0.0;
 Height:=0.0;
 TextIndex:=1;
 LastGlyph:=-1;
 while TextIndex<=length(aText) do begin
  CurrentCodePoint:=PUCUUTF8CodeUnitGetCharAndIncFallback(aText,TextIndex);
  if fCodePointToGlyphHashMap.TryGet(CurrentCodePoint,Int64Value) then begin
   CurrentGlyph:=Int64Value;
   if (CurrentGlyph>=0) or (CurrentGlyph<length(fGlyphs)) then begin
    if ((LastGlyph>=0) and (LastGlyph<length(fGlyphs))) and
       fKerningPairHashMap.TryGet(CombineTwoUInt32IntoOneUInt64(LastGlyph,CurrentGlyph),Int64Value) then begin
     KerningPair:=@fKerningPairs[Int64Value];
     result.x:=result.x+KerningPair^.Horizontal;
     result.y:=result.y+KerningPair^.Vertical;
    end;
    Glyph:=@fGlyphs[CurrentGlyph];
    if LastGlyph<0 then begin
     result.x:=result.x+Glyph^.LeftSideBearing;
     result.y:=result.y+Glyph^.TopSideBearing;
    end;
    NewWidth:=result.x+(Glyph^.BoundsMaxX-Glyph^.BoundsMinX);
    if Width<NewWidth then begin
     Width:=NewWidth;
    end;
    NewHeight:=result.y+(Glyph^.BoundsMaxY-Glyph^.BoundsMinY);
    if Height<NewHeight then begin
     Height:=NewHeight;
    end;
    result.x:=result.x+Glyph^.AdvanceWidth;
    result.y:=result.y+Glyph^.AdvanceHeight;
   end;
  end else begin
   CurrentGlyph:=0;
  end;
  LastGlyph:=CurrentGlyph;
 end;
 if result.x=0 then begin
  result.x:=fMaxX-fMinX;
 end;
 if result.x<Width then begin
  result.x:=Width;
 end;
 if result.y=0 then begin
  result.y:=fMaxY-fMinY;
 end;
 if result.y<Height then begin
  result.y:=Height;
 end;
 result:=result*GetScaleFactor(aSize);
end;

function TpvFont.RowHeight(const Percent:TpvFloat):TpvFloat;
begin
 result:=fUnitsPerEm*(Percent*0.01);
end;

procedure TpvFont.Draw(const aCanvas:TObject;const aText:TpvUTF8String;const aPosition:TpvVector2;const aSize:TpvFloat);
var TextIndex,CurrentCodePoint,CurrentGlyph,LastGlyph:TpvInt32;
    x,y,ScaleFactor,RescaleFactor:TpvFloat;
    Int64Value:TpvInt64;
    KerningPair:PpvFontKerningPair;
    Glyph:PpvFontGlyph;
    Src,Dest:TpvRect;
begin
 x:=0.0;
 y:=0.0;
 ScaleFactor:=GetScaleFactor(aSize);
 RescaleFactor:=ScaleFactor*fInverseBaseScaleFactor;
 TextIndex:=1;
 LastGlyph:=-1;
 while TextIndex<=length(aText) do begin
  CurrentCodePoint:=PUCUUTF8CodeUnitGetCharAndIncFallback(aText,TextIndex);
  if fCodePointToGlyphHashMap.TryGet(CurrentCodePoint,Int64Value) then begin
   CurrentGlyph:=Int64Value;
   if (CurrentGlyph>=0) or (CurrentGlyph<length(fGlyphs)) then begin
    if ((LastGlyph>=0) and (LastGlyph<length(fGlyphs))) and
       fKerningPairHashMap.TryGet(CombineTwoUInt32IntoOneUInt64(LastGlyph,CurrentGlyph),Int64Value) then begin
     KerningPair:=@fKerningPairs[Int64Value];
     x:=x+KerningPair^.Horizontal;
     y:=y+KerningPair^.Vertical;
    end;
    Glyph:=@fGlyphs[CurrentGlyph];
    if LastGlyph<0 then begin
     x:=x+Glyph^.LeftSideBearing;
     y:=y+Glyph^.TopSideBearing;
    end;
    Src.Left:=0.0;
    Src.Top:=0.0;
    Src.Right:=Src.Left+Glyph^.Width;
    Src.Bottom:=Src.Top+Glyph^.Height;
    Dest.Left:=aPosition.x+(x*ScaleFactor)+(Glyph^.OffsetX*RescaleFactor);
    Dest.Top:=aPosition.y+(y*ScaleFactor)+(Glyph^.OffsetY*RescaleFactor);
    Dest.Right:=aPosition.x+(x*ScaleFactor)+((Glyph^.OffsetX+Glyph^.Width)*RescaleFactor);
    Dest.Bottom:=aPosition.y+(y*ScaleFactor)+((Glyph^.OffsetY+Glyph^.Height)*RescaleFactor);
    TpvCanvas(aCanvas).DrawFontGlyphSprite(Glyph^.Sprite,Src,Dest);
    x:=x+Glyph^.AdvanceWidth;
    y:=y+Glyph^.AdvanceHeight;
   end;
  end else begin
   CurrentGlyph:=0;
  end;
  LastGlyph:=CurrentGlyph;
 end;
end;

end.
