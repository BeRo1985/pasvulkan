(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2020, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.SignedDistanceField2D;
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
     PasVulkan.VectorPath,
     PasVulkan.Sprites;

type PpvSignedDistanceField2DPixel=^TpvSignedDistanceField2DPixel;
     TpvSignedDistanceField2DPixel=packed record
      r,g,b,a:TpvUInt8;
     end;

     TpvSignedDistanceField2DPixels=array of TpvSignedDistanceField2DPixel;

     PpvSignedDistanceField2D=^TpvSignedDistanceField2D;
     TpvSignedDistanceField2D=record
      Width:TpvInt32;
      Height:TpvInt32;
      Pixels:TpvSignedDistanceField2DPixels;
     end;

     TpvSignedDistanceField2DArray=array of TpvSignedDistanceField2D;

     PpvSignedDistanceField2DPathSegmentSide=^TpvSignedDistanceField2DPathSegmentSide;
     TpvSignedDistanceField2DPathSegmentSide=
      (
       Left=-1,
       On=0,
       Right=1,
       None=2
      );

     PpvSignedDistanceField2DDataItem=^TpvSignedDistanceField2DDataItem;
     TpvSignedDistanceField2DDataItem=record
      SquaredDistance:TpvFloat;
      SquaredDistanceR:TpvFloat;
      SquaredDistanceG:TpvFloat;
      SquaredDistanceB:TpvFloat;
      Distance:TpvFloat;
      PseudoSquaredDistanceR:TpvFloat;
      PseudoSquaredDistanceG:TpvFloat;
      PseudoSquaredDistanceB:TpvFloat;
      DeltaWindingScore:TpvInt32;
     end;

     TpvSignedDistanceField2DData=array of TpvSignedDistanceField2DDataItem;

     TpvSignedDistanceField2DDoublePrecisionPoint=record
      x:TpvDouble;
      y:TpvDouble;
     end;

     PpvSignedDistanceField2DDoublePrecisionPoint=^TpvSignedDistanceField2DDoublePrecisionPoint;

     TpvSignedDistanceField2DDoublePrecisionPoints=array[0..65535] of TpvSignedDistanceField2DDoublePrecisionPoint;

     PpvSignedDistanceField2DDoublePrecisionPoints=^TpvSignedDistanceField2DDoublePrecisionPoints;

     PpvSignedDistanceField2DDoublePrecisionAffineMatrix=^TpvSignedDistanceField2DDoublePrecisionAffineMatrix;
     TpvSignedDistanceField2DDoublePrecisionAffineMatrix=array[0..5] of TpvDouble;

     PpvSignedDistanceField2DPathSegmentType=^TpvSignedDistanceField2DPathSegmentType;
     TpvSignedDistanceField2DPathSegmentType=
      (
       Line,
       QuadraticBezierCurve
      );

     PpvSignedDistanceField2DBoundingBox=^TpvSignedDistanceField2DBoundingBox;
     TpvSignedDistanceField2DBoundingBox=record
      Min:TpvSignedDistanceField2DDoublePrecisionPoint;
      Max:TpvSignedDistanceField2DDoublePrecisionPoint;
     end;

     PpvSignedDistanceField2DPathSegmentColor=^TpvSignedDistanceField2DPathSegmentColor;
     TpvSignedDistanceField2DPathSegmentColor=
      (
       Black=0,
       Red=1,
       Green=2,
       Yellow=3,
       Blue=4,
       Magenta=5,
       Cyan=6,
       White=7
      );

     PpvSignedDistanceField2DPathSegmentPoints=^TpvSignedDistanceField2DPathSegmentPoints;
     TpvSignedDistanceField2DPathSegmentPoints=array[0..2] of TpvSignedDistanceField2DDoublePrecisionPoint;

     PpvSignedDistanceField2DPathSegment=^TpvSignedDistanceField2DPathSegment;
     TpvSignedDistanceField2DPathSegment=record
      Type_:TpvSignedDistanceField2DPathSegmentType;
      Color:TpvSignedDistanceField2DPathSegmentColor;
      Points:TpvSignedDistanceField2DPathSegmentPoints;
      P0T,P2T:TpvSignedDistanceField2DDoublePrecisionPoint;
      XFormMatrix:TpvSignedDistanceField2DDoublePrecisionAffineMatrix;
      ScalingFactor:TpvDouble;
      SquaredScalingFactor:TpvDouble;
      NearlyZeroScaled:TpvDouble;
      SquaredTangentToleranceScaled:TpvDouble;
      BoundingBox:TpvSignedDistanceField2DBoundingBox;
     end;

     TpvSignedDistanceField2DPathSegments=array of TpvSignedDistanceField2DPathSegment;

     PpvSignedDistanceField2DPathContour=^TpvSignedDistanceField2DPathContour;
     TpvSignedDistanceField2DPathContour=record
      PathSegments:TpvSignedDistanceField2DPathSegments;
      CountPathSegments:TpvInt32;
     end;

     TpvSignedDistanceField2DPathContours=array of TpvSignedDistanceField2DPathContour;

     PpvSignedDistanceField2DShape=^TpvSignedDistanceField2DShape;
     TpvSignedDistanceField2DShape=record
      Contours:TpvSignedDistanceField2DPathContours;
      CountContours:TpvInt32;
     end;

     PpvSignedDistanceField2DRowDataIntersectionType=^TpvSignedDistanceField2DRowDataIntersectionType;
     TpvSignedDistanceField2DRowDataIntersectionType=
      (
       NoIntersection,
       VerticalLine,
       TangentLine,
       TwoPointsIntersect
      );

     PpvSignedDistanceField2DRowData=^TpvSignedDistanceField2DRowData;
     TpvSignedDistanceField2DRowData=record
      IntersectionType:TpvSignedDistanceField2DRowDataIntersectionType;
      QuadraticXDirection:TpvInt32;
      ScanlineXDirection:TpvInt32;
      YAtIntersection:TpvFloat;
      XAtIntersection:array[0..1] of TpvFloat;
     end;

     PpvSignedDistanceField2DPointInPolygonPathSegment=^TpvSignedDistanceField2DPointInPolygonPathSegment;
     TpvSignedDistanceField2DPointInPolygonPathSegment=record
      Points:array[0..1] of TpvSignedDistanceField2DDoublePrecisionPoint;
     end;

     TpvSignedDistanceField2DPointInPolygonPathSegments=array of TpvSignedDistanceField2DPointInPolygonPathSegment;

     EpvSignedDistanceField2DMSDFGenerator=class(Exception);

     { TpvSignedDistanceField2DMSDFGenerator }

     TpvSignedDistanceField2DMSDFGenerator=class
      public
       type { TVector2 }
            TVector2=record
             public
              x:TpvDouble;
              y:TpvDouble;
              constructor Create(const aValue:TpvDouble); overload;
              constructor Create(const aX,aY:TpvDouble); overload;
              function Length:TpvDouble;
              function Direction:TpvDouble;
              function Normalize:TVector2;
              function Dot(const aRight:TVector2):TpvDouble;
              function Cross(const aRight:TVector2):TpvDouble;
              function OrthoNormal:TVector2;
              function Lerp(const b:TVector2;const t:TpvDouble):TVector2;
              class operator Equal(const a,b:TVector2):boolean;
              class operator NotEqual(const a,b:TVector2):boolean;
              class operator Add(const a,b:TVector2):TVector2;
              class operator Subtract(const a,b:TVector2):TVector2;
              class operator Multiply(const a,b:TVector2):TVector2; overload;
              class operator Multiply(const a:TVector2;const b:TpvDouble):TVector2; overload;
              class operator Divide(const a,b:TVector2):TVector2; overload;
              class operator Divide(const a:TVector2;const b:TpvDouble):TVector2; overload;
              class operator Negative(const a:TVector2):TVector2;
              class operator Positive(const a:TVector2):TVector2;
            end;
            PVector2=^TVector2;
       const InfinateDistance=-1e240;
       type { TSignedDistance }
            TSignedDistance=record
             public
              Distance:TpvDouble;
              Dot:TpvDouble;
              constructor Create(const aDistance,aDot:TpvDouble);
              class function Empty:TSignedDistance; static;
              class operator LessThan(const a,b:TSignedDistance):boolean;
            end;
            { TBounds }
            TBounds=record
             public
              l:TpvDouble;
              b:TpvDouble;
              r:TpvDouble;
              t:TpvDouble;
              procedure PointBounds(const p:TpvSignedDistanceField2DMSDFGenerator.TVector2);
            end;
            TEdgeColor=
             (
	            BLACK=0,
	            RED=1,
	            GREEN=2,
	            YELLOW=3,
	            BLUE=4,
	            MAGENTA=5,
	            CYAN=6,
	            WHITE=7
             );
            TEdgeType=
             (
	            LINEAR=0,
	            QUADRATIC=1,
	            CUBIC=2
             );
       const TOO_LARGE_RATIO=1e12;
             MSDFGEN_CUBIC_SEARCH_STARTS=8;
             MSDFGEN_CUBIC_SEARCH_STEPS=8;
       type { TEdgeSegment }
            TEdgeSegment=record
             public
              Points:array[0..3] of TpvSignedDistanceField2DMSDFGenerator.TVector2;
              Color:TpvSignedDistanceField2DMSDFGenerator.TEdgeColor;
              Type_:TpvSignedDistanceField2DMSDFGenerator.TEdgeType;
              constructor Create(const aP0,aP1:TpvSignedDistanceField2DMSDFGenerator.TVector2;const aColor:TpvSignedDistanceField2DMSDFGenerator.TEdgeColor=TpvSignedDistanceField2DMSDFGenerator.TEdgeColor.WHITE); overload;
              constructor Create(const aP0,aP1,aP2:TpvSignedDistanceField2DMSDFGenerator.TVector2;const aColor:TpvSignedDistanceField2DMSDFGenerator.TEdgeColor=TpvSignedDistanceField2DMSDFGenerator.TEdgeColor.WHITE); overload;
              constructor Create(const aP0,aP1,aP2,aP3:TpvSignedDistanceField2DMSDFGenerator.TVector2;const aColor:TpvSignedDistanceField2DMSDFGenerator.TEdgeColor=TpvSignedDistanceField2DMSDFGenerator.TEdgeColor.WHITE); overload;
              function Point(const aParam:TpvDouble):TpvSignedDistanceField2DMSDFGenerator.TVector2;
              function Direction(const aParam:TpvDouble):TpvSignedDistanceField2DMSDFGenerator.TVector2;
              function MinSignedDistance(const aOrigin:TpvSignedDistanceField2DMSDFGenerator.TVector2;var aParam:TpvDouble):TpvSignedDistanceField2DMSDFGenerator.TSignedDistance;
              procedure DistanceToPseudoDistance(var aDistance:TpvSignedDistanceField2DMSDFGenerator.TSignedDistance;const aOrigin:TpvSignedDistanceField2DMSDFGenerator.TVector2;const aParam:TpvDouble);
              procedure Bounds(var aBounds:TpvSignedDistanceField2DMSDFGenerator.TBounds);
              procedure SplitInThirds(out aPart1,aPart2,aPart3:TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment);
            end;
            PEdgeSegment=^TEdgeSegment;
            TEdgeSegments=array of TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment;
            { TContour }
            TContour=record
             public
              Edges:TpvSignedDistanceField2DMSDFGenerator.TEdgeSegments;
              Count:TpvSizeInt;
              class function Create:TContour; static;
              function AddEdge(const aEdge:TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment):TpvSignedDistanceField2DMSDFGenerator.PEdgeSegment;
              procedure Bounds(var aBounds:TpvSignedDistanceField2DMSDFGenerator.TBounds);
              procedure BoundMiters(var aBounds:TpvSignedDistanceField2DMSDFGenerator.TBounds;const aBorder,aMiterLimit:TpvDouble;const aPolarity:TpvSizeInt);
              function Winding:TpvSizeInt;
            end;
            PContour=^TContour;
            TContours=array of TpvSignedDistanceField2DMSDFGenerator.TContour;
            { TShape }
            TShape=record
             Contours:TContours;
             Count:TpvSizeInt;
             InverseYAxis:boolean;
             class function Create:TShape; static;
             function AddContour:TpvSignedDistanceField2DMSDFGenerator.PContour;
             function Validate:boolean;
             procedure Normalize;
             procedure Bounds(var aBounds:TpvSignedDistanceField2DMSDFGenerator.TBounds);
             procedure BoundMiters(var aBounds:TpvSignedDistanceField2DMSDFGenerator.TBounds;const aBorder,aMiterLimit:TpvDouble;const aPolarity:TpvSizeInt);
             function GetBounds(const aBorder:TpvDouble=0.0;const aMiterLimit:TpvDouble=0;const aPolarity:TpvSizeInt=0):TpvSignedDistanceField2DMSDFGenerator.TBounds;
            end;
            PShape=^TShape;
            TShapes=array of TpvSignedDistanceField2DMSDFGenerator.TShape;
            TPixel=record
             r:TpvDouble;
             g:TpvDouble;
             b:TpvDouble;
             a:TpvDouble;
            end;
            PPixel=^TPixel;
            TPixels=array of TPixel;
            TPixelArray=array[0..65535] of TPixel;
            PPixelArray=^TPixelArray;
            TImage=record
             Width:TpvSizeInt;
             Height:TpvSizeInt;
             Pixels:TPixels;
            end;
            PImage=^TImage;
      public
       class function Median(a,b,c:TpvDouble):TpvDouble; static;
       class function Sign(n:TpvDouble):TpvInt32; static;
       class function NonZeroSign(n:TpvDouble):TpvInt32; static;
       class function SolveQuadratic(out x0,x1:TpvDouble;const a,b,c:TpvDouble):TpvSizeInt; static;
       class function SolveCubicNormed(out x0,x1,x2:TpvDouble;a,b,c:TpvDouble):TpvSizeInt; static;
       class function SolveCubic(out x0,x1,x2:TpvDouble;const a,b,c,d:TpvDouble):TpvSizeInt; static;
       class function Shoelace(const a,b:TpvSignedDistanceField2DMSDFGenerator.TVector2):TpvDouble; static;
       class procedure AutoFrame(const aShape:TpvSignedDistanceField2DMSDFGenerator.TShape;const aWidth,aHeight:TpvSizeInt;const aPixelRange:TpvDouble;out aTranslate,aScale:TpvSignedDistanceField2DMSDFGenerator.TVector2); static;
       class function IsCorner(const aDirection,bDirection:TpvSignedDistanceField2DMSDFGenerator.TVector2;const aCrossThreshold:TpvDouble):boolean; static;
       class procedure SwitchColor(var aColor:TpvSignedDistanceField2DMSDFGenerator.TEdgeColor;var aSeed:TpvUInt64;const aBanned:TpvSignedDistanceField2DMSDFGenerator.TEdgeColor=TpvSignedDistanceField2DMSDFGenerator.TEdgeColor.BLACK); static;
       class procedure EdgeColoringSimple(var aShape:TpvSignedDistanceField2DMSDFGenerator.TShape;const aAngleThreshold:TpvDouble;aSeed:TpvUInt64); static;
       class procedure GenerateDistanceFieldPixel(var aImage:TpvSignedDistanceField2DMSDFGenerator.TImage;const aShape:TpvSignedDistanceField2DMSDFGenerator.TShape;const aRange:TpvDouble;const aScale,aTranslate:TpvSignedDistanceField2DMSDFGenerator.TVector2;const aX,aY:TpvSizeInt); static;
       class procedure GenerateDistanceField(var aImage:TpvSignedDistanceField2DMSDFGenerator.TImage;const aShape:TpvSignedDistanceField2DMSDFGenerator.TShape;const aRange:TpvDouble;const aScale,aTranslate:TpvSignedDistanceField2DMSDFGenerator.TVector2); static;
       class function DetectClash(const a,b:TpvSignedDistanceField2DMSDFGenerator.TPixel;const aThreshold:TpvDouble):boolean; static;
       class procedure ErrorCorrection(var aImage:TpvSignedDistanceField2DMSDFGenerator.TImage;const aThreshold:TpvSignedDistanceField2DMSDFGenerator.TVector2); static;
     end;

     { TpvSignedDistanceField2DGenerator }

     TpvSignedDistanceField2DGenerator=class
      private
       const DistanceField2DMagnitudeValue=VulkanDistanceField2DSpreadValue;
             DistanceField2DPadValue=VulkanDistanceField2DSpreadValue;
             DistanceField2DScalar1Value=1.0;
             DistanceField2DCloseValue=DistanceField2DScalar1Value/16.0;
             DistanceField2DCloseSquaredValue=DistanceField2DCloseValue*DistanceField2DCloseValue;
             DistanceField2DNearlyZeroValue=DistanceField2DScalar1Value/int64(1 shl 18);
             DistanceField2DTangentToleranceValue=DistanceField2DScalar1Value/int64(1 shl 11);
             DistanceField2DRasterizerToScreenScale=1.0;
             CurveTessellationTolerance=0.125;
             CurveTessellationToleranceSquared=CurveTessellationTolerance*CurveTessellationTolerance;
             CurveRecursionLimit=16;
      public
       type TMultiChannelMode=
             (
              None,
              MSDFGENCompatible,
              Gradients,
              Multisampling
             );
      private
       fPointInPolygonPathSegments:TpvSignedDistanceField2DPointInPolygonPathSegments;
       fVectorPath:TpvVectorPath;
       fScale:TpvDouble;
       fOffsetX:TpvDouble;
       fOffsetY:TpvDouble;
       fDistanceField:PpvSignedDistanceField2D;
       fMultiChannelMode:TMultiChannelMode;
       fShape:TpvSignedDistanceField2DShape;
       fDistanceFieldData:TpvSignedDistanceField2DData;
       fColorChannelIndex:TpvSizeInt;
      protected
       function Clamp(const Value,MinValue,MaxValue:TpvInt64):TpvInt64; overload;
       function Clamp(const Value,MinValue,MaxValue:TpvDouble):TpvDouble; overload;
       function DoublePrecisionPointAdd(const a,b:TpvSignedDistanceField2DDoublePrecisionPoint):TpvSignedDistanceField2DDoublePrecisionPoint;
       function DoublePrecisionPointSub(const a,b:TpvSignedDistanceField2DDoublePrecisionPoint):TpvSignedDistanceField2DDoublePrecisionPoint;
       function DoublePrecisionPointLength(const p:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
       function DoublePrecisionPointDistance(const a,b:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
       function DoublePrecisionPointLengthSquared(const v:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
       function DoublePrecisionPointDistanceSquared(const a,b:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
       function DoublePrecisionPointCrossProduct(const a,b:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
       function DoublePrecisionPointIsLeft(const a,b,c:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
       function DoublePrecisionPointDotProduct(const a,b:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
       function DoublePrecisionPointNormalize(const v:TpvSignedDistanceField2DDoublePrecisionPoint):TpvSignedDistanceField2DDoublePrecisionPoint;
       function DoublePrecisionPointLerp(const a,b:TpvSignedDistanceField2DDoublePrecisionPoint;const t:TpvDouble):TpvSignedDistanceField2DDoublePrecisionPoint;
       function DoublePrecisionPointLerpEx(const a,b:TpvSignedDistanceField2DDoublePrecisionPoint;const t:TpvDouble):TpvSignedDistanceField2DDoublePrecisionPoint;
       function DoublePrecisionPointMap(const p:TpvSignedDistanceField2DDoublePrecisionPoint;const m:TpvSignedDistanceField2DDoublePrecisionAffineMatrix):TpvSignedDistanceField2DDoublePrecisionPoint;
       procedure GetOffset(out oX,oY:TpvDouble);
       procedure ApplyOffset(var aX,aY:TpvDouble); overload;
       function ApplyOffset(const aPoint:TpvSignedDistanceField2DDoublePrecisionPoint):TpvSignedDistanceField2DDoublePrecisionPoint; overload;
       function BetweenClosedOpen(const a,b,c:TpvDouble;const Tolerance:TpvDouble=0.0;const XFormToleranceToX:boolean=false):boolean;
       function BetweenClosed(const a,b,c:TpvDouble;const Tolerance:TpvDouble=0.0;const XFormToleranceToX:boolean=false):boolean;
       function NearlyZero(const Value:TpvDouble;const Tolerance:TpvDouble=DistanceField2DNearlyZeroValue):boolean;
       function NearlyEqual(const x,y:TpvDouble;const Tolerance:TpvDouble=DistanceField2DNearlyZeroValue;const XFormToleranceToX:boolean=false):boolean;
       function SignOf(const Value:TpvDouble):TpvInt32;
       function IsColinear(const Points:array of TpvSignedDistanceField2DDoublePrecisionPoint):boolean;
       function PathSegmentDirection(const PathSegment:TpvSignedDistanceField2DPathSegment;const Which:TpvInt32):TpvSignedDistanceField2DDoublePrecisionPoint;
       function PathSegmentCountPoints(const PathSegment:TpvSignedDistanceField2DPathSegment):TpvInt32;
       function PathSegmentEndPoint(const PathSegment:TpvSignedDistanceField2DPathSegment):PpvSignedDistanceField2DDoublePrecisionPoint;
       function PathSegmentCornerPoint(const PathSegment:TpvSignedDistanceField2DPathSegment;const WhichA,WhichB:TpvInt32):PpvSignedDistanceField2DDoublePrecisionPoint;
       procedure InitializePathSegment(var PathSegment:TpvSignedDistanceField2DPathSegment);
       procedure InitializeDistances;
       function AddLineToPathSegmentArray(var Contour:TpvSignedDistanceField2DPathContour;const Points:array of TpvSignedDistanceField2DDoublePrecisionPoint):TpvInt32;
       function AddQuadraticBezierCurveToPathSegmentArray(var Contour:TpvSignedDistanceField2DPathContour;const Points:array of TpvSignedDistanceField2DDoublePrecisionPoint):TpvInt32;
       function AddQuadraticBezierCurveAsSubdividedLinesToPathSegmentArray(var Contour:TpvSignedDistanceField2DPathContour;const Points:array of TpvSignedDistanceField2DDoublePrecisionPoint):TpvInt32;
       function AddCubicBezierCurveAsSubdividedQuadraticBezierCurvesToPathSegmentArray(var Contour:TpvSignedDistanceField2DPathContour;const Points:array of TpvSignedDistanceField2DDoublePrecisionPoint):TpvInt32;
       function AddCubicBezierCurveAsSubdividedLinesToPathSegmentArray(var Contour:TpvSignedDistanceField2DPathContour;const Points:array of TpvSignedDistanceField2DDoublePrecisionPoint):TpvInt32;
       function CubeRoot(Value:TpvDouble):TpvDouble;
       function CalculateNearestPointForQuadraticBezierCurve(const PathSegment:TpvSignedDistanceField2DPathSegment;const XFormPoint:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
       procedure PrecomputationForRow(out RowData:TpvSignedDistanceField2DRowData;const PathSegment:TpvSignedDistanceField2DPathSegment;const PointLeft,PointRight:TpvSignedDistanceField2DDoublePrecisionPoint);
       function CalculateSideOfQuadraticBezierCurve(const PathSegment:TpvSignedDistanceField2DPathSegment;const Point,XFormPoint:TpvSignedDistanceField2DDoublePrecisionPoint;const RowData:TpvSignedDistanceField2DRowData):TpvSignedDistanceField2DPathSegmentSide;
       function DistanceToPathSegment(const Point:TpvSignedDistanceField2DDoublePrecisionPoint;const PathSegment:TpvSignedDistanceField2DPathSegment;const RowData:TpvSignedDistanceField2DRowData;out PathSegmentSide:TpvSignedDistanceField2DPathSegmentSide):TpvDouble;
       procedure ConvertShape(const DoSubdivideCurvesIntoLines:boolean);
       procedure SplitPathSegmentIntoThreePartsInsideContour(var Contour:TpvSignedDistanceField2DPathContour;const BasePathSegmentIndex:TpvInt32);
       procedure SplitPathSegmentIntoThreePartsToContour(var Contour:TpvSignedDistanceField2DPathContour;const BasePathSegmentIndex:TpvInt32;const BasePathSegment:TpvSignedDistanceField2DPathSegment);
       procedure NormalizeShape;
       procedure PathSegmentColorizeShape;
       function GetLineNonClippedTime(const p,p0,p1:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
       function GetQuadraticBezierCurveNonClippedTime(const p,p0,p1,p2:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
       function GetNonClampedSignedLineDistance(const p,p0,p1:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
       procedure CalculateDistanceFieldDataLineRange(const FromY,ToY:TpvInt32);
       procedure CalculateDistanceFieldDataLineRangeParallelForJobFunction(const Job:PPasMPJob;const ThreadIndex:TPasMPInt32;const Data:TpvPointer;const FromIndex,ToIndex:TPasMPNativeInt);
       function PackDistanceFieldValue(Distance:TpvDouble):TpvUInt8;
       function PackPseudoDistanceFieldValue(Distance:TpvDouble):TpvUInt8;
       procedure ConvertToPointInPolygonPathSegments;
       function GetWindingNumberAtPointInPolygon(const Point:TpvSignedDistanceField2DDoublePrecisionPoint):TpvInt32;
       function GenerateDistanceFieldPicture(const DistanceFieldData:TpvSignedDistanceField2DData;const Width,Height,TryIteration:TpvInt32):boolean;
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       procedure Execute(var aDistanceField:TpvSignedDistanceField2D;const aVectorPath:TpvVectorPath;const aScale:TpvDouble=1.0;const aOffsetX:TpvDouble=0.0;const aOffsetY:TpvDouble=0.0;const aMultiChannelMode:TMultiChannelMode=TMultiChannelMode.None);
       class procedure Generate(var aDistanceField:TpvSignedDistanceField2D;const aVectorPath:TpvVectorPath;const aScale:TpvDouble=1.0;const aOffsetX:TpvDouble=0.0;const aOffsetY:TpvDouble=0.0;const aMultiChannelMode:TMultiChannelMode=TMultiChannelMode.None); static;
     end;

implementation

{ TpvSignedDistanceField2DMSDFGenerator.TVector2 }

constructor TpvSignedDistanceField2DMSDFGenerator.TVector2.Create(const aValue:TpvDouble);
begin
 x:=aValue;
 y:=aValue;
end;

constructor TpvSignedDistanceField2DMSDFGenerator.TVector2.Create(const aX,aY:TpvDouble);
begin
 x:=aX;
 y:=aY;
end;

function TpvSignedDistanceField2DMSDFGenerator.TVector2.Length:TpvDouble;
begin
 result:=sqrt(sqr(x)+sqr(y));
end;

function TpvSignedDistanceField2DMSDFGenerator.TVector2.Direction:TpvDouble;
begin
 result:=ArcTan2(y,x);
end;

function TpvSignedDistanceField2DMSDFGenerator.TVector2.Normalize:TpvSignedDistanceField2DMSDFGenerator.TVector2;
var Len:TpvDouble;
begin
 Len:=Length;
 if IsZero(Len) then begin
  result.x:=0.0;
  result.y:=0.0;
 end else begin
  result.x:=x/Len;
  result.y:=y/Len;
 end;
end;

function TpvSignedDistanceField2DMSDFGenerator.TVector2.Dot(const aRight:TpvSignedDistanceField2DMSDFGenerator.TVector2): TpvDouble;
begin
 result:=(x*aRight.x)+(y*aRight.y);
end;

function TpvSignedDistanceField2DMSDFGenerator.TVector2.Cross(const aRight:TpvSignedDistanceField2DMSDFGenerator.TVector2): TpvDouble;
begin
 result:=(x*aRight.y)-(y*aRight.x);
end;

function TpvSignedDistanceField2DMSDFGenerator.TVector2.OrthoNormal:TpvSignedDistanceField2DMSDFGenerator.TVector2;
var Len:TpvDouble;
begin
 Len:=Length;
 if IsZero(Len) then begin
  result.x:=0.0;
  result.y:=0.0;
 end else begin
  result.x:=y/Len;
  result.y:=(-x)/Len;
 end;
end;

function TpvSignedDistanceField2DMSDFGenerator.TVector2.Lerp(const b:TpvSignedDistanceField2DMSDFGenerator.TVector2;const t:TpvDouble):TpvSignedDistanceField2DMSDFGenerator.TVector2;
begin
 result.x:=(x*(1.0-t))+(b.x*t);
 result.y:=(y*(1.0-t))+(b.y*t);
end;

class operator TpvSignedDistanceField2DMSDFGenerator.TVector2.Equal(const a,b:TpvSignedDistanceField2DMSDFGenerator.TVector2): boolean;
begin
 result:=SameValue(a.x,b.x) and SameValue(a.y,b.y);
end;

class operator TpvSignedDistanceField2DMSDFGenerator.TVector2.NotEqual(const a,b:TpvSignedDistanceField2DMSDFGenerator.TVector2): boolean;
begin
 result:=(not SameValue(a.x,b.x)) or (not SameValue(a.y,b.y));
end;

class operator TpvSignedDistanceField2DMSDFGenerator.TVector2.Add(const a,b:TpvSignedDistanceField2DMSDFGenerator.TVector2):TpvSignedDistanceField2DMSDFGenerator.TVector2;
begin
 result.x:=a.x+b.x;
 result.y:=a.y+b.y;
end;

class operator TpvSignedDistanceField2DMSDFGenerator.TVector2.Subtract(const a,b:TpvSignedDistanceField2DMSDFGenerator.TVector2):TpvSignedDistanceField2DMSDFGenerator.TVector2;
begin
 result.x:=a.x-b.x;
 result.y:=a.y-b.y;
end;

class operator TpvSignedDistanceField2DMSDFGenerator.TVector2.Multiply(const a,b:TpvSignedDistanceField2DMSDFGenerator.TVector2):TpvSignedDistanceField2DMSDFGenerator.TVector2;
begin
 result.x:=a.x*b.x;
 result.y:=a.y*b.y;
end;

class operator TpvSignedDistanceField2DMSDFGenerator.TVector2.Multiply(const a:TpvSignedDistanceField2DMSDFGenerator.TVector2;const b:TpvDouble):TpvSignedDistanceField2DMSDFGenerator.TVector2;
begin
 result.x:=a.x*b;
 result.y:=a.y*b;
end;

class operator TpvSignedDistanceField2DMSDFGenerator.TVector2.Divide(const a,b:TpvSignedDistanceField2DMSDFGenerator.TVector2):TpvSignedDistanceField2DMSDFGenerator.TVector2;
begin
 result.x:=a.x/b.x;
 result.y:=a.y/b.y;
end;

class operator TpvSignedDistanceField2DMSDFGenerator.TVector2.Divide(const a:TpvSignedDistanceField2DMSDFGenerator.TVector2;const b:TpvDouble):TpvSignedDistanceField2DMSDFGenerator.TVector2;
begin
 result.x:=a.x/b;
 result.y:=a.y/b;
end;

class operator TpvSignedDistanceField2DMSDFGenerator.TVector2.Negative(const a:TpvSignedDistanceField2DMSDFGenerator.TVector2):TpvSignedDistanceField2DMSDFGenerator.TVector2;
begin
 result.x:=-a.x;
 result.y:=-a.y;
end;

class operator TpvSignedDistanceField2DMSDFGenerator.TVector2.Positive(const a:TpvSignedDistanceField2DMSDFGenerator.TVector2):TpvSignedDistanceField2DMSDFGenerator.TVector2;
begin
 result.x:=a.x;
 result.y:=a.y;
end;

{ TpvSignedDistanceField2DMSDFGenerator.TSignedDistance }

constructor TpvSignedDistanceField2DMSDFGenerator.TSignedDistance.Create(const aDistance,aDot:TpvDouble);
begin
 Distance:=aDistance;
 Dot:=aDot;
end;

class function TpvSignedDistanceField2DMSDFGenerator.TSignedDistance.Empty:TpvSignedDistanceField2DMSDFGenerator.TSignedDistance;
begin
 result.Distance:=TpvSignedDistanceField2DMSDFGenerator.InfinateDistance;
 result.Dot:=1.0;
end;

class operator TpvSignedDistanceField2DMSDFGenerator.TSignedDistance.LessThan(const a,b:TpvSignedDistanceField2DMSDFGenerator.TSignedDistance):boolean;
begin
 result:=(abs(a.Distance)<abs(b.Distance)) or (SameValue(a.Distance,b.Distance) and (a.Dot<b.Dot));
end;

{ TpvSignedDistanceField2DMSDFGenerator.TBounds }

procedure TpvSignedDistanceField2DMSDFGenerator.TBounds.PointBounds(const p:TpvSignedDistanceField2DMSDFGenerator.TVector2);
begin
 if p.x<l then begin
  l:=p.x;
 end;
 if p.y<b then begin
  b:=p.y;
 end;
 if p.x>r then begin
  r:=p.x;
 end;
 if p.y>t then begin
  t:=p.y;
 end;
end;

{ TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment }

constructor TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment.Create(const aP0,aP1:TpvSignedDistanceField2DMSDFGenerator.TVector2;const aColor:TpvSignedDistanceField2DMSDFGenerator.TEdgeColor);
begin
 Points[0]:=aP0;
 Points[1]:=aP1;
 Color:=aColor;
 Type_:=TpvSignedDistanceField2DMSDFGenerator.TEdgeType.LINEAR;
end;

constructor TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment.Create(const aP0,aP1,aP2:TpvSignedDistanceField2DMSDFGenerator.TVector2;const aColor:TpvSignedDistanceField2DMSDFGenerator.TEdgeColor);
begin
 Points[0]:=aP0;
 Points[1]:=aP1;
 Points[2]:=aP2;
 Color:=aColor;
 Type_:=TpvSignedDistanceField2DMSDFGenerator.TEdgeType.QUADRATIC;
end;

constructor TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment.Create(const aP0,aP1,aP2,aP3:TpvSignedDistanceField2DMSDFGenerator.TVector2;const aColor:TpvSignedDistanceField2DMSDFGenerator.TEdgeColor);
begin
 Points[0]:=aP0;
 Points[1]:=aP1;
 Points[2]:=aP2;
 Points[3]:=aP3;
 Color:=aColor;
 Type_:=TpvSignedDistanceField2DMSDFGenerator.TEdgeType.CUBIC;
end;

function TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment.Point(const aParam:TpvDouble):TpvSignedDistanceField2DMSDFGenerator.TVector2;
var p12:TpvSignedDistanceField2DMSDFGenerator.TVector2;
begin
 case Type_ of
  TpvSignedDistanceField2DMSDFGenerator.TEdgeType.LINEAR:begin
   result:=Points[0].Lerp(Points[1],aParam);
  end;
  TpvSignedDistanceField2DMSDFGenerator.TEdgeType.QUADRATIC:begin
   result:=(Points[0].Lerp(Points[1],aParam)).Lerp(Points[1].Lerp(Points[2],aParam),aParam);
  end;
  else {TpvSignedDistanceField2DMSDFGenerator.TEdgeType.CUBIC:}begin
   p12:=Points[1].Lerp(Points[2],aParam);
   result:=((Points[0].Lerp(Points[1],aParam)).Lerp(p12,aParam)).Lerp(p12.Lerp(Points[2].Lerp(Points[3],aParam),aParam),aParam);
  end;
 end;
end;

function TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment.Direction(const aParam:TpvDouble):TpvSignedDistanceField2DMSDFGenerator.TVector2;
begin
 case Type_ of
  TpvSignedDistanceField2DMSDFGenerator.TEdgeType.LINEAR:begin
   result:=Points[1]-Points[0];
  end;
  TpvSignedDistanceField2DMSDFGenerator.TEdgeType.QUADRATIC:begin
   result:=(Points[1]-Points[0]).Lerp(Points[2]-Points[1],aParam);
   if IsZero(result.x) and IsZero(result.y) then begin
    result:=Points[2]-Points[0];
   end;
  end;
  else {TpvSignedDistanceField2DMSDFGenerator.TEdgeType.CUBIC:}begin
   result:=((Points[1]-Points[0]).Lerp(Points[2]-Points[1],aParam)).Lerp((Points[2]-Points[1]).Lerp(Points[3]-Points[2],aParam),aParam);
   if IsZero(result.x) and IsZero(result.y) then begin
    if SameValue(aParam,0) then begin
     result:=Points[2]-Points[0];
    end else if SameValue(aParam,1) then begin
     result:=Points[3]-Points[1];
    end;
   end;
  end;
 end;
end;

function TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment.MinSignedDistance(const aOrigin:TpvSignedDistanceField2DMSDFGenerator.TVector2;var aParam:TpvDouble):TpvSignedDistanceField2DMSDFGenerator.TSignedDistance;
var aq,ab,eq,qa,br,epDir,qe,sa,d1,d2:TpvSignedDistanceField2DMSDFGenerator.TVector2;
    EndPointDistance,OrthoDistance,a,b,c,d,MinDistance,Distance,Time:TpvDouble;
    t:array[0..3] of TpvDouble;
    Solutions,Index,Step:TpvSizeInt;
begin
 case Type_ of
  TpvSignedDistanceField2DMSDFGenerator.TEdgeType.LINEAR:begin
   aq:=aOrigin-Points[0];
   ab:=Points[1]-Points[0];
   aParam:=aq.Dot(ab)/ab.Dot(ab);
   eq:=Points[ord(aParam>0.5) and 1]-aOrigin;
   EndPointDistance:=eq.Length;
   if (aParam>0.0) and (aParam<1.0) then begin
    OrthoDistance:=ab.OrthoNormal.Dot(aq);
    if abs(OrthoDistance)<EndPointDistance then begin
     result:=TpvSignedDistanceField2DMSDFGenerator.TSignedDistance.Create(OrthoDistance,0.0);
     exit;
    end;
   end;
   result:=TpvSignedDistanceField2DMSDFGenerator.TSignedDistance.Create(TpvSignedDistanceField2DMSDFGenerator.NonZeroSign(aq.Cross(ab))*EndPointDistance,abs(ab.Normalize.Dot(eq.Normalize)));
  end;
  TpvSignedDistanceField2DMSDFGenerator.TEdgeType.QUADRATIC:begin
   qa:=Points[0]-aOrigin;
   ab:=Points[1]-Points[0];
   br:=(Points[2]-Points[1])-ab;
   a:=br.Dot(br);
   b:=3.0*ab.Dot(br);
   c:=(2.0*ab.Dot(ab))+qa.Dot(br);
   d:=qa.Dot(ab);
   Solutions:=SolveCubic(t[0],t[1],t[2],a,b,c,d);
   epDir:=Direction(0);
	 MinDistance:=TpvSignedDistanceField2DMSDFGenerator.NonZeroSign(epDir.Cross(qa))*qa.Length;
	 aParam:=-qa.Dot(epDir)/epDir.Dot(epDir);
   epDir:=Direction(1);
	 Distance:=TpvSignedDistanceField2DMSDFGenerator.NonZeroSign(epDir.Cross(Points[2]-aOrigin))*((Points[2]-aOrigin).Length);
	 if abs(Distance)<abs(MinDistance) then begin
	  MinDistance:=Distance;
	  aParam:=(aOrigin-Points[1]).Dot(epDir)/epDir.Dot(epDir);
   end;
   for Index:=0 to Solutions-1 do begin
		if (t[Index]>0.0) and (t[Index]<1.0) then begin
     qe:=((Points[0]+(ab*(2.0*t[Index])))+(br*sqr(t[Index])))-aOrigin;
     Distance:=TpvSignedDistanceField2DMSDFGenerator.NonZeroSign((Points[2]-Points[0]).Cross(qe))*qe.Length;
  	 if abs(Distance)<abs(MinDistance) then begin
	    MinDistance:=Distance;
      aParam:=t[Index];
     end;
    end;
   end;
   if (aParam>=0.0) and (aParam<=1.0) then begin
    result:=TpvSignedDistanceField2DMSDFGenerator.TSignedDistance.Create(MinDistance,0.0);
   end else if aParam<0.5 then begin
    result:=TpvSignedDistanceField2DMSDFGenerator.TSignedDistance.Create(MinDistance,abs(Direction(0).Normalize.Dot(qa.Normalize)));
   end else begin
    result:=TpvSignedDistanceField2DMSDFGenerator.TSignedDistance.Create(MinDistance,abs(Direction(1).Normalize.Dot((Points[2]-aOrigin).Normalize)));
   end;
  end;
  else {TpvSignedDistanceField2DMSDFGenerator.TEdgeType.CUBIC:}begin
   qa:=Points[0]-aOrigin;
   ab:=Points[1]-Points[0];
   br:=(Points[2]-Points[1])-ab;
   sa:=((Points[3]-Points[2])-(Points[2]-Points[1]))-br;
   epDir:=Direction(0);
	 MinDistance:=TpvSignedDistanceField2DMSDFGenerator.NonZeroSign(epDir.Cross(qa))*qa.Length;
	 aParam:=-qa.Dot(epDir)/epDir.Dot(epDir);
   begin
    epDir:=Direction(1);
 	  Distance:=TpvSignedDistanceField2DMSDFGenerator.NonZeroSign(epDir.Cross(Points[3]-aOrigin))*(Points[3]-aOrigin).Length;
 	  aParam:=-qa.Dot(epDir)/epDir.Dot(epDir);
    if abs(Distance)<abs(MinDistance) then begin
     MinDistance:=Distance;
     aParam:=(epDir-(Points[3]-aOrigin)).Dot(epDir)/epDir.Dot(epDir);
    end;
   end;
   for Index:=0 to TpvSignedDistanceField2DMSDFGenerator.MSDFGEN_CUBIC_SEARCH_STARTS-1 do begin
    Time:=Index/TpvSignedDistanceField2DMSDFGenerator.MSDFGEN_CUBIC_SEARCH_STARTS;
    Step:=0;
    repeat
		 qe:=(((Points[0]+(ab*(3.0*Time)))+(br*(3.0*sqr(Time))))+(sa*(sqr(Time)*Time)))-aOrigin;
		 Distance:=TpvSignedDistanceField2DMSDFGenerator.NonZeroSign(Direction(Time).Cross(qe))*qe.Length();
     if abs(Distance)<abs(MinDistance) then begin
      MinDistance:=Distance;
      aParam:=Time;
     end;
     if Step<=TpvSignedDistanceField2DMSDFGenerator.MSDFGEN_CUBIC_SEARCH_STEPS then begin
      break;
     end;
     d1:=((sa*(3.0*sqr(Time)))+(br*(6*Time)))+(ab*3.0);
     d2:=(sa*(6*Time))+(br*6);
     Time:=Time-(qe.Dot(d1)/(d1.Dot(d1)+qe.Dot(d2)));
     if (Time<0.0) or (Time>1.0) then begin
      break;
     end else begin
      inc(Step);
     end;
    until false;
   end;
   if (aParam>=0.0) and (aParam<=1.0) then begin
    result:=TpvSignedDistanceField2DMSDFGenerator.TSignedDistance.Create(MinDistance,0.0);
   end else if aParam<0.5 then begin
    result:=TpvSignedDistanceField2DMSDFGenerator.TSignedDistance.Create(MinDistance,abs(Direction(0).Normalize.Dot(qa.Normalize)));
   end else begin
    result:=TpvSignedDistanceField2DMSDFGenerator.TSignedDistance.Create(MinDistance,abs(Direction(1).Normalize.Dot((Points[3]-aOrigin).Normalize)));
   end;
  end;
 end;
end;

procedure TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment.DistanceToPseudoDistance(var aDistance:TpvSignedDistanceField2DMSDFGenerator.TSignedDistance;const aOrigin:TpvSignedDistanceField2DMSDFGenerator.TVector2;const aParam:TpvDouble);
var dir,aq,bq:TpvSignedDistanceField2DMSDFGenerator.TVector2;
    ts,PseudoDistance:TpvDouble;
begin
 if aParam<0.0 then begin
  dir:=Direction(0).Normalize;
  aq:=aOrigin-Point(0);
  ts:=aq.Dot(dir);
  if ts<0.0 then begin
   PseudoDistance:=aq.Cross(dir);
   if abs(PseudoDistance)<=abs(aDistance.Distance) then begin
    aDistance.Distance:=PseudoDistance;
    aDistance.Dot:=0.0;
   end;
  end;
 end else if aParam>1.0 then begin
  dir:=Direction(1).Normalize;
  bq:=aOrigin-Point(1);
  ts:=bq.Dot(dir);
  if ts<0.0 then begin
   PseudoDistance:=bq.Cross(dir);
   if abs(PseudoDistance)<=abs(aDistance.Distance) then begin
    aDistance.Distance:=PseudoDistance;
    aDistance.Dot:=0.0;
   end;
  end;
 end;
end;

procedure TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment.Bounds(var aBounds:TpvSignedDistanceField2DMSDFGenerator.TBounds);
var b,a0,a1,a2:TpvSignedDistanceField2DMSDFGenerator.TVector2;
    Param:TpvDouble;
    Params:array[0..1] of TpvDouble;
    Solutions:TpvSizeInt;
begin
 case Type_ of
  TpvSignedDistanceField2DMSDFGenerator.TEdgeType.LINEAR:begin
   aBounds.PointBounds(Points[0]);
   aBounds.PointBounds(Points[1]);
  end;
  TpvSignedDistanceField2DMSDFGenerator.TEdgeType.QUADRATIC:begin
   aBounds.PointBounds(Points[0]);
   aBounds.PointBounds(Points[2]);
   b:=(Points[1]-Points[0])-(Points[2]-Points[1]);
   if not IsZero(b.x) then begin
    Param:=(Points[1].x-Points[0].x)/b.x;
    if (Param>0.0) and (Param<1.0) then begin
     aBounds.PointBounds(Point(Param));
    end;
   end;
   if not IsZero(b.y) then begin
    Param:=(Points[1].y-Points[0].y)/b.y;
    if (Param>0.0) and (Param<1.0) then begin
     aBounds.PointBounds(Point(Param));
    end;
   end;
  end;
  else {TpvSignedDistanceField2DMSDFGenerator.TEdgeType.CUBIC:}begin
   aBounds.PointBounds(Points[0]);
   aBounds.PointBounds(Points[3]);
   a0:=Points[1]-Points[0];
   a1:=((Points[2]-Points[1])-a0)*2.0;
   a2:=((Points[3]-(Points[2]*3.0))+(Points[1]*3.0))-Points[0];
   Solutions:=SolveQuadratic(Params[0],Params[1],a2.x,a1.x,a0.x);
   if (Solutions>0) and ((Params[0]>0.0) and (Params[0]<1.0)) then begin
    aBounds.PointBounds(Point(Params[0]));
   end;
   if (Solutions>1) and ((Params[1]>0.0) and (Params[1]<1.0)) then begin
    aBounds.PointBounds(Point(Params[1]));
   end;
   Solutions:=SolveQuadratic(Params[0],Params[1],a2.y,a1.y,a0.y);
   if (Solutions>0) and ((Params[0]>0.0) and (Params[0]<1.0)) then begin
    aBounds.PointBounds(Point(Params[0]));
   end;
   if (Solutions>1) and ((Params[1]>0.0) and (Params[1]<1.0)) then begin
    aBounds.PointBounds(Point(Params[1]));
   end;
  end;
 end;
end;

procedure TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment.SplitInThirds(out aPart1,aPart2,aPart3:TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment);
begin
 case Type_ of
  TpvSignedDistanceField2DMSDFGenerator.TEdgeType.LINEAR:begin
   aPart1:=TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment.Create(Points[0],Point(1.0/3.0),Color);
   aPart2:=TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment.Create(Point(2.0/3.0),Point(2.0/3.0),Color);
   aPart3:=TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment.Create(Point(2.0/3.0),Points[1],Color);
  end;
  TpvSignedDistanceField2DMSDFGenerator.TEdgeType.QUADRATIC:begin
   aPart1:=TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment.Create(Points[0],Points[0].Lerp(Points[1],1.0/3.0),Point(1.0/3.0),Color);
   aPart2:=TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment.Create(Point(1.0/3.0),Points[0].Lerp(Points[1],5.0/9.0).Lerp(Points[1].Lerp(Points[2],4.0/9.0),0.5),Point(2.0/3.0),Color);
   aPart3:=TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment.Create(Point(2.0/3.0),Points[1].Lerp(Points[2],2.0/3.0),Points[2],Color);
  end;
  else {TpvSignedDistanceField2DMSDFGenerator.TEdgeType.CUBIC:}begin
   if Points[0]=Points[1] then begin
    aPart1:=TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment.Create(Points[0],(Points[0].Lerp(Points[1],1.0/3.0)).Lerp(Points[1].Lerp(Points[2],1.0/3.0),1.0/3.0),Point(1.0/3.0),Color);
   end else begin
    aPart1:=TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment.Create(Points[0].Lerp(Points[1],1.0/3.0),(Points[0].Lerp(Points[1],1.0/3.0)).Lerp(Points[1].Lerp(Points[2],1.0/3.0),1.0/3.0),Point(1.0/3.0),Color);
   end;
   aPart2:=TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment.Create(Point(1.0/3.0),
                                                                     ((Points[0].Lerp(Points[1],1.0/3.0)).Lerp(Points[1].Lerp(Points[2],1.0/3.0),1.0/3.0)).Lerp(((Points[1].Lerp(Points[2],1.0/3.0)).Lerp(Points[2].Lerp(Points[3],1.0/3.0),1.0/3.0)),2.0/3.0),
                                                                     ((Points[0].Lerp(Points[1],2.0/3.0)).Lerp(Points[1].Lerp(Points[2],2.0/3.0),2.0/3.0)).Lerp(((Points[1].Lerp(Points[2],2.0/3.0)).Lerp(Points[2].Lerp(Points[3],2.0/3.0),2.0/3.0)),1.0/3.0),
                                                                     Point(2.0/3.0),
                                                                     Color);
   if Points[2]=Points[3] then begin
    aPart3:=TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment.Create(Point(2.0/3.0),
                                                                      (Points[1].Lerp(Points[2],2.0/3.0)).Lerp(Points[2].Lerp(Points[3],2.0/3.0),2.0/3.0),
                                                                      Points[3],
                                                                      Points[3],
                                                                      Color);
   end else begin
    aPart3:=TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment.Create(Point(2.0/3.0),
                                                                      (Points[1].Lerp(Points[2],2.0/3.0)).Lerp(Points[2].Lerp(Points[3],2.0/3.0),2.0/3.0),
                                                                      Points[2].Lerp(Points[3],2.0/3.0),
                                                                      Points[3],
                                                                      Color);
   end;
  end;
 end;
end;

{ TpvSignedDistanceField2DMSDFGenerator.TContour }

class function TpvSignedDistanceField2DMSDFGenerator.TContour.Create:TpvSignedDistanceField2DMSDFGenerator.TContour;
begin
 result.Edges:=nil;
 result.Count:=0;
end;

function TpvSignedDistanceField2DMSDFGenerator.TContour.AddEdge(const aEdge:TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment):TpvSignedDistanceField2DMSDFGenerator.PEdgeSegment;
begin
 if Count>=length(Edges) then begin
  SetLength(Edges,(Count+1)+((Count+1) shr 1)); // Grow factor 1.5
 end;
 Edges[Count]:=aEdge;
 result:=@Edges[Count];
 inc(Count);
end;

procedure TpvSignedDistanceField2DMSDFGenerator.TContour.Bounds(var aBounds:TpvSignedDistanceField2DMSDFGenerator.TBounds);
var Index:TpvSizeInt;
begin
 for Index:=0 to Count-1 do begin
  Edges[Index].Bounds(aBounds);
 end;
end;

procedure TpvSignedDistanceField2DMSDFGenerator.TContour.BoundMiters(var aBounds:TpvSignedDistanceField2DMSDFGenerator.TBounds;const aBorder,aMiterLimit:TpvDouble;const aPolarity:TpvSizeInt);
var Index:TpvSizeInt;
    PreviousDirection,Direction,Miter:TpvSignedDistanceField2DMSDFGenerator.TVector2;
    Edge:TpvSignedDistanceField2DMSDFGenerator.PEdgeSegment;
    MiterLength,q:TpvDouble;
begin
 if Count>0 then begin
  PreviousDirection:=Edges[Count-1].Direction(1).Normalize;
  for Index:=0 to Count-1 do begin
   Edge:=@Edges[Index];
   Direction:=-Edge^.Direction(0).Normalize;
   if (aPolarity*PreviousDirection.Cross(Direction))>=0.0 then begin
    MiterLength:=aMiterLimit;
    q:=(1.0-PreviousDirection.Dot(Direction))*0.5;
    if q>0.0 then begin
     MiterLength:=Min(1.0/sqrt(q),aMiterLimit);
    end;
    Miter:=Edge^.Point(0)+((PreviousDirection+Direction).Normalize*(aBorder*MiterLength));
    aBounds.PointBounds(Miter);
   end;
   PreviousDirection:=Edge^.Direction(1).Normalize;
  end;
 end;
end;

function TpvSignedDistanceField2DMSDFGenerator.TContour.Winding:TpvSizeInt;
var Total:TpvDouble;
    Index:TpvSizeInt;
    Edge:TpvSignedDistanceField2DMSDFGenerator.PEdgeSegment;
    a,b,c,d,Previous,Current:TpvSignedDistanceField2DMSDFGenerator.TVector2;
begin
 if Count>0 then begin
  Total:=0.0;
  case Count of
   1:Begin
    a:=Edges[0].Point(0.0);
    b:=Edges[0].Point(1.0/3.0);
    c:=Edges[0].Point(2.0/3.0);
    Total:=Total+TpvSignedDistanceField2DMSDFGenerator.Shoelace(a,b);
    Total:=Total+TpvSignedDistanceField2DMSDFGenerator.Shoelace(b,c);
    Total:=Total+TpvSignedDistanceField2DMSDFGenerator.Shoelace(c,a);
   end;
   2:begin
    a:=Edges[0].Point(0.0);
    b:=Edges[0].Point(0.5);
    c:=Edges[1].Point(0.0);
    d:=Edges[1].Point(0.5);
    Total:=Total+TpvSignedDistanceField2DMSDFGenerator.Shoelace(a,b);
    Total:=Total+TpvSignedDistanceField2DMSDFGenerator.Shoelace(b,c);
    Total:=Total+TpvSignedDistanceField2DMSDFGenerator.Shoelace(c,d);
    Total:=Total+TpvSignedDistanceField2DMSDFGenerator.Shoelace(d,a);
   end;
   else begin
    Previous:=Edges[Count-1].Point(0.0);
    for Index:=0 to Count-1 do begin
     Edge:=@Edges[Index];
     Current:=Edge^.Point(0.0);
     Total:=Total+TpvSignedDistanceField2DMSDFGenerator.Shoelace(Previous,Current);
     Previous:=Current;
    end;
   end;
  end;
  result:=TpvSignedDistanceField2DMSDFGenerator.Sign(Total);
 end else begin
  result:=0;
 end;
end;

{ TpvSignedDistanceField2DMSDFGenerator.TShape }

class function TpvSignedDistanceField2DMSDFGenerator.TShape.Create:TpvSignedDistanceField2DMSDFGenerator.TShape;
begin
 result.Contours:=nil;
 result.Count:=0;
 result.InverseYAxis:=false;
end;

function TpvSignedDistanceField2DMSDFGenerator.TShape.AddContour:TpvSignedDistanceField2DMSDFGenerator.PContour;
begin
 if Count>=length(Contours) then begin
  SetLength(Contours,(Count+1)+((Count+1) shr 1)); // Grow factor 1.5
 end;
 result:=@Contours[Count];
 inc(Count);
 result^:=TContour.Create;
end;

function TpvSignedDistanceField2DMSDFGenerator.TShape.Validate:boolean;
var ContourIndex,EdgeIndex:TpvSizeInt;
    Contour:TpvSignedDistanceField2DMSDFGenerator.PContour;
    Edge:TpvSignedDistanceField2DMSDFGenerator.PEdgeSegment;
    Corner:TpvSignedDistanceField2DMSDFGenerator.TVector2;
begin
 for ContourIndex:=0 to Count-1 do begin
  Contour:=@Contours[ContourIndex];
  if Contour^.Count>0 then begin
   Corner:=Contour^.Edges[Contour^.Count-1].Point(1);
   for EdgeIndex:=0 to Contour^.Count-1 do begin
    Edge:=@Contour^.Edges[EdgeIndex];
    if Edge^.Point(0)<>Corner then begin
     result:=false;
     exit;
    end;
    Corner:=Edge^.Point(1);
   end;
  end;
 end;
 result:=true;
end;

procedure TpvSignedDistanceField2DMSDFGenerator.TShape.Normalize;
var ContourIndex:TpvSizeInt;
    Contour:TpvSignedDistanceField2DMSDFGenerator.PContour;
    Part1,Part2,Part3:TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment;
begin
 for ContourIndex:=0 to Count-1 do begin
  Contour:=@Contours[ContourIndex];
  if Contour^.Count=1 then begin
   Contour^.Edges[0].SplitInThirds(Part1,Part2,Part3);
   Contour^.Edges:=nil;
   SetLength(Contour^.Edges,3);
   Contour^.Count:=3;
   Contour^.Edges[0]:=Part1;
   Contour^.Edges[1]:=Part2;
   Contour^.Edges[2]:=Part3;
  end;
 end;
end;

procedure TpvSignedDistanceField2DMSDFGenerator.TShape.Bounds(var aBounds:TpvSignedDistanceField2DMSDFGenerator.TBounds);
var ContourIndex:TpvSizeInt;
    Contour:TpvSignedDistanceField2DMSDFGenerator.PContour;
begin
 for ContourIndex:=0 to Count-1 do begin
  Contour:=@Contours[ContourIndex];
  Contour^.Bounds(aBounds);
 end;
end;

procedure TpvSignedDistanceField2DMSDFGenerator.TShape.BoundMiters(var aBounds:TpvSignedDistanceField2DMSDFGenerator.TBounds;const aBorder,aMiterLimit:TpvDouble;const aPolarity:TpvSizeInt);
var ContourIndex:TpvSizeInt;
    Contour:TpvSignedDistanceField2DMSDFGenerator.PContour;
begin
 for ContourIndex:=0 to Count-1 do begin
  Contour:=@Contours[ContourIndex];
  Contour^.BoundMiters(aBounds,aBorder,aMiterLimit,aPolarity);
 end;
end;

function TpvSignedDistanceField2DMSDFGenerator.TShape.GetBounds(const aBorder:TpvDouble;const aMiterLimit:TpvDouble;const aPolarity:TpvSizeInt):TpvSignedDistanceField2DMSDFGenerator.TBounds;
begin
 result.l:=MaxDouble;
 result.b:=MaxDouble;
 result.r:=-MaxDouble;
 result.t:=-MaxDouble;
 Bounds(result);
 if aBorder>0.0 then begin
  result.l:=result.l-aBorder;
  result.b:=result.b-aBorder;
  result.r:=result.r+aBorder;
  result.t:=result.t+aBorder;
  if aMiterLimit>0.0 then begin
   BoundMiters(result,aBorder,aMiterLimit,aPolarity);
  end;
 end;
end;

{ TpvSignedDistanceField2DMSDFGenerator }

class function TpvSignedDistanceField2DMSDFGenerator.Median(a,b,c:TpvDouble):TpvDouble;
begin
 result:=Max(Min(a,b),Min(Max(a,b),c));
end;

class function TpvSignedDistanceField2DMSDFGenerator.Sign(n:TpvDouble):TpvInt32;
begin
 result:=(ord(n<0) and 1)-(ord(n>0) and 1);
end;

class function TpvSignedDistanceField2DMSDFGenerator.NonZeroSign(n:TpvDouble):TpvInt32;
begin
 result:=((ord(n>0) and 1) shl 1)-1;
end;

class function TpvSignedDistanceField2DMSDFGenerator.SolveQuadratic(out x0,x1:TpvDouble;const a,b,c:TpvDouble):TpvSizeInt;
var d:TpvDouble;
begin
 if IsZero(a) or ((abs(b)+abs(c))>(TpvSignedDistanceField2DMSDFGenerator.TOO_LARGE_RATIO*abs(a))) then begin
  if IsZero(b) or (abs(c)>(TpvSignedDistanceField2DMSDFGenerator.TOO_LARGE_RATIO*abs(b))) then begin
   if IsZero(c) then begin
    result:=-1;
   end else begin
    result:=0;
   end;
  end else begin
   x0:=(-c)/b;
   result:=1;
  end;
 end else begin
  d:=sqr(b)-(4.0*a*c);
  if IsZero(d) then begin
   x0:=(-b)/(2.0*a);
   result:=1;
  end else if d>0.0 then begin
   d:=sqrt(d);
   x0:=((-b)+d)/(2.0*a);
   x1:=((-b)-d)/(2.0*a);
   result:=2;
  end else begin
   result:=0;
  end;
 end;
end;

class function TpvSignedDistanceField2DMSDFGenerator.SolveCubicNormed(out x0,x1,x2:TpvDouble;a,b,c:TpvDouble):TpvSizeInt;
var a2,q,r,r2,q3,t,aa,bb:TpvDouble;
begin
 a2:=sqr(a);
 q:=(a2-(3.0*b))/9.0;
 r:=((a*((2.0*a2)-(9.0*b)))+(27*c))/54.0;
 r2:=sqr(r);
 q3:=sqr(q)*q;
 if r2<q3 then begin
  t:=r/sqrt(q3);
  if t<-1.0 then begin
   t:=-1.0;
  end else if t>1.0 then begin
   t:=1.0;
  end;
  t:=ArcCos(t);
  a:=a/3.0;
  q:=(-2.0)*sqrt(q);
  x0:=(q*cos(t/3.0))-a;
  x1:=(q*cos(((t+2)*PI)/3.0))-a;
  x2:=(q*cos(((t-2)*PI)/3.0))-a;
  result:=3;
 end else begin
  aa:=-Power(abs(r)+sqrt(r2-q3),1.0/3.0);
  if r<0 then begin
   aa:=-aa;
  end;
  if IsZero(aa) then begin
   bb:=0.0;
  end else begin
   bb:=q/aa;
  end;
  a:=a/3.0;
  x0:=(aa+bb)-a;
  x1:=((-0.5)*(aa+bb))-a;
  x2:=0.5*sqrt(3)*(aa-bb);
  if abs(x2)<1e-14 then begin
   result:=2;
  end else begin
   result:=1;
  end;
 end;
end;

class function TpvSignedDistanceField2DMSDFGenerator.SolveCubic(out x0,x1,x2:TpvDouble;const a,b,c,d:TpvDouble):TpvSizeInt;
var bn,cn,dn:TpvDouble;
begin
 if IsZero(a) then begin
  result:=SolveQuadratic(x0,x1,b,c,d);
 end else begin
  bn:=b/a;
  cn:=c/a;
  dn:=d/a;
  if (abs(bn)<TpvSignedDistanceField2DMSDFGenerator.TOO_LARGE_RATIO) and
     (abs(cn)<TpvSignedDistanceField2DMSDFGenerator.TOO_LARGE_RATIO) and
     (abs(dn)<TpvSignedDistanceField2DMSDFGenerator.TOO_LARGE_RATIO) then begin
   result:=SolveCubicNormed(x0,x1,x2,bn,cn,dn);
  end else begin
   result:=SolveQuadratic(x0,x1,b,c,d);
  end;
 end;
end;

class function TpvSignedDistanceField2DMSDFGenerator.Shoelace(const a,b:TpvSignedDistanceField2DMSDFGenerator.TVector2):TpvDouble;
begin
 result:=(b.x-a.x)*(a.y+b.y);
end;

class procedure TpvSignedDistanceField2DMSDFGenerator.AutoFrame(const aShape:TpvSignedDistanceField2DMSDFGenerator.TShape;const aWidth,aHeight:TpvSizeInt;const aPixelRange:TpvDouble;out aTranslate,aScale:TpvSignedDistanceField2DMSDFGenerator.TVector2);
var Bounds:TpvSignedDistanceField2DMSDFGenerator.TBounds;
    l,b,r,t:TpvDouble;
    Frame,Dimensions:TpvSignedDistanceField2DMSDFGenerator.TVector2;
begin
 Bounds:=aShape.GetBounds;
 l:=Bounds.l;
 b:=Bounds.b;
 r:=Bounds.r;
 t:=Bounds.t;
 if (l>=r) or (b>=t) then begin
  l:=0.0;
  b:=0.0;
  r:=1.0;
  t:=1.0;
 end;
 Frame:=TpvSignedDistanceField2DMSDFGenerator.TVector2.Create(aWidth-aPixelRange,aHeight-aPixelRange);
 if (Frame.x<=1e-6) or (Frame.y<=1e-6) then begin
  raise EpvSignedDistanceField2DMSDFGenerator.Create('Cannot fit the specified pixel range');
 end;
 Dimensions:=TpvSignedDistanceField2DMSDFGenerator.TVector2.Create(r-l,t-b);
 if (Dimensions.x*Frame.y)<(Dimensions.y*Frame.x) then begin
  aTranslate:=TpvSignedDistanceField2DMSDFGenerator.TVector2.Create(((((Frame.x/Frame.y)*Dimensions.y)-Dimensions.x)*0.5)-l,-b);
  aScale:=TpvSignedDistanceField2DMSDFGenerator.TVector2.Create(Frame.y/Dimensions.y);
 end else begin
  aTranslate:=TpvSignedDistanceField2DMSDFGenerator.TVector2.Create(-l,((((Frame.y/Frame.x)*Dimensions.x)-Dimensions.y)*0.5)-b);
  aScale:=TpvSignedDistanceField2DMSDFGenerator.TVector2.Create(Frame.x/Dimensions.x);
 end;
 aTranslate:=aTranslate+(TpvSignedDistanceField2DMSDFGenerator.TVector2.Create(aPixelRange*0.5)/aScale);
end;

class function TpvSignedDistanceField2DMSDFGenerator.IsCorner(const aDirection,bDirection:TpvSignedDistanceField2DMSDFGenerator.TVector2;const aCrossThreshold:TpvDouble):boolean;
begin
 result:=(aDirection.Dot(bDirection)<=0) or (abs(aDirection.Dot(bDirection))>aCrossThreshold);
end;

class procedure TpvSignedDistanceField2DMSDFGenerator.SwitchColor(var aColor:TpvSignedDistanceField2DMSDFGenerator.TEdgeColor;var aSeed:TpvUInt64;const aBanned:TpvSignedDistanceField2DMSDFGenerator.TEdgeColor=TpvSignedDistanceField2DMSDFGenerator.TEdgeColor.BLACK);
const Start:array[0..2] of TpvSignedDistanceField2DMSDFGenerator.TEdgeColor=
       (
        TpvSignedDistanceField2DMSDFGenerator.TEdgeColor.CYAN,
        TpvSignedDistanceField2DMSDFGenerator.TEdgeColor.MAGENTA,
        TpvSignedDistanceField2DMSDFGenerator.TEdgeColor.YELLOW
       );
var Combined:TpvSignedDistanceField2DMSDFGenerator.TEdgeColor;
    Shifted:TpvUInt32;
begin
 Combined:=TpvSignedDistanceField2DMSDFGenerator.TEdgeColor(TpvUInt32(TpvUInt32(aColor) and TpvUInt32(aBanned)));
 case Combined of
  TpvSignedDistanceField2DMSDFGenerator.TEdgeColor.RED,
  TpvSignedDistanceField2DMSDFGenerator.TEdgeColor.GREEN,
  TpvSignedDistanceField2DMSDFGenerator.TEdgeColor.BLUE:begin
   aColor:=TpvSignedDistanceField2DMSDFGenerator.TEdgeColor(TpvUInt32(TpvUInt32(aColor) xor TpvUInt32(TpvSignedDistanceField2DMSDFGenerator.TEdgeColor.WHITE)));
  end;
  TpvSignedDistanceField2DMSDFGenerator.TEdgeColor.BLACK,
  TpvSignedDistanceField2DMSDFGenerator.TEdgeColor.WHITE:begin
   aColor:=Start[aSeed mod 3];
   aSeed:=aSeed div 3;
  end;
  else begin
   Shifted:=TpvUInt32(aColor) shl (1+(aSeed and 1));
   aColor:=TpvSignedDistanceField2DMSDFGenerator.TEdgeColor(TpvUInt32(TpvUInt32(Shifted or (Shifted shr 3)) and TpvUInt32(TpvSignedDistanceField2DMSDFGenerator.TEdgeColor.WHITE)));
   aSeed:=aSeed shr 1;
  end;
 end;
end;

class procedure TpvSignedDistanceField2DMSDFGenerator.EdgeColoringSimple(var aShape:TpvSignedDistanceField2DMSDFGenerator.TShape;const aAngleThreshold:TpvDouble;aSeed:TpvUInt64);
type TCorners=array of TpvSizeInt;
var ContourIndex,EdgeIndex,CountCorners,Corner,Spline,Start,Index:TpvSizeInt;
    CrossThreshold:TpvDouble;
    Corners:TCorners;
    Contour:TpvSignedDistanceField2DMSDFGenerator.PContour;
    PreviousDirection:TpvSignedDistanceField2DMSDFGenerator.TVector2;
    Edge:TpvSignedDistanceField2DMSDFGenerator.PEdgeSegment;
    Color,InitialColor:TpvSignedDistanceField2DMSDFGenerator.TEdgeColor;
    Colors:array[0..5] of TpvSignedDistanceField2DMSDFGenerator.TEdgeColor;
    Parts:array[0..6] of TpvSignedDistanceField2DMSDFGenerator.TEdgeSegment;
begin
 CrossThreshold:=sin(aAngleThreshold);
 Corners:=nil;
 try
  for ContourIndex:=0 to aShape.Count-1 do begin
   Contour:=@aShape.Contours[ContourIndex];
   try
    CountCorners:=0;
    if Contour^.Count>0 then begin
     PreviousDirection:=Contour^.Edges[Contour^.Count-1].Direction(1);
     for EdgeIndex:=0 to Contour^.Count-1 do begin
      Edge:=@Contour^.Edges[EdgeIndex];
      if IsCorner(PreviousDirection.Normalize,Edge^.Direction(0).Normalize,CrossThreshold) then begin
       if CountCorners>=length(Corners) then begin
        SetLength(Corners,(CountCorners+1)+((CountCorners+1) shr 1));
       end;
       Corners[CountCorners]:=EdgeIndex;
       inc(CountCorners);
      end;
      PreviousDirection:=Edge^.Direction(1);
     end;
     case CountCorners of
      0:begin
       for EdgeIndex:=0 to Contour^.Count-1 do begin
        Edge:=@Contour^.Edges[EdgeIndex];
        Edge^.Color:=TpvSignedDistanceField2DMSDFGenerator.TEdgeColor.WHITE;
       end;
      end;
      1:begin
       Colors[0]:=TpvSignedDistanceField2DMSDFGenerator.TEdgeColor.WHITE;
       TpvSignedDistanceField2DMSDFGenerator.SwitchColor(Colors[0],aSeed);
       Colors[2]:=Colors[0];
       TpvSignedDistanceField2DMSDFGenerator.SwitchColor(Colors[2],aSeed);
       Corner:=Corners[0];
       if Contour.Count>=3 then begin
        for EdgeIndex:=0 to Contour.Count-1 do begin
         Edge:=@Contour^.Edges[EdgeIndex];
         Edge^.Color:=Colors[3+trunc((((3+((2.875*EdgeIndex)/(Contour.Count-1)))-1.4375)+0.5)-3)];
        end;
       end else if Contour.Count>=1 then begin
        Contour.Edges[0].SplitInThirds(Parts[(3*Corner)+0],Parts[(3*Corner)+1],Parts[(3*Corner)+2]);
        if Contour.Count>=2 then begin
         Contour.Edges[0].SplitInThirds(Parts[3-(3*Corner)],Parts[4-(3*Corner)],Parts[5-(3*Corner)]);
         Parts[0].Color:=Colors[0];
         Parts[1].Color:=Colors[0];
         Parts[2].Color:=Colors[1];
         Parts[3].Color:=Colors[1];
         Parts[4].Color:=Colors[2];
         Parts[5].Color:=Colors[2];
         Contour.Count:=6;
         SetLength(Contour.Edges,6);
         Contour.Edges[0]:=Parts[0];
         Contour.Edges[1]:=Parts[1];
         Contour.Edges[2]:=Parts[2];
         Contour.Edges[3]:=Parts[3];
         Contour.Edges[4]:=Parts[4];
         Contour.Edges[5]:=Parts[5];
        end else begin
         Parts[0].Color:=Colors[0];
         Parts[1].Color:=Colors[1];
         Parts[2].Color:=Colors[2];
         Contour.Count:=3;
         SetLength(Contour.Edges,3);
         Contour.Edges[0]:=Parts[0];
         Contour.Edges[1]:=Parts[1];
         Contour.Edges[2]:=Parts[2];
        end;
       end;
      end;
      else begin
       Spline:=0;
       Start:=Corners[0];
       Color:=TpvSignedDistanceField2DMSDFGenerator.TEdgeColor.WHITE;
       TpvSignedDistanceField2DMSDFGenerator.SwitchColor(Color,aSeed);
       InitialColor:=Color;
       for EdgeIndex:=0 to Contour^.Count-1 do begin
        Index:=(Start+EdgeIndex) mod Contour^.Count;
        if ((Spline+1)<CountCorners) and (Corners[Spline+1]=Index) then begin
         inc(Spline);
         TpvSignedDistanceField2DMSDFGenerator.SwitchColor(Color,aSeed,TpvSignedDistanceField2DMSDFGenerator.TEdgeColor(TpvUInt32(TpvUInt32(ord(Spline=(CountCorners-1)) and 1)*TpvUInt32(InitialColor))));
        end;
        Contour^.Edges[Index].Color:=Color;
       end;
      end;
     end;
    end;
   finally
    Corners:=nil;
   end;
  end;
 finally
  Corners:=nil;
 end;

end;

class procedure TpvSignedDistanceField2DMSDFGenerator.GenerateDistanceFieldPixel(var aImage:TpvSignedDistanceField2DMSDFGenerator.TImage;const aShape:TpvSignedDistanceField2DMSDFGenerator.TShape;const aRange:TpvDouble;const aScale,aTranslate:TpvSignedDistanceField2DMSDFGenerator.TVector2;const aX,aY:TpvSizeInt);
type TEdgePoint=Record
      MinDistance:TpvSignedDistanceField2DMSDFGenerator.TSignedDistance;
      NearEdge:TpvSignedDistanceField2DMSDFGenerator.PEdgeSegment;
      NearParam:TpvDouble;
     end;
var x,y,ContourIndex,EdgeIndex:TpvSizeInt;
    Contour:TpvSignedDistanceField2DMSDFGenerator.PContour;
    Edge:TpvSignedDistanceField2DMSDFGenerator.PEdgeSegment;
    r,g,b:TEdgePoint;
    p:TpvSignedDistanceField2DMSDFGenerator.TVector2;
    Param:TpvDouble;
    MinDistance,Distance:TpvSignedDistanceField2DMSDFGenerator.TSignedDistance;
    Pixel:TpvSignedDistanceField2DMSDFGenerator.PPixel;
begin
 x:=aX;
 if aShape.InverseYAxis then begin
  y:=aImage.Height-(aY+1);
 end else begin
  y:=aY;
 end;
 p:=(TpvSignedDistanceField2DMSDFGenerator.TVector2.Create(x+0.5,y+0.5)/aScale)-aTranslate;
 MinDistance:=TpvSignedDistanceField2DMSDFGenerator.TSignedDistance.Empty;
 r.MinDistance:=TpvSignedDistanceField2DMSDFGenerator.TSignedDistance.Empty;
 r.NearEdge:=nil;
 r.NearParam:=0.0;
 g.MinDistance:=TpvSignedDistanceField2DMSDFGenerator.TSignedDistance.Empty;
 g.NearEdge:=nil;
 g.NearParam:=0.0;
 b.MinDistance:=TpvSignedDistanceField2DMSDFGenerator.TSignedDistance.Empty;
 b.NearEdge:=nil;
 b.NearParam:=0.0;
 for ContourIndex:=0 to aShape.Count-1 do begin
  Contour:=@aShape.Contours[ContourIndex];
  if Contour^.Count>0 then begin
   for EdgeIndex:=0 to Contour^.Count-1 do begin
    Edge:=@Contour^.Edges[EdgeIndex];
    Distance:=Edge^.MinSignedDistance(p,Param);
    if Distance<MinDistance then begin
     MinDistance:=Distance;
    end;
    if ((TpvUInt32(Edge^.Color) and TpvUInt32(TpvSignedDistanceField2DMSDFGenerator.TEdgeColor.RED))<>0) and (Distance<r.MinDistance) then begin
     r.MinDistance:=Distance;
     r.NearEdge:=Edge;
     r.NearParam:=Param;
    end;
    if ((TpvUInt32(Edge^.Color) and TpvUInt32(TpvSignedDistanceField2DMSDFGenerator.TEdgeColor.GREEN))<>0) and (Distance<g.MinDistance) then begin
     g.MinDistance:=Distance;
     g.NearEdge:=Edge;
     g.NearParam:=Param;
    end;
    if ((TpvUInt32(Edge^.Color) and TpvUInt32(TpvSignedDistanceField2DMSDFGenerator.TEdgeColor.BLUE))<>0) and (Distance<b.MinDistance) then begin
     b.MinDistance:=Distance;
     b.NearEdge:=Edge;
     b.NearParam:=Param;
    end;
   end;
  end;
 end;
 if assigned(r.NearEdge) then begin
  r.NearEdge^.DistanceToPseudoDistance(r.MinDistance,p,r.NearParam);
 end;
 if assigned(r.NearEdge) then begin
  g.NearEdge^.DistanceToPseudoDistance(g.MinDistance,p,g.NearParam);
 end;
 if assigned(r.NearEdge) then begin
  b.NearEdge^.DistanceToPseudoDistance(b.MinDistance,p,b.NearParam);
 end;
 Pixel:=@aImage.Pixels[(aY*aImage.Width)+aX];
 Pixel^.r:=(r.MinDistance.Distance/aRange)+0.5;
 Pixel^.g:=(g.MinDistance.Distance/aRange)+0.5;
 Pixel^.b:=(b.MinDistance.Distance/aRange)+0.5;
 Pixel^.a:=(MinDistance.Distance/aRange)+0.5;
end;

class procedure TpvSignedDistanceField2DMSDFGenerator.GenerateDistanceField(var aImage:TpvSignedDistanceField2DMSDFGenerator.TImage;const aShape:TpvSignedDistanceField2DMSDFGenerator.TShape;const aRange:TpvDouble;const aScale,aTranslate:TpvSignedDistanceField2DMSDFGenerator.TVector2);
var x,y:TpvSizeInt;
begin
 for y:=0 to aImage.Height-1 do begin
  for x:=0 to aImage.Width-1 do begin
   GenerateDistanceFieldPixel(aImage,aShape,aRange,aScale,aTranslate,x,y);
  end;
 end;
end;

class function TpvSignedDistanceField2DMSDFGenerator.DetectClash(const a,b:TpvSignedDistanceField2DMSDFGenerator.TPixel;const aThreshold:TpvDouble):boolean;
var a0,a1,a2,b0,b1,b2,t:TpvDouble;
begin
 a0:=a.r;
 a1:=a.g;
 a2:=a.b;
 b0:=b.r;
 b1:=b.g;
 b2:=b.b;
 if abs(b0-a0)<abs(b1-a1) then begin
  t:=a0;
  a0:=a1;
  a1:=t;
  t:=b0;
  b0:=b1;
  b1:=t;
 end;
 if abs(b1-a1)<abs(b2-a2) then begin
  t:=a1;
  a1:=a2;
  a2:=t;
  t:=b1;
  b1:=b2;
  b2:=t;
  if abs(b0-a0)<abs(b1-a1) then begin
   t:=a0;
   a0:=a1;
   a1:=t;
   t:=b0;
   b0:=b1;
   b1:=t;
  end;
 end;
 result:=(abs(b1-a1)>=aThreshold) and (not (SameValue(b0,b1) and SameValue(b0,b2))) and (abs(a2-0.5)>=abs(b2-0.5));
end;

class procedure TpvSignedDistanceField2DMSDFGenerator.ErrorCorrection(var aImage:TpvSignedDistanceField2DMSDFGenerator.TImage;const aThreshold:TpvSignedDistanceField2DMSDFGenerator.TVector2);
type TClash=record
      x,y:TpvSizeInt;
     end;
     PClash=^TClash;
     TClashes=array of TClash;
var x,y,Count,Index:TpvSizeInt;
    Clashes:TClashes;
    Clash:PClash;
    Pixel:TpvSignedDistanceField2DMSDFGenerator.PPixel;
    Median:TpvDouble;
begin

 Clashes:=nil;
 Count:=0;
 try
  for y:=0 to aImage.Height-1 do begin
   for x:=0 to aImage.Width-1 do begin
    if ((x>0) and TpvSignedDistanceField2DMSDFGenerator.DetectClash(aImage.Pixels[(y*aImage.Width)+x],aImage.Pixels[(y*aImage.Width)+(x-1)],aThreshold.x)) or
       ((x<(aImage.Width-1)) and TpvSignedDistanceField2DMSDFGenerator.DetectClash(aImage.Pixels[(y*aImage.Width)+x],aImage.Pixels[(y*aImage.Width)+(x+1)],aThreshold.x)) or
		   ((y>0) and TpvSignedDistanceField2DMSDFGenerator.DetectClash(aImage.Pixels[(y*aImage.Width)+x],aImage.Pixels[((y-1)*aImage.Width)+x],aThreshold.y)) or
       ((y<(aImage.Height-1)) and TpvSignedDistanceField2DMSDFGenerator.DetectClash(aImage.Pixels[(y*aImage.Width)+x],aImage.Pixels[((y+1)*aImage.Width)+x],aThreshold.y)) then begin
     if Count>=length(Clashes) then begin
      SetLength(Clashes,(Count+1)+((Count+1) shr 1));
     end;
     Clash:=@Clashes[Count];
     inc(Count);
     Clash^.x:=x;
     Clash^.y:=y;
    end;
   end;
  end;
  for Index:=0 to Count-1 do begin
   Clash:=@Clashes[Index];
   Pixel:=@aImage.Pixels[(Clash^.y*aImage.Width)+Clash^.x];
   Median:=TpvSignedDistanceField2DMSDFGenerator.Median(Pixel^.r,Pixel^.g,Pixel^.b);
   Pixel^.r:=Median;
   Pixel^.g:=Median;
   Pixel^.b:=Median;
  end;
 finally
  Clashes:=nil;
 end;


 Clashes:=nil;
 Count:=0;
 try
  for y:=0 to aImage.Height-1 do begin
   for x:=0 to aImage.Width-1 do begin
    if ((x>0) and (y>0) and TpvSignedDistanceField2DMSDFGenerator.DetectClash(aImage.Pixels[(y*aImage.Width)+x],aImage.Pixels[((y-1)*aImage.Width)+(x-1)],aThreshold.x)) or
       ((x<(aImage.Width-1)) and (y>0) and TpvSignedDistanceField2DMSDFGenerator.DetectClash(aImage.Pixels[(y*aImage.Width)+x],aImage.Pixels[((y-1)*aImage.Width)+(x+1)],aThreshold.x)) or
		   ((x>0) and (y<(aImage.Height-1)) and TpvSignedDistanceField2DMSDFGenerator.DetectClash(aImage.Pixels[(y*aImage.Width)+x],aImage.Pixels[((y+1)*aImage.Width)+(x-1)],aThreshold.y)) or
       ((x<(aImage.Width-1)) and (y<(aImage.Height-1)) and TpvSignedDistanceField2DMSDFGenerator.DetectClash(aImage.Pixels[(y*aImage.Width)+x],aImage.Pixels[((y+1)*aImage.Width)+(x+1)],aThreshold.y)) then begin
     if Count>=length(Clashes) then begin
      SetLength(Clashes,(Count+1)+((Count+1) shr 1));
     end;
     Clash:=@Clashes[Count];
     inc(Count);
     Clash^.x:=x;
     Clash^.y:=y;
    end;
   end;
  end;
  for Index:=0 to Count-1 do begin
   Clash:=@Clashes[Index];
   Pixel:=@aImage.Pixels[(Clash^.y*aImage.Width)+Clash^.x];
   Median:=TpvSignedDistanceField2DMSDFGenerator.Median(Pixel^.r,Pixel^.g,Pixel^.b);
   Pixel^.r:=Median;
   Pixel^.g:=Median;
   Pixel^.b:=Median;
  end;
 finally
  Clashes:=nil;
 end;

end;

{ TpvSignedDistanceField2DGenerator }

constructor TpvSignedDistanceField2DGenerator.Create;
begin
 inherited Create;
 fPointInPolygonPathSegments:=nil;
 fVectorPath:=nil;
 fDistanceField:=nil;
 fMultiChannelMode:=TMultiChannelMode.None;
end;

destructor TpvSignedDistanceField2DGenerator.Destroy;
begin
 fPointInPolygonPathSegments:=nil;
 fVectorPath:=nil;
 fDistanceField:=nil;
 inherited Destroy;
end;

function TpvSignedDistanceField2DGenerator.Clamp(const Value,MinValue,MaxValue:TpvInt64):TpvInt64;
begin
 if Value<=MinValue then begin
  result:=MinValue;
 end else if Value>=MaxValue then begin
  result:=MaxValue;
 end else begin
  result:=Value;
 end;
end;

function TpvSignedDistanceField2DGenerator.Clamp(const Value,MinValue,MaxValue:TpvDouble):TpvDouble;
begin
 if Value<=MinValue then begin
  result:=MinValue;
 end else if Value>=MaxValue then begin
  result:=MaxValue;
 end else begin
  result:=Value;
 end;
end;

function TpvSignedDistanceField2DGenerator.DoublePrecisionPointAdd(const a,b:TpvSignedDistanceField2DDoublePrecisionPoint):TpvSignedDistanceField2DDoublePrecisionPoint;
begin
 result.x:=a.x+b.x;
 result.y:=a.y+b.y;
end;

function TpvSignedDistanceField2DGenerator.DoublePrecisionPointSub(const a,b:TpvSignedDistanceField2DDoublePrecisionPoint):TpvSignedDistanceField2DDoublePrecisionPoint;
begin
 result.x:=a.x-b.x;
 result.y:=a.y-b.y;
end;

function TpvSignedDistanceField2DGenerator.DoublePrecisionPointLength(const p:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
begin
 result:=sqrt(sqr(p.x)+sqr(p.y));
end;

function TpvSignedDistanceField2DGenerator.DoublePrecisionPointDistance(const a,b:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
begin
 result:=sqrt(sqr(a.x-b.x)+sqr(a.y-b.y));
end;

function TpvSignedDistanceField2DGenerator.DoublePrecisionPointLengthSquared(const v:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
begin
 result:=sqr(v.x)+sqr(v.y);
end;

function TpvSignedDistanceField2DGenerator.DoublePrecisionPointDistanceSquared(const a,b:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
begin
 result:=sqr(a.x-b.x)+sqr(a.y-b.y);
end;

function TpvSignedDistanceField2DGenerator.DoublePrecisionPointCrossProduct(const a,b:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
begin
 result:=(a.x*b.y)-(a.y*b.x);
end;

function TpvSignedDistanceField2DGenerator.DoublePrecisionPointIsLeft(const a,b,c:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
begin
 result:=((b.x*a.x)*(c.y*a.y))-((c.x*a.x)*(b.y*a.y));
end;

function TpvSignedDistanceField2DGenerator.DoublePrecisionPointDotProduct(const a,b:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
begin
 result:=(a.x*b.x)+(a.y*b.y);
end;

function TpvSignedDistanceField2DGenerator.DoublePrecisionPointNormalize(const v:TpvSignedDistanceField2DDoublePrecisionPoint):TpvSignedDistanceField2DDoublePrecisionPoint;
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

function TpvSignedDistanceField2DGenerator.DoublePrecisionPointLerp(const a,b:TpvSignedDistanceField2DDoublePrecisionPoint;const t:TpvDouble):TpvSignedDistanceField2DDoublePrecisionPoint;
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

function TpvSignedDistanceField2DGenerator.DoublePrecisionPointLerpEx(const a,b:TpvSignedDistanceField2DDoublePrecisionPoint;const t:TpvDouble):TpvSignedDistanceField2DDoublePrecisionPoint;
begin
 result.x:=(a.x*(1.0-t))+(b.x*t);
 result.y:=(a.y*(1.0-t))+(b.y*t);
end;

function TpvSignedDistanceField2DGenerator.DoublePrecisionPointMap(const p:TpvSignedDistanceField2DDoublePrecisionPoint;const m:TpvSignedDistanceField2DDoublePrecisionAffineMatrix):TpvSignedDistanceField2DDoublePrecisionPoint;
begin
 result.x:=(p.x*m[0])+(p.y*m[1])+m[2];
 result.y:=(p.x*m[3])+(p.y*m[4])+m[5];
end;

procedure TpvSignedDistanceField2DGenerator.GetOffset(out oX,oY:TpvDouble);
begin
 case fMultiChannelMode of
  TMultiChannelMode.Gradients:begin
   case fColorChannelIndex of
    1:begin
     oX:=1.0;
     oY:=0.0;
    end;
    2:begin
     oX:=0.0;
     oY:=1.0;
    end;
    else {0:}begin
     oX:=0.0;
     oY:=0.0;
    end;
   end;
  end;
  TMultiChannelMode.Multisampling:begin
   case fColorChannelIndex of
    0:begin
     oX:=0.125;
     oY:=0.375;
    end;
    1:begin
     oX:=-0.125;
     oY:=-0.375;
    end;
    2:begin
     oX:=0.375;
     oY:=-0.125;
    end;
    else {3:}begin
     oX:=-0.375;
     oY:=0.125;
    end;
   end;
  end;
  else begin
   oX:=0.0;
   oY:=0.0;
  end;
 end;
end;

procedure TpvSignedDistanceField2DGenerator.ApplyOffset(var aX,aY:TpvDouble);
var oX,oY:TpvDouble;
begin
 GetOffset(oX,oY);
 aX:=aX+oX;
 aY:=aY+oY;
end;

function TpvSignedDistanceField2DGenerator.ApplyOffset(const aPoint:TpvSignedDistanceField2DDoublePrecisionPoint):TpvSignedDistanceField2DDoublePrecisionPoint;
var oX,oY:TpvDouble;
begin
 GetOffset(oX,oY);
 result.x:=aPoint.x+oX;
 result.y:=aPoint.y+oY;
end;

function TpvSignedDistanceField2DGenerator.BetweenClosedOpen(const a,b,c:TpvDouble;const Tolerance:TpvDouble=0.0;const XFormToleranceToX:boolean=false):boolean;
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

function TpvSignedDistanceField2DGenerator.BetweenClosed(const a,b,c:TpvDouble;const Tolerance:TpvDouble=0.0;const XFormToleranceToX:boolean=false):boolean;
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

function TpvSignedDistanceField2DGenerator.NearlyZero(const Value:TpvDouble;const Tolerance:TpvDouble=DistanceField2DNearlyZeroValue):boolean;
begin
 Assert(Tolerance>=0.0);
 result:=abs(Value)<=Tolerance;
end;

function TpvSignedDistanceField2DGenerator.NearlyEqual(const x,y:TpvDouble;const Tolerance:TpvDouble=DistanceField2DNearlyZeroValue;const XFormToleranceToX:boolean=false):boolean;
begin
 Assert(Tolerance>=0.0);
 if XFormToleranceToX then begin
  result:=abs(x-y)<=(Tolerance/sqrt((sqr(y)*4.0)+1.0));
 end else begin
  result:=abs(x-y)<=Tolerance;
 end;
end;

function TpvSignedDistanceField2DGenerator.SignOf(const Value:TpvDouble):TpvInt32;
begin
 if Value<0.0 then begin
  result:=-1;
 end else begin
  result:=1;
 end;
end;

function TpvSignedDistanceField2DGenerator.IsColinear(const Points:array of TpvSignedDistanceField2DDoublePrecisionPoint):boolean;
begin
 Assert(length(Points)=3);
 result:=abs(((Points[1].y-Points[0].y)*(Points[1].x-Points[2].x))-
             ((Points[1].y-Points[2].y)*(Points[1].x-Points[0].x)))<=DistanceField2DCloseSquaredValue;
end;

function TpvSignedDistanceField2DGenerator.PathSegmentDirection(const PathSegment:TpvSignedDistanceField2DPathSegment;const Which:TpvInt32):TpvSignedDistanceField2DDoublePrecisionPoint;
begin
 case PathSegment.Type_ of
  TpvSignedDistanceField2DPathSegmentType.Line:begin
   result.x:=PathSegment.Points[1].x-PathSegment.Points[0].x;
   result.y:=PathSegment.Points[1].y-PathSegment.Points[0].y;
  end;
  TpvSignedDistanceField2DPathSegmentType.QuadraticBezierCurve:begin
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

function TpvSignedDistanceField2DGenerator.PathSegmentCountPoints(const PathSegment:TpvSignedDistanceField2DPathSegment):TpvInt32;
begin
 case PathSegment.Type_ of
  TpvSignedDistanceField2DPathSegmentType.Line:begin
   result:=2;
  end;
  TpvSignedDistanceField2DPathSegmentType.QuadraticBezierCurve:begin
   result:=3;
  end;
  else begin
   result:=0;
   Assert(false);
  end;
 end;
end;

function TpvSignedDistanceField2DGenerator.PathSegmentEndPoint(const PathSegment:TpvSignedDistanceField2DPathSegment):PpvSignedDistanceField2DDoublePrecisionPoint;
begin
 case PathSegment.Type_ of
  TpvSignedDistanceField2DPathSegmentType.Line:begin
   result:=@PathSegment.Points[1];
  end;
  TpvSignedDistanceField2DPathSegmentType.QuadraticBezierCurve:begin
   result:=@PathSegment.Points[2];
  end;
  else begin
   result:=nil;
   Assert(false);
  end;
 end;
end;

function TpvSignedDistanceField2DGenerator.PathSegmentCornerPoint(const PathSegment:TpvSignedDistanceField2DPathSegment;const WhichA,WhichB:TpvInt32):PpvSignedDistanceField2DDoublePrecisionPoint;
begin
 case PathSegment.Type_ of
  TpvSignedDistanceField2DPathSegmentType.Line:begin
   result:=@PathSegment.Points[WhichB and 1];
  end;
  TpvSignedDistanceField2DPathSegmentType.QuadraticBezierCurve:begin
   result:=@PathSegment.Points[(WhichA and 1)+(WhichB and 1)];
  end;
  else begin
   result:=nil;
   Assert(false);
  end;
 end;
end;

procedure TpvSignedDistanceField2DGenerator.InitializePathSegment(var PathSegment:TpvSignedDistanceField2DPathSegment);
var p0,p1,p2,p1mp0,d,t,sp0,sp1,sp2,p01p,p02p,p12p:TpvSignedDistanceField2DDoublePrecisionPoint;
    Hypotenuse,CosTheta,SinTheta,a,b,h,c,g,f,gd,fd,x,y,Lambda:TpvDouble;
begin
 case PathSegment.Type_ of
  TpvSignedDistanceField2DPathSegmentType.Line:begin
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
 PathSegment.NearlyZeroScaled:=DistanceField2DNearlyZeroValue/PathSegment.ScalingFactor;
 PathSegment.SquaredTangentToleranceScaled:=sqr(DistanceField2DTangentToleranceValue)/PathSegment.SquaredScalingFactor;
 PathSegment.P0T:=DoublePrecisionPointMap(p0,PathSegment.XFormMatrix);
 PathSegment.P2T:=DoublePrecisionPointMap(p2,PathSegment.XFormMatrix);
end;

procedure TpvSignedDistanceField2DGenerator.InitializeDistances;
var Index:TpvInt32;
begin
 case fMultiChannelMode of
  TMultiChannelMode.Gradients:begin
   for Index:=0 to length(fDistanceFieldData)-1 do begin
    fDistanceFieldData[Index].SquaredDistance:=sqr(DistanceField2DMagnitudeValue);
    if fColorChannelIndex=0 then begin
     fDistanceFieldData[Index].Distance:=DistanceField2DMagnitudeValue;
    end;
    fDistanceFieldData[Index].DeltaWindingScore:=0;
   end;
  end;
  TMultiChannelMode.Multisampling:begin
   for Index:=0 to length(fDistanceFieldData)-1 do begin
    fDistanceFieldData[Index].SquaredDistance:=sqr(DistanceField2DMagnitudeValue);
    fDistanceFieldData[Index].DeltaWindingScore:=0;
   end;
  end;
  else begin
   for Index:=0 to length(fDistanceFieldData)-1 do begin
    fDistanceFieldData[Index].SquaredDistance:=sqr(DistanceField2DMagnitudeValue);
    fDistanceFieldData[Index].SquaredDistanceR:=sqr(DistanceField2DMagnitudeValue);
    fDistanceFieldData[Index].SquaredDistanceG:=sqr(DistanceField2DMagnitudeValue);
    fDistanceFieldData[Index].SquaredDistanceB:=sqr(DistanceField2DMagnitudeValue);
    fDistanceFieldData[Index].Distance:=DistanceField2DMagnitudeValue;
    fDistanceFieldData[Index].PseudoSquaredDistanceR:=sqr(DistanceField2DMagnitudeValue);
    fDistanceFieldData[Index].PseudoSquaredDistanceG:=sqr(DistanceField2DMagnitudeValue);
    fDistanceFieldData[Index].PseudoSquaredDistanceB:=sqr(DistanceField2DMagnitudeValue);
    fDistanceFieldData[Index].DeltaWindingScore:=0;
   end;
  end;
 end;
end;

function TpvSignedDistanceField2DGenerator.AddLineToPathSegmentArray(var Contour:TpvSignedDistanceField2DPathContour;const Points:array of TpvSignedDistanceField2DDoublePrecisionPoint):TpvInt32;
var PathSegment:PpvSignedDistanceField2DPathSegment;
begin
 Assert(length(Points)=2);
 result:=Contour.CountPathSegments;
 if not (SameValue(Points[0].x,Points[1].x) and SameValue(Points[0].y,Points[1].y)) then begin
  inc(Contour.CountPathSegments);
  if length(Contour.PathSegments)<=Contour.CountPathSegments then begin
   SetLength(Contour.PathSegments,Contour.CountPathSegments*2);
  end;
  PathSegment:=@Contour.PathSegments[result];
  PathSegment^.Type_:=TpvSignedDistanceField2DPathSegmentType.Line;
  PathSegment^.Color:=TpvSignedDistanceField2DPathSegmentColor.Black;
  PathSegment^.Points[0]:=Points[0];
  PathSegment^.Points[1]:=Points[1];
  InitializePathSegment(PathSegment^);
 end;
end;

function TpvSignedDistanceField2DGenerator.AddQuadraticBezierCurveToPathSegmentArray(var Contour:TpvSignedDistanceField2DPathContour;const Points:array of TpvSignedDistanceField2DDoublePrecisionPoint):TpvInt32;
var PathSegment:PpvSignedDistanceField2DPathSegment;
begin
 Assert(length(Points)=3);
 result:=Contour.CountPathSegments;
 if (DoublePrecisionPointDistanceSquared(Points[0],Points[1])<DistanceField2DCloseSquaredValue) or
    (DoublePrecisionPointDistanceSquared(Points[1],Points[2])<DistanceField2DCloseSquaredValue) or
    IsColinear(Points) then begin
  if not (SameValue(Points[0].x,Points[2].x) and SameValue(Points[0].y,Points[2].y)) then begin
   inc(Contour.CountPathSegments);
   if length(Contour.PathSegments)<=Contour.CountPathSegments then begin
    SetLength(Contour.PathSegments,Contour.CountPathSegments*2);
   end;
   PathSegment:=@Contour.PathSegments[result];
   PathSegment^.Type_:=TpvSignedDistanceField2DPathSegmentType.Line;
   PathSegment^.Color:=TpvSignedDistanceField2DPathSegmentColor.Black;
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
  PathSegment^.Type_:=TpvSignedDistanceField2DPathSegmentType.QuadraticBezierCurve;
  PathSegment^.Color:=TpvSignedDistanceField2DPathSegmentColor.Black;
  PathSegment^.Points[0]:=Points[0];
  PathSegment^.Points[1]:=Points[1];
  PathSegment^.Points[2]:=Points[2];
  InitializePathSegment(PathSegment^);
 end;
end;

function TpvSignedDistanceField2DGenerator.AddQuadraticBezierCurveAsSubdividedLinesToPathSegmentArray(var Contour:TpvSignedDistanceField2DPathContour;const Points:array of TpvSignedDistanceField2DDoublePrecisionPoint):TpvInt32;
var LastPoint:TpvSignedDistanceField2DDoublePrecisionPoint;
 procedure LineToPointAt(const Point:TpvSignedDistanceField2DDoublePrecisionPoint);
 begin
  if not (SameValue(LastPoint.x,Point.x) and SameValue(LastPoint.y,Point.y)) then begin
   AddLineToPathSegmentArray(Contour,[LastPoint,Point]);
  end;
  LastPoint:=Point;
 end;
 procedure Recursive(const x1,y1,x2,y2,x3,y3:TpvDouble;const Level:TpvInt32);
 var x12,y12,x23,y23,x123,y123,dx,dy:TpvDouble;
     Point:TpvSignedDistanceField2DDoublePrecisionPoint;
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

function TpvSignedDistanceField2DGenerator.AddCubicBezierCurveAsSubdividedQuadraticBezierCurvesToPathSegmentArray(var Contour:TpvSignedDistanceField2DPathContour;const Points:array of TpvSignedDistanceField2DDoublePrecisionPoint):TpvInt32;
const MaxChoppedPoints=10;
type TChoppedPoints=array[0..MaxChoppedPoints-1] of TpvSignedDistanceField2DDoublePrecisionPoint;
var ChoppedPoints:TChoppedPoints;
 procedure OutputLine(const aP0,aP1:TpvSignedDistanceField2DDoublePrecisionPoint);
 begin
  AddLineToPathSegmentArray(Contour,[aP0,aP1]);
 end;
 procedure OutputQuad(const aP0,aP1,aP2:TpvSignedDistanceField2DDoublePrecisionPoint);
 begin
  AddQuadraticBezierCurveToPathSegmentArray(Contour,[aP0,aP1,aP2]);
 end;
 procedure ChopCubicAt(Src,Dst:PpvSignedDistanceField2DDoublePrecisionPoints;const t:TpvDouble); overload;
 var p0,p1,p2,p3,ab,bc,cd,abc,bcd,abcd:TpvSignedDistanceField2DDoublePrecisionPoint;
 begin
  if SameValue(t,1.0) then begin
   Dst^[0]:=Src^[0];
   Dst^[1]:=Src^[1];
   Dst^[2]:=Src^[2];
   Dst^[3]:=Src^[3];
   Dst^[4]:=Src^[3];
   Dst^[5]:=Src^[3];
   Dst^[6]:=Src^[3];
  end else begin
   p0:=Src^[0];
   p1:=Src^[1];
   p2:=Src^[2];
   p3:=Src^[3];
   ab:=DoublePrecisionPointLerpEx(p0,p1,t);
   bc:=DoublePrecisionPointLerpEx(p1,p2,t);
   cd:=DoublePrecisionPointLerpEx(p2,p3,t);
   abc:=DoublePrecisionPointLerpEx(ab,bc,t);
   bcd:=DoublePrecisionPointLerpEx(bc,cd,t);
   abcd:=DoublePrecisionPointLerpEx(abc,bcd,t);
   Dst^[0]:=p0;
   Dst^[1]:=ab;
   Dst^[2]:=abc;
   Dst^[3]:=abcd;
   Dst^[4]:=bcd;
   Dst^[5]:=cd;
   Dst^[6]:=p3;
  end;
 end;
 procedure ChopCubicAt(Src,Dst:PpvSignedDistanceField2DDoublePrecisionPoints;const t0,t1:TpvDouble); overload;
 var p0,p1,p2,p3,
     ab0,bc0,cd0,abc0,bcd0,abcd0,
     ab1,bc1,cd1,abc1,bcd1,abcd1,
     Middle0,Middle1:TpvSignedDistanceField2DDoublePrecisionPoint;
 begin
  if SameValue(t1,1.0) then begin
   ChopCubicAt(Src,Dst,t0);
   Dst^[7]:=Src^[3];
   Dst^[8]:=Src^[3];
   Dst^[9]:=Src^[3];
  end else begin
   p0:=Src^[0];
   p1:=Src^[1];
   p2:=Src^[2];
   p3:=Src^[3];
   ab0:=DoublePrecisionPointLerpEx(p0,p1,t0);
   bc0:=DoublePrecisionPointLerpEx(p1,p2,t0);
   cd0:=DoublePrecisionPointLerpEx(p2,p3,t0);
   abc0:=DoublePrecisionPointLerpEx(ab0,bc0,t0);
   bcd0:=DoublePrecisionPointLerpEx(bc0,cd0,t0);
   abcd0:=DoublePrecisionPointLerpEx(abc0,bcd0,t0);
   ab1:=DoublePrecisionPointLerpEx(p0,p1,t1);
   bc1:=DoublePrecisionPointLerpEx(p1,p2,t1);
   cd1:=DoublePrecisionPointLerpEx(p2,p3,t1);
   abc1:=DoublePrecisionPointLerpEx(ab1,bc1,t1);
   bcd1:=DoublePrecisionPointLerpEx(bc1,cd1,t1);
   abcd1:=DoublePrecisionPointLerpEx(abc1,bcd1,t1);
   Middle0:=DoublePrecisionPointLerpEx(abc0,bcd0,t1);
   Middle1:=DoublePrecisionPointLerpEx(abc1,bcd1,t0);
   Dst^[0]:=p0;
   Dst^[1]:=ab0;
   Dst^[2]:=abc0;
   Dst^[3]:=abcd0;
   Dst^[4]:=Middle0;
   Dst^[5]:=Middle1;
   Dst^[6]:=abcd1;
   Dst^[7]:=bcd1;
   Dst^[8]:=cd1;
   Dst^[9]:=p3;
  end;
 end;
 function ChopCubicAtInflections(const aSrc:array of TpvSignedDistanceField2DDoublePrecisionPoint;out aDst:TChoppedPoints):TpvSizeInt;
  function ValidUnitDivide(aNumerator,aDenominator:TpvDouble;out aRatio:TpvDouble):boolean;
  begin
   if aNumerator<0.0 then begin
    aNumerator:=-aNumerator;
    aDenominator:=-aDenominator;
   end;
   if IsZero(aNumerator) or IsZero(aDenominator) or (aNumerator>=aDenominator) then begin
    result:=false;
   end else begin
    aRatio:=aNumerator/aDenominator;
    if IsNaN(aRatio) or IsZero(aRatio) then begin
     result:=false;
    end else begin
     result:=true;
    end;
   end;
  end;
  function FindUnitQuadRoots(const A,B,C:TpvDouble;out aRoot0,aRoot1:TpvDouble):TpvSizeInt;
  var dr,Q:TpvDouble;
  begin
   if IsZero(A) then begin
    if ValidUnitDivide(-C,B,aRoot0) then begin
     result:=1;
    end else begin
     result:=0;
    end;
   end else begin
    dr:=sqr(B)-(4.0*A*C);
    if dr<0.0 then begin
     result:=0;
    end else begin
     dr:=sqrt(dr);
     if IsInfinite(dr) or IsNaN(dr) then begin
      result:=0;
     end else begin
      if B<0.0 then begin
       Q:=-(B-dr)*0.5;
      end else begin
       Q:=-(B+dr)*0.5;
      end;
      if ValidUnitDivide(Q,A,aRoot0) then begin
       if ValidUnitDivide(C,Q,aRoot1) then begin
        result:=2;
        if aRoot0>aRoot1 then begin
         Q:=aRoot0;
         aRoot0:=aRoot1;
         aRoot1:=Q;
        end else if SameValue(aRoot0,aRoot1) then begin
         dec(result);
        end;
       end else begin
        result:=1;
       end;
      end else begin
       if ValidUnitDivide(C,Q,aRoot0) then begin
        result:=1;
       end else begin
        result:=0;
       end;
      end;
     end;
    end;
   end;
  end;
 var Index,Count:TpvSizeInt;
     Times:array[0..1] of TpvDouble;
     Ax,Ay,Bx,By,Cx,Cy,t0,t1,LastTime:TpvDouble;
     Src:PpvSignedDistanceField2DDoublePrecisionPoint;
     Dst:PpvSignedDistanceField2DDoublePrecisionPoint;
 begin
  Ax:=aSrc[1].x-aSrc[0].x;
  Ay:=aSrc[1].y-aSrc[0].y;
  Bx:=aSrc[2].x-(2.0*aSrc[1].x)+aSrc[0].x;
  By:=aSrc[2].y-(2.0*aSrc[1].y)+aSrc[0].y;
  Cx:=aSrc[3].x+(3.0*(aSrc[1].x-aSrc[2].x))-aSrc[0].x;
  Cy:=aSrc[3].y+(3.0*(aSrc[1].y-aSrc[2].y))-aSrc[0].y;
  Count:=FindUnitQuadRoots((Bx*Cy)-(By*Cx),(Ax*Cy)-(Ay*Cx),(Ax*By)-(Ay*Bx),Times[0],Times[1]);
  if Count=0 then begin
   aDst[0]:=aSrc[0];
   aDst[1]:=aSrc[1];
   aDst[2]:=aSrc[2];
   aDst[3]:=aSrc[3];
  end else begin
   Src:=@aSrc[0];
   Dst:=@aDst[0];
   Index:=0;
   while Index<(Count-1) do begin
    t0:=Times[Index+0];
    t1:=Times[Index+1];
    if Index<>0 then begin
     LastTime:=Times[Index-1];
     t0:=Clamp(TpvDouble((t0-LastTime)/(1.0-LastTime)),TpvDouble(0.0),TpvDouble(1.0));
     t1:=Clamp(TpvDouble((t1-LastTime)/(1.0-LastTime)),TpvDouble(0.0),TpvDouble(1.0));
    end;
    ChopCubicAt(TpvPointer(Src),TpvPointer(Dst),t0,t1);
    inc(Src,4);
    inc(Dst,6);
    inc(Index,2);
   end;
   if Index<Count then begin
    t0:=Times[Index];
    if Index<>0 then begin
     LastTime:=Times[Index-1];
     t0:=Clamp(TpvDouble((t0-LastTime)/(1.0-LastTime)),TpvDouble(0.0),TpvDouble(1.0));
    end;
    ChopCubicAt(TpvPointer(Src),TpvPointer(Dst),t0);
   end;
  end;
  result:=Count+1;
 end;
 procedure ConvertNonInflectCubicToQuads(const aPoints:PpvSignedDistanceField2DDoublePrecisionPoints;const aSquaredTolerance:TpvDouble;const aSubLevel:TpvSizeInt=0;const aPreserveFirstTangent:boolean=true;const aPreserveLastTangent:boolean=true);
 const LengthScale=DistanceField2DScalar1Value*1.5;
       MaxSubdivisions=10;
 var ab,dc,c0,c1,c:TpvSignedDistanceField2DDoublePrecisionPoint;
     p:array[0..7] of TpvSignedDistanceField2DDoublePrecisionPoint;
 begin
  ab:=DoublePrecisionPointSub(aPoints^[1],aPoints^[0]);
  dc:=DoublePrecisionPointSub(aPoints^[2],aPoints^[3]);
  if DoublePrecisionPointLengthSquared(ab)<DistanceField2DNearlyZeroValue then begin
   if DoublePrecisionPointLengthSquared(dc)<DistanceField2DNearlyZeroValue then begin
    OutputLine(aPoints^[0],aPoints^[3]);
    exit;
   end else begin
    ab:=DoublePrecisionPointSub(aPoints^[2],aPoints^[0]);
   end;
  end;
  if DoublePrecisionPointLengthSquared(dc)<DistanceField2DNearlyZeroValue then begin
   dc:=DoublePrecisionPointSub(aPoints^[1],aPoints^[3]);
  end;
  ab.x:=ab.x*LengthScale;
  ab.y:=ab.y*LengthScale;
  dc.x:=dc.x*LengthScale;
  dc.y:=dc.y*LengthScale;
  c0:=DoublePrecisionPointAdd(aPoints^[0],ab);
  c1:=DoublePrecisionPointAdd(aPoints^[3],dc);
  if (aSubLevel>MaxSubdivisions) or (DoublePrecisionPointDistanceSquared(c0,c1)<aSquaredTolerance) then begin
   if aPreserveFirstTangent=aPreserveLastTangent then begin
    c:=DoublePrecisionPointLerpEx(c0,c1,0.5);
   end else if aPreserveFirstTangent then begin
    c:=c0;
   end else begin
    c:=c1;
   end;
   OutputQuad(aPoints^[0],c,aPoints^[3]);
  end else begin
   ChopCubicAt(aPoints,TpvPointer(@p[0]),0.5);
   ConvertNonInflectCubicToQuads(TpvPointer(@p[0]),aSquaredTolerance,aSubLevel+1,aPreserveFirstTangent,false);
   ConvertNonInflectCubicToQuads(TpvPointer(@p[3]),aSquaredTolerance,aSubLevel+1,false,aPreserveLastTangent);
  end;
 end;
var Count,Index:TpvSizeInt;
begin
 Assert(length(Points)=4);
 result:=Contour.CountPathSegments;
 if not ((IsNaN(Points[0].x) or IsInfinite(Points[0].x)) or
         (IsNaN(Points[0].y) or IsInfinite(Points[0].y)) or
         (IsNaN(Points[1].x) or IsInfinite(Points[1].x)) or
         (IsNaN(Points[1].y) or IsInfinite(Points[1].y)) or
         (IsNaN(Points[2].x) or IsInfinite(Points[2].x)) or
         (IsNaN(Points[2].y) or IsInfinite(Points[2].y)) or
         (IsNaN(Points[3].x) or IsInfinite(Points[3].x)) or
         (IsNaN(Points[3].y) or IsInfinite(Points[3].y))) then begin
  Count:=ChopCubicAtInflections(Points,ChoppedPoints);
  if Count>0 then begin
   for Index:=0 to Count-1 do begin
    ConvertNonInflectCubicToQuads(TpvPointer(@ChoppedPoints[Index*3]),DistanceField2DScalar1Value,0,true,true);
   end;
  end;
 end;
end;

function TpvSignedDistanceField2DGenerator.AddCubicBezierCurveAsSubdividedLinesToPathSegmentArray(var Contour:TpvSignedDistanceField2DPathContour;const Points:array of TpvSignedDistanceField2DDoublePrecisionPoint):TpvInt32;
var LastPoint:TpvSignedDistanceField2DDoublePrecisionPoint;
 procedure LineToPointAt(const Point:TpvSignedDistanceField2DDoublePrecisionPoint);
 begin
  if not (SameValue(LastPoint.x,Point.x) and SameValue(LastPoint.y,Point.y)) then begin
   AddLineToPathSegmentArray(Contour,[LastPoint,Point]);
  end;
  LastPoint:=Point;
 end;
 procedure Recursive(const x1,y1,x2,y2,x3,y3,x4,y4:TpvDouble;const Level:TpvInt32);
 var x12,y12,x23,y23,x34,y34,x123,y123,x234,y234,x1234,y1234,dx,dy:TpvDouble;
     Point:TpvSignedDistanceField2DDoublePrecisionPoint;
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

function TpvSignedDistanceField2DGenerator.CubeRoot(Value:TpvDouble):TpvDouble;
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

function TpvSignedDistanceField2DGenerator.CalculateNearestPointForQuadraticBezierCurve(const PathSegment:TpvSignedDistanceField2DPathSegment;const XFormPoint:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
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

procedure TpvSignedDistanceField2DGenerator.PrecomputationForRow(out RowData:TpvSignedDistanceField2DRowData;const PathSegment:TpvSignedDistanceField2DPathSegment;const PointLeft,PointRight:TpvSignedDistanceField2DDoublePrecisionPoint);
var XFormPointLeft,XFormPointRight:TpvSignedDistanceField2DDoublePrecisionPoint;
    x0,y0,x1,y1,m,b,m2,c,Tolerance,d:TpvDouble;
begin
 if PathSegment.Type_=TpvSignedDistanceField2DPathSegmentType.QuadraticBezierCurve then begin
  XFormPointLeft:=DoublePrecisionPointMap(PointLeft,PathSegment.XFormMatrix);
  XFormPointRight:=DoublePrecisionPointMap(PointRight,PathSegment.XFormMatrix);
  RowData.QuadraticXDirection:=SignOf(PathSegment.P2T.x-PathSegment.P0T.x);
  RowData.ScanlineXDirection:=SignOf(XFormPointRight.x-XFormPointLeft.x);
  x0:=XFormPointLeft.x;
  y0:=XFormPointLeft.y;
  x1:=XFormPointRight.x;
  y1:=XFormPointRight.y;
  if NearlyEqual(x0,x1,PathSegment.NearlyZeroScaled,true) then begin
   RowData.IntersectionType:=TpvSignedDistanceField2DRowDataIntersectionType.VerticalLine;
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
    RowData.IntersectionType:=TpvSignedDistanceField2DRowDataIntersectionType.TangentLine;
    RowData.XAtIntersection[0]:=m*0.5;
    RowData.XAtIntersection[1]:=m*0.5;
   end else if c<=0.0 then begin
    RowData.IntersectionType:=TpvSignedDistanceField2DRowDataIntersectionType.NoIntersection;
   end else begin
    RowData.IntersectionType:=TpvSignedDistanceField2DRowDataIntersectionType.TwoPointsIntersect;
    d:=sqrt(c);
    RowData.XAtIntersection[0]:=(m+d)*0.5;
    RowData.XAtIntersection[1]:=(m-d)*0.5;
   end;
  end;
 end;
end;

function TpvSignedDistanceField2DGenerator.CalculateSideOfQuadraticBezierCurve(const PathSegment:TpvSignedDistanceField2DPathSegment;const Point,XFormPoint:TpvSignedDistanceField2DDoublePrecisionPoint;const RowData:TpvSignedDistanceField2DRowData):TpvSignedDistanceField2DPathSegmentSide;
var p0,p1:TpvDouble;
    sp0,sp1:TpvInt32;
    ip0,ip1:boolean;
begin
 case RowData.IntersectionType of
  TpvSignedDistanceField2DRowDataIntersectionType.VerticalLine:begin
   result:=TpvSignedDistanceField2DPathSegmentSide(TpvInt32(SignOf(XFormPoint.y-RowData.YAtIntersection)*RowData.QuadraticXDirection));
  end;
  TpvSignedDistanceField2DRowDataIntersectionType.TwoPointsIntersect:begin
   result:=TpvSignedDistanceField2DPathSegmentSide.None;
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
    result:=TpvSignedDistanceField2DPathSegmentSide(TpvInt32(sp0*RowData.QuadraticXDirection));
   end;
   if ip1 and BetweenClosed(p1,PathSegment.P0T.x,PathSegment.P2T.x,PathSegment.NearlyZeroScaled,true) then begin
    sp1:=SignOf(p1-XFormPoint.x);
    if (result=TpvSignedDistanceField2DPathSegmentSide.None) or (sp1=1) then begin
     result:=TpvSignedDistanceField2DPathSegmentSide(TpvInt32(-sp1*RowData.QuadraticXDirection));
    end;
   end;
  end;
  TpvSignedDistanceField2DRowDataIntersectionType.TangentLine:begin
   result:=TpvSignedDistanceField2DPathSegmentSide.None;
   if RowData.ScanlineXDirection=1 then begin
    if SameValue(PathSegment.Points[0].y,Point.y) then begin
     result:=TpvSignedDistanceField2DPathSegmentSide(TpvInt32(SignOf(RowData.XAtIntersection[0]-XFormPoint.x)));
    end else if SameValue(PathSegment.Points[2].y,Point.y) then begin
     result:=TpvSignedDistanceField2DPathSegmentSide(TpvInt32(SignOf(XFormPoint.x-RowData.XAtIntersection[0])));
    end;
   end;
  end;
  else begin
   result:=TpvSignedDistanceField2DPathSegmentSide.None;
  end;
 end;
end;

function TpvSignedDistanceField2DGenerator.DistanceToPathSegment(const Point:TpvSignedDistanceField2DDoublePrecisionPoint;const PathSegment:TpvSignedDistanceField2DPathSegment;const RowData:TpvSignedDistanceField2DRowData;out PathSegmentSide:TpvSignedDistanceField2DPathSegmentSide):TpvDouble;
var XFormPoint,x:TpvSignedDistanceField2DDoublePrecisionPoint;
    NearestPoint:TpvDouble;
begin
 XFormPoint:=DoublePrecisionPointMap(Point,PathSegment.XFormMatrix);
 case PathSegment.Type_ of
  TpvSignedDistanceField2DPathSegmentType.Line:begin
   if BetweenClosed(XFormPoint.x,PathSegment.P0T.x,PathSegment.P2T.x) then begin
    result:=sqr(XFormPoint.y);
   end else if XFormPoint.x<PathSegment.P0T.x then begin
    result:=sqr(XFormPoint.x)+sqr(XFormPoint.y);
   end else begin
    result:=sqr(XFormPoint.x-PathSegment.P2T.x)+sqr(XFormPoint.y);
   end;
   if BetweenClosedOpen(Point.y,PathSegment.BoundingBox.Min.y,PathSegment.BoundingBox.Max.y) then begin
    PathSegmentSide:=TpvSignedDistanceField2DPathSegmentSide(TpvInt32(SignOf(XFormPoint.y)));
   end else begin
    PathSegmentSide:=TpvSignedDistanceField2DPathSegmentSide.None;
   end;
  end;
  TpvSignedDistanceField2DPathSegmentType.QuadraticBezierCurve:begin
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
    PathSegmentSide:=TpvSignedDistanceField2DPathSegmentSide.None;
   end;
  end;
  else begin
   PathSegmentSide:=TpvSignedDistanceField2DPathSegmentSide.None;
   result:=0.0;
  end;
 end;
end;

procedure TpvSignedDistanceField2DGenerator.ConvertShape(const DoSubdivideCurvesIntoLines:boolean);
var CommandIndex:TpvInt32;
    Command:TpvVectorPathCommand;
    Contour:PpvSignedDistanceField2DPathContour;
    StartPoint,LastPoint,ControlPoint,OtherControlPoint,Point:TpvSignedDistanceField2DDoublePrecisionPoint;
    Scale:TpvDouble;
begin
 Scale:=fScale*DistanceField2DRasterizerToScreenScale;
 fShape.Contours:=nil;
 fShape.CountContours:=0;
 try
  Contour:=nil;
  try
   StartPoint.x:=0.0;
   StartPoint.y:=0.0;
   LastPoint.x:=0.0;
   LastPoint.y:=0.0;
   for CommandIndex:=0 to fVectorPath.Commands.Count-1 do begin
    Command:=fVectorPath.Commands[CommandIndex];
    case Command.CommandType of
     TpvVectorPathCommandType.MoveTo:begin
      if assigned(Contour) then begin
       if not (SameValue(LastPoint.x,StartPoint.x) and SameValue(LastPoint.y,StartPoint.y)) then begin
        AddLineToPathSegmentArray(Contour^,[LastPoint,StartPoint]);
       end;
       SetLength(Contour^.PathSegments,Contour^.CountPathSegments);
      end;
      if length(fShape.Contours)<(fShape.CountContours+1) then begin
       SetLength(fShape.Contours,(fShape.CountContours+1)*2);
      end;
      Contour:=@fShape.Contours[fShape.CountContours];
      inc(fShape.CountContours);
      LastPoint.x:=(Command.x0*Scale)+fOffsetX;
      LastPoint.y:=(Command.y0*Scale)+fOffsetY;
      StartPoint:=LastPoint;
     end;
     TpvVectorPathCommandType.LineTo:begin
      if not assigned(Contour) then begin
       if length(fShape.Contours)<(fShape.CountContours+1) then begin
        SetLength(fShape.Contours,(fShape.CountContours+1)*2);
       end;
       Contour:=@fShape.Contours[fShape.CountContours];
       inc(fShape.CountContours);
      end;
      Point.x:=(Command.x0*Scale)+fOffsetX;
      Point.y:=(Command.y0*Scale)+fOffsetY;
      if assigned(Contour) and not (SameValue(LastPoint.x,Point.x) and SameValue(LastPoint.y,Point.y)) then begin
       AddLineToPathSegmentArray(Contour^,[LastPoint,Point]);
      end;
      LastPoint:=Point;
     end;
     TpvVectorPathCommandType.QuadraticCurveTo:begin
      if not assigned(Contour) then begin
       if length(fShape.Contours)<(fShape.CountContours+1) then begin
        SetLength(fShape.Contours,(fShape.CountContours+1)*2);
       end;
       Contour:=@fShape.Contours[fShape.CountContours];
       inc(fShape.CountContours);
      end;
      ControlPoint.x:=(Command.x0*Scale)+fOffsetX;
      ControlPoint.y:=(Command.y0*Scale)+fOffsetY;
      Point.x:=(Command.x1*Scale)+fOffsetX;
      Point.y:=(Command.y1*Scale)+fOffsetY;
      if assigned(Contour) and not ((SameValue(LastPoint.x,Point.x) and SameValue(LastPoint.y,Point.y)) and
                                    (SameValue(LastPoint.x,ControlPoint.x) and SameValue(LastPoint.y,ControlPoint.y))) then begin
       if DoSubdivideCurvesIntoLines then begin
        AddQuadraticBezierCurveAsSubdividedLinesToPathSegmentArray(Contour^,[LastPoint,ControlPoint,Point]);
       end else begin
//      AddQuadraticBezierCurveAsSubdividedLinesToPathSegmentArray(Contour^,[LastPoint,ControlPoint,Point]);
        AddQuadraticBezierCurveToPathSegmentArray(Contour^,[LastPoint,ControlPoint,Point]);
       end;
      end;
      LastPoint:=Point;
     end;
     TpvVectorPathCommandType.CubicCurveTo:begin
      if not assigned(Contour) then begin
       if length(fShape.Contours)<(fShape.CountContours+1) then begin
        SetLength(fShape.Contours,(fShape.CountContours+1)*2);
       end;
       Contour:=@fShape.Contours[fShape.CountContours];
       inc(fShape.CountContours);
      end;
      ControlPoint.x:=(Command.x0*Scale)+fOffsetX;
      ControlPoint.y:=(Command.y0*Scale)+fOffsetY;
      OtherControlPoint.x:=(Command.x1*Scale)+fOffsetX;
      OtherControlPoint.y:=(Command.y1*Scale)+fOffsetY;
      Point.x:=(Command.x2*Scale)+fOffsetX;
      Point.y:=(Command.y2*Scale)+fOffsetY;
      if assigned(Contour) and not ((SameValue(LastPoint.x,Point.x) and SameValue(LastPoint.y,Point.y)) and
                                    (SameValue(LastPoint.x,OtherControlPoint.x) and SameValue(LastPoint.y,OtherControlPoint.y)) and
                                    (SameValue(LastPoint.x,ControlPoint.x) and SameValue(LastPoint.y,ControlPoint.y))) then begin
       if DoSubdivideCurvesIntoLines then begin
        AddCubicBezierCurveAsSubdividedLinesToPathSegmentArray(Contour^,[LastPoint,ControlPoint,OtherControlPoint,Point]);
       end else begin
//      AddCubicBezierCurveAsSubdividedLinesToPathSegmentArray(Contour^,[LastPoint,ControlPoint,OtherControlPoint,Point]);
        AddCubicBezierCurveAsSubdividedQuadraticBezierCurvesToPathSegmentArray(Contour^,[LastPoint,ControlPoint,OtherControlPoint,Point]);
       end;
      end;
      LastPoint:=Point;
     end;
     TpvVectorPathCommandType.Close:begin
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
  SetLength(fShape.Contours,fShape.CountContours);
 end;
end;

procedure TpvSignedDistanceField2DGenerator.SplitPathSegmentIntoThreePartsInsideContour(var Contour:TpvSignedDistanceField2DPathContour;const BasePathSegmentIndex:TpvInt32);
var BasePathSegment:TpvSignedDistanceField2DPathSegment;
begin
 if (BasePathSegmentIndex>=0) and (BasePathSegmentIndex<Contour.CountPathSegments) then begin
  BasePathSegment:=Contour.PathSegments[BasePathSegmentIndex];
  if BasePathSegment.Type_ in [TpvSignedDistanceField2DPathSegmentType.Line,TpvSignedDistanceField2DPathSegmentType.QuadraticBezierCurve] then begin
   inc(Contour.CountPathSegments,2);
   if length(Contour.PathSegments)<=Contour.CountPathSegments then begin
    SetLength(Contour.PathSegments,Contour.CountPathSegments*2);
   end;
   Move(Contour.PathSegments[BasePathSegmentIndex+1],Contour.PathSegments[BasePathSegmentIndex+3],(Contour.CountPathSegments-(BasePathSegmentIndex+3))*SizeOf(TpvSignedDistanceField2DPathSegment));
   FillChar(Contour.PathSegments[BasePathSegmentIndex],SizeOf(TpvSignedDistanceField2DPathSegment)*3,#0);
  end else begin
   Assert(false);
  end;
  case BasePathSegment.Type_ of
   TpvSignedDistanceField2DPathSegmentType.Line:begin
    Contour.PathSegments[BasePathSegmentIndex+0].Type_:=TpvSignedDistanceField2DPathSegmentType.Line;
    Contour.PathSegments[BasePathSegmentIndex+0].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+0].Points[0]:=BasePathSegment.Points[0];
    Contour.PathSegments[BasePathSegmentIndex+0].Points[1]:=DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],1.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+1].Type_:=TpvSignedDistanceField2DPathSegmentType.Line;
    Contour.PathSegments[BasePathSegmentIndex+1].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+1].Points[0]:=Contour.PathSegments[BasePathSegmentIndex+0].Points[1];
    Contour.PathSegments[BasePathSegmentIndex+1].Points[1]:=DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],2.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+2].Type_:=TpvSignedDistanceField2DPathSegmentType.Line;
    Contour.PathSegments[BasePathSegmentIndex+2].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+2].Points[0]:=Contour.PathSegments[BasePathSegmentIndex+1].Points[1];
    Contour.PathSegments[BasePathSegmentIndex+2].Points[1]:=BasePathSegment.Points[1];
   end;
   TpvSignedDistanceField2DPathSegmentType.QuadraticBezierCurve:begin
    Contour.PathSegments[BasePathSegmentIndex+0].Type_:=TpvSignedDistanceField2DPathSegmentType.QuadraticBezierCurve;
    Contour.PathSegments[BasePathSegmentIndex+0].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+0].Points[0]:=BasePathSegment.Points[0];
    Contour.PathSegments[BasePathSegmentIndex+0].Points[1]:=DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],1.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+0].Points[2]:=DoublePrecisionPointLerp(DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],1.0/3.0),DoublePrecisionPointLerp(BasePathSegment.Points[1],BasePathSegment.Points[2],1.0/3.0),1.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+1].Type_:=TpvSignedDistanceField2DPathSegmentType.QuadraticBezierCurve;
    Contour.PathSegments[BasePathSegmentIndex+1].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+1].Points[0]:=Contour.PathSegments[BasePathSegmentIndex+0].Points[2];
    Contour.PathSegments[BasePathSegmentIndex+1].Points[1]:=DoublePrecisionPointLerp(DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],5.0/9.0),DoublePrecisionPointLerp(BasePathSegment.Points[1],BasePathSegment.Points[2],4.0/9.0),0.5);
    Contour.PathSegments[BasePathSegmentIndex+1].Points[2]:=DoublePrecisionPointLerp(DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],2.0/3.0),DoublePrecisionPointLerp(BasePathSegment.Points[1],BasePathSegment.Points[2],2.0/3.0),2.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+2].Type_:=TpvSignedDistanceField2DPathSegmentType.QuadraticBezierCurve;
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

procedure TpvSignedDistanceField2DGenerator.SplitPathSegmentIntoThreePartsToContour(var Contour:TpvSignedDistanceField2DPathContour;const BasePathSegmentIndex:TpvInt32;const BasePathSegment:TpvSignedDistanceField2DPathSegment);
begin
 if (BasePathSegmentIndex>=0) and (BasePathSegmentIndex<Contour.CountPathSegments) then begin
  case BasePathSegment.Type_ of
   TpvSignedDistanceField2DPathSegmentType.Line:begin
    Contour.PathSegments[BasePathSegmentIndex+0].Type_:=TpvSignedDistanceField2DPathSegmentType.Line;
    Contour.PathSegments[BasePathSegmentIndex+0].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+0].Points[0]:=BasePathSegment.Points[0];
    Contour.PathSegments[BasePathSegmentIndex+0].Points[1]:=DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],1.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+1].Type_:=TpvSignedDistanceField2DPathSegmentType.Line;
    Contour.PathSegments[BasePathSegmentIndex+1].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+1].Points[0]:=Contour.PathSegments[BasePathSegmentIndex+0].Points[1];
    Contour.PathSegments[BasePathSegmentIndex+1].Points[1]:=DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],2.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+2].Type_:=TpvSignedDistanceField2DPathSegmentType.Line;
    Contour.PathSegments[BasePathSegmentIndex+2].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+2].Points[0]:=Contour.PathSegments[BasePathSegmentIndex+1].Points[1];
    Contour.PathSegments[BasePathSegmentIndex+2].Points[1]:=BasePathSegment.Points[1];
   end;
   TpvSignedDistanceField2DPathSegmentType.QuadraticBezierCurve:begin
    Contour.PathSegments[BasePathSegmentIndex+0].Type_:=TpvSignedDistanceField2DPathSegmentType.QuadraticBezierCurve;
    Contour.PathSegments[BasePathSegmentIndex+0].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+0].Points[0]:=BasePathSegment.Points[0];
    Contour.PathSegments[BasePathSegmentIndex+0].Points[1]:=DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],1.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+0].Points[2]:=DoublePrecisionPointLerp(DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],1.0/3.0),DoublePrecisionPointLerp(BasePathSegment.Points[1],BasePathSegment.Points[2],1.0/3.0),1.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+1].Type_:=TpvSignedDistanceField2DPathSegmentType.QuadraticBezierCurve;
    Contour.PathSegments[BasePathSegmentIndex+1].Color:=BasePathSegment.Color;
    Contour.PathSegments[BasePathSegmentIndex+1].Points[0]:=Contour.PathSegments[BasePathSegmentIndex+0].Points[2];
    Contour.PathSegments[BasePathSegmentIndex+1].Points[1]:=DoublePrecisionPointLerp(DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],5.0/9.0),DoublePrecisionPointLerp(BasePathSegment.Points[1],BasePathSegment.Points[2],4.0/9.0),0.5);
    Contour.PathSegments[BasePathSegmentIndex+1].Points[2]:=DoublePrecisionPointLerp(DoublePrecisionPointLerp(BasePathSegment.Points[0],BasePathSegment.Points[1],2.0/3.0),DoublePrecisionPointLerp(BasePathSegment.Points[1],BasePathSegment.Points[2],2.0/3.0),2.0/3.0);
    Contour.PathSegments[BasePathSegmentIndex+2].Type_:=TpvSignedDistanceField2DPathSegmentType.QuadraticBezierCurve;
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

procedure TpvSignedDistanceField2DGenerator.NormalizeShape;
var ContourIndex:TpvInt32;
    Contour:PpvSignedDistanceField2DPathContour;
begin
 for ContourIndex:=0 to fShape.CountContours-1 do begin
  Contour:=@fShape.Contours[ContourIndex];
  if Contour^.CountPathSegments=1 then begin
   try
    SplitPathSegmentIntoThreePartsInsideContour(Contour^,0);
   finally
    SetLength(Contour^.PathSegments,Contour^.CountPathSegments);
   end;
  end;
 end;
end;

procedure TpvSignedDistanceField2DGenerator.PathSegmentColorizeShape;
const AngleThreshold=3.0;
      EdgeThreshold=1.00000001;
type PCorner=^TCorner;
     TCorner=TpvInt32;
     TCorners=array of TCorner;
var ContourIndex,PathSegmentIndex,CountCorners,CornerIndex,SplineIndex,StartIndex,
    OtherPathSegmentIndex:TpvInt32;
    Seed:TpvUInt64;
    Contour:PpvSignedDistanceField2DPathContour;
    PathSegment:PpvSignedDistanceField2DPathSegment;
    Corners:TCorners;
    CurrentDirection,PreviousDirection,a,b:TpvSignedDistanceField2DDoublePrecisionPoint;
    CrossThreshold:TpvDouble;
    Color,InitialColor:TpvSignedDistanceField2DPathSegmentColor;
    Colors:array[0..2] of TpvSignedDistanceField2DPathSegmentColor;
    PathSegments:TpvSignedDistanceField2DPathSegments;
 procedure SwitchColor(var Color:TpvSignedDistanceField2DPathSegmentColor;const BannedColor:TpvSignedDistanceField2DPathSegmentColor=TpvSignedDistanceField2DPathSegmentColor.Black);
 const StartColors:array[0..2] of TpvSignedDistanceField2DPathSegmentColor=(TpvSignedDistanceField2DPathSegmentColor.Cyan,TpvSignedDistanceField2DPathSegmentColor.Magenta,TpvSignedDistanceField2DPathSegmentColor.Yellow);
 var CombinedColor:TpvSignedDistanceField2DPathSegmentColor;
     Shifted:TpvUInt64;
 begin
  CombinedColor:=TpvSignedDistanceField2DPathSegmentColor(TpvInt32(TpvInt32(Color) and TpvInt32(BannedColor)));
  if CombinedColor in [TpvSignedDistanceField2DPathSegmentColor.Red,TpvSignedDistanceField2DPathSegmentColor.Green,TpvSignedDistanceField2DPathSegmentColor.Blue] then begin
   Color:=TpvSignedDistanceField2DPathSegmentColor(TpvInt32(TpvInt32(CombinedColor) xor TpvInt32(TpvSignedDistanceField2DPathSegmentColor(TpvSignedDistanceField2DPathSegmentColor.White))));
  end else if CombinedColor in [TpvSignedDistanceField2DPathSegmentColor.Black,TpvSignedDistanceField2DPathSegmentColor.White] then begin
   Color:=StartColors[Seed mod 3];
   Seed:=Seed div 3;
  end else begin
   Shifted:=TpvInt32(Color) shl (1+(Seed and 1));
   Color:=TpvSignedDistanceField2DPathSegmentColor(TpvInt32((Shifted or (Shifted shr 3)) and TpvInt32(TpvSignedDistanceField2DPathSegmentColor(TpvSignedDistanceField2DPathSegmentColor.White))));
   Seed:=Seed shr 1;
  end;
 end;
begin

 Seed:=$7ffffffffffffff;

 CrossThreshold:=sin(AngleThreshold);

 for ContourIndex:=0 to fShape.CountContours-1 do begin

  Contour:=@fShape.Contours[ContourIndex];
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
       PathSegment^.Color:=TpvSignedDistanceField2DPathSegmentColor.White;
      end;
     end;
     1:begin
      Colors[0]:=TpvSignedDistanceField2DPathSegmentColor.White;
      Colors[1]:=TpvSignedDistanceField2DPathSegmentColor.White;
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
      Color:=TpvSignedDistanceField2DPathSegmentColor.White;
      SwitchColor(Color);
      InitialColor:=Color;
      for PathSegmentIndex:=0 to Contour^.CountPathSegments-1 do begin
       OtherPathSegmentIndex:=StartIndex+PathSegmentIndex;
       if OtherPathSegmentIndex>=Contour^.CountPathSegments then begin
        dec(OtherPathSegmentIndex,Contour^.CountPathSegments);
       end;
       if ((SplineIndex+1)<CountCorners) and (Corners[SplineIndex+1]=OtherPathSegmentIndex) then begin
        inc(SplineIndex);
        SwitchColor(Color,TpvSignedDistanceField2DPathSegmentColor(TpvInt32(IfThen(SplineIndex=(CountCorners-1),TpvInt32(InitialColor),TpvInt32(TpvSignedDistanceField2DPathSegmentColor(TpvSignedDistanceField2DPathSegmentColor.Black))))));
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

function TpvSignedDistanceField2DGenerator.GetLineNonClippedTime(const p,p0,p1:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
var pAP,pAB:TpvSignedDistanceField2DDoublePrecisionPoint;
begin
 pAP.x:=p.x-p0.x;
 pAP.y:=p.y-p0.y;
 pAB.x:=p1.x-p0.x;
 pAB.y:=p1.y-p0.y;
 result:=((pAP.x*pAB.x)+(pAP.y*pAB.y))/(sqr(pAB.x)+sqr(pAB.y));
end;

function TpvSignedDistanceField2DGenerator.GetQuadraticBezierCurveNonClippedTime(const p,p0,p1,p2:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
var b0,b1,b2,d21,d10,d20,gf,pp,d0p:TpvSignedDistanceField2DDoublePrecisionPoint;
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

function TpvSignedDistanceField2DGenerator.GetNonClampedSignedLineDistance(const p,p0,p1:TpvSignedDistanceField2DDoublePrecisionPoint):TpvDouble;
begin
 result:=((p.x*(p0.y-p1.y))+(p0.x*(p1.y-p.y))+(p1.x*(p.y-p0.y)))/sqrt(sqr(p1.x-p0.x)+sqr(p1.y-p0.y));
end;

procedure TpvSignedDistanceField2DGenerator.CalculateDistanceFieldDataLineRange(const FromY,ToY:TpvInt32);
var ContourIndex,PathSegmentIndex,x0,y0,x1,y1,x,y,PixelIndex,Dilation,DeltaWindingScore:TpvInt32;
    Contour:PpvSignedDistanceField2DPathContour;
    PathSegment:PpvSignedDistanceField2DPathSegment;
    PathSegmentBoundingBox:TpvSignedDistanceField2DBoundingBox;
    PreviousPathSegmentSide,PathSegmentSide:TpvSignedDistanceField2DPathSegmentSide;
    RowData:TpvSignedDistanceField2DRowData;
    DistanceFieldDataItem:PpvSignedDistanceField2DDataItem;
    PointLeft,PointRight,Point,p0,p1,Direction,OriginPointDifference:TpvSignedDistanceField2DDoublePrecisionPoint;
    pX,pY,CurrentSquaredDistance,CurrentSquaredPseudoDistance,Time,Value,oX,oY:TpvDouble;
begin
 GetOffset(oX,oY);
 RowData.QuadraticXDirection:=0;
 for ContourIndex:=0 to fShape.CountContours-1 do begin
  Contour:=@fShape.Contours[ContourIndex];
  for PathSegmentIndex:=0 to Contour^.CountPathSegments-1 do begin
   PathSegment:=@Contour^.PathSegments[PathSegmentIndex];
   PathSegmentBoundingBox.Min.x:=PathSegment.BoundingBox.Min.x-DistanceField2DPadValue;
   PathSegmentBoundingBox.Min.y:=PathSegment.BoundingBox.Min.y-DistanceField2DPadValue;
   PathSegmentBoundingBox.Max.x:=PathSegment.BoundingBox.Max.x+DistanceField2DPadValue;
   PathSegmentBoundingBox.Max.y:=PathSegment.BoundingBox.Max.y+DistanceField2DPadValue;
   x0:=Clamp(Trunc(Floor(PathSegmentBoundingBox.Min.x)),0,fDistanceField.Width-1);
   y0:=Clamp(Trunc(Floor(PathSegmentBoundingBox.Min.y)),0,fDistanceField.Height-1);
   x1:=Clamp(Trunc(Ceil(PathSegmentBoundingBox.Max.x)),0,fDistanceField.Width-1);
   y1:=Clamp(Trunc(Ceil(PathSegmentBoundingBox.Max.y)),0,fDistanceField.Height-1);
{  x0:=0;
   y0:=0;
   x1:=DistanceField.Width-1;
   y1:=DistanceField.Height-1;}
   for y:=Max(FromY,y0) to Min(ToY,y1) do begin
    PreviousPathSegmentSide:=TpvSignedDistanceField2DPathSegmentSide.None;
    pY:=y+oY+0.5;
    PointLeft.x:=x0;
    PointLeft.y:=pY;
    PointRight.x:=x1;
    PointRight.y:=pY;
    if BetweenClosedOpen(pY,PathSegment.BoundingBox.Min.y,PathSegment.BoundingBox.Max.y) then begin
     PrecomputationForRow(RowData,PathSegment^,PointLeft,PointRight);
    end;
    for x:=x0 to x1 do begin
     PixelIndex:=(y*fDistanceField.Width)+x;
     pX:=x+oX+0.5;
     Point.x:=pX;
     Point.y:=pY;
     DistanceFieldDataItem:=@fDistanceFieldData[PixelIndex];
     Dilation:=Clamp(Floor(sqrt(Max(1,DistanceFieldDataItem^.SquaredDistance))+0.5),1,DistanceField2DPadValue);
     PathSegmentBoundingBox.Min.x:=Floor(PathSegment.BoundingBox.Min.x)-DistanceField2DPadValue;
     PathSegmentBoundingBox.Min.y:=Floor(PathSegment.BoundingBox.Min.y)-DistanceField2DPadValue;
     PathSegmentBoundingBox.Max.x:=Ceil(PathSegment.BoundingBox.Max.x)+DistanceField2DPadValue;
     PathSegmentBoundingBox.Max.y:=Ceil(PathSegment.BoundingBox.Max.y)+DistanceField2DPadValue;
     if (Dilation<>DistanceField2DPadValue) and not
        (((x>=PathSegmentBoundingBox.Min.x) and (x<=PathSegmentBoundingBox.Max.x)) and
         ((y>=PathSegmentBoundingBox.Min.y) and (y<=PathSegmentBoundingBox.Max.y))) then begin
      continue;
     end else begin
      PathSegmentSide:=TpvSignedDistanceField2DPathSegmentSide.None;
      CurrentSquaredDistance:=DistanceToPathSegment(Point,PathSegment^,RowData,PathSegmentSide);
      CurrentSquaredPseudoDistance:=CurrentSquaredDistance;
(**)  if fMultiChannelMode=TMultiChannelMode.MSDFGENCompatible then begin
       case PathSegment^.Type_ of
        TpvSignedDistanceField2DPathSegmentType.Line:begin
         Time:=GetLineNonClippedTime(Point,PathSegment^.Points[0],PathSegment^.Points[1]);
        end;
        TpvSignedDistanceField2DPathSegmentType.QuadraticBezierCurve:begin
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
      if (PreviousPathSegmentSide=TpvSignedDistanceField2DPathSegmentSide.Left) and (PathSegmentSide=TpvSignedDistanceField2DPathSegmentSide.Right) then begin
       DeltaWindingScore:=-1;
      end else if (PreviousPathSegmentSide=TpvSignedDistanceField2DPathSegmentSide.Right) and (PathSegmentSide=TpvSignedDistanceField2DPathSegmentSide.Left) then begin
       DeltaWindingScore:=1;
      end else begin
       DeltaWindingScore:=0;
      end;
      PreviousPathSegmentSide:=PathSegmentSide;
      if CurrentSquaredDistance<DistanceFieldDataItem^.SquaredDistance then begin
       DistanceFieldDataItem^.SquaredDistance:=CurrentSquaredDistance;
      end;
      if fMultiChannelMode=TMultiChannelMode.MSDFGENCompatible then begin
       if (((TpvInt32(PathSegment^.Color) and TpvInt32(TpvSignedDistanceField2DPathSegmentColor(TpvSignedDistanceField2DPathSegmentColor.Red)))<>0)) and
          (CurrentSquaredDistance<DistanceFieldDataItem^.SquaredDistanceR) then begin
        DistanceFieldDataItem^.SquaredDistanceR:=CurrentSquaredDistance;
        DistanceFieldDataItem^.PseudoSquaredDistanceR:=CurrentSquaredPseudoDistance;
       end;
       if (((TpvInt32(PathSegment^.Color) and TpvInt32(TpvSignedDistanceField2DPathSegmentColor(TpvSignedDistanceField2DPathSegmentColor.Green)))<>0)) and
          (CurrentSquaredDistance<DistanceFieldDataItem^.SquaredDistanceG) then begin
        DistanceFieldDataItem^.SquaredDistanceG:=CurrentSquaredDistance;
        DistanceFieldDataItem^.PseudoSquaredDistanceG:=CurrentSquaredPseudoDistance;
       end;
       if (((TpvInt32(PathSegment^.Color) and TpvInt32(TpvSignedDistanceField2DPathSegmentColor(TpvSignedDistanceField2DPathSegmentColor.Blue)))<>0)) and
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

procedure TpvSignedDistanceField2DGenerator.CalculateDistanceFieldDataLineRangeParallelForJobFunction(const Job:PPasMPJob;const ThreadIndex:TPasMPInt32;const Data:TpvPointer;const FromIndex,ToIndex:TPasMPNativeInt);
begin
 CalculateDistanceFieldDataLineRange(FromIndex,ToIndex);
end;

function TpvSignedDistanceField2DGenerator.PackDistanceFieldValue(Distance:TpvDouble):TpvUInt8;
begin
 result:=Clamp(Round((Distance*(128.0/DistanceField2DMagnitudeValue))+128.0),0,255);
end;

function TpvSignedDistanceField2DGenerator.PackPseudoDistanceFieldValue(Distance:TpvDouble):TpvUInt8;
begin
 result:=Clamp(Round((Distance*(128.0/DistanceField2DMagnitudeValue))+128.0),0,255);
end;

procedure TpvSignedDistanceField2DGenerator.ConvertToPointInPolygonPathSegments;
var ContourIndex,PathSegmentIndex,CountPathSegments:TpvInt32;
    Contour:PpvSignedDistanceField2DPathContour;
    PathSegment:PpvSignedDistanceField2DPathSegment;
    StartPoint,LastPoint:TpvSignedDistanceField2DDoublePrecisionPoint;
 procedure AddPathSegment(const p0,p1:TpvSignedDistanceField2DDoublePrecisionPoint);
 var Index:TpvInt32;
     PointInPolygonPathSegment:PpvSignedDistanceField2DPointInPolygonPathSegment;
 begin
  if not (SameValue(p0.x,p1.x) and SameValue(p0.y,p1.y)) then begin
   Index:=CountPathSegments;
   inc(CountPathSegments);
   if length(fPointInPolygonPathSegments)<CountPathSegments then begin
    SetLength(fPointInPolygonPathSegments,CountPathSegments*2);
   end;
   PointInPolygonPathSegment:=@fPointInPolygonPathSegments[Index];
   PointInPolygonPathSegment^.Points[0]:=p0;
   PointInPolygonPathSegment^.Points[1]:=p1;
  end;
 end;
 procedure AddQuadraticBezierCurveAsSubdividedLinesToPathSegmentArray(const p0,p1,p2:TpvSignedDistanceField2DDoublePrecisionPoint);
 var LastPoint:TpvSignedDistanceField2DDoublePrecisionPoint;
  procedure LineToPointAt(const Point:TpvSignedDistanceField2DDoublePrecisionPoint);
  begin
   AddPathSegment(LastPoint,Point);
   LastPoint:=Point;
  end;
  procedure Recursive(const x1,y1,x2,y2,x3,y3:TpvDouble;const Level:TpvInt32);
  var x12,y12,x23,y23,x123,y123,dx,dy:TpvDouble;
      Point:TpvSignedDistanceField2DDoublePrecisionPoint;
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
 fPointInPolygonPathSegments:=nil;
 CountPathSegments:=0;
 try
  for ContourIndex:=0 to fShape.CountContours-1 do begin
   Contour:=@fShape.Contours[ContourIndex];
   if Contour^.CountPathSegments>0 then begin
    StartPoint.x:=0.0;
    StartPoint.y:=0.0;
    LastPoint.x:=0.0;
    LastPoint.y:=0.0;
    for PathSegmentIndex:=0 to Contour^.CountPathSegments-1 do begin
     PathSegment:=@Contour^.PathSegments[PathSegmentIndex];
     case PathSegment^.Type_ of
      TpvSignedDistanceField2DPathSegmentType.Line:begin
       if PathSegmentIndex=0 then begin
        StartPoint:=PathSegment^.Points[0];
       end;
       LastPoint:=PathSegment^.Points[1];
       AddPathSegment(PathSegment^.Points[0],PathSegment^.Points[1]);
      end;
      TpvSignedDistanceField2DPathSegmentType.QuadraticBezierCurve:begin
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
  SetLength(fPointInPolygonPathSegments,CountPathSegments);
 end;
end;

function TpvSignedDistanceField2DGenerator.GetWindingNumberAtPointInPolygon(const Point:TpvSignedDistanceField2DDoublePrecisionPoint):TpvInt32;
var Index,CaseIndex:TpvInt32;
    PointInPolygonPathSegment:PpvSignedDistanceField2DPointInPolygonPathSegment;
    x0,y0,x1,y1:TpvDouble;
begin
 result:=0;
 for Index:=0 to length(fPointInPolygonPathSegments)-1 do begin
  PointInPolygonPathSegment:=@fPointInPolygonPathSegments[Index];
  if not (SameValue(PointInPolygonPathSegment^.Points[0].x,PointInPolygonPathSegment^.Points[1].x) and
          SameValue(PointInPolygonPathSegment^.Points[0].y,PointInPolygonPathSegment^.Points[1].y)) then begin
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
end;

function TpvSignedDistanceField2DGenerator.GenerateDistanceFieldPicture(const DistanceFieldData:TpvSignedDistanceField2DData;const Width,Height,TryIteration:TpvInt32):boolean;
var x,y,PixelIndex,DistanceFieldSign,WindingNumber,Value:TpvInt32;
    DistanceFieldDataItem:PpvSignedDistanceField2DDataItem;
    DistanceFieldPixel:PpvSignedDistanceField2DPixel;
    p:TpvSignedDistanceField2DDoublePrecisionPoint;
    oX,oY:TpvDouble;
begin

 result:=true;

 GetOffset(oX,oY);

 PixelIndex:=0;
 for y:=0 to Height-1 do begin
  WindingNumber:=0;
  for x:=0 to Width-1 do begin
   DistanceFieldDataItem:=@DistanceFieldData[PixelIndex];
   if TryIteration=2 then begin
    p.x:=x+oX+0.5;
    p.y:=y+oY+0.5;
    WindingNumber:=GetWindingNumberAtPointInPolygon(p);
   end else begin
    inc(WindingNumber,DistanceFieldDataItem^.DeltaWindingScore);
    if (x=(Width-1)) and (WindingNumber<>0) then begin
     result:=false;
     break;
    end;
   end;
   case fVectorPath.FillRule of
    TpvVectorPathFillRule.NonZero:begin
     if WindingNumber<>0 then begin
      DistanceFieldSign:=1;
     end else begin
      DistanceFieldSign:=-1;
     end;
    end;
    else {TpvVectorPathFillRule.EvenOdd:}begin
     if (WindingNumber and 1)<>0 then begin
      DistanceFieldSign:=1;
     end else begin
      DistanceFieldSign:=-1;
     end;
    end;
   end;
   DistanceFieldPixel:=@fDistanceField^.Pixels[PixelIndex];
   case fMultiChannelMode of
    TMultiChannelMode.MSDFGENCompatible:begin
     DistanceFieldPixel^.r:=PackPseudoDistanceFieldValue(sqrt(DistanceFieldDataItem^.PseudoSquaredDistanceR)*DistanceFieldSign);
     DistanceFieldPixel^.g:=PackPseudoDistanceFieldValue(sqrt(DistanceFieldDataItem^.PseudoSquaredDistanceG)*DistanceFieldSign);
     DistanceFieldPixel^.b:=PackPseudoDistanceFieldValue(sqrt(DistanceFieldDataItem^.PseudoSquaredDistanceB)*DistanceFieldSign);
     DistanceFieldPixel^.a:=PackDistanceFieldValue(sqrt(DistanceFieldDataItem^.SquaredDistance)*DistanceFieldSign);
    end;
    TMultiChannelMode.Gradients:begin
     case fColorChannelIndex of
      1:begin
       DistanceFieldPixel^.g:=PackDistanceFieldValue((sqrt(DistanceFieldDataItem^.SquaredDistance)*DistanceFieldSign)-DistanceFieldDataItem^.Distance);
      end;
      2:begin
       DistanceFieldPixel^.b:=PackDistanceFieldValue((sqrt(DistanceFieldDataItem^.SquaredDistance)*DistanceFieldSign)-DistanceFieldDataItem^.Distance);
      end;
      else {0:}begin
       DistanceFieldDataItem^.Distance:=sqrt(DistanceFieldDataItem^.SquaredDistance)*DistanceFieldSign;
       Value:=PackDistanceFieldValue(DistanceFieldDataItem^.Distance);
       DistanceFieldPixel^.r:=Value;
       DistanceFieldPixel^.g:=Value;
       DistanceFieldPixel^.b:=Value;
       DistanceFieldPixel^.a:=Value;
      end;
     end;
    end;
    TMultiChannelMode.Multisampling:begin
     Value:=PackDistanceFieldValue(sqrt(DistanceFieldDataItem^.SquaredDistance)*DistanceFieldSign);
     case fColorChannelIndex of
      0:begin
       DistanceFieldPixel^.r:=Value;
      end;
      1:begin
       DistanceFieldPixel^.g:=Value;
      end;
      2:begin
       DistanceFieldPixel^.b:=Value;
      end;
      else {3:}begin
       DistanceFieldPixel^.a:=Value;
      end;
     end;
    end;
    else begin
     Value:=PackDistanceFieldValue(sqrt(DistanceFieldDataItem^.SquaredDistance)*DistanceFieldSign);
     DistanceFieldPixel^.r:=Value;
     DistanceFieldPixel^.g:=Value;
     DistanceFieldPixel^.b:=Value;
     DistanceFieldPixel^.a:=Value;
    end;
   end;
   inc(PixelIndex);
  end;
  if not result then begin
   break;
  end;
 end;

end;

procedure TpvSignedDistanceField2DGenerator.Execute(var aDistanceField:TpvSignedDistanceField2D;const aVectorPath:TpvVectorPath;const aScale:TpvDouble;const aOffsetX:TpvDouble;const aOffsetY:TpvDouble;const aMultiChannelMode:TMultiChannelMode);
var TryIteration,ColorChannelIndex,CountColorChannels:TpvInt32;
    PasMPInstance:TPasMP;
begin

 PasMPInstance:=TPasMP.GetGlobalInstance;

 fDistanceField:=@aDistanceField;

 fVectorPath:=aVectorPath;

 fScale:=aScale;

 fOffsetX:=aOffsetX;

 fOffsetY:=aOffsetY;

 fMultiChannelMode:=aMultiChannelMode;

 case aMultiChannelMode of
  TMultiChannelMode.Gradients:begin
   CountColorChannels:=3;
  end;
  TMultiChannelMode.Multisampling:begin
   CountColorChannels:=4;
  end;
  else begin
   CountColorChannels:=1;
  end;
 end;

 fDistanceFieldData:=nil;
 try

  SetLength(fDistanceFieldData,fDistanceField.Width*fDistanceField.Height);

  try

   for ColorChannelIndex:=0 to CountColorChannels-1 do begin

    Initialize(fShape);
    try

     fColorChannelIndex:=ColorChannelIndex;

     fPointInPolygonPathSegments:=nil;
     try

      for TryIteration:=0 to 2 do begin
       case TryIteration of
        0,1:begin
         InitializeDistances;
         ConvertShape(TryIteration in [1,2]);
         if fMultiChannelMode=TMultiChannelMode.MSDFGENCompatible then begin
          NormalizeShape;
          PathSegmentColorizeShape;
          NormalizeShape;
         end;
        end;
        else {2:}begin
         InitializeDistances;
         ConvertShape(true);
         ConvertToPointInPolygonPathSegments;
        end;
       end;
       PasMPInstance.Invoke(PasMPInstance.ParallelFor(nil,0,fDistanceField.Height-1,CalculateDistanceFieldDataLineRangeParallelForJobFunction,1,10,nil,0));
       if GenerateDistanceFieldPicture(fDistanceFieldData,fDistanceField.Width,fDistanceField.Height,TryIteration) then begin
        break;
       end else begin
        // Try it again, after all quadratic bezier curves were subdivided into lines at the next try iteration
       end;
      end;

     finally
      fPointInPolygonPathSegments:=nil;
     end;

    finally
     Finalize(fShape);
    end;

   end;

  finally
   fDistanceField:=nil;
   fVectorPath:=nil;
  end;

 finally
  fDistanceFieldData:=nil;
 end;

end;

class procedure TpvSignedDistanceField2DGenerator.Generate(var aDistanceField:TpvSignedDistanceField2D;const aVectorPath:TpvVectorPath;const aScale:TpvDouble;const aOffsetX:TpvDouble;const aOffsetY:TpvDouble;const aMultiChannelMode:TMultiChannelMode);
var Generator:TpvSignedDistanceField2DGenerator;
begin
 Generator:=TpvSignedDistanceField2DGenerator.Create;
 try
  Generator.Execute(aDistanceField,aVectorPath,aScale,aOffsetX,aOffsetY,aMultiChannelMode);
 finally
  Generator.Free;
 end;
end;

end.
