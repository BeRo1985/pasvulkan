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
unit PasVulkan.VectorPath;
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
     Generics.Collections,
     Vulkan,
     PasDblStrUtils,
     PasMP,
     PasVulkan.Types,
     PasVulkan.Collections,
     PasVulkan.Math;

type PpvVectorPathCommandType=^TpvVectorPathCommandType;
     TpvVectorPathCommandType=
      (
       MoveTo,
       LineTo,
       QuadraticCurveTo,
       CubicCurveTo,
       Close
      );

     { TpvVectorPathVector }

     TpvVectorPathVector=record
      public
       x:TpvDouble;
       y:TpvDouble;
       constructor Create(const aValue:TpvDouble); overload;
       constructor Create(const aX,aY:TpvDouble); overload;
       function Length:TpvDouble; {$ifdef CANINLINE}inline;{$endif}
       function LengthSquared:TpvDouble; {$ifdef CANINLINE}inline;{$endif}
       function Distance(const b:TpvVectorPathVector):TpvDouble; {$ifdef CANINLINE}inline;{$endif}
       function DistanceSquared(const b:TpvVectorPathVector):TpvDouble; {$ifdef CANINLINE}inline;{$endif}
       function Direction:TpvDouble; {$ifdef CANINLINE}inline;{$endif}
       function Normalize:TpvVectorPathVector; {$ifdef CANINLINE}inline;{$endif}
       function Minimum(const aRight:TpvVectorPathVector):TpvVectorPathVector;
       function Maximum(const aRight:TpvVectorPathVector):TpvVectorPathVector;
       function Dot(const aRight:TpvVectorPathVector):TpvDouble; {$ifdef CANINLINE}inline;{$endif}
       function Cross(const aRight:TpvVectorPathVector):TpvDouble; {$ifdef CANINLINE}inline;{$endif}
       function OrthoNormal:TpvVectorPathVector; {$ifdef CANINLINE}inline;{$endif}
       function Lerp(const b:TpvVectorPathVector;const t:TpvDouble):TpvVectorPathVector;
       function ClampedLerp(const b:TpvVectorPathVector;const t:TpvDouble):TpvVectorPathVector;
       class function IsLeft(const a,b,c:TpvVectorPathVector):TpvDouble; static;
       class operator Equal(const a,b:TpvVectorPathVector):boolean; {$ifdef CANINLINE}inline;{$endif}
       class operator NotEqual(const a,b:TpvVectorPathVector):boolean; {$ifdef CANINLINE}inline;{$endif}
       class operator Add(const a,b:TpvVectorPathVector):TpvVectorPathVector; {$ifdef CANINLINE}inline;{$endif}
       class operator Subtract(const a,b:TpvVectorPathVector):TpvVectorPathVector; {$ifdef CANINLINE}inline;{$endif}
       class operator Multiply(const a,b:TpvVectorPathVector):TpvVectorPathVector; overload; {$ifdef CANINLINE}inline;{$endif}
       class operator Multiply(const a:TpvVectorPathVector;const b:TpvDouble):TpvVectorPathVector; overload; {$ifdef CANINLINE}inline;{$endif}
       class operator Multiply(const a:TpvDouble;const b:TpvVectorPathVector):TpvVectorPathVector; overload; {$ifdef CANINLINE}inline;{$endif}
       class operator Divide(const a,b:TpvVectorPathVector):TpvVectorPathVector; overload; {$ifdef CANINLINE}inline;{$endif}
       class operator Divide(const a:TpvVectorPathVector;const b:TpvDouble):TpvVectorPathVector; overload; {$ifdef CANINLINE}inline;{$endif}
       class operator Negative(const a:TpvVectorPathVector):TpvVectorPathVector; {$ifdef CANINLINE}inline;{$endif}
       class operator Positive(const a:TpvVectorPathVector):TpvVectorPathVector; {$ifdef CANINLINE}inline;{$endif}
     end;

     PpvVectorPathVector=^TpvVectorPathVector;

     TpvVectorPathVectors=array of TpvVectorPathVector;

     TpvVectorPathRawVectors=array[0..65535] of TpvVectorPathVector;

     PpvVectorPathRawVectors=^TpvVectorPathRawVectors;

     { TpvVectorPathBoundingBox }

     TpvVectorPathBoundingBox=record
      public
       constructor Create(const aMin,aMax:TpvVectorPathVector);
      public
       case boolean of
        false:(
         Min:TpvVectorPathVector;
         Max:TpvVectorPathVector;
        );
        true:(
         MinMax:array[0..1] of TpvVectorPathVector;
        );
     end;

     PpvVectorPathBoundingBox=^TpvVectorPathBoundingBox;

     { TpvVectorPathVectorCommand }

     TpvVectorPathCommand=class
      private
       fCommandType:TpvVectorPathCommandType;
       fX0:TpvDouble;
       fY0:TpvDouble;
       fX1:TpvDouble;
       fY1:TpvDouble;
       fX2:TpvDouble;
       fY2:TpvDouble;
      public
       constructor Create(const aCommandType:TpvVectorPathCommandType;
                          const aX0:TpvDouble=0.0;
                          const aY0:TpvDouble=0.0;
                          const aX1:TpvDouble=0.0;
                          const aY1:TpvDouble=0.0;
                          const aX2:TpvDouble=0.0;
                          const aY2:TpvDouble=0.0); reintroduce;
      published
       property CommandType:TpvVectorPathCommandType read fCommandType write fCommandType;
       property x0:TpvDouble read fX0 write fX0;
       property y0:TpvDouble read fY0 write fY0;
       property x1:TpvDouble read fX1 write fX1;
       property y1:TpvDouble read fY1 write fY1;
       property x2:TpvDouble read fX2 write fX2;
       property y2:TpvDouble read fY2 write fY2;
     end;

     TpvVectorPathCommandList=class(TObjectList<TpvVectorPathCommand>);

     TpvVectorPathFillRule=
      (
       NonZero,
       EvenOdd
      );

     PpvVectorPathFillRule=^TpvVectorPathFillRule;

     TpvVectorPathSegmentType=
      (
       Line,
       QuadraticCurve,
       CubicCurve
      );

     PpvVectorPathSegmentType=^TpvVectorPathSegmentType;

     { TpvVectorPathSegment }

     TpvVectorPathSegment=class
      private
       fType:TpvVectorPathSegmentType;
      public
       constructor Create; reintroduce; overload; virtual;
       destructor Destroy; override;
       procedure Assign(const aSegment:TpvVectorPathSegment); virtual;
       function Clone:TpvVectorPathSegment; virtual;
       function GetBoundingBox:TpvVectorPathBoundingBox; virtual;
      published
       property Type_:TpvVectorPathSegmentType read fType;
     end;

     { TpvVectorPathSegmentLine }

     TpvVectorPathSegmentLine=class(TpvVectorPathSegment)
      public
       Points:array[0..1] of TpvVectorPathVector;
      public
       constructor Create; overload; override;
       constructor Create(const aP0,aP1:TpvVectorPathVector); overload;
       procedure Assign(const aSegment:TpvVectorPathSegment); override;
       function Clone:TpvVectorPathSegment; override;
       function GetBoundingBox:TpvVectorPathBoundingBox; override;
     end;

     { TpvVectorPathSegmentQuadraticCurve }

     TpvVectorPathSegmentQuadraticCurve=class(TpvVectorPathSegment)
      public
       Points:array[0..2] of TpvVectorPathVector;
      public
       constructor Create; overload; override;
       constructor Create(const aP0,aP1,aP2:TpvVectorPathVector); overload;
       procedure Assign(const aSegment:TpvVectorPathSegment); override;
       function Clone:TpvVectorPathSegment; override;
       function GetBoundingBox:TpvVectorPathBoundingBox; override;
     end;

     { TpvVectorPathSegmentCubicCurve }

     TpvVectorPathSegmentCubicCurve=class(TpvVectorPathSegment)
      public
       Points:array[0..3] of TpvVectorPathVector;
      public
       constructor Create; overload; override;
       constructor Create(const aP0,aP1,aP2,aP3:TpvVectorPathVector); overload;
       procedure Assign(const aSegment:TpvVectorPathSegment); override;
       function Clone:TpvVectorPathSegment; override;
       function GetBoundingBox:TpvVectorPathBoundingBox; override;
     end;

     TpvVectorPathSegments=TpvObjectGenericList<TpvVectorPathSegment>;

     { TpvVectorPathContour }

     TpvVectorPathContour=class
      private
       fSegments:TpvVectorPathSegments;
       fClosed:boolean;
      public
       constructor Create; reintroduce; overload;
       constructor Create(const aContour:TpvVectorPathContour); overload;
       destructor Destroy; override;
       procedure Assign(const aContour:TpvVectorPathContour);
       function Clone:TpvVectorPathContour;
      published
       property Segments:TpvVectorPathSegments read fSegments;
       property Closed:boolean read fClosed write fClosed;
     end;

     TpvVectorPathContours=TpvObjectGenericList<TpvVectorPathContour>;

     TpvVectorPath=class;

     { TpvVectorPathShape }

     TpvVectorPathShape=class
      private
       fFillRule:TpvVectorPathFillRule;
       fContours:TpvVectorPathContours;
      public
       constructor Create; reintroduce; overload;
       constructor Create(const aVectorPathShape:TpvVectorPathShape); reintroduce; overload;
       constructor Create(const aVectorPath:TpvVectorPath); reintroduce; overload;
       destructor Destroy; override;
       procedure Assign(const aVectorPathShape:TpvVectorPathShape); overload;
       procedure Assign(const aVectorPath:TpvVectorPath); overload;
       function Clone:TpvVectorPathShape;
       procedure ConvertCubicCurvesToQuadraticCurves(const aPixelRatio:TpvDouble=1.0);
       procedure ConvertCurvesToLines(const aPixelRatio:TpvDouble=1.0);
       function GetSignedDistance(const aX,aY,aScale:TpvDouble;out aInsideOutsideSign:TpvInt32):TpvDouble;
       function GetBeginEndPoints:TpvVectorPathVectors;
       function GetIntersectionPoints:TpvVectorPathVectors;
      published
       property FillRule:TpvVectorPathFillRule read fFillRule write fFillRule;
       property Contours:TpvVectorPathContours read fContours;
     end;

     { TpvVectorPath }

     TpvVectorPath=class
      private
       fCommands:TpvVectorPathCommandList;
       fFillRule:TpvVectorPathFillRule;
      public
       constructor Create; reintroduce;
       constructor CreateFromSVGPath(const aCommands:TpvRawByteString);
       constructor CreateFromShape(const aShape:TpvVectorPathShape);
       destructor Destroy; override;
       procedure Assign(const aFrom:TpvVectorPath); overload;
       procedure Assign(const aShape:TpvVectorPathShape); overload;
       procedure MoveTo(const aX,aY:TpvDouble);
       procedure LineTo(const aX,aY:TpvDouble);
       procedure QuadraticCurveTo(const aCX,aCY,aAX,aAY:TpvDouble);
       procedure CubicCurveTo(const aC0X,aC0Y,aC1X,aC1Y,aAX,aAY:TpvDouble);
       procedure Close;
       function GetShape:TpvVectorPathShape;
      published
       property FillRule:TpvVectorPathFillRule read fFillRule write fFillRule;
       property Commands:TpvVectorPathCommandList read fCommands;
     end;

implementation

{ TpvVectorPathVector }

constructor TpvVectorPathVector.Create(const aValue:TpvDouble);
begin
 x:=aValue;
 y:=aValue;
end;

constructor TpvVectorPathVector.Create(const aX,aY:TpvDouble);
begin
 x:=aX;
 y:=aY;
end;

function TpvVectorPathVector.Length:TpvDouble;
begin
 result:=sqrt(sqr(x)+sqr(y));
end;

function TpvVectorPathVector.LengthSquared:TpvDouble;
begin
 result:=sqr(x)+sqr(y);
end;

function TpvVectorPathVector.Distance(const b:TpvVectorPathVector):TpvDouble;
begin
 result:=(b-self).Length;
end;

function TpvVectorPathVector.DistanceSquared(const b:TpvVectorPathVector):TpvDouble;
begin
 result:=(b-self).LengthSquared;
end;

function TpvVectorPathVector.Direction:TpvDouble;
begin
 result:=ArcTan2(y,x);
end;

function TpvVectorPathVector.Normalize:TpvVectorPathVector;
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

function TpvVectorPathVector.Minimum(const aRight:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=Min(x,aRight.x);
 result.y:=Min(y,aRight.y);
end;

function TpvVectorPathVector.Maximum(const aRight:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=Max(x,aRight.x);
 result.y:=Max(y,aRight.y);
end;

function TpvVectorPathVector.Dot(const aRight:TpvVectorPathVector):TpvDouble;
begin
 result:=(x*aRight.x)+(y*aRight.y);
end;

function TpvVectorPathVector.Cross(const aRight:TpvVectorPathVector):TpvDouble;
begin
 result:=(x*aRight.y)-(y*aRight.x);
end;

function TpvVectorPathVector.OrthoNormal:TpvVectorPathVector;
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

function TpvVectorPathVector.Lerp(const b:TpvVectorPathVector;const t:TpvDouble):TpvVectorPathVector;
begin
 result.x:=(x*(1.0-t))+(b.x*t);
 result.y:=(y*(1.0-t))+(b.y*t);
end;

function TpvVectorPathVector.ClampedLerp(const b:TpvVectorPathVector;const t:TpvDouble):TpvVectorPathVector;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  result.x:=(x*(1.0-t))+(b.x*t);
  result.y:=(y*(1.0-t))+(b.y*t);
 end;
end;

class function TpvVectorPathVector.IsLeft(const a,b,c:TpvVectorPathVector):TpvDouble;
begin
 result:=((b.x*a.x)*(c.y*a.y))-((c.x*a.x)*(b.y*a.y));
end;

class operator TpvVectorPathVector.Equal(const a,b:TpvVectorPathVector):boolean;
begin
 result:=SameValue(a.x,b.x) and SameValue(a.y,b.y);
end;

class operator TpvVectorPathVector.NotEqual(const a,b:TpvVectorPathVector):boolean;
begin
 result:=(not SameValue(a.x,b.x)) or (not SameValue(a.y,b.y));
end;

class operator TpvVectorPathVector.Add(const a,b:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=a.x+b.x;
 result.y:=a.y+b.y;
end;

class operator TpvVectorPathVector.Subtract(const a,b:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=a.x-b.x;
 result.y:=a.y-b.y;
end;

class operator TpvVectorPathVector.Multiply(const a,b:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=a.x*b.x;
 result.y:=a.y*b.y;
end;

class operator TpvVectorPathVector.Multiply(const a:TpvVectorPathVector;const b:TpvDouble):TpvVectorPathVector;
begin
 result.x:=a.x*b;
 result.y:=a.y*b;
end;

class operator TpvVectorPathVector.Multiply(const a:TpvDouble;const b:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=a*b.x;
 result.y:=a*b.y;
end;

class operator TpvVectorPathVector.Divide(const a,b:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=a.x/b.x;
 result.y:=a.y/b.y;
end;

class operator TpvVectorPathVector.Divide(const a:TpvVectorPathVector;const b:TpvDouble):TpvVectorPathVector;
begin
 result.x:=a.x/b;
 result.y:=a.y/b;
end;

class operator TpvVectorPathVector.Negative(const a:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=-a.x;
 result.y:=-a.y;
end;

class operator TpvVectorPathVector.Positive(const a:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=a.x;
 result.y:=a.y;
end;

{ TpvVectorPathBoundingBox }

constructor TpvVectorPathBoundingBox.Create(const aMin,aMax:TpvVectorPathVector);
begin
 MinMax[0]:=aMin;
 MinMax[1]:=aMax;
end;

{ TpvVectorPathCommand }

constructor TpvVectorPathCommand.Create(const aCommandType:TpvVectorPathCommandType;
                                        const aX0:TpvDouble=0.0;
                                        const aY0:TpvDouble=0.0;
                                        const aX1:TpvDouble=0.0;
                                        const aY1:TpvDouble=0.0;
                                        const aX2:TpvDouble=0.0;
                                        const aY2:TpvDouble=0.0);
begin
 inherited Create;
 fCommandType:=aCommandType;
 fX0:=aX0;
 fY0:=aY0;
 fX1:=aX1;
 fY1:=aY1;
 fX2:=aX2;
 fY2:=aY2;
end;

{ TpvVectorPathSegment }

constructor TpvVectorPathSegment.Create;
begin
 inherited Create;
end;

destructor TpvVectorPathSegment.Destroy;
begin
 inherited Destroy;
end;

procedure TpvVectorPathSegment.Assign(const aSegment:TpvVectorPathSegment);
begin
end;

function TpvVectorPathSegment.Clone:TpvVectorPathSegment;
begin
 result:=TpvVectorPathSegment(ClassType).Create;
 result.Assign(self);
end;

function TpvVectorPathSegment.GetBoundingBox:TpvVectorPathBoundingBox;
begin
 result:=TpvVectorPathBoundingBox.Create(TpvVectorPathVector.Create(MaxDouble,MaxDouble),
                                         TpvVectorPathVector.Create(-MaxDouble,-MaxDouble));
end;

{ TpvVectorPathSegmentLine }

constructor TpvVectorPathSegmentLine.Create;
begin
 inherited Create;
 fType:=TpvVectorPathSegmentType.Line;
end;

constructor TpvVectorPathSegmentLine.Create(const aP0,aP1:TpvVectorPathVector);
begin
 Create;
 Points[0]:=aP0;
 Points[1]:=aP1;
end;

procedure TpvVectorPathSegmentLine.Assign(const aSegment:TpvVectorPathSegment);
begin
 if assigned(aSegment) and (aSegment is TpvVectorPathSegmentLine) then begin
  Points:=TpvVectorPathSegmentLine(aSegment).Points;
 end;
end;

function TpvVectorPathSegmentLine.Clone:TpvVectorPathSegment;
begin
 result:=TpvVectorPathSegmentLine.Create(Points[0],Points[1]);
end;

function TpvVectorPathSegmentLine.GetBoundingBox:TpvVectorPathBoundingBox;
begin
 result:=TpvVectorPathBoundingBox.Create(Points[0].Minimum(Points[1]),
                                         Points[0].Maximum(Points[1]));
end;

{ TpvVectorPathSegmentQuadraticCurve }

constructor TpvVectorPathSegmentQuadraticCurve.Create;
begin
 inherited Create;
 fType:=TpvVectorPathSegmentType.QuadraticCurve;
end;

constructor TpvVectorPathSegmentQuadraticCurve.Create(const aP0,aP1,aP2:TpvVectorPathVector);
begin
 Create;
 Points[0]:=aP0;
 Points[1]:=aP1;
 Points[2]:=aP2;
end;

procedure TpvVectorPathSegmentQuadraticCurve.Assign(const aSegment:TpvVectorPathSegment);
begin
 if assigned(aSegment) and (aSegment is TpvVectorPathSegmentQuadraticCurve) then begin
  Points:=TpvVectorPathSegmentQuadraticCurve(aSegment).Points;
 end;
end;

function TpvVectorPathSegmentQuadraticCurve.Clone:TpvVectorPathSegment;
begin
 result:=TpvVectorPathSegmentQuadraticCurve.Create(Points[0],Points[1],Points[2]);
end;

function TpvVectorPathSegmentQuadraticCurve.GetBoundingBox:TpvVectorPathBoundingBox;
begin
 result:=TpvVectorPathBoundingBox.Create(Points[0].Minimum(Points[1].Minimum(Points[2])),
                                         Points[0].Maximum(Points[1].Maximum(Points[2])));
end;

{ TpvVectorPathSegmentCubicCurve }

constructor TpvVectorPathSegmentCubicCurve.Create;
begin
 inherited Create;
 fType:=TpvVectorPathSegmentType.CubicCurve;
end;

constructor TpvVectorPathSegmentCubicCurve.Create(const aP0,aP1,aP2,aP3:TpvVectorPathVector);
begin
 Create;
 Points[0]:=aP0;
 Points[1]:=aP1;
 Points[2]:=aP2;
 Points[3]:=aP3;
end;

procedure TpvVectorPathSegmentCubicCurve.Assign(const aSegment:TpvVectorPathSegment);
begin
 if assigned(aSegment) and (aSegment is TpvVectorPathSegmentCubicCurve) then begin
  Points:=TpvVectorPathSegmentCubicCurve(aSegment).Points;
 end;
end;

function TpvVectorPathSegmentCubicCurve.Clone:TpvVectorPathSegment;
begin
 result:=TpvVectorPathSegmentCubicCurve.Create(Points[0],Points[1],Points[2],Points[3]);
end;

function TpvVectorPathSegmentCubicCurve.GetBoundingBox:TpvVectorPathBoundingBox;
begin
 result:=TpvVectorPathBoundingBox.Create(Points[0].Minimum(Points[1].Minimum(Points[2].Minimum(Points[3]))),
                                         Points[0].Maximum(Points[1].Maximum(Points[2].Maximum(Points[3]))));
end;

{ TpvVectorPathContour }

constructor TpvVectorPathContour.Create;
begin
 inherited Create;
 fSegments:=TpvVectorPathSegments.Create;
 fSegments.OwnsObjects:=true;
 fClosed:=false;
end;

constructor TpvVectorPathContour.Create(const aContour:TpvVectorPathContour);
begin
 Create;
 Assign(aContour);
end;

destructor TpvVectorPathContour.Destroy;
begin
 FreeAndNil(fSegments);
 inherited Destroy;
end;

procedure TpvVectorPathContour.Assign(const aContour:TpvVectorPathContour);
var Segment:TpvVectorPathSegment;
begin
 fSegments.Clear;
 if assigned(aContour) and assigned(aContour.fSegments) then begin
  for Segment in aContour.fSegments do begin
   if assigned(Segment) then begin
    fSegments.Add(Segment.Clone);
   end;
  end;
 end;
end;

function TpvVectorPathContour.Clone:TpvVectorPathContour;
begin
 result:=TpvVectorPathContour.Create(self);
end;

{ TpvVectorPathShape }

constructor TpvVectorPathShape.Create;
begin
 inherited Create;
 fContours:=TpvVectorPathContours.Create;
 fContours.OwnsObjects:=true;
end;

constructor TpvVectorPathShape.Create(const aVectorPathShape:TpvVectorPathShape);
begin
 Create;
 if assigned(aVectorPathShape) then begin
  Assign(aVectorPathShape);
 end;
end;

constructor TpvVectorPathShape.Create(const aVectorPath:TpvVectorPath);
begin
 Create;
 if assigned(aVectorPath) then begin
  Assign(aVectorPath);
 end;
end;

destructor TpvVectorPathShape.Destroy;
begin
 FreeAndNil(fContours);
 inherited Destroy;
end;

procedure TpvVectorPathShape.Assign(const aVectorPathShape:TpvVectorPathShape);
var Contour:TpvVectorPathContour;
begin
 fContours.Clear;
 if assigned(aVectorPathShape) then begin
  fFillRule:=aVectorPathShape.fFillRule;
  if assigned(aVectorPathShape.fContours) then begin
   for Contour in aVectorPathShape.fContours do begin
    if assigned(Contour) then begin
     fContours.Add(Contour.Clone);
    end;
   end;
  end;
 end;
end;

procedure TpvVectorPathShape.Assign(const aVectorPath:TpvVectorPath);
var CommandIndex:TpvSizeInt;
    Command:TpvVectorPathCommand;
    Contour:TpvVectorPathContour;
    StartPoint,LastPoint,ControlPoint,OtherControlPoint,Point:TpvVectorPathVector;
begin
 fContours.Clear;
 if assigned(aVectorPath) then begin
  fFillRule:=aVectorPath.fFillRule;
  Contour:=nil;
  StartPoint:=TpvVectorPathVector.Create(0.0,0.0);
  LastPoint:=TpvVectorPathVector.Create(0.0,0.0);
  for CommandIndex:=0 to aVectorPath.fCommands.Count-1 do begin
   Command:=aVectorPath.fCommands[CommandIndex];
   case Command.CommandType of
    TpvVectorPathCommandType.MoveTo:begin
     if assigned(Contour) then begin
      if LastPoint<>StartPoint then begin
       Contour.fSegments.Add(TpvVectorPathSegmentLine.Create(LastPoint,StartPoint));
      end;
     end;
     Contour:=TpvVectorPathContour.Create;
     fContours.Add(Contour);
     LastPoint.x:=Command.x0;
     LastPoint.y:=Command.y0;
     StartPoint:=LastPoint;
    end;
    TpvVectorPathCommandType.LineTo:begin
     if not assigned(Contour) then begin
      Contour:=TpvVectorPathContour.Create;
      fContours.Add(Contour);
     end;
     Point.x:=Command.x0;
     Point.y:=Command.y0;
     if assigned(Contour) and (LastPoint<>Point) then begin
      Contour.fSegments.Add(TpvVectorPathSegmentLine.Create(LastPoint,Point));
     end;
     LastPoint:=Point;
    end;
    TpvVectorPathCommandType.QuadraticCurveTo:begin
     if not assigned(Contour) then begin
      Contour:=TpvVectorPathContour.Create;
      fContours.Add(Contour);
     end;
     ControlPoint.x:=Command.x0;
     ControlPoint.y:=Command.y0;
     Point.x:=Command.x1;
     Point.y:=Command.y1;
     if assigned(Contour) and not ((LastPoint=ControlPoint) and (LastPoint=Point)) then begin
      Contour.fSegments.Add(TpvVectorPathSegmentQuadraticCurve.Create(LastPoint,ControlPoint,Point));
     end;
     LastPoint:=Point;
    end;
    TpvVectorPathCommandType.CubicCurveTo:begin
     if not assigned(Contour) then begin
      Contour:=TpvVectorPathContour.Create;
      fContours.Add(Contour);
     end;
     ControlPoint.x:=Command.x0;
     ControlPoint.y:=Command.y0;
     OtherControlPoint.x:=Command.x1;
     OtherControlPoint.y:=Command.y1;
     Point.x:=Command.x2;
     Point.y:=Command.y2;
     if assigned(Contour) and not ((LastPoint=ControlPoint) and (LastPoint=OtherControlPoint) and (LastPoint=Point)) then begin
      Contour.fSegments.Add(TpvVectorPathSegmentCubicCurve.Create(LastPoint,ControlPoint,OtherControlPoint,Point));
     end;
     LastPoint:=Point;
    end;
    TpvVectorPathCommandType.Close:begin
     if assigned(Contour) then begin
      Contour.fClosed:=true;
      if LastPoint<>StartPoint then begin
       Contour.fSegments.Add(TpvVectorPathSegmentLine.Create(LastPoint,StartPoint));
      end;
     end;
     Contour:=nil;
    end;
   end;
  end;
 end;
end;

function TpvVectorPathShape.Clone:TpvVectorPathShape;
begin
 result:=TpvVectorPathShape.Create(self);
end;

function TpvVectorPathShape.GetBeginEndPoints:TpvVectorPathVectors;
var Count:TpvSizeInt;
    Contour:TpvVectorPathContour;
    Segment:TpvVectorPathSegment;
begin
 result:=nil;
 Count:=0;
 try
  for Contour in fContours do begin
   for Segment in Contour.fSegments do begin
    case Segment.Type_ of
     TpvVectorPathSegmentType.Line:begin
      if (Count+1)>=length(result) then begin
       SetLength(result,(Count+2)*2);
      end;
      result[Count]:=TpvVectorPathSegmentLine(Segment).Points[0];
      result[Count+1]:=TpvVectorPathSegmentLine(Segment).Points[1];
      inc(Count,2);
     end;
     TpvVectorPathSegmentType.QuadraticCurve:begin
      if (Count+1)>=length(result) then begin
       SetLength(result,(Count+2)*2);
      end;
      result[Count]:=TpvVectorPathSegmentQuadraticCurve(Segment).Points[0];
      result[Count+1]:=TpvVectorPathSegmentQuadraticCurve(Segment).Points[2];
      inc(Count,2);
     end;
     TpvVectorPathSegmentType.CubicCurve:begin
      if (Count+1)>=length(result) then begin
       SetLength(result,(Count+2)*2);
      end;
      result[Count]:=TpvVectorPathSegmentCubicCurve(Segment).Points[0];
      result[Count+1]:=TpvVectorPathSegmentCubicCurve(Segment).Points[3];
      inc(Count,2);
     end;
     else begin
     end;
    end;
   end;
  end;
 finally
  SetLength(result,Count);
 end;
end;

function TpvVectorPathShape.GetIntersectionPoints:TpvVectorPathVectors;
var Vectors:TpvVectorPathVectors;
    Count:TpvSizeInt;
 procedure OutputPoint(const aVector:TpvVectorPathVector);
 begin
  if Count>=length(Vectors) then begin
   SetLength(Vectors,(Count+1)*2);
  end;
  Vectors[Count]:=aVector;
  inc(Count);
 end;
 procedure HandleLineLine(const aSegment0,aSegment1:TpvVectorPathSegmentLine);
 var a,b,Determinant:TpvDouble;
 begin
  Determinant:=((aSegment1.Points[1].y-aSegment1.Points[0].y)*(aSegment0.Points[1].x-aSegment0.Points[0].x))-((aSegment1.Points[1].x-aSegment1.Points[0].x)*(aSegment0.Points[1].y-aSegment0.Points[0].y));
  if not IsZero(Determinant) then begin
   a:=(((aSegment1.Points[1].x-aSegment1.Points[0].x)*(aSegment0.Points[0].y-aSegment1.Points[0].y))-((aSegment1.Points[1].y-aSegment1.Points[0].y)*(aSegment0.Points[0].x-aSegment1.Points[0].x)))/Determinant;
   b:=(((aSegment0.Points[1].x-aSegment0.Points[0].x)*(aSegment0.Points[0].y-aSegment1.Points[0].y))-((aSegment0.Points[1].y-aSegment0.Points[0].y)*(aSegment0.Points[0].x-aSegment1.Points[0].x)))/Determinant;
   if ((a>=0.0) and (a<=1.0)) and ((b>=0.0) and (b<=1.0)) then begin
    OutputPoint(aSegment0.Points[0].Lerp(aSegment0.Points[1],a));
   end;
  end;
 end;
 procedure HandleLineQuadraticCurve(const aSegment0:TpvVectorPathSegmentLine;const aSegment1:TpvVectorPathSegmentQuadraticCurve);
 var Min_,Max_,c0,c1,c2,n,p:TpvVectorPathVector;
     a,cl,t:TpvDouble;
     Roots:array[0..1] of TpvDouble;
     RootIndex,CountRoots:TpvSizeInt;
 begin
  Min_:=aSegment0.Points[0].Minimum(aSegment0.Points[1]);
  Max_:=aSegment0.Points[0].Maximum(aSegment0.Points[1]);
  c2:=aSegment1.Points[0]+((aSegment1.Points[1]*(-2.0))+aSegment1.Points[2]);
  c1:=(aSegment1.Points[0]*(-2.0))+(aSegment1.Points[1]*2.0);
  c0:=TpvVectorPathVector.Create(aSegment1.Points[0].x,aSegment1.Points[0].y);
  n:=TpvVectorPathVector.Create(aSegment0.Points[0].y-aSegment0.Points[1].y,aSegment0.Points[1].x-aSegment0.Points[0].x);
  cl:=(aSegment0.Points[0].x*aSegment0.Points[1].y)-(aSegment0.Points[1].x*aSegment0.Points[0].y);
  a:=n.Dot(c0)+cl;
  if IsZero(a) then begin
   CountRoots:=0;
  end else begin
   CountRoots:=SolveQuadratic(a,n.Dot(c1)/a,n.Dot(c2)/a,Roots[0],Roots[1]);
  end;
  for RootIndex:=0 to CountRoots-1 do begin
   t:=Roots[RootIndex];
   if (t>=0.0) and (t<=1.0) then begin
    p:=(aSegment1.Points[0].Lerp(aSegment1.Points[1],t)).Lerp(aSegment1.Points[1].Lerp(aSegment1.Points[2],t),t);
    if SameValue(aSegment0.Points[0].x,aSegment0.Points[1].x) then begin
     if (p.y>=Min_.y) and (p.y<=Max_.y) then begin
      OutputPoint(p);
     end;
    end else if SameValue(aSegment0.Points[0].y,aSegment0.Points[1].y) then begin
     if (p.x>=Min_.x) and (p.x<=Max_.x) then begin
      OutputPoint(p);
     end;
    end else if ((p.x>=Min_.x) and (p.x<=Max_.x)) and ((p.y>=Min_.y) and (p.y<=Max_.y)) then begin
     OutputPoint(p);
    end;
   end;
  end;
 end;
 procedure HandleLineCubicCurve(const aSegment0:TpvVectorPathSegmentLine;aSegment1:TpvVectorPathSegmentCubicCurve);
 var Min_,Max_,c0,c1,c2,c3,n,p,p1,p2,p3,p4,p5,p6,p7,p8,p9:TpvVectorPathVector;
     a,cl,t:TpvDouble;
     Roots:array[0..2] of TpvDouble;
     RootIndex,CountRoots:TpvSizeInt;
 begin
  Min_:=aSegment0.Points[0].Minimum(aSegment0.Points[1]);
  Max_:=aSegment0.Points[0].Maximum(aSegment0.Points[1]);
  p1:=aSegment1.Points[0];
  p2:=aSegment1.Points[1];
  p3:=aSegment1.Points[2];
  p4:=aSegment1.Points[3];
  c0:=p1;
  c1:=(p1*(-3.0))+(p2*3.0);
  c2:=(p1*3.0)+((p2*(-6.0))+(p3*3.0));
  c3:=(p1*(-1.0))+((p2*3.0)+((p3*(-3.0))+p4));
  n:=TpvVectorPathVector.Create(aSegment0.Points[0].y-aSegment0.Points[1].y,aSegment0.Points[1].x-aSegment0.Points[0].x);
  cl:=(aSegment0.Points[0].x*aSegment0.Points[1].y)-(aSegment0.Points[1].x*aSegment0.Points[0].y);
  a:=n.Dot(c0)+cl;
  if IsZero(a) then begin
   CountRoots:=0;
  end else begin
   CountRoots:=SolveCubic(a,n.Dot(c1),n.Dot(c2),n.Dot(c3),Roots[0],Roots[1],Roots[2]);
  end;
  for RootIndex:=0 to CountRoots-1 do begin
   t:=Roots[RootIndex];
   if (t>=0.0) and (t<=1.0) then begin
    p5:=p1.Lerp(p2,t);
    p6:=p2.Lerp(p3,t);
    p7:=p3.Lerp(p4,t);
    p8:=p5.Lerp(p6,t);
    p9:=p6.Lerp(p7,t);
    p:=p8.Lerp(p9,t);
    if SameValue(aSegment0.Points[0].x,aSegment0.Points[1].x) then begin
     if (p.y>=Min_.y) and (p.y<=Max_.y) then begin
      OutputPoint(p);
     end;
    end else if SameValue(aSegment0.Points[0].y,aSegment0.Points[1].y) then begin
     if (p.x>=Min_.x) and (p.x<=Max_.x) then begin
      OutputPoint(p);
     end;
    end else if ((p.x>=Min_.x) and (p.x<=Max_.x)) and ((p.y>=Min_.y) and (p.y<=Max_.y)) then begin
     OutputPoint(p);
    end;
   end;
  end;
 end;
 procedure HandleQuadraticCurveQuadraticCurve(const aSegment0,aSegment1:TpvVectorPathSegmentQuadraticCurve);
 var a1,a2,a3,b1,b2,b3,c10,c11,c12,c20,c21,c22:TpvVectorPathVector;
     v0,v1,v2,v3,v4,v5,v6,s,XRoot:TpvDouble;
     Roots:array[0..3] of TpvDouble;
     XRoots,YRoots:array[0..1] of TpvDouble;
     CountRoots,CountXRoots,CountYRoots,Index,XIndex,YIndex:TpvSizeInt;
     OK:boolean;
 begin
  a1:=aSegment0.Points[0];
  a2:=aSegment0.Points[1];
  a3:=aSegment0.Points[2];
  b1:=aSegment1.Points[0];
  b2:=aSegment1.Points[1];
  b3:=aSegment1.Points[2];
  c10:=a1;
  c11:=(a1*(-2.0))+(a2*2.0);
  c12:=a1+((a2*(-2.0))+a3);
  c20:=b1;
  c21:=(b1*(-2.0))+(b2*2.0);
  c22:=b1+((b2*(-2.0))+b3);
  if IsZero(c12.y) then begin
   v0:=c12.x*(c10.y-c20.y);
   v1:=v0-(c11.x*c11.y);
   v2:=v0+v1;
   v3:=c11.y*c11.y;
   CountRoots:=SolveQuartic(c12.x*c22.y*c22.y,
                            2.0*c12.x*c21.y*c22.y,
                            (((c12.x*c21.y*c21.y)-(c22.x*v3))-(c22.y*v0))-(c22.y*v1),
                            (((-c21.x)*v3)-(c21.y*v0))-(c21.y*v1),
                            ((c10.x-c20.x)*v3)+((c10.y-c20.y)*v1),
                            Roots[0],
                            Roots[1],
                            Roots[2],
                            Roots[3]);
  end else begin
   v0:=(c12.x*c22.y)-(c12.y*c22.x);
   v1:=(c12.x*c21.y)-(c21.x*c12.y);
   v2:=(c11.x*c12.y)-(c11.y*c12.x);
   v3:=c10.y-c20.y;
   v4:=(c12.y*(c10.x-c20.x))-(c12.x*v3);
   v5:=((-c11.y)*v2)+(c12.y*v4);
   v6:=v2*v2;
   CountRoots:=SolveQuartic(sqr(v0),
                            2.0*v0*v1,
                            ((((-c22.y)*v6)+(c12.y*v1*v1))+(c12.y*v0*v4)+(v0*v5))/c12.y,
                            ((((-c21.y)*v6)+(c12.y*v1*v4))+(v1*v5))/c12.y,
                            ((v3*v6)+(v4*v5))/c12.y,
                            Roots[0],
                            Roots[1],
                            Roots[2],
                            Roots[3]);
  end;
  for Index:=0 to CountRoots-1 do begin
   s:=Roots[Index];
   if (s>=0.0) and (s<=1.0) then begin
    CountXRoots:=SolveQuadratic(c12.x,
                                c11.x,
                                ((c10.x-c20.x)-(s*c21.x))-(sqr(s)*c22.x),
                                XRoots[0],
                                XRoots[1]
                               );
    CountYRoots:=SolveQuadratic(c12.y,
                                c11.y,
                                ((c10.y-c20.y)-(s*c21.y))-(sqr(s)*c22.y),
                                YRoots[0],
                                YRoots[1]
                               );
    if (CountXRoots>0) and (CountYRoots>0) then begin
     OK:=false;
     for XIndex:=0 to CountXRoots-1 do begin
      XRoot:=XRoots[XIndex];
      if (XRoot>=0.0) and (XRoot<=1.0) then begin
       for YIndex:=0 to CountYRoots-1 do begin
        if SameValue(XRoot,YRoots[XIndex],1e-4) then begin
         OutputPoint((c22*sqr(s))+(c21*s)+c20);
         OK:=true;
         break;
        end;
       end;
      end;
      if OK then begin
       break;
      end;
     end;
    end;
   end;
  end;
 end;
 procedure HandleQuadraticCurveCubicCurve(const aSegment0:TpvVectorPathSegmentQuadraticCurve;aSegment1:TpvVectorPathSegmentCubicCurve);
 var a1,a2,a3,b1,b2,b3,b4,
     c10,c11,c12,c20,c21,c22,c23,
     c10s,c11s,c12s,c20s,c21s,c22s,c23s:TpvVectorPathVector;
     PolyCoefs:array[0..6] of TpvDouble;
     Roots:TpvDoubleDynamicArray;
     XRoots,YRoots:array[0..1] of TpvDouble;
     CountXRoots,CountYRoots,Index,XIndex,YIndex:TpvSizeInt;
     OK:boolean;
     s,XRoot:TpvDouble;
 begin
  a1:=aSegment0.Points[0];
  a2:=aSegment0.Points[1];
  a3:=aSegment0.Points[2];
  b1:=aSegment1.Points[0];
  b2:=aSegment1.Points[1];
  b3:=aSegment1.Points[2];
  b4:=aSegment1.Points[3];
  c10:=a1;
  c11:=(a1*(-2.0))+(a2*2.0);
  c12:=(a1+(a2*(-2.0)))+a3;
  c20:=b1;
  c21:=(b1*(-3.0))+(b2*3.0);
  c22:=((b1*3.0)+(b2*(-6.0)))+(b3*3.0);
  c23:=(((b1*(-1.0))+(b2*3.0))+(b3*(-3.0)))+b4;
  c10s:=c10*c10;
  c11s:=c11*c11;
  c12s:=c12*c12;
  c20s:=c20*c20;
  c21s:=c21*c21;
  c22s:=c22*c22;
  c23s:=c23*c23;
  PolyCoefs[0]:=(((-2.0)*c12.x*c12.y*c23.x*c23.y)+(c12s.x*c23s.y))+(c12s.y*c23s.x);
  PolyCoefs[1]:=((((-2.0)*c12.x*c12.y*c22.x*c23.y)-(2.0*c12.x*c12.y*c22.y*c23.x))+(2.0*c12s.y*c22.x*c23.x))+(2.0*c12s.x*c22.y*c23.y);
  PolyCoefs[2]:=(((((((-2.0)*c12.x*c21.x*c12.y*c23.y)-(2.0*c12.x*c12.y*c21.y*c23.x))-(2.0*c12.x*c12.y*c22.x*c22.y))+(2.0*c21.x*c12s.y*c23.x))+(c12s.y*c22s.x))+(c12s.x*((2.0*c21.y*c23.y)+c22s.y)));
  PolyCoefs[3]:=(((((((((((((((2.0*c10.x*c12.x*c12.y*c23.y)+(2.0*c10.y*c12.x*c12.y*c23.x))+(c11.x*c11.y*c12.x*c23.y))+(c11.x*c11.y*c12.y*c23.x))-(2.0*c20.x*c12.x*c12.y*c23.y))-(2.0*c12.x*c20.y*c12.y*c23.x))-(2.0*c12.x*c21.x*c12.y*c22.y))-(2.0*c12.x*c12.y*c21.y*c22.x))-(2.0*c10.x*c12s.y*c23.x))-(2.0*c10.y*c12s.x*c23.y))+(2.0*c20.x*c12s.y*c23.x))+(2.0*c21.x*c12s.y*c22.x))-(c11s.y*c12.x*c23.x))-(c11s.x*c12.y*c23.y))+(c12s.x*((2.0*c20.y*c23.y)+(2.0*c21.y*c22.y))));
  PolyCoefs[4]:=((((((((((((((2.0*c10.x*c12.x*c12.y*c22.y)+(2.0*c10.y*c12.x*c12.y*c22.x))+(c11.x*c11.y*c12.x*c22.y))+(c11.x*c11.y*c12.y*c22.x))-(2.0*c20.x*c12.x*c12.y*c22.y))-(2.0*c12.x*c20.y*c12.y*c22.x))-(2.0*c12.x*c21.x*c12.y*c21.y))-(2.0*c10.x*c12s.y*c22.x))-(2.0*c10.y*c12s.x*c22.y))+(2.0*c20.x*c12s.y*c22.x))-(c11s.y*c12.x*c22.x))-(c11s.x*c12.y*c22.y))+(c21s.x*c12s.y))+(c12s.x*((2.0*c20.y*c22.y)+c21s.y)));
  PolyCoefs[5]:=(((((((((((2.0*c10.x*c12.x*c12.y*c21.y)+(2.0*c10.y*c12.x*c21.x*c12.y))+(c11.x*c11.y*c12.x*c21.y))+(c11.x*c11.y*c21.x*c12.y)-(2.0*c20.x*c12.x*c12.y*c21.y))-(2.0*c12.x*c20.y*c21.x*c12.y))-(2.0*c10.x*c21.x*c12s.y))-(2.0*c10.y*c12s.x*c21.y))+(2.0*c20.x*c21.x*c12s.y))-(c11s.y*c12.x*c21.x))-(c11s.x*c12.y*c21.y))+(2.0*c12s.x*c20.y*c21.y));
  PolyCoefs[6]:=(((((((((((((((((((-2.0)*c10.x*c10.y*c12.x*c12.y)-(c10.x*c11.x*c11.y*c12.y))-(c10.y*c11.x*c11.y*c12.x))+(2.0*c10.x*c12.x*c20.y*c12.y))+(2.0*c10.y*c20.x*c12.x*c12.y))+(c11.x*c20.x*c11.y*c12.y))+(c11.x*c11.y*c12.x*c20.y))-(2.0*c20.x*c12.x*c20.y*c12.y))-(2.0*c10.x*c20.x*c12s.y))+(c10.x*c11s.y*c12.x))+(c10.y*c11s.x*c12.y))-(2.0*c10.y*c12s.x*c20.y))-(c20.x*c11s.y*c12.x))-(c11s.x*c20.y*c12.y))+(c10s.x*c12s.y))+(c10s.y*c12s.x))+(c20s.x*c12s.y))+(c12s.x*c20s.y));
  Roots:=SolveRootsInInterval(PolyCoefs,0.0,1.0);
  for Index:=0 to length(Roots)-1 do begin
   s:=Roots[Index];
   if (s>=0.0) and (s<=1.0) then begin
    CountXRoots:=SolveQuadratic(c12.x,
                                c11.x,
                                (((c10.x-c20.x)-(s*c21.x))-(sqr(s)*c22.x))-((sqr(s)*s)*c23.x),
                                XRoots[0],
                                XRoots[1]
                               );
    CountYRoots:=SolveQuadratic(c12.y,
                                c11.y,
                                (((c10.x-c20.y)-(s*c21.y))-(sqr(s)*c22.y))-((sqr(s)*s)*c23.y),
                                YRoots[0],
                                YRoots[1]
                               );
    if (CountXRoots>0) and (CountYRoots>0) then begin
     OK:=false;
     for XIndex:=0 to CountXRoots-1 do begin
      XRoot:=XRoots[XIndex];
      if (XRoot>=0.0) and (XRoot<=1.0) then begin
       for YIndex:=0 to CountYRoots-1 do begin
        if SameValue(XRoot,YRoots[XIndex],1e-4) then begin
         OutputPoint((c23*(sqr(s)*s))+(c22*sqr(s))+(c21*s)+c20);
         OK:=true;
         break;
        end;
       end;
      end;
      if OK then begin
       break;
      end;
     end;
    end;
   end;
  end;
 end;
 procedure HandleCubicCurveCubicCurve(const aSegment0,aSegment1:TpvVectorPathSegmentCubicCurve);
 var a1,a2,a3,a4,b1,b2,b3,b4,
     c10,c11,c12,c13,c20,c21,c22,c23,
     c10s,c11s,c12s,c13s,c20s,c21s,c22s,c23s,
     c10c,c11c,c12c,c13c,c20c,c21c,c22c,c23c:TpvVectorPathVector;
     PolyCoefs:array[0..9] of TpvDouble;
     Roots:TpvDoubleDynamicArray;
     XRoots,YRoots:array[0..1] of TpvDouble;
     CountXRoots,CountYRoots,Index,XIndex,YIndex:TpvSizeInt;
     OK:boolean;
     s,XRoot:TpvDouble;
 begin
  a1:=aSegment0.Points[0];
  a2:=aSegment0.Points[1];
  a3:=aSegment0.Points[2];
  a4:=aSegment0.Points[3];
  b1:=aSegment1.Points[0];
  b2:=aSegment1.Points[1];
  b3:=aSegment1.Points[2];
  b4:=aSegment1.Points[3];
  c10:=a1;
  c11:=(a1*(-3.0))+(a2*3.0);
  c12:=((a1*3.0)+(a2*(-6.0)))+(a3*3.0);
  c13:=(((a1*(-1.0))+(a2*3.0))+(a3*(-3.0)))+a4;
  c20:=b1;
  c21:=(b1*(-3.0))+(b2*3.0);
  c22:=((b1*3.0)+(b2*(-6.0)))+(b3*3.0);
  c23:=(((b1*(-1.0))+(b2*3.0))+(b3*(-3.0)))+b4;
  c10s:=c10*c10;
  c11s:=c11*c11;
  c12s:=c12*c12;
  c13s:=c13*c13;
  c20s:=c20*c20;
  c21s:=c21*c21;
  c22s:=c22*c22;
  c23s:=c23*c23;
  c10c:=c10s*c10;
  c11c:=c11s*c11;
  c12c:=c12s*c12;
  c13c:=c13s*c13;
  c20c:=c20s*c20;
  c21c:=c21s*c21;
  c22c:=c22s*c22;
  c23c:=c23s*c23;
  PolyCoefs[0]:=-c13c.x*c23c.y+c13c.y*c23c.x-3*c13.x*c13s.y*c23s.x*c23.y+3*c13s.x*c13.y*c23.x*c23s.y;
  PolyCoefs[1]:=-6*c13.x*c22.x*c13s.y*c23.x*c23.y+6*c13s.x*c13.y*c22.y*c23.x*c23.y+3*c22.x*c13c.y*c23s.x-3*c13c.x*c22.y*c23s.y-3*c13.x*c13s.y*c22.y*c23s.x+3*c13s.x*c22.x*c13.y*c23s.y;
  PolyCoefs[2]:=-6*c21.x*c13.x*c13s.y*c23.x*c23.y-6*c13.x*c22.x*c13s.y*c22.y*c23.x+6*c13s.x*c22.x*c13.y*c22.y*c23.y+3*c21.x*c13c.y*c23s.x+3*c22s.x*c13c.y*c23.x+3*c21.x*c13s.x*c13.y*c23s.y-3*c13.x*c21.y*c13s.y*c23s.x-3*c13.x*c22s.x*c13s.y*c23.y+c13s.x*c13.y*c23.x*(6*c21.y*c23.y+3*c22s.y)+c13c.x*(-c21.y*c23s.y-2*c22s.y*c23.y-c23.y*(2*c21.y*c23.y+c22s.y));
  PolyCoefs[3]:=c11.x*c12.y*c13.x*c13.y*c23.x*c23.y-c11.y*c12.x*c13.x*c13.y*c23.x*c23.y+6*c21.x*c22.x*c13c.y*c23.x+3*c11.x*c12.x*c13.x*c13.y*c23s.y+6*c10.x*c13.x*c13s.y*c23.x*c23.y-3*c11.x*c12.x*c13s.y*c23.x*c23.y-3*c11.y*c12.y*c13.x*c13.y*c23s.x-6*c10.y*c13s.x*c13.y*c23.x*c23.y-6*c20.x*c13.x*c13s.y*c23.x*c23.y+3*c11.y*c12.y*c13s.x*c23.x*c23.y-2*c12.x*c12s.y*c13.x*c23.x*c23.y-6*c21.x*c13.x*c22.x*c13s.y*c23.y-6*c21.x*c13.x*c13s.y*c22.y*c23.x-6*c13.x*c21.y*c22.x*c13s.y*c23.x+6*c21.x*c13s.x*c13.y*c22.y*c23.y+2*c12s.x*c12.y*c13.y*c23.x*c23.y+c22c.x*c13c.y-3*c10.x*c13c.y*c23s.x+3*c10.y*c13c.x*c23s.y+3*c20.x*c13c.y*c23s.x+c12c.y*c13.x*c23s.x-c12c.x*c13.y*c23s.y-3*c10.x*c13s.x*c13.y*c23s.y+3*c10.y*c13.x*c13s.y*c23s.x-2*c11.x*c12.y*c13s.x*c23s.y+c11.x*c12.y*c13s.y*c23s.x-c11.y*c12.x*c13s.x*c23s.y+2*c11.y*c12.x*c13s.y*c23s.x+3*c20.x*c13s.x*c13.y*c23s.y-c12.x*c12s.y*c13.y*c23s.x-3*c20.y*c13.x*c13s.y*c23s.x+c12s.x*c12.y*c13.x*c23s.y-
                3*c13.x*c22s.x*c13s.y*c22.y+c13s.x*c13.y*c23.x*(6*c20.y*c23.y+6*c21.y*c22.y)+c13s.x*c22.x*c13.y*(6*c21.y*c23.y+3*c22s.y)+c13c.x*(-2*c21.y*c22.y*c23.y-c20.y*c23s.y-c22.y*(2*c21.y*c23.y+c22s.y)-c23.y*(2*c20.y*c23.y+2*c21.y*c22.y));
  PolyCoefs[4]:=6*c11.x*c12.x*c13.x*c13.y*c22.y*c23.y+c11.x*c12.y*c13.x*c22.x*c13.y*c23.y+c11.x*c12.y*c13.x*c13.y*c22.y*c23.x-c11.y*c12.x*c13.x*c22.x*c13.y*c23.y-c11.y*c12.x*c13.x*c13.y*c22.y*c23.x-6*c11.y*c12.y*c13.x*c22.x*c13.y*c23.x-6*c10.x*c22.x*c13c.y*c23.x+6*c20.x*c22.x*c13c.y*c23.x+6*c10.y*c13c.x*c22.y*c23.y+2*c12c.y*c13.x*c22.x*c23.x-2*c12c.x*c13.y*c22.y*c23.y+6*c10.x*c13.x*c22.x*c13s.y*c23.y+6*c10.x*c13.x*c13s.y*c22.y*c23.x+6*c10.y*c13.x*c22.x*c13s.y*c23.x-3*c11.x*c12.x*c22.x*c13s.y*c23.y-3*c11.x*c12.x*c13s.y*c22.y*c23.x+2*c11.x*c12.y*c22.x*c13s.y*c23.x+4*c11.y*c12.x*c22.x*c13s.y*c23.x-6*c10.x*c13s.x*c13.y*c22.y*c23.y-6*c10.y*c13s.x*c22.x*c13.y*c23.y-6*c10.y*c13s.x*c13.y*c22.y*c23.x-4*c11.x*c12.y*c13s.x*c22.y*c23.y-6*c20.x*c13.x*c22.x*c13s.y*c23.y-6*c20.x*c13.x*c13s.y*c22.y*c23.x-2*c11.y*c12.x*c13s.x*c22.y*c23.y+3*c11.y*c12.y*c13s.x*c22.x*c23.y+3*c11.y*c12.y*c13s.x*c22.y*c23.x-2*c12.x*c12s.y*c13.x*c22.x*c23.y-
                2*c12.x*c12s.y*c13.x*c22.y*c23.x-2*c12.x*c12s.y*c22.x*c13.y*c23.x-6*c20.y*c13.x*c22.x*c13s.y*c23.x-6*c21.x*c13.x*c21.y*c13s.y*c23.x-6*c21.x*c13.x*c22.x*c13s.y*c22.y+6*c20.x*c13s.x*c13.y*c22.y*c23.y+2*c12s.x*c12.y*c13.x*c22.y*c23.y+2*c12s.x*c12.y*c22.x*c13.y*c23.y+2*c12s.x*c12.y*c13.y*c22.y*c23.x+3*c21.x*c22s.x*c13c.y+3*c21s.x*c13c.y*c23.x-3*c13.x*c21.y*c22s.x*c13s.y-3*c21s.x*c13.x*c13s.y*c23.y+c13s.x*c22.x*c13.y*(6*c20.y*c23.y+6*c21.y*c22.y)+c13s.x*c13.y*c23.x*(6*c20.y*c22.y+3*c21s.y)+c21.x*c13s.x*c13.y*(6*c21.y*c23.y+3*c22s.y)+c13c.x*(-2*c20.y*c22.y*c23.y-c23.y*(2*c20.y*c22.y+c21s.y)-c21.y*(2*c21.y*c23.y+c22s.y)-c22.y*(2*c20.y*c23.y+2*c21.y*c22.y));
  PolyCoefs[5]:=c11.x*c21.x*c12.y*c13.x*c13.y*c23.y+c11.x*c12.y*c13.x*c21.y*c13.y*c23.x+c11.x*c12.y*c13.x*c22.x*c13.y*c22.y-c11.y*c12.x*c21.x*c13.x*c13.y*c23.y-c11.y*c12.x*c13.x*c21.y*c13.y*c23.x-c11.y*c12.x*c13.x*c22.x*c13.y*c22.y-6*c11.y*c21.x*c12.y*c13.x*c13.y*c23.x-6*c10.x*c21.x*c13c.y*c23.x+6*c20.x*c21.x*c13c.y*c23.x+2*c21.x*c12c.y*c13.x*c23.x+6*c10.x*c21.x*c13.x*c13s.y*c23.y+6*c10.x*c13.x*c21.y*c13s.y*c23.x+6*c10.x*c13.x*c22.x*c13s.y*c22.y+6*c10.y*c21.x*c13.x*c13s.y*c23.x-3*c11.x*c12.x*c21.x*c13s.y*c23.y-3*c11.x*c12.x*c21.y*c13s.y*c23.x-3*c11.x*c12.x*c22.x*c13s.y*c22.y+2*c11.x*c21.x*c12.y*c13s.y*c23.x+4*c11.y*c12.x*c21.x*c13s.y*c23.x-6*c10.y*c21.x*c13s.x*c13.y*c23.y-6*c10.y*c13s.x*c21.y*c13.y*c23.x-6*c10.y*c13s.x*c22.x*c13.y*c22.y-6*c20.x*c21.x*c13.x*c13s.y*c23.y-6*c20.x*c13.x*c21.y*c13s.y*c23.x-6*c20.x*c13.x*c22.x*c13s.y*c22.y+3*c11.y*c21.x*c12.y*c13s.x*c23.y-3*c11.y*c12.y*c13.x*c22s.x*c13.y+3*c11.y*c12.y*c13s.x*c21.y*c23.x+
                3*c11.y*c12.y*c13s.x*c22.x*c22.y-2*c12.x*c21.x*c12s.y*c13.x*c23.y-2*c12.x*c21.x*c12s.y*c13.y*c23.x-2*c12.x*c12s.y*c13.x*c21.y*c23.x-2*c12.x*c12s.y*c13.x*c22.x*c22.y-6*c20.y*c21.x*c13.x*c13s.y*c23.x-6*c21.x*c13.x*c21.y*c22.x*c13s.y+6*c20.y*c13s.x*c21.y*c13.y*c23.x+2*c12s.x*c21.x*c12.y*c13.y*c23.y+2*c12s.x*c12.y*c21.y*c13.y*c23.x+2*c12s.x*c12.y*c22.x*c13.y*c22.y-3*c10.x*c22s.x*c13c.y+3*c20.x*c22s.x*c13c.y+3*c21s.x*c22.x*c13c.y+c12c.y*c13.x*c22s.x+3*c10.y*c13.x*c22s.x*c13s.y+c11.x*c12.y*c22s.x*c13s.y+2*c11.y*c12.x*c22s.x*c13s.y-c12.x*c12s.y*c22s.x*c13.y-3*c20.y*c13.x*c22s.x*c13s.y-3*c21s.x*c13.x*c13s.y*c22.y+c12s.x*c12.y*c13.x*(2*c21.y*c23.y+c22s.y)+c11.x*c12.x*c13.x*c13.y*(6*c21.y*c23.y+3*c22s.y)+c21.x*c13s.x*c13.y*(6*c20.y*c23.y+6*c21.y*c22.y)+c12c.x*c13.y*(-2*c21.y*c23.y-c22s.y)+c10.y*c13c.x*(6*c21.y*c23.y+3*c22s.y)+c11.y*c12.x*c13s.x*(-2*c21.y*c23.y-c22s.y)+
                c11.x*c12.y*c13s.x*(-4*c21.y*c23.y-2*c22s.y)+c10.x*c13s.x*c13.y*(-6*c21.y*c23.y-3*c22s.y)+c13s.x*c22.x*c13.y*(6*c20.y*c22.y+3*c21s.y)+c20.x*c13s.x*c13.y*(6*c21.y*c23.y+3*c22s.y)+c13c.x*(-2*c20.y*c21.y*c23.y-c22.y*(2*c20.y*c22.y+c21s.y)-c20.y*(2*c21.y*c23.y+c22s.y)-c21.y*(2*c20.y*c23.y+2*c21.y*c22.y));
  PolyCoefs[6]:=-c10.x*c11.x*c12.y*c13.x*c13.y*c23.y+c10.x*c11.y*c12.x*c13.x*c13.y*c23.y+6*c10.x*c11.y*c12.y*c13.x*c13.y*c23.x-6*c10.y*c11.x*c12.x*c13.x*c13.y*c23.y-c10.y*c11.x*c12.y*c13.x*c13.y*c23.x+c10.y*c11.y*c12.x*c13.x*c13.y*c23.x+c11.x*c11.y*c12.x*c12.y*c13.x*c23.y-c11.x*c11.y*c12.x*c12.y*c13.y*c23.x+c11.x*c20.x*c12.y*c13.x*c13.y*c23.y+c11.x*c20.y*c12.y*c13.x*c13.y*c23.x+c11.x*c21.x*c12.y*c13.x*c13.y*c22.y+c11.x*c12.y*c13.x*c21.y*c22.x*c13.y-c20.x*c11.y*c12.x*c13.x*c13.y*c23.y-6*c20.x*c11.y*c12.y*c13.x*c13.y*c23.x-c11.y*c12.x*c20.y*c13.x*c13.y*c23.x-c11.y*c12.x*c21.x*c13.x*c13.y*c22.y-c11.y*c12.x*c13.x*c21.y*c22.x*c13.y-6*c11.y*c21.x*c12.y*c13.x*c22.x*c13.y-6*c10.x*c20.x*c13c.y*c23.x-6*c10.x*c21.x*c22.x*c13c.y-2*c10.x*c12c.y*c13.x*c23.x+6*c20.x*c21.x*c22.x*c13c.y+2*c20.x*c12c.y*c13.x*c23.x+2*c21.x*c12c.y*c13.x*c22.x+2*c10.y*c12c.x*c13.y*c23.y-6*c10.x*c10.y*c13.x*c13s.y*c23.x+3*c10.x*c11.x*c12.x*c13s.y*c23.y-
                2*c10.x*c11.x*c12.y*c13s.y*c23.x-4*c10.x*c11.y*c12.x*c13s.y*c23.x+3*c10.y*c11.x*c12.x*c13s.y*c23.x+6*c10.x*c10.y*c13s.x*c13.y*c23.y+6*c10.x*c20.x*c13.x*c13s.y*c23.y-3*c10.x*c11.y*c12.y*c13s.x*c23.y+2*c10.x*c12.x*c12s.y*c13.x*c23.y+2*c10.x*c12.x*c12s.y*c13.y*c23.x+6*c10.x*c20.y*c13.x*c13s.y*c23.x+6*c10.x*c21.x*c13.x*c13s.y*c22.y+6*c10.x*c13.x*c21.y*c22.x*c13s.y+4*c10.y*c11.x*c12.y*c13s.x*c23.y+6*c10.y*c20.x*c13.x*c13s.y*c23.x+2*c10.y*c11.y*c12.x*c13s.x*c23.y-3*c10.y*c11.y*c12.y*c13s.x*c23.x+2*c10.y*c12.x*c12s.y*c13.x*c23.x+6*c10.y*c21.x*c13.x*c22.x*c13s.y-3*c11.x*c20.x*c12.x*c13s.y*c23.y+2*c11.x*c20.x*c12.y*c13s.y*c23.x+c11.x*c11.y*c12s.y*c13.x*c23.x-3*c11.x*c12.x*c20.y*c13s.y*c23.x-3*c11.x*c12.x*c21.x*c13s.y*c22.y-3*c11.x*c12.x*c21.y*c22.x*c13s.y+2*c11.x*c21.x*c12.y*c22.x*c13s.y+4*c20.x*c11.y*c12.x*c13s.y*c23.x+4*c11.y*c12.x*c21.x*c22.x*c13s.y-2*c10.x*c12s.x*c12.y*c13.y*c23.y-6*c10.y*c20.x*c13s.x*c13.y*c23.y-
                6*c10.y*c20.y*c13s.x*c13.y*c23.x-6*c10.y*c21.x*c13s.x*c13.y*c22.y-2*c10.y*c12s.x*c12.y*c13.x*c23.y-2*c10.y*c12s.x*c12.y*c13.y*c23.x-6*c10.y*c13s.x*c21.y*c22.x*c13.y-c11.x*c11.y*c12s.x*c13.y*c23.y-2*c11.x*c11s.y*c13.x*c13.y*c23.x+3*c20.x*c11.y*c12.y*c13s.x*c23.y-2*c20.x*c12.x*c12s.y*c13.x*c23.y-2*c20.x*c12.x*c12s.y*c13.y*c23.x-6*c20.x*c20.y*c13.x*c13s.y*c23.x-6*c20.x*c21.x*c13.x*c13s.y*c22.y-6*c20.x*c13.x*c21.y*c22.x*c13s.y+3*c11.y*c20.y*c12.y*c13s.x*c23.x+3*c11.y*c21.x*c12.y*c13s.x*c22.y+3*c11.y*c12.y*c13s.x*c21.y*c22.x-2*c12.x*c20.y*c12s.y*c13.x*c23.x-2*c12.x*c21.x*c12s.y*c13.x*c22.y-2*c12.x*c21.x*c12s.y*c22.x*c13.y-2*c12.x*c12s.y*c13.x*c21.y*c22.x-6*c20.y*c21.x*c13.x*c22.x*c13s.y-c11s.y*c12.x*c12.y*c13.x*c23.x+2*c20.x*c12s.x*c12.y*c13.y*c23.y+6*c20.y*c13s.x*c21.y*c22.x*c13.y+2*c11s.x*c11.y*c13.x*c13.y*c23.y+c11s.x*c12.x*c12.y*c13.y*c23.y+2*c12s.x*c20.y*c12.y*c13.y*c23.x+2*c12s.x*c21.x*c12.y*c13.y*c22.y+
                2*c12s.x*c12.y*c21.y*c22.x*c13.y+c21c.x*c13c.y+3*c10s.x*c13c.y*c23.x-3*c10s.y*c13c.x*c23.y+3*c20s.x*c13c.y*c23.x+c11c.y*c13s.x*c23.x-c11c.x*c13s.y*c23.y-c11.x*c11s.y*c13s.x*c23.y+c11s.x*c11.y*c13s.y*c23.x-3*c10s.x*c13.x*c13s.y*c23.y+3*c10s.y*c13s.x*c13.y*c23.x-c11s.x*c12s.y*c13.x*c23.y+c11s.y*c12s.x*c13.y*c23.x-3*c21s.x*c13.x*c21.y*c13s.y-3*c20s.x*c13.x*c13s.y*c23.y+3*c20s.y*c13s.x*c13.y*c23.x+c11.x*c12.x*c13.x*c13.y*(6*c20.y*c23.y+6*c21.y*c22.y)+c12c.x*c13.y*(-2*c20.y*c23.y-2*c21.y*c22.y)+c10.y*c13c.x*(6*c20.y*c23.y+6*c21.y*c22.y)+c11.y*c12.x*c13s.x*(-2*c20.y*c23.y-2*c21.y*c22.y)+c12s.x*c12.y*c13.x*(2*c20.y*c23.y+2*c21.y*c22.y)+c11.x*c12.y*c13s.x*(-4*c20.y*c23.y-4*c21.y*c22.y)+c10.x*c13s.x*c13.y*(-6*c20.y*c23.y-6*c21.y*c22.y)+c20.x*c13s.x*c13.y*(6*c20.y*c23.y+6*c21.y*c22.y)+c21.x*c13s.x*c13.y*(6*c20.y*c22.y+3*c21s.y)+c13c.x*(-2*c20.y*c21.y*c22.y-c20s.y*c23.y-c21.y*(2*c20.y*c22.y+c21s.y)-c20.y*(2*c20.y*c23.y+2*c21.y*c22.y));
  PolyCoefs[7]:=-c10.x*c11.x*c12.y*c13.x*c13.y*c22.y+c10.x*c11.y*c12.x*c13.x*c13.y*c22.y+6*c10.x*c11.y*c12.y*c13.x*c22.x*c13.y-6*c10.y*c11.x*c12.x*c13.x*c13.y*c22.y-c10.y*c11.x*c12.y*c13.x*c22.x*c13.y+c10.y*c11.y*c12.x*c13.x*c22.x*c13.y+c11.x*c11.y*c12.x*c12.y*c13.x*c22.y-c11.x*c11.y*c12.x*c12.y*c22.x*c13.y+c11.x*c20.x*c12.y*c13.x*c13.y*c22.y+c11.x*c20.y*c12.y*c13.x*c22.x*c13.y+c11.x*c21.x*c12.y*c13.x*c21.y*c13.y-c20.x*c11.y*c12.x*c13.x*c13.y*c22.y-6*c20.x*c11.y*c12.y*c13.x*c22.x*c13.y-c11.y*c12.x*c20.y*c13.x*c22.x*c13.y-c11.y*c12.x*c21.x*c13.x*c21.y*c13.y-6*c10.x*c20.x*c22.x*c13c.y-2*c10.x*c12c.y*c13.x*c22.x+2*c20.x*c12c.y*c13.x*c22.x+2*c10.y*c12c.x*c13.y*c22.y-6*c10.x*c10.y*c13.x*c22.x*c13s.y+3*c10.x*c11.x*c12.x*c13s.y*c22.y-2*c10.x*c11.x*c12.y*c22.x*c13s.y-4*c10.x*c11.y*c12.x*c22.x*c13s.y+3*c10.y*c11.x*c12.x*c22.x*c13s.y+6*c10.x*c10.y*c13s.x*c13.y*c22.y+6*c10.x*c20.x*c13.x*c13s.y*c22.y-3*c10.x*c11.y*c12.y*c13s.x*c22.y+
                2*c10.x*c12.x*c12s.y*c13.x*c22.y+2*c10.x*c12.x*c12s.y*c22.x*c13.y+6*c10.x*c20.y*c13.x*c22.x*c13s.y+6*c10.x*c21.x*c13.x*c21.y*c13s.y+4*c10.y*c11.x*c12.y*c13s.x*c22.y+6*c10.y*c20.x*c13.x*c22.x*c13s.y+2*c10.y*c11.y*c12.x*c13s.x*c22.y-3*c10.y*c11.y*c12.y*c13s.x*c22.x+2*c10.y*c12.x*c12s.y*c13.x*c22.x-3*c11.x*c20.x*c12.x*c13s.y*c22.y+2*c11.x*c20.x*c12.y*c22.x*c13s.y+c11.x*c11.y*c12s.y*c13.x*c22.x-3*c11.x*c12.x*c20.y*c22.x*c13s.y-3*c11.x*c12.x*c21.x*c21.y*c13s.y+4*c20.x*c11.y*c12.x*c22.x*c13s.y-2*c10.x*c12s.x*c12.y*c13.y*c22.y-6*c10.y*c20.x*c13s.x*c13.y*c22.y-6*c10.y*c20.y*c13s.x*c22.x*c13.y-6*c10.y*c21.x*c13s.x*c21.y*c13.y-2*c10.y*c12s.x*c12.y*c13.x*c22.y-2*c10.y*c12s.x*c12.y*c22.x*c13.y-c11.x*c11.y*c12s.x*c13.y*c22.y-2*c11.x*c11s.y*c13.x*c22.x*c13.y+3*c20.x*c11.y*c12.y*c13s.x*c22.y-2*c20.x*c12.x*c12s.y*c13.x*c22.y-2*c20.x*c12.x*c12s.y*c22.x*c13.y-6*c20.x*c20.y*c13.x*c22.x*c13s.y-6*c20.x*c21.x*c13.x*c21.y*c13s.y+
                3*c11.y*c20.y*c12.y*c13s.x*c22.x+3*c11.y*c21.x*c12.y*c13s.x*c21.y-2*c12.x*c20.y*c12s.y*c13.x*c22.x-2*c12.x*c21.x*c12s.y*c13.x*c21.y-c11s.y*c12.x*c12.y*c13.x*c22.x+2*c20.x*c12s.x*c12.y*c13.y*c22.y-3*c11.y*c21s.x*c12.y*c13.x*c13.y+6*c20.y*c21.x*c13s.x*c21.y*c13.y+2*c11s.x*c11.y*c13.x*c13.y*c22.y+c11s.x*c12.x*c12.y*c13.y*c22.y+2*c12s.x*c20.y*c12.y*c22.x*c13.y+2*c12s.x*c21.x*c12.y*c21.y*c13.y-3*c10.x*c21s.x*c13c.y+3*c20.x*c21s.x*c13c.y+3*c10s.x*c22.x*c13c.y-3*c10s.y*c13c.x*c22.y+3*c20s.x*c22.x*c13c.y+c21s.x*c12c.y*c13.x+c11c.y*c13s.x*c22.x-c11c.x*c13s.y*c22.y+3*c10.y*c21s.x*c13.x*c13s.y-c11.x*c11s.y*c13s.x*c22.y+c11.x*c21s.x*c12.y*c13s.y+2*c11.y*c12.x*c21s.x*c13s.y+c11s.x*c11.y*c22.x*c13s.y-c12.x*c21s.x*c12s.y*c13.y-3*c20.y*c21s.x*c13.x*c13s.y-3*c10s.x*c13.x*c13s.y*c22.y+3*c10s.y*c13s.x*c22.x*c13.y-c11s.x*c12s.y*c13.x*c22.y+c11s.y*c12s.x*c22.x*c13.y-3*c20s.x*c13.x*c13s.y*c22.y+3*c20s.y*c13s.x*c22.x*c13.y+
                c12s.x*c12.y*c13.x*(2*c20.y*c22.y+c21s.y)+c11.x*c12.x*c13.x*c13.y*(6*c20.y*c22.y+3*c21s.y)+c12c.x*c13.y*(-2*c20.y*c22.y-c21s.y)+c10.y*c13c.x*(6*c20.y*c22.y+3*c21s.y)+c11.y*c12.x*c13s.x*(-2*c20.y*c22.y-c21s.y)+c11.x*c12.y*c13s.x*(-4*c20.y*c22.y-2*c21s.y)+c10.x*c13s.x*c13.y*(-6*c20.y*c22.y-3*c21s.y)+c20.x*c13s.x*c13.y*(6*c20.y*c22.y+3*c21s.y)+c13c.x*(-2*c20.y*c21s.y-c20s.y*c22.y-c20.y*(2*c20.y*c22.y+c21s.y));
  PolyCoefs[8]:=-c10.x*c11.x*c12.y*c13.x*c21.y*c13.y+c10.x*c11.y*c12.x*c13.x*c21.y*c13.y+6*c10.x*c11.y*c21.x*c12.y*c13.x*c13.y-6*c10.y*c11.x*c12.x*c13.x*c21.y*c13.y-c10.y*c11.x*c21.x*c12.y*c13.x*c13.y+c10.y*c11.y*c12.x*c21.x*c13.x*c13.y-c11.x*c11.y*c12.x*c21.x*c12.y*c13.y+c11.x*c11.y*c12.x*c12.y*c13.x*c21.y+c11.x*c20.x*c12.y*c13.x*c21.y*c13.y+6*c11.x*c12.x*c20.y*c13.x*c21.y*c13.y+c11.x*c20.y*c21.x*c12.y*c13.x*c13.y-c20.x*c11.y*c12.x*c13.x*c21.y*c13.y-6*c20.x*c11.y*c21.x*c12.y*c13.x*c13.y-c11.y*c12.x*c20.y*c21.x*c13.x*c13.y-6*c10.x*c20.x*c21.x*c13c.y-2*c10.x*c21.x*c12c.y*c13.x+6*c10.y*c20.y*c13c.x*c21.y+2*c20.x*c21.x*c12c.y*c13.x+2*c10.y*c12c.x*c21.y*c13.y-2*c12c.x*c20.y*c21.y*c13.y-6*c10.x*c10.y*c21.x*c13.x*c13s.y+3*c10.x*c11.x*c12.x*c21.y*c13s.y-2*c10.x*c11.x*c21.x*c12.y*c13s.y-4*c10.x*c11.y*c12.x*c21.x*c13s.y+3*c10.y*c11.x*c12.x*c21.x*c13s.y+6*c10.x*c10.y*c13s.x*c21.y*c13.y+6*c10.x*c20.x*c13.x*c21.y*c13s.y-
                3*c10.x*c11.y*c12.y*c13s.x*c21.y+2*c10.x*c12.x*c21.x*c12s.y*c13.y+2*c10.x*c12.x*c12s.y*c13.x*c21.y+6*c10.x*c20.y*c21.x*c13.x*c13s.y+4*c10.y*c11.x*c12.y*c13s.x*c21.y+6*c10.y*c20.x*c21.x*c13.x*c13s.y+2*c10.y*c11.y*c12.x*c13s.x*c21.y-3*c10.y*c11.y*c21.x*c12.y*c13s.x+2*c10.y*c12.x*c21.x*c12s.y*c13.x-3*c11.x*c20.x*c12.x*c21.y*c13s.y+2*c11.x*c20.x*c21.x*c12.y*c13s.y+c11.x*c11.y*c21.x*c12s.y*c13.x-3*c11.x*c12.x*c20.y*c21.x*c13s.y+4*c20.x*c11.y*c12.x*c21.x*c13s.y-6*c10.x*c20.y*c13s.x*c21.y*c13.y-2*c10.x*c12s.x*c12.y*c21.y*c13.y-6*c10.y*c20.x*c13s.x*c21.y*c13.y-6*c10.y*c20.y*c21.x*c13s.x*c13.y-2*c10.y*c12s.x*c21.x*c12.y*c13.y-2*c10.y*c12s.x*c12.y*c13.x*c21.y-c11.x*c11.y*c12s.x*c21.y*c13.y-4*c11.x*c20.y*c12.y*c13s.x*c21.y-2*c11.x*c11s.y*c21.x*c13.x*c13.y+3*c20.x*c11.y*c12.y*c13s.x*c21.y-2*c20.x*c12.x*c21.x*c12s.y*c13.y-2*c20.x*c12.x*c12s.y*c13.x*c21.y-6*c20.x*c20.y*c21.x*c13.x*c13s.y-2*c11.y*c12.x*c20.y*c13s.x*c21.y+
                3*c11.y*c20.y*c21.x*c12.y*c13s.x-2*c12.x*c20.y*c21.x*c12s.y*c13.x-c11s.y*c12.x*c21.x*c12.y*c13.x+6*c20.x*c20.y*c13s.x*c21.y*c13.y+2*c20.x*c12s.x*c12.y*c21.y*c13.y+2*c11s.x*c11.y*c13.x*c21.y*c13.y+c11s.x*c12.x*c12.y*c21.y*c13.y+2*c12s.x*c20.y*c21.x*c12.y*c13.y+2*c12s.x*c20.y*c12.y*c13.x*c21.y+3*c10s.x*c21.x*c13c.y-3*c10s.y*c13c.x*c21.y+3*c20s.x*c21.x*c13c.y+c11c.y*c21.x*c13s.x-c11c.x*c21.y*c13s.y-3*c20s.y*c13c.x*c21.y-c11.x*c11s.y*c13s.x*c21.y+c11s.x*c11.y*c21.x*c13s.y-3*c10s.x*c13.x*c21.y*c13s.y+3*c10s.y*c21.x*c13s.x*c13.y-c11s.x*c12s.y*c13.x*c21.y+c11s.y*c12s.x*c21.x*c13.y-3*c20s.x*c13.x*c21.y*c13s.y+3*c20s.y*c21.x*c13s.x*c13.y;
  PolyCoefs[9]:=c10.x*c10.y*c11.x*c12.y*c13.x*c13.y-c10.x*c10.y*c11.y*c12.x*c13.x*c13.y+c10.x*c11.x*c11.y*c12.x*c12.y*c13.y-c10.y*c11.x*c11.y*c12.x*c12.y*c13.x-c10.x*c11.x*c20.y*c12.y*c13.x*c13.y+6*c10.x*c20.x*c11.y*c12.y*c13.x*c13.y+c10.x*c11.y*c12.x*c20.y*c13.x*c13.y-c10.y*c11.x*c20.x*c12.y*c13.x*c13.y-6*c10.y*c11.x*c12.x*c20.y*c13.x*c13.y+c10.y*c20.x*c11.y*c12.x*c13.x*c13.y-c11.x*c20.x*c11.y*c12.x*c12.y*c13.y+c11.x*c11.y*c12.x*c20.y*c12.y*c13.x+c11.x*c20.x*c20.y*c12.y*c13.x*c13.y-c20.x*c11.y*c12.x*c20.y*c13.x*c13.y-2*c10.x*c20.x*c12c.y*c13.x+2*c10.y*c12c.x*c20.y*c13.y-3*c10.x*c10.y*c11.x*c12.x*c13s.y-6*c10.x*c10.y*c20.x*c13.x*c13s.y+3*c10.x*c10.y*c11.y*c12.y*c13s.x-2*c10.x*c10.y*c12.x*c12s.y*c13.x-2*c10.x*c11.x*c20.x*c12.y*c13s.y-c10.x*c11.x*c11.y*c12s.y*c13.x+3*c10.x*c11.x*c12.x*c20.y*c13s.y-4*c10.x*c20.x*c11.y*c12.x*c13s.y+3*c10.y*c11.x*c20.x*c12.x*c13s.y+6*c10.x*c10.y*c20.y*c13s.x*c13.y+2*c10.x*c10.y*c12s.x*c12.y*c13.y+
                2*c10.x*c11.x*c11s.y*c13.x*c13.y+2*c10.x*c20.x*c12.x*c12s.y*c13.y+6*c10.x*c20.x*c20.y*c13.x*c13s.y-3*c10.x*c11.y*c20.y*c12.y*c13s.x+2*c10.x*c12.x*c20.y*c12s.y*c13.x+c10.x*c11s.y*c12.x*c12.y*c13.x+c10.y*c11.x*c11.y*c12s.x*c13.y+4*c10.y*c11.x*c20.y*c12.y*c13s.x-3*c10.y*c20.x*c11.y*c12.y*c13s.x+2*c10.y*c20.x*c12.x*c12s.y*c13.x+2*c10.y*c11.y*c12.x*c20.y*c13s.x+c11.x*c20.x*c11.y*c12s.y*c13.x-3*c11.x*c20.x*c12.x*c20.y*c13s.y-2*c10.x*c12s.x*c20.y*c12.y*c13.y-6*c10.y*c20.x*c20.y*c13s.x*c13.y-2*c10.y*c20.x*c12s.x*c12.y*c13.y-2*c10.y*c11s.x*c11.y*c13.x*c13.y-c10.y*c11s.x*c12.x*c12.y*c13.y-2*c10.y*c12s.x*c20.y*c12.y*c13.x-2*c11.x*c20.x*c11s.y*c13.x*c13.y-c11.x*c11.y*c12s.x*c20.y*c13.y+3*c20.x*c11.y*c20.y*c12.y*c13s.x-2*c20.x*c12.x*c20.y*c12s.y*c13.x-c20.x*c11s.y*c12.x*c12.y*c13.x+3*c10s.y*c11.x*c12.x*c13.x*c13.y+3*c11.x*c12.x*c20s.y*c13.x*c13.y+2*c20.x*c12s.x*c20.y*c12.y*c13.y-3*c10s.x*c11.y*c12.y*c13.x*c13.y+
                2*c11s.x*c11.y*c20.y*c13.x*c13.y+c11s.x*c12.x*c20.y*c12.y*c13.y-3*c20s.x*c11.y*c12.y*c13.x*c13.y-c10c.x*c13c.y+c10c.y*c13c.x+c20c.x*c13c.y-c20c.y*c13c.x-3*c10.x*c20s.x*c13c.y-c10.x*c11c.y*c13s.x+3*c10s.x*c20.x*c13c.y+c10.y*c11c.x*c13s.y+3*c10.y*c20s.y*c13c.x+c20.x*c11c.y*c13s.x+c10s.x*c12c.y*c13.x-3*c10s.y*c20.y*c13c.x-c10s.y*c12c.x*c13.y+c20s.x*c12c.y*c13.x-c11c.x*c20.y*c13s.y-c12c.x*c20s.y*c13.y-c10.x*c11s.x*c11.y*c13s.y+c10.y*c11.x*c11s.y*c13s.x-3*c10.x*c10s.y*c13s.x*c13.y-c10.x*c11s.y*c12s.x*c13.y+c10.y*c11s.x*c12s.y*c13.x-c11.x*c11s.y*c20.y*c13s.x+3*c10s.x*c10.y*c13.x*c13s.y+c10s.x*c11.x*c12.y*c13s.y+2*c10s.x*c11.y*c12.x*c13s.y-2*c10s.y*c11.x*c12.y*c13s.x-c10s.y*c11.y*c12.x*c13s.x+c11s.x*c20.x*c11.y*c13s.y-3*c10.x*c20s.y*c13s.x*c13.y+3*c10.y*c20s.x*c13.x*c13s.y+c11.x*c20s.x*c12.y*c13s.y-2*c11.x*c20s.y*c12.y*c13s.x+c20.x*c11s.y*c12s.x*c13.y-c11.y*c12.x*c20s.y*c13s.x-c10s.x*c12.x*c12s.y*c13.y-3*c10s.x*c20.y*c13.x*c13s.y+
                3*c10s.y*c20.x*c13s.x*c13.y+c10s.y*c12s.x*c12.y*c13.x-c11s.x*c20.y*c12s.y*c13.x+2*c20s.x*c11.y*c12.x*c13s.y+3*c20.x*c20s.y*c13s.x*c13.y-c20s.x*c12.x*c12s.y*c13.y-3*c20s.x*c20.y*c13.x*c13s.y+c12s.x*c20s.y*c12.y*c13.x;
  Roots:=SolveRootsInInterval(PolyCoefs,0.0,1.0);
  for Index:=0 to length(Roots)-1 do begin
   s:=Roots[Index];
   if (s>=0.0) and (s<=1.0) then begin
    CountXRoots:=SolveQuadratic(c12.x,
                                c11.x,
                                (((c10.x-c20.x)-(s*c21.x))-(sqr(s)*c22.x))-((sqr(s)*s)*c23.x),
                                XRoots[0],
                                XRoots[1]
                               );
    CountYRoots:=SolveQuadratic(c12.y,
                                c11.y,
                                (((c10.x-c20.y)-(s*c21.y))-(sqr(s)*c22.y))-((sqr(s)*s)*c23.y),
                                YRoots[0],
                                YRoots[1]
                               );
    if (CountXRoots>0) and (CountYRoots>0) then begin
     OK:=false;
     for XIndex:=0 to CountXRoots-1 do begin
      XRoot:=XRoots[XIndex];
      if (XRoot>=0.0) and (XRoot<=1.0) then begin
       for YIndex:=0 to CountYRoots-1 do begin
        if SameValue(XRoot,YRoots[XIndex],1e-4) then begin
         OutputPoint((c23*(sqr(s)*s))+(c22*sqr(s))+(c21*s)+c20);
         OK:=true;
         break;
        end;
       end;
      end;
      if OK then begin
       break;
      end;
     end;
    end;
   end;
  end;
 end;
var SegmentIndex,OtherSegmentIndex:TpvSizeInt;
    Segment,OtherSegment:TpvVectorPathSegment;
    Segments:TpvVectorPathSegments;
    Contour:TpvVectorPathContour;
begin
 Segments:=TpvVectorPathSegments.Create;
 try
  Segments.OwnsObjects:=false;
  for Contour in fContours do begin
   for Segment in Contour.fSegments do begin
    Segments.Add(Segment);
   end;
  end;
  Vectors:=nil;
  Count:=0;
  try
   for SegmentIndex:=0 to Segments.Count-1 do begin
    Segment:=Segments[SegmentIndex];
    for OtherSegmentIndex:=SegmentIndex+1 to Segments.Count-1 do begin
     OtherSegment:=Segments[OtherSegmentIndex];
     case Segment.Type_ of
      TpvVectorPathSegmentType.Line:begin
       case OtherSegment.Type_ of
        TpvVectorPathSegmentType.Line:begin
         HandleLineLine(TpvVectorPathSegmentLine(Segment),TpvVectorPathSegmentLine(OtherSegment));
        end;
        TpvVectorPathSegmentType.QuadraticCurve:begin
         HandleLineQuadraticCurve(TpvVectorPathSegmentLine(Segment),TpvVectorPathSegmentQuadraticCurve(OtherSegment));
        end;
        TpvVectorPathSegmentType.CubicCurve:begin
         HandleLineCubicCurve(TpvVectorPathSegmentLine(Segment),TpvVectorPathSegmentCubicCurve(OtherSegment));
        end;
        else begin
        end;
       end;
      end;
      TpvVectorPathSegmentType.QuadraticCurve:begin
       case OtherSegment.Type_ of
        TpvVectorPathSegmentType.Line:begin
         HandleLineQuadraticCurve(TpvVectorPathSegmentLine(OtherSegment),TpvVectorPathSegmentQuadraticCurve(Segment));
        end;
        TpvVectorPathSegmentType.QuadraticCurve:begin
         HandleQuadraticCurveQuadraticCurve(TpvVectorPathSegmentQuadraticCurve(Segment),TpvVectorPathSegmentQuadraticCurve(OtherSegment));
        end;
        TpvVectorPathSegmentType.CubicCurve:begin
         HandleQuadraticCurveCubicCurve(TpvVectorPathSegmentQuadraticCurve(Segment),TpvVectorPathSegmentCubicCurve(OtherSegment));
        end;
        else begin
        end;
       end;
      end;
      TpvVectorPathSegmentType.CubicCurve:begin
       case OtherSegment.Type_ of
        TpvVectorPathSegmentType.Line:begin
         HandleLineCubicCurve(TpvVectorPathSegmentLine(OtherSegment),TpvVectorPathSegmentCubicCurve(Segment));
        end;
        TpvVectorPathSegmentType.QuadraticCurve:begin
         HandleQuadraticCurveCubicCurve(TpvVectorPathSegmentQuadraticCurve(OtherSegment),TpvVectorPathSegmentCubicCurve(Segment));
        end;
        TpvVectorPathSegmentType.CubicCurve:begin
         HandleCubicCurveCubicCurve(TpvVectorPathSegmentCubicCurve(Segment),TpvVectorPathSegmentCubicCurve(OtherSegment));
        end;
        else begin
        end;
       end;
      end;
      else begin
      end;
     end;
    end;
   end;
  finally
   SetLength(Vectors,Count);
  end;
 finally
  FreeAndNil(Segments);
 end;
 result:=Vectors;
end;

procedure TpvVectorPathShape.ConvertCubicCurvesToQuadraticCurves(const aPixelRatio:TpvDouble);
var ValueOne,NearlyZeroValue,LengthScale:TpvDouble;
    Contour:TpvVectorPathContour;
 procedure ConvertCubicCurveToQuadraticCurve(const aCP0,aCP1,aCP2,aCP3:TpvVectorPathVector);
 const MaxChoppedPoints=10;
 type TChoppedPoints=array[0..MaxChoppedPoints-1] of TpvVectorPathVector;
 var ChoppedPoints:TChoppedPoints;
  procedure OutputLine(const aP0,aP1:TpvVectorPathVector);
  begin
   Contour.fSegments.Add(TpvVectorPathSegmentLine.Create(aP0,aP1));
  end;
  procedure OutputQuad(const aP0,aP1,aP2:TpvVectorPathVector);
  begin
   Contour.fSegments.Add(TpvVectorPathSegmentQuadraticCurve.Create(aP0,aP1,aP2));
  end;
  procedure ChopCubicAt(Src,Dst:PpvVectorPathRawVectors;const t:TpvDouble); overload;
  var p0,p1,p2,p3,ab,bc,cd,abc,bcd,abcd:TpvVectorPathVector;
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
    ab:=p0.Lerp(p1,t);
    bc:=p1.Lerp(p2,t);
    cd:=p2.Lerp(p3,t);
    abc:=ab.Lerp(bc,t);
    bcd:=bc.Lerp(cd,t);
    abcd:=abc.Lerp(bcd,t);
    Dst^[0]:=p0;
    Dst^[1]:=ab;
    Dst^[2]:=abc;
    Dst^[3]:=abcd;
    Dst^[4]:=bcd;
    Dst^[5]:=cd;
    Dst^[6]:=p3;
   end;
  end;
  procedure ChopCubicAt(Src,Dst:PpvVectorPathRawVectors;const t0,t1:TpvDouble); overload;
  var p0,p1,p2,p3,
      ab0,bc0,cd0,abc0,bcd0,abcd0,
      ab1,bc1,cd1,abc1,bcd1,abcd1,
      Middle0,Middle1:TpvVectorPathVector;
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
    ab0:=p0.Lerp(p1,t0);
    bc0:=p1.Lerp(p2,t0);
    cd0:=p2.Lerp(p3,t0);
    abc0:=ab0.Lerp(bc0,t0);
    bcd0:=bc0.Lerp(cd0,t0);
    abcd0:=abc0.Lerp(bcd0,t0);
    ab1:=p0.Lerp(p1,t1);
    bc1:=p1.Lerp(p2,t1);
    cd1:=p2.Lerp(p3,t1);
    abc1:=ab1.Lerp(bc1,t1);
    bcd1:=bc1.Lerp(cd1,t1);
    abcd1:=abc1.Lerp(bcd1,t1);
    Middle0:=abc0.Lerp(bcd0,t1);
    Middle1:=abc1.Lerp(bcd1,t0);
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
  function ChopCubicAtInflections(const aSrc:array of TpvVectorPathVector;out aDst:TChoppedPoints):TpvSizeInt;
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
      Src:PpvVectorPathVector;
      Dst:PpvVectorPathVector;
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
  function IsNearlyZeroValue(const aValue:TpvDouble):Boolean;
  begin
   result:=(aValue<NearlyZeroValue) or IsZero(aValue);
  end;
  procedure ConvertNonInflectCubicToQuads(const aPoints:PpvVectorPathRawVectors;const aSquaredTolerance:TpvDouble;const aSubLevel:TpvSizeInt=0;const aPreserveFirstTangent:boolean=true;const aPreserveLastTangent:boolean=true);
  const MaxSubdivisions=10;
  var ab,dc,c0,c1,c:TpvVectorPathVector;
      p:array[0..7] of TpvVectorPathVector;
  begin
   ab:=aPoints^[1]-aPoints^[0];
   dc:=aPoints^[2]-aPoints^[3];
   if IsNearlyZeroValue(ab.LengthSquared) then begin
    if IsNearlyZeroValue(dc.LengthSquared) then begin
     OutputLine(aPoints^[0],aPoints^[3]);
     exit;
    end else begin
     ab:=aPoints^[2]-aPoints^[0];
    end;
   end;
   if IsNearlyZeroValue(dc.LengthSquared) then begin
    dc:=aPoints^[1]-aPoints^[3];
   end;
   ab.x:=ab.x*LengthScale;
   ab.y:=ab.y*LengthScale;
   dc.x:=dc.x*LengthScale;
   dc.y:=dc.y*LengthScale;
   c0:=aPoints^[0]+ab;
   c1:=aPoints^[3]+dc;
   if (aSubLevel>MaxSubdivisions) or ((c0-c1).LengthSquared<aSquaredTolerance) then begin
    if aPreserveFirstTangent=aPreserveLastTangent then begin
     c:=c0.Lerp(c1,0.5);
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
     Points:array[0..3] of TpvVectorPathVector;
 begin
  Points[0]:=aCP0;
  Points[1]:=aCP1;
  Points[2]:=aCP2;
  Points[3]:=aCP3;
  if (IsNaN(Points[0].x) or IsInfinite(Points[0].x)) or
     (IsNaN(Points[0].y) or IsInfinite(Points[0].y)) or
     (IsNaN(Points[1].x) or IsInfinite(Points[1].x)) or
     (IsNaN(Points[1].y) or IsInfinite(Points[1].y)) or
     (IsNaN(Points[2].x) or IsInfinite(Points[2].x)) or
     (IsNaN(Points[2].y) or IsInfinite(Points[2].y)) or
     (IsNaN(Points[3].x) or IsInfinite(Points[3].x)) or
     (IsNaN(Points[3].y) or IsInfinite(Points[3].y)) then begin
   OutputLine(Points[0],Points[2]);
  end else begin
   Count:=ChopCubicAtInflections(Points,ChoppedPoints);
   if Count>0 then begin
    for Index:=0 to Count-1 do begin
     ConvertNonInflectCubicToQuads(TpvPointer(@ChoppedPoints[Index*3]),ValueOne,0,true,true);
    end;
   end;
  end;
 end;
var Segments:TpvVectorPathSegments;
    Segment:TpvVectorPathSegment;
begin
 ValueOne:=aPixelRatio;
 NearlyZeroValue:=ValueOne/TpvInt64(1 shl 18);
 LengthScale:=ValueOne*1.5;
 for Contour in fContours do begin
  Segments:=Contour.fSegments;
  try
   Contour.fSegments:=TpvVectorPathSegments.Create;
   Contour.fSegments.OwnsObjects:=true;
   for Segment in Segments do begin
    case Segment.Type_ of
     TpvVectorPathSegmentType.CubicCurve:begin
      ConvertCubicCurveToQuadraticCurve(TpvVectorPathSegmentCubicCurve(Segment).Points[0],
                                        TpvVectorPathSegmentCubicCurve(Segment).Points[1],
                                        TpvVectorPathSegmentCubicCurve(Segment).Points[2],
                                        TpvVectorPathSegmentCubicCurve(Segment).Points[3]);
     end;
     else begin
      Contour.fSegments.Add(Segment.Clone);
     end;
    end;
   end;
  finally
   FreeAndNil(Segments);
  end;
 end;
end;

procedure TpvVectorPathShape.ConvertCurvesToLines(const aPixelRatio:TpvDouble);
const CurveRecursionLimit=16;
var Contour:TpvVectorPathContour;
    CurveTessellationTolerance,CurveTessellationToleranceSquared,
    LastX,LastY:TpvDouble;
 procedure DoLineTo(const aToX,aToY:TpvDouble);
 begin
  if (not SameValue(LastX,aToX)) or (not SameValue(LastY,aToY)) then begin
   Contour.fSegments.Add(TpvVectorPathSegmentLine.Create(TpvVectorPathVector.Create(LastX,LastY),TpvVectorPathVector.Create(aToX,aToY)));
  end;
  LastX:=aToX;
  LastY:=aToY;
 end;
 procedure DoQuadraticCurveTo(const aLX,aLY,aC0X,aC0Y,aA0X,aA0Y:TpvDouble);
  procedure Recursive(const x1,y1,x2,y2,x3,y3:TpvDouble;const Level:TpvInt32);
  var x12,y12,x23,y23,x123,y123,dx,dy:TpvDouble;
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
    DoLineTo(x3,y3);
   end else begin
    Recursive(x1,y1,x12,y12,x123,y123,Level+1);
    Recursive(x123,y123,x23,y23,x3,y3,Level+1);
   end;
  end;
 begin
  LastX:=aLX;
  LastY:=aLY;
  Recursive(aLX,aLY,aC0X,aC0Y,aA0X,aA0Y,0);
  DoLineTo(aA0X,aA0Y);
 end;
 procedure DoCubicCurveTo(const aLX,aLY,aC0X,aC0Y,aC1X,aC1Y,aA0X,aA0Y:TpvDouble);
  procedure Recursive(const x1,y1,x2,y2,x3,y3,x4,y4:TpvDouble;const Level:TpvInt32);
  var x12,y12,x23,y23,x34,y34,x123,y123,x234,y234,x1234,y1234,dx,dy:TpvDouble;
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
    DoLineTo(x4,y4);
   end else begin
    Recursive(x1,y1,x12,y12,x123,y123,x1234,y1234,Level+1);
    Recursive(x1234,y1234,x234,y234,x34,y34,x4,y4,Level+1);
   end;
  end;
 begin
  LastX:=aLX;
  LastY:=aLY;
  Recursive(aLX,aLY,aC0X,aC0Y,aC1X,aC1Y,aA0X,aA0Y,0);
  DoLineTo(aA0X,aA0Y);
 end;
var Segments:TpvVectorPathSegments;
    Segment:TpvVectorPathSegment;
begin
 CurveTessellationTolerance:=aPixelRatio*0.125;
 CurveTessellationToleranceSquared:=CurveTessellationTolerance*CurveTessellationTolerance;
 LastX:=0.0;
 LastY:=0.0;
 for Contour in fContours do begin
  Segments:=Contour.fSegments;
  try
   Contour.fSegments:=TpvVectorPathSegments.Create;
   Contour.fSegments.OwnsObjects:=true;
   for Segment in Segments do begin
    case Segment.Type_ of
     TpvVectorPathSegmentType.QuadraticCurve:begin
      DoQuadraticCurveTo(TpvVectorPathSegmentQuadraticCurve(Segment).Points[0].x,TpvVectorPathSegmentQuadraticCurve(Segment).Points[0].y,
                         TpvVectorPathSegmentQuadraticCurve(Segment).Points[1].x,TpvVectorPathSegmentQuadraticCurve(Segment).Points[1].y,
                         TpvVectorPathSegmentQuadraticCurve(Segment).Points[2].x,TpvVectorPathSegmentQuadraticCurve(Segment).Points[2].y);
     end;
     TpvVectorPathSegmentType.CubicCurve:begin
      DoCubicCurveTo(TpvVectorPathSegmentCubicCurve(Segment).Points[0].x,TpvVectorPathSegmentCubicCurve(Segment).Points[0].y,
                     TpvVectorPathSegmentCubicCurve(Segment).Points[1].x,TpvVectorPathSegmentCubicCurve(Segment).Points[1].y,
                     TpvVectorPathSegmentCubicCurve(Segment).Points[2].x,TpvVectorPathSegmentCubicCurve(Segment).Points[2].y,
                     TpvVectorPathSegmentCubicCurve(Segment).Points[3].x,TpvVectorPathSegmentCubicCurve(Segment).Points[3].y);
     end;
     else begin
      Contour.fSegments.Add(Segment.Clone);
     end;
    end;
   end;
  finally
   FreeAndNil(Segments);
  end;
 end;
end;

function TpvVectorPathShape.GetSignedDistance(const aX,aY,aScale:TpvDouble;out aInsideOutsideSign:TpvInt32):TpvDouble;
const CurveTessellationTolerance=0.25;
      CurveTessellationToleranceSquared=CurveTessellationTolerance*CurveTessellationTolerance;
      CurveRecursionLimit=16;
var ResultDistance,LastX,LastY:TpvDouble;
 procedure LineDistance(const aPX,aPY,aAX,aAY,aBX,aBY:TpvDouble);
 var pax,pay,bax,bay,t:TpvDouble;
 begin
  pax:=aPX-aAX;
  pay:=aPY-aAY;
  bax:=aBX-aAX;
  bay:=aBY-aAY;
  if ((aAY>aPY)<>(aBY>aPY)) and (pax<(bax*(pay/bay))) then begin
   aInsideOutsideSign:=-aInsideOutsideSign;
  end;
  t:=sqr(bax)+sqr(bay);
  if t>0.0 then begin
   t:=Min(Max(((pax*bax)+(pay*bay))/t,0.0),1.0);
  end else begin
   t:=0.0;
  end;
  ResultDistance:=Min(ResultDistance,sqr(pax-(bax*t))+sqr(pay-(bay*t)));
 end;
 procedure DoLineTo(const aLX,aLY,aToX,aToY:TpvDouble);
 begin
  LineDistance(aX,aY,aLX,aLY,aToX,aToY);
  LastX:=aToX;
  LastY:=aToY;
 end;
 procedure DoQuadraticCurveTo(const aLX,aLY,aC0X,aC0Y,aA0X,aA0Y:TpvDouble);
  procedure Recursive(const x1,y1,x2,y2,x3,y3:TpvDouble;const Level:TpvInt32);
  var x12,y12,x23,y23,x123,y123,dx,dy:TpvDouble;
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
    DoLineTo(LastX,LastY,x3,y3);
   end else begin
    Recursive(x1,y1,x12,y12,x123,y123,level+1);
    Recursive(x123,y123,x23,y23,x3,y3,level+1);
   end;
  end;
 begin
  LastX:=aLX;
  LastY:=aLY;
  Recursive(aLX,aLY,aC0X,aC0Y,aA0X,aA0Y,0);
  DoLineTo(LastX,LastY,aA0X,aA0Y);
 end;
 procedure DoCubicCurveTo(const aLX,aLY,aC0X,aC0Y,aC1X,aC1Y,aA0X,aA0Y:TpvDouble);
  procedure Recursive(const x1,y1,x2,y2,x3,y3,x4,y4:TpvDouble;const Level:TpvInt32);
  var x12,y12,x23,y23,x34,y34,x123,y123,x234,y234,x1234,y1234,dx,dy:TpvDouble;
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
    DoLineTo(LastX,LastY,x4,y4);
   end else begin
    Recursive(x1,y1,x12,y12,x123,y123,x1234,y1234,Level+1);
    Recursive(x1234,y1234,x234,y234,x34,y34,x4,y4,Level+1);
   end;
  end;
 begin
  LastX:=aLX;
  LastY:=aLY;
  Recursive(aLX,aLY,aC0X,aC0Y,aC1X,aC1Y,aA0X,aA0Y,0);
  DoLineTo(LastX,LastY,aA0X,aA0Y);
 end;
var Contour:TpvVectorPathContour;
    Segments:TpvVectorPathSegments;
    Segment:TpvVectorPathSegment;
begin
 ResultDistance:=Infinity;
 aInsideOutsideSign:=1;
 LastX:=0.0;
 LastY:=0.0;
 for Contour in fContours do begin
  for Segment in Contour.fSegments do begin
   case Segment.Type_ of
    TpvVectorPathSegmentType.Line:begin
     DoLineTo(TpvVectorPathSegmentLine(Segment).Points[0].x,TpvVectorPathSegmentLine(Segment).Points[0].y,
              TpvVectorPathSegmentLine(Segment).Points[1].x,TpvVectorPathSegmentLine(Segment).Points[1].y);
    end;
    TpvVectorPathSegmentType.QuadraticCurve:begin
     DoQuadraticCurveTo(TpvVectorPathSegmentQuadraticCurve(Segment).Points[0].x,TpvVectorPathSegmentQuadraticCurve(Segment).Points[0].y,
                        TpvVectorPathSegmentQuadraticCurve(Segment).Points[1].x,TpvVectorPathSegmentQuadraticCurve(Segment).Points[1].y,
                        TpvVectorPathSegmentQuadraticCurve(Segment).Points[2].x,TpvVectorPathSegmentQuadraticCurve(Segment).Points[2].y);
    end;
    TpvVectorPathSegmentType.CubicCurve:begin
     DoCubicCurveTo(TpvVectorPathSegmentCubicCurve(Segment).Points[0].x,TpvVectorPathSegmentCubicCurve(Segment).Points[0].y,
                    TpvVectorPathSegmentCubicCurve(Segment).Points[1].x,TpvVectorPathSegmentCubicCurve(Segment).Points[1].y,
                    TpvVectorPathSegmentCubicCurve(Segment).Points[2].x,TpvVectorPathSegmentCubicCurve(Segment).Points[2].y,
                    TpvVectorPathSegmentCubicCurve(Segment).Points[3].x,TpvVectorPathSegmentCubicCurve(Segment).Points[3].y);
    end;
    else begin
    end;
   end;
  end;
 end;
 result:=sqrt(ResultDistance);
end;

{ TpvVectorPath }

constructor TpvVectorPath.Create;
begin
 inherited Create;
 fCommands:=TpvVectorPathCommandList.Create(true);
 fFillRule:=TpvVectorPathFillRule.EvenOdd;
end;

constructor TpvVectorPath.CreateFromSVGPath(const aCommands:TpvRawByteString);
var i,SrcPos,SrcLen,large_arc_flag,sweep_flag:TpvInt32;
    lx,ly,lcx,lcy,x0,y0,x1,y1,x2,y2,lmx,lmy,rx,ry,x_axis_rotation,x,y:TpvDouble;
    Src:TpvRawByteString;
    Command,LastCommand:AnsiChar;
 procedure SkipBlank;
 begin
  while (SrcPos<=SrcLen) and (Src[SrcPos] in [#0..#32]) do begin
   inc(SrcPos);
  end;
 end;
 function GetFloat:TpvDouble;
 var StartPos:TpvInt32;
 begin
  SkipBlank;
  StartPos:=SrcPos;
  if (SrcPos<=SrcLen) and (Src[SrcPos] in ['-','+']) then begin
   inc(SrcPos);
  end;
  while (SrcPos<=SrcLen) and (Src[SrcPos] in ['0'..'9']) do begin
   inc(SrcPos);
  end;
  if (SrcPos<=SrcLen) and (Src[SrcPos] in ['.']) then begin
   inc(SrcPos);
   while (SrcPos<=SrcLen) and (Src[SrcPos] in ['0'..'9']) do begin
    inc(SrcPos);
   end;
  end;
  if (SrcPos<=SrcLen) and (Src[SrcPos] in ['e','E']) then begin
   inc(SrcPos);
   if (SrcPos<=SrcLen) and (Src[SrcPos] in ['-','+']) then begin
    inc(SrcPos);
   end;
   while (SrcPos<=SrcLen) and (Src[SrcPos] in ['0'..'9']) do begin
    inc(SrcPos);
   end;
  end;
  if StartPos<SrcPos then begin
   result:=ConvertStringToDouble(TpvRawByteString(copy(String(Src),StartPos,SrcPos-StartPos)),rmNearest,nil,-1);
  end else begin
   result:=0.0;
  end;
  SkipBlank;
 end;
 function GetInt:TpvInt32;
 var s:TpvRawByteString;
 begin
  SkipBlank;
  s:='';
  if (SrcPos<=SrcLen) and (Src[SrcPos] in ['-','+']) then begin
   s:=s+Src[SrcPos];
   inc(SrcPos);
  end;
  while (SrcPos<=SrcLen) and (Src[SrcPos] in ['0'..'9']) do begin
   s:=s+Src[SrcPos];
   inc(SrcPos);
  end;
  if (SrcPos<=SrcLen) and (Src[SrcPos] in ['.']) then begin
   inc(SrcPos);
   while (SrcPos<=SrcLen) and (Src[SrcPos] in ['0'..'9']) do begin
    inc(SrcPos);
   end;
  end;
  if (SrcPos<=SrcLen) and (Src[SrcPos] in ['e','E']) then begin
   inc(SrcPos);
   if (SrcPos<=SrcLen) and (Src[SrcPos] in ['-','+']) then begin
    inc(SrcPos);
   end;
   while (SrcPos<=SrcLen) and (Src[SrcPos] in ['0'..'9']) do begin
    inc(SrcPos);
   end;
  end;
  SkipBlank;
  result:=Trunc(ConvertStringToDouble(s,rmNearest,nil,-1));
 end;
 procedure ConvertArcToCubicCurves(rx,ry,x_axis_rotation:TpvDouble;large_arc_flag,sweep_flag:TpvInt32;x,y:TpvDouble);
 var sin_th,cos_th,a00,a01,a10,a11,x0,y0,x1,y1,xc,yc,d,sfactor,sfactor_sq,
     th0,th1,th_arc,dx,dy,dx1,dy1,Pr1,Pr2,Px,Py,check:TpvDouble;
     i,n_segs:TpvInt32;
  procedure ProcessSegment(xc,yc,th0,th1,rx,ry,x_axis_rotation:TpvDouble);
  var sin_th,cos_th,a00,a01,a10,a11,x1,y1,x2,y2,x3,y3,t,th_half:TpvDouble;
      i:TpvInt32;
  begin
   sin_th:=sin(x_axis_rotation*deg2rad);
   cos_th:=cos(x_axis_rotation*deg2rad);
   a00:=cos_th*rx;
   a01:=(-sin_th)*ry;
   a10:=sin_th*rx;
   a11:=cos_th*ry;
   th_half:=0.5*(th1-th0);
   t:=(8.0/3.0)*sin(th_half*0.5)*sin(th_half*0.5)/sin(th_half);
   x1:=(xc+cos(th0))-(t*sin(th0));
   y1:=(yc+sin(th0))+(t*cos(th0));
   x3:=xc+cos(th1);
   y3:=yc+sin(th1);
   x2:=x3+(t*sin(th1));
   y2:=y3-(t*cos(th1));
   CubicCurveTo((a00*x1)+(a01*y1),(a10*x1)+(a11*y1),
                (a00*x2)+(a01*y2),(a10*x2)+(a11*y2),
                (a00*x3)+(a01*y3),(a10*x3)+(a11*y3));
  end;
 begin
  if (abs(rx)<1e-14) or (abs(ry)<1e-14) then begin
   LineTo(x,y);
   exit;
  end;
  sin_th:=sin(x_axis_rotation*deg2rad);
  cos_th:=cos(x_axis_rotation*deg2rad);
  dx:=(lx-x)*0.5;
  dy:=(ly-y)*0.5;
  dx1:=(cos_th*dx)+(sin_th*dy);
  dy1:=(cos_th*dy)-(sin_th*dx);
  Pr1:=sqr(rx);
  Pr2:=sqr(ry);
  Px:=sqr(dx1);
  Py:=sqr(dy1);
  check:=(Px/Pr1)+(Py/Pr2);
  if check>1.0 then begin
   rx:=rx*sqrt(check);
   ry:=ry*sqrt(check);
  end;
  a00:=cos_th/rx;
  a01:=sin_th/rx;
  a10:=(-sin_th)/ry;
  a11:=cos_th/ry;
  x0:=(a00*lx)+(a01*ly);
  y0:=(a10*lx)+(a11*ly);
  x1:=(a00*x)+(a01*y);
  y1:=(a10*x)+(a11*y);
  d:=sqr(x1-x0)+sqr(y1-y0);
  sfactor_sq:=(1.0/d)-0.25;
  if sfactor_sq<0.0 then begin
   sfactor_sq:=0.0;
  end;
  sfactor:=sqrt(sfactor_sq);
  if sweep_flag=large_arc_flag then begin
   sfactor:=-sfactor;
  end;
  xc:=(0.5*(x0+x1))-(sfactor*(y1-y0));
  yc:=(0.5*(y0+y1))+(sfactor*(x1-x0));
  th0:=arctan2(y0-yc,x0-xc);
  th1:=arctan2(y1-yc,x1-xc);
  th_arc:=th1-th0;
  if (th_arc<0.0) and (sweep_flag<>0) then begin
   th_arc:=th_arc+TwoPI;
  end else if (th_arc>0.0) and (sweep_flag=0) then begin
   th_arc:=th_arc-TwoPI;
  end;
  n_segs:=ceil(abs(th_arc/((pi*0.5)+0.001)));
  for i:=0 to n_segs-1 do begin
   ProcessSegment(xc,yc,
                  th0+((i*th_arc)/n_segs),
                  th0+(((i+1)*th_arc)/n_segs),
                  rx,ry,
                  x_axis_rotation);
  end;
  if n_segs=0 then begin
   LineTo(x,y);
  end;
 end;
begin
 Create;
 SrcLen:=length(aCommands);
 Src:=aCommands;
 for i:=1 to SrcLen do begin
  if Src[i] in [#0..#32,','] then begin
   Src[i]:=' ';
  end;
 end;
 lx:=0;
 ly:=0;
 lcx:=0;
 lcy:=0;
 lmx:=0;
 lmy:=0;
 SrcPos:=1;
 Command:=#0;
 LastCommand:=#0;
 while SrcPos<=SrcLen do begin
  SkipBlank;
  if SrcPos<=SrcLen then begin
   if Src[SrcPos] in ['A'..'Z','a'..'z'] then begin
    Command:=Src[SrcPos];
    inc(SrcPos);
    SkipBlank;
   end;
   case Command of
    'Z','z':begin
     Close;
     lx:=lmx;
     ly:=lmy;
    end;
    'H':begin
     lx:=GetFloat;
     LineTo(lx,ly);
    end;
    'h':begin
     lx:=lx+GetFloat;
     LineTo(lx,ly);
    end;
    'V':begin
     ly:=GetFloat;
     LineTo(lx,ly);
    end;
    'v':begin
     ly:=ly+GetFloat;
     LineTo(lx,ly);
    end;
    'M':begin
     lx:=GetFloat;
     ly:=GetFloat;
     lmx:=lx;
     lmy:=ly;
     MoveTo(lx,ly);
     Command:='L';
    end;
    'm':begin
     lx:=lx+GetFloat;
     ly:=ly+GetFloat;
     lmx:=lx;
     lmy:=ly;
     MoveTo(lx,ly);
     Command:='l';
    end;
    'L':begin
     lx:=GetFloat;
     ly:=GetFloat;
     LineTo(lx,ly);
    end;
    'l':begin
     lx:=lx+GetFloat;
     ly:=ly+GetFloat;
     LineTo(lx,ly);
    end;
    'A':begin
     rx:=GetFloat;
     ry:=GetFloat;
     x_axis_rotation:=GetFloat;
     large_arc_flag:=GetInt;
     sweep_flag:=GetInt;
     x:=GetFloat;
     y:=GetFloat;
     ConvertArcToCubicCurves(rx,ry,x_axis_rotation,large_arc_flag,sweep_flag,x,y);
     lx:=x;
     ly:=y;
    end;
    'a':begin
     rx:=GetFloat;
     ry:=GetFloat;
     x_axis_rotation:=GetFloat;
     large_arc_flag:=GetInt;
     sweep_flag:=GetInt;
     x:=lx+GetFloat;
     y:=ly+GetFloat;
     ConvertArcToCubicCurves(rx,ry,x_axis_rotation,large_arc_flag,sweep_flag,x,y);
     lx:=x;
     ly:=y;
    end;
    'T':begin
     lcx:=lx+(lx-lcx);
     lcy:=ly+(ly-lcy);
     if not (LastCommand in ['T','t','Q','q']) then begin
      lcx:=lx;
      lcy:=ly;
     end;
     lx:=GetFloat;
     ly:=GetFloat;
     QuadraticCurveTo(lcx,lcy,lx,ly);
    end;
    't':begin
     lcx:=lx+(lx-lcx);
     lcy:=ly+(ly-lcy);
     if not (LastCommand in ['T','t','Q','q']) then begin
      lcx:=lx;
      lcy:=ly;
     end;
     lx:=lx+GetFloat;
     ly:=ly+GetFloat;
     QuadraticCurveTo(lcx,lcy,lx,ly);
    end;
    'S':begin
     x0:=lx+(lx-lcx);
     y0:=ly+(ly-lcy);
     if not (LastCommand in ['S','s','C','c']) then begin
      x0:=lx;
      y0:=ly;
     end;
     x1:=GetFloat;
     y1:=GetFloat;
     x2:=GetFloat;
     y2:=GetFloat;
     lcx:=x1;
     lcy:=y1;
     lx:=x2;
     ly:=y2;
     CubicCurveTo(x0,y0,x1,y1,x2,y2);
    end;
    's':begin
     x0:=lx+(lx-lcx);
     y0:=ly+(ly-lcy);
     if not (LastCommand in ['S','s','C','c']) then begin
      x0:=lx;
      y0:=ly;
     end;
     x1:=lx+GetFloat;
     y1:=ly+GetFloat;
     x2:=lx+GetFloat;
     y2:=ly+GetFloat;
     lcx:=x1;
     lcy:=y1;
     lx:=x2;
     ly:=y2;
     CubicCurveTo(x0,y0,x1,y1,x2,y2);
    end;
    'Q':begin
     x0:=GetFloat;
     y0:=GetFloat;
     x1:=GetFloat;
     y1:=GetFloat;
     lcx:=x0;
     lcy:=y0;
     lx:=x1;
     ly:=y1;
     QuadraticCurveTo(x0,y0,x1,y1);
    end;
    'q':begin
     x0:=lx+GetFloat;
     y0:=ly+GetFloat;
     x1:=lx+GetFloat;
     y1:=ly+GetFloat;
     lcx:=x0;
     lcy:=y0;
     lx:=x1;
     ly:=y1;
     QuadraticCurveTo(x0,y0,x1,y1);
    end;
    'C':begin
     x0:=GetFloat;
     y0:=GetFloat;
     x1:=GetFloat;
     y1:=GetFloat;
     x2:=GetFloat;
     y2:=GetFloat;
     lcx:=x1;
     lcy:=y1;
     lx:=x2;
     ly:=y2;
     CubicCurveTo(x0,y0,x1,y1,x2,y2);
    end;
    'c':begin
     x0:=lx+GetFloat;
     y0:=ly+GetFloat;
     x1:=lx+GetFloat;
     y1:=ly+GetFloat;
     x2:=lx+GetFloat;
     y2:=ly+GetFloat;
     lcx:=x1;
     lcy:=y1;
     lx:=x2;
     ly:=y2;
     CubicCurveTo(x0,y0,x1,y1,x2,y2);
    end;
    else begin
     break;
    end;
   end;
  end else begin
   break;
  end;
 end;
end;

constructor TpvVectorPath.CreateFromShape(const aShape:TpvVectorPathShape);
begin
 Create;
 Assign(aShape);
end;

destructor TpvVectorPath.Destroy;
begin
 FreeAndNil(fCommands);
 inherited Destroy;
end;

procedure TpvVectorPath.Assign(const aFrom:TpvVectorPath);
var Index:TpvSizeInt;
    SrcCmd:TpvVectorPathCommand;
begin
 fCommands.Clear;
 for Index:=0 to aFrom.fCommands.Count-1 do begin
  SrcCmd:=aFrom.fCommands[Index];
  fCommands.Add(TpvVectorPathCommand.Create(SrcCmd.fCommandType,SrcCmd.fX0,SrcCmd.fY0,SrcCmd.fX1,SrcCmd.fY1,SrcCmd.fX2,SrcCmd.fY2));
 end;
 fFillRule:=aFrom.fFillRule;
end;

procedure TpvVectorPath.Assign(const aShape:TpvVectorPathShape);
var Contour:TpvVectorPathContour;
    Segment:TpvVectorPathSegment;
    First:boolean;
    Last,Start:TpvVectorPathVector;
begin
 fCommands.Clear;
 if assigned(aShape) then begin
  fFillRule:=aShape.fFillRule;
  First:=true;
  Last:=TpvVectorPathVector.Create(0.0,0.0);
  Start:=TpvVectorPathVector.Create(0.0,0.0);
  for Contour in aShape.fContours do begin
   for Segment in Contour.fSegments do begin
    case Segment.Type_ of
     TpvVectorPathSegmentType.Line:begin
      if First then begin
       First:=false;
       Start:=TpvVectorPathSegmentLine(Segment).Points[0];
       Last:=Start;
       fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.MoveTo,
                                                 TpvVectorPathSegmentLine(Segment).Points[0].x,TpvVectorPathSegmentLine(Segment).Points[0].y));
      end else if Last<>TpvVectorPathSegmentLine(Segment).Points[0] then begin
       fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.MoveTo,
                                                 TpvVectorPathSegmentLine(Segment).Points[0].x,TpvVectorPathSegmentLine(Segment).Points[0].y));
      end;
      fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.LineTo,
                                                TpvVectorPathSegmentLine(Segment).Points[1].x,TpvVectorPathSegmentLine(Segment).Points[1].y));
      Last:=TpvVectorPathSegmentLine(Segment).Points[1];
     end;
     TpvVectorPathSegmentType.QuadraticCurve:begin
      if First then begin
       First:=false;
       Start:=TpvVectorPathSegmentQuadraticCurve(Segment).Points[0];
       Last:=Start;
       fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.MoveTo,
                                                 TpvVectorPathSegmentQuadraticCurve(Segment).Points[0].x,TpvVectorPathSegmentQuadraticCurve(Segment).Points[0].y));
      end else if Last<>TpvVectorPathSegmentQuadraticCurve(Segment).Points[0] then begin
       fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.MoveTo,
                                                 TpvVectorPathSegmentQuadraticCurve(Segment).Points[0].x,TpvVectorPathSegmentQuadraticCurve(Segment).Points[0].y));
      end;
      fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.QuadraticCurveTo,
                                                TpvVectorPathSegmentQuadraticCurve(Segment).Points[1].x,TpvVectorPathSegmentQuadraticCurve(Segment).Points[1].y,
                                                TpvVectorPathSegmentQuadraticCurve(Segment).Points[2].x,TpvVectorPathSegmentQuadraticCurve(Segment).Points[2].y));
      Last:=TpvVectorPathSegmentQuadraticCurve(Segment).Points[2];
     end;
     TpvVectorPathSegmentType.CubicCurve:begin
      if First then begin
       First:=false;
       Start:=TpvVectorPathSegmentCubicCurve(Segment).Points[0];
       Last:=Start;
       fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.MoveTo,
                                                 TpvVectorPathSegmentCubicCurve(Segment).Points[0].x,TpvVectorPathSegmentCubicCurve(Segment).Points[0].y));
      end else if Last<>TpvVectorPathSegmentCubicCurve(Segment).Points[0] then begin
       fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.MoveTo,
                                                 TpvVectorPathSegmentCubicCurve(Segment).Points[0].x,TpvVectorPathSegmentCubicCurve(Segment).Points[0].y));
      end;
      fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.CubicCurveTo,
                                                TpvVectorPathSegmentCubicCurve(Segment).Points[1].x,TpvVectorPathSegmentCubicCurve(Segment).Points[1].y,
                                                TpvVectorPathSegmentCubicCurve(Segment).Points[2].x,TpvVectorPathSegmentCubicCurve(Segment).Points[2].y,
                                                TpvVectorPathSegmentCubicCurve(Segment).Points[3].x,TpvVectorPathSegmentCubicCurve(Segment).Points[3].y));
      Last:=TpvVectorPathSegmentCubicCurve(Segment).Points[3];
     end;
     else begin
     end;
    end;
   end;
   if (not First) and Contour.fClosed then begin
    fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.Close));
   end;
  end;
 end;
end;

procedure TpvVectorPath.MoveTo(const aX,aY:TpvDouble);
begin
 fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.MoveTo,aX,aY));
end;

procedure TpvVectorPath.LineTo(const aX,aY:TpvDouble);
begin
 fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.LineTo,aX,aY));
end;

procedure TpvVectorPath.QuadraticCurveTo(const aCX,aCY,aAX,aAY:TpvDouble);
begin
 fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.QuadraticCurveTo,aCX,aCY,aAX,aAY));
end;

procedure TpvVectorPath.CubicCurveTo(const aC0X,aC0Y,aC1X,aC1Y,aAX,aAY:TpvDouble);
begin
 fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.CubicCurveTo,aC0X,aC0Y,aC1X,aC1Y,aAX,aAY));
end;

procedure TpvVectorPath.Close;
begin
 fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.Close));
end;

function TpvVectorPath.GetShape:TpvVectorPathShape;
begin
 result:=TpvVectorPathShape.Create(self);
end;

end.
