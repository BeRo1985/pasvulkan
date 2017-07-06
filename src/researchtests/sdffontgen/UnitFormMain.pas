unit UnitFormMain;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Math, fpImage, GraphType, intfgraphics, Vulkan, PasVulkan;

const SDFSize=256;
  
type PDistanceFieldPixel=^TDistanceFieldPixel;
     TDistanceFieldPixel=record
      r,g,b,a:byte;
     end;

     TDistanceFieldPixels=array of TDistanceFieldPixel;

     PDistanceField=^TDistanceField;
     TDistanceField=record
      Width:TVkInt32;
      Height:TVkInt32;
      Pixels:TDistanceFieldPixels;
     end;

  TFormMain = class(TForm)
    ImagePreview: TImage;
    ImageSDF: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    DistanceField:TDistanceField;
    procedure DoIt;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{$ifndef HasSAR}
function SARLongint(Value,Shift:TVkInt32):TVkInt32;
{$ifdef cpu386}
{$ifdef fpc} assembler; register; //inline;
asm
 mov ecx,edx
 sar eax,cl
end;// ['eax','edx','ecx'];
{$else} assembler; register;
asm
 mov ecx,edx
 sar eax,cl
end;
{$endif}
{$else}
{$ifdef cpuarm} assembler; //inline;
asm
 mov r0,r0,asr R1
end;// ['r0','R1'];
{$else}{$ifdef CAN_INLINE}inline;{$endif}
begin
 Shift:=Shift and 31;
 result:=(TVkUInt32(Value) shr Shift) or (TVkUInt32(TVkInt32(TVkUInt32(0-TVkUInt32(TVkUInt32(Value) shr 31)) and TVkUInt32(0-TVkUInt32(ord(Shift<>0) and 1)))) shl (32-Shift));
end;
{$endif}
{$endif}
{$endif}

procedure TFormMain.FormCreate(Sender: TObject);
begin
 {}
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
 {}
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
 DoIt;
end;

procedure TFormMain.DoIt;
var VulkanTrueTypeFont:TVulkanTrueTypeFont;
    Stream:TStream;
    GlyphBuffer:TVulkanTrueTypeFontGlyphBuffer;
    PolygonBuffer:TVulkanTrueTypeFontPolygonBuffer;
    IntfImage:TLazIntfImage;
 procedure GenerateSignedDistanceField(const MultiChannel:boolean);
 const DistanceFieldSpreadValue=4;
       DistanceFieldMagnitudeValue=DistanceFieldSpreadValue;
       DistanceFieldPadValue=DistanceFieldSpreadValue;
       Scalar1Value=1.0;
       CloseValue=Scalar1Value/16.0;
       CloseSquaredValue=CloseValue*CloseValue;
       NearlyZeroValue=Scalar1Value/int64(1 shl 18);
       TangentToleranceValue=Scalar1Value/int64(1 shl 11);
       ConicToleranceValue=0.25;
       RasterizerToScreenScale=1.0/256.0;
       ScreenToRasterizerScale=256.0;
 type PPathSegmentSide=^TPathSegmentSide;
      TPathSegmentSide=
       (
        pssLeft=-1,
        pssOn=0,
        pssRight=1,
        pssNone=2
       );
      PDistanceFieldDataItem=^TDistanceFieldDataItem;
      TDistanceFieldDataItem=record
       SquaredDistance:TVkFloat;
       SquaredDistanceR:TVkFloat;
       SquaredDistanceG:TVkFloat;
       SquaredDistanceB:TVkFloat;
       PseudoSquaredDistanceR:TVkFloat;
       PseudoSquaredDistanceG:TVkFloat;
       PseudoSquaredDistanceB:TVkFloat;
       DeltaWindingScore:TVkInt32;
      end;
      TDistanceFieldData=array of TDistanceFieldDataItem;
      PDoublePrecisionPoint=^TDoublePrecisionPoint;
      TDoublePrecisionPoint=record
       x:TVkDouble;
       y:TVkDouble;
      end;
      PDoublePrecisionAffineMatrix=^TDoublePrecisionAffineMatrix;
      TDoublePrecisionAffineMatrix=array[0..5] of TVkDouble;
      PPathSegmentType=^TPathSegmentType;
      TPathSegmentType=
       (
        pstLine,
        pstQuadraticBezierCurve
       );
      PBoundingBox=^TBoundingBox;
      TBoundingBox=record
       Min:TDoublePrecisionPoint;
       Max:TDoublePrecisionPoint;
      end;
      PPathSegmentColor=^TPathSegmentColor;
      TPathSegmentColor=
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
      PPathSegmentPoints=^TPathSegmentPoints;
      TPathSegmentPoints=array[0..2] of TDoublePrecisionPoint;
      PPathSegment=^TPathSegment;
      TPathSegment=record
       Type_:TPathSegmentType;
       Color:TPathSegmentColor;
       Points:TPathSegmentPoints;
       P0T,P2T:TDoublePrecisionPoint;
       XFormMatrix:TDoublePrecisionAffineMatrix;
       ScalingFactor:TVkDouble;
       SquaredScalingFactor:TVkDouble;
       NearlyZeroScaled:TVkDouble;
       SquaredTangentToleranceScaled:TVkDouble;
       BoundingBox:TBoundingBox;
      end;
      TPathSegments=array of TPathSegment;
      PContour=^TContour;
      TContour=record
       PathSegments:TPathSegments;
       CountPathSegments:TVkInt32;
      end;
      TContours=array of TContour;
      PShape=^TShape;
      TShape=record
       Contours:TContours;
       CountContours:TVkInt32;
      end;
      PRowDataIntersectionType=^TRowDataIntersectionType;
      TRowDataIntersectionType=
       (
        rditNoIntersection,
        rditVerticalLine,
        rditTangentLine,
        rditTwoPointsIntersect
       );
      PRowData=^TRowData;
      TRowData=record
       IntersectionType:TRowDataIntersectionType;
       QuadraticXDirection:TVkInt32;
       ScanlineXDirection:TVkInt32;
       YAtIntersection:TVkFloat;
       XAtIntersection:array[0..1] of TVkFloat;
      end;
  const DoublePrecisionAffineMatrixIdentity:TDoublePrecisionAffineMatrix=(1.0,0.0,0.0,0.0,1.0,0.0);
  function Clamp(const Value,MinValue,MaxValue:TVkInt64):TVkInt64; overload;
  begin
   if Value<=MinValue then begin
    result:=MinValue;
   end else if Value>=MaxValue then begin
    result:=MaxValue;
   end else begin
    result:=Value;
   end;
  end;
  function Clamp(const Value,MinValue,MaxValue:TVkDouble):TVkDouble; overload;
  begin
   if Value<=MinValue then begin
    result:=MinValue;
   end else if Value>=MaxValue then begin
    result:=MaxValue;
   end else begin
    result:=Value;
   end;
  end;
  function DoublePrecisionPointLength(const p:TDoublePrecisionPoint):TVkDouble;
  begin
   result:=sqrt(sqr(p.x)+sqr(p.y));
  end;
  function DoublePrecisionPointDistance(const a,b:TDoublePrecisionPoint):TVkDouble;
  begin
   result:=sqrt(sqr(a.x-b.x)+sqr(a.y-b.y));
  end;
  function DoublePrecisionPointLengthSquared(const v:TDoublePrecisionPoint):TVkDouble;
  begin
   result:=sqr(v.x)+sqr(v.y);
  end;
  function DoublePrecisionPointDistanceSquared(const a,b:TDoublePrecisionPoint):TVkDouble;
  begin
   result:=sqr(a.x-b.x)+sqr(a.y-b.y);
  end;
  function DoublePrecisionPointCrossProduct(const a,b:TDoublePrecisionPoint):TVkDouble;
  begin
   result:=(a.x*b.y)-(a.y*b.x);
  end;
  function DoublePrecisionPointNormalize(const v:TDoublePrecisionPoint):TDoublePrecisionPoint;
  var f:TVkDouble;
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
  function DoublePrecisionPointLerp(const a,b:TDoublePrecisionPoint;const t:TVkDouble):TDoublePrecisionPoint;
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
  function DoublePrecisionPointMap(const p:TDoublePrecisionPoint;const m:TDoublePrecisionAffineMatrix):TDoublePrecisionPoint;
  begin
   result.x:=(p.x*m[0])+(p.y*m[1])+m[2];
   result.y:=(p.x*m[3])+(p.y*m[4])+m[5];
  end;
  function BetweenClosedOpen(const a,b,c:TVkDouble;const Tolerance:TVkDouble=0.0;const XFormToleranceToX:boolean=false):boolean;
  var ToleranceB,ToleranceC:TVkDouble;
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
  function BetweenClosed(const a,b,c:TVkDouble;const Tolerance:TVkDouble=0.0;const XFormToleranceToX:boolean=false):boolean;
  var ToleranceB,ToleranceC:TVkDouble;
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
  function NearlyZero(const Value:TVkDouble;const Tolerance:TVkDouble=NearlyZeroValue):boolean;
  begin
   Assert(Tolerance>=0.0);
   result:=abs(Value)<=Tolerance;
  end;
  function NearlyEqual(const x,y:TVkDouble;const Tolerance:TVkDouble=NearlyZeroValue;const XFormToleranceToX:boolean=false):boolean;
  begin
   Assert(Tolerance>=0.0);
   if XFormToleranceToX then begin
    result:=abs(x-y)<=(Tolerance/sqrt((sqr(y)*4.0)+1.0));
   end else begin
    result:=abs(x-y)<=Tolerance;
   end;
  end;
  function SignOf(const Value:TVkDouble):TVkInt32;
  begin
   if Value<0.0 then begin
    result:=-1;
   end else begin
    result:=1;
   end;
  end;
  function IsColinear(const Points:array of TDoublePrecisionPoint):boolean;
  begin
   Assert(length(Points)=3);
   result:=abs(((Points[1].y-Points[0].y)*(Points[1].x-Points[2].x))-
               ((Points[1].y-Points[2].y)*(Points[1].x-Points[0].x)))<=CloseSquaredValue;
  end;
  function PathSegmentDirection(const PathSegment:TPathSegment;const Which:TVkInt32):TDoublePrecisionPoint;
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
  function PathSegmentCountPoints(const PathSegment:TPathSegment):TVkInt32;
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
  function PathSegmentEndPoint(const PathSegment:TPathSegment):PDoublePrecisionPoint;
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
  function PathSegmentCornerPoint(const PathSegment:TPathSegment;const WhichA,WhichB:TVkInt32):PDoublePrecisionPoint;
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
  procedure InitializePathSegment(var PathSegment:TPathSegment);
  var p0,p1,p2,p1mp0,d,t,sp0,sp1,sp2,p01p,p02p,p12p:TDoublePrecisionPoint;
      Hypotenuse,CosTheta,SinTheta,a,b,h,c,g,f,gd,fd,x,y,Lambda:TVkDouble;
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
   PathSegment.NearlyZeroScaled:=NearlyZeroValue/PathSegment.ScalingFactor;
   PathSegment.SquaredTangentToleranceScaled:=sqr(TangentToleranceValue)/PathSegment.SquaredScalingFactor;
   PathSegment.P0T:=DoublePrecisionPointMap(p0,PathSegment.XFormMatrix);
   PathSegment.P2T:=DoublePrecisionPointMap(p2,PathSegment.XFormMatrix);
  end;
  procedure InitializeDistances(var Data:TDistanceFieldData);
  var Index:TVkInt32;
  begin
   for Index:=0 to length(Data)-1 do begin
    Data[Index].SquaredDistance:=sqr(DistanceFieldMagnitudeValue);
    Data[Index].SquaredDistanceR:=sqr(DistanceFieldMagnitudeValue);
    Data[Index].SquaredDistanceG:=sqr(DistanceFieldMagnitudeValue);
    Data[Index].SquaredDistanceB:=sqr(DistanceFieldMagnitudeValue);
    Data[Index].PseudoSquaredDistanceR:=sqr(DistanceFieldMagnitudeValue);
    Data[Index].PseudoSquaredDistanceG:=sqr(DistanceFieldMagnitudeValue);
    Data[Index].PseudoSquaredDistanceB:=sqr(DistanceFieldMagnitudeValue);
    Data[Index].DeltaWindingScore:=0;
   end;
  end;
  function AddLineToPathSegmentArray(var Contour:TContour;const Points:array of TDoublePrecisionPoint):TVkInt32;
  var PathSegment:PPathSegment;
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
  function AddQuadraticBezierCurveToPathSegmentArray(var Contour:TContour;const Points:array of TDoublePrecisionPoint):TVkInt32;
  var PathSegment:PPathSegment;
  begin
   Assert(length(Points)=3);
   result:=Contour.CountPathSegments;
   if (DoublePrecisionPointDistanceSquared(Points[0],Points[1])<CloseSquaredValue) or
      (DoublePrecisionPointDistanceSquared(Points[1],Points[2])<CloseSquaredValue) or
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
  function AddQuadraticBezierCurveAsSubdividedLinesToPathSegmentArray(var Contour:TContour;const Points:array of TDoublePrecisionPoint;const Tolerance:TVkDouble=RasterizerToScreenScale;const MaxLevel:TVkInt32=32):TVkInt32;
  var LastPoint:TDoublePrecisionPoint;
   procedure LineToPointAt(const Point:TDoublePrecisionPoint);
   begin
    AddLineToPathSegmentArray(Contour,[LastPoint,Point]);
    LastPoint:=Point;
   end;
   procedure Recursive(const x1,y1,x2,y2,x3,y3:TVkDouble;const Level:TVkInt32);
   var x12,y12,x23,y23,x123,y123,mx,my,d:TVkDouble;
       Point:TDoublePrecisionPoint;
   begin
    x12:=(x1+x2)*0.5;
    y12:=(y1+y2)*0.5;
    x23:=(x2+x3)*0.5;
    y23:=(y2+y3)*0.5;
    x123:=(x12+x23)*0.5;
    y123:=(y12+y23)*0.5;
    mx:=(x1+x3)*0.5;
    my:=(y1+y3)*0.5;
    d:=abs(mx-x123)+abs(my-y123);
    if (Level>MaxLevel) or (d<Tolerance) then begin
     Point.x:=x123;
     Point.y:=y123;
     LineToPointAt(Point);
    end else begin
     Recursive(x1,y1,x12,y12,x123,y123,level+1);
     Recursive(x123,y123,x23,y23,x3,y3,level+1);
    end;
   end;
  begin
   Assert(length(Points)=3);
   result:=Contour.CountPathSegments;
   LastPoint:=Points[0];
   Recursive(Points[0].x,Points[0].y,Points[1].x,Points[1].y,Points[2].x,Points[2].y,0);
   LineToPointAt(Points[2]);
  end;
  function AddCubicBezierCurveAsSubdividedQuadraticBezierCurvesToPathSegmentArray(var Contour:TContour;const Points:array of TDoublePrecisionPoint):TVkInt32;
  type TLine=record
        a,b,c:TVkDouble;
        Exist,Vertical:boolean;
       end;
       TPointLine=record
        p:TDoublePrecisionPoint;
        l:TLine;
       end;
  var LastPoint:TDoublePrecisionPoint;
   procedure MoveTo(const p:TDoublePrecisionPoint);
   begin
    LastPoint:=p;
   end;
   procedure LineTo(const p:TDoublePrecisionPoint);
   begin
    AddLineToPathSegmentArray(Contour,[LastPoint,p]);
    LastPoint:=p;
   end;
   procedure CurveTo(const p0,p1:TDoublePrecisionPoint);
   begin
    AddQuadraticBezierCurveToPathSegmentArray(Contour,[LastPoint,p0,p1]);
    LastPoint:=p1;
   end;
   function GetLine(const P0,P1:TDoublePrecisionPoint):TLine;
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
   function GetLine2(const P0,v0:TDoublePrecisionPoint):TLine;
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
   function GetLineCross(const l0,l1:TLine;var b:boolean):TDoublePrecisionPoint;
   var u:TVkDouble;
   begin

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
   function GetCubicPoint(const c0,c1,c2,c3,t:TVkDouble):TVkDouble;
   var ts,g,b,a:TVkDouble;
   begin
    ts:=t*t;
    g:=3*(c1-c0);
    b:=(3*(c2-c1))-g;
    a:=((c3-c0)-b)-g;
    result:=(a*ts*t)+(b*ts)+(g*t)+c0;
   end;
   function GetCubicDerivative(const c0,c1,c2,c3,t:TVkDouble):TVkDouble;
   var g,b,a:TVkDouble;
   begin
    g:=3*(c1-c0);
    b:=(3*(c2-c1))-g;
    a:=((c3-c0)-b)-g;
    result:=(3*a*t*t)+(2*b*t)+g;
   end;
   function GetCubicTangent(const P0,P1,P2,P3:TDoublePrecisionPoint;t:TVkDouble):TPointLine;
   var P,V:TDoublePrecisionPoint;
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
   procedure CubicCurveToTangent(const P0,P1,P2,P3:TDoublePrecisionPoint);
   const NumberOfSegments=8;
    function SliceCubicBezierSegment(const p0,p1,p2,p3:TDoublePrecisionPoint;const u1,u2:TVkDouble;const Tu1,Tu2:TPointLine;Recursion:TVkInt32):TVkInt32;
    var P,ControlPoint:TDoublePrecisionPoint;
        b:boolean;
        d,uMid:TVkDouble;
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
       TimeStep:TVkDouble;
       i:TVkInt32;
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
  function AddCubicBezierCurveAsSubdividedLinesToPathSegmentArray(var Contour:TContour;const Points:array of TDoublePrecisionPoint;const Tolerance:TVkDouble=RasterizerToScreenScale;const MaxLevel:TVkInt32=32):TVkInt32;
  var LastPoint:TDoublePrecisionPoint;
   procedure LineToPointAt(const Point:TDoublePrecisionPoint);
   begin
    AddLineToPathSegmentArray(Contour,[LastPoint,Point]);
    LastPoint:=Point;
   end;
   procedure Recursive(const x1,y1,x2,y2,x3,y3,x4,y4:TVkDouble;const Level:TVkInt32);
   var x12,y12,x23,y23,x34,y34,x123,y123,x234,y234,x1234,y1234,mx,my,d:TVkDouble;
       Point:TDoublePrecisionPoint;
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
    mx:=(x1+x4)*0.5;
    my:=(y1+y4)*0.5;
    d:=abs(mx-x1234)+abs(my-y1234);
    if (Level>MaxLevel) or (d<Tolerance) then begin
     Point.x:=x1234;
     Point.y:=y1234;
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
  function CubeRoot(Value:TVkDouble):TVkDouble;
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
  function CalculateNearestPointForQuadraticBezierCurve(const PathSegment:TPathSegment;const XFormPoint:TDoublePrecisionPoint):TVkDouble;
  const OneDiv3=1.0/3.0;
        OneDiv27=1.0/27.0;
  var a,b,a3,b2,c,SqrtC,CosPhi,Phi:TVkDouble;
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
  procedure PrecomputationForRow(out RowData:TRowData;const PathSegment:TPathSegment;const PointLeft,PointRight:TDoublePrecisionPoint);
  var XFormPointLeft,XFormPointRight:TDoublePrecisionPoint;
      x0,y0,x1,y1,m,b,m2,c,Tolerance,d:TVkDouble;
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
  function CalculateSideOfQuadraticBezierCurve(const PathSegment:TPathSegment;const Point,XFormPoint:TDoublePrecisionPoint;const RowData:TRowData):TPathSegmentSide;
  var p0,p1:TVkDouble;
      sp0,sp1:TVkInt32;
      ip0,ip1:boolean;
  begin
   case RowData.IntersectionType of
    rditVerticalLine:begin
     result:=TPathSegmentSide(TVkInt32(SignOf(XFormPoint.y-RowData.YAtIntersection)*RowData.QuadraticXDirection));
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
      result:=TPathSegmentSide(TVkInt32(sp0*RowData.QuadraticXDirection));
     end;
     if ip1 and BetweenClosed(p1,PathSegment.P0T.x,PathSegment.P2T.x,PathSegment.NearlyZeroScaled,true) then begin
      sp1:=SignOf(p1-XFormPoint.x);
      if (result=pssNone) or (sp1=1) then begin
       result:=TPathSegmentSide(TVkInt32(-sp1*RowData.QuadraticXDirection));
      end;
     end;
    end;
    rditTangentLine:begin
     result:=pssNone;
     if RowData.ScanlineXDirection=1 then begin
      if SameValue(PathSegment.Points[0].y,Point.y) then begin
       result:=TPathSegmentSide(TVkInt32(SignOf(RowData.XAtIntersection[0]-XFormPoint.x)));
      end else if SameValue(PathSegment.Points[2].y,Point.y) then begin
       result:=TPathSegmentSide(TVkInt32(SignOf(XFormPoint.x-RowData.XAtIntersection[0])));
      end;
     end;
    end;
    else begin
     result:=pssNone;
    end;
   end;
  end;
  function DistanceToPathSegment(const Point:TDoublePrecisionPoint;const PathSegment:TPathSegment;const RowData:TRowData;out PathSegmentSide:TPathSegmentSide):TVkDouble;
  var XFormPoint,x:TDoublePrecisionPoint;
      NearestPoint:TVkDouble;
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
      PathSegmentSide:=TPathSegmentSide(TVkInt32(SignOf(XFormPoint.y)));
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
 var OffsetX,OffsetY:TVkDouble;
     bx0,by0,bx1,by1:TVkInt32;
  procedure ConvertShape(out Shape:TShape;const DoSubdivideQuadraticBezierCubicIntoLines:boolean);
  var CommandIndex:TVkInt32;
      Contour:PContour;
      StartPoint,LastPoint,ControlPoint,Point:TDoublePrecisionPoint;
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
     for CommandIndex:=0 to PolygonBuffer.CountCommands-1 do begin
      case PolygonBuffer.Commands[CommandIndex].CommandType of
       VkTTF_PolygonCommandType_MOVETO:begin
        if assigned(Contour) then begin
         SetLength(Contour^.PathSegments,Contour^.CountPathSegments);
        end;
        if length(Shape.Contours)<(Shape.CountContours+1) then begin
         SetLength(Shape.Contours,(Shape.CountContours+1)*2);
        end;
        Contour:=@Shape.Contours[Shape.CountContours];
        inc(Shape.CountContours);
        LastPoint.x:=(PolygonBuffer.Commands[CommandIndex].Points[0].x*RasterizerToScreenScale)+OffsetX;
        LastPoint.y:=(PolygonBuffer.Commands[CommandIndex].Points[0].y*RasterizerToScreenScale)+OffsetY;
        StartPoint:=LastPoint;
       end;
       VkTTF_PolygonCommandType_LINETO:begin
        Point.x:=(PolygonBuffer.Commands[CommandIndex].Points[0].x*RasterizerToScreenScale)+OffsetX;
        Point.y:=(PolygonBuffer.Commands[CommandIndex].Points[0].y*RasterizerToScreenScale)+OffsetY;
        if assigned(Contour) and not (SameValue(LastPoint.x,Point.x) and SameValue(LastPoint.y,Point.y)) then begin
         AddLineToPathSegmentArray(Contour^,[LastPoint,Point]);
        end;
        LastPoint:=Point;
       end;
       VkTTF_PolygonCommandType_CURVETO:begin
        ControlPoint.x:=(PolygonBuffer.Commands[CommandIndex].Points[0].x*RasterizerToScreenScale)+OffsetX;
        ControlPoint.y:=(PolygonBuffer.Commands[CommandIndex].Points[0].y*RasterizerToScreenScale)+OffsetY;
        Point.x:=(PolygonBuffer.Commands[CommandIndex].Points[1].x*RasterizerToScreenScale)+OffsetX;
        Point.y:=(PolygonBuffer.Commands[CommandIndex].Points[1].y*RasterizerToScreenScale)+OffsetY;
        if assigned(Contour) and not ((SameValue(LastPoint.x,Point.x) and SameValue(LastPoint.y,Point.y)) and
                                      (SameValue(LastPoint.x,ControlPoint.x) and SameValue(LastPoint.y,ControlPoint.y))) then begin
         if DoSubdivideQuadraticBezierCubicIntoLines then begin
          AddQuadraticBezierCurveAsSubdividedLinesToPathSegmentArray(Contour^,[LastPoint,ControlPoint,Point]);
         end else begin
          AddQuadraticBezierCurveToPathSegmentArray(Contour^,[LastPoint,ControlPoint,Point]);
         end;
        end;
        LastPoint:=Point;
       end;
       VkTTF_PolygonCommandType_CLOSE:begin
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
  procedure SplitPathSegmentIntoThreePartsInsideContour(var Contour:TContour;const BasePathSegmentIndex:TVkInt32);
  var BasePathSegment:TPathSegment;
  begin
   if (BasePathSegmentIndex>=0) and (BasePathSegmentIndex<Contour.CountPathSegments) then begin
    BasePathSegment:=Contour.PathSegments[BasePathSegmentIndex];
    if BasePathSegment.Type_ in [pstLine,pstQuadraticBezierCurve] then begin
     inc(Contour.CountPathSegments,2);
     if length(Contour.PathSegments)<=Contour.CountPathSegments then begin
      SetLength(Contour.PathSegments,Contour.CountPathSegments*2);
     end;
     Move(Contour.PathSegments[BasePathSegmentIndex+1],Contour.PathSegments[BasePathSegmentIndex+3],(Contour.CountPathSegments-(BasePathSegmentIndex+3))*SizeOf(TPathSegment));
     FillChar(Contour.PathSegments[BasePathSegmentIndex],SizeOf(TPathSegment)*3,#0);
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
  procedure SplitPathSegmentIntoThreePartsToContour(var Contour:TContour;const BasePathSegmentIndex:TVkInt32;const BasePathSegment:TPathSegment);
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
  procedure NormalizeShape(var Shape:TShape);
  var ContourIndex:TVkInt32;
      Contour:PContour;
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
  procedure PathSegmentColorizeShape(var Shape:TShape);
  const AngleThreshold=3.0;
        EdgeThreshold=1.00000001;
  type PCorner=^TCorner;
       TCorner=TVkInt32;
       TCorners=array of TCorner;
  var ContourIndex,PathSegmentIndex,CountCorners,CornerIndex,SplineIndex,StartIndex,
      OtherPathSegmentIndex:TVkInt32;
      Seed:TVkUInt64;
      Contour:PContour;
      PathSegment:PPathSegment;
      Corners:TCorners;
      CurrentDirection,PreviousDirection,a,b:TDoublePrecisionPoint;
      CrossThreshold:TVkDouble;
      Color,InitialColor:TPathSegmentColor;
      Colors:array[0..2] of TPathSegmentColor;
      PathSegments:TPathSegments;
   procedure SwitchColor(var Color:TPathSegmentColor;const BannedColor:TPathSegmentColor=pscBlack);
   const StartColors:array[0..2] of TPathSegmentColor=(pscCyan,pscMagenta,pscYellow);
   var CombinedColor:TPathSegmentColor;
       Shifted:TVkUInt64;
   begin
    CombinedColor:=TPathSegmentColor(TVkInt32(TVkInt32(Color) and TVkInt32(BannedColor)));
    if CombinedColor in [pscRed,pscGreen,pscBlue] then begin
     Color:=TPathSegmentColor(TVkInt32(TVkInt32(CombinedColor) xor TVkInt32(TPathSegmentColor(pscWhite))));
    end else if CombinedColor in [pscBlack,pscWhite] then begin
     Color:=StartColors[Seed mod 3];
     Seed:=Seed div 3;
    end else begin
     Shifted:=TVkInt32(Color) shl (1+(Seed and 1));
     Color:=TPathSegmentColor(TVkInt32((Shifted or (Shifted shr 3)) and TVkInt32(TPathSegmentColor(pscWhite))));
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
          SwitchColor(Color,TPathSegmentColor(TVkInt32(IfThen(SplineIndex=(CountCorners-1),TVkInt32(InitialColor),TVkInt32(TPathSegmentColor(pscBlack))))));
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
  function GetLineNonClippedTime(const p,p0,p1:TDoublePrecisionPoint):TVkDouble;
  var pAP,pAB:TDoublePrecisionPoint;
  begin
   pAP.x:=p.x-p0.x;
   pAP.y:=p.y-p0.y;
   pAB.x:=p1.x-p0.x;
   pAB.y:=p1.y-p0.y;
   result:=((pAP.x*pAB.x)+(pAP.y*pAB.y))/(sqr(pAB.x)+sqr(pAB.y));
  end;
  function GetQuadraticBezierCurveNonClippedTime(const p,p0,p1,p2:TDoublePrecisionPoint):TVkDouble;
  var b0,b1,b2,d21,d10,d20,gf,pp,d0p:TDoublePrecisionPoint;
      a,b,d,f,ap,bp,v,c:TVkDouble;
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
  function GetNonClampedSignedLineDistance(const p,p0,p1:TDoublePrecisionPoint):TVkDouble;
  begin
   result:=sqr(((p.x*(p0.y-p1.y))+(p0.x*(p1.y-p.y))+(p1.x*(p.y-p0.y)))/sqrt(sqr(p1.x-p0.x)+sqr(p1.y-p0.y)));
  end;
  procedure CalculateDistanceFieldData(const Shape:TShape;var DistanceFieldData:TDistanceFieldData;const Width,Height:TVkInt32);
  var ContourIndex,PathSegmentIndex,x0,y0,x1,y1,x,y,PixelIndex,Dilation,DeltaWindingScore:TVkInt32;
      Contour:PContour;
      PathSegment:PPathSegment;
      PathSegmentBoundingBox:TBoundingBox;
      PreviousPathSegmentSide,PathSegmentSide:TPathSegmentSide;
      RowData:TRowData;
      DistanceFieldDataItem:PDistanceFieldDataItem;
      PointLeft,PointRight,Point,pAP,pAB:TDoublePrecisionPoint;
      pX,pY,CurrentSquaredDistance,CurrentSquaredPseudoDistance,Time,Value:TvkDouble;
  begin
   for ContourIndex:=0 to Shape.CountContours-1 do begin
    Contour:=@Shape.Contours[ContourIndex];
    for PathSegmentIndex:=0 to Contour^.CountPathSegments-1 do begin
     PathSegment:=@Contour^.PathSegments[PathSegmentIndex];
     PathSegmentBoundingBox.Min.x:=PathSegment.BoundingBox.Min.x-DistanceFieldPadValue;
     PathSegmentBoundingBox.Min.y:=PathSegment.BoundingBox.Min.y-DistanceFieldPadValue;
     PathSegmentBoundingBox.Max.x:=PathSegment.BoundingBox.Max.x+DistanceFieldPadValue;
     PathSegmentBoundingBox.Max.y:=PathSegment.BoundingBox.Max.y+DistanceFieldPadValue;
     x0:=Clamp(Trunc(Floor(PathSegmentBoundingBox.Min.x)),0,Width-1);
     y0:=Clamp(Trunc(Floor(PathSegmentBoundingBox.Min.y)),0,Height-1);
     x1:=Clamp(Trunc(Ceil(PathSegmentBoundingBox.Max.x)),0,Width-1);
     y1:=Clamp(Trunc(Ceil(PathSegmentBoundingBox.Max.y)),0,Height-1);
     for y:=y0 to y1 do begin
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
       PixelIndex:=(y*Width)+x;
       pX:=x+0.5;
       Point.x:=pX;
       Point.y:=pY;
       DistanceFieldDataItem:=@DistanceFieldData[PixelIndex];
       Dilation:=Clamp(Floor(sqrt(Max(1,DistanceFieldDataItem^.SquaredDistance))+0.5),1,DistanceFieldPadValue);
       PathSegmentBoundingBox.Min.x:=Floor(PathSegment.BoundingBox.Min.x)-DistanceFieldPadValue;
       PathSegmentBoundingBox.Min.y:=Floor(PathSegment.BoundingBox.Min.y)-DistanceFieldPadValue;
       PathSegmentBoundingBox.Max.x:=Ceil(PathSegment.BoundingBox.Max.x)+DistanceFieldPadValue;
       PathSegmentBoundingBox.Max.y:=Ceil(PathSegment.BoundingBox.Max.y)+DistanceFieldPadValue;
       if (Dilation<>DistanceFieldPadValue) and not
          (((x>=PathSegmentBoundingBox.Min.x) and (x<=PathSegmentBoundingBox.Max.x)) and
           ((y>=PathSegmentBoundingBox.Min.y) and (y<=PathSegmentBoundingBox.Max.y))) then begin
        continue;
       end else begin
        PathSegmentSide:=pssNone;
        CurrentSquaredDistance:=DistanceToPathSegment(Point,PathSegment^,RowData,PathSegmentSide);
        CurrentSquaredPseudoDistance:=CurrentSquaredDistance;
        if MultiChannel then begin
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
          pAB:=DoublePrecisionPointNormalize(PathSegmentDirection(PathSegment^,0));
          pAP.x:=Point.x-PathSegment^.Points[0].x;
          pAP.y:=Point.y-PathSegment^.Points[0].y;
          if ((pAP.x*pAB.x)+(pAP.y*pAB.y))<0.0 then begin
           Value:=((pAP.x*pAB.y)-(pAP.y*pAB.x));
           Value:=GetNonClampedSignedLineDistance(Point,PathSegmentCornerPoint(PathSegment^,0,0)^,PathSegmentCornerPoint(PathSegment^,0,1)^);
           if abs(Value)<=abs(CurrentSquaredPseudoDistance) then begin
            CurrentSquaredPseudoDistance:=abs(Value);
           end;
          end;
         end else if Time>=1.0 then begin
          pAB:=DoublePrecisionPointNormalize(PathSegmentDirection(PathSegment^,1));
          pAP.x:=Point.x-PathSegment^.Points[1].x;
          pAP.y:=Point.y-PathSegment^.Points[1].y;
          if ((pAP.x*pAB.x)+(pAP.y*pAB.y))>=0.0 then begin
           Value:=((pAP.x*pAB.y)-(pAP.y*pAB.x));
           Value:=GetNonClampedSignedLineDistance(Point,PathSegmentCornerPoint(PathSegment^,1,0)^,PathSegmentCornerPoint(PathSegment^,1,1)^);
           if abs(Value)<=abs(CurrentSquaredPseudoDistance) then begin
            CurrentSquaredPseudoDistance:=abs(Value);
           end;
          end;
         end;
        end;
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
         if (((TVKInt32(PathSegment^.Color) and TVkInt32(TPathSegmentColor(pscRed)))<>0)) and
            (CurrentSquaredDistance<DistanceFieldDataItem^.SquaredDistanceR) then begin
          DistanceFieldDataItem^.SquaredDistanceR:=CurrentSquaredDistance;
          DistanceFieldDataItem^.PseudoSquaredDistanceR:=CurrentSquaredPseudoDistance;
         end;
         if (((TVKInt32(PathSegment^.Color) and TVkInt32(TPathSegmentColor(pscGreen)))<>0)) and
            (CurrentSquaredDistance<DistanceFieldDataItem^.SquaredDistanceG) then begin
          DistanceFieldDataItem^.SquaredDistanceG:=CurrentSquaredDistance;
          DistanceFieldDataItem^.PseudoSquaredDistanceG:=CurrentSquaredPseudoDistance;
         end;
         if (((TVKInt32(PathSegment^.Color) and TVkInt32(TPathSegmentColor(pscBlue)))<>0)) and
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
  function PackDistanceFieldValue(Distance:TVkDouble):TVkUInt8;
  begin
   result:=Clamp(Round((Distance*(128.0/DistanceFieldMagnitudeValue))+128.0),0,255);
  end;
  function PackPseudoDistanceFieldValue(Distance:TVkDouble):TVkUInt8;
  begin
   result:=Clamp(Round((Distance*(128.0/DistanceFieldMagnitudeValue))+128.0),0,255);
  end;
  function GenerateDistanceFieldPicture(const DistanceFieldData:TDistanceFieldData;const Width,Height:TVkInt32):boolean;
  var x,y,PixelIndex,DistanceFieldSign,WindingNumber,Value:TVkInt32;
      DistanceFieldDataItem:PDistanceFieldDataItem;
      DistanceFieldPixel:PDistanceFieldPixel;
  begin

   result:=true;

   DistanceField.Width:=Width;
   DistanceField.Height:=Height;
   SetLength(DistanceField.Pixels,Width*Height);

   PixelIndex:=0;
   for y:=0 to Height-1 do begin
    WindingNumber:=0;
    for x:=0 to Width-1 do begin
     DistanceFieldDataItem:=@DistanceFieldData[PixelIndex];
     inc(WindingNumber,DistanceFieldDataItem^.DeltaWindingScore);
     if WindingNumber<>0 then begin
      DistanceFieldSign:=1;
     end else begin
      DistanceFieldSign:=-1;
     end;
     if (x=(Width-1)) and (WindingNumber<>0) then begin
      result:=false;
      break;
     end else begin
      DistanceFieldPixel:=@DistanceField.Pixels[PixelIndex];
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
    end;
    if not result then begin
     break;
    end;
   end;

  end;
 var Width,Height,TryIteration:TVkInt32;
     Shape:TShape;
     DistanceFieldData:TDistanceFieldData;
 begin
  Initialize(Shape);
  try

   DistanceFieldData:=nil;
   try

    VulkanTrueTypeFont.GetPolygonBufferBounds(PolygonBuffer,bx0,by0,bx1,by1);

    Width:=SARLongint((bx1-bx0)+255,8)+(DistanceFieldPadValue*4);
    Height:=SARLongint((by1-by0)+255,8)+(DistanceFieldPadValue*4);

    OffsetX:=(DistanceFieldPadValue*2)-(bx0*RasterizerToScreenScale);
    OffsetY:=(DistanceFieldPadValue*2)-(by0*RasterizerToScreenScale);

    SetLength(DistanceFieldData,Width*Height);

    for TryIteration:=0 to 1 do begin
                                                                    
     InitializeDistances(DistanceFieldData);
     
     ConvertShape(Shape,TryIteration=1);

     if MultiChannel then begin

      NormalizeShape(Shape);

      PathSegmentColorizeShape(Shape);

      NormalizeShape(Shape);

     end;

     CalculateDistanceFieldData(Shape,DistanceFieldData,Width,Height);

     if GenerateDistanceFieldPicture(DistanceFieldData,Width,Height) then begin
      break;
     end else begin
      // Try it again, after all quadratic bezier curves were subdivided into lines at the next try iteration
     end;

    end;

   finally
    DistanceFieldData:=nil;
   end;

  finally
   Finalize(Shape);
  end;

 end;
var GlyphIndex,CommandIndex,x0,y0,x1,y1,lastcx,lastcy,w,h:TVkInt32;
 procedure Scale(var x,y:TVkInt32);
 begin
  x:=(TVkInt64(x-x0)*ImagePreview.Width) div (x1-x0);
  y:=(TVkInt64(y-y0)*ImagePreview.Height) div (y1-y0);
 end;
 procedure MoveToPointAt(x,y:TVkInt32);
 begin
  lastcx:=x;
  lastcy:=y;
  Scale(x,y);
  ImagePreview.Canvas.MoveTo(x,y);
 end;
 procedure LineToPointAt(x,y:TVkInt32);
 begin
  lastcx:=x;
  lastcy:=y;
  Scale(x,y);
  ImagePreview.Canvas.LineTo(x,y);
 end;
 procedure QuadraticCurveTo(cx,cy,ax,ay:TVkInt32;Tolerance:TVkInt32=2;MaxLevel:TVkInt32=32);
  procedure Recursive(x1,y1,x2,y2,x3,y3,level:TVkInt32);
  var x12,y12,x23,y23,x123,y123,mx,my,d:TVkInt32;
  begin
   x12:=SARLongint(x1+x2,1);
   y12:=SARLongint(y1+y2,1);
   x23:=SARLongint(x2+x3,1);
   y23:=SARLongint(y2+y3,1);
   x123:=SARLongint(x12+x23,1);
   y123:=SARLongint(y12+y23,1);
   mx:=SARLongint(x1+x3,1);
   my:=SARLongint(y1+y3,1);
   d:=abs(mx-x123)+abs(my-y123);
   if (level>MaxLevel) or (d<Tolerance) then begin
    LineToPointAt(x123,y123);
   end else begin
    Recursive(x1,y1,x12,y12,x123,y123,level+1);
    Recursive(x123,y123,x23,y23,x3,y3,level+1);
   end;
  end;
 begin
  Recursive(lastcx,lastcy,cx,cy,ax,ay,0);
  LineToPointAt(ax,ay);
 end;
var sx,sy,x,y,i:TVkInt32;
    p:PVKUInt8;
    c:TFPColor;
begin
 Stream:=TFileStream.Create('droidsans.ttf',fmOpenRead or fmShareDenyNone);
 try
  VulkanTrueTypeFont:=TVulkanTrueTypeFont.Create(Stream);
 finally
  Stream.Free;
 end;
 try
  SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
  if VulkanTrueTypeFont.NumGlyphs>0 then begin

   GlyphIndex:=VulkanTrueTypeFont.GetGlyphIndex(TVkUInt8(TVkChar('G')));

   VulkanTrueTypeFont.Size:=-32;

   GlyphBuffer.Points:=nil;
   VulkanTrueTypeFont.ResetGlyphBuffer(GlyphBuffer);
   VulkanTrueTypeFont.FillGlyphBuffer(GlyphBuffer,GlyphIndex);

   PolygonBuffer.Commands:=nil;
   VulkanTrueTypeFont.ResetPolygonBuffer(PolygonBuffer);
   VulkanTrueTypeFont.FillPolygonBuffer(PolygonBuffer,GlyphBuffer);

   VulkanTrueTypeFont.GetPolygonBufferBounds(PolygonBuffer,x0,y0,x1,y1);

   w:=x1-x0;
   h:=y1-y0;
   dec(x0,(w*10) div 100);
   inc(x1,(w*10) div 100);
   dec(y0,(h*10) div 100);
   inc(y1,(h*10) div 100);

   ImagePreview.Canvas.Brush.Color:=clBlack;
   ImagePreview.Canvas.Brush.Style:=bsSolid;
   ImagePreview.Canvas.FillRect(ImagePreview.Canvas.ClipRect);

   ImagePreview.Canvas.Pen.Color:=clWhite;
   ImagePreview.Canvas.Pen.Style:=psSolid;

   sx:=0;
   sy:=0;

   for CommandIndex:=0 to PolygonBuffer.CountCommands-1 do begin
    case PolygonBuffer.Commands[CommandIndex].CommandType of
     VkTTF_PolygonCommandType_MOVETO:begin
      sx:=PolygonBuffer.Commands[CommandIndex].Points[0].x;
      sy:=PolygonBuffer.Commands[CommandIndex].Points[0].y;
      MoveToPointAt(PolygonBuffer.Commands[CommandIndex].Points[0].x,PolygonBuffer.Commands[CommandIndex].Points[0].y);
     end;
     VkTTF_PolygonCommandType_LINETO:begin
      LineToPointAt(PolygonBuffer.Commands[CommandIndex].Points[0].x,PolygonBuffer.Commands[CommandIndex].Points[0].y);
     end;
     VkTTF_PolygonCommandType_CURVETO:begin
      QuadraticCurveTo(PolygonBuffer.Commands[CommandIndex].Points[0].x,PolygonBuffer.Commands[CommandIndex].Points[0].y,
                       PolygonBuffer.Commands[CommandIndex].Points[1].x,PolygonBuffer.Commands[CommandIndex].Points[1].y,
                       2,32);
     end;
     VkTTF_PolygonCommandType_CLOSE:begin
      LineToPointAt(sx,sy);
     end;
    end;
   end;

   GenerateSignedDistanceField(true);

   ImageSDF.Width:=DistanceField.Width;
   ImageSDF.Height:=DistanceField.Height;
   ImageSDF.Picture.Bitmap.Width:=DistanceField.Width;
   ImageSDF.Picture.Bitmap.Height:=DistanceField.Height;
   ImageSDF.Picture.Bitmap.PixelFormat:=pf32Bit;
   ImageSDF.Picture.Bitmap.HandleType:=bmDIB;
   IntfImage:=ImageSDF.Picture.Bitmap.CreateIntfImage;
   if assigned(IntfImage) then begin
    try
     i:=0;
     for y:=0 to IntfImage.Height-1 do begin
      for x:=0 to IntfImage.Width-1 do begin
       c.red:=(DistanceField.Pixels[i].r shl 8) or DistanceField.Pixels[i].r;
       c.green:=(DistanceField.Pixels[i].g shl 8) or DistanceField.Pixels[i].g;
       c.blue:=(DistanceField.Pixels[i].b shl 8) or DistanceField.Pixels[i].b;
       c.alpha:=(DistanceField.Pixels[i].a shl 8) or DistanceField.Pixels[i].a;
       IntfImage.Colors[x,y]:=c;
       inc(i);
      end;
     end;
     ImageSDF.Picture.Bitmap.LoadFromIntfImage(IntfImage);
    finally
     IntfImage.Free;
    end;
    ImageSDF.Picture.SaveToFile('test.png');
   end;

  end;
 finally
  VulkanTrueTypeFont.Free;
 end;
end;

end.
