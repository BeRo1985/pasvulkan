unit UnitFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Math;

const SDFSize=256;
  
type
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
    procedure DoIt;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses Vulkan,PasVulkan;

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

var DistanceField:array[0..1023,0..1023] of TVkUInt8;

procedure TFormMain.DoIt;
var GlyphIndex,CommandIndex,x0,y0,x1,y1,lastcx,lastcy,w,h:TVkInt32;
    VulkanTrueTypeFont:TVulkanTrueTypeFont;
    Stream:TStream;
    GlyphBuffer:TVulkanTrueTypeFontGlyphBuffer;
    PolygonBuffer:TVulkanTrueTypeFontPolygonBuffer;
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
 procedure GenerateSignedDistanceField(const Width,Height:TVkInt32);
 const DistanceFieldMagnitudeValue=4;
       DistanceFieldPadValue=4;
       Scalar1Value=1.0;
       CloseValue=Scalar1Value/16.0;
       CloseSquaredValue=CloseValue*CloseValue;
       NearlyZeroValue=Scalar1Value/int64(1 shl 18);
       TangentToleranceValue=Scalar1Value/int64(1 shl 11);
       ConicToleranceValue=0.25;
 type PSegmentSide=^TSegmentSide;
      TSegmentSide=
       (
        ssLeft=-1,
        ssOn=0,
        ssRight=1,
        ssNone=2
       );
      PDistanceFieldData=^TDistanceFieldData;
      TDistanceFieldData=record
       SquaredDistance:TVkFloat;
       DeltaWindingScore:TVkInt32;
      end;
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
      PPathSegmentPoints=^TPathSegmentPoints;
      TPathSegmentPoints=array[0..2] of TDoublePrecisionPoint;
      PPathSegment=^TPathSegment;
      TPathSegment=record
       Type_:TPathSegmentType;
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
      PPathSegmentArray=^TPathSegmentArray;
      TPathSegmentArray=record
       Segments:TPathSegments;
       Count:TVkInt32;
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
  function PathSegmentCountPoints(const PathSegment:TPathSegment):TVkInt32;
  begin
   case PathSegment.Type_ of
    pstLine:begin
     result:=2;
    end;
    else {pstQuadraticBezierCurve:}begin
     result:=3;
    end;
   end;
  end;
  function PathSegmentEndPoint(const PathSegment:TPathSegment):PDoublePrecisionPoint;
  begin
   case PathSegment.Type_ of
    pstLine:begin
     result:=@PathSegment.Points[1];
    end;
    else {pstQuad:}begin
     result:=@PathSegment.Points[2];
    end;
   end;
  end;
  procedure InitializePathSegment(var PathSegment:TPathSegment);
  var p0,p1,p2,p1mp0,t,m,sp0,sp1,sp2,p01p,p02p,p12p:TDoublePrecisionPoint;
      Hypotenuse,CosTheta,SinTheta,sa,a,sb,b,h,c,g,f,gd,fd,x,y,Lambda:TVkDouble;
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
     if ((p1mp0.x-p2.x)+p1.x)=0 then begin
      if ((p1mp0.x-p2.x)+p1.x)=0 then begin
      end;
     end;
     t.x:=Min(Max(p1mp0.x/((p1mp0.x-p2.x)+p1.x),1.0),0.0)*p1mp0.x;
     t.y:=Min(Max(p1mp0.y/((p1mp0.y-p2.y)+p1.y),1.0),0.0)*p1mp0.y;
     m.x:=p0.x+t.x;
     m.y:=p0.y+t.y;
     PathSegment.BoundingBox.Min.x:=Min(PathSegment.BoundingBox.Min.x,m.x);
     PathSegment.BoundingBox.Min.y:=Min(PathSegment.BoundingBox.Min.y,m.x);
     PathSegment.BoundingBox.Max.x:=Max(PathSegment.BoundingBox.Max.x,m.x);
     PathSegment.BoundingBox.Max.y:=Max(PathSegment.BoundingBox.Max.y,m.y);
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
     sa:=(p0.y-(2.0*p1.y))+p2.y;
     a:=sqr(sa);
     h:=-(((p0.y-(2.0*p1.y))+p2.y)*((p0.x-(2.0*p1.x))+p2.x));
     sb:=(p0.x-(2.0*p1.x))+p2.x;
     b:=sqr(sb);
     c:=((((((sp0.x*sp2.y)-(4.0*p01p.x*p12p.y))-(2.0*p02p.x*p02p.y))+(4.0*p02p.x*p1.y))+(4.0*sp1.x*p02p.y))-(4.0*p12p.x*p01p.y))+(sp2.x*sp0.y);
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
  procedure InitializeDistances(var Data:array of TDistanceFieldData);
  var Index:TVkInt32;
  begin
   for Index:=0 to length(Data)-1 do begin
    Data[Index].SquaredDistance:=sqr(DistanceFieldMagnitudeValue);
    Data[Index].DeltaWindingScore:=0;
   end;
  end;
  function AddLineToSegment(var PathSegmentArray:TPathSegmentArray;const Points:array of TDoublePrecisionPoint):TVkInt32;
  var PathSegment:PPathSegment;
  begin
   Assert(length(Points)=2);
   result:=PathSegmentArray.Count;
   inc(PathSegmentArray.Count);
   if length(PathSegmentArray.Segments)<=PathSegmentArray.Count then begin
    SetLength(PathSegmentArray.Segments,PathSegmentArray.Count*2);
   end;
   PathSegment:=@PathSegmentArray.Segments[result];
   PathSegment^.Type_:=pstLine;
   PathSegment^.Points[0]:=Points[0];
   PathSegment^.Points[1]:=Points[1];
   InitializePathSegment(PathSegment^);
  end;
  function AddQuadraticBezierCurveToSegment(var PathSegmentArray:TPathSegmentArray;const Points:array of TDoublePrecisionPoint):TVkInt32;
  var PathSegment:PPathSegment;
  begin
   Assert(length(Points)=3);
   result:=PathSegmentArray.Count;
   inc(PathSegmentArray.Count);
   if length(PathSegmentArray.Segments)<=PathSegmentArray.Count then begin
    SetLength(PathSegmentArray.Segments,PathSegmentArray.Count*2);
   end;
   PathSegment:=@PathSegmentArray.Segments[result];
   if (DoublePrecisionPointDistanceSquared(Points[0],Points[1])<CloseSquaredValue) or
      (DoublePrecisionPointDistanceSquared(Points[1],Points[2])<CloseSquaredValue) or
      IsColinear(Points) or
      IsZero((Points[1].x+((Points[1].x-Points[0].x)-Points[2].x))) or
      IsZero((Points[1].y+((Points[1].y-Points[0].y)-Points[2].y))) then begin
    PathSegment^.Type_:=pstLine;
    PathSegment^.Points[0]:=Points[0];
    PathSegment^.Points[1]:=Points[2];
   end else begin
    PathSegment^.Type_:=pstQuadraticBezierCurve;
    PathSegment^.Points[0]:=Points[0];
    PathSegment^.Points[1]:=Points[1];
    PathSegment^.Points[2]:=Points[2];
   end;
   InitializePathSegment(PathSegment^);
  end;
  function AddCubicBezierCurveToSegment(var PathSegmentArray:TPathSegmentArray;const Points:array of TDoublePrecisionPoint):TVkInt32;
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
    AddLineToSegment(PathSegmentArray,[LastPoint,p]);
    LastPoint:=p;
   end;
   procedure CurveTo(const p0,p1:TDoublePrecisionPoint);
   begin
    AddQuadraticBezierCurveToSegment(PathSegmentArray,[LastPoint,p0,p1]);
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
   function GetCubicPt(c0,c1,c2,c3,t:TVkDouble):TVkDouble;
   var ts,g,b,a:TVkDouble;
   begin
    ts:=t*t;
    g:=3*(c1-c0);
    b:=(3*(c2-c1))-g;
    a:=((c3-c0)-b)-g;
    result:=(a*ts*t)+(b*ts)+(g*t)+c0;
   end;
   function GetCubicDerivative(c0,c1,c2,c3,t:TVkDouble):TVkDouble;
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
    P.x:=getCubicPt(P0.x,P1.x,P2.x,P3.x,t);
    P.y:=getCubicPt(P0.y,P1.y,P2.y,P3.y,t);

    // Calculates the tangent values of the cubic bezier at t
    V.x:=getCubicDerivative(P0.x,P1.x,P2.x,P3.x,t);
	  V.y:=getCubicDerivative(P0.y,P1.y,P2.y,P3.y,t);

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
   result:=PathSegmentArray.Count;
   CubicCurveToTangent(Points[0],Points[1],Points[2],Points[3]);
  end;
  Function CubeRoot(Value:TVkDouble):TVkDouble;
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
    result:=CubeRoot((b*(-0.5))+SqrtC)+CubeRoot((b*(-0.5))-SqrtC);
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
         SameValue(PathSegment.Points[1].y,PointLeft.y)) and
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
  function CalculateSideOfQuadraticBezierCurve(const PathSegment:TPathSegment;const Point,XFormPoint:TDoublePrecisionPoint;const RowData:TRowData):TSegmentSide;
  var p0,p1:TVkDouble;
      sp0,sp1:TVkInt32;
      ip0,ip1:boolean;
  begin
   case RowData.IntersectionType of
    rditVerticalLine:begin
     result:=TSegmentSide(TVkInt32(SignOf(XFormPoint.y-RowData.YAtIntersection)*RowData.QuadraticXDirection));
    end;
    rditTwoPointsIntersect:begin
     result:=ssNone;
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
      result:=TSegmentSide(TVkInt32(sp0*RowData.QuadraticXDirection));
     end;
     if ip1 and BetweenClosed(p1,PathSegment.P0T.x,PathSegment.P2T.x,PathSegment.NearlyZeroScaled,true) then begin
      sp1:=SignOf(p1-XFormPoint.x);
      if (result=ssNone) or (sp1=1) then begin
       result:=TSegmentSide(TVkInt32(-sp1*RowData.QuadraticXDirection));
      end;             
     end;
    end;
    rditTangentLine:begin
     result:=ssNone;
     if RowData.ScanlineXDirection=1 then begin
      if SameValue(PathSegment.Points[0].y,Point.y) then begin
       result:=TSegmentSide(TVkInt32(SignOf(RowData.XAtIntersection[0]-XFormPoint.x)));
      end else if SameValue(PathSegment.Points[0].y,Point.y) then begin
       result:=TSegmentSide(TVkInt32(SignOf(XFormPoint.x-RowData.XAtIntersection[0])));
      end;
     end;
    end;
    else begin
     result:=ssNone;
    end;
   end;
  end;
  function DistanceToPathSegment(const Point:TDoublePrecisionPoint;const PathSegment:TPathSegment;const RowData:TRowData;out SegmentSide:TSegmentSide):TVkDouble;
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
      SegmentSide:=TSegmentSide(TVkInt32(SignOf(XFormPoint.y)));
     end else begin
      SegmentSide:=ssNone;
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
      SegmentSide:=CalculateSideOfQuadraticBezierCurve(PathSegment,Point,XFormPoint,RowData);
     end else begin
      SegmentSide:=ssNone;
     end;
    end;
    else begin
     SegmentSide:=ssNone;
     result:=0.0;
    end;
   end;
  end;
  procedure CalculateDistanceFieldData(const PathSegmentArray:TPathSegmentArray;var DistanceFieldData:array of TDistanceFieldData;const Width,Height:TVkInt32);
  var PathSegmentIndex,x0,y0,x1,y1,x,y,PixelIndex,Dilation,DeltaWindingScore:TVkInt32;
      PathSegment:PPathSegment;
      PathSegmentBoundingBox:TBoundingBox;
      PreviousSegmentSide,SegmentSide:TSegmentSide;
      RowData:TRowData;
      PointLeft,PointRight,Point:TDoublePrecisionPoint;
      pX,pY,SquaredDistance,CurrentSquaredDistance:TvkDouble;
  begin
   for PathSegmentIndex:=0 to PathSegmentArray.Count-1 do begin
    PathSegment:=@PathSegmentArray.Segments[PathSegmentIndex];
    PathSegmentBoundingBox.Min.x:=PathSegment.BoundingBox.Min.x-DistanceFieldPadValue;
    PathSegmentBoundingBox.Min.y:=PathSegment.BoundingBox.Min.y-DistanceFieldPadValue;
    PathSegmentBoundingBox.Max.x:=PathSegment.BoundingBox.Max.x+DistanceFieldPadValue;
    PathSegmentBoundingBox.Max.y:=PathSegment.BoundingBox.Max.y+DistanceFieldPadValue;
    x0:=Min(Max(Trunc(Floor(PathSegmentBoundingBox.Min.x)),0),Width-1);
    y0:=Min(Max(Trunc(Floor(PathSegmentBoundingBox.Min.y)),0),Height-1);
    x1:=Min(Max(Trunc(Ceil(PathSegmentBoundingBox.Max.x)),0),Width-1);
    y1:=Min(Max(Trunc(Ceil(PathSegmentBoundingBox.Max.y)),0),Height-1);
    for y:=y0 to y1 do begin
     PreviousSegmentSide:=ssNone;
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
      SquaredDistance:=DistanceFieldData[PixelIndex].SquaredDistance;
      if SquaredDistance<sqr(1.5) then begin
       Dilation:=1
      end else if SquaredDistance<sqr(2.5) then begin
       Dilation:=2
      end else if SquaredDistance<sqr(3.5) then begin
       Dilation:=3
      end else begin
       Dilation:=DistanceFieldPadValue;
      end;
      Dilation:=Min(Dilation,DistanceFieldPadValue);
      PathSegmentBoundingBox.Min.x:=Floor(PathSegment.BoundingBox.Min.x)-DistanceFieldPadValue;
      PathSegmentBoundingBox.Min.y:=Floor(PathSegment.BoundingBox.Min.y)-DistanceFieldPadValue;
      PathSegmentBoundingBox.Max.x:=Ceil(PathSegment.BoundingBox.Max.x)+DistanceFieldPadValue;
      PathSegmentBoundingBox.Max.y:=Ceil(PathSegment.BoundingBox.Max.y)+DistanceFieldPadValue;
      if (Dilation<>DistanceFieldPadValue) and not
         (((x>=PathSegmentBoundingBox.Min.x) and (x<=PathSegmentBoundingBox.Max.x)) and
          ((y>=PathSegmentBoundingBox.Min.y) and (y<=PathSegmentBoundingBox.Max.y))) then begin
       continue;
      end else begin
       SegmentSide:=ssNone;
       CurrentSquaredDistance:=DistanceToPathSegment(Point,PathSegment^,RowData,SegmentSide);
       if (PreviousSegmentSide=ssLeft) and (SegmentSide=ssRight) then begin
        DeltaWindingScore:=-1;
       end else if (PreviousSegmentSide=ssRight) and (SegmentSide=ssLeft) then begin
        DeltaWindingScore:=1;
       end else begin
        DeltaWindingScore:=0;
       end;
       PreviousSegmentSide:=SegmentSide;
       if CurrentSquaredDistance<SquaredDistance then begin
        DistanceFieldData[PixelIndex].SquaredDistance:=CurrentSquaredDistance;
       end;
       inc(DistanceFieldData[PixelIndex].DeltaWindingScore,DeltaWindingScore);
      end;
     end;
    end;
   end;
  end;
  function PackDistanceFieldValue(Distance:TVkDouble):TVkUInt8;
  begin
   result:=Min(Max(round((Distance*(128.0/DistanceFieldMagnitudeValue))+128.0),0),255);
  end;
  procedure QuadraticCurveTo(var PathSegmentArray:TPathSegmentArray;sx,sy,cx,cy,ax,ay:TVkDouble;Tolerance:TVkDouble=1.0/256.0;MaxLevel:TVkInt32=32);
  var lx,ly:TVkDouble;
   procedure LineToPointAt(ax,ay:TVkDouble);
   var p0,p1:TDoublePrecisionPoint;
   begin
    p0.x:=lx;
    p0.y:=ly;
    p1.x:=ax;
    p1.y:=ay;
    AddLineToSegment(PathSegmentArray,[p0,p1]);
    lx:=ax;
    ly:=ay;
   end;
   procedure Recursive(x1,y1,x2,y2,x3,y3:TVkDouble;level:TVkInt32);
   var x12,y12,x23,y23,x123,y123,mx,my,d:TVkDouble;
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
    if (level>MaxLevel) or (d<Tolerance) then begin
     LineToPointAt(x123,y123);
    end else begin
     Recursive(x1,y1,x12,y12,x123,y123,level+1);
     Recursive(x123,y123,x23,y23,x3,y3,level+1);
    end;
   end;
  begin
   lx:=sx;
   ly:=sy;
   Recursive(lx,ly,cx,cy,ax,ay,0);
   LineToPointAt(ax,ay);
  end;
 var CommandIndex,x,y,x0,x1,y0,y1,PixelIndex,DistanceFieldSign,WindingNumber:TVkInt32;
     PathSegmentArray:TPathSegmentArray;
     DistanceFieldData:array of TDistanceFieldData;
     StartPoint,LastPoint,ControlPoint,Point:TDoublePrecisionPoint;
     sx,sy,ox,oy:double;
 begin
  PathSegmentArray.Segments:=nil;
  PathSegmentArray.Count:=0;
  try
   DistanceFieldData:=nil;
   try
    SetLength(DistanceFieldData,Width*Height);
    InitializeDistances(DistanceFieldData);
    StartPoint.x:=0.0;
    StartPoint.y:=0.0;
    LastPoint.x:=0.0;
    LastPoint.y:=0.0;
    VulkanTrueTypeFont.GetPolygonBufferBounds(PolygonBuffer,x0,y0,x1,y1);
    sx:=(SDFSize*0.75)/(x1-x0);
    sy:=(SDFSize*0.75)/(y1-y0);
    ox:=(SDFSize*0.125)-(x0*sx);
    oy:=(SDFSize*0.125)-(y0*sy);
    for CommandIndex:=0 to PolygonBuffer.CountCommands-1 do begin
     case PolygonBuffer.Commands[CommandIndex].CommandType of
      VkTTF_PolygonCommandType_MOVETO:begin
       LastPoint.x:=(PolygonBuffer.Commands[CommandIndex].Points[0].x*sx)+ox;
       LastPoint.y:=(PolygonBuffer.Commands[CommandIndex].Points[0].y*sy)+oy;
       StartPoint:=LastPoint;
      end;
      VkTTF_PolygonCommandType_LINETO:begin
       Point.x:=(PolygonBuffer.Commands[CommandIndex].Points[0].x*sx)+ox;
       Point.y:=(PolygonBuffer.Commands[CommandIndex].Points[0].y*sy)+oy;
       if not (SameValue(LastPoint.x,Point.x) and SameValue(LastPoint.y,Point.y)) then begin
        AddLineToSegment(PathSegmentArray,[LastPoint,Point]);
       end;
       LastPoint:=Point;
      end;
      VkTTF_PolygonCommandType_CURVETO:begin
       ControlPoint.x:=(PolygonBuffer.Commands[CommandIndex].Points[0].x*sx)+ox;
       ControlPoint.y:=(PolygonBuffer.Commands[CommandIndex].Points[0].y*sy)+oy;
       Point.x:=(PolygonBuffer.Commands[CommandIndex].Points[1].x*sx)+ox;
       Point.y:=(PolygonBuffer.Commands[CommandIndex].Points[1].y*sy)+oy;
       QuadraticCurveTo(PathSegmentArray,LastPoint.x,LastPoint.y,ControlPoint.x,ControlPoint.y,Point.x,Point.y);
//       AddQuadraticBezierCurveToSegment(PathSegmentArray,[LastPoint,ControlPoint,Point]);
//       AddLineToSegment(PathSegmentArray,[LastPoint,Point]);
       LastPoint:=Point;
      end;
      VkTTF_PolygonCommandType_CLOSE:begin
       if not (SameValue(LastPoint.x,StartPoint.x) and SameValue(LastPoint.y,StartPoint.y)) then begin
        AddLineToSegment(PathSegmentArray,[LastPoint,StartPoint]);
       end;
      end;
     end;
    end;
    CalculateDistanceFieldData(PathSegmentArray,DistanceFieldData,Width,Height);
    for y:=0 to Height-1 do begin
     WindingNumber:=0;
     for x:=0 to Width-1 do begin
      PixelIndex:=(y*Width)+x;
      inc(WindingNumber,DistanceFieldData[PixelIndex].DeltaWindingScore);
      if WindingNumber<>0 then begin
       DistanceFieldSign:=-1;
      end else begin
       DistanceFieldSign:=1;
      end;
      if (x=(Width-1)) and (WindingNumber<>0) then begin
       for x1:=0 to Width-1 do begin
        PixelIndex:=(y*Width)+x1;
        DistanceFieldSign:=1;
        DistanceField[y,x1]:=PackDistanceFieldValue(sqrt(DistanceFieldData[PixelIndex].SquaredDistance)*DistanceFieldSign);
       end;
      end else begin
       DistanceField[y,x]:=PackDistanceFieldValue(sqrt(DistanceFieldData[PixelIndex].SquaredDistance)*DistanceFieldSign);
      end;
     end;
    end;
   finally
    DistanceFieldData:=nil;
   end;
  finally
   PathSegmentArray.Segments:=nil;
  end;
 end;
var sx,sy,x,y:TVkInt32;
    p:PVKUInt8;
begin
 Stream:=TFileStream.Create('droidsans.ttf',fmOpenRead or fmShareDenyNone);
 try
  VulkanTrueTypeFont:=TVulkanTrueTypeFont.Create(Stream);
 finally
  Stream.Free;
 end;
 try
  SetExceptionMask([exInvalidOp,exDenormalized,exOverflow,exUnderflow,exPrecision]);
  if VulkanTrueTypeFont.NumGlyphs>0 then begin

   GlyphIndex:=VulkanTrueTypeFont.GetGlyphIndex(TVkUInt8(TVkChar('B')));

   VulkanTrueTypeFont.ResetGlyphBuffer(GlyphBuffer);
   VulkanTrueTypeFont.FillGlyphBuffer(GlyphBuffer,GlyphIndex);

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

   GenerateSignedDistanceField(SDFSize,SDFSize);

   ImageSDF.Width:=SDFSize;
   ImageSDF.Height:=SDFSize;
   ImageSDF.Picture.Bitmap.Width:=SDFSize;
   ImageSDF.Picture.Bitmap.Height:=SDFSize;
   ImageSDF.Picture.Bitmap.PixelFormat:=pf32Bit;
   ImageSDF.Picture.Bitmap.HandleType:=bmDIB;
   for y:=0 to ImageSDF.Picture.Bitmap.Height-1 do begin
    p:=ImageSDF.Picture.Bitmap.ScanLine[y];
    for x:=0 to ImageSDF.Picture.Bitmap.Width-1 do begin
     p^:=DistanceField[y,x];
     inc(p);
     p^:=DistanceField[y,x];
     inc(p);
     p^:=DistanceField[y,x];
     inc(p);
     p^:=0;
     inc(p);
    end;
   end;

  end;
 finally
  VulkanTrueTypeFont.Free;
 end;
end;

end.
