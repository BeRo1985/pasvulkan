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
unit PasVulkan.VectorPath.BooleanOperations;
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
     PasVulkan.VectorPath;

// Boolean operations on filled polygon sets: intersection, union, difference and exclusive-or.
//
// The work happens in four separate passes rather than in one combined sweep, because the one
// thing that makes a plane sweep hard to get right is doing geometry and bookkeeping at the same
// time: every edge split invalidates the winding numbers worked out a moment earlier. Keeping
// the passes apart costs one extra traversal and buys a result that is straightforward to reason
// about and to test.
//
//  1. Gather. Every contour of both inputs becomes a list of edges, with the coordinates snapped
//     onto a fine grid so that points which are meant to coincide actually do compare equal.
//
//  2. Split. Every pair of edges that could possibly cross is found through a uniform grid and
//     cut at the crossing, collinear overlaps included. Afterwards edges only ever meet at their
//     endpoints, which is the precondition everything below relies on.
//
//  3. Merge and wind. Edges lying exactly on top of each other merge into one, which keeps a
//     shared boundary from counting twice. Each remaining edge then gets the two winding numbers
//     on either side of it, and the chosen fill rules turn those into "inside here, outside
//     there"; the edge belongs to the result exactly when the two sides disagree.
//
//  4. Chain. The surviving edges are linked end to end into closed contours, wound so that the
//     non-zero rule fills precisely the computed area.

type EpvVectorPathBooleanOperation=class(Exception);

     PpvVectorPathBooleanOperationKind=^TpvVectorPathBooleanOperationKind;
     TpvVectorPathBooleanOperationKind=
      (
       Intersection=0,
       Union,
       Difference,
       ExclusiveOr
      );

     { TpvVectorPathBooleanOperation }

     TpvVectorPathBooleanOperation=class
      public
       const // The grid the coordinates get snapped onto, as a reciprocal. A 1/65536th of a unit
             // is far below anything a rasterizer can show, and still leaves a double plenty of
             // room before it starts losing the integer part.
             DefaultGridScale=65536.0;

             // How often the splitting pass may run. One pass finds every crossing; the extra
             // ones only catch the rare case where snapping a split point creates a new overlap.
             MaximumSplitPasses=4;
      private
       type { TEdge }
            TEdge=record
             public

              // Always ordered so that P0 comes before P1 in sweep order.
              P0:TpvVectorPathVector;
              P1:TpvVectorPathVector;

              // How much this edge adds to the winding number of its input. Both are kept
              // because merging coincident edges folds several of them into one.
              DeltaSubject:TpvInt32;
              DeltaClip:TpvInt32;

              InResult:boolean;

              // True when the contour runs from P0 to P1, false when it runs the other way.
              // This is what makes outer contours and holes wind against each other.
              Forward_:boolean;

              Used:boolean;
            end;
            PEdge=^TEdge;
            TEdges=array of TEdge;

            { TSplitPoint }
            TSplitPoint=record
             EdgeIndex:TpvSizeInt;
             Parameter:TpvDouble;
             Point:TpvVectorPathVector;
            end;
            TSplitPoints=array of TSplitPoint;

            TSizeIntArray=array of TpvSizeInt;
      private
       fEdges:TEdges;
       fCountEdges:TpvSizeInt;

       fSplitPoints:TSplitPoints;
       fCountSplitPoints:TpvSizeInt;

       // Column index over x, so that a downward ray only has to look at the edges whose x range
       // actually covers it.
       fColumnStart:TSizeIntArray;
       fColumnItems:TSizeIntArray;
       fCountColumns:TpvSizeInt;
       fColumnMinimumX:TpvDouble;
       fColumnSizeX:TpvDouble;

       fOperation:TpvVectorPathBooleanOperationKind;
       fSubjectFillRule:TpvVectorPathFillRule;
       fClipFillRule:TpvVectorPathFillRule;

       fGridScale:TpvDouble;
       fInverseGridScale:TpvDouble;

       function Snap(const aPoint:TpvVectorPathVector):TpvVectorPathVector;
       function SamePoint(const a,b:TpvVectorPathVector):boolean; {$ifdef CANINLINE}inline;{$endif}
       function ComparePoints(const a,b:TpvVectorPathVector):TpvInt32; {$ifdef CANINLINE}inline;{$endif}

       procedure AddEdge(const aFrom,aTo:TpvVectorPathVector;const aIsSubject:boolean);
       procedure AddShape(const aShape:TpvVectorPathShape;const aIsSubject:boolean);

       procedure AddSplitPoint(const aEdgeIndex:TpvSizeInt;const aParameter:TpvDouble;const aPoint:TpvVectorPathVector);
       function CollectIntersections(const aIndexA,aIndexB:TpvSizeInt):boolean;
       function SplitPass:boolean;

       procedure MergeCoincidentEdges;

       // The y value of an edge at a given x, only meaningful for a non-vertical edge.
       function EdgeYAt(const aEdgeIndex:TpvSizeInt;const aX:TpvDouble):TpvDouble;

       procedure BuildColumnIndex;

       // Winding numbers of both inputs at a point, counted by a ray straight down. A vertical
       // edge never crosses such a ray and therefore never contributes.
       procedure WindingAt(const aX,aY:TpvDouble;const aExcludeEdge:TpvSizeInt;
                           out aWindingSubject,aWindingClip:TpvInt32);

       procedure ComputeWindings;

       function IsInside(const aWinding:TpvInt32;const aFillRule:TpvVectorPathFillRule):boolean; {$ifdef CANINLINE}inline;{$endif}
       function CombineInside(const aSubjectInside,aClipInside:boolean):boolean; {$ifdef CANINLINE}inline;{$endif}

       procedure BuildResultContours(const aResult:TpvVectorPathShape);
      public
       constructor Create; reintroduce;
       destructor Destroy; override;

       // Runs the operation and returns a freshly created shape that the caller owns. Both
       // inputs must consist of line segments only, so the caller flattens any curves first.
       // Either input may be nil or empty.
       function Execute(const aSubject,aClip:TpvVectorPathShape;
                        const aOperation:TpvVectorPathBooleanOperationKind;
                        const aSubjectFillRule:TpvVectorPathFillRule=TpvVectorPathFillRule.NonZero;
                        const aClipFillRule:TpvVectorPathFillRule=TpvVectorPathFillRule.NonZero):TpvVectorPathShape;
      published

       // Coordinates are rounded to a multiple of one over this before anything else happens.
       // Larger means finer.
       property GridScale:TpvDouble read fGridScale write fGridScale;
     end;

// Convenience wrappers. Each returns a freshly created shape that the caller owns.
function pvVectorPathIntersection(const aSubject,aClip:TpvVectorPathShape;
                                  const aSubjectFillRule:TpvVectorPathFillRule=TpvVectorPathFillRule.NonZero;
                                  const aClipFillRule:TpvVectorPathFillRule=TpvVectorPathFillRule.NonZero):TpvVectorPathShape;

function pvVectorPathUnion(const aSubject,aClip:TpvVectorPathShape;
                           const aSubjectFillRule:TpvVectorPathFillRule=TpvVectorPathFillRule.NonZero;
                           const aClipFillRule:TpvVectorPathFillRule=TpvVectorPathFillRule.NonZero):TpvVectorPathShape;

function pvVectorPathDifference(const aSubject,aClip:TpvVectorPathShape;
                                const aSubjectFillRule:TpvVectorPathFillRule=TpvVectorPathFillRule.NonZero;
                                const aClipFillRule:TpvVectorPathFillRule=TpvVectorPathFillRule.NonZero):TpvVectorPathShape;

function pvVectorPathExclusiveOr(const aSubject,aClip:TpvVectorPathShape;
                                 const aSubjectFillRule:TpvVectorPathFillRule=TpvVectorPathFillRule.NonZero;
                                 const aClipFillRule:TpvVectorPathFillRule=TpvVectorPathFillRule.NonZero):TpvVectorPathShape;

// Rewrites a shape so that it describes the same filled area under the non-zero rule, with its
// self intersections resolved and its holes wound the other way. A union of the shape with
// nothing, which is what makes an even-odd shape usable where non-zero is expected.
function pvVectorPathNormalize(const aShape:TpvVectorPathShape;
                               const aFillRule:TpvVectorPathFillRule=TpvVectorPathFillRule.NonZero):TpvVectorPathShape;

implementation

const EPSILON=1e-9;

{ TpvVectorPathBooleanOperation }

constructor TpvVectorPathBooleanOperation.Create;
begin

 inherited Create;

 fEdges:=nil;
 fCountEdges:=0;

 fSplitPoints:=nil;
 fCountSplitPoints:=0;

 fColumnStart:=nil;
 fColumnItems:=nil;
 fCountColumns:=0;
 fColumnMinimumX:=0.0;
 fColumnSizeX:=1.0;

 fOperation:=TpvVectorPathBooleanOperationKind.Union;
 fSubjectFillRule:=TpvVectorPathFillRule.NonZero;
 fClipFillRule:=TpvVectorPathFillRule.NonZero;

 fGridScale:=DefaultGridScale;
 fInverseGridScale:=1.0/DefaultGridScale;

end;

destructor TpvVectorPathBooleanOperation.Destroy;
begin

 fEdges:=nil;
 fSplitPoints:=nil;
 fColumnStart:=nil;
 fColumnItems:=nil;

 inherited Destroy;

end;

function TpvVectorPathBooleanOperation.Snap(const aPoint:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=round(aPoint.x*fGridScale)*fInverseGridScale;
 result.y:=round(aPoint.y*fGridScale)*fInverseGridScale;
end;

function TpvVectorPathBooleanOperation.SamePoint(const a,b:TpvVectorPathVector):boolean;
begin
 result:=(a.x=b.x) and (a.y=b.y);
end;

function TpvVectorPathBooleanOperation.ComparePoints(const a,b:TpvVectorPathVector):TpvInt32;
begin

 // Lexicographic by x and then y, which is the order the sweep needs and which is exact because
 // the points have already been snapped to the grid.
 if a.x<b.x then begin
  result:=-1;
 end else if a.x>b.x then begin
  result:=1;
 end else if a.y<b.y then begin
  result:=-1;
 end else if a.y>b.y then begin
  result:=1;
 end else begin
  result:=0;
 end;

end;

procedure TpvVectorPathBooleanOperation.AddEdge(const aFrom,aTo:TpvVectorPathVector;const aIsSubject:boolean);
var FromPoint,ToPoint:TpvVectorPathVector;
    Edge:PEdge;
    Delta:TpvInt32;
begin

 FromPoint:=Snap(aFrom);
 ToPoint:=Snap(aTo);

 // A zero length edge encloses no area and would only confuse the ordering.
 if SamePoint(FromPoint,ToPoint) then begin
  exit;
 end;

 if fCountEdges>=Length(fEdges) then begin
  SetLength(fEdges,(fCountEdges+1)*2);
 end;
 Edge:=@fEdges[fCountEdges];
 inc(fCountEdges);

 // Stored left to right, with the delta remembering which way the original contour ran, so that
 // the winding numbers come out right either way.
 if ComparePoints(FromPoint,ToPoint)<0 then begin
  Edge^.P0:=FromPoint;
  Edge^.P1:=ToPoint;
  Delta:=1;
 end else begin
  Edge^.P0:=ToPoint;
  Edge^.P1:=FromPoint;
  Delta:=-1;
 end;

 if aIsSubject then begin
  Edge^.DeltaSubject:=Delta;
  Edge^.DeltaClip:=0;
 end else begin
  Edge^.DeltaSubject:=0;
  Edge^.DeltaClip:=Delta;
 end;

 Edge^.InResult:=false;
 Edge^.Forward_:=false;
 Edge^.Used:=false;

end;

procedure TpvVectorPathBooleanOperation.AddShape(const aShape:TpvVectorPathShape;const aIsSubject:boolean);
var Contour:TpvVectorPathContour;
    Segment:TpvVectorPathSegment;
    SegmentLine:TpvVectorPathSegmentLine;
    First,Current:TpvVectorPathVector;
    HasFirst:boolean;
begin

 if not assigned(aShape) then begin
  exit;
 end;

 for Contour in aShape.Contours do begin

  if not assigned(Contour) then begin
   continue;
  end;

  HasFirst:=false;
  First:=TpvVectorPathVector.Create(0.0,0.0);
  Current:=First;

  for Segment in Contour.Segments do begin
   if Segment is TpvVectorPathSegmentLine then begin
    SegmentLine:=TpvVectorPathSegmentLine(Segment);
    if not HasFirst then begin
     HasFirst:=true;
     First:=SegmentLine.Points[0];
    end;
    AddEdge(SegmentLine.Points[0],SegmentLine.Points[1],aIsSubject);
    Current:=SegmentLine.Points[1];
   end;
  end;

  // A filled area is only defined for a closed contour, so one left open gets closed here.
  if HasFirst and not SamePoint(Snap(Current),Snap(First)) then begin
   AddEdge(Current,First,aIsSubject);
  end;

 end;

end;

procedure TpvVectorPathBooleanOperation.AddSplitPoint(const aEdgeIndex:TpvSizeInt;const aParameter:TpvDouble;const aPoint:TpvVectorPathVector);
begin

 if fCountSplitPoints>=Length(fSplitPoints) then begin
  SetLength(fSplitPoints,(fCountSplitPoints+1)*2);
 end;

 fSplitPoints[fCountSplitPoints].EdgeIndex:=aEdgeIndex;
 fSplitPoints[fCountSplitPoints].Parameter:=aParameter;
 fSplitPoints[fCountSplitPoints].Point:=aPoint;
 inc(fCountSplitPoints);

end;

function TpvVectorPathBooleanOperation.CollectIntersections(const aIndexA,aIndexB:TpvSizeInt):boolean;
var EdgeA,EdgeB:PEdge;
    DirectionA,DirectionB,Delta,Point:TpvVectorPathVector;
    Denominator,LengthA,LengthB,s,t,tb0,tb1,Low,High,Swap:TpvDouble;
 function PointOnA(const aParameter:TpvDouble):TpvVectorPathVector;
 begin
  result.x:=EdgeA^.P0.x+(DirectionA.x*aParameter);
  result.y:=EdgeA^.P0.y+(DirectionA.y*aParameter);
 end;
 function ParameterOnA(const aPoint:TpvVectorPathVector):TpvDouble;
 begin
  result:=(((aPoint.x-EdgeA^.P0.x)*DirectionA.x)+((aPoint.y-EdgeA^.P0.y)*DirectionA.y))/LengthA;
 end;
 function ParameterOnB(const aPoint:TpvVectorPathVector):TpvDouble;
 begin
  result:=(((aPoint.x-EdgeB^.P0.x)*DirectionB.x)+((aPoint.y-EdgeB^.P0.y)*DirectionB.y))/LengthB;
 end;
 // Records a cut on both edges, but only where it is not already an endpoint of that edge.
 procedure Cut(const aPoint:TpvVectorPathVector);
 var Snapped:TpvVectorPathVector;
 begin
  Snapped:=Snap(aPoint);
  if (not SamePoint(Snapped,EdgeA^.P0)) and (not SamePoint(Snapped,EdgeA^.P1)) then begin
   AddSplitPoint(aIndexA,ParameterOnA(Snapped),Snapped);
   result:=true;
  end;
  if (not SamePoint(Snapped,EdgeB^.P0)) and (not SamePoint(Snapped,EdgeB^.P1)) then begin
   AddSplitPoint(aIndexB,ParameterOnB(Snapped),Snapped);
   result:=true;
  end;
 end;
begin

 result:=false;
 EdgeA:=@fEdges[aIndexA];
 EdgeB:=@fEdges[aIndexB];

 // Cheap rejection first: edges whose extents miss each other cannot possibly cross.
 if (Max(EdgeA^.P0.x,EdgeA^.P1.x)<Min(EdgeB^.P0.x,EdgeB^.P1.x)) or
    (Max(EdgeB^.P0.x,EdgeB^.P1.x)<Min(EdgeA^.P0.x,EdgeA^.P1.x)) or
    (Max(EdgeA^.P0.y,EdgeA^.P1.y)<Min(EdgeB^.P0.y,EdgeB^.P1.y)) or
    (Max(EdgeB^.P0.y,EdgeB^.P1.y)<Min(EdgeA^.P0.y,EdgeA^.P1.y)) then begin
  exit;
 end;

 DirectionA.x:=EdgeA^.P1.x-EdgeA^.P0.x;
 DirectionA.y:=EdgeA^.P1.y-EdgeA^.P0.y;
 DirectionB.x:=EdgeB^.P1.x-EdgeB^.P0.x;
 DirectionB.y:=EdgeB^.P1.y-EdgeB^.P0.y;
 LengthA:=sqr(DirectionA.x)+sqr(DirectionA.y);
 LengthB:=sqr(DirectionB.x)+sqr(DirectionB.y);

 if (LengthA<=0.0) or (LengthB<=0.0) then begin
  exit;
 end;

 Denominator:=(DirectionA.x*DirectionB.y)-(DirectionA.y*DirectionB.x);
 Delta.x:=EdgeB^.P0.x-EdgeA^.P0.x;
 Delta.y:=EdgeB^.P0.y-EdgeA^.P0.y;

 // Parallel edges only matter when they sit on the same line and genuinely overlap, in which
 // case both get cut at the ends of the shared stretch.
 if abs(Denominator)<=(EPSILON*sqrt(LengthA*LengthB)) then begin

  if abs((Delta.x*DirectionA.y)-(Delta.y*DirectionA.x))>(EPSILON*sqrt(LengthA)) then begin
   exit;
  end;

  tb0:=ParameterOnA(EdgeB^.P0);
  tb1:=ParameterOnA(EdgeB^.P1);
  if tb0>tb1 then begin
   Swap:=tb0;
   tb0:=tb1;
   tb1:=Swap;
  end;

  Low:=Max(0.0,tb0);
  High:=Min(1.0,tb1);

  // Touching at a single point needs no cut.
  if Low>=(High-EPSILON) then begin
   exit;
  end;

  Cut(PointOnA(Low));
  Cut(PointOnA(High));
  exit;

 end;

 s:=((Delta.x*DirectionB.y)-(Delta.y*DirectionB.x))/Denominator;
 t:=((Delta.x*DirectionA.y)-(Delta.y*DirectionA.x))/Denominator;

 if (s<-EPSILON) or (s>(1.0+EPSILON)) or (t<-EPSILON) or (t>(1.0+EPSILON)) then begin
  exit;
 end;

 Point:=PointOnA(s);
 Cut(Point);

end;

function TpvVectorPathBooleanOperation.SplitPass:boolean;
var Index,OtherIndex,CellIndex,CellX,CellY,CellX0,CellY0,CellX1,CellY1:TpvSizeInt;
    GridWidth,GridHeight,CellCount,Total:TpvSizeInt;
    MinimumX,MinimumY,MaximumX,MaximumY,CellSizeX,CellSizeY:TpvDouble;
    CellStart,CellCounts,CellItems:TSizeIntArray;
    Edge:PEdge;
    NewEdges:TEdges;
    CountNewEdges:TpvSizeInt;
    SplitIndex,RunStart,RunEnd,Position:TpvSizeInt;
    Previous:TpvVectorPathVector;
 procedure SortSplitPoints;
 var i,j:TpvSizeInt;
     Key:TSplitPoint;
 begin
  for i:=1 to fCountSplitPoints-1 do begin
   Key:=fSplitPoints[i];
   j:=i-1;
   while (j>=0) and
         ((fSplitPoints[j].EdgeIndex>Key.EdgeIndex) or
          ((fSplitPoints[j].EdgeIndex=Key.EdgeIndex) and (fSplitPoints[j].Parameter>Key.Parameter))) do begin
    fSplitPoints[j+1]:=fSplitPoints[j];
    dec(j);
   end;
   fSplitPoints[j+1]:=Key;
  end;
 end;
 procedure EmitEdge(const aFrom,aTo:TpvVectorPathVector;const aSource:PEdge);
 begin
  if SamePoint(aFrom,aTo) then begin
   exit;
  end;
  if CountNewEdges>=Length(NewEdges) then begin
   SetLength(NewEdges,(CountNewEdges+1)*2);
  end;
  NewEdges[CountNewEdges]:=aSource^;
  if ComparePoints(aFrom,aTo)<0 then begin
   NewEdges[CountNewEdges].P0:=aFrom;
   NewEdges[CountNewEdges].P1:=aTo;
  end else begin
   NewEdges[CountNewEdges].P0:=aTo;
   NewEdges[CountNewEdges].P1:=aFrom;
  end;
  inc(CountNewEdges);
 end;
 procedure CellRangeOf(const aEdge:PEdge;out aX0,aY0,aX1,aY1:TpvSizeInt);
 begin
  aX0:=Max(0,Min(GridWidth-1,trunc((Min(aEdge^.P0.x,aEdge^.P1.x)-MinimumX)/CellSizeX)));
  aX1:=Max(0,Min(GridWidth-1,trunc((Max(aEdge^.P0.x,aEdge^.P1.x)-MinimumX)/CellSizeX)));
  aY0:=Max(0,Min(GridHeight-1,trunc((Min(aEdge^.P0.y,aEdge^.P1.y)-MinimumY)/CellSizeY)));
  aY1:=Max(0,Min(GridHeight-1,trunc((Max(aEdge^.P0.y,aEdge^.P1.y)-MinimumY)/CellSizeY)));
 end;
begin

 result:=false;

 if fCountEdges<2 then begin
  exit;
 end;

 fCountSplitPoints:=0;

 // Extent of everything, which the grid is laid over.
 MinimumX:=fEdges[0].P0.x;
 MinimumY:=fEdges[0].P0.y;
 MaximumX:=MinimumX;
 MaximumY:=MinimumY;
 for Index:=0 to fCountEdges-1 do begin
  Edge:=@fEdges[Index];
  MinimumX:=Min(MinimumX,Min(Edge^.P0.x,Edge^.P1.x));
  MinimumY:=Min(MinimumY,Min(Edge^.P0.y,Edge^.P1.y));
  MaximumX:=Max(MaximumX,Max(Edge^.P0.x,Edge^.P1.x));
  MaximumY:=Max(MaximumY,Max(Edge^.P0.y,Edge^.P1.y));
 end;

 GridWidth:=Max(1,Min(256,round(sqrt(fCountEdges/2.0))));
 GridHeight:=GridWidth;
 CellCount:=GridWidth*GridHeight;
 CellSizeX:=(MaximumX-MinimumX)/GridWidth;
 CellSizeY:=(MaximumY-MinimumY)/GridHeight;
 if CellSizeX<=0.0 then begin
  CellSizeX:=1.0;
 end;
 if CellSizeY<=0.0 then begin
  CellSizeY:=1.0;
 end;

 CellCounts:=nil;
 CellStart:=nil;
 CellItems:=nil;
 try

  // Count how many edges land in each cell.
  SetLength(CellCounts,CellCount);
  for Index:=0 to CellCount-1 do begin
   CellCounts[Index]:=0;
  end;
  for Index:=0 to fCountEdges-1 do begin
   CellRangeOf(@fEdges[Index],CellX0,CellY0,CellX1,CellY1);
   for CellY:=CellY0 to CellY1 do begin
    for CellX:=CellX0 to CellX1 do begin
     inc(CellCounts[(CellY*GridWidth)+CellX]);
    end;
   end;
  end;

  // Prefix sum, so that one flat array can hold all the cell contents.
  SetLength(CellStart,CellCount+1);
  Total:=0;
  for Index:=0 to CellCount-1 do begin
   CellStart[Index]:=Total;
   inc(Total,CellCounts[Index]);
   CellCounts[Index]:=0;
  end;
  CellStart[CellCount]:=Total;

  // Fill it.
  SetLength(CellItems,Max(1,Total));
  for Index:=0 to fCountEdges-1 do begin
   CellRangeOf(@fEdges[Index],CellX0,CellY0,CellX1,CellY1);
   for CellY:=CellY0 to CellY1 do begin
    for CellX:=CellX0 to CellX1 do begin
     CellIndex:=(CellY*GridWidth)+CellX;
     CellItems[CellStart[CellIndex]+CellCounts[CellIndex]]:=Index;
     inc(CellCounts[CellIndex]);
    end;
   end;
  end;

  // Test only the pairs that share a cell. A pair sharing several cells gets tested more than
  // once, which merely produces duplicate split points, and those collapse when the cuts land.
  for CellIndex:=0 to CellCount-1 do begin
   for Index:=CellStart[CellIndex] to CellStart[CellIndex+1]-2 do begin
    for OtherIndex:=Index+1 to CellStart[CellIndex+1]-1 do begin
     if CellItems[Index]<CellItems[OtherIndex] then begin
      if CollectIntersections(CellItems[Index],CellItems[OtherIndex]) then begin
       result:=true;
      end;
     end;
    end;
   end;
  end;

 finally
  CellCounts:=nil;
  CellStart:=nil;
  CellItems:=nil;
 end;

 if fCountSplitPoints=0 then begin
  result:=false;
  exit;
 end;

 SortSplitPoints;

 // Rebuild the edge list, cutting every edge at the points collected for it.
 NewEdges:=nil;
 CountNewEdges:=0;
 SplitIndex:=0;
 for Index:=0 to fCountEdges-1 do begin

  Edge:=@fEdges[Index];

  while (SplitIndex<fCountSplitPoints) and (fSplitPoints[SplitIndex].EdgeIndex<Index) do begin
   inc(SplitIndex);
  end;

  if (SplitIndex>=fCountSplitPoints) or (fSplitPoints[SplitIndex].EdgeIndex<>Index) then begin
   EmitEdge(Edge^.P0,Edge^.P1,Edge);
   continue;
  end;

  RunStart:=SplitIndex;
  RunEnd:=RunStart;
  while (RunEnd<fCountSplitPoints) and (fSplitPoints[RunEnd].EdgeIndex=Index) do begin
   inc(RunEnd);
  end;

  Previous:=Edge^.P0;
  for Position:=RunStart to RunEnd-1 do begin
   if not SamePoint(fSplitPoints[Position].Point,Previous) then begin
    EmitEdge(Previous,fSplitPoints[Position].Point,Edge);
    Previous:=fSplitPoints[Position].Point;
   end;
  end;
  EmitEdge(Previous,Edge^.P1,Edge);

  SplitIndex:=RunEnd;

 end;

 fEdges:=NewEdges;
 fCountEdges:=CountNewEdges;

end;

procedure TpvVectorPathBooleanOperation.MergeCoincidentEdges;
var Index,Target,CountMerged:TpvSizeInt;
    Order:TSizeIntArray;
    Merged:TEdges;
 function CompareEdges(const aIndexA,aIndexB:TpvSizeInt):TpvInt32;
 begin
  result:=ComparePoints(fEdges[aIndexA].P0,fEdges[aIndexB].P0);
  if result=0 then begin
   result:=ComparePoints(fEdges[aIndexA].P1,fEdges[aIndexB].P1);
  end;
 end;
 procedure SortOrder;
 var i,j,Key:TpvSizeInt;
 begin
  for i:=1 to fCountEdges-1 do begin
   Key:=Order[i];
   j:=i-1;
   while (j>=0) and (CompareEdges(Order[j],Key)>0) do begin
    Order[j+1]:=Order[j];
    dec(j);
   end;
   Order[j+1]:=Key;
  end;
 end;
begin

 if fCountEdges=0 then begin
  exit;
 end;

 Order:=nil;
 Merged:=nil;
 try

  SetLength(Order,fCountEdges);
  for Index:=0 to fCountEdges-1 do begin
   Order[Index]:=Index;
  end;
  SortOrder;

  // Everything with the very same two endpoints folds into one edge whose deltas are the sum of
  // theirs. This is what keeps a boundary shared by both inputs from counting twice.
  SetLength(Merged,fCountEdges);
  CountMerged:=0;
  Index:=0;
  while Index<fCountEdges do begin
   Merged[CountMerged]:=fEdges[Order[Index]];
   Target:=Index+1;
   while (Target<fCountEdges) and (CompareEdges(Order[Index],Order[Target])=0) do begin
    inc(Merged[CountMerged].DeltaSubject,fEdges[Order[Target]].DeltaSubject);
    inc(Merged[CountMerged].DeltaClip,fEdges[Order[Target]].DeltaClip);
    inc(Target);
   end;
   inc(CountMerged);
   Index:=Target;
  end;

  SetLength(Merged,CountMerged);
  fEdges:=Merged;
  fCountEdges:=CountMerged;
  Merged:=nil;

 finally
  Order:=nil;
  Merged:=nil;
 end;

end;

function TpvVectorPathBooleanOperation.EdgeYAt(const aEdgeIndex:TpvSizeInt;const aX:TpvDouble):TpvDouble;
var Edge:PEdge;
    DeltaX:TpvDouble;
begin

 Edge:=@fEdges[aEdgeIndex];
 DeltaX:=Edge^.P1.x-Edge^.P0.x;

 if DeltaX<=0.0 then begin
  result:=Min(Edge^.P0.y,Edge^.P1.y);
 end else begin
  result:=Edge^.P0.y+(((Edge^.P1.y-Edge^.P0.y)*(aX-Edge^.P0.x))/DeltaX);
 end;

end;

procedure TpvVectorPathBooleanOperation.BuildColumnIndex;
var Index,Column,Column0,Column1,Total:TpvSizeInt;
    MinimumX,MaximumX:TpvDouble;
    Counts:TSizeIntArray;
    Edge:PEdge;
begin

 fColumnStart:=nil;
 fColumnItems:=nil;
 fCountColumns:=0;

 if fCountEdges=0 then begin
  exit;
 end;

 MinimumX:=fEdges[0].P0.x;
 MaximumX:=fEdges[0].P1.x;
 for Index:=0 to fCountEdges-1 do begin
  MinimumX:=Min(MinimumX,fEdges[Index].P0.x);
  MaximumX:=Max(MaximumX,fEdges[Index].P1.x);
 end;

 fCountColumns:=Max(1,Min(4096,round(sqrt(fCountEdges))*2));
 fColumnMinimumX:=MinimumX;
 fColumnSizeX:=(MaximumX-MinimumX)/fCountColumns;
 if fColumnSizeX<=0.0 then begin
  fColumnSizeX:=1.0;
 end;

 Counts:=nil;
 try

  // Count, prefix sum, fill. A vertical edge never crosses a downward ray, so it is left out
  // of the index altogether.
  SetLength(Counts,fCountColumns);
  for Index:=0 to fCountColumns-1 do begin
   Counts[Index]:=0;
  end;
  for Index:=0 to fCountEdges-1 do begin
   Edge:=@fEdges[Index];
   if Edge^.P0.x=Edge^.P1.x then begin
    continue;
   end;
   Column0:=Max(0,Min(fCountColumns-1,trunc((Edge^.P0.x-fColumnMinimumX)/fColumnSizeX)));
   Column1:=Max(0,Min(fCountColumns-1,trunc((Edge^.P1.x-fColumnMinimumX)/fColumnSizeX)));
   for Column:=Column0 to Column1 do begin
    inc(Counts[Column]);
   end;
  end;

  SetLength(fColumnStart,fCountColumns+1);
  Total:=0;
  for Index:=0 to fCountColumns-1 do begin
   fColumnStart[Index]:=Total;
   inc(Total,Counts[Index]);
   Counts[Index]:=0;
  end;
  fColumnStart[fCountColumns]:=Total;

  SetLength(fColumnItems,Max(1,Total));
  for Index:=0 to fCountEdges-1 do begin
   Edge:=@fEdges[Index];
   if Edge^.P0.x=Edge^.P1.x then begin
    continue;
   end;
   Column0:=Max(0,Min(fCountColumns-1,trunc((Edge^.P0.x-fColumnMinimumX)/fColumnSizeX)));
   Column1:=Max(0,Min(fCountColumns-1,trunc((Edge^.P1.x-fColumnMinimumX)/fColumnSizeX)));
   for Column:=Column0 to Column1 do begin
    fColumnItems[fColumnStart[Column]+Counts[Column]]:=Index;
    inc(Counts[Column]);
   end;
  end;

 finally
  Counts:=nil;
 end;

end;

procedure TpvVectorPathBooleanOperation.WindingAt(const aX,aY:TpvDouble;const aExcludeEdge:TpvSizeInt;
                                                  out aWindingSubject,aWindingClip:TpvInt32);
var Column,Position,Index:TpvSizeInt;
    Edge:PEdge;
begin

 aWindingSubject:=0;
 aWindingClip:=0;

 if fCountColumns=0 then begin
  exit;
 end;

 Column:=Max(0,Min(fCountColumns-1,trunc((aX-fColumnMinimumX)/fColumnSizeX)));

 // Half open in x, so that an edge is counted at its left endpoint and not at its right one.
 // That is what makes two edges meeting at a vertex count as one crossing instead of two.
 for Position:=fColumnStart[Column] to fColumnStart[Column+1]-1 do begin

  Index:=fColumnItems[Position];
  if Index=aExcludeEdge then begin
   continue;
  end;

  Edge:=@fEdges[Index];
  if (Edge^.P0.x>aX) or (aX>=Edge^.P1.x) then begin
   continue;
  end;

  if EdgeYAt(Index,aX)<aY then begin
   inc(aWindingSubject,Edge^.DeltaSubject);
   inc(aWindingClip,Edge^.DeltaClip);
  end;

 end;

end;

function TpvVectorPathBooleanOperation.IsInside(const aWinding:TpvInt32;const aFillRule:TpvVectorPathFillRule):boolean;
begin
 if aFillRule=TpvVectorPathFillRule.EvenOdd then begin
  result:=(aWinding and 1)<>0;
 end else begin
  result:=aWinding<>0;
 end;
end;

function TpvVectorPathBooleanOperation.CombineInside(const aSubjectInside,aClipInside:boolean):boolean;
begin

 case fOperation of
  TpvVectorPathBooleanOperationKind.Intersection:begin
   result:=aSubjectInside and aClipInside;
  end;
  TpvVectorPathBooleanOperationKind.Difference:begin
   result:=aSubjectInside and not aClipInside;
  end;
  TpvVectorPathBooleanOperationKind.ExclusiveOr:begin
   result:=aSubjectInside xor aClipInside;
  end;
  else {TpvVectorPathBooleanOperationKind.Union:}begin
   result:=aSubjectInside or aClipInside;
  end;
 end;

end;

procedure TpvVectorPathBooleanOperation.ComputeWindings;
var Index:TpvSizeInt;
    Edge:PEdge;
    MiddleX,MiddleY,SideStep:TpvDouble;
    BelowSubject,BelowClip,AboveSubject,AboveClip:TpvInt32;
    InsideBelow,InsideAbove:boolean;
begin

 BuildColumnIndex;

 // Half a grid step is enough to get clear of a vertical edge, since every endpoint sits on the
 // grid and therefore nothing else starts or ends in between.
 SideStep:=fInverseGridScale*0.5;

 for Index:=0 to fCountEdges-1 do begin

  Edge:=@fEdges[Index];
  MiddleX:=(Edge^.P0.x+Edge^.P1.x)*0.5;
  MiddleY:=(Edge^.P0.y+Edge^.P1.y)*0.5;

  if Edge^.P0.x=Edge^.P1.x then begin

   // A vertical edge separates left from right, not above from below, so that is what gets
   // asked. The midpoint lies strictly inside the edge, and edges only meet at endpoints, so
   // neither sample can land on another edge.
   WindingAt(MiddleX-SideStep,MiddleY,Index,BelowSubject,BelowClip);
   WindingAt(MiddleX+SideStep,MiddleY,Index,AboveSubject,AboveClip);

   InsideBelow:=CombineInside(IsInside(BelowSubject,fSubjectFillRule),
                              IsInside(BelowClip,fClipFillRule));
   InsideAbove:=CombineInside(IsInside(AboveSubject,fSubjectFillRule),
                              IsInside(AboveClip,fClipFillRule));

   Edge^.InResult:=InsideBelow<>InsideAbove;

   // Inside on the left means the contour runs upwards here, which keeps the inside on the left
   // of the direction of travel.
   Edge^.Forward_:=InsideBelow;

  end else begin

   // For everything else the ray straight down from the midpoint gives the winding below, and
   // the edge's own delta gives the winding above.
   WindingAt(MiddleX,MiddleY,Index,BelowSubject,BelowClip);
   AboveSubject:=BelowSubject+Edge^.DeltaSubject;
   AboveClip:=BelowClip+Edge^.DeltaClip;

   InsideBelow:=CombineInside(IsInside(BelowSubject,fSubjectFillRule),
                              IsInside(BelowClip,fClipFillRule));
   InsideAbove:=CombineInside(IsInside(AboveSubject,fSubjectFillRule),
                              IsInside(AboveClip,fClipFillRule));

   Edge^.InResult:=InsideBelow<>InsideAbove;

   // Inside above means the contour runs left to right along this edge.
   Edge^.Forward_:=InsideAbove;

  end;

 end;

end;

procedure TpvVectorPathBooleanOperation.BuildResultContours(const aResult:TpvVectorPathShape);
var Index,Position,EdgeIndex,NextIndex,VertexIndex,Total,Steps:TpvSizeInt;
    CountResultEdges,CountEndpoints,CountVertices,CountPoints:TpvSizeInt;
    ResultEdges,VertexStart,VertexItems,VertexCounts:TSizeIntArray;
    Endpoints,VertexPoints,Points:TpvVectorPathVectors;
    Contour:TpvVectorPathContour;
    Target,StartPoint:TpvVectorPathVector;
    Found:boolean;
 function FindVertex(const aPoint:TpvVectorPathVector):TpvSizeInt;
 var Low,High,Middle,Comparison:TpvSizeInt;
 begin
  result:=-1;
  Low:=0;
  High:=CountVertices-1;
  while Low<=High do begin
   Middle:=(Low+High) shr 1;
   Comparison:=ComparePoints(VertexPoints[Middle],aPoint);
   if Comparison=0 then begin
    result:=Middle;
    exit;
   end else if Comparison<0 then begin
    Low:=Middle+1;
   end else begin
    High:=Middle-1;
   end;
  end;
 end;
 procedure SortEndpoints;
 var i,j:TpvSizeInt;
     Key:TpvVectorPathVector;
 begin
  for i:=1 to CountEndpoints-1 do begin
   Key:=Endpoints[i];
   j:=i-1;
   while (j>=0) and (ComparePoints(Endpoints[j],Key)>0) do begin
    Endpoints[j+1]:=Endpoints[j];
    dec(j);
   end;
   Endpoints[j+1]:=Key;
  end;
 end;
 // Registers one endpoint of an edge with the vertex it sits on.
 procedure LinkEdgeToVertex(const aEdgeIndex:TpvSizeInt;const aPoint:TpvVectorPathVector;const aCountOnly:boolean);
 var Vertex:TpvSizeInt;
 begin
  Vertex:=FindVertex(aPoint);
  if Vertex<0 then begin
   exit;
  end;
  if aCountOnly then begin
   inc(VertexCounts[Vertex]);
  end else begin
   VertexItems[VertexStart[Vertex]+VertexCounts[Vertex]]:=aEdgeIndex;
   inc(VertexCounts[Vertex]);
  end;
 end;
begin

 // Gather the edges that survived.
 ResultEdges:=nil;
 CountResultEdges:=0;
 for Index:=0 to fCountEdges-1 do begin
  if fEdges[Index].InResult then begin
   if CountResultEdges>=Length(ResultEdges) then begin
    SetLength(ResultEdges,(CountResultEdges+1)*2);
   end;
   ResultEdges[CountResultEdges]:=Index;
   inc(CountResultEdges);
   fEdges[Index].Used:=false;
  end;
 end;

 if CountResultEdges=0 then begin
  exit;
 end;

 Endpoints:=nil;
 VertexPoints:=nil;
 VertexStart:=nil;
 VertexItems:=nil;
 VertexCounts:=nil;
 Points:=nil;
 try

  // Collect and deduplicate the endpoints into a sorted vertex list.
  CountEndpoints:=0;
  SetLength(Endpoints,CountResultEdges*2);
  for Index:=0 to CountResultEdges-1 do begin
   Endpoints[CountEndpoints]:=fEdges[ResultEdges[Index]].P0;
   inc(CountEndpoints);
   Endpoints[CountEndpoints]:=fEdges[ResultEdges[Index]].P1;
   inc(CountEndpoints);
  end;
  SortEndpoints;

  SetLength(VertexPoints,CountEndpoints);
  CountVertices:=0;
  for Index:=0 to CountEndpoints-1 do begin
   if (CountVertices=0) or (not SamePoint(VertexPoints[CountVertices-1],Endpoints[Index])) then begin
    VertexPoints[CountVertices]:=Endpoints[Index];
    inc(CountVertices);
   end;
  end;
  SetLength(VertexPoints,CountVertices);

  // Bucket the edges by the vertices they touch: count, prefix sum, fill.
  SetLength(VertexCounts,CountVertices);
  for Index:=0 to CountVertices-1 do begin
   VertexCounts[Index]:=0;
  end;
  for Index:=0 to CountResultEdges-1 do begin
   EdgeIndex:=ResultEdges[Index];
   LinkEdgeToVertex(EdgeIndex,fEdges[EdgeIndex].P0,true);
   LinkEdgeToVertex(EdgeIndex,fEdges[EdgeIndex].P1,true);
  end;

  SetLength(VertexStart,CountVertices+1);
  Total:=0;
  for Index:=0 to CountVertices-1 do begin
   VertexStart[Index]:=Total;
   inc(Total,VertexCounts[Index]);
   VertexCounts[Index]:=0;
  end;
  VertexStart[CountVertices]:=Total;

  SetLength(VertexItems,Max(1,Total));
  for Index:=0 to CountResultEdges-1 do begin
   EdgeIndex:=ResultEdges[Index];
   LinkEdgeToVertex(EdgeIndex,fEdges[EdgeIndex].P0,false);
   LinkEdgeToVertex(EdgeIndex,fEdges[EdgeIndex].P1,false);
  end;

  // Walk each unused edge into a closed contour, always leaving a vertex along an edge whose
  // own direction flag points away from it.
  for Index:=0 to CountResultEdges-1 do begin

   EdgeIndex:=ResultEdges[Index];
   if fEdges[EdgeIndex].Used then begin
    continue;
   end;

   CountPoints:=0;
   SetLength(Points,CountResultEdges+2);

   if fEdges[EdgeIndex].Forward_ then begin
    StartPoint:=fEdges[EdgeIndex].P0;
    Target:=fEdges[EdgeIndex].P1;
   end else begin
    StartPoint:=fEdges[EdgeIndex].P1;
    Target:=fEdges[EdgeIndex].P0;
   end;
   fEdges[EdgeIndex].Used:=true;

   Points[CountPoints]:=StartPoint;
   inc(CountPoints);

   Steps:=0;
   repeat

    inc(Steps);
    if Steps>(CountResultEdges+2) then begin
     break;
    end;

    if CountPoints>=Length(Points) then begin
     SetLength(Points,(CountPoints+1)*2);
    end;
    Points[CountPoints]:=Target;
    inc(CountPoints);

    if SamePoint(Target,StartPoint) then begin
     break;
    end;

    VertexIndex:=FindVertex(Target);
    if VertexIndex<0 then begin
     break;
    end;

    Found:=false;
    for Position:=VertexStart[VertexIndex] to VertexStart[VertexIndex+1]-1 do begin
     NextIndex:=VertexItems[Position];
     if fEdges[NextIndex].Used then begin
      continue;
     end;
     if fEdges[NextIndex].Forward_ then begin
      if SamePoint(fEdges[NextIndex].P0,Target) then begin
       Target:=fEdges[NextIndex].P1;
       fEdges[NextIndex].Used:=true;
       Found:=true;
       break;
      end;
     end else begin
      if SamePoint(fEdges[NextIndex].P1,Target) then begin
       Target:=fEdges[NextIndex].P0;
       fEdges[NextIndex].Used:=true;
       Found:=true;
       break;
      end;
     end;
    end;

    if not Found then begin
     break;
    end;

   until false;

   // The closing point repeats the first one, so it goes again.
   if (CountPoints>1) and SamePoint(Points[CountPoints-1],Points[0]) then begin
    dec(CountPoints);
   end;

   if CountPoints>=3 then begin
    Contour:=TpvVectorPathContour.Create;
    Contour.Closed:=true;
    for Position:=0 to CountPoints-1 do begin
     if Position<(CountPoints-1) then begin
      Contour.Segments.Add(TpvVectorPathSegmentLine.Create(Points[Position],Points[Position+1]));
     end else begin
      Contour.Segments.Add(TpvVectorPathSegmentLine.Create(Points[Position],Points[0]));
     end;
    end;
    aResult.Contours.Add(Contour);
   end;

  end;

 finally
  Endpoints:=nil;
  VertexPoints:=nil;
  VertexStart:=nil;
  VertexItems:=nil;
  VertexCounts:=nil;
  Points:=nil;
  ResultEdges:=nil;
 end;

end;

function TpvVectorPathBooleanOperation.Execute(const aSubject,aClip:TpvVectorPathShape;
                                               const aOperation:TpvVectorPathBooleanOperationKind;
                                               const aSubjectFillRule,aClipFillRule:TpvVectorPathFillRule):TpvVectorPathShape;
var Pass:TpvSizeInt;
begin

 fOperation:=aOperation;
 fSubjectFillRule:=aSubjectFillRule;
 fClipFillRule:=aClipFillRule;

 if fGridScale<=0.0 then begin
  fGridScale:=DefaultGridScale;
 end;
 fInverseGridScale:=1.0/fGridScale;

 fEdges:=nil;
 fCountEdges:=0;
 fSplitPoints:=nil;
 fCountSplitPoints:=0;

 result:=TpvVectorPathShape.Create;
 try

  // The result is always wound so that the non-zero rule fills exactly the computed area.
  result.FillRule:=TpvVectorPathFillRule.NonZero;

  AddShape(aSubject,true);
  AddShape(aClip,false);

  if fCountEdges=0 then begin
   exit;
  end;

  for Pass:=1 to MaximumSplitPasses do begin
   if not SplitPass then begin
    break;
   end;
  end;

  MergeCoincidentEdges;
  ComputeWindings;
  BuildResultContours(result);

 except
  FreeAndNil(result);
  raise;
 end;

end;

function pvVectorPathIntersection(const aSubject,aClip:TpvVectorPathShape;
                                  const aSubjectFillRule,aClipFillRule:TpvVectorPathFillRule):TpvVectorPathShape;
var Operation:TpvVectorPathBooleanOperation;
begin
 Operation:=TpvVectorPathBooleanOperation.Create;
 try
  result:=Operation.Execute(aSubject,aClip,TpvVectorPathBooleanOperationKind.Intersection,aSubjectFillRule,aClipFillRule);
 finally
  FreeAndNil(Operation);
 end;
end;

function pvVectorPathUnion(const aSubject,aClip:TpvVectorPathShape;
                           const aSubjectFillRule,aClipFillRule:TpvVectorPathFillRule):TpvVectorPathShape;
var Operation:TpvVectorPathBooleanOperation;
begin
 Operation:=TpvVectorPathBooleanOperation.Create;
 try
  result:=Operation.Execute(aSubject,aClip,TpvVectorPathBooleanOperationKind.Union,aSubjectFillRule,aClipFillRule);
 finally
  FreeAndNil(Operation);
 end;
end;

function pvVectorPathDifference(const aSubject,aClip:TpvVectorPathShape;
                                const aSubjectFillRule,aClipFillRule:TpvVectorPathFillRule):TpvVectorPathShape;
var Operation:TpvVectorPathBooleanOperation;
begin
 Operation:=TpvVectorPathBooleanOperation.Create;
 try
  result:=Operation.Execute(aSubject,aClip,TpvVectorPathBooleanOperationKind.Difference,aSubjectFillRule,aClipFillRule);
 finally
  FreeAndNil(Operation);
 end;
end;

function pvVectorPathExclusiveOr(const aSubject,aClip:TpvVectorPathShape;
                                 const aSubjectFillRule,aClipFillRule:TpvVectorPathFillRule):TpvVectorPathShape;
var Operation:TpvVectorPathBooleanOperation;
begin
 Operation:=TpvVectorPathBooleanOperation.Create;
 try
  result:=Operation.Execute(aSubject,aClip,TpvVectorPathBooleanOperationKind.ExclusiveOr,aSubjectFillRule,aClipFillRule);
 finally
  FreeAndNil(Operation);
 end;
end;

function pvVectorPathNormalize(const aShape:TpvVectorPathShape;
                               const aFillRule:TpvVectorPathFillRule):TpvVectorPathShape;
var Operation:TpvVectorPathBooleanOperation;
begin
 Operation:=TpvVectorPathBooleanOperation.Create;
 try
  result:=Operation.Execute(aShape,nil,TpvVectorPathBooleanOperationKind.Union,aFillRule,TpvVectorPathFillRule.NonZero);
 finally
  FreeAndNil(Operation);
 end;
end;

end.
