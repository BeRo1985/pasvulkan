(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2021, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
 *                  General guideTriangles for code contributors                  *
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
unit PasVulkan.BVH.Triangles;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

{$ifdef Delphi2009AndUp}
 {$warn DUPLICATE_CTOR_DTOR off}
{$endif}

{$undef UseDouble}
{$ifdef UseDouble}
 {$define NonSIMD}
{$endif}

{-$define NonSIMD}

{$ifdef NonSIMD}
 {$undef SIMD}
{$else}
 {$ifdef cpu386}
  {$if not (defined(Darwin) or defined(CompileForWithPIC))}
   {$define SIMD}
  {$ifend}
 {$endif}
 {$ifdef cpux64}
  {$define SIMD}
 {$endif}
{$endif}

{$define NonRecursive}

{$ifndef fpc}
 {$scopedenums on}
{$endif}

interface

uses SysUtils,
     Classes,
     Math,
     PasMP,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Collections;

type EpvTriangleBVH=class(Exception);

     { TpvTriangleBVHRay }
     TpvTriangleBVHRay=record
      public
       Origin:TpvVector3;
       Direction:TpvVector3;
       constructor Create(const aOrigin,aDirection:TpvVector3);
     end;
     PpvTriangleBVHRay=^TpvTriangleBVHRay;

     { TpvTriangleBVHTriangle }
     TpvTriangleBVHTriangle=packed record
      public
       Points:array[0..2] of TpvVector3;
       Normal:TpvVector3;
       Center:TpvVector3;
       Data:TpvPtrInt;
       Flags:TpvUInt32;
       function RayIntersection(const aRayOrigin,aRayDirection:TpvVector3;out aTime,aU,aV,aW:TpvScalar):boolean; overload;
       function RayIntersection(const aRay:TpvTriangleBVHRay;out aTime,aU,aV,aW:TpvScalar):boolean; overload;
     end;
     PpvTriangleBVHTriangle=^TpvTriangleBVHTriangle;

     TpvTriangleBVHTriangles=array of TpvTriangleBVHTriangle;

     TpvTriangleBVHIntersection=packed record
      public
       Time:TpvScalar;
       Triangle:PpvTriangleBVHTriangle;
       IntersectionPoint:TpvVector3;
       Barycentrics:TpvVector3;
     end;
     PpvTriangleBVHIntersection=^TpvTriangleBVHIntersection;

     { TpvTriangleBVHTreeNode }
     TpvTriangleBVHTreeNode=packed record
      Bounds:TpvAABB;
      FirstLeftChild:TpvInt32;
      FirstTriangleIndex:TpvInt32;
      CountTriangles:TpvInt32;
     end;
     PpvTriangleBVHTreeNode=^TpvTriangleBVHTreeNode;

     TpvTriangleBVHTreeNodes=array of TpvTriangleBVHTreeNode;

     { TpvTriangleBVHSkipListNode }
     TpvTriangleBVHSkipListNode=packed record // must be GPU-friendly
      Min:TpvVector4;
      Max:TpvVector4;
      FirstTriangleIndex:TpvInt32;
      CountTriangles:TpvInt32;
      SkipCount:TpvInt32;
      Dummy:TpvInt32;
     end; // 48 bytes per Skip list item
     PpvTriangleBVHSkipListNode=^TpvTriangleBVHSkipListNode;

     TpvTriangleBVHSkipListNodes=array of TpvTriangleBVHSkipListNode;

     TpvTriangleBVHNodeQueue=TPasMPUnboundedQueue<TpvInt32>;

     TpvTriangleBVHBuildMode=
      (
       SAHBruteforce,
       SAHSteps,
       SAHBinned
      );

     { TpvTriangleBVH }

     TpvTriangleBVH=class
      public
       const MaximumTrianglesPerNode=8;
      private
       type TTreeNodeStack=TpvDynamicStack<TpvUInt64>;
            TSkipListNodeMap=array of TpvInt32;
      private
       fPasMPInstance:TPasMP;
       fBounds:TpvAABB;
       fTriangles:TpvTriangleBVHTriangles;
       fCountTriangles:TpvInt32;
       fTreeNodes:TpvTriangleBVHTreeNodes;
       fCountTreeNodes:TpvInt32;
       fTreeNodeRoot:TpvInt32;
       fSkipListNodeMap:TSkipListNodeMap;
       fSkipListNodes:TpvTriangleBVHSkipListNodes;
       fCountSkipListNodes:TpvInt32;
       fNodeQueue:TpvTriangleBVHNodeQueue;
       fCountActiveWorkers:TPasMPInt32;
       fTreeNodeStack:TTreeNodeStack;
       fBVHBuildMode:TpvTriangleBVHBuildMode;
       fBVHSubdivisionSteps:TpvInt32;
       function EvaluateSAH(const aParentTreeNode:PpvTriangleBVHTreeNode;const aAxis:TpvInt32;const aSplitPosition:TpvFloat):TpvFloat;
       function FindBestSplitPlaneBruteforce(const aParentTreeNode:PpvTriangleBVHTreeNode;out aAxis:TpvInt32;out aSplitPosition:TpvFloat):TpvFloat;
       function FindBestSplitPlaneSteps(const aParentTreeNode:PpvTriangleBVHTreeNode;out aAxis:TpvInt32;out aSplitPosition:TpvFloat):TpvFloat;
       function FindBestSplitPlaneBinned(const aParentTreeNode:PpvTriangleBVHTreeNode;out aAxis:TpvInt32;out aSplitPosition:TpvFloat):TpvFloat;
       function CalculateNodeCost(const aParentTreeNode:PpvTriangleBVHTreeNode):TpvFloat;
       procedure UpdateNodeBounds(const aParentTreeNode:PpvTriangleBVHTreeNode);
       procedure ProcessNodeQueue;
       procedure BuildJob(const Job:PPasMPJob;const ThreadIndex:TPasMPInt32);
      public
       constructor Create(const aPasMPInstance:TPasMP);
       destructor Destroy; override;
       procedure Clear;
       procedure AddTriangle(const aPoint0,aPoint1,aPoint2:TpvVector3;const aNormal:PpvVector3=nil;const aData:TpvPtrInt=0;const aFlags:TpvUInt32=TpvUInt32($ffffffff));
       procedure Build;
       procedure LoadFromStream(const aStream:TStream);
       procedure SaveToStream(const aStream:TStream);
       function RayIntersection(const aRay:TpvTriangleBVHRay;var aIntersection:TpvTriangleBVHIntersection;const aFastCheck:boolean=false;const aFlags:TpvUInt32=TpvUInt32($ffffffff);const aAvoidFlags:TpvUInt32=TpvUInt32(0)):boolean;
       function CountRayIntersections(const aRay:TpvTriangleBVHRay;const aFlags:TpvUInt32=TpvUInt32($ffffffff);const aAvoidFlags:TpvUInt32=TpvUInt32(0)):TpvInt32;
       function LineIntersection(const aV0,aV1:TpvVector3;const aFlags:TpvUInt32=TpvUInt32($ffffffff);const aAvoidFlags:TpvUInt32=TpvUInt32(0)):boolean;
       function IsOpenSpacePerEvenOddRule(const aPosition:TpvVector3;const aFlags:TpvUInt32=TpvUInt32($ffffffff);const aAvoidFlags:TpvUInt32=TpvUInt32(0)):boolean; overload;
       function IsOpenSpacePerEvenOddRule(const aPosition:TpvVector3;out aNearestNormal,aNearestNormalPosition:TpvVector3;const aFlags:TpvUInt32=TpvUInt32($ffffffff);const aAvoidFlags:TpvUInt32=TpvUInt32(0)):boolean; overload;
       function IsOpenSpacePerNormals(const aPosition:TpvVector3;const aFlags:TpvUInt32=TpvUInt32($ffffffff);const aAvoidFlags:TpvUInt32=TpvUInt32(0)):boolean; overload;
       function IsOpenSpacePerNormals(const aPosition:TpvVector3;out aNearestNormal,aNearestNormalPosition:TpvVector3;const aFlags:TpvUInt32=TpvUInt32($ffffffff);const aAvoidFlags:TpvUInt32=TpvUInt32(0)):boolean; overload;
      public
       property Triangles:TpvTriangleBVHTriangles read fTriangles;
       property CountTriangles:TpvInt32 read fCountTriangles;
       property TreeNodes:TpvTriangleBVHTreeNodes read fTreeNodes;
       property CountTreeNodes:TpvInt32 read fCountTreeNodes;
       property TreeNodeRoot:TpvInt32 read fTreeNodeRoot;
       property SkipListNodes:TpvTriangleBVHSkipListNodes read fSkipListNodes;
       property CountSkipListNodes:TpvInt32 read fCountSkipListNodes;
       property BVHBuildMode:TpvTriangleBVHBuildMode read fBVHBuildMode write fBVHBuildMode;
       property BVHSubdivisionSteps:TpvInt32 read fBVHSubdivisionSteps write fBVHSubdivisionSteps;
     end;

implementation

type TTriangleBVHFileSignature=array[0..7] of AnsiChar;

const TriangleBVHFileSignature:TTriangleBVHFileSignature=('T','B','V','H','F','i','l','e'); // Triangle BVH File

      TriangleBVHFileVersion=TpvUInt32($00000001);
      
{ TpvTriangleBVHRay }

constructor TpvTriangleBVHRay.Create(const aOrigin,aDirection:TpvVector3);
begin
 Origin:=aOrigin;
 Direction:=aDirection;
end;

{ TpvTriangleBVHTriangle }

function TpvTriangleBVHTriangle.RayIntersection(const aRayOrigin,aRayDirection:TpvVector3;out aTime,aU,aV,aW:TpvScalar):boolean;
const EPSILON=1e-7;
var v0v1,v0v2,p,t,q:TpvVector3;
    Determinant,InverseDeterminant:TpvScalar;
begin
 result:=false;
 v0v1:=Points[1]-Points[0];
 v0v2:=Points[2]-Points[0];
 p:=aRayDirection.Cross(v0v2);
 Determinant:=v0v1.Dot(p);
 if Determinant<EPSILON then begin
  exit;
 end;
 InverseDeterminant:=1.0/Determinant;
 t:=aRayOrigin-Points[0];
 aV:=t.Dot(p)*InverseDeterminant;
 if (aV<0.0) or (aV>1.0) then begin
  exit;
 end;
 q:=t.Cross(v0v1);
 aW:=aRayDirection.Dot(q)*InverseDeterminant;
 if (aW<0.0) or ((aV+aW)>1.0) then begin
  exit;
 end;
 aTime:=v0v2.Dot(q)*InverseDeterminant;
 aU:=1.0-(aV+aW);
 result:=true;
end;

function TpvTriangleBVHTriangle.RayIntersection(const aRay:TpvTriangleBVHRay;out aTime,aU,aV,aW:TpvScalar):boolean;
const EPSILON=1e-7;
var v0v1,v0v2,p,t,q:TpvVector3;
    Determinant,InverseDeterminant:TpvScalar;
begin
 result:=false;
 v0v1:=Points[1]-Points[0];
 v0v2:=Points[2]-Points[0];
 p:=aRay.Direction.Cross(v0v2);
 Determinant:=v0v1.Dot(p);
 if Determinant<EPSILON then begin
  exit;
 end;
 InverseDeterminant:=1.0/Determinant;
 t:=aRay.Origin-Points[0];
 aV:=t.Dot(p)*InverseDeterminant;
 if (aV<0.0) or (aV>1.0) then begin
  exit;
 end;
 q:=t.Cross(v0v1);
 aW:=aRay.Direction.Dot(q)*InverseDeterminant;
 if (aW<0.0) or ((aV+aW)>1.0) then begin
  exit;
 end;
 aTime:=v0v2.Dot(q)*InverseDeterminant;
 aU:=1.0-(aV+aW);
 result:=true;
end;

{ TpvTriangleBVH }

constructor TpvTriangleBVH.Create(const aPasMPInstance:TPasMP);
begin

 inherited Create;

 fPasMPInstance:=aPasMPInstance;

 fTriangles:=nil;
 fCountTriangles:=0;

 fTreeNodes:=nil;
 fCountTreeNodes:=0;
 fTreeNodeRoot:=-1;

 fSkipListNodes:=nil;
 fCountSkipListNodes:=0;

 fNodeQueue:=TpvTriangleBVHNodeQueue.Create;

 fTreeNodeStack.Initialize;

 fSkipListNodeMap:=nil;

 fBVHBuildMode:=TpvTriangleBVHBuildMode.SAHSteps;

 fBVHSubdivisionSteps:=8;

end;

destructor TpvTriangleBVH.Destroy;
begin
 fTriangles:=nil;
 fTreeNodes:=nil;
 fSkipListNodes:=nil;
 fTreeNodeStack.Finalize;
 FreeAndNil(fNodeQueue);
 fSkipListNodeMap:=nil;
 inherited Destroy;
end;

procedure TpvTriangleBVH.Clear;
begin

 fCountTriangles:=0;

 fCountTreeNodes:=0;
 fTreeNodeRoot:=-1;

 fCountSkipListNodes:=0;

end;

procedure TpvTriangleBVH.AddTriangle(const aPoint0,aPoint1,aPoint2:TpvVector3;const aNormal:PpvVector3;const aData:TpvPtrInt;const aFlags:TpvUInt32);
var Index:TpvInt32;
    Triangle:PpvTriangleBVHTriangle;
begin
 Index:=fCountTriangles;
 inc(fCountTriangles);
 if length(fTriangles)<=fCountTriangles then begin
  SetLength(fTriangles,fCountTriangles+((fCountTriangles+1) shr 1));
 end;
 Triangle:=@fTriangles[Index];
 Triangle^.Points[0]:=aPoint0;
 Triangle^.Points[1]:=aPoint1;
 Triangle^.Points[2]:=aPoint2;
 if assigned(aNormal) then begin
  Triangle^.Normal:=aNormal^;
 end else begin
  Triangle^.Normal:=((Triangle^.Points[1]-Triangle^.Points[0]).Cross(Triangle^.Points[2]-Triangle^.Points[0])).Normalize;
 end;
 Triangle^.Center:=(aPoint0+aPoint1+aPoint2)/3.0;
 Triangle^.Data:=aData;
 Triangle^.Flags:=aFlags;
 if Index=0 then begin
  fBounds.Min.x:=Min(Min(aPoint0.x,aPoint1.x),aPoint2.x);
  fBounds.Min.y:=Min(Min(aPoint0.y,aPoint1.y),aPoint2.y);
  fBounds.Min.z:=Min(Min(aPoint0.z,aPoint1.z),aPoint2.z);
  fBounds.Max.x:=Max(Max(aPoint0.x,aPoint1.x),aPoint2.x);
  fBounds.Max.y:=Max(Max(aPoint0.y,aPoint1.y),aPoint2.y);
  fBounds.Max.z:=Max(Max(aPoint0.z,aPoint1.z),aPoint2.z);
 end else begin
  fBounds.Min.x:=Min(fBounds.Min.x,Min(Min(aPoint0.x,aPoint1.x),aPoint2.x));
  fBounds.Min.y:=Min(fBounds.Min.y,Min(Min(aPoint0.y,aPoint1.y),aPoint2.y));
  fBounds.Min.z:=Min(fBounds.Min.z,Min(Min(aPoint0.z,aPoint1.z),aPoint2.z));
  fBounds.Max.x:=Max(fBounds.Max.x,Max(Max(aPoint0.x,aPoint1.x),aPoint2.x));
  fBounds.Max.y:=Max(fBounds.Max.y,Max(Max(aPoint0.y,aPoint1.y),aPoint2.y));
  fBounds.Max.z:=Max(fBounds.Max.z,Max(Max(aPoint0.z,aPoint1.z),aPoint2.z));
 end;
end;

function TpvTriangleBVH.EvaluateSAH(const aParentTreeNode:PpvTriangleBVHTreeNode;const aAxis:TpvInt32;const aSplitPosition:TpvFloat):TpvFloat;
var LeftAABB,RightAABB:TpvAABB;
    LeftCount,RightCount,TriangleIndex:TpvInt32;
    Triangle:PpvTriangleBVHTriangle;
begin
 LeftCount:=0;
 RightCount:=0;
 for TriangleIndex:=aParentTreeNode^.FirstTriangleIndex to aParentTreeNode^.FirstTriangleIndex+(aParentTreeNode^.CountTriangles-1) do begin
  Triangle:=@fTriangles[TriangleIndex];
  if Triangle^.Center.xyz[aAxis]<aSplitPosition then begin
   if LeftCount=0 then begin
    LeftAABB.Min.x:=Min(Min(Triangle^.Points[0].x,Triangle^.Points[1].x),Triangle^.Points[2].x);
    LeftAABB.Min.y:=Min(Min(Triangle^.Points[0].y,Triangle^.Points[1].y),Triangle^.Points[2].y);
    LeftAABB.Min.z:=Min(Min(Triangle^.Points[0].z,Triangle^.Points[1].z),Triangle^.Points[2].z);
    LeftAABB.Max.x:=Max(Max(Triangle^.Points[0].x,Triangle^.Points[1].x),Triangle^.Points[2].x);
    LeftAABB.Max.y:=Max(Max(Triangle^.Points[0].y,Triangle^.Points[1].y),Triangle^.Points[2].y);
    LeftAABB.Max.z:=Max(Max(Triangle^.Points[0].z,Triangle^.Points[1].z),Triangle^.Points[2].z);
   end else begin
    LeftAABB.Min.x:=Min(LeftAABB.Min.x,Min(Min(Triangle^.Points[0].x,Triangle^.Points[1].x),Triangle^.Points[2].x));
    LeftAABB.Min.y:=Min(LeftAABB.Min.y,Min(Min(Triangle^.Points[0].y,Triangle^.Points[1].y),Triangle^.Points[2].y));
    LeftAABB.Min.z:=Min(LeftAABB.Min.z,Min(Min(Triangle^.Points[0].z,Triangle^.Points[1].z),Triangle^.Points[2].z));
    LeftAABB.Max.x:=Max(LeftAABB.Max.x,Max(Max(Triangle^.Points[0].x,Triangle^.Points[1].x),Triangle^.Points[2].x));
    LeftAABB.Max.y:=Max(LeftAABB.Max.y,Max(Max(Triangle^.Points[0].y,Triangle^.Points[1].y),Triangle^.Points[2].y));
    LeftAABB.Max.z:=Max(LeftAABB.Max.z,Max(Max(Triangle^.Points[0].z,Triangle^.Points[1].z),Triangle^.Points[2].z));
   end;
   inc(LeftCount);
  end else begin
   if RightCount=0 then begin
    RightAABB.Min.x:=Min(Min(Triangle^.Points[0].x,Triangle^.Points[1].x),Triangle^.Points[2].x);
    RightAABB.Min.y:=Min(Min(Triangle^.Points[0].y,Triangle^.Points[1].y),Triangle^.Points[2].y);
    RightAABB.Min.z:=Min(Min(Triangle^.Points[0].z,Triangle^.Points[1].z),Triangle^.Points[2].z);
    RightAABB.Max.x:=Max(Max(Triangle^.Points[0].x,Triangle^.Points[1].x),Triangle^.Points[2].x);
    RightAABB.Max.y:=Max(Max(Triangle^.Points[0].y,Triangle^.Points[1].y),Triangle^.Points[2].y);
    RightAABB.Max.z:=Max(Max(Triangle^.Points[0].z,Triangle^.Points[1].z),Triangle^.Points[2].z);
   end else begin
    RightAABB.Min.x:=Min(RightAABB.Min.x,Min(Min(Triangle^.Points[0].x,Triangle^.Points[1].x),Triangle^.Points[2].x));
    RightAABB.Min.y:=Min(RightAABB.Min.y,Min(Min(Triangle^.Points[0].y,Triangle^.Points[1].y),Triangle^.Points[2].y));
    RightAABB.Min.z:=Min(RightAABB.Min.z,Min(Min(Triangle^.Points[0].z,Triangle^.Points[1].z),Triangle^.Points[2].z));
    RightAABB.Max.x:=Max(RightAABB.Max.x,Max(Max(Triangle^.Points[0].x,Triangle^.Points[1].x),Triangle^.Points[2].x));
    RightAABB.Max.y:=Max(RightAABB.Max.y,Max(Max(Triangle^.Points[0].y,Triangle^.Points[1].y),Triangle^.Points[2].y));
    RightAABB.Max.z:=Max(RightAABB.Max.z,Max(Max(Triangle^.Points[0].z,Triangle^.Points[1].z),Triangle^.Points[2].z));
   end;
   inc(RightCount);
  end;
 end;
 result:=0.0;
 if LeftCount>0 then begin
  result:=result+(LeftCount*LeftAABB.Area);
 end;
 if RightCount>0 then begin
  result:=result+(RightCount*RightAABB.Area);
 end;
 if (result<=0.0) or IsZero(result) then begin
  result:=Infinity;
 end;
end;

function TpvTriangleBVH.FindBestSplitPlaneBruteforce(const aParentTreeNode:PpvTriangleBVHTreeNode;out aAxis:TpvInt32;out aSplitPosition:TpvFloat):TpvFloat;
var AxisIndex,TriangleIndex:TpvInt32;
    Triangle:PpvTriangleBVHTriangle;
    Cost,SplitPosition:TpvFloat;
begin
 aAxis:=-1;
 aSplitPosition:=0.0;
 result:=Infinity;
 for AxisIndex:=0 to 2 do begin
  for TriangleIndex:=aParentTreeNode^.FirstTriangleIndex to aParentTreeNode^.FirstTriangleIndex+(aParentTreeNode^.CountTriangles-1) do begin
   Triangle:=@fTriangles[TriangleIndex];
   SplitPosition:=Triangle^.Center.xyz[AxisIndex];
   Cost:=EvaluateSAH(aParentTreeNode,AxisIndex,SplitPosition);
   if result>Cost then begin
    result:=Cost;
    aAxis:=AxisIndex;
    aSplitPosition:=SplitPosition;
   end;
  end;
 end;
end;

function TpvTriangleBVH.FindBestSplitPlaneSteps(const aParentTreeNode:PpvTriangleBVHTreeNode;out aAxis:TpvInt32;out aSplitPosition:TpvFloat):TpvFloat;
var AxisIndex,StepIndex:TpvInt32;
    Cost,SplitPosition,Time:TpvFloat;
begin
 aAxis:=-1;
 aSplitPosition:=0.0;
 result:=Infinity;
 for AxisIndex:=0 to 2 do begin
  for StepIndex:=0 to fBVHSubdivisionSteps-1 do begin
   Time:=(StepIndex+1)/(fBVHSubdivisionSteps+1);
   SplitPosition:=(aParentTreeNode^.Bounds.Min.xyz[AxisIndex]*(1.0-Time))+
                  (aParentTreeNode^.Bounds.Max.xyz[AxisIndex]*Time);
   Cost:=EvaluateSAH(aParentTreeNode,AxisIndex,SplitPosition);
   if result>Cost then begin
    result:=Cost;
    aAxis:=AxisIndex;
    aSplitPosition:=SplitPosition;
   end;
  end;
 end;
end;

function TpvTriangleBVH.FindBestSplitPlaneBinned(const aParentTreeNode:PpvTriangleBVHTreeNode;out aAxis:TpvInt32;out aSplitPosition:TpvFloat):TpvFloat;
const CountBINs=8;
type TBIN=record
      Count:Int32;
      Bounds:TpvAABB;
     end;
     PBIN=^TBIN;
     TBINs=array[0..CountBINs-1] of TBIN;
var AxisIndex,TriangleIndex,BINIndex,LeftSum,RightSum:TpvInt32;
    BoundsMin,BoundsMax,Scale,PlaneCost:TpvFloat;
    LeftArea,RightArea:array[0..CountBINs-1] of TpvFloat;
    LeftCount,RightCount:array[0..CountBINs-1] of TpvInt32;
    LeftBounds,RightBounds:TpvAABB;
    Triangle:PpvTriangleBVHTriangle;
    BINs:TBINs;
    BIN:PBIN;
begin

 result:=1e30;

 aAxis:=-1;

 if aParentTreeNode^.CountTriangles>0 then begin

  for AxisIndex:=0 to 2 do begin

   BoundsMin:=1e30;
   BoundsMax:=-1e30;

   for TriangleIndex:=aParentTreeNode^.FirstTriangleIndex to aParentTreeNode^.FirstTriangleIndex+(aParentTreeNode^.CountTriangles-1) do begin
    Triangle:=@fTriangles[TriangleIndex];
    BoundsMin:=Min(BoundsMin,Triangle^.Center[AxisIndex]);
    BoundsMax:=Max(BoundsMax,Triangle^.Center[AxisIndex]);
   end;

   if BoundsMin<>BoundsMax then begin

    Scale:=CountBINs/(BoundsMax-BoundsMin);

    FillChar(BINs,SizeOf(TBINs),#0);

    for TriangleIndex:=aParentTreeNode^.FirstTriangleIndex to aParentTreeNode^.FirstTriangleIndex+(aParentTreeNode^.CountTriangles-1) do begin
     Triangle:=@fTriangles[TriangleIndex];
     BINIndex:=Min(trunc((Triangle^.Center[AxisIndex]-BoundsMin)*Scale),CountBINs-1);
     BIN:=@BINs[BINIndex];
     if BIN^.Count=0 then begin
      BIN^.Bounds.Min.x:=Min(Min(Triangle^.Points[0].x,Triangle^.Points[1].x),Triangle^.Points[2].x);
      BIN^.Bounds.Min.y:=Min(Min(Triangle^.Points[0].y,Triangle^.Points[1].y),Triangle^.Points[2].y);
      BIN^.Bounds.Min.z:=Min(Min(Triangle^.Points[0].z,Triangle^.Points[1].z),Triangle^.Points[2].z);
      BIN^.Bounds.Max.x:=Max(Max(Triangle^.Points[0].x,Triangle^.Points[1].x),Triangle^.Points[2].x);
      BIN^.Bounds.Max.y:=Max(Max(Triangle^.Points[0].y,Triangle^.Points[1].y),Triangle^.Points[2].y);
      BIN^.Bounds.Max.z:=Max(Max(Triangle^.Points[0].z,Triangle^.Points[1].z),Triangle^.Points[2].z);
     end else begin
      BIN^.Bounds.Min.x:=Min(BIN^.Bounds.Min.x,Min(Min(Triangle^.Points[0].x,Triangle^.Points[1].x),Triangle^.Points[2].x));
      BIN^.Bounds.Min.y:=Min(BIN^.Bounds.Min.y,Min(Min(Triangle^.Points[0].y,Triangle^.Points[1].y),Triangle^.Points[2].y));
      BIN^.Bounds.Min.z:=Min(BIN^.Bounds.Min.z,Min(Min(Triangle^.Points[0].z,Triangle^.Points[1].z),Triangle^.Points[2].z));
      BIN^.Bounds.Max.x:=Max(BIN^.Bounds.Max.x,Max(Max(Triangle^.Points[0].x,Triangle^.Points[1].x),Triangle^.Points[2].x));
      BIN^.Bounds.Max.y:=Max(BIN^.Bounds.Max.y,Max(Max(Triangle^.Points[0].y,Triangle^.Points[1].y),Triangle^.Points[2].y));
      BIN^.Bounds.Max.z:=Max(BIN^.Bounds.Max.z,Max(Max(Triangle^.Points[0].z,Triangle^.Points[1].z),Triangle^.Points[2].z));
     end;
     inc(BIN^.Count);
    end;

    LeftSum:=0;
    RightSum:=0;
    for BINIndex:=0 to CountBINs-2 do begin

     BIN:=@BINs[BINIndex];
     inc(LeftSum,BIN^.Count);
     LeftCount[BINIndex]:=LeftSum;
     if BINIndex=0 then begin
      LeftBounds:=BIN^.Bounds;
     end else begin
      LeftBounds:=LeftBounds.Combine(BIN^.Bounds);
     end;
     LeftArea[BINIndex]:=LeftBounds.Area;

     BIN:=@BINs[CountBINs-(BINIndex+1)];
     inc(RightSum,BIN^.Count);
     RightCount[CountBINs-(BINIndex+2)]:=RightSum;
     if BINIndex=0 then begin
      RightBounds:=BIN^.Bounds;
     end else begin
      RightBounds:=RightBounds.Combine(BIN^.Bounds);
     end;
     RightArea[CountBINs-(BINIndex+2)]:=RightBounds.Area;

    end;

    Scale:=(BoundsMax-BoundsMin)/CountBINs;
    for BINIndex:=0 to CountBINs-2 do begin
     PlaneCost:=(LeftCount[BINIndex]*LeftArea[BINIndex])+(RightCount[BINIndex]*RightArea[BINIndex]);
     if PlaneCost<result then begin
      result:=PlaneCost;
      aAxis:=AxisIndex;
      aSplitPosition:=BoundsMin+((BINIndex+1)*Scale);
     end;
    end;

   end;

  end;

 end;

end;

function TpvTriangleBVH.CalculateNodeCost(const aParentTreeNode:PpvTriangleBVHTreeNode):TpvFloat;
begin
 result:=aParentTreeNode^.Bounds.Area*aParentTreeNode^.CountTriangles;
end;

procedure TpvTriangleBVH.UpdateNodeBounds(const aParentTreeNode:PpvTriangleBVHTreeNode);
var TriangleIndex:TpvInt32;
    Triangle:PpvTriangleBVHTriangle;
begin
 if aParentTreeNode^.CountTriangles>0 then begin
  Triangle:=@fTriangles[aParentTreeNode^.FirstTriangleIndex];
  aParentTreeNode.Bounds.Min.x:=Min(Min(Triangle^.Points[0].x,Triangle^.Points[1].x),Triangle^.Points[2].x);
  aParentTreeNode.Bounds.Min.y:=Min(Min(Triangle^.Points[0].y,Triangle^.Points[1].y),Triangle^.Points[2].y);
  aParentTreeNode.Bounds.Min.z:=Min(Min(Triangle^.Points[0].z,Triangle^.Points[1].z),Triangle^.Points[2].z);
  aParentTreeNode.Bounds.Max.x:=Max(Max(Triangle^.Points[0].x,Triangle^.Points[1].x),Triangle^.Points[2].x);
  aParentTreeNode.Bounds.Max.y:=Max(Max(Triangle^.Points[0].y,Triangle^.Points[1].y),Triangle^.Points[2].y);
  aParentTreeNode.Bounds.Max.z:=Max(Max(Triangle^.Points[0].z,Triangle^.Points[1].z),Triangle^.Points[2].z);
  for TriangleIndex:=aParentTreeNode^.FirstTriangleIndex+1 to aParentTreeNode^.FirstTriangleIndex+(aParentTreeNode^.CountTriangles-1) do begin
   Triangle:=@fTriangles[TriangleIndex];
   aParentTreeNode.Bounds.Min.x:=Min(aParentTreeNode.Bounds.Min.x,Min(Min(Triangle^.Points[0].x,Triangle^.Points[1].x),Triangle^.Points[2].x));
   aParentTreeNode.Bounds.Min.y:=Min(aParentTreeNode.Bounds.Min.y,Min(Min(Triangle^.Points[0].y,Triangle^.Points[1].y),Triangle^.Points[2].y));
   aParentTreeNode.Bounds.Min.z:=Min(aParentTreeNode.Bounds.Min.z,Min(Min(Triangle^.Points[0].z,Triangle^.Points[1].z),Triangle^.Points[2].z));
   aParentTreeNode.Bounds.Max.x:=Max(aParentTreeNode.Bounds.Max.x,Max(Max(Triangle^.Points[0].x,Triangle^.Points[1].x),Triangle^.Points[2].x));
   aParentTreeNode.Bounds.Max.y:=Max(aParentTreeNode.Bounds.Max.y,Max(Max(Triangle^.Points[0].y,Triangle^.Points[1].y),Triangle^.Points[2].y));
   aParentTreeNode.Bounds.Max.z:=Max(aParentTreeNode.Bounds.Max.z,Max(Max(Triangle^.Points[0].z,Triangle^.Points[1].z),Triangle^.Points[2].z));
  end;
 end;
end;

procedure TpvTriangleBVH.ProcessNodeQueue;
var ParentTreeNodeIndex,AxisIndex,
    LeftIndex,RightIndex,
    LeftCount,
    LeftChildIndex,RightChildIndex:TpvInt32;
    SplitPosition,SplitCost:TpvFloat;
    ParentTreeNode,ChildTreeNode:PpvTriangleBVHTreeNode;
    TemporaryTriangle:TpvTriangleBVHTriangle;
    Added:boolean;
begin
 while (fCountActiveWorkers<>0) or not fNodeQueue.IsEmpty do begin

  Added:=false;

  while fNodeQueue.Dequeue(ParentTreeNodeIndex) do begin

   if not Added then begin
    TPasMPInterlocked.Increment(fCountActiveWorkers);
    Added:=true;
   end;

   ParentTreeNode:=@fTreeNodes[ParentTreeNodeIndex];
   if ParentTreeNode^.CountTriangles>0 then begin

    case fBVHBuildMode of
     TpvTriangleBVHBuildMode.SAHSteps:begin
      SplitCost:=FindBestSplitPlaneSteps(ParentTreeNode,AxisIndex,SplitPosition);
     end;
     TpvTriangleBVHBuildMode.SAHBinned:begin
      SplitCost:=FindBestSplitPlaneBinned(ParentTreeNode,AxisIndex,SplitPosition);
     end;
     else begin
      SplitCost:=FindBestSplitPlaneBruteforce(ParentTreeNode,AxisIndex,SplitPosition);
     end;
    end;

    if SplitCost<CalculateNodeCost(ParentTreeNode) then begin

     LeftIndex:=ParentTreeNode^.FirstTriangleIndex;
     RightIndex:=ParentTreeNode^.FirstTriangleIndex+(ParentTreeNode^.CountTriangles-1);
     while LeftIndex<=RightIndex do begin
      if fTriangles[LeftIndex].Center[AxisIndex]<SplitPosition then begin
       inc(LeftIndex);
      end else begin
       TemporaryTriangle:=fTriangles[LeftIndex];
       fTriangles[LeftIndex]:=fTriangles[RightIndex];
       fTriangles[RightIndex]:=TemporaryTriangle;
       dec(RightIndex);
      end;
     end;

     LeftCount:=LeftIndex-ParentTreeNode^.FirstTriangleIndex;

     if (LeftCount<>0) and (LeftCount<>ParentTreeNode^.CountTriangles) then begin

      LeftChildIndex:=TPasMPInterlocked.Add(fCountTreeNodes,2);
      RightChildIndex:=LeftChildIndex+1;

      ParentTreeNode^.FirstLeftChild:=LeftChildIndex;

      ChildTreeNode:=@fTreeNodes[LeftChildIndex];
      ChildTreeNode^.FirstTriangleIndex:=ParentTreeNode^.FirstTriangleIndex;
      ChildTreeNode^.CountTriangles:=LeftCount;
      ChildTreeNode^.FirstLeftChild:=-1;
      UpdateNodeBounds(ChildTreeNode);
      fNodeQueue.Enqueue(LeftChildIndex);

      ChildTreeNode:=@fTreeNodes[RightChildIndex];
      ChildTreeNode^.FirstTriangleIndex:=LeftIndex;
      ChildTreeNode^.CountTriangles:=ParentTreeNode^.CountTriangles-LeftCount;
      ChildTreeNode^.FirstLeftChild:=-1;
      UpdateNodeBounds(ChildTreeNode);
      fNodeQueue.Enqueue(RightChildIndex);

     end;

    end;

   end;

  end;

  if Added then begin
   TPasMPInterlocked.Decrement(fCountActiveWorkers);
  end;

 end;
end;

procedure TpvTriangleBVH.BuildJob(const Job:PPasMPJob;const ThreadIndex:TPasMPInt32);
begin
 ProcessNodeQueue;
end;

procedure TpvTriangleBVH.Build;
var JobIndex,TreeNodeIndex,SkipListNodeIndex:TPasMPInt32;
    TreeNode:PpvTriangleBVHTreeNode;
    Jobs:array of PPasMPJob;
    StackItem:TpvUInt64;
    SkipListNode:PpvTriangleBVHSkipListNode;
begin

 if length(fTreeNodes)<=Max(1,length(fTriangles)) then begin
  SetLength(fTreeNodes,Max(1,length(fTriangles)*2));
 end;

 fCountTreeNodes:=1;
 fTreeNodeRoot:=0;
 TreeNode:=@fTreeNodes[fTreeNodeRoot];
 TreeNode^.Bounds:=fBounds;
 TreeNode^.FirstLeftChild:=-1;
 if fCountTriangles>0 then begin
  TreeNode^.FirstTriangleIndex:=0;
  TreeNode^.CountTriangles:=fCountTriangles;
  if fCountTriangles>=MaximumTrianglesPerNode then begin
   fNodeQueue.Clear;
   fNodeQueue.Enqueue(0);
   fCountActiveWorkers:=0;
   if assigned(fPasMPInstance) and (fPasMPInstance.CountJobWorkerThreads>0) then begin
    Jobs:=nil;
    try
     SetLength(Jobs,fPasMPInstance.CountJobWorkerThreads);
     for JobIndex:=0 to length(Jobs)-1 do begin
      Jobs[JobIndex]:=fPasMPInstance.Acquire(BuildJob,self,nil,0,0);
     end;
     fPasMPInstance.Invoke(Jobs);
    finally
     Jobs:=nil;
    end;
   end else begin
    ProcessNodeQueue;
   end;
  end;
 end else begin
  TreeNode^.FirstTriangleIndex:=-1;
  TreeNode^.CountTriangles:=0;
 end;

 if length(fSkipListNodeMap)<=fCountTreeNodes then begin
  SetLength(fSkipListNodeMap,fCountTreeNodes+((fCountTreeNodes+1) shr 1));
 end;
 if length(fSkipListNodes)<=fCountTreeNodes then begin
  SetLength(fSkipListNodes,fCountTreeNodes+((fCountTreeNodes+1) shr 1));
 end;
 fCountSkipListNodes:=0;
 fTreeNodeStack.Push((TpvUInt64(fTreeNodeRoot) shl 1) or 0);
 while fTreeNodeStack.Pop(StackItem) do begin
  TreeNodeIndex:=StackItem shr 1;
  TreeNode:=@fTreeNodes[TreeNodeIndex];
  case StackItem and 1 of
   0:begin
    SkipListNodeIndex:=fCountSkipListNodes;
    inc(fCountSkipListNodes);
    SkipListNode:=@fSkipListNodes[SkipListNodeIndex];
    fSkipListNodeMap[TreeNodeIndex]:=SkipListNodeIndex;
    SkipListNode^.Min.xyz:=TreeNode^.Bounds.Min;
    SkipListNode^.Min.w:=0.0;
    SkipListNode^.Max.xyz:=TreeNode^.Bounds.Max;
    SkipListNode^.Max.w:=0.0;
    if TreeNode^.FirstLeftChild>=0 then begin
     // No leaf
     SkipListNode^.FirstTriangleIndex:=-1;
     SkipListNode^.CountTriangles:=0;
    end else begin
     // Leaf
     SkipListNode^.FirstTriangleIndex:=TreeNode^.FirstTriangleIndex;
     SkipListNode^.CountTriangles:=TreeNode^.CountTriangles;
    end;
    SkipListNode^.SkipCount:=0;
    SkipListNode^.Dummy:=0;
    fTreeNodeStack.Push((TpvUInt64(TreeNodeIndex) shl 1) or 1);
    if TreeNode^.FirstLeftChild>=0 then begin
     fTreeNodeStack.Push((TpvUInt64(TreeNode^.FirstLeftChild+1) shl 1) or 0);
     fTreeNodeStack.Push((TpvUInt64(TreeNode^.FirstLeftChild+0) shl 1) or 0);
    end;
   end;
   else {1:}begin
    SkipListNodeIndex:=fSkipListNodeMap[TreeNodeIndex];
    fSkipListNodes[SkipListNodeIndex].SkipCount:=fCountSkipListNodes-SkipListNodeIndex;
   end;
  end;
 end;

end;

procedure TpvTriangleBVH.LoadFromStream(const aStream:TStream);
var Signature:TTriangleBVHFileSignature;
    Version:TpvUInt32;
    CountTriangles,CountTreeNodes,CountSkipListNodes:TpvUInt32;
   {TriangleIndex,TreeNodeIndex,SkipListNodeIndex:TpvSizeInt;
    Triangle:PpvTriangleBVHTriangle;
    TreeNode:PpvTriangleBVHTreeNode;
    SkipListNode:PpvTriangleBVHSkipListNode;}
begin

 aStream.ReadBuffer(Signature,SizeOf(TriangleBVHFileSignature));
 if Signature<>TriangleBVHFileSignature then begin
  raise EpvTriangleBVH.Create('Invalid signature');
 end;

 aStream.ReadBuffer(Version,SizeOf(TpvUInt32));
 if Version<>TriangleBVHFileVersion then begin
  raise EpvTriangleBVH.Create('Invalid version');
 end;

 aStream.ReadBuffer(CountTriangles,SizeOf(TpvUInt32));

 aStream.ReadBuffer(CountTreeNodes,SizeOf(TpvUInt32));

 aStream.ReadBuffer(CountSkipListNodes,SizeOf(TpvUInt32));

 Clear;

 SetLength(fTriangles,CountTriangles);

 SetLength(fTreeNodes,CountTreeNodes);

 SetLength(fSkipListNodes,CountSkipListNodes);

{$if true}

 if CountTriangles>0 then begin
  aStream.ReadBuffer(fTriangles[0],CountTriangles*SizeOf(TpvTriangleBVHTriangle));
 end;

 if CountTreeNodes>0 then begin
  aStream.ReadBuffer(fTreeNodes[0],CountTreeNodes*SizeOf(TpvTriangleBVHTreeNode));
 end;

 if CountSkipListNodes>0 then begin
  aStream.ReadBuffer(fSkipListNodes[0],CountSkipListNodes*SizeOf(TpvTriangleBVHSkipListNode));
 end;

{$else}
 for TriangleIndex:=0 to CountTriangles-1 do begin
  Triangle:=@fTriangles[TriangleIndex];
  aStream.ReadBuffer(Triangle^.Points[0],SizeOf(TpvVector3));
  aStream.ReadBuffer(Triangle^.Points[1],SizeOf(TpvVector3));
  aStream.ReadBuffer(Triangle^.Points[2],SizeOf(TpvVector3));
  aStream.ReadBuffer(Triangle^.Normal,SizeOf(TpvVector3));
  aStream.ReadBuffer(Triangle^.Center,SizeOf(TpvVector3));
  aStream.ReadBuffer(Triangle^.Data,SizeOf(TpvPtrInt));
  aStream.ReadBuffer(Triangle^.Flags,SizeOf(TpvUInt32));
 end;

 for TreeNodeIndex:=0 to CountTreeNodes-1 do begin
  TreeNode:=@fTreeNodes[TreeNodeIndex];
  aStream.ReadBuffer(TreeNode^.Bounds,SizeOf(TpvAABB));
  aStream.ReadBuffer(TreeNode^.FirstLeftChild,SizeOf(TpvInt32));
  aStream.ReadBuffer(TreeNode^.FirstTriangleIndex,SizeOf(TpvInt32));
  aStream.ReadBuffer(TreeNode^.CountTriangles,SizeOf(TpvInt32));
 end;

 for SkipListNodeIndex:=0 to CountSkipListNodes-1 do begin
  SkipListNode:=@fSkipListNodes[SkipListNodeIndex];
  aStream.ReadBuffer(SkipListNode^.Min,SizeOf(TpvVector4));
  aStream.ReadBuffer(SkipListNode^.Max,SizeOf(TpvVector4));
  aStream.ReadBuffer(SkipListNode^.FirstTriangleIndex,SizeOf(TpvInt32));
  aStream.ReadBuffer(SkipListNode^.CountTriangles,SizeOf(TpvInt32));
  aStream.ReadBuffer(SkipListNode^.SkipCount,SizeOf(TpvInt32));
  aStream.ReadBuffer(SkipListNode^.Dummy,SizeOf(TpvInt32));
 end;
{$ifend} 

end;

procedure TpvTriangleBVH.SaveToStream(const aStream:TStream);
var Signature:TTriangleBVHFileSignature;
    Version:TpvUInt32;
    CountTriangles,CountTreeNodes,CountSkipListNodes:TpvUInt32;
   {TriangleIndex,TreeNodeIndex,SkipListNodeIndex:TpvSizeInt;
    Triangle:PpvTriangleBVHTriangle;
    TreeNode:PpvTriangleBVHTreeNode;
    SkipListNode:PpvTriangleBVHSkipListNode;}
begin
 
 Signature:=TriangleBVHFileSignature;
 aStream.WriteBuffer(Signature,SizeOf(TriangleBVHFileSignature));

 Version:=TriangleBVHFileVersion;
 aStream.WriteBuffer(Version,SizeOf(TpvUInt32));

 CountTriangles:=fCountTriangles;
 aStream.WriteBuffer(CountTriangles,SizeOf(TpvUInt32));

 CountTreeNodes:=fCountTreeNodes;
 aStream.WriteBuffer(CountTreeNodes,SizeOf(TpvUInt32));

 CountSkipListNodes:=fCountSkipListNodes;
 aStream.WriteBuffer(CountSkipListNodes,SizeOf(TpvUInt32));

{$if true}

 if fCountTriangles>0 then begin
  aStream.WriteBuffer(fTriangles[0],fCountTriangles*SizeOf(TpvTriangleBVHTriangle));
 end; 

 if fCountTreeNodes>0 then begin
  aStream.WriteBuffer(fTreeNodes[0],fCountTreeNodes*SizeOf(TpvTriangleBVHTreeNode));
 end; 

 if fCountSkipListNodes>0 then begin 
  aStream.WriteBuffer(fSkipListNodes[0],fCountSkipListNodes*SizeOf(TpvTriangleBVHSkipListNode));
 end; 

{$else}
 for TriangleIndex:=0 to fCountTriangles-1 do begin
  Triangle:=@fTriangles[TriangleIndex];
  aStream.WriteBuffer(Triangle^.Points[0],SizeOf(TpvVector3));
  aStream.WriteBuffer(Triangle^.Points[1],SizeOf(TpvVector3));
  aStream.WriteBuffer(Triangle^.Points[2],SizeOf(TpvVector3));
  aStream.WriteBuffer(Triangle^.Normal,SizeOf(TpvVector3));
  aStream.WriteBuffer(Triangle^.Center,SizeOf(TpvVector3));
  aStream.WriteBuffer(Triangle^.Data,SizeOf(TpvPtrInt));
  aStream.WriteBuffer(Triangle^.Flags,SizeOf(TpvUInt32));
 end;

 for TreeNodeIndex:=0 to fCountTreeNodes-1 do begin
  TreeNode:=@fTreeNodes[TreeNodeIndex];
  aStream.WriteBuffer(TreeNode^.Bounds,SizeOf(TpvAABB));
  aStream.WriteBuffer(TreeNode^.FirstLeftChild,SizeOf(TpvInt32));
  aStream.WriteBuffer(TreeNode^.FirstTriangleIndex,SizeOf(TpvInt32));
  aStream.WriteBuffer(TreeNode^.CountTriangles,SizeOf(TpvInt32));
 end;

 for SkipListNodeIndex:=0 to fCountSkipListNodes-1 do begin
  SkipListNode:=@fSkipListNodes[SkipListNodeIndex];
  aStream.WriteBuffer(SkipListNode^.Min,SizeOf(TpvVector4));
  aStream.WriteBuffer(SkipListNode^.Max,SizeOf(TpvVector4));
  aStream.WriteBuffer(SkipListNode^.FirstTriangleIndex,SizeOf(TpvInt32));
  aStream.WriteBuffer(SkipListNode^.CountTriangles,SizeOf(TpvInt32));
  aStream.WriteBuffer(SkipListNode^.SkipCount,SizeOf(TpvInt32));
  aStream.WriteBuffer(SkipListNode^.Dummy,SizeOf(TpvInt32));
 end;
{$ifend} 

end;

function TpvTriangleBVH.RayIntersection(const aRay:TpvTriangleBVHRay;var aIntersection:TpvTriangleBVHIntersection;const aFastCheck:boolean;const aFlags:TpvUInt32;const aAvoidFlags:TpvUInt32):boolean;
var SkipListNodeIndex,CountSkipListNodes,TriangleIndex:TpvInt32;
    SkipListNode:PpvTriangleBVHSkipListNode;
    Triangle:PpvTriangleBVHTriangle;
    Time,u,v,w:TpvScalar;
    OK:boolean;
begin
 result:=false;
 SkipListNodeIndex:=0;
 CountSkipListNodes:=fCountSkipListNodes;
 while SkipListNodeIndex<CountSkipListNodes do begin
  SkipListNode:=@fSkipListNodes[SkipListNodeIndex];
  if TpvAABB.FastRayIntersection(SkipListNode^.Min.Vector3,SkipListNode^.Max.Vector3,aRay.Origin,aRay.Direction) then begin
   for TriangleIndex:=SkipListNode^.FirstTriangleIndex to (SkipListNode^.FirstTriangleIndex+SkipListNode^.CountTriangles)-1 do begin
    Triangle:=@fTriangles[TriangleIndex];
    if ((Triangle^.Flags and aFlags)<>0) and ((Triangle^.Flags and aAvoidFlags)=0) then begin
     OK:=Triangle^.RayIntersection(aRay,Time,u,v,w);
     if OK then begin
      if IsInfinite(aIntersection.Time) or (Time<aIntersection.Time) then begin
       result:=true;
       aIntersection.Time:=Time;
       aIntersection.Triangle:=Triangle;
       aIntersection.IntersectionPoint:=aRay.Origin+(aRay.Direction*Time);
       aIntersection.Barycentrics:=TpvVector3.InlineableCreate(u,v,w);
       if aFastCheck then begin
        exit;
       end;
      end;
     end;
    end;
   end;
   inc(SkipListNodeIndex);
  end else begin
   if SkipListNode^.SkipCount=0 then begin
    break;
   end else begin
    inc(SkipListNodeIndex,SkipListNode^.SkipCount);
   end;
  end;
 end;
end;

function TpvTriangleBVH.CountRayIntersections(const aRay:TpvTriangleBVHRay;const aFlags:TpvUInt32;const aAvoidFlags:TpvUInt32):TpvInt32;
var SkipListNodeIndex,CountSkipListNodes,TriangleIndex:TpvInt32;
    SkipListNode:PpvTriangleBVHSkipListNode;
    Triangle:PpvTriangleBVHTriangle;
    Time,u,v,w:TpvScalar;
begin
 result:=0;
 SkipListNodeIndex:=0;
 CountSkipListNodes:=fCountSkipListNodes;
 while SkipListNodeIndex<CountSkipListNodes do begin
  SkipListNode:=@fSkipListNodes[SkipListNodeIndex];
  if TpvAABB.FastRayIntersection(SkipListNode^.Min.Vector3,SkipListNode^.Max.Vector3,aRay.Origin,aRay.Direction) then begin
   for TriangleIndex:=SkipListNode^.FirstTriangleIndex to (SkipListNode^.FirstTriangleIndex+SkipListNode^.CountTriangles)-1 do begin
    Triangle:=@fTriangles[TriangleIndex];
    if ((Triangle^.Flags and aFlags)<>0) and ((Triangle^.Flags and aAvoidFlags)=0) then begin
     if Triangle^.RayIntersection(aRay,Time,u,v,w) then begin
      inc(result);
     end;
    end;
   end;
   inc(SkipListNodeIndex);
  end else begin
   if SkipListNode^.SkipCount=0 then begin
    break;
   end else begin
    inc(SkipListNodeIndex,SkipListNode^.SkipCount);
   end;
  end;
 end;
end;

function TpvTriangleBVH.LineIntersection(const aV0,aV1:TpvVector3;const aFlags:TpvUInt32;const aAvoidFlags:TpvUInt32):boolean;
var Ray:TpvTriangleBVHRay;
    Intersection:TpvTriangleBVHIntersection;
    Distance:TpvFloat;
begin
 result:=false;
 FillChar(Intersection,SizeOf(TpvTriangleBVHIntersection),AnsiChar(#0));
 Ray.Origin:=aV0;
 Ray.Direction:=aV1-aV0;
 Distance:=Ray.Direction.Length;
 Ray.Direction:=Ray.Direction/Distance;
 Intersection.Time:=Distance;
 if RayIntersection(Ray,Intersection,true,aFlags,aAvoidFlags) then begin
  if Intersection.Time<Distance then begin
   result:=true;
  end;
 end;
end;

function TpvTriangleBVH.IsOpenSpacePerEvenOddRule(const aPosition:TpvVector3;const aFlags:TpvUInt32;const aAvoidFlags:TpvUInt32):boolean;
const Directions:array[0..5] of TpvVector3=((x:-1.0;y:0.0;z:0.0),
                                            (x:1.0;y:0.0;z:0.0),
                                            (x:0.0;y:1.0;z:0.0),
                                            (x:0.0;y:-1.0;z:0.0),
                                            (x:0.0;y:0.0;z:1.0),
                                            (x:0.0;y:0.0;z:-1.0));
var DirectionIndex,Count:TpvInt32;
    Ray:TpvTriangleBVHRay;
begin
 Count:=0;
 Ray.Origin:=aPosition;
 for DirectionIndex:=low(Directions) to high(Directions) do begin
  Ray.Direction:=Directions[DirectionIndex];
  inc(Count,CountRayIntersections(Ray,aFlags,aAvoidFlags));
 end;
 // When it's even = Outside any mesh, so we are in open space
 // When it's odd = Inside a mesh, so we are not in open space
 result:=(Count and 1)=0;
end;

function TpvTriangleBVH.IsOpenSpacePerEvenOddRule(const aPosition:TpvVector3;out aNearestNormal,aNearestNormalPosition:TpvVector3;const aFlags:TpvUInt32;const aAvoidFlags:TpvUInt32):boolean;
const Directions:array[0..5] of TpvVector3=((x:-1.0;y:0.0;z:0.0),
                                            (x:1.0;y:0.0;z:0.0),
                                            (x:0.0;y:1.0;z:0.0),
                                            (x:0.0;y:-1.0;z:0.0),
                                            (x:0.0;y:0.0;z:1.0),
                                            (x:0.0;y:0.0;z:-1.0));
var DirectionIndex,Count:TpvInt32;
    Ray:TpvTriangleBVHRay;
    Intersection:TpvTriangleBVHIntersection;
    Direction:TpvVector3;
    Distance,BestDistance:TpvFloat;
begin
 Count:=0;
 Ray.Origin:=aPosition;
 for DirectionIndex:=low(Directions) to high(Directions) do begin
  Ray.Direction:=Directions[DirectionIndex];
  inc(Count,CountRayIntersections(Ray));
 end;
 // When it's even = Outside any mesh, so we are in open space
 // When it's odd = Inside a mesh, so we are not in open space
 result:=(Count and 1)=0;
 if result then begin
  Count:=0;
  BestDistance:=Infinity;
  for DirectionIndex:=low(Directions) to high(Directions) do begin
   Ray.Direction:=Directions[DirectionIndex];
   FillChar(Intersection,SizeOf(TpvTriangleBVHIntersection),AnsiChar(#0));
   Intersection.Time:=Infinity;
   if RayIntersection(Ray,Intersection,false,aFlags,aAvoidFlags) then begin
    Direction:=Intersection.IntersectionPoint-aPosition;
    Distance:=Direction.Length;
    Direction:=Direction/Distance;
    if Direction.Dot(Intersection.Triangle^.Normal)>0.0 then begin
     if (Count=0) or (BestDistance>Distance) then begin
      aNearestNormal:=Intersection.Triangle^.Normal;
      aNearestNormalPosition:=Intersection.IntersectionPoint+(Intersection.Triangle^.Normal*1e-2);
      BestDistance:=Distance;
      inc(Count);
     end;
    end;
   end;
  end;
 end;
end;

function TpvTriangleBVH.IsOpenSpacePerNormals(const aPosition:TpvVector3;const aFlags:TpvUInt32;const aAvoidFlags:TpvUInt32):boolean;
const Directions:array[0..5] of TpvVector3=((x:-1.0;y:0.0;z:0.0),
                                            (x:1.0;y:0.0;z:0.0),
                                            (x:0.0;y:1.0;z:0.0),
                                            (x:0.0;y:-1.0;z:0.0),
                                            (x:0.0;y:0.0;z:1.0),
                                            (x:0.0;y:0.0;z:-1.0));
var DirectionIndex:TpvInt32;
    Ray:TpvTriangleBVHRay;
    Intersection:TpvTriangleBVHIntersection;
begin
 result:=true;
 Ray.Origin:=aPosition;
 for DirectionIndex:=low(Directions) to high(Directions) do begin
  Ray.Direction:=Directions[DirectionIndex];
  FillChar(Intersection,SizeOf(TpvTriangleBVHIntersection),AnsiChar(#0));
  Intersection.Time:=Infinity;
  if RayIntersection(Ray,Intersection,false,aFlags,aAvoidFlags) then begin
   if ((Intersection.IntersectionPoint-aPosition).Normalize).Dot(Intersection.Triangle^.Normal)>0.0 then begin
    // Hit point normal is pointing away from us, so we are not in open space and inside a mesh
    result:=false;
    break;
   end;
  end;
 end;
end;

function TpvTriangleBVH.IsOpenSpacePerNormals(const aPosition:TpvVector3;out aNearestNormal,aNearestNormalPosition:TpvVector3;const aFlags:TpvUInt32;const aAvoidFlags:TpvUInt32):boolean;
const Directions:array[0..5] of TpvVector3=((x:-1.0;y:0.0;z:0.0),
                                            (x:1.0;y:0.0;z:0.0),
                                            (x:0.0;y:1.0;z:0.0),
                                            (x:0.0;y:-1.0;z:0.0),
                                            (x:0.0;y:0.0;z:1.0),
                                            (x:0.0;y:0.0;z:-1.0));
var DirectionIndex:TpvInt32;
    Ray:TpvTriangleBVHRay;
    Intersection:TpvTriangleBVHIntersection;
    Direction:TpvVector3;
    Distance,BestDistance:TpvFloat;
begin
 result:=true;
 Ray.Origin:=aPosition;
 BestDistance:=Infinity;
 for DirectionIndex:=low(Directions) to high(Directions) do begin
  Ray.Direction:=Directions[DirectionIndex];
  FillChar(Intersection,SizeOf(TpvTriangleBVHIntersection),AnsiChar(#0));
  Intersection.Time:=Infinity;
  if RayIntersection(Ray,Intersection,false,aFlags,aAvoidFlags) then begin
   Direction:=Intersection.IntersectionPoint-aPosition;
   Distance:=Direction.Length;
   Direction:=Direction/Distance;;
   if Direction.Dot(Intersection.Triangle^.Normal)>0.0 then begin
    // Hit point normal is pointing away from us, so we are not in open space and inside a mesh
    if result or (BestDistance>Distance) then begin
     aNearestNormal:=Intersection.Triangle^.Normal;
     aNearestNormalPosition:=Intersection.IntersectionPoint+(Intersection.Triangle^.Normal*1e-2);
     BestDistance:=Distance;
    end;
    result:=false;
   end;
  end;
 end;
end;

end.
