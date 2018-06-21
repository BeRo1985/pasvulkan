(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2018, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.CSG.BSP;
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

{$warnings off}

interface

uses SysUtils,Classes,Math,
     PasVulkan.Types,
     PasVulkan.Math;

type TpvCSGBSP=class
      public
       const Epsilon=1e-5;
             OneMinusEpsilon=1.0-Epsilon;
             OnePlusEpsilon=1.0+Epsilon;
             TJunctionEpsilon=1e-4;
             TJunctionOneMinusEpsilon=1.0-TJunctionEpsilon;
             TJunctionOnePlusEpsilon=1.0+TJunctionEpsilon;
             daabbtNULLNODE=-1;
             AABBMULTIPLIER=2.0;
       type TFloat=TpvDouble;
            PFloat=^TFloat;
            TDynamicArray<T>=record
             public
              Items:array of T;
              Count:TpvSizeInt;
              procedure Initialize;
              procedure Finalize;
              procedure Clear;
              procedure Finish;
              procedure Assign(const aFrom:{$ifdef fpc}TpvCSGBSP.{$endif}TDynamicArray<T>); overload;
              procedure Assign(const aItems:array of T); overload;
              function AddNew:TpvSizeInt; overload;
              function Add(const aItem:T):TpvSizeInt; overload;
              function Add(const aItems:array of T):TpvSizeInt; overload;
              function Add(const aFrom:{$ifdef fpc}TpvCSGBSP.{$endif}TDynamicArray<T>):TpvSizeInt; overload;
              function AddRangeFrom(const aFrom:{$ifdef fpc}TpvCSGBSP.{$endif}TDynamicArray<T>;const aStartIndex,aCount:TpvSizeInt):TpvSizeInt; overload;
              procedure Exchange(const aIndexA,aIndexB:TpvSizeInt); inline;
            end;
            TDynamicStack<T>=record
             public
              Items:array of T;
              Count:TpvSizeInt;
              procedure Initialize;
              procedure Finalize;
              procedure Push(const aItem:T);
              function Pop(out aItem:T):boolean;
            end;
            TVector2=record
             public
              x,y:TFloat;
              constructor Create(const aFrom:TpvVector2);
              class operator Equal(const aLeft,aRight:TVector2):boolean;
              class operator NotEqual(const aLeft,aRight:TVector2):boolean;
              class operator Add(const aLeft,aRight:TVector2):TVector2;
              class operator Subtract(const aLeft,aRight:TVector2):TVector2;
              class operator Multiply(const aLeft:TVector2;const aRight:TFloat):TVector2;
              class operator Divide(const aLeft:TVector2;const aRight:TFloat):TVector2;
              class operator Negative(const aVector:TVector2):TVector2;
              function Lerp(const aWith:TVector2;const aTime:TFloat):TVector2;
              function ToVector:TpvVector2;
            end;
            PVector2=^TVector2;
            TVector3=record
             public
              x,y,z:TFloat;
              constructor Create(const aFrom:TpvVector3);
              class operator Equal(const aLeft,aRight:TVector3):boolean;
              class operator NotEqual(const aLeft,aRight:TVector3):boolean;
              class operator Add(const aLeft,aRight:TVector3):TVector3;
              class operator Subtract(const aLeft,aRight:TVector3):TVector3;
              class operator Multiply(const aLeft:TVector3;const aRight:TFloat):TVector3;
              class operator Divide(const aLeft:TVector3;const aRight:TFloat):TVector3;
              class operator Negative(const aVector:TVector3):TVector3;
              function Cross(const aWith:TVector3):TVector3;
              function Spacing(const aWith:TVector3):TFloat;
              function Dot(const aWith:TVector3):TFloat;
              function Length:TFloat;
              function SquaredLength:TFloat;
              function Lerp(const aWith:TVector3;const aTime:TFloat):TVector3;
              function Normalize:TVector3;
              function ToVector:TpvVector3;
            end;
            PVector3=^TVector3;
            TVector4=record
             public
              x,y,z,w:TFloat;
              constructor Create(const aFrom:TpvVector4);
              class operator Equal(const aLeft,aRight:TVector4):boolean;
              class operator NotEqual(const aLeft,aRight:TVector4):boolean;
              class operator Add(const aLeft,aRight:TVector4):TVector4;
              class operator Subtract(const aLeft,aRight:TVector4):TVector4;
              class operator Multiply(const aLeft:TVector4;const aRight:TFloat):TVector4;
              class operator Divide(const aLeft:TVector4;const aRight:TFloat):TVector4;
              class operator Negative(const aVector:TVector4):TVector4;
              function Lerp(const aWith:TVector4;const aTime:TFloat):TVector4;
              function ToVector:TpvVector4;
            end;
            PVector4=^TVector4;
            TVertex=record
             public
              Position:TVector3;
              Normal:TVector3;
              TexCoord:TVector4;
              Color:TVector4;
              class operator Equal(const aLeft,aRight:TVertex):boolean;
              class operator NotEqual(const aLeft,aRight:TVertex):boolean;
              class operator Add(const aLeft,aRight:TVertex):TVertex;
              class operator Subtract(const aLeft,aRight:TVertex):TVertex;
              class operator Multiply(const aLeft:TVertex;const aRight:TFloat):TVertex;
              class operator Divide(const aLeft:TVertex;const aRight:TFloat):TVertex;
              class operator Negative(const aLeft:TVertex):TVertex;
              function Lerp(const aWith:TVertex;const aTime:TFloat):TVertex;
              procedure Flip;
              function CloneFlip:TVertex;
              function Normalize:TVertex;
            end;
            TAABB=record
             public
              Min:TVector3;
              Max:TVector3;
              function Cost:TFloat;
              function Combine(const aAABB:TAABB):TAABB;
              function Contains(const aAABB:TAABB;const aThreshold:TFloat=0.0):boolean;
              function Intersects(const aAABB:TAABB;const aThreshold:TFloat=Epsilon):boolean;
            end;
            PAABB=^TAABB;
            PVertex=^TVertex;
            TVertexList=TDynamicArray<TVertex>;
            PVertexList=^TVertexList;
            TIndex=TpvSizeInt;
            PIndex=^TIndex;
            TIndexList=TDynamicArray<TIndex>;
            PIndexList=^TIndexList;
            TPlane=record
             public
              Normal:TVector3;
              Distance:TFloat;
              constructor Create(const aV0,aV1,aV2:TVector3); overload;
              constructor Create(const aNormal:TVector3;const aDistance:TFloat); overload;
              class function CreateEmpty:TPlane; static;
              function DistanceTo(const aWith:TVector3):TFloat;
              function IntersectWithPointToPoint(const aPointA,aPointB:TVector3;const aIntersectionPoint:PVector3=nil):boolean; overload;
              function IntersectWithPointToPoint(const aPointA,aPointB:TVertex;const aIntersectionPoint:PVertex=nil):boolean; overload;
              function OK:boolean;
              function Flip:TPlane;
              procedure SplitTriangles(var aVertices:TVertexList;
                                       const aIndices:TIndexList;
                                       const aCoplanarBackList:PIndexList;
                                       const aCoplanarFrontList:PIndexList;
                                       const aBackList:PIndexList;
                                       const aFrontList:PIndexList);
              procedure SplitPolygons(var aVertices:TVertexList;
                                      const aIndices:TIndexList;
                                      const aCoplanarBackList:PIndexList;
                                      const aCoplanarFrontList:PIndexList;
                                      const aBackList:PIndexList;
                                      const aFrontList:PIndexList);
            end;
            PPlane=^TPlane;
            PDynamicAABBTreeNode=^TDynamicAABBTreeNode;
            TDynamicAABBTreeNode=record
             AABB:TAABB;
             UserData:TpvPtrInt;
             Children:array[0..1] of TpvSizeInt;
             Height:TpvSizeInt;
             case boolean of
              false:(
               Parent:TpvSizeInt;
              );
              true:(
               Next:TpvSizeInt;
              );
            end;
            PDynamicAABBTreeNodes=^TDynamicAABBTreeNodes;
            TDynamicAABBTreeNodes=array[0..0] of TDynamicAABBTreeNode;
            PDynamicAABBTreeSizeIntArray=^TDynamicAABBTreeSizeIntArray;
            TDynamicAABBTreeSizeIntArray=array[0..65535] of TpvSizeInt;
            TDynamicAABBFrozenTreeNode=record
             Left:TpvSizeInt;
             Right:TpvSizeInt;
             AABB:TAABB;
             UserData:pointer;
            end;
            TDynamicAABBTree=class
             public
              Root:TpvSizeInt;
              Nodes:PDynamicAABBTreeNodes;
              NodeCount:TpvSizeInt;
              NodeCapacity:TpvSizeInt;
              FreeList:TpvSizeInt;
              Path:TpvSizeUInt;
              InsertionCount:TpvSizeInt;
              Stack:PDynamicAABBTreeSizeIntArray;
              StackCapacity:TpvSizeInt;
              constructor Create;
              destructor Destroy; override;
              function AllocateNode:TpvSizeInt;
              procedure FreeNode(const aNodeID:TpvSizeInt);
              function Balance(const aNodeID:TpvSizeInt):TpvSizeInt;
              procedure InsertLeaf(const aLeaf:TpvSizeInt);
              procedure RemoveLeaf(const aLeaf:TpvSizeInt);
              function CreateProxy(const aAABB:TAABB;const aUserData:TpvPtrInt):TpvSizeInt;
              procedure DestroyProxy(const aNodeID:TpvSizeInt);
              function MoveProxy(const aNodeID:TpvSizeInt;const aAABB:TAABB;const aDisplacement:TVector3):boolean;
              procedure Rebalance(const aIterations:TpvSizeInt);
            end;
            TSplitSettings=record
             SearchBestFactor:TpvSizeInt;
             PolygonSplitCost:TFloat;
             PolygonImbalanceCost:TFloat;
            end;
            PSplitSettings=^TSplitSettings;
            TNode=class;
            TMesh=class
             public
              type TCSGOperation=
                    (
                     Union,
                     Subtraction,
                     Intersection
                    );
                   PCSGOperation=^TCSGOperation;
                   TMode=
                    (
                     Triangles,
                     Polygons
                    );
                   PMode=^TMode;
             private
              fMode:TMode;
              fVertices:TVertexList;
              fIndices:TIndexList;
              fPointerToVertices:PVertexList;
              fPointerToIndices:PIndexList;
              procedure SetMode(const aMode:TMode);
              procedure SetVertices(const aVertices:TVertexList);
              procedure SetIndices(const aIndices:TIndexList);
             public
              constructor Create(const aMode:TMode=TMode.Triangles); reintroduce; overload;
              constructor Create(const aFrom:TMesh); reintroduce; overload;
              constructor Create(const aFrom:TNode); reintroduce; overload;
              constructor CreateCube(const aCX,aCY,aCZ,aRX,aRY,aRZ:TFloat;const aMode:TMode=TMode.Triangles);
              constructor CreateSphere(const aCX,aCY,aCZ,aRadius:TFloat;const aSlices:TpvSizeInt=16;const aStacks:TpvSizeInt=8;const aMode:TMode=TMode.Triangles);
              constructor CreateFromCSGOperation(const aLeftMesh:TMesh;
                                                 const aRightMesh:TMesh;
                                                 const aCSGOperation:TCSGOperation;
                                                 const aSplitSettings:PSplitSettings=nil);
              constructor CreateUnion(const aLeftMesh:TMesh;
                                      const aRightMesh:TMesh;
                                      const aSplitSettings:PSplitSettings=nil);
              constructor CreateSubtraction(const aLeftMesh:TMesh;
                                            const aRightMesh:TMesh;
                                            const aSplitSettings:PSplitSettings=nil);
              constructor CreateIntersection(const aLeftMesh:TMesh;
                                             const aRightMesh:TMesh;
                                             const aSplitSettings:PSplitSettings=nil);
              constructor CreateSymmetricDifference(const aLeftMesh:TMesh;
                                                    const aRightMesh:TMesh;
                                                    const aSplitSettings:PSplitSettings=nil);
              destructor Destroy; override;
              procedure Assign(const aFrom:TMesh);
              procedure Invert;
              procedure ConvertToPolygons;
              procedure ConvertToTriangles;
              procedure Canonicalize;
              procedure CalculateNormals(const aSoftNormals:boolean=true);
              procedure RemoveDuplicateAndUnusedVertices;
              procedure FixTJunctions;
              function ToNode(const aSplitSettings:PSplitSettings=nil):TNode;
             public
              property Vertices:TVertexList read fVertices write SetVertices;
              property Indices:TIndexList read fIndices write SetIndices;
              property PointerToVertices:PVertexList read fPointerToVertices;
              property PointerToIndices:PIndexList read fPointerToIndices;
             published
              property Mode:TMode read fMode write SetMode;
            end;
            TNode=class
             private
              fIndices:TIndexList;
              fPointerToIndices:PIndexList;
              fMesh:TMesh;
              fBack:TNode;
              fFront:TNode;
              fPlane:TPlane;
              procedure SetIndices(const aIndices:TIndexList);
             public
              constructor Create(const aMesh:TMesh); reintroduce;
              destructor Destroy; override;
              procedure Invert;
              procedure EvaluateSplitPlane(const aPlane:TPlane;
                                           out aCountPolygonsSplits:TpvSizeInt;
                                           out aCountBackPolygons:TpvSizeInt;
                                           out aCountFrontPolygons:TpvSizeInt);
              function FindSplitPlane(const aIndices:TIndexList;
                                      const aSplitSettings:PSplitSettings):TPlane;
              function ClipPolygons(var aVertices:TVertexList;const aIndices:TIndexList):TIndexList;
              procedure ClipTo(const aNode:TNode);
              procedure Merge(const aNode:TNode;
                              const aSplitSettings:PSplitSettings=nil);
              procedure Build(const aIndices:TIndexList;
                              const aSplitSettings:PSplitSettings=nil);
              function ToMesh:TMesh;
             public
              property Plane:TPlane read fPlane write fPlane;
              property Indices:TIndexList read fIndices write SetIndices;
              property PointerToIndices:PIndexList read fPointerToIndices;
             published
              property Mesh:TMesh read fMesh write fMesh;
              property Back:TNode read fBack write fBack;
              property Front:TNode read fFront write fFront;
            end;
      public
       const DefaultSplitSettings:TSplitSettings=
              (
               SearchBestFactor:0;
               PolygonSplitCost:1.0;
               PolygonImbalanceCost:0.25;
              );
             ThresholdAABBVector:TVector3=(x:AABBEPSILON;y:AABBEPSILON;z:AABBEPSILON);
      public
       class function EpsilonSign(const aValue:TFloat):TpvSizeInt; static;
     end;


implementation

{ TpvCSGBSP }

class function TpvCSGBSP.EpsilonSign(const aValue:TFloat):TpvSizeInt;
begin
 if aValue<-Epsilon then begin
  result:=-1;
 end else if aValue>Epsilon then begin
  result:=1;
 end else begin
  result:=0;
 end;
end;

{ TpvCSGBSP.TDynamicArray<T> }

procedure TpvCSGBSP.TDynamicArray<T>.Initialize;
begin
 Items:=nil;
 Count:=0;
end;

procedure TpvCSGBSP.TDynamicArray<T>.Finalize;
begin
 Items:=nil;
 Count:=0;
end;

procedure TpvCSGBSP.TDynamicArray<T>.Clear;
begin
 Items:=nil;
 Count:=0;
end;

procedure TpvCSGBSP.TDynamicArray<T>.Finish;
begin
 SetLength(Items,Count);
end;

procedure TpvCSGBSP.TDynamicArray<T>.Assign(const aFrom:TpvCSGBSP.TDynamicArray<T>);
begin
 Items:=aFrom.Items;
 Count:=aFrom.Count;
end;

procedure TpvCSGBSP.TDynamicArray<T>.Assign(const aItems:array of T);
var Index:TpvSizeInt;
begin
 Count:=length(aItems);
 SetLength(Items,Count);
 for Index:=0 to Count-1 do begin
  Items[Index]:=aItems[Index];
 end;
end;

function TpvCSGBSP.TDynamicArray<T>.AddNew:TpvSizeInt;
begin
 result:=Count;
 if length(Items)<(Count+1) then begin
  SetLength(Items,(Count+1)+((Count+1) shr 1));
 end;
 System.Initialize(Items[Count]);
 inc(Count);
end;

function TpvCSGBSP.TDynamicArray<T>.Add(const aItem:T):TpvSizeInt;
begin
 result:=Count;
 if length(Items)<(Count+1) then begin
  SetLength(Items,(Count+1)+((Count+1) shr 1));
 end;
 Items[Count]:=aItem;
 inc(Count);
end;

function TpvCSGBSP.TDynamicArray<T>.Add(const aItems:array of T):TpvSizeInt;
var Index,FromCount:TpvSizeInt;
begin
 result:=Count;
 FromCount:=length(aItems);
 if FromCount>0 then begin
  if length(Items)<(Count+FromCount) then begin
   SetLength(Items,(Count+FromCount)+((Count+FromCount) shr 1));
  end;
  for Index:=0 to FromCount-1 do begin
   Items[Count]:=aItems[Index];
   inc(Count);
  end;
 end;
end;

function TpvCSGBSP.TDynamicArray<T>.Add(const aFrom:TpvCSGBSP.TDynamicArray<T>):TpvSizeInt;
var Index:TpvSizeInt;
begin
 result:=Count;
 if aFrom.Count>0 then begin
  if length(Items)<(Count+aFrom.Count) then begin
   SetLength(Items,(Count+aFrom.Count)+((Count+aFrom.Count) shr 1));
  end;
  for Index:=0 to aFrom.Count-1 do begin
   Items[Count]:=aFrom.Items[Index];
   inc(Count);
  end;
 end;
end;

function TpvCSGBSP.TDynamicArray<T>.AddRangeFrom(const aFrom:{$ifdef fpc}TpvCSGBSP.{$endif}TDynamicArray<T>;const aStartIndex,aCount:TpvSizeInt):TpvSizeInt;
var Index:TpvSizeInt;
begin
 result:=Count;
 if aCount>0 then begin
  if length(Items)<(Count+aCount) then begin
   SetLength(Items,(Count+aCount)+((Count+aCount) shr 1));
  end;
  for Index:=0 to aCount-1 do begin
   Items[Count]:=aFrom.Items[aStartIndex+Index];
   inc(Count);
  end;
 end;
end;

procedure TpvCSGBSP.TDynamicArray<T>.Exchange(const aIndexA,aIndexB:TpvSizeInt);
var Temp:T;
begin
 Temp:=Items[aIndexA];
 Items[aIndexA]:=Items[aIndexB];
 Items[aIndexB]:=Temp;
end;

{ TpvCSGBSP.TDynamicStack<T> }

procedure TpvCSGBSP.TDynamicStack<T>.Initialize;
begin
 Items:=nil;
 Count:=0;
end;

procedure TpvCSGBSP.TDynamicStack<T>.Finalize;
begin
 Items:=nil;
 Count:=0;
end;

procedure TpvCSGBSP.TDynamicStack<T>.Push(const aItem:T);
begin
 if length(Items)<(Count+1) then begin
  SetLength(Items,(Count+1)+((Count+1) shr 1));
 end;
 Items[Count]:=aItem;
 inc(Count);
end;

function TpvCSGBSP.TDynamicStack<T>.Pop(out aItem:T):boolean;
begin
 result:=Count>0;
 if result then begin
  dec(Count);
  aItem:=Items[Count];
 end;
end;

{ TpvCSGBSP.TVector2 }

constructor TpvCSGBSP.TVector2.Create(const aFrom:TpvVector2);
begin
 x:=aFrom.x;
 y:=aFrom.y;
end;

class operator TpvCSGBSP.TVector2.Equal(const aLeft,aRight:TVector2):boolean;
begin
 result:=SameValue(aLeft.x,aRight.x) and
         SameValue(aLeft.y,aRight.y);
end;

class operator TpvCSGBSP.TVector2.NotEqual(const aLeft,aRight:TVector2):boolean;
begin
 result:=not (SameValue(aLeft.x,aRight.x) and
              SameValue(aLeft.y,aRight.y));
end;

class operator TpvCSGBSP.TVector2.Add(const aLeft,aRight:TVector2):TVector2;
begin
 result.x:=aLeft.x+aRight.x;
 result.y:=aLeft.y+aRight.y;
end;

class operator TpvCSGBSP.TVector2.Subtract(const aLeft,aRight:TVector2):TVector2;
begin
 result.x:=aLeft.x-aRight.x;
 result.y:=aLeft.y-aRight.y;
end;

class operator TpvCSGBSP.TVector2.Multiply(const aLeft:TVector2;const aRight:TFloat):TVector2;
begin
 result.x:=aLeft.x*aRight;
 result.y:=aLeft.y*aRight;
end;

class operator TpvCSGBSP.TVector2.Divide(const aLeft:TVector2;const aRight:TFloat):TVector2;
begin
 result.x:=aLeft.x/aRight;
 result.y:=aLeft.y/aRight;
end;

class operator TpvCSGBSP.TVector2.Negative(const aVector:TVector2):TVector2;
begin
 result.x:=-aVector.x;
 result.y:=-aVector.y;
end;

function TpvCSGBSP.TVector2.Lerp(const aWith:TVector2;const aTime:TFloat):TVector2;
var InverseTime:TFloat;
begin
 if aTime<=0.0 then begin
  result:=self;
 end else if aTime>=1.0 then begin
  result:=aWith;
 end else begin
  InverseTime:=1.0-aTime;
  result.x:=(x*InverseTime)+(aWith.x*aTime);
  result.y:=(y*InverseTime)+(aWith.y*aTime);
 end;
end;

function TpvCSGBSP.TVector2.ToVector:TpvVector2;
begin
 result.x:=x;
 result.y:=y;
end;

{ TpvCSGBSP.TVector3 }

constructor TpvCSGBSP.TVector3.Create(const aFrom:TpvVector3);
begin
 x:=aFrom.x;
 y:=aFrom.y;
 z:=aFrom.z;
end;

class operator TpvCSGBSP.TVector3.Equal(const aLeft,aRight:TVector3):boolean;
begin
 result:=SameValue(aLeft.x,aRight.x) and
         SameValue(aLeft.y,aRight.y) and
         SameValue(aLeft.z,aRight.z);
end;

class operator TpvCSGBSP.TVector3.NotEqual(const aLeft,aRight:TVector3):boolean;
begin
 result:=not (SameValue(aLeft.x,aRight.x) and
              SameValue(aLeft.y,aRight.y) and
              SameValue(aLeft.z,aRight.z));
end;

class operator TpvCSGBSP.TVector3.Add(const aLeft,aRight:TVector3):TVector3;
begin
 result.x:=aLeft.x+aRight.x;
 result.y:=aLeft.y+aRight.y;
 result.z:=aLeft.z+aRight.z;
end;

class operator TpvCSGBSP.TVector3.Subtract(const aLeft,aRight:TVector3):TVector3;
begin
 result.x:=aLeft.x-aRight.x;
 result.y:=aLeft.y-aRight.y;
 result.z:=aLeft.z-aRight.z;
end;

class operator TpvCSGBSP.TVector3.Multiply(const aLeft:TVector3;const aRight:TFloat):TVector3;
begin
 result.x:=aLeft.x*aRight;
 result.y:=aLeft.y*aRight;
 result.z:=aLeft.z*aRight;
end;

class operator TpvCSGBSP.TVector3.Divide(const aLeft:TVector3;const aRight:TFloat):TVector3;
begin
 result.x:=aLeft.x/aRight;
 result.y:=aLeft.y/aRight;
 result.z:=aLeft.z/aRight;
end;

class operator TpvCSGBSP.TVector3.Negative(const aVector:TVector3):TVector3;
begin
 result.x:=-aVector.x;
 result.y:=-aVector.y;
 result.z:=-aVector.z;
end;

function TpvCSGBSP.TVector3.Cross(const aWith:TVector3):TVector3;
begin
 result.x:=(y*aWith.z)-(z*aWith.y);
 result.y:=(z*aWith.x)-(x*aWith.z);
 result.z:=(x*aWith.y)-(y*aWith.x);
end;

function TpvCSGBSP.TVector3.Spacing(const aWith:TVector3):TFloat;
begin
 result:=abs(x-aWith.x)+abs(y-aWith.y)+abs(z-aWith.z);
end;

function TpvCSGBSP.TVector3.Dot(const aWith:TVector3):TFloat;
begin
 result:=(x*aWith.x)+(y*aWith.y)+(z*aWith.z);
end;

function TpvCSGBSP.TVector3.Length:TFloat;
begin
 result:=sqrt(sqr(x)+sqr(y)+sqr(z));
end;

function TpvCSGBSP.TVector3.SquaredLength:TFloat;
begin
 result:=sqr(x)+sqr(y)+sqr(z);
end;

function TpvCSGBSP.TVector3.Lerp(const aWith:TVector3;const aTime:TFloat):TVector3;
begin
 if aTime<=0.0 then begin
  result:=self;
 end else if aTime>=1.0 then begin
  result:=aWith;
 end else begin
  result:=(self*(1.0-aTime))+(aWith*aTime);
 end;
end;

function TpvCSGBSP.TVector3.Normalize:TVector3;
begin
 result:=self/Length;
end;

function TpvCSGBSP.TVector3.ToVector:TpvVector3;
begin
 result.x:=x;
 result.y:=y;
 result.z:=z;
end;

{ TpvCSGBSP.TVector4 }

constructor TpvCSGBSP.TVector4.Create(const aFrom:TpvVector4);
begin
 x:=aFrom.x;
 y:=aFrom.y;
 z:=aFrom.z;
 w:=aFrom.w;
end;

class operator TpvCSGBSP.TVector4.Equal(const aLeft,aRight:TVector4):boolean;
begin
 result:=SameValue(aLeft.x,aRight.x) and
         SameValue(aLeft.y,aRight.y) and
         SameValue(aLeft.z,aRight.z) and
         SameValue(aLeft.w,aRight.w);
end;

class operator TpvCSGBSP.TVector4.NotEqual(const aLeft,aRight:TVector4):boolean;
begin
 result:=not (SameValue(aLeft.x,aRight.x) and
              SameValue(aLeft.y,aRight.y) and
              SameValue(aLeft.z,aRight.z) and
              SameValue(aLeft.w,aRight.w));
end;

class operator TpvCSGBSP.TVector4.Add(const aLeft,aRight:TVector4):TVector4;
begin
 result.x:=aLeft.x+aRight.x;
 result.y:=aLeft.y+aRight.y;
 result.z:=aLeft.z+aRight.z;
 result.w:=aLeft.w+aRight.w;
end;

class operator TpvCSGBSP.TVector4.Subtract(const aLeft,aRight:TVector4):TVector4;
begin
 result.x:=aLeft.x-aRight.x;
 result.y:=aLeft.y-aRight.y;
 result.z:=aLeft.z-aRight.z;
 result.w:=aLeft.w-aRight.w;
end;

class operator TpvCSGBSP.TVector4.Multiply(const aLeft:TVector4;const aRight:TFloat):TVector4;
begin
 result.x:=aLeft.x*aRight;
 result.y:=aLeft.y*aRight;
 result.z:=aLeft.z*aRight;
 result.w:=aLeft.w*aRight;
end;

class operator TpvCSGBSP.TVector4.Divide(const aLeft:TVector4;const aRight:TFloat):TVector4;
begin
 result.x:=aLeft.x/aRight;
 result.y:=aLeft.y/aRight;
 result.z:=aLeft.z/aRight;
 result.w:=aLeft.w/aRight;
end;

class operator TpvCSGBSP.TVector4.Negative(const aVector:TVector4):TVector4;
begin
 result.x:=-aVector.x;
 result.y:=-aVector.y;
 result.z:=-aVector.z;
 result.w:=-aVector.w;
end;

function TpvCSGBSP.TVector4.Lerp(const aWith:TVector4;const aTime:TFloat):TVector4;
var InverseTime:TFloat;
begin
 if aTime<=0.0 then begin
  result:=self;
 end else if aTime>=1.0 then begin
  result:=aWith;
 end else begin
  InverseTime:=1.0-aTime;
  result.x:=(x*InverseTime)+(aWith.x*aTime);
  result.y:=(y*InverseTime)+(aWith.y*aTime);
  result.z:=(z*InverseTime)+(aWith.z*aTime);
  result.w:=(w*InverseTime)+(aWith.w*aTime);
 end;
end;

function TpvCSGBSP.TVector4.ToVector:TpvVector4;
begin
 result.x:=x;
 result.y:=y;
 result.z:=z;
 result.w:=w;
end;

{ TpvCSGBSP.TVertex }

class operator TpvCSGBSP.TVertex.Equal(const aLeft,aRight:TVertex):boolean;
begin
 result:=(aLeft.Position=aRight.Position) and
         (aLeft.Normal=aRight.Normal) and
         (aLeft.TexCoord=aRight.TexCoord) and
         (aLeft.Color=aRight.Color);
end;

class operator TpvCSGBSP.TVertex.NotEqual(const aLeft,aRight:TVertex):boolean;
begin
 result:=(aLeft.Position<>aRight.Position) or
         (aLeft.Normal<>aRight.Normal) or
         (aLeft.TexCoord<>aRight.TexCoord) or
         (aLeft.Color<>aRight.Color);
end;

class operator TpvCSGBSP.TVertex.Add(const aLeft,aRight:TVertex):TVertex;
begin
 result.Position:=aLeft.Position+aRight.Position;
 result.Normal:=aLeft.Normal+aRight.Normal;
 result.TexCoord:=aLeft.TexCoord+aRight.TexCoord;
 result.Color:=aLeft.Color+aRight.Color;
end;

class operator TpvCSGBSP.TVertex.Subtract(const aLeft,aRight:TVertex):TVertex;
begin
 result.Position:=aLeft.Position-aRight.Position;
 result.Normal:=aLeft.Normal-aRight.Normal;
 result.TexCoord:=aLeft.TexCoord-aRight.TexCoord;
 result.Color:=aLeft.Color-aRight.Color;
end;

class operator TpvCSGBSP.TVertex.Multiply(const aLeft:TVertex;const aRight:TFloat):TVertex;
begin
 result.Position:=aLeft.Position*aRight;
 result.Normal:=aLeft.Normal*aRight;
 result.TexCoord:=aLeft.TexCoord*aRight;
 result.Color:=aLeft.Color*aRight;
end;

class operator TpvCSGBSP.TVertex.Divide(const aLeft:TVertex;const aRight:TFloat):TVertex;
begin
 result.Position:=aLeft.Position/aRight;
 result.Normal:=aLeft.Normal/aRight;
 result.TexCoord:=aLeft.TexCoord/aRight;
 result.Color:=aLeft.Color/aRight;
end;

class operator TpvCSGBSP.TVertex.Negative(const aLeft:TVertex):TVertex;
begin
 result.Position:=-aLeft.Position;
 result.Normal:=-aLeft.Normal;
 result.TexCoord:=-aLeft.TexCoord;
 result.Color:=-aLeft.Color;
end;

function TpvCSGBSP.TVertex.Lerp(const aWith:TVertex;const aTime:TFloat):TVertex;
begin
 result.Position:=Position.Lerp(aWith.Position,aTime);
 result.Normal:=Normal.Lerp(aWith.Normal,aTime);
 result.TexCoord:=TexCoord.Lerp(aWith.TexCoord,aTime);
 result.Color:=Color.Lerp(aWith.Color,aTime);
end;

procedure TpvCSGBSP.TVertex.Flip;
begin
 Normal:=-Normal;
end;

function TpvCSGBSP.TVertex.CloneFlip:TVertex;
begin
 result.Position:=Position;
 result.Normal:=-Normal;
 result.TexCoord:=TexCoord;
 result.Color:=Color;
end;

function TpvCSGBSP.TVertex.Normalize:TVertex;
begin
 result.Position:=Position;
 result.Normal:=Normal.Normalize;
 result.TexCoord:=TexCoord;
 result.Color:=Color;
end;

{ TpvCSGBSP.TPlane }

constructor TpvCSGBSP.TPlane.Create(const aV0,aV1,aV2:TVector3);
begin
 Normal:=((aV1-aV0).Cross(aV2-aV0)).Normalize;
 Distance:=-Normal.Dot(aV0);
end;

constructor TpvCSGBSP.TPlane.Create(const aNormal:TVector3;const aDistance:TFloat);
begin
 Normal:=aNormal;
 Distance:=aDistance;
end;

class function TpvCSGBSP.TPlane.CreateEmpty;
begin
 result.Normal.x:=0.0;
 result.Normal.y:=0.0;
 result.Normal.z:=0.0;
 result.Distance:=0.0;
end;

function TpvCSGBSP.TPlane.DistanceTo(const aWith:TVector3):TFloat;
begin
 result:=Normal.Dot(aWith)+Distance;
end;

function TpvCSGBSP.TPlane.IntersectWithPointToPoint(const aPointA,aPointB:TVector3;const aIntersectionPoint:PVector3=nil):boolean;
var NormalDotDirection,Time:TFloat;
    Direction:TVector3;
begin
 result:=false;
 Direction:=aPointB-aPointA;
 NormalDotDirection:=Normal.Dot(Direction);
 if not IsZero(NormalDotDirection) then begin
  Time:=-(DistanceTo(aPointA)/NormalDotDirection);
  result:=(Time>=Epsilon) and (Time<=OneMinusEpsilon);
  if result and assigned(aIntersectionPoint) then begin
   aIntersectionPoint^:=aPointA.Lerp(aPointB,Time);
   if (aIntersectionPoint^=aPointA) or
      (aIntersectionPoint^=aPointB) then begin
    result:=false;
   end;
  end;
 end;
end;

function TpvCSGBSP.TPlane.IntersectWithPointToPoint(const aPointA,aPointB:TVertex;const aIntersectionPoint:PVertex=nil):boolean;
var NormalDotDirection,Time:TFloat;
    Direction:TVector3;
begin
 result:=false;
 Direction:=aPointB.Position-aPointA.Position;
 NormalDotDirection:=Normal.Dot(Direction);
 if not IsZero(NormalDotDirection) then begin
  Time:=-(DistanceTo(aPointA.Position)/NormalDotDirection);
  result:=(Time>=Epsilon) and (Time<=OneMinusEpsilon);
  if result and assigned(aIntersectionPoint) then begin
   aIntersectionPoint^:=aPointA.Lerp(aPointB,Time);
   if (aIntersectionPoint^.Position=aPointA.Position) or
      (aIntersectionPoint^.Position=aPointB.Position) then begin
    result:=false;
   end;
  end;
 end;
end;

function TpvCSGBSP.TPlane.OK:boolean;
begin
 result:=Normal.Length>0.0;
end;

function TpvCSGBSP.TPlane.Flip:TPlane;
begin
 result.Normal:=-Normal;
 result.Distance:=-Distance;
end;

procedure TpvCSGBSP.TPlane.SplitTriangles(var aVertices:TVertexList;
                                          const aIndices:TIndexList;
                                          const aCoplanarBackList:PIndexList;
                                          const aCoplanarFrontList:PIndexList;
                                          const aBackList:PIndexList;
                                          const aFrontList:PIndexList);
var Index,Count,DummyIndex:TpvSizeInt;
    Sides:array[0..2] of TpvSizeInt;
    VertexIndices,PointIndices:array[0..2] of TIndex;
    PlaneDistances:array[0..2] of TFloat;
begin

 Index:=0;
 Count:=aIndices.Count;

 while (Index+2)<Count do begin

  VertexIndices[0]:=aIndices.Items[Index+0];
  VertexIndices[1]:=aIndices.Items[Index+1];
  VertexIndices[2]:=aIndices.Items[Index+2];

  PlaneDistances[0]:=DistanceTo(aVertices.Items[VertexIndices[0]].Position);
  PlaneDistances[1]:=DistanceTo(aVertices.Items[VertexIndices[1]].Position);
  PlaneDistances[2]:=DistanceTo(aVertices.Items[VertexIndices[2]].Position);

  Sides[0]:=TpvCSGBSP.EpsilonSign(PlaneDistances[0]);
  Sides[1]:=TpvCSGBSP.EpsilonSign(PlaneDistances[1]);
  Sides[2]:=TpvCSGBSP.EpsilonSign(PlaneDistances[2]);

  PlaneDistances[0]:=PlaneDistances[0]*abs(Sides[0]);
  PlaneDistances[1]:=PlaneDistances[1]*abs(Sides[1]);
  PlaneDistances[2]:=PlaneDistances[2]*abs(Sides[2]);

  if (Sides[0]*Sides[1])<0 then begin
   PointIndices[0]:=aVertices.Add(aVertices.Items[VertexIndices[0]].Lerp(aVertices.Items[VertexIndices[1]],abs(PlaneDistances[0])/(abs(PlaneDistances[0])+abs(PlaneDistances[1]))));
  end;
  if (Sides[1]*Sides[2])<0 then begin
   PointIndices[1]:=aVertices.Add(aVertices.Items[VertexIndices[1]].Lerp(aVertices.Items[VertexIndices[2]],abs(PlaneDistances[1])/(abs(PlaneDistances[1])+abs(PlaneDistances[2]))));
  end;
  if (Sides[2]*Sides[0])<0 then begin
   PointIndices[2]:=aVertices.Add(aVertices.Items[VertexIndices[2]].Lerp(aVertices.Items[VertexIndices[0]],abs(PlaneDistances[2])/(abs(PlaneDistances[2])+abs(PlaneDistances[0]))));
  end;

  case ((Sides[0]+1) shl 0) or
       ((Sides[1]+1) shl 2) or
       ((Sides[2]+1) shl 4) of

   // All points are on one side of the plane (or on the plane)
   // in this case we simply add the complete triangle to the proper halve of the subtree
   (((-1)+1) shl 0) or (((-1)+1) shl 2) or (((-1)+1) shl 4),
   (((-1)+1) shl 0) or (((-1)+1) shl 2) or (((0)+1) shl 4),
   (((-1)+1) shl 0) or (((0)+1) shl 2) or (((-1)+1) shl 4),
   (((-1)+1) shl 0) or (((0)+1) shl 2) or (((0)+1) shl 4),
   (((0)+1) shl 0) or (((-1)+1) shl 2) or (((-1)+1) shl 4),
   (((0)+1) shl 0) or (((-1)+1) shl 2) or (((0)+1) shl 4),
   (((0)+1) shl 0) or (((0)+1) shl 2) or (((-1)+1) shl 4):begin
    if assigned(aBackList) then begin
     aBackList^.Add([VertexIndices[0],VertexIndices[1],VertexIndices[2]]);
    end;
   end;

   (((0)+1) shl 0) or (((0)+1) shl 2) or (((1)+1) shl 4),
   (((0)+1) shl 0) or (((1)+1) shl 2) or (((0)+1) shl 4),
   (((0)+1) shl 0) or (((1)+1) shl 2) or (((1)+1) shl 4),
   (((1)+1) shl 0) or (((0)+1) shl 2) or (((0)+1) shl 4),
   (((1)+1) shl 0) or (((0)+1) shl 2) or (((1)+1) shl 4),
   (((1)+1) shl 0) or (((1)+1) shl 2) or (((0)+1) shl 4),
   (((1)+1) shl 0) or (((1)+1) shl 2) or (((1)+1) shl 4):begin
    if assigned(aFrontList) then begin
     aFrontList^.Add([VertexIndices[0],VertexIndices[1],VertexIndices[2]]);
    end;
   end;

   // Triangle on the dividing plane
   (((0)+1) shl 0) or (((0)+1) shl 2) or (((0)+1) shl 4):begin
    if aCoplanarFrontList<>aCoplanarBackList then begin
     if Normal.Dot(TPlane.Create(aVertices.Items[VertexIndices[0]].Position,
                                 aVertices.Items[VertexIndices[1]].Position,
                                 aVertices.Items[VertexIndices[2]].Position).Normal)>0.0 then begin
      if assigned(aCoplanarFrontList) then begin
       aCoplanarFrontList^.Add([VertexIndices[0],VertexIndices[1],VertexIndices[2]]);
      end;
     end else begin
      if assigned(aCoplanarBackList) then begin
       aCoplanarBackList^.Add([VertexIndices[0],VertexIndices[1],VertexIndices[2]]);
      end;
     end;
    end else if assigned(aCoplanarFrontList) then begin
     aCoplanarFrontList^.Add([VertexIndices[0],VertexIndices[1],VertexIndices[2]]);
    end;
   end;

   // And now all the ways that the triangle can be cut by the plane

   (((1)+1) shl 0) or (((-1)+1) shl 2) or (((0)+1) shl 4):begin
    if assigned(aBackList) then begin
     aBackList^.Add([VertexIndices[1],VertexIndices[2],PointIndices[0]]);
    end;
    if assigned(aFrontList) then begin
     aFrontList^.Add([VertexIndices[2],VertexIndices[0],PointIndices[0]]);
    end;
   end;

   (((-1)+1) shl 0) or (((0)+1) shl 2) or (((1)+1) shl 4):begin
    if assigned(aBackList) then begin
     aBackList^.Add([VertexIndices[0],VertexIndices[1],PointIndices[2]]);
    end;
    if assigned(aFrontList) then begin
     aFrontList^.Add([VertexIndices[1],VertexIndices[2],PointIndices[2]]);
    end;
   end;

   (((0)+1) shl 0) or (((1)+1) shl 2) or (((-1)+1) shl 4):begin
    if assigned(aBackList) then begin
     aBackList^.Add([VertexIndices[2],VertexIndices[0],PointIndices[1]]);
    end;
    if assigned(aFrontList) then begin
     aFrontList^.Add([VertexIndices[0],VertexIndices[1],PointIndices[1]]);
    end;
   end;

   (((-1)+1) shl 0) or (((1)+1) shl 2) or (((0)+1) shl 4):begin
    if assigned(aBackList) then begin
     aBackList^.Add([VertexIndices[2],VertexIndices[0],PointIndices[0]]);
    end;
    if assigned(aFrontList) then begin
     aFrontList^.Add([VertexIndices[1],VertexIndices[2],PointIndices[0]]);
    end;
   end;

   (((1)+1) shl 0) or (((0)+1) shl 2) or (((-1)+1) shl 4):begin
    if assigned(aBackList) then begin
     aBackList^.Add([VertexIndices[1],VertexIndices[2],PointIndices[2]]);
    end;
    if assigned(aFrontList) then begin
     aFrontList^.Add([VertexIndices[0],VertexIndices[1],PointIndices[2]]);
    end;
   end;

   (((0)+1) shl 0) or (((-1)+1) shl 2) or (((1)+1) shl 4):begin
    if assigned(aBackList) then begin
     aBackList^.Add([VertexIndices[0],VertexIndices[1],PointIndices[1]]);
    end;
    if assigned(aFrontList) then begin
     aFrontList^.Add([VertexIndices[2],VertexIndices[0],PointIndices[1]]);
    end;
   end;

   (((1)+1) shl 0) or (((-1)+1) shl 2) or (((-1)+1) shl 4):begin
    if assigned(aFrontList) then begin
     aFrontList^.Add([VertexIndices[0],PointIndices[0],PointIndices[2]]);
    end;
    if assigned(aBackList) then begin
     aBackList^.Add([VertexIndices[1],PointIndices[2],PointIndices[0]]);
     aBackList^.Add([VertexIndices[1],VertexIndices[2],PointIndices[2]]);
    end;
   end;

   (((-1)+1) shl 0) or (((1)+1) shl 2) or (((-1)+1) shl 4):begin
    if assigned(aFrontList) then begin
     aFrontList^.Add([VertexIndices[1],PointIndices[1],PointIndices[0]]);
    end;
    if assigned(aBackList) then begin
     aBackList^.Add([VertexIndices[2],PointIndices[0],PointIndices[1]]);
     aBackList^.Add([VertexIndices[2],VertexIndices[0],PointIndices[0]]);
    end;
   end;

   (((-1)+1) shl 0) or (((-1)+1) shl 2) or (((1)+1) shl 4):begin
    if assigned(aFrontList) then begin
     aFrontList^.Add([VertexIndices[2],PointIndices[2],PointIndices[1]]);
    end;
    if assigned(aBackList) then begin
     aBackList^.Add([VertexIndices[0],PointIndices[1],PointIndices[2]]);
     aBackList^.Add([VertexIndices[0],VertexIndices[1],PointIndices[1]]);
    end;
   end;

   (((-1)+1) shl 0) or (((1)+1) shl 2) or (((1)+1) shl 4):begin
    if assigned(aBackList) then begin
     aBackList^.Add([VertexIndices[0],PointIndices[0],PointIndices[2]]);
    end;
    if assigned(aFrontList) then begin
     aFrontList^.Add([VertexIndices[1],PointIndices[2],PointIndices[0]]);
     aFrontList^.Add([VertexIndices[1],VertexIndices[2],PointIndices[2]]);
    end;
   end;

   (((1)+1) shl 0) or (((-1)+1) shl 2) or (((1)+1) shl 4):begin
    if assigned(aBackList) then begin
     aBackList^.Add([VertexIndices[1],PointIndices[1],PointIndices[0]]);
    end;
    if assigned(aFrontList) then begin
     aFrontList^.Add([VertexIndices[0],PointIndices[0],PointIndices[1]]);
     aFrontList^.Add([VertexIndices[2],VertexIndices[0],PointIndices[1]]);
    end;
   end;

   (((1)+1) shl 0) or (((1)+1) shl 2) or (((-1)+1) shl 4):begin
    if assigned(aBackList) then begin
     aBackList^.Add([VertexIndices[2],PointIndices[2],PointIndices[1]]);
    end;
    if assigned(aFrontList) then begin
     aFrontList^.Add([VertexIndices[0],PointIndices[1],PointIndices[2]]);
     aFrontList^.Add([VertexIndices[0],VertexIndices[1],PointIndices[1]]);
    end;
   end;

   // Otherwise it is a error
   else begin
    Assert(false);
   end;

  end;

  inc(Index,3);

 end;

end;

procedure TpvCSGBSP.TPlane.SplitPolygons(var aVertices:TVertexList;
                                         const aIndices:TIndexList;
                                         const aCoplanarBackList:PIndexList;
                                         const aCoplanarFrontList:PIndexList;
                                         const aBackList:PIndexList;
                                         const aFrontList:PIndexList);
const Coplanar=0;
      Front=1;
      Back=2;
      Spanning=3;
      EpsilonSignToOrientation:array[0..3] of TpvInt32=(Back,Coplanar,Front,Spanning);
var Index,OtherIndex,Count,CountPolygonVertices,
    IndexA,IndexB,
    PolygonOrientation,
    VertexOrientation,
    VertexOrientationA,
    VertexOrientationB:TpvSizeInt;
    VertexOrientations:array of TpvSizeInt;
    VectorDistance:TpvDouble;
    BackVertexIndices,FrontVertexIndices:TIndexList;
    VertexIndex:TIndex;
    VertexA,VertexB:PVertex;
begin

 VertexOrientations:=nil;
 try

  BackVertexIndices.Initialize;
  try

   FrontVertexIndices.Initialize;
   try

    Index:=0;

    Count:=aIndices.Count;

    while Index<Count do begin

     CountPolygonVertices:=aIndices.Items[Index];
     inc(Index);

     if (CountPolygonVertices>0) and
        ((Index+(CountPolygonVertices-1))<Count) then begin

      if CountPolygonVertices>2 then begin

       PolygonOrientation:=0;

       if length(VertexOrientations)<CountPolygonVertices then begin
        SetLength(VertexOrientations,(CountPolygonVertices*3) shr 1);
       end;

       for IndexA:=0 to CountPolygonVertices-1 do begin
        VertexIndex:=aIndices.Items[Index+IndexA];
        VertexOrientation:=EpsilonSignToOrientation[(TpvCSGBSP.EpsilonSign(DistanceTo(aVertices.Items[VertexIndex].Position))+1) and 3];
        PolygonOrientation:=PolygonOrientation or VertexOrientation;
        VertexOrientations[IndexA]:=VertexOrientation;
       end;

       case PolygonOrientation of

        Coplanar:begin
         if assigned(aCoplanarFrontList) or assigned(aCoplanarBackList) then begin
          if Normal.Dot(TPlane.Create(aVertices.Items[aIndices.Items[Index+0]].Position,
                                      aVertices.Items[aIndices.Items[Index+1]].Position,
                                      aVertices.Items[aIndices.Items[Index+2]].Position).Normal)>0.0 then begin
           if assigned(aCoplanarFrontList) then begin
            aCoplanarFrontList^.Add(CountPolygonVertices);
            aCoplanarFrontList^.AddRangeFrom(aIndices,Index,CountPolygonVertices);
           end;
          end else begin
           if assigned(aCoplanarBackList) then begin
            aCoplanarBackList^.Add(CountPolygonVertices);
            aCoplanarBackList^.AddRangeFrom(aIndices,Index,CountPolygonVertices);
           end;
          end;
         end;
        end;

        Front:begin
         if assigned(aFrontList) then begin
          aFrontList^.Add(CountPolygonVertices);
          aFrontList^.AddRangeFrom(aIndices,Index,CountPolygonVertices);
         end;
        end;

        Back:begin
         if assigned(aBackList) then begin
          aBackList^.Add(CountPolygonVertices);
          aBackList^.AddRangeFrom(aIndices,Index,CountPolygonVertices);
         end;
        end;

        else {SPANNING:}begin

         BackVertexIndices.Count:=0;

         FrontVertexIndices.Count:=0;

         for IndexA:=0 to CountPolygonVertices-1 do begin

          IndexB:=IndexA+1;
          if IndexB>=CountPolygonVertices then begin
           IndexB:=0;
          end;

          VertexIndex:=aIndices.Items[Index+IndexA];

          VertexA:=@aVertices.Items[VertexIndex];

          VertexOrientationA:=VertexOrientations[IndexA];
          VertexOrientationB:=VertexOrientations[IndexB];

          if VertexOrientationA<>Front then begin
           BackVertexIndices.Add(VertexIndex);
          end;

          if VertexOrientationA<>Back then begin
           FrontVertexIndices.Add(VertexIndex);
          end;

          if (VertexOrientationA or VertexOrientationB)=Spanning then begin
           VertexB:=@aVertices.Items[aIndices.Items[Index+IndexB]];
           VertexIndex:=aVertices.Add(VertexA^.Lerp(VertexB^,-(DistanceTo(VertexA^.Position)/Normal.Dot(VertexB^.Position-VertexA^.Position))));
           BackVertexIndices.Add(VertexIndex);
           FrontVertexIndices.Add(VertexIndex);
          end;

         end;

         if assigned(aBackList) and (BackVertexIndices.Count>2) then begin
          aBackList^.Add(BackVertexIndices.Count);
          aBackList^.Add(BackVertexIndices);
         end;

         if assigned(aFrontList) and (FrontVertexIndices.Count>2) then begin
          aFrontList^.Add(FrontVertexIndices.Count);
          aFrontList^.Add(FrontVertexIndices);
         end;

        end;

       end;

      end;

     end else begin
      Assert(false);
     end;

     inc(Index,CountPolygonVertices);

    end;

   finally
    FrontVertexIndices.Finalize;
   end;

  finally
   BackVertexIndices.Finalize;
  end;

 finally
  VertexOrientations:=nil;
 end;

end;

{ TpvCSGBSP.TAABB }

function TpvCSGBSP.TAABB.Cost:TFloat;
begin
// result:=(self.Max.x-self.Min.x)+(self.Max.y-self.Min.y)+(self.Max.z-self.Min.z); // Manhattan distance
 result:=(self.Max.x-self.Min.x)*(self.Max.y-self.Min.y)*(self.Max.z-self.Min.z); // Volume
end;

function TpvCSGBSP.TAABB.Combine(const aAABB:TAABB):TAABB;
begin
 result.Min.x:=Math.Min(self.Min.x,aaABB.Min.x);
 result.Min.y:=Math.Min(self.Min.y,aAABB.Min.y);
 result.Min.z:=Math.Min(self.Min.z,aAABB.Min.z);
 result.Max.x:=Math.Max(self.Max.x,aAABB.Max.x);
 result.Max.y:=Math.Max(self.Max.y,aAABB.Max.y);
 result.Max.z:=Math.Max(self.Max.z,aAABB.Max.z);
end;

function TpvCSGBSP.TAABB.Contains(const aAABB:TAABB;const aThreshold:TFloat=0.0):boolean;
begin
 result:=((self.Min.x-aThreshold)<=(aAABB.Min.x+aThreshold)) and ((self.Min.y-aThreshold)<=(aAABB.Min.y+aThreshold)) and ((self.Min.z-aThreshold)<=(aAABB.Min.z+aThreshold)) and
         ((self.Max.x+aThreshold)>=(aAABB.Min.x+aThreshold)) and ((self.Max.y+aThreshold)>=(aAABB.Min.y+aThreshold)) and ((self.Max.z+aThreshold)>=(aAABB.Min.z+aThreshold)) and
         ((self.Min.x-aThreshold)<=(aAABB.Max.x-aThreshold)) and ((self.Min.y-aThreshold)<=(aAABB.Max.y-aThreshold)) and ((self.Min.z-aThreshold)<=(aAABB.Max.z-aThreshold)) and
         ((self.Max.x+aThreshold)>=(aAABB.Max.x-aThreshold)) and ((self.Max.y+aThreshold)>=(aAABB.Max.y-aThreshold)) and ((self.Max.z+aThreshold)>=(aAABB.Max.z-aThreshold));
end;

function TpvCSGBSP.TAABB.Intersects(const aAABB:TAABB;const aThreshold:TFloat=Epsilon):boolean;
begin
 result:=(((self.Max.x+aThreshold)>=(aAABB.Min.x-aThreshold)) and ((self.Min.x-aThreshold)<=(aAABB.Max.x+aThreshold))) and
         (((self.Max.y+aThreshold)>=(aAABB.Min.y-aThreshold)) and ((self.Min.y-aThreshold)<=(aAABB.Max.y+aThreshold))) and
         (((self.Max.z+aThreshold)>=(aAABB.Min.z-aThreshold)) and ((self.Min.z-aThreshold)<=(aAABB.Max.z+aThreshold)));
end;

{ TpvCSGBSP.TDynamicAABBTree }

constructor TpvCSGBSP.TDynamicAABBTree.Create;
var i:TpvSizeInt;
begin
 inherited Create;
 Root:=daabbtNULLNODE;
 NodeCount:=0;
 NodeCapacity:=16;
 GetMem(Nodes,NodeCapacity*SizeOf(TDynamicAABBTreeNode));
 FillChar(Nodes^,NodeCapacity*SizeOf(TDynamicAABBTreeNode),#0);
 for i:=0 to NodeCapacity-2 do begin
  Nodes^[i].Next:=i+1;
  Nodes^[i].Height:=-1;
 end;
 Nodes^[NodeCapacity-1].Next:=daabbtNULLNODE;
 Nodes^[NodeCapacity-1].Height:=-1;
 FreeList:=0;
 Path:=0;
 InsertionCount:=0;
 StackCapacity:=16;
 GetMem(Stack,StackCapacity*SizeOf(TpvSizeInt));
end;

destructor TpvCSGBSP.TDynamicAABBTree.Destroy;
begin
 FreeMem(Nodes);
 FreeMem(Stack);
 inherited Destroy;
end;

function TpvCSGBSP.TDynamicAABBTree.AllocateNode:TpvSizeInt;
var Node:PDynamicAABBTreeNode;
    i:TpvSizeInt;
begin
 if FreeList=daabbtNULLNODE then begin
  inc(NodeCapacity,NodeCapacity);
  ReallocMem(Nodes,NodeCapacity*SizeOf(TDynamicAABBTreeNode));
  FillChar(Nodes^[NodeCount],(NodeCapacity-NodeCount)*SizeOf(TDynamicAABBTreeNode),#0);
  for i:=NodeCount to NodeCapacity-2 do begin
   Nodes^[i].Next:=i+1;
   Nodes^[i].Height:=-1;
  end;
  Nodes^[NodeCapacity-1].Next:=daabbtNULLNODE;
  Nodes^[NodeCapacity-1].Height:=-1;
  FreeList:=NodeCount;
 end;
 result:=FreeList;
 FreeList:=Nodes^[result].Next;
 Node:=@Nodes^[result];
 Node^.Parent:=daabbtNULLNODE;
 Node^.Children[0]:=daabbtNULLNODE;
 Node^.Children[1]:=daabbtNULLNODE;
 Node^.Height:=0;
 Node^.UserData:=0;
 inc(NodeCount);
end;

procedure TpvCSGBSP.TDynamicAABBTree.FreeNode(const aNodeID:TpvSizeInt);
var Node:PDynamicAABBTreeNode;
begin
 Node:=@Nodes^[aNodeID];
 Node^.Next:=FreeList;
 Node^.Height:=-1;
 FreeList:=aNodeID;
 dec(NodeCount);
end;

function TpvCSGBSP.TDynamicAABBTree.Balance(const aNodeID:TpvSizeInt):TpvSizeInt;
var NodeA,NodeB,NodeC,NodeD,NodeE,NodeF,NodeG:PDynamicAABBTreeNode;
    NodeBID,NodeCID,NodeDID,NodeEID,NodeFID,NodeGID,NodeBalance:TpvSizeInt;
begin
 NodeA:=@Nodes^[aNodeID];
 if (NodeA.Children[0]<0) or (NodeA^.Height<2) then begin
  result:=aNodeID;
 end else begin
  NodeBID:=NodeA^.Children[0];
  NodeCID:=NodeA^.Children[1];
  NodeB:=@Nodes^[NodeBID];
  NodeC:=@Nodes^[NodeCID];
  NodeBalance:=NodeC^.Height-NodeB^.Height;
  if NodeBalance>1 then begin
   NodeFID:=NodeC^.Children[0];
   NodeGID:=NodeC^.Children[1];
   NodeF:=@Nodes^[NodeFID];
   NodeG:=@Nodes^[NodeGID];
   NodeC^.Children[0]:=aNodeID;
   NodeC^.Parent:=NodeA^.Parent;
   NodeA^.Parent:=NodeCID;
   if NodeC^.Parent>=0 then begin
    if Nodes^[NodeC^.Parent].Children[0]=aNodeID then begin
     Nodes^[NodeC^.Parent].Children[0]:=NodeCID;
    end else begin
     Nodes^[NodeC^.Parent].Children[1]:=NodeCID;
    end;
   end else begin
    Root:=NodeCID;
   end;
   if NodeF^.Height>NodeG^.Height then begin
    NodeC^.Children[1]:=NodeFID;
    NodeA^.Children[1]:=NodeGID;
    NodeG^.Parent:=aNodeID;
    NodeA^.AABB:=NodeB^.AABB.Combine(NodeG^.AABB);
    NodeC^.AABB:=NodeA^.AABB.Combine(NodeF^.AABB);
    NodeA^.Height:=1+Max(NodeB^.Height,NodeG^.Height);
    NodeC^.Height:=1+Max(NodeA^.Height,NodeF^.Height);
   end else begin
    NodeC^.Children[1]:=NodeGID;
    NodeA^.Children[1]:=NodeFID;
    NodeF^.Parent:=aNodeID;
    NodeA^.AABB:=NodeB^.AABB.Combine(NodeF^.AABB);
    NodeC^.AABB:=NodeA^.AABB.Combine(NodeG^.AABB);
    NodeA^.Height:=1+Max(NodeB^.Height,NodeF^.Height);
    NodeC^.Height:=1+Max(NodeA^.Height,NodeG^.Height);
   end;
   result:=NodeCID;
  end else if NodeBalance<-1 then begin
   NodeDID:=NodeB^.Children[0];
   NodeEID:=NodeB^.Children[1];
   NodeD:=@Nodes^[NodeDID];
   NodeE:=@Nodes^[NodeEID];
   NodeB^.Children[0]:=aNodeID;
   NodeB^.Parent:=NodeA^.Parent;
   NodeA^.Parent:=NodeBID;
   if NodeB^.Parent>=0 then begin
    if Nodes^[NodeB^.Parent].Children[0]=aNodeID then begin
     Nodes^[NodeB^.Parent].Children[0]:=NodeBID;
    end else begin
     Nodes^[NodeB^.Parent].Children[1]:=NodeBID;
    end;
   end else begin
    Root:=NodeBID;
   end;
   if NodeD^.Height>NodeE^.Height then begin
    NodeB^.Children[1]:=NodeDID;
    NodeA^.Children[0]:=NodeEID;
    NodeE^.Parent:=aNodeID;
    NodeA^.AABB:=NodeC^.AABB.Combine(NodeE^.AABB);
    NodeB^.AABB:=NodeA^.AABB.Combine(NodeD^.AABB);
    NodeA^.Height:=1+Max(NodeC^.Height,NodeE^.Height);
    NodeB^.Height:=1+Max(NodeA^.Height,NodeD^.Height);
   end else begin
    NodeB^.Children[1]:=NodeEID;
    NodeA^.Children[0]:=NodeDID;
    NodeD^.Parent:=aNodeID;
    NodeA^.AABB:=NodeC^.AABB.Combine(NodeD^.AABB);
    NodeB^.AABB:=NodeA^.AABB.Combine(NodeE^.AABB);
    NodeA^.Height:=1+Max(NodeC^.Height,NodeD^.Height);
    NodeB^.Height:=1+Max(NodeA^.Height,NodeE^.Height);
   end;
   result:=NodeBID;
  end else begin
   result:=aNodeID;
  end;
 end;
end;

procedure TpvCSGBSP.TDynamicAABBTree.InsertLeaf(const aLeaf:TpvSizeInt);
var Node:PDynamicAABBTreeNode;
    LeafAABB,CombinedAABB,AABB:TAABB;
    Index,Sibling,OldParent,NewParent:TpvSizeInt;
    Children:array[0..1] of TpvSizeInt;
    CombinedCost,Cost,InheritanceCost:TFloat;
    Costs:array[0..1] of TFloat;
begin
 inc(InsertionCount);
 if Root<0 then begin
  Root:=aLeaf;
  Nodes^[aLeaf].Parent:=daabbtNULLNODE;
 end else begin
  LeafAABB:=Nodes^[aLeaf].AABB;
  Index:=Root;
  while Nodes^[Index].Children[0]>=0 do begin
   Children[0]:=Nodes^[Index].Children[0];
   Children[1]:=Nodes^[Index].Children[1];

   CombinedAABB:=Nodes^[Index].AABB.Combine(LeafAABB);
   CombinedCost:=CombinedAABB.Cost;
   Cost:=CombinedCost*2.0;
   InheritanceCost:=2.0*(CombinedCost-Nodes^[Index].AABB.Cost);

   AABB:=LeafAABB.Combine(Nodes^[Children[0]].AABB);
   if Nodes^[Children[0]].Children[0]<0 then begin
    Costs[0]:=AABB.Cost+InheritanceCost;
   end else begin
    Costs[0]:=(AABB.Cost-Nodes^[Children[0]].AABB.Cost)+InheritanceCost;
   end;

   AABB:=LeafAABB.Combine(Nodes^[Children[1]].AABB);
   if Nodes^[Children[1]].Children[1]<0 then begin
    Costs[1]:=AABB.Cost+InheritanceCost;
   end else begin
    Costs[1]:=(AABB.Cost-Nodes^[Children[1]].AABB.Cost)+InheritanceCost;
   end;

   if (Cost<Costs[0]) and (Cost<Costs[1]) then begin
    break;
   end else begin
    if Costs[0]<Costs[1] then begin
     Index:=Children[0];
    end else begin
     Index:=Children[1];
    end;
   end;

  end;

  Sibling:=Index;

  OldParent:=Nodes^[Sibling].Parent;
  NewParent:=AllocateNode;
  Nodes^[NewParent].Parent:=OldParent;
  Nodes^[NewParent].UserData:=0;
  Nodes^[NewParent].AABB:=LeafAABB.Combine(Nodes^[Sibling].AABB);
  Nodes^[NewParent].Height:=Nodes^[Sibling].Height+1;

  if OldParent>=0 then begin
   if Nodes^[OldParent].Children[0]=Sibling then begin
    Nodes^[OldParent].Children[0]:=NewParent;
   end else begin
    Nodes^[OldParent].Children[1]:=NewParent;
   end;
   Nodes^[NewParent].Children[0]:=Sibling;
   Nodes^[NewParent].Children[1]:=aLeaf;
   Nodes^[Sibling].Parent:=NewParent;
   Nodes^[aLeaf].Parent:=NewParent;
  end else begin
   Nodes^[NewParent].Children[0]:=Sibling;
   Nodes^[NewParent].Children[1]:=aLeaf;
   Nodes^[Sibling].Parent:=NewParent;
   Nodes^[aLeaf].Parent:=NewParent;
   Root:=NewParent;
  end;

  Index:=Nodes^[aLeaf].Parent;
  while Index>=0 do begin
   Index:=Balance(Index);
   Node:=@Nodes^[Index];
   Node^.AABB:=Nodes^[Node^.Children[0]].AABB.Combine(Nodes^[Node^.Children[1]].AABB);
   Node^.Height:=1+Max(Nodes^[Node^.Children[0]].Height,Nodes^[Node^.Children[1]].Height);
   Index:=Node^.Parent;
  end;

 end;
end;

procedure TpvCSGBSP.TDynamicAABBTree.RemoveLeaf(const aLeaf:TpvSizeInt);
var Node:PDynamicAABBTreeNode;
    Parent,GrandParent,Sibling,Index:TpvSizeInt;
begin
 if Root=aLeaf then begin
  Root:=daabbtNULLNODE;
 end else begin
  Parent:=Nodes^[aLeaf].Parent;
  GrandParent:=Nodes^[Parent].Parent;
  if Nodes^[Parent].Children[0]=aLeaf then begin
   Sibling:=Nodes^[Parent].Children[1];
  end else begin
   Sibling:=Nodes^[Parent].Children[0];
  end;
  if GrandParent>=0 then begin
   if Nodes^[GrandParent].Children[0]=Parent then begin
    Nodes^[GrandParent].Children[0]:=Sibling;
   end else begin
    Nodes^[GrandParent].Children[1]:=Sibling;
   end;
   Nodes^[Sibling].Parent:=GrandParent;
   FreeNode(Parent);
   Index:=GrandParent;
   while Index>=0 do begin
    Index:=Balance(Index);
    Node:=@Nodes^[Index];
    Node^.AABB:=Nodes^[Node^.Children[0]].AABB.Combine(Nodes^[Node^.Children[1]].AABB);
    Node^.Height:=1+Max(Nodes^[Node^.Children[0]].Height,Nodes^[Node^.Children[1]].Height);
    Index:=Node^.Parent;
   end;
  end else begin
   Root:=Sibling;
   Nodes^[Sibling].Parent:=daabbtNULLNODE;
   FreeNode(Parent);
  end;
 end;
end;

function TpvCSGBSP.TDynamicAABBTree.CreateProxy(const aAABB:TAABB;const aUserData:TpvPtrInt):TpvSizeInt;
var Node:PDynamicAABBTreeNode;
begin
 result:=AllocateNode;
 Node:=@Nodes^[result];
 Node^.AABB.Min:=aAABB.Min-ThresholdAABBVector;
 Node^.AABB.Max:=aAABB.Max+ThresholdAABBVector;
 Node^.UserData:=aUserData;
 Node^.Height:=0;
 InsertLeaf(result);
end;

procedure TpvCSGBSP.TDynamicAABBTree.DestroyProxy(const aNodeID:TpvSizeInt);
begin
 RemoveLeaf(aNodeID);
 FreeNode(aNodeID);
end;

function TpvCSGBSP.TDynamicAABBTree.MoveProxy(const aNodeID:TpvSizeInt;const aAABB:TAABB;const aDisplacement:TVector3):boolean;
var Node:PDynamicAABBTreeNode;
    b:TAABB;
    d:TVector3;
begin
 Node:=@Nodes^[aNodeID];
 result:=not Node^.AABB.Contains(aAABB);
 if result then begin
  RemoveLeaf(aNodeID);
  b.Min:=aAABB.Min-ThresholdAABBVector;
  b.Max:=aAABB.Max+ThresholdAABBVector;
  d:=aDisplacement*AABBMULTIPLIER;
  if d.x<0.0 then begin
   b.Min.x:=b.Min.x+d.x;
  end else if d.x>0.0 then begin
   b.Max.x:=b.Max.x+d.x;
  end;
  if d.y<0.0 then begin
   b.Min.y:=b.Min.y+d.y;
  end else if d.y>0.0 then begin
   b.Max.y:=b.Max.y+d.y;
  end;
  if d.z<0.0 then begin
   b.Min.z:=b.Min.z+d.z;
  end else if d.z>0.0 then begin
   b.Max.z:=b.Max.z+d.z;
  end;
  Node^.AABB:=b;
  InsertLeaf(aNodeID);
 end;
end;

procedure TpvCSGBSP.TDynamicAABBTree.Rebalance(const aIterations:TpvSizeInt);
var Counter,Node:TpvSizeInt;
    Bit:TpvSizeUInt;
//  Children:PDynamicAABBTreeSizeIntArray;
begin
 if (Root>=0) and (Root<NodeCount) then begin
  for Counter:=1 to aIterations do begin
   Bit:=0;
   Node:=Root;
   while Nodes[Node].Children[0]>=0 do begin
    Node:=Nodes[Node].Children[(Path shr Bit) and 1];
    Bit:=(Bit+1) and 31;
   end;
   inc(Path);
   if ((Node>=0) and (Node<NodeCount)) and (Nodes[Node].Children[0]<0) then begin
    RemoveLeaf(Node);
    InsertLeaf(Node);
   end else begin
    break;
   end;
  end;
 end;
end;

{ TpvCSGBSP.TMesh }

constructor TpvCSGBSP.TMesh.Create(const aMode:TMode=TMode.Triangles);
begin
 inherited Create;
 fMode:=aMode;
 fVertices.Initialize;
 fIndices.Initialize;
 fPointerToVertices:=@fVertices;
 fPointerToIndices:=@fIndices;
end;

constructor TpvCSGBSP.TMesh.Create(const aFrom:TMesh);
begin
 Create(aFrom.fMode);
 if assigned(aFrom) then begin
  SetVertices(aFrom.fVertices);
  SetIndices(aFrom.fIndices);
 end;
end;

constructor TpvCSGBSP.TMesh.Create(const aFrom:TNode);
var Mesh:TMesh;
begin
 Mesh:=aFrom.ToMesh;
 try
  Create(Mesh.fMode);
  fVertices.Assign(Mesh.fVertices);
  fIndices.Assign(Mesh.fIndices);
  Mesh.fVertices.Clear;
  Mesh.fIndices.Clear;
 finally
  FreeAndNil(Mesh);
 end;
end;

constructor TpvCSGBSP.TMesh.CreateCube(const aCX,aCY,aCZ,aRX,aRY,aRZ:TFloat;const aMode:TMode=TMode.Triangles);
const SideVertexIndices:array[0..5,0..3] of TpvUInt8=
       (
        (0,4,6,2), // Left
        (1,3,7,5), // Right
        (0,1,5,4), // Bottom
        (2,6,7,3), // Top
        (0,2,3,1), // Back
        (4,5,7,6)  // Front
       );
      SideNormals:array[0..5] of TVector3=
       (
        (x:-1.0;y:0.0;z:0.0),
        (x:1.0;y:0.0;z:0.0),
        (x:0.0;y:-1.0;z:0.0),
        (x:0.0;y:1.0;z:0.0),
        (x:0.0;y:0.0;z:-1.0),
        (x:0.0;y:0.0;z:1.0)
       );
var SideIndex,SideVertexIndex,VertexIndex,BaseVertexIndex:TpvSizeInt;
    Vertex:TVertex;
begin
 Create(aMode);
 for SideIndex:=0 to 5 do begin
  BaseVertexIndex:=fVertices.Count;
  for SideVertexIndex:=0 to 3 do begin
   VertexIndex:=SideVertexIndices[SideIndex,SideVertexIndex];
   Vertex.Position.x:=aCX+(((((VertexIndex shr 0) and 1) shl 1)-1)*aRX);
   Vertex.Position.y:=aCY+(((((VertexIndex shr 1) and 1) shl 1)-1)*aRY);
   Vertex.Position.z:=aCZ+(((((VertexIndex shr 2) and 1) shl 1)-1)*aRZ);
   Vertex.Normal:=SideNormals[SideIndex];
   Vertex.TexCoord.x:=SideVertexIndex and 1;
   Vertex.TexCoord.y:=((SideVertexIndex shr 1) and 1) xor (SideVertexIndex and 1);
   Vertex.Color.x:=1.0;
   Vertex.Color.y:=1.0;
   Vertex.Color.z:=1.0;
   Vertex.Color.w:=1.0;
   fVertices.Add(Vertex);
  end;
  case fMode of
   TMode.Triangles:begin
    fIndices.Add([BaseVertexIndex+0,BaseVertexIndex+1,BaseVertexIndex+2,
                  BaseVertexIndex+0,BaseVertexIndex+2,BaseVertexIndex+3]);
   end;
   else {TMode.Polygons:}begin
    fIndices.Add([4,BaseVertexIndex+0,BaseVertexIndex+1,BaseVertexIndex+2,BaseVertexIndex+3]);
   end;
  end;
 end;
 RemoveDuplicateAndUnusedVertices;
end;

constructor TpvCSGBSP.TMesh.CreateSphere(const aCX,aCY,aCZ,aRadius:TFloat;const aSlices:TpvSizeInt=16;const aStacks:TpvSizeInt=8;const aMode:TMode=TMode.Triangles);
 function AddVertex(const aTheta,aPhi:TpvDouble):TIndex;
 var Theta,Phi,dx,dy,dz:TFloat;
     Vertex:TVertex;
 begin
  Theta:=aTheta*TwoPI;
  Phi:=aPhi*PI;
  dx:=cos(Theta)*sin(Phi);
  dy:=cos(Phi);
  dz:=sin(Theta)*sin(Phi);
  Vertex.Position.x:=aCX+(dx*aRadius);
  Vertex.Position.y:=aCY+(dy*aRadius);
  Vertex.Position.z:=aCZ+(dz*aRadius);
  Vertex.Normal.x:=dx;
  Vertex.Normal.y:=dy;
  Vertex.Normal.z:=dz;
  Vertex.TexCoord.x:=aTheta;
  Vertex.TexCoord.y:=aPhi;
  Vertex.Color.x:=1.0;
  Vertex.Color.y:=1.0;
  Vertex.Color.z:=1.0;
  Vertex.Color.w:=1.0;
  result:=fVertices.Add(Vertex);
 end;
var SliceIndex,StackIndex:TpvSizeInt;
    PolygonIndices:TIndexList;
begin
 Create(aMode);
 PolygonIndices.Initialize;
 try
  for SliceIndex:=0 to aSlices-1 do begin
   for StackIndex:=0 to aStacks-1 do begin
    PolygonIndices.Clear;
    PolygonIndices.Add(AddVertex(SliceIndex/aSlices,StackIndex/aStacks));
    if StackIndex>0 then begin
     PolygonIndices.Add(AddVertex((SliceIndex+1)/aSlices,StackIndex/aStacks));
    end;
    if StackIndex<(aStacks-1) then begin
     PolygonIndices.Add(AddVertex((SliceIndex+1)/aSlices,(StackIndex+1)/aStacks));
    end;
    PolygonIndices.Add(AddVertex(SliceIndex/aSlices,(StackIndex+1)/aStacks));
    case fMode of
     TMode.Triangles:begin
      case PolygonIndices.Count of
       3:begin
        fIndices.Add(PolygonIndices);
       end;
       4:begin
        fIndices.Add([PolygonIndices.Items[0],PolygonIndices.Items[1],PolygonIndices.Items[2],
                      PolygonIndices.Items[0],PolygonIndices.Items[2],PolygonIndices.Items[3]]);
       end;
       else begin
        Assert(false);
       end;
      end;
     end;
     else {TMode.Polygons:}begin
      fIndices.Add(PolygonIndices.Count);
      fIndices.Add(PolygonIndices);
     end;
    end;
   end;
  end;
 finally
  PolygonIndices.Finalize;
 end;
 RemoveDuplicateAndUnusedVertices;
end;

constructor TpvCSGBSP.TMesh.CreateFromCSGOperation(const aLeftMesh:TMesh;
                                                   const aRightMesh:TMesh;
                                                   const aCSGOperation:TCSGOperation;
                                                   const aSplitSettings:PSplitSettings=nil);
 function ProcessTriangle(const aNode:TNode;
                          const aVertex0:TVertex;
                          const aVertex1:TVertex;
                          const aVertex2:TVertex;
                          const aInside:boolean;
                          const aKeepEdge:boolean;
                          const aKeepNow:boolean;
                          const aInvert:boolean):boolean;
 type TWorkData=record
       Node:TNode;
       Vertex0:TVertex;
       Vertex1:TVertex;
       Vertex2:TVertex;
       Inside:boolean;
       KeepEdge:boolean;
       KeepNow:boolean;
       Invert:boolean;
       Completed:boolean;
       Clipped:boolean;
       PreviousWorkData:TpvSizeInt;
       OldCountVertices:TpvSizeInt;
       OldCountIndices:TpvSizeInt;
      end;
      PWorkData=^TWorkData;
      TJobStackItem=record
       WorkData:TpvSizeInt;
       Step:TpvSizeInt;
      end;
      TWorkDataArray=TDynamicArray<TWorkData>;
      TJobStack=TDynamicStack<TJobStackItem>;
 var WorkDataArray:TWorkDataArray;
     JobStack:TJobStack;
  function NewWorkData(const aNode:TNode;
                       const aVertex0:TVertex;
                       const aVertex1:TVertex;
                       const aVertex2:TVertex;
                       const aInside:boolean;
                       const aKeepEdge:boolean;
                       const aKeepNow:boolean;
                       const aInvert:boolean;
                       const aPreviousWorkData:TpvSizeInt):TpvSizeInt;
  var WorkData:PWorkData;
  begin
   result:=WorkDataArray.AddNew;
   WorkData:=@WorkDataArray.Items[result];
   WorkData^.Node:=aNode;
   WorkData^.Vertex0:=aVertex0;
   WorkData^.Vertex1:=aVertex1;
   WorkData^.Vertex2:=aVertex2;
   WorkData^.Inside:=aInside;
   WorkData^.KeepEdge:=aKeepEdge;
   WorkData^.KeepNow:=aKeepEdge;
   WorkData^.Invert:=aInvert;
   WorkData^.PreviousWorkData:=aPreviousWorkData;
   WorkData^.Completed:=true;
   WorkData^.Clipped:=true;
  end;
  procedure NewJobStackItem(const aWorkData:TpvSizeInt;
                            const aStep:TpvSizeInt);
  var JobStackItem:TJobStackItem;
  begin
   JobStackItem.WorkData:=aWorkData;
   JobStackItem.Step:=aStep;
   JobStack.Push(JobStackItem);
  end;
 var WorkDataIndex:TpvSizeInt;
     JobStackItem:TJobStackItem;
     WorkData,OtherWorkData:PWorkData;
     FunctionResult:boolean;
  procedure Append(const aVertex0,aVertex1,aVertex2:TVertex);
  begin
   fIndices.Add(fVertices.Add(aVertex0));
   fIndices.Add(fVertices.Add(aVertex1));
   fIndices.Add(fVertices.Add(aVertex2));
  end;
  function NextTriangle(const aNode:TNode;
                        const aVertex0:TVertex;
                        const aVertex1:TVertex;
                        const aVertex2:TVertex;
                        const aInside:boolean;
                        const aKeepEdge:boolean;
                        const aKeepNow:boolean;
                        const aInvert:boolean):TpvSizeInt;
  var Completed:boolean;
  begin
    if assigned(aNode) then begin
     NewJobStackItem(NewWorkData(aNode,
                                 aVertex0,
                                 aVertex1,
                                 aVertex2,
                                 aInside,
                                 aKeepEdge,
                                 aKeepNow,
                                 aInvert,
                                 WorkDataIndex),
                     0);
    WorkData:=@WorkDataArray.Items[JobStackItem.WorkData];
   end else begin
    if aKeepNow then begin
     if aInvert then begin
      Append(aVertex2.CloneFlip,aVertex1.CloneFlip,aVertex0.CloneFlip);
     end else begin
      Append(aVertex0,aVertex1,aVertex2);
     end;
    end;
    Completed:=aKeepNow;
    if assigned(WorkData) then begin
     WorkData^.Completed:=WorkData^.Completed and Completed;
    end else begin
     FunctionResult:=Completed;
    end;
   end;
  end;
 var Sides:array[0..2] of TpvSizeInt;
     Points:array[0..2] of TVertex;
     PlaneDistances:array[0..2] of TFloat;
 begin

  FunctionResult:=true;
  try

   WorkDataArray.Initialize;
   try

    JobStack.Initialize;
    try

     WorkDataIndex:=-1;

     WorkData:=nil;

     NextTriangle(aNode,
                  aVertex0,
                  aVertex1,
                  aVertex2,
                  aInside,
                  aKeepEdge,
                  aKeepNow,
                  aInvert);

     while JobStack.Pop(JobStackItem) do begin

      WorkDataIndex:=JobStackItem.WorkData;

      WorkData:=@WorkDataArray.Items[WorkDataIndex];

      case JobStackItem.Step of

       0:begin

        PlaneDistances[0]:=WorkData^.Node.fPlane.DistanceTo(WorkData^.Vertex0.Position);
        PlaneDistances[1]:=WorkData^.Node.fPlane.DistanceTo(WorkData^.Vertex1.Position);
        PlaneDistances[2]:=WorkData^.Node.fPlane.DistanceTo(WorkData^.Vertex2.Position);

        Sides[0]:=TpvCSGBSP.EpsilonSign(PlaneDistances[0]);
        Sides[1]:=TpvCSGBSP.EpsilonSign(PlaneDistances[1]);
        Sides[2]:=TpvCSGBSP.EpsilonSign(PlaneDistances[2]);

        PlaneDistances[0]:=PlaneDistances[0]*abs(Sides[0]);
        PlaneDistances[1]:=PlaneDistances[1]*abs(Sides[1]);
        PlaneDistances[2]:=PlaneDistances[2]*abs(Sides[2]);

        if (Sides[0]*Sides[1])<0 then begin
         Points[0]:=WorkData^.Vertex0.Lerp(WorkData^.Vertex1,abs(PlaneDistances[0])/(abs(PlaneDistances[0])+abs(PlaneDistances[1])));
        end;
        if (Sides[1]*Sides[2])<0 then begin
         Points[1]:=WorkData^.Vertex1.Lerp(WorkData^.Vertex2,abs(PlaneDistances[1])/(abs(PlaneDistances[1])+abs(PlaneDistances[2])));
        end;
        if (Sides[2]*Sides[0])<0 then begin
         Points[2]:=WorkData^.Vertex2.Lerp(WorkData^.Vertex0,abs(PlaneDistances[2])/(abs(PlaneDistances[2])+abs(PlaneDistances[0])));
        end;

        WorkData^.OldCountVertices:=fVertices.Count;
        WorkData^.OldCountIndices:=fIndices.Count;

        WorkData^.Completed:=true;

        WorkData^.Clipped:=true;

        NewJobStackItem(JobStackItem.WorkData,1);

        case ((Sides[0]+1) shl 0) or
             ((Sides[1]+1) shl 2) or
             ((Sides[2]+1) shl 4) of

         // All points are on one side of the plane (or on the plane)
         // in this case we simply add the complete triangle to the proper halve of the subtree
         (((-1)+1) shl 0) or (((-1)+1) shl 2) or (((-1)+1) shl 4),
         (((-1)+1) shl 0) or (((-1)+1) shl 2) or (((0)+1) shl 4),
         (((-1)+1) shl 0) or (((0)+1) shl 2) or (((-1)+1) shl 4),
         (((-1)+1) shl 0) or (((0)+1) shl 2) or (((0)+1) shl 4),
         (((0)+1) shl 0) or (((-1)+1) shl 2) or (((-1)+1) shl 4),
         (((0)+1) shl 0) or (((-1)+1) shl 2) or (((0)+1) shl 4),
         (((0)+1) shl 0) or (((0)+1) shl 2) or (((-1)+1) shl 4):begin
          NextTriangle(WorkData^.Node.fBack,WorkData^.Vertex0,WorkData^.Vertex1,WorkData^.Vertex2,WorkData^.Inside,WorkData^.KeepEdge,WorkData^.Inside,WorkData^.Invert);
          WorkData^.Clipped:=false;
         end;

         (((0)+1) shl 0) or (((0)+1) shl 2) or (((1)+1) shl 4),
         (((0)+1) shl 0) or (((1)+1) shl 2) or (((0)+1) shl 4),
         (((0)+1) shl 0) or (((1)+1) shl 2) or (((1)+1) shl 4),
         (((1)+1) shl 0) or (((0)+1) shl 2) or (((0)+1) shl 4),
         (((1)+1) shl 0) or (((0)+1) shl 2) or (((1)+1) shl 4),
         (((1)+1) shl 0) or (((1)+1) shl 2) or (((0)+1) shl 4),
         (((1)+1) shl 0) or (((1)+1) shl 2) or (((1)+1) shl 4):begin
          NextTriangle(WorkData^.Node.fFront,WorkData^.Vertex0,WorkData^.Vertex1,WorkData^.Vertex2,WorkData^.Inside,WorkData^.KeepEdge,not WorkData^.Inside,WorkData^.Invert);
          WorkData^.Clipped:=false;
         end;

         // Triangle on the dividing plane
         (((0)+1) shl 0) or (((0)+1) shl 2) or (((0)+1) shl 4):begin
          if WorkData^.KeepEdge then begin
           Append(WorkData^.Vertex0,WorkData^.Vertex1,WorkData^.Vertex2);
           WorkData^.Clipped:=false;
          end;
         end;

         // And now all the ways that the triangle can be cut by the plane

         (((1)+1) shl 0) or (((-1)+1) shl 2) or (((0)+1) shl 4):begin
          NextTriangle(WorkData^.Node.fBack,WorkData^.Vertex1,WorkData^.Vertex2,Points[0],WorkData^.Inside,WorkData^.KeepEdge,WorkData^.Inside,WorkData^.Invert);
          NextTriangle(WorkData^.Node.fFront,WorkData^.Vertex2,WorkData^.Vertex1,Points[0],WorkData^.Inside,WorkData^.KeepEdge,not WorkData^.Inside,WorkData^.Invert);
         end;

         (((-1)+1) shl 0) or (((0)+1) shl 2) or (((1)+1) shl 4):begin
          NextTriangle(WorkData^.Node.fBack,WorkData^.Vertex0,WorkData^.Vertex1,Points[2],WorkData^.Inside,WorkData^.KeepEdge,WorkData^.Inside,WorkData^.Invert);
          NextTriangle(WorkData^.Node.fFront,WorkData^.Vertex1,WorkData^.Vertex2,Points[2],WorkData^.Inside,WorkData^.KeepEdge,not WorkData^.Inside,WorkData^.Invert);
         end;

         (((0)+1) shl 0) or (((1)+1) shl 2) or (((-1)+1) shl 4):begin
          NextTriangle(WorkData^.Node.fBack,WorkData^.Vertex2,WorkData^.Vertex0,Points[1],WorkData^.Inside,WorkData^.KeepEdge,WorkData^.Inside,WorkData^.Invert);
          NextTriangle(WorkData^.Node.fFront,WorkData^.Vertex0,WorkData^.Vertex1,Points[1],WorkData^.Inside,WorkData^.KeepEdge,not WorkData^.Inside,WorkData^.Invert);
         end;

         (((-1)+1) shl 0) or (((1)+1) shl 2) or (((0)+1) shl 4):begin
          NextTriangle(WorkData^.Node.fBack,WorkData^.Vertex2,WorkData^.Vertex0,Points[0],WorkData^.Inside,WorkData^.KeepEdge,WorkData^.Inside,WorkData^.Invert);
          NextTriangle(WorkData^.Node.fFront,WorkData^.Vertex1,WorkData^.Vertex2,Points[0],WorkData^.Inside,WorkData^.KeepEdge,not WorkData^.Inside,WorkData^.Invert);
         end;

         (((1)+1) shl 0) or (((0)+1) shl 2) or (((-1)+1) shl 4):begin
          NextTriangle(WorkData^.Node.fBack,WorkData^.Vertex1,WorkData^.Vertex2,Points[2],WorkData^.Inside,WorkData^.KeepEdge,WorkData^.Inside,WorkData^.Invert);
          NextTriangle(WorkData^.Node.fFront,WorkData^.Vertex0,WorkData^.Vertex1,Points[2],WorkData^.Inside,WorkData^.KeepEdge,not WorkData^.Inside,WorkData^.Invert);
         end;

         (((0)+1) shl 0) or (((-1)+1) shl 2) or (((1)+1) shl 4):begin
          NextTriangle(WorkData^.Node.fBack,WorkData^.Vertex0,WorkData^.Vertex1,Points[1],WorkData^.Inside,WorkData^.KeepEdge,WorkData^.Inside,WorkData^.Invert);
          NextTriangle(WorkData^.Node.fFront,WorkData^.Vertex2,WorkData^.Vertex0,Points[1],WorkData^.Inside,WorkData^.KeepEdge,not WorkData^.Inside,WorkData^.Invert);
         end;

         (((1)+1) shl 0) or (((-1)+1) shl 2) or (((-1)+1) shl 4):begin
          NextTriangle(WorkData^.Node.fFront,WorkData^.Vertex0,Points[0],Points[2],WorkData^.Inside,WorkData^.KeepEdge,not WorkData^.Inside,WorkData^.Invert);
          NextTriangle(WorkData^.Node.fBack,WorkData^.Vertex1,Points[2],Points[0],WorkData^.Inside,WorkData^.KeepEdge,WorkData^.Inside,WorkData^.Invert);
          NextTriangle(WorkData^.Node.fBack,WorkData^.Vertex1,WorkData^.Vertex2,Points[2],WorkData^.Inside,WorkData^.KeepEdge,WorkData^.Inside,WorkData^.Invert);
         end;

         (((-1)+1) shl 0) or (((1)+1) shl 2) or (((-1)+1) shl 4):begin
          NextTriangle(WorkData^.Node.fFront,WorkData^.Vertex1,Points[1],Points[0],WorkData^.Inside,WorkData^.KeepEdge,not WorkData^.Inside,WorkData^.Invert);
          NextTriangle(WorkData^.Node.fBack,WorkData^.Vertex2,Points[0],Points[1],WorkData^.Inside,WorkData^.KeepEdge,WorkData^.Inside,WorkData^.Invert);
          NextTriangle(WorkData^.Node.fBack,WorkData^.Vertex2,WorkData^.Vertex0,Points[0],WorkData^.Inside,WorkData^.KeepEdge,WorkData^.Inside,WorkData^.Invert);
         end;

         (((-1)+1) shl 0) or (((-1)+1) shl 2) or (((1)+1) shl 4):begin
          NextTriangle(WorkData^.Node.fFront,WorkData^.Vertex2,Points[2],Points[1],WorkData^.Inside,WorkData^.KeepEdge,not WorkData^.Inside,WorkData^.Invert);
          NextTriangle(WorkData^.Node.fBack,WorkData^.Vertex0,Points[1],Points[2],WorkData^.Inside,WorkData^.KeepEdge,WorkData^.Inside,WorkData^.Invert);
          NextTriangle(WorkData^.Node.fBack,WorkData^.Vertex0,WorkData^.Vertex1,Points[1],WorkData^.Inside,WorkData^.KeepEdge,WorkData^.Inside,WorkData^.Invert);
         end;

         (((-1)+1) shl 0) or (((1)+1) shl 2) or (((1)+1) shl 4):begin
          NextTriangle(WorkData^.Node.fBack,WorkData^.Vertex0,Points[0],Points[2],WorkData^.Inside,WorkData^.KeepEdge,WorkData^.Inside,WorkData^.Invert);
          NextTriangle(WorkData^.Node.fFront,WorkData^.Vertex1,Points[2],Points[0],WorkData^.Inside,WorkData^.KeepEdge,not WorkData^.Inside,WorkData^.Invert);
          NextTriangle(WorkData^.Node.fFront,WorkData^.Vertex1,WorkData^.Vertex2,Points[2],WorkData^.Inside,WorkData^.KeepEdge,not WorkData^.Inside,WorkData^.Invert);
         end;

         (((1)+1) shl 0) or (((-1)+1) shl 2) or (((1)+1) shl 4):begin
          NextTriangle(WorkData^.Node.fBack,WorkData^.Vertex1,Points[1],Points[0],WorkData^.Inside,WorkData^.KeepEdge,WorkData^.Inside,WorkData^.Invert);
          NextTriangle(WorkData^.Node.fFront,WorkData^.Vertex0,Points[0],Points[1],WorkData^.Inside,WorkData^.KeepEdge,not WorkData^.Inside,WorkData^.Invert);
          NextTriangle(WorkData^.Node.fFront,WorkData^.Vertex2,WorkData^.Vertex0,Points[1],WorkData^.Inside,WorkData^.KeepEdge,not WorkData^.Inside,WorkData^.Invert);
         end;

         (((1)+1) shl 0) or (((1)+1) shl 2) or (((-1)+1) shl 4):begin
          NextTriangle(WorkData^.Node.fBack,WorkData^.Vertex2,Points[2],Points[1],WorkData^.Inside,WorkData^.KeepEdge,WorkData^.Inside,WorkData^.Invert);
          NextTriangle(WorkData^.Node.fFront,WorkData^.Vertex0,Points[1],Points[2],WorkData^.Inside,WorkData^.KeepEdge,not WorkData^.Inside,WorkData^.Invert);
          NextTriangle(WorkData^.Node.fFront,WorkData^.Vertex0,WorkData^.Vertex1,Points[1],WorkData^.Inside,WorkData^.KeepEdge,not WorkData^.Inside,WorkData^.Invert);
         end;

         // Otherwise it is a error
         else begin
          WorkData^.Completed:=false;
         end;

        end;

       end;

       1:begin

        if WorkData^.Completed and WorkData^.Clipped then begin
         fVertices.Count:=WorkData^.OldCountVertices;
         fIndices.Count:=WorkData^.OldCountIndices;
         if WorkData^.Invert then begin
          Append(WorkData^.Vertex2.CloneFlip,WorkData^.Vertex1.CloneFlip,WorkData^.Vertex0.CloneFlip);
         end else begin
          Append(WorkData^.Vertex0,WorkData^.Vertex1,WorkData^.Vertex2);
         end;
        end;

        if WorkData^.PreviousWorkData>=0 then begin
         OtherWorkData:=@WorkDataArray.Items[WorkData^.PreviousWorkData];
         OtherWorkData^.Completed:=OtherWorkData^.Completed and WorkData^.Completed;
        end else begin
         FunctionResult:=WorkData^.Completed;
        end;
        dec(WorkDataArray.Count);

       end;

      end;

     end;

    finally
     JobStack.Finalize;
    end;

   finally
    WorkDataArray.Finalize;
   end;

  finally
   result:=FunctionResult;
  end;

 end;
 procedure Process(const aLeftNode:TNode;
                   const aRightNode:TNode;
                   const aVertices:TVertexList;
                   const aInside:boolean;
                   const aKeepEdge:boolean;
                   const aInvert:boolean);
 type TJobStack=TDynamicStack<TNode>;
 var JobStack:TJobStack;
     Node:TNode;
     Index,Count:TpvSizeInt;
 begin
  if assigned(aLeftNode) and assigned(aRightNode) then begin
   JobStack.Initialize;
   try
    JobStack.Push(aRightNode);
    while JobStack.Pop(Node) do begin
     if assigned(Node) then begin
      Index:=0;
      Count:=Node.fIndices.Count;
      while (Index+2)<Count do begin
       ProcessTriangle(aLeftNode,
                        aVertices.Items[Node.fIndices.Items[Index+0]],
                        aVertices.Items[Node.fIndices.Items[Index+1]],
                        aVertices.Items[Node.fIndices.Items[Index+2]],
                        aInside,
                        aKeepEdge,
                        false,
                        aInvert);
       inc(Index,3);
      end;
      if assigned(Node.fFront) then begin
       JobStack.Push(Node.fFront);
      end;
      if assigned(Node.fBack) then begin
       JobStack.Push(Node.fBack);
      end;
     end;
    end;
   finally
    JobStack.Finalize;
   end;
  end;
 end;
 procedure AesignNodeToMesh(const aNode:TNode);
 var Mesh:TMesh;
 begin
  Mesh:=aNode.ToMesh;
  try
   fMode:=Mesh.fMode;
   fVertices.Assign(Mesh.fVertices);
   fIndices.Assign(Mesh.fIndices);
   Mesh.fVertices.Clear;
   Mesh.fIndices.Clear;
  finally
   FreeAndNil(Mesh);
  end;
 end;
 procedure ProcessUnion(const aLeftMesh,aRightMesh:TMesh);
 var ma,mb:TMesh;
     na,nb:TNode;
 begin
  ma:=TMesh.Create(aLeftMesh);
  try
   ma.SetMode(TMode.Polygons);
   na:=ma.ToNode(aSplitSettings);
   try
    mb:=TMesh.Create(aRightMesh);
    try
     mb.SetMode(TMode.Polygons);
     nb:=mb.ToNode(aSplitSettings);
     try
      na.ClipTo(nb);
      nb.ClipTo(na);
      nb.Invert;
      nb.ClipTo(na);
      nb.Invert;
      na.Merge(nb,aSplitSettings);
     finally
      FreeAndNil(nb);
     end;
    finally
     FreeAndNil(mb);
    end;
    AesignNodeToMesh(na);
   finally
    FreeAndNil(na);
   end;
  finally
   FreeAndNil(ma);
  end;
 end;
 procedure ProcessSubtraction(const aLeftMesh,aRightMesh:TMesh);
 var ma,mb:TMesh;
     na,nb:TNode;
 begin
  ma:=TMesh.Create(aLeftMesh);
  try
   ma.SetMode(TMode.Polygons);
   na:=ma.ToNode(aSplitSettings);
   try
    mb:=TMesh.Create(aRightMesh);
    try
     mb.SetMode(TMode.Polygons);
     nb:=mb.ToNode(aSplitSettings);
     try
      na.Invert;
      na.ClipTo(nb);
      nb.ClipTo(na);
      nb.Invert;
      nb.ClipTo(na);
      nb.Invert;
      na.Merge(nb,aSplitSettings);
     finally
      FreeAndNil(nb);
     end;
    finally
     FreeAndNil(mb);
    end;
    na.Invert;
    AesignNodeToMesh(na);
   finally
    FreeAndNil(na);
   end;
  finally
   FreeAndNil(ma);
  end;
 end;
 procedure ProcessIntersection(const aLeftMesh,aRightMesh:TMesh);
 var ma,mb:TMesh;
     na,nb:TNode;
 begin
  ma:=TMesh.Create(aLeftMesh);
  try
   ma.SetMode(TMode.Polygons);
   na:=ma.ToNode(aSplitSettings);
   try
    mb:=TMesh.Create(aRightMesh);
    try
     mb.SetMode(TMode.Polygons);
     nb:=mb.ToNode(aSplitSettings);
     try
      na.Invert;
      nb.ClipTo(na);
      nb.Invert;
      na.ClipTo(nb);
      nb.ClipTo(na);
      na.Merge(nb,aSplitSettings);
     finally
      FreeAndNil(nb);
     end;
    finally
     FreeAndNil(mb);
    end;
    na.Invert;
    AesignNodeToMesh(na);
   finally
    FreeAndNil(na);
   end;
  finally
   FreeAndNil(ma);
  end;
 end;
 procedure ProcessTriangles;
 var LeftNode,RightNode:TNode;
 begin
  LeftNode:=aLeftMesh.ToNode;
  try
   RightNode:=aRightMesh.ToNode;
   try
    case aCSGOperation of
     TCSGOperation.Union:begin
      Process(RightNode,LeftNode,aLeftMesh.fVertices,false,false,false);
      Process(LeftNode,RightNode,aRightMesh.fVertices,false,true,false);
     end;
     TCSGOperation.Subtraction:begin
      Process(RightNode,LeftNode,aLeftMesh.fVertices,false,false,false);
      Process(LeftNode,RightNode,aRightMesh.fVertices,true,true,true);
     end;
     TCSGOperation.Intersection:begin
      Process(RightNode,LeftNode,aLeftMesh.fVertices,true,false,false);
      Process(LeftNode,RightNode,aRightMesh.fVertices,true,true,false);
     end;
     else begin
      Assert(false);
     end;
    end;
   finally
    FreeAndNil(RightNode);
   end;
  finally
   FreeAndNil(LeftNode);
  end;
 end;
 procedure ProcessPolygons;
 begin
  case aCSGOperation of
   TCSGOperation.Union:begin
    ProcessUnion(aLeftMesh,aRightMesh);
   end;
   TCSGOperation.Subtraction:begin
    ProcessSubtraction(aLeftMesh,aRightMesh);
   end;
   TCSGOperation.Intersection:begin
    ProcessIntersection(aLeftMesh,aRightMesh);
   end;
   else begin
    Assert(false);
   end;
  end;
 end;
begin
 Create;
 if (aLeftMesh.fMode=TMode.Triangles) and
    (aRightMesh.fMode=TMode.Triangles) then begin
  ProcessTriangles;
 end else begin
  ProcessPolygons;
 end;
 RemoveDuplicateAndUnusedVertices;
end;

constructor TpvCSGBSP.TMesh.CreateUnion(const aLeftMesh:TMesh;
                                        const aRightMesh:TMesh;
                                        const aSplitSettings:PSplitSettings=nil);
begin
 CreateFromCSGOperation(aLeftMesh,aRightMesh,TCSGOperation.Union,aSplitSettings);
end;

constructor TpvCSGBSP.TMesh.CreateSubtraction(const aLeftMesh:TMesh;
                                              const aRightMesh:TMesh;
                                              const aSplitSettings:PSplitSettings=nil);
begin
 CreateFromCSGOperation(aLeftMesh,aRightMesh,TCSGOperation.Subtraction,aSplitSettings);
end;

constructor TpvCSGBSP.TMesh.CreateIntersection(const aLeftMesh:TMesh;
                                               const aRightMesh:TMesh;
                                               const aSplitSettings:PSplitSettings=nil);
begin
 CreateFromCSGOperation(aLeftMesh,aRightMesh,TCSGOperation.Intersection,aSplitSettings);
end;

constructor TpvCSGBSP.TMesh.CreateSymmetricDifference(const aLeftMesh:TMesh;
                                                      const aRightMesh:TMesh;
                                                      const aSplitSettings:PSplitSettings=nil);
var a,b:TMesh;
begin
 // Possible symmertic difference (boolean XOR) implementations:
 // Intersection(Union(A,B),Inverse(Intersection(A,B)))
 // Intersection(Union(A,B),Union(Inverse(A),Inverse(B)))
 // Union(Subtraction(A,B),Subtraction(B,A)) <= used here, because it seems the most robust mnethod in this BSP-based triangle-based CSG implementation!
 // Subtraction(Union(A,B),Intersection(A,B))
 a:=TMesh.CreateSubtraction(aLeftMesh,aRightMesh,aSplitSettings);
 try
  b:=TMesh.CreateSubtraction(aRightMesh,aLeftMesh,aSplitSettings);
  try
   CreateUnion(a,b,aSplitSettings);
  finally
   FreeAndNil(b);
  end;
 finally
  FreeAndNil(a);
 end;
end;

destructor TpvCSGBSP.TMesh.Destroy;
begin
 fVertices.Finalize;
 fIndices.Finalize;
 fPointerToVertices:=nil;
 fPointerToIndices:=nil;
 inherited Destroy;
end;

procedure TpvCSGBSP.TMesh.SetMode(const aMode:TMode);
var Index,Count,CountPolygonVertices,PolygonVertexIndex:TpvSizeInt;
    NewIndices:TIndexList;
begin
 if fMode<>aMode then begin
  NewIndices.Initialize;
  try
   if (fMode=TMode.Triangles) and (aMode=TMode.Polygons) then begin
    Index:=0;
    Count:=fIndices.Count;
    while (Index+2)<Count do begin
     NewIndices.Add([3,fIndices.Items[Index+0],fIndices.Items[Index+1],fIndices.Items[Index+2]]);
     inc(Index,3);
    end;
    //Assert(Index=Count);
   end else if (fMode=TMode.Polygons) and (aMode=TMode.Triangles) then begin
    Index:=0;
    Count:=fIndices.Count;
    while Index<Count do begin
     CountPolygonVertices:=fIndices.Items[Index];
     inc(Index);
     if (CountPolygonVertices>0) and
        ((Index+(CountPolygonVertices-1))<Count) then begin
      if CountPolygonVertices>2 then begin
       for PolygonVertexIndex:=2 to CountPolygonVertices-1 do begin
        NewIndices.Add([fIndices.Items[Index+0],
                        fIndices.Items[Index+(PolygonVertexIndex-1)],
                        fIndices.Items[Index+PolygonVertexIndex]]);
       end;
      end;
      inc(Index,CountPolygonVertices);
     end else begin
      Assert(false);
     end;
    end;
    Assert(Index=Count);
   end else begin
    Assert(false);
   end;
   SetIndices(NewIndices);
  finally
   NewIndices.Finalize;
  end;
  fMode:=aMode;
 end;
end;

procedure TpvCSGBSP.TMesh.SetVertices(const aVertices:TVertexList);
begin
 fVertices.Assign(aVertices);
end;

procedure TpvCSGBSP.TMesh.SetIndices(const aIndices:TIndexList);
begin
 fIndices.Assign(aIndices);
end;

procedure TpvCSGBSP.TMesh.Assign(const aFrom:TMesh);
begin
 fMode:=aFrom.fMode;
 SetVertices(aFrom.fVertices);
 SetIndices(aFrom.fIndices);
end;

procedure TpvCSGBSP.TMesh.Invert;
var Index,Count,CountPolygonVertices,PolygonVertexIndex,IndexA,IndexB:TpvSizeInt;
begin
 for Index:=0 To fVertices.Count-1 do begin
  fVertices.Items[Index].Flip;
 end;
 case fMode of
  TMode.Triangles:begin
   Index:=0;
   Count:=fIndices.Count;
   while (Index+2)<Count do begin
    fIndices.Exchange(Index+0,Index+2);
    inc(Index,3);
   end;
  end;
  else {TMode.Polygons:}begin
   Index:=0;
   Count:=fIndices.Count;
   while Index<Count do begin
    CountPolygonVertices:=fIndices.Items[Index];
    inc(Index);
    if CountPolygonVertices>0 then begin
     if (Index+(CountPolygonVertices-1))<Count then begin
      for PolygonVertexIndex:=0 to (CountPolygonVertices shr 1)-1 do begin
       IndexA:=Index+PolygonVertexIndex;
       IndexB:=Index+(CountPolygonVertices-(PolygonVertexIndex+1));
       if IndexA<>IndexB then begin
        fIndices.Exchange(IndexA,IndexB);
       end;
      end;
      inc(Index,CountPolygonVertices);
     end else begin
      Assert(false);
     end;
    end;
   end;
  end;
 end;
end;

procedure TpvCSGBSP.TMesh.ConvertToPolygons;
begin
 SetMode(TMode.Polygons);
end;

procedure TpvCSGBSP.TMesh.ConvertToTriangles;
begin
 SetMode(TMode.Triangles);
end;

procedure TpvCSGBSP.TMesh.Canonicalize;
var Index,Count,CountPolygonVertices:TpvSizeInt;
    NewIndices:TIndexList;
begin
 NewIndices.Initialize;
 try
  Index:=0;
  Count:=fIndices.Count;
  while Index<Count do begin
   case fMode of
    TMode.Triangles:begin
     CountPolygonVertices:=3;
    end;
    else {TMode.Polygons:}begin
     CountPolygonVertices:=fIndices.Items[Index];
     inc(Index);
    end;
   end;
   if CountPolygonVertices>2 then begin
    if (Index+(CountPolygonVertices-1))<Count then begin
     case fMode of
      TMode.Triangles:begin
      end;
      else {TMode.Polygons:}begin
       NewIndices.Add(CountPolygonVertices);
      end;
     end;
     NewIndices.AddRangeFrom(fIndices,Index,CountPolygonVertices);
    end;
   end;
   inc(Index,CountPolygonVertices);
  end;
  SetIndices(NewIndices);
 finally
  NewIndices.Finalize;
 end;
 RemoveDuplicateAndUnusedVertices;
end;

procedure TpvCSGBSP.TMesh.CalculateNormals(const aSoftNormals:boolean=true);
var Index,Count,CountPolygonVertices:TpvSizeInt;
    Vertex0,Vertex1,Vertex2:PVertex;
    Vertex:TVertex;
    Normal:TVector3;
    Normals:array of TVector3;
begin
 Normals:=nil;
 try
  RemoveDuplicateAndUnusedVertices;
  try
   if aSoftNormals then begin
    SetLength(Normals,fVertices.Count);
    for Index:=0 to fVertices.Count-1 do begin
     Normals[Index].x:=0.0;
     Normals[Index].y:=0.0;
     Normals[Index].z:=0.0;
    end;
   end;
   Index:=0;
   Count:=fIndices.Count;
   while Index<Count do begin
    case fMode of
     TMode.Triangles:begin
      CountPolygonVertices:=3;
     end;
     else {TMode.Polygons:}begin
      CountPolygonVertices:=fIndices.Items[Index];
      inc(Index);
     end;
    end;
    if CountPolygonVertices>2 then begin
     Vertex0:=@fVertices.Items[fIndices.Items[Index+0]];
     Vertex1:=@fVertices.Items[fIndices.Items[Index+1]];
     Vertex2:=@fVertices.Items[fIndices.Items[Index+2]];
     Normal:=(Vertex1^.Position-Vertex0^.Position).Cross(Vertex2^.Position-Vertex0^.Position).Normalize;
    end else begin
     Normal.x:=0.0;
     Normal.y:=0.0;
     Normal.z:=0.0;
    end;
    while (CountPolygonVertices>0) and (Index<Count) do begin
     if aSoftNormals then begin
      Normals[fIndices.Items[Index]]:=Normals[fIndices.Items[Index]]+Normal;
     end else begin
      Vertex:=fVertices.Items[fIndices.Items[Index]];
      Vertex.Normal:=Normal;
      fIndices.Items[Index]:=fVertices.Add(Vertex);
     end;
     inc(Index);
     dec(CountPolygonVertices);
    end;
   end;
   if aSoftNormals then begin
    for Index:=0 to length(Normals)-1 do begin
     fVertices.Items[Index].Normal:=Normals[Index].Normalize;
    end;
   end;
  finally
   RemoveDuplicateAndUnusedVertices;
  end;
 finally
  SetLength(Normals,0);
 end;
end;

procedure TpvCSGBSP.TMesh.RemoveDuplicateAndUnusedVertices;
const HashBits=16;
      HashSize=1 shl HashBits;
      HashMask=HashSize-1;
type THashTableItem=record
      Next:TpvSizeInt;
      Hash:TpvUInt32;
      VertexIndex:TIndex;
     end;
     PHashTableItem=^THashTableItem;
     THashTableItems=array of THashTableItem;
     THashTable=array of TpvSizeInt;
var Index,Count,HashIndex,CountHashTableItems,
    CountPolygonVertices:TpvSizeInt;
    Vertex,OtherVertex:PVertex;
    VertexIndex:TIndex;
    NewVertices:TVertexList;
    NewIndices:TIndexList;
    HashTable:THashTable;
    HashTableItems:THashTableItems;
    Hash:TpvUInt32;
    HashTableItem:PHashTableItem;
begin
 NewVertices.Initialize;
 try
  NewIndices.Initialize;
  try
   HashTable:=nil;
   try
    SetLength(HashTable,HashSize);
    for Index:=0 to HashSize-1 do begin
     HashTable[Index]:=-1;
    end;
    HashTableItems:=nil;
    CountHashTableItems:=0;
    try
     Index:=0;
     Count:=fIndices.Count;
     while Index<Count do begin
      case fMode of
       TMode.Triangles:begin
        CountPolygonVertices:=3;
       end;
       else {TMode.Polygons:}begin
        CountPolygonVertices:=fIndices.Items[Index];
        NewIndices.Add(CountPolygonVertices);
        inc(Index);
       end;
      end;
      while (CountPolygonVertices>0) and (Index<Count) do begin
       dec(CountPolygonVertices);
       Vertex:=@fVertices.Items[fIndices.Items[Index]];
       VertexIndex:=-1;
       Hash:=(trunc(Vertex.Position.x*4096)*73856093) xor
             (trunc(Vertex.Position.y*4096)*19349653) xor
             (trunc(Vertex.Position.z*4096)*83492791);
       HashIndex:=HashTable[Hash and HashMask];
       while HashIndex>=0 do begin
        HashTableItem:=@HashTableItems[HashIndex];
        if HashTableItem^.Hash=Hash then begin
         OtherVertex:=@NewVertices.Items[HashTableItem^.VertexIndex];
         if Vertex^=OtherVertex^ then begin
          VertexIndex:=HashTableItem^.VertexIndex;
          break;
         end;
        end;
        HashIndex:=HashTableItem^.Next;
       end;
       if VertexIndex<0 then begin
        VertexIndex:=NewVertices.Add(Vertex^);
        HashIndex:=CountHashTableItems;
        inc(CountHashTableItems);
        if CountHashTableItems>length(HashTableItems) then begin
         SetLength(HashTableItems,CountHashTableItems*2);
        end;
        HashTableItem:=@HashTableItems[HashIndex];
        HashTableItem^.Next:=HashTable[Hash and HashMask];
        HashTable[Hash and HashMask]:=HashIndex;
        HashTableItem^.Hash:=Hash;
        HashTableItem^.VertexIndex:=VertexIndex;
       end;
       NewIndices.Add(VertexIndex);
       inc(Index);
      end;
     end;
     SetVertices(NewVertices);
     SetIndices(NewIndices);
    finally
     HashTableItems:=nil;
    end;
   finally
    HashTable:=nil;
   end;
  finally
   NewIndices.Finalize;
  end;
 finally
  NewVertices.Finalize;
 end;
end;

procedure TpvCSGBSP.TMesh.FixTJunctions;
const Map:array[0..2,0..3] of TpvSizeInt=((3,1,2,2),(0,3,2,0),(0,1,3,1));
type TAABBTreeStack=TDynamicStack<TpvSizeInt>;
var Index,TriangleVertexIndex,VertexIndex,
    CountIndices,CountVertices,Previous:TpvSizeInt;
    TryAgain:boolean;
    Position0,Position1,VertexPosition,
    Direction,DifferenceToVertexPosition:TVector3;
    Time:TFloat;
    CurrentVertex:PVertex;
    Vertex:TVertex;
    VertexIndices:array[0..3] of TIndex;
    Vertices:array[0..3] of PVertex;
    VertexDynamicAABBTree:TDynamicAABBTree;
    AABB,PointAABB:TAABB;
    AABBTreeStack:TAABBTreeStack;
    NodeID:TpvSizeInt;
    Node:PDynamicAABBTreeNode;
begin
 SetMode(TMode.Triangles);
 RemoveDuplicateAndUnusedVertices;
 try
  VertexDynamicAABBTree:=TDynamicAABBTree.Create;
  try
   AABBTreeStack.Initialize;
   try
    for Index:=0 to fVertices.Count-1 do begin
     CurrentVertex:=@fVertices.Items[Index];
     PointAABB.Min:=CurrentVertex^.Position-ThresholdAABBVector;
     PointAABB.Max:=CurrentVertex^.Position+ThresholdAABBVector;
     VertexDynamicAABBTree.CreateProxy(PointAABB,Index);
    end;
    repeat
     TryAgain:=false;
     CountIndices:=fIndices.Count;
     CountVertices:=fVertices.Count;
     Index:=0;
     while ((Index+2)<CountIndices) and not TryAgain do begin
      VertexIndices[0]:=fIndices.Items[Index+0];
      VertexIndices[1]:=fIndices.Items[Index+1];
      VertexIndices[2]:=fIndices.Items[Index+2];
      Vertices[0]:=@fVertices.Items[VertexIndices[0]];
      Vertices[1]:=@fVertices.Items[VertexIndices[1]];
      Vertices[2]:=@fVertices.Items[VertexIndices[2]];
      Previous:=2;
      TriangleVertexIndex:=0;
      while (TriangleVertexIndex<3) and not TryAgain do begin
       Position0:=fVertices.Items[VertexIndices[Previous]].Position;
       Position1:=fVertices.Items[VertexIndices[TriangleVertexIndex]].Position;
       AABB.Min.x:=Min(Position0.x,Position1.x)-Epsilon;
       AABB.Min.y:=Min(Position0.y,Position1.y)-Epsilon;
       AABB.Min.z:=Min(Position0.z,Position1.z)-Epsilon;
       AABB.Max.x:=Max(Position0.x,Position1.x)+Epsilon;
       AABB.Max.y:=Max(Position0.y,Position1.y)+Epsilon;
       AABB.Max.z:=Max(Position0.z,Position1.z)+Epsilon;
       if VertexDynamicAABBTree.Root>=0 then begin
        AABBTreeStack.Count:=0;
        AABBTreeStack.Push(VertexDynamicAABBTree.Root);
        while AABBTreeStack.Pop(NodeID) and not TryAgain do begin
         Node:=@VertexDynamicAABBTree.Nodes[NodeID];
         if Node^.AABB.Intersects(AABB) then begin
          if Node^.Children[0]<0 then begin
           VertexIndex:=Node^.UserData;
           if VertexIndex>=0 then begin
            CurrentVertex:=@fVertices.Items[VertexIndex];
            if (VertexIndices[0]<>VertexIndex) and
               (VertexIndices[1]<>VertexIndex) and
               (VertexIndices[2]<>VertexIndex) and
               (fVertices.Items[VertexIndices[0]].Position<>CurrentVertex^.Position) and
               (fVertices.Items[VertexIndices[1]].Position<>CurrentVertex^.Position) and
               (fVertices.Items[VertexIndices[2]].Position<>CurrentVertex^.Position) and
               ((Position0-CurrentVertex^.Position).Normalize.Dot((CurrentVertex^.Position-Position1).Normalize)>=TJunctionOneMinusEpsilon) then begin
             Direction:=Position1-Position0;
             DifferenceToVertexPosition:=CurrentVertex^.Position-Position0;
             Time:=DifferenceToVertexPosition.Dot(Direction)/Direction.Dot(Direction);
             if ((Time>=TJunctionEpsilon) and (Time<=TJunctionOneMinusEpsilon)) and
                (((Direction*Time)-DifferenceToVertexPosition).SquaredLength<TJunctionEpsilon) then begin
              VertexPosition:=CurrentVertex^.Position;
              Vertex:=fVertices.Items[fIndices.Items[Index+Previous]].Lerp(fVertices.Items[fIndices.Items[Index+TriangleVertexIndex]],Time);
//            Vertex.Position:=VertexPosition;
              VertexIndices[3]:=fVertices.Add(Vertex);
              if Vertex.Position<>VertexPosition then begin
               PointAABB.Min:=Vertex.Position-ThresholdAABBVector;
               PointAABB.Max:=Vertex.Position+ThresholdAABBVector;
               VertexDynamicAABBTree.CreateProxy(PointAABB,VertexIndices[3]);
              end;
              fIndices.Items[Index+Map[TriangleVertexIndex,3]]:=VertexIndices[3];
              fIndices.Add([VertexIndices[Map[TriangleVertexIndex,0]],
                            VertexIndices[Map[TriangleVertexIndex,1]],
                            VertexIndices[Map[TriangleVertexIndex,2]]]);
              TryAgain:=true;
              break;
             end;
            end;
           end;
          end else begin
           if Node^.Children[1]>=0 then begin
            AABBTreeStack.Push(Node^.Children[1]);
           end;
           if Node^.Children[0]>=0 then begin
            AABBTreeStack.Push(Node^.Children[0]);
           end;
          end;
         end;
        end;
       end;
       Previous:=TriangleVertexIndex;
       inc(TriangleVertexIndex);
      end;
      inc(Index,3);
     end;
    until not TryAgain;
   finally
    AABBTreeStack.Finalize;
   end;
  finally
   FreeAndNil(VertexDynamicAABBTree);
  end;
 finally
  RemoveDuplicateAndUnusedVertices;
 end;
end;

function TpvCSGBSP.TMesh.ToNode(const aSplitSettings:PSplitSettings=nil):TNode;
begin
 result:=TNode.Create(self);
 result.Build(fIndices,aSplitSettings);
end;

{ TpvCSGBSP.TNode }

constructor TpvCSGBSP.TNode.Create(const aMesh:TMesh);
begin
 inherited Create;
 fMesh:=aMesh;
 fIndices.Initialize;
 fPointerToIndices:=@fIndices;
 fBack:=nil;
 fFront:=nil;
end;

destructor TpvCSGBSP.TNode.Destroy;
type TJobStack=TDynamicStack<TNode>;
var JobStack:TJobStack;
    Node:TNode;
begin
 fIndices.Finalize;
 if assigned(fFront) or assigned(fBack) then begin
  JobStack.Initialize;
  try
   JobStack.Push(self);
   while JobStack.Pop(Node) do begin
    if assigned(Node.fFront) then begin
     JobStack.Push(Node.fFront);
     Node.fFront:=nil;
    end;
    if assigned(Node.fBack) then begin
     JobStack.Push(Node.fBack);
     Node.fBack:=nil;
    end;
    if Node<>self then begin
     FreeAndNil(Node);
    end;
   end;
  finally
   JobStack.Finalize;
  end;
 end;
 fPointerToIndices:=nil;
 inherited Destroy;
end;

procedure TpvCSGBSP.TNode.SetIndices(const aIndices:TIndexList);
begin
 fIndices.Assign(aIndices);
end;

procedure TpvCSGBSP.TNode.Invert;
type TJobStack=TDynamicStack<TNode>;
var JobStack:TJobStack;
    Node,TempNode:TNode;
    Index,Count,
    CountPolygonVertices,PolygonVertexIndex,
    IndexA,IndexB:TpvSizeInt;
begin
 fMesh.Invert;
 JobStack.Initialize;
 try
  JobStack.Push(self);
  while JobStack.Pop(Node) do begin
   case fMesh.fMode of
    TMesh.TMode.Triangles:begin
     Index:=0;
     Count:=Node.fIndices.Count;
     while (Index+2)<Count do begin
      Node.fIndices.Exchange(Index+0,Index+2);
      inc(Index,3);
     end;
    end;
    else {TMesh.TMode.Polygons:}begin
     Index:=0;
     Count:=Node.fIndices.Count;
     while Index<Count do begin
      CountPolygonVertices:=Node.fIndices.Items[Index];
      inc(Index);
      if CountPolygonVertices>0 then begin
       if (Index+(CountPolygonVertices-1))<Count then begin
        for PolygonVertexIndex:=0 to (CountPolygonVertices shr 1)-1 do begin
         IndexA:=Index+PolygonVertexIndex;
         IndexB:=Index+(CountPolygonVertices-(PolygonVertexIndex+1));
         if IndexA<>IndexB then begin
          Node.fIndices.Exchange(IndexA,IndexB);
         end;
        end;
       end else begin
        Assert(false);
       end;
       inc(Index,CountPolygonVertices);
      end;
     end;
    end;
   end;
   Node.fPlane:=Node.fPlane.Flip;
   TempNode:=Node.fBack;
   Node.fBack:=Node.fFront;
   Node.fFront:=TempNode;
   if assigned(Node.fFront) then begin
    JobStack.Push(Node.fFront);
   end;
   if assigned(Node.fBack) then begin
    JobStack.Push(Node.fBack);
   end;
  end;
 finally
  JobStack.Finalize;
 end;
end;

procedure TpvCSGBSP.TNode.EvaluateSplitPlane(const aPlane:TPlane;
                                             out aCountPolygonsSplits:TpvSizeInt;
                                             out aCountBackPolygons:TpvSizeInt;
                                             out aCountFrontPolygons:TpvSizeInt);
const TriangleSplitMask=(0 shl 0) or (1 shl 2) or (1 shl 2) or (1 shl 3) or (1 shl 4) or (1 shl 5) or (1 shl 6) or (0 shl 7);
      BackTriangleMask=(1 shl 0) or (2 shl 2) or (2 shl 4) or (1 shl (3 shl 1)) or (2 shl (4 shl 1)) or (1 shl (5 shl 1)) or (1 shl (6 shl 1)) or (0 shl (7 shl 1));
      FrontTriangleMask=(0 shl 0) or (1 shl 2) or (1 shl 4) or (2 shl (3 shl 1)) or (1 shl (4 shl 1)) or (2 shl (5 shl 1)) or (2 shl (6 shl 1)) or (1 shl (7 shl 1));
var Index,Count,CountPolygonVertices,Code:TpvSizeInt;
    Vertices:TVertexList;
begin
 aCountPolygonsSplits:=0;
 aCountBackPolygons:=0;
 aCountFrontPolygons:=0;
 Count:=fIndices.Count;
 if Count>0 then begin
  Vertices:=fMesh.fVertices;
  Index:=0;
  while Index<Count do begin
   case fMesh.fMode of
    TMesh.TMode.Triangles:begin
     CountPolygonVertices:=3;
    end;
    else {TMesh.TMode.Polygons:}begin
     CountPolygonVertices:=fIndices.Items[Index];
     inc(Index);
    end;
   end;
   if (CountPolygonVertices>2) and ((Index+(CountPolygonVertices-1))<Count) then begin
    Code:=((ord(aPlane.DistanceTo(Vertices.Items[fIndices.Items[Index+0]].Position)>0.0) and 1) shl 2) or
          ((ord(aPlane.DistanceTo(Vertices.Items[fIndices.Items[Index+1]].Position)>0.0) and 1) shl 1) or
          ((ord(aPlane.DistanceTo(Vertices.Items[fIndices.Items[Index+2]].Position)>0.0) and 1) shl 0);
    inc(aCountPolygonsSplits,(TriangleSplitMask shr Code) and 1);
    inc(aCountBackPolygons,(BackTriangleMask shr (Code shl 1)) and 3);
    inc(aCountFrontPolygons,(FrontTriangleMask shr (Code shl 1)) and 3);
   end;
   inc(Index,CountPolygonVertices);
  end;
 end;
end;

function TpvCSGBSP.TNode.FindSplitPlane(const aIndices:TIndexList;
                                        const aSplitSettings:PSplitSettings):TPlane;
var Index,Count,TriangleCount,VertexBaseIndex,
    CountPolygonsSplits,CountBackPolygons,CountFrontPolygons,
    CountPolygonVertices:TpvSizeInt;
    Plane:TPlane;
    Score,BestScore:TFloat;
    Vertices:TVertexList;
    SplitSettings:PSplitSettings;
begin
 if assigned(aSplitSettings) then begin
  SplitSettings:=aSplitSettings;
 end else begin
  SplitSettings:=@DefaultSplitSettings;
 end;
 if SplitSettings^.SearchBestFactor<=0 then begin
  if aIndices.Count>2 then begin
   Index:=0;
   case fMesh.fMode of
    TMesh.TMode.Triangles:begin
     CountPolygonVertices:=3;
    end;
    else {TMesh.TMode.Polygons:}begin
     CountPolygonVertices:=aIndices.Items[Index];
     inc(Index);
    end;
   end;
   Count:=aIndices.Count;
   if (CountPolygonVertices>2) and ((Index+(CountPolygonVertices-1))<Count) then begin
    result:=TPlane.Create(fMesh.fVertices.Items[aIndices.Items[Index+0]].Position,
                          fMesh.fVertices.Items[aIndices.Items[Index+1]].Position,
                          fMesh.fVertices.Items[aIndices.Items[Index+2]].Position);
   end;
  end else begin
   result:=TPlane.CreateEmpty;
  end;
 end else begin
  result:=TPlane.CreateEmpty;
  case fMesh.fMode of
   TMesh.TMode.Triangles:begin
    Count:=aIndices.Count;
    TriangleCount:=Count div 3;
    Vertices:=fMesh.fVertices;
    BestScore:=Infinity;
    for Index:=0 to ((TriangleCount+(SplitSettings^.SearchBestFactor-1)) div SplitSettings^.SearchBestFactor)-1 do begin
     if SplitSettings^.SearchBestFactor=1 then begin
      VertexBaseIndex:=(Index mod TriangleCount)*3;
     end else begin
      VertexBaseIndex:=Random(TriangleCount)*3;
     end;
     Plane:=TPlane.Create(Vertices.Items[aIndices.Items[VertexBaseIndex+0]].Position,
                          Vertices.Items[aIndices.Items[VertexBaseIndex+1]].Position,
                          Vertices.Items[aIndices.Items[VertexBaseIndex+2]].Position);
     EvaluateSplitPlane(Plane,CountPolygonsSplits,CountBackPolygons,CountFrontPolygons);
     Score:=(CountPolygonsSplits*SplitSettings^.PolygonSplitCost)+
            (abs(CountBackPolygons-CountFrontPolygons)*SplitSettings^.PolygonImbalanceCost);
     if (Index=0) or (BestScore>Score) then begin
      BestScore:=Score;
      result:=Plane;
     end;
    end;
   end;
   else {TMesh.TMode.Polygons:}begin
    Count:=aIndices.Count;
    Vertices:=fMesh.fVertices;
    BestScore:=Infinity;
    Index:=0;
    while Index<Count do begin
     case fMesh.fMode of
      TMesh.TMode.Triangles:begin
       CountPolygonVertices:=3;
      end;
      else {TMesh.TMode.Polygons:}begin
       CountPolygonVertices:=aIndices.Items[Index];
       inc(Index);
      end;
     end;
     if (CountPolygonVertices>2) and ((Index+(CountPolygonVertices-1))<Count) then begin
      VertexBaseIndex:=Index;
      Plane:=TPlane.Create(Vertices.Items[aIndices.Items[VertexBaseIndex+0]].Position,
                           Vertices.Items[aIndices.Items[VertexBaseIndex+1]].Position,
                           Vertices.Items[aIndices.Items[VertexBaseIndex+2]].Position);
      EvaluateSplitPlane(Plane,CountPolygonsSplits,CountBackPolygons,CountFrontPolygons);
      Score:=(CountPolygonsSplits*SplitSettings^.PolygonSplitCost)+
             (abs(CountBackPolygons-CountFrontPolygons)*SplitSettings^.PolygonImbalanceCost);
      if (Index=0) or (BestScore>Score) then begin
       BestScore:=Score;
       result:=Plane;
      end;
     end;
     inc(Index,CountPolygonVertices);
    end;
   end;
  end;
 end;
end;

procedure TpvCSGBSP.TNode.Build(const aIndices:TIndexList;
                                const aSplitSettings:PSplitSettings=nil);
type TJobStackItem=record
      Node:TNode;
      Indices:TIndexList;
     end;
     TJobStack=TDynamicStack<TJobStackItem>;
var JobStack:TJobStack;
    JobStackItem,NewJobStackItem,FrontJobStackItem,BackJobStackItem:TJobStackItem;
    Index,CountVertexIndices:TpvSizeInt;
begin
 JobStack.Initialize;
 try
  NewJobStackItem.Node:=self;
  NewJobStackItem.Indices:=aIndices;
  JobStack.Push(NewJobStackItem);
  while JobStack.Pop(JobStackItem) do begin
   try
    CountVertexIndices:=JobStackItem.Indices.Count;
    if ((fMesh.fMode=TMesh.TMode.Triangles) and (CountVertexIndices>2)) or
       ((fMesh.fMode=TMesh.TMode.Polygons) and (CountVertexIndices>1)) then begin
     FrontJobStackItem.Indices.Initialize;
     BackJobStackItem.Indices.Initialize;
     if not JobStackItem.Node.fPlane.OK then begin
      JobStackItem.Node.fPlane:=FindSplitPlane(JobStackItem.Indices,aSplitSettings);
     end;
     case fMesh.fMode of
      TMesh.TMode.Triangles:begin
       JobStackItem.Node.fPlane.SplitTriangles(fMesh.fVertices,
                                               JobStackItem.Indices,
                                               @JobStackItem.Node.fIndices,
                                               @JobStackItem.Node.fIndices,
                                               @BackJobStackItem.Indices,
                                               @FrontJobStackItem.Indices);
      end;
      else {TMesh.TMode.Polygons:}begin
       JobStackItem.Node.fPlane.SplitPolygons(fMesh.fVertices,
                                              JobStackItem.Indices,
                                              @JobStackItem.Node.fIndices,
                                              @JobStackItem.Node.fIndices,
                                              @BackJobStackItem.Indices,
                                              @FrontJobStackItem.Indices);
      end;
     end;
     if BackJobStackItem.Indices.Count>0 then begin
      if not assigned(JobStackItem.Node.fBack) then begin
       JobStackItem.Node.fBack:=TNode.Create(fMesh);
      end;
      BackJobStackItem.Node:=JobStackItem.Node.fBack;
      JobStack.Push(BackJobStackItem);
     end;
     if FrontJobStackItem.Indices.Count>0 then begin
      if not assigned(JobStackItem.Node.fFront) then begin
       JobStackItem.Node.fFront:=TNode.Create(fMesh);
      end;
      FrontJobStackItem.Node:=JobStackItem.Node.fFront;
      JobStack.Push(FrontJobStackItem);
     end;
    end;
   finally
    JobStackItem.Indices.Finalize;
   end;
  end;
 finally
  JobStack.Finalize;
 end;
end;

function TpvCSGBSP.TNode.ClipPolygons(var aVertices:TVertexList;const aIndices:TIndexList):TIndexList;
type TJobStackItem=record
      Node:TNode;
      Indices:TIndexList;
     end;
     TJobStack=TDynamicStack<TJobStackItem>;
var JobStack:TJobStack;
    JobStackItem,NewJobStackItem,FrontJobStackItem,BackJobStackItem:TJobStackItem;
    Index,Count:TpvSizeInt;
    BackIndices:PIndexList;
begin
 result.Initialize;
 try
  JobStack.Initialize;
  try
   NewJobStackItem.Node:=self;
   NewJobStackItem.Indices.Assign(aIndices);
   JobStack.Push(NewJobStackItem);
   while JobStack.Pop(JobStackItem) do begin
    try
     if JobStackItem.Node.fPlane.OK then begin
      FrontJobStackItem.Indices.Initialize;
      try
       if assigned(JobStackItem.Node.fBack) then begin
        BackJobStackItem.Indices.Initialize;
        BackIndices:=@BackJobStackItem.Indices;
       end else begin
        BackIndices:=nil;
       end;
       try
        case fMesh.fMode of
         TMesh.TMode.Triangles:begin
          JobStackItem.Node.fPlane.SplitTriangles(aVertices,
                                                  JobStackItem.Indices,
                                                  BackIndices,
                                                  @FrontJobStackItem.Indices,
                                                  BackIndices,
                                                  @FrontJobStackItem.Indices);
         end;
         else {TMesh.TMode.Polygons:}begin
          JobStackItem.Node.fPlane.SplitPolygons(aVertices,
                                                 JobStackItem.Indices,
                                                 BackIndices,
                                                 @FrontJobStackItem.Indices,
                                                 BackIndices,
                                                 @FrontJobStackItem.Indices);
         end;
        end;
        if assigned(JobStackItem.Node.fBack) then begin
         BackJobStackItem.Node:=JobStackItem.Node.fBack;
         JobStack.Push(BackJobStackItem);
        end;
        if assigned(JobStackItem.Node.fFront) then begin
         FrontJobStackItem.Node:=JobStackItem.Node.fFront;
         JobStack.Push(FrontJobStackItem);
        end else if FrontJobStackItem.Indices.Count>0 then begin
         result.Add(FrontJobStackItem.Indices);
        end;
       finally
        BackJobStackItem.Indices.Finalize;
       end;
      finally
       FrontJobStackItem.Indices.Finalize;
      end;
     end else if JobStackItem.Indices.Count>0 then begin
      result.Add(JobStackItem.Indices);
     end;
    finally
     JobStackItem.Indices.Finalize;
    end;
   end;
  finally
   JobStack.Finalize;
  end;
 except
  result.Finalize;
  raise;
 end;
end;

procedure TpvCSGBSP.TNode.ClipTo(const aNode:TNode);
type TJobStack=TDynamicStack<TNode>;
var JobStack:TJobStack;
    Node:TNode;
begin
 JobStack.Initialize;
 try
  JobStack.Push(self);
  while JobStack.Pop(Node) do begin
   Node.SetIndices(aNode.ClipPolygons(Node.fMesh.fVertices,Node.fIndices));
   if assigned(Node.fFront) then begin
    JobStack.Push(Node.fFront);
   end;
   if assigned(Node.fBack) then begin
    JobStack.Push(Node.fBack);
   end;
  end;
 finally
  JobStack.Finalize;
 end;
end;

procedure TpvCSGBSP.TNode.Merge(const aNode:TNode;
                                const aSplitSettings:PSplitSettings=nil);
var Index,Offset,Count,CountPolygonVertices:TpvSizeInt;
    OtherMesh:TMesh;
begin
 Offset:=fMesh.fVertices.Count;
 OtherMesh:=aNode.ToMesh;
 try
  OtherMesh.SetMode(fMesh.fMode);
  fMesh.fVertices.Add(OtherMesh.fVertices);
  case fMesh.fMode of
   TMesh.TMode.Triangles:begin
    for Index:=0 to OtherMesh.fIndices.Count-1 do begin
     OtherMesh.fIndices.Items[Index]:=OtherMesh.fIndices.Items[Index]+Offset;
    end;
   end;
   else {TMesh.TMode.Polygons:}begin
    Index:=0;
    Count:=OtherMesh.fIndices.Count;
    while Index<Count do begin
     CountPolygonVertices:=OtherMesh.fIndices.Items[Index];
     inc(Index);
     while (CountPolygonVertices>0) and (Index<Count) do begin
      OtherMesh.fIndices.Items[Index]:=OtherMesh.fIndices.Items[Index]+Offset;
      inc(Index);
      dec(CountPolygonVertices);
     end;
    end;
   end;
  end;
  Build(OtherMesh.fIndices,aSplitSettings);
 finally
  FreeAndNil(OtherMesh);
 end;
end;

function TpvCSGBSP.TNode.ToMesh:TMesh;
type TJobStack=TDynamicStack<TNode>;
var JobStack:TJobStack;
    Node:TNode;
begin
 result:=TMesh.Create(fMesh.fMode);
 try
  result.SetVertices(fMesh.fVertices);
  JobStack.Initialize;
  try
   JobStack.Push(self);
   while JobStack.Pop(Node) do begin
    result.fIndices.Add(Node.fIndices);
    if assigned(Node.fFront) then begin
     JobStack.Push(Node.fFront);
    end;
    if assigned(Node.fBack) then begin
     JobStack.Push(Node.fBack);
    end;
   end;
  finally
   JobStack.Finalize;
  end;
 finally
  result.RemoveDuplicateAndUnusedVertices;
 end;
end;

end.
