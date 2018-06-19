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
unit PasVulkan.CSG.BSP.TriangleMesh;
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

type TpvCSGBSPTriangleMesh=class
      public
       const Epsilon=1e-5;
             OneMinusEpsilon=1.0-Epsilon;
             OnePlusEpsilon=1.0+Epsilon;
             TJunctionEpsilon=1e-4;
             TJunctionOneMinusEpsilon=1.0-TJunctionEpsilon;
             TJunctionOnePlusEpsilon=1.0+TJunctionEpsilon;
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
              procedure Assign(const aFrom:TDynamicArray<T>); overload;
              procedure Assign(const aItems:array of T); overload;
              function AddNew:TpvSizeInt; overload;
              function Add(const aItem:T):TpvSizeInt; overload;
              function Add(const aItems:array of T):TpvSizeInt; overload;
              function Add(const aFrom:TDynamicArray<T>):TpvSizeInt; overload;
              procedure Exchange(const aIndexA,aIndexB:TpvSizeInt);
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
              class operator Add(const aLeft,aRight:TVertex):TVertex;
              class operator Subtract(const aLeft,aRight:TVertex):TVertex;
              class operator Multiply(const aLeft:TVertex;const aRight:TFloat):TVertex;
              class operator Divide(const aLeft:TVertex;const aRight:TFloat):TVertex;
              class operator Negative(const aLeft:TVertex):TVertex;
              function Lerp(const aWith:TVertex;const aTime:TFloat):TVertex;
              function Flip:TVertex;
              function Normalize:TVertex;
            end;
            PVertex=^TVertex;
            TVertexList=TDynamicArray<TVertex>;
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
              procedure Split(var aVertices:TVertexList;
                              const aIndices:TIndexList;
                              const aCoplanarBackList:PIndexList;
                              const aCoplanarFrontList:PIndexList;
                              const aBackList:PIndexList;
                              const aFrontList:PIndexList);
            end;
            PPlane=^TPlane;
            TNode=class;
            TMesh=class
             public
              type TCSGOperation=
                    (
                     Union,
                     Subtraction,
                     Intersection
                    );
             private
              fVertices:TVertexList;
              fIndices:TIndexList;
              procedure SetVertices(const aVertices:TVertexList);
              procedure SetIndices(const aIndices:TIndexList);
             public
              constructor Create; reintroduce; overload;
              constructor Create(const aFrom:TMesh); reintroduce; overload;
              constructor Create(const aFrom:TNode); reintroduce; overload;
              constructor CreateCube(const aCX,aCY,aCZ,aRX,aRY,aRZ:TFloat);
              constructor CreateSphere(const aCX,aCY,aCZ,aRadius:TFloat;const aSlices:TpvSizeInt=16;const aStacks:TpvSizeInt=8);
              constructor CreateFromCSGOperation(const aLeftMesh:TMesh;
                                                 const aRightMesh:TMesh;
                                                 const aCSGOperation:TCSGOperation);
              constructor CreateUnion(const aLeftMesh,aRightMesh:TMesh);
              constructor CreateSubtraction(const aLeftMesh,aRightMesh:TMesh);
              constructor CreateIntersection(const aLeftMesh,aRightMesh:TMesh);
              constructor CreateSymmetricDifference(const aLeftMesh,aRightMesh:TMesh);
              constructor CreateRobustUnion(const aLeftMesh,aRightMesh:TMesh);
              constructor CreateRobustSubtraction(const aLeftMesh,aRightMesh:TMesh);
              constructor CreateRobustIntersection(const aLeftMesh,aRightMesh:TMesh);
              constructor CreateRobustSymmetricDifference(const aLeftMesh,aRightMesh:TMesh);
              destructor Destroy; override;
              procedure Assign(const aFrom:TMesh);
              procedure Invert;
              procedure RemoveDuplicateVertices;
              procedure FixTJunctions;
              function ToNode:TNode;
             published
              property Vertices:TVertexList read fVertices write SetVertices;
              property Indices:TIndexList read fIndices write SetIndices;
            end;
            TNode=class
             private
              fIndices:TIndexList;
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
                                           out aCountTriangleSplits:TpvSizeInt;
                                           out aCountBackTriangles:TpvSizeInt;
                                           out aCountFrontTriangles:TpvSizeInt);
              procedure FindSplitPlane(const aIndices:TIndexList;
                                       out aPlane:TPlane;
                                       out aTriangleStartVertexIndex:TpvSizeInt;
                                       const aSearchBestFactor:TpvSizeInt=1;
                                       const aTriangleSplitCost:TFloat=1.0;
                                       const aTriangleImbalanceCost:TFloat=0.25);
              function ClipTriangles(var aVertices:TVertexList;const aIndices:TIndexList):TIndexList;
              procedure ClipTo(const aNode:TNode);
              procedure Merge(const aNode:TNode);
              procedure Build(const aIndices:TIndexList;const aDoFree:boolean=false);
              function ToMesh:TMesh;
              property Plane:TPlane read fPlane write fPlane;
             published
              property Indices:TIndexList read fIndices write SetIndices;
              property Mesh:TMesh read fMesh write fMesh;
              property Back:TNode read fBack write fBack;
              property Front:TNode read fFront write fFront;
            end;
      public
       class function EpsilonSign(const aValue:TFloat):TpvSizeInt;
     end;


implementation

{ TpvCSGBSP }

class function TpvCSGBSPTriangleMesh.EpsilonSign(const aValue:TFloat):TpvSizeInt;
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

procedure TpvCSGBSPTriangleMesh.TDynamicArray<T>.Initialize;
begin
 Items:=nil;
 Count:=0;
end;

procedure TpvCSGBSPTriangleMesh.TDynamicArray<T>.Finalize;
begin
 Items:=nil;
 Count:=0;
end;

procedure TpvCSGBSPTriangleMesh.TDynamicArray<T>.Clear;
begin
 Items:=nil;
 Count:=0;
end;

procedure TpvCSGBSPTriangleMesh.TDynamicArray<T>.Finish;
begin
 SetLength(Items,Count);
end;

procedure TpvCSGBSPTriangleMesh.TDynamicArray<T>.Assign(const aFrom:TDynamicArray<T>);
begin
 Items:=aFrom.Items;
 Count:=aFrom.Count;
end;

procedure TpvCSGBSPTriangleMesh.TDynamicArray<T>.Assign(const aItems:array of T);
var Index:TpvSizeInt;
begin
 Count:=length(aItems);
 SetLength(Items,Count);
 for Index:=0 to Count-1 do begin
  Items[Index]:=aItems[Index];
 end;
end;

function TpvCSGBSPTriangleMesh.TDynamicArray<T>.AddNew:TpvSizeInt;
begin
 result:=Count;
 if length(Items)<(Count+1) then begin
  SetLength(Items,(Count+1)+((Count+1) div 2));
 end;
 System.Initialize(Items[Count]);
 inc(Count);
end;

function TpvCSGBSPTriangleMesh.TDynamicArray<T>.Add(const aItem:T):TpvSizeInt;
begin
 result:=Count;
 if length(Items)<(Count+1) then begin
  SetLength(Items,(Count+1)+((Count+1) div 2));
 end;
 Items[Count]:=aItem;
 inc(Count);
end;

function TpvCSGBSPTriangleMesh.TDynamicArray<T>.Add(const aItems:array of T):TpvSizeInt;
var Index,FromCount:TpvSizeInt;
begin
 result:=Count;
 FromCount:=length(aItems);
 if FromCount>0 then begin
  if length(Items)<(Count+FromCount) then begin
   SetLength(Items,(Count+FromCount)+((Count+FromCount) div 2));
  end;
  for Index:=0 to FromCount-1 do begin
   Items[Count]:=aItems[Index];
   inc(Count);
  end;
 end;
end;

function TpvCSGBSPTriangleMesh.TDynamicArray<T>.Add(const aFrom:TDynamicArray<T>):TpvSizeInt;
var Index:TpvSizeInt;
begin
 result:=Count;
 if aFrom.Count>0 then begin
  if length(Items)<(Count+aFrom.Count) then begin
   SetLength(Items,(Count+aFrom.Count)+((Count+aFrom.Count) div 2));
  end;
  for Index:=0 to aFrom.Count-1 do begin
   Items[Count]:=aFrom.Items[Index];
   inc(Count);
  end;
 end;
end;

procedure TpvCSGBSPTriangleMesh.TDynamicArray<T>.Exchange(const aIndexA,aIndexB:TpvSizeInt);
var Temp:T;
begin
 Temp:=Items[aIndexA];
 Items[aIndexA]:=Items[aIndexB];
 Items[aIndexB]:=Temp;
end;

{ TpvCSGBSP.TDynamicStack<T> }

procedure TpvCSGBSPTriangleMesh.TDynamicStack<T>.Initialize;
begin
 Items:=nil;
 Count:=0;
end;

procedure TpvCSGBSPTriangleMesh.TDynamicStack<T>.Finalize;
begin
 Items:=nil;
 Count:=0;
end;

procedure TpvCSGBSPTriangleMesh.TDynamicStack<T>.Push(const aItem:T);
begin
 if length(Items)<(Count+1) then begin
  SetLength(Items,(Count+1)+((Count+1) div 2));
 end;
 Items[Count]:=aItem;
 inc(Count);
end;

function TpvCSGBSPTriangleMesh.TDynamicStack<T>.Pop(out aItem:T):boolean;
begin
 result:=Count>0;
 if result then begin
  dec(Count);
  aItem:=Items[Count];
 end;
end;

{ TpvCSGBSP.TVector2 }

constructor TpvCSGBSPTriangleMesh.TVector2.Create(const aFrom:TpvVector2);
begin
 x:=aFrom.x;
 y:=aFrom.y;
end;

class operator TpvCSGBSPTriangleMesh.TVector2.Equal(const aLeft,aRight:TVector2):boolean;
begin
 result:=SameValue(aLeft.x,aRight.x) and
         SameValue(aLeft.y,aRight.y);
end;

class operator TpvCSGBSPTriangleMesh.TVector2.NotEqual(const aLeft,aRight:TVector2):boolean;
begin
 result:=not (SameValue(aLeft.x,aRight.x) and
              SameValue(aLeft.y,aRight.y));
end;

class operator TpvCSGBSPTriangleMesh.TVector2.Add(const aLeft,aRight:TVector2):TVector2;
begin
 result.x:=aLeft.x+aRight.x;
 result.y:=aLeft.y+aRight.y;
end;

class operator TpvCSGBSPTriangleMesh.TVector2.Subtract(const aLeft,aRight:TVector2):TVector2;
begin
 result.x:=aLeft.x-aRight.x;
 result.y:=aLeft.y-aRight.y;
end;

class operator TpvCSGBSPTriangleMesh.TVector2.Multiply(const aLeft:TVector2;const aRight:TFloat):TVector2;
begin
 result.x:=aLeft.x*aRight;
 result.y:=aLeft.y*aRight;
end;

class operator TpvCSGBSPTriangleMesh.TVector2.Divide(const aLeft:TVector2;const aRight:TFloat):TVector2;
begin
 result.x:=aLeft.x/aRight;
 result.y:=aLeft.y/aRight;
end;

class operator TpvCSGBSPTriangleMesh.TVector2.Negative(const aVector:TVector2):TVector2;
begin
 result.x:=-aVector.x;
 result.y:=-aVector.y;
end;

function TpvCSGBSPTriangleMesh.TVector2.Lerp(const aWith:TVector2;const aTime:TFloat):TVector2;
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

function TpvCSGBSPTriangleMesh.TVector2.ToVector:TpvVector2;
begin
 result.x:=x;
 result.y:=y;
end;

{ TpvCSGBSP.TVector3 }

constructor TpvCSGBSPTriangleMesh.TVector3.Create(const aFrom:TpvVector3);
begin
 x:=aFrom.x;
 y:=aFrom.y;
 z:=aFrom.z;
end;

class operator TpvCSGBSPTriangleMesh.TVector3.Equal(const aLeft,aRight:TVector3):boolean;
begin
 result:=SameValue(aLeft.x,aRight.x) and
         SameValue(aLeft.y,aRight.y) and
         SameValue(aLeft.z,aRight.z);
end;

class operator TpvCSGBSPTriangleMesh.TVector3.NotEqual(const aLeft,aRight:TVector3):boolean;
begin
 result:=not (SameValue(aLeft.x,aRight.x) and
              SameValue(aLeft.y,aRight.y) and
              SameValue(aLeft.z,aRight.z));
end;

class operator TpvCSGBSPTriangleMesh.TVector3.Add(const aLeft,aRight:TVector3):TVector3;
begin
 result.x:=aLeft.x+aRight.x;
 result.y:=aLeft.y+aRight.y;
 result.z:=aLeft.z+aRight.z;
end;

class operator TpvCSGBSPTriangleMesh.TVector3.Subtract(const aLeft,aRight:TVector3):TVector3;
begin
 result.x:=aLeft.x-aRight.x;
 result.y:=aLeft.y-aRight.y;
 result.z:=aLeft.z-aRight.z;
end;

class operator TpvCSGBSPTriangleMesh.TVector3.Multiply(const aLeft:TVector3;const aRight:TFloat):TVector3;
begin
 result.x:=aLeft.x*aRight;
 result.y:=aLeft.y*aRight;
 result.z:=aLeft.z*aRight;
end;

class operator TpvCSGBSPTriangleMesh.TVector3.Divide(const aLeft:TVector3;const aRight:TFloat):TVector3;
begin
 result.x:=aLeft.x/aRight;
 result.y:=aLeft.y/aRight;
 result.z:=aLeft.z/aRight;
end;

class operator TpvCSGBSPTriangleMesh.TVector3.Negative(const aVector:TVector3):TVector3;
begin
 result.x:=-aVector.x;
 result.y:=-aVector.y;
 result.z:=-aVector.z;
end;

function TpvCSGBSPTriangleMesh.TVector3.Cross(const aWith:TVector3):TVector3;
begin
 result.x:=(y*aWith.z)-(z*aWith.y);
 result.y:=(z*aWith.x)-(x*aWith.z);
 result.z:=(x*aWith.y)-(y*aWith.x);
end;

function TpvCSGBSPTriangleMesh.TVector3.Spacing(const aWith:TVector3):TFloat;
begin
 result:=abs(x-aWith.x)+abs(y-aWith.y)+abs(z-aWith.z);
end;

function TpvCSGBSPTriangleMesh.TVector3.Dot(const aWith:TVector3):TFloat;
begin
 result:=(x*aWith.x)+(y*aWith.y)+(z*aWith.z);
end;

function TpvCSGBSPTriangleMesh.TVector3.Length:TFloat;
begin
 result:=sqrt(sqr(x)+sqr(y)+sqr(z));
end;

function TpvCSGBSPTriangleMesh.TVector3.SquaredLength:TFloat;
begin
 result:=sqr(x)+sqr(y)+sqr(z);
end;

function TpvCSGBSPTriangleMesh.TVector3.Lerp(const aWith:TVector3;const aTime:TFloat):TVector3;
begin
 if aTime<=0.0 then begin
  result:=self;
 end else if aTime>=1.0 then begin
  result:=aWith;
 end else begin
  result:=(self*(1.0-aTime))+(aWith*aTime);
 end;
end;

function TpvCSGBSPTriangleMesh.TVector3.Normalize:TVector3;
begin
 result:=self/Length;
end;

function TpvCSGBSPTriangleMesh.TVector3.ToVector:TpvVector3;
begin
 result.x:=x;
 result.y:=y;
 result.z:=z;
end;

{ TpvCSGBSP.TVector4 }

constructor TpvCSGBSPTriangleMesh.TVector4.Create(const aFrom:TpvVector4);
begin
 x:=aFrom.x;
 y:=aFrom.y;
 z:=aFrom.z;
 w:=aFrom.w;
end;

class operator TpvCSGBSPTriangleMesh.TVector4.Equal(const aLeft,aRight:TVector4):boolean;
begin
 result:=SameValue(aLeft.x,aRight.x) and
         SameValue(aLeft.y,aRight.y) and
         SameValue(aLeft.z,aRight.z) and
         SameValue(aLeft.w,aRight.w);
end;

class operator TpvCSGBSPTriangleMesh.TVector4.NotEqual(const aLeft,aRight:TVector4):boolean;
begin
 result:=not (SameValue(aLeft.x,aRight.x) and
              SameValue(aLeft.y,aRight.y) and
              SameValue(aLeft.z,aRight.z) and
              SameValue(aLeft.w,aRight.w));
end;

class operator TpvCSGBSPTriangleMesh.TVector4.Add(const aLeft,aRight:TVector4):TVector4;
begin
 result.x:=aLeft.x+aRight.x;
 result.y:=aLeft.y+aRight.y;
 result.z:=aLeft.z+aRight.z;
 result.w:=aLeft.w+aRight.w;
end;

class operator TpvCSGBSPTriangleMesh.TVector4.Subtract(const aLeft,aRight:TVector4):TVector4;
begin
 result.x:=aLeft.x-aRight.x;
 result.y:=aLeft.y-aRight.y;
 result.z:=aLeft.z-aRight.z;
 result.w:=aLeft.w-aRight.w;
end;

class operator TpvCSGBSPTriangleMesh.TVector4.Multiply(const aLeft:TVector4;const aRight:TFloat):TVector4;
begin
 result.x:=aLeft.x*aRight;
 result.y:=aLeft.y*aRight;
 result.z:=aLeft.z*aRight;
 result.w:=aLeft.w*aRight;
end;

class operator TpvCSGBSPTriangleMesh.TVector4.Divide(const aLeft:TVector4;const aRight:TFloat):TVector4;
begin
 result.x:=aLeft.x/aRight;
 result.y:=aLeft.y/aRight;
 result.z:=aLeft.z/aRight;
 result.w:=aLeft.w/aRight;
end;

class operator TpvCSGBSPTriangleMesh.TVector4.Negative(const aVector:TVector4):TVector4;
begin
 result.x:=-aVector.x;
 result.y:=-aVector.y;
 result.z:=-aVector.z;
 result.w:=-aVector.w;
end;

function TpvCSGBSPTriangleMesh.TVector4.Lerp(const aWith:TVector4;const aTime:TFloat):TVector4;
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

function TpvCSGBSPTriangleMesh.TVector4.ToVector:TpvVector4;
begin
 result.x:=x;
 result.y:=y;
 result.z:=z;
 result.w:=w;
end;

{ TpvCSGBSP.TVertex }

class operator TpvCSGBSPTriangleMesh.TVertex.Add(const aLeft,aRight:TVertex):TVertex;
begin
 result.Position:=aLeft.Position+aRight.Position;
 result.Normal:=aLeft.Normal+aRight.Normal;
 result.TexCoord:=aLeft.TexCoord+aRight.TexCoord;
 result.Color:=aLeft.Color+aRight.Color;
end;

class operator TpvCSGBSPTriangleMesh.TVertex.Subtract(const aLeft,aRight:TVertex):TVertex;
begin
 result.Position:=aLeft.Position-aRight.Position;
 result.Normal:=aLeft.Normal-aRight.Normal;
 result.TexCoord:=aLeft.TexCoord-aRight.TexCoord;
 result.Color:=aLeft.Color-aRight.Color;
end;

class operator TpvCSGBSPTriangleMesh.TVertex.Multiply(const aLeft:TVertex;const aRight:TFloat):TVertex;
begin
 result.Position:=aLeft.Position*aRight;
 result.Normal:=aLeft.Normal*aRight;
 result.TexCoord:=aLeft.TexCoord*aRight;
 result.Color:=aLeft.Color*aRight;
end;

class operator TpvCSGBSPTriangleMesh.TVertex.Divide(const aLeft:TVertex;const aRight:TFloat):TVertex;
begin
 result.Position:=aLeft.Position/aRight;
 result.Normal:=aLeft.Normal/aRight;
 result.TexCoord:=aLeft.TexCoord/aRight;
 result.Color:=aLeft.Color/aRight;
end;

class operator TpvCSGBSPTriangleMesh.TVertex.Negative(const aLeft:TVertex):TVertex;
begin
 result.Position:=-aLeft.Position;
 result.Normal:=-aLeft.Normal;
 result.TexCoord:=-aLeft.TexCoord;
 result.Color:=-aLeft.Color;
end;

function TpvCSGBSPTriangleMesh.TVertex.Lerp(const aWith:TVertex;const aTime:TFloat):TVertex;
begin
 result.Position:=Position.Lerp(aWith.Position,aTime);
 result.Normal:=Normal.Lerp(aWith.Normal,aTime);
 result.TexCoord:=TexCoord.Lerp(aWith.TexCoord,aTime);
 result.Color:=Color.Lerp(aWith.Color,aTime);
end;

function TpvCSGBSPTriangleMesh.TVertex.Flip:TVertex;
begin
 result.Position:=Position;
 result.Normal:=-Normal;
 result.TexCoord:=TexCoord;
 result.Color:=Color;
end;

function TpvCSGBSPTriangleMesh.TVertex.Normalize:TVertex;
begin
 result.Position:=Position;
 result.Normal:=Normal.Normalize;
 result.TexCoord:=TexCoord;
 result.Color:=Color;
end;

{ TpvCSGBSP.TPlane }

constructor TpvCSGBSPTriangleMesh.TPlane.Create(const aV0,aV1,aV2:TVector3);
begin
 Normal:=((aV1-aV0).Cross(aV2-aV0)).Normalize;
 Distance:=-Normal.Dot(aV0);
end;

constructor TpvCSGBSPTriangleMesh.TPlane.Create(const aNormal:TVector3;const aDistance:TFloat);
begin
 Normal:=aNormal;
 Distance:=aDistance;
end;

class function TpvCSGBSPTriangleMesh.TPlane.CreateEmpty;
begin
 result.Normal.x:=0.0;
 result.Normal.y:=0.0;
 result.Normal.z:=0.0;
 result.Distance:=0.0;
end;

function TpvCSGBSPTriangleMesh.TPlane.DistanceTo(const aWith:TVector3):TFloat;
begin
 result:=Normal.Dot(aWith)+Distance;
end;

function TpvCSGBSPTriangleMesh.TPlane.IntersectWithPointToPoint(const aPointA,aPointB:TVector3;const aIntersectionPoint:PVector3=nil):boolean;
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

function TpvCSGBSPTriangleMesh.TPlane.IntersectWithPointToPoint(const aPointA,aPointB:TVertex;const aIntersectionPoint:PVertex=nil):boolean;
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

function TpvCSGBSPTriangleMesh.TPlane.OK:boolean;
begin
 result:=Normal.Length>0.0;
end;

function TpvCSGBSPTriangleMesh.TPlane.Flip:TPlane;
begin
 result.Normal:=-Normal;
 result.Distance:=-Distance;
end;

procedure TpvCSGBSPTriangleMesh.TPlane.Split(var aVertices:TVertexList;
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

  Sides[0]:=TpvCSGBSPTriangleMesh.EpsilonSign(PlaneDistances[0]);
  Sides[1]:=TpvCSGBSPTriangleMesh.EpsilonSign(PlaneDistances[1]);
  Sides[2]:=TpvCSGBSPTriangleMesh.EpsilonSign(PlaneDistances[2]);

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

{ TpvCSGBSP.TMesh }

constructor TpvCSGBSPTriangleMesh.TMesh.Create;
begin
 inherited Create;
 fVertices.Initialize;
 fIndices.Initialize;
end;

constructor TpvCSGBSPTriangleMesh.TMesh.Create(const aFrom:TMesh);
begin
 Create;
 if assigned(aFrom) then begin
  SetVertices(aFrom.fVertices);
  SetIndices(aFrom.fIndices);
 end;
end;

constructor TpvCSGBSPTriangleMesh.TMesh.Create(const aFrom:TNode);
var Mesh:TMesh;
begin
 Create;
 Mesh:=aFrom.ToMesh;
 try
  fVertices.Assign(Mesh.fVertices);
  fIndices.Assign(Mesh.fIndices);
  Mesh.fVertices.Clear;
  Mesh.fIndices.Clear;
 finally
  FreeAndNil(Mesh);
 end;
end;

constructor TpvCSGBSPTriangleMesh.TMesh.CreateCube(const aCX,aCY,aCZ,aRX,aRY,aRZ:TFloat);
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
 Create;
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
  fIndices.Add([BaseVertexIndex+0,BaseVertexIndex+1,BaseVertexIndex+2,
                BaseVertexIndex+0,BaseVertexIndex+2,BaseVertexIndex+3]);
 end;
 RemoveDuplicateVertices;
end;

constructor TpvCSGBSPTriangleMesh.TMesh.CreateSphere(const aCX,aCY,aCZ,aRadius:TFloat;const aSlices:TpvSizeInt=16;const aStacks:TpvSizeInt=8);
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
  result:=fVertices.Add(Vertex);
 end;
var SliceIndex,StackIndex:TpvSizeInt;
    PolygonIndices:TIndexList;
begin
 Create;
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
  end;
 finally
  PolygonIndices.Finalize;
 end;
 RemoveDuplicateVertices;
end;

constructor TpvCSGBSPTriangleMesh.TMesh.CreateFromCSGOperation(const aLeftMesh:TMesh;
                                                               const aRightMesh:TMesh;
                                                               const aCSGOperation:TCSGOperation);
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
      Append(aVertex2.Flip,aVertex1.Flip,aVertex0.Flip);
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

        Sides[0]:=TpvCSGBSPTriangleMesh.EpsilonSign(PlaneDistances[0]);
        Sides[1]:=TpvCSGBSPTriangleMesh.EpsilonSign(PlaneDistances[1]);
        Sides[2]:=TpvCSGBSPTriangleMesh.EpsilonSign(PlaneDistances[2]);

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
          Append(WorkData^.Vertex2.Flip,WorkData^.Vertex1.Flip,WorkData^.Vertex0.Flip);
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
var LeftNode,RightNode:TNode;
begin
 Create;
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
   end;
  finally
   FreeAndNil(RightNode);
  end;
 finally
  FreeAndNil(LeftNode);
 end;
 RemoveDuplicateVertices;
end;

constructor TpvCSGBSPTriangleMesh.TMesh.CreateUnion(const aLeftMesh,aRightMesh:TMesh);
begin
 CreateFromCSGOperation(aLeftMesh,aRightMesh,TCSGOperation.Union);
end;

constructor TpvCSGBSPTriangleMesh.TMesh.CreateSubtraction(const aLeftMesh,aRightMesh:TMesh);
begin
 CreateFromCSGOperation(aLeftMesh,aRightMesh,TCSGOperation.Subtraction);
end;

constructor TpvCSGBSPTriangleMesh.TMesh.CreateIntersection(const aLeftMesh,aRightMesh:TMesh);
begin
 CreateFromCSGOperation(aLeftMesh,aRightMesh,TCSGOperation.Intersection);
end;

constructor TpvCSGBSPTriangleMesh.TMesh.CreateSymmetricDifference(const aLeftMesh,aRightMesh:TMesh);
var a,b:TMesh;
begin
 // Possible symmertic difference (boolean XOR) implementations:
 // Intersection(Union(A,B),Inverse(Intersection(A,B)))
 // Intersection(Union(A,B),Union(Inverse(A),Inverse(B)))
 // Union(Subtraction(A,B),Subtraction(B,A)) <= used here, because it seems the most robust mnethod in this BSP-based triangle-based CSG implementation!
 // Subtraction(Union(A,B),Intersection(A,B))
 a:=TMesh.CreateSubtraction(aLeftMesh,aRightMesh);
 try
  b:=TMesh.CreateSubtraction(aRightMesh,aLeftMesh);
  try
   CreateUnion(a,b);
  finally
   FreeAndNil(b);
  end;
 finally
  FreeAndNil(a);
 end;
end;

constructor TpvCSGBSPTriangleMesh.TMesh.CreateRobustUnion(const aLeftMesh,aRightMesh:TMesh);
var ma,mb:TMesh;
    na,nb:TNode;
begin
 ma:=TMesh.Create(aLeftMesh);
 try
  na:=ma.ToNode;
  try
   mb:=TMesh.Create(aRightMesh);
   try
    nb:=mb.ToNode;
    try
     na.ClipTo(nb);
     nb.ClipTo(na);
     nb.Invert;
     nb.ClipTo(na);
     nb.Invert;
     na.Merge(nb);
    finally
     FreeAndNil(nb);
    end;
   finally
    FreeAndNil(mb);
   end;
   Create(na);
  finally
   FreeAndNil(na);
  end;
 finally
  FreeAndNil(ma);
 end;
end;

constructor TpvCSGBSPTriangleMesh.TMesh.CreateRobustSubtraction(const aLeftMesh,aRightMesh:TMesh);
var ma,mb:TMesh;
    na,nb:TNode;
begin
 ma:=TMesh.Create(aLeftMesh);
 try
  na:=ma.ToNode;
  try
   mb:=TMesh.Create(aRightMesh);
   try
    nb:=mb.ToNode;
    try
     na.Invert;
     na.ClipTo(nb);
     nb.ClipTo(na);
     nb.Invert;
     nb.ClipTo(na);
     nb.Invert;
     na.Merge(nb);
    finally
     FreeAndNil(nb);
    end;
   finally
    FreeAndNil(mb);
   end;
   na.Invert;
   Create(na);
  finally
   FreeAndNil(na);
  end;
 finally
  FreeAndNil(ma);
 end;
end;

constructor TpvCSGBSPTriangleMesh.TMesh.CreateRobustIntersection(const aLeftMesh,aRightMesh:TMesh);
var ma,mb:TMesh;
    na,nb:TNode;
begin
 ma:=TMesh.Create(aLeftMesh);
 try
  na:=ma.ToNode;
  try
   mb:=TMesh.Create(aRightMesh);
   try
    nb:=mb.ToNode;
    try
     na.Invert;
     nb.ClipTo(na);
     nb.Invert;
     na.ClipTo(nb);
     nb.ClipTo(na);
     na.Merge(nb);
    finally
     FreeAndNil(nb);
    end;
   finally
    FreeAndNil(mb);
   end;
   na.Invert;
   Create(na);
  finally
   FreeAndNil(na);
  end;
 finally
  FreeAndNil(ma);
 end;
end;

constructor TpvCSGBSPTriangleMesh.TMesh.CreateRobustSymmetricDifference(const aLeftMesh,aRightMesh:TMesh);
var a,b:TMesh;
begin
 // Possible symmertic difference (boolean XOR) implementations:
 // Intersection(Union(A,B),Inverse(Intersection(A,B)))
 // Intersection(Union(A,B),Union(Inverse(A),Inverse(B)))
 // Union(Subtraction(A,B),Subtraction(B,A)) <= used here, because it seems the most robust mnethod in this BSP-based triangle-based CSG implementation!
 // Subtraction(Union(A,B),Intersection(A,B))
 a:=TMesh.CreateRobustSubtraction(aLeftMesh,aRightMesh);
 try
  b:=TMesh.CreateRobustSubtraction(aRightMesh,aLeftMesh);
  try
   CreateRobustUnion(a,b);
  finally
   FreeAndNil(b);
  end;
 finally
  FreeAndNil(a);
 end;
end;

destructor TpvCSGBSPTriangleMesh.TMesh.Destroy;
begin
 fVertices.Finalize;
 fIndices.Finalize;
 inherited Destroy;
end;

procedure TpvCSGBSPTriangleMesh.TMesh.SetVertices(const aVertices:TVertexList);
begin
 fVertices.Assign(aVertices);
end;

procedure TpvCSGBSPTriangleMesh.TMesh.SetIndices(const aIndices:TIndexList);
begin
 fIndices.Assign(aIndices);
end;

procedure TpvCSGBSPTriangleMesh.TMesh.Assign(const aFrom:TMesh);
begin
 SetVertices(aFrom.fVertices);
 SetIndices(aFrom.fIndices);
end;

procedure TpvCSGBSPTriangleMesh.TMesh.Invert;
var Index,Count:TpvSizeInt;
begin
 for Index:=0 To fVertices.Count-1 do begin
  fVertices.Items[Index]:=fVertices.Items[Index].Flip;
 end;
 Index:=0;
 Count:=fIndices.Count;
 while (Index+2)<Count do begin
  fIndices.Exchange(Index+0,Index+2);
  inc(Index,3);
 end;
end;

procedure TpvCSGBSPTriangleMesh.TMesh.RemoveDuplicateVertices;
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
var Index,Count,HashIndex,CountHashTableItems:TpvSizeInt;
    Vertex,OtherVertex:TVertex;
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
      Vertex:=fVertices.Items[fIndices.Items[Index]];
      VertexIndex:=-1;
      Hash:=(trunc(Vertex.Position.x*4096)*73856093) xor
            (trunc(Vertex.Position.y*4096)*19349653) xor
            (trunc(Vertex.Position.z*4096)*83492791);
      HashIndex:=HashTable[Hash and HashMask];
      while HashIndex>=0 do begin
       HashTableItem:=@HashTableItems[HashIndex];
       if HashTableItem^.Hash=Hash then begin
        OtherVertex:=NewVertices.Items[HashTableItem^.VertexIndex];
        if SameValue(Vertex.Position.x,OtherVertex.Position.x) and
           SameValue(Vertex.Position.y,OtherVertex.Position.y) and
           SameValue(Vertex.Position.z,OtherVertex.Position.z) and
           SameValue(Vertex.Normal.x,OtherVertex.Normal.x) and
           SameValue(Vertex.Normal.y,OtherVertex.Normal.y) and
           SameValue(Vertex.Normal.z,OtherVertex.Normal.z) and
           SameValue(Vertex.TexCoord.x,OtherVertex.TexCoord.x) and
           SameValue(Vertex.TexCoord.y,OtherVertex.TexCoord.y) and
           SameValue(Vertex.TexCoord.z,OtherVertex.TexCoord.z) and
           SameValue(Vertex.TexCoord.w,OtherVertex.TexCoord.w) and
           SameValue(Vertex.Color.x,OtherVertex.Color.x) and
           SameValue(Vertex.Color.y,OtherVertex.Color.y) and
           SameValue(Vertex.Color.z,OtherVertex.Color.z) and
           SameValue(Vertex.Color.w,OtherVertex.Color.w) then begin
         VertexIndex:=HashTableItem^.VertexIndex;
         break;
        end;
       end;
       HashIndex:=HashTableItem^.Next;
      end;
      if VertexIndex<0 then begin
       VertexIndex:=NewVertices.Add(Vertex);
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

procedure TpvCSGBSPTriangleMesh.TMesh.FixTJunctions;
const Map:array[0..2,0..3] of TpvSizeInt=((3,1,2,2),(0,3,2,0),(0,1,3,1));
var Index,TriangleVertexIndex,VertexIndex,
    CountIndices,CountVertices,Previous:TpvSizeInt;
    TryAgain:boolean;
    Position0,Position1,VertexPosition,
    Direction,DifferenceToVertexPosition:TVector3;
    Time:TFloat;
    Vertex:TVertex;
    VertexIndices:array[0..3] of TIndex;
begin
 RemoveDuplicateVertices;
 try
  repeat
   TryAgain:=false;
   CountIndices:=fIndices.Count;
   CountVertices:=fVertices.Count;
   Index:=0;
   while ((Index+2)<CountIndices) and not TryAgain do begin
    VertexIndices[0]:=fIndices.Items[Index+0];
    VertexIndices[1]:=fIndices.Items[Index+1];
    VertexIndices[2]:=fIndices.Items[Index+2];
    Previous:=2;
    TriangleVertexIndex:=0;
    while (TriangleVertexIndex<3) and not TryAgain do begin
     Position0:=fVertices.Items[VertexIndices[Previous]].Position;
     Position1:=fVertices.Items[VertexIndices[TriangleVertexIndex]].Position;
     VertexIndex:=0;
     while (VertexIndex<CountVertices) and not TryAgain do begin
      VertexPosition:=fVertices.Items[VertexIndex].Position;
      if (VertexIndices[0]<>VertexIndex) and
         (VertexIndices[1]<>VertexIndex) and
         (VertexIndices[2]<>VertexIndex) and
         (fVertices.Items[VertexIndices[0]].Position<>VertexPosition) and
         (fVertices.Items[VertexIndices[1]].Position<>VertexPosition) and
         (fVertices.Items[VertexIndices[2]].Position<>VertexPosition) and
         ((Position0-VertexPosition).Normalize.Dot((VertexPosition-Position1).Normalize)>=TJunctionOneMinusEpsilon) then begin
       Direction:=Position1-Position0;
       DifferenceToVertexPosition:=VertexPosition-Position0;
       Time:=DifferenceToVertexPosition.Dot(Direction)/Direction.Dot(Direction);
       if ((Time>=TJunctionEpsilon) and (Time<=TJunctionOneMinusEpsilon)) and
          (((Direction*Time)-DifferenceToVertexPosition).SquaredLength<TJunctionEpsilon) then begin
        Vertex:=fVertices.Items[fIndices.Items[Index+Previous]].Lerp(fVertices.Items[fIndices.Items[Index+TriangleVertexIndex]],Time);
        Vertex.Position:=VertexPosition;
        VertexIndices[3]:=fVertices.Add(Vertex);
        fIndices.Items[Index+Map[TriangleVertexIndex,3]]:=VertexIndices[3];
        fIndices.Add([VertexIndices[Map[TriangleVertexIndex,0]],
                      VertexIndices[Map[TriangleVertexIndex,1]],
                      VertexIndices[Map[TriangleVertexIndex,2]]]);
        TryAgain:=true;
        break;
       end;
      end;
      inc(VertexIndex);
     end;
     Previous:=TriangleVertexIndex;
     inc(TriangleVertexIndex);
    end;
    inc(Index,3);
   end;
  until not TryAgain;
 finally
  RemoveDuplicateVertices;
 end;
end;

function TpvCSGBSPTriangleMesh.TMesh.ToNode:TNode;
begin
 result:=TNode.Create(self);
 result.Build(fIndices);
end;

{ TpvCSGBSP.TNode }

constructor TpvCSGBSPTriangleMesh.TNode.Create(const aMesh:TMesh);
begin
 inherited Create;
 fMesh:=aMesh;
 fIndices.Initialize;
 fBack:=nil;
 fFront:=nil;
end;

destructor TpvCSGBSPTriangleMesh.TNode.Destroy;
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
 inherited Destroy;
end;

procedure TpvCSGBSPTriangleMesh.TNode.SetIndices(const aIndices:TIndexList);
begin
 fIndices.Assign(aIndices);
end;

procedure TpvCSGBSPTriangleMesh.TNode.Invert;
type TJobStack=TDynamicStack<TNode>;
var JobStack:TJobStack;
    Node,TempNode:TNode;
    Index,Count:TpvSizeInt;
begin
 fMesh.Invert;
 JobStack.Initialize;
 try
  JobStack.Push(self);
  while JobStack.Pop(Node) do begin
   Index:=0;
   Count:=Node.fIndices.Count;
   while (Index+2)<Count do begin
    Node.fIndices.Exchange(Index+0,Index+2);
    inc(Index,3);
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

procedure TpvCSGBSPTriangleMesh.TNode.EvaluateSplitPlane(const aPlane:TPlane;
                                                         out aCountTriangleSplits:TpvSizeInt;
                                                         out aCountBackTriangles:TpvSizeInt;
                                                         out aCountFrontTriangles:TpvSizeInt);
const TriangleSplitMask=(0 shl 0) or (1 shl 2) or (1 shl 2) or (1 shl 3) or (1 shl 4) or (1 shl 5) or (1 shl 6) or (0 shl 7);
      BackTriangleMask=(1 shl 0) or (2 shl 2) or (2 shl 4) or (1 shl (3 shl 1)) or (2 shl (4 shl 1)) or (1 shl (5 shl 1)) or (1 shl (6 shl 1)) or (0 shl (7 shl 1));
      FrontTriangleMask=(0 shl 0) or (1 shl 2) or (1 shl 4) or (2 shl (3 shl 1)) or (1 shl (4 shl 1)) or (2 shl (5 shl 1)) or (2 shl (6 shl 1)) or (1 shl (7 shl 1));
var Index,Count,Code:TpvSizeInt;
    Vertices:TVertexList;
begin
 aCountTriangleSplits:=0;
 aCountBackTriangles:=0;
 aCountFrontTriangles:=0;
 Count:=fIndices.Count;
 if Count>2 then begin
  Vertices:=fMesh.fVertices;
  Index:=0;
  while (Index+2)<Count do begin
   Code:=((ord(aPlane.DistanceTo(Vertices.Items[fIndices.Items[Index+0]].Position)>0.0) and 1) shl 2) or
         ((ord(aPlane.DistanceTo(Vertices.Items[fIndices.Items[Index+1]].Position)>0.0) and 1) shl 1) or
         ((ord(aPlane.DistanceTo(Vertices.Items[fIndices.Items[Index+2]].Position)>0.0) and 1) shl 0);
   inc(aCountTriangleSplits,(TriangleSplitMask shr Code) and 1);
   inc(aCountBackTriangles,(BackTriangleMask shr (Code shl 1)) and 3);
   inc(aCountFrontTriangles,(FrontTriangleMask shr (Code shl 1)) and 3);
   inc(Index,3);
  end;
 end;
end;

procedure TpvCSGBSPTriangleMesh.TNode.FindSplitPlane(const aIndices:TIndexList;
                                                     out aPlane:TPlane;
                                                     out aTriangleStartVertexIndex:TpvSizeInt;
                                                     const aSearchBestFactor:TpvSizeInt=1;
                                                     const aTriangleSplitCost:TFloat=1.0;
                                                     const aTriangleImbalanceCost:TFloat=0.25);
var Index,Count,TriangleCount,VertexBaseIndex,
    CountTriangleSplits,CountBackTriangles,CountFrontTriangles:TpvSizeInt;
    Plane:TPlane;
    Score,BestScore:TFloat;
    Vertices:TVertexList;
begin
 if aSearchBestFactor<=0 then begin
  if aIndices.Count>2 then begin
   aPlane:=TPlane.Create(fMesh.fVertices.Items[aIndices.Items[0]].Position,
                         fMesh.fVertices.Items[aIndices.Items[1]].Position,
                         fMesh.fVertices.Items[aIndices.Items[2]].Position);
   aTriangleStartVertexIndex:=0;
  end else begin
   aPlane:=TPlane.CreateEmpty;
   aTriangleStartVertexIndex:=-3;
  end;
 end else begin
  aPlane:=TPlane.CreateEmpty;
  aTriangleStartVertexIndex:=0;
  Count:=aIndices.Count;
  TriangleCount:=Count div 3;
  Vertices:=fMesh.fVertices;
  BestScore:=Infinity;
  for Index:=0 to ((TriangleCount+(aSearchBestFactor-1)) div aSearchBestFactor)-1 do begin
   if aSearchBestFactor=1 then begin
    VertexBaseIndex:=(Index mod TriangleCount)*3;
   end else begin
    VertexBaseIndex:=Random(TriangleCount)*3;
   end;
   Plane:=TPlane.Create(Vertices.Items[aIndices.Items[VertexBaseIndex+0]].Position,
                        Vertices.Items[aIndices.Items[VertexBaseIndex+1]].Position,
                        Vertices.Items[aIndices.Items[VertexBaseIndex+2]].Position);
   EvaluateSplitPlane(Plane,CountTriangleSplits,CountBackTriangles,CountFrontTriangles);
   Score:=(CountTriangleSplits*aTriangleSplitCost)+
          (abs(CountBackTriangles-CountFrontTriangles)*aTriangleImbalanceCost);
   if (Index=0) or (BestScore>Score) then begin
    BestScore:=Score;
    aPlane:=Plane;
    aTriangleStartVertexIndex:=VertexBaseIndex;
   end;
  end;
 end;
end;

procedure TpvCSGBSPTriangleMesh.TNode.Build(const aIndices:TIndexList;const aDoFree:boolean=false);
type TJobStackItem=record
      Node:TNode;
      Indices:TIndexList;
     end;
     TJobStack=TDynamicStack<TJobStackItem>;
var JobStack:TJobStack;
    JobStackItem,NewJobStackItem,FrontJobStackItem,BackJobStackItem:TJobStackItem;
    Index,VertexIndex,CountVertexIndices:TpvSizeInt;
begin
 JobStack.Initialize;
 try
  NewJobStackItem.Node:=self;
  NewJobStackItem.Indices:=aIndices;
  JobStack.Push(NewJobStackItem);
  while JobStack.Pop(JobStackItem) do begin
   try
    CountVertexIndices:=JobStackItem.Indices.Count;
    if CountVertexIndices>2 then begin
     FrontJobStackItem.Indices.Initialize;
     BackJobStackItem.Indices.Initialize;
     if not JobStackItem.Node.fPlane.OK then begin
      FindSplitPlane(JobStackItem.Indices,
                     JobStackItem.Node.fPlane,
                     VertexIndex);
     end;
     JobStackItem.Node.fPlane.Split(fMesh.fVertices,
                                    JobStackItem.Indices,
                                    @JobStackItem.Node.fIndices,
                                    @JobStackItem.Node.fIndices,
                                    @BackJobStackItem.Indices,
                                    @FrontJobStackItem.Indices);
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

function TpvCSGBSPTriangleMesh.TNode.ClipTriangles(var aVertices:TVertexList;const aIndices:TIndexList):TIndexList;
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
        JobStackItem.Node.fPlane.Split(aVertices,
                                       JobStackItem.Indices,
                                       BackIndices,
                                       @FrontJobStackItem.Indices,
                                       BackIndices,
                                       @FrontJobStackItem.Indices);
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

procedure TpvCSGBSPTriangleMesh.TNode.ClipTo(const aNode:TNode);
type TJobStack=TDynamicStack<TNode>;
var JobStack:TJobStack;
    Node:TNode;
begin
 JobStack.Initialize;
 try
  JobStack.Push(self);
  while JobStack.Pop(Node) do begin
   Node.SetIndices(aNode.ClipTriangles(Node.fMesh.fVertices,Node.fIndices));
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

procedure TpvCSGBSPTriangleMesh.TNode.Merge(const aNode:TNode);
var Index,Offset:TpvSizeInt;
    OtherMesh:TMesh;
begin
 Offset:=fMesh.fVertices.Count;
 OtherMesh:=aNode.ToMesh;
 try
  fMesh.fVertices.Add(OtherMesh.fVertices);
  for Index:=0 to OtherMesh.fIndices.Count-1 do begin
   OtherMesh.fIndices.Items[Index]:=OtherMesh.fIndices.Items[Index]+Offset;
  end;
  Build(OtherMesh.fIndices);
 finally
  FreeAndNil(OtherMesh);
 end;
end;

function TpvCSGBSPTriangleMesh.TNode.ToMesh:TMesh;
type TJobStack=TDynamicStack<TNode>;
var JobStack:TJobStack;
    Node:TNode;
begin
 result:=TMesh.Create;
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
  result.RemoveDuplicateVertices;
 end;
end;

end.
