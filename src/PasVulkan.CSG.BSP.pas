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
     Generics.Collections,
     PasVulkan.Types,PasVulkan.Math;

type TpvCSGBSPArray<T>=record
      public
       Items:array of T;
       Count:TpvSizeInt;
       procedure Initialize;
       procedure Finalize;
       procedure Finish;
       procedure Assign(const aFrom:TpvCSGBSPArray<T>);
       procedure Add(const aItem:T);
       procedure Append(const aFrom:TpvCSGBSPArray<T>);
     end;

     PpvCSGBSPVector2=^TpvCSGBSPVector2;
     TpvCSGBSPVector2=record
      public
       x,y:TpvDouble;
       constructor Create(const aFrom:TpvVector2);
       function Lerp(const aWith:TpvCSGBSPVector2;const aTime:TpvDouble):TpvCSGBSPVector2;
       function ToVector:TpvVector2;
     end;

     PpvCSGBSPVector3=^TpvCSGBSPVector3;
     TpvCSGBSPVector3=record
      public
       x,y,z:TpvDouble;
       constructor Create(const aFrom:TpvVector3);
       class operator Add(const aLeft,aRight:TpvCSGBSPVector3):TpvCSGBSPVector3;
       class operator Subtract(const aLeft,aRight:TpvCSGBSPVector3):TpvCSGBSPVector3;
       class operator Multiply(const aLeft:TpvCSGBSPVector3;const aRight:TpvDouble):TpvCSGBSPVector3;
       class operator Divide(const aLeft:TpvCSGBSPVector3;const aRight:TpvDouble):TpvCSGBSPVector3;
       class operator Negative(const aVector:TpvCSGBSPVector3):TpvCSGBSPVector3;
       function Cross(const aWith:TpvCSGBSPVector3):TpvCSGBSPVector3;
       function Dot(const aWith:TpvCSGBSPVector3):TpvDouble;
       function Length:TpvDouble;
       function Lerp(const aWith:TpvCSGBSPVector3;const aTime:TpvDouble):TpvCSGBSPVector3;
       function Normalize:TpvCSGBSPVector3;
       function ToVector:TpvVector3;
     end;

     PpvCSGBSPVector4=^TpvCSGBSPVector4;
     TpvCSGBSPVector4=record
      public
       x,y,z,w:TpvDouble;
       constructor Create(const aFrom:TpvVector4);
       function Lerp(const aWith:TpvCSGBSPVector4;const aTime:TpvDouble):TpvCSGBSPVector4;
       function ToVector:TpvVector4;
     end;

     PpvCSGBSPVertex=^TpvCSGBSPVertex;
     TpvCSGBSPVertex=record
      public
       Position:TpvCSGBSPVector3;
       Normal:TpvCSGBSPVector3;
       TexCoord:TpvCSGBSPVector2;
       Color:TpvCSGBSPVector4;
       procedure Flip;
       function Interpolate(const aWith:TpvCSGBSPVertex;const aTime:TpvDouble):TpvCSGBSPVertex;
     end;

     TpvCSGBSPVertices=TpvCSGBSPArray<TpvCSGBSPVertex>;

     TpvCSGBSPPlane=record
      public
       Normal:TpvCSGBSPVector3;
       Distance:TpvDouble;
       constructor Create(const aV0,aV1,aV2:TpvCSGBSPVector3); overload;
       constructor Create(const aNormal:TpvCSGBSPVector3;const aDistance:TpvDouble); overload;
       class function CreateEmpty:TpvCSGBSPPlane; static;
       function OK:boolean;
       procedure Flip;
     end;

     PpvCSGBSPPolygon=^TpvCSGBSPPolygon;
     TpvCSGBSPPolygon=record
      public
       Vertices:TpvCSGBSPVertices;
       Plane:TpvCSGBSPPlane;
       procedure Initialize;
       procedure Finalize;
       constructor CreateFromVertices(const aVertices:TpvCSGBSPVertices);
       procedure CalculateProperties;
       procedure Flip;
       function Clone:TpvCSGBSPPolygon;
     end;

     TpvCSGBSPPolygons=TpvCSGBSPArray<TpvCSGBSPPolygon>;

     TpvCSGBSPPolygonsHelper=record helper for TpvCSGBSPPolygons
      public
       class function CreateCube(const aCX,aCY,aCZ,aRX,aRY,aRZ:TpvDouble):TpvCSGBSPPolygons; static;
       class function CreateSphere(const aCX,aCY,aCZ,aRadius:TpvDouble;const aSlices:TpvSizeInt=16;const aStacks:TpvSizeInt=8):TpvCSGBSPPolygons; static;
       class function CreateSubtraction(const aLeft,aRight:TpvCSGBSPPolygons):TpvCSGBSPPolygons; static;
       class function CreateUnion(const aLeft,aRight:TpvCSGBSPPolygons):TpvCSGBSPPolygons; static;
       class function CreateIntersection(const aLeft,aRight:TpvCSGBSPPolygons):TpvCSGBSPPolygons; static;
       class function CreateSymmetricDifference(const aLeft,aRight:TpvCSGBSPPolygons):TpvCSGBSPPolygons; static;
       function Subtraction(const aWith:TpvCSGBSPPolygons):TpvCSGBSPPolygons;
       function Union(const aWith:TpvCSGBSPPolygons):TpvCSGBSPPolygons;
       function Intersection(const aWith:TpvCSGBSPPolygons):TpvCSGBSPPolygons;
       function SymmetricDifference(const aWith:TpvCSGBSPPolygons):TpvCSGBSPPolygons;
       function ToTrianglePolygons:TpvCSGBSPPolygons;
     end;

     TpvCSGBSPPlaneHelper=record helper for TpvCSGBSPPlane
      public
       procedure SplitPolygon(const aPolygon:TpvCSGBSPPolygon;var aCoplanarFrontList,aCoplanarBackList,aFrontList,aBackList:TpvCSGBSPPolygons);
     end;

     TpvCSGBSPNode=class
      private
       fPolygons:TpvCSGBSPPolygons;
       fPlane:TpvCSGBSPPlane;
       fFrontNode:TpvCSGBSPNode;
       fBackNode:TpvCSGBSPNode;
      public
       constructor Create; reintroduce; overload;
       constructor Create(const aPolygons:TpvCSGBSPPolygons;const aDoFree:boolean=false); reintroduce; overload;
       constructor CreateSubtraction(const aNodeA,aNodeB:TpvCSGBSPNode);
       constructor CreateUnion(const aNodeA,aNodeB:TpvCSGBSPNode);
       constructor CreateIntersection(const aNodeA,aNodeB:TpvCSGBSPNode);
       constructor CreateSymmetricDifference(const aNodeA,aNodeB:TpvCSGBSPNode);
       destructor Destroy; override;
       procedure Build(const aPolygons:TpvCSGBSPPolygons;const aDoFree:boolean=false);
       function ClipPolygons(const aPolygons:TpvCSGBSPPolygons):TpvCSGBSPPolygons;
       procedure ClipTo(const aNode:TpvCSGBSPNode);
       function AllPolygons:TpvCSGBSPPolygons;
       function Clone:TpvCSGBSPNode;
       function Invert:TpvCSGBSPNode;
     end;

implementation

const EPSILON=1e-5;

procedure TpvCSGBSPArray<T>.Initialize;
begin
 Items:=nil;
 Count:=0;
end;

procedure TpvCSGBSPArray<T>.Finalize;
begin
 Items:=nil;
 Count:=0;
end;

procedure TpvCSGBSPArray<T>.Finish;
begin
 SetLength(Items,Count);
end;

procedure TpvCSGBSPArray<T>.Assign(const aFrom:TpvCSGBSPArray<T>);
begin
 Items:=aFrom.Items;
 Count:=aFrom.Count;
end;

procedure TpvCSGBSPArray<T>.Add(const aItem:T);
begin
 if length(Items)<(Count+1) then begin
  SetLength(Items,(Count+1)+((Count+1) div 2));
 end;
 Items[Count]:=aItem;
 inc(Count);
end;

procedure TpvCSGBSPArray<T>.Append(const aFrom:TpvCSGBSPArray<T>);
var Index:TpvSizeInt;
begin
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

constructor TpvCSGBSPVector2.Create(const aFrom:TpvVector2);
begin
 x:=aFrom.x;
 y:=aFrom.y;
end;

function TpvCSGBSPVector2.Lerp(const aWith:TpvCSGBSPVector2;const aTime:TpvDouble):TpvCSGBSPVector2;
var InverseTime:TpvDouble;
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

function TpvCSGBSPVector2.ToVector:TpvVector2;
begin
 result.x:=x;
 result.y:=y;
end;

constructor TpvCSGBSPVector3.Create(const aFrom:TpvVector3);
begin
 x:=aFrom.x;
 y:=aFrom.y;
 z:=aFrom.z;
end;

class operator TpvCSGBSPVector3.Add(const aLeft,aRight:TpvCSGBSPVector3):TpvCSGBSPVector3;
begin
 result.x:=aLeft.x+aRight.x;
 result.y:=aLeft.y+aRight.y;
 result.z:=aLeft.z+aRight.z;
end;

class operator TpvCSGBSPVector3.Subtract(const aLeft,aRight:TpvCSGBSPVector3):TpvCSGBSPVector3;
begin
 result.x:=aLeft.x-aRight.x;
 result.y:=aLeft.y-aRight.y;
 result.z:=aLeft.z-aRight.z;
end;

class operator TpvCSGBSPVector3.Multiply(const aLeft:TpvCSGBSPVector3;const aRight:TpvDouble):TpvCSGBSPVector3;
begin
 result.x:=aLeft.x*aRight;
 result.y:=aLeft.y*aRight;
 result.z:=aLeft.z*aRight;
end;

class operator TpvCSGBSPVector3.Divide(const aLeft:TpvCSGBSPVector3;const aRight:TpvDouble):TpvCSGBSPVector3;
begin
 result.x:=aLeft.x/aRight;
 result.y:=aLeft.y/aRight;
 result.z:=aLeft.z/aRight;
end;

class operator TpvCSGBSPVector3.Negative(const aVector:TpvCSGBSPVector3):TpvCSGBSPVector3;
begin
 result.x:=-aVector.x;
 result.y:=-aVector.y;
 result.z:=-aVector.z;
end;

function TpvCSGBSPVector3.Cross(const aWith:TpvCSGBSPVector3):TpvCSGBSPVector3;
begin
 result.x:=(y*aWith.z)-(z*aWith.y);
 result.y:=(z*aWith.x)-(x*aWith.z);
 result.z:=(x*aWith.y)-(y*aWith.x);
end;

function TpvCSGBSPVector3.Dot(const aWith:TpvCSGBSPVector3):TpvDouble;
begin
 result:=(x*aWith.x)+(y*aWith.y)+(z*aWith.z);
end;

function TpvCSGBSPVector3.Length:TpvDouble;
begin
 result:=sqrt(sqr(x)+sqr(y)+sqr(z));
end;

function TpvCSGBSPVector3.Lerp(const aWith:TpvCSGBSPVector3;const aTime:TpvDouble):TpvCSGBSPVector3;
begin
 if aTime<=0.0 then begin
  result:=self;
 end else if aTime>=1.0 then begin
  result:=aWith;
 end else begin
  result:=(self*(1.0-aTime))+(aWith*aTime);
 end;
end;

function TpvCSGBSPVector3.Normalize:TpvCSGBSPVector3;
begin
 result:=self/Length;
end;

function TpvCSGBSPVector3.ToVector:TpvVector3;
begin
 result.x:=x;
 result.y:=y;
 result.z:=z;
end;

constructor TpvCSGBSPVector4.Create(const aFrom:TpvVector4);
begin
 x:=aFrom.x;
 y:=aFrom.y;
 z:=aFrom.z;
 w:=aFrom.w;
end;

function TpvCSGBSPVector4.Lerp(const aWith:TpvCSGBSPVector4;const aTime:TpvDouble):TpvCSGBSPVector4;
var InverseTime:TpvDouble;
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

function TpvCSGBSPVector4.ToVector:TpvVector4;
begin
 result.x:=x;
 result.y:=y;
 result.z:=z;
 result.w:=w;
end;

procedure TpvCSGBSPVertex.Flip;
begin
 Normal:=-Normal;
end;

function TpvCSGBSPVertex.Interpolate(const aWith:TpvCSGBSPVertex;const aTime:TpvDouble):TpvCSGBSPVertex;
begin
 result.Position:=Position.Lerp(aWith.Position,aTime);
 result.Normal:=Normal.Lerp(aWith.Normal,aTime);
 result.TexCoord:=TexCoord.Lerp(aWith.TexCoord,aTime);
 result.Color:=Color.Lerp(aWith.Color,aTime);
end;

constructor TpvCSGBSPPlane.Create(const aV0,aV1,aV2:TpvCSGBSPVector3);
begin
 Normal:=((aV1-aV0).Cross(aV2-aV0)).Normalize;
 Distance:=Normal.Dot(aV0);
end;

constructor TpvCSGBSPPlane.Create(const aNormal:TpvCSGBSPVector3;const aDistance:TpvDouble);
begin
 Normal:=aNormal;
 Distance:=aDistance;
end;

class function TpvCSGBSPPlane.CreateEmpty;
begin
 result.Normal.x:=0.0;
 result.Normal.y:=0.0;
 result.Normal.z:=0.0;
 result.Distance:=0.0;
end;

function TpvCSGBSPPlane.OK:boolean;
begin
 result:=Normal.Length>0.0;
end;

procedure TpvCSGBSPPlane.Flip;
begin
 Normal:=-Normal;
 Distance:=-Distance;
end;

procedure TpvCSGBSPPlaneHelper.SplitPolygon(const aPolygon:TpvCSGBSPPolygon;var aCoplanarFrontList,aCoplanarBackList,aFrontList,aBackList:TpvCSGBSPPolygons);
const COPLANAR=0;
      FRONT=1;
      BACK=2;
      SPANNING=3;
var IndexA,IndexB,PolygonType,VectorType,VectorTypeA,VectorTypeB:TpvSizeInt;
    VectorTypes:array of TpvSizeInt;
    Time,VectorDistance:TpvDouble;
    FrontVertices,BackVertices:TpvCSGBSPVertices;
    Vertex:TpvCSGBSPVertex;
    VertexA,VertexB:PpvCSGBSPVertex;
begin
 VectorTypes:=nil;
 try
  PolygonType:=0;
  SetLength(VectorTypes,aPolygon.Vertices.Count);
  for IndexA:=0 to aPolygon.Vertices.Count-1 do begin
   VertexA:=@aPolygon.Vertices.Items[IndexA];
   VectorDistance:=Normal.Dot(VertexA^.Position)-Distance;
   if VectorDistance<-EPSILON then begin
    VectorType:=BACK;
   end else if VectorDistance>EPSILON then begin
    VectorType:=FRONT;
   end else begin
    VectorType:=COPLANAR;
   end;
   PolygonType:=PolygonType or VectorType;
   VectorTypes[IndexA]:=VectorType;
  end;
  case PolygonType of
   COPLANAR:begin
    if Normal.Dot(aPolygon.Plane.Normal)>0.0 then begin
     aCoplanarFrontList.Add(aPolygon);
    end else begin
     aCoplanarBackList.Add(aPolygon);
    end;
   end;
   FRONT:begin
    aFrontList.Add(aPolygon);
   end;
   BACK:begin
    aBackList.Add(aPolygon);
   end;
   else {SPANNING:}begin
    FrontVertices.Initialize;
    BackVertices.Initialize;
    for IndexA:=0 to aPolygon.Vertices.Count-1 do begin
     IndexB:=IndexA+1;
     if IndexB>=aPolygon.Vertices.Count then begin
      IndexB:=0;
     end;
     VertexA:=@aPolygon.Vertices.Items[IndexA];
     VertexB:=@aPolygon.Vertices.Items[IndexB];
     VectorTypeA:=VectorTypes[IndexA];
     VectorTypeB:=VectorTypes[IndexB];
     if VectorTypeA<>BACK then begin
      FrontVertices.Add(VertexA^);
     end;
     if VectorTypeA<>FRONT then begin
      BackVertices.Add(VertexA^);
     end;
     if (VectorTypeA or VectorTypeB)=SPANNING then begin
      Time:=(Distance-Normal.Dot(VertexA^.Position))/Normal.Dot(VertexB^.Position-VertexA^.Position);
      Vertex:=VertexA^.Interpolate(VertexB^,Time);
      FrontVertices.Add(Vertex);
      BackVertices.Add(Vertex);
     end;
    end;
    if FrontVertices.Count>=3 then begin
     aFrontList.Add(TpvCSGBSPPolygon.CreateFromVertices(FrontVertices));
    end;
    if BackVertices.Count>=3 then begin
     aBackList.Add(TpvCSGBSPPolygon.CreateFromVertices(BackVertices));
    end;
    BackVertices.Finalize;
    FrontVertices.Finalize;
   end;
  end;
 finally
  VectorTypes:=nil;
 end;
end;

procedure TpvCSGBSPPolygon.Initialize;
begin
 Vertices.Initialize;
 Plane:=TpvCSGBSPPlane.CreateEmpty;
end;

procedure TpvCSGBSPPolygon.Finalize;
begin
 Vertices.Finalize;
end;

constructor TpvCSGBSPPolygon.CreateFromVertices(const aVertices:TpvCSGBSPVertices);
begin
 Vertices.Assign(aVertices);
 CalculateProperties;
end;

procedure TpvCSGBSPPolygon.CalculateProperties;
begin
 if Vertices.Count>2 then begin
  Plane:=TpvCSGBSPPlane.Create(Vertices.Items[0].Position,Vertices.Items[1].Position,Vertices.Items[2].Position);
 end else begin
  Plane:=TpvCSGBSPPlane.CreateEmpty;
 end;
end;

procedure TpvCSGBSPPolygon.Flip;
var Index,OtherIndex:TpvSizeInt;
    Vertex:TpvCSGBSPVertex;
begin
 for Index:=0 to (Vertices.Count shr 1)-1 do begin
  OtherIndex:=Vertices.Count-(Index+1);
  if Index<>OtherIndex then begin
   Vertex:=Vertices.Items[Index];
   Vertices.Items[Index]:=Vertices.Items[OtherIndex];
   Vertices.Items[OtherIndex]:=Vertex;
  end;
 end;
 for Index:=0 to Vertices.Count-1 do begin
  Vertices.Items[Index].Flip;
 end;
 Plane.Flip;
end;

function TpvCSGBSPPolygon.Clone:TpvCSGBSPPolygon;
var Vertex:TpvCSGBSPVertex;
begin
 result.Initialize;
 result.Vertices.Assign(Vertices);
 result.Plane:=Plane;
end;

class function TpvCSGBSPPolygonsHelper.CreateCube(const aCX,aCY,aCZ,aRX,aRY,aRZ:TpvDouble):TpvCSGBSPPolygons;
const SideVertexIndices:array[0..5,0..3] of TpvUInt8=
       (
        (0,4,6,2), // Left
        (1,3,7,5), // Right
        (0,1,5,4), // Bottom
        (2,6,7,3), // Top
        (0,2,3,1), // Back
        (4,5,7,6)  // Front
       );
      SideNormals:array[0..5] of TpvCSGBSPVector3=
       (
        (x:-1.0;y:0.0;z:0.0),
        (x:1.0;y:0.0;z:0.0),
        (x:0.0;y:-1.0;z:0.0),
        (x:0.0;y:1.0;z:0.0),
        (x:0.0;y:0.0;z:-1.0),
        (x:0.0;y:0.0;z:1.0)
       );
var SideIndex,SideVertexIndex,VertexIndex:TpvSizeInt;
    Vertices:TpvCSGBSPVertices;
    Vertex:TpvCSGBSPVertex;
begin
 result.Initialize;
 for SideIndex:=0 to 5 do begin
  Vertices.Initialize;
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
   Vertices.Add(Vertex);
  end;
  result.Add(TpvCSGBSPPolygon.CreateFromVertices(Vertices));
 end;
end;

class function TpvCSGBSPPolygonsHelper.CreateSphere(const aCX,aCY,aCZ,aRadius:TpvDouble;const aSlices:TpvSizeInt=16;const aStacks:TpvSizeInt=8):TpvCSGBSPPolygons;
var Vertices:TpvCSGBSPVertices;
 procedure AddVertex(const aTheta,aPhi:TpvDouble);
 var Theta,Phi,dx,dy,dz:TpvDouble;
     Vertex:TpvCSGBSPVertex;
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
  Vertices.Add(Vertex);
 end;
var SliceIndex,StackIndex:TpvSizeInt;
begin
 result.Initialize;
 for SliceIndex:=0 to aSlices-1 do begin
  for StackIndex:=0 to aStacks-1 do begin
   Vertices.Initialize;
   AddVertex(SliceIndex/aSlices,StackIndex/aStacks);
   if StackIndex>0 then begin
    AddVertex((SliceIndex+1)/aSlices,StackIndex/aStacks);
   end;
   if StackIndex<(aStacks-1) then begin
    AddVertex((SliceIndex+1)/aSlices,(StackIndex+1)/aStacks);
   end;
   AddVertex(SliceIndex/aSlices,(StackIndex+1)/aStacks);
   result.Add(TpvCSGBSPPolygon.CreateFromVertices(Vertices));
  end;
 end;
end;

class function TpvCSGBSPPolygonsHelper.CreateSubtraction(const aLeft,aRight:TpvCSGBSPPolygons):TpvCSGBSPPolygons;
begin
 result:=aLeft.Subtraction(aRight);
end;

class function TpvCSGBSPPolygonsHelper.CreateUnion(const aLeft,aRight:TpvCSGBSPPolygons):TpvCSGBSPPolygons;
begin
 result:=aLeft.Union(aRight);
end;

class function TpvCSGBSPPolygonsHelper.CreateIntersection(const aLeft,aRight:TpvCSGBSPPolygons):TpvCSGBSPPolygons;
begin
 result:=aLeft.Intersection(aRight);
end;

class function TpvCSGBSPPolygonsHelper.CreateSymmetricDifference(const aLeft,aRight:TpvCSGBSPPolygons):TpvCSGBSPPolygons;
begin
 result:=aLeft.SymmetricDifference(aRight);
end;

function TpvCSGBSPPolygonsHelper.Subtraction(const aWith:TpvCSGBSPPolygons):TpvCSGBSPPolygons;
var a,b,c:TpvCSGBSPNode;
begin
 c:=nil;
 try
  a:=TpvCSGBSPNode.Create(self);
  try
   b:=TpvCSGBSPNode.Create(aWith);
   try
    c:=TpvCSGBSPNode.CreateSubtraction(a,b);
   finally
    FreeAndNil(b);
   end;
  finally
   FreeAndNil(a);
  end;
  result:=c.AllPolygons;
 finally
  FreeAndNil(c);
 end;
end;

function TpvCSGBSPPolygonsHelper.Union(const aWith:TpvCSGBSPPolygons):TpvCSGBSPPolygons;
var a,b,c:TpvCSGBSPNode;
begin
 c:=nil;
 try
  a:=TpvCSGBSPNode.Create(self);
  try
   b:=TpvCSGBSPNode.Create(aWith);
   try
    c:=TpvCSGBSPNode.CreateUnion(a,b);
   finally
    FreeAndNil(b);
   end;
  finally
   FreeAndNil(a);
  end;
  result:=c.AllPolygons;
 finally
  FreeAndNil(c);
 end;
end;

function TpvCSGBSPPolygonsHelper.Intersection(const aWith:TpvCSGBSPPolygons):TpvCSGBSPPolygons;
var a,b,c:TpvCSGBSPNode;
begin
 c:=nil;
 try
  a:=TpvCSGBSPNode.Create(self);
  try
   b:=TpvCSGBSPNode.Create(aWith);
   try
    c:=TpvCSGBSPNode.CreateIntersection(a,b);
   finally
    FreeAndNil(b);
   end;
  finally
   FreeAndNil(a);
  end;
  result:=c.AllPolygons;
 finally
  FreeAndNil(c);
 end;
end;

function TpvCSGBSPPolygonsHelper.SymmetricDifference(const aWith:TpvCSGBSPPolygons):TpvCSGBSPPolygons;
var a,b,c:TpvCSGBSPNode;
begin
 c:=nil;
 try
  a:=TpvCSGBSPNode.Create(self);
  try
   b:=TpvCSGBSPNode.Create(aWith);
   try
    c:=TpvCSGBSPNode.CreateSymmetricDifference(a,b);
   finally
    FreeAndNil(b);
   end;
  finally
   FreeAndNil(a);
  end;
  result:=c.AllPolygons;
 finally
  FreeAndNil(c);
 end;
end;

function TpvCSGBSPPolygonsHelper.ToTrianglePolygons:TpvCSGBSPPolygons;
var PolygonIndex,VertexIndex:TpvSizeInt;
    InputPolygon:PpvCSGBSPPolygon;
    OutputPolygon:TpvCSGBSPPolygon;
begin
 result.Initialize;
 for PolygonIndex:=0 to Count-1 do begin
  InputPolygon:=@Items[PolygonIndex];
  for VertexIndex:=2 to InputPolygon.Vertices.Count-1 do begin
   OutputPolygon.Initialize;
   OutputPolygon.Vertices.Add(InputPolygon.Vertices.Items[0]);
   OutputPolygon.Vertices.Add(InputPolygon.Vertices.Items[VertexIndex-1]);
   OutputPolygon.Vertices.Add(InputPolygon.Vertices.Items[VertexIndex]);
   OutputPolygon.CalculateProperties;
   result.Add(OutputPolygon);
  end;
 end;
 result.Finish;
end;

constructor TpvCSGBSPNode.Create;
begin
 inherited Create;
 fPolygons.Initialize;
 fPlane:=TpvCSGBSPPlane.CreateEmpty;
 fFrontNode:=nil;
 fBackNode:=nil;
end;

constructor TpvCSGBSPNode.Create(const aPolygons:TpvCSGBSPPolygons;const aDoFree:boolean=false);
begin
 Create;
 Build(aPolygons,aDoFree);
end;

constructor TpvCSGBSPNode.CreateSubtraction(const aNodeA,aNodeB:TpvCSGBSPNode);
var a,b:TpvCSGBSPNode;
begin
 a:=aNodeA.Clone;
 try
  b:=aNodeB.Clone;
  try
   a.Invert;
   a.ClipTo(b);
   b.ClipTo(a);
   b.Invert;
   b.ClipTo(a);
   b.Invert;
   a.Build(b.AllPolygons);
   a.Invert;
   Create(a.AllPolygons);
  finally
   FreeAndNil(b);
  end;
 finally
  FreeAndNil(a);
 end;
end;

constructor TpvCSGBSPNode.CreateUnion(const aNodeA,aNodeB:TpvCSGBSPNode);
var a,b:TpvCSGBSPNode;
begin
 a:=aNodeA.Clone;
 try
  b:=aNodeB.Clone;
  try
   a.ClipTo(b);
   b.ClipTo(a);
   b.Invert;
   b.ClipTo(a);
   b.Invert;
   a.Build(b.AllPolygons);
   Create(a.AllPolygons);
  finally
   FreeAndNil(b);
  end;
 finally
  FreeAndNil(a);
 end;
end;

constructor TpvCSGBSPNode.CreateIntersection(const aNodeA,aNodeB:TpvCSGBSPNode);
var a,b:TpvCSGBSPNode;
begin
 a:=aNodeA.Clone;
 try
  b:=aNodeB.Clone;
  try
   a.Invert;
   b.ClipTo(a);
   b.Invert;
   a.ClipTo(b);
   b.ClipTo(a);
   a.Build(b.AllPolygons);
   a.Invert;
   Create(a.AllPolygons);
  finally
   FreeAndNil(b);
  end;
 finally
  FreeAndNil(a);
 end;
end;

constructor TpvCSGBSPNode.CreateSymmetricDifference(const aNodeA,aNodeB:TpvCSGBSPNode);
var a,b:TpvCSGBSPNode;
begin
 // Possible symmertic difference (boolean XOR) implementations:
 // Intersection(Union(A,B),Inverse(Intersection(A,B))) <= used here, because it seems the most robust mnethod in this BSP-based CSG implementation!
 // Intersection(Union(A,B),Union(Inverse(A),Inverse(B)))
 // Union(Subtraction(A,B),Subtraction(B,A))
 // Subtraction(Union(A,B),Intersection(A,B))
 a:=TpvCSGBSPNode.CreateUnion(aNodeA,aNodeB);
 try
  b:=TpvCSGBSPNode.CreateIntersection(aNodeA,aNodeB);
  try
   CreateIntersection(a,b.Invert);
  finally
   FreeAndNil(b);
  end;
 finally
  FreeAndNil(a);
 end;
end;

destructor TpvCSGBSPNode.Destroy;
begin
 fPolygons.Finalize;
 FreeAndNil(fFrontNode);
 FreeAndNil(fBackNode);
 inherited Destroy;
end;

procedure TpvCSGBSPNode.Build(const aPolygons:TpvCSGBSPPolygons;const aDoFree:boolean=false);
{$ifdef NonRecursive}
type TJobStackItem=record
      Node:TpvCSGBSPNode;
      List:TpvCSGBSPPolygons;
     end;
     TJobStack=TStack<TJobStackItem>;
var JobStack:TJobStack;
    JobStackItem,NewJobStackItem,FrontJobStackItem,BackJobStackItem:TJobStackItem;
    PolygonIndex,SplitPolygonIndex:TpvSizeInt;
begin
 JobStack:=TJobStack.Create;
 try
  NewJobStackItem.Node:=self;
  NewJobStackItem.List:=aPolygons;
  JobStack.Push(NewJobStackItem);
  while JobStack.Count>0 do begin
   JobStackItem:=JobStack.Pop;
   FrontJobStackItem.List.Initialize;
   BackJobStackItem.List.Initialize;
   if JobStackItem.List.Count>0 then begin
    if JobStackItem.Node.fPlane.OK then begin
     SplitPolygonIndex:=-1;
    end else begin
     SplitPolygonIndex:=0;
     JobStackItem.Node.fPlane:=JobStackItem.List.Items[SplitPolygonIndex].Plane;
     JobStackItem.Node.fPolygons.Add(JobStackItem.List.Items[SplitPolygonIndex]);
    end;
    for PolygonIndex:=SplitPolygonIndex+1 to JobStackItem.List.Count-1 do begin
     JobStackItem.Node.fPlane.SplitPolygon(JobStackItem.List.Items[PolygonIndex],
                                           JobStackItem.Node.fPolygons,
                                           JobStackItem.Node.fPolygons,
                                           FrontJobStackItem.List,
                                           BackJobStackItem.List);
    end;
    if BackJobStackItem.List.Count>0 then begin
     if not assigned(JobStackItem.Node.fBackNode) then begin
      JobStackItem.Node.fBackNode:=TpvCSGBSPNode.Create;
     end;
     BackJobStackItem.Node:=JobStackItem.Node.fBackNode;
     JobStack.Push(BackJobStackItem);
    end;
    if FrontJobStackItem.List.Count>0 then begin
     if not assigned(JobStackItem.Node.fFrontNode) then begin
      JobStackItem.Node.fFrontNode:=TpvCSGBSPNode.Create;
     end;
     FrontJobStackItem.Node:=JobStackItem.Node.fFrontNode;
     JobStack.Push(FrontJobStackItem);
    end;
   end;
  end;
 finally
  FreeAndNil(JobStack);
 end;
end;
{$else}
var PolygonIndex,SplitPolygonIndex:TpvSizeInt;
    FrontList,BackList:TpvCSGBSPPolygons;
begin
 FrontList.Initialize;
 BackList.Initialize;
 if aPolygons.Count>0 then begin
  if fPlane.OK then begin
   SplitPolygonIndex:=-1;
  end else begin
   SplitPolygonIndex:=0;
   fPlane:=aPolygons.Items[SplitPolygonIndex].Plane;
   fPolygons.Add(aPolygons.Items[SplitPolygonIndex]);
  end;
  for PolygonIndex:=SplitPolygonIndex+1 to aPolygons.Count-1 do begin
   fPlane.SplitPolygon(aPolygons.Items[PolygonIndex],
                       fPolygons,
                       fPolygons,
                       FrontList,
                       BackList);
  end;
  if FrontList.Count>0 then begin
   if not assigned(fFrontNode) then begin
    fFrontNode:=TpvCSGBSPNode.Create;
   end;
   fFrontNode.Build(FrontList);
  end;
  if BackList.Count>0 then begin
   if not assigned(fBackNode) then begin
    fBackNode:=TpvCSGBSPNode.Create;
   end;
   fBackNode.Build(BackList);
  end;
 end;
end;
{$endif}

function TpvCSGBSPNode.AllPolygons:TpvCSGBSPPolygons;
{$ifdef NonRecursive}
type TJobStackItem=record
      Node:TpvCSGBSPNode;
     end;
     TJobStack=TStack<TJobStackItem>;
var JobStack:TJobStack;
    JobStackItem,NewJobStackItem:TJobStackItem;
begin
 result.Initialize;
 JobStack:=TJobStack.Create;
 try
  NewJobStackItem.Node:=self;
  JobStack.Push(NewJobStackItem);
  while JobStack.Count>0 do begin
   JobStackItem:=JobStack.Pop;
   result.Append(JobStackItem.Node.fPolygons);
   if assigned(JobStackItem.Node.fBackNode) then begin
    NewJobStackItem.Node:=JobStackItem.Node.fBackNode;
    JobStack.Push(NewJobStackItem);
   end;
   if assigned(JobStackItem.Node.fFrontNode) then begin
    NewJobStackItem.Node:=JobStackItem.Node.fFrontNode;
    JobStack.Push(NewJobStackItem);
   end;
  end;
 finally
  FreeAndNil(JobStack);
 end;
end;
{$else}
begin
 result.Assign(fPolygons);
 if assigned(fFrontNode) then begin
  result.Append(fFrontNode.AllPolygons);
 end;
 if assigned(fBackNode) then begin
  result.Append(fBackNode.AllPolygons);
 end;
end;
{$endif}

function TpvCSGBSPNode.Clone:TpvCSGBSPNode;
{$ifdef NonRecursive}
type TJobStackItem=record
      DstNode:TpvCSGBSPNode;
      SrcNode:TpvCSGBSPNode;
     end;
     TJobStack=TStack<TJobStackItem>;
var JobStack:TJobStack;
    JobStackItem,NewJobStackItem:TJobStackItem;
begin
 JobStack:=TJobStack.Create;
 try
  result:=TpvCSGBSPNode.Create;
  NewJobStackItem.DstNode:=result;
  NewJobStackItem.SrcNode:=self;
  JobStack.Push(NewJobStackItem);
  while JobStack.Count>0 do begin
   JobStackItem:=JobStack.Pop;
   JobStackItem.DstNode.fPolygons.Assign(JobStackItem.SrcNode.fPolygons);
   JobStackItem.DstNode.fPlane:=JobStackItem.SrcNode.fPlane;
   if assigned(JobStackItem.SrcNode.fBackNode) then begin
    JobStackItem.DstNode.fBackNode:=TpvCSGBSPNode.Create;
    NewJobStackItem.DstNode:=JobStackItem.DstNode.fBackNode;
    NewJobStackItem.SrcNode:=JobStackItem.SrcNode.fBackNode;
    JobStack.Push(NewJobStackItem);
   end;
   if assigned(JobStackItem.SrcNode.fFrontNode) then begin
    JobStackItem.DstNode.fFrontNode:=TpvCSGBSPNode.Create;
    NewJobStackItem.DstNode:=JobStackItem.DstNode.fFrontNode;
    NewJobStackItem.SrcNode:=JobStackItem.SrcNode.fFrontNode;
    JobStack.Push(NewJobStackItem);
   end;
  end;
 finally
  JobStack.Free;
 end;
end;
{$else}
var Polygon:TpvCSGBSPPolygon;
begin
 result:=TpvCSGBSPNode.Create;
 result.fPolygons.Assign(fPolygons);
 result.fPlane:=fPlane;
 if assigned(fFrontNode) then begin
  result.fFrontNode:=fFrontNode.Clone;
 end;
 if assigned(fBackNode) then begin
  result.fBackNode:=fBackNode.Clone;
 end;
end;
{$endif}

function TpvCSGBSPNode.Invert:TpvCSGBSPNode;
{$ifdef NonRecursive}
type TJobStack=TStack<TpvCSGBSPNode>;
var JobStack:TJobStack;
    Node,Temp:TpvCSGBSPNode;
    PolygonIndex:TpvSizeInt;
begin
 JobStack:=TJobStack.Create;
 try
  JobStack.Push(self);
  while JobStack.Count>0 do begin
   Node:=JobStack.Pop;
   for PolygonIndex:=0 to Node.fPolygons.Count-1 do begin
    Node.fPolygons.Items[PolygonIndex].Flip;
   end;
   Node.fPlane.Flip;
   Temp:=Node.fFrontNode;
   Node.fFrontNode:=Node.fBackNode;
   Node.fBackNode:=Temp;
   if assigned(Node.fBackNode) then begin
    JobStack.Push(Node.fBackNode);
   end;
   if assigned(Node.fFrontNode) then begin
    JobStack.Push(Node.fFrontNode);
   end;
  end;
 finally
  JobStack.Free;
 end;
 result:=self;
end;
{$else}
var PolygonIndex:TpvSizeInt;
    Temp:TpvCSGBSPNode;
begin
 for PolygonIndex:=0 to fPolygons.Count-1 do begin
  fPolygons.Items[PolygonIndex].Flip;
 end;
 fPlane.Flip;
 Temp:=fFrontNode;
 fFrontNode:=fBackNode;
 fBackNode:=Temp;
 if assigned(fFrontNode) then begin
  fFrontNode.Invert;
 end;
 if assigned(fBackNode) then begin
  fBackNode.Invert;
 end;
 result:=self;
end;
{$endif}

function TpvCSGBSPNode.ClipPolygons(const aPolygons:TpvCSGBSPPolygons):TpvCSGBSPPolygons;
{$ifdef NonRecursive}
type TJobStackItem=record
      Node:TpvCSGBSPNode;
      List:TpvCSGBSPPolygons;
     end;
     TJobStack=TStack<TJobStackItem>;
var JobStack:TJobStack;
    JobStackItem,NewJobStackItem,FrontJobStackItem,BackJobStackItem:TJobStackItem;
    PolygonIndex:TpvSizeInt;
begin
 result.Initialize;
 JobStack:=TJobStack.Create;
 try
  NewJobStackItem.Node:=self;
  NewJobStackItem.List:=aPolygons;
  JobStack.Push(NewJobStackItem);
  while JobStack.Count>0 do begin
   JobStackItem:=JobStack.Pop;
   if JobStackItem.Node.fPlane.OK then begin
    FrontJobStackItem.List.Initialize;
    BackJobStackItem.List.Initialize;
    for PolygonIndex:=0 to JobStackItem.List.Count-1 do begin
     JobStackItem.Node.fPlane.SplitPolygon(JobStackItem.List.Items[PolygonIndex],
                                           FrontJobStackItem.List,
                                           BackJobStackItem.List,
                                           FrontJobStackItem.List,
                                           BackJobStackItem.List);
    end;
    if assigned(JobStackItem.Node.fBackNode) then begin
     BackJobStackItem.Node:=JobStackItem.Node.fBackNode;
     JobStack.Push(BackJobStackItem);
    end;
    if assigned(JobStackItem.Node.fFrontNode) then begin
     FrontJobStackItem.Node:=JobStackItem.Node.fFrontNode;
     JobStack.Push(FrontJobStackItem);
    end else begin
     result.Append(FrontJobStackItem.List);
     FrontJobStackItem.List.Finalize;
    end;
   end else begin
    result.Append(JobStackItem.List);
    JobStackItem.List.Finalize;
   end;
  end;
 finally
  FreeAndNil(JobStack);
 end;
end;
{$else}
var FrontList,BackList:TpvCSGBSPPolygons;
    PolygonIndex:TpvSizeInt;
begin
 if fPlane.OK then begin
  result.Initialize;
  FrontList.Initialize;
  BackList.Initialize;
  for PolygonIndex:=0 to aPolygons.Count-1 do begin
   fPlane.SplitPolygon(aPolygons.Items[PolygonIndex],FrontList,BackList,FrontList,BackList);
  end;
  if assigned(fFrontNode) then begin
   FrontList:=fFrontNode.ClipPolygons(FrontList);
  end;
  result.Assign(FrontList);
  if assigned(fBackNode) then begin
   result.Append(fBackNode.ClipPolygons(BackList));
  end;
 end else begin
  result.Assign(aPolygons);
 end;
end;
{$endif}

procedure TpvCSGBSPNode.ClipTo(const aNode:TpvCSGBSPNode);
{$ifdef NonRecursive}
type TJobStack=TStack<TpvCSGBSPNode>;
var JobStack:TJobStack;
    Node:TpvCSGBSPNode;
begin
 JobStack:=TJobStack.Create;
 try
  JobStack.Push(self);
  while JobStack.Count>0 do begin
   Node:=JobStack.Pop;
   Node.fPolygons:=aNode.ClipPolygons(Node.fPolygons);
   if assigned(Node.fBackNode) then begin
    JobStack.Push(Node.fBackNode);
   end;
   if assigned(Node.fFrontNode) then begin
    JobStack.Push(Node.fFrontNode);
   end;
  end;
 finally
  FreeAndNil(JobStack);
 end;
end;
{$else}
begin
 fPolygons:=aNode.ClipPolygons(fPolygons);
 if assigned(fFrontNode) then begin
  fFrontNode.ClipTo(aNode);
 end;
 if assigned(fBackNode) then begin
  fBackNode.ClipTo(aNode);
 end;
end;
{$endif}

end.
