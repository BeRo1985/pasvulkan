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

type PpvCSGBSPClassification=^TpvCSGBSPClassification;
     TpvCSGBSPClassification=
      (
       Coplanar=0,
       Front=1,
       Back=2,
       Spanning=3
      );

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

     TpvCSGBSPVertex=class
      public
       Position:TpvCSGBSPVector3;
       Normal:TpvCSGBSPVector3;
       TexCoord:TpvCSGBSPVector2;
       Color:TpvCSGBSPVector4;
       function Clone:TpvCSGBSPVertex;
       procedure Flip;
       function Interpolate(const aWith:TpvCSGBSPVertex;const aTime:TpvDouble):TpvCSGBSPVertex;
     end;

     TpvCSGBSPVertices=class(TObjectList<TpvCSGBSPVertex>)
     end;

     TpvCSGBSPPolygon=class;

     TpvCSGBSPPolygons=class(TObjectList<TpvCSGBSPPolygon>)
      public
       constructor CreateCube(const aCX,aCY,aCZ,aRX,aRY,aRZ:TpvDouble);
       constructor CreateSphere(const aCX,aCY,aCZ,aRadius:TpvDouble;const aSlices:TpvSizeInt=16;const aStacks:TpvSizeInt=8);
       function ToTrianglePolygons:TpvCSGBSPPolygons;
     end;

     TpvCSGBSPPlane=class
      private
       fNormal:TpvCSGBSPVector3;
       fDistance:TpvDouble;
      public
       constructor Create(const aV0,aV1,aV2:TpvCSGBSPVector3); reintroduce; overload;
       constructor Create(const aNormal:TpvCSGBSPVector3;const aDistance:TpvDouble); reintroduce; overload;
       constructor CreateEmpty;
       function Clone:TpvCSGBSPPlane;
       function OK:boolean;
       procedure Flip;
       procedure SplitPolygon(const aPolygon:TpvCSGBSPPolygon;const aCoplanarFrontList,aCoplanarBackList,aFrontList,aBackList:TpvCSGBSPPolygons);
     end;

     TpvCSGBSPPolygon=class
      private
       fVertices:TpvCSGBSPVertices;
       fPlane:TpvCSGBSPPlane;
      public
       constructor Create; reintroduce;
       constructor CreateFromVertices(const aVertices:TpvCSGBSPVertices); reintroduce;
       destructor Destroy; override;
       procedure CalculateProperties;
       procedure Flip;
       function Clone:TpvCSGBSPPolygon;
      public
       property Plane:TpvCSGBSPPlane read fPlane write fPlane;
      published
       property Vertices:TpvCSGBSPVertices read fVertices;
     end;

     TpvCSGBSPNode=class
      private
       fPolygons:TpvCSGBSPPolygons;
       fPlane:TpvCSGBSPPlane;
       fFrontNode:TpvCSGBSPNode;
       fBackNode:TpvCSGBSPNode;
       function ClipPolygons(const aPolygons:TpvCSGBSPPolygons):TpvCSGBSPPolygons;
       procedure ClipTo(const aNode:TpvCSGBSPNode);
      public
       constructor Create; reintroduce; overload;
       constructor Create(const aPolygons:TpvCSGBSPPolygons;const aDoFree:boolean=false); reintroduce; overload;
       constructor CreateSubtract(const aNodeA,aNodeB:TpvCSGBSPNode);
       constructor CreateUnion(const aNodeA,aNodeB:TpvCSGBSPNode);
       constructor CreateIntersection(const aNodeA,aNodeB:TpvCSGBSPNode);
       constructor CreateDifference(const aNodeA,aNodeB:TpvCSGBSPNode);
       destructor Destroy; override;
       procedure Build(const aPolygons:TpvCSGBSPPolygons;const aDoFree:boolean=false);
       function AllPolygons:TpvCSGBSPPolygons;
       function Clone:TpvCSGBSPNode;
       function Invert:TpvCSGBSPNode;
     end;

implementation

const EPSILON=1e-5;

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

function TpvCSGBSPVertex.Clone:TpvCSGBSPVertex;
begin
 result:=TpvCSGBSPVertex.Create;
 result.Position:=Position;
 result.Normal:=Normal;
 result.TexCoord:=TexCoord;
 result.Color:=Color;
end;

procedure TpvCSGBSPVertex.Flip;
begin
 Normal:=-Normal;
end;

function TpvCSGBSPVertex.Interpolate(const aWith:TpvCSGBSPVertex;const aTime:TpvDouble):TpvCSGBSPVertex;
begin
 result:=TpvCSGBSPVertex.Create;
 result.Position:=Position.Lerp(aWith.Position,aTime);
 result.Normal:=Normal.Lerp(aWith.Normal,aTime);
 result.TexCoord:=TexCoord.Lerp(aWith.TexCoord,aTime);
 result.Color:=Color.Lerp(aWith.Color,aTime);
end;

constructor TpvCSGBSPPlane.Create(const aV0,aV1,aV2:TpvCSGBSPVector3);
begin
 inherited Create;
 fNormal:=((aV1-aV0).Cross(aV2-aV0)).Normalize;
 fDistance:=fNormal.Dot(aV0);
end;

constructor TpvCSGBSPPlane.Create(const aNormal:TpvCSGBSPVector3;const aDistance:TpvDouble);
begin
 inherited Create;
 fNormal:=aNormal;
 fDistance:=aDistance;
end;

constructor TpvCSGBSPPlane.CreateEmpty;
begin
 inherited Create;
 fNormal.x:=0.0;
 fNormal.y:=0.0;
 fNormal.z:=0.0;
 fDistance:=0.0;
end;

function TpvCSGBSPPlane.Clone:TpvCSGBSPPlane;
begin
 result:=TpvCSGBSPPlane.Create(fNormal,fDistance);
end;

function TpvCSGBSPPlane.OK:boolean;
begin
 result:=fNormal.Length>0.0;
end;

procedure TpvCSGBSPPlane.Flip;
begin
 fNormal:=-fNormal;
 fDistance:=-fDistance;
end;

procedure TpvCSGBSPPlane.SplitPolygon(const aPolygon:TpvCSGBSPPolygon;const aCoplanarFrontList,aCoplanarBackList,aFrontList,aBackList:TpvCSGBSPPolygons);
const COPLANAR=0;
      FRONT=1;
      BACK=2;
      SPANNING=3;
var IndexA,IndexB,PolygonType,VectorType,VectorTypeA,VectorTypeB:TpvSizeInt;
    VectorTypes:array of TpvSizeInt;
    Time,VectorDistance:TpvDouble;
    FrontVertices,BackVertices:TpvCSGBSPVertices;
    Vertex,VertexA,VertexB:TpvCSGBSPVertex;
begin
 VectorTypes:=nil;
 try
  PolygonType:=0;
  SetLength(VectorTypes,aPolygon.fVertices.Count);
  for IndexA:=0 to aPolygon.fVertices.Count-1 do begin
   Vertex:=aPolygon.fVertices.Items[IndexA];
   VectorDistance:=fNormal.Dot(Vertex.Position)-fDistance;
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
    if fNormal.Dot(aPolygon.fPlane.fNormal)>0.0 then begin
     aCoplanarFrontList.Add(aPolygon.Clone);
    end else begin
     aCoplanarBackList.Add(aPolygon.Clone);
    end;
   end;
   FRONT:begin
    aFrontList.Add(aPolygon.Clone);
   end;
   BACK:begin
    aBackList.Add(aPolygon.Clone);
   end;
   else {SPANNING:}begin
    FrontVertices:=TpvCSGBSPVertices.Create(true);
    try
     BackVertices:=TpvCSGBSPVertices.Create(true);
     try
      for IndexA:=0 to aPolygon.fVertices.Count-1 do begin
       IndexB:=IndexA+1;
       if IndexB>=aPolygon.fVertices.Count then begin
        IndexB:=0;
       end;
       VertexA:=aPolygon.fVertices.Items[IndexA];
       VertexB:=aPolygon.fVertices.Items[IndexB];
       VectorTypeA:=VectorTypes[IndexA];
       VectorTypeB:=VectorTypes[IndexB];
       if VectorTypeA<>BACK then begin
        FrontVertices.Add(VertexA.Clone);
       end;
       if VectorTypeA<>FRONT then begin
        BackVertices.Add(VertexA.Clone);
       end;
       if (VectorTypeA or VectorTypeB)=SPANNING then begin
        Time:=(fDistance-fNormal.Dot(VertexA.Position))/fNormal.Dot(VertexB.Position-VertexA.Position);
        Vertex:=VertexA.Interpolate(VertexB,Time);
        try
         FrontVertices.Add(Vertex.Clone);
         BackVertices.Add(Vertex.Clone);
        finally
         FreeAndNil(Vertex);
        end;
       end;
      end;
      if FrontVertices.Count>=3 then begin
       aFrontList.Add(TpvCSGBSPPolygon.CreateFromVertices(FrontVertices));
      end;
      if BackVertices.Count>=3 then begin
       aBackList.Add(TpvCSGBSPPolygon.CreateFromVertices(BackVertices));
      end;
     finally
      FreeAndNil(BackVertices);
     end;
    finally
     FreeAndNil(FrontVertices);
    end;
   end;
  end;
 finally
  VectorTypes:=nil;
 end;
end;

constructor TpvCSGBSPPolygon.Create;
begin
 inherited Create;
 fVertices:=TpvCSGBSPVertices.Create(true);
 fPlane:=nil;
end;

constructor TpvCSGBSPPolygon.CreateFromVertices(const aVertices:TpvCSGBSPVertices);
var Vertex:TpvCSGBSPVertex;
begin
 Create;
 for Vertex in aVertices do begin
  fVertices.Add(Vertex.Clone);
 end;
 CalculateProperties;
end;

destructor TpvCSGBSPPolygon.Destroy;
begin
 FreeAndNil(fVertices);
 FreeAndNil(fPlane);
 inherited Destroy;
end;

procedure TpvCSGBSPPolygon.CalculateProperties;
begin
 if fVertices.Count>2 then begin
  FreeAndNil(fPlane);
  fPlane:=TpvCSGBSPPlane.Create(fVertices[0].Position,fVertices[1].Position,fVertices[2].Position);
 end;
end;

procedure TpvCSGBSPPolygon.Flip;
var Vertex:TpvCSGBSPVertex;
begin
 fVertices.Reverse;
 for Vertex in fVertices do begin
  Vertex.Flip;
 end;
 fPlane.Flip;
end;

function TpvCSGBSPPolygon.Clone:TpvCSGBSPPolygon;
begin
 result:=TpvCSGBSPPolygon.CreateFromVertices(fVertices);
 result.fPlane:=fPlane.Clone;
end;

constructor TpvCSGBSPPolygons.CreateCube(const aCX,aCY,aCZ,aRX,aRY,aRZ:TpvDouble);
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
 inherited Create(true);
 for SideIndex:=0 to 5 do begin
  Vertices:=TpvCSGBSPVertices.Create(true);
  try
   for SideVertexIndex:=0 to 3 do begin
    VertexIndex:=SideVertexIndices[SideIndex,SideVertexIndex];
    Vertex:=TpvCSGBSPVertex.Create;
    try
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
    finally
     Vertices.Add(Vertex);
    end;
   end;
  finally
   try
    Add(TpvCSGBSPPolygon.CreateFromVertices(Vertices));
   finally
    FreeAndNil(Vertices);
   end;
  end;
 end;
end;

constructor TpvCSGBSPPolygons.CreateSphere(const aCX,aCY,aCZ,aRadius:TpvDouble;const aSlices:TpvSizeInt=16;const aStacks:TpvSizeInt=8);
var Vertices:TpvCSGBSPVertices;
 procedure AddVertex(const aTheta,aPhi:TpvDouble);
 var Theta,Phi,dx,dy,dz:TpvDouble;
     Vertex:TpvCSGBSPVertex;
 begin
  Theta:=aTheta*TwoPI;
  Phi:=aPhi*PI;
  Vertex:=TpvCSGBSPVertex.Create;
  try
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
  finally
   Vertices.Add(Vertex);
  end;
 end;
var SliceIndex,StackIndex:TpvSizeInt;
begin
 inherited Create(true);
 for SliceIndex:=0 to aSlices-1 do begin
  for StackIndex:=0 to aStacks-1 do begin
   Vertices:=TpvCSGBSPVertices.Create(true);
   try
    AddVertex(SliceIndex/aSlices,StackIndex/aStacks);
    if StackIndex>0 then begin
     AddVertex((SliceIndex+1)/aSlices,StackIndex/aStacks);
    end;
    if StackIndex<(aStacks-1) then begin
     AddVertex((SliceIndex+1)/aSlices,(StackIndex+1)/aStacks);
    end;
    AddVertex(SliceIndex/aSlices,(StackIndex+1)/aStacks);
   finally
    Add(TpvCSGBSPPolygon.CreateFromVertices(Vertices));
   end;
  end;
 end;
end;

function TpvCSGBSPPolygons.ToTrianglePolygons:TpvCSGBSPPolygons;
var VertexIndex:TpvSizeInt;
    InputPolygon,OutputPolygon:TpvCSGBSPPolygon;
begin
 result:=TpvCSGBSPPolygons.Create(true);
 for InputPolygon in self do begin
  for VertexIndex:=2 to InputPolygon.fVertices.Count-1 do begin
   OutputPolygon:=TpvCSGBSPPolygon.Create;
   OutputPolygon.fVertices.Add(InputPolygon.fVertices[0].Clone);
   OutputPolygon.fVertices.Add(InputPolygon.fVertices[VertexIndex-1].Clone);
   OutputPolygon.fVertices.Add(InputPolygon.fVertices[VertexIndex].Clone);
   OutputPolygon.CalculateProperties;
   result.Add(OutputPolygon);
  end;
 end;
end;

constructor TpvCSGBSPNode.Create;
begin
 inherited Create;
 fPolygons:=TpvCSGBSPPolygons.Create(true);
 fPlane:=nil;
 fFrontNode:=nil;
 fBackNode:=nil;
end;

constructor TpvCSGBSPNode.Create(const aPolygons:TpvCSGBSPPolygons;const aDoFree:boolean=false);
begin
 Create;
 Build(aPolygons,aDoFree);
end;

constructor TpvCSGBSPNode.CreateSubtract(const aNodeA,aNodeB:TpvCSGBSPNode);
var a,b:TpvCSGBSPNode;
    p:TpvCSGBSPPolygons;
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
   p:=b.AllPolygons;
   try
    a.Build(p);
   finally
    FreeAndNil(p);
   end;
   a.Invert;
   p:=a.AllPolygons;
   try
    Create(p);
   finally
    FreeAndNil(p);
   end;
  finally
   FreeAndNil(b);
  end;
 finally
  FreeAndNil(a);
 end;
end;

constructor TpvCSGBSPNode.CreateUnion(const aNodeA,aNodeB:TpvCSGBSPNode);
var a,b:TpvCSGBSPNode;
    p:TpvCSGBSPPolygons;
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
   p:=b.AllPolygons;
   try
    a.Build(p);
   finally
    FreeAndNil(p);
   end;
   p:=a.AllPolygons;
   try
    Create(p);
   finally
    FreeAndNil(p);
   end;
  finally
   FreeAndNil(b);
  end;
 finally
  FreeAndNil(a);
 end;
end;

constructor TpvCSGBSPNode.CreateIntersection(const aNodeA,aNodeB:TpvCSGBSPNode);
var a,b:TpvCSGBSPNode;
    p:TpvCSGBSPPolygons;
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
   p:=b.AllPolygons;
   try
    a.Build(p);
   finally
    FreeAndNil(p);
   end;
   a.Invert;
   p:=a.AllPolygons;
   try
    Create(p);
   finally
    FreeAndNil(p);
   end;
  finally
   FreeAndNil(b);
  end;
 finally
  FreeAndNil(a);
 end;
end;

constructor TpvCSGBSPNode.CreateDifference(const aNodeA,aNodeB:TpvCSGBSPNode);
var a,b:TpvCSGBSPNode;
    p:TpvCSGBSPPolygons;
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
   p:=b.AllPolygons;
   try
    a.Build(p);
   finally
    FreeAndNil(p);
   end;
  finally
   FreeAndNil(b);
  end;
  b:=aNodeB.Clone;
  try
   b.ClipTo(a);
   a.ClipTo(b);
   a.Invert;
   a.ClipTo(b);
   a.Invert;
   p:=a.AllPolygons;
   try
    b.Build(p);
   finally
    FreeAndNil(p);
   end;
   p:=b.AllPolygons;
   try
    Create(p);
   finally
    FreeAndNil(p);
   end;
  finally
   FreeAndNil(b);
  end;
 finally
  FreeAndNil(a);
 end;
end;

destructor TpvCSGBSPNode.Destroy;
begin
 FreeAndNil(fPolygons);
 FreeAndNil(fPlane);
 FreeAndNil(fFrontNode);
 FreeAndNil(fBackNode);
 inherited Destroy;
end;

procedure TpvCSGBSPNode.Build(const aPolygons:TpvCSGBSPPolygons;const aDoFree:boolean=false);
{$ifdef NonRecursive}
type TJobStackItem=record
      Node:TpvCSGBSPNode;
      List:TpvCSGBSPPolygons;
      DoFree:boolean;
     end;
     TJobStack=TStack<TJobStackItem>;
var JobStack:TJobStack;
    JobStackItem,NewJobStackItem:TJobStackItem;
    Polygon:TpvCSGBSPPolygon;
    FrontList,BackList:TpvCSGBSPPolygons;
begin
 JobStack:=TJobStack.Create;
 try
  NewJobStackItem.Node:=self;
  NewJobStackItem.List:=aPolygons;
  NewJobStackItem.DoFree:=aDoFree;
  JobStack.Push(NewJobStackItem);
  while JobStack.Count>0 do begin
   JobStackItem:=JobStack.Pop;
   try
    FrontList:=TpvCSGBSPPolygons.Create(true);
    try
     BackList:=TpvCSGBSPPolygons.Create(true);
     try
      if JobStackItem.List.Count>0 then begin
       if not assigned(JobStackItem.Node.fPlane) then begin
        Polygon:=JobStackItem.List.Items[0];
        JobStackItem.Node.fPlane:=Polygon.fPlane.Clone;
       end;
       for Polygon in JobStackItem.List do begin
        JobStackItem.Node.fPlane.SplitPolygon(Polygon,
                                              JobStackItem.Node.fPolygons,
                                              JobStackItem.Node.fPolygons,
                                              FrontList,
                                              BackList);
       end;
       if BackList.Count>0 then begin
        if not assigned(JobStackItem.Node.fBackNode) then begin
         JobStackItem.Node.fBackNode:=TpvCSGBSPNode.Create;
        end;
        NewJobStackItem.Node:=JobStackItem.Node.fBackNode;
        NewJobStackItem.List:=BackList;
        NewJobStackItem.DoFree:=true;
        BackList:=nil;
        JobStack.Push(NewJobStackItem);
       end;
       if FrontList.Count>0 then begin
        if not assigned(JobStackItem.Node.fFrontNode) then begin
         JobStackItem.Node.fFrontNode:=TpvCSGBSPNode.Create;
        end;
        NewJobStackItem.Node:=JobStackItem.Node.fFrontNode;
        NewJobStackItem.List:=FrontList;
        NewJobStackItem.DoFree:=true;
        FrontList:=nil;
        JobStack.Push(NewJobStackItem);
       end;
      end;
     finally
      FreeAndNil(BackList);
     end;
    finally
     FreeAndNil(FrontList);
    end;
   finally
    if JobStackItem.DoFree then begin
     FreeAndNil(JobStackItem.List);
    end;
   end;
  end;
 finally
  FreeAndNil(JobStack);
 end;
end;
{$else}
var Polygon:TpvCSGBSPPolygon;
    FrontList,BackList:TpvCSGBSPPolygons;
begin
 try
  FrontList:=TpvCSGBSPPolygons.Create(true);
  try
   BackList:=TpvCSGBSPPolygons.Create(true);
   try
    if aPolygons.Count>0 then begin
     if not assigned(fPlane) then begin
      Polygon:=aPolygons.Items[0];
      fPlane:=Polygon.fPlane.Clone;
     end;
     for Polygon in aPolygons do begin
      fPlane.SplitPolygon(Polygon,
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
   finally
    FreeAndNil(BackList);
   end;
  finally
   FreeAndNil(FrontList);
  end;
 finally
  if aDoFree then begin
   aPolygons.Free;
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
    i:TpvSizeInt;
begin
 result:=TpvCSGBSPPolygons.Create(true);
 JobStack:=TJobStack.Create;
 try
  NewJobStackItem.Node:=self;
  JobStack.Push(NewJobStackItem);
  while JobStack.Count>0 do begin
   JobStackItem:=JobStack.Pop;
   for i:=0 to JobStackItem.Node.fPolygons.Count-1 do begin
    result.Add(JobStackItem.Node.fPolygons[i].Clone);
   end;
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
var Polygon:TpvCSGBSPPolygon;
    Polygons:TpvCSGBSPPolygons;
begin
 result:=TpvCSGBSPPolygons.Create(true);
 for Polygon in fPolygons do begin
  result.Add(Polygon.Clone);
 end;
 if assigned(fFrontNode) then begin
  Polygons:=fFrontNode.AllPolygons;
  try
   for Polygon in Polygons do begin
    result.Add(Polygon.Clone);
   end;
  finally
   FreeAndNil(Polygons);
  end;
 end;
 if assigned(fBackNode) then begin
  Polygons:=fBackNode.AllPolygons;
  try
   for Polygon in Polygons do begin
    result.Add(Polygon.Clone);
   end;
  finally
   FreeAndNil(Polygons);
  end;
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
    Polygon:TpvCSGBSPPolygon;
begin
 JobStack:=TJobStack.Create;
 try
  result:=TpvCSGBSPNode.Create;
  NewJobStackItem.DstNode:=result;
  NewJobStackItem.SrcNode:=self;
  JobStack.Push(NewJobStackItem);
  while JobStack.Count>0 do begin
   JobStackItem:=JobStack.Pop;
   for Polygon in JobStackItem.SrcNode.fPolygons do begin
    JobStackItem.DstNode.fPolygons.Add(Polygon.Clone);
   end;
   JobStackItem.DstNode.fPlane:=JobStackItem.SrcNode.fPlane.Clone;
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
 for Polygon in fPolygons do begin
  result.fPolygons.Add(Polygon.Clone);
 end;
 result.fPlane:=fPlane.Clone;
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
    Polygon:TpvCSGBSPPolygon;
begin
 JobStack:=TJobStack.Create;
 try
  JobStack.Push(self);
  while JobStack.Count>0 do begin
   Node:=JobStack.Pop;
   for Polygon in Node.fPolygons do begin
    Polygon.Flip;
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
var Polygon:TpvCSGBSPPolygon;
    Temp:TpvCSGBSPNode;
begin
 for Polygon in fPolygons do begin
  Polygon.Flip;
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
    JobStackItem,NewJobStackItem:TJobStackItem;
    FrontList,BackList:TpvCSGBSPPolygons;
    Polygon:TpvCSGBSPPolygon;
begin
 result:=TpvCSGBSPPolygons.Create(true);
 JobStack:=TJobStack.Create;
 try
  NewJobStackItem.Node:=self;
  NewJobStackItem.List:=aPolygons;
  JobStack.Push(NewJobStackItem);
  while JobStack.Count>0 do begin
   JobStackItem:=JobStack.Pop;
   try
    if assigned(JobStackItem.Node.fPlane) then begin
     FrontList:=TpvCSGBSPPolygons.Create(true);
     try
      BackList:=TpvCSGBSPPolygons.Create(true);
      try
       for Polygon in JobStackItem.List do begin
        JobStackItem.Node.fPlane.SplitPolygon(Polygon,FrontList,BackList,FrontList,BackList);
       end;
       if assigned(JobStackItem.Node.fBackNode) then begin
        NewJobStackItem.Node:=JobStackItem.Node.fBackNode;
        NewJobStackItem.List:=BackList;
        JobStack.Push(NewJobStackItem);
        BackList:=nil;
       end;
       if assigned(JobStackItem.Node.fFrontNode) then begin
        NewJobStackItem.Node:=JobStackItem.Node.fFrontNode;
        NewJobStackItem.List:=FrontList;
        JobStack.Push(NewJobStackItem);
        FrontList:=nil;
       end else begin
        for Polygon in FrontList do begin
         result.Add(Polygon.Clone);
        end;
       end;
      finally
       FreeAndNil(BackList);
      end;
     finally
      FreeAndNil(FrontList);
     end;
    end else begin
     for Polygon in JobStackItem.List do begin
      result.Add(Polygon.Clone);
     end;
    end;
   finally
    FreeAndNil(JobStackItem.List);
   end;
  end;
 finally
  FreeAndNil(JobStack);
 end;
end;
{$else}
var FrontList,BackList:TpvCSGBSPPolygons;
    Polygon:TpvCSGBSPPolygon;
begin
 try
  result:=TpvCSGBSPPolygons.Create(true);
  if assigned(fPlane) then begin
   FrontList:=TpvCSGBSPPolygons.Create(true);
   try
    BackList:=TpvCSGBSPPolygons.Create(true);
    try
     for Polygon in aPolygons do begin
      fPlane.SplitPolygon(Polygon,FrontList,BackList,FrontList,BackList);
     end;
     if assigned(fFrontNode) then begin
      FrontList:=fFrontNode.ClipPolygons(FrontList);
     end;
     for Polygon in FrontList do begin
      result.Add(Polygon.Clone);
     end;
     if assigned(fBackNode) then begin
      BackList:=fBackNode.ClipPolygons(BackList);
      for Polygon in BackList do begin
       result.Add(Polygon.Clone);
      end;
     end;
    finally
     FreeAndNil(BackList);
    end;
   finally
    FreeAndNil(FrontList);
   end;
  end else begin
   for Polygon in aPolygons do begin
    result.Add(Polygon.Clone);
   end;
  end;
 finally
  aPolygons.Free;
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
