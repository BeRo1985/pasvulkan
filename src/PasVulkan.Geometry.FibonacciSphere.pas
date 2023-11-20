(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2023, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.Geometry.FibonacciSphere;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

{$scopedenums on}

interface

uses Classes,SysUtils,Math,PasVulkan.Types,PasVulkan.Math,PasVulkan.Collections;

type { TpvFibonacciSphere }
     TpvFibonacciSphere=class
      public             
       const GoldenRatio=1.61803398874989485; // (1.0+sqrt(5.0))/2.0 (golden ratio)
             GoldenRatioMinusOne=0.61803398874989485; // ((1.0+sqrt(5.0))/2.0)-1.0
             GoldenAngle=2.39996322972865332; // PI*(3.0-sqrt(5.0)) (golden angle) 
             Sqrt5=2.236067977499789696; // sqrt(5.0)
             OneOverSqrt5=0.447213595499957939; // 1.0/sqrt(5.0)
             PImulSqrt5=7.024814731040726393; // PI*sqrt(5.0)
             HalfPI=1.570796326794896619; // PI/2.0
             TwoPI=6.283185307179586477; // PI*2.0
             LogGoldenRatio=0.481211825059603447; // ln((1.0+sqrt(5.0))/2.0) (log of golden ratio)
             OneOverLogGoldenRatio=2.0780869212350275376; // 1.0/ln((1.0+sqrt(5.0))/2.0) (1.0/log of golden ratio)
       type { TVector }
            TVector=record
             x:TpvDouble;
             y:TpvDouble;
             z:TpvDouble;
             constructor Create(const aX,aY,aZ:TpvDouble); 
             class function InlineableCreate(const aX,aY,aZ:TpvDouble):TVector; static; inline;
             class operator Add(const a,b:TVector):TVector;
             class operator Subtract(const a,b:TVector):TVector;
             class operator Multiply(const a:TVector;const b:TpvDouble):TVector;
             class operator Multiply(const a,b:TVector):TVector;
             class operator Divide(const a:TVector;const b:TpvDouble):TVector;
             function Length:TpvDouble;
             function SquaredLength:TpvDouble;
             function Normalize:TVector;
             function Distance(const aVector:TVector):TpvDouble;
             function SquaredDistance(const aVector:TVector):TpvDouble;
             function Dot(const aVector:TVector):TpvDouble;
             function Cross(const aVector:TVector):TVector;
            end; 
            PVector=^TVector;
            TVectors=array of TpvFibonacciSphere.TVector;
            TVertex=record
             Position:TpvVector3;
             Normal:TpvVector3;
             Tangent:TpvVector3;
             Bitangent:TpvVector3;
             TexCoord:TpvVector2;
            end;
            PVertex=^TpvFibonacciSphere.TVertex;
            TVertices=TpvDynamicArrayList<TpvFibonacciSphere.TVertex>;
            TIndices=TpvDynamicArrayList<TpvUInt32>;
      private
       fCountPoints:TpvSizeInt;
       fRadius:TpvDouble;
       fVertices:TVertices;
       fIndices:TIndices;
      public
       constructor Create(const aCountPoints:TpvSizeInt;const aRadius:TpvDouble=1.0);
       destructor Destroy; override;
       procedure Generate;
      published 
       property CountPoints:TpvSizeInt read fCountPoints;
       property Radius:TpvDouble read fRadius;
       property Vertices:TVertices read fVertices;
       property Indices:TIndices read fIndices;
     end;

implementation

{ TpvFibonacciSphere.TVector }

constructor TpvFibonacciSphere.TVector.Create(const aX,aY,aZ:TpvDouble);
begin
 x:=aX;
 y:=aY;
 z:=aZ;
end;

class function TpvFibonacciSphere.TVector.InlineableCreate(const aX,aY,aZ:TpvDouble):TVector;
begin
 result.x:=aX;
 result.y:=aY;
 result.z:=aZ;
end;

class operator TpvFibonacciSphere.TVector.Add(const a,b:TVector):TVector;
begin
 result.x:=a.x+b.x;
 result.y:=a.y+b.y;
 result.z:=a.z+b.z;
end;

class operator TpvFibonacciSphere.TVector.Subtract(const a,b:TVector):TVector;
begin
 result.x:=a.x-b.x;
 result.y:=a.y-b.y;
 result.z:=a.z-b.z;
end;

class operator TpvFibonacciSphere.TVector.Multiply(const a:TVector;const b:TpvDouble):TVector;
begin
 result.x:=a.x*b;
 result.y:=a.y*b;
 result.z:=a.z*b;
end;

class operator TpvFibonacciSphere.TVector.Multiply(const a,b:TVector):TVector;
begin
 result.x:=a.x*b.x;
 result.y:=a.y*b.y;
 result.z:=a.z*b.z;
end;

class operator TpvFibonacciSphere.TVector.Divide(const a:TVector;const b:TpvDouble):TVector;
begin
 result.x:=a.x/b;
 result.y:=a.y/b;
 result.z:=a.z/b;
end;

function TpvFibonacciSphere.TVector.Length:TpvDouble;
begin
 result:=sqrt(sqr(x)+sqr(y)+sqr(z));
end;

function TpvFibonacciSphere.TVector.SquaredLength:TpvDouble;
begin
 result:=sqr(x)+sqr(y)+sqr(z);
end;

function TpvFibonacciSphere.TVector.Normalize:TVector;
var l:TpvDouble;
begin
 l:=Length;
 if l>0.0 then begin
  result.x:=x/l;
  result.y:=y/l;
  result.z:=z/l;
 end else begin
  result.x:=x;
  result.y:=y;
  result.z:=z;
 end;
end;

function TpvFibonacciSphere.TVector.Distance(const aVector:TVector):TpvDouble;
begin
 result:=sqrt(sqr(x-aVector.x)+sqr(y-aVector.y)+sqr(z-aVector.z));
end;

function TpvFibonacciSphere.TVector.SquaredDistance(const aVector:TVector):TpvDouble;
begin
 result:=sqr(x-aVector.x)+sqr(y-aVector.y)+sqr(z-aVector.z);
end;

function TpvFibonacciSphere.TVector.Dot(const aVector:TVector):TpvDouble;
begin
 result:=(x*aVector.x)+(y*aVector.y)+(z*aVector.z);
end;

function TpvFibonacciSphere.TVector.Cross(const aVector:TVector):TVector;
begin
 result.x:=(y*aVector.z)-(z*aVector.y);
 result.y:=(z*aVector.x)-(x*aVector.z);
 result.z:=(x*aVector.y)-(y*aVector.x);
end;

{ TpvFibonacciSphere }

constructor TpvFibonacciSphere.Create(const aCountPoints:TpvSizeInt;const aRadius:TpvDouble=1.0);
begin
 inherited Create;
 fCountPoints:=Max(32,aCountPoints);
 fRadius:=aRadius;
 fVertices:=TVertices.Create;
 fIndices:=TIndices.Create;
end;

destructor TpvFibonacciSphere.Destroy;
begin
 FreeAndNil(fVertices);
 FreeAndNil(fIndices);
 inherited Destroy;
end;

procedure TpvFibonacciSphere.Generate;
var Index,OtherIndex,CountNearestSamples,CountAdjacentVertices,r,c,k,PreviousK,NextK,
    i0,i1,i2:TpvSizeInt;
    Phi,Z,SinTheta,PhiSinus,PhiCosinus,CosTheta:TpvDouble;
    Vertex:PVertex;
    Vector,Normal,Tangent,Bitangent:TpvFibonacciSphere.TVector;
    NearestSamples,AdjacentVertices:array[0..11] of TpvSizeInt;
    Points:TVectors;
begin
 
 Points:=nil;
 try

  SetLength(Points,fCountPoints);

  // Generate vertices (the comparatively yet easy part)
  begin

   fVertices.Clear;

   for Index:=0 to fCountPoints-1 do begin

     Phi:=TwoPI*((Index*GoldenRatioMinusOne)-Floor(Index*GoldenRatioMinusOne));
     Z:=1.0-(((Index shl 1) or 1)/fCountPoints);
     SinTheta:=sqrt(1.0-sqr(Z));
     SinCos(Phi,PhiSinus,PhiCosinus);
     Vector:=TpvFibonacciSphere.TVector.InlineableCreate(PhiCosinus*SinTheta,z,PhiSinus*SinTheta).Normalize;

     Points[Index]:=Vector;

     Normal:=Vector;
     Tangent:=TpvFibonacciSphere.TVector.InlineableCreate(-Normal.z,0.0,Normal.x).Normalize;
     Bitangent:=Normal.Cross(Tangent).Normalize;

     Vertex:=fVertices.AddNew;
     Vertex^.Position:=TpvVector3.InlineableCreate(Vector.x,Vector.y,Vector.z)*fRadius;
     Vertex^.Normal:=TpvVector3.InlineableCreate(Normal.x,Normal.y,Normal.z);
     Vertex^.Tangent:=TpvVector3.InlineableCreate(Tangent.x,Tangent.y,Tangent.z);
     Vertex^.Bitangent:=TpvVector3.InlineableCreate(Bitangent.x,Bitangent.y,Bitangent.z);
     Vertex^.TexCoord:=TpvVector2.InlineableCreate((ArcTan2(Vector.z,Vector.x)+PI)/TwoPI,ArcCos(Vector.y)/PI);

   end;

  end;

  // Generate indices (the not so easy part) 
  begin 

   fIndices.Clear;

   for Index:=0 to fCountPoints-1 do begin

    CosTheta:=1.0-(((Index shl 1) or 1)/fCountPoints);

    z:=Max(0.0,round(0.5*Ln(fCountPoints*PImulSqrt5*(1.0-sqr(CosTheta)))*OneOverLogGoldenRatio));

    CountNearestSamples:=0;

    for OtherIndex:=0 to 11 do begin
     r:=OtherIndex-Trunc(Floor(OtherIndex/6)*6);
     c:=(5-abs(5-(r shl 1)))+Floor(Trunc(r)/3);
     k:=(Round(Pow(GoldenRatio,(z+c)-2)*OneOverSqrt5)*IfThen(OtherIndex<6,1,-1))+Index;
     if (k>=0) and (k<fCountPoints) and ((Points[k]-Points[Index]).SquaredLength<=(20.0*PI)/(Sqrt5*fCountPoints)) then begin
      NearestSamples[CountNearestSamples]:=k;
      inc(CountNearestSamples);
     end;
    end;

    CountAdjacentVertices:=0;

    for OtherIndex:=0 to CountNearestSamples-1 do begin

     k:=NearestSamples[OtherIndex];

     if OtherIndex>0 then begin
      PreviousK:=NearestSamples[OtherIndex-1];
     end else begin
      PreviousK:=NearestSamples[CountNearestSamples-1];
     end;

     if (OtherIndex+1)<CountNearestSamples then begin
      NextK:=NearestSamples[OtherIndex+1];
     end else begin
      NextK:=NearestSamples[0];
     end;

     if Points[PreviousK].SquaredDistance(Points[NextK])>Points[PreviousK].SquaredDistance(Points[k]) then begin
      AdjacentVertices[CountAdjacentVertices]:=k;
      inc(CountAdjacentVertices);
     end;

    end;

    if (OtherIndex=0) and (CountAdjacentVertices>0) then begin
     dec(CountAdjacentVertices); // Special case for the pole
    end;

    i0:=Index;

    // Add triangles from the adjacent neighbours
    for OtherIndex:=0 to CountAdjacentVertices-1 do begin
     i1:=AdjacentVertices[OtherIndex];
     if (OtherIndex+1)<CountAdjacentVertices then begin
      i2:=AdjacentVertices[OtherIndex+1];
     end else begin
      i2:=AdjacentVertices[0];
     end;
     if (i1>i0) and (i2>i0) then begin // Avoid duplicate triangles, so only add triangles with vertices in ascending positive order
      if ((Points[i1]-Points[i0]).Cross(Points[i2]-Points[i0])).Dot(Points[i0])<0.0 then begin // Only add triangles with vertices in counter-clockwise order
       fIndices.Add(i0);
       fIndices.Add(i1);
       fIndices.Add(i2);
      end else begin
       fIndices.Add(i0);
       fIndices.Add(i2);
       fIndices.Add(i1);
      end;
     end;
    end;

   end;

  end;

 finally
  Points:=nil;
 end; 

end;
      
end.
