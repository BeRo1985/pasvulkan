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
unit PasVulkan.Frustum;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math;

type { TpvFrustum }
     TpvFrustum=class
      public
       const COMPLETE_OUT=0;
             PARTIALLY_IN=1;
             COMPLETE_IN=2;
             fpRIGHT=0;
             fpLEFT=1;
             fpBOTTOM=2;
             fpTOP=3;
             fpFAR=4;
             fpNEAR=5;
       type TPlanes=array[0..5] of TpvPlane;
            TCorners=array[0..7] of TpvVector3;
      private
       fFrustumPlanes:TPlanes;
       fFrustumSphere:TpvSphere;
       fAABB:TpvAABB;
       fSphere:TpvSphere;
       fWorldSpaceCorners:TCorners;
       fViewSpaceCorners:TCorners;
      public
       constructor Create;
       destructor Destroy; override;
       procedure ExtractFrustum(aModelViewMatrix,aProjectionMatrix:pointer);
       procedure ExtractFrustumSphere(const aZNear,aZFar,aFOV,aAspectRatio:TpvScalar;const aPosition,aDirection:TpvVector3);
       function AABBInFrustum(const AAABB:TpvAABB):TpvInt32; overload;
       function AABBInFrustum(const AAABB:TpvAABB;var Mask:TpvUInt32):TpvInt32; overload;
       function SphereInFrustum(const aSphere:TpvSphere;const aRadius:TpvScalar=0.0):TpvInt32;
       function PointInFrustum(const aPoint:TpvVector3):boolean;
      public
       property FrustumPlanes:TPlanes read fFrustumPlanes write fFrustumPlanes;
       property FrustumSphere:TpvSphere read fFrustumSphere write fFrustumSphere;
       property AABB:TpvAABB read fAABB write fAABB;
       property Sphere:TpvSphere read fSphere write fSphere;
       property WorldSpaceCorners:TCorners read fWorldSpaceCorners write fWorldSpaceCorners;
       property ViewSpaceCorners:TCorners read fViewSpaceCorners write fViewSpaceCorners;
     end;

implementation

type TGLMatrix=array[0..15] of TpvFloat;

function IntersectionPoint(const a,b,c:TpvPlane):TpvVector3;
var v:array[0..2] of TpvVector3;
    f:TpvScalar;
begin
 f:=-a.Normal.Dot(b.Normal.Cross(c.Normal));
 v[0]:=b.Normal.Cross(c.Normal)*a.Distance;
 v[1]:=c.Normal.Cross(a.Normal)*b.Distance;
 v[2]:=a.Normal.Cross(b.Normal)*c.Distance;
 result.x:=(v[0].x+v[1].x+v[2].x)/f;
 result.y:=(v[0].y+v[1].y+v[2].y)/f;
 result.z:=(v[0].z+v[1].z+v[2].z)/f;
end;

constructor TpvFrustum.Create;
begin
 inherited Create;
end;

destructor TpvFrustum.Destroy;
begin
 inherited Destroy;
end;

procedure TpvFrustum.ExtractFrustum(aModelViewMatrix,aProjectionMatrix:pointer);
var modl,proj,clip:TGLMatrix;
    i:TpvInt32;
begin

 modl:=TGLMatrix(aModelViewMatrix^);
 proj:=TGLMatrix(aProjectionMatrix^);

 clip[0]:=(modl[0]*proj[0])+(modl[1]*proj[4])+(modl[2]*proj[8])+(modl[3]*proj[12]);
 clip[1]:=(modl[0]*proj[1])+(modl[1]*proj[5])+(modl[2]*proj[9])+(modl[3]*proj[13]);
 clip[2]:=(modl[0]*proj[2])+(modl[1]*proj[6])+(modl[2]*proj[10])+(modl[3]*proj[14]);
 clip[3]:=(modl[0]*proj[3])+(modl[1]*proj[7])+(modl[2]*proj[11])+(modl[3]*proj[15]);

 clip[4]:=(modl[4]*proj[0])+(modl[5]*proj[4])+(modl[6]*proj[8])+(modl[7]*proj[12]);
 clip[5]:=(modl[4]*proj[1])+(modl[5]*proj[5])+(modl[6]*proj[9])+(modl[7]*proj[13]);
 clip[6]:=(modl[4]*proj[2])+(modl[5]*proj[6])+(modl[6]*proj[10])+(modl[7]*proj[14]);
 clip[7]:=(modl[4]*proj[3])+(modl[5]*proj[7])+(modl[6]*proj[11])+(modl[7]*proj[15]);

 clip[8]:=(modl[8]*proj[0])+(modl[9]*proj[4])+(modl[10]*proj[8])+(modl[11]*proj[12]);
 clip[9]:=(modl[8]*proj[1])+(modl[9]*proj[5])+(modl[10]*proj[9])+(modl[11]*proj[13]);
 clip[10]:=(modl[8]*proj[2])+(modl[9]*proj[6])+(modl[10]*proj[10])+(modl[11]*proj[14]);
 clip[11]:=(modl[8]*proj[3])+(modl[9]*proj[7])+(modl[10]*proj[11])+(modl[11]*proj[15]);

 clip[12]:=(modl[12]*proj[0])+(modl[13]*proj[4])+(modl[14]*proj[8])+(modl[15]*proj[12]);
 clip[13]:=(modl[12]*proj[1])+(modl[13]*proj[5])+(modl[14]*proj[9])+(modl[15]*proj[13]);
 clip[14]:=(modl[12]*proj[2])+(modl[13]*proj[6])+(modl[14]*proj[10])+(modl[15]*proj[14]);
 clip[15]:=(modl[12]*proj[3])+(modl[13]*proj[7])+(modl[14]*proj[11])+(modl[15]*proj[15]);

 // right
 fFrustumPlanes[fpRIGHT].x:=clip[3]-clip[0];
 fFrustumPlanes[fpRIGHT].y:=clip[7]-clip[4];
 fFrustumPlanes[fpRIGHT].z:=clip[11]-clip[8];
 fFrustumPlanes[fpRIGHT].w:=clip[15]-clip[12];
 fFrustumPlanes[fpRIGHT]:=fFrustumPlanes[fpRIGHT].Normalize;

 // left
 fFrustumPlanes[fpLEFT].x:=clip[3]+clip[0];
 fFrustumPlanes[fpLEFT].y:=clip[7]+clip[4];
 fFrustumPlanes[fpLEFT].z:=clip[11]+clip[8];
 fFrustumPlanes[fpLEFT].w:=clip[15]+clip[12];
 fFrustumPlanes[fpLEFT]:=fFrustumPlanes[fpLEFT].Normalize;

 // bottom
 fFrustumPlanes[fpBOTTOM].x:=clip[3]+clip[1];
 fFrustumPlanes[fpBOTTOM].y:=clip[7]+clip[5];
 fFrustumPlanes[fpBOTTOM].z:=clip[11]+clip[9];
 fFrustumPlanes[fpBOTTOM].w:=clip[15]+clip[13];
 fFrustumPlanes[fpBOTTOM]:=fFrustumPlanes[fpBOTTOM].Normalize;

 // top
 fFrustumPlanes[fpTOP].x:=clip[3]-clip[1];
 fFrustumPlanes[fpTOP].y:=clip[7]-clip[5];
 fFrustumPlanes[fpTOP].z:=clip[11]-clip[9];
 fFrustumPlanes[fpTOP].w:=clip[15]-clip[13];
 fFrustumPlanes[fpTOP]:=fFrustumPlanes[fpTOP].Normalize;

 // far
 fFrustumPlanes[fpFAR].x:=clip[3]-clip[2];
 fFrustumPlanes[fpFAR].y:=clip[7]-clip[6];
 fFrustumPlanes[fpFAR].z:=clip[11]-clip[10];
 fFrustumPlanes[fpFAR].w:=clip[15]-clip[14];
 fFrustumPlanes[fpFAR]:=fFrustumPlanes[fpFAR].Normalize;

 // near
 fFrustumPlanes[fpNEAR].x:=clip[3]+clip[2];
 fFrustumPlanes[fpNEAR].y:=clip[7]+clip[6];
 fFrustumPlanes[fpNEAR].z:=clip[11]+clip[10];
 fFrustumPlanes[fpNEAR].w:=clip[15]+clip[14];
 fFrustumPlanes[fpNEAR]:=fFrustumPlanes[fpNEAR].Normalize;

 fWorldSpaceCorners[0]:=IntersectionPoint(fFrustumPlanes[fpNEAR],fFrustumPlanes[fpLEFT],fFrustumPlanes[fpTOP]);
 fWorldSpaceCorners[1]:=IntersectionPoint(fFrustumPlanes[fpNEAR],fFrustumPlanes[fpRIGHT],fFrustumPlanes[fpTOP]);
 fWorldSpaceCorners[2]:=IntersectionPoint(fFrustumPlanes[fpNEAR],fFrustumPlanes[fpRIGHT],fFrustumPlanes[fpBOTTOM]);
 fWorldSpaceCorners[3]:=IntersectionPoint(fFrustumPlanes[fpNEAR],fFrustumPlanes[fpLEFT],fFrustumPlanes[fpBOTTOM]);
 fWorldSpaceCorners[4]:=IntersectionPoint(fFrustumPlanes[fpFAR],fFrustumPlanes[fpLEFT],fFrustumPlanes[fpTOP]);
 fWorldSpaceCorners[5]:=IntersectionPoint(fFrustumPlanes[fpFAR],fFrustumPlanes[fpRIGHT],fFrustumPlanes[fpTOP]);
 fWorldSpaceCorners[6]:=IntersectionPoint(fFrustumPlanes[fpFAR],fFrustumPlanes[fpRIGHT],fFrustumPlanes[fpBOTTOM]);
 fWorldSpaceCorners[7]:=IntersectionPoint(fFrustumPlanes[fpFAR],fFrustumPlanes[fpLEFT],fFrustumPlanes[fpBOTTOM]);

 fAABB.Min:=fWorldSpaceCorners[0];
 fAABB.Max:=fWorldSpaceCorners[0];
 for i:=1 to 7 do begin
  fAABB.Min.x:=min(fAABB.Min.x,fWorldSpaceCorners[i].x);
  fAABB.Min.y:=min(fAABB.Min.y,fWorldSpaceCorners[i].y);
  fAABB.Min.z:=min(fAABB.Min.z,fWorldSpaceCorners[i].z);
  fAABB.Max.x:=max(fAABB.Max.x,fWorldSpaceCorners[i].x);
  fAABB.Max.y:=max(fAABB.Max.y,fWorldSpaceCorners[i].y);
  fAABB.Max.z:=max(fAABB.Max.z,fWorldSpaceCorners[i].z);
 end;

 fSphere.Center:=fWorldSpaceCorners[0];
 for i:=1 to 7 do begin
  fSphere.Center.x:=fSphere.Center.x+fWorldSpaceCorners[i].x;
  fSphere.Center.y:=fSphere.Center.y+fWorldSpaceCorners[i].y;
  fSphere.Center.z:=fSphere.Center.z+fWorldSpaceCorners[i].z;
 end;
 fSphere.Center.x:=fSphere.Center.x*0.125;
 fSphere.Center.y:=fSphere.Center.y*0.125;
 fSphere.Center.z:=fSphere.Center.z*0.125;
 fSphere.Radius:=0.0;
 for i:=0 to 7 do begin
  fSphere.Radius:=max(fSphere.Radius,fSphere.Center.DistanceTo(fWorldSpaceCorners[i]));
 end;

 for i:=0 to 7 do begin
  fViewSpaceCorners[i]:=(PpvMatrix4x4(aModelViewMatrix)^*fWorldSpaceCorners[i]).xyz;
 end;

end;

procedure TpvFrustum.ExtractFrustumSphere(const aZNear,aZFar,aFOV,aAspectRatio:TpvScalar;const aPosition,aDirection:TpvVector3);
var ViewLen,Width,Height:TpvScalar;
begin
 ViewLen:=aZFar-aZNear;
 Height:=ViewLen*tan(aFOV*0.5);
 Width:=Height*aAspectRatio;
 fFrustumSphere.Radius:=TpvVector3.Create(Width,Height,ViewLen).DistanceTo(TpvVector3.Create(0.0,0.0,aZNear+(ViewLen*0.5)));
 fFrustumSphere.Center:=aPosition+(aDirection*((ViewLen*0.5)+aZNear));
end;

function TpvFrustum.AABBInFrustum(const AAABB:TpvAABB):TpvInt32;
var p:TpvInt32;
    m,n:TpvScalar;
    Center,Extents:TpvVector3;
begin
 if ((fAABB.Max.x>=AAABB.Min.x) and (fAABB.Min.x<=AAABB.Max.x)) and
    ((fAABB.Max.y>=AAABB.Min.y) and (fAABB.Min.y<=AAABB.Max.y)) and
    ((fAABB.Max.z>=AAABB.Min.z) and (fAABB.Min.z<=AAABB.Max.z)) then begin
  Center.x:=(AAABB.Min.x+AAABB.Max.x)*0.5;
  Center.y:=(AAABB.Min.y+AAABB.Max.y)*0.5;
  Center.z:=(AAABB.Min.z+AAABB.Max.z)*0.5;
  Extents.x:=(AAABB.Max.x-AAABB.Min.x)*0.5;
  Extents.y:=(AAABB.Max.y-AAABB.Min.y)*0.5;
  Extents.z:=(AAABB.Max.z-AAABB.Min.z)*0.5;
  result:=COMPLETE_IN;
  for p:=0 to 5 do begin
   m:=(fFrustumPlanes[p].x*Center.x)+(fFrustumPlanes[p].y*Center.y)+(fFrustumPlanes[p].z*Center.z)+fFrustumPlanes[p].w;
   n:=(abs(fFrustumPlanes[p].x)*Extents.x)+(abs(fFrustumPlanes[p].y)*Extents.y)+(abs(fFrustumPlanes[p].z)*Extents.z);
   if (m+n)<0.0 then begin
    result:=COMPLETE_OUT;
    exit;
   end else if (m-n)<0.0 then begin
    result:=PARTIALLY_IN;
   end;
  end;
 end else begin
  result:=COMPLETE_OUT;
 end;
end;

function TpvFrustum.AABBInFrustum(const AAABB:TpvAABB;var Mask:TpvUInt32):TpvInt32;
var p:TpvInt32;
    Bit,InMask,OutMask:TpvUInt32;
    m,n:TpvScalar;
    Center,Extents:TpvVector3;
begin
 if ((fAABB.Max.x>=AAABB.Min.x) and (fAABB.Min.x<=AAABB.Max.x)) and
    ((fAABB.Max.y>=AAABB.Min.y) and (fAABB.Min.y<=AAABB.Max.y)) and
    ((fAABB.Max.z>=AAABB.Min.z) and (fAABB.Min.z<=AAABB.Max.z)) then begin
  Center.x:=(AAABB.Min.x+AAABB.Max.x)*0.5;
  Center.y:=(AAABB.Min.y+AAABB.Max.y)*0.5;
  Center.z:=(AAABB.Min.z+AAABB.Max.z)*0.5;
  Extents.x:=(AAABB.Max.x-AAABB.Min.x)*0.5;
  Extents.y:=(AAABB.Max.y-AAABB.Min.y)*0.5;
  Extents.z:=(AAABB.Max.z-AAABB.Min.z)*0.5;
  result:=COMPLETE_IN;
  InMask:=Mask;
  OutMask:=$40000000;
  Bit:=1;
  for p:=0 to 5 do begin
   if (InMask and Bit)<>0 then begin
    m:=(fFrustumPlanes[p].x*Center.x)+(fFrustumPlanes[p].y*Center.y)+(fFrustumPlanes[p].z*Center.z)+fFrustumPlanes[p].w;
    n:=(abs(fFrustumPlanes[p].x)*Extents.x)+(abs(fFrustumPlanes[p].y)*Extents.y)+(abs(fFrustumPlanes[p].z)*Extents.z);
    if (m+n)<0.0 then begin
     Mask:=0;
     result:=COMPLETE_OUT;
     exit;
    end else if (m-n)<0.0 then begin
     OutMask:=OutMask or (Bit or $80000000);
     result:=PARTIALLY_IN;
    end;
   end;
   inc(Bit,Bit);
  end;
  Mask:=OutMask;
 end else begin
  Mask:=0;
  result:=COMPLETE_OUT;
 end;
end;

function TpvFrustum.SphereInFrustum(const aSphere:TpvSphere;const aRadius:TpvScalar=0.0):TpvInt32;
var p,c:TpvInt32;
    d,r:TpvScalar;
begin
 c:=0;
 r:=aSphere.Radius+aRadius;
 for p:=0 to 5 do begin
  d:=(fFrustumPlanes[p].x*aSphere.Center.x)+(fFrustumPlanes[p].y*aSphere.Center.y)+(fFrustumPlanes[p].z*aSphere.Center.z)+fFrustumPlanes[p].w;
  if d<=(-r) then begin
   result:=COMPLETE_OUT;
   exit;
  end else if d>r then begin
   inc(c);
  end;
 end;
 if c=6 then begin
  result:=COMPLETE_IN;
 end else begin
  result:=PARTIALLY_IN;
 end;
end;

function TpvFrustum.PointInFrustum(const aPoint:TpvVector3):boolean;
var i:TpvInt32;
begin
 result:=true;
 for i:=0 to 5 do begin
  if ((fFrustumPlanes[i].x*aPoint.x)+(fFrustumPlanes[i].y*aPoint.y)+(fFrustumPlanes[i].z*aPoint.z)+fFrustumPlanes[i].w)<=0.0 then begin
   result:=false;
   break;
  end;
 end;
end;

end.

