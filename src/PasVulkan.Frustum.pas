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
{$scopedenums on}

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasMP,
     PasVulkan.Types,
     PasVulkan.Math;

type { TpvFrustum }
     TpvFrustum=record
      public
       const COMPLETE_OUT=0;
             PARTIALLY_IN=1;
             COMPLETE_IN=2;
       type TFlag=
             (
              ReverseZ,
              InfiniteFarPlane
             );
            PFlag=^TFlag;
            TFlags=set of TFlag;
            PFlags=^TFlags;
            TFrustumSide=
             (
              Left=0,
              Right=1,
              Bottom=2,
              Top=3,
              Near_=4,
              Far_=5
             );
            TPlanes=array[TFrustumSide] of TpvPlane;
            TAbsoluteNormals=array[TFrustumSide] of TpvVector3;
            TCorners=array[0..7] of TpvVector3;
      private
       fPlanes:TPlanes;
       fAbsoluteNormals:TAbsoluteNormals;
       fWorldSpaceCorners:TCorners;
       fFlags:TFlags;
       fMaximumPlaneSide:TFrustumSide;
      public
       procedure Init(const aViewMatrix,aProjectionMatrix:TpvMatrix4x4);
       class function ExtractFrustumSphere(const aZNear,aZFar,aFOV,aAspectRatio:TpvScalar;const aPosition,aDirection:TpvVector3):TpvSphere; static;
       function AABBInFrustum(const aAABBMin,aAABBMax:TpvVector3):TpvInt32; overload;
       function AABBInFrustum(const aAABBMin,aAABBMax:TpvVector3;var aMask:TpvUInt32):TpvInt32; overload;
       function AABBInFrustum(const aAABB:TpvAABB):TpvInt32; overload;
       function AABBInFrustum(const aAABB:TpvAABB;var aMask:TpvUInt32):TpvInt32; overload;
       function SphereInFrustum(const aSphereCenter:TpvVector3;const aSphereRadius:TpvScalar;const aRadius:TpvScalar=0.0):TpvInt32; overload;
       function SphereInFrustum(const aSphere:TpvSphere;const aRadius:TpvScalar=0.0):TpvInt32; overload;
       function PointInFrustum(const aPoint:TpvVector3):boolean;
      public
       property Planes:TPlanes read fPlanes write fPlanes;
       property AbsoluteNormals:TAbsoluteNormals read fAbsoluteNormals write fAbsoluteNormals;
     end;

     PpvFrustum=^TpvFrustum;

     TpvFrustumDynamicArray=array of TpvFrustum;

implementation

procedure TpvFrustum.Init(const aViewMatrix,aProjectionMatrix:TpvMatrix4x4);
const LeftRight=0;
      LeftBottom=1;
      LeftTop=2;
      LeftNear=3;
      LeftFar=4;
      RightBottom=5;
      RightTop=6;
      RightNear=7;
      RightFar=8;
      BottomTop=9;
      BottomNear=10;
      BottomFar=11;
      TopNear=12;
      TopFar=13;
      NearFar=14;
var FrustumSide:TFrustumSide;
    ViewProjectionMatrix:TpvMatrix4x4;
    Crosses:array[0..14] of TpvVector3;
begin

 fFlags:=[];

 if aProjectionMatrix.RawComponents[2,3]<(-1e-7) then begin
  Include(fFlags,TpvFrustum.TFlag.ReverseZ);
  if IsZero(aProjectionMatrix.RawComponents[2,2]) and not IsZero(aProjectionMatrix.RawComponents[3,2]) then begin
   Include(fFlags,TpvFrustum.TFlag.InfiniteFarPlane);
  end;
 end;

 if TpvFrustum.TFlag.InfiniteFarPlane in fFlags then begin
  fMaximumPlaneSide:=TFrustumSide.Near_;
 end else begin
  fMaximumPlaneSide:=TFrustumSide.Far_;
 end;

 ViewProjectionMatrix:=(aViewMatrix*aProjectionMatrix).Transpose;

 fPlanes[TFrustumSide.Left]:=TpvPlane.Create(ViewProjectionMatrix.RawVectors[3]+ViewProjectionMatrix.RawVectors[0]).Normalize;
 fPlanes[TFrustumSide.Right]:=TpvPlane.Create(ViewProjectionMatrix.RawVectors[3]-ViewProjectionMatrix.RawVectors[0]).Normalize;
 fPlanes[TFrustumSide.Bottom]:=TpvPlane.Create(ViewProjectionMatrix.RawVectors[3]+ViewProjectionMatrix.RawVectors[1]).Normalize;
 fPlanes[TFrustumSide.Top]:=TpvPlane.Create(ViewProjectionMatrix.RawVectors[3]-ViewProjectionMatrix.RawVectors[1]).Normalize;
 if TpvFrustum.TFlag.ReverseZ in fFlags then begin
  fPlanes[TFrustumSide.Near_]:=TpvPlane.Create(ViewProjectionMatrix.RawVectors[3]-ViewProjectionMatrix.RawVectors[2]).Normalize;
  fPlanes[TFrustumSide.Far_]:=TpvPlane.Create({ViewProjectionMatrix.RawVectors[3]+}ViewProjectionMatrix.RawVectors[2]).Normalize;
 end else begin
  fPlanes[TFrustumSide.Near_]:=TpvPlane.Create({ViewProjectionMatrix.RawVectors[3]+}ViewProjectionMatrix.RawVectors[2]).Normalize;
  fPlanes[TFrustumSide.Far_]:=TpvPlane.Create(ViewProjectionMatrix.RawVectors[3]-ViewProjectionMatrix.RawVectors[2]).Normalize;
 end;

 Crosses[LeftRight]:=fPlanes[TFrustumSide.Left].Normal.Cross(fPlanes[TFrustumSide.Right].Normal);
 Crosses[LeftBottom]:=fPlanes[TFrustumSide.Left].Normal.Cross(fPlanes[TFrustumSide.Bottom].Normal);
 Crosses[LeftTop]:=fPlanes[TFrustumSide.Left].Normal.Cross(fPlanes[TFrustumSide.Top].Normal);
 Crosses[LeftNear]:=fPlanes[TFrustumSide.Left].Normal.Cross(fPlanes[TFrustumSide.Near_].Normal);
 Crosses[LeftFar]:=fPlanes[TFrustumSide.Left].Normal.Cross(fPlanes[TFrustumSide.Far_].Normal);
 Crosses[RightBottom]:=fPlanes[TFrustumSide.Right].Normal.Cross(fPlanes[TFrustumSide.Bottom].Normal);
 Crosses[RightTop]:=fPlanes[TFrustumSide.Right].Normal.Cross(fPlanes[TFrustumSide.Top].Normal);
 Crosses[RightNear]:=fPlanes[TFrustumSide.Right].Normal.Cross(fPlanes[TFrustumSide.Near_].Normal);
 Crosses[RightFar]:=fPlanes[TFrustumSide.Right].Normal.Cross(fPlanes[TFrustumSide.Far_].Normal);
 Crosses[BottomTop]:=fPlanes[TFrustumSide.Bottom].Normal.Cross(fPlanes[TFrustumSide.Top].Normal);
 Crosses[BottomNear]:=fPlanes[TFrustumSide.Bottom].Normal.Cross(fPlanes[TFrustumSide.Near_].Normal);
 Crosses[BottomFar]:=fPlanes[TFrustumSide.Bottom].Normal.Cross(fPlanes[TFrustumSide.Far_].Normal);
 Crosses[TopNear]:=fPlanes[TFrustumSide.Top].Normal.Cross(fPlanes[TFrustumSide.Near_].Normal);
 Crosses[TopFar]:=fPlanes[TFrustumSide.Top].Normal.Cross(fPlanes[TFrustumSide.Far_].Normal);
 Crosses[NearFar]:=fPlanes[TFrustumSide.Near_].Normal.Cross(fPlanes[TFrustumSide.Far_].Normal);

 for FrustumSide:=Low(TFrustumSide) to High(TFrustumSide) do begin
  fAbsoluteNormals[FrustumSide]:=fPlanes[FrustumSide].Normal.Abs;
 end;

 fWorldSpaceCorners[0]:=((Crosses[BottomNear]*fPlanes[TFrustumSide.Left].Distance)+
                         (Crosses[LeftNear]*-fPlanes[TFrustumSide.Bottom].Distance)+
                         (Crosses[LeftBottom]*fPlanes[TFrustumSide.Near_].Distance))/
                        (-(fPlanes[TFrustumSide.Left].Normal.Dot(Crosses[BottomNear])));

 fWorldSpaceCorners[1]:=((Crosses[TopNear]*fPlanes[TFrustumSide.Left].Distance)+
                         (Crosses[LeftNear]*fPlanes[TFrustumSide.Top].Distance)+
                         (Crosses[LeftTop]*fPlanes[TFrustumSide.Near_].Distance))/
                        (-(fPlanes[TFrustumSide.Left].Normal.Dot(Crosses[TopNear])));

 fWorldSpaceCorners[2]:=((Crosses[BottomNear]*fPlanes[TFrustumSide.Right].Distance)+
                         (Crosses[RightNear]*-fPlanes[TFrustumSide.Bottom].Distance)+
                         (Crosses[RightBottom]*fPlanes[TFrustumSide.Near_].Distance))/
                        (-(fPlanes[TFrustumSide.Right].Normal.Dot(Crosses[BottomNear])));

 fWorldSpaceCorners[3]:=((Crosses[TopNear]*fPlanes[TFrustumSide.Right].Distance)+
                         (Crosses[RightNear]*-fPlanes[TFrustumSide.Top].Distance)+
                         (Crosses[RightTop]*fPlanes[TFrustumSide.Near_].Distance))/
                        (-(fPlanes[TFrustumSide.Right].Normal.Dot(Crosses[TopNear])));

 if TpvFrustum.TFlag.InfiniteFarPlane in fFlags then begin

  fWorldSpaceCorners[4]:=TpvVector3.Origin;

  fWorldSpaceCorners[5]:=TpvVector3.Origin;

  fWorldSpaceCorners[6]:=TpvVector3.Origin;

  fWorldSpaceCorners[7]:=TpvVector3.Origin;

 end else begin

  fWorldSpaceCorners[4]:=((Crosses[BottomFar]*fPlanes[TFrustumSide.Left].Distance)+
                          (Crosses[LeftFar]*-fPlanes[TFrustumSide.Bottom].Distance)+
                          (Crosses[LeftBottom]*fPlanes[TFrustumSide.Far_].Distance))/
                         (-(fPlanes[TFrustumSide.Left].Normal.Dot(Crosses[BottomFar])));

  fWorldSpaceCorners[5]:=((Crosses[TopFar]*fPlanes[TFrustumSide.Left].Distance)+
                          (Crosses[LeftFar]*-fPlanes[TFrustumSide.Top].Distance)+
                          (Crosses[LeftTop]*fPlanes[TFrustumSide.Far_].Distance))/
                         (-(fPlanes[TFrustumSide.Left].Normal.Dot(Crosses[TopFar])));

  fWorldSpaceCorners[6]:=((Crosses[BottomFar]*fPlanes[TFrustumSide.Right].Distance)+
                          (Crosses[RightFar]*-fPlanes[TFrustumSide.Bottom].Distance)+
                          (Crosses[RightBottom]*fPlanes[TFrustumSide.Far_].Distance))/
                         (-(fPlanes[TFrustumSide.Right].Normal.Dot(Crosses[BottomFar])));

  fWorldSpaceCorners[7]:=((Crosses[TopFar]*fPlanes[TFrustumSide.Right].Distance)+
                          (Crosses[RightFar]*-fPlanes[TFrustumSide.Top].Distance)+
                          (Crosses[RightTop]*fPlanes[TFrustumSide.Far_].Distance))/
                         (-(fPlanes[TFrustumSide.Right].Normal.Dot(Crosses[TopFar])));

 end;

end;

class function TpvFrustum.ExtractFrustumSphere(const aZNear,aZFar,aFOV,aAspectRatio:TpvScalar;const aPosition,aDirection:TpvVector3):TpvSphere;
var ViewLen,Width,Height:TpvScalar;
begin
 ViewLen:=aZFar-aZNear;
 Height:=ViewLen*tan(aFOV*0.5);
 Width:=Height*aAspectRatio;
 result.Radius:=TpvVector3.Create(Width,Height,ViewLen).DistanceTo(TpvVector3.Create(0.0,0.0,aZNear+(ViewLen*0.5)));
 result.Center:=aPosition+(aDirection*((ViewLen*0.5)+aZNear));
end;

function TpvFrustum.AABBInFrustum(const aAABBMin,aAABBMax:TpvVector3):TpvInt32;
var FrustumSide:TFrustumSide;
    DistanceFromCenter,PlaneAbsoluteNormalDotExtents:TpvScalar;
    Center,Extents:TpvVector3;
begin
 Center:=(aAABBMin+aAABBMax)*0.5;
 Extents:=(aAABBMax-aAABBMin)*0.5;
 result:=COMPLETE_IN;
 for FrustumSide:=Low(TFrustumSide) to fMaximumPlaneSide do begin
  DistanceFromCenter:=fPlanes[FrustumSide].DistanceTo(Center);
  PlaneAbsoluteNormalDotExtents:=fAbsoluteNormals[FrustumSide].Dot(Extents);
  if (DistanceFromCenter+PlaneAbsoluteNormalDotExtents)<0.0 then begin
   result:=COMPLETE_OUT;
   exit;
  end else if (DistanceFromCenter-PlaneAbsoluteNormalDotExtents)<0.0 then begin
   result:=PARTIALLY_IN;
  end;
 end;
 if (not (TpvFrustum.TFlag.InfiniteFarPlane in fFlags)) and
    (((fWorldSpaceCorners[0].x<aAABBMin.x) and
      (fWorldSpaceCorners[1].x<aAABBMin.x) and
      (fWorldSpaceCorners[2].x<aAABBMin.x) and
      (fWorldSpaceCorners[3].x<aAABBMin.x) and
      (fWorldSpaceCorners[4].x<aAABBMin.x) and
      (fWorldSpaceCorners[5].x<aAABBMin.x) and
      (fWorldSpaceCorners[6].x<aAABBMin.x) and
      (fWorldSpaceCorners[7].x<aAABBMin.x)) or
     ((fWorldSpaceCorners[0].x>aAABBMax.x) and
      (fWorldSpaceCorners[1].x>aAABBMax.x) and
      (fWorldSpaceCorners[2].x>aAABBMax.x) and
      (fWorldSpaceCorners[3].x>aAABBMax.x) and
      (fWorldSpaceCorners[4].x>aAABBMax.x) and
      (fWorldSpaceCorners[5].x>aAABBMax.x) and
      (fWorldSpaceCorners[6].x>aAABBMax.x) and
      (fWorldSpaceCorners[7].x>aAABBMax.x)) or
     ((fWorldSpaceCorners[0].y<aAABBMin.y) and
      (fWorldSpaceCorners[1].y<aAABBMin.y) and
      (fWorldSpaceCorners[2].y<aAABBMin.y) and
      (fWorldSpaceCorners[3].y<aAABBMin.y) and
      (fWorldSpaceCorners[4].y<aAABBMin.y) and
      (fWorldSpaceCorners[5].y<aAABBMin.y) and
      (fWorldSpaceCorners[6].y<aAABBMin.y) and
      (fWorldSpaceCorners[7].y<aAABBMin.y)) or
     ((fWorldSpaceCorners[0].y>aAABBMax.y) and
      (fWorldSpaceCorners[1].y>aAABBMax.y) and
      (fWorldSpaceCorners[2].y>aAABBMax.y) and
      (fWorldSpaceCorners[3].y>aAABBMax.y) and
      (fWorldSpaceCorners[4].y>aAABBMax.y) and
      (fWorldSpaceCorners[5].y>aAABBMax.y) and
      (fWorldSpaceCorners[6].y>aAABBMax.y) and
      (fWorldSpaceCorners[7].y>aAABBMax.y)) or
     ((fWorldSpaceCorners[0].z<aAABBMin.z) and
      (fWorldSpaceCorners[1].z<aAABBMin.z) and
      (fWorldSpaceCorners[2].z<aAABBMin.z) and
      (fWorldSpaceCorners[3].z<aAABBMin.z) and
      (fWorldSpaceCorners[4].z<aAABBMin.z) and
      (fWorldSpaceCorners[5].z<aAABBMin.z) and
      (fWorldSpaceCorners[6].z<aAABBMin.z) and
      (fWorldSpaceCorners[7].z<aAABBMin.z)) or
     ((fWorldSpaceCorners[0].z>aAABBMax.z) and
      (fWorldSpaceCorners[1].z>aAABBMax.z) and
      (fWorldSpaceCorners[2].z>aAABBMax.z) and
      (fWorldSpaceCorners[3].z>aAABBMax.z) and
      (fWorldSpaceCorners[4].z>aAABBMax.z) and
      (fWorldSpaceCorners[5].z>aAABBMax.z) and
      (fWorldSpaceCorners[6].z>aAABBMax.z) and
      (fWorldSpaceCorners[7].z>aAABBMax.z))) then begin
  result:=COMPLETE_OUT;
  exit;
 end;
end;

function TpvFrustum.AABBInFrustum(const aAABBMin,aAABBMax:TpvVector3;var aMask:TpvUInt32):TpvInt32;
var FrustumSide:TFrustumSide;
    Bit,InMask,OutMask:TpvUInt32;
    DistanceFromCenter,PlaneAbsoluteNormalDotExtents:TpvScalar;
    Center,Extents:TpvVector3;
begin
 InMask:=aMask;
 if (InMask and TpvUInt32($80000000))<>0 then begin
  Center:=(aAABBMin+aAABBMax)*0.5;
  Extents:=(aAABBMax-aAABBMin)*0.5;
  OutMask:=$40000000;
  result:=COMPLETE_IN;
  InMask:=InMask and TpvUInt32($3f);
  repeat
   Bit:=TPasMPMath.BitScanForward32(InMask);
   FrustumSide:=TFrustumSide(Bit);
   DistanceFromCenter:=fPlanes[FrustumSide].DistanceTo(Center);
   PlaneAbsoluteNormalDotExtents:=fAbsoluteNormals[FrustumSide].Dot(Extents);
   if (DistanceFromCenter+PlaneAbsoluteNormalDotExtents)<0.0 then begin
    aMask:=0;
    result:=COMPLETE_OUT;
    exit;
   end else if (DistanceFromCenter-PlaneAbsoluteNormalDotExtents)<0.0 then begin
    OutMask:=OutMask or ((TpvUInt32(1) shl Bit) or TpvUInt32($80000000));
    result:=PARTIALLY_IN;
   end;
   InMask:=InMask and (InMask-1);
  until (InMask=0) or (FrustumSide=fMaximumPlaneSide);
  aMask:=OutMask;
 end else begin
  if (InMask and TpvUInt32($40000000))<>0 then begin
   result:=COMPLETE_IN;
  end else begin
   aMask:=0;
   result:=COMPLETE_OUT;
  end;
 end;
end;

function TpvFrustum.AABBInFrustum(const aAABB:TpvAABB):TpvInt32;
begin
 result:=AABBInFrustum(aAABB.Min,aAABB.Max);
end;

function TpvFrustum.AABBInFrustum(const aAABB:TpvAABB;var aMask:TpvUInt32):TpvInt32;
begin
 result:=AABBInFrustum(aAABB.Min,aAABB.Max,aMask);
end;

function TpvFrustum.SphereInFrustum(const aSphereCenter:TpvVector3;const aSphereRadius:TpvScalar;const aRadius:TpvScalar=0.0):TpvInt32; overload;
var FrustumSide:TFrustumSide;
    Count:TpvSizeInt;
    Distance,Radius:TpvScalar;
begin
 Count:=0;
 Radius:=aSphereRadius+aRadius;
 for FrustumSide:=Low(TFrustumSide) to fMaximumPlaneSide do begin
  Distance:=fPlanes[FrustumSide].DistanceTo(aSphereCenter);
  if Distance<=(-Radius) then begin
   result:=COMPLETE_OUT;
   exit;
  end else if Distance>Radius then begin
   inc(Count);
  end;
 end;
 if Count=6 then begin
  result:=COMPLETE_IN;
 end else begin
  result:=PARTIALLY_IN;
 end;
end;

function TpvFrustum.SphereInFrustum(const aSphere:TpvSphere;const aRadius:TpvScalar=0.0):TpvInt32;
var FrustumSide:TFrustumSide;
    Count:TpvSizeInt;
    Distance,Radius:TpvScalar;
begin
 Count:=0;
 Radius:=aSphere.Radius+aRadius;
 for FrustumSide:=Low(TFrustumSide) to fMaximumPlaneSide do begin
  Distance:=fPlanes[FrustumSide].DistanceTo(aSphere.Center);
  if Distance<=(-Radius) then begin
   result:=COMPLETE_OUT;
   exit;
  end else if Distance>Radius then begin
   inc(Count);
  end;
 end;
 if Count=6 then begin
  result:=COMPLETE_IN;
 end else begin
  result:=PARTIALLY_IN;
 end;
end;

function TpvFrustum.PointInFrustum(const aPoint:TpvVector3):boolean;
var FrustumSide:TFrustumSide;
begin
 result:=true;
 for FrustumSide:=Low(TFrustumSide) to fMaximumPlaneSide do begin
  if fPlanes[FrustumSide].DistanceTo(aPoint)<=0.0 then begin
   result:=false;
   break;
  end;
 end;
end;

end.

