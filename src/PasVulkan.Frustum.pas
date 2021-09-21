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
     TpvFrustum=record
      public
       const COMPLETE_OUT=0;
             PARTIALLY_IN=1;
             COMPLETE_IN=2;
       type TFrustumSide=
             (
              Left,
              Right,
              Top,
              Bottom,
              Near_,
              Far_
             );
            TPlanes=array[TFrustumSide] of TpvPlane;
            TAbsoluteNormals=array[TFrustumSide] of TpvVector3;
            TCorners=array[0..7] of TpvVector3;
      private
       fPlanes:TPlanes;
       fAbsoluteNormals:TAbsoluteNormals;
      public
       procedure Init(const aViewMatrix,aProjectionMatrix:TpvMatrix4x4);
       class function ExtractFrustumSphere(const aZNear,aZFar,aFOV,aAspectRatio:TpvScalar;const aPosition,aDirection:TpvVector3):TpvSphere; static;
       function AABBInFrustum(const aAABB:TpvAABB):TpvInt32; overload;
       function AABBInFrustum(const aAABB:TpvAABB;var aMask:TpvUInt32):TpvInt32; overload;
       function SphereInFrustum(const aSphere:TpvSphere;const aRadius:TpvScalar=0.0):TpvInt32;
       function PointInFrustum(const aPoint:TpvVector3):boolean;
      public
       property Planes:TPlanes read fPlanes write fPlanes;
       property AbsoluteNormals:TAbsoluteNormals read fAbsoluteNormals write fAbsoluteNormals;
     end;

     PpvFrustum=^TpvFrustum;

     TpvFrustumDynamicArray=array of TpvFrustum;

implementation

function IntersectionPoint(const a,b,c:TpvPlane):TpvVector3;
begin
 result:=((b.Normal.Cross(c.Normal)*a.Distance)+
          (c.Normal.Cross(a.Normal)*b.Distance)+
          (a.Normal.Cross(b.Normal)*c.Distance))/(-a.Normal.Dot(b.Normal.Cross(c.Normal)));
end;

procedure TpvFrustum.Init(const aViewMatrix,aProjectionMatrix:TpvMatrix4x4);
var FrustumSide:TFrustumSide;
    ViewProjectionMatrix:TpvMatrix4x4;
begin

 ViewProjectionMatrix:=aViewMatrix*aProjectionMatrix;

 fPlanes[TFrustumSide.Left]:=TpvPlane.Create(ViewProjectionMatrix.Rows[3]+ViewProjectionMatrix.Rows[0]).Normalize;
 fPlanes[TFrustumSide.Right]:=TpvPlane.Create(ViewProjectionMatrix.Rows[3]-ViewProjectionMatrix.Rows[0]).Normalize;
 fPlanes[TFrustumSide.Top]:=TpvPlane.Create(ViewProjectionMatrix.Rows[3]-ViewProjectionMatrix.Rows[1]).Normalize;
 fPlanes[TFrustumSide.Bottom]:=TpvPlane.Create(ViewProjectionMatrix.Rows[3]+ViewProjectionMatrix.Rows[1]).Normalize;
 fPlanes[TFrustumSide.Near_]:=TpvPlane.Create(ViewProjectionMatrix.Rows[2]+ViewProjectionMatrix.Rows[2]).Normalize;
 fPlanes[TFrustumSide.Far_]:=TpvPlane.Create(ViewProjectionMatrix.Rows[3]-ViewProjectionMatrix.Rows[2]).Normalize;

 for FrustumSide:=Low(TFrustumSide) to High(TFrustumSide) do begin
  fAbsoluteNormals[FrustumSide]:=fPlanes[FrustumSide].Normal.Abs;
 end;

{fWorldSpaceCorners[0]:=IntersectionPoint(fPlanes[TFrustumSide.Near_],fPlanes[TFrustumSide.Left],fPlanes[TFrustumSide.Top]);
 fWorldSpaceCorners[1]:=IntersectionPoint(fPlanes[TFrustumSide.Near_],fPlanes[TFrustumSide.Right],fPlanes[TFrustumSide.Top]);
 fWorldSpaceCorners[2]:=IntersectionPoint(fPlanes[TFrustumSide.Near_],fPlanes[TFrustumSide.Right],fPlanes[TFrustumSide.Bottom]);
 fWorldSpaceCorners[3]:=IntersectionPoint(fPlanes[TFrustumSide.Near_],fPlanes[TFrustumSide.Left],fPlanes[TFrustumSide.Bottom]);
 fWorldSpaceCorners[4]:=IntersectionPoint(fPlanes[TFrustumSide.Far_],fPlanes[TFrustumSide.Left],fPlanes[TFrustumSide.Top]);
 fWorldSpaceCorners[5]:=IntersectionPoint(fPlanes[TFrustumSide.Far_],fPlanes[TFrustumSide.Right],fPlanes[TFrustumSide.Top]);
 fWorldSpaceCorners[6]:=IntersectionPoint(fPlanes[TFrustumSide.Far_],fPlanes[TFrustumSide.Right],fPlanes[TFrustumSide.Bottom]);
 fWorldSpaceCorners[7]:=IntersectionPoint(fPlanes[TFrustumSide.Far_],fPlanes[TFrustumSide.Left],fPlanes[TFrustumSide.Bottom]);}

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

function TpvFrustum.AABBInFrustum(const aAABB:TpvAABB):TpvInt32;
var FrustumSide:TFrustumSide;
    DistanceFromCenter,PlaneAbsoluteNormalDotExtents:TpvScalar;
    Center,Extents:TpvVector3;
begin
 Center:=(aAABB.Min+aAABB.Max)*0.5;
 Extents:=(aAABB.Max-aAABB.Min)*0.5;
 result:=COMPLETE_IN;
 for FrustumSide:=Low(TFrustumSide) to High(TFrustumSide) do begin
  DistanceFromCenter:=fPlanes[FrustumSide].DistanceTo(Center);
  PlaneAbsoluteNormalDotExtents:=fAbsoluteNormals[FrustumSide].Dot(Extents);
  if (DistanceFromCenter+PlaneAbsoluteNormalDotExtents)<0.0 then begin
   result:=COMPLETE_OUT;
   exit;
  end else if (DistanceFromCenter-PlaneAbsoluteNormalDotExtents)<0.0 then begin
   result:=PARTIALLY_IN;
  end;
 end;
end;

function TpvFrustum.AABBInFrustum(const aAABB:TpvAABB;var aMask:TpvUInt32):TpvInt32;
var FrustumSide:TFrustumSide;
    Bit,InMask,OutMask:TpvUInt32;
    DistanceFromCenter,PlaneAbsoluteNormalDotExtents:TpvScalar;
    Center,Extents:TpvVector3;
begin
 Center:=(aAABB.Min+aAABB.Max)*0.5;
 Extents:=(aAABB.Max-aAABB.Min)*0.5;
 result:=COMPLETE_IN;
 InMask:=aMask;
 OutMask:=$40000000;
 Bit:=1;
 for FrustumSide:=Low(TFrustumSide) to High(TFrustumSide) do begin
  if (InMask and Bit)<>0 then begin
   DistanceFromCenter:=fPlanes[FrustumSide].DistanceTo(Center);
   PlaneAbsoluteNormalDotExtents:=fAbsoluteNormals[FrustumSide].Dot(Extents);
   if (DistanceFromCenter+PlaneAbsoluteNormalDotExtents)<0.0 then begin
    aMask:=0;
    result:=COMPLETE_OUT;
    exit;
   end else if (DistanceFromCenter-PlaneAbsoluteNormalDotExtents)<0.0 then begin
    OutMask:=OutMask or (Bit or $80000000);
    result:=PARTIALLY_IN;
   end;
  end;
  inc(Bit,Bit);
 end;
 aMask:=OutMask;
end;

function TpvFrustum.SphereInFrustum(const aSphere:TpvSphere;const aRadius:TpvScalar=0.0):TpvInt32;
var FrustumSide:TFrustumSide;
    Count:TpvSizeInt;
    Distance,Radius:TpvScalar;
begin
 Count:=0;
 Radius:=aSphere.Radius+aRadius;
 for FrustumSide:=Low(TFrustumSide) to High(TFrustumSide) do begin
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
 for FrustumSide:=Low(TFrustumSide) to High(TFrustumSide) do begin
  if fPlanes[FrustumSide].DistanceTo(aPoint)<=0.0 then begin
   result:=false;
   break;
  end;
 end;
end;

end.

