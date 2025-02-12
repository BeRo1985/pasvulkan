(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2024, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.Math.Double;
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
 {$ifndef fpc}
//  {$undef SIMD} // Due to inline assembler bugs in Delphi
 {$endif}
{$endif}

{$if defined(cpux64) and defined(Windows)}
 {$define ExplicitX64SIMDRegs}
{$ifend}

{$warnings off}

interface

uses SysUtils,
     Classes,
     Math,
     PasVulkan.Types,
     Vulkan,
     PasVulkan.Math;

type TpvVector2D=record
      public
       x,y:TpvDouble;
       constructor Create(const aFrom:TpvVector2); overload;
       constructor Create(const aX,aY:TpvDouble); overload;
       class operator Equal(const aLeft,aRight:TpvVector2D):boolean;
       class operator NotEqual(const aLeft,aRight:TpvVector2D):boolean;
       class operator Add(const aLeft,aRight:TpvVector2D):TpvVector2D;
       class operator Subtract(const aLeft,aRight:TpvVector2D):TpvVector2D;
       class operator Multiply(const aLeft:TpvVector2D;const aRight:TpvDouble):TpvVector2D;
       class operator Divide(const aLeft:TpvVector2D;const aRight:TpvDouble):TpvVector2D;
       class operator Negative(const aVector:TpvVector2D):TpvVector2D;
       function Dot(const aWith:TpvVector2D):TpvDouble;
       function Length:TpvDouble;
       function SquaredLength:TpvDouble;
       function Normalize:TpvVector2D;
       function Lerp(const aWith:TpvVector2D;const aTime:TpvDouble):TpvVector2D;
       function Nlerp(const aWith:TpvVector2D;const aTime:TpvDouble):TpvVector2D;
       function Slerp(const aWith:TpvVector2D;const aTime:TpvDouble):TpvVector2D;
       function Sqlerp(const aB,aC,aD:TpvVector2D;const aTime:TpvDouble):TpvVector2D;
       function ToVector:TpvVector2;
     end;
     PpvVector2D=^TpvVector2D;

     TpvVector3D=record
      public
       constructor Create(const aFrom:TpvVector3); overload;
       constructor Create(const aX,aY,aZ:TpvDouble); overload;
       class operator Equal(const aLeft,aRight:TpvVector3D):boolean;
       class operator NotEqual(const aLeft,aRight:TpvVector3D):boolean;
       class operator Add(const aLeft,aRight:TpvVector3D):TpvVector3D;
       class operator Subtract(const aLeft,aRight:TpvVector3D):TpvVector3D;
       class operator Multiply(const aLeft:TpvVector3D;const aRight:TpvDouble):TpvVector3D;
       class operator Divide(const aLeft:TpvVector3D;const aRight:TpvDouble):TpvVector3D;
       class operator Negative(const aVector:TpvVector3D):TpvVector3D;
       function Cross(const aWith:TpvVector3D):TpvVector3D;
       function Spacing(const aWith:TpvVector3D):TpvDouble;
       function Dot(const aWith:TpvVector3D):TpvDouble;
       function Length:TpvDouble;
       function SquaredLength:TpvDouble;
       function Normalize:TpvVector3D;
       function Perpendicular:TpvVector3D;
       function Lerp(const aWith:TpvVector3D;const aTime:TpvDouble):TpvVector3D;
       function Nlerp(const aWith:TpvVector3D;const aTime:TpvDouble):TpvVector3D;
       function Slerp(const aWith:TpvVector3D;const aTime:TpvDouble):TpvVector3D;
       function Sqlerp(const aB,aC,aD:TpvVector3D;const aTime:TpvDouble):TpvVector3D;
       function ToVector:TpvVector3;
      public
       case TpvInt32 of
        0:(x,y,z:TpvDouble);
        1:(xy:TpvVector2D);
     end;
     PpvVector3D=^TpvVector3D;

     TpvVector4D=record
      public
       constructor Create(const aFrom:TpvVector4); overload;
       constructor Create(const aX,aY,aZ,aW:TpvDouble); overload;
       class operator Equal(const aLeft,aRight:TpvVector4D):boolean;
       class operator NotEqual(const aLeft,aRight:TpvVector4D):boolean;
       class operator Add(const aLeft,aRight:TpvVector4D):TpvVector4D;
       class operator Subtract(const aLeft,aRight:TpvVector4D):TpvVector4D;
       class operator Multiply(const aLeft:TpvVector4D;const aRight:TpvDouble):TpvVector4D;
       class operator Divide(const aLeft:TpvVector4D;const aRight:TpvDouble):TpvVector4D;
       class operator Negative(const aVector:TpvVector4D):TpvVector4D;
       function Dot(const aWith:TpvVector4D):TpvDouble;
       function Length:TpvDouble;
       function SquaredLength:TpvDouble;
       function Normalize:TpvVector4D;
       function Lerp(const aWith:TpvVector4D;const aTime:TpvDouble):TpvVector4D;
       function Nlerp(const aWith:TpvVector4D;const aTime:TpvDouble):TpvVector4D;
       function Slerp(const aWith:TpvVector4D;const aTime:TpvDouble):TpvVector4D;
       function Sqlerp(const aB,aC,aD:TpvVector4D;const aTime:TpvDouble):TpvVector4D;
       function ToVector:TpvVector4;
      public
       case TpvInt32 of
        0:(x,y,z,w:TpvDouble);
        1:(xyz:TpvVector3D);
        2:(xy:TpvVector2D);
     end;
     PpvVector4D=^TpvVector4D;

     TpvQuaternionD=record
      public
       constructor Create(const aFrom:TpvQuaternion); overload;
       constructor Create(const aX,aY,aZ,aW:TpvDouble); overload;
       constructor Create(const aMatrix:TpvMatrix3x3); overload;
       constructor Create(const aMatrix:TpvMatrix4x4); overload;
       class operator Equal(const aLeft,aRight:TpvQuaternionD):boolean;
       class operator NotEqual(const aLeft,aRight:TpvQuaternionD):boolean;
       class operator Add(const aLeft,aRight:TpvQuaternionD):TpvQuaternionD;
       class operator Subtract(const aLeft,aRight:TpvQuaternionD):TpvQuaternionD;
       class operator Multiply(const aLeft,aRight:TpvQuaternionD):TpvQuaternionD;
       class operator Multiply(const aLeft:TpvQuaternionD;const aRight:TpvDouble):TpvQuaternionD;
       class operator Multiply(const aLeft:TpvDouble;const aRight:TpvQuaternionD):TpvQuaternionD;
       class operator Divide(const aLeft,aRight:TpvQuaternionD):TpvQuaternionD;
       class operator Negative(const aQuaternion:TpvQuaternionD):TpvQuaternionD;
       function Dot(const aWith:TpvQuaternionD):TpvDouble;
       function Length:TpvDouble;
       function SquaredLength:TpvDouble;
       function Normalize:TpvQuaternionD;
       function Conjugate:TpvQuaternionD;
       function Inverse:TpvQuaternionD;
       function Log:TpvQuaternionD;
       function Exp:TpvQuaternionD; 
       function Lerp(const aWith:TpvQuaternionD;const aTime:TpvDouble):TpvQuaternionD;
       function Nlerp(const aWith:TpvQuaternionD;const aTime:TpvDouble):TpvQuaternionD;
       function Slerp(const aWith:TpvQuaternionD;const aTime:TpvDouble):TpvQuaternionD;
       function ApproximatedSlerp(const aWith:TpvQuaternionD;const aTime:TpvDouble):TpvQuaternionD;
       function Elerp(const aWith:TpvQuaternionD;const aTime:TpvDouble):TpvQuaternionD;
       function Sqlerp(const aB,aC,aD:TpvQuaternionD;const aTime:TpvDouble):TpvQuaternionD;
       function ToMatrix3x3:TpvMatrix3x3;
       function ToMatrix4x4:TpvMatrix4x4;
       function ToVector:TpvVector4;
       function ToQuaternion:TpvQuaternion;
      public
       case TpvInt32 of
        0:(x,y,z,w:TpvDouble);
        1:(Vector:TpvVector4D);
        2:(xyz:TpvVector3D);
        3:(xy:TpvVector2D);
     end;
     PpvQuaternionD=^TpvQuaternionD;

     TpvDecomposedMatrix3x3D=record
      public
       Valid:boolean;
       Scale:TpvVector3D;
       Skew:TpvVector3D; // XY XZ YZ
       Rotation:TpvQuaternionD;
       class function Create:TpvDecomposedMatrix3x3D; static;
       function Lerp(const aWith:TpvDecomposedMatrix3x3D;const aTime:TpvDouble):TpvDecomposedMatrix3x3D; 
       function Nlerp(const aWith:TpvDecomposedMatrix3x3D;const aTime:TpvDouble):TpvDecomposedMatrix3x3D;
       function Slerp(const aWith:TpvDecomposedMatrix3x3D;const aTime:TpvDouble):TpvDecomposedMatrix3x3D;
       function Elerp(const aWith:TpvDecomposedMatrix3x3D;const aTime:TpvDouble):TpvDecomposedMatrix3x3D;
       function Sqlerp(const aB,aC,aD:TpvDecomposedMatrix3x3D;const aTime:TpvDouble):TpvDecomposedMatrix3x3D;
     end;      
     PpvDecomposedMatrix3x3D=^TpvDecomposedMatrix3x3D;

     TpvMatrix3x3D=record
      public
       constructor Create(const aXX,aXY,aXZ,aYX,aYY,aYZ,aZX,aZY,aZZ:TpvDouble); overload;
       constructor Create(const aFrom:TpvMatrix3x3); overload;
       constructor Create(const aFrom:TpvMatrix4x4); overload;
       constructor Create(const aFrom:TpvQuaternion); overload;
       constructor Create(const aFrom:TpvQuaternionD); overload;
       constructor Create(const aFrom:TpvDecomposedMatrix3x3D); overload;
       class operator Equal(const aLeft,aRight:TpvMatrix3x3D):boolean;
       class operator NotEqual(const aLeft,aRight:TpvMatrix3x3D):boolean;
       class operator Add(const aLeft,aRight:TpvMatrix3x3D):TpvMatrix3x3D;
       class operator Subtract(const aLeft,aRight:TpvMatrix3x3D):TpvMatrix3x3D;
       class operator Multiply(const aLeft,aRight:TpvMatrix3x3D):TpvMatrix3x3D;
       class operator Multiply(const aLeft:TpvMatrix3x3D;const aRight:TpvDouble):TpvMatrix3x3D;
       class operator Multiply(const aLeft:TpvDouble;const aRight:TpvMatrix3x3D):TpvMatrix3x3D;
       class operator Multiply(const aLeft:TpvMatrix3x3D;const aRight:TpvVector3D):TpvVector3D;
       class operator Multiply(const aLeft:TpvVector3D;const aRight:TpvMatrix3x3D):TpvVector3D;
       class operator Multiply(const aLeft:TpvMatrix3x3D;const aRight:TpvVector4D):TpvVector4D;
       class operator Multiply(const aLeft:TpvVector4D;const aRight:TpvMatrix3x3D):TpvVector4D;
       class operator Divide(const aLeft,aRight:TpvMatrix3x3D):TpvMatrix3x3D;
       class operator Divide(const aLeft:TpvMatrix3x3D;const aRight:TpvDouble):TpvMatrix3x3D;
       class operator Negative(const aMatrix:TpvMatrix3x3D):TpvMatrix3x3D;
       function Transpose:TpvMatrix3x3D;
       function Determinant:TpvDouble;
       function Inverse:TpvMatrix3x3D;
       function ToMatrix3x3:TpvMatrix3x3;       
       function ToQuaternionD:TpvQuaternionD;
       function Decompose:TpvDecomposedMatrix3x3D;
       function Lerp(const aWith:TpvMatrix3x3D;const aTime:TpvDouble):TpvMatrix3x3D;
       function Nlerp(const aWith:TpvMatrix3x3D;const aTime:TpvDouble):TpvMatrix3x3D;
       function Slerp(const aWith:TpvMatrix3x3D;const aTime:TpvDouble):TpvMatrix3x3D;
       function Elerp(const aWith:TpvMatrix3x3D;const aTime:TpvDouble):TpvMatrix3x3D;
       function Sqlerp(const aB,aC,aD:TpvMatrix3x3D;const aTime:TpvDouble):TpvMatrix3x3D;
      public
       case TpvInt32 of
        0:(RawComponents:array[0..2,0..2] of TpvDouble);
        1:(Columns:array[0..2] of TpvVector3D);
        2:(Right,Up,Forwards:TpvVector3D);
        3:(Tangent,Bitangent,Normal:TpvVector3D);
     end;

     TpvDecomposedMatrix4x4D=record
      public
       Valid:boolean;
       Perspective:TpvVector4D;
       Translation:TpvVector3D;
       Scale:TpvVector3D;
       Skew:TpvVector3D; // XY XZ YZ
       Rotation:TpvQuaternionD;
       class function Create:TpvDecomposedMatrix4x4D; static;
       function Lerp(const aWith:TpvDecomposedMatrix4x4D;const aTime:TpvDouble):TpvDecomposedMatrix4x4D; 
       function Nlerp(const aWith:TpvDecomposedMatrix4x4D;const aTime:TpvDouble):TpvDecomposedMatrix4x4D;
       function Slerp(const aWith:TpvDecomposedMatrix4x4D;const aTime:TpvDouble):TpvDecomposedMatrix4x4D;
       function Elerp(const aWith:TpvDecomposedMatrix4x4D;const aTime:TpvDouble):TpvDecomposedMatrix4x4D;
       function Sqlerp(const aB,aC,aD:TpvDecomposedMatrix4x4D;const aTime:TpvDouble):TpvDecomposedMatrix4x4D;
     end;     
     PpvDecomposedMatrix4x4D=^TpvDecomposedMatrix4x4D;

implementation

{ TpvVector2D }

constructor TpvVector2D.Create(const aFrom:TpvVector2);
begin
 x:=aFrom.x;
 y:=aFrom.y;
end;

constructor TpvVector2D.Create(const aX,aY:TpvDouble);
begin
 x:=aX;
 y:=aY;
end;

class operator TpvVector2D.Equal(const aLeft,aRight:TpvVector2D):boolean;
begin
 result:=SameValue(aLeft.x,aRight.x) and
         SameValue(aLeft.y,aRight.y);
end;

class operator TpvVector2D.NotEqual(const aLeft,aRight:TpvVector2D):boolean;
begin
 result:=not (SameValue(aLeft.x,aRight.x) and
              SameValue(aLeft.y,aRight.y));
end;

class operator TpvVector2D.Add(const aLeft,aRight:TpvVector2D):TpvVector2D;
begin
 result.x:=aLeft.x+aRight.x;
 result.y:=aLeft.y+aRight.y;
end;

class operator TpvVector2D.Subtract(const aLeft,aRight:TpvVector2D):TpvVector2D;
begin
 result.x:=aLeft.x-aRight.x;
 result.y:=aLeft.y-aRight.y;
end;

class operator TpvVector2D.Multiply(const aLeft:TpvVector2D;const aRight:TpvDouble):TpvVector2D;
begin
 result.x:=aLeft.x*aRight;
 result.y:=aLeft.y*aRight;
end;

class operator TpvVector2D.Divide(const aLeft:TpvVector2D;const aRight:TpvDouble):TpvVector2D;
begin
 result.x:=aLeft.x/aRight;
 result.y:=aLeft.y/aRight;
end;

class operator TpvVector2D.Negative(const aVector:TpvVector2D):TpvVector2D;
begin
 result.x:=-aVector.x;
 result.y:=-aVector.y;
end;

function TpvVector2D.Dot(const aWith:TpvVector2D):TpvDouble;
begin
 result:=(x*aWith.x)+(y*aWith.y);
end;

function TpvVector2D.Length:TpvDouble;
begin
 result:=sqrt(sqr(x)+sqr(y));
end;

function TpvVector2D.SquaredLength:TpvDouble;
begin
 result:=sqr(x)+sqr(y);
end;

function TpvVector2D.Normalize:TpvVector2D;
var l:TpvDouble;
begin
 l:=Length;
 if l>0.0 then begin
  result:=self/l;
 end else begin
  result:=self;
 end; 
end;

function TpvVector2D.Lerp(const aWith:TpvVector2D;const aTime:TpvDouble):TpvVector2D;
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

function TpvVector2D.Nlerp(const aWith:TpvVector2D;const aTime:TpvDouble):TpvVector2D;
begin
 result:=Lerp(aWith,aTime).Normalize;
end;

function TpvVector2D.Slerp(const aWith:TpvVector2D;const aTime:TpvDouble):TpvVector2D;
var DotProduct,Theta,Sinus,Cosinus:TpvDouble;
begin
 if aTime<=0.0 then begin
  result:=self;
 end else if aTime>=1.0 then begin
  result:=aWith;
 end else if self=aWith then begin
  result:=aWith;
 end else begin
  DotProduct:=Dot(aWith);
  if DotProduct<-1.0 then begin
   DotProduct:=-1.0;
  end else if DotProduct>1.0 then begin
   DotProduct:=1.0;
  end;
  Theta:=ArcCos(DotProduct)*aTime;
  SinCos(Theta,Sinus,Cosinus);
  result:=(self*Cosinus)+((aWith-(self*DotProduct)).Normalize*Sinus);
 end;
end;

function TpvVector2D.Sqlerp(const aB,aC,aD:TpvVector2D;const aTime:TpvDouble):TpvVector2D;
begin
 result:=Slerp(aD,aTime).Slerp(aB.Slerp(aC,aTime),(2.0*aTime)*(1.0-aTime));
end;

function TpvVector2D.ToVector:TpvVector2;
begin
 result.x:=x;
 result.y:=y;
end;

{ TpvVector3D }

constructor TpvVector3D.Create(const aFrom:TpvVector3);
begin
 x:=aFrom.x;
 y:=aFrom.y;
 z:=aFrom.z;
end;

constructor TpvVector3D.Create(const aX,aY,aZ:TpvDouble);
begin
 x:=aX;
 y:=aY;
 z:=aZ;
end;

class operator TpvVector3D.Equal(const aLeft,aRight:TpvVector3D):boolean;
begin
 result:=SameValue(aLeft.x,aRight.x) and
         SameValue(aLeft.y,aRight.y) and
         SameValue(aLeft.z,aRight.z);
end;

class operator TpvVector3D.NotEqual(const aLeft,aRight:TpvVector3D):boolean;
begin
 result:=not (SameValue(aLeft.x,aRight.x) and
              SameValue(aLeft.y,aRight.y) and
              SameValue(aLeft.z,aRight.z));
end;

class operator TpvVector3D.Add(const aLeft,aRight:TpvVector3D):TpvVector3D;
begin
 result.x:=aLeft.x+aRight.x;
 result.y:=aLeft.y+aRight.y;
 result.z:=aLeft.z+aRight.z;
end;

class operator TpvVector3D.Subtract(const aLeft,aRight:TpvVector3D):TpvVector3D;
begin
 result.x:=aLeft.x-aRight.x;
 result.y:=aLeft.y-aRight.y;
 result.z:=aLeft.z-aRight.z;
end;

class operator TpvVector3D.Multiply(const aLeft:TpvVector3D;const aRight:TpvDouble):TpvVector3D;
begin
 result.x:=aLeft.x*aRight;
 result.y:=aLeft.y*aRight;
 result.z:=aLeft.z*aRight;
end;

class operator TpvVector3D.Divide(const aLeft:TpvVector3D;const aRight:TpvDouble):TpvVector3D;
begin
 result.x:=aLeft.x/aRight;
 result.y:=aLeft.y/aRight;
 result.z:=aLeft.z/aRight;
end;

class operator TpvVector3D.Negative(const aVector:TpvVector3D):TpvVector3D;
begin
 result.x:=-aVector.x;
 result.y:=-aVector.y;
 result.z:=-aVector.z;
end;

function TpvVector3D.Cross(const aWith:TpvVector3D):TpvVector3D;
begin
 result.x:=(y*aWith.z)-(z*aWith.y);
 result.y:=(z*aWith.x)-(x*aWith.z);
 result.z:=(x*aWith.y)-(y*aWith.x);
end;

function TpvVector3D.Spacing(const aWith:TpvVector3D):TpvDouble;
begin
 result:=abs(x-aWith.x)+abs(y-aWith.y)+abs(z-aWith.z);
end;

function TpvVector3D.Dot(const aWith:TpvVector3D):TpvDouble;
begin
 result:=(x*aWith.x)+(y*aWith.y)+(z*aWith.z);
end;

function TpvVector3D.Length:TpvDouble;
begin
 result:=sqrt(sqr(x)+sqr(y)+sqr(z));
end;

function TpvVector3D.SquaredLength:TpvDouble;
begin
 result:=sqr(x)+sqr(y)+sqr(z);
end;

function TpvVector3D.Normalize:TpvVector3D;
begin
 result:=self/Length;
end;

function TpvVector3D.Perpendicular:TpvVector3D;
var v,p:TpvVector3D;
begin
 v:=self.Normalize;
 p.x:=System.abs(v.x);
 p.y:=System.abs(v.y);
 p.z:=System.abs(v.z);
 if (p.x<=p.y) and (p.x<=p.z) then begin
  p.x:=1.0;
  p.y:=0.0;
  p.z:=0.0;
 end else if (p.y<=p.x) and (p.y<=p.z) then begin
  p.x:=0.0;
  p.y:=1.0;
  p.z:=0.0;
 end else begin
  p.x:=0.0;
  p.y:=0.0;
  p.z:=1.0;
 end;
 result:=p-(v*v.Dot(p));
end;

function TpvVector3D.Lerp(const aWith:TpvVector3D;const aTime:TpvDouble):TpvVector3D;
begin
 if aTime<=0.0 then begin
  result:=self;
 end else if aTime>=1.0 then begin
  result:=aWith;
 end else begin
  result:=(self*(1.0-aTime))+(aWith*aTime);
 end;
end;

function TpvVector3D.Nlerp(const aWith:TpvVector3D;const aTime:TpvDouble):TpvVector3D;
begin
 result:=Lerp(aWith,aTime).Normalize;
end;

function TpvVector3D.Slerp(const aWith:TpvVector3D;const aTime:TpvDouble):TpvVector3D;
var DotProduct,Theta,Sinus,Cosinus:TpvDouble;
begin
 if aTime<=0.0 then begin
  result:=self;
 end else if aTime>=1.0 then begin
  result:=aWith;
 end else if self=aWith then begin
  result:=aWith;
 end else begin
  DotProduct:=Dot(aWith);
  if DotProduct<-1.0 then begin
   DotProduct:=-1.0;
  end else if DotProduct>1.0 then begin
   DotProduct:=1.0;
  end;
  Theta:=ArcCos(DotProduct)*aTime;
  SinCos(Theta,Sinus,Cosinus);
  result:=(self*Cosinus)+((aWith-(self*DotProduct)).Normalize*Sinus);
 end;
end;

function TpvVector3D.Sqlerp(const aB,aC,aD:TpvVector3D;const aTime:TpvDouble):TpvVector3D;
begin
 result:=Slerp(aD,aTime).Slerp(aB.Slerp(aC,aTime),(2.0*aTime)*(1.0-aTime));
end;

function TpvVector3D.ToVector:TpvVector3;
begin
 result.x:=x;
 result.y:=y;
 result.z:=z;
end;

{ TpvVector4D }

constructor TpvVector4D.Create(const aFrom:TpvVector4);
begin
 x:=aFrom.x;
 y:=aFrom.y;
 z:=aFrom.z;
 w:=aFrom.w;
end;

constructor TpvVector4D.Create(const aX,aY,aZ,aW:TpvDouble);
begin
 x:=aX;
 y:=aY;
 z:=aZ;
 w:=aW;
end;

class operator TpvVector4D.Equal(const aLeft,aRight:TpvVector4D):boolean;
begin
 result:=SameValue(aLeft.x,aRight.x) and
         SameValue(aLeft.y,aRight.y) and
         SameValue(aLeft.z,aRight.z) and
         SameValue(aLeft.w,aRight.w);
end;

class operator TpvVector4D.NotEqual(const aLeft,aRight:TpvVector4D):boolean;
begin
 result:=not (SameValue(aLeft.x,aRight.x) and
              SameValue(aLeft.y,aRight.y) and
              SameValue(aLeft.z,aRight.z) and
              SameValue(aLeft.w,aRight.w));
end;

class operator TpvVector4D.Add(const aLeft,aRight:TpvVector4D):TpvVector4D;
begin
 result.x:=aLeft.x+aRight.x;
 result.y:=aLeft.y+aRight.y;
 result.z:=aLeft.z+aRight.z;
 result.w:=aLeft.w+aRight.w;
end;

class operator TpvVector4D.Subtract(const aLeft,aRight:TpvVector4D):TpvVector4D;
begin
 result.x:=aLeft.x-aRight.x;
 result.y:=aLeft.y-aRight.y;
 result.z:=aLeft.z-aRight.z;
 result.w:=aLeft.w-aRight.w;
end;

class operator TpvVector4D.Multiply(const aLeft:TpvVector4D;const aRight:TpvDouble):TpvVector4D;
begin
 result.x:=aLeft.x*aRight;
 result.y:=aLeft.y*aRight;
 result.z:=aLeft.z*aRight;
 result.w:=aLeft.w*aRight;
end;

class operator TpvVector4D.Divide(const aLeft:TpvVector4D;const aRight:TpvDouble):TpvVector4D;
begin
 result.x:=aLeft.x/aRight;
 result.y:=aLeft.y/aRight;
 result.z:=aLeft.z/aRight;
 result.w:=aLeft.w/aRight;
end;

class operator TpvVector4D.Negative(const aVector:TpvVector4D):TpvVector4D;
begin
 result.x:=-aVector.x;
 result.y:=-aVector.y;
 result.z:=-aVector.z;
 result.w:=-aVector.w;
end;

function TpvVector4D.Dot(const aWith:TpvVector4D):TpvDouble;
begin
 result:=(x*aWith.x)+(y*aWith.y)+(z*aWith.z)+(w*aWith.w);
end;

function TpvVector4D.Length:TpvDouble;
begin
 result:=sqrt(sqr(x)+sqr(y)+sqr(z)+sqr(w));
end;

function TpvVector4D.SquaredLength:TpvDouble;
begin
 result:=sqr(x)+sqr(y)+sqr(z)+sqr(w);
end;

function TpvVector4D.Normalize:TpvVector4D;
var l:TpvDouble;
begin
 l:=Length;
 if l>0.0 then begin
  result:=self/l;
 end else begin
  result:=self;
 end;
end;

function TpvVector4D.Lerp(const aWith:TpvVector4D;const aTime:TpvDouble):TpvVector4D;
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

function TpvVector4D.Nlerp(const aWith:TpvVector4D;const aTime:TpvDouble):TpvVector4D;
begin
 result:=Lerp(aWith,aTime).Normalize;
end;

function TpvVector4D.Slerp(const aWith:TpvVector4D;const aTime:TpvDouble):TpvVector4D;
var DotProduct,Theta,Sinus,Cosinus:TpvDouble;
begin
 if aTime<=0.0 then begin
  result:=self;
 end else if aTime>=1.0 then begin
  result:=aWith;
 end else if self=aWith then begin
  result:=aWith;
 end else begin
  DotProduct:=Dot(aWith);
  if DotProduct<-1.0 then begin
   DotProduct:=-1.0;
  end else if DotProduct>1.0 then begin
   DotProduct:=1.0;
  end;
  Theta:=ArcCos(DotProduct)*aTime;
  SinCos(Theta,Sinus,Cosinus);
  result:=(self*Cosinus)+((aWith-(self*DotProduct)).Normalize*Sinus);
 end;
end;

function TpvVector4D.Sqlerp(const aB,aC,aD:TpvVector4D;const aTime:TpvDouble):TpvVector4D;
begin
 result:=Slerp(aD,aTime).Slerp(aB.Slerp(aC,aTime),(2.0*aTime)*(1.0-aTime));
end;

function TpvVector4D.ToVector:TpvVector4;
begin
 result.x:=x;
 result.y:=y;
 result.z:=z;
 result.w:=w;
end;

{ TpvQuaternionD }

constructor TpvQuaternionD.Create(const aFrom:TpvQuaternion);
begin
 x:=aFrom.x;
 y:=aFrom.y;
 z:=aFrom.z;
 w:=aFrom.w;
end;

constructor TpvQuaternionD.Create(const aX,aY,aZ,aW:TpvDouble);
begin
 x:=aX;
 y:=aY;
 z:=aZ;
 w:=aW;
end;

constructor TpvQuaternionD.Create(const aMatrix:TpvMatrix3x3);
var t,s:TpvDouble;
begin
 t:=aMatrix.RawComponents[0,0]+(aMatrix.RawComponents[1,1]+aMatrix.RawComponents[2,2]);
 if t>2.9999999 then begin
  x:=0.0;
  y:=0.0;
  z:=0.0;
  w:=1.0;
 end else if t>0.0000001 then begin
  s:=sqrt(1.0+t)*2.0;
  x:=(aMatrix.RawComponents[1,2]-aMatrix.RawComponents[2,1])/s;
  y:=(aMatrix.RawComponents[2,0]-aMatrix.RawComponents[0,2])/s;
  z:=(aMatrix.RawComponents[0,1]-aMatrix.RawComponents[1,0])/s;
  w:=s*0.25;
 end else if (aMatrix.RawComponents[0,0]>aMatrix.RawComponents[1,1]) and (aMatrix.RawComponents[0,0]>aMatrix.RawComponents[2,2]) then begin
  s:=sqrt(1.0+(aMatrix.RawComponents[0,0]-(aMatrix.RawComponents[1,1]+aMatrix.RawComponents[2,2])))*2.0;
  x:=s*0.25;
  y:=(aMatrix.RawComponents[1,0]+aMatrix.RawComponents[0,1])/s;
  z:=(aMatrix.RawComponents[2,0]+aMatrix.RawComponents[0,2])/s;
  w:=(aMatrix.RawComponents[1,2]-aMatrix.RawComponents[2,1])/s;
 end else if aMatrix.RawComponents[1,1]>aMatrix.RawComponents[2,2] then begin
  s:=sqrt(1.0+(aMatrix.RawComponents[1,1]-(aMatrix.RawComponents[0,0]+aMatrix.RawComponents[2,2])))*2.0;
  x:=(aMatrix.RawComponents[1,0]+aMatrix.RawComponents[0,1])/s;
  y:=s*0.25;
  z:=(aMatrix.RawComponents[2,1]+aMatrix.RawComponents[1,2])/s;
  w:=(aMatrix.RawComponents[2,0]-aMatrix.RawComponents[0,2])/s;
 end else begin
  s:=sqrt(1.0+(aMatrix.RawComponents[2,2]-(aMatrix.RawComponents[0,0]+aMatrix.RawComponents[1,1])))*2.0;
  x:=(aMatrix.RawComponents[2,0]+aMatrix.RawComponents[0,2])/s;
  y:=(aMatrix.RawComponents[2,1]+aMatrix.RawComponents[1,2])/s;
  z:=s*0.25;
  w:=(aMatrix.RawComponents[0,1]-aMatrix.RawComponents[1,0])/s;
 end;
 t:=sqrt(sqr(x)+sqr(y)+sqr(z)+sqr(w));
 if t>0.0 then begin
  x:=x/t;
  y:=y/t;
  z:=z/t;
  w:=w/t;
 end;
end;

constructor TpvQuaternionD.Create(const aMatrix:TpvMatrix4x4);
var t,s:TpvDouble;
begin
 t:=aMatrix.RawComponents[0,0]+(aMatrix.RawComponents[1,1]+aMatrix.RawComponents[2,2]);
 if t>2.9999999 then begin
  x:=0.0;
  y:=0.0;
  z:=0.0;
  w:=1.0;
 end else if t>0.0000001 then begin
  s:=sqrt(1.0+t)*2.0;
  x:=(aMatrix.RawComponents[1,2]-aMatrix.RawComponents[2,1])/s;
  y:=(aMatrix.RawComponents[2,0]-aMatrix.RawComponents[0,2])/s;
  z:=(aMatrix.RawComponents[0,1]-aMatrix.RawComponents[1,0])/s;
  w:=s*0.25;
 end else if (aMatrix.RawComponents[0,0]>aMatrix.RawComponents[1,1]) and (aMatrix.RawComponents[0,0]>aMatrix.RawComponents[2,2]) then begin
  s:=sqrt(1.0+(aMatrix.RawComponents[0,0]-(aMatrix.RawComponents[1,1]+aMatrix.RawComponents[2,2])))*2.0;
  x:=s*0.25;
  y:=(aMatrix.RawComponents[1,0]+aMatrix.RawComponents[0,1])/s;
  z:=(aMatrix.RawComponents[2,0]+aMatrix.RawComponents[0,2])/s;
  w:=(aMatrix.RawComponents[1,2]-aMatrix.RawComponents[2,1])/s;
 end else if aMatrix.RawComponents[1,1]>aMatrix.RawComponents[2,2] then begin
  s:=sqrt(1.0+(aMatrix.RawComponents[1,1]-(aMatrix.RawComponents[0,0]+aMatrix.RawComponents[2,2])))*2.0;
  x:=(aMatrix.RawComponents[1,0]+aMatrix.RawComponents[0,1])/s;
  y:=s*0.25;
  z:=(aMatrix.RawComponents[2,1]+aMatrix.RawComponents[1,2])/s;
  w:=(aMatrix.RawComponents[2,0]-aMatrix.RawComponents[0,2])/s;
 end else begin
  s:=sqrt(1.0+(aMatrix.RawComponents[2,2]-(aMatrix.RawComponents[0,0]+aMatrix.RawComponents[1,1])))*2.0;
  x:=(aMatrix.RawComponents[2,0]+aMatrix.RawComponents[0,2])/s;
  y:=(aMatrix.RawComponents[2,1]+aMatrix.RawComponents[1,2])/s;
  z:=s*0.25;
  w:=(aMatrix.RawComponents[0,1]-aMatrix.RawComponents[1,0])/s;
 end;
 t:=sqrt(sqr(x)+sqr(y)+sqr(z)+sqr(w));
 if t>0.0 then begin
  x:=x/t;
  y:=y/t;
  z:=z/t;
  w:=w/t;
 end;
end;

class operator TpvQuaternionD.Equal(const aLeft,aRight:TpvQuaternionD):boolean;
begin
 result:=SameValue(aLeft.x,aRight.x) and
         SameValue(aLeft.y,aRight.y) and
         SameValue(aLeft.z,aRight.z) and
         SameValue(aLeft.w,aRight.w);
end;

class operator TpvQuaternionD.NotEqual(const aLeft,aRight:TpvQuaternionD):boolean;
begin
 result:=not (SameValue(aLeft.x,aRight.x) and
              SameValue(aLeft.y,aRight.y) and
              SameValue(aLeft.z,aRight.z) and
              SameValue(aLeft.w,aRight.w));
end;

class operator TpvQuaternionD.Add(const aLeft,aRight:TpvQuaternionD):TpvQuaternionD;
begin
 result.x:=aLeft.x+aRight.x;
 result.y:=aLeft.y+aRight.y;
 result.z:=aLeft.z+aRight.z;
 result.w:=aLeft.w+aRight.w;
end;

class operator TpvQuaternionD.Subtract(const aLeft,aRight:TpvQuaternionD):TpvQuaternionD;
begin
 result.x:=aLeft.x-aRight.x;
 result.y:=aLeft.y-aRight.y;
 result.z:=aLeft.z-aRight.z;
 result.w:=aLeft.w-aRight.w;
end;

class operator TpvQuaternionD.Multiply(const aLeft,aRight:TpvQuaternionD):TpvQuaternionD;
begin
 result.x:=(((aLeft.w*aRight.x)+(aLeft.x*aRight.w))+(aLeft.y*aRight.z))-(aLeft.z*aRight.y);
 result.y:=(((aLeft.w*aRight.y)-(aLeft.x*aRight.z))+(aLeft.y*aRight.w))+(aLeft.z*aRight.x);
 result.z:=(((aLeft.w*aRight.z)+(aLeft.x*aRight.y))-(aLeft.y*aRight.x))+(aLeft.z*aRight.w);
 result.w:=(((aLeft.w*aRight.w)-(aLeft.x*aRight.x))-(aLeft.y*aRight.y))-(aLeft.z*aRight.z);
end;

class operator TpvQuaternionD.Multiply(const aLeft:TpvQuaternionD;const aRight:TpvDouble):TpvQuaternionD;
begin
 result.x:=aLeft.x*aRight;
 result.y:=aLeft.y*aRight;
 result.z:=aLeft.z*aRight;
 result.w:=aLeft.w*aRight;
end;

class operator TpvQuaternionD.Multiply(const aLeft:TpvDouble;const aRight:TpvQuaternionD):TpvQuaternionD;
begin
 result.x:=aLeft*aRight.x;
 result.y:=aLeft*aRight.y;
 result.z:=aLeft*aRight.z;
 result.w:=aLeft*aRight.w;
end;

class operator TpvQuaternionD.Divide(const aLeft,aRight:TpvQuaternionD):TpvQuaternionD;
begin
 result:=aLeft*aRight.Inverse;
end;

class operator TpvQuaternionD.Negative(const aQuaternion:TpvQuaternionD):TpvQuaternionD;
begin
 result.x:=-aQuaternion.x;
 result.y:=-aQuaternion.y;
 result.z:=-aQuaternion.z;
 result.w:=-aQuaternion.w;
end;

function TpvQuaternionD.Dot(const aWith:TpvQuaternionD):TpvDouble;
begin
 result:=(x*aWith.x)+(y*aWith.y)+(z*aWith.z)+(w*aWith.w);
end;

function TpvQuaternionD.Length:TpvDouble;
begin
 result:=sqrt(sqr(x)+sqr(y)+sqr(z)+sqr(w));
end;

function TpvQuaternionD.SquaredLength:TpvDouble;
begin
 result:=sqr(x)+sqr(y)+sqr(z)+sqr(w);
end;

function TpvQuaternionD.Normalize:TpvQuaternionD;
var InverseLength:TpvDouble;
begin
 InverseLength:=1.0/Length;
 result.x:=x*InverseLength;
 result.y:=y*InverseLength;
 result.z:=z*InverseLength;
 result.w:=w*InverseLength;
end;

function TpvQuaternionD.Conjugate:TpvQuaternionD;
begin
 result.x:=-x;
 result.y:=-y;
 result.z:=-z;
 result.w:=w;
end;

function TpvQuaternionD.Inverse:TpvQuaternionD;
var InverseLength:TpvDouble;
begin
 InverseLength:=1.0/SquaredLength;
 result.x:=-x*InverseLength;
 result.y:=-y*InverseLength;
 result.z:=-z*InverseLength;
 result.w:=w*InverseLength;
end;

function TpvQuaternionD.Exp:TpvQuaternionD;
var Angle,Sinus,Coefficent:TpvDouble;
begin
 Angle:=sqrt(sqr(x)+sqr(y)+sqr(z));
 SinCos(Angle,Sinus,Coefficent);
 result.w:=cos(Angle);
 if System.Abs(Sinus)>1e-6 then begin
  Coefficent:=Sinus/Angle;
  result.x:=x*Coefficent;
  result.y:=y*Coefficent;
  result.z:=z*Coefficent;
 end else begin
  result.x:=x;
  result.y:=y;
  result.z:=z;
 end;
end;

function TpvQuaternionD.Log:TpvQuaternionD;
var Theta,SinTheta,Coefficent:TpvDouble;
begin
 result.x:=x;
 result.y:=y;
 result.z:=z;
 result.w:=0.0;
 if System.Abs(w)<1.0 then begin
  Theta:=ArcCos(w);
  SinCos(Theta,SinTheta,Coefficent);
  if System.Abs(SinTheta)>1e-6 then begin
   Coefficent:=Theta/SinTheta;
   result.x:=result.x*Coefficent;
   result.y:=result.y*Coefficent;
   result.z:=result.z*Coefficent;
  end;
 end;
end;

function TpvQuaternionD.Lerp(const aWith:TpvQuaternionD;const aTime:TpvDouble):TpvQuaternionD;
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

function TpvQuaternionD.Nlerp(const aWith:TpvQuaternionD;const aTime:TpvDouble):TpvQuaternionD;
begin
 result:=Lerp(aWith,aTime).Normalize;
end;

function TpvQuaternionD.Slerp(const aWith:TpvQuaternionD;const aTime:TpvDouble):TpvQuaternionD;
var Omega,co,so,s0,s1,s2:TpvDouble;
begin
 co:=(x*aWith.x)+(y*aWith.y)+(z*aWith.z)+(w*aWith.w);
 if co<0.0 then begin
  co:=-co;
  s2:=-1.0;
 end else begin
  s2:=1.0;
 end;
 if (1.0-co)>EPSILON then begin
  Omega:=ArcCos(co);
  so:=sin(Omega);
  s0:=sin((1.0-aTime)*Omega)/so;
  s1:=sin(aTime*Omega)/so;
 end else begin
  s0:=1.0-aTime;
  s1:=aTime;
 end;
 result:=(s0*self)+(aWith*(s1*s2));
end;

function TpvQuaternionD.ApproximatedSlerp(const aWith:TpvQuaternionD;const aTime:TpvDouble):TpvQuaternionD;
var ca,d,a,b,k,o:TpvDouble;
begin
 ca:=(x*aWith.x)+(y*aWith.y)+(z*aWith.z)+(w*aWith.w);
 d:=abs(ca);
 a:=1.0904+(d*(-3.2452+(d*(3.55645-(d*1.43519)))));
 b:=0.848013+(d*(-1.06021+(d*0.215638)));
 k:=(a*sqr(aTime-0.5))+b;
 o:=aTime+(((aTime*(aTime-0.5))*(aTime-1.0))*k);
 if ca<0.0 then begin
  result:=Nlerp(-aWith,o);
 end else begin
  result:=Nlerp(aWith,o);
 end;
end;

function TpvQuaternionD.Elerp(const aWith:TpvQuaternionD;const aTime:TpvDouble):TpvQuaternionD;
var SignFactor:TpvDouble;
begin
 if Dot(aWith)<0.0 then begin
  SignFactor:=-1.0;
 end else begin
  SignFactor:=1.0;
 end;
 if aTime<=0.0 then begin
  result:=self;
 end else if aTime>=1.0 then begin
  result:=aWith*SignFactor;
 end else begin
  result:=((Log*(1.0-aTime))+((aWith*SignFactor).Log*aTime)).Exp;
 end;
end;

function TpvQuaternionD.Sqlerp(const aB,aC,aD:TpvQuaternionD;const aTime:TpvDouble):TpvQuaternionD;
begin
 result:=Slerp(aD,aTime).Slerp(aB.Slerp(aC,aTime),(2.0*aTime)*(1.0-aTime));
end;


function TpvQuaternionD.ToMatrix3x3:TpvMatrix3x3;
var qx2,qy2,qz2,qxqx2,qxqy2,qxqz2,qxqw2,qyqy2,qyqz2,qyqw2,qzqz2,qzqw2:TpvDouble;
    Quaternion:TpvQuaternionD;
begin 
 Quaternion:=self.Normalize;
 qx2:=Quaternion.x+Quaternion.x;
 qy2:=Quaternion.y+Quaternion.y;
 qz2:=Quaternion.z+Quaternion.z;
 qxqx2:=Quaternion.x*qx2;
 qxqy2:=Quaternion.x*qy2;
 qxqz2:=Quaternion.x*qz2;
 qxqw2:=Quaternion.w*qx2;
 qyqy2:=Quaternion.y*qy2;
 qyqz2:=Quaternion.y*qz2;
 qyqw2:=Quaternion.w*qy2;
 qzqz2:=Quaternion.z*qz2;
 qzqw2:=Quaternion.w*qz2;
 result.RawComponents[0,0]:=1.0-(qyqy2+qzqz2);
 result.RawComponents[0,1]:=qxqy2+qzqw2;
 result.RawComponents[0,2]:=qxqz2-qyqw2;
 result.RawComponents[1,0]:=qxqy2-qzqw2;
 result.RawComponents[1,1]:=1.0-(qxqx2+qzqz2);
 result.RawComponents[1,2]:=qyqz2+qxqw2;
 result.RawComponents[2,0]:=qxqz2+qyqw2;
 result.RawComponents[2,1]:=qyqz2-qxqw2;
 result.RawComponents[2,2]:=1.0-(qxqx2+qyqy2);
end;

function TpvQuaternionD.ToMatrix4x4:TpvMatrix4x4;
var qx2,qy2,qz2,qxqx2,qxqy2,qxqz2,qxqw2,qyqy2,qyqz2,qyqw2,qzqz2,qzqw2:TpvDouble;
    Quaternion:TpvQuaternionD;
begin
 Quaternion:=self.Normalize;
 qx2:=Quaternion.x+Quaternion.x;
 qy2:=Quaternion.y+Quaternion.y;
 qz2:=Quaternion.z+Quaternion.z;
 qxqx2:=Quaternion.x*qx2;
 qxqy2:=Quaternion.x*qy2;
 qxqz2:=Quaternion.x*qz2;
 qxqw2:=Quaternion.w*qx2;
 qyqy2:=Quaternion.y*qy2;
 qyqz2:=Quaternion.y*qz2;
 qyqw2:=Quaternion.w*qy2;
 qzqz2:=Quaternion.z*qz2;
 qzqw2:=Quaternion.w*qz2;
 result.RawComponents[0,0]:=1.0-(qyqy2+qzqz2);
 result.RawComponents[0,1]:=qxqy2+qzqw2;
 result.RawComponents[0,2]:=qxqz2-qyqw2;
 result.RawComponents[0,3]:=0.0;
 result.RawComponents[1,0]:=qxqy2-qzqw2;
 result.RawComponents[1,1]:=1.0-(qxqx2+qzqz2);
 result.RawComponents[1,2]:=qyqz2+qxqw2;
 result.RawComponents[1,3]:=0.0;
 result.RawComponents[2,0]:=qxqz2+qyqw2;
 result.RawComponents[2,1]:=qyqz2-qxqw2;
 result.RawComponents[2,2]:=1.0-(qxqx2+qyqy2);
 result.RawComponents[2,3]:=0.0;
 result.RawComponents[3,0]:=0.0;
 result.RawComponents[3,1]:=0.0;
 result.RawComponents[3,2]:=0.0;
 result.RawComponents[3,3]:=1.0;
end;

function TpvQuaternionD.ToVector:TpvVector4;
begin
 result.x:=x;
 result.y:=y;
 result.z:=z;
 result.w:=w;
end;

function TpvQuaternionD.ToQuaternion:TpvQuaternion;
begin
 result.x:=x;
 result.y:=y;
 result.z:=z;
 result.w:=w;
end;

class function TpvDecomposedMatrix3x3D.Create:TpvDecomposedMatrix3x3D;
begin
 result.Scale:=TpvVector3D.Create(1.0,1.0,1.0);
 result.Skew:=TpvVector3D.Create(0.0,0.0,0.0);
 result.Rotation:=TpvQuaternionD.Create(0.0,0.0,0.0,1.0);
 result.Valid:=true;
end;

function TpvDecomposedMatrix3x3D.Lerp(const aWith:TpvDecomposedMatrix3x3D;const aTime:TpvDouble):TpvDecomposedMatrix3x3D;
begin
 if aTime<=0.0 then begin
  result:=self;
 end else if aTime>=1.0 then begin
  result:=aWith;
 end else begin
  result.Scale:=Scale.Lerp(aWith.Scale,aTime);
  result.Skew:=Skew.Lerp(aWith.Skew,aTime);
  result.Rotation:=Rotation.Lerp(aWith.Rotation,aTime);
 end;
end;

function TpvDecomposedMatrix3x3D.Nlerp(const aWith:TpvDecomposedMatrix3x3D;const aTime:TpvDouble):TpvDecomposedMatrix3x3D;
begin
 if aTime<=0.0 then begin
  result:=self;
 end else if aTime>=1.0 then begin
  result:=aWith;
 end else begin
  result.Scale:=Scale.Lerp(aWith.Scale,aTime);
  result.Skew:=Skew.Lerp(aWith.Skew,aTime);
  result.Rotation:=Rotation.Nlerp(aWith.Rotation,aTime);
 end;
end;

function TpvDecomposedMatrix3x3D.Slerp(const aWith:TpvDecomposedMatrix3x3D;const aTime:TpvDouble):TpvDecomposedMatrix3x3D;
begin
 if aTime<=0.0 then begin
  result:=self;
 end else if aTime>=1.0 then begin
  result:=aWith;
 end else begin
  result.Scale:=Scale.Lerp(aWith.Scale,aTime);
  result.Skew:=Skew.Lerp(aWith.Skew,aTime);
  result.Rotation:=Rotation.Slerp(aWith.Rotation,aTime);
 end;
end;

function TpvDecomposedMatrix3x3D.Elerp(const aWith:TpvDecomposedMatrix3x3D;const aTime:TpvDouble):TpvDecomposedMatrix3x3D;
begin
 if aTime<=0.0 then begin
  result:=self;
 end else if aTime>=1.0 then begin
  result:=aWith;
 end else begin
  result.Scale:=Scale.Lerp(aWith.Scale,aTime);
  result.Skew:=Skew.Lerp(aWith.Skew,aTime);
  result.Rotation:=Rotation.Elerp(aWith.Rotation,aTime);
 end;
end;

function TpvDecomposedMatrix3x3D.Sqlerp(const aB,aC,aD:TpvDecomposedMatrix3x3D;const aTime:TpvDouble):TpvDecomposedMatrix3x3D;
begin
 result:=Slerp(aD,aTime).Slerp(aB.Slerp(aC,aTime),(2.0*aTime)*(1.0-aTime));
end;

constructor TpvMatrix3x3D.Create(const aXX,aXY,aXZ,aYX,aYY,aYZ,aZX,aZY,aZZ:TpvDouble);
begin
 RawComponents[0,0]:=aXX;
 RawComponents[0,1]:=aXY;
 RawComponents[0,2]:=aXZ;
 RawComponents[1,0]:=aYX;
 RawComponents[1,1]:=aYY;
 RawComponents[1,2]:=aYZ;
 RawComponents[2,0]:=aZX;
 RawComponents[2,1]:=aZY;
 RawComponents[2,2]:=aZZ;
end;

constructor TpvMatrix3x3D.Create(const aFrom:TpvMatrix3x3);
begin
 RawComponents[0,0]:=aFrom.RawComponents[0,0];
 RawComponents[0,1]:=aFrom.RawComponents[0,1];
 RawComponents[0,2]:=aFrom.RawComponents[0,2];
 RawComponents[1,0]:=aFrom.RawComponents[1,0];
 RawComponents[1,1]:=aFrom.RawComponents[1,1];
 RawComponents[1,2]:=aFrom.RawComponents[1,2];
 RawComponents[2,0]:=aFrom.RawComponents[2,0];
 RawComponents[2,1]:=aFrom.RawComponents[2,1];
 RawComponents[2,2]:=aFrom.RawComponents[2,2];
end;

constructor TpvMatrix3x3D.Create(const aFrom:TpvMatrix4x4);
begin
 RawComponents[0,0]:=aFrom.RawComponents[0,0];
 RawComponents[0,1]:=aFrom.RawComponents[0,1];
 RawComponents[0,2]:=aFrom.RawComponents[0,2];
 RawComponents[1,0]:=aFrom.RawComponents[1,0];
 RawComponents[1,1]:=aFrom.RawComponents[1,1];
 RawComponents[1,2]:=aFrom.RawComponents[1,2];
 RawComponents[2,0]:=aFrom.RawComponents[2,0];
 RawComponents[2,1]:=aFrom.RawComponents[2,1];
 RawComponents[2,2]:=aFrom.RawComponents[2,2];
end;

constructor TpvMatrix3x3D.Create(const aFrom:TpvQuaternion);
var qx2,qy2,qz2,qxqx2,qxqy2,qxqz2,qxqw2,qyqy2,qyqz2,qyqw2,qzqz2,qzqw2:TpvDouble;
    From:TpvQuaternion;
begin
 From:=aFrom.Normalize;
 qx2:=From.x+From.x;
 qy2:=From.y+From.y;
 qz2:=From.z+From.z;
 qxqx2:=From.x*qx2;
 qxqy2:=From.x*qy2;
 qxqz2:=From.x*qz2;
 qxqw2:=From.w*qx2;
 qyqy2:=From.y*qy2;
 qyqz2:=From.y*qz2;
 qyqw2:=From.w*qy2;
 qzqz2:=From.z*qz2;
 qzqw2:=From.w*qz2;
 RawComponents[0,0]:=1.0-(qyqy2+qzqz2);
 RawComponents[0,1]:=qxqy2+qzqw2;
 RawComponents[0,2]:=qxqz2-qyqw2;
 RawComponents[1,0]:=qxqy2-qzqw2;
 RawComponents[1,1]:=1.0-(qxqx2+qzqz2);
 RawComponents[1,2]:=qyqz2+qxqw2;
 RawComponents[2,0]:=qxqz2+qyqw2;
 RawComponents[2,1]:=qyqz2-qxqw2;
 RawComponents[2,2]:=1.0-(qxqx2+qyqy2);
end;

constructor TpvMatrix3x3D.Create(const aFrom:TpvQuaternionD);
var qx2,qy2,qz2,qxqx2,qxqy2,qxqz2,qxqw2,qyqy2,qyqz2,qyqw2,qzqz2,qzqw2:TpvDouble;
    From:TpvQuaternionD;
begin
 From:=aFrom.Normalize;
 qx2:=From.x+From.x;
 qy2:=From.y+From.y;
 qz2:=From.z+From.z;
 qxqx2:=From.x*qx2;
 qxqy2:=From.x*qy2;
 qxqz2:=From.x*qz2;
 qxqw2:=From.w*qx2;
 qyqy2:=From.y*qy2;
 qyqz2:=From.y*qz2;
 qyqw2:=From.w*qy2;
 qzqz2:=From.z*qz2;
 qzqw2:=From.w*qz2;
 RawComponents[0,0]:=1.0-(qyqy2+qzqz2);
 RawComponents[0,1]:=qxqy2+qzqw2;
 RawComponents[0,2]:=qxqz2-qyqw2;
 RawComponents[1,0]:=qxqy2-qzqw2;
 RawComponents[1,1]:=1.0-(qxqx2+qzqz2);
 RawComponents[1,2]:=qyqz2+qxqw2;
 RawComponents[2,0]:=qxqz2+qyqw2;
 RawComponents[2,1]:=qyqz2-qxqw2;
 RawComponents[2,2]:=1.0-(qxqx2+qyqy2);
end;

constructor TpvMatrix3x3D.Create(const aFrom:TpvDecomposedMatrix3x3D);
begin
 self:=TpvMatrix3x3D.Create(aFrom.Rotation);
 if aFrom.Skew.z<>0.0 then begin // YZ
  self:=TpvMatrix3x3D.Create(1.0,0.0,0.0,
                             0.0,1.0,0.0,
                             0.0,aFrom.Skew.z,1.0)*self;
 end;
 if aFrom.Skew.y<>0.0 then begin // XZ
  self:=TpvMatrix3x3D.Create(1.0,0.0,0.0,
                             0.0,1.0,0.0,
                             aFrom.Skew.y,0.0,1.0)*self;
 end;
 if aFrom.Skew.x<>0.0 then begin // XY
  self:=TpvMatrix3x3D.Create(1.0,0.0,0.0,
                             aFrom.Skew.x,1.0,0.0,
                             0.0,0.0,1.0)*self;
 end;
 self:=TpvMatrix3x3D.Create(aFrom.Scale.x,0.0,0.0,
                            0.0,aFrom.Scale.y,0.0,
                            0.0,0.0,aFrom.Scale.z)*self;
end;

class operator TpvMatrix3x3D.Equal(const aLeft,aRight:TpvMatrix3x3D):boolean;
begin
 result:=(aLeft.RawComponents[0,0]=aRight.RawComponents[0,0]) and
         (aLeft.RawComponents[0,1]=aRight.RawComponents[0,1]) and
         (aLeft.RawComponents[0,2]=aRight.RawComponents[0,2]) and
         (aLeft.RawComponents[1,0]=aRight.RawComponents[1,0]) and
         (aLeft.RawComponents[1,1]=aRight.RawComponents[1,1]) and
         (aLeft.RawComponents[1,2]=aRight.RawComponents[1,2]) and
         (aLeft.RawComponents[2,0]=aRight.RawComponents[2,0]) and
         (aLeft.RawComponents[2,1]=aRight.RawComponents[2,1]) and
         (aLeft.RawComponents[2,2]=aRight.RawComponents[2,2]);
end;

class operator TpvMatrix3x3D.NotEqual(const aLeft,aRight:TpvMatrix3x3D):boolean;
begin
 result:=(aLeft.RawComponents[0,0]<>aRight.RawComponents[0,0]) or
         (aLeft.RawComponents[0,1]<>aRight.RawComponents[0,1]) or
         (aLeft.RawComponents[0,2]<>aRight.RawComponents[0,2]) or
         (aLeft.RawComponents[1,0]<>aRight.RawComponents[1,0]) or
         (aLeft.RawComponents[1,1]<>aRight.RawComponents[1,1]) or
         (aLeft.RawComponents[1,2]<>aRight.RawComponents[1,2]) or
         (aLeft.RawComponents[2,0]<>aRight.RawComponents[2,0]) or
         (aLeft.RawComponents[2,1]<>aRight.RawComponents[2,1]) or
         (aLeft.RawComponents[2,2]<>aRight.RawComponents[2,2]);
end;

class operator TpvMatrix3x3D.Add(const aLeft,aRight:TpvMatrix3x3D):TpvMatrix3x3D;
begin
 result.RawComponents[0,0]:=aLeft.RawComponents[0,0]+aRight.RawComponents[0,0];
 result.RawComponents[0,1]:=aLeft.RawComponents[0,1]+aRight.RawComponents[0,1];
 result.RawComponents[0,2]:=aLeft.RawComponents[0,2]+aRight.RawComponents[0,2];
 result.RawComponents[1,0]:=aLeft.RawComponents[1,0]+aRight.RawComponents[1,0];
 result.RawComponents[1,1]:=aLeft.RawComponents[1,1]+aRight.RawComponents[1,1];
 result.RawComponents[1,2]:=aLeft.RawComponents[1,2]+aRight.RawComponents[1,2];
 result.RawComponents[2,0]:=aLeft.RawComponents[2,0]+aRight.RawComponents[2,0];
 result.RawComponents[2,1]:=aLeft.RawComponents[2,1]+aRight.RawComponents[2,1];
 result.RawComponents[2,2]:=aLeft.RawComponents[2,2]+aRight.RawComponents[2,2];
end;

class operator TpvMatrix3x3D.Subtract(const aLeft,aRight:TpvMatrix3x3D):TpvMatrix3x3D;
begin
 result.RawComponents[0,0]:=aLeft.RawComponents[0,0]-aRight.RawComponents[0,0];
 result.RawComponents[0,1]:=aLeft.RawComponents[0,1]-aRight.RawComponents[0,1];
 result.RawComponents[0,2]:=aLeft.RawComponents[0,2]-aRight.RawComponents[0,2];
 result.RawComponents[1,0]:=aLeft.RawComponents[1,0]-aRight.RawComponents[1,0];
 result.RawComponents[1,1]:=aLeft.RawComponents[1,1]-aRight.RawComponents[1,1];
 result.RawComponents[1,2]:=aLeft.RawComponents[1,2]-aRight.RawComponents[1,2];
 result.RawComponents[2,0]:=aLeft.RawComponents[2,0]-aRight.RawComponents[2,0];
 result.RawComponents[2,1]:=aLeft.RawComponents[2,1]-aRight.RawComponents[2,1];
 result.RawComponents[2,2]:=aLeft.RawComponents[2,2]-aRight.RawComponents[2,2];
end;

class operator TpvMatrix3x3D.Multiply(const aLeft,aRight:TpvMatrix3x3D):TpvMatrix3x3D;
begin
 result.RawComponents[0,0]:=(aLeft.RawComponents[0,0]*aRight.RawComponents[0,0])+(aLeft.RawComponents[0,1]*aRight.RawComponents[1,0])+(aLeft.RawComponents[0,2]*aRight.RawComponents[2,0]);
 result.RawComponents[0,1]:=(aLeft.RawComponents[0,0]*aRight.RawComponents[0,1])+(aLeft.RawComponents[0,1]*aRight.RawComponents[1,1])+(aLeft.RawComponents[0,2]*aRight.RawComponents[2,1]);
 result.RawComponents[0,2]:=(aLeft.RawComponents[0,0]*aRight.RawComponents[0,2])+(aLeft.RawComponents[0,1]*aRight.RawComponents[1,2])+(aLeft.RawComponents[0,2]*aRight.RawComponents[2,2]);
 result.RawComponents[1,0]:=(aLeft.RawComponents[1,0]*aRight.RawComponents[0,0])+(aLeft.RawComponents[1,1]*aRight.RawComponents[1,0])+(aLeft.RawComponents[1,2]*aRight.RawComponents[2,0]);
 result.RawComponents[1,1]:=(aLeft.RawComponents[1,0]*aRight.RawComponents[0,1])+(aLeft.RawComponents[1,1]*aRight.RawComponents[1,1])+(aLeft.RawComponents[1,2]*aRight.RawComponents[2,1]);
 result.RawComponents[1,2]:=(aLeft.RawComponents[1,0]*aRight.RawComponents[0,2])+(aLeft.RawComponents[1,1]*aRight.RawComponents[1,2])+(aLeft.RawComponents[1,2]*aRight.RawComponents[2,2]);
 result.RawComponents[2,0]:=(aLeft.RawComponents[2,0]*aRight.RawComponents[0,0])+(aLeft.RawComponents[2,1]*aRight.RawComponents[1,0])+(aLeft.RawComponents[2,2]*aRight.RawComponents[2,0]);
 result.RawComponents[2,1]:=(aLeft.RawComponents[2,0]*aRight.RawComponents[0,1])+(aLeft.RawComponents[2,1]*aRight.RawComponents[1,1])+(aLeft.RawComponents[2,2]*aRight.RawComponents[2,1]);
 result.RawComponents[2,2]:=(aLeft.RawComponents[2,0]*aRight.RawComponents[0,2])+(aLeft.RawComponents[2,1]*aRight.RawComponents[1,2])+(aLeft.RawComponents[2,2]*aRight.RawComponents[2,2]);
end;

class operator TpvMatrix3x3D.Multiply(const aLeft:TpvMatrix3x3D;const aRight:TpvDouble):TpvMatrix3x3D;
begin
 result.RawComponents[0,0]:=aLeft.RawComponents[0,0]*aRight;
 result.RawComponents[0,1]:=aLeft.RawComponents[0,1]*aRight;
 result.RawComponents[0,2]:=aLeft.RawComponents[0,2]*aRight;
 result.RawComponents[1,0]:=aLeft.RawComponents[1,0]*aRight;
 result.RawComponents[1,1]:=aLeft.RawComponents[1,1]*aRight;
 result.RawComponents[1,2]:=aLeft.RawComponents[1,2]*aRight;
 result.RawComponents[2,0]:=aLeft.RawComponents[2,0]*aRight;
 result.RawComponents[2,1]:=aLeft.RawComponents[2,1]*aRight;
 result.RawComponents[2,2]:=aLeft.RawComponents[2,2]*aRight;
end;

class operator TpvMatrix3x3D.Multiply(const aLeft:TpvDouble;const aRight:TpvMatrix3x3D):TpvMatrix3x3D;
begin
 result.RawComponents[0,0]:=aLeft*aRight.RawComponents[0,0];
 result.RawComponents[0,1]:=aLeft*aRight.RawComponents[0,1];
 result.RawComponents[0,2]:=aLeft*aRight.RawComponents[0,2];
 result.RawComponents[1,0]:=aLeft*aRight.RawComponents[1,0];
 result.RawComponents[1,1]:=aLeft*aRight.RawComponents[1,1];
 result.RawComponents[1,2]:=aLeft*aRight.RawComponents[1,2];
 result.RawComponents[2,0]:=aLeft*aRight.RawComponents[2,0];
 result.RawComponents[2,1]:=aLeft*aRight.RawComponents[2,1];
 result.RawComponents[2,2]:=aLeft*aRight.RawComponents[2,2];
end;

class operator TpvMatrix3x3D.Multiply(const aLeft:TpvMatrix3x3D;const aRight:TpvVector3D):TpvVector3D;
begin
 result.x:=(aLeft.RawComponents[0,0]*aRight.x)+(aLeft.RawComponents[1,0]*aRight.y)+(aLeft.RawComponents[2,0]*aRight.z);
 result.y:=(aLeft.RawComponents[0,1]*aRight.x)+(aLeft.RawComponents[1,1]*aRight.y)+(aLeft.RawComponents[2,1]*aRight.z);
 result.z:=(aLeft.RawComponents[0,2]*aRight.x)+(aLeft.RawComponents[1,2]*aRight.y)+(aLeft.RawComponents[2,2]*aRight.z);
end;

class operator TpvMatrix3x3D.Multiply(const aLeft:TpvVector3D;const aRight:TpvMatrix3x3D):TpvVector3D;
begin
 result.x:=(aLeft.x*aRight.RawComponents[0,0])+(aLeft.y*aRight.RawComponents[0,1])+(aLeft.z*aRight.RawComponents[0,2]);
 result.y:=(aLeft.x*aRight.RawComponents[1,0])+(aLeft.y*aRight.RawComponents[1,1])+(aLeft.z*aRight.RawComponents[1,2]);
 result.z:=(aLeft.x*aRight.RawComponents[2,0])+(aLeft.y*aRight.RawComponents[2,1])+(aLeft.z*aRight.RawComponents[2,2]);
end;

class operator TpvMatrix3x3D.Multiply(const aLeft:TpvMatrix3x3D;const aRight:TpvVector4D):TpvVector4D;
begin
 result.x:=(aLeft.RawComponents[0,0]*aRight.x)+(aLeft.RawComponents[1,0]*aRight.y)+(aLeft.RawComponents[2,0]*aRight.z);
 result.y:=(aLeft.RawComponents[0,1]*aRight.x)+(aLeft.RawComponents[1,1]*aRight.y)+(aLeft.RawComponents[2,1]*aRight.z);
 result.z:=(aLeft.RawComponents[0,2]*aRight.x)+(aLeft.RawComponents[1,2]*aRight.y)+(aLeft.RawComponents[2,2]*aRight.z);
 result.w:=aRight.w;
end;

class operator TpvMatrix3x3D.Multiply(const aLeft:TpvVector4D;const aRight:TpvMatrix3x3D):TpvVector4D;
begin
 result.x:=(aLeft.x*aRight.RawComponents[0,0])+(aLeft.y*aRight.RawComponents[0,1])+(aLeft.z*aRight.RawComponents[0,2]);
 result.y:=(aLeft.x*aRight.RawComponents[1,0])+(aLeft.y*aRight.RawComponents[1,1])+(aLeft.z*aRight.RawComponents[1,2]);
 result.z:=(aLeft.x*aRight.RawComponents[2,0])+(aLeft.y*aRight.RawComponents[2,1])+(aLeft.z*aRight.RawComponents[2,2]);
 result.w:=aLeft.w;
end;

class operator TpvMatrix3x3D.Divide(const aLeft,aRight:TpvMatrix3x3D):TpvMatrix3x3D;
begin
 result:=aLeft*aRight.Inverse;
end;

class operator TpvMatrix3x3D.Divide(const aLeft:TpvMatrix3x3D;const aRight:TpvDouble):TpvMatrix3x3D;
begin
 result.RawComponents[0,0]:=aLeft.RawComponents[0,0]/aRight;
 result.RawComponents[0,1]:=aLeft.RawComponents[0,1]/aRight;
 result.RawComponents[0,2]:=aLeft.RawComponents[0,2]/aRight;
 result.RawComponents[1,0]:=aLeft.RawComponents[1,0]/aRight;
 result.RawComponents[1,1]:=aLeft.RawComponents[1,1]/aRight;
 result.RawComponents[1,2]:=aLeft.RawComponents[1,2]/aRight;
 result.RawComponents[2,0]:=aLeft.RawComponents[2,0]/aRight;
 result.RawComponents[2,1]:=aLeft.RawComponents[2,1]/aRight;
 result.RawComponents[2,2]:=aLeft.RawComponents[2,2]/aRight;
end;

class operator TpvMatrix3x3D.Negative(const aMatrix:TpvMatrix3x3D):TpvMatrix3x3D;
begin
 result.RawComponents[0,0]:=-aMatrix.RawComponents[0,0];
 result.RawComponents[0,1]:=-aMatrix.RawComponents[0,1];
 result.RawComponents[0,2]:=-aMatrix.RawComponents[0,2];
 result.RawComponents[1,0]:=-aMatrix.RawComponents[1,0];
 result.RawComponents[1,1]:=-aMatrix.RawComponents[1,1];
 result.RawComponents[1,2]:=-aMatrix.RawComponents[1,2];
 result.RawComponents[2,0]:=-aMatrix.RawComponents[2,0];
 result.RawComponents[2,1]:=-aMatrix.RawComponents[2,1];
 result.RawComponents[2,2]:=-aMatrix.RawComponents[2,2];
end;

function TpvMatrix3x3D.Transpose:TpvMatrix3x3D;
begin
 result.RawComponents[0,0]:=RawComponents[0,0];
 result.RawComponents[0,1]:=RawComponents[1,0];
 result.RawComponents[0,2]:=RawComponents[2,0];
 result.RawComponents[1,0]:=RawComponents[0,1];
 result.RawComponents[1,1]:=RawComponents[1,1];
 result.RawComponents[1,2]:=RawComponents[2,1];
 result.RawComponents[2,0]:=RawComponents[0,2];
 result.RawComponents[2,1]:=RawComponents[1,2];
 result.RawComponents[2,2]:=RawComponents[2,2];
end;

function TpvMatrix3x3D.Determinant:TpvDouble;
begin
 result:=(((((RawComponents[0,0]*RawComponents[1,1]*RawComponents[2,2])+
             (RawComponents[0,1]*RawComponents[1,2]*RawComponents[2,0]))+
            (RawComponents[0,2]*RawComponents[1,0]*RawComponents[2,1]))-
           (RawComponents[0,2]*RawComponents[1,1]*RawComponents[2,0]))-
          (RawComponents[0,1]*RawComponents[1,0]*RawComponents[2,2]))-
         (RawComponents[0,0]*RawComponents[1,2]*RawComponents[2,1]);
end;

function TpvMatrix3x3D.Inverse:TpvMatrix3x3D;
var DeterminantValue:TpvDouble;
begin
 DeterminantValue:=Determinant;
 if DeterminantValue<>0.0 then begin
  DeterminantValue:=1.0/DeterminantValue;
  result.RawComponents[0,0]:=(((RawComponents[1,1]*RawComponents[2,2])-(RawComponents[1,2]*RawComponents[2,1]))*DeterminantValue);
  result.RawComponents[0,1]:=(((RawComponents[0,2]*RawComponents[2,1])-(RawComponents[0,1]*RawComponents[2,2]))*DeterminantValue);
  result.RawComponents[0,2]:=(((RawComponents[0,1]*RawComponents[1,2])-(RawComponents[0,2]*RawComponents[1,1]))*DeterminantValue);
  result.RawComponents[1,0]:=(((RawComponents[1,2]*RawComponents[2,0])-(RawComponents[1,0]*RawComponents[2,2]))*DeterminantValue);
  result.RawComponents[1,1]:=(((RawComponents[0,0]*RawComponents[2,2])-(RawComponents[0,2]*RawComponents[2,0]))*DeterminantValue);
  result.RawComponents[1,2]:=(((RawComponents[1,0]*RawComponents[0,2])-(RawComponents[0,0]*RawComponents[1,2]))*DeterminantValue);
  result.RawComponents[2,0]:=(((RawComponents[1,0]*RawComponents[2,1])-(RawComponents[1,1]*RawComponents[2,0]))*DeterminantValue);
  result.RawComponents[2,1]:=(((RawComponents[0,1]*RawComponents[2,0])-(RawComponents[0,0]*RawComponents[2,1]))*DeterminantValue);
  result.RawComponents[2,2]:=(((RawComponents[0,0]*RawComponents[1,1])-(RawComponents[0,1]*RawComponents[1,0]))*DeterminantValue);
 end else begin
  result:=TpvMatrix3x3D.Create(0.0,0.0,0.0,
                               0.0,0.0,0.0,
                               0.0,0.0,0.0);
 end; 
end;

function TpvMatrix3x3D.ToMatrix3x3:TpvMatrix3x3;
begin
 result.RawComponents[0,0]:=RawComponents[0,0];
 result.RawComponents[0,1]:=RawComponents[0,1];
 result.RawComponents[0,2]:=RawComponents[0,2];
 result.RawComponents[1,0]:=RawComponents[1,0];
 result.RawComponents[1,1]:=RawComponents[1,1];
 result.RawComponents[1,2]:=RawComponents[1,2];
 result.RawComponents[2,0]:=RawComponents[2,0];
 result.RawComponents[2,1]:=RawComponents[2,1];
 result.RawComponents[2,2]:=RawComponents[2,2];
end;

function TpvMatrix3x3D.ToQuaternionD:TpvQuaternionD;
var t,s:TpvDouble;
begin
 t:=RawComponents[0,0]+(RawComponents[1,1]+RawComponents[2,2]);
 if t>2.9999999 then begin
  result.x:=0.0;
  result.y:=0.0;
  result.z:=0.0;
  result.w:=1.0;
 end else if t>0.0000001 then begin
  s:=sqrt(1.0+t)*2.0;
  result.x:=(RawComponents[1,2]-RawComponents[2,1])/s;
  result.y:=(RawComponents[2,0]-RawComponents[0,2])/s;
  result.z:=(RawComponents[0,1]-RawComponents[1,0])/s;
  result.w:=s*0.25;
 end else if (RawComponents[0,0]>RawComponents[1,1]) and (RawComponents[0,0]>RawComponents[2,2]) then begin
  s:=sqrt(1.0+(RawComponents[0,0]-(RawComponents[1,1]+RawComponents[2,2])))*2.0;
  result.x:=s*0.25;
  result.y:=(RawComponents[1,0]+RawComponents[0,1])/s;
  result.z:=(RawComponents[2,0]+RawComponents[0,2])/s;
  result.w:=(RawComponents[1,2]-RawComponents[2,1])/s;
 end else if RawComponents[1,1]>RawComponents[2,2] then begin
  s:=sqrt(1.0+(RawComponents[1,1]-(RawComponents[0,0]+RawComponents[2,2])))*2.0;
  result.x:=(RawComponents[1,0]+RawComponents[0,1])/s;
  result.y:=s*0.25;
  result.z:=(RawComponents[2,1]+RawComponents[1,2])/s;
  result.w:=(RawComponents[2,0]-RawComponents[0,2])/s;
 end else begin
  s:=sqrt(1.0+(RawComponents[2,2]-(RawComponents[0,0]+RawComponents[1,1])))*2.0;
  result.x:=(RawComponents[2,0]+RawComponents[0,2])/s;
  result.y:=(RawComponents[2,1]+RawComponents[1,2])/s;
  result.z:=s*0.25;
  result.w:=(RawComponents[0,1]-RawComponents[1,0])/s;
 end;
 t:=sqrt(sqr(result.x)+sqr(result.y)+sqr(result.z)+sqr(result.w));
 if t>0.0 then begin
  result.x:=result.x/t;
  result.y:=result.y/t;
  result.z:=result.z/t;
  result.w:=result.w/t;
 end;
end;

function TpvMatrix3x3D.Decompose:TpvDecomposedMatrix3x3D;
var LocalMatrix:TpvMatrix3x3D;
begin

 if (RawComponents[0,0]=1.0) and
    (RawComponents[0,1]=0.0) and
    (RawComponents[0,2]=0.0) and
    (RawComponents[1,0]=0.0) and
    (RawComponents[1,1]=1.0) and
    (RawComponents[1,2]=0.0) and
    (RawComponents[2,0]=0.0) and
    (RawComponents[2,1]=0.0) and
    (RawComponents[2,2]=1.0) then begin

  result.Scale:=TpvVector3D.Create(1.0,1.0,1.0);
  result.Skew:=TpvVector3D.Create(0.0,0.0,0.0);
  result.Rotation:=TpvQuaternionD.Create(0.0,0.0,0.0,1.0);
  
  result.Valid:=true;

 end else if Determinant=0.0 then begin
  
  result.Valid:=false;

 end else begin
  
  LocalMatrix:=self;
  
  result.Scale.x:=LocalMatrix.Right.Length;
  LocalMatrix.Right:=LocalMatrix.Right.Normalize;
  
  result.Skew.x:=LocalMatrix.Right.Dot(LocalMatrix.Up);
  LocalMatrix.Up:=LocalMatrix.Up-(LocalMatrix.Right*result.Skew.x);
  
  result.Scale.y:=LocalMatrix.Up.Length;
  LocalMatrix.Up:=LocalMatrix.Up.Normalize;
  
  result.Skew.x:=result.Skew.x/result.Scale.y;
  
  result.Skew.y:=LocalMatrix.Right.Dot(LocalMatrix.Forwards);
  LocalMatrix.Forwards:=LocalMatrix.Forwards-(LocalMatrix.Right*result.Skew.y);
  result.Skew.z:=LocalMatrix.Up.Dot(LocalMatrix.Forwards);
  LocalMatrix.Forwards:=LocalMatrix.Forwards-(LocalMatrix.Up*result.Skew.z);
  
  result.Scale.z:=LocalMatrix.Forwards.Length;
  LocalMatrix.Forwards:=LocalMatrix.Forwards.Normalize;

  result.Skew.y:=result.Skew.y/result.Scale.z;
  result.Skew.z:=result.Skew.z/result.Scale.z;

  if LocalMatrix.Right.Dot(LocalMatrix.Up.Cross(LocalMatrix.Forwards))<0.0 then begin
   result.Scale.x:=-result.Scale.x;
   LocalMatrix:=-LocalMatrix;
  end;

  result.Rotation:=LocalMatrix.ToQuaternionD;

  result.Valid:=true;

 end;

end;

function TpvMatrix3x3D.Lerp(const aWith:TpvMatrix3x3D;const aTime:TpvDouble):TpvMatrix3x3D;
var InverseTime:TpvDouble;
begin
 if aTime<=0.0 then begin
  result:=self;
 end else if aTime>=1.0 then begin
  result:=aWith;
 end else begin
  InverseTime:=1.0-aTime;
  result.RawComponents[0,0]:=(RawComponents[0,0]*InverseTime)+(aWith.RawComponents[0,0]*aTime);
  result.RawComponents[0,1]:=(RawComponents[0,1]*InverseTime)+(aWith.RawComponents[0,1]*aTime);
  result.RawComponents[0,2]:=(RawComponents[0,2]*InverseTime)+(aWith.RawComponents[0,2]*aTime);
  result.RawComponents[1,0]:=(RawComponents[1,0]*InverseTime)+(aWith.RawComponents[1,0]*aTime);
  result.RawComponents[1,1]:=(RawComponents[1,1]*InverseTime)+(aWith.RawComponents[1,1]*aTime);
  result.RawComponents[1,2]:=(RawComponents[1,2]*InverseTime)+(aWith.RawComponents[1,2]*aTime);
  result.RawComponents[2,0]:=(RawComponents[2,0]*InverseTime)+(aWith.RawComponents[2,0]*aTime);
  result.RawComponents[2,1]:=(RawComponents[2,1]*InverseTime)+(aWith.RawComponents[2,1]*aTime);
  result.RawComponents[2,2]:=(RawComponents[2,2]*InverseTime)+(aWith.RawComponents[2,2]*aTime);
 end;
end;

function TpvMatrix3x3D.Nlerp(const aWith:TpvMatrix3x3D;const aTime:TpvDouble):TpvMatrix3x3D;
begin
 result:=TpvMatrix3x3D.Create(Decompose.Nlerp(aWith.Decompose,aTime));
end;

function TpvMatrix3x3D.Slerp(const aWith:TpvMatrix3x3D;const aTime:TpvDouble):TpvMatrix3x3D;
begin
 result:=TpvMatrix3x3D.Create(Decompose.Slerp(aWith.Decompose,aTime));
end;

function TpvMatrix3x3D.Elerp(const aWith:TpvMatrix3x3D;const aTime:TpvDouble):TpvMatrix3x3D;
begin
 result:=TpvMatrix3x3D.Create(Decompose.Elerp(aWith.Decompose,aTime));
end;

function TpvMatrix3x3D.Sqlerp(const aB,aC,aD:TpvMatrix3x3D;const aTime:TpvDouble):TpvMatrix3x3D;
begin
 result:=TpvMatrix3x3D.Create(Decompose.Sqlerp(aB.Decompose,aC.Decompose,aD.Decompose,aTime));
end;

class function TpvDecomposedMatrix4x4D.Create:TpvDecomposedMatrix4x4D;
begin
 result.Perspective:=TpvVector4D.Create(0.0,0.0,0.0,1.0);
 result.Translation:=TpvVector3D.Create(0.0,0.0,0.0);
 result.Scale:=TpvVector3D.Create(1.0,1.0,1.0);
 result.Skew:=TpvVector3D.Create(0.0,0.0,0.0);
 result.Rotation:=TpvQuaternionD.Create(0.0,0.0,0.0,1.0);
 result.Valid:=true;
end;

function TpvDecomposedMatrix4x4D.Lerp(const aWith:TpvDecomposedMatrix4x4D;const aTime:TpvDouble):TpvDecomposedMatrix4x4D;
begin
 if aTime<=0.0 then begin
  result:=self;
 end else if aTime>=1.0 then begin
  result:=aWith;
 end else begin
  result.Perspective:=Perspective.Lerp(aWith.Perspective,aTime);
  result.Translation:=Translation.Lerp(aWith.Translation,aTime);
  result.Scale:=Scale.Lerp(aWith.Scale,aTime);
  result.Skew:=Skew.Lerp(aWith.Skew,aTime);
  result.Rotation:=Rotation.Lerp(aWith.Rotation,aTime);
 end;
end;

function TpvDecomposedMatrix4x4D.Nlerp(const aWith:TpvDecomposedMatrix4x4D;const aTime:TpvDouble):TpvDecomposedMatrix4x4D;
begin
 if aTime<=0.0 then begin
  result:=self;
 end else if aTime>=1.0 then begin
  result:=aWith;
 end else begin
  result.Perspective:=Perspective.Lerp(aWith.Perspective,aTime);
  result.Translation:=Translation.Lerp(aWith.Translation,aTime);
  result.Scale:=Scale.Lerp(aWith.Scale,aTime);
  result.Skew:=Skew.Lerp(aWith.Skew,aTime);
  result.Rotation:=Rotation.Nlerp(aWith.Rotation,aTime);
 end;
end;

function TpvDecomposedMatrix4x4D.Slerp(const aWith:TpvDecomposedMatrix4x4D;const aTime:TpvDouble):TpvDecomposedMatrix4x4D;
begin
 if aTime<=0.0 then begin
  result:=self;
 end else if aTime>=1.0 then begin
  result:=aWith;
 end else begin
  result.Perspective:=Perspective.Lerp(aWith.Perspective,aTime);
  result.Translation:=Translation.Lerp(aWith.Translation,aTime);
  result.Scale:=Scale.Lerp(aWith.Scale,aTime);
  result.Skew:=Skew.Lerp(aWith.Skew,aTime);
  result.Rotation:=Rotation.Slerp(aWith.Rotation,aTime);
 end;
end;

function TpvDecomposedMatrix4x4D.Elerp(const aWith:TpvDecomposedMatrix4x4D;const aTime:TpvDouble):TpvDecomposedMatrix4x4D;
begin
 if aTime<=0.0 then begin
  result:=self;
 end else if aTime>=1.0 then begin
  result:=aWith;
 end else begin
  result.Perspective:=Perspective.Lerp(aWith.Perspective,aTime);
  result.Translation:=Translation.Lerp(aWith.Translation,aTime);
  result.Scale:=Scale.Lerp(aWith.Scale,aTime);
  result.Skew:=Skew.Lerp(aWith.Skew,aTime);
  result.Rotation:=Rotation.Elerp(aWith.Rotation,aTime);
 end;
end;

function TpvDecomposedMatrix4x4D.Sqlerp(const aB,aC,aD:TpvDecomposedMatrix4x4D;const aTime:TpvDouble):TpvDecomposedMatrix4x4D;
begin
 result:=Slerp(aD,aTime).Slerp(aB.Slerp(aC,aTime),(2.0*aTime)*(1.0-aTime));
end;

end.
