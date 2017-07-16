(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2017, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
 * 5. Write code, which is compatible with Delphi 7-XE7 and FreePascal >= 3.0 *
 *    so don't use generics/templates, operator overloading and another newer *
 *    syntax features than Delphi 7 has support for that, but if needed, make *
 *    it out-ifdef-able.                                                      *
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
unit PasVulkan.Math; 
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
  {$define SIMD}
 {$endif}
 {$ifdef cpux64}
  {$define SIMD}
 {$endif}
{$endif}

{$warnings off}

interface

uses SysUtils,Classes,Math,PasVulkan.Types.Standard;

const EPSILON={$ifdef UseDouble}1e-14{$else}1e-5{$endif}; // actually {$ifdef UseDouble}1e-16{$else}1e-7{$endif}; but we are conservative here

      SPHEREEPSILON=EPSILON;

      AABBEPSILON=EPSILON;

      MAX_SCALAR={$ifdef UseDouble}1.7e+308{$else}3.4e+28{$endif};

      DEG2RAD=pi/180.0;
      RAD2DEG=180.0/pi;

      HalfPI=pi*0.5;

      SupraEngineFPUPrecisionMode:TFPUPrecisionMode={$ifdef cpu386}pmExtended{$else}{$ifdef cpux64}pmExtended{$else}pmDouble{$endif}{$endif};

      SupraEngineFPUExceptionMask:TFPUExceptionMask=[exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision];

type PScalar=^TScalar;
     TScalar={$ifdef UseDouble}TDouble{$else}TFloat{$endif};

     PClipRect=^TClipRect;
     TClipRect=array[0..3] of TInt32;

     PFloatClipRect=^TFloatClipRect;
     TFloatClipRect=array[0..3] of TFloat;

     PPIntPoint=^PIntPoint;
     PIntPoint=^TIntPoint;
     TIntPoint=record
      public
       x,y:TInt32;
     end;

     PVector2=^TVector2;
     TVector2=record
      public
       constructor Create(const pX:TScalar); overload;
       constructor Create(const pX,pY:TScalar); overload;
       class operator Implicit(const a:TScalar):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Explicit(const a:TScalar):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Equal(const a,b:TVector2):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator NotEqual(const a,b:TVector2):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Inc(const a:TVector2):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Dec(const a:TVector2):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Add(const a,b:TVector2):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Add(const a:TVector2;const b:TScalar):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Add(const a:TScalar;const b:TVector2):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Subtract(const a,b:TVector2):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Subtract(const a:TVector2;const b:TScalar):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Subtract(const a:TScalar;const b:TVector2): TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a,b:TVector2):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a:TVector2;const b:TScalar):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a:TScalar;const b:TVector2):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Divide(const a,b:TVector2):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Divide(const a:TVector2;const b:TScalar):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Divide(const a:TScalar;const b:TVector2):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator IntDivide(const a,b:TVector2):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator IntDivide(const a:TVector2;const b:TScalar):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator IntDivide(const a:TScalar;const b:TVector2):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a,b:TVector2):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a:TVector2;const b:TScalar):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a:TScalar;const b:TVector2):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Negative(const a:TVector2):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Positive(const a:TVector2):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
      private
       {$i PasVulkan.Math.TVector2.Swizzle.Definitions.inc}
      private
       function GetComponent(const pIndex:TInt32):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetComponent(const pIndex:TInt32;const pValue:TScalar); {$ifdef CAN_INLINE}inline;{$endif}
      public
       function Perpendicular:TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       function Length:TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       function SquaredLength:TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       function Normalize:TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       function DistanceTo(const b:TVector2):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       function Dot(const b:TVector2):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       function Cross(const b:TVector2):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       function Lerp(const b:TVector2;const t:TScalar):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       function Angle(const b,c:TVector2):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       function Rotate(const Angle:TScalar):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function Rotate(const Center:TVector2;const Angle:TScalar):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
       property Components[const pIndex:TInt32]:TScalar read GetComponent write SetComponent; default;
       case TUInt8 of
        0:(RawComponents:array[0..1] of TScalar);
        1:(x,y:TScalar);
        2:(u,v:TScalar);
        3:(s,t:TScalar);
        4:(r,g:TScalar);
     end;

     PVector3=^TVector3;
     TVector3=record
      public
       constructor Create(const pX:TScalar); overload;
       constructor Create(const pX,pY,pZ:TScalar); overload;
       constructor Create(const pXY:TVector2;const pZ:TScalar=0.0); overload;
       class operator Implicit(const a:TScalar):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Explicit(const a:TScalar):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Equal(const a,b:TVector3):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator NotEqual(const a,b:TVector3):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Inc({$ifdef fpc}constref{$else}const{$endif} a:TVector3):TVector3; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Dec({$ifdef fpc}constref{$else}const{$endif} a:TVector3):TVector3; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Add({$ifdef fpc}constref{$else}const{$endif} a,b:TVector3):TVector3; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Add(const a:TVector3;const b:TScalar):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Add(const a:TScalar;const b:TVector3):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Subtract({$ifdef fpc}constref{$else}const{$endif} a,b:TVector3):TVector3; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Subtract(const a:TVector3;const b:TScalar):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Subtract(const a:TScalar;const b:TVector3):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply({$ifdef fpc}constref{$else}const{$endif} a,b:TVector3):TVector3; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Multiply(const a:TVector3;const b:TScalar):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a:TScalar;const b:TVector3):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Divide({$ifdef fpc}constref{$else}const{$endif} a,b:TVector3):TVector3; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Divide(const a:TVector3;const b:TScalar):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Divide(const a:TScalar;const b:TVector3):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator IntDivide({$ifdef fpc}constref{$else}const{$endif} a,b:TVector3):TVector3; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator IntDivide(const a:TVector3;const b:TScalar):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator IntDivide(const a:TScalar;const b:TVector3):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a,b:TVector3):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a:TVector3;const b:TScalar):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a:TScalar;const b:TVector3):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Negative({$ifdef fpc}constref{$else}const{$endif} a:TVector3):TVector3; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Positive(const a:TVector3):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
      private
       {$i PasVulkan.Math.TVector3.Swizzle.Definitions.inc}
      private
       function GetComponent(const pIndex:TInt32):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetComponent(const pIndex:TInt32;const pValue:TScalar); {$ifdef CAN_INLINE}inline;{$endif}
      public
       function Flip:TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       function Perpendicular:TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       function OneUnitOrthogonalVector:TVector3;
       function Length:TScalar; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function SquaredLength:TScalar; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function Normalize:TVector3; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function DistanceTo({$ifdef fpc}constref{$else}const{$endif} b:TVector3):TScalar; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function Abs:TVector3; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function Dot({$ifdef fpc}constref{$else}const{$endif} b:TVector3):TScalar; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function AngleTo(const b:TVector3):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       function Cross({$ifdef fpc}constref{$else}const{$endif} b:TVector3):TVector3; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function Lerp(const b:TVector3;const t:TScalar):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       function Angle(const b,c:TVector3):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       function RotateX(const Angle:TScalar):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       function RotateY(const Angle:TScalar):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       function RotateZ(const Angle:TScalar):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       function ProjectToBounds(const MinVector,MaxVector:TVector3):TScalar;
       property Components[const pIndex:TInt32]:TScalar read GetComponent write SetComponent; default;
       case TUInt8 of
        0:(RawComponents:array[0..2] of TScalar);
        1:(x,y,z:TScalar);
        2:(r,g,b:TScalar);
        3:(s,t,p:TScalar);
        4:(Pitch,Yaw,Roll:TScalar);
        6:(Vector2:TVector2);
     end;

     PVector4=^TVector4;
     TVector4=record
      public
       constructor Create(const pX:TScalar); overload;
       constructor Create(const pX,pY,pZ,pW:TScalar); overload;
       constructor Create(const pXY:TVector2;const pZ:TScalar=0.0;const pW:TScalar=1.0); overload;
       constructor Create(const pXYZ:TVector3;const pW:TScalar=1.0); overload;
       class operator Implicit(const a:TScalar):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Explicit(const a:TScalar):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Equal(const a,b:TVector4):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator NotEqual(const a,b:TVector4):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Inc({$ifdef fpc}constref{$else}const{$endif} a:TVector4):TVector4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Dec({$ifdef fpc}constref{$else}const{$endif} a:TVector4):TVector4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Add({$ifdef fpc}constref{$else}const{$endif} a,b:TVector4):TVector4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Add(const a:TVector4;const b:TScalar):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Add(const a:TScalar;const b:TVector4):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Subtract({$ifdef fpc}constref{$else}const{$endif} a,b:TVector4):TVector4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Subtract(const a:TVector4;const b:TScalar):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Subtract(const a:TScalar;const b:TVector4): TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply({$ifdef fpc}constref{$else}const{$endif} a,b:TVector4):TVector4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Multiply(const a:TVector4;const b:TScalar):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a:TScalar;const b:TVector4):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Divide({$ifdef fpc}constref{$else}const{$endif} a,b:TVector4):TVector4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Divide(const a:TVector4;const b:TScalar):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Divide(const a:TScalar;const b:TVector4):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator IntDivide({$ifdef fpc}constref{$else}const{$endif} a,b:TVector4):TVector4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator IntDivide(const a:TVector4;const b:TScalar):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator IntDivide(const a:TScalar;const b:TVector4):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a,b:TVector4):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a:TVector4;const b:TScalar):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a:TScalar;const b:TVector4):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Negative({$ifdef fpc}constref{$else}const{$endif} a:TVector4):TVector4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Positive(const a:TVector4):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
      private
       {$i PasVulkan.Math.TVector4.Swizzle.Definitions.inc}
      private
       function GetComponent(const pIndex:TInt32):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetComponent(const pIndex:TInt32;const pValue:TScalar); {$ifdef CAN_INLINE}inline;{$endif}
      public
       function Flip:TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       function Perpendicular:TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       function Length:TScalar; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function SquaredLength:TScalar; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function Normalize:TVector4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function DistanceTo({$ifdef fpc}constref{$else}const{$endif} b:TVector4):TScalar; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function Abs:TVector4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function Dot({$ifdef fpc}constref{$else}const{$endif} b:TVector4):TScalar; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function AngleTo(const b:TVector4):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       function Cross({$ifdef fpc}constref{$else}const{$endif} b:TVector4):TVector4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function Lerp(const b:TVector4;const t:TScalar):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       function Angle(const b,c:TVector4):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       function RotateX(const Angle:TScalar):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       function RotateY(const Angle:TScalar):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       function RotateZ(const Angle:TScalar):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       function Rotate(const Angle:TScalar;const Axis:TVector3):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       function ProjectToBounds(const MinVector,MaxVector:TVector4):TScalar;
      public
       property Components[const pIndex:TInt32]:TScalar read GetComponent write SetComponent; default;
       case TUInt8 of
        0:(RawComponents:array[0..3] of TScalar);
        1:(x,y,z,w:TScalar);
        2:(r,g,b,a:TScalar);
        3:(s,t,p,q:TScalar);
        5:(Vector2:TVector2);
        6:(Vector3:TVector3);
     end;

     TVector2Helper=record helper for TVector2
      {$i PasVulkan.Math.TVector2Helper.Swizzle.Definitions.inc}
     end;

     TVector3Helper=record helper for TVector3
      {$i PasVulkan.Math.TVector3Helper.Swizzle.Definitions.inc}
     end;

     TVector4Helper=record helper for TVector4
      {$i PasVulkan.Math.TVector4Helper.Swizzle.Definitions.inc}
     end;

     PPackedTangentSpace=^TPackedTangentSpace;
     TPackedTangentSpace=record
      x,y,z,w:TUInt8;
     end;

     PNormalizedSphericalCoordinates=^TNormalizedSphericalCoordinates;
     TNormalizedSphericalCoordinates=record
      Longitude:TScalar;
      Latitude:TScalar;
     end;

     TVector3Array=array of TVector3;

     PVector3s=^TVector3s;
     TVector3s=array[0..$ff] of TVector3;

     PPVector3s=^TPVector3s;
     TPVector3s=array[0..$ff] of PVector3;

     PPlane=^TPlane;
     TPlane=record
      public
       constructor Create(const pNormal:TVector3;const pDistance:TScalar); overload;
       constructor Create(const x,y,z,pDistance:TScalar); overload;
       constructor Create(const pA,pB,pC:TVector3); overload;
       constructor Create(const pA,pB,pC:TVector4); overload;
       constructor Create(const Vector:TVector4); overload;
       function ToVector:TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       function Normalize:TPlane; {$ifdef CAN_INLINE}inline;{$endif}
       function DistanceTo(const Point:TVector3):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function DistanceTo(const Point:TVector4):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
       procedure ClipSegment(const p0,p1:TVector3;out Clipped:TVector3); overload;
       function ClipSegmentClosest(const p0,p1:TVector3;out Clipped0,Clipped1:TVector3):TInt32; overload;
       function ClipSegmentLine(var p0,p1:TVector3):boolean;
       case TUInt8 of
        0:(
         RawComponents:array[0..3] of TScalar;
        );
        1:(
         x,y,z,w:TScalar;
        );
        2:(
         Normal:TVector3;
         Distance:TScalar;
        );
     end;

     PQuaternion=^TQuaternion;
     TQuaternion=record
      public
       constructor Create(const pX:TScalar); overload;
       constructor Create(const pX,pY,pZ,pW:TScalar); overload;
       constructor Create(const pVector:TVector4); overload;
       constructor CreateFromAngleAxis(const Angle:TScalar;const Axis:TVector3);
       constructor CreateFromEuler(const Pitch,Yaw,Roll:TScalar); overload;
       constructor CreateFromEuler(const Angles:TVector3); overload;
       constructor CreateFromNormalizedSphericalCoordinates(const NormalizedSphericalCoordinates:TNormalizedSphericalCoordinates);
       constructor CreateFromToRotation(const FromDirection,ToDirection:TVector3);
       class operator Implicit(const a:TScalar):TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Explicit(const a:TScalar):TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Equal(const a,b:TQuaternion):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator NotEqual(const a,b:TQuaternion):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Inc({$ifdef fpc}constref{$else}const{$endif} a:TQuaternion):TQuaternion; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Dec({$ifdef fpc}constref{$else}const{$endif} a:TQuaternion):TQuaternion; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Add({$ifdef fpc}constref{$else}const{$endif} a,b:TQuaternion):TQuaternion; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Add(const a:TQuaternion;const b:TScalar):TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Add(const a:TScalar;const b:TQuaternion):TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Subtract({$ifdef fpc}constref{$else}const{$endif} a,b:TQuaternion):TQuaternion; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Subtract(const a:TQuaternion;const b:TScalar):TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Subtract(const a:TScalar;const b:TQuaternion): TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply({$ifdef fpc}constref{$else}const{$endif} a,b:TQuaternion):TQuaternion; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Multiply(const a:TQuaternion;const b:TScalar):TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a:TScalar;const b:TQuaternion):TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply({$ifdef fpc}constref{$else}const{$endif} a:TQuaternion;{$ifdef fpc}constref{$else}const{$endif} b:TVector3):TVector3; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Multiply(const a:TVector3;const b:TQuaternion):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply({$ifdef fpc}constref{$else}const{$endif} a:TQuaternion;{$ifdef fpc}constref{$else}const{$endif} b:TVector4):TVector4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Multiply(const a:TVector4;const b:TQuaternion):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Divide({$ifdef fpc}constref{$else}const{$endif} a,b:TQuaternion):TQuaternion; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Divide(const a:TQuaternion;const b:TScalar):TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Divide(const a:TScalar;const b:TQuaternion):TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator IntDivide({$ifdef fpc}constref{$else}const{$endif} a,b:TQuaternion):TQuaternion; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator IntDivide(const a:TQuaternion;const b:TScalar):TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator IntDivide(const a:TScalar;const b:TQuaternion):TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a,b:TQuaternion):TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a:TQuaternion;const b:TScalar):TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a:TScalar;const b:TQuaternion):TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Negative({$ifdef fpc}constref{$else}const{$endif} a:TQuaternion):TQuaternion; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Positive(const a:TQuaternion):TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
      private
       function GetComponent(const pIndex:TInt32):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetComponent(const pIndex:TInt32;const pValue:TScalar); {$ifdef CAN_INLINE}inline;{$endif}
      public
       function ToNormalizedSphericalCoordinates:TNormalizedSphericalCoordinates; {$ifdef CAN_INLINE}inline;{$endif}
       function ToEuler:TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       function ToPitch:TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       function ToYaw:TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       function ToRoll:TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       procedure ToAngleAxis(out Angle:TScalar;out Axis:TVector3); {$ifdef CAN_INLINE}inline;{$endif}
       function Generator:TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       function Flip:TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       function Perpendicular:TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       function Conjugate:TQuaternion; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function Inverse:TQuaternion; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function Length:TScalar; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function SquaredLength:TScalar; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function Normalize:TQuaternion; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function DistanceTo({$ifdef fpc}constref{$else}const{$endif} b:TQuaternion):TScalar; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function Abs:TQuaternion; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function Dot({$ifdef fpc}constref{$else}const{$endif} b:TQuaternion):TScalar; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function Lerp(const b:TQuaternion;const t:TScalar):TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       function Nlerp(const b:TQuaternion;const t:TScalar):TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       function Slerp(const b:TQuaternion;const t:TScalar):TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       function RotateAroundAxis(const b:TQuaternion):TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       function Integrate(const Omega:TVector3;const DeltaTime:TScalar):TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       function Spin(const Omega:TVector3;const DeltaTime:TScalar):TQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       property Components[const pIndex:TInt32]:TScalar read GetComponent write SetComponent; default;
       case TUInt8 of
        0:(RawComponents:array[0..3] of TScalar);
        1:(x,y,z,w:TScalar);
        2:(Vector:TVector4);
     end;

     PMatrix2x2=^TMatrix2x2;
     TMatrix2x2=record
      public
//     constructor Create; overload;
       constructor Create(const pX:TScalar); overload;
       constructor Create(const pXX,pXY,pYX,pYY:TScalar); overload;
       constructor Create(const pX,pY:TVector2); overload;
       class operator Implicit(const a:TScalar):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Explicit(const a:TScalar):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Equal(const a,b:TMatrix2x2):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator NotEqual(const a,b:TMatrix2x2):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Inc(const a:TMatrix2x2):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Dec(const a:TMatrix2x2):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Add(const a,b:TMatrix2x2):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Add(const a:TMatrix2x2;const b:TScalar):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Add(const a:TScalar;const b:TMatrix2x2):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Subtract(const a,b:TMatrix2x2):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Subtract(const a:TMatrix2x2;const b:TScalar):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Subtract(const a:TScalar;const b:TMatrix2x2): TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a,b:TMatrix2x2):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a:TMatrix2x2;const b:TScalar):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a:TScalar;const b:TMatrix2x2):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a:TMatrix2x2;const b:TVector2):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a:TVector2;const b:TMatrix2x2):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Divide(const a,b:TMatrix2x2):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Divide(const a:TMatrix2x2;const b:TScalar):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Divide(const a:TScalar;const b:TMatrix2x2):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator IntDivide(const a,b:TMatrix2x2):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator IntDivide(const a:TMatrix2x2;const b:TScalar):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator IntDivide(const a:TScalar;const b:TMatrix2x2):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a,b:TMatrix2x2):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a:TMatrix2x2;const b:TScalar):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a:TScalar;const b:TMatrix2x2):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Negative(const a:TMatrix2x2):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Positive(const a:TMatrix2x2):TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
      private
       function GetComponent(const pIndexA,pIndexB:TInt32):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetComponent(const pIndexA,pIndexB:TInt32;const pValue:TScalar); {$ifdef CAN_INLINE}inline;{$endif}
       function GetColumn(const pIndex:TInt32):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetColumn(const pIndex:TInt32;const pValue:TVector2); {$ifdef CAN_INLINE}inline;{$endif}
       function GetRow(const pIndex:TInt32):TVector2; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetRow(const pIndex:TInt32;const pValue:TVector2); {$ifdef CAN_INLINE}inline;{$endif}
      public
       function Determinant:TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       function Inverse:TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       function Transpose:TMatrix2x2; {$ifdef CAN_INLINE}inline;{$endif}
       property Components[const pIndexA,pIndexB:TInt32]:TScalar read GetComponent write SetComponent; default;
       property Columns[const pIndex:TInt32]:TVector2 read GetColumn write SetColumn;
       property Rows[const pIndex:TInt32]:TVector2 read GetRow write SetRow;
       case TInt32 of
        0:(RawComponents:array[0..1,0..1] of TScalar);
     end;

     PDecomposedMatrix3x3=^TDecomposedMatrix3x3;
     TDecomposedMatrix3x3=record
      public
       Valid:boolean;
       Scale:TVector3;
       Skew:TVector3; // XY XZ YZ
       Rotation:TQuaternion;
       function Lerp(const b:TDecomposedMatrix3x3;const t:TScalar):TDecomposedMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       function Nlerp(const b:TDecomposedMatrix3x3;const t:TScalar):TDecomposedMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       function Slerp(const b:TDecomposedMatrix3x3;const t:TScalar):TDecomposedMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
     end;

     PMatrix3x3=^TMatrix3x3;
     TMatrix3x3=record
      public
//     constructor Create; overload;
       constructor Create(const pX:TScalar); overload;
       constructor Create(const pXX,pXY,pXZ,pYX,pYY,pYZ,pZX,pZY,pZZ:TScalar); overload;
       constructor Create(const pX,pY,pZ:TVector3); overload;
       constructor CreateRotateX(const Angle:TScalar);
       constructor CreateRotateY(const Angle:TScalar);
       constructor CreateRotateZ(const Angle:TScalar);
       constructor CreateRotate(const Angle:TScalar;const Axis:TVector3);
       constructor CreateScale(const sx,sy,sz:TScalar); overload;
       constructor CreateScale(const pScale:TVector3); overload;
       constructor CreateFromToRotation(const FromDirection,ToDirection:TVector3);
       constructor CreateConstruct(const pForwards,pUp:TVector3);
       constructor CreateOuterProduct(const u,v:TVector3);
       constructor CreateFromQuaternion(pQuaternion:TQuaternion);
       constructor CreateFromQTangent(pQTangent:TQuaternion);
       constructor CreateRecomposed(const DecomposedMatrix3x3:TDecomposedMatrix3x3);
       class operator Implicit(const a:TScalar):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Explicit(const a:TScalar):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Equal(const a,b:TMatrix3x3):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator NotEqual(const a,b:TMatrix3x3):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Inc(const a:TMatrix3x3):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Dec(const a:TMatrix3x3):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Add(const a,b:TMatrix3x3):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Add(const a:TMatrix3x3;const b:TScalar):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Add(const a:TScalar;const b:TMatrix3x3):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Subtract(const a,b:TMatrix3x3):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Subtract(const a:TMatrix3x3;const b:TScalar):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Subtract(const a:TScalar;const b:TMatrix3x3): TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a,b:TMatrix3x3):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a:TMatrix3x3;const b:TScalar):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a:TScalar;const b:TMatrix3x3):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a:TMatrix3x3;const b:TVector3):TVector3;  {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a:TVector3;const b:TMatrix3x3):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a:TMatrix3x3;const b:TVector4):TVector4;  {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a:TVector4;const b:TMatrix3x3):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a:TMatrix3x3;const b:TPlane):TPlane; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a:TPlane;const b:TMatrix3x3):TPlane; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Divide(const a,b:TMatrix3x3):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Divide(const a:TMatrix3x3;const b:TScalar):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Divide(const a:TScalar;const b:TMatrix3x3):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator IntDivide(const a,b:TMatrix3x3):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator IntDivide(const a:TMatrix3x3;const b:TScalar):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator IntDivide(const a:TScalar;const b:TMatrix3x3):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a,b:TMatrix3x3):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a:TMatrix3x3;const b:TScalar):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a:TScalar;const b:TMatrix3x3):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Negative(const a:TMatrix3x3):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Positive(const a:TMatrix3x3):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
      private
       function GetComponent(const pIndexA,pIndexB:TInt32):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetComponent(const pIndexA,pIndexB:TInt32;const pValue:TScalar); {$ifdef CAN_INLINE}inline;{$endif}
       function GetColumn(const pIndex:TInt32):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetColumn(const pIndex:TInt32;const pValue:TVector3); {$ifdef CAN_INLINE}inline;{$endif}
       function GetRow(const pIndex:TInt32):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetRow(const pIndex:TInt32;const pValue:TVector3); {$ifdef CAN_INLINE}inline;{$endif}
      public
       function Determinant:TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       function Inverse:TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       function Transpose:TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       function EulerAngles:TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       function Normalize:TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       function OrthoNormalize:TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       function RobustOrthoNormalize(const Tolerance:TScalar=1e-3):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       function ToQuaternion:TQuaternion;
       function ToQTangent:TQuaternion;
       function SimpleLerp(const b:TMatrix3x3;const t:TScalar):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       function SimpleNlerp(const b:TMatrix3x3;const t:TScalar):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       function SimpleSlerp(const b:TMatrix3x3;const t:TScalar):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       function Lerp(const b:TMatrix3x3;const t:TScalar):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       function Nlerp(const b:TMatrix3x3;const t:TScalar):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       function Slerp(const b:TMatrix3x3;const t:TScalar):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       function MulInverse(const a:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function MulInverse(const a:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function Decompose:TDecomposedMatrix3x3;
       property Components[const pIndexA,pIndexB:TInt32]:TScalar read GetComponent write SetComponent; default;
       property Columns[const pIndex:TInt32]:TVector3 read GetColumn write SetColumn;
       property Rows[const pIndex:TInt32]:TVector3 read GetRow write SetRow;
       case TInt32 of
        0:(RawComponents:array[0..2,0..2] of TScalar);
        1:(m00,m01,m02,m10,m11,m12,m20,m21,m22:TScalar);
        2:(Tangent,Bitangent,Normal:TVector3);
        3:(Right,Up,Forwards:TVector3);
       end;

     PDecomposedMatrix4x4=^TDecomposedMatrix4x4;
     TDecomposedMatrix4x4=record
      public
       Valid:boolean;
       Perspective:TVector4;
       Translation:TVector3;
       Scale:TVector3;
       Skew:TVector3; // XY XZ YZ
       Rotation:TQuaternion;
       function Lerp(const b:TDecomposedMatrix4x4;const t:TScalar):TDecomposedMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       function Nlerp(const b:TDecomposedMatrix4x4;const t:TScalar):TDecomposedMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       function Slerp(const b:TDecomposedMatrix4x4;const t:TScalar):TDecomposedMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
     end;

     PMatrix4x4=^TMatrix4x4;
     TMatrix4x4=record
      public
//     constructor Create; overload;
       constructor Create(const pX:TScalar); overload;
       constructor Create(const pXX,pXY,pXZ,pXW,pYX,pYY,pYZ,pYW,pZX,pZY,pZZ,pZW,pWX,pWY,pWZ,pWW:TScalar); overload;
       constructor Create(const pX,pY,pZ,pW:TVector4); overload;
       constructor Create(const pMatrix:TMatrix3x3); overload;
       constructor CreateRotateX(const Angle:TScalar);
       constructor CreateRotateY(const Angle:TScalar);
       constructor CreateRotateZ(const Angle:TScalar);
       constructor CreateRotate(const Angle:TScalar;const Axis:TVector3);
       constructor CreateRotation(const pMatrix:TMatrix4x4); overload;
       constructor CreateScale(const sx,sy,sz:TScalar); overload;
       constructor CreateScale(const pScale:TVector3); overload;
       constructor CreateScale(const sx,sy,sz,sw:TScalar); overload;
       constructor CreateScale(const pScale:TVector4); overload;
       constructor CreateTranslation(const tx,ty,tz:TScalar); overload;
       constructor CreateTranslation(const pTranslation:TVector3); overload;
       constructor CreateTranslation(const tx,ty,tz,tw:TScalar); overload;
       constructor CreateTranslation(const pTranslation:TVector4); overload;
       constructor CreateTranslated(const pMatrix:TMatrix4x4;pTranslation:TVector3); overload;
       constructor CreateTranslated(const pMatrix:TMatrix4x4;pTranslation:TVector4); overload;
       constructor CreateFromToRotation(const FromDirection,ToDirection:TVector3);
       constructor CreateConstruct(const pForwards,pUp:TVector3);
       constructor CreateOuterProduct(const u,v:TVector3);
       constructor CreateFromQuaternion(pQuaternion:TQuaternion);
       constructor CreateFromQTangent(pQTangent:TQuaternion);
       constructor CreateReflect(const pPlane:TPlane);
       constructor CreateFrustum(const Left,Right,Bottom,Top,zNear,zFar:TScalar);
       constructor CreateOrtho(const Left,Right,Bottom,Top,zNear,zFar:TScalar);
       constructor CreateOrthoLH(const Left,Right,Bottom,Top,zNear,zFar:TScalar);
       constructor CreateOrthoRH(const Left,Right,Bottom,Top,zNear,zFar:TScalar);
       constructor CreateOrthoOffCenterLH(const Left,Right,Bottom,Top,zNear,zFar:TScalar);
       constructor CreateOrthoOffCenterRH(const Left,Right,Bottom,Top,zNear,zFar:TScalar);
       constructor CreatePerspective(const fovy,Aspect,zNear,zFar:TScalar);
       constructor CreateLookAt(const Eye,Center,Up:TVector3);
       constructor CreateFill(const Eye,RightVector,UpVector,ForwardVector:TVector3);
       constructor CreateConstructX(const xAxis:TVector3);
       constructor CreateConstructY(const yAxis:TVector3);
       constructor CreateConstructZ(const zAxis:TVector3);
       constructor CreateProjectionMatrixClip(const ProjectionMatrix:TMatrix4x4;const ClipPlane:TPlane);
       constructor CreateRecomposed(const DecomposedMatrix4x4:TDecomposedMatrix4x4);
       class operator Implicit({$ifdef fpc}constref{$else}const{$endif} a:TScalar):TMatrix4x4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Explicit({$ifdef fpc}constref{$else}const{$endif} a:TScalar):TMatrix4x4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Equal({$ifdef fpc}constref{$else}const{$endif} a,b:TMatrix4x4):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator NotEqual({$ifdef fpc}constref{$else}const{$endif} a,b:TMatrix4x4):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Inc({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4):TMatrix4x4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Dec({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4):TMatrix4x4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Add({$ifdef fpc}constref{$else}const{$endif} a,b:TMatrix4x4):TMatrix4x4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Add({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4;{$ifdef fpc}constref{$else}const{$endif} b:TScalar):TMatrix4x4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Add({$ifdef fpc}constref{$else}const{$endif} a:TScalar;{$ifdef fpc}constref{$else}const{$endif} b:TMatrix4x4):TMatrix4x4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Subtract({$ifdef fpc}constref{$else}const{$endif} a,b:TMatrix4x4):TMatrix4x4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Subtract({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4;{$ifdef fpc}constref{$else}const{$endif} b:TScalar):TMatrix4x4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Subtract({$ifdef fpc}constref{$else}const{$endif} a:TScalar;{$ifdef fpc}constref{$else}const{$endif} b:TMatrix4x4): TMatrix4x4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Multiply({$ifdef fpc}constref{$else}const{$endif} a,b:TMatrix4x4):TMatrix4x4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Multiply({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4;{$ifdef fpc}constref{$else}const{$endif} b:TScalar):TMatrix4x4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Multiply({$ifdef fpc}constref{$else}const{$endif} a:TScalar;{$ifdef fpc}constref{$else}const{$endif} b:TMatrix4x4):TMatrix4x4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Multiply({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4;{$ifdef fpc}constref{$else}const{$endif} b:TVector3):TVector3; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Multiply({$ifdef fpc}constref{$else}const{$endif} a:TVector3;{$ifdef fpc}constref{$else}const{$endif} b:TMatrix4x4):TVector3; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Multiply({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4;{$ifdef fpc}constref{$else}const{$endif} b:TVector4):TVector4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Multiply({$ifdef fpc}constref{$else}const{$endif} a:TVector4;{$ifdef fpc}constref{$else}const{$endif} b:TMatrix4x4):TVector4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Multiply({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4;{$ifdef fpc}constref{$else}const{$endif} b:TPlane):TPlane; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Multiply({$ifdef fpc}constref{$else}const{$endif} a:TPlane;{$ifdef fpc}constref{$else}const{$endif} b:TMatrix4x4):TPlane; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Divide({$ifdef fpc}constref{$else}const{$endif} a,b:TMatrix4x4):TMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Divide({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4;{$ifdef fpc}constref{$else}const{$endif} b:TScalar):TMatrix4x4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Divide({$ifdef fpc}constref{$else}const{$endif} a:TScalar;{$ifdef fpc}constref{$else}const{$endif} b:TMatrix4x4):TMatrix4x4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator IntDivide({$ifdef fpc}constref{$else}const{$endif} a,b:TMatrix4x4):TMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator IntDivide({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4;{$ifdef fpc}constref{$else}const{$endif} b:TScalar):TMatrix4x4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator IntDivide({$ifdef fpc}constref{$else}const{$endif} a:TScalar;{$ifdef fpc}constref{$else}const{$endif} b:TMatrix4x4):TMatrix4x4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Modulus({$ifdef fpc}constref{$else}const{$endif} a,b:TMatrix4x4):TMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4;{$ifdef fpc}constref{$else}const{$endif} b:TScalar):TMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus({$ifdef fpc}constref{$else}const{$endif} a:TScalar;{$ifdef fpc}constref{$else}const{$endif} b:TMatrix4x4):TMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Negative({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4):TMatrix4x4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Positive(const a:TMatrix4x4):TMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
      private
       function GetComponent(const pIndexA,pIndexB:TInt32):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetComponent(const pIndexA,pIndexB:TInt32;const pValue:TScalar); {$ifdef CAN_INLINE}inline;{$endif}
       function GetColumn(const pIndex:TInt32):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetColumn(const pIndex:TInt32;const pValue:TVector4); {$ifdef CAN_INLINE}inline;{$endif}
       function GetRow(const pIndex:TInt32):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetRow(const pIndex:TInt32;const pValue:TVector4); {$ifdef CAN_INLINE}inline;{$endif}
      public
       function Determinant:TScalar; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function SimpleInverse:TMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       function Inverse:TMatrix4x4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function Transpose:TMatrix4x4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function EulerAngles:TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       function Normalize:TMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       function OrthoNormalize:TMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       function RobustOrthoNormalize(const Tolerance:TScalar=1e-3):TMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       function ToQuaternion:TQuaternion;
       function ToQTangent:TQuaternion;
       function ToMatrix3x3:TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
       function ToRotation:TMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       function SimpleLerp(const b:TMatrix4x4;const t:TScalar):TMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       function SimpleNlerp(const b:TMatrix4x4;const t:TScalar):TMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       function SimpleSlerp(const b:TMatrix4x4;const t:TScalar):TMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       function Lerp(const b:TMatrix4x4;const t:TScalar):TMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       function Nlerp(const b:TMatrix4x4;const t:TScalar):TMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       function Slerp(const b:TMatrix4x4;const t:TScalar):TMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       function MulInverse({$ifdef fpc}constref{$else}const{$endif} a:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function MulInverse({$ifdef fpc}constref{$else}const{$endif} a:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function MulInverted({$ifdef fpc}constref{$else}const{$endif} a:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function MulInverted({$ifdef fpc}constref{$else}const{$endif} a:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function MulBasis({$ifdef fpc}constref{$else}const{$endif} a:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function MulBasis({$ifdef fpc}constref{$else}const{$endif} a:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function MulTransposedBasis({$ifdef fpc}constref{$else}const{$endif} a:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function MulTransposedBasis({$ifdef fpc}constref{$else}const{$endif} a:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function MulHomogen({$ifdef fpc}constref{$else}const{$endif} a:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function MulHomogen({$ifdef fpc}constref{$else}const{$endif} a:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function Decompose:TDecomposedMatrix4x4;
       property Components[const pIndexA,pIndexB:TInt32]:TScalar read GetComponent write SetComponent; default;
       property Columns[const pIndex:TInt32]:TVector4 read GetColumn write SetColumn;
       property Rows[const pIndex:TInt32]:TVector4 read GetRow write SetRow;
       case TInt32 of
        0:(RawComponents:array[0..3,0..3] of TScalar);
        1:(m00,m01,m02,m03,m10,m11,m12,m13,m20,m21,m22,m23,m30,m31,m32,m33:TScalar);
        2:(Tangent,Bitangent,Normal,Translation:TVector4);
        3:(Right,Up,Forwards,Offset:TVector4);
     end;

     // Dual quaternion with uniform scaling support
     PDualQuaternion=^TDualQuaternion;
     TDualQuaternion=record
      public
       constructor Create(const pQ0,PQ1:TQuaternion); overload;
       constructor CreateFromRotationTranslationScale(const pRotation:TQuaternion;const pTranslation:TVector3;const pScale:TScalar); overload;
       constructor CreateFromMatrix(const pMatrix:TMatrix4x4); overload;
       class operator Implicit(const a:TMatrix4x4):TDualQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Explicit(const a:TMatrix4x4):TDualQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Implicit(const a:TDualQuaternion):TMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Explicit(const a:TDualQuaternion):TMatrix4x4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Equal(const a,b:TDualQuaternion):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator NotEqual(const a,b:TDualQuaternion):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Add({$ifdef fpc}constref{$else}const{$endif} a,b:TDualQuaternion):TDualQuaternion; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Subtract({$ifdef fpc}constref{$else}const{$endif} a,b:TDualQuaternion):TDualQuaternion; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Multiply({$ifdef fpc}constref{$else}const{$endif} a,b:TDualQuaternion):TDualQuaternion; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Multiply(const a:TDualQuaternion;const b:TScalar):TDualQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a:TScalar;const b:TDualQuaternion):TDualQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply({$ifdef fpc}constref{$else}const{$endif} a:TDualQuaternion;{$ifdef fpc}constref{$else}const{$endif} b:TVector3):TVector3; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Multiply(const a:TVector3;const b:TDualQuaternion):TVector3; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply({$ifdef fpc}constref{$else}const{$endif} a:TDualQuaternion;{$ifdef fpc}constref{$else}const{$endif} b:TVector4):TVector4; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Multiply(const a:TVector4;const b:TDualQuaternion):TVector4; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Divide({$ifdef fpc}constref{$else}const{$endif} a,b:TDualQuaternion):TDualQuaternion; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Divide(const a:TDualQuaternion;const b:TScalar):TDualQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Divide(const a:TScalar;const b:TDualQuaternion):TDualQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator IntDivide({$ifdef fpc}constref{$else}const{$endif} a,b:TDualQuaternion):TDualQuaternion; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator IntDivide(const a:TDualQuaternion;const b:TScalar):TDualQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator IntDivide(const a:TScalar;const b:TDualQuaternion):TDualQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a,b:TDualQuaternion):TDualQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a:TDualQuaternion;const b:TScalar):TDualQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a:TScalar;const b:TDualQuaternion):TDualQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Negative({$ifdef fpc}constref{$else}const{$endif} a:TDualQuaternion):TDualQuaternion; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       class operator Positive(const a:TDualQuaternion):TDualQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       function Flip:TDualQuaternion; {$ifdef CAN_INLINE}inline;{$endif}
       function Conjugate:TDualQuaternion; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function Inverse:TDualQuaternion; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       function Normalize:TDualQuaternion; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
       case TUInt8 of
        0:(RawQuaternions:array[0..1] of TQuaternion);
        1:(QuaternionR,QuaternionD:TQuaternion);
     end;

     PSegment=^TSegment;
     TSegment=record
      public
       Points:array[0..1] of TVector3;
       constructor Create(const p0,p1:TVector3);
       function SquaredDistanceTo(const p:TVector3):TScalar; overload;
       function SquaredDistanceTo(const p:TVector3;out Nearest:TVector3):TScalar; overload;
       procedure ClosestPointTo(const p:TVector3;out Time:TScalar;out ClosestPoint:TVector3);
       function Transform(const Transform:TMatrix4x4):TSegment; {$ifdef CAN_INLINE}inline;{$endif}
       procedure ClosestPoints(const SegmentB:TSegment;out TimeA:TScalar;out ClosestPointA:TVector3;out TimeB:TScalar;out ClosestPointB:TVector3);
       function Intersect(const SegmentB:TSegment;out TimeA,TimeB:TScalar;out IntersectionPoint:TVector3):boolean;
     end;

     PRelativeSegment=^TRelativeSegment;
     TRelativeSegment=record
      public
       Origin:TVector3;
       Delta:TVector3;
       function SquaredSegmentDistanceTo(const pOtherRelativeSegment:TRelativeSegment;out t0,t1:TScalar):TScalar;
     end;

     PTriangle=^TTriangle;
     TTriangle=record
      public
       Points:array[0..2] of TVector3;
       Normal:TVector3;
       constructor Create(const pA,pB,pC:TVector3);
       function Contains(const p:TVector3):boolean;
       procedure ProjectToVector(const Vector:TVector3;out TriangleMin,TriangleMax:TScalar);
       function ProjectToPoint(var pPoint:TVector3;out s,t:TScalar):TScalar;
       function SegmentIntersect(const Segment:TSegment;out Time:TScalar;out IntersectionPoint:TVector3):boolean;
       function ClosestPointTo(const Point:TVector3;out ClosestPoint:TVector3):boolean; overload;
       function ClosestPointTo(const Segment:TSegment;out Time:TScalar;out pClosestPointOnSegment,pClosestPointOnTriangle:TVector3):boolean; overload;
       function GetClosestPointTo(const pPoint:TVector3;out ClosestPoint:TVector3):TScalar;
       function DistanceTo(const Point:TVector3):TScalar;
       function SquaredDistanceTo(const Point:TVector3):TScalar;
       function RayIntersection(const RayOrigin,RayDirection:TVector3;var Time,u,v:TScalar):boolean;
     end;

     PSegmentTriangle=^TSegmentTriangle;
     TSegmentTriangle=record
      public
       Origin:TVector3;
       Edge0:TVector3;
       Edge1:TVector3;
       Edge2:TVector3;
       function RelativeSegmentIntersection(const pSegment:TRelativeSegment;out tS,tT0,tT1:TScalar):boolean;
       function SquaredPointTriangleDistance(const pPoint:TVector3;out pfSParam,pfTParam:TScalar):TScalar;
       function SquaredDistanceTo(const pRelativeSegment:TRelativeSegment;out segT,triT0,triT1:TScalar):TScalar;
     end;

     POBB=^TOBB;
     TOBB=packed record
      public
       Center:TVector3;
       Extents:TVector3;
       procedure ProjectToVector(const Vector:TVector3;out OBBMin,OBBMax:TScalar);
       function RelativeSegmentIntersection(const pRelativeSegment:TRelativeSegment;out fracOut:TScalar;out posOut,NormalOut:TVector3):boolean;
       function TriangleIntersection(const Triangle:TTriangle;out Position,Normal:TVector3;out Penetration:TScalar):boolean; overload;
       function TriangleIntersection(const pTriangle:TTriangle;const MTV:PVector3=nil):boolean; overload;
       case TInt32 of
        0:(
         Axis:array[0..2] of TVector3;
        );
        1:(
         Matrix:TMatrix3x3;
        );
     end;

     POBBs=^TOBBs;
     TOBBs=array[0..65535] of TOBB;

     PAABB=^TAABB;
     TAABB=record
      public
       constructor Create(const pMin,pMax:TVector3);
       constructor CreateFromOBB(const OBB:TOBB);
       function Cost:TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       function Volume:TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       function Area:TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       function Flip:TAABB;
       function SquareMagnitude:TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       function Resize(const f:TScalar):TAABB; {$ifdef CAN_INLINE}inline;{$endif}
       function Combine(const WithAABB:TAABB):TAABB; {$ifdef CAN_INLINE}inline;{$endif}
       function CombineVector3(v:TVector3):TAABB; {$ifdef CAN_INLINE}inline;{$endif}
       function DistanceTo(const ToAABB:TAABB):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       function Radius:TScalar; {$ifdef CAN_INLINE}inline;{$endif}
       function Compare(const WithAABB:TAABB):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       function Intersect(const WithAABB:TAABB;Threshold:TScalar=EPSILON):boolean; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function Contains(const AABB:TAABB):boolean; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function Contains(const Vector:TVector3):boolean; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function Touched(const Vector:TVector3;const Threshold:TScalar=1e-5):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       function GetIntersection(const WithAABB:TAABB):TAABB; {$ifdef CAN_INLINE}inline;{$endif}
       function FastRayIntersection(const Origin,Direction:TVector3):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       function RayIntersectionHitDistance(const Origin,Direction:TVector3;var HitDist:TScalar):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       function RayIntersectionHitPoint(const Origin,Direction:TVector3;var HitPoint:TVector3):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       function RayIntersection(const Origin,Direction:TVector3;var Time:TScalar):boolean; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function LineIntersection(const StartPoint,EndPoint:TVector3):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       function TriangleIntersection(const Triangle:TTriangle):boolean;
       function Transform(const Transform:TMatrix4x4):TAABB; {$ifdef CAN_INLINE}inline;{$endif}
       function MatrixMul(const Transform:TMatrix4x4):TAABB; {$ifdef CAN_INLINE}inline;{$endif}
       function ScissorRect(var Scissor:TClipRect;const mvp:TMatrix4x4;const vp:TClipRect;zcull:boolean):boolean; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function ScissorRect(var Scissor:TFloatClipRect;const mvp:TMatrix4x4;const vp:TFloatClipRect;zcull:boolean):boolean; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function MovingTest(const aAABBTo,bAABBFrom,bAABBTo:TAABB;var t:TScalar):boolean;
       function SweepTest(const bAABB:TAABB;const aV,bV:TVector3;var FirstTime,LastTime:TScalar):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       case boolean of
        false:(
         Min,Max:TVector3;
        );
        true:(
         MinMax:array[0..1] of TVector3;
        );
     end;

     PAABBs=^TAABBs;
     TAABBs=array[0..65535] of TAABB;

     PSphere=^TSphere;
     TSphere=record
      public
       Center:TVector3;
       Radius:TScalar;
       constructor Create(const pCenter:TVector3;const pRadius:TScalar);
       constructor CreateFromAABB(const pAABB:TAABB);
       constructor CreateFromFrustum(const zNear,zFar,FOV,AspectRatio:TScalar;const Position,Direction:TVector3);
       function ToAABB(const pScale:TScalar=1.0):TAABB;
       function Cull(const p:array of TPlane):boolean;
       function Contains(const b:TSphere):boolean; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function Contains(const v:TVector3):boolean; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function DistanceTo(const b:TSphere):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function DistanceTo(const b:TVector3):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function Intersect(const b:TSphere):boolean; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function Intersect(const b:TAABB):boolean; overload; {$ifdef CAN_INLINE}inline;{$endif}
       function RayIntersection(const Origin,Direction:TVector3):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       function Extends(const WithSphere:TSphere):TSphere; {$ifdef CAN_INLINE}inline;{$endif}
       function Transform(const Transform:TMatrix4x4):TSphere; {$ifdef CAN_INLINE}inline;{$endif}
       function TriangleIntersection(const Triangle:TTriangle;out Position,Normal:TVector3;out Depth:TScalar):boolean; overload;
       function TriangleIntersection(const SegmentTriangle:TSegmentTriangle;const TriangleNormal:TVector3;out Position,Normal:TVector3;out Depth:TScalar):boolean; overload;
       function SweptIntersection(const SphereB:TSphere;const VelocityA,VelocityB:TVector3;out TimeFirst,TimeLast:TScalar):boolean; {$ifdef CAN_INLINE}inline;{$endif}
     end;

     PSpheres=^TSpheres;
     TSpheres=array[0..65535] of TSphere;

     PCapsule=^TCapsule;
     TCapsule=packed record
      LineStartPoint:TVector3;
      LineEndPoint:TVector3;
      Radius:TScalar;
     end;

     PMinkowskiDescription=^TMinkowskiDescription;
     TMinkowskiDescription=record
      public
       HalfAxis:TVector4;
       Position_LM:TVector3;
     end;

     PSphereCoords=^TSphereCoords;
     TSphereCoords=record
      public
       Radius:TScalar;
       Theta:TScalar;
       Phi:TScalar;
       constructor CreateFromCartesianVector(const v:TVector3); overload;
       constructor CreateFromCartesianVector(const v:TVector4); overload;
       function ToCartesianVector:TVector3;
     end;

     Vec2=TVector2;

     Vec3=TVector3;

     Vec4=TVector4;

     Mat2=TMatrix2x2;

     Mat3=TMatrix3x3;

     Mat4=TMatrix4x4;

{$ifndef InSupraEngine}
     TPooledObject=class(TPersistent);
{$endif}

     TPropertyVector2=class(TPooledObject)
      private
       fVector:PVector2;
       function GetX:TScalar;
       function GetY:TScalar;
       function GetVector:TVector2;
       procedure SetX(const pNewValue:TScalar);
       procedure SetY(const pNewValue:TScalar);
       procedure SetVector(const pNewVector:TVector2);
      public
       constructor Create(AVector:PVector2);
       destructor Destroy; override;
       property Vector:TVector2 read GetVector write SetVector;
      published
       property x:TScalar read GetX write SetX;
       property y:TScalar read GetY write SetY;
     end;

     TPropertyVector3=class(TPooledObject)
      private
       fVector:PVector3;
       function GetX:TScalar;
       function GetY:TScalar;
       function GetZ:TScalar;
       function GetVector:TVector3;
       procedure SetX(const pNewValue:TScalar);
       procedure SetY(const pNewValue:TScalar);
       procedure SetZ(const pNewValue:TScalar);
       procedure SetVector(const pNewVector:TVector3);
      public
       constructor Create(AVector:PVector3);
       destructor Destroy; override;
       property Vector:TVector3 read GetVector write SetVector;
      published
       property x:TScalar read GetX write SetX;
       property y:TScalar read GetY write SetY;
       property z:TScalar read GetZ write SetZ;
     end;

     TPropertyVector4=class(TPooledObject)
      private
       fVector:PVector4;
       function GetX:TScalar;
       function GetY:TScalar;
       function GetZ:TScalar;
       function GetW:TScalar;
       function GetVector:TVector4;
       procedure SetX(const pNewValue:TScalar);
       procedure SetY(const pNewValue:TScalar);
       procedure SetZ(const pNewValue:TScalar);
       procedure SetW(const pNewValue:TScalar);
       procedure SetVector(const pNewVector:TVector4);
      public
       constructor Create(AVector:PVector4);
       destructor Destroy; override;
       property Vector:TVector4 read GetVector write SetVector;
      published
       property x:TScalar read GetX write SetX;
       property y:TScalar read GetY write SetY;
       property z:TScalar read GetZ write SetZ;
       property w:TScalar read GetW write SetW;
     end;

     TPropertyQuaternion=class(TPooledObject)
      private
       fQuaternion:PQuaternion;
       function GetX:TScalar;
       function GetY:TScalar;
       function GetZ:TScalar;
       function GetW:TScalar;
       function GetQuaternion:TQuaternion;
       procedure SetX(const pNewValue:TScalar);
       procedure SetY(const pNewValue:TScalar);
       procedure SetZ(const pNewValue:TScalar);
       procedure SetW(const pNewValue:TScalar);
       procedure SetQuaternion(const NewQuaternion:TQuaternion);
      public
       constructor Create(AQuaternion:PQuaternion);
       destructor Destroy; override;
       property Quaternion:TQuaternion read GetQuaternion write SetQuaternion;
      published
       property x:TScalar read GetX write SetX;
       property y:TScalar read GetY write SetY;
       property z:TScalar read GetZ write SetZ;
       property w:TScalar read GetW write SetW;
     end;

     TPropertyAngle=class(TPooledObject)
      private
       fRadianAngle:PScalar;
       function GetAngle:TScalar;
       function GetRadianAngle:TScalar;
       procedure SetAngle(const pNewValue:TScalar);
       procedure SetRadianAngle(const pNewValue:TScalar);
      public
       constructor Create(ARadianAngle:PScalar);
       destructor Destroy; override;
       property RadianAngle:TScalar read GetRadianAngle write SetRadianAngle;
      published
       property Angle:TScalar read GetAngle write SetAngle;
     end;

     TPropertyRotation3D=class(TPooledObject)
      private
       fQuaternion:PQuaternion;
       function GetX:TScalar;
       function GetY:TScalar;
       function GetZ:TScalar;
       function GetW:TScalar;
       function GetPitch:TScalar;
       function GetYaw:TScalar;
       function GetRoll:TScalar;
       function GetQuaternion:TQuaternion;
       procedure SetX(const pNewValue:TScalar);
       procedure SetY(const pNewValue:TScalar);
       procedure SetZ(const pNewValue:TScalar);
       procedure SetW(const pNewValue:TScalar);
       procedure SetPitch(const pNewValue:TScalar);
       procedure SetYaw(const pNewValue:TScalar);
       procedure SetRoll(const pNewValue:TScalar);
       procedure SetQuaternion(const NewQuaternion:TQuaternion);
      public
       constructor Create(AQuaternion:PQuaternion);
       destructor Destroy; override;
       property x:TScalar read GetX write SetX;
       property y:TScalar read GetY write SetY;
       property z:TScalar read GetZ write SetZ;
       property w:TScalar read GetW write SetW;
       property Quaternion:TQuaternion read GetQuaternion write SetQuaternion;
      published
       property Pitch:TScalar read GetPitch write SetPitch;
       property Yaw:TScalar read GetYaw write SetYaw;
       property Roll:TScalar read GetRoll write SetRoll;
     end;

     TPropertyColorRGB=class(TPooledObject)
      private
       fVector:PVector3;
       function GetR:TScalar;
       function GetG:TScalar;
       function GetB:TScalar;
       function GetVector:TVector3;
       procedure SetR(const pNewValue:TScalar);
       procedure SetG(const pNewValue:TScalar);
       procedure SetB(const pNewValue:TScalar);
       procedure SetVector(const pNewVector:TVector3);
      public
       constructor Create(AVector:PVector3);
       destructor Destroy; override;
       property Vector:TVector3 read GetVector write SetVector;
      published
       property r:TScalar read GetR write SetR;
       property g:TScalar read GetG write SetG;
       property b:TScalar read GetB write SetB;
     end;

     TPropertyColorRGBA=class(TPooledObject)
      private
       fVector:PVector4;
       function GetR:TScalar;
       function GetG:TScalar;
       function GetB:TScalar;
       function GetA:TScalar;
       function GetVector:TVector4;
       procedure SetR(const pNewValue:TScalar);
       procedure SetG(const pNewValue:TScalar);
       procedure SetB(const pNewValue:TScalar);
       procedure SetA(const pNewValue:TScalar);
       procedure SetVector(const pNewVector:TVector4);
      public
       constructor Create(AVector:PVector4);
       destructor Destroy; override;
       property Vector:TVector4 read GetVector write SetVector;
      published
       property r:TScalar read GetR write SetR;
       property g:TScalar read GetG write SetG;
       property b:TScalar read GetB write SetB;
       property a:TScalar read GetA write SetA;
     end;

const Vector2Origin:TVector2=(x:0.0;y:0.0);
      Vector2XAxis:TVector2=(x:1.0;y:0.0);
      Vector2YAxis:TVector2=(x:0.0;y:1.0);
      Vector2ZAxis:TVector2=(x:0.0;y:0.0);

      Vector3Origin:TVector3=(x:0.0;y:0.0;z:0.0);
      Vector3XAxis:TVector3=(x:1.0;y:0.0;z:0.0);
      Vector3YAxis:TVector3=(x:0.0;y:1.0;z:0.0);
      Vector3ZAxis:TVector3=(x:0.0;y:0.0;z:1.0);
      Vector3All:TVector3=(x:1.0;y:1.0;z:1.0);

      Vector4Origin:TVector4=(x:0.0;y:0.0;z:0.0;w:1.0);
      Vector4XAxis:TVector4=(x:1.0;y:0.0;z:0.0;w:1.0);
      Vector4YAxis:TVector4=(x:0.0;y:1.0;z:0.0;w:1.0);
      Vector4ZAxis:TVector4=(x:0.0;y:0.0;z:1.0;w:1.0);

      Matrix2x2Identity:TMatrix2x2=(RawComponents:((1.0,0.0),(0.0,1.0)));
      Matrix2x2Null:TMatrix2x2=(RawComponents:((0.0,0.0),(0.0,0.0)));

      Matrix3x3Identity:TMatrix3x3=(RawComponents:((1.0,0.0,0.0),(0.0,1.0,0.0),(0.0,0.0,1.0)));
      Matrix3x3Null:TMatrix3x3=(RawComponents:((0.0,0.0,0.0),(0.0,0.0,0.0),(0.0,0.0,0.0)));

      Matrix4x4Identity:TMatrix4x4=(RawComponents:((1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0,1.0)));
      Matrix4x4RightToLeftHanded:TMatrix4x4=(RawComponents:((1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,-1.0,0.0),(0.0,0.0,0,1.0)));
      Matrix4x4Flip:TMatrix4x4=(RawComponents:((0.0,0.0,-1.0,0.0),(-1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,0,1.0)));
      Matrix4x4InverseFlip:TMatrix4x4=(RawComponents:((0.0,-1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(-1.0,0.0,0,0.0),(0.0,0.0,0,1.0)));
      Matrix4x4FlipYZ:TMatrix4x4=(RawComponents:((1.0,0.0,0,0.0),(0.0,0.0,1.0,0.0),(0.0,-1.0,0.0,0.0),(0.0,0.0,0,1.0)));
      Matrix4x4InverseFlipYZ:TMatrix4x4=(RawComponents:((1.0,0.0,0,0.0),(0.0,0.0,-1.0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,0,1.0)));
      Matrix4x4Null:TMatrix4x4=(RawComponents:((0.0,0.0,0,0.0),(0.0,0.0,0,0.0),(0.0,0.0,0,0.0),(0.0,0.0,0,0.0)));
      Matrix4x4NormalizedSpace:TMatrix4x4=(RawComponents:((2.0,0.0,0,0.0),(0.0,2.0,0.0,0.0),(0.0,0.0,2.0,0.0),(-1.0,-1.0,-1.0,1.0)));

      QuaternionIdentity:TQuaternion=(x:0.0;y:0.0;z:0.0;w:1.0);

function IntLog2(x:TUInt32):TUInt32; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}

function Modulo(x,y:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
function ModuloPos(x,y:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
function IEEERemainder(x,y:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
function Modulus(x,y:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}

function Determinant4x4(const v0,v1,v2,v3:TVector4):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
function SolveQuadraticRoots(const a,b,c:TScalar;out t1,t2:TScalar):boolean; {$ifdef CAN_INLINE}inline;{$endif}
function LinearPolynomialRoot(const a,b:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
function QuadraticPolynomialRoot(const a,b,c:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
function CubicPolynomialRoot(const a,b,c,d:TScalar):TScalar;

function FloatLerp(const v1,v2,w:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}

function Cross(const a,b:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Cross(const a,b:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Cross(const a,b:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}

function Dot(const a,b:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Dot(const a,b:TVector2):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Dot(const a,b:TVector3):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Dot(const a,b:TVector4):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}

function Len(const a:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Len(const a:TVector2):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Len(const a:TVector3):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Len(const a:TVector4):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}

function Normalize(const a:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Normalize(const a:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Normalize(const a:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Normalize(const a:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}

function Minimum(const a,b:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Minimum(const a,b:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Minimum(const a,b:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Minimum(const a,b:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}

function Maximum(const a,b:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Maximum(const a,b:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Maximum(const a,b:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Maximum(const a,b:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}

function FaceForward(const N,I:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function FaceForward(const N,I:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
function FaceForward(const N,I:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
function FaceForward(const N,I:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}

function FaceForward(const N,I,Nref:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function FaceForward(const N,I,Nref:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
function FaceForward(const N,I,Nref:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
function FaceForward(const N,I,Nref:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}

function Reflect(const I,N:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Reflect(const I,N:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Reflect(const I,N:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Reflect(const I,N:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}

function Refract(const I,N:TScalar;const Eta:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Refract(const I,N:TVector2;const Eta:TScalar):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Refract(const I,N:TVector3;const Eta:TScalar):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Refract(const I,N:TVector4;const Eta:TScalar):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}

function Clamp(const Value,MinValue,MaxValue:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Clamp(const Value,MinValue,MaxValue:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Clamp(const Value,MinValue,MaxValue:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Clamp(const Value,MinValue,MaxValue:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}

function Mix(const a,b,t:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Mix(const a,b,t:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Mix(const a,b,t:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Mix(const a,b,t:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}

function Step(const Edge,Value:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Step(const Edge,Value:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Step(const Edge,Value:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
function Step(const Edge,Value:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}

function NearestStep(const Edge0,Edge1,Value:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function NearestStep(const Edge0,Edge1,Value:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
function NearestStep(const Edge0,Edge1,Value:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
function NearestStep(const Edge0,Edge1,Value:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}

function LinearStep(const Edge0,Edge1,Value:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function LinearStep(const Edge0,Edge1,Value:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
function LinearStep(const Edge0,Edge1,Value:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
function LinearStep(const Edge0,Edge1,Value:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}

function SmoothStep(const Edge0,Edge1,Value:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function SmoothStep(const Edge0,Edge1,Value:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
function SmoothStep(const Edge0,Edge1,Value:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
function SmoothStep(const Edge0,Edge1,Value:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}

function SmootherStep(const Edge0,Edge1,Value:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function SmootherStep(const Edge0,Edge1,Value:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
function SmootherStep(const Edge0,Edge1,Value:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
function SmootherStep(const Edge0,Edge1,Value:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}

function SmoothestStep(const Edge0,Edge1,Value:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function SmoothestStep(const Edge0,Edge1,Value:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
function SmoothestStep(const Edge0,Edge1,Value:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
function SmoothestStep(const Edge0,Edge1,Value:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}

function SuperSmoothestStep(const Edge0,Edge1,Value:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
function SuperSmoothestStep(const Edge0,Edge1,Value:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
function SuperSmoothestStep(const Edge0,Edge1,Value:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
function SuperSmoothestStep(const Edge0,Edge1,Value:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}

procedure DoCalculateInterval(const Vertices:PVector3s;const Count:TInt32;const Axis:TVector3;out OutMin,OutMax:TScalar);
function DoSpanIntersect(const Vertices1:PVector3s;const Count1:TInt32;const Vertices2:PVector3s;const Count2:TInt32;const AxisTest:TVector3;out AxisPenetration:TVector3):TScalar;

function BoxGetDistanceToPoint(Point:TVector3;const Center,Size:TVector3;const InvTransformMatrix,TransformMatrix:TMatrix4x4;var ClosestBoxPoint:TVector3):TScalar;
function GetDistanceFromLine(const p0,p1,p:TVector3;var Projected:TVector3;const Time:PScalar=nil):TScalar;
procedure LineClosestApproach(const pa,ua,pb,ub:TVector3;var Alpha,Beta:TScalar);
procedure ClosestLineBoxPoints(const p1,p2,c:TVector3;const ir,r:TMatrix4x4;const side:TVector3;var lret,bret:TVector3);
procedure ClosestLineSegmentPoints(const a0,a1,b0,b1:TVector3;var cp0,cp1:TVector3);
function LineSegmentIntersection(const a0,a1,b0,b1:TVector3;const p:PVector3=nil):boolean;
function LineLineIntersection(const a0,a1,b0,b1:TVector3;const pa:PVector3=nil;const pb:PVector3=nil;const ta:PScalar=nil;const tb:PScalar=nil):boolean;

function IsPointsSameSide(const p0,p1,Origin,Direction:TVector3):boolean; overload; {$ifdef CAN_INLINE}inline;{$endif}

function PointInTriangle(const p0,p1,p2,Normal,p:TVector3):boolean; overload; {$ifdef CAN_INLINE}inline;{$endif}
function PointInTriangle(const p0,p1,p2,p:TVector3):boolean; overload; {$ifdef CAN_INLINE}inline;{$endif}

function GetOverlap(const MinA,MaxA,MinB,MaxB:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}

function OldTriangleTriangleIntersection(const a0,a1,a2,b0,b1,b2:TVector3):boolean;
function TriangleTriangleIntersection(const v0,v1,v2,u0,u1,u2:TVector3):boolean;

function ClosestPointToLine(const LineStartPoint,LineEndPoint,Point:TVector3;const ClosestPointOnLine:PVector3=nil;const Time:PScalar=nil):TScalar;
function ClosestPointToAABB(const AABB:TAABB;const Point:TVector3;const ClosestPointOnAABB:PVector3=nil):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
function ClosestPointToOBB(const OBB:TOBB;const Point:TVector3;out ClosestPoint:TVector3):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
function ClosestPointToSphere(const Sphere:TSphere;const Point:TVector3;out ClosestPoint:TVector3):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
function ClosestPointToCapsule(const Capsule:TCapsule;const Point:TVector3;out ClosestPoint:TVector3;const Time:PScalar=nil):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
function ClosestPointToTriangle(const a,b,c,p:TVector3;out ClosestPoint:TVector3):TScalar;

function SquaredDistanceFromPointToAABB(const AABB:TAABB;const Point:TVector3):TScalar; {$ifdef CAN_INLINE}inline;{$endif}

function SquaredDistanceFromPointToTriangle(const p,a,b,c:TVector3):TScalar;

function IsParallel(const a,b:TVector3;const Tolerance:TScalar=1e-5):boolean; {$ifdef CAN_INLINE}inline;{$endif}

function Vector3ToAnglesLDX(v:TVector3):TVector3;

procedure AnglesToVector3LDX(const Angles:TVector3;var ForwardVector,RightVector,UpVector:TVector3);

function UnsignedAngle(const v0,v1:TVector3):TScalar; {$ifdef CAN_INLINE}inline;{$endif}

function AngleDegClamp(a:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
function AngleDegDiff(a,b:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}

function AngleClamp(a:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
function AngleDiff(a,b:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
function AngleLerp(a,b,x:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}

function InertiaTensorTransform(const Inertia,Transform:TMatrix3x3):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
function InertiaTensorParallelAxisTheorem(const Center:TVector3;const Mass:TScalar):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}

procedure OrthoNormalize(var Tangent,Bitangent,Normal:TVector3);

procedure RobustOrthoNormalize(var Tangent,Bitangent,Normal:TVector3;const Tolerance:TScalar=1e-3);

function MaxOverlaps(const Min1,Max1,Min2,Max2:TScalar;var LowerLim,UpperLim:TScalar):boolean;

function PackFP32FloatToM6E5Float(const pValue:TFloat):TUInt32;
function PackFP32FloatToM5E5Float(const pValue:TFloat):TUInt32;
function Float32ToFloat11(const pValue:TFloat):TUInt32;
function Float32ToFloat10(const pValue:TFloat):TUInt32;

function ConvertRGB32FToRGB9E5(r,g,b:TFloat):TUInt32;
function ConvertRGB32FToR11FG11FB10F(const r,g,b:TFloat):TUInt32; {$ifdef CAN_INLINE}inline;{$endif}

function PackTangentSpace(const Tangent,Bitangent,Normal:TVector3):TPackedTangentSpace;
procedure UnpackTangentSpace(var PackedTangentSpace:TPackedTangentSpace;var Tangent,Bitangent,Normal:TVector3);

implementation

function IntLog2(x:TUInt32):TUInt32; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}
begin
 if x<>0 then begin
  result:=BSRWord(x);
 end else begin
  result:=0;
 end;
end;
{$else}
{$ifdef cpu386}
asm
 test eax,eax
 jz @Done
 bsr eax,eax
 @Done:
end;{$else}{$ifdef cpux86_64}
asm
{$ifdef Windows}
 mov eax,ecx
{$else}
 mov eax,edi
{$endif}
 test eax,eax
 jz @Done
 bsr eax,eax
 @Done:
end;
{$else}
begin
 x:=x or (x shr 1);
 x:=x or (x shr 2);
 x:=x or (x shr 4);
 x:=x or (x shr 8);
 x:=x or (x shr 16);
 x:=x shr 1;
 x:=x-((x shr 1) and $55555555);
 x:=((x shr 2) and $33333333)+(x and $33333333);
 x:=((x shr 4)+x) and $0f0f0f0f;
 x:=x+(x shr 8);
 x:=x+(x shr 16);
 result:=x and $3f;
end;
{$endif}
{$endif}
{$endif}

function Modulo(x,y:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=x-(floor(x/y)*y);
end;

function ModuloPos(x,y:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
begin
 if y>0.0 then begin
  result:=Modulo(x,y);
  while result<0.0 do begin
   result:=result+y;
  end;
  while result>=y do begin
   result:=result-y;
  end;
 end else begin
  result:=x;
 end;
end;

function IEEERemainder(x,y:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=x-(round(x/y)*y);
end;

function Modulus(x,y:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=(abs(x)-(abs(y)*(floor(abs(x)/abs(y)))))*sign(x);
end;

function Determinant4x4(const v0,v1,v2,v3:TVector4):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=(v0.w*v1.z*v2.y*v3.x)-(v0.z*v1.w*v2.y*v3.x)-
         (v0.w*v1.y*v2.z*v3.x)+(v0.y*v1.w*v2.z*v3.x)+
         (v0.z*v1.y*v2.w*v3.x)-(v0.y*v1.z*v2.w*v3.x)-
         (v0.w*v1.z*v2.x*v3.y)+(v0.z*v1.w*v2.x*v3.y)+
         (v0.w*v1.x*v2.z*v3.y)-(v0.x*v1.w*v2.z*v3.y)-
         (v0.z*v1.x*v2.w*v3.y)+(v0.x*v1.z*v2.w*v3.y)+
         (v0.w*v1.y*v2.x*v3.z)-(v0.y*v1.w*v2.x*v3.z)-
         (v0.w*v1.x*v2.y*v3.z)+(v0.x*v1.w*v2.y*v3.z)+
         (v0.y*v1.x*v2.w*v3.z)-(v0.x*v1.y*v2.w*v3.z)-
         (v0.z*v1.y*v2.x*v3.w)+(v0.y*v1.z*v2.x*v3.w)+
         (v0.z*v1.x*v2.y*v3.w)-(v0.x*v1.z*v2.y*v3.w)-
         (v0.y*v1.x*v2.z*v3.w)+(v0.x*v1.y*v2.z*v3.w);
end;

function SolveQuadraticRoots(const a,b,c:TScalar;out t1,t2:TScalar):boolean; {$ifdef CAN_INLINE}inline;{$endif}
var d,InverseDenominator:TScalar;
begin
 result:=false;
 d:=sqr(b)-(4.0*(a*c));
 if d>=0.0 then begin
  InverseDenominator:=1.0/(2.0*a);
  if abs(d)<EPSILON then begin
   t1:=(-b)*InverseDenominator;
   t2:=t1;
  end else begin
   d:=sqrt(d);
   t1:=((-b)-d)*InverseDenominator;
   t2:=((-b)+d)*InverseDenominator;
  end;
  result:=true;
 end;
end;

function LinearPolynomialRoot(const a,b:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
begin
 if abs(a)>EPSILON then begin
  result:=-(b/a);
 end else begin
  result:=0.0;
 end;
end;

function QuadraticPolynomialRoot(const a,b,c:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
var d,InverseDenominator,t0,t1:TScalar;
begin
 if abs(a)>EPSILON then begin
  d:=sqr(b)-(4.0*(a*c));
  InverseDenominator:=1.0/(2.0*a);
  if d>=0.0 then begin
   if d<EPSILON then begin
    t0:=(-b)*InverseDenominator;
    t1:=t0;
   end else begin
    d:=sqrt(d);
    t0:=((-b)+d)*InverseDenominator;
    t1:=((-b)-d)*InverseDenominator;
   end;
   if abs(t0)<abs(t1) then begin
    result:=t0;
   end else begin
    result:=t1;
   end;
  end else begin
   result:=0.0;
  end;
 end else begin
  result:=LinearPolynomialRoot(b,c);
 end;
end;

function CubicPolynomialRoot(const a,b,c,d:TScalar):TScalar;
var f,g,h,hs,r,s,t,u,i,j,k,l,m,n,p,t0,t1,t2:TScalar;
begin
 if abs(a)>EPSILON then begin
  if abs(1.0-a)<EPSILON then begin
   f:=((3.0*c)-sqr(b))/3.0;
   g:=((2.0*(b*sqr(b)))-(9.0*(b*c))+(27.0*d))/27.0;
   h:=(sqr(g)*0.25)+((f*sqr(f))/27.0);
   if (abs(f)<1e-12) and (abs(h)<1e-12) and (abs(g)<1e-12) then begin
    result:=d;
    if result<0.0 then begin
     result:=power(-result,1.0/3.0);
    end else begin
     result:=-power(result,1.0/3.0);
    end;
   end else if h>0.0 then begin
    hs:=sqrt(h);
    r:=(-(g*0.5))+hs;
    if r<0.0 then begin
     s:=-power(-r,1.0/3.0);
    end else begin
     s:=power(r,1.0/3.0);
    end;
    t:=(-(g*0.5))-hs;
    if t<0.0 then begin
     u:=-power(-t,1.0/3.0);
    end else begin
     u:=power(t,1.0/3.0);
    end;
    result:=(s+u)-(b/3.0);
   end else begin
    i:=sqrt((sqr(g)/4.0)-h);
    if i<0.0 then begin
     j:=-power(-i,1.0/3.0);
    end else begin
     j:=power(i,1.0/3.0);
    end;
    k:=ArcCos(-(g/(2.0*i)));
    l:=-j;
    m:=cos(k/3.0);
    n:=sqrt(3.0)*sin(k/3.0);
    p:=-(b/3.0);
    t0:=(2.0*(j*cos(k/3.0)))-(b/3.0);
    t1:=(l*(m+n))+p;
    t2:=(l*(m-n))+p;
    if abs(t0)<abs(t1) then begin
     if abs(t0)<abs(t2) then begin
      result:=t0;
     end else begin
      result:=t2;
     end;
    end else begin
     if abs(t1)<abs(t2) then begin
      result:=t1;
     end else begin
      result:=t2;
     end;
    end;
   end;
  end else begin
   f:=((3.0*(c/a))-(sqr(b)/sqr(a)))/3.0;
   g:=(((2.0*(b*sqr(b)))/(a*sqr(a)))-((9.0*(b*c))/sqr(a))+(27.0*(d/a)))/27.0;
   h:=(sqr(g)*0.25)+((f*sqr(f))/27.0);
   if (abs(f)<1e-12) and (abs(h)<1e-12) and (abs(g)<1e-12) then begin
    result:=d/a;
    if result<0.0 then begin
     result:=power(-result,1.0/3.0);
    end else begin
     result:=-power(result,1.0/3.0);
    end;
   end else if h>0.0 then begin
    hs:=sqrt(h);
    r:=(-(g*0.5))+hs;
    if r<0.0 then begin
     s:=-power(-r,1.0/3.0);
    end else begin
     s:=power(r,1.0/3.0);
    end;
    t:=(-(g*0.5))-hs;
    if t<0.0 then begin
     u:=-power(-t,1.0/3.0);
    end else begin
     u:=power(t,1.0/3.0);
    end;
    result:=(s+u)-(b/(3.0*a));
   end else begin
    i:=sqrt((sqr(g)/4.0)-h);
    if i<0.0 then begin
     j:=-power(-i,1.0/3.0);
    end else begin
     j:=power(i,1.0/3.0);
    end;
    k:=ArcCos(-(g/(2.0*i)));
    l:=-j;
    m:=cos(k/3.0);
    n:=sqrt(3.0)*sin(k/3.0);
    p:=-(b/(3.0*a));
    t0:=(2.0*(j*cos(k/3.0)))-(b/(3.0*a));
    t1:=(l*(m+n))+p;
    t2:=(l*(m-n))+p;
    if abs(t0)<abs(t1) then begin
     if abs(t0)<abs(t2) then begin
      result:=t0;
     end else begin
      result:=t2;
     end;
    end else begin
     if abs(t1)<abs(t2) then begin
      result:=t1;
     end else begin
      result:=t2;
     end;
    end;
   end;
  end;
 end else begin
  result:=QuadraticPolynomialRoot(b,c,d);
 end;
end;

function FloatLerp(const v1,v2,w:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
begin
 if w<0.0 then begin
  result:=v1;
 end else if w>1.0 then begin
  result:=v2;
 end else begin
  result:=(v1*(1.0-w))+(v2*w);
 end;
end;

constructor TVector2.Create(const pX:TScalar);
begin
 x:=pX;
 y:=pX;
end;

constructor TVector2.Create(const pX,pY:TScalar);
begin
 x:=pX;
 y:=pY;
end;

class operator TVector2.Implicit(const a:TScalar):TVector2;
begin
 result.x:=a;
 result.y:=a;
end;

class operator TVector2.Explicit(const a:TScalar):TVector2;
begin
 result.x:=a;
 result.y:=a;
end;

class operator TVector2.Equal(const a,b:TVector2):boolean;
begin
 result:=SameValue(a.x,b.x) and SameValue(a.y,b.y);
end;

class operator TVector2.NotEqual(const a,b:TVector2):boolean;
begin
 result:=(not SameValue(a.x,b.x)) or (not SameValue(a.y,b.y));
end;

class operator TVector2.Inc(const a:TVector2):TVector2;
begin
 result.x:=a.x+1.0;
 result.y:=a.y+1.0;
end;

class operator TVector2.Dec(const a:TVector2):TVector2;
begin
 result.x:=a.x-1.0;
 result.y:=a.y-1.0;
end;

class operator TVector2.Add(const a,b:TVector2):TVector2;
begin
 result.x:=a.x+b.x;
 result.y:=a.y+b.y;
end;

class operator TVector2.Add(const a:TVector2;const b:TScalar):TVector2;
begin
 result.x:=a.x+b;
 result.y:=a.y+b;
end;

class operator TVector2.Add(const a:TScalar;const b:TVector2):TVector2;
begin
 result.x:=a+b.x;
 result.y:=a+b.y;
end;

class operator TVector2.Subtract(const a,b:TVector2):TVector2;
begin
 result.x:=a.x-b.x;
 result.y:=a.y-b.y;
end;

class operator TVector2.Subtract(const a:TVector2;const b:TScalar):TVector2;
begin
 result.x:=a.x-b;
 result.y:=a.y-b;
end;

class operator TVector2.Subtract(const a:TScalar;const b:TVector2): TVector2;
begin
 result.x:=a-b.x;
 result.y:=a-b.y;
end;

class operator TVector2.Multiply(const a,b:TVector2):TVector2;
begin
 result.x:=a.x*b.x;
 result.y:=a.y*b.y;
end;

class operator TVector2.Multiply(const a:TVector2;const b:TScalar):TVector2;
begin
 result.x:=a.x*b;
 result.y:=a.y*b;
end;

class operator TVector2.Multiply(const a:TScalar;const b:TVector2):TVector2;
begin
 result.x:=a*b.x;
 result.y:=a*b.y;
end;

class operator TVector2.Divide(const a,b:TVector2):TVector2;
begin
 result.x:=a.x/b.x;
 result.y:=a.y/b.y;
end;

class operator TVector2.Divide(const a:TVector2;const b:TScalar):TVector2;
begin
 result.x:=a.x/b;
 result.y:=a.y/b;
end;

class operator TVector2.Divide(const a:TScalar;const b:TVector2):TVector2;
begin
 result.x:=a/b.x;
 result.y:=a/b.y;
end;

class operator TVector2.IntDivide(const a,b:TVector2):TVector2;
begin
 result.x:=a.x/b.x;
 result.y:=a.y/b.y;
end;

class operator TVector2.IntDivide(const a:TVector2;const b:TScalar):TVector2;
begin
 result.x:=a.x/b;
 result.y:=a.y/b;
end;

class operator TVector2.IntDivide(const a:TScalar;const b:TVector2):TVector2;
begin
 result.x:=a/b.x;
 result.y:=a/b.y;
end;

class operator TVector2.Modulus(const a,b:TVector2):TVector2;
begin
 result.x:=Modulus(a.x,b.x);
 result.y:=Modulus(a.y,b.y);
end;

class operator TVector2.Modulus(const a:TVector2;const b:TScalar):TVector2;
begin
 result.x:=Modulus(a.x,b);
 result.y:=Modulus(a.y,b);
end;

class operator TVector2.Modulus(const a:TScalar;const b:TVector2):TVector2;
begin
 result.x:=Modulus(a,b.x);
 result.y:=Modulus(a,b.y);
end;

class operator TVector2.Negative(const a:TVector2):TVector2;
begin
 result.x:=-a.x;
 result.y:=-a.y;
end;

class operator TVector2.Positive(const a:TVector2):TVector2;
begin
 result:=a;
end;

{$i PasVulkan.Math.TVector2.Swizzle.Implementations.inc}

function TVector2.GetComponent(const pIndex:TInt32):TScalar;
begin
 result:=RawComponents[pIndex];
end;

procedure TVector2.SetComponent(const pIndex:TInt32;const pValue:TScalar);
begin
 RawComponents[pIndex]:=pValue;
end;

function TVector2.Perpendicular:TVector2;
begin
 result.x:=-y;
 result.y:=x;
end;

function TVector2.Length:TScalar;
begin
 result:=sqrt(sqr(x)+sqr(y));
end;

function TVector2.SquaredLength:TScalar;
begin
 result:=sqr(x)+sqr(y);
end;

function TVector2.Normalize:TVector2;
var Factor:TScalar;
begin
 Factor:=sqrt(sqr(x)+sqr(y));
 if Factor<>0.0 then begin
  Factor:=1.0/Factor;
  result.x:=x*Factor;
  result.y:=y*Factor;
 end else begin
  result.x:=0.0;
  result.y:=0.0;
 end;
end;

function TVector2.DistanceTo(const b:TVector2):TScalar;
begin
 result:=sqrt(sqr(x-b.x)+sqr(y-b.y));
end;

function TVector2.Dot(const b:TVector2):TScalar;
begin
 result:=(x*b.x)+(y*b.y);
end;

function TVector2.Cross(const b:TVector2):TVector2;
begin
 result.x:=(y*b.x)-(x*b.y);
 result.y:=(x*b.y)-(y*b.x);
end;

function TVector2.Lerp(const b:TVector2;const t:TScalar):TVector2;
var InvT:TScalar;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  InvT:=1.0-t;
  result.x:=(x*InvT)+(b.x*t);
  result.y:=(y*InvT)+(b.y*t);
 end;
end;

function TVector2.Angle(const b,c:TVector2):TScalar;
var DeltaAB,DeltaCB:TVector2;
    LengthAB,LengthCB:TScalar;
begin
 DeltaAB:=self-b;
 DeltaCB:=c-b;
 LengthAB:=DeltaAB.Length;
 LengthCB:=DeltaCB.Length;
 if (LengthAB=0.0) or (LengthCB=0.0) then begin
  result:=0.0;
 end else begin
  result:=ArcCos(DeltaAB.Dot(DeltaCB)/(LengthAB*LengthCB));
 end;
end;

function TVector2.Rotate(const Angle:TScalar):TVector2;
var Sinus,Cosinus:TScalar;
begin
 Sinus:=0.0;
 Cosinus:=0.0;
 SinCos(Angle,Sinus,Cosinus);
 result.x:=(x*Cosinus)-(y*Sinus);
 result.y:=(y*Cosinus)+(x*Sinus);
end;

function TVector2.Rotate(const Center:TVector2;const Angle:TScalar):TVector2;
var Sinus,Cosinus:TScalar;
begin
 Sinus:=0.0;
 Cosinus:=0.0;
 SinCos(Angle,Sinus,Cosinus);
 result.x:=(((x-Center.x)*Cosinus)-((y-Center.y)*Sinus))+Center.x;
 result.y:=(((y-Center.y)*Cosinus)+((x-Center.x)*Sinus))+Center.y;
end;

constructor TVector3.Create(const pX:TScalar);
begin
 x:=pX;
 y:=pX;
 z:=pX;
end;

constructor TVector3.Create(const pX,pY,pZ:TScalar);
begin
 x:=pX;
 y:=pY;
 z:=pZ;
end;

constructor TVector3.Create(const pXY:TVector2;const pZ:TScalar=0.0);
begin
 x:=pXY.x;
 y:=pXY.y;
 z:=pZ;
end;

class operator TVector3.Implicit(const a:TScalar):TVector3;
begin
 result.x:=a;
 result.y:=a;
 result.z:=a;
end;

class operator TVector3.Explicit(const a:TScalar):TVector3;
begin
 result.x:=a;
 result.y:=a;
 result.z:=a;
end;

class operator TVector3.Equal(const a,b:TVector3):boolean;
begin
 result:=SameValue(a.x,b.x) and SameValue(a.y,b.y) and SameValue(a.z,b.z);
end;

class operator TVector3.NotEqual(const a,b:TVector3):boolean;
begin
 result:=(not SameValue(a.x,b.x)) or (not SameValue(a.y,b.y)) or (not SameValue(a.z,b.z));
end;

class operator TVector3.Inc({$ifdef fpc}constref{$else}const{$endif} a:TVector3):TVector3;
{$if defined(cpu386) or defined(cpux64)}
const One:TFloat=1.0;
asm
 movss xmm0,dword ptr [a+0]
 movss xmm1,dword ptr [a+4]
 movss xmm2,dword ptr [a+8]
{$if defined(cpu386)}
 movss xmm3,dword ptr [One]
{$elseif defined(fpc)}
 movss xmm3,dword ptr [rip+One]
{$else}
 movss xmm3,dword ptr [rel One]
{$ifend}
 addss xmm0,xmm3
 addss xmm1,xmm3
 addss xmm2,xmm3
 movss dword ptr [result+0],xmm0
 movss dword ptr [result+4],xmm1
 movss dword ptr [result+8],xmm2
end;
{$else}
begin
 result.x:=a.x+1.0;
 result.y:=a.y+1.0;
 result.z:=a.z+1.0;
end;
{$ifend}

class operator TVector3.Dec({$ifdef fpc}constref{$else}const{$endif} a:TVector3):TVector3;
{$if defined(cpu386) or defined(cpux64)}
const One:TFloat=1.0;
asm
 movss xmm0,dword ptr [a+0]
 movss xmm1,dword ptr [a+4]
 movss xmm2,dword ptr [a+8]
{$if defined(cpu386)}
 movss xmm3,dword ptr [One]
{$elseif defined(fpc)}
 movss xmm3,dword ptr [rip+One]
{$else}
 movss xmm3,dword ptr [rel One]
{$ifend}
 subss xmm0,xmm3
 subss xmm1,xmm3
 subss xmm2,xmm3
 movss dword ptr [result+0],xmm0
 movss dword ptr [result+4],xmm1
 movss dword ptr [result+8],xmm2
end;
{$else}
begin
 result.x:=a.x-1.0;
 result.y:=a.y-1.0;
 result.z:=a.z-1.0;
end;
{$ifend}

class operator TVector3.Add({$ifdef fpc}constref{$else}const{$endif} a,b:TVector3):TVector3;
{$if defined(cpu386) or defined(cpux64)}
asm
 movss xmm0,dword ptr [a+0]
 movss xmm1,dword ptr [a+4]
 movss xmm2,dword ptr [a+8]
 addss xmm0,dword ptr [b+0]
 addss xmm1,dword ptr [b+4]
 addss xmm2,dword ptr [b+8]
 movss dword ptr [result+0],xmm0
 movss dword ptr [result+4],xmm1
 movss dword ptr [result+8],xmm2
end;
{$else}
begin
 result.x:=a.x+b.x;
 result.y:=a.y+b.y;
 result.z:=a.z+b.z;
end;
{$ifend}

class operator TVector3.Add(const a:TVector3;const b:TScalar):TVector3;
begin
 result.x:=a.x+b;
 result.y:=a.y+b;
 result.z:=a.z+b;
end;

class operator TVector3.Add(const a:TScalar;const b:TVector3):TVector3;
begin
 result.x:=a+b.x;
 result.y:=a+b.y;
 result.z:=a+b.z;
end;

class operator TVector3.Subtract({$ifdef fpc}constref{$else}const{$endif} a,b:TVector3):TVector3;
{$if defined(cpu386) or defined(cpux64)}
asm
 movss xmm0,dword ptr [a+0]
 movss xmm1,dword ptr [a+4]
 movss xmm2,dword ptr [a+8]
 subss xmm0,dword ptr [b+0]
 subss xmm1,dword ptr [b+4]
 subss xmm2,dword ptr [b+8]
 movss dword ptr [result+0],xmm0
 movss dword ptr [result+4],xmm1
 movss dword ptr [result+8],xmm2
end;
{$else}
begin
 result.x:=a.x-b.x;
 result.y:=a.y-b.y;
 result.z:=a.z-b.z;
end;
{$ifend}

class operator TVector3.Subtract(const a:TVector3;const b:TScalar):TVector3;
begin
 result.x:=a.x-b;
 result.y:=a.y-b;
 result.z:=a.z-b;
end;

class operator TVector3.Subtract(const a:TScalar;const b:TVector3): TVector3;
begin
 result.x:=a-b.x;
 result.y:=a-b.y;
 result.z:=a-b.z;
end;

class operator TVector3.Multiply({$ifdef fpc}constref{$else}const{$endif} a,b:TVector3):TVector3;
{$if defined(cpu386) or defined(cpux64)}
asm
 movss xmm0,dword ptr [a]
 movss xmm1,xmm0
 movss xmm2,xmm0
 mulss xmm0,dword ptr [b+0]
 mulss xmm1,dword ptr [b+4]
 mulss xmm2,dword ptr [b+8]
 movss dword ptr [result+0],xmm0
 movss dword ptr [result+4],xmm1
 movss dword ptr [result+8],xmm2
end;
{$else}
begin
 result.x:=a.x*b.x;
 result.y:=a.y*b.y;
 result.z:=a.z*b.z;
end;
{$ifend}

class operator TVector3.Multiply(const a:TVector3;const b:TScalar):TVector3;
begin
 result.x:=a.x*b;
 result.y:=a.y*b;
 result.z:=a.z*b;
end;

class operator TVector3.Multiply(const a:TScalar;const b:TVector3):TVector3;
begin
 result.x:=a*b.x;
 result.y:=a*b.y;
 result.z:=a*b.z;
end;

class operator TVector3.Divide({$ifdef fpc}constref{$else}const{$endif} a,b:TVector3):TVector3;
{$if defined(cpu386) or defined(cpux64)}
asm
 movss xmm0,dword ptr [a]
 movss xmm1,xmm0
 movss xmm2,xmm0
 divss xmm0,dword ptr [b+0]
 divss xmm1,dword ptr [b+4]
 divss xmm2,dword ptr [b+8]
 movss dword ptr [result+0],xmm0
 movss dword ptr [result+4],xmm1
 movss dword ptr [result+8],xmm2
end;
{$else}
begin
 result.x:=a.x/b.x;
 result.y:=a.y/b.y;
 result.z:=a.z/b.z;
end;
{$ifend}

class operator TVector3.Divide(const a:TVector3;const b:TScalar):TVector3;
begin
 result.x:=a.x/b;
 result.y:=a.y/b;
 result.z:=a.z/b;
end;

class operator TVector3.Divide(const a:TScalar;const b:TVector3):TVector3;
begin
 result.x:=a/b.x;
 result.y:=a/b.y;
 result.z:=a/b.z;
end;

class operator TVector3.IntDivide({$ifdef fpc}constref{$else}const{$endif} a,b:TVector3):TVector3;
{$if defined(cpu386) or defined(cpux64)}
asm
 movss xmm0,dword ptr [a]
 movss xmm1,xmm0
 movss xmm2,xmm0
 divss xmm0,dword ptr [b+0]
 divss xmm1,dword ptr [b+4]
 divss xmm2,dword ptr [b+8]
 movss dword ptr [result+0],xmm0
 movss dword ptr [result+4],xmm1
 movss dword ptr [result+8],xmm2
end;
{$else}
begin
 result.x:=a.x/b.x;
 result.y:=a.y/b.y;
 result.z:=a.z/b.z;
end;
{$ifend}

class operator TVector3.IntDivide(const a:TVector3;const b:TScalar):TVector3;
begin
 result.x:=a.x/b;
 result.y:=a.y/b;
 result.z:=a.z/b;
end;

class operator TVector3.IntDivide(const a:TScalar;const b:TVector3):TVector3;
begin
 result.x:=a/b.x;
 result.y:=a/b.y;
 result.z:=a/b.z;
end;

class operator TVector3.Modulus(const a,b:TVector3):TVector3;
begin
 result.x:=Modulus(a.x,b.x);
 result.y:=Modulus(a.y,b.y);
 result.z:=Modulus(a.z,b.z);
end;

class operator TVector3.Modulus(const a:TVector3;const b:TScalar):TVector3;
begin
 result.x:=Modulus(a.x,b);
 result.y:=Modulus(a.y,b);
 result.z:=Modulus(a.z,b);
end;

class operator TVector3.Modulus(const a:TScalar;const b:TVector3):TVector3;
begin
 result.x:=Modulus(a,b.x);
 result.y:=Modulus(a,b.y);
 result.z:=Modulus(a,b.z);
end;

class operator TVector3.Negative({$ifdef fpc}constref{$else}const{$endif} a:TVector3):TVector3;
{$if defined(cpu386) or defined(cpux64)}
asm
 xorps xmm0,xmm0
 xorps xmm1,xmm1
 xorps xmm2,xmm2
 subss xmm0,dword ptr [a+0]
 subss xmm1,dword ptr [a+4]
 subss xmm2,dword ptr [a+8]
 movss dword ptr [result+0],xmm0
 movss dword ptr [result+4],xmm1
 movss dword ptr [result+8],xmm2
end;
{$else}
begin
 result.x:=-a.x;
 result.y:=-a.y;
 result.z:=-a.z;
end;
{$ifend}

class operator TVector3.Positive(const a:TVector3):TVector3;
begin
 result:=a;
end;

{$i PasVulkan.Math.TVector3.Swizzle.Implementations.inc}

function TVector3.GetComponent(const pIndex:TInt32):TScalar;
begin
 result:=RawComponents[pIndex];
end;

procedure TVector3.SetComponent(const pIndex:TInt32;const pValue:TScalar);
begin
 RawComponents[pIndex]:=pValue;
end;

function TVector3.Flip:TVector3;
begin
 result.x:=x;
 result.y:=z;
 result.z:=-y;
end;

function TVector3.Perpendicular:TVector3;
var v,p:TVector3;
begin
 v:=p.Normalize;
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

function TVector3.OneUnitOrthogonalVector:TVector3;
var MinimumAxis:TInt32;
    l:TScalar;
begin
 if System.abs(x)<System.abs(y) then begin
  if System.abs(x)<System.abs(z) then begin
   MinimumAxis:=0;
  end else begin
   MinimumAxis:=2;
  end;
 end else begin
  if System.abs(y)<System.abs(z) then begin
   MinimumAxis:=1;
  end else begin
   MinimumAxis:=2;
  end;
 end;
 case MinimumAxis of
  0:begin
   l:=sqrt(sqr(y)+sqr(z));
   result.x:=0.0;
   result.y:=-(z/l);
   result.z:=y/l;
  end;
  1:begin
   l:=sqrt(sqr(x)+sqr(z));
   result.x:=-(z/l);
   result.y:=0.0;
   result.z:=x/l;
  end;
  else begin
   l:=sqrt(sqr(x)+sqr(y));
   result.x:=-(y/l);
   result.y:=x/l;
   result.z:=0.0;
  end;
 end;
end;

function TVector3.Length:TScalar;
{$if defined(cpu386)}
asm
 xorps xmm2,xmm2
 movss xmm0,dword ptr [eax+0]
 movss xmm1,dword ptr [eax+4]
 movss xmm2,dword ptr [eax+8]
 movlhps xmm0,xmm1
 shufps xmm0,xmm2,$88
 mulps xmm0,xmm0         // xmm0 = ?, z*z, y*y, x*x
 movhlps xmm1,xmm0       // xmm1 = ?, ?, ?, z*z
 addss xmm1,xmm0         // xmm1 = ?, ?, ?, z*z + x*x
 shufps xmm0,xmm0,$55    // xmm0 = ?, ?, ?, y*y
 addss xmm1,xmm0         // xmm1 = ?, ?, ?, z*z + y*y + x*x
 sqrtss xmm0,xmm1
 movss dword ptr [result],xmm0
end;
{$elseif defined(cpux64)}
asm
 xorps xmm2,xmm2
{$ifdef Windows}
 movss xmm0,dword ptr [rcx+0]
 movss xmm1,dword ptr [rcx+4]
 movss xmm2,dword ptr [rcx+8]
{$else}
 movss xmm0,dword ptr [rdi+0]
 movss xmm1,dword ptr [rdi+4]
 movss xmm2,dword ptr [rdi+8]
{$endif}
 movlhps xmm0,xmm1
 shufps xmm0,xmm2,$88
 mulps xmm0,xmm0         // xmm0 = ?, z*z, y*y, x*x
 movhlps xmm1,xmm0       // xmm1 = ?, ?, ?, z*z
 addss xmm1,xmm0         // xmm1 = ?, ?, ?, z*z + x*x
 shufps xmm0,xmm0,$55    // xmm0 = ?, ?, ?, y*y
 addss xmm1,xmm0         // xmm1 = ?, ?, ?, z*z + y*y + x*x
 sqrtss xmm0,xmm1
 movss dword ptr [result],xmm0
end;
{$else}
begin
 result:=sqrt(sqr(x)+sqr(y)+sqr(z));
end;
{$ifend}

function TVector3.SquaredLength:TScalar;
{$if defined(cpu386)}
asm
 xorps xmm2,xmm2
 movss xmm0,dword ptr [eax+0]
 movss xmm1,dword ptr [eax+4]
 movss xmm2,dword ptr [eax+8]
 movlhps xmm0,xmm1
 shufps xmm0,xmm2,$88
 mulps xmm0,xmm0         // xmm0 = ?, z*z, y*y, x*x
 movhlps xmm1,xmm0       // xmm1 = ?, ?, ?, z*z
 addss xmm1,xmm0         // xmm1 = ?, ?, ?, z*z + x*x
 shufps xmm0,xmm0,$55    // xmm0 = ?, ?, ?, y*y
 addss xmm0,xmm1         // xmm0 = ?, ?, ?, z*z + y*y + x*x
 movss dword ptr [result],xmm0
end;
{$elseif defined(cpux64)}
asm
 xorps xmm2,xmm2
{$ifdef Windows}
 movss xmm0,dword ptr [rcx+0]
 movss xmm1,dword ptr [rcx+4]
 movss xmm2,dword ptr [rcx+8]
{$else}
 movss xmm0,dword ptr [rdi+0]
 movss xmm1,dword ptr [rdi+4]
 movss xmm2,dword ptr [rdi+8]
{$endif}
 movlhps xmm0,xmm1
 shufps xmm0,xmm2,$88
 mulps xmm0,xmm0         // xmm0 = ?, z*z, y*y, x*x
 movhlps xmm1,xmm0       // xmm1 = ?, ?, ?, z*z
 addss xmm1,xmm0         // xmm1 = ?, ?, ?, z*z + x*x
 shufps xmm0,xmm0,$55    // xmm0 = ?, ?, ?, y*y
 addss xmm0,xmm1         // xmm0 = ?, ?, ?, z*z + y*y + x*x
 movss dword ptr [result],xmm0
end;
{$else}
begin
 result:=sqr(x)+sqr(y)+sqr(z);
end;
{$ifend}

function TVector3.Normalize:TVector3;
{$if defined(cpu386)}
asm
 xorps xmm2,xmm2
 movss xmm0,dword ptr [eax+0]
 movss xmm1,dword ptr [eax+4]
 movss xmm2,dword ptr [eax+8]
 movlhps xmm0,xmm1
 shufps xmm0,xmm2,$88
 movaps xmm2,xmm0
 mulps xmm0,xmm0         // xmm0 = ?, z*z, y*y, x*x
 movhlps xmm1,xmm0       // xmm1 = ?, ?, ?, z*z
 addss xmm1,xmm0         // xmm1 = ?, ?, ?, z*z + x*x
 shufps xmm0,xmm0,$55    // xmm0 = ?, ?, ?, y*y
 addss xmm1,xmm0         // xmm1 = ?, ?, ?, z*z + y*y + x*x
 sqrtss xmm0,xmm1        // not rsqrtss! because rsqrtss has only 12-bit accuracy
 shufps xmm0,xmm0,$00
 divps xmm2,xmm0
 movaps xmm1,xmm2
 subps xmm1,xmm2
 cmpps xmm1,xmm2,7
 andps xmm2,xmm1
 movaps xmm0,xmm2
 movaps xmm1,xmm2
 shufps xmm1,xmm1,$55
 shufps xmm2,xmm2,$aa
 movss dword ptr [result+0],xmm0
 movss dword ptr [result+4],xmm1
 movss dword ptr [result+8],xmm2
end;
{$elseif defined(cpux64)}
asm
 xorps xmm2,xmm2
{$ifdef Windows}
 movss xmm0,dword ptr [rcx+0]
 movss xmm1,dword ptr [rcx+4]
 movss xmm2,dword ptr [rcx+8]
{$else}
 movss xmm0,dword ptr [rdi+0]
 movss xmm1,dword ptr [rdi+4]
 movss xmm2,dword ptr [rdi+8]
{$endif}
 movlhps xmm0,xmm1
 shufps xmm0,xmm2,$88
 movaps xmm2,xmm0
 mulps xmm0,xmm0         // xmm0 = ?, z*z, y*y, x*x
 movhlps xmm1,xmm0       // xmm1 = ?, ?, ?, z*z
 addss xmm1,xmm0         // xmm1 = ?, ?, ?, z*z + x*x
 shufps xmm0,xmm0,$55    // xmm0 = ?, ?, ?, y*y
 addss xmm1,xmm0         // xmm1 = ?, ?, ?, z*z + y*y + x*x
 sqrtss xmm0,xmm1        // not rsqrtss! because rsqrtss has only 12-bit accuracy
 shufps xmm0,xmm0,$00
 divps xmm2,xmm0
 movaps xmm1,xmm2
 subps xmm1,xmm2
 cmpps xmm1,xmm2,7
 andps xmm2,xmm1
 movaps xmm0,xmm2
 movaps xmm1,xmm2
 shufps xmm1,xmm1,$55
 shufps xmm2,xmm2,$aa
 movss dword ptr [result+0],xmm0
 movss dword ptr [result+4],xmm1
 movss dword ptr [result+8],xmm2
end;
{$else}
var Factor:TScalar;
begin
 Factor:=sqrt(sqr(x)+sqr(y)+sqr(z));
 if Factor<>0.0 then begin
  Factor:=1.0/Factor;
  result.x:=x*Factor;
  result.y:=y*Factor;
  result.z:=z*Factor;
 end else begin
  result.x:=0.0;
  result.y:=0.0;
  result.z:=0.0;
 end;
end;
{$ifend}

function TVector3.DistanceTo({$ifdef fpc}constref{$else}const{$endif} b:TVector3):TScalar;
{$if defined(cpu386)}
asm
 xorps xmm2,xmm2
 movss xmm0,dword ptr [eax+0]
 movss xmm1,dword ptr [eax+4]
 movss xmm2,dword ptr [eax+8]
 subss xmm0,dword ptr [edx+0]
 subss xmm1,dword ptr [edx+4]
 subss xmm2,dword ptr [edx+8]
 movlhps xmm0,xmm1
 shufps xmm0,xmm2,$88
 mulps xmm0,xmm0         // xmm0 = ?, z*z, y*y, x*x
 movhlps xmm1,xmm0       // xmm1 = ?, ?, ?, z*z
 addss xmm1,xmm0         // xmm1 = ?, ?, ?, z*z + x*x
 shufps xmm0,xmm0,$55    // xmm0 = ?, ?, ?, y*y
 addss xmm1,xmm0         // xmm1 = ?, ?, ?, z*z + y*y + x*x
 sqrtss xmm0,xmm1
 movss dword ptr [result],xmm0
end;
{$elseif defined(cpux64)}
asm
 xorps xmm2,xmm2
{$ifdef Windows}
 movss xmm0,dword ptr [rcx+0]
 movss xmm1,dword ptr [rcx+4]
 movss xmm2,dword ptr [rcx+8]
 subss xmm0,dword ptr [rdx+0]
 subss xmm1,dword ptr [rdx+4]
 subss xmm2,dword ptr [rdx+8]
{$else}
 movss xmm0,dword ptr [rdi+0]
 movss xmm1,dword ptr [rdi+4]
 movss xmm2,dword ptr [rdi+8]
 subss xmm0,dword ptr [rsi+0]
 subss xmm1,dword ptr [rsi+4]
 subss xmm2,dword ptr [rsi+8]
{$endif}
 movlhps xmm0,xmm1
 shufps xmm0,xmm2,$88
 mulps xmm0,xmm0         // xmm0 = ?, z*z, y*y, x*x
 movhlps xmm1,xmm0       // xmm1 = ?, ?, ?, z*z
 addss xmm1,xmm0         // xmm1 = ?, ?, ?, z*z + x*x
 shufps xmm0,xmm0,$55    // xmm0 = ?, ?, ?, y*y
 addss xmm1,xmm0         // xmm1 = ?, ?, ?, z*z + y*y + x*x
 sqrtss xmm0,xmm1
 movss dword ptr [result],xmm0
end;
{$else}
begin
 result:=sqrt(sqr(x-b.x)+sqr(y-b.y)+sqr(z-b.z));
end;
{$ifend}

function TVector3.Abs:TVector3;
{$if defined(cpu386)}
asm
 movss xmm0,dword ptr [eax+0]
 movss xmm1,dword ptr [eax+4]
 movss xmm2,dword ptr [eax+8]
 movlhps xmm0,xmm1
 shufps xmm0,xmm2,$88
 xorps xmm1,xmm1
 subps xmm1,xmm0
 maxps xmm0,xmm1
 movaps xmm1,xmm0
 movaps xmm2,xmm0
 shufps xmm1,xmm1,$55
 shufps xmm2,xmm2,$aa
 movss dword ptr [result+0],xmm0
 movss dword ptr [result+4],xmm1
 movss dword ptr [result+8],xmm2
end;
{$elseif defined(cpux64)}
asm
{$ifdef Windows}
 movss xmm0,dword ptr [ecx+0]
 movss xmm1,dword ptr [ecx+4]
 movss xmm2,dword ptr [ecx+8]
{$else}
 movss xmm0,dword ptr [edi+0]
 movss xmm1,dword ptr [edi+4]
 movss xmm2,dword ptr [edi+8]
{$endif}
 movlhps xmm0,xmm1
 shufps xmm0,xmm2,$88
 xorps xmm1,xmm1
 subps xmm1,xmm0
 maxps xmm0,xmm1
 movaps xmm1,xmm0
 movaps xmm2,xmm0
 shufps xmm1,xmm1,$55
 shufps xmm2,xmm2,$aa
 movss dword ptr [result+0],xmm0
 movss dword ptr [result+4],xmm1
 movss dword ptr [result+8],xmm2
end;
{$else}
begin
 result.x:=System.abs(x);
 result.y:=System.abs(y);
 result.z:=System.abs(z);
end;
{$ifend}

function TVector3.Dot({$ifdef fpc}constref{$else}const{$endif} b:TVector3):TScalar;
{$if defined(cpu386)}
asm
 movss xmm0,dword ptr [eax+0]
 movss xmm1,dword ptr [eax+4]
 movss xmm2,dword ptr [eax+8]
 mulss xmm0,dword ptr [edx+0]
 mulss xmm1,dword ptr [edx+4]
 mulss xmm2,dword ptr [edx+8]
 addss xmm0,xmm1
 addss xmm0,xmm2
 movss dword ptr [result],xmm0
end;
{$elseif defined(cpux64)}
asm
{$ifdef Windows}
 movss xmm0,dword ptr [rcx+0]
 movss xmm1,dword ptr [rcx+4]
 movss xmm2,dword ptr [rcx+8]
 mulss xmm0,dword ptr [rdx+0]
 mulss xmm1,dword ptr [rdx+4]
 mulss xmm2,dword ptr [rdx+8]
{$else}
 movss xmm0,dword ptr [rdi+0]
 movss xmm1,dword ptr [rdi+4]
 movss xmm2,dword ptr [rdi+8]
 mulss xmm0,dword ptr [rsi+0]
 mulss xmm1,dword ptr [rsi+4]
 mulss xmm2,dword ptr [rsi+8]
{$endif}
 addss xmm0,xmm1
 addss xmm0,xmm2
 movss dword ptr [result],xmm0
end;
{$else}
begin
 result:=(x*b.x)+(y*b.y)+(z*b.z);
end;
{$ifend}


function TVector3.AngleTo(const b:TVector3):TScalar;
var d:single;
begin
 d:=sqrt(SquaredLength*b.SquaredLength);
 if d<>0.0 then begin
  result:=Dot(b)/d;
 end else begin
  result:=0.0;
 end
end;

function TVector3.Cross({$ifdef fpc}constref{$else}const{$endif} b:TVector3):TVector3;
{$if defined(cpu386)}
asm
{$ifdef SSEVector3CrossOtherVariant}
 xorps xmm2,xmm2
 xorps xmm4,xmm4
 movss xmm0,dword ptr [eax+0]
 movss xmm1,dword ptr [eax+4]
 movss xmm2,dword ptr [eax+8]
 movlhps xmm0,xmm1
 shufps xmm0,xmm2,$88
 movss xmm2,dword ptr [edx+0]
 movss xmm3,dword ptr [edx+4]
 movss xmm4,dword ptr [edx+8]
 movlhps xmm2,xmm3
 shufps xmm2,xmm4,$88
 movaps xmm1,xmm0
 movaps xmm3,xmm2
 shufps xmm0,xmm0,$c9
 shufps xmm1,xmm1,$d2
 shufps xmm2,xmm2,$d2
 shufps xmm3,xmm3,$c9
 mulps xmm0,xmm2
 mulps xmm1,xmm3
 subps xmm0,xmm1
 movaps xmm1,xmm0
 movaps xmm2,xmm0
 shufps xmm1,xmm1,$55
 shufps xmm2,xmm2,$aa
 movss dword ptr [ecx+0],xmm0
 movss dword ptr [ecx+4],xmm1
 movss dword ptr [ecx+8],xmm2
{$else}
 xorps xmm2,xmm2
 xorps xmm3,xmm3
 movss xmm0,dword ptr [eax+0]
 movss xmm1,dword ptr [eax+4]
 movss xmm2,dword ptr [eax+8]
 movlhps xmm0,xmm1
 shufps xmm0,xmm2,$88
 movss xmm1,dword ptr [edx+0]
 movss xmm2,dword ptr [edx+4]
 movss xmm3,dword ptr [edx+8]
 movlhps xmm1,xmm2
 shufps xmm1,xmm3,$88
 movaps xmm2,xmm0
 movaps xmm3,xmm1
 shufps xmm0,xmm0,$12
 shufps xmm1,xmm1,$09
 shufps xmm2,xmm2,$09
 shufps xmm3,xmm3,$12
 mulps xmm0,xmm1
 mulps xmm2,xmm3
 subps xmm2,xmm0
 movaps xmm0,xmm2
 movaps xmm1,xmm2
 shufps xmm1,xmm1,$55
 shufps xmm2,xmm2,$aa
 movss dword ptr [ecx+0],xmm0
 movss dword ptr [ecx+4],xmm1
 movss dword ptr [ecx+8],xmm2
{$endif}
end;
{$elseif defined(cpux64)}
asm
{$ifdef SSEVector3CrossOtherVariant}
{$ifdef Windows}
 xorps xmm2,xmm2
 xorps xmm4,xmm4
 movss xmm0,dword ptr [rcx+0]
 movss xmm1,dword ptr [rcx+4]
 movss xmm2,dword ptr [rcx+8]
 movlhps xmm0,xmm1
 shufps xmm0,xmm2,$88
 movss xmm2,dword ptr [r8+0]
 movss xmm3,dword ptr [r8+4]
 movss xmm4,dword ptr [r8+8]
 movlhps xmm2,xmm3
 shufps xmm2,xmm4,$88
{$else}
 xorps xmm2,xmm2
 xorps xmm4,xmm4
 movss xmm0,dword ptr [rdi+0]
 movss xmm1,dword ptr [rdi+4]
 movss xmm2,dword ptr [rdi+8]
 movlhps xmm0,xmm1
 shufps xmm0,xmm2,$88
 movss xmm2,dword ptr [rsi+0]
 movss xmm3,dword ptr [rsi+4]
 movss xmm4,dword ptr [rsi+8]
 movlhps xmm2,xmm3
 shufps xmm2,xmm4,$88
{$endif}
 movaps xmm1,xmm0
 movaps xmm3,xmm2
 shufps xmm0,xmm0,$c9
 shufps xmm1,xmm1,$d2
 shufps xmm2,xmm2,$d2
 shufps xmm3,xmm3,$c9
 mulps xmm0,xmm2
 mulps xmm1,xmm3
 subps xmm0,xmm1
 movaps xmm1,xmm0
 movaps xmm2,xmm0
 shufps xmm1,xmm1,$55
 shufps xmm2,xmm2,$aa
{$ifdef Windows}
 movss dword ptr [rdx+0],xmm0
 movss dword ptr [rdx+4],xmm1
 movss dword ptr [rdx+8],xmm2
{$else}
 movss dword ptr [rax+0],xmm0
 movss dword ptr [rax+4],xmm1
 movss dword ptr [rax+8],xmm2
{$endif}
{$else}
{$ifdef Windows}
 xorps xmm2,xmm2
 xorps xmm3,xmm3
 movss xmm0,dword ptr [rcx+0]
 movss xmm1,dword ptr [rcx+4]
 movss xmm2,dword ptr [rcx+8]
 movlhps xmm0,xmm1
 shufps xmm0,xmm2,$88
 movss xmm1,dword ptr [r8+0]
 movss xmm2,dword ptr [r8+4]
 movss xmm3,dword ptr [r8+8]
 movlhps xmm1,xmm2
 shufps xmm1,xmm3,$88
{$else}
 xorps xmm2,xmm2
 xorps xmm3,xmm3
 movss xmm0,dword ptr [rdi+0]
 movss xmm1,dword ptr [rdi+4]
 movss xmm2,dword ptr [rdi+8]
 movlhps xmm0,xmm1
 shufps xmm0,xmm2,$88
 movss xmm1,dword ptr [rsi+0]
 movss xmm2,dword ptr [rsi+4]
 movss xmm3,dword ptr [rsi+8]
 movlhps xmm1,xmm2
 shufps xmm1,xmm3,$88
{$endif}
 movaps xmm2,xmm0
 movaps xmm3,xmm1
 shufps xmm0,xmm0,$12
 shufps xmm1,xmm1,$09
 shufps xmm2,xmm2,$09
 shufps xmm3,xmm3,$12
 mulps xmm0,xmm1
 mulps xmm2,xmm3
 subps xmm2,xmm0
 movaps xmm0,xmm2
 movaps xmm1,xmm2
 shufps xmm1,xmm1,$55
 shufps xmm2,xmm2,$aa
{$ifdef Windows}
 movss dword ptr [rdx+0],xmm0
 movss dword ptr [rdx+4],xmm1
 movss dword ptr [rdx+8],xmm2
{$else}
 movss dword ptr [rax+0],xmm0
 movss dword ptr [rax+4],xmm1
 movss dword ptr [rax+8],xmm2
{$endif}
{$endif}
end;
{$else}
begin
 result.x:=(y*b.z)-(z*b.y);
 result.y:=(z*b.x)-(x*b.z);
 result.z:=(x*b.y)-(y*b.x);
end;
{$ifend}

function TVector3.Lerp(const b:TVector3;const t:TScalar):TVector3;
var InvT:TScalar;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  InvT:=1.0-t;
  result:=(self*InvT)+(b*t);
 end;
end;

function TVector3.Angle(const b,c:TVector3):TScalar;
var DeltaAB,DeltaCB:TVector3;
    LengthAB,LengthCB:TScalar;
begin
 DeltaAB:=self-b;
 DeltaCB:=c-b;
 LengthAB:=DeltaAB.Length;
 LengthCB:=DeltaCB.Length;
 if (LengthAB=0.0) or (LengthCB=0.0) then begin
  result:=0.0;
 end else begin
  result:=ArcCos(DeltaAB.Dot(DeltaCB)/(LengthAB*LengthCB));
 end;
end;

function TVector3.RotateX(const Angle:TScalar):TVector3;
var Sinus,Cosinus:TScalar;
begin
 Sinus:=0.0;
 Cosinus:=0.0;
 SinCos(Angle,Sinus,Cosinus);
 result.x:=x;
 result.y:=(y*Cosinus)-(z*Sinus);
 result.z:=(y*Sinus)+(z*Cosinus);
end;

function TVector3.RotateY(const Angle:TScalar):TVector3;
var Sinus,Cosinus:TScalar;
begin
 Sinus:=0.0;
 Cosinus:=0.0;
 SinCos(Angle,Sinus,Cosinus);
 result.x:=(z*Sinus)+(x*Cosinus);
 result.y:=y;
 result.z:=(z*Cosinus)-(x*Sinus);
end;

function TVector3.RotateZ(const Angle:TScalar):TVector3;
var Sinus,Cosinus:TScalar;
begin
 Sinus:=0.0;
 Cosinus:=0.0;
 SinCos(Angle,Sinus,Cosinus);
 result.x:=(x*Cosinus)-(y*Sinus);
 result.y:=(x*Sinus)+(y*Cosinus);
 result.z:=z;
end;

function TVector3.ProjectToBounds(const MinVector,MaxVector:TVector3):TScalar;
begin
 if x<0.0 then begin
  result:=x*MaxVector.x;
 end else begin
  result:=x*MinVector.x;
 end;
 if y<0.0 then begin
  result:=result+(y*MaxVector.y);
 end else begin
  result:=result+(y*MinVector.y);
 end;
 if z<0.0 then begin
  result:=result+(z*MaxVector.z);
 end else begin
  result:=result+(z*MinVector.z);
 end;
end;

constructor TVector4.Create(const pX:TScalar);
begin
 x:=pX;
 y:=pX;
 z:=pX;
 w:=pX;
end;

constructor TVector4.Create(const pX,pY,pZ,pW:TScalar);
begin
 x:=pX;
 y:=pY;
 z:=pZ;
 w:=pW;
end;

constructor TVector4.Create(const pXY:TVector2;const pZ:TScalar=0.0;const pW:TScalar=1.0);
begin
 x:=pXY.x;
 y:=pXY.y;
 z:=pZ;
 w:=pW;
end;

constructor TVector4.Create(const pXYZ:TVector3;const pW:TScalar=1.0);
begin
 x:=pXYZ.x;
 y:=pXYZ.y;
 z:=pXYZ.z;
 w:=pW;
end;

class operator TVector4.Implicit(const a:TScalar):TVector4;
begin
 result.x:=a;
 result.y:=a;
 result.z:=a;
 result.w:=a;
end;

class operator TVector4.Explicit(const a:TScalar):TVector4;
begin
 result.x:=a;
 result.y:=a;
 result.z:=a;
 result.w:=a;
end;

class operator TVector4.Equal(const a,b:TVector4):boolean;
begin
 result:=SameValue(a.x,b.x) and SameValue(a.y,b.y) and SameValue(a.z,b.z) and SameValue(a.w,b.w);
end;

class operator TVector4.NotEqual(const a,b:TVector4):boolean;
begin
 result:=(not SameValue(a.x,b.x)) or (not SameValue(a.y,b.y)) or (not SameValue(a.z,b.z)) or (not SameValue(a.w,b.w));
end;

class operator TVector4.Inc({$ifdef fpc}constref{$else}const{$endif} a:TVector4):TVector4;
{$if defined(cpu386)}
const One:TVector4=(x:1.0;y:1.0;z:1.0;w:1.0);
asm
 movups xmm0,dqword ptr [a]
 movups xmm1,dqword ptr [One]
 addps xmm0,xmm1
 movups dqword ptr [result],xmm0
end;
{$elseif defined(cpux64)}
const One:TVector4=(x:1.0;y:1.0;z:1.0;w:1.0);
asm
 movups xmm0,dqword ptr [a]
{$ifdef fpc}
 movups xmm1,dqword ptr [rip+One]
{$else}
 movups xmm1,dqword ptr [rel One]
{$endif}
 addps xmm0,xmm1
 movups dqword ptr [result],xmm0
end;
{$else}
begin
 result.x:=a.x+1.0;
 result.y:=a.y+1.0;
 result.z:=a.z+1.0;
 result.w:=a.w+1.0;
end;
{$ifend}

class operator TVector4.Dec({$ifdef fpc}constref{$else}const{$endif} a:TVector4):TVector4;
{$if defined(cpu386)}
const One:TVector4=(x:1.0;y:1.0;z:1.0;w:1.0);
asm
 movups xmm0,dqword ptr [a]
 movups xmm1,dqword ptr [One]
 subps xmm0,xmm1
 movups dqword ptr [result],xmm0
end;
{$elseif defined(cpux64)}
const One:TVector4=(x:1.0;y:1.0;z:1.0;w:1.0);
asm
 movups xmm0,dqword ptr [a]
{$ifdef fpc}
 movups xmm1,dqword ptr [rip+One]
{$else}
 movups xmm1,dqword ptr [rel One]
{$endif}
 subps xmm0,xmm1
 movups dqword ptr [result],xmm0
end;
{$else}
begin
 result.x:=a.x-1.0;
 result.y:=a.y-1.0;
 result.z:=a.z-1.0;
 result.w:=a.w-1.0;
end;
{$ifend}

class operator TVector4.Add({$ifdef fpc}constref{$else}const{$endif} a,b:TVector4):TVector4;
{$if defined(cpu386) or defined(cpux64)}
asm
 movups xmm0,dqword ptr [a]
 movups xmm1,dqword ptr [b]
 addps xmm0,xmm1
 movups dqword ptr [result],xmm0
end;
{$else}
begin
 result.x:=a.x+b.x;
 result.y:=a.y+b.y;
 result.z:=a.z+b.z;
 result.w:=a.w+b.w;
end;
{$ifend}

class operator TVector4.Add(const a:TVector4;const b:TScalar):TVector4;
begin
 result.x:=a.x+b;
 result.y:=a.y+b;
 result.z:=a.z+b;
 result.w:=a.w+b;
end;

class operator TVector4.Add(const a:TScalar;const b:TVector4):TVector4;
begin
 result.x:=a+b.x;
 result.y:=a+b.y;
 result.z:=a+b.z;
 result.w:=a+b.w;
end;

class operator TVector4.Subtract({$ifdef fpc}constref{$else}const{$endif} a,b:TVector4):TVector4;
{$if defined(cpu386) or defined(cpux64)}
asm
 movups xmm0,dqword ptr [a]
 movups xmm1,dqword ptr [b]
 subps xmm0,xmm1
 movups dqword ptr [result],xmm0
end;
{$else}
begin
 result.x:=a.x-b.x;
 result.y:=a.y-b.y;
 result.z:=a.z-b.z;
 result.w:=a.w-b.w;
end;
{$ifend}

class operator TVector4.Subtract(const a:TVector4;const b:TScalar):TVector4;
begin
 result.x:=a.x-b;
 result.y:=a.y-b;
 result.z:=a.z-b;
 result.w:=a.w-b;
end;

class operator TVector4.Subtract(const a:TScalar;const b:TVector4): TVector4;
begin
 result.x:=a-b.x;
 result.y:=a-b.y;
 result.z:=a-b.z;
 result.w:=a-b.w;
end;

class operator TVector4.Multiply({$ifdef fpc}constref{$else}const{$endif} a,b:TVector4):TVector4;
{$if defined(cpu386) or defined(cpux64)}
asm
 movups xmm0,dqword ptr [a]
 movups xmm1,dqword ptr [b]
 mulps xmm0,xmm1
 movups dqword ptr [result],xmm0
end;
{$else}
begin
 result.x:=a.x*b.x;
 result.y:=a.y*b.y;
 result.z:=a.z*b.z;
 result.w:=a.w*b.w;
end;
{$ifend}

class operator TVector4.Multiply(const a:TVector4;const b:TScalar):TVector4;
begin
 result.x:=a.x*b;
 result.y:=a.y*b;
 result.z:=a.z*b;
 result.w:=a.w*b;
end;

class operator TVector4.Multiply(const a:TScalar;const b:TVector4):TVector4;
begin
 result.x:=a*b.x;
 result.y:=a*b.y;
 result.z:=a*b.z;
 result.w:=a*b.w;
end;

class operator TVector4.Divide({$ifdef fpc}constref{$else}const{$endif} a,b:TVector4):TVector4;
{$if defined(cpu386) or defined(cpux64)}
asm
 movups xmm0,dqword ptr [a]
 movups xmm1,dqword ptr [b]
 divps xmm0,xmm1
 movups dqword ptr [result],xmm0
end;
{$else}
begin
 result.x:=a.x/b.x;
 result.y:=a.y/b.y;
 result.z:=a.z/b.z;
 result.w:=a.w/b.w;
end;
{$ifend}

class operator TVector4.Divide(const a:TVector4;const b:TScalar):TVector4;
begin
 result.x:=a.x/b;
 result.y:=a.y/b;
 result.z:=a.z/b;
 result.w:=a.w/b;
end;

class operator TVector4.Divide(const a:TScalar;const b:TVector4):TVector4;
begin
 result.x:=a/b.x;
 result.y:=a/b.y;
 result.z:=a/b.z;
 result.w:=a/b.z;
end;

class operator TVector4.IntDivide({$ifdef fpc}constref{$else}const{$endif} a,b:TVector4):TVector4;
{$if defined(cpu386) or defined(cpux64)}
asm
 movups xmm0,dqword ptr [a]
 movups xmm1,dqword ptr [b]
 divps xmm0,xmm1
 movups dqword ptr [result],xmm0
end;
{$else}
begin
 result.x:=a.x/b.x;
 result.y:=a.y/b.y;
 result.z:=a.z/b.z;
 result.w:=a.w/b.w;
end;
{$ifend}

class operator TVector4.IntDivide(const a:TVector4;const b:TScalar):TVector4;
begin
 result.x:=a.x/b;
 result.y:=a.y/b;
 result.z:=a.z/b;
 result.w:=a.w/b;
end;

class operator TVector4.IntDivide(const a:TScalar;const b:TVector4):TVector4;
begin
 result.x:=a/b.x;
 result.y:=a/b.y;
 result.z:=a/b.z;
 result.w:=a/b.w;
end;

class operator TVector4.Modulus(const a,b:TVector4):TVector4;
begin
 result.x:=Modulus(a.x,b.x);
 result.y:=Modulus(a.y,b.y);
 result.z:=Modulus(a.z,b.z);
 result.w:=Modulus(a.w,b.w);
end;

class operator TVector4.Modulus(const a:TVector4;const b:TScalar):TVector4;
begin
 result.x:=Modulus(a.x,b);
 result.y:=Modulus(a.y,b);
 result.z:=Modulus(a.z,b);
 result.w:=Modulus(a.w,b);
end;

class operator TVector4.Modulus(const a:TScalar;const b:TVector4):TVector4;
begin
 result.x:=Modulus(a,b.x);
 result.y:=Modulus(a,b.y);
 result.z:=Modulus(a,b.z);
 result.w:=Modulus(a,b.w);
end;

class operator TVector4.Negative({$ifdef fpc}constref{$else}const{$endif} a:TVector4):TVector4;
{$if defined(cpu386) or defined(cpux64)}
asm
 xorps xmm0,xmm0
 movups xmm1,dqword ptr [a]
 subps xmm0,xmm1
 movups dqword ptr [result],xmm0
end;
{$else}
begin
 result.x:=-a.x;
 result.y:=-a.y;
 result.z:=-a.z;
 result.w:=-a.w;
end;
{$ifend}

class operator TVector4.Positive(const a:TVector4):TVector4;
begin
 result:=a;
end;

{$i PasVulkan.Math.TVector4.Swizzle.Implementations.inc}

function TVector4.GetComponent(const pIndex:TInt32):TScalar;
begin
 result:=RawComponents[pIndex];
end;

procedure TVector4.SetComponent(const pIndex:TInt32;const pValue:TScalar);
begin
 RawComponents[pIndex]:=pValue;
end;

function TVector4.Flip:TVector4;
begin
 result.x:=x;
 result.y:=z;
 result.z:=-y;
 result.w:=w;
end;

function TVector4.Perpendicular:TVector4;
var v,p:TVector4;
begin
 v:=p.Normalize;
 p.x:=System.abs(v.x);
 p.y:=System.abs(v.y);
 p.z:=System.abs(v.z);
 p.w:=System.abs(v.w);
 if (p.x<=p.y) and (p.x<=p.z) and (p.x<=p.w) then begin
  p.x:=1.0;
  p.y:=0.0;
  p.z:=0.0;
  p.w:=0.0;
 end else if (p.y<=p.x) and (p.y<=p.z) and (p.y<=p.w) then begin
  p.x:=0.0;
  p.y:=1.0;
  p.z:=0.0;
  p.w:=0.0;
 end else if (p.z<=p.x) and (p.z<=p.y) and (p.z<=p.w) then begin
  p.x:=0.0;
  p.y:=0.0;
  p.z:=0.0;
  p.w:=1.0;
 end else begin
  p.x:=0.0;
  p.y:=0.0;
  p.z:=1.0;
  p.w:=0.0;
 end;
 result:=p-(v*v.Dot(p));
end;

function TVector4.Length:TScalar;
{$if defined(cpu386)}
asm
 movups xmm0,dqword ptr [eax]
 mulps xmm0,xmm0         // xmm0 = w*w, z*z, y*y, x*x
 movaps xmm1,xmm0        // xmm1 = xmm0
 shufps xmm1,xmm1,$4e    // xmm1 = z*z, w*w, x*x, y*y
 addps xmm0,xmm1         // xmm0 = xmm0 + xmm1 = (zw*zw, zw*zw, xy*zw, xy*zw)
 movaps xmm1,xmm0        // xmm1 = xmm0
 shufps xmm1,xmm1,$b1    // xmm0 = xy*xy, xy*xy, zw*zw, zw*zw
 addps xmm1,xmm0         // xmm1 = xmm1 + xmm0 = (xyzw, xyzw, xyzw, xyzw)
 sqrtss xmm0,xmm1
 movss dword ptr [result],xmm0
end;
{$elseif defined(cpux64)}
asm
{$ifdef Windows}
 movups xmm0,dqword ptr [rcx]
{$else}
 movups xmm0,dqword ptr [rdi]
{$endif}
 mulps xmm0,xmm0         // xmm0 = w*w, z*z, y*y, x*x
 movaps xmm1,xmm0        // xmm1 = xmm0
 shufps xmm1,xmm1,$4e    // xmm1 = z*z, w*w, x*x, y*y
 addps xmm0,xmm1         // xmm0 = xmm0 + xmm1 = (zw*zw, zw*zw, xy*zw, xy*zw)
 movaps xmm1,xmm0        // xmm1 = xmm0
 shufps xmm1,xmm1,$b1    // xmm0 = xy*xy, xy*xy, zw*zw, zw*zw
 addps xmm1,xmm0         // xmm1 = xmm1 + xmm0 = (xyzw, xyzw, xyzw, xyzw)
 sqrtss xmm0,xmm1
 movss dword ptr [result],xmm0
end;
{$else}
begin
 result:=sqrt(sqr(x)+sqr(y)+sqr(z)+sqr(w));
end;
{$ifend}

function TVector4.SquaredLength:TScalar;
{$if defined(cpu386)}
asm
 movups xmm0,dqword ptr [eax]
 mulps xmm0,xmm0         // xmm0 = w*w, z*z, y*y, x*x
 movaps xmm1,xmm0        // xmm1 = xmm0
 shufps xmm1,xmm1,$4e    // xmm1 = z*z, w*w, x*x, y*y
 addps xmm0,xmm1         // xmm0 = xmm0 + xmm1 = (zw*zw, zw*zw, xy*zw, xy*zw)
 movaps xmm1,xmm0        // xmm1 = xmm0
 shufps xmm1,xmm1,$b1    // xmm0 = xy*xy, xy*xy, zw*zw, zw*zw
 addps xmm1,xmm0         // xmm1 = xmm1 + xmm0 = (xyzw, xyzw, xyzw, xyzw)
 movss dword ptr [result],xmm0
end;
{$elseif defined(cpux64)}
asm
{$ifdef Windows}
 movups xmm0,dqword ptr [rcx]
{$else}
 movups xmm0,dqword ptr [rdi]
{$endif}
 mulps xmm0,xmm0         // xmm0 = w*w, z*z, y*y, x*x
 movaps xmm1,xmm0        // xmm1 = xmm0
 shufps xmm1,xmm1,$4e    // xmm1 = z*z, w*w, x*x, y*y
 addps xmm0,xmm1         // xmm0 = xmm0 + xmm1 = (zw*zw, zw*zw, xy*zw, xy*zw)
 movaps xmm1,xmm0        // xmm1 = xmm0
 shufps xmm1,xmm1,$b1    // xmm0 = xy*xy, xy*xy, zw*zw, zw*zw
 addps xmm1,xmm0         // xmm1 = xmm1 + xmm0 = (xyzw, xyzw, xyzw, xyzw)
 movss dword ptr [result],xmm0
end;
{$else}
begin
 result:=sqr(x)+sqr(y)+sqr(z)+sqr(w);
end;
{$ifend}

function TVector4.Normalize:TVector4;
{$if defined(cpu386)}
asm
 movups xmm0,dqword ptr [eax]
 movaps xmm2,xmm0
 mulps xmm0,xmm0         // xmm0 = w*w, z*z, y*y, x*x
 movaps xmm1,xmm0        // xmm1 = xmm0
 shufps xmm1,xmm1,$4e    // xmm1 = z*z, w*w, x*x, y*y
 addps xmm0,xmm1         // xmm0 = xmm0 + xmm1 = (zw*zw, zw*zw, xy*zw, xy*zw)
 movaps xmm1,xmm0        // xmm1 = xmm0
 shufps xmm1,xmm1,$b1    // xmm0 = xy*xy, xy*xy, zw*zw, zw*zw
 addps xmm1,xmm0         // xmm1 = xmm1 + xmm0 = (xyzw, xyzw, xyzw, xyzw)
 sqrtss xmm0,xmm1        // not rsqrtss! because rsqrtss has only 12-bit accuracy
 shufps xmm0,xmm0,$00
 divps xmm2,xmm0
 movaps xmm1,xmm2
 subps xmm1,xmm2
 cmpps xmm1,xmm2,7
 andps xmm2,xmm1
 movups dqword ptr [edx],xmm2
end;
{$elseif defined(cpux64)}
asm
{$ifdef Windows}
 movups xmm0,dqword ptr [rcx]
{$else}
 movups xmm0,dqword ptr [rdi]
{$endif}
 movaps xmm2,xmm0
 mulps xmm0,xmm0         // xmm0 = w*w, z*z, y*y, x*x
 movaps xmm1,xmm0        // xmm1 = xmm0
 shufps xmm1,xmm1,$4e    // xmm1 = z*z, w*w, x*x, y*y
 addps xmm0,xmm1         // xmm0 = xmm0 + xmm1 = (zw*zw, zw*zw, xy*zw, xy*zw)
 movaps xmm1,xmm0        // xmm1 = xmm0
 shufps xmm1,xmm1,$b1    // xmm0 = xy*xy, xy*xy, zw*zw, zw*zw
 addps xmm1,xmm0         // xmm1 = xmm1 + xmm0 = (xyzw, xyzw, xyzw, xyzw)
 sqrtss xmm0,xmm1        // not rsqrtss! because rsqrtss has only 12-bit accuracy
 shufps xmm0,xmm0,$00
 divps xmm2,xmm0
 movaps xmm1,xmm2
 subps xmm1,xmm2
 cmpps xmm1,xmm2,7
 andps xmm2,xmm1
{$ifdef Windows}
 movups dqword ptr [rdx],xmm2
{$else}
 movups dqword ptr [rax],xmm2
{$endif}
end;
{$else}
var Factor:TScalar;
begin
 Factor:=sqrt(sqr(x)+sqr(y)+sqr(z)+sqr(w));
 if Factor<>0.0 then begin
  Factor:=1.0/Factor;
  result.x:=x*Factor;
  result.y:=y*Factor;
  result.z:=z*Factor;
  result.w:=w*Factor;
 end else begin
  result.x:=0.0;
  result.y:=0.0;
  result.z:=0.0;
  result.w:=0.0;
 end;
end;
{$ifend}

function TVector4.DistanceTo({$ifdef fpc}constref{$else}const{$endif} b:TVector4):TScalar;
{$if defined(cpu386)}
asm
 movups xmm0,dqword ptr [eax]
 movups xmm1,dqword ptr [edx]
 subps xmm0,xmm1
 mulps xmm0,xmm0         // xmm0 = w*w, z*z, y*y, x*x
 movaps xmm1,xmm0        // xmm1 = xmm0
 shufps xmm1,xmm1,$4e    // xmm1 = z*z, w*w, x*x, y*y
 addps xmm0,xmm1         // xmm0 = xmm0 + xmm1 = (zw*zw, zw*zw, xy*zw, xy*zw)
 movaps xmm1,xmm0        // xmm1 = xmm0
 shufps xmm1,xmm1,$b1    // xmm0 = xy*xy, xy*xy, zw*zw, zw*zw
 addps xmm1,xmm0         // xmm1 = xmm1 + xmm0 = (xyzw, xyzw, xyzw, xyzw)
 sqrtss xmm0,xmm1
 movss dword ptr [result],xmm0
end;
{$elseif defined(cpux64)}
asm
{$ifdef Windows}
 movups xmm0,dqword ptr [rcx]
 movups xmm1,dqword ptr [rdx]
{$else}
 movups xmm0,dqword ptr [rdi]
 movups xmm1,dqword ptr [rsi]
{$endif}
 subps xmm0,xmm1
 mulps xmm0,xmm0         // xmm0 = w*w, z*z, y*y, x*x
 movaps xmm1,xmm0        // xmm1 = xmm0
 shufps xmm1,xmm1,$4e    // xmm1 = z*z, w*w, x*x, y*y
 addps xmm0,xmm1         // xmm0 = xmm0 + xmm1 = (zw*zw, zw*zw, xy*zw, xy*zw)
 movaps xmm1,xmm0        // xmm1 = xmm0
 shufps xmm1,xmm1,$b1    // xmm0 = xy*xy, xy*xy, zw*zw, zw*zw
 addps xmm1,xmm0         // xmm1 = xmm1 + xmm0 = (xyzw, xyzw, xyzw, xyzw)
 sqrtss xmm0,xmm1
 movss dword ptr [result],xmm0
end;
{$else}
begin
 result:=sqrt(sqr(x-b.x)+sqr(y-b.y)+sqr(z-b.z)+sqr(w-b.w));
end;
{$ifend}

function TVector4.Abs:TVector4;
{$if defined(cpu386)}
asm
 movups xmm0,dqword ptr [eax]
 xorps xmm1,xmm1
 subps xmm1,xmm0
 maxps xmm0,xmm1
 movups dqword ptr [edx],xmm0
end;
{$elseif defined(cpux64)}
asm
{$ifdef Windows}
 movups xmm0,dqword ptr [rcx]
{$else}
 movups xmm0,dqword ptr [rdi]
{$endif}
 xorps xmm1,xmm1
 subps xmm1,xmm0
 maxps xmm0,xmm1
{$ifdef Windows}
 movups dqword ptr [rdx],xmm0
{$else}
 movups dqword ptr [rax],xmm0
{$endif}
end;
{$else}
begin
 result.x:=System.abs(x);
 result.y:=System.abs(y);
 result.z:=System.abs(z);
 result.w:=System.abs(w);
end;
{$ifend}

function TVector4.Dot({$ifdef fpc}constref{$else}const{$endif} b:TVector4):TScalar; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
{$if defined(cpu386)}
asm
 movups xmm0,dqword ptr [eax]
 movups xmm1,dqword ptr [edx]
 mulps xmm0,xmm1
 movaps xmm1,xmm0
 shufps xmm1,xmm0,$b1
 addps xmm0,xmm1
 movhlps xmm1,xmm0
 addss xmm0,xmm1
 movss dword ptr [result],xmm0
end;
{$elseif defined(cpux64)}
asm
{$ifdef Windows}
 movups xmm0,dqword ptr [rcx]
 movups xmm1,dqword ptr [rdx]
{$else}
 movups xmm0,dqword ptr [rdi]
 movups xmm1,dqword ptr [rsi]
{$endif}
 mulps xmm0,xmm1
 movaps xmm1,xmm0
 shufps xmm1,xmm0,$b1
 addps xmm0,xmm1
 movhlps xmm1,xmm0
 addss xmm0,xmm1
 movss dword ptr [result],xmm0
end;
{$else}
begin
 result:=(x*b.x)+(y*b.y)+(z*b.z)+(w*b.w);
end;
{$ifend}

function TVector4.AngleTo(const b:TVector4):TScalar;
var d:single;
begin
 d:=sqrt(SquaredLength*b.SquaredLength);
 if d<>0.0 then begin
  result:=Dot(b)/d;
 end else begin
  result:=0.0;
 end
end;

function TVector4.Cross({$ifdef fpc}constref{$else}const{$endif} b:TVector4):TVector4;
{$if defined(cpu386)}
const AndMask:array[0..3] of TUInt32=($ffffffff,$ffffffff,$ffffffff,$00000000);
      OrMask:array[0..3] of TUInt32=($00000000,$00000000,$00000000,$3f800000);
asm
{$ifdef SSEVector3CrossOtherVariant}
 movups xmm0,dqword ptr [eax]
 movups xmm2,dqword ptr [edx]
 movups xmm4,dqword ptr [AndMask]
 movups xmm5,dqword ptr [OrMask]
 andps xmm0,xmm4
 andps xmm2,xmm4
 movaps xmm1,xmm0
 movaps xmm3,xmm2
 shufps xmm0,xmm0,$c9
 shufps xmm1,xmm1,$d2
 shufps xmm2,xmm2,$d2
 shufps xmm3,xmm3,$c9
 mulps xmm0,xmm2
 mulps xmm1,xmm3
 subps xmm0,xmm1
 andps xmm0,xmm4
 orps xmm0,xmm5
 movups dqword ptr [ecx],xmm0
{$else}
 movups xmm0,dqword ptr [eax]
 movups xmm1,dqword ptr [edx]
 movups xmm4,dqword ptr [AndMask]
 movups xmm5,dqword ptr [OrMask]
 andps xmm0,xmm4
 andps xmm1,xmm4
 movaps xmm2,xmm0
 movaps xmm3,xmm1
 shufps xmm0,xmm0,$12
 shufps xmm1,xmm1,$09
 shufps xmm2,xmm2,$09
 shufps xmm3,xmm3,$12
 mulps xmm0,xmm1
 mulps xmm2,xmm3
 subps xmm2,xmm0
 andps xmm2,xmm4
 orps xmm2,xmm5
 movups dqword ptr [ecx],xmm2
{$endif}
end;
{$elseif defined(cpux64)}
const AndMask:array[0..3] of TUInt32=($ffffffff,$ffffffff,$ffffffff,$00000000);
      OrMask:array[0..3] of TUInt32=($00000000,$00000000,$00000000,$3f800000);
asm
{$ifdef SSEVector3CrossOtherVariant}
{$ifdef Windows}
 movups xmm0,dqword ptr [rcx]
 movups xmm2,dqword ptr [r8]
{$else}
 movups xmm0,dqword ptr [rdi]
 movups xmm2,dqword ptr [rsi]
{$endif}
{$ifdef fpc}
 movups xmm4,dqword ptr [rip+AndMask]
 movups xmm5,dqword ptr [rip+OrMask]
{$else}
 movups xmm4,dqword ptr [rel AndMask]
 movups xmm5,dqword ptr [rel OrMask]
{$endif}
 andps xmm0,xmm4
 andps xmm2,xmm4
 movaps xmm1,xmm0
 movaps xmm3,xmm2
 shufps xmm0,xmm0,$c9
 shufps xmm1,xmm1,$d2
 shufps xmm2,xmm2,$d2
 shufps xmm3,xmm3,$c9
 mulps xmm0,xmm2
 mulps xmm1,xmm3
 subps xmm0,xmm1
 andps xmm0,xmm4
 orps xmm0,xmm5
{$ifdef Windows}
 movups dqword ptr [rdx],xmm0
{$else}
 movups dqword ptr [rax],xmm0
{$endif}
{$else}
{$ifdef Windows}
 movups xmm0,dqword ptr [rcx]
 movups xmm1,dqword ptr [r8]
{$else}
 movups xmm0,dqword ptr [rdi]
 movups xmm1,dqword ptr [rsi]
{$endif}
{$ifdef fpc}
 movups xmm4,dqword ptr [rip+AndMask]
 movups xmm5,dqword ptr [rip+OrMask]
{$else}
 movups xmm4,dqword ptr [rel AndMask]
 movups xmm5,dqword ptr [rel OrMask]
{$endif}
 andps xmm0,xmm4
 andps xmm1,xmm4
 movaps xmm2,xmm0
 movaps xmm3,xmm1
 shufps xmm0,xmm0,$12
 shufps xmm1,xmm1,$09
 shufps xmm2,xmm2,$09
 shufps xmm3,xmm3,$12
 mulps xmm0,xmm1
 mulps xmm2,xmm3
 subps xmm2,xmm0
 andps xmm2,xmm4
 orps xmm2,xmm5
{$ifdef Windows}
 movups dqword ptr [rdx],xmm2
{$else}
 movups dqword ptr [rax],xmm2
{$endif}
{$endif}
end;
{$else}
begin
 result.x:=(y*b.z)-(z*b.y);
 result.y:=(z*b.x)-(x*b.z);
 result.z:=(x*b.y)-(y*b.x);
 result.w:=1.0;
end;
{$ifend}

function TVector4.Lerp(const b:TVector4;const t:TScalar):TVector4;
var InvT:TScalar;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  InvT:=1.0-t;
  result.x:=(x*InvT)+(b.x*t);
  result.y:=(y*InvT)+(b.y*t);
  result.z:=(z*InvT)+(b.z*t);
  result.w:=(w*InvT)+(b.w*t);
 end;
end;

function TVector4.Angle(const b,c:TVector4):TScalar;
var DeltaAB,DeltaCB:TVector4;
    LengthAB,LengthCB:TScalar;
begin
 DeltaAB:=self-b;
 DeltaCB:=c-b;
 LengthAB:=DeltaAB.Length;
 LengthCB:=DeltaCB.Length;
 if (LengthAB=0.0) or (LengthCB=0.0) then begin
  result:=0.0;
 end else begin
  result:=ArcCos(DeltaAB.Dot(DeltaCB)/(LengthAB*LengthCB));
 end;
end;

function TVector4.RotateX(const Angle:TScalar):TVector4;
var Sinus,Cosinus:TScalar;
begin
 Sinus:=0.0;
 Cosinus:=0.0;
 SinCos(Angle,Sinus,Cosinus);
 result.x:=x;
 result.y:=(y*Cosinus)-(z*Sinus);
 result.z:=(y*Sinus)+(z*Cosinus);
 result.w:=w;
end;

function TVector4.RotateY(const Angle:TScalar):TVector4;
var Sinus,Cosinus:TScalar;
begin
 Sinus:=0.0;
 Cosinus:=0.0;
 SinCos(Angle,Sinus,Cosinus);
 result.x:=(z*Sinus)+(x*Cosinus);
 result.y:=y;
 result.z:=(z*Cosinus)-(x*Sinus);
 result.w:=w;
end;

function TVector4.RotateZ(const Angle:TScalar):TVector4;
var Sinus,Cosinus:TScalar;
begin
 Sinus:=0.0;
 Cosinus:=0.0;
 SinCos(Angle,Sinus,Cosinus);
 result.x:=(x*Cosinus)-(y*Sinus);
 result.y:=(x*Sinus)+(y*Cosinus);
 result.z:=z;
 result.w:=w;
end;

function TVector4.Rotate(const Angle:TScalar;const Axis:TVector3):TVector4;
begin
 result:=TMatrix4x4.CreateRotate(Angle,Axis)*self;
end;

function TVector4.ProjectToBounds(const MinVector,MaxVector:TVector4):TScalar;
begin
 if x<0.0 then begin
  result:=x*MaxVector.x;
 end else begin
  result:=x*MinVector.x;
 end;
 if y<0.0 then begin
  result:=result+(y*MaxVector.y);
 end else begin
  result:=result+(y*MinVector.y);
 end;
 if z<0.0 then begin
  result:=result+(z*MaxVector.z);
 end else begin
  result:=result+(z*MinVector.z);
 end;
 if w<0.0 then begin
  result:=result+(w*MaxVector.w);
 end else begin
  result:=result+(w*MinVector.w);
 end;
end;

{$i PasVulkan.Math.TVector2Helper.Swizzle.Implementations.inc}
{$i PasVulkan.Math.TVector3Helper.Swizzle.Implementations.inc}
{$i PasVulkan.Math.TVector4Helper.Swizzle.Implementations.inc}

constructor TPlane.Create(const pNormal:TVector3;const pDistance:TScalar);
begin
 Normal:=pNormal;
 Distance:=pDistance;
end;

constructor TPlane.Create(const x,y,z,pDistance:TScalar);
begin
 Normal.x:=x;
 Normal.y:=y;
 Normal.z:=z;
 Distance:=pDistance;
end;

constructor TPlane.Create(const pA,pB,pC:TVector3);
begin
 Normal:=((pB-pA).Cross(pC-pA)).Normalize;
 Distance:=-((Normal.x*pA.x)+(Normal.y*pA.y)+(Normal.z*pA.z));
end;

constructor TPlane.Create(const pA,pB,pC:TVector4);
begin
 Normal:=((pB.xyz-pA.xyz).Cross(pC.xyz-pA.xyz)).Normalize;
 Distance:=-((Normal.x*pA.x)+(Normal.y*pA.y)+(Normal.z*pA.z));
end;

constructor TPlane.Create(const Vector:TVector4);
begin
 Normal.x:=Vector.x;
 Normal.y:=Vector.y;
 Normal.z:=Vector.z;
 Distance:=Vector.w;
end;

function TPlane.ToVector:TVector4;
begin
 result.x:=Normal.x;
 result.y:=Normal.y;
 result.z:=Normal.z;
 result.w:=Distance;
end;

function TPlane.Normalize:TPlane;
var l:TScalar;
begin
 l:=Normal.Length;
 if l>0.0 then begin
  l:=1.0/l;
  result.Normal:=Normal*l;
  result.Distance:=Distance*l;
 end else begin
  result.Normal:=0.0;
  result.Distance:=0.0;
 end;
end;

function TPlane.DistanceTo(const Point:TVector3):TScalar;
begin
 result:=Normal.Dot(Point)+Distance;
end;

function TPlane.DistanceTo(const Point:TVector4):TScalar;
begin
 result:=Normal.Dot(Point.xyz)+(Point.w*Distance);
end;

procedure TPlane.ClipSegment(const p0,p1:TVector3;out Clipped:TVector3);
begin
 Clipped:=p0+((p1-p0).Normalize*(-DistanceTo(p0)));
//Clipped:=p0+((p1-p0)*((-DistanceTo(p0))/Normal.Dot(p1-p0)));
end;

function TPlane.ClipSegmentClosest(const p0,p1:TVector3;out Clipped0,Clipped1:TVector3):TInt32;
var d0,d1:TScalar;
begin
 d0:=-DistanceTo(p0);
 d1:=-DistanceTo(p1);
 if (d0>(-EPSILON)) and (d1>(-EPSILON)) then begin
  if d0<d1 then begin
   result:=0;
   Clipped0:=p0;
   Clipped1:=p1;
  end else begin
   result:=1;
   Clipped0:=p1;
   Clipped1:=p0;
  end;
 end else if (d0<EPSILON) and (d1<EPSILON) then begin
  if d0>d1 then begin
   result:=2;
   Clipped0:=p0;
   Clipped1:=p1;
  end else begin
   result:=3;
   Clipped0:=p1;
   Clipped1:=p0;
  end;
 end else begin
  if d0<d1 then begin
   result:=4;
   Clipped1:=p0;
  end else begin
   result:=5;
   Clipped1:=p1;
  end;
  Clipped0:=p1-p0;
  Clipped0:=p0+(Clipped0*((-d0)/Normal.Dot(Clipped0)));
 end;
end;

function TPlane.ClipSegmentLine(var p0,p1:TVector3):boolean;
var d0,d1:TScalar;
    o0,o1:boolean;
begin
 d0:=DistanceTo(p0);
 d1:=DistanceTo(p1);
 o0:=d0<0.0;
 o1:=d1<0.0;
 if o0 and o1 then begin
  // Both points are below which means that the whole line segment is below => return false
  result:=false;
 end else begin
  // At least one point is above or in the plane which means that the line segment is above => return true
  if (o0<>o1) and (abs(d0-d1)>EPSILON) then begin
   if o0 then begin
    // p1 is above or in the plane which means that the line segment is above => clip l0
    p0:=p0+((p1-p0)*(d0/(d0-d1)));
   end else begin
    // p0 is above or in the plane which means that the line segment is above => clip l1
    p1:=p0+((p1-p0)*(d0/(d0-d1)));
   end;
  end else begin
   // Near parallel case => no clipping
  end;
  result:=true;
 end;
end;

constructor TQuaternion.Create(const pX:TScalar);
begin
 x:=pX;
 y:=pX;
 z:=pX;
 w:=pX;
end;

constructor TQuaternion.Create(const pX,pY,pZ,pW:TScalar);
begin
 x:=pX;
 y:=pY;
 z:=pZ;
 w:=pW;
end;

constructor TQuaternion.Create(const pVector:TVector4);
begin
 Vector:=pVector;
end;

constructor TQuaternion.CreateFromAngleAxis(const Angle:TScalar;const Axis:TVector3);
var s:TScalar;
begin
{s:=sin(Angle*0.5);
 w:=cos(Angle*0.5);}
 SinCos(Angle*0.5,s,w);
 x:=Axis.x*s;
 y:=Axis.y*s;
 z:=Axis.z*s;
 self:=self.Normalize;
end;

constructor TQuaternion.CreateFromEuler(const Pitch,Yaw,Roll:TScalar);
var sp,sy,sr,cp,cy,cr:TScalar;
begin
 // Order of rotations: Roll (Z), Pitch (X), Yaw (Y)
 SinCos(Pitch*0.5,sp,cp);
 SinCos(Yaw*0.5,sy,cy);
 SinCos(Roll*0.5,sr,cr);
{sp:=sin(Pitch*0.5);
 sy:=sin(Yaw*0.5);
 sr:=sin(Roll*0.5);
 cp:=cos(Pitch*0.5);
 cy:=cos(Yaw*0.5);
 cr:=cos(Roll*0.5);}
 Vector:=TVector4.Create((sp*cy*cr)+(cp*sy*sr),
                              (cp*sy*cr)-(sp*cy*sr),
                              (cp*cy*sr)-(sp*sy*cr),
                              (cp*cy*cr)+(sp*sy*sr)
                             ).Normalize;
end;

constructor TQuaternion.CreateFromEuler(const Angles:TVector3);
var sp,sy,sr,cp,cy,cr:TScalar;
begin
 // Order of rotations: Roll (Z), Pitch (X), Yaw (Y)
 SinCos(Angles.Pitch*0.5,sp,cp);
 SinCos(Angles.Yaw*0.5,sy,cy);
 SinCos(Angles.Roll*0.5,sr,cr);
{sp:=sin(Angles.Pitch*0.5);
 sy:=sin(Angles.Yaw*0.5);
 sr:=sin(Angles.Roll*0.5);
 cp:=cos(Angles.Pitch*0.5);
 cy:=cos(Angles.Yaw*0.5);
 cr:=cos(Angles.Roll*0.5);{}
 Vector:=TVector4.Create((sp*cy*cr)+(cp*sy*sr),
                              (cp*sy*cr)-(sp*cy*sr),
                              (cp*cy*sr)-(sp*sy*cr),
                              (cp*cy*cr)+(sp*sy*sr)
                             ).Normalize;
end;

constructor TQuaternion.CreateFromNormalizedSphericalCoordinates(const NormalizedSphericalCoordinates:TNormalizedSphericalCoordinates);
begin
 x:=cos(NormalizedSphericalCoordinates.Latitude)*sin(NormalizedSphericalCoordinates.Longitude);
 y:=sin(NormalizedSphericalCoordinates.Latitude);
 z:=cos(NormalizedSphericalCoordinates.Latitude)*cos(NormalizedSphericalCoordinates.Longitude);
 w:=0.0;
end;

constructor TQuaternion.CreateFromToRotation(const FromDirection,ToDirection:TVector3);
begin
 Vector.xyz:=FromDirection.Normalize.Cross(ToDirection.Normalize);
 Vector.w:=sqrt((sqr(FromDirection.x)+sqr(FromDirection.y)+sqr(FromDirection.z))*
                (sqr(ToDirection.x)+sqr(ToDirection.y)+sqr(ToDirection.z)))+
                ((FromDirection.x*ToDirection.x)+(FromDirection.y*ToDirection.y)+(FromDirection.z*ToDirection.z));
end;

class operator TQuaternion.Implicit(const a:TScalar):TQuaternion;
begin
 result.x:=a;
 result.y:=a;
 result.z:=a;
 result.w:=a;
end;

class operator TQuaternion.Explicit(const a:TScalar):TQuaternion;
begin
 result.x:=a;
 result.y:=a;
 result.z:=a;
 result.w:=a;
end;

class operator TQuaternion.Equal(const a,b:TQuaternion):boolean;
begin
 result:=SameValue(a.x,b.x) and SameValue(a.y,b.y) and SameValue(a.z,b.z) and SameValue(a.w,b.w);
end;

class operator TQuaternion.NotEqual(const a,b:TQuaternion):boolean;
begin
 result:=(not SameValue(a.x,b.x)) or (not SameValue(a.y,b.y)) or (not SameValue(a.z,b.z)) or (not SameValue(a.w,b.w));
end;

class operator TQuaternion.Inc({$ifdef fpc}constref{$else}const{$endif} a:TQuaternion):TQuaternion;
{$if defined(cpu386)}
const One:TQuaternion=(x:1.0;y:1.0;z:1.0;w:1.0);
asm
 movups xmm0,dqword ptr [a]
 movups xmm1,dqword ptr [One]
 addps xmm0,xmm1
 movups dqword ptr [result],xmm0
end;
{$elseif defined(cpux64)}
const One:TQuaternion=(x:1.0;y:1.0;z:1.0;w:1.0);
asm
 movups xmm0,dqword ptr [a]
{$ifdef fpc}
 movups xmm1,dqword ptr [rip+One]
{$else}
 movups xmm1,dqword ptr [rel One]
{$endif}
 addps xmm0,xmm1
 movups dqword ptr [result],xmm0
end;
{$else}
begin
 result.x:=a.x+1.0;
 result.y:=a.y+1.0;
 result.z:=a.z+1.0;
 result.w:=a.w+1.0;
end;
{$ifend}

class operator TQuaternion.Dec({$ifdef fpc}constref{$else}const{$endif} a:TQuaternion):TQuaternion;
{$if defined(cpu386)}
const One:TQuaternion=(x:1.0;y:1.0;z:1.0;w:1.0);
asm
 movups xmm0,dqword ptr [a]
 movups xmm1,dqword ptr [One]
 subps xmm0,xmm1
 movups dqword ptr [result],xmm0
end;
{$elseif defined(cpux64)}
const One:TQuaternion=(x:1.0;y:1.0;z:1.0;w:1.0);
asm
 movups xmm0,dqword ptr [a]
{$ifdef fpc}
 movups xmm1,dqword ptr [rip+One]
{$else}
 movups xmm1,dqword ptr [rel One]
{$endif}
 subps xmm0,xmm1
 movups dqword ptr [result],xmm0
end;
{$else}
begin
 result.x:=a.x-1.0;
 result.y:=a.y-1.0;
 result.z:=a.z-1.0;
 result.w:=a.w-1.0;
end;
{$ifend}

class operator TQuaternion.Add({$ifdef fpc}constref{$else}const{$endif} a,b:TQuaternion):TQuaternion;
{$if defined(cpu386) or defined(cpux64)}
asm
 movups xmm0,dqword ptr [a]
 movups xmm1,dqword ptr [b]
 addps xmm0,xmm1
 movups dqword ptr [result],xmm0
end;
{$else}
begin
 result.x:=a.x+b.x;
 result.y:=a.y+b.y;
 result.z:=a.z+b.z;
 result.w:=a.w+b.w;
end;
{$ifend}

class operator TQuaternion.Add(const a:TQuaternion;const b:TScalar):TQuaternion;
begin
 result.x:=a.x+b;
 result.y:=a.y+b;
 result.z:=a.z+b;
 result.w:=a.w+b;
end;

class operator TQuaternion.Add(const a:TScalar;const b:TQuaternion):TQuaternion;
begin
 result.x:=a+b.x;
 result.y:=a+b.y;
 result.z:=a+b.z;
 result.w:=a+b.w;
end;

class operator TQuaternion.Subtract({$ifdef fpc}constref{$else}const{$endif} a,b:TQuaternion):TQuaternion;
{$if defined(cpu386) or defined(cpux64)}
asm
 movups xmm0,dqword ptr [a]
 movups xmm1,dqword ptr [b]
 subps xmm0,xmm1
 movups dqword ptr [result],xmm0
end;
{$else}
begin
 result.x:=a.x-b.x;
 result.y:=a.y-b.y;
 result.z:=a.z-b.z;
 result.w:=a.w-b.w;
end;
{$ifend}

class operator TQuaternion.Subtract(const a:TQuaternion;const b:TScalar):TQuaternion;
begin
 result.x:=a.x-b;
 result.y:=a.y-b;
 result.z:=a.z-b;
 result.w:=a.w-b;
end;

class operator TQuaternion.Subtract(const a:TScalar;const b:TQuaternion): TQuaternion;
begin
 result.x:=a-b.x;
 result.y:=a-b.y;
 result.z:=a-b.z;
 result.w:=a-b.w;
end;

class operator TQuaternion.Multiply({$ifdef fpc}constref{$else}const{$endif} a,b:TQuaternion):TQuaternion;
{$if defined(cpu386) or defined(cpux64)}
const XORMaskW:array[0..3] of TUInt32=($00000000,$00000000,$00000000,$80000000);
asm
 movups xmm4,dqword ptr [a]
 movaps xmm0,xmm4
 shufps xmm0,xmm4,$49
 movups xmm2,dqword ptr [b]
 movaps xmm3,xmm2
 movaps xmm1,xmm2
 shufps xmm3,xmm2,$52 // 001010010b
 mulps xmm3,xmm0
 movaps xmm0,xmm4
 shufps xmm0,xmm4,$24 // 000100100b
 shufps xmm1,xmm2,$3f // 000111111b
{$ifdef cpu386}
 movups xmm5,dqword ptr [XORMaskW]
{$else}
{$ifdef fpc}
 movups xmm5,dqword ptr [rip+XORMaskW]
{$else}
 movups xmm5,dqword ptr [rel XORMaskW]
{$endif}
{$endif}
 mulps xmm1,xmm0
 movaps xmm0,xmm4
 shufps xmm0,xmm4,$92 // 001001001b
 shufps xmm4,xmm4,$ff // 011111111b
 mulps xmm4,xmm2
 addps xmm3,xmm1
 movaps xmm1,xmm2
 shufps xmm1,xmm2,$89 // 010001001b
 mulps xmm1,xmm0
 xorps xmm3,xmm5
 subps xmm4,xmm1
 addps xmm3,xmm4
 movups dqword ptr [result],xmm3
end;
{$else}
begin
 result.x:=((a.w*b.x)+(a.x*b.w)+(a.y*b.z))-(a.z*b.y);
 result.y:=((a.w*b.y)+(a.y*b.w)+(a.z*b.x))-(a.x*b.z);
 result.z:=((a.w*b.z)+(a.z*b.w)+(a.x*b.y))-(a.y*b.x);
 result.w:=(a.w*b.w)-((a.x*b.x)+(a.y*b.y)+(a.z*b.z));
end;
{$ifend}

class operator TQuaternion.Multiply(const a:TQuaternion;const b:TScalar):TQuaternion;
begin
 result.x:=a.x*b;
 result.y:=a.y*b;
 result.z:=a.z*b;
 result.w:=a.w*b;
end;

class operator TQuaternion.Multiply(const a:TScalar;const b:TQuaternion):TQuaternion;
begin
 result.x:=a*b.x;
 result.y:=a*b.y;
 result.z:=a*b.z;
 result.w:=a*b.w;
end;

class operator TQuaternion.Multiply({$ifdef fpc}constref{$else}const{$endif} a:TQuaternion;{$ifdef fpc}constref{$else}const{$endif} b:TVector3):TVector3;
{$if defined(cpu386) or defined(cpux64)}
const Mask:array[0..3] of TUInt32=($ffffffff,$ffffffff,$ffffffff,$00000000);
asm

 // q = a
 // v = b

 movups xmm4,dqword ptr [a] // xmm4 = q.xyzw

 xorps xmm7,xmm7
 movss xmm5,dword ptr [b+0]
 movss xmm6,dword ptr [b+4]
 movss xmm7,dword ptr [b+8]
 movlhps xmm5,xmm6
 shufps xmm5,xmm7,$88
//movups xmm5,dqword ptr [b] // xmm5 = v.xyz?

 movaps xmm6,xmm4
 shufps xmm6,xmm6,$ff // xmm6 = q.wwww

{$ifdef cpu386}
 movups xmm7,dqword ptr [Mask] // xmm7 = Mask
{$else}
{$ifdef fpc}
 movups xmm7,dqword ptr [rip+Mask] // xmm7 = Mask
{$else}
 movups xmm7,dqword ptr [rel Mask] // xmm7 = Mask
{$endif}
{$endif}

 andps xmm4,xmm7 // xmm4 = q.xyz0

 andps xmm5,xmm7 // xmm5 = v.xyz0

 // t:=Vector3ScalarMul(Vector3Cross(qv,v),2.0);
 movaps xmm0,xmm4 // xmm4 = qv
 movaps xmm1,xmm5 // xmm5 = v
 movaps xmm2,xmm4 // xmm4 = qv
 movaps xmm3,xmm5 // xmm5 = v
 shufps xmm0,xmm0,$12
 shufps xmm1,xmm1,$09
 shufps xmm2,xmm2,$09
 shufps xmm3,xmm3,$12
 mulps xmm0,xmm1
 mulps xmm2,xmm3
 subps xmm2,xmm0
 addps xmm2,xmm2

 // xmm6 = Vector3Add(v,Vector3ScalarMul(t,q.w))
 mulps xmm6,xmm2 // xmm6 = q.wwww, xmm2 = t
 addps xmm6,xmm5 // xmm5 = v

 // Vector3Cross(qv,t)
 movaps xmm1,xmm4 // xmm4 = qv
 movaps xmm3,xmm2 // xmm2 = t
 shufps xmm4,xmm4,$12
 shufps xmm2,xmm2,$09
 shufps xmm1,xmm1,$09
 shufps xmm3,xmm3,$12
 mulps xmm4,xmm2
 mulps xmm1,xmm3
 subps xmm1,xmm4

 // result:=Vector3Add(Vector3Add(v,Vector3ScalarMul(t,q.w)),Vector3Cross(qv,t));
 addps xmm1,xmm6

 movaps xmm0,xmm1
 movaps xmm2,xmm0
 shufps xmm1,xmm1,$55
 shufps xmm2,xmm2,$aa
 movss dword ptr [result+0],xmm0
 movss dword ptr [result+4],xmm1
 movss dword ptr [result+8],xmm2
//movups dqword ptr [result],xmm1

end;
{$else}
var t:TVector3;
begin
 // q = a
 // v = b
 // t = 2 * cross(q.xyz, v)
 // v' = v + q.w * t + cross(q.xyz, t)
 t:=a.Vector.xyz.Cross(b)*2.0;
 result:=(b+(a.w*t))+a.Vector.xyz.Cross(t);
end;
{$ifend}

class operator TQuaternion.Multiply(const a:TVector3;const b:TQuaternion):TVector3;
begin
 result:=b.Inverse*a;
end;

class operator TQuaternion.Multiply({$ifdef fpc}constref{$else}const{$endif} a:TQuaternion;{$ifdef fpc}constref{$else}const{$endif} b:TVector4):TVector4;
{$if defined(cpu386) or defined(cpux64)}
const AndMask:array[0..3] of TUInt32=($ffffffff,$ffffffff,$ffffffff,$00000000);
      OrMask:array[0..3] of TUInt32=($00000000,$00000000,$00000000,$3f800000);
asm

 // q = a
 // v = b

 movups xmm4,dqword ptr [a] // xmm4 = q.xyzw

 movups xmm5,dqword ptr [b] // xmm5 = v.xyz?

 movaps xmm6,xmm4
 shufps xmm6,xmm6,$ff // xmm6 = q.wwww

{$ifdef cpu386}
 movups xmm7,dqword ptr [AndMask] // xmm7 = AndMask
{$else}
{$ifdef fpc}
 movups xmm7,dqword ptr [rip+AndMask] // xmm7 = AndMask
{$else}
 movups xmm7,dqword ptr [rel AndMask] // xmm7 = AndMask
{$endif}
{$endif}

 andps xmm4,xmm7 // xmm4 = q.xyz0

 andps xmm5,xmm7 // xmm5 = v.xyz0

 // t:=Vector3ScalarMul(Vector3Cross(qv,v),2.0);
 movaps xmm0,xmm4 // xmm4 = qv
 movaps xmm1,xmm5 // xmm5 = v
 movaps xmm2,xmm4 // xmm4 = qv
 movaps xmm3,xmm5 // xmm5 = v
 shufps xmm0,xmm0,$12
 shufps xmm1,xmm1,$09
 shufps xmm2,xmm2,$09
 shufps xmm3,xmm3,$12
 mulps xmm0,xmm1
 mulps xmm2,xmm3
 subps xmm2,xmm0
 addps xmm2,xmm2

 // xmm6 = Vector3Add(v,Vector3ScalarMul(t,q.w))
 mulps xmm6,xmm2 // xmm6 = q.wwww, xmm2 = t
 addps xmm6,xmm5 // xmm5 = v

 // Vector3Cross(qv,t)
 movaps xmm1,xmm4 // xmm4 = qv
 movaps xmm3,xmm2 // xmm2 = t
 shufps xmm4,xmm4,$12
 shufps xmm2,xmm2,$09
 shufps xmm1,xmm1,$09
 shufps xmm3,xmm3,$12
 mulps xmm4,xmm2
 mulps xmm1,xmm3
 subps xmm1,xmm4

{$ifdef cpu386}
 movups xmm4,dqword ptr [OrMask] // xmm4 = OrMask
{$else}
{$ifdef fpc}
 movups xmm4,dqword ptr [rip+OrMask] // xmm4 = OrMask
{$else}
 movups xmm4,dqword ptr [rel OrMask] // xmm4 = OrMask
{$endif}
{$endif}

 // result:=Vector3Add(Vector3Add(v,Vector3ScalarMul(t,q.w)),Vector3Cross(qv,t));
 addps xmm1,xmm6

 andps xmm1,xmm7 // xmm1 = xmm1.xyz0
 orps xmm1,xmm4 // xmm1 = xmm1.xyz1

 movups dqword ptr [result],xmm1

end;
{$else}
var t:TVector3;
begin
 // q = a
 // v = b
 // t = 2 * cross(q.xyz, v)
 // v' = v + q.w * t + cross(q.xyz, t)
 t:=a.Vector.xyz.Cross(b.xyz)*2.0;
 result.xyz:=(b.xyz+(a.w*t))+a.Vector.xyz.Cross(t);
 result.w:=1.0;
end;
{$ifend}

class operator TQuaternion.Multiply(const a:TVector4;const b:TQuaternion):TVector4;
begin
 result:=b.Inverse*a;
end;

class operator TQuaternion.Divide({$ifdef fpc}constref{$else}const{$endif} a,b:TQuaternion):TQuaternion;
{$if defined(cpu386) or defined(cpux64)}
asm
 movups xmm0,dqword ptr [a]
 movups xmm1,dqword ptr [b]
 divps xmm0,xmm1
 movups dqword ptr [result],xmm0
end;
{$else}
begin
 result.x:=a.x/b.x;
 result.y:=a.y/b.y;
 result.z:=a.z/b.z;
 result.w:=a.w/b.w;
end;
{$ifend}

class operator TQuaternion.Divide(const a:TQuaternion;const b:TScalar):TQuaternion;
begin
 result.x:=a.x/b;
 result.y:=a.y/b;
 result.z:=a.z/b;
 result.w:=a.w/b;
end;

class operator TQuaternion.Divide(const a:TScalar;const b:TQuaternion):TQuaternion;
begin
 result.x:=a/b.x;
 result.y:=a/b.y;
 result.z:=a/b.z;
 result.w:=a/b.z;
end;

class operator TQuaternion.IntDivide({$ifdef fpc}constref{$else}const{$endif} a,b:TQuaternion):TQuaternion;
{$if defined(cpu386) or defined(cpux64)}
asm
 movups xmm0,dqword ptr [a]
 movups xmm1,dqword ptr [b]
 divps xmm0,xmm1
 movups dqword ptr [result],xmm0
end;
{$else}
begin
 result.x:=a.x/b.x;
 result.y:=a.y/b.y;
 result.z:=a.z/b.z;
 result.w:=a.w/b.w;
end;
{$ifend}

class operator TQuaternion.IntDivide(const a:TQuaternion;const b:TScalar):TQuaternion;
begin
 result.x:=a.x/b;
 result.y:=a.y/b;
 result.z:=a.z/b;
 result.w:=a.w/b;
end;

class operator TQuaternion.IntDivide(const a:TScalar;const b:TQuaternion):TQuaternion;
begin
 result.x:=a/b.x;
 result.y:=a/b.y;
 result.z:=a/b.z;
 result.w:=a/b.w;
end;

class operator TQuaternion.Modulus(const a,b:TQuaternion):TQuaternion;
begin
 result.x:=Modulus(a.x,b.x);
 result.y:=Modulus(a.y,b.y);
 result.z:=Modulus(a.z,b.z);
 result.w:=Modulus(a.w,b.w);
end;

class operator TQuaternion.Modulus(const a:TQuaternion;const b:TScalar):TQuaternion;
begin
 result.x:=Modulus(a.x,b);
 result.y:=Modulus(a.y,b);
 result.z:=Modulus(a.z,b);
 result.w:=Modulus(a.w,b);
end;

class operator TQuaternion.Modulus(const a:TScalar;const b:TQuaternion):TQuaternion;
begin
 result.x:=Modulus(a,b.x);
 result.y:=Modulus(a,b.y);
 result.z:=Modulus(a,b.z);
 result.w:=Modulus(a,b.w);
end;

class operator TQuaternion.Negative({$ifdef fpc}constref{$else}const{$endif} a:TQuaternion):TQuaternion;
{$if defined(cpu386) or defined(cpux64)}
asm
 xorps xmm0,xmm0
 movups xmm1,dqword ptr [a]
 subps xmm0,xmm1
 movups dqword ptr [result],xmm0
end;
{$else}
begin
 result.x:=-a.x;
 result.y:=-a.y;
 result.z:=-a.z;
 result.w:=-a.w;
end;
{$ifend}

class operator TQuaternion.Positive(const a:TQuaternion):TQuaternion;
begin
 result:=a;
end;

function TQuaternion.GetComponent(const pIndex:TInt32):TScalar;
begin
 result:=RawComponents[pIndex];
end;

procedure TQuaternion.SetComponent(const pIndex:TInt32;const pValue:TScalar);
begin
 RawComponents[pIndex]:=pValue;
end;

function TQuaternion.ToNormalizedSphericalCoordinates:TNormalizedSphericalCoordinates;
var ty:TScalar;
begin
 ty:=y;
 if ty<-1.0 then begin
  ty:=-1.0;
 end else if ty>1.0 then begin
  ty:=1.0;
 end;
 result.Latitude:=ArcSin(ty);
 if (sqr(x)+sqr(z))>0.00005 then begin
  result.Longitude:=ArcTan2(x,z);
 end else begin
  result.Longitude:=0.0;
 end;
end;

function TQuaternion.ToEuler:TVector3;
var t:TScalar;
begin
 // Order of rotations: Roll (Z), Pitch (X), Yaw (Y)
 t:=2.0*((x*w)-(y*z));
 if t<-0.995 then begin
  result.Pitch:=-HalfPI;
  result.Yaw:=0.0;
  result.Roll:=-ArcTan2(2.0*((x*z)-(y*w)),1.0-(2.0*(sqr(y)+sqr(z))));
 end else if t>0.995 then begin
  result.Pitch:=HalfPI;
  result.Yaw:=0.0;
  result.Roll:=ArcTan2(2.0*((x*z)-(y*w)),1.0-(2.0*(sqr(y)+sqr(z))));
 end else begin
  result.Pitch:=ArcSin(t);
  result.Yaw:=ArcTan2(2.0*((x*z)+(y*w)),1.0-(2.0*(sqr(x)+sqr(y))));
  result.Roll:=ArcTan2(2.0*((x*y)+(z*w)),1.0-(2.0*(sqr(x)+sqr(z))));
 end;
end;

function TQuaternion.ToPitch:TScalar;
var t:TScalar;
begin
 // Order of rotations: Roll (Z), Pitch (X), Yaw (Y)
 t:=2.0*((x*w)-(y*z));
 if t<-0.995 then begin
  result:=-HalfPI;
 end else if t>0.995 then begin
  result:=HalfPI;
 end else begin
  result:=ArcSin(t);
 end;
end;

function TQuaternion.ToYaw:TScalar;
var t:TScalar;
begin
 // Order of rotations: Roll (Z), Pitch (X), Yaw (Y)
 t:=2.0*((x*w)-(y*z));
 if System.abs(t)>0.995 then begin
  result:=0.0;
 end else begin
  result:=ArcTan2(2.0*((x*z)+(y*w)),1.0-(2.0*(sqr(x)+sqr(y))));
 end;
end;

function TQuaternion.ToRoll:TScalar;
var t:TScalar;
begin
 // Order of rotations: Roll (Z), Pitch (X), Yaw (Y)
 t:=2.0*((x*w)-(y*z));
 if t<-0.995 then begin
  result:=-ArcTan2(2.0*((x*z)-(y*w)),1.0-(2.0*(sqr(y)+sqr(z))));
 end else if t>0.995 then begin
  result:=ArcTan2(2.0*((x*z)-(y*w)),1.0-(2.0*(sqr(y)+sqr(z))));
 end else begin
  result:=ArcTan2(2.0*((x*y)+(z*w)),1.0-(2.0*(sqr(x)+sqr(z))));
 end;
end;

procedure TQuaternion.ToAngleAxis(out Angle:TScalar;out Axis:TVector3);
var SinAngle:TScalar;
    Quaternion:TQuaternion;
begin
 Quaternion:=Normalize;
 SinAngle:=sqrt(1.0-sqr(Quaternion.w));
 if System.abs(SinAngle)<EPSILON then begin
  SinAngle:=1.0;
 end;
 Angle:=2.0*ArcCos(Quaternion.w);
 Axis.x:=Quaternion.x/SinAngle;
 Axis.y:=Quaternion.y/SinAngle;
 Axis.z:=Quaternion.z/SinAngle;
end;

function TQuaternion.Generator:TVector3;
var s:TScalar;
begin
 s:=sqrt(1.0-sqr(w));
 result.x:=x;
 result.y:=y;
 result.z:=z;
 if s>0.0 then begin
  result:=result*s;
 end;
 result:=result*(2.0*ArcTan2(s,w));
end;

function TQuaternion.Flip:TQuaternion;
begin
 result.x:=x;
 result.y:=z;
 result.z:=-y;
 result.w:=w;
end;

function TQuaternion.Perpendicular:TQuaternion;
var v,p:TQuaternion;
begin
 v:=p.Normalize;
 p.x:=System.abs(v.x);
 p.y:=System.abs(v.y);
 p.z:=System.abs(v.z);
 p.w:=System.abs(v.w);
 if (p.x<=p.y) and (p.x<=p.z) and (p.x<=p.w) then begin
  p.x:=1.0;
  p.y:=0.0;
  p.z:=0.0;
  p.w:=0.0;
 end else if (p.y<=p.x) and (p.y<=p.z) and (p.y<=p.w) then begin
  p.x:=0.0;
  p.y:=1.0;
  p.z:=0.0;
  p.w:=0.0;
 end else if (p.z<=p.x) and (p.z<=p.y) and (p.z<=p.w) then begin
  p.x:=0.0;
  p.y:=0.0;
  p.z:=0.0;
  p.w:=1.0;
 end else begin
  p.x:=0.0;
  p.y:=0.0;
  p.z:=1.0;
  p.w:=0.0;
 end;
 result:=p-(v*v.Dot(p));
end;

function TQuaternion.Conjugate:TQuaternion;
{$if defined(cpu386)}
const XORMask:array[0..3] of TUInt32=($80000000,$80000000,$80000000,$00000000);
asm
 movups xmm0,dqword ptr [eax]
 movups xmm1,dqword ptr [XORMask]
 xorps xmm0,xmm1
 movups dqword ptr [result],xmm0
end;
{$elseif defined(cpux64)}
const XORMask:array[0..3] of TUInt32=($80000000,$80000000,$80000000,$00000000);
asm
{$ifdef Windows}
 movups xmm0,dqword ptr [rcx]
{$else}
 movups xmm0,dqword ptr [rdi]
{$endif}
{$ifdef fpc}
 movups xmm1,dqword ptr [rip+XORMask]
{$else}
 movups xmm1,dqword ptr [rel XORMask]
{$endif}
 xorps xmm0,xmm1
 movups dqword ptr [result],xmm0
end;
{$else}
begin
 result.x:=-x;
 result.y:=-y;
 result.z:=-z;
 result.w:=w;
end;
{$ifend}

function TQuaternion.Inverse:TQuaternion;
{$if defined(cpu386)}
const XORMask:array[0..3] of TUInt32=($80000000,$80000000,$80000000,$00000000);
asm
 movups xmm2,dqword ptr [eax]
 movups xmm3,dqword ptr [XORMask]
 movaps xmm0,xmm2
 mulps xmm0,xmm0
 movhlps xmm1,xmm0
 addps xmm0,xmm1
 pshufd xmm1,xmm0,$01
 addss xmm0,xmm1
 sqrtss xmm0,xmm0           // not rsqrtss! because rsqrtss has only 12-bit accuracy
 shufps xmm0,xmm0,$00
 divps xmm2,xmm0
 xorps xmm2,xmm3
 movups dqword ptr [result],xmm2
end;
{$elseif defined(cpux64)}
const XORMask:array[0..3] of TUInt32=($80000000,$80000000,$80000000,$00000000);
asm
{$ifdef Windows}
 movups xmm2,dqword ptr [rcx]
{$else}
 movups xmm2,dqword ptr [rdi]
{$endif}
{$ifdef fpc}
 movups xmm3,dqword ptr [rip+XORMask]
{$else}
 movups xmm3,dqword ptr [rel XORMask]
{$endif}
 movaps xmm0,xmm2
 mulps xmm0,xmm0
 movhlps xmm1,xmm0
 addps xmm0,xmm1
 pshufd xmm1,xmm0,$01
 addss xmm0,xmm1
 sqrtss xmm0,xmm0            // not rsqrtss! because rsqrtss has only 12-bit accuracy
 shufps xmm0,xmm0,$00
 divps xmm2,xmm0
 xorps xmm2,xmm3
 movups dqword ptr [result],xmm2
end;
{$else}
var Normal:TScalar;
begin
 Normal:=sqrt(sqr(x)+sqr(y)+sqr(z)+sqr(w));
 if Normal>0.0 then begin
  Normal:=1.0/Normal;
 end;
 result.x:=-(x*Normal);
 result.y:=-(y*Normal);
 result.z:=-(z*Normal);
 result.w:=w*Normal;
end;
{$ifend}

function TQuaternion.Length:TScalar;
{$if defined(cpu386)}
asm
 movups xmm0,dqword ptr [eax]
 mulps xmm0,xmm0
 movhlps xmm1,xmm0
 addps xmm0,xmm1
 pshufd xmm1,xmm0,$01
 addss xmm1,xmm0
 sqrtss xmm0,xmm1
 movss dword ptr [result],xmm0
end;
{$elseif defined(cpux64)}
asm
{$ifdef Windows}
 movups xmm0,dqword ptr [rcx]
{$else}
 movups xmm0,dqword ptr [rdi]
{$endif}
 mulps xmm0,xmm0
 movhlps xmm1,xmm0
 addps xmm0,xmm1
 pshufd xmm1,xmm0,$01
 addss xmm1,xmm0
 sqrtss xmm0,xmm1
 movss dword ptr [result],xmm0
end;
{$else}
begin
 result:=sqrt(sqr(x)+sqr(y)+sqr(z)+sqr(w));
end;
{$ifend}

function TQuaternion.SquaredLength:TScalar;
{$if defined(cpu386)}
asm
 movups xmm0,dqword ptr [eax]
 mulps xmm0,xmm0
 movhlps xmm1,xmm0
 addps xmm0,xmm1
 pshufd xmm1,xmm0,$01
 addss xmm0,xmm1
 movss dword ptr [result],xmm0
end;
{$elseif defined(cpux64)}
asm
{$ifdef Windows}
 movups xmm0,dqword ptr [rcx]
{$else}
 movups xmm0,dqword ptr [rdi]
{$endif}
 mulps xmm0,xmm0
 movhlps xmm1,xmm0
 addps xmm0,xmm1
 pshufd xmm1,xmm0,$01
 addss xmm0,xmm1
 movss dword ptr [result],xmm0
end;
{$else}
begin
 result:=sqr(x)+sqr(y)+sqr(z)+sqr(w);
end;
{$ifend}

function TQuaternion.Normalize:TQuaternion;
{$if defined(cpu386)}
asm
 movups xmm0,dqword ptr [eax]
 movaps xmm2,xmm0
 mulps xmm0,xmm0
 movhlps xmm1,xmm0
 addps xmm0,xmm1
 pshufd xmm1,xmm0,$01
 addss xmm0,xmm1
 sqrtss xmm0,xmm0               // not rsqrtss! because rsqrtss has only 12-bit accuracy
 shufps xmm0,xmm0,$00
 divps xmm2,xmm0
 subps xmm1,xmm2
 cmpps xmm1,xmm0,7
 andps xmm2,xmm1
 movups dqword ptr [edx],xmm2
end;
{$elseif defined(cpux64)}
asm
{$ifdef Windows}
 movups xmm0,dqword ptr [rcx]
{$else}
 movups xmm0,dqword ptr [rdi]
{$endif}
 movaps xmm2,xmm0
 mulps xmm0,xmm0
 movhlps xmm1,xmm0
 addps xmm0,xmm1
 pshufd xmm1,xmm0,$01
 addss xmm0,xmm1
 sqrtss xmm0,xmm0                // not rsqrtss! because rsqrtss has only 12-bit accuracy
 shufps xmm0,xmm0,$00
 divps xmm2,xmm0
 subps xmm1,xmm2
 cmpps xmm1,xmm0,7
 andps xmm2,xmm1
{$ifdef Windows}
 movups dqword ptr [rdx],xmm2
{$else}
 movups dqword ptr [rax],xmm2
{$endif}
end;
{$else}
var Factor:TScalar;
begin
 Factor:=sqrt(sqr(x)+sqr(y)+sqr(z)+sqr(w));
 if Factor<>0.0 then begin
  Factor:=1.0/Factor;
  result.x:=x*Factor;
  result.y:=y*Factor;
  result.z:=z*Factor;
  result.w:=w*Factor;
 end else begin
  result.x:=0.0;
  result.y:=0.0;
  result.z:=0.0;
  result.w:=0.0;
 end;
end;
{$ifend}

function TQuaternion.DistanceTo({$ifdef fpc}constref{$else}const{$endif} b:TQuaternion):TScalar;
{$if defined(cpu386)}
asm
 movups xmm0,dqword ptr [eax]
 movups xmm1,dqword ptr [edx]
 subps xmm0,xmm1
 mulps xmm0,xmm0         // xmm0 = w*w, z*z, y*y, x*x
 movaps xmm1,xmm0        // xmm1 = xmm0
 shufps xmm1,xmm1,$4e    // xmm1 = z*z, w*w, x*x, y*y
 addps xmm0,xmm1         // xmm0 = xmm0 + xmm1 = (zw*zw, zw*zw, xy*zw, xy*zw)
 movaps xmm1,xmm0        // xmm1 = xmm0
 shufps xmm1,xmm1,$b1    // xmm0 = xy*xy, xy*xy, zw*zw, zw*zw
 addps xmm1,xmm0         // xmm1 = xmm1 + xmm0 = (xyzw, xyzw, xyzw, xyzw)
 sqrtss xmm0,xmm1
 movss dword ptr [result],xmm0
end;
{$elseif defined(cpux64)}
asm
{$ifdef Windows}
 movups xmm0,dqword ptr [rcx]
 movups xmm1,dqword ptr [rdx]
{$else}
 movups xmm0,dqword ptr [rdi]
 movups xmm1,dqword ptr [rsi]
{$endif}
 subps xmm0,xmm1
 mulps xmm0,xmm0         // xmm0 = w*w, z*z, y*y, x*x
 movaps xmm1,xmm0        // xmm1 = xmm0
 shufps xmm1,xmm1,$4e    // xmm1 = z*z, w*w, x*x, y*y
 addps xmm0,xmm1         // xmm0 = xmm0 + xmm1 = (zw*zw, zw*zw, xy*zw, xy*zw)
 movaps xmm1,xmm0        // xmm1 = xmm0
 shufps xmm1,xmm1,$b1    // xmm0 = xy*xy, xy*xy, zw*zw, zw*zw
 addps xmm1,xmm0         // xmm1 = xmm1 + xmm0 = (xyzw, xyzw, xyzw, xyzw)
 sqrtss xmm0,xmm1
 movss dword ptr [result],xmm0
end;
{$else}
begin
 result:=sqrt(sqr(x-b.x)+sqr(y-b.y)+sqr(z-b.z)+sqr(w-b.w));
end;
{$ifend}

function TQuaternion.Abs:TQuaternion;
{$if defined(cpu386)}
asm
 movups xmm0,dqword ptr [eax]
 xorps xmm1,xmm1
 subps xmm1,xmm0
 maxps xmm0,xmm1
 movups dqword ptr [edx],xmm0
end;
{$elseif defined(cpux64)}
asm
{$ifdef Windows}
 movups xmm0,dqword ptr [rcx]
{$else}
 movups xmm0,dqword ptr [rdi]
{$endif}
 xorps xmm1,xmm1
 subps xmm1,xmm0
 maxps xmm0,xmm1
{$ifdef Windows}
 movups dqword ptr [rdx],xmm0
{$else}
 movups dqword ptr [rax],xmm0
{$endif}
end;
{$else}
begin
 result.x:=System.abs(x);
 result.y:=System.abs(y);
 result.z:=System.abs(z);
 result.w:=System.abs(w);
end;
{$ifend}

function TQuaternion.Dot({$ifdef fpc}constref{$else}const{$endif} b:TQuaternion):TScalar; {$if not (defined(cpu386) or defined(cpux64))}{$ifdef CAN_INLINE}inline;{$endif}{$ifend}
{$if defined(cpu386)}
asm
 movups xmm0,dqword ptr [eax]
 movups xmm1,dqword ptr [edx]
 mulps xmm0,xmm1
 movaps xmm1,xmm0
 shufps xmm1,xmm0,$b1
 addps xmm0,xmm1
 movhlps xmm1,xmm0
 addss xmm0,xmm1
 movss dword ptr [result],xmm0
end;
{$elseif defined(cpux64)}
asm
{$ifdef Windows}
 movups xmm0,dqword ptr [rcx]
 movups xmm1,dqword ptr [rdx]
{$else}
 movups xmm0,dqword ptr [rdi]
 movups xmm1,dqword ptr [rsi]
{$endif}
 mulps xmm0,xmm1
 movaps xmm1,xmm0
 shufps xmm1,xmm0,$b1
 addps xmm0,xmm1
 movhlps xmm1,xmm0
 addss xmm0,xmm1
 movss dword ptr [result],xmm0
end;
{$else}
begin
 result:=(x*b.x)+(y*b.y)+(z*b.z)+(w*b.w);
end;
{$ifend}

function TQuaternion.Lerp(const b:TQuaternion;const t:TScalar):TQuaternion;
var SignFactor:TScalar;
begin
 if Dot(b)<0.0 then begin
  SignFactor:=-1.0;
 end else begin
  SignFactor:=1.0;
 end;
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b*SignFactor;
 end else begin
  result:=(self*(1.0-t))+(b*(t*SignFactor));
 end;
end;

function TQuaternion.Nlerp(const b:TQuaternion;const t:TScalar):TQuaternion;
begin
 result:=Lerp(b,t).Normalize;
end;

function TQuaternion.Slerp(const b:TQuaternion;const t:TScalar):TQuaternion;
var Omega,co,so,s0,s1,s2:TScalar;
begin
 co:=Dot(b);
 if co<0.0 then begin
  co:=-co;
  s2:=-1.0;
 end else begin
  s2:=1.0;
 end;
 if (1.0-co)>EPSILON then begin
  Omega:=ArcCos(co);
  so:=sin(Omega);
  s0:=sin((1.0-t)*Omega)/so;
  s1:=sin(t*Omega)/so;
 end else begin
  s0:=1.0-t;
  s1:=t;
 end;
 result:=(s0*self)+(b*(s1*s2));
end;

function TQuaternion.RotateAroundAxis(const b:TQuaternion):TQuaternion;
begin
 result.x:=((x*b.w)+(z*b.y))-(y*b.z);
 result.y:=((x*b.z)+(y*b.w))-(z*b.x);
 result.z:=((y*b.x)+(z*b.w))-(x*b.y);
 result.w:=((x*b.x)+(y*b.y))+(z*b.z);
end;

function TQuaternion.Integrate(const Omega:TVector3;const DeltaTime:TScalar):TQuaternion;
var ThetaLenSquared,ThetaLen,s,w:TScalar;
    Theta:TVector3;
begin
 Theta:=Omega*(DeltaTime*0.5);
 ThetaLenSquared:=Theta.SquaredLength;
 if (sqr(ThetaLenSquared)/24.0)<EPSILON then begin
  s:=1.0-(ThetaLenSquared/6.0);
  w:=1.0-(ThetaLenSquared*0.5);
 end else begin
  ThetaLen:=sqrt(ThetaLenSquared);
  s:=sin(ThetaLen)/ThetaLen;
  w:=cos(ThetaLen);
 end;
 result.Vector.xyz:=Theta*s;
 result.Vector.w:=w;
 result:=result*self;
end;

function TQuaternion.Spin(const Omega:TVector3;const DeltaTime:TScalar):TQuaternion;
var wq:TQuaternion;
begin
 wq.x:=Omega.x*DeltaTime;
 wq.y:=Omega.y*DeltaTime;
 wq.z:=Omega.z*DeltaTime;
 wq.w:=0.0;
 result:=(self+((wq*self)*0.5)).Normalize;
end;

{constructor TMatrix2x2.Create;
begin
 RawComponents[0,0]:=1.0;
 RawComponents[0,1]:=0.0;
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=1.0;
end;{}

constructor TMatrix2x2.Create(const pX:TScalar);
begin
 RawComponents[0,0]:=pX;
 RawComponents[0,1]:=pX;
 RawComponents[1,0]:=pX;
 RawComponents[1,1]:=pX;
end;

constructor TMatrix2x2.Create(const pXX,pXY,pYX,pYY:TScalar);
begin
 RawComponents[0,0]:=pXX;
 RawComponents[0,1]:=pXY;
 RawComponents[1,0]:=pYX;
 RawComponents[1,1]:=pYY;
end;

constructor TMatrix2x2.Create(const pX,pY:TVector2);
begin
 RawComponents[0,0]:=pX.x;
 RawComponents[0,1]:=pX.y;
 RawComponents[1,0]:=pY.x;
 RawComponents[1,1]:=pY.y;
end;

class operator TMatrix2x2.Implicit(const a:TScalar):TMatrix2x2;
begin
 result.RawComponents[0,0]:=a;
 result.RawComponents[0,1]:=a;
 result.RawComponents[1,0]:=a;
 result.RawComponents[1,1]:=a;
end;

class operator TMatrix2x2.Explicit(const a:TScalar):TMatrix2x2;
begin
 result.RawComponents[0,0]:=a;
 result.RawComponents[0,1]:=a;
 result.RawComponents[1,0]:=a;
 result.RawComponents[1,1]:=a;
end;

class operator TMatrix2x2.Equal(const a,b:TMatrix2x2):boolean;
begin
 result:=SameValue(a.RawComponents[0,0],b.RawComponents[0,0]) and
         SameValue(a.RawComponents[0,1],b.RawComponents[0,1]) and
         SameValue(a.RawComponents[1,0],b.RawComponents[1,0]) and
         SameValue(a.RawComponents[1,1],b.RawComponents[1,1]);
end;

class operator TMatrix2x2.NotEqual(const a,b:TMatrix2x2):boolean;
begin
 result:=(not SameValue(a.RawComponents[0,0],b.RawComponents[0,0])) or
         (not SameValue(a.RawComponents[0,1],b.RawComponents[0,1])) or
         (not SameValue(a.RawComponents[1,0],b.RawComponents[1,0])) or
         (not SameValue(a.RawComponents[1,1],b.RawComponents[1,1]));
end;

class operator TMatrix2x2.Inc(const a:TMatrix2x2):TMatrix2x2;
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]+1.0;
 result.RawComponents[0,1]:=a.RawComponents[0,1]+1.0;
 result.RawComponents[1,0]:=a.RawComponents[1,0]+1.0;
 result.RawComponents[1,1]:=a.RawComponents[1,1]+1.0;
end;

class operator TMatrix2x2.Dec(const a:TMatrix2x2):TMatrix2x2;
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]-1.0;
 result.RawComponents[0,1]:=a.RawComponents[0,1]-1.0;
 result.RawComponents[1,0]:=a.RawComponents[1,0]-1.0;
 result.RawComponents[1,1]:=a.RawComponents[1,1]-1.0;
end;

class operator TMatrix2x2.Add(const a,b:TMatrix2x2):TMatrix2x2;
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]+b.RawComponents[0,0];
 result.RawComponents[0,1]:=a.RawComponents[0,1]+b.RawComponents[0,1];
 result.RawComponents[1,0]:=a.RawComponents[1,0]+b.RawComponents[1,0];
 result.RawComponents[1,1]:=a.RawComponents[1,1]+b.RawComponents[1,1];
end;

class operator TMatrix2x2.Add(const a:TMatrix2x2;const b:TScalar):TMatrix2x2;
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]+b;
 result.RawComponents[0,1]:=a.RawComponents[0,1]+b;
 result.RawComponents[1,0]:=a.RawComponents[1,0]+b;
 result.RawComponents[1,1]:=a.RawComponents[1,1]+b;
end;

class operator TMatrix2x2.Add(const a:TScalar;const b:TMatrix2x2):TMatrix2x2;
begin
 result.RawComponents[0,0]:=a+b.RawComponents[0,0];
 result.RawComponents[0,1]:=a+b.RawComponents[0,1];
 result.RawComponents[1,0]:=a+b.RawComponents[1,0];
 result.RawComponents[1,1]:=a+b.RawComponents[1,1];
end;

class operator TMatrix2x2.Subtract(const a,b:TMatrix2x2):TMatrix2x2;
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]-b.RawComponents[0,0];
 result.RawComponents[0,1]:=a.RawComponents[0,1]-b.RawComponents[0,1];
 result.RawComponents[1,0]:=a.RawComponents[1,0]-b.RawComponents[1,0];
 result.RawComponents[1,1]:=a.RawComponents[1,1]-b.RawComponents[1,1];
end;

class operator TMatrix2x2.Subtract(const a:TMatrix2x2;const b:TScalar):TMatrix2x2;
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]-b;
 result.RawComponents[0,1]:=a.RawComponents[0,1]-b;
 result.RawComponents[1,0]:=a.RawComponents[1,0]-b;
 result.RawComponents[1,1]:=a.RawComponents[1,1]-b;
end;

class operator TMatrix2x2.Subtract(const a:TScalar;const b:TMatrix2x2): TMatrix2x2;
begin
 result.RawComponents[0,0]:=a-b.RawComponents[0,0];
 result.RawComponents[0,1]:=a-b.RawComponents[0,1];
 result.RawComponents[1,0]:=a-b.RawComponents[1,0];
 result.RawComponents[1,1]:=a-b.RawComponents[1,1];
end;

class operator TMatrix2x2.Multiply(const a,b:TMatrix2x2):TMatrix2x2;
begin
 result.RawComponents[0,0]:=(a.RawComponents[0,0]*b.RawComponents[0,0])+(a.RawComponents[0,1]*b.RawComponents[1,0]);
 result.RawComponents[0,1]:=(a.RawComponents[0,0]*b.RawComponents[0,1])+(a.RawComponents[0,1]*b.RawComponents[1,1]);
 result.RawComponents[1,0]:=(a.RawComponents[1,0]*b.RawComponents[0,0])+(a.RawComponents[1,1]*b.RawComponents[1,0]);
 result.RawComponents[1,1]:=(a.RawComponents[1,0]*b.RawComponents[0,1])+(a.RawComponents[1,1]*b.RawComponents[1,1]);
end;

class operator TMatrix2x2.Multiply(const a:TMatrix2x2;const b:TScalar):TMatrix2x2;
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]*b;
 result.RawComponents[0,1]:=a.RawComponents[0,1]*b;
 result.RawComponents[1,0]:=a.RawComponents[1,0]*b;
 result.RawComponents[1,1]:=a.RawComponents[1,1]*b;
end;

class operator TMatrix2x2.Multiply(const a:TScalar;const b:TMatrix2x2):TMatrix2x2;
begin
 result.RawComponents[0,0]:=a*b.RawComponents[0,0];
 result.RawComponents[0,1]:=a*b.RawComponents[0,1];
 result.RawComponents[1,0]:=a*b.RawComponents[1,0];
 result.RawComponents[1,1]:=a*b.RawComponents[1,1];
end;

class operator TMatrix2x2.Multiply(const a:TMatrix2x2;const b:TVector2):TVector2;
begin
 result.x:=(a.RawComponents[0,0]*b.x)+(a.RawComponents[1,0]*b.y);
 result.y:=(a.RawComponents[0,1]*b.x)+(a.RawComponents[1,1]*b.y);
end;

class operator TMatrix2x2.Multiply(const a:TVector2;const b:TMatrix2x2):TVector2;
begin
 result.x:=(a.x*b.RawComponents[0,0])+(a.y*b.RawComponents[0,1]);
 result.y:=(a.x*b.RawComponents[1,0])+(a.y*b.RawComponents[1,1]);
end;

class operator TMatrix2x2.Divide(const a,b:TMatrix2x2):TMatrix2x2;
begin
 result:=a*b.Inverse;
end;

class operator TMatrix2x2.Divide(const a:TMatrix2x2;const b:TScalar):TMatrix2x2;
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]/b;
 result.RawComponents[0,1]:=a.RawComponents[0,1]/b;
 result.RawComponents[1,0]:=a.RawComponents[1,0]/b;
 result.RawComponents[1,1]:=a.RawComponents[1,1]/b;
end;

class operator TMatrix2x2.Divide(const a:TScalar;const b:TMatrix2x2):TMatrix2x2;
begin
 result.RawComponents[0,0]:=a/b.RawComponents[0,0];
 result.RawComponents[0,1]:=a/b.RawComponents[0,1];
 result.RawComponents[1,0]:=a/b.RawComponents[1,0];
 result.RawComponents[1,1]:=a/b.RawComponents[1,1];
end;

class operator TMatrix2x2.IntDivide(const a,b:TMatrix2x2):TMatrix2x2;
begin
 result:=a*b.Inverse;
end;

class operator TMatrix2x2.IntDivide(const a:TMatrix2x2;const b:TScalar):TMatrix2x2;
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]/b;
 result.RawComponents[0,1]:=a.RawComponents[0,1]/b;
 result.RawComponents[1,0]:=a.RawComponents[1,0]/b;
 result.RawComponents[1,1]:=a.RawComponents[1,1]/b;
end;

class operator TMatrix2x2.IntDivide(const a:TScalar;const b:TMatrix2x2):TMatrix2x2;
begin
 result.RawComponents[0,0]:=a/b.RawComponents[0,0];
 result.RawComponents[0,1]:=a/b.RawComponents[0,1];
 result.RawComponents[1,0]:=a/b.RawComponents[1,0];
 result.RawComponents[1,1]:=a/b.RawComponents[1,1];
end;

class operator TMatrix2x2.Modulus(const a,b:TMatrix2x2):TMatrix2x2;
begin
 result.RawComponents[0,0]:=Modulo(a.RawComponents[0,0],b.RawComponents[0,0]);
 result.RawComponents[0,1]:=Modulo(a.RawComponents[0,1],b.RawComponents[0,1]);
 result.RawComponents[1,0]:=Modulo(a.RawComponents[1,0],b.RawComponents[1,0]);
 result.RawComponents[1,1]:=Modulo(a.RawComponents[1,1],b.RawComponents[1,1]);
end;

class operator TMatrix2x2.Modulus(const a:TMatrix2x2;const b:TScalar):TMatrix2x2;
begin
 result.RawComponents[0,0]:=Modulo(a.RawComponents[0,0],b);
 result.RawComponents[0,1]:=Modulo(a.RawComponents[0,1],b);
 result.RawComponents[1,0]:=Modulo(a.RawComponents[1,0],b);
 result.RawComponents[1,1]:=Modulo(a.RawComponents[1,1],b);
end;

class operator TMatrix2x2.Modulus(const a:TScalar;const b:TMatrix2x2):TMatrix2x2;
begin
 result.RawComponents[0,0]:=Modulo(a,b.RawComponents[0,0]);
 result.RawComponents[0,1]:=Modulo(a,b.RawComponents[0,1]);
 result.RawComponents[1,0]:=Modulo(a,b.RawComponents[1,0]);
 result.RawComponents[1,1]:=Modulo(a,b.RawComponents[1,1]);
end;

class operator TMatrix2x2.Negative(const a:TMatrix2x2):TMatrix2x2;
begin
 result.RawComponents[0,0]:=-a.RawComponents[0,0];
 result.RawComponents[0,1]:=-a.RawComponents[0,1];
 result.RawComponents[1,0]:=-a.RawComponents[1,0];
 result.RawComponents[1,1]:=-a.RawComponents[1,1];
end;

class operator TMatrix2x2.Positive(const a:TMatrix2x2):TMatrix2x2;
begin
 result:=a;
end;

function TMatrix2x2.GetComponent(const pIndexA,pIndexB:TInt32):TScalar;
begin
 result:=RawComponents[pIndexA,pIndexB];
end;

procedure TMatrix2x2.SetComponent(const pIndexA,pIndexB:TInt32;const pValue:TScalar);
begin
 RawComponents[pIndexA,pIndexB]:=pValue;
end;

function TMatrix2x2.GetColumn(const pIndex:TInt32):TVector2;
begin
 result.x:=RawComponents[pIndex,0];
 result.y:=RawComponents[pIndex,1];
end;

procedure TMatrix2x2.SetColumn(const pIndex:TInt32;const pValue:TVector2);
begin
 RawComponents[pIndex,0]:=pValue.x;
 RawComponents[pIndex,1]:=pValue.y;
end;

function TMatrix2x2.GetRow(const pIndex:TInt32):TVector2;
begin
 result.x:=RawComponents[0,pIndex];
 result.y:=RawComponents[1,pIndex];
end;

procedure TMatrix2x2.SetRow(const pIndex:TInt32;const pValue:TVector2);
begin
 RawComponents[0,pIndex]:=pValue.x;
 RawComponents[1,pIndex]:=pValue.y;
end;

function TMatrix2x2.Determinant:TScalar;
begin
 result:=(RawComponents[0,0]*RawComponents[1,1])-(RawComponents[0,1]*RawComponents[1,0]);
end;

function TMatrix2x2.Inverse:TMatrix2x2;
var d:TScalar;
begin
 d:=(RawComponents[0,0]*RawComponents[1,1])-(RawComponents[0,1]*RawComponents[1,0]);
 if d<>0.0 then begin
  d:=1.0/d;
  result.RawComponents[0,0]:=RawComponents[1,1]*d;
  result.RawComponents[0,1]:=-(RawComponents[0,1]*d);
  result.RawComponents[1,0]:=-(RawComponents[1,0]*d);
  result.RawComponents[1,1]:=RawComponents[0,0]*d;
 end else begin
  result:=Matrix2x2Identity;
 end;
end;

function TMatrix2x2.Transpose:TMatrix2x2;
begin
 result.RawComponents[0,0]:=RawComponents[0,0];
 result.RawComponents[0,1]:=RawComponents[1,0];
 result.RawComponents[1,0]:=RawComponents[0,1];
 result.RawComponents[1,1]:=RawComponents[1,1];
end;

function TDecomposedMatrix3x3.Lerp(const b:TDecomposedMatrix3x3;const t:TScalar):TDecomposedMatrix3x3;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  result.Scale:=Scale.Lerp(b.Scale,t);
  result.Skew:=Skew.Lerp(b.Skew,t);
  result.Rotation:=Rotation.Lerp(b.Rotation,t);
 end;
end;

function TDecomposedMatrix3x3.Nlerp(const b:TDecomposedMatrix3x3;const t:TScalar):TDecomposedMatrix3x3;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  result.Scale:=Scale.Lerp(b.Scale,t);
  result.Skew:=Skew.Lerp(b.Skew,t);
  result.Rotation:=Rotation.Nlerp(b.Rotation,t);
 end;
end;

function TDecomposedMatrix3x3.Slerp(const b:TDecomposedMatrix3x3;const t:TScalar):TDecomposedMatrix3x3;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  result.Scale:=Scale.Lerp(b.Scale,t);
  result.Skew:=Skew.Lerp(b.Skew,t);
  result.Rotation:=Rotation.Slerp(b.Rotation,t);
 end;
end;

{constructor TMatrix3x3.Create;
begin
 RawComponents[0,0]:=1.0;
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=0.0;
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=1.0;
 RawComponents[1,2]:=0.0;
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=1.0;
end;{}

constructor TMatrix3x3.Create(const pX:TScalar);
begin
 RawComponents[0,0]:=pX;
 RawComponents[0,1]:=pX;
 RawComponents[0,2]:=pX;
 RawComponents[1,0]:=pX;
 RawComponents[1,1]:=pX;
 RawComponents[1,2]:=pX;
 RawComponents[2,0]:=pX;
 RawComponents[2,1]:=pX;
 RawComponents[2,2]:=pX;
end;

constructor TMatrix3x3.Create(const pXX,pXY,pXZ,pYX,pYY,pYZ,pZX,pZY,pZZ:TScalar);
begin
 RawComponents[0,0]:=pXX;
 RawComponents[0,1]:=pXY;
 RawComponents[0,2]:=pXZ;
 RawComponents[1,0]:=pYX;
 RawComponents[1,1]:=pYY;
 RawComponents[1,2]:=pYZ;
 RawComponents[2,0]:=pZX;
 RawComponents[2,1]:=pZY;
 RawComponents[2,2]:=pZZ;
end;

constructor TMatrix3x3.Create(const pX,pY,pZ:TVector3);
begin
 RawComponents[0,0]:=pX.x;
 RawComponents[0,1]:=pX.y;
 RawComponents[0,2]:=pX.z;
 RawComponents[1,0]:=pY.x;
 RawComponents[1,1]:=pY.y;
 RawComponents[1,2]:=pY.z;
 RawComponents[2,0]:=pZ.x;
 RawComponents[2,1]:=pZ.y;
 RawComponents[2,2]:=pZ.z;
end;

constructor TMatrix3x3.CreateRotateX(const Angle:TScalar);
begin
 RawComponents[0,0]:=1.0;
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=0.0;
 RawComponents[1,0]:=0.0;
 SinCos(Angle,RawComponents[1,2],RawComponents[1,1]);
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=-RawComponents[1,2];
 RawComponents[2,2]:=RawComponents[1,1];
end;

constructor TMatrix3x3.CreateRotateY(const Angle:TScalar);
begin
 SinCos(Angle,RawComponents[2,0],RawComponents[0,0]);
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=-RawComponents[2,0];
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=1.0;
 RawComponents[1,2]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=RawComponents[0,0];
end;

constructor TMatrix3x3.CreateRotateZ(const Angle:TScalar);
begin
 SinCos(Angle,RawComponents[0,1],RawComponents[0,0]);
 RawComponents[0,2]:=0.0;
 RawComponents[1,0]:=-RawComponents[0,1];
 RawComponents[1,1]:=RawComponents[0,0];
 RawComponents[1,2]:=0.0;
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=1.0;
end;

constructor TMatrix3x3.CreateRotate(const Angle:TScalar;const Axis:TVector3);
var SinusAngle,CosinusAngle:TScalar;
begin
 SinCos(Angle,SinusAngle,CosinusAngle);
 RawComponents[0,0]:=CosinusAngle+((1.0-CosinusAngle)*sqr(Axis.x));
 RawComponents[1,0]:=((1.0-CosinusAngle)*Axis.x*Axis.y)-(Axis.z*SinusAngle);
 RawComponents[2,0]:=((1.0-CosinusAngle)*Axis.x*Axis.z)+(Axis.y*SinusAngle);
 RawComponents[0,1]:=((1.0-CosinusAngle)*Axis.x*Axis.z)+(Axis.z*SinusAngle);
 RawComponents[1,1]:=CosinusAngle+((1.0-CosinusAngle)*sqr(Axis.y));
 RawComponents[2,1]:=((1.0-CosinusAngle)*Axis.y*Axis.z)-(Axis.x*SinusAngle);
 RawComponents[0,2]:=((1.0-CosinusAngle)*Axis.x*Axis.z)-(Axis.y*SinusAngle);
 RawComponents[1,2]:=((1.0-CosinusAngle)*Axis.y*Axis.z)+(Axis.x*SinusAngle);
 RawComponents[2,2]:=CosinusAngle+((1.0-CosinusAngle)*sqr(Axis.z));
end;

constructor TMatrix3x3.CreateScale(const sx,sy,sz:TScalar);
begin
 RawComponents[0,0]:=sx;
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=0.0;
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=sy;
 RawComponents[1,2]:=0.0;
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=sz;
end;

constructor TMatrix3x3.CreateScale(const pScale:TVector3);
begin
 RawComponents[0,0]:=pScale.x;
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=0.0;
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=pScale.y;
 RawComponents[1,2]:=0.0;
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=pScale.z;
end;

constructor TMatrix3x3.CreateFromToRotation(const FromDirection,ToDirection:TVector3);
var e,h,hvx,hvz,hvxy,hvxz,hvyz:TScalar;
    x,u,v,c:TVector3;
begin
 e:=FromDirection.Dot(ToDirection);
 if abs(e)>(1.0-EPSILON) then begin
  x:=FromDirection.Abs;
  if x.x<x.y then begin
   if x.x<x.z then begin
    x.x:=1.0;
    x.y:=0.0;
    x.z:=0.0;
   end else begin
    x.x:=0.0;
    x.y:=0.0;
    x.z:=1.0;
   end;
  end else begin
   if x.y<x.z then begin
    x.x:=0.0;
    x.y:=1.0;
    x.z:=0.0;
   end else begin
    x.x:=0.0;
    x.y:=0.0;
    x.z:=1.0;
   end;
  end;
  u:=x-FromDirection;
  v:=x-ToDirection;
  c.x:=2.0/(sqr(u.x)+sqr(u.y)+sqr(u.z));
  c.y:=2.0/(sqr(v.x)+sqr(v.y)+sqr(v.z));
  c.z:=c.x*c.y*((u.x*v.x)+(u.y*v.y)+(u.z*v.z));
  RawComponents[0,0]:=1.0+((c.z*(v.x*u.x))-((c.y*(v.x*v.x))+(c.x*(u.x*u.x))));
  RawComponents[0,1]:=(c.z*(v.x*u.y))-((c.y*(v.x*v.y))+(c.x*(u.x*u.y)));
  RawComponents[0,2]:=(c.z*(v.x*u.z))-((c.y*(v.x*v.z))+(c.x*(u.x*u.z)));
  RawComponents[1,0]:=(c.z*(v.y*u.x))-((c.y*(v.y*v.x))+(c.x*(u.y*u.x)));
  RawComponents[1,1]:=1.0+((c.z*(v.y*u.y))-((c.y*(v.y*v.y))+(c.x*(u.y*u.y))));
  RawComponents[1,2]:=(c.z*(v.y*u.z))-((c.y*(v.y*v.z))+(c.x*(u.y*u.z)));
  RawComponents[2,0]:=(c.z*(v.z*u.x))-((c.y*(v.z*v.x))+(c.x*(u.z*u.x)));
  RawComponents[2,1]:=(c.z*(v.z*u.y))-((c.y*(v.z*v.y))+(c.x*(u.z*u.y)));
  RawComponents[2,2]:=1.0+((c.z*(v.z*u.z))-((c.y*(v.z*v.z))+(c.x*(u.z*u.z))));
 end else begin
  v:=FromDirection.Cross(ToDirection);
  h:=1.0/(1.0+e);
  hvx:=h*v.x;
  hvz:=h*v.z;
  hvxy:=hvx*v.y;
  hvxz:=hvx*v.z;
  hvyz:=hvz*v.y;
  RawComponents[0,0]:=e+(hvx*v.x);
  RawComponents[0,1]:=hvxy-v.z;
  RawComponents[0,2]:=hvxz+v.y;
  RawComponents[1,0]:=hvxy+v.z;
  RawComponents[1,1]:=e+(h*sqr(v.y));
  RawComponents[1,2]:=hvyz-v.x;
  RawComponents[2,0]:=hvxz-v.y;
  RawComponents[2,1]:=hvyz+v.x;
  RawComponents[2,2]:=e+(hvz*v.z);
 end;
end;

constructor TMatrix3x3.CreateConstruct(const pForwards,pUp:TVector3);
var RightVector,UpVector,ForwardVector:TVector3;
begin
 ForwardVector:=(-pForwards).Normalize;
 RightVector:=pUp.Cross(ForwardVector).Normalize;
 UpVector:=ForwardVector.Cross(RightVector).Normalize;
 RawComponents[0,0]:=RightVector.x;
 RawComponents[0,1]:=RightVector.y;
 RawComponents[0,2]:=RightVector.z;
 RawComponents[1,0]:=UpVector.x;
 RawComponents[1,1]:=UpVector.y;
 RawComponents[1,2]:=UpVector.z;
 RawComponents[2,0]:=ForwardVector.x;
 RawComponents[2,1]:=ForwardVector.y;
 RawComponents[2,2]:=ForwardVector.z;
end;

constructor TMatrix3x3.CreateOuterProduct(const u,v:TVector3);
begin
 RawComponents[0,0]:=u.x*v.x;
 RawComponents[0,1]:=u.x*v.y;
 RawComponents[0,2]:=u.x*v.z;
 RawComponents[1,0]:=u.y*v.x;
 RawComponents[1,1]:=u.y*v.y;
 RawComponents[1,2]:=u.y*v.z;
 RawComponents[2,0]:=u.z*v.x;
 RawComponents[2,1]:=u.z*v.y;
 RawComponents[2,2]:=u.z*v.z;
end;

constructor TMatrix3x3.CreateFromQuaternion(pQuaternion:TQuaternion);
var qx2,qy2,qz2,qxqx2,qxqy2,qxqz2,qxqw2,qyqy2,qyqz2,qyqw2,qzqz2,qzqw2:TScalar;
begin
 pQuaternion:=pQuaternion.Normalize;
 qx2:=pQuaternion.x+pQuaternion.x;
 qy2:=pQuaternion.y+pQuaternion.y;
 qz2:=pQuaternion.z+pQuaternion.z;
 qxqx2:=pQuaternion.x*qx2;
 qxqy2:=pQuaternion.x*qy2;
 qxqz2:=pQuaternion.x*qz2;
 qxqw2:=pQuaternion.w*qx2;
 qyqy2:=pQuaternion.y*qy2;
 qyqz2:=pQuaternion.y*qz2;
 qyqw2:=pQuaternion.w*qy2;
 qzqz2:=pQuaternion.z*qz2;
 qzqw2:=pQuaternion.w*qz2;
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

constructor TMatrix3x3.CreateFromQTangent(pQTangent:TQuaternion);
var qx2,qy2,qz2,qxqx2,qxqy2,qxqz2,qxqw2,qyqy2,qyqz2,qyqw2,qzqz2,qzqw2:TScalar;
begin
 pQTangent:=pQTangent.Normalize;
 qx2:=pQTangent.x+pQTangent.x;
 qy2:=pQTangent.y+pQTangent.y;
 qz2:=pQTangent.z+pQTangent.z;
 qxqx2:=pQTangent.x*qx2;
 qxqy2:=pQTangent.x*qy2;
 qxqz2:=pQTangent.x*qz2;
 qxqw2:=pQTangent.w*qx2;
 qyqy2:=pQTangent.y*qy2;
 qyqz2:=pQTangent.y*qz2;
 qyqw2:=pQTangent.w*qy2;
 qzqz2:=pQTangent.z*qz2;
 qzqw2:=pQTangent.w*qz2;
 RawComponents[0,0]:=1.0-(qyqy2+qzqz2);
 RawComponents[0,1]:=qxqy2+qzqw2;
 RawComponents[0,2]:=qxqz2-qyqw2;
 RawComponents[1,0]:=qxqy2-qzqw2;
 RawComponents[1,1]:=1.0-(qxqx2+qzqz2);
 RawComponents[1,2]:=qyqz2+qxqw2;
 RawComponents[2,0]:=(RawComponents[0,1]*RawComponents[1,2])-(RawComponents[0,2]*RawComponents[1,1]);
 RawComponents[2,1]:=(RawComponents[0,2]*RawComponents[1,0])-(RawComponents[0,0]*RawComponents[1,2]);
 RawComponents[2,2]:=(RawComponents[0,0]*RawComponents[1,1])-(RawComponents[0,1]*RawComponents[1,0]);
{RawComponents[2,0]:=qxqz2+qyqw2;
 RawComponents[2,1]:=qyqz2-qxqw2;
 RawComponents[2,2]:=1.0-(qxqx2+qyqy2);}
 if pQTangent.w<0.0 then begin
  RawComponents[2,0]:=-RawComponents[2,0];
  RawComponents[2,1]:=-RawComponents[2,1];
  RawComponents[2,2]:=-RawComponents[2,2];
 end;
end;

constructor TMatrix3x3.CreateRecomposed(const DecomposedMatrix3x3:TDecomposedMatrix3x3);
begin

 self:=TMatrix3x3.CreateFromQuaternion(DecomposedMatrix3x3.Rotation);

 if DecomposedMatrix3x3.Skew.z<>0.0 then begin // YZ
  self:=TMatrix3x3.Create(1.0,0.0,0.0,
                          0.0,1.0,0.0,
                          0.0,DecomposedMatrix3x3.Skew.z,1.0)*self;
 end;

 if DecomposedMatrix3x3.Skew.y<>0.0 then begin // XZ
  self:=TMatrix3x3.Create(1.0,0.0,0.0,
                          0.0,1.0,0.0,
                          DecomposedMatrix3x3.Skew.y,0.0,1.0)*self;
 end;

 if DecomposedMatrix3x3.Skew.x<>0.0 then begin // XY
  self:=TMatrix3x3.Create(1.0,0.0,0.0,
                          DecomposedMatrix3x3.Skew.x,1.0,0.0,
                          0.0,0.0,1.0)*self;
 end;

 self:=TMatrix3x3.CreateScale(DecomposedMatrix3x3.Scale)*self;

end;

class operator TMatrix3x3.Implicit(const a:TScalar):TMatrix3x3;
begin
 result.RawComponents[0,0]:=a;
 result.RawComponents[0,1]:=a;
 result.RawComponents[0,2]:=a;
 result.RawComponents[1,0]:=a;
 result.RawComponents[1,1]:=a;
 result.RawComponents[1,2]:=a;
 result.RawComponents[2,0]:=a;
 result.RawComponents[2,1]:=a;
 result.RawComponents[2,2]:=a;
end;

class operator TMatrix3x3.Explicit(const a:TScalar):TMatrix3x3;
begin
 result.RawComponents[0,0]:=a;
 result.RawComponents[0,1]:=a;
 result.RawComponents[0,2]:=a;
 result.RawComponents[1,0]:=a;
 result.RawComponents[1,1]:=a;
 result.RawComponents[1,2]:=a;
 result.RawComponents[2,0]:=a;
 result.RawComponents[2,1]:=a;
 result.RawComponents[2,2]:=a;
end;

class operator TMatrix3x3.Equal(const a,b:TMatrix3x3):boolean;
begin
 result:=SameValue(a.RawComponents[0,0],b.RawComponents[0,0]) and
         SameValue(a.RawComponents[0,1],b.RawComponents[0,1]) and
         SameValue(a.RawComponents[0,2],b.RawComponents[0,2]) and
         SameValue(a.RawComponents[1,0],b.RawComponents[1,0]) and
         SameValue(a.RawComponents[1,1],b.RawComponents[1,1]) and
         SameValue(a.RawComponents[1,2],b.RawComponents[1,2]) and
         SameValue(a.RawComponents[2,0],b.RawComponents[2,0]) and
         SameValue(a.RawComponents[2,1],b.RawComponents[2,1]) and
         SameValue(a.RawComponents[2,2],b.RawComponents[2,2]);
end;

class operator TMatrix3x3.NotEqual(const a,b:TMatrix3x3):boolean;
begin
 result:=(not SameValue(a.RawComponents[0,0],b.RawComponents[0,0])) or
         (not SameValue(a.RawComponents[0,1],b.RawComponents[0,1])) or
         (not SameValue(a.RawComponents[0,2],b.RawComponents[0,2])) or
         (not SameValue(a.RawComponents[1,0],b.RawComponents[1,0])) or
         (not SameValue(a.RawComponents[1,1],b.RawComponents[1,1])) or
         (not SameValue(a.RawComponents[1,2],b.RawComponents[1,2])) or
         (not SameValue(a.RawComponents[2,0],b.RawComponents[2,0])) or
         (not SameValue(a.RawComponents[2,1],b.RawComponents[2,1])) or
         (not SameValue(a.RawComponents[2,2],b.RawComponents[2,2]));
end;

class operator TMatrix3x3.Inc(const a:TMatrix3x3):TMatrix3x3;
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]+1.0;
 result.RawComponents[0,1]:=a.RawComponents[0,1]+1.0;
 result.RawComponents[0,2]:=a.RawComponents[0,2]+1.0;
 result.RawComponents[1,0]:=a.RawComponents[1,0]+1.0;
 result.RawComponents[1,1]:=a.RawComponents[1,1]+1.0;
 result.RawComponents[1,2]:=a.RawComponents[1,2]+1.0;
 result.RawComponents[2,0]:=a.RawComponents[2,0]+1.0;
 result.RawComponents[2,1]:=a.RawComponents[2,1]+1.0;
 result.RawComponents[2,2]:=a.RawComponents[2,2]+1.0;
end;

class operator TMatrix3x3.Dec(const a:TMatrix3x3):TMatrix3x3;
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]-1.0;
 result.RawComponents[0,1]:=a.RawComponents[0,1]-1.0;
 result.RawComponents[0,2]:=a.RawComponents[0,2]-1.0;
 result.RawComponents[1,0]:=a.RawComponents[1,0]-1.0;
 result.RawComponents[1,1]:=a.RawComponents[1,1]-1.0;
 result.RawComponents[1,2]:=a.RawComponents[1,2]-1.0;
 result.RawComponents[2,0]:=a.RawComponents[2,0]-1.0;
 result.RawComponents[2,1]:=a.RawComponents[2,1]-1.0;
 result.RawComponents[2,2]:=a.RawComponents[2,2]-1.0;
end;

class operator TMatrix3x3.Add(const a,b:TMatrix3x3):TMatrix3x3;
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]+b.RawComponents[0,0];
 result.RawComponents[0,1]:=a.RawComponents[0,1]+b.RawComponents[0,1];
 result.RawComponents[0,2]:=a.RawComponents[0,2]+b.RawComponents[0,2];
 result.RawComponents[1,0]:=a.RawComponents[1,0]+b.RawComponents[1,0];
 result.RawComponents[1,1]:=a.RawComponents[1,1]+b.RawComponents[1,1];
 result.RawComponents[1,2]:=a.RawComponents[1,2]+b.RawComponents[1,2];
 result.RawComponents[2,0]:=a.RawComponents[2,0]+b.RawComponents[2,0];
 result.RawComponents[2,1]:=a.RawComponents[2,1]+b.RawComponents[2,1];
 result.RawComponents[2,2]:=a.RawComponents[2,2]+b.RawComponents[2,2];
end;

class operator TMatrix3x3.Add(const a:TMatrix3x3;const b:TScalar):TMatrix3x3;
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]+b;
 result.RawComponents[0,1]:=a.RawComponents[0,1]+b;
 result.RawComponents[0,2]:=a.RawComponents[0,2]+b;
 result.RawComponents[1,0]:=a.RawComponents[1,0]+b;
 result.RawComponents[1,1]:=a.RawComponents[1,1]+b;
 result.RawComponents[1,2]:=a.RawComponents[1,2]+b;
 result.RawComponents[2,0]:=a.RawComponents[2,0]+b;
 result.RawComponents[2,1]:=a.RawComponents[2,1]+b;
 result.RawComponents[2,2]:=a.RawComponents[2,2]+b;
end;

class operator TMatrix3x3.Add(const a:TScalar;const b:TMatrix3x3):TMatrix3x3;
begin
 result.RawComponents[0,0]:=a+b.RawComponents[0,0];
 result.RawComponents[0,1]:=a+b.RawComponents[0,1];
 result.RawComponents[0,2]:=a+b.RawComponents[0,2];
 result.RawComponents[1,0]:=a+b.RawComponents[1,0];
 result.RawComponents[1,1]:=a+b.RawComponents[1,1];
 result.RawComponents[1,2]:=a+b.RawComponents[1,2];
 result.RawComponents[2,0]:=a+b.RawComponents[2,0];
 result.RawComponents[2,1]:=a+b.RawComponents[2,1];
 result.RawComponents[2,2]:=a+b.RawComponents[2,2];
end;

class operator TMatrix3x3.Subtract(const a,b:TMatrix3x3):TMatrix3x3;
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]-b.RawComponents[0,0];
 result.RawComponents[0,1]:=a.RawComponents[0,1]-b.RawComponents[0,1];
 result.RawComponents[0,2]:=a.RawComponents[0,2]-b.RawComponents[0,2];
 result.RawComponents[1,0]:=a.RawComponents[1,0]-b.RawComponents[1,0];
 result.RawComponents[1,1]:=a.RawComponents[1,1]-b.RawComponents[1,1];
 result.RawComponents[1,2]:=a.RawComponents[1,2]-b.RawComponents[1,2];
 result.RawComponents[2,0]:=a.RawComponents[2,0]-b.RawComponents[2,0];
 result.RawComponents[2,1]:=a.RawComponents[2,1]-b.RawComponents[2,1];
 result.RawComponents[2,2]:=a.RawComponents[2,2]-b.RawComponents[2,2];
end;

class operator TMatrix3x3.Subtract(const a:TMatrix3x3;const b:TScalar):TMatrix3x3;
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]-b;
 result.RawComponents[0,1]:=a.RawComponents[0,1]-b;
 result.RawComponents[0,2]:=a.RawComponents[0,2]-b;
 result.RawComponents[1,0]:=a.RawComponents[1,0]-b;
 result.RawComponents[1,1]:=a.RawComponents[1,1]-b;
 result.RawComponents[1,2]:=a.RawComponents[1,2]-b;
 result.RawComponents[2,0]:=a.RawComponents[2,0]-b;
 result.RawComponents[2,1]:=a.RawComponents[2,1]-b;
 result.RawComponents[2,2]:=a.RawComponents[2,2]-b;
end;

class operator TMatrix3x3.Subtract(const a:TScalar;const b:TMatrix3x3):TMatrix3x3;
begin
 result.RawComponents[0,0]:=a-b.RawComponents[0,0];
 result.RawComponents[0,1]:=a-b.RawComponents[0,1];
 result.RawComponents[0,2]:=a-b.RawComponents[0,2];
 result.RawComponents[1,0]:=a-b.RawComponents[1,0];
 result.RawComponents[1,1]:=a-b.RawComponents[1,1];
 result.RawComponents[1,2]:=a-b.RawComponents[1,2];
 result.RawComponents[2,0]:=a-b.RawComponents[2,0];
 result.RawComponents[2,1]:=a-b.RawComponents[2,1];
 result.RawComponents[2,2]:=a-b.RawComponents[2,2];
end;

class operator TMatrix3x3.Multiply(const a,b:TMatrix3x3):TMatrix3x3;
begin
 result.RawComponents[0,0]:=(a.RawComponents[0,0]*b.RawComponents[0,0])+(a.RawComponents[0,1]*b.RawComponents[1,0])+(a.RawComponents[0,2]*b.RawComponents[2,0]);
 result.RawComponents[0,1]:=(a.RawComponents[0,0]*b.RawComponents[0,1])+(a.RawComponents[0,1]*b.RawComponents[1,1])+(a.RawComponents[0,2]*b.RawComponents[2,1]);
 result.RawComponents[0,2]:=(a.RawComponents[0,0]*b.RawComponents[0,2])+(a.RawComponents[0,1]*b.RawComponents[1,2])+(a.RawComponents[0,2]*b.RawComponents[2,2]);
 result.RawComponents[1,0]:=(a.RawComponents[1,0]*b.RawComponents[0,0])+(a.RawComponents[1,1]*b.RawComponents[1,0])+(a.RawComponents[1,2]*b.RawComponents[2,0]);
 result.RawComponents[1,1]:=(a.RawComponents[1,0]*b.RawComponents[0,1])+(a.RawComponents[1,1]*b.RawComponents[1,1])+(a.RawComponents[1,2]*b.RawComponents[2,1]);
 result.RawComponents[1,2]:=(a.RawComponents[1,0]*b.RawComponents[0,2])+(a.RawComponents[1,1]*b.RawComponents[1,2])+(a.RawComponents[1,2]*b.RawComponents[2,2]);
 result.RawComponents[2,0]:=(a.RawComponents[2,0]*b.RawComponents[0,0])+(a.RawComponents[2,1]*b.RawComponents[1,0])+(a.RawComponents[2,2]*b.RawComponents[2,0]);
 result.RawComponents[2,1]:=(a.RawComponents[2,0]*b.RawComponents[0,1])+(a.RawComponents[2,1]*b.RawComponents[1,1])+(a.RawComponents[2,2]*b.RawComponents[2,1]);
 result.RawComponents[2,2]:=(a.RawComponents[2,0]*b.RawComponents[0,2])+(a.RawComponents[2,1]*b.RawComponents[1,2])+(a.RawComponents[2,2]*b.RawComponents[2,2]);
end;

class operator TMatrix3x3.Multiply(const a:TMatrix3x3;const b:TScalar):TMatrix3x3;
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]*b;
 result.RawComponents[0,1]:=a.RawComponents[0,1]*b;
 result.RawComponents[0,2]:=a.RawComponents[0,2]*b;
 result.RawComponents[1,0]:=a.RawComponents[1,0]*b;
 result.RawComponents[1,1]:=a.RawComponents[1,1]*b;
 result.RawComponents[1,2]:=a.RawComponents[1,2]*b;
 result.RawComponents[2,0]:=a.RawComponents[2,0]*b;
 result.RawComponents[2,1]:=a.RawComponents[2,1]*b;
 result.RawComponents[2,2]:=a.RawComponents[2,2]*b;
end;

class operator TMatrix3x3.Multiply(const a:TScalar;const b:TMatrix3x3):TMatrix3x3;
begin
 result.RawComponents[0,0]:=a*b.RawComponents[0,0];
 result.RawComponents[0,1]:=a*b.RawComponents[0,1];
 result.RawComponents[0,2]:=a*b.RawComponents[0,2];
 result.RawComponents[1,0]:=a*b.RawComponents[1,0];
 result.RawComponents[1,1]:=a*b.RawComponents[1,1];
 result.RawComponents[1,2]:=a*b.RawComponents[1,2];
 result.RawComponents[2,0]:=a*b.RawComponents[2,0];
 result.RawComponents[2,1]:=a*b.RawComponents[2,1];
 result.RawComponents[2,2]:=a*b.RawComponents[2,2];
end;

class operator TMatrix3x3.Multiply(const a:TMatrix3x3;const b:TVector3):TVector3;
begin
 result.x:=(a.RawComponents[0,0]*b.x)+(a.RawComponents[1,0]*b.y)+(a.RawComponents[2,0]*b.z);
 result.y:=(a.RawComponents[0,1]*b.x)+(a.RawComponents[1,1]*b.y)+(a.RawComponents[2,1]*b.z);
 result.z:=(a.RawComponents[0,2]*b.x)+(a.RawComponents[1,2]*b.y)+(a.RawComponents[2,2]*b.z);
end;

class operator TMatrix3x3.Multiply(const a:TVector3;const b:TMatrix3x3):TVector3;
begin
 result.x:=(a.x*b.RawComponents[0,0])+(a.y*b.RawComponents[0,1])+(a.z*b.RawComponents[0,2]);
 result.y:=(a.x*b.RawComponents[1,0])+(a.y*b.RawComponents[1,1])+(a.z*b.RawComponents[1,2]);
 result.z:=(a.x*b.RawComponents[2,0])+(a.y*b.RawComponents[2,1])+(a.z*b.RawComponents[2,2]);
end;

class operator TMatrix3x3.Multiply(const a:TMatrix3x3;const b:TVector4):TVector4;
begin
 result.x:=(a.RawComponents[0,0]*b.x)+(a.RawComponents[1,0]*b.y)+(a.RawComponents[2,0]*b.z);
 result.y:=(a.RawComponents[0,1]*b.x)+(a.RawComponents[1,1]*b.y)+(a.RawComponents[2,1]*b.z);
 result.z:=(a.RawComponents[0,2]*b.x)+(a.RawComponents[1,2]*b.y)+(a.RawComponents[2,2]*b.z);
 result.w:=b.w;
end;

class operator TMatrix3x3.Multiply(const a:TVector4;const b:TMatrix3x3):TVector4;
begin
 result.x:=(a.x*b.RawComponents[0,0])+(a.y*b.RawComponents[0,1])+(a.z*b.RawComponents[0,2]);
 result.y:=(a.x*b.RawComponents[1,0])+(a.y*b.RawComponents[1,1])+(a.z*b.RawComponents[1,2]);
 result.z:=(a.x*b.RawComponents[2,0])+(a.y*b.RawComponents[2,1])+(a.z*b.RawComponents[2,2]);
 result.w:=a.w;
end;

class operator TMatrix3x3.Multiply(const a:TMatrix3x3;const b:TPlane):TPlane;
begin
 result.Normal:=a.Inverse.Transpose*b.Normal;
 result.Distance:=result.Normal.Dot(a*((b.Normal*b.Distance)));
end;

class operator TMatrix3x3.Multiply(const a:TPlane;const b:TMatrix3x3):TPlane;
begin
 result:=b.Transpose*a;
end;

class operator TMatrix3x3.Divide(const a,b:TMatrix3x3):TMatrix3x3;
begin
 result:=a*b.Inverse;
end;

class operator TMatrix3x3.Divide(const a:TMatrix3x3;const b:TScalar):TMatrix3x3;
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]/b;
 result.RawComponents[0,1]:=a.RawComponents[0,1]/b;
 result.RawComponents[0,2]:=a.RawComponents[0,2]/b;
 result.RawComponents[1,0]:=a.RawComponents[1,0]/b;
 result.RawComponents[1,1]:=a.RawComponents[1,1]/b;
 result.RawComponents[1,2]:=a.RawComponents[1,2]/b;
 result.RawComponents[2,0]:=a.RawComponents[2,0]/b;
 result.RawComponents[2,1]:=a.RawComponents[2,1]/b;
 result.RawComponents[2,2]:=a.RawComponents[2,2]/b;
end;

class operator TMatrix3x3.Divide(const a:TScalar;const b:TMatrix3x3):TMatrix3x3;
begin
 result.RawComponents[0,0]:=a/b.RawComponents[0,0];
 result.RawComponents[0,1]:=a/b.RawComponents[0,1];
 result.RawComponents[0,2]:=a/b.RawComponents[0,2];
 result.RawComponents[1,0]:=a/b.RawComponents[1,0];
 result.RawComponents[1,1]:=a/b.RawComponents[1,1];
 result.RawComponents[1,2]:=a/b.RawComponents[1,2];
 result.RawComponents[2,0]:=a/b.RawComponents[2,0];
 result.RawComponents[2,1]:=a/b.RawComponents[2,1];
 result.RawComponents[2,2]:=a/b.RawComponents[2,2];
end;

class operator TMatrix3x3.IntDivide(const a,b:TMatrix3x3):TMatrix3x3;
begin
 result:=a*b.Inverse;
end;

class operator TMatrix3x3.IntDivide(const a:TMatrix3x3;const b:TScalar):TMatrix3x3;
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]/b;
 result.RawComponents[0,1]:=a.RawComponents[0,1]/b;
 result.RawComponents[0,2]:=a.RawComponents[0,2]/b;
 result.RawComponents[1,0]:=a.RawComponents[1,0]/b;
 result.RawComponents[1,1]:=a.RawComponents[1,1]/b;
 result.RawComponents[1,2]:=a.RawComponents[1,2]/b;
 result.RawComponents[2,0]:=a.RawComponents[2,0]/b;
 result.RawComponents[2,1]:=a.RawComponents[2,1]/b;
 result.RawComponents[2,2]:=a.RawComponents[2,2]/b;
end;

class operator TMatrix3x3.IntDivide(const a:TScalar;const b:TMatrix3x3):TMatrix3x3;
begin
 result.RawComponents[0,0]:=a/b.RawComponents[0,0];
 result.RawComponents[0,1]:=a/b.RawComponents[0,1];
 result.RawComponents[0,2]:=a/b.RawComponents[0,2];
 result.RawComponents[1,0]:=a/b.RawComponents[1,0];
 result.RawComponents[1,1]:=a/b.RawComponents[1,1];
 result.RawComponents[1,2]:=a/b.RawComponents[1,2];
 result.RawComponents[2,0]:=a/b.RawComponents[2,0];
 result.RawComponents[2,1]:=a/b.RawComponents[2,1];
 result.RawComponents[2,2]:=a/b.RawComponents[2,2];
end;

class operator TMatrix3x3.Modulus(const a,b:TMatrix3x3):TMatrix3x3;
begin
 result.RawComponents[0,0]:=Modulo(a.RawComponents[0,0],b.RawComponents[0,0]);
 result.RawComponents[0,1]:=Modulo(a.RawComponents[0,1],b.RawComponents[0,1]);
 result.RawComponents[0,2]:=Modulo(a.RawComponents[0,2],b.RawComponents[0,2]);
 result.RawComponents[1,0]:=Modulo(a.RawComponents[1,0],b.RawComponents[1,0]);
 result.RawComponents[1,1]:=Modulo(a.RawComponents[1,1],b.RawComponents[1,1]);
 result.RawComponents[1,2]:=Modulo(a.RawComponents[1,2],b.RawComponents[1,2]);
 result.RawComponents[2,0]:=Modulo(a.RawComponents[2,0],b.RawComponents[2,0]);
 result.RawComponents[2,1]:=Modulo(a.RawComponents[2,1],b.RawComponents[2,1]);
 result.RawComponents[2,2]:=Modulo(a.RawComponents[2,2],b.RawComponents[2,2]);
end;

class operator TMatrix3x3.Modulus(const a:TMatrix3x3;const b:TScalar):TMatrix3x3;
begin
 result.RawComponents[0,0]:=Modulo(a.RawComponents[0,0],b);
 result.RawComponents[0,1]:=Modulo(a.RawComponents[0,1],b);
 result.RawComponents[0,2]:=Modulo(a.RawComponents[0,2],b);
 result.RawComponents[1,0]:=Modulo(a.RawComponents[1,0],b);
 result.RawComponents[1,1]:=Modulo(a.RawComponents[1,1],b);
 result.RawComponents[1,2]:=Modulo(a.RawComponents[1,2],b);
 result.RawComponents[2,0]:=Modulo(a.RawComponents[2,0],b);
 result.RawComponents[2,1]:=Modulo(a.RawComponents[2,1],b);
 result.RawComponents[2,2]:=Modulo(a.RawComponents[2,2],b);
end;

class operator TMatrix3x3.Modulus(const a:TScalar;const b:TMatrix3x3):TMatrix3x3;
begin
 result.RawComponents[0,0]:=Modulo(a,b.RawComponents[0,0]);
 result.RawComponents[0,1]:=Modulo(a,b.RawComponents[0,1]);
 result.RawComponents[0,2]:=Modulo(a,b.RawComponents[0,2]);
 result.RawComponents[1,0]:=Modulo(a,b.RawComponents[1,0]);
 result.RawComponents[1,1]:=Modulo(a,b.RawComponents[1,1]);
 result.RawComponents[1,2]:=Modulo(a,b.RawComponents[1,2]);
 result.RawComponents[2,0]:=Modulo(a,b.RawComponents[2,0]);
 result.RawComponents[2,1]:=Modulo(a,b.RawComponents[2,1]);
 result.RawComponents[2,2]:=Modulo(a,b.RawComponents[2,2]);
end;

class operator TMatrix3x3.Negative(const a:TMatrix3x3):TMatrix3x3;
begin
 result.RawComponents[0,0]:=-a.RawComponents[0,0];
 result.RawComponents[0,1]:=-a.RawComponents[0,1];
 result.RawComponents[0,2]:=-a.RawComponents[0,2];
 result.RawComponents[1,0]:=-a.RawComponents[1,0];
 result.RawComponents[1,1]:=-a.RawComponents[1,1];
 result.RawComponents[1,2]:=-a.RawComponents[1,2];
 result.RawComponents[2,0]:=-a.RawComponents[2,0];
 result.RawComponents[2,1]:=-a.RawComponents[2,1];
 result.RawComponents[2,2]:=-a.RawComponents[2,2];
end;

class operator TMatrix3x3.Positive(const a:TMatrix3x3):TMatrix3x3;
begin
 result:=a;
end;

function TMatrix3x3.GetComponent(const pIndexA,pIndexB:TInt32):TScalar;
begin
 result:=RawComponents[pIndexA,pIndexB];
end;

procedure TMatrix3x3.SetComponent(const pIndexA,pIndexB:TInt32;const pValue:TScalar);
begin
 RawComponents[pIndexA,pIndexB]:=pValue;
end;

function TMatrix3x3.GetColumn(const pIndex:TInt32):TVector3;
begin
 result.x:=RawComponents[pIndex,0];
 result.y:=RawComponents[pIndex,1];
 result.z:=RawComponents[pIndex,2];
end;

procedure TMatrix3x3.SetColumn(const pIndex:TInt32;const pValue:TVector3);
begin
 RawComponents[pIndex,0]:=pValue.x;
 RawComponents[pIndex,1]:=pValue.y;
 RawComponents[pIndex,2]:=pValue.z;
end;

function TMatrix3x3.GetRow(const pIndex:TInt32):TVector3;
begin
 result.x:=RawComponents[0,pIndex];
 result.y:=RawComponents[1,pIndex];
 result.z:=RawComponents[2,pIndex];
end;

procedure TMatrix3x3.SetRow(const pIndex:TInt32;const pValue:TVector3);
begin
 RawComponents[0,pIndex]:=pValue.x;
 RawComponents[1,pIndex]:=pValue.y;
 RawComponents[2,pIndex]:=pValue.z;
end;

function TMatrix3x3.Determinant:TScalar;
begin
 result:=((RawComponents[0,0]*((RawComponents[1,1]*RawComponents[2,2])-(RawComponents[1,2]*RawComponents[2,1])))-
          (RawComponents[0,1]*((RawComponents[1,0]*RawComponents[2,2])-(RawComponents[1,2]*RawComponents[2,0]))))+
          (RawComponents[0,2]*((RawComponents[1,0]*RawComponents[2,1])-(RawComponents[1,1]*RawComponents[2,0])));
end;

function TMatrix3x3.Inverse:TMatrix3x3;
var d:TScalar;
begin
 d:=((RawComponents[0,0]*((RawComponents[1,1]*RawComponents[2,2])-(RawComponents[1,2]*RawComponents[2,1])))-
     (RawComponents[0,1]*((RawComponents[1,0]*RawComponents[2,2])-(RawComponents[1,2]*RawComponents[2,0]))))+
     (RawComponents[0,2]*((RawComponents[1,0]*RawComponents[2,1])-(RawComponents[1,1]*RawComponents[2,0])));
 if d<>0.0 then begin
  d:=1.0/d;
  result.RawComponents[0,0]:=((RawComponents[1,1]*RawComponents[2,2])-(RawComponents[1,2]*RawComponents[2,1]))*d;
  result.RawComponents[0,1]:=((RawComponents[0,2]*RawComponents[2,1])-(RawComponents[0,1]*RawComponents[2,2]))*d;
  result.RawComponents[0,2]:=((RawComponents[0,1]*RawComponents[1,2])-(RawComponents[0,2]*RawComponents[1,1]))*d;
  result.RawComponents[1,0]:=((RawComponents[1,2]*RawComponents[2,0])-(RawComponents[1,0]*RawComponents[2,2]))*d;
  result.RawComponents[1,1]:=((RawComponents[0,0]*RawComponents[2,2])-(RawComponents[0,2]*RawComponents[2,0]))*d;
  result.RawComponents[1,2]:=((RawComponents[0,2]*RawComponents[1,0])-(RawComponents[0,0]*RawComponents[1,2]))*d;
  result.RawComponents[2,0]:=((RawComponents[1,0]*RawComponents[2,1])-(RawComponents[1,1]*RawComponents[2,0]))*d;
  result.RawComponents[2,1]:=((RawComponents[0,1]*RawComponents[2,0])-(RawComponents[0,0]*RawComponents[2,1]))*d;
  result.RawComponents[2,2]:=((RawComponents[0,0]*RawComponents[1,1])-(RawComponents[0,1]*RawComponents[1,0]))*d;
 end else begin
  result:=Matrix3x3Identity;
 end;
end;

function TMatrix3x3.Transpose:TMatrix3x3;
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

function TMatrix3x3.EulerAngles:TVector3;
var v0,v1:TVector3;
begin
 if abs((-1.0)-RawComponents[0,2])<EPSILON then begin
  result.x:=0.0;
  result.y:=pi*0.5;
  result.z:=ArcTan2(RawComponents[1,0],RawComponents[2,0]);
 end else if abs(1.0-RawComponents[0,2])<EPSILON then begin
  result.x:=0.0;
  result.y:=-(pi*0.5);
  result.z:=ArcTan2(-RawComponents[1,0],-RawComponents[2,0]);
 end else begin
  v0.x:=-ArcSin(RawComponents[0,2]);
  v1.x:=pi-v0.x;
  v0.y:=ArcTan2(RawComponents[1,2]/cos(v0.x),RawComponents[2,2]/cos(v0.x));
  v1.y:=ArcTan2(RawComponents[1,2]/cos(v1.x),RawComponents[2,2]/cos(v1.x));
  v0.z:=ArcTan2(RawComponents[0,1]/cos(v0.x),RawComponents[0,0]/cos(v0.x));
  v1.z:=ArcTan2(RawComponents[0,1]/cos(v1.x),RawComponents[0,0]/cos(v1.x));
  if v0.SquaredLength<v1.SquaredLength then begin
   result:=v0;
  end else begin
   result:=v1;
  end;
 end;
end;

function TMatrix3x3.Normalize:TMatrix3x3;
begin
 result.Right:=Right.Normalize;
 result.Up:=Up.Normalize;
 result.Forwards:=Forwards.Normalize;
end;

function TMatrix3x3.OrthoNormalize:TMatrix3x3;
begin
 result.Normal:=Normal.Normalize;
 result.Tangent:=(Tangent-(result.Normal*Tangent.Dot(result.Normal))).Normalize;
 result.Bitangent:=result.Normal.Cross(result.Tangent).Normalize;
 result.Bitangent:=result.Bitangent-(result.Normal*result.Bitangent.Dot(result.Normal));
 result.Bitangent:=(result.Bitangent-(result.Tangent*result.Bitangent.Dot(result.Tangent))).Normalize;
 result.Tangent:=result.Bitangent.Cross(result.Normal).Normalize;
 result.Normal:=result.Tangent.Cross(result.Bitangent).Normalize;
end;

function TMatrix3x3.RobustOrthoNormalize(const Tolerance:TScalar=1e-3):TMatrix3x3;
var Bisector,Axis:TVector3;
begin
 begin
  if Normal.Length<Tolerance then begin
   // Degenerate case, compute new normal
   Normal:=Tangent.Cross(Bitangent);
   if Normal.Length<Tolerance then begin
    result.Tangent:=Vector3XAxis;
    result.Bitangent:=Vector3YAxis;
    result.Normal:=Vector3ZAxis;
    exit;
   end;
  end;
  result.Normal:=Normal.Normalize;
 end;
 begin
  // Project tangent and bitangent onto the normal orthogonal plane
  result.Tangent:=Tangent-(result.Normal*Tangent.Dot(result.Normal));
  result.Bitangent:=Bitangent-(result.Normal*Bitangent.Dot(result.Normal));
 end;
 begin
  // Check for several degenerate cases
  if result.Tangent.Length<Tolerance then begin
   if result.Bitangent.Length<Tolerance then begin
    result.Tangent:=result.Normal.Normalize;
    if (result.Tangent.x<=result.Tangent.y) and (result.Tangent.x<=result.Tangent.z) then begin
     result.Tangent:=Vector3XAxis;
    end else if (result.Tangent.y<=result.Tangent.x) and (result.Tangent.y<=result.Tangent.z) then begin
     result.Tangent:=Vector3YAxis;
    end else begin
     result.Tangent:=Vector3ZAxis;
    end;
    result.Tangent:=result.Tangent-(result.Normal*result.Tangent.Dot(result.Normal));
    result.Bitangent:=result.Normal.Cross(result.Tangent).Normalize;
   end else begin
    result.Tangent:=result.Bitangent.Cross(result.Normal).Normalize;
   end;
  end else begin
   result.Tangent:=result.Tangent.Normalize;
   if result.Bitangent.Length<Tolerance then begin
    result.Bitangent:=result.Normal.Cross(result.Tangent).Normalize;
   end else begin
    result.Bitangent:=result.Bitangent.Normalize;
    Bisector:=result.Tangent+result.Bitangent;
    if Bisector.Length<Tolerance then begin
     Bisector:=result.Tangent;
    end else begin
     Bisector:=Bisector.Normalize;
    end;
    Axis:=Bisector.Cross(result.Normal).Normalize;
    if Axis.Dot(Tangent)>0.0 then begin
     result.Tangent:=(Bisector+Axis).Normalize;
     result.Bitangent:=(Bisector-Axis).Normalize;
    end else begin
     result.Tangent:=(Bisector-Axis).Normalize;
     result.Bitangent:=(Bisector+Axis).Normalize;
    end;
   end;
  end;
 end;
 result.Bitangent:=result.Normal.Cross(result.Tangent).Normalize;
 result.Tangent:=result.Bitangent.Cross(result.Normal).Normalize;
 result.Normal:=result.Tangent.Cross(result.Bitangent).Normalize;
end;

function TMatrix3x3.ToQuaternion:TQuaternion;
var t,s:TScalar;
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
 result:=result.Normalize;
end;

function TMatrix3x3.ToQTangent:TQuaternion;
const Threshold=1.0/32767.0;
var Scale,t,s,Renormalization:TScalar;
begin
 if ((((((RawComponents[0,0]*RawComponents[1,1]*RawComponents[2,2])+
         (RawComponents[0,1]*RawComponents[1,2]*RawComponents[2,0])
        )+
        (RawComponents[0,2]*RawComponents[1,0]*RawComponents[2,1])
       )-
       (RawComponents[0,2]*RawComponents[1,1]*RawComponents[2,0])
      )-
      (RawComponents[0,1]*RawComponents[1,0]*RawComponents[2,2])
     )-
     (RawComponents[0,0]*RawComponents[1,2]*RawComponents[2,1])
    )<0.0 then begin
  // Reflection matrix, so flip y axis in case the tangent frame encodes a reflection
  Scale:=-1.0;
  RawComponents[2,0]:=-RawComponents[2,0];
  RawComponents[2,1]:=-RawComponents[2,1];
  RawComponents[2,2]:=-RawComponents[2,2];
 end else begin
  // Rotation matrix, so nothing is doing to do
  Scale:=1.0;
 end;
 begin
  // Convert to quaternion
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
  result:=result.Normalize;
 end;
 begin
  // Make sure, that we don't end up with 0 as w component
  if abs(result.w)<=Threshold then begin
   Renormalization:=sqrt(1.0-sqr(Threshold));
   result.x:=result.x*Renormalization;
   result.y:=result.y*Renormalization;
   result.z:=result.z*Renormalization;
   if result.w>0.0 then begin
    result.w:=Threshold;
   end else begin
    result.w:=-Threshold;
   end;
  end;
 end;
 if ((Scale<0.0) and (result.w>=0.0)) or ((Scale>=0.0) and (result.w<0.0)) then begin
  // Encode reflection into quaternion's w element by making sign of w negative,
  // if y axis needs to be flipped, otherwise it stays positive
  result.x:=-result.x;
  result.y:=-result.y;
  result.z:=-result.z;
  result.w:=-result.w;
 end;
end;

function TMatrix3x3.SimpleLerp(const b:TMatrix3x3;const t:TScalar):TMatrix3x3;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  result:=(self*(1.0-t))+(b*t);
 end;
end;

function TMatrix3x3.SimpleNlerp(const b:TMatrix3x3;const t:TScalar):TMatrix3x3;
var Scale:TVector3;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  Scale:=TVector3.Create(Right.Length,
                         Up.Length,
                         Forwards.Length).Lerp(TVector3.Create(b.Right.Length,
                                                               b.Up.Length,
                                                               b.Forwards.Length),
                                               t);
  result:=TMatrix3x3.CreateFromQuaternion(Normalize.ToQuaternion.Nlerp(b.Normalize.ToQuaternion,t));
  result.Right:=result.Right*Scale.x;
  result.Up:=result.Up*Scale.y;
  result.Forwards:=result.Forwards*Scale.z;
 end;
end;

function TMatrix3x3.SimpleSlerp(const b:TMatrix3x3;const t:TScalar):TMatrix3x3;
var Scale:TVector3;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  Scale:=TVector3.Create(Right.Length,
                         Up.Length,
                         Forwards.Length).Lerp(TVector3.Create(b.Right.Length,
                                                               b.Up.Length,
                                                               b.Forwards.Length),
                                               t);
  result:=TMatrix3x3.CreateFromQuaternion(Normalize.ToQuaternion.Slerp(b.Normalize.ToQuaternion,t));
  result.Right:=result.Right*Scale.x;
  result.Up:=result.Up*Scale.y;
  result.Forwards:=result.Forwards*Scale.z;
 end;
end;

function TMatrix3x3.Lerp(const b:TMatrix3x3;const t:TScalar):TMatrix3x3;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  result:=TMatrix3x3.CreateRecomposed(Decompose.Lerp(b.Decompose,t));
 end;
end;

function TMatrix3x3.Nlerp(const b:TMatrix3x3;const t:TScalar):TMatrix3x3;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  result:=TMatrix3x3.CreateRecomposed(Decompose.Nlerp(b.Decompose,t));
 end;
end;

function TMatrix3x3.Slerp(const b:TMatrix3x3;const t:TScalar):TMatrix3x3;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  result:=TMatrix3x3.CreateRecomposed(Decompose.Slerp(b.Decompose,t));
 end;
end;

function TMatrix3x3.MulInverse(const a:TVector3):TVector3;
var d:TScalar;
begin
 d:=((RawComponents[0,0]*((RawComponents[1,1]*RawComponents[2,2])-(RawComponents[2,1]*RawComponents[1,2])))-
     (RawComponents[0,1]*((RawComponents[1,0]*RawComponents[2,2])-(RawComponents[2,0]*RawComponents[1,2]))))+
     (RawComponents[0,2]*((RawComponents[1,0]*RawComponents[2,1])-(RawComponents[2,0]*RawComponents[1,1])));
 if d<>0.0 then begin
  d:=1.0/d;
 end;
 result.x:=((a.x*((RawComponents[1,1]*RawComponents[2,2])-(RawComponents[1,2]*RawComponents[2,1])))+(a.y*((RawComponents[1,2]*RawComponents[2,0])-(RawComponents[1,0]*RawComponents[2,2])))+(a.z*((RawComponents[1,0]*RawComponents[2,1])-(RawComponents[1,1]*RawComponents[2,0]))))*d;
 result.y:=((RawComponents[0,0]*((a.y*RawComponents[2,2])-(a.z*RawComponents[2,1])))+(RawComponents[0,1]*((a.z*RawComponents[2,0])-(a.x*RawComponents[2,2])))+(RawComponents[0,2]*((a.x*RawComponents[2,1])-(a.y*RawComponents[2,0]))))*d;
 result.z:=((RawComponents[0,0]*((RawComponents[1,1]*a.z)-(RawComponents[1,2]*a.y)))+(RawComponents[0,1]*((RawComponents[1,2]*a.x)-(RawComponents[1,0]*a.z)))+(RawComponents[0,2]*((RawComponents[1,0]*a.y)-(RawComponents[1,1]*a.x))))*d;
end;

function TMatrix3x3.MulInverse(const a:TVector4):TVector4;
var d:TScalar;
begin
 d:=((RawComponents[0,0]*((RawComponents[1,1]*RawComponents[2,2])-(RawComponents[2,1]*RawComponents[1,2])))-
     (RawComponents[0,1]*((RawComponents[1,0]*RawComponents[2,2])-(RawComponents[2,0]*RawComponents[1,2]))))+
     (RawComponents[0,2]*((RawComponents[1,0]*RawComponents[2,1])-(RawComponents[2,0]*RawComponents[1,1])));
 if d<>0.0 then begin
  d:=1.0/d;
 end;
 result.x:=((a.x*((RawComponents[1,1]*RawComponents[2,2])-(RawComponents[1,2]*RawComponents[2,1])))+(a.y*((RawComponents[1,2]*RawComponents[2,0])-(RawComponents[1,0]*RawComponents[2,2])))+(a.z*((RawComponents[1,0]*RawComponents[2,1])-(RawComponents[1,1]*RawComponents[2,0]))))*d;
 result.y:=((RawComponents[0,0]*((a.y*RawComponents[2,2])-(a.z*RawComponents[2,1])))+(RawComponents[0,1]*((a.z*RawComponents[2,0])-(a.x*RawComponents[2,2])))+(RawComponents[0,2]*((a.x*RawComponents[2,1])-(a.y*RawComponents[2,0]))))*d;
 result.z:=((RawComponents[0,0]*((RawComponents[1,1]*a.z)-(RawComponents[1,2]*a.y)))+(RawComponents[0,1]*((RawComponents[1,2]*a.x)-(RawComponents[1,0]*a.z)))+(RawComponents[0,2]*((RawComponents[1,0]*a.y)-(RawComponents[1,1]*a.x))))*d;
 result.w:=a.w;
end;

function TMatrix3x3.Decompose:TDecomposedMatrix3x3;
var LocalMatrix:TMatrix3x3;
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

  result.Scale:=TVector3.Create(1.0,1.0,1.0);
  result.Skew:=TVector3.Create(0.0,0.0,0.0);
  result.Rotation:=TQuaternion.Create(0.0,0.0,0.0,1.0);

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

  result.Skew.yz:=result.Skew.yz/result.Scale.z;

  if LocalMatrix.Right.Dot(LocalMatrix.Up.Cross(LocalMatrix.Forwards))<0.0 then begin
   result.Scale.x:=-result.Scale.x;
   LocalMatrix:=-LocalMatrix;
  end;

  result.Rotation:=LocalMatrix.ToQuaternion;

  result.Valid:=true;

 end;

end;

function TDecomposedMatrix4x4.Lerp(const b:TDecomposedMatrix4x4;const t:TScalar):TDecomposedMatrix4x4;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  result.Perspective:=Perspective.Lerp(b.Perspective,t);
  result.Translation:=Translation.Lerp(b.Translation,t);
  result.Scale:=Scale.Lerp(b.Scale,t);
  result.Skew:=Skew.Lerp(b.Skew,t);
  result.Rotation:=Rotation.Lerp(b.Rotation,t);
 end;
end;

function TDecomposedMatrix4x4.Nlerp(const b:TDecomposedMatrix4x4;const t:TScalar):TDecomposedMatrix4x4;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  result.Perspective:=Perspective.Lerp(b.Perspective,t);
  result.Translation:=Translation.Lerp(b.Translation,t);
  result.Scale:=Scale.Lerp(b.Scale,t);
  result.Skew:=Skew.Lerp(b.Skew,t);
  result.Rotation:=Rotation.Nlerp(b.Rotation,t);
 end;
end;

function TDecomposedMatrix4x4.Slerp(const b:TDecomposedMatrix4x4;const t:TScalar):TDecomposedMatrix4x4;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  result.Perspective:=Perspective.Lerp(b.Perspective,t);
  result.Translation:=Translation.Lerp(b.Translation,t);
  result.Scale:=Scale.Lerp(b.Scale,t);
  result.Skew:=Skew.Lerp(b.Skew,t);
  result.Rotation:=Rotation.Slerp(b.Rotation,t);
 end;
end;

{constructor TMatrix4x4.Create;
begin
 RawComponents[0,0]:=1.0;
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=0.0;
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=1.0;
 RawComponents[1,2]:=0.0;
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=1.0;
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=0.0;
 RawComponents[3,1]:=0.0;
 RawComponents[3,2]:=0.0;
 RawComponents[3,3]:=1.0;
end;{}

constructor TMatrix4x4.Create(const pX:TScalar);
begin
 RawComponents[0,0]:=pX;
 RawComponents[0,1]:=pX;
 RawComponents[0,2]:=pX;
 RawComponents[0,3]:=pX;
 RawComponents[1,0]:=pX;
 RawComponents[1,1]:=pX;
 RawComponents[1,2]:=pX;
 RawComponents[1,3]:=pX;
 RawComponents[2,0]:=pX;
 RawComponents[2,1]:=pX;
 RawComponents[2,2]:=pX;
 RawComponents[2,3]:=pX;
 RawComponents[3,0]:=pX;
 RawComponents[3,1]:=pX;
 RawComponents[3,2]:=pX;
 RawComponents[3,3]:=pX;
end;

constructor TMatrix4x4.Create(const pXX,pXY,pXZ,pXW,pYX,pYY,pYZ,pYW,pZX,pZY,pZZ,pZW,pWX,pWY,pWZ,pWW:TScalar);
begin
 RawComponents[0,0]:=pXX;
 RawComponents[0,1]:=pXY;
 RawComponents[0,2]:=pXZ;
 RawComponents[0,3]:=pXW;
 RawComponents[1,0]:=pYX;
 RawComponents[1,1]:=pYY;
 RawComponents[1,2]:=pYZ;
 RawComponents[1,3]:=pYW;
 RawComponents[2,0]:=pZX;
 RawComponents[2,1]:=pZY;
 RawComponents[2,2]:=pZZ;
 RawComponents[2,3]:=pZW;
 RawComponents[3,0]:=pWX;
 RawComponents[3,1]:=pWY;
 RawComponents[3,2]:=pWZ;
 RawComponents[3,3]:=pWW;
end;

constructor TMatrix4x4.Create(const pX,pY,pZ,pW:TVector4);
begin
 RawComponents[0,0]:=pX.x;
 RawComponents[0,1]:=pX.y;
 RawComponents[0,2]:=pX.z;
 RawComponents[0,3]:=pX.w;
 RawComponents[1,0]:=pY.x;
 RawComponents[1,1]:=pY.y;
 RawComponents[1,2]:=pY.z;
 RawComponents[1,3]:=pY.w;
 RawComponents[2,0]:=pZ.x;
 RawComponents[2,1]:=pZ.y;
 RawComponents[2,2]:=pZ.z;
 RawComponents[2,3]:=pZ.w;
 RawComponents[3,0]:=pW.x;
 RawComponents[3,1]:=pW.y;
 RawComponents[3,2]:=pW.z;
 RawComponents[3,3]:=pW.w;
end;


constructor TMatrix4x4.Create(const pMatrix:TMatrix3x3);
begin
 RawComponents[0,0]:=pMatrix.RawComponents[0,0];
 RawComponents[0,1]:=pMatrix.RawComponents[0,1];
 RawComponents[0,2]:=pMatrix.RawComponents[0,2];
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=pMatrix.RawComponents[1,0];
 RawComponents[1,1]:=pMatrix.RawComponents[1,1];
 RawComponents[1,2]:=pMatrix.RawComponents[1,2];
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=pMatrix.RawComponents[2,0];
 RawComponents[2,1]:=pMatrix.RawComponents[2,1];
 RawComponents[2,2]:=pMatrix.RawComponents[2,2];
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=0.0;
 RawComponents[3,1]:=0.0;
 RawComponents[3,2]:=0.0;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateRotateX(const Angle:TScalar);
begin
 RawComponents[0,0]:=1.0;
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=0.0;
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=0.0;
 SinCos(Angle,RawComponents[1,2],RawComponents[1,1]);
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=-RawComponents[1,2];
 RawComponents[2,2]:=RawComponents[1,1];
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=0.0;
 RawComponents[3,1]:=0.0;
 RawComponents[3,2]:=0.0;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateRotateY(const Angle:TScalar);
begin
 SinCos(Angle,RawComponents[2,0],RawComponents[0,0]);
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=-RawComponents[2,0];
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=1.0;
 RawComponents[1,2]:=0.0;
 RawComponents[1,3]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=RawComponents[0,0];
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=0.0;
 RawComponents[3,1]:=0.0;
 RawComponents[3,2]:=0.0;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateRotateZ(const Angle:TScalar);
begin
 SinCos(Angle,RawComponents[0,1],RawComponents[0,0]);
 RawComponents[0,2]:=0.0;
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=-RawComponents[0,1];
 RawComponents[1,1]:=RawComponents[0,0];
 RawComponents[1,2]:=0.0;
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=1.0;
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=0.0;
 RawComponents[3,1]:=0.0;
 RawComponents[3,2]:=0.0;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateRotate(const Angle:TScalar;const Axis:TVector3);
var SinusAngle,CosinusAngle:TScalar;
begin
 SinCos(Angle,SinusAngle,CosinusAngle);
 RawComponents[0,0]:=CosinusAngle+((1.0-CosinusAngle)*sqr(Axis.x));
 RawComponents[1,0]:=((1.0-CosinusAngle)*Axis.x*Axis.y)-(Axis.z*SinusAngle);
 RawComponents[2,0]:=((1.0-CosinusAngle)*Axis.x*Axis.z)+(Axis.y*SinusAngle);
 RawComponents[0,3]:=0.0;
 RawComponents[0,1]:=((1.0-CosinusAngle)*Axis.x*Axis.z)+(Axis.z*SinusAngle);
 RawComponents[1,1]:=CosinusAngle+((1.0-CosinusAngle)*sqr(Axis.y));
 RawComponents[2,1]:=((1.0-CosinusAngle)*Axis.y*Axis.z)-(Axis.x*SinusAngle);
 RawComponents[1,3]:=0.0;
 RawComponents[0,2]:=((1.0-CosinusAngle)*Axis.x*Axis.z)-(Axis.y*SinusAngle);
 RawComponents[1,2]:=((1.0-CosinusAngle)*Axis.y*Axis.z)+(Axis.x*SinusAngle);
 RawComponents[2,2]:=CosinusAngle+((1.0-CosinusAngle)*sqr(Axis.z));
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=0.0;
 RawComponents[3,1]:=0.0;
 RawComponents[3,2]:=0.0;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateRotation(const pMatrix:TMatrix4x4);
begin
 RawComponents[0,0]:=pMatrix.RawComponents[0,0];
 RawComponents[0,1]:=pMatrix.RawComponents[0,1];
 RawComponents[0,2]:=pMatrix.RawComponents[0,2];
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=pMatrix.RawComponents[1,0];
 RawComponents[1,1]:=pMatrix.RawComponents[1,1];
 RawComponents[1,2]:=pMatrix.RawComponents[1,2];
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=pMatrix.RawComponents[2,0];
 RawComponents[2,1]:=pMatrix.RawComponents[2,1];
 RawComponents[2,2]:=pMatrix.RawComponents[2,2];
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=0.0;
 RawComponents[3,1]:=0.0;
 RawComponents[3,2]:=0.0;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateScale(const sx,sy,sz:TScalar);
begin
 RawComponents[0,0]:=sx;
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=0.0;
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=sy;
 RawComponents[1,2]:=0.0;
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=sz;
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=0.0;
 RawComponents[3,1]:=0.0;
 RawComponents[3,2]:=0.0;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateScale(const pScale:TVector3);
begin
 RawComponents[0,0]:=pScale.x;
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=0.0;
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=pScale.y;
 RawComponents[1,2]:=0.0;
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=pScale.z;
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=0.0;
 RawComponents[3,1]:=0.0;
 RawComponents[3,2]:=0.0;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateScale(const sx,sy,sz,sw:TScalar);
begin
 RawComponents[0,0]:=sx;
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=0.0;
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=sy;
 RawComponents[1,2]:=0.0;
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=sz;
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=0.0;
 RawComponents[3,1]:=0.0;
 RawComponents[3,2]:=0.0;
 RawComponents[3,3]:=sw;
end;

constructor TMatrix4x4.CreateScale(const pScale:TVector4);
begin
 RawComponents[0,0]:=pScale.x;
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=0.0;
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=pScale.y;
 RawComponents[1,2]:=0.0;
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=pScale.z;
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=0.0;
 RawComponents[3,1]:=0.0;
 RawComponents[3,2]:=0.0;
 RawComponents[3,3]:=pScale.w;
end;

constructor TMatrix4x4.CreateTranslation(const tx,ty,tz:TScalar);
begin
 RawComponents[0,0]:=1.0;
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=0.0;
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=1.0;
 RawComponents[1,2]:=0.0;
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=1.0;
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=tx;
 RawComponents[3,1]:=ty;
 RawComponents[3,2]:=tz;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateTranslation(const pTranslation:TVector3);
begin
 RawComponents[0,0]:=1.0;
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=0.0;
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=1.0;
 RawComponents[1,2]:=0.0;
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=1.0;
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=pTranslation.x;
 RawComponents[3,1]:=pTranslation.y;
 RawComponents[3,2]:=pTranslation.z;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateTranslation(const tx,ty,tz,tw:TScalar);
begin
 RawComponents[0,0]:=1.0;
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=0.0;
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=1.0;
 RawComponents[1,2]:=0.0;
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=1.0;
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=tx;
 RawComponents[3,1]:=ty;
 RawComponents[3,2]:=tz;
 RawComponents[3,3]:=tw;
end;

constructor TMatrix4x4.CreateTranslation(const pTranslation:TVector4);
begin
 RawComponents[0,0]:=1.0;
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=0.0;
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=1.0;
 RawComponents[1,2]:=0.0;
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=1.0;
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=pTranslation.x;
 RawComponents[3,1]:=pTranslation.y;
 RawComponents[3,2]:=pTranslation.z;
 RawComponents[3,3]:=pTranslation.w;
end;

constructor TMatrix4x4.CreateTranslated(const pMatrix:TMatrix4x4;pTranslation:TVector3);
begin
 RawComponents[0]:=pMatrix.RawComponents[0];
 RawComponents[1]:=pMatrix.RawComponents[1];
 RawComponents[2]:=pMatrix.RawComponents[2];
 RawComponents[3,0]:=(pMatrix.RawComponents[0,0]*pTranslation.x)+(pMatrix.RawComponents[1,0]*pTranslation.y)+(pMatrix.RawComponents[2,0]*pTranslation.z)+pMatrix.RawComponents[3,0];
 RawComponents[3,1]:=(pMatrix.RawComponents[0,1]*pTranslation.x)+(pMatrix.RawComponents[1,1]*pTranslation.y)+(pMatrix.RawComponents[2,1]*pTranslation.z)+pMatrix.RawComponents[3,1];
 RawComponents[3,2]:=(pMatrix.RawComponents[0,2]*pTranslation.x)+(pMatrix.RawComponents[1,2]*pTranslation.y)+(pMatrix.RawComponents[2,2]*pTranslation.z)+pMatrix.RawComponents[3,2];
 RawComponents[3,3]:=(pMatrix.RawComponents[0,3]*pTranslation.x)+(pMatrix.RawComponents[1,3]*pTranslation.y)+(pMatrix.RawComponents[2,3]*pTranslation.z)+pMatrix.RawComponents[3,3];
end;

constructor TMatrix4x4.CreateTranslated(const pMatrix:TMatrix4x4;pTranslation:TVector4);
begin
 RawComponents[0]:=pMatrix.RawComponents[0];
 RawComponents[1]:=pMatrix.RawComponents[1];
 RawComponents[2]:=pMatrix.RawComponents[2];
 RawComponents[3,0]:=(pMatrix.RawComponents[0,0]*pTranslation.x)+(pMatrix.RawComponents[1,0]*pTranslation.y)+(pMatrix.RawComponents[2,0]*pTranslation.z)+(pMatrix.RawComponents[3,0]*pTranslation.w);
 RawComponents[3,1]:=(pMatrix.RawComponents[0,1]*pTranslation.x)+(pMatrix.RawComponents[1,1]*pTranslation.y)+(pMatrix.RawComponents[2,1]*pTranslation.z)+(pMatrix.RawComponents[3,1]*pTranslation.w);
 RawComponents[3,2]:=(pMatrix.RawComponents[0,2]*pTranslation.x)+(pMatrix.RawComponents[1,2]*pTranslation.y)+(pMatrix.RawComponents[2,2]*pTranslation.z)+(pMatrix.RawComponents[3,2]*pTranslation.w);
 RawComponents[3,3]:=(pMatrix.RawComponents[0,3]*pTranslation.x)+(pMatrix.RawComponents[1,3]*pTranslation.y)+(pMatrix.RawComponents[2,3]*pTranslation.z)+(pMatrix.RawComponents[3,3]*pTranslation.w);
end;

constructor TMatrix4x4.CreateFromToRotation(const FromDirection,ToDirection:TVector3);
var e,h,hvx,hvz,hvxy,hvxz,hvyz:TScalar;
    x,u,v,c:TVector3;
begin
 e:=FromDirection.Dot(ToDirection);
 if abs(e)>(1.0-EPSILON) then begin
  x:=FromDirection.Abs;
  if x.x<x.y then begin
   if x.x<x.z then begin
    x.x:=1.0;
    x.y:=0.0;
    x.z:=0.0;
   end else begin
    x.x:=0.0;
    x.y:=0.0;
    x.z:=1.0;
   end;
  end else begin
   if x.y<x.z then begin
    x.x:=0.0;
    x.y:=1.0;
    x.z:=0.0;
   end else begin
    x.x:=0.0;
    x.y:=0.0;
    x.z:=1.0;
   end;
  end;
  u:=x-FromDirection;
  v:=x-ToDirection;
  c.x:=2.0/(sqr(u.x)+sqr(u.y)+sqr(u.z));
  c.y:=2.0/(sqr(v.x)+sqr(v.y)+sqr(v.z));
  c.z:=c.x*c.y*((u.x*v.x)+(u.y*v.y)+(u.z*v.z));
  RawComponents[0,0]:=1.0+((c.z*(v.x*u.x))-((c.y*(v.x*v.x))+(c.x*(u.x*u.x))));
  RawComponents[0,1]:=(c.z*(v.x*u.y))-((c.y*(v.x*v.y))+(c.x*(u.x*u.y)));
  RawComponents[0,2]:=(c.z*(v.x*u.z))-((c.y*(v.x*v.z))+(c.x*(u.x*u.z)));
  RawComponents[0,3]:=0.0;
  RawComponents[1,0]:=(c.z*(v.y*u.x))-((c.y*(v.y*v.x))+(c.x*(u.y*u.x)));
  RawComponents[1,1]:=1.0+((c.z*(v.y*u.y))-((c.y*(v.y*v.y))+(c.x*(u.y*u.y))));
  RawComponents[1,2]:=(c.z*(v.y*u.z))-((c.y*(v.y*v.z))+(c.x*(u.y*u.z)));
  RawComponents[1,3]:=0.0;
  RawComponents[2,0]:=(c.z*(v.z*u.x))-((c.y*(v.z*v.x))+(c.x*(u.z*u.x)));
  RawComponents[2,1]:=(c.z*(v.z*u.y))-((c.y*(v.z*v.y))+(c.x*(u.z*u.y)));
  RawComponents[2,2]:=1.0+((c.z*(v.z*u.z))-((c.y*(v.z*v.z))+(c.x*(u.z*u.z))));
  RawComponents[2,3]:=0.0;
  RawComponents[3,0]:=0.0;
  RawComponents[3,1]:=0.0;
  RawComponents[3,2]:=0.0;
  RawComponents[3,3]:=1.0;
 end else begin
  v:=FromDirection.Cross(ToDirection);
  h:=1.0/(1.0+e);
  hvx:=h*v.x;
  hvz:=h*v.z;
  hvxy:=hvx*v.y;
  hvxz:=hvx*v.z;
  hvyz:=hvz*v.y;
  RawComponents[0,0]:=e+(hvx*v.x);
  RawComponents[0,1]:=hvxy-v.z;
  RawComponents[0,2]:=hvxz+v.y;
  RawComponents[0,3]:=0.0;
  RawComponents[1,0]:=hvxy+v.z;
  RawComponents[1,1]:=e+(h*sqr(v.y));
  RawComponents[1,2]:=hvyz-v.x;
  RawComponents[1,3]:=0.0;
  RawComponents[2,0]:=hvxz-v.y;
  RawComponents[2,1]:=hvyz+v.x;
  RawComponents[2,2]:=e+(hvz*v.z);
  RawComponents[2,3]:=0.0;
  RawComponents[3,0]:=0.0;
  RawComponents[3,1]:=0.0;
  RawComponents[3,2]:=0.0;
  RawComponents[3,3]:=1.0;
 end;
end;

constructor TMatrix4x4.CreateConstruct(const pForwards,pUp:TVector3);
var RightVector,UpVector,ForwardVector:TVector3;
begin
 ForwardVector:=(-pForwards).Normalize;
 RightVector:=pUp.Cross(ForwardVector).Normalize;
 UpVector:=ForwardVector.Cross(RightVector).Normalize;
 RawComponents[0,0]:=RightVector.x;
 RawComponents[0,1]:=RightVector.y;
 RawComponents[0,2]:=RightVector.z;
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=UpVector.x;
 RawComponents[1,1]:=UpVector.y;
 RawComponents[1,2]:=UpVector.z;
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=ForwardVector.x;
 RawComponents[2,1]:=ForwardVector.y;
 RawComponents[2,2]:=ForwardVector.z;
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=0.0;
 RawComponents[3,1]:=0.0;
 RawComponents[3,2]:=0.0;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateOuterProduct(const u,v:TVector3);
begin
 RawComponents[0,0]:=u.x*v.x;
 RawComponents[0,1]:=u.x*v.y;
 RawComponents[0,2]:=u.x*v.z;
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=u.y*v.x;
 RawComponents[1,1]:=u.y*v.y;
 RawComponents[1,2]:=u.y*v.z;
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=u.z*v.x;
 RawComponents[2,1]:=u.z*v.y;
 RawComponents[2,2]:=u.z*v.z;
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=0.0;
 RawComponents[3,1]:=0.0;
 RawComponents[3,2]:=0.0;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateFromQuaternion(pQuaternion:TQuaternion);
var qx2,qy2,qz2,qxqx2,qxqy2,qxqz2,qxqw2,qyqy2,qyqz2,qyqw2,qzqz2,qzqw2:TScalar;
begin
 pQuaternion:=pQuaternion.Normalize;
 qx2:=pQuaternion.x+pQuaternion.x;
 qy2:=pQuaternion.y+pQuaternion.y;
 qz2:=pQuaternion.z+pQuaternion.z;
 qxqx2:=pQuaternion.x*qx2;
 qxqy2:=pQuaternion.x*qy2;
 qxqz2:=pQuaternion.x*qz2;
 qxqw2:=pQuaternion.w*qx2;
 qyqy2:=pQuaternion.y*qy2;
 qyqz2:=pQuaternion.y*qz2;
 qyqw2:=pQuaternion.w*qy2;
 qzqz2:=pQuaternion.z*qz2;
 qzqw2:=pQuaternion.w*qz2;
 RawComponents[0,0]:=1.0-(qyqy2+qzqz2);
 RawComponents[0,1]:=qxqy2+qzqw2;
 RawComponents[0,2]:=qxqz2-qyqw2;
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=qxqy2-qzqw2;
 RawComponents[1,1]:=1.0-(qxqx2+qzqz2);
 RawComponents[1,2]:=qyqz2+qxqw2;
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=qxqz2+qyqw2;
 RawComponents[2,1]:=qyqz2-qxqw2;
 RawComponents[2,2]:=1.0-(qxqx2+qyqy2);
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=0.0;
 RawComponents[3,1]:=0.0;
 RawComponents[3,2]:=0.0;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateFromQTangent(pQTangent:TQuaternion);
var qx2,qy2,qz2,qxqx2,qxqy2,qxqz2,qxqw2,qyqy2,qyqz2,qyqw2,qzqz2,qzqw2:TScalar;
begin
 pQTangent:=pQTangent.Normalize;
 qx2:=pQTangent.x+pQTangent.x;
 qy2:=pQTangent.y+pQTangent.y;
 qz2:=pQTangent.z+pQTangent.z;
 qxqx2:=pQTangent.x*qx2;
 qxqy2:=pQTangent.x*qy2;
 qxqz2:=pQTangent.x*qz2;
 qxqw2:=pQTangent.w*qx2;
 qyqy2:=pQTangent.y*qy2;
 qyqz2:=pQTangent.y*qz2;
 qyqw2:=pQTangent.w*qy2;
 qzqz2:=pQTangent.z*qz2;
 qzqw2:=pQTangent.w*qz2;
 RawComponents[0,0]:=1.0-(qyqy2+qzqz2);
 RawComponents[0,1]:=qxqy2+qzqw2;
 RawComponents[0,2]:=qxqz2-qyqw2;
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=qxqy2-qzqw2;
 RawComponents[1,1]:=1.0-(qxqx2+qzqz2);
 RawComponents[1,2]:=qyqz2+qxqw2;
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=(RawComponents[0,1]*RawComponents[1,2])-(RawComponents[0,2]*RawComponents[1,1]);
 RawComponents[2,1]:=(RawComponents[0,2]*RawComponents[1,0])-(RawComponents[0,0]*RawComponents[1,2]);
 RawComponents[2,2]:=(RawComponents[0,0]*RawComponents[1,1])-(RawComponents[0,1]*RawComponents[1,0]);
{RawComponents[2,0]:=qxqz2+qyqw2;
 RawComponents[2,1]:=qyqz2-qxqw2;
 RawComponents[2,2]:=1.0-(qxqx2+qyqy2);}
 if pQTangent.w<0.0 then begin
  RawComponents[2,0]:=-RawComponents[2,0];
  RawComponents[2,1]:=-RawComponents[2,1];
  RawComponents[2,2]:=-RawComponents[2,2];
 end;
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=0.0;
 RawComponents[3,1]:=0.0;
 RawComponents[3,2]:=0.0;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateReflect(const pPlane:TPlane);
var Plane:TPlane;
    l:TScalar;
begin
 Plane:=pPlane;
 l:=sqr(Plane.Normal.x)+sqr(Plane.Normal.y)+sqr(Plane.Normal.z);
 if l>0.0 then begin
  l:=sqrt(l);
  Plane.Normal.x:=Plane.Normal.x/l;
  Plane.Normal.y:=Plane.Normal.y/l;
  Plane.Normal.z:=Plane.Normal.z/l;
  Plane.Distance:=Plane.Distance/l;
 end else begin
  Plane.Normal.x:=0.0;
  Plane.Normal.y:=0.0;
  Plane.Normal.z:=0.0;
  Plane.Distance:=0.0;
 end;
 RawComponents[0,0]:=1.0-(2.0*(Plane.Normal.x*Plane.Normal.x));
 RawComponents[0,1]:=-(2.0*(Plane.Normal.x*Plane.Normal.y));
 RawComponents[0,2]:=-(2.0*(Plane.Normal.x*Plane.Normal.z));
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=-(2.0*(Plane.Normal.x*Plane.Normal.y));
 RawComponents[1,1]:=1.0-(2.0*(Plane.Normal.y*Plane.Normal.y));
 RawComponents[1,2]:=-(2.0*(Plane.Normal.y*Plane.Normal.z));
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=-(2.0*(Plane.Normal.z*Plane.Normal.x));
 RawComponents[2,1]:=-(2.0*(Plane.Normal.z*Plane.Normal.y));
 RawComponents[2,2]:=1.0-(2.0*(Plane.Normal.z*Plane.Normal.z));
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=-(2.0*(Plane.Distance*Plane.Normal.x));
 RawComponents[3,1]:=-(2.0*(Plane.Distance*Plane.Normal.y));
 RawComponents[3,2]:=-(2.0*(Plane.Distance*Plane.Normal.z));
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateFrustum(const Left,Right,Bottom,Top,zNear,zFar:TScalar);
var rml,tmb,fmn:TScalar;
begin
 rml:=Right-Left;
 tmb:=Top-Bottom;
 fmn:=zFar-zNear;
 RawComponents[0,0]:=(zNear*2.0)/rml;
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=0.0;
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=(zNear*2.0)/tmb;
 RawComponents[1,2]:=0.0;
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=(Right+Left)/rml;
 RawComponents[2,1]:=(Top+Bottom)/tmb;
 RawComponents[2,2]:=(-(zFar+zNear))/fmn;
 RawComponents[2,3]:=-1.0;
 RawComponents[3,0]:=0.0;
 RawComponents[3,1]:=0.0;
 RawComponents[3,2]:=(-((zFar*zNear)*2.0))/fmn;
 RawComponents[3,3]:=0.0;
end;

constructor TMatrix4x4.CreateOrtho(const Left,Right,Bottom,Top,zNear,zFar:TScalar);
var rml,tmb,fmn:TScalar;
begin
 rml:=Right-Left;
 tmb:=Top-Bottom;
 fmn:=zFar-zNear;
 RawComponents[0,0]:=2.0/rml;
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=0.0;
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=2.0/tmb;
 RawComponents[1,2]:=0.0;
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=(-2.0)/fmn;
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=(-(Right+Left))/rml;
 RawComponents[3,1]:=(-(Top+Bottom))/tmb;
 RawComponents[3,2]:=(-(zFar+zNear))/fmn;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateOrthoLH(const Left,Right,Bottom,Top,zNear,zFar:TScalar);
var rml,tmb,fmn:TScalar;
begin
 rml:=Right-Left;
 tmb:=Top-Bottom;
 fmn:=zFar-zNear;
 RawComponents[0,0]:=2.0/rml;
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=0.0;
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=2.0/tmb;
 RawComponents[1,2]:=0.0;
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=1.0/fmn;
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=0;
 RawComponents[3,1]:=0;
 RawComponents[3,2]:=(-zNear)/fmn;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateOrthoRH(const Left,Right,Bottom,Top,zNear,zFar:TScalar);
var rml,tmb,fmn:TScalar;
begin
 rml:=Right-Left;
 tmb:=Top-Bottom;
 fmn:=zFar-zNear;
 RawComponents[0,0]:=2.0/rml;
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=0.0;
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=2.0/tmb;
 RawComponents[1,2]:=0.0;
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=1.0/fmn;
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=0;
 RawComponents[3,1]:=0;
 RawComponents[3,2]:=zNear/fmn;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateOrthoOffCenterLH(const Left,Right,Bottom,Top,zNear,zFar:TScalar);
var rml,tmb,fmn:TScalar;
begin
 rml:=Right-Left;
 tmb:=Top-Bottom;
 fmn:=zFar-zNear;
 RawComponents[0,0]:=2.0/rml;
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=0.0;
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=2.0/tmb;
 RawComponents[1,2]:=0.0;
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=1.0/fmn;
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=(Right+Left)/rml;
 RawComponents[3,1]:=(Top+Bottom)/tmb;
 RawComponents[3,2]:=zNear/fmn;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateOrthoOffCenterRH(const Left,Right,Bottom,Top,zNear,zFar:TScalar);
var rml,tmb,fmn:TScalar;
begin
 rml:=Right-Left;
 tmb:=Top-Bottom;
 fmn:=zFar-zNear;
 RawComponents[0,0]:=2.0/rml;
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=0.0;
 RawComponents[0,3]:=0.0;
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=2.0/tmb;
 RawComponents[1,2]:=0.0;
 RawComponents[1,3]:=0.0;
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=(-2.0)/fmn;
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=(-(Right+Left))/rml;
 RawComponents[3,1]:=(-(Top+Bottom))/tmb;
 RawComponents[3,2]:=(-(zFar+zNear))/fmn;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreatePerspective(const fovy,Aspect,zNear,zFar:TScalar);
var Sine,Cotangent,ZDelta,Radians:TScalar;
begin
 Radians:=(fovy*0.5)*DEG2RAD;
 ZDelta:=zFar-zNear;
 Sine:=sin(Radians);
 if not ((ZDelta=0) or (Sine=0) or (aspect=0)) then begin
  Cotangent:=cos(Radians)/Sine;
  RawComponents:=Matrix4x4Identity.RawComponents;
  RawComponents[0,0]:=Cotangent/aspect;
  RawComponents[1,1]:=Cotangent;
  RawComponents[2,2]:=(-(zFar+zNear))/ZDelta;
  RawComponents[2,3]:=-1-0;
  RawComponents[3,2]:=(-(2.0*zNear*zFar))/ZDelta;
  RawComponents[3,3]:=0.0;
 end;
end;

constructor TMatrix4x4.CreateLookAt(const Eye,Center,Up:TVector3);
var RightVector,UpVector,ForwardVector:TVector3;
begin
 ForwardVector:=(Eye-Center).Normalize;
 RightVector:=(Up.Cross(ForwardVector)).Normalize;
 UpVector:=(ForwardVector.Cross(RightVector)).Normalize;
 RawComponents[0,0]:=RightVector.x;
 RawComponents[1,0]:=RightVector.y;
 RawComponents[2,0]:=RightVector.z;
 RawComponents[3,0]:=-((RightVector.x*Eye.x)+(RightVector.y*Eye.y)+(RightVector.z*Eye.z));
 RawComponents[0,1]:=UpVector.x;
 RawComponents[1,1]:=UpVector.y;
 RawComponents[2,1]:=UpVector.z;
 RawComponents[3,1]:=-((UpVector.x*Eye.x)+(UpVector.y*Eye.y)+(UpVector.z*Eye.z));
 RawComponents[0,2]:=ForwardVector.x;
 RawComponents[1,2]:=ForwardVector.y;
 RawComponents[2,2]:=ForwardVector.z;
 RawComponents[3,2]:=-((ForwardVector.x*Eye.x)+(ForwardVector.y*Eye.y)+(ForwardVector.z*Eye.z));
 RawComponents[0,3]:=0.0;
 RawComponents[1,3]:=0.0;
 RawComponents[2,3]:=0.0;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateFill(const Eye,RightVector,UpVector,ForwardVector:TVector3);
begin
 RawComponents[0,0]:=RightVector.x;
 RawComponents[1,0]:=RightVector.y;
 RawComponents[2,0]:=RightVector.z;
 RawComponents[3,0]:=-((RightVector.x*Eye.x)+(RightVector.y*Eye.y)+(RightVector.z*Eye.z));
 RawComponents[0,1]:=UpVector.x;
 RawComponents[1,1]:=UpVector.y;
 RawComponents[2,1]:=UpVector.z;
 RawComponents[3,1]:=-((UpVector.x*Eye.x)+(UpVector.y*Eye.y)+(UpVector.z*Eye.z));
 RawComponents[0,2]:=ForwardVector.x;
 RawComponents[1,2]:=ForwardVector.y;
 RawComponents[2,2]:=ForwardVector.z;
 RawComponents[3,2]:=-((ForwardVector.x*Eye.x)+(ForwardVector.y*Eye.y)+(ForwardVector.z*Eye.z));
 RawComponents[0,3]:=0.0;
 RawComponents[1,3]:=0.0;
 RawComponents[2,3]:=0.0;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateConstructX(const xAxis:TVector3);
var a,b,c:TVector3;
begin
 a:=xAxis.Normalize;
 RawComponents[0,0]:=a.x;
 RawComponents[0,1]:=a.y;
 RawComponents[0,2]:=a.z;
 RawComponents[0,3]:=0.0;
//b:=TVector3.Create(0.0,0.0,1.0).Cross(a).Normalize;
 b:=a.Perpendicular.Normalize;
 RawComponents[1,0]:=b.x;
 RawComponents[1,1]:=b.y;
 RawComponents[1,2]:=b.z;
 RawComponents[1,3]:=0.0;
 c:=b.Cross(a).Normalize;
 RawComponents[2,0]:=c.x;
 RawComponents[2,1]:=c.y;
 RawComponents[2,2]:=c.z;
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=0.0;
 RawComponents[3,1]:=0.0;
 RawComponents[3,2]:=0.0;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateConstructY(const yAxis:TVector3);
var a,b,c:TVector3;
begin
 a:=yAxis.Normalize;
 RawComponents[1,0]:=a.x;
 RawComponents[1,1]:=a.y;
 RawComponents[1,2]:=a.z;
 RawComponents[1,3]:=0.0;
 b:=a.Perpendicular.Normalize;
 RawComponents[0,0]:=b.x;
 RawComponents[0,1]:=b.y;
 RawComponents[0,2]:=b.z;
 RawComponents[0,3]:=0.0;
 c:=b.Cross(a).Normalize;
 RawComponents[2,0]:=c.x;
 RawComponents[2,1]:=c.y;
 RawComponents[2,2]:=c.z;
 RawComponents[2,3]:=0.0;
 RawComponents[3,0]:=0.0;
 RawComponents[3,1]:=0.0;
 RawComponents[3,2]:=0.0;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateConstructZ(const zAxis:TVector3);
var a,b,c:TVector3;
begin
 a:=zAxis.Normalize;
 RawComponents[2,0]:=a.x;
 RawComponents[2,1]:=a.y;
 RawComponents[2,2]:=a.z;
 RawComponents[2,3]:=0.0;
//b:=TVector3.Create(0.0,1.0,0.0).Cross(a).Normalize;
 b:=a.Perpendicular.Normalize;
 RawComponents[1,0]:=b.x;
 RawComponents[1,1]:=b.y;
 RawComponents[1,2]:=b.z;
 RawComponents[1,3]:=0.0;
 c:=b.Cross(a).Normalize;
 RawComponents[0,0]:=c.x;
 RawComponents[0,1]:=c.y;
 RawComponents[0,2]:=c.z;
 RawComponents[0,3]:=0.0;
 RawComponents[3,0]:=0.0;
 RawComponents[3,1]:=0.0;
 RawComponents[3,2]:=0.0;
 RawComponents[3,3]:=1.0;
end;

constructor TMatrix4x4.CreateProjectionMatrixClip(const ProjectionMatrix:TMatrix4x4;const ClipPlane:TPlane);
var q,c:TVector4;
begin
 RawComponents:=ProjectionMatrix.RawComponents;
 q.x:=(Sign(ClipPlane.Normal.x)+RawComponents[2,0])/RawComponents[0,0];
 q.y:=(Sign(ClipPlane.Normal.y)+RawComponents[2,1])/RawComponents[1,1];
 q.z:=-1.0;
 q.w:=(1.0+RawComponents[2,2])/RawComponents[3,2];
 c.x:=ClipPlane.Normal.x;
 c.y:=ClipPlane.Normal.y;
 c.z:=ClipPlane.Normal.z;
 c.w:=ClipPlane.Distance;
 c:=c*(2.0/c.Dot(q));
 RawComponents[0,2]:=c.x;
 RawComponents[1,2]:=c.y;
 RawComponents[2,2]:=c.z+1.0;
 RawComponents[3,2]:=c.w;
end;

constructor TMatrix4x4.CreateRecomposed(const DecomposedMatrix4x4:TDecomposedMatrix4x4);
begin

 RawComponents[0,0]:=1.0;
 RawComponents[0,1]:=0.0;
 RawComponents[0,2]:=0.0;
 RawComponents[0,3]:=DecomposedMatrix4x4.Perspective.x;
 RawComponents[1,0]:=0.0;
 RawComponents[1,1]:=1.0;
 RawComponents[1,2]:=0.0;
 RawComponents[1,3]:=DecomposedMatrix4x4.Perspective.y;
 RawComponents[2,0]:=0.0;
 RawComponents[2,1]:=0.0;
 RawComponents[2,2]:=1.0;
 RawComponents[2,3]:=DecomposedMatrix4x4.Perspective.z;
 RawComponents[3,0]:=0.0;
 RawComponents[3,1]:=0.0;
 RawComponents[3,2]:=0.0;
 RawComponents[3,3]:=DecomposedMatrix4x4.Perspective.w;

//self:=TMatrix4x4.CreateTranslation(DecomposedMatrix4x4.Translation)*self;
 Translation:=Translation+
              (Right*DecomposedMatrix4x4.Translation.x)+
              (Up*DecomposedMatrix4x4.Translation.y)+
              (Forwards*DecomposedMatrix4x4.Translation.z);

 self:=TMatrix4x4.CreateFromQuaternion(DecomposedMatrix4x4.Rotation)*self;

 if DecomposedMatrix4x4.Skew.z<>0.0 then begin // YZ
  self:=TMatrix4x4.Create(1.0,0.0,0.0,0.0,
                          0.0,1.0,0.0,0.0,
                          0.0,DecomposedMatrix4x4.Skew.z,1.0,0.0,
                          0.0,0.0,0.0,1.0)*self;
 end;

 if DecomposedMatrix4x4.Skew.y<>0.0 then begin // XZ
  self:=TMatrix4x4.Create(1.0,0.0,0.0,0.0,
                          0.0,1.0,0.0,0.0,
                          DecomposedMatrix4x4.Skew.y,0.0,1.0,0.0,
                          0.0,0.0,0.0,1.0)*self;
 end;

 if DecomposedMatrix4x4.Skew.x<>0.0 then begin // XY
  self:=TMatrix4x4.Create(1.0,0.0,0.0,0.0,
                          DecomposedMatrix4x4.Skew.x,1.0,0.0,0.0,
                          0.0,0.0,1.0,0.0,
                          0.0,0.0,0.0,1.0)*self;
 end;

 self:=TMatrix4x4.CreateScale(DecomposedMatrix4x4.Scale)*self;

end;

class operator TMatrix4x4.Implicit({$ifdef fpc}constref{$else}const{$endif} a:TScalar):TMatrix4x4;
begin
 result.RawComponents[0,0]:=a;
 result.RawComponents[0,1]:=a;
 result.RawComponents[0,2]:=a;
 result.RawComponents[0,3]:=a;
 result.RawComponents[1,0]:=a;
 result.RawComponents[1,1]:=a;
 result.RawComponents[1,2]:=a;
 result.RawComponents[1,3]:=a;
 result.RawComponents[2,0]:=a;
 result.RawComponents[2,1]:=a;
 result.RawComponents[2,2]:=a;
 result.RawComponents[2,3]:=a;
 result.RawComponents[3,0]:=a;
 result.RawComponents[3,1]:=a;
 result.RawComponents[3,2]:=a;
 result.RawComponents[3,3]:=a;
end;

class operator TMatrix4x4.Explicit({$ifdef fpc}constref{$else}const{$endif} a:TScalar):TMatrix4x4;
begin
 result.RawComponents[0,0]:=a;
 result.RawComponents[0,1]:=a;
 result.RawComponents[0,2]:=a;
 result.RawComponents[0,3]:=a;
 result.RawComponents[1,0]:=a;
 result.RawComponents[1,1]:=a;
 result.RawComponents[1,2]:=a;
 result.RawComponents[1,3]:=a;
 result.RawComponents[2,0]:=a;
 result.RawComponents[2,1]:=a;
 result.RawComponents[2,2]:=a;
 result.RawComponents[2,3]:=a;
 result.RawComponents[3,0]:=a;
 result.RawComponents[3,1]:=a;
 result.RawComponents[3,2]:=a;
 result.RawComponents[3,3]:=a;
end;

class operator TMatrix4x4.Equal({$ifdef fpc}constref{$else}const{$endif} a,b:TMatrix4x4):boolean;
begin
 result:=SameValue(a.RawComponents[0,0],b.RawComponents[0,0]) and
         SameValue(a.RawComponents[0,1],b.RawComponents[0,1]) and
         SameValue(a.RawComponents[0,2],b.RawComponents[0,2]) and
         SameValue(a.RawComponents[0,3],b.RawComponents[0,3]) and
         SameValue(a.RawComponents[1,0],b.RawComponents[1,0]) and
         SameValue(a.RawComponents[1,1],b.RawComponents[1,1]) and
         SameValue(a.RawComponents[1,2],b.RawComponents[1,2]) and
         SameValue(a.RawComponents[1,3],b.RawComponents[1,3]) and
         SameValue(a.RawComponents[2,0],b.RawComponents[2,0]) and
         SameValue(a.RawComponents[2,1],b.RawComponents[2,1]) and
         SameValue(a.RawComponents[2,2],b.RawComponents[2,2]) and
         SameValue(a.RawComponents[2,3],b.RawComponents[2,3]) and
         SameValue(a.RawComponents[3,0],b.RawComponents[3,0]) and
         SameValue(a.RawComponents[3,1],b.RawComponents[3,1]) and
         SameValue(a.RawComponents[3,2],b.RawComponents[3,2]) and
         SameValue(a.RawComponents[3,3],b.RawComponents[3,3]);
end;

class operator TMatrix4x4.NotEqual({$ifdef fpc}constref{$else}const{$endif} a,b:TMatrix4x4):boolean;
begin
 result:=(not SameValue(a.RawComponents[0,0],b.RawComponents[0,0])) or
         (not SameValue(a.RawComponents[0,1],b.RawComponents[0,1])) or
         (not SameValue(a.RawComponents[0,2],b.RawComponents[0,2])) or
         (not SameValue(a.RawComponents[0,3],b.RawComponents[0,3])) or
         (not SameValue(a.RawComponents[1,0],b.RawComponents[1,0])) or
         (not SameValue(a.RawComponents[1,1],b.RawComponents[1,1])) or
         (not SameValue(a.RawComponents[1,2],b.RawComponents[1,2])) or
         (not SameValue(a.RawComponents[1,3],b.RawComponents[1,3])) or
         (not SameValue(a.RawComponents[2,0],b.RawComponents[2,0])) or
         (not SameValue(a.RawComponents[2,1],b.RawComponents[2,1])) or
         (not SameValue(a.RawComponents[2,2],b.RawComponents[2,2])) or
         (not SameValue(a.RawComponents[2,3],b.RawComponents[2,3])) or
         (not SameValue(a.RawComponents[3,0],b.RawComponents[3,0])) or
         (not SameValue(a.RawComponents[3,1],b.RawComponents[3,1])) or
         (not SameValue(a.RawComponents[3,2],b.RawComponents[3,2])) or
         (not SameValue(a.RawComponents[3,3],b.RawComponents[3,3]));
end;

class operator TMatrix4x4.Inc({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4):TMatrix4x4;
{$if defined(cpu386) or defined(cpux64)}
const cOne:array[0..3] of single=(1.0,1.0,1.0,1.0);
asm
 movups xmm0,dqword ptr [a+0]
 movups xmm1,dqword ptr [a+16]
 movups xmm2,dqword ptr [a+32]
 movups xmm3,dqword ptr [a+48]
{$ifdef cpu386}
 movups xmm4,dqword ptr [cOne]
{$else}
{$ifdef fpc}
 movups xmm4,dqword ptr [rip+cOne]
{$else}
 movups xmm4,dqword ptr [rel cOne]
{$endif}
{$endif}
 addps xmm0,xmm4
 addps xmm1,xmm4
 addps xmm2,xmm4
 addps xmm3,xmm4
 movups dqword ptr [result+0],xmm0
 movups dqword ptr [result+16],xmm1
 movups dqword ptr [result+32],xmm2
 movups dqword ptr [result+48],xmm3
end;
{$else}
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]+1.0;
 result.RawComponents[0,1]:=a.RawComponents[0,1]+1.0;
 result.RawComponents[0,2]:=a.RawComponents[0,2]+1.0;
 result.RawComponents[0,3]:=a.RawComponents[0,3]+1.0;
 result.RawComponents[1,0]:=a.RawComponents[1,0]+1.0;
 result.RawComponents[1,1]:=a.RawComponents[1,1]+1.0;
 result.RawComponents[1,2]:=a.RawComponents[1,2]+1.0;
 result.RawComponents[1,3]:=a.RawComponents[1,3]+1.0;
 result.RawComponents[2,0]:=a.RawComponents[2,0]+1.0;
 result.RawComponents[2,1]:=a.RawComponents[2,1]+1.0;
 result.RawComponents[2,2]:=a.RawComponents[2,2]+1.0;
 result.RawComponents[2,3]:=a.RawComponents[2,3]+1.0;
 result.RawComponents[3,0]:=a.RawComponents[3,0]+1.0;
 result.RawComponents[3,1]:=a.RawComponents[3,1]+1.0;
 result.RawComponents[3,2]:=a.RawComponents[3,2]+1.0;
 result.RawComponents[3,3]:=a.RawComponents[3,3]+1.0;
end;
{$ifend}

class operator TMatrix4x4.Dec({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4):TMatrix4x4;
{$if defined(cpu386) or defined(cpux64)}
const cOne:array[0..3] of single=(1.0,1.0,1.0,1.0);
asm
 movups xmm0,dqword ptr [a+0]
 movups xmm1,dqword ptr [a+16]
 movups xmm2,dqword ptr [a+32]
 movups xmm3,dqword ptr [a+48]
{$ifdef cpu386}
 movups xmm4,dqword ptr [cOne]
{$else}
{$ifdef fpc}
 movups xmm4,dqword ptr [rip+cOne]
{$else}
 movups xmm4,dqword ptr [rel cOne]
{$endif}
{$endif}
 subps xmm0,xmm4
 subps xmm1,xmm4
 subps xmm2,xmm4
 subps xmm3,xmm4
 movups dqword ptr [result+0],xmm0
 movups dqword ptr [result+16],xmm1
 movups dqword ptr [result+32],xmm2
 movups dqword ptr [result+48],xmm3
end;
{$else}
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]-1.0;
 result.RawComponents[0,1]:=a.RawComponents[0,1]-1.0;
 result.RawComponents[0,2]:=a.RawComponents[0,2]-1.0;
 result.RawComponents[0,3]:=a.RawComponents[0,3]-1.0;
 result.RawComponents[1,0]:=a.RawComponents[1,0]-1.0;
 result.RawComponents[1,1]:=a.RawComponents[1,1]-1.0;
 result.RawComponents[1,2]:=a.RawComponents[1,2]-1.0;
 result.RawComponents[1,3]:=a.RawComponents[1,3]-1.0;
 result.RawComponents[2,0]:=a.RawComponents[2,0]-1.0;
 result.RawComponents[2,1]:=a.RawComponents[2,1]-1.0;
 result.RawComponents[2,2]:=a.RawComponents[2,2]-1.0;
 result.RawComponents[2,3]:=a.RawComponents[2,3]-1.0;
 result.RawComponents[3,0]:=a.RawComponents[3,0]-1.0;
 result.RawComponents[3,1]:=a.RawComponents[3,1]-1.0;
 result.RawComponents[3,2]:=a.RawComponents[3,2]-1.0;
 result.RawComponents[3,3]:=a.RawComponents[3,3]-1.0;
end;
{$ifend}

class operator TMatrix4x4.Add({$ifdef fpc}constref{$else}const{$endif} a,b:TMatrix4x4):TMatrix4x4;
{$if defined(cpu386) or defined(cpux64)}
asm
 movups xmm0,dqword ptr [a+0]
 movups xmm1,dqword ptr [a+16]
 movups xmm2,dqword ptr [a+32]
 movups xmm3,dqword ptr [a+48]
 movups xmm4,dqword ptr [b+0]
 movups xmm5,dqword ptr [b+16]
 movups xmm6,dqword ptr [b+32]
 movups xmm7,dqword ptr [b+48]
 addps xmm0,xmm4
 addps xmm1,xmm5
 addps xmm2,xmm6
 addps xmm3,xmm7
 movups dqword ptr [result+0],xmm0
 movups dqword ptr [result+16],xmm1
 movups dqword ptr [result+32],xmm2
 movups dqword ptr [result+48],xmm3
end;
{$else}
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]+b.RawComponents[0,0];
 result.RawComponents[0,1]:=a.RawComponents[0,1]+b.RawComponents[0,1];
 result.RawComponents[0,2]:=a.RawComponents[0,2]+b.RawComponents[0,2];
 result.RawComponents[0,3]:=a.RawComponents[0,3]+b.RawComponents[0,3];
 result.RawComponents[1,0]:=a.RawComponents[1,0]+b.RawComponents[1,0];
 result.RawComponents[1,1]:=a.RawComponents[1,1]+b.RawComponents[1,1];
 result.RawComponents[1,2]:=a.RawComponents[1,2]+b.RawComponents[1,2];
 result.RawComponents[1,3]:=a.RawComponents[1,3]+b.RawComponents[1,3];
 result.RawComponents[2,0]:=a.RawComponents[2,0]+b.RawComponents[2,0];
 result.RawComponents[2,1]:=a.RawComponents[2,1]+b.RawComponents[2,1];
 result.RawComponents[2,2]:=a.RawComponents[2,2]+b.RawComponents[2,2];
 result.RawComponents[2,3]:=a.RawComponents[2,3]+b.RawComponents[2,3];
 result.RawComponents[3,0]:=a.RawComponents[3,0]+b.RawComponents[3,0];
 result.RawComponents[3,1]:=a.RawComponents[3,1]+b.RawComponents[3,1];
 result.RawComponents[3,2]:=a.RawComponents[3,2]+b.RawComponents[3,2];
 result.RawComponents[3,3]:=a.RawComponents[3,3]+b.RawComponents[3,3];
end;
{$ifend}

class operator TMatrix4x4.Add({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4;{$ifdef fpc}constref{$else}const{$endif} b:TScalar):TMatrix4x4;
{$if defined(cpu386) or defined(cpux64)}
asm
 movups xmm0,dqword ptr [a+0]
 movups xmm1,dqword ptr [a+16]
 movups xmm2,dqword ptr [a+32]
 movups xmm3,dqword ptr [a+48]
 movss xmm4,dword ptr [b]
 shufps xmm4,xmm4,$00
 addps xmm0,xmm4
 addps xmm1,xmm4
 addps xmm2,xmm4
 addps xmm3,xmm4
 movups dqword ptr [result+0],xmm0
 movups dqword ptr [result+16],xmm1
 movups dqword ptr [result+32],xmm2
 movups dqword ptr [result+48],xmm3
end;
{$else}
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]+b;
 result.RawComponents[0,1]:=a.RawComponents[0,1]+b;
 result.RawComponents[0,2]:=a.RawComponents[0,2]+b;
 result.RawComponents[0,3]:=a.RawComponents[0,3]+b;
 result.RawComponents[1,0]:=a.RawComponents[1,0]+b;
 result.RawComponents[1,1]:=a.RawComponents[1,1]+b;
 result.RawComponents[1,2]:=a.RawComponents[1,2]+b;
 result.RawComponents[1,3]:=a.RawComponents[1,3]+b;
 result.RawComponents[2,0]:=a.RawComponents[2,0]+b;
 result.RawComponents[2,1]:=a.RawComponents[2,1]+b;
 result.RawComponents[2,2]:=a.RawComponents[2,2]+b;
 result.RawComponents[2,3]:=a.RawComponents[2,3]+b;
 result.RawComponents[3,0]:=a.RawComponents[3,0]+b;
 result.RawComponents[3,1]:=a.RawComponents[3,1]+b;
 result.RawComponents[3,2]:=a.RawComponents[3,2]+b;
 result.RawComponents[3,3]:=a.RawComponents[3,3]+b;
end;
{$ifend}

class operator TMatrix4x4.Add({$ifdef fpc}constref{$else}const{$endif} a:TScalar;{$ifdef fpc}constref{$else}const{$endif} b:TMatrix4x4):TMatrix4x4;
{$if defined(cpu386) or defined(cpux64)}
asm
 movss xmm0,dword ptr [a]
 shufps xmm0,xmm0,$00
 movaps xmm1,xmm0
 movaps xmm2,xmm0
 movaps xmm3,xmm0
 movups xmm4,dqword ptr [b+0]
 movups xmm5,dqword ptr [b+16]
 movups xmm6,dqword ptr [b+32]
 movups xmm7,dqword ptr [b+48]
 addps xmm0,xmm4
 addps xmm1,xmm5
 addps xmm2,xmm6
 addps xmm3,xmm7
 movups dqword ptr [result+0],xmm0
 movups dqword ptr [result+16],xmm1
 movups dqword ptr [result+32],xmm2
 movups dqword ptr [result+48],xmm3
end;
{$else}
begin
 result.RawComponents[0,0]:=a+b.RawComponents[0,0];
 result.RawComponents[0,1]:=a+b.RawComponents[0,1];
 result.RawComponents[0,2]:=a+b.RawComponents[0,2];
 result.RawComponents[0,3]:=a+b.RawComponents[0,3];
 result.RawComponents[1,0]:=a+b.RawComponents[1,0];
 result.RawComponents[1,1]:=a+b.RawComponents[1,1];
 result.RawComponents[1,2]:=a+b.RawComponents[1,2];
 result.RawComponents[1,3]:=a+b.RawComponents[1,3];
 result.RawComponents[2,0]:=a+b.RawComponents[2,0];
 result.RawComponents[2,1]:=a+b.RawComponents[2,1];
 result.RawComponents[2,2]:=a+b.RawComponents[2,2];
 result.RawComponents[2,3]:=a+b.RawComponents[2,3];
 result.RawComponents[3,0]:=a+b.RawComponents[3,0];
 result.RawComponents[3,1]:=a+b.RawComponents[3,1];
 result.RawComponents[3,2]:=a+b.RawComponents[3,2];
 result.RawComponents[3,3]:=a+b.RawComponents[3,3];
end;
{$ifend}

class operator TMatrix4x4.Subtract({$ifdef fpc}constref{$else}const{$endif} a,b:TMatrix4x4):TMatrix4x4;
{$if defined(cpu386) or defined(cpux64)}
asm
 movups xmm0,dqword ptr [a+0]
 movups xmm1,dqword ptr [a+16]
 movups xmm2,dqword ptr [a+32]
 movups xmm3,dqword ptr [a+48]
 movups xmm4,dqword ptr [b+0]
 movups xmm5,dqword ptr [b+16]
 movups xmm6,dqword ptr [b+32]
 movups xmm7,dqword ptr [b+48]
 subps xmm0,xmm4
 subps xmm1,xmm5
 subps xmm2,xmm6
 subps xmm3,xmm7
 movups dqword ptr [result+0],xmm0
 movups dqword ptr [result+16],xmm1
 movups dqword ptr [result+32],xmm2
 movups dqword ptr [result+48],xmm3
end;
{$else}
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]-b.RawComponents[0,0];
 result.RawComponents[0,1]:=a.RawComponents[0,1]-b.RawComponents[0,1];
 result.RawComponents[0,2]:=a.RawComponents[0,2]-b.RawComponents[0,2];
 result.RawComponents[0,3]:=a.RawComponents[0,3]-b.RawComponents[0,3];
 result.RawComponents[1,0]:=a.RawComponents[1,0]-b.RawComponents[1,0];
 result.RawComponents[1,1]:=a.RawComponents[1,1]-b.RawComponents[1,1];
 result.RawComponents[1,2]:=a.RawComponents[1,2]-b.RawComponents[1,2];
 result.RawComponents[1,3]:=a.RawComponents[1,3]-b.RawComponents[1,3];
 result.RawComponents[2,0]:=a.RawComponents[2,0]-b.RawComponents[2,0];
 result.RawComponents[2,1]:=a.RawComponents[2,1]-b.RawComponents[2,1];
 result.RawComponents[2,2]:=a.RawComponents[2,2]-b.RawComponents[2,2];
 result.RawComponents[2,3]:=a.RawComponents[2,3]-b.RawComponents[2,3];
 result.RawComponents[3,0]:=a.RawComponents[3,0]-b.RawComponents[3,0];
 result.RawComponents[3,1]:=a.RawComponents[3,1]-b.RawComponents[3,1];
 result.RawComponents[3,2]:=a.RawComponents[3,2]-b.RawComponents[3,2];
 result.RawComponents[3,3]:=a.RawComponents[3,3]-b.RawComponents[3,3];
end;
{$ifend}

class operator TMatrix4x4.Subtract({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4;{$ifdef fpc}constref{$else}const{$endif} b:TScalar):TMatrix4x4;
{$if defined(cpu386) or defined(cpux64)}
asm
 movups xmm0,dqword ptr [a+0]
 movups xmm1,dqword ptr [a+16]
 movups xmm2,dqword ptr [a+32]
 movups xmm3,dqword ptr [a+48]
 movss xmm4,dword ptr [b]
 shufps xmm4,xmm4,$00
 subps xmm0,xmm4
 subps xmm1,xmm4
 subps xmm2,xmm4
 subps xmm3,xmm4
 movups dqword ptr [result+0],xmm0
 movups dqword ptr [result+16],xmm1
 movups dqword ptr [result+32],xmm2
 movups dqword ptr [result+48],xmm3
end;
{$else}
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]-b;
 result.RawComponents[0,1]:=a.RawComponents[0,1]-b;
 result.RawComponents[0,2]:=a.RawComponents[0,2]-b;
 result.RawComponents[0,3]:=a.RawComponents[0,3]-b;
 result.RawComponents[1,0]:=a.RawComponents[1,0]-b;
 result.RawComponents[1,1]:=a.RawComponents[1,1]-b;
 result.RawComponents[1,2]:=a.RawComponents[1,2]-b;
 result.RawComponents[1,3]:=a.RawComponents[1,3]-b;
 result.RawComponents[2,0]:=a.RawComponents[2,0]-b;
 result.RawComponents[2,1]:=a.RawComponents[2,1]-b;
 result.RawComponents[2,2]:=a.RawComponents[2,2]-b;
 result.RawComponents[2,3]:=a.RawComponents[2,3]-b;
 result.RawComponents[3,0]:=a.RawComponents[3,0]-b;
 result.RawComponents[3,1]:=a.RawComponents[3,1]-b;
 result.RawComponents[3,2]:=a.RawComponents[3,2]-b;
 result.RawComponents[3,3]:=a.RawComponents[3,3]-b;
end;
{$ifend}

class operator TMatrix4x4.Subtract({$ifdef fpc}constref{$else}const{$endif} a:TScalar;{$ifdef fpc}constref{$else}const{$endif} b:TMatrix4x4): TMatrix4x4;
{$if defined(cpu386) or defined(cpux64)}
asm
 movss xmm0,dword ptr [a]
 shufps xmm0,xmm0,$00
 movaps xmm1,xmm0
 movaps xmm2,xmm0
 movaps xmm3,xmm0
 movups xmm4,dqword ptr [b+0]
 movups xmm5,dqword ptr [b+16]
 movups xmm6,dqword ptr [b+32]
 movups xmm7,dqword ptr [b+48]
 subps xmm0,xmm4
 subps xmm1,xmm5
 subps xmm2,xmm6
 subps xmm3,xmm7
 movups dqword ptr [result+0],xmm0
 movups dqword ptr [result+16],xmm1
 movups dqword ptr [result+32],xmm2
 movups dqword ptr [result+48],xmm3
end;
{$else}
begin
 result.RawComponents[0,0]:=a-b.RawComponents[0,0];
 result.RawComponents[0,1]:=a-b.RawComponents[0,1];
 result.RawComponents[0,2]:=a-b.RawComponents[0,2];
 result.RawComponents[0,3]:=a-b.RawComponents[0,3];
 result.RawComponents[1,0]:=a-b.RawComponents[1,0];
 result.RawComponents[1,1]:=a-b.RawComponents[1,1];
 result.RawComponents[1,2]:=a-b.RawComponents[1,2];
 result.RawComponents[1,3]:=a-b.RawComponents[1,3];
 result.RawComponents[2,0]:=a-b.RawComponents[2,0];
 result.RawComponents[2,1]:=a-b.RawComponents[2,1];
 result.RawComponents[2,2]:=a-b.RawComponents[2,2];
 result.RawComponents[2,3]:=a-b.RawComponents[2,3];
 result.RawComponents[3,0]:=a-b.RawComponents[3,0];
 result.RawComponents[3,1]:=a-b.RawComponents[3,1];
 result.RawComponents[3,2]:=a-b.RawComponents[3,2];
 result.RawComponents[3,3]:=a-b.RawComponents[3,3];
end;
{$ifend}

class operator TMatrix4x4.Multiply({$ifdef fpc}constref{$else}const{$endif} a,b:TMatrix4x4):TMatrix4x4;
{$if defined(cpu386) or defined(cpux64)}
asm
 movups xmm0,dqword ptr [b+0]
 movups xmm1,dqword ptr [b+16]
 movups xmm2,dqword ptr [b+32]
 movups xmm3,dqword ptr [b+48]

 movups xmm7,dqword ptr [a+0]
 pshufd xmm4,xmm7,$00
 pshufd xmm5,xmm7,$55
 pshufd xmm6,xmm7,$aa
 pshufd xmm7,xmm7,$ff
 mulps xmm4,xmm0
 mulps xmm5,xmm1
 mulps xmm6,xmm2
 mulps xmm7,xmm3
 addps xmm4,xmm5
 addps xmm6,xmm7
 addps xmm4,xmm6
 movups dqword ptr [result+0],xmm4

 movups xmm7,dqword ptr [a+16]
 pshufd xmm4,xmm7,$00
 pshufd xmm5,xmm7,$55
 pshufd xmm6,xmm7,$aa
 pshufd xmm7,xmm7,$ff
 mulps xmm4,xmm0
 mulps xmm5,xmm1
 mulps xmm6,xmm2
 mulps xmm7,xmm3
 addps xmm4,xmm5
 addps xmm6,xmm7
 addps xmm4,xmm6
 movups dqword ptr [result+16],xmm4

 movups xmm7,dqword ptr [a+32]
 pshufd xmm4,xmm7,$00
 pshufd xmm5,xmm7,$55
 pshufd xmm6,xmm7,$aa
 pshufd xmm7,xmm7,$ff
 mulps xmm4,xmm0
 mulps xmm5,xmm1
 mulps xmm6,xmm2
 mulps xmm7,xmm3
 addps xmm4,xmm5
 addps xmm6,xmm7
 addps xmm4,xmm6
 movups dqword ptr [result+32],xmm4

 movups xmm7,dqword ptr [a+48]
 pshufd xmm4,xmm7,$00
 pshufd xmm5,xmm7,$55
 pshufd xmm6,xmm7,$aa
 pshufd xmm7,xmm7,$ff
 mulps xmm4,xmm0
 mulps xmm5,xmm1
 mulps xmm6,xmm2
 mulps xmm7,xmm3
 addps xmm4,xmm5
 addps xmm6,xmm7
 addps xmm4,xmm6
 movups dqword ptr [result+48],xmm4

end;
{$else}
begin
 result.RawComponents[0,0]:=(a.RawComponents[0,0]*b.RawComponents[0,0])+(a.RawComponents[0,1]*b.RawComponents[1,0])+(a.RawComponents[0,2]*b.RawComponents[2,0])+(a.RawComponents[0,3]*b.RawComponents[3,0]);
 result.RawComponents[0,1]:=(a.RawComponents[0,0]*b.RawComponents[0,1])+(a.RawComponents[0,1]*b.RawComponents[1,1])+(a.RawComponents[0,2]*b.RawComponents[2,1])+(a.RawComponents[0,3]*b.RawComponents[3,1]);
 result.RawComponents[0,2]:=(a.RawComponents[0,0]*b.RawComponents[0,2])+(a.RawComponents[0,1]*b.RawComponents[1,2])+(a.RawComponents[0,2]*b.RawComponents[2,2])+(a.RawComponents[0,3]*b.RawComponents[3,2]);
 result.RawComponents[0,3]:=(a.RawComponents[0,0]*b.RawComponents[0,3])+(a.RawComponents[0,1]*b.RawComponents[1,3])+(a.RawComponents[0,2]*b.RawComponents[2,3])+(a.RawComponents[0,3]*b.RawComponents[3,3]);
 result.RawComponents[1,0]:=(a.RawComponents[1,0]*b.RawComponents[0,0])+(a.RawComponents[1,1]*b.RawComponents[1,0])+(a.RawComponents[1,2]*b.RawComponents[2,0])+(a.RawComponents[1,3]*b.RawComponents[3,0]);
 result.RawComponents[1,1]:=(a.RawComponents[1,0]*b.RawComponents[0,1])+(a.RawComponents[1,1]*b.RawComponents[1,1])+(a.RawComponents[1,2]*b.RawComponents[2,1])+(a.RawComponents[1,3]*b.RawComponents[3,1]);
 result.RawComponents[1,2]:=(a.RawComponents[1,0]*b.RawComponents[0,2])+(a.RawComponents[1,1]*b.RawComponents[1,2])+(a.RawComponents[1,2]*b.RawComponents[2,2])+(a.RawComponents[1,3]*b.RawComponents[3,2]);
 result.RawComponents[1,3]:=(a.RawComponents[1,0]*b.RawComponents[0,3])+(a.RawComponents[1,1]*b.RawComponents[1,3])+(a.RawComponents[1,2]*b.RawComponents[2,3])+(a.RawComponents[1,3]*b.RawComponents[3,3]);
 result.RawComponents[2,0]:=(a.RawComponents[2,0]*b.RawComponents[0,0])+(a.RawComponents[2,1]*b.RawComponents[1,0])+(a.RawComponents[2,2]*b.RawComponents[2,0])+(a.RawComponents[2,3]*b.RawComponents[3,0]);
 result.RawComponents[2,1]:=(a.RawComponents[2,0]*b.RawComponents[0,1])+(a.RawComponents[2,1]*b.RawComponents[1,1])+(a.RawComponents[2,2]*b.RawComponents[2,1])+(a.RawComponents[2,3]*b.RawComponents[3,1]);
 result.RawComponents[2,2]:=(a.RawComponents[2,0]*b.RawComponents[0,2])+(a.RawComponents[2,1]*b.RawComponents[1,2])+(a.RawComponents[2,2]*b.RawComponents[2,2])+(a.RawComponents[2,3]*b.RawComponents[3,2]);
 result.RawComponents[2,3]:=(a.RawComponents[2,0]*b.RawComponents[0,3])+(a.RawComponents[2,1]*b.RawComponents[1,3])+(a.RawComponents[2,2]*b.RawComponents[2,3])+(a.RawComponents[2,3]*b.RawComponents[3,3]);
 result.RawComponents[3,0]:=(a.RawComponents[3,0]*b.RawComponents[0,0])+(a.RawComponents[3,1]*b.RawComponents[1,0])+(a.RawComponents[3,2]*b.RawComponents[2,0])+(a.RawComponents[3,3]*b.RawComponents[3,0]);
 result.RawComponents[3,1]:=(a.RawComponents[3,0]*b.RawComponents[0,1])+(a.RawComponents[3,1]*b.RawComponents[1,1])+(a.RawComponents[3,2]*b.RawComponents[2,1])+(a.RawComponents[3,3]*b.RawComponents[3,1]);
 result.RawComponents[3,2]:=(a.RawComponents[3,0]*b.RawComponents[0,2])+(a.RawComponents[3,1]*b.RawComponents[1,2])+(a.RawComponents[3,2]*b.RawComponents[2,2])+(a.RawComponents[3,3]*b.RawComponents[3,2]);
 result.RawComponents[3,3]:=(a.RawComponents[3,0]*b.RawComponents[0,3])+(a.RawComponents[3,1]*b.RawComponents[1,3])+(a.RawComponents[3,2]*b.RawComponents[2,3])+(a.RawComponents[3,3]*b.RawComponents[3,3]);
end;
{$ifend}

class operator TMatrix4x4.Multiply({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4;{$ifdef fpc}constref{$else}const{$endif} b:TScalar):TMatrix4x4;
{$if defined(cpu386) or defined(cpux64)}
asm
 movups xmm0,dqword ptr [a+0]
 movups xmm1,dqword ptr [a+16]
 movups xmm2,dqword ptr [a+32]
 movups xmm3,dqword ptr [a+48]
 movss xmm4,dword ptr [b]
 shufps xmm4,xmm4,$00
 mulps xmm0,xmm4
 mulps xmm1,xmm4
 mulps xmm2,xmm4
 mulps xmm3,xmm4
 movups dqword ptr [result+0],xmm0
 movups dqword ptr [result+16],xmm1
 movups dqword ptr [result+32],xmm2
 movups dqword ptr [result+48],xmm3
end;
{$else}
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]*b;
 result.RawComponents[0,1]:=a.RawComponents[0,1]*b;
 result.RawComponents[0,2]:=a.RawComponents[0,2]*b;
 result.RawComponents[0,3]:=a.RawComponents[0,3]*b;
 result.RawComponents[1,0]:=a.RawComponents[1,0]*b;
 result.RawComponents[1,1]:=a.RawComponents[1,1]*b;
 result.RawComponents[1,2]:=a.RawComponents[1,2]*b;
 result.RawComponents[1,3]:=a.RawComponents[1,3]*b;
 result.RawComponents[2,0]:=a.RawComponents[2,0]*b;
 result.RawComponents[2,1]:=a.RawComponents[2,1]*b;
 result.RawComponents[2,2]:=a.RawComponents[2,2]*b;
 result.RawComponents[2,3]:=a.RawComponents[2,3]*b;
 result.RawComponents[3,0]:=a.RawComponents[3,0]*b;
 result.RawComponents[3,1]:=a.RawComponents[3,1]*b;
 result.RawComponents[3,2]:=a.RawComponents[3,2]*b;
 result.RawComponents[3,3]:=a.RawComponents[3,3]*b;
end;
{$ifend}

class operator TMatrix4x4.Multiply({$ifdef fpc}constref{$else}const{$endif} a:TScalar;{$ifdef fpc}constref{$else}const{$endif} b:TMatrix4x4):TMatrix4x4;
{$if defined(cpu386) or defined(cpux64)}
asm
 movss xmm0,dword ptr [a]
 shufps xmm0,xmm0,$00
 movaps xmm1,xmm0
 movaps xmm2,xmm0
 movaps xmm3,xmm0
 movups xmm4,dqword ptr [b+0]
 movups xmm5,dqword ptr [b+16]
 movups xmm6,dqword ptr [b+32]
 movups xmm7,dqword ptr [b+48]
 mulps xmm0,xmm4
 mulps xmm1,xmm5
 mulps xmm2,xmm6
 mulps xmm3,xmm7
 movups dqword ptr [result+0],xmm0
 movups dqword ptr [result+16],xmm1
 movups dqword ptr [result+32],xmm2
 movups dqword ptr [result+48],xmm3
end;
{$else}
begin
 result.RawComponents[0,0]:=a*b.RawComponents[0,0];
 result.RawComponents[0,1]:=a*b.RawComponents[0,1];
 result.RawComponents[0,2]:=a*b.RawComponents[0,2];
 result.RawComponents[0,3]:=a*b.RawComponents[0,3];
 result.RawComponents[1,0]:=a*b.RawComponents[1,0];
 result.RawComponents[1,1]:=a*b.RawComponents[1,1];
 result.RawComponents[1,2]:=a*b.RawComponents[1,2];
 result.RawComponents[1,3]:=a*b.RawComponents[1,3];
 result.RawComponents[2,0]:=a*b.RawComponents[2,0];
 result.RawComponents[2,1]:=a*b.RawComponents[2,1];
 result.RawComponents[2,2]:=a*b.RawComponents[2,2];
 result.RawComponents[2,3]:=a*b.RawComponents[2,3];
 result.RawComponents[3,0]:=a*b.RawComponents[3,0];
 result.RawComponents[3,1]:=a*b.RawComponents[3,1];
 result.RawComponents[3,2]:=a*b.RawComponents[3,2];
 result.RawComponents[3,3]:=a*b.RawComponents[3,3];
end;
{$ifend}

class operator TMatrix4x4.Multiply({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4;{$ifdef fpc}constref{$else}const{$endif} b:TVector3):TVector3;
{$if defined(cpu386) or defined(cpux64)}
const Mask:array[0..3] of TUInt32=($ffffffff,$ffffffff,$ffffffff,$00000000);
      cOne:array[0..3] of TScalar=(0.0,0.0,0.0,1.0);
asm
 xorps xmm2,xmm2
 movss xmm0,dword ptr [b+0]
 movss xmm1,dword ptr [b+4]
 movss xmm2,dword ptr [b+8]
 movlhps xmm0,xmm1
 shufps xmm0,xmm2,$88
//movups xmm0,dqword ptr [b]     // d c b a
{$ifdef cpu386}
 movups xmm1,dqword ptr [Mask]
 movups xmm2,dqword ptr [cOne]
{$else}
{$ifdef fpc}
 movups xmm1,dqword ptr [rip+Mask]
 movups xmm2,dqword ptr [rip+cOne]
{$else}
 movups xmm1,dqword ptr [rel Mask]
 movups xmm2,dqword ptr [rel cOne]
{$endif}
{$endif}
 andps xmm0,xmm1
 addps xmm0,xmm2
 movaps xmm1,xmm0               // d c b a
 movaps xmm2,xmm0               // d c b a
 movaps xmm3,xmm0               // d c b a
 shufps xmm0,xmm0,$00           // a a a a 00000000b
 shufps xmm1,xmm1,$55           // b b b b 01010101b
 shufps xmm2,xmm2,$aa           // c c c c 10101010b
 shufps xmm3,xmm3,$ff           // d d d d 11111111b
 movups xmm4,dqword ptr [a+0]
 movups xmm5,dqword ptr [a+16]
 movups xmm6,dqword ptr [a+32]
 movups xmm7,dqword ptr [a+48]
 mulps xmm0,xmm4
 mulps xmm1,xmm5
 mulps xmm2,xmm6
 mulps xmm3,xmm7
 addps xmm0,xmm1
 addps xmm2,xmm3
 addps xmm0,xmm2
 movaps xmm1,xmm0
 movaps xmm2,xmm0
 shufps xmm1,xmm1,$55
 shufps xmm2,xmm2,$aa
 movss dword ptr [result+0],xmm0
 movss dword ptr [result+4],xmm1
 movss dword ptr [result+8],xmm2
//movups dqword ptr [result],xmm0
end;
{$else}
begin
 result.x:=(a.RawComponents[0,0]*b.x)+(a.RawComponents[1,0]*b.y)+(a.RawComponents[2,0]*b.z)+a.RawComponents[3,0];
 result.y:=(a.RawComponents[0,1]*b.x)+(a.RawComponents[1,1]*b.y)+(a.RawComponents[2,1]*b.z)+a.RawComponents[3,1];
 result.z:=(a.RawComponents[0,2]*b.x)+(a.RawComponents[1,2]*b.y)+(a.RawComponents[2,2]*b.z)+a.RawComponents[3,2];
end;
{$ifend}

class operator TMatrix4x4.Multiply({$ifdef fpc}constref{$else}const{$endif} a:TVector3;{$ifdef fpc}constref{$else}const{$endif} b:TMatrix4x4):TVector3;
{$if defined(cpu386) or defined(cpux64)}
const Mask:array[0..3] of TUInt32=($ffffffff,$ffffffff,$ffffffff,$00000000);
      cOne:array[0..3] of TScalar=(0.0,0.0,0.0,1.0);
asm
 xorps xmm2,xmm2
 movss xmm0,dword ptr [a+0]
 movss xmm1,dword ptr [a+4]
 movss xmm2,dword ptr [a+8]
 movlhps xmm0,xmm1
 shufps xmm0,xmm2,$88
//movups xmm0,dqword ptr [a]     // d c b a
{$ifdef cpu386}
 movups xmm1,dqword ptr [Mask]
 movups xmm2,dqword ptr [cOne]
{$else}
{$ifdef fpc}
 movups xmm1,dqword ptr [rip+Mask]
 movups xmm2,dqword ptr [rip+cOne]
{$else}
 movups xmm1,dqword ptr [rel Mask]
 movups xmm2,dqword ptr [rel cOne]
{$endif}
{$endif}
 andps xmm0,xmm1
 addps xmm0,xmm2
 movaps xmm1,xmm0               // d c b a
 movaps xmm2,xmm0               // d c b a
 movaps xmm3,xmm0               // d c b a
 movups xmm4,dqword ptr [b+0]
 movups xmm5,dqword ptr [b+16]
 movups xmm6,dqword ptr [b+32]
 movups xmm7,dqword ptr [b+48]
 mulps xmm0,xmm4
 mulps xmm1,xmm5
 mulps xmm2,xmm6
 mulps xmm3,xmm7
 addps xmm0,xmm1
 addps xmm2,xmm3
 addps xmm0,xmm2
 movaps xmm1,xmm0
 movaps xmm2,xmm0
 shufps xmm1,xmm1,$55
 shufps xmm2,xmm2,$aa
 movss dword ptr [result+0],xmm0
 movss dword ptr [result+4],xmm1
 movss dword ptr [result+8],xmm2
//movups dqword ptr [result],xmm0
end;
{$else}
begin
 result.x:=(a.x*b.RawComponents[0,0])+(a.y*b.RawComponents[0,1])+(a.z*b.RawComponents[0,2])+b.RawComponents[0,3];
 result.y:=(a.x*b.RawComponents[1,0])+(a.y*b.RawComponents[1,1])+(a.z*b.RawComponents[1,2])+b.RawComponents[1,3];
 result.z:=(a.x*b.RawComponents[2,0])+(a.y*b.RawComponents[2,1])+(a.z*b.RawComponents[2,2])+b.RawComponents[2,3];
end;
{$ifend}

class operator TMatrix4x4.Multiply({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4;{$ifdef fpc}constref{$else}const{$endif} b:TVector4):TVector4;
{$if defined(cpu386) or defined(cpux64)}
asm
 movups xmm0,dqword ptr [b]     // d c b a
 movaps xmm1,xmm0               // d c b a
 movaps xmm2,xmm0               // d c b a
 movaps xmm3,xmm0               // d c b a
 shufps xmm0,xmm0,$00           // a a a a 00000000b
 shufps xmm1,xmm1,$55           // b b b b 01010101b
 shufps xmm2,xmm2,$aa           // c c c c 10101010b
 shufps xmm3,xmm3,$ff           // d d d d 11111111b
 movups xmm4,dqword ptr [a+0]
 movups xmm5,dqword ptr [a+16]
 movups xmm6,dqword ptr [a+32]
 movups xmm7,dqword ptr [a+48]
 mulps xmm0,xmm4
 mulps xmm1,xmm5
 mulps xmm2,xmm6
 mulps xmm3,xmm7
 addps xmm0,xmm1
 addps xmm2,xmm3
 addps xmm0,xmm2
 movups dqword ptr [result],xmm0
end;
{$else}
begin
 result.x:=(a.RawComponents[0,0]*b.x)+(a.RawComponents[1,0]*b.y)+(a.RawComponents[2,0]*b.z)+(a.RawComponents[3,0]*b.w);
 result.y:=(a.RawComponents[0,1]*b.x)+(a.RawComponents[1,1]*b.y)+(a.RawComponents[2,1]*b.z)+(a.RawComponents[3,1]*b.w);
 result.z:=(a.RawComponents[0,2]*b.x)+(a.RawComponents[1,2]*b.y)+(a.RawComponents[2,2]*b.z)+(a.RawComponents[3,2]*b.w);
 result.w:=(a.RawComponents[0,3]*b.x)+(a.RawComponents[1,3]*b.y)+(a.RawComponents[2,3]*b.z)+(a.RawComponents[3,3]*b.w);
end;
{$ifend}

class operator TMatrix4x4.Multiply({$ifdef fpc}constref{$else}const{$endif} a:TVector4;{$ifdef fpc}constref{$else}const{$endif} b:TMatrix4x4):TVector4;
{$if defined(cpu386) or defined(cpux64)}
asm
 movups xmm0,dqword ptr [a]     // d c b a
 movaps xmm1,xmm0               // d c b a
 movaps xmm2,xmm0               // d c b a
 movaps xmm3,xmm0               // d c b a
 movups xmm4,dqword ptr [b+0]
 movups xmm5,dqword ptr [b+16]
 movups xmm6,dqword ptr [b+32]
 movups xmm7,dqword ptr [b+48]
 mulps xmm0,xmm4
 mulps xmm1,xmm5
 mulps xmm2,xmm6
 mulps xmm3,xmm7
 addps xmm0,xmm1
 addps xmm2,xmm3
 addps xmm0,xmm2
 movups dqword ptr [result],xmm0
end;
{$else}
begin
 result.x:=(a.x*b.RawComponents[0,0])+(a.y*b.RawComponents[0,1])+(a.z*b.RawComponents[0,2])+(a.w*b.RawComponents[0,3]);
 result.y:=(a.x*b.RawComponents[1,0])+(a.y*b.RawComponents[1,1])+(a.z*b.RawComponents[1,2])+(a.w*b.RawComponents[1,3]);
 result.z:=(a.x*b.RawComponents[2,0])+(a.y*b.RawComponents[2,1])+(a.z*b.RawComponents[2,2])+(a.w*b.RawComponents[2,3]);
 result.w:=(a.x*b.RawComponents[3,0])+(a.y*b.RawComponents[3,1])+(a.z*b.RawComponents[3,2])+(a.w*b.RawComponents[3,3]);
end;
{$ifend}

class operator TMatrix4x4.Multiply({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4;{$ifdef fpc}constref{$else}const{$endif} b:TPlane):TPlane;
begin
 result.Normal:=a.Inverse.Transpose.MulBasis(b.Normal);
 result.Distance:=result.Normal.Dot(a*((b.Normal*b.Distance)));
end;

class operator TMatrix4x4.Multiply({$ifdef fpc}constref{$else}const{$endif} a:TPlane;{$ifdef fpc}constref{$else}const{$endif} b:TMatrix4x4):TPlane;
begin
 result:=b.Transpose*a;
end;

class operator TMatrix4x4.Divide({$ifdef fpc}constref{$else}const{$endif} a,b:TMatrix4x4):TMatrix4x4;
begin
 result:=a*b.Inverse;
end;

class operator TMatrix4x4.Divide({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4;{$ifdef fpc}constref{$else}const{$endif} b:TScalar):TMatrix4x4;
{$if defined(cpu386) or defined(cpux64)}
asm
 movups xmm0,dqword ptr [a+0]
 movups xmm1,dqword ptr [a+16]
 movups xmm2,dqword ptr [a+32]
 movups xmm3,dqword ptr [a+48]
 movss xmm4,dword ptr [b]
 shufps xmm4,xmm4,$00
 divps xmm0,xmm4
 divps xmm1,xmm4
 divps xmm2,xmm4
 divps xmm3,xmm4
 movups dqword ptr [result+0],xmm0
 movups dqword ptr [result+16],xmm1
 movups dqword ptr [result+32],xmm2
 movups dqword ptr [result+48],xmm3
end;
{$else}
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]/b;
 result.RawComponents[0,1]:=a.RawComponents[0,1]/b;
 result.RawComponents[0,2]:=a.RawComponents[0,2]/b;
 result.RawComponents[0,3]:=a.RawComponents[0,3]/b;
 result.RawComponents[1,0]:=a.RawComponents[1,0]/b;
 result.RawComponents[1,1]:=a.RawComponents[1,1]/b;
 result.RawComponents[1,2]:=a.RawComponents[1,2]/b;
 result.RawComponents[1,3]:=a.RawComponents[1,3]/b;
 result.RawComponents[2,0]:=a.RawComponents[2,0]/b;
 result.RawComponents[2,1]:=a.RawComponents[2,1]/b;
 result.RawComponents[2,2]:=a.RawComponents[2,2]/b;
 result.RawComponents[2,3]:=a.RawComponents[2,3]/b;
 result.RawComponents[3,0]:=a.RawComponents[3,0]/b;
 result.RawComponents[3,1]:=a.RawComponents[3,1]/b;
 result.RawComponents[3,2]:=a.RawComponents[3,2]/b;
 result.RawComponents[3,3]:=a.RawComponents[3,3]/b;
end;
{$ifend}

class operator TMatrix4x4.Divide({$ifdef fpc}constref{$else}const{$endif} a:TScalar;{$ifdef fpc}constref{$else}const{$endif} b:TMatrix4x4):TMatrix4x4;
{$if defined(cpu386) or defined(cpux64)}
asm
 movss xmm0,dword ptr [a]
 shufps xmm0,xmm0,$00
 movaps xmm1,xmm0
 movaps xmm2,xmm0
 movaps xmm3,xmm0
 movups xmm4,dqword ptr [b+0]
 movups xmm5,dqword ptr [b+16]
 movups xmm6,dqword ptr [b+32]
 movups xmm7,dqword ptr [b+48]
 divps xmm0,xmm4
 divps xmm1,xmm5
 divps xmm2,xmm6
 divps xmm3,xmm7
 movups dqword ptr [result+0],xmm0
 movups dqword ptr [result+16],xmm1
 movups dqword ptr [result+32],xmm2
 movups dqword ptr [result+48],xmm3
end;
{$else}
begin
 result.RawComponents[0,0]:=a/b.RawComponents[0,0];
 result.RawComponents[0,1]:=a/b.RawComponents[0,1];
 result.RawComponents[0,2]:=a/b.RawComponents[0,2];
 result.RawComponents[0,3]:=a/b.RawComponents[0,3];
 result.RawComponents[1,0]:=a/b.RawComponents[1,0];
 result.RawComponents[1,1]:=a/b.RawComponents[1,1];
 result.RawComponents[1,2]:=a/b.RawComponents[1,2];
 result.RawComponents[1,3]:=a/b.RawComponents[1,3];
 result.RawComponents[2,0]:=a/b.RawComponents[2,0];
 result.RawComponents[2,1]:=a/b.RawComponents[2,1];
 result.RawComponents[2,2]:=a/b.RawComponents[2,2];
 result.RawComponents[2,3]:=a/b.RawComponents[2,3];
 result.RawComponents[3,0]:=a/b.RawComponents[3,0];
 result.RawComponents[3,1]:=a/b.RawComponents[3,1];
 result.RawComponents[3,2]:=a/b.RawComponents[3,2];
 result.RawComponents[3,3]:=a/b.RawComponents[3,3];
end;
{$ifend}

class operator TMatrix4x4.IntDivide({$ifdef fpc}constref{$else}const{$endif} a,b:TMatrix4x4):TMatrix4x4;
begin
 result:=a*b.Inverse;
end;

class operator TMatrix4x4.IntDivide({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4;{$ifdef fpc}constref{$else}const{$endif} b:TScalar):TMatrix4x4;
{$if defined(cpu386) or defined(cpux64)}
asm
 movups xmm0,dqword ptr [a+0]
 movups xmm1,dqword ptr [a+16]
 movups xmm2,dqword ptr [a+32]
 movups xmm3,dqword ptr [a+48]
 movss xmm4,dword ptr [b]
 shufps xmm4,xmm4,$00
 divps xmm0,xmm4
 divps xmm1,xmm4
 divps xmm2,xmm4
 divps xmm3,xmm4
 movups dqword ptr [result+0],xmm0
 movups dqword ptr [result+16],xmm1
 movups dqword ptr [result+32],xmm2
 movups dqword ptr [result+48],xmm3
end;
{$else}
begin
 result.RawComponents[0,0]:=a.RawComponents[0,0]/b;
 result.RawComponents[0,1]:=a.RawComponents[0,1]/b;
 result.RawComponents[0,2]:=a.RawComponents[0,2]/b;
 result.RawComponents[0,3]:=a.RawComponents[0,3]/b;
 result.RawComponents[1,0]:=a.RawComponents[1,0]/b;
 result.RawComponents[1,1]:=a.RawComponents[1,1]/b;
 result.RawComponents[1,2]:=a.RawComponents[1,2]/b;
 result.RawComponents[1,3]:=a.RawComponents[1,3]/b;
 result.RawComponents[2,0]:=a.RawComponents[2,0]/b;
 result.RawComponents[2,1]:=a.RawComponents[2,1]/b;
 result.RawComponents[2,2]:=a.RawComponents[2,2]/b;
 result.RawComponents[2,3]:=a.RawComponents[2,3]/b;
 result.RawComponents[3,0]:=a.RawComponents[3,0]/b;
 result.RawComponents[3,1]:=a.RawComponents[3,1]/b;
 result.RawComponents[3,2]:=a.RawComponents[3,2]/b;
 result.RawComponents[3,3]:=a.RawComponents[3,3]/b;
end;
{$ifend}

class operator TMatrix4x4.IntDivide({$ifdef fpc}constref{$else}const{$endif} a:TScalar;{$ifdef fpc}constref{$else}const{$endif} b:TMatrix4x4):TMatrix4x4;
{$if defined(cpu386) or defined(cpux64)}
asm
 movss xmm0,dword ptr [a]
 shufps xmm0,xmm0,$00
 movaps xmm1,xmm0
 movaps xmm2,xmm0
 movaps xmm3,xmm0
 movups xmm4,dqword ptr [b+0]
 movups xmm5,dqword ptr [b+16]
 movups xmm6,dqword ptr [b+32]
 movups xmm7,dqword ptr [b+48]
 divps xmm0,xmm4
 divps xmm1,xmm5
 divps xmm2,xmm6
 divps xmm3,xmm7
 movups dqword ptr [result+0],xmm0
 movups dqword ptr [result+16],xmm1
 movups dqword ptr [result+32],xmm2
 movups dqword ptr [result+48],xmm3
end;
{$else}
begin
 result.RawComponents[0,0]:=a/b.RawComponents[0,0];
 result.RawComponents[0,1]:=a/b.RawComponents[0,1];
 result.RawComponents[0,2]:=a/b.RawComponents[0,2];
 result.RawComponents[0,3]:=a/b.RawComponents[0,3];
 result.RawComponents[1,0]:=a/b.RawComponents[1,0];
 result.RawComponents[1,1]:=a/b.RawComponents[1,1];
 result.RawComponents[1,2]:=a/b.RawComponents[1,2];
 result.RawComponents[1,3]:=a/b.RawComponents[1,3];
 result.RawComponents[2,0]:=a/b.RawComponents[2,0];
 result.RawComponents[2,1]:=a/b.RawComponents[2,1];
 result.RawComponents[2,2]:=a/b.RawComponents[2,2];
 result.RawComponents[2,3]:=a/b.RawComponents[2,3];
 result.RawComponents[3,0]:=a/b.RawComponents[3,0];
 result.RawComponents[3,1]:=a/b.RawComponents[3,1];
 result.RawComponents[3,2]:=a/b.RawComponents[3,2];
 result.RawComponents[3,3]:=a/b.RawComponents[3,3];
end;
{$ifend}

class operator TMatrix4x4.Modulus({$ifdef fpc}constref{$else}const{$endif} a,b:TMatrix4x4):TMatrix4x4;
begin
 result.RawComponents[0,0]:=Modulo(a.RawComponents[0,0],b.RawComponents[0,0]);
 result.RawComponents[0,1]:=Modulo(a.RawComponents[0,1],b.RawComponents[0,1]);
 result.RawComponents[0,2]:=Modulo(a.RawComponents[0,2],b.RawComponents[0,2]);
 result.RawComponents[0,3]:=Modulo(a.RawComponents[0,3],b.RawComponents[0,3]);
 result.RawComponents[1,0]:=Modulo(a.RawComponents[1,0],b.RawComponents[1,0]);
 result.RawComponents[1,1]:=Modulo(a.RawComponents[1,1],b.RawComponents[1,1]);
 result.RawComponents[1,2]:=Modulo(a.RawComponents[1,2],b.RawComponents[1,2]);
 result.RawComponents[1,3]:=Modulo(a.RawComponents[1,3],b.RawComponents[1,3]);
 result.RawComponents[2,0]:=Modulo(a.RawComponents[2,0],b.RawComponents[2,0]);
 result.RawComponents[2,1]:=Modulo(a.RawComponents[2,1],b.RawComponents[2,1]);
 result.RawComponents[2,2]:=Modulo(a.RawComponents[2,2],b.RawComponents[2,2]);
 result.RawComponents[2,3]:=Modulo(a.RawComponents[2,3],b.RawComponents[2,3]);
 result.RawComponents[3,0]:=Modulo(a.RawComponents[3,0],b.RawComponents[3,0]);
 result.RawComponents[3,1]:=Modulo(a.RawComponents[3,1],b.RawComponents[3,1]);
 result.RawComponents[3,2]:=Modulo(a.RawComponents[3,2],b.RawComponents[3,2]);
 result.RawComponents[3,3]:=Modulo(a.RawComponents[3,3],b.RawComponents[3,3]);
end;

class operator TMatrix4x4.Modulus({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4;{$ifdef fpc}constref{$else}const{$endif} b:TScalar):TMatrix4x4;
begin
 result.RawComponents[0,0]:=Modulo(a.RawComponents[0,0],b);
 result.RawComponents[0,1]:=Modulo(a.RawComponents[0,1],b);
 result.RawComponents[0,2]:=Modulo(a.RawComponents[0,2],b);
 result.RawComponents[0,3]:=Modulo(a.RawComponents[0,3],b);
 result.RawComponents[1,0]:=Modulo(a.RawComponents[1,0],b);
 result.RawComponents[1,1]:=Modulo(a.RawComponents[1,1],b);
 result.RawComponents[1,2]:=Modulo(a.RawComponents[1,2],b);
 result.RawComponents[1,3]:=Modulo(a.RawComponents[1,3],b);
 result.RawComponents[2,0]:=Modulo(a.RawComponents[2,0],b);
 result.RawComponents[2,1]:=Modulo(a.RawComponents[2,1],b);
 result.RawComponents[2,2]:=Modulo(a.RawComponents[2,2],b);
 result.RawComponents[2,3]:=Modulo(a.RawComponents[2,3],b);
 result.RawComponents[3,0]:=Modulo(a.RawComponents[3,0],b);
 result.RawComponents[3,1]:=Modulo(a.RawComponents[3,1],b);
 result.RawComponents[3,2]:=Modulo(a.RawComponents[3,2],b);
 result.RawComponents[3,3]:=Modulo(a.RawComponents[3,3],b);
end;

class operator TMatrix4x4.Modulus({$ifdef fpc}constref{$else}const{$endif} a:TScalar;{$ifdef fpc}constref{$else}const{$endif} b:TMatrix4x4):TMatrix4x4;
begin
 result.RawComponents[0,0]:=Modulo(a,b.RawComponents[0,0]);
 result.RawComponents[0,1]:=Modulo(a,b.RawComponents[0,1]);
 result.RawComponents[0,2]:=Modulo(a,b.RawComponents[0,2]);
 result.RawComponents[0,3]:=Modulo(a,b.RawComponents[0,3]);
 result.RawComponents[1,0]:=Modulo(a,b.RawComponents[1,0]);
 result.RawComponents[1,1]:=Modulo(a,b.RawComponents[1,1]);
 result.RawComponents[1,2]:=Modulo(a,b.RawComponents[1,2]);
 result.RawComponents[1,3]:=Modulo(a,b.RawComponents[1,3]);
 result.RawComponents[2,0]:=Modulo(a,b.RawComponents[2,0]);
 result.RawComponents[2,1]:=Modulo(a,b.RawComponents[2,1]);
 result.RawComponents[2,2]:=Modulo(a,b.RawComponents[2,2]);
 result.RawComponents[2,3]:=Modulo(a,b.RawComponents[2,3]);
 result.RawComponents[3,0]:=Modulo(a,b.RawComponents[3,0]);
 result.RawComponents[3,1]:=Modulo(a,b.RawComponents[3,1]);
 result.RawComponents[3,2]:=Modulo(a,b.RawComponents[3,2]);
 result.RawComponents[3,3]:=Modulo(a,b.RawComponents[3,3]);
end;

class operator TMatrix4x4.Negative({$ifdef fpc}constref{$else}const{$endif} a:TMatrix4x4):TMatrix4x4;
{$if defined(cpu386) or defined(cpux64)}
asm
 xorps xmm0,xmm0
 xorps xmm1,xmm1
 xorps xmm2,xmm2
 xorps xmm3,xmm3
 movups xmm4,dqword ptr [a+0]
 movups xmm5,dqword ptr [a+16]
 movups xmm6,dqword ptr [a+32]
 movups xmm7,dqword ptr [a+48]
 subps xmm0,xmm4
 subps xmm1,xmm5
 subps xmm2,xmm6
 subps xmm3,xmm7
 movups dqword ptr [result+0],xmm0
 movups dqword ptr [result+16],xmm1
 movups dqword ptr [result+32],xmm2
 movups dqword ptr [result+48],xmm3
end;
{$else}
begin
 result.RawComponents[0,0]:=-a.RawComponents[0,0];
 result.RawComponents[0,1]:=-a.RawComponents[0,1];
 result.RawComponents[0,2]:=-a.RawComponents[0,2];
 result.RawComponents[0,3]:=-a.RawComponents[0,3];
 result.RawComponents[1,0]:=-a.RawComponents[1,0];
 result.RawComponents[1,1]:=-a.RawComponents[1,1];
 result.RawComponents[1,2]:=-a.RawComponents[1,2];
 result.RawComponents[1,3]:=-a.RawComponents[1,3];
 result.RawComponents[2,0]:=-a.RawComponents[2,0];
 result.RawComponents[2,1]:=-a.RawComponents[2,1];
 result.RawComponents[2,2]:=-a.RawComponents[2,2];
 result.RawComponents[2,3]:=-a.RawComponents[2,3];
 result.RawComponents[3,0]:=-a.RawComponents[3,0];
 result.RawComponents[3,1]:=-a.RawComponents[3,1];
 result.RawComponents[3,2]:=-a.RawComponents[3,2];
 result.RawComponents[3,3]:=-a.RawComponents[3,3];
end;
{$ifend}

class operator TMatrix4x4.Positive(const a:TMatrix4x4):TMatrix4x4;
begin
 result:=a;
end;

function TMatrix4x4.GetComponent(const pIndexA,pIndexB:TInt32):TScalar;
begin
 result:=RawComponents[pIndexA,pIndexB];
end;

procedure TMatrix4x4.SetComponent(const pIndexA,pIndexB:TInt32;const pValue:TScalar);
begin
 RawComponents[pIndexA,pIndexB]:=pValue;
end;

function TMatrix4x4.GetColumn(const pIndex:TInt32):TVector4;
begin
 result.x:=RawComponents[pIndex,0];
 result.y:=RawComponents[pIndex,1];
 result.z:=RawComponents[pIndex,2];
 result.w:=RawComponents[pIndex,3];
end;

procedure TMatrix4x4.SetColumn(const pIndex:TInt32;const pValue:TVector4);
begin
 RawComponents[pIndex,0]:=pValue.x;
 RawComponents[pIndex,1]:=pValue.y;
 RawComponents[pIndex,2]:=pValue.z;
 RawComponents[pIndex,3]:=pValue.w;
end;

function TMatrix4x4.GetRow(const pIndex:TInt32):TVector4;
begin
 result.x:=RawComponents[0,pIndex];
 result.y:=RawComponents[1,pIndex];
 result.z:=RawComponents[2,pIndex];
 result.w:=RawComponents[3,pIndex];
end;

procedure TMatrix4x4.SetRow(const pIndex:TInt32;const pValue:TVector4);
begin
 RawComponents[0,pIndex]:=pValue.x;
 RawComponents[1,pIndex]:=pValue.y;
 RawComponents[2,pIndex]:=pValue.z;
 RawComponents[3,pIndex]:=pValue.w;
end;

function TMatrix4x4.Determinant:TScalar;
{$if defined(cpu386)}
asm
 movups xmm0,dqword ptr [eax+32]
 movups xmm1,dqword ptr [eax+48]
 movups xmm2,dqword ptr [eax+16]
 movaps xmm3,xmm0
 movaps xmm4,xmm0
 movaps xmm6,xmm1
 movaps xmm7,xmm2
 shufps xmm0,xmm0,$1b // 00011011b
 shufps xmm1,xmm1,$b1 // 10110001b
 shufps xmm2,xmm2,$4e // 01001110b
 shufps xmm7,xmm7,$39 // 00111001b
 mulps xmm0,xmm1
 shufps xmm3,xmm3,$7d // 01111101b
 shufps xmm6,xmm6,$0a // 00001010b
 movaps xmm5,xmm0
 shufps xmm0,xmm0,$4e // 01001110b
 shufps xmm4,xmm4,$0a // 00001010b
 shufps xmm1,xmm1,$28 // 00101000b
 subps xmm5,xmm0
 mulps xmm3,xmm6
 mulps xmm4,xmm1
 mulps xmm5,xmm2
 shufps xmm2,xmm2,$39 // 00111001b
 subps xmm3,xmm4
 movaps xmm0,xmm3
 shufps xmm0,xmm0,$39 // 00111001b
 mulps xmm3,xmm2
 mulps xmm0,xmm7
 addps xmm5,xmm3
 subps xmm5,xmm0
 movups xmm6,dqword ptr [eax+0]
 mulps xmm5,xmm6
 movhlps xmm7,xmm5
 addps xmm5,xmm7
 movaps xmm6,xmm5
 shufps xmm6,xmm6,$01
 addss xmm5,xmm6
 movss dword ptr [result],xmm5
end;
{$elseif defined(cpux64)}
asm
{$ifdef Windows}
 movups xmm0,dqword ptr [rcx+32]
 movups xmm1,dqword ptr [rcx+48]
 movups xmm2,dqword ptr [rcx+16]
{$else}
 movups xmm0,dqword ptr [rdi+32]
 movups xmm1,dqword ptr [rdi+48]
 movups xmm2,dqword ptr [rdi+16]
{$endif}
 movaps xmm3,xmm0
 movaps xmm4,xmm0
 movaps xmm6,xmm1
 movaps xmm7,xmm2
 shufps xmm0,xmm0,$1b // 00011011b
 shufps xmm1,xmm1,$b1 // 10110001b
 shufps xmm2,xmm2,$4e // 01001110b
 shufps xmm7,xmm7,$39 // 00111001b
 mulps xmm0,xmm1
 shufps xmm3,xmm3,$7d // 01111101b
 shufps xmm6,xmm6,$0a // 00001010b
 movaps xmm5,xmm0
 shufps xmm0,xmm0,$4e // 01001110b
 shufps xmm4,xmm4,$0a // 00001010b
 shufps xmm1,xmm1,$28 // 00101000b
 subps xmm5,xmm0
 mulps xmm3,xmm6
 mulps xmm4,xmm1
 mulps xmm5,xmm2
 shufps xmm2,xmm2,$39 // 00111001b
 subps xmm3,xmm4
 movaps xmm0,xmm3
 shufps xmm0,xmm0,$39 // 00111001b
 mulps xmm3,xmm2
 mulps xmm0,xmm7
 addps xmm5,xmm3
 subps xmm5,xmm0
{$ifdef Windows}
 movups xmm6,dqword ptr [rcx+0]
{$else}
 movups xmm6,dqword ptr [rdi+0]
{$endif}
 mulps xmm5,xmm6
 movhlps xmm7,xmm5
 addps xmm5,xmm7
 movaps xmm6,xmm5
 shufps xmm6,xmm6,$01
 addss xmm5,xmm6
 movss dword ptr [result],xmm5
end;
{$else}
begin
 result:=(RawComponents[0,0]*((((RawComponents[1,1]*RawComponents[2,2]*RawComponents[3,3])-(RawComponents[1,1]*RawComponents[2,3]*RawComponents[3,2]))-(RawComponents[2,1]*RawComponents[1,2]*RawComponents[3,3])+(RawComponents[2,1]*RawComponents[1,3]*RawComponents[3,2])+(RawComponents[3,1]*RawComponents[1,2]*RawComponents[2,3]))-(RawComponents[3,1]*RawComponents[1,3]*RawComponents[2,2])))+
         (RawComponents[0,1]*(((((-(RawComponents[1,0]*RawComponents[2,2]*RawComponents[3,3]))+(RawComponents[1,0]*RawComponents[2,3]*RawComponents[3,2])+(RawComponents[2,0]*RawComponents[1,2]*RawComponents[3,3]))-(RawComponents[2,0]*RawComponents[1,3]*RawComponents[3,2]))-(RawComponents[3,0]*RawComponents[1,2]*RawComponents[2,3]))+(RawComponents[3,0]*RawComponents[1,3]*RawComponents[2,2])))+
         (RawComponents[0,2]*(((((RawComponents[1,0]*RawComponents[2,1]*RawComponents[3,3])-(RawComponents[1,0]*RawComponents[2,3]*RawComponents[3,1]))-(RawComponents[2,0]*RawComponents[1,1]*RawComponents[3,3]))+(RawComponents[2,0]*RawComponents[1,3]*RawComponents[3,1])+(RawComponents[3,0]*RawComponents[1,1]*RawComponents[2,3]))-(RawComponents[3,0]*RawComponents[1,3]*RawComponents[2,1])))+
         (RawComponents[0,3]*(((((-(RawComponents[1,0]*RawComponents[2,1]*RawComponents[3,2]))+(RawComponents[1,0]*RawComponents[2,2]*RawComponents[3,1])+(RawComponents[2,0]*RawComponents[1,1]*RawComponents[3,2]))-(RawComponents[2,0]*RawComponents[1,2]*RawComponents[3,1]))-(RawComponents[3,0]*RawComponents[1,1]*RawComponents[2,2]))+(RawComponents[3,0]*RawComponents[1,2]*RawComponents[2,1])));
end;
{$ifend}

function TMatrix4x4.SimpleInverse:TMatrix4x4;
begin
 result.RawComponents[0,0]:=RawComponents[0,0];
 result.RawComponents[0,1]:=RawComponents[1,0];
 result.RawComponents[0,2]:=RawComponents[2,0];
 result.RawComponents[0,3]:=RawComponents[0,3];
 result.RawComponents[1,0]:=RawComponents[0,1];
 result.RawComponents[1,1]:=RawComponents[1,1];
 result.RawComponents[1,2]:=RawComponents[2,1];
 result.RawComponents[1,3]:=RawComponents[1,3];
 result.RawComponents[2,0]:=RawComponents[0,2];
 result.RawComponents[2,1]:=RawComponents[1,2];
 result.RawComponents[2,2]:=RawComponents[2,2];
 result.RawComponents[2,3]:=RawComponents[2,3];
 result.RawComponents[3,0]:=-PVector3(pointer(@RawComponents[3,0]))^.Dot(TVector3.Create(RawComponents[0,0],RawComponents[0,1],RawComponents[0,2]));
 result.RawComponents[3,1]:=-PVector3(pointer(@RawComponents[3,0]))^.Dot(TVector3.Create(RawComponents[1,0],RawComponents[1,1],RawComponents[1,2]));
 result.RawComponents[3,2]:=-PVector3(pointer(@RawComponents[3,0]))^.Dot(TVector3.Create(RawComponents[2,0],RawComponents[2,1],RawComponents[2,2]));
 result.RawComponents[3,3]:=RawComponents[3,3];
end;

function TMatrix4x4.Inverse:TMatrix4x4;
{$if defined(cpu386)}
asm
 mov ecx,esp
 and esp,$fffffff0
 sub esp,$b0
 movlps xmm2,qword ptr [eax+8]
 movlps xmm4,qword ptr [eax+40]
 movhps xmm2,qword ptr [eax+24]
 movhps xmm4,qword ptr [eax+56]
 movlps xmm3,qword ptr [eax+32]
 movlps xmm1,qword ptr [eax]
 movhps xmm3,qword ptr [eax+48]
 movhps xmm1,qword ptr [eax+16]
 movaps xmm5,xmm2
 shufps xmm5,xmm4,$88
 shufps xmm4,xmm2,$dd
 movaps xmm2,xmm4
 mulps xmm2,xmm5
 shufps xmm2,xmm2,$b1
 movaps xmm6,xmm2
 shufps xmm6,xmm6,$4e
 movaps xmm7,xmm3
 shufps xmm3,xmm1,$dd
 shufps xmm1,xmm7,$88
 movaps xmm7,xmm3
 mulps xmm3,xmm6
 mulps xmm6,xmm1
 movaps xmm0,xmm6
 movaps xmm6,xmm7
 mulps xmm7,xmm2
 mulps xmm2,xmm1
 subps xmm3,xmm7
 movaps xmm7,xmm6
 mulps xmm7,xmm5
 shufps xmm5,xmm5,$4e
 shufps xmm7,xmm7,$b1
 movaps dqword ptr [esp+16],xmm2
 movaps xmm2,xmm4
 mulps xmm2,xmm7
 addps xmm2,xmm3
 movaps xmm3,xmm7
 shufps xmm7,xmm7,$4e
 mulps xmm3,xmm1
 movaps dqword ptr [esp+32],xmm3
 movaps xmm3,xmm4
 mulps xmm3,xmm7
 mulps xmm7,xmm1
 subps xmm2,xmm3
 movaps xmm3,xmm6
 shufps xmm3,xmm3,$4e
 mulps xmm3,xmm4
 shufps xmm3,xmm3,$b1
 movaps dqword ptr [esp+48],xmm7
 movaps xmm7,xmm5
 mulps xmm5,xmm3
 addps xmm5,xmm2
 movaps xmm2,xmm3
 shufps xmm3,xmm3,$4e
 mulps xmm2,xmm1
 movaps dqword ptr [esp+64],xmm4
 movaps xmm4,xmm7
 mulps xmm7,xmm3
 mulps xmm3,xmm1
 subps xmm5,xmm7
 subps xmm3,xmm2
 movaps xmm2,xmm1
 mulps xmm1,xmm5
 shufps xmm3,xmm3,$4e
 movaps xmm7,xmm1
 shufps xmm1,xmm1,$4e
 movaps dqword ptr [esp],xmm5
 addps xmm1,xmm7
 movaps xmm5,xmm1
 shufps xmm1,xmm1,$b1
 addss xmm1,xmm5
 movaps xmm5,xmm6
 mulps xmm5,xmm2
 shufps xmm5,xmm5,$b1
 movaps xmm7,xmm5
 shufps xmm5,xmm5,$4e
 movaps dqword ptr [esp+80],xmm4
 movaps xmm4,dqword ptr [esp+64]
 movaps dqword ptr [esp+64],xmm6
 movaps xmm6,xmm4
 mulps xmm6,xmm7
 addps xmm6,xmm3
 movaps xmm3,xmm4
 mulps xmm3,xmm5
 subps xmm3,xmm6
 movaps xmm6,xmm4
 mulps xmm6,xmm2
 shufps xmm6,xmm6,$b1
 movaps dqword ptr [esp+112],xmm5
 movaps xmm5,dqword ptr [esp+64]
 movaps dqword ptr [esp+128],xmm7
 movaps xmm7,xmm6
 mulps xmm7,xmm5
 addps xmm7,xmm3
 movaps xmm3,xmm6
 shufps xmm3,xmm3,$4e
 movaps dqword ptr [esp+144],xmm4
 movaps xmm4,xmm5
 mulps xmm5,xmm3
 movaps dqword ptr [esp+160],xmm4
 movaps xmm4,xmm6
 movaps xmm6,xmm7
 subps xmm6,xmm5
 movaps xmm5,xmm0
 movaps xmm7,dqword ptr [esp+16]
 subps xmm5,xmm7
 shufps xmm5,xmm5,$4e
 movaps xmm7,dqword ptr [esp+80]
 mulps xmm4,xmm7
 mulps xmm3,xmm7
 subps xmm5,xmm4
 mulps xmm2,xmm7
 addps xmm3,xmm5
 shufps xmm2,xmm2,$b1
 movaps xmm4,xmm2
 shufps xmm4,xmm4,$4e
 movaps xmm5,dqword ptr [esp+144]
 movaps xmm0,xmm6
 movaps xmm6,xmm5
 mulps xmm5,xmm2
 mulps xmm6,xmm4
 addps xmm5,xmm3
 movaps xmm3,xmm4
 movaps xmm4,xmm5
 subps xmm4,xmm6
 movaps xmm5,dqword ptr [esp+48]
 movaps xmm6,dqword ptr [esp+32]
 subps xmm5,xmm6
 shufps xmm5,xmm5,$4e
 movaps xmm6,[esp+128]
 mulps xmm6,xmm7
 subps xmm6,xmm5
 movaps xmm5,dqword ptr [esp+112]
 mulps xmm7,xmm5
 subps xmm6,xmm7
 movaps xmm5,dqword ptr [esp+160]
 mulps xmm2,xmm5
 mulps xmm5,xmm3
 subps xmm6,xmm2
 movaps xmm2,xmm5
 addps xmm2,xmm6
 movaps xmm6,xmm0
 movaps xmm0,xmm1
 movaps xmm1,dqword ptr [esp]
 movaps xmm3,xmm0
 rcpss xmm5,xmm0
 mulss xmm0,xmm5
 mulss xmm0,xmm5
 addss xmm5,xmm5
 subss xmm5,xmm0
 movaps xmm0,xmm5
 addss xmm5,xmm5
 mulss xmm0,xmm0
 mulss xmm3,xmm0
 subss xmm5,xmm3
 shufps xmm5,xmm5,$00
 mulps xmm1,xmm5
 mulps xmm4,xmm5
 mulps xmm6,xmm5
 mulps xmm5,xmm2
 movups dqword ptr [result+0],xmm1
 movups dqword ptr [result+16],xmm4
 movups dqword ptr [result+32],xmm6
 movups dqword ptr [result+48],xmm5
 mov esp,ecx
end;
{$elseif defined(cpux64)}
asm
 mov r9,rsp
 mov r8,$fffffffffffffff0
 and rsp,r8
 sub rsp,$b0
{$ifdef Windows}
 movlps xmm2,qword ptr [rcx+8]
 movlps xmm4,qword ptr [rcx+40]
 movhps xmm2,qword ptr [rcx+24]
 movhps xmm4,qword ptr [rcx+56]
 movlps xmm3,qword ptr [rcx+32]
 movlps xmm1,qword ptr [rcx]
 movhps xmm3,qword ptr [rcx+48]
 movhps xmm1,qword ptr [rcx+16]
{$else}
 movlps xmm2,qword ptr [rdi+8]
 movlps xmm4,qword ptr [rdi+40]
 movhps xmm2,qword ptr [rdi+24]
 movhps xmm4,qword ptr [rdi+56]
 movlps xmm3,qword ptr [rdi+32]
 movlps xmm1,qword ptr [rdi]
 movhps xmm3,qword ptr [rdi+48]
 movhps xmm1,qword ptr [rdi+16]
{$endif}
 movaps xmm5,xmm2
 shufps xmm5,xmm4,$88
 shufps xmm4,xmm2,$dd
 movaps xmm2,xmm4
 mulps xmm2,xmm5
 shufps xmm2,xmm2,$b1
 movaps xmm6,xmm2
 shufps xmm6,xmm6,$4e
 movaps xmm7,xmm3
 shufps xmm3,xmm1,$dd
 shufps xmm1,xmm7,$88
 movaps xmm7,xmm3
 mulps xmm3,xmm6
 mulps xmm6,xmm1
 movaps xmm0,xmm6
 movaps xmm6,xmm7
 mulps xmm7,xmm2
 mulps xmm2,xmm1
 subps xmm3,xmm7
 movaps xmm7,xmm6
 mulps xmm7,xmm5
 shufps xmm5,xmm5,$4e
 shufps xmm7,xmm7,$b1
 movaps dqword ptr [rsp+16],xmm2
 movaps xmm2,xmm4
 mulps xmm2,xmm7
 addps xmm2,xmm3
 movaps xmm3,xmm7
 shufps xmm7,xmm7,$4e
 mulps xmm3,xmm1
 movaps dqword ptr [rsp+32],xmm3
 movaps xmm3,xmm4
 mulps xmm3,xmm7
 mulps xmm7,xmm1
 subps xmm2,xmm3
 movaps xmm3,xmm6
 shufps xmm3,xmm3,$4e
 mulps xmm3,xmm4
 shufps xmm3,xmm3,$b1
 movaps dqword ptr [rsp+48],xmm7
 movaps xmm7,xmm5
 mulps xmm5,xmm3
 addps xmm5,xmm2
 movaps xmm2,xmm3
 shufps xmm3,xmm3,$4e
 mulps xmm2,xmm1
 movaps dqword ptr [rsp+64],xmm4
 movaps xmm4,xmm7
 mulps xmm7,xmm3
 mulps xmm3,xmm1
 subps xmm5,xmm7
 subps xmm3,xmm2
 movaps xmm2,xmm1
 mulps xmm1,xmm5
 shufps xmm3,xmm3,$4e
 movaps xmm7,xmm1
 shufps xmm1,xmm1,$4e
 movaps dqword ptr [rsp],xmm5
 addps xmm1,xmm7
 movaps xmm5,xmm1
 shufps xmm1,xmm1,$b1
 addss xmm1,xmm5
 movaps xmm5,xmm6
 mulps xmm5,xmm2
 shufps xmm5,xmm5,$b1
 movaps xmm7,xmm5
 shufps xmm5,xmm5,$4e
 movaps dqword ptr [rsp+80],xmm4
 movaps xmm4,dqword ptr [rsp+64]
 movaps dqword ptr [rsp+64],xmm6
 movaps xmm6,xmm4
 mulps xmm6,xmm7
 addps xmm6,xmm3
 movaps xmm3,xmm4
 mulps xmm3,xmm5
 subps xmm3,xmm6
 movaps xmm6,xmm4
 mulps xmm6,xmm2
 shufps xmm6,xmm6,$b1
 movaps dqword ptr [rsp+112],xmm5
 movaps xmm5,dqword ptr [rsp+64]
 movaps dqword ptr [rsp+128],xmm7
 movaps xmm7,xmm6
 mulps xmm7,xmm5
 addps xmm7,xmm3
 movaps xmm3,xmm6
 shufps xmm3,xmm3,$4e
 movaps dqword ptr [rsp+144],xmm4
 movaps xmm4,xmm5
 mulps xmm5,xmm3
 movaps dqword ptr [rsp+160],xmm4
 movaps xmm4,xmm6
 movaps xmm6,xmm7
 subps xmm6,xmm5
 movaps xmm5,xmm0
 movaps xmm7,dqword ptr [rsp+16]
 subps xmm5,xmm7
 shufps xmm5,xmm5,$4e
 movaps xmm7,dqword ptr [rsp+80]
 mulps xmm4,xmm7
 mulps xmm3,xmm7
 subps xmm5,xmm4
 mulps xmm2,xmm7
 addps xmm3,xmm5
 shufps xmm2,xmm2,$b1
 movaps xmm4,xmm2
 shufps xmm4,xmm4,$4e
 movaps xmm5,dqword ptr [rsp+144]
 movaps xmm0,xmm6
 movaps xmm6,xmm5
 mulps xmm5,xmm2
 mulps xmm6,xmm4
 addps xmm5,xmm3
 movaps xmm3,xmm4
 movaps xmm4,xmm5
 subps xmm4,xmm6
 movaps xmm5,dqword ptr [rsp+48]
 movaps xmm6,dqword ptr [rsp+32]
 subps xmm5,xmm6
 shufps xmm5,xmm5,$4e
 movaps xmm6,[esp+128]
 mulps xmm6,xmm7
 subps xmm6,xmm5
 movaps xmm5,dqword ptr [rsp+112]
 mulps xmm7,xmm5
 subps xmm6,xmm7
 movaps xmm5,dqword ptr [rsp+160]
 mulps xmm2,xmm5
 mulps xmm5,xmm3
 subps xmm6,xmm2
 movaps xmm2,xmm5
 addps xmm2,xmm6
 movaps xmm6,xmm0
 movaps xmm0,xmm1
 movaps xmm1,dqword ptr [rsp]
 movaps xmm3,xmm0
 rcpss xmm5,xmm0
 mulss xmm0,xmm5
 mulss xmm0,xmm5
 addss xmm5,xmm5
 subss xmm5,xmm0
 movaps xmm0,xmm5
 addss xmm5,xmm5
 mulss xmm0,xmm0
 mulss xmm3,xmm0
 subss xmm5,xmm3
 shufps xmm5,xmm5,$00
 mulps xmm1,xmm5
 mulps xmm4,xmm5
 mulps xmm6,xmm5
 mulps xmm5,xmm2
 movups dqword ptr [result+0],xmm1
 movups dqword ptr [result+16],xmm4
 movups dqword ptr [result+32],xmm6
 movups dqword ptr [result+48],xmm5
 mov rsp,r9
end;
{$else}
var t0,t4,t8,t12,d:TScalar;
begin
 t0:=(((RawComponents[1,1]*RawComponents[2,2]*RawComponents[3,3])-(RawComponents[1,1]*RawComponents[2,3]*RawComponents[3,2]))-(RawComponents[2,1]*RawComponents[1,2]*RawComponents[3,3])+(RawComponents[2,1]*RawComponents[1,3]*RawComponents[3,2])+(RawComponents[3,1]*RawComponents[1,2]*RawComponents[2,3]))-(RawComponents[3,1]*RawComponents[1,3]*RawComponents[2,2]);
 t4:=((((-(RawComponents[1,0]*RawComponents[2,2]*RawComponents[3,3]))+(RawComponents[1,0]*RawComponents[2,3]*RawComponents[3,2])+(RawComponents[2,0]*RawComponents[1,2]*RawComponents[3,3]))-(RawComponents[2,0]*RawComponents[1,3]*RawComponents[3,2]))-(RawComponents[3,0]*RawComponents[1,2]*RawComponents[2,3]))+(RawComponents[3,0]*RawComponents[1,3]*RawComponents[2,2]);
 t8:=((((RawComponents[1,0]*RawComponents[2,1]*RawComponents[3,3])-(RawComponents[1,0]*RawComponents[2,3]*RawComponents[3,1]))-(RawComponents[2,0]*RawComponents[1,1]*RawComponents[3,3]))+(RawComponents[2,0]*RawComponents[1,3]*RawComponents[3,1])+(RawComponents[3,0]*RawComponents[1,1]*RawComponents[2,3]))-(RawComponents[3,0]*RawComponents[1,3]*RawComponents[2,1]);
 t12:=((((-(RawComponents[1,0]*RawComponents[2,1]*RawComponents[3,2]))+(RawComponents[1,0]*RawComponents[2,2]*RawComponents[3,1])+(RawComponents[2,0]*RawComponents[1,1]*RawComponents[3,2]))-(RawComponents[2,0]*RawComponents[1,2]*RawComponents[3,1]))-(RawComponents[3,0]*RawComponents[1,1]*RawComponents[2,2]))+(RawComponents[3,0]*RawComponents[1,2]*RawComponents[2,1]);
 d:=(RawComponents[0,0]*t0)+(RawComponents[0,1]*t4)+(RawComponents[0,2]*t8)+(RawComponents[0,3]*t12);
 if d<>0.0 then begin
  d:=1.0/d;
  result.RawComponents[0,0]:=t0*d;
  result.RawComponents[0,1]:=(((((-(RawComponents[0,1]*RawComponents[2,2]*RawComponents[3,3]))+(RawComponents[0,1]*RawComponents[2,3]*RawComponents[3,2])+(RawComponents[2,1]*RawComponents[0,2]*RawComponents[3,3]))-(RawComponents[2,1]*RawComponents[0,3]*RawComponents[3,2]))-(RawComponents[3,1]*RawComponents[0,2]*RawComponents[2,3]))+(RawComponents[3,1]*RawComponents[0,3]*RawComponents[2,2]))*d;
  result.RawComponents[0,2]:=(((((RawComponents[0,1]*RawComponents[1,2]*RawComponents[3,3])-(RawComponents[0,1]*RawComponents[1,3]*RawComponents[3,2]))-(RawComponents[1,1]*RawComponents[0,2]*RawComponents[3,3]))+(RawComponents[1,1]*RawComponents[0,3]*RawComponents[3,2])+(RawComponents[3,1]*RawComponents[0,2]*RawComponents[1,3]))-(RawComponents[3,1]*RawComponents[0,3]*RawComponents[1,2]))*d;
  result.RawComponents[0,3]:=(((((-(RawComponents[0,1]*RawComponents[1,2]*RawComponents[2,3]))+(RawComponents[0,1]*RawComponents[1,3]*RawComponents[2,2])+(RawComponents[1,1]*RawComponents[0,2]*RawComponents[2,3]))-(RawComponents[1,1]*RawComponents[0,3]*RawComponents[2,2]))-(RawComponents[2,1]*RawComponents[0,2]*RawComponents[1,3]))+(RawComponents[2,1]*RawComponents[0,3]*RawComponents[1,2]))*d;
  result.RawComponents[1,0]:=t4*d;
  result.RawComponents[1,1]:=((((RawComponents[0,0]*RawComponents[2,2]*RawComponents[3,3])-(RawComponents[0,0]*RawComponents[2,3]*RawComponents[3,2]))-(RawComponents[2,0]*RawComponents[0,2]*RawComponents[3,3])+(RawComponents[2,0]*RawComponents[0,3]*RawComponents[3,2])+(RawComponents[3,0]*RawComponents[0,2]*RawComponents[2,3]))-(RawComponents[3,0]*RawComponents[0,3]*RawComponents[2,2]))*d;
  result.RawComponents[1,2]:=(((((-(RawComponents[0,0]*RawComponents[1,2]*RawComponents[3,3]))+(RawComponents[0,0]*RawComponents[1,3]*RawComponents[3,2])+(RawComponents[1,0]*RawComponents[0,2]*RawComponents[3,3]))-(RawComponents[1,0]*RawComponents[0,3]*RawComponents[3,2]))-(RawComponents[3,0]*RawComponents[0,2]*RawComponents[1,3]))+(RawComponents[3,0]*RawComponents[0,3]*RawComponents[1,2]))*d;
  result.RawComponents[1,3]:=(((((RawComponents[0,0]*RawComponents[1,2]*RawComponents[2,3])-(RawComponents[0,0]*RawComponents[1,3]*RawComponents[2,2]))-(RawComponents[1,0]*RawComponents[0,2]*RawComponents[2,3]))+(RawComponents[1,0]*RawComponents[0,3]*RawComponents[2,2])+(RawComponents[2,0]*RawComponents[0,2]*RawComponents[1,3]))-(RawComponents[2,0]*RawComponents[0,3]*RawComponents[1,2]))*d;
  result.RawComponents[2,0]:=t8*d;
  result.RawComponents[2,1]:=(((((-(RawComponents[0,0]*RawComponents[2,1]*RawComponents[3,3]))+(RawComponents[0,0]*RawComponents[2,3]*RawComponents[3,1])+(RawComponents[2,0]*RawComponents[0,1]*RawComponents[3,3]))-(RawComponents[2,0]*RawComponents[0,3]*RawComponents[3,1]))-(RawComponents[3,0]*RawComponents[0,1]*RawComponents[2,3]))+(RawComponents[3,0]*RawComponents[0,3]*RawComponents[2,1]))*d;
  result.RawComponents[2,2]:=(((((RawComponents[0,0]*RawComponents[1,1]*RawComponents[3,3])-(RawComponents[0,0]*RawComponents[1,3]*RawComponents[3,1]))-(RawComponents[1,0]*RawComponents[0,1]*RawComponents[3,3]))+(RawComponents[1,0]*RawComponents[0,3]*RawComponents[3,1])+(RawComponents[3,0]*RawComponents[0,1]*RawComponents[1,3]))-(RawComponents[3,0]*RawComponents[0,3]*RawComponents[1,1]))*d;
  result.RawComponents[2,3]:=(((((-(RawComponents[0,0]*RawComponents[1,1]*RawComponents[2,3]))+(RawComponents[0,0]*RawComponents[1,3]*RawComponents[2,1])+(RawComponents[1,0]*RawComponents[0,1]*RawComponents[2,3]))-(RawComponents[1,0]*RawComponents[0,3]*RawComponents[2,1]))-(RawComponents[2,0]*RawComponents[0,1]*RawComponents[1,3]))+(RawComponents[2,0]*RawComponents[0,3]*RawComponents[1,1]))*d;
  result.RawComponents[3,0]:=t12*d;
  result.RawComponents[3,1]:=(((((RawComponents[0,0]*RawComponents[2,1]*RawComponents[3,2])-(RawComponents[0,0]*RawComponents[2,2]*RawComponents[3,1]))-(RawComponents[2,0]*RawComponents[0,1]*RawComponents[3,2]))+(RawComponents[2,0]*RawComponents[0,2]*RawComponents[3,1])+(RawComponents[3,0]*RawComponents[0,1]*RawComponents[2,2]))-(RawComponents[3,0]*RawComponents[0,2]*RawComponents[2,1]))*d;
  result.RawComponents[3,2]:=(((((-(RawComponents[0,0]*RawComponents[1,1]*RawComponents[3,2]))+(RawComponents[0,0]*RawComponents[1,2]*RawComponents[3,1])+(RawComponents[1,0]*RawComponents[0,1]*RawComponents[3,2]))-(RawComponents[1,0]*RawComponents[0,2]*RawComponents[3,1]))-(RawComponents[3,0]*RawComponents[0,1]*RawComponents[1,2]))+(RawComponents[3,0]*RawComponents[0,2]*RawComponents[1,1]))*d;
  result.RawComponents[3,3]:=(((((RawComponents[0,0]*RawComponents[1,1]*RawComponents[2,2])-(RawComponents[0,0]*RawComponents[1,2]*RawComponents[2,1]))-(RawComponents[1,0]*RawComponents[0,1]*RawComponents[2,2]))+(RawComponents[1,0]*RawComponents[0,2]*RawComponents[2,1])+(RawComponents[2,0]*RawComponents[0,1]*RawComponents[1,2]))-(RawComponents[2,0]*RawComponents[0,2]*RawComponents[1,1]))*d;
 end;
end;
{$ifend}

function TMatrix4x4.Transpose:TMatrix4x4;
{$if defined(cpu386)}
asm
 movups xmm0,dqword ptr [eax+0]
 movups xmm4,dqword ptr [eax+16]
 movups xmm2,dqword ptr [eax+32]
 movups xmm5,dqword ptr [eax+48]
 movaps xmm1,xmm0
 movaps xmm3,xmm2
 unpcklps xmm0,xmm4
 unpckhps xmm1,xmm4
 unpcklps xmm2,xmm5
 unpckhps xmm3,xmm5
 movaps xmm4,xmm0
 movaps xmm6,xmm1
 shufps xmm0,xmm2,$44 // 01000100b
 shufps xmm4,xmm2,$ee // 11101110b
 shufps xmm1,xmm3,$44 // 01000100b
 shufps xmm6,xmm3,$ee // 11101110b
 movups dqword ptr [result+0],xmm0
 movups dqword ptr [result+16],xmm4
 movups dqword ptr [result+32],xmm1
 movups dqword ptr [result+48],xmm6
end;
{$elseif defined(cpux64)}
asm
{$ifdef Windows}
 movups xmm0,dqword ptr [rcx+0]
 movups xmm4,dqword ptr [rcx+16]
 movups xmm2,dqword ptr [rcx+32]
 movups xmm5,dqword ptr [rcx+48]
{$else}
 movups xmm0,dqword ptr [rdi+0]
 movups xmm4,dqword ptr [rdi+16]
 movups xmm2,dqword ptr [rdi+32]
 movups xmm5,dqword ptr [rdi+48]
{$endif}
 movaps xmm1,xmm0
 movaps xmm3,xmm2
 unpcklps xmm0,xmm4
 unpckhps xmm1,xmm4
 unpcklps xmm2,xmm5
 unpckhps xmm3,xmm5
 movaps xmm4,xmm0
 movaps xmm6,xmm1
 shufps xmm0,xmm2,$44 // 01000100b
 shufps xmm4,xmm2,$ee // 11101110b
 shufps xmm1,xmm3,$44 // 01000100b
 shufps xmm6,xmm3,$ee // 11101110b
 movups dqword ptr [result+0],xmm0
 movups dqword ptr [result+16],xmm4
 movups dqword ptr [result+32],xmm1
 movups dqword ptr [result+48],xmm6
end;
{$else}
begin
 result.RawComponents[0,0]:=RawComponents[0,0];
 result.RawComponents[0,1]:=RawComponents[1,0];
 result.RawComponents[0,2]:=RawComponents[2,0];
 result.RawComponents[0,3]:=RawComponents[3,0];
 result.RawComponents[1,0]:=RawComponents[0,1];
 result.RawComponents[1,1]:=RawComponents[1,1];
 result.RawComponents[1,2]:=RawComponents[2,1];
 result.RawComponents[1,3]:=RawComponents[3,1];
 result.RawComponents[2,0]:=RawComponents[0,2];
 result.RawComponents[2,1]:=RawComponents[1,2];
 result.RawComponents[2,2]:=RawComponents[2,2];
 result.RawComponents[2,3]:=RawComponents[3,2];
 result.RawComponents[3,0]:=RawComponents[0,3];
 result.RawComponents[3,1]:=RawComponents[1,3];
 result.RawComponents[3,2]:=RawComponents[2,3];
 result.RawComponents[3,3]:=RawComponents[3,3];
end;
{$ifend}

function TMatrix4x4.EulerAngles:TVector3;
var v0,v1:TVector3;
begin
 if abs((-1.0)-RawComponents[0,2])<EPSILON then begin
  result.x:=0.0;
  result.y:=pi*0.5;
  result.z:=ArcTan2(RawComponents[1,0],RawComponents[2,0]);
 end else if abs(1.0-RawComponents[0,2])<EPSILON then begin
  result.x:=0.0;
  result.y:=-(pi*0.5);
  result.z:=ArcTan2(-RawComponents[1,0],-RawComponents[2,0]);
 end else begin
  v0.x:=-ArcSin(RawComponents[0,2]);
  v1.x:=pi-v0.x;
  v0.y:=ArcTan2(RawComponents[1,2]/cos(v0.x),RawComponents[2,2]/cos(v0.x));
  v1.y:=ArcTan2(RawComponents[1,2]/cos(v1.x),RawComponents[2,2]/cos(v1.x));
  v0.z:=ArcTan2(RawComponents[0,1]/cos(v0.x),RawComponents[0,0]/cos(v0.x));
  v1.z:=ArcTan2(RawComponents[0,1]/cos(v1.x),RawComponents[0,0]/cos(v1.x));
  if v0.SquaredLength<v1.SquaredLength then begin
   result:=v0;
  end else begin
   result:=v1;
  end;
 end;
end;

function TMatrix4x4.Normalize:TMatrix4x4;
begin
 result.Right.xyz:=Right.xyz.Normalize;
 result.RawComponents[0,3]:=RawComponents[0,3];
 result.Up.xyz:=Up.xyz.Normalize;
 result.RawComponents[1,3]:=RawComponents[1,3];
 result.Forwards.xyz:=Forwards.xyz.Normalize;
 result.RawComponents[2,3]:=RawComponents[2,3];
 result.Translation:=Translation;
end;

function TMatrix4x4.OrthoNormalize:TMatrix4x4;
var Backup:TVector3;
begin
 Backup.x:=RawComponents[0,3];
 Backup.y:=RawComponents[1,3];
 Backup.z:=RawComponents[2,3];
 Normal.xyz:=Normal.xyz.Normalize;
 Tangent.xyz:=(Tangent.xyz-(Normal.xyz*Tangent.xyz.Dot(Normal.xyz))).Normalize;
 Bitangent.xyz:=Normal.xyz.Cross(Tangent.xyz).Normalize;
 Bitangent.xyz:=Bitangent.xyz-(Normal.xyz*Bitangent.xyz.Dot(Normal.xyz));
 Bitangent.xyz:=(Bitangent.xyz-(Tangent.xyz*Bitangent.xyz.Dot(Tangent.xyz))).Normalize;
 Tangent.xyz:=Bitangent.xyz.Cross(Normal.xyz).Normalize;
 Normal.xyz:=Tangent.xyz.Cross(Bitangent.xyz).Normalize;
 result.RawComponents:=RawComponents;
 result.RawComponents[0,3]:=Backup.x;
 result.RawComponents[1,3]:=Backup.y;
 result.RawComponents[2,3]:=Backup.z;
end;

function TMatrix4x4.RobustOrthoNormalize(const Tolerance:TScalar=1e-3):TMatrix4x4;
var Backup,Bisector,Axis:TVector3;
begin
 Backup.x:=RawComponents[0,3];
 Backup.y:=RawComponents[1,3];
 Backup.z:=RawComponents[2,3];
 begin
  if Normal.xyz.Length<Tolerance then begin
   // Degenerate case, compute new normal
   Normal.xyz:=Tangent.xyz.Cross(Bitangent.xyz);
   if Normal.xyz.Length<Tolerance then begin
    Tangent.xyz:=Vector3XAxis;
    Bitangent.xyz:=Vector3YAxis;
    Normal.xyz:=Vector3ZAxis;
    RawComponents[0,3]:=Backup.x;
    RawComponents[1,3]:=Backup.y;
    RawComponents[2,3]:=Backup.z;
    exit;
   end;
  end;
  Normal.xyz:=Normal.xyz.Normalize;
 end;
 begin
  // Project tangent and bitangent onto the normal orthogonal plane
  Tangent.xyz:=Tangent.xyz-(Normal.xyz*Tangent.xyz.Dot(Normal.xyz));
  Bitangent.xyz:=Bitangent.xyz-(Normal.xyz*Bitangent.xyz.Dot(Normal.xyz));
 end;
 begin
  // Check for several degenerate cases
  if Tangent.xyz.Length<Tolerance then begin
   if Bitangent.xyz.Length<Tolerance then begin
    Tangent.xyz:=Normal.xyz.Normalize;
    if (Tangent.x<=Tangent.y) and (Tangent.x<=Tangent.z) then begin
     Tangent.xyz:=Vector3XAxis;
    end else if (Tangent.y<=Tangent.x) and (Tangent.y<=Tangent.z) then begin
     Tangent.xyz:=Vector3YAxis;
    end else begin
     Tangent.xyz:=Vector3ZAxis;
    end;
    Tangent.xyz:=Tangent.xyz-(Normal.xyz*Tangent.xyz.Dot(Normal.xyz));
    Bitangent.xyz:=Normal.xyz.Cross(Tangent.xyz).Normalize;
   end else begin
    Tangent.xyz:=Bitangent.xyz.Cross(Normal.xyz).Normalize;
   end;
  end else begin
   Tangent.xyz:=Tangent.xyz.Normalize;
   if Bitangent.xyz.Length<Tolerance then begin
    Bitangent.xyz:=Normal.xyz.Cross(Tangent.xyz).Normalize;
   end else begin
    Bitangent.xyz:=Bitangent.xyz.Normalize;
    Bisector:=Tangent.xyz+Bitangent.xyz;
    if Bisector.Length<Tolerance then begin
     Bisector:=Tangent.xyz;
    end else begin
     Bisector:=Bisector.Normalize;
    end;
    Axis:=Bisector.Cross(Normal.xyz).Normalize;
    if Axis.Dot(Tangent.xyz)>0.0 then begin
     Tangent.xyz:=(Bisector+Axis).Normalize;
     Bitangent.xyz:=(Bisector-Axis).Normalize;
    end else begin
     Tangent.xyz:=(Bisector-Axis).Normalize;
     Bitangent.xyz:=(Bisector+Axis).Normalize;
    end;
   end;
  end;
 end;
 Bitangent.xyz:=Normal.xyz.Cross(Tangent.xyz).Normalize;
 Tangent.xyz:=Bitangent.xyz.Cross(Normal.xyz).Normalize;
 Normal.xyz:=Tangent.xyz.Cross(Bitangent.xyz).Normalize;
 result.RawComponents:=RawComponents;
 result.RawComponents[0,3]:=Backup.x;
 result.RawComponents[1,3]:=Backup.y;
 result.RawComponents[2,3]:=Backup.z;
end;

function TMatrix4x4.ToQuaternion:TQuaternion;
var t,s:TScalar;
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
 result:=result.Normalize;
end;

function TMatrix4x4.ToQTangent:TQuaternion;
const Threshold=1.0/32767.0;
var Scale,t,s,Renormalization:TScalar;
begin
 if ((((((RawComponents[0,0]*RawComponents[1,1]*RawComponents[2,2])+
         (RawComponents[0,1]*RawComponents[1,2]*RawComponents[2,0])
        )+
        (RawComponents[0,2]*RawComponents[1,0]*RawComponents[2,1])
       )-
       (RawComponents[0,2]*RawComponents[1,1]*RawComponents[2,0])
      )-
      (RawComponents[0,1]*RawComponents[1,0]*RawComponents[2,2])
     )-
     (RawComponents[0,0]*RawComponents[1,2]*RawComponents[2,1])
    )<0.0 then begin
  // Reflection matrix, so flip y axis in case the tangent frame encodes a reflection
  Scale:=-1.0;
  RawComponents[2,0]:=-RawComponents[2,0];
  RawComponents[2,1]:=-RawComponents[2,1];
  RawComponents[2,2]:=-RawComponents[2,2];
 end else begin
  // Rotation matrix, so nothing is doing to do
  Scale:=1.0;
 end;
 begin
  // Convert to quaternion
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
  result:=result.Normalize;
 end;
 begin
  // Make sure, that we don't end up with 0 as w component
  if abs(result.w)<=Threshold then begin
   Renormalization:=sqrt(1.0-sqr(Threshold));
   result.x:=result.x*Renormalization;
   result.y:=result.y*Renormalization;
   result.z:=result.z*Renormalization;
   if result.w>0.0 then begin
    result.w:=Threshold;
   end else begin
    result.w:=-Threshold;
   end;
  end;
 end;
 if ((Scale<0.0) and (result.w>=0.0)) or ((Scale>=0.0) and (result.w<0.0)) then begin
  // Encode reflection into quaternion's w element by making sign of w negative,
  // if y axis needs to be flipped, otherwise it stays positive
  result.x:=-result.x;
  result.y:=-result.y;
  result.z:=-result.z;
  result.w:=-result.w;
 end;
end;

function TMatrix4x4.ToMatrix3x3:TMatrix3x3;
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

function TMatrix4x4.ToRotation:TMatrix4x4;
begin
 result.RawComponents[0,0]:=RawComponents[0,0];
 result.RawComponents[0,1]:=RawComponents[0,1];
 result.RawComponents[0,2]:=RawComponents[0,2];
 result.RawComponents[0,3]:=0.0;
 result.RawComponents[1,0]:=RawComponents[1,0];
 result.RawComponents[1,1]:=RawComponents[1,1];
 result.RawComponents[1,2]:=RawComponents[1,2];
 result.RawComponents[1,3]:=0.0;
 result.RawComponents[2,0]:=RawComponents[2,0];
 result.RawComponents[2,1]:=RawComponents[2,1];
 result.RawComponents[2,2]:=RawComponents[2,2];
 result.RawComponents[2,3]:=0.0;
 result.RawComponents[3,0]:=0.0;
 result.RawComponents[3,1]:=0.0;
 result.RawComponents[3,2]:=0.0;
 result.RawComponents[3,3]:=1.0;
end;

function TMatrix4x4.SimpleLerp(const b:TMatrix4x4;const t:TScalar):TMatrix4x4;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  result:=(self*(1.0-t))+(b*t);
 end;
end;

function TMatrix4x4.SimpleNlerp(const b:TMatrix4x4;const t:TScalar):TMatrix4x4;
var InvT:TScalar;
    Scale:TVector3;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  Scale:=TVector3.Create(Right.xyz.Length,
                         Up.xyz.Length,
                         Forwards.xyz.Length).Lerp(TVector3.Create(b.Right.xyz.Length,
                                                                   b.Up.xyz.Length,
                                                                   b.Forwards.xyz.Length),
                                                   t);
  result:=TMatrix4x4.CreateFromQuaternion(Normalize.ToQuaternion.Nlerp(b.Normalize.ToQuaternion,t));
  result.Right.xyz:=result.Right.xyz*Scale.x;
  result.Up.xyz:=result.Up.xyz*Scale.y;
  result.Forwards.xyz:=result.Forwards.xyz*Scale.z;
  result.Translation:=Translation.Lerp(b.Translation,t);
  InvT:=1.0-t;
  result[0,3]:=(RawComponents[0,3]*InvT)+(b.RawComponents[0,3]*t);
  result[1,3]:=(RawComponents[1,3]*InvT)+(b.RawComponents[1,3]*t);
  result[2,3]:=(RawComponents[2,3]*InvT)+(b.RawComponents[2,3]*t);
 end;
end;

function TMatrix4x4.SimpleSlerp(const b:TMatrix4x4;const t:TScalar):TMatrix4x4;
var InvT:TScalar;
    Scale:TVector3;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  Scale:=TVector3.Create(Right.xyz.Length,
                         Up.xyz.Length,
                         Forwards.xyz.Length).Lerp(TVector3.Create(b.Right.xyz.Length,
                                                                   b.Up.xyz.Length,
                                                                   b.Forwards.xyz.Length),
                                                   t);
  result:=TMatrix4x4.CreateFromQuaternion(Normalize.ToQuaternion.Slerp(b.Normalize.ToQuaternion,t));
  result.Right.xyz:=result.Right.xyz*Scale.x;
  result.Up.xyz:=result.Up.xyz*Scale.y;
  result.Forwards.xyz:=result.Forwards.xyz*Scale.z;
  result.Translation:=Translation.Lerp(b.Translation,t);
  InvT:=1.0-t;
  result[0,3]:=(RawComponents[0,3]*InvT)+(b.RawComponents[0,3]*t);
  result[1,3]:=(RawComponents[1,3]*InvT)+(b.RawComponents[1,3]*t);
  result[2,3]:=(RawComponents[2,3]*InvT)+(b.RawComponents[2,3]*t);
 end;
end;

function TMatrix4x4.Lerp(const b:TMatrix4x4;const t:TScalar):TMatrix4x4;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  result:=TMatrix4x4.CreateRecomposed(Decompose.Lerp(b.Decompose,t));
 end;
end;

function TMatrix4x4.Nlerp(const b:TMatrix4x4;const t:TScalar):TMatrix4x4;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  result:=TMatrix4x4.CreateRecomposed(Decompose.Nlerp(b.Decompose,t));
 end;
end;

function TMatrix4x4.Slerp(const b:TMatrix4x4;const t:TScalar):TMatrix4x4;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  result:=TMatrix4x4.CreateRecomposed(Decompose.Slerp(b.Decompose,t));
 end;
end;

function TMatrix4x4.MulInverse({$ifdef fpc}constref{$else}const{$endif} a:TVector3):TVector3;
var d:TScalar;
begin
 d:=((RawComponents[0,0]*((RawComponents[1,1]*RawComponents[2,2])-(RawComponents[2,1]*RawComponents[1,2])))-
     (RawComponents[0,1]*((RawComponents[1,0]*RawComponents[2,2])-(RawComponents[2,0]*RawComponents[1,2]))))+
     (RawComponents[0,2]*((RawComponents[1,0]*RawComponents[2,1])-(RawComponents[2,0]*RawComponents[1,1])));
 if d<>0.0 then begin
  d:=1.0/d;
 end;
 result.x:=((a.x*((RawComponents[1,1]*RawComponents[2,2])-(RawComponents[1,2]*RawComponents[2,1])))+(a.y*((RawComponents[1,2]*RawComponents[2,0])-(RawComponents[1,0]*RawComponents[2,2])))+(a.z*((RawComponents[1,0]*RawComponents[2,1])-(RawComponents[1,1]*RawComponents[2,0]))))*d;
 result.y:=((RawComponents[0,0]*((a.y*RawComponents[2,2])-(a.z*RawComponents[2,1])))+(RawComponents[0,1]*((a.z*RawComponents[2,0])-(a.x*RawComponents[2,2])))+(RawComponents[0,2]*((a.x*RawComponents[2,1])-(a.y*RawComponents[2,0]))))*d;
 result.z:=((RawComponents[0,0]*((RawComponents[1,1]*a.z)-(RawComponents[1,2]*a.y)))+(RawComponents[0,1]*((RawComponents[1,2]*a.x)-(RawComponents[1,0]*a.z)))+(RawComponents[0,2]*((RawComponents[1,0]*a.y)-(RawComponents[1,1]*a.x))))*d;
end;

function TMatrix4x4.MulInverse({$ifdef fpc}constref{$else}const{$endif} a:TVector4):TVector4;
var d:TScalar;
begin
 d:=((RawComponents[0,0]*((RawComponents[1,1]*RawComponents[2,2])-(RawComponents[2,1]*RawComponents[1,2])))-
     (RawComponents[0,1]*((RawComponents[1,0]*RawComponents[2,2])-(RawComponents[2,0]*RawComponents[1,2]))))+
     (RawComponents[0,2]*((RawComponents[1,0]*RawComponents[2,1])-(RawComponents[2,0]*RawComponents[1,1])));
 if d<>0.0 then begin
  d:=1.0/d;
 end;
 result.x:=((a.x*((RawComponents[1,1]*RawComponents[2,2])-(RawComponents[1,2]*RawComponents[2,1])))+(a.y*((RawComponents[1,2]*RawComponents[2,0])-(RawComponents[1,0]*RawComponents[2,2])))+(a.z*((RawComponents[1,0]*RawComponents[2,1])-(RawComponents[1,1]*RawComponents[2,0]))))*d;
 result.y:=((RawComponents[0,0]*((a.y*RawComponents[2,2])-(a.z*RawComponents[2,1])))+(RawComponents[0,1]*((a.z*RawComponents[2,0])-(a.x*RawComponents[2,2])))+(RawComponents[0,2]*((a.x*RawComponents[2,1])-(a.y*RawComponents[2,0]))))*d;
 result.z:=((RawComponents[0,0]*((RawComponents[1,1]*a.z)-(RawComponents[1,2]*a.y)))+(RawComponents[0,1]*((RawComponents[1,2]*a.x)-(RawComponents[1,0]*a.z)))+(RawComponents[0,2]*((RawComponents[1,0]*a.y)-(RawComponents[1,1]*a.x))))*d;
 result.w:=a.w;
end;

function TMatrix4x4.MulInverted({$ifdef fpc}constref{$else}const{$endif} a:TVector3):TVector3;
var p:TVector3;
begin
 p.x:=a.x-RawComponents[3,0];
 p.y:=a.y-RawComponents[3,1];
 p.z:=a.z-RawComponents[3,2];
 result.x:=(RawComponents[0,0]*p.x)+(RawComponents[0,1]*p.y)+(RawComponents[0,2]*p.z);
 result.y:=(RawComponents[1,0]*p.x)+(RawComponents[1,1]*p.y)+(RawComponents[1,2]*p.z);
 result.z:=(RawComponents[2,0]*p.x)+(RawComponents[2,1]*p.y)+(RawComponents[2,2]*p.z);
end;

function TMatrix4x4.MulInverted({$ifdef fpc}constref{$else}const{$endif} a:TVector4):TVector4;
var p:TVector3;
begin
 p.x:=a.x-RawComponents[3,0];
 p.y:=a.y-RawComponents[3,1];
 p.z:=a.z-RawComponents[3,2];
 result.x:=(RawComponents[0,0]*p.x)+(RawComponents[0,1]*p.y)+(RawComponents[0,2]*p.z);
 result.y:=(RawComponents[1,0]*p.x)+(RawComponents[1,1]*p.y)+(RawComponents[1,2]*p.z);
 result.z:=(RawComponents[2,0]*p.x)+(RawComponents[2,1]*p.y)+(RawComponents[2,2]*p.z);
 result.w:=a.w;
end;

function TMatrix4x4.MulBasis({$ifdef fpc}constref{$else}const{$endif} a:TVector3):TVector3;
begin
 result.x:=(RawComponents[0,0]*a.x)+(RawComponents[1,0]*a.y)+(RawComponents[2,0]*a.z);
 result.y:=(RawComponents[0,1]*a.x)+(RawComponents[1,1]*a.y)+(RawComponents[2,1]*a.z);
 result.z:=(RawComponents[0,2]*a.x)+(RawComponents[1,2]*a.y)+(RawComponents[2,2]*a.z);
end;

function TMatrix4x4.MulBasis({$ifdef fpc}constref{$else}const{$endif} a:TVector4):TVector4;
begin
 result.x:=(RawComponents[0,0]*a.x)+(RawComponents[1,0]*a.y)+(RawComponents[2,0]*a.z);
 result.y:=(RawComponents[0,1]*a.x)+(RawComponents[1,1]*a.y)+(RawComponents[2,1]*a.z);
 result.z:=(RawComponents[0,2]*a.x)+(RawComponents[1,2]*a.y)+(RawComponents[2,2]*a.z);
 result.w:=a.w;
end;

function TMatrix4x4.MulTransposedBasis({$ifdef fpc}constref{$else}const{$endif} a:TVector3):TVector3;
begin
 result.x:=(RawComponents[0,0]*a.x)+(RawComponents[0,1]*a.y)+(RawComponents[0,2]*a.z);
 result.y:=(RawComponents[1,0]*a.x)+(RawComponents[1,1]*a.y)+(RawComponents[1,2]*a.z);
 result.z:=(RawComponents[2,0]*a.x)+(RawComponents[2,1]*a.y)+(RawComponents[2,2]*a.z);
end;

function TMatrix4x4.MulTransposedBasis({$ifdef fpc}constref{$else}const{$endif} a:TVector4):TVector4;
begin
 result.x:=(RawComponents[0,0]*a.x)+(RawComponents[0,1]*a.y)+(RawComponents[0,2]*a.z);
 result.y:=(RawComponents[1,0]*a.x)+(RawComponents[1,1]*a.y)+(RawComponents[1,2]*a.z);
 result.z:=(RawComponents[2,0]*a.x)+(RawComponents[2,1]*a.y)+(RawComponents[2,2]*a.z);
 result.w:=a.w;
end;

function TMatrix4x4.MulHomogen({$ifdef fpc}constref{$else}const{$endif} a:TVector3):TVector3;
var Temporary:TVector4;
begin
 Temporary:=self*TVector4.Create(a,1.0);
 Temporary:=Temporary/Temporary.w;
 result:=Temporary.xyz;
end;

function TMatrix4x4.MulHomogen({$ifdef fpc}constref{$else}const{$endif} a:TVector4):TVector4;
begin
 result:=self*a;
 result:=result/result.w;
end;

function TMatrix4x4.Decompose:TDecomposedMatrix4x4;
var LocalMatrix,PerspectiveMatrix:TMatrix4x4;
    BasisMatrix:TMatrix3x3;
begin

 if RawComponents[3,3]=0.0 then begin

  result.Valid:=false;

 end else if (RawComponents[0,0]=1.0) and
             (RawComponents[0,1]=0.0) and
             (RawComponents[0,2]=0.0) and
             (RawComponents[0,3]=0.0) and
             (RawComponents[1,0]=0.0) and
             (RawComponents[1,1]=1.0) and
             (RawComponents[1,2]=0.0) and
             (RawComponents[1,3]=0.0) and
             (RawComponents[2,0]=0.0) and
             (RawComponents[2,1]=0.0) and
             (RawComponents[2,2]=1.0) and
             (RawComponents[2,3]=0.0) and
             (RawComponents[3,0]=0.0) and
             (RawComponents[3,1]=0.0) and
             (RawComponents[3,2]=0.0) and
             (RawComponents[3,3]=1.0) then begin

  result.Perspective:=TVector4.Create(0.0,0.0,0.0,1.0);
  result.Translation:=TVector3.Create(0.0,0.0,0.0);
  result.Scale:=TVector3.Create(1.0,1.0,1.0);
  result.Skew:=TVector3.Create(0.0,0.0,0.0);
  result.Rotation:=TQuaternion.Create(0.0,0.0,0.0,1.0);

  result.Valid:=true;

 end else begin

  LocalMatrix.Tangent:=Tangent/RawComponents[3,3];
  LocalMatrix.Bitangent:=Bitangent/RawComponents[3,3];
  LocalMatrix.Normal:=Normal/RawComponents[3,3];
  LocalMatrix.Translation:=Translation/RawComponents[3,3];

  PerspectiveMatrix:=LocalMatrix;
  PerspectiveMatrix.RawComponents[0,3]:=0.0;
  PerspectiveMatrix.RawComponents[1,3]:=0.0;
  PerspectiveMatrix.RawComponents[2,3]:=0.0;
  PerspectiveMatrix.RawComponents[3,3]:=1.0;

  if PerspectiveMatrix.Determinant=0.0 then begin

   result.Valid:=false;

  end else begin

   if (LocalMatrix.RawComponents[0,3]<>0.0) or
      (LocalMatrix.RawComponents[1,3]<>0.0) or
      (LocalMatrix.RawComponents[2,3]<>0.0) then begin

    result.Perspective:=PerspectiveMatrix.Inverse.Transpose*TVector4.Create(LocalMatrix.RawComponents[0,3],
                                                                                         LocalMatrix.RawComponents[1,3],
                                                                                         LocalMatrix.RawComponents[2,3],
                                                                                         LocalMatrix.RawComponents[3,3]);

    LocalMatrix.RawComponents[0,3]:=0.0;
    LocalMatrix.RawComponents[1,3]:=0.0;
    LocalMatrix.RawComponents[2,3]:=0.0;
    LocalMatrix.RawComponents[3,3]:=1.0;

   end else begin
    result.Perspective.x:=0.0;
    result.Perspective.y:=0.0;
    result.Perspective.z:=0.0;
    result.Perspective.w:=1.0;
   end;

   result.Translation:=LocalMatrix.Translation.xyz;
   LocalMatrix.Translation.xyz:=TVector3.Create(0.0,0.0,0.0);

   BasisMatrix:=ToMatrix3x3;

   result.Scale.x:=BasisMatrix.Right.Length;
   BasisMatrix.Right:=BasisMatrix.Right.Normalize;

   result.Skew.x:=BasisMatrix.Right.Dot(BasisMatrix.Up);
   BasisMatrix.Up:=BasisMatrix.Up-(BasisMatrix.Right*result.Skew.x);

   result.Scale.y:=BasisMatrix.Up.Length;
   BasisMatrix.Up:=BasisMatrix.Up.Normalize;

   result.Skew.x:=result.Skew.x/result.Scale.y;

   result.Skew.y:=BasisMatrix.Right.Dot(BasisMatrix.Forwards);
   BasisMatrix.Forwards:=BasisMatrix.Forwards-(BasisMatrix.Right*result.Skew.y);
   result.Skew.z:=BasisMatrix.Up.Dot(BasisMatrix.Forwards);
   BasisMatrix.Forwards:=BasisMatrix.Forwards-(BasisMatrix.Up*result.Skew.z);

   result.Scale.z:=BasisMatrix.Forwards.Length;
   BasisMatrix.Forwards:=BasisMatrix.Forwards.Normalize;

   result.Skew.yz:=result.Skew.yz/result.Scale.z;

   if BasisMatrix.Right.Dot(BasisMatrix.Up.Cross(BasisMatrix.Forwards))<0.0 then begin
    result.Scale.x:=-result.Scale.x;
    BasisMatrix:=-BasisMatrix;
   end;

   result.Rotation:=BasisMatrix.ToQuaternion;

   result.Valid:=true;

  end;

 end;

end;

constructor TDualQuaternion.Create(const pQ0,PQ1:TQuaternion);
begin
 RawQuaternions[0]:=pQ0;
 RawQuaternions[1]:=pQ1;
end;

constructor TDualQuaternion.CreateFromRotationTranslationScale(const pRotation:TQuaternion;const pTranslation:TVector3;const pScale:TScalar);
begin
 RawQuaternions[0]:=pRotation.Normalize;
 RawQuaternions[1]:=((0.5/RawQuaternions[0].Length)*RawQuaternions[0])*TQuaternion.Create(pTranslation.x,pTranslation.y,pTranslation.z,0.0);
 RawQuaternions[0]:=RawQuaternions[0]*pScale;
end;

constructor TDualQuaternion.CreateFromMatrix(const pMatrix:TMatrix4x4);
begin
 RawQuaternions[0]:=pMatrix.ToQuaternion;
 RawQuaternions[1]:=((0.5/RawQuaternions[0].Length)*RawQuaternions[0])*TQuaternion.Create(pMatrix.Translation.x,pMatrix.Translation.y,pMatrix.Translation.z,0.0);
 RawQuaternions[0]:=RawQuaternions[0]*((pMatrix.Right.xyz.Length+pMatrix.Up.xyz.Length+pMatrix.Forwards.xyz.Length)/3.0);
end;

class operator TDualQuaternion.Implicit(const a:TMatrix4x4):TDualQuaternion;
begin
 result:=TDualQuaternion.CreateFromMatrix(a);
end;

class operator TDualQuaternion.Explicit(const a:TMatrix4x4):TDualQuaternion;
begin
 result:=TDualQuaternion.CreateFromMatrix(a);
end;

class operator TDualQuaternion.Implicit(const a:TDualQuaternion):TMatrix4x4;
var Scale:TScalar;
begin
 Scale:=a.RawQuaternions[0].Length;
 result:=TMatrix4x4.CreateFromQuaternion(a.RawQuaternions[0].Normalize);
 result.Right.xyz:=result.Right.xyz*Scale;
 result.Up.xyz:=result.Up.xyz*Scale;
 result.Forwards.xyz:=result.Forwards.xyz*Scale;
 result.Translation.xyz:=TQuaternion((2.0*a.RawQuaternions[0].Conjugate)*a.RawQuaternions[1]).Vector.xyz;
end;

class operator TDualQuaternion.Explicit(const a:TDualQuaternion):TMatrix4x4;
var Scale:TScalar;
begin
 Scale:=a.RawQuaternions[0].Length;
 result:=TMatrix4x4.CreateFromQuaternion(a.RawQuaternions[0].Normalize);
 result.Right.xyz:=result.Right.xyz*Scale;
 result.Up.xyz:=result.Up.xyz*Scale;
 result.Forwards.xyz:=result.Forwards.xyz*Scale;
 result.Translation.xyz:=TQuaternion((2.0*a.RawQuaternions[0].Conjugate)*a.RawQuaternions[1]).Vector.xyz;
end;

class operator TDualQuaternion.Equal(const a,b:TDualQuaternion):boolean;
begin
 result:=(a.RawQuaternions[0]=b.RawQuaternions[0]) and
         (a.RawQuaternions[1]=b.RawQuaternions[1]);
end;

class operator TDualQuaternion.NotEqual(const a,b:TDualQuaternion):boolean;
begin
 result:=(a.RawQuaternions[0]<>b.RawQuaternions[0]) or
         (a.RawQuaternions[1]<>b.RawQuaternions[1]);
end;

class operator TDualQuaternion.Add({$ifdef fpc}constref{$else}const{$endif} a,b:TDualQuaternion):TDualQuaternion;
begin
 result.RawQuaternions[0]:=a.RawQuaternions[0]+b.RawQuaternions[0];
 result.RawQuaternions[1]:=a.RawQuaternions[1]+b.RawQuaternions[1];
end;

class operator TDualQuaternion.Subtract({$ifdef fpc}constref{$else}const{$endif} a,b:TDualQuaternion):TDualQuaternion;
begin
 result.RawQuaternions[0]:=a.RawQuaternions[0]-b.RawQuaternions[0];
 result.RawQuaternions[1]:=a.RawQuaternions[1]-b.RawQuaternions[1];
end;

class operator TDualQuaternion.Multiply({$ifdef fpc}constref{$else}const{$endif} a,b:TDualQuaternion):TDualQuaternion;
begin
 result.RawQuaternions[0]:=a.RawQuaternions[0]*b.RawQuaternions[0];
 result.RawQuaternions[1]:=((a.RawQuaternions[0]*b.RawQuaternions[1])/a.RawQuaternions[0].Length)+(a.RawQuaternions[1]*b.RawQuaternions[1]);
end;

class operator TDualQuaternion.Multiply(const a:TDualQuaternion;const b:TScalar):TDualQuaternion;
begin
 result.RawQuaternions[0]:=a.RawQuaternions[0]*b;
 result.RawQuaternions[1]:=a.RawQuaternions[1]*b;
end;

class operator TDualQuaternion.Multiply(const a:TScalar;const b:TDualQuaternion):TDualQuaternion;
begin
 result.RawQuaternions[0]:=a*b.RawQuaternions[0];
 result.RawQuaternions[1]:=a*b.RawQuaternions[1];
end;

class operator TDualQuaternion.Multiply({$ifdef fpc}constref{$else}const{$endif} a:TDualQuaternion;{$ifdef fpc}constref{$else}const{$endif} b:TVector3):TVector3;
begin
 result:=TQuaternion(a.RawQuaternions[0].Conjugate*((2.0*a.RawQuaternions[1])+(TQuaternion.Create(b.x,b.y,b.z,0.0)*a.RawQuaternions[0]))).Vector.xyz;
end;

class operator TDualQuaternion.Multiply(const a:TVector3;const b:TDualQuaternion):TVector3;
begin
 result:=b.Inverse*a;
end;

class operator TDualQuaternion.Multiply({$ifdef fpc}constref{$else}const{$endif} a:TDualQuaternion;{$ifdef fpc}constref{$else}const{$endif} b:TVector4):TVector4;
begin
 result.xyz:=TQuaternion(a.RawQuaternions[0].Conjugate*((2.0*a.RawQuaternions[1])+(TQuaternion.Create(b.x,b.y,b.z,0.0)*a.RawQuaternions[0]))).Vector.xyz;
 result.w:=1.0;
end;

class operator TDualQuaternion.Multiply(const a:TVector4;const b:TDualQuaternion):TVector4;
begin
 result:=b.Inverse*a;
end;

class operator TDualQuaternion.Divide({$ifdef fpc}constref{$else}const{$endif} a,b:TDualQuaternion):TDualQuaternion;
begin
 result.RawQuaternions[0]:=a.RawQuaternions[0]/b.RawQuaternions[0];
 result.RawQuaternions[1]:=a.RawQuaternions[1]/b.RawQuaternions[1];
end;

class operator TDualQuaternion.Divide(const a:TDualQuaternion;const b:TScalar):TDualQuaternion;
begin
 result.RawQuaternions[0]:=a.RawQuaternions[0]/b;
 result.RawQuaternions[1]:=a.RawQuaternions[1]/b;
end;

class operator TDualQuaternion.Divide(const a:TScalar;const b:TDualQuaternion):TDualQuaternion;
begin
 result.RawQuaternions[0]:=a/b.RawQuaternions[0];
 result.RawQuaternions[1]:=a/b.RawQuaternions[1];
end;

class operator TDualQuaternion.IntDivide({$ifdef fpc}constref{$else}const{$endif} a,b:TDualQuaternion):TDualQuaternion;
begin
 result.RawQuaternions[0]:=a.RawQuaternions[0]/b.RawQuaternions[0];
 result.RawQuaternions[1]:=a.RawQuaternions[1]/b.RawQuaternions[1];
end;

class operator TDualQuaternion.IntDivide(const a:TDualQuaternion;const b:TScalar):TDualQuaternion;
begin
 result.RawQuaternions[0]:=a.RawQuaternions[0]/b;
 result.RawQuaternions[1]:=a.RawQuaternions[1]/b;
end;

class operator TDualQuaternion.IntDivide(const a:TScalar;const b:TDualQuaternion):TDualQuaternion;
begin
 result.RawQuaternions[0]:=a/b.RawQuaternions[0];
 result.RawQuaternions[1]:=a/b.RawQuaternions[1];
end;

class operator TDualQuaternion.Modulus(const a,b:TDualQuaternion):TDualQuaternion;
begin
 result.RawQuaternions[0]:=a.RawQuaternions[0] mod b.RawQuaternions[0];
 result.RawQuaternions[1]:=a.RawQuaternions[1] mod b.RawQuaternions[1];
end;

class operator TDualQuaternion.Modulus(const a:TDualQuaternion;const b:TScalar):TDualQuaternion;
begin
 result.RawQuaternions[0]:=a.RawQuaternions[0] mod b;
 result.RawQuaternions[1]:=a.RawQuaternions[1] mod b;
end;

class operator TDualQuaternion.Modulus(const a:TScalar;const b:TDualQuaternion):TDualQuaternion;
begin
 result.RawQuaternions[0]:=a mod b.RawQuaternions[0];
 result.RawQuaternions[1]:=a mod b.RawQuaternions[1];
end;

class operator TDualQuaternion.Negative({$ifdef fpc}constref{$else}const{$endif} a:TDualQuaternion):TDualQuaternion;
begin
 result.RawQuaternions[0]:=-a.RawQuaternions[0];
 result.RawQuaternions[1]:=-a.RawQuaternions[1];
end;

class operator TDualQuaternion.Positive(const a:TDualQuaternion):TDualQuaternion;
begin
 result:=a;
end;

function TDualQuaternion.Flip:TDualQuaternion;
begin
 result.RawQuaternions[0]:=RawQuaternions[0].Flip;
 result.RawQuaternions[1]:=RawQuaternions[1].Flip;
end;

function TDualQuaternion.Conjugate:TDualQuaternion;
begin
 result.RawQuaternions[0].x:=-RawQuaternions[0].x;
 result.RawQuaternions[0].y:=-RawQuaternions[0].y;
 result.RawQuaternions[0].z:=-RawQuaternions[0].z;
 result.RawQuaternions[0].w:=RawQuaternions[0].w;
 result.RawQuaternions[1].x:=RawQuaternions[1].x;
 result.RawQuaternions[1].y:=RawQuaternions[1].y;
 result.RawQuaternions[1].z:=RawQuaternions[1].z;
 result.RawQuaternions[1].w:=-RawQuaternions[1].w;
end;

function TDualQuaternion.Inverse:TDualQuaternion;
begin
 result.RawQuaternions[0]:=RawQuaternions[0].Conjugate;
 result.RawQuaternions[1]:=((-result.RawQuaternions[0])*RawQuaternions[1])*result.RawQuaternions[0];
 result:=result/RawQuaternions[0].Length;
end;

function TDualQuaternion.Normalize:TDualQuaternion;
var Scale:TScalar;
begin
 Scale:=RawQuaternions[0].Length;
 result.RawQuaternions[0]:=RawQuaternions[0]/Scale;
 result.RawQuaternions[1]:=RawQuaternions[1]/Scale;
 result.RawQuaternions[1]:=result.RawQuaternions[1]-(result.RawQuaternions[0].Dot(result.RawQuaternions[1])*result.RawQuaternions[0]);
end;

constructor TSegment.Create(const p0,p1:TVector3);
begin
 Points[0]:=p0;
 Points[1]:=p1;
end;

function TSegment.SquaredDistanceTo(const p:TVector3):TScalar;
var pq,pp:TVector3;
    e,f:TScalar;
begin
 pq:=Points[1]-Points[0];
 pp:=p-Points[0];
 e:=pp.Dot(pq);
 if e<=0.0 then begin
  result:=pp.SquaredLength;
 end else begin
  f:=pq.SquaredLength;
  if e<f then begin
   result:=pp.SquaredLength-(sqr(e)/f);
  end else begin
   result:=(p-Points[1]).SquaredLength;
  end;
 end;
end;

function TSegment.SquaredDistanceTo(const p:TVector3;out Nearest:TVector3):TScalar;
var t,DotUV:TScalar;
    Diff,v:TVector3;
begin
 Diff:=p-Points[0];
 v:=Points[1]-Points[0];
 t:=v.Dot(Diff);
 if t>0.0 then begin
  DotUV:=v.SquaredLength;
  if t<DotUV then begin
   t:=t/DotUV;
   Diff:=Diff-(v*t);
  end else begin
   t:=1;
   Diff:=Diff-v;
  end;
 end else begin
  t:=0.0;
 end;
 Nearest:=Points[0].Lerp(Points[1],t);
 result:=Diff.SquaredLength;
end;

procedure TSegment.ClosestPointTo(const p:TVector3;out Time:TScalar;out ClosestPoint:TVector3);
var u,v:TVector3;
begin
 u:=Points[1]-Points[0];
 v:=p-Points[0];
 Time:=u.Dot(v)/u.SquaredLength;
 if Time<=0.0 then begin
  ClosestPoint:=Points[0];
 end else if Time>=1.0 then begin
  ClosestPoint:=Points[1];
 end else begin
  ClosestPoint:=(Points[0]*(1.0-Time))+(Points[1]*Time);
 end;
end;

function TSegment.Transform(const Transform:TMatrix4x4):TSegment;
begin
 result.Points[0]:=Transform*Points[0];
 result.Points[1]:=Transform*Points[1];
end;

procedure TSegment.ClosestPoints(const SegmentB:TSegment;out TimeA:TScalar;out ClosestPointA:TVector3;out TimeB:TScalar;out ClosestPointB:TVector3);
var dA,dB,r:TVector3;
    a,b,c,{d,}e,f,Denominator,aA,aB,bA,bB:TScalar;
begin
 dA:=Points[1]-Points[0];
 dB:=SegmentB.Points[1]-SegmentB.Points[0];
 r:=Points[0]-SegmentB.Points[0];
 a:=dA.SquaredLength;
 e:=dB.SquaredLength;
 f:=dB.Dot(r);
 if (a<EPSILON) and (e<EPSILON) then begin
  // segment a and b are both points
  TimeA:=0.0;
  TimeB:=0.0;
  ClosestPointA:=Points[0];
  ClosestPointB:=SegmentB.Points[0];
 end else begin
  if a<EPSILON then begin
   // segment a is a point
	 TimeA:=0.0;
   TimeB:=f/e;
   if TimeB<0.0 then begin
    TimeB:=0.0;
   end else if TimeB>1.0 then begin
    TimeB:=1.0;
   end;
  end else begin
   c:=dA.Dot(r);
   if e<EPSILON then begin
		// segment b is a point
    TimeA:=-(c/a);
    if TimeA<0.0 then begin
     TimeA:=0.0;
    end else if TimeA>1.0 then begin
     TimeA:=1.0;
    end;
    TimeB:=0.0;
	 end else begin
    b:=dA.Dot(dB);
    Denominator:=(a*e)-sqr(b);
		if Denominator<EPSILON then begin
     // segments are parallel
     aA:=dB.Dot(Points[0]);
     aB:=dB.Dot(Points[1]);
     bA:=dB.Dot(SegmentB.Points[0]);
     bB:=dB.Dot(SegmentB.Points[1]);
     if (aA<=bA) and (aB<=bA) then begin
			// segment A is completely "before" segment B
      if aB>aA then begin
       TimeA:=1.0;
      end else begin
       TimeA:=0.0;
      end;
      TimeB:=0.0;
     end else if (aA>=bB) and (aB>=bB) then begin
      // segment B is completely "before" segment A
      if aB>aA then begin
       TimeA:=0.0;
      end else begin
       TimeA:=1.0;
      end;
      TimeB:=1.0;
     end else begin
      // segments A and B overlap, use midpoint of shared length
			if aA>aB then begin
       f:=aA;
       aA:=aB;
       aB:=f;
      end;
      f:=(Min(aB,bB)+Max(aA,bA))*0.5;
      TimeB:=(f-bA)/e;
      ClosestPointB:=SegmentB.Points[0]+(dB*TimeB);
      ClosestPointTo(ClosestPointB,TimeB,ClosestPointA);
      exit;
     end;
    end	else begin
     // general case
     TimeA:=((b*f)-(c*e))/Denominator;
     if TimeA<0.0 then begin
      TimeA:=0.0;
     end else if TimeA>1.0 then begin
      TimeA:=1.0;
     end;
     TimeB:=((b*TimeA)+f)/e;
     if TimeB<0.0 then begin
      TimeB:=0.0;
      TimeA:=-(c/a);
      if TimeA<0.0 then begin
       TimeA:=0.0;
      end else if TimeA>1.0 then begin
       TimeA:=1.0;
      end;
     end else if TimeB>1.0 then begin
      TimeB:=1.0;
      TimeA:=(b-c)/a;
      if TimeA<0.0 then begin
       TimeA:=0.0;
      end else if TimeA>1.0 then begin
       TimeA:=1.0;
      end;
     end;
    end;
   end;
  end;
  ClosestPointA:=Points[0]+(dA*TimeA);
  ClosestPointB:=SegmentB.Points[0]+(dB*TimeB);
 end;
end;

function TSegment.Intersect(const SegmentB:TSegment;out TimeA,TimeB:TScalar;out IntersectionPoint:TVector3):boolean;
var PointA:TVector3;
begin
 ClosestPoints(SegmentB,TimeA,PointA,TimeB,IntersectionPoint);
 result:=(PointA-IntersectionPoint).SquaredLength<EPSILON;
end;

function TRelativeSegment.SquaredSegmentDistanceTo(const pOtherRelativeSegment:TRelativeSegment;out t0,t1:TScalar):TScalar;
var kDiff:TVector3;
    fA00,fA01,fA11,fB0,fC,fDet,fB1,fS,fT,fSqrDist,fTmp,fInvDet:TScalar;
begin
 kDiff:=self.Origin-pOtherRelativeSegment.Origin;
 fA00:=self.Delta.SquaredLength;
 fA01:=-self.Delta.Dot(pOtherRelativeSegment.Delta);
 fA11:=pOtherRelativeSegment.Delta.SquaredLength;
 fB0:=kDiff.Dot(self.Delta);
 fC:=kDiff.SquaredLength;
 fDet:=abs((fA00*fA11)-(fA01*fA01));
 if fDet>=EPSILON then begin
  // line segments are not parallel
  fB1:=-kDiff.Dot(pOtherRelativeSegment.Delta);
  fS:=(fA01*fB1)-(fA11*fB0);
  fT:=(fA01*fB0)-(fA00*fB1);
  if fS>=0.0 then begin
   if fS<=fDet then begin
    if fT>=0.0 then begin
     if fT<=fDet then begin // region 0 (interior)
      // minimum at two interior points of 3D lines
      fInvDet:=1.0/fDet;
      fS:=fS*fInvDet;
      fT:=fT*fInvDet;
      fSqrDist:=(fS*((fA00*fS)+(fA01*fT)+(2.0*fB0)))+(fT*((fA01*fS)+(fA11*fT)+(2.0*fB1)))+fC;
     end else begin // region 3 (side)
      fT:=1.0;
      fTmp:=fA01+fB0;
      if fTmp>=0.0 then begin
       fS:=0.0;
       fSqrDist:=fA11+(2.0*fB1)+fC;
      end else if (-fTmp)>=fA00 then begin
       fS:=1.0;
       fSqrDist:=fA00+fA11+fC+(2.0*(fB1+fTmp));
      end else begin
       fS:=-fTmp/fA00;
       fSqrDist:=fTmp*fS+fA11+(2.0*fB1)+fC;
      end;
     end;
    end else begin // region 7 (side)
     fT:=0.0;
     if fB0>=0.0 then begin
      fS:=0.0;
      fSqrDist:=fC;
     end else if (-fB0)>=fA00 then begin
      fS:=1.0;
      fSqrDist:=fA00+(2.0*fB0)+fC;
     end else begin
      fS:=(-fB0)/fA00;
      fSqrDist:=(fB0*fS)+fC;
     end;
    end;
   end else begin
    if fT>=0.0 then begin
     if fT<=fDet then begin // region 1 (side)
      fS:=1.0;
      fTmp:=fA01+fB1;
      if fTmp>=0.0 then begin
       fT:=0.0;
       fSqrDist:=fA00+(2.0*fB0)+fC;
      end else if (-fTmp)>=fA11 then begin
       fT:=1.0;
       fSqrDist:=fA00+fA11+fC+(2.0*(fB0+fTmp));
      end else begin
       fT:=(-fTmp)/fA11;
       fSqrDist:=(fTmp*fT)+fA00+(2.0*fB0)+fC;
      end;
     end else begin // region 2 (corner)
      fTmp:=fA01+fB0;
      if (-fTmp)<=fA00 then begin
       fT:=1.0;
       if fTmp>=0.0 then begin
        fS:=0.0;
        fSqrDist:=fA11+(2.0*fB1)+fC;
       end else begin
        fS:=(-fTmp)/fA00;
        fSqrDist:=(fTmp*fS)+fA11+(2.0*fB1)+fC;
       end;
      end else begin
       fS:=1.0;
       fTmp:=fA01+fB1;
       if fTmp>=0.0 then begin
        fT:=0.0;
        fSqrDist:=fA00+(2.0*fB0)+fC;
       end else if (-fTmp)>=fA11 then begin
        fT:=1.0;
        fSqrDist:=fA00+fA11+fC+(2.0*(fB0+fTmp));
       end else begin
        fT:=(-fTmp)/fA11;
        fSqrDist:=(fTmp*fT)+fA00+(2.0*fB0)+fC;
       end;
      end;
     end;
    end else begin // region 8 (corner)
     if (-fB0)<fA00 then begin
      fT:=0.0;
      if fB0>=0.0 then begin
       fS:=0.0;
       fSqrDist:=fC;
      end else begin
       fS:=(-fB0)/fA00;
       fSqrDist:=(fB0*fS)+fC;
      end;
     end else begin
      fS:=1.0;
      fTmp:=fA01+fB1;
      if fTmp>=0.0 then begin
       fT:=0.0;
       fSqrDist:=fA00+(2.0*fB0)+fC;
      end else if (-fTmp)>=fA11 then begin
       fT:=1.0;
       fSqrDist:=fA00+fA11+fC+(2.0*(fB0+fTmp));
      end else begin
       fT:=(-fTmp)/fA11;
       fSqrDist:=(fTmp*fT)+fA00+(2.0*fB0)+fC;
      end;
      end;
    end;
   end;
  end else begin
   if fT>=0.0 then begin
    if fT<=fDet then begin // region 5 (side)
     fS:=0.0;
     if fB1>=0.0 then begin
      fT:=0.0;
      fSqrDist:=fC;
     end else if (-fB1)>=fA11 then begin
      fT:=1.0;
      fSqrDist:=fA11+(2.0*fB1)+fC;
     end else begin
      fT:=(-fB1)/fA11;
      fSqrDist:=fB1*fT+fC;
     end
    end else begin // region 4 (corner)
     fTmp:=fA01+fB0;
     if fTmp<0.0 then begin
      fT:=1.0;
      if (-fTmp)>=fA00 then begin
       fS:=1.0;
       fSqrDist:=fA00+fA11+fC+(2.0*(fB1+fTmp));
      end else begin
       fS:=(-fTmp)/fA00;
       fSqrDist:=fTmp*fS+fA11+(2.0*fB1)+fC;
      end;
     end else begin
      fS:=0.0;
      if fB1>=0.0 then begin
       fT:=0.0;
       fSqrDist:=fC;
      end else if (-fB1)>=fA11 then begin
       fT:=1.0;
       fSqrDist:=fA11+(2.0*fB1)+fC;
      end else begin
       fT:=(-fB1)/fA11;
       fSqrDist:=(fB1*fT)+fC;
      end;
     end;
    end;
   end else begin // region 6 (corner)
    if fB0<0.0 then begin
     fT:=0.0;
     if (-fB0)>=fA00 then begin
      fS:=1.0;
      fSqrDist:=fA00+(2.0*fB0)+fC;
     end else begin
      fS:=(-fB0)/fA00;
      fSqrDist:=(fB0*fS)+fC;
     end;
    end else begin
     fS:=0.0;
     if fB1>=0.0 then begin
      fT:=0.0;
      fSqrDist:=fC;
     end else if (-fB1)>=fA11 then begin
      fT:=1.0;
      fSqrDist:=fA11+(2.0*fB1)+fC;
     end else begin
      fT:=(-fB1)/fA11;
      fSqrDist:=(fB1*fT)+fC;
     end;
    end;
   end;
  end;
 end else begin // line segments are parallel
  if fA01>0.0 then begin // direction vectors form an obtuse angle
   if fB0>=0.0 then begin
    fS:=0.0;
    fT:=0.0;
    fSqrDist:=fC;
   end else if (-fB0)<=fA00 then begin
    fS:=(-fB0)/fA00;
    fT:=0.0;
    fSqrDist:=(fB0*fS)+fC;
   end else begin
    fB1:=-kDiff.Dot(pOtherRelativeSegment.Delta);
    fS:=1.0;
    fTmp:=fA00+fB0;
    if (-fTmp)>=fA01 then begin
     fT:=1.0;
     fSqrDist:=fA00+fA11+fC+(2.0*(fA01+fB0+fB1));
    end else begin
     fT:=(-fTmp)/fA01;
     fSqrDist:=fA00+(2.0*fB0)+fC+(fT*((fA11*fT)+(2.0*(fA01+fB1))));
    end;
   end;
  end else begin // direction vectors form an acute angle
   if (-fB0)>=fA00 then begin
    fS:=1.0;
    fT:=0.0;
    fSqrDist:=fA00+(2.0*fB0)+fC;
   end else if fB0<=0.0 then begin
    fS:=(-fB0)/fA00;
    fT:=0.0;
    fSqrDist:=(fB0*fS)+fC;
   end else begin
    fB1:=-kDiff.Dot(pOtherRelativeSegment.Delta);
    fS:=0.0;
    if fB0>=(-fA01) then begin
     fT:=1.0;
     fSqrDist:=fA11+(2.0*fB1)+fC;
    end else begin
     fT:=(-fB0)/fA01;
     fSqrDist:=fC+(fT*((2.0)*fB1)+(fA11*fT));
    end;
   end;
  end;
 end;
 t0:=fS;
 t1:=fT;
 result:=abs(fSqrDist);
end;

constructor TTriangle.Create(const pA,pB,pC:TVector3);
begin
 Points[0]:=pA;
 Points[1]:=pB;
 Points[2]:=pC;
 Normal:=((pB-pA).Cross(pC-pA)).Normalize;
end;

function TTriangle.Contains(const p:TVector3):boolean;
var vA,vB,vC:TVector3;
    dAB,dAC,dBC:TScalar;
begin
 vA:=Points[0]-p;
 vB:=Points[1]-p;
 vC:=Points[2]-p;
 dAB:=vA.Dot(vB);
 dAC:=vA.Dot(vC);
 dBC:=vB.Dot(vC);
 if ((dBC*dAC)-(vC.SquaredLength*dAB))<0.0 then begin
  result:=false;
 end else begin
  result:=((dAB*dBC)-(dAC*vB.SquaredLength))>=0.0;
 end;
end;

procedure TTriangle.ProjectToVector(const Vector:TVector3;out TriangleMin,TriangleMax:TScalar);
var Projection:TScalar;
begin
 Projection:=Vector.Dot(Points[0]);
 TriangleMin:=Projection;
 TriangleMax:=Projection;
 Projection:=Vector.Dot(Points[1]);
 TriangleMin:=Min(TriangleMin,Projection);
 TriangleMax:=Max(TriangleMax,Projection);
 Projection:=Vector.Dot(Points[2]);
 TriangleMin:=Min(TriangleMin,Projection);
 TriangleMax:=Max(TriangleMax,Projection);
end;

function TTriangle.ProjectToPoint(var pPoint:TVector3;out s,t:TScalar):TScalar;
var Diff,Edge0,Edge1:TVector3;
    A00,A01,A11,B0,C,Det,B1,SquaredDistance,InvDet,Tmp0,Tmp1,Numer,Denom:TScalar;
begin
 Diff:=Points[0]-pPoint;
 Edge0:=Points[1]-Points[0];
 Edge1:=Points[2]-Points[0];
 A00:=Edge0.SquaredLength;
 A01:=Edge0.Dot(Edge1);
 A11:=Edge1.SquaredLength;
 B0:=Diff.Dot(Edge0);
 B1:=Diff.Dot(Edge1);
 C:=Diff.SquaredLength;
 Det:=max(abs((A00*A11)-(A01*A01)),EPSILON);
 s:=(A01*B1)-(A11*B0);
 t:=(A01*B0)-(A00*B1);
 if (s+t)<=Det then begin
  if s<0.0 then begin
   if t<0.0 then begin // region 4
    if B0<0.0 then begin
     t:=0.0;
     if (-B0)>=A00 then begin
      s:=1.0;
      SquaredDistance:=A00+(2.0*B0)+C;
     end else begin
      s:=(-B0)/A00;
      SquaredDistance:=(B0*s)+C;
     end;
    end else begin
     s:=0.0;
     if B1>=0.0 then begin
      t:=0.0;
      SquaredDistance:=C;
     end else if (-B1)>=A11 then begin
      t:=1.0;
      SquaredDistance:=A11+(2.0*B1)+C;
     end else begin
      t:=(-B1)/A11;
      SquaredDistance:=(B1*t)+C;
     end;
    end;
   end else begin // region 3
    s:=0.0;
    if B1>=0.0 then begin
     t:=0.0;
     SquaredDistance:=C;
    end else if (-B1)>=A11 then begin
     t:=1.0;
     SquaredDistance:=A11+(2.0*B1)+C;
    end else begin
     t:=(-B1)/A11;
     SquaredDistance:=(B1*t)+C;
    end;
   end;
  end else if t<0.0 then begin // region 5
   t:=0.0;
   if B0>=0.0 then begin
    s:=0.0;
    SquaredDistance:=C;
   end else if (-B0)>=A00 then begin
    s:=1.0;
    SquaredDistance:=A00+(2.0*B0)+C;
   end else begin
    s:=(-B0)/A00;
    SquaredDistance:=(B0*s)+C;
   end;
  end else begin // region 0
   // minimum at interior point
   InvDet:=1.0/Det;
   s:=s*InvDet;
   t:=t*InvDet;
   SquaredDistance:=(s*((A00*s)+(A01*t)+(2.0*B0)))+(t*((A01*s)+(A11*t)+(2.0*B1)))+C;
  end;
 end else begin
  if s<0.0 then begin // region 2
   Tmp0:=A01+B0;
   Tmp1:=A11+B1;
   if Tmp1>Tmp0 then begin
    Numer:=Tmp1-Tmp0;
    Denom:=A00-(2.0*A01)+A11;
    if Numer>=Denom then begin
     s:=1.0;
     t:=0.0;
     SquaredDistance:=A00+(2.0*B0)+C;
    end else begin
     s:=Numer/Denom;
     t:=1.0-s;
     SquaredDistance:=(s*((A00*s)+(A01*t)+(2*B0)))+(t*((A01*s)+(A11*t)+(2*B1)))+C;
    end;
   end else begin
    s:=0.0;
    if Tmp1<=0.0 then begin
     t:=1.0;
     SquaredDistance:=A11+(2.0*B1)+C;
    end else if B1>=0.0 then begin
     t:=0.0;
     SquaredDistance:=C;
    end else begin
     t:=(-B1)/A11;
     SquaredDistance:=(B1*t)+C;
    end;
   end;
  end else if t<0.0 then begin // region 6
   Tmp0:=A01+B1;
   Tmp1:=A00+B0;
   if Tmp1>Tmp0 then begin
    Numer:=Tmp1-Tmp0;
    Denom:=A00-(2.0*A01)+A11;
    if Numer>=Denom then begin
     t:=1.0;
     s:=0.0;
     SquaredDistance:=A11+(2*B1)+C;
    end else begin
     t:=Numer/Denom;
     s:=1.0-t;
     SquaredDistance:=(s*((A00*s)+(A01*t)+(2.0*B0)))+(t*((A01*s)+(A11*t)+(2.0*B1)))+C;
    end;
   end else begin
    t:=0.0;
    if Tmp1<=0.0 then begin
     s:=1.0;
     SquaredDistance:=A00+(2.0*B0)+C;
    end else if B0>=0.0 then begin
     s:=0.0;
     SquaredDistance:=C;
    end else begin
     s:=(-B0)/A00;
     SquaredDistance:=(B0*s)+C;
    end;
   end;
  end else begin // region 1
   Numer:=((A11+B1)-A01)-B0;
   if Numer<=0.0 then begin
    s:=0.0;
    t:=1.0;
    SquaredDistance:=A11+(2.0*B1)+C;
   end else begin
    Denom:=A00-(2.0*A01)+A11;
    if Numer>=Denom then begin
     s:=1.0;
     t:=0.0;
     SquaredDistance:=A00+(2.0*B0)+C;
    end else begin
     s:=Numer/Denom;
     t:=1.0-s;
     SquaredDistance:=(s*((A00*s)+(A01*t)+(2.0*B0)))+(t*((A01*s)+(A11*t)+(2.0*B1)))+C;
    end;
   end;
  end;
 end;
 pPoint.x:=Points[0].x+((Edge0.x*s)+(Edge1.x*t));
 pPoint.y:=Points[0].y+((Edge0.y*s)+(Edge1.y*t));
 pPoint.z:=Points[0].z+((Edge0.z*s)+(Edge1.z*t));
 result:=abs(SquaredDistance);
end;

function TTriangle.SegmentIntersect(const Segment:TSegment;out Time:TScalar;out IntersectionPoint:TVector3):boolean;
var Switched:boolean;
    d,t,v,w:TScalar;
    vAB,vAC,pBA,vApA,e,n:TVector3;
    s:TSegment;
begin

 result:=false;

 Time:=NaN;

 IntersectionPoint:=Vector3Origin;

 Switched:=false;

 vAB:=Points[1]-Points[0];
 vAC:=Points[2]-Points[0];

 pBA:=Segment.Points[0]-Segment.Points[1];

 n:=vAB.Cross(vAC);

 d:=n.Dot(pBA);

 if abs(d)<EPSILON then begin
  exit; // segment is parallel
 end else if d<0.0 then begin
  s.Points[0]:=Segment.Points[1];
  s.Points[1]:=Segment.Points[0];
  Switched:=true;
  pBA:=s.Points[0]-s.Points[1];
  d:=-d;
 end else begin
  s:=Segment;
 end;

 vApA:=s.Points[0]-Points[0];
 t:=n.Dot(vApA);
 e:=pBA.Cross(vApA);

 v:=vAC.Dot(e);
 if (v<0.0) or (v>d) then begin
  exit; // intersects outside triangle
 end;

 w:=-vAB.Dot(e);
 if (w<0.0) or ((v+w)>d) then begin
  exit; // intersects outside triangle
 end;

 d:=1.0/d;
 t:=t*d;
 v:=v*d;
 w:=w*d;
 Time:=t;

 IntersectionPoint:=Points[0]+((vAB*v)+(vAC*w));

 if Switched then begin
	Time:=1.0-Time;
 end;

 result:=(Time>=0.0) and (Time<=1.0);
end;

function TTriangle.ClosestPointTo(const Point:TVector3;out ClosestPoint:TVector3):boolean;
var u,v,w,d1,d2,d3,d4,d5,d6,Denominator:TScalar;
    vAB,vAC,vAp,vBp,vCp:TVector3;
begin
 result:=false;

 vAB:=Points[1]-Points[0];
 vAC:=Points[2]-Points[0];
 vAp:=Point-Points[0];

 d1:=vAB.Dot(vAp);
 d2:=vAC.Dot(vAp);
 if (d1<=0.0) and (d2<=0.0) then begin
	ClosestPoint:=Points[0]; // closest point is vertex A
	exit;
 end;

 vBp:=Point-Points[1];
 d3:=vAB.Dot(vBp);
 d4:=vAC.Dot(vBp);
 if (d3>=0.0) and (d4<=d3) then begin
	ClosestPoint:=Points[1]; // closest point is vertex B
	exit;
 end;

 w:=(d1*d4)-(d3*d2);
 if (w<=0.0) and (d1>=0.0) and (d3<=0.0) then begin
 	// closest point is along edge 1-2
	ClosestPoint:=Points[0]+(vAB*(d1/(d1-d3)));
  exit;
 end;

 vCp:=Point-Points[2];
 d5:=vAB.Dot(vCp);
 d6:=vAC.Dot(vCp);
 if (d6>=0.0) and (d5<=d6) then begin
	ClosestPoint:=Points[2]; // closest point is vertex C
	exit;
 end;

 v:=(d5*d2)-(d1*d6);
 if (v<=0.0) and (d2>=0.0) and (d6<=0.0) then begin
 	// closest point is along edge 1-3
	ClosestPoint:=Points[0]+(vAC*(d2/(d2-d6)));
  exit;
 end;

 u:=(d3*d6)-(d5*d4);
 if (u<=0.0) and ((d4-d3)>=0.0) and ((d5-d6)>=0.0) then begin
	// closest point is along edge 2-3
	ClosestPoint:=Points[1]+((Points[2]-Points[1])*((d4-d3)/((d4-d3)+(d5-d6))));
  exit;
 end;

 Denominator:=1.0/(u+v+w);

 ClosestPoint:=Points[0]+((vAB*(v*Denominator))+(vAC*(w*Denominator)));

 result:=true;
end;

function TTriangle.ClosestPointTo(const Segment:TSegment;out Time:TScalar;out pClosestPointOnSegment,pClosestPointOnTriangle:TVector3):boolean;
var MinDist,dtri,d1,d2,sa,sb,dist:TScalar;
    pAInside,pBInside:boolean;
    pa,pb:TVector3;
    Edge:TSegment;
begin

 result:=SegmentIntersect(Segment,Time,pClosestPointOnTriangle);

 if result then begin

 	// segment intersects triangle
  pClosestPointOnSegment:=pClosestPointOnTriangle;

 end else begin

  MinDist:=MAX_SCALAR;

  pClosestPointOnSegment:=Vector3Origin;

  dtri:=Normal.Dot(Points[0]);

  pAInside:=Contains(Segment.Points[0]);
  pBInside:=Contains(Segment.Points[1]);

  if pAInside and pBInside then begin
   // both points inside triangle
   d1:=Normal.Dot(Segment.Points[0])-dtri;
   d2:=Normal.Dot(Segment.Points[1])-dtri;
   if abs(d2-d1)<EPSILON then begin
    // segment is parallel to triangle
    pClosestPointOnSegment:=(Segment.Points[0]+Segment.Points[1])*0.5;
    MinDist:=d1;
    Time:=0.5;
   end	else if abs(d1)<abs(d2) then begin
    pClosestPointOnSegment:=Segment.Points[0];
    MinDist:=d1;
    Time:=0.0;
   end else begin
    pClosestPointOnSegment:=Segment.Points[1];
    MinDist:=d2;
    Time:=1.0;
   end;
   pClosestPointOnTriangle:=pClosestPointOnSegment+(Normal*(-MinDist));
   result:=true;
   exit;
  end else if pAInside then begin
   // one point is inside triangle
   pClosestPointOnSegment:=Segment.Points[0];
   Time:=0.0;
   MinDist:=Normal.Dot(pClosestPointOnSegment)-dtri;
   pClosestPointOnTriangle:=pClosestPointOnSegment+(Normal*(-MinDist));
   MinDist:=sqr(MinDist);
  end else if pBInside then begin
   // one point is inside triangle
   pClosestPointOnSegment:=Segment.Points[1];
   Time:=1.0;
   MinDist:=Normal.Dot(pClosestPointOnSegment)-dtri;
   pClosestPointOnTriangle:=pClosestPointOnSegment+(Normal*(-MinDist));
   MinDist:=sqr(MinDist);
  end;

  // test edge 1
  Edge.Points[0]:=Points[0];
  Edge.Points[1]:=Points[1];
  Segment.ClosestPoints(Edge,sa,pa,sb,pb);
  Dist:=(pa-pb).SquaredLength;
  if Dist<MinDist then begin
   MinDist:=Dist;
   Time:=sa;
   pClosestPointOnSegment:=pa;
   pClosestPointOnTriangle:=pb;
  end;

  // test edge 2
  Edge.Points[0]:=Points[1];
  Edge.Points[1]:=Points[2];
  Segment.ClosestPoints(Edge,sa,pa,sb,pb);
  Dist:=(pa-pb).SquaredLength;
  if Dist<MinDist then begin
   MinDist:=Dist;
   Time:=sa;
   pClosestPointOnSegment:=pa;
   pClosestPointOnTriangle:=pb;
  end;

  // test edge 3
  Edge.Points[0]:=Points[2];
  Edge.Points[1]:=Points[0];
  Segment.ClosestPoints(Edge,sa,pa,sb,pb);
  Dist:=(pa-pb).SquaredLength;
  if Dist<MinDist then begin
// MinDist:=Dist;
   Time:=sa;
   pClosestPointOnSegment:=pa;
   pClosestPointOnTriangle:=pb;
  end;

 end;

end;

function TTriangle.GetClosestPointTo(const pPoint:TVector3;out ClosestPoint:TVector3):TScalar;
var Diff,Edge0,Edge1:TVector3;
    A00,A01,A11,B0,C,Det,B1,s,t,SquaredDistance,InvDet,Tmp0,Tmp1,Numer,Denom:TScalar;
begin
 Diff:=Points[0]-pPoint;
 Edge0:=Points[1]-Points[0];
 Edge1:=Points[2]-Points[0];
 A00:=Edge0.SquaredLength;
 A01:=Edge0.Dot(Edge1);
 A11:=Edge1.SquaredLength;
 B0:=Diff.Dot(Edge0);
 B1:=Diff.Dot(Edge1);
 C:=Diff.SquaredLength;
 Det:=max(abs((A00*A11)-(A01*A01)),EPSILON);
 s:=(A01*B1)-(A11*B0);
 t:=(A01*B0)-(A00*B1);
 if (s+t)<=Det then begin
  if s<0.0 then begin
   if t<0.0 then begin // region 4
    if B0<0.0 then begin
     t:=0.0;
     if (-B0)>=A00 then begin
      s:=1.0;
      SquaredDistance:=A00+(2.0*B0)+C;
     end else begin
      s:=(-B0)/A00;
      SquaredDistance:=(B0*s)+C;
     end;
    end else begin
     s:=0.0;
     if B1>=0.0 then begin
      t:=0.0;
      SquaredDistance:=C;
     end else if (-B1)>=A11 then begin
      t:=1.0;
      SquaredDistance:=A11+(2.0*B1)+C;
     end else begin
      t:=(-B1)/A11;
      SquaredDistance:=(B1*t)+C;
     end;
    end;
   end else begin // region 3
    s:=0.0;
    if B1>=0.0 then begin
     t:=0.0;
     SquaredDistance:=C;
    end else if (-B1)>=A11 then begin
     t:=1.0;
     SquaredDistance:=A11+(2.0*B1)+C;
    end else begin
     t:=(-B1)/A11;
     SquaredDistance:=(B1*t)+C;
    end;
   end;
  end else if t<0.0 then begin // region 5
   t:=0.0;
   if B0>=0.0 then begin
    s:=0.0;
    SquaredDistance:=C;
   end else if (-B0)>=A00 then begin
    s:=1.0;
    SquaredDistance:=A00+(2.0*B0)+C;
   end else begin
    s:=(-B0)/A00;
    SquaredDistance:=(B0*s)+C;
   end;
  end else begin // region 0
   // minimum at interior point
   InvDet:=1.0/Det;
   s:=s*InvDet;
   t:=t*InvDet;
   SquaredDistance:=(s*((A00*s)+(A01*t)+(2.0*B0)))+(t*((A01*s)+(A11*t)+(2.0*B1)))+C;
  end;
 end else begin
  if s<0.0 then begin // region 2
   Tmp0:=A01+B0;
   Tmp1:=A11+B1;
   if Tmp1>Tmp0 then begin
    Numer:=Tmp1-Tmp0;
    Denom:=A00-(2.0*A01)+A11;
    if Numer>=Denom then begin
     s:=1.0;
     t:=0.0;
     SquaredDistance:=A00+(2.0*B0)+C;
    end else begin
     s:=Numer/Denom;
     t:=1.0-s;
     SquaredDistance:=(s*((A00*s)+(A01*t)+(2*B0)))+(t*((A01*s)+(A11*t)+(2*B1)))+C;
    end;
   end else begin
    s:=0.0;
    if Tmp1<=0.0 then begin
     t:=1.0;
     SquaredDistance:=A11+(2.0*B1)+C;
    end else if B1>=0.0 then begin
     t:=0.0;
     SquaredDistance:=C;
    end else begin
     t:=(-B1)/A11;
     SquaredDistance:=(B1*t)+C;
    end;
   end;
  end else if t<0.0 then begin // region 6
   Tmp0:=A01+B1;
   Tmp1:=A00+B0;
   if Tmp1>Tmp0 then begin
    Numer:=Tmp1-Tmp0;
    Denom:=A00-(2.0*A01)+A11;
    if Numer>=Denom then begin
     t:=1.0;
     s:=0.0;
     SquaredDistance:=A11+(2*B1)+C;
    end else begin
     t:=Numer/Denom;
     s:=1.0-t;
     SquaredDistance:=(s*((A00*s)+(A01*t)+(2.0*B0)))+(t*((A01*s)+(A11*t)+(2.0*B1)))+C;
    end;
   end else begin
    t:=0.0;
    if Tmp1<=0.0 then begin
     s:=1.0;
     SquaredDistance:=A00+(2.0*B0)+C;
    end else if B0>=0.0 then begin
     s:=0.0;
     SquaredDistance:=C;
    end else begin
     s:=(-B0)/A00;
     SquaredDistance:=(B0*s)+C;
    end;
   end;
  end else begin // region 1
   Numer:=((A11+B1)-A01)-B0;
   if Numer<=0.0 then begin
    s:=0.0;
    t:=1.0;
    SquaredDistance:=A11+(2.0*B1)+C;
   end else begin
    Denom:=A00-(2.0*A01)+A11;
    if Numer>=Denom then begin
     s:=1.0;
     t:=0.0;
     SquaredDistance:=A00+(2.0*B0)+C;
    end else begin
     s:=Numer/Denom;
     t:=1.0-s;
     SquaredDistance:=(s*((A00*s)+(A01*t)+(2.0*B0)))+(t*((A01*s)+(A11*t)+(2.0*B1)))+C;
    end;
   end;
  end;
 end;
 ClosestPoint.x:=Points[0].x+((Edge0.x*s)+(Edge1.x*t));
 ClosestPoint.y:=Points[0].y+((Edge0.y*s)+(Edge1.y*t));
 ClosestPoint.z:=Points[0].z+((Edge0.z*s)+(Edge1.z*t));
 result:=abs(SquaredDistance);
end;

function TTriangle.DistanceTo(const Point:TVector3):TScalar;
var SegmentTriangle:TSegmentTriangle;
    s,t:TScalar;
begin
 SegmentTriangle.Origin:=Points[0];
 SegmentTriangle.Edge0:=Points[1]-Points[0];
 SegmentTriangle.Edge1:=Points[2]-Points[0];
 SegmentTriangle.Edge2:=SegmentTriangle.Edge1-SegmentTriangle.Edge0;
{result:=}SegmentTriangle.SquaredPointTriangleDistance(Point,s,t);
{if result>EPSILON then begin
  result:=sqrt(result);
 end else if result<EPSILON then begin
  result:=-sqrt(-result);
 end else begin
  result:=0;
 end;}
 result:=Point.DistanceTo(SegmentTriangle.Origin+((SegmentTriangle.Edge0*s)+(SegmentTriangle.Edge1*t)));
end;

function TTriangle.SquaredDistanceTo(const Point:TVector3):TScalar;
var SegmentTriangle:TSegmentTriangle;
    s,t:TScalar;
begin
 SegmentTriangle.Origin:=Points[0];
 SegmentTriangle.Edge0:=Points[1]-Points[0];
 SegmentTriangle.Edge1:=Points[2]-Points[0];
 SegmentTriangle.Edge2:=SegmentTriangle.Edge1-SegmentTriangle.Edge0;
 result:=SegmentTriangle.SquaredPointTriangleDistance(Point,s,t);
end;

function TTriangle.RayIntersection(const RayOrigin,RayDirection:TVector3;var Time,u,v:TScalar):boolean;
var e0,e1,p,t,q:TVector3;
    Determinant,InverseDeterminant:TScalar;
begin
 result:=false;

 e0.x:=Points[1].x-Points[0].x;
 e0.y:=Points[1].y-Points[0].y;
 e0.z:=Points[1].z-Points[0].z;
 e1.x:=Points[2].x-Points[0].x;
 e1.y:=Points[2].y-Points[0].y;
 e1.z:=Points[2].z-Points[0].z;

 p.x:=(RayDirection.y*e1.z)-(RayDirection.z*e1.y);
 p.y:=(RayDirection.z*e1.x)-(RayDirection.x*e1.z);
 p.z:=(RayDirection.x*e1.y)-(RayDirection.y*e1.x);

 Determinant:=(e0.x*p.x)+(e0.y*p.y)+(e0.z*p.z);
 if Determinant<EPSILON then begin
  exit;
 end;

 t.x:=RayOrigin.x-Points[0].x;
 t.y:=RayOrigin.y-Points[0].y;
 t.z:=RayOrigin.z-Points[0].z;

 u:=(t.x*p.x)+(t.y*p.y)+(t.z*p.z);
 if (u<0.0) or (u>Determinant) then begin
  exit;
 end;

 q.x:=(t.y*e0.z)-(t.z*e0.y);
 q.y:=(t.z*e0.x)-(t.x*e0.z);
 q.z:=(t.x*e0.y)-(t.y*e0.x);

 v:=(RayDirection.x*q.x)+(RayDirection.y*q.y)+(RayDirection.z*q.z);
 if (v<0.0) or ((u+v)>Determinant) then begin
  exit;
 end;

 Time:=(e1.x*q.x)+(e1.y*q.y)+(e1.z*q.z);
 if abs(Determinant)<EPSILON then begin
  Determinant:=0.01;
 end;
 InverseDeterminant:=1.0/Determinant;
 Time:=Time*InverseDeterminant;
 u:=u*InverseDeterminant;
 v:=v*InverseDeterminant;

 result:=true;
end;

function TSegmentTriangle.RelativeSegmentIntersection(const pSegment:TRelativeSegment;out tS,tT0,tT1:TScalar):boolean;
var u,v,t,a,f:TScalar;
    e1,e2,p,s,q:TVector3;
begin
 result:=false;
 tS:=0.0;
 tT0:=0.0;
 tT1:=0.0;

 e1:=Edge0;
 e2:=Edge1;

 p:=pSegment.Delta.Cross(e2);
 a:=e1.Dot(p);
 if abs(a)<EPSILON then begin
  exit;
 end;

 f:=1.0/a;

 s:=pSegment.Origin-Origin;
 u:=f*s.Dot(p);
 if (u<0.0) or (u>1.0) then begin
  exit;
 end;

 q:=s.Cross(e1);
 v:=f*pSegment.Delta.Dot(q);
 if (v<0.0) or ((u+v)>1.0) then begin
  exit;
 end;

 t:=f*e2.Dot(q);
 if (t<0.0) or (t>1.0) then begin
  exit;
 end;

 tS:=t;
 tT0:=u;
 tT1:=v;

 result:=true;
end;

function TSegmentTriangle.SquaredPointTriangleDistance(const pPoint:TVector3;out pfSParam,pfTParam:TScalar):TScalar;
var kDiff:TVector3;
    fA00,fA01,fA11,fB0,fC,fDet,fB1,fS,fT,fSqrDist,fInvDet,fTmp0,fTmp1,fNumer,fDenom:TScalar;
begin
 kDiff:=self.Origin-pPoint;
 fA00:=self.Edge0.SquaredLength;
 fA01:=self.Edge0.Dot(self.Edge1);
 fA11:=self.Edge1.SquaredLength;
 fB0:=kDiff.Dot(self.Edge0);
 fB1:=kDiff.Dot(self.Edge1);
 fC:=kDiff.SquaredLength;
 fDet:=max(abs((fA00*fA11)-(fA01*fA01)),EPSILON);
 fS:=(fA01*fB1)-(fA11*fB0);
 fT:=(fA01*fB0)-(fA00*fB1);
 if (fS+fT)<=fDet then begin
  if fS<0.0 then begin
   if fT<0.0 then begin // region 4
    if fB0<0.0 then begin
     fT:=0.0;
     if (-fB0)>=fA00 then begin
      fS:=1.0;
      fSqrDist:=fA00+(2.0*fB0)+fC;
     end else begin
      fS:=(-fB0)/fA00;
      fSqrDist:=(fB0*fS)+fC;
     end;
    end else begin
     fS:=0.0;
     if fB1>=0.0 then begin
      fT:=0.0;
      fSqrDist:=fC;
     end else if (-fB1)>=fA11 then begin
      fT:=1.0;
      fSqrDist:=fA11+(2.0*fB1)+fC;
     end else begin
      fT:=(-fB1)/fA11;
      fSqrDist:=(fB1*fT)+fC;
     end;
    end;
   end else begin // region 3
    fS:=0.0;
    if fB1>=0.0 then begin
     fT:=0.0;
     fSqrDist:=fC;
    end else if (-fB1)>=fA11 then begin
     fT:=1.0;
     fSqrDist:=fA11+(2.0*fB1)+fC;
    end else begin
     fT:=(-fB1)/fA11;
     fSqrDist:=(fB1*fT)+fC;
    end;
   end;
  end else if fT<0.0 then begin // region 5
   fT:=0.0;
   if fB0>=0.0 then begin
    fS:=0.0;
    fSqrDist:=fC;
   end else if (-fB0)>=fA00 then begin
    fS:=1.0;
    fSqrDist:=fA00+(2.0*fB0)+fC;
   end else begin
    fS:=(-fB0)/fA00;
    fSqrDist:=(fB0*fS)+fC;
   end;
  end else begin // region 0
   // minimum at interior point
   fInvDet:=1.0/fDet;
   fS:=fS*fInvDet;
   fT:=fT*fInvDet;
   fSqrDist:=(fS*((fA00*fS)+(fA01*fT)+(2.0*fB0)))+(fT*((fA01*fS)+(fA11*fT)+(2.0*fB1)))+fC;
  end;
 end else begin
  if fS<0.0 then begin // region 2
   fTmp0:=fA01+fB0;
   fTmp1:=fA11+fB1;
   if fTmp1>fTmp0 then begin
    fNumer:=fTmp1-fTmp0;
    fDenom:=fA00-(2.0*fA01)+fA11;
    if fNumer>=fDenom then begin
     fS:=1.0;
     fT:=0.0;
     fSqrDist:=fA00+(2.0*fB0)+fC;
    end else begin
     fS:=fNumer/fDenom;
     fT:=1.0-fS;
     fSqrDist:=(fS*((fA00*fS)+(fA01*fT)+(2*fB0)))+(fT*((fA01*fS)+(fA11*fT)+(2*fB1)))+fC;
    end;
   end else begin
    fS:=0.0;
    if fTmp1<=0.0 then begin
     fT:=1.0;
     fSqrDist:=fA11+(2.0*fB1)+fC;
    end else if fB1>=0.0 then begin
     fT:=0.0;
     fSqrDist:=fC;
    end else begin
     fT:=(-fB1)/fA11;
     fSqrDist:=(fB1*fT)+fC;
    end;
   end;
  end else if fT<0.0 then begin // region 6
   fTmp0:=fA01+fB1;
   fTmp1:=fA00+fB0;
   if fTmp1>fTmp0 then begin
    fNumer:=fTmp1-fTmp0;
    fDenom:=fA00-(2.0*fA01)+fA11;
    if fNumer>=fDenom then begin
     fT:=1.0;
     fS:=0.0;
     fSqrDist:=fA11+(2*fB1)+fC;
    end else begin
     fT:=fNumer/fDenom;
     fS:=1.0-fT;
     fSqrDist:=(fS*((fA00*fS)+(fA01*fT)+(2.0*fB0)))+(fT*((fA01*fS)+(fA11*fT)+(2.0*fB1)))+fC;
    end;
   end else begin
    fT:=0.0;
    if fTmp1<=0.0 then begin
     fS:=1.0;
     fSqrDist:=fA00+(2.0*fB0)+fC;
    end else if fB0>=0.0 then begin
     fS:=0.0;
     fSqrDist:=fC;
    end else begin
     fS:=(-fB0)/fA00;
     fSqrDist:=(fB0*fS)+fC;
    end;
   end;
  end else begin // region 1
   fNumer:=((fA11+fB1)-fA01)-fB0;
   if fNumer<=0.0 then begin
    fS:=0.0;
    fT:=1.0;
    fSqrDist:=fA11+(2.0*fB1)+fC;
   end else begin
    fDenom:=fA00-(2.0*fA01)+fA11;
    if fNumer>=fDenom then begin
     fS:=1.0;
     fT:=0.0;
     fSqrDist:=fA00+(2.0*fB0)+fC;
    end else begin
     fS:=fNumer/fDenom;
     fT:=1.0-fS;
     fSqrDist:=(fS*((fA00*fS)+(fA01*fT)+(2.0*fB0)))+(fT*((fA01*fS)+(fA11*fT)+(2.0*fB1)))+fC;
    end;
   end;
  end;
 end;
 pfSParam:=fS;
 pfTParam:=fT;
 result:=abs(fSqrDist);
end;

function TSegmentTriangle.SquaredDistanceTo(const pRelativeSegment:TRelativeSegment;out segT,triT0,triT1:TScalar):TScalar;
var s,t,u,distEdgeSq,startTriSq,endTriSq:TScalar;
    tseg:TRelativeSegment;
begin
 result:=INFINITY;
 if RelativeSegmentIntersection(pRelativeSegment,segT,triT0,triT1) then begin
  segT:=0.0;
  triT0:=0.0;
  triT1:=0.0;
  result:=0.0;
  exit;
 end;
 tseg.Origin:=Origin;
 tseg.Delta:=Edge0;
 distEdgeSq:=pRelativeSegment.SquaredSegmentDistanceTo(tseg,s,t);
 if distEdgeSq<result then begin
  result:=distEdgeSq;
  segT:=s;
  triT0:=t;
  triT1:=0.0;
 end;
 tseg.Delta:=Edge1;
 distEdgeSq:=pRelativeSegment.SquaredSegmentDistanceTo(tseg,s,t);
 if distEdgeSq<result then begin
  result:=distEdgeSq;
  segT:=s;
  triT0:=0.0;
  triT1:=t;
 end;
 tseg.Origin:=Origin+Edge1;
 tseg.Delta:=Edge2;
 distEdgeSq:=pRelativeSegment.SquaredSegmentDistanceTo(tseg,s,t);
 if distEdgeSq<result then begin
  result:=distEdgeSq;
  segT:=s;
  triT0:=1.0-t;
  triT1:=t;
 end;
 startTriSq:=SquaredPointTriangleDistance(pRelativeSegment.Origin,t,u);
 if startTriSq<result then begin
  result:=startTriSq;
  segT:=0.0;
  triT0:=t;
  triT1:=u;
 end;
 endTriSq:=SquaredPointTriangleDistance(pRelativeSegment.Origin+pRelativeSegment.Delta,t,u);
 if endTriSq<result then begin
  result:=endTriSq;
  segT:=1.0;
  triT0:=t;
  triT1:=u;
 end;
end;

procedure TOBB.ProjectToVector(const Vector:TVector3;out OBBMin,OBBMax:TScalar);
var ProjectionCenter,ProjectionRadius:TScalar;
begin
 ProjectionCenter:=Center.Dot(Vector);
 ProjectionRadius:=abs(Vector.Dot(Axis[0])*Extents.x)+
                   abs(Vector.Dot(Axis[1])*Extents.y)+
                   abs(Vector.Dot(Axis[2])*Extents.z);
 OBBMin:=ProjectionCenter-ProjectionRadius;
 OBBMax:=ProjectionCenter+ProjectionRadius;
end;

function TOBB.RelativeSegmentIntersection(const pRelativeSegment:TRelativeSegment;out fracOut:TScalar;out posOut,NormalOut:TVector3):boolean;
var min_,max_,e,f,t1,t2,t:TScalar;
    p{,h}:TVector3;
    dirMax,dirMin,dir:TInt32;
begin
 result:=false;

 fracOut:=1e+34;
 posOut:=Vector3Origin;
 normalOut:=Vector3Origin;

 min_:=-1e+34;
 max_:=1e+34;

 p:=Center-pRelativeSegment.Origin;
//h:=Extents;

 dirMax:=0;
 dirMin:=0;

 for dir:=0 to 2 do begin
  e:=Axis[Dir].Dot(p);
  f:=Axis[Dir].Dot(pRelativeSegment.Delta);
  if abs(f)>EPSILON then begin
   t1:=(e+Extents.RawComponents[dir])/f;
   t2:=(e-Extents.RawComponents[dir])/f;
   if t1>t2 then begin
    t:=t1;
    t1:=t2;
    t2:=t;
   end;
   if min_<t1 then begin
    min_:=t1;
    dirMin:=dir;
   end;
   if max_>t2 then begin
    max_:=t2;
    dirMax:=dir;
   end;
   if (min_>max_) or (max_<0.0) then begin
    exit;
   end;
  end else if (((-e)-Extents.RawComponents[dir])>0.0) or (((-e)+Extents.RawComponents[dir])<0.0) then begin
   exit;
  end;
 end;

 if min_>0.0 then begin
  dir:=dirMin;
  fracOut:=min_;
 end else begin
  dir:=dirMax;
  fracOut:=max_;
 end;

 fracOut:=Min(Max(fracOut,0.0),1.0);

 posOut:=pRelativeSegment.Origin+(pRelativeSegment.Delta*fracOut);

 if Axis[dir].Dot(pRelativeSegment.Delta)>0.0 then begin
  normalOut:=-Axis[dir];
 end else begin
  normalOut:=Axis[dir];
 end;

 result:=true;
end;

function TOBB.TriangleIntersection(const Triangle:TTriangle;out Position,Normal:TVector3;out Penetration:TScalar):boolean;
const OBBEdges:array[0..11,0..1] of TInt32=
       ((0,1),
        (0,3),
        (0,4),
        (1,2),
        (1,5),
        (2,3),
        (2,6),
        (3,7),
        (4,5),
        (4,7),
        (5,6),
        (6,7));
      ModuloThree:array[0..5] of TInt32=(0,1,2,0,1,2);
var OBBVertices:array[0..7] of TVector3;
    TriangleVertices,TriangleEdges:array[0..2] of TVector3;
    TriangleNormal,BestAxis,CurrentAxis,v,p,n,pt0,pt1:TVector3;
    BestPenetration,CurrentPenetration,tS,tT0,tT1:TScalar;
    BestAxisIndex,i,j:TInt32;
    seg,s1,s2:TRelativeSegment;
    SegmentTriangle:TSegmentTriangle;
begin
 result:=false;

 // ---
 OBBVertices[0]:=self.Center+(self.Axis[0]*(-self.Extents.x))+(self.Axis[1]*(-self.Extents.y))+(self.Axis[2]*(-self.Extents.z));
 // +--
 OBBVertices[1]:=self.Center+(self.Axis[0]*self.Extents.x)+(self.Axis[1]*(-self.Extents.y))+(self.Axis[2]*(-self.Extents.z));
 // ++-
 OBBVertices[2]:=self.Center+(self.Axis[0]*self.Extents.x)+(self.Axis[1]*self.Extents.y)+(self.Axis[2]*(-self.Extents.z));
 // -+-
 OBBVertices[3]:=self.Center+(self.Axis[0]*(-self.Extents.x))+(self.Axis[1]*self.Extents.y)+(self.Axis[2]*(-self.Extents.z));
 // --+
 OBBVertices[4]:=self.Center+(self.Axis[0]*(-self.Extents.x))+(self.Axis[1]*(-self.Extents.y))+(self.Axis[2]*self.Extents.z);
 // +-+
 OBBVertices[5]:=self.Center+(self.Axis[0]*self.Extents.x)+(self.Axis[1]*(-self.Extents.y))+(self.Axis[2]*self.Extents.z);
 // +++
 OBBVertices[6]:=self.Center+(self.Axis[0]*self.Extents.x)+(self.Axis[1]*self.Extents.y)+(self.Axis[2]*self.Extents.z);
 // -++
 OBBVertices[7]:=self.Center+(self.Axis[0]*(-self.Extents.x))+(self.Axis[1]*(-self.Extents.y))+(self.Axis[2]*self.Extents.z);

 TriangleVertices[0]:=Triangle.Points[0];
 TriangleVertices[1]:=Triangle.Points[1];
 TriangleVertices[2]:=Triangle.Points[2];

 TriangleEdges[0]:=Triangle.Points[1]-Triangle.Points[0];
 TriangleEdges[1]:=Triangle.Points[2]-Triangle.Points[1];
 TriangleEdges[2]:=Triangle.Points[0]-Triangle.Points[2];

 TriangleNormal:=TriangleEdges[0].Cross(TriangleEdges[1]).Normalize;

 BestPenetration:=0;
 BestAxis:=Vector3Origin;
 BestAxisIndex:=-1;

 for i:=0 to 2 do begin
  CurrentPenetration:=DoSpanIntersect(@OBBVertices[0],8,@TriangleVertices[0],3,self.Axis[i],CurrentAxis);
  if CurrentPenetration<0.0 then begin
   exit;
  end else if (i=0) or (CurrentPenetration<BestPenetration) then begin
   BestPenetration:=CurrentPenetration;
   BestAxis:=CurrentAxis;
   BestAxisIndex:=i;
  end;
 end;

 CurrentPenetration:=DoSpanIntersect(@OBBVertices[0],8,@TriangleVertices[0],3,TriangleNormal,CurrentAxis);
 if CurrentPenetration<0.0 then begin
  exit;
 end else if CurrentPenetration<BestPenetration then begin
  BestPenetration:=CurrentPenetration;
  BestAxis:=CurrentAxis;
  BestAxisIndex:=3;
 end;

 for i:=0 to 2 do begin
  for j:=0 to 2 do begin
   CurrentPenetration:=DoSpanIntersect(@OBBVertices[0],8,@TriangleVertices[0],3,self.Axis[i].Cross(TriangleEdges[j]),CurrentAxis);
   if CurrentPenetration<0.0 then begin
    exit;
   end else if CurrentPenetration<BestPenetration then begin
    BestPenetration:=CurrentPenetration;
    BestAxis:=CurrentAxis;
    BestAxisIndex:=((i*3)+j)+4;
   end;
  end;
 end;

 Penetration:=BestPenetration;
 Normal:=BestAxis;

 if BestAxisIndex>=0 then begin
  j:=0;
  v:=Vector3Origin;
  SegmentTriangle.Origin:=Triangle.Points[0];
  SegmentTriangle.Edge0:=Triangle.Points[1]-Triangle.Points[0];
  SegmentTriangle.Edge1:=Triangle.Points[2]-Triangle.Points[0];
  for i:=0 to 11 do begin
   seg.Origin:=OBBVertices[OBBEdges[i,0]];
   seg.Delta:=OBBVertices[OBBEdges[i,1]]-OBBVertices[OBBEdges[i,0]];
   if SegmentTriangle.RelativeSegmentIntersection(seg,tS,tT0,tT1) then begin
    v:=v+seg.Origin+(seg.Delta*tS);
    inc(j);
   end;
  end;
  for i:=0 to 2 do begin
   pt0:=TriangleVertices[i];
   pt1:=TriangleVertices[ModuloThree[i+1]];
   s1.Origin:=pt0;
   s1.Delta:=pt1-pt0;
   s2.Origin:=pt1;
   s2.Delta:=pt0-pt1;
   if RelativeSegmentIntersection(s1,tS,p,n) then begin
    v:=v+p;
    inc(j);
   end;
   if RelativeSegmentIntersection(s2,tS,p,n) then begin
    v:=v+p;
    inc(j);
   end;
  end;
  if j>0 then begin
   Position:=v/j;
  end else begin
   ClosestPointToOBB(self,(Triangle.Points[0]+Triangle.Points[1]+Triangle.Points[2])/3.0,Position);
  end;
 end;

 result:=true;

end;

function TOBB.TriangleIntersection(const pTriangle:TTriangle;const MTV:PVector3=nil):boolean;
var TriangleEdges:array[0..2] of TVector3;
    TriangleNormal{,d},ProjectionVector:TVector3;
    TriangleMin,TriangleMax,OBBMin,OBBMax,Projection,BestOverlap,Overlap:TScalar;
    OBBAxisIndex,TriangleEdgeIndex:TInt32;
    BestAxis,TheAxis:TVector3;
begin
 result:=false;

 TriangleEdges[0]:=pTriangle.Points[1]-pTriangle.Points[0];
 TriangleEdges[1]:=pTriangle.Points[2]-pTriangle.Points[0];
 TriangleEdges[2]:=pTriangle.Points[2]-pTriangle.Points[1];

 TriangleNormal:=TriangleEdges[0].Cross(TriangleEdges[1]);

 //d:=TriangleEdges[0]-Center;

 TriangleMin:=TriangleNormal.Dot(pTriangle.Points[0]);
 TriangleMax:=TriangleMin;
 ProjectToVector(TriangleNormal,OBBMin,OBBMax);
 if (TriangleMin>OBBMax) or (TriangleMax<OBBMin) then begin
  exit;
 end;
 BestAxis:=TriangleNormal;
 BestOverlap:=GetOverlap(OBBMin,OBBMax,TriangleMin,TriangleMax);

 for OBBAxisIndex:=0 to 2 do begin
  TheAxis:=self.Axis[OBBAxisIndex];
  pTriangle.ProjectToVector(TheAxis,TriangleMin,TriangleMax);
  Projection:=TheAxis.Dot(Center);
  OBBMin:=Projection-Extents.RawComponents[OBBAxisIndex];
  OBBMax:=Projection+Extents.RawComponents[OBBAxisIndex];
  if (TriangleMin>OBBMax) or (TriangleMax<OBBMin) then begin
   exit;
  end;
  Overlap:=GetOverlap(OBBMin,OBBMax,TriangleMin,TriangleMax);
  if Overlap<BestOverlap then begin
   BestAxis:=TheAxis;
   BestOverlap:=Overlap;
  end;
 end;

 for OBBAxisIndex:=0 to 2 do begin
  for TriangleEdgeIndex:=0 to 2 do begin
   ProjectionVector:=TriangleEdges[TriangleEdgeIndex].Cross(Axis[OBBAxisIndex]);
   ProjectToVector(ProjectionVector,OBBMin,OBBMax);
   pTriangle.ProjectToVector(ProjectionVector,TriangleMin,TriangleMax);
   if (TriangleMin>OBBMax) or (TriangleMax<OBBMin) then begin
    exit;
   end;
   Overlap:=GetOverlap(OBBMin,OBBMax,TriangleMin,TriangleMax);
   if Overlap<BestOverlap then begin
    BestAxis:=ProjectionVector;
    BestOverlap:=Overlap;
   end;
  end;
 end;

 if assigned(MTV) then begin
  MTV^:=BestAxis*BestOverlap;
 end;

 result:=true;
end;

constructor TAABB.Create(const pMin,pMax:TVector3);
begin
 Min:=pMin;
 Max:=pMax;
end;

constructor TAABB.CreateFromOBB(const OBB:TOBB);
var t:TVector3;
begin
 t.x:=abs((OBB.Matrix[0,0]*OBB.Extents.x)+(OBB.Matrix[1,0]*OBB.Extents.y)+(OBB.Matrix[2,0]*OBB.Extents.z));
 t.y:=abs((OBB.Matrix[0,1]*OBB.Extents.x)+(OBB.Matrix[1,1]*OBB.Extents.y)+(OBB.Matrix[2,1]*OBB.Extents.z));
 t.z:=abs((OBB.Matrix[0,2]*OBB.Extents.x)+(OBB.Matrix[1,2]*OBB.Extents.y)+(OBB.Matrix[2,2]*OBB.Extents.z));
 Min.x:=OBB.Center.x-t.x;
 Min.y:=OBB.Center.y-t.y;
 Min.z:=OBB.Center.z-t.z;
 Max.x:=OBB.Center.x+t.x;
 Max.y:=OBB.Center.y+t.y;
 Max.z:=OBB.Center.z+t.z;
end;

function TAABB.Cost:TScalar;
begin
 result:=(Max.x-Min.x)+(Max.y-Min.y)+(Max.z-Min.z); // Manhattan distance
end;

function TAABB.Volume:TScalar;
begin
 result:=(Max.x-Min.x)*(Max.y-Min.y)*(Max.z-Min.z); // Volume
end;

function TAABB.Area:TScalar;
begin
 result:=2.0*((abs(Max.x-Min.x)*abs(Max.y-Min.y))+
              (abs(Max.y-Min.y)*abs(Max.z-Min.z))+
              (abs(Max.x-Min.x)*abs(Max.z-Min.z)));
end;

function TAABB.Flip:TAABB;
var a,b:TVector3;
begin
 a:=Min.Flip;
 b:=Max.Flip;
 result.Min.x:=Math.Min(a.x,b.x);
 result.Min.y:=Math.Min(a.y,b.y);
 result.Min.z:=Math.Min(a.z,b.z);
 result.Max.x:=Math.Max(a.x,b.x);
 result.Max.y:=Math.Max(a.y,b.y);
 result.Max.z:=Math.Max(a.z,b.z);
end;

function TAABB.SquareMagnitude:TScalar;
begin
 result:=sqr(Max.x-Min.x)+(Max.y-Min.y)+sqr(Max.z-Min.z);
end;

function TAABB.Resize(const f:TScalar):TAABB;
var v:TVector3;
begin
 v:=(Max-Min)*f;
 result.Min:=Min-v;
 result.Max:=Max+v;
end;

function TAABB.Combine(const WithAABB:TAABB):TAABB;
begin
 result.Min.x:=Math.Min(Min.x,WithAABB.Min.x);
 result.Min.y:=Math.Min(Min.y,WithAABB.Min.y);
 result.Min.z:=Math.Min(Min.z,WithAABB.Min.z);
 result.Max.x:=Math.Max(Max.x,WithAABB.Max.x);
 result.Max.y:=Math.Max(Max.y,WithAABB.Max.y);
 result.Max.z:=Math.Max(Max.z,WithAABB.Max.z);
end;

function TAABB.CombineVector3(v:TVector3):TAABB;
begin
 result.Min.x:=Math.Min(Min.x,v.x);
 result.Min.y:=Math.Min(Min.y,v.y);
 result.Min.z:=Math.Min(Min.z,v.z);
 result.Max.x:=Math.Max(Max.x,v.x);
 result.Max.y:=Math.Max(Max.y,v.y);
 result.Max.z:=Math.Max(Max.z,v.z);
end;

function TAABB.DistanceTo(const ToAABB:TAABB):TScalar;
begin
 result:=0.0;
 if Min.x>ToAABB.Max.x then begin
  result:=result+sqr(ToAABB.Max.x-Min.x);
 end else if ToAABB.Min.x>Max.x then begin
  result:=result+sqr(Max.x-ToAABB.Min.x);
 end;
 if Min.y>ToAABB.Max.y then begin
  result:=result+sqr(ToAABB.Max.y-Min.y);
 end else if ToAABB.Min.y>Max.y then begin
  result:=result+sqr(Max.y-ToAABB.Min.y);
 end;
 if Min.z>ToAABB.Max.z then begin
  result:=result+sqr(ToAABB.Max.z-Min.z);
 end else if ToAABB.Min.z>Max.z then begin
  result:=result+sqr(Max.z-ToAABB.Min.z);
 end;
 if result>0.0 then begin
  result:=sqrt(result);
 end;
end;

function TAABB.Radius:TScalar;
begin
 result:=Math.Max(Min.DistanceTo((Min+Max)*0.5),Max.DistanceTo((Min+Max)*0.5));
end;

function TAABB.Compare(const WithAABB:TAABB):boolean;
begin
 result:=(Min=WithAABB.Min) and (Max=WithAABB.Max);
end;

function TAABB.Intersect(const WithAABB:TAABB;Threshold:TScalar=EPSILON):boolean;
begin
 result:=(((Max.x+Threshold)>=(WithAABB.Min.x-Threshold)) and ((Min.x-Threshold)<=(WithAABB.Max.x+Threshold))) and
         (((Max.y+Threshold)>=(WithAABB.Min.y-Threshold)) and ((Min.y-Threshold)<=(WithAABB.Max.y+Threshold))) and
         (((Max.z+Threshold)>=(WithAABB.Min.z-Threshold)) and ((Min.z-Threshold)<=(WithAABB.Max.z+Threshold)));
end;

function TAABB.Contains(const AABB:TAABB):boolean;
begin
 result:=((Min.x-EPSILON)<=(AABB.Min.x+EPSILON)) and ((Min.y-EPSILON)<=(AABB.Min.y+EPSILON)) and ((Min.z-EPSILON)<=(AABB.Min.z+EPSILON)) and
         ((Max.x+EPSILON)>=(AABB.Min.x+EPSILON)) and ((Max.y+EPSILON)>=(AABB.Min.y+EPSILON)) and ((Max.z+EPSILON)>=(AABB.Min.z+EPSILON)) and
         ((Min.x-EPSILON)<=(AABB.Max.x-EPSILON)) and ((Min.y-EPSILON)<=(AABB.Max.y-EPSILON)) and ((Min.z-EPSILON)<=(AABB.Max.z-EPSILON)) and
         ((Max.x+EPSILON)>=(AABB.Max.x-EPSILON)) and ((Max.y+EPSILON)>=(AABB.Max.y-EPSILON)) and ((Max.z+EPSILON)>=(AABB.Max.z-EPSILON));
end;

function TAABB.Contains(const Vector:TVector3):boolean;
begin
 result:=((Vector.x>=(Min.x-EPSILON)) and (Vector.x<=(Max.x+EPSILON))) and
         ((Vector.y>=(Min.y-EPSILON)) and (Vector.y<=(Max.y+EPSILON))) and
         ((Vector.z>=(Min.z-EPSILON)) and (Vector.z<=(Max.z+EPSILON)));
end;

function TAABB.Touched(const Vector:TVector3;const Threshold:TScalar=1e-5):boolean;
begin
 result:=((Vector.x>=(Min.x-Threshold)) and (Vector.x<=(Max.x+Threshold))) and
         ((Vector.y>=(Min.y-Threshold)) and (Vector.y<=(Max.y+Threshold))) and
         ((Vector.z>=(Min.z-Threshold)) and (Vector.z<=(Max.z+Threshold)));
end;

function TAABB.GetIntersection(const WithAABB:TAABB):TAABB;
begin
 result.Min.x:=Math.Max(Min.x,WithAABB.Min.x);
 result.Min.y:=Math.Max(Min.y,WithAABB.Min.y);
 result.Min.z:=Math.Max(Min.z,WithAABB.Min.z);
 result.Max.x:=Math.Min(Max.x,WithAABB.Max.x);
 result.Max.y:=Math.Min(Max.y,WithAABB.Max.y);
 result.Max.z:=Math.Min(Max.z,WithAABB.Max.z);
end;

function TAABB.FastRayIntersection(const Origin,Direction:TVector3):boolean;
var Center,BoxExtents,Diff:TVector3;
begin
 Center:=(Min+Max)*0.5;
 BoxExtents:=Center-Min;
 Diff:=Origin-Center;
 result:=not ((((abs(Diff.x)>BoxExtents.x) and ((Diff.x*Direction.x)>=0)) or
               ((abs(Diff.y)>BoxExtents.y) and ((Diff.y*Direction.y)>=0)) or
               ((abs(Diff.z)>BoxExtents.z) and ((Diff.z*Direction.z)>=0))) or
              ((abs((Direction.y*Diff.z)-(Direction.z*Diff.y))>((BoxExtents.y*abs(Direction.z))+(BoxExtents.z*abs(Direction.y)))) or
               (abs((Direction.z*Diff.x)-(Direction.x*Diff.z))>((BoxExtents.x*abs(Direction.z))+(BoxExtents.z*abs(Direction.x)))) or
               (abs((Direction.x*Diff.y)-(Direction.y*Diff.x))>((BoxExtents.x*abs(Direction.y))+(BoxExtents.y*abs(Direction.x))))));
end;

function TAABB.RayIntersectionHitDistance(const Origin,Direction:TVector3;var HitDist:TScalar):boolean;
var DirFrac:TVector3;
    t:array[0..5] of TScalar;
    tMin,tMax:TScalar;
begin
 DirFrac.x:=1.0/Direction.x;
 DirFrac.y:=1.0/Direction.y;
 DirFrac.z:=1.0/Direction.z;
 t[0]:=(Min.x-Origin.x)*DirFrac.x;
 t[1]:=(Max.x-Origin.x)*DirFrac.x;
 t[2]:=(Min.y-Origin.y)*DirFrac.y;
 t[3]:=(Max.y-Origin.y)*DirFrac.y;
 t[4]:=(Min.z-Origin.z)*DirFrac.z;
 t[5]:=(Max.z-Origin.z)*DirFrac.z;
 tMin:=Math.Max(Math.Max(Math.Min(t[0],t[1]),Math.Min(t[2],t[3])),Math.Min(t[4],t[5]));
 tMax:=Math.Min(Math.Min(Math.Max(t[0],t[1]),Math.Max(t[2],t[3])),Math.Max(t[4],t[5]));
 if (tMax<0) or (tMin>tMax) then begin
  HitDist:=tMax;
  result:=false;
 end else begin
  HitDist:=tMin;
  result:=true;
 end;
end;

function TAABB.RayIntersectionHitPoint(const Origin,Direction:TVector3;var HitPoint:TVector3):boolean;
const RIGHT=0;
      LEFT=1;
      MIDDLE=2;
var i,WhicHPlane:TInt32;
    Inside:longbool;
    Quadrant:array[0..2] of TInt32;
    MaxT,CandidatePlane:TVector3;
begin
 Inside:=true;
 for i:=0 to 2 do begin
  if Origin.RawComponents[i]<Min.RawComponents[i] then begin
   Quadrant[i]:=LEFT;
   CandidatePlane.RawComponents[i]:=Min.RawComponents[i];
   Inside:=false;
  end else if Origin.RawComponents[i]>Max.RawComponents[i] then begin
   Quadrant[i]:=RIGHT;
   CandidatePlane.RawComponents[i]:=Max.RawComponents[i];
   Inside:=false;
  end else begin
   Quadrant[i]:=MIDDLE;
  end;
 end;
 if Inside then begin
  HitPoint:=Origin;
  result:=true;
 end else begin
  for i:=0 to 2 do begin
   if (Quadrant[i]<>MIDDLE) and (Direction.RawComponents[i]<>0.0) then begin
    MaxT.RawComponents[i]:=(CandidatePlane.RawComponents[i]-Origin.RawComponents[i])/Direction.RawComponents[i];
   end else begin
    MaxT.RawComponents[i]:=-1.0;
   end;
  end;
  WhichPlane:=0;
  for i:=1 to 2 do begin
   if MaxT.RawComponents[WhichPlane]<MaxT.RawComponents[i] then begin
    WhichPlane:=i;
   end;
  end;
  if MaxT.RawComponents[WhichPlane]<0.0 then begin
   result:=false;
  end else begin
   for i:=0 to 2 do begin
    if WhichPlane<>i then begin
     HitPoint.RawComponents[i]:=Origin.RawComponents[i]+(MaxT.RawComponents[WhichPlane]*Direction.RawComponents[i]);
     if (HitPoint.RawComponents[i]<Min.RawComponents[i]) or (HitPoint.RawComponents[i]>Min.RawComponents[i]) then begin
      result:=false;
      exit;
     end;
    end else begin
     HitPoint.RawComponents[i]:=CandidatePlane.RawComponents[i];
    end;
   end;
   result:=true;
  end;
 end;
end;

function TAABB.RayIntersection(const Origin,Direction:TVector3;var Time:TScalar):boolean;
var InvDirection,a,b,AABBMin,AABBMax:TVector3;
    TimeMin,TimeMax:TScalar;
begin
 if Direction.x<>0.0 then begin
  InvDirection.x:=1.0/Direction.x;
 end else begin
  InvDirection.x:=0.0;
 end;
 if Direction.y<>0.0 then begin
  InvDirection.y:=1.0/Direction.y;
 end else begin
  InvDirection.y:=0.0;
 end;
 if Direction.z<>0.0 then begin
  InvDirection.z:=1.0/Direction.z;
 end else begin
  InvDirection.z:=0.0;
 end;
 a.x:=(Min.x-Origin.x)*InvDirection.x;
 a.y:=(Min.y-Origin.y)*InvDirection.y;
 a.z:=(Min.z-Origin.z)*InvDirection.z;
 b.x:=(Max.x-Origin.x)*InvDirection.x;
 b.y:=(Max.y-Origin.y)*InvDirection.y;
 b.z:=(Max.z-Origin.z)*InvDirection.z;
 if a.x<b.x then begin
  AABBMin.x:=a.x;
  AABBMax.x:=b.x;
 end else begin
  AABBMin.x:=b.x;
  AABBMax.x:=a.x;
 end;
 if a.y<b.y then begin
  AABBMin.y:=a.y;
  AABBMax.y:=b.y;
 end else begin
  AABBMin.y:=b.y;
  AABBMax.y:=a.y;
 end;
 if a.z<b.z then begin
  AABBMin.z:=a.z;
  AABBMax.z:=b.z;
 end else begin
  AABBMin.z:=b.z;
  AABBMax.z:=a.z;
 end;
 if AABBMin.x<AABBMin.y then begin
  if AABBMin.x<AABBMin.z then begin
   TimeMin:=AABBMin.x;
  end else begin
   TimeMin:=AABBMin.z;
  end;
 end else begin
  if AABBMin.y<AABBMin.z then begin
   TimeMin:=AABBMin.y;
  end else begin
   TimeMin:=AABBMin.z;
  end;
 end;
 if AABBMax.x>AABBMax.y then begin
  if AABBMax.x>AABBMax.z then begin
   TimeMax:=AABBMax.x;
  end else begin
   TimeMax:=AABBMax.z;
  end;
 end else begin
  if AABBMax.y>AABBMax.z then begin
   TimeMax:=AABBMax.y;
  end else begin
   TimeMax:=AABBMax.z;
  end;
 end;
 if (TimeMax<0) or (TimeMin>TimeMax) then begin
  Time:=TimeMax;
  result:=false;
 end else begin
  Time:=TimeMin;
  result:=true;
 end;
end;

function TAABB.LineIntersection(const StartPoint,EndPoint:TVector3):boolean;
var Direction,InvDirection,a,b:TVector3;
    Len,TimeMin,TimeMax:TScalar;
begin
 if Contains(StartPoint) or Contains(EndPoint) then begin
  result:=true;
 end else begin
  Direction:=EndPoint-StartPoint;
  Len:=Direction.Length;
  if Len<>0.0 then begin
   Direction:=Direction/Len;
  end;
  if Direction.x<>0.0 then begin
   InvDirection.x:=1.0/Direction.x;
  end else begin
   InvDirection.x:=Infinity;
  end;
  if Direction.y<>0.0 then begin
   InvDirection.y:=1.0/Direction.y;
  end else begin
   InvDirection.y:=Infinity;
  end;
  if Direction.z<>0.0 then begin
   InvDirection.z:=1.0/Direction.z;
  end else begin
   InvDirection.z:=Infinity;
  end;
  a.x:=((Min.x-EPSILON)-StartPoint.x)*InvDirection.x;
  a.y:=((Min.y-EPSILON)-StartPoint.y)*InvDirection.y;
  a.z:=((Min.z-EPSILON)-StartPoint.z)*InvDirection.z;
  b.x:=((Max.x+EPSILON)-StartPoint.x)*InvDirection.x;
  b.y:=((Max.y+EPSILON)-StartPoint.y)*InvDirection.y;
  b.z:=((Max.z+EPSILON)-StartPoint.z)*InvDirection.z;
  TimeMin:=Math.Max(Math.Max(Math.Min(a.x,a.y),Math.Min(a.z,b.x)),Math.Min(b.y,b.z));
  TimeMax:=Math.Min(Math.Min(Math.Max(a.x,a.y),Math.Max(a.z,b.x)),Math.Max(b.y,b.z));
  result:=((TimeMin<=TimeMax) and (TimeMax>=0.0)) and (TimeMin<=(Len+EPSILON));
 end;
end;

function TAABB.TriangleIntersection(const Triangle:TTriangle):boolean;
 function FindMin(const a,b,c:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
 begin
  result:=a;
  if result>b then begin
   result:=b;
  end;
  if result>c then begin
   result:=c;
  end;
 end;
 function FindMax(const a,b,c:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
 begin
  result:=a;
  if result<b then begin
   result:=b;
  end;
  if result<c then begin
   result:=c;
  end;
 end;
 function PlaneBoxOverlap(const Normal:TVector3;d:TFloat;MaxBox:TVector3):boolean; {$ifdef CAN_INLINE}inline;{$endif}
 var vmin,vmax:TVector3;
 begin
  if Normal.x>0 then begin
   vmin.x:=-MaxBox.x;
   vmax.x:=MaxBox.x;
  end else begin
   vmin.x:=MaxBox.x;
   vmax.x:=-MaxBox.x;
  end;
  if Normal.y>0 then begin
   vmin.y:=-MaxBox.y;
   vmax.y:=MaxBox.y;
  end else begin
   vmin.y:=MaxBox.y;
   vmax.y:=-MaxBox.y;
  end;
  if Normal.z>0 then begin
   vmin.z:=-MaxBox.z;
   vmax.z:=MaxBox.z;
  end else begin
   vmin.z:=MaxBox.z;
   vmax.z:=-MaxBox.z;
  end;
  if (Normal.Dot(vmin)+d)>0 then begin
   result:=false;
  end else if (Normal.Dot(vmax)+d)>=0 then begin
   result:=true;
  end else begin
   result:=false;
  end;
 end;
var BoxCenter,BoxHalfSize,Normal,v0,v1,v2,e0,e1,e2:TVector3;
    fex,fey,fez:TFloat;
 function AxisTestX01(a,b,fa,fb:TFloat):boolean;
 var p0,p2,pmin,pmax,Radius:TFloat;
 begin
  p0:=(a*v0.y)-(b*v0.z);
  p2:=(a*v2.y)-(b*v2.z);
  if p0<p2 then begin
   pmin:=p0;
   pmax:=p2;
  end else begin
   pmin:=p2;
   pmax:=p0;
  end;
  Radius:=(fa*BoxHalfSize.y)+(fb*BoxHalfSize.z);
  result:=not ((pmin>Radius) or (pmax<(-radius)));
 end;
 function AxisTestX2(a,b,fa,fb:TFloat):boolean;
 var p0,p1,pmin,pmax,Radius:TFloat;
 begin
  p0:=(a*v0.y)-(b*v0.z);
  p1:=(a*v1.y)-(b*v1.z);
  if p0<p1 then begin
   pmin:=p0;
   pmax:=p1;
  end else begin
   pmin:=p1;
   pmax:=p0;
  end;
  Radius:=(fa*BoxHalfSize.y)+(fb*BoxHalfSize.z);
  result:=not ((pmin>Radius) or (pmax<(-radius)));
 end;
 function AxisTestY02(a,b,fa,fb:TFloat):boolean;
 var p0,p2,pmin,pmax,Radius:TFloat;
 begin
  p0:=(-(a*v0.x))+(b*v0.z);
  p2:=(-(a*v2.x))+(b*v2.z);
  if p0<p2 then begin
   pmin:=p0;
   pmax:=p2;
  end else begin
   pmin:=p2;
   pmax:=p0;
  end;
  Radius:=(fa*BoxHalfSize.x)+(fb*BoxHalfSize.z);
  result:=not ((pmin>Radius) or (pmax<(-radius)));
 end;
 function AxisTestY1(a,b,fa,fb:TFloat):boolean;
 var p0,p1,pmin,pmax,Radius:TFloat;
 begin
  p0:=(-(a*v0.x))+(b*v0.z);
  p1:=(-(a*v1.x))+(b*v1.z);
  if p0<p1 then begin
   pmin:=p0;
   pmax:=p1;
  end else begin
   pmin:=p1;
   pmax:=p0;
  end;
  Radius:=(fa*BoxHalfSize.x)+(fb*BoxHalfSize.z);
  result:=not ((pmin>Radius) or (pmax<(-radius)));
 end;
 function AxisTestZ12(a,b,fa,fb:TFloat):boolean;
 var p1,p2,pmin,pmax,Radius:TFloat;
 begin
  p1:=(a*v1.x)-(b*v1.y);
  p2:=(a*v2.x)-(b*v2.y);
  if p2<p1 then begin
   pmin:=p2;
   pmax:=p1;
  end else begin
   pmin:=p1;
   pmax:=p2;
  end;
  Radius:=(fa*BoxHalfSize.x)+(fb*BoxHalfSize.y);
  result:=not ((pmin>Radius) or (pmax<(-radius)));
 end;
 function AxisTestZ0(a,b,fa,fb:TFloat):boolean;
 var p0,p1,pmin,pmax,Radius:TFloat;
 begin
  p0:=(a*v0.x)-(b*v0.y);
  p1:=(a*v1.x)-(b*v1.y);
  if p0<p1 then begin
   pmin:=p0;
   pmax:=p1;
  end else begin
   pmin:=p1;
   pmax:=p0;
  end;
  Radius:=(fa*BoxHalfSize.x)+(fb*BoxHalfSize.y);
  result:=not ((pmin>Radius) or (pmax<(-radius)));
 end;
 procedure FindMinMax(const a,b,c:TFloat;var omin,omax:TFloat);
 begin
  omin:=a;
  if omin>b then begin
   omin:=b;
  end;
  if omin>c then begin
   omin:=c;
  end;
  omax:=a;
  if omax<b then begin
   omax:=b;
  end;
  if omax<c then begin
   omax:=c;
  end;
 end;
begin
 BoxCenter:=(Min+Max)*0.5;
 BoxHalfSize:=(Max-Min)*0.5;
 v0:=Triangle.Points[0]-BoxCenter;
 v1:=Triangle.Points[1]-BoxCenter;
 v2:=Triangle.Points[2]-BoxCenter;
 e0:=v1-v0;
 e1:=v2-v1;
 e2:=v0-v2;
 fex:=abs(e0.x);
 fey:=abs(e0.y);
 fez:=abs(e0.z);
 if (not AxisTestX01(e0.z,e0.y,fez,fey)) or (not AxisTestY02(e0.z,e0.x,fez,fex)) or (not AxisTestZ12(e0.y,e0.x,fey,fex)) then begin
  result:=false;
  exit;
 end;
 fex:=abs(e1.x);
 fey:=abs(e1.y);
 fez:=abs(e1.z);
 if (not AxisTestX01(e1.z,e1.y,fez,fey)) or (not AxisTestY02(e1.z,e1.x,fez,fex)) or (not AxisTestZ0(e1.y,e1.x,fey,fex)) then begin
  result:=false;
  exit;
 end;
 fex:=abs(e2.x);
 fey:=abs(e2.y);
 fez:=abs(e2.z);
 if (not AxisTestX2(e2.z,e2.y,fez,fey)) or (not AxisTestY1(e2.z,e2.x,fez,fex)) or (not AxisTestZ12(e2.y,e2.x,fey,fex)) then begin
  result:=false;
  exit;
 end;
 if ((FindMin(v0.x,v1.x,v2.x)>BoxHalfSize.x) or (FindMax(v0.x,v1.x,v2.x)<(-BoxHalfSize.x))) or
    ((FindMin(v0.y,v1.y,v2.y)>BoxHalfSize.y) or (FindMax(v0.y,v1.y,v2.y)<(-BoxHalfSize.y))) or
    ((FindMin(v0.z,v1.z,v2.z)>BoxHalfSize.z) or (FindMax(v0.z,v1.z,v2.z)<(-BoxHalfSize.z))) then begin
  result:=false;
  exit;
 end;
 Normal:=e0.Cross(e1);
 result:=PlaneBoxOverlap(Normal,-Normal.Dot(v0),BoxHalfSize);
end;

function TAABB.Transform(const Transform:TMatrix4x4):TAABB;
var i,j:TInt32;
    a,b:TScalar;
begin
 result.Min.x:=Transform[3,0];
 result.Min.y:=Transform[3,1];
 result.Min.z:=Transform[3,2];
 result.Max:=result.Min;
 for i:=0 to 2 do begin
  for j:=0 to 2 do begin
   a:=Transform[j,i]*Min.RawComponents[j];
   b:=Transform[j,i]*Max.RawComponents[j];
   if a<b then begin
    result.Min.RawComponents[i]:=result.Min.RawComponents[i]+a;
    result.Max.RawComponents[i]:=result.Max.RawComponents[i]+b;
   end else begin
    result.Min.RawComponents[i]:=result.Min.RawComponents[i]+b;
    result.Max.RawComponents[i]:=result.Max.RawComponents[i]+a;
   end;
  end;
 end;
end;

function TAABB.MatrixMul(const Transform:TMatrix4x4):TAABB;
var Rotation:TMatrix4x4;
    v:array[0..7] of TVector3;
    Center,NewCenter,MinVector,MaxVector:TVector3;
    i:TInt32;
begin
 Rotation:=TMatrix4x4.CreateRotation(Transform);

 Center:=(Min+Max)*0.5;

 MinVector:=Min-Center;
 MaxVector:=Max-Center;

 NewCenter:=Center+(Transform*Vector3Origin);

 v[0]:=Rotation*TVector3.Create(MinVector.x,MinVector.y,MinVector.z);
 v[1]:=Rotation*TVector3.Create(MaxVector.x,MinVector.y,MinVector.z);
 v[2]:=Rotation*TVector3.Create(MaxVector.x,MaxVector.y,MinVector.z);
 v[3]:=Rotation*TVector3.Create(MaxVector.x,MaxVector.y,MaxVector.z);
 v[4]:=Rotation*TVector3.Create(MinVector.x,MaxVector.y,MaxVector.z);
 v[5]:=Rotation*TVector3.Create(MinVector.x,MinVector.y,MaxVector.z);
 v[6]:=Rotation*TVector3.Create(MaxVector.x,MinVector.y,MaxVector.z);
 v[7]:=Rotation*TVector3.Create(MinVector.x,MaxVector.y,MinVector.z);

 result.Min:=v[0];
 result.Max:=v[0];
 for i:=0 to 7 do begin
  if result.Min.x>v[i].x then begin
   result.Min.x:=v[i].x;
  end;
  if result.Min.y>v[i].y then begin
   result.Min.y:=v[i].y;
  end;
  if result.Min.z>v[i].z then begin
   result.Min.z:=v[i].z;
  end;
  if result.Max.x<v[i].x then begin
   result.Max.x:=v[i].x;
  end;
  if result.Max.y<v[i].y then begin
   result.Max.y:=v[i].y;
  end;
  if result.Max.z<v[i].z then begin
   result.Max.z:=v[i].z;
  end;
 end;
 result.Min:=result.Min+NewCenter;
 result.Max:=result.Max+NewCenter;
end;

function TAABB.ScissorRect(var Scissor:TClipRect;const mvp:TMatrix4x4;const vp:TClipRect;zcull:boolean):boolean;
var p:TVector4;
    i,x,y,z_far,z_near:TInt32;
begin
 z_near:=0;
 z_far:=0;
 for i:=0 to 7 do begin

  // Get bound edge point
  p.x:=MinMax[i and 1].x;
  p.y:=MinMax[(i shr 1) and 1].y;
  p.z:=MinMax[(i shr 2) and 1].z;
  p.w:=1.0;

  // Project
  p:=mvp*p;
  p.x:=p.x/p.w;
  p.y:=p.y/p.w;
  p.z:=p.z/p.w;

  // Convert to screen space
  p.x:=vp[0]+(vp[2]*((p.x+1.0)*0.5));
  p.y:=vp[1]+(vp[3]*((p.y+1.0)*0.5));
  p.z:=(p.z+1.0)*0.5;
  if zcull then begin
   if p.z<-EPSILON then begin
    inc(z_far);
   end else if p.z>(1.0+EPSILON) then begin
    inc(z_near);
   end;
  end;

  // Round to integer values
  x:=round(p.x);
  y:=round(p.y);

  // Clip
  if x<vp[0] then begin
   x:=vp[0];
  end;
  if x>(vp[0]+vp[2]) then begin
   x:=vp[0]+vp[2];
  end;
  if y<vp[1] then begin
   y:=vp[1];
  end;
  if y>(vp[1]+vp[3]) then begin
   y:=vp[1]+vp[3];
  end;

  // Extend
  if i=0 then begin
   Scissor[0]:=x;
   Scissor[1]:=y;
   Scissor[2]:=x;
   Scissor[3]:=y;
  end else begin
   if x<Scissor[0] then begin
    Scissor[0]:=x;
   end;
   if y<Scissor[1] then begin
    Scissor[1]:=y;
   end;
   if x>Scissor[2] then begin
    Scissor[2]:=x;
   end;
   if y>Scissor[3] then begin
    Scissor[3]:=y;
   end;
  end;

 end;
 if (z_far=8) or (z_near=8) then begin
  result:=false;
 end else if (z_near>0) and (z_near<8) then begin
  result:=true;
  Scissor[0]:=vp[0];
  Scissor[1]:=vp[1];
  Scissor[2]:=vp[0]+vp[2];
  Scissor[3]:=vp[1]+vp[3];
 end else begin
  result:=true;
 end;
end;

function TAABB.ScissorRect(var Scissor:TFloatClipRect;const mvp:TMatrix4x4;const vp:TFloatClipRect;zcull:boolean):boolean;
var p:TVector4;
    i,z_far,z_near:TInt32;
begin
 z_near:=0;
 z_far:=0;
 for i:=0 to 7 do begin

  // Get bound edge point
  p.x:=MinMax[i and 1].x;
  p.y:=MinMax[(i shr 1) and 1].y;
  p.z:=MinMax[(i shr 2) and 1].z;
  p.w:=1.0;

  // Project
  p:=mvp*p;
  p.x:=p.x/p.w;
  p.y:=p.y/p.w;
  p.z:=p.z/p.w;

  // Convert to screen space
  p.x:=vp[0]+(vp[2]*((p.x+1.0)*0.5));
  p.y:=vp[1]+(vp[3]*((p.y+1.0)*0.5));
  p.z:=(p.z+1.0)*0.5;
  if zcull then begin
   if p.z<-EPSILON then begin
    inc(z_far);
   end else if p.z>(1.0+EPSILON) then begin
    inc(z_near);
   end;
  end;

  // Clip
  if p.x<vp[0] then begin
   p.x:=vp[0];
  end;
  if p.x>(vp[0]+vp[2]) then begin
   p.x:=vp[0]+vp[2];
  end;
  if p.y<vp[1] then begin
   p.y:=vp[1];
  end;
  if p.y>(vp[1]+vp[3]) then begin
   p.y:=vp[1]+vp[3];
  end;

  // Extend
  if i=0 then begin
   Scissor[0]:=p.x;
   Scissor[1]:=p.y;
   Scissor[2]:=p.x;
   Scissor[3]:=p.y;
  end else begin
   if p.x<Scissor[0] then begin
    Scissor[0]:=p.x;
   end;
   if p.y<Scissor[1] then begin
    Scissor[1]:=p.y;
   end;
   if p.x>Scissor[2] then begin
    Scissor[2]:=p.x;
   end;
   if p.y>Scissor[3] then begin
    Scissor[3]:=p.y;
   end;
  end;

 end;
 if (z_far=8) or (z_near=8) then begin
  result:=false;
 end else if (z_near>0) and (z_near<8) then begin
  result:=true;
  Scissor[0]:=vp[0];
  Scissor[1]:=vp[1];
  Scissor[2]:=vp[0]+vp[2];
  Scissor[3]:=vp[1]+vp[3];
 end else begin
  result:=true;
 end;
end;

function TAABB.MovingTest(const aAABBTo,bAABBFrom,bAABBTo:TAABB;var t:TScalar):boolean;
var Axis,AxisSamples,Samples,Sample,FirstSample:TInt32;
    aAABB,bAABB:TAABB;
    f{,MinRadius},Size,Distance,BestDistance:TScalar;
    HasDistance:boolean;
begin
 if Intersect(bAABBFrom) then begin
  t:=0.0;
  result:=true;
 end else begin
  result:=false;
  if Combine(aAABBTo).Intersect(bAABBFrom.Combine(bAABBTo)) then begin
   FirstSample:=0;
   Samples:=1;
   for Axis:=0 to 2 do begin
    if Min.RawComponents[Axis]>aAABBTo.Max.RawComponents[Axis] then begin
     Distance:=Min.RawComponents[Axis]-aAABBTo.Max.RawComponents[Axis];
    end else if aAABBTo.Min.RawComponents[Axis]>Max.RawComponents[Axis] then begin
     Distance:=aAABBTo.Min.RawComponents[Axis]-Max.RawComponents[Axis];
    end else begin
     Distance:=0;
    end;
    Size:=Math.Min(abs(Max.RawComponents[Axis]-Min.RawComponents[Axis]),abs(aAABBTo.Max.RawComponents[Axis]-aAABBTo.Min.RawComponents[Axis]));
    if Size>0.0 then begin
     AxisSamples:=round((Distance+Size)/Size);
     if Samples<AxisSamples then begin
      Samples:=AxisSamples;
     end;
    end;
    if bAABBFrom.Min.RawComponents[Axis]>bAABBTo.Max.RawComponents[Axis] then begin
     Distance:=bAABBFrom.Min.RawComponents[Axis]-bAABBTo.Max.RawComponents[Axis];
    end else if bAABBTo.Min.RawComponents[Axis]>bAABBFrom.Max.RawComponents[Axis] then begin
     Distance:=bAABBTo.Min.RawComponents[Axis]-bAABBFrom.Max.RawComponents[Axis];
    end else begin
     Distance:=0;
    end;
    Size:=Math.Min(abs(bAABBFrom.Max.RawComponents[Axis]-bAABBFrom.Min.RawComponents[Axis]),abs(bAABBTo.Max.RawComponents[Axis]-bAABBTo.Min.RawComponents[Axis]));
    if Size>0.0 then begin
     AxisSamples:=round((Distance+Size)/Size);
     if Samples<AxisSamples then begin
      Samples:=AxisSamples;
     end;
    end;
   end;
   BestDistance:=1e+18;
   HasDistance:=false;
   for Sample:=FirstSample to Samples do begin
    f:=Sample/Samples;
    aAABB.Min:=Min.Lerp(aAABBTo.Min,f);
    aAABB.Max:=Max.Lerp(aAABBTo.Max,f);
    bAABB.Min:=bAABBFrom.Min.Lerp(bAABBTo.Min,f);
    bAABB.Max:=bAABBFrom.Max.Lerp(bAABBTo.Max,f);
    if aAABB.Intersect(bAABB) then begin
     t:=f;
     result:=true;
     break;
    end else begin
     Distance:=aAABB.DistanceTo(bAABB);
     if (not HasDistance) and (Distance<BestDistance) then begin
      BestDistance:=Distance;
      HasDistance:=true;
     end else begin
      break;
     end;
    end;
   end;
  end;
 end;
end;

function TAABB.SweepTest(const bAABB:TAABB;const aV,bV:TVector3;var FirstTime,LastTime:TScalar):boolean;
var Axis:TInt32;
    v,tMin,tMax:TVector3;
begin
 if Intersect(bAABB) then begin
  FirstTime:=0.0;
  LastTime:=0.0;
  result:=true;
 end else begin
  v:=bV-aV;
  for Axis:=0 to 2 do begin
   if v.RawComponents[Axis]<0.0 then begin
    tMin.RawComponents[Axis]:=(Max.RawComponents[Axis]-bAABB.Min.RawComponents[Axis])/v.RawComponents[Axis];
    tMax.RawComponents[Axis]:=(Min.RawComponents[Axis]-bAABB.Max.RawComponents[Axis])/v.RawComponents[Axis];
   end else if v.RawComponents[Axis]>0.0 then begin
    tMin.RawComponents[Axis]:=(Min.RawComponents[Axis]-bAABB.Max.RawComponents[Axis])/v.RawComponents[Axis];
    tMax.RawComponents[Axis]:=(Max.RawComponents[Axis]-bAABB.Min.RawComponents[Axis])/v.RawComponents[Axis];
   end else if (Max.RawComponents[Axis]>=bAABB.Min.RawComponents[Axis]) and (Min.RawComponents[Axis]<=bAABB.Max.RawComponents[Axis]) then begin
    tMin.RawComponents[Axis]:=0.0;
    tMax.RawComponents[Axis]:=1.0;
   end else begin
    result:=false;
    exit;
   end;
  end;
  FirstTime:=Math.Max(Math.Max(tMin.x,tMin.y),tMin.z);
  LastTime:=Math.Min(Math.Min(tMax.x,tMax.y),tMax.z);
  result:=(LastTime>=0.0) and (FirstTime<=1.0) and (FirstTime<=LastTime);
 end;
end;

constructor TSphere.Create(const pCenter:TVector3;const pRadius:TScalar);
begin
 Center:=pCenter;
 Radius:=pRadius;
end;

constructor TSphere.CreateFromAABB(const pAABB:TAABB);
begin
 Center:=(pAABB.Min+pAABB.Max)*0.5;
 Radius:=pAABB.Min.DistanceTo(pAABB.Max)*0.5;
end;

constructor TSphere.CreateFromFrustum(const zNear,zFar,FOV,AspectRatio:TScalar;const Position,Direction:TVector3);
var ViewLen,Width,Height:TScalar;
begin
 ViewLen:=zFar-zNear;
 Height:=ViewLen*tan((FOV*0.5)*DEG2RAD);
 Width:=Height*AspectRatio;
 Radius:=TVector3.Create(Width,Height,ViewLen).DistanceTo(TVector3.Create(0.0,0.0,zNear+(ViewLen*0.5)));
 Center:=Position+(Direction*((ViewLen*0.5)+zNear));
end;

function TSphere.ToAABB(const pScale:TScalar=1.0):TAABB;
begin
 result.Min.x:=Center.x-(Radius*pScale);
 result.Min.y:=Center.y-(Radius*pScale);
 result.Min.z:=Center.z-(Radius*pScale);
 result.Max.x:=Center.x+(Radius*pScale);
 result.Max.y:=Center.y+(Radius*pScale);
 result.Max.z:=Center.z+(Radius*pScale);
end;

function TSphere.Cull(const p:array of TPlane):boolean;
var i:TInt32;
begin
 result:=true;
 for i:=0 to length(p)-1 do begin
  if p[i].DistanceTo(Center)<-Radius then begin
   result:=false;
   exit;
  end;
 end;
end;

function TSphere.Contains(const b:TSphere):boolean;
begin
 result:=((Radius+EPSILON)>=(b.Radius-EPSILON)) and ((Center-b.Center).Length<=((Radius+EPSILON)-(b.Radius-EPSILON)));
end;

function TSphere.Contains(const v:TVector3):boolean;
begin
 result:=Center.DistanceTo(v)<(Radius+EPSILON);
end;

function TSphere.DistanceTo(const b:TSphere):TScalar;
begin
 result:=Max((Center-b.Center).Length-(Radius+b.Radius),0.0);
end;

function TSphere.DistanceTo(const b:TVector3):TScalar;
begin
 result:=Max(Center.DistanceTo(b)-Radius,0.0);
end;

function TSphere.Intersect(const b:TSphere):boolean;
begin
 result:=(Center-b.Center).Length<=(Radius+b.Radius+(EPSILON*2.0));
end;

function TSphere.Intersect(const b:TAABB):boolean;
var c:TVector3;
begin
 c.x:=Min(Max(Center.x,b.Min.x),b.Max.x);
 c.y:=Min(Max(Center.y,b.Min.y),b.Max.y);
 c.z:=Min(Max(Center.z,b.Min.z),b.Max.z);
 result:=(c-Center).SquaredLength<sqr(Radius);
end;

function TSphere.RayIntersection(const Origin,Direction:TVector3):boolean;
var m:TVector3;
    p,d:TScalar;
begin
 m:=Origin-Center;
 p:=-m.Dot(Direction);
 d:=(sqr(p)-m.SquaredLength)+sqr(Radius);
 result:=(d>0.0) and ((p+sqrt(d))>0.0);
end;

function TSphere.Extends(const WithSphere:TSphere):TSphere;
var x0,y0,z0,r0,x1,y1,z1,r1,xn,yn,zn,dn,t:TScalar;
begin
 x0:=Center.x;
 y0:=Center.y;
 z0:=Center.z;
 r0:=Radius;

 x1:=WithSphere.Center.x;
 y1:=WithSphere.Center.y;
 z1:=WithSphere.Center.z;
 r1:=WithSphere.Radius;

 xn:=x1-x0;
 yn:=y1-y0;
 zn:=z1-z0;
 dn:=sqrt(sqr(xn)+sqr(yn)+sqr(zn));
 if abs(dn)<EPSILON then begin
  result:=self;
  exit;
 end;

 if (dn+r1)<r0 then begin
  result:=self;
  exit;
 end;

 result.Radius:=(dn+r0+r1)*0.5;
 t:=(result.Radius-r0)/dn;
 result.Center.x:=x0+(xn*t);
 result.Center.y:=y0+(xn*t);
 result.Center.z:=z0+(xn*t);
end;

function TSphere.Transform(const Transform:TMatrix4x4):TSphere;
begin
 result.Center:=Transform*Center;
 result.Radius:=result.Center.DistanceTo(Transform*TVector3.Create(Center.x,Center.y+Radius,Center.z));
end;

function TSphere.TriangleIntersection(const Triangle:TTriangle;out Position,Normal:TVector3;out Depth:TScalar):boolean;
var SegmentTriangle:TSegmentTriangle;
    Dist,d2,s,t:TScalar;
begin
 result:=false;
 if ((Triangle.Normal.Dot(Center)-Triangle.Normal.Dot(Triangle.Points[0]))-Radius)<EPSILON then begin
  SegmentTriangle.Origin:=Triangle.Points[0];
  SegmentTriangle.Edge0:=Triangle.Points[1]-Triangle.Points[0];
  SegmentTriangle.Edge1:=Triangle.Points[2]-Triangle.Points[0];
  SegmentTriangle.Edge2:=SegmentTriangle.Edge1-SegmentTriangle.Edge0;
  d2:=SegmentTriangle.SquaredPointTriangleDistance(Center,s,t);
  if d2<sqr(Radius) then begin
   Dist:=sqrt(d2);
   Depth:=Radius-Dist;
   if Dist>EPSILON then begin
    Normal:=((Center-(SegmentTriangle.Origin+((SegmentTriangle.Edge0*s)+(SegmentTriangle.Edge1*t))))).Normalize;
   end else begin
    Normal:=Triangle.Normal;
   end;
   Position:=Center-(Normal*Radius);
   result:=true;
  end;
 end;
end;

function TSphere.TriangleIntersection(const SegmentTriangle:TSegmentTriangle;const TriangleNormal:TVector3;out Position,Normal:TVector3;out Depth:TScalar):boolean;
var Dist,d2,s,t:TScalar;
begin
 result:=false;
 d2:=SegmentTriangle.SquaredPointTriangleDistance(Center,s,t);
 if d2<sqr(Radius) then begin
  Dist:=sqrt(d2);
  Depth:=Radius-Dist;
  if Dist>EPSILON then begin
   Normal:=((Center-(SegmentTriangle.Origin+((SegmentTriangle.Edge0*s)+(SegmentTriangle.Edge1*t))))).Normalize;
  end else begin
   Normal:=TriangleNormal;
  end;
  Position:=Center-(Normal*Radius);
  result:=true;
 end;
end;

function TSphere.SweptIntersection(const SphereB:TSphere;const VelocityA,VelocityB:TVector3;out TimeFirst,TimeLast:TScalar):boolean;
var ab,vab:TVector3;
    rab,a,b,c:TScalar;
begin
 result:=false;
 ab:=SphereB.Center-Center;
 vab:=VelocityB-VelocityA;
 rab:=Radius+SphereB.Radius;
 c:=ab.SquaredLength-sqr(rab);
 if c<=0.0 then begin
  TimeFirst:=0.0;
  TimeLast:=0.0;
  result:=true;
 end else begin
  a:=vab.SquaredLength;
  b:=2.0*vab.Dot(ab);
  if SolveQuadraticRoots(a,b,c,TimeFirst,TimeLast) then begin
   if TimeFirst>TimeLast then begin
    a:=TimeFirst;
    TimeFirst:=TimeLast;
    TimeLast:=a;
   end;
   result:=(TimeLast>=0.0) and (TimeFirst<=1.0);
  end;
 end;
end;

constructor TSphereCoords.CreateFromCartesianVector(const v:TVector3);
begin
 Radius:=v.Length;
 Theta:=ArcCos(v.z/Radius);
 Phi:=Sign(v.y)*ArcCos(v.x/sqrt(v.x*v.x+v.y*v.y));
end;

constructor TSphereCoords.CreateFromCartesianVector(const v:TVector4);
begin
 Radius:=v.Length;
 Theta:=ArcCos(v.z/Radius);
 Phi:=Sign(v.y)*ArcCos(v.x/sqrt(v.x*v.x+v.y*v.y));
end;

function TSphereCoords.ToCartesianVector:TVector3;
begin
 result.x:=Radius*sin(Theta)*cos(Phi);
 result.y:=Radius*sin(Theta)*sin(Phi);
 result.z:=Radius*cos(Theta);
end;

function Cross(const a,b:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=a.Cross(b);
end;

function Cross(const a,b:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=a.Cross(b);
end;

function Cross(const a,b:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=a.Cross(b);
end;

function Dot(const a,b:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=a*b;
end;

function Dot(const a,b:TVector2):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=a.Dot(b);
end;

function Dot(const a,b:TVector3):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=a.Dot(b);
end;

function Dot(const a,b:TVector4):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=a.Dot(b);
end;

function Distance(const a,b:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=abs(a-b);
end;

function Distance(const a,b:TVector2):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=a.DistanceTo(b);
end;

function Distance(const a,b:TVector3):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=a.DistanceTo(b);
end;

function Distance(const a,b:TVector4):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=a.DistanceTo(b);
end;

function Len(const a:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=abs(a);
end;

function Len(const a:TVector2):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=a.Length;
end;

function Len(const a:TVector3):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=a.Length;
end;

function Len(const a:TVector4):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=a.Length;
end;

function Normalize(const a:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=a;
end;

function Normalize(const a:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=a.Normalize;
end;

function Normalize(const a:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=a.Normalize;
end;

function Normalize(const a:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=a.Normalize;
end;

function Minimum(const a,b:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=Min(a,b);
end;

function Minimum(const a,b:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=Min(a.x,b.x);
 result.y:=Min(a.y,b.y);
end;

function Minimum(const a,b:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=Min(a.x,b.x);
 result.y:=Min(a.y,b.y);
 result.z:=Min(a.z,b.z);
end;

function Minimum(const a,b:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=Min(a.x,b.x);
 result.y:=Min(a.y,b.y);
 result.z:=Min(a.z,b.z);
 result.w:=Min(a.w,b.w);
end;

function Maximum(const a,b:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=Max(a,b);
end;

function Maximum(const a,b:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=Max(a.x,b.x);
 result.y:=Max(a.y,b.y);
end;

function Maximum(const a,b:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=Max(a.x,b.x);
 result.y:=Max(a.y,b.y);
 result.z:=Max(a.z,b.z);
end;

function Maximum(const a,b:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=Max(a.x,b.x);
 result.y:=Max(a.y,b.y);
 result.z:=Max(a.z,b.z);
 result.w:=Max(a.w,b.w);
end;

function FaceForward(const N,I:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 if (N*I)>0.0 then begin
  result:=-N;
 end else begin
  result:=N;
 end;
end;

function FaceForward(const N,I:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 if N.Dot(I)>0.0 then begin
  result:=-N;
 end else begin
  result:=N;
 end;
end;

function FaceForward(const N,I:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 if N.Dot(I)>0.0 then begin
  result:=-N;
 end else begin
  result:=N;
 end;
end;

function FaceForward(const N,I:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 if N.Dot(I)>0.0 then begin
  result:=-N;
 end else begin
  result:=N;
 end;
end;

function FaceForward(const N,I,Nref:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 if (I*Nref)>0.0 then begin
  result:=-N;
 end else begin
  result:=N;
 end;
end;

function FaceForward(const N,I,Nref:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 if I.Dot(Nref)>0.0 then begin
  result:=-N;
 end else begin
  result:=N;
 end;
end;

function FaceForward(const N,I,Nref:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 if I.Dot(Nref)>0.0 then begin
  result:=-N;
 end else begin
  result:=N;
 end;
end;

function FaceForward(const N,I,Nref:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 if I.Dot(Nref)>0.0 then begin
  result:=-N;
 end else begin
  result:=N;
 end;
end;

function Reflect(const I,N:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=I-((2.0*(N*I))*N);
end;

function Reflect(const I,N:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=I-((2.0*N.Dot(I))*N);
end;

function Reflect(const I,N:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=I-((2.0*N.Dot(I))*N);
end;

function Reflect(const I,N:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=I-((2.0*N.Dot(I))*N);
end;

function Refract(const I,N:TScalar;const Eta:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
var NdotI,k:TScalar;
begin
 NdotI:=N*I;
 k:=1.0-(sqr(Eta)*(1.0-sqr(NdotI)));
 if k>=0.0 then begin
  result:=((Eta*I)-((Eta*NdotI)+sqrt(k))*N);
 end else begin
  result:=0.0;
 end;
end;

function Refract(const I,N:TVector2;const Eta:TScalar):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
var NdotI,k:TScalar;
begin
 NdotI:=N.Dot(I);
 k:=1.0-(sqr(Eta)*(1.0-sqr(NdotI)));
 if k>=0.0 then begin
  result:=((Eta*I)-((Eta*NdotI)+sqrt(k))*N);
 end else begin
  result:=0.0;
 end;
end;

function Refract(const I,N:TVector3;const Eta:TScalar):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
var NdotI,k:TScalar;
begin
 NdotI:=N.Dot(I);
 k:=1.0-(sqr(Eta)*(1.0-sqr(NdotI)));
 if k>=0.0 then begin
  result:=((Eta*I)-((Eta*NdotI)+sqrt(k))*N);
 end else begin
  result:=0.0;
 end;
end;

function Refract(const I,N:TVector4;const Eta:TScalar):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
var NdotI,k:TScalar;
begin
 NdotI:=N.Dot(I);
 k:=1.0-(sqr(Eta)*(1.0-sqr(NdotI)));
 if k>=0.0 then begin
  result:=((Eta*I)-((Eta*NdotI)+sqrt(k))*N);
 end else begin
  result:=0.0;
 end;
end;

function Clamp(const Value,MinValue,MaxValue:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=Min(Max(Value,MinValue),MaxValue);
end;

function Clamp(const Value,MinValue,MaxValue:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=Min(Max(Value.x,MinValue.x),MaxValue.x);
 result.y:=Min(Max(Value.y,MinValue.y),MaxValue.y);
end;

function Clamp(const Value,MinValue,MaxValue:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=Min(Max(Value.x,MinValue.x),MaxValue.x);
 result.y:=Min(Max(Value.y,MinValue.y),MaxValue.y);
 result.z:=Min(Max(Value.z,MinValue.z),MaxValue.z);
end;

function Clamp(const Value,MinValue,MaxValue:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=Min(Max(Value.x,MinValue.x),MaxValue.x);
 result.y:=Min(Max(Value.y,MinValue.y),MaxValue.y);
 result.z:=Min(Max(Value.z,MinValue.z),MaxValue.z);
 result.w:=Min(Max(Value.w,MinValue.w),MaxValue.w);
end;

function Mix(const a,b,t:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 if t<=0.0 then begin
  result:=a;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  result:=(a*(1.0-t))+(b*t);
 end;
end;

function Mix(const a,b,t:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=Mix(a.x,b.x,t.x);
 result.y:=Mix(a.y,b.y,t.y);
end;

function Mix(const a,b,t:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=Mix(a.x,b.x,t.x);
 result.y:=Mix(a.y,b.y,t.y);
 result.z:=Mix(a.z,b.z,t.z);
end;

function Mix(const a,b,t:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=Mix(a.x,b.x,t.x);
 result.y:=Mix(a.y,b.y,t.y);
 result.z:=Mix(a.z,b.z,t.z);
 result.w:=Mix(a.w,b.w,t.w);
end;

function Step(const Edge,Value:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 if Value<Edge then begin
  result:=0.0;
 end else begin
  result:=1.0;
 end;
end;

function Step(const Edge,Value:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=Step(Edge.x,Value.x);
 result.y:=Step(Edge.y,Value.y);
end;

function Step(const Edge,Value:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=Step(Edge.x,Value.x);
 result.y:=Step(Edge.y,Value.y);
 result.z:=Step(Edge.z,Value.z);
end;

function Step(const Edge,Value:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=Step(Edge.x,Value.x);
 result.y:=Step(Edge.y,Value.y);
 result.z:=Step(Edge.z,Value.z);
 result.w:=Step(Edge.w,Value.w);
end;

function NearestStep(const Edge0,Edge1,Value:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=(Value-Edge0)/(Edge1-Edge0);
 if result<0.5 then begin
  result:=0.0;
 end else begin
  result:=1.0;
 end;
end;

function NearestStep(const Edge0,Edge1,Value:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=NearestStep(Edge0.x,Edge1.x,Value.x);
 result.y:=NearestStep(Edge0.y,Edge1.y,Value.y);
end;

function NearestStep(const Edge0,Edge1,Value:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=NearestStep(Edge0.x,Edge1.x,Value.x);
 result.y:=NearestStep(Edge0.y,Edge1.y,Value.y);
 result.z:=NearestStep(Edge0.z,Edge1.z,Value.z);
end;

function NearestStep(const Edge0,Edge1,Value:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=NearestStep(Edge0.x,Edge1.x,Value.x);
 result.y:=NearestStep(Edge0.y,Edge1.y,Value.y);
 result.z:=NearestStep(Edge0.z,Edge1.z,Value.z);
 result.w:=NearestStep(Edge0.w,Edge1.w,Value.w);
end;

function LinearStep(const Edge0,Edge1,Value:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=(Value-Edge0)/(Edge1-Edge0);
 if result<=0.0 then begin
  result:=0.0;
 end else if result>=1.0 then begin
  result:=1.0;
 end;
end;

function LinearStep(const Edge0,Edge1,Value:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=LinearStep(Edge0.x,Edge1.x,Value.x);
 result.y:=LinearStep(Edge0.y,Edge1.y,Value.y);
end;

function LinearStep(const Edge0,Edge1,Value:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=LinearStep(Edge0.x,Edge1.x,Value.x);
 result.y:=LinearStep(Edge0.y,Edge1.y,Value.y);
 result.z:=LinearStep(Edge0.z,Edge1.z,Value.z);
end;

function LinearStep(const Edge0,Edge1,Value:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=LinearStep(Edge0.x,Edge1.x,Value.x);
 result.y:=LinearStep(Edge0.y,Edge1.y,Value.y);
 result.z:=LinearStep(Edge0.z,Edge1.z,Value.z);
 result.w:=LinearStep(Edge0.w,Edge1.w,Value.w);
end;

function SmoothStep(const Edge0,Edge1,Value:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=(Value-Edge0)/(Edge1-Edge0);
 if result<=0.0 then begin
  result:=0.0;
 end else if result>=1.0 then begin
  result:=1.0;
 end else begin
  result:=sqr(result)*(3.0-(2.0*result));
 end;
end;

function SmoothStep(const Edge0,Edge1,Value:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=SmoothStep(Edge0.x,Edge1.x,Value.x);
 result.y:=SmoothStep(Edge0.y,Edge1.y,Value.y);
end;

function SmoothStep(const Edge0,Edge1,Value:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=SmoothStep(Edge0.x,Edge1.x,Value.x);
 result.y:=SmoothStep(Edge0.y,Edge1.y,Value.y);
 result.z:=SmoothStep(Edge0.z,Edge1.z,Value.z);
end;

function SmoothStep(const Edge0,Edge1,Value:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=SmoothStep(Edge0.x,Edge1.x,Value.x);
 result.y:=SmoothStep(Edge0.y,Edge1.y,Value.y);
 result.z:=SmoothStep(Edge0.z,Edge1.z,Value.z);
 result.w:=SmoothStep(Edge0.w,Edge1.w,Value.w);
end;

function SmootherStep(const Edge0,Edge1,Value:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=(Value-Edge0)/(Edge1-Edge0);
 if result<=0.0 then begin
  result:=0.0;
 end else if result>=1.0 then begin
  result:=1.0;
 end else begin
  result:=(sqr(result)*result)*(result*((result*6.0)-15.0)+10);
 end;
end;

function SmootherStep(const Edge0,Edge1,Value:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=SmootherStep(Edge0.x,Edge1.x,Value.x);
 result.y:=SmootherStep(Edge0.y,Edge1.y,Value.y);
end;

function SmootherStep(const Edge0,Edge1,Value:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=SmootherStep(Edge0.x,Edge1.x,Value.x);
 result.y:=SmootherStep(Edge0.y,Edge1.y,Value.y);
 result.z:=SmootherStep(Edge0.z,Edge1.z,Value.z);
end;

function SmootherStep(const Edge0,Edge1,Value:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=SmootherStep(Edge0.x,Edge1.x,Value.x);
 result.y:=SmootherStep(Edge0.y,Edge1.y,Value.y);
 result.z:=SmootherStep(Edge0.z,Edge1.z,Value.z);
 result.w:=SmootherStep(Edge0.w,Edge1.w,Value.w);
end;

function SmoothestStep(const Edge0,Edge1,Value:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=(Value-Edge0)/(Edge1-Edge0);
 if result<=0.0 then begin
  result:=0.0;
 end else if result>=1.0 then begin
  result:=1.0;
 end else begin
  result:=((((-20.0)*sqr(result)*result)+(70.0*sqr(result)))-(84.0*result)+35.0)*sqr(sqr(result));
 end;
end;

function SmoothestStep(const Edge0,Edge1,Value:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=SmoothestStep(Edge0.x,Edge1.x,Value.x);
 result.y:=SmoothestStep(Edge0.y,Edge1.y,Value.y);
end;

function SmoothestStep(const Edge0,Edge1,Value:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=SmoothestStep(Edge0.x,Edge1.x,Value.x);
 result.y:=SmoothestStep(Edge0.y,Edge1.y,Value.y);
 result.z:=SmoothestStep(Edge0.z,Edge1.z,Value.z);
end;

function SmoothestStep(const Edge0,Edge1,Value:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=SmoothestStep(Edge0.x,Edge1.x,Value.x);
 result.y:=SmoothestStep(Edge0.y,Edge1.y,Value.y);
 result.z:=SmoothestStep(Edge0.z,Edge1.z,Value.z);
 result.w:=SmoothestStep(Edge0.w,Edge1.w,Value.w);
end;

function SuperSmoothestStep(const Edge0,Edge1,Value:TScalar):TScalar; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=(Value-Edge0)/(Edge1-Edge0);
 if result<=0.0 then begin
  result:=0.0;
 end else if result>=1.0 then begin
  result:=1.0;
 end else begin
  result:=0.5-(cos((0.5-(cos(result*PI)*0.5))*PI)*0.5);
 end;
end;

function SuperSmoothestStep(const Edge0,Edge1,Value:TVector2):TVector2; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=SuperSmoothestStep(Edge0.x,Edge1.x,Value.x);
 result.y:=SuperSmoothestStep(Edge0.y,Edge1.y,Value.y);
end;

function SuperSmoothestStep(const Edge0,Edge1,Value:TVector3):TVector3; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=SuperSmoothestStep(Edge0.x,Edge1.x,Value.x);
 result.y:=SuperSmoothestStep(Edge0.y,Edge1.y,Value.y);
 result.z:=SuperSmoothestStep(Edge0.z,Edge1.z,Value.z);
end;

function SuperSmoothestStep(const Edge0,Edge1,Value:TVector4):TVector4; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result.x:=SuperSmoothestStep(Edge0.x,Edge1.x,Value.x);
 result.y:=SuperSmoothestStep(Edge0.y,Edge1.y,Value.y);
 result.z:=SuperSmoothestStep(Edge0.z,Edge1.z,Value.z);
 result.w:=SuperSmoothestStep(Edge0.w,Edge1.w,Value.w);
end;

procedure DoCalculateInterval(const Vertices:PVector3s;const Count:TInt32;const Axis:TVector3;out OutMin,OutMax:TScalar);
var Distance:TScalar;
    Index:TInt32;
begin
 Distance:=Vertices^[0].Dot(Axis);
 OutMin:=Distance;
 OutMax:=Distance;
 for Index:=1 to Count-1 do begin
  Distance:=Vertices^[Index].Dot(Axis);
  if OutMin>Distance then begin
   OutMin:=Distance;
  end;
  if OutMax<Distance then begin
   OutMax:=Distance;
  end;
 end;
end;

function DoSpanIntersect(const Vertices1:PVector3s;const Count1:TInt32;const Vertices2:PVector3s;const Count2:TInt32;const AxisTest:TVector3;out AxisPenetration:TVector3):TScalar;
var min1,max1,min2,max2,len1,len2:TScalar;
begin
 AxisPenetration:=AxisTest.Normalize;
 DoCalculateInterval(Vertices1,Count1,AxisPenetration,min1,max1);
 DoCalculateInterval(Vertices2,Count2,AxisPenetration,min2,max2);
 if (max1<min2) or (min1>max2) then begin
  result:=-1.0;
 end else begin
  len1:=max1-min1;
  len2:=max2-min2;
  if (min1>min2) and (max1<max2) then begin
   result:=len1+min(abs(min1-min2),abs(max1-max2));
  end else if (min2>min1) and (max2<max1) then begin
   result:=len2+min(abs(min1-min2),abs(max1-max2));
  end else begin
   result:=(len1+len2)-(max(max1,max2)-min(min1,min2));
  end;
	if min2<min1 then begin
   AxisPenetration:=-AxisPenetration;
  end;
 end;
end;

function BoxGetDistanceToPoint(Point:TVector3;const Center,Size:TVector3;const InvTransformMatrix,TransformMatrix:TMatrix4x4;var ClosestBoxPoint:TVector3):TScalar;
var HalfSize:TVector3;
begin
 result:=0;
 ClosestBoxPoint:=(InvTransformMatrix*Point)-Center;
 HalfSize.x:=abs(Size.x*0.5);
 HalfSize.y:=abs(Size.y*0.5);
 HalfSize.z:=abs(Size.z*0.5);
 if ClosestBoxPoint.x<-HalfSize.x then begin
  result:=result+sqr(ClosestBoxPoint.x-(-HalfSize.x));
  ClosestBoxPoint.x:=-HalfSize.x;
 end else if ClosestBoxPoint.x>HalfSize.x then begin
  result:=result+sqr(ClosestBoxPoint.x-HalfSize.x);
  ClosestBoxPoint.x:=HalfSize.x;
 end;
 if ClosestBoxPoint.y<-HalfSize.y then begin
  result:=result+sqr(ClosestBoxPoint.y-(-HalfSize.y));
  ClosestBoxPoint.y:=-HalfSize.y;
 end else if ClosestBoxPoint.y>HalfSize.y then begin
  result:=result+sqr(ClosestBoxPoint.y-HalfSize.y);
  ClosestBoxPoint.y:=HalfSize.y;
 end;
 if ClosestBoxPoint.z<-HalfSize.z then begin
  result:=result+sqr(ClosestBoxPoint.z-(-HalfSize.z));
  ClosestBoxPoint.z:=-HalfSize.z;
 end else if ClosestBoxPoint.z>HalfSize.z then begin
  result:=result+sqr(ClosestBoxPoint.z-HalfSize.z);
  ClosestBoxPoint.z:=HalfSize.z;
 end;
 ClosestBoxPoint:=TransformMatrix*(ClosestBoxPoint+Center);
end;

function GetDistanceFromLine(const p0,p1,p:TVector3;var Projected:TVector3;const Time:PScalar=nil):TScalar;
var p10:TVector3;
    t:TScalar;
begin
 p10:=p1-p0;
 t:=p10.Length;
 if t<EPSILON then begin
  p10:=Vector3Origin;
 end else begin
  p10:=p10/t;
 end;
 t:=p10.Dot(p-p0);
 if assigned(Time) then begin
  Time^:=t;
 end;
 Projected:=p0+(p10*t);
 result:=(p-Projected).Length;
end;

procedure LineClosestApproach(const pa,ua,pb,ub:TVector3;var Alpha,Beta:TScalar);
var p:TVector3;
    uaub,q1,q2,d:TScalar;
begin
 p:=pb-pa;
 uaub:=ua.Dot(ub);
 q1:=ua.Dot(p);
 q2:=ub.Dot(p);
 d:=1.0-sqr(uaub);
 if d<EPSILON then begin
  Alpha:=0;
  Beta:=0;
 end else begin
  d:=1.0/d;
  Alpha:=(q1+(uaub*q2))*d;
  Beta:=((uaub*q1)+q2)*d;
 end;
end;

procedure ClosestLineBoxPoints(const p1,p2,c:TVector3;const ir,r:TMatrix4x4;const side:TVector3;var lret,bret:TVector3);
const tanchorepsilon:TFloat={$ifdef physicsdouble}1e-307{$else}1e-19{$endif};
var tmp,s,v,sign,v2,h:TVector3;
    region:array[0..2] of TInt32;
    tanchor:array[0..2] of TScalar;
    i:TInt32;
    t,dd2dt,nextt,nextdd2dt:TScalar;
    DoGetAnswer:boolean;
begin
 s:=ir*(p1-c);
 v:=ir*(p2-p1);
 for i:=0 to 2 do begin
  if v.RawComponents[i]<0 then begin
   s.RawComponents[i]:=-s.RawComponents[i];
   v.RawComponents[i]:=-v.RawComponents[i];
   sign.RawComponents[i]:=-1;
  end else begin
   sign.RawComponents[i]:=1;
  end;
 end;
 v2:=v*v;
 h:=side*0.5;
 for i:=0 to 2 do begin
  if v.RawComponents[i]>tanchorepsilon then begin
   if s.RawComponents[i]<-h.RawComponents[i] then begin
    region[i]:=-1;
    tanchor[i]:=((-h.RawComponents[i])-s.RawComponents[i])/v.RawComponents[i];
   end else begin
    if s.RawComponents[i]>h.RawComponents[i] then begin
     region[i]:=1;
    end else begin
     region[i]:=0;
    end;
    tanchor[i]:=(h.RawComponents[i]-s.RawComponents[i])/v.RawComponents[i];
   end;
  end else begin
   region[i]:=0;
   tanchor[i]:=2;
  end;
 end;
 t:=0;
 dd2dt:=0;
 for i:=0 to 2 do begin
  if region[i]<>0 then begin
   dd2dt:=dd2dt-(v2.RawComponents[i]*tanchor[i]);
  end;
 end;
 if dd2dt<0 then begin
  DoGetAnswer:=false;
  repeat
   nextt:=1;
   for i:=0 to 2 do begin
    if (tanchor[i]>t) and (tanchor[i]<1) and (tanchor[i]<nextt) then begin
     nextt:=tanchor[i];
    end;
   end;
   nextdd2dt:=0;
   for i:=0 to 2 do begin
    if region[i]<>0 then begin
     nextdd2dt:=nextdd2dt+(v2.RawComponents[i]*(nextt-tanchor[i]));
    end;
   end;
   if nextdd2dt>=0 then begin
    t:=t-(dd2dt/((nextdd2dt-dd2dt)/(nextt-t)));
    DoGetAnswer:=true;
    break;
   end;
   for i:=0 to 2 do begin
    if abs(tanchor[i]-nextt)<EPSILON then begin
     tanchor[i]:=(h.RawComponents[i]-s.RawComponents[i])/v.RawComponents[i];
     inc(region[i]);
    end;
   end;
   t:=nextt;
   dd2dt:=nextdd2dt;
  until t>=1;
  if not DoGetAnswer then begin
   t:=1;
  end;
 end;
 lret:=p1+((p2-p1)*t);
 for i:=0 to 2 do begin
  tmp.RawComponents[i]:=sign.RawComponents[i]*(s.RawComponents[i]+(t*v.RawComponents[i]));
  if tmp.RawComponents[i]<-h.RawComponents[i] then begin
   tmp.RawComponents[i]:=-h.RawComponents[i];
  end else if tmp.RawComponents[i]>h.RawComponents[i] then begin
   tmp.RawComponents[i]:=h.RawComponents[i];
  end;
 end;
 bret:=c+(r*tmp);
end;

procedure ClosestLineSegmentPoints(const a0,a1,b0,b1:TVector3;var cp0,cp1:TVector3);
var a0a1,b0b1,a0b0,a0b1,a1b0,a1b1,n:TVector3;
    la,lb,k,da0,da1,da2,da3,db0,db1,db2,db3,det,Alpha,Beta:TScalar;
begin
 a0a1:=a1-a0;
 b0b1:=b1-b0;
 a0b0:=b0-a0;
 da0:=a0a1.Dot(a0b0);
 db0:=b0b1.Dot(a0b0);
 if (da0<=0) and (db0>=0) then begin
  cp0:=a0;
  cp1:=b0;
  exit;
 end;
 a0b1:=b1-a0;
 da1:=a0a1.Dot(a0b1);
 db1:=b0b1.Dot(a0b1);
 if (da1<=0) and (db1<=0) then begin
  cp0:=a0;
  cp1:=b1;
  exit;
 end;
 a1b0:=b0-a1;
 da2:=a0a1.Dot(a1b0);
 db2:=b0b1.Dot(a1b0);
 if (da2>=0) and (db2>=0) then begin
  cp0:=a1;
  cp1:=b0;
  exit;
 end;
 a1b1:=b1-a1;
 da3:=a0a1.Dot(a1b1);
 db3:=b0b1.Dot(a1b1);
 if (da3>=0) and (db3<=0) then begin
  cp0:=a1;
  cp1:=b1;
  exit;
 end;
 la:=a0a1.Dot(a0a1);
 if (da0>=0) and (da2<=0) then begin
  k:=da0/la;
  n:=a0b0-(a0a1*k);
  if b0b1.Dot(n)>=0 then begin
   cp0:=a0+(a0a1*k);
   cp1:=b0;
   exit;
  end;
 end;
 if (da1>=0) and (da3<=0) then begin
  k:=da1/la;
  n:=a0b1-(a0a1*k);
  if b0b1.Dot(n)<=0 then begin
   cp0:=a0+(a0a1*k);
   cp1:=b1;
   exit;
  end;
 end;
 lb:=b0b1.Dot(b0b1);
 if (db0<=0) and (db1>=0) then begin
  k:=-db0/lb;
  n:=(b0b1*k)-a0a1;
  if a0a1.Dot(n)>=0 then begin
   cp0:=a0;
   cp1:=b0+(b0b1*k);
   exit;
  end;
 end;
 if (db2<=0) and (db3>=0) then begin
  k:=-db2/lb;
  n:=(b0b1*k)-a1b0;
  if a0a1.Dot(n)>=0 then begin
   cp0:=a1;
   cp1:=b0+(b0b1*k);
   exit;
  end;
 end;
 k:=a0a1.Dot(b0b1);
 det:=(la*lb)-sqr(k);
 if det<=EPSILON then begin
  cp0:=a0;
  cp1:=b0;
 end else begin
  det:=1/det;
  Alpha:=((lb*da0)-(k*db0))*det;
  Beta:=((k*da0)-(la*db0))*det;
  cp0:=a0+(a0a1*Alpha);
  cp1:=b0+(b0b1*Beta);
 end;
end;

function LineSegmentIntersection(const a0,a1,b0,b1:TVector3;const p:PVector3=nil):boolean;
var da,db,dc,cdadb:TVector3;
    t:TScalar;
begin
 result:=false;
 da:=a1-a0;
 db:=b1-b0;
 dc:=b0-a0;
 cdadb:=da.Cross(db);
 if abs(cdadb.Dot(dc))>EPSILON then begin
  // Lines are not coplanar
  exit;
 end;
 t:=dc.Cross(db).Dot(cdadb)/cdadb.SquaredLength;
 if (t>=0.0) and (t<=1.0) then begin
  if assigned(p) then begin
   p^:=a0.Lerp(a1,t);
  end;
  result:=true;
 end;
end;

function LineLineIntersection(const a0,a1,b0,b1:TVector3;const pa:PVector3=nil;const pb:PVector3=nil;const ta:PScalar=nil;const tb:PScalar=nil):boolean;
var p02,p32,p10:TVector3;
    d0232,d3210,d0210,d3232,d1010,Numerator,Denominator,lta,ltb:TScalar;
begin
 result:=false;

 p32.x:=b1.x-b0.x;
 p32.y:=b1.y-b0.y;
 p32.z:=b1.z-b0.z;
 if (abs(p32.x)<EPSILON) and (abs(p32.y)<EPSILON) and (abs(p32.z)<EPSILON) then begin
  exit;
 end;

 p10.x:=a1.x-a0.x;
 p10.y:=a1.y-a0.y;
 p10.z:=a1.z-a0.z;
 if (abs(p10.x)<EPSILON) and (abs(p10.y)<EPSILON) and (abs(p10.z)<EPSILON) then begin
  exit;
 end;

 p02.x:=a0.x-b0.x;
 p02.y:=a0.y-b0.y;
 p02.z:=a0.z-b0.z;

 d0232:=(p02.x*p32.x)+(p02.y*p32.y)+(p02.z*p32.z);
 d3210:=(p32.x*p10.x)+(p32.y*p10.y)+(p32.z*p10.z);
 d0210:=(p02.x*p10.x)+(p02.y*p10.y)+(p02.z*p10.z);
 d3232:=(p32.x*p32.x)+(p32.y*p32.y)+(p32.z*p32.z);
 d1010:=(p10.x*p10.x)+(p10.y*p10.y)+(p10.z*p10.z);

 Denominator:=(d1010*d3232)-(d3210*d3210);
 if abs(Denominator)<EPSILON then begin
  exit;
 end;

 if assigned(pa) or assigned(pb) or assigned(ta) or assigned(tb) then begin
  Numerator:=(d0232*d3210)-(d0210*d3232);
  lta:=Numerator/Denominator;
  ltb:=(d0232+(d3210*lta))/d3232;
  if assigned(ta) then begin
   ta^:=lta;
  end;
  if assigned(tb) then begin
   tb^:=ltb;
  end;
  if assigned(pa) then begin
   pa^:=a0.Lerp(a1,lta);
  end;
  if assigned(pb) then begin
   pb^:=b0.Lerp(b1,ltb);
  end;
 end;

 result:=true;
end;

function IsPointsSameSide(const p0,p1,Origin,Direction:TVector3):boolean; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=Direction.Cross(p0-Origin).Dot(Direction.Cross(p1-Origin))>=0.0;
end;

function PointInTriangle(const p0,p1,p2,Normal,p:TVector3):boolean; overload; {$ifdef CAN_INLINE}inline;{$endif}
var r0,r1,r2:TScalar;
begin
 r0:=(p1-p0).Cross(Normal).Dot(p-p0);
 r1:=(p2-p1).Cross(Normal).Dot(p-p1);
 r2:=(p0-p2).Cross(Normal).Dot(p-p2);
 result:=((r0>0.0) and (r1>0.0) and (r2>0.0)) or ((r0<=0.0) and (r1<=0.0) and (r2<=0.0));
end;

function PointInTriangle(const p0,p1,p2,p:TVector3):boolean; overload; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=IsPointsSameSide(p,p0,p1,p2-p1) and
         IsPointsSameSide(p,p1,p0,p2-p0) and
         IsPointsSameSide(p,p2,p0,p1-p0);
end;

function GetOverlap(const MinA,MaxA,MinB,MaxB:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
var Mins,Maxs:TScalar;
begin
 if (MinA>MaxB) or (MaxA<MinB) then begin
  result:=0.0;
 end else begin
  if MinA<MinB then begin
   result:=MinB-MaxA;
  end else begin
   result:=MinA-MaxB;
  end;
  if ((MinB>=MinA) and (MaxB<=MaxA)) or ((MinA>=MinB) and (MaxA<=MaxB)) then begin
   Mins:=abs(MinA-MinB);
   Maxs:=abs(MaxA-MaxB);
   if Mins<Maxs then begin
    result:=result+Mins;
   end else begin
    result:=result+Maxs;
   end;
  end;
 end;
end;

function OldTriangleTriangleIntersection(const a0,a1,a2,b0,b1,b2:TVector3):boolean;
const EPSILON=1e-2;
      LINEEPSILON=1e-6;
var Index,NextIndex,RemainingIndex,i,j,k,h:TInt32;
    tS,tT0,tT1:TScalar;
    v:array[0..1,0..2] of TVector3;
    SegmentTriangles:array[0..1] of TSegmentTriangle;
    Segment:TRelativeSegment;
    lv,plv:TVector3;
    OK:boolean;
begin
 result:=false;
 v[0,0]:=a0;
 v[0,1]:=a1;
 v[0,2]:=a2;
 v[1,0]:=b0;
 v[1,1]:=b1;
 v[1,2]:=b2;
 SegmentTriangles[0].Origin:=a0;
 SegmentTriangles[0].Edge0:=a1-a0;
 SegmentTriangles[0].Edge1:=a2-a0;
 SegmentTriangles[1].Origin:=b0;
 SegmentTriangles[1].Edge0:=b1-b0;
 SegmentTriangles[1].Edge1:=b2-b0;
 for Index:=0 to 2 do begin
  NextIndex:=Index+1;
  if NextIndex>2 then begin
   dec(NextIndex,3);
  end;
  RemainingIndex:=NextIndex+1;
  if RemainingIndex>2 then begin
   dec(RemainingIndex,3);
  end;

  for i:=0 to 3 do begin
   case i of
    0:begin
     Segment.Origin:=v[0,Index];
     Segment.Delta:=v[0,NextIndex]-v[0,Index];
     j:=1;
    end;
    1:begin
     Segment.Origin:=v[1,Index];
     Segment.Delta:=v[1,NextIndex]-v[1,Index];
     j:=0;
    end;
    2:begin
     Segment.Origin:=v[0,Index];
     Segment.Delta:=((v[0,NextIndex]+v[0,RemainingIndex])*0.5)-v[0,Index];
     j:=1;
    end;
    else begin
     Segment.Origin:=v[1,Index];
     Segment.Delta:=((v[1,NextIndex]+v[1,RemainingIndex])*0.5)-v[1,Index];
     j:=0;
    end;
   end;
   if SegmentTriangles[j].RelativeSegmentIntersection(Segment,tS,tT0,tT1) then begin
    OK:=true;
    if i<2 then begin
     lv:=Segment.Origin+(Segment.Delta*tS);
     for k:=0 to 2 do begin
      h:=k+1;
      if h>2 then begin
       dec(h,2);
      end;
      if GetDistanceFromLine(v[j,k],v[j,h],lv,plv)<EPSILON then begin
       OK:=false;
       break;
      end;
     end;
    end;
    if OK and (((tT0>EPSILON) and (tT0<(1.0-EPSILON))) or ((tT1>EPSILON) and (tT1<(1.0-EPSILON)))) then begin
     result:=true;
     exit;
    end;
   end;
  end;

 end;
end;

function TriangleTriangleIntersection(const v0,v1,v2,u0,u1,u2:TVector3):boolean;
const EPSILON=1e-6;
 procedure SORT(var a,b:TScalar); {$ifdef CAN_INLINE}inline;{$endif}
 var c:TScalar;
 begin
  if a>b then begin
   c:=a;
   a:=b;
   b:=c;
  end;
 end;
 procedure ISECT(const VV0,VV1,VV2,D0,D1,D2:TScalar;var isect0,isect1:TScalar); {$ifdef CAN_INLINE}inline;{$endif}
 begin
  isect0:=VV0+(((VV1-VV0)*D0)/(D0-D1));
  isect1:=VV0+(((VV2-VV0)*D0)/(D0-D2));
 end;
 function EDGE_EDGE_TEST(const v0,u0,u1:TVector3;const Ax,Ay:TScalar;const i0,i1:TInt32):boolean; {$ifdef CAN_INLINE}inline;{$endif}
 var Bx,By,Cx,Cy,e,f,d:TScalar;
 begin
  result:=false;
  Bx:=U0.RawComponents[i0]-U1.RawComponents[i0];
  By:=U0.RawComponents[i1]-U1.RawComponents[i1];
  Cx:=V0.RawComponents[i0]-U0.RawComponents[i0];
  Cy:=V0.RawComponents[i1]-U0.RawComponents[i1];
  f:=(Ay*Bx)-(Ax*By);
  d:=(By*Cx)-(Bx*Cy);
  if ((f>0.0) and (d>=0.0) and (d<=f)) or ((f<0.0) and (d<=0.0) and (d>=f)) then begin
   e:=(Ax*Cy)-(Ay*Cx);
   if f>0.0 then begin
    if (e>=0.0) and (e<=f) then begin
     result:=true;
    end;
   end else begin
    if (e<=0.0) and (e>=f) then begin
     result:=true;
    end;
   end;
  end;
 end;
 function POINT_IN_TRI(const v0,u0,u1,u2:TVector3;const i0,i1:TInt32):boolean;
 var a,b,c,d0,d1,d2:TScalar;
 begin

  // is T1 completly inside T2?
  // check if V0 is inside tri(U0,U1,U2)

  a:=U1.RawComponents[i1]-U0.RawComponents[i1];
  b:=-(U1.RawComponents[i0]-U0.RawComponents[i0]);
  c:=(-(a*U0.RawComponents[i0]))-(b*U0.RawComponents[i1]);
  d0:=((a*V0.RawComponents[i0])+(b*V0.RawComponents[i1]))+c;

  a:=U2.RawComponents[i1]-U1.RawComponents[i1];
  b:=-(U2.RawComponents[i0]-U1.RawComponents[i0]);
  c:=(-(a*U1.RawComponents[i0]))-(b*U1.RawComponents[i1]);
  d1:=((a*V0.RawComponents[i0])+(b*V0.RawComponents[i1]))+c;

  a:=U0.RawComponents[i1]-U2.RawComponents[i1];
  b:=-(U0.RawComponents[i0]-U2.RawComponents[i0]);
  c:=(-(a*U2.RawComponents[i0]))-(b*U2.RawComponents[i1]);
  d2:=((a*V0.RawComponents[i0])+(b*V0.RawComponents[i1]))+c;

  result:=((d0*d1)>0.0) and ((d0*d2)>0.0);
 end;
 function EDGE_AGAINST_TRI_EDGES(const v0,v1,u0,u1,u2:TVector3;const i0,i1:TInt32):boolean;
 var Ax,Ay:TScalar;
 begin
  Ax:=v1.RawComponents[i0]-v0.RawComponents[i0];
  Ay:=v1.RawComponents[i1]-v0.RawComponents[i1];
  result:=EDGE_EDGE_TEST(V0,U0,U1,Ax,Ay,i0,i1) or // test edge U0,U1 against V0,V1
          EDGE_EDGE_TEST(V0,U1,U2,Ax,Ay,i0,i1) or // test edge U1,U2 against V0,V1
          EDGE_EDGE_TEST(V0,U2,U0,Ax,Ay,i0,i1);   // test edge U2,U1 against V0,V1
 end;
 function coplanar_tri_tri(const n,v0,v1,v2,u0,u1,u2:TVector3):boolean;
 var i0,i1:TInt32;
     a:TVector3;
 begin
  a.x:=abs(n.x);
  a.y:=abs(n.y);
  a.z:=abs(n.z);
  if a.x>a.y then begin
   if a.x>a.z then begin
    i0:=1;
    i1:=2;
   end else begin
    i0:=0;
    i1:=1;
   end;
  end else begin
   if a.y<a.z then begin
    i0:=0;
    i1:=1;
   end else begin
    i0:=0;
    i1:=2;
   end;
  end;
  // test all edges of triangle 1 against the edges of triangle 2
  result:=EDGE_AGAINST_TRI_EDGES(V0,V1,U0,U1,U2,i0,i1) or
          EDGE_AGAINST_TRI_EDGES(V1,V2,U0,U1,U2,i0,i1) or
          EDGE_AGAINST_TRI_EDGES(V2,V0,U0,U1,U2,i0,i1) or
          POINT_IN_TRI(V0,U0,U1,U2,i0,i1) or // finally, test if tri1 is totally contained in tri2 or vice versa
          POINT_IN_TRI(U0,V0,V1,V2,i0,i1);
 end;
 function COMPUTE_INTERVALS(const N1:TVector3;const VV0,VV1,VV2,D0,D1,D2,D0D1,D0D2:TScalar;var isect0,isect1:TScalar):boolean;
 begin
  result:=false;
  if D0D1>0.0 then begin
   // here we know that D0D2<=0.0
   // that is D0, D1 are on the same side, D2 on the other or on the plane
   ISECT(VV2,VV0,VV1,D2,D0,D1,isect0,isect1);
  end else if D0D2>0.0 then begin
   // here we know that d0d1<=0.0
   ISECT(VV1,VV0,VV2,D1,D0,D2,isect0,isect1);
  end else if ((D1*D2)>0.0) or (D0<>0.0) then begin
   // here we know that d0d1<=0.0 or that D0<>0.0
   ISECT(VV0,VV1,VV2,D0,D1,D2,isect0,isect1);
  end else if D1<>0.0 then begin
   ISECT(VV1,VV0,VV2,D1,D0,D2,isect0,isect1);
  end else if D2<>0.0 then begin
   ISECT(VV2,VV0,VV1,D2,D0,D1,isect0,isect1);
  end else begin
   // triangles are coplanar
   result:=coplanar_tri_tri(N1,V0,V1,V2,U0,U1,U2);
  end;
 end;
var index:TInt32;
    d1,d2,du0,du1,du2,dv0,dv1,dv2,du0du1,du0du2,dv0dv1,dv0dv2,vp0,vp1,vp2,up0,up1,up2,b,c,m:TScalar;
    isect1,isect2:array[0..1] of TScalar;
    e1,e2,n1,n2,d:TVector3;
begin

 result:=false;

 // compute plane equation of triangle(V0,V1,V2)
 e1:=v1-v0;
 e2:=v2-v0;
 n1:=e1.Cross(e2);
 d1:=-n1.Dot(v0);

 // put U0,U1,U2 into plane equation 1 to compute signed distances to the plane
 du0:=n1.Dot(u0)+d1;
 du1:=n1.Dot(u1)+d1;
 du2:=n1.Dot(u2)+d1;

 // coplanarity robustness check
 if abs(du0)<EPSILON then begin
  du0:=0.0;
 end;
 if abs(du1)<EPSILON then begin
  du1:=0.0;
 end;
 if abs(du2)<EPSILON then begin
  du2:=0.0;
 end;

 du0du1:=du0*du1;
 du0du2:=du0*du2;

 // same sign on all of them + not equal 0 ?
 if (du0du1>0.0) and (du0du2>0.0) then begin
  // no intersection occurs
  exit;
 end;

 // compute plane of triangle (U0,U1,U2)
 e1:=u1-u0;
 e2:=u2-u0;
 n2:=e1.Cross(e2);
 d2:=-n2.Dot(u0);

 // put V0,V1,V2 into plane equation 2
 dv0:=n2.Dot(v0)+d2;
 dv1:=n2.Dot(v1)+d2;
 dv2:=n2.Dot(v2)+d2;

 // coplanarity robustness check
 if abs(dv0)<EPSILON then begin
  dv0:=0.0;
 end;
 if abs(dv1)<EPSILON then begin
  dv1:=0.0;
 end;
 if abs(dv2)<EPSILON then begin
  dv2:=0.0;
 end;

 dv0dv1:=dv0*dv1;
 dv0dv2:=dv0*dv2;

 // same sign on all of them + not equal 0 ?
 if (dv0dv1>0.0) and (dv0dv2>0.0) then begin
  // no intersection occurs
  exit;
 end;

 // compute direction of intersection line
 d:=n1.Cross(n2);

 // compute and index to the largest component of D
 m:=abs(d.x);
 index:=0;
 b:=abs(d.y);
 c:=abs(d.z);
 if m<b then begin
  m:=b;
  index:=1;
 end;
 if m<c then begin
//m:=c;
  index:=2;
 end;

 // this is the simplified projection onto L
 vp0:=v0.RawComponents[index];
 vp1:=v1.RawComponents[index];
 vp2:=v2.RawComponents[index];

 up0:=u0.RawComponents[index];
 up1:=u1.RawComponents[index];
 up2:=u2.RawComponents[index];

 // compute interval for triangle 1
 result:=COMPUTE_INTERVALS(N1,vp0,vp1,vp2,dv0,dv1,dv2,dv0dv1,dv0dv2,isect1[0],isect1[1]);
 if result then begin
  exit;
 end;

 // compute interval for triangle 2
 result:=COMPUTE_INTERVALS(N1,up0,up1,up2,du0,du1,du2,du0du1,du0du2,isect2[0],isect2[1]);
 if result then begin
  exit;
 end;

 SORT(isect1[0],isect1[1]);
 SORT(isect2[0],isect2[1]);

 result:=not ((isect1[1]<isect2[0]) or (isect2[1]<isect1[0]));
end;

function ClosestPointToLine(const LineStartPoint,LineEndPoint,Point:TVector3;const ClosestPointOnLine:PVector3=nil;const Time:PScalar=nil):TScalar;
var LineSegmentPointsDifference,ClosestPoint:TVector3;
    LineSegmentLengthSquared,PointOnLineSegmentTime:TScalar;
begin
 LineSegmentPointsDifference:=LineEndPoint-LineStartPoint;
 LineSegmentLengthSquared:=LineSegmentPointsDifference.SquaredLength;
 if LineSegmentLengthSquared<EPSILON then begin
  PointOnLineSegmentTime:=0.0;
  ClosestPoint:=LineStartPoint;
 end else begin
  PointOnLineSegmentTime:=(Point-LineStartPoint).Dot(LineSegmentPointsDifference)/LineSegmentLengthSquared;
  if PointOnLineSegmentTime<=0.0 then begin
   PointOnLineSegmentTime:=0.0;
   ClosestPoint:=LineStartPoint;
  end else if PointOnLineSegmentTime>=1.0 then begin
   PointOnLineSegmentTime:=1.0;
   ClosestPoint:=LineEndPoint;
  end else begin
   ClosestPoint:=LineStartPoint+(LineSegmentPointsDifference*PointOnLineSegmentTime);
  end;
 end;
 if assigned(ClosestPointOnLine) then begin
  ClosestPointOnLine^:=ClosestPoint;
 end;
 if assigned(Time) then begin
  Time^:=PointOnLineSegmentTime;
 end;
 result:=Point.DistanceTo(ClosestPoint);
end;

function ClosestPointToAABB(const AABB:TAABB;const Point:TVector3;const ClosestPointOnAABB:PVector3=nil):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
var ClosestPoint:TVector3;
begin
 ClosestPoint.x:=Min(Max(Point.x,AABB.Min.x),AABB.Max.x);
 ClosestPoint.y:=Min(Max(Point.y,AABB.Min.y),AABB.Max.y);
 ClosestPoint.z:=Min(Max(Point.z,AABB.Min.z),AABB.Max.z);
 if assigned(ClosestPointOnAABB) then begin
  ClosestPointOnAABB^:=ClosestPoint;
 end;
 result:=ClosestPoint.DistanceTo(Point);
end;

function ClosestPointToOBB(const OBB:TOBB;const Point:TVector3;out ClosestPoint:TVector3):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
var DistanceVector:TVector3;
begin
 DistanceVector:=Point-OBB.Center;
 ClosestPoint:=OBB.Center+
               (OBB.Axis[0]*Min(Max(DistanceVector.Dot(OBB.Axis[0]),-OBB.Extents.RawComponents[0]),OBB.Extents.RawComponents[0]))+
               (OBB.Axis[1]*Min(Max(DistanceVector.Dot(OBB.Axis[1]),-OBB.Extents.RawComponents[1]),OBB.Extents.RawComponents[1]))+
               (OBB.Axis[2]*Min(Max(DistanceVector.Dot(OBB.Axis[2]),-OBB.Extents.RawComponents[2]),OBB.Extents.RawComponents[2]));
 result:=ClosestPoint.DistanceTo(Point);
end;

function ClosestPointToSphere(const Sphere:TSphere;const Point:TVector3;out ClosestPoint:TVector3):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=Max(0.0,Sphere.Center.DistanceTo(Point)-Sphere.Radius);
 ClosestPoint:=Point+((Sphere.Center-Point).Normalize*result);
end;

function ClosestPointToCapsule(const Capsule:TCapsule;const Point:TVector3;out ClosestPoint:TVector3;const Time:PScalar=nil):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
var LineSegmentPointsDifference,LineClosestPoint:TVector3;
    LineSegmentLengthSquared,PointOnLineSegmentTime:TScalar;
begin
 LineSegmentPointsDifference:=Capsule.LineEndPoint-Capsule.LineStartPoint;
 LineSegmentLengthSquared:=LineSegmentPointsDifference.SquaredLength;
 if LineSegmentLengthSquared<EPSILON then begin
  PointOnLineSegmentTime:=0.0;
  LineClosestPoint:=Capsule.LineStartPoint;
 end else begin
  PointOnLineSegmentTime:=(Point-Capsule.LineStartPoint).Dot(LineSegmentPointsDifference)/LineSegmentLengthSquared;
  if PointOnLineSegmentTime<=0.0 then begin
   PointOnLineSegmentTime:=0.0;
   LineClosestPoint:=Capsule.LineStartPoint;
  end else if PointOnLineSegmentTime>=1.0 then begin
   PointOnLineSegmentTime:=1.0;
   LineClosestPoint:=Capsule.LineEndPoint;
  end else begin
   LineClosestPoint:=Capsule.LineStartPoint+(LineSegmentPointsDifference*PointOnLineSegmentTime);
  end;
 end;
 LineSegmentPointsDifference:=LineClosestPoint-Point;
 result:=Max(0.0,LineSegmentPointsDifference.Length-Capsule.Radius);
 ClosestPoint:=Point+(LineSegmentPointsDifference.Normalize*result);
 if assigned(Time) then begin
  Time^:=PointOnLineSegmentTime;
 end;
end;

function ClosestPointToTriangle(const a,b,c,p:TVector3;out ClosestPoint:TVector3):TScalar;
var ab,ac,bc,pa,pb,pc,ap,bp,cp,n:TVector3;
    snom,sdenom,tnom,tdenom,unom,udenom,vc,vb,va,u,v,w:TScalar;
begin

 ab.x:=b.x-a.x;
 ab.y:=b.y-a.y;
 ab.z:=b.z-a.z;

 ac.x:=c.x-a.x;
 ac.y:=c.y-a.y;
 ac.z:=c.z-a.z;

 bc.x:=c.x-b.x;
 bc.y:=c.y-b.y;
 bc.z:=c.z-b.z;

 pa.x:=p.x-a.x;
 pa.y:=p.y-a.y;
 pa.z:=p.z-a.z;

 pb.x:=p.x-b.x;
 pb.y:=p.y-b.y;
 pb.z:=p.z-b.z;

 pc.x:=p.x-c.x;
 pc.y:=p.y-c.y;
 pc.z:=p.z-c.z;

 // Determine the parametric position s for the projection of P onto AB (i.e. P’ = A+s*AB, where
 // s = snom/(snom+sdenom), and then parametric position t for P projected onto AC
 snom:=(ab.x*pa.x)+(ab.y*pa.y)+(ab.z*pa.z);
 sdenom:=(pb.x*(a.x-b.x))+(pb.y*(a.y-b.y))+(pb.z*(a.z-b.z));
 tnom:=(ac.x*pa.x)+(ac.y*pa.y)+(ac.z*pa.z);
 tdenom:=(pc.x*(a.x-c.x))+(pc.y*(a.y-c.y))+(pc.z*(a.z-c.z));
 if (snom<=0.0) and (tnom<=0.0) then begin
  // Vertex voronoi region hit early out
  ClosestPoint:=a;
  result:=sqrt(sqr(ClosestPoint.x-p.x)+sqr(ClosestPoint.y-p.y)+sqr(ClosestPoint.z-p.z));
  exit;
 end;

 // Parametric position u for P projected onto BC
 unom:=(bc.x*pb.x)+(bc.y*pb.y)+(bc.z*pb.z);
 udenom:=(pc.x*(b.x-c.x))+(pc.y*(b.y-c.y))+(pc.z*(b.z-c.z));
 if (sdenom<=0.0) and (unom<=0.0) then begin
  // Vertex voronoi region hit early out
  ClosestPoint:=b;
  result:=sqrt(sqr(ClosestPoint.x-p.x)+sqr(ClosestPoint.y-p.y)+sqr(ClosestPoint.z-p.z));
  exit;
 end;
 if (tdenom<=0.0) and (udenom<=0.0) then begin
  // Vertex voronoi region hit early out
  ClosestPoint:=c;
  result:=sqrt(sqr(ClosestPoint.x-p.x)+sqr(ClosestPoint.y-p.y)+sqr(ClosestPoint.z-p.z));
  exit;
 end;

 // Determine if P is outside (or on) edge AB by finding the area formed by vectors PA, PB and
 // the triangle normal. A scalar triple product is used. P is outside (or on) AB if the triple
 // scalar product [N PA PB] <= 0
 n.x:=(ab.y*ac.z)-(ab.z*ac.y);
 n.y:=(ab.z*ac.x)-(ab.x*ac.z);
 n.z:=(ab.x*ac.y)-(ab.y*ac.x);
 ap.x:=a.x-p.x;
 ap.y:=a.y-p.y;
 ap.z:=a.z-p.z;
 bp.x:=b.x-p.x;
 bp.y:=b.y-p.y;
 bp.z:=b.z-p.z;
 vc:=(n.x*((ap.y*bp.z)-(ap.z*bp.y)))+(n.y*((ap.z*bp.x)-(ap.x*bp.z)))+(n.z*((ap.x*bp.y)-(ap.y*bp.x)));

 // If P is outside of AB (signed area <= 0) and within voronoi feature region, then return
 // projection of P onto AB
 if (vc<=0.0) and (snom>=0.0) and (sdenom>=0.0) then begin
  u:=snom/(snom+sdenom);
  ClosestPoint.x:=a.x+(ab.x*u);
  ClosestPoint.y:=a.y+(ab.y*u);
  ClosestPoint.z:=a.z+(ab.z*u);
  result:=sqrt(sqr(ClosestPoint.x-p.x)+sqr(ClosestPoint.y-p.y)+sqr(ClosestPoint.z-p.z));
  exit;
 end;

 // Repeat the same test for P onto BC
 cp.x:=c.x-p.x;
 cp.y:=c.y-p.y;
 cp.z:=c.z-p.z;
 va:=(n.x*((bp.y*cp.z)-(bp.z*cp.y)))+(n.y*((bp.z*cp.x)-(bp.x*cp.z)))+(n.z*((bp.x*cp.y)-(bp.y*cp.x)));
 if (va<=0.0) and (unom>=0.0) and (udenom>=0.0) then begin
  v:=unom/(unom+udenom);
  ClosestPoint.x:=b.x+(bc.x*v);
  ClosestPoint.y:=b.y+(bc.y*v);
  ClosestPoint.z:=b.z+(bc.z*v);
  result:=sqrt(sqr(ClosestPoint.x-p.x)+sqr(ClosestPoint.y-p.y)+sqr(ClosestPoint.z-p.z));
  exit;
 end;

 // Repeat the same test for P onto CA
 vb:=(n.x*((cp.y*ap.z)-(cp.z*ap.y)))+(n.y*((cp.z*ap.x)-(cp.x*ap.z)))+(n.z*((cp.x*ap.y)-(cp.y*ap.x)));
 if (vb<=0.0) and (tnom>=0.0) and (tdenom>=0.0) then begin
  w:=tnom/(tnom+tdenom);
  ClosestPoint.x:=a.x+(ac.x*w);
  ClosestPoint.y:=a.y+(ac.y*w);
  ClosestPoint.z:=a.z+(ac.z*w);
  result:=sqrt(sqr(ClosestPoint.x-p.x)+sqr(ClosestPoint.y-p.y)+sqr(ClosestPoint.z-p.z));
  exit;
 end;

 // P must project onto inside face. Find closest point using the barycentric coordinates
 w:=1.0/(va+vb+vc);
 u:=va*w;
 v:=vb*w;
 w:=(1.0-u)-v;

 ClosestPoint.x:=(a.x*u)+(b.x*v)+(c.x*w);
 ClosestPoint.y:=(a.y*u)+(b.y*v)+(c.y*w);
 ClosestPoint.z:=(a.z*u)+(b.z*v)+(c.z*w);
 result:=sqrt(sqr(ClosestPoint.x-p.x)+sqr(ClosestPoint.y-p.y)+sqr(ClosestPoint.z-p.z));
end;

function SquaredDistanceFromPointToAABB(const AABB:TAABB;const Point:TVector3):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
var ClosestPoint:TVector3;
begin
 ClosestPoint.x:=Min(Max(Point.x,AABB.Min.x),AABB.Max.x);
 ClosestPoint.y:=Min(Max(Point.y,AABB.Min.y),AABB.Max.y);
 ClosestPoint.z:=Min(Max(Point.z,AABB.Min.z),AABB.Max.z);
 result:=(ClosestPoint-Point).SquaredLength;
end;

function SquaredDistanceFromPointToTriangle(const p,a,b,c:TVector3):TScalar;
var ab,ac,bc,pa,pb,pc,ap,bp,cp,n:TVector3;
    snom,sdenom,tnom,tdenom,unom,udenom,vc,vb,va,u,v,w:TScalar;
begin

 ab.x:=b.x-a.x;
 ab.y:=b.y-a.y;
 ab.z:=b.z-a.z;

 ac.x:=c.x-a.x;
 ac.y:=c.y-a.y;
 ac.z:=c.z-a.z;

 bc.x:=c.x-b.x;
 bc.y:=c.y-b.y;
 bc.z:=c.z-b.z;

 pa.x:=p.x-a.x;
 pa.y:=p.y-a.y;
 pa.z:=p.z-a.z;

 pb.x:=p.x-b.x;
 pb.y:=p.y-b.y;
 pb.z:=p.z-b.z;

 pc.x:=p.x-c.x;
 pc.y:=p.y-c.y;
 pc.z:=p.z-c.z;

 // Determine the parametric position s for the projection of P onto AB (i.e. PPU2 = A+s*AB, where
 // s = snom/(snom+sdenom), and then parametric position t for P projected onto AC
 snom:=(ab.x*pa.x)+(ab.y*pa.y)+(ab.z*pa.z);
 sdenom:=(pb.x*(a.x-b.x))+(pb.y*(a.y-b.y))+(pb.z*(a.z-b.z));
 tnom:=(ac.x*pa.x)+(ac.y*pa.y)+(ac.z*pa.z);
 tdenom:=(pc.x*(a.x-c.x))+(pc.y*(a.y-c.y))+(pc.z*(a.z-c.z));
 if (snom<=0.0) and (tnom<=0.0) then begin
  // Vertex voronoi region hit early out
  result:=sqr(a.x-p.x)+sqr(a.y-p.y)+sqr(a.z-p.z);
  exit;
 end;

 // Parametric position u for P projected onto BC
 unom:=(bc.x*pb.x)+(bc.y*pb.y)+(bc.z*pb.z);
 udenom:=(pc.x*(b.x-c.x))+(pc.y*(b.y-c.y))+(pc.z*(b.z-c.z));
 if (sdenom<=0.0) and (unom<=0.0) then begin
  // Vertex voronoi region hit early out
  result:=sqr(b.x-p.x)+sqr(b.y-p.y)+sqr(b.z-p.z);
  exit;
 end;
 if (tdenom<=0.0) and (udenom<=0.0) then begin
  // Vertex voronoi region hit early out
  result:=sqr(c.x-p.x)+sqr(c.y-p.y)+sqr(c.z-p.z);
  exit;
 end;

 // Determine if P is outside (or on) edge AB by finding the area formed by vectors PA, PB and
 // the triangle normal. A scalar triple product is used. P is outside (or on) AB if the triple
 // scalar product [N PA PB] <= 0
 n.x:=(ab.y*ac.z)-(ab.z*ac.y);
 n.y:=(ab.z*ac.x)-(ab.x*ac.z);
 n.z:=(ab.x*ac.y)-(ab.y*ac.x);
 ap.x:=a.x-p.x;
 ap.y:=a.y-p.y;
 ap.z:=a.z-p.z;
 bp.x:=b.x-p.x;
 bp.y:=b.y-p.y;
 bp.z:=b.z-p.z;
 vc:=(n.x*((ap.y*bp.z)-(ap.z*bp.y)))+(n.y*((ap.z*bp.x)-(ap.x*bp.z)))+(n.z*((ap.x*bp.y)-(ap.y*bp.x)));

 // If P is outside of AB (signed area <= 0) and within voronoi feature region, then return
 // projection of P onto AB
 if (vc<=0.0) and (snom>=0.0) and (sdenom>=0.0) then begin
  u:=snom/(snom+sdenom);
  result:=sqr((a.x+(ab.x*u))-p.x)+sqr((a.y+(ab.y*u))-p.y)+sqr((a.z+(ab.z*u))-p.z);
  exit;
 end;

 // Repeat the same test for P onto BC
 cp.x:=c.x-p.x;
 cp.y:=c.y-p.y;
 cp.z:=c.z-p.z;
 va:=(n.x*((bp.y*cp.z)-(bp.z*cp.y)))+(n.y*((bp.z*cp.x)-(bp.x*cp.z)))+(n.z*((bp.x*cp.y)-(bp.y*cp.x)));
 if (va<=0.0) and (unom>=0.0) and (udenom>=0.0) then begin
  v:=unom/(unom+udenom);
  result:=sqr((b.x+(bc.x*v))-p.x)+sqr((b.y+(bc.y*v))-p.y)+sqr((b.z+(bc.z*v))-p.z);
  exit;
 end;

 // Repeat the same test for P onto CA
 vb:=(n.x*((cp.y*ap.z)-(cp.z*ap.y)))+(n.y*((cp.z*ap.x)-(cp.x*ap.z)))+(n.z*((cp.x*ap.y)-(cp.y*ap.x)));
 if (vb<=0.0) and (tnom>=0.0) and (tdenom>=0.0) then begin
  w:=tnom/(tnom+tdenom);
  result:=sqr((a.x+(ac.x*w))-p.x)+sqr((a.y+(ac.y*w))-p.y)+sqr((a.z+(ac.z*w))-p.z);
  exit;
 end;

 // P must project onto inside face. Find closest point using the barycentric coordinates
 w:=1.0/(va+vb+vc);
 u:=va*w;
 v:=vb*w;
 w:=(1.0-u)-v;

 result:=sqr(((a.x*u)+(b.x*v)+(c.x*w))-p.x)+sqr(((a.y*u)+(b.y*v)+(c.y*w))-p.y)+sqr(((a.z*u)+(b.z*v)+(c.z*w))-p.z);

end;

function IsParallel(const a,b:TVector3;const Tolerance:TScalar=1e-5):boolean; {$ifdef CAN_INLINE}inline;{$endif}
var t:TVector3;
begin
 t:=a-(b*(a.Length/b.Length));
 result:=(abs(t.x)<Tolerance) and (abs(t.y)<Tolerance) and (abs(t.z)<Tolerance);
end;

function Vector3ToAnglesLDX(v:TVector3):TVector3;
var Yaw,Pitch:TScalar;
begin
 if (v.x=0.0) and (v.y=0.0) then begin
  Yaw:=0.0;
  if v.z>0.0 then begin
   Pitch:=pi*0.5;
  end else begin
   Pitch:=pi*1.5;
  end;
 end else begin
  if v.x<>0.0 then begin
   Yaw:=arctan2(v.y,v.x);
  end else if v.y>0.0 then begin
   Yaw:=pi*0.5;
  end else begin
   Yaw:=pi;
  end;
  if Yaw<0.0 then begin
   Yaw:=Yaw+(2.0*pi);
  end;
  Pitch:=arctan2(v.z,sqrt(sqr(v.x)+sqr(v.y)));
  if Pitch<0.0 then begin
   Pitch:=Pitch+(2.0*pi);
  end;
 end;
 result.Pitch:=-Pitch;
 result.Yaw:=Yaw;
 result.Roll:=0.0;
end;

procedure AnglesToVector3LDX(const Angles:TVector3;var ForwardVector,RightVector,UpVector:TVector3);
var cp,sp,cy,sy,cr,sr:TScalar;
begin
 cp:=cos(Angles.Pitch);
 sp:=sin(Angles.Pitch);
 cy:=cos(Angles.Yaw);
 sy:=sin(Angles.Yaw);
 cr:=cos(Angles.Roll);
 sr:=sin(Angles.Roll);
 ForwardVector:=TVector3.Create(cp*cy,cp*sy,-sp).Normalize;
 RightVector:=TVector3.Create(((-(sr*sp*cy))-(cr*(-sy))),((-(sr*sp*sy))-(cr*cy)),-(sr*cp)).Normalize;
 UpVector:=TVector3.Create((cr*sp*cy)+((-sr)*(-sy)),(cr*sp*sy)+((-sr)*cy),cr*cp).Normalize;
end;

function UnsignedAngle(const v0,v1:TVector3):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
begin
//result:=ArcCos(v0.Normalize.Dot(v1)));
 result:=ArcTan2(v0.Cross(v1).Length,v0.Dot(v1));
 if IsNaN(result) or IsInfinite(result) or (abs(result)<1e-12) then begin
  result:=0.0;
 end else begin
  result:=ModuloPos(result,pi*2.0);
 end;
end;

function AngleDegClamp(a:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
begin
 a:=ModuloPos(ModuloPos(a+180.0,360.0)+360.0,360.0)-180.0;
 while a<-180.0 do begin
  a:=a+360.0;
 end;
 while a>180.0 do begin
  a:=a-360.0;
 end;
 result:=a;
end;

function AngleDegDiff(a,b:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=AngleDegClamp(AngleDegClamp(b)-AngleDegClamp(a));
end;

function AngleClamp(a:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
begin
 a:=ModuloPos(ModuloPos(a+pi,pi*2.0)+(pi*2.0),pi*2.0)-pi;
 while a<(-pi) do begin
  a:=a+(pi*2.0);
 end;
 while a>pi do begin
  a:=a-(pi*2.0);
 end;
 result:=a;
end;

function AngleDiff(a,b:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=AngleClamp(AngleClamp(b)-AngleClamp(a));
end;

function AngleLerp(a,b,x:TScalar):TScalar; {$ifdef CAN_INLINE}inline;{$endif}
begin
{if (b-a)>pi then begin
  b:=b-(pi*2);
 end;
 if (b-a)<(-pi) then begin
  b:=b+(pi*2);
 end;
 result:=a+((b-a)*x);}
 result:=a+(AngleDiff(a,b)*x);
end;

function InertiaTensorTransform(const Inertia,Transform:TMatrix3x3):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=(Transform*Inertia)*Transform.Transpose;
end;

function InertiaTensorParallelAxisTheorem(const Center:TVector3;const Mass:TScalar):TMatrix3x3; {$ifdef CAN_INLINE}inline;{$endif}
var CenterDotCenter:TScalar;
begin
 CenterDotCenter:=sqr(Center.x)+sqr(Center.y)+sqr(Center.z);
 result[0,0]:=((Matrix3x3Identity[0,0]*CenterDotCenter)-(Center.x*Center.x))*Mass;
 result[0,1]:=((Matrix3x3Identity[0,1]*CenterDotCenter)-(Center.y*Center.x))*Mass;
 result[0,2]:=((Matrix3x3Identity[0,2]*CenterDotCenter)-(Center.z*Center.x))*Mass;
 result[1,0]:=((Matrix3x3Identity[1,0]*CenterDotCenter)-(Center.x*Center.y))*Mass;
 result[1,1]:=((Matrix3x3Identity[1,1]*CenterDotCenter)-(Center.y*Center.y))*Mass;
 result[1,2]:=((Matrix3x3Identity[1,2]*CenterDotCenter)-(Center.z*Center.y))*Mass;
 result[2,0]:=((Matrix3x3Identity[2,0]*CenterDotCenter)-(Center.x*Center.z))*Mass;
 result[2,1]:=((Matrix3x3Identity[2,1]*CenterDotCenter)-(Center.y*Center.z))*Mass;
 result[2,2]:=((Matrix3x3Identity[2,2]*CenterDotCenter)-(Center.z*Center.z))*Mass;
end;

procedure OrthoNormalize(var Tangent,Bitangent,Normal:TVector3);
begin
 Normal:=Normal.Normalize;
 Tangent:=(Tangent-(Normal*Tangent.Dot(Normal))).Normalize;
 Bitangent:=Normal.Cross(Tangent).Normalize;
 Bitangent:=Bitangent-(Normal*Bitangent.Dot(Normal));
 Bitangent:=(Bitangent-(Tangent*Bitangent.Dot(Tangent))).Normalize;
 Tangent:=Bitangent.Cross(Normal).Normalize;
 Normal:=Tangent.Cross(Bitangent).Normalize;
end;

procedure RobustOrthoNormalize(var Tangent,Bitangent,Normal:TVector3;const Tolerance:TScalar=1e-3);
var Bisector,Axis:TVector3;
begin
 begin
  if Normal.Length<Tolerance then begin
   // Degenerate case, compute new normal
   Normal:=Tangent.Cross(Bitangent);
   if Normal.Length<Tolerance then begin
    Tangent:=Vector3XAxis;
    Bitangent:=Vector3YAxis;
    Normal:=Vector3ZAxis;
    exit;
   end;
  end;
  Normal:=Normal.Normalize;
 end;
 begin
  // Project tangent and bitangent onto the normal orthogonal plane
  Tangent:=Tangent-(Normal*Tangent.Dot(Normal));
  Bitangent:=Bitangent-(Normal*Bitangent.Dot(Normal));
 end;
 begin
  // Check for several degenerate cases
  if Tangent.Length<Tolerance then begin
   if Bitangent.Length<Tolerance then begin
    Tangent:=Normal.Normalize;
    if (Tangent.x<=Tangent.y) and (Tangent.x<=Tangent.z) then begin
     Tangent:=Vector3XAxis;
    end else if (Tangent.y<=Tangent.x) and (Tangent.y<=Tangent.z) then begin
     Tangent:=Vector3YAxis;
    end else begin
     Tangent:=Vector3ZAxis;
    end;
    Tangent:=Tangent-(Normal*Tangent.Dot(Normal));
    Bitangent:=Normal.Cross(Tangent).Normalize;
   end else begin
    Tangent:=Bitangent.Cross(Normal).Normalize;
   end;
  end else begin
   Tangent:=Tangent.Normalize;
   if Bitangent.Length<Tolerance then begin
    Bitangent:=Normal.Cross(Tangent).Normalize;
   end else begin
    Bitangent:=Bitangent.Normalize;
    Bisector:=Tangent+Bitangent;
    if Bisector.Length<Tolerance then begin
     Bisector:=Tangent;
    end else begin
     Bisector:=Bisector.Normalize;
    end;
    Axis:=Bisector.Cross(Normal).Normalize;
    if Axis.Dot(Tangent)>0.0 then begin
     Tangent:=(Bisector+Axis).Normalize;
     Bitangent:=(Bisector-Axis).Normalize;
    end else begin
     Tangent:=(Bisector-Axis).Normalize;
     Bitangent:=(Bisector+Axis).Normalize;
    end;
   end;
  end;
 end;
 Bitangent:=Normal.Cross(Tangent).Normalize;
 Tangent:=Bitangent.Cross(Normal).Normalize;
 Normal:=Tangent.Cross(Bitangent).Normalize;
end;

function MaxOverlaps(const Min1,Max1,Min2,Max2:TScalar;var LowerLim,UpperLim:TScalar):boolean;
begin
 if (Max1<Min2) or (Max2<Min1) then begin
  result:=false;
 end else begin
  if (Min2<=Min1) and (Min1<=Max2) then begin
   if (Min1<=Max2) and (Max2<=Max1) then begin
    LowerLim:=Min1;
    UpperLim:=Max1;
   end else if (Max1-Min2)<(Max2-Min1) then begin
    LowerLim:=Min2;
    UpperLim:=Max1;
   end else begin
    LowerLim:=Min1;
    UpperLim:=Max2;
   end;
  end else begin
   if (Min1<=Max2) and (Max2<=Max1) then begin
    LowerLim:=Min2;
    UpperLim:=Max1;
   end else if (Max2-Min1)<(Max1-Min2) then begin
    LowerLim:=Min1;
    UpperLim:=Max2;
   end else begin
    LowerLim:=Min2;
    UpperLim:=Max1;
   end;
  end;
  result:=true;
 end;
end;

function ConvertRGB32FToRGB9E5(r,g,b:TFloat):TUInt32;
const RGB9E5_EXPONENT_BITS=5;
      RGB9E5_MANTISSA_BITS=9;
      RGB9E5_EXP_BIAS=15;
      RGB9E5_MAX_VALID_BIASED_EXP=31;
      MAX_RGB9E5_EXP=RGB9E5_MAX_VALID_BIASED_EXP-RGB9E5_EXP_BIAS;
      RGB9E5_MANTISSA_VALUES=1 shl RGB9E5_MANTISSA_BITS;
      MAX_RGB9E5_MANTISSA=RGB9E5_MANTISSA_VALUES-1;
      MAX_RGB9E5=((MAX_RGB9E5_MANTISSA+0.0)/RGB9E5_MANTISSA_VALUES)*(1 shl MAX_RGB9E5_EXP);
      EPSILON_RGB9E5=(1.0/RGB9E5_MANTISSA_VALUES)/(1 shl RGB9E5_EXP_BIAS);
var Exponent,MaxMantissa,ri,gi,bi:TInt32;
    MaxComponent,Denominator:TFloat;
    CastedMaxComponent:TUInt32 absolute MaxComponent;
begin
 if r>0.0 then begin
  if r>MAX_RGB9E5 then begin
   r:=MAX_RGB9E5;
  end;
 end else begin
  r:=0.0;
 end;
 if g>0.0 then begin
  if g>MAX_RGB9E5 then begin
   g:=MAX_RGB9E5;
  end;
 end else begin
  g:=0.0;
 end;
 if b>0.0 then begin
  if b>MAX_RGB9E5 then begin
   b:=MAX_RGB9E5;
  end;
 end else begin
  b:=0.0;
 end;
 if r<g then begin
  if g<b then begin
   MaxComponent:=b;
  end else begin
   MaxComponent:=g;
  end;
 end else begin
  if r<b then begin
   MaxComponent:=b;
  end else begin
   MaxComponent:=r;
  end;
 end;
 Exponent:=(TInt32(CastedMaxComponent and $7f800000) shr 23)-127;
 if Exponent<((-RGB9E5_EXP_BIAS)-1) then begin
  Exponent:=((-RGB9E5_EXP_BIAS)-1);
 end;
 inc(Exponent,RGB9E5_EXP_BIAS+1);
 if Exponent<0 then begin
  Exponent:=0;
 end else if Exponent>RGB9E5_MAX_VALID_BIASED_EXP then begin
  Exponent:=RGB9E5_MAX_VALID_BIASED_EXP;
 end;
 Denominator:=power(2.0,Exponent-(RGB9E5_EXP_BIAS+RGB9E5_MANTISSA_BITS));
 MaxMantissa:=trunc(floor((MaxComponent/Denominator)+0.5));
 if MaxMantissa=(MAX_RGB9E5_MANTISSA+1) then begin
  Denominator:=Denominator*2.0;
  inc(Exponent);
  Assert(Exponent<=RGB9E5_MAX_VALID_BIASED_EXP);
 end else begin
  Assert(Exponent<=MAX_RGB9E5_MANTISSA);
 end;
 ri:=trunc(floor((r/Denominator))+0.5);
 gi:=trunc(floor((g/Denominator))+0.5);
 bi:=trunc(floor((b/Denominator))+0.5);
 if ri<0 then begin
  ri:=0;
 end else if ri>MAX_RGB9E5_MANTISSA then begin
  ri:=MAX_RGB9E5_MANTISSA;
 end;
 if gi<0 then begin
  gi:=0;
 end else if gi>MAX_RGB9E5_MANTISSA then begin
  gi:=MAX_RGB9E5_MANTISSA;
 end;
 if bi<0 then begin
  bi:=0;
 end else if bi>MAX_RGB9E5_MANTISSA then begin
  bi:=MAX_RGB9E5_MANTISSA;
 end;
 result:=TUInt32(ri) or (TUInt32(gi) shl 9) or (TUInt32(bi) shl 18) or (TUInt32(Exponent and 31) shl 27);
end;

function PackFP32FloatToM6E5Float(const pValue:TFloat):TUInt32;
const Float32MantissaBits=23;
      Float32ExponentBits=8;
      Float32Bits=32;
      Float32ExponentBias=127;
      Float6E5MantissaBits=6;
      Float6E5MantissaMask=(1 shl Float6E5MantissaBits)-1;
      Float6E5ExponentBits=5;
      Float6E5Bits=11;
      Float6E5ExponentBias=15;
var CastedValue:TUInt32 absolute pValue;
    Exponent,Mantissa:TUInt32;
begin

 // Extract the exponent and the mantissa from the 32-bit floating point value
 Exponent:=(CastedValue and $7f800000) shr Float32MantissaBits;
 Mantissa:=(CastedValue shr (Float32MantissaBits-Float6E5MantissaBits)) and Float6E5MantissaMask;

 // Round mantissa
 if (CastedValue and (1 shl ((Float32MantissaBits-Float6E5MantissaBits)-1)))<>0 then begin
  inc(Mantissa);
  if (Mantissa and (1 shl Float6E5MantissaBits))<>0 then begin
   Mantissa:=0;
   inc(Exponent);
  end;
 end;

 if Exponent<=(Float32ExponentBias-Float6E5ExponentBias) then begin
  // Denormal
  if Exponent<((Float32ExponentBias-Float6E5ExponentBias)-Float6E5MantissaBits) then begin
   result:=0;
  end else begin
   result:=(Mantissa or (1 shl Float6E5MantissaBits)) shr (((Float32ExponentBias-Float6E5ExponentBias)+1)-Exponent);
  end;
 end else if Exponent>(Float32ExponentBias+Float6E5ExponentBias) then begin
  // |x| > 2^15, overflow, an existing INF, or NaN
  if Exponent=((1 shl Float32ExponentBits)-1) then begin
   if Mantissa<>0 then begin
    // Return allows -NaN to return as NaN even if there is no sign bit.
    result:=((1 shl (Float6E5ExponentBits+Float6E5MantissaBits))-1) or ((CastedValue shr (Float32Bits-Float6E5Bits)) and (1 shl (Float32MantissaBits+Float32ExponentBits)));
    exit;
   end else begin
    result:=((1 shl Float6E5ExponentBits)-1) shl Float6E5MantissaBits;
   end;
  end else begin
   result:=((((1 shl Float6E5ExponentBits)-1) shl Float6E5MantissaBits)-(1 shl Float6E5MantissaBits)) or Float6E5MantissaMask;
  end;
 end else begin
  result:=((Exponent-(Float32ExponentBias-Float6E5ExponentBias)) shl Float6E5MantissaBits) or Mantissa;
 end;

 if (CastedValue and (1 shl (Float32MantissaBits+Float32ExponentBits)))<>0 then begin
  // Clamp negative value
  result:=0;
 end;
end;

function PackFP32FloatToM5E5Float(const pValue:TFloat):TUInt32;
const Float32MantissaBits=23;
      Float32ExponentBits=8;
      Float32Bits=32;
      Float32ExponentBias=127;
      Float5E5MantissaBits=5;
      Float5E5MantissaMask=(1 shl Float5E5MantissaBits)-1;
      Float5E5ExponentBits=5;
      Float5E5Bits=10;
      Float5E5ExponentBias=15;
var CastedValue:TUInt32 absolute pValue;
    Exponent,Mantissa:TUInt32;
begin

 // Extract the exponent and the mantissa from the 32-bit floating point value
 Exponent:=(CastedValue and $7f800000) shr Float32MantissaBits;
 Mantissa:=(CastedValue shr (Float32MantissaBits-Float5E5MantissaBits)) and Float5E5MantissaMask;

 // Round mantissa
 if (CastedValue and (1 shl ((Float32MantissaBits-Float5E5MantissaBits)-1)))<>0 then begin
  inc(Mantissa);
  if (Mantissa and (1 shl Float5E5MantissaBits))<>0 then begin
   Mantissa:=0;
   inc(Exponent);
  end;
 end;

 if Exponent<=(Float32ExponentBias-Float5E5ExponentBias) then begin
  // Denormal
  if Exponent<((Float32ExponentBias-Float5E5ExponentBias)-Float5E5MantissaBits) then begin
   result:=0;
  end else begin
   result:=(Mantissa or (1 shl Float5E5MantissaBits)) shr (((Float32ExponentBias-Float5E5ExponentBias)+1)-Exponent);
  end;
 end else if Exponent>(Float32ExponentBias+Float5E5ExponentBias) then begin
  // |x| > 2^15, overflow, an existing INF, or NaN
  if Exponent=((1 shl Float32ExponentBits)-1) then begin
   if Mantissa<>0 then begin
    // Return allows -NaN to return as NaN even if there is no sign bit.
    result:=((1 shl (Float5E5ExponentBits+Float5E5MantissaBits))-1) or ((CastedValue shr (Float32Bits-Float5E5Bits)) and (1 shl (Float32MantissaBits+Float32ExponentBits)));
    exit;
   end else begin
    result:=((1 shl Float5E5ExponentBits)-1) shl Float5E5MantissaBits;
   end;
  end else begin
   result:=((((1 shl Float5E5ExponentBits)-1) shl Float5E5MantissaBits)-(1 shl Float5E5MantissaBits)) or Float5E5MantissaMask;
  end;
 end else begin
  result:=((Exponent-(Float32ExponentBias-Float5E5ExponentBias)) shl Float5E5MantissaBits) or Mantissa;
 end;

 if (CastedValue and (1 shl (Float32MantissaBits+Float32ExponentBits)))<>0 then begin
  // Clamp negative value
  result:=0;
 end;
end;

function Float32ToFloat11(const pValue:TFloat):TUInt32;
const EXPONENT_BIAS=15;
      EXPONENT_BITS=$1f;
      EXPONENT_SHIFT=6;
      MANTISSA_BITS=$3f;
      MANTISSA_SHIFT=23-EXPONENT_SHIFT;
      MAX_EXPONENT=EXPONENT_BITS shl EXPONENT_SHIFT;
var CastedValue:TUInt32 absolute pValue;
    Sign:TUInt32;
    Exponent,Mantissa:TInt32;
begin
 Sign:=CastedValue shr 31;
 Exponent:=TInt32(TUInt32((CastedValue and $7f800000) shr 23))-127;
 Mantissa:=CastedValue and $007fffff;
 if Exponent=128 then begin
  // Infinity or NaN
  (* From the GL_EXT_packed_float spec:
   *     "Additionally: negative infinity is converted to zero; positive
   *      infinity is converted to positive infinity; and both positive and
   *      negative NaN are converted to positive NaN."
   *)
  if Mantissa<>0 then begin
   result:=MAX_EXPONENT or 1; // NaN
  end else begin
   if Sign<>0 then begin
    result:=0; // 0.0
   end else begin
    result:=MAX_EXPONENT; // Infinity
   end;
  end;
 end else if Sign<>0 then begin
  result:=0;
 end else if pValue>65024.0 then begin
  (* From the GL_EXT_packed_float spec:
   *     "Likewise, finite positive values greater than 65024 (the maximum
   *      finite representable unsigned 11-bit floating-point value) are
   *      converted to 65024."
   *)
  result:=(30 shl EXPONENT_SHIFT) or 63;
 end else if Exponent>-15 then begin
  result:=((Exponent+EXPONENT_BIAS) shl EXPONENT_SHIFT) or (Mantissa shr MANTISSA_SHIFT);
 end else begin
  result:=0;
 end;
end;

function Float32ToFloat10(const pValue:TFloat):TUInt32;
const EXPONENT_BIAS=15;
      EXPONENT_BITS=$1f;
      EXPONENT_SHIFT=5;
      MANTISSA_BITS=$1f;
      MANTISSA_SHIFT=23-EXPONENT_SHIFT;
      MAX_EXPONENT=EXPONENT_BITS shl EXPONENT_SHIFT;
var CastedValue:TUInt32 absolute pValue;
    Sign:TUInt32;
    Exponent,Mantissa:TInt32;
begin
 Sign:=CastedValue shr 31;
 Exponent:=TInt32(TUInt32((CastedValue and $7f800000) shr 23))-127;
 Mantissa:=CastedValue and $007fffff;
 if Exponent=128 then begin
  // Infinity or NaN
  (* From the GL_EXT_packed_float spec:
   *     "Additionally: negative infinity is converted to zero; positive
   *      infinity is converted to positive infinity; and both positive and
   *      negative NaN are converted to positive NaN."
   *)
  if Mantissa<>0 then begin
   result:=MAX_EXPONENT or 1; // NaN
  end else begin
   if Sign<>0 then begin
    result:=0; // 0.0
   end else begin
    result:=MAX_EXPONENT; // Infinity
   end;
  end;
 end else if Sign<>0 then begin
  result:=0;
 end else if pValue>64512.0 then begin
  (* From the GL_EXT_packed_float spec:
   *     "Likewise, finite positive values greater than 64512 (the maximum
   *      finite representable unsigned 11-bit floating-point value) are
   *      converted to 64512."
   *)
  result:=(30 shl EXPONENT_SHIFT) or 31;
 end else if Exponent>-15 then begin
  result:=((Exponent+EXPONENT_BIAS) shl EXPONENT_SHIFT) or (Mantissa shr MANTISSA_SHIFT);
 end else begin
  result:=0;
 end;
end;

function ConvertRGB32FToR11FG11FB10F(const r,g,b:TFloat):TUInt32; {$ifdef CAN_INLINE}inline;{$endif}
begin
//result:=(PackFP32FloatToM6E5Float(r) and $7ff) or ((PackFP32FloatToM6E5Float(g) and $7ff) shl 11) or ((PackFP32FloatToM6E5Float(b) and $3ff) shl 22);
 result:=(Float32ToFloat11(r) and $7ff) or ((Float32ToFloat11(g) and $7ff) shl 11) or ((Float32ToFloat10(b) and $3ff) shl 22);
end;

function PackTangentSpace(const Tangent,Bitangent,Normal:TVector3):TPackedTangentSpace;
var q:TQuaternion;
begin
 q:=TMatrix3x3.Create(Tangent,Bitangent,Normal).ToQTangent;
 result.x:=Min(Max((round(q.x*127)+128),0),255);
 result.y:=Min(Max((round(q.y*127)+128),0),255);
 result.z:=Min(Max((round(q.z*127)+128),0),255);
 result.w:=Min(Max((round(q.w*127)+128),0),255);
end;
{
begin
result.x:=Min(Max((round((ArcSin(Normal.z)/pi)*127)+128),0),255);
 result.y:=Min(Max((round((ArcTan2(Normal.y,Normal.x)/pi)*127)+128),0),255);
 result.z:=Min(Max((round((ArcSin(Tangent.z)/pi)*127)+128),0),255);
 result.w:=Min(Max((round((ArcTan2(Tangent.y,Tangent.x)/pi)*127)+128),0),255);
end;{}

procedure UnpackTangentSpace(var PackedTangentSpace:TPackedTangentSpace;var Tangent,Bitangent,Normal:TVector3);
var q:TQuaternion;
    m:TMatrix3x3;
begin
 q.x:=(PackedTangentSpace.x-128)/127;
 q.y:=(PackedTangentSpace.y-128)/127;
 q.z:=(PackedTangentSpace.z-128)/127;
 q.w:=(PackedTangentSpace.w-128)/127;
 m:=TMatrix3x3.CreateFromQTangent(q);
 Tangent.x:=m[0,0];
 Tangent.y:=m[0,1];
 Tangent.z:=m[0,2];
 Bitangent.x:=m[1,0];
 Bitangent.y:=m[1,1];
 Bitangent.z:=m[1,2];
 Normal.x:=m[2,0];
 Normal.y:=m[2,1];
 Normal.z:=m[2,2];
end;
{var Latitude,Longitude:single;
begin
 Latitude:=((PackedTangentSpace.x-128)/127)*pi;
 Longitude:=((PackedTangentSpace.y-128)/127)*pi;
 Normal.x:=cos(Latitude)*cos(Longitude);
 Normal.y:=cos(Latitude)*sin(Longitude);
 Normal.z:=sin(Latitude);
 Latitude:=((PackedTangentSpace.z-128)/127)*pi;
 Longitude:=((PackedTangentSpace.w-128)/127)*pi;
 Tangent.x:=cos(Latitude)*cos(Longitude);
 Tangent.y:=cos(Latitude)*sin(Longitude);
 Tangent.z:=sin(Latitude);
 Bitangent:=Vector3Norm(Vector3Cross(Normal,Tangent));
end;{}

constructor TPropertyVector2.Create(AVector:PVector2);
begin
 inherited Create;
 fVector:=AVector;
end;

destructor TPropertyVector2.Destroy;
begin
 inherited Destroy;
end;

function TPropertyVector2.GetX:TScalar;
begin
 result:=fVector^.x;
end;

function TPropertyVector2.GetY:TScalar;
begin
 result:=fVector^.y;
end;

function TPropertyVector2.GetVector:TVector2;
begin
 result:=fVector^;
end;

procedure TPropertyVector2.SetX(const pNewValue:TScalar);
begin
 fVector^.x:=pNewValue;
end;

procedure TPropertyVector2.SetY(const pNewValue:TScalar);
begin
 fVector^.y:=pNewValue;
end;

procedure TPropertyVector2.SetVector(const pNewVector:TVector2);
begin
 fVector^:=pNewVector;
end;

constructor TPropertyVector3.Create(AVector:PVector3);
begin
 inherited Create;
 fVector:=AVector;
end;

destructor TPropertyVector3.Destroy;
begin
 inherited Destroy;
end;

function TPropertyVector3.GetX:TScalar;
begin
 result:=fVector^.x;
end;

function TPropertyVector3.GetY:TScalar;
begin
 result:=fVector^.y;
end;

function TPropertyVector3.GetZ:TScalar;
begin
 result:=fVector^.z;
end;

function TPropertyVector3.GetVector:TVector3;
begin
 result:=fVector^;
end;

procedure TPropertyVector3.SetX(const pNewValue:TScalar);
begin
 fVector^.x:=pNewValue;
end;

procedure TPropertyVector3.SetY(const pNewValue:TScalar);
begin
 fVector^.y:=pNewValue;
end;

procedure TPropertyVector3.SetZ(const pNewValue:TScalar);
begin
 fVector^.z:=pNewValue;
end;

procedure TPropertyVector3.SetVector(const pNewVector:TVector3);
begin
 fVector^:=pNewVector;
end;

constructor TPropertyVector4.Create(AVector:PVector4);
begin
 inherited Create;
 fVector:=AVector;
end;

destructor TPropertyVector4.Destroy;
begin
 inherited Destroy;
end;

function TPropertyVector4.GetX:TScalar;
begin
 result:=fVector^.x;
end;

function TPropertyVector4.GetY:TScalar;
begin
 result:=fVector^.y;
end;

function TPropertyVector4.GetZ:TScalar;
begin
 result:=fVector^.z;
end;

function TPropertyVector4.GetW:TScalar;
begin
 result:=fVector^.w;
end;

function TPropertyVector4.GetVector:TVector4;
begin
 result:=fVector^;
end;

procedure TPropertyVector4.SetX(const pNewValue:TScalar);
begin
 fVector^.x:=pNewValue;
end;

procedure TPropertyVector4.SetY(const pNewValue:TScalar);
begin
 fVector^.y:=pNewValue;
end;

procedure TPropertyVector4.SetZ(const pNewValue:TScalar);
begin
 fVector^.z:=pNewValue;
end;

procedure TPropertyVector4.SetW(const pNewValue:TScalar);
begin
 fVector^.w:=pNewValue;
end;

procedure TPropertyVector4.SetVector(const pNewVector:TVector4);
begin
 fVector^:=pNewVector;
end;

constructor TPropertyQuaternion.Create(AQuaternion:PQuaternion);
begin
 inherited Create;
 fQuaternion:=AQuaternion;
end;

destructor TPropertyQuaternion.Destroy;
begin
 inherited Destroy;
end;

function TPropertyQuaternion.GetX:TScalar;
begin
 result:=fQuaternion^.x;
end;

function TPropertyQuaternion.GetY:TScalar;
begin
 result:=fQuaternion^.y;
end;

function TPropertyQuaternion.GetZ:TScalar;
begin
 result:=fQuaternion^.z;
end;

function TPropertyQuaternion.GetW:TScalar;
begin
 result:=fQuaternion^.w;
end;

function TPropertyQuaternion.GetQuaternion:TQuaternion;
begin
 result:=fQuaternion^;
end;

procedure TPropertyQuaternion.SetX(const pNewValue:TScalar);
begin
 fQuaternion^.x:=pNewValue;
end;

procedure TPropertyQuaternion.SetY(const pNewValue:TScalar);
begin
 fQuaternion^.y:=pNewValue;
end;

procedure TPropertyQuaternion.SetZ(const pNewValue:TScalar);
begin
 fQuaternion^.z:=pNewValue;
end;

procedure TPropertyQuaternion.SetW(const pNewValue:TScalar);
begin
 fQuaternion^.w:=pNewValue;
end;

procedure TPropertyQuaternion.SetQuaternion(const NewQuaternion:TQuaternion);
begin
 fQuaternion^:=NewQuaternion;
end;

constructor TPropertyAngle.Create(ARadianAngle:PScalar);
begin
 inherited Create;
 fRadianAngle:=ARadianAngle;
end;

destructor TPropertyAngle.Destroy;
begin
 inherited Destroy;
end;

function TPropertyAngle.GetAngle:TScalar;
begin
 result:=fRadianAngle^*RAD2DEG;
end;

function TPropertyAngle.GetRadianAngle:TScalar;
begin
 result:=fRadianAngle^;
end;

procedure TPropertyAngle.SetAngle(const pNewValue:TScalar);
begin
 fRadianAngle^:=pNewValue*DEG2RAD;
end;

procedure TPropertyAngle.SetRadianAngle(const pNewValue:TScalar);
begin
 fRadianAngle^:=pNewValue;
end;

constructor TPropertyRotation3D.Create(AQuaternion:PQuaternion);
begin
 inherited Create;
 fQuaternion:=AQuaternion;
end;

destructor TPropertyRotation3D.Destroy;
begin
 inherited Destroy;
end;

function TPropertyRotation3D.GetX:TScalar;
begin
 result:=fQuaternion^.x;
end;

function TPropertyRotation3D.GetY:TScalar;
begin
 result:=fQuaternion^.y;
end;

function TPropertyRotation3D.GetZ:TScalar;
begin
 result:=fQuaternion^.z;
end;

function TPropertyRotation3D.GetW:TScalar;
begin
 result:=fQuaternion^.w;
end;

function TPropertyRotation3D.GetPitch:TScalar;
begin
 result:=fQuaternion^.Normalize.ToEuler.Pitch*RAD2DEG;
end;

function TPropertyRotation3D.GetYaw:TScalar;
begin
 result:=fQuaternion^.Normalize.ToEuler.Yaw*RAD2DEG;
end;

function TPropertyRotation3D.GetRoll:TScalar;
begin
 result:=fQuaternion^.Normalize.ToEuler.Roll*RAD2DEG;
end;

function TPropertyRotation3D.GetQuaternion:TQuaternion;
begin
 result:=fQuaternion^;
end;

procedure TPropertyRotation3D.SetX(const pNewValue:TScalar);
begin
 fQuaternion^.x:=pNewValue;
end;

procedure TPropertyRotation3D.SetY(const pNewValue:TScalar);
begin
 fQuaternion^.y:=pNewValue;
end;

procedure TPropertyRotation3D.SetZ(const pNewValue:TScalar);
begin
 fQuaternion^.z:=pNewValue;
end;

procedure TPropertyRotation3D.SetW(const pNewValue:TScalar);
begin
 fQuaternion^.w:=pNewValue;
end;

procedure TPropertyRotation3D.SetPitch(const pNewValue:TScalar);
var Angles:TVector3;
begin
 Angles:=fQuaternion^.Normalize.ToEuler;
 Angles.Pitch:=pNewValue*DEG2RAD;
 fQuaternion^:=TQuaternion.CreateFromEuler(Angles);
end;

procedure TPropertyRotation3D.SetYaw(const pNewValue:TScalar);
var Angles:TVector3;
begin
 Angles:=fQuaternion^.Normalize.ToEuler;
 Angles.Yaw:=pNewValue*DEG2RAD;
 fQuaternion^:=TQuaternion.CreateFromEuler(Angles);
end;

procedure TPropertyRotation3D.SetRoll(const pNewValue:TScalar);
var Angles:TVector3;
begin
 Angles:=fQuaternion^.Normalize.ToEuler;
 Angles.Roll:=pNewValue*DEG2RAD;
 fQuaternion^:=TQuaternion.CreateFromEuler(Angles);
end;

procedure TPropertyRotation3D.SetQuaternion(const NewQuaternion:TQuaternion);
begin
 fQuaternion^:=NewQuaternion;
end;

constructor TPropertyColorRGB.Create(AVector:PVector3);
begin
 inherited Create;
 fVector:=AVector;
end;

destructor TPropertyColorRGB.Destroy;
begin
 inherited Destroy;
end;

function TPropertyColorRGB.GetR:TScalar;
begin
 result:=fVector^.r;
end;

function TPropertyColorRGB.GetG:TScalar;
begin
 result:=fVector^.g;
end;

function TPropertyColorRGB.GetB:TScalar;
begin
 result:=fVector^.b;
end;

function TPropertyColorRGB.GetVector:TVector3;
begin
 result:=fVector^;
end;

procedure TPropertyColorRGB.SetR(const pNewValue:TScalar);
begin
 fVector^.r:=pNewValue;
end;

procedure TPropertyColorRGB.SetG(const pNewValue:TScalar);
begin
 fVector^.g:=pNewValue;
end;

procedure TPropertyColorRGB.SetB(const pNewValue:TScalar);
begin
 fVector^.b:=pNewValue;
end;

procedure TPropertyColorRGB.SetVector(const pNewVector:TVector3);
begin
 fVector^:=pNewVector;
end;

constructor TPropertyColorRGBA.Create(AVector:PVector4);
begin
 inherited Create;
 fVector:=AVector;
end;

destructor TPropertyColorRGBA.Destroy;
begin
 inherited Destroy;
end;

function TPropertyColorRGBA.GetR:TScalar;
begin
 result:=fVector^.r;
end;

function TPropertyColorRGBA.GetG:TScalar;
begin
 result:=fVector^.g;
end;

function TPropertyColorRGBA.GetB:TScalar;
begin
 result:=fVector^.b;
end;

function TPropertyColorRGBA.GetA:TScalar;
begin
 result:=fVector^.a;
end;

function TPropertyColorRGBA.GetVector:TVector4;
begin
 result:=fVector^;
end;

procedure TPropertyColorRGBA.SetR(const pNewValue:TScalar);
begin
 fVector^.r:=pNewValue;
end;

procedure TPropertyColorRGBA.SetG(const pNewValue:TScalar);
begin
 fVector^.g:=pNewValue;
end;

procedure TPropertyColorRGBA.SetB(const pNewValue:TScalar);
begin
 fVector^.b:=pNewValue;
end;

procedure TPropertyColorRGBA.SetA(const pNewValue:TScalar);
begin
 fVector^.a:=pNewValue;
end;

procedure TPropertyColorRGBA.SetVector(const pNewVector:TVector4);
begin
 fVector^:=pNewVector;
end;

initialization
end.
