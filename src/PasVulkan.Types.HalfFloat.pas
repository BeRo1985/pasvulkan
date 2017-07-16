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
 * 5. Write code which's compatible with Delphi >= 2009 and FreePascal >= 3.0 *
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
unit PasVulkan.Types.HalfFloat;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

uses PasVulkan.Types.Standard;

type PPpvHalfFloat=^PpvHalfFloat;
     PpvHalfFloat=^TpvHalfFloat;
     TpvHalfFloat=record
      public
       Value:TpvUInt16;
       constructor Create(const pValue:TpvFloat);
       class function FromFloat(const pValue:TpvFloat):TpvHalfFloat; static; {$ifdef CAN_INLINE}inline;{$endif}
       function ToFloat:TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Implicit(const a:TpvFloat):TpvHalfFloat; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Implicit(const a:TpvHalfFloat):TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Explicit(const a:TpvFloat):TpvHalfFloat; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Explicit(const a:TpvHalfFloat):TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Equal(const a,b:TpvHalfFloat):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Equal(const a:TpvHalfFloat;const b:TpvFloat):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Equal(const a:TpvFloat;const b:TpvHalfFloat):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator NotEqual(const a,b:TpvHalfFloat):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator NotEqual(const a:TpvHalfFloat;const b:TpvFloat):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator NotEqual(const a:TpvFloat;const b:TpvHalfFloat):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator GreaterThan(const a,b:TpvHalfFloat):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator GreaterThan(const a:TpvHalfFloat;const b:TpvFloat):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator GreaterThan(const a:TpvFloat;const b:TpvHalfFloat):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator GreaterThanOrEqual(const a,b:TpvHalfFloat):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator GreaterThanOrEqual(const a:TpvHalfFloat;const b:TpvFloat):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator GreaterThanOrEqual(const a:TpvFloat;const b:TpvHalfFloat):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator LessThan(const a,b:TpvHalfFloat):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator LessThan(const a:TpvHalfFloat;const b:TpvFloat):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator LessThan(const a:TpvFloat;const b:TpvHalfFloat):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator LessThanOrEqual(const a,b:TpvHalfFloat):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator LessThanOrEqual(const a:TpvHalfFloat;const b:TpvFloat):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator LessThanOrEqual(const a:TpvFloat;const b:TpvHalfFloat):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Inc(const a:TpvHalfFloat):TpvHalfFloat; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Dec(const a:TpvHalfFloat):TpvHalfFloat; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Add(const a,b:TpvHalfFloat):TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Add(const a:TpvHalfFloat;const b:TpvFloat):TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Add(const a:TpvFloat;const b:TpvHalfFloat):TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Subtract(const a,b:TpvHalfFloat):TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Subtract(const a:TpvHalfFloat;const b:TpvFloat):TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Subtract(const a:TpvFloat;const b:TpvHalfFloat):TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a,b:TpvHalfFloat):TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a:TpvHalfFloat;const b:TpvFloat):TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Multiply(const a:TpvFloat;const b:TpvHalfFloat):TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Divide(const a,b:TpvHalfFloat):TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Divide(const a:TpvHalfFloat;const b:TpvFloat):TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Divide(const a:TpvFloat;const b:TpvHalfFloat):TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Modulus(const a,b:TpvHalfFloat):TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Negative(const a:TpvHalfFloat):TpvHalfFloat; {$ifdef CAN_INLINE}inline;{$endif}
       class operator Positive(const a:TpvHalfFloat):TpvHalfFloat; {$ifdef CAN_INLINE}inline;{$endif}
       function Abs:TpvHalfFloat; {$ifdef CAN_INLINE}inline;{$endif}
       function IsNaN:boolean; {$ifdef CAN_INLINE}inline;{$endif}
       function IsInfinity:boolean; {$ifdef CAN_INLINE}inline;{$endif}
       function IsNegativeInfinity:boolean; {$ifdef CAN_INLINE}inline;{$endif}
       function IsPositiveInfinity:boolean; {$ifdef CAN_INLINE}inline;{$endif}
     end;

var FloatToHalfFloatBaseTable:array[0..511] of TpvUInt16;
    FloatToHalfFloatShiftTable:array[0..511] of TpvUInt8;

    HalfFloatToFloatMantissaTable:array[0..2047] of TpvUInt32;
    HalfFloatToFloatExponentTable:array[0..63] of TpvUInt32;
    HalfFloatToFloatOffsetTable:array[0..63] of TpvUInt32;

implementation

uses Math;

procedure GenerateHalfFloatLookUpTables;
var i,e:TpvInt32;
    Mantissa,Exponent:TpvUInt32;
begin
 for i:=0 to 255 do begin
  e:=i-127;
  case e of
   -127..-25:begin
    // Very small numbers maps to zero
    FloatToHalfFloatBaseTable[i or $000]:=$0000;
    FloatToHalfFloatBaseTable[i or $100]:=$8000;
    FloatToHalfFloatShiftTable[i or $000]:=24;
    FloatToHalfFloatShiftTable[i or $100]:=24;
   end;
   -24..-15:begin
    // Small numbers maps to denormals
    FloatToHalfFloatBaseTable[i or $000]:=($0400 shr ((-e)-14)) or $0000;
    FloatToHalfFloatBaseTable[i or $100]:=($0400 shr ((-e)-14)) or $8000;
    FloatToHalfFloatShiftTable[i or $000]:=(-e)-1;
    FloatToHalfFloatShiftTable[i or $100]:=(-e)-1;
   end;
   -14..15:begin
    // Normal numbers just loses precision
    FloatToHalfFloatBaseTable[i or $000]:=((e+15) shl 10) or $0000;
    FloatToHalfFloatBaseTable[i or $100]:=((e+15) shl 10) or $8000;
    FloatToHalfFloatShiftTable[i or $000]:=13;
    FloatToHalfFloatShiftTable[i or $100]:=13;
   end;
   16..127:begin
    // Large numbers maps to infinity
    FloatToHalfFloatBaseTable[i or $000]:=$7c00;
    FloatToHalfFloatBaseTable[i or $100]:=$fc00;
    FloatToHalfFloatShiftTable[i or $000]:=24;
    FloatToHalfFloatShiftTable[i or $100]:=24;
   end;
   else begin
    // Infinity and NaN's stay infinity and NaN's
    FloatToHalfFloatBaseTable[i or $000]:=$7c00;
    FloatToHalfFloatBaseTable[i or $100]:=$fc00;
    FloatToHalfFloatShiftTable[i or $000]:=13;
    FloatToHalfFloatShiftTable[i or $100]:=13;
   end;
  end;
 end;
 begin
  begin
   HalfFloatToFloatMantissaTable[0]:=0;
   for i:=1 to 1023 do begin
    Mantissa:=i shl 13;
    Exponent:=0;
    while (Mantissa and $00800000)=0 do begin // While not normalized
     dec(Exponent,$00800000);                 // Decrement exponent by 1 shl 23
     Mantissa:=Mantissa shl 1;                // Shift mantissa
    end;
    Mantissa:=Mantissa and not $00800000;     // Clear leading 1 bit
    inc(Exponent,$38800000);                  // Adjust bias by (127-14) shl 23
    HalfFloatToFloatMantissaTable[i]:=Mantissa or Exponent;
   end;
   for i:=1024 to 2047 do begin
    HalfFloatToFloatMantissaTable[i]:=TpvUInt32($38000000)+TpvUInt32(TpvUInt32(i-1024) shl 13);
   end;
  end;
  begin
   HalfFloatToFloatExponentTable[0]:=0;
   for i:=1 to 30 do begin
    HalfFloatToFloatExponentTable[i]:=i shl 23;
   end;
   HalfFloatToFloatExponentTable[31]:=$47800000;
   HalfFloatToFloatExponentTable[32]:=0;
   for i:=33 to 62 do begin
    HalfFloatToFloatExponentTable[i]:=TpvUInt32(TpvUInt32(i-32) shl 23) or TpvUInt32($80000000);
   end;
   HalfFloatToFloatExponentTable[63]:=$c7800000;
  end;
  begin
   HalfFloatToFloatOffsetTable[0]:=0;
   for i:=1 to 31 do begin
    HalfFloatToFloatOffsetTable[i]:=1024;
   end;
   HalfFloatToFloatOffsetTable[32]:=0;
   for i:=33 to 63 do begin
    HalfFloatToFloatOffsetTable[i]:=1024;
   end;
  end;
 end;
end;

constructor TpvHalfFloat.Create(const pValue:TpvFloat);
var CastedValue:TpvUInt32 absolute pValue;
begin
 Value:=FloatToHalfFloatBaseTable[CastedValue shr 23]+TpvUInt16((CastedValue and $007fffff) shr FloatToHalfFloatShiftTable[CastedValue shr 23]);
end;

class function TpvHalfFloat.FromFloat(const pValue:TpvFloat):TpvHalfFloat;
var CastedValue:TpvUInt32 absolute pValue;
begin
 result.Value:=FloatToHalfFloatBaseTable[CastedValue shr 23]+TpvUInt16((CastedValue and $007fffff) shr FloatToHalfFloatShiftTable[CastedValue shr 23]);
end;

function TpvHalfFloat.ToFloat:TpvFloat;
var f:TpvUInt32;
begin
 f:=HalfFloatToFloatMantissaTable[HalfFloatToFloatOffsetTable[Value shr 10]+(Value and $3ff)]+HalfFloatToFloatExponentTable[Value shr 10];
 result:=TpvFloat(pointer(@f)^);
end;

class operator TpvHalfFloat.Implicit(const a:TpvFloat):TpvHalfFloat;
begin
 result:=TpvHalfFloat.FromFloat(a);
end;

class operator TpvHalfFloat.Implicit(const a:TpvHalfFloat):TpvFloat;
begin
 result:=a.ToFloat;
end;

class operator TpvHalfFloat.Explicit(const a:TpvFloat):TpvHalfFloat;
begin
 result:=TpvHalfFloat.FromFloat(a);
end;

class operator TpvHalfFloat.Explicit(const a:TpvHalfFloat):TpvFloat;
begin
 result:=a.ToFloat;
end;

class operator TpvHalfFloat.Equal(const a,b:TpvHalfFloat):boolean;
begin
 result:=a.ToFloat=b.ToFloat;
end;

class operator TpvHalfFloat.Equal(const a:TpvHalfFloat;const b:TpvFloat):boolean;
begin
 result:=a.ToFloat=b;
end;

class operator TpvHalfFloat.Equal(const a:TpvFloat;const b:TpvHalfFloat):boolean;
begin
 result:=a=b.ToFloat;
end;

class operator TpvHalfFloat.NotEqual(const a,b:TpvHalfFloat):boolean;
begin
 result:=a.ToFloat=b.ToFloat;
end;

class operator TpvHalfFloat.NotEqual(const a:TpvHalfFloat;const b:TpvFloat):boolean;
begin
 result:=a.ToFloat<>b;
end;

class operator TpvHalfFloat.NotEqual(const a:TpvFloat;const b:TpvHalfFloat):boolean;
begin
 result:=a<>b.ToFloat;
end;

class operator TpvHalfFloat.GreaterThan(const a,b:TpvHalfFloat):boolean;
begin
 result:=a.ToFloat>b.ToFloat;
end;

class operator TpvHalfFloat.GreaterThan(const a:TpvHalfFloat;const b:TpvFloat):boolean;
begin
 result:=a.ToFloat>b;
end;

class operator TpvHalfFloat.GreaterThan(const a:TpvFloat;const b:TpvHalfFloat):boolean;
begin
 result:=a>b.ToFloat;
end;

class operator TpvHalfFloat.GreaterThanOrEqual(const a,b:TpvHalfFloat):boolean;
begin
 result:=a.ToFloat>=b.ToFloat;
end;

class operator TpvHalfFloat.GreaterThanOrEqual(const a:TpvHalfFloat;const b:TpvFloat):boolean;
begin
 result:=a.ToFloat>=b;
end;

class operator TpvHalfFloat.GreaterThanOrEqual(const a:TpvFloat;const b:TpvHalfFloat):boolean;
begin
 result:=a>=b.ToFloat;
end;

class operator TpvHalfFloat.LessThan(const a,b:TpvHalfFloat):boolean;
begin
 result:=a.ToFloat<b.ToFloat;
end;

class operator TpvHalfFloat.LessThan(const a:TpvHalfFloat;const b:TpvFloat):boolean;
begin
 result:=a.ToFloat<b;
end;

class operator TpvHalfFloat.LessThan(const a:TpvFloat;const b:TpvHalfFloat):boolean;
begin
 result:=a<b.ToFloat;
end;

class operator TpvHalfFloat.LessThanOrEqual(const a,b:TpvHalfFloat):boolean;
begin
 result:=a.ToFloat<=b.ToFloat;
end;

class operator TpvHalfFloat.LessThanOrEqual(const a:TpvHalfFloat;const b:TpvFloat):boolean;
begin
 result:=a.ToFloat<=b;
end;

class operator TpvHalfFloat.LessThanOrEqual(const a:TpvFloat;const b:TpvHalfFloat):boolean;
begin
 result:=a<=b.ToFloat;
end;

class operator TpvHalfFloat.Inc(const a:TpvHalfFloat):TpvHalfFloat;
begin
 result:=TpvHalfFloat.FromFloat(a.ToFloat+1.0);
end;

class operator TpvHalfFloat.Dec(const a:TpvHalfFloat):TpvHalfFloat;
begin
 result:=TpvHalfFloat.FromFloat(a.ToFloat-1.0);
end;

class operator TpvHalfFloat.Add(const a,b:TpvHalfFloat):TpvFloat;
begin
 result:=a.ToFloat+b.ToFloat;
end;

class operator TpvHalfFloat.Add(const a:TpvHalfFloat;const b:TpvFloat):TpvFloat;
begin
 result:=a.ToFloat+b;
end;

class operator TpvHalfFloat.Add(const a:TpvFloat;const b:TpvHalfFloat):TpvFloat;
begin
 result:=a+b.ToFloat;
end;

class operator TpvHalfFloat.Subtract(const a,b:TpvHalfFloat):TpvFloat;
begin
 result:=a.ToFloat-b.ToFloat;
end;

class operator TpvHalfFloat.Subtract(const a:TpvHalfFloat;const b:TpvFloat):TpvFloat;
begin
 result:=a.ToFloat-b;
end;

class operator TpvHalfFloat.Subtract(const a:TpvFloat;const b:TpvHalfFloat):TpvFloat;
begin
 result:=a-b.ToFloat;
end;

class operator TpvHalfFloat.Multiply(const a,b:TpvHalfFloat):TpvFloat;
begin
 result:=a.ToFloat*b.ToFloat;
end;

class operator TpvHalfFloat.Multiply(const a:TpvHalfFloat;const b:TpvFloat):TpvFloat;
begin
 result:=a.ToFloat*b;
end;

class operator TpvHalfFloat.Multiply(const a:TpvFloat;const b:TpvHalfFloat):TpvFloat;
begin
 result:=a*b.ToFloat;
end;

class operator TpvHalfFloat.Divide(const a,b:TpvHalfFloat):TpvFloat;
begin
 result:=a.ToFloat/b.ToFloat;
end;

class operator TpvHalfFloat.Divide(const a:TpvHalfFloat;const b:TpvFloat):TpvFloat;
begin
 result:=a.ToFloat/b;
end;

class operator TpvHalfFloat.Divide(const a:TpvFloat;const b:TpvHalfFloat):TpvFloat;
begin
 result:=a/b.ToFloat;
end;

class operator TpvHalfFloat.Modulus(const a,b:TpvHalfFloat):TpvFloat;
var x,y:TpvFloat;
begin
 x:=a.ToFloat;
 y:=b.ToFloat;
 result:=x-(floor(x/y)*y);
end;

class operator TpvHalfFloat.Negative(const a:TpvHalfFloat):TpvHalfFloat;
begin
 result.Value:=a.Value xor $8000;
end;

class operator TpvHalfFloat.Positive(const a:TpvHalfFloat):TpvHalfFloat;
begin
 result:=a;
end;

function TpvHalfFloat.Abs:TpvHalfFloat;
begin
 result.Value:=Value and $7fff;
end;

function TpvHalfFloat.IsNaN:boolean;
begin
 result:=(Value and $7fff)>$7c00;
end;

function TpvHalfFloat.IsInfinity:boolean;
begin
 result:=(Value and $7fff)=$7c00;
end;

function TpvHalfFloat.IsNegativeInfinity:boolean;
begin
 result:=Value=$fc00;
end;

function TpvHalfFloat.IsPositiveInfinity:boolean;
begin
 result:=Value=$7c00;
end;

initialization
 GenerateHalfFloatLookUpTables;
end.
