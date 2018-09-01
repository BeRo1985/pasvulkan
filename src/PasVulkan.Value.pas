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
unit PasVulkan.Value;
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
     Variants,
     PasDblStrUtils,
     PasVulkan.Types;

type PpvValueType=^TpvValueType;
     TpvValueType=
      (
       Unknown,
       Boolean,
       Integer,
       Double,
       Char,
       String_
      );

     PPpvValue=^PpvValue;
     PpvValue=^TpvValue;
     TpvValue=record
      public
       class operator Implicit(const aInput:boolean):TpvValue; inline;
       class operator Implicit(const aInput:TpvInt64):TpvValue; inline;
       class operator Implicit(const aInput:TpvDouble):TpvValue; inline;
       class operator Implicit(const aInput:Char):TpvValue; inline;
       class operator Implicit(const aInput:String):TpvValue; inline;
       class operator Implicit(const aInput:Variant):TpvValue; inline;
       class operator Implicit(const aInput:TpvValue):boolean;
       class operator Implicit(const aInput:TpvValue):TpvInt64;
       class operator Implicit(const aInput:TpvValue):TpvDouble;
       class operator Implicit(const aInput:TpvValue):Char;
       class operator Implicit(const aInput:TpvValue):String;
       class operator Implicit(const aInput:TpvValue):Variant;
       class operator Explicit(const aInput:boolean):TpvValue; inline;
       class operator Explicit(const aInput:TpvInt64):TpvValue; inline;
       class operator Explicit(const aInput:TpvDouble):TpvValue; inline;
       class operator Explicit(const aInput:Char):TpvValue; inline;
       class operator Explicit(const aInput:String):TpvValue; inline;
       class operator Explicit(const aInput:Variant):TpvValue; inline;
       class operator Explicit(const aInput:TpvValue):boolean;
       class operator Explicit(const aInput:TpvValue):TpvInt64;
       class operator Explicit(const aInput:TpvValue):TpvDouble;
       class operator Explicit(const aInput:TpvValue):Char;
       class operator Explicit(const aInput:TpvValue):String;
       class operator Explicit(const aInput:TpvValue):Variant;
       class operator Equal(const aInputA,aInputB:TpvValue):boolean;
       class operator NotEqual(const aInputA,aInputB:TpvValue):boolean;
       class operator GreaterThan(const aInputA,aInputB:TpvValue):boolean;
       class operator GreaterThanOrEqual(const aInputA,aInputB:TpvValue):boolean;
       class operator LessThan(const aInputA,aInputB:TpvValue):boolean;
       class operator LessThanOrEqual(const aInputA,aInputB:TpvValue):boolean;
       class operator Inc(const aInput:TpvValue):TpvValue;
       class operator Dec(const aInput:TpvValue):TpvValue;
       class operator Add(const aInputA,aInputB:TpvValue):TpvValue;
       class operator Subtract(const aInputA,aInputB:TpvValue):TpvValue;
       class operator Multiply(const aInputA,aInputB:TpvValue):TpvValue;
       class operator Divide(const aInputA,aInputB:TpvValue):TpvValue;
       class operator IntDivide(const aInputA,aInputB:TpvValue):TpvValue;
       class operator Modulus(const aInputA,aInputB:TpvValue):TpvValue;
       class operator LeftShift(const aInputA,aInputB:TpvValue):TpvValue;
       class operator RightShift(const aInputA,aInputB:TpvValue):TpvValue;
       class operator LogicalAnd(const aInputA,aInputB:TpvValue):TpvValue;
       class operator LogicalOr(const aInputA,aInputB:TpvValue):TpvValue;
       class operator LogicalXor(const aInputA,aInputB:TpvValue):TpvValue;
       class operator BitwiseAnd(const aInputA,aInputB:TpvValue):TpvValue;
       class operator BitwiseOr(const aInputA,aInputB:TpvValue):TpvValue;
       class operator BitwiseXor(const aInputA,aInputB:TpvValue):TpvValue;
       class operator Negative(const aInput:TpvValue):TpvValue;
       class operator Positive(const aInput:TpvValue):TpvValue;
      public
       pvValueType:TpvValueType;
       pvValueString:string;
       case TpvValueType of
        TpvValueType.Unknown:(
        );
        TpvValueType.Boolean:(
         pvValueBoolean:Boolean;
        );
        TpvValueType.Integer:(
         pvValueInt64:Int64;
        );
        TpvValueType.Double:(
         pvValueDouble:Double;
        );
        TpvValueType.Char:(
         pvValueChar:Char;
        );
     end;

implementation

class operator TpvValue.Implicit(const aInput:boolean):TpvValue;
begin
 result.pvValueType:=TpvValueType.Boolean;
 result.pvValueBoolean:=aInput;
end;

class operator TpvValue.Implicit(const aInput:TpvInt64):TpvValue;
begin
 result.pvValueType:=TpvValueType.Integer;
 result.pvValueInt64:=aInput;
end;

class operator TpvValue.Implicit(const aInput:TpvDouble):TpvValue;
begin
 result.pvValueType:=TpvValueType.Double;
 result.pvValueDouble:=aInput;
end;

class operator TpvValue.Implicit(const aInput:Char):TpvValue;
begin
 result.pvValueType:=TpvValueType.Char;
 result.pvValueChar:=aInput;
end;

class operator TpvValue.Implicit(const aInput:String):TpvValue;
begin
 result.pvValueType:=TpvValueType.String_;
 result.pvValueString:=aInput;
end;

class operator TpvValue.Implicit(const aInput:Variant):TpvValue;
begin
 case VarType(aInput) of
  varSmallInt,varInteger,varShortInt,varByte,varWord,varLongWord,varInt64{$ifdef fpc},varQWord{$endif}:begin
   result.pvValueType:=TpvValueType.Integer;
   result.pvValueInt64:=aInput;
  end;
  varSingle,varDouble,varDATE,varCurrency:begin
   result.pvValueType:=TpvValueType.Double;
   result.pvValueDouble:=aInput;
  end;
  varBoolean:begin
   result.pvValueType:=TpvValueType.Boolean;
   result.pvValueBoolean:=aInput;
  end;
  varString,varOleStr:begin
   result.pvValueType:=TpvValueType.String_;
   result.pvValueString:=aInput;
  end;
  else begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
 end;
end;

class operator TpvValue.Implicit(const aInput:TpvValue):boolean;
begin
case aInput.pvValueType of
  TpvValueType.Boolean:begin
   result:=aInput.pvValueBoolean;
  end;
  TpvValueType.Integer:begin
   result:=aInput.pvValueInt64<>0;
  end;
  TpvValueType.Double:begin
   result:=aInput.pvValueDouble<>0.0;
  end;
  TpvValueType.Char:begin
   result:=aInput.pvValueChar<>#0;
  end;
  TpvValueType.String_:begin
   result:=length(aInput.pvValueString)>0;
  end;
  else begin
   result:=false;
  end;
 end;
end;

class operator TpvValue.Implicit(const aInput:TpvValue):TpvInt64;
begin
 case aInput.pvValueType of
  TpvValueType.Boolean:begin
   result:=ord(aInput.pvValueBoolean) and 1;
  end;
  TpvValueType.Integer:begin
   result:=aInput.pvValueInt64;
  end;
  TpvValueType.Double:begin
   result:=trunc(aInput.pvValueDouble);
  end;
  TpvValueType.Char:begin
   result:=StrToIntDef(aInput.pvValueChar,0);
  end;
  TpvValueType.String_:begin
   result:=StrToIntDef(aInput.pvValueString,0);
  end;
  else begin
   result:=0;
  end;
 end;
end;

class operator TpvValue.Implicit(const aInput:TpvValue):TpvDouble;
begin
case aInput.pvValueType of
  TpvValueType.Boolean:begin
   result:=ord(aInput.pvValueBoolean) and 1;
  end;
  TpvValueType.Integer:begin
   result:=aInput.pvValueInt64;
  end;
  TpvValueType.Double:begin
   result:=aInput.pvValueDouble;
  end;
  TpvValueType.Char:begin
   result:=ConvertStringToDouble(aInput.pvValueChar,rmNearest,nil,-1);
  end;
  TpvValueType.String_:begin
   result:=ConvertStringToDouble(aInput.pvValueString,rmNearest,nil,-1);
  end;
  else begin
   result:=0.0;
  end;
 end;
end;

class operator TpvValue.Implicit(const aInput:TpvValue):Char;
begin
 case aInput.pvValueType of
  TpvValueType.Boolean:begin
   if aInput.pvValueBoolean then begin
    result:=#1;
   end else begin
    result:=#0;
   end;
  end;
  TpvValueType.Integer:begin
   result:=chr(aInput.pvValueInt64);
  end;
  TpvValueType.Double:begin
   result:=chr(trunc(aInput.pvValueDouble));
  end;
  TpvValueType.Char:begin
   result:=aInput.pvValueChar;
  end;
  TpvValueType.String_:begin
   if length(aInput.pvValueString)>0 then begin
    result:=aInput.pvValueString[1];
   end else begin
    result:=#0;
   end;
  end;
  else begin
   result:=#0;
  end;
 end;
end;

class operator TpvValue.Implicit(const aInput:TpvValue):String;
begin
 case aInput.pvValueType of
  TpvValueType.Boolean:begin
   if aInput.pvValueBoolean then begin
    result:='true';
   end else begin
    result:='false';
   end;
  end;
  TpvValueType.Integer:begin
   result:=IntToStr(aInput.pvValueInt64);
  end;
  TpvValueType.Double:begin
   result:=ConvertDoubleToString(aInput.pvValueDouble,omStandard,0);
  end;
  TpvValueType.Char:begin
   result:=aInput.pvValueChar;
  end;
  TpvValueType.String_:begin
   result:=aInput.pvValueString;
  end;
  else begin
   result:='';
  end;
 end;
end;

class operator TpvValue.Implicit(const aInput:TpvValue):Variant;
begin
 case aInput.pvValueType of
  TpvValueType.Boolean:begin
   result:=aInput.pvValueBoolean;
  end;
  TpvValueType.Integer:begin
   result:=aInput.pvValueInt64;
  end;
  TpvValueType.Double:begin
   result:=aInput.pvValueDouble;
  end;
  TpvValueType.Char:begin
   result:=aInput.pvValueChar;
  end;
  TpvValueType.String_:begin
   result:=aInput.pvValueString;
  end;
  else begin
   result:=Variants.Unassigned;
  end;
 end;
end;

class operator TpvValue.Explicit(const aInput:boolean):TpvValue;
begin
 result.pvValueType:=TpvValueType.Boolean;
 result.pvValueBoolean:=aInput;
end;

class operator TpvValue.Explicit(const aInput:TpvInt64):TpvValue;
begin
 result.pvValueType:=TpvValueType.Integer;
 result.pvValueInt64:=aInput;
end;

class operator TpvValue.Explicit(const aInput:TpvDouble):TpvValue;
begin
 result.pvValueType:=TpvValueType.Double;
 result.pvValueDouble:=aInput;
end;

class operator TpvValue.Explicit(const aInput:Char):TpvValue;
begin
 result.pvValueType:=TpvValueType.Char;
 result.pvValueChar:=aInput;
end;

class operator TpvValue.Explicit(const aInput:String):TpvValue;
begin
 result.pvValueType:=TpvValueType.String_;
 result.pvValueString:=aInput;
end;

class operator TpvValue.Explicit(const aInput:Variant):TpvValue;
begin
 case VarType(aInput) of
  varSmallInt,varInteger,varShortInt,varByte,varWord,varLongWord,varInt64{$ifdef fpc},varQWord{$endif}:begin
   result.pvValueType:=TpvValueType.Integer;
   result.pvValueInt64:=aInput;
  end;
  varSingle,varDouble,varDATE,varCurrency:begin
   result.pvValueType:=TpvValueType.Double;
   result.pvValueDouble:=aInput;
  end;
  varBoolean:begin
   result.pvValueType:=TpvValueType.Boolean;
   result.pvValueBoolean:=aInput;
  end;
  varString,varOleStr:begin
   result.pvValueType:=TpvValueType.String_;
   result.pvValueString:=aInput;
  end;
  else begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
 end;
end;

class operator TpvValue.Explicit(const aInput:TpvValue):boolean;
begin
case aInput.pvValueType of
  TpvValueType.Boolean:begin
   result:=aInput.pvValueBoolean;
  end;
  TpvValueType.Integer:begin
   result:=aInput.pvValueInt64<>0;
  end;
  TpvValueType.Double:begin
   result:=aInput.pvValueDouble<>0.0;
  end;
  TpvValueType.Char:begin
   result:=aInput.pvValueChar<>#0;
  end;
  TpvValueType.String_:begin
   result:=length(aInput.pvValueString)>0;
  end;
  else begin
   result:=false;
  end;
 end;
end;

class operator TpvValue.Explicit(const aInput:TpvValue):TpvInt64;
begin
 case aInput.pvValueType of
  TpvValueType.Boolean:begin
   result:=ord(aInput.pvValueBoolean) and 1;
  end;
  TpvValueType.Integer:begin
   result:=aInput.pvValueInt64;
  end;
  TpvValueType.Double:begin
   result:=trunc(aInput.pvValueDouble);
  end;
  TpvValueType.Char:begin
   result:=StrToIntDef(aInput.pvValueChar,0);
  end;
  TpvValueType.String_:begin
   result:=StrToIntDef(aInput.pvValueString,0);
  end;
  else begin
   result:=0;
  end;
 end;
end;

class operator TpvValue.Explicit(const aInput:TpvValue):TpvDouble;
begin
case aInput.pvValueType of
  TpvValueType.Boolean:begin
   result:=ord(aInput.pvValueBoolean) and 1;
  end;
  TpvValueType.Integer:begin
   result:=aInput.pvValueInt64;
  end;
  TpvValueType.Double:begin
   result:=aInput.pvValueDouble;
  end;
  TpvValueType.Char:begin
   result:=ConvertStringToDouble(aInput.pvValueChar,rmNearest,nil,-1);
  end;
  TpvValueType.String_:begin
   result:=ConvertStringToDouble(aInput.pvValueString,rmNearest,nil,-1);
  end;
  else begin
   result:=0.0;
  end;
 end;
end;

class operator TpvValue.Explicit(const aInput:TpvValue):Char;
begin
 case aInput.pvValueType of
  TpvValueType.Boolean:begin
   if aInput.pvValueBoolean then begin
    result:=#1;
   end else begin
    result:=#0;
   end;
  end;
  TpvValueType.Integer:begin
   result:=chr(aInput.pvValueInt64);
  end;
  TpvValueType.Double:begin
   result:=chr(trunc(aInput.pvValueDouble));
  end;
  TpvValueType.Char:begin
   result:=aInput.pvValueChar;
  end;
  TpvValueType.String_:begin
   if length(aInput.pvValueString)>0 then begin
    result:=aInput.pvValueString[1];
   end else begin
    result:=#0;
   end;
  end;
  else begin
   result:=#0;
  end;
 end;
end;

class operator TpvValue.Explicit(const aInput:TpvValue):String;
begin
 case aInput.pvValueType of
  TpvValueType.Boolean:begin
   if aInput.pvValueBoolean then begin
    result:='true';
   end else begin
    result:='false';
   end;
  end;
  TpvValueType.Integer:begin
   result:=IntToStr(aInput.pvValueInt64);
  end;
  TpvValueType.Double:begin
   result:=ConvertDoubleToString(aInput.pvValueDouble,omStandard,0);
  end;
  TpvValueType.Char:begin
   result:=aInput.pvValueChar;
  end;
  TpvValueType.String_:begin
   result:=aInput.pvValueString;
  end;
  else begin
   result:='';
  end;
 end;
end;

class operator TpvValue.Explicit(const aInput:TpvValue):Variant;
begin
 case aInput.pvValueType of
  TpvValueType.Boolean:begin
   result:=aInput.pvValueBoolean;
  end;
  TpvValueType.Integer:begin
   result:=aInput.pvValueInt64;
  end;
  TpvValueType.Double:begin
   result:=aInput.pvValueDouble;
  end;
  TpvValueType.Char:begin
   result:=aInput.pvValueChar;
  end;
  TpvValueType.String_:begin
   result:=aInput.pvValueString;
  end;
  else begin
   result:=Variants.Unassigned;
  end;
 end;
end;

class operator TpvValue.Equal(const aInputA,aInputB:TpvValue):boolean;
begin
case aInputA.pvValueType of
  TpvValueType.Boolean:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result:=aInputA.pvValueBoolean=aInputB.pvValueBoolean;
    end;
    TpvValueType.Integer:begin
     result:=(ord(aInputA.pvValueBoolean) and 1)=aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result:=(ord(aInputA.pvValueBoolean) and 1)=aInputB.pvValueDouble;
    end;
    TpvValueType.String_:begin
     result:=aInputA.pvValueBoolean=boolean(length(aInputB.pvValueString)>0);
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.Integer:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result:=aInputA.pvValueInt64=(ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result:=aInputA.pvValueInt64=aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result:=aInputA.pvValueInt64=aInputB.pvValueDouble;
    end;
    TpvValueType.String_:begin
     result:=IntToStr(aInputA.pvValueInt64)=aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.Double:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result:=aInputA.pvValueDouble=(ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result:=aInputA.pvValueDouble=aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result:=aInputA.pvValueDouble=aInputB.pvValueDouble;
    end;
    TpvValueType.String_:begin
     result:=ConvertDoubleToString(aInputA.pvValueDouble,omStandard,0)=aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.Char:begin
   case aInputB.pvValueType of
    TpvValueType.Char:begin
     result:=aInputA.pvValueChar=aInputB.pvValueChar;
    end;
    TpvValueType.String_:begin
     result:=aInputA.pvValueChar=aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.String_:begin
   case aInputB.pvValueType of
    TpvValueType.Char:begin
     result:=aInputA.pvValueChar=aInputB.pvValueString;
    end;
    TpvValueType.String_:begin
     result:=aInputA.pvValueString=aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  else begin
   result:=false;
  end;
 end;
end;

class operator TpvValue.NotEqual(const aInputA,aInputB:TpvValue):boolean;
begin
 case aInputA.pvValueType of
  TpvValueType.Boolean:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result:=aInputA.pvValueBoolean<>aInputB.pvValueBoolean;
    end;
    TpvValueType.Integer:begin
     result:=(ord(aInputA.pvValueBoolean) and 1)<>aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result:=(ord(aInputA.pvValueBoolean) and 1)<>aInputB.pvValueDouble;
    end;
    TpvValueType.String_:begin
     result:=aInputA.pvValueBoolean<>boolean(length(aInputB.pvValueString)>0);
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.Integer:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result:=aInputA.pvValueInt64<>(ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result:=aInputA.pvValueInt64<>aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result:=aInputA.pvValueInt64<>aInputB.pvValueDouble;
    end;
    TpvValueType.String_:begin
     result:=IntToStr(aInputA.pvValueInt64)<>aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.Double:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result:=aInputA.pvValueDouble<>(ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result:=aInputA.pvValueDouble<>aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result:=aInputA.pvValueDouble<>aInputB.pvValueDouble;
    end;
    TpvValueType.String_:begin
     result:=ConvertDoubleToString(aInputA.pvValueDouble,omStandard,0)<>aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.Char:begin
   case aInputB.pvValueType of
    TpvValueType.Char:begin
     result:=aInputA.pvValueChar<>aInputB.pvValueChar;
    end;
    TpvValueType.String_:begin
     result:=aInputA.pvValueChar<>aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.String_:begin
   case aInputB.pvValueType of
    TpvValueType.Char:begin
     result:=aInputA.pvValueChar<>aInputB.pvValueString;
    end;
    TpvValueType.String_:begin
     result:=aInputA.pvValueString<>aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  else begin
   result:=false;
  end;
 end;
end;

class operator TpvValue.GreaterThan(const aInputA,aInputB:TpvValue):boolean;
begin
 case aInputA.pvValueType of
  TpvValueType.Boolean:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result:=aInputA.pvValueBoolean>aInputB.pvValueBoolean;
    end;
    TpvValueType.Integer:begin
     result:=(ord(aInputA.pvValueBoolean) and 1)>aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result:=(ord(aInputA.pvValueBoolean) and 1)>aInputB.pvValueDouble;
    end;
    TpvValueType.String_:begin
     result:=aInputA.pvValueBoolean>(length(aInputB.pvValueString)>0);
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.Integer:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result:=aInputA.pvValueInt64>(ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result:=aInputA.pvValueInt64>aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result:=aInputA.pvValueInt64>aInputB.pvValueDouble;
    end;
    TpvValueType.String_:begin
     result:=IntToStr(aInputA.pvValueInt64)>aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.Double:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result:=aInputA.pvValueDouble>(ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result:=aInputA.pvValueDouble>aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result:=aInputA.pvValueDouble>aInputB.pvValueDouble;
    end;
    TpvValueType.String_:begin
     result:=ConvertDoubleToString(aInputA.pvValueDouble,omStandard,0)>aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.Char:begin
   case aInputB.pvValueType of
    TpvValueType.Char:begin
     result:=aInputA.pvValueChar>aInputB.pvValueChar;
    end;
    TpvValueType.String_:begin
     result:=aInputA.pvValueChar>aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.String_:begin
   case aInputB.pvValueType of
    TpvValueType.Char:begin
     result:=aInputA.pvValueChar>aInputB.pvValueString;
    end;
    TpvValueType.String_:begin
     result:=aInputA.pvValueString>aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  else begin
   result:=false;
  end;
 end;
end;

class operator TpvValue.GreaterThanOrEqual(const aInputA,aInputB:TpvValue):boolean;
begin
 case aInputA.pvValueType of
  TpvValueType.Boolean:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result:=aInputA.pvValueBoolean>=aInputB.pvValueBoolean;
    end;
    TpvValueType.Integer:begin
     result:=(ord(aInputA.pvValueBoolean) and 1)>=aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result:=(ord(aInputA.pvValueBoolean) and 1)>=aInputB.pvValueDouble;
    end;
    TpvValueType.String_:begin
     result:=aInputA.pvValueBoolean>=boolean(length(aInputB.pvValueString)>0);
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.Integer:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result:=aInputA.pvValueInt64>=(ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result:=aInputA.pvValueInt64>=aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result:=aInputA.pvValueInt64>=aInputB.pvValueDouble;
    end;
    TpvValueType.String_:begin
     result:=IntToStr(aInputA.pvValueInt64)>=aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.Double:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result:=aInputA.pvValueDouble>=(ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result:=aInputA.pvValueDouble>=aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result:=aInputA.pvValueDouble>=aInputB.pvValueDouble;
    end;
    TpvValueType.String_:begin
     result:=ConvertDoubleToString(aInputA.pvValueDouble,omStandard,0)>aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.Char:begin
   case aInputB.pvValueType of
    TpvValueType.Char:begin
     result:=aInputA.pvValueChar>=aInputB.pvValueChar;
    end;
    TpvValueType.String_:begin
     result:=aInputA.pvValueChar>=aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.String_:begin
   case aInputB.pvValueType of
    TpvValueType.Char:begin
     result:=aInputA.pvValueChar>=aInputB.pvValueString;
    end;
    TpvValueType.String_:begin
     result:=aInputA.pvValueString>=aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  else begin
   result:=false;
  end;
 end;
end;

class operator TpvValue.LessThan(const aInputA,aInputB:TpvValue):boolean;
begin
 case aInputA.pvValueType of
  TpvValueType.Boolean:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result:=aInputA.pvValueBoolean<aInputB.pvValueBoolean;
    end;
    TpvValueType.Integer:begin
     result:=(ord(aInputA.pvValueBoolean) and 1)<aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result:=(ord(aInputA.pvValueBoolean) and 1)<aInputB.pvValueDouble;
    end;
    TpvValueType.String_:begin
     result:=aInputA.pvValueBoolean<boolean(length(aInputB.pvValueString)>0);
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.Integer:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result:=aInputA.pvValueInt64<(ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result:=aInputA.pvValueInt64<aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result:=aInputA.pvValueInt64<aInputB.pvValueDouble;
    end;
    TpvValueType.String_:begin
     result:=IntToStr(aInputA.pvValueInt64)<aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.Double:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result:=aInputA.pvValueDouble<(ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result:=aInputA.pvValueDouble<aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result:=aInputA.pvValueDouble<aInputB.pvValueDouble;
    end;
    TpvValueType.String_:begin
     result:=ConvertDoubleToString(aInputA.pvValueDouble,omStandard,0)<aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.Char:begin
   case aInputB.pvValueType of
    TpvValueType.Char:begin
     result:=aInputA.pvValueChar<aInputB.pvValueChar;
    end;
    TpvValueType.String_:begin
     result:=aInputA.pvValueChar<aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.String_:begin
   case aInputB.pvValueType of
    TpvValueType.Char:begin
     result:=aInputA.pvValueChar<aInputB.pvValueString;
    end;
    TpvValueType.String_:begin
     result:=aInputA.pvValueString<aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  else begin
   result:=false;
  end;
 end;
end;

class operator TpvValue.LessThanOrEqual(const aInputA,aInputB:TpvValue):boolean;
begin
case aInputA.pvValueType of
  TpvValueType.Boolean:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result:=aInputA.pvValueBoolean<=aInputB.pvValueBoolean;
    end;
    TpvValueType.Integer:begin
     result:=(ord(aInputA.pvValueBoolean) and 1)<=aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result:=(ord(aInputA.pvValueBoolean) and 1)<=aInputB.pvValueDouble;
    end;
    TpvValueType.String_:begin
     result:=aInputA.pvValueBoolean<=boolean(length(aInputB.pvValueString)>0);
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.Integer:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result:=aInputA.pvValueInt64<=(ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result:=aInputA.pvValueInt64<=aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result:=aInputA.pvValueInt64<=aInputB.pvValueDouble;
    end;
    TpvValueType.String_:begin
     result:=IntToStr(aInputA.pvValueInt64)<=aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.Double:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result:=aInputA.pvValueDouble<=(ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result:=aInputA.pvValueDouble<=aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result:=aInputA.pvValueDouble<=aInputB.pvValueDouble;
    end;
    TpvValueType.String_:begin
     result:=ConvertDoubleToString(aInputA.pvValueDouble,omStandard,0)<=aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.Char:begin
   case aInputB.pvValueType of
    TpvValueType.Char:begin
     result:=aInputA.pvValueChar<=aInputB.pvValueChar;
    end;
    TpvValueType.String_:begin
     result:=aInputA.pvValueChar<=aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  TpvValueType.String_:begin
   case aInputB.pvValueType of
    TpvValueType.Char:begin
     result:=aInputA.pvValueChar<=aInputB.pvValueString;
    end;
    TpvValueType.String_:begin
     result:=aInputA.pvValueString<=aInputB.pvValueString;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
  else begin
   result:=false;
  end;
 end;
end;

class operator TpvValue.Inc(const aInput:TpvValue):TpvValue;
begin
 case aInput.pvValueType of
  TpvValueType.Boolean:begin
   result.pvValueType:=TpvValueType.Boolean;
   result.pvValueBoolean:=not result.pvValueBoolean;
  end;
  TpvValueType.Integer:begin
   result.pvValueType:=TpvValueType.Integer;
   result.pvValueInt64:=aInput.pvValueInt64+1;
  end;
  TpvValueType.Double:begin
   result.pvValueType:=TpvValueType.Double;
   result.pvValueDouble:=aInput.pvValueDouble+1;
  end;
  TpvValueType.Char:begin
   result.pvValueType:=TpvValueType.Char;
   result.pvValueChar:=aInput.pvValueChar;
   inc(result.pvValueChar);
  end;
  TpvValueType.String_:begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
  else begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
 end;
end;

class operator TpvValue.Dec(const aInput:TpvValue):TpvValue;
begin
 case aInput.pvValueType of
  TpvValueType.Boolean:begin
   result.pvValueType:=TpvValueType.Boolean;
   result.pvValueBoolean:=not result.pvValueBoolean;
  end;
  TpvValueType.Integer:begin
   result.pvValueType:=TpvValueType.Integer;
   result.pvValueInt64:=aInput.pvValueInt64-1;
  end;
  TpvValueType.Double:begin
   result.pvValueType:=TpvValueType.Double;
   result.pvValueDouble:=aInput.pvValueDouble-1;
  end;
  TpvValueType.Char:begin
   result.pvValueType:=TpvValueType.Char;
   result.pvValueChar:=aInput.pvValueChar;
   dec(result.pvValueChar);
  end;
  TpvValueType.String_:begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
  else begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
 end;
end;

class operator TpvValue.Add(const aInputA,aInputB:TpvValue):TpvValue;
begin
 case aInputA.pvValueType of
  TpvValueType.Boolean:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Boolean;
     result.pvValueBoolean:=aInputA.pvValueBoolean xor aInputB.pvValueBoolean;
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=(ord(aInputA.pvValueBoolean) and 1)+aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=(ord(aInputA.pvValueBoolean) and 1)+aInputB.pvValueDouble;
    end;
    TpvValueType.Char:begin
     result.pvValueType:=TpvValueType.String_;
     if aInputA.pvValueBoolean then begin
      result.pvValueString:='true'+aInputB.pvValueChar;
     end else begin
      result.pvValueString:='false'+aInputB.pvValueChar;
     end;
    end;
    TpvValueType.String_:begin
     result.pvValueType:=TpvValueType.String_;
     if aInputA.pvValueBoolean then begin
      result.pvValueString:='true'+aInputB.pvValueString;
     end else begin
      result.pvValueString:='false'+aInputB.pvValueString;
     end;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  TpvValueType.Integer:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64+(ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64+aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueInt64+aInputB.pvValueDouble;
    end;
    TpvValueType.Char:begin
     result.pvValueType:=TpvValueType.String_;
     result.pvValueString:=IntToStr(aInputA.pvValueInt64)+aInputB.pvValueChar;
    end;
    TpvValueType.String_:begin
     result.pvValueType:=TpvValueType.String_;
     result.pvValueString:=IntToStr(aInputA.pvValueInt64)+aInputB.pvValueString;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  TpvValueType.Double:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueDouble+(ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueDouble+aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueDouble+aInputB.pvValueDouble;
    end;
    TpvValueType.Char:begin
     result.pvValueType:=TpvValueType.String_;
     result.pvValueString:=ConvertDoubleToString(aInputA.pvValueDouble,omStandard,0)+aInputB.pvValueChar;
    end;
    TpvValueType.String_:begin
     result.pvValueType:=TpvValueType.String_;
     result.pvValueString:=ConvertDoubleToString(aInputA.pvValueDouble,omStandard,0)+aInputB.pvValueString;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  TpvValueType.Char:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.String_;
     if aInputB.pvValueBoolean then begin
      result.pvValueString:=aInputA.pvValueChar+'true';
     end else begin
      result.pvValueString:=aInputA.pvValueChar+'false';
     end;
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.String_;
     result.pvValueString:=aInputA.pvValueChar+IntToStr(aInputB.pvValueInt64);
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.String_;
     result.pvValueString:=aInputA.pvValueChar+ConvertDoubleToString(aInputB.pvValueDouble,omStandard,0);
    end;
    TpvValueType.Char:begin
     result.pvValueType:=TpvValueType.String_;
     result.pvValueString:=aInputA.pvValueChar+aInputB.pvValueChar;
    end;
    TpvValueType.String_:begin
     result.pvValueType:=TpvValueType.String_;
     result.pvValueString:=aInputA.pvValueChar+aInputB.pvValueString;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  TpvValueType.String_:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.String_;
     if aInputB.pvValueBoolean then begin
      result.pvValueString:=aInputA.pvValueString+'true';
     end else begin
      result.pvValueString:=aInputA.pvValueString+'false';
     end;
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.String_;
     result.pvValueString:=aInputA.pvValueString+IntToStr(aInputB.pvValueInt64);
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.String_;
     result.pvValueString:=aInputA.pvValueString+ConvertDoubleToString(aInputB.pvValueDouble,omStandard,0);
    end;
    TpvValueType.Char:begin
     result.pvValueType:=TpvValueType.String_;
     result.pvValueString:=aInputA.pvValueString+aInputB.pvValueChar;
    end;
    TpvValueType.String_:begin
     result.pvValueType:=TpvValueType.String_;
     result.pvValueString:=aInputA.pvValueString+aInputB.pvValueString;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  else begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
 end;
end;

class operator TpvValue.Subtract(const aInputA,aInputB:TpvValue):TpvValue;
begin
 case aInputA.pvValueType of
  TpvValueType.Boolean:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Boolean;
     result.pvValueBoolean:=aInputA.pvValueBoolean xor aInputB.pvValueBoolean;
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=(ord(aInputA.pvValueBoolean) and 1)-aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=(ord(aInputA.pvValueBoolean) and 1)-aInputB.pvValueDouble;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  TpvValueType.Integer:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64-(ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64-aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueInt64-aInputB.pvValueDouble;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  TpvValueType.Double:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueDouble-(ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueDouble-aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueDouble-aInputB.pvValueDouble;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  else begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
 end;
end;

class operator TpvValue.Multiply(const aInputA,aInputB:TpvValue):TpvValue;
begin
 case aInputA.pvValueType of
  TpvValueType.Integer:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64*(ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64*aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueInt64*aInputB.pvValueDouble;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  TpvValueType.Double:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueDouble*(ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueDouble*aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueDouble*aInputB.pvValueDouble;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  else begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
 end;
end;

class operator TpvValue.Divide(const aInputA,aInputB:TpvValue):TpvValue;
begin
 case aInputA.pvValueType of
  TpvValueType.Integer:begin
   case aInputB.pvValueType of
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueInt64/aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueInt64/aInputB.pvValueDouble;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  TpvValueType.Double:begin
   case aInputB.pvValueType of
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueDouble/aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueDouble/aInputB.pvValueDouble;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  else begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
 end;
end;

class operator TpvValue.IntDivide(const aInputA,aInputB:TpvValue):TpvValue;
begin
 case aInputA.pvValueType of
  TpvValueType.Integer:begin
   case aInputB.pvValueType of
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64 div aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueInt64/aInputB.pvValueDouble;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  TpvValueType.Double:begin
   case aInputB.pvValueType of
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueDouble/aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueDouble/aInputB.pvValueDouble;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  else begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
 end;
end;

class operator TpvValue.Modulus(const aInputA,aInputB:TpvValue):TpvValue;
begin
 case aInputA.pvValueType of
  TpvValueType.Integer:begin
   case aInputB.pvValueType of
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64 mod aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueInt64-(floor(aInputA.pvValueInt64*aInputB.pvValueDouble)/aInputB.pvValueDouble);
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  TpvValueType.Double:begin
   case aInputB.pvValueType of
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueDouble-(floor(aInputA.pvValueDouble*aInputB.pvValueInt64)/aInputB.pvValueInt64);
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueDouble-(floor(aInputA.pvValueDouble*aInputB.pvValueDouble)/aInputB.pvValueDouble);
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  else begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
 end;
end;

class operator TpvValue.LeftShift(const aInputA,aInputB:TpvValue):TpvValue;
begin
 case aInputA.pvValueType of
  TpvValueType.Boolean:begin
   case aInputB.pvValueType of
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=int64(ord(aInputA.pvValueBoolean) and 1) shl aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=(ord(aInputA.pvValueBoolean) and 1)*power(2.0,aInputB.pvValueDouble);
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  TpvValueType.Integer:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64 shl (ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64 shl aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueInt64*power(2.0,aInputB.pvValueDouble);
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  TpvValueType.Double:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueDouble*(2.0*(ord(aInputB.pvValueBoolean) and 1));
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueDouble*power(2.0,aInputB.pvValueInt64);
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueDouble*power(2.0,aInputB.pvValueDouble);
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  else begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
 end;
end;

class operator TpvValue.RightShift(const aInputA,aInputB:TpvValue):TpvValue;
begin
 case aInputA.pvValueType of
  TpvValueType.Boolean:begin
   case aInputB.pvValueType of
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=int64(ord(aInputA.pvValueBoolean) and 1) shr aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=(ord(aInputA.pvValueBoolean) and 1)/power(2.0,aInputB.pvValueDouble);
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  TpvValueType.Integer:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64 shr (ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64 shr aInputB.pvValueInt64;
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueInt64/power(2.0,aInputB.pvValueDouble);
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  TpvValueType.Double:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueDouble*(0.5*(ord(aInputB.pvValueBoolean) and 1));
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueDouble/power(2.0,aInputB.pvValueInt64);
    end;
    TpvValueType.Double:begin
     result.pvValueType:=TpvValueType.Double;
     result.pvValueDouble:=aInputA.pvValueDouble/power(2.0,aInputB.pvValueDouble);
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  else begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
 end;
end;

class operator TpvValue.LogicalAnd(const aInputA,aInputB:TpvValue):TpvValue;
begin
 case aInputA.pvValueType of
  TpvValueType.Boolean:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Boolean;
     result.pvValueBoolean:=aInputA.pvValueBoolean and aInputB.pvValueBoolean;
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=(ord(aInputA.pvValueBoolean) and 1) and aInputB.pvValueInt64;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  TpvValueType.Integer:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64 and (ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64 and aInputB.pvValueInt64;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  else begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
 end;
end;

class operator TpvValue.LogicalOr(const aInputA,aInputB:TpvValue):TpvValue;
begin
 case aInputA.pvValueType of
  TpvValueType.Boolean:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Boolean;
     result.pvValueBoolean:=aInputA.pvValueBoolean or aInputB.pvValueBoolean;
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=(ord(aInputA.pvValueBoolean) and 1) or aInputB.pvValueInt64;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  TpvValueType.Integer:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64 or (ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64 or aInputB.pvValueInt64;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  else begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
 end;
end;

class operator TpvValue.LogicalXor(const aInputA,aInputB:TpvValue):TpvValue;
begin
 case aInputA.pvValueType of
  TpvValueType.Boolean:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Boolean;
     result.pvValueBoolean:=aInputA.pvValueBoolean xor aInputB.pvValueBoolean;
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=(ord(aInputA.pvValueBoolean) and 1) xor aInputB.pvValueInt64;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  TpvValueType.Integer:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64 xor (ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64 xor aInputB.pvValueInt64;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  else begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
 end;
end;

class operator TpvValue.BitwiseAnd(const aInputA,aInputB:TpvValue):TpvValue;
begin
 case aInputA.pvValueType of
  TpvValueType.Boolean:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Boolean;
     result.pvValueBoolean:=aInputA.pvValueBoolean and aInputB.pvValueBoolean;
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=(ord(aInputA.pvValueBoolean) and 1) and aInputB.pvValueInt64;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  TpvValueType.Integer:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64 and (ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64 and aInputB.pvValueInt64;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  else begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
 end;
end;

class operator TpvValue.BitwiseOr(const aInputA,aInputB:TpvValue):TpvValue;
begin
 case aInputA.pvValueType of
  TpvValueType.Boolean:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Boolean;
     result.pvValueBoolean:=aInputA.pvValueBoolean or aInputB.pvValueBoolean;
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=(ord(aInputA.pvValueBoolean) and 1) or aInputB.pvValueInt64;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  TpvValueType.Integer:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64 or (ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64 or aInputB.pvValueInt64;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  else begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
 end;
end;

class operator TpvValue.BitwiseXor(const aInputA,aInputB:TpvValue):TpvValue;
begin
 case aInputA.pvValueType of
  TpvValueType.Boolean:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Boolean;
     result.pvValueBoolean:=aInputA.pvValueBoolean xor aInputB.pvValueBoolean;
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=(ord(aInputA.pvValueBoolean) and 1) xor aInputB.pvValueInt64;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  TpvValueType.Integer:begin
   case aInputB.pvValueType of
    TpvValueType.Boolean:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64 xor (ord(aInputB.pvValueBoolean) and 1);
    end;
    TpvValueType.Integer:begin
     result.pvValueType:=TpvValueType.Integer;
     result.pvValueInt64:=aInputA.pvValueInt64 xor aInputB.pvValueInt64;
    end;
    else begin
     result.pvValueType:=TpvValueType.Unknown;
    end;
   end;
  end;
  else begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
 end;
end;

class operator TpvValue.Negative(const aInput:TpvValue):TpvValue;
begin
 case aInput.pvValueType of
  TpvValueType.Boolean:begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
  TpvValueType.Integer:begin
   result.pvValueType:=TpvValueType.Integer;
   result.pvValueInt64:=-aInput.pvValueInt64;
  end;
  TpvValueType.Double:begin
   result.pvValueType:=TpvValueType.Double;
   result.pvValueDouble:=-aInput.pvValueDouble;
  end;
  TpvValueType.Char:begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
  TpvValueType.String_:begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
  else begin
   result.pvValueType:=TpvValueType.Unknown;
  end;
 end;
end;

class operator TpvValue.Positive(const aInput:TpvValue):TpvValue;
begin
 result:=aInput;
end;

end.

