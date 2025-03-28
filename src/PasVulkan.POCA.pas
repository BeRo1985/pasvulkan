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
unit PasVulkan.POCA;
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
     PasMP,
     PasDblStrUtils,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Math.Double,
     POCA;

function POCANewVector2(const aContext:PPOCAContext;const aVector2:TpvVector2D):TPOCAValue; overload;
function POCANewVector2(const aContext:PPOCAContext;const aX:TpvDouble=0.0;const aY:TpvDouble=0.0):TPOCAValue; overload;
function POCAGetVector2Value(const aValue:TPOCAValue):TpvVector2D;

function POCANewVector3(const aContext:PPOCAContext;const aVector3:TpvVector3D):TPOCAValue; overload;
function POCANewVector3(const aContext:PPOCAContext;const aX:TpvDouble=0.0;const aY:TpvDouble=0.0;const aZ:TpvDouble=0.0):TPOCAValue; overload;
function POCAGetVector3Value(const aValue:TPOCAValue):TpvVector3D;

function POCANewVector4(const aContext:PPOCAContext;const aVector4:TpvVector4D):TPOCAValue; overload;
function POCANewVector4(const aContext:PPOCAContext;const aX:TpvDouble=0.0;const aY:TpvDouble=0.0;const aZ:TpvDouble=0.0;const aW:TpvDouble=0.0):TPOCAValue; overload;
function POCAGetVector4Value(const aValue:TPOCAValue):TpvVector4D;

function POCANewQuaternion(const aContext:PPOCAContext;const aQuaternion:TpvQuaternionD):TPOCAValue; overload;
function POCANewQuaternion(const aContext:PPOCAContext;const aX:TpvDouble=0.0;const aY:TpvDouble=0.0;const aZ:TpvDouble=0.0;const aW:TpvDouble=1.0):TPOCAValue; overload;
function POCAGetQuaternionValue(const aValue:TPOCAValue):TpvQuaternionD;

function POCANewMatrix3x3(const aContext:PPOCAContext;const aMatrix3x3:TpvMatrix3x3D):TPOCAValue; overload;
function POCANewMatrix3x3(const aContext:PPOCAContext;const aM00:TpvDouble=1.0;const aM01:TpvDouble=0.0;const aM02:TpvDouble=0.0;const aM10:TpvDouble=0.0;const aM11:TpvDouble=1.0;const aM12:TpvDouble=0.0;const aM20:TpvDouble=0.0;const aM21:TpvDouble=0.0;const aM22:TpvDouble=1.0):TPOCAValue; overload;
function POCAGetMatrix3x3Value(const aValue:TPOCAValue):TpvMatrix3x3D;

function POCANewMatrix4x4(const aContext:PPOCAContext;const aMatrix4x4:TpvMatrix4x4D):TPOCAValue; overload;
function POCANewMatrix4x4(const aContext:PPOCAContext;const aM00:TpvDouble=1.0;const aM01:TpvDouble=0.0;const aM02:TpvDouble=0.0;const aM03:TpvDouble=0.0;const aM10:TpvDouble=0.0;const aM11:TpvDouble=1.0;const aM12:TpvDouble=0.0;const aM13:TpvDouble=0.0;const aM20:TpvDouble=0.0;const aM21:TpvDouble=0.0;const aM22:TpvDouble=1.0;const aM23:TpvDouble=0.0;const aM30:TpvDouble=0.0;const aM31:TpvDouble=0.0;const aM32:TpvDouble=0.0;const aM33:TpvDouble=1.0):TPOCAValue; overload;
function POCAGetMatrix4x4Value(const aValue:TPOCAValue):TpvMatrix4x4D;

procedure InitializeForPOCAContext(const aContext:PPOCAContext);

implementation

// Pointers to the ghost types as forward declarations, for to avoid circular references and more complicated code
var POCAVector2GhostPointer:PPOCAGhostType=nil;
    POCAVector3GhostPointer:PPOCAGhostType=nil;
    POCAVector4GhostPointer:PPOCAGhostType=nil;
    POCAQuaternionGhostPointer:PPOCAGhostType=nil;
    POCAMatrix3x3GhostPointer:PPOCAGhostType=nil;
    POCAMatrix4x4GhostPointer:PPOCAGhostType=nil;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Vector2
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

var POCAVector2Hash,POCAVector2HashEvents:TPOCAValue;

procedure POCAVector2GhostDestroy(const aGhost:PPOCAGhost);
begin
 if assigned(aGhost) and assigned(aGhost^.Ptr) then begin
  FreeMem(aGhost^.Ptr);
 end;
end;

function POCAVector2GhostExistKey(const aContext:PPOCAContext;const aGhost:PPOCAGhost;const aKey:TPOCAValue):longbool;
var s:TpvUTF8String;
begin
 s:=POCAGetStringValue(aContext,aKey);
 if (s='x') or (s='r') then begin
  result:=true;
 end else if (s='y') or (s='g') then begin
  result:=true;
 end else if (s='z') or (s='b') then begin
  result:=true;
 end else begin
  result:=false;
 end;
end;

function POCAVector2GhostGetKey(const aContext:PPOCAContext;const aGhost:PPOCAGhost;const aKey:TPOCAValue;out aValue:TPOCAValue):longbool;
var Vector2:PpvVector2D;
    s:TpvUTF8String;
begin
 Vector2:=PpvVector2D(PPOCAGhost(aGhost)^.Ptr);
 s:=POCAGetStringValue(aContext,aKey);
 if (s='x') or (s='r') then begin
  aValue.Num:=Vector2^.x;
  result:=true;
 end else if (s='y') or (s='g') then begin
  aValue.Num:=Vector2^.y;
  result:=true;
 end else begin
  result:=false;
 end;
end;

function POCAVector2GhostSetKey(const aContext:PPOCAContext;const aGhost:PPOCAGhost;const aKey:TPOCAValue;const aValue:TPOCAValue):longbool;
var Vector2:PpvVector2D;
    s:TpvUTF8String;
begin
 Vector2:=PpvVector2D(PPOCAGhost(aGhost)^.Ptr);
 s:=POCAGetStringValue(aContext,aKey);
 if (s='x') or (s='r') then begin
  Vector2^.x:=POCAGetNumberValue(aContext,aValue);
  result:=true;
 end else if (s='y') or (s='g') then begin
  Vector2^.y:=POCAGetNumberValue(aContext,aValue);
  result:=true;
 end else begin
  result:=false;
 end;
end;

const POCAVector2Ghost:TPOCAGhostType=
       (
        Destroy:POCAVector2GhostDestroy;
        CanDestroy:nil;
        Mark:nil;
        ExistKey:POCAVector2GhostExistKey;
        GetKey:POCAVector2GhostGetKey;
        SetKey:POCAVector2GhostSetKey;
        Name:'Vector2'
       );

function POCANewVector2(const aContext:PPOCAContext;const aVector2:TpvVector2D):TPOCAValue; overload;
var Vector2:PpvVector2D;
begin
 Vector2:=nil;
 GetMem(Vector2,SizeOf(TpvVector2D));
 Vector2^:=aVector2;
 result:=POCANewGhost(aContext,@POCAVector2Ghost,Vector2,nil,pgptRAW);
 POCAGhostSetHashValue(result,POCAVector2Hash);
end;

function POCANewVector2(const aContext:PPOCAContext;const aX:TpvDouble;const aY:TpvDouble):TPOCAValue; overload;
begin
 result:=POCANewVector2(aContext,TpvVector2D.Create(aX,aY));
end;

function POCAGetVector2Value(const aValue:TPOCAValue):TpvVector2D;
begin
 if POCAGhostGetType(aValue)=@POCAVector2Ghost then begin
  result:=PpvVector2D(POCAGhostFastGetPointer(aValue))^;
 end else begin
  result:=TpvVector2D.Create(0.0,0.0);
 end;
end;

function POCAVector2FunctionCREATE(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2:TpvVector2D;
    Vector3:PpvVector3D;
    Vector4:PpvVector4D;
begin
 if (aCountArguments>0) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) then begin
  Vector2:=PpvVector2D(POCAGhostFastGetPointer(aArguments^[0]))^;
 end else if (aCountArguments>0) and (POCAGhostGetType(aArguments^[0])=POCAVector3GhostPointer) then begin
  Vector3:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector2.x:=Vector3^.x;
  Vector2.y:=Vector3^.y;
 end else if (aCountArguments>0) and (POCAGhostGetType(aArguments^[0])=POCAVector4GhostPointer) then begin
  Vector4:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector2.x:=Vector4^.x;
  Vector2.y:=Vector4^.y;
 end else begin
  if aCountArguments>0 then begin
   Vector2.x:=POCAGetNumberValue(aContext,aArguments^[0]);
  end else begin
   Vector2.x:=0.0;
  end;
  if aCountArguments>1 then begin
   Vector2.y:=POCAGetNumberValue(aContext,aArguments^[1]);
  end else begin
   Vector2.y:=0.0;
  end;
 end;
 result:=POCANewVector2(aContext,Vector2);
end;

function POCAVector2FunctionLength(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2:PpvVector2D;
begin
 if POCAGhostGetType(aThis)=@POCAVector2Ghost then begin
  Vector2:=POCAGhostFastGetPointer(aThis);
  result:=POCANewNumber(aContext,Vector2^.Length);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionSquaredLength(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2:PpvVector2D;
begin
 if POCAGhostGetType(aThis)=@POCAVector2Ghost then begin
  Vector2:=POCAGhostFastGetPointer(aThis);
  result:=POCANewNumber(aContext,Vector2^.SquaredLength);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionNormalize(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2:PpvVector2D;
begin
 if POCAGhostGetType(aThis)=@POCAVector2Ghost then begin
  Vector2:=POCAGhostFastGetPointer(aThis);
  result:=POCANewVector2(aContext,Vector2^.Normalize);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionDot(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2:PpvVector2D;
    OtherVector2:PpvVector2D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector2Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) then begin
  Vector2:=POCAGhostFastGetPointer(aThis);
  OtherVector2:=POCAGhostFastGetPointer(aArguments^[0]);
  result.Num:=Vector2^.Dot(OtherVector2^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionCross(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2:PpvVector2D;
    OtherVector2:PpvVector2D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector2Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) then begin
  Vector2:=POCAGhostFastGetPointer(aThis);
  OtherVector2:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewVector2(aContext,Vector2^.Cross(OtherVector2^));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionDistanceTo(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2:PpvVector2D;
    OtherVector2:PpvVector2D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector2Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) then begin
  Vector2:=POCAGhostFastGetPointer(aThis);
  OtherVector2:=POCAGhostFastGetPointer(aArguments^[0]);
  result.Num:=(Vector2^-OtherVector2^).Length;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionLerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2:PpvVector2D;
    OtherVector2:PpvVector2D;
    Time:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aThis)=@POCAVector2Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Vector2:=POCAGhostFastGetPointer(aThis);
  OtherVector2:=POCAGhostFastGetPointer(aArguments^[0]);
  Time:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewVector2(aContext,Vector2^.Lerp(OtherVector2^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionNlerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2:PpvVector2D;
    OtherVector2:PpvVector2D;
    Time:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aThis)=@POCAVector2Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Vector2:=POCAGhostFastGetPointer(aThis);
  OtherVector2:=POCAGhostFastGetPointer(aArguments^[0]);
  Time:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewVector2(aContext,Vector2^.Nlerp(OtherVector2^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionSlerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2:PpvVector2D;
    OtherVector2:PpvVector2D;
    Time:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aThis)=@POCAVector2Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Vector2:=POCAGhostFastGetPointer(aThis);
  OtherVector2:=POCAGhostFastGetPointer(aArguments^[0]);
  Time:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewVector2(aContext,Vector2^.Slerp(OtherVector2^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionSqlerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var A,B,C,D:PpvVector2D;
    Time:TpvDouble;
begin
 if (aCountArguments=4) and (POCAGhostGetType(aThis)=@POCAVector2Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAVector2Ghost) and (POCAGhostGetType(aArguments^[2])=@POCAVector2Ghost) and (POCAGetValueType(aArguments^[3])=pvtNUMBER) then begin
  A:=POCAGhostFastGetPointer(aThis);
  B:=POCAGhostFastGetPointer(aArguments^[0]);
  C:=POCAGhostFastGetPointer(aArguments^[1]);
  D:=POCAGhostFastGetPointer(aArguments^[2]);
  Time:=POCAGetNumberValue(aContext,aArguments^[3]);
  result:=POCANewVector2(aContext,A^.Sqlerp(B^,C^,D^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionAdd(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2:PpvVector2D;
    OtherVector2:PpvVector2D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector2Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) then begin
  Vector2:=POCAGhostFastGetPointer(aThis);
  OtherVector2:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector2^:=Vector2^+OtherVector2^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionSub(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2:PpvVector2D;
    OtherVector2:PpvVector2D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector2Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) then begin
  Vector2:=POCAGhostFastGetPointer(aThis);
  OtherVector2:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector2^:=Vector2^-OtherVector2^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionMul(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2,OtherVector2:PpvVector2D;
    Factor:TpvDouble;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector2Ghost) and (POCAGetValueType(aArguments^[0])=pvtNUMBER) then begin
  Vector2:=POCAGhostFastGetPointer(aThis);
  Factor:=POCAGetNumberValue(aContext,aArguments^[0]);
  Vector2^:=Vector2^*Factor;
  result:=aThis;
 end else if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector2Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) then begin
  Vector2:=POCAGhostFastGetPointer(aThis);
  OtherVector2:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector2^:=Vector2^*OtherVector2^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionDiv(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2,OtherVector2:PpvVector2D;
    Factor:TpvDouble;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector2Ghost) and (POCAGetValueType(aArguments^[0])=pvtNUMBER) then begin
  Vector2:=POCAGhostFastGetPointer(aThis);
  Factor:=POCAGetNumberValue(aContext,aArguments^[0]);
  Vector2^:=Vector2^/Factor;
  result:=aThis;
 end else if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector2Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) then begin
  Vector2:=POCAGhostFastGetPointer(aThis);
  OtherVector2:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector2^:=Vector2^/OtherVector2^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionNeg(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2:PpvVector2D;
begin
 if (aCountArguments=0) and (POCAGhostGetType(aThis)=@POCAVector2Ghost) then begin
  Vector2:=POCAGhostFastGetPointer(aThis);
  Vector2^:=-Vector2^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2,OtherVector2:PpvVector2D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector2Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) then begin
  Vector2:=POCAGhostFastGetPointer(aThis);
  OtherVector2:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewNumber(aContext,ord(Vector2^=OtherVector2^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionNotEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2,OtherVector2:PpvVector2D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector2Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) then begin
  Vector2:=POCAGhostFastGetPointer(aThis);
  OtherVector2:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewNumber(aContext,ord(Vector2^<>OtherVector2^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionToString(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2:PpvVector2D;
    s:TpvUTF8String;
begin
 if (aCountArguments=0) and (POCAGhostGetType(aThis)=@POCAVector2Ghost) then begin
  Vector2:=POCAGhostFastGetPointer(aThis);
  s:='['+ConvertDoubleToString(Vector2^.x,omStandard,-1)+','+ConvertDoubleToString(Vector2^.y,omStandard,-1)+']';
  result:=POCANewString(aContext,s);
 end else begin
  result:=POCAValueNull;
 end;
end;

// "THIS" is null, because it is a binary operator, so the first argument is the first operand and the second argument is the second operand
function POCAVector2FunctionOpAdd(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2,OtherVector2:PpvVector2D;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAVector2Ghost) then begin
  Vector2:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherVector2:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewVector2(aContext,Vector2^+OtherVector2^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionOpSub(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2,OtherVector2:PpvVector2D;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAVector2Ghost) then begin
  Vector2:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherVector2:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewVector2(aContext,Vector2^-OtherVector2^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionOpMul(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2,OtherVector2:PpvVector2D;
    Factor:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Vector2:=POCAGhostFastGetPointer(aArguments^[0]);
  Factor:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewVector2(aContext,Vector2^*Factor);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAVector2Ghost) then begin
  Vector2:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherVector2:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewVector2(aContext,Vector2^*OtherVector2^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionOpDiv(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2,OtherVector2:PpvVector2D;
    Factor:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Vector2:=POCAGhostFastGetPointer(aArguments^[0]);
  Factor:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewVector2(aContext,Vector2^/Factor);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAVector2Ghost) then begin
  Vector2:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherVector2:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewVector2(aContext,Vector2^/OtherVector2^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionOpEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2,OtherVector2:PpvVector2D;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAVector2Ghost) then begin
  Vector2:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherVector2:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewNumber(aContext,ord(Vector2^=OtherVector2^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionOpNotEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2,OtherVector2:PpvVector2D;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAVector2Ghost) then begin
  Vector2:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherVector2:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewNumber(aContext,ord(Vector2^<>OtherVector2^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionOpNeg(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2:PpvVector2D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) then begin
  Vector2:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewVector2(aContext,-Vector2^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionOpSqrt(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2:PpvVector2D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) then begin
  Vector2:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewVector2(aContext,Sqrt(Vector2^.x),Sqrt(Vector2^.y));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector2FunctionOpToString(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector2:PpvVector2D;
    s:TpvUTF8String;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAVector2Ghost) then begin
  Vector2:=POCAGhostFastGetPointer(aArguments^[0]);
  s:='['+ConvertDoubleToString(Vector2^.x,omStandard,-1)+','+ConvertDoubleToString(Vector2^.y,omStandard,-1)+']';
  result:=POCANewString(aContext,s);
 end else begin
  result:=POCAValueNull;
 end;
end;

procedure POCAInitVector2Hash(aContext:PPOCAContext);
begin

 POCAVector2GhostPointer:=@POCAVector2Ghost;

 POCAVector2Hash:=POCANewHash(aContext);
 POCAArrayPush(aContext^.Instance^.Globals.RootArray,POCAVector2Hash);
 POCAAddNativeFunction(aContext,POCAVector2Hash,'length',POCAVector2FunctionLength);
 POCAAddNativeFunction(aContext,POCAVector2Hash,'squaredLength',POCAVector2FunctionSquaredLength);
 POCAAddNativeFunction(aContext,POCAVector2Hash,'normalize',POCAVector2FunctionNormalize);
 POCAAddNativeFunction(aContext,POCAVector2Hash,'dot',POCAVector2FunctionDot);
 POCAAddNativeFunction(aContext,POCAVector2Hash,'cross',POCAVector2FunctionCross);
 POCAAddNativeFunction(aContext,POCAVector2Hash,'distanceTo',POCAVector2FunctionDistanceTo);
 POCAAddNativeFunction(aContext,POCAVector2Hash,'lerp',POCAVector2FunctionLerp);
 POCAAddNativeFunction(aContext,POCAVector2Hash,'nlerp',POCAVector2FunctionNlerp);
 POCAAddNativeFunction(aContext,POCAVector2Hash,'slerp',POCAVector2FunctionSlerp);
 POCAAddNativeFunction(aContext,POCAVector2Hash,'sqlerp',POCAVector2FunctionSqlerp);
 POCAAddNativeFunction(aContext,POCAVector2Hash,'add',POCAVector2FunctionAdd);
 POCAAddNativeFunction(aContext,POCAVector2Hash,'sub',POCAVector2FunctionSub);
 POCAAddNativeFunction(aContext,POCAVector2Hash,'mul',POCAVector2FunctionMul);
 POCAAddNativeFunction(aContext,POCAVector2Hash,'div',POCAVector2FunctionDiv);
 POCAAddNativeFunction(aContext,POCAVector2Hash,'neg',POCAVector2FunctionNeg);
 POCAAddNativeFunction(aContext,POCAVector2Hash,'equal',POCAVector2FunctionEqual);
 POCAAddNativeFunction(aContext,POCAVector2Hash,'notEqual',POCAVector2FunctionNotEqual);
 POCAAddNativeFunction(aContext,POCAVector2Hash,'toString',POCAVector2FunctionToString);

 POCAVector2HashEvents:=POCANewHash(aContext);
 POCAArrayPush(aContext^.Instance^.Globals.RootArray,POCAVector2HashEvents);
 POCAAddNativeFunction(aContext,POCAVector2HashEvents,'__add',POCAVector2FunctionOpAdd);
 POCAAddNativeFunction(aContext,POCAVector2HashEvents,'__sub',POCAVector2FunctionOpSub);
 POCAAddNativeFunction(aContext,POCAVector2HashEvents,'__mul',POCAVector2FunctionOpMul);
 POCAAddNativeFunction(aContext,POCAVector2HashEvents,'__div',POCAVector2FunctionOpDiv);
 POCAAddNativeFunction(aContext,POCAVector2HashEvents,'__eq',POCAVector2FunctionOpEqual);
 POCAAddNativeFunction(aContext,POCAVector2HashEvents,'__neq',POCAVector2FunctionOpNotEqual);
 POCAAddNativeFunction(aContext,POCAVector2HashEvents,'__neg',POCAVector2FunctionOpNeg);
 POCAAddNativeFunction(aContext,POCAVector2HashEvents,'__sqrt',POCAVector2FunctionOpSqrt);
 POCAAddNativeFunction(aContext,POCAVector2HashEvents,'__tostring',POCAVector2FunctionOpToString);

 POCAHashSetHashEvents(aContext,POCAVector2Hash,POCAVector2HashEvents);

end;

procedure POCAInitVector2Namespace(aContext:PPOCAContext);
var Hash:TPOCAValue;
begin
 Hash:=POCANewHash(aContext);
 POCAArrayPush(aContext^.Instance^.Globals.RootArray,Hash);
 POCAAddNativeFunction(aContext,Hash,'create',POCAVector2FunctionCREATE);
 POCAHashSetString(aContext,aContext^.Instance^.Globals.Namespace,'Vector2',Hash);
end;

procedure POCAInitVector2(aContext:PPOCAContext);
begin
 POCAInitVector2Hash(aContext);
 POCAInitVector2Namespace(aContext);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Vector3
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

var POCAVector3Hash,POCAVector3HashEvents:TPOCAValue;

procedure POCAVector3GhostDestroy(const aGhost:PPOCAGhost);
begin
 if assigned(aGhost) and assigned(aGhost^.Ptr) then begin
  FreeMem(aGhost^.Ptr);
 end;
end;

function POCAVector3GhostExistKey(const aContext:PPOCAContext;const aGhost:PPOCAGhost;const aKey:TPOCAValue):longbool;
var s:TpvUTF8String;
begin
 s:=POCAGetStringValue(aContext,aKey);
 if (s='x') or (s='r') then begin
  result:=true;
 end else if (s='y') or (s='g') then begin
  result:=true;
 end else if (s='z') or (s='b') then begin
  result:=true;
 end else begin
  result:=false;
 end;
end;

function POCAVector3GhostGetKey(const aContext:PPOCAContext;const aGhost:PPOCAGhost;const aKey:TPOCAValue;out aValue:TPOCAValue):longbool;
var Vector3:PpvVector3D;
    s:TpvUTF8String;
begin
 Vector3:=PpvVector3D(PPOCAGhost(aGhost)^.Ptr);
 s:=POCAGetStringValue(aContext,aKey);
 if (s='x') or (s='r') then begin
  aValue.Num:=Vector3^.x;
  result:=true;
 end else if (s='y') or (s='g') then begin
  aValue.Num:=Vector3^.y;
  result:=true;
 end else if (s='z') or (s='b') then begin
  aValue.Num:=Vector3^.z;
  result:=true;
 end else begin
  result:=false;
 end;
end;

function POCAVector3GhostSetKey(const aContext:PPOCAContext;const aGhost:PPOCAGhost;const aKey:TPOCAValue;const aValue:TPOCAValue):longbool;
var Vector3:PpvVector3D;
    s:TpvUTF8String;
begin
 Vector3:=PpvVector3D(PPOCAGhost(aGhost)^.Ptr);
 s:=POCAGetStringValue(aContext,aKey);
 if (s='x') or (s='r') then begin
  Vector3^.x:=POCAGetNumberValue(aContext,aValue);
  result:=true;
 end else if (s='y') or (s='g') then begin
  Vector3^.y:=POCAGetNumberValue(aContext,aValue);
  result:=true;
 end else if (s='z') or (s='b') then begin
  Vector3^.z:=POCAGetNumberValue(aContext,aValue);
  result:=true;
 end else begin
  result:=false;
 end;
end;

const POCAVector3Ghost:TPOCAGhostType=
       (
        Destroy:POCAVector3GhostDestroy;
        CanDestroy:nil;
        Mark:nil;
        ExistKey:POCAVector3GhostExistKey;
        GetKey:POCAVector3GhostGetKey;
        SetKey:POCAVector3GhostSetKey;
        Name:'Vector3'
       );

function POCANewVector3(const aContext:PPOCAContext;const aVector3:TpvVector3D):TPOCAValue; overload;
var Vector3:PpvVector3D;
begin
 Vector3:=nil;
 GetMem(Vector3,SizeOf(TpvVector3D));
 Vector3^:=aVector3;
 result:=POCANewGhost(aContext,@POCAVector3Ghost,Vector3,nil,pgptRAW);
 POCAGhostSetHashValue(result,POCAVector3Hash);
end;

function POCANewVector3(const aContext:PPOCAContext;const aX:TpvDouble;const aY:TpvDouble;const aZ:TpvDouble):TPOCAValue; overload;
begin
 result:=POCANewVector3(aContext,TpvVector3D.Create(aX,aY,aZ));
end;

function POCAGetVector3Value(const aValue:TPOCAValue):TpvVector3D;
begin
 if POCAGhostGetType(aValue)=@POCAVector3Ghost then begin
  result:=PpvVector3D(POCAGhostFastGetPointer(aValue))^;
 end else begin
  result:=TpvVector3D.Create(0.0,0.0,0.0);
 end;
end;

function POCAVector3FunctionCREATE(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3:TpvVector3D;
    Vector2:PpvVector2D;
    Vector4:PpvVector4D;
begin
 if (aCountArguments>0) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) then begin
  Vector3:=PpvVector3D(POCAGhostFastGetPointer(aArguments^[0]))^;
 end else if (aCountArguments>0) and (POCAGhostGetType(aArguments^[0])=POCAVector2GhostPointer) then begin
  Vector2:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector3.x:=Vector2^.x;
  Vector3.y:=Vector2^.y;
  if aCountArguments>1 then begin
   Vector3.z:=POCAGetNumberValue(aContext,aArguments^[1]);
  end else begin
   Vector3.z:=0.0;
  end;
 end else if (aCountArguments>0) and (POCAGhostGetType(aArguments^[0])=POCAVector4GhostPointer) then begin
  Vector4:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector3:=Vector4^.xyz;
 end else begin
  if aCountArguments>0 then begin
   Vector3.x:=POCAGetNumberValue(aContext,aArguments^[0]);
  end else begin
   Vector3.x:=0.0;
  end;
  if aCountArguments>1 then begin
   Vector3.y:=POCAGetNumberValue(aContext,aArguments^[1]);
  end else begin
   Vector3.y:=0.0;
  end;
  if aCountArguments>2 then begin
   Vector3.z:=POCAGetNumberValue(aContext,aArguments^[2]);
  end else begin
   Vector3.z:=0.0;
  end;
 end;
 result:=POCANewVector3(aContext,Vector3);
end;

function POCAVector3FunctionLength(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3:PpvVector3D;
begin
 if POCAGhostGetType(aThis)=@POCAVector3Ghost then begin
  Vector3:=POCAGhostFastGetPointer(aThis);
  result:=POCANewNumber(aContext,Vector3^.Length);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionSquaredLength(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3:PpvVector3D;
begin
 if POCAGhostGetType(aThis)=@POCAVector3Ghost then begin
  Vector3:=POCAGhostFastGetPointer(aThis);
  result:=POCANewNumber(aContext,Vector3^.SquaredLength);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionNormalize(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3:PpvVector3D;
begin
 if POCAGhostGetType(aThis)=@POCAVector3Ghost then begin
  Vector3:=POCAGhostFastGetPointer(aThis);
  result:=POCANewVector3(aContext,Vector3^.Normalize);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionDot(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3:PpvVector3D;
    OtherVector3:PpvVector3D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) then begin
  Vector3:=POCAGhostFastGetPointer(aThis);
  OtherVector3:=POCAGhostFastGetPointer(aArguments^[0]);
  result.Num:=Vector3^.Dot(OtherVector3^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionCross(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3:PpvVector3D;
    OtherVector3:PpvVector3D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) then begin
  Vector3:=POCAGhostFastGetPointer(aThis);
  OtherVector3:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewVector3(aContext,Vector3^.Cross(OtherVector3^));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionDistanceTo(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3:PpvVector3D;
    OtherVector3:PpvVector3D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) then begin
  Vector3:=POCAGhostFastGetPointer(aThis);
  OtherVector3:=POCAGhostFastGetPointer(aArguments^[0]);
  result.Num:=(Vector3^-OtherVector3^).Length;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionLerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3:PpvVector3D;
    OtherVector3:PpvVector3D;
    Time:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aThis)=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Vector3:=POCAGhostFastGetPointer(aThis);
  OtherVector3:=POCAGhostFastGetPointer(aArguments^[0]);
  Time:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewVector3(aContext,Vector3^.Lerp(OtherVector3^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionNlerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3:PpvVector3D;
    OtherVector3:PpvVector3D;
    Time:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aThis)=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Vector3:=POCAGhostFastGetPointer(aThis);
  OtherVector3:=POCAGhostFastGetPointer(aArguments^[0]);
  Time:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewVector3(aContext,Vector3^.Nlerp(OtherVector3^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionSlerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3:PpvVector3D;
    OtherVector3:PpvVector3D;
    Time:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aThis)=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Vector3:=POCAGhostFastGetPointer(aThis);
  OtherVector3:=POCAGhostFastGetPointer(aArguments^[0]);
  Time:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewVector3(aContext,Vector3^.Slerp(OtherVector3^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionSqlerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var A,B,C,D:PpvVector3D;
    Time:TpvDouble;
begin
 if (aCountArguments=4) and (POCAGhostGetType(aThis)=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[2])=@POCAVector3Ghost) and (POCAGetValueType(aArguments^[3])=pvtNUMBER) then begin
  A:=POCAGhostFastGetPointer(aThis);
  B:=POCAGhostFastGetPointer(aArguments^[0]);
  C:=POCAGhostFastGetPointer(aArguments^[1]);
  D:=POCAGhostFastGetPointer(aArguments^[2]);
  Time:=POCAGetNumberValue(aContext,aArguments^[3]);
  result:=POCANewVector3(aContext,A^.Sqlerp(B^,C^,D^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionAdd(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3:PpvVector3D;
    OtherVector3:PpvVector3D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) then begin
  Vector3:=POCAGhostFastGetPointer(aThis);
  OtherVector3:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector3^:=Vector3^+OtherVector3^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionSub(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3:PpvVector3D;
    OtherVector3:PpvVector3D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) then begin
  Vector3:=POCAGhostFastGetPointer(aThis);
  OtherVector3:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector3^:=Vector3^-OtherVector3^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionMul(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3,OtherVector3:PpvVector3D;
    Factor:TpvDouble;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector3Ghost) and (POCAGetValueType(aArguments^[0])=pvtNUMBER) then begin
  Vector3:=POCAGhostFastGetPointer(aThis);
  Factor:=POCAGetNumberValue(aContext,aArguments^[0]);
  Vector3^:=Vector3^*Factor;
  result:=aThis;
 end else if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) then begin
  Vector3:=POCAGhostFastGetPointer(aThis);
  OtherVector3:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector3^:=Vector3^*OtherVector3^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionDiv(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3,OtherVector3:PpvVector3D;
    Factor:TpvDouble;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector3Ghost) and (POCAGetValueType(aArguments^[0])=pvtNUMBER) then begin
  Vector3:=POCAGhostFastGetPointer(aThis);
  Factor:=POCAGetNumberValue(aContext,aArguments^[0]);
  Vector3^:=Vector3^/Factor;
  result:=aThis;
 end else if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) then begin
  Vector3:=POCAGhostFastGetPointer(aThis);
  OtherVector3:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector3^:=Vector3^/OtherVector3^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionNeg(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3:PpvVector3D;
begin
 if (aCountArguments=0) and (POCAGhostGetType(aThis)=@POCAVector3Ghost) then begin
  Vector3:=POCAGhostFastGetPointer(aThis);
  Vector3^:=-Vector3^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3,OtherVector3:PpvVector3D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) then begin
  Vector3:=POCAGhostFastGetPointer(aThis);
  OtherVector3:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewNumber(aContext,ord(Vector3^=OtherVector3^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionNotEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3,OtherVector3:PpvVector3D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) then begin
  Vector3:=POCAGhostFastGetPointer(aThis);
  OtherVector3:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewNumber(aContext,ord(Vector3^<>OtherVector3^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionToString(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3:PpvVector3D;
    s:TpvUTF8String;
begin
 if (aCountArguments=0) and (POCAGhostGetType(aThis)=@POCAVector3Ghost) then begin
  Vector3:=POCAGhostFastGetPointer(aThis);
  s:='['+ConvertDoubleToString(Vector3^.x,omStandard,-1)+','+ConvertDoubleToString(Vector3^.y,omStandard,-1)+','+ConvertDoubleToString(Vector3^.z,omStandard,-1)+']';
  result:=POCANewString(aContext,s);
 end else begin
  result:=POCAValueNull;
 end;
end;

// "THIS" is null, because it is a binary operator, so the first argument is the first operand and the second argument is the second operand
function POCAVector3FunctionOpAdd(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3,OtherVector3:PpvVector3D;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAVector3Ghost) then begin
  Vector3:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherVector3:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewVector3(aContext,Vector3^+OtherVector3^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionOpSub(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3,OtherVector3:PpvVector3D;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAVector3Ghost) then begin
  Vector3:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherVector3:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewVector3(aContext,Vector3^-OtherVector3^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionOpMul(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3,OtherVector3:PpvVector3D;
    Factor:TpvDouble;
    Quaternion:PpvQuaternionD;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Vector3:=POCAGhostFastGetPointer(aArguments^[0]);
  Factor:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewVector3(aContext,Vector3^*Factor);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAVector3Ghost) then begin
  Vector3:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherVector3:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewVector3(aContext,Vector3^*OtherVector3^);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[1])=POCAQuaternionGhostPointer) then begin
  Vector3:=POCAGhostFastGetPointer(aArguments^[0]);
  Quaternion:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewVector3(aContext,Vector3^*Quaternion^);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=POCAQuaternionGhostPointer) and (POCAGhostGetType(aArguments^[1])=@POCAVector3Ghost) then begin
  Quaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector3:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewVector3(aContext,Quaternion^*Vector3^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionOpDiv(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3,OtherVector3:PpvVector3D;
    Factor:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Vector3:=POCAGhostFastGetPointer(aArguments^[0]);
  Factor:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewVector3(aContext,Vector3^/Factor);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAVector3Ghost) then begin
  Vector3:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherVector3:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewVector3(aContext,Vector3^/OtherVector3^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionOpEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3,OtherVector3:PpvVector3D;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAVector3Ghost) then begin
  Vector3:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherVector3:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewNumber(aContext,ord(Vector3^=OtherVector3^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionOpNotEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3,OtherVector3:PpvVector3D;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAVector3Ghost) then begin
  Vector3:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherVector3:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewNumber(aContext,ord(Vector3^<>OtherVector3^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionOpNeg(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3:PpvVector3D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) then begin
  Vector3:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewVector3(aContext,-Vector3^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionOpSqrt(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3:PpvVector3D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) then begin
  Vector3:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewVector3(aContext,Sqrt(Vector3^.x),Sqrt(Vector3^.y),Sqrt(Vector3^.z));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector3FunctionOpToString(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector3:PpvVector3D;
    s:TpvUTF8String;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAVector3Ghost) then begin
  Vector3:=POCAGhostFastGetPointer(aArguments^[0]);
  s:='['+ConvertDoubleToString(Vector3^.x,omStandard,-1)+','+ConvertDoubleToString(Vector3^.y,omStandard,-1)+','+ConvertDoubleToString(Vector3^.z,omStandard,-1)+']';
  result:=POCANewString(aContext,s);
 end else begin
  result:=POCAValueNull;
 end;
end;

procedure POCAInitVector3Hash(aContext:PPOCAContext);
begin

 POCAVector3GhostPointer:=@POCAVector3Ghost;

 POCAVector3Hash:=POCANewHash(aContext);
 POCAArrayPush(aContext^.Instance^.Globals.RootArray,POCAVector3Hash);
 POCAAddNativeFunction(aContext,POCAVector3Hash,'length',POCAVector3FunctionLength);
 POCAAddNativeFunction(aContext,POCAVector3Hash,'squaredLength',POCAVector3FunctionSquaredLength);
 POCAAddNativeFunction(aContext,POCAVector3Hash,'normalize',POCAVector3FunctionNormalize);
 POCAAddNativeFunction(aContext,POCAVector3Hash,'dot',POCAVector3FunctionDot);
 POCAAddNativeFunction(aContext,POCAVector3Hash,'cross',POCAVector3FunctionCross);
 POCAAddNativeFunction(aContext,POCAVector3Hash,'distanceTo',POCAVector3FunctionDistanceTo);
 POCAAddNativeFunction(aContext,POCAVector3Hash,'lerp',POCAVector3FunctionLerp);
 POCAAddNativeFunction(aContext,POCAVector3Hash,'nlerp',POCAVector3FunctionNlerp);
 POCAAddNativeFunction(aContext,POCAVector3Hash,'slerp',POCAVector3FunctionSlerp);
 POCAAddNativeFunction(aContext,POCAVector3Hash,'sqlerp',POCAVector3FunctionSqlerp);
 POCAAddNativeFunction(aContext,POCAVector3Hash,'add',POCAVector3FunctionAdd);
 POCAAddNativeFunction(aContext,POCAVector3Hash,'sub',POCAVector3FunctionSub);
 POCAAddNativeFunction(aContext,POCAVector3Hash,'mul',POCAVector3FunctionMul);
 POCAAddNativeFunction(aContext,POCAVector3Hash,'div',POCAVector3FunctionDiv);
 POCAAddNativeFunction(aContext,POCAVector3Hash,'neg',POCAVector3FunctionNeg);
 POCAAddNativeFunction(aContext,POCAVector3Hash,'equal',POCAVector3FunctionEqual);
 POCAAddNativeFunction(aContext,POCAVector3Hash,'notEqual',POCAVector3FunctionNotEqual);
 POCAAddNativeFunction(aContext,POCAVector3Hash,'toString',POCAVector3FunctionToString);

 POCAVector3HashEvents:=POCANewHash(aContext);
 POCAArrayPush(aContext^.Instance^.Globals.RootArray,POCAVector3HashEvents);
 POCAAddNativeFunction(aContext,POCAVector3HashEvents,'__add',POCAVector3FunctionOpAdd);
 POCAAddNativeFunction(aContext,POCAVector3HashEvents,'__sub',POCAVector3FunctionOpSub);
 POCAAddNativeFunction(aContext,POCAVector3HashEvents,'__mul',POCAVector3FunctionOpMul);
 POCAAddNativeFunction(aContext,POCAVector3HashEvents,'__div',POCAVector3FunctionOpDiv);
 POCAAddNativeFunction(aContext,POCAVector3HashEvents,'__eq',POCAVector3FunctionOpEqual);
 POCAAddNativeFunction(aContext,POCAVector3HashEvents,'__neq',POCAVector3FunctionOpNotEqual);
 POCAAddNativeFunction(aContext,POCAVector3HashEvents,'__neg',POCAVector3FunctionOpNeg);
 POCAAddNativeFunction(aContext,POCAVector3HashEvents,'__sqrt',POCAVector3FunctionOpSqrt);
 POCAAddNativeFunction(aContext,POCAVector3HashEvents,'__tostring',POCAVector3FunctionOpToString);

 POCAHashSetHashEvents(aContext,POCAVector3Hash,POCAVector3HashEvents);

end;

procedure POCAInitVector3Namespace(aContext:PPOCAContext);
var Hash:TPOCAValue;
begin
 Hash:=POCANewHash(aContext);
 POCAArrayPush(aContext^.Instance^.Globals.RootArray,Hash);
 POCAAddNativeFunction(aContext,Hash,'create',POCAVector3FunctionCREATE);
 POCAHashSetString(aContext,aContext^.Instance^.Globals.Namespace,'Vector3',Hash);
end;

procedure POCAInitVector3(aContext:PPOCAContext);
begin
 POCAInitVector3Hash(aContext);
 POCAInitVector3Namespace(aContext);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Vector4
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

var POCAVector4Hash,POCAVector4HashEvents:TPOCAValue;

procedure POCAVector4GhostDestroy(const aGhost:PPOCAGhost);
begin
 if assigned(aGhost) and assigned(aGhost^.Ptr) then begin
  FreeMem(aGhost^.Ptr);
 end;
end;

function POCAVector4GhostExistKey(const aContext:PPOCAContext;const aGhost:PPOCAGhost;const aKey:TPOCAValue):longbool;
var s:TpvUTF8String;
begin
 s:=POCAGetStringValue(aContext,aKey);
 if (s='x') or (s='r') then begin
  result:=true;
 end else if (s='y') or (s='g') then begin
  result:=true;
 end else if (s='z') or (s='b') then begin
  result:=true;
 end else if (s='w') or (s='a') then begin
  result:=true;
 end else begin
  result:=false;
 end;
end;

function POCAVector4GhostGetKey(const aContext:PPOCAContext;const aGhost:PPOCAGhost;const aKey:TPOCAValue;out aValue:TPOCAValue):longbool;
var Vector4:PpvVector4D;
    s:TpvUTF8String;
begin
 Vector4:=PpvVector4D(PPOCAGhost(aGhost)^.Ptr);
 s:=POCAGetStringValue(aContext,aKey);
 if (s='x') or (s='r') then begin
  aValue.Num:=Vector4^.x;
  result:=true;
 end else if (s='y') or (s='g') then begin
  aValue.Num:=Vector4^.y;
  result:=true;
 end else if (s='z') or (s='b') then begin
  aValue.Num:=Vector4^.z;
  result:=true;
 end else if (s='w') or (s='a') then begin
  aValue.Num:=Vector4^.w;
  result:=true;
 end else begin
  result:=false;
 end;
end;

function POCAVector4GhostSetKey(const aContext:PPOCAContext;const aGhost:PPOCAGhost;const aKey:TPOCAValue;const aValue:TPOCAValue):longbool;
var Vector4:PpvVector4D;
    s:TpvUTF8String;
begin
 Vector4:=PpvVector4D(PPOCAGhost(aGhost)^.Ptr);
 s:=POCAGetStringValue(aContext,aKey);
 if (s='x') or (s='r') then begin
  Vector4^.x:=POCAGetNumberValue(aContext,aValue);
  result:=true;
 end else if (s='y') or (s='g') then begin
  Vector4^.y:=POCAGetNumberValue(aContext,aValue);
  result:=true;
 end else if (s='z') or (s='b') then begin
  Vector4^.z:=POCAGetNumberValue(aContext,aValue);
  result:=true;
 end else if (s='w') or (s='a') then begin
  Vector4^.w:=POCAGetNumberValue(aContext,aValue);
  result:=true;
 end else begin
  result:=false;
 end;
end;

const POCAVector4Ghost:TPOCAGhostType=
       (
        Destroy:POCAVector4GhostDestroy;
        CanDestroy:nil;
        Mark:nil;
        ExistKey:POCAVector4GhostExistKey;
        GetKey:POCAVector4GhostGetKey;
        SetKey:POCAVector4GhostSetKey;
        Name:'Vector4'
       );

function POCANewVector4(const aContext:PPOCAContext;const aVector4:TpvVector4D):TPOCAValue; overload;
var Vector4:PpvVector4D;
begin
 Vector4:=nil;
 GetMem(Vector4,SizeOf(TpvVector4D));
 Vector4^:=aVector4;
 result:=POCANewGhost(aContext,@POCAVector4Ghost,Vector4,nil,pgptRAW);
 POCAGhostSetHashValue(result,POCAVector4Hash);
end;

function POCANewVector4(const aContext:PPOCAContext;const aX:TpvDouble;const aY:TpvDouble;const aZ:TpvDouble;const aW:TpvDouble):TPOCAValue; overload;
begin
 result:=POCANewVector4(aContext,TpvVector4D.Create(aX,aY,aZ,aW));
end;

function POCAGetVector4Value(const aValue:TPOCAValue):TpvVector4D;
begin
 if POCAGhostGetType(aValue)=@POCAVector4Ghost then begin
  result:=PpvVector4D(POCAGhostFastGetPointer(aValue))^;
 end else begin
  result:=TpvVector4D.Create(0.0,0.0,0.0,0.0);
 end;
end;

function POCAVector4FunctionCREATE(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4:TpvVector4D;
    Vector2:PpvVector2D;
    Vector3:PpvVector3D;
begin
 if (aCountArguments>0) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) then begin
  Vector4:=PpvVector4D(POCAGhostFastGetPointer(aArguments^[0]))^;
 end else if (aCountArguments>0) and (POCAGhostGetType(aArguments^[0])=POCAVector2GhostPointer) then begin
  Vector2:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector4.x:=Vector2^.x;
  Vector4.y:=Vector2^.y;
  if aCountArguments>1 then begin
   Vector4.z:=POCAGetNumberValue(aContext,aArguments^[1]);
  end else begin
   Vector4.z:=0.0;
  end;
  if aCountArguments>2 then begin
   Vector4.w:=POCAGetNumberValue(aContext,aArguments^[2]);
  end else begin
   Vector4.w:=0.0;
  end;
 end else if (aCountArguments>0) and (POCAGhostGetType(aArguments^[0])=POCAVector3GhostPointer) then begin
  Vector3:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector4.xyz:=Vector3^;
  if aCountArguments>1 then begin
   Vector4.w:=POCAGetNumberValue(aContext,aArguments^[1]);
  end else begin
   Vector4.w:=0.0;
  end;
 end else begin
  if aCountArguments>0 then begin
   Vector4.x:=POCAGetNumberValue(aContext,aArguments^[0]);
  end else begin
   Vector4.x:=0.0;
  end;
  if aCountArguments>1 then begin
   Vector4.y:=POCAGetNumberValue(aContext,aArguments^[1]);
  end else begin
   Vector4.y:=0.0;
  end;
  if aCountArguments>2 then begin
   Vector4.z:=POCAGetNumberValue(aContext,aArguments^[2]);
  end else begin
   Vector4.z:=0.0;
  end;
  if aCountArguments>3 then begin
   Vector4.w:=POCAGetNumberValue(aContext,aArguments^[3]);
  end else begin
   Vector4.w:=0.0;
  end;
 end;
 result:=POCANewVector4(aContext,Vector4);
end;

function POCAVector4FunctionLength(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4:PpvVector4D;
begin
 if POCAGhostGetType(aThis)=@POCAVector4Ghost then begin
  Vector4:=POCAGhostFastGetPointer(aThis);
  result:=POCANewNumber(aContext,Vector4^.Length);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionSquaredLength(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4:PpvVector4D;
begin
 if POCAGhostGetType(aThis)=@POCAVector4Ghost then begin
  Vector4:=POCAGhostFastGetPointer(aThis);
  result:=POCANewNumber(aContext,Vector4^.SquaredLength);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionNormalize(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4:PpvVector4D;
begin
 if POCAGhostGetType(aThis)=@POCAVector4Ghost then begin
  Vector4:=POCAGhostFastGetPointer(aThis);
  result:=POCANewVector4(aContext,Vector4^.Normalize);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionDot(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4:PpvVector4D;
    OtherVector4:PpvVector4D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) then begin
  Vector4:=POCAGhostFastGetPointer(aThis);
  OtherVector4:=POCAGhostFastGetPointer(aArguments^[0]);
  result.Num:=Vector4^.Dot(OtherVector4^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionCross(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4:PpvVector4D;
    OtherVector4:PpvVector4D;
    Temporary:TpvVector4D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) then begin
  Vector4:=POCAGhostFastGetPointer(aThis);
  OtherVector4:=POCAGhostFastGetPointer(aArguments^[0]);
  Temporary.xyz:=Vector4^.xyz.Cross(OtherVector4^.xyz);
  Temporary.w:=1.0;
  result:=POCANewVector4(aContext,Temporary);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionDistanceTo(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4:PpvVector4D;
    OtherVector4:PpvVector4D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) then begin
  Vector4:=POCAGhostFastGetPointer(aThis);
  OtherVector4:=POCAGhostFastGetPointer(aArguments^[0]);
  result.Num:=(Vector4^-OtherVector4^).Length;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionLerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4:PpvVector4D;
    OtherVector4:PpvVector4D;
    Time:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aThis)=@POCAVector4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Vector4:=POCAGhostFastGetPointer(aThis);
  OtherVector4:=POCAGhostFastGetPointer(aArguments^[0]);
  Time:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewVector4(aContext,Vector4^.Lerp(OtherVector4^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionNlerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4:PpvVector4D;
    OtherVector4:PpvVector4D;
    Time:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aThis)=@POCAVector4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Vector4:=POCAGhostFastGetPointer(aThis);
  OtherVector4:=POCAGhostFastGetPointer(aArguments^[0]);
  Time:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewVector4(aContext,Vector4^.Nlerp(OtherVector4^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionSlerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4:PpvVector4D;
    OtherVector4:PpvVector4D;
    Time:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aThis)=@POCAVector4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Vector4:=POCAGhostFastGetPointer(aThis);
  OtherVector4:=POCAGhostFastGetPointer(aArguments^[0]);
  Time:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewVector4(aContext,Vector4^.Slerp(OtherVector4^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionSqlerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var A,B,C,D:PpvVector4D;
    Time:TpvDouble;
begin
 if (aCountArguments=4) and (POCAGhostGetType(aThis)=@POCAVector4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAVector4Ghost) and (POCAGhostGetType(aArguments^[2])=@POCAVector4Ghost) and (POCAGetValueType(aArguments^[3])=pvtNUMBER) then begin
  A:=POCAGhostFastGetPointer(aThis);
  B:=POCAGhostFastGetPointer(aArguments^[0]);
  C:=POCAGhostFastGetPointer(aArguments^[1]);
  D:=POCAGhostFastGetPointer(aArguments^[2]);
  Time:=POCAGetNumberValue(aContext,aArguments^[3]);
  result:=POCANewVector4(aContext,A^.Sqlerp(B^,C^,D^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionAdd(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4:PpvVector4D;
    OtherVector4:PpvVector4D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) then begin
  Vector4:=POCAGhostFastGetPointer(aThis);
  OtherVector4:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector4^:=Vector4^+OtherVector4^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionSub(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4:PpvVector4D;
    OtherVector4:PpvVector4D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) then begin
  Vector4:=POCAGhostFastGetPointer(aThis);
  OtherVector4:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector4^:=Vector4^-OtherVector4^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionMul(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4,OtherVector4:PpvVector4D;
    Factor:TpvDouble;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector4Ghost) and (POCAGetValueType(aArguments^[0])=pvtNUMBER) then begin
  Vector4:=POCAGhostFastGetPointer(aThis);
  Factor:=POCAGetNumberValue(aContext,aArguments^[0]);
  Vector4^:=Vector4^*Factor;
  result:=aThis;
 end else if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) then begin
  Vector4:=POCAGhostFastGetPointer(aThis);
  OtherVector4:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector4^:=Vector4^*OtherVector4^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionDiv(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4,OtherVector4:PpvVector4D;
    Factor:TpvDouble;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector4Ghost) and (POCAGetValueType(aArguments^[0])=pvtNUMBER) then begin
  Vector4:=POCAGhostFastGetPointer(aThis);
  Factor:=POCAGetNumberValue(aContext,aArguments^[0]);
  Vector4^:=Vector4^/Factor;
  result:=aThis;
 end else if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) then begin
  Vector4:=POCAGhostFastGetPointer(aThis);
  OtherVector4:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector4^:=Vector4^/OtherVector4^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionNeg(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4:PpvVector4D;
begin
 if (aCountArguments=0) and (POCAGhostGetType(aThis)=@POCAVector4Ghost) then begin
  Vector4:=POCAGhostFastGetPointer(aThis);
  Vector4^:=-Vector4^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4,OtherVector4:PpvVector4D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) then begin
  Vector4:=POCAGhostFastGetPointer(aThis);
  OtherVector4:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewNumber(aContext,ord(Vector4^=OtherVector4^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionNotEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4,OtherVector4:PpvVector4D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAVector4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) then begin
  Vector4:=POCAGhostFastGetPointer(aThis);
  OtherVector4:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewNumber(aContext,ord(Vector4^<>OtherVector4^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionToString(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4:PpvVector4D;
    s:TpvUTF8String;
begin
 if (aCountArguments=0) and (POCAGhostGetType(aThis)=@POCAVector4Ghost) then begin
  Vector4:=POCAGhostFastGetPointer(aThis);
  s:='['+ConvertDoubleToString(Vector4^.x,omStandard,-1)+','+ConvertDoubleToString(Vector4^.y,omStandard,-1)+','+ConvertDoubleToString(Vector4^.z,omStandard,-1)+','+ConvertDoubleToString(Vector4^.w,omStandard,-1)+']';
  result:=POCANewString(aContext,s);
 end else begin
  result:=POCAValueNull;
 end;
end;

// "THIS" is null, because it is a binary operator, so the first argument is the first operand and the second argument is the second operand
function POCAVector4FunctionOpAdd(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4,OtherVector4:PpvVector4D;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAVector4Ghost) then begin
  Vector4:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherVector4:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewVector4(aContext,Vector4^+OtherVector4^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionOpSub(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4,OtherVector4:PpvVector4D;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAVector4Ghost) then begin
  Vector4:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherVector4:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewVector4(aContext,Vector4^-OtherVector4^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionOpMul(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4,OtherVector4:PpvVector4D;
    Factor:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Vector4:=POCAGhostFastGetPointer(aArguments^[0]);
  Factor:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewVector4(aContext,Vector4^*Factor);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAVector4Ghost) then begin
  Vector4:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherVector4:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewVector4(aContext,Vector4^*OtherVector4^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionOpDiv(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4,OtherVector4:PpvVector4D;
    Factor:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Vector4:=POCAGhostFastGetPointer(aArguments^[0]);
  Factor:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewVector4(aContext,Vector4^/Factor);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAVector4Ghost) then begin
  Vector4:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherVector4:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewVector4(aContext,Vector4^/OtherVector4^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionOpEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4,OtherVector4:PpvVector4D;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAVector4Ghost) then begin
  Vector4:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherVector4:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewNumber(aContext,ord(Vector4^=OtherVector4^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionOpNotEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4,OtherVector4:PpvVector4D;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAVector4Ghost) then begin
  Vector4:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherVector4:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewNumber(aContext,ord(Vector4^<>OtherVector4^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionOpNeg(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4:PpvVector4D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) then begin
  Vector4:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewVector4(aContext,-Vector4^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionOpSqrt(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4:PpvVector4D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) then begin
  Vector4:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewVector4(aContext,Sqrt(Vector4^.x),Sqrt(Vector4^.y),Sqrt(Vector4^.z));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAVector4FunctionOpToString(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Vector4:PpvVector4D;
    s:TpvUTF8String;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAVector4Ghost) then begin
  Vector4:=POCAGhostFastGetPointer(aArguments^[0]);
  s:='['+ConvertDoubleToString(Vector4^.x,omStandard,-1)+','+ConvertDoubleToString(Vector4^.y,omStandard,-1)+','+ConvertDoubleToString(Vector4^.z,omStandard,-1)+','+ConvertDoubleToString(Vector4^.w,omStandard,-1)+']';
  result:=POCANewString(aContext,s);
 end else begin
  result:=POCAValueNull;
 end;
end;

procedure POCAInitVector4Hash(aContext:PPOCAContext);
begin

 POCAVector4GhostPointer:=@POCAVector4Ghost;

 POCAVector4Hash:=POCANewHash(aContext);
 POCAArrayPush(aContext^.Instance^.Globals.RootArray,POCAVector4Hash);
 POCAAddNativeFunction(aContext,POCAVector4Hash,'length',POCAVector4FunctionLength);
 POCAAddNativeFunction(aContext,POCAVector4Hash,'squaredLength',POCAVector4FunctionSquaredLength);
 POCAAddNativeFunction(aContext,POCAVector4Hash,'normalize',POCAVector4FunctionNormalize);
 POCAAddNativeFunction(aContext,POCAVector4Hash,'dot',POCAVector4FunctionDot);
 POCAAddNativeFunction(aContext,POCAVector4Hash,'cross',POCAVector4FunctionCross);
 POCAAddNativeFunction(aContext,POCAVector4Hash,'distanceTo',POCAVector4FunctionDistanceTo);
 POCAAddNativeFunction(aContext,POCAVector4Hash,'lerp',POCAVector4FunctionLerp);
 POCAAddNativeFunction(aContext,POCAVector4Hash,'nlerp',POCAVector4FunctionNlerp);
 POCAAddNativeFunction(aContext,POCAVector4Hash,'slerp',POCAVector4FunctionSlerp);
 POCAAddNativeFunction(aContext,POCAVector4Hash,'sqlerp',POCAVector4FunctionSqlerp);
 POCAAddNativeFunction(aContext,POCAVector4Hash,'add',POCAVector4FunctionAdd);
 POCAAddNativeFunction(aContext,POCAVector4Hash,'sub',POCAVector4FunctionSub);
 POCAAddNativeFunction(aContext,POCAVector4Hash,'mul',POCAVector4FunctionMul);
 POCAAddNativeFunction(aContext,POCAVector4Hash,'div',POCAVector4FunctionDiv);
 POCAAddNativeFunction(aContext,POCAVector4Hash,'neg',POCAVector4FunctionNeg);
 POCAAddNativeFunction(aContext,POCAVector4Hash,'equal',POCAVector4FunctionEqual);
 POCAAddNativeFunction(aContext,POCAVector4Hash,'notEqual',POCAVector4FunctionNotEqual);
 POCAAddNativeFunction(aContext,POCAVector4Hash,'toString',POCAVector4FunctionToString);

 POCAVector4HashEvents:=POCANewHash(aContext);
 POCAArrayPush(aContext^.Instance^.Globals.RootArray,POCAVector4HashEvents);
 POCAAddNativeFunction(aContext,POCAVector4HashEvents,'__add',POCAVector4FunctionOpAdd);
 POCAAddNativeFunction(aContext,POCAVector4HashEvents,'__sub',POCAVector4FunctionOpSub);
 POCAAddNativeFunction(aContext,POCAVector4HashEvents,'__mul',POCAVector4FunctionOpMul);
 POCAAddNativeFunction(aContext,POCAVector4HashEvents,'__div',POCAVector4FunctionOpDiv);
 POCAAddNativeFunction(aContext,POCAVector4HashEvents,'__eq',POCAVector4FunctionOpEqual);
 POCAAddNativeFunction(aContext,POCAVector4HashEvents,'__neq',POCAVector4FunctionOpNotEqual);
 POCAAddNativeFunction(aContext,POCAVector4HashEvents,'__neg',POCAVector4FunctionOpNeg);
 POCAAddNativeFunction(aContext,POCAVector4HashEvents,'__sqrt',POCAVector4FunctionOpSqrt);
 POCAAddNativeFunction(aContext,POCAVector4HashEvents,'__tostring',POCAVector4FunctionOpToString);

 POCAHashSetHashEvents(aContext,POCAVector4Hash,POCAVector4HashEvents);

end;

procedure POCAInitVector4Namespace(aContext:PPOCAContext);
var Hash:TPOCAValue;
begin
 Hash:=POCANewHash(aContext);
 POCAArrayPush(aContext^.Instance^.Globals.RootArray,Hash);
 POCAAddNativeFunction(aContext,Hash,'create',POCAVector4FunctionCREATE);
 POCAHashSetString(aContext,aContext^.Instance^.Globals.Namespace,'Vector4',Hash);
end;

procedure POCAInitVector4(aContext:PPOCAContext);
begin
 POCAInitVector4Hash(aContext);
 POCAInitVector4Namespace(aContext);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Quaternion
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

var POCAQuaternionHash,POCAQuaternionHashEvents:TPOCAValue;

procedure POCAQuaternionGhostDestroy(const aGhost:PPOCAGhost);
begin
 if assigned(aGhost) and assigned(aGhost^.Ptr) then begin
  FreeMem(aGhost^.Ptr);
 end;
end;

function POCAQuaternionGhostExistKey(const aContext:PPOCAContext;const aGhost:PPOCAGhost;const aKey:TPOCAValue):longbool;
var s:TpvUTF8String;
begin
 s:=POCAGetStringValue(aContext,aKey);
 if s='x' then begin
  result:=true;
 end else if s='y' then begin
  result:=true;
 end else if s='z' then begin
  result:=true;
 end else if s='w' then begin
  result:=true;
 end else begin
  result:=false;
 end;
end;

function POCAQuaternionGhostGetKey(const aContext:PPOCAContext;const aGhost:PPOCAGhost;const aKey:TPOCAValue;out aValue:TPOCAValue):longbool;
var Quaternion:PpvQuaternionD;
    s:TpvUTF8String;
begin
 Quaternion:=PpvQuaternionD(PPOCAGhost(aGhost)^.Ptr);
 s:=POCAGetStringValue(aContext,aKey);
 if s='x' then begin
  aValue.Num:=Quaternion^.x;
  result:=true;
 end else if s='y' then begin
  aValue.Num:=Quaternion^.y;
  result:=true;
 end else if s='z'then begin
  aValue.Num:=Quaternion^.z;
  result:=true;
 end else if s='w' then begin
  aValue.Num:=Quaternion^.w;
  result:=true;
 end else begin
  result:=false;
 end;
end;

function POCAQuaternionGhostSetKey(const aContext:PPOCAContext;const aGhost:PPOCAGhost;const aKey:TPOCAValue;const aValue:TPOCAValue):longbool;
var Quaternion:PpvQuaternionD;
    s:TpvUTF8String;
begin
 Quaternion:=PpvQuaternionD(PPOCAGhost(aGhost)^.Ptr);
 s:=POCAGetStringValue(aContext,aKey);
 if s='x' then begin
  Quaternion^.x:=POCAGetNumberValue(aContext,aValue);
  result:=true;
 end else if s='y' then begin
  Quaternion^.y:=POCAGetNumberValue(aContext,aValue);
  result:=true;
 end else if s='z' then begin
  Quaternion^.z:=POCAGetNumberValue(aContext,aValue);
  result:=true;
 end else if s='w' then begin
  Quaternion^.w:=POCAGetNumberValue(aContext,aValue);
  result:=true;
 end else begin
  result:=false;
 end;
end;

const POCAQuaternionGhost:TPOCAGhostType=
       (
        Destroy:POCAQuaternionGhostDestroy;
        CanDestroy:nil;
        Mark:nil;
        ExistKey:POCAQuaternionGhostExistKey;
        GetKey:POCAQuaternionGhostGetKey;
        SetKey:POCAQuaternionGhostSetKey;
        Name:'Quaternion'
       );

function POCANewQuaternion(const aContext:PPOCAContext;const aQuaternion:TpvQuaternionD):TPOCAValue; overload;
var Quaternion:PpvQuaternionD;
begin
 Quaternion:=nil;
 GetMem(Quaternion,SizeOf(TpvQuaternionD));
 Quaternion^:=aQuaternion;
 result:=POCANewGhost(aContext,@POCAQuaternionGhost,Quaternion,nil,pgptRAW);
 POCAGhostSetHashValue(result,POCAQuaternionHash);
end;

function POCANewQuaternion(const aContext:PPOCAContext;const aX:TpvDouble;const aY:TpvDouble;const aZ:TpvDouble;const aW:TpvDouble):TPOCAValue; overload;
begin
 result:=POCANewQuaternion(aContext,TpvQuaternionD.Create(aX,aY,aZ,aW));
end;

function POCAGetQuaternionValue(const aValue:TPOCAValue):TpvQuaternionD;
begin
 if POCAGhostGetType(aValue)=@POCAQuaternionGhost then begin
  result:=PpvQuaternionD(POCAGhostFastGetPointer(aValue))^;
 end else begin
  result:=TpvQuaternionD.Create(0.0,0.0,0.0,0.0);
 end;
end;

function POCAQuaternionFunctionCREATE(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion:TpvQuaternionD;
    Vector2:PpvVector2D;
    Vector3:PpvVector3D;
    Vector4:PpvVector4D;
begin
 if (aCountArguments>0) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) then begin
  Quaternion:=PpvQuaternionD(POCAGhostFastGetPointer(aArguments^[0]))^;
 end else if (aCountArguments>0) and (POCAGhostGetType(aArguments^[0])=POCAVector2GhostPointer) then begin
  Vector2:=POCAGhostFastGetPointer(aArguments^[0]);
  Quaternion.x:=Vector2^.x;
  Quaternion.y:=Vector2^.y;
  if aCountArguments>1 then begin
   Quaternion.z:=POCAGetNumberValue(aContext,aArguments^[1]);
  end else begin
   Quaternion.z:=0.0;
  end;
  if aCountArguments>2 then begin
   Quaternion.w:=POCAGetNumberValue(aContext,aArguments^[2]);
  end else begin
   Quaternion.w:=0.0;
  end;
 end else if (aCountArguments>0) and (POCAGhostGetType(aArguments^[0])=POCAVector3GhostPointer) then begin
  Vector3:=POCAGhostFastGetPointer(aArguments^[0]);
  Quaternion.xyz:=Vector3^;
  if aCountArguments>1 then begin
   Quaternion.w:=POCAGetNumberValue(aContext,aArguments^[1]);
  end else begin
   Quaternion.w:=0.0;
  end;
 end else if (aCountArguments>0) and (POCAGhostGetType(aArguments^[0])=POCAVector4GhostPointer) then begin
  Vector4:=POCAGhostFastGetPointer(aArguments^[0]);
  Quaternion.x:=Vector4^.x;
  Quaternion.y:=Vector4^.y;
  Quaternion.z:=Vector4^.z;
  Quaternion.w:=Vector4^.w;
 end else begin
  if aCountArguments>0 then begin
   Quaternion.x:=POCAGetNumberValue(aContext,aArguments^[0]);
  end else begin
   Quaternion.x:=0.0;
  end;
  if aCountArguments>1 then begin
   Quaternion.y:=POCAGetNumberValue(aContext,aArguments^[1]);
  end else begin
   Quaternion.y:=0.0;
  end;
  if aCountArguments>2 then begin
   Quaternion.z:=POCAGetNumberValue(aContext,aArguments^[2]);
  end else begin
   Quaternion.z:=0.0;
  end;
  if aCountArguments>3 then begin
   Quaternion.w:=POCAGetNumberValue(aContext,aArguments^[3]);
  end else begin
   Quaternion.w:=0.0;
  end;
 end;
 result:=POCANewQuaternion(aContext,Quaternion);
end;

function POCAQuaternionFunctionConjugate(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion:PpvQuaternionD;
begin
 if POCAGhostGetType(aThis)=@POCAQuaternionGhost then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  result:=POCANewQuaternion(aContext,Quaternion^.Conjugate);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionInverse(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion:PpvQuaternionD;
begin
 if POCAGhostGetType(aThis)=@POCAQuaternionGhost then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  result:=POCANewQuaternion(aContext,Quaternion^.Inverse);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionLog(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion:PpvQuaternionD;
begin
 if POCAGhostGetType(aThis)=@POCAQuaternionGhost then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  result:=POCANewQuaternion(aContext,Quaternion^.Log);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionExp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion:PpvQuaternionD;
begin
 if POCAGhostGetType(aThis)=@POCAQuaternionGhost then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  result:=POCANewQuaternion(aContext,Quaternion^.Exp);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionLength(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion:PpvQuaternionD;
begin
 if POCAGhostGetType(aThis)=@POCAQuaternionGhost then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  result:=POCANewNumber(aContext,Quaternion^.Length);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionSquaredLength(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion:PpvQuaternionD;
begin
 if POCAGhostGetType(aThis)=@POCAQuaternionGhost then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  result:=POCANewNumber(aContext,Quaternion^.SquaredLength);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionNormalize(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion:PpvQuaternionD;
begin
 if POCAGhostGetType(aThis)=@POCAQuaternionGhost then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  result:=POCANewQuaternion(aContext,Quaternion^.Normalize);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionDot(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion:PpvQuaternionD;
    OtherQuaternion:PpvQuaternionD;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  OtherQuaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  result.Num:=Quaternion^.Dot(OtherQuaternion^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionCross(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion:PpvQuaternionD;
    OtherQuaternion:PpvQuaternionD;
    Temporary:TpvQuaternionD;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  OtherQuaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  Temporary.xyz:=Quaternion^.xyz.Cross(OtherQuaternion^.xyz);
  Temporary.w:=1.0;
  result:=POCANewQuaternion(aContext,Temporary);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionLerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion:PpvQuaternionD;
    OtherQuaternion:PpvQuaternionD;
    Time:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aThis)=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  OtherQuaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  Time:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewQuaternion(aContext,Quaternion^.Lerp(OtherQuaternion^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionNlerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion:PpvQuaternionD;
    OtherQuaternion:PpvQuaternionD;
    Time:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aThis)=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  OtherQuaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  Time:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewQuaternion(aContext,Quaternion^.Nlerp(OtherQuaternion^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionSlerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion:PpvQuaternionD;
    OtherQuaternion:PpvQuaternionD;
    Time:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aThis)=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  OtherQuaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  Time:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewQuaternion(aContext,Quaternion^.Slerp(OtherQuaternion^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionSqlerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var A,B,C,D:PpvQuaternionD;
    Time:TpvDouble;
begin
 if (aCountArguments=4) and (POCAGhostGetType(aThis)=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[1])=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[2])=@POCAQuaternionGhost) and (POCAGetValueType(aArguments^[3])=pvtNUMBER) then begin
  A:=POCAGhostFastGetPointer(aThis);
  B:=POCAGhostFastGetPointer(aArguments^[0]);
  C:=POCAGhostFastGetPointer(aArguments^[1]);
  D:=POCAGhostFastGetPointer(aArguments^[2]);
  Time:=POCAGetNumberValue(aContext,aArguments^[3]);
  result:=POCANewQuaternion(aContext,A^.Sqlerp(B^,C^,D^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionAdd(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion:PpvQuaternionD;
    OtherQuaternion:PpvQuaternionD;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  OtherQuaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  Quaternion^:=Quaternion^+OtherQuaternion^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionSub(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion:PpvQuaternionD;
    OtherQuaternion:PpvQuaternionD;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  OtherQuaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  Quaternion^:=Quaternion^-OtherQuaternion^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionMul(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion,OtherQuaternion:PpvQuaternionD;
    Factor:TpvDouble;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAQuaternionGhost) and (POCAGetValueType(aArguments^[0])=pvtNUMBER) then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  Factor:=POCAGetNumberValue(aContext,aArguments^[0]);
  Quaternion^:=Quaternion^*Factor;
  result:=aThis;
 end else if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  OtherQuaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  Quaternion^:=Quaternion^*OtherQuaternion^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionDiv(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion,OtherQuaternion:PpvQuaternionD;
    Factor:TpvDouble;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAQuaternionGhost) and (POCAGetValueType(aArguments^[0])=pvtNUMBER) then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  Factor:=POCAGetNumberValue(aContext,aArguments^[0]);
  Quaternion^.x:=Quaternion^.x/Factor;
  Quaternion^.y:=Quaternion^.y/Factor;
  Quaternion^.z:=Quaternion^.z/Factor;
  Quaternion^.w:=Quaternion^.w/Factor;
  result:=aThis;
 end else if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  OtherQuaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  Quaternion^:=Quaternion^/OtherQuaternion^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionNeg(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion:PpvQuaternionD;
begin
 if (aCountArguments=0) and (POCAGhostGetType(aThis)=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  Quaternion^:=-Quaternion^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion,OtherQuaternion:PpvQuaternionD;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  OtherQuaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewNumber(aContext,ord(Quaternion^=OtherQuaternion^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionNotEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion,OtherQuaternion:PpvQuaternionD;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  OtherQuaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewNumber(aContext,ord(Quaternion^<>OtherQuaternion^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionToString(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion:PpvQuaternionD;
    s:TpvUTF8String;
begin
 if (aCountArguments=0) and (POCAGhostGetType(aThis)=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aThis);
  s:='['+ConvertDoubleToString(Quaternion^.x,omStandard,-1)+','+ConvertDoubleToString(Quaternion^.y,omStandard,-1)+','+ConvertDoubleToString(Quaternion^.z,omStandard,-1)+','+ConvertDoubleToString(Quaternion^.w,omStandard,-1)+']';
  result:=POCANewString(aContext,s);
 end else begin
  result:=POCAValueNull;
 end;
end;

// "THIS" is null, because it is a binary operator, so the first argument is the first operand and the second argument is the second operand
function POCAQuaternionFunctionOpAdd(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion,OtherQuaternion:PpvQuaternionD;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[1])=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherQuaternion:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewQuaternion(aContext,Quaternion^+OtherQuaternion^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionOpSub(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion,OtherQuaternion:PpvQuaternionD;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[1])=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherQuaternion:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewQuaternion(aContext,Quaternion^-OtherQuaternion^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionOpMul(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion,OtherQuaternion:PpvQuaternionD;
    Factor:TpvDouble;
    Vector3:PpvVector3D;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Quaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  Factor:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewQuaternion(aContext,Quaternion^*Factor);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[1])=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherQuaternion:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewQuaternion(aContext,Quaternion^*OtherQuaternion^);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[1])=POCAVector3GhostPointer) then begin
  Quaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector3:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewVector3(aContext,Quaternion^*Vector3^);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=POCAVector3GhostPointer) and (POCAGhostGetType(aArguments^[1])=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aArguments^[1]);
  Vector3:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewVector3(aContext,Vector3^*Quaternion^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionOpDiv(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion,OtherQuaternion:PpvQuaternionD;
    Factor:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Quaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  Factor:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewQuaternion(aContext,Quaternion^.x/Factor,Quaternion^.y/Factor,Quaternion^.z/Factor,Quaternion^.w/Factor);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[1])=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherQuaternion:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewQuaternion(aContext,Quaternion^/OtherQuaternion^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionOpEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion,OtherQuaternion:PpvQuaternionD;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[1])=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherQuaternion:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewNumber(aContext,ord(Quaternion^=OtherQuaternion^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionOpNotEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion,OtherQuaternion:PpvQuaternionD;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[1])=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherQuaternion:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewNumber(aContext,ord(Quaternion^<>OtherQuaternion^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionOpNeg(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion:PpvQuaternionD;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewQuaternion(aContext,-Quaternion^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionOpSqrt(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion:PpvQuaternionD;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewQuaternion(aContext,Sqrt(Quaternion^.x),Sqrt(Quaternion^.y),Sqrt(Quaternion^.z));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAQuaternionFunctionOpToString(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Quaternion:PpvQuaternionD;
    s:TpvUTF8String;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  s:='['+ConvertDoubleToString(Quaternion^.x,omStandard,-1)+','+ConvertDoubleToString(Quaternion^.y,omStandard,-1)+','+ConvertDoubleToString(Quaternion^.z,omStandard,-1)+','+ConvertDoubleToString(Quaternion^.w,omStandard,-1)+']';
  result:=POCANewString(aContext,s);
 end else begin
  result:=POCAValueNull;
 end;
end;

procedure POCAInitQuaternionHash(aContext:PPOCAContext);
begin

 POCAQuaternionGhostPointer:=@POCAQuaternionGhost;

 POCAQuaternionHash:=POCANewHash(aContext);
 POCAArrayPush(aContext^.Instance^.Globals.RootArray,POCAQuaternionHash);
 POCAAddNativeFunction(aContext,POCAQuaternionHash,'conjugate',POCAQuaternionFunctionConjugate);
 POCAAddNativeFunction(aContext,POCAQuaternionHash,'inverse',POCAQuaternionFunctionInverse);
 POCAAddNativeFunction(aContext,POCAQuaternionHash,'log',POCAQuaternionFunctionLog);
 POCAAddNativeFunction(aContext,POCAQuaternionHash,'exp',POCAQuaternionFunctionExp);
 POCAAddNativeFunction(aContext,POCAQuaternionHash,'length',POCAQuaternionFunctionLength);
 POCAAddNativeFunction(aContext,POCAQuaternionHash,'squaredLength',POCAQuaternionFunctionSquaredLength);
 POCAAddNativeFunction(aContext,POCAQuaternionHash,'normalize',POCAQuaternionFunctionNormalize);
 POCAAddNativeFunction(aContext,POCAQuaternionHash,'dot',POCAQuaternionFunctionDot);
 POCAAddNativeFunction(aContext,POCAQuaternionHash,'cross',POCAQuaternionFunctionCross);
 POCAAddNativeFunction(aContext,POCAQuaternionHash,'lerp',POCAQuaternionFunctionLerp);
 POCAAddNativeFunction(aContext,POCAQuaternionHash,'nlerp',POCAQuaternionFunctionNlerp);
 POCAAddNativeFunction(aContext,POCAQuaternionHash,'slerp',POCAQuaternionFunctionSlerp);
 POCAAddNativeFunction(aContext,POCAQuaternionHash,'sqlerp',POCAQuaternionFunctionSqlerp);
 POCAAddNativeFunction(aContext,POCAQuaternionHash,'add',POCAQuaternionFunctionAdd);
 POCAAddNativeFunction(aContext,POCAQuaternionHash,'sub',POCAQuaternionFunctionSub);
 POCAAddNativeFunction(aContext,POCAQuaternionHash,'mul',POCAQuaternionFunctionMul);
 POCAAddNativeFunction(aContext,POCAQuaternionHash,'div',POCAQuaternionFunctionDiv);
 POCAAddNativeFunction(aContext,POCAQuaternionHash,'neg',POCAQuaternionFunctionNeg);
 POCAAddNativeFunction(aContext,POCAQuaternionHash,'equal',POCAQuaternionFunctionEqual);
 POCAAddNativeFunction(aContext,POCAQuaternionHash,'notEqual',POCAQuaternionFunctionNotEqual);
 POCAAddNativeFunction(aContext,POCAQuaternionHash,'toString',POCAQuaternionFunctionToString);

 POCAQuaternionHashEvents:=POCANewHash(aContext);
 POCAArrayPush(aContext^.Instance^.Globals.RootArray,POCAQuaternionHashEvents);
 POCAAddNativeFunction(aContext,POCAQuaternionHashEvents,'__add',POCAQuaternionFunctionOpAdd);
 POCAAddNativeFunction(aContext,POCAQuaternionHashEvents,'__sub',POCAQuaternionFunctionOpSub);
 POCAAddNativeFunction(aContext,POCAQuaternionHashEvents,'__mul',POCAQuaternionFunctionOpMul);
 POCAAddNativeFunction(aContext,POCAQuaternionHashEvents,'__div',POCAQuaternionFunctionOpDiv);
 POCAAddNativeFunction(aContext,POCAQuaternionHashEvents,'__eq',POCAQuaternionFunctionOpEqual);
 POCAAddNativeFunction(aContext,POCAQuaternionHashEvents,'__neq',POCAQuaternionFunctionOpNotEqual);
 POCAAddNativeFunction(aContext,POCAQuaternionHashEvents,'__neg',POCAQuaternionFunctionOpNeg);
 POCAAddNativeFunction(aContext,POCAQuaternionHashEvents,'__sqrt',POCAQuaternionFunctionOpSqrt);
 POCAAddNativeFunction(aContext,POCAQuaternionHashEvents,'__tostring',POCAQuaternionFunctionOpToString);

 POCAHashSetHashEvents(aContext,POCAQuaternionHash,POCAQuaternionHashEvents);

end;

procedure POCAInitQuaternionNamespace(aContext:PPOCAContext);
var Hash:TPOCAValue;
begin
 Hash:=POCANewHash(aContext);
 POCAArrayPush(aContext^.Instance^.Globals.RootArray,Hash);
 POCAAddNativeFunction(aContext,Hash,'create',POCAQuaternionFunctionCREATE);
 POCAHashSetString(aContext,aContext^.Instance^.Globals.Namespace,'Quaternion',Hash);
end;

procedure POCAInitQuaternion(aContext:PPOCAContext);
begin
 POCAInitQuaternionHash(aContext);
 POCAInitQuaternionNamespace(aContext);
end;

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Matrix3x3
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

var POCAMatrix3x3Hash,POCAMatrix3x3HashEvents:TPOCAValue;
    
procedure POCAMatrix3x3GhostDestroy(const aGhost:PPOCAGhost);
begin
 if assigned(aGhost) and assigned(aGhost^.Ptr) then begin
  FreeMem(aGhost^.Ptr);
 end;
end;

function POCAMatrix3x3GhostExistKey(const aContext:PPOCAContext;const aGhost:PPOCAGhost;const aKey:TPOCAValue):longbool;
var s:TpvUTF8String;
begin
 s:=POCAGetStringValue(aContext,aKey);
 if s='m00' then begin
  result:=true;
 end else if s='m01' then begin
  result:=true;
 end else if s='m02' then begin
  result:=true;
 end else if s='m10' then begin
  result:=true;
 end else if s='m11' then begin
  result:=true;
 end else if s='m12' then begin
  result:=true;
 end else if s='m20' then begin
  result:=true;
 end else if s='m21' then begin
  result:=true;
 end else if s='m22' then begin
  result:=true;
 end else begin
  result:=false;
 end;
end;

function POCAMatrix3x3GhostGetKey(const aContext:PPOCAContext;const aGhost:PPOCAGhost;const aKey:TPOCAValue;out aValue:TPOCAValue):longbool;
var Matrix3x3:PpvMatrix3x3D;
    s:TpvUTF8String;
begin
 Matrix3x3:=PpvMatrix3x3D(PPOCAGhost(aGhost)^.Ptr);
 s:=POCAGetStringValue(aContext,aKey);
 if s='m00' then begin
  aValue.Num:=Matrix3x3^.RawComponents[0,0];
  result:=true;
 end else if s='m01' then begin
  aValue.Num:=Matrix3x3^.RawComponents[0,1];
  result:=true;
 end else if s='m02' then begin
  aValue.Num:=Matrix3x3^.RawComponents[0,2];
  result:=true;
 end else if s='m10' then begin
  aValue.Num:=Matrix3x3^.RawComponents[1,0];
  result:=true;
 end else if s='m11' then begin
  aValue.Num:=Matrix3x3^.RawComponents[1,1];
  result:=true;
 end else if s='m12' then begin
  aValue.Num:=Matrix3x3^.RawComponents[1,2];
  result:=true;
 end else if s='m20' then begin
  aValue.Num:=Matrix3x3^.RawComponents[2,0];
  result:=true;
 end else if s='m21' then begin
  aValue.Num:=Matrix3x3^.RawComponents[2,1];
  result:=true;
 end else if s='m22' then begin
  aValue.Num:=Matrix3x3^.RawComponents[2,2];
  result:=true;
 end else begin
  result:=false;
 end;
end;

function POCAMatrix3x3GhostSetKey(const aContext:PPOCAContext;const aGhost:PPOCAGhost;const aKey:TPOCAValue;const aValue:TPOCAValue):longbool;
var Matrix3x3:PpvMatrix3x3D;
    s:TpvUTF8String;
begin
 Matrix3x3:=PpvMatrix3x3D(PPOCAGhost(aGhost)^.Ptr);
 s:=POCAGetStringValue(aContext,aKey);
 if s='m00' then begin
  Matrix3x3^.RawComponents[0,0]:=aValue.Num;
  result:=true;
 end else if s='m01' then begin
  Matrix3x3^.RawComponents[0,1]:=aValue.Num;
  result:=true;
 end else if s='m02' then begin
  Matrix3x3^.RawComponents[0,2]:=aValue.Num;
  result:=true;
 end else if s='m10' then begin
  Matrix3x3^.RawComponents[1,0]:=aValue.Num;
  result:=true;
 end else if s='m11' then begin
  Matrix3x3^.RawComponents[1,1]:=aValue.Num;
  result:=true;
 end else if s='m12' then begin
  Matrix3x3^.RawComponents[1,2]:=aValue.Num;
  result:=true;
 end else if s='m20' then begin
  Matrix3x3^.RawComponents[2,0]:=aValue.Num;
  result:=true;
 end else if s='m21' then begin
  Matrix3x3^.RawComponents[2,1]:=aValue.Num;
  result:=true;
 end else if s='m22' then begin
  Matrix3x3^.RawComponents[2,2]:=aValue.Num;
  result:=true;
 end else begin
  result:=false;
 end;
end;

const POCAMatrix3x3Ghost:TPOCAGhostType=
       (
        Destroy:POCAMatrix3x3GhostDestroy;
        CanDestroy:nil;
        Mark:nil;
        ExistKey:POCAMatrix3x3GhostExistKey;
        GetKey:POCAMatrix3x3GhostGetKey;
        SetKey:POCAMatrix3x3GhostSetKey;
        Name:'Matrix3x3'
       );

function POCANewMatrix3x3(const aContext:PPOCAContext;const aMatrix3x3:TpvMatrix3x3D):TPOCAValue;
var Matrix3x3:PpvMatrix3x3D;
begin
 GetMem(Matrix3x3,SizeOf(TpvMatrix3x3D));
 Matrix3x3^:=aMatrix3x3;
 result:=POCANewGhost(aContext,@POCAMatrix3x3Ghost,Matrix3x3,nil,pgptRAW);
 POCAGhostSetHashValue(result,POCAMatrix3x3Hash);
end;

function POCANewMatrix3x3(const aContext:PPOCAContext;const aM00:TpvDouble;const aM01:TpvDouble;const aM02:TpvDouble;const aM10:TpvDouble;const aM11:TpvDouble;const aM12:TpvDouble;const aM20:TpvDouble;const aM21:TpvDouble;const aM22:TpvDouble):TPOCAValue;
var Matrix3x3:TpvMatrix3x3D; 
begin 
 Matrix3x3.RawComponents[0,0]:=aM00;
 Matrix3x3.RawComponents[0,1]:=aM01; 
 Matrix3x3.RawComponents[0,2]:=aM02;
 Matrix3x3.RawComponents[1,0]:=aM10;
 Matrix3x3.RawComponents[1,1]:=aM11;
 Matrix3x3.RawComponents[1,2]:=aM12;
 Matrix3x3.RawComponents[2,0]:=aM20;
 Matrix3x3.RawComponents[2,1]:=aM21;
 Matrix3x3.RawComponents[2,2]:=aM22;
 result:=POCANewMatrix3x3(aContext,Matrix3x3);
end;

function POCAGetMatrix3x3Value(const aValue:TPOCAValue):TpvMatrix3x3D;
begin
 if POCAGhostGetType(aValue)=@POCAMatrix3x3Ghost then begin
  result:=PpvMatrix3x3D(POCAGhostFastGetPointer(aValue))^;
 end else begin
  result:=TpvMatrix3x3.Create(1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0);
 end;
end;

function POCAMatrix3x3FunctionCREATE(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3:TpvMatrix3x3D;
    Matrix4x4:PpvMatrix4x4D;
    Quaternion:PpvQuaternionD;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) then begin
  Matrix3x3:=PpvMatrix3x3D(POCAGhostFastGetPointer(aArguments^[0]))^;
 end else if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=POCAMatrix4x4GhostPointer) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  Matrix3x3.RawComponents[0,0]:=Matrix4x4^.RawComponents[0,0];
  Matrix3x3.RawComponents[0,1]:=Matrix4x4^.RawComponents[0,1];
  Matrix3x3.RawComponents[0,2]:=Matrix4x4^.RawComponents[0,2];
  Matrix3x3.RawComponents[1,0]:=Matrix4x4^.RawComponents[1,0];
  Matrix3x3.RawComponents[1,1]:=Matrix4x4^.RawComponents[1,1];
  Matrix3x3.RawComponents[1,2]:=Matrix4x4^.RawComponents[1,2];
  Matrix3x3.RawComponents[2,0]:=Matrix4x4^.RawComponents[2,0];
  Matrix3x3.RawComponents[2,1]:=Matrix4x4^.RawComponents[2,1];
  Matrix3x3.RawComponents[2,2]:=Matrix4x4^.RawComponents[2,2];
 end else if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  Matrix3x3:=TpvMatrix3x3D.Create(Quaternion^);
 end else begin
  if aCountArguments>0 then begin
   Matrix3x3.RawComponents[0,0]:=POCAGetNumberValue(aContext,aArguments^[0]);
  end else begin
   Matrix3x3.RawComponents[0,0]:=1.0;
  end;
  if aCountArguments>1 then begin
   Matrix3x3.RawComponents[0,1]:=POCAGetNumberValue(aContext,aArguments^[1]);
  end else begin
   Matrix3x3.RawComponents[0,1]:=0.0;
  end;
  if aCountArguments>2 then begin
   Matrix3x3.RawComponents[0,2]:=POCAGetNumberValue(aContext,aArguments^[2]);
  end else begin
   Matrix3x3.RawComponents[0,2]:=0.0;
  end;
  if aCountArguments>3 then begin
   Matrix3x3.RawComponents[1,0]:=POCAGetNumberValue(aContext,aArguments^[3]);
  end else begin
   Matrix3x3.RawComponents[1,0]:=0.0;
  end;
  if aCountArguments>4 then begin
   Matrix3x3.RawComponents[1,1]:=POCAGetNumberValue(aContext,aArguments^[4]);
  end else begin
   Matrix3x3.RawComponents[1,1]:=1.0;
  end;    
  if aCountArguments>5 then begin
   Matrix3x3.RawComponents[1,2]:=POCAGetNumberValue(aContext,aArguments^[5]);
  end else begin
   Matrix3x3.RawComponents[1,2]:=0.0;
  end;
  if aCountArguments>6 then begin
   Matrix3x3.RawComponents[2,0]:=POCAGetNumberValue(aContext,aArguments^[6]);
  end else begin
   Matrix3x3.RawComponents[2,0]:=0.0;
  end;
  if aCountArguments>7 then begin
   Matrix3x3.RawComponents[2,1]:=POCAGetNumberValue(aContext,aArguments^[7]);
  end else begin
   Matrix3x3.RawComponents[2,1]:=0.0;
  end;
  if aCountArguments>8 then begin
   Matrix3x3.RawComponents[2,2]:=POCAGetNumberValue(aContext,aArguments^[8]);
  end else begin
   Matrix3x3.RawComponents[2,2]:=1.0;
  end;
 end;
 result:=POCANewMatrix3x3(aContext,Matrix3x3);
end;

function POCAMatrix3x3FunctionEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3,OtherMatrix3x3:PpvMatrix3x3D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAMatrix3x3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aThis);
  OtherMatrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewNumber(aContext,ord(Matrix3x3^=OtherMatrix3x3^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionNotEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3,OtherMatrix3x3:PpvMatrix3x3D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAMatrix3x3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aThis);
  OtherMatrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewNumber(aContext,ord(Matrix3x3^<>OtherMatrix3x3^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionToString(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3:PpvMatrix3x3D;
    s:TpvUTF8String;
begin
 if (aCountArguments=0) and (POCAGhostGetType(aThis)=@POCAMatrix3x3Ghost) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aThis);
  s:='[['+ConvertDoubleToString(Matrix3x3^.RawComponents[0,0],omStandard,-1)+','+ConvertDoubleToString(Matrix3x3^.RawComponents[0,1],omStandard,-1)+','+ConvertDoubleToString(Matrix3x3^.RawComponents[0,2],omStandard,-1)+'],'+
      '['+ConvertDoubleToString(Matrix3x3^.RawComponents[1,0],omStandard,-1)+','+ConvertDoubleToString(Matrix3x3^.RawComponents[1,1],omStandard,-1)+','+ConvertDoubleToString(Matrix3x3^.RawComponents[1,2],omStandard,-1)+'],'+
      '['+ConvertDoubleToString(Matrix3x3^.RawComponents[2,0],omStandard,-1)+','+ConvertDoubleToString(Matrix3x3^.RawComponents[2,1],omStandard,-1)+','+ConvertDoubleToString(Matrix3x3^.RawComponents[2,2],omStandard,-1)+']]';
  result:=POCANewString(aContext,s);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionAdd(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3,OtherMatrix3x3:PpvMatrix3x3D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAMatrix3x3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aThis);
  OtherMatrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  Matrix3x3^:=Matrix3x3^+OtherMatrix3x3^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionSub(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3,OtherMatrix3x3:PpvMatrix3x3D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAMatrix3x3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aThis);
  OtherMatrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  Matrix3x3^:=Matrix3x3^-OtherMatrix3x3^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionMul(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3,OtherMatrix3x3:PpvMatrix3x3D;
    Factor:TpvDouble;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAMatrix3x3Ghost) and (POCAGetValueType(aArguments^[0])=pvtNUMBER) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aThis);
  Factor:=POCAGetNumberValue(aContext,aArguments^[0]);
  Matrix3x3^:=Matrix3x3^*Factor;
  result:=aThis;
 end else if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAMatrix3x3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aThis);
  OtherMatrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  Matrix3x3^:=Matrix3x3^*OtherMatrix3x3^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionOpAdd(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3,OtherMatrix3x3:PpvMatrix3x3D;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAMatrix3x3Ghost) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherMatrix3x3:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewMatrix3x3(aContext,Matrix3x3^+OtherMatrix3x3^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionOpSub(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3,OtherMatrix3x3:PpvMatrix3x3D;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAMatrix3x3Ghost) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherMatrix3x3:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewMatrix3x3(aContext,Matrix3x3^-OtherMatrix3x3^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionOpMul(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3,OtherMatrix3x3:PpvMatrix3x3D;
    Vector3:PpvVector3D;
    Quaternion:PpvQuaternionD;
    Factor:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  Factor:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewMatrix3x3(aContext,Matrix3x3^*Factor);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAMatrix3x3Ghost) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherMatrix3x3:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewMatrix3x3(aContext,Matrix3x3^*OtherMatrix3x3^);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) and (POCAGhostGetType(aArguments^[1])=POCAVector3GhostPointer) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector3:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewVector3(aContext,Matrix3x3^*Vector3^);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=POCAVector3GhostPointer) and (POCAGhostGetType(aArguments^[1])=@POCAMatrix3x3Ghost) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aArguments^[1]);
  Vector3:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewVector3(aContext,Vector3^*Matrix3x3^);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=POCAMatrix4x4GhostPointer) and (POCAGhostGetType(aArguments^[1])=@POCAQuaternionGhost) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  Quaternion:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewMatrix3x3(aContext,Matrix3x3^*Quaternion^);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[1])=POCAMatrix4x4GhostPointer) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aArguments^[1]);
  Quaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewMatrix3x3(aContext,Quaternion^*Matrix3x3^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionOpDiv(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3,OtherMatrix3x3:PpvMatrix3x3D;
    Factor:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  Factor:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewMatrix3x3(aContext,Matrix3x3^/Factor);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAMatrix3x3Ghost) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherMatrix3x3:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewMatrix3x3(aContext,Matrix3x3^/OtherMatrix3x3^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionOpNeg(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3:PpvMatrix3x3D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewMatrix3x3(aContext,-Matrix3x3^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionOpSqrt(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3:TpvMatrix3x3D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) then begin
  Matrix3x3:=PpvMatrix3x3D(POCAGhostFastGetPointer(aArguments^[0]))^;
  Matrix3x3.RawComponents[0,0]:=Sqrt(Matrix3x3.RawComponents[0,0]);
  Matrix3x3.RawComponents[0,1]:=Sqrt(Matrix3x3.RawComponents[0,1]);
  Matrix3x3.RawComponents[0,2]:=Sqrt(Matrix3x3.RawComponents[0,2]);
  Matrix3x3.RawComponents[1,0]:=Sqrt(Matrix3x3.RawComponents[1,0]);
  Matrix3x3.RawComponents[1,1]:=Sqrt(Matrix3x3.RawComponents[1,1]);
  Matrix3x3.RawComponents[1,2]:=Sqrt(Matrix3x3.RawComponents[1,2]);
  Matrix3x3.RawComponents[2,0]:=Sqrt(Matrix3x3.RawComponents[2,0]);
  Matrix3x3.RawComponents[2,1]:=Sqrt(Matrix3x3.RawComponents[2,1]);
  Matrix3x3.RawComponents[2,2]:=Sqrt(Matrix3x3.RawComponents[2,2]);
  result:=POCANewMatrix3x3(aContext,Matrix3x3);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionOpToString(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3:PpvMatrix3x3D;
    s:TpvUTF8String;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  s:='[['+ConvertDoubleToString(Matrix3x3^.RawComponents[0,0],omStandard,-1)+','+ConvertDoubleToString(Matrix3x3^.RawComponents[0,1],omStandard,-1)+','+ConvertDoubleToString(Matrix3x3^.RawComponents[0,2],omStandard,-1)+'],'+
      '['+ConvertDoubleToString(Matrix3x3^.RawComponents[1,0],omStandard,-1)+','+ConvertDoubleToString(Matrix3x3^.RawComponents[1,1],omStandard,-1)+','+ConvertDoubleToString(Matrix3x3^.RawComponents[1,2],omStandard,-1)+'],'+
      '['+ConvertDoubleToString(Matrix3x3^.RawComponents[2,0],omStandard,-1)+','+ConvertDoubleToString(Matrix3x3^.RawComponents[2,1],omStandard,-1)+','+ConvertDoubleToString(Matrix3x3^.RawComponents[2,2],omStandard,-1)+']]';
  result:=POCANewString(aContext,s);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionOpEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3,OtherMatrix3x3:PpvMatrix3x3D;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAMatrix3x3Ghost) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherMatrix3x3:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewNumber(aContext,ord(Matrix3x3^=OtherMatrix3x3^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionOpNotEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3,OtherMatrix3x3:PpvMatrix3x3D;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAMatrix3x3Ghost) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherMatrix3x3:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewNumber(aContext,ord(Matrix3x3^<>OtherMatrix3x3^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionTranspose(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3:PpvMatrix3x3D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewMatrix3x3(aContext,Matrix3x3^.Transpose);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionDeterminant(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3:PpvMatrix3x3D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  result.Num:=Matrix3x3^.Determinant;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionInverse(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3:PpvMatrix3x3D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewMatrix3x3(aContext,Matrix3x3^.Inverse);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionAdjugate(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3:PpvMatrix3x3D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewMatrix3x3(aContext,Matrix3x3^.Adjugate);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionLerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3,OtherMatrix3x3:PpvMatrix3x3D;
    Time:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aThis)=@POCAMatrix3x3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aThis);
  OtherMatrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  Time:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewMatrix3x3(aContext,Matrix3x3^.Lerp(OtherMatrix3x3^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionNlerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3,OtherMatrix3x3:PpvMatrix3x3D;
    Time:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aThis)=@POCAMatrix3x3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aThis);
  OtherMatrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  Time:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewMatrix3x3(aContext,Matrix3x3^.Nlerp(OtherMatrix3x3^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionSlerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3,OtherMatrix3x3:PpvMatrix3x3D;
    Time:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aThis)=@POCAMatrix3x3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aThis);
  OtherMatrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  Time:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewMatrix3x3(aContext,Matrix3x3^.Slerp(OtherMatrix3x3^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionElerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix3x3,OtherMatrix3x3:PpvMatrix3x3D;
    Time:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aThis)=@POCAMatrix3x3Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aThis);
  OtherMatrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  Time:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewMatrix3x3(aContext,Matrix3x3^.Elerp(OtherMatrix3x3^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix3x3FunctionSqlerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var A,B,C,D:TpvMatrix3x3D;
    Time:TpvDouble;
begin
 if (aCountArguments=4) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix3x3Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAMatrix3x3Ghost) and (POCAGhostGetType(aArguments^[2])=@POCAMatrix3x3Ghost) and (POCAGetValueType(aArguments^[3])=pvtNUMBER) then begin
  A:=PpvMatrix3x3D(POCAGhostFastGetPointer(aThis))^;
  B:=PpvMatrix3x3D(POCAGhostFastGetPointer(aArguments^[0]))^;
  C:=PpvMatrix3x3D(POCAGhostFastGetPointer(aArguments^[1]))^;
  D:=PpvMatrix3x3D(POCAGhostFastGetPointer(aArguments^[2]))^;
  Time:=POCAGetNumberValue(aContext,aArguments^[3]);
  result:=POCANewMatrix3x3(aContext,A.Sqlerp(B,C,D,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

procedure POCAInitMatrix3x3Hash(aContext:PPOCAContext);
begin

 POCAMatrix3x3GhostPointer:=@POCAMatrix3x3Ghost;

 POCAMatrix3x3Hash:=POCANewHash(aContext);
 POCAArrayPush(aContext^.Instance^.Globals.RootArray,POCAMatrix3x3Hash);
 POCAAddNativeFunction(aContext,POCAMatrix3x3Hash,'add',POCAMatrix3x3FunctionAdd);
 POCAAddNativeFunction(aContext,POCAMatrix3x3Hash,'sub',POCAMatrix3x3FunctionSub);
 POCAAddNativeFunction(aContext,POCAMatrix3x3Hash,'mul',POCAMatrix3x3FunctionMul);
 POCAAddNativeFunction(aContext,POCAMatrix3x3Hash,'transpose',POCAMatrix3x3FunctionTranspose);
 POCAAddNativeFunction(aContext,POCAMatrix3x3Hash,'determinant',POCAMatrix3x3FunctionDeterminant);
 POCAAddNativeFunction(aContext,POCAMatrix3x3Hash,'inverse',POCAMatrix3x3FunctionInverse);
 POCAAddNativeFunction(aContext,POCAMatrix3x3Hash,'adjugate',POCAMatrix3x3FunctionAdjugate);
 POCAAddNativeFunction(aContext,POCAMatrix3x3Hash,'lerp',POCAMatrix3x3FunctionLerp);
 POCAAddNativeFunction(aContext,POCAMatrix3x3Hash,'nlerp',POCAMatrix3x3FunctionNlerp);
 POCAAddNativeFunction(aContext,POCAMatrix3x3Hash,'slerp',POCAMatrix3x3FunctionSlerp);
 POCAAddNativeFunction(aContext,POCAMatrix3x3Hash,'sqlerp',POCAMatrix3x3FunctionSqlerp);
 POCAAddNativeFunction(aContext,POCAMatrix3x3Hash,'equal',POCAMatrix3x3FunctionEqual);
 POCAAddNativeFunction(aContext,POCAMatrix3x3Hash,'notEqual',POCAMatrix3x3FunctionNotEqual);
 POCAAddNativeFunction(aContext,POCAMatrix3x3Hash,'toString',POCAMatrix3x3FunctionToString);

 POCAMatrix3x3HashEvents:=POCANewHash(aContext);
 POCAArrayPush(aContext^.Instance^.Globals.RootArray,POCAMatrix3x3HashEvents);
 POCAAddNativeFunction(aContext,POCAMatrix3x3HashEvents,'__add',POCAMatrix3x3FunctionOpAdd);
 POCAAddNativeFunction(aContext,POCAMatrix3x3HashEvents,'__sub',POCAMatrix3x3FunctionOpSub);
 POCAAddNativeFunction(aContext,POCAMatrix3x3HashEvents,'__mul',POCAMatrix3x3FunctionOpMul);
 POCAAddNativeFunction(aContext,POCAMatrix3x3HashEvents,'__div',POCAMatrix3x3FunctionOpDiv);
 POCAAddNativeFunction(aContext,POCAMatrix3x3HashEvents,'__eq',POCAMatrix3x3FunctionOpEqual);
 POCAAddNativeFunction(aContext,POCAMatrix3x3HashEvents,'__neq',POCAMatrix3x3FunctionOpNotEqual);
 POCAAddNativeFunction(aContext,POCAMatrix3x3HashEvents,'__neg',POCAMatrix3x3FunctionOpNeg);
 POCAAddNativeFunction(aContext,POCAMatrix3x3HashEvents,'__sqrt',POCAMatrix3x3FunctionOpSqrt);
 POCAAddNativeFunction(aContext,POCAMatrix3x3HashEvents,'__tostring',POCAMatrix3x3FunctionOpToString);

end;

procedure POCAInitMatrix3x3Namespace(aContext:PPOCAContext);
var Hash:TPOCAValue;
begin
 Hash:=POCANewHash(aContext);
 POCAArrayPush(aContext^.Instance^.Globals.RootArray,Hash);
 POCAAddNativeFunction(aContext,Hash,'create',POCAMatrix3x3FunctionCREATE);
 POCAHashSetString(aContext,aContext^.Instance^.Globals.Namespace,'Matrix3x3',Hash);
end;

procedure POCAInitMatrix3x3(aContext:PPOCAContext);
var Hash:TPOCAValue;
begin
 POCAInitMatrix3x3Hash(aContext);
 POCAInitMatrix3x3Namespace(aContext);
end;

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Matrix4x4
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

var POCAMatrix4x4Hash,POCAMatrix4x4HashEvents:TPOCAValue;
    
procedure POCAMatrix4x4GhostDestroy(const aGhost:PPOCAGhost);
begin
 if assigned(aGhost) and assigned(aGhost^.Ptr) then begin
  FreeMem(aGhost^.Ptr);
 end;
end;

function POCAMatrix4x4GhostExistKey(const aContext:PPOCAContext;const aGhost:PPOCAGhost;const aKey:TPOCAValue):longbool;
var s:TpvUTF8String;
begin
 s:=POCAGetStringValue(aContext,aKey);
 if s='m00' then begin
  result:=true;
 end else if s='m01' then begin
  result:=true;
 end else if s='m02' then begin
  result:=true;
 end else if s='m03' then begin
  result:=true;
 end else if s='m10' then begin
  result:=true;
 end else if s='m11' then begin
  result:=true;
 end else if s='m12' then begin
  result:=true;
 end else if s='m13' then begin
  result:=true;
 end else if s='m20' then begin
  result:=true;
 end else if s='m21' then begin
  result:=true;
 end else if s='m22' then begin
  result:=true;
 end else if s='m23' then begin
  result:=true;
 end else if s='m30' then begin
  result:=true;
 end else if s='m31' then begin
  result:=true;
 end else if s='m32' then begin
  result:=true;
 end else if s='m33' then begin
  result:=true;
 end else begin
  result:=false;
 end;
end;

function POCAMatrix4x4GhostGetKey(const aContext:PPOCAContext;const aGhost:PPOCAGhost;const aKey:TPOCAValue;out aValue:TPOCAValue):longbool;
var Matrix4x4:PpvMatrix4x4D;
    s:TpvUTF8String;
begin
 Matrix4x4:=PpvMatrix4x4D(PPOCAGhost(aGhost)^.Ptr);
 s:=POCAGetStringValue(aContext,aKey);
 if s='m00' then begin
  aValue.Num:=Matrix4x4^.RawComponents[0,0];
  result:=true;
 end else if s='m01' then begin
  aValue.Num:=Matrix4x4^.RawComponents[0,1];
  result:=true;
 end else if s='m02' then begin
  aValue.Num:=Matrix4x4^.RawComponents[0,2];
  result:=true;
 end else if s='m03' then begin
  aValue.Num:=Matrix4x4^.RawComponents[0,3];
  result:=true;
 end else if s='m10' then begin
  aValue.Num:=Matrix4x4^.RawComponents[1,0];
  result:=true;
 end else if s='m11' then begin
  aValue.Num:=Matrix4x4^.RawComponents[1,1];
  result:=true;
 end else if s='m12' then begin
  aValue.Num:=Matrix4x4^.RawComponents[1,2];
  result:=true;
 end else if s='m13' then begin
  aValue.Num:=Matrix4x4^.RawComponents[1,3];
  result:=true;
 end else if s='m20' then begin
  aValue.Num:=Matrix4x4^.RawComponents[2,0];
  result:=true;
 end else if s='m21' then begin
  aValue.Num:=Matrix4x4^.RawComponents[2,1];
  result:=true;
 end else if s='m22' then begin
  aValue.Num:=Matrix4x4^.RawComponents[2,2];
  result:=true;
 end else if s='m23' then begin
  aValue.Num:=Matrix4x4^.RawComponents[2,3];
  result:=true;
 end else if s='m30' then begin
  aValue.Num:=Matrix4x4^.RawComponents[3,0];
  result:=true;
 end else if s='m31' then begin
  aValue.Num:=Matrix4x4^.RawComponents[3,1];
  result:=true;
 end else if s='m32' then begin
  aValue.Num:=Matrix4x4^.RawComponents[3,2];
  result:=true;
 end else if s='m33' then begin
  aValue.Num:=Matrix4x4^.RawComponents[3,3];
  result:=true;
 end else begin
  result:=false;
 end;
end;

function POCAMatrix4x4GhostSetKey(const aContext:PPOCAContext;const aGhost:PPOCAGhost;const aKey:TPOCAValue;const aValue:TPOCAValue):longbool;
var Matrix4x4:PpvMatrix4x4D;
    s:TpvUTF8String;
begin
 Matrix4x4:=PpvMatrix4x4D(PPOCAGhost(aGhost)^.Ptr);
 s:=POCAGetStringValue(aContext,aKey);
 if s='m00' then begin
  Matrix4x4^.RawComponents[0,0]:=aValue.Num;
  result:=true;
 end else if s='m01' then begin
  Matrix4x4^.RawComponents[0,1]:=aValue.Num;
  result:=true;
 end else if s='m02' then begin
  Matrix4x4^.RawComponents[0,2]:=aValue.Num;
  result:=true;
 end else if s='m03' then begin
  Matrix4x4^.RawComponents[0,3]:=aValue.Num;
  result:=true;
 end else if s='m10' then begin
  Matrix4x4^.RawComponents[1,0]:=aValue.Num;
  result:=true;
 end else if s='m11' then begin
  Matrix4x4^.RawComponents[1,1]:=aValue.Num;
  result:=true;
 end else if s='m12' then begin
  Matrix4x4^.RawComponents[1,2]:=aValue.Num;
  result:=true;
 end else if s='m13' then begin
  Matrix4x4^.RawComponents[1,3]:=aValue.Num;
  result:=true;
 end else if s='m20' then begin
  Matrix4x4^.RawComponents[2,0]:=aValue.Num;
  result:=true;
 end else if s='m21' then begin
  Matrix4x4^.RawComponents[2,1]:=aValue.Num;
  result:=true;
 end else if s='m22' then begin
  Matrix4x4^.RawComponents[2,2]:=aValue.Num;
  result:=true;
 end else if s='m23' then begin
  Matrix4x4^.RawComponents[2,3]:=aValue.Num;
  result:=true;
 end else if s='m30' then begin
  Matrix4x4^.RawComponents[3,0]:=aValue.Num;
  result:=true;
 end else if s='m31' then begin
  Matrix4x4^.RawComponents[3,1]:=aValue.Num;
  result:=true;
 end else if s='m32' then begin
  Matrix4x4^.RawComponents[3,2]:=aValue.Num;
  result:=true;
 end else if s='m33' then begin
  Matrix4x4^.RawComponents[3,3]:=aValue.Num;
  result:=true;
 end else begin
  result:=false;
 end;
end;

const POCAMatrix4x4Ghost:TPOCAGhostType=
       (
        Destroy:POCAMatrix4x4GhostDestroy;
        CanDestroy:nil;
        Mark:nil;
        ExistKey:POCAMatrix4x4GhostExistKey;
        GetKey:POCAMatrix4x4GhostGetKey;
        SetKey:POCAMatrix4x4GhostSetKey;
        Name:'Matrix4x4'
       );

function POCANewMatrix4x4(const aContext:PPOCAContext;const aMatrix4x4:TpvMatrix4x4D):TPOCAValue;
var Matrix4x4:PpvMatrix4x4D;
begin
 GetMem(Matrix4x4,SizeOf(TpvMatrix4x4D));
 Matrix4x4^:=aMatrix4x4;
 result:=POCANewGhost(aContext,@POCAMatrix4x4Ghost,Matrix4x4,nil,pgptRAW);
 POCAGhostSetHashValue(result,POCAMatrix4x4Hash);
end;

function POCANewMatrix4x4(const aContext:PPOCAContext;const aM00:TpvDouble;const aM01:TpvDouble;const aM02:TpvDouble;const aM03:TpvDouble;const aM10:TpvDouble;const aM11:TpvDouble;const aM12:TpvDouble;const aM13:TpvDouble;const aM20:TpvDouble;const aM21:TpvDouble;const aM22:TpvDouble;const aM23:TpvDouble;const aM30:TpvDouble;const aM31:TpvDouble;const aM32:TpvDouble;const aM33:TpvDouble):TPOCAValue;
var Matrix4x4:TpvMatrix4x4D; 
begin 
 Matrix4x4.RawComponents[0,0]:=aM00;
 Matrix4x4.RawComponents[0,1]:=aM01; 
 Matrix4x4.RawComponents[0,2]:=aM02;
 Matrix4x4.RawComponents[0,3]:=aM03;
 Matrix4x4.RawComponents[1,0]:=aM10;
 Matrix4x4.RawComponents[1,1]:=aM11;
 Matrix4x4.RawComponents[1,2]:=aM12;
 Matrix4x4.RawComponents[1,3]:=aM13;
 Matrix4x4.RawComponents[2,0]:=aM20;
 Matrix4x4.RawComponents[2,1]:=aM21;
 Matrix4x4.RawComponents[2,2]:=aM22;
 Matrix4x4.RawComponents[2,3]:=aM23;
 Matrix4x4.RawComponents[3,0]:=aM30;
 Matrix4x4.RawComponents[3,1]:=aM31;
 Matrix4x4.RawComponents[3,2]:=aM32;
 Matrix4x4.RawComponents[3,3]:=aM33;
 result:=POCANewMatrix4x4(aContext,Matrix4x4);
end;

function POCAGetMatrix4x4Value(const aValue:TPOCAValue):TpvMatrix4x4D;
begin
 if POCAGhostGetType(aValue)=@POCAMatrix4x4Ghost then begin
  result:=PpvMatrix4x4D(POCAGhostFastGetPointer(aValue))^;
 end else begin
  result:=TpvMatrix4x4.Create(1.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,1.0);
 end;
end;

function POCAMatrix4x4FunctionCREATE(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4:TpvMatrix4x4D;
    Matrix3x3:PpvMatrix3x3D;
    Quaternion:PpvQuaternionD;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) then begin
  Matrix4x4:=PpvMatrix4x4D(POCAGhostFastGetPointer(aArguments^[0]))^;
 end else if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=POCAMatrix3x3GhostPointer) then begin
  Matrix3x3:=POCAGhostFastGetPointer(aArguments^[0]);
  Matrix4x4.RawComponents[0,0]:=Matrix3x3^.RawComponents[0,0];
  Matrix4x4.RawComponents[0,1]:=Matrix3x3^.RawComponents[0,1];
  Matrix4x4.RawComponents[0,2]:=Matrix3x3^.RawComponents[0,2];
  Matrix4x4.RawComponents[0,3]:=0.0;
  Matrix4x4.RawComponents[1,0]:=Matrix3x3^.RawComponents[1,0];
  Matrix4x4.RawComponents[1,1]:=Matrix3x3^.RawComponents[1,1];
  Matrix4x4.RawComponents[1,2]:=Matrix3x3^.RawComponents[1,2];
  Matrix4x4.RawComponents[1,3]:=0.0;
  Matrix4x4.RawComponents[2,0]:=Matrix3x3^.RawComponents[2,0];
  Matrix4x4.RawComponents[2,1]:=Matrix3x3^.RawComponents[2,1];
  Matrix4x4.RawComponents[2,2]:=Matrix3x3^.RawComponents[2,2];
  Matrix4x4.RawComponents[2,3]:=0.0;
  Matrix4x4.RawComponents[3,0]:=0.0;
  Matrix4x4.RawComponents[3,1]:=0.0;
  Matrix4x4.RawComponents[3,2]:=0.0;
  Matrix4x4.RawComponents[3,3]:=1.0;
 end else if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) then begin
  Quaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  Matrix4x4:=TpvMatrix4x4D.Create(Quaternion^);
 end else begin
  if aCountArguments>0 then begin
   Matrix4x4.RawComponents[0,0]:=POCAGetNumberValue(aContext,aArguments^[0]);
  end else begin
   Matrix4x4.RawComponents[0,0]:=1.0;
  end;
  if aCountArguments>1 then begin
   Matrix4x4.RawComponents[0,1]:=POCAGetNumberValue(aContext,aArguments^[1]);
  end else begin
   Matrix4x4.RawComponents[0,1]:=0.0;
  end;
  if aCountArguments>2 then begin
   Matrix4x4.RawComponents[0,2]:=POCAGetNumberValue(aContext,aArguments^[2]);
  end else begin
   Matrix4x4.RawComponents[0,2]:=0.0;
  end;
  if aCountArguments>3 then begin
   Matrix4x4.RawComponents[0,3]:=POCAGetNumberValue(aContext,aArguments^[3]);
  end else begin
   Matrix4x4.RawComponents[0,3]:=0.0;
  end;
  if aCountArguments>4 then begin
   Matrix4x4.RawComponents[1,0]:=POCAGetNumberValue(aContext,aArguments^[4]);
  end else begin
   Matrix4x4.RawComponents[1,0]:=0.0;
  end;
  if aCountArguments>5 then begin
   Matrix4x4.RawComponents[1,1]:=POCAGetNumberValue(aContext,aArguments^[5]);
  end else begin
   Matrix4x4.RawComponents[1,1]:=1.0;
  end;
  if aCountArguments>6 then begin
   Matrix4x4.RawComponents[1,2]:=POCAGetNumberValue(aContext,aArguments^[6]);
  end else begin
   Matrix4x4.RawComponents[1,2]:=0.0;
  end;
  if aCountArguments>7 then begin
   Matrix4x4.RawComponents[1,3]:=POCAGetNumberValue(aContext,aArguments^[7]);
  end else begin
   Matrix4x4.RawComponents[1,3]:=0.0;
  end;
  if aCountArguments>8 then begin
   Matrix4x4.RawComponents[2,0]:=POCAGetNumberValue(aContext,aArguments^[8]);
  end else begin
   Matrix4x4.RawComponents[2,0]:=0.0;
  end;
  if aCountArguments>9 then begin
   Matrix4x4.RawComponents[2,1]:=POCAGetNumberValue(aContext,aArguments^[9]);
  end else begin
   Matrix4x4.RawComponents[2,1]:=0.0;
  end;
  if aCountArguments>10 then begin
   Matrix4x4.RawComponents[2,2]:=POCAGetNumberValue(aContext,aArguments^[10]);
  end else begin
   Matrix4x4.RawComponents[2,2]:=1.0;
  end;
  if aCountArguments>11 then begin
   Matrix4x4.RawComponents[2,3]:=POCAGetNumberValue(aContext,aArguments^[11]);
  end else begin
   Matrix4x4.RawComponents[2,3]:=0.0;
  end;
  if aCountArguments>12 then begin
   Matrix4x4.RawComponents[3,0]:=POCAGetNumberValue(aContext,aArguments^[12]);
  end else begin
   Matrix4x4.RawComponents[3,0]:=0.0;
  end;
  if aCountArguments>13 then begin
   Matrix4x4.RawComponents[3,1]:=POCAGetNumberValue(aContext,aArguments^[13]);
  end else begin
   Matrix4x4.RawComponents[3,1]:=0.0;
  end;
  if aCountArguments>14 then begin
   Matrix4x4.RawComponents[3,2]:=POCAGetNumberValue(aContext,aArguments^[14]);
  end else begin
   Matrix4x4.RawComponents[3,2]:=0.0;
  end;
  if aCountArguments>15 then begin
   Matrix4x4.RawComponents[3,3]:=POCAGetNumberValue(aContext,aArguments^[15]);
  end else begin
   Matrix4x4.RawComponents[3,3]:=1.0;
  end;  
 end;
 result:=POCANewMatrix4x4(aContext,Matrix4x4);
end;

function POCAMatrix4x4FunctionEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4,OtherMatrix4x4:PpvMatrix4x4D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAMatrix4x4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aThis);
  OtherMatrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewNumber(aContext,ord(Matrix4x4^=OtherMatrix4x4^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionNotEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4,OtherMatrix4x4:PpvMatrix4x4D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAMatrix4x4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aThis);
  OtherMatrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewNumber(aContext,ord(Matrix4x4^<>OtherMatrix4x4^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionToString(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4:PpvMatrix4x4D;
    s:TpvUTF8String;
begin
 if (aCountArguments=0) and (POCAGhostGetType(aThis)=@POCAMatrix4x4Ghost) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aThis);
  s:='[['+ConvertDoubleToString(Matrix4x4^.RawComponents[0,0],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[0,1],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[0,2],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[0,3],omStandard,-1)+'],'+
      '['+ConvertDoubleToString(Matrix4x4^.RawComponents[1,0],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[1,1],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[1,2],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[1,3],omStandard,-1)+'],'+
      '['+ConvertDoubleToString(Matrix4x4^.RawComponents[2,0],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[2,1],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[2,2],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[2,3],omStandard,-1)+'],'+
      '['+ConvertDoubleToString(Matrix4x4^.RawComponents[3,0],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[3,1],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[3,2],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[3,3],omStandard,-1)+']]';
  result:=POCANewString(aContext,s);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionAdd(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4,OtherMatrix4x4:PpvMatrix4x4D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAMatrix4x4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aThis);
  OtherMatrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  Matrix4x4^:=Matrix4x4^+OtherMatrix4x4^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionSub(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4,OtherMatrix4x4:PpvMatrix4x4D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAMatrix4x4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aThis);
  OtherMatrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  Matrix4x4^:=Matrix4x4^-OtherMatrix4x4^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionMul(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4,OtherMatrix4x4:PpvMatrix4x4D;
    Factor:TpvDouble;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAMatrix4x4Ghost) and (POCAGetValueType(aArguments^[0])=pvtNUMBER) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aThis);
  Factor:=POCAGetNumberValue(aContext,aArguments^[0]);
  Matrix4x4^:=Matrix4x4^*Factor;
  result:=aThis;
 end else if (aCountArguments=1) and (POCAGhostGetType(aThis)=@POCAMatrix4x4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aThis);
  OtherMatrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  Matrix4x4^:=Matrix4x4^*OtherMatrix4x4^;
  result:=aThis;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionOpAdd(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4,OtherMatrix4x4:PpvMatrix4x4D;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAMatrix4x4Ghost) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherMatrix4x4:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewMatrix4x4(aContext,Matrix4x4^+OtherMatrix4x4^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionOpSub(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4,OtherMatrix4x4:PpvMatrix4x4D;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAMatrix4x4Ghost) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherMatrix4x4:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewMatrix4x4(aContext,Matrix4x4^-OtherMatrix4x4^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionOpMul(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4,OtherMatrix4x4:PpvMatrix4x4D;
    Vector3:PpvVector3D;
    Quaternion:PpvQuaternionD;
    Factor:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  Factor:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewMatrix4x4(aContext,Matrix4x4^*Factor);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAMatrix4x4Ghost) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherMatrix4x4:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewMatrix4x4(aContext,Matrix4x4^*OtherMatrix4x4^);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) and (POCAGhostGetType(aArguments^[1])=POCAVector3GhostPointer) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  Vector3:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewVector3(aContext,(Matrix4x4^*Vector3^).xyz);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=POCAVector3GhostPointer) and (POCAGhostGetType(aArguments^[1])=@POCAMatrix4x4Ghost) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aArguments^[1]);
  Vector3:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewVector3(aContext,(Vector3^*Matrix4x4^).xyz);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=POCAMatrix4x4GhostPointer) and (POCAGhostGetType(aArguments^[1])=@POCAQuaternionGhost) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  Quaternion:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewMatrix4x4(aContext,Matrix4x4^*Quaternion^);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAQuaternionGhost) and (POCAGhostGetType(aArguments^[1])=POCAMatrix4x4GhostPointer) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aArguments^[1]);
  Quaternion:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewMatrix4x4(aContext,Quaternion^*Matrix4x4^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionOpDiv(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4,OtherMatrix4x4:PpvMatrix4x4D;
    Factor:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  Factor:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewMatrix4x4(aContext,Matrix4x4^/Factor);
 end else if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAMatrix4x4Ghost) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherMatrix4x4:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewMatrix4x4(aContext,Matrix4x4^/OtherMatrix4x4^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionOpNeg(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4:PpvMatrix4x4D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewMatrix4x4(aContext,-Matrix4x4^);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionOpSqrt(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4:TpvMatrix4x4D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) then begin
  Matrix4x4:=PpvMatrix4x4D(POCAGhostFastGetPointer(aArguments^[0]))^;
  Matrix4x4.RawComponents[0,0]:=Sqrt(Matrix4x4.RawComponents[0,0]);
  Matrix4x4.RawComponents[0,1]:=Sqrt(Matrix4x4.RawComponents[0,1]);
  Matrix4x4.RawComponents[0,2]:=Sqrt(Matrix4x4.RawComponents[0,2]);
  Matrix4x4.RawComponents[1,0]:=Sqrt(Matrix4x4.RawComponents[1,0]);
  Matrix4x4.RawComponents[1,1]:=Sqrt(Matrix4x4.RawComponents[1,1]);
  Matrix4x4.RawComponents[1,2]:=Sqrt(Matrix4x4.RawComponents[1,2]);
  Matrix4x4.RawComponents[2,0]:=Sqrt(Matrix4x4.RawComponents[2,0]);
  Matrix4x4.RawComponents[2,1]:=Sqrt(Matrix4x4.RawComponents[2,1]);
  Matrix4x4.RawComponents[2,2]:=Sqrt(Matrix4x4.RawComponents[2,2]);
  result:=POCANewMatrix4x4(aContext,Matrix4x4);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionOpToString(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4:PpvMatrix4x4D;
    s:TpvUTF8String;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  s:='[['+ConvertDoubleToString(Matrix4x4^.RawComponents[0,0],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[0,1],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[0,2],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[0,3],omStandard,-1)+'],'+
      '['+ConvertDoubleToString(Matrix4x4^.RawComponents[1,0],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[1,1],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[1,2],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[1,3],omStandard,-1)+'],'+
      '['+ConvertDoubleToString(Matrix4x4^.RawComponents[2,0],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[2,1],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[2,2],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[2,3],omStandard,-1)+'],'+
      '['+ConvertDoubleToString(Matrix4x4^.RawComponents[3,0],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[3,1],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[3,2],omStandard,-1)+','+ConvertDoubleToString(Matrix4x4^.RawComponents[3,3],omStandard,-1)+']]';
  result:=POCANewString(aContext,s);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionOpEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4,OtherMatrix4x4:PpvMatrix4x4D;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAMatrix4x4Ghost) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherMatrix4x4:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewNumber(aContext,ord(Matrix4x4^=OtherMatrix4x4^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionOpNotEqual(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4,OtherMatrix4x4:PpvMatrix4x4D;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAMatrix4x4Ghost) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  OtherMatrix4x4:=POCAGhostFastGetPointer(aArguments^[1]);
  result:=POCANewNumber(aContext,ord(Matrix4x4^<>OtherMatrix4x4^) and 1);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionTranspose(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4:PpvMatrix4x4D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewMatrix4x4(aContext,Matrix4x4^.Transpose);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionDeterminant(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4:PpvMatrix4x4D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  result.Num:=Matrix4x4^.Determinant;
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionInverse(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4:PpvMatrix4x4D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewMatrix4x4(aContext,Matrix4x4^.Inverse);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionAdjugate(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4:PpvMatrix4x4D;
begin
 if (aCountArguments=1) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  result:=POCANewMatrix4x4(aContext,Matrix4x4^.Adjugate);
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionLerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4,OtherMatrix4x4:PpvMatrix4x4D;
    Time:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aThis)=@POCAMatrix4x4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aThis);
  OtherMatrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  Time:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewMatrix4x4(aContext,Matrix4x4^.Lerp(OtherMatrix4x4^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionNlerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4,OtherMatrix4x4:PpvMatrix4x4D;
    Time:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aThis)=@POCAMatrix4x4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aThis);
  OtherMatrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  Time:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewMatrix4x4(aContext,Matrix4x4^.Nlerp(OtherMatrix4x4^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionSlerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4,OtherMatrix4x4:PpvMatrix4x4D;
    Time:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aThis)=@POCAMatrix4x4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aThis);
  OtherMatrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  Time:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewMatrix4x4(aContext,Matrix4x4^.Slerp(OtherMatrix4x4^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionElerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var Matrix4x4,OtherMatrix4x4:PpvMatrix4x4D;
    Time:TpvDouble;
begin
 if (aCountArguments=2) and (POCAGhostGetType(aThis)=@POCAMatrix4x4Ghost) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) and (POCAGetValueType(aArguments^[1])=pvtNUMBER) then begin
  Matrix4x4:=POCAGhostFastGetPointer(aThis);
  OtherMatrix4x4:=POCAGhostFastGetPointer(aArguments^[0]);
  Time:=POCAGetNumberValue(aContext,aArguments^[1]);
  result:=POCANewMatrix4x4(aContext,Matrix4x4^.Elerp(OtherMatrix4x4^,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

function POCAMatrix4x4FunctionSqlerp(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:pointer):TPOCAValue;
var A,B,C,D:TpvMatrix4x4D;
    Time:TpvDouble;
begin
 if (aCountArguments=4) and (POCAGhostGetType(aArguments^[0])=@POCAMatrix4x4Ghost) and (POCAGhostGetType(aArguments^[1])=@POCAMatrix4x4Ghost) and (POCAGhostGetType(aArguments^[2])=@POCAMatrix4x4Ghost) and (POCAGetValueType(aArguments^[3])=pvtNUMBER) then begin
  A:=PpvMatrix4x4D(POCAGhostFastGetPointer(aThis))^;
  B:=PpvMatrix4x4D(POCAGhostFastGetPointer(aArguments^[0]))^;
  C:=PpvMatrix4x4D(POCAGhostFastGetPointer(aArguments^[1]))^;
  D:=PpvMatrix4x4D(POCAGhostFastGetPointer(aArguments^[2]))^;
  Time:=POCAGetNumberValue(aContext,aArguments^[3]);
  result:=POCANewMatrix4x4(aContext,A.Sqlerp(B,C,D,Time));
 end else begin
  result:=POCAValueNull;
 end;
end;

procedure POCAInitMatrix4x4Hash(aContext:PPOCAContext);
begin

 POCAMatrix4x4GhostPointer:=@POCAMatrix4x4Ghost;

 POCAMatrix4x4Hash:=POCANewHash(aContext);
 POCAArrayPush(aContext^.Instance^.Globals.RootArray,POCAMatrix4x4Hash);
 POCAAddNativeFunction(aContext,POCAMatrix4x4Hash,'add',POCAMatrix4x4FunctionAdd);
 POCAAddNativeFunction(aContext,POCAMatrix4x4Hash,'sub',POCAMatrix4x4FunctionSub);
 POCAAddNativeFunction(aContext,POCAMatrix4x4Hash,'mul',POCAMatrix4x4FunctionMul);
 POCAAddNativeFunction(aContext,POCAMatrix4x4Hash,'transpose',POCAMatrix4x4FunctionTranspose);
 POCAAddNativeFunction(aContext,POCAMatrix4x4Hash,'determinant',POCAMatrix4x4FunctionDeterminant);
 POCAAddNativeFunction(aContext,POCAMatrix4x4Hash,'inverse',POCAMatrix4x4FunctionInverse);
 POCAAddNativeFunction(aContext,POCAMatrix4x4Hash,'adjugate',POCAMatrix4x4FunctionAdjugate);
 POCAAddNativeFunction(aContext,POCAMatrix4x4Hash,'lerp',POCAMatrix4x4FunctionLerp);
 POCAAddNativeFunction(aContext,POCAMatrix4x4Hash,'nlerp',POCAMatrix4x4FunctionNlerp);
 POCAAddNativeFunction(aContext,POCAMatrix4x4Hash,'slerp',POCAMatrix4x4FunctionSlerp);
 POCAAddNativeFunction(aContext,POCAMatrix4x4Hash,'sqlerp',POCAMatrix4x4FunctionSqlerp);
 POCAAddNativeFunction(aContext,POCAMatrix4x4Hash,'equal',POCAMatrix4x4FunctionEqual);
 POCAAddNativeFunction(aContext,POCAMatrix4x4Hash,'notEqual',POCAMatrix4x4FunctionNotEqual);
 POCAAddNativeFunction(aContext,POCAMatrix4x4Hash,'toString',POCAMatrix4x4FunctionToString);

 POCAMatrix4x4HashEvents:=POCANewHash(aContext);
 POCAArrayPush(aContext^.Instance^.Globals.RootArray,POCAMatrix4x4HashEvents);
 POCAAddNativeFunction(aContext,POCAMatrix4x4HashEvents,'__add',POCAMatrix4x4FunctionOpAdd);
 POCAAddNativeFunction(aContext,POCAMatrix4x4HashEvents,'__sub',POCAMatrix4x4FunctionOpSub);
 POCAAddNativeFunction(aContext,POCAMatrix4x4HashEvents,'__mul',POCAMatrix4x4FunctionOpMul);
 POCAAddNativeFunction(aContext,POCAMatrix4x4HashEvents,'__div',POCAMatrix4x4FunctionOpDiv);
 POCAAddNativeFunction(aContext,POCAMatrix4x4HashEvents,'__eq',POCAMatrix4x4FunctionOpEqual);
 POCAAddNativeFunction(aContext,POCAMatrix4x4HashEvents,'__neq',POCAMatrix4x4FunctionOpNotEqual);
 POCAAddNativeFunction(aContext,POCAMatrix4x4HashEvents,'__neg',POCAMatrix4x4FunctionOpNeg);
 POCAAddNativeFunction(aContext,POCAMatrix4x4HashEvents,'__sqrt',POCAMatrix4x4FunctionOpSqrt);
 POCAAddNativeFunction(aContext,POCAMatrix4x4HashEvents,'__tostring',POCAMatrix4x4FunctionOpToString);

end;

procedure POCAInitMatrix4x4Namespace(aContext:PPOCAContext);
var Hash:TPOCAValue;
begin
 Hash:=POCANewHash(aContext);
 POCAArrayPush(aContext^.Instance^.Globals.RootArray,Hash);
 POCAAddNativeFunction(aContext,Hash,'create',POCAMatrix4x4FunctionCREATE);
 POCAHashSetString(aContext,aContext^.Instance^.Globals.Namespace,'Matrix4x4',Hash);
end;

procedure POCAInitMatrix4x4(aContext:PPOCAContext);
var Hash:TPOCAValue;
begin
 POCAInitMatrix4x4Hash(aContext);
 POCAInitMatrix4x4Namespace(aContext);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Initialization
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

procedure InitializeForPOCAContext(const aContext:PPOCAContext);
begin
 POCAInitVector2(aContext);
 POCAInitVector3(aContext);
 POCAInitVector4(aContext);
 POCAInitQuaternion(aContext);
 POCAInitMatrix3x3(aContext);
 POCAInitMatrix4x4(aContext);
end;

end.

