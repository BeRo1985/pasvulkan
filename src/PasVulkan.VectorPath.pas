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
unit PasVulkan.VectorPath;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

interface

uses SysUtils,
     Classes,
     Math,
     Generics.Collections,
     Vulkan,
     PasDblStrUtils,
     PasMP,
     PasVulkan.Types,
     PasVulkan.Math;

type PpvVectorPathCommandType=^TpvVectorPathCommandType;
     TpvVectorPathCommandType=
      (
       MoveTo,
       LineTo,
       QuadraticCurveTo,
       CubicCurveTo,
       Close
      );

     TpvVectorPathVector=record
      public
       x:TpvDouble;
       y:TpvDouble;
       constructor Create(const aValue:TpvDouble); overload;
       constructor Create(const aX,aY:TpvDouble); overload;
       function Length:TpvDouble;
       function LengthSquared:TpvDouble;
       function Direction:TpvDouble;
       function Normalize:TpvVectorPathVector;
       function Dot(const aRight:TpvVectorPathVector):TpvDouble;
       function Cross(const aRight:TpvVectorPathVector):TpvDouble;
       function OrthoNormal:TpvVectorPathVector;
       function Lerp(const b:TpvVectorPathVector;const t:TpvDouble):TpvVectorPathVector;
       class operator Equal(const a,b:TpvVectorPathVector):boolean;
       class operator NotEqual(const a,b:TpvVectorPathVector):boolean;
       class operator Add(const a,b:TpvVectorPathVector):TpvVectorPathVector;
       class operator Subtract(const a,b:TpvVectorPathVector):TpvVectorPathVector;
       class operator Multiply(const a,b:TpvVectorPathVector):TpvVectorPathVector; overload;
       class operator Multiply(const a:TpvVectorPathVector;const b:TpvDouble):TpvVectorPathVector; overload;
       class operator Divide(const a,b:TpvVectorPathVector):TpvVectorPathVector; overload;
       class operator Divide(const a:TpvVectorPathVector;const b:TpvDouble):TpvVectorPathVector; overload;
       class operator Negative(const a:TpvVectorPathVector):TpvVectorPathVector;
       class operator Positive(const a:TpvVectorPathVector):TpvVectorPathVector;
     end;

     PpvVectorPathVector=^TpvVectorPathVector;

     TpvVectorPathVectorArray=array of TpvVectorPathVector;

     TpvVectorPathVectors=array[0..65535] of TpvVectorPathVector;

     PpvVectorPathVectors=^TpvVectorPathVectors;

     TpvVectorPathCommand=class
      private
       fCommandType:TpvVectorPathCommandType;
       fX0:TpvDouble;
       fY0:TpvDouble;
       fX1:TpvDouble;
       fY1:TpvDouble;
       fX2:TpvDouble;
       fY2:TpvDouble;
      public
       constructor Create(const aCommandType:TpvVectorPathCommandType;
                          const aX0:TpvDouble=0.0;
                          const aY0:TpvDouble=0.0;
                          const aX1:TpvDouble=0.0;
                          const aY1:TpvDouble=0.0;
                          const aX2:TpvDouble=0.0;
                          const aY2:TpvDouble=0.0); reintroduce;
      published
       property CommandType:TpvVectorPathCommandType read fCommandType write fCommandType;
       property x0:TpvDouble read fX0 write fX0;
       property y0:TpvDouble read fY0 write fY0;
       property x1:TpvDouble read fX1 write fX1;
       property y1:TpvDouble read fY1 write fY1;
       property x2:TpvDouble read fX2 write fX2;
       property y2:TpvDouble read fY2 write fY2;
     end;

     TpvVectorPathCommandList=class(TObjectList<TpvVectorPathCommand>);

     PpvVectorPathFillRule=^TpvVectorPathFillRule;
     TpvVectorPathFillRule=
      (
       NonZero,
       EvenOdd
      );

     { TpvVectorPath }

     TpvVectorPath=class
      private
       fCommands:TpvVectorPathCommandList;
       fFillRule:TpvVectorPathFillRule;
      public
       constructor Create; reintroduce;
       constructor CreateFromSVGPath(const aCommands:TpvRawByteString);
       destructor Destroy; override;
       procedure Assign(const aFrom:TpvVectorPath);
       procedure MoveTo(const aX,aY:TpvDouble);
       procedure LineTo(const aX,aY:TpvDouble);
       procedure QuadraticCurveTo(const aCX,aCY,aAX,aAY:TpvDouble);
       procedure CubicCurveTo(const aC0X,aC0Y,aC1X,aC1Y,aAX,aAY:TpvDouble);
       procedure Close;
       procedure ConvertCubicCurvesToQuadraticCurves;
       function GetSignedDistance(const aX,aY,aScale:TpvDouble;out aInsideOutsideSign:TpvInt32):TpvDouble;
      published
       property FillRule:TpvVectorPathFillRule read fFillRule write fFillRule;
       property Commands:TpvVectorPathCommandList read fCommands;
     end;

implementation

constructor TpvVectorPathVector.Create(const aValue:TpvDouble);
begin
 x:=aValue;
 y:=aValue;
end;

constructor TpvVectorPathVector.Create(const aX,aY:TpvDouble);
begin
 x:=aX;
 y:=aY;
end;

function TpvVectorPathVector.Length:TpvDouble;
begin
 result:=sqrt(sqr(x)+sqr(y));
end;

function TpvVectorPathVector.LengthSquared:TpvDouble;
begin
 result:=sqr(x)+sqr(y);
end;

function TpvVectorPathVector.Direction:TpvDouble;
begin
 result:=ArcTan2(y,x);
end;

function TpvVectorPathVector.Normalize:TpvVectorPathVector;
var Len:TpvDouble;
begin
 Len:=Length;
 if IsZero(Len) then begin
  result.x:=0.0;
  result.y:=0.0;
 end else begin
  result.x:=x/Len;
  result.y:=y/Len;
 end;
end;

function TpvVectorPathVector.Dot(const aRight:TpvVectorPathVector): TpvDouble;
begin
 result:=(x*aRight.x)+(y*aRight.y);
end;

function TpvVectorPathVector.Cross(const aRight:TpvVectorPathVector): TpvDouble;
begin
 result:=(x*aRight.y)-(y*aRight.x);
end;

function TpvVectorPathVector.OrthoNormal:TpvVectorPathVector;
var Len:TpvDouble;
begin
 Len:=Length;
 if IsZero(Len) then begin
  result.x:=0.0;
  result.y:=0.0;
 end else begin
  result.x:=y/Len;
  result.y:=(-x)/Len;
 end;
end;

function TpvVectorPathVector.Lerp(const b:TpvVectorPathVector;const t:TpvDouble):TpvVectorPathVector;
begin
 result.x:=(x*(1.0-t))+(b.x*t);
 result.y:=(y*(1.0-t))+(b.y*t);
end;

class operator TpvVectorPathVector.Equal(const a,b:TpvVectorPathVector): boolean;
begin
 result:=SameValue(a.x,b.x) and SameValue(a.y,b.y);
end;

class operator TpvVectorPathVector.NotEqual(const a,b:TpvVectorPathVector): boolean;
begin
 result:=(not SameValue(a.x,b.x)) or (not SameValue(a.y,b.y));
end;

class operator TpvVectorPathVector.Add(const a,b:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=a.x+b.x;
 result.y:=a.y+b.y;
end;

class operator TpvVectorPathVector.Subtract(const a,b:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=a.x-b.x;
 result.y:=a.y-b.y;
end;

class operator TpvVectorPathVector.Multiply(const a,b:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=a.x*b.x;
 result.y:=a.y*b.y;
end;

class operator TpvVectorPathVector.Multiply(const a:TpvVectorPathVector;const b:TpvDouble):TpvVectorPathVector;
begin
 result.x:=a.x*b;
 result.y:=a.y*b;
end;

class operator TpvVectorPathVector.Divide(const a,b:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=a.x/b.x;
 result.y:=a.y/b.y;
end;

class operator TpvVectorPathVector.Divide(const a:TpvVectorPathVector;const b:TpvDouble):TpvVectorPathVector;
begin
 result.x:=a.x/b;
 result.y:=a.y/b;
end;

class operator TpvVectorPathVector.Negative(const a:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=-a.x;
 result.y:=-a.y;
end;

class operator TpvVectorPathVector.Positive(const a:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=a.x;
 result.y:=a.y;
end;

constructor TpvVectorPathCommand.Create(const aCommandType:TpvVectorPathCommandType;
                                        const aX0:TpvDouble=0.0;
                                        const aY0:TpvDouble=0.0;
                                        const aX1:TpvDouble=0.0;
                                        const aY1:TpvDouble=0.0;
                                        const aX2:TpvDouble=0.0;
                                        const aY2:TpvDouble=0.0);
begin
 inherited Create;
 fCommandType:=aCommandType;
 fX0:=aX0;
 fY0:=aY0;
 fX1:=aX1;
 fY1:=aY1;
 fX2:=aX2;
 fY2:=aY2;
end;

constructor TpvVectorPath.Create;
begin
 inherited Create;
 fCommands:=TpvVectorPathCommandList.Create(true);
 fFillRule:=TpvVectorPathFillRule.EvenOdd;
end;

constructor TpvVectorPath.CreateFromSVGPath(const aCommands:TpvRawByteString);
var i,SrcPos,SrcLen,large_arc_flag,sweep_flag:TpvInt32;
    lx,ly,lcx,lcy,x0,y0,x1,y1,x2,y2,lmx,lmy,rx,ry,x_axis_rotation,x,y:TpvDouble;
    Src:TpvRawByteString;
    Command,LastCommand:AnsiChar;
 procedure SkipBlank;
 begin
  while (SrcPos<=SrcLen) and (Src[SrcPos] in [#0..#32]) do begin
   inc(SrcPos);
  end;
 end;
 function GetFloat:TpvDouble;
 var StartPos:TpvInt32;
 begin
  SkipBlank;
  StartPos:=SrcPos;
  if (SrcPos<=SrcLen) and (Src[SrcPos] in ['-','+']) then begin
   inc(SrcPos);
  end;
  while (SrcPos<=SrcLen) and (Src[SrcPos] in ['0'..'9']) do begin
   inc(SrcPos);
  end;
  if (SrcPos<=SrcLen) and (Src[SrcPos] in ['.']) then begin
   inc(SrcPos);
   while (SrcPos<=SrcLen) and (Src[SrcPos] in ['0'..'9']) do begin
    inc(SrcPos);
   end;
  end;
  if (SrcPos<=SrcLen) and (Src[SrcPos] in ['e','E']) then begin
   inc(SrcPos);
   if (SrcPos<=SrcLen) and (Src[SrcPos] in ['-','+']) then begin
    inc(SrcPos);
   end;
   while (SrcPos<=SrcLen) and (Src[SrcPos] in ['0'..'9']) do begin
    inc(SrcPos);
   end;
  end;
  if StartPos<SrcPos then begin
   result:=ConvertStringToDouble(TpvRawByteString(copy(String(Src),StartPos,SrcPos-StartPos)),rmNearest,nil,-1);
  end else begin
   result:=0.0;
  end;
  SkipBlank;
 end;
 function GetInt:TpvInt32;
 var s:TpvRawByteString;
 begin
  SkipBlank;
  s:='';
  if (SrcPos<=SrcLen) and (Src[SrcPos] in ['-','+']) then begin
   s:=s+Src[SrcPos];
   inc(SrcPos);
  end;
  while (SrcPos<=SrcLen) and (Src[SrcPos] in ['0'..'9']) do begin
   s:=s+Src[SrcPos];
   inc(SrcPos);
  end;
  if (SrcPos<=SrcLen) and (Src[SrcPos] in ['.']) then begin
   inc(SrcPos);
   while (SrcPos<=SrcLen) and (Src[SrcPos] in ['0'..'9']) do begin
    inc(SrcPos);
   end;
  end;
  if (SrcPos<=SrcLen) and (Src[SrcPos] in ['e','E']) then begin
   inc(SrcPos);
   if (SrcPos<=SrcLen) and (Src[SrcPos] in ['-','+']) then begin
    inc(SrcPos);
   end;
   while (SrcPos<=SrcLen) and (Src[SrcPos] in ['0'..'9']) do begin
    inc(SrcPos);
   end;
  end;
  SkipBlank;
  result:=Trunc(ConvertStringToDouble(s,rmNearest,nil,-1));
 end;
 procedure ConvertArcToCubicCurves(rx,ry,x_axis_rotation:TpvDouble;large_arc_flag,sweep_flag:TpvInt32;x,y:TpvDouble);
 var sin_th,cos_th,a00,a01,a10,a11,x0,y0,x1,y1,xc,yc,d,sfactor,sfactor_sq,
     th0,th1,th_arc,dx,dy,dx1,dy1,Pr1,Pr2,Px,Py,check:TpvDouble;
     i,n_segs:TpvInt32;
  procedure ProcessSegment(xc,yc,th0,th1,rx,ry,x_axis_rotation:TpvDouble);
  var sin_th,cos_th,a00,a01,a10,a11,x1,y1,x2,y2,x3,y3,t,th_half:TpvDouble;
      i:TpvInt32;
  begin
   sin_th:=sin(x_axis_rotation*deg2rad);
   cos_th:=cos(x_axis_rotation*deg2rad);
   a00:=cos_th*rx;
   a01:=(-sin_th)*ry;
   a10:=sin_th*rx;
   a11:=cos_th*ry;
   th_half:=0.5*(th1-th0);
   t:=(8.0/3.0)*sin(th_half*0.5)*sin(th_half*0.5)/sin(th_half);
   x1:=(xc+cos(th0))-(t*sin(th0));
   y1:=(yc+sin(th0))+(t*cos(th0));
   x3:=xc+cos(th1);
   y3:=yc+sin(th1);
   x2:=x3+(t*sin(th1));
   y2:=y3-(t*cos(th1));
   CubicCurveTo((a00*x1)+(a01*y1),(a10*x1)+(a11*y1),
                (a00*x2)+(a01*y2),(a10*x2)+(a11*y2),
                (a00*x3)+(a01*y3),(a10*x3)+(a11*y3));
  end;
 begin
  if (abs(rx)<1e-14) or (abs(ry)<1e-14) then begin
   LineTo(x,y);
   exit;
  end;
  sin_th:=sin(x_axis_rotation*deg2rad);
  cos_th:=cos(x_axis_rotation*deg2rad);
  dx:=(lx-x)*0.5;
  dy:=(ly-y)*0.5;
  dx1:=(cos_th*dx)+(sin_th*dy);
  dy1:=(cos_th*dy)-(sin_th*dx);
  Pr1:=sqr(rx);
  Pr2:=sqr(ry);
  Px:=sqr(dx1);
  Py:=sqr(dy1);
  check:=(Px/Pr1)+(Py/Pr2);
  if check>1.0 then begin
   rx:=rx*sqrt(check);
   ry:=ry*sqrt(check);
  end;
  a00:=cos_th/rx;
  a01:=sin_th/rx;
  a10:=(-sin_th)/ry;
  a11:=cos_th/ry;
  x0:=(a00*lx)+(a01*ly);
  y0:=(a10*lx)+(a11*ly);
  x1:=(a00*x)+(a01*y);
  y1:=(a10*x)+(a11*y);
  d:=sqr(x1-x0)+sqr(y1-y0);
  sfactor_sq:=(1.0/d)-0.25;
  if sfactor_sq<0.0 then begin
   sfactor_sq:=0.0;
  end;
  sfactor:=sqrt(sfactor_sq);
  if sweep_flag=large_arc_flag then begin
   sfactor:=-sfactor;
  end;
  xc:=(0.5*(x0+x1))-(sfactor*(y1-y0));
  yc:=(0.5*(y0+y1))+(sfactor*(x1-x0));
  th0:=arctan2(y0-yc,x0-xc);
  th1:=arctan2(y1-yc,x1-xc);
  th_arc:=th1-th0;
  if (th_arc<0.0) and (sweep_flag<>0) then begin
   th_arc:=th_arc+TwoPI;
  end else if (th_arc>0.0) and (sweep_flag=0) then begin
   th_arc:=th_arc-TwoPI;
  end;
  n_segs:=ceil(abs(th_arc/((pi*0.5)+0.001)));
  for i:=0 to n_segs-1 do begin
   ProcessSegment(xc,yc,
                  th0+((i*th_arc)/n_segs),
                  th0+(((i+1)*th_arc)/n_segs),
                  rx,ry,
                  x_axis_rotation);
  end;
  if n_segs=0 then begin
   LineTo(x,y);
  end;
 end;
begin
 Create;
 SrcLen:=length(aCommands);
 Src:=aCommands;
 for i:=1 to SrcLen do begin
  if Src[i] in [#0..#32,','] then begin
   Src[i]:=' ';
  end;
 end;
 lx:=0;
 ly:=0;
 lcx:=0;
 lcy:=0;
 lmx:=0;
 lmy:=0;
 SrcPos:=1;
 Command:=#0;
 LastCommand:=#0;
 while SrcPos<=SrcLen do begin
  SkipBlank;
  if SrcPos<=SrcLen then begin
   if Src[SrcPos] in ['A'..'Z','a'..'z'] then begin
    Command:=Src[SrcPos];
    inc(SrcPos);
    SkipBlank;
   end;
   case Command of
    'Z','z':begin
     Close;
     lx:=lmx;
     ly:=lmy;
    end;
    'H':begin
     lx:=GetFloat;
     LineTo(lx,ly);
    end;
    'h':begin
     lx:=lx+GetFloat;
     LineTo(lx,ly);
    end;
    'V':begin
     ly:=GetFloat;
     LineTo(lx,ly);
    end;
    'v':begin
     ly:=ly+GetFloat;
     LineTo(lx,ly);
    end;
    'M':begin
     lx:=GetFloat;
     ly:=GetFloat;
     lmx:=lx;
     lmy:=ly;
     MoveTo(lx,ly);
     Command:='L';
    end;
    'm':begin
     lx:=lx+GetFloat;
     ly:=ly+GetFloat;
     lmx:=lx;
     lmy:=ly;
     MoveTo(lx,ly);
     Command:='l';
    end;
    'L':begin
     lx:=GetFloat;
     ly:=GetFloat;
     LineTo(lx,ly);
    end;
    'l':begin
     lx:=lx+GetFloat;
     ly:=ly+GetFloat;
     LineTo(lx,ly);
    end;
    'A':begin
     rx:=GetFloat;
     ry:=GetFloat;
     x_axis_rotation:=GetFloat;
     large_arc_flag:=GetInt;
     sweep_flag:=GetInt;
     x:=GetFloat;
     y:=GetFloat;
     ConvertArcToCubicCurves(rx,ry,x_axis_rotation,large_arc_flag,sweep_flag,x,y);
     lx:=x;
     ly:=y;
    end;
    'a':begin
     rx:=GetFloat;
     ry:=GetFloat;
     x_axis_rotation:=GetFloat;
     large_arc_flag:=GetInt;
     sweep_flag:=GetInt;
     x:=lx+GetFloat;
     y:=ly+GetFloat;
     ConvertArcToCubicCurves(rx,ry,x_axis_rotation,large_arc_flag,sweep_flag,x,y);
     lx:=x;
     ly:=y;
    end;
    'T':begin
     lcx:=lx+(lx-lcx);
     lcy:=ly+(ly-lcy);
     if not (LastCommand in ['T','t','Q','q']) then begin
      lcx:=lx;
      lcy:=ly;
     end;
     lx:=GetFloat;
     ly:=GetFloat;
     QuadraticCurveTo(lcx,lcy,lx,ly);
    end;
    't':begin
     lcx:=lx+(lx-lcx);
     lcy:=ly+(ly-lcy);
     if not (LastCommand in ['T','t','Q','q']) then begin
      lcx:=lx;
      lcy:=ly;
     end;
     lx:=lx+GetFloat;
     ly:=ly+GetFloat;
     QuadraticCurveTo(lcx,lcy,lx,ly);
    end;
    'S':begin
     x0:=lx+(lx-lcx);
     y0:=ly+(ly-lcy);
     if not (LastCommand in ['S','s','C','c']) then begin
      x0:=lx;
      y0:=ly;
     end;
     x1:=GetFloat;
     y1:=GetFloat;
     x2:=GetFloat;
     y2:=GetFloat;
     lcx:=x1;
     lcy:=y1;
     lx:=x2;
     ly:=y2;
     CubicCurveTo(x0,y0,x1,y1,x2,y2);
    end;
    's':begin
     x0:=lx+(lx-lcx);
     y0:=ly+(ly-lcy);
     if not (LastCommand in ['S','s','C','c']) then begin
      x0:=lx;
      y0:=ly;
     end;
     x1:=lx+GetFloat;
     y1:=ly+GetFloat;
     x2:=lx+GetFloat;
     y2:=ly+GetFloat;
     lcx:=x1;
     lcy:=y1;
     lx:=x2;
     ly:=y2;
     CubicCurveTo(x0,y0,x1,y1,x2,y2);
    end;
    'Q':begin
     x0:=GetFloat;
     y0:=GetFloat;
     x1:=GetFloat;
     y1:=GetFloat;
     lcx:=x0;
     lcy:=y0;
     lx:=x1;
     ly:=y1;
     QuadraticCurveTo(x0,y0,x1,y1);
    end;
    'q':begin
     x0:=lx+GetFloat;
     y0:=ly+GetFloat;
     x1:=lx+GetFloat;
     y1:=ly+GetFloat;
     lcx:=x0;
     lcy:=y0;
     lx:=x1;
     ly:=y1;
     QuadraticCurveTo(x0,y0,x1,y1);
    end;
    'C':begin
     x0:=GetFloat;
     y0:=GetFloat;
     x1:=GetFloat;
     y1:=GetFloat;
     x2:=GetFloat;
     y2:=GetFloat;
     lcx:=x1;
     lcy:=y1;
     lx:=x2;
     ly:=y2;
     CubicCurveTo(x0,y0,x1,y1,x2,y2);
    end;
    'c':begin
     x0:=lx+GetFloat;
     y0:=ly+GetFloat;
     x1:=lx+GetFloat;
     y1:=ly+GetFloat;
     x2:=lx+GetFloat;
     y2:=ly+GetFloat;
     lcx:=x1;
     lcy:=y1;
     lx:=x2;
     ly:=y2;
     CubicCurveTo(x0,y0,x1,y1,x2,y2);
    end;
    else begin
     break;
    end;
   end;
  end else begin
   break;
  end;
 end;
end;

destructor TpvVectorPath.Destroy;
begin
 FreeAndNil(fCommands);
 inherited Destroy;
end;

procedure TpvVectorPath.Assign(const aFrom:TpvVectorPath);
var Index:TpvSizeInt;
    SrcCmd:TpvVectorPathCommand;
begin
 fCommands.Clear;
 for Index:=0 to aFrom.fCommands.Count-1 do begin
  SrcCmd:=aFrom.fCommands[Index];
  fCommands.Add(TpvVectorPathCommand.Create(SrcCmd.fCommandType,SrcCmd.fX0,SrcCmd.fY0,SrcCmd.fX1,SrcCmd.fY1,SrcCmd.fX2,SrcCmd.fY2));
 end;
 fFillRule:=aFrom.fFillRule;
end;

procedure TpvVectorPath.MoveTo(const aX,aY:TpvDouble);
begin
 fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.MoveTo,aX,aY));
end;

procedure TpvVectorPath.LineTo(const aX,aY:TpvDouble);
begin
 fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.LineTo,aX,aY));
end;

procedure TpvVectorPath.QuadraticCurveTo(const aCX,aCY,aAX,aAY:TpvDouble);
begin
 fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.QuadraticCurveTo,aCX,aCY,aAX,aAY));
end;

procedure TpvVectorPath.CubicCurveTo(const aC0X,aC0Y,aC1X,aC1Y,aAX,aAY:TpvDouble);
begin
 fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.CubicCurveTo,aC0X,aC0Y,aC1X,aC1Y,aAX,aAY));
end;

procedure TpvVectorPath.Close;
begin
 fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.Close));
end;

procedure TpvVectorPath.ConvertCubicCurvesToQuadraticCurves;
const ValueOne=1.0;
      NearlyZeroValue=ValueOne/TpvInt64(1 shl 18);
var Index:TpvSizeInt;
    OldCommands:TpvVectorPathCommandList;
    OldCommand:TpvVectorPathCommand;
    StartX,StartY,LastX,LastY:TpvDouble;
 procedure ConvertCubicCurveToQuadraticCurve(const aX0,aY0,aX1,aY1,aX2,aY2:TpvDouble);
 const MaxChoppedPoints=10;
 type TChoppedPoints=array[0..MaxChoppedPoints-1] of TpvVectorPathVector;
 var ChoppedPoints:TChoppedPoints;
  procedure OutputLine(const aP0,aP1:TpvVectorPathVector);
  begin
   if (not SameValue(LastX,aP0.x)) or (not SameValue(LastY,aP0.y)) then begin
    fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.MoveTo,aP0.x,aP0.y));
   end;
   fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.MoveTo,aP1.x,aP1.y));
   LastX:=aP1.x;
   LastY:=aP1.y;
  end;
  procedure OutputQuad(const aP0,aP1,aP2:TpvVectorPathVector);
  begin
   if (not SameValue(LastX,aP0.x)) or (not SameValue(LastY,aP0.y)) then begin
    fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.MoveTo,aP0.x,aP0.y));
   end;
   fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.QuadraticCurveTo,aP1.x,aP1.y,aP2.x,aP2.y));
   LastX:=aP2.x;
   LastY:=aP2.y;
  end;
  procedure ChopCubicAt(Src,Dst:PpvVectorPathVectors;const t:TpvDouble); overload;
  var p0,p1,p2,p3,ab,bc,cd,abc,bcd,abcd:TpvVectorPathVector;
  begin
   if SameValue(t,1.0) then begin
    Dst^[0]:=Src^[0];
    Dst^[1]:=Src^[1];
    Dst^[2]:=Src^[2];
    Dst^[3]:=Src^[3];
    Dst^[4]:=Src^[3];
    Dst^[5]:=Src^[3];
    Dst^[6]:=Src^[3];
   end else begin
    p0:=Src^[0];
    p1:=Src^[1];
    p2:=Src^[2];
    p3:=Src^[3];
    ab:=p0.Lerp(p1,t);
    bc:=p1.Lerp(p2,t);
    cd:=p2.Lerp(p3,t);
    abc:=ab.Lerp(bc,t);
    bcd:=bc.Lerp(cd,t);
    abcd:=abc.Lerp(bcd,t);
    Dst^[0]:=p0;
    Dst^[1]:=ab;
    Dst^[2]:=abc;
    Dst^[3]:=abcd;
    Dst^[4]:=bcd;
    Dst^[5]:=cd;
    Dst^[6]:=p3;
   end;
  end;
  procedure ChopCubicAt(Src,Dst:PpvVectorPathVectors;const t0,t1:TpvDouble); overload;
  var p0,p1,p2,p3,
      ab0,bc0,cd0,abc0,bcd0,abcd0,
      ab1,bc1,cd1,abc1,bcd1,abcd1,
      Middle0,Middle1:TpvVectorPathVector;
  begin
   if SameValue(t1,1.0) then begin
    ChopCubicAt(Src,Dst,t0);
    Dst^[7]:=Src^[3];
    Dst^[8]:=Src^[3];
    Dst^[9]:=Src^[3];
   end else begin
    p0:=Src^[0];
    p1:=Src^[1];
    p2:=Src^[2];
    p3:=Src^[3];
    ab0:=p0.Lerp(p1,t0);
    bc0:=p1.Lerp(p2,t0);
    cd0:=p2.Lerp(p3,t0);
    abc0:=ab0.Lerp(bc0,t0);
    bcd0:=bc0.Lerp(cd0,t0);
    abcd0:=abc0.Lerp(bcd0,t0);
    ab1:=p0.Lerp(p1,t1);
    bc1:=p1.Lerp(p2,t1);
    cd1:=p2.Lerp(p3,t1);
    abc1:=ab1.Lerp(bc1,t1);
    bcd1:=bc1.Lerp(cd1,t1);
    abcd1:=abc1.Lerp(bcd1,t1);
    Middle0:=abc0.Lerp(bcd0,t1);
    Middle1:=abc1.Lerp(bcd1,t0);
    Dst^[0]:=p0;
    Dst^[1]:=ab0;
    Dst^[2]:=abc0;
    Dst^[3]:=abcd0;
    Dst^[4]:=Middle0;
    Dst^[5]:=Middle1;
    Dst^[6]:=abcd1;
    Dst^[7]:=bcd1;
    Dst^[8]:=cd1;
    Dst^[9]:=p3;
   end;
  end;
  function ChopCubicAtInflections(const aSrc:array of TpvVectorPathVector;out aDst:TChoppedPoints):TpvSizeInt;
   function ValidUnitDivide(aNumerator,aDenominator:TpvDouble;out aRatio:TpvDouble):boolean;
   begin
    if aNumerator<0.0 then begin
     aNumerator:=-aNumerator;
     aDenominator:=-aDenominator;
    end;
    if IsZero(aNumerator) or IsZero(aDenominator) or (aNumerator>=aDenominator) then begin
     result:=false;
    end else begin
     aRatio:=aNumerator/aDenominator;
     if IsNaN(aRatio) or IsZero(aRatio) then begin
      result:=false;
     end else begin
      result:=true;
     end;
    end;
   end;
   function FindUnitQuadRoots(const A,B,C:TpvDouble;out aRoot0,aRoot1:TpvDouble):TpvSizeInt;
   var dr,Q:TpvDouble;
   begin
    if IsZero(A) then begin
     if ValidUnitDivide(-C,B,aRoot0) then begin
      result:=1;
     end else begin
      result:=0;
     end;
    end else begin
     dr:=sqr(B)-(4.0*A*C);
     if dr<0.0 then begin
      result:=0;
     end else begin
      dr:=sqrt(dr);
      if IsInfinite(dr) or IsNaN(dr) then begin
       result:=0;
      end else begin
       if B<0.0 then begin
        Q:=-(B-dr)*0.5;
       end else begin
        Q:=-(B+dr)*0.5;
       end;
       if ValidUnitDivide(Q,A,aRoot0) then begin
        if ValidUnitDivide(C,Q,aRoot1) then begin
         result:=2;
         if aRoot0>aRoot1 then begin
          Q:=aRoot0;
          aRoot0:=aRoot1;
          aRoot1:=Q;
         end else if SameValue(aRoot0,aRoot1) then begin
          dec(result);
         end;
        end else begin
         result:=1;
        end;
       end else begin
        if ValidUnitDivide(C,Q,aRoot0) then begin
         result:=1;
        end else begin
         result:=0;
        end;
       end;
      end;
     end;
    end;
   end;
  var Index,Count:TpvSizeInt;
      Times:array[0..1] of TpvDouble;
      Ax,Ay,Bx,By,Cx,Cy,t0,t1,LastTime:TpvDouble;
      Src:PpvVectorPathVector;
      Dst:PpvVectorPathVector;
  begin
   Ax:=aSrc[1].x-aSrc[0].x;
   Ay:=aSrc[1].y-aSrc[0].y;
   Bx:=aSrc[2].x-(2.0*aSrc[1].x)+aSrc[0].x;
   By:=aSrc[2].y-(2.0*aSrc[1].y)+aSrc[0].y;
   Cx:=aSrc[3].x+(3.0*(aSrc[1].x-aSrc[2].x))-aSrc[0].x;
   Cy:=aSrc[3].y+(3.0*(aSrc[1].y-aSrc[2].y))-aSrc[0].y;
   Count:=FindUnitQuadRoots((Bx*Cy)-(By*Cx),(Ax*Cy)-(Ay*Cx),(Ax*By)-(Ay*Bx),Times[0],Times[1]);
   if Count=0 then begin
    aDst[0]:=aSrc[0];
    aDst[1]:=aSrc[1];
    aDst[2]:=aSrc[2];
    aDst[3]:=aSrc[3];
   end else begin
    Src:=@aSrc[0];
    Dst:=@aDst[0];
    Index:=0;
    while Index<(Count-1) do begin
     t0:=Times[Index+0];
     t1:=Times[Index+1];
     if Index<>0 then begin
      LastTime:=Times[Index-1];
      t0:=Clamp(TpvDouble((t0-LastTime)/(1.0-LastTime)),TpvDouble(0.0),TpvDouble(1.0));
      t1:=Clamp(TpvDouble((t1-LastTime)/(1.0-LastTime)),TpvDouble(0.0),TpvDouble(1.0));
     end;
     ChopCubicAt(TpvPointer(Src),TpvPointer(Dst),t0,t1);
     inc(Src,4);
     inc(Dst,6);
     inc(Index,2);
    end;
    if Index<Count then begin
     t0:=Times[Index];
     if Index<>0 then begin
      LastTime:=Times[Index-1];
      t0:=Clamp(TpvDouble((t0-LastTime)/(1.0-LastTime)),TpvDouble(0.0),TpvDouble(1.0));
     end;
     ChopCubicAt(TpvPointer(Src),TpvPointer(Dst),t0);
    end;
   end;
   result:=Count+1;
  end;
  procedure ConvertNonInflectCubicToQuads(const aPoints:PpvVectorPathVectors;const aSquaredTolerance:TpvDouble;const aSubLevel:TpvSizeInt=0;const aPreserveFirstTangent:boolean=true;const aPreserveLastTangent:boolean=true);
  const LengthScale=ValueOne*1.5;
        MaxSubdivisions=10;
  var ab,dc,c0,c1,c:TpvVectorPathVector;
      p:array[0..7] of TpvVectorPathVector;
  begin
   ab:=aPoints^[1]-aPoints^[0];
   dc:=aPoints^[2]-aPoints^[3];
   if ab.LengthSquared<NearlyZeroValue then begin
    if dc.LengthSquared<NearlyZeroValue then begin
     OutputLine(aPoints^[0],aPoints^[3]);
     exit;
    end else begin
     ab:=aPoints^[2]-aPoints^[0];
    end;
   end;
   if dc.LengthSquared<NearlyZeroValue then begin
    dc:=aPoints^[1]-aPoints^[3];
   end;
   ab.x:=ab.x*LengthScale;
   ab.y:=ab.y*LengthScale;
   dc.x:=dc.x*LengthScale;
   dc.y:=dc.y*LengthScale;
   c0:=aPoints^[0]+ab;
   c1:=aPoints^[3]+dc;
   if (aSubLevel>MaxSubdivisions) or ((c0-c1).LengthSquared<aSquaredTolerance) then begin
    if aPreserveFirstTangent=aPreserveLastTangent then begin
     c:=c0.Lerp(c1,0.5);
    end else if aPreserveFirstTangent then begin
     c:=c0;
    end else begin
     c:=c1;
    end;
    OutputQuad(aPoints^[0],c,aPoints^[3]);
   end else begin
    ChopCubicAt(aPoints,TpvPointer(@p[0]),0.5);
    ConvertNonInflectCubicToQuads(TpvPointer(@p[0]),aSquaredTolerance,aSubLevel+1,aPreserveFirstTangent,false);
    ConvertNonInflectCubicToQuads(TpvPointer(@p[3]),aSquaredTolerance,aSubLevel+1,false,aPreserveLastTangent);
   end;
  end;
 var Count,Index:TpvSizeInt;
     Points:array[0..3] of TpvVectorPathVector;
 begin
  Points[0]:=TpvVectorPathVector.Create(LastX,LastY);
  Points[1]:=TpvVectorPathVector.Create(aX0,aY0);
  Points[2]:=TpvVectorPathVector.Create(aX1,aY1);
  Points[3]:=TpvVectorPathVector.Create(aX2,aY2);
  if not ((IsNaN(Points[0].x) or IsInfinite(Points[0].x)) or
          (IsNaN(Points[0].y) or IsInfinite(Points[0].y)) or
          (IsNaN(Points[1].x) or IsInfinite(Points[1].x)) or
          (IsNaN(Points[1].y) or IsInfinite(Points[1].y)) or
          (IsNaN(Points[2].x) or IsInfinite(Points[2].x)) or
          (IsNaN(Points[2].y) or IsInfinite(Points[2].y)) or
          (IsNaN(Points[3].x) or IsInfinite(Points[3].x)) or
          (IsNaN(Points[3].y) or IsInfinite(Points[3].y))) then begin
   Count:=ChopCubicAtInflections(Points,ChoppedPoints);
   if Count>0 then begin
    for Index:=0 to Count-1 do begin
     ConvertNonInflectCubicToQuads(TpvPointer(@ChoppedPoints[Index*3]),ValueOne,0,true,true);
    end;
   end;
  end;
  LastX:=aX2;
  LastY:=aY2;
 end;
begin
 StartX:=0.0;
 StartY:=0.0;
 LastX:=0.0;
 LastY:=0.0;
 OldCommands:=TpvVectorPathCommandList.Create;
 try
  fCommands:=TpvPointer(TPasMPInterlocked.Exchange(TpvPointer(OldCommands),TpvPointer(fCommands)));
  for Index:=0 to OldCommands.Count-1 do begin
   OldCommand:=OldCommands[Index];
   case OldCommand.fCommandType of
    TpvVectorPathCommandType.MoveTo:begin
     fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.MoveTo,OldCommand.fX0,OldCommand.fY0));
     StartX:=OldCommand.x0;
     StartY:=OldCommand.y0;
     LastX:=OldCommand.x0;
     LastY:=OldCommand.y0;
    end;
    TpvVectorPathCommandType.LineTo:begin
     fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.LineTo,OldCommand.fX0,OldCommand.fY0));
     LastX:=OldCommand.x0;
     LastY:=OldCommand.y0;
    end;
    TpvVectorPathCommandType.QuadraticCurveTo:begin
     fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.QuadraticCurveTo,OldCommand.fX0,OldCommand.fY0,OldCommand.fX1,OldCommand.fY1));
     LastX:=OldCommand.x1;
     LastY:=OldCommand.y1;
    end;
    TpvVectorPathCommandType.CubicCurveTo:begin
     ConvertCubicCurveToQuadraticCurve(OldCommand.fX0,OldCommand.fY0,OldCommand.fX1,OldCommand.fY1,OldCommand.fX2,OldCommand.fY2);
     LastX:=OldCommand.x2;
     LastY:=OldCommand.y2;
    end;
    TpvVectorPathCommandType.Close:begin
     fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.Close));
     LastX:=StartX;
     LastY:=StartY;
    end;
   end;
  end;
 finally
  FreeAndNil(OldCommands);
 end;
end;

function TpvVectorPath.GetSignedDistance(const aX,aY,aScale:TpvDouble;out aInsideOutsideSign:TpvInt32):TpvDouble;
const CurveTessellationTolerance=0.25;
      CurveTessellationToleranceSquared=CurveTessellationTolerance*CurveTessellationTolerance;
      CurveRecursionLimit=16;
var Index:TpvInt32;
    Command:TpvVectorPathCommand;
    ResultDistance,StartX,StartY,LastX,LastY:TpvDouble;
 procedure LineDistance(const aPX,aPY,aAX,aAY,aBX,aBY:TpvDouble);
 var pax,pay,bax,bay,t:TpvDouble;
 begin
  pax:=aPX-aAX;
  pay:=aPY-aAY;
  bax:=aBX-aAX;
  bay:=aBY-aAY;
  if ((aAY>aPY)<>(aBY>aPY)) and (pax<(bax*(pay/bay))) then begin
   aInsideOutsideSign:=-aInsideOutsideSign;
  end;
  t:=sqr(bax)+sqr(bay);
  if t>0.0 then begin
   t:=Min(Max(((pax*bax)+(pay*bay))/t,0.0),1.0);
  end else begin
   t:=0.0;
  end;
  ResultDistance:=Min(ResultDistance,sqr(pax-(bax*t))+sqr(pay-(bay*t)));
 end;
 procedure DoLineTo(const aToX,aToY:TpvDouble);
 begin
  LineDistance(aX,aY,LastX,LastY,aToX,aToY);
  LastX:=aToX;
  LastY:=aToY;
 end;
 procedure DoQuadraticCurveTo(const aC0X,aC0Y,aA0X,aA0Y:TpvDouble);
  procedure Recursive(const x1,y1,x2,y2,x3,y3:TpvDouble;const Level:TpvInt32);
  var x12,y12,x23,y23,x123,y123,dx,dy:TpvDouble;
  begin
   x12:=(x1+x2)*0.5;
   y12:=(y1+y2)*0.5;
   x23:=(x2+x3)*0.5;
   y23:=(y2+y3)*0.5;
   x123:=(x12+x23)*0.5;
   y123:=(y12+y23)*0.5;
   dx:=x3-x1;
   dy:=y3-y1;
   if (Level>CurveRecursionLimit) or
      ((Level>0) and
       (sqr(((x2-x3)*dy)-((y2-y3)*dx))<((sqr(dx)+sqr(dy))*CurveTessellationToleranceSquared))) then begin
    LineTo(x3,y3);
   end else begin
    Recursive(x1,y1,x12,y12,x123,y123,level+1);
    Recursive(x123,y123,x23,y23,x3,y3,level+1);
   end;
  end;
 begin
  Recursive(LastX,LastY,aC0X,aC0Y,aA0X,aA0Y,0);
  DoLineTo(aA0X,aA0Y);
 end;
 procedure DoCubicCurveTo(const aC0X,aC0Y,aC1X,aC1Y,aA0X,aA0Y:TpvDouble);
  procedure Recursive(const x1,y1,x2,y2,x3,y3,x4,y4:TpvDouble;const Level:TpvInt32);
  var x12,y12,x23,y23,x34,y34,x123,y123,x234,y234,x1234,y1234,dx,dy:TpvDouble;
  begin
   x12:=(x1+x2)*0.5;
   y12:=(y1+y2)*0.5;
   x23:=(x2+x3)*0.5;
   y23:=(y2+y3)*0.5;
   x34:=(x3+x4)*0.5;
   y34:=(y3+y4)*0.5;
   x123:=(x12+x23)*0.5;
   y123:=(y12+y23)*0.5;
   x234:=(x23+x34)*0.5;
   y234:=(y23+y34)*0.5;
   x1234:=(x123+x234)*0.5;
   y1234:=(y123+y234)*0.5;
   dx:=x4-x1;
   dy:=y4-y1;
   if (Level>CurveRecursionLimit) or
      ((Level>0) and
       (sqr(abs(((x2-x4)*dy)-((y2-y4)*dx))+
            abs(((x3-x4)*dy)-((y3-y4)*dx)))<((sqr(dx)+sqr(dy))*CurveTessellationToleranceSquared))) then begin
    DoLineTo(x4,y4);
   end else begin
    Recursive(x1,y1,x12,y12,x123,y123,x1234,y1234,Level+1);
    Recursive(x1234,y1234,x234,y234,x34,y34,x4,y4,Level+1);
   end;
  end;
 begin
  Recursive(LastX,LastY,aC0X,aC0Y,aC1X,aC1Y,aA0X,aA0Y,0);
  DoLineTo(aA0X,aA0Y);
 end;
begin
 ResultDistance:=Infinity;
 aInsideOutsideSign:=1;
 StartX:=0.0;
 StartY:=0.0;
 LastX:=0.0;
 LastY:=0.0;
 for Index:=0 to fCommands.Count-1 do begin
  Command:=fCommands.Items[Index];
  case Command.fCommandType of
   TpvVectorPathCommandType.MoveTo:begin
    StartX:=Command.x0*aScale;
    StartY:=Command.y0*aScale;
    LastX:=Command.x0*aScale;
    LastY:=Command.y0*aScale;
   end;
   TpvVectorPathCommandType.LineTo:begin
    DoLineTo(Command.x0*aScale,Command.y0*aScale);
   end;
   TpvVectorPathCommandType.QuadraticCurveTo:begin
    DoQuadraticCurveTo(Command.x0*aScale,Command.y0*aScale,
                       Command.x1*aScale,Command.y1*aScale);
   end;
   TpvVectorPathCommandType.CubicCurveTo:begin
    DoCubicCurveTo(Command.x0*aScale,Command.y0*aScale,
                   Command.x1*aScale,Command.y1*aScale,
                   Command.x2*aScale,Command.y2*aScale);
   end;
   TpvVectorPathCommandType.Close:begin
    DoLineTo(StartX,StartY);
   end;
  end;
 end;
 result:=sqrt(ResultDistance);
end;

end.
