(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2026, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.VectorGraphics.Paints;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

{$define CANINLINE}

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.VectorPath,
     PasVulkan.VectorGraphics.Rasterizer;

type { TpvVectorGraphicsGradientStop }

     TpvVectorGraphicsGradientStop=record
      public
       Offset:TpvFloat;
       Color:TpvVectorGraphicsColor;
      public
       constructor Create(const aOffset:TpvFloat;const aColor:TpvVectorGraphicsColor);
     end;

     PpvVectorGraphicsGradientStop=^TpvVectorGraphicsGradientStop;

     TpvVectorGraphicsGradientStops=array of TpvVectorGraphicsGradientStop;

     // What happens outside the zero to one range of a gradient or outside an image or pattern
     // tile. The names match the spreadMethod values of SVG, with Repeat_ spelled with a
     // trailing underscore because repeat is a Pascal keyword.
     PpvVectorGraphicsSpreadMethod=^TpvVectorGraphicsSpreadMethod;
     TpvVectorGraphicsSpreadMethod=
      (
       Pad=0,
       Reflect,
       Repeat_
      );

     // 256 entry ramp the gradients sample from, built once from the stops.
     TpvVectorGraphicsGradientBuffer=array[0..255] of TpvVectorGraphicsColor;

     PpvVectorGraphicsGradientBuffer=^TpvVectorGraphicsGradientBuffer;

     { TpvVectorGraphicsSolidPaint }

     TpvVectorGraphicsSolidPaint=class(TpvVectorGraphicsPaint)
      public
       constructor Create(const aColor:TpvVectorGraphicsColor=$ff000000); override;
     end;

     { TpvVectorGraphicsGradientPaint }

     // Shared base of the two gradient paints. Holds the ramp, the spread method, the matrix
     // that takes a device pixel into the gradient's own coordinate system, and the opacity that
     // the enclosing object contributes.
     TpvVectorGraphicsGradientPaint=class(TpvVectorGraphicsPaint)
      protected
       fStops:TpvVectorGraphicsGradientStops;
       fBuffer:TpvVectorGraphicsGradientBuffer;
       fSpreadMethod:TpvVectorGraphicsSpreadMethod;
       fMatrix:TpvVectorPathMatrix;
       fRadius:TpvDouble;
       fOpacity:TpvUInt8;
       fDirty:boolean;
       procedure SetStops(const aStops:TpvVectorGraphicsGradientStops);
       function ApplySpreadMethod(const aValue:TpvInt32):TpvInt32; {$ifdef CANINLINE}inline;{$endif}
       function GetBuffer:PpvVectorGraphicsGradientBuffer;
      public
       constructor Create(const aColor:TpvVectorGraphicsColor=$ff000000); override;
       destructor Destroy; override;
       // Sorts the stops by offset and bakes the 256 entry ramp. Called automatically before the
       // first sample after the stops changed.
       procedure Update;
       function SampleStops(const aOffset:TpvFloat):TpvVectorGraphicsColor;
       function IsSolid:boolean; override;
      public
       property Stops:TpvVectorGraphicsGradientStops read fStops write SetStops;
       property Matrix:TpvVectorPathMatrix read fMatrix write fMatrix;
       // The baked 256 entry ramp, brought up to date first, which a backend that samples the
       // gradient somewhere else needs in order to arrive at the same colors.
       property Buffer:PpvVectorGraphicsGradientBuffer read GetBuffer;
      published
       property SpreadMethod:TpvVectorGraphicsSpreadMethod read fSpreadMethod write fSpreadMethod;
       property Radius:TpvDouble read fRadius write fRadius;
       property Opacity:TpvUInt8 read fOpacity write fOpacity;
     end;

     { TpvVectorGraphicsLinearGradientPaint }

     // The ramp runs along the local x axis from zero to Radius, so the matrix is what turns an
     // arbitrary gradient vector into that canonical form.
     TpvVectorGraphicsLinearGradientPaint=class(TpvVectorGraphicsGradientPaint)
      public
       function GetColorAt(const aX,aY:TpvInt32):TpvVectorGraphicsColor; override;
     end;

     { TpvVectorGraphicsRadialGradientPaint }

     // The ramp runs from the focus point outwards to the circle of Radius around the local
     // origin. A focus away from the origin gives the off-center highlight SVG allows.
     TpvVectorGraphicsRadialGradientPaint=class(TpvVectorGraphicsGradientPaint)
      private
       fFocusX:TpvDouble;
       fFocusY:TpvDouble;
       fFocusMatrix:TpvVectorPathMatrix;
      public
       constructor Create(const aColor:TpvVectorGraphicsColor=$ff000000); override;
       function GetColorAt(const aX,aY:TpvInt32):TpvVectorGraphicsColor; override;
      public
       property FocusMatrix:TpvVectorPathMatrix read fFocusMatrix write fFocusMatrix;
      published
       property FocusX:TpvDouble read fFocusX write fFocusX;
       property FocusY:TpvDouble read fFocusY write fFocusY;
     end;

     { TpvVectorGraphicsImagePaint }

     // Samples a plain $aarrggbb image with bilinear filtering. Serves both the image element
     // and, with the pattern buffer plugged in, the rendered pattern tiles.
     TpvVectorGraphicsImagePaint=class(TpvVectorGraphicsPaint)
      private
       fPixels:PpvVectorGraphicsRawColorArray;
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fStride:TpvInt32;
       fMatrix:TpvVectorPathMatrix;
       fSpreadMethod:TpvVectorGraphicsSpreadMethod;
       fOpacity:TpvUInt8;
       fBilinear:boolean;
      public
       constructor Create(const aColor:TpvVectorGraphicsColor=$ff000000); override;
       procedure SetImage(const aPixels:pointer;const aWidth,aHeight:TpvInt32;const aStride:TpvInt32=0);
       function GetColorAt(const aX,aY:TpvInt32):TpvVectorGraphicsColor; override;
       function IsSolid:boolean; override;
      public
       property Matrix:TpvVectorPathMatrix read fMatrix write fMatrix;
      published
       property Width:TpvInt32 read fWidth;
       property Height:TpvInt32 read fHeight;
       property SpreadMethod:TpvVectorGraphicsSpreadMethod read fSpreadMethod write fSpreadMethod;
       property Opacity:TpvUInt8 read fOpacity write fOpacity;
       property Bilinear:boolean read fBilinear write fBilinear;
     end;

implementation

{ TpvVectorGraphicsGradientStop }

constructor TpvVectorGraphicsGradientStop.Create(const aOffset:TpvFloat;const aColor:TpvVectorGraphicsColor);
begin
 Offset:=aOffset;
 Color:=aColor;
end;

{ TpvVectorGraphicsSolidPaint }

constructor TpvVectorGraphicsSolidPaint.Create(const aColor:TpvVectorGraphicsColor);
begin
 inherited Create(aColor);
end;

{ TpvVectorGraphicsGradientPaint }

constructor TpvVectorGraphicsGradientPaint.Create(const aColor:TpvVectorGraphicsColor);
var Index:TpvSizeInt;
begin

 inherited Create(aColor);

 fStops:=nil;
 fSpreadMethod:=TpvVectorGraphicsSpreadMethod.Pad;
 fMatrix:=TpvVectorPathMatrix.CreateIdentity;
 fRadius:=1.0;
 fOpacity:=255;
 fDirty:=true;

 // Until stops arrive the ramp is flat, so that sampling it never yields nonsense.
 for Index:=Low(TpvVectorGraphicsGradientBuffer) to High(TpvVectorGraphicsGradientBuffer) do begin
  fBuffer[Index]:=aColor;
 end;

end;

destructor TpvVectorGraphicsGradientPaint.Destroy;
begin
 fStops:=nil;
 inherited Destroy;
end;

procedure TpvVectorGraphicsGradientPaint.SetStops(const aStops:TpvVectorGraphicsGradientStops);
begin
 fStops:=copy(aStops);
 fDirty:=true;
end;

function TpvVectorGraphicsGradientPaint.IsSolid:boolean;
begin
 result:=false;
end;

function TpvVectorGraphicsGradientPaint.GetBuffer:PpvVectorGraphicsGradientBuffer;
begin
 if fDirty then begin
  Update;
 end;
 result:=@fBuffer;
end;

function TpvVectorGraphicsGradientPaint.SampleStops(const aOffset:TpvFloat):TpvVectorGraphicsColor;
var Index:TpvSizeInt;
    Left,Right:TpvVectorGraphicsRGBA;
    Mixed:TpvVectorGraphicsRGBA;
    t,ti:TpvFloat;
    Value:TpvInt32;
begin

 // Outside the range the end stops simply carry on.
 if length(fStops)=0 then begin
  result:=0;
  exit;
 end else if aOffset<=fStops[0].Offset then begin
  result:=fStops[0].Color;
  exit;
 end else if aOffset>=fStops[length(fStops)-1].Offset then begin
  result:=fStops[length(fStops)-1].Color;
  exit;
 end;

 result:=fStops[0].Color;

 // Find the pair the offset falls between and mix them.
 for Index:=1 to length(fStops)-1 do begin

  if fStops[Index].Offset<aOffset then begin
   continue;
  end;

  Left:=TpvVectorGraphicsRGBA.CreateFromColor(fStops[Index-1].Color);
  Right:=TpvVectorGraphicsRGBA.CreateFromColor(fStops[Index].Color);

  // Two stops at the very same offset make a hard color step, with the later one winning.
  if SameValue(fStops[Index].Offset,fStops[Index-1].Offset) then begin
   Mixed:=Right;
  end else begin
   t:=(aOffset-fStops[Index-1].Offset)/(fStops[Index].Offset-fStops[Index-1].Offset);
   ti:=1.0-t;
   // Rounded rather than truncated, because two stops that agree on a channel would otherwise
   // come out one short of it in between them: the two weights add up to one only to within the
   // precision of a float, so a value of 255 lands just below it and truncating takes it to 254.
   Value:=round((Left.a*ti)+(Right.a*t));
   Mixed.a:=Min(Max(Value,0),255);
   Value:=round((Left.r*ti)+(Right.r*t));
   Mixed.r:=Min(Max(Value,0),255);
   Value:=round((Left.g*ti)+(Right.g*t));
   Mixed.g:=Min(Max(Value,0),255);
   Value:=round((Left.b*ti)+(Right.b*t));
   Mixed.b:=Min(Max(Value,0),255);
  end;

  result:=Mixed.ToColor;
  exit;

 end;

end;

procedure TpvVectorGraphicsGradientPaint.Update;
var Index:TpvSizeInt;
    Stop:TpvVectorGraphicsGradientStop;
begin

 if not fDirty then begin
  exit;
 end;
 fDirty:=false;

 // Gather sort, which is what the stop lists of a few entries that SVG produces deserve, and
 // which keeps equal offsets in their original order so that hard color steps stay hard.
 Index:=0;
 while Index<(length(fStops)-1) do begin
  if fStops[Index].Offset>fStops[Index+1].Offset then begin
   Stop:=fStops[Index];
   fStops[Index]:=fStops[Index+1];
   fStops[Index+1]:=Stop;
   if Index>0 then begin
    dec(Index);
   end else begin
    inc(Index);
   end;
  end else begin
   inc(Index);
  end;
 end;

 // Bake the ramp once, so that sampling it later is a plain array lookup.
 for Index:=Low(TpvVectorGraphicsGradientBuffer) to High(TpvVectorGraphicsGradientBuffer) do begin
  fBuffer[Index]:=SampleStops((Index-Low(TpvVectorGraphicsGradientBuffer))/
                              (High(TpvVectorGraphicsGradientBuffer)-Low(TpvVectorGraphicsGradientBuffer)));
 end;

end;

function TpvVectorGraphicsGradientPaint.ApplySpreadMethod(const aValue:TpvInt32):TpvInt32;
begin

 case fSpreadMethod of
  TpvVectorGraphicsSpreadMethod.Reflect:begin
   // Fold back and forth, so every other repetition runs the ramp backwards.
   result:=aValue and $1ff;
   if result>=$100 then begin
    result:=255-(result-256);
   end;
  end;
  TpvVectorGraphicsSpreadMethod.Repeat_:begin
   result:=aValue and $ff;
  end;
  else {TpvVectorGraphicsSpreadMethod.Pad:}begin
   result:=aValue;
  end;
 end;

 // Pad needs the clamp, and the other two are already in range.
 if result<0 then begin
  result:=0;
 end else if result>255 then begin
  result:=255;
 end;

end;

{ TpvVectorGraphicsLinearGradientPaint }

function TpvVectorGraphicsLinearGradientPaint.GetColorAt(const aX,aY:TpvInt32):TpvVectorGraphicsColor;
const OneOver256=1.0/256.0;
var Position:TpvVectorPathVector;
    CurrentRadius:TpvDouble;
    Index:TpvInt32;
begin

 if fDirty then begin
  Update;
 end;

 // Into the gradient's own space, where the ramp runs along x from zero to the radius.
 Position:=fMatrix.Transform(aX*OneOver256,aY*OneOver256);
 CurrentRadius:=fRadius;
 if SameValue(CurrentRadius,0.0) then begin
  CurrentRadius:=1.0;
 end;

 Index:=trunc((Position.x*(256.0/CurrentRadius))+0.5);
 result:=fBuffer[ApplySpreadMethod(Index)];

 if fOpacity<>255 then begin
  result:=pvVectorGraphicsModulateColor(result,fOpacity);
 end;

end;

{ TpvVectorGraphicsRadialGradientPaint }

constructor TpvVectorGraphicsRadialGradientPaint.Create(const aColor:TpvVectorGraphicsColor);
begin
 inherited Create(aColor);
 fFocusX:=0.0;
 fFocusY:=0.0;
 fFocusMatrix:=TpvVectorPathMatrix.CreateIdentity;
end;

function TpvVectorGraphicsRadialGradientPaint.GetColorAt(const aX,aY:TpvInt32):TpvVectorGraphicsColor;
const OneOver256=1.0/256.0;
var Position,Focus:TpvVectorPathVector;
    CurrentRadius,Denominator,Factor,DeltaX,DeltaY,Cross,Discriminant:TpvDouble;
    Index:TpvInt32;
begin

 if fDirty then begin
  Update;
 end;

 Position:=fMatrix.Transform(aX*OneOver256,aY*OneOver256);
 Focus:=fFocusMatrix.Transform(fFocusX,fFocusY);

 CurrentRadius:=fRadius;
 if SameValue(CurrentRadius,0.0) then begin
  CurrentRadius:=1.0;
 end;

 if SameValue(Focus.x,0.0) and SameValue(Focus.y,0.0) then begin

  // Centered focus, so plain distance from the origin.
  Index:=trunc(((sqrt(sqr(Position.x)+sqr(Position.y))*(256.0/CurrentRadius)))+0.5);

 end else begin

  // An off-center focus makes the ramp run along the ray from the focus through the point out
  // to the circle, which is the highlight effect SVG describes.
  Denominator:=sqr(CurrentRadius)-(sqr(Focus.x)+sqr(Focus.y));

  // A focus exactly on the circle makes that denominator zero, so it gets nudged inwards.
  if Denominator=0.0 then begin
   if Focus.x<>0.0 then begin
    if Focus.x<0.0 then begin
     Focus.x:=Focus.x+1.0;
    end else begin
     Focus.x:=Focus.x-1.0;
    end;
   end;
   if Focus.y<>0.0 then begin
    if Focus.y<0.0 then begin
     Focus.y:=Focus.y+1.0;
    end else begin
     Focus.y:=Focus.y-1.0;
    end;
   end;
   Denominator:=sqr(CurrentRadius)-(sqr(Focus.x)+sqr(Focus.y));
  end;

  Factor:=CurrentRadius/Denominator;
  DeltaX:=Position.x-Focus.x;
  DeltaY:=Position.y-Focus.y;
  Cross:=(DeltaX*Focus.y)-(DeltaY*Focus.x);
  Discriminant:=(sqr(CurrentRadius)*(sqr(DeltaX)+sqr(DeltaY)))-sqr(Cross);
  Index:=trunc((((((DeltaX*Focus.x)+(DeltaY*Focus.y))+sqrt(abs(Discriminant)))*Factor)*(256.0/CurrentRadius))+0.5);

 end;

 result:=fBuffer[ApplySpreadMethod(Index)];

 if fOpacity<>255 then begin
  result:=pvVectorGraphicsModulateColor(result,fOpacity);
 end;

end;

{ TpvVectorGraphicsImagePaint }

constructor TpvVectorGraphicsImagePaint.Create(const aColor:TpvVectorGraphicsColor);
begin

 inherited Create(aColor);

 fPixels:=nil;
 fWidth:=0;
 fHeight:=0;
 fStride:=0;

 fMatrix:=TpvVectorPathMatrix.CreateIdentity;
 fSpreadMethod:=TpvVectorGraphicsSpreadMethod.Pad;
 fOpacity:=255;
 fBilinear:=true;

end;

procedure TpvVectorGraphicsImagePaint.SetImage(const aPixels:pointer;const aWidth,aHeight:TpvInt32;const aStride:TpvInt32);
begin

 fPixels:=aPixels;
 fWidth:=aWidth;
 fHeight:=aHeight;

 // A stride of zero means the rows follow each other without padding.
 if aStride>0 then begin
  fStride:=aStride;
 end else begin
  fStride:=aWidth;
 end;

end;

function TpvVectorGraphicsImagePaint.IsSolid:boolean;
begin
 result:=false;
end;

function TpvVectorGraphicsImagePaint.GetColorAt(const aX,aY:TpvInt32):TpvVectorGraphicsColor;
const OneOver256=1.0/256.0;
var Position:TpvVectorPathVector;
    FixedX,FixedY,FullWidth,FullHeight,PixelX,PixelY,FractionX,FractionY,Shift:TpvInt32;
    Samples:array[0..3] of TpvVectorGraphicsColor;
    RowColors:array[0..1] of TpvVectorGraphicsColor;
 function SampleAt(const aPixelX,aPixelY:TpvInt32):TpvVectorGraphicsColor; {$ifdef CANINLINE}inline;{$endif}
 begin
  if (aPixelX>=0) and (aPixelX<fWidth) and (aPixelY>=0) and (aPixelY<fHeight) then begin
   result:=fPixels^[(TpvSizeInt(aPixelY)*TpvSizeInt(fStride))+TpvSizeInt(aPixelX)];
  end else begin
   result:=0;
  end;
 end;
 function LerpColors(const aLeft,aRight:TpvVectorGraphicsColor;const aFactor:TpvInt32):TpvVectorGraphicsColor; {$ifdef CANINLINE}inline;{$endif}
 var Channel,LeftValue,RightValue:TpvInt32;
 begin
  result:=0;
  for Channel:=0 to 3 do begin
   Shift:=Channel*8;
   LeftValue:=(aLeft shr Shift) and $ff;
   RightValue:=(aRight shr Shift) and $ff;
   result:=result or (TpvVectorGraphicsColor(LeftValue+(((RightValue-LeftValue)*aFactor) div 256)) shl Shift);
  end;
 end;
begin

 if not (assigned(fPixels) and (fWidth>0) and (fHeight>0)) then begin
  result:=0;
  exit;
 end;

 // Into image space, kept in 8.8 fixed point so that the fractional part doubles as the
 // bilinear weight.
 Position:=fMatrix.Transform(aX*OneOver256,aY*OneOver256);
 FixedX:=trunc((Position.x*256.0)+0.5);
 FixedY:=trunc((Position.y*256.0)+0.5);
 FullWidth:=fWidth*256;
 FullHeight:=fHeight*256;

 // Tiling, if any, happens before the sample position is split into pixel and fraction.
 case fSpreadMethod of
  TpvVectorGraphicsSpreadMethod.Reflect:begin
   FixedX:=abs(FixedX) mod (FullWidth*2);
   FixedY:=abs(FixedY) mod (FullHeight*2);
   if FixedX>=FullWidth then begin
    FixedX:=FullWidth-(FixedX-FullWidth);
   end;
   if FixedY>=FullHeight then begin
    FixedY:=FullHeight-(FixedY-FullHeight);
   end;
  end;
  TpvVectorGraphicsSpreadMethod.Repeat_:begin
   FixedX:=FixedX mod FullWidth;
   if FixedX<0 then begin
    inc(FixedX,FullWidth);
   end;
   FixedY:=FixedY mod FullHeight;
   if FixedY<0 then begin
    inc(FixedY,FullHeight);
   end;
  end;
  else {TpvVectorGraphicsSpreadMethod.Pad:}begin
  end;
 end;
 FractionX:=FixedX and $ff;
 FractionY:=FixedY and $ff;
 PixelX:=SARLongint(FixedX,8);
 PixelY:=SARLongint(FixedY,8);

 // Under the pad method a sample may still land outside, where the image paints nothing.
 if (PixelX<0) or (PixelX>=fWidth) or (PixelY<0) or (PixelY>=fHeight) then begin
  result:=0;
  exit;
 end;

 if fBilinear and ((FractionX<>0) or (FractionY<>0)) then begin

  // Four neighbours, with the edge ones standing in for themselves at the border.
  Samples[0]:=SampleAt(PixelX,PixelY);
  if (PixelX+1)<fWidth then begin
   Samples[1]:=SampleAt(PixelX+1,PixelY);
  end else begin
   Samples[1]:=Samples[0];
  end;
  if (PixelY+1)<fHeight then begin
   Samples[2]:=SampleAt(PixelX,PixelY+1);
  end else begin
   Samples[2]:=Samples[0];
  end;
  if ((PixelX+1)<fWidth) and ((PixelY+1)<fHeight) then begin
   Samples[3]:=SampleAt(PixelX+1,PixelY+1);
  end else begin
   Samples[3]:=Samples[0];
  end;
  RowColors[0]:=LerpColors(Samples[0],Samples[1],FractionX);
  RowColors[1]:=LerpColors(Samples[2],Samples[3],FractionX);
  result:=LerpColors(RowColors[0],RowColors[1],FractionY);

 end else begin

  result:=SampleAt(PixelX,PixelY);

 end;

 if fOpacity<>255 then begin
  result:=pvVectorGraphicsModulateColor(result,fOpacity);
 end;

end;

end.
