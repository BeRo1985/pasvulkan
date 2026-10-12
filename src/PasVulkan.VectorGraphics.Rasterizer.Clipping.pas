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
unit PasVulkan.VectorGraphics.Rasterizer.Clipping;
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
     PasVulkan.Collections,
     PasVulkan.VectorPath,
     PasVulkan.VectorPath.BooleanOperations,
     PasVulkan.VectorGraphics.Rasterizer;

type { TpvVectorGraphicsRasterizerClipping }

     // A rasterizer that intersects whatever it is asked to draw with a set of clip paths and
     // hands the remainder to another rasterizer.
     //
     // The coverage mask of the scene graph is the default way of clipping here and gives soft,
     // antialiased edges. This backend is the other way round: it cuts the geometry itself, so
     // the clip boundary ends up as real polygon edges. That is what the old renderer did
     // through its polygon clipper, which makes this the path to take when output is to be
     // compared against it, and it is also what the GPU side wants, since a clipped shape stays
     // a shape instead of turning into a mask texture.
     TpvVectorGraphicsRasterizerClipping=class(TpvVectorGraphicsRasterizer)
      private
       fTarget:TpvVectorGraphicsRasterizer;
       fClipShape:TpvVectorPathShape;
       fOwnsClipShape:boolean;

       fCurrentShape:TpvVectorPathShape;
       fCurrentContour:TpvVectorPathContour;
       fCurrentPosition:TpvVectorPathVector;
       fStartPosition:TpvVectorPathVector;
       fHasCurrentPosition:boolean;

       function ToVector(const aX,aY:TpvInt32):TpvVectorPathVector; {$ifdef CANINLINE}inline;{$endif}
       procedure SetTarget(const aTarget:TpvVectorGraphicsRasterizer);
      public
       constructor Create(const aPixelBuffer:TpvVectorGraphicsPixelBuffer=nil); override;
       destructor Destroy; override;

       procedure Reset; override;
       procedure MoveTo(const aX,aY:TpvInt32); override;
       procedure LineTo(const aX,aY:TpvInt32); override;
       procedure Close; override;
       procedure Render; override;

       // Sets the region to clip to. The shape must be made of line segments only; it is copied,
       // so the caller keeps ownership of what it passes in.
       procedure SetClipShape(const aShape:TpvVectorPathShape;
                              const aFillRule:TpvVectorPathFillRule=TpvVectorPathFillRule.NonZero);

       // Narrows the current clip region by another shape, which is what a nested clip path does.
       procedure IntersectClipShape(const aShape:TpvVectorPathShape;
                                    const aFillRule:TpvVectorPathFillRule=TpvVectorPathFillRule.NonZero);

       procedure ClearClipShape;

       // Takes over the paint, color, gamma and mask of another rasterizer, so that the two draw
       // alike.
       procedure AssignStateFrom(const aFrom:TpvVectorGraphicsRasterizer);
      public
       property ClipShape:TpvVectorPathShape read fClipShape;
      published

       // Where the clipped geometry goes. Without one, this rasterizer draws nothing.
       property Target:TpvVectorGraphicsRasterizer read fTarget write SetTarget;
     end;

implementation

{ TpvVectorGraphicsRasterizerClipping }

constructor TpvVectorGraphicsRasterizerClipping.Create(const aPixelBuffer:TpvVectorGraphicsPixelBuffer);
begin

 inherited Create(aPixelBuffer);

 fTarget:=nil;
 fClipShape:=nil;
 fOwnsClipShape:=false;

 fCurrentShape:=nil;
 fCurrentContour:=nil;
 fHasCurrentPosition:=false;

end;

destructor TpvVectorGraphicsRasterizerClipping.Destroy;
begin

 FreeAndNil(fCurrentShape);
 ClearClipShape;

 inherited Destroy;

end;

function TpvVectorGraphicsRasterizerClipping.ToVector(const aX,aY:TpvInt32):TpvVectorPathVector;
const OneOverPixelFactor=1.0/pvVectorGraphicsPixelFactor;
begin
 result.x:=aX*OneOverPixelFactor;
 result.y:=aY*OneOverPixelFactor;
end;

procedure TpvVectorGraphicsRasterizerClipping.SetTarget(const aTarget:TpvVectorGraphicsRasterizer);
begin

 fTarget:=aTarget;

 // The clipped geometry is handed on in device coordinates, so both have to agree on the size.
 if assigned(fTarget) then begin
  Resize(fTarget.Width,fTarget.Height);
 end;

end;

procedure TpvVectorGraphicsRasterizerClipping.ClearClipShape;
begin

 if fOwnsClipShape then begin
  FreeAndNil(fClipShape);
 end else begin
  fClipShape:=nil;
 end;
 fOwnsClipShape:=false;

end;

procedure TpvVectorGraphicsRasterizerClipping.SetClipShape(const aShape:TpvVectorPathShape;
                                                           const aFillRule:TpvVectorPathFillRule);
begin

 ClearClipShape;

 if not assigned(aShape) then begin
  exit;
 end;

 // Normalizing once up front means the clip region arrives in non-zero form, so that every
 // later intersection can assume it and an even-odd clip path still behaves.
 fClipShape:=pvVectorPathNormalize(aShape,aFillRule);
 fOwnsClipShape:=true;

end;

procedure TpvVectorGraphicsRasterizerClipping.IntersectClipShape(const aShape:TpvVectorPathShape;
                                                                 const aFillRule:TpvVectorPathFillRule);
var Narrowed:TpvVectorPathShape;
begin

 if not assigned(aShape) then begin
  exit;
 end;

 if not assigned(fClipShape) then begin
  SetClipShape(aShape,aFillRule);
  exit;
 end;

 Narrowed:=pvVectorPathIntersection(fClipShape,aShape,TpvVectorPathFillRule.NonZero,aFillRule);
 ClearClipShape;
 fClipShape:=Narrowed;
 fOwnsClipShape:=true;

end;

procedure TpvVectorGraphicsRasterizerClipping.AssignStateFrom(const aFrom:TpvVectorGraphicsRasterizer);
begin

 if not assigned(aFrom) then begin
  exit;
 end;

 SetPaint(aFrom.Paint);
 Gamma:=aFrom.Gamma;
 FillRule:=aFrom.FillRule;
 Mask:=aFrom.Mask;
 FlipY:=aFrom.FlipY;

end;

procedure TpvVectorGraphicsRasterizerClipping.Reset;
begin

 inherited Reset;

 FreeAndNil(fCurrentShape);
 fCurrentContour:=nil;
 fHasCurrentPosition:=false;

end;

procedure TpvVectorGraphicsRasterizerClipping.MoveTo(const aX,aY:TpvInt32);
begin

 Close;

 if not assigned(fCurrentShape) then begin
  fCurrentShape:=TpvVectorPathShape.Create;
 end;

 fCurrentContour:=TpvVectorPathContour.Create;
 fCurrentContour.Closed:=true;
 fCurrentShape.Contours.Add(fCurrentContour);

 fCurrentPosition:=ToVector(aX,aY);
 fStartPosition:=fCurrentPosition;
 fHasCurrentPosition:=true;

end;

procedure TpvVectorGraphicsRasterizerClipping.LineTo(const aX,aY:TpvInt32);
var Position:TpvVectorPathVector;
begin

 if not (fHasCurrentPosition and assigned(fCurrentContour)) then begin
  MoveTo(aX,aY);
  exit;
 end;

 Position:=ToVector(aX,aY);
 fCurrentContour.Segments.Add(TpvVectorPathSegmentLine.Create(fCurrentPosition,Position));
 fCurrentPosition:=Position;

end;

procedure TpvVectorGraphicsRasterizerClipping.Close;
begin

 if fHasCurrentPosition and assigned(fCurrentContour) then begin
  if (not SameValue(fCurrentPosition.x,fStartPosition.x)) or
     (not SameValue(fCurrentPosition.y,fStartPosition.y)) then begin
   fCurrentContour.Segments.Add(TpvVectorPathSegmentLine.Create(fCurrentPosition,fStartPosition));
   fCurrentPosition:=fStartPosition;
  end;
 end;

 fCurrentContour:=nil;
 fHasCurrentPosition:=false;

end;

procedure TpvVectorGraphicsRasterizerClipping.Render;
var Clipped:TpvVectorPathShape;
    Identity:TpvVectorPathMatrix;
begin

 Close;

 if (not assigned(fTarget)) or (not assigned(fCurrentShape)) or (fCurrentShape.Contours.Count=0) then begin
  Reset;
  exit;
 end;

 Identity:=TpvVectorPathMatrix.CreateIdentity;

 // Without a clip region there is nothing to cut, so the geometry goes straight through.
 if not assigned(fClipShape) then begin
  fTarget.FillRule:=fFillRule;
  fTarget.FillShape(fCurrentShape,Identity,false);
  Reset;
  exit;
 end;

 Clipped:=pvVectorPathIntersection(fCurrentShape,fClipShape,fFillRule,TpvVectorPathFillRule.NonZero);
 try

  if Clipped.Contours.Count>0 then begin
   // The intersection comes back in non-zero form no matter what went in.
   fTarget.FillRule:=TpvVectorPathFillRule.NonZero;
   fTarget.FillShape(Clipped,Identity,false);
  end;

 finally
  FreeAndNil(Clipped);
 end;

 Reset;

end;

end.
