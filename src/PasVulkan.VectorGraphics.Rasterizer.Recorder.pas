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
unit PasVulkan.VectorGraphics.Rasterizer.Recorder;
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
     PasVulkan.VectorGraphics.Rasterizer,
     PasVulkan.VectorGraphics.Paints,
     PasVulkan.VectorGraphics.Filters;

type { TpvVectorGraphicsRasterizerRecorder }

     // A rasterizer that draws nothing and instead keeps what it was asked to draw. Every Render
     // call appends one entry holding the outline in device space, curved segments included,
     // together with the fill and the fill rule that were in effect.
     //
     // This is the hand-off point to the GPU side: the recorded shapes go straight into
     // TpvVectorPathGPUShape, which builds the segment grid the vector shader samples. The whole
     // scene graph, including clip paths, patterns and the stroker, therefore runs unchanged for
     // both the software and the GPU path, and only the backend differs.
     TpvVectorGraphicsRasterizerRecorder=class(TpvVectorGraphicsRasterizer)
      public
       type { TRecordKind }

            // What one entry of the recording stands for. A shape is drawn outright, while a
            // texture marks a part of the picture that cannot be expressed as shapes at all and
            // has to be rendered on its own and then put through a filter chain.
            TRecordKind=
             (
              Shape=0,
              Texture
             );

            { TFillKind }

            // How the recorded area is filled. A solid fill is the plain color, while the two
            // gradients come with the ramp and the mapping a backend needs in order to evaluate
            // them by itself.
            TFillKind=
             (
              Solid=0,
              LinearGradient,
              RadialGradient
             );

            // The ramp of a recorded gradient, the same 256 entries the software rasterizer
            // samples, so that both routes come out on the same colors.
            TGradientRamp=array[0..255] of TpvVectorGraphicsColor;

            PGradientRamp=^TGradientRamp;

            { TRecord_ }
            TRecord_=class
             private
              fKind:TRecordKind;
              fShape:TpvVectorPathShape;
              fColor:TpvVectorGraphicsColor;
              fFillRule:TpvVectorPathFillRule;
              fBlendMode:TpvVectorGraphicsBlendMode;
              fBoundingBox:TpvVectorPathBoundingBox;
              fFilter:TpvVectorGraphicsFilter;
              fFilterContext:TpvVectorGraphicsFilterContext;
              fAlpha:TpvUInt8;
              fFirstChildIndex:TpvSizeInt;
              fCountChildren:TpvSizeInt;
              fFillKind:TFillKind;
              fGradientMatrix:TpvVectorPathMatrix;
              fGradientFocus:TpvVectorPathVector;
              fGradientSpreadMethod:TpvVectorGraphicsSpreadMethod;
              fGradientRamp:TGradientRamp;
              function GetGradientRamp:PGradientRamp;
             public
              constructor Create; reintroduce;
              destructor Destroy; override;
             public
              property BoundingBox:TpvVectorPathBoundingBox read fBoundingBox;
              // What the filter needs to know about where the subtree sits, handed over with it.
              property FilterContext:TpvVectorGraphicsFilterContext read fFilterContext write fFilterContext;
              // Takes a point of the shape into the space of the gradient, with the radius
              // already divided out, so that the ramp runs from zero to one along the local x
              // axis of a linear gradient and from the origin outwards for a radial one.
              property GradientMatrix:TpvVectorPathMatrix read fGradientMatrix;
              // The focus point of a radial gradient, in that same space. Zero means centered,
              // which is also what a linear gradient carries.
              property GradientFocus:TpvVectorPathVector read fGradientFocus;
              // The 256 entries of the ramp. Only meaningful for a gradient fill.
              property GradientRamp:PGradientRamp read GetGradientRamp;
             published
              property Kind:TRecordKind read fKind;
              // The outline in device space, with its curved segments still in it, ready to be
              // fed to TpvVectorPathGPUShape. Only a shape record has one.
              property Shape:TpvVectorPathShape read fShape;
              // The plain color of a solid fill. A gradient fill carries opaque white here, so
              // that a backend can multiply the two without having to tell the cases apart, and
              // the opacity of the gradient sits in its ramp.
              property Color:TpvVectorGraphicsColor read fColor;
              property FillRule:TpvVectorPathFillRule read fFillRule;
              // Whether the area is one color or one of the two gradients.
              property FillKind:TFillKind read fFillKind;
              // What happens outside the zero to one range of the gradient.
              property GradientSpreadMethod:TpvVectorGraphicsSpreadMethod read fGradientSpreadMethod;
              // How this entry is meant to combine with what is already there. The GPU side has
              // to honour it, since the recorder itself composites nothing.
              property BlendMode:TpvVectorGraphicsBlendMode read fBlendMode;
              // For a texture record: the filter to run over the rendered subtree, the opacity
              // the outcome is composited with, and which of the records that follow make up the
              // subtree. The filter belongs to the document, not to the record.
              property Filter:TpvVectorGraphicsFilter read fFilter;
              property Alpha:TpvUInt8 read fAlpha;
              property FirstChildIndex:TpvSizeInt read fFirstChildIndex;
              property CountChildren:TpvSizeInt read fCountChildren;
            end;
            TRecords=TpvObjectGenericList<TRecord_>;
      private
       fRecords:TRecords;
       fCurrentShape:TpvVectorPathShape;
       fCurrentContour:TpvVectorPathContour;
       fHasCurrentPosition:boolean;
       fCurrentPosition:TpvVectorPathVector;
       fStartPosition:TpvVectorPathVector;
       fFlushEmptyRecords:boolean;
       fFullyRecorded:boolean;
       // The buffer that was in effect when this recorder was created, which is the one that
       // stands for the page itself.
       fBasePixelBuffer:TpvVectorGraphicsPixelBuffer;
       // True while drawing goes into a buffer other than that one, which is how a coverage mask
       // and a pattern tile are built. Those are pixels by nature, so nothing of them can be
       // recorded and whatever would be is dropped instead of ending up in the page.
       fInForeignPixelBuffer:boolean;
       function ToVector(const aX,aY:TpvInt32):TpvVectorPathVector; {$ifdef CANINLINE}inline;{$endif}
       // Copies whatever the current paint is into aRecord: a plain color, or the ramp and the
       // mapping of one of the two gradients. A paint that is neither leaves the record on its
       // solid default and makes FullyRecorded say no.
       procedure TakeFill(const aRecord:TRecord_);
      public
       constructor Create(const aPixelBuffer:TpvVectorGraphicsPixelBuffer=nil); override;
       destructor Destroy; override;
       procedure Reset; override;
       procedure MoveTo(const aX,aY:TpvInt32); override;
       procedure LineTo(const aX,aY:TpvInt32); override;
       procedure Close; override;
       procedure Render; override;
       function WantsCurvedSegments:boolean; override;
       function ReplacePixelBuffer(const aPixelBuffer:TpvVectorGraphicsPixelBuffer):TpvVectorGraphicsPixelBuffer; override;
       procedure FillShape(const aShape:TpvVectorPathShape;const aMatrix:TpvVectorPathMatrix;
                           const aUseShapeFillRule:boolean=true); override;
       // Opens a texture record and returns its index. Everything recorded until the matching
       // EndTexture belongs to it, which is how a filtered subtree reaches the GPU side: render
       // those entries into a texture of their own, run the filter chain over it, then composite
       // the outcome with the given opacity and blend mode.
       function BeginTexture(const aFilter:TpvVectorGraphicsFilter;
                             const aFilterContext:TpvVectorGraphicsFilterContext;
                             const aAlpha:TpvUInt8;
                             const aBlendMode:TpvVectorGraphicsBlendMode):TpvSizeInt;
       // Closes the texture record that aIndex opened.
       procedure EndTexture(const aIndex:TpvSizeInt);
       // Throws away everything recorded so far.
       procedure ClearRecords;
       // Hands the records over to the caller, who then owns them, and starts an empty list.
       function ExtractRecords:TRecords;
       // Union of the bounding boxes of everything recorded.
       function GetBoundingBox:TpvVectorPathBoundingBox;
      published
       property Records:TRecords read fRecords;
       // When set, a Render that produced no geometry still appends an empty record, which keeps
       // the record indices lined up with the draw calls for callers that rely on that.
       property FlushEmptyRecords:boolean read fFlushEmptyRecords write fFlushEmptyRecords;
       // True while the records describe everything that was drawn, which is the case as long as
       // every area was filled with a plain color or with one of the two gradients, with no
       // pattern or image paint, no coverage mask, no blend mode and no texture record among
       // them. A backend that draws nothing but filled outlines asks this to find out whether it
       // can take the records as they are or has to fall back to a software render. Reset by
       // ClearRecords and by ExtractRecords, so it covers one recording run.
       property FullyRecorded:boolean read fFullyRecorded;
     end;

implementation

{ TpvVectorGraphicsRasterizerRecorder.TRecord_ }

constructor TpvVectorGraphicsRasterizerRecorder.TRecord_.Create;
begin

 inherited Create;

 fKind:=TRecordKind.Shape;
 fShape:=TpvVectorPathShape.Create;
 fColor:=0;
 fFillRule:=TpvVectorPathFillRule.NonZero;
 fFillKind:=TFillKind.Solid;
 fGradientMatrix:=TpvVectorPathMatrix.CreateIdentity;
 fGradientFocus:=TpvVectorPathVector.Create(0.0,0.0);
 fGradientSpreadMethod:=TpvVectorGraphicsSpreadMethod.Pad;
 FillChar(fGradientRamp,SizeOf(TGradientRamp),#0);
 fBlendMode:=TpvVectorGraphicsBlendMode.Normal;

 // Only a texture record uses these.
 fFilter:=nil;
 fAlpha:=255;
 fFirstChildIndex:=0;
 fCountChildren:=0;

 // The inverted bounds of an empty box, so that the first point set on it decides all four sides.
 fBoundingBox:=TpvVectorPathBoundingBox.Create(TpvVectorPathVector.Create(MaxDouble,MaxDouble),
                                               TpvVectorPathVector.Create(-MaxDouble,-MaxDouble));

end;

destructor TpvVectorGraphicsRasterizerRecorder.TRecord_.Destroy;
begin
 FreeAndNil(fShape);
 inherited Destroy;
end;

function TpvVectorGraphicsRasterizerRecorder.TRecord_.GetGradientRamp:PGradientRamp;
begin
 result:=@fGradientRamp;
end;

{ TpvVectorGraphicsRasterizerRecorder }

constructor TpvVectorGraphicsRasterizerRecorder.Create(const aPixelBuffer:TpvVectorGraphicsPixelBuffer);
begin

 inherited Create(aPixelBuffer);

 fRecords:=TRecords.Create;
 fRecords.OwnsObjects:=true;

 fCurrentShape:=nil;
 fCurrentContour:=nil;
 fHasCurrentPosition:=false;
 fFlushEmptyRecords:=false;
 fFullyRecorded:=true;
 fBasePixelBuffer:=fPixelBuffer;
 fInForeignPixelBuffer:=false;

end;

destructor TpvVectorGraphicsRasterizerRecorder.Destroy;
begin

 FreeAndNil(fCurrentShape);
 FreeAndNil(fRecords);

 inherited Destroy;

end;

function TpvVectorGraphicsRasterizerRecorder.ToVector(const aX,aY:TpvInt32):TpvVectorPathVector;
const OneOverPixelFactor=1.0/pvVectorGraphicsPixelFactor;
begin
 result.x:=aX*OneOverPixelFactor;
 result.y:=aY*OneOverPixelFactor;
end;

procedure TpvVectorGraphicsRasterizerRecorder.TakeFill(const aRecord:TRecord_);
var GradientPaint:TpvVectorGraphicsGradientPaint;
    RadialPaint:TpvVectorGraphicsRadialGradientPaint;
    Radius,OneOverRadius:TpvDouble;
    Focus:TpvVectorPathVector;
    SourceRamp:PpvVectorGraphicsGradientBuffer;
    Index:TpvSizeInt;
    Opacity:TpvUInt8;
begin

 if not (assigned(fPaint) and (fPaint is TpvVectorGraphicsGradientPaint)) then begin
  exit;
 end;

 GradientPaint:=TpvVectorGraphicsGradientPaint(fPaint);

 // A radius of zero would make the mapping below collapse, and the software rasterizer treats it
 // as one, so the same is done here.
 Radius:=GradientPaint.Radius;
 if SameValue(Radius,0.0) then begin
  Radius:=1.0;
 end;
 OneOverRadius:=1.0/Radius;

 if GradientPaint is TpvVectorGraphicsRadialGradientPaint then begin
  aRecord.fFillKind:=TFillKind.RadialGradient;
  RadialPaint:=TpvVectorGraphicsRadialGradientPaint(GradientPaint);
  Focus:=RadialPaint.FocusMatrix.Transform(RadialPaint.FocusX,RadialPaint.FocusY);
  aRecord.fGradientFocus:=TpvVectorPathVector.Create(Focus.x*OneOverRadius,Focus.y*OneOverRadius);
 end else begin
  aRecord.fFillKind:=TFillKind.LinearGradient;
  aRecord.fGradientFocus:=TpvVectorPathVector.Create(0.0,0.0);
 end;

 // Dividing the radius out here is what lets the ramp run from zero to one on the other side,
 // which in turn is what keeps the radial formula free of the radius altogether.
 aRecord.fGradientMatrix:=GradientPaint.Matrix*TpvVectorPathMatrix.CreateScale(OneOverRadius,OneOverRadius);

 aRecord.fGradientSpreadMethod:=GradientPaint.SpreadMethod;

 // The opacity of the enclosing object goes into the ramp rather than being carried separately,
 // which is the same thing the software rasterizer does to every sample it takes.
 SourceRamp:=GradientPaint.Buffer;
 Opacity:=GradientPaint.Opacity;
 if Opacity=255 then begin
  Move(SourceRamp^[0],aRecord.fGradientRamp[0],SizeOf(TGradientRamp));
 end else begin
  for Index:=0 to 255 do begin
   aRecord.fGradientRamp[Index]:=pvVectorGraphicsModulateColor(SourceRamp^[Index],Opacity);
  end;
 end;

 // Whatever color the paint happened to carry has no say any more, so it is made neutral.
 aRecord.fColor:=TpvVectorGraphicsColor($ffffffff);

end;

function TpvVectorGraphicsRasterizerRecorder.BeginTexture(const aFilter:TpvVectorGraphicsFilter;
                                                          const aFilterContext:TpvVectorGraphicsFilterContext;
                                                          const aAlpha:TpvUInt8;
                                                          const aBlendMode:TpvVectorGraphicsBlendMode):TpvSizeInt;
var CurrentRecord:TRecord_;
begin

 // Anything half drawn belongs before the texture, not inside it.
 Render;

 // A texture record is a subtree that gets composited as a whole, which a backend that only
 // draws filled outlines cannot do.
 fFullyRecorded:=false;

 CurrentRecord:=TRecord_.Create;
 CurrentRecord.fKind:=TRecordKind.Texture;
 CurrentRecord.fFilter:=aFilter;
 CurrentRecord.fFilterContext:=aFilterContext;
 CurrentRecord.fAlpha:=aAlpha;
 CurrentRecord.fBlendMode:=aBlendMode;

 // The subtree starts right after this entry, and how long it is only becomes known at the end.
 result:=fRecords.Add(CurrentRecord);
 CurrentRecord.fFirstChildIndex:=result+1;
 CurrentRecord.fCountChildren:=0;

end;

procedure TpvVectorGraphicsRasterizerRecorder.EndTexture(const aIndex:TpvSizeInt);
var CurrentRecord:TRecord_;
begin

 if (aIndex<0) or (aIndex>=fRecords.Count) then begin
  exit;
 end;

 Render;

 CurrentRecord:=fRecords[aIndex];
 if CurrentRecord.fKind<>TRecordKind.Texture then begin
  exit;
 end;

 CurrentRecord.fCountChildren:=fRecords.Count-CurrentRecord.fFirstChildIndex;

end;

procedure TpvVectorGraphicsRasterizerRecorder.ClearRecords;
begin
 fRecords.Clear;
 fFullyRecorded:=true;
end;

function TpvVectorGraphicsRasterizerRecorder.ExtractRecords:TRecords;
begin
 result:=fRecords;
 fRecords:=TRecords.Create;
 fRecords.OwnsObjects:=true;
 fFullyRecorded:=true;
end;

function TpvVectorGraphicsRasterizerRecorder.GetBoundingBox:TpvVectorPathBoundingBox;
var Index:TpvSizeInt;
begin

 result:=TpvVectorPathBoundingBox.Create(TpvVectorPathVector.Create(MaxDouble,MaxDouble),
                                         TpvVectorPathVector.Create(-MaxDouble,-MaxDouble));

 for Index:=0 to fRecords.Count-1 do begin
  result:=result.Combine(fRecords[Index].fBoundingBox);
 end;

end;

procedure TpvVectorGraphicsRasterizerRecorder.Reset;
begin

 inherited Reset;

 FreeAndNil(fCurrentShape);
 fCurrentContour:=nil;
 fHasCurrentPosition:=false;

end;

procedure TpvVectorGraphicsRasterizerRecorder.MoveTo(const aX,aY:TpvInt32);
begin

 Close;

 if not assigned(fCurrentShape) then begin
  fCurrentShape:=TpvVectorPathShape.Create;
 end;

 // Each subpath becomes a contour of its own.
 fCurrentContour:=TpvVectorPathContour.Create;
 fCurrentContour.Closed:=true;
 fCurrentShape.Contours.Add(fCurrentContour);

 fCurrentPosition:=ToVector(aX,aY);
 fStartPosition:=fCurrentPosition;
 fHasCurrentPosition:=true;

end;

procedure TpvVectorGraphicsRasterizerRecorder.LineTo(const aX,aY:TpvInt32);
var Position:TpvVectorPathVector;
begin

 // A line without a preceding move starts a subpath of its own.
 if not (fHasCurrentPosition and assigned(fCurrentContour)) then begin
  MoveTo(aX,aY);
  exit;
 end;

 Position:=ToVector(aX,aY);
 fCurrentContour.Segments.Add(TpvVectorPathSegmentLine.Create(fCurrentPosition,Position));
 fCurrentPosition:=Position;

end;

procedure TpvVectorGraphicsRasterizerRecorder.Close;
begin

 // A contour that does not already end where it began gets the closing segment here.
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

procedure TpvVectorGraphicsRasterizerRecorder.Render;
var CurrentRecord:TRecord_;
    HasGeometry:boolean;
begin

 Close;

 // A coverage mask or a pattern tile is being built, which only exists as pixels. Recording its
 // outlines would put them into the page as if they were drawn there, so they are dropped and
 // the run is marked as no longer describing everything.
 if fInForeignPixelBuffer then begin
  fFullyRecorded:=false;
  Reset;
  exit;
 end;

 HasGeometry:=assigned(fCurrentShape) and (fCurrentShape.Contours.Count>0);
 if not (HasGeometry or fFlushEmptyRecords) then begin
  Reset;
  exit;
 end;

 CurrentRecord:=TRecord_.Create;
 try

  // The shape moves into the record rather than being copied, which keeps a page full of shapes
  // from being duplicated on every frame.
  if HasGeometry then begin
   FreeAndNil(CurrentRecord.fShape);
   CurrentRecord.fShape:=fCurrentShape;
   fCurrentShape:=nil;
   CurrentRecord.fBoundingBox:=CurrentRecord.fShape.GetBoundingBox;
  end;

  CurrentRecord.fShape.FillRule:=fFillRule;
  CurrentRecord.fColor:=fPaint.Color;
  CurrentRecord.fFillRule:=fFillRule;
  CurrentRecord.fBlendMode:=fElementBlendMode;

  TakeFill(CurrentRecord);

 except
  FreeAndNil(CurrentRecord);
  raise;
 end;

 fRecords.Add(CurrentRecord);

 Reset;

end;

function TpvVectorGraphicsRasterizerRecorder.WantsCurvedSegments:boolean;
begin
 result:=true;
end;

function TpvVectorGraphicsRasterizerRecorder.ReplacePixelBuffer(const aPixelBuffer:TpvVectorGraphicsPixelBuffer):TpvVectorGraphicsPixelBuffer;
begin
 result:=inherited ReplacePixelBuffer(aPixelBuffer);
 fInForeignPixelBuffer:=fPixelBuffer<>fBasePixelBuffer;
end;

procedure TpvVectorGraphicsRasterizerRecorder.FillShape(const aShape:TpvVectorPathShape;
                                                        const aMatrix:TpvVectorPathMatrix;
                                                        const aUseShapeFillRule:boolean);
begin

 if not assigned(aShape) then begin
  exit;
 end;

 Reset;

 if aUseShapeFillRule then begin
  fFillRule:=aShape.FillRule;
 end;

 // A plain color and the two gradients are what a record can carry. A pattern or an image fill
 // is a buffer of pixels and would come out as one flat color, and so would a coverage mask,
 // which lives in the pixels as well and has nowhere to go in a record. A blend mode normally
 // brings a texture record with it, which already settles the question, but it is asked here as
 // well so that the answer does not depend on how the caller went about it.
 if (not assigned(fPaint)) or
    not (fPaint.IsSolid or (fPaint is TpvVectorGraphicsGradientPaint)) or
    assigned(fMask) or
    (fElementBlendMode<>TpvVectorGraphicsBlendMode.Normal) then begin
  fFullyRecorded:=false;
 end;

 // The outline is taken over as it stands instead of being walked through MoveTo and LineTo,
 // so that its curved segments survive. That is what makes the recorded shape independent of
 // the resolution it will later be drawn at, because the GPU side evaluates quadratic curves
 // by itself and breaks the cubic ones down when it builds its own copy.
 fCurrentShape:=TpvVectorPathShape.Create;
 fCurrentShape.Assign(aShape);
 fCurrentShape.Transform(aMatrix);

 Render;

end;

end.
