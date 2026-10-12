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
     PasVulkan.VectorGraphics.Filters;

type { TpvVectorGraphicsRasterizerRecorder }

     // A rasterizer that draws nothing and instead keeps what it was asked to draw. Every Render
     // call appends one entry holding the flattened outline in device space together with the
     // paint color and fill rule that were in effect.
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
             public
              constructor Create; reintroduce;
              destructor Destroy; override;
             public
              property BoundingBox:TpvVectorPathBoundingBox read fBoundingBox;
              // What the filter needs to know about where the subtree sits, handed over with it.
              property FilterContext:TpvVectorGraphicsFilterContext read fFilterContext write fFilterContext;
             published
              property Kind:TRecordKind read fKind;
              // The outline in device space, made of line segments only, ready to be fed to
              // TpvVectorPathGPUShape or to be triangulated. Only a shape record has one.
              property Shape:TpvVectorPathShape read fShape;
              property Color:TpvVectorGraphicsColor read fColor;
              property FillRule:TpvVectorPathFillRule read fFillRule;
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
       function ToVector(const aX,aY:TpvInt32):TpvVectorPathVector; {$ifdef CANINLINE}inline;{$endif}
      public
       constructor Create(const aPixelBuffer:TpvVectorGraphicsPixelBuffer=nil); override;
       destructor Destroy; override;
       procedure Reset; override;
       procedure MoveTo(const aX,aY:TpvInt32); override;
       procedure LineTo(const aX,aY:TpvInt32); override;
       procedure Close; override;
       procedure Render; override;
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

function TpvVectorGraphicsRasterizerRecorder.BeginTexture(const aFilter:TpvVectorGraphicsFilter;
                                                          const aFilterContext:TpvVectorGraphicsFilterContext;
                                                          const aAlpha:TpvUInt8;
                                                          const aBlendMode:TpvVectorGraphicsBlendMode):TpvSizeInt;
var CurrentRecord:TRecord_;
begin

 // Anything half drawn belongs before the texture, not inside it.
 Render;

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
end;

function TpvVectorGraphicsRasterizerRecorder.ExtractRecords:TRecords;
begin
 result:=fRecords;
 fRecords:=TRecords.Create;
 fRecords.OwnsObjects:=true;
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

 except
  FreeAndNil(CurrentRecord);
  raise;
 end;

 fRecords.Add(CurrentRecord);

 Reset;

end;

end.
