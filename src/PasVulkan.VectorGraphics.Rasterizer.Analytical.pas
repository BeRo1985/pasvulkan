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
unit PasVulkan.VectorGraphics.Rasterizer.Analytical;
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
     PasMP,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.VectorPath,
     PasVulkan.VectorGraphics.Rasterizer;

type { TpvVectorGraphicsRasterizerAnalytical }

     // Exact area rasterizer after the manner of the cell based scanline converter in libart and
     // Anti-Grain Geometry. Every edge contributes signed area and cover to the cells it passes
     // through; summing the cells of a scanline left to right then yields the exact coverage of
     // every span, with no sampling anywhere, which is what makes this the quality reference the
     // other backends get compared against.
     TpvVectorGraphicsRasterizerAnalytical=class(TpvVectorGraphicsRasterizer)
      private
       type TCell=record
             x:TpvInt32;
             Area:TpvInt32;
             Cover:TpvInt32;
             // The cells of one scanline form a sorted singly linked list. Indices instead of
             // pointers, so that growing the cell array stays safe and cheap.
             PreviousIndex:TpvSizeInt;
             NextIndex:TpvSizeInt;
            end;
            PCell=^TCell;
            TCells=array of TCell;
            TScanLine=record
             CellRootIndex:TpvSizeInt;
             CellLastIndex:TpvSizeInt;
            end;
            PScanLine=^TScanLine;
            TScanLines=array of TScanLine;
      private
       const CellNone=-1;
             MemoryIncrementBits=8;
             MemoryIncrement=1 shl MemoryIncrementBits;
             MemoryIncrementMask=MemoryIncrement-1;
      private
       fCellX:TpvInt32;
       fCellY:TpvInt32;
       fArea:TpvInt32;
       fCover:TpvInt32;
       fCells:TCells;
       fCountCells:TpvSizeInt;
       fX:TpvInt32;
       fY:TpvInt32;
       fMoveToX:TpvInt32;
       fMoveToY:TpvInt32;
       fLastX:TpvInt32;
       fLastY:TpvInt32;
       fScanLines:TScanLines;
       fRenderMinimumY:TpvInt32;
       fRenderMaximumY:TpvInt32;
       fNeedToClose:boolean;
       fPasMPInstance:TPasMP;
       function NewCell:TpvSizeInt;
       procedure RecordCell;
       procedure SetCell(const aCellX,aCellY:TpvInt32;const aForce:boolean=false);
       procedure StartCell(const aCellX,aCellY:TpvInt32);
       procedure RenderScanLine(const aCellY,aX1,aY1,aX2,aY2:TpvInt32);
       procedure RenderLine(const aToX,aToY:TpvInt32);
       procedure ProcessSpan(const aX,aY,aArea,aCount,aThreadIndex:TpvInt32);
       procedure ProcessScanLinesParallelForJob(const aJob:PPasMPJob;const aThreadIndex:TPasMPInt32;const aData:pointer;const aFromIndex,aToIndex:TPasMPNativeInt);
       procedure ProcessScanLines(const aFromIndex,aToIndex,aThreadIndex:TpvInt32);
       procedure MakeScanLineSpansAndRenderThese;
      public
       constructor Create(const aPixelBuffer:TpvVectorGraphicsPixelBuffer=nil); override;
       destructor Destroy; override;
       procedure Resize(const aWidth,aHeight:TpvInt32); override;
       procedure Reset; override;
       procedure MoveTo(const aX,aY:TpvInt32); override;
       procedure LineTo(const aX,aY:TpvInt32); override;
       procedure Close; override;
       procedure Render; override;
      published
       // When set and more than one worker thread is available, the scanlines get blended in
       // parallel. Off for rasterizations that run inside an already parallel job.
       property PasMPInstance:TPasMP read fPasMPInstance write fPasMPInstance;
     end;

implementation

{ TpvVectorGraphicsRasterizerAnalytical }

constructor TpvVectorGraphicsRasterizerAnalytical.Create(const aPixelBuffer:TpvVectorGraphicsPixelBuffer);
begin

 inherited Create(aPixelBuffer);

 fPasMPInstance:=TPasMP.GetGlobalInstance;

 fCells:=nil;
 fCountCells:=0;
 fScanLines:=nil;

 Resize(Width,Height);
 Reset;

end;

destructor TpvVectorGraphicsRasterizerAnalytical.Destroy;
begin

 fCells:=nil;
 fScanLines:=nil;

 inherited Destroy;

end;

procedure TpvVectorGraphicsRasterizerAnalytical.Resize(const aWidth,aHeight:TpvInt32);
var Index:TpvSizeInt;
begin

 inherited Resize(aWidth,aHeight);

 // One scanline per pixel row, each holding the list of cells the edges touched there.
 if length(fScanLines)<>Height then begin
  SetLength(fScanLines,Height);
  for Index:=0 to length(fScanLines)-1 do begin
   fScanLines[Index].CellRootIndex:=CellNone;
   fScanLines[Index].CellLastIndex:=CellNone;
  end;
 end;

end;

procedure TpvVectorGraphicsRasterizerAnalytical.Reset;
var Index:TpvSizeInt;
begin

 inherited Reset;

 // The cells are reused rather than freed, so only the count goes back to zero.
 fCountCells:=0;
 fArea:=0;
 fCover:=0;

 for Index:=0 to length(fScanLines)-1 do begin
  fScanLines[Index].CellRootIndex:=CellNone;
  fScanLines[Index].CellLastIndex:=CellNone;
 end;

 // An impossible cell, so that the first SetCell always records a change.
 fCellX:=-1;
 fCellY:=-1;

 // An empty range, which the first touched scanline then opens up.
 fRenderMinimumY:=High(TpvInt32);
 fRenderMaximumY:=Low(TpvInt32);

 fNeedToClose:=false;
 fX:=0;
 fY:=0;
 fMoveToX:=0;
 fMoveToY:=0;
 fLastX:=0;
 fLastY:=0;

end;

function TpvVectorGraphicsRasterizerAnalytical.NewCell:TpvSizeInt;
 procedure InsertBefore(const aNewIndex,aOldIndex:TpvSizeInt);
 var NewCell,OldCell:PCell;
 begin
  NewCell:=@fCells[aNewIndex];
  OldCell:=@fCells[aOldIndex];
  NewCell^.NextIndex:=aOldIndex;
  NewCell^.PreviousIndex:=OldCell^.PreviousIndex;
  if OldCell^.PreviousIndex<>CellNone then begin
   fCells[OldCell^.PreviousIndex].NextIndex:=aNewIndex;
  end;
  OldCell^.PreviousIndex:=aNewIndex;
  if aOldIndex=fScanLines[fCellY].CellRootIndex then begin
   fScanLines[fCellY].CellRootIndex:=aNewIndex;
  end;
 end;
 procedure InsertAfter(const aOldIndex,aNewIndex:TpvSizeInt);
 var NewCell,OldCell:PCell;
 begin
  NewCell:=@fCells[aNewIndex];
  OldCell:=@fCells[aOldIndex];
  NewCell^.NextIndex:=OldCell^.NextIndex;
  NewCell^.PreviousIndex:=aOldIndex;
  if OldCell^.NextIndex<>CellNone then begin
   fCells[OldCell^.NextIndex].PreviousIndex:=aNewIndex;
  end;
  OldCell^.NextIndex:=aNewIndex;
  if aOldIndex=fScanLines[fCellY].CellLastIndex then begin
   fScanLines[fCellY].CellLastIndex:=aNewIndex;
  end;
 end;
 function GetCell:TpvSizeInt;
 var Cell:PCell;
 begin
  if fCountCells>=length(fCells) then begin
   SetLength(fCells,((fCountCells+1)+MemoryIncrement) and not TpvSizeInt(MemoryIncrementMask));
  end;
  result:=fCountCells;
  inc(fCountCells);
  Cell:=@fCells[result];
  Cell^.x:=fCellX;
  Cell^.Area:=0;
  Cell^.Cover:=0;
  Cell^.PreviousIndex:=CellNone;
  Cell^.NextIndex:=CellNone;
 end;
var CurrentIndex:TpvSizeInt;
    ScanLine:PScanLine;
begin

 // Outside the buffer there is no scanline to put a cell on.
 if (fCellY<0) or (fCellY>=length(fScanLines)) then begin
  result:=CellNone;
  exit;
 end;

 ScanLine:=@fScanLines[fCellY];

 if (ScanLine^.CellRootIndex=CellNone) or (ScanLine^.CellLastIndex=CellNone) then begin

  // First cell on this scanline.
  result:=GetCell;
  fScanLines[fCellY].CellRootIndex:=result;
  fScanLines[fCellY].CellLastIndex:=result;

 end else if fCells[ScanLine^.CellRootIndex].x=fCellX then begin

  // The two ends are checked first, since edges arrive roughly in order and usually land there.
  result:=ScanLine^.CellRootIndex;

 end else if fCells[ScanLine^.CellLastIndex].x=fCellX then begin

  result:=ScanLine^.CellLastIndex;

 end else if fCells[ScanLine^.CellRootIndex].x>fCellX then begin

  CurrentIndex:=ScanLine^.CellRootIndex;
  result:=GetCell;
  InsertBefore(result,CurrentIndex);

 end else if fCells[ScanLine^.CellLastIndex].x<fCellX then begin

  CurrentIndex:=ScanLine^.CellLastIndex;
  result:=GetCell;
  InsertAfter(CurrentIndex,result);

 end else begin

  // Somewhere in the middle, so the list has to be walked.
  CurrentIndex:=ScanLine^.CellRootIndex;
  while (CurrentIndex<>CellNone) and (fCells[CurrentIndex].x<fCellX) do begin
   CurrentIndex:=fCells[CurrentIndex].NextIndex;
  end;

  if CurrentIndex=CellNone then begin
   CurrentIndex:=ScanLine^.CellLastIndex;
   result:=GetCell;
   InsertAfter(CurrentIndex,result);
  end else if fCells[CurrentIndex].x=fCellX then begin
   result:=CurrentIndex;
  end else begin
   result:=GetCell;
   InsertBefore(result,CurrentIndex);
  end;

 end;

 // Remember how far the drawing reaches, so that only the touched scanlines get blended later.
 if fRenderMinimumY>fCellY then begin
  fRenderMinimumY:=fCellY;
 end;
 if fRenderMaximumY<fCellY then begin
  fRenderMaximumY:=fCellY;
 end;

end;

procedure TpvVectorGraphicsRasterizerAnalytical.RecordCell;
var CellIndex:TpvSizeInt;
    Cell:PCell;
begin

 // A cell that gathered neither area nor cover contributes nothing and need not exist.
 if (fArea<>0) or (fCover<>0) then begin
  CellIndex:=NewCell;
  if CellIndex<>CellNone then begin
   Cell:=@fCells[CellIndex];
   inc(Cell^.Area,fArea);
   inc(Cell^.Cover,fCover);
  end;
 end;

end;

procedure TpvVectorGraphicsRasterizerAnalytical.SetCell(const aCellX,aCellY:TpvInt32;const aForce:boolean);
begin

 // Moving on means banking what the current cell gathered and starting the next one empty.
 if (fCellX<>aCellX) or (fCellY<>aCellY) or aForce then begin
  RecordCell;
  fCellX:=aCellX;
  fCellY:=aCellY;
  fArea:=0;
  fCover:=0;
 end;

end;

procedure TpvVectorGraphicsRasterizerAnalytical.StartCell(const aCellX,aCellY:TpvInt32);
begin
 fArea:=0;
 fCover:=0;
 SetCell(aCellX,aCellY,true);
end;

procedure TpvVectorGraphicsRasterizerAnalytical.RenderScanLine(const aCellY,aX1,aY1,aX2,aY2:TpvInt32);
var CellX1,CellX2,FractionX1,FractionX2,Delta,p,First,DeltaX,Increment,Lift,Modulus,Remainder,Y1:TpvInt32;
begin

 DeltaX:=aX2-aX1;
 CellX1:=aX1 div pvVectorGraphicsPixelFactor;
 CellX2:=aX2 div pvVectorGraphicsPixelFactor;
 FractionX1:=aX1 mod pvVectorGraphicsPixelFactor;
 FractionX2:=aX2 mod pvVectorGraphicsPixelFactor;

 // Horizontal within the subpixel grid, so no area at all. Happens often.
 if aY1=aY2 then begin
  SetCell(CellX2,aCellY);
  exit;
 end;

 // Everything inside one cell, which is the common case for nearly vertical edges.
 if CellX1=CellX2 then begin
  Delta:=aY2-aY1;
  inc(fArea,(FractionX1+FractionX2)*Delta);
  inc(fCover,Delta);
  exit;
 end;

 // A run of adjacent cells has to be walked, so set up the direction and the first partial step.
 if DeltaX<0 then begin
  p:=FractionX1*(aY2-aY1);
  First:=0;
  Increment:=-1;
  DeltaX:=-DeltaX;
 end else begin
  p:=(pvVectorGraphicsOnePixel-FractionX1)*(aY2-aY1);
  First:=pvVectorGraphicsOnePixel;
  Increment:=1;
 end;

 Delta:=p div DeltaX;
 Modulus:=p mod DeltaX;
 if Modulus<0 then begin
  dec(Delta);
  inc(Modulus,DeltaX);
 end;

 // The partial cell the edge starts in.
 inc(fArea,(FractionX1+First)*Delta);
 inc(fCover,Delta);
 inc(CellX1,Increment);
 SetCell(CellX1,aCellY);
 Y1:=aY1+Delta;

 // The whole cells in between, stepped with an integer error term so that no rounding drifts.
 if CellX1<>CellX2 then begin

  p:=pvVectorGraphicsOnePixel*((aY2-Y1)+Delta);
  Lift:=p div DeltaX;
  Remainder:=p mod DeltaX;
  if Remainder<0 then begin
   dec(Lift);
   inc(Remainder,DeltaX);
  end;
  dec(Modulus,DeltaX);

  while CellX1<>CellX2 do begin
   Delta:=Lift;
   inc(Modulus,Remainder);
   if Modulus>=0 then begin
    dec(Modulus,DeltaX);
    inc(Delta);
   end;
   inc(fArea,pvVectorGraphicsOnePixel*Delta);
   inc(fCover,Delta);
   inc(Y1,Delta);
   inc(CellX1,Increment);
   SetCell(CellX1,aCellY);
  end;

 end;

 // And the partial cell the edge ends in.
 Delta:=aY2-Y1;
 inc(fArea,((FractionX2+pvVectorGraphicsOnePixel)-First)*Delta);
 inc(fCover,Delta);

end;

procedure TpvVectorGraphicsRasterizerAnalytical.RenderLine(const aToX,aToY:TpvInt32);
var CellY1,CellY2,FractionY1,FractionY2,DeltaX,DeltaY,NextX,X2,p,First,Delta,
    Remainder,Modulus,Lift,Increment,Minimum,Maximum,CellX,TwoFractionX,NextArea:TpvInt32;
begin

 CellY1:=fY div pvVectorGraphicsPixelFactor;
 CellY2:=aToY div pvVectorGraphicsPixelFactor;
 FractionY1:=fY mod pvVectorGraphicsPixelFactor;
 FractionY2:=aToY mod pvVectorGraphicsPixelFactor;
 DeltaX:=aToX-fX;
 DeltaY:=aToY-fY;

 // Vertical clipping, so that an edge far above or below the buffer costs nothing.
 if CellY1<CellY2 then begin
  Minimum:=CellY1;
  Maximum:=CellY2;
 end else begin
  Minimum:=CellY2;
  Maximum:=CellY1;
 end;
 if (Minimum>=Height) or (Maximum<0) then begin
  fX:=aToX;
  fY:=aToY;
  exit;
 end;

 // Everything on a single scanline.
 if CellY1=CellY2 then begin
  RenderScanLine(CellY1,fX,FractionY1,aToX,FractionY2);
  fX:=aToX;
  fY:=aToY;
  exit;
 end;

 Increment:=1;

 // Exactly vertical, which is frequent enough in rectangles and glyph stems that it is worth
 // skipping RenderScanLine for it: every cell gets the same area and cover.
 if DeltaX=0 then begin

  CellX:=fX div pvVectorGraphicsPixelFactor;
  TwoFractionX:=(fX mod pvVectorGraphicsPixelFactor)*2;

  First:=pvVectorGraphicsOnePixel;
  if DeltaY<0 then begin
   First:=0;
   Increment:=-1;
  end;

  // The partial cell at the start.
  Delta:=First-FractionY1;
  inc(fArea,TwoFractionX*Delta);
  inc(fCover,Delta);
  inc(CellY1,Increment);
  SetCell(CellX,CellY1);

  // The whole cells in between, all identical.
  Delta:=(First+First)-pvVectorGraphicsOnePixel;
  NextArea:=TwoFractionX*Delta;
  while CellY1<>CellY2 do begin
   inc(fArea,NextArea);
   inc(fCover,Delta);
   inc(CellY1,Increment);
   SetCell(CellX,CellY1);
  end;

  // And the partial cell at the end.
  Delta:=(FractionY2-pvVectorGraphicsOnePixel)+First;
  inc(fArea,TwoFractionX*Delta);
  inc(fCover,Delta);

  fX:=aToX;
  fY:=aToY;
  exit;

 end;

 // Several scanlines have to be walked, so set up the direction and the first partial step.
 if DeltaY<0 then begin
  p:=FractionY1*DeltaX;
  First:=0;
  Increment:=-1;
  DeltaY:=-DeltaY;
 end else begin
  p:=(pvVectorGraphicsOnePixel-FractionY1)*DeltaX;
  First:=pvVectorGraphicsOnePixel;
  Increment:=1;
 end;

 Delta:=p div DeltaY;
 Modulus:=p mod DeltaY;
 if Modulus<0 then begin
  dec(Delta);
  inc(Modulus,DeltaY);
 end;

 // The partial scanline the edge starts on.
 NextX:=fX+Delta;
 RenderScanLine(CellY1,fX,FractionY1,NextX,First);
 inc(CellY1,Increment);
 SetCell(NextX div pvVectorGraphicsPixelFactor,CellY1);

 // The whole scanlines in between, stepped with an integer error term so nothing drifts.
 if CellY1<>CellY2 then begin

  p:=pvVectorGraphicsOnePixel*DeltaX;
  Lift:=p div DeltaY;
  Remainder:=p mod DeltaY;
  if Remainder<0 then begin
   dec(Lift);
   inc(Remainder,DeltaY);
  end;

  while CellY1<>CellY2 do begin
   Delta:=Lift;
   inc(Modulus,Remainder);
   if Modulus>=0 then begin
    dec(Modulus,DeltaY);
    inc(Delta);
   end;
   X2:=NextX+Delta;
   RenderScanLine(CellY1,NextX,pvVectorGraphicsOnePixel-First,X2,First);
   NextX:=X2;
   inc(CellY1,Increment);
   SetCell(NextX div pvVectorGraphicsPixelFactor,CellY1);
  end;

 end;

 // And the partial scanline the edge ends on.
 RenderScanLine(CellY1,NextX,pvVectorGraphicsOnePixel-First,aToX,FractionY2);

 fX:=aToX;
 fY:=aToY;

end;

procedure TpvVectorGraphicsRasterizerAnalytical.ProcessSpan(const aX,aY,aArea,aCount,aThreadIndex:TpvInt32);
var Coverage:TpvInt32;
begin

 // The accumulated area is twice the covered subpixel area, so shifting it down by
 // 2*PixelBits+1 and up by 8 lands on an eight bit coverage.
 Coverage:=abs(aArea div (1 shl ((pvVectorGraphicsPixelBits*2)+1-8)));

 case fFillRule of
  TpvVectorPathFillRule.EvenOdd:begin
   // Fold the winding back and forth, so that every second turn cancels out again.
   Coverage:=Coverage and 511;
   if Coverage>256 then begin
    Coverage:=512-Coverage;
   end else if Coverage=256 then begin
    Coverage:=255;
   end;
  end;
  else {TpvVectorPathFillRule.NonZero:}begin
   if Coverage>255 then begin
    Coverage:=255;
   end;
  end;
 end;

 if Coverage<>0 then begin
  RenderSpanCoverage(aY,aX,aCount,Coverage,aThreadIndex);
 end;

end;

procedure TpvVectorGraphicsRasterizerAnalytical.ProcessScanLines(const aFromIndex,aToIndex,aThreadIndex:TpvInt32);
var Cover,X,Area,Index:TpvInt32;
    CellIndex:TpvSizeInt;
    Cell:PCell;
begin

 if fCountCells<=0 then begin
  exit;
 end;

 // Each scanline is swept left to right, carrying the running winding from one cell to the next.
 for Index:=Max(fRenderMinimumY,aFromIndex) to Min(fRenderMaximumY,aToIndex) do begin

  X:=0;
  Cover:=0;
  CellIndex:=fScanLines[Index].CellRootIndex;

  while CellIndex<>CellNone do begin

   Cell:=@fCells[CellIndex];

   // The gap before this cell is fully covered by whatever winding is in effect there.
   if (Cell^.x>X) and (Cover<>0) then begin
    ProcessSpan(X,Index,Cover*(pvVectorGraphicsOnePixel*2),Cell^.x-X,aThreadIndex);
   end;

   // And the cell itself, where the accumulated area gives the partial coverage.
   inc(Cover,Cell^.Cover);
   Area:=(Cover*(pvVectorGraphicsOnePixel*2))-Cell^.Area;
   if (Area<>0) and (Cell^.x>=0) then begin
    ProcessSpan(Cell^.x,Index,Area,1,aThreadIndex);
   end;

   X:=Cell^.x+1;
   CellIndex:=Cell^.NextIndex;

  end;

 end;

end;

procedure TpvVectorGraphicsRasterizerAnalytical.ProcessScanLinesParallelForJob(const aJob:PPasMPJob;const aThreadIndex:TPasMPInt32;const aData:pointer;const aFromIndex,aToIndex:TPasMPNativeInt);
begin
 ProcessScanLines(aFromIndex,aToIndex,aThreadIndex);
end;

procedure TpvVectorGraphicsRasterizerAnalytical.MakeScanLineSpansAndRenderThese;
begin

 if (fCountCells>0) and (fRenderMinimumY<=fRenderMaximumY) then begin
  if assigned(fPasMPInstance) and (fPasMPInstance.CountJobWorkerThreads>1) then begin
   // Every worker blends into its own scanlines and its own scratch buffer, so no locking is
   // needed anywhere in here.
   fPasMPInstance.Invoke(fPasMPInstance.ParallelFor(self,fRenderMinimumY,fRenderMaximumY,ProcessScanLinesParallelForJob,1,8,nil,0));
  end else begin
   ProcessScanLines(fRenderMinimumY,fRenderMaximumY,0);
  end;
 end;

end;

procedure TpvVectorGraphicsRasterizerAnalytical.MoveTo(const aX,aY:TpvInt32);
begin

 // Starting a subpath closes whatever was still open.
 Close;
 fNeedToClose:=true;

 StartCell(aX div pvVectorGraphicsPixelFactor,aY div pvVectorGraphicsPixelFactor);

 fX:=aX;
 fY:=aY;
 fMoveToX:=aX;
 fMoveToY:=aY;
 fLastX:=aX;
 fLastY:=aY;

end;

procedure TpvVectorGraphicsRasterizerAnalytical.LineTo(const aX,aY:TpvInt32);
begin

 RenderLine(aX,aY);

 fLastX:=aX;
 fLastY:=aY;

end;

procedure TpvVectorGraphicsRasterizerAnalytical.Close;
begin

 // A filled area needs closed contours, so an open one gets its closing edge here.
 if fNeedToClose then begin

  fNeedToClose:=false;

  if (fMoveToX<>fLastX) or (fMoveToY<>fLastY) then begin
   RenderLine(fMoveToX,fMoveToY);
   fLastX:=fMoveToX;
   fLastY:=fMoveToY;
  end;

  RecordCell;

 end;

end;

procedure TpvVectorGraphicsRasterizerAnalytical.Render;
begin

 Close;
 MakeScanLineSpansAndRenderThese;
 Reset;

end;

end.
