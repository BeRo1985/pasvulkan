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
unit PasVulkan.Font;
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
     PUCU,
     PasMP,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Collections,
     PasVulkan.Framework,
     PasVulkan.VectorPath,
     PasVulkan.SignedDistanceField2D,
     PasVulkan.TrueTypeFont,
     PasVulkan.Sprites;

type EpvFont=class(Exception);

     TpvFontCodePointBitmap=array of TpvUInt32;

     PpvFontCharacterRange=^TpvFontCharacterRange;
     TpvFontCharacterRange=set of AnsiChar;

     PpvFontCodePointRange=^TpvFontCodePointRange;
     TpvFontCodePointRange=record
      public
       FromCodePoint:TpvUInt32;
       ToCodePoint:TpvUInt32;
       constructor Create(const aFromCodePoint,aToCodePoint:TpvUInt32); overload;
       constructor Create(const aFromCodePoint,aToCodePoint:WideChar); overload;
       constructor Create(const aCharacterRange:TpvFontCharacterRange); overload;
     end;

     TpvFontCodePointRanges=array of TpvFontCodePointRange;

     PpvFontGlyphSideBearings=^TpvFontGlyphSideBearings;
     TpvFontGlyphSideBearings=packed record
      case TpvInt32 of
       0:(
        Left:TpvFloat;
        Top:TpvFloat;
        Right:TpvFloat;
        Bottom:TpvFloat;
       );
       1:(
        LeftTop:TpvVector2;
        RightBottom:TpvVector2;
       );
       2:(
        Rect:TpvRect;
       );
     end;

     PpvFontGlyph=^TpvFontGlyph;
     TpvFontGlyph=record
      Advance:TpvVector2;
      Bounds:TpvRect;
      SideBearings:TpvFontGlyphSideBearings;
      Offset:TpvVector2;
      Size:TpvVector2;
      Width:TpvInt32;
      Height:TpvInt32;
      Sprite:TpvSprite;
     end;

     TPpvFontGlyphs=array of PpvFontGlyph;

     TpvFontGlyphs=array of TpvFontGlyph;

     PpvFontCodePointGlyphPair=^TpvFontCodePointGlyphPair;
     TpvFontCodePointGlyphPair=record
      CodePoint:TpvUInt32;
      Glyph:TpvInt32;
     end;

     TpvFontCodePointGlyphPairs=array of TpvFontCodePointGlyphPair;

     PpvFontKerningPair=^TpvFontKerningPair;
     TpvFontKerningPair=record
      Left:TpvUInt32;
      Right:TpvUInt32;
      Horizontal:TpvInt32;
      Vertical:TpvInt32;
     end;

     TpvFontKerningPairs=array of TpvFontKerningPair;

     TpvFontKerningPairVectors=array of TpvVector2;

     PpvFontSignedDistanceFieldJob=^TpvFontSignedDistanceFieldJob;
     TpvFontSignedDistanceFieldJob=record
      DistanceField:PpvSignedDistanceField2D;
      OffsetX:TpvDouble;
      OffsetY:TpvDouble;
      MultiChannel:boolean;
      PolygonBuffer:TpvTrueTypeFontPolygonBuffer;
     end;

     TpvFontSignedDistanceFieldJobs=array of TpvFontSignedDistanceFieldJob;

     TpvFontInt64HashMap=class(TpvHashMap<TpvInt64,TpvInt64>);

     TpvFont=class
      private
       fDevice:TpvVulkanDevice;
       fSpriteAtlas:TpvSpriteAtlas;
       fTargetPPI:TpvInt32;
       fUnitsPerEm:TpvInt32;
       fBaseScaleFactor:TpvFloat;
       fInverseBaseScaleFactor:TpvFloat;
       fBaseSize:TpvFloat;
       fInverseBaseSize:TpvFloat;
       fMinX:TpvFloat;
       fMinY:TpvFloat;
       fMaxX:TpvFloat;
       fMaxY:TpvFloat;
       fMinimumCodePoint:TpvUInt32;
       fMaximumCodePoint:TpvUInt32;
       fCodePointBitmap:TpvFontCodePointBitmap;
       fGlyphs:TpvFontGlyphs;
       fCodePointGlyphPairs:TpvFontCodePointGlyphPairs;
       fKerningPairs:TpvFontKerningPairs;
       fKerningPairVectors:TpvFontKerningPairVectors;
       fCodePointToGlyphHashMap:TpvFontInt64HashMap;
       fKerningPairHashMap:TpvFontInt64HashMap;
       fSignedDistanceFieldJobs:TpvFontSignedDistanceFieldJobs;
       procedure GenerateSignedDistanceField(var aSignedDistanceField:TpvSignedDistanceField2D;const aOffsetX,aOffsetY:TpvDouble;const aMultiChannel:boolean;const aPolygonBuffer:TpvTrueTypeFontPolygonBuffer;const aFillRule:TpvInt32);
       procedure GenerateSignedDistanceFieldParallelForJobFunction(const Job:PPasMPJob;const ThreadIndex:TPasMPInt32;const Data:TVkPointer;const FromIndex,ToIndex:TPasMPNativeInt);
      public
       constructor Create(const aDevice:TpvVulkanDevice;const aSpriteAtlas:TpvSpriteAtlas;const aTargetPPI:TpvInt32=72;const aBaseSize:TpvFloat=12.0); reintroduce;
       constructor CreateFromTrueTypeFont(const aDevice:TpvVulkanDevice;const aSpriteAtlas:TpvSpriteAtlas;const aTrueTypeFont:TpvTrueTypeFont;const aCodePointRanges:array of TpvFontCodePointRange;const aPadding:TpvInt32=2);
       destructor Destroy; override;
       procedure LoadFromStream(const aStream:TStream);
       procedure LoadFromFile(const aFileName:string);
       procedure SaveToStream(const aStream:TStream);
       procedure SaveToFile(const aFileName:string);
       function GetScaleFactor(const aSize:TpvFloat):TpvFloat;
       function TextWidth(const aText:TpvUTF8String;const aSize:TpvFloat):TpvFloat;
       function TextHeight(const aText:TpvUTF8String;const aSize:TpvFloat):TpvFloat;
       function TextSize(const aText:TpvUTF8String;const aSize:TpvFloat):TpvVector2;
       function RowHeight(const aPercent:TpvFloat;const aSize:TpvFloat):TpvFloat;
       procedure GetTextGlyphRects(const aText:TpvUTF8String;const aPosition:TpvVector2;const aSize:TpvFloat;var aRects:TpvRectArray;out aCountRects:TpvInt32);
       procedure Draw(const aCanvas:TObject;const aText:TpvUTF8String;const aPosition:TpvVector2;const aSize:TpvFloat);
      published
       property BaseSize:TpvFloat read fBaseSize;
     end;

implementation

uses PasDblStrUtils,
     PasVulkan.XML,
     PasVulkan.Utils,
     PasVulkan.Canvas;

constructor TpvFontCodePointRange.Create(const aFromCodePoint,aToCodePoint:TpvUInt32);
begin
 FromCodePoint:=Min(aFromCodePoint,aToCodePoint);
 ToCodePoint:=Max(aFromCodePoint,aToCodePoint);
end;

constructor TpvFontCodePointRange.Create(const aFromCodePoint,aToCodePoint:WideChar);
begin
 FromCodePoint:=Min(TpvUInt16(WideChar(aFromCodePoint)),TpvUInt16(WideChar(aToCodePoint)));
 ToCodePoint:=Max(TpvUInt16(WideChar(aFromCodePoint)),TpvUInt16(WideChar(aToCodePoint)));
end;

constructor TpvFontCodePointRange.Create(const aCharacterRange:TpvFontCharacterRange);
var Index:AnsiChar;
begin
 FromCodePoint:=High(TpvUInt32);
 ToCodePoint:=Low(TpvUInt32);
 for Index:=Low(AnsiChar) to High(AnsiChar) do begin
  if Index in aCharacterRange then begin
   FromCodePoint:=TpvUInt8(AnsiChar(Index));
   break;
  end;
 end;
 for Index:=High(AnsiChar) downto Low(AnsiChar) do begin
  if Index in aCharacterRange then begin
   ToCodePoint:=TpvUInt8(AnsiChar(Index));
   break;
  end;
 end;
end;

function CompareVulkanFontGlyphsByArea(const a,b:TpvPointer):TpvInt32;
begin
 result:=(PpvFontGlyph(b)^.Width*PpvFontGlyph(b)^.Height)-(PpvFontGlyph(a)^.Width*PpvFontGlyph(a)^.Height);
end;

function CompareVulkanFontKerningPairs(const a,b:TpvPointer):TpvInt32;
begin
 result:=TpvInt64(PpvFontKerningPair(a)^.Left)-TpvInt64(PpvFontKerningPair(b)^.Left);
 if result=0 then begin
  result:=TpvInt64(PpvFontKerningPair(a)^.Right)-TpvInt64(PpvFontKerningPair(b)^.Right);
 end;
end;

constructor TpvFont.Create(const aDevice:TpvVulkanDevice;const aSpriteAtlas:TpvSpriteAtlas;const aTargetPPI:TpvInt32=72;const aBaseSize:TpvFloat=12.0);
begin

 inherited Create;

 fDevice:=aDevice;

 fSpriteAtlas:=aSpriteAtlas;

 fTargetPPI:=aTargetPPI;

 fUnitsPerEm:=72;

 fBaseScaleFactor:=1.0;

 fInverseBaseScaleFactor:=1.0;

 fBaseSize:=aBaseSize;

 fInverseBaseSize:=1.0/fBaseSize;

 fMinX:=0.0;
 fMinY:=0.0;
 fMaxX:=0.0;
 fMaxY:=0.0;

 fMinimumCodePoint:=High(TpvUInt32);
 fMaximumCodePoint:=Low(TpvUInt32);

 fCodePointBitmap:=nil;

 fGlyphs:=nil;

 fCodePointGlyphPairs:=nil;

 fKerningPairs:=nil;

 fKerningPairVectors:=nil;

 fCodePointToGlyphHashMap:=TpvFontInt64HashMap.Create(-1);

 fKerningPairHashMap:=TpvFontInt64HashMap.Create(-1);

 fSignedDistanceFieldJobs:=nil;

end;

constructor TpvFont.CreateFromTrueTypeFont(const aDevice:TpvVulkanDevice;const aSpriteAtlas:TpvSpriteAtlas;const aTrueTypeFont:TpvTrueTypeFont;const aCodePointRanges:array of TpvFontCodePointRange;const aPadding:TpvInt32=2);
const GlyphMetaDataScaleFactor=1.0;
      GlyphRasterizationScaleFactor=1.0/256.0;
var Index,TTFGlyphIndex,GlyphIndex,OtherGlyphIndex,CountGlyphs,
    CodePointGlyphPairIndex,CountCodePointGlyphPairs,
    TrueTypeFontKerningIndex,TrueTypeFontKerningPairIndex,
    KerningPairIndex,CountKerningPairs:TpvInt32;
    x0,y0,x1,y1:TpvDouble;
    KerningPairDoubleIndex:TpvUInt64;
    Int64Value:TpvInt64;
    CodePointRange:PpvFontCodePointRange;
    CodePointIndex,BitmapCodePointIndex:TpvUInt32;
    CodePointBitmap:TpvFontCodePointBitmap;
    CodePointToTTFGlyphHashMap:TpvFontInt64HashMap;
    TTFGlyphToGlyphHashMap:TpvFontInt64HashMap;
    GlyphToTTFGlyphHashMap:TpvFontInt64HashMap;
    KerningPairHashMap:TpvFontInt64HashMap;
    Glyph:PpvFontGlyph;
    GlyphBuffer:TpvTrueTypeFontGlyphBuffer;
    PolygonBuffers:TpvTrueTypeFontPolygonBuffers;
    SortedGlyphs:TPpvFontGlyphs;
    DistanceField:TpvSignedDistanceField2D;
    TrueTypeFontKerningTable:PpvTrueTypeFontKerningTable;
    TrueTypeFontKerningPair:PpvTrueTypeFontKerningPair;
    CodePointGlyphPair:PpvFontCodePointGlyphPair;
    KerningPair:PpvFontKerningPair;
    GlyphDistanceField:PpvSignedDistanceField2D;
    GlyphDistanceFields:TpvSignedDistanceField2DArray;
    PasMPInstance:TPasMP;
    GlyphDistanceFieldJob:PpvFontSignedDistanceFieldJob;
    UniqueID:string;
    GUID:TGUID;
begin

 Create(aDevice,aSpriteAtlas,aTrueTypeFont.TargetPPI,aTrueTypeFont.Size);

 PasMPInstance:=TPasMP.GetGlobalInstance;

 fUnitsPerEm:=aTrueTypeFont.GetUnitsPerEm;

 fBaseScaleFactor:=aTrueTypeFont.GetScaleFactor;

 fInverseBaseScaleFactor:=1.0/fBaseScaleFactor;

 fMinX:=aTrueTypeFont.MinX;
 fMinY:=aTrueTypeFont.MinY;
 fMaxX:=aTrueTypeFont.MaxX;
 fMaxY:=aTrueTypeFont.MaxY;

 for Index:=low(aCodePointRanges) to high(aCodePointRanges) do begin
  CodePointRange:=@aCodePointRanges[Index];
  fMinimumCodePoint:=Min(fMinimumCodePoint,Min(CodePointRange^.FromCodePoint,CodePointRange^.ToCodePoint));
  fMaximumCodePoint:=Max(fMaximumCodePoint,Max(CodePointRange^.FromCodePoint,CodePointRange^.ToCodePoint));
 end;

 if fMinimumCodePoint<=fMaximumCodePoint then begin

  SetLength(CodePointBitmap,((fMaximumCodePoint-fMinimumCodePoint)+32) shr 5);

  FillChar(CodePointBitmap[0],length(CodePointBitmap)*SizeOf(TpvUInt32),#0);

  for Index:=low(aCodePointRanges) to high(aCodePointRanges) do begin
   CodePointRange:=@aCodePointRanges[Index];
   for CodePointIndex:=Min(CodePointRange^.FromCodePoint,CodePointRange^.ToCodePoint) to Max(CodePointRange^.FromCodePoint,CodePointRange^.ToCodePoint) do begin
    BitmapCodePointIndex:=CodePointIndex-fMinimumCodePoint;
    CodePointBitmap[BitmapCodePointIndex shr 5]:=CodePointBitmap[BitmapCodePointIndex shr 5] or (TpvUInt32(1) shl (BitmapCodePointIndex and 31));
   end;
  end;

  TTFGlyphToGlyphHashMap:=TpvFontInt64HashMap.Create(-1);
  try

   GlyphToTTFGlyphHashMap:=TpvFontInt64HashMap.Create(-1);
   try

    // Collect used glyphs
    CodePointToTTFGlyphHashMap:=TpvFontInt64HashMap.Create(-1);
    try
     CountGlyphs:=0;
     CountCodePointGlyphPairs:=0;
     try
      for CodePointIndex:=fMinimumCodePoint to fMaximumCodePoint do begin
       BitmapCodePointIndex:=CodePointIndex-fMinimumCodePoint;
       if (CodePointBitmap[BitmapCodePointIndex shr 5] and (TpvUInt32(1) shl (BitmapCodePointIndex and 31)))<>0 then begin
        TTFGlyphIndex:=aTrueTypeFont.GetGlyphIndex(CodePointIndex);
        if (TTFGlyphIndex>=0) and (TTFGlyphIndex<aTrueTypeFont.CountGlyphs) then begin
         if not CodePointToTTFGlyphHashMap.ExistKey(CodePointIndex) then begin
          CodePointToTTFGlyphHashMap.Add(CodePointIndex,TTFGlyphIndex);
          if TTFGlyphToGlyphHashMap.TryGet(TTFGlyphIndex,Int64Value) then begin
           GlyphIndex:=Int64Value;
          end else begin
           GlyphIndex:=CountGlyphs;
           inc(CountGlyphs);
           TTFGlyphToGlyphHashMap.Add(TTFGlyphIndex,GlyphIndex);
           GlyphToTTFGlyphHashMap.Add(GlyphIndex,TTFGlyphIndex);
          end;
          fCodePointToGlyphHashMap.Add(CodePointIndex,GlyphIndex);
          CodePointGlyphPairIndex:=CountCodePointGlyphPairs;
          inc(CountCodePointGlyphPairs);
          if length(fCodePointGlyphPairs)<CountCodePointGlyphPairs then begin
           SetLength(fCodePointGlyphPairs,CountCodePointGlyphPairs*2);
          end;
          CodePointGlyphPair:=@fCodePointGlyphPairs[CodePointGlyphPairIndex];
          CodePointGlyphPair^.CodePoint:=CodePointIndex;
          CodePointGlyphPair^.Glyph:=GlyphIndex;
         end;
        end;
       end;
      end;
     finally
      SetLength(fGlyphs,CountGlyphs);
      SetLength(fCodePointGlyphPairs,CountCodePointGlyphPairs);
     end;
    finally
     CodePointToTTFGlyphHashMap.Free;
    end;

    // Convert glyph data and get polygon data
    PolygonBuffers:=nil;
    try

     SetLength(PolygonBuffers,CountGlyphs);

     for GlyphIndex:=0 to CountGlyphs-1 do begin

      Glyph:=@fGlyphs[GlyphIndex];

      FillChar(Glyph^,SizeOf(TpvFontGlyph),#0);

      if GlyphToTTFGlyphHashMap.TryGet(GlyphIndex,Int64Value) then begin

       TTFGlyphIndex:=Int64Value;

       Glyph^.Advance.x:=aTrueTypeFont.GetGlyphAdvanceWidth(TTFGlyphIndex)*GlyphMetaDataScaleFactor;
       Glyph^.Advance.y:=aTrueTypeFont.GetGlyphAdvanceHeight(TTFGlyphIndex)*GlyphMetaDataScaleFactor;
       Glyph^.SideBearings.Left:=aTrueTypeFont.GetGlyphLeftSideBearing(TTFGlyphIndex)*GlyphMetaDataScaleFactor;
       Glyph^.SideBearings.Top:=aTrueTypeFont.GetGlyphTopSideBearing(TTFGlyphIndex)*GlyphMetaDataScaleFactor;
       Glyph^.SideBearings.Right:=aTrueTypeFont.GetGlyphRightSideBearing(TTFGlyphIndex)*GlyphMetaDataScaleFactor;
       Glyph^.SideBearings.Bottom:=aTrueTypeFont.GetGlyphBottomSideBearing(TTFGlyphIndex)*GlyphMetaDataScaleFactor;
       Glyph^.Bounds.Left:=aTrueTypeFont.Glyphs[TTFGlyphIndex].Bounds.XMin*GlyphMetaDataScaleFactor;
       Glyph^.Bounds.Top:=aTrueTypeFont.Glyphs[TTFGlyphIndex].Bounds.YMin*GlyphMetaDataScaleFactor;
       Glyph^.Bounds.Right:=aTrueTypeFont.Glyphs[TTFGlyphIndex].Bounds.XMax*GlyphMetaDataScaleFactor;
       Glyph^.Bounds.Bottom:=aTrueTypeFont.Glyphs[TTFGlyphIndex].Bounds.YMax*GlyphMetaDataScaleFactor;

       GlyphBuffer.Points:=nil;
       PolygonBuffers[GlyphIndex].Commands:=nil;
       try

        if aTrueTypeFont.IsPostScriptGlyph(TTFGlyphIndex) then begin

         aTrueTypeFont.ResetPolygonBuffer(PolygonBuffers[GlyphIndex]);
         aTrueTypeFont.FillPostScriptPolygonBuffer(PolygonBuffers[GlyphIndex],TTFGlyphIndex);

        end else begin

         aTrueTypeFont.ResetGlyphBuffer(GlyphBuffer);
         aTrueTypeFont.FillGlyphBuffer(GlyphBuffer,TTFGlyphIndex);

         aTrueTypeFont.ResetPolygonBuffer(PolygonBuffers[GlyphIndex]);
         aTrueTypeFont.FillPolygonBuffer(PolygonBuffers[GlyphIndex],GlyphBuffer);

        end;

        aTrueTypeFont.GetPolygonBufferBounds(PolygonBuffers[GlyphIndex],x0,y0,x1,y1);

        Glyph^.Offset.x:=(x0*GlyphRasterizationScaleFactor)-(VulkanDistanceField2DSpreadValue*2.0);
        Glyph^.Offset.y:=(y0*GlyphRasterizationScaleFactor)-(VulkanDistanceField2DSpreadValue*2.0);

        Glyph^.Width:=Max(1,ceil(((x1-x0)*GlyphRasterizationScaleFactor)+(VulkanDistanceField2DSpreadValue*4.0)));
        Glyph^.Height:=Max(1,ceil(((y1-y0)*GlyphRasterizationScaleFactor)+(VulkanDistanceField2DSpreadValue*4.0)));

        Glyph^.Size:=TpvVector2.Create(Glyph^.Width,Glyph^.Height);

       finally
        GlyphBuffer.Points:=nil;
       end;

      end;
     end;

     GlyphDistanceFields:=nil;
     try

      SetLength(GlyphDistanceFields,CountGlyphs);

      fSignedDistanceFieldJobs:=nil;
      try

       SetLength(fSignedDistanceFieldJobs,CountGlyphs);

       // Rasterize glyph signed distance field sprites
       for GlyphIndex:=0 to CountGlyphs-1 do begin
        Glyph:=@fGlyphs[GlyphIndex];
        GlyphDistanceField:=@GlyphDistanceFields[GlyphIndex];
        GlyphDistanceField^.Width:=Max(1,Glyph^.Width);
        GlyphDistanceField^.Height:=Max(1,Glyph^.Height);
        GlyphDistanceField^.Pixels:=nil;
        SetLength(GlyphDistanceField^.Pixels,GlyphDistanceField^.Width*GlyphDistanceField^.Height);
        GlyphDistanceFieldJob:=@fSignedDistanceFieldJobs[GlyphIndex];
        GlyphDistanceFieldJob^.DistanceField:=GlyphDistanceField;
        GlyphDistanceFieldJob^.OffsetX:=-Glyph^.Offset.x;
        GlyphDistanceFieldJob^.OffsetY:=-Glyph^.Offset.y;
        GlyphDistanceFieldJob^.MultiChannel:=false;
        GlyphDistanceFieldJob^.PolygonBuffer:=PolygonBuffers[GlyphIndex];
       end;

       if CountGlyphs>0 then begin
        PasMPInstance.Invoke(PasMPInstance.ParallelFor(@fSignedDistanceFieldJobs[0],0,CountGlyphs-1,GenerateSignedDistanceFieldParallelForJobFunction,1,10,nil,0));
       end;

      finally
       fSignedDistanceFieldJobs:=nil;
      end;

      // Insert glyph signed distance field sprites by sorted area size order
      SortedGlyphs:=nil;
      try

       SetLength(SortedGlyphs,length(fGlyphs));

       for GlyphIndex:=0 to length(fGlyphs)-1 do begin
        SortedGlyphs[GlyphIndex]:=@fGlyphs[GlyphIndex];
       end;

       if length(SortedGlyphs)>1 then begin
        IndirectIntroSort(@SortedGlyphs[0],0,length(SortedGlyphs)-1,CompareVulkanFontGlyphsByArea);
       end;

       CreateGUID(GUID);

       UniqueID:=StringReplace(String(aTrueTypeFont.FullName),' ','_',[rfReplaceAll])+'_'+
                 StringReplace(Copy(GUIDToString(GUID),2,36),'-','',[rfReplaceAll])+
                 IntToHex(TPvUInt64(TpvPtrUInt(self)),SizeOf(TpvPtrUInt) shl 1);

       for GlyphIndex:=0 to length(SortedGlyphs)-1 do begin
        Glyph:=SortedGlyphs[GlyphIndex];
        if (Glyph^.Width>0) and (Glyph^.Height>0) then begin
         OtherGlyphIndex:={%H-}((TpvPtrUInt(TpvPointer(Glyph))-TpvPtrUInt(TpvPointer(@fGlyphs[0])))) div SizeOf(TpvFontGlyph);
         Glyph^.Sprite:=aSpriteAtlas.LoadRawSprite(TpvRawByteString(String(UniqueID+IntToHex(TPvUInt64(TpvPtrUInt(Glyph)),SizeOf(TpvPtrUInt) shl 1)+'_glyph'+IntToStr(OtherGlyphIndex))),
                                                   @GlyphDistanceFields[OtherGlyphIndex].Pixels[0],
                                                   Glyph^.Width,
                                                   Glyph^.Height,
                                                   false,
                                                   aPadding);
         Glyph^.Sprite.SignedDistanceField:=true;
        end;
       end;

      finally
       SortedGlyphs:=nil;
      end;

     finally
      GlyphDistanceFields:=nil;
     end;

    finally
     PolygonBuffers:=nil;
    end;

   finally
    GlyphToTTFGlyphHashMap.Free;
   end;

   // Convert kerning pair lookup data
   fKerningPairs:=nil;
   fKerningPairVectors:=nil;
   CountKerningPairs:=0;
   try
    KerningPairHashMap:=TpvFontInt64HashMap.Create(-1);
    try
     for TrueTypeFontKerningIndex:=0 to length(aTrueTypeFont.KerningTables)-1 do begin
      TrueTypeFontKerningTable:=@aTrueTypeFont.KerningTables[TrueTypeFontKerningIndex];
      for TrueTypeFontKerningPairIndex:=0 to length(TrueTypeFontKerningTable^.KerningPairs)-1 do begin
       TrueTypeFontKerningPair:=@TrueTypeFontKerningTable^.KerningPairs[TrueTypeFontKerningPairIndex];
       if TTFGlyphToGlyphHashMap.TryGet(TrueTypeFontKerningPair^.Left,Int64Value) then begin
        GlyphIndex:=Int64Value;
        if TTFGlyphToGlyphHashMap.TryGet(TrueTypeFontKerningPair^.Right,Int64Value) then begin
         OtherGlyphIndex:=Int64Value;
         KerningPairDoubleIndex:=CombineTwoUInt32IntoOneUInt64(GlyphIndex,OtherGlyphIndex);
         if not KerningPairHashMap.ExistKey(KerningPairDoubleIndex) then begin
          KerningPairIndex:=CountKerningPairs;
          inc(CountKerningPairs);
          if length(fKerningPairs)<CountKerningPairs then begin
           SetLength(fKerningPairs,CountKerningPairs*2);
          end;
          KerningPairHashMap.Add(KerningPairDoubleIndex,KerningPairIndex);
          KerningPair:=@fKerningPairs[KerningPairIndex];
          KerningPair^.Left:=TrueTypeFontKerningPair^.Left;
          KerningPair^.Right:=TrueTypeFontKerningPair^.Right;
          KerningPair^.Horizontal:=aTrueTypeFont.GetKerning(KerningPair^.Left,KerningPair^.Right,true);
          KerningPair^.Vertical:=aTrueTypeFont.GetKerning(KerningPair^.Left,KerningPair^.Right,false);
         end;
        end;
       end;
      end;
     end;
    finally
     KerningPairHashMap.Free;
    end;
   finally
    SetLength(fKerningPairs,CountKerningPairs);
    if length(fKerningPairs)>1 then begin
     UntypedDirectIntroSort(@fKerningPairs[0],0,length(fKerningPairs)-1,SizeOf(TpvFontKerningPair),CompareVulkanFontKerningPairs);
    end;
    SetLength(fKerningPairVectors,CountKerningPairs);
    for KerningPairIndex:=0 to length(fKerningPairs)-1 do begin
     KerningPair:=@fKerningPairs[KerningPairIndex];
     fKerningPairVectors[KerningPairIndex]:=TpvVector2.Create(KerningPair^.Horizontal,KerningPair^.Vertical);
     KerningPairDoubleIndex:=CombineTwoUInt32IntoOneUInt64(KerningPair^.Left,KerningPair^.Right);
     fKerningPairHashMap.Add(KerningPairDoubleIndex,KerningPairIndex);
    end;
   end;

  finally
   TTFGlyphToGlyphHashMap.Free;
  end;

  fCodePointBitmap:=CodePointBitmap;

 end;

end;

destructor TpvFont.Destroy;
begin

 fCodePointBitmap:=nil;

 fGlyphs:=nil;

 fCodePointGlyphPairs:=nil;

 fKerningPairs:=nil;

 fKerningPairVectors:=nil;

 fCodePointToGlyphHashMap.Free;

 fKerningPairHashMap.Free;

 fSignedDistanceFieldJobs:=nil;

 inherited Destroy;
end;

procedure TpvFont.LoadFromStream(const aStream:TStream);
var CodePointIndex,BitmapCodePointIndex:TpvUInt32;
    GlyphIndex,KerningPairIndex:TpvSizeInt;
    XML:TpvXML;
    XMLRootTag,XMLGlyphTag,XMLCodePointTag,XMLKerningPairTag:TpvXMLTag;
    Glyph:PpvFontGlyph;
    KerningPair:PpvFontKerningPair;
begin

 XML:=TpvXML.Create;
 try

  XML.LoadFromStream(aStream);

  XMLRootTag:=XML.Root.FindTag('font');

  if not assigned(XMLRootTag) then begin
   raise EpvFont.Create('Missing root tag');
  end;

  fTargetPPI:=StrToInt(String(XMLRootTag.GetParameter('targetppi')));
  fUnitsPerEm:=StrToInt(String(XMLRootTag.GetParameter('unitsperem')));
  fBaseScaleFactor:=ConvertStringToDouble(TpvRawByteString(XMLRootTag.GetParameter('basescalefactor')));
  fInverseBaseScaleFactor:=ConvertStringToDouble(TpvRawByteString(XMLRootTag.GetParameter('inversebasescalefactor')));
  fBaseSize:=ConvertStringToDouble(TpvRawByteString(XMLRootTag.GetParameter('basesize')));
  fInverseBaseSize:=ConvertStringToDouble(TpvRawByteString(XMLRootTag.GetParameter('inversebasesize')));
  fMinX:=ConvertStringToDouble(TpvRawByteString(XMLRootTag.GetParameter('minx')));
  fMinY:=ConvertStringToDouble(TpvRawByteString(XMLRootTag.GetParameter('miny')));
  fMaxX:=ConvertStringToDouble(TpvRawByteString(XMLRootTag.GetParameter('maxx')));
  fMaxY:=ConvertStringToDouble(TpvRawByteString(XMLRootTag.GetParameter('maxy')));
  fMinimumCodePoint:=StrToInt(String(XMLRootTag.GetParameter('minimumcodepoint')));
  fMaximumCodePoint:=StrToInt(String(XMLRootTag.GetParameter('maximumcodepoint')));
  SetLength(fGlyphs,StrToInt(String(XMLRootTag.GetParameter('countglyphs'))));
  SetLength(fKerningPairs,StrToInt(String(XMLRootTag.GetParameter('countkerningpairs'))));
  SetLength(fKerningPairVectors,length(fKerningPairs));

  SetLength(fCodePointBitmap,((fMaximumCodePoint-fMinimumCodePoint)+32) shr 5);

  FillChar(fCodePointBitmap[0],length(fCodePointBitmap)*SizeOf(TpvUInt32),#0);

  FillChar(fGlyphs[0],length(fGlyphs)*SizeOf(TpvFontGlyph),#0);

  FillChar(fKerningPairs[0],length(fKerningPairs)*SizeOf(TpvFontKerningPair),#0);

  FillChar(fKerningPairVectors[0],length(fKerningPairVectors)*SizeOf(TpvVector2),#0);

  for XMLGlyphTag in XMLRootTag.FindTags('glyph') do begin
   GlyphIndex:=StrToInt(String(XMLGlyphTag.GetParameter('id')));
   if (GlyphIndex<0) or (GlyphIndex>=length(fGlyphs)) then begin
    raise EpvFont.Create('Glyph index out of range');
   end;
   Glyph:=@fGlyphs[GlyphIndex];
   Glyph^.Advance.x:=ConvertStringToDouble(TpvRawByteString(XMLGlyphTag.GetParameter('advancex')));
   Glyph^.Advance.y:=ConvertStringToDouble(TpvRawByteString(XMLGlyphTag.GetParameter('advancey')));
   Glyph^.Bounds.Left:=ConvertStringToDouble(TpvRawByteString(XMLGlyphTag.GetParameter('boundsleft')));
   Glyph^.Bounds.Top:=ConvertStringToDouble(TpvRawByteString(XMLGlyphTag.GetParameter('boundstop')));
   Glyph^.Bounds.Right:=ConvertStringToDouble(TpvRawByteString(XMLGlyphTag.GetParameter('boundsright')));
   Glyph^.Bounds.Bottom:=ConvertStringToDouble(TpvRawByteString(XMLGlyphTag.GetParameter('boundsbottom')));
   Glyph^.SideBearings.Left:=ConvertStringToDouble(TpvRawByteString(XMLGlyphTag.GetParameter('sidebearingsleft')));
   Glyph^.SideBearings.Top:=ConvertStringToDouble(TpvRawByteString(XMLGlyphTag.GetParameter('sidebearingstop')));
   Glyph^.SideBearings.Right:=ConvertStringToDouble(TpvRawByteString(XMLGlyphTag.GetParameter('sidebearingsright')));
   Glyph^.SideBearings.Bottom:=ConvertStringToDouble(TpvRawByteString(XMLGlyphTag.GetParameter('sidebearingsbottom')));
   Glyph^.Offset.x:=ConvertStringToDouble(TpvRawByteString(XMLGlyphTag.GetParameter('offsetx')));
   Glyph^.Offset.y:=ConvertStringToDouble(TpvRawByteString(XMLGlyphTag.GetParameter('offsety')));
   Glyph^.Size.x:=ConvertStringToDouble(TpvRawByteString(XMLGlyphTag.GetParameter('sizex')));
   Glyph^.Size.y:=ConvertStringToDouble(TpvRawByteString(XMLGlyphTag.GetParameter('sizey')));
   Glyph^.Width:=StrToInt(String(XMLGlyphTag.GetParameter('width')));
   Glyph^.Height:=StrToInt(String(XMLGlyphTag.GetParameter('height')));
   Glyph^.Sprite:=fSpriteAtlas.Sprites[TpvRawByteString(XMLGlyphTag.GetParameter('sprite'))];
  end;

 finally
  XML.Free;
 end;

end;

procedure TpvFont.LoadFromFile(const aFileName:string);
var Stream:TStream;
begin
 Stream:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
 try
  LoadFromStream(Stream);
 finally
  Stream.Free;
 end;
end;

procedure TpvFont.SaveToStream(const aStream:TStream);
var CodePointIndex,BitmapCodePointIndex:TpvUInt32;
    GlyphIndex,KerningPairIndex:TpvSizeInt;
    XML:TpvXML;
    XMLRootTag,XMLGlyphTag,XMLCodePointTag,XMLKerningPairTag:TpvXMLTag;
    Glyph:PpvFontGlyph;
    KerningPair:PpvFontKerningPair;
begin

 XML:=TpvXML.Create;
 try

  XMLRootTag:=TpvXMLTag.Create;
  try

   XMLRootTag.Name:='font';

   XMLRootTag.AddParameter('targetppi',TpvRawByteString(IntToStr(fTargetPPI)));
   XMLRootTag.AddParameter('unitsperem',TpvRawByteString(IntToStr(fUnitsPerEm)));
   XMLRootTag.AddParameter('basescalefactor',TpvRawByteString(ConvertDoubleToString(fBaseScaleFactor)));
   XMLRootTag.AddParameter('inversebasescalefactor',TpvRawByteString(ConvertDoubleToString(fInverseBaseScaleFactor)));
   XMLRootTag.AddParameter('basesize',TpvRawByteString(ConvertDoubleToString(fBaseSize)));
   XMLRootTag.AddParameter('inversebasesize',TpvRawByteString(ConvertDoubleToString(fInverseBaseSize)));
   XMLRootTag.AddParameter('minx',TpvRawByteString(ConvertDoubleToString(fMinX)));
   XMLRootTag.AddParameter('miny',TpvRawByteString(ConvertDoubleToString(fMinY)));
   XMLRootTag.AddParameter('maxx',TpvRawByteString(ConvertDoubleToString(fMaxX)));
   XMLRootTag.AddParameter('maxy',TpvRawByteString(ConvertDoubleToString(fMaxY)));
   XMLRootTag.AddParameter('minimumcodepoint',TpvRawByteString(IntToStr(fMinimumCodePoint)));
   XMLRootTag.AddParameter('maximumcodepoint',TpvRawByteString(IntToStr(fMaximumCodePoint)));
   XMLRootTag.AddParameter('countglyphs',TpvRawByteString(IntToStr(length(fGlyphs))));
   XMLRootTag.AddParameter('countkerningpairs',TpvRawByteString(IntToStr(length(fKerningPairs))));

   for GlyphIndex:=0 to length(fGlyphs)-1 do begin
    Glyph:=@fGlyphs[GlyphIndex];
    XMLGlyphTag:=TpvXMLTag.Create;
    try
     XMLGlyphTag.Name:='glyph';
     XMLGlyphTag.AddParameter('id',TpvRawByteString(IntToStr(GlyphIndex)));
     XMLGlyphTag.AddParameter('advancex',TpvRawByteString(ConvertDoubleToString(Glyph^.Advance.x)));
     XMLGlyphTag.AddParameter('advancey',TpvRawByteString(ConvertDoubleToString(Glyph^.Advance.y)));
     XMLGlyphTag.AddParameter('boundsleft',TpvRawByteString(ConvertDoubleToString(Glyph^.Bounds.Left)));
     XMLGlyphTag.AddParameter('boundstop',TpvRawByteString(ConvertDoubleToString(Glyph^.Bounds.Top)));
     XMLGlyphTag.AddParameter('boundsright',TpvRawByteString(ConvertDoubleToString(Glyph^.Bounds.Right)));
     XMLGlyphTag.AddParameter('boundsbottom',TpvRawByteString(ConvertDoubleToString(Glyph^.Bounds.Bottom)));
     XMLGlyphTag.AddParameter('sidebearingsleft',TpvRawByteString(ConvertDoubleToString(Glyph^.SideBearings.Left)));
     XMLGlyphTag.AddParameter('sidebearingstop',TpvRawByteString(ConvertDoubleToString(Glyph^.SideBearings.Top)));
     XMLGlyphTag.AddParameter('sidebearingsright',TpvRawByteString(ConvertDoubleToString(Glyph^.SideBearings.Right)));
     XMLGlyphTag.AddParameter('sidebearingsbottom',TpvRawByteString(ConvertDoubleToString(Glyph^.SideBearings.Bottom)));
     XMLGlyphTag.AddParameter('offsetx',TpvRawByteString(ConvertDoubleToString(Glyph^.Offset.x)));
     XMLGlyphTag.AddParameter('offsety',TpvRawByteString(ConvertDoubleToString(Glyph^.Offset.y)));
     XMLGlyphTag.AddParameter('sizex',TpvRawByteString(ConvertDoubleToString(Glyph^.Size.x)));
     XMLGlyphTag.AddParameter('sizey',TpvRawByteString(ConvertDoubleToString(Glyph^.Size.y)));
     XMLGlyphTag.AddParameter('width',TpvRawByteString(IntToStr(Glyph^.Width)));
     XMLGlyphTag.AddParameter('height',TpvRawByteString(IntToStr(Glyph^.Height)));
     if assigned(Glyph^.Sprite) then begin
      XMLGlyphTag.AddParameter('sprite',TpvRawByteString(Glyph^.Sprite.Name));
     end else begin
      XMLGlyphTag.AddParameter('sprite','');
     end;
    finally
     XMLRootTag.Add(XMLGlyphTag);
    end;
   end;

   for CodePointIndex:=fMinimumCodePoint to fMaximumCodePoint do begin
    BitmapCodePointIndex:=CodePointIndex-fMinimumCodePoint;
    if (fCodePointBitmap[BitmapCodePointIndex shr 5] and (TpvUInt32(1) shl (BitmapCodePointIndex and 31)))<>0 then begin
     XMLCodePointTag:=TpvXMLTag.Create;
     try
      XMLCodePointTag.Name:='codepoint';
      XMLCodePointTag.AddParameter('id',TpvRawByteString(IntToStr(CodePointIndex)));
      XMLCodePointTag.AddParameter('glyph',TpvRawByteString(IntToStr(fCodePointToGlyphHashMap[CodePointIndex])));
     finally
      XMLRootTag.Add(XMLCodePointTag);
     end;
    end;
   end;

   for KerningPairIndex:=0 to length(fKerningPairs)-1 do begin
    KerningPair:=@fKerningPairs[KerningPairIndex];
    XMLKerningPairTag:=TpvXMLTag.Create;
    try
     XMLKerningPairTag.Name:='kerningpair';
     XMLKerningPairTag.AddParameter('id',TpvRawByteString(IntToStr(KerningPairIndex)));
     XMLKerningPairTag.AddParameter('left',TpvRawByteString(IntToStr(KerningPair^.Left)));
     XMLKerningPairTag.AddParameter('right',TpvRawByteString(IntToStr(KerningPair^.Right)));
     XMLKerningPairTag.AddParameter('horizontal',TpvRawByteString(IntToStr(KerningPair^.Horizontal)));
     XMLKerningPairTag.AddParameter('vertical',TpvRawByteString(IntToStr(KerningPair^.Vertical)));
    finally
     XMLRootTag.Add(XMLKerningPairTag);
    end;
   end;

  finally
   XML.Root.Add(XMLRootTag);
  end;

  XML.SaveToStream(aStream);

 finally
  XML.Free;
 end;


end;

procedure TpvFont.SaveToFile(const aFileName:string);
var Stream:TStream;
begin
 Stream:=TFileStream.Create(aFileName,fmCreate);
 try
  SaveToStream(Stream);
 finally
  Stream.Free;
 end;
end;

function TpvFont.GetScaleFactor(const aSize:TpvFloat):TpvFloat;
begin
 if aSize<0.0 then begin
  result:=(-aSize)/fUnitsPerEm;
 end else begin
  result:=(aSize*fTargetPPI)/(fUnitsPerEm*72);
 end;
end;

procedure TpvFont.GenerateSignedDistanceField(var aSignedDistanceField:TpvSignedDistanceField2D;const aOffsetX,aOffsetY:TpvDouble;const aMultiChannel:boolean;const aPolygonBuffer:TpvTrueTypeFontPolygonBuffer;const aFillRule:TpvInt32);
const Scale=1.0/256.0;
var CommandIndex:TpvInt32;
    Command:PpvTrueTypeFontPolygonCommand;
    VectorPath:TpvVectorPath;
begin
 VectorPath:=TpvVectorPath.Create;
 try
  if aFillRule=pvTTF_PolygonWindingRule_NONZERO then begin
   VectorPath.FillRule:=pvvpfrNonZero;
  end else begin
   VectorPath.FillRule:=pvvpfrEvenOdd;
  end;
  for CommandIndex:=0 to aPolygonBuffer.CountCommands-1 do begin
   Command:=@aPolygonBuffer.Commands[CommandIndex];
   case Command^.CommandType of
    pvTTF_PolygonCommandType_MoveTo:begin
     VectorPath.MoveTo(Command^.Points[0].x,
                       Command^.Points[0].y);
    end;
    pvTTF_PolygonCommandType_LineTo:begin
     VectorPath.LineTo(Command^.Points[0].x,
                       Command^.Points[0].y);
    end;
    pvTTF_PolygonCommandType_QuadraticCurveTo:begin
     VectorPath.QuadraticCurveTo(Command^.Points[0].x,
                                 Command^.Points[0].y,
                                 Command^.Points[1].x,
                                 Command^.Points[1].y);
    end;
    pvTTF_PolygonCommandType_CubicCurveTo:begin
     VectorPath.CubicCurveTo(Command^.Points[0].x,
                             Command^.Points[0].y,
                             Command^.Points[1].x,
                             Command^.Points[1].y,
                             Command^.Points[2].x,
                             Command^.Points[2].y);
    end;
    pvTTF_PolygonCommandType_Close:begin
     VectorPath.Close;
    end;
   end;
  end;
  TpvSignedDistanceField2DGenerator.Generate(aSignedDistanceField,VectorPath,Scale,aOffsetX,aOffsetY);
 finally
  VectorPath.Free;
 end;
end;

procedure TpvFont.GenerateSignedDistanceFieldParallelForJobFunction(const Job:PPasMPJob;const ThreadIndex:TPasMPInt32;const Data:TpvPointer;const FromIndex,ToIndex:TPasMPNativeInt);
var Index:TPasMPNativeInt;
    JobData:PpvFontSignedDistanceFieldJob;
begin
 Index:=FromIndex;
 while Index<=ToIndex do begin
  JobData:=@fSignedDistanceFieldJobs[Index];
  GenerateSignedDistanceField(JobData^.DistanceField^,JobData^.OffsetX,JobData^.OffsetY,JobData^.MultiChannel,JobData^.PolygonBuffer,pvTTF_PolygonWindingRule_NONZERO);
  inc(Index);
 end;
end;

function TpvFont.TextWidth(const aText:TpvUTF8String;const aSize:TpvFloat):TpvFloat;
var TextIndex,CurrentGlyph,LastGlyph:TpvInt32;
    CurrentCodePoint:TpvUInt32;
    Width,NewWidth:TpvFloat;
    Int64Value:TpvInt64;
    Glyph:PpvFontGlyph;
begin
 result:=0.0;
 Width:=0.0;
 TextIndex:=1;
 LastGlyph:=-1;
 while TextIndex<=length(aText) do begin
  CurrentCodePoint:=PUCUUTF8CodeUnitGetCharAndIncFallback(aText,TextIndex);
  if fCodePointToGlyphHashMap.TryGet(CurrentCodePoint,Int64Value) then begin
   CurrentGlyph:=Int64Value;
   if (CurrentGlyph>=0) or (CurrentGlyph<length(fGlyphs)) then begin
    if ((LastGlyph>=0) and (LastGlyph<length(fGlyphs))) and
       fKerningPairHashMap.TryGet(CombineTwoUInt32IntoOneUInt64(LastGlyph,CurrentGlyph),Int64Value) then begin
     result:=result+fKerningPairs[Int64Value].Horizontal;
    end;
    Glyph:=@fGlyphs[CurrentGlyph];
    if LastGlyph<0 then begin
     result:=result+Glyph^.SideBearings.Left;
    end;
    NewWidth:=result+(Glyph^.Bounds.Right-Glyph^.Bounds.Left);
    if Width<NewWidth then begin
     Width:=NewWidth;
    end;
    result:=result+Glyph^.Advance.x;
   end;
  end else begin
   CurrentGlyph:=0;
  end;
  LastGlyph:=CurrentGlyph;
 end;
 if result=0 then begin
  result:=fMaxX-fMinX;
 end;
 if result<Width then begin
  result:=Width;
 end;
 result:=result*GetScaleFactor(aSize);
end;

function TpvFont.TextHeight(const aText:TpvUTF8String;const aSize:TpvFloat):TpvFloat;
var TextIndex,CurrentGlyph,LastGlyph:TpvInt32;
    CurrentCodePoint:TpvUInt32;
    Height,NewHeight:TpvFloat;
    Int64Value:TpvInt64;
    Glyph:PpvFontGlyph;
begin
 result:=0.0;
 Height:=0.0;
 TextIndex:=1;
 LastGlyph:=-1;
 while TextIndex<=length(aText) do begin
  CurrentCodePoint:=PUCUUTF8CodeUnitGetCharAndIncFallback(aText,TextIndex);
  if fCodePointToGlyphHashMap.TryGet(CurrentCodePoint,Int64Value) then begin
   CurrentGlyph:=Int64Value;
   if (CurrentGlyph>=0) or (CurrentGlyph<length(fGlyphs)) then begin
    if ((LastGlyph>=0) and (LastGlyph<length(fGlyphs))) and
       fKerningPairHashMap.TryGet(CombineTwoUInt32IntoOneUInt64(LastGlyph,CurrentGlyph),Int64Value) then begin
     result:=result+fKerningPairs[Int64Value].Vertical;
    end;
    Glyph:=@fGlyphs[CurrentGlyph];
    if LastGlyph<0 then begin
     result:=result+Glyph^.SideBearings.Top;
    end;
    NewHeight:=result+(Glyph^.Bounds.Bottom-Glyph^.Bounds.Top);
    if Height<NewHeight then begin
     Height:=NewHeight;
    end;
    result:=result+Glyph^.Advance.y;
   end;
  end else begin
   CurrentGlyph:=0;
  end;
  LastGlyph:=CurrentGlyph;
 end;
 if result=0 then begin
  result:=fMaxY-fMinY;
 end;
 if result<Height then begin
  result:=Height;
 end;
 result:=result*GetScaleFactor(aSize);
end;

function TpvFont.TextSize(const aText:TpvUTF8String;const aSize:TpvFloat):TpvVector2;
var TextIndex,CurrentGlyph,LastGlyph:TpvInt32;
    CurrentCodePoint:TpvUInt32;
    Size:TpvVector2;
    Int64Value:TpvInt64;
    Glyph:PpvFontGlyph;
begin
 result:=TpvVector2.Null;
 Size:=TpvVector2.Null;
 TextIndex:=1;
 LastGlyph:=-1;
 while TextIndex<=length(aText) do begin
  CurrentCodePoint:=PUCUUTF8CodeUnitGetCharAndIncFallback(aText,TextIndex);
  if fCodePointToGlyphHashMap.TryGet(CurrentCodePoint,Int64Value) then begin
   CurrentGlyph:=Int64Value;
   if (CurrentGlyph>=0) or (CurrentGlyph<length(fGlyphs)) then begin
    if ((LastGlyph>=0) and (LastGlyph<length(fGlyphs))) and
       fKerningPairHashMap.TryGet(CombineTwoUInt32IntoOneUInt64(LastGlyph,CurrentGlyph),Int64Value) then begin
     result:=result+fKerningPairVectors[Int64Value];
    end;
    Glyph:=@fGlyphs[CurrentGlyph];
    if LastGlyph<0 then begin
     result:=result+Glyph^.SideBearings.LeftTop;
    end;
    Size:=Maximum(Size,result+(Glyph^.Bounds.RightBottom-Glyph^.Bounds.LeftTop));
    result:=result+Glyph^.Advance;
   end;
  end else begin
   CurrentGlyph:=0;
  end;
  LastGlyph:=CurrentGlyph;
 end;
 if result.x=0 then begin
  result.x:=fMaxX-fMinX;
 end;
 if result.y=0 then begin
  result.y:=fMaxY-fMinY;
 end;
 result:=Maximum(result,Size)*GetScaleFactor(aSize);
end;

function TpvFont.RowHeight(const aPercent:TpvFloat;const aSize:TpvFloat):TpvFloat;
begin
 result:=fUnitsPerEm*(aPercent*0.01);
 if not IsZero(aSize) then begin
  result:=result*GetScaleFactor(aSize);
 end;
end;

procedure TpvFont.GetTextGlyphRects(const aText:TpvUTF8String;const aPosition:TpvVector2;const aSize:TpvFloat;var aRects:TpvRectArray;out aCountRects:TpvInt32);
var TextIndex,CurrentCodePoint,CurrentGlyph,LastGlyph:TpvInt32;
    ScaleFactor,RescaleFactor:TpvFloat;
    Int64Value:TpvInt64;
    Glyph:PpvFontGlyph;
    Position:TpvVector2;
begin
 aCountRects:=0;
 Position:=TpvVector2.Null;
 ScaleFactor:=GetScaleFactor(aSize);
 RescaleFactor:=ScaleFactor*fInverseBaseScaleFactor;
 TextIndex:=1;
 LastGlyph:=-1;
 while TextIndex<=length(aText) do begin
  CurrentCodePoint:=PUCUUTF8CodeUnitGetCharAndIncFallback(aText,TextIndex);
  if fCodePointToGlyphHashMap.TryGet(CurrentCodePoint,Int64Value) then begin
   CurrentGlyph:=Int64Value;
   if (CurrentGlyph>=0) or (CurrentGlyph<length(fGlyphs)) then begin
    if ((LastGlyph>=0) and (LastGlyph<length(fGlyphs))) and
       fKerningPairHashMap.TryGet(CombineTwoUInt32IntoOneUInt64(LastGlyph,CurrentGlyph),Int64Value) then begin
     Position:=Position+fKerningPairVectors[Int64Value];
    end;
    Glyph:=@fGlyphs[CurrentGlyph];
    if LastGlyph<0 then begin
     Position:=Position+Glyph^.SideBearings.LeftTop;
    end;
    if length(aRects)<=aCountRects then begin
     SetLength(aRects,(aCountRects+1)*2);
    end;
    aRects[aCountRects]:=TpvRect.CreateRelative(aPosition+(Position*ScaleFactor)+(Glyph^.Offset*RescaleFactor),
                                                Glyph^.Size*RescaleFactor);
    inc(aCountRects);
    Position:=Position+Glyph^.Advance;
   end;
  end else begin
   CurrentGlyph:=0;
  end;
  LastGlyph:=CurrentGlyph;
 end;
end;

procedure TpvFont.Draw(const aCanvas:TObject;const aText:TpvUTF8String;const aPosition:TpvVector2;const aSize:TpvFloat);
var TextIndex,CurrentCodePoint,CurrentGlyph,LastGlyph:TpvInt32;
    ScaleFactor,RescaleFactor:TpvFloat;
    Int64Value:TpvInt64;
    Glyph:PpvFontGlyph;
    Position:TpvVector2;
begin
 Position:=TpvVector2.Null;
 ScaleFactor:=GetScaleFactor(aSize);
 RescaleFactor:=ScaleFactor*fInverseBaseScaleFactor;
 TextIndex:=1;
 LastGlyph:=-1;
 while TextIndex<=length(aText) do begin
  CurrentCodePoint:=PUCUUTF8CodeUnitGetCharAndIncFallback(aText,TextIndex);
  if fCodePointToGlyphHashMap.TryGet(CurrentCodePoint,Int64Value) then begin
   CurrentGlyph:=Int64Value;
   if (CurrentGlyph>=0) or (CurrentGlyph<length(fGlyphs)) then begin
    if ((LastGlyph>=0) and (LastGlyph<length(fGlyphs))) and
       fKerningPairHashMap.TryGet(CombineTwoUInt32IntoOneUInt64(LastGlyph,CurrentGlyph),Int64Value) then begin
     Position:=Position+fKerningPairVectors[Int64Value];
    end;
    Glyph:=@fGlyphs[CurrentGlyph];
    if LastGlyph<0 then begin
     Position:=Position+Glyph^.SideBearings.LeftTop;
    end;
    TpvCanvas(aCanvas).DrawSprite(Glyph^.Sprite,
                                  TpvRect.CreateRelative(TpvVector2.Null,
                                                         Glyph^.Size),
                                  TpvRect.CreateRelative(aPosition+(Position*ScaleFactor)+(Glyph^.Offset*RescaleFactor),
                                                         Glyph^.Size*RescaleFactor));
    Position:=Position+Glyph^.Advance;
   end;
  end else begin
   CurrentGlyph:=0;
  end;
  LastGlyph:=CurrentGlyph;
 end;
end;

end.
