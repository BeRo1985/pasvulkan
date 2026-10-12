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
unit PasVulkan.VectorGraphics.Berosh;
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
     PasDblStrUtils,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Collections,
     PasVulkan.XML,
     PasVulkan.VectorPath,
     PasVulkan.VectorGraphics.Rasterizer,
     PasVulkan.VectorGraphics.Paints,
     PasVulkan.VectorGraphics;

// Reader and writer for the XML document format of the Berosh vector editor.
//
// Unlike SVG, this format stores the scene graph as it stands rather than a drawing of it: the
// transform of an object is kept in its parts, the timeline comes along, fills remember whether
// they are a gradient or a pattern, and shapes keep their parameters instead of being flattened
// into paths. Round tripping a document through here therefore loses nothing.
//
// Three things in the original writer do not agree with its own reader, and are handled here by
// reading both spellings while writing only the correct one, so that old files keep working:
//
//  - Timeline items were written as siblings of the timeline element instead of inside it.
//  - Two stroke pattern tags were written as "stroke" and read back as "storke".
//  - A morph item wrote its shape commands under the index of the enclosing loop rather than
//    the timeline entry, so they could land on the wrong item.

type EpvVectorGraphicsBerosh=class(Exception);

     { TpvVectorGraphicsBeroshReader }

     TpvVectorGraphicsBeroshReader=class
      private
       fDocument:TpvVectorGraphicsDocument;

       // Group instances and pattern fills reference clips by numeric id, which can only be
       // resolved once every clip has been read.
       fPendingInstances:TpvObjectGenericList<TpvVectorGraphicsObject>;
       fPendingInstanceIDs:TpvGenericList<TpvInt32>;
       fPendingPatternGraphics:TpvObjectGenericList<TpvVectorGraphicsObject>;
       fPendingPatternIDs:TpvGenericList<TpvInt32>;
       fPendingPatternIsStroke:TpvGenericList<boolean>;
       fClipsByID:TpvGenericList<TpvInt32>;
       fClips:TpvObjectGenericList<TpvVectorGraphicsGroup>;

       function ParseFloat(const aText:TpvRawByteString;const aDefault:TpvDouble=0.0):TpvDouble;
       function ParseInteger(const aText:TpvRawByteString;const aDefault:TpvInt64=0):TpvInt64;
       function ParseBoolean(const aText:TpvRawByteString;const aDefault:boolean=false):boolean;
       function ParseColor(const aText:TpvRawByteString):TpvVectorGraphicsColor;
       function ParseFillRule(const aText:TpvRawByteString):TpvVectorPathFillRule;
       function ParseSpreadMethod(const aText:TpvRawByteString):TpvVectorGraphicsSpreadMethod;

       procedure ReadSimpleMatrix(const aTag:TpvXMLTag;out aSimpleMatrix:TpvVectorGraphicsSimpleMatrix);
       procedure ReadPathCommands(const aTag:TpvXMLTag;const aPath:TpvVectorPath);
       procedure ReadFillData(const aTag:TpvXMLTag;var aFillData:TpvVectorGraphicsFillData;
                              const aOwner:TpvVectorGraphicsObject;const aIsStroke:boolean);
       procedure ReadTimeLine(const aTag:TpvXMLTag;const aObject:TpvVectorGraphicsObject);
       procedure ReadObjectBase(const aTag:TpvXMLTag;const aObject:TpvVectorGraphicsObject);
       procedure ReadGraphic(const aTag:TpvXMLTag;const aGraphic:TpvVectorGraphicsGraphic);
       procedure ReadGroup(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup);
       function ReadObject(const aTag:TpvXMLTag;const aParent:TpvVectorGraphicsGroup):TpvVectorGraphicsObject;
       procedure ReadImage(const aTag:TpvXMLTag);

       procedure ResolveReferences;
      public
       constructor Create; reintroduce;
       destructor Destroy; override;

       // Fills aDocument from the parsed XML tree. A fresh document should be handed in.
       function Load(const aXML:TpvXML;const aDocument:TpvVectorGraphicsDocument):boolean;
     end;

     { TpvVectorGraphicsBeroshWriter }

     TpvVectorGraphicsBeroshWriter=class
      private
       fDocument:TpvVectorGraphicsDocument;

       // Clips referenced by group instances and pattern fills get a numeric id on the way out.
       fClips:TpvObjectGenericList<TpvVectorGraphicsGroup>;
       fClipIDs:TpvGenericList<TpvInt32>;
       fNextClipID:TpvInt32;

       function FormatFloat_(const aValue:TpvDouble):TpvRawByteString;
       function FormatColor(const aColor:TpvVectorGraphicsColor):TpvRawByteString;
       function FormatBoolean(const aValue:boolean):TpvRawByteString;

       function ClipIDOf(const aGroup:TpvVectorGraphicsGroup):TpvInt32;
       procedure CollectClips(const aObject:TpvVectorGraphicsObject);

       procedure WriteSimpleMatrix(const aTag:TpvXMLTag;const aSimpleMatrix:TpvVectorGraphicsSimpleMatrix);
       procedure WritePathCommands(const aTag:TpvXMLTag;const aPath:TpvVectorPath);
       procedure WriteFillData(const aTag:TpvXMLTag;const aFillData:TpvVectorGraphicsFillData);
       procedure WriteTimeLine(const aTag:TpvXMLTag;const aObject:TpvVectorGraphicsObject);
       procedure WriteObjectBase(const aTag:TpvXMLTag;const aObject:TpvVectorGraphicsObject);
       procedure WriteGraphic(const aTag:TpvXMLTag;const aGraphic:TpvVectorGraphicsGraphic);
       procedure WriteGroup(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup);
       function WriteObject(const aObject:TpvVectorGraphicsObject;const aParentTag:TpvXMLTag):boolean;
       procedure WriteImage(const aTag:TpvXMLTag;const aImage:TpvVectorGraphicsImageResource);
      public
       constructor Create; reintroduce;
       destructor Destroy; override;

       // Builds the XML tree for aDocument into aXML, which the caller then writes out.
       function Save(const aDocument:TpvVectorGraphicsDocument;const aXML:TpvXML):boolean;
     end;

// Convenience entry points. Loading returns a freshly created document that the caller owns, or
// nil when the input could not be parsed.
function pvVectorGraphicsLoadBeroshFromStream(const aStream:TStream):TpvVectorGraphicsDocument;

function pvVectorGraphicsLoadBeroshFromFile(const aFileName:string):TpvVectorGraphicsDocument;

function pvVectorGraphicsSaveBeroshToStream(const aDocument:TpvVectorGraphicsDocument;const aStream:TStream):boolean;

function pvVectorGraphicsSaveBeroshToFile(const aDocument:TpvVectorGraphicsDocument;const aFileName:string):boolean;

implementation

var BeroshFormatSettings:TFormatSettings;

const HexDigits:array[0..15] of AnsiChar='0123456789abcdef';

function ByteToHex(const aValue:TpvUInt8):TpvRawByteString;
begin
 SetLength(result,2);
 result[1]:=HexDigits[(aValue shr 4) and $f];
 result[2]:=HexDigits[aValue and $f];
end;

function HexDigitValue(const aCharacter:AnsiChar):TpvInt32;
begin

 // Minus one for anything that is not a hexadecimal digit, which is how the callers detect the
 // end of a run of them.
 case aCharacter of
  '0'..'9':begin
   result:=Ord(aCharacter)-Ord('0');
  end;
  'a'..'f':begin
   result:=(Ord(aCharacter)-Ord('a'))+10;
  end;
  'A'..'F':begin
   result:=(Ord(aCharacter)-Ord('A'))+10;
  end;
  else begin
   result:=-1;
  end;
 end;

end;

{ TpvVectorGraphicsBeroshReader }

constructor TpvVectorGraphicsBeroshReader.Create;
begin

 inherited Create;

 fDocument:=nil;

 fPendingInstances:=TpvObjectGenericList<TpvVectorGraphicsObject>.Create;
 fPendingInstances.OwnsObjects:=false;
 fPendingInstanceIDs:=TpvGenericList<TpvInt32>.Create;

 fPendingPatternGraphics:=TpvObjectGenericList<TpvVectorGraphicsObject>.Create;
 fPendingPatternGraphics.OwnsObjects:=false;
 fPendingPatternIDs:=TpvGenericList<TpvInt32>.Create;
 fPendingPatternIsStroke:=TpvGenericList<boolean>.Create;

 fClipsByID:=TpvGenericList<TpvInt32>.Create;
 fClips:=TpvObjectGenericList<TpvVectorGraphicsGroup>.Create;
 fClips.OwnsObjects:=false;

end;

destructor TpvVectorGraphicsBeroshReader.Destroy;
begin

 FreeAndNil(fPendingInstances);
 FreeAndNil(fPendingInstanceIDs);
 FreeAndNil(fPendingPatternGraphics);
 FreeAndNil(fPendingPatternIDs);
 FreeAndNil(fPendingPatternIsStroke);
 FreeAndNil(fClipsByID);
 FreeAndNil(fClips);

 inherited Destroy;

end;

function TpvVectorGraphicsBeroshReader.ParseFloat(const aText:TpvRawByteString;const aDefault:TpvDouble):TpvDouble;
var Text:TpvRawByteString;
    Index:TpvSizeInt;
    OK:TPasDblStrUtilsBoolean;
begin

 Text:=TpvRawByteString(Trim(String(aText)));
 if Length(Text)=0 then begin
  result:=aDefault;
  exit;
 end;

 // Files written on a machine with a decimal comma turn up often enough to be worth handling.
 for Index:=1 to Length(Text) do begin
  if Text[Index]=',' then begin
   Text[Index]:='.';
  end;
 end;

 result:=ConvertStringToDouble(TPasDblStrUtilsRawByteString(Text),rmNearest,@OK);
 if not OK then begin
  result:=aDefault;
 end;

end;

function TpvVectorGraphicsBeroshReader.ParseInteger(const aText:TpvRawByteString;const aDefault:TpvInt64):TpvInt64;
var Code:TpvInt32;
begin
 Val(String(Trim(String(aText))),result,Code);
 if Code<>0 then begin
  result:=aDefault;
 end;
end;

function TpvVectorGraphicsBeroshReader.ParseBoolean(const aText:TpvRawByteString;const aDefault:boolean):boolean;
var Text:string;
begin

 Text:=LowerCase(Trim(String(aText)));
 if Length(Text)=0 then begin
  result:=aDefault;
  exit;
 end;

 // The original accepted the German spellings too, so they stay accepted.
 result:=(Text='true') or (Text='on') or (Text='1') or (Text='wahr') or (Text='an');

end;

function TpvVectorGraphicsBeroshReader.ParseColor(const aText:TpvRawByteString):TpvVectorGraphicsColor;
var Text:TpvRawByteString;
    Index,Digit:TpvInt32;
    Value:TpvUInt32;
begin

 result:=0;
 Text:=TpvRawByteString(Trim(String(aText)));
 if (Length(Text)>0) and (Text[1]='#') then begin
  Delete(Text,1,1);
 end;

 Value:=0;
 for Index:=1 to Length(Text) do begin
  Digit:=HexDigitValue(Text[Index]);
  if Digit<0 then begin
   break;
  end;
  Value:=(Value shl 4) or TpvUInt32(Digit);
 end;

 result:=Value;

end;

function TpvVectorGraphicsBeroshReader.ParseFillRule(const aText:TpvRawByteString):TpvVectorPathFillRule;
begin
 if LowerCase(Trim(String(aText)))='evenodd' then begin
  result:=TpvVectorPathFillRule.EvenOdd;
 end else begin
  result:=TpvVectorPathFillRule.NonZero;
 end;
end;

function TpvVectorGraphicsBeroshReader.ParseSpreadMethod(const aText:TpvRawByteString):TpvVectorGraphicsSpreadMethod;
var Text:string;
begin

 Text:=LowerCase(Trim(String(aText)));

 // Anything unrecognised falls back to pad, which is the default of the format.
 if Text='reflect' then begin
  result:=TpvVectorGraphicsSpreadMethod.Reflect;
 end else if Text='repeat' then begin
  result:=TpvVectorGraphicsSpreadMethod.Repeat_;
 end else begin
  result:=TpvVectorGraphicsSpreadMethod.Pad;
 end;

end;

procedure TpvVectorGraphicsBeroshReader.ReadSimpleMatrix(const aTag:TpvXMLTag;out aSimpleMatrix:TpvVectorGraphicsSimpleMatrix);
begin

 aSimpleMatrix:=TpvVectorGraphicsSimpleMatrix.CreateDefault;

 aSimpleMatrix.Matrix.m00:=ParseFloat(aTag.GetParameter('m0','1'),1.0);
 aSimpleMatrix.Matrix.m01:=ParseFloat(aTag.GetParameter('m1','0'),0.0);
 aSimpleMatrix.Matrix.m10:=ParseFloat(aTag.GetParameter('m2','0'),0.0);
 aSimpleMatrix.Matrix.m11:=ParseFloat(aTag.GetParameter('m3','1'),1.0);
 aSimpleMatrix.Matrix.m20:=ParseFloat(aTag.GetParameter('m4','0'),0.0);
 aSimpleMatrix.Matrix.m21:=ParseFloat(aTag.GetParameter('m5','0'),0.0);

 aSimpleMatrix.TranslateX:=ParseFloat(aTag.GetParameter('translatex','0'),0.0);
 aSimpleMatrix.TranslateY:=ParseFloat(aTag.GetParameter('translatey','0'),0.0);
 aSimpleMatrix.ScaleX:=ParseFloat(aTag.GetParameter('scalex','1'),1.0);
 aSimpleMatrix.ScaleY:=ParseFloat(aTag.GetParameter('scaley','1'),1.0);
 aSimpleMatrix.RotationDegrees:=ParseFloat(aTag.GetParameter('rotation','0'),0.0);
 aSimpleMatrix.SkewX:=ParseFloat(aTag.GetParameter('skewx','0'),0.0);
 aSimpleMatrix.SkewY:=ParseFloat(aTag.GetParameter('skewy','0'),0.0);

end;

procedure TpvVectorGraphicsBeroshReader.ReadPathCommands(const aTag:TpvXMLTag;const aPath:TpvVectorPath);
var Index:TpvSizeInt;
    SubTag:TpvXMLTag;
    Name:string;
begin

 aPath.Commands.Clear;

 for Index:=0 to aTag.Items.Count-1 do begin

  if not (aTag.Items[Index] is TpvXMLTag) then begin
   continue;
  end;

  SubTag:=TpvXMLTag(aTag.Items[Index]);
  Name:=LowerCase(String(SubTag.Name));

  if Name='moveto' then begin
   aPath.MoveTo(ParseFloat(SubTag.GetParameter('x','0')),
                ParseFloat(SubTag.GetParameter('y','0')));
  end else if Name='lineto' then begin
   aPath.LineTo(ParseFloat(SubTag.GetParameter('x','0')),
                ParseFloat(SubTag.GetParameter('y','0')));
  end else if Name='quadraticcubicto' then begin
   aPath.QuadraticCurveTo(ParseFloat(SubTag.GetParameter('cx','0')),
                          ParseFloat(SubTag.GetParameter('cy','0')),
                          ParseFloat(SubTag.GetParameter('ax','0')),
                          ParseFloat(SubTag.GetParameter('ay','0')));
  end else if Name='cubiccubicto' then begin
   aPath.CubicCurveTo(ParseFloat(SubTag.GetParameter('c1x','0')),
                      ParseFloat(SubTag.GetParameter('c1y','0')),
                      ParseFloat(SubTag.GetParameter('c2x','0')),
                      ParseFloat(SubTag.GetParameter('c2y','0')),
                      ParseFloat(SubTag.GetParameter('ax','0')),
                      ParseFloat(SubTag.GetParameter('ay','0')));
  end else if Name='close' then begin
   aPath.Close;
  end;

 end;

end;

procedure TpvVectorGraphicsBeroshReader.ReadFillData(const aTag:TpvXMLTag;var aFillData:TpvVectorGraphicsFillData;
                                                     const aOwner:TpvVectorGraphicsObject;const aIsStroke:boolean);
var Index,StopIndex,CountStops:TpvSizeInt;
    SubTag,StopTag:TpvXMLTag;
    Name:string;
    ImageID,PatternID:TpvInt32;
begin

 aFillData:=TpvVectorGraphicsFillData.CreateDefault;

 Name:=LowerCase(Trim(String(aTag.GetParameter('mode','none'))));
 if Name='color' then begin
  aFillData.Mode:=TpvVectorGraphicsFillMode.Color;
 end else if Name='image' then begin
  aFillData.Mode:=TpvVectorGraphicsFillMode.Image;
 end else if Name='pattern' then begin
  aFillData.Mode:=TpvVectorGraphicsFillMode.Pattern;
 end else if Name='radialgradient' then begin
  aFillData.Mode:=TpvVectorGraphicsFillMode.RadialGradient;
 end else if Name='lineargradient' then begin
  aFillData.Mode:=TpvVectorGraphicsFillMode.LinearGradient;
 end else begin
  aFillData.Mode:=TpvVectorGraphicsFillMode.None;
 end;

 aFillData.Color:=ParseColor(aTag.GetParameter('color','#00000000'));
 aFillData.FillRule:=ParseFillRule(aTag.GetParameter('fillrule','nonzero'));
 aFillData.ClipRule:=ParseFillRule(aTag.GetParameter('cliprule','nonzero'));
 aFillData.SpreadMethod:=ParseSpreadMethod(aTag.GetParameter('repeatmode','pad'));

 for Index:=0 to aTag.Items.Count-1 do begin

  if not (aTag.Items[Index] is TpvXMLTag) then begin
   continue;
  end;

  SubTag:=TpvXMLTag(aTag.Items[Index]);
  Name:=LowerCase(String(SubTag.Name));

  if Name='image' then begin

   ImageID:=ParseInteger(SubTag.GetParameter('id','0'),0);
   for StopIndex:=0 to fDocument.Images.Count-1 do begin
    if fDocument.Images[StopIndex].ID=ImageID then begin
     aFillData.Image:=fDocument.Images[StopIndex];
     break;
    end;
   end;

  end else if Name='matrix' then begin

   ReadSimpleMatrix(SubTag,aFillData.SimpleMatrix);

  end else if Name='gradientfocusmatrix' then begin

   ReadSimpleMatrix(SubTag,aFillData.GradientFocusSimpleMatrix);

  end else if Name='gradient' then begin

   aFillData.GradientFocusX:=ParseFloat(SubTag.GetParameter('focusx','0'),0.0);
   aFillData.GradientFocusY:=ParseFloat(SubTag.GetParameter('focusy','0'),0.0);
   aFillData.GradientRadius:=ParseFloat(SubTag.GetParameter('radius','1'),1.0);

   CountStops:=0;
   SetLength(aFillData.GradientStops,SubTag.Items.Count);
   for StopIndex:=0 to SubTag.Items.Count-1 do begin
    if SubTag.Items[StopIndex] is TpvXMLTag then begin
     StopTag:=TpvXMLTag(SubTag.Items[StopIndex]);
     if LowerCase(String(StopTag.Name))='item' then begin
      aFillData.GradientStops[CountStops].Offset:=ParseFloat(StopTag.GetParameter('offset','0'),0.0);
      aFillData.GradientStops[CountStops].Color:=ParseColor(StopTag.GetParameter('color','#00000000'));
      inc(CountStops);
     end;
    end;
   end;
   SetLength(aFillData.GradientStops,CountStops);

  end else if Name='pattern' then begin

   // The clip it points at may not have been read yet, so the link is made afterwards.
   PatternID:=ParseInteger(SubTag.GetParameter('movieclipid','0'),0);
   if (PatternID<>0) and assigned(aOwner) then begin
    fPendingPatternGraphics.Add(aOwner);
    fPendingPatternIDs.Add(PatternID);
    fPendingPatternIsStroke.Add(aIsStroke);
   end;

  end;

 end;

end;

procedure TpvVectorGraphicsBeroshReader.ReadTimeLine(const aTag:TpvXMLTag;const aObject:TpvVectorGraphicsObject);
var Index,Count:TpvSizeInt;
    ItemTag:TpvXMLTag;
    TimeLine:TpvVectorGraphicsTimeLine;
    Item:PpvVectorGraphicsTimeLineItem;
 procedure ReadItem(const aItemTag:TpvXMLTag);
 var ValueIndex:TpvSizeInt;
     ValueName:string;
     SubValueTag:TpvXMLTag;
 begin

  Count:=Length(TimeLine);
  SetLength(TimeLine,Count+1);
  Item:=@TimeLine[Count];

  Item^.StartTick:=ParseInteger(aItemTag.GetParameter('starttick','0'),0);
  Item^.EndTick:=ParseInteger(aItemTag.GetParameter('endtick','0'),0);
  Item^.HasAlpha:=false;
  Item^.HasFillColor:=false;
  Item^.HasStrokeColor:=false;
  Item^.HasStrokeWidth:=false;
  Item^.HasFillSimpleMatrix:=false;
  Item^.HasStrokeFillSimpleMatrix:=false;
  Item^.MorphPath:=nil;
  Item^.TransformSimpleMatrix:=TpvVectorGraphicsSimpleMatrix.CreateDefault;

  ValueName:=LowerCase(Trim(String(aItemTag.GetParameter('action','none'))));
  if ValueName='show' then begin
   Item^.Action:=TpvVectorGraphicsTimeLineAction.Show;
  end else if ValueName='hide' then begin
   Item^.Action:=TpvVectorGraphicsTimeLineAction.Hide;
  end else if ValueName='transform' then begin
   Item^.Action:=TpvVectorGraphicsTimeLineAction.Transform;
  end else if ValueName='move' then begin
   Item^.Action:=TpvVectorGraphicsTimeLineAction.Move;
  end else if ValueName='morph' then begin
   Item^.Action:=TpvVectorGraphicsTimeLineAction.Morph;
  end else begin
   Item^.Action:=TpvVectorGraphicsTimeLineAction.None;
  end;

  ValueName:=LowerCase(Trim(String(aItemTag.GetParameter('interpolationmode','nearest'))));
  if ValueName='linear' then begin
   Item^.InterpolationMode:=TpvVectorGraphicsInterpolationMode.Linear;
  end else if ValueName='cosine' then begin
   Item^.InterpolationMode:=TpvVectorGraphicsInterpolationMode.Cosine;
  end else if ValueName='cubic' then begin
   Item^.InterpolationMode:=TpvVectorGraphicsInterpolationMode.Cubic;
  end else if ValueName='hermite' then begin
   Item^.InterpolationMode:=TpvVectorGraphicsInterpolationMode.Hermite;
  end else begin
   Item^.InterpolationMode:=TpvVectorGraphicsInterpolationMode.Nearest;
  end;

  if Item^.Action=TpvVectorGraphicsTimeLineAction.Transform then begin
   ReadSimpleMatrix(aItemTag,Item^.TransformSimpleMatrix);
  end;

  for ValueIndex:=0 to aItemTag.Items.Count-1 do begin

   if not (aItemTag.Items[ValueIndex] is TpvXMLTag) then begin
    continue;
   end;

   SubValueTag:=TpvXMLTag(aItemTag.Items[ValueIndex]);
   ValueName:=LowerCase(String(SubValueTag.Name));

   if ValueName='linecolor' then begin
    Item^.HasStrokeColor:=ParseBoolean(SubValueTag.GetParameter('active','false'));
    Item^.StrokeColor:=ParseColor(SubValueTag.GetParameter('value','#00000000'));
   end else if ValueName='linewidth' then begin
    Item^.HasStrokeWidth:=ParseBoolean(SubValueTag.GetParameter('active','false'));
    Item^.StrokeWidth:=ParseFloat(SubValueTag.GetParameter('value','1'),1.0);
   end else if ValueName='linefillmatrix' then begin
    Item^.HasStrokeFillSimpleMatrix:=ParseBoolean(SubValueTag.GetParameter('active','false'));
    ReadSimpleMatrix(SubValueTag,Item^.StrokeFillSimpleMatrix);
   end else if ValueName='fillcolor' then begin
    Item^.HasFillColor:=ParseBoolean(SubValueTag.GetParameter('active','false'));
    Item^.FillColor:=ParseColor(SubValueTag.GetParameter('value','#00000000'));
   end else if ValueName='fillmatrix' then begin
    Item^.HasFillSimpleMatrix:=ParseBoolean(SubValueTag.GetParameter('active','false'));
    ReadSimpleMatrix(SubValueTag,Item^.FillSimpleMatrix);
   end else if ValueName='alpha' then begin
    Item^.HasAlpha:=ParseBoolean(SubValueTag.GetParameter('active','false'));
    Item^.Alpha:=Min(Max(ParseInteger(SubValueTag.GetParameter('value','0'),0),0),255);
   end else if ValueName='shapecommands' then begin
    Item^.MorphPath:=TpvVectorPath.Create;
    ReadPathCommands(SubValueTag,Item^.MorphPath);
   end;

  end;

 end;
begin

 TimeLine:=aObject.TimeLine;

 for Index:=0 to aTag.Items.Count-1 do begin
  if aTag.Items[Index] is TpvXMLTag then begin
   ItemTag:=TpvXMLTag(aTag.Items[Index]);
   if LowerCase(String(ItemTag.Name))='item' then begin
    ReadItem(ItemTag);
   end;
  end;
 end;

 aObject.TimeLine:=TimeLine;

end;

procedure TpvVectorGraphicsBeroshReader.ReadObjectBase(const aTag:TpvXMLTag;const aObject:TpvVectorGraphicsObject);
var Index:TpvSizeInt;
    SubTag:TpvXMLTag;
    Name,Value:string;
    SimpleMatrix:TpvVectorGraphicsSimpleMatrix;
    TimeLineTag:TpvXMLTag;
    LooseItems:boolean;
begin

 TimeLineTag:=nil;
 LooseItems:=false;

 for Index:=0 to aTag.Items.Count-1 do begin

  if not (aTag.Items[Index] is TpvXMLTag) then begin
   continue;
  end;

  SubTag:=TpvXMLTag(aTag.Items[Index]);
  Name:=LowerCase(String(SubTag.Name));

  if Name='name' then begin

   aObject.Name:=TpvUTF8String(SubTag.GetParameter('value',''));

  end else if Name='matrix' then begin

   ReadSimpleMatrix(SubTag,SimpleMatrix);
   aObject.SimpleMatrix:=SimpleMatrix;

  end else if Name='alpha' then begin

   aObject.Alpha:=Min(Max(ParseInteger(SubTag.GetParameter('value','255'),255),0),255);

  end else if Name='z' then begin

   aObject.Z:=ParseFloat(SubTag.GetParameter('value','0'),0.0);

  end else if Name='offset' then begin

   aObject.OffsetX:=ParseFloat(SubTag.GetParameter('x','0'),0.0);
   aObject.OffsetY:=ParseFloat(SubTag.GetParameter('y','0'),0.0);

  end else if Name='mask' then begin

   Value:=LowerCase(Trim(String(SubTag.GetParameter('mode','none'))));
   if Value='clip' then begin
    aObject.MaskMode:=TpvVectorGraphicsMaskMode.Clip;
   end else if Value='mono' then begin
    aObject.MaskMode:=TpvVectorGraphicsMaskMode.Mono;
   end else if Value='color' then begin
    aObject.MaskMode:=TpvVectorGraphicsMaskMode.Color;
   end else begin
    aObject.MaskMode:=TpvVectorGraphicsMaskMode.None;
   end;

  end else if Name='flags' then begin

   // Older files expressed the mask mode through these flags instead of a mask element.
   if ParseBoolean(SubTag.GetParameter('clipmask','false')) then begin
    aObject.MaskMode:=TpvVectorGraphicsMaskMode.Clip;
   end else if ParseBoolean(SubTag.GetParameter('mask','false')) then begin
    if ParseBoolean(SubTag.GetParameter('maskmono','false')) then begin
     aObject.MaskMode:=TpvVectorGraphicsMaskMode.Mono;
    end else begin
     aObject.MaskMode:=TpvVectorGraphicsMaskMode.Color;
    end;
   end;

   aObject.Visible:=ParseBoolean(SubTag.GetParameter('visible','true'),true);
   aObject.Locked:=ParseBoolean(SubTag.GetParameter('locked','false'));
   aObject.Outline:=ParseBoolean(SubTag.GetParameter('outline','false'));

  end else if Name='timeline' then begin

   TimeLineTag:=SubTag;

  end else if Name='item' then begin

   // The original writer put the timeline items next to the timeline element rather than inside
   // it, so a file written that way is still read correctly.
   LooseItems:=true;

  end;

 end;

 if assigned(TimeLineTag) then begin
  ReadTimeLine(TimeLineTag,aObject);
 end;

 if LooseItems then begin
  ReadTimeLine(aTag,aObject);
 end;

end;

procedure TpvVectorGraphicsBeroshReader.ReadGraphic(const aTag:TpvXMLTag;const aGraphic:TpvVectorGraphicsGraphic);
var Index,SubIndex:TpvSizeInt;
    SubTag,FillTag:TpvXMLTag;
    Name,Value:string;
    Style:TpvVectorGraphicsStyle;
begin

 ReadObjectBase(aTag,aGraphic);

 Style:=aGraphic.Style;

 for Index:=0 to aTag.Items.Count-1 do begin

  if not (aTag.Items[Index] is TpvXMLTag) then begin
   continue;
  end;

  SubTag:=TpvXMLTag(aTag.Items[Index]);
  Name:=LowerCase(String(SubTag.Name));

  if Name='settings' then begin

   Style.OffImage:=ParseBoolean(SubTag.GetParameter('offimage','false'));

  end else if Name='stroke' then begin

   Value:=LowerCase(Trim(String(SubTag.GetParameter('capmode','butt'))));
   if Value='round' then begin
    Style.StrokeCapMode:=TpvVectorPathStrokeCapMode.Round;
   end else if Value='square' then begin
    Style.StrokeCapMode:=TpvVectorPathStrokeCapMode.Square;
   end else begin
    Style.StrokeCapMode:=TpvVectorPathStrokeCapMode.Butt;
   end;

   Value:=LowerCase(Trim(String(SubTag.GetParameter('joinmode','miter'))));
   if Value='bevel' then begin
    Style.StrokeJoinMode:=TpvVectorPathStrokeJoinMode.Bevel;
   end else if Value='round' then begin
    Style.StrokeJoinMode:=TpvVectorPathStrokeJoinMode.Round;
   end else if Value='miterrevert' then begin
    Style.StrokeJoinMode:=TpvVectorPathStrokeJoinMode.MiterRevert;
   end else if Value='miterround' then begin
    Style.StrokeJoinMode:=TpvVectorPathStrokeJoinMode.MiterRound;
   end else begin
    Style.StrokeJoinMode:=TpvVectorPathStrokeJoinMode.Miter;
   end;

   Value:=LowerCase(Trim(String(SubTag.GetParameter('innerjoinmode','miter'))));
   if Value='bevel' then begin
    Style.StrokeInnerJoinMode:=TpvVectorPathStrokeInnerJoinMode.Bevel;
   end else if Value='jag' then begin
    Style.StrokeInnerJoinMode:=TpvVectorPathStrokeInnerJoinMode.Jag;
   end else if Value='round' then begin
    Style.StrokeInnerJoinMode:=TpvVectorPathStrokeInnerJoinMode.Round;
   end else begin
    Style.StrokeInnerJoinMode:=TpvVectorPathStrokeInnerJoinMode.Miter;
   end;

   Style.StrokeWidth:=ParseFloat(SubTag.GetParameter('width','1'),1.0);
   Style.StrokeWidthFixed:=ParseBoolean(SubTag.GetParameter('fixedwidth','false'));
   Style.StrokeInnerMiterLimit:=ParseFloat(SubTag.GetParameter('innermiterlimit','1.01'),1.01);
   Style.StrokeMiterLimit:=ParseFloat(SubTag.GetParameter('miterlimit','4'),4.0);

   // The pictorial stroke pattern of the editor becomes a plain dash array here.
   Style.StrokeDashArray:=pvVectorPathStrokePatternToDashArray(SubTag.GetParameter('pattern',''),
                                                               ParseFloat(SubTag.GetParameter('patternstepsize','1'),1.0));
   Style.StrokeDashOffset:=ParseFloat(SubTag.GetParameter('patternstart','0'),0.0);

   for SubIndex:=0 to SubTag.Items.Count-1 do begin
    if SubTag.Items[SubIndex] is TpvXMLTag then begin
     FillTag:=TpvXMLTag(SubTag.Items[SubIndex]);
     if LowerCase(String(FillTag.Name))='fill' then begin
      ReadFillData(FillTag,Style.StrokeFillData,aGraphic,true);
     end;
    end;
   end;

  end else if Name='fill' then begin

   ReadFillData(SubTag,Style.FillData,aGraphic,false);

  end else if Name='commands' then begin

   ReadPathCommands(SubTag,aGraphic.Path);

  end;

 end;

 aGraphic.Style:=Style;

end;

procedure TpvVectorGraphicsBeroshReader.ReadGroup(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup);
var Index,SubIndex:TpvSizeInt;
    SubTag,ObjectTag:TpvXMLTag;
    Name:string;
    ClipID:TpvInt32;
begin

 ReadObjectBase(aTag,aGroup);

 for Index:=0 to aTag.Items.Count-1 do begin

  if not (aTag.Items[Index] is TpvXMLTag) then begin
   continue;
  end;

  SubTag:=TpvXMLTag(aTag.Items[Index]);
  Name:=LowerCase(String(SubTag.Name));

  if Name='id' then begin

   // The numeric id only serves to resolve references and is not kept on the group itself.
   ClipID:=ParseInteger(SubTag.GetParameter('value','0'),0);
   fClipsByID.Add(ClipID);
   fClips.Add(aGroup);

  end else if Name='settings' then begin

   aGroup.OffImage:=ParseBoolean(SubTag.GetParameter('offimage','false'));
   aGroup.Gamma:=ParseFloat(SubTag.GetParameter('gamma','1'),1.0);
   aGroup.LoopTime:=ParseInteger(SubTag.GetParameter('looptime','0'),0);
   aGroup.Width:=ParseFloat(SubTag.GetParameter('width','0'),0.0);
   aGroup.Height:=ParseFloat(SubTag.GetParameter('height','0'),0.0);
   aGroup.BackgroundColor:=ParseColor(SubTag.GetParameter('color','#00000000'));

  end else if Name='objects' then begin

   for SubIndex:=0 to SubTag.Items.Count-1 do begin
    if SubTag.Items[SubIndex] is TpvXMLTag then begin
     ObjectTag:=TpvXMLTag(SubTag.Items[SubIndex]);
     ReadObject(ObjectTag,aGroup);
    end;
   end;

  end;

 end;

end;

function TpvVectorGraphicsBeroshReader.ReadObject(const aTag:TpvXMLTag;const aParent:TpvVectorGraphicsGroup):TpvVectorGraphicsObject;
var Index:TpvSizeInt;
    SubTag:TpvXMLTag;
    Name,SubName:string;
    Shape:TpvVectorGraphicsShape;
    Line:TpvVectorGraphicsLine;
    Rectangle:TpvVectorGraphicsRectangle;
    Ellipse:TpvVectorGraphicsEllipse;
    Text:TpvVectorGraphicsText;
    Group:TpvVectorGraphicsGroup;
    Instance:TpvVectorGraphicsGroupInstance;
    TextContent:TpvUTF8String;
    TextIndex:TpvSizeInt;
begin

 result:=nil;
 Name:=LowerCase(String(aTag.Name));

 if Name='movieclip' then begin

  Group:=TpvVectorGraphicsGroup.Create(fDocument,aParent);
  aParent.Add(Group);
  ReadGroup(aTag,Group);
  result:=Group;

 end else if Name='movieclipinstance' then begin

  Instance:=TpvVectorGraphicsGroupInstance.Create(fDocument,aParent);
  aParent.Add(Instance);
  ReadObjectBase(aTag,Instance);

  // The clip it points at may not have been read yet, so the link is made afterwards.
  for Index:=0 to aTag.Items.Count-1 do begin
   if aTag.Items[Index] is TpvXMLTag then begin
    SubTag:=TpvXMLTag(aTag.Items[Index]);
    if LowerCase(String(SubTag.Name))='movieclip' then begin
     fPendingInstances.Add(Instance);
     fPendingInstanceIDs.Add(ParseInteger(SubTag.GetParameter('id','0'),0));
    end;
   end;
  end;

  result:=Instance;

 end else if Name='shape' then begin

  Shape:=TpvVectorGraphicsShape.Create(fDocument,aParent);
  aParent.Add(Shape);
  ReadGraphic(aTag,Shape);
  result:=Shape;

 end else if Name='line' then begin

  Line:=TpvVectorGraphicsLine.Create(fDocument,aParent);
  aParent.Add(Line);
  ReadGraphic(aTag,Line);

  for Index:=0 to aTag.Items.Count-1 do begin
   if aTag.Items[Index] is TpvXMLTag then begin
    SubTag:=TpvXMLTag(aTag.Items[Index]);
    SubName:=LowerCase(String(SubTag.Name));
    if SubName='startpoint' then begin
     Line.x1:=ParseFloat(SubTag.GetParameter('x','0'),0.0);
     Line.y1:=ParseFloat(SubTag.GetParameter('y','0'),0.0);
    end else if SubName='endpoint' then begin
     Line.x2:=ParseFloat(SubTag.GetParameter('x','0'),0.0);
     Line.y2:=ParseFloat(SubTag.GetParameter('y','0'),0.0);
    end;
   end;
  end;

  Line.MarkPathDirty;
  result:=Line;

 end else if Name='rect' then begin

  Rectangle:=TpvVectorGraphicsRectangle.Create(fDocument,aParent);
  aParent.Add(Rectangle);
  ReadGraphic(aTag,Rectangle);

  for Index:=0 to aTag.Items.Count-1 do begin
   if aTag.Items[Index] is TpvXMLTag then begin
    SubTag:=TpvXMLTag(aTag.Items[Index]);
    SubName:=LowerCase(String(SubTag.Name));
    if SubName='position' then begin
     Rectangle.x:=ParseFloat(SubTag.GetParameter('x','0'),0.0);
     Rectangle.y:=ParseFloat(SubTag.GetParameter('y','0'),0.0);
    end else if SubName='size' then begin
     Rectangle.Width:=ParseFloat(SubTag.GetParameter('width','0'),0.0);
     Rectangle.Height:=ParseFloat(SubTag.GetParameter('height','0'),0.0);
    end else if SubName='round' then begin
     Rectangle.RadiusX:=ParseFloat(SubTag.GetParameter('x','0'),0.0);
     Rectangle.RadiusY:=ParseFloat(SubTag.GetParameter('y','0'),0.0);
    end;
   end;
  end;

  Rectangle.MarkPathDirty;
  result:=Rectangle;

 end else if Name='ellipse' then begin

  Ellipse:=TpvVectorGraphicsEllipse.Create(fDocument,aParent);
  aParent.Add(Ellipse);
  ReadGraphic(aTag,Ellipse);

  for Index:=0 to aTag.Items.Count-1 do begin
   if aTag.Items[Index] is TpvXMLTag then begin
    SubTag:=TpvXMLTag(aTag.Items[Index]);
    SubName:=LowerCase(String(SubTag.Name));
    if SubName='center' then begin
     Ellipse.CenterX:=ParseFloat(SubTag.GetParameter('x','0'),0.0);
     Ellipse.CenterY:=ParseFloat(SubTag.GetParameter('y','0'),0.0);
    end else if SubName='radius' then begin
     Ellipse.RadiusX:=ParseFloat(SubTag.GetParameter('x','0'),0.0);
     Ellipse.RadiusY:=ParseFloat(SubTag.GetParameter('y','0'),0.0);
    end;
   end;
  end;

  Ellipse.MarkPathDirty;
  result:=Ellipse;

 end else if Name='text' then begin

  Text:=TpvVectorGraphicsText.Create(fDocument,aParent);
  aParent.Add(Text);
  ReadGraphic(aTag,Text);

  for Index:=0 to aTag.Items.Count-1 do begin

   if not (aTag.Items[Index] is TpvXMLTag) then begin
    continue;
   end;

   SubTag:=TpvXMLTag(aTag.Items[Index]);
   SubName:=LowerCase(String(SubTag.Name));

   if SubName='font' then begin

    Text.FontFamily:=TpvUTF8String(SubTag.GetParameter('name',''));
    Text.Bold:=ParseBoolean(SubTag.GetParameter('bold','false'));
    Text.Italic:=ParseBoolean(SubTag.GetParameter('italic','false'));
    Text.FontSize:=ParseFloat(SubTag.GetParameter('size','12'),12.0);
    Text.LetterSpacing:=ParseFloat(SubTag.GetParameter('letterspacing','0'),0.0);

    SubName:=LowerCase(Trim(String(SubTag.GetParameter('textanchorx','left'))));
    if SubName='middle' then begin
     Text.Anchor:=TpvVectorGraphicsTextAnchor.Middle;
    end else if SubName='right' then begin
     Text.Anchor:=TpvVectorGraphicsTextAnchor.End_;
    end else begin
     Text.Anchor:=TpvVectorGraphicsTextAnchor.Start;
    end;

    SubName:=LowerCase(Trim(String(SubTag.GetParameter('textanchory','top'))));
    if SubName='middle' then begin
     Text.Baseline:=TpvVectorGraphicsTextBaseline.Middle;
    end else if SubName='bottom' then begin
     Text.Baseline:=TpvVectorGraphicsTextBaseline.Bottom;
    end else begin
     Text.Baseline:=TpvVectorGraphicsTextBaseline.Top;
    end;

   end else if SubName='text' then begin

    TextContent:='';
    for TextIndex:=0 to SubTag.Items.Count-1 do begin
     if SubTag.Items[TextIndex] is TpvXMLText then begin
      TextContent:=TextContent+TpvUTF8String(TpvXMLText(SubTag.Items[TextIndex]).Text);
     end else if SubTag.Items[TextIndex] is TpvXMLCDataTag then begin
      TextContent:=TextContent+TpvUTF8String(TpvXMLCDataTag(SubTag.Items[TextIndex]).Text);
     end;
    end;
    Text.Text:=TextContent;

   end;

  end;

  Text.MarkPathDirty;
  result:=Text;

 end;

end;

procedure TpvVectorGraphicsBeroshReader.ReadImage(const aTag:TpvXMLTag);
var Index,Position,Count,High_,Low_:TpvSizeInt;
    SubTag:TpvXMLTag;
    Name:string;
    Image:TpvVectorGraphicsImageResource;
    Width,Height:TpvInt32;
    Data:TpvRawByteString;
    Pixels:TpvVectorGraphicsColorArray;
    Bytes:array of TpvUInt8;
begin

 Image:=fDocument.NewImage;
 Width:=0;
 Height:=0;
 Data:='';

 for Index:=0 to aTag.Items.Count-1 do begin

  if not (aTag.Items[Index] is TpvXMLTag) then begin
   continue;
  end;

  SubTag:=TpvXMLTag(aTag.Items[Index]);
  Name:=LowerCase(String(SubTag.Name));

  if Name='settings' then begin
   Image.ID:=ParseInteger(SubTag.GetParameter('id','0'),0);
  end else if Name='size' then begin
   Width:=ParseInteger(SubTag.GetParameter('width','0'),0);
   Height:=ParseInteger(SubTag.GetParameter('height','0'),0);
  end else if Name='pixels' then begin
   Data:=SubTag.GetParameter('data','');
  end;

 end;

 if (Width<=0) or (Height<=0) then begin
  exit;
 end;

 Image.SetSize(Width,Height);

 if Length(Data)<(Width*Height*8) then begin
  exit;
 end;

 // Two hex digits per byte, four bytes per pixel, in the same order the renderer uses.
 Bytes:=nil;
 try

  Count:=Width*Height*4;
  SetLength(Bytes,Count);
  Position:=1;
  for Index:=0 to Count-1 do begin
   High_:=HexDigitValue(Data[Position]);
   Low_:=HexDigitValue(Data[Position+1]);
   if (High_<0) or (Low_<0) then begin
    exit;
   end;
   Bytes[Index]:=(High_ shl 4) or Low_;
   inc(Position,2);
  end;

  SetLength(Pixels,Width*Height);
  for Index:=0 to (Width*Height)-1 do begin
   Pixels[Index]:=TpvVectorGraphicsColor(Bytes[(Index*4)+0]) or
                  (TpvVectorGraphicsColor(Bytes[(Index*4)+1]) shl 8) or
                  (TpvVectorGraphicsColor(Bytes[(Index*4)+2]) shl 16) or
                  (TpvVectorGraphicsColor(Bytes[(Index*4)+3]) shl 24);
  end;
  Image.SetPixels(@Pixels[0],Width,Height);

 finally
  Bytes:=nil;
  Pixels:=nil;
 end;

end;

procedure TpvVectorGraphicsBeroshReader.ResolveReferences;
var Index:TpvSizeInt;
    Graphic:TpvVectorGraphicsGraphic;
    Style:TpvVectorGraphicsStyle;
 function FindClip(const aID:TpvInt32):TpvVectorGraphicsGroup;
 var Scan:TpvSizeInt;
 begin
  result:=nil;
  for Scan:=0 to fClipsByID.Count-1 do begin
   if fClipsByID[Scan]=aID then begin
    result:=fClips[Scan];
    exit;
   end;
  end;
 end;
begin

 for Index:=0 to fPendingInstances.Count-1 do begin
  if fPendingInstances[Index] is TpvVectorGraphicsGroupInstance then begin
   TpvVectorGraphicsGroupInstance(fPendingInstances[Index]).Target:=FindClip(fPendingInstanceIDs[Index]);
  end;
 end;

 for Index:=0 to fPendingPatternGraphics.Count-1 do begin
  if fPendingPatternGraphics[Index] is TpvVectorGraphicsGraphic then begin
   Graphic:=TpvVectorGraphicsGraphic(fPendingPatternGraphics[Index]);
   Style:=Graphic.Style;
   if fPendingPatternIsStroke[Index] then begin
    Style.StrokeFillData.Pattern:=FindClip(fPendingPatternIDs[Index]);
   end else begin
    Style.FillData.Pattern:=FindClip(fPendingPatternIDs[Index]);
   end;
   Graphic.Style:=Style;
  end;
 end;

end;

function TpvVectorGraphicsBeroshReader.Load(const aXML:TpvXML;const aDocument:TpvVectorGraphicsDocument):boolean;
var Index:TpvSizeInt;
    RootTag,Tag:TpvXMLTag;
    Name:string;
    Group:TpvVectorGraphicsGroup;
begin

 result:=false;

 if not (assigned(aXML) and assigned(aDocument)) then begin
  exit;
 end;

 fDocument:=aDocument;

 RootTag:=nil;
 for Index:=0 to aXML.Root.Items.Count-1 do begin
  if aXML.Root.Items[Index] is TpvXMLTag then begin
   Tag:=TpvXMLTag(aXML.Root.Items[Index]);
   Name:=LowerCase(String(Tag.Name));
   if (Name='movie') or (Name='berosh') then begin
    RootTag:=Tag;
    break;
   end;
  end;
 end;

 if not assigned(RootTag) then begin
  exit;
 end;

 ReadObjectBase(RootTag,fDocument.Root);

 // The images come first, because fills reference them by id while they are being read.
 for Index:=0 to RootTag.Items.Count-1 do begin
  if RootTag.Items[Index] is TpvXMLTag then begin
   Tag:=TpvXMLTag(RootTag.Items[Index]);
   if LowerCase(String(Tag.Name))='movieimage' then begin
    ReadImage(Tag);
   end;
  end;
 end;

 for Index:=0 to RootTag.Items.Count-1 do begin

  if not (RootTag.Items[Index] is TpvXMLTag) then begin
   continue;
  end;

  Tag:=TpvXMLTag(RootTag.Items[Index]);
  Name:=LowerCase(String(Tag.Name));

  if Name='settings' then begin

   fDocument.DrawBackground:=ParseBoolean(Tag.GetParameter('drawbackground','false'));

  end else if Name='rootmovieclip' then begin

   ReadGroup(Tag,fDocument.Root);

  end else if Name='movieclip' then begin

   // The standalone clips live in defs, where they serve as targets for instances and patterns
   // without being drawn on their own.
   Group:=TpvVectorGraphicsGroup.Create(fDocument,fDocument.Defs);
   fDocument.Defs.Add(Group);
   Group.Visible:=false;
   ReadGroup(Tag,Group);

  end;

 end;

 ResolveReferences;

 result:=true;

end;

{ TpvVectorGraphicsBeroshWriter }

constructor TpvVectorGraphicsBeroshWriter.Create;
begin

 inherited Create;

 fDocument:=nil;

 fClips:=TpvObjectGenericList<TpvVectorGraphicsGroup>.Create;
 fClips.OwnsObjects:=false;
 fClipIDs:=TpvGenericList<TpvInt32>.Create;
 fNextClipID:=1;

end;

destructor TpvVectorGraphicsBeroshWriter.Destroy;
begin

 FreeAndNil(fClips);
 FreeAndNil(fClipIDs);

 inherited Destroy;

end;

function TpvVectorGraphicsBeroshWriter.FormatFloat_(const aValue:TpvDouble):TpvRawByteString;
var Text:string;
    Index:TpvSizeInt;
begin

 Text:=Format('%.10g',[aValue],BeroshFormatSettings);

 // Trailing zeros only make the file bigger.
 if (Pos('.',Text)>0) and (Pos('e',LowerCase(Text))=0) then begin
  Index:=Length(Text);
  while (Index>0) and (Text[Index]='0') do begin
   dec(Index);
  end;
  if (Index>0) and (Text[Index]='.') then begin
   dec(Index);
  end;
  Text:=Copy(Text,1,Index);
 end;

 if Length(Text)=0 then begin
  Text:='0';
 end;

 result:=TpvRawByteString(Text);

end;

function TpvVectorGraphicsBeroshWriter.FormatColor(const aColor:TpvVectorGraphicsColor):TpvRawByteString;
begin
 result:=TpvRawByteString('#'+LowerCase(Format('%.8x',[aColor])));
end;

function TpvVectorGraphicsBeroshWriter.FormatBoolean(const aValue:boolean):TpvRawByteString;
begin
 if aValue then begin
  result:='true';
 end else begin
  result:='false';
 end;
end;

function TpvVectorGraphicsBeroshWriter.ClipIDOf(const aGroup:TpvVectorGraphicsGroup):TpvInt32;
var Index:TpvSizeInt;
begin

 result:=0;
 if not assigned(aGroup) then begin
  exit;
 end;

 for Index:=0 to fClips.Count-1 do begin
  if fClips[Index]=aGroup then begin
   result:=fClipIDs[Index];
   exit;
  end;
 end;

 result:=fNextClipID;
 inc(fNextClipID);
 fClips.Add(aGroup);
 fClipIDs.Add(result);

end;

procedure TpvVectorGraphicsBeroshWriter.CollectClips(const aObject:TpvVectorGraphicsObject);
var Index:TpvSizeInt;
    Graphic:TpvVectorGraphicsGraphic;
    Style:TpvVectorGraphicsStyle;
begin

 if not assigned(aObject) then begin
  exit;
 end;

 // Every group that something points at needs an id before anything gets written.
 if aObject is TpvVectorGraphicsGroupInstance then begin
  ClipIDOf(TpvVectorGraphicsGroupInstance(aObject).Group);
 end else if aObject is TpvVectorGraphicsGraphic then begin
  Graphic:=TpvVectorGraphicsGraphic(aObject);
  Style:=Graphic.Style;
  ClipIDOf(Style.FillData.Pattern);
  ClipIDOf(Style.StrokeFillData.Pattern);
 end;

 if aObject is TpvVectorGraphicsGroup then begin
  for Index:=0 to TpvVectorGraphicsGroup(aObject).Children.Count-1 do begin
   CollectClips(TpvVectorGraphicsGroup(aObject).Children[Index]);
  end;
 end;

end;

procedure TpvVectorGraphicsBeroshWriter.WriteSimpleMatrix(const aTag:TpvXMLTag;const aSimpleMatrix:TpvVectorGraphicsSimpleMatrix);
begin

 aTag.AddParameter('m0',FormatFloat_(aSimpleMatrix.Matrix.m00));
 aTag.AddParameter('m1',FormatFloat_(aSimpleMatrix.Matrix.m01));
 aTag.AddParameter('m2',FormatFloat_(aSimpleMatrix.Matrix.m10));
 aTag.AddParameter('m3',FormatFloat_(aSimpleMatrix.Matrix.m11));
 aTag.AddParameter('m4',FormatFloat_(aSimpleMatrix.Matrix.m20));
 aTag.AddParameter('m5',FormatFloat_(aSimpleMatrix.Matrix.m21));

 aTag.AddParameter('translatex',FormatFloat_(aSimpleMatrix.TranslateX));
 aTag.AddParameter('translatey',FormatFloat_(aSimpleMatrix.TranslateY));
 aTag.AddParameter('scalex',FormatFloat_(aSimpleMatrix.ScaleX));
 aTag.AddParameter('scaley',FormatFloat_(aSimpleMatrix.ScaleY));
 aTag.AddParameter('rotation',FormatFloat_(aSimpleMatrix.RotationDegrees));
 aTag.AddParameter('skewx',FormatFloat_(aSimpleMatrix.SkewX));
 aTag.AddParameter('skewy',FormatFloat_(aSimpleMatrix.SkewY));

end;

procedure TpvVectorGraphicsBeroshWriter.WritePathCommands(const aTag:TpvXMLTag;const aPath:TpvVectorPath);
var Command:TpvVectorPathCommand;
    SubTag:TpvXMLTag;
begin

 if not assigned(aPath) then begin
  exit;
 end;

 for Command in aPath.Commands do begin

  if not assigned(Command) then begin
   continue;
  end;

  SubTag:=TpvXMLTag.Create;
  aTag.Add(SubTag);

  case Command.CommandType of
   TpvVectorPathCommandType.MoveTo:begin
    SubTag.Name:='moveto';
    SubTag.AddParameter('x',FormatFloat_(Command.x0));
    SubTag.AddParameter('y',FormatFloat_(Command.y0));
   end;
   TpvVectorPathCommandType.LineTo:begin
    SubTag.Name:='lineto';
    SubTag.AddParameter('x',FormatFloat_(Command.x0));
    SubTag.AddParameter('y',FormatFloat_(Command.y0));
   end;
   TpvVectorPathCommandType.QuadraticCurveTo:begin
    SubTag.Name:='quadraticcubicto';
    SubTag.AddParameter('cx',FormatFloat_(Command.x0));
    SubTag.AddParameter('cy',FormatFloat_(Command.y0));
    SubTag.AddParameter('ax',FormatFloat_(Command.x1));
    SubTag.AddParameter('ay',FormatFloat_(Command.y1));
   end;
   TpvVectorPathCommandType.CubicCurveTo:begin
    SubTag.Name:='cubiccubicto';
    SubTag.AddParameter('c1x',FormatFloat_(Command.x0));
    SubTag.AddParameter('c1y',FormatFloat_(Command.y0));
    SubTag.AddParameter('c2x',FormatFloat_(Command.x1));
    SubTag.AddParameter('c2y',FormatFloat_(Command.y1));
    SubTag.AddParameter('ax',FormatFloat_(Command.x2));
    SubTag.AddParameter('ay',FormatFloat_(Command.y2));
   end;
   else {TpvVectorPathCommandType.Close:}begin
    SubTag.Name:='close';
   end;
  end;

 end;

end;

procedure TpvVectorGraphicsBeroshWriter.WriteFillData(const aTag:TpvXMLTag;const aFillData:TpvVectorGraphicsFillData);
var Index:TpvSizeInt;
    SubTag,StopTag:TpvXMLTag;
    Name:TpvRawByteString;
begin

 case aFillData.Mode of
  TpvVectorGraphicsFillMode.Color:begin
   Name:='color';
  end;
  TpvVectorGraphicsFillMode.Image:begin
   Name:='image';
  end;
  TpvVectorGraphicsFillMode.Pattern:begin
   Name:='pattern';
  end;
  TpvVectorGraphicsFillMode.RadialGradient:begin
   Name:='radialgradient';
  end;
  TpvVectorGraphicsFillMode.LinearGradient:begin
   Name:='lineargradient';
  end;
  else begin
   Name:='none';
  end;
 end;
 aTag.AddParameter('mode',Name);

 aTag.AddParameter('color',FormatColor(aFillData.Color));

 if aFillData.FillRule=TpvVectorPathFillRule.EvenOdd then begin
  aTag.AddParameter('fillrule','evenodd');
 end else begin
  aTag.AddParameter('fillrule','nonzero');
 end;

 if aFillData.ClipRule=TpvVectorPathFillRule.EvenOdd then begin
  aTag.AddParameter('cliprule','evenodd');
 end else begin
  aTag.AddParameter('cliprule','nonzero');
 end;

 case aFillData.SpreadMethod of
  TpvVectorGraphicsSpreadMethod.Reflect:begin
   aTag.AddParameter('repeatmode','reflect');
  end;
  TpvVectorGraphicsSpreadMethod.Repeat_:begin
   aTag.AddParameter('repeatmode','repeat');
  end;
  else begin
   aTag.AddParameter('repeatmode','pad');
  end;
 end;

 SubTag:=TpvXMLTag.Create;
 SubTag.Name:='image';
 aTag.Add(SubTag);
 if assigned(aFillData.Image) then begin
  SubTag.AddParameter('id',TpvRawByteString(IntToStr(aFillData.Image.ID)));
 end else begin
  SubTag.AddParameter('id','0');
 end;

 SubTag:=TpvXMLTag.Create;
 SubTag.Name:='matrix';
 aTag.Add(SubTag);
 WriteSimpleMatrix(SubTag,aFillData.SimpleMatrix);

 SubTag:=TpvXMLTag.Create;
 SubTag.Name:='gradientfocusmatrix';
 aTag.Add(SubTag);
 WriteSimpleMatrix(SubTag,aFillData.GradientFocusSimpleMatrix);

 SubTag:=TpvXMLTag.Create;
 SubTag.Name:='gradient';
 aTag.Add(SubTag);
 SubTag.AddParameter('focusx',FormatFloat_(aFillData.GradientFocusX));
 SubTag.AddParameter('focusy',FormatFloat_(aFillData.GradientFocusY));
 SubTag.AddParameter('radius',FormatFloat_(aFillData.GradientRadius));
 for Index:=0 to Length(aFillData.GradientStops)-1 do begin
  StopTag:=TpvXMLTag.Create;
  StopTag.Name:='item';
  SubTag.Add(StopTag);
  StopTag.AddParameter('offset',FormatFloat_(aFillData.GradientStops[Index].Offset));
  StopTag.AddParameter('color',FormatColor(aFillData.GradientStops[Index].Color));
 end;

 SubTag:=TpvXMLTag.Create;
 SubTag.Name:='pattern';
 aTag.Add(SubTag);
 SubTag.AddParameter('movieclipid',TpvRawByteString(IntToStr(ClipIDOf(aFillData.Pattern))));

end;

procedure TpvVectorGraphicsBeroshWriter.WriteTimeLine(const aTag:TpvXMLTag;const aObject:TpvVectorGraphicsObject);
var Index:TpvSizeInt;
    TimeLineTag,ItemTag,ValueTag:TpvXMLTag;
    TimeLine:TpvVectorGraphicsTimeLine;
    Name:TpvRawByteString;
 procedure AddValue(const aName:TpvRawByteString;const aActive:boolean;const aValue:TpvRawByteString);
 begin
  ValueTag:=TpvXMLTag.Create;
  ValueTag.Name:=aName;
  ItemTag.Add(ValueTag);
  ValueTag.AddParameter('active',FormatBoolean(aActive));
  ValueTag.AddParameter('value',aValue);
 end;
begin

 TimeLine:=aObject.TimeLine;

 TimeLineTag:=TpvXMLTag.Create;
 TimeLineTag.Name:='timeline';
 aTag.Add(TimeLineTag);

 for Index:=0 to Length(TimeLine)-1 do begin

  // Inside the timeline element, which is where the reader looks for them.
  ItemTag:=TpvXMLTag.Create;
  ItemTag.Name:='item';
  TimeLineTag.Add(ItemTag);

  ItemTag.AddParameter('starttick',TpvRawByteString(IntToStr(TimeLine[Index].StartTick)));
  ItemTag.AddParameter('endtick',TpvRawByteString(IntToStr(TimeLine[Index].EndTick)));

  case TimeLine[Index].Action of
   TpvVectorGraphicsTimeLineAction.Show:begin
    Name:='show';
   end;
   TpvVectorGraphicsTimeLineAction.Hide:begin
    Name:='hide';
   end;
   TpvVectorGraphicsTimeLineAction.Transform:begin
    Name:='transform';
   end;
   TpvVectorGraphicsTimeLineAction.Move:begin
    Name:='move';
   end;
   TpvVectorGraphicsTimeLineAction.Morph:begin
    Name:='morph';
   end;
   else begin
    Name:='none';
   end;
  end;
  ItemTag.AddParameter('action',Name);

  if TimeLine[Index].Action in [TpvVectorGraphicsTimeLineAction.Move,
                                TpvVectorGraphicsTimeLineAction.Morph] then begin
   case TimeLine[Index].InterpolationMode of
    TpvVectorGraphicsInterpolationMode.Linear:begin
     Name:='linear';
    end;
    TpvVectorGraphicsInterpolationMode.Cosine:begin
     Name:='cosine';
    end;
    TpvVectorGraphicsInterpolationMode.Cubic:begin
     Name:='cubic';
    end;
    TpvVectorGraphicsInterpolationMode.Hermite:begin
     Name:='hermite';
    end;
    else begin
     Name:='nearest';
    end;
   end;
   ItemTag.AddParameter('interpolationmode',Name);
  end;

  case TimeLine[Index].Action of

   TpvVectorGraphicsTimeLineAction.Transform:begin
    WriteSimpleMatrix(ItemTag,TimeLine[Index].TransformSimpleMatrix);
   end;

   TpvVectorGraphicsTimeLineAction.Move:begin

    AddValue('linecolor',TimeLine[Index].HasStrokeColor,FormatColor(TimeLine[Index].StrokeColor));

    ValueTag:=TpvXMLTag.Create;
    ValueTag.Name:='linefillmatrix';
    ItemTag.Add(ValueTag);
    ValueTag.AddParameter('active',FormatBoolean(TimeLine[Index].HasStrokeFillSimpleMatrix));
    WriteSimpleMatrix(ValueTag,TimeLine[Index].StrokeFillSimpleMatrix);

    AddValue('linewidth',TimeLine[Index].HasStrokeWidth,FormatFloat_(TimeLine[Index].StrokeWidth));
    AddValue('fillcolor',TimeLine[Index].HasFillColor,FormatColor(TimeLine[Index].FillColor));

    ValueTag:=TpvXMLTag.Create;
    ValueTag.Name:='fillmatrix';
    ItemTag.Add(ValueTag);
    ValueTag.AddParameter('active',FormatBoolean(TimeLine[Index].HasFillSimpleMatrix));
    WriteSimpleMatrix(ValueTag,TimeLine[Index].FillSimpleMatrix);

    AddValue('alpha',TimeLine[Index].HasAlpha,TpvRawByteString(IntToStr(TimeLine[Index].Alpha)));

   end;

   TpvVectorGraphicsTimeLineAction.Morph:begin
    ValueTag:=TpvXMLTag.Create;
    ValueTag.Name:='shapecommands';
    ItemTag.Add(ValueTag);
    WritePathCommands(ValueTag,TimeLine[Index].MorphPath);
   end;

   else begin
   end;

  end;

 end;

end;

procedure TpvVectorGraphicsBeroshWriter.WriteObjectBase(const aTag:TpvXMLTag;const aObject:TpvVectorGraphicsObject);
var Tag:TpvXMLTag;
    Name:TpvRawByteString;
begin

 Tag:=TpvXMLTag.Create;
 Tag.Name:='name';
 aTag.Add(Tag);
 Tag.AddParameter('value',TpvRawByteString(aObject.Name));

 Tag:=TpvXMLTag.Create;
 Tag.Name:='matrix';
 aTag.Add(Tag);
 WriteSimpleMatrix(Tag,aObject.SimpleMatrix);

 Tag:=TpvXMLTag.Create;
 Tag.Name:='alpha';
 aTag.Add(Tag);
 Tag.AddParameter('value',TpvRawByteString(IntToStr(aObject.Alpha)));

 Tag:=TpvXMLTag.Create;
 Tag.Name:='z';
 aTag.Add(Tag);
 Tag.AddParameter('value',FormatFloat_(aObject.Z));

 Tag:=TpvXMLTag.Create;
 Tag.Name:='offset';
 aTag.Add(Tag);
 Tag.AddParameter('x',FormatFloat_(aObject.OffsetX));
 Tag.AddParameter('y',FormatFloat_(aObject.OffsetY));

 case aObject.MaskMode of
  TpvVectorGraphicsMaskMode.Clip:begin
   Name:='clip';
  end;
  TpvVectorGraphicsMaskMode.Mono:begin
   Name:='mono';
  end;
  TpvVectorGraphicsMaskMode.Color:begin
   Name:='color';
  end;
  else begin
   Name:='none';
  end;
 end;
 Tag:=TpvXMLTag.Create;
 Tag.Name:='mask';
 aTag.Add(Tag);
 Tag.AddParameter('mode',Name);

 Tag:=TpvXMLTag.Create;
 Tag.Name:='flags';
 aTag.Add(Tag);
 Tag.AddParameter('visible',FormatBoolean(aObject.Visible));
 Tag.AddParameter('locked',FormatBoolean(aObject.Locked));
 Tag.AddParameter('outline',FormatBoolean(aObject.Outline));

 WriteTimeLine(aTag,aObject);

end;

procedure TpvVectorGraphicsBeroshWriter.WriteGraphic(const aTag:TpvXMLTag;const aGraphic:TpvVectorGraphicsGraphic);
var Tag,FillTag:TpvXMLTag;
    Style:TpvVectorGraphicsStyle;
    Name:TpvRawByteString;
    Index:TpvSizeInt;
    Pattern:TpvRawByteString;
begin

 WriteObjectBase(aTag,aGraphic);
 Style:=aGraphic.Style;

 Tag:=TpvXMLTag.Create;
 Tag.Name:='settings';
 aTag.Add(Tag);
 Tag.AddParameter('offimage',FormatBoolean(Style.OffImage));

 Tag:=TpvXMLTag.Create;
 Tag.Name:='stroke';
 aTag.Add(Tag);

 case Style.StrokeCapMode of
  TpvVectorPathStrokeCapMode.Round:begin
   Name:='round';
  end;
  TpvVectorPathStrokeCapMode.Square:begin
   Name:='square';
  end;
  else begin
   Name:='butt';
  end;
 end;
 Tag.AddParameter('capmode',Name);

 case Style.StrokeJoinMode of
  TpvVectorPathStrokeJoinMode.Bevel:begin
   Name:='bevel';
  end;
  TpvVectorPathStrokeJoinMode.Round:begin
   Name:='round';
  end;
  TpvVectorPathStrokeJoinMode.MiterRevert:begin
   Name:='miterrevert';
  end;
  TpvVectorPathStrokeJoinMode.MiterRound:begin
   Name:='miterround';
  end;
  else begin
   Name:='miter';
  end;
 end;
 Tag.AddParameter('joinmode',Name);

 case Style.StrokeInnerJoinMode of
  TpvVectorPathStrokeInnerJoinMode.Bevel:begin
   Name:='bevel';
  end;
  TpvVectorPathStrokeInnerJoinMode.Jag:begin
   Name:='jag';
  end;
  TpvVectorPathStrokeInnerJoinMode.Round:begin
   Name:='round';
  end;
  else begin
   Name:='miter';
  end;
 end;
 Tag.AddParameter('innerjoinmode',Name);

 Tag.AddParameter('width',FormatFloat_(Style.StrokeWidth));
 Tag.AddParameter('fixedwidth',FormatBoolean(Style.StrokeWidthFixed));
 Tag.AddParameter('innermiterlimit',FormatFloat_(Style.StrokeInnerMiterLimit));
 Tag.AddParameter('miterlimit',FormatFloat_(Style.StrokeMiterLimit));

 // The dash array goes back out as the pictorial pattern the format uses, with one character
 // per step. A length that is not a whole number of steps is rounded to the nearest one.
 Pattern:='';
 for Index:=0 to Length(Style.StrokeDashArray)-1 do begin
  if (Index and 1)=0 then begin
   Pattern:=Pattern+TpvRawByteString(StringOfChar('-',Max(0,round(Style.StrokeDashArray[Index]))));
  end else begin
   Pattern:=Pattern+TpvRawByteString(StringOfChar(' ',Max(0,round(Style.StrokeDashArray[Index]))));
  end;
 end;
 Tag.AddParameter('pattern',Pattern);
 Tag.AddParameter('patternstepsize','1');
 Tag.AddParameter('patternstart',FormatFloat_(Style.StrokeDashOffset));

 FillTag:=TpvXMLTag.Create;
 FillTag.Name:='fill';
 Tag.Add(FillTag);
 WriteFillData(FillTag,Style.StrokeFillData);

 Tag:=TpvXMLTag.Create;
 Tag.Name:='fill';
 aTag.Add(Tag);
 WriteFillData(Tag,Style.FillData);

end;

procedure TpvVectorGraphicsBeroshWriter.WriteGroup(const aTag:TpvXMLTag;const aGroup:TpvVectorGraphicsGroup);
var Tag,ObjectsTag:TpvXMLTag;
    Index:TpvSizeInt;
begin

 WriteObjectBase(aTag,aGroup);

 Tag:=TpvXMLTag.Create;
 Tag.Name:='id';
 aTag.Add(Tag);
 Tag.AddParameter('value',TpvRawByteString(IntToStr(ClipIDOf(aGroup))));

 Tag:=TpvXMLTag.Create;
 Tag.Name:='settings';
 aTag.Add(Tag);
 Tag.AddParameter('offimage',FormatBoolean(aGroup.OffImage));
 Tag.AddParameter('gamma',FormatFloat_(aGroup.Gamma));
 Tag.AddParameter('looptime',TpvRawByteString(IntToStr(aGroup.LoopTime)));
 Tag.AddParameter('width',FormatFloat_(aGroup.Width));
 Tag.AddParameter('height',FormatFloat_(aGroup.Height));
 Tag.AddParameter('color',FormatColor(aGroup.BackgroundColor));

 ObjectsTag:=TpvXMLTag.Create;
 ObjectsTag.Name:='objects';
 aTag.Add(ObjectsTag);

 for Index:=0 to aGroup.Children.Count-1 do begin
  WriteObject(aGroup.Children[Index],ObjectsTag);
 end;

end;

function TpvVectorGraphicsBeroshWriter.WriteObject(const aObject:TpvVectorGraphicsObject;const aParentTag:TpvXMLTag):boolean;
var Tag,SubTag:TpvXMLTag;
    TextTag:TpvXMLText;
begin

 result:=false;
 if not assigned(aObject) then begin
  exit;
 end;

 if aObject is TpvVectorGraphicsGroup then begin

  Tag:=TpvXMLTag.Create;
  Tag.Name:='movieclip';
  aParentTag.Add(Tag);
  WriteGroup(Tag,TpvVectorGraphicsGroup(aObject));

 end else if aObject is TpvVectorGraphicsGroupInstance then begin

  Tag:=TpvXMLTag.Create;
  Tag.Name:='movieclipinstance';
  aParentTag.Add(Tag);
  WriteObjectBase(Tag,aObject);

  SubTag:=TpvXMLTag.Create;
  SubTag.Name:='movieclip';
  Tag.Add(SubTag);
  SubTag.AddParameter('id',TpvRawByteString(IntToStr(ClipIDOf(TpvVectorGraphicsGroupInstance(aObject).Group))));

 end else if aObject is TpvVectorGraphicsLine then begin

  Tag:=TpvXMLTag.Create;
  Tag.Name:='line';
  aParentTag.Add(Tag);
  WriteGraphic(Tag,TpvVectorGraphicsGraphic(aObject));

  SubTag:=TpvXMLTag.Create;
  SubTag.Name:='startpoint';
  Tag.Add(SubTag);
  SubTag.AddParameter('x',FormatFloat_(TpvVectorGraphicsLine(aObject).x1));
  SubTag.AddParameter('y',FormatFloat_(TpvVectorGraphicsLine(aObject).y1));

  SubTag:=TpvXMLTag.Create;
  SubTag.Name:='endpoint';
  Tag.Add(SubTag);
  SubTag.AddParameter('x',FormatFloat_(TpvVectorGraphicsLine(aObject).x2));
  SubTag.AddParameter('y',FormatFloat_(TpvVectorGraphicsLine(aObject).y2));

 end else if aObject is TpvVectorGraphicsRectangle then begin

  Tag:=TpvXMLTag.Create;
  Tag.Name:='rect';
  aParentTag.Add(Tag);
  WriteGraphic(Tag,TpvVectorGraphicsGraphic(aObject));

  SubTag:=TpvXMLTag.Create;
  SubTag.Name:='position';
  Tag.Add(SubTag);
  SubTag.AddParameter('x',FormatFloat_(TpvVectorGraphicsRectangle(aObject).x));
  SubTag.AddParameter('y',FormatFloat_(TpvVectorGraphicsRectangle(aObject).y));

  SubTag:=TpvXMLTag.Create;
  SubTag.Name:='size';
  Tag.Add(SubTag);
  SubTag.AddParameter('width',FormatFloat_(TpvVectorGraphicsRectangle(aObject).Width));
  SubTag.AddParameter('height',FormatFloat_(TpvVectorGraphicsRectangle(aObject).Height));

  SubTag:=TpvXMLTag.Create;
  SubTag.Name:='round';
  Tag.Add(SubTag);
  SubTag.AddParameter('x',FormatFloat_(TpvVectorGraphicsRectangle(aObject).RadiusX));
  SubTag.AddParameter('y',FormatFloat_(TpvVectorGraphicsRectangle(aObject).RadiusY));

 end else if aObject is TpvVectorGraphicsEllipse then begin

  Tag:=TpvXMLTag.Create;
  Tag.Name:='ellipse';
  aParentTag.Add(Tag);
  WriteGraphic(Tag,TpvVectorGraphicsGraphic(aObject));

  SubTag:=TpvXMLTag.Create;
  SubTag.Name:='center';
  Tag.Add(SubTag);
  SubTag.AddParameter('x',FormatFloat_(TpvVectorGraphicsEllipse(aObject).CenterX));
  SubTag.AddParameter('y',FormatFloat_(TpvVectorGraphicsEllipse(aObject).CenterY));

  SubTag:=TpvXMLTag.Create;
  SubTag.Name:='radius';
  Tag.Add(SubTag);
  SubTag.AddParameter('x',FormatFloat_(TpvVectorGraphicsEllipse(aObject).RadiusX));
  SubTag.AddParameter('y',FormatFloat_(TpvVectorGraphicsEllipse(aObject).RadiusY));

 end else if aObject is TpvVectorGraphicsText then begin

  Tag:=TpvXMLTag.Create;
  Tag.Name:='text';
  aParentTag.Add(Tag);
  WriteGraphic(Tag,TpvVectorGraphicsGraphic(aObject));

  SubTag:=TpvXMLTag.Create;
  SubTag.Name:='font';
  Tag.Add(SubTag);
  SubTag.AddParameter('name',TpvRawByteString(TpvVectorGraphicsText(aObject).FontFamily));
  SubTag.AddParameter('filename','');
  SubTag.AddParameter('thin','false');
  SubTag.AddParameter('bold',FormatBoolean(TpvVectorGraphicsText(aObject).Bold));
  SubTag.AddParameter('italic',FormatBoolean(TpvVectorGraphicsText(aObject).Italic));

  case TpvVectorGraphicsText(aObject).Anchor of
   TpvVectorGraphicsTextAnchor.Middle:begin
    SubTag.AddParameter('textanchorx','middle');
   end;
   TpvVectorGraphicsTextAnchor.End_:begin
    SubTag.AddParameter('textanchorx','right');
   end;
   else begin
    SubTag.AddParameter('textanchorx','left');
   end;
  end;

  case TpvVectorGraphicsText(aObject).Baseline of
   TpvVectorGraphicsTextBaseline.Middle,
   TpvVectorGraphicsTextBaseline.Central:begin
    SubTag.AddParameter('textanchory','middle');
   end;
   TpvVectorGraphicsTextBaseline.Bottom:begin
    SubTag.AddParameter('textanchory','bottom');
   end;
   else begin
    SubTag.AddParameter('textanchory','top');
   end;
  end;

  SubTag.AddParameter('letterspacing',FormatFloat_(TpvVectorGraphicsText(aObject).LetterSpacing));
  SubTag.AddParameter('size',FormatFloat_(TpvVectorGraphicsText(aObject).FontSize));

  SubTag:=TpvXMLTag.Create;
  SubTag.Name:='text';
  Tag.Add(SubTag);
  TextTag:=TpvXMLText.Create;
  TextTag.Text:=TpvXMLString(TpvVectorGraphicsText(aObject).Text);
  SubTag.Add(TextTag);

 end else if aObject is TpvVectorGraphicsGraphic then begin

  // Everything else that has a path is written as a free form shape.
  Tag:=TpvXMLTag.Create;
  Tag.Name:='shape';
  aParentTag.Add(Tag);
  WriteGraphic(Tag,TpvVectorGraphicsGraphic(aObject));

  SubTag:=TpvXMLTag.Create;
  SubTag.Name:='commands';
  Tag.Add(SubTag);
  WritePathCommands(SubTag,TpvVectorGraphicsGraphic(aObject).Path);

 end else begin

  exit;

 end;

 result:=true;

end;

procedure TpvVectorGraphicsBeroshWriter.WriteImage(const aTag:TpvXMLTag;const aImage:TpvVectorGraphicsImageResource);
var Tag:TpvXMLTag;
    Index,Count:TpvSizeInt;
    Pixels:PpvVectorGraphicsRawColorArray;
    Data:TpvRawByteString;
    Color:TpvVectorGraphicsColor;
    Position:TpvSizeInt;
begin

 Tag:=TpvXMLTag.Create;
 Tag.Name:='settings';
 aTag.Add(Tag);
 Tag.AddParameter('id',TpvRawByteString(IntToStr(aImage.ID)));

 Tag:=TpvXMLTag.Create;
 Tag.Name:='size';
 aTag.Add(Tag);
 Tag.AddParameter('width',TpvRawByteString(IntToStr(aImage.Width)));
 Tag.AddParameter('height',TpvRawByteString(IntToStr(aImage.Height)));

 Pixels:=aImage.GetPixelPointer;
 Count:=aImage.Width*aImage.Height;
 if (Count<=0) or not assigned(Pixels) then begin
  exit;
 end;

 // Two hex digits per byte, four bytes per pixel.
 SetLength(Data,Count*8);
 Position:=1;
 for Index:=0 to Count-1 do begin
  Color:=Pixels^[Index];
  Data[Position]:=HexDigits[(Color shr 4) and $f];
  Data[Position+1]:=HexDigits[Color and $f];
  Data[Position+2]:=HexDigits[(Color shr 12) and $f];
  Data[Position+3]:=HexDigits[(Color shr 8) and $f];
  Data[Position+4]:=HexDigits[(Color shr 20) and $f];
  Data[Position+5]:=HexDigits[(Color shr 16) and $f];
  Data[Position+6]:=HexDigits[(Color shr 28) and $f];
  Data[Position+7]:=HexDigits[(Color shr 24) and $f];
  inc(Position,8);
 end;

 Tag:=TpvXMLTag.Create;
 Tag.Name:='pixels';
 aTag.Add(Tag);
 Tag.AddParameter('data',Data);

end;

function TpvVectorGraphicsBeroshWriter.Save(const aDocument:TpvVectorGraphicsDocument;const aXML:TpvXML):boolean;
var ProcessTag:TpvXMLProcessTag;
    CommentTag:TpvXMLCommentTag;
    RootTag,Tag:TpvXMLTag;
    Index:TpvSizeInt;
begin

 result:=false;

 if not (assigned(aDocument) and assigned(aXML)) then begin
  exit;
 end;

 fDocument:=aDocument;
 fClips.Clear;
 fClipIDs.Clear;
 fNextClipID:=1;

 // Give every referenced group an id before anything is written, so that a reference never
 // points at an id that has not been handed out yet.
 CollectClips(aDocument.Root);
 for Index:=0 to aDocument.Defs.Children.Count-1 do begin
  CollectClips(aDocument.Defs.Children[Index]);
 end;

 aXML.Root.Clear;

 ProcessTag:=TpvXMLProcessTag.Create;
 ProcessTag.Name:='xml';
 ProcessTag.AddParameter('version','1.0');
 ProcessTag.AddParameter('encoding','UTF-8');
 aXML.Root.Add(ProcessTag);

 CommentTag:=TpvXMLCommentTag.Create;
 CommentTag.Text:=' Created with Berosh ';
 aXML.Root.Add(CommentTag);

 RootTag:=TpvXMLTag.Create;
 RootTag.Name:='movie';
 aXML.Root.Add(RootTag);

 WriteObjectBase(RootTag,aDocument.Root);

 Tag:=TpvXMLTag.Create;
 Tag.Name:='settings';
 RootTag.Add(Tag);
 Tag.AddParameter('drawbackground',FormatBoolean(aDocument.DrawBackground));
 Tag.AddParameter('lastmovieclipid',TpvRawByteString(IntToStr(fNextClipID)));
 Tag.AddParameter('lastmovieimageid',TpvRawByteString(IntToStr(aDocument.Images.Count)));

 Tag:=TpvXMLTag.Create;
 Tag.Name:='rootmovieclip';
 RootTag.Add(Tag);
 WriteGroup(Tag,aDocument.Root);

 // The standalone clips, which instances and pattern fills point at.
 for Index:=0 to aDocument.Defs.Children.Count-1 do begin
  if aDocument.Defs.Children[Index] is TpvVectorGraphicsGroup then begin
   Tag:=TpvXMLTag.Create;
   Tag.Name:='movieclip';
   RootTag.Add(Tag);
   WriteGroup(Tag,TpvVectorGraphicsGroup(aDocument.Defs.Children[Index]));
  end;
 end;

 for Index:=0 to aDocument.Images.Count-1 do begin
  Tag:=TpvXMLTag.Create;
  Tag.Name:='movieimage';
  RootTag.Add(Tag);
  WriteImage(Tag,aDocument.Images[Index]);
 end;

 result:=true;

end;

function pvVectorGraphicsLoadBeroshFromStream(const aStream:TStream):TpvVectorGraphicsDocument;
var XML:TpvXML;
    Reader:TpvVectorGraphicsBeroshReader;
begin

 result:=nil;
 XML:=TpvXML.Create;
 try

  if not XML.Parse(aStream) then begin
   exit;
  end;

  result:=TpvVectorGraphicsDocument.Create;
  try
   Reader:=TpvVectorGraphicsBeroshReader.Create;
   try
    if not Reader.Load(XML,result) then begin
     FreeAndNil(result);
    end;
   finally
    FreeAndNil(Reader);
   end;
  except
   FreeAndNil(result);
   raise;
  end;

 finally
  FreeAndNil(XML);
 end;

end;

function pvVectorGraphicsLoadBeroshFromFile(const aFileName:string):TpvVectorGraphicsDocument;
var Stream:TFileStream;
begin
 Stream:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
 try
  result:=pvVectorGraphicsLoadBeroshFromStream(Stream);
 finally
  FreeAndNil(Stream);
 end;
end;

function pvVectorGraphicsSaveBeroshToStream(const aDocument:TpvVectorGraphicsDocument;const aStream:TStream):boolean;
var XML:TpvXML;
    Writer:TpvVectorGraphicsBeroshWriter;
begin

 result:=false;
 XML:=TpvXML.Create;
 try

  Writer:=TpvVectorGraphicsBeroshWriter.Create;
  try
   if not Writer.Save(aDocument,XML) then begin
    exit;
   end;
  finally
   FreeAndNil(Writer);
  end;

  XML.FormatIndent:=true;
  result:=XML.Write(aStream);

 finally
  FreeAndNil(XML);
 end;

end;

function pvVectorGraphicsSaveBeroshToFile(const aDocument:TpvVectorGraphicsDocument;const aFileName:string):boolean;
var Stream:TFileStream;
begin
 Stream:=TFileStream.Create(aFileName,fmCreate);
 try
  result:=pvVectorGraphicsSaveBeroshToStream(aDocument,Stream);
 finally
  FreeAndNil(Stream);
 end;
end;

initialization

 // A fixed format setting, so that the numbers written out do not follow whatever decimal
 // separator the machine happens to be configured with.
 BeroshFormatSettings:=DefaultFormatSettings;
 BeroshFormatSettings.DecimalSeparator:='.';
 BeroshFormatSettings.ThousandSeparator:=#0;

end.
