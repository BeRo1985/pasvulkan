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
unit PasVulkan.CSG.BSP;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

{$ifdef Delphi2009AndUp}
 {$warn DUPLICATE_CTOR_DTOR off}
{$endif}

{$undef UseDouble}
{$ifdef UseDouble}
 {$define NonSIMD}
{$endif}

{-$define NonSIMD}

{$ifdef NonSIMD}
 {$undef SIMD}
{$else}
 {$ifdef cpu386}
  {$if not (defined(Darwin) or defined(CompileForWithPIC))}
   {$define SIMD}
  {$ifend}
 {$endif}
 {$ifdef cpux64}
  {$define SIMD}
 {$endif}
{$endif}

{$warnings off}

interface

uses SysUtils,Classes,Math,PasVulkan.Types,PasVulkan.Math;

type PpvCSGBSPClassification=^TpvCSGBSPClassification;
     TpvCSGBSPClassification=
      (
       Coplanar=0,
       Front=1,
       Back=2,
       Spanning=3
      );

     PpvCSGBSPVertex=^TpvCSGBSPVertex;
     TpvCSGBSPVertex=record
      public
       Position:TpvVector3;
       Normal:TpvVector3;
       TexCoord:TpvVector2;
       Color:TpvVector4;
     end;

     PpvCSGBSPPolygonVertices=^TpvCSGBSPPolygonVertices;
     TpvCSGBSPPolygonVertices=record
      public
       Vertices:array of TpvCSGBSPVertex;
       CountVertices:TpvSizeInt;
       procedure Initialize;
       procedure Deinitialize;
       function Clone:TpvCSGBSPPolygonVertices;
       procedure Add(const aVertex:TpvCSGBSPVertex);
     end;

     PpvCSGBSPPolygon=^TpvCSGBSPPolygon;
     TpvCSGBSPPolygon=record
      public
       Vertices:TpvCSGBSPPolygonVertices;
       Plane:TpvPlane;
       procedure Initialize;
       procedure Deinitialize;
       procedure CalculateProperties;
       procedure Flip;
       function Clone:TpvCSGBSPPolygon;
       function ClassifyVertex(const aVertex:TpvVector3):TpvCSGBSPClassification;
       function ClassifySide(const aPolygon:TpvCSGBSPPolygon):TpvCSGBSPClassification;
     end;

     TpvCSGBSPPolygons=array of TpvCSGBSPPolygon;

     PpvCSGBSPPolygonList=^TpvCSGBSPPolygonList;
     TpvCSGBSPPolygonList=record
      public
       Polygons:TpvCSGBSPPolygons;
       CountPolygons:TpvSizeInt;
       procedure Initialize;
       procedure Deinitialize;
       function Clone:TpvCSGBSPPolygonList;
       procedure Add(const aPolygon:TpvCSGBSPPolygon);
     end;

     TpvCSGBSPNode=class
      public
       PolygonList:TpvCSGBSPPolygonList;
       Divider:TpvCSGBSPPolygon;
       HasDivider:boolean;
       FrontNode:TpvCSGBSPNode;
       BackNode:TpvCSGBSPNode;
       constructor Create(const aInputPolygonList:TpvCSGBSPPolygonList);
       destructor Destroy; override;
       function IsConvex:boolean;
       procedure Build(const aInputPolygonList:TpvCSGBSPPolygonList);
       procedure AllPolygons(var aOutputPolygonList:TpvCSGBSPPolygonList);
       function Clone:TpvCSGBSPNode;
       function Invert:TpvCSGBSPNode;
       procedure ClipPolygons(const aInputPolygonList:TpvCSGBSPPolygonList;var aOutputPolygonList:TpvCSGBSPPolygonList);
       procedure ClipTo(const aNode:TpvCSGBSPNode);
     end;

     TpvCSGBSPTree=class
      public
       Root:TpvCSGBSPNode;
       constructor Create(const aInputPolygonList:TpvCSGBSPPolygonList); overload;
       constructor Create(const aNode:TpvCSGBSPNode); overload;
       destructor Destroy; override;
       function Subtract(aOtherTree:TpvCSGBSPTree):TpvCSGBSPNode;
       function Union(aOtherTree:TpvCSGBSPTree):TpvCSGBSPNode;
       function Intersection(aOtherTree:TpvCSGBSPTree):TpvCSGBSPNode;
       function Difference(aOtherTree:TpvCSGBSPTree):TpvCSGBSPNode;
       function Invert:TpvCSGBSPNode;
       procedure GetTrianglePolygons(out aOutputPolygonList:TpvCSGBSPPolygonList);
     end;

     TpvCSGBSP=class
      public
       class procedure PolygonSplit(const aThis,aPolygon:TpvCSGBSPPolygon;var aCoplanarFrontList,aCoplanarBackList,aFrontList,aBackList:TpvCSGBSPPolygonList); static;
     end;

implementation

const EPSILON=1e-6;

procedure TpvCSGBSPPolygonVertices.Initialize;
begin
 Vertices:=nil;
 CountVertices:=0;
end;

procedure TpvCSGBSPPolygonVertices.Deinitialize;
begin
 SetLength(Vertices,0);
 CountVertices:=0;
end;

function TpvCSGBSPPolygonVertices.Clone:TpvCSGBSPPolygonVertices;
begin
 result.Initialize;
 result.Vertices:=copy(Vertices);
 result.CountVertices:=CountVertices;
end;

procedure TpvCSGBSPPolygonVertices.Add(const aVertex:TpvCSGBSPVertex);
begin
 if (CountVertices+1)>length(Vertices) then begin
  SetLength(Vertices,(CountVertices+1)*2);
 end;
 Vertices[CountVertices]:=aVertex;
 inc(CountVertices);
end;

procedure TpvCSGBSPPolygon.Initialize;
begin
 Vertices.Initialize;
end;

procedure TpvCSGBSPPolygon.Deinitialize;
begin
 Vertices.Deinitialize;
end;

procedure TpvCSGBSPPolygonList.Initialize;
begin
 Polygons:=nil;
 CountPolygons:=0;
end;

procedure TpvCSGBSPPolygon.CalculateProperties;
begin
 Plane.Normal:=(Vertices.Vertices[2].Position-Vertices.Vertices[0].Position).Cross(Vertices.Vertices[1].Position-Vertices.Vertices[0].Position).Normalize;
 Plane.Distance:=-Plane.Normal.Dot(Vertices.Vertices[0].Position);
end;

procedure TpvCSGBSPPolygon.Flip;
var i:TpvSizeInt;
    Vertex:TpvCSGBSPVertex;
begin
 for i:=0 to (Vertices.CountVertices shr 1)-1 do begin
  Vertex:=Vertices.Vertices[i];
  Vertices.Vertices[i]:=Vertices.Vertices[Vertices.CountVertices-(i+1)];
  Vertices.Vertices[Vertices.CountVertices-(i+1)]:=Vertex;
 end;
 Plane.Normal:=-Plane.Normal;
 Plane.Distance:=-Plane.Distance;
end;

function TpvCSGBSPPolygon.Clone:TpvCSGBSPPolygon;
begin
 result.Initialize;
 result.Vertices:=Vertices.Clone;
 result.Plane:=Plane;
end;

function TpvCSGBSPPolygon.ClassifyVertex(const aVertex:TpvVector3):TpvCSGBSPClassification;
var SideValue:single;
begin
 SideValue:=Plane.Normal.Dot(aVertex)+Plane.Distance;
 if SideValue<(-EPSILON) then begin
  result:=TpvCSGBSPClassification.Back;
 end else if SideValue>EPSILON then begin
  result:=TpvCSGBSPClassification.Front;
 end else begin
  result:=TpvCSGBSPClassification.Coplanar;
 end;
end;

function TpvCSGBSPPolygon.ClassifySide(const aPolygon:TpvCSGBSPPolygon):TpvCSGBSPClassification;
var i,Positive,Negative:TpvSizeInt;
begin
 Positive:=0;
 Negative:=0;
 for i:=0 to aPolygon.Vertices.CountVertices-1 do begin
  case ClassifyVertex(aPolygon.Vertices.Vertices[i].Position) of
   TpvCSGBSPClassification.Back:begin
    inc(Negative);
   end;
   TpvCSGBSPClassification.Front:begin
    inc(Positive);
   end;
  end;
 end;
 if (Positive>0) and (Negative=0) then begin
  result:=TpvCSGBSPClassification.Front;
 end else if (Positive=0) and (Negative>0) then begin
  result:=TpvCSGBSPClassification.Back;
 end else if (Positive=0) and (Negative=0) then begin
  result:=TpvCSGBSPClassification.Coplanar;
 end else begin
  result:=TpvCSGBSPClassification.Spanning;
 end;
end;

class procedure TpvCSGBSP.PolygonSplit(const aThis,aPolygon:TpvCSGBSPPolygon;var aCoplanarFrontList,aCoplanarBackList,aFrontList,aBackList:TpvCSGBSPPolygonList);
var i,j:TpvSizeInt;
    ci,cj:TpvCSGBSPClassification;
    t:single;
    fv,bv:TpvCSGBSPPolygonVertices;
    vi,vj:PpvCSGBSPVertex;
    v:TpvCSGBSPVertex;
    p:TpvCSGBSPPolygon;
begin
 case aThis.ClassifySide(aPolygon) of
  TpvCSGBSPClassification.Coplanar:begin
   if aThis.Plane.Normal.Dot(aPolygon.Plane.Normal)>0.0 then begin
    aCoplanarFrontList.Add(aPolygon);
   end else begin
    aCoplanarBackList.Add(aPolygon);
   end;
  end;
  TpvCSGBSPClassification.Front:begin
   aFrontList.Add(aPolygon);
  end;
  TpvCSGBSPClassification.Back:begin
   aBackList.Add(aPolygon);
  end;
  else {TpvCSGBSPClassification.Spanning:}begin
   fv.Initialize;
   bv.Initialize;
   try
    for i:=0 to aPolygon.Vertices.CountVertices-1 do begin
     j:=i+1;
     if j>=aPolygon.Vertices.CountVertices then begin
      j:=0;
     end;
     vi:=@aPolygon.Vertices.Vertices[i];
     vj:=@aPolygon.Vertices.Vertices[j];
     ci:=aThis.ClassifyVertex(vi^.Position);
     cj:=aThis.ClassifyVertex(vj^.Position);
     if ci<>TpvCSGBSPClassification.Back then begin
      fv.Add(vi^);
     end;
     if ci<>TpvCSGBSPClassification.Front then begin
      bv.Add(vi^);
     end;
{$undef ReferenceCheckImplementation}
{$ifdef ReferenceCheckImplementation}
    if (ci=TpvCSGBSPClassification.Spanning) or
        (cj=TpvCSGBSPClassification.Spanning) or
        ((ci=TpvCSGBSPClassification.Back) and (cj=TpvCSGBSPClassification.Front)) or
        ((ci=TpvCSGBSPClassification.Front) and (cj=TpvCSGBSPClassification.Back)) then begin
{$else}
     if TpvCSGBSPClassification(TpvInt32(TpvInt32(ci) or TpvInt32(cj)))=TpvCSGBSPClassification.Spanning then begin
{$endif}
      t:=(aThis.Plane.Normal.Dot(vi^.Position)+aThis.Plane.Distance)/aThis.Plane.Normal.Dot(vj^.Position-vi^.Position);
      v.Position:=vi^.Position.Lerp(vj^.Position,t);
      v.Normal:=vi^.Normal.Lerp(vj^.Normal,t);
      v.TexCoord:=vi^.TexCoord.Lerp(vj^.TexCoord,t);
      v.Color:=vi^.Color.Lerp(vj^.Color,t);
      fv.Add(v);
      bv.Add(v);
     end;
    end;
    if fv.CountVertices>=3 then begin
     p.Vertices:=fv.Clone;
     p.CalculateProperties;
     aFrontList.Add(p);
    end;
    if bv.CountVertices>=3 then begin
     p.Vertices:=bv.Clone;
     p.CalculateProperties;
     aBackList.Add(p);
    end;
   finally
    fv.Deinitialize;
    bv.Deinitialize;
   end;
  end;
 end;
end;

procedure TpvCSGBSPPolygonList.Deinitialize;
var i:TpvSizeInt;
begin
 for i:=0 to CountPolygons-1 do begin
  Polygons[i].Deinitialize;
 end;
 SetLength(Polygons,0);
 CountPolygons:=0;
end;

function TpvCSGBSPPolygonList.Clone:TpvCSGBSPPolygonList;
var i:TpvSizeInt;
begin
 result.Initialize;
 for i:=0 to CountPolygons-1 do begin
  result.Add(Polygons[i].Clone);
 end;
end;

procedure TpvCSGBSPPolygonList.Add(const aPolygon:TpvCSGBSPPolygon);
begin
 if (CountPolygons+1)>length(Polygons) then begin
  SetLength(Polygons,(CountPolygons+1)*2);
 end;
 Polygons[CountPolygons]:=aPolygon;
 inc(CountPolygons);
end;

constructor TpvCSGBSPNode.Create(const aInputPolygonList:TpvCSGBSPPolygonList);
var i:TpvSizeInt;
    FrontList,BackList:TpvCSGBSPPolygonList;
begin
 inherited Create;
 PolygonList.Initialize;
 FillChar(Divider,SizeOf(TpvCSGBSPPolygon),AnsiChar(#0));
 HasDivider:=false;
 FrontNode:=nil;
 BackNode:=nil;
 FrontList.Initialize;
 BackList.Initialize;
 try
  if aInputPolygonList.CountPolygons>0 then begin
   Divider:=aInputPolygonList.Polygons[0].Clone;
   HasDivider:=true;
   for i:=0 to aInputPolygonList.CountPolygons-1 do begin
    TpvCSGBSP.PolygonSplit(Divider,aInputPolygonList.Polygons[i],PolygonList,PolygonList,FrontList,BackList);
   end;
   if FrontList.CountPolygons>0 then begin
    FrontNode:=TpvCSGBSPNode.Create(FrontList);
   end;
   if BackList.CountPolygons>0 then begin
    BackNode:=TpvCSGBSPNode.Create(BackList);
   end;
  end;
 finally
  FrontList.Deinitialize;
  BackList.Deinitialize;
 end;
end;

destructor TpvCSGBSPNode.Destroy;
begin
 PolygonList.Deinitialize;
 Divider.Deinitialize;
 FreeAndNil(FrontNode);
 FreeAndNil(BackNode);
 inherited Destroy;
end;

function TpvCSGBSPNode.IsConvex:boolean;
var i,j:TpvSizeInt;
begin
 result:=true;
 for i:=0 to PolygonList.CountPolygons-1 do begin
  for j:=0 to PolygonList.CountPolygons-1 do begin
   if (i<>j) and (PolygonList.Polygons[i].ClassifySide(PolygonList.Polygons[j])<>TpvCSGBSPClassification.Back) then begin
    result:=false;
    exit;
   end;
  end;
 end;
end;

procedure TpvCSGBSPNode.Build(const aInputPolygonList:TpvCSGBSPPolygonList);
var i:TpvSizeInt;
    FrontList,BackList:TpvCSGBSPPolygonList;
begin
 FrontList.Initialize;
 BackList.Initialize;
 try
  if aInputPolygonList.CountPolygons>0 then begin
   if not HasDivider then begin
    Divider:=aInputPolygonList.Polygons[0].Clone;
   end;
   for i:=0 to aInputPolygonList.CountPolygons-1 do begin
    TpvCSGBSP.PolygonSplit(Divider,aInputPolygonList.Polygons[i],PolygonList,PolygonList,FrontList,BackList);
   end;
   if FrontList.CountPolygons>0 then begin
    if assigned(FrontNode) then begin
     FrontNode.Build(FrontList);
    end else begin
     FrontNode:=TpvCSGBSPNode.Create(FrontList);
    end;
   end;
   if BackList.CountPolygons>0 then begin
    if assigned(BackNode) then begin
     BackNode.Build(BackList);
    end else begin
     BackNode:=TpvCSGBSPNode.Create(BackList);
    end;
   end;
  end;
 finally
  FrontList.Deinitialize;
  BackList.Deinitialize;
 end;
end;

procedure TpvCSGBSPNode.AllPolygons(var aOutputPolygonList:TpvCSGBSPPolygonList);
var i:TpvSizeInt;
begin
 for i:=0 to PolygonList.CountPolygons-1 do begin
  aOutputPolygonList.Add(PolygonList.Polygons[i].Clone);
 end;
 if assigned(FrontNode) then begin
  FrontNode.AllPolygons(aOutputPolygonList);
 end;
 if assigned(BackNode) then begin
  BackNode.AllPolygons(aOutputPolygonList);
 end;
end;

function TpvCSGBSPNode.Clone:TpvCSGBSPNode;
var EmptyList:TpvCSGBSPPolygonList;
begin
 EmptyList.Initialize;
 try
  result:=TpvCSGBSPNode.Create(EmptyList);
  result.PolygonList:=PolygonList.Clone;
  result.Divider:=Divider.Clone;
  result.HasDivider:=HasDivider;
  if assigned(FrontNode) then begin
   result.FrontNode:=FrontNode.Clone;
  end else begin
   result.FrontNode:=nil;
  end;
  if assigned(BackNode) then begin
   result.BackNode:=BackNode.Clone;
  end else begin
   result.BackNode:=nil;
  end;
 finally
  EmptyList.Deinitialize;
 end;
end;

function TpvCSGBSPNode.Invert:TpvCSGBSPNode;
var i:TpvSizeInt;
    Temp:TpvCSGBSPNode;
begin
 for i:=0 to PolygonList.CountPolygons-1 do begin
  PolygonList.Polygons[i].Flip;
 end;
 Divider.Flip;
 if assigned(FrontNode) then begin
  FrontNode.Invert;
 end;
 if assigned(BackNode) then begin
  BackNode.Invert;
 end;
 Temp:=FrontNode;
 FrontNode:=BackNode;
 BackNode:=Temp;
 result:=self;
end;

procedure TpvCSGBSPNode.ClipPolygons(const aInputPolygonList:TpvCSGBSPPolygonList;var aOutputPolygonList:TpvCSGBSPPolygonList);
var i:TpvSizeInt;
    FrontList,BackList,TempList:TpvCSGBSPPolygonList;
begin
 FrontList.Initialize;
 BackList.Initialize;
 TempList.Initialize;
 try
  if HasDivider then begin
   for i:=0 to aInputPolygonList.CountPolygons-1 do begin
    TpvCSGBSP.PolygonSplit(Divider,aInputPolygonList.Polygons[i],PolygonList,PolygonList,FrontList,BackList);
   end;
   if assigned(FrontNode) then begin
    TempList:=FrontList.Clone;
    FrontList.Deinitialize;
    FrontNode.ClipPolygons(TempList,FrontList);
   end;
   if assigned(BackNode) then begin
    TempList:=BackList.Clone;
    BackList.Deinitialize;
    BackNode.ClipPolygons(TempList,BackList);
   end else begin
    BackList.Deinitialize;
   end;
   for i:=0 to FrontList.CountPolygons-1 do begin
    aOutputPolygonList.Add(FrontList.Polygons[i]);
   end;
   for i:=0 to BackList.CountPolygons-1 do begin
    aOutputPolygonList.Add(BackList.Polygons[i]);
   end;
  end else begin
   for i:=0 to aInputPolygonList.CountPolygons-1 do begin
    aOutputPolygonList.Add(aInputPolygonList.Polygons[i]);
   end;
  end;
 finally
  FrontList.Deinitialize;
  BackList.Deinitialize;
  TempList.Deinitialize;
 end;
end;

procedure TpvCSGBSPNode.ClipTo(const aNode:TpvCSGBSPNode);
var TempList:TpvCSGBSPPolygonList;
begin
 TempList.Initialize;
 try
  TempList:=PolygonList.Clone;
  PolygonList.Deinitialize;
  aNode.ClipPolygons(TempList,PolygonList);
 finally
  TempList.Deinitialize;
 end;
 if assigned(FrontNode) then begin
  FrontNode.ClipTo(aNode);
 end;
 if assigned(BackNode) then begin
  BackNode.ClipTo(aNode);
 end;
end;

constructor TpvCSGBSPTree.Create(const aInputPolygonList:TpvCSGBSPPolygonList);
begin
 inherited Create;
 Root:=TpvCSGBSPNode.Create(aInputPolygonList);
end;

constructor TpvCSGBSPTree.Create(const aNode:TpvCSGBSPNode);
begin
 inherited Create;
 Root:=aNode;
end;

destructor TpvCSGBSPTree.Destroy;
begin
 FreeAndNil(Root);
 inherited Destroy;
end;

function TpvCSGBSPTree.Subtract(aOtherTree:TpvCSGBSPTree):TpvCSGBSPNode;
var a,b:TpvCSGBSPNode;
    TempList:TpvCSGBSPPolygonList;
begin
 b:=nil;
 TempList.Initialize;
 try
  a:=Root.Clone;
  b:=aOtherTree.Root.Clone;
  a.Invert;
  a.ClipTo(b);
  b.ClipTo(a);
  b.Invert;
  b.ClipTo(a);
  b.Invert;
  b.AllPolygons(TempList);
  a.Build(TempList);
  a.Invert;
  result:=a;
 finally
  TempList.Deinitialize;
  b.Free;
 end;
end;

function TpvCSGBSPTree.Union(aOtherTree:TpvCSGBSPTree):TpvCSGBSPNode;
var a,b:TpvCSGBSPNode;
    TempList:TpvCSGBSPPolygonList;
begin
 b:=nil;
 TempList.Initialize;
 try
  a:=Root.Clone;
  b:=aOtherTree.Root.Clone;
  a.ClipTo(b);
  b.ClipTo(a);
  b.Invert;
  b.ClipTo(a);
  b.Invert;
  b.AllPolygons(TempList);
  a.Build(TempList);
  result:=a;
 finally
  TempList.Deinitialize;
  b.Free;
 end;
end;

function TpvCSGBSPTree.Intersection(aOtherTree:TpvCSGBSPTree):TpvCSGBSPNode;
var a,b:TpvCSGBSPNode;
    TempList:TpvCSGBSPPolygonList;
begin
 b:=nil;
 TempList.Initialize;
 try
  a:=Root.Clone;
  b:=aOtherTree.Root.Clone;
  a.Invert;
  b.ClipTo(a);
  b.Invert;
  a.ClipTo(a);
  b.ClipTo(a);
  b.AllPolygons(TempList);
  a.Build(TempList);
  a.Invert;
  result:=a;
 finally
  TempList.Deinitialize;
  b.Free;
 end;
end;

function TpvCSGBSPTree.Difference(aOtherTree:TpvCSGBSPTree):TpvCSGBSPNode;
var a,b:TpvCSGBSPNode;
    TempList:TpvCSGBSPPolygonList;
begin
 a:=nil;
 b:=nil;
 TempList.Initialize;
 try
  a:=Root.Clone;
  b:=aOtherTree.Root.Clone;

  a.ClipTo(b);
  b.ClipTo(a);
  b.Invert;
  b.ClipTo(a);
  b.Invert;
  b.AllPolygons(TempList);
  a.Build(TempList);

  b.Free;
  
  b:=aOtherTree.Root.Clone;
  b.ClipTo(a);
  a.ClipTo(b);
  a.Invert;
  a.ClipTo(b);
  a.Invert;
  a.AllPolygons(TempList);
  b.Build(TempList);

  result:=b.Clone;
 finally
  TempList.Deinitialize;
  a.Free;
  b.Free;
 end;
end;

function TpvCSGBSPTree.Invert:TpvCSGBSPNode;
begin
 result:=Root.Clone.Invert;
end;

procedure TpvCSGBSPTree.GetTrianglePolygons(out aOutputPolygonList:TpvCSGBSPPolygonList);
var i,j:TpvSizeInt;
    TempList:TpvCSGBSPPolygonList;
    Polygon:TpvCSGBSPPolygon;
begin
 TempList.Initialize;
 Polygon.Initialize;
 try
  Root.AllPolygons(TempList);
  SetLength(Polygon.Vertices.Vertices,3);
  Polygon.Vertices.CountVertices:=3;
  for i:=0 to TempList.CountPolygons-1 do begin
   for j:=2 to TempList.Polygons[i].Vertices.CountVertices-1 do begin
    Polygon.Vertices.Vertices[0]:=TempList.Polygons[i].Vertices.Vertices[0];
    Polygon.Vertices.Vertices[1]:=TempList.Polygons[i].Vertices.Vertices[j-1];
    Polygon.Vertices.Vertices[2]:=TempList.Polygons[i].Vertices.Vertices[j];
    Polygon.CalculateProperties;
    aOutputPolygonList.Add(Polygon.Clone);
   end;
  end;
 finally
  Polygon.Deinitialize;
  TempList.Deinitialize;
 end;
end;

end.
