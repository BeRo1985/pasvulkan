unit UnitFormMain;
// Copyright (C) 2017, Benjamin 'BeRo' Rosseaux
// License: zlib

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Math, PasMP;

const BitmapWidth=64;
      BitmapHeight=128;
      BitmapDepth=256; // 0x00 .. 0xff

      SpreadScale=8.0;

type
  PCorrespondingBorderPoint=^TCorrespondingBorderPoint;
  TCorrespondingBorderPoint=record
   x,y:longint;
  end;

  TFormMain = class(TForm)
    ImageBitmap: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Bitmap:array[0..BitmapDepth-1,0..BitmapHeight-1,0..BitmapWidth-1] of boolean;
    CorrespondingBorderPoints:array[0..BitmapDepth-1,0..BitmapHeight-1,0..BitmapWidth-1] of TCorrespondingBorderPoint;
    SignedDistanceField:array[0..BitmapDepth-1,0..BitmapHeight-1,0..BitmapWidth-1] of single;
    SquaredSpreadDistance:single;
    SpreadDistance:single;
    procedure GenerateImageBitmap;
{   procedure GenerateSignedDistanceField;
    procedure GenerateSignedDistanceField2;}
    procedure CreateSignedDistanceFieldChamferDistance;
    procedure CreateSignedDistanceFieldDeadReckoning;
    procedure CreateSignedDistanceFieldDeadReckoningOpt;
    procedure CreateSignedDistanceFieldBruteforceLine(ay,az:longint);
    procedure CreateSignedDistanceFieldBruteforceLineJob(const Job:PPasMPJob;const ThreadIndex:TPasMPInt32;const Data:pointer;const FromIndex,ToIndex:TPasMPNativeInt);
    procedure CreateSignedDistanceFieldBruteforceCharJob(const Job:PPasMPJob;const ThreadIndex:TPasMPInt32;const Data:pointer;const FromIndex,ToIndex:TPasMPNativeInt);
    procedure CreateSignedDistanceFieldBruteforce;
    procedure CreateSignedDistanceFelzenszwalbHuttenlocher;
    procedure CreateSignedDistanceUnknown;
    procedure DumpSignedDistanceField;
    procedure SaveSignedDistanceField;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.GenerateImageBitmap;
type PLongwords=^TLongwords;
     TLongwords=array[0..BitmapWidth-1] of longword;
var x,y,z,w,h:longint;
    b:TBitmap;
    l:PLongwords;
    c:ansichar;
begin
 b:=ImageBitmap.Picture.Bitmap;
 b.PixelFormat:=pf32Bit;
 b.Width:=BitmapWidth;
 b.Height:=BitmapHeight;
 ImageBitmap.Canvas.Brush.Color:=clBlack;
 ImageBitmap.Canvas.Pen.Color:=clWhite;
 ImageBitmap.Canvas.Font.Color:=clWhite;
//ImageBitmap.Canvas.Font.Name:='Perfect DOS VGA 437 Win';
//ImageBitmap.Canvas.Font.Name:='Liberation Mono';
 ImageBitmap.Canvas.Font.Name:='Noto Mono';
 ImageBitmap.Canvas.Font.Height:=BitmapHeight;
 y:=BitmapHeight;
 for z:=0 to BitmapDepth-1 do begin
  c:=AnsiChar(byte(z));
  x:=BitmapHeight;
  repeat
   ImageBitmap.Canvas.Font.Height:=x;
   w:=b.Canvas.TextWidth(c);
   h:=b.Canvas.TextHeight(c);
   if (w<=BitmapWidth) and (h<=BitmapHeight) then begin
    break;
   end else if x>2 then begin
    dec(x);
   end else begin
    break;
   end;
  until false;
  y:=Min(y,x);
 end;
 ImageBitmap.Canvas.Font.Height:=y;
 for z:=0 to BitmapDepth-1 do begin
  c:=AnsiChar(byte(z));
  w:=b.Canvas.TextWidth(c);
  h:=b.Canvas.TextHeight(c);
  b.Canvas.FillRect(Rect(0,0,BitmapWidth,BitmapHeight));
  b.Canvas.TextOut((BitmapWidth-w) div 2,
                   (BitmapHeight-h) div 2,
                   c);
  for y:=0 to BitmapHeight-1 do begin
   l:=b.ScanLine[y];
   for x:=0 to BitmapWidth-1 do begin
    Bitmap[z,y,x]:=(TColor(l^[x])<>clBlack) and (l^[x]<>0);
   end;
  end;
 end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
 SquaredSpreadDistance:=sqr(BitmapWidth)+sqr(BitmapHeight);
 SpreadDistance:=sqrt(SquaredSpreadDistance)*(1.0/SpreadScale);//*sqrt(0.5);
 GenerateImageBitmap;
//CreateSignedDistanceFieldChamferDistance;
//CreateSignedDistanceFieldDeadReckoning;
//CreateSignedDistanceFieldDeadReckoningOpt;
//CreateSignedDistanceFieldBruteforce;
 CreateSignedDistanceFelzenszwalbHuttenlocher;
//CreateSignedDistanceUnknown;
 DumpSignedDistanceField;
 SaveSignedDistanceField;
end;

procedure TFormMain.CreateSignedDistanceFieldChamferDistance;
const DISTANCE_ORTHO=3.0;
      DISTANCE_DIAGONAL=4.0;
var x,y,z:longint;
    c:boolean;
begin
 for z:=0 to BitmapDepth-1 do begin
  for y:=0 to BitmapHeight-1 do begin
   for x:=0 to BitmapWidth-1 do begin
    SignedDistanceField[z,y,x]:=3e+24;
   end;
  end;
  for y:=0 to BitmapHeight-1 do begin
   for x:=0 to BitmapWidth-1 do begin
    c:=Bitmap[z,y,x];
    if (((x-1)>=0) and (Bitmap[z,y,x-1]<>c)) or
       (((x+1)<BitmapWidth) and (Bitmap[z,y,x+1]<>c)) or
       (((y-1)>=0) and (Bitmap[z,y-1,x]<>c)) or
       (((y+1)<BitmapHeight) and (Bitmap[z,y+1,x]<>c)) then begin
     SignedDistanceField[z,y,x]:=0.0;
    end;
   end;
  end;
  for y:=0 to BitmapHeight-1 do begin
   for x:=0 to BitmapWidth-1 do begin
    if ((x-1)>=0) and
       ((y-1)>=0) and
       ((SignedDistanceField[z,y-1,x-1]+DISTANCE_DIAGONAL)<SignedDistanceField[z,y,x]) then begin
     SignedDistanceField[z,y,x]:=SignedDistanceField[z,y-1,x-1]+DISTANCE_DIAGONAL;
    end;
    if ((y-1)>=0) and
       ((SignedDistanceField[z,y-1,x]+DISTANCE_ORTHO)<SignedDistanceField[z,y,x]) then begin
     SignedDistanceField[z,y,x]:=SignedDistanceField[z,y-1,x]+DISTANCE_ORTHO;
    end;
    if ((x+1)<BitmapWidth) and
       ((y-1)>=0) and
       ((SignedDistanceField[z,y-1,x+1]+DISTANCE_DIAGONAL)<SignedDistanceField[z,y,x]) then begin
     SignedDistanceField[z,y,x]:=SignedDistanceField[z,y-1,x+1]+DISTANCE_DIAGONAL;
    end;
    if ((x-1)>=0) and
       ((SignedDistanceField[z,y,x-1]+DISTANCE_ORTHO)<SignedDistanceField[z,y,x]) then begin
     SignedDistanceField[z,y,x]:=SignedDistanceField[z,y,x-1]+DISTANCE_ORTHO;
    end;
   end;
  end;
  for y:=BitmapHeight-1 downto 0 do begin
   for x:=BitmapWidth-1 downto 0 do begin
    if ((x+1)<BitmapWidth) and
       ((SignedDistanceField[z,y,x+1]+DISTANCE_ORTHO)<SignedDistanceField[z,y,x]) then begin
     SignedDistanceField[z,y,x]:=SignedDistanceField[z,y,x+1]+DISTANCE_ORTHO;
    end;
    if ((x-1)>=0) and
       ((y+1)<BitmapHeight) and
       ((SignedDistanceField[z,y+1,x-1]+DISTANCE_DIAGONAL)<SignedDistanceField[z,y,x]) then begin
     SignedDistanceField[z,y,x]:=SignedDistanceField[z,y+1,x-1]+DISTANCE_DIAGONAL;
    end;
    if ((y+1)<BitmapHeight) and
       ((SignedDistanceField[z,y+1,x]+DISTANCE_ORTHO)<SignedDistanceField[z,y,x]) then begin
     SignedDistanceField[z,y,x]:=SignedDistanceField[z,y+1,x]+DISTANCE_ORTHO;
    end;
    if ((x+1)<BitmapWidth) and
       ((y+1)<BitmapHeight) and
       ((SignedDistanceField[z,y+1,x+1]+DISTANCE_DIAGONAL)<SignedDistanceField[z,y,x]) then begin
     SignedDistanceField[z,y,x]:=SignedDistanceField[z,y+1,x+1]+DISTANCE_DIAGONAL;
    end;
   end;
  end;
  for y:=0 to BitmapHeight-1 do begin
   for x:=0 to BitmapWidth-1 do begin
    if Bitmap[z,y,x] then begin
     SignedDistanceField[z,y,x]:=-SignedDistanceField[z,y,x];
    end;
   end;
  end;
  for y:=0 to BitmapHeight-1 do begin
   for x:=0 to BitmapWidth-1 do begin
    SignedDistanceField[z,y,x]:=Min(Max(SignedDistanceField[z,y,x]/SpreadDistance,-1.0),1.0);
   end;
  end;
 end;
end;

procedure TFormMain.CreateSignedDistanceFieldDeadReckoning;
const DISTANCE_ORTHO=1.0;
      DISTANCE_DIAGONAL=1.41421356237; // sqrt(2.0)
var x,y,z:longint;
    c:boolean;
begin
 for z:=0 to BitmapDepth-1 do begin
  for y:=0 to BitmapHeight-1 do begin
   for x:=0 to BitmapWidth-1 do begin
    SignedDistanceField[z,y,x]:=3e+24;
    CorrespondingBorderPoints[z,y,x].x:=-1;
    CorrespondingBorderPoints[z,y,x].y:=-1;
   end;
  end;
  for y:=0 to BitmapHeight-1 do begin
   for x:=0 to BitmapWidth-1 do begin
    c:=Bitmap[z,y,x];
    if (((x-1)>=0) and (Bitmap[z,y,x-1]<>c)) or
       (((x+1)<BitmapWidth) and (Bitmap[z,y,x+1]<>c)) or
       (((y-1)>=0) and (Bitmap[z,y-1,x]<>c)) or
       (((y+1)<BitmapHeight) and (Bitmap[z,y+1,x]<>c)) then begin
     SignedDistanceField[z,y,x]:=0.0;
     CorrespondingBorderPoints[z,y,x].x:=x;
     CorrespondingBorderPoints[z,y,x].y:=y;
    end;
   end;
  end;
  for y:=0 to BitmapHeight-1 do begin
   for x:=0 to BitmapWidth-1 do begin
    if ((x-1)>=0) and
       ((y-1)>=0) and
       ((SignedDistanceField[z,y-1,x-1]+DISTANCE_DIAGONAL)<SignedDistanceField[z,y,x]) then begin
     CorrespondingBorderPoints[z,y,x]:=CorrespondingBorderPoints[z,y-1,x-1];
     SignedDistanceField[z,y,x]:=sqrt(sqr(x-CorrespondingBorderPoints[z,y,x].x)+sqr(y-CorrespondingBorderPoints[z,y,x].y));
    end;
    if ((y-1)>=0) and
       ((SignedDistanceField[z,y-1,x]+DISTANCE_ORTHO)<SignedDistanceField[z,y,x]) then begin
     CorrespondingBorderPoints[z,y,x]:=CorrespondingBorderPoints[z,y-1,x];
     SignedDistanceField[z,y,x]:=sqrt(sqr(x-CorrespondingBorderPoints[z,y,x].x)+sqr(y-CorrespondingBorderPoints[z,y,x].y));
    end;
    if ((x+1)<BitmapWidth) and
       ((y-1)>=0) and
       ((SignedDistanceField[z,y-1,x+1]+DISTANCE_DIAGONAL)<SignedDistanceField[z,y,x]) then begin
     CorrespondingBorderPoints[z,y,x]:=CorrespondingBorderPoints[z,y-1,x+1];
     SignedDistanceField[z,y,x]:=sqrt(sqr(x-CorrespondingBorderPoints[z,y,x].x)+sqr(y-CorrespondingBorderPoints[z,y,x].y));
    end;
    if ((x-1)>=0) and
       ((SignedDistanceField[z,y,x-1]+DISTANCE_ORTHO)<SignedDistanceField[z,y,x]) then begin
     CorrespondingBorderPoints[z,y,x]:=CorrespondingBorderPoints[z,y,x-1];
     SignedDistanceField[z,y,x]:=sqrt(sqr(x-CorrespondingBorderPoints[z,y,x].x)+sqr(y-CorrespondingBorderPoints[z,y,x].y));
    end;
   end;
  end;
  for y:=BitmapHeight-1 downto 0 do begin
   for x:=BitmapWidth-1 downto 0 do begin
    if ((x+1)<BitmapWidth) and
       ((SignedDistanceField[z,y,x+1]+DISTANCE_ORTHO)<SignedDistanceField[z,y,x]) then begin
     CorrespondingBorderPoints[z,y,x]:=CorrespondingBorderPoints[z,y,x+1];
     SignedDistanceField[z,y,x]:=sqrt(sqr(x-CorrespondingBorderPoints[z,y,x].x)+sqr(y-CorrespondingBorderPoints[z,y,x].y));
    end;
    if ((x-1)>=0) and
       ((y+1)<BitmapHeight) and
       ((SignedDistanceField[z,y+1,x-1]+DISTANCE_DIAGONAL)<SignedDistanceField[z,y,x]) then begin
     CorrespondingBorderPoints[z,y,x]:=CorrespondingBorderPoints[z,y+1,x-1];
     SignedDistanceField[z,y,x]:=sqrt(sqr(x-CorrespondingBorderPoints[z,y,x].x)+sqr(y-CorrespondingBorderPoints[z,y,x].y));
    end;
    if ((y+1)<BitmapHeight) and
       ((SignedDistanceField[z,y+1,x]+DISTANCE_ORTHO)<SignedDistanceField[z,y,x]) then begin
     CorrespondingBorderPoints[z,y,x]:=CorrespondingBorderPoints[z,y+1,x];
     SignedDistanceField[z,y,x]:=sqrt(sqr(x-CorrespondingBorderPoints[z,y,x].x)+sqr(y-CorrespondingBorderPoints[z,y,x].y));
    end;
    if ((x+1)<BitmapWidth) and
       ((y+1)<BitmapHeight) and
       ((SignedDistanceField[z,y+1,x+1]+DISTANCE_DIAGONAL)<SignedDistanceField[z,y,x]) then begin
     CorrespondingBorderPoints[z,y,x]:=CorrespondingBorderPoints[z,y+1,x+1];
     SignedDistanceField[z,y,x]:=sqrt(sqr(x-CorrespondingBorderPoints[z,y,x].x)+sqr(y-CorrespondingBorderPoints[z,y,x].y));
    end;
   end;
  end;
  for y:=0 to BitmapHeight-1 do begin
   for x:=0 to BitmapWidth-1 do begin
    if Bitmap[z,y,x] then begin
     SignedDistanceField[z,y,x]:=-SignedDistanceField[z,y,x];
    end;
   end;
  end;
  for y:=0 to BitmapHeight-1 do begin
   for x:=0 to BitmapWidth-1 do begin
    SignedDistanceField[z,y,x]:=Min(Max(SignedDistanceField[z,y,x]/SpreadDistance,-1.0),1.0);
   end;
  end;
 end;
end;

procedure TFormMain.CreateSignedDistanceFieldDeadReckoningOpt;
const DISTANCE_ORTHO=1.0;
      DISTANCE_DIAGONAL=2.0;
var x,y,z:longint;
    c:boolean;
begin
 for z:=0 to BitmapDepth-1 do begin
  for y:=0 to BitmapHeight-1 do begin
   for x:=0 to BitmapWidth-1 do begin
    SignedDistanceField[z,y,x]:=3e+24;
    CorrespondingBorderPoints[z,y,x].x:=-1;
    CorrespondingBorderPoints[z,y,x].y:=-1;
   end;
  end;
  for y:=0 to BitmapHeight-1 do begin
   for x:=0 to BitmapWidth-1 do begin
    c:=Bitmap[z,y,x];
    if (((x-1)>=0) and (Bitmap[z,y,x-1]<>c)) or
       (((x+1)<BitmapWidth) and (Bitmap[z,y,x+1]<>c)) or
       (((y-1)>=0) and (Bitmap[z,y-1,x]<>c)) or
       (((y+1)<BitmapHeight) and (Bitmap[z,y+1,x]<>c)) then begin
     SignedDistanceField[z,y,x]:=0.0;
     CorrespondingBorderPoints[z,y,x].x:=x;
     CorrespondingBorderPoints[z,y,x].y:=y;
    end;
   end;
  end;
  for y:=0 to BitmapHeight-1 do begin
   for x:=0 to BitmapWidth-1 do begin
    if ((x-1)>=0) and
       ((y-1)>=0) and
       ((SignedDistanceField[z,y-1,x-1]+DISTANCE_DIAGONAL)<SignedDistanceField[z,y,x]) then begin
     CorrespondingBorderPoints[z,y,x]:=CorrespondingBorderPoints[z,y-1,x-1];
     SignedDistanceField[z,y,x]:=sqr(x-CorrespondingBorderPoints[z,y,x].x)+sqr(y-CorrespondingBorderPoints[z,y,x].y);
    end;
    if ((y-1)>=0) and
       ((SignedDistanceField[z,y-1,x]+DISTANCE_ORTHO)<SignedDistanceField[z,y,x]) then begin
     CorrespondingBorderPoints[z,y,x]:=CorrespondingBorderPoints[z,y-1,x];
     SignedDistanceField[z,y,x]:=sqr(x-CorrespondingBorderPoints[z,y,x].x)+sqr(y-CorrespondingBorderPoints[z,y,x].y);
    end;
    if ((x+1)<BitmapWidth) and
       ((y-1)>=0) and
       ((SignedDistanceField[z,y-1,x+1]+DISTANCE_DIAGONAL)<SignedDistanceField[z,y,x]) then begin
     CorrespondingBorderPoints[z,y,x]:=CorrespondingBorderPoints[z,y-1,x+1];
     SignedDistanceField[z,y,x]:=sqr(x-CorrespondingBorderPoints[z,y,x].x)+sqr(y-CorrespondingBorderPoints[z,y,x].y);
    end;
    if ((x-1)>=0) and
       ((SignedDistanceField[z,y,x-1]+DISTANCE_ORTHO)<SignedDistanceField[z,y,x]) then begin
     CorrespondingBorderPoints[z,y,x]:=CorrespondingBorderPoints[z,y,x-1];
     SignedDistanceField[z,y,x]:=sqr(x-CorrespondingBorderPoints[z,y,x].x)+sqr(y-CorrespondingBorderPoints[z,y,x].y);
    end;
   end;
  end;
  for y:=BitmapHeight-1 downto 0 do begin
   for x:=BitmapWidth-1 downto 0 do begin
    if ((x+1)<BitmapWidth) and
       ((SignedDistanceField[z,y,x+1]+DISTANCE_ORTHO)<SignedDistanceField[z,y,x]) then begin
     CorrespondingBorderPoints[z,y,x]:=CorrespondingBorderPoints[z,y,x+1];
     SignedDistanceField[z,y,x]:=sqr(x-CorrespondingBorderPoints[z,y,x].x)+sqr(y-CorrespondingBorderPoints[z,y,x].y);
    end;
    if ((x-1)>=0) and
       ((y+1)<BitmapHeight) and
       ((SignedDistanceField[z,y+1,x-1]+DISTANCE_DIAGONAL)<SignedDistanceField[z,y,x]) then begin
     CorrespondingBorderPoints[z,y,x]:=CorrespondingBorderPoints[z,y+1,x-1];
     SignedDistanceField[z,y,x]:=sqr(x-CorrespondingBorderPoints[z,y,x].x)+sqr(y-CorrespondingBorderPoints[z,y,x].y);
    end;
    if ((y+1)<BitmapHeight) and
       ((SignedDistanceField[z,y+1,x]+DISTANCE_ORTHO)<SignedDistanceField[z,y,x]) then begin
     CorrespondingBorderPoints[z,y,x]:=CorrespondingBorderPoints[z,y+1,x];
     SignedDistanceField[z,y,x]:=sqr(x-CorrespondingBorderPoints[z,y,x].x)+sqr(y-CorrespondingBorderPoints[z,y,x].y);
    end;
    if ((x+1)<BitmapWidth) and
       ((y+1)<BitmapHeight) and
       ((SignedDistanceField[z,y+1,x+1]+DISTANCE_DIAGONAL)<SignedDistanceField[z,y,x]) then begin
     CorrespondingBorderPoints[z,y,x]:=CorrespondingBorderPoints[z,y+1,x+1];
     SignedDistanceField[z,y,x]:=sqr(x-CorrespondingBorderPoints[z,y,x].x)+sqr(y-CorrespondingBorderPoints[z,y,x].y);
    end;
   end;
  end;
  for y:=0 to BitmapHeight-1 do begin
   for x:=0 to BitmapWidth-1 do begin
    if abs(SignedDistanceField[z,y,x])>0.0 then begin
     SignedDistanceField[z,y,x]:=sqrt(SignedDistanceField[z,y,x]);
    end;
   end;
  end;
  for y:=0 to BitmapHeight-1 do begin
   for x:=0 to BitmapWidth-1 do begin
    if Bitmap[z,y,x] then begin
     SignedDistanceField[z,y,x]:=-SignedDistanceField[z,y,x];
    end;
   end;
  end;
  for y:=0 to BitmapHeight-1 do begin
   for x:=0 to BitmapWidth-1 do begin
    SignedDistanceField[z,y,x]:=Min(Max(SignedDistanceField[z,y,x]/SpreadDistance,-1.0),1.0);
   end;
  end;
 end;
end;

procedure TFormMain.CreateSignedDistanceFieldBruteforceLine(ay,az:longint);
var ax,bx,by:longint;
    BestDist,Dist:single;
    Inside,First:boolean;
begin
 for ax:=0 to BitmapWidth-1 do begin
  BestDist:=SquaredSpreadDistance;
  Inside:=Bitmap[az,ay,ax];
  First:=true;
  for by:=0 to BitmapHeight-1 do begin
   for bx:=0 to BitmapWidth-1 do begin
    if Bitmap[az,by,bx]<>Inside then begin
//   Dist:=sqr((bx-ax)/BitmapWidth)+sqr((by-ay)/BitmapHeight);
     Dist:=sqr(bx-ax)+sqr(by-ay);
     if First or (BestDist>Dist) then begin
      BestDist:=Dist;
      First:=false;
     end;
    end;
   end;
  end;
  BestDist:=sqrt(BestDist);
  if Inside then begin
   BestDist:=-BestDist;
  end else begin
   BestDist:=+BestDist;
  end;
  SignedDistanceField[az,ay,ax]:=BestDist;
 end;
end;

procedure TFormMain.CreateSignedDistanceFieldBruteforceLineJob(const Job:PPasMPJob;const ThreadIndex:TPasMPInt32;const Data:pointer;const FromIndex,ToIndex:TPasMPNativeInt);
var y,z:longint;
begin
 z:=TPasMPPtrInt(Data);
 for y:=FromIndex to ToIndex do begin
  CreateSignedDistanceFieldBruteforceLine(y,z);
 end;
end;

procedure TFormMain.CreateSignedDistanceFieldBruteforceCharJob(const Job:PPasMPJob;const ThreadIndex:TPasMPInt32;const Data:pointer;const FromIndex,ToIndex:TPasMPNativeInt);
var z:longint;
begin
 for z:=FromIndex to ToIndex do begin
  GlobalPasMP.Invoke(GlobalPasMP.ParallelFor(pointer(TPasMPPtrInt(z)),0,BitmapHeight-1,CreateSignedDistanceFieldBruteforceLineJob));
 end;
end;

procedure TFormMain.CreateSignedDistanceFieldBruteforce;
var x,y,z:longint;
begin
 TPasMP.GetGlobalInstance;
 GlobalPasMP.Invoke(GlobalPasMP.ParallelFor(nil,0,BitmapDepth-1,CreateSignedDistanceFieldBruteforceCharJob));
 for z:=0 to BitmapDepth-1 do begin
  for y:=0 to BitmapHeight-1 do begin
   for x:=0 to BitmapWidth-1 do begin
    SignedDistanceField[z,y,x]:=Min(Max(SignedDistanceField[z,y,x]/SpreadDistance,-1.0),1.0);
   end;
  end;
 end;
end;

procedure TFormMain.CreateSignedDistanceFelzenszwalbHuttenlocher;
const Inf=1e+20;
type TFloats=array of single;
     TTemporaryField=array of array of single;
     TInts=array of longint;
var x,y,z:longint;
    p:boolean;
    f,d,w:TFloats;
    v:TInts;
    TemporaryFields:array[boolean] of TTemporaryField;
 procedure DistanceTransform(const n:longint);
 var k,q:longint;
     s:single;
 begin
  k:=0;
  v[0]:=0;
  w[0]:=-Inf;
  w[1]:=+Inf;
  for q:=1 to n-1 do begin
   s:=((f[q]+sqr(q))-(f[v[k]]+sqr(v[k])))/((2*q)-(2*v[k]));
   while s<=w[k] do begin
    dec(k);
    s:=((f[q]+sqr(q))-(f[v[k]]+sqr(v[k])))/((2*q)-(2*v[k]));
   end;
   inc(k);
   v[k]:=q;
   w[k]:=s;
   w[k+1]:=Inf;
  end;
  k:=0;
  for q:=0 to n-1 do begin
   while w[k+1]<q do begin
    inc(k);
   end;
   d[q]:=sqr(q-v[k])+f[v[k]];
  end;
 end;
begin
 f:=nil;
 d:=nil;
 w:=nil;
 v:=nil;
 TemporaryFields[false]:=nil;
 TemporaryFields[true]:=nil;
 try
  SetLength(f,Max(BitmapWidth,BitmapHeight));
  SetLength(d,Max(BitmapWidth,BitmapHeight));
  SetLength(w,Max(BitmapWidth,BitmapHeight)+1);
  SetLength(v,Max(BitmapWidth,BitmapHeight));
  SetLength(TemporaryFields[false],BitmapHeight,BitmapWidth);
  SetLength(TemporaryFields[true],BitmapHeight,BitmapWidth);
  for z:=0 to BitmapDepth-1 do begin
   for p:=false to true do begin
    for y:=0 to BitmapHeight-1 do begin
     for x:=0 to BitmapWidth-1 do begin
      if Bitmap[z,y,x] xor p then begin
       TemporaryFields[p,y,x]:=0.0;
      end else begin
       TemporaryFields[p,y,x]:=Inf;
      end;
     end;
    end;
    for x:=0 to BitmapWidth-1 do begin
     for y:=0 to BitmapHeight-1 do begin
      f[y]:=TemporaryFields[p,y,x];
     end;
     DistanceTransform(BitmapHeight);
     for y:=0 to BitmapHeight-1 do begin
      TemporaryFields[p,y,x]:=d[y];
     end;
    end;
    for y:=0 to BitmapHeight-1 do begin
     for x:=0 to BitmapWidth-1 do begin
      f[x]:=TemporaryFields[p,y,x];
     end;
     DistanceTransform(BitmapWidth);
     for x:=0 to BitmapWidth-1 do begin
      TemporaryFields[p,y,x]:=d[x];
     end;
    end;
   end;
   for y:=0 to BitmapHeight-1 do begin
    for x:=0 to BitmapWidth-1 do begin
     SignedDistanceField[z,y,x]:=Min(Max((sqrt(TemporaryFields[Bitmap[z,y,x],y,x])*(1-((ord(Bitmap[z,y,x]) and 1) shl 1)))/SpreadDistance,-1.0),1.0);
    end;
   end;
  end;
 finally
  f:=nil;
  d:=nil;
  w:=nil;
  v:=nil;
  TemporaryFields[false]:=nil;
  TemporaryFields[true]:=nil;
 end;
end;

procedure TFormMain.CreateSignedDistanceUnknown;
type PLongints=^TLongints;
     TLongints=array[0..65535] of longint;
var w,h:longint;
    Data,Data2,s,t,rc:PLongints;
 procedure VerticalPass;
 var x,y,b:longint;
 begin
  for x:=0 to w-1 do begin
   b:=1;
   for y:=1 to h-1 do begin
    if Data^[(y*w)+x]>(Data^[((y-1)*w)+x]+b) then begin
     Data^[(y*w)+x]:=Data^[((y-1)*w)+x]+b;
     inc(b,2);
    end else begin
     b:=1;
    end;
   end;
   b:=1;
   for y:=h-2 downto 0 do begin
    if Data^[(y*w)+x]>(Data^[((y+1)*w)+x]+b) then begin
     Data^[(y*w)+x]:=Data^[((y+1)*w)+x]+b;
     inc(b,2);
    end else begin
     b:=1;
    end;
   end;
  end;
 end;
 procedure HorizontalPass;
 var x,y,q,irx,c:longint;
     ir:PLongints;
 begin
  for y:=0 to h-1 do begin
   ir:=pointer(@Data^[y*w]);
   q:=0;
   s^[0]:=0;
   t^[0]:=0;
   for x:=1 to w-1 do begin
    irx:=ir^[x];
    while (q>=0) and ((sqr(t^[q]-s^[q])+ir^[s^[q]])>(sqr(t^[q]-x)+irx)) do begin
     dec(q);
    end;
    if q<0 then begin
     q:=0;
     s^[0]:=x;
    end else begin
     c:=1+((((sqr(x)-sqr(s^[q]))+ir^[x])-ir^[s^[q]]) div (2*(x-s^[q])));
     if c<w then begin
      inc(q);
      s^[q]:=x;
      t^[q]:=c;
     end;
    end;
   end;
   for x:=0 downto w-1 do begin
    rc^[x]:=ir^[x];
   end;
   for x:=w-1 downto 0 do begin
    ir^[x]:=sqr(x-s^[q])+rc^[s^[q]];
    if x=t^[q] then begin
     dec(q);
    end;
   end;
  end;
 end;
var x,y,z,i,v:longint;
    InsideOutsidePass:boolean;
    d,Sign:single;
begin
 w:=BitmapWidth;
 h:=BitmapHeight;
 GetMem(Data,w*h*SizeOf(longint));
 GetMem(Data2,w*h*SizeOf(longint));
 GetMem(s,w*SizeOf(longint));
 GetMem(t,w*SizeOf(longint));
 GetMem(rc,w*SizeOf(longint));
 for z:=0 to BitmapDepth-1 do begin
  for InsideOutsidePass:=false to true do begin
   Move(Data^,Data2^,w*h*SizeOf(longint));
   FillChar(s^,w*SizeOf(longint),AnsiChar(#0));
   FillChar(t^,w*SizeOf(longint),AnsiChar(#0));
   FillChar(rc^,w*SizeOf(longint),AnsiChar(#0));
   v:=$7fffffff-(sqr(w)+sqr(h));
   i:=0;
   for y:=0 to h-1 do begin
    for x:=0 to w-1 do begin
     if Bitmap[z,y,x]=InsideOutsidePass then begin
      Data^[i]:=v;
     end else begin
      Data^[i]:=0;
     end;
     inc(i);
    end;
   end;
   VerticalPass;
   HorizontalPass;
  end;
  i:=0;
  for y:=0 to h-1 do begin
   for x:=0 to w-1 do begin
    if Data2^[i]=0 then begin
     d:=1.0;//Data^[i];
     Sign:=-1;
    end else begin
     d:=Data2^[i];
     Sign:=1;
    end;
    SignedDistanceField[z,y,x]:=min(max(sqrt(d)/(w*h),0.0),1.0)*Sign;
    inc(i);
   end;
  end;
 end;
 FreeMem(rc);
 FreeMem(t);
 FreeMem(s);
 FreeMem(Data2);
 FreeMem(Data);
end;

procedure TFormMain.DumpSignedDistanceField;
type PLongwords=^TLongwords;
     TLongwords=array[0..1023] of longword;
var x,y,lx,ly:longint;
    b:TBitmap;
    l:PLongwords;
begin
 b:=ImageBitmap.Picture.Bitmap;
 b.Width:=BitmapWidth*16;
 b.Height:=BitmapHeight*16;
 for ly:=0 to 15 do begin
  for lx:=0 to 15 do begin
   for y:=0 to BitmapHeight-1 do begin
    l:=b.ScanLine[(ly*BitmapHeight)+y];
    for x:=0 to BitmapWidth-1 do begin
     l^[(lx*BitmapWidth)+x]:=$010101*Min(Max(round(128+(SignedDistanceField[(ly shl 4) or lx,y,x]*128)),0),255);
    end;
   end;
  end;
 end;
 ImageBitmap.Picture.SaveToFile('sdffont.bmp');
end;

procedure TFormMain.SaveSignedDistanceField;
var x,y,z,i:longint;
    t:textfile;
begin
 AssignFile(t,'../../UnitSDFFont.pas');
 Rewrite(t);
 writeln(t,'unit UnitSDFFont;');
 writeln(t,'interface');
 writeln(t,'const SDFFontWidth=',BitmapWidth,';');
 writeln(t,'      SDFFontHeight=',BitmapHeight,';');
 writeln(t,'      SDFFontDepth=',BitmapDepth,';');
 writeln(t,'      SDFFontSpreadScale=',SpreadScale:1:8,';');
 writeln(t,'      SDFFontData:array[0..',(BitmapWidth*BitmapHeight*BitmapDepth)-1,'] of byte=(');
 write(t,'       ');
 i:=0;
 for z:=0 to BitmapDepth-1 do begin
  for y:=0 to BitmapHeight-1 do begin
   for x:=0 to BitmapWidth-1 do begin
//  write(t,Min(Max(round(SignedDistanceField[z,y,x]*32768)+32768,0),65535):5);
    write(t,Min(Max(round(SignedDistanceField[z,y,x]*128)+128,0),255):3);
//  write(t,SignedDistanceField[z,y,x]:14:8);
    if not ((x=(BitmapWidth-1)) and (y=(BitmapHeight-1)) and (z=(BitmapDepth-1))) then begin
     write(t,',');
     inc(i);
     if i>31 then begin
      writeln(t);
      write(t,'       ');
      i:=0;
     end;
    end;
   end;
  end;
 end;
 writeln(t,');');
 writeln(t,'implementation');
 writeln(t,'end.');
 CloseFile(t);
end;

end.
