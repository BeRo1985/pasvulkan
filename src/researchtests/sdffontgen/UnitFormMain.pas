unit UnitFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DoIt;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses Vulkan,PasVulkan;

procedure TFormMain.FormCreate(Sender: TObject);
begin
 {}
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
 {}
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
 DoIt;
end;

procedure TFormMain.DoIt;
var GlyphIndex,x0,y0,x1,y1:TVkInt32;
    VulkanTrueTypeFont:TVulkanTrueTypeFont;
    Stream:TStream;
    GlyphBuffer:TVulkanTrueTypeFontGlyphBuffer;
    PolygonBuffer:TVulkanTrueTypeFontPolygonBuffer;
begin
 Stream:=TFileStream.Create('droidsans.ttf',fmOpenRead or fmShareDenyNone);
 try
  VulkanTrueTypeFont:=TVulkanTrueTypeFont.Create(Stream);
 finally
  Stream.Free;
 end;
 try
  if VulkanTrueTypeFont.NumGlyphs>0 then begin

   GlyphIndex:=VulkanTrueTypeFont.GetGlyphIndex(TVkUInt8(TVkChar('A')));

   VulkanTrueTypeFont.ResetGlyphBuffer(GlyphBuffer);
   VulkanTrueTypeFont.FillGlyphBuffer(GlyphBuffer,GlyphIndex);

   VulkanTrueTypeFont.ResetPolygonBuffer(PolygonBuffer);
   VulkanTrueTypeFont.FillPolygonBuffer(PolygonBuffer,GlyphBuffer);

   VulkanTrueTypeFont.GetPolygonBufferBounds(PolygonBuffer,x0,y0,x1,y1);

  end;
 finally
  VulkanTrueTypeFont.Free;
 end;
end;

end.
