unit UnitFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PasVulkan;

type
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    VulkanTrueTypeFont:TVulkanTrueTypeFont;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
var fs:TStream;
begin
 fs:=TFileStream.Create('droidsans.ttf',fmOpenRead or fmShareDenyNone);
 try
  VulkanTrueTypeFont:=TVulkanTrueTypeFont.Create(fs);
 finally
  fs.Free;
 end;   
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
 VulkanTrueTypeFont.Free;
end;

end.
