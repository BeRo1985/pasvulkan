program sdffontgen;

uses
  Forms,
  UnitFormMain in 'UnitFormMain.pas' {FormMain},
  PasVulkan in '..\..\PasVulkan.pas',
  Vulkan in '..\..\Vulkan.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
