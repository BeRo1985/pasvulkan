program sdffontgen;

uses
  Forms,
  UnitFormMain in 'UnitFormMain.pas' {FormMain},
  PasMP in '..\..\..\externals\pasmp\src\PasMP.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
