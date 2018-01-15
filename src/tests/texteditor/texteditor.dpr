program texteditor;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$if defined(Win32) or defined(Win64)}
 {$define Windows}
 {$apptype console}
{$endif}

(*{$ifdef Unix}
      cthreads,
     {$endif}*)

uses {$ifdef Unix}
      cthreads,
     {$endif}
     SysUtils,
     Classes,
     PasMP in '..\..\..\externals\pasmp\src\PasMP.pas',
     PUCU in '..\..\..\externals\pucu\src\PUCU.pas',
     PasVulkan.Types in '..\..\PasVulkan.Types.pas',
     PasVulkan.TextEditor in '..\..\PasVulkan.TextEditor.pas',
     UnitConsole in 'UnitConsole.pas',
     UnitMain in 'UnitMain.pas';

begin
 Main;
end.
