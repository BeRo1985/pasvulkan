program entitycomponentsystem;
{$ifdef fpc}
 {$mode delphi}
{$else}
 {$legacyifend on}
{$endif}
{$if defined(Win32) or defined(Win64) or defined(Windows)}
 {$apptype console}
{$ifend}

uses
  SysUtils,
  PasVulkan.Types in '..\..\PasVulkan.Types.pas',
  PasVulkan.Framework in '..\..\PasVulkan.Framework.pas',
  PasVulkan.EntityComponentSystem in '..\..\PasVulkan.EntityComponentSystem.pas';

begin
 try
  readln;
 except
  on E:Exception do begin
   Writeln(E.ClassName, ': ', E.Message);
  end;
 end;
end.
