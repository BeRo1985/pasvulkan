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
  PasVulkan.EntityComponentSystem in '..\..\PasVulkan.EntityComponentSystem.pas',
  PasVulkan.Components.RecordBased.Name in '..\..\PasVulkan.Components.RecordBased.Name.pas',
  PasVulkan.Components.RecordBased.Parent in '..\..\PasVulkan.Components.RecordBased.Parent.pas',
  PasVulkan.Components.RecordBased.Renderer in '..\..\PasVulkan.Components.RecordBased.Renderer.pas',
  PasVulkan.Components.RecordBased.SortKey in '..\..\PasVulkan.Components.RecordBased.SortKey.pas',
  PasVulkan.Components.RecordBased.Transform in '..\..\PasVulkan.Components.RecordBased.Transform.pas';

var World:TpvEntityComponentSystem.TWorld;
begin
 try
  World:=TpvEntityComponentSystem.TWorld.Create;
  try

  finally
   FreeAndNil(World);
  end;
  readln;
 except
  on E:Exception do begin
   Writeln(E.ClassName, ': ', E.Message);
  end;
 end;
end.
