program SDL2Test;
{$ifdef fpc}
 {$mode delphi}
{$endif} 
{$if defined(win32) or defined(win64)}
 {$apptype console}
{$ifend}

uses
  UnitSDL2 in 'UnitSDL2.pas',
  UnitStaticLinking in 'UnitStaticLinking.pas',
  UnitGlobals in 'UnitGlobals.pas',
  Vulkan in '..\..\src\Vulkan.pas',
  PasVulkan in '..\..\src\PasVulkan.pas',
  UnitVulkanPresentationSurface in 'UnitVulkanPresentationSurface.pas',
  UnitSDL2Main in 'UnitSDL2Main.pas',
  UnitMain in 'UnitMain.pas';

begin
 SDLMain;
{$ifndef fpc}
 if DebugHook<>0 then begin
  writeln('Press return to exit . . . ');
  readln;
 end;
{$endif}
end.
