program examples;
{$ifdef fpc}
 {$mode delphi}
{$endif} 
{$if defined(win32) or defined(win64)}
 {$apptype console}
{$ifend}

// {$if defined(fpc) and defined(Unix)}cthreads,{$ifend}

uses
  {$if defined(fpc) and defined(Unix)}cthreads,{$ifend}
  SysUtils,
  Classes,
  Vulkan in '..\src\Vulkan.pas',
  PasVulkan in '..\src\PasVulkan.pas',
  PasVulkanSDL2 in '..\src\PasVulkanSDL2.pas',
  PasVulkanStaticLinking in '..\src\PasVulkanStaticLinking.pas',
  PasVulkanApplication in '..\src\PasVulkanApplication.pas',
  UnitScreenExampleTriangle in 'UnitScreenExampleTriangle.pas';

procedure SDLMain;
begin
 VulkanApplication:=TVulkanApplication.Create;
 try
{$ifndef Release}
  VulkanApplication.VulkanDebugging:=true;
  VulkanApplication.VulkanValidation:=true;
{$endif}
  VulkanApplication.Title:='SDL Vulkan Examples Application';
  VulkanApplication.StartScreen:=TScreenExampleTriangle;
  VulkanApplication.Run;
 finally
  FreeAndNil(VulkanApplication);
 end;
end;

begin
 SDLMain;
{$ifndef fpc}
 if DebugHook<>0 then begin
  writeln('Press return to exit . . . ');
  readln;
 end;
{$endif}
end.
