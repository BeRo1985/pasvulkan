unit UnitGlobals;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$if defined(Win32) or defined(Win64)}
 {$define Windows}
{$ifend}
{$scopedenums on}

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.VirtualReality,
     PasVulkan.Scene3D.Renderer.Globals;

type TTransparencyMode=TpvScene3DRendererTransparencyMode;

     TAntialiasingMode=TpvScene3DRendererAntialiasingMode;

     TShadowMode=TpvScene3DRendererShadowMode;

implementation

end.
