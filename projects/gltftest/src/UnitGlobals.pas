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
     PasVulkan.VirtualReality;

type TTransparencyMode=
      (
       Auto=0,
       Direct,
       SPINLOCKOIT,
       INTERLOCKOIT,
       LOOPOIT,
       WBOIT,
       MBOIT
      );

     TAntialiasingMode=
      (
       Auto=0,
       None,
       DSAA,
       FXAA,
       SMAA,
       MSAA
      );

     TShadowMode=
      (
       Auto=0,
       None=1,
       PCF=2,
       DPCF=3,
       PCSS=4,
       MSM=5
      );

implementation

end.
