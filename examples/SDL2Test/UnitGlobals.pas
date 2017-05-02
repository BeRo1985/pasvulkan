unit UnitGlobals;
{$ifdef fpc}
 {$mode delphi}
{$endif}

interface

uses UnitSDL2,Vulkan,PasVulkan;

const ApplicationTitle='Vulkan SDL2Test';
      ApplicationVersion='1.0';
      ApplicationVersionNumber=$010000;
      ApplicationCopyright='Copyright (C) 2017, Benjamin ''BeRo'' Rosseaux';

      ApplicationWindowTitle:pansichar=ApplicationTitle+' '+ApplicationVersion;

var StartFullscreen:boolean=false;
    StartWidth:TSDLInt32=1280;
    StartHeight:TSDLInt32=720;
    StartAntialiasing:TSDLInt32=0;
    StartUSteps:TSDLInt32=32;
    StartVSteps:TSDLInt32=32;
    StartWSteps:TSDLInt32=32;
    StartDuration:TSDLInt32=0;
    StartVSync:boolean=true;

    VulkanInstance:TVulkanInstance=nil;
    VulkanDevice:TVulkanDevice=nil;

    VulkanDebugging:boolean=true;

    VulkanBlock:boolean=true;

    VulkanEnableDebugging:boolean=false;

    VulkanValidation:boolean={$ifdef VulkanNoValidaten}false{$else}true{$endif};


implementation

end.
