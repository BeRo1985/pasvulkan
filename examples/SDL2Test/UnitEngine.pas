unit UnitEngine;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$if defined(Win32) or defined(Win64)}
 {$define Windows}
{$ifend}

interface

uses SysUtils,
     Classes,
     Vulkan,
     PasVulkan;

type TEngine=class
     end;

implementation

end.
