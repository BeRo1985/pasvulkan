unit UnitMain;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$if defined(Win32) or defined(Win64)}
 {$define Windows}
{$ifend}

interface

uses {$if defined(Unix)}
      BaseUnix,Unix,UnixType,ctypes,
     {$ifend}
     SysUtils,Classes,Math,
     Vulkan,
     PasVulkan,
     UnitSDL2,
     UnitVulkanPresentationSurface,
     UnitGlobals;

procedure StartGraphics;
procedure StopGraphics;
procedure ResizeGraphics(NewWidth,NewHeight:TSDLInt32);
procedure DrawGraphics;

implementation

procedure StartGraphics;
begin
end;

procedure StopGraphics;
begin
end;

procedure ResizeGraphics(NewWidth,NewHeight:TSDLInt32);
begin
end;

procedure DrawGraphics;
begin
end;

end.
 