program testlzbrsf;
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

uses SysUtils,
     Classes,
     Math,
     PasVulkan.Compression.LZBRSF;

begin
end.

