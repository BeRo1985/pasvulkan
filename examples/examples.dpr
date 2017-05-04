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

type TExampleVulkanApplication=class(TVulkanApplication)
      public
       function HandleEvent(const pEvent:TSDL_Event):boolean; override;
     end;

function TExampleVulkanApplication.HandleEvent(const pEvent:TSDL_Event):boolean;
begin
 result:=inherited HandleEvent(pEvent);
 if not result then begin
  case pEvent.type_ of
   SDL_KEYDOWN:begin
    case pEvent.key.keysym.sym of
     SDLK_ESCAPE:begin
      if ((pEvent.key.keysym.modifier and ((KMOD_LALT or KMOD_RALT) or (KMOD_LMETA or KMOD_RMETA)))=0) and (pEvent.key.repeat_=0) then begin
       Terminate;
       result:=true;
      end;
     end;
    end;
   end;
  end;
 end;
end;

procedure SDLMain;
begin
 VulkanApplication:=TExampleVulkanApplication.Create;
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
