(******************************************************************************
 *                              PasVulkanApplication                          *
 ******************************************************************************
 *                        Version 2017-05-04-05-07-0000                       *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2017, Benjamin Rosseaux (benjamin@rosseaux.de)          *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 *                                                                            *
 ******************************************************************************
 *                  General guidelines for code contributors                  *
 *============================================================================*
 *                                                                            *
 * 1. Make sure you are legally allowed to make a contribution under the zlib *
 *    license.                                                                *
 * 2. The zlib license header goes at the top of each source file, with       *
 *    appropriate copyright notice.                                           *
 * 3. This PasVulkan wrapper may be used only with the PasVulkan-own Vulkan   *
 *    Pascal header.                                                          *
 * 4. After a pull request, check the status of your pull request on          *
      http://github.com/BeRo1985/pasvulkan                                    *
 * 5. Write code, which is compatible with Delphi 7-XE7 and FreePascal >= 3.0 *
 *    so don't use generics/templates, operator overloading and another newer *
 *    syntax features than Delphi 7 has support for that, but if needed, make *
 *    it out-ifdef-able.                                                      *
 * 6. Don't use Delphi-only, FreePascal-only or Lazarus-only libraries/units, *
 *    but if needed, make it out-ifdef-able.                                  *
 * 7. No use of third-party libraries/units as possible, but if needed, make  *
 *    it out-ifdef-able.                                                      *
 * 8. Try to use const when possible.                                         *
 * 9. Make sure to comment out writeln, used while debugging.                 *
 * 10. Make sure the code compiles on 32-bit and 64-bit platforms (x86-32,    *
 *     x86-64, ARM, ARM64, etc.).                                             *
 * 11. Make sure the code runs on all platforms with Vulkan support           *
 *                                                                            *
 ******************************************************************************)
unit PasVulkanApplication;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
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
     PasVulkanSDL2;

type EVulkanApplication=class(Exception);

     TVulkanApplication=class;

     TVulkanApplicationOnEvent=function(const pApplication:TVulkanApplication;const pEvent:TSDL_Event):boolean of object;

     TVulkanApplicationOnStep=procedure(const pApplication:TVulkanApplication) of object;

     TVulkanApplication=class
      private

       fTitle:string;
       fVersion:TVkUInt32;

       fCurrentWidth:TSDLInt32;
       fCurrentHeight:TSDLInt32;
       fCurrentFullscreen:TSDLInt32;
       fCurrentVSync:TSDLInt32;
       fCurrentVisibleMouseCursor:TSDLInt32;
       fCurrentCatchMouse:TSDLInt32;
       fCurrentActive:TSDLInt32;

       fWidth:TSDLInt32;
       fHeight:TSDLInt32;
       fFullscreen:boolean;
       fVSync:boolean;
       fResizable:boolean;
       fVisibleMouseCursor:boolean;
       fCatchMouse:boolean;

       fActive:boolean;

       fTerminated:boolean;

       fSDLDisplayMode:TSDL_DisplayMode;
       fSurfaceWindow:PSDL_Window;
       fEvent:TSDL_Event;

       fScreenWidth:TSDLInt32;
       fScreenHeight:TSDLInt32;

       fVideoFlags:TSDLUInt32;

       fResetGraphics:boolean;

       fGraphicsReady:boolean;

       fOnEvent:TVulkanApplicationOnEvent;

       fOnStep:TVulkanApplicationOnStep;

       procedure Activate;
       procedure Deactivate;

      protected

       procedure AllocateVulkanInstance;
       procedure FreeVulkanInstance;

       procedure AllocateVulkanSurface;
       procedure FreeVulkanSurface;

       procedure StartGraphics;
       procedure StopGraphics;

      public

       constructor Create; reintroduce;
       destructor Destroy; override;
       procedure Initialize;
       procedure Terminate;
       procedure ProcessMessages;
       procedure Run;

      published
       property Title:string read fTitle write fTitle;
       property Version:TVkUInt32 read fVersion write fVersion;
       property Width:TSDLInt32 read fWidth write fWidth;
       property Height:TSDLInt32 read fHeight write fHeight;
       property Fullscreen:boolean read fFullscreen write fFullscreen;
       property VSync:boolean read fVSync write fVSync;
       property Resizable:boolean read fResizable write fResizable;
       property VisibleMouseCursor:boolean read fVisibleMouseCursor write fVisibleMouseCursor;
       property CatchMouse:boolean read fCatchMouse write fCatchMouse;
       property Active:boolean read fActive;
       property Terminated:boolean read fTerminated;
       property OnEvent:TVulkanApplicationOnEvent read fOnEvent write fOnEvent;
       property OnStep:TVulkanApplicationOnStep read fOnStep write fOnStep;
     end;

var VulkanApplication:TVulkanApplication=nil;

implementation

{$if defined(Unix)}
procedure signal_handler(pSignal:cint); cdecl;
begin
 case pSignal of
  SIGINT,SIGTERM,SIGKILL:begin
   if assigned(VulkanApplication) then begin
    VulkanApplication.Terminate;
   end;
  end;
 end;
end;

procedure InstallSignalHandlers;
begin
 fpsignal(SIGTERM,signal_handler);
 fpsignal(SIGINT,signal_handler);
 fpsignal(SIGHUP,signalhandler(SIG_IGN));
 fpsignal(SIGCHLD,signalhandler(SIG_IGN));
 fpsignal(SIGPIPE,signalhandler(SIG_IGN));
 fpsignal(SIGALRM,signalhandler(SIG_IGN));
 fpsignal(SIGWINCH,signalhandler(SIG_IGN));
end;
{$ifend}

constructor TVulkanApplication.Create;
begin

 SDL_SetMainReady;

 SDL_SetHint(SDL_HINT_WINDOWS_DISABLE_THREAD_NAMING,'1');

 inherited Create;

 fTitle:='SDL2 Vulkan Application';
 fVersion:=$0100;

 fCurrentWidth:=-1;
 fCurrentHeight:=-1;
 fCurrentFullscreen:=-1;
 fCurrentVSync:=-1;
 fCurrentVisibleMouseCursor:=-1;
 fCurrentCatchMouse:=-1;
 fCurrentActive:=-1;

 fWidth:=1280;
 fHeight:=720;
 fFullscreen:=false;
 fVSync:=false;
 fResizable:=true;
 fVisibleMouseCursor:=false;
 fCatchMouse:=false;

 fActive:=true;

 fTerminated:=false;

 fResetGraphics:=false;

 fGraphicsReady:=false;

 fOnEvent:=nil;

 VulkanApplication:=self;

end;

destructor TVulkanApplication.Destroy;
begin
 VulkanApplication:=nil;
 inherited Destroy;
end;

procedure TVulkanApplication.AllocateVulkanInstance;
begin
end;

procedure TVulkanApplication.FreeVulkanInstance;
begin
end;

procedure TVulkanApplication.AllocateVulkanSurface;
begin
end;

procedure TVulkanApplication.FreeVulkanSurface;
begin
end;

procedure TVulkanApplication.StartGraphics;
begin
end;

procedure TVulkanApplication.StopGraphics;
begin
end;

procedure TVulkanApplication.Initialize;
begin
end;

procedure TVulkanApplication.Terminate;
begin
 fTerminated:=true;
end;

procedure TVulkanApplication.Activate;
begin
 if not fGraphicsReady then begin
  try
   AllocateVulkanSurface;
   StartGraphics;
   fGraphicsReady:=true;
  except
   Terminate;
   raise;
  end;
 end;
end;

procedure TVulkanApplication.Deactivate;
begin
 if fGraphicsReady then begin
  StopGraphics;
  FreeVulkanSurface;
  fGraphicsReady:=false;
 end;
end;

procedure TVulkanApplication.ProcessMessages;
begin

 while SDL_PollEvent(@fEvent)<>0 do begin
  if assigned(fOnEvent) and fOnEvent(self,fEvent) then begin
   continue;
  end;
  case fEvent.type_ of
   SDL_QUITEV,
   SDL_APP_TERMINATING:begin
    fActive:=false;
    Terminate;
    break;
   end;
   SDL_APP_WILLENTERBACKGROUND:begin
    fActive:=false;
   end;
   SDL_APP_DIDENTERFOREGROUND:begin
    fActive:=true;
   end;
   SDL_RENDER_TARGETS_RESET,
   SDL_RENDER_DEVICE_RESET:begin
    fResetGraphics:=true;
   end;
   SDL_KEYDOWN:begin
    case fEvent.key.keysym.sym of
     SDLK_F4:begin
      if (fEvent.key.keysym.modifier and ((KMOD_LALT or KMOD_RALT) or (KMOD_LMETA or KMOD_RMETA)))<>0 then begin
       Terminate;
       break;
      end;
     end;
     SDLK_RETURN:begin
      if ((fEvent.key.keysym.modifier and ((KMOD_LALT or KMOD_RALT) or (KMOD_LMETA or KMOD_RMETA)))<>0) and (fEvent.key.repeat_=0) then begin
       fFullScreen:=not fFullScreen;
      end;
     end;
     SDLK_SPACE:begin
     end;
    end;
   end;
   SDL_KEYUP:begin
   end;
   SDL_WINDOWEVENT:begin
    case fEvent.window.event of
     SDL_WINDOWEVENT_RESIZED:begin
      fWidth:=fEvent.window.Data1;
      fHeight:=fEvent.window.Data2;
//    VulkanPresentationSurface.SetSize(WindowWidth,WindowHeight);
//    Main.ResizeGraphics(WindowWidth,WindowHeight);
     end;
    end;
   end;
   SDL_MOUSEMOTION:begin
   end;
   SDL_MOUSEBUTTONDOWN:begin
   end;
   SDL_MOUSEBUTTONUP:begin
   end;
   SDL_JOYDEVICEADDED:begin
   end;
   SDL_JOYDEVICEREMOVED:begin
   end;
   SDL_CONTROLLERDEVICEADDED:begin
   end;
   SDL_CONTROLLERDEVICEREMOVED:begin
   end;
   SDL_CONTROLLERDEVICEREMAPPED:begin
   end;
  end;
 end;

 if assigned(fOnStep) then begin
  fOnStep(self);
 end;
 
 if fCurrentFullScreen<>ord(fFullScreen) then begin
  fCurrentFullScreen:=ord(fFullScreen);
  if fFullScreen then begin
   SDL_SetWindowFullscreen(fSurfaceWindow,SDL_WINDOW_FULLSCREEN_DESKTOP);
  end else begin
   SDL_SetWindowFullscreen(fSurfaceWindow,0);
  end;
  //fResetGraphics:=true;
 end;

 if fResetGraphics then begin
  fResetGraphics:=false;
  if fActive then begin
   Deactivate;
   Activate;
  end;
 end;

 if fCurrentActive<>ord(fActive) then begin
  fCurrentActive:=ord(fActive);
  if fActive then begin
   Activate;
  end else begin
   Deactivate;
  end;
 end;

 if fGraphicsReady then begin

 end;

end;

procedure TVulkanApplication.Run;
begin

 if SDL_Init(SDL_INIT_VIDEO or SDL_INIT_EVENTS or SDL_INIT_TIMER)<0 then begin
  raise EVulkanApplication.Create('Unable to initialize SDL: '+SDL_GetError);
 end;

{$ifdef Unix}
 InstallSignalHandlers;
{$endif}

 if SDL_GetCurrentDisplayMode(0,@fSDLDisplayMode)=0 then begin
  fScreenWidth:=fSDLDisplayMode.w;
  fScreenHeight:=fSDLDisplayMode.h;
 end else begin
  fScreenWidth:=-1;
  fScreenHeight:=-1;
 end;

 fVideoFlags:=0;
 if fFullscreen then begin
  if (fWidth=fScreenWidth) and (fHeight=fScreenHeight) then begin
   fVideoFlags:=fVideoFlags or SDL_WINDOW_FULLSCREEN_DESKTOP;
  end else begin
   fVideoFlags:=fVideoFlags or SDL_WINDOW_FULLSCREEN;
  end;
  fCurrentFullscreen:=1;
  fFullscreen:=true;
 end;
 if fResizable then begin
  fVideoFlags:=fVideoFlags or SDL_WINDOW_RESIZABLE;
 end;

 fSurfaceWindow:=SDL_CreateWindow(PAnsiChar(fTitle),
                                  ((fScreenWidth-fWidth)+1) div 2,
                                  ((fScreenHeight-fHeight)+1) div 2,
                                  fWidth,
                                  fHeight,
                                  SDL_WINDOW_SHOWN or fVideoFlags);
 if not assigned(fSurfaceWindow) then begin
  raise EVulkanApplication.Create('Unable to initialize SDL: '+SDL_GetError);
 end;

 try

  Activate;
  try

   while not fTerminated do begin
    ProcessMessages;
   end;

  finally
   Deactivate;
  end;

 finally

  if assigned(fSurfaceWindow) then begin
   SDL_DestroyWindow(fSurfaceWindow);
   fSurfaceWindow:=nil;
  end;

 end;

end;

end.
