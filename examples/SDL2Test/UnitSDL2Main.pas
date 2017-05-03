unit UnitSDL2Main;
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

procedure CreateVulkanDevice(const Surface:TVulkanSurface=nil);

procedure VulkanDebugLn(const What:string);

procedure SDLMain;

implementation

uses UnitMain; 

var SDLDisplayMode:TSDL_DisplayMode;
    SurfaceWindow:PSDL_Window;
    Event:TSDL_Event;

    ScreenWidth:TSDLInt32=1280;
    ScreenHeight:TSDLInt32=720;

    WindowWidth:TSDLInt32=1280;
    WindowHeight:TSDLInt32=720;

    VideoFlags:TSDLUInt32;
    Fullscreen:boolean=false;

    SDLRunning:longbool=true;

    GraphicsReady:longbool=false;

procedure DoLogError(const What,Where:ansistring);
begin
 WriteLn(System.ErrOutput,Where,': ',What);
end;

var VulkanDebugLnLast:string='';

procedure VulkanDebugLn(const What:string);
begin
{if VulkanDebugLnLast<>What then begin
  VulkanDebugLnLast:=What;
  WriteLn(What);
 end;}
 WriteLn(What);
end;

type TVulkanDebug=class
      function OnDebugReportCallback(const flags:TVkDebugReportFlagsEXT;const objectType:TVkDebugReportObjectTypeEXT;const object_:TVkUInt64;const location:TVkSize;messageCode:TVkInt32;const pLayerPrefix:TVulkaNCharString;const pMessage:TVulkanCharString):TVkBool32;
     end;

function TVulkanDebug.OnDebugReportCallback(const flags:TVkDebugReportFlagsEXT;const objectType:TVkDebugReportObjectTypeEXT;const object_:TVkUInt64;const location:TVkSize;messageCode:TVkInt32;const pLayerPrefix:TVulkaNCharString;const pMessage:TVulkanCharString):TVkBool32;
begin
 VulkanDebugLn('[Debug] '+pLayerPrefix+': '+pMessage);
 result:=VK_FALSE;
end;

var VulkanDebug:TVulkanDebug=nil;

procedure CreateVulkanDevice(const Surface:TVulkanSurface=nil);
begin
 if not assigned(VulkanDevice) then begin
  VulkanDevice:=TVulkanDevice.Create(VulkanInstance,nil,Surface,nil);
  VulkanDevice.AddQueues;
  VulkanDevice.EnabledExtensionNames.Add(VK_KHR_SWAPCHAIN_EXTENSION_NAME);
  VulkanDevice.Initialize;
 end;
end;

procedure AllocateVulkanInstance;
var i:TVkInt32;
    SDL_SysWMinfo:TSDL_SysWMinfo;
begin
 if not assigned(VulkanInstance) then begin
  SDL_VERSION(SDL_SysWMinfo.version);
  if SDL_GetWindowWMInfo(SurfaceWindow,@SDL_SysWMinfo)<>0 then begin
   VulkanInstance:=TVulkanInstance.Create(ApplicationTitle,ApplicationVersionNumber,
                                          ApplicationTitle,ApplicationVersionNumber,
                                          VK_API_VERSION_1_0,false,nil);
   for i:=0 to VulkanInstance.AvailableLayerNames.Count-1 do begin
    VulkanDebugLn('Layer: '+VulkanInstance.AvailableLayerNames[i]);
   end;
   for i:=0 to VulkanInstance.AvailableExtensionNames.Count-1 do begin
    VulkanDebugLn('Extension: '+VulkanInstance.AvailableExtensionNames[i]);
   end;
   VulkanInstance.EnabledExtensionNames.Add(VK_KHR_SURFACE_EXTENSION_NAME);
   case SDL_SysWMinfo.subsystem of
{$if defined(Android)}
    SDL_SYSWM_ANDROID:begin
     VulkanInstance.EnabledExtensionNames.Add(VK_KHR_ANDROID_SURFACE_EXTENSION_NAME);
    end;
{$ifend}
{$if defined(Mir) and defined(Unix)}
    SDL_SYSWM_MIR:begin
     VulkanInstance.EnabledExtensionNames.Add(VK_KHR_MIR_SURFACE_EXTENSION_NAME);
    end;
{$ifend}
{$if defined(Wayland) and defined(Unix)}
    SDL_SYSWM_WAYLAND:begin
     VulkanInstance.EnabledExtensionNames.Add(VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME);
    end;
{$ifend}
{$if defined(Windows)}
    SDL_SYSWM_WINDOWS:begin
     VulkanInstance.EnabledExtensionNames.Add(VK_KHR_WIN32_SURFACE_EXTENSION_NAME);
    end;
{$ifend}
{$if (defined(XLIB) or defined(XCB)) and defined(Unix)}
    SDL_SYSWM_X11:begin
{$if defined(XLIB) and defined(Unix)}
     VulkanInstance.EnabledExtensionNames.Add(VK_KHR_XLIB_SURFACE_EXTENSION_NAME);
{$elseif defined(XCB) and defined(Unix)}
     VulkanInstance.EnabledExtensionNames.Add(VK_KHR_XCB_SURFACE_EXTENSION_NAME);
{$ifend}
    end;
{$ifend}
    else begin
     raise EVulkanException.Create('Vulkan initialization failure');
    end;
   end;
   if VulkanDebugging and
      (VulkanInstance.AvailableExtensionNames.IndexOf(VK_EXT_DEBUG_REPORT_EXTENSION_NAME)>=0) then begin
    VulkanInstance.EnabledExtensionNames.Add(VK_EXT_DEBUG_REPORT_EXTENSION_NAME);
    VulkanEnableDebugging:=true;
    if VulkanValidation then begin
     if VulkanInstance.AvailableLayerNames.IndexOf('VK_LAYER_LUNARG_standard_validation')>=0 then begin
      VulkanInstance.EnabledLayerNames.Add('VK_LAYER_LUNARG_standard_validation');
     end;
    end;
   end else begin
    VulkanEnableDebugging:=false;
   end;
   VulkanInstance.Initialize;
   if VulkanEnableDebugging then begin
    VulkanInstance.OnInstanceDebugReportCallback:=VulkanDebug.OnDebugReportCallback;
    VulkanInstance.InstallDebugReportCallback;
   end;
  end;
 end;
end;

procedure FreeVulkanInstance;
begin
 FreeAndNil(VulkanPresentationSurface);
 FreeAndNil(VulkanDevice);
 FreeAndNil(VulkanInstance);
 VulkanPresentationSurface:=nil;
 VulkanDevice:=nil;
 VulkanInstance:=nil;
end;

procedure AllocateVulkanSurface;
var SDL_SysWMinfo:TSDL_SysWMinfo;
    VulkanSurfaceCreateInfo:TVulkanSurfaceCreateInfo;
begin
 if not assigned(VulkanPresentationSurface) then begin
  SDL_VERSION(SDL_SysWMinfo.version);
  if SDL_GetWindowWMInfo(SurfaceWindow,@SDL_SysWMinfo)<>0 then begin
   FillChar(VulkanSurfaceCreateInfo,SizeOf(TVulkanSurfaceCreateInfo),#0);
   case SDL_SysWMinfo.subsystem of
{$if defined(Android)}
    SDL_SYSWM_ANDROID:begin
     VulkanSurfaceCreateInfo.Android.sType:=VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR;
     VulkanSurfaceCreateInfo.Android.window:=SDL_SysWMinfo.Window;
    end;
{$ifend}
{$if defined(Mir) and defined(Unix)}
    SDL_SYSWM_MIR:begin
     VulkanSurfaceCreateInfo.Mir.sType:=VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR;
     VulkanSurfaceCreateInfo.Mir.connection:=SDL_SysWMinfo.Mir.Connection;
     VulkanSurfaceCreateInfo.Mir.mirSurface:=SDL_SysWMinfo.Mir.Surface;
    end;
{$ifend}
{$if defined(Wayland) and defined(Unix)}
    SDL_SYSWM_WAYLAND:begin
     VulkanSurfaceCreateInfo.Wayland.sType:=VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR;
     VulkanSurfaceCreateInfo.Wayland.display:=SDL_SysWMinfo.Wayland.Display;
     VulkanSurfaceCreateInfo.Wayland.surface:=SDL_SysWMinfo.Wayland.surface;
    end;
{$ifend}
{$if defined(Windows)}
    SDL_SYSWM_WINDOWS:begin
     VulkanSurfaceCreateInfo.Win32.sType:=VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
     VulkanSurfaceCreateInfo.Win32.hwnd_:=SDL_SysWMinfo.Window;
    end;
{$ifend}
{$if defined(XLIB) and defined(Unix)}
    SDL_SYSWM_X11:begin
     VulkanSurfaceCreateInfo.XLIB.sType:=VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR;
     VulkanSurfaceCreateInfo.XLIB.Dpy:=SDL_SysWMinfo.X11.Display;
     VulkanSurfaceCreateInfo.XLIB.Window:=SDL_SysWMinfo.X11.Window;
    end;
{$ifend}
{$if (defined(XCB) and not defined(XLIB)) and defined(Unix)}
    SDL_SYSWM_X11:begin
     raise EVulkanException.Create('Vulkan initialization failure');
     exit;
    end;
{$ifend}
    else begin
     raise EVulkanException.Create('Vulkan initialization failure');
     exit;
    end;
   end;
   VulkanPresentationSurface:=TVulkanPresentationSurface.Create(WindowWidth,
                                                                WindowHeight,
                                                                StartVSync,
                                                                VulkanSurfaceCreateInfo);
  end;
 end;
end;

procedure FreeVulkanSurface;
begin
 if assigned(VulkanPresentationSurface) then begin
  VulkanPresentationSurface.Free;
  VulkanPresentationSurface:=nil;
 end;
end;

{$if defined(Unix)}
procedure signal_handler(pSignal:cint); cdecl;
begin
 case pSignal of
  SIGINT,SIGTERM,SIGKILL:begin
   TPasMPInterlocked.Write(SDLRunning,TPasMPBool32(false));
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

procedure SDLMainError;
begin
 DoLogError('Could not initialize SDL : '+SDL_GetError,'Main');
end;

procedure SDLMain;
//var i:TSDLInt32;
begin

 WindowWidth:=StartWidth;
 WindowHeight:=StartHeight;

 SDL_SetMainReady;

 SDL_SetHint(SDL_HINT_WINDOWS_DISABLE_THREAD_NAMING,'1');

 if SDL_Init(SDL_INIT_VIDEO or SDL_INIT_EVENTS or SDL_INIT_TIMER)<0 then begin
  SDLMainError;
  exit;
 end;

{$if defined(Unix)}
 InstallSignalHandlers;
{$ifend}

 if SDL_GetCurrentDisplayMode(0,@SDLDisplayMode)=0 then begin
  ScreenWidth:=SDLDisplayMode.w;
  ScreenHeight:=SDLDisplayMode.h;
 end;

 VideoFlags:=0;
 if StartFullscreen then begin
  if (WindowWidth=SDLDisplayMode.w) and (WindowHeight=SDLDisplayMode.h) then begin
   VideoFlags:=VideoFlags or SDL_WINDOW_FULLSCREEN_DESKTOP;
  end else begin
   VideoFlags:=VideoFlags or SDL_WINDOW_FULLSCREEN;
  end;
  Fullscreen:=true;
 end;

 SurfaceWindow:=SDL_CreateWindow(ApplicationWindowTitle,(ScreenWidth-WindowWidth) div 2,(ScreenHeight-WindowHeight) div 2,WindowWidth,WindowHeight,SDL_WINDOW_SHOWN or SDL_WINDOW_RESIZABLE or VideoFlags);
 if not assigned(SurfaceWindow) then begin
  DoLogError('Unable to initialize SDL','Main');
  exit;
 end;

 SDL_ShowCursor(1);

 VulkanDebug:=TVulkanDebug.Create;
 try

  AllocateVulkanInstance;

  Main:=nil;
  try
   AllocateVulkanSurface;

   Main.StartGraphics;
   GraphicsReady:=true;

   VulkanPresentationSurface.SetSize(WindowWidth,WindowHeight);
   Main.ResizeGraphics(WindowWidth,WindowHeight);

   while SDLRunning do begin

    while SDL_PollEvent(@Event)<>0 do begin
     case Event.type_ of
      SDL_QUITEV,SDL_APP_TERMINATING:begin
       SDLRunning:=false;
       break;
      end;
      SDL_APP_WILLENTERBACKGROUND:begin
       if GraphicsReady then begin
        try
         Main.StopGraphics;
         FreeVulkanSurface;
        except
        end;
        GraphicsReady:=false;
       end;
       //SDL_PauseAudio(1);
      end;
      SDL_APP_DIDENTERFOREGROUND:begin
       if not GraphicsReady then begin
        try
         AllocateVulkanSurface;
         Main.StartGraphics;
        except
         SDLRunning:=false;
         break;
        end;
        GraphicsReady:=true;
       end;
       //SDL_PauseAudio(0);
      end;
      SDL_RENDER_TARGETS_RESET,SDL_RENDER_DEVICE_RESET:begin
       if GraphicsReady then begin
        try
         Main.StopGraphics;
         FreeVulkanSurface;
        except
        end;
        GraphicsReady:=false;
       end;
       if not GraphicsReady then begin
        try
         AllocateVulkanSurface;
         Main.StartGraphics;
        except
         SDLRunning:=false;
         break;
        end;
        GraphicsReady:=true;
       end;
      end;
      SDL_KEYDOWN:begin
       case Event.key.keysym.sym of
        SDLK_ESCAPE:begin
  //     BackKey;
         SDLRunning:=false;
         break;
        end;
        SDLK_F4:begin
         if (Event.key.keysym.modifier and ((KMOD_LALT or KMOD_RALT) or (KMOD_LMETA or KMOD_RMETA)))<>0 then begin
          SDLRunning:=false;
          break;
         end;
        end;
        SDLK_RETURN:begin
         if (Event.key.keysym.modifier and ((KMOD_LALT or KMOD_RALT) or (KMOD_LMETA or KMOD_RMETA)))<>0 then begin
          FullScreen:=not FullScreen;
          if FullScreen then begin
           SDL_SetWindowFullscreen(SurfaceWindow,SDL_WINDOW_FULLSCREEN_DESKTOP);
          end else begin
           SDL_SetWindowFullscreen(SurfaceWindow,0);
          end;
  {       if GraphicsReady then begin
           try
            StopGraphics;
            FreeVulkanSurface;
           except
           end;
           GraphicsReady:=false;
          end;
          if not GraphicsReady then begin
           try
            AllocateVulkanSurface;
            StartGraphics;
           except
            TPasMPInterlocked.Write(SDLRunning,TPasMPBool32(false));
            break;
           end;
           GraphicsReady:=true;
          end;}
         end;
        end;
        SDLK_SPACE:begin
        end;
       end;
      end;
      SDL_KEYUP:begin
      end;
      SDL_WINDOWEVENT:begin
       case event.window.event of
        SDL_WINDOWEVENT_RESIZED:begin
         WindowWidth:=event.window.Data1;
         WindowHeight:=event.window.Data2;
         VulkanPresentationSurface.SetSize(WindowWidth,WindowHeight);
         Main.ResizeGraphics(WindowWidth,WindowHeight);
  {      if GraphicsReady then begin
          try
           Main.StopGraphics;
           FreeVulkanSurface;
          except
          end;
          GraphicsReady:=false;
         end;
         if not GraphicsReady then begin
          try
           AllocateVulkanSurface;
           Main.StartGraphics;
          except
           SDLRunning:=false;
           break;
          end;
          GraphicsReady:=true;
         end;}
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

    if GraphicsReady then begin
     if VulkanPresentationSurface.AcquireBackBuffer(VulkanBlock) then begin
      VulkanPresentationSurface.PresentBackBuffer;
     end;
    end;

   end;

   GraphicsReady:=false;
   Main.StopGraphics;

   FreeVulkanSurface;

  finally
   FreeAndNil(Main);
  end;

  FreeVulkanInstance;

  if assigned(SurfaceWindow) then begin
   SDL_DestroyWindow(SurfaceWindow);
   SurfaceWindow:=nil;
  end;

 finally
  FreeAndNil(VulkanDebug);
 end;

end;

end.

