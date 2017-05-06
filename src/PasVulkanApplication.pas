(******************************************************************************
 *                              PasVulkanApplication                          *
 ******************************************************************************
 *                        Version 2017-05-05-02-17-0000                       *
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
     {$elseif defined(Windows)}
      Windows,
     {$ifend}
     SysUtils,Classes,Math,
     Vulkan,
     PasVulkan,
     PasVulkanSDL2,
     PasVulkanAndroid;

const MaxSwapChainImages=3;

type EVulkanApplication=class(Exception);

     TVulkanApplication=class;

     TVulkanApplicationOnEvent=function(const fVulkanApplication:TVulkanApplication;const pEvent:TSDL_Event):boolean of object;

     TVulkanApplicationOnStep=procedure(const fVulkanApplication:TVulkanApplication) of object;

     TVulkanPresentationSurface=class;

     TVulkanPresentationSurfaceOnAfterCreateSwapChain=procedure(const pSurface:TVulkanPresentationSurface) of object;
     TVulkanPresentationSurfaceOnBeforeDestroySwapChain=procedure(const pSurface:TVulkanPresentationSurface) of object;

     TVulkanPresentationSurface=class
      private
       fVulkanApplication:TVulkanApplication;
       fVulkanInstance:TVulkanInstance;
       fVulkanSurface:TVulkanSurface;
       fVulkanDevice:TVulkanDevice;
       fVulkanSwapChain:TVulkanSwapChain;
       fVulkanSwapChainImageFences:array[0..MaxSwapChainImages-1] of TVulkanFence;
       fVulkanSwapChainImageFencesReady:array[0..MaxSwapChainImages-1] of boolean;
       fVulkanSwapChainSimpleDirectRenderTarget:TVulkanSwapChainSimpleDirectRenderTarget;
       fVulkanCommandPool:TVulkanCommandPool;
       fVulkanCommandBuffer:TVulkanCommandBuffer;
       fVulkanCommandBuffers:array[0..MaxSwapChainImages-1] of TVulkanCommandBuffer;
       fVulkanCommandBufferFences:array[0..MaxSwapChainImages-1] of TVulkanFence;
       fVulkanCommandBufferFencesReady:array[0..MaxSwapChainImages-1] of boolean;
       fVulkanPresentCompleteSemaphores:array[0..MaxSwapChainImages-1] of TVulkanSemaphore;
       fVulkanDrawCompleteSemaphores:array[0..MaxSwapChainImages-1] of TVulkanSemaphore;
       fDoNeedToRecreateVulkanSwapChain:boolean;
       fGraphicsPipelinesReady:boolean;
       fWidth:TVkInt32;
       fHeight:TVkInt32;
       fLastImageIndex:TVkInt32;
       fCurrentImageIndex:TVkInt32;
       fReady:boolean;
       fVSync:boolean;
       fOnAfterCreateSwapChain:TVulkanPresentationSurfaceOnAfterCreateSwapChain;
       fOnBeforeDestroySwapChain:TVulkanPresentationSurfaceOnBeforeDestroySwapChain;
       procedure AfterCreateSwapChain;
       procedure BeforeDestroySwapChain;
      public
       constructor Create(const pVulkanApplication:TVulkanApplication;
                          const pWidth,pHeight:TVkInt32;
                          const pVSync:boolean;
                          const pSurfaceCreateInfo:TVulkanSurfaceCreateInfo);
       destructor Destroy; override;
       procedure SetSize(const pNewWidth,pNewHeight:TVkInt32);
       procedure SetVSync(const pVSync:boolean);
       procedure ClearAll;
       procedure WaitIdle;
       function AcquireBackBuffer(const pBlock:boolean):boolean;
       function PresentBackBuffer:boolean;
      published
       property Width:TVkInt32 read fWidth;
       property Height:TVkInt32 read fHeight;
       property LastImageIndex:TVkInt32 read fLastImageIndex;
       property CurrentImageIndex:TVkInt32 read fCurrentImageIndex;
       property Ready:boolean read fReady write fReady;
       property VSync:boolean read fVSync write SetVSync;
       property VulkanSwapChain:TVulkanSwapChain read fVulkanSwapChain;
       property VulkanCommandBuffer:TVulkanCommandBuffer read fVulkanCommandBuffer;
       property VulkanSwapChainSimpleDirectRenderTarget:TVulkanSwapChainSimpleDirectRenderTarget read fVulkanSwapChainSimpleDirectRenderTarget;
     end;

     TVulkanScreen=class
      public

       constructor Create; virtual;

       destructor Destroy; override;

       procedure Show; virtual;

       procedure Hide; virtual;

       procedure Resume; virtual;

       procedure Pause; virtual;

       procedure Resize(const pWidth,pHeight:TSDLInt32); virtual;

       procedure AfterCreateSwapChain; virtual;

       procedure BeforeDestroySwapChain; virtual;

       function HandleEvent(const pEvent:TSDL_Event):boolean; virtual;

       procedure Update(const pDeltaTime:double); virtual;

       procedure Draw; virtual;

     end;

     TVulkanScreenClass=class of TVulkanScreen;

     TVulkanApplicationAssets=class
      private
       fVulkanApplication:TVulkanApplication;
       fBasePath:string;
      public
       constructor Create(const pVulkanApplication:TVulkanApplication);
       destructor Destroy; override;
       function GetAsset(const pFileName:string):TStream;
       function ExistAsset(const pFileName:string):boolean;
     end;

     TVulkanApplicationCommandPools=array of array of TVulkanCommandPool;

     TVulkanApplicationCommandBuffers=array of array of TVulkanCommandBuffer;

     TVulkanApplicationCommandBufferFences=array of array of TVulkanFence;

     TVulkanApplication=class
      private

       fTitle:string;
       fVersion:TVkUInt32;

       fAssets:TVulkanApplicationAssets;

       fCurrentWidth:TSDLInt32;
       fCurrentHeight:TSDLInt32;
       fCurrentFullscreen:TSDLInt32;
       fCurrentVSync:TSDLInt32;
       fCurrentVisibleMouseCursor:TSDLInt32;
       fCurrentCatchMouse:TSDLInt32;
       fCurrentBlocking:TSDLInt32;
       fCurrentActive:TSDLInt32;

       fWidth:TSDLInt32;
       fHeight:TSDLInt32;
       fFullscreen:boolean;
       fVSync:boolean;
       fResizable:boolean;
       fVisibleMouseCursor:boolean;
       fCatchMouse:boolean;
       fBlocking:boolean;

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

       fVulkanDebugging:boolean;

       fVulkanValidation:boolean;

       fVulkanDebuggingEnabled:boolean;

       fVulkanInstance:TVulkanInstance;

       fVulkanDevice:TVulkanDevice;

       fCountThreads:TVkInt32;

       fVulkanCountCommandQueues:TVkInt32;

       fVulkanCommandPools:array of TVulkanApplicationCommandPools;
       fVulkanCommandBuffers:array of TVulkanApplicationCommandBuffers;
       fVulkanCommandBufferFences:array of TVulkanApplicationCommandBufferFences;

       fVulkanPresentCommandPools:TVulkanApplicationCommandPools;
       fVulkanPresentCommandBuffers:TVulkanApplicationCommandBuffers;
       fVulkanPresentCommandBufferFences:TVulkanApplicationCommandBufferFences;

       fVulkanGraphicsCommandPools:TVulkanApplicationCommandPools;
       fVulkanGraphicsCommandBuffers:TVulkanApplicationCommandBuffers;
       fVulkanGraphicsCommandBufferFences:TVulkanApplicationCommandBufferFences;

       fVulkanComputeCommandPools:TVulkanApplicationCommandPools;
       fVulkanComputeCommandBuffers:TVulkanApplicationCommandBuffers;
       fVulkanComputeCommandBufferFences:TVulkanApplicationCommandBufferFences;

       fVulkanTransferCommandPools:TVulkanApplicationCommandPools;
       fVulkanTransferCommandBuffers:TVulkanApplicationCommandBuffers;
       fVulkanTransferCommandBufferFences:TVulkanApplicationCommandBufferFences;

       fVulkanPresentationSurface:TVulkanPresentationSurface;

       fOnEvent:TVulkanApplicationOnEvent;

       fOnStep:TVulkanApplicationOnStep;

       fScreen:TVulkanScreen;

       fStartScreen:TVulkanScreenClass;

       fNextScreen:TVulkanScreen;

       fHasNewNextScreen:boolean;

{$if defined(fpc) and defined(android)}
       fJavaEnv:PJNIEnv;
       fJavaClass:jclass;
       fJavaObject:jobject;
{$ifend}

       procedure Activate;
       procedure Deactivate;

      protected

       procedure VulkanDebugLn(const What:TVkCharString);

       function VulkanOnDebugReportCallback(const flags:TVkDebugReportFlagsEXT;const objectType:TVkDebugReportObjectTypeEXT;const object_:TVkUInt64;const location:TVkSize;messageCode:TVkInt32;const pLayerPrefix:TVulkaNCharString;const pMessage:TVulkanCharString):TVkBool32;

       procedure CreateVulkanDevice(const pSurface:TVulkanSurface=nil);

       procedure AllocateVulkanInstance;
       procedure FreeVulkanInstance;

       procedure AllocateVulkanSurface;
       procedure FreeVulkanSurface;

       procedure StartGraphics;
       procedure StopGraphics;

       procedure SetScreen(const pScreen:TVulkanScreen);
       procedure SetNextScreen(const pNextScreen:TVulkanScreen);

      public

       constructor Create; reintroduce;
       destructor Destroy; override;

       procedure Initialize;

       procedure Terminate;

       function HandleEvent(const pEvent:TSDL_Event):boolean; virtual;

       procedure ProcessMessages;

       procedure Run;

       procedure Resume; virtual;

       procedure Pause; virtual;

     public

{$if defined(fpc) and defined(android)}
       property JavaEnv:PJNIEnv read fJavaEnv write fJavaEnv;
       property JavaClass:jclass read fJavaClass write fJavaClass;
       property JavaObject:jobject read fJavaObject write fJavaObject;
{$ifend}

      published

       property Assets:TVulkanApplicationAssets read fAssets;

       property Title:string read fTitle write fTitle;
       property Version:TVkUInt32 read fVersion write fVersion;

       property Width:TSDLInt32 read fWidth write fWidth;
       property Height:TSDLInt32 read fHeight write fHeight;

       property Fullscreen:boolean read fFullscreen write fFullscreen;

       property VSync:boolean read fVSync write fVSync;

       property Resizable:boolean read fResizable write fResizable;

       property VisibleMouseCursor:boolean read fVisibleMouseCursor write fVisibleMouseCursor;

       property CatchMouse:boolean read fCatchMouse write fCatchMouse;

       property Blocking:boolean read fBlocking write fBlocking;

       property Active:boolean read fActive;

       property Terminated:boolean read fTerminated;

       property CountThreads:TVkInt32 read fCountThreads write fCountThreads;

       property OnEvent:TVulkanApplicationOnEvent read fOnEvent write fOnEvent;
       property OnStep:TVulkanApplicationOnStep read fOnStep write fOnStep;

       property VulkanDebugging:boolean read fVulkanDebugging write fVulkanDebugging;

       property VulkanValidation:boolean read fVulkanValidation write fVulkanValidation;

       property VulkanDebuggingEnabled:boolean read fVulkanDebuggingEnabled;

       property VulkanInstance:TVulkanInstance read fVulkanInstance;

       property VulkanDevice:TVulkanDevice read fVulkanDevice;

       property VulkanPresentCommandPools:TVulkanApplicationCommandPools read fVulkanPresentCommandPools;
       property VulkanPresentCommandBuffers:TVulkanApplicationCommandBuffers read fVulkanPresentCommandBuffers;
       property VulkanPresentCommandBufferFences:TVulkanApplicationCommandBufferFences read fVulkanPresentCommandBufferFences;

       property VulkanGraphicsCommandPools:TVulkanApplicationCommandPools read fVulkanGraphicsCommandPools;
       property VulkanGraphicsCommandBuffers:TVulkanApplicationCommandBuffers read fVulkanGraphicsCommandBuffers;
       property VulkanGraphicsCommandBufferFences:TVulkanApplicationCommandBufferFences read fVulkanGraphicsCommandBufferFences;

       property VulkanComputeCommandPools:TVulkanApplicationCommandPools read fVulkanComputeCommandPools;
       property VulkanComputeCommandBuffers:TVulkanApplicationCommandBuffers read fVulkanComputeCommandBuffers;
       property VulkanComputeCommandBufferFences:TVulkanApplicationCommandBufferFences read fVulkanComputeCommandBufferFences;

       property VulkanTransferCommandPools:TVulkanApplicationCommandPools read fVulkanTransferCommandPools;
       property VulkanTransferCommandBuffers:TVulkanApplicationCommandBuffers read fVulkanTransferCommandBuffers;
       property VulkanTransferCommandBufferFences:TVulkanApplicationCommandBufferFences read fVulkanTransferCommandBufferFences;

       property VulkanPresentationSurface:TVulkanPresentationSurface read fVulkanPresentationSurface;

       property StartScreen:TVulkanScreenClass read fStartScreen write fStartScreen;

       property Screen:TVulkanScreen read fScreen write SetScreen;

       property NextScreen:TVulkanScreen read fNextScreen write SetNextScreen;

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

constructor TVulkanPresentationSurface.Create(const pVulkanApplication:TVulkanApplication;
                                              const pWidth,pHeight:TVkInt32;
                                              const pVSync:boolean;
                                              const pSurfaceCreateInfo:TVulkanSurfaceCreateInfo);
var Index:TVkInt32;
begin
 inherited Create;

 fVulkanApplication:=pVulkanApplication;

 fVulkanInstance:=fVulkanApplication.VulkanInstance;

 SetSize(pWidth,pHeight);

 fVSync:=pVSync;

 fReady:=false;

 fLastImageIndex:=-2;

 fCurrentImageIndex:=-1;

 fGraphicsPipelinesReady:=false;

 fOnAfterCreateSwapChain:=nil;
 fOnBeforeDestroySwapChain:=nil;
 
 try

  fVulkanSurface:=TVulkanSurface.Create(fVulkanInstance,pSurfaceCreateInfo);

  fVulkanDevice:=fVulkanApplication.fVulkanDevice;
  if not assigned(fVulkanDevice) then begin
   fVulkanApplication.CreateVulkanDevice(fVulkanSurface);
   fVulkanDevice:=fVulkanApplication.VulkanDevice;
   if not assigned(fVulkanDevice) then begin
    raise EVulkanSurfaceException.Create('Device does not support surface');
   end;
  end;

  fVulkanSwapChain:=TVulkanSwapChain.Create(fVulkanDevice,
                                            fVulkanSurface,
                                            nil,
                                            Width,
                                            Height,
                                            IfThen(pVSync,MaxSwapChainImages,1),
                                            1,
                                            VK_FORMAT_UNDEFINED,
                                            VK_COLOR_SPACE_SRGB_NONLINEAR_KHR,
                                            TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT),
                                            VK_SHARING_MODE_EXCLUSIVE,
                                            nil,
                                            VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
                                            TVkPresentModeKHR(integer(IfThen(pVSync,integer(VK_PRESENT_MODE_MAILBOX_KHR),integer(VK_PRESENT_MODE_IMMEDIATE_KHR)))));

  fVulkanSwapChainSimpleDirectRenderTarget:=TVulkanSwapChainSimpleDirectRenderTarget.Create(fVulkanDevice,
                                                                                            fVulkanSwapChain,
                                                                                            fVulkanApplication.VulkanPresentCommandBuffers[0,0],
                                                                                            fVulkanApplication.VulkanPresentCommandBufferFences[0,0],
                                                                                            fVulkanApplication.VulkanGraphicsCommandBuffers[0,0],
                                                                                            fVulkanApplication.VulkanGraphicsCommandBufferFences[0,0]);

  fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.ClearValues[0].color.float32[0]:=0.0;
  fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.ClearValues[0].color.float32[1]:=0.0;
  fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.ClearValues[0].color.float32[2]:=0.0;

  fVulkanCommandPool:=TVulkanCommandPool.Create(fVulkanDevice,
                                                fVulkanDevice.GraphicsQueueFamilyIndex,
                                                TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

  fVulkanCommandBuffer:=nil;

  for Index:=0 to MaxSwapChainImages-1 do begin
   fVulkanSwapChainImageFences[Index]:=TVulkanFence.Create(fVulkanDevice);
   fVulkanSwapChainImageFencesReady[Index]:=false;
   fVulkanCommandBuffers[Index]:=TVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
   fVulkanCommandBufferFences[Index]:=TVulkanFence.Create(fVulkanDevice);
   fVulkanCommandBufferFencesReady[Index]:=false;
   fVulkanPresentCompleteSemaphores[Index]:=TVulkanSemaphore.Create(fVulkanDevice);
   fVulkanDrawCompleteSemaphores[Index]:=TVulkanSemaphore.Create(fVulkanDevice);
  end;

  fDoNeedToRecreateVulkanSwapChain:=false;

  AfterCreateSwapChain;

 except

  for Index:=0 to MaxSwapChainImages-1 do begin
   FreeAndNil(fVulkanSwapChainImageFences[Index]);
   FreeAndNil(fVulkanCommandBuffers[Index]);
   FreeAndNil(fVulkanCommandBufferFences[Index]);
   FreeAndNil(fVulkanPresentCompleteSemaphores[Index]);
   FreeAndNil(fVulkanDrawCompleteSemaphores[Index]);
  end;
  FreeAndNil(fVulkanCommandPool);
  FreeAndNil(fVulkanSwapChain);
//FreeAndNil(fVulkanDevice);
  FreeAndNil(fVulkanSurface);
  raise;
 end;
end;

destructor TVulkanPresentationSurface.Destroy;
var Index:TVkInt32;
begin
 if assigned(fVulkanDevice) then begin
  fVulkanDevice.WaitIdle;
  for Index:=0 to MaxSwapChainImages-1 do begin
   if fVulkanSwapChainImageFencesReady[Index] and assigned(fVulkanSwapChainImageFences[Index]) then begin
    fVulkanSwapChainImageFences[Index].WaitFor;
    fVulkanSwapChainImageFences[Index].Reset;
    fVulkanSwapChainImageFencesReady[Index]:=false;
   end;
   if fVulkanCommandBufferFencesReady[Index] and assigned(fVulkanCommandBufferFences[Index]) then begin
    fVulkanCommandBufferFences[Index].WaitFor;
    fVulkanCommandBufferFences[Index].Reset;
    fVulkanCommandBufferFencesReady[Index]:=false;
   end;
  end;
  fVulkanDevice.WaitIdle;
 end;
 ClearAll;
 if assigned(fVulkanDevice) then begin
  fVulkanDevice.WaitIdle;
 end;
 BeforeDestroySwapChain;
 if assigned(fVulkanDevice) then begin
  fVulkanDevice.WaitIdle;
 end;
 FreeAndNil(fVulkanSwapChainSimpleDirectRenderTarget);
 if assigned(fVulkanDevice) then begin
  fVulkanDevice.WaitIdle;
 end;
 for Index:=0 to MaxSwapChainImages-1 do begin
  FreeAndNil(fVulkanSwapChainImageFences[Index]);
  FreeAndNil(fVulkanCommandBuffers[Index]);
  FreeAndNil(fVulkanCommandBufferFences[Index]);
  FreeAndNil(fVulkanPresentCompleteSemaphores[Index]);
  FreeAndNil(fVulkanDrawCompleteSemaphores[Index]);
 end;
 FreeAndNil(fVulkanCommandPool);
 FreeAndNil(fVulkanSwapChain);
//FreeAndNil(fVulkanDevice);
 FreeAndNil(fVulkanSurface);
 inherited Destroy;
end;

procedure TVulkanPresentationSurface.SetSize(const pNewWidth,pNewHeight:TVkInt32);
begin
 fWidth:=pNewWidth;
 fHeight:=pNewHeight;
end;

procedure TVulkanPresentationSurface.SetVSync(const pVSync:boolean);
begin
 if fVSync<>pVSync then begin
  fVSync:=pVSync;
  fDoNeedToRecreateVulkanSwapChain:=true;
 end;
end;

procedure TVulkanPresentationSurface.ClearAll;
begin
end;

procedure TVulkanPresentationSurface.WaitIdle;
var Index:TVkInt32;
begin
 if assigned(fVulkanDevice) then begin
  fVulkanDevice.WaitIdle;
  for Index:=0 to MaxSwapChainImages-1 do begin
   if fVulkanSwapChainImageFencesReady[Index] and assigned(fVulkanSwapChainImageFences[Index]) then begin
    fVulkanSwapChainImageFences[Index].WaitFor;
    fVulkanSwapChainImageFences[Index].Reset;
    fVulkanSwapChainImageFencesReady[Index]:=false;
   end;
   if fVulkanCommandBufferFencesReady[Index] and assigned(fVulkanCommandBufferFences[Index]) then begin
    fVulkanCommandBufferFences[Index].WaitFor;
    fVulkanCommandBufferFences[Index].Reset;
    fVulkanCommandBufferFencesReady[Index]:=false;
   end;
  end;
  fVulkanDevice.WaitIdle;
 end;
end;

procedure TVulkanPresentationSurface.AfterCreateSwapChain;
begin
 if not fGraphicsPipelinesReady then begin
  if assigned(fVulkanApplication.fScreen) then begin
   fVulkanApplication.fScreen.AfterCreateSwapChain;
  end;
  if assigned(fOnAfterCreateSwapChain) then begin
   fOnAfterCreateSwapChain(self);
  end;
  fGraphicsPipelinesReady:=true;
 end;
end;

procedure TVulkanPresentationSurface.BeforeDestroySwapChain;
begin
 if fGraphicsPipelinesReady then begin
  fGraphicsPipelinesReady:=false;
  WaitIdle;
  if assigned(fOnBeforeDestroySwapChain) then begin
   fOnBeforeDestroySwapChain(self);
  end;
  if assigned(fVulkanApplication.fScreen) then begin
   fVulkanApplication.fScreen.BeforeDestroySwapChain;
  end;
 end;
end;

function TVulkanPresentationSurface.AcquireBackBuffer(const pBlock:boolean):boolean;
var ImageIndex:TVkInt32;
    OldVulkanSwapChain:TVulkanSwapChain;
    TimeOut:TVkUInt64;
begin
 result:=false;

 fLastImageIndex:=fCurrentImageIndex;

 fCurrentImageIndex:=fVulkanSwapChain.CurrentImageIndex;

 if (fCurrentImageIndex<0) or (fCurrentImageIndex>=MaxSwapChainImages) then begin
  exit;
 end;

 if fVulkanSwapChainImageFencesReady[fCurrentImageIndex] then begin
  if fVulkanSwapChainImageFences[fCurrentImageIndex].GetStatus<>VK_SUCCESS then begin
   if pBlock then begin
    fVulkanSwapChainImageFences[fCurrentImageIndex].WaitFor;
   end else begin
    exit;
   end;
  end;
  fVulkanSwapChainImageFences[fCurrentImageIndex].Reset;
  fVulkanSwapChainImageFencesReady[fCurrentImageIndex]:=false;
 end;

 if fVulkanCommandBufferFencesReady[fCurrentImageIndex] then begin
  if fVulkanCommandBufferFences[fCurrentImageIndex].GetStatus<>VK_SUCCESS then begin
   if pBlock then begin
    fVulkanCommandBufferFences[fCurrentImageIndex].WaitFor;
   end else begin
    exit;
   end;
  end;
  fVulkanCommandBufferFences[fCurrentImageIndex].Reset;
  fVulkanCommandBufferFencesReady[fCurrentImageIndex]:=false;
 end;

 if (fVulkanSwapChain.Width<>Width) or (fVulkanSwapChain.Height<>Height) then begin
  fDoNeedToRecreateVulkanSwapChain:=true;
  fVulkanApplication.VulkanDebugLn('New surface dimension size detected!');
 end else begin
  try
   if pBlock then begin
    TimeOut:=TVkUInt64(high(TVkUInt64));
   end else begin
    TimeOut:=0;
   end;
   case fVulkanSwapChain.AcquireNextImage(fVulkanPresentCompleteSemaphores[fCurrentImageIndex],fVulkanSwapChainImageFences[fCurrentImageIndex],TimeOut) of
    VK_SUCCESS:begin
     fVulkanSwapChainImageFencesReady[fCurrentImageIndex]:=true;
    end;
    VK_SUBOPTIMAL_KHR:begin
     fDoNeedToRecreateVulkanSwapChain:=true;
     fVulkanApplication.VulkanDebugLn('Suboptimal surface detected!');
    end;
    else {VK_SUCCESS,VK_TIMEOUT:}begin
     exit;
    end;
   end;
  except
   on VulkanResultException:EVulkanResultException do begin
    case VulkanResultException.ResultCode of
     VK_ERROR_SURFACE_LOST_KHR,
     VK_ERROR_OUT_OF_DATE_KHR,
     VK_SUBOPTIMAL_KHR:begin
      fDoNeedToRecreateVulkanSwapChain:=true;
      fVulkanApplication.VulkanDebugLn(VulkanResultException.ClassName+': '+VulkanResultException.Message);
     end;
     else begin
      raise;
     end;
    end;
   end;
  end;
 end;

 if fDoNeedToRecreateVulkanSwapChain then begin

  for ImageIndex:=0 to MaxSwapChainImages-1 do begin
   if fVulkanCommandBufferFencesReady[ImageIndex] then begin
    fVulkanCommandBufferFences[ImageIndex].WaitFor;
    fVulkanCommandBufferFences[ImageIndex].Reset;
    fVulkanCommandBufferFencesReady[ImageIndex]:=false;
   end;
  end;

  fVulkanDevice.WaitIdle;

  fVulkanApplication.VulkanDebugLn('Recreating swap chain... ');
  fDoNeedToRecreateVulkanSwapChain:=false;
  OldVulkanSwapChain:=fVulkanSwapChain;
  try
   BeforeDestroySwapChain;
   FreeAndNil(fVulkanSwapChainSimpleDirectRenderTarget);
   fVulkanSwapChain:=TVulkanSwapChain.Create(fVulkanDevice,
                                             fVulkanSurface,
                                             OldVulkanSwapChain,
                                             Width,
                                             Height,
                                             IfThen(fVSync,MaxSwapChainImages,1),
                                             1,
                                             VK_FORMAT_UNDEFINED,
                                             VK_COLOR_SPACE_SRGB_NONLINEAR_KHR,
                                             TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT),
                                             VK_SHARING_MODE_EXCLUSIVE,
                                             nil,
                                             VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
                                             TVkPresentModeKHR(integer(IfThen(fVSync,integer(VK_PRESENT_MODE_MAILBOX_KHR),integer(VK_PRESENT_MODE_IMMEDIATE_KHR)))));
   fVulkanSwapChainSimpleDirectRenderTarget:=TVulkanSwapChainSimpleDirectRenderTarget.Create(fVulkanDevice,
                                                                                             fVulkanSwapChain,
                                                                                             fVulkanApplication.VulkanPresentCommandBuffers[0,0],
                                                                                             fVulkanApplication.VulkanPresentCommandBufferFences[0,0],
                                                                                             fVulkanApplication.VulkanGraphicsCommandBuffers[0,0],
                                                                                             fVulkanApplication.VulkanGraphicsCommandBufferFences[0,0]);
   AfterCreateSwapChain;
  finally
   OldVulkanSwapChain.Free;
  end;
  fVulkanApplication.VulkanDebugLn('Recreated swap chain... ');

  fCurrentImageIndex:=fVulkanSwapChain.CurrentImageIndex;

 end else begin

  result:=true;

 end;

end;

function TVulkanPresentationSurface.PresentBackBuffer:boolean;
begin

 result:=false;
                                
 fVulkanCommandBuffer:=fVulkanCommandBuffers[fCurrentImageIndex];

 fVulkanCommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));

 fVulkanCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));

 fVulkanCommandBuffer.MetaCmdPresentToDrawImageBarrier(fVulkanSwapChain.CurrentImage);

 if assigned(fVulkanApplication.fScreen) then begin

  fVulkanApplication.fScreen.Draw;

 end else begin

  fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.BeginRenderPass(fVulkanCommandBuffer,
                                                                      fVulkanSwapChainSimpleDirectRenderTarget.FrameBuffer,
                                                                      VK_SUBPASS_CONTENTS_INLINE,
                                                                      0,0,fVulkanSwapChain.Width,fVulkanSwapChain.Height);

  fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.EndRenderPass(fVulkanCommandBuffer);

 end;

 fVulkanCommandBuffer.MetaCmdDrawToPresentImageBarrier(fVulkanSwapChain.CurrentImage);

 fVulkanCommandBuffer.EndRecording;

 fVulkanCommandBuffer.Execute(fVulkanDevice.GraphicsQueue,
                              TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                              fVulkanPresentCompleteSemaphores[fCurrentImageIndex],
                              fVulkanDrawCompleteSemaphores[fCurrentImageIndex],
                              fVulkanCommandBufferFences[fCurrentImageIndex],
                              false);
 fVulkanCommandBufferFencesReady[fCurrentImageIndex]:=true;

 try
  case fVulkanSwapChain.QueuePresent(fVulkanDevice.GraphicsQueue,fVulkanDrawCompleteSemaphores[fCurrentImageIndex]) of
   VK_SUCCESS:begin
    //VulkanDevice.WaitIdle; // A GPU/CPU frame synchronization point only for debug cases here, when something got run wrong
    result:=true;
   end;
   VK_SUBOPTIMAL_KHR:begin
    fDoNeedToRecreateVulkanSwapChain:=true;
   end;
  end;
 except
  on VulkanResultException:EVulkanResultException do begin
   case VulkanResultException.ResultCode of
    VK_ERROR_SURFACE_LOST_KHR,
    VK_ERROR_OUT_OF_DATE_KHR,
    VK_SUBOPTIMAL_KHR:begin
     fDoNeedToRecreateVulkanSwapChain:=true;
    end;
    else begin
     raise;
    end;
   end;
  end;
 end;

end;

constructor TVulkanScreen.Create;
begin
 inherited Create;
end;

destructor TVulkanScreen.Destroy;
begin
 inherited Destroy;
end;

procedure TVulkanScreen.Show;
begin
end;

procedure TVulkanScreen.Hide;
begin
end;

procedure TVulkanScreen.Resume;
begin
end;

procedure TVulkanScreen.Pause;
begin
end;

procedure TVulkanScreen.Resize(const pWidth,pHeight:TSDLInt32);
begin
end;

procedure TVulkanScreen.AfterCreateSwapChain;
begin
end;

procedure TVulkanScreen.BeforeDestroySwapChain; 
begin
end;

function TVulkanScreen.HandleEvent(const pEvent:TSDL_Event):boolean;
begin
 result:=false;
end;

procedure TVulkanScreen.Update(const pDeltaTime:double);
begin
end;

procedure TVulkanScreen.Draw;
begin
end;

constructor TVulkanApplicationAssets.Create(const pVulkanApplication:TVulkanApplication);
begin
 inherited Create;
 fVulkanApplication:=pVulkanApplication;
 fBasePath:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'assets');
end;

destructor TVulkanApplicationAssets.Destroy;
begin
 inherited Destroy;
end;

function TVulkanApplicationAssets.GetAsset(const pFileName:string):TStream;
begin
 result:=TFileStream.Create(StringReplace(StringReplace(fBasePath+pFileName,'/',PathDelim,[rfReplaceAll]),'\',PathDelim,[rfReplaceAll]),fmOpenRead or fmShareDenyWrite);
end;

function TVulkanApplicationAssets.ExistAsset(const pFileName:string):boolean;
begin
 result:=FileExists(StringReplace(StringReplace(fBasePath+pFileName,'/',PathDelim,[rfReplaceAll]),'\',PathDelim,[rfReplaceAll]));
end;

constructor TVulkanApplication.Create;
begin

 SDL_SetMainReady;

 SDL_SetHint(SDL_HINT_WINDOWS_DISABLE_THREAD_NAMING,'1');

 inherited Create;

 fTitle:='SDL2 Vulkan Application';
 fVersion:=$0100;

 fAssets:=TVulkanApplicationAssets.Create(self);

 fCurrentWidth:=-1;
 fCurrentHeight:=-1;
 fCurrentFullscreen:=-1;
 fCurrentVSync:=-1;
 fCurrentVisibleMouseCursor:=-1;
 fCurrentCatchMouse:=-1;
 fCurrentBlocking:=-1;
 fCurrentActive:=-1;

 fWidth:=1280;
 fHeight:=720;
 fFullscreen:=false;
 fVSync:=false;
 fResizable:=true;
 fVisibleMouseCursor:=false;
 fCatchMouse:=false;
 fBlocking:=true;

 fActive:=true;

 fTerminated:=false;

 fResetGraphics:=false;

 fGraphicsReady:=false;

 fVulkanDebugging:=false;

 fVulkanDebuggingEnabled:=false;

 fVulkanValidation:=false;

 fVulkanInstance:=nil;

 fVulkanDevice:=nil;

 fCountThreads:=1;

 fVulkanCountCommandQueues:=0;
 
 fVulkanCommandPools:=nil;
 fVulkanCommandBuffers:=nil;
 fVulkanCommandBufferFences:=nil;

 fVulkanPresentCommandPools:=nil;
 fVulkanPresentCommandBuffers:=nil;
 fVulkanPresentCommandBufferFences:=nil;

 fVulkanGraphicsCommandPools:=nil;
 fVulkanGraphicsCommandBuffers:=nil;
 fVulkanGraphicsCommandBufferFences:=nil;

 fVulkanComputeCommandPools:=nil;
 fVulkanComputeCommandBuffers:=nil;
 fVulkanComputeCommandBufferFences:=nil;

 fVulkanTransferCommandPools:=nil;
 fVulkanTransferCommandBuffers:=nil;
 fVulkanTransferCommandBufferFences:=nil;

 fVulkanPresentationSurface:=nil;

 fScreen:=nil;

 fNextScreen:=nil;

 fHasNewNextScreen:=false;

 fOnEvent:=nil;

 VulkanApplication:=self;

end;

destructor TVulkanApplication.Destroy;
begin
 FreeAndNil(fAssets);
 VulkanApplication:=nil;
 inherited Destroy;
end;

procedure TVulkanApplication.VulkanDebugLn(const What:TVkCharString);
{$ifdef Windows}
var StdOut:THandle;
begin
 StdOut:=GetStdHandle(Std_Output_Handle);
 Win32Check(StdOut<>Invalid_Handle_Value);
 if StdOut<>0 then begin
  WriteLn(What);
 end;
end;
{$else}
begin
 WriteLn(What);
end;
{$endif}

function TVulkanApplication.VulkanOnDebugReportCallback(const flags:TVkDebugReportFlagsEXT;const objectType:TVkDebugReportObjectTypeEXT;const object_:TVkUInt64;const location:TVkSize;messageCode:TVkInt32;const pLayerPrefix:TVulkaNCharString;const pMessage:TVulkanCharString):TVkBool32;
begin
 VulkanDebugLn('[Debug] '+pLayerPrefix+': '+pMessage);
 result:=VK_FALSE;
end;

procedure TVulkanApplication.CreateVulkanDevice(const pSurface:TVulkanSurface=nil);
var QueueFamilyIndex,ThreadIndex,SwapChainImageIndex:TVkInt32;
begin
 if not assigned(VulkanDevice) then begin

  fVulkanDevice:=TVulkanDevice.Create(VulkanInstance,nil,pSurface,nil);
  fVulkanDevice.AddQueues;
  fVulkanDevice.EnabledExtensionNames.Add(VK_KHR_SWAPCHAIN_EXTENSION_NAME);
  fVulkanDevice.Initialize;

  fVulkanCountCommandQueues:=length(fVulkanDevice.PhysicalDevice.QueueFamilyProperties);
  SetLength(fVulkanCommandPools,fVulkanCountCommandQueues,fCountThreads,MaxSwapChainImages);
  SetLength(fVulkanCommandBuffers,fVulkanCountCommandQueues,fCountThreads,MaxSwapChainImages);
  SetLength(fVulkanCommandBufferFences,fVulkanCountCommandQueues,fCountThreads,MaxSwapChainImages);
  for QueueFamilyIndex:=0 to length(fVulkanDevice.PhysicalDevice.QueueFamilyProperties)-1 do begin
   if (QueueFamilyIndex=fVulkanDevice.PresentQueueFamilyIndex) or
      (QueueFamilyIndex=fVulkanDevice.GraphicsQueueFamilyIndex) or
      (QueueFamilyIndex=fVulkanDevice.ComputeQueueFamilyIndex) or
      (QueueFamilyIndex=fVulkanDevice.TransferQueueFamilyIndex) then begin
    for ThreadIndex:=0 to fCountThreads-1 do begin
     for SwapChainImageIndex:=0 to MaxSwapChainImages-1 do begin
      fVulkanCommandPools[QueueFamilyIndex,ThreadIndex,SwapChainImageIndex]:=TVulkanCommandPool.Create(fVulkanDevice,QueueFamilyIndex,TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
      fVulkanCommandBuffers[QueueFamilyIndex,ThreadIndex,SwapChainImageIndex]:=TVulkanCommandBuffer.Create(fVulkanCommandPools[QueueFamilyIndex,ThreadIndex,SwapChainImageIndex],VK_COMMAND_BUFFER_LEVEL_PRIMARY);
      fVulkanCommandBufferFences[QueueFamilyIndex,ThreadIndex,SwapChainImageIndex]:=TVulkanFence.Create(fVulkanDevice);
     end;
    end;
   end;
  end;

  if fVulkanDevice.PresentQueueFamilyIndex>=0 then begin
   fVulkanPresentCommandPools:=fVulkanCommandPools[fVulkanDevice.PresentQueueFamilyIndex];
   fVulkanPresentCommandBuffers:=fVulkanCommandBuffers[fVulkanDevice.PresentQueueFamilyIndex];
   fVulkanPresentCommandBufferFences:=fVulkanCommandBufferFences[fVulkanDevice.PresentQueueFamilyIndex];
  end else begin
   fVulkanPresentCommandPools:=nil;
   fVulkanPresentCommandBuffers:=nil;
   fVulkanPresentCommandBufferFences:=nil;
  end;

  if fVulkanDevice.GraphicsQueueFamilyIndex>=0 then begin
   fVulkanGraphicsCommandPools:=fVulkanCommandPools[fVulkanDevice.GraphicsQueueFamilyIndex];
   fVulkanGraphicsCommandBuffers:=fVulkanCommandBuffers[fVulkanDevice.GraphicsQueueFamilyIndex];
   fVulkanGraphicsCommandBufferFences:=fVulkanCommandBufferFences[fVulkanDevice.GraphicsQueueFamilyIndex];
  end else begin
   fVulkanGraphicsCommandPools:=nil;
   fVulkanGraphicsCommandBuffers:=nil;
   fVulkanGraphicsCommandBufferFences:=nil;
  end;

  if fVulkanDevice.ComputeQueueFamilyIndex>=0 then begin
   fVulkanComputeCommandPools:=fVulkanCommandPools[fVulkanDevice.ComputeQueueFamilyIndex];
   fVulkanComputeCommandBuffers:=fVulkanCommandBuffers[fVulkanDevice.ComputeQueueFamilyIndex];
   fVulkanComputeCommandBufferFences:=fVulkanCommandBufferFences[fVulkanDevice.ComputeQueueFamilyIndex];
  end else begin
   fVulkanComputeCommandPools:=nil;
   fVulkanComputeCommandBuffers:=nil;
   fVulkanComputeCommandBufferFences:=nil;
  end;

  if fVulkanDevice.TransferQueueFamilyIndex>=0 then begin
   fVulkanTransferCommandPools:=fVulkanCommandPools[fVulkanDevice.TransferQueueFamilyIndex];
   fVulkanTransferCommandBuffers:=fVulkanCommandBuffers[fVulkanDevice.TransferQueueFamilyIndex];
   fVulkanTransferCommandBufferFences:=fVulkanCommandBufferFences[fVulkanDevice.TransferQueueFamilyIndex];
  end else begin
   fVulkanTransferCommandPools:=nil;
   fVulkanTransferCommandBuffers:=nil;
   fVulkanTransferCommandBufferFences:=nil;
  end;

 end;
end;

procedure TVulkanApplication.AllocateVulkanInstance;
var i:TVkInt32;
    SDL_SysWMinfo:TSDL_SysWMinfo;
begin
 if not assigned(fVulkanInstance) then begin
  SDL_VERSION(SDL_SysWMinfo.version);
  if SDL_GetWindowWMInfo(fSurfaceWindow,@SDL_SysWMinfo)<>0 then begin
   fVulkanInstance:=TVulkanInstance.Create(Title,Version,
                                           'PasVulkanApplication',$0100,
                                           VK_API_VERSION_1_0,false,nil);
   for i:=0 to fVulkanInstance.AvailableLayerNames.Count-1 do begin
    VulkanDebugLn('Layer: '+fVulkanInstance.AvailableLayerNames[i]);
   end;
   for i:=0 to fVulkanInstance.AvailableExtensionNames.Count-1 do begin
    VulkanDebugLn('Extension: '+fVulkanInstance.AvailableExtensionNames[i]);
   end;
   fVulkanInstance.EnabledExtensionNames.Add(VK_KHR_SURFACE_EXTENSION_NAME);
   case SDL_SysWMinfo.subsystem of
{$if defined(Android)}
    SDL_SYSWM_ANDROID:begin
     fVulkanInstance.EnabledExtensionNames.Add(VK_KHR_ANDROID_SURFACE_EXTENSION_NAME);
    end;
{$ifend}
{$if defined(Mir) and defined(Unix)}
    SDL_SYSWM_MIR:begin
     fVulkanInstance.EnabledExtensionNames.Add(VK_KHR_MIR_SURFACE_EXTENSION_NAME);
    end;
{$ifend}
{$if defined(Wayland) and defined(Unix)}
    SDL_SYSWM_WAYLAND:begin
     fVulkanInstance.EnabledExtensionNames.Add(VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME);
    end;
{$ifend}
{$if defined(Windows)}
    SDL_SYSWM_WINDOWS:begin
     fVulkanInstance.EnabledExtensionNames.Add(VK_KHR_WIN32_SURFACE_EXTENSION_NAME);
    end;
{$ifend}
{$if (defined(XLIB) or defined(XCB)) and defined(Unix)}
    SDL_SYSWM_X11:begin
{$if defined(XLIB) and defined(Unix)}
     fVulkanInstance.EnabledExtensionNames.Add(VK_KHR_XLIB_SURFACE_EXTENSION_NAME);
{$elseif defined(XCB) and defined(Unix)}
     fVulkanInstance.EnabledExtensionNames.Add(VK_KHR_XCB_SURFACE_EXTENSION_NAME);
{$ifend}
    end;
{$ifend}
    else begin
     raise EVulkanException.Create('Vulkan initialization failure');
    end;
   end;
   if fVulkanDebugging and
      (fVulkanInstance.AvailableExtensionNames.IndexOf(VK_EXT_DEBUG_REPORT_EXTENSION_NAME)>=0) then begin
    fVulkanInstance.EnabledExtensionNames.Add(VK_EXT_DEBUG_REPORT_EXTENSION_NAME);
    fVulkanDebuggingEnabled:=true;
    if fVulkanValidation then begin
     if fVulkanInstance.AvailableLayerNames.IndexOf('VK_LAYER_LUNARG_standard_validation')>=0 then begin
      fVulkanInstance.EnabledLayerNames.Add('VK_LAYER_LUNARG_standard_validation');
     end;
    end;
   end else begin
    fVulkanDebuggingEnabled:=false;
   end;
   fVulkanInstance.Initialize;
   if fVulkanDebuggingEnabled then begin
    fVulkanInstance.OnInstanceDebugReportCallback:=VulkanOnDebugReportCallback;
    fVulkanInstance.InstallDebugReportCallback;
   end;
  end;
 end;
end;

procedure TVulkanApplication.FreeVulkanInstance;
var Index,SubIndex,SubSubIndex:TVkInt32;
begin

 fVulkanPresentCommandPools:=nil;
 fVulkanPresentCommandBuffers:=nil;
 fVulkanPresentCommandBufferFences:=nil;

 fVulkanGraphicsCommandPools:=nil;
 fVulkanGraphicsCommandBuffers:=nil;
 fVulkanGraphicsCommandBufferFences:=nil;

 fVulkanComputeCommandPools:=nil;
 fVulkanComputeCommandBuffers:=nil;
 fVulkanComputeCommandBufferFences:=nil;

 fVulkanTransferCommandPools:=nil;
 fVulkanTransferCommandBuffers:=nil;
 fVulkanTransferCommandBufferFences:=nil;

 for Index:=0 to fVulkanCountCommandQueues-1 do begin
  for SubIndex:=0 to fCountThreads-1 do begin
   for SubSubIndex:=0 to MaxSwapChainImages-1 do begin
    FreeAndNil(fVulkanCommandBufferFences[Index,SubIndex,SubSubIndex]);
    FreeAndNil(fVulkanCommandBuffers[Index,SubIndex,SubSubIndex]);
    FreeAndNil(fVulkanCommandPools[Index,SubIndex,SubSubIndex]);
   end;
  end;
 end;

 fVulkanCommandPools:=nil;
 fVulkanCommandBuffers:=nil;
 fVulkanCommandBufferFences:=nil;

 //FreeAndNil(VulkanPresentationSurface);
 FreeAndNil(fVulkanDevice);
 FreeAndNil(fVulkanInstance);
//VulkanPresentationSurface:=nil;

 fVulkanDevice:=nil;
 fVulkanInstance:=nil;

end;

procedure TVulkanApplication.AllocateVulkanSurface;
var SDL_SysWMinfo:TSDL_SysWMinfo;
    VulkanSurfaceCreateInfo:TVulkanSurfaceCreateInfo;
begin
 if not assigned(fVulkanPresentationSurface) then begin
  SDL_VERSION(SDL_SysWMinfo.version);
  if SDL_GetWindowWMInfo(fSurfaceWindow,@SDL_SysWMinfo)<>0 then begin
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
   fVulkanPresentationSurface:=TVulkanPresentationSurface.Create(self,
                                                                 fWidth,
                                                                 fHeight,
                                                                 fVSync,
                                                                 VulkanSurfaceCreateInfo);
  end;
 end;
end;

procedure TVulkanApplication.FreeVulkanSurface;
begin
 if assigned(fVulkanPresentationSurface) then begin
  fVulkanPresentationSurface.Free;
  fVulkanPresentationSurface:=nil;
 end;
end;

procedure TVulkanApplication.StartGraphics;
begin
end;

procedure TVulkanApplication.StopGraphics;
begin
end;

procedure TVulkanApplication.SetScreen(const pScreen:TVulkanScreen);
begin
 if fScreen<>pScreen then begin
  if assigned(fScreen) then begin
   fScreen.Pause;
   if assigned(fVulkanPresentationSurface) then begin
    fVulkanPresentationSurface.WaitIdle;
   end;
   fScreen.BeforeDestroySwapChain;
   fScreen.Hide;
   fScreen.Free;
  end;
  fScreen:=pScreen;
  if assigned(fScreen) then begin
   fScreen.Show;
   if assigned(fScreen) then begin
    fScreen.Resize(fWidth,fHeight);
   end;
   if assigned(fVulkanPresentationSurface) then begin
    fVulkanPresentationSurface.WaitIdle;
   end;
   fScreen.AfterCreateSwapChain;
   fScreen.Resume;
  end;
 end;
end;

procedure TVulkanApplication.SetNextScreen(const pNextScreen:TVulkanScreen);
begin
 if (fScreen<>pNextScreen) and (fNextScreen<>pNextScreen) then begin
  if assigned(fNextScreen) then begin
   fNextScreen.Free;
  end;
  fNextScreen:=pNextScreen;
  fHasNewNextScreen:=true;
 end;
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

function TVulkanApplication.HandleEvent(const pEvent:TSDL_Event):boolean;
begin
 if assigned(fOnEvent) and fOnEvent(self,pEvent) then begin
  result:=true;
 end else if assigned(fScreen) and fScreen.HandleEvent(pEvent) then begin
  result:=true;
 end else begin
  result:=false;
 end;
end;

procedure TVulkanApplication.ProcessMessages;
begin

 if fCurrentVisibleMouseCursor<>ord(fVisibleMouseCursor) then begin
  fCurrentVisibleMouseCursor:=ord(fVisibleMouseCursor);
  if fVisibleMouseCursor then begin
   SDL_ShowCursor(1);
  end else begin
   SDL_ShowCursor(0);
  end;
 end;

 if fCurrentCatchMouse<>ord(fCatchMouse) then begin
  fCurrentCatchMouse:=ord(fCatchMouse);
  if fCatchMouse then begin
   SDL_SetRelativeMouseMode(1);
  end else begin
   SDL_SetRelativeMouseMode(0);
  end;
 end;

 if fHasNewNextScreen then begin
  fHasNewNextScreen:=false;
  if fScreen<>fNextScreen then begin
   SetScreen(fNextScreen);
  end;
  fNextScreen:=nil;
 end;

 if (fCurrentWidth<>fWidth) or (fCurrentHeight<>fHeight) or (fCurrentVSync<>ord(fVSync)) then begin
  fCurrentWidth:=fWidth;
  fCurrentHeight:=fHeight;
  fCurrentVSync:=ord(fVSync);
  if not fFullscreen then begin
   SDL_SetWindowSize(fSurfaceWindow,fWidth,fHeight);
  end;
  if assigned(fVulkanPresentationSurface) then begin
   fVulkanPresentationSurface.SetSize(fWidth,fHeight);
   fVulkanPresentationSurface.SetVSync(fVSync);
  end;
 end;

 while SDL_PollEvent(@fEvent)<>0 do begin
  if HandleEvent(fEvent) then begin
   continue;
  end;
  case fEvent.type_ of
   SDL_QUITEV,
   SDL_APP_TERMINATING:begin
    fActive:=false;
    Terminate;
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
      if ((fEvent.key.keysym.modifier and ((KMOD_LALT or KMOD_RALT) or (KMOD_LMETA or KMOD_RMETA)))<>0) and (fEvent.key.repeat_=0) then begin
       Terminate;
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
      fCurrentWidth:=fWidth;
      fCurrentHeight:=fHeight;
      fVulkanPresentationSurface.SetSize(fWidth,fHeight);
      if assigned(fScreen) then begin
       fScreen.Resize(fWidth,fHeight);
      end;
     end;
    end;
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
   Pause;
   Deactivate;
   Activate;
   Resume;
  end;
 end;

 if fCurrentActive<>ord(fActive) then begin
  fCurrentActive:=ord(fActive);
  if fActive then begin
   Activate;
   Resume;
  end else begin
   Pause;
   Deactivate;
  end;
 end;

 if fGraphicsReady then begin
  if assigned(fScreen) then begin
   fScreen.Update(0.0);
  end;
  if fVulkanPresentationSurface.AcquireBackBuffer(fBlocking) then begin
   fVulkanPresentationSurface.PresentBackBuffer;
  end;
 end;

end;

procedure TVulkanApplication.Run;
begin

 if fCountThreads<1 then begin
  fCountThreads:=1;
 end;

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
  fCurrentFullscreen:=ord(true);
 end else begin
  fCurrentFullscreen:=0;
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

 fCurrentWidth:=fWidth;
 fCurrentHeight:=fHeight;

 fCurrentVSync:=ord(fVSync);

 try

  AllocateVulkanInstance;
  try

   AllocateVulkanSurface;
   try

    Resume;
    try

     if assigned(fStartScreen) then begin
      SetScreen(fStartScreen.Create);
     end;

     while not fTerminated do begin
      ProcessMessages;
     end;

    finally

     Pause;

     SetScreen(nil);

     FreeAndNil(fNextScreen);
     FreeAndNil(fScreen);

    end;

   finally

    FreeVulkanSurface;

   end;

  finally
   FreeVulkanInstance;
  end;

 finally

  if assigned(fSurfaceWindow) then begin
   SDL_DestroyWindow(fSurfaceWindow);
   fSurfaceWindow:=nil;
  end;

 end;

end;

procedure TVulkanApplication.Resume;
begin
 if assigned(fScreen) then begin
  fScreen.Resume;
 end;
end;

procedure TVulkanApplication.Pause;
begin
 if assigned(fScreen) then begin
  fScreen.Pause;
 end;
end;

end.
