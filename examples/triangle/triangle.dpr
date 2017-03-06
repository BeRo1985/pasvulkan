program triangle;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$ifdef Win32}
 {$define Windows}
{$endif}
{$ifdef Win64}
 {$define Windows}
{$endif}
{$ifdef WinCE}
 {$define Windows}
{$endif}
{$ifdef Windows}
 {$define VK_USE_PLATFORM_WIN32_KHR}
{$endif}
{$ifdef Windows}
 {$apptype console}
{$endif}
uses
  Windows,
  Messages,
  MultiMon,
  SysUtils,
  Classes,
  PasVulkan in '..\..\src\PasVulkan.pas',
  vulkan in '..\..\src\vulkan.pas';

const MaxSwapChainImages=3; // 1 = single buffer, 2 = double buffer, 3 = triple buffer, 4 = quadruple buffer,
                            // 5 = quintuple buffer, 6 = sextuple buffer, 7 = septuple buffer, 8 = septuple buffer,
                            // etc.

var WndClass:TWndClass;
    hWindow:HWND;
    Msg:TMsg;
    Running:boolean;

    SurfaceWidth:TVkInt32=640;
    SurfaceHeight:TVkInt32=360;

    VulkanInstance:TVulkanInstance=nil;
    VulkanSurface:TVulkanSurface=nil;
    VulkanDevice:TVulkanDevice=nil;
    VulkanInitializationCommandBufferFence:TVulkanFence=nil;
    VulkanInitializationCommandPool:TVulkanCommandPool=nil;
    VulkanInitializationCommandBuffer:TVulkanCommandBuffer=nil;
    VulkanSwapChain:TVulkanSwapChain=nil;
    VulkanSwapChainSimpleDirectRenderTarget:TVulkanSwapChainSimpleDirectRenderTarget=nil;
    VulkanCommandPool:TVulkanCommandPool=nil;

    // each one instance (command buffer, fences, semaphores, etc.) per swap chain buffer (two when in a double buffer case)
    // for so that the GPU and CPU can work asynchronously without explicit CPU/GPU frame synchronization points, until
    // the CPU reaches the wrapped previous already processed swap chain image index again 
    VulkanSwapChainImageFences:array[0..MaxSwapChainImages-1] of TVulkanFence;
    VulkanSwapChainImageFencesReady:array[0..MaxSwapChainImages-1] of boolean;
    VulkanCommandBuffers:array[0..MaxSwapChainImages-1] of TVulkanCommandBuffer;
    VulkanCommandBufferFences:array[0..MaxSwapChainImages-1] of TVulkanFence;
    VulkanCommandBufferFencesReady:array[0..MaxSwapChainImages-1] of boolean;
    VulkanPresentCompleteSemaphores:array[0..MaxSwapChainImages-1] of TVulkanSemaphore;
    VulkanDrawCompleteSemaphores:array[0..MaxSwapChainImages-1] of TVulkanSemaphore;

    DoNeedToRecreateVulkanSwapChain:boolean=false;
    EnableDebugging:boolean=false;

    TriangleVertexShaderModule:TVulkanShaderModule=nil;
    TriangleFragmentShaderModule:TVulkanShaderModule=nil;

    VulkanPipelineShaderStageTriangleVertex:TVulkanPipelineShaderStage=nil;
    VulkanPipelineShaderStageTriangleFragment:TVulkanPipelineShaderStage=nil;

    VulkanPipelineCache:TVulkanPipelineCache=nil;

    VulkanPipelineLayout:TVulkanPipelineLayout=nil;

    VulkanGraphicsPipeline:TVulkanGraphicsPipeline=nil;

var DebugLast:TVulkanCharString='';

procedure DebugLn(const s:TVulkanCharString);
begin
 if DebugLast<>s then begin
  DebugLast:=s;
  WriteLn(s);
 end;
end;

type TVulkanDebug=class
      function OnDebugReportCallback(const flags:TVkDebugReportFlagsEXT;const objectType:TVkDebugReportObjectTypeEXT;const object_:TVkUInt64;const location:TVkSize;messageCode:TVkInt32;const pLayerPrefix:TVulkaNCharString;const pMessage:TVulkanCharString):TVkBool32;
     end;

function TVulkanDebug.OnDebugReportCallback(const flags:TVkDebugReportFlagsEXT;const objectType:TVkDebugReportObjectTypeEXT;const object_:TVkUInt64;const location:TVkSize;messageCode:TVkInt32;const pLayerPrefix:TVulkaNCharString;const pMessage:TVulkanCharString):TVkBool32;
begin
 DebugLn('[Debug] '+pLayerPrefix+': '+pMessage);
 result:=VK_FALSE;
end;

var VulkanDebug:TVulkanDebug=nil;

procedure RecreateVulkanGraphicsPipeline;
begin
 FreeAndNil(VulkanGraphicsPipeline);

 VulkanGraphicsPipeline:=TVulkanGraphicsPipeline.Create(VulkanDevice,
                                                        VulkanPipelineCache,
                                                        0,
                                                        [],
                                                        VulkanPipelineLayout,
                                                        VulkanSwapChainSimpleDirectRenderTarget.RenderPass,
                                                        0,
                                                        nil,
                                                        0);

 VulkanGraphicsPipeline.AddStage(VulkanPipelineShaderStageTriangleVertex);
 VulkanGraphicsPipeline.AddStage(VulkanPipelineShaderStageTriangleFragment);

 VulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 VulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 VulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,SurfaceWidth,SurfaceHeight,0.0,1.0);
 VulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,SurfaceWidth,SurfaceHeight);

 VulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 VulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 VulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
 VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_CLOCKWISE;
 VulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
 VulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
 VulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
 VulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
 VulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

 VulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=VK_SAMPLE_COUNT_1_BIT;
 VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=false;
 VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=0.0;
 VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
 VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=false;
 VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;

 VulkanGraphicsPipeline.ColorBlendState.LogicOpEnable:=false;
 VulkanGraphicsPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
 VulkanGraphicsPipeline.ColorBlendState.BlendConstants[0]:=0.0;
 VulkanGraphicsPipeline.ColorBlendState.BlendConstants[1]:=0.0;
 VulkanGraphicsPipeline.ColorBlendState.BlendConstants[2]:=0.0;
 VulkanGraphicsPipeline.ColorBlendState.BlendConstants[3]:=0.0;
 VulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
                                                                     VK_BLEND_FACTOR_ZERO,
                                                                     VK_BLEND_FACTOR_ZERO,
                                                                     VK_BLEND_OP_ADD,
                                                                     VK_BLEND_FACTOR_ZERO,
                                                                     VK_BLEND_FACTOR_ZERO,
                                                                     VK_BLEND_OP_ADD,
                                                                     TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                     TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                     TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                     TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

 VulkanGraphicsPipeline.Initialize;

 VulkanGraphicsPipeline.FreeMemory;
 
end;

procedure VulkanDraw;
var Tries,CurrentImageIndex:TVkInt32;
    OldVulkanSwapChain:TVulkanSwapChain;
    OK:boolean;
    VulkanCommandBuffer:TVulkanCommandBuffer;
begin

 CurrentImageIndex:=VulkanSwapChain.CurrentImageIndex;

 if VulkanSwapChainImageFencesReady[CurrentImageIndex] then begin
  VulkanSwapChainImageFences[CurrentImageIndex].WaitFor;
  VulkanSwapChainImageFences[CurrentImageIndex].Reset;
  VulkanSwapChainImageFencesReady[CurrentImageIndex]:=false;
 end;

 if VulkanCommandBufferFencesReady[CurrentImageIndex] then begin
  VulkanCommandBufferFences[CurrentImageIndex].WaitFor;
  VulkanCommandBufferFences[CurrentImageIndex].Reset;
  VulkanCommandBufferFencesReady[CurrentImageIndex]:=false;
 end;

 for Tries:=1 to 2 do begin

  OK:=false;

  if (VulkanSwapChain.Width<>SurfaceWidth) or (VulkanSwapChain.Height<>SurfaceHeight) then begin
   DoNeedToRecreateVulkanSwapChain:=true;
   writeln('New surface dimension size detected!');
  end else begin
   try
    if VulkanSwapChain.AcquireNextImage(VulkanPresentCompleteSemaphores[CurrentImageIndex],VulkanSwapChainImageFences[CurrentImageIndex])=VK_SUBOPTIMAL_KHR then begin
     DoNeedToRecreateVulkanSwapChain:=true;
     writeln('Suboptimal surface detected!');
    end else begin
     VulkanSwapChainImageFencesReady[CurrentImageIndex]:=true;
    end;
   except
    on VulkanResultException:EVulkanResultException do begin
     case VulkanResultException.ResultCode of
      VK_ERROR_SURFACE_LOST_KHR,
      VK_ERROR_OUT_OF_DATE_KHR,
      VK_SUBOPTIMAL_KHR:begin
       DoNeedToRecreateVulkanSwapChain:=true;
       writeln(VulkanResultException.ClassName,': ',VulkanResultException.Message);
      end;
      else begin
       raise;
      end;
     end;
    end;
   end;
  end;

  if DoNeedToRecreateVulkanSwapChain then begin

   for CurrentImageIndex:=0 to MaxSwapChainImages-1 do begin
    if VulkanSwapChainImageFencesReady[CurrentImageIndex] then begin
     VulkanSwapChainImageFences[CurrentImageIndex].WaitFor;
     VulkanSwapChainImageFences[CurrentImageIndex].Reset;
     VulkanSwapChainImageFencesReady[CurrentImageIndex]:=false;
    end;
    if VulkanCommandBufferFencesReady[CurrentImageIndex] then begin
     VulkanCommandBufferFences[CurrentImageIndex].WaitFor;
     VulkanCommandBufferFences[CurrentImageIndex].Reset;
     VulkanCommandBufferFencesReady[CurrentImageIndex]:=false;
    end;
   end;

   VulkanDevice.WaitIdle;

   write('Recreating swap chain... ');
   DoNeedToRecreateVulkanSwapChain:=false;
   OldVulkanSwapChain:=VulkanSwapChain;
   try
    FreeAndNil(VulkanGraphicsPipeline);
    FreeAndNil(VulkanSwapChainSimpleDirectRenderTarget);
    VulkanSwapChain:=TVulkanSwapChain.Create(VulkanDevice,VulkanSurface,OldVulkanSwapChain,SurfaceWidth,SurfaceHeight,MaxSwapChainImages,1);
    VulkanSwapChainSimpleDirectRenderTarget:=TVulkanSwapChainSimpleDirectRenderTarget.Create(VulkanDevice,VulkanSwapChain,VulkanInitializationCommandBuffer,VulkanInitializationCommandBufferFence);
    RecreateVulkanGraphicsPipeline;
   finally
    OldVulkanSwapChain.Free;
   end;
   writeln('done!');

   CurrentImageIndex:=VulkanSwapChain.CurrentImageIndex;

  end else begin

   VulkanCommandBuffer:=VulkanCommandBuffers[VulkanSwapChain.CurrentImageIndex];

   VulkanCommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));

   VulkanCommandBuffer.BeginRecording;

   VulkanCommandBuffer.MetaCmdPresentToDrawImageBarrier(VulkanSwapChain.CurrentImage);

   VulkanSwapChainSimpleDirectRenderTarget.RenderPass.ClearValues[0].color.float32[0]:=(cos(Now*86400.0*2.0*pi)*0.5)+0.5;
   VulkanSwapChainSimpleDirectRenderTarget.RenderPass.ClearValues[0].color.float32[1]:=(sin(Now*86400.0*2.0*pi)*0.5)+0.5;
   VulkanSwapChainSimpleDirectRenderTarget.RenderPass.ClearValues[0].color.float32[2]:=(cos(Now*86400.0*pi*0.731)*0.5)+0.5;

   VulkanSwapChainSimpleDirectRenderTarget.RenderPass.BeginRenderPass(VulkanCommandBuffer,
                                                                      VulkanSwapChainSimpleDirectRenderTarget.FrameBuffer,
                                                                      VK_SUBPASS_CONTENTS_INLINE,
                                                                      0,0,VulkanSwapChain.Width,VulkanSwapChain.Height);
   VulkanCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,VulkanGraphicsPipeline.Handle);
   VulkanCommandBuffer.CmdDraw(3,1,0,0);
   VulkanSwapChainSimpleDirectRenderTarget.RenderPass.EndRenderPass(VulkanCommandBuffer);

   VulkanCommandBuffer.MetaCmdDrawToPresentImageBarrier(VulkanSwapChain.CurrentImage);

   VulkanCommandBuffer.EndRecording;

   VulkanCommandBuffer.Execute(VulkanDevice.GraphicsQueue,
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                               VulkanPresentCompleteSemaphores[CurrentImageIndex],
                               VulkanDrawCompleteSemaphores[CurrentImageIndex],
                               VulkanCommandBufferFences[CurrentImageIndex],
                               false);
   VulkanCommandBufferFencesReady[CurrentImageIndex]:=true;

   try
    if VulkanSwapChain.QueuePresent(VulkanDevice.GraphicsQueue,VulkanDrawCompleteSemaphores[CurrentImageIndex])<>VK_SUBOPTIMAL_KHR then begin
     //VulkanDevice.WaitIdle; // A GPU/CPU frame synchronization point only for debug cases here, when something got run wrong
     OK:=true;
    end else begin
     DoNeedToRecreateVulkanSwapChain:=true;
    end;
   except
    on VulkanResultException:EVulkanResultException do begin
     case VulkanResultException.ResultCode of
      VK_ERROR_SURFACE_LOST_KHR,
      VK_ERROR_OUT_OF_DATE_KHR,
      VK_SUBOPTIMAL_KHR:begin
       DoNeedToRecreateVulkanSwapChain:=true;
      end;
      else begin
       raise;
      end;
     end;
    end;
   end;

  end;

  if OK then begin
   break;
  end;
  
 end;
end;

function WindowProc(pWindow:HWND;pMsg:UINT;pWParam:WPARAM;pLParam:LPARAM):LRESULT; stdcall;
begin
 case pMsg of
  WM_CLOSE:begin
   PostQuitMessage(0);
   result:=0;
  end;
  WM_DESTROY:begin
   PostQuitMessage(0);
   result:=0;
  end;
  WM_KEYDOWN:begin
   case pWParam of
    VK_ESCAPE:begin
     SendMessage(pWindow,WM_CLOSE,0,0);
    end;
   end;
   result:=0;
  end;
  WM_COMMAND:begin
   result:=0;
  end;
  WM_SIZE:begin
   result:=DefWindowProc(pWindow,pMsg,pWParam,pLParam);
   SurfaceWidth:=LoWord(pLParam);
   SurfaceHeight:=HiWord(pLParam);
  end;
  else begin
   result:=DefWindowProc(pWindow,pMsg,pWParam,pLParam);
  end;
 end;
end;

{$ifdef Windows}
function GetConsoleWindow:HWND; stdcall; external kernel32 name 'GetConsoleWindow';
{$endif}

function MonitorEnumProc(pMonitor:HMONITOR;pHDC:HDC;lprcMonitor:PRECT;dwData:LPARAM):boolean; stdcall;
var mi:TMonitorInfoA;
begin
 mi.cbSize:=SizeOf(TMonitorInfoA);
 GetMonitorInfoA(pMonitor,@mi);
 if (mi.dwFlags and MONITORINFOF_PRIMARY)<>0 then begin
  HMONITOR(pointer(dwData)^):=pMonitor;
  result:=false;
 end else begin
  result:=true;
 end;
end;

function GetPrimaryMonitor:HMONITOR;
const PointZero:TPoint=(x:0;y:0);
begin
 if not EnumDisplayMonitors(0,nil,MonitorEnumProc,LPARAM(@result)) then begin
  result:=MonitorFromPoint(PointZero,MONITOR_DEFAULTTOPRIMARY);
 end;
end;

var Index,MonitorLeft,MonitorTop,MonitorWidth,MonitorHeight:TVkInt32;
    ConsoleHwnd:HWND;
    R:TRect;
    MonitorInfo:TMonitorInfoA;
begin

 MonitorInfo.cbSize:=SizeOf(TMonitorInfoA);
 GetMonitorInfoA(GetPrimaryMonitor,@MonitorInfo);

 MonitorLeft:=MonitorInfo.rcWork.Left;
 MonitorTop:=MonitorInfo.rcWork.Top;
 MonitorWidth:=MonitorInfo.rcWork.Right-MonitorInfo.rcWork.Left;
 MonitorHeight:=MonitorInfo.rcWork.Bottom-MonitorInfo.rcWork.Top;

 ConsoleHwnd:=GetConsoleWindow;
 GetWindowRect(ConsoleHwnd,R);
 SetWindowPos(ConsoleHwnd,
              0,
              MonitorLeft+(((MonitorWidth-((R.Right-R.Left)))*1) div 16),
              MonitorTop+((MonitorHeight-(R.Bottom-R.Top)) div 2),
              0,
              0,
              SWP_NOSIZE);

 FillChar(WndClass,SizeOf(TWndClass),#0);
 WndClass.style:=0;
 WndClass.lpfnWndProc:=@WindowProc;
 WndClass.cbClsExtra:=0;
 WndClass.cbWndExtra:=0;
 WndClass.hInstance:=hInstance;
 WndClass.hIcon:=0;
 WndClass.hCursor:=LoadCursor(0,IDC_ARROW);
 WndClass.hbrBackground:=COLOR_WINDOW;
 WndClass.lpszMenuName:=nil;
 WndClass.lpszClassName:='TVulkanExample';
 Windows.RegisterClass(WndClass);

 hWindow:=CreateWindow('TVulkanExample',
                       'Vulkan Triangle',
                       WS_OVERLAPPEDWINDOW,
                       MonitorLeft+(((MonitorWidth-SurfaceWidth)*7) div 8),
                       MonitorTop+((MonitorHeight-SurfaceHeight) div 2),
                       SurfaceWidth,
                       SurfaceHeight,
                       0,
                       0,
                       hInstance,
                       nil);

 for Index:=0 to MaxSwapChainImages-1 do begin
  VulkanSwapChainImageFences[Index]:=nil;
  VulkanSwapChainImageFencesReady[Index]:=false;
  VulkanCommandBuffers[Index]:=nil;
  VulkanCommandBufferFences[Index]:=nil;
  VulkanCommandBufferFencesReady[Index]:=false;
  VulkanPresentCompleteSemaphores[Index]:=nil;
  VulkanDrawCompleteSemaphores[Index]:=nil;
 end;

 try

  try

   VulkanDebug:=TVulkanDebug.Create;

   VulkanInstance:=TVulkanInstance.Create('Test application',1,'Test engine',1,VK_API_VERSION_1_0,true);
   for Index:=0 to VulkanInstance.AvailableLayerNames.Count-1 do begin
    DebugLn('Layer: '+TVulkanCharString(VulkanInstance.AvailableLayerNames[Index]));
   end;
   for Index:=0 to VulkanInstance.AvailableExtensionNames.Count-1 do begin
    DebugLn('Extension: '+TVulkanCharString(VulkanInstance.AvailableExtensionNames[Index]));
   end;
   VulkanInstance.EnabledExtensionNames.Add(VK_KHR_SURFACE_EXTENSION_NAME);
{$if defined(Android)}
   VulkanInstance.EnabledExtensionNames.Add(VK_KHR_ANDROID_SURFACE_EXTENSION_NAME);
{$elseif defined(Mir)}
   VulkanInstance.EnabledExtensionNames.Add(VK_KHR_MIR_SURFACE_EXTENSION_NAME);
{$elseif defined(Wayland)}
   VulkanInstance.EnabledExtensionNames.Add(VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME);
{$elseif defined(Windows)}
   VulkanInstance.EnabledExtensionNames.Add(VK_KHR_WIN32_SURFACE_EXTENSION_NAME);
{$elseif defined(X11)}
   VulkanInstance.EnabledExtensionNames.Add(VK_KHR_X11_SURFACE_EXTENSION_NAME);
{$elseif defined(XCB)}
   VulkanInstance.EnabledExtensionNames.Add(VK_KHR_XCB_SURFACE_EXTENSION_NAME);
{$ifend}
   if VulkanInstance.AvailableExtensionNames.IndexOf(VK_EXT_DEBUG_REPORT_EXTENSION_NAME)>=0 then begin
    VulkanInstance.EnabledExtensionNames.Add(VK_EXT_DEBUG_REPORT_EXTENSION_NAME);
    EnableDebugging:=true;
   end else begin
    EnableDebugging:=false;
   end;
{$define VULKAN_VALIDATION}
{$ifdef VULKAN_VALIDATION}
   if VulkanInstance.AvailableLayerNames.IndexOf('VK_LAYER_LUNARG_standard_validation')>=0 then begin
    VulkanInstance.EnabledLayerNames.Add('VK_LAYER_LUNARG_standard_validation');
   end;
{$endif}
   VulkanInstance.Initialize;
   if EnableDebugging then begin
    VulkanInstance.OnInstanceDebugReportCallback:=VulkanDebug.OnDebugReportCallback;
    VulkanInstance.InstallDebugReportCallback;
   end;

   VulkanSurface:=TVulkanSurface.CreateWin32(VulkanInstance,hInstance,hWindow);

   if GetClientRect(hWindow,R) then begin
    SurfaceWidth:=R.Right-R.Left;
    SurfaceHeight:=R.Bottom-R.Top;
   end;

   VulkanDevice:=TVulkanDevice.Create(VulkanInstance,nil,VulkanSurface);
   VulkanDevice.AddQueues;
   VulkanDevice.EnabledExtensionNames.Add(VK_KHR_SWAPCHAIN_EXTENSION_NAME);
   VulkanDevice.Initialize;

   VulkanInitializationCommandBufferFence:=TVulkanFence.Create(VulkanDevice);

   VulkanInitializationCommandPool:=TVulkanCommandPool.Create(VulkanDevice,VulkanDevice.GraphicsQueueFamilyIndex,TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

   VulkanInitializationCommandBuffer:=TVulkanCommandBuffer.Create(VulkanInitializationCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

   VulkanSwapChain:=TVulkanSwapChain.Create(VulkanDevice,VulkanSurface,nil,SurfaceWidth,SurfaceHeight,MaxSwapChainImages,1);

   VulkanSwapChainSimpleDirectRenderTarget:=TVulkanSwapChainSimpleDirectRenderTarget.Create(VulkanDevice,VulkanSwapChain,VulkanInitializationCommandBuffer,VulkanInitializationCommandBufferFence);

   VulkanCommandPool:=TVulkanCommandPool.Create(VulkanDevice,VulkanDevice.GraphicsQueueFamilyIndex,TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

   for Index:=0 to MaxSwapChainImages-1 do begin
    VulkanSwapChainImageFences[Index]:=TVulkanFence.Create(VulkanDevice);
    VulkanCommandBuffers[Index]:=TVulkanCommandBuffer.Create(VulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
    VulkanCommandBufferFences[Index]:=TVulkanFence.Create(VulkanDevice);
    VulkanPresentCompleteSemaphores[Index]:=TVulkanSemaphore.Create(VulkanDevice);
    VulkanDrawCompleteSemaphores[Index]:=TVulkanSemaphore.Create(VulkanDevice);
   end;

   DoNeedToRecreateVulkanSwapChain:=false;

   TriangleVertexShaderModule:=TVulkanShaderModule.Create(VulkanDevice,'triangle_vert.spv');

   TriangleFragmentShaderModule:=TVulkanShaderModule.Create(VulkanDevice,'triangle_frag.spv');

   VulkanPipelineShaderStageTriangleVertex:=TVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,TriangleVertexShaderModule,'main');

   VulkanPipelineShaderStageTriangleFragment:=TVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,TriangleFragmentShaderModule,'main');

   VulkanPipelineCache:=TVulkanPipelineCache.Create(VulkanDevice);

   VulkanPipelineLayout:=TVulkanPipelineLayout.Create(VulkanDevice);
   VulkanPipelineLayout.Initialize;

   RecreateVulkanGraphicsPipeline;

   ShowWindow(hWindow,SW_SHOWNORMAL);

   Running:=true;
   repeat
    if PeekMessage(Msg,hWindow,0,0,0) then begin
     if Msg.message=WM_QUIT then begin
      Running:=false;
     end;
     if GetMessage(Msg,0,0,0) then begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
     end;
    end else begin 
     VulkanDraw;
    end;
   until not Running;

   for Index:=0 to MaxSwapChainImages-1 do begin
    if VulkanSwapChainImageFencesReady[Index] then begin
     VulkanSwapChainImageFences[Index].WaitFor;
     VulkanSwapChainImageFences[Index].Reset;
     VulkanSwapChainImageFencesReady[Index]:=false;
    end;
    if VulkanCommandBufferFencesReady[Index] then begin
     VulkanCommandBufferFences[Index].WaitFor;
     VulkanCommandBufferFences[Index].Reset;
     VulkanCommandBufferFencesReady[Index]:=false;
    end;
   end;

   VulkanDevice.WaitIdle;

  except
   on e:Exception do begin
    WriteLn(e.Message);
    ReadLn;
    raise;
   end;
  end;

 finally

  VulkanGraphicsPipeline.Free;

  VulkanPipelineLayout.Free;

  VulkanPipelineCache.Free;

  VulkanPipelineShaderStageTriangleVertex.Free;

  VulkanPipelineShaderStageTriangleFragment.Free;

  TriangleFragmentShaderModule.Free;
  TriangleVertexShaderModule.Free;

  for Index:=0 to MaxSwapChainImages-1 do begin
   VulkanSwapChainImageFences[Index].Free;
   VulkanCommandBufferFences[Index].Free;
   VulkanPresentCompleteSemaphores[Index].Free;
   VulkanDrawCompleteSemaphores[Index].Free;
   VulkanCommandBuffers[Index].Free;
  end;
  VulkanCommandPool.Free;
  VulkanSwapChainSimpleDirectRenderTarget.Free;
  VulkanSwapChain.Free;
  VulkanInitializationCommandBuffer.Free;
  VulkanInitializationCommandPool.Free;
  VulkanInitializationCommandBufferFence.Free;
  VulkanDevice.Free;
  VulkanSurface.Free;
  VulkanInstance.Free;
  VulkanDebug.Free;

  CloseWindow(hWindow);

 end;

end.

