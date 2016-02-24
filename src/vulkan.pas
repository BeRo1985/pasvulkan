(*
** Copyright (c) 2015-2016 The Khronos Group Inc.
** Copyright (c) 2016, Benjamin Rosseaux (benjamin@rosseaux.de, the pascal headers)
**
** Permission is hereby granted, free of charge, to any person obtaining a
** copy of this software and/or associated documentation files (the
** "Materials"), to deal in the Materials without restriction, including
** without limitation the rights to use, copy, modify, merge, publish,
** distribute, sublicense, and/or sell copies of the Materials, and to
** permit persons to whom the Materials are furnished to do so, subject to
** the following conditions:
**
** The above copyright notice and this permission notice shall be included
** in all copies or substantial portions of the Materials.
**
** THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
** MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
*)
(*
** This header is generated from the Khronos Vulkan XML API Registry.
**
*)
unit vulkan;
{$ifdef fpc}
 {$mode delphi}
 {$z4}
 {$packrecords c}
 {$define CAN_INLINE}
 {$notes off}
{$else}
 {$z4}
 {$undef CAN_INLINE}
 {$ifdef ver180}
  {$define CAN_INLINE}
 {$else}
  {$ifdef conditionalexpressions}
   {$if compilerversion>=18}
    {$define CAN_INLINE}
   {$ifend}
  {$endif}
 {$endif}
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

interface

uses {$ifdef Windows}Windows,{$endif}{$ifdef Unix}BaseUnix,UnixType,dl,{$endif}{$ifdef X11}x,xlib,{$endif}{$ifdef XCB}xcb,{$endif}{$ifdef Mir}Mir,{$endif}{$ifdef Wayland}Wayland,{$endif}{$ifdef Android}Android,{$endif}SysUtils;

const VK_DEFAULT_LIB_NAME={$ifdef Windows}'vulkan.dll'{$else}{$ifdef Unix}'libvulkan.so'{$else}'libvulkan'{$endif}{$endif};

type PPVkInt8=^PVkInt8;
     PVkInt8=^TVkInt8;
     TVkInt8=shortint;

     PPVkUInt8=^PVkUInt8;
     PVkUInt8=^TVkUInt8;
     TVkUInt8=byte;

     PPVkInt16=^PVkInt16;
     PVkInt16=^TVkInt16;
     TVkInt16=smallint;

     PPVkUInt16=^PVkUInt16;
     PVkUInt16=^TVkUInt16;
     TVkUInt16=word;

     PPVkInt32=^PVkInt32;
     PVkInt32=^TVkInt32;
     TVkInt32=longint;

     PPVkUInt32=^PVkUInt32;
     PVkUInt32=^TVkUInt32;
     TVkUInt32=longword;

     PPVkInt64=^PVkInt64;
     PVkInt64=^TVkInt64;
     TVkInt64=int64;

     PPVkUInt64=^PVkUInt64;
     PVkUInt64=^TVkUInt64;
     TVkUInt64=uint64;

     PPVkChar=^PVkChar;
     PVkChar=PAnsiChar;
     TVkChar=AnsiChar;

     PPVkPointer=^PVkPointer;
     PVkPointer=^TVkPointer;
     TVkPointer=pointer;

     PPVkFloat=^PVkFloat;
     PVkFloat=^TVkFloat;
     TVkFloat=single;

     PPVkDouble=^PVkDouble;
     PVkDouble=^TVkDouble;
     TVkDouble=double;

     PPVkPtrUInt=^PVkPtrUInt;
     PPVkPtrInt=^PVkPtrInt;
     PVkPtrUInt=^TVkPtrUInt;
     PVkPtrInt=^TVkPtrInt;
{$ifdef fpc}
     TVkPtrUInt=PtrUInt;
     TVkPtrInt=PtrInt;
 {$undef OldDelphi}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=23.0}
   {$undef OldDelphi}
     TVkPtrUInt=NativeUInt;
     TVkPtrInt=NativeInt;
  {$else}
   {$define OldDelphi}
  {$ifend}
 {$else}
  {$define OldDelphi}
 {$endif}
{$endif}
{$ifdef OldDelphi}
{$ifdef cpu64}
     TVkPtrUInt=uint64;
     TVkPtrInt=int64;
{$else}
     TVkPtrUInt=longword;
     TVkPtrInt=longint;
{$endif}
{$endif}

const VK_API_VERSION=(1 shl 22) or (0 shl 12) or (3 shl 0);

      VK_NULL_HANDLE=0;

      VK_NULL_INSTANCE=0;

      VK_KHR_SURFACE_SPEC_VERSION=25;
      VK_KHR_SURFACE_EXTENSION_NAME='VK_KHR_surface';
      VK_KHR_SWAPCHAIN_SPEC_VERSION=67;
      VK_KHR_SWAPCHAIN_EXTENSION_NAME='VK_KHR_swapchain';
      VK_KHR_DISPLAY_SPEC_VERSION=21;
      VK_KHR_DISPLAY_EXTENSION_NAME='VK_KHR_display';
      VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION=9;
      VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME='VK_KHR_display_swapchain';
      VK_KHR_XLIB_SURFACE_SPEC_VERSION=6;
      VK_KHR_XLIB_SURFACE_EXTENSION_NAME='VK_KHR_xlib_surface';
      VK_KHR_XCB_SURFACE_SPEC_VERSION=6;
      VK_KHR_XCB_SURFACE_EXTENSION_NAME='VK_KHR_xcb_surface';
      VK_KHR_WAYLAND_SURFACE_SPEC_VERSION=5;
      VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME='VK_KHR_wayland_surface';
      VK_KHR_MIR_SURFACE_SPEC_VERSION=4;
      VK_KHR_MIR_SURFACE_EXTENSION_NAME='VK_KHR_mir_surface';
      VK_KHR_ANDROID_SURFACE_SPEC_VERSION=6;
      VK_KHR_ANDROID_SURFACE_EXTENSION_NAME='VK_KHR_android_surface';
      VK_KHR_WIN32_SURFACE_SPEC_VERSION=5;
      VK_KHR_WIN32_SURFACE_EXTENSION_NAME='VK_KHR_win32_surface';
      VK_ANDROID_NATIVE_BUFFER_SPEC_VERSION=4;
      VK_ANDROID_NATIVE_BUFFER_NUMBER=11;
      VK_ANDROID_NATIVE_BUFFER_NAME='VK_ANDROID_native_buffer';
      VK_EXT_DEBUG_REPORT_SPEC_VERSION=1;
      VK_EXT_DEBUG_REPORT_EXTENSION_NAME='VK_EXT_debug_report';
      VK_NV_EXTENSION_0_SPEC_VERSION=0;
      VK_NV_EXTENSION_0_EXTENSION_NAME='VK_NV_extension_0';
      VK_NV_EXTENSION_1_SPEC_VERSION=0;
      VK_NV_EXTENSION_1_EXTENSION_NAME='VK_NV_extension_1';
      VK_MAX_PHYSICAL_DEVICE_NAME_SIZE=256;
      VK_UUID_SIZE=16;
      VK_MAX_EXTENSION_NAME_SIZE=256;
      VK_MAX_DESCRIPTION_SIZE=256;
      VK_MAX_MEMORY_TYPES=32;
      VK_MAX_MEMORY_HEAPS=16;
      VK_LOD_CLAMP_NONE=1000.0;
      VK_REMAINING_MIP_LEVELS=TVkUInt32($ffffffff);
      VK_REMAINING_ARRAY_LAYERS=TVkUInt32($ffffffff);
      VK_WHOLE_SIZE=TVkUInt64($ffffffffffffffff);
      VK_ATTACHMENT_UNUSED=TVkUInt32($ffffffff);
      VK_TRUE=1;
      VK_FALSE=0;
      VK_QUEUE_FAMILY_IGNORED=TVkUInt32($ffffffff);
      VK_SUBPASS_EXTERNAL=TVkUInt32($ffffffff);

type PPVkDispatchableHandle=^PVkDispatchableHandle;
     PVkDispatchableHandle=^TVkDispatchableHandle;
     TVkDispatchableHandle=TVkPtrInt;

     PPVkNonDispatchableHandle=^PVkNonDispatchableHandle;
     PVkNonDispatchableHandle=^TVkNonDispatchableHandle;
     TVkNonDispatchableHandle=TVkUInt64;

     PPVkEnum=^PVkEnum;
     PVkEnum=^TVkEnum;
     TVkEnum=TVkInt32;

{$ifdef Windows}
     PPVkHINSTANCE=^PVkHINSTANCE;
     PVkHINSTANCE=^TVkHINSTANCE;
     TVkHINSTANCE=TVkPtrUInt;

     PPVkHWND=^PVkHWND;
     PVkHWND=^TVkHWND;
     TVkHWND=HWND;
{$endif}

     PPVkSampleMask=^PVkSampleMask;
     PVkSampleMask=^TVkSampleMask;
     TVkSampleMask=TVkUInt32;

     PPVkBool32=^PVkBool32;
     PVkBool32=^TVkBool32;
     TVkBool32=TVkUInt32;

     PPVkFlags=^PVkFlags;
     PVkFlags=^TVkFlags;
     TVkFlags=TVkUInt32;

     PPVkDeviceSize=^PVkDeviceSize;
     PVkDeviceSize=^TVkDeviceSize;
     TVkDeviceSize=TVkUInt64;

     PPVkFramebufferCreateFlags=^PVkFramebufferCreateFlags;
     PVkFramebufferCreateFlags=^TVkFramebufferCreateFlags;
     TVkFramebufferCreateFlags=TVkFlags;

     PPVkQueryPoolCreateFlags=^PVkQueryPoolCreateFlags;
     PVkQueryPoolCreateFlags=^TVkQueryPoolCreateFlags;
     TVkQueryPoolCreateFlags=TVkFlags;

     PPVkRenderPassCreateFlags=^PVkRenderPassCreateFlags;
     PVkRenderPassCreateFlags=^TVkRenderPassCreateFlags;
     TVkRenderPassCreateFlags=TVkFlags;

     PPVkSamplerCreateFlags=^PVkSamplerCreateFlags;
     PVkSamplerCreateFlags=^TVkSamplerCreateFlags;
     TVkSamplerCreateFlags=TVkFlags;

     PPVkPipelineLayoutCreateFlags=^PVkPipelineLayoutCreateFlags;
     PVkPipelineLayoutCreateFlags=^TVkPipelineLayoutCreateFlags;
     TVkPipelineLayoutCreateFlags=TVkFlags;

     PPVkPipelineCacheCreateFlags=^PVkPipelineCacheCreateFlags;
     PVkPipelineCacheCreateFlags=^TVkPipelineCacheCreateFlags;
     TVkPipelineCacheCreateFlags=TVkFlags;

     PPVkPipelineDepthStencilStateCreateFlags=^PVkPipelineDepthStencilStateCreateFlags;
     PVkPipelineDepthStencilStateCreateFlags=^TVkPipelineDepthStencilStateCreateFlags;
     TVkPipelineDepthStencilStateCreateFlags=TVkFlags;

     PPVkPipelineDynamicStateCreateFlags=^PVkPipelineDynamicStateCreateFlags;
     PVkPipelineDynamicStateCreateFlags=^TVkPipelineDynamicStateCreateFlags;
     TVkPipelineDynamicStateCreateFlags=TVkFlags;

     PPVkPipelineColorBlendStateCreateFlags=^PVkPipelineColorBlendStateCreateFlags;
     PVkPipelineColorBlendStateCreateFlags=^TVkPipelineColorBlendStateCreateFlags;
     TVkPipelineColorBlendStateCreateFlags=TVkFlags;

     PPVkPipelineMultisampleStateCreateFlags=^PVkPipelineMultisampleStateCreateFlags;
     PVkPipelineMultisampleStateCreateFlags=^TVkPipelineMultisampleStateCreateFlags;
     TVkPipelineMultisampleStateCreateFlags=TVkFlags;

     PPVkPipelineRasterizationStateCreateFlags=^PVkPipelineRasterizationStateCreateFlags;
     PVkPipelineRasterizationStateCreateFlags=^TVkPipelineRasterizationStateCreateFlags;
     TVkPipelineRasterizationStateCreateFlags=TVkFlags;

     PPVkPipelineViewportStateCreateFlags=^PVkPipelineViewportStateCreateFlags;
     PVkPipelineViewportStateCreateFlags=^TVkPipelineViewportStateCreateFlags;
     TVkPipelineViewportStateCreateFlags=TVkFlags;

     PPVkPipelineTessellationStateCreateFlags=^PVkPipelineTessellationStateCreateFlags;
     PVkPipelineTessellationStateCreateFlags=^TVkPipelineTessellationStateCreateFlags;
     TVkPipelineTessellationStateCreateFlags=TVkFlags;

     PPVkPipelineInputAssemblyStateCreateFlags=^PVkPipelineInputAssemblyStateCreateFlags;
     PVkPipelineInputAssemblyStateCreateFlags=^TVkPipelineInputAssemblyStateCreateFlags;
     TVkPipelineInputAssemblyStateCreateFlags=TVkFlags;

     PPVkPipelineVertexInputStateCreateFlags=^PVkPipelineVertexInputStateCreateFlags;
     PVkPipelineVertexInputStateCreateFlags=^TVkPipelineVertexInputStateCreateFlags;
     TVkPipelineVertexInputStateCreateFlags=TVkFlags;

     PPVkPipelineShaderStageCreateFlags=^PVkPipelineShaderStageCreateFlags;
     PVkPipelineShaderStageCreateFlags=^TVkPipelineShaderStageCreateFlags;
     TVkPipelineShaderStageCreateFlags=TVkFlags;

     PPVkDescriptorSetLayoutCreateFlags=^PVkDescriptorSetLayoutCreateFlags;
     PVkDescriptorSetLayoutCreateFlags=^TVkDescriptorSetLayoutCreateFlags;
     TVkDescriptorSetLayoutCreateFlags=TVkFlags;

     PPVkBufferViewCreateFlags=^PVkBufferViewCreateFlags;
     PVkBufferViewCreateFlags=^TVkBufferViewCreateFlags;
     TVkBufferViewCreateFlags=TVkFlags;

     PPVkInstanceCreateFlags=^PVkInstanceCreateFlags;
     PVkInstanceCreateFlags=^TVkInstanceCreateFlags;
     TVkInstanceCreateFlags=TVkFlags;

     PPVkDeviceCreateFlags=^PVkDeviceCreateFlags;
     PVkDeviceCreateFlags=^TVkDeviceCreateFlags;
     TVkDeviceCreateFlags=TVkFlags;

     PPVkDeviceQueueCreateFlags=^PVkDeviceQueueCreateFlags;
     PVkDeviceQueueCreateFlags=^TVkDeviceQueueCreateFlags;
     TVkDeviceQueueCreateFlags=TVkFlags;

     PPVkQueueFlags=^PVkQueueFlags;
     PVkQueueFlags=^TVkQueueFlags;
     TVkQueueFlags=TVkFlags;

     PPVkMemoryPropertyFlags=^PVkMemoryPropertyFlags;
     PVkMemoryPropertyFlags=^TVkMemoryPropertyFlags;
     TVkMemoryPropertyFlags=TVkFlags;

     PPVkMemoryHeapFlags=^PVkMemoryHeapFlags;
     PVkMemoryHeapFlags=^TVkMemoryHeapFlags;
     TVkMemoryHeapFlags=TVkFlags;

     PPVkAccessFlags=^PVkAccessFlags;
     PVkAccessFlags=^TVkAccessFlags;
     TVkAccessFlags=TVkFlags;

     PPVkBufferUsageFlags=^PVkBufferUsageFlags;
     PVkBufferUsageFlags=^TVkBufferUsageFlags;
     TVkBufferUsageFlags=TVkFlags;

     PPVkBufferCreateFlags=^PVkBufferCreateFlags;
     PVkBufferCreateFlags=^TVkBufferCreateFlags;
     TVkBufferCreateFlags=TVkFlags;

     PPVkShaderStageFlags=^PVkShaderStageFlags;
     PVkShaderStageFlags=^TVkShaderStageFlags;
     TVkShaderStageFlags=TVkFlags;

     PPVkImageUsageFlags=^PVkImageUsageFlags;
     PVkImageUsageFlags=^TVkImageUsageFlags;
     TVkImageUsageFlags=TVkFlags;

     PPVkImageCreateFlags=^PVkImageCreateFlags;
     PVkImageCreateFlags=^TVkImageCreateFlags;
     TVkImageCreateFlags=TVkFlags;

     PPVkImageViewCreateFlags=^PVkImageViewCreateFlags;
     PVkImageViewCreateFlags=^TVkImageViewCreateFlags;
     TVkImageViewCreateFlags=TVkFlags;

     PPVkPipelineCreateFlags=^PVkPipelineCreateFlags;
     PVkPipelineCreateFlags=^TVkPipelineCreateFlags;
     TVkPipelineCreateFlags=TVkFlags;

     PPVkColorComponentFlags=^PVkColorComponentFlags;
     PVkColorComponentFlags=^TVkColorComponentFlags;
     TVkColorComponentFlags=TVkFlags;

     PPVkFenceCreateFlags=^PVkFenceCreateFlags;
     PVkFenceCreateFlags=^TVkFenceCreateFlags;
     TVkFenceCreateFlags=TVkFlags;

     PPVkSemaphoreCreateFlags=^PVkSemaphoreCreateFlags;
     PVkSemaphoreCreateFlags=^TVkSemaphoreCreateFlags;
     TVkSemaphoreCreateFlags=TVkFlags;

     PPVkFormatFeatureFlags=^PVkFormatFeatureFlags;
     PVkFormatFeatureFlags=^TVkFormatFeatureFlags;
     TVkFormatFeatureFlags=TVkFlags;

     PPVkQueryControlFlags=^PVkQueryControlFlags;
     PVkQueryControlFlags=^TVkQueryControlFlags;
     TVkQueryControlFlags=TVkFlags;

     PPVkQueryResultFlags=^PVkQueryResultFlags;
     PVkQueryResultFlags=^TVkQueryResultFlags;
     TVkQueryResultFlags=TVkFlags;

     PPVkShaderModuleCreateFlags=^PVkShaderModuleCreateFlags;
     PVkShaderModuleCreateFlags=^TVkShaderModuleCreateFlags;
     TVkShaderModuleCreateFlags=TVkFlags;

     PPVkEventCreateFlags=^PVkEventCreateFlags;
     PVkEventCreateFlags=^TVkEventCreateFlags;
     TVkEventCreateFlags=TVkFlags;

     PPVkCommandPoolCreateFlags=^PVkCommandPoolCreateFlags;
     PVkCommandPoolCreateFlags=^TVkCommandPoolCreateFlags;
     TVkCommandPoolCreateFlags=TVkFlags;

     PPVkCommandPoolResetFlags=^PVkCommandPoolResetFlags;
     PVkCommandPoolResetFlags=^TVkCommandPoolResetFlags;
     TVkCommandPoolResetFlags=TVkFlags;

     PPVkCommandBufferResetFlags=^PVkCommandBufferResetFlags;
     PVkCommandBufferResetFlags=^TVkCommandBufferResetFlags;
     TVkCommandBufferResetFlags=TVkFlags;

     PPVkCommandBufferUsageFlags=^PVkCommandBufferUsageFlags;
     PVkCommandBufferUsageFlags=^TVkCommandBufferUsageFlags;
     TVkCommandBufferUsageFlags=TVkFlags;

     PPVkQueryPipelineStatisticFlags=^PVkQueryPipelineStatisticFlags;
     PVkQueryPipelineStatisticFlags=^TVkQueryPipelineStatisticFlags;
     TVkQueryPipelineStatisticFlags=TVkFlags;

     PPVkMemoryMapFlags=^PVkMemoryMapFlags;
     PVkMemoryMapFlags=^TVkMemoryMapFlags;
     TVkMemoryMapFlags=TVkFlags;

     PPVkImageAspectFlags=^PVkImageAspectFlags;
     PVkImageAspectFlags=^TVkImageAspectFlags;
     TVkImageAspectFlags=TVkFlags;

     PPVkSparseMemoryBindFlags=^PVkSparseMemoryBindFlags;
     PVkSparseMemoryBindFlags=^TVkSparseMemoryBindFlags;
     TVkSparseMemoryBindFlags=TVkFlags;

     PPVkSparseImageFormatFlags=^PVkSparseImageFormatFlags;
     PVkSparseImageFormatFlags=^TVkSparseImageFormatFlags;
     TVkSparseImageFormatFlags=TVkFlags;

     PPVkSubpassDescriptionFlags=^PVkSubpassDescriptionFlags;
     PVkSubpassDescriptionFlags=^TVkSubpassDescriptionFlags;
     TVkSubpassDescriptionFlags=TVkFlags;

     PPVkPipelineStageFlags=^PVkPipelineStageFlags;
     PVkPipelineStageFlags=^TVkPipelineStageFlags;
     TVkPipelineStageFlags=TVkFlags;

     PPVkSampleCountFlags=^PVkSampleCountFlags;
     PVkSampleCountFlags=^TVkSampleCountFlags;
     TVkSampleCountFlags=TVkFlags;

     PPVkAttachmentDescriptionFlags=^PVkAttachmentDescriptionFlags;
     PVkAttachmentDescriptionFlags=^TVkAttachmentDescriptionFlags;
     TVkAttachmentDescriptionFlags=TVkFlags;

     PPVkStencilFaceFlags=^PVkStencilFaceFlags;
     PVkStencilFaceFlags=^TVkStencilFaceFlags;
     TVkStencilFaceFlags=TVkFlags;

     PPVkCullModeFlags=^PVkCullModeFlags;
     PVkCullModeFlags=^TVkCullModeFlags;
     TVkCullModeFlags=TVkFlags;

     PPVkDescriptorPoolCreateFlags=^PVkDescriptorPoolCreateFlags;
     PVkDescriptorPoolCreateFlags=^TVkDescriptorPoolCreateFlags;
     TVkDescriptorPoolCreateFlags=TVkFlags;

     PPVkDescriptorPoolResetFlags=^PVkDescriptorPoolResetFlags;
     PVkDescriptorPoolResetFlags=^TVkDescriptorPoolResetFlags;
     TVkDescriptorPoolResetFlags=TVkFlags;

     PPVkDependencyFlags=^PVkDependencyFlags;
     PVkDependencyFlags=^TVkDependencyFlags;
     TVkDependencyFlags=TVkFlags;

     PPVkCompositeAlphaFlagsKHR=^PVkCompositeAlphaFlagsKHR;
     PVkCompositeAlphaFlagsKHR=^TVkCompositeAlphaFlagsKHR;
     TVkCompositeAlphaFlagsKHR=TVkFlags;

     PPVkDisplayPlaneAlphaFlagsKHR=^PVkDisplayPlaneAlphaFlagsKHR;
     PVkDisplayPlaneAlphaFlagsKHR=^TVkDisplayPlaneAlphaFlagsKHR;
     TVkDisplayPlaneAlphaFlagsKHR=TVkFlags;

     PPVkSurfaceTransformFlagsKHR=^PVkSurfaceTransformFlagsKHR;
     PVkSurfaceTransformFlagsKHR=^TVkSurfaceTransformFlagsKHR;
     TVkSurfaceTransformFlagsKHR=TVkFlags;

     PPVkSwapchainCreateFlagsKHR=^PVkSwapchainCreateFlagsKHR;
     PVkSwapchainCreateFlagsKHR=^TVkSwapchainCreateFlagsKHR;
     TVkSwapchainCreateFlagsKHR=TVkFlags;

     PPVkDisplayModeCreateFlagsKHR=^PVkDisplayModeCreateFlagsKHR;
     PVkDisplayModeCreateFlagsKHR=^TVkDisplayModeCreateFlagsKHR;
     TVkDisplayModeCreateFlagsKHR=TVkFlags;

     PPVkDisplaySurfaceCreateFlagsKHR=^PVkDisplaySurfaceCreateFlagsKHR;
     PVkDisplaySurfaceCreateFlagsKHR=^TVkDisplaySurfaceCreateFlagsKHR;
     TVkDisplaySurfaceCreateFlagsKHR=TVkFlags;

     PPVkAndroidSurfaceCreateFlagsKHR=^PVkAndroidSurfaceCreateFlagsKHR;
     PVkAndroidSurfaceCreateFlagsKHR=^TVkAndroidSurfaceCreateFlagsKHR;
     TVkAndroidSurfaceCreateFlagsKHR=TVkFlags;

     PPVkMirSurfaceCreateFlagsKHR=^PVkMirSurfaceCreateFlagsKHR;
     PVkMirSurfaceCreateFlagsKHR=^TVkMirSurfaceCreateFlagsKHR;
     TVkMirSurfaceCreateFlagsKHR=TVkFlags;

     PPVkWaylandSurfaceCreateFlagsKHR=^PVkWaylandSurfaceCreateFlagsKHR;
     PVkWaylandSurfaceCreateFlagsKHR=^TVkWaylandSurfaceCreateFlagsKHR;
     TVkWaylandSurfaceCreateFlagsKHR=TVkFlags;

     PPVkWin32SurfaceCreateFlagsKHR=^PVkWin32SurfaceCreateFlagsKHR;
     PVkWin32SurfaceCreateFlagsKHR=^TVkWin32SurfaceCreateFlagsKHR;
     TVkWin32SurfaceCreateFlagsKHR=TVkFlags;

     PPVkXlibSurfaceCreateFlagsKHR=^PVkXlibSurfaceCreateFlagsKHR;
     PVkXlibSurfaceCreateFlagsKHR=^TVkXlibSurfaceCreateFlagsKHR;
     TVkXlibSurfaceCreateFlagsKHR=TVkFlags;

     PPVkXcbSurfaceCreateFlagsKHR=^PVkXcbSurfaceCreateFlagsKHR;
     PVkXcbSurfaceCreateFlagsKHR=^TVkXcbSurfaceCreateFlagsKHR;
     TVkXcbSurfaceCreateFlagsKHR=TVkFlags;

     PPVkDebugReportFlagsEXT=^PVkDebugReportFlagsEXT;
     PVkDebugReportFlagsEXT=^TVkDebugReportFlagsEXT;
     TVkDebugReportFlagsEXT=TVkFlags;

     PPVkInstance=^PVkInstance;
     PVkInstance=^TVkInstance;
     TVkInstance=TVkDispatchableHandle;

     PPVkPhysicalDevice=^PVkPhysicalDevice;
     PVkPhysicalDevice=^TVkPhysicalDevice;
     TVkPhysicalDevice=TVkDispatchableHandle;

     PPVkDevice=^PVkDevice;
     PVkDevice=^TVkDevice;
     TVkDevice=TVkDispatchableHandle;

     PPVkQueue=^PVkQueue;
     PVkQueue=^TVkQueue;
     TVkQueue=TVkDispatchableHandle;

     PPVkCommandBuffer=^PVkCommandBuffer;
     PVkCommandBuffer=^TVkCommandBuffer;
     TVkCommandBuffer=TVkDispatchableHandle;

     PPVkDeviceMemory=^PVkDeviceMemory;
     PVkDeviceMemory=^TVkDeviceMemory;
     TVkDeviceMemory=TVkNonDispatchableHandle;

     PPVkCommandPool=^PVkCommandPool;
     PVkCommandPool=^TVkCommandPool;
     TVkCommandPool=TVkNonDispatchableHandle;

     PPVkBuffer=^PVkBuffer;
     PVkBuffer=^TVkBuffer;
     TVkBuffer=TVkNonDispatchableHandle;

     PPVkBufferView=^PVkBufferView;
     PVkBufferView=^TVkBufferView;
     TVkBufferView=TVkNonDispatchableHandle;

     PPVkImage=^PVkImage;
     PVkImage=^TVkImage;
     TVkImage=TVkNonDispatchableHandle;

     PPVkImageView=^PVkImageView;
     PVkImageView=^TVkImageView;
     TVkImageView=TVkNonDispatchableHandle;

     PPVkShaderModule=^PVkShaderModule;
     PVkShaderModule=^TVkShaderModule;
     TVkShaderModule=TVkNonDispatchableHandle;

     PPVkPipeline=^PVkPipeline;
     PVkPipeline=^TVkPipeline;
     TVkPipeline=TVkNonDispatchableHandle;

     PPVkPipelineLayout=^PVkPipelineLayout;
     PVkPipelineLayout=^TVkPipelineLayout;
     TVkPipelineLayout=TVkNonDispatchableHandle;

     PPVkSampler=^PVkSampler;
     PVkSampler=^TVkSampler;
     TVkSampler=TVkNonDispatchableHandle;

     PPVkDescriptorSet=^PVkDescriptorSet;
     PVkDescriptorSet=^TVkDescriptorSet;
     TVkDescriptorSet=TVkNonDispatchableHandle;

     PPVkDescriptorSetLayout=^PVkDescriptorSetLayout;
     PVkDescriptorSetLayout=^TVkDescriptorSetLayout;
     TVkDescriptorSetLayout=TVkNonDispatchableHandle;

     PPVkDescriptorPool=^PVkDescriptorPool;
     PVkDescriptorPool=^TVkDescriptorPool;
     TVkDescriptorPool=TVkNonDispatchableHandle;

     PPVkFence=^PVkFence;
     PVkFence=^TVkFence;
     TVkFence=TVkNonDispatchableHandle;

     PPVkSemaphore=^PVkSemaphore;
     PVkSemaphore=^TVkSemaphore;
     TVkSemaphore=TVkNonDispatchableHandle;

     PPVkEvent=^PVkEvent;
     PVkEvent=^TVkEvent;
     TVkEvent=TVkNonDispatchableHandle;

     PPVkQueryPool=^PVkQueryPool;
     PVkQueryPool=^TVkQueryPool;
     TVkQueryPool=TVkNonDispatchableHandle;

     PPVkFramebuffer=^PVkFramebuffer;
     PVkFramebuffer=^TVkFramebuffer;
     TVkFramebuffer=TVkNonDispatchableHandle;

     PPVkRenderPass=^PVkRenderPass;
     PVkRenderPass=^TVkRenderPass;
     TVkRenderPass=TVkNonDispatchableHandle;

     PPVkPipelineCache=^PVkPipelineCache;
     PVkPipelineCache=^TVkPipelineCache;
     TVkPipelineCache=TVkNonDispatchableHandle;

     PPVkDisplayKHR=^PVkDisplayKHR;
     PVkDisplayKHR=^TVkDisplayKHR;
     TVkDisplayKHR=TVkNonDispatchableHandle;

     PPVkDisplayModeKHR=^PVkDisplayModeKHR;
     PVkDisplayModeKHR=^TVkDisplayModeKHR;
     TVkDisplayModeKHR=TVkNonDispatchableHandle;

     PPVkSurfaceKHR=^PVkSurfaceKHR;
     PVkSurfaceKHR=^TVkSurfaceKHR;
     TVkSurfaceKHR=TVkNonDispatchableHandle;

     PPVkSwapchainKHR=^PVkSwapchainKHR;
     PVkSwapchainKHR=^TVkSwapchainKHR;
     TVkSwapchainKHR=TVkNonDispatchableHandle;

     PPVkDebugReportCallbackEXT=^PVkDebugReportCallbackEXT;
     PVkDebugReportCallbackEXT=^TVkDebugReportCallbackEXT;
     TVkDebugReportCallbackEXT=TVkNonDispatchableHandle;

     PPVkImageLayout=^PVkImageLayout;
     PVkImageLayout=^TVkImageLayout;
     TVkImageLayout=
      (
       VK_IMAGE_LAYOUT_UNDEFINED=0,                                              // Implicit layout an image is when its contents are undefined due to various reasons (e.g. right after creation)
       VK_IMAGE_LAYOUT_BEGIN_RANGE=0,                                            // VK_IMAGE_LAYOUT_UNDEFINED
       VK_IMAGE_LAYOUT_GENERAL=1,                                                // General layout when image can be used for any kind of access
       VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL=2,                               // Optimal layout when image is only used for color attachment read/write
       VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL=3,                       // Optimal layout when image is only used for depth/stencil attachment read/write
       VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL=4,                        // Optimal layout when image is used for read only depth/stencil attachment and shader access
       VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL=5,                               // Optimal layout when image is used for read only shader access
       VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL=6,                                   // Optimal layout when image is used only as source of transfer operations
       VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL=7,                                   // Optimal layout when image is used only as destination of transfer operations
       VK_IMAGE_LAYOUT_PREINITIALIZED=8,                                         // Initial layout used when the data is populated by the CPU
       VK_IMAGE_LAYOUT_END_RANGE=8,                                              // VK_IMAGE_LAYOUT_PREINITIALIZED
       VK_IMAGE_LAYOUT_RANGE_SIZE=9,                                             // (VK_IMAGE_LAYOUT_PREINITIALIZED-VK_IMAGE_LAYOUT_UNDEFINED)+1
       VK_IMAGE_LAYOUT_PRESENT_SRC_KHR=1000001002,
       VK_IMAGE_LAYOUT_MAX_ENUM=$7fffffff
      );

     PPVkAttachmentLoadOp=^PVkAttachmentLoadOp;
     PVkAttachmentLoadOp=^TVkAttachmentLoadOp;
     TVkAttachmentLoadOp=
      (
       VK_ATTACHMENT_LOAD_OP_LOAD=0,
       VK_ATTACHMENT_LOAD_OP_BEGIN_RANGE=0,                                      // VK_ATTACHMENT_LOAD_OP_LOAD
       VK_ATTACHMENT_LOAD_OP_CLEAR=1,
       VK_ATTACHMENT_LOAD_OP_DONT_CARE=2,
       VK_ATTACHMENT_LOAD_OP_END_RANGE=2,                                        // VK_ATTACHMENT_LOAD_OP_DONT_CARE
       VK_ATTACHMENT_LOAD_OP_RANGE_SIZE=3,                                       // (VK_ATTACHMENT_LOAD_OP_DONT_CARE-VK_ATTACHMENT_LOAD_OP_LOAD)+1
       VK_ATTACHMENT_LOAD_OP_MAX_ENUM=$7fffffff
      );

     PPVkAttachmentStoreOp=^PVkAttachmentStoreOp;
     PVkAttachmentStoreOp=^TVkAttachmentStoreOp;
     TVkAttachmentStoreOp=
      (
       VK_ATTACHMENT_STORE_OP_STORE=0,
       VK_ATTACHMENT_STORE_OP_BEGIN_RANGE=0,                                     // VK_ATTACHMENT_STORE_OP_STORE
       VK_ATTACHMENT_STORE_OP_DONT_CARE=1,
       VK_ATTACHMENT_STORE_OP_END_RANGE=1,                                       // VK_ATTACHMENT_STORE_OP_DONT_CARE
       VK_ATTACHMENT_STORE_OP_RANGE_SIZE=2,                                      // (VK_ATTACHMENT_STORE_OP_DONT_CARE-VK_ATTACHMENT_STORE_OP_STORE)+1
       VK_ATTACHMENT_STORE_OP_MAX_ENUM=$7fffffff
      );

     PPVkImageType=^PVkImageType;
     PVkImageType=^TVkImageType;
     TVkImageType=
      (
       VK_IMAGE_TYPE_1D=0,
       VK_IMAGE_TYPE_BEGIN_RANGE=0,                                              // VK_IMAGE_TYPE_1D
       VK_IMAGE_TYPE_2D=1,
       VK_IMAGE_TYPE_3D=2,
       VK_IMAGE_TYPE_END_RANGE=2,                                                // VK_IMAGE_TYPE_3D
       VK_IMAGE_TYPE_RANGE_SIZE=3,                                               // (VK_IMAGE_TYPE_3D-VK_IMAGE_TYPE_1D)+1
       VK_IMAGE_TYPE_MAX_ENUM=$7fffffff
      );

     PPVkImageTiling=^PVkImageTiling;
     PVkImageTiling=^TVkImageTiling;
     TVkImageTiling=
      (
       VK_IMAGE_TILING_OPTIMAL=0,
       VK_IMAGE_TILING_BEGIN_RANGE=0,                                            // VK_IMAGE_TILING_OPTIMAL
       VK_IMAGE_TILING_LINEAR=1,
       VK_IMAGE_TILING_END_RANGE=1,                                              // VK_IMAGE_TILING_LINEAR
       VK_IMAGE_TILING_RANGE_SIZE=2,                                             // (VK_IMAGE_TILING_LINEAR-VK_IMAGE_TILING_OPTIMAL)+1
       VK_IMAGE_TILING_MAX_ENUM=$7fffffff
      );

     PPVkImageViewType=^PVkImageViewType;
     PVkImageViewType=^TVkImageViewType;
     TVkImageViewType=
      (
       VK_IMAGE_VIEW_TYPE_1D=0,
       VK_IMAGE_VIEW_TYPE_BEGIN_RANGE=0,                                         // VK_IMAGE_VIEW_TYPE_1D
       VK_IMAGE_VIEW_TYPE_2D=1,
       VK_IMAGE_VIEW_TYPE_3D=2,
       VK_IMAGE_VIEW_TYPE_CUBE=3,
       VK_IMAGE_VIEW_TYPE_1D_ARRAY=4,
       VK_IMAGE_VIEW_TYPE_2D_ARRAY=5,
       VK_IMAGE_VIEW_TYPE_CUBE_ARRAY=6,
       VK_IMAGE_VIEW_TYPE_END_RANGE=6,                                           // VK_IMAGE_VIEW_TYPE_CUBE_ARRAY
       VK_IMAGE_VIEW_TYPE_RANGE_SIZE=7,                                          // (VK_IMAGE_VIEW_TYPE_CUBE_ARRAY-VK_IMAGE_VIEW_TYPE_1D)+1
       VK_IMAGE_VIEW_TYPE_MAX_ENUM=$7fffffff
      );

     PPVkCommandBufferLevel=^PVkCommandBufferLevel;
     PVkCommandBufferLevel=^TVkCommandBufferLevel;
     TVkCommandBufferLevel=
      (
       VK_COMMAND_BUFFER_LEVEL_PRIMARY=0,
       VK_COMMAND_BUFFER_LEVEL_BEGIN_RANGE=0,                                    // VK_COMMAND_BUFFER_LEVEL_PRIMARY
       VK_COMMAND_BUFFER_LEVEL_SECONDARY=1,
       VK_COMMAND_BUFFER_LEVEL_END_RANGE=1,                                      // VK_COMMAND_BUFFER_LEVEL_SECONDARY
       VK_COMMAND_BUFFER_LEVEL_RANGE_SIZE=2,                                     // (VK_COMMAND_BUFFER_LEVEL_SECONDARY-VK_COMMAND_BUFFER_LEVEL_PRIMARY)+1
       VK_COMMAND_BUFFER_LEVEL_MAX_ENUM=$7fffffff
      );

     PPVkComponentSwizzle=^PVkComponentSwizzle;
     PVkComponentSwizzle=^TVkComponentSwizzle;
     TVkComponentSwizzle=
      (
       VK_COMPONENT_SWIZZLE_IDENTITY=0,
       VK_COMPONENT_SWIZZLE_BEGIN_RANGE=0,                                       // VK_COMPONENT_SWIZZLE_IDENTITY
       VK_COMPONENT_SWIZZLE_ZERO=1,
       VK_COMPONENT_SWIZZLE_ONE=2,
       VK_COMPONENT_SWIZZLE_R=3,
       VK_COMPONENT_SWIZZLE_G=4,
       VK_COMPONENT_SWIZZLE_B=5,
       VK_COMPONENT_SWIZZLE_A=6,
       VK_COMPONENT_SWIZZLE_END_RANGE=6,                                         // VK_COMPONENT_SWIZZLE_A
       VK_COMPONENT_SWIZZLE_RANGE_SIZE=7,                                        // (VK_COMPONENT_SWIZZLE_A-VK_COMPONENT_SWIZZLE_IDENTITY)+1
       VK_COMPONENT_SWIZZLE_MAX_ENUM=$7fffffff
      );

     PPVkDescriptorType=^PVkDescriptorType;
     PVkDescriptorType=^TVkDescriptorType;
     TVkDescriptorType=
      (
       VK_DESCRIPTOR_TYPE_SAMPLER=0,
       VK_DESCRIPTOR_TYPE_BEGIN_RANGE=0,                                         // VK_DESCRIPTOR_TYPE_SAMPLER
       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER=1,
       VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE=2,
       VK_DESCRIPTOR_TYPE_STORAGE_IMAGE=3,
       VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER=4,
       VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER=5,
       VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER=6,
       VK_DESCRIPTOR_TYPE_STORAGE_BUFFER=7,
       VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC=8,
       VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC=9,
       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT=10,
       VK_DESCRIPTOR_TYPE_END_RANGE=10,                                          // VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT
       VK_DESCRIPTOR_TYPE_RANGE_SIZE=11,                                         // (VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT-VK_DESCRIPTOR_TYPE_SAMPLER)+1
       VK_DESCRIPTOR_TYPE_MAX_ENUM=$7fffffff
      );

     PPVkQueryType=^PVkQueryType;
     PVkQueryType=^TVkQueryType;
     TVkQueryType=
      (
       VK_QUERY_TYPE_OCCLUSION=0,
       VK_QUERY_TYPE_BEGIN_RANGE=0,                                              // VK_QUERY_TYPE_OCCLUSION
       VK_QUERY_TYPE_PIPELINE_STATISTICS=1,                                      // Optional
       VK_QUERY_TYPE_TIMESTAMP=2,
       VK_QUERY_TYPE_END_RANGE=2,                                                // VK_QUERY_TYPE_TIMESTAMP
       VK_QUERY_TYPE_RANGE_SIZE=3,                                               // (VK_QUERY_TYPE_TIMESTAMP-VK_QUERY_TYPE_OCCLUSION)+1
       VK_QUERY_TYPE_MAX_ENUM=$7fffffff
      );

     PPVkBorderColor=^PVkBorderColor;
     PVkBorderColor=^TVkBorderColor;
     TVkBorderColor=
      (
       VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK=0,
       VK_BORDER_COLOR_BEGIN_RANGE=0,                                            // VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK
       VK_BORDER_COLOR_INT_TRANSPARENT_BLACK=1,
       VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK=2,
       VK_BORDER_COLOR_INT_OPAQUE_BLACK=3,
       VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE=4,
       VK_BORDER_COLOR_INT_OPAQUE_WHITE=5,
       VK_BORDER_COLOR_END_RANGE=5,                                              // VK_BORDER_COLOR_INT_OPAQUE_WHITE
       VK_BORDER_COLOR_RANGE_SIZE=6,                                             // (VK_BORDER_COLOR_INT_OPAQUE_WHITE-VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK)+1
       VK_BORDER_COLOR_MAX_ENUM=$7fffffff
      );

     PPVkPipelineBindPoint=^PVkPipelineBindPoint;
     PVkPipelineBindPoint=^TVkPipelineBindPoint;
     TVkPipelineBindPoint=
      (
       VK_PIPELINE_BIND_POINT_GRAPHICS=0,
       VK_PIPELINE_BIND_POINT_BEGIN_RANGE=0,                                     // VK_PIPELINE_BIND_POINT_GRAPHICS
       VK_PIPELINE_BIND_POINT_COMPUTE=1,
       VK_PIPELINE_BIND_POINT_END_RANGE=1,                                       // VK_PIPELINE_BIND_POINT_COMPUTE
       VK_PIPELINE_BIND_POINT_RANGE_SIZE=2,                                      // (VK_PIPELINE_BIND_POINT_COMPUTE-VK_PIPELINE_BIND_POINT_GRAPHICS)+1
       VK_PIPELINE_BIND_POINT_MAX_ENUM=$7fffffff
      );

     PPVkPipelineCacheHeaderVersion=^PVkPipelineCacheHeaderVersion;
     PVkPipelineCacheHeaderVersion=^TVkPipelineCacheHeaderVersion;
     TVkPipelineCacheHeaderVersion=
      (
       VK_PIPELINE_CACHE_HEADER_VERSION_ONE=1,
       VK_PIPELINE_CACHE_HEADER_VERSION_BEGIN_RANGE=1,                           // VK_PIPELINE_CACHE_HEADER_VERSION_ONE
       VK_PIPELINE_CACHE_HEADER_VERSION_END_RANGE=1,                             // VK_PIPELINE_CACHE_HEADER_VERSION_ONE
       VK_PIPELINE_CACHE_HEADER_VERSION_RANGE_SIZE=1,                            // (VK_PIPELINE_CACHE_HEADER_VERSION_ONE-VK_PIPELINE_CACHE_HEADER_VERSION_ONE)+1
       VK_PIPELINE_CACHE_HEADER_VERSION_MAX_ENUM=$7fffffff
      );

     PPVkPrimitiveTopology=^PVkPrimitiveTopology;
     PVkPrimitiveTopology=^TVkPrimitiveTopology;
     TVkPrimitiveTopology=
      (
       VK_PRIMITIVE_TOPOLOGY_POINT_LIST=0,
       VK_PRIMITIVE_TOPOLOGY_BEGIN_RANGE=0,                                      // VK_PRIMITIVE_TOPOLOGY_POINT_LIST
       VK_PRIMITIVE_TOPOLOGY_LINE_LIST=1,
       VK_PRIMITIVE_TOPOLOGY_LINE_STRIP=2,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST=3,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP=4,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN=5,
       VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY=6,
       VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY=7,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY=8,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY=9,
       VK_PRIMITIVE_TOPOLOGY_PATCH_LIST=10,
       VK_PRIMITIVE_TOPOLOGY_END_RANGE=10,                                       // VK_PRIMITIVE_TOPOLOGY_PATCH_LIST
       VK_PRIMITIVE_TOPOLOGY_RANGE_SIZE=11,                                      // (VK_PRIMITIVE_TOPOLOGY_PATCH_LIST-VK_PRIMITIVE_TOPOLOGY_POINT_LIST)+1
       VK_PRIMITIVE_TOPOLOGY_MAX_ENUM=$7fffffff
      );

     PPVkSharingMode=^PVkSharingMode;
     PVkSharingMode=^TVkSharingMode;
     TVkSharingMode=
      (
       VK_SHARING_MODE_EXCLUSIVE=0,
       VK_SHARING_MODE_BEGIN_RANGE=0,                                            // VK_SHARING_MODE_EXCLUSIVE
       VK_SHARING_MODE_CONCURRENT=1,
       VK_SHARING_MODE_END_RANGE=1,                                              // VK_SHARING_MODE_CONCURRENT
       VK_SHARING_MODE_RANGE_SIZE=2,                                             // (VK_SHARING_MODE_CONCURRENT-VK_SHARING_MODE_EXCLUSIVE)+1
       VK_SHARING_MODE_MAX_ENUM=$7fffffff
      );

     PPVkIndexType=^PVkIndexType;
     PVkIndexType=^TVkIndexType;
     TVkIndexType=
      (
       VK_INDEX_TYPE_UINT16=0,
       VK_INDEX_TYPE_BEGIN_RANGE=0,                                              // VK_INDEX_TYPE_UINT16
       VK_INDEX_TYPE_UINT32=1,
       VK_INDEX_TYPE_END_RANGE=1,                                                // VK_INDEX_TYPE_UINT32
       VK_INDEX_TYPE_RANGE_SIZE=2,                                               // (VK_INDEX_TYPE_UINT32-VK_INDEX_TYPE_UINT16)+1
       VK_INDEX_TYPE_MAX_ENUM=$7fffffff
      );

     PPVkFilter=^PVkFilter;
     PVkFilter=^TVkFilter;
     TVkFilter=
      (
       VK_FILTER_NEAREST=0,
       VK_FILTER_BEGIN_RANGE=0,                                                  // VK_FILTER_NEAREST
       VK_FILTER_LINEAR=1,
       VK_FILTER_END_RANGE=1,                                                    // VK_FILTER_LINEAR
       VK_FILTER_RANGE_SIZE=2,                                                   // (VK_FILTER_LINEAR-VK_FILTER_NEAREST)+1
       VK_FILTER_MAX_ENUM=$7fffffff
      );

     PPVkSamplerMipmapMode=^PVkSamplerMipmapMode;
     PVkSamplerMipmapMode=^TVkSamplerMipmapMode;
     TVkSamplerMipmapMode=
      (
       VK_SAMPLER_MIPMAP_MODE_NEAREST=0,                                         // Choose nearest mip level
       VK_SAMPLER_MIPMAP_MODE_BEGIN_RANGE=0,                                     // VK_SAMPLER_MIPMAP_MODE_NEAREST
       VK_SAMPLER_MIPMAP_MODE_LINEAR=1,                                          // Linear filter between mip levels
       VK_SAMPLER_MIPMAP_MODE_END_RANGE=1,                                       // VK_SAMPLER_MIPMAP_MODE_LINEAR
       VK_SAMPLER_MIPMAP_MODE_RANGE_SIZE=2,                                      // (VK_SAMPLER_MIPMAP_MODE_LINEAR-VK_SAMPLER_MIPMAP_MODE_NEAREST)+1
       VK_SAMPLER_MIPMAP_MODE_MAX_ENUM=$7fffffff
      );

     PPVkSamplerAddressMode=^PVkSamplerAddressMode;
     PVkSamplerAddressMode=^TVkSamplerAddressMode;
     TVkSamplerAddressMode=
      (
       VK_SAMPLER_ADDRESS_MODE_REPEAT=0,
       VK_SAMPLER_ADDRESS_MODE_BEGIN_RANGE=0,                                    // VK_SAMPLER_ADDRESS_MODE_REPEAT
       VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT=1,
       VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE=2,
       VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER=3,
       VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE=4,
       VK_SAMPLER_ADDRESS_MODE_END_RANGE=4,                                      // VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE
       VK_SAMPLER_ADDRESS_MODE_RANGE_SIZE=5,                                     // (VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE-VK_SAMPLER_ADDRESS_MODE_REPEAT)+1
       VK_SAMPLER_ADDRESS_MODE_MAX_ENUM=$7fffffff
      );

     PPVkCompareOp=^PVkCompareOp;
     PVkCompareOp=^TVkCompareOp;
     TVkCompareOp=
      (
       VK_COMPARE_OP_NEVER=0,
       VK_COMPARE_OP_BEGIN_RANGE=0,                                              // VK_COMPARE_OP_NEVER
       VK_COMPARE_OP_LESS=1,
       VK_COMPARE_OP_EQUAL=2,
       VK_COMPARE_OP_LESS_OR_EQUAL=3,
       VK_COMPARE_OP_GREATER=4,
       VK_COMPARE_OP_NOT_EQUAL=5,
       VK_COMPARE_OP_GREATER_OR_EQUAL=6,
       VK_COMPARE_OP_ALWAYS=7,
       VK_COMPARE_OP_END_RANGE=7,                                                // VK_COMPARE_OP_ALWAYS
       VK_COMPARE_OP_RANGE_SIZE=8,                                               // (VK_COMPARE_OP_ALWAYS-VK_COMPARE_OP_NEVER)+1
       VK_COMPARE_OP_MAX_ENUM=$7fffffff
      );

     PPVkPolygonMode=^PVkPolygonMode;
     PVkPolygonMode=^TVkPolygonMode;
     TVkPolygonMode=
      (
       VK_POLYGON_MODE_FILL=0,
       VK_POLYGON_MODE_BEGIN_RANGE=0,                                            // VK_POLYGON_MODE_FILL
       VK_POLYGON_MODE_LINE=1,
       VK_POLYGON_MODE_POINT=2,
       VK_POLYGON_MODE_END_RANGE=2,                                              // VK_POLYGON_MODE_POINT
       VK_POLYGON_MODE_RANGE_SIZE=3,                                             // (VK_POLYGON_MODE_POINT-VK_POLYGON_MODE_FILL)+1
       VK_POLYGON_MODE_MAX_ENUM=$7fffffff
      );

     PPVkCullModeFlagBits=^PVkCullModeFlagBits;
     PVkCullModeFlagBits=^TVkCullModeFlagBits;
     TVkCullModeFlagBits=
      (
       VK_CULL_MODE_NONE=$00000001,
       VK_CULL_MODE_FRONT_BIT=$00000001,
       VK_CULL_MODE_FRONT_AND_BACK=$00000001,
       VK_CULL_MODE_BACK_BIT=$00000002
      );

     PPVkFrontFace=^PVkFrontFace;
     PVkFrontFace=^TVkFrontFace;
     TVkFrontFace=
      (
       VK_FRONT_FACE_COUNTER_CLOCKWISE=0,
       VK_FRONT_FACE_BEGIN_RANGE=0,                                              // VK_FRONT_FACE_COUNTER_CLOCKWISE
       VK_FRONT_FACE_CLOCKWISE=1,
       VK_FRONT_FACE_END_RANGE=1,                                                // VK_FRONT_FACE_CLOCKWISE
       VK_FRONT_FACE_RANGE_SIZE=2,                                               // (VK_FRONT_FACE_CLOCKWISE-VK_FRONT_FACE_COUNTER_CLOCKWISE)+1
       VK_FRONT_FACE_MAX_ENUM=$7fffffff
      );

     PPVkBlendFactor=^PVkBlendFactor;
     PVkBlendFactor=^TVkBlendFactor;
     TVkBlendFactor=
      (
       VK_BLEND_FACTOR_ZERO=0,
       VK_BLEND_FACTOR_BEGIN_RANGE=0,                                            // VK_BLEND_FACTOR_ZERO
       VK_BLEND_FACTOR_ONE=1,
       VK_BLEND_FACTOR_SRC_COLOR=2,
       VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR=3,
       VK_BLEND_FACTOR_DST_COLOR=4,
       VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR=5,
       VK_BLEND_FACTOR_SRC_ALPHA=6,
       VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA=7,
       VK_BLEND_FACTOR_DST_ALPHA=8,
       VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA=9,
       VK_BLEND_FACTOR_CONSTANT_COLOR=10,
       VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR=11,
       VK_BLEND_FACTOR_CONSTANT_ALPHA=12,
       VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA=13,
       VK_BLEND_FACTOR_SRC_ALPHA_SATURATE=14,
       VK_BLEND_FACTOR_SRC1_COLOR=15,
       VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR=16,
       VK_BLEND_FACTOR_SRC1_ALPHA=17,
       VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA=18,
       VK_BLEND_FACTOR_END_RANGE=18,                                             // VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA
       VK_BLEND_FACTOR_RANGE_SIZE=19,                                            // (VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA-VK_BLEND_FACTOR_ZERO)+1
       VK_BLEND_FACTOR_MAX_ENUM=$7fffffff
      );

     PPVkBlendOp=^PVkBlendOp;
     PVkBlendOp=^TVkBlendOp;
     TVkBlendOp=
      (
       VK_BLEND_OP_ADD=0,
       VK_BLEND_OP_BEGIN_RANGE=0,                                                // VK_BLEND_OP_ADD
       VK_BLEND_OP_SUBTRACT=1,
       VK_BLEND_OP_REVERSE_SUBTRACT=2,
       VK_BLEND_OP_MIN=3,
       VK_BLEND_OP_MAX=4,
       VK_BLEND_OP_END_RANGE=4,                                                  // VK_BLEND_OP_MAX
       VK_BLEND_OP_RANGE_SIZE=5,                                                 // (VK_BLEND_OP_MAX-VK_BLEND_OP_ADD)+1
       VK_BLEND_OP_MAX_ENUM=$7fffffff
      );

     PPVkStencilOp=^PVkStencilOp;
     PVkStencilOp=^TVkStencilOp;
     TVkStencilOp=
      (
       VK_STENCIL_OP_KEEP=0,
       VK_STENCIL_OP_BEGIN_RANGE=0,                                              // VK_STENCIL_OP_KEEP
       VK_STENCIL_OP_ZERO=1,
       VK_STENCIL_OP_REPLACE=2,
       VK_STENCIL_OP_INCREMENT_AND_CLAMP=3,
       VK_STENCIL_OP_DECREMENT_AND_CLAMP=4,
       VK_STENCIL_OP_INVERT=5,
       VK_STENCIL_OP_INCREMENT_AND_WRAP=6,
       VK_STENCIL_OP_DECREMENT_AND_WRAP=7,
       VK_STENCIL_OP_END_RANGE=7,                                                // VK_STENCIL_OP_DECREMENT_AND_WRAP
       VK_STENCIL_OP_RANGE_SIZE=8,                                               // (VK_STENCIL_OP_DECREMENT_AND_WRAP-VK_STENCIL_OP_KEEP)+1
       VK_STENCIL_OP_MAX_ENUM=$7fffffff
      );

     PPVkLogicOp=^PVkLogicOp;
     PVkLogicOp=^TVkLogicOp;
     TVkLogicOp=
      (
       VK_LOGIC_OP_CLEAR=0,
       VK_LOGIC_OP_BEGIN_RANGE=0,                                                // VK_LOGIC_OP_CLEAR
       VK_LOGIC_OP_AND=1,
       VK_LOGIC_OP_AND_REVERSE=2,
       VK_LOGIC_OP_COPY=3,
       VK_LOGIC_OP_AND_INVERTED=4,
       VK_LOGIC_OP_NO_OP=5,
       VK_LOGIC_OP_XOR=6,
       VK_LOGIC_OP_OR=7,
       VK_LOGIC_OP_NOR=8,
       VK_LOGIC_OP_EQUIVALENT=9,
       VK_LOGIC_OP_INVERT=10,
       VK_LOGIC_OP_OR_REVERSE=11,
       VK_LOGIC_OP_COPY_INVERTED=12,
       VK_LOGIC_OP_OR_INVERTED=13,
       VK_LOGIC_OP_NAND=14,
       VK_LOGIC_OP_SET=15,
       VK_LOGIC_OP_END_RANGE=15,                                                 // VK_LOGIC_OP_SET
       VK_LOGIC_OP_RANGE_SIZE=16,                                                // (VK_LOGIC_OP_SET-VK_LOGIC_OP_CLEAR)+1
       VK_LOGIC_OP_MAX_ENUM=$7fffffff
      );

     PPVkInternalAllocationType=^PVkInternalAllocationType;
     PVkInternalAllocationType=^TVkInternalAllocationType;
     TVkInternalAllocationType=
      (
       VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE=0,
       VK_INTERNAL_ALLOCATION_TYPE_BEGIN_RANGE=0,                                // VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE
       VK_INTERNAL_ALLOCATION_TYPE_END_RANGE=0,                                  // VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE
       VK_INTERNAL_ALLOCATION_TYPE_RANGE_SIZE=1,                                 // (VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE-VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE)+1
       VK_INTERNAL_ALLOCATION_TYPE_MAX_ENUM=$7fffffff
      );

     PPVkSystemAllocationScope=^PVkSystemAllocationScope;
     PVkSystemAllocationScope=^TVkSystemAllocationScope;
     TVkSystemAllocationScope=
      (
       VK_SYSTEM_ALLOCATION_SCOPE_COMMAND=0,
       VK_SYSTEM_ALLOCATION_SCOPE_BEGIN_RANGE=0,                                 // VK_SYSTEM_ALLOCATION_SCOPE_COMMAND
       VK_SYSTEM_ALLOCATION_SCOPE_OBJECT=1,
       VK_SYSTEM_ALLOCATION_SCOPE_CACHE=2,
       VK_SYSTEM_ALLOCATION_SCOPE_DEVICE=3,
       VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE=4,
       VK_SYSTEM_ALLOCATION_SCOPE_END_RANGE=4,                                   // VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE
       VK_SYSTEM_ALLOCATION_SCOPE_RANGE_SIZE=5,                                  // (VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE-VK_SYSTEM_ALLOCATION_SCOPE_COMMAND)+1
       VK_SYSTEM_ALLOCATION_SCOPE_MAX_ENUM=$7fffffff
      );

     PPVkPhysicalDeviceType=^PVkPhysicalDeviceType;
     PVkPhysicalDeviceType=^TVkPhysicalDeviceType;
     TVkPhysicalDeviceType=
      (
       VK_PHYSICAL_DEVICE_TYPE_OTHER=0,
       VK_PHYSICAL_DEVICE_TYPE_BEGIN_RANGE=0,                                    // VK_PHYSICAL_DEVICE_TYPE_OTHER
       VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU=1,
       VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU=2,
       VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU=3,
       VK_PHYSICAL_DEVICE_TYPE_CPU=4,
       VK_PHYSICAL_DEVICE_TYPE_END_RANGE=4,                                      // VK_PHYSICAL_DEVICE_TYPE_CPU
       VK_PHYSICAL_DEVICE_TYPE_RANGE_SIZE=5,                                     // (VK_PHYSICAL_DEVICE_TYPE_CPU-VK_PHYSICAL_DEVICE_TYPE_OTHER)+1
       VK_PHYSICAL_DEVICE_TYPE_MAX_ENUM=$7fffffff
      );

     PPVkVertexInputRate=^PVkVertexInputRate;
     PVkVertexInputRate=^TVkVertexInputRate;
     TVkVertexInputRate=
      (
       VK_VERTEX_INPUT_RATE_VERTEX=0,
       VK_VERTEX_INPUT_RATE_BEGIN_RANGE=0,                                       // VK_VERTEX_INPUT_RATE_VERTEX
       VK_VERTEX_INPUT_RATE_INSTANCE=1,
       VK_VERTEX_INPUT_RATE_END_RANGE=1,                                         // VK_VERTEX_INPUT_RATE_INSTANCE
       VK_VERTEX_INPUT_RATE_RANGE_SIZE=2,                                        // (VK_VERTEX_INPUT_RATE_INSTANCE-VK_VERTEX_INPUT_RATE_VERTEX)+1
       VK_VERTEX_INPUT_RATE_MAX_ENUM=$7fffffff
      );

     PPVkFormat=^PVkFormat;
     PVkFormat=^TVkFormat;
     TVkFormat=
      (
       VK_FORMAT_UNDEFINED=0,
       VK_FORMAT_BEGIN_RANGE=0,                                                  // VK_FORMAT_UNDEFINED
       VK_FORMAT_R4G4_UNORM_PACK8=1,
       VK_FORMAT_R4G4B4A4_UNORM_PACK16=2,
       VK_FORMAT_B4G4R4A4_UNORM_PACK16=3,
       VK_FORMAT_R5G6B5_UNORM_PACK16=4,
       VK_FORMAT_B5G6R5_UNORM_PACK16=5,
       VK_FORMAT_R5G5B5A1_UNORM_PACK16=6,
       VK_FORMAT_B5G5R5A1_UNORM_PACK16=7,
       VK_FORMAT_A1R5G5B5_UNORM_PACK16=8,
       VK_FORMAT_R8_UNORM=9,
       VK_FORMAT_R8_SNORM=10,
       VK_FORMAT_R8_USCALED=11,
       VK_FORMAT_R8_SSCALED=12,
       VK_FORMAT_R8_UINT=13,
       VK_FORMAT_R8_SINT=14,
       VK_FORMAT_R8_SRGB=15,
       VK_FORMAT_R8G8_UNORM=16,
       VK_FORMAT_R8G8_SNORM=17,
       VK_FORMAT_R8G8_USCALED=18,
       VK_FORMAT_R8G8_SSCALED=19,
       VK_FORMAT_R8G8_UINT=20,
       VK_FORMAT_R8G8_SINT=21,
       VK_FORMAT_R8G8_SRGB=22,
       VK_FORMAT_R8G8B8_UNORM=23,
       VK_FORMAT_R8G8B8_SNORM=24,
       VK_FORMAT_R8G8B8_USCALED=25,
       VK_FORMAT_R8G8B8_SSCALED=26,
       VK_FORMAT_R8G8B8_UINT=27,
       VK_FORMAT_R8G8B8_SINT=28,
       VK_FORMAT_R8G8B8_SRGB=29,
       VK_FORMAT_B8G8R8_UNORM=30,
       VK_FORMAT_B8G8R8_SNORM=31,
       VK_FORMAT_B8G8R8_USCALED=32,
       VK_FORMAT_B8G8R8_SSCALED=33,
       VK_FORMAT_B8G8R8_UINT=34,
       VK_FORMAT_B8G8R8_SINT=35,
       VK_FORMAT_B8G8R8_SRGB=36,
       VK_FORMAT_R8G8B8A8_UNORM=37,
       VK_FORMAT_R8G8B8A8_SNORM=38,
       VK_FORMAT_R8G8B8A8_USCALED=39,
       VK_FORMAT_R8G8B8A8_SSCALED=40,
       VK_FORMAT_R8G8B8A8_UINT=41,
       VK_FORMAT_R8G8B8A8_SINT=42,
       VK_FORMAT_R8G8B8A8_SRGB=43,
       VK_FORMAT_B8G8R8A8_UNORM=44,
       VK_FORMAT_B8G8R8A8_SNORM=45,
       VK_FORMAT_B8G8R8A8_USCALED=46,
       VK_FORMAT_B8G8R8A8_SSCALED=47,
       VK_FORMAT_B8G8R8A8_UINT=48,
       VK_FORMAT_B8G8R8A8_SINT=49,
       VK_FORMAT_B8G8R8A8_SRGB=50,
       VK_FORMAT_A8B8G8R8_UNORM_PACK32=51,
       VK_FORMAT_A8B8G8R8_SNORM_PACK32=52,
       VK_FORMAT_A8B8G8R8_USCALED_PACK32=53,
       VK_FORMAT_A8B8G8R8_SSCALED_PACK32=54,
       VK_FORMAT_A8B8G8R8_UINT_PACK32=55,
       VK_FORMAT_A8B8G8R8_SINT_PACK32=56,
       VK_FORMAT_A8B8G8R8_SRGB_PACK32=57,
       VK_FORMAT_A2R10G10B10_UNORM_PACK32=58,
       VK_FORMAT_A2R10G10B10_SNORM_PACK32=59,
       VK_FORMAT_A2R10G10B10_USCALED_PACK32=60,
       VK_FORMAT_A2R10G10B10_SSCALED_PACK32=61,
       VK_FORMAT_A2R10G10B10_UINT_PACK32=62,
       VK_FORMAT_A2R10G10B10_SINT_PACK32=63,
       VK_FORMAT_A2B10G10R10_UNORM_PACK32=64,
       VK_FORMAT_A2B10G10R10_SNORM_PACK32=65,
       VK_FORMAT_A2B10G10R10_USCALED_PACK32=66,
       VK_FORMAT_A2B10G10R10_SSCALED_PACK32=67,
       VK_FORMAT_A2B10G10R10_UINT_PACK32=68,
       VK_FORMAT_A2B10G10R10_SINT_PACK32=69,
       VK_FORMAT_R16_UNORM=70,
       VK_FORMAT_R16_SNORM=71,
       VK_FORMAT_R16_USCALED=72,
       VK_FORMAT_R16_SSCALED=73,
       VK_FORMAT_R16_UINT=74,
       VK_FORMAT_R16_SINT=75,
       VK_FORMAT_R16_SFLOAT=76,
       VK_FORMAT_R16G16_UNORM=77,
       VK_FORMAT_R16G16_SNORM=78,
       VK_FORMAT_R16G16_USCALED=79,
       VK_FORMAT_R16G16_SSCALED=80,
       VK_FORMAT_R16G16_UINT=81,
       VK_FORMAT_R16G16_SINT=82,
       VK_FORMAT_R16G16_SFLOAT=83,
       VK_FORMAT_R16G16B16_UNORM=84,
       VK_FORMAT_R16G16B16_SNORM=85,
       VK_FORMAT_R16G16B16_USCALED=86,
       VK_FORMAT_R16G16B16_SSCALED=87,
       VK_FORMAT_R16G16B16_UINT=88,
       VK_FORMAT_R16G16B16_SINT=89,
       VK_FORMAT_R16G16B16_SFLOAT=90,
       VK_FORMAT_R16G16B16A16_UNORM=91,
       VK_FORMAT_R16G16B16A16_SNORM=92,
       VK_FORMAT_R16G16B16A16_USCALED=93,
       VK_FORMAT_R16G16B16A16_SSCALED=94,
       VK_FORMAT_R16G16B16A16_UINT=95,
       VK_FORMAT_R16G16B16A16_SINT=96,
       VK_FORMAT_R16G16B16A16_SFLOAT=97,
       VK_FORMAT_R32_UINT=98,
       VK_FORMAT_R32_SINT=99,
       VK_FORMAT_R32_SFLOAT=100,
       VK_FORMAT_R32G32_UINT=101,
       VK_FORMAT_R32G32_SINT=102,
       VK_FORMAT_R32G32_SFLOAT=103,
       VK_FORMAT_R32G32B32_UINT=104,
       VK_FORMAT_R32G32B32_SINT=105,
       VK_FORMAT_R32G32B32_SFLOAT=106,
       VK_FORMAT_R32G32B32A32_UINT=107,
       VK_FORMAT_R32G32B32A32_SINT=108,
       VK_FORMAT_R32G32B32A32_SFLOAT=109,
       VK_FORMAT_R64_UINT=110,
       VK_FORMAT_R64_SINT=111,
       VK_FORMAT_R64_SFLOAT=112,
       VK_FORMAT_R64G64_UINT=113,
       VK_FORMAT_R64G64_SINT=114,
       VK_FORMAT_R64G64_SFLOAT=115,
       VK_FORMAT_R64G64B64_UINT=116,
       VK_FORMAT_R64G64B64_SINT=117,
       VK_FORMAT_R64G64B64_SFLOAT=118,
       VK_FORMAT_R64G64B64A64_UINT=119,
       VK_FORMAT_R64G64B64A64_SINT=120,
       VK_FORMAT_R64G64B64A64_SFLOAT=121,
       VK_FORMAT_B10G11R11_UFLOAT_PACK32=122,
       VK_FORMAT_E5B9G9R9_UFLOAT_PACK32=123,
       VK_FORMAT_D16_UNORM=124,
       VK_FORMAT_X8_D24_UNORM_PACK32=125,
       VK_FORMAT_D32_SFLOAT=126,
       VK_FORMAT_S8_UINT=127,
       VK_FORMAT_D16_UNORM_S8_UINT=128,
       VK_FORMAT_D24_UNORM_S8_UINT=129,
       VK_FORMAT_D32_SFLOAT_S8_UINT=130,
       VK_FORMAT_BC1_RGB_UNORM_BLOCK=131,
       VK_FORMAT_BC1_RGB_SRGB_BLOCK=132,
       VK_FORMAT_BC1_RGBA_UNORM_BLOCK=133,
       VK_FORMAT_BC1_RGBA_SRGB_BLOCK=134,
       VK_FORMAT_BC2_UNORM_BLOCK=135,
       VK_FORMAT_BC2_SRGB_BLOCK=136,
       VK_FORMAT_BC3_UNORM_BLOCK=137,
       VK_FORMAT_BC3_SRGB_BLOCK=138,
       VK_FORMAT_BC4_UNORM_BLOCK=139,
       VK_FORMAT_BC4_SNORM_BLOCK=140,
       VK_FORMAT_BC5_UNORM_BLOCK=141,
       VK_FORMAT_BC5_SNORM_BLOCK=142,
       VK_FORMAT_BC6H_UFLOAT_BLOCK=143,
       VK_FORMAT_BC6H_SFLOAT_BLOCK=144,
       VK_FORMAT_BC7_UNORM_BLOCK=145,
       VK_FORMAT_BC7_SRGB_BLOCK=146,
       VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK=147,
       VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK=148,
       VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK=149,
       VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK=150,
       VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK=151,
       VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK=152,
       VK_FORMAT_EAC_R11_UNORM_BLOCK=153,
       VK_FORMAT_EAC_R11_SNORM_BLOCK=154,
       VK_FORMAT_EAC_R11G11_UNORM_BLOCK=155,
       VK_FORMAT_EAC_R11G11_SNORM_BLOCK=156,
       VK_FORMAT_ASTC_4x4_UNORM_BLOCK=157,
       VK_FORMAT_ASTC_4x4_SRGB_BLOCK=158,
       VK_FORMAT_ASTC_5x4_UNORM_BLOCK=159,
       VK_FORMAT_ASTC_5x4_SRGB_BLOCK=160,
       VK_FORMAT_ASTC_5x5_UNORM_BLOCK=161,
       VK_FORMAT_ASTC_5x5_SRGB_BLOCK=162,
       VK_FORMAT_ASTC_6x5_UNORM_BLOCK=163,
       VK_FORMAT_ASTC_6x5_SRGB_BLOCK=164,
       VK_FORMAT_ASTC_6x6_UNORM_BLOCK=165,
       VK_FORMAT_ASTC_6x6_SRGB_BLOCK=166,
       VK_FORMAT_ASTC_8x5_UNORM_BLOCK=167,
       VK_FORMAT_ASTC_8x5_SRGB_BLOCK=168,
       VK_FORMAT_ASTC_8x6_UNORM_BLOCK=169,
       VK_FORMAT_ASTC_8x6_SRGB_BLOCK=170,
       VK_FORMAT_ASTC_8x8_UNORM_BLOCK=171,
       VK_FORMAT_ASTC_8x8_SRGB_BLOCK=172,
       VK_FORMAT_ASTC_10x5_UNORM_BLOCK=173,
       VK_FORMAT_ASTC_10x5_SRGB_BLOCK=174,
       VK_FORMAT_ASTC_10x6_UNORM_BLOCK=175,
       VK_FORMAT_ASTC_10x6_SRGB_BLOCK=176,
       VK_FORMAT_ASTC_10x8_UNORM_BLOCK=177,
       VK_FORMAT_ASTC_10x8_SRGB_BLOCK=178,
       VK_FORMAT_ASTC_10x10_UNORM_BLOCK=179,
       VK_FORMAT_ASTC_10x10_SRGB_BLOCK=180,
       VK_FORMAT_ASTC_12x10_UNORM_BLOCK=181,
       VK_FORMAT_ASTC_12x10_SRGB_BLOCK=182,
       VK_FORMAT_ASTC_12x12_UNORM_BLOCK=183,
       VK_FORMAT_ASTC_12x12_SRGB_BLOCK=184,
       VK_FORMAT_END_RANGE=184,                                                  // VK_FORMAT_ASTC_12x12_SRGB_BLOCK
       VK_FORMAT_RANGE_SIZE=185,                                                 // (VK_FORMAT_ASTC_12x12_SRGB_BLOCK-VK_FORMAT_UNDEFINED)+1
       VK_FORMAT_MAX_ENUM=$7fffffff
      );

     PPVkStructureType=^PVkStructureType;
     PVkStructureType=^TVkStructureType;
     TVkStructureType=
      (
       VK_STRUCTURE_TYPE_APPLICATION_INFO=0,
       VK_STRUCTURE_TYPE_BEGIN_RANGE=0,                                          // VK_STRUCTURE_TYPE_APPLICATION_INFO
       VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO=1,
       VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO=2,
       VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO=3,
       VK_STRUCTURE_TYPE_SUBMIT_INFO=4,
       VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO=5,
       VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE=6,
       VK_STRUCTURE_TYPE_BIND_SPARSE_INFO=7,
       VK_STRUCTURE_TYPE_FENCE_CREATE_INFO=8,
       VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO=9,
       VK_STRUCTURE_TYPE_EVENT_CREATE_INFO=10,
       VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO=11,
       VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO=12,
       VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO=13,
       VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO=14,
       VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO=15,
       VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO=16,
       VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO=17,
       VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO=18,
       VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO=19,
       VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO=20,
       VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO=21,
       VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO=22,
       VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO=23,
       VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO=24,
       VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO=25,
       VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO=26,
       VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO=27,
       VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO=28,
       VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO=29,
       VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO=30,
       VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO=31,
       VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO=32,
       VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO=33,
       VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO=34,
       VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET=35,
       VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET=36,
       VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO=37,
       VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO=38,
       VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO=39,
       VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO=40,
       VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO=41,
       VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO=42,
       VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO=43,
       VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER=44,
       VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER=45,
       VK_STRUCTURE_TYPE_MEMORY_BARRIER=46,
       VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO=47,
       VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO=48,
       VK_STRUCTURE_TYPE_END_RANGE=48,                                           // VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO
       VK_STRUCTURE_TYPE_RANGE_SIZE=49,                                          // (VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO-VK_STRUCTURE_TYPE_APPLICATION_INFO)+1
       VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR=1000001000,
       VK_STRUCTURE_TYPE_PRESENT_INFO_KHR=1000001001,
       VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR=1000002000,
       VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR=1000002001,
       VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR=1000003000,
       VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR=1000004000,
       VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR=1000005000,
       VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR=1000006000,
       VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR=1000007000,
       VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR=1000008000,
       VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR=1000009000,
       VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT=1000011000,
       VK_STRUCTURE_TYPE_MAX_ENUM=$7fffffff
      );

     PPVkSubpassContents=^PVkSubpassContents;
     PVkSubpassContents=^TVkSubpassContents;
     TVkSubpassContents=
      (
       VK_SUBPASS_CONTENTS_INLINE=0,
       VK_SUBPASS_CONTENTS_BEGIN_RANGE=0,                                        // VK_SUBPASS_CONTENTS_INLINE
       VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS=1,
       VK_SUBPASS_CONTENTS_END_RANGE=1,                                          // VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS
       VK_SUBPASS_CONTENTS_RANGE_SIZE=2,                                         // (VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS-VK_SUBPASS_CONTENTS_INLINE)+1
       VK_SUBPASS_CONTENTS_MAX_ENUM=$7fffffff
      );

     PPVkResult=^PVkResult;
     PVkResult=^TVkResult;
     TVkResult=
      (
       VK_NV_EXTENSION_1_ERROR=-1000013000,
       VK_NV_EXTENSION_0_ERROR=-1000012000,
       VK_ERROR_VALIDATION_FAILED_EXT=-1000011001,
       VK_ERROR_INCOMPATIBLE_DISPLAY_KHR=-1000003001,
       VK_ERROR_OUT_OF_DATE_KHR=-1000001004,
       VK_ERROR_NATIVE_WINDOW_IN_USE_KHR=-1000000001,
       VK_ERROR_SURFACE_LOST_KHR=-1000000000,
       VK_RESULT_UNUSED_START=-12,
       VK_RESULT_BEGIN_RANGE=-12,                                                // VK_RESULT_UNUSED_START
       VK_ERROR_FORMAT_NOT_SUPPORTED=-11,                                        // Requested format is not supported on this device
       VK_ERROR_TOO_MANY_OBJECTS=-10,                                            // Too many objects of the type have already been created
       VK_ERROR_INCOMPATIBLE_DRIVER=-9,                                          // Unable to find a Vulkan driver
       VK_ERROR_FEATURE_NOT_PRESENT=-8,                                          // Requested feature is not available on this device
       VK_ERROR_EXTENSION_NOT_PRESENT=-7,                                        // Extension specified does not exist
       VK_ERROR_LAYER_NOT_PRESENT=-6,                                            // Layer specified does not exist
       VK_ERROR_MEMORY_MAP_FAILED=-5,                                            // Mapping of a memory object has failed
       VK_ERROR_DEVICE_LOST=-4,                                                  // Initialization of a object has failed
       VK_ERROR_INITIALIZATION_FAILED=-3,                                        // The logical device has been lost. See <<devsandqueues-lost-device>>
       VK_ERROR_OUT_OF_DEVICE_MEMORY=-2,                                         // A device memory allocation has failed
       VK_ERROR_OUT_OF_HOST_MEMORY=-1,                                           // A host memory allocation has failed
       VK_SUCCESS=0,                                                             // Command completed successfully
       VK_NOT_READY=1,                                                           // A fence or query has not yet completed
       VK_TIMEOUT=2,                                                             // A wait operation has not completed in the specified time
       VK_EVENT_SET=3,                                                           // An event is signaled
       VK_EVENT_RESET=4,                                                         // An event is unsignalled
       VK_INCOMPLETE=5,                                                          // A return array was too small for the resul
       VK_RESULT_END_RANGE=5,                                                    // VK_INCOMPLETE
       VK_RESULT_RANGE_SIZE=18,                                                  // (VK_INCOMPLETE-VK_RESULT_UNUSED_START)+1
       VK_SUBOPTIMAL_KHR=1000001003,
       VK_RESULT_MAX_ENUM=$7fffffff
      );

     PPVkDynamicState=^PVkDynamicState;
     PVkDynamicState=^TVkDynamicState;
     TVkDynamicState=
      (
       VK_DYNAMIC_STATE_VIEWPORT=0,
       VK_DYNAMIC_STATE_BEGIN_RANGE=0,                                           // VK_DYNAMIC_STATE_VIEWPORT
       VK_DYNAMIC_STATE_SCISSOR=1,
       VK_DYNAMIC_STATE_LINE_WIDTH=2,
       VK_DYNAMIC_STATE_DEPTH_BIAS=3,
       VK_DYNAMIC_STATE_BLEND_CONSTANTS=4,
       VK_DYNAMIC_STATE_DEPTH_BOUNDS=5,
       VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK=6,
       VK_DYNAMIC_STATE_STENCIL_WRITE_MASK=7,
       VK_DYNAMIC_STATE_STENCIL_REFERENCE=8,
       VK_DYNAMIC_STATE_END_RANGE=8,                                             // VK_DYNAMIC_STATE_STENCIL_REFERENCE
       VK_DYNAMIC_STATE_RANGE_SIZE=9,                                            // (VK_DYNAMIC_STATE_STENCIL_REFERENCE-VK_DYNAMIC_STATE_VIEWPORT)+1
       VK_DYNAMIC_STATE_MAX_ENUM=$7fffffff
      );

     PPVkQueueFlagBits=^PVkQueueFlagBits;
     PVkQueueFlagBits=^TVkQueueFlagBits;
     TVkQueueFlagBits=
      (
       VK_QUEUE_GRAPHICS_BIT=$00000001,                                          // Queue supports graphics operations
       VK_QUEUE_COMPUTE_BIT=$00000002,                                           // Queue supports compute operations
       VK_QUEUE_TRANSFER_BIT=$00000004,                                          // Queue supports transfer operations
       VK_QUEUE_SPARSE_BINDING_BIT=$00000008                                     // Queue supports sparse resource memory management operations
      );

     PPVkMemoryPropertyFlagBits=^PVkMemoryPropertyFlagBits;
     PVkMemoryPropertyFlagBits=^TVkMemoryPropertyFlagBits;
     TVkMemoryPropertyFlagBits=
      (
       VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT=$00000001,                            // If otherwise stated, then allocate memory on device
       VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT=$00000002,                            // Memory is mappable by host
       VK_MEMORY_PROPERTY_HOST_COHERENT_BIT=$00000004,                           // Memory will have i/o coherency. If not set, application may need to use vkFlushMappedMemoryRanges and vkInvalidateMappedMemoryRanges to flush/invalidate host cache
       VK_MEMORY_PROPERTY_HOST_CACHED_BIT=$00000008,                             // Memory will be cached by the host
       VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT=$00000010                         // Memory may be allocated by the driver when it is required
      );

     PPVkMemoryHeapFlagBits=^PVkMemoryHeapFlagBits;
     PVkMemoryHeapFlagBits=^TVkMemoryHeapFlagBits;
     TVkMemoryHeapFlagBits=
      (
       VK_MEMORY_HEAP_DEVICE_LOCAL_BIT=$00000001                                 // If set, heap represents device memory
      );

     PPVkAccessFlagBits=^PVkAccessFlagBits;
     PVkAccessFlagBits=^TVkAccessFlagBits;
     TVkAccessFlagBits=
      (
       VK_ACCESS_INDIRECT_COMMAND_READ_BIT=$00000001,                            // Controls coherency of indirect command reads
       VK_ACCESS_INDEX_READ_BIT=$00000002,                                       // Controls coherency of index reads
       VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT=$00000004,                            // Controls coherency of vertex attribute reads
       VK_ACCESS_UNIFORM_READ_BIT=$00000008,                                     // Controls coherency of uniform buffer reads
       VK_ACCESS_INPUT_ATTACHMENT_READ_BIT=$00000010,                            // Controls coherency of input attachment reads
       VK_ACCESS_SHADER_READ_BIT=$00000020,                                      // Controls coherency of shader reads
       VK_ACCESS_SHADER_WRITE_BIT=$00000040,                                     // Controls coherency of shader writes
       VK_ACCESS_COLOR_ATTACHMENT_READ_BIT=$00000080,                            // Controls coherency of color attachment reads
       VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT=$00000100,                           // Controls coherency of color attachment writes
       VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT=$00000200,                    // Controls coherency of depth/stencil attachment reads
       VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT=$00000400,                   // Controls coherency of depth/stencil attachment writes
       VK_ACCESS_TRANSFER_READ_BIT=$00000800,                                    // Controls coherency of transfer reads
       VK_ACCESS_TRANSFER_WRITE_BIT=$00001000,                                   // Controls coherency of transfer writes
       VK_ACCESS_HOST_READ_BIT=$00002000,                                        // Controls coherency of host reads
       VK_ACCESS_HOST_WRITE_BIT=$00004000,                                       // Controls coherency of host writes
       VK_ACCESS_MEMORY_READ_BIT=$00008000,                                      // Controls coherency of memory reads
       VK_ACCESS_MEMORY_WRITE_BIT=$00010000                                      // Controls coherency of memory writes
      );

     PPVkBufferUsageFlagBits=^PVkBufferUsageFlagBits;
     PVkBufferUsageFlagBits=^TVkBufferUsageFlagBits;
     TVkBufferUsageFlagBits=
      (
       VK_BUFFER_USAGE_TRANSFER_SRC_BIT=$00000001,                               // Can be used as a source of transfer operations
       VK_BUFFER_USAGE_TRANSFER_DST_BIT=$00000002,                               // Can be used as a destination of transfer operations
       VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT=$00000004,                       // Can be used as TBO
       VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT=$00000008,                       // Can be used as IBO
       VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT=$00000010,                             // Can be used as UBO
       VK_BUFFER_USAGE_STORAGE_BUFFER_BIT=$00000020,                             // Can be used as SSBO
       VK_BUFFER_USAGE_INDEX_BUFFER_BIT=$00000040,                               // Can be used as source of fixed-function index fetch (index buffer)
       VK_BUFFER_USAGE_VERTEX_BUFFER_BIT=$00000080,                              // Can be used as source of fixed-function vertex fetch (VBO)
       VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT=$00000100                             // Can be the source of indirect parameters (e.g. indirect buffer, parameter buffer)
      );

     PPVkBufferCreateFlagBits=^PVkBufferCreateFlagBits;
     PVkBufferCreateFlagBits=^TVkBufferCreateFlagBits;
     TVkBufferCreateFlagBits=
      (
       VK_BUFFER_CREATE_SPARSE_BINDING_BIT=$00000001,                            // Buffer should support sparse backing
       VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT=$00000002,                          // Buffer should support sparse backing with partial residency
       VK_BUFFER_CREATE_SPARSE_ALIASED_BIT=$00000004                             // Buffer should support constent data access to physical memory blocks mapped into multiple locations of sparse buffers
      );

     PPVkShaderStageFlagBits=^PVkShaderStageFlagBits;
     PVkShaderStageFlagBits=^TVkShaderStageFlagBits;
     TVkShaderStageFlagBits=
      (
       VK_SHADER_STAGE_VERTEX_BIT=$00000001,
       VK_SHADER_STAGE_ALL_GRAPHICS=$00000001,
       VK_SHADER_STAGE_ALL=$00000001,
       VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT=$00000002,
       VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT=$00000004,
       VK_SHADER_STAGE_GEOMETRY_BIT=$00000008,
       VK_SHADER_STAGE_FRAGMENT_BIT=$00000010,
       VK_SHADER_STAGE_COMPUTE_BIT=$00000020
      );

     PPVkImageUsageFlagBits=^PVkImageUsageFlagBits;
     PVkImageUsageFlagBits=^TVkImageUsageFlagBits;
     TVkImageUsageFlagBits=
      (
       VK_IMAGE_USAGE_TRANSFER_SRC_BIT=$00000001,                                // Can be used as a source of transfer operations
       VK_IMAGE_USAGE_TRANSFER_DST_BIT=$00000002,                                // Can be used as a destination of transfer operations
       VK_IMAGE_USAGE_SAMPLED_BIT=$00000004,                                     // Can be sampled from (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
       VK_IMAGE_USAGE_STORAGE_BIT=$00000008,                                     // Can be used as storage image (STORAGE_IMAGE descriptor type)
       VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT=$00000010,                            // Can be used as framebuffer color attachment
       VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT=$00000020,                    // Can be used as framebuffer depth/stencil attachment
       VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT=$00000040,                        // Image data not needed outside of rendering
       VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT=$00000080                             // Can be used as framebuffer input attachment
      );

     PPVkImageCreateFlagBits=^PVkImageCreateFlagBits;
     PVkImageCreateFlagBits=^TVkImageCreateFlagBits;
     TVkImageCreateFlagBits=
      (
       VK_IMAGE_CREATE_SPARSE_BINDING_BIT=$00000001,                             // Image should support sparse backing
       VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT=$00000002,                           // Image should support sparse backing with partial residency
       VK_IMAGE_CREATE_SPARSE_ALIASED_BIT=$00000004,                             // Image should support constent data access to physical memory blocks mapped into multiple locations of sparse images
       VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT=$00000008,                             // Allows image views to have different format than the base image
       VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT=$00000010                             // Allows creating image views with cube type from the created image
      );

     PPVkPipelineCreateFlagBits=^PVkPipelineCreateFlagBits;
     PVkPipelineCreateFlagBits=^TVkPipelineCreateFlagBits;
     TVkPipelineCreateFlagBits=
      (
       VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT=$00000001,
       VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT=$00000002,
       VK_PIPELINE_CREATE_DERIVATIVE_BIT=$00000004
      );

     PPVkColorComponentFlagBits=^PVkColorComponentFlagBits;
     PVkColorComponentFlagBits=^TVkColorComponentFlagBits;
     TVkColorComponentFlagBits=
      (
       VK_COLOR_COMPONENT_R_BIT=$00000001,
       VK_COLOR_COMPONENT_G_BIT=$00000002,
       VK_COLOR_COMPONENT_B_BIT=$00000004,
       VK_COLOR_COMPONENT_A_BIT=$00000008
      );

     PPVkFenceCreateFlagBits=^PVkFenceCreateFlagBits;
     PVkFenceCreateFlagBits=^TVkFenceCreateFlagBits;
     TVkFenceCreateFlagBits=
      (
       VK_FENCE_CREATE_SIGNALED_BIT=$00000001
      );

     PPVkFormatFeatureFlagBits=^PVkFormatFeatureFlagBits;
     PVkFormatFeatureFlagBits=^TVkFormatFeatureFlagBits;
     TVkFormatFeatureFlagBits=
      (
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT=$00000001,                            // Format can be used for sampled images (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
       VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT=$00000002,                            // Format can be used for storage images (STORAGE_IMAGE descriptor type)
       VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT=$00000004,                     // Format supports atomic operations in case it's used for storage images
       VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT=$00000008,                     // Format can be used for uniform texel buffers (TBOs)
       VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT=$00000010,                     // Format can be used for storage texel buffers (IBOs)
       VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT=$00000020,              // Format supports atomic operations in case it's used for storage texel buffers
       VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT=$00000040,                            // Format can be used for vertex buffers (VBOs)
       VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT=$00000080,                         // Format can be used for color attachment images
       VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT=$00000100,                   // Format supports blending in case it's used for color attachment images
       VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT=$00000200,                 // Format can be used for depth/stencil attachment images
       VK_FORMAT_FEATURE_BLIT_SRC_BIT=$00000400,                                 // Format can be used as the source image of blits with vkCmdBlitImage
       VK_FORMAT_FEATURE_BLIT_DST_BIT=$00000800,                                 // Format can be used as the destination image of blits with vkCmdBlitImage
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT=$00001000               // Format can be filtered with VK_FILTER_LINEAR when being sampled
      );

     PPVkQueryControlFlagBits=^PVkQueryControlFlagBits;
     PVkQueryControlFlagBits=^TVkQueryControlFlagBits;
     TVkQueryControlFlagBits=
      (
       VK_QUERY_CONTROL_PRECISE_BIT=$00000001                                    // Require precise results to be collected by the query
      );

     PPVkQueryResultFlagBits=^PVkQueryResultFlagBits;
     PVkQueryResultFlagBits=^TVkQueryResultFlagBits;
     TVkQueryResultFlagBits=
      (
       VK_QUERY_RESULT_64_BIT=$00000001,                                         // Results of the queries are written to the destination buffer as 64-bit values
       VK_QUERY_RESULT_WAIT_BIT=$00000002,                                       // Results of the queries are waited on before proceeding with the result copy
       VK_QUERY_RESULT_WITH_AVAILABILITY_BIT=$00000004,                          // Besides the results of the query, the availability of the results is also written
       VK_QUERY_RESULT_PARTIAL_BIT=$00000008                                     // Copy the partial results of the query even if the final results aren't available
      );

     PPVkCommandBufferUsageFlagBits=^PVkCommandBufferUsageFlagBits;
     PVkCommandBufferUsageFlagBits=^TVkCommandBufferUsageFlagBits;
     TVkCommandBufferUsageFlagBits=
      (
       VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT=$00000001,
       VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT=$00000002,
       VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT=$00000004                    // Command buffer may be submitted/executed more than once simultaneously
      );

     PPVkQueryPipelineStatisticFlagBits=^PVkQueryPipelineStatisticFlagBits;
     PVkQueryPipelineStatisticFlagBits=^TVkQueryPipelineStatisticFlagBits;
     TVkQueryPipelineStatisticFlagBits=
      (
       VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT=$00000001,        // Optional
       VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT=$00000002,      // Optional
       VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT=$00000004,      // Optional
       VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT=$00000008,    // Optional
       VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT=$00000010,     // Optional
       VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT=$00000020,           // Optional
       VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT=$00000040,            // Optional
       VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT=$00000080,    // Optional
       VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT=$00000100, // Optional
       VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT=$00000200, // Optional
       VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT=$00000400      // Optional
      );

     PPVkImageAspectFlagBits=^PVkImageAspectFlagBits;
     PVkImageAspectFlagBits=^TVkImageAspectFlagBits;
     TVkImageAspectFlagBits=
      (
       VK_IMAGE_ASPECT_COLOR_BIT=$00000001,
       VK_IMAGE_ASPECT_DEPTH_BIT=$00000002,
       VK_IMAGE_ASPECT_STENCIL_BIT=$00000004,
       VK_IMAGE_ASPECT_METADATA_BIT=$00000008
      );

     PPVkSparseImageFormatFlagBits=^PVkSparseImageFormatFlagBits;
     PVkSparseImageFormatFlagBits=^TVkSparseImageFormatFlagBits;
     TVkSparseImageFormatFlagBits=
      (
       VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT=$00000001,                      // Image uses a single miptail region for all array layers
       VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT=$00000002,                    // Image requires mip levels to be an exact multiple of the sparse image block size for non-miptail levels.
       VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT=$00000004               // Image uses a non-standard sparse block size
      );

     PPVkSparseMemoryBindFlagBits=^PVkSparseMemoryBindFlagBits;
     PVkSparseMemoryBindFlagBits=^TVkSparseMemoryBindFlagBits;
     TVkSparseMemoryBindFlagBits=
      (
       VK_SPARSE_MEMORY_BIND_METADATA_BIT=$00000001                              // Operation binds resource metadata to memory
      );

     PPVkPipelineStageFlagBits=^PVkPipelineStageFlagBits;
     PVkPipelineStageFlagBits=^TVkPipelineStageFlagBits;
     TVkPipelineStageFlagBits=
      (
       VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT=$00000001,                              // Before subsequent commands are processed
       VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT=$00000002,                            // Draw/DispatchIndirect command fetch
       VK_PIPELINE_STAGE_VERTEX_INPUT_BIT=$00000004,                             // Vertex/index fetch
       VK_PIPELINE_STAGE_VERTEX_SHADER_BIT=$00000008,                            // Vertex shading
       VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT=$00000010,              // Tessellation control shading
       VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT=$00000020,           // Tessellation evaluation shading
       VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT=$00000040,                          // Geometry shading
       VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT=$00000080,                          // Fragment shading
       VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT=$00000100,                     // Early fragment (depth and stencil) tests
       VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT=$00000200,                      // Late fragment (depth and stencil) tests
       VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT=$00000400,                  // Color attachment writes
       VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT=$00000800,                           // Compute shading
       VK_PIPELINE_STAGE_TRANSFER_BIT=$00001000,                                 // Transfer/copy operations
       VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT=$00002000,                           // After previous commands have completed
       VK_PIPELINE_STAGE_HOST_BIT=$00004000,                                     // Indicates host (CPU) is a source/sink of the dependency
       VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT=$00008000,                             // All stages of the graphics pipeline
       VK_PIPELINE_STAGE_ALL_COMMANDS_BIT=$00010000                              // All stages supported on the queue
      );

     PPVkCommandPoolCreateFlagBits=^PVkCommandPoolCreateFlagBits;
     PVkCommandPoolCreateFlagBits=^TVkCommandPoolCreateFlagBits;
     TVkCommandPoolCreateFlagBits=
      (
       VK_COMMAND_POOL_CREATE_TRANSIENT_BIT=$00000001,                           // Command buffers have a short lifetime
       VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT=$00000002                 // Command buffers may release their memory individually
      );

     PPVkCommandPoolResetFlagBits=^PVkCommandPoolResetFlagBits;
     PVkCommandPoolResetFlagBits=^TVkCommandPoolResetFlagBits;
     TVkCommandPoolResetFlagBits=
      (
       VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT=$00000001                     // Release resources owned by the pool
      );

     PPVkCommandBufferResetFlagBits=^PVkCommandBufferResetFlagBits;
     PVkCommandBufferResetFlagBits=^TVkCommandBufferResetFlagBits;
     TVkCommandBufferResetFlagBits=
      (
       VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT=$00000001                   // Release resources owned by the buffer
      );

     PPVkSampleCountFlagBits=^PVkSampleCountFlagBits;
     PVkSampleCountFlagBits=^TVkSampleCountFlagBits;
     TVkSampleCountFlagBits=
      (
       VK_SAMPLE_COUNT_1_BIT=$00000001,                                          // Sample count 1 supported
       VK_SAMPLE_COUNT_2_BIT=$00000002,                                          // Sample count 2 supported
       VK_SAMPLE_COUNT_4_BIT=$00000004,                                          // Sample count 4 supported
       VK_SAMPLE_COUNT_8_BIT=$00000008,                                          // Sample count 8 supported
       VK_SAMPLE_COUNT_16_BIT=$00000010,                                         // Sample count 16 supported
       VK_SAMPLE_COUNT_32_BIT=$00000020,                                         // Sample count 32 supported
       VK_SAMPLE_COUNT_64_BIT=$00000040                                          // Sample count 64 supported
      );

     PPVkAttachmentDescriptionFlagBits=^PVkAttachmentDescriptionFlagBits;
     PVkAttachmentDescriptionFlagBits=^TVkAttachmentDescriptionFlagBits;
     TVkAttachmentDescriptionFlagBits=
      (
       VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT=$00000001                         // The attachment may alias physical memory of another attachment in the same render pass
      );

     PPVkStencilFaceFlagBits=^PVkStencilFaceFlagBits;
     PVkStencilFaceFlagBits=^TVkStencilFaceFlagBits;
     TVkStencilFaceFlagBits=
      (
       VK_STENCIL_FACE_FRONT_BIT=$00000001,                                      // Front face
       VK_STENCIL_FRONT_AND_BACK=$00000001,                                      // Front and back faces
       VK_STENCIL_FACE_BACK_BIT=$00000002                                        // Back face
      );

     PPVkDescriptorPoolCreateFlagBits=^PVkDescriptorPoolCreateFlagBits;
     PVkDescriptorPoolCreateFlagBits=^TVkDescriptorPoolCreateFlagBits;
     TVkDescriptorPoolCreateFlagBits=
      (
       VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT=$00000001               // Descriptor sets may be freed individually
      );

     PPVkDependencyFlagBits=^PVkDependencyFlagBits;
     PVkDependencyFlagBits=^TVkDependencyFlagBits;
     TVkDependencyFlagBits=
      (
       VK_DEPENDENCY_BY_REGION_BIT=$00000001                                     // Dependency is per pixel region 
      );

     PPVkPresentModeKHR=^PVkPresentModeKHR;
     PVkPresentModeKHR=^TVkPresentModeKHR;
     TVkPresentModeKHR=
      (
       VK_PRESENT_MODE_IMMEDIATE_KHR=0,
       VK_PRESENT_MODE_BEGIN_RANGE=0,                                            // VK_PRESENT_MODE_IMMEDIATE_KHR
       VK_PRESENT_MODE_MAILBOX_KHR=1,
       VK_PRESENT_MODE_FIFO_KHR=2,
       VK_PRESENT_MODE_FIFO_RELAXED_KHR=3,
       VK_PRESENT_MODE_END_RANGE=3,                                              // VK_PRESENT_MODE_FIFO_RELAXED_KHR
       VK_PRESENT_MODE_RANGE_SIZE=4,                                             // (VK_PRESENT_MODE_FIFO_RELAXED_KHR-VK_PRESENT_MODE_IMMEDIATE_KHR)+1
       VK_PRESENT_MODE_MAX_ENUM=$7fffffff
      );

     PPVkColorSpaceKHR=^PVkColorSpaceKHR;
     PVkColorSpaceKHR=^TVkColorSpaceKHR;
     TVkColorSpaceKHR=
      (
       VK_COLORSPACE_SRGB_NONLINEAR_KHR=0,
       VK_COLORSPACE_BEGIN_RANGE=0,                                              // VK_COLORSPACE_SRGB_NONLINEAR_KHR
       VK_COLORSPACE_END_RANGE=0,                                                // VK_COLORSPACE_SRGB_NONLINEAR_KHR
       VK_COLORSPACE_RANGE_SIZE=1,                                               // (VK_COLORSPACE_SRGB_NONLINEAR_KHR-VK_COLORSPACE_SRGB_NONLINEAR_KHR)+1
       VK_COLORSPACE_MAX_ENUM=$7fffffff
      );

     PPVkDisplayPlaneAlphaFlagBitsKHR=^PVkDisplayPlaneAlphaFlagBitsKHR;
     PVkDisplayPlaneAlphaFlagBitsKHR=^TVkDisplayPlaneAlphaFlagBitsKHR;
     TVkDisplayPlaneAlphaFlagBitsKHR=
      (
       VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR=$00000001,
       VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR=$00000002,
       VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR=$00000004,
       VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR=$00000008
      );

     PPVkCompositeAlphaFlagBitsKHR=^PVkCompositeAlphaFlagBitsKHR;
     PVkCompositeAlphaFlagBitsKHR=^TVkCompositeAlphaFlagBitsKHR;
     TVkCompositeAlphaFlagBitsKHR=
      (
       VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR=$00000001,
       VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR=$00000002,
       VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR=$00000004,
       VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR=$00000008
      );

     PPVkSurfaceTransformFlagBitsKHR=^PVkSurfaceTransformFlagBitsKHR;
     PVkSurfaceTransformFlagBitsKHR=^TVkSurfaceTransformFlagBitsKHR;
     TVkSurfaceTransformFlagBitsKHR=
      (
       VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR=$00000001,
       VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR=$00000002,
       VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR=$00000004,
       VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR=$00000008,
       VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR=$00000010,
       VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR=$00000020,
       VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR=$00000040,
       VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR=$00000080,
       VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR=$00000100
      );

     PPVkDebugReportFlagBitsEXT=^PVkDebugReportFlagBitsEXT;
     PVkDebugReportFlagBitsEXT=^TVkDebugReportFlagBitsEXT;
     TVkDebugReportFlagBitsEXT=
      (
       VK_DEBUG_REPORT_INFORMATION_BIT_EXT=$00000001,
       VK_DEBUG_REPORT_WARNING_BIT_EXT=$00000002,
       VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT=$00000004,
       VK_DEBUG_REPORT_ERROR_BIT_EXT=$00000008,
       VK_DEBUG_REPORT_DEBUG_BIT_EXT=$00000010
      );

     PPVkDebugReportObjectTypeEXT=^PVkDebugReportObjectTypeEXT;
     PVkDebugReportObjectTypeEXT=^TVkDebugReportObjectTypeEXT;
     TVkDebugReportObjectTypeEXT=
      (
       VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT=0,
       VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT=1,
       VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT=2,
       VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT=3,
       VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT=4,
       VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT=5,
       VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT=6,
       VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT=7,
       VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT=8,
       VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT=9,
       VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT=10,
       VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT=11,
       VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT=12,
       VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT=13,
       VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT=14,
       VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT=15,
       VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT=16,
       VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT=17,
       VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT=18,
       VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT=19,
       VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT=20,
       VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT=21,
       VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT=22,
       VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT=23,
       VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT=24,
       VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT=25,
       VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT=26,
       VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT=27,
       VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT=28
      );

     PPVkDebugReportErrorEXT=^PVkDebugReportErrorEXT;
     PVkDebugReportErrorEXT=^TVkDebugReportErrorEXT;
     TVkDebugReportErrorEXT=
      (
       VK_DEBUG_REPORT_ERROR_NONE_EXT=0,
       VK_DEBUG_REPORT_ERROR_CALLBACK_REF_EXT=1
      );

     PPPFN_vkInternalAllocationNotification=^PPFN_vkInternalAllocationNotification;
     PPFN_vkInternalAllocationNotification=^TPFN_vkInternalAllocationNotification;
     TPFN_vkInternalAllocationNotification=procedure(pUserData:TVkPointer;size:TVkPtrInt;allocationType:TVkInternalAllocationType;allocationScope:TVkSystemAllocationScope); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}

     PPPFN_vkInternalFreeNotification=^PPFN_vkInternalFreeNotification;
     PPFN_vkInternalFreeNotification=^TPFN_vkInternalFreeNotification;
     TPFN_vkInternalFreeNotification=procedure(pUserData:TVkPointer;size:TVkPtrInt;allocationType:TVkInternalAllocationType;allocationScope:TVkSystemAllocationScope); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}

     PPPFN_vkReallocationFunction=^PPFN_vkReallocationFunction;
     PPFN_vkReallocationFunction=^TPFN_vkReallocationFunction;
     TPFN_vkReallocationFunction=function(pUserData:TVkPointer;pOriginal:TVkPointer;size:TVkPtrInt;alignment:TVkPtrInt;allocationScope:TVkSystemAllocationScope):TVkPointer; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}

     PPPFN_vkAllocationFunction=^PPFN_vkAllocationFunction;
     PPFN_vkAllocationFunction=^TPFN_vkAllocationFunction;
     TPFN_vkAllocationFunction=function(pUserData:TVkPointer;size:TVkPtrInt;alignment:TVkPtrInt;allocationScope:TVkSystemAllocationScope):TVkPointer; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}

     PPPFN_vkFreeFunction=^PPFN_vkFreeFunction;
     PPFN_vkFreeFunction=^TPFN_vkFreeFunction;
     TPFN_vkFreeFunction=procedure(pUserData:TVkPointer;pMemory:TVkPointer); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}

     PPPFN_vkVoidFunction=^PPFN_vkVoidFunction;
     PPFN_vkVoidFunction=^TPFN_vkVoidFunction;
     TPFN_vkVoidFunction=procedure(); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}

     PPPFN_vkDebugReportCallbackEXT=^PPFN_vkDebugReportCallbackEXT;
     PPFN_vkDebugReportCallbackEXT=^TPFN_vkDebugReportCallbackEXT;
     TPFN_vkDebugReportCallbackEXT=function(flags:TVkDebugReportFlagsEXT;objectType:TVkDebugReportObjectTypeEXT;object_:TVkUInt64;location:TVkPtrInt;messageCode:TVkInt32;const pLayerPrefix:PVkChar;const pMessage:PVkChar;pUserData:TVkPointer):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}

     PPVkOffset2D=^PVkOffset2D;
     PVkOffset2D=^TVkOffset2D;
     TVkOffset2D=record
      x:TVkInt32;
      y:TVkInt32;
     end;

     PPVkOffset3D=^PVkOffset3D;
     PVkOffset3D=^TVkOffset3D;
     TVkOffset3D=record
      x:TVkInt32;
      y:TVkInt32;
      z:TVkInt32;
     end;

     PPVkExtent2D=^PVkExtent2D;
     PVkExtent2D=^TVkExtent2D;
     TVkExtent2D=record
      width:TVkUInt32;
      height:TVkUInt32;
     end;

     PPVkExtent3D=^PVkExtent3D;
     PVkExtent3D=^TVkExtent3D;
     TVkExtent3D=record
      width:TVkUInt32;
      height:TVkUInt32;
      depth:TVkUInt32;
     end;

     PPVkViewport=^PVkViewport;
     PVkViewport=^TVkViewport;
     TVkViewport=record
      x:TVkFloat;
      y:TVkFloat;
      width:TVkFloat;
      height:TVkFloat;
      minDepth:TVkFloat;
      maxDepth:TVkFloat;
     end;

     PPVkRect2D=^PVkRect2D;
     PVkRect2D=^TVkRect2D;
     TVkRect2D=record
      offset:TVkOffset2D;
      extent:TVkExtent2D;
     end;

     PPVkRect3D=^PVkRect3D;
     PVkRect3D=^TVkRect3D;
     TVkRect3D=record
      offset:TVkOffset3D;
      extent:TVkExtent3D;
     end;

     PPVkClearRect=^PVkClearRect;
     PVkClearRect=^TVkClearRect;
     TVkClearRect=record
      rect:TVkRect2D;
      baseArrayLayer:TVkUInt32;
      layerCount:TVkUInt32;
     end;

     PPVkComponentMapping=^PVkComponentMapping;
     PVkComponentMapping=^TVkComponentMapping;
     TVkComponentMapping=record
      r:TVkComponentSwizzle;
      g:TVkComponentSwizzle;
      b:TVkComponentSwizzle;
      a:TVkComponentSwizzle;
     end;

     PPVkPhysicalDeviceSparseProperties=^PVkPhysicalDeviceSparseProperties;
     PVkPhysicalDeviceSparseProperties=^TVkPhysicalDeviceSparseProperties;
     TVkPhysicalDeviceSparseProperties=record
      residencyStandard2DBlockShape:TVkBool32;
      residencyStandard2DMultisampleBlockShape:TVkBool32;
      residencyStandard3DBlockShape:TVkBool32;
      residencyAlignedMipSize:TVkBool32;
      residencyNonResidentStrict:TVkBool32;
     end;

     PPVkExtensionProperties=^PVkExtensionProperties;
     PVkExtensionProperties=^TVkExtensionProperties;
     TVkExtensionProperties=record
      extensionName:array[0..VK_MAX_EXTENSION_NAME_SIZE-1] of TVkChar;
      specVersion:TVkUInt32;
     end;

     PPVkLayerProperties=^PVkLayerProperties;
     PVkLayerProperties=^TVkLayerProperties;
     TVkLayerProperties=record
      layerName:array[0..VK_MAX_EXTENSION_NAME_SIZE-1] of TVkChar;
      specVersion:TVkUInt32;
      implementationVersion:TVkUInt32;
      description:array[0..VK_MAX_DESCRIPTION_SIZE-1] of TVkChar;
     end;

     PPVkApplicationInfo=^PVkApplicationInfo;
     PVkApplicationInfo=^TVkApplicationInfo;
     TVkApplicationInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      pApplicationName:PVkChar;
      applicationVersion:TVkUInt32;
      pEngineName:PVkChar;
      engineVersion:TVkUInt32;
      apiVersion:TVkUInt32;
     end;

     PPVkAllocationCallbacks=^PVkAllocationCallbacks;
     PVkAllocationCallbacks=^TVkAllocationCallbacks;
     TVkAllocationCallbacks=record
      pUserData:TVkPointer;
      pfnAllocation:TPFN_vkAllocationFunction;
      pfnReallocation:TPFN_vkReallocationFunction;
      pfnFree:TPFN_vkFreeFunction;
      pfnInternalAllocation:TPFN_vkInternalAllocationNotification;
      pfnInternalFree:TPFN_vkInternalFreeNotification;
     end;

     PPVkDeviceQueueCreateInfo=^PVkDeviceQueueCreateInfo;
     PVkDeviceQueueCreateInfo=^TVkDeviceQueueCreateInfo;
     TVkDeviceQueueCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkDeviceQueueCreateFlags;
      queueFamilyIndex:TVkUInt32;
      queueCount:TVkUInt32;
      pQueuePriorities:PVkFloat;
     end;

     PPVkPhysicalDeviceFeatures=^PVkPhysicalDeviceFeatures;
     PVkPhysicalDeviceFeatures=^TVkPhysicalDeviceFeatures;
     TVkPhysicalDeviceFeatures=record
      robustBufferAccess:TVkBool32;
      fullDrawIndexUint32:TVkBool32;
      imageCubeArray:TVkBool32;
      independentBlend:TVkBool32;
      geometryShader:TVkBool32;
      tessellationShader:TVkBool32;
      sampleRateShading:TVkBool32;
      dualSrcBlend:TVkBool32;
      logicOp:TVkBool32;
      multiDrawIndirect:TVkBool32;
      drawIndirectFirstInstance:TVkBool32;
      depthClamp:TVkBool32;
      depthBiasClamp:TVkBool32;
      fillModeNonSolid:TVkBool32;
      depthBounds:TVkBool32;
      wideLines:TVkBool32;
      largePoints:TVkBool32;
      alphaToOne:TVkBool32;
      multiViewport:TVkBool32;
      samplerAnisotropy:TVkBool32;
      textureCompressionETC2:TVkBool32;
      textureCompressionASTC_LDR:TVkBool32;
      textureCompressionBC:TVkBool32;
      occlusionQueryPrecise:TVkBool32;
      pipelineStatisticsQuery:TVkBool32;
      vertexPipelineStoresAndAtomics:TVkBool32;
      fragmentStoresAndAtomics:TVkBool32;
      shaderTessellationAndGeometryPointSize:TVkBool32;
      shaderImageGatherExtended:TVkBool32;
      shaderStorageImageExtendedFormats:TVkBool32;
      shaderStorageImageMultisample:TVkBool32;
      shaderStorageImageReadWithoutFormat:TVkBool32;
      shaderStorageImageWriteWithoutFormat:TVkBool32;
      shaderUniformBufferArrayDynamicIndexing:TVkBool32;
      shaderSampledImageArrayDynamicIndexing:TVkBool32;
      shaderStorageBufferArrayDynamicIndexing:TVkBool32;
      shaderStorageImageArrayDynamicIndexing:TVkBool32;
      shaderClipDistance:TVkBool32;
      shaderCullDistance:TVkBool32;
      shaderFloat64:TVkBool32;
      shaderInt64:TVkBool32;
      shaderInt16:TVkBool32;
      shaderResourceResidency:TVkBool32;
      shaderResourceMinLod:TVkBool32;
      sparseBinding:TVkBool32;
      sparseResidencyBuffer:TVkBool32;
      sparseResidencyImage2D:TVkBool32;
      sparseResidencyImage3D:TVkBool32;
      sparseResidency2Samples:TVkBool32;
      sparseResidency4Samples:TVkBool32;
      sparseResidency8Samples:TVkBool32;
      sparseResidency16Samples:TVkBool32;
      sparseResidencyAliased:TVkBool32;
      variableMultisampleRate:TVkBool32;
      inheritedQueries:TVkBool32;
     end;

     PPVkInstanceCreateInfo=^PVkInstanceCreateInfo;
     PVkInstanceCreateInfo=^TVkInstanceCreateInfo;
     TVkInstanceCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkInstanceCreateFlags;
      pApplicationInfo:PVkApplicationInfo;
      enabledLayerCount:TVkUInt32;
      ppEnabledLayerNames:PVkChar;
      enabledExtensionCount:TVkUInt32;
      ppEnabledExtensionNames:PVkChar;
     end;

     PPVkQueueFamilyProperties=^PVkQueueFamilyProperties;
     PVkQueueFamilyProperties=^TVkQueueFamilyProperties;
     TVkQueueFamilyProperties=record
      queueFlags:TVkQueueFlags;
      queueCount:TVkUInt32;
      timestampValidBits:TVkUInt32;
      minImageTransferGranularity:TVkExtent3D;
     end;

     PPVkMemoryType=^PVkMemoryType;
     PVkMemoryType=^TVkMemoryType;
     TVkMemoryType=record
      propertyFlags:TVkMemoryPropertyFlags;
      heapIndex:TVkUInt32;
     end;

     PPVkMemoryAllocateInfo=^PVkMemoryAllocateInfo;
     PVkMemoryAllocateInfo=^TVkMemoryAllocateInfo;
     TVkMemoryAllocateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      allocationSize:TVkDeviceSize;
      memoryTypeIndex:TVkUInt32;
     end;

     PPVkMemoryRequirements=^PVkMemoryRequirements;
     PVkMemoryRequirements=^TVkMemoryRequirements;
     TVkMemoryRequirements=record
      size:TVkDeviceSize;
      alignment:TVkDeviceSize;
      memoryTypeBits:TVkUInt32;
     end;

     PPVkSparseImageFormatProperties=^PVkSparseImageFormatProperties;
     PVkSparseImageFormatProperties=^TVkSparseImageFormatProperties;
     TVkSparseImageFormatProperties=record
      aspectMask:TVkImageAspectFlags;
      imageGranularity:TVkExtent3D;
      flags:TVkSparseImageFormatFlags;
     end;

     PPVkSparseImageMemoryRequirements=^PVkSparseImageMemoryRequirements;
     PVkSparseImageMemoryRequirements=^TVkSparseImageMemoryRequirements;
     TVkSparseImageMemoryRequirements=record
      formatProperties:TVkSparseImageFormatProperties;
      imageMipTailFirstLod:TVkUInt32;
      imageMipTailSize:TVkDeviceSize;
      imageMipTailOffset:TVkDeviceSize;
      imageMipTailStride:TVkDeviceSize;
     end;

     PPVkMemoryHeap=^PVkMemoryHeap;
     PVkMemoryHeap=^TVkMemoryHeap;
     TVkMemoryHeap=record
      size:TVkDeviceSize;
      flags:TVkMemoryHeapFlags;
     end;

     PPVkPhysicalDeviceMemoryProperties=^PVkPhysicalDeviceMemoryProperties;
     PVkPhysicalDeviceMemoryProperties=^TVkPhysicalDeviceMemoryProperties;
     TVkPhysicalDeviceMemoryProperties=record
      memoryTypeCount:TVkUInt32;
      memoryTypes:array[0..VK_MAX_MEMORY_TYPES-1] of TVkMemoryType;
      memoryHeapCount:TVkUInt32;
      memoryHeaps:array[0..VK_MAX_MEMORY_HEAPS-1] of TVkMemoryHeap;
     end;

     PPVkMappedMemoryRange=^PVkMappedMemoryRange;
     PVkMappedMemoryRange=^TVkMappedMemoryRange;
     TVkMappedMemoryRange=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      memory:TVkDeviceMemory;
      offset:TVkDeviceSize;
      size:TVkDeviceSize;
     end;

     PPVkFormatProperties=^PVkFormatProperties;
     PVkFormatProperties=^TVkFormatProperties;
     TVkFormatProperties=record
      linearTilingFeatures:TVkFormatFeatureFlags;
      optimalTilingFeatures:TVkFormatFeatureFlags;
      bufferFeatures:TVkFormatFeatureFlags;
     end;

     PPVkImageFormatProperties=^PVkImageFormatProperties;
     PVkImageFormatProperties=^TVkImageFormatProperties;
     TVkImageFormatProperties=record
      maxExtent:TVkExtent3D;
      maxMipLevels:TVkUInt32;
      maxArrayLayers:TVkUInt32;
      sampleCounts:TVkSampleCountFlags;
      maxResourceSize:TVkDeviceSize;
     end;

     PPVkDescriptorBufferInfo=^PVkDescriptorBufferInfo;
     PVkDescriptorBufferInfo=^TVkDescriptorBufferInfo;
     TVkDescriptorBufferInfo=record
      buffer:TVkBuffer;
      offset:TVkDeviceSize;
      range:TVkDeviceSize;
     end;

     PPVkDescriptorImageInfo=^PVkDescriptorImageInfo;
     PVkDescriptorImageInfo=^TVkDescriptorImageInfo;
     TVkDescriptorImageInfo=record
      sampler:TVkSampler;
      imageView:TVkImageView;
      imageLayout:TVkImageLayout;
     end;

     PPVkWriteDescriptorSet=^PVkWriteDescriptorSet;
     PVkWriteDescriptorSet=^TVkWriteDescriptorSet;
     TVkWriteDescriptorSet=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      dstSet:TVkDescriptorSet;
      dstBinding:TVkUInt32;
      dstArrayElement:TVkUInt32;
      descriptorCount:TVkUInt32;
      descriptorType:TVkDescriptorType;
      pImageInfo:PVkDescriptorImageInfo;
      pBufferInfo:PVkDescriptorBufferInfo;
      pTexelBufferView:PVkBufferView;
     end;

     PPVkCopyDescriptorSet=^PVkCopyDescriptorSet;
     PVkCopyDescriptorSet=^TVkCopyDescriptorSet;
     TVkCopyDescriptorSet=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      srcSet:TVkDescriptorSet;
      srcBinding:TVkUInt32;
      srcArrayElement:TVkUInt32;
      dstSet:TVkDescriptorSet;
      dstBinding:TVkUInt32;
      dstArrayElement:TVkUInt32;
      descriptorCount:TVkUInt32;
     end;

     PPVkBufferCreateInfo=^PVkBufferCreateInfo;
     PVkBufferCreateInfo=^TVkBufferCreateInfo;
     TVkBufferCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkBufferCreateFlags;
      size:TVkDeviceSize;
      usage:TVkBufferUsageFlags;
      sharingMode:TVkSharingMode;
      queueFamilyIndexCount:TVkUInt32;
      pQueueFamilyIndices:PVkUInt32;
     end;

     PPVkBufferViewCreateInfo=^PVkBufferViewCreateInfo;
     PVkBufferViewCreateInfo=^TVkBufferViewCreateInfo;
     TVkBufferViewCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkBufferViewCreateFlags;
      buffer:TVkBuffer;
      format:TVkFormat;
      offset:TVkDeviceSize;
      range:TVkDeviceSize;
     end;

     PPVkImageSubresource=^PVkImageSubresource;
     PVkImageSubresource=^TVkImageSubresource;
     TVkImageSubresource=record
      aspectMask:TVkImageAspectFlags;
      mipLevel:TVkUInt32;
      arrayLayer:TVkUInt32;
     end;

     PPVkImageSubresourceLayers=^PVkImageSubresourceLayers;
     PVkImageSubresourceLayers=^TVkImageSubresourceLayers;
     TVkImageSubresourceLayers=record
      aspectMask:TVkImageAspectFlags;
      mipLevel:TVkUInt32;
      baseArrayLayer:TVkUInt32;
      layerCount:TVkUInt32;
     end;

     PPVkImageSubresourceRange=^PVkImageSubresourceRange;
     PVkImageSubresourceRange=^TVkImageSubresourceRange;
     TVkImageSubresourceRange=record
      aspectMask:TVkImageAspectFlags;
      baseMipLevel:TVkUInt32;
      levelCount:TVkUInt32;
      baseArrayLayer:TVkUInt32;
      layerCount:TVkUInt32;
     end;

     PPVkMemoryBarrier=^PVkMemoryBarrier;
     PVkMemoryBarrier=^TVkMemoryBarrier;
     TVkMemoryBarrier=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      srcAccessMask:TVkAccessFlags;
      dstAccessMask:TVkAccessFlags;
     end;

     PPVkBufferMemoryBarrier=^PVkBufferMemoryBarrier;
     PVkBufferMemoryBarrier=^TVkBufferMemoryBarrier;
     TVkBufferMemoryBarrier=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      srcAccessMask:TVkAccessFlags;
      dstAccessMask:TVkAccessFlags;
      srcQueueFamilyIndex:TVkUInt32;
      dstQueueFamilyIndex:TVkUInt32;
      buffer:TVkBuffer;
      offset:TVkDeviceSize;
      size:TVkDeviceSize;
     end;

     PPVkImageMemoryBarrier=^PVkImageMemoryBarrier;
     PVkImageMemoryBarrier=^TVkImageMemoryBarrier;
     TVkImageMemoryBarrier=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      srcAccessMask:TVkAccessFlags;
      dstAccessMask:TVkAccessFlags;
      oldLayout:TVkImageLayout;
      newLayout:TVkImageLayout;
      srcQueueFamilyIndex:TVkUInt32;
      dstQueueFamilyIndex:TVkUInt32;
      image:TVkImage;
      subresourceRange:TVkImageSubresourceRange;
     end;

     PPVkImageCreateInfo=^PVkImageCreateInfo;
     PVkImageCreateInfo=^TVkImageCreateInfo;
     TVkImageCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkImageCreateFlags;
      imageType:TVkImageType;
      format:TVkFormat;
      extent:TVkExtent3D;
      mipLevels:TVkUInt32;
      arrayLayers:TVkUInt32;
      samples:TVkSampleCountFlagBits;
      tiling:TVkImageTiling;
      usage:TVkImageUsageFlags;
      sharingMode:TVkSharingMode;
      queueFamilyIndexCount:TVkUInt32;
      pQueueFamilyIndices:PVkUInt32;
      initialLayout:TVkImageLayout;
     end;

     PPVkSubresourceLayout=^PVkSubresourceLayout;
     PVkSubresourceLayout=^TVkSubresourceLayout;
     TVkSubresourceLayout=record
      offset:TVkDeviceSize;
      size:TVkDeviceSize;
      rowPitch:TVkDeviceSize;
      arrayPitch:TVkDeviceSize;
      depthPitch:TVkDeviceSize;
     end;

     PPVkImageViewCreateInfo=^PVkImageViewCreateInfo;
     PVkImageViewCreateInfo=^TVkImageViewCreateInfo;
     TVkImageViewCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkImageViewCreateFlags;
      image:TVkImage;
      viewType:TVkImageViewType;
      format:TVkFormat;
      components:TVkComponentMapping;
      subresourceRange:TVkImageSubresourceRange;
     end;

     PPVkBufferCopy=^PVkBufferCopy;
     PVkBufferCopy=^TVkBufferCopy;
     TVkBufferCopy=record
      srcOffset:TVkDeviceSize;
      dstOffset:TVkDeviceSize;
      size:TVkDeviceSize;
     end;

     PPVkSparseMemoryBind=^PVkSparseMemoryBind;
     PVkSparseMemoryBind=^TVkSparseMemoryBind;
     TVkSparseMemoryBind=record
      resourceOffset:TVkDeviceSize;
      size:TVkDeviceSize;
      memory:TVkDeviceMemory;
      memoryOffset:TVkDeviceSize;
      flags:TVkSparseMemoryBindFlags;
     end;

     PPVkSparseImageMemoryBind=^PVkSparseImageMemoryBind;
     PVkSparseImageMemoryBind=^TVkSparseImageMemoryBind;
     TVkSparseImageMemoryBind=record
      subresource:TVkImageSubresource;
      offset:TVkOffset3D;
      extent:TVkExtent3D;
      memory:TVkDeviceMemory;
      memoryOffset:TVkDeviceSize;
      flags:TVkSparseMemoryBindFlags;
     end;

     PPVkSparseBufferMemoryBindInfo=^PVkSparseBufferMemoryBindInfo;
     PVkSparseBufferMemoryBindInfo=^TVkSparseBufferMemoryBindInfo;
     TVkSparseBufferMemoryBindInfo=record
      buffer:TVkBuffer;
      bindCount:TVkUInt32;
      pBinds:PVkSparseMemoryBind;
     end;

     PPVkSparseImageOpaqueMemoryBindInfo=^PVkSparseImageOpaqueMemoryBindInfo;
     PVkSparseImageOpaqueMemoryBindInfo=^TVkSparseImageOpaqueMemoryBindInfo;
     TVkSparseImageOpaqueMemoryBindInfo=record
      image:TVkImage;
      bindCount:TVkUInt32;
      pBinds:PVkSparseMemoryBind;
     end;

     PPVkSparseImageMemoryBindInfo=^PVkSparseImageMemoryBindInfo;
     PVkSparseImageMemoryBindInfo=^TVkSparseImageMemoryBindInfo;
     TVkSparseImageMemoryBindInfo=record
      image:TVkImage;
      bindCount:TVkUInt32;
      pBinds:PVkSparseImageMemoryBind;
     end;

     PPVkBindSparseInfo=^PVkBindSparseInfo;
     PVkBindSparseInfo=^TVkBindSparseInfo;
     TVkBindSparseInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      waitSemaphoreCount:TVkUInt32;
      pWaitSemaphores:PVkSemaphore;
      bufferBindCount:TVkUInt32;
      pBufferBinds:PVkSparseBufferMemoryBindInfo;
      imageOpaqueBindCount:TVkUInt32;
      pImageOpaqueBinds:PVkSparseImageOpaqueMemoryBindInfo;
      imageBindCount:TVkUInt32;
      pImageBinds:PVkSparseImageMemoryBindInfo;
      signalSemaphoreCount:TVkUInt32;
      pSignalSemaphores:PVkSemaphore;
     end;

     PPVkImageCopy=^PVkImageCopy;
     PVkImageCopy=^TVkImageCopy;
     TVkImageCopy=record
      srcSubresource:TVkImageSubresourceLayers;
      srcOffset:TVkOffset3D;
      dstSubresource:TVkImageSubresourceLayers;
      dstOffset:TVkOffset3D;
      extent:TVkExtent3D;
     end;

     PPVkImageBlit=^PVkImageBlit;
     PVkImageBlit=^TVkImageBlit;
     TVkImageBlit=record
      srcSubresource:TVkImageSubresourceLayers;
      srcOffsets:array[0..1] of TVkOffset3D;
      dstSubresource:TVkImageSubresourceLayers;
      dstOffsets:array[0..1] of TVkOffset3D;
     end;

     PPVkBufferImageCopy=^PVkBufferImageCopy;
     PVkBufferImageCopy=^TVkBufferImageCopy;
     TVkBufferImageCopy=record
      bufferOffset:TVkDeviceSize;
      bufferRowLength:TVkUInt32;
      bufferImageHeight:TVkUInt32;
      imageSubresource:TVkImageSubresourceLayers;
      imageOffset:TVkOffset3D;
      imageExtent:TVkExtent3D;
     end;

     PPVkImageResolve=^PVkImageResolve;
     PVkImageResolve=^TVkImageResolve;
     TVkImageResolve=record
      srcSubresource:TVkImageSubresourceLayers;
      srcOffset:TVkOffset3D;
      dstSubresource:TVkImageSubresourceLayers;
      dstOffset:TVkOffset3D;
      extent:TVkExtent3D;
     end;

     PPVkShaderModuleCreateInfo=^PVkShaderModuleCreateInfo;
     PVkShaderModuleCreateInfo=^TVkShaderModuleCreateInfo;
     TVkShaderModuleCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkShaderModuleCreateFlags;
      codeSize:TVkPtrInt;
      pCode:PVkUInt32;
     end;

     PPVkDescriptorSetLayoutBinding=^PVkDescriptorSetLayoutBinding;
     PVkDescriptorSetLayoutBinding=^TVkDescriptorSetLayoutBinding;
     TVkDescriptorSetLayoutBinding=record
      binding:TVkUInt32;
      descriptorType:TVkDescriptorType;
      descriptorCount:TVkUInt32;
      stageFlags:TVkShaderStageFlags;
      pImmutableSamplers:PVkSampler;
     end;

     PPVkDescriptorSetLayoutCreateInfo=^PVkDescriptorSetLayoutCreateInfo;
     PVkDescriptorSetLayoutCreateInfo=^TVkDescriptorSetLayoutCreateInfo;
     TVkDescriptorSetLayoutCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkDescriptorSetLayoutCreateFlags;
      bindingCount:TVkUInt32;
      pBindings:PVkDescriptorSetLayoutBinding;
     end;

     PPVkDescriptorPoolSize=^PVkDescriptorPoolSize;
     PVkDescriptorPoolSize=^TVkDescriptorPoolSize;
     TVkDescriptorPoolSize=record
      type_:TVkDescriptorType;
      descriptorCount:TVkUInt32;
     end;

     PPVkDescriptorPoolCreateInfo=^PVkDescriptorPoolCreateInfo;
     PVkDescriptorPoolCreateInfo=^TVkDescriptorPoolCreateInfo;
     TVkDescriptorPoolCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkDescriptorPoolCreateFlags;
      maxSets:TVkUInt32;
      poolSizeCount:TVkUInt32;
      pPoolSizes:PVkDescriptorPoolSize;
     end;

     PPVkDescriptorSetAllocateInfo=^PVkDescriptorSetAllocateInfo;
     PVkDescriptorSetAllocateInfo=^TVkDescriptorSetAllocateInfo;
     TVkDescriptorSetAllocateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      descriptorPool:TVkDescriptorPool;
      descriptorSetCount:TVkUInt32;
      pSetLayouts:PVkDescriptorSetLayout;
     end;

     PPVkSpecializationMapEntry=^PVkSpecializationMapEntry;
     PVkSpecializationMapEntry=^TVkSpecializationMapEntry;
     TVkSpecializationMapEntry=record
      constantID:TVkUInt32;
      offset:TVkUInt32;
      size:TVkPtrInt;
     end;

     PPVkSpecializationInfo=^PVkSpecializationInfo;
     PVkSpecializationInfo=^TVkSpecializationInfo;
     TVkSpecializationInfo=record
      mapEntryCount:TVkUInt32;
      pMapEntries:PVkSpecializationMapEntry;
      dataSize:TVkPtrInt;
      pData:TVkPointer;
     end;

     PPVkPipelineShaderStageCreateInfo=^PVkPipelineShaderStageCreateInfo;
     PVkPipelineShaderStageCreateInfo=^TVkPipelineShaderStageCreateInfo;
     TVkPipelineShaderStageCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineShaderStageCreateFlags;
      stage:TVkShaderStageFlagBits;
      module:TVkShaderModule;
      pName:PVkChar;
      pSpecializationInfo:PVkSpecializationInfo;
     end;

     PPVkComputePipelineCreateInfo=^PVkComputePipelineCreateInfo;
     PVkComputePipelineCreateInfo=^TVkComputePipelineCreateInfo;
     TVkComputePipelineCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineCreateFlags;
      stage:TVkPipelineShaderStageCreateInfo;
      layout:TVkPipelineLayout;
      basePipelineHandle:TVkPipeline;
      basePipelineIndex:TVkInt32;
     end;

     PPVkVertexInputBindingDescription=^PVkVertexInputBindingDescription;
     PVkVertexInputBindingDescription=^TVkVertexInputBindingDescription;
     TVkVertexInputBindingDescription=record
      binding:TVkUInt32;
      stride:TVkUInt32;
      inputRate:TVkVertexInputRate;
     end;

     PPVkVertexInputAttributeDescription=^PVkVertexInputAttributeDescription;
     PVkVertexInputAttributeDescription=^TVkVertexInputAttributeDescription;
     TVkVertexInputAttributeDescription=record
      location:TVkUInt32;
      binding:TVkUInt32;
      format:TVkFormat;
      offset:TVkUInt32;
     end;

     PPVkPipelineVertexInputStateCreateInfo=^PVkPipelineVertexInputStateCreateInfo;
     PVkPipelineVertexInputStateCreateInfo=^TVkPipelineVertexInputStateCreateInfo;
     TVkPipelineVertexInputStateCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineVertexInputStateCreateFlags;
      vertexBindingDescriptionCount:TVkUInt32;
      pVertexBindingDescriptions:PVkVertexInputBindingDescription;
      vertexAttributeDescriptionCount:TVkUInt32;
      pVertexAttributeDescriptions:PVkVertexInputAttributeDescription;
     end;

     PPVkPipelineInputAssemblyStateCreateInfo=^PVkPipelineInputAssemblyStateCreateInfo;
     PVkPipelineInputAssemblyStateCreateInfo=^TVkPipelineInputAssemblyStateCreateInfo;
     TVkPipelineInputAssemblyStateCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineInputAssemblyStateCreateFlags;
      topology:TVkPrimitiveTopology;
      primitiveRestartEnable:TVkBool32;
     end;

     PPVkPipelineTessellationStateCreateInfo=^PVkPipelineTessellationStateCreateInfo;
     PVkPipelineTessellationStateCreateInfo=^TVkPipelineTessellationStateCreateInfo;
     TVkPipelineTessellationStateCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineTessellationStateCreateFlags;
      patchControlPoints:TVkUInt32;
     end;

     PPVkPipelineViewportStateCreateInfo=^PVkPipelineViewportStateCreateInfo;
     PVkPipelineViewportStateCreateInfo=^TVkPipelineViewportStateCreateInfo;
     TVkPipelineViewportStateCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineViewportStateCreateFlags;
      viewportCount:TVkUInt32;
      pViewports:PVkViewport;
      scissorCount:TVkUInt32;
      pScissors:PVkRect2D;
     end;

     PPVkPipelineRasterizationStateCreateInfo=^PVkPipelineRasterizationStateCreateInfo;
     PVkPipelineRasterizationStateCreateInfo=^TVkPipelineRasterizationStateCreateInfo;
     TVkPipelineRasterizationStateCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineRasterizationStateCreateFlags;
      depthClampEnable:TVkBool32;
      rasterizerDiscardEnable:TVkBool32;
      polygonMode:TVkPolygonMode;
      cullMode:TVkCullModeFlags;
      frontFace:TVkFrontFace;
      depthBiasEnable:TVkBool32;
      depthBiasConstantFactor:TVkFloat;
      depthBiasClamp:TVkFloat;
      depthBiasSlopeFactor:TVkFloat;
      lineWidth:TVkFloat;
     end;

     PPVkPipelineMultisampleStateCreateInfo=^PVkPipelineMultisampleStateCreateInfo;
     PVkPipelineMultisampleStateCreateInfo=^TVkPipelineMultisampleStateCreateInfo;
     TVkPipelineMultisampleStateCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineMultisampleStateCreateFlags;
      rasterizationSamples:TVkSampleCountFlagBits;
      sampleShadingEnable:TVkBool32;
      minSampleShading:TVkFloat;
      pSampleMask:PVkSampleMask;
      alphaToCoverageEnable:TVkBool32;
      alphaToOneEnable:TVkBool32;
     end;

     PPVkPipelineColorBlendAttachmentState=^PVkPipelineColorBlendAttachmentState;
     PVkPipelineColorBlendAttachmentState=^TVkPipelineColorBlendAttachmentState;
     TVkPipelineColorBlendAttachmentState=record
      blendEnable:TVkBool32;
      srcColorBlendFactor:TVkBlendFactor;
      dstColorBlendFactor:TVkBlendFactor;
      colorBlendOp:TVkBlendOp;
      srcAlphaBlendFactor:TVkBlendFactor;
      dstAlphaBlendFactor:TVkBlendFactor;
      alphaBlendOp:TVkBlendOp;
      colorWriteMask:TVkColorComponentFlags;
     end;

     PPVkPipelineColorBlendStateCreateInfo=^PVkPipelineColorBlendStateCreateInfo;
     PVkPipelineColorBlendStateCreateInfo=^TVkPipelineColorBlendStateCreateInfo;
     TVkPipelineColorBlendStateCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineColorBlendStateCreateFlags;
      logicOpEnable:TVkBool32;
      logicOp:TVkLogicOp;
      attachmentCount:TVkUInt32;
      pAttachments:PVkPipelineColorBlendAttachmentState;
      blendConstants:array[0..3] of TVkFloat;
     end;

     PPVkPipelineDynamicStateCreateInfo=^PVkPipelineDynamicStateCreateInfo;
     PVkPipelineDynamicStateCreateInfo=^TVkPipelineDynamicStateCreateInfo;
     TVkPipelineDynamicStateCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineDynamicStateCreateFlags;
      dynamicStateCount:TVkUInt32;
      pDynamicStates:PVkDynamicState;
     end;

     PPVkStencilOpState=^PVkStencilOpState;
     PVkStencilOpState=^TVkStencilOpState;
     TVkStencilOpState=record
      failOp:TVkStencilOp;
      passOp:TVkStencilOp;
      depthFailOp:TVkStencilOp;
      compareOp:TVkCompareOp;
      compareMask:TVkUInt32;
      writeMask:TVkUInt32;
      reference:TVkUInt32;
     end;

     PPVkPipelineDepthStencilStateCreateInfo=^PVkPipelineDepthStencilStateCreateInfo;
     PVkPipelineDepthStencilStateCreateInfo=^TVkPipelineDepthStencilStateCreateInfo;
     TVkPipelineDepthStencilStateCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineDepthStencilStateCreateFlags;
      depthTestEnable:TVkBool32;
      depthWriteEnable:TVkBool32;
      depthCompareOp:TVkCompareOp;
      depthBoundsTestEnable:TVkBool32;
      stencilTestEnable:TVkBool32;
      front:TVkStencilOpState;
      back:TVkStencilOpState;
      minDepthBounds:TVkFloat;
      maxDepthBounds:TVkFloat;
     end;

     PPVkGraphicsPipelineCreateInfo=^PVkGraphicsPipelineCreateInfo;
     PVkGraphicsPipelineCreateInfo=^TVkGraphicsPipelineCreateInfo;
     TVkGraphicsPipelineCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineCreateFlags;
      stageCount:TVkUInt32;
      pStages:PVkPipelineShaderStageCreateInfo;
      pVertexInputState:PVkPipelineVertexInputStateCreateInfo;
      pInputAssemblyState:PVkPipelineInputAssemblyStateCreateInfo;
      pTessellationState:PVkPipelineTessellationStateCreateInfo;
      pViewportState:PVkPipelineViewportStateCreateInfo;
      pRasterizationState:PVkPipelineRasterizationStateCreateInfo;
      pMultisampleState:PVkPipelineMultisampleStateCreateInfo;
      pDepthStencilState:PVkPipelineDepthStencilStateCreateInfo;
      pColorBlendState:PVkPipelineColorBlendStateCreateInfo;
      pDynamicState:PVkPipelineDynamicStateCreateInfo;
      layout:TVkPipelineLayout;
      renderPass:TVkRenderPass;
      subpass:TVkUInt32;
      basePipelineHandle:TVkPipeline;
      basePipelineIndex:TVkInt32;
     end;

     PPVkPipelineCacheCreateInfo=^PVkPipelineCacheCreateInfo;
     PVkPipelineCacheCreateInfo=^TVkPipelineCacheCreateInfo;
     TVkPipelineCacheCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineCacheCreateFlags;
      initialDataSize:TVkPtrInt;
      pInitialData:TVkPointer;
     end;

     PPVkPushConstantRange=^PVkPushConstantRange;
     PVkPushConstantRange=^TVkPushConstantRange;
     TVkPushConstantRange=record
      stageFlags:TVkShaderStageFlags;
      offset:TVkUInt32;
      size:TVkUInt32;
     end;

     PPVkPipelineLayoutCreateInfo=^PVkPipelineLayoutCreateInfo;
     PVkPipelineLayoutCreateInfo=^TVkPipelineLayoutCreateInfo;
     TVkPipelineLayoutCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineLayoutCreateFlags;
      setLayoutCount:TVkUInt32;
      pSetLayouts:PVkDescriptorSetLayout;
      pushConstantRangeCount:TVkUInt32;
      pPushConstantRanges:PVkPushConstantRange;
     end;

     PPVkSamplerCreateInfo=^PVkSamplerCreateInfo;
     PVkSamplerCreateInfo=^TVkSamplerCreateInfo;
     TVkSamplerCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkSamplerCreateFlags;
      magFilter:TVkFilter;
      minFilter:TVkFilter;
      mipmapMode:TVkSamplerMipmapMode;
      addressModeU:TVkSamplerAddressMode;
      addressModeV:TVkSamplerAddressMode;
      addressModeW:TVkSamplerAddressMode;
      mipLodBias:TVkFloat;
      anisotropyEnable:TVkBool32;
      maxAnisotropy:TVkFloat;
      compareEnable:TVkBool32;
      compareOp:TVkCompareOp;
      minLod:TVkFloat;
      maxLod:TVkFloat;
      borderColor:TVkBorderColor;
      unnormalizedCoordinates:TVkBool32;
     end;

     PPVkCommandPoolCreateInfo=^PVkCommandPoolCreateInfo;
     PVkCommandPoolCreateInfo=^TVkCommandPoolCreateInfo;
     TVkCommandPoolCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkCommandPoolCreateFlags;
      queueFamilyIndex:TVkUInt32;
     end;

     PPVkCommandBufferAllocateInfo=^PVkCommandBufferAllocateInfo;
     PVkCommandBufferAllocateInfo=^TVkCommandBufferAllocateInfo;
     TVkCommandBufferAllocateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      commandPool:TVkCommandPool;
      level:TVkCommandBufferLevel;
      commandBufferCount:TVkUInt32;
     end;

     PPVkCommandBufferInheritanceInfo=^PVkCommandBufferInheritanceInfo;
     PVkCommandBufferInheritanceInfo=^TVkCommandBufferInheritanceInfo;
     TVkCommandBufferInheritanceInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      renderPass:TVkRenderPass;
      subpass:TVkUInt32;
      framebuffer:TVkFramebuffer;
      occlusionQueryEnable:TVkBool32;
      queryFlags:TVkQueryControlFlags;
      pipelineStatistics:TVkQueryPipelineStatisticFlags;
     end;

     PPVkCommandBufferBeginInfo=^PVkCommandBufferBeginInfo;
     PVkCommandBufferBeginInfo=^TVkCommandBufferBeginInfo;
     TVkCommandBufferBeginInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkCommandBufferUsageFlags;
      pInheritanceInfo:PVkCommandBufferInheritanceInfo;
     end;

     PPVkClearColorValue=^PVkClearColorValue;
     PVkClearColorValue=^TVkClearColorValue;
     TVkClearColorValue=record
      case longint of
       0:(
        float32:array[0..3] of TVkFloat;
       );
       1:(
        int32:array[0..3] of TVkInt32;
       );
       2:(
        uint32:array[0..3] of TVkUInt32;
       );
     end;

     PPVkClearDepthStencilValue=^PVkClearDepthStencilValue;
     PVkClearDepthStencilValue=^TVkClearDepthStencilValue;
     TVkClearDepthStencilValue=record
      depth:TVkFloat;
      stencil:TVkUInt32;
     end;

     PPVkClearValue=^PVkClearValue;
     PVkClearValue=^TVkClearValue;
     TVkClearValue=record
      case longint of
       0:(
        color:TVkClearColorValue;
       );
       1:(
        depthStencil:TVkClearDepthStencilValue;
       );
     end;

     PPVkRenderPassBeginInfo=^PVkRenderPassBeginInfo;
     PVkRenderPassBeginInfo=^TVkRenderPassBeginInfo;
     TVkRenderPassBeginInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      renderPass:TVkRenderPass;
      framebuffer:TVkFramebuffer;
      renderArea:TVkRect2D;
      clearValueCount:TVkUInt32;
      pClearValues:PVkClearValue;
     end;

     PPVkClearAttachment=^PVkClearAttachment;
     PVkClearAttachment=^TVkClearAttachment;
     TVkClearAttachment=record
      aspectMask:TVkImageAspectFlags;
      colorAttachment:TVkUInt32;
      clearValue:TVkClearValue;
     end;

     PPVkAttachmentDescription=^PVkAttachmentDescription;
     PVkAttachmentDescription=^TVkAttachmentDescription;
     TVkAttachmentDescription=record
      flags:TVkAttachmentDescriptionFlags;
      format:TVkFormat;
      samples:TVkSampleCountFlagBits;
      loadOp:TVkAttachmentLoadOp;
      storeOp:TVkAttachmentStoreOp;
      stencilLoadOp:TVkAttachmentLoadOp;
      stencilStoreOp:TVkAttachmentStoreOp;
      initialLayout:TVkImageLayout;
      finalLayout:TVkImageLayout;
     end;

     PPVkAttachmentReference=^PVkAttachmentReference;
     PVkAttachmentReference=^TVkAttachmentReference;
     TVkAttachmentReference=record
      attachment:TVkUInt32;
      layout:TVkImageLayout;
     end;

     PPVkSubpassDescription=^PVkSubpassDescription;
     PVkSubpassDescription=^TVkSubpassDescription;
     TVkSubpassDescription=record
      flags:TVkSubpassDescriptionFlags;
      pipelineBindPoint:TVkPipelineBindPoint;
      inputAttachmentCount:TVkUInt32;
      pInputAttachments:PVkAttachmentReference;
      colorAttachmentCount:TVkUInt32;
      pColorAttachments:PVkAttachmentReference;
      pResolveAttachments:PVkAttachmentReference;
      pDepthStencilAttachment:PVkAttachmentReference;
      preserveAttachmentCount:TVkUInt32;
      pPreserveAttachments:PVkUInt32;
     end;

     PPVkSubpassDependency=^PVkSubpassDependency;
     PVkSubpassDependency=^TVkSubpassDependency;
     TVkSubpassDependency=record
      srcSubpass:TVkUInt32;
      dstSubpass:TVkUInt32;
      srcStageMask:TVkPipelineStageFlags;
      dstStageMask:TVkPipelineStageFlags;
      srcAccessMask:TVkAccessFlags;
      dstAccessMask:TVkAccessFlags;
      dependencyFlags:TVkDependencyFlags;
     end;

     PPVkRenderPassCreateInfo=^PVkRenderPassCreateInfo;
     PVkRenderPassCreateInfo=^TVkRenderPassCreateInfo;
     TVkRenderPassCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkRenderPassCreateFlags;
      attachmentCount:TVkUInt32;
      pAttachments:PVkAttachmentDescription;
      subpassCount:TVkUInt32;
      pSubpasses:PVkSubpassDescription;
      dependencyCount:TVkUInt32;
      pDependencies:PVkSubpassDependency;
     end;

     PPVkEventCreateInfo=^PVkEventCreateInfo;
     PVkEventCreateInfo=^TVkEventCreateInfo;
     TVkEventCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkEventCreateFlags;
     end;

     PPVkFenceCreateInfo=^PVkFenceCreateInfo;
     PVkFenceCreateInfo=^TVkFenceCreateInfo;
     TVkFenceCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkFenceCreateFlags;
     end;

     PPVkDeviceCreateInfo=^PVkDeviceCreateInfo;
     PVkDeviceCreateInfo=^TVkDeviceCreateInfo;
     TVkDeviceCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkDeviceCreateFlags;
      queueCreateInfoCount:TVkUInt32;
      pQueueCreateInfos:PVkDeviceQueueCreateInfo;
      enabledLayerCount:TVkUInt32;
      ppEnabledLayerNames:PVkChar;
      enabledExtensionCount:TVkUInt32;
      ppEnabledExtensionNames:PVkChar;
      pEnabledFeatures:PVkPhysicalDeviceFeatures;
     end;

     PPVkPhysicalDeviceLimits=^PVkPhysicalDeviceLimits;
     PVkPhysicalDeviceLimits=^TVkPhysicalDeviceLimits;
     TVkPhysicalDeviceLimits=record
      maxImageDimension1D:TVkUInt32;
      maxImageDimension2D:TVkUInt32;
      maxImageDimension3D:TVkUInt32;
      maxImageDimensionCube:TVkUInt32;
      maxImageArrayLayers:TVkUInt32;
      maxTexelBufferElements:TVkUInt32;
      maxUniformBufferRange:TVkUInt32;
      maxStorageBufferRange:TVkUInt32;
      maxPushConstantsSize:TVkUInt32;
      maxMemoryAllocationCount:TVkUInt32;
      maxSamplerAllocationCount:TVkUInt32;
      bufferImageGranularity:TVkDeviceSize;
      sparseAddressSpaceSize:TVkDeviceSize;
      maxBoundDescriptorSets:TVkUInt32;
      maxPerStageDescriptorSamplers:TVkUInt32;
      maxPerStageDescriptorUniformBuffers:TVkUInt32;
      maxPerStageDescriptorStorageBuffers:TVkUInt32;
      maxPerStageDescriptorSampledImages:TVkUInt32;
      maxPerStageDescriptorStorageImages:TVkUInt32;
      maxPerStageDescriptorInputAttachments:TVkUInt32;
      maxPerStageResources:TVkUInt32;
      maxDescriptorSetSamplers:TVkUInt32;
      maxDescriptorSetUniformBuffers:TVkUInt32;
      maxDescriptorSetUniformBuffersDynamic:TVkUInt32;
      maxDescriptorSetStorageBuffers:TVkUInt32;
      maxDescriptorSetStorageBuffersDynamic:TVkUInt32;
      maxDescriptorSetSampledImages:TVkUInt32;
      maxDescriptorSetStorageImages:TVkUInt32;
      maxDescriptorSetInputAttachments:TVkUInt32;
      maxVertexInputAttributes:TVkUInt32;
      maxVertexInputBindings:TVkUInt32;
      maxVertexInputAttributeOffset:TVkUInt32;
      maxVertexInputBindingStride:TVkUInt32;
      maxVertexOutputComponents:TVkUInt32;
      maxTessellationGenerationLevel:TVkUInt32;
      maxTessellationPatchSize:TVkUInt32;
      maxTessellationControlPerVertexInputComponents:TVkUInt32;
      maxTessellationControlPerVertexOutputComponents:TVkUInt32;
      maxTessellationControlPerPatchOutputComponents:TVkUInt32;
      maxTessellationControlTotalOutputComponents:TVkUInt32;
      maxTessellationEvaluationInputComponents:TVkUInt32;
      maxTessellationEvaluationOutputComponents:TVkUInt32;
      maxGeometryShaderInvocations:TVkUInt32;
      maxGeometryInputComponents:TVkUInt32;
      maxGeometryOutputComponents:TVkUInt32;
      maxGeometryOutputVertices:TVkUInt32;
      maxGeometryTotalOutputComponents:TVkUInt32;
      maxFragmentInputComponents:TVkUInt32;
      maxFragmentOutputAttachments:TVkUInt32;
      maxFragmentDualSrcAttachments:TVkUInt32;
      maxFragmentCombinedOutputResources:TVkUInt32;
      maxComputeSharedMemorySize:TVkUInt32;
      maxComputeWorkGroupCount:array[0..2] of TVkUInt32;
      maxComputeWorkGroupInvocations:TVkUInt32;
      maxComputeWorkGroupSize:array[0..2] of TVkUInt32;
      subPixelPrecisionBits:TVkUInt32;
      subTexelPrecisionBits:TVkUInt32;
      mipmapPrecisionBits:TVkUInt32;
      maxDrawIndexedIndexValue:TVkUInt32;
      maxDrawIndirectCount:TVkUInt32;
      maxSamplerLodBias:TVkFloat;
      maxSamplerAnisotropy:TVkFloat;
      maxViewports:TVkUInt32;
      maxViewportDimensions:array[0..1] of TVkUInt32;
      viewportBoundsRange:array[0..1] of TVkFloat;
      viewportSubPixelBits:TVkUInt32;
      minMemoryMapAlignment:TVkPtrInt;
      minTexelBufferOffsetAlignment:TVkDeviceSize;
      minUniformBufferOffsetAlignment:TVkDeviceSize;
      minStorageBufferOffsetAlignment:TVkDeviceSize;
      minTexelOffset:TVkInt32;
      maxTexelOffset:TVkUInt32;
      minTexelGatherOffset:TVkInt32;
      maxTexelGatherOffset:TVkUInt32;
      minInterpolationOffset:TVkFloat;
      maxInterpolationOffset:TVkFloat;
      subPixelInterpolationOffsetBits:TVkUInt32;
      maxFramebufferWidth:TVkUInt32;
      maxFramebufferHeight:TVkUInt32;
      maxFramebufferLayers:TVkUInt32;
      framebufferColorSampleCounts:TVkSampleCountFlags;
      framebufferDepthSampleCounts:TVkSampleCountFlags;
      framebufferStencilSampleCounts:TVkSampleCountFlags;
      framebufferNoAttachmentsSampleCounts:TVkSampleCountFlags;
      maxColorAttachments:TVkUInt32;
      sampledImageColorSampleCounts:TVkSampleCountFlags;
      sampledImageIntegerSampleCounts:TVkSampleCountFlags;
      sampledImageDepthSampleCounts:TVkSampleCountFlags;
      sampledImageStencilSampleCounts:TVkSampleCountFlags;
      storageImageSampleCounts:TVkSampleCountFlags;
      maxSampleMaskWords:TVkUInt32;
      timestampComputeAndGraphics:TVkBool32;
      timestampPeriod:TVkFloat;
      maxClipDistances:TVkUInt32;
      maxCullDistances:TVkUInt32;
      maxCombinedClipAndCullDistances:TVkUInt32;
      discreteQueuePriorities:TVkUInt32;
      pointSizeRange:array[0..1] of TVkFloat;
      lineWidthRange:array[0..1] of TVkFloat;
      pointSizeGranularity:TVkFloat;
      lineWidthGranularity:TVkFloat;
      strictLines:TVkBool32;
      standardSampleLocations:TVkBool32;
      optimalBufferCopyOffsetAlignment:TVkDeviceSize;
      optimalBufferCopyRowPitchAlignment:TVkDeviceSize;
      nonCoherentAtomSize:TVkDeviceSize;
     end;

     PPVkPhysicalDeviceProperties=^PVkPhysicalDeviceProperties;
     PVkPhysicalDeviceProperties=^TVkPhysicalDeviceProperties;
     TVkPhysicalDeviceProperties=record
      apiVersion:TVkUInt32;
      driverVersion:TVkUInt32;
      vendorID:TVkUInt32;
      deviceID:TVkUInt32;
      deviceType:TVkPhysicalDeviceType;
      deviceName:array[0..VK_MAX_PHYSICAL_DEVICE_NAME_SIZE-1] of TVkChar;
      pipelineCacheUUID:array[0..VK_UUID_SIZE-1] of TVkUInt8;
      limits:TVkPhysicalDeviceLimits;
      sparseProperties:TVkPhysicalDeviceSparseProperties;
     end;

     PPVkSemaphoreCreateInfo=^PVkSemaphoreCreateInfo;
     PVkSemaphoreCreateInfo=^TVkSemaphoreCreateInfo;
     TVkSemaphoreCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkSemaphoreCreateFlags;
     end;

     PPVkQueryPoolCreateInfo=^PVkQueryPoolCreateInfo;
     PVkQueryPoolCreateInfo=^TVkQueryPoolCreateInfo;
     TVkQueryPoolCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkQueryPoolCreateFlags;
      queryType:TVkQueryType;
      queryCount:TVkUInt32;
      pipelineStatistics:TVkQueryPipelineStatisticFlags;
     end;

     PPVkFramebufferCreateInfo=^PVkFramebufferCreateInfo;
     PVkFramebufferCreateInfo=^TVkFramebufferCreateInfo;
     TVkFramebufferCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkFramebufferCreateFlags;
      renderPass:TVkRenderPass;
      attachmentCount:TVkUInt32;
      pAttachments:PVkImageView;
      width:TVkUInt32;
      height:TVkUInt32;
      layers:TVkUInt32;
     end;

     PPVkDrawIndirectCommand=^PVkDrawIndirectCommand;
     PVkDrawIndirectCommand=^TVkDrawIndirectCommand;
     TVkDrawIndirectCommand=record
      vertexCount:TVkUInt32;
      instanceCount:TVkUInt32;
      firstVertex:TVkUInt32;
      firstInstance:TVkUInt32;
     end;

     PPVkDrawIndexedIndirectCommand=^PVkDrawIndexedIndirectCommand;
     PVkDrawIndexedIndirectCommand=^TVkDrawIndexedIndirectCommand;
     TVkDrawIndexedIndirectCommand=record
      indexCount:TVkUInt32;
      instanceCount:TVkUInt32;
      firstIndex:TVkUInt32;
      vertexOffset:TVkInt32;
      firstInstance:TVkUInt32;
     end;

     PPVkDispatchIndirectCommand=^PVkDispatchIndirectCommand;
     PVkDispatchIndirectCommand=^TVkDispatchIndirectCommand;
     TVkDispatchIndirectCommand=record
      x:TVkUInt32;
      y:TVkUInt32;
      z:TVkUInt32;
     end;

     PPVkSubmitInfo=^PVkSubmitInfo;
     PVkSubmitInfo=^TVkSubmitInfo;
     TVkSubmitInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      waitSemaphoreCount:TVkUInt32;
      pWaitSemaphores:PVkSemaphore;
      pWaitDstStageMask:PVkPipelineStageFlags;
      commandBufferCount:TVkUInt32;
      pCommandBuffers:PVkCommandBuffer;
      signalSemaphoreCount:TVkUInt32;
      pSignalSemaphores:PVkSemaphore;
     end;

     PPVkDisplayPropertiesKHR=^PVkDisplayPropertiesKHR;
     PVkDisplayPropertiesKHR=^TVkDisplayPropertiesKHR;
     TVkDisplayPropertiesKHR=record
      display:TVkDisplayKHR;
      displayName:PVkChar;
      physicalDimensions:TVkExtent2D;
      physicalResolution:TVkExtent2D;
      supportedTransforms:TVkSurfaceTransformFlagsKHR;
      planeReorderPossible:TVkBool32;
      persistentContent:TVkBool32;
     end;

     PPVkDisplayPlanePropertiesKHR=^PVkDisplayPlanePropertiesKHR;
     PVkDisplayPlanePropertiesKHR=^TVkDisplayPlanePropertiesKHR;
     TVkDisplayPlanePropertiesKHR=record
      currentDisplay:TVkDisplayKHR;
      currentStackIndex:TVkUInt32;
     end;

     PPVkDisplayModeParametersKHR=^PVkDisplayModeParametersKHR;
     PVkDisplayModeParametersKHR=^TVkDisplayModeParametersKHR;
     TVkDisplayModeParametersKHR=record
      visibleRegion:TVkExtent2D;
      refreshRate:TVkUInt32;
     end;

     PPVkDisplayModePropertiesKHR=^PVkDisplayModePropertiesKHR;
     PVkDisplayModePropertiesKHR=^TVkDisplayModePropertiesKHR;
     TVkDisplayModePropertiesKHR=record
      displayMode:TVkDisplayModeKHR;
      parameters:TVkDisplayModeParametersKHR;
     end;

     PPVkDisplayModeCreateInfoKHR=^PVkDisplayModeCreateInfoKHR;
     PVkDisplayModeCreateInfoKHR=^TVkDisplayModeCreateInfoKHR;
     TVkDisplayModeCreateInfoKHR=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkDisplayModeCreateFlagsKHR;
      parameters:TVkDisplayModeParametersKHR;
     end;

     PPVkDisplayPlaneCapabilitiesKHR=^PVkDisplayPlaneCapabilitiesKHR;
     PVkDisplayPlaneCapabilitiesKHR=^TVkDisplayPlaneCapabilitiesKHR;
     TVkDisplayPlaneCapabilitiesKHR=record
      supportedAlpha:TVkDisplayPlaneAlphaFlagsKHR;
      minSrcPosition:TVkOffset2D;
      maxSrcPosition:TVkOffset2D;
      minSrcExtent:TVkExtent2D;
      maxSrcExtent:TVkExtent2D;
      minDstPosition:TVkOffset2D;
      maxDstPosition:TVkOffset2D;
      minDstExtent:TVkExtent2D;
      maxDstExtent:TVkExtent2D;
     end;

     PPVkDisplaySurfaceCreateInfoKHR=^PVkDisplaySurfaceCreateInfoKHR;
     PVkDisplaySurfaceCreateInfoKHR=^TVkDisplaySurfaceCreateInfoKHR;
     TVkDisplaySurfaceCreateInfoKHR=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkDisplaySurfaceCreateFlagsKHR;
      displayMode:TVkDisplayModeKHR;
      planeIndex:TVkUInt32;
      planeStackIndex:TVkUInt32;
      transform:TVkSurfaceTransformFlagBitsKHR;
      globalAlpha:TVkFloat;
      alphaMode:TVkDisplayPlaneAlphaFlagBitsKHR;
      imageExtent:TVkExtent2D;
     end;

     PPVkDisplayPresentInfoKHR=^PVkDisplayPresentInfoKHR;
     PVkDisplayPresentInfoKHR=^TVkDisplayPresentInfoKHR;
     TVkDisplayPresentInfoKHR=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      srcRect:TVkRect2D;
      dstRect:TVkRect2D;
      persistent:TVkBool32;
     end;

     PPVkSurfaceCapabilitiesKHR=^PVkSurfaceCapabilitiesKHR;
     PVkSurfaceCapabilitiesKHR=^TVkSurfaceCapabilitiesKHR;
     TVkSurfaceCapabilitiesKHR=record
      minImageCount:TVkUInt32;
      maxImageCount:TVkUInt32;
      currentExtent:TVkExtent2D;
      minImageExtent:TVkExtent2D;
      maxImageExtent:TVkExtent2D;
      maxImageArrayLayers:TVkUInt32;
      supportedTransforms:TVkSurfaceTransformFlagsKHR;
      currentTransform:TVkSurfaceTransformFlagBitsKHR;
      supportedCompositeAlpha:TVkCompositeAlphaFlagsKHR;
      supportedUsageFlags:TVkImageUsageFlags;
     end;

{$ifdef Android}
     PPVkAndroidSurfaceCreateInfoKHR=^PVkAndroidSurfaceCreateInfoKHR;
     PVkAndroidSurfaceCreateInfoKHR=^TVkAndroidSurfaceCreateInfoKHR;
     TVkAndroidSurfaceCreateInfoKHR=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkAndroidSurfaceCreateFlagsKHR;
      window:PANativeWindow;
     end;
{$endif}

{$ifdef Mir}
     PPVkMirSurfaceCreateInfoKHR=^PVkMirSurfaceCreateInfoKHR;
     PVkMirSurfaceCreateInfoKHR=^TVkMirSurfaceCreateInfoKHR;
     TVkMirSurfaceCreateInfoKHR=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkMirSurfaceCreateFlagsKHR;
      connection:PMirConnection;
      mirSurface:PMirSurface;
     end;
{$endif}

{$ifdef Wayland}
     PPVkWaylandSurfaceCreateInfoKHR=^PVkWaylandSurfaceCreateInfoKHR;
     PVkWaylandSurfaceCreateInfoKHR=^TVkWaylandSurfaceCreateInfoKHR;
     TVkWaylandSurfaceCreateInfoKHR=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkWaylandSurfaceCreateFlagsKHR;
      display:Pwl_display;
      surface:Pwl_surface;
     end;
{$endif}

{$ifdef Windows}
     PPVkWin32SurfaceCreateInfoKHR=^PVkWin32SurfaceCreateInfoKHR;
     PVkWin32SurfaceCreateInfoKHR=^TVkWin32SurfaceCreateInfoKHR;
     TVkWin32SurfaceCreateInfoKHR=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkWin32SurfaceCreateFlagsKHR;
      hinstance_:TVkHINSTANCE;
      hwnd_:TVkHWND;
     end;
{$endif}

{$ifdef X11}
     PPVkXlibSurfaceCreateInfoKHR=^PVkXlibSurfaceCreateInfoKHR;
     PVkXlibSurfaceCreateInfoKHR=^TVkXlibSurfaceCreateInfoKHR;
     TVkXlibSurfaceCreateInfoKHR=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkXlibSurfaceCreateFlagsKHR;
      dpy:PDisplay;
      window:TWindow;
     end;
{$endif}

{$ifdef XCB}
     PPVkXcbSurfaceCreateInfoKHR=^PVkXcbSurfaceCreateInfoKHR;
     PVkXcbSurfaceCreateInfoKHR=^TVkXcbSurfaceCreateInfoKHR;
     TVkXcbSurfaceCreateInfoKHR=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkXcbSurfaceCreateFlagsKHR;
      connection:Pxcb_connection;
      window:Txcb_window;
     end;
{$endif}

     PPVkSurfaceFormatKHR=^PVkSurfaceFormatKHR;
     PVkSurfaceFormatKHR=^TVkSurfaceFormatKHR;
     TVkSurfaceFormatKHR=record
      format:TVkFormat;
      colorSpace:TVkColorSpaceKHR;
     end;

     PPVkSwapchainCreateInfoKHR=^PVkSwapchainCreateInfoKHR;
     PVkSwapchainCreateInfoKHR=^TVkSwapchainCreateInfoKHR;
     TVkSwapchainCreateInfoKHR=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkSwapchainCreateFlagsKHR;
      surface:TVkSurfaceKHR;
      minImageCount:TVkUInt32;
      imageFormat:TVkFormat;
      imageColorSpace:TVkColorSpaceKHR;
      imageExtent:TVkExtent2D;
      imageArrayLayers:TVkUInt32;
      imageUsage:TVkImageUsageFlags;
      imageSharingMode:TVkSharingMode;
      queueFamilyIndexCount:TVkUInt32;
      pQueueFamilyIndices:PVkUInt32;
      preTransform:TVkSurfaceTransformFlagBitsKHR;
      compositeAlpha:TVkCompositeAlphaFlagBitsKHR;
      presentMode:TVkPresentModeKHR;
      clipped:TVkBool32;
      oldSwapchain:TVkSwapchainKHR;
     end;

     PPVkPresentInfoKHR=^PVkPresentInfoKHR;
     PVkPresentInfoKHR=^TVkPresentInfoKHR;
     TVkPresentInfoKHR=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      waitSemaphoreCount:TVkUInt32;
      pWaitSemaphores:PVkSemaphore;
      swapchainCount:TVkUInt32;
      pSwapchains:PVkSwapchainKHR;
      pImageIndices:PVkUInt32;
      pResults:PVkResult;
     end;

     PPVkDebugReportCallbackCreateInfoEXT=^PVkDebugReportCallbackCreateInfoEXT;
     PVkDebugReportCallbackCreateInfoEXT=^TVkDebugReportCallbackCreateInfoEXT;
     TVkDebugReportCallbackCreateInfoEXT=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkDebugReportFlagsEXT;
      pfnCallback:TPFN_vkDebugReportCallbackEXT;
      pUserData:TVkPointer;
     end;

     TvkCreateInstance=function(const pCreateInfo:PVkInstanceCreateInfo;const pAllocator:PVkAllocationCallbacks;pInstance:PVkInstance):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroyInstance=procedure(instance:TVkInstance;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkEnumeratePhysicalDevices=function(instance:TVkInstance;pPhysicalDeviceCount:PVkUInt32;pPhysicalDevices:PVkPhysicalDevice):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetDeviceProcAddr=function(device:TVkDevice;const pName:PVkChar):TPFN_vkVoidFunction; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetInstanceProcAddr=function(instance:TVkInstance;const pName:PVkChar):TPFN_vkVoidFunction; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceProperties=procedure(physicalDevice:TVkPhysicalDevice;pProperties:PVkPhysicalDeviceProperties); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceQueueFamilyProperties=procedure(physicalDevice:TVkPhysicalDevice;pQueueFamilyPropertyCount:PVkUInt32;pQueueFamilyProperties:PVkQueueFamilyProperties); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceMemoryProperties=procedure(physicalDevice:TVkPhysicalDevice;pMemoryProperties:PVkPhysicalDeviceMemoryProperties); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceFeatures=procedure(physicalDevice:TVkPhysicalDevice;pFeatures:PVkPhysicalDeviceFeatures); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceFormatProperties=procedure(physicalDevice:TVkPhysicalDevice;format:TVkFormat;pFormatProperties:PVkFormatProperties); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceImageFormatProperties=function(physicalDevice:TVkPhysicalDevice;format:TVkFormat;type_:TVkImageType;tiling:TVkImageTiling;usage:TVkImageUsageFlags;flags:TVkImageCreateFlags;pImageFormatProperties:PVkImageFormatProperties):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateDevice=function(physicalDevice:TVkPhysicalDevice;const pCreateInfo:PVkDeviceCreateInfo;const pAllocator:PVkAllocationCallbacks;pDevice:PVkDevice):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroyDevice=procedure(device:TVkDevice;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkEnumerateInstanceLayerProperties=function(pPropertyCount:PVkUInt32;pProperties:PVkLayerProperties):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkEnumerateInstanceExtensionProperties=function(const pLayerName:PVkChar;pPropertyCount:PVkUInt32;pProperties:PVkExtensionProperties):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkEnumerateDeviceLayerProperties=function(physicalDevice:TVkPhysicalDevice;pPropertyCount:PVkUInt32;pProperties:PVkLayerProperties):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkEnumerateDeviceExtensionProperties=function(physicalDevice:TVkPhysicalDevice;const pLayerName:PVkChar;pPropertyCount:PVkUInt32;pProperties:PVkExtensionProperties):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetDeviceQueue=procedure(device:TVkDevice;queueFamilyIndex:TVkUInt32;queueIndex:TVkUInt32;pQueue:PVkQueue); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkQueueSubmit=function(queue:TVkQueue;submitCount:TVkUInt32;const pSubmits:PVkSubmitInfo;fence:TVkFence):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkQueueWaitIdle=function(queue:TVkQueue):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDeviceWaitIdle=function(device:TVkDevice):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkAllocateMemory=function(device:TVkDevice;const pAllocateInfo:PVkMemoryAllocateInfo;const pAllocator:PVkAllocationCallbacks;pMemory:PVkDeviceMemory):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkFreeMemory=procedure(device:TVkDevice;memory:TVkDeviceMemory;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkMapMemory=function(device:TVkDevice;memory:TVkDeviceMemory;offset:TVkDeviceSize;size:TVkDeviceSize;flags:TVkMemoryMapFlags;ppData:TVkPointer):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkUnmapMemory=procedure(device:TVkDevice;memory:TVkDeviceMemory); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkFlushMappedMemoryRanges=function(device:TVkDevice;memoryRangeCount:TVkUInt32;const pMemoryRanges:PVkMappedMemoryRange):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkInvalidateMappedMemoryRanges=function(device:TVkDevice;memoryRangeCount:TVkUInt32;const pMemoryRanges:PVkMappedMemoryRange):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetDeviceMemoryCommitment=procedure(device:TVkDevice;memory:TVkDeviceMemory;pCommittedMemoryInBytes:PVkDeviceSize); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetBufferMemoryRequirements=procedure(device:TVkDevice;buffer:TVkBuffer;pMemoryRequirements:PVkMemoryRequirements); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkBindBufferMemory=function(device:TVkDevice;buffer:TVkBuffer;memory:TVkDeviceMemory;memoryOffset:TVkDeviceSize):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetImageMemoryRequirements=procedure(device:TVkDevice;image:TVkImage;pMemoryRequirements:PVkMemoryRequirements); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkBindImageMemory=function(device:TVkDevice;image:TVkImage;memory:TVkDeviceMemory;memoryOffset:TVkDeviceSize):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetImageSparseMemoryRequirements=procedure(device:TVkDevice;image:TVkImage;pSparseMemoryRequirementCount:PVkUInt32;pSparseMemoryRequirements:PVkSparseImageMemoryRequirements); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceSparseImageFormatProperties=procedure(physicalDevice:TVkPhysicalDevice;format:TVkFormat;type_:TVkImageType;samples:TVkSampleCountFlagBits;usage:TVkImageUsageFlags;tiling:TVkImageTiling;pPropertyCount:PVkUInt32;pProperties:PVkSparseImageFormatProperties); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkQueueBindSparse=function(queue:TVkQueue;bindInfoCount:TVkUInt32;const pBindInfo:PVkBindSparseInfo;fence:TVkFence):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateFence=function(device:TVkDevice;const pCreateInfo:PVkFenceCreateInfo;const pAllocator:PVkAllocationCallbacks;pFence:PVkFence):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroyFence=procedure(device:TVkDevice;fence:TVkFence;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkResetFences=function(device:TVkDevice;fenceCount:TVkUInt32;const pFences:PVkFence):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetFenceStatus=function(device:TVkDevice;fence:TVkFence):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkWaitForFences=function(device:TVkDevice;fenceCount:TVkUInt32;const pFences:PVkFence;waitAll:TVkBool32;timeout:TVkUInt64):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateSemaphore=function(device:TVkDevice;const pCreateInfo:PVkSemaphoreCreateInfo;const pAllocator:PVkAllocationCallbacks;pSemaphore:PVkSemaphore):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroySemaphore=procedure(device:TVkDevice;semaphore:TVkSemaphore;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateEvent=function(device:TVkDevice;const pCreateInfo:PVkEventCreateInfo;const pAllocator:PVkAllocationCallbacks;pEvent:PVkEvent):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroyEvent=procedure(device:TVkDevice;event:TVkEvent;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetEventStatus=function(device:TVkDevice;event:TVkEvent):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkSetEvent=function(device:TVkDevice;event:TVkEvent):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkResetEvent=function(device:TVkDevice;event:TVkEvent):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateQueryPool=function(device:TVkDevice;const pCreateInfo:PVkQueryPoolCreateInfo;const pAllocator:PVkAllocationCallbacks;pQueryPool:PVkQueryPool):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroyQueryPool=procedure(device:TVkDevice;queryPool:TVkQueryPool;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetQueryPoolResults=function(device:TVkDevice;queryPool:TVkQueryPool;firstQuery:TVkUInt32;queryCount:TVkUInt32;dataSize:TVkPtrInt;pData:TVkPointer;stride:TVkDeviceSize;flags:TVkQueryResultFlags):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateBuffer=function(device:TVkDevice;const pCreateInfo:PVkBufferCreateInfo;const pAllocator:PVkAllocationCallbacks;pBuffer:PVkBuffer):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroyBuffer=procedure(device:TVkDevice;buffer:TVkBuffer;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateBufferView=function(device:TVkDevice;const pCreateInfo:PVkBufferViewCreateInfo;const pAllocator:PVkAllocationCallbacks;pView:PVkBufferView):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroyBufferView=procedure(device:TVkDevice;bufferView:TVkBufferView;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateImage=function(device:TVkDevice;const pCreateInfo:PVkImageCreateInfo;const pAllocator:PVkAllocationCallbacks;pImage:PVkImage):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroyImage=procedure(device:TVkDevice;image:TVkImage;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetImageSubresourceLayout=procedure(device:TVkDevice;image:TVkImage;const pSubresource:PVkImageSubresource;pLayout:PVkSubresourceLayout); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateImageView=function(device:TVkDevice;const pCreateInfo:PVkImageViewCreateInfo;const pAllocator:PVkAllocationCallbacks;pView:PVkImageView):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroyImageView=procedure(device:TVkDevice;imageView:TVkImageView;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateShaderModule=function(device:TVkDevice;const pCreateInfo:PVkShaderModuleCreateInfo;const pAllocator:PVkAllocationCallbacks;pShaderModule:PVkShaderModule):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroyShaderModule=procedure(device:TVkDevice;shaderModule:TVkShaderModule;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreatePipelineCache=function(device:TVkDevice;const pCreateInfo:PVkPipelineCacheCreateInfo;const pAllocator:PVkAllocationCallbacks;pPipelineCache:PVkPipelineCache):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroyPipelineCache=procedure(device:TVkDevice;pipelineCache:TVkPipelineCache;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetPipelineCacheData=function(device:TVkDevice;pipelineCache:TVkPipelineCache;pDataSize:PVkPtrInt;pData:TVkPointer):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkMergePipelineCaches=function(device:TVkDevice;dstCache:TVkPipelineCache;srcCacheCount:TVkUInt32;const pSrcCaches:PVkPipelineCache):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateGraphicsPipelines=function(device:TVkDevice;pipelineCache:TVkPipelineCache;createInfoCount:TVkUInt32;const pCreateInfos:PVkGraphicsPipelineCreateInfo;const pAllocator:PVkAllocationCallbacks;pPipelines:PVkPipeline):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateComputePipelines=function(device:TVkDevice;pipelineCache:TVkPipelineCache;createInfoCount:TVkUInt32;const pCreateInfos:PVkComputePipelineCreateInfo;const pAllocator:PVkAllocationCallbacks;pPipelines:PVkPipeline):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroyPipeline=procedure(device:TVkDevice;pipeline:TVkPipeline;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreatePipelineLayout=function(device:TVkDevice;const pCreateInfo:PVkPipelineLayoutCreateInfo;const pAllocator:PVkAllocationCallbacks;pPipelineLayout:PVkPipelineLayout):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroyPipelineLayout=procedure(device:TVkDevice;pipelineLayout:TVkPipelineLayout;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateSampler=function(device:TVkDevice;const pCreateInfo:PVkSamplerCreateInfo;const pAllocator:PVkAllocationCallbacks;pSampler:PVkSampler):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroySampler=procedure(device:TVkDevice;sampler:TVkSampler;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateDescriptorSetLayout=function(device:TVkDevice;const pCreateInfo:PVkDescriptorSetLayoutCreateInfo;const pAllocator:PVkAllocationCallbacks;pSetLayout:PVkDescriptorSetLayout):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroyDescriptorSetLayout=procedure(device:TVkDevice;descriptorSetLayout:TVkDescriptorSetLayout;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateDescriptorPool=function(device:TVkDevice;const pCreateInfo:PVkDescriptorPoolCreateInfo;const pAllocator:PVkAllocationCallbacks;pDescriptorPool:PVkDescriptorPool):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroyDescriptorPool=procedure(device:TVkDevice;descriptorPool:TVkDescriptorPool;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkResetDescriptorPool=function(device:TVkDevice;descriptorPool:TVkDescriptorPool;flags:TVkDescriptorPoolResetFlags):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkAllocateDescriptorSets=function(device:TVkDevice;const pAllocateInfo:PVkDescriptorSetAllocateInfo;pDescriptorSets:PVkDescriptorSet):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkFreeDescriptorSets=function(device:TVkDevice;descriptorPool:TVkDescriptorPool;descriptorSetCount:TVkUInt32;const pDescriptorSets:PVkDescriptorSet):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkUpdateDescriptorSets=procedure(device:TVkDevice;descriptorWriteCount:TVkUInt32;const pDescriptorWrites:PVkWriteDescriptorSet;descriptorCopyCount:TVkUInt32;const pDescriptorCopies:PVkCopyDescriptorSet); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateFramebuffer=function(device:TVkDevice;const pCreateInfo:PVkFramebufferCreateInfo;const pAllocator:PVkAllocationCallbacks;pFramebuffer:PVkFramebuffer):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroyFramebuffer=procedure(device:TVkDevice;framebuffer:TVkFramebuffer;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateRenderPass=function(device:TVkDevice;const pCreateInfo:PVkRenderPassCreateInfo;const pAllocator:PVkAllocationCallbacks;pRenderPass:PVkRenderPass):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroyRenderPass=procedure(device:TVkDevice;renderPass:TVkRenderPass;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetRenderAreaGranularity=procedure(device:TVkDevice;renderPass:TVkRenderPass;pGranularity:PVkExtent2D); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateCommandPool=function(device:TVkDevice;const pCreateInfo:PVkCommandPoolCreateInfo;const pAllocator:PVkAllocationCallbacks;pCommandPool:PVkCommandPool):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroyCommandPool=procedure(device:TVkDevice;commandPool:TVkCommandPool;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkResetCommandPool=function(device:TVkDevice;commandPool:TVkCommandPool;flags:TVkCommandPoolResetFlags):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkAllocateCommandBuffers=function(device:TVkDevice;const pAllocateInfo:PVkCommandBufferAllocateInfo;pCommandBuffers:PVkCommandBuffer):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkFreeCommandBuffers=procedure(device:TVkDevice;commandPool:TVkCommandPool;commandBufferCount:TVkUInt32;const pCommandBuffers:PVkCommandBuffer); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkBeginCommandBuffer=function(commandBuffer:TVkCommandBuffer;const pBeginInfo:PVkCommandBufferBeginInfo):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkEndCommandBuffer=function(commandBuffer:TVkCommandBuffer):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkResetCommandBuffer=function(commandBuffer:TVkCommandBuffer;flags:TVkCommandBufferResetFlags):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdBindPipeline=procedure(commandBuffer:TVkCommandBuffer;pipelineBindPoint:TVkPipelineBindPoint;pipeline:TVkPipeline); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetViewport=procedure(commandBuffer:TVkCommandBuffer;firstViewport:TVkUInt32;viewportCount:TVkUInt32;const pViewports:PVkViewport); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetScissor=procedure(commandBuffer:TVkCommandBuffer;firstScissor:TVkUInt32;scissorCount:TVkUInt32;const pScissors:PVkRect2D); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetLineWidth=procedure(commandBuffer:TVkCommandBuffer;lineWidth:TVkFloat); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetDepthBias=procedure(commandBuffer:TVkCommandBuffer;depthBiasConstantFactor:TVkFloat;depthBiasClamp:TVkFloat;depthBiasSlopeFactor:TVkFloat); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetBlendConstants=procedure(commandBuffer:TVkCommandBuffer;const blendConstants:TVkFloat); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetDepthBounds=procedure(commandBuffer:TVkCommandBuffer;minDepthBounds:TVkFloat;maxDepthBounds:TVkFloat); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetStencilCompareMask=procedure(commandBuffer:TVkCommandBuffer;faceMask:TVkStencilFaceFlags;compareMask:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetStencilWriteMask=procedure(commandBuffer:TVkCommandBuffer;faceMask:TVkStencilFaceFlags;writeMask:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetStencilReference=procedure(commandBuffer:TVkCommandBuffer;faceMask:TVkStencilFaceFlags;reference:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdBindDescriptorSets=procedure(commandBuffer:TVkCommandBuffer;pipelineBindPoint:TVkPipelineBindPoint;layout:TVkPipelineLayout;firstSet:TVkUInt32;descriptorSetCount:TVkUInt32;const pDescriptorSets:PVkDescriptorSet;dynamicOffsetCount:TVkUInt32;const pDynamicOffsets:PVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdBindIndexBuffer=procedure(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;indexType:TVkIndexType); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdBindVertexBuffers=procedure(commandBuffer:TVkCommandBuffer;firstBinding:TVkUInt32;bindingCount:TVkUInt32;const pBuffers:PVkBuffer;const pOffsets:PVkDeviceSize); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdDraw=procedure(commandBuffer:TVkCommandBuffer;vertexCount:TVkUInt32;instanceCount:TVkUInt32;firstVertex:TVkUInt32;firstInstance:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdDrawIndexed=procedure(commandBuffer:TVkCommandBuffer;indexCount:TVkUInt32;instanceCount:TVkUInt32;firstIndex:TVkUInt32;vertexOffset:TVkInt32;firstInstance:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdDrawIndirect=procedure(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;drawCount:TVkUInt32;stride:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdDrawIndexedIndirect=procedure(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;drawCount:TVkUInt32;stride:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdDispatch=procedure(commandBuffer:TVkCommandBuffer;x:TVkUInt32;y:TVkUInt32;z:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdDispatchIndirect=procedure(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdCopyBuffer=procedure(commandBuffer:TVkCommandBuffer;srcBuffer:TVkBuffer;dstBuffer:TVkBuffer;regionCount:TVkUInt32;const pRegions:PVkBufferCopy); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdCopyImage=procedure(commandBuffer:TVkCommandBuffer;srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkImageCopy); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdBlitImage=procedure(commandBuffer:TVkCommandBuffer;srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkImageBlit;filter:TVkFilter); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdCopyBufferToImage=procedure(commandBuffer:TVkCommandBuffer;srcBuffer:TVkBuffer;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkBufferImageCopy); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdCopyImageToBuffer=procedure(commandBuffer:TVkCommandBuffer;srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstBuffer:TVkBuffer;regionCount:TVkUInt32;const pRegions:PVkBufferImageCopy); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdUpdateBuffer=procedure(commandBuffer:TVkCommandBuffer;dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;dataSize:TVkDeviceSize;const pData:PVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdFillBuffer=procedure(commandBuffer:TVkCommandBuffer;dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;size:TVkDeviceSize;data:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdClearColorImage=procedure(commandBuffer:TVkCommandBuffer;image:TVkImage;imageLayout:TVkImageLayout;const pColor:PVkClearColorValue;rangeCount:TVkUInt32;const pRanges:PVkImageSubresourceRange); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdClearDepthStencilImage=procedure(commandBuffer:TVkCommandBuffer;image:TVkImage;imageLayout:TVkImageLayout;const pDepthStencil:PVkClearDepthStencilValue;rangeCount:TVkUInt32;const pRanges:PVkImageSubresourceRange); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdClearAttachments=procedure(commandBuffer:TVkCommandBuffer;attachmentCount:TVkUInt32;const pAttachments:PVkClearAttachment;rectCount:TVkUInt32;const pRects:PVkClearRect); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdResolveImage=procedure(commandBuffer:TVkCommandBuffer;srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkImageResolve); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdSetEvent=procedure(commandBuffer:TVkCommandBuffer;event:TVkEvent;stageMask:TVkPipelineStageFlags); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdResetEvent=procedure(commandBuffer:TVkCommandBuffer;event:TVkEvent;stageMask:TVkPipelineStageFlags); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdWaitEvents=procedure(commandBuffer:TVkCommandBuffer;eventCount:TVkUInt32;const pEvents:PVkEvent;srcStageMask:TVkPipelineStageFlags;dstStageMask:TVkPipelineStageFlags;memoryBarrierCount:TVkUInt32;const pMemoryBarriers:PVkMemoryBarrier;bufferMemoryBarrierCount:TVkUInt32;const pBufferMemoryBarriers:PVkBufferMemoryBarrier;imageMemoryBarrierCount:TVkUInt32;const pImageMemoryBarriers:PVkImageMemoryBarrier); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdPipelineBarrier=procedure(commandBuffer:TVkCommandBuffer;srcStageMask:TVkPipelineStageFlags;dstStageMask:TVkPipelineStageFlags;dependencyFlags:TVkDependencyFlags;memoryBarrierCount:TVkUInt32;const pMemoryBarriers:PVkMemoryBarrier;bufferMemoryBarrierCount:TVkUInt32;const pBufferMemoryBarriers:PVkBufferMemoryBarrier;imageMemoryBarrierCount:TVkUInt32;const pImageMemoryBarriers:PVkImageMemoryBarrier); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdBeginQuery=procedure(commandBuffer:TVkCommandBuffer;queryPool:TVkQueryPool;query:TVkUInt32;flags:TVkQueryControlFlags); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdEndQuery=procedure(commandBuffer:TVkCommandBuffer;queryPool:TVkQueryPool;query:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdResetQueryPool=procedure(commandBuffer:TVkCommandBuffer;queryPool:TVkQueryPool;firstQuery:TVkUInt32;queryCount:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdWriteTimestamp=procedure(commandBuffer:TVkCommandBuffer;pipelineStage:TVkPipelineStageFlagBits;queryPool:TVkQueryPool;query:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdCopyQueryPoolResults=procedure(commandBuffer:TVkCommandBuffer;queryPool:TVkQueryPool;firstQuery:TVkUInt32;queryCount:TVkUInt32;dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;stride:TVkDeviceSize;flags:TVkQueryResultFlags); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdPushConstants=procedure(commandBuffer:TVkCommandBuffer;layout:TVkPipelineLayout;stageFlags:TVkShaderStageFlags;offset:TVkUInt32;size:TVkUInt32;const pValues:TVkPointer); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdBeginRenderPass=procedure(commandBuffer:TVkCommandBuffer;const pRenderPassBegin:PVkRenderPassBeginInfo;contents:TVkSubpassContents); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdNextSubpass=procedure(commandBuffer:TVkCommandBuffer;contents:TVkSubpassContents); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdEndRenderPass=procedure(commandBuffer:TVkCommandBuffer); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCmdExecuteCommands=procedure(commandBuffer:TVkCommandBuffer;commandBufferCount:TVkUInt32;const pCommandBuffers:PVkCommandBuffer); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
{$ifdef Android}
     TvkCreateAndroidSurfaceKHR=function(instance:TVkInstance;const pCreateInfo:PVkAndroidSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
{$endif}
     TvkGetPhysicalDeviceDisplayPropertiesKHR=function(physicalDevice:TVkPhysicalDevice;pPropertyCount:PVkUInt32;pProperties:PVkDisplayPropertiesKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceDisplayPlanePropertiesKHR=function(physicalDevice:TVkPhysicalDevice;pPropertyCount:PVkUInt32;pProperties:PVkDisplayPlanePropertiesKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetDisplayPlaneSupportedDisplaysKHR=function(physicalDevice:TVkPhysicalDevice;planeIndex:TVkUInt32;pDisplayCount:PVkUInt32;pDisplays:PVkDisplayKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetDisplayModePropertiesKHR=function(physicalDevice:TVkPhysicalDevice;display:TVkDisplayKHR;pPropertyCount:PVkUInt32;pProperties:PVkDisplayModePropertiesKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateDisplayModeKHR=function(physicalDevice:TVkPhysicalDevice;display:TVkDisplayKHR;const pCreateInfo:PVkDisplayModeCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pMode:PVkDisplayModeKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetDisplayPlaneCapabilitiesKHR=function(physicalDevice:TVkPhysicalDevice;mode:TVkDisplayModeKHR;planeIndex:TVkUInt32;pCapabilities:PVkDisplayPlaneCapabilitiesKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateDisplayPlaneSurfaceKHR=function(instance:TVkInstance;const pCreateInfo:PVkDisplaySurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateSharedSwapchainsKHR=function(device:TVkDevice;swapchainCount:TVkUInt32;const pCreateInfos:PVkSwapchainCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSwapchains:PVkSwapchainKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
{$ifdef Mir}
     TvkCreateMirSurfaceKHR=function(instance:TVkInstance;const pCreateInfo:PVkMirSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
{$endif}
{$ifdef Mir}
     TvkGetPhysicalDeviceMirPresentationSupportKHR=function(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;connection:PMirConnection):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
{$endif}
     TvkDestroySurfaceKHR=procedure(instance:TVkInstance;surface:TVkSurfaceKHR;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceSurfaceSupportKHR=function(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;surface:TVkSurfaceKHR;pSupported:PVkBool32):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceSurfaceCapabilitiesKHR=function(physicalDevice:TVkPhysicalDevice;surface:TVkSurfaceKHR;pSurfaceCapabilities:PVkSurfaceCapabilitiesKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceSurfaceFormatsKHR=function(physicalDevice:TVkPhysicalDevice;surface:TVkSurfaceKHR;pSurfaceFormatCount:PVkUInt32;pSurfaceFormats:PVkSurfaceFormatKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceSurfacePresentModesKHR=function(physicalDevice:TVkPhysicalDevice;surface:TVkSurfaceKHR;pPresentModeCount:PVkUInt32;pPresentModes:PVkPresentModeKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkCreateSwapchainKHR=function(device:TVkDevice;const pCreateInfo:PVkSwapchainCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSwapchain:PVkSwapchainKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroySwapchainKHR=procedure(device:TVkDevice;swapchain:TVkSwapchainKHR;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetSwapchainImagesKHR=function(device:TVkDevice;swapchain:TVkSwapchainKHR;pSwapchainImageCount:PVkUInt32;pSwapchainImages:PVkImage):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkAcquireNextImageKHR=function(device:TVkDevice;swapchain:TVkSwapchainKHR;timeout:TVkUInt64;semaphore:TVkSemaphore;fence:TVkFence;pImageIndex:PVkUInt32):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkQueuePresentKHR=function(queue:TVkQueue;const pPresentInfo:PVkPresentInfoKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
{$ifdef Wayland}
     TvkCreateWaylandSurfaceKHR=function(instance:TVkInstance;const pCreateInfo:PVkWaylandSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
{$endif}
{$ifdef Wayland}
     TvkGetPhysicalDeviceWaylandPresentationSupportKHR=function(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;display:Pwl_display):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
{$endif}
     TvkCreateWin32SurfaceKHR=function(instance:TVkInstance;const pCreateInfo:PVkWin32SurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkGetPhysicalDeviceWin32PresentationSupportKHR=function(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
{$ifdef X11}
     TvkCreateXlibSurfaceKHR=function(instance:TVkInstance;const pCreateInfo:PVkXlibSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
{$endif}
{$ifdef X11}
     TvkGetPhysicalDeviceXlibPresentationSupportKHR=function(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;dpy:PDisplay;visualID:TVisualID):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
{$endif}
{$ifdef XCB}
     TvkCreateXcbSurfaceKHR=function(instance:TVkInstance;const pCreateInfo:PVkXcbSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
{$endif}
{$ifdef XCB}
     TvkGetPhysicalDeviceXcbPresentationSupportKHR=function(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;connection:Pxcb_connection;visual_id:Txcb_visualid):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
{$endif}
     TvkCreateDebugReportCallbackEXT=function(instance:TVkInstance;const pCreateInfo:PVkDebugReportCallbackCreateInfoEXT;const pAllocator:PVkAllocationCallbacks;pCallback:PVkDebugReportCallbackEXT):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDestroyDebugReportCallbackEXT=procedure(instance:TVkInstance;callback:TVkDebugReportCallbackEXT;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TvkDebugReportMessageEXT=procedure(instance:TVkInstance;flags:TVkDebugReportFlagsEXT;objectType:TVkDebugReportObjectTypeEXT;object_:TVkUInt64;location:TVkPtrInt;messageCode:TVkInt32;const pLayerPrefix:PVkChar;const pMessage:PVkChar); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}

     PPVulkanCommands=^PVulkanCommands;
     PVulkanCommands=^TVulkanCommands;
     TVulkanCommands=record
      CreateInstance:TvkCreateInstance;
      DestroyInstance:TvkDestroyInstance;
      EnumeratePhysicalDevices:TvkEnumeratePhysicalDevices;
      GetDeviceProcAddr:TvkGetDeviceProcAddr;
      GetInstanceProcAddr:TvkGetInstanceProcAddr;
      GetPhysicalDeviceProperties:TvkGetPhysicalDeviceProperties;
      GetPhysicalDeviceQueueFamilyProperties:TvkGetPhysicalDeviceQueueFamilyProperties;
      GetPhysicalDeviceMemoryProperties:TvkGetPhysicalDeviceMemoryProperties;
      GetPhysicalDeviceFeatures:TvkGetPhysicalDeviceFeatures;
      GetPhysicalDeviceFormatProperties:TvkGetPhysicalDeviceFormatProperties;
      GetPhysicalDeviceImageFormatProperties:TvkGetPhysicalDeviceImageFormatProperties;
      CreateDevice:TvkCreateDevice;
      DestroyDevice:TvkDestroyDevice;
      EnumerateInstanceLayerProperties:TvkEnumerateInstanceLayerProperties;
      EnumerateInstanceExtensionProperties:TvkEnumerateInstanceExtensionProperties;
      EnumerateDeviceLayerProperties:TvkEnumerateDeviceLayerProperties;
      EnumerateDeviceExtensionProperties:TvkEnumerateDeviceExtensionProperties;
      GetDeviceQueue:TvkGetDeviceQueue;
      QueueSubmit:TvkQueueSubmit;
      QueueWaitIdle:TvkQueueWaitIdle;
      DeviceWaitIdle:TvkDeviceWaitIdle;
      AllocateMemory:TvkAllocateMemory;
      FreeMemory:TvkFreeMemory;
      MapMemory:TvkMapMemory;
      UnmapMemory:TvkUnmapMemory;
      FlushMappedMemoryRanges:TvkFlushMappedMemoryRanges;
      InvalidateMappedMemoryRanges:TvkInvalidateMappedMemoryRanges;
      GetDeviceMemoryCommitment:TvkGetDeviceMemoryCommitment;
      GetBufferMemoryRequirements:TvkGetBufferMemoryRequirements;
      BindBufferMemory:TvkBindBufferMemory;
      GetImageMemoryRequirements:TvkGetImageMemoryRequirements;
      BindImageMemory:TvkBindImageMemory;
      GetImageSparseMemoryRequirements:TvkGetImageSparseMemoryRequirements;
      GetPhysicalDeviceSparseImageFormatProperties:TvkGetPhysicalDeviceSparseImageFormatProperties;
      QueueBindSparse:TvkQueueBindSparse;
      CreateFence:TvkCreateFence;
      DestroyFence:TvkDestroyFence;
      ResetFences:TvkResetFences;
      GetFenceStatus:TvkGetFenceStatus;
      WaitForFences:TvkWaitForFences;
      CreateSemaphore:TvkCreateSemaphore;
      DestroySemaphore:TvkDestroySemaphore;
      CreateEvent:TvkCreateEvent;
      DestroyEvent:TvkDestroyEvent;
      GetEventStatus:TvkGetEventStatus;
      SetEvent:TvkSetEvent;
      ResetEvent:TvkResetEvent;
      CreateQueryPool:TvkCreateQueryPool;
      DestroyQueryPool:TvkDestroyQueryPool;
      GetQueryPoolResults:TvkGetQueryPoolResults;
      CreateBuffer:TvkCreateBuffer;
      DestroyBuffer:TvkDestroyBuffer;
      CreateBufferView:TvkCreateBufferView;
      DestroyBufferView:TvkDestroyBufferView;
      CreateImage:TvkCreateImage;
      DestroyImage:TvkDestroyImage;
      GetImageSubresourceLayout:TvkGetImageSubresourceLayout;
      CreateImageView:TvkCreateImageView;
      DestroyImageView:TvkDestroyImageView;
      CreateShaderModule:TvkCreateShaderModule;
      DestroyShaderModule:TvkDestroyShaderModule;
      CreatePipelineCache:TvkCreatePipelineCache;
      DestroyPipelineCache:TvkDestroyPipelineCache;
      GetPipelineCacheData:TvkGetPipelineCacheData;
      MergePipelineCaches:TvkMergePipelineCaches;
      CreateGraphicsPipelines:TvkCreateGraphicsPipelines;
      CreateComputePipelines:TvkCreateComputePipelines;
      DestroyPipeline:TvkDestroyPipeline;
      CreatePipelineLayout:TvkCreatePipelineLayout;
      DestroyPipelineLayout:TvkDestroyPipelineLayout;
      CreateSampler:TvkCreateSampler;
      DestroySampler:TvkDestroySampler;
      CreateDescriptorSetLayout:TvkCreateDescriptorSetLayout;
      DestroyDescriptorSetLayout:TvkDestroyDescriptorSetLayout;
      CreateDescriptorPool:TvkCreateDescriptorPool;
      DestroyDescriptorPool:TvkDestroyDescriptorPool;
      ResetDescriptorPool:TvkResetDescriptorPool;
      AllocateDescriptorSets:TvkAllocateDescriptorSets;
      FreeDescriptorSets:TvkFreeDescriptorSets;
      UpdateDescriptorSets:TvkUpdateDescriptorSets;
      CreateFramebuffer:TvkCreateFramebuffer;
      DestroyFramebuffer:TvkDestroyFramebuffer;
      CreateRenderPass:TvkCreateRenderPass;
      DestroyRenderPass:TvkDestroyRenderPass;
      GetRenderAreaGranularity:TvkGetRenderAreaGranularity;
      CreateCommandPool:TvkCreateCommandPool;
      DestroyCommandPool:TvkDestroyCommandPool;
      ResetCommandPool:TvkResetCommandPool;
      AllocateCommandBuffers:TvkAllocateCommandBuffers;
      FreeCommandBuffers:TvkFreeCommandBuffers;
      BeginCommandBuffer:TvkBeginCommandBuffer;
      EndCommandBuffer:TvkEndCommandBuffer;
      ResetCommandBuffer:TvkResetCommandBuffer;
      CmdBindPipeline:TvkCmdBindPipeline;
      CmdSetViewport:TvkCmdSetViewport;
      CmdSetScissor:TvkCmdSetScissor;
      CmdSetLineWidth:TvkCmdSetLineWidth;
      CmdSetDepthBias:TvkCmdSetDepthBias;
      CmdSetBlendConstants:TvkCmdSetBlendConstants;
      CmdSetDepthBounds:TvkCmdSetDepthBounds;
      CmdSetStencilCompareMask:TvkCmdSetStencilCompareMask;
      CmdSetStencilWriteMask:TvkCmdSetStencilWriteMask;
      CmdSetStencilReference:TvkCmdSetStencilReference;
      CmdBindDescriptorSets:TvkCmdBindDescriptorSets;
      CmdBindIndexBuffer:TvkCmdBindIndexBuffer;
      CmdBindVertexBuffers:TvkCmdBindVertexBuffers;
      CmdDraw:TvkCmdDraw;
      CmdDrawIndexed:TvkCmdDrawIndexed;
      CmdDrawIndirect:TvkCmdDrawIndirect;
      CmdDrawIndexedIndirect:TvkCmdDrawIndexedIndirect;
      CmdDispatch:TvkCmdDispatch;
      CmdDispatchIndirect:TvkCmdDispatchIndirect;
      CmdCopyBuffer:TvkCmdCopyBuffer;
      CmdCopyImage:TvkCmdCopyImage;
      CmdBlitImage:TvkCmdBlitImage;
      CmdCopyBufferToImage:TvkCmdCopyBufferToImage;
      CmdCopyImageToBuffer:TvkCmdCopyImageToBuffer;
      CmdUpdateBuffer:TvkCmdUpdateBuffer;
      CmdFillBuffer:TvkCmdFillBuffer;
      CmdClearColorImage:TvkCmdClearColorImage;
      CmdClearDepthStencilImage:TvkCmdClearDepthStencilImage;
      CmdClearAttachments:TvkCmdClearAttachments;
      CmdResolveImage:TvkCmdResolveImage;
      CmdSetEvent:TvkCmdSetEvent;
      CmdResetEvent:TvkCmdResetEvent;
      CmdWaitEvents:TvkCmdWaitEvents;
      CmdPipelineBarrier:TvkCmdPipelineBarrier;
      CmdBeginQuery:TvkCmdBeginQuery;
      CmdEndQuery:TvkCmdEndQuery;
      CmdResetQueryPool:TvkCmdResetQueryPool;
      CmdWriteTimestamp:TvkCmdWriteTimestamp;
      CmdCopyQueryPoolResults:TvkCmdCopyQueryPoolResults;
      CmdPushConstants:TvkCmdPushConstants;
      CmdBeginRenderPass:TvkCmdBeginRenderPass;
      CmdNextSubpass:TvkCmdNextSubpass;
      CmdEndRenderPass:TvkCmdEndRenderPass;
      CmdExecuteCommands:TvkCmdExecuteCommands;
{$ifdef Android}
      CreateAndroidSurfaceKHR:TvkCreateAndroidSurfaceKHR;
{$endif}
      GetPhysicalDeviceDisplayPropertiesKHR:TvkGetPhysicalDeviceDisplayPropertiesKHR;
      GetPhysicalDeviceDisplayPlanePropertiesKHR:TvkGetPhysicalDeviceDisplayPlanePropertiesKHR;
      GetDisplayPlaneSupportedDisplaysKHR:TvkGetDisplayPlaneSupportedDisplaysKHR;
      GetDisplayModePropertiesKHR:TvkGetDisplayModePropertiesKHR;
      CreateDisplayModeKHR:TvkCreateDisplayModeKHR;
      GetDisplayPlaneCapabilitiesKHR:TvkGetDisplayPlaneCapabilitiesKHR;
      CreateDisplayPlaneSurfaceKHR:TvkCreateDisplayPlaneSurfaceKHR;
      CreateSharedSwapchainsKHR:TvkCreateSharedSwapchainsKHR;
{$ifdef Mir}
      CreateMirSurfaceKHR:TvkCreateMirSurfaceKHR;
{$endif}
{$ifdef Mir}
      GetPhysicalDeviceMirPresentationSupportKHR:TvkGetPhysicalDeviceMirPresentationSupportKHR;
{$endif}
      DestroySurfaceKHR:TvkDestroySurfaceKHR;
      GetPhysicalDeviceSurfaceSupportKHR:TvkGetPhysicalDeviceSurfaceSupportKHR;
      GetPhysicalDeviceSurfaceCapabilitiesKHR:TvkGetPhysicalDeviceSurfaceCapabilitiesKHR;
      GetPhysicalDeviceSurfaceFormatsKHR:TvkGetPhysicalDeviceSurfaceFormatsKHR;
      GetPhysicalDeviceSurfacePresentModesKHR:TvkGetPhysicalDeviceSurfacePresentModesKHR;
      CreateSwapchainKHR:TvkCreateSwapchainKHR;
      DestroySwapchainKHR:TvkDestroySwapchainKHR;
      GetSwapchainImagesKHR:TvkGetSwapchainImagesKHR;
      AcquireNextImageKHR:TvkAcquireNextImageKHR;
      QueuePresentKHR:TvkQueuePresentKHR;
{$ifdef Wayland}
      CreateWaylandSurfaceKHR:TvkCreateWaylandSurfaceKHR;
{$endif}
{$ifdef Wayland}
      GetPhysicalDeviceWaylandPresentationSupportKHR:TvkGetPhysicalDeviceWaylandPresentationSupportKHR;
{$endif}
      CreateWin32SurfaceKHR:TvkCreateWin32SurfaceKHR;
      GetPhysicalDeviceWin32PresentationSupportKHR:TvkGetPhysicalDeviceWin32PresentationSupportKHR;
{$ifdef X11}
      CreateXlibSurfaceKHR:TvkCreateXlibSurfaceKHR;
{$endif}
{$ifdef X11}
      GetPhysicalDeviceXlibPresentationSupportKHR:TvkGetPhysicalDeviceXlibPresentationSupportKHR;
{$endif}
{$ifdef XCB}
      CreateXcbSurfaceKHR:TvkCreateXcbSurfaceKHR;
{$endif}
{$ifdef XCB}
      GetPhysicalDeviceXcbPresentationSupportKHR:TvkGetPhysicalDeviceXcbPresentationSupportKHR;
{$endif}
      CreateDebugReportCallbackEXT:TvkCreateDebugReportCallbackEXT;
      DestroyDebugReportCallbackEXT:TvkDestroyDebugReportCallbackEXT;
      DebugReportMessageEXT:TvkDebugReportMessageEXT;
     end;

     PPVulkanDeviceCommands=^PVulkanDeviceCommands;
     PVulkanDeviceCommands=^TVulkanDeviceCommands;
     TVulkanDeviceCommands=record
      GetDeviceProcAddr:TvkGetDeviceProcAddr;
      DestroyDevice:TvkDestroyDevice;
      GetDeviceQueue:TvkGetDeviceQueue;
      QueueSubmit:TvkQueueSubmit;
      QueueWaitIdle:TvkQueueWaitIdle;
      DeviceWaitIdle:TvkDeviceWaitIdle;
      AllocateMemory:TvkAllocateMemory;
      FreeMemory:TvkFreeMemory;
      MapMemory:TvkMapMemory;
      UnmapMemory:TvkUnmapMemory;
      FlushMappedMemoryRanges:TvkFlushMappedMemoryRanges;
      InvalidateMappedMemoryRanges:TvkInvalidateMappedMemoryRanges;
      GetDeviceMemoryCommitment:TvkGetDeviceMemoryCommitment;
      GetBufferMemoryRequirements:TvkGetBufferMemoryRequirements;
      BindBufferMemory:TvkBindBufferMemory;
      GetImageMemoryRequirements:TvkGetImageMemoryRequirements;
      BindImageMemory:TvkBindImageMemory;
      GetImageSparseMemoryRequirements:TvkGetImageSparseMemoryRequirements;
      QueueBindSparse:TvkQueueBindSparse;
      CreateFence:TvkCreateFence;
      DestroyFence:TvkDestroyFence;
      ResetFences:TvkResetFences;
      GetFenceStatus:TvkGetFenceStatus;
      WaitForFences:TvkWaitForFences;
      CreateSemaphore:TvkCreateSemaphore;
      DestroySemaphore:TvkDestroySemaphore;
      CreateEvent:TvkCreateEvent;
      DestroyEvent:TvkDestroyEvent;
      GetEventStatus:TvkGetEventStatus;
      SetEvent:TvkSetEvent;
      ResetEvent:TvkResetEvent;
      CreateQueryPool:TvkCreateQueryPool;
      DestroyQueryPool:TvkDestroyQueryPool;
      GetQueryPoolResults:TvkGetQueryPoolResults;
      CreateBuffer:TvkCreateBuffer;
      DestroyBuffer:TvkDestroyBuffer;
      CreateBufferView:TvkCreateBufferView;
      DestroyBufferView:TvkDestroyBufferView;
      CreateImage:TvkCreateImage;
      DestroyImage:TvkDestroyImage;
      GetImageSubresourceLayout:TvkGetImageSubresourceLayout;
      CreateImageView:TvkCreateImageView;
      DestroyImageView:TvkDestroyImageView;
      CreateShaderModule:TvkCreateShaderModule;
      DestroyShaderModule:TvkDestroyShaderModule;
      CreatePipelineCache:TvkCreatePipelineCache;
      DestroyPipelineCache:TvkDestroyPipelineCache;
      GetPipelineCacheData:TvkGetPipelineCacheData;
      MergePipelineCaches:TvkMergePipelineCaches;
      CreateGraphicsPipelines:TvkCreateGraphicsPipelines;
      CreateComputePipelines:TvkCreateComputePipelines;
      DestroyPipeline:TvkDestroyPipeline;
      CreatePipelineLayout:TvkCreatePipelineLayout;
      DestroyPipelineLayout:TvkDestroyPipelineLayout;
      CreateSampler:TvkCreateSampler;
      DestroySampler:TvkDestroySampler;
      CreateDescriptorSetLayout:TvkCreateDescriptorSetLayout;
      DestroyDescriptorSetLayout:TvkDestroyDescriptorSetLayout;
      CreateDescriptorPool:TvkCreateDescriptorPool;
      DestroyDescriptorPool:TvkDestroyDescriptorPool;
      ResetDescriptorPool:TvkResetDescriptorPool;
      AllocateDescriptorSets:TvkAllocateDescriptorSets;
      FreeDescriptorSets:TvkFreeDescriptorSets;
      UpdateDescriptorSets:TvkUpdateDescriptorSets;
      CreateFramebuffer:TvkCreateFramebuffer;
      DestroyFramebuffer:TvkDestroyFramebuffer;
      CreateRenderPass:TvkCreateRenderPass;
      DestroyRenderPass:TvkDestroyRenderPass;
      GetRenderAreaGranularity:TvkGetRenderAreaGranularity;
      CreateCommandPool:TvkCreateCommandPool;
      DestroyCommandPool:TvkDestroyCommandPool;
      ResetCommandPool:TvkResetCommandPool;
      AllocateCommandBuffers:TvkAllocateCommandBuffers;
      FreeCommandBuffers:TvkFreeCommandBuffers;
      BeginCommandBuffer:TvkBeginCommandBuffer;
      EndCommandBuffer:TvkEndCommandBuffer;
      ResetCommandBuffer:TvkResetCommandBuffer;
      CmdBindPipeline:TvkCmdBindPipeline;
      CmdSetViewport:TvkCmdSetViewport;
      CmdSetScissor:TvkCmdSetScissor;
      CmdSetLineWidth:TvkCmdSetLineWidth;
      CmdSetDepthBias:TvkCmdSetDepthBias;
      CmdSetBlendConstants:TvkCmdSetBlendConstants;
      CmdSetDepthBounds:TvkCmdSetDepthBounds;
      CmdSetStencilCompareMask:TvkCmdSetStencilCompareMask;
      CmdSetStencilWriteMask:TvkCmdSetStencilWriteMask;
      CmdSetStencilReference:TvkCmdSetStencilReference;
      CmdBindDescriptorSets:TvkCmdBindDescriptorSets;
      CmdBindIndexBuffer:TvkCmdBindIndexBuffer;
      CmdBindVertexBuffers:TvkCmdBindVertexBuffers;
      CmdDraw:TvkCmdDraw;
      CmdDrawIndexed:TvkCmdDrawIndexed;
      CmdDrawIndirect:TvkCmdDrawIndirect;
      CmdDrawIndexedIndirect:TvkCmdDrawIndexedIndirect;
      CmdDispatch:TvkCmdDispatch;
      CmdDispatchIndirect:TvkCmdDispatchIndirect;
      CmdCopyBuffer:TvkCmdCopyBuffer;
      CmdCopyImage:TvkCmdCopyImage;
      CmdBlitImage:TvkCmdBlitImage;
      CmdCopyBufferToImage:TvkCmdCopyBufferToImage;
      CmdCopyImageToBuffer:TvkCmdCopyImageToBuffer;
      CmdUpdateBuffer:TvkCmdUpdateBuffer;
      CmdFillBuffer:TvkCmdFillBuffer;
      CmdClearColorImage:TvkCmdClearColorImage;
      CmdClearDepthStencilImage:TvkCmdClearDepthStencilImage;
      CmdClearAttachments:TvkCmdClearAttachments;
      CmdResolveImage:TvkCmdResolveImage;
      CmdSetEvent:TvkCmdSetEvent;
      CmdResetEvent:TvkCmdResetEvent;
      CmdWaitEvents:TvkCmdWaitEvents;
      CmdPipelineBarrier:TvkCmdPipelineBarrier;
      CmdBeginQuery:TvkCmdBeginQuery;
      CmdEndQuery:TvkCmdEndQuery;
      CmdResetQueryPool:TvkCmdResetQueryPool;
      CmdWriteTimestamp:TvkCmdWriteTimestamp;
      CmdCopyQueryPoolResults:TvkCmdCopyQueryPoolResults;
      CmdPushConstants:TvkCmdPushConstants;
      CmdBeginRenderPass:TvkCmdBeginRenderPass;
      CmdNextSubpass:TvkCmdNextSubpass;
      CmdEndRenderPass:TvkCmdEndRenderPass;
      CmdExecuteCommands:TvkCmdExecuteCommands;
      CreateSharedSwapchainsKHR:TvkCreateSharedSwapchainsKHR;
      CreateSwapchainKHR:TvkCreateSwapchainKHR;
      DestroySwapchainKHR:TvkDestroySwapchainKHR;
      GetSwapchainImagesKHR:TvkGetSwapchainImagesKHR;
      AcquireNextImageKHR:TvkAcquireNextImageKHR;
      QueuePresentKHR:TvkQueuePresentKHR;
     end;

     TVulkan=class
      private
       fCommands:TVulkanCommands;
      public
       constructor Create; reintroduce; overload;
       constructor Create(const AVulkanCommands:TVulkanCommands); reintroduce; overload;
       destructor Destroy; override;
       function CreateInstance(const pCreateInfo:PVkInstanceCreateInfo;const pAllocator:PVkAllocationCallbacks;pInstance:PVkInstance):TVkResult; virtual;
       procedure DestroyInstance(instance:TVkInstance;const pAllocator:PVkAllocationCallbacks); virtual;
       function EnumeratePhysicalDevices(instance:TVkInstance;pPhysicalDeviceCount:PVkUInt32;pPhysicalDevices:PVkPhysicalDevice):TVkResult; virtual;
       function GetDeviceProcAddr(device:TVkDevice;const pName:PVkChar):TPFN_vkVoidFunction; virtual;
       function GetInstanceProcAddr(instance:TVkInstance;const pName:PVkChar):TPFN_vkVoidFunction; virtual;
       procedure GetPhysicalDeviceProperties(physicalDevice:TVkPhysicalDevice;pProperties:PVkPhysicalDeviceProperties); virtual;
       procedure GetPhysicalDeviceQueueFamilyProperties(physicalDevice:TVkPhysicalDevice;pQueueFamilyPropertyCount:PVkUInt32;pQueueFamilyProperties:PVkQueueFamilyProperties); virtual;
       procedure GetPhysicalDeviceMemoryProperties(physicalDevice:TVkPhysicalDevice;pMemoryProperties:PVkPhysicalDeviceMemoryProperties); virtual;
       procedure GetPhysicalDeviceFeatures(physicalDevice:TVkPhysicalDevice;pFeatures:PVkPhysicalDeviceFeatures); virtual;
       procedure GetPhysicalDeviceFormatProperties(physicalDevice:TVkPhysicalDevice;format:TVkFormat;pFormatProperties:PVkFormatProperties); virtual;
       function GetPhysicalDeviceImageFormatProperties(physicalDevice:TVkPhysicalDevice;format:TVkFormat;type_:TVkImageType;tiling:TVkImageTiling;usage:TVkImageUsageFlags;flags:TVkImageCreateFlags;pImageFormatProperties:PVkImageFormatProperties):TVkResult; virtual;
       function CreateDevice(physicalDevice:TVkPhysicalDevice;const pCreateInfo:PVkDeviceCreateInfo;const pAllocator:PVkAllocationCallbacks;pDevice:PVkDevice):TVkResult; virtual;
       procedure DestroyDevice(device:TVkDevice;const pAllocator:PVkAllocationCallbacks); virtual;
       function EnumerateInstanceLayerProperties(pPropertyCount:PVkUInt32;pProperties:PVkLayerProperties):TVkResult; virtual;
       function EnumerateInstanceExtensionProperties(const pLayerName:PVkChar;pPropertyCount:PVkUInt32;pProperties:PVkExtensionProperties):TVkResult; virtual;
       function EnumerateDeviceLayerProperties(physicalDevice:TVkPhysicalDevice;pPropertyCount:PVkUInt32;pProperties:PVkLayerProperties):TVkResult; virtual;
       function EnumerateDeviceExtensionProperties(physicalDevice:TVkPhysicalDevice;const pLayerName:PVkChar;pPropertyCount:PVkUInt32;pProperties:PVkExtensionProperties):TVkResult; virtual;
       procedure GetDeviceQueue(device:TVkDevice;queueFamilyIndex:TVkUInt32;queueIndex:TVkUInt32;pQueue:PVkQueue); virtual;
       function QueueSubmit(queue:TVkQueue;submitCount:TVkUInt32;const pSubmits:PVkSubmitInfo;fence:TVkFence):TVkResult; virtual;
       function QueueWaitIdle(queue:TVkQueue):TVkResult; virtual;
       function DeviceWaitIdle(device:TVkDevice):TVkResult; virtual;
       function AllocateMemory(device:TVkDevice;const pAllocateInfo:PVkMemoryAllocateInfo;const pAllocator:PVkAllocationCallbacks;pMemory:PVkDeviceMemory):TVkResult; virtual;
       procedure FreeMemory(device:TVkDevice;memory:TVkDeviceMemory;const pAllocator:PVkAllocationCallbacks); virtual;
       function MapMemory(device:TVkDevice;memory:TVkDeviceMemory;offset:TVkDeviceSize;size:TVkDeviceSize;flags:TVkMemoryMapFlags;ppData:TVkPointer):TVkResult; virtual;
       procedure UnmapMemory(device:TVkDevice;memory:TVkDeviceMemory); virtual;
       function FlushMappedMemoryRanges(device:TVkDevice;memoryRangeCount:TVkUInt32;const pMemoryRanges:PVkMappedMemoryRange):TVkResult; virtual;
       function InvalidateMappedMemoryRanges(device:TVkDevice;memoryRangeCount:TVkUInt32;const pMemoryRanges:PVkMappedMemoryRange):TVkResult; virtual;
       procedure GetDeviceMemoryCommitment(device:TVkDevice;memory:TVkDeviceMemory;pCommittedMemoryInBytes:PVkDeviceSize); virtual;
       procedure GetBufferMemoryRequirements(device:TVkDevice;buffer:TVkBuffer;pMemoryRequirements:PVkMemoryRequirements); virtual;
       function BindBufferMemory(device:TVkDevice;buffer:TVkBuffer;memory:TVkDeviceMemory;memoryOffset:TVkDeviceSize):TVkResult; virtual;
       procedure GetImageMemoryRequirements(device:TVkDevice;image:TVkImage;pMemoryRequirements:PVkMemoryRequirements); virtual;
       function BindImageMemory(device:TVkDevice;image:TVkImage;memory:TVkDeviceMemory;memoryOffset:TVkDeviceSize):TVkResult; virtual;
       procedure GetImageSparseMemoryRequirements(device:TVkDevice;image:TVkImage;pSparseMemoryRequirementCount:PVkUInt32;pSparseMemoryRequirements:PVkSparseImageMemoryRequirements); virtual;
       procedure GetPhysicalDeviceSparseImageFormatProperties(physicalDevice:TVkPhysicalDevice;format:TVkFormat;type_:TVkImageType;samples:TVkSampleCountFlagBits;usage:TVkImageUsageFlags;tiling:TVkImageTiling;pPropertyCount:PVkUInt32;pProperties:PVkSparseImageFormatProperties); virtual;
       function QueueBindSparse(queue:TVkQueue;bindInfoCount:TVkUInt32;const pBindInfo:PVkBindSparseInfo;fence:TVkFence):TVkResult; virtual;
       function CreateFence(device:TVkDevice;const pCreateInfo:PVkFenceCreateInfo;const pAllocator:PVkAllocationCallbacks;pFence:PVkFence):TVkResult; virtual;
       procedure DestroyFence(device:TVkDevice;fence:TVkFence;const pAllocator:PVkAllocationCallbacks); virtual;
       function ResetFences(device:TVkDevice;fenceCount:TVkUInt32;const pFences:PVkFence):TVkResult; virtual;
       function GetFenceStatus(device:TVkDevice;fence:TVkFence):TVkResult; virtual;
       function WaitForFences(device:TVkDevice;fenceCount:TVkUInt32;const pFences:PVkFence;waitAll:TVkBool32;timeout:TVkUInt64):TVkResult; virtual;
       function CreateSemaphore(device:TVkDevice;const pCreateInfo:PVkSemaphoreCreateInfo;const pAllocator:PVkAllocationCallbacks;pSemaphore:PVkSemaphore):TVkResult; virtual;
       procedure DestroySemaphore(device:TVkDevice;semaphore:TVkSemaphore;const pAllocator:PVkAllocationCallbacks); virtual;
       function CreateEvent(device:TVkDevice;const pCreateInfo:PVkEventCreateInfo;const pAllocator:PVkAllocationCallbacks;pEvent:PVkEvent):TVkResult; virtual;
       procedure DestroyEvent(device:TVkDevice;event:TVkEvent;const pAllocator:PVkAllocationCallbacks); virtual;
       function GetEventStatus(device:TVkDevice;event:TVkEvent):TVkResult; virtual;
       function SetEvent(device:TVkDevice;event:TVkEvent):TVkResult; virtual;
       function ResetEvent(device:TVkDevice;event:TVkEvent):TVkResult; virtual;
       function CreateQueryPool(device:TVkDevice;const pCreateInfo:PVkQueryPoolCreateInfo;const pAllocator:PVkAllocationCallbacks;pQueryPool:PVkQueryPool):TVkResult; virtual;
       procedure DestroyQueryPool(device:TVkDevice;queryPool:TVkQueryPool;const pAllocator:PVkAllocationCallbacks); virtual;
       function GetQueryPoolResults(device:TVkDevice;queryPool:TVkQueryPool;firstQuery:TVkUInt32;queryCount:TVkUInt32;dataSize:TVkPtrInt;pData:TVkPointer;stride:TVkDeviceSize;flags:TVkQueryResultFlags):TVkResult; virtual;
       function CreateBuffer(device:TVkDevice;const pCreateInfo:PVkBufferCreateInfo;const pAllocator:PVkAllocationCallbacks;pBuffer:PVkBuffer):TVkResult; virtual;
       procedure DestroyBuffer(device:TVkDevice;buffer:TVkBuffer;const pAllocator:PVkAllocationCallbacks); virtual;
       function CreateBufferView(device:TVkDevice;const pCreateInfo:PVkBufferViewCreateInfo;const pAllocator:PVkAllocationCallbacks;pView:PVkBufferView):TVkResult; virtual;
       procedure DestroyBufferView(device:TVkDevice;bufferView:TVkBufferView;const pAllocator:PVkAllocationCallbacks); virtual;
       function CreateImage(device:TVkDevice;const pCreateInfo:PVkImageCreateInfo;const pAllocator:PVkAllocationCallbacks;pImage:PVkImage):TVkResult; virtual;
       procedure DestroyImage(device:TVkDevice;image:TVkImage;const pAllocator:PVkAllocationCallbacks); virtual;
       procedure GetImageSubresourceLayout(device:TVkDevice;image:TVkImage;const pSubresource:PVkImageSubresource;pLayout:PVkSubresourceLayout); virtual;
       function CreateImageView(device:TVkDevice;const pCreateInfo:PVkImageViewCreateInfo;const pAllocator:PVkAllocationCallbacks;pView:PVkImageView):TVkResult; virtual;
       procedure DestroyImageView(device:TVkDevice;imageView:TVkImageView;const pAllocator:PVkAllocationCallbacks); virtual;
       function CreateShaderModule(device:TVkDevice;const pCreateInfo:PVkShaderModuleCreateInfo;const pAllocator:PVkAllocationCallbacks;pShaderModule:PVkShaderModule):TVkResult; virtual;
       procedure DestroyShaderModule(device:TVkDevice;shaderModule:TVkShaderModule;const pAllocator:PVkAllocationCallbacks); virtual;
       function CreatePipelineCache(device:TVkDevice;const pCreateInfo:PVkPipelineCacheCreateInfo;const pAllocator:PVkAllocationCallbacks;pPipelineCache:PVkPipelineCache):TVkResult; virtual;
       procedure DestroyPipelineCache(device:TVkDevice;pipelineCache:TVkPipelineCache;const pAllocator:PVkAllocationCallbacks); virtual;
       function GetPipelineCacheData(device:TVkDevice;pipelineCache:TVkPipelineCache;pDataSize:PVkPtrInt;pData:TVkPointer):TVkResult; virtual;
       function MergePipelineCaches(device:TVkDevice;dstCache:TVkPipelineCache;srcCacheCount:TVkUInt32;const pSrcCaches:PVkPipelineCache):TVkResult; virtual;
       function CreateGraphicsPipelines(device:TVkDevice;pipelineCache:TVkPipelineCache;createInfoCount:TVkUInt32;const pCreateInfos:PVkGraphicsPipelineCreateInfo;const pAllocator:PVkAllocationCallbacks;pPipelines:PVkPipeline):TVkResult; virtual;
       function CreateComputePipelines(device:TVkDevice;pipelineCache:TVkPipelineCache;createInfoCount:TVkUInt32;const pCreateInfos:PVkComputePipelineCreateInfo;const pAllocator:PVkAllocationCallbacks;pPipelines:PVkPipeline):TVkResult; virtual;
       procedure DestroyPipeline(device:TVkDevice;pipeline:TVkPipeline;const pAllocator:PVkAllocationCallbacks); virtual;
       function CreatePipelineLayout(device:TVkDevice;const pCreateInfo:PVkPipelineLayoutCreateInfo;const pAllocator:PVkAllocationCallbacks;pPipelineLayout:PVkPipelineLayout):TVkResult; virtual;
       procedure DestroyPipelineLayout(device:TVkDevice;pipelineLayout:TVkPipelineLayout;const pAllocator:PVkAllocationCallbacks); virtual;
       function CreateSampler(device:TVkDevice;const pCreateInfo:PVkSamplerCreateInfo;const pAllocator:PVkAllocationCallbacks;pSampler:PVkSampler):TVkResult; virtual;
       procedure DestroySampler(device:TVkDevice;sampler:TVkSampler;const pAllocator:PVkAllocationCallbacks); virtual;
       function CreateDescriptorSetLayout(device:TVkDevice;const pCreateInfo:PVkDescriptorSetLayoutCreateInfo;const pAllocator:PVkAllocationCallbacks;pSetLayout:PVkDescriptorSetLayout):TVkResult; virtual;
       procedure DestroyDescriptorSetLayout(device:TVkDevice;descriptorSetLayout:TVkDescriptorSetLayout;const pAllocator:PVkAllocationCallbacks); virtual;
       function CreateDescriptorPool(device:TVkDevice;const pCreateInfo:PVkDescriptorPoolCreateInfo;const pAllocator:PVkAllocationCallbacks;pDescriptorPool:PVkDescriptorPool):TVkResult; virtual;
       procedure DestroyDescriptorPool(device:TVkDevice;descriptorPool:TVkDescriptorPool;const pAllocator:PVkAllocationCallbacks); virtual;
       function ResetDescriptorPool(device:TVkDevice;descriptorPool:TVkDescriptorPool;flags:TVkDescriptorPoolResetFlags):TVkResult; virtual;
       function AllocateDescriptorSets(device:TVkDevice;const pAllocateInfo:PVkDescriptorSetAllocateInfo;pDescriptorSets:PVkDescriptorSet):TVkResult; virtual;
       function FreeDescriptorSets(device:TVkDevice;descriptorPool:TVkDescriptorPool;descriptorSetCount:TVkUInt32;const pDescriptorSets:PVkDescriptorSet):TVkResult; virtual;
       procedure UpdateDescriptorSets(device:TVkDevice;descriptorWriteCount:TVkUInt32;const pDescriptorWrites:PVkWriteDescriptorSet;descriptorCopyCount:TVkUInt32;const pDescriptorCopies:PVkCopyDescriptorSet); virtual;
       function CreateFramebuffer(device:TVkDevice;const pCreateInfo:PVkFramebufferCreateInfo;const pAllocator:PVkAllocationCallbacks;pFramebuffer:PVkFramebuffer):TVkResult; virtual;
       procedure DestroyFramebuffer(device:TVkDevice;framebuffer:TVkFramebuffer;const pAllocator:PVkAllocationCallbacks); virtual;
       function CreateRenderPass(device:TVkDevice;const pCreateInfo:PVkRenderPassCreateInfo;const pAllocator:PVkAllocationCallbacks;pRenderPass:PVkRenderPass):TVkResult; virtual;
       procedure DestroyRenderPass(device:TVkDevice;renderPass:TVkRenderPass;const pAllocator:PVkAllocationCallbacks); virtual;
       procedure GetRenderAreaGranularity(device:TVkDevice;renderPass:TVkRenderPass;pGranularity:PVkExtent2D); virtual;
       function CreateCommandPool(device:TVkDevice;const pCreateInfo:PVkCommandPoolCreateInfo;const pAllocator:PVkAllocationCallbacks;pCommandPool:PVkCommandPool):TVkResult; virtual;
       procedure DestroyCommandPool(device:TVkDevice;commandPool:TVkCommandPool;const pAllocator:PVkAllocationCallbacks); virtual;
       function ResetCommandPool(device:TVkDevice;commandPool:TVkCommandPool;flags:TVkCommandPoolResetFlags):TVkResult; virtual;
       function AllocateCommandBuffers(device:TVkDevice;const pAllocateInfo:PVkCommandBufferAllocateInfo;pCommandBuffers:PVkCommandBuffer):TVkResult; virtual;
       procedure FreeCommandBuffers(device:TVkDevice;commandPool:TVkCommandPool;commandBufferCount:TVkUInt32;const pCommandBuffers:PVkCommandBuffer); virtual;
       function BeginCommandBuffer(commandBuffer:TVkCommandBuffer;const pBeginInfo:PVkCommandBufferBeginInfo):TVkResult; virtual;
       function EndCommandBuffer(commandBuffer:TVkCommandBuffer):TVkResult; virtual;
       function ResetCommandBuffer(commandBuffer:TVkCommandBuffer;flags:TVkCommandBufferResetFlags):TVkResult; virtual;
       procedure CmdBindPipeline(commandBuffer:TVkCommandBuffer;pipelineBindPoint:TVkPipelineBindPoint;pipeline:TVkPipeline); virtual;
       procedure CmdSetViewport(commandBuffer:TVkCommandBuffer;firstViewport:TVkUInt32;viewportCount:TVkUInt32;const pViewports:PVkViewport); virtual;
       procedure CmdSetScissor(commandBuffer:TVkCommandBuffer;firstScissor:TVkUInt32;scissorCount:TVkUInt32;const pScissors:PVkRect2D); virtual;
       procedure CmdSetLineWidth(commandBuffer:TVkCommandBuffer;lineWidth:TVkFloat); virtual;
       procedure CmdSetDepthBias(commandBuffer:TVkCommandBuffer;depthBiasConstantFactor:TVkFloat;depthBiasClamp:TVkFloat;depthBiasSlopeFactor:TVkFloat); virtual;
       procedure CmdSetBlendConstants(commandBuffer:TVkCommandBuffer;const blendConstants:TVkFloat); virtual;
       procedure CmdSetDepthBounds(commandBuffer:TVkCommandBuffer;minDepthBounds:TVkFloat;maxDepthBounds:TVkFloat); virtual;
       procedure CmdSetStencilCompareMask(commandBuffer:TVkCommandBuffer;faceMask:TVkStencilFaceFlags;compareMask:TVkUInt32); virtual;
       procedure CmdSetStencilWriteMask(commandBuffer:TVkCommandBuffer;faceMask:TVkStencilFaceFlags;writeMask:TVkUInt32); virtual;
       procedure CmdSetStencilReference(commandBuffer:TVkCommandBuffer;faceMask:TVkStencilFaceFlags;reference:TVkUInt32); virtual;
       procedure CmdBindDescriptorSets(commandBuffer:TVkCommandBuffer;pipelineBindPoint:TVkPipelineBindPoint;layout:TVkPipelineLayout;firstSet:TVkUInt32;descriptorSetCount:TVkUInt32;const pDescriptorSets:PVkDescriptorSet;dynamicOffsetCount:TVkUInt32;const pDynamicOffsets:PVkUInt32); virtual;
       procedure CmdBindIndexBuffer(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;indexType:TVkIndexType); virtual;
       procedure CmdBindVertexBuffers(commandBuffer:TVkCommandBuffer;firstBinding:TVkUInt32;bindingCount:TVkUInt32;const pBuffers:PVkBuffer;const pOffsets:PVkDeviceSize); virtual;
       procedure CmdDraw(commandBuffer:TVkCommandBuffer;vertexCount:TVkUInt32;instanceCount:TVkUInt32;firstVertex:TVkUInt32;firstInstance:TVkUInt32); virtual;
       procedure CmdDrawIndexed(commandBuffer:TVkCommandBuffer;indexCount:TVkUInt32;instanceCount:TVkUInt32;firstIndex:TVkUInt32;vertexOffset:TVkInt32;firstInstance:TVkUInt32); virtual;
       procedure CmdDrawIndirect(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;drawCount:TVkUInt32;stride:TVkUInt32); virtual;
       procedure CmdDrawIndexedIndirect(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;drawCount:TVkUInt32;stride:TVkUInt32); virtual;
       procedure CmdDispatch(commandBuffer:TVkCommandBuffer;x:TVkUInt32;y:TVkUInt32;z:TVkUInt32); virtual;
       procedure CmdDispatchIndirect(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize); virtual;
       procedure CmdCopyBuffer(commandBuffer:TVkCommandBuffer;srcBuffer:TVkBuffer;dstBuffer:TVkBuffer;regionCount:TVkUInt32;const pRegions:PVkBufferCopy); virtual;
       procedure CmdCopyImage(commandBuffer:TVkCommandBuffer;srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkImageCopy); virtual;
       procedure CmdBlitImage(commandBuffer:TVkCommandBuffer;srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkImageBlit;filter:TVkFilter); virtual;
       procedure CmdCopyBufferToImage(commandBuffer:TVkCommandBuffer;srcBuffer:TVkBuffer;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkBufferImageCopy); virtual;
       procedure CmdCopyImageToBuffer(commandBuffer:TVkCommandBuffer;srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstBuffer:TVkBuffer;regionCount:TVkUInt32;const pRegions:PVkBufferImageCopy); virtual;
       procedure CmdUpdateBuffer(commandBuffer:TVkCommandBuffer;dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;dataSize:TVkDeviceSize;const pData:PVkUInt32); virtual;
       procedure CmdFillBuffer(commandBuffer:TVkCommandBuffer;dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;size:TVkDeviceSize;data:TVkUInt32); virtual;
       procedure CmdClearColorImage(commandBuffer:TVkCommandBuffer;image:TVkImage;imageLayout:TVkImageLayout;const pColor:PVkClearColorValue;rangeCount:TVkUInt32;const pRanges:PVkImageSubresourceRange); virtual;
       procedure CmdClearDepthStencilImage(commandBuffer:TVkCommandBuffer;image:TVkImage;imageLayout:TVkImageLayout;const pDepthStencil:PVkClearDepthStencilValue;rangeCount:TVkUInt32;const pRanges:PVkImageSubresourceRange); virtual;
       procedure CmdClearAttachments(commandBuffer:TVkCommandBuffer;attachmentCount:TVkUInt32;const pAttachments:PVkClearAttachment;rectCount:TVkUInt32;const pRects:PVkClearRect); virtual;
       procedure CmdResolveImage(commandBuffer:TVkCommandBuffer;srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkImageResolve); virtual;
       procedure CmdSetEvent(commandBuffer:TVkCommandBuffer;event:TVkEvent;stageMask:TVkPipelineStageFlags); virtual;
       procedure CmdResetEvent(commandBuffer:TVkCommandBuffer;event:TVkEvent;stageMask:TVkPipelineStageFlags); virtual;
       procedure CmdWaitEvents(commandBuffer:TVkCommandBuffer;eventCount:TVkUInt32;const pEvents:PVkEvent;srcStageMask:TVkPipelineStageFlags;dstStageMask:TVkPipelineStageFlags;memoryBarrierCount:TVkUInt32;const pMemoryBarriers:PVkMemoryBarrier;bufferMemoryBarrierCount:TVkUInt32;const pBufferMemoryBarriers:PVkBufferMemoryBarrier;imageMemoryBarrierCount:TVkUInt32;const pImageMemoryBarriers:PVkImageMemoryBarrier); virtual;
       procedure CmdPipelineBarrier(commandBuffer:TVkCommandBuffer;srcStageMask:TVkPipelineStageFlags;dstStageMask:TVkPipelineStageFlags;dependencyFlags:TVkDependencyFlags;memoryBarrierCount:TVkUInt32;const pMemoryBarriers:PVkMemoryBarrier;bufferMemoryBarrierCount:TVkUInt32;const pBufferMemoryBarriers:PVkBufferMemoryBarrier;imageMemoryBarrierCount:TVkUInt32;const pImageMemoryBarriers:PVkImageMemoryBarrier); virtual;
       procedure CmdBeginQuery(commandBuffer:TVkCommandBuffer;queryPool:TVkQueryPool;query:TVkUInt32;flags:TVkQueryControlFlags); virtual;
       procedure CmdEndQuery(commandBuffer:TVkCommandBuffer;queryPool:TVkQueryPool;query:TVkUInt32); virtual;
       procedure CmdResetQueryPool(commandBuffer:TVkCommandBuffer;queryPool:TVkQueryPool;firstQuery:TVkUInt32;queryCount:TVkUInt32); virtual;
       procedure CmdWriteTimestamp(commandBuffer:TVkCommandBuffer;pipelineStage:TVkPipelineStageFlagBits;queryPool:TVkQueryPool;query:TVkUInt32); virtual;
       procedure CmdCopyQueryPoolResults(commandBuffer:TVkCommandBuffer;queryPool:TVkQueryPool;firstQuery:TVkUInt32;queryCount:TVkUInt32;dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;stride:TVkDeviceSize;flags:TVkQueryResultFlags); virtual;
       procedure CmdPushConstants(commandBuffer:TVkCommandBuffer;layout:TVkPipelineLayout;stageFlags:TVkShaderStageFlags;offset:TVkUInt32;size:TVkUInt32;const pValues:TVkPointer); virtual;
       procedure CmdBeginRenderPass(commandBuffer:TVkCommandBuffer;const pRenderPassBegin:PVkRenderPassBeginInfo;contents:TVkSubpassContents); virtual;
       procedure CmdNextSubpass(commandBuffer:TVkCommandBuffer;contents:TVkSubpassContents); virtual;
       procedure CmdEndRenderPass(commandBuffer:TVkCommandBuffer); virtual;
       procedure CmdExecuteCommands(commandBuffer:TVkCommandBuffer;commandBufferCount:TVkUInt32;const pCommandBuffers:PVkCommandBuffer); virtual;
{$ifdef Android}
       function CreateAndroidSurfaceKHR(instance:TVkInstance;const pCreateInfo:PVkAndroidSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; virtual;
{$endif}
       function GetPhysicalDeviceDisplayPropertiesKHR(physicalDevice:TVkPhysicalDevice;pPropertyCount:PVkUInt32;pProperties:PVkDisplayPropertiesKHR):TVkResult; virtual;
       function GetPhysicalDeviceDisplayPlanePropertiesKHR(physicalDevice:TVkPhysicalDevice;pPropertyCount:PVkUInt32;pProperties:PVkDisplayPlanePropertiesKHR):TVkResult; virtual;
       function GetDisplayPlaneSupportedDisplaysKHR(physicalDevice:TVkPhysicalDevice;planeIndex:TVkUInt32;pDisplayCount:PVkUInt32;pDisplays:PVkDisplayKHR):TVkResult; virtual;
       function GetDisplayModePropertiesKHR(physicalDevice:TVkPhysicalDevice;display:TVkDisplayKHR;pPropertyCount:PVkUInt32;pProperties:PVkDisplayModePropertiesKHR):TVkResult; virtual;
       function CreateDisplayModeKHR(physicalDevice:TVkPhysicalDevice;display:TVkDisplayKHR;const pCreateInfo:PVkDisplayModeCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pMode:PVkDisplayModeKHR):TVkResult; virtual;
       function GetDisplayPlaneCapabilitiesKHR(physicalDevice:TVkPhysicalDevice;mode:TVkDisplayModeKHR;planeIndex:TVkUInt32;pCapabilities:PVkDisplayPlaneCapabilitiesKHR):TVkResult; virtual;
       function CreateDisplayPlaneSurfaceKHR(instance:TVkInstance;const pCreateInfo:PVkDisplaySurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; virtual;
       function CreateSharedSwapchainsKHR(device:TVkDevice;swapchainCount:TVkUInt32;const pCreateInfos:PVkSwapchainCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSwapchains:PVkSwapchainKHR):TVkResult; virtual;
{$ifdef Mir}
       function CreateMirSurfaceKHR(instance:TVkInstance;const pCreateInfo:PVkMirSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; virtual;
{$endif}
{$ifdef Mir}
       function GetPhysicalDeviceMirPresentationSupportKHR(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;connection:PMirConnection):TVkBool32; virtual;
{$endif}
       procedure DestroySurfaceKHR(instance:TVkInstance;surface:TVkSurfaceKHR;const pAllocator:PVkAllocationCallbacks); virtual;
       function GetPhysicalDeviceSurfaceSupportKHR(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;surface:TVkSurfaceKHR;pSupported:PVkBool32):TVkResult; virtual;
       function GetPhysicalDeviceSurfaceCapabilitiesKHR(physicalDevice:TVkPhysicalDevice;surface:TVkSurfaceKHR;pSurfaceCapabilities:PVkSurfaceCapabilitiesKHR):TVkResult; virtual;
       function GetPhysicalDeviceSurfaceFormatsKHR(physicalDevice:TVkPhysicalDevice;surface:TVkSurfaceKHR;pSurfaceFormatCount:PVkUInt32;pSurfaceFormats:PVkSurfaceFormatKHR):TVkResult; virtual;
       function GetPhysicalDeviceSurfacePresentModesKHR(physicalDevice:TVkPhysicalDevice;surface:TVkSurfaceKHR;pPresentModeCount:PVkUInt32;pPresentModes:PVkPresentModeKHR):TVkResult; virtual;
       function CreateSwapchainKHR(device:TVkDevice;const pCreateInfo:PVkSwapchainCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSwapchain:PVkSwapchainKHR):TVkResult; virtual;
       procedure DestroySwapchainKHR(device:TVkDevice;swapchain:TVkSwapchainKHR;const pAllocator:PVkAllocationCallbacks); virtual;
       function GetSwapchainImagesKHR(device:TVkDevice;swapchain:TVkSwapchainKHR;pSwapchainImageCount:PVkUInt32;pSwapchainImages:PVkImage):TVkResult; virtual;
       function AcquireNextImageKHR(device:TVkDevice;swapchain:TVkSwapchainKHR;timeout:TVkUInt64;semaphore:TVkSemaphore;fence:TVkFence;pImageIndex:PVkUInt32):TVkResult; virtual;
       function QueuePresentKHR(queue:TVkQueue;const pPresentInfo:PVkPresentInfoKHR):TVkResult; virtual;
{$ifdef Wayland}
       function CreateWaylandSurfaceKHR(instance:TVkInstance;const pCreateInfo:PVkWaylandSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; virtual;
{$endif}
{$ifdef Wayland}
       function GetPhysicalDeviceWaylandPresentationSupportKHR(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;display:Pwl_display):TVkBool32; virtual;
{$endif}
       function CreateWin32SurfaceKHR(instance:TVkInstance;const pCreateInfo:PVkWin32SurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; virtual;
       function GetPhysicalDeviceWin32PresentationSupportKHR(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32):TVkBool32; virtual;
{$ifdef X11}
       function CreateXlibSurfaceKHR(instance:TVkInstance;const pCreateInfo:PVkXlibSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; virtual;
{$endif}
{$ifdef X11}
       function GetPhysicalDeviceXlibPresentationSupportKHR(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;dpy:PDisplay;visualID:TVisualID):TVkBool32; virtual;
{$endif}
{$ifdef XCB}
       function CreateXcbSurfaceKHR(instance:TVkInstance;const pCreateInfo:PVkXcbSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; virtual;
{$endif}
{$ifdef XCB}
       function GetPhysicalDeviceXcbPresentationSupportKHR(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;connection:Pxcb_connection;visual_id:Txcb_visualid):TVkBool32; virtual;
{$endif}
       function CreateDebugReportCallbackEXT(instance:TVkInstance;const pCreateInfo:PVkDebugReportCallbackCreateInfoEXT;const pAllocator:PVkAllocationCallbacks;pCallback:PVkDebugReportCallbackEXT):TVkResult; virtual;
       procedure DestroyDebugReportCallbackEXT(instance:TVkInstance;callback:TVkDebugReportCallbackEXT;const pAllocator:PVkAllocationCallbacks); virtual;
       procedure DebugReportMessageEXT(instance:TVkInstance;flags:TVkDebugReportFlagsEXT;objectType:TVkDebugReportObjectTypeEXT;object_:TVkUInt64;location:TVkPtrInt;messageCode:TVkInt32;const pLayerPrefix:PVkChar;const pMessage:PVkChar); virtual;
       property Commands:TVulkanCommands read fCommands;
     end;

var LibVulkan:pointer=nil;

    vk:TVulkan=nil;

    vkCreateInstance:TvkCreateInstance=nil;
    vkDestroyInstance:TvkDestroyInstance=nil;
    vkEnumeratePhysicalDevices:TvkEnumeratePhysicalDevices=nil;
    vkGetDeviceProcAddr:TvkGetDeviceProcAddr=nil;
    vkGetInstanceProcAddr:TvkGetInstanceProcAddr=nil;
    vkGetPhysicalDeviceProperties:TvkGetPhysicalDeviceProperties=nil;
    vkGetPhysicalDeviceQueueFamilyProperties:TvkGetPhysicalDeviceQueueFamilyProperties=nil;
    vkGetPhysicalDeviceMemoryProperties:TvkGetPhysicalDeviceMemoryProperties=nil;
    vkGetPhysicalDeviceFeatures:TvkGetPhysicalDeviceFeatures=nil;
    vkGetPhysicalDeviceFormatProperties:TvkGetPhysicalDeviceFormatProperties=nil;
    vkGetPhysicalDeviceImageFormatProperties:TvkGetPhysicalDeviceImageFormatProperties=nil;
    vkCreateDevice:TvkCreateDevice=nil;
    vkDestroyDevice:TvkDestroyDevice=nil;
    vkEnumerateInstanceLayerProperties:TvkEnumerateInstanceLayerProperties=nil;
    vkEnumerateInstanceExtensionProperties:TvkEnumerateInstanceExtensionProperties=nil;
    vkEnumerateDeviceLayerProperties:TvkEnumerateDeviceLayerProperties=nil;
    vkEnumerateDeviceExtensionProperties:TvkEnumerateDeviceExtensionProperties=nil;
    vkGetDeviceQueue:TvkGetDeviceQueue=nil;
    vkQueueSubmit:TvkQueueSubmit=nil;
    vkQueueWaitIdle:TvkQueueWaitIdle=nil;
    vkDeviceWaitIdle:TvkDeviceWaitIdle=nil;
    vkAllocateMemory:TvkAllocateMemory=nil;
    vkFreeMemory:TvkFreeMemory=nil;
    vkMapMemory:TvkMapMemory=nil;
    vkUnmapMemory:TvkUnmapMemory=nil;
    vkFlushMappedMemoryRanges:TvkFlushMappedMemoryRanges=nil;
    vkInvalidateMappedMemoryRanges:TvkInvalidateMappedMemoryRanges=nil;
    vkGetDeviceMemoryCommitment:TvkGetDeviceMemoryCommitment=nil;
    vkGetBufferMemoryRequirements:TvkGetBufferMemoryRequirements=nil;
    vkBindBufferMemory:TvkBindBufferMemory=nil;
    vkGetImageMemoryRequirements:TvkGetImageMemoryRequirements=nil;
    vkBindImageMemory:TvkBindImageMemory=nil;
    vkGetImageSparseMemoryRequirements:TvkGetImageSparseMemoryRequirements=nil;
    vkGetPhysicalDeviceSparseImageFormatProperties:TvkGetPhysicalDeviceSparseImageFormatProperties=nil;
    vkQueueBindSparse:TvkQueueBindSparse=nil;
    vkCreateFence:TvkCreateFence=nil;
    vkDestroyFence:TvkDestroyFence=nil;
    vkResetFences:TvkResetFences=nil;
    vkGetFenceStatus:TvkGetFenceStatus=nil;
    vkWaitForFences:TvkWaitForFences=nil;
    vkCreateSemaphore:TvkCreateSemaphore=nil;
    vkDestroySemaphore:TvkDestroySemaphore=nil;
    vkCreateEvent:TvkCreateEvent=nil;
    vkDestroyEvent:TvkDestroyEvent=nil;
    vkGetEventStatus:TvkGetEventStatus=nil;
    vkSetEvent:TvkSetEvent=nil;
    vkResetEvent:TvkResetEvent=nil;
    vkCreateQueryPool:TvkCreateQueryPool=nil;
    vkDestroyQueryPool:TvkDestroyQueryPool=nil;
    vkGetQueryPoolResults:TvkGetQueryPoolResults=nil;
    vkCreateBuffer:TvkCreateBuffer=nil;
    vkDestroyBuffer:TvkDestroyBuffer=nil;
    vkCreateBufferView:TvkCreateBufferView=nil;
    vkDestroyBufferView:TvkDestroyBufferView=nil;
    vkCreateImage:TvkCreateImage=nil;
    vkDestroyImage:TvkDestroyImage=nil;
    vkGetImageSubresourceLayout:TvkGetImageSubresourceLayout=nil;
    vkCreateImageView:TvkCreateImageView=nil;
    vkDestroyImageView:TvkDestroyImageView=nil;
    vkCreateShaderModule:TvkCreateShaderModule=nil;
    vkDestroyShaderModule:TvkDestroyShaderModule=nil;
    vkCreatePipelineCache:TvkCreatePipelineCache=nil;
    vkDestroyPipelineCache:TvkDestroyPipelineCache=nil;
    vkGetPipelineCacheData:TvkGetPipelineCacheData=nil;
    vkMergePipelineCaches:TvkMergePipelineCaches=nil;
    vkCreateGraphicsPipelines:TvkCreateGraphicsPipelines=nil;
    vkCreateComputePipelines:TvkCreateComputePipelines=nil;
    vkDestroyPipeline:TvkDestroyPipeline=nil;
    vkCreatePipelineLayout:TvkCreatePipelineLayout=nil;
    vkDestroyPipelineLayout:TvkDestroyPipelineLayout=nil;
    vkCreateSampler:TvkCreateSampler=nil;
    vkDestroySampler:TvkDestroySampler=nil;
    vkCreateDescriptorSetLayout:TvkCreateDescriptorSetLayout=nil;
    vkDestroyDescriptorSetLayout:TvkDestroyDescriptorSetLayout=nil;
    vkCreateDescriptorPool:TvkCreateDescriptorPool=nil;
    vkDestroyDescriptorPool:TvkDestroyDescriptorPool=nil;
    vkResetDescriptorPool:TvkResetDescriptorPool=nil;
    vkAllocateDescriptorSets:TvkAllocateDescriptorSets=nil;
    vkFreeDescriptorSets:TvkFreeDescriptorSets=nil;
    vkUpdateDescriptorSets:TvkUpdateDescriptorSets=nil;
    vkCreateFramebuffer:TvkCreateFramebuffer=nil;
    vkDestroyFramebuffer:TvkDestroyFramebuffer=nil;
    vkCreateRenderPass:TvkCreateRenderPass=nil;
    vkDestroyRenderPass:TvkDestroyRenderPass=nil;
    vkGetRenderAreaGranularity:TvkGetRenderAreaGranularity=nil;
    vkCreateCommandPool:TvkCreateCommandPool=nil;
    vkDestroyCommandPool:TvkDestroyCommandPool=nil;
    vkResetCommandPool:TvkResetCommandPool=nil;
    vkAllocateCommandBuffers:TvkAllocateCommandBuffers=nil;
    vkFreeCommandBuffers:TvkFreeCommandBuffers=nil;
    vkBeginCommandBuffer:TvkBeginCommandBuffer=nil;
    vkEndCommandBuffer:TvkEndCommandBuffer=nil;
    vkResetCommandBuffer:TvkResetCommandBuffer=nil;
    vkCmdBindPipeline:TvkCmdBindPipeline=nil;
    vkCmdSetViewport:TvkCmdSetViewport=nil;
    vkCmdSetScissor:TvkCmdSetScissor=nil;
    vkCmdSetLineWidth:TvkCmdSetLineWidth=nil;
    vkCmdSetDepthBias:TvkCmdSetDepthBias=nil;
    vkCmdSetBlendConstants:TvkCmdSetBlendConstants=nil;
    vkCmdSetDepthBounds:TvkCmdSetDepthBounds=nil;
    vkCmdSetStencilCompareMask:TvkCmdSetStencilCompareMask=nil;
    vkCmdSetStencilWriteMask:TvkCmdSetStencilWriteMask=nil;
    vkCmdSetStencilReference:TvkCmdSetStencilReference=nil;
    vkCmdBindDescriptorSets:TvkCmdBindDescriptorSets=nil;
    vkCmdBindIndexBuffer:TvkCmdBindIndexBuffer=nil;
    vkCmdBindVertexBuffers:TvkCmdBindVertexBuffers=nil;
    vkCmdDraw:TvkCmdDraw=nil;
    vkCmdDrawIndexed:TvkCmdDrawIndexed=nil;
    vkCmdDrawIndirect:TvkCmdDrawIndirect=nil;
    vkCmdDrawIndexedIndirect:TvkCmdDrawIndexedIndirect=nil;
    vkCmdDispatch:TvkCmdDispatch=nil;
    vkCmdDispatchIndirect:TvkCmdDispatchIndirect=nil;
    vkCmdCopyBuffer:TvkCmdCopyBuffer=nil;
    vkCmdCopyImage:TvkCmdCopyImage=nil;
    vkCmdBlitImage:TvkCmdBlitImage=nil;
    vkCmdCopyBufferToImage:TvkCmdCopyBufferToImage=nil;
    vkCmdCopyImageToBuffer:TvkCmdCopyImageToBuffer=nil;
    vkCmdUpdateBuffer:TvkCmdUpdateBuffer=nil;
    vkCmdFillBuffer:TvkCmdFillBuffer=nil;
    vkCmdClearColorImage:TvkCmdClearColorImage=nil;
    vkCmdClearDepthStencilImage:TvkCmdClearDepthStencilImage=nil;
    vkCmdClearAttachments:TvkCmdClearAttachments=nil;
    vkCmdResolveImage:TvkCmdResolveImage=nil;
    vkCmdSetEvent:TvkCmdSetEvent=nil;
    vkCmdResetEvent:TvkCmdResetEvent=nil;
    vkCmdWaitEvents:TvkCmdWaitEvents=nil;
    vkCmdPipelineBarrier:TvkCmdPipelineBarrier=nil;
    vkCmdBeginQuery:TvkCmdBeginQuery=nil;
    vkCmdEndQuery:TvkCmdEndQuery=nil;
    vkCmdResetQueryPool:TvkCmdResetQueryPool=nil;
    vkCmdWriteTimestamp:TvkCmdWriteTimestamp=nil;
    vkCmdCopyQueryPoolResults:TvkCmdCopyQueryPoolResults=nil;
    vkCmdPushConstants:TvkCmdPushConstants=nil;
    vkCmdBeginRenderPass:TvkCmdBeginRenderPass=nil;
    vkCmdNextSubpass:TvkCmdNextSubpass=nil;
    vkCmdEndRenderPass:TvkCmdEndRenderPass=nil;
    vkCmdExecuteCommands:TvkCmdExecuteCommands=nil;
{$ifdef Android}
    vkCreateAndroidSurfaceKHR:TvkCreateAndroidSurfaceKHR=nil;
{$endif}
    vkGetPhysicalDeviceDisplayPropertiesKHR:TvkGetPhysicalDeviceDisplayPropertiesKHR=nil;
    vkGetPhysicalDeviceDisplayPlanePropertiesKHR:TvkGetPhysicalDeviceDisplayPlanePropertiesKHR=nil;
    vkGetDisplayPlaneSupportedDisplaysKHR:TvkGetDisplayPlaneSupportedDisplaysKHR=nil;
    vkGetDisplayModePropertiesKHR:TvkGetDisplayModePropertiesKHR=nil;
    vkCreateDisplayModeKHR:TvkCreateDisplayModeKHR=nil;
    vkGetDisplayPlaneCapabilitiesKHR:TvkGetDisplayPlaneCapabilitiesKHR=nil;
    vkCreateDisplayPlaneSurfaceKHR:TvkCreateDisplayPlaneSurfaceKHR=nil;
    vkCreateSharedSwapchainsKHR:TvkCreateSharedSwapchainsKHR=nil;
{$ifdef Mir}
    vkCreateMirSurfaceKHR:TvkCreateMirSurfaceKHR=nil;
{$endif}
{$ifdef Mir}
    vkGetPhysicalDeviceMirPresentationSupportKHR:TvkGetPhysicalDeviceMirPresentationSupportKHR=nil;
{$endif}
    vkDestroySurfaceKHR:TvkDestroySurfaceKHR=nil;
    vkGetPhysicalDeviceSurfaceSupportKHR:TvkGetPhysicalDeviceSurfaceSupportKHR=nil;
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR:TvkGetPhysicalDeviceSurfaceCapabilitiesKHR=nil;
    vkGetPhysicalDeviceSurfaceFormatsKHR:TvkGetPhysicalDeviceSurfaceFormatsKHR=nil;
    vkGetPhysicalDeviceSurfacePresentModesKHR:TvkGetPhysicalDeviceSurfacePresentModesKHR=nil;
    vkCreateSwapchainKHR:TvkCreateSwapchainKHR=nil;
    vkDestroySwapchainKHR:TvkDestroySwapchainKHR=nil;
    vkGetSwapchainImagesKHR:TvkGetSwapchainImagesKHR=nil;
    vkAcquireNextImageKHR:TvkAcquireNextImageKHR=nil;
    vkQueuePresentKHR:TvkQueuePresentKHR=nil;
{$ifdef Wayland}
    vkCreateWaylandSurfaceKHR:TvkCreateWaylandSurfaceKHR=nil;
{$endif}
{$ifdef Wayland}
    vkGetPhysicalDeviceWaylandPresentationSupportKHR:TvkGetPhysicalDeviceWaylandPresentationSupportKHR=nil;
{$endif}
    vkCreateWin32SurfaceKHR:TvkCreateWin32SurfaceKHR=nil;
    vkGetPhysicalDeviceWin32PresentationSupportKHR:TvkGetPhysicalDeviceWin32PresentationSupportKHR=nil;
{$ifdef X11}
    vkCreateXlibSurfaceKHR:TvkCreateXlibSurfaceKHR=nil;
{$endif}
{$ifdef X11}
    vkGetPhysicalDeviceXlibPresentationSupportKHR:TvkGetPhysicalDeviceXlibPresentationSupportKHR=nil;
{$endif}
{$ifdef XCB}
    vkCreateXcbSurfaceKHR:TvkCreateXcbSurfaceKHR=nil;
{$endif}
{$ifdef XCB}
    vkGetPhysicalDeviceXcbPresentationSupportKHR:TvkGetPhysicalDeviceXcbPresentationSupportKHR=nil;
{$endif}
    vkCreateDebugReportCallbackEXT:TvkCreateDebugReportCallbackEXT=nil;
    vkDestroyDebugReportCallbackEXT:TvkDestroyDebugReportCallbackEXT=nil;
    vkDebugReportMessageEXT:TvkDebugReportMessageEXT=nil;

function VK_MAKE_VERSION(const VersionMajor,VersionMinor,VersionPatch:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}
function VK_VERSION_MAJOR(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}
function VK_VERSION_MINOR(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}
function VK_VERSION_PATCH(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}

function vkLoadLibrary(const LibraryName:string):pointer; {$ifdef CAN_INLINE}inline;{$endif}
function vkFreeLibrary(LibraryHandle:pointer):boolean; {$ifdef CAN_INLINE}inline;{$endif}
function vkGetProcAddress(LibraryHandle:pointer;const ProcName:string):pointer; {$ifdef CAN_INLINE}inline;{$endif}

function vkVoidFunctionToPointer(const VoidFunction:TPFN_vkVoidFunction):pointer; {$ifdef CAN_INLINE}inline;{$endif}

function LoadVulkanLibrary(const LibraryName:string=VK_DEFAULT_LIB_NAME):boolean;
function LoadVulkanGlobalCommands:boolean;
function LoadVulkanInstanceCommands(const GetInstanceProcAddr:TvkGetInstanceProcAddr;const Instance:TVkInstance;out InstanceCommands:TVulkanCommands):boolean;
function LoadVulkanDeviceCommands(const GetDeviceProcAddr:TvkGetDeviceProcAddr;const Device:TVkDevice;out DeviceCommands:TVulkanCommands):boolean; overload;
function LoadVulkanDeviceCommands(const GetDeviceProcAddr:TvkGetDeviceProcAddr;const Device:TVkDevice;out DeviceCommands:TVulkanDeviceCommands):boolean; overload;

implementation

function VK_MAKE_VERSION(const VersionMajor,VersionMinor,VersionPatch:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=(VersionMajor shl 22) or (VersionMinor shl 12) or (VersionPatch shl 0);
end;

function VK_VERSION_MAJOR(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=Version shr 22;
end;

function VK_VERSION_MINOR(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=(Version shr 12) and $3ff;
end;

function VK_VERSION_PATCH(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=(Version shr 0) and $fff;
end;

function vkLoadLibrary(const LibraryName:string):pointer; {$ifdef CAN_INLINE}inline;{$endif}
begin
{$ifdef Windows}
 result:=pointer(LoadLibrary(PChar(LibraryName)));
{$else}
{$ifdef Linux}
 result:=dlopen(PChar(LibraryName),RTLD_NOW or RTLD_LAZY);
{$else}
 result:=nil;
{$endif}
{$endif}
end;

function vkFreeLibrary(LibraryHandle:pointer):boolean; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=assigned(LibraryHandle);
 if result then begin
{$ifdef Windows}
  result:=FreeLibrary(HMODULE(LibraryHandle));
{$else}
{$ifdef Unix}
  result:=dlclose(LibraryHandle)=0;
{$else}
  result:=false;
{$endif}
{$endif}
 end;
end;

function vkGetProcAddress(LibraryHandle:pointer;const ProcName:string):pointer; {$ifdef CAN_INLINE}inline;{$endif}
begin
{$ifdef Windows}
 result:=GetProcAddress(HMODULE(LibraryHandle),PChar(ProcName));
{$else}
{$ifdef Unix}
 result:=dlsym(LibraryHandle,PChar(ProcName));
{$else}
 result:=nil;
{$endif}
{$endif}
end;

function vkVoidFunctionToPointer(const VoidFunction:TPFN_vkVoidFunction):pointer; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=addr(VoidFunction);
end;

function LoadVulkanLibrary(const LibraryName:string=VK_DEFAULT_LIB_NAME):boolean;
begin
 LibVulkan:=vkLoadLibrary(LibraryName);
 result:=assigned(LibVulkan);
 if result then begin
  vkGetInstanceProcAddr:=vkGetProcAddress(LibVulkan,'vkGetInstanceProcAddr');
  @vk.fCommands.GetInstanceProcAddr:=addr(vkGetInstanceProcAddr);
  result:=assigned(vkGetInstanceProcAddr);
 end;
end;

function LoadVulkanGlobalCommands:boolean;
begin
 FillChar(vk,SizeOf(TVulkanCommands),#0);
 result:=assigned(vkGetInstanceProcAddr);
 if result then begin
  @vk.fCommands.GetInstanceProcAddr:=addr(vkGetInstanceProcAddr);
  @vkCreateInstance:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateInstance')));
  @vk.fCommands.CreateInstance:=addr(vkCreateInstance);
  @vkDestroyInstance:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroyInstance')));
  @vk.fCommands.DestroyInstance:=addr(vkDestroyInstance);
  @vkEnumeratePhysicalDevices:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkEnumeratePhysicalDevices')));
  @vk.fCommands.EnumeratePhysicalDevices:=addr(vkEnumeratePhysicalDevices);
  @vkGetDeviceProcAddr:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetDeviceProcAddr')));
  @vk.fCommands.GetDeviceProcAddr:=addr(vkGetDeviceProcAddr);
  @vkGetPhysicalDeviceProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetPhysicalDeviceProperties')));
  @vk.fCommands.GetPhysicalDeviceProperties:=addr(vkGetPhysicalDeviceProperties);
  @vkGetPhysicalDeviceQueueFamilyProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetPhysicalDeviceQueueFamilyProperties')));
  @vk.fCommands.GetPhysicalDeviceQueueFamilyProperties:=addr(vkGetPhysicalDeviceQueueFamilyProperties);
  @vkGetPhysicalDeviceMemoryProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetPhysicalDeviceMemoryProperties')));
  @vk.fCommands.GetPhysicalDeviceMemoryProperties:=addr(vkGetPhysicalDeviceMemoryProperties);
  @vkGetPhysicalDeviceFeatures:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetPhysicalDeviceFeatures')));
  @vk.fCommands.GetPhysicalDeviceFeatures:=addr(vkGetPhysicalDeviceFeatures);
  @vkGetPhysicalDeviceFormatProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetPhysicalDeviceFormatProperties')));
  @vk.fCommands.GetPhysicalDeviceFormatProperties:=addr(vkGetPhysicalDeviceFormatProperties);
  @vkGetPhysicalDeviceImageFormatProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetPhysicalDeviceImageFormatProperties')));
  @vk.fCommands.GetPhysicalDeviceImageFormatProperties:=addr(vkGetPhysicalDeviceImageFormatProperties);
  @vkCreateDevice:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateDevice')));
  @vk.fCommands.CreateDevice:=addr(vkCreateDevice);
  @vkDestroyDevice:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroyDevice')));
  @vk.fCommands.DestroyDevice:=addr(vkDestroyDevice);
  @vkEnumerateInstanceLayerProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkEnumerateInstanceLayerProperties')));
  @vk.fCommands.EnumerateInstanceLayerProperties:=addr(vkEnumerateInstanceLayerProperties);
  @vkEnumerateInstanceExtensionProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkEnumerateInstanceExtensionProperties')));
  @vk.fCommands.EnumerateInstanceExtensionProperties:=addr(vkEnumerateInstanceExtensionProperties);
  @vkEnumerateDeviceLayerProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkEnumerateDeviceLayerProperties')));
  @vk.fCommands.EnumerateDeviceLayerProperties:=addr(vkEnumerateDeviceLayerProperties);
  @vkEnumerateDeviceExtensionProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkEnumerateDeviceExtensionProperties')));
  @vk.fCommands.EnumerateDeviceExtensionProperties:=addr(vkEnumerateDeviceExtensionProperties);
  @vkGetDeviceQueue:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetDeviceQueue')));
  @vk.fCommands.GetDeviceQueue:=addr(vkGetDeviceQueue);
  @vkQueueSubmit:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkQueueSubmit')));
  @vk.fCommands.QueueSubmit:=addr(vkQueueSubmit);
  @vkQueueWaitIdle:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkQueueWaitIdle')));
  @vk.fCommands.QueueWaitIdle:=addr(vkQueueWaitIdle);
  @vkDeviceWaitIdle:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDeviceWaitIdle')));
  @vk.fCommands.DeviceWaitIdle:=addr(vkDeviceWaitIdle);
  @vkAllocateMemory:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkAllocateMemory')));
  @vk.fCommands.AllocateMemory:=addr(vkAllocateMemory);
  @vkFreeMemory:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkFreeMemory')));
  @vk.fCommands.FreeMemory:=addr(vkFreeMemory);
  @vkMapMemory:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkMapMemory')));
  @vk.fCommands.MapMemory:=addr(vkMapMemory);
  @vkUnmapMemory:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkUnmapMemory')));
  @vk.fCommands.UnmapMemory:=addr(vkUnmapMemory);
  @vkFlushMappedMemoryRanges:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkFlushMappedMemoryRanges')));
  @vk.fCommands.FlushMappedMemoryRanges:=addr(vkFlushMappedMemoryRanges);
  @vkInvalidateMappedMemoryRanges:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkInvalidateMappedMemoryRanges')));
  @vk.fCommands.InvalidateMappedMemoryRanges:=addr(vkInvalidateMappedMemoryRanges);
  @vkGetDeviceMemoryCommitment:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetDeviceMemoryCommitment')));
  @vk.fCommands.GetDeviceMemoryCommitment:=addr(vkGetDeviceMemoryCommitment);
  @vkGetBufferMemoryRequirements:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetBufferMemoryRequirements')));
  @vk.fCommands.GetBufferMemoryRequirements:=addr(vkGetBufferMemoryRequirements);
  @vkBindBufferMemory:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkBindBufferMemory')));
  @vk.fCommands.BindBufferMemory:=addr(vkBindBufferMemory);
  @vkGetImageMemoryRequirements:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetImageMemoryRequirements')));
  @vk.fCommands.GetImageMemoryRequirements:=addr(vkGetImageMemoryRequirements);
  @vkBindImageMemory:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkBindImageMemory')));
  @vk.fCommands.BindImageMemory:=addr(vkBindImageMemory);
  @vkGetImageSparseMemoryRequirements:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetImageSparseMemoryRequirements')));
  @vk.fCommands.GetImageSparseMemoryRequirements:=addr(vkGetImageSparseMemoryRequirements);
  @vkGetPhysicalDeviceSparseImageFormatProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetPhysicalDeviceSparseImageFormatProperties')));
  @vk.fCommands.GetPhysicalDeviceSparseImageFormatProperties:=addr(vkGetPhysicalDeviceSparseImageFormatProperties);
  @vkQueueBindSparse:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkQueueBindSparse')));
  @vk.fCommands.QueueBindSparse:=addr(vkQueueBindSparse);
  @vkCreateFence:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateFence')));
  @vk.fCommands.CreateFence:=addr(vkCreateFence);
  @vkDestroyFence:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroyFence')));
  @vk.fCommands.DestroyFence:=addr(vkDestroyFence);
  @vkResetFences:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkResetFences')));
  @vk.fCommands.ResetFences:=addr(vkResetFences);
  @vkGetFenceStatus:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetFenceStatus')));
  @vk.fCommands.GetFenceStatus:=addr(vkGetFenceStatus);
  @vkWaitForFences:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkWaitForFences')));
  @vk.fCommands.WaitForFences:=addr(vkWaitForFences);
  @vkCreateSemaphore:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateSemaphore')));
  @vk.fCommands.CreateSemaphore:=addr(vkCreateSemaphore);
  @vkDestroySemaphore:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroySemaphore')));
  @vk.fCommands.DestroySemaphore:=addr(vkDestroySemaphore);
  @vkCreateEvent:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateEvent')));
  @vk.fCommands.CreateEvent:=addr(vkCreateEvent);
  @vkDestroyEvent:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroyEvent')));
  @vk.fCommands.DestroyEvent:=addr(vkDestroyEvent);
  @vkGetEventStatus:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetEventStatus')));
  @vk.fCommands.GetEventStatus:=addr(vkGetEventStatus);
  @vkSetEvent:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkSetEvent')));
  @vk.fCommands.SetEvent:=addr(vkSetEvent);
  @vkResetEvent:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkResetEvent')));
  @vk.fCommands.ResetEvent:=addr(vkResetEvent);
  @vkCreateQueryPool:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateQueryPool')));
  @vk.fCommands.CreateQueryPool:=addr(vkCreateQueryPool);
  @vkDestroyQueryPool:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroyQueryPool')));
  @vk.fCommands.DestroyQueryPool:=addr(vkDestroyQueryPool);
  @vkGetQueryPoolResults:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetQueryPoolResults')));
  @vk.fCommands.GetQueryPoolResults:=addr(vkGetQueryPoolResults);
  @vkCreateBuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateBuffer')));
  @vk.fCommands.CreateBuffer:=addr(vkCreateBuffer);
  @vkDestroyBuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroyBuffer')));
  @vk.fCommands.DestroyBuffer:=addr(vkDestroyBuffer);
  @vkCreateBufferView:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateBufferView')));
  @vk.fCommands.CreateBufferView:=addr(vkCreateBufferView);
  @vkDestroyBufferView:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroyBufferView')));
  @vk.fCommands.DestroyBufferView:=addr(vkDestroyBufferView);
  @vkCreateImage:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateImage')));
  @vk.fCommands.CreateImage:=addr(vkCreateImage);
  @vkDestroyImage:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroyImage')));
  @vk.fCommands.DestroyImage:=addr(vkDestroyImage);
  @vkGetImageSubresourceLayout:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetImageSubresourceLayout')));
  @vk.fCommands.GetImageSubresourceLayout:=addr(vkGetImageSubresourceLayout);
  @vkCreateImageView:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateImageView')));
  @vk.fCommands.CreateImageView:=addr(vkCreateImageView);
  @vkDestroyImageView:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroyImageView')));
  @vk.fCommands.DestroyImageView:=addr(vkDestroyImageView);
  @vkCreateShaderModule:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateShaderModule')));
  @vk.fCommands.CreateShaderModule:=addr(vkCreateShaderModule);
  @vkDestroyShaderModule:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroyShaderModule')));
  @vk.fCommands.DestroyShaderModule:=addr(vkDestroyShaderModule);
  @vkCreatePipelineCache:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreatePipelineCache')));
  @vk.fCommands.CreatePipelineCache:=addr(vkCreatePipelineCache);
  @vkDestroyPipelineCache:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroyPipelineCache')));
  @vk.fCommands.DestroyPipelineCache:=addr(vkDestroyPipelineCache);
  @vkGetPipelineCacheData:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetPipelineCacheData')));
  @vk.fCommands.GetPipelineCacheData:=addr(vkGetPipelineCacheData);
  @vkMergePipelineCaches:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkMergePipelineCaches')));
  @vk.fCommands.MergePipelineCaches:=addr(vkMergePipelineCaches);
  @vkCreateGraphicsPipelines:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateGraphicsPipelines')));
  @vk.fCommands.CreateGraphicsPipelines:=addr(vkCreateGraphicsPipelines);
  @vkCreateComputePipelines:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateComputePipelines')));
  @vk.fCommands.CreateComputePipelines:=addr(vkCreateComputePipelines);
  @vkDestroyPipeline:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroyPipeline')));
  @vk.fCommands.DestroyPipeline:=addr(vkDestroyPipeline);
  @vkCreatePipelineLayout:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreatePipelineLayout')));
  @vk.fCommands.CreatePipelineLayout:=addr(vkCreatePipelineLayout);
  @vkDestroyPipelineLayout:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroyPipelineLayout')));
  @vk.fCommands.DestroyPipelineLayout:=addr(vkDestroyPipelineLayout);
  @vkCreateSampler:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateSampler')));
  @vk.fCommands.CreateSampler:=addr(vkCreateSampler);
  @vkDestroySampler:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroySampler')));
  @vk.fCommands.DestroySampler:=addr(vkDestroySampler);
  @vkCreateDescriptorSetLayout:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateDescriptorSetLayout')));
  @vk.fCommands.CreateDescriptorSetLayout:=addr(vkCreateDescriptorSetLayout);
  @vkDestroyDescriptorSetLayout:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroyDescriptorSetLayout')));
  @vk.fCommands.DestroyDescriptorSetLayout:=addr(vkDestroyDescriptorSetLayout);
  @vkCreateDescriptorPool:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateDescriptorPool')));
  @vk.fCommands.CreateDescriptorPool:=addr(vkCreateDescriptorPool);
  @vkDestroyDescriptorPool:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroyDescriptorPool')));
  @vk.fCommands.DestroyDescriptorPool:=addr(vkDestroyDescriptorPool);
  @vkResetDescriptorPool:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkResetDescriptorPool')));
  @vk.fCommands.ResetDescriptorPool:=addr(vkResetDescriptorPool);
  @vkAllocateDescriptorSets:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkAllocateDescriptorSets')));
  @vk.fCommands.AllocateDescriptorSets:=addr(vkAllocateDescriptorSets);
  @vkFreeDescriptorSets:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkFreeDescriptorSets')));
  @vk.fCommands.FreeDescriptorSets:=addr(vkFreeDescriptorSets);
  @vkUpdateDescriptorSets:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkUpdateDescriptorSets')));
  @vk.fCommands.UpdateDescriptorSets:=addr(vkUpdateDescriptorSets);
  @vkCreateFramebuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateFramebuffer')));
  @vk.fCommands.CreateFramebuffer:=addr(vkCreateFramebuffer);
  @vkDestroyFramebuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroyFramebuffer')));
  @vk.fCommands.DestroyFramebuffer:=addr(vkDestroyFramebuffer);
  @vkCreateRenderPass:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateRenderPass')));
  @vk.fCommands.CreateRenderPass:=addr(vkCreateRenderPass);
  @vkDestroyRenderPass:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroyRenderPass')));
  @vk.fCommands.DestroyRenderPass:=addr(vkDestroyRenderPass);
  @vkGetRenderAreaGranularity:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetRenderAreaGranularity')));
  @vk.fCommands.GetRenderAreaGranularity:=addr(vkGetRenderAreaGranularity);
  @vkCreateCommandPool:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateCommandPool')));
  @vk.fCommands.CreateCommandPool:=addr(vkCreateCommandPool);
  @vkDestroyCommandPool:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroyCommandPool')));
  @vk.fCommands.DestroyCommandPool:=addr(vkDestroyCommandPool);
  @vkResetCommandPool:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkResetCommandPool')));
  @vk.fCommands.ResetCommandPool:=addr(vkResetCommandPool);
  @vkAllocateCommandBuffers:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkAllocateCommandBuffers')));
  @vk.fCommands.AllocateCommandBuffers:=addr(vkAllocateCommandBuffers);
  @vkFreeCommandBuffers:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkFreeCommandBuffers')));
  @vk.fCommands.FreeCommandBuffers:=addr(vkFreeCommandBuffers);
  @vkBeginCommandBuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkBeginCommandBuffer')));
  @vk.fCommands.BeginCommandBuffer:=addr(vkBeginCommandBuffer);
  @vkEndCommandBuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkEndCommandBuffer')));
  @vk.fCommands.EndCommandBuffer:=addr(vkEndCommandBuffer);
  @vkResetCommandBuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkResetCommandBuffer')));
  @vk.fCommands.ResetCommandBuffer:=addr(vkResetCommandBuffer);
  @vkCmdBindPipeline:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdBindPipeline')));
  @vk.fCommands.CmdBindPipeline:=addr(vkCmdBindPipeline);
  @vkCmdSetViewport:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdSetViewport')));
  @vk.fCommands.CmdSetViewport:=addr(vkCmdSetViewport);
  @vkCmdSetScissor:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdSetScissor')));
  @vk.fCommands.CmdSetScissor:=addr(vkCmdSetScissor);
  @vkCmdSetLineWidth:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdSetLineWidth')));
  @vk.fCommands.CmdSetLineWidth:=addr(vkCmdSetLineWidth);
  @vkCmdSetDepthBias:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdSetDepthBias')));
  @vk.fCommands.CmdSetDepthBias:=addr(vkCmdSetDepthBias);
  @vkCmdSetBlendConstants:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdSetBlendConstants')));
  @vk.fCommands.CmdSetBlendConstants:=addr(vkCmdSetBlendConstants);
  @vkCmdSetDepthBounds:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdSetDepthBounds')));
  @vk.fCommands.CmdSetDepthBounds:=addr(vkCmdSetDepthBounds);
  @vkCmdSetStencilCompareMask:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdSetStencilCompareMask')));
  @vk.fCommands.CmdSetStencilCompareMask:=addr(vkCmdSetStencilCompareMask);
  @vkCmdSetStencilWriteMask:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdSetStencilWriteMask')));
  @vk.fCommands.CmdSetStencilWriteMask:=addr(vkCmdSetStencilWriteMask);
  @vkCmdSetStencilReference:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdSetStencilReference')));
  @vk.fCommands.CmdSetStencilReference:=addr(vkCmdSetStencilReference);
  @vkCmdBindDescriptorSets:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdBindDescriptorSets')));
  @vk.fCommands.CmdBindDescriptorSets:=addr(vkCmdBindDescriptorSets);
  @vkCmdBindIndexBuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdBindIndexBuffer')));
  @vk.fCommands.CmdBindIndexBuffer:=addr(vkCmdBindIndexBuffer);
  @vkCmdBindVertexBuffers:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdBindVertexBuffers')));
  @vk.fCommands.CmdBindVertexBuffers:=addr(vkCmdBindVertexBuffers);
  @vkCmdDraw:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdDraw')));
  @vk.fCommands.CmdDraw:=addr(vkCmdDraw);
  @vkCmdDrawIndexed:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdDrawIndexed')));
  @vk.fCommands.CmdDrawIndexed:=addr(vkCmdDrawIndexed);
  @vkCmdDrawIndirect:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdDrawIndirect')));
  @vk.fCommands.CmdDrawIndirect:=addr(vkCmdDrawIndirect);
  @vkCmdDrawIndexedIndirect:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdDrawIndexedIndirect')));
  @vk.fCommands.CmdDrawIndexedIndirect:=addr(vkCmdDrawIndexedIndirect);
  @vkCmdDispatch:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdDispatch')));
  @vk.fCommands.CmdDispatch:=addr(vkCmdDispatch);
  @vkCmdDispatchIndirect:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdDispatchIndirect')));
  @vk.fCommands.CmdDispatchIndirect:=addr(vkCmdDispatchIndirect);
  @vkCmdCopyBuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdCopyBuffer')));
  @vk.fCommands.CmdCopyBuffer:=addr(vkCmdCopyBuffer);
  @vkCmdCopyImage:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdCopyImage')));
  @vk.fCommands.CmdCopyImage:=addr(vkCmdCopyImage);
  @vkCmdBlitImage:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdBlitImage')));
  @vk.fCommands.CmdBlitImage:=addr(vkCmdBlitImage);
  @vkCmdCopyBufferToImage:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdCopyBufferToImage')));
  @vk.fCommands.CmdCopyBufferToImage:=addr(vkCmdCopyBufferToImage);
  @vkCmdCopyImageToBuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdCopyImageToBuffer')));
  @vk.fCommands.CmdCopyImageToBuffer:=addr(vkCmdCopyImageToBuffer);
  @vkCmdUpdateBuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdUpdateBuffer')));
  @vk.fCommands.CmdUpdateBuffer:=addr(vkCmdUpdateBuffer);
  @vkCmdFillBuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdFillBuffer')));
  @vk.fCommands.CmdFillBuffer:=addr(vkCmdFillBuffer);
  @vkCmdClearColorImage:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdClearColorImage')));
  @vk.fCommands.CmdClearColorImage:=addr(vkCmdClearColorImage);
  @vkCmdClearDepthStencilImage:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdClearDepthStencilImage')));
  @vk.fCommands.CmdClearDepthStencilImage:=addr(vkCmdClearDepthStencilImage);
  @vkCmdClearAttachments:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdClearAttachments')));
  @vk.fCommands.CmdClearAttachments:=addr(vkCmdClearAttachments);
  @vkCmdResolveImage:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdResolveImage')));
  @vk.fCommands.CmdResolveImage:=addr(vkCmdResolveImage);
  @vkCmdSetEvent:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdSetEvent')));
  @vk.fCommands.CmdSetEvent:=addr(vkCmdSetEvent);
  @vkCmdResetEvent:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdResetEvent')));
  @vk.fCommands.CmdResetEvent:=addr(vkCmdResetEvent);
  @vkCmdWaitEvents:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdWaitEvents')));
  @vk.fCommands.CmdWaitEvents:=addr(vkCmdWaitEvents);
  @vkCmdPipelineBarrier:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdPipelineBarrier')));
  @vk.fCommands.CmdPipelineBarrier:=addr(vkCmdPipelineBarrier);
  @vkCmdBeginQuery:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdBeginQuery')));
  @vk.fCommands.CmdBeginQuery:=addr(vkCmdBeginQuery);
  @vkCmdEndQuery:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdEndQuery')));
  @vk.fCommands.CmdEndQuery:=addr(vkCmdEndQuery);
  @vkCmdResetQueryPool:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdResetQueryPool')));
  @vk.fCommands.CmdResetQueryPool:=addr(vkCmdResetQueryPool);
  @vkCmdWriteTimestamp:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdWriteTimestamp')));
  @vk.fCommands.CmdWriteTimestamp:=addr(vkCmdWriteTimestamp);
  @vkCmdCopyQueryPoolResults:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdCopyQueryPoolResults')));
  @vk.fCommands.CmdCopyQueryPoolResults:=addr(vkCmdCopyQueryPoolResults);
  @vkCmdPushConstants:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdPushConstants')));
  @vk.fCommands.CmdPushConstants:=addr(vkCmdPushConstants);
  @vkCmdBeginRenderPass:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdBeginRenderPass')));
  @vk.fCommands.CmdBeginRenderPass:=addr(vkCmdBeginRenderPass);
  @vkCmdNextSubpass:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdNextSubpass')));
  @vk.fCommands.CmdNextSubpass:=addr(vkCmdNextSubpass);
  @vkCmdEndRenderPass:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdEndRenderPass')));
  @vk.fCommands.CmdEndRenderPass:=addr(vkCmdEndRenderPass);
  @vkCmdExecuteCommands:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCmdExecuteCommands')));
  @vk.fCommands.CmdExecuteCommands:=addr(vkCmdExecuteCommands);
{$ifdef Android}
  @vkCreateAndroidSurfaceKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateAndroidSurfaceKHR')));
  @vk.fCommands.CreateAndroidSurfaceKHR:=addr(vkCreateAndroidSurfaceKHR);
{$endif}
  @vkGetPhysicalDeviceDisplayPropertiesKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetPhysicalDeviceDisplayPropertiesKHR')));
  @vk.fCommands.GetPhysicalDeviceDisplayPropertiesKHR:=addr(vkGetPhysicalDeviceDisplayPropertiesKHR);
  @vkGetPhysicalDeviceDisplayPlanePropertiesKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetPhysicalDeviceDisplayPlanePropertiesKHR')));
  @vk.fCommands.GetPhysicalDeviceDisplayPlanePropertiesKHR:=addr(vkGetPhysicalDeviceDisplayPlanePropertiesKHR);
  @vkGetDisplayPlaneSupportedDisplaysKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetDisplayPlaneSupportedDisplaysKHR')));
  @vk.fCommands.GetDisplayPlaneSupportedDisplaysKHR:=addr(vkGetDisplayPlaneSupportedDisplaysKHR);
  @vkGetDisplayModePropertiesKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetDisplayModePropertiesKHR')));
  @vk.fCommands.GetDisplayModePropertiesKHR:=addr(vkGetDisplayModePropertiesKHR);
  @vkCreateDisplayModeKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateDisplayModeKHR')));
  @vk.fCommands.CreateDisplayModeKHR:=addr(vkCreateDisplayModeKHR);
  @vkGetDisplayPlaneCapabilitiesKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetDisplayPlaneCapabilitiesKHR')));
  @vk.fCommands.GetDisplayPlaneCapabilitiesKHR:=addr(vkGetDisplayPlaneCapabilitiesKHR);
  @vkCreateDisplayPlaneSurfaceKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateDisplayPlaneSurfaceKHR')));
  @vk.fCommands.CreateDisplayPlaneSurfaceKHR:=addr(vkCreateDisplayPlaneSurfaceKHR);
  @vkCreateSharedSwapchainsKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateSharedSwapchainsKHR')));
  @vk.fCommands.CreateSharedSwapchainsKHR:=addr(vkCreateSharedSwapchainsKHR);
{$ifdef Mir}
  @vkCreateMirSurfaceKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateMirSurfaceKHR')));
  @vk.fCommands.CreateMirSurfaceKHR:=addr(vkCreateMirSurfaceKHR);
{$endif}
{$ifdef Mir}
  @vkGetPhysicalDeviceMirPresentationSupportKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetPhysicalDeviceMirPresentationSupportKHR')));
  @vk.fCommands.GetPhysicalDeviceMirPresentationSupportKHR:=addr(vkGetPhysicalDeviceMirPresentationSupportKHR);
{$endif}
  @vkDestroySurfaceKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroySurfaceKHR')));
  @vk.fCommands.DestroySurfaceKHR:=addr(vkDestroySurfaceKHR);
  @vkGetPhysicalDeviceSurfaceSupportKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetPhysicalDeviceSurfaceSupportKHR')));
  @vk.fCommands.GetPhysicalDeviceSurfaceSupportKHR:=addr(vkGetPhysicalDeviceSurfaceSupportKHR);
  @vkGetPhysicalDeviceSurfaceCapabilitiesKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetPhysicalDeviceSurfaceCapabilitiesKHR')));
  @vk.fCommands.GetPhysicalDeviceSurfaceCapabilitiesKHR:=addr(vkGetPhysicalDeviceSurfaceCapabilitiesKHR);
  @vkGetPhysicalDeviceSurfaceFormatsKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetPhysicalDeviceSurfaceFormatsKHR')));
  @vk.fCommands.GetPhysicalDeviceSurfaceFormatsKHR:=addr(vkGetPhysicalDeviceSurfaceFormatsKHR);
  @vkGetPhysicalDeviceSurfacePresentModesKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetPhysicalDeviceSurfacePresentModesKHR')));
  @vk.fCommands.GetPhysicalDeviceSurfacePresentModesKHR:=addr(vkGetPhysicalDeviceSurfacePresentModesKHR);
  @vkCreateSwapchainKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateSwapchainKHR')));
  @vk.fCommands.CreateSwapchainKHR:=addr(vkCreateSwapchainKHR);
  @vkDestroySwapchainKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroySwapchainKHR')));
  @vk.fCommands.DestroySwapchainKHR:=addr(vkDestroySwapchainKHR);
  @vkGetSwapchainImagesKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetSwapchainImagesKHR')));
  @vk.fCommands.GetSwapchainImagesKHR:=addr(vkGetSwapchainImagesKHR);
  @vkAcquireNextImageKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkAcquireNextImageKHR')));
  @vk.fCommands.AcquireNextImageKHR:=addr(vkAcquireNextImageKHR);
  @vkQueuePresentKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkQueuePresentKHR')));
  @vk.fCommands.QueuePresentKHR:=addr(vkQueuePresentKHR);
{$ifdef Wayland}
  @vkCreateWaylandSurfaceKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateWaylandSurfaceKHR')));
  @vk.fCommands.CreateWaylandSurfaceKHR:=addr(vkCreateWaylandSurfaceKHR);
{$endif}
{$ifdef Wayland}
  @vkGetPhysicalDeviceWaylandPresentationSupportKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetPhysicalDeviceWaylandPresentationSupportKHR')));
  @vk.fCommands.GetPhysicalDeviceWaylandPresentationSupportKHR:=addr(vkGetPhysicalDeviceWaylandPresentationSupportKHR);
{$endif}
  @vkCreateWin32SurfaceKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateWin32SurfaceKHR')));
  @vk.fCommands.CreateWin32SurfaceKHR:=addr(vkCreateWin32SurfaceKHR);
  @vkGetPhysicalDeviceWin32PresentationSupportKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetPhysicalDeviceWin32PresentationSupportKHR')));
  @vk.fCommands.GetPhysicalDeviceWin32PresentationSupportKHR:=addr(vkGetPhysicalDeviceWin32PresentationSupportKHR);
{$ifdef X11}
  @vkCreateXlibSurfaceKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateXlibSurfaceKHR')));
  @vk.fCommands.CreateXlibSurfaceKHR:=addr(vkCreateXlibSurfaceKHR);
{$endif}
{$ifdef X11}
  @vkGetPhysicalDeviceXlibPresentationSupportKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetPhysicalDeviceXlibPresentationSupportKHR')));
  @vk.fCommands.GetPhysicalDeviceXlibPresentationSupportKHR:=addr(vkGetPhysicalDeviceXlibPresentationSupportKHR);
{$endif}
{$ifdef XCB}
  @vkCreateXcbSurfaceKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateXcbSurfaceKHR')));
  @vk.fCommands.CreateXcbSurfaceKHR:=addr(vkCreateXcbSurfaceKHR);
{$endif}
{$ifdef XCB}
  @vkGetPhysicalDeviceXcbPresentationSupportKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkGetPhysicalDeviceXcbPresentationSupportKHR')));
  @vk.fCommands.GetPhysicalDeviceXcbPresentationSupportKHR:=addr(vkGetPhysicalDeviceXcbPresentationSupportKHR);
{$endif}
  @vkCreateDebugReportCallbackEXT:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateDebugReportCallbackEXT')));
  @vk.fCommands.CreateDebugReportCallbackEXT:=addr(vkCreateDebugReportCallbackEXT);
  @vkDestroyDebugReportCallbackEXT:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDestroyDebugReportCallbackEXT')));
  @vk.fCommands.DestroyDebugReportCallbackEXT:=addr(vkDestroyDebugReportCallbackEXT);
  @vkDebugReportMessageEXT:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkDebugReportMessageEXT')));
  @vk.fCommands.DebugReportMessageEXT:=addr(vkDebugReportMessageEXT);
 end;
end;

function LoadVulkanInstanceCommands(const GetInstanceProcAddr:TvkGetInstanceProcAddr;const Instance:TVkInstance;out InstanceCommands:TVulkanCommands):boolean;
begin
 FillChar(InstanceCommands,SizeOf(TVulkanCommands),#0);
 result:=assigned(GetInstanceProcAddr);
 if result then begin
  @InstanceCommands.CreateInstance:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateInstance')));
  @InstanceCommands.DestroyInstance:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroyInstance')));
  @InstanceCommands.EnumeratePhysicalDevices:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkEnumeratePhysicalDevices')));
  @InstanceCommands.GetDeviceProcAddr:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetDeviceProcAddr')));
  @InstanceCommands.GetInstanceProcAddr:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetInstanceProcAddr')));
  @InstanceCommands.GetPhysicalDeviceProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceProperties')));
  @InstanceCommands.GetPhysicalDeviceQueueFamilyProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceQueueFamilyProperties')));
  @InstanceCommands.GetPhysicalDeviceMemoryProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceMemoryProperties')));
  @InstanceCommands.GetPhysicalDeviceFeatures:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceFeatures')));
  @InstanceCommands.GetPhysicalDeviceFormatProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceFormatProperties')));
  @InstanceCommands.GetPhysicalDeviceImageFormatProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceImageFormatProperties')));
  @InstanceCommands.CreateDevice:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateDevice')));
  @InstanceCommands.DestroyDevice:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroyDevice')));
  @InstanceCommands.EnumerateInstanceLayerProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkEnumerateInstanceLayerProperties')));
  @InstanceCommands.EnumerateInstanceExtensionProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkEnumerateInstanceExtensionProperties')));
  @InstanceCommands.EnumerateDeviceLayerProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkEnumerateDeviceLayerProperties')));
  @InstanceCommands.EnumerateDeviceExtensionProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkEnumerateDeviceExtensionProperties')));
  @InstanceCommands.GetDeviceQueue:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetDeviceQueue')));
  @InstanceCommands.QueueSubmit:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkQueueSubmit')));
  @InstanceCommands.QueueWaitIdle:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkQueueWaitIdle')));
  @InstanceCommands.DeviceWaitIdle:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDeviceWaitIdle')));
  @InstanceCommands.AllocateMemory:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkAllocateMemory')));
  @InstanceCommands.FreeMemory:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkFreeMemory')));
  @InstanceCommands.MapMemory:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkMapMemory')));
  @InstanceCommands.UnmapMemory:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkUnmapMemory')));
  @InstanceCommands.FlushMappedMemoryRanges:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkFlushMappedMemoryRanges')));
  @InstanceCommands.InvalidateMappedMemoryRanges:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkInvalidateMappedMemoryRanges')));
  @InstanceCommands.GetDeviceMemoryCommitment:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetDeviceMemoryCommitment')));
  @InstanceCommands.GetBufferMemoryRequirements:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetBufferMemoryRequirements')));
  @InstanceCommands.BindBufferMemory:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkBindBufferMemory')));
  @InstanceCommands.GetImageMemoryRequirements:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetImageMemoryRequirements')));
  @InstanceCommands.BindImageMemory:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkBindImageMemory')));
  @InstanceCommands.GetImageSparseMemoryRequirements:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetImageSparseMemoryRequirements')));
  @InstanceCommands.GetPhysicalDeviceSparseImageFormatProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceSparseImageFormatProperties')));
  @InstanceCommands.QueueBindSparse:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkQueueBindSparse')));
  @InstanceCommands.CreateFence:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateFence')));
  @InstanceCommands.DestroyFence:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroyFence')));
  @InstanceCommands.ResetFences:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkResetFences')));
  @InstanceCommands.GetFenceStatus:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetFenceStatus')));
  @InstanceCommands.WaitForFences:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkWaitForFences')));
  @InstanceCommands.CreateSemaphore:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateSemaphore')));
  @InstanceCommands.DestroySemaphore:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroySemaphore')));
  @InstanceCommands.CreateEvent:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateEvent')));
  @InstanceCommands.DestroyEvent:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroyEvent')));
  @InstanceCommands.GetEventStatus:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetEventStatus')));
  @InstanceCommands.SetEvent:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkSetEvent')));
  @InstanceCommands.ResetEvent:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkResetEvent')));
  @InstanceCommands.CreateQueryPool:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateQueryPool')));
  @InstanceCommands.DestroyQueryPool:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroyQueryPool')));
  @InstanceCommands.GetQueryPoolResults:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetQueryPoolResults')));
  @InstanceCommands.CreateBuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateBuffer')));
  @InstanceCommands.DestroyBuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroyBuffer')));
  @InstanceCommands.CreateBufferView:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateBufferView')));
  @InstanceCommands.DestroyBufferView:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroyBufferView')));
  @InstanceCommands.CreateImage:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateImage')));
  @InstanceCommands.DestroyImage:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroyImage')));
  @InstanceCommands.GetImageSubresourceLayout:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetImageSubresourceLayout')));
  @InstanceCommands.CreateImageView:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateImageView')));
  @InstanceCommands.DestroyImageView:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroyImageView')));
  @InstanceCommands.CreateShaderModule:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateShaderModule')));
  @InstanceCommands.DestroyShaderModule:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroyShaderModule')));
  @InstanceCommands.CreatePipelineCache:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreatePipelineCache')));
  @InstanceCommands.DestroyPipelineCache:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroyPipelineCache')));
  @InstanceCommands.GetPipelineCacheData:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPipelineCacheData')));
  @InstanceCommands.MergePipelineCaches:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkMergePipelineCaches')));
  @InstanceCommands.CreateGraphicsPipelines:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateGraphicsPipelines')));
  @InstanceCommands.CreateComputePipelines:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateComputePipelines')));
  @InstanceCommands.DestroyPipeline:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroyPipeline')));
  @InstanceCommands.CreatePipelineLayout:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreatePipelineLayout')));
  @InstanceCommands.DestroyPipelineLayout:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroyPipelineLayout')));
  @InstanceCommands.CreateSampler:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateSampler')));
  @InstanceCommands.DestroySampler:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroySampler')));
  @InstanceCommands.CreateDescriptorSetLayout:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateDescriptorSetLayout')));
  @InstanceCommands.DestroyDescriptorSetLayout:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroyDescriptorSetLayout')));
  @InstanceCommands.CreateDescriptorPool:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateDescriptorPool')));
  @InstanceCommands.DestroyDescriptorPool:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroyDescriptorPool')));
  @InstanceCommands.ResetDescriptorPool:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkResetDescriptorPool')));
  @InstanceCommands.AllocateDescriptorSets:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkAllocateDescriptorSets')));
  @InstanceCommands.FreeDescriptorSets:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkFreeDescriptorSets')));
  @InstanceCommands.UpdateDescriptorSets:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkUpdateDescriptorSets')));
  @InstanceCommands.CreateFramebuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateFramebuffer')));
  @InstanceCommands.DestroyFramebuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroyFramebuffer')));
  @InstanceCommands.CreateRenderPass:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateRenderPass')));
  @InstanceCommands.DestroyRenderPass:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroyRenderPass')));
  @InstanceCommands.GetRenderAreaGranularity:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetRenderAreaGranularity')));
  @InstanceCommands.CreateCommandPool:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateCommandPool')));
  @InstanceCommands.DestroyCommandPool:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroyCommandPool')));
  @InstanceCommands.ResetCommandPool:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkResetCommandPool')));
  @InstanceCommands.AllocateCommandBuffers:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkAllocateCommandBuffers')));
  @InstanceCommands.FreeCommandBuffers:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkFreeCommandBuffers')));
  @InstanceCommands.BeginCommandBuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkBeginCommandBuffer')));
  @InstanceCommands.EndCommandBuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkEndCommandBuffer')));
  @InstanceCommands.ResetCommandBuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkResetCommandBuffer')));
  @InstanceCommands.CmdBindPipeline:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdBindPipeline')));
  @InstanceCommands.CmdSetViewport:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdSetViewport')));
  @InstanceCommands.CmdSetScissor:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdSetScissor')));
  @InstanceCommands.CmdSetLineWidth:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdSetLineWidth')));
  @InstanceCommands.CmdSetDepthBias:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdSetDepthBias')));
  @InstanceCommands.CmdSetBlendConstants:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdSetBlendConstants')));
  @InstanceCommands.CmdSetDepthBounds:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdSetDepthBounds')));
  @InstanceCommands.CmdSetStencilCompareMask:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdSetStencilCompareMask')));
  @InstanceCommands.CmdSetStencilWriteMask:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdSetStencilWriteMask')));
  @InstanceCommands.CmdSetStencilReference:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdSetStencilReference')));
  @InstanceCommands.CmdBindDescriptorSets:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdBindDescriptorSets')));
  @InstanceCommands.CmdBindIndexBuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdBindIndexBuffer')));
  @InstanceCommands.CmdBindVertexBuffers:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdBindVertexBuffers')));
  @InstanceCommands.CmdDraw:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdDraw')));
  @InstanceCommands.CmdDrawIndexed:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdDrawIndexed')));
  @InstanceCommands.CmdDrawIndirect:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdDrawIndirect')));
  @InstanceCommands.CmdDrawIndexedIndirect:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdDrawIndexedIndirect')));
  @InstanceCommands.CmdDispatch:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdDispatch')));
  @InstanceCommands.CmdDispatchIndirect:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdDispatchIndirect')));
  @InstanceCommands.CmdCopyBuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdCopyBuffer')));
  @InstanceCommands.CmdCopyImage:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdCopyImage')));
  @InstanceCommands.CmdBlitImage:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdBlitImage')));
  @InstanceCommands.CmdCopyBufferToImage:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdCopyBufferToImage')));
  @InstanceCommands.CmdCopyImageToBuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdCopyImageToBuffer')));
  @InstanceCommands.CmdUpdateBuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdUpdateBuffer')));
  @InstanceCommands.CmdFillBuffer:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdFillBuffer')));
  @InstanceCommands.CmdClearColorImage:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdClearColorImage')));
  @InstanceCommands.CmdClearDepthStencilImage:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdClearDepthStencilImage')));
  @InstanceCommands.CmdClearAttachments:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdClearAttachments')));
  @InstanceCommands.CmdResolveImage:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdResolveImage')));
  @InstanceCommands.CmdSetEvent:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdSetEvent')));
  @InstanceCommands.CmdResetEvent:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdResetEvent')));
  @InstanceCommands.CmdWaitEvents:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdWaitEvents')));
  @InstanceCommands.CmdPipelineBarrier:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdPipelineBarrier')));
  @InstanceCommands.CmdBeginQuery:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdBeginQuery')));
  @InstanceCommands.CmdEndQuery:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdEndQuery')));
  @InstanceCommands.CmdResetQueryPool:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdResetQueryPool')));
  @InstanceCommands.CmdWriteTimestamp:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdWriteTimestamp')));
  @InstanceCommands.CmdCopyQueryPoolResults:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdCopyQueryPoolResults')));
  @InstanceCommands.CmdPushConstants:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdPushConstants')));
  @InstanceCommands.CmdBeginRenderPass:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdBeginRenderPass')));
  @InstanceCommands.CmdNextSubpass:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdNextSubpass')));
  @InstanceCommands.CmdEndRenderPass:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdEndRenderPass')));
  @InstanceCommands.CmdExecuteCommands:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdExecuteCommands')));
{$ifdef Android}
  @InstanceCommands.CreateAndroidSurfaceKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateAndroidSurfaceKHR')));
{$endif}
  @InstanceCommands.GetPhysicalDeviceDisplayPropertiesKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceDisplayPropertiesKHR')));
  @InstanceCommands.GetPhysicalDeviceDisplayPlanePropertiesKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceDisplayPlanePropertiesKHR')));
  @InstanceCommands.GetDisplayPlaneSupportedDisplaysKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetDisplayPlaneSupportedDisplaysKHR')));
  @InstanceCommands.GetDisplayModePropertiesKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetDisplayModePropertiesKHR')));
  @InstanceCommands.CreateDisplayModeKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateDisplayModeKHR')));
  @InstanceCommands.GetDisplayPlaneCapabilitiesKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetDisplayPlaneCapabilitiesKHR')));
  @InstanceCommands.CreateDisplayPlaneSurfaceKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateDisplayPlaneSurfaceKHR')));
  @InstanceCommands.CreateSharedSwapchainsKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateSharedSwapchainsKHR')));
{$ifdef Mir}
  @InstanceCommands.CreateMirSurfaceKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateMirSurfaceKHR')));
{$endif}
{$ifdef Mir}
  @InstanceCommands.GetPhysicalDeviceMirPresentationSupportKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceMirPresentationSupportKHR')));
{$endif}
  @InstanceCommands.DestroySurfaceKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroySurfaceKHR')));
  @InstanceCommands.GetPhysicalDeviceSurfaceSupportKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceSurfaceSupportKHR')));
  @InstanceCommands.GetPhysicalDeviceSurfaceCapabilitiesKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceSurfaceCapabilitiesKHR')));
  @InstanceCommands.GetPhysicalDeviceSurfaceFormatsKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceSurfaceFormatsKHR')));
  @InstanceCommands.GetPhysicalDeviceSurfacePresentModesKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceSurfacePresentModesKHR')));
  @InstanceCommands.CreateSwapchainKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateSwapchainKHR')));
  @InstanceCommands.DestroySwapchainKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroySwapchainKHR')));
  @InstanceCommands.GetSwapchainImagesKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetSwapchainImagesKHR')));
  @InstanceCommands.AcquireNextImageKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkAcquireNextImageKHR')));
  @InstanceCommands.QueuePresentKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkQueuePresentKHR')));
{$ifdef Wayland}
  @InstanceCommands.CreateWaylandSurfaceKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateWaylandSurfaceKHR')));
{$endif}
{$ifdef Wayland}
  @InstanceCommands.GetPhysicalDeviceWaylandPresentationSupportKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceWaylandPresentationSupportKHR')));
{$endif}
  @InstanceCommands.CreateWin32SurfaceKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateWin32SurfaceKHR')));
  @InstanceCommands.GetPhysicalDeviceWin32PresentationSupportKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceWin32PresentationSupportKHR')));
{$ifdef X11}
  @InstanceCommands.CreateXlibSurfaceKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateXlibSurfaceKHR')));
{$endif}
{$ifdef X11}
  @InstanceCommands.GetPhysicalDeviceXlibPresentationSupportKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceXlibPresentationSupportKHR')));
{$endif}
{$ifdef XCB}
  @InstanceCommands.CreateXcbSurfaceKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateXcbSurfaceKHR')));
{$endif}
{$ifdef XCB}
  @InstanceCommands.GetPhysicalDeviceXcbPresentationSupportKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceXcbPresentationSupportKHR')));
{$endif}
  @InstanceCommands.CreateDebugReportCallbackEXT:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateDebugReportCallbackEXT')));
  @InstanceCommands.DestroyDebugReportCallbackEXT:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroyDebugReportCallbackEXT')));
  @InstanceCommands.DebugReportMessageEXT:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDebugReportMessageEXT')));
 end;
end;

function LoadVulkanDeviceCommands(const GetDeviceProcAddr:TvkGetDeviceProcAddr;const Device:TVkDevice;out DeviceCommands:TVulkanCommands):boolean; overload;
begin
 FillChar(DeviceCommands,SizeOf(TVulkanCommands),#0);
 result:=assigned(GetDeviceProcAddr);
 if result then begin
  // Device commands of any Vulkan command whose first parameter is one of: vkDevice, VkQueue, VkCommandBuffer
  @DeviceCommands.GetDeviceProcAddr:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetDeviceProcAddr')));
  @DeviceCommands.DestroyDevice:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyDevice')));
  @DeviceCommands.GetDeviceQueue:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetDeviceQueue')));
  @DeviceCommands.QueueSubmit:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkQueueSubmit')));
  @DeviceCommands.QueueWaitIdle:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkQueueWaitIdle')));
  @DeviceCommands.DeviceWaitIdle:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDeviceWaitIdle')));
  @DeviceCommands.AllocateMemory:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkAllocateMemory')));
  @DeviceCommands.FreeMemory:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkFreeMemory')));
  @DeviceCommands.MapMemory:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkMapMemory')));
  @DeviceCommands.UnmapMemory:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkUnmapMemory')));
  @DeviceCommands.FlushMappedMemoryRanges:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkFlushMappedMemoryRanges')));
  @DeviceCommands.InvalidateMappedMemoryRanges:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkInvalidateMappedMemoryRanges')));
  @DeviceCommands.GetDeviceMemoryCommitment:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetDeviceMemoryCommitment')));
  @DeviceCommands.GetBufferMemoryRequirements:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetBufferMemoryRequirements')));
  @DeviceCommands.BindBufferMemory:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkBindBufferMemory')));
  @DeviceCommands.GetImageMemoryRequirements:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetImageMemoryRequirements')));
  @DeviceCommands.BindImageMemory:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkBindImageMemory')));
  @DeviceCommands.GetImageSparseMemoryRequirements:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetImageSparseMemoryRequirements')));
  @DeviceCommands.QueueBindSparse:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkQueueBindSparse')));
  @DeviceCommands.CreateFence:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateFence')));
  @DeviceCommands.DestroyFence:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyFence')));
  @DeviceCommands.ResetFences:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkResetFences')));
  @DeviceCommands.GetFenceStatus:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetFenceStatus')));
  @DeviceCommands.WaitForFences:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkWaitForFences')));
  @DeviceCommands.CreateSemaphore:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateSemaphore')));
  @DeviceCommands.DestroySemaphore:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroySemaphore')));
  @DeviceCommands.CreateEvent:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateEvent')));
  @DeviceCommands.DestroyEvent:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyEvent')));
  @DeviceCommands.GetEventStatus:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetEventStatus')));
  @DeviceCommands.SetEvent:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkSetEvent')));
  @DeviceCommands.ResetEvent:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkResetEvent')));
  @DeviceCommands.CreateQueryPool:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateQueryPool')));
  @DeviceCommands.DestroyQueryPool:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyQueryPool')));
  @DeviceCommands.GetQueryPoolResults:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetQueryPoolResults')));
  @DeviceCommands.CreateBuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateBuffer')));
  @DeviceCommands.DestroyBuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyBuffer')));
  @DeviceCommands.CreateBufferView:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateBufferView')));
  @DeviceCommands.DestroyBufferView:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyBufferView')));
  @DeviceCommands.CreateImage:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateImage')));
  @DeviceCommands.DestroyImage:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyImage')));
  @DeviceCommands.GetImageSubresourceLayout:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetImageSubresourceLayout')));
  @DeviceCommands.CreateImageView:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateImageView')));
  @DeviceCommands.DestroyImageView:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyImageView')));
  @DeviceCommands.CreateShaderModule:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateShaderModule')));
  @DeviceCommands.DestroyShaderModule:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyShaderModule')));
  @DeviceCommands.CreatePipelineCache:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreatePipelineCache')));
  @DeviceCommands.DestroyPipelineCache:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyPipelineCache')));
  @DeviceCommands.GetPipelineCacheData:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetPipelineCacheData')));
  @DeviceCommands.MergePipelineCaches:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkMergePipelineCaches')));
  @DeviceCommands.CreateGraphicsPipelines:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateGraphicsPipelines')));
  @DeviceCommands.CreateComputePipelines:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateComputePipelines')));
  @DeviceCommands.DestroyPipeline:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyPipeline')));
  @DeviceCommands.CreatePipelineLayout:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreatePipelineLayout')));
  @DeviceCommands.DestroyPipelineLayout:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyPipelineLayout')));
  @DeviceCommands.CreateSampler:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateSampler')));
  @DeviceCommands.DestroySampler:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroySampler')));
  @DeviceCommands.CreateDescriptorSetLayout:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateDescriptorSetLayout')));
  @DeviceCommands.DestroyDescriptorSetLayout:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyDescriptorSetLayout')));
  @DeviceCommands.CreateDescriptorPool:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateDescriptorPool')));
  @DeviceCommands.DestroyDescriptorPool:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyDescriptorPool')));
  @DeviceCommands.ResetDescriptorPool:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkResetDescriptorPool')));
  @DeviceCommands.AllocateDescriptorSets:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkAllocateDescriptorSets')));
  @DeviceCommands.FreeDescriptorSets:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkFreeDescriptorSets')));
  @DeviceCommands.UpdateDescriptorSets:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkUpdateDescriptorSets')));
  @DeviceCommands.CreateFramebuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateFramebuffer')));
  @DeviceCommands.DestroyFramebuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyFramebuffer')));
  @DeviceCommands.CreateRenderPass:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateRenderPass')));
  @DeviceCommands.DestroyRenderPass:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyRenderPass')));
  @DeviceCommands.GetRenderAreaGranularity:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetRenderAreaGranularity')));
  @DeviceCommands.CreateCommandPool:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateCommandPool')));
  @DeviceCommands.DestroyCommandPool:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyCommandPool')));
  @DeviceCommands.ResetCommandPool:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkResetCommandPool')));
  @DeviceCommands.AllocateCommandBuffers:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkAllocateCommandBuffers')));
  @DeviceCommands.FreeCommandBuffers:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkFreeCommandBuffers')));
  @DeviceCommands.BeginCommandBuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkBeginCommandBuffer')));
  @DeviceCommands.EndCommandBuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkEndCommandBuffer')));
  @DeviceCommands.ResetCommandBuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkResetCommandBuffer')));
  @DeviceCommands.CmdBindPipeline:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdBindPipeline')));
  @DeviceCommands.CmdSetViewport:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdSetViewport')));
  @DeviceCommands.CmdSetScissor:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdSetScissor')));
  @DeviceCommands.CmdSetLineWidth:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdSetLineWidth')));
  @DeviceCommands.CmdSetDepthBias:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdSetDepthBias')));
  @DeviceCommands.CmdSetBlendConstants:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdSetBlendConstants')));
  @DeviceCommands.CmdSetDepthBounds:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdSetDepthBounds')));
  @DeviceCommands.CmdSetStencilCompareMask:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdSetStencilCompareMask')));
  @DeviceCommands.CmdSetStencilWriteMask:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdSetStencilWriteMask')));
  @DeviceCommands.CmdSetStencilReference:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdSetStencilReference')));
  @DeviceCommands.CmdBindDescriptorSets:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdBindDescriptorSets')));
  @DeviceCommands.CmdBindIndexBuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdBindIndexBuffer')));
  @DeviceCommands.CmdBindVertexBuffers:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdBindVertexBuffers')));
  @DeviceCommands.CmdDraw:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdDraw')));
  @DeviceCommands.CmdDrawIndexed:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdDrawIndexed')));
  @DeviceCommands.CmdDrawIndirect:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdDrawIndirect')));
  @DeviceCommands.CmdDrawIndexedIndirect:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdDrawIndexedIndirect')));
  @DeviceCommands.CmdDispatch:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdDispatch')));
  @DeviceCommands.CmdDispatchIndirect:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdDispatchIndirect')));
  @DeviceCommands.CmdCopyBuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdCopyBuffer')));
  @DeviceCommands.CmdCopyImage:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdCopyImage')));
  @DeviceCommands.CmdBlitImage:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdBlitImage')));
  @DeviceCommands.CmdCopyBufferToImage:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdCopyBufferToImage')));
  @DeviceCommands.CmdCopyImageToBuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdCopyImageToBuffer')));
  @DeviceCommands.CmdUpdateBuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdUpdateBuffer')));
  @DeviceCommands.CmdFillBuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdFillBuffer')));
  @DeviceCommands.CmdClearColorImage:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdClearColorImage')));
  @DeviceCommands.CmdClearDepthStencilImage:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdClearDepthStencilImage')));
  @DeviceCommands.CmdClearAttachments:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdClearAttachments')));
  @DeviceCommands.CmdResolveImage:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdResolveImage')));
  @DeviceCommands.CmdSetEvent:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdSetEvent')));
  @DeviceCommands.CmdResetEvent:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdResetEvent')));
  @DeviceCommands.CmdWaitEvents:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdWaitEvents')));
  @DeviceCommands.CmdPipelineBarrier:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdPipelineBarrier')));
  @DeviceCommands.CmdBeginQuery:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdBeginQuery')));
  @DeviceCommands.CmdEndQuery:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdEndQuery')));
  @DeviceCommands.CmdResetQueryPool:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdResetQueryPool')));
  @DeviceCommands.CmdWriteTimestamp:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdWriteTimestamp')));
  @DeviceCommands.CmdCopyQueryPoolResults:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdCopyQueryPoolResults')));
  @DeviceCommands.CmdPushConstants:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdPushConstants')));
  @DeviceCommands.CmdBeginRenderPass:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdBeginRenderPass')));
  @DeviceCommands.CmdNextSubpass:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdNextSubpass')));
  @DeviceCommands.CmdEndRenderPass:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdEndRenderPass')));
  @DeviceCommands.CmdExecuteCommands:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdExecuteCommands')));
  @DeviceCommands.CreateSharedSwapchainsKHR:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateSharedSwapchainsKHR')));
  @DeviceCommands.CreateSwapchainKHR:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateSwapchainKHR')));
  @DeviceCommands.DestroySwapchainKHR:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroySwapchainKHR')));
  @DeviceCommands.GetSwapchainImagesKHR:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetSwapchainImagesKHR')));
  @DeviceCommands.AcquireNextImageKHR:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkAcquireNextImageKHR')));
  @DeviceCommands.QueuePresentKHR:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkQueuePresentKHR')));
 end;
end;

function LoadVulkanDeviceCommands(const GetDeviceProcAddr:TvkGetDeviceProcAddr;const Device:TVkDevice;out DeviceCommands:TVulkanDeviceCommands):boolean; overload;
begin
 FillChar(DeviceCommands,SizeOf(TVulkanDeviceCommands),#0);
 result:=assigned(GetDeviceProcAddr);
 if result then begin
  // Device commands of any Vulkan command whose first parameter is one of: vkDevice, VkQueue, VkCommandBuffer
  @DeviceCommands.GetDeviceProcAddr:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetDeviceProcAddr')));
  @DeviceCommands.DestroyDevice:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyDevice')));
  @DeviceCommands.GetDeviceQueue:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetDeviceQueue')));
  @DeviceCommands.QueueSubmit:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkQueueSubmit')));
  @DeviceCommands.QueueWaitIdle:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkQueueWaitIdle')));
  @DeviceCommands.DeviceWaitIdle:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDeviceWaitIdle')));
  @DeviceCommands.AllocateMemory:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkAllocateMemory')));
  @DeviceCommands.FreeMemory:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkFreeMemory')));
  @DeviceCommands.MapMemory:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkMapMemory')));
  @DeviceCommands.UnmapMemory:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkUnmapMemory')));
  @DeviceCommands.FlushMappedMemoryRanges:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkFlushMappedMemoryRanges')));
  @DeviceCommands.InvalidateMappedMemoryRanges:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkInvalidateMappedMemoryRanges')));
  @DeviceCommands.GetDeviceMemoryCommitment:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetDeviceMemoryCommitment')));
  @DeviceCommands.GetBufferMemoryRequirements:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetBufferMemoryRequirements')));
  @DeviceCommands.BindBufferMemory:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkBindBufferMemory')));
  @DeviceCommands.GetImageMemoryRequirements:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetImageMemoryRequirements')));
  @DeviceCommands.BindImageMemory:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkBindImageMemory')));
  @DeviceCommands.GetImageSparseMemoryRequirements:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetImageSparseMemoryRequirements')));
  @DeviceCommands.QueueBindSparse:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkQueueBindSparse')));
  @DeviceCommands.CreateFence:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateFence')));
  @DeviceCommands.DestroyFence:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyFence')));
  @DeviceCommands.ResetFences:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkResetFences')));
  @DeviceCommands.GetFenceStatus:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetFenceStatus')));
  @DeviceCommands.WaitForFences:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkWaitForFences')));
  @DeviceCommands.CreateSemaphore:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateSemaphore')));
  @DeviceCommands.DestroySemaphore:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroySemaphore')));
  @DeviceCommands.CreateEvent:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateEvent')));
  @DeviceCommands.DestroyEvent:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyEvent')));
  @DeviceCommands.GetEventStatus:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetEventStatus')));
  @DeviceCommands.SetEvent:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkSetEvent')));
  @DeviceCommands.ResetEvent:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkResetEvent')));
  @DeviceCommands.CreateQueryPool:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateQueryPool')));
  @DeviceCommands.DestroyQueryPool:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyQueryPool')));
  @DeviceCommands.GetQueryPoolResults:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetQueryPoolResults')));
  @DeviceCommands.CreateBuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateBuffer')));
  @DeviceCommands.DestroyBuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyBuffer')));
  @DeviceCommands.CreateBufferView:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateBufferView')));
  @DeviceCommands.DestroyBufferView:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyBufferView')));
  @DeviceCommands.CreateImage:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateImage')));
  @DeviceCommands.DestroyImage:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyImage')));
  @DeviceCommands.GetImageSubresourceLayout:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetImageSubresourceLayout')));
  @DeviceCommands.CreateImageView:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateImageView')));
  @DeviceCommands.DestroyImageView:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyImageView')));
  @DeviceCommands.CreateShaderModule:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateShaderModule')));
  @DeviceCommands.DestroyShaderModule:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyShaderModule')));
  @DeviceCommands.CreatePipelineCache:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreatePipelineCache')));
  @DeviceCommands.DestroyPipelineCache:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyPipelineCache')));
  @DeviceCommands.GetPipelineCacheData:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetPipelineCacheData')));
  @DeviceCommands.MergePipelineCaches:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkMergePipelineCaches')));
  @DeviceCommands.CreateGraphicsPipelines:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateGraphicsPipelines')));
  @DeviceCommands.CreateComputePipelines:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateComputePipelines')));
  @DeviceCommands.DestroyPipeline:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyPipeline')));
  @DeviceCommands.CreatePipelineLayout:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreatePipelineLayout')));
  @DeviceCommands.DestroyPipelineLayout:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyPipelineLayout')));
  @DeviceCommands.CreateSampler:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateSampler')));
  @DeviceCommands.DestroySampler:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroySampler')));
  @DeviceCommands.CreateDescriptorSetLayout:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateDescriptorSetLayout')));
  @DeviceCommands.DestroyDescriptorSetLayout:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyDescriptorSetLayout')));
  @DeviceCommands.CreateDescriptorPool:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateDescriptorPool')));
  @DeviceCommands.DestroyDescriptorPool:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyDescriptorPool')));
  @DeviceCommands.ResetDescriptorPool:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkResetDescriptorPool')));
  @DeviceCommands.AllocateDescriptorSets:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkAllocateDescriptorSets')));
  @DeviceCommands.FreeDescriptorSets:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkFreeDescriptorSets')));
  @DeviceCommands.UpdateDescriptorSets:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkUpdateDescriptorSets')));
  @DeviceCommands.CreateFramebuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateFramebuffer')));
  @DeviceCommands.DestroyFramebuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyFramebuffer')));
  @DeviceCommands.CreateRenderPass:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateRenderPass')));
  @DeviceCommands.DestroyRenderPass:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyRenderPass')));
  @DeviceCommands.GetRenderAreaGranularity:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetRenderAreaGranularity')));
  @DeviceCommands.CreateCommandPool:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateCommandPool')));
  @DeviceCommands.DestroyCommandPool:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyCommandPool')));
  @DeviceCommands.ResetCommandPool:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkResetCommandPool')));
  @DeviceCommands.AllocateCommandBuffers:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkAllocateCommandBuffers')));
  @DeviceCommands.FreeCommandBuffers:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkFreeCommandBuffers')));
  @DeviceCommands.BeginCommandBuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkBeginCommandBuffer')));
  @DeviceCommands.EndCommandBuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkEndCommandBuffer')));
  @DeviceCommands.ResetCommandBuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkResetCommandBuffer')));
  @DeviceCommands.CmdBindPipeline:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdBindPipeline')));
  @DeviceCommands.CmdSetViewport:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdSetViewport')));
  @DeviceCommands.CmdSetScissor:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdSetScissor')));
  @DeviceCommands.CmdSetLineWidth:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdSetLineWidth')));
  @DeviceCommands.CmdSetDepthBias:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdSetDepthBias')));
  @DeviceCommands.CmdSetBlendConstants:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdSetBlendConstants')));
  @DeviceCommands.CmdSetDepthBounds:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdSetDepthBounds')));
  @DeviceCommands.CmdSetStencilCompareMask:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdSetStencilCompareMask')));
  @DeviceCommands.CmdSetStencilWriteMask:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdSetStencilWriteMask')));
  @DeviceCommands.CmdSetStencilReference:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdSetStencilReference')));
  @DeviceCommands.CmdBindDescriptorSets:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdBindDescriptorSets')));
  @DeviceCommands.CmdBindIndexBuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdBindIndexBuffer')));
  @DeviceCommands.CmdBindVertexBuffers:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdBindVertexBuffers')));
  @DeviceCommands.CmdDraw:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdDraw')));
  @DeviceCommands.CmdDrawIndexed:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdDrawIndexed')));
  @DeviceCommands.CmdDrawIndirect:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdDrawIndirect')));
  @DeviceCommands.CmdDrawIndexedIndirect:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdDrawIndexedIndirect')));
  @DeviceCommands.CmdDispatch:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdDispatch')));
  @DeviceCommands.CmdDispatchIndirect:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdDispatchIndirect')));
  @DeviceCommands.CmdCopyBuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdCopyBuffer')));
  @DeviceCommands.CmdCopyImage:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdCopyImage')));
  @DeviceCommands.CmdBlitImage:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdBlitImage')));
  @DeviceCommands.CmdCopyBufferToImage:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdCopyBufferToImage')));
  @DeviceCommands.CmdCopyImageToBuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdCopyImageToBuffer')));
  @DeviceCommands.CmdUpdateBuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdUpdateBuffer')));
  @DeviceCommands.CmdFillBuffer:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdFillBuffer')));
  @DeviceCommands.CmdClearColorImage:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdClearColorImage')));
  @DeviceCommands.CmdClearDepthStencilImage:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdClearDepthStencilImage')));
  @DeviceCommands.CmdClearAttachments:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdClearAttachments')));
  @DeviceCommands.CmdResolveImage:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdResolveImage')));
  @DeviceCommands.CmdSetEvent:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdSetEvent')));
  @DeviceCommands.CmdResetEvent:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdResetEvent')));
  @DeviceCommands.CmdWaitEvents:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdWaitEvents')));
  @DeviceCommands.CmdPipelineBarrier:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdPipelineBarrier')));
  @DeviceCommands.CmdBeginQuery:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdBeginQuery')));
  @DeviceCommands.CmdEndQuery:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdEndQuery')));
  @DeviceCommands.CmdResetQueryPool:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdResetQueryPool')));
  @DeviceCommands.CmdWriteTimestamp:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdWriteTimestamp')));
  @DeviceCommands.CmdCopyQueryPoolResults:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdCopyQueryPoolResults')));
  @DeviceCommands.CmdPushConstants:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdPushConstants')));
  @DeviceCommands.CmdBeginRenderPass:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdBeginRenderPass')));
  @DeviceCommands.CmdNextSubpass:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdNextSubpass')));
  @DeviceCommands.CmdEndRenderPass:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdEndRenderPass')));
  @DeviceCommands.CmdExecuteCommands:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdExecuteCommands')));
  @DeviceCommands.CreateSharedSwapchainsKHR:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateSharedSwapchainsKHR')));
  @DeviceCommands.CreateSwapchainKHR:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateSwapchainKHR')));
  @DeviceCommands.DestroySwapchainKHR:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroySwapchainKHR')));
  @DeviceCommands.GetSwapchainImagesKHR:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetSwapchainImagesKHR')));
  @DeviceCommands.AcquireNextImageKHR:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkAcquireNextImageKHR')));
  @DeviceCommands.QueuePresentKHR:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkQueuePresentKHR')));
 end;
end;

constructor TVulkan.Create;
begin
 inherited Create;
 FillChar(fCommands,SizeOf(TVulkanCommands),#0);
end;

constructor TVulkan.Create(const AVulkanCommands:TVulkanCommands);
begin
 inherited Create;
 fCommands:=AVulkanCommands;
end;

destructor TVulkan.Destroy;
begin
 inherited Destroy;
end;

function TVulkan.CreateInstance(const pCreateInfo:PVkInstanceCreateInfo;const pAllocator:PVkAllocationCallbacks;pInstance:PVkInstance):TVkResult;
begin
 result:=fCommands.CreateInstance(pCreateInfo,pAllocator,pInstance);
end;

procedure TVulkan.DestroyInstance(instance:TVkInstance;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroyInstance(instance,pAllocator);
end;

function TVulkan.EnumeratePhysicalDevices(instance:TVkInstance;pPhysicalDeviceCount:PVkUInt32;pPhysicalDevices:PVkPhysicalDevice):TVkResult;
begin
 result:=fCommands.EnumeratePhysicalDevices(instance,pPhysicalDeviceCount,pPhysicalDevices);
end;

function TVulkan.GetDeviceProcAddr(device:TVkDevice;const pName:PVkChar):TPFN_vkVoidFunction;
begin
 result:=fCommands.GetDeviceProcAddr(device,pName);
end;

function TVulkan.GetInstanceProcAddr(instance:TVkInstance;const pName:PVkChar):TPFN_vkVoidFunction;
begin
 result:=fCommands.GetInstanceProcAddr(instance,pName);
end;

procedure TVulkan.GetPhysicalDeviceProperties(physicalDevice:TVkPhysicalDevice;pProperties:PVkPhysicalDeviceProperties);
begin
 fCommands.GetPhysicalDeviceProperties(physicalDevice,pProperties);
end;

procedure TVulkan.GetPhysicalDeviceQueueFamilyProperties(physicalDevice:TVkPhysicalDevice;pQueueFamilyPropertyCount:PVkUInt32;pQueueFamilyProperties:PVkQueueFamilyProperties);
begin
 fCommands.GetPhysicalDeviceQueueFamilyProperties(physicalDevice,pQueueFamilyPropertyCount,pQueueFamilyProperties);
end;

procedure TVulkan.GetPhysicalDeviceMemoryProperties(physicalDevice:TVkPhysicalDevice;pMemoryProperties:PVkPhysicalDeviceMemoryProperties);
begin
 fCommands.GetPhysicalDeviceMemoryProperties(physicalDevice,pMemoryProperties);
end;

procedure TVulkan.GetPhysicalDeviceFeatures(physicalDevice:TVkPhysicalDevice;pFeatures:PVkPhysicalDeviceFeatures);
begin
 fCommands.GetPhysicalDeviceFeatures(physicalDevice,pFeatures);
end;

procedure TVulkan.GetPhysicalDeviceFormatProperties(physicalDevice:TVkPhysicalDevice;format:TVkFormat;pFormatProperties:PVkFormatProperties);
begin
 fCommands.GetPhysicalDeviceFormatProperties(physicalDevice,format,pFormatProperties);
end;

function TVulkan.GetPhysicalDeviceImageFormatProperties(physicalDevice:TVkPhysicalDevice;format:TVkFormat;type_:TVkImageType;tiling:TVkImageTiling;usage:TVkImageUsageFlags;flags:TVkImageCreateFlags;pImageFormatProperties:PVkImageFormatProperties):TVkResult;
begin
 result:=fCommands.GetPhysicalDeviceImageFormatProperties(physicalDevice,format,type_,tiling,usage,flags,pImageFormatProperties);
end;

function TVulkan.CreateDevice(physicalDevice:TVkPhysicalDevice;const pCreateInfo:PVkDeviceCreateInfo;const pAllocator:PVkAllocationCallbacks;pDevice:PVkDevice):TVkResult;
begin
 result:=fCommands.CreateDevice(physicalDevice,pCreateInfo,pAllocator,pDevice);
end;

procedure TVulkan.DestroyDevice(device:TVkDevice;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroyDevice(device,pAllocator);
end;

function TVulkan.EnumerateInstanceLayerProperties(pPropertyCount:PVkUInt32;pProperties:PVkLayerProperties):TVkResult;
begin
 result:=fCommands.EnumerateInstanceLayerProperties(pPropertyCount,pProperties);
end;

function TVulkan.EnumerateInstanceExtensionProperties(const pLayerName:PVkChar;pPropertyCount:PVkUInt32;pProperties:PVkExtensionProperties):TVkResult;
begin
 result:=fCommands.EnumerateInstanceExtensionProperties(pLayerName,pPropertyCount,pProperties);
end;

function TVulkan.EnumerateDeviceLayerProperties(physicalDevice:TVkPhysicalDevice;pPropertyCount:PVkUInt32;pProperties:PVkLayerProperties):TVkResult;
begin
 result:=fCommands.EnumerateDeviceLayerProperties(physicalDevice,pPropertyCount,pProperties);
end;

function TVulkan.EnumerateDeviceExtensionProperties(physicalDevice:TVkPhysicalDevice;const pLayerName:PVkChar;pPropertyCount:PVkUInt32;pProperties:PVkExtensionProperties):TVkResult;
begin
 result:=fCommands.EnumerateDeviceExtensionProperties(physicalDevice,pLayerName,pPropertyCount,pProperties);
end;

procedure TVulkan.GetDeviceQueue(device:TVkDevice;queueFamilyIndex:TVkUInt32;queueIndex:TVkUInt32;pQueue:PVkQueue);
begin
 fCommands.GetDeviceQueue(device,queueFamilyIndex,queueIndex,pQueue);
end;

function TVulkan.QueueSubmit(queue:TVkQueue;submitCount:TVkUInt32;const pSubmits:PVkSubmitInfo;fence:TVkFence):TVkResult;
begin
 result:=fCommands.QueueSubmit(queue,submitCount,pSubmits,fence);
end;

function TVulkan.QueueWaitIdle(queue:TVkQueue):TVkResult;
begin
 result:=fCommands.QueueWaitIdle(queue);
end;

function TVulkan.DeviceWaitIdle(device:TVkDevice):TVkResult;
begin
 result:=fCommands.DeviceWaitIdle(device);
end;

function TVulkan.AllocateMemory(device:TVkDevice;const pAllocateInfo:PVkMemoryAllocateInfo;const pAllocator:PVkAllocationCallbacks;pMemory:PVkDeviceMemory):TVkResult;
begin
 result:=fCommands.AllocateMemory(device,pAllocateInfo,pAllocator,pMemory);
end;

procedure TVulkan.FreeMemory(device:TVkDevice;memory:TVkDeviceMemory;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.FreeMemory(device,memory,pAllocator);
end;

function TVulkan.MapMemory(device:TVkDevice;memory:TVkDeviceMemory;offset:TVkDeviceSize;size:TVkDeviceSize;flags:TVkMemoryMapFlags;ppData:TVkPointer):TVkResult;
begin
 result:=fCommands.MapMemory(device,memory,offset,size,flags,ppData);
end;

procedure TVulkan.UnmapMemory(device:TVkDevice;memory:TVkDeviceMemory);
begin
 fCommands.UnmapMemory(device,memory);
end;

function TVulkan.FlushMappedMemoryRanges(device:TVkDevice;memoryRangeCount:TVkUInt32;const pMemoryRanges:PVkMappedMemoryRange):TVkResult;
begin
 result:=fCommands.FlushMappedMemoryRanges(device,memoryRangeCount,pMemoryRanges);
end;

function TVulkan.InvalidateMappedMemoryRanges(device:TVkDevice;memoryRangeCount:TVkUInt32;const pMemoryRanges:PVkMappedMemoryRange):TVkResult;
begin
 result:=fCommands.InvalidateMappedMemoryRanges(device,memoryRangeCount,pMemoryRanges);
end;

procedure TVulkan.GetDeviceMemoryCommitment(device:TVkDevice;memory:TVkDeviceMemory;pCommittedMemoryInBytes:PVkDeviceSize);
begin
 fCommands.GetDeviceMemoryCommitment(device,memory,pCommittedMemoryInBytes);
end;

procedure TVulkan.GetBufferMemoryRequirements(device:TVkDevice;buffer:TVkBuffer;pMemoryRequirements:PVkMemoryRequirements);
begin
 fCommands.GetBufferMemoryRequirements(device,buffer,pMemoryRequirements);
end;

function TVulkan.BindBufferMemory(device:TVkDevice;buffer:TVkBuffer;memory:TVkDeviceMemory;memoryOffset:TVkDeviceSize):TVkResult;
begin
 result:=fCommands.BindBufferMemory(device,buffer,memory,memoryOffset);
end;

procedure TVulkan.GetImageMemoryRequirements(device:TVkDevice;image:TVkImage;pMemoryRequirements:PVkMemoryRequirements);
begin
 fCommands.GetImageMemoryRequirements(device,image,pMemoryRequirements);
end;

function TVulkan.BindImageMemory(device:TVkDevice;image:TVkImage;memory:TVkDeviceMemory;memoryOffset:TVkDeviceSize):TVkResult;
begin
 result:=fCommands.BindImageMemory(device,image,memory,memoryOffset);
end;

procedure TVulkan.GetImageSparseMemoryRequirements(device:TVkDevice;image:TVkImage;pSparseMemoryRequirementCount:PVkUInt32;pSparseMemoryRequirements:PVkSparseImageMemoryRequirements);
begin
 fCommands.GetImageSparseMemoryRequirements(device,image,pSparseMemoryRequirementCount,pSparseMemoryRequirements);
end;

procedure TVulkan.GetPhysicalDeviceSparseImageFormatProperties(physicalDevice:TVkPhysicalDevice;format:TVkFormat;type_:TVkImageType;samples:TVkSampleCountFlagBits;usage:TVkImageUsageFlags;tiling:TVkImageTiling;pPropertyCount:PVkUInt32;pProperties:PVkSparseImageFormatProperties);
begin
 fCommands.GetPhysicalDeviceSparseImageFormatProperties(physicalDevice,format,type_,samples,usage,tiling,pPropertyCount,pProperties);
end;

function TVulkan.QueueBindSparse(queue:TVkQueue;bindInfoCount:TVkUInt32;const pBindInfo:PVkBindSparseInfo;fence:TVkFence):TVkResult;
begin
 result:=fCommands.QueueBindSparse(queue,bindInfoCount,pBindInfo,fence);
end;

function TVulkan.CreateFence(device:TVkDevice;const pCreateInfo:PVkFenceCreateInfo;const pAllocator:PVkAllocationCallbacks;pFence:PVkFence):TVkResult;
begin
 result:=fCommands.CreateFence(device,pCreateInfo,pAllocator,pFence);
end;

procedure TVulkan.DestroyFence(device:TVkDevice;fence:TVkFence;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroyFence(device,fence,pAllocator);
end;

function TVulkan.ResetFences(device:TVkDevice;fenceCount:TVkUInt32;const pFences:PVkFence):TVkResult;
begin
 result:=fCommands.ResetFences(device,fenceCount,pFences);
end;

function TVulkan.GetFenceStatus(device:TVkDevice;fence:TVkFence):TVkResult;
begin
 result:=fCommands.GetFenceStatus(device,fence);
end;

function TVulkan.WaitForFences(device:TVkDevice;fenceCount:TVkUInt32;const pFences:PVkFence;waitAll:TVkBool32;timeout:TVkUInt64):TVkResult;
begin
 result:=fCommands.WaitForFences(device,fenceCount,pFences,waitAll,timeout);
end;

function TVulkan.CreateSemaphore(device:TVkDevice;const pCreateInfo:PVkSemaphoreCreateInfo;const pAllocator:PVkAllocationCallbacks;pSemaphore:PVkSemaphore):TVkResult;
begin
 result:=fCommands.CreateSemaphore(device,pCreateInfo,pAllocator,pSemaphore);
end;

procedure TVulkan.DestroySemaphore(device:TVkDevice;semaphore:TVkSemaphore;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroySemaphore(device,semaphore,pAllocator);
end;

function TVulkan.CreateEvent(device:TVkDevice;const pCreateInfo:PVkEventCreateInfo;const pAllocator:PVkAllocationCallbacks;pEvent:PVkEvent):TVkResult;
begin
 result:=fCommands.CreateEvent(device,pCreateInfo,pAllocator,pEvent);
end;

procedure TVulkan.DestroyEvent(device:TVkDevice;event:TVkEvent;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroyEvent(device,event,pAllocator);
end;

function TVulkan.GetEventStatus(device:TVkDevice;event:TVkEvent):TVkResult;
begin
 result:=fCommands.GetEventStatus(device,event);
end;

function TVulkan.SetEvent(device:TVkDevice;event:TVkEvent):TVkResult;
begin
 result:=fCommands.SetEvent(device,event);
end;

function TVulkan.ResetEvent(device:TVkDevice;event:TVkEvent):TVkResult;
begin
 result:=fCommands.ResetEvent(device,event);
end;

function TVulkan.CreateQueryPool(device:TVkDevice;const pCreateInfo:PVkQueryPoolCreateInfo;const pAllocator:PVkAllocationCallbacks;pQueryPool:PVkQueryPool):TVkResult;
begin
 result:=fCommands.CreateQueryPool(device,pCreateInfo,pAllocator,pQueryPool);
end;

procedure TVulkan.DestroyQueryPool(device:TVkDevice;queryPool:TVkQueryPool;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroyQueryPool(device,queryPool,pAllocator);
end;

function TVulkan.GetQueryPoolResults(device:TVkDevice;queryPool:TVkQueryPool;firstQuery:TVkUInt32;queryCount:TVkUInt32;dataSize:TVkPtrInt;pData:TVkPointer;stride:TVkDeviceSize;flags:TVkQueryResultFlags):TVkResult;
begin
 result:=fCommands.GetQueryPoolResults(device,queryPool,firstQuery,queryCount,dataSize,pData,stride,flags);
end;

function TVulkan.CreateBuffer(device:TVkDevice;const pCreateInfo:PVkBufferCreateInfo;const pAllocator:PVkAllocationCallbacks;pBuffer:PVkBuffer):TVkResult;
begin
 result:=fCommands.CreateBuffer(device,pCreateInfo,pAllocator,pBuffer);
end;

procedure TVulkan.DestroyBuffer(device:TVkDevice;buffer:TVkBuffer;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroyBuffer(device,buffer,pAllocator);
end;

function TVulkan.CreateBufferView(device:TVkDevice;const pCreateInfo:PVkBufferViewCreateInfo;const pAllocator:PVkAllocationCallbacks;pView:PVkBufferView):TVkResult;
begin
 result:=fCommands.CreateBufferView(device,pCreateInfo,pAllocator,pView);
end;

procedure TVulkan.DestroyBufferView(device:TVkDevice;bufferView:TVkBufferView;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroyBufferView(device,bufferView,pAllocator);
end;

function TVulkan.CreateImage(device:TVkDevice;const pCreateInfo:PVkImageCreateInfo;const pAllocator:PVkAllocationCallbacks;pImage:PVkImage):TVkResult;
begin
 result:=fCommands.CreateImage(device,pCreateInfo,pAllocator,pImage);
end;

procedure TVulkan.DestroyImage(device:TVkDevice;image:TVkImage;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroyImage(device,image,pAllocator);
end;

procedure TVulkan.GetImageSubresourceLayout(device:TVkDevice;image:TVkImage;const pSubresource:PVkImageSubresource;pLayout:PVkSubresourceLayout);
begin
 fCommands.GetImageSubresourceLayout(device,image,pSubresource,pLayout);
end;

function TVulkan.CreateImageView(device:TVkDevice;const pCreateInfo:PVkImageViewCreateInfo;const pAllocator:PVkAllocationCallbacks;pView:PVkImageView):TVkResult;
begin
 result:=fCommands.CreateImageView(device,pCreateInfo,pAllocator,pView);
end;

procedure TVulkan.DestroyImageView(device:TVkDevice;imageView:TVkImageView;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroyImageView(device,imageView,pAllocator);
end;

function TVulkan.CreateShaderModule(device:TVkDevice;const pCreateInfo:PVkShaderModuleCreateInfo;const pAllocator:PVkAllocationCallbacks;pShaderModule:PVkShaderModule):TVkResult;
begin
 result:=fCommands.CreateShaderModule(device,pCreateInfo,pAllocator,pShaderModule);
end;

procedure TVulkan.DestroyShaderModule(device:TVkDevice;shaderModule:TVkShaderModule;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroyShaderModule(device,shaderModule,pAllocator);
end;

function TVulkan.CreatePipelineCache(device:TVkDevice;const pCreateInfo:PVkPipelineCacheCreateInfo;const pAllocator:PVkAllocationCallbacks;pPipelineCache:PVkPipelineCache):TVkResult;
begin
 result:=fCommands.CreatePipelineCache(device,pCreateInfo,pAllocator,pPipelineCache);
end;

procedure TVulkan.DestroyPipelineCache(device:TVkDevice;pipelineCache:TVkPipelineCache;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroyPipelineCache(device,pipelineCache,pAllocator);
end;

function TVulkan.GetPipelineCacheData(device:TVkDevice;pipelineCache:TVkPipelineCache;pDataSize:PVkPtrInt;pData:TVkPointer):TVkResult;
begin
 result:=fCommands.GetPipelineCacheData(device,pipelineCache,pDataSize,pData);
end;

function TVulkan.MergePipelineCaches(device:TVkDevice;dstCache:TVkPipelineCache;srcCacheCount:TVkUInt32;const pSrcCaches:PVkPipelineCache):TVkResult;
begin
 result:=fCommands.MergePipelineCaches(device,dstCache,srcCacheCount,pSrcCaches);
end;

function TVulkan.CreateGraphicsPipelines(device:TVkDevice;pipelineCache:TVkPipelineCache;createInfoCount:TVkUInt32;const pCreateInfos:PVkGraphicsPipelineCreateInfo;const pAllocator:PVkAllocationCallbacks;pPipelines:PVkPipeline):TVkResult;
begin
 result:=fCommands.CreateGraphicsPipelines(device,pipelineCache,createInfoCount,pCreateInfos,pAllocator,pPipelines);
end;

function TVulkan.CreateComputePipelines(device:TVkDevice;pipelineCache:TVkPipelineCache;createInfoCount:TVkUInt32;const pCreateInfos:PVkComputePipelineCreateInfo;const pAllocator:PVkAllocationCallbacks;pPipelines:PVkPipeline):TVkResult;
begin
 result:=fCommands.CreateComputePipelines(device,pipelineCache,createInfoCount,pCreateInfos,pAllocator,pPipelines);
end;

procedure TVulkan.DestroyPipeline(device:TVkDevice;pipeline:TVkPipeline;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroyPipeline(device,pipeline,pAllocator);
end;

function TVulkan.CreatePipelineLayout(device:TVkDevice;const pCreateInfo:PVkPipelineLayoutCreateInfo;const pAllocator:PVkAllocationCallbacks;pPipelineLayout:PVkPipelineLayout):TVkResult;
begin
 result:=fCommands.CreatePipelineLayout(device,pCreateInfo,pAllocator,pPipelineLayout);
end;

procedure TVulkan.DestroyPipelineLayout(device:TVkDevice;pipelineLayout:TVkPipelineLayout;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroyPipelineLayout(device,pipelineLayout,pAllocator);
end;

function TVulkan.CreateSampler(device:TVkDevice;const pCreateInfo:PVkSamplerCreateInfo;const pAllocator:PVkAllocationCallbacks;pSampler:PVkSampler):TVkResult;
begin
 result:=fCommands.CreateSampler(device,pCreateInfo,pAllocator,pSampler);
end;

procedure TVulkan.DestroySampler(device:TVkDevice;sampler:TVkSampler;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroySampler(device,sampler,pAllocator);
end;

function TVulkan.CreateDescriptorSetLayout(device:TVkDevice;const pCreateInfo:PVkDescriptorSetLayoutCreateInfo;const pAllocator:PVkAllocationCallbacks;pSetLayout:PVkDescriptorSetLayout):TVkResult;
begin
 result:=fCommands.CreateDescriptorSetLayout(device,pCreateInfo,pAllocator,pSetLayout);
end;

procedure TVulkan.DestroyDescriptorSetLayout(device:TVkDevice;descriptorSetLayout:TVkDescriptorSetLayout;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroyDescriptorSetLayout(device,descriptorSetLayout,pAllocator);
end;

function TVulkan.CreateDescriptorPool(device:TVkDevice;const pCreateInfo:PVkDescriptorPoolCreateInfo;const pAllocator:PVkAllocationCallbacks;pDescriptorPool:PVkDescriptorPool):TVkResult;
begin
 result:=fCommands.CreateDescriptorPool(device,pCreateInfo,pAllocator,pDescriptorPool);
end;

procedure TVulkan.DestroyDescriptorPool(device:TVkDevice;descriptorPool:TVkDescriptorPool;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroyDescriptorPool(device,descriptorPool,pAllocator);
end;

function TVulkan.ResetDescriptorPool(device:TVkDevice;descriptorPool:TVkDescriptorPool;flags:TVkDescriptorPoolResetFlags):TVkResult;
begin
 result:=fCommands.ResetDescriptorPool(device,descriptorPool,flags);
end;

function TVulkan.AllocateDescriptorSets(device:TVkDevice;const pAllocateInfo:PVkDescriptorSetAllocateInfo;pDescriptorSets:PVkDescriptorSet):TVkResult;
begin
 result:=fCommands.AllocateDescriptorSets(device,pAllocateInfo,pDescriptorSets);
end;

function TVulkan.FreeDescriptorSets(device:TVkDevice;descriptorPool:TVkDescriptorPool;descriptorSetCount:TVkUInt32;const pDescriptorSets:PVkDescriptorSet):TVkResult;
begin
 result:=fCommands.FreeDescriptorSets(device,descriptorPool,descriptorSetCount,pDescriptorSets);
end;

procedure TVulkan.UpdateDescriptorSets(device:TVkDevice;descriptorWriteCount:TVkUInt32;const pDescriptorWrites:PVkWriteDescriptorSet;descriptorCopyCount:TVkUInt32;const pDescriptorCopies:PVkCopyDescriptorSet);
begin
 fCommands.UpdateDescriptorSets(device,descriptorWriteCount,pDescriptorWrites,descriptorCopyCount,pDescriptorCopies);
end;

function TVulkan.CreateFramebuffer(device:TVkDevice;const pCreateInfo:PVkFramebufferCreateInfo;const pAllocator:PVkAllocationCallbacks;pFramebuffer:PVkFramebuffer):TVkResult;
begin
 result:=fCommands.CreateFramebuffer(device,pCreateInfo,pAllocator,pFramebuffer);
end;

procedure TVulkan.DestroyFramebuffer(device:TVkDevice;framebuffer:TVkFramebuffer;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroyFramebuffer(device,framebuffer,pAllocator);
end;

function TVulkan.CreateRenderPass(device:TVkDevice;const pCreateInfo:PVkRenderPassCreateInfo;const pAllocator:PVkAllocationCallbacks;pRenderPass:PVkRenderPass):TVkResult;
begin
 result:=fCommands.CreateRenderPass(device,pCreateInfo,pAllocator,pRenderPass);
end;

procedure TVulkan.DestroyRenderPass(device:TVkDevice;renderPass:TVkRenderPass;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroyRenderPass(device,renderPass,pAllocator);
end;

procedure TVulkan.GetRenderAreaGranularity(device:TVkDevice;renderPass:TVkRenderPass;pGranularity:PVkExtent2D);
begin
 fCommands.GetRenderAreaGranularity(device,renderPass,pGranularity);
end;

function TVulkan.CreateCommandPool(device:TVkDevice;const pCreateInfo:PVkCommandPoolCreateInfo;const pAllocator:PVkAllocationCallbacks;pCommandPool:PVkCommandPool):TVkResult;
begin
 result:=fCommands.CreateCommandPool(device,pCreateInfo,pAllocator,pCommandPool);
end;

procedure TVulkan.DestroyCommandPool(device:TVkDevice;commandPool:TVkCommandPool;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroyCommandPool(device,commandPool,pAllocator);
end;

function TVulkan.ResetCommandPool(device:TVkDevice;commandPool:TVkCommandPool;flags:TVkCommandPoolResetFlags):TVkResult;
begin
 result:=fCommands.ResetCommandPool(device,commandPool,flags);
end;

function TVulkan.AllocateCommandBuffers(device:TVkDevice;const pAllocateInfo:PVkCommandBufferAllocateInfo;pCommandBuffers:PVkCommandBuffer):TVkResult;
begin
 result:=fCommands.AllocateCommandBuffers(device,pAllocateInfo,pCommandBuffers);
end;

procedure TVulkan.FreeCommandBuffers(device:TVkDevice;commandPool:TVkCommandPool;commandBufferCount:TVkUInt32;const pCommandBuffers:PVkCommandBuffer);
begin
 fCommands.FreeCommandBuffers(device,commandPool,commandBufferCount,pCommandBuffers);
end;

function TVulkan.BeginCommandBuffer(commandBuffer:TVkCommandBuffer;const pBeginInfo:PVkCommandBufferBeginInfo):TVkResult;
begin
 result:=fCommands.BeginCommandBuffer(commandBuffer,pBeginInfo);
end;

function TVulkan.EndCommandBuffer(commandBuffer:TVkCommandBuffer):TVkResult;
begin
 result:=fCommands.EndCommandBuffer(commandBuffer);
end;

function TVulkan.ResetCommandBuffer(commandBuffer:TVkCommandBuffer;flags:TVkCommandBufferResetFlags):TVkResult;
begin
 result:=fCommands.ResetCommandBuffer(commandBuffer,flags);
end;

procedure TVulkan.CmdBindPipeline(commandBuffer:TVkCommandBuffer;pipelineBindPoint:TVkPipelineBindPoint;pipeline:TVkPipeline);
begin
 fCommands.CmdBindPipeline(commandBuffer,pipelineBindPoint,pipeline);
end;

procedure TVulkan.CmdSetViewport(commandBuffer:TVkCommandBuffer;firstViewport:TVkUInt32;viewportCount:TVkUInt32;const pViewports:PVkViewport);
begin
 fCommands.CmdSetViewport(commandBuffer,firstViewport,viewportCount,pViewports);
end;

procedure TVulkan.CmdSetScissor(commandBuffer:TVkCommandBuffer;firstScissor:TVkUInt32;scissorCount:TVkUInt32;const pScissors:PVkRect2D);
begin
 fCommands.CmdSetScissor(commandBuffer,firstScissor,scissorCount,pScissors);
end;

procedure TVulkan.CmdSetLineWidth(commandBuffer:TVkCommandBuffer;lineWidth:TVkFloat);
begin
 fCommands.CmdSetLineWidth(commandBuffer,lineWidth);
end;

procedure TVulkan.CmdSetDepthBias(commandBuffer:TVkCommandBuffer;depthBiasConstantFactor:TVkFloat;depthBiasClamp:TVkFloat;depthBiasSlopeFactor:TVkFloat);
begin
 fCommands.CmdSetDepthBias(commandBuffer,depthBiasConstantFactor,depthBiasClamp,depthBiasSlopeFactor);
end;

procedure TVulkan.CmdSetBlendConstants(commandBuffer:TVkCommandBuffer;const blendConstants:TVkFloat);
begin
 fCommands.CmdSetBlendConstants(commandBuffer,blendConstants);
end;

procedure TVulkan.CmdSetDepthBounds(commandBuffer:TVkCommandBuffer;minDepthBounds:TVkFloat;maxDepthBounds:TVkFloat);
begin
 fCommands.CmdSetDepthBounds(commandBuffer,minDepthBounds,maxDepthBounds);
end;

procedure TVulkan.CmdSetStencilCompareMask(commandBuffer:TVkCommandBuffer;faceMask:TVkStencilFaceFlags;compareMask:TVkUInt32);
begin
 fCommands.CmdSetStencilCompareMask(commandBuffer,faceMask,compareMask);
end;

procedure TVulkan.CmdSetStencilWriteMask(commandBuffer:TVkCommandBuffer;faceMask:TVkStencilFaceFlags;writeMask:TVkUInt32);
begin
 fCommands.CmdSetStencilWriteMask(commandBuffer,faceMask,writeMask);
end;

procedure TVulkan.CmdSetStencilReference(commandBuffer:TVkCommandBuffer;faceMask:TVkStencilFaceFlags;reference:TVkUInt32);
begin
 fCommands.CmdSetStencilReference(commandBuffer,faceMask,reference);
end;

procedure TVulkan.CmdBindDescriptorSets(commandBuffer:TVkCommandBuffer;pipelineBindPoint:TVkPipelineBindPoint;layout:TVkPipelineLayout;firstSet:TVkUInt32;descriptorSetCount:TVkUInt32;const pDescriptorSets:PVkDescriptorSet;dynamicOffsetCount:TVkUInt32;const pDynamicOffsets:PVkUInt32);
begin
 fCommands.CmdBindDescriptorSets(commandBuffer,pipelineBindPoint,layout,firstSet,descriptorSetCount,pDescriptorSets,dynamicOffsetCount,pDynamicOffsets);
end;

procedure TVulkan.CmdBindIndexBuffer(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;indexType:TVkIndexType);
begin
 fCommands.CmdBindIndexBuffer(commandBuffer,buffer,offset,indexType);
end;

procedure TVulkan.CmdBindVertexBuffers(commandBuffer:TVkCommandBuffer;firstBinding:TVkUInt32;bindingCount:TVkUInt32;const pBuffers:PVkBuffer;const pOffsets:PVkDeviceSize);
begin
 fCommands.CmdBindVertexBuffers(commandBuffer,firstBinding,bindingCount,pBuffers,pOffsets);
end;

procedure TVulkan.CmdDraw(commandBuffer:TVkCommandBuffer;vertexCount:TVkUInt32;instanceCount:TVkUInt32;firstVertex:TVkUInt32;firstInstance:TVkUInt32);
begin
 fCommands.CmdDraw(commandBuffer,vertexCount,instanceCount,firstVertex,firstInstance);
end;

procedure TVulkan.CmdDrawIndexed(commandBuffer:TVkCommandBuffer;indexCount:TVkUInt32;instanceCount:TVkUInt32;firstIndex:TVkUInt32;vertexOffset:TVkInt32;firstInstance:TVkUInt32);
begin
 fCommands.CmdDrawIndexed(commandBuffer,indexCount,instanceCount,firstIndex,vertexOffset,firstInstance);
end;

procedure TVulkan.CmdDrawIndirect(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;drawCount:TVkUInt32;stride:TVkUInt32);
begin
 fCommands.CmdDrawIndirect(commandBuffer,buffer,offset,drawCount,stride);
end;

procedure TVulkan.CmdDrawIndexedIndirect(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;drawCount:TVkUInt32;stride:TVkUInt32);
begin
 fCommands.CmdDrawIndexedIndirect(commandBuffer,buffer,offset,drawCount,stride);
end;

procedure TVulkan.CmdDispatch(commandBuffer:TVkCommandBuffer;x:TVkUInt32;y:TVkUInt32;z:TVkUInt32);
begin
 fCommands.CmdDispatch(commandBuffer,x,y,z);
end;

procedure TVulkan.CmdDispatchIndirect(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize);
begin
 fCommands.CmdDispatchIndirect(commandBuffer,buffer,offset);
end;

procedure TVulkan.CmdCopyBuffer(commandBuffer:TVkCommandBuffer;srcBuffer:TVkBuffer;dstBuffer:TVkBuffer;regionCount:TVkUInt32;const pRegions:PVkBufferCopy);
begin
 fCommands.CmdCopyBuffer(commandBuffer,srcBuffer,dstBuffer,regionCount,pRegions);
end;

procedure TVulkan.CmdCopyImage(commandBuffer:TVkCommandBuffer;srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkImageCopy);
begin
 fCommands.CmdCopyImage(commandBuffer,srcImage,srcImageLayout,dstImage,dstImageLayout,regionCount,pRegions);
end;

procedure TVulkan.CmdBlitImage(commandBuffer:TVkCommandBuffer;srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkImageBlit;filter:TVkFilter);
begin
 fCommands.CmdBlitImage(commandBuffer,srcImage,srcImageLayout,dstImage,dstImageLayout,regionCount,pRegions,filter);
end;

procedure TVulkan.CmdCopyBufferToImage(commandBuffer:TVkCommandBuffer;srcBuffer:TVkBuffer;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkBufferImageCopy);
begin
 fCommands.CmdCopyBufferToImage(commandBuffer,srcBuffer,dstImage,dstImageLayout,regionCount,pRegions);
end;

procedure TVulkan.CmdCopyImageToBuffer(commandBuffer:TVkCommandBuffer;srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstBuffer:TVkBuffer;regionCount:TVkUInt32;const pRegions:PVkBufferImageCopy);
begin
 fCommands.CmdCopyImageToBuffer(commandBuffer,srcImage,srcImageLayout,dstBuffer,regionCount,pRegions);
end;

procedure TVulkan.CmdUpdateBuffer(commandBuffer:TVkCommandBuffer;dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;dataSize:TVkDeviceSize;const pData:PVkUInt32);
begin
 fCommands.CmdUpdateBuffer(commandBuffer,dstBuffer,dstOffset,dataSize,pData);
end;

procedure TVulkan.CmdFillBuffer(commandBuffer:TVkCommandBuffer;dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;size:TVkDeviceSize;data:TVkUInt32);
begin
 fCommands.CmdFillBuffer(commandBuffer,dstBuffer,dstOffset,size,data);
end;

procedure TVulkan.CmdClearColorImage(commandBuffer:TVkCommandBuffer;image:TVkImage;imageLayout:TVkImageLayout;const pColor:PVkClearColorValue;rangeCount:TVkUInt32;const pRanges:PVkImageSubresourceRange);
begin
 fCommands.CmdClearColorImage(commandBuffer,image,imageLayout,pColor,rangeCount,pRanges);
end;

procedure TVulkan.CmdClearDepthStencilImage(commandBuffer:TVkCommandBuffer;image:TVkImage;imageLayout:TVkImageLayout;const pDepthStencil:PVkClearDepthStencilValue;rangeCount:TVkUInt32;const pRanges:PVkImageSubresourceRange);
begin
 fCommands.CmdClearDepthStencilImage(commandBuffer,image,imageLayout,pDepthStencil,rangeCount,pRanges);
end;

procedure TVulkan.CmdClearAttachments(commandBuffer:TVkCommandBuffer;attachmentCount:TVkUInt32;const pAttachments:PVkClearAttachment;rectCount:TVkUInt32;const pRects:PVkClearRect);
begin
 fCommands.CmdClearAttachments(commandBuffer,attachmentCount,pAttachments,rectCount,pRects);
end;

procedure TVulkan.CmdResolveImage(commandBuffer:TVkCommandBuffer;srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkImageResolve);
begin
 fCommands.CmdResolveImage(commandBuffer,srcImage,srcImageLayout,dstImage,dstImageLayout,regionCount,pRegions);
end;

procedure TVulkan.CmdSetEvent(commandBuffer:TVkCommandBuffer;event:TVkEvent;stageMask:TVkPipelineStageFlags);
begin
 fCommands.CmdSetEvent(commandBuffer,event,stageMask);
end;

procedure TVulkan.CmdResetEvent(commandBuffer:TVkCommandBuffer;event:TVkEvent;stageMask:TVkPipelineStageFlags);
begin
 fCommands.CmdResetEvent(commandBuffer,event,stageMask);
end;

procedure TVulkan.CmdWaitEvents(commandBuffer:TVkCommandBuffer;eventCount:TVkUInt32;const pEvents:PVkEvent;srcStageMask:TVkPipelineStageFlags;dstStageMask:TVkPipelineStageFlags;memoryBarrierCount:TVkUInt32;const pMemoryBarriers:PVkMemoryBarrier;bufferMemoryBarrierCount:TVkUInt32;const pBufferMemoryBarriers:PVkBufferMemoryBarrier;imageMemoryBarrierCount:TVkUInt32;const pImageMemoryBarriers:PVkImageMemoryBarrier);
begin
 fCommands.CmdWaitEvents(commandBuffer,eventCount,pEvents,srcStageMask,dstStageMask,memoryBarrierCount,pMemoryBarriers,bufferMemoryBarrierCount,pBufferMemoryBarriers,imageMemoryBarrierCount,pImageMemoryBarriers);
end;

procedure TVulkan.CmdPipelineBarrier(commandBuffer:TVkCommandBuffer;srcStageMask:TVkPipelineStageFlags;dstStageMask:TVkPipelineStageFlags;dependencyFlags:TVkDependencyFlags;memoryBarrierCount:TVkUInt32;const pMemoryBarriers:PVkMemoryBarrier;bufferMemoryBarrierCount:TVkUInt32;const pBufferMemoryBarriers:PVkBufferMemoryBarrier;imageMemoryBarrierCount:TVkUInt32;const pImageMemoryBarriers:PVkImageMemoryBarrier);
begin
 fCommands.CmdPipelineBarrier(commandBuffer,srcStageMask,dstStageMask,dependencyFlags,memoryBarrierCount,pMemoryBarriers,bufferMemoryBarrierCount,pBufferMemoryBarriers,imageMemoryBarrierCount,pImageMemoryBarriers);
end;

procedure TVulkan.CmdBeginQuery(commandBuffer:TVkCommandBuffer;queryPool:TVkQueryPool;query:TVkUInt32;flags:TVkQueryControlFlags);
begin
 fCommands.CmdBeginQuery(commandBuffer,queryPool,query,flags);
end;

procedure TVulkan.CmdEndQuery(commandBuffer:TVkCommandBuffer;queryPool:TVkQueryPool;query:TVkUInt32);
begin
 fCommands.CmdEndQuery(commandBuffer,queryPool,query);
end;

procedure TVulkan.CmdResetQueryPool(commandBuffer:TVkCommandBuffer;queryPool:TVkQueryPool;firstQuery:TVkUInt32;queryCount:TVkUInt32);
begin
 fCommands.CmdResetQueryPool(commandBuffer,queryPool,firstQuery,queryCount);
end;

procedure TVulkan.CmdWriteTimestamp(commandBuffer:TVkCommandBuffer;pipelineStage:TVkPipelineStageFlagBits;queryPool:TVkQueryPool;query:TVkUInt32);
begin
 fCommands.CmdWriteTimestamp(commandBuffer,pipelineStage,queryPool,query);
end;

procedure TVulkan.CmdCopyQueryPoolResults(commandBuffer:TVkCommandBuffer;queryPool:TVkQueryPool;firstQuery:TVkUInt32;queryCount:TVkUInt32;dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;stride:TVkDeviceSize;flags:TVkQueryResultFlags);
begin
 fCommands.CmdCopyQueryPoolResults(commandBuffer,queryPool,firstQuery,queryCount,dstBuffer,dstOffset,stride,flags);
end;

procedure TVulkan.CmdPushConstants(commandBuffer:TVkCommandBuffer;layout:TVkPipelineLayout;stageFlags:TVkShaderStageFlags;offset:TVkUInt32;size:TVkUInt32;const pValues:TVkPointer);
begin
 fCommands.CmdPushConstants(commandBuffer,layout,stageFlags,offset,size,pValues);
end;

procedure TVulkan.CmdBeginRenderPass(commandBuffer:TVkCommandBuffer;const pRenderPassBegin:PVkRenderPassBeginInfo;contents:TVkSubpassContents);
begin
 fCommands.CmdBeginRenderPass(commandBuffer,pRenderPassBegin,contents);
end;

procedure TVulkan.CmdNextSubpass(commandBuffer:TVkCommandBuffer;contents:TVkSubpassContents);
begin
 fCommands.CmdNextSubpass(commandBuffer,contents);
end;

procedure TVulkan.CmdEndRenderPass(commandBuffer:TVkCommandBuffer);
begin
 fCommands.CmdEndRenderPass(commandBuffer);
end;

procedure TVulkan.CmdExecuteCommands(commandBuffer:TVkCommandBuffer;commandBufferCount:TVkUInt32;const pCommandBuffers:PVkCommandBuffer);
begin
 fCommands.CmdExecuteCommands(commandBuffer,commandBufferCount,pCommandBuffers);
end;

{$ifdef Android}
function TVulkan.CreateAndroidSurfaceKHR(instance:TVkInstance;const pCreateInfo:PVkAndroidSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult;
begin
 result:=fCommands.CreateAndroidSurfaceKHR(instance,pCreateInfo,pAllocator,pSurface);
end;
{$endif}

function TVulkan.GetPhysicalDeviceDisplayPropertiesKHR(physicalDevice:TVkPhysicalDevice;pPropertyCount:PVkUInt32;pProperties:PVkDisplayPropertiesKHR):TVkResult;
begin
 result:=fCommands.GetPhysicalDeviceDisplayPropertiesKHR(physicalDevice,pPropertyCount,pProperties);
end;

function TVulkan.GetPhysicalDeviceDisplayPlanePropertiesKHR(physicalDevice:TVkPhysicalDevice;pPropertyCount:PVkUInt32;pProperties:PVkDisplayPlanePropertiesKHR):TVkResult;
begin
 result:=fCommands.GetPhysicalDeviceDisplayPlanePropertiesKHR(physicalDevice,pPropertyCount,pProperties);
end;

function TVulkan.GetDisplayPlaneSupportedDisplaysKHR(physicalDevice:TVkPhysicalDevice;planeIndex:TVkUInt32;pDisplayCount:PVkUInt32;pDisplays:PVkDisplayKHR):TVkResult;
begin
 result:=fCommands.GetDisplayPlaneSupportedDisplaysKHR(physicalDevice,planeIndex,pDisplayCount,pDisplays);
end;

function TVulkan.GetDisplayModePropertiesKHR(physicalDevice:TVkPhysicalDevice;display:TVkDisplayKHR;pPropertyCount:PVkUInt32;pProperties:PVkDisplayModePropertiesKHR):TVkResult;
begin
 result:=fCommands.GetDisplayModePropertiesKHR(physicalDevice,display,pPropertyCount,pProperties);
end;

function TVulkan.CreateDisplayModeKHR(physicalDevice:TVkPhysicalDevice;display:TVkDisplayKHR;const pCreateInfo:PVkDisplayModeCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pMode:PVkDisplayModeKHR):TVkResult;
begin
 result:=fCommands.CreateDisplayModeKHR(physicalDevice,display,pCreateInfo,pAllocator,pMode);
end;

function TVulkan.GetDisplayPlaneCapabilitiesKHR(physicalDevice:TVkPhysicalDevice;mode:TVkDisplayModeKHR;planeIndex:TVkUInt32;pCapabilities:PVkDisplayPlaneCapabilitiesKHR):TVkResult;
begin
 result:=fCommands.GetDisplayPlaneCapabilitiesKHR(physicalDevice,mode,planeIndex,pCapabilities);
end;

function TVulkan.CreateDisplayPlaneSurfaceKHR(instance:TVkInstance;const pCreateInfo:PVkDisplaySurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult;
begin
 result:=fCommands.CreateDisplayPlaneSurfaceKHR(instance,pCreateInfo,pAllocator,pSurface);
end;

function TVulkan.CreateSharedSwapchainsKHR(device:TVkDevice;swapchainCount:TVkUInt32;const pCreateInfos:PVkSwapchainCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSwapchains:PVkSwapchainKHR):TVkResult;
begin
 result:=fCommands.CreateSharedSwapchainsKHR(device,swapchainCount,pCreateInfos,pAllocator,pSwapchains);
end;

{$ifdef Mir}
function TVulkan.CreateMirSurfaceKHR(instance:TVkInstance;const pCreateInfo:PVkMirSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult;
begin
 result:=fCommands.CreateMirSurfaceKHR(instance,pCreateInfo,pAllocator,pSurface);
end;
{$endif}

{$ifdef Mir}
function TVulkan.GetPhysicalDeviceMirPresentationSupportKHR(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;connection:PMirConnection):TVkBool32;
begin
 result:=fCommands.GetPhysicalDeviceMirPresentationSupportKHR(physicalDevice,queueFamilyIndex,connection);
end;
{$endif}

procedure TVulkan.DestroySurfaceKHR(instance:TVkInstance;surface:TVkSurfaceKHR;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroySurfaceKHR(instance,surface,pAllocator);
end;

function TVulkan.GetPhysicalDeviceSurfaceSupportKHR(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;surface:TVkSurfaceKHR;pSupported:PVkBool32):TVkResult;
begin
 result:=fCommands.GetPhysicalDeviceSurfaceSupportKHR(physicalDevice,queueFamilyIndex,surface,pSupported);
end;

function TVulkan.GetPhysicalDeviceSurfaceCapabilitiesKHR(physicalDevice:TVkPhysicalDevice;surface:TVkSurfaceKHR;pSurfaceCapabilities:PVkSurfaceCapabilitiesKHR):TVkResult;
begin
 result:=fCommands.GetPhysicalDeviceSurfaceCapabilitiesKHR(physicalDevice,surface,pSurfaceCapabilities);
end;

function TVulkan.GetPhysicalDeviceSurfaceFormatsKHR(physicalDevice:TVkPhysicalDevice;surface:TVkSurfaceKHR;pSurfaceFormatCount:PVkUInt32;pSurfaceFormats:PVkSurfaceFormatKHR):TVkResult;
begin
 result:=fCommands.GetPhysicalDeviceSurfaceFormatsKHR(physicalDevice,surface,pSurfaceFormatCount,pSurfaceFormats);
end;

function TVulkan.GetPhysicalDeviceSurfacePresentModesKHR(physicalDevice:TVkPhysicalDevice;surface:TVkSurfaceKHR;pPresentModeCount:PVkUInt32;pPresentModes:PVkPresentModeKHR):TVkResult;
begin
 result:=fCommands.GetPhysicalDeviceSurfacePresentModesKHR(physicalDevice,surface,pPresentModeCount,pPresentModes);
end;

function TVulkan.CreateSwapchainKHR(device:TVkDevice;const pCreateInfo:PVkSwapchainCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSwapchain:PVkSwapchainKHR):TVkResult;
begin
 result:=fCommands.CreateSwapchainKHR(device,pCreateInfo,pAllocator,pSwapchain);
end;

procedure TVulkan.DestroySwapchainKHR(device:TVkDevice;swapchain:TVkSwapchainKHR;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroySwapchainKHR(device,swapchain,pAllocator);
end;

function TVulkan.GetSwapchainImagesKHR(device:TVkDevice;swapchain:TVkSwapchainKHR;pSwapchainImageCount:PVkUInt32;pSwapchainImages:PVkImage):TVkResult;
begin
 result:=fCommands.GetSwapchainImagesKHR(device,swapchain,pSwapchainImageCount,pSwapchainImages);
end;

function TVulkan.AcquireNextImageKHR(device:TVkDevice;swapchain:TVkSwapchainKHR;timeout:TVkUInt64;semaphore:TVkSemaphore;fence:TVkFence;pImageIndex:PVkUInt32):TVkResult;
begin
 result:=fCommands.AcquireNextImageKHR(device,swapchain,timeout,semaphore,fence,pImageIndex);
end;

function TVulkan.QueuePresentKHR(queue:TVkQueue;const pPresentInfo:PVkPresentInfoKHR):TVkResult;
begin
 result:=fCommands.QueuePresentKHR(queue,pPresentInfo);
end;

{$ifdef Wayland}
function TVulkan.CreateWaylandSurfaceKHR(instance:TVkInstance;const pCreateInfo:PVkWaylandSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult;
begin
 result:=fCommands.CreateWaylandSurfaceKHR(instance,pCreateInfo,pAllocator,pSurface);
end;
{$endif}

{$ifdef Wayland}
function TVulkan.GetPhysicalDeviceWaylandPresentationSupportKHR(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;display:Pwl_display):TVkBool32;
begin
 result:=fCommands.GetPhysicalDeviceWaylandPresentationSupportKHR(physicalDevice,queueFamilyIndex,display);
end;
{$endif}

function TVulkan.CreateWin32SurfaceKHR(instance:TVkInstance;const pCreateInfo:PVkWin32SurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult;
begin
 result:=fCommands.CreateWin32SurfaceKHR(instance,pCreateInfo,pAllocator,pSurface);
end;

function TVulkan.GetPhysicalDeviceWin32PresentationSupportKHR(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32):TVkBool32;
begin
 result:=fCommands.GetPhysicalDeviceWin32PresentationSupportKHR(physicalDevice,queueFamilyIndex);
end;

{$ifdef X11}
function TVulkan.CreateXlibSurfaceKHR(instance:TVkInstance;const pCreateInfo:PVkXlibSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult;
begin
 result:=fCommands.CreateXlibSurfaceKHR(instance,pCreateInfo,pAllocator,pSurface);
end;
{$endif}

{$ifdef X11}
function TVulkan.GetPhysicalDeviceXlibPresentationSupportKHR(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;dpy:PDisplay;visualID:TVisualID):TVkBool32;
begin
 result:=fCommands.GetPhysicalDeviceXlibPresentationSupportKHR(physicalDevice,queueFamilyIndex,dpy,visualID);
end;
{$endif}

{$ifdef XCB}
function TVulkan.CreateXcbSurfaceKHR(instance:TVkInstance;const pCreateInfo:PVkXcbSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult;
begin
 result:=fCommands.CreateXcbSurfaceKHR(instance,pCreateInfo,pAllocator,pSurface);
end;
{$endif}

{$ifdef XCB}
function TVulkan.GetPhysicalDeviceXcbPresentationSupportKHR(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;connection:Pxcb_connection;visual_id:Txcb_visualid):TVkBool32;
begin
 result:=fCommands.GetPhysicalDeviceXcbPresentationSupportKHR(physicalDevice,queueFamilyIndex,connection,visual_id);
end;
{$endif}

function TVulkan.CreateDebugReportCallbackEXT(instance:TVkInstance;const pCreateInfo:PVkDebugReportCallbackCreateInfoEXT;const pAllocator:PVkAllocationCallbacks;pCallback:PVkDebugReportCallbackEXT):TVkResult;
begin
 result:=fCommands.CreateDebugReportCallbackEXT(instance,pCreateInfo,pAllocator,pCallback);
end;

procedure TVulkan.DestroyDebugReportCallbackEXT(instance:TVkInstance;callback:TVkDebugReportCallbackEXT;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroyDebugReportCallbackEXT(instance,callback,pAllocator);
end;

procedure TVulkan.DebugReportMessageEXT(instance:TVkInstance;flags:TVkDebugReportFlagsEXT;objectType:TVkDebugReportObjectTypeEXT;object_:TVkUInt64;location:TVkPtrInt;messageCode:TVkInt32;const pLayerPrefix:PVkChar;const pMessage:PVkChar);
begin
 fCommands.DebugReportMessageEXT(instance,flags,objectType,object_,location,messageCode,pLayerPrefix,pMessage);
end;

initialization
 vk:=TVulkan.Create;
finalization
 if assigned(LibVulkan) then begin
  vkFreeLibrary(LibVulkan);
 end;
 vk.Free;
end.
