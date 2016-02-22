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
type PVkInt8=^TVkInt8;
     TVkInt8=shortint;
     PVkUInt8=^TVkUInt8;
     TVkUInt8=byte;
     PVkInt16=^TVkInt16;
     TVkInt16=smallint;
     PVkUInt16=^TVkUInt16;
     TVkUInt16=word;
     PVkInt32=^TVkInt32;
     TVkInt32=longint;
     PVkUInt32=^TVkUInt32;
     TVkUInt32=longword;
     PVkInt64=^TVkInt64;
     TVkInt64=int64;
     PVkUInt64=^TVkUInt64;
     TVkUInt64=uint64;
     PVkChar=^TVkChar;
     TVkChar=ansichar;
     PVkPointer=^TVkPointer;
     TVkPointer=pointer;
     PVkFloat=^TVkFloat;
     TVkFloat=single;
     PVkDouble=^TVkDouble;
     TVkDouble=double;
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
type PVkDispatchableHandle=^TVkDispatchableHandle;
     TVkDispatchableHandle=TVkPtrInt;
     PVkNonDispatchableHandle=^TVkNonDispatchableHandle;
     TVkNonDispatchableHandle=TVkUInt64;
     PVkEnum=^TVkEnum;
     TVkEnum=TVkInt32;
{$ifdef Windows}
     PVkHINSTANCE=^TVkHINSTANCE;
     TVkHINSTANCE=TVkPtrUInt;
     PVkHWND=^TVkHWND;
     TVkHWND=HWND;
{$endif}
     PVkSampleMask=^TVkSampleMask;
     TVkSampleMask=TVkUInt32;
     PVkBool32=^TVkBool32;
     TVkBool32=TVkUInt32;
     PVkFlags=^TVkFlags;
     TVkFlags=TVkUInt32;
     PVkDeviceSize=^TVkDeviceSize;
     TVkDeviceSize=TVkUInt64;
     PVkFramebufferCreateFlags=^TVkFramebufferCreateFlags;
     TVkFramebufferCreateFlags=TVkFlags;
     PVkQueryPoolCreateFlags=^TVkQueryPoolCreateFlags;
     TVkQueryPoolCreateFlags=TVkFlags;
     PVkRenderPassCreateFlags=^TVkRenderPassCreateFlags;
     TVkRenderPassCreateFlags=TVkFlags;
     PVkSamplerCreateFlags=^TVkSamplerCreateFlags;
     TVkSamplerCreateFlags=TVkFlags;
     PVkPipelineLayoutCreateFlags=^TVkPipelineLayoutCreateFlags;
     TVkPipelineLayoutCreateFlags=TVkFlags;
     PVkPipelineCacheCreateFlags=^TVkPipelineCacheCreateFlags;
     TVkPipelineCacheCreateFlags=TVkFlags;
     PVkPipelineDepthStencilStateCreateFlags=^TVkPipelineDepthStencilStateCreateFlags;
     TVkPipelineDepthStencilStateCreateFlags=TVkFlags;
     PVkPipelineDynamicStateCreateFlags=^TVkPipelineDynamicStateCreateFlags;
     TVkPipelineDynamicStateCreateFlags=TVkFlags;
     PVkPipelineColorBlendStateCreateFlags=^TVkPipelineColorBlendStateCreateFlags;
     TVkPipelineColorBlendStateCreateFlags=TVkFlags;
     PVkPipelineMultisampleStateCreateFlags=^TVkPipelineMultisampleStateCreateFlags;
     TVkPipelineMultisampleStateCreateFlags=TVkFlags;
     PVkPipelineRasterizationStateCreateFlags=^TVkPipelineRasterizationStateCreateFlags;
     TVkPipelineRasterizationStateCreateFlags=TVkFlags;
     PVkPipelineViewportStateCreateFlags=^TVkPipelineViewportStateCreateFlags;
     TVkPipelineViewportStateCreateFlags=TVkFlags;
     PVkPipelineTessellationStateCreateFlags=^TVkPipelineTessellationStateCreateFlags;
     TVkPipelineTessellationStateCreateFlags=TVkFlags;
     PVkPipelineInputAssemblyStateCreateFlags=^TVkPipelineInputAssemblyStateCreateFlags;
     TVkPipelineInputAssemblyStateCreateFlags=TVkFlags;
     PVkPipelineVertexInputStateCreateFlags=^TVkPipelineVertexInputStateCreateFlags;
     TVkPipelineVertexInputStateCreateFlags=TVkFlags;
     PVkPipelineShaderStageCreateFlags=^TVkPipelineShaderStageCreateFlags;
     TVkPipelineShaderStageCreateFlags=TVkFlags;
     PVkDescriptorSetLayoutCreateFlags=^TVkDescriptorSetLayoutCreateFlags;
     TVkDescriptorSetLayoutCreateFlags=TVkFlags;
     PVkBufferViewCreateFlags=^TVkBufferViewCreateFlags;
     TVkBufferViewCreateFlags=TVkFlags;
     PVkInstanceCreateFlags=^TVkInstanceCreateFlags;
     TVkInstanceCreateFlags=TVkFlags;
     PVkDeviceCreateFlags=^TVkDeviceCreateFlags;
     TVkDeviceCreateFlags=TVkFlags;
     PVkDeviceQueueCreateFlags=^TVkDeviceQueueCreateFlags;
     TVkDeviceQueueCreateFlags=TVkFlags;
     PVkQueueFlags=^TVkQueueFlags;
     TVkQueueFlags=TVkFlags;
     PVkMemoryPropertyFlags=^TVkMemoryPropertyFlags;
     TVkMemoryPropertyFlags=TVkFlags;
     PVkMemoryHeapFlags=^TVkMemoryHeapFlags;
     TVkMemoryHeapFlags=TVkFlags;
     PVkAccessFlags=^TVkAccessFlags;
     TVkAccessFlags=TVkFlags;
     PVkBufferUsageFlags=^TVkBufferUsageFlags;
     TVkBufferUsageFlags=TVkFlags;
     PVkBufferCreateFlags=^TVkBufferCreateFlags;
     TVkBufferCreateFlags=TVkFlags;
     PVkShaderStageFlags=^TVkShaderStageFlags;
     TVkShaderStageFlags=TVkFlags;
     PVkImageUsageFlags=^TVkImageUsageFlags;
     TVkImageUsageFlags=TVkFlags;
     PVkImageCreateFlags=^TVkImageCreateFlags;
     TVkImageCreateFlags=TVkFlags;
     PVkImageViewCreateFlags=^TVkImageViewCreateFlags;
     TVkImageViewCreateFlags=TVkFlags;
     PVkPipelineCreateFlags=^TVkPipelineCreateFlags;
     TVkPipelineCreateFlags=TVkFlags;
     PVkColorComponentFlags=^TVkColorComponentFlags;
     TVkColorComponentFlags=TVkFlags;
     PVkFenceCreateFlags=^TVkFenceCreateFlags;
     TVkFenceCreateFlags=TVkFlags;
     PVkSemaphoreCreateFlags=^TVkSemaphoreCreateFlags;
     TVkSemaphoreCreateFlags=TVkFlags;
     PVkFormatFeatureFlags=^TVkFormatFeatureFlags;
     TVkFormatFeatureFlags=TVkFlags;
     PVkQueryControlFlags=^TVkQueryControlFlags;
     TVkQueryControlFlags=TVkFlags;
     PVkQueryResultFlags=^TVkQueryResultFlags;
     TVkQueryResultFlags=TVkFlags;
     PVkShaderModuleCreateFlags=^TVkShaderModuleCreateFlags;
     TVkShaderModuleCreateFlags=TVkFlags;
     PVkEventCreateFlags=^TVkEventCreateFlags;
     TVkEventCreateFlags=TVkFlags;
     PVkCommandPoolCreateFlags=^TVkCommandPoolCreateFlags;
     TVkCommandPoolCreateFlags=TVkFlags;
     PVkCommandPoolResetFlags=^TVkCommandPoolResetFlags;
     TVkCommandPoolResetFlags=TVkFlags;
     PVkCommandBufferResetFlags=^TVkCommandBufferResetFlags;
     TVkCommandBufferResetFlags=TVkFlags;
     PVkCommandBufferUsageFlags=^TVkCommandBufferUsageFlags;
     TVkCommandBufferUsageFlags=TVkFlags;
     PVkQueryPipelineStatisticFlags=^TVkQueryPipelineStatisticFlags;
     TVkQueryPipelineStatisticFlags=TVkFlags;
     PVkMemoryMapFlags=^TVkMemoryMapFlags;
     TVkMemoryMapFlags=TVkFlags;
     PVkImageAspectFlags=^TVkImageAspectFlags;
     TVkImageAspectFlags=TVkFlags;
     PVkSparseMemoryBindFlags=^TVkSparseMemoryBindFlags;
     TVkSparseMemoryBindFlags=TVkFlags;
     PVkSparseImageFormatFlags=^TVkSparseImageFormatFlags;
     TVkSparseImageFormatFlags=TVkFlags;
     PVkSubpassDescriptionFlags=^TVkSubpassDescriptionFlags;
     TVkSubpassDescriptionFlags=TVkFlags;
     PVkPipelineStageFlags=^TVkPipelineStageFlags;
     TVkPipelineStageFlags=TVkFlags;
     PVkSampleCountFlags=^TVkSampleCountFlags;
     TVkSampleCountFlags=TVkFlags;
     PVkAttachmentDescriptionFlags=^TVkAttachmentDescriptionFlags;
     TVkAttachmentDescriptionFlags=TVkFlags;
     PVkStencilFaceFlags=^TVkStencilFaceFlags;
     TVkStencilFaceFlags=TVkFlags;
     PVkCullModeFlags=^TVkCullModeFlags;
     TVkCullModeFlags=TVkFlags;
     PVkDescriptorPoolCreateFlags=^TVkDescriptorPoolCreateFlags;
     TVkDescriptorPoolCreateFlags=TVkFlags;
     PVkDescriptorPoolResetFlags=^TVkDescriptorPoolResetFlags;
     TVkDescriptorPoolResetFlags=TVkFlags;
     PVkDependencyFlags=^TVkDependencyFlags;
     TVkDependencyFlags=TVkFlags;
     PVkCompositeAlphaFlagsKHR=^TVkCompositeAlphaFlagsKHR;
     TVkCompositeAlphaFlagsKHR=TVkFlags;
     PVkDisplayPlaneAlphaFlagsKHR=^TVkDisplayPlaneAlphaFlagsKHR;
     TVkDisplayPlaneAlphaFlagsKHR=TVkFlags;
     PVkSurfaceTransformFlagsKHR=^TVkSurfaceTransformFlagsKHR;
     TVkSurfaceTransformFlagsKHR=TVkFlags;
     PVkSwapchainCreateFlagsKHR=^TVkSwapchainCreateFlagsKHR;
     TVkSwapchainCreateFlagsKHR=TVkFlags;
     PVkDisplayModeCreateFlagsKHR=^TVkDisplayModeCreateFlagsKHR;
     TVkDisplayModeCreateFlagsKHR=TVkFlags;
     PVkDisplaySurfaceCreateFlagsKHR=^TVkDisplaySurfaceCreateFlagsKHR;
     TVkDisplaySurfaceCreateFlagsKHR=TVkFlags;
     PVkAndroidSurfaceCreateFlagsKHR=^TVkAndroidSurfaceCreateFlagsKHR;
     TVkAndroidSurfaceCreateFlagsKHR=TVkFlags;
     PVkMirSurfaceCreateFlagsKHR=^TVkMirSurfaceCreateFlagsKHR;
     TVkMirSurfaceCreateFlagsKHR=TVkFlags;
     PVkWaylandSurfaceCreateFlagsKHR=^TVkWaylandSurfaceCreateFlagsKHR;
     TVkWaylandSurfaceCreateFlagsKHR=TVkFlags;
     PVkWin32SurfaceCreateFlagsKHR=^TVkWin32SurfaceCreateFlagsKHR;
     TVkWin32SurfaceCreateFlagsKHR=TVkFlags;
     PVkXlibSurfaceCreateFlagsKHR=^TVkXlibSurfaceCreateFlagsKHR;
     TVkXlibSurfaceCreateFlagsKHR=TVkFlags;
     PVkXcbSurfaceCreateFlagsKHR=^TVkXcbSurfaceCreateFlagsKHR;
     TVkXcbSurfaceCreateFlagsKHR=TVkFlags;
     PVkDebugReportFlagsEXT=^TVkDebugReportFlagsEXT;
     TVkDebugReportFlagsEXT=TVkFlags;
     PVkInstance=^TVkInstance;
     TVkInstance=TVkDispatchableHandle;
     PVkPhysicalDevice=^TVkPhysicalDevice;
     TVkPhysicalDevice=TVkDispatchableHandle;
     PVkDevice=^TVkDevice;
     TVkDevice=TVkDispatchableHandle;
     PVkQueue=^TVkQueue;
     TVkQueue=TVkDispatchableHandle;
     PVkCommandBuffer=^TVkCommandBuffer;
     TVkCommandBuffer=TVkDispatchableHandle;
     PVkDeviceMemory=^TVkDeviceMemory;
     TVkDeviceMemory=TVkNonDispatchableHandle;
     PVkCommandPool=^TVkCommandPool;
     TVkCommandPool=TVkNonDispatchableHandle;
     PVkBuffer=^TVkBuffer;
     TVkBuffer=TVkNonDispatchableHandle;
     PVkBufferView=^TVkBufferView;
     TVkBufferView=TVkNonDispatchableHandle;
     PVkImage=^TVkImage;
     TVkImage=TVkNonDispatchableHandle;
     PVkImageView=^TVkImageView;
     TVkImageView=TVkNonDispatchableHandle;
     PVkShaderModule=^TVkShaderModule;
     TVkShaderModule=TVkNonDispatchableHandle;
     PVkPipeline=^TVkPipeline;
     TVkPipeline=TVkNonDispatchableHandle;
     PVkPipelineLayout=^TVkPipelineLayout;
     TVkPipelineLayout=TVkNonDispatchableHandle;
     PVkSampler=^TVkSampler;
     TVkSampler=TVkNonDispatchableHandle;
     PVkDescriptorSet=^TVkDescriptorSet;
     TVkDescriptorSet=TVkNonDispatchableHandle;
     PVkDescriptorSetLayout=^TVkDescriptorSetLayout;
     TVkDescriptorSetLayout=TVkNonDispatchableHandle;
     PVkDescriptorPool=^TVkDescriptorPool;
     TVkDescriptorPool=TVkNonDispatchableHandle;
     PVkFence=^TVkFence;
     TVkFence=TVkNonDispatchableHandle;
     PVkSemaphore=^TVkSemaphore;
     TVkSemaphore=TVkNonDispatchableHandle;
     PVkEvent=^TVkEvent;
     TVkEvent=TVkNonDispatchableHandle;
     PVkQueryPool=^TVkQueryPool;
     TVkQueryPool=TVkNonDispatchableHandle;
     PVkFramebuffer=^TVkFramebuffer;
     TVkFramebuffer=TVkNonDispatchableHandle;
     PVkRenderPass=^TVkRenderPass;
     TVkRenderPass=TVkNonDispatchableHandle;
     PVkPipelineCache=^TVkPipelineCache;
     TVkPipelineCache=TVkNonDispatchableHandle;
     PVkDisplayKHR=^TVkDisplayKHR;
     TVkDisplayKHR=TVkNonDispatchableHandle;
     PVkDisplayModeKHR=^TVkDisplayModeKHR;
     TVkDisplayModeKHR=TVkNonDispatchableHandle;
     PVkSurfaceKHR=^TVkSurfaceKHR;
     TVkSurfaceKHR=TVkNonDispatchableHandle;
     PVkSwapchainKHR=^TVkSwapchainKHR;
     TVkSwapchainKHR=TVkNonDispatchableHandle;
     PVkDebugReportCallbackEXT=^TVkDebugReportCallbackEXT;
     TVkDebugReportCallbackEXT=TVkNonDispatchableHandle;
     PVkImageLayout=^TVkImageLayout;
     TVkImageLayout=
      (
       VK_IMAGE_LAYOUT_UNDEFINED=0, // Implicit layout an image is when its contents are undefined due to various reasons (e.g. right after creation)
       VK_IMAGE_LAYOUT_BEGIN_RANGE=0, // VK_IMAGE_LAYOUT_UNDEFINED
       VK_IMAGE_LAYOUT_GENERAL=1, // General layout when image can be used for any kind of access
       VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL=2, // Optimal layout when image is only used for color attachment read/write
       VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL=3, // Optimal layout when image is only used for depth/stencil attachment read/write
       VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL=4, // Optimal layout when image is used for read only depth/stencil attachment and shader access
       VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL=5, // Optimal layout when image is used for read only shader access
       VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL=6, // Optimal layout when image is used only as source of transfer operations
       VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL=7, // Optimal layout when image is used only as destination of transfer operations
       VK_IMAGE_LAYOUT_PREINITIALIZED=8, // Initial layout used when the data is populated by the CPU
       VK_IMAGE_LAYOUT_END_RANGE=8, // VK_IMAGE_LAYOUT_PREINITIALIZED
       VK_IMAGE_LAYOUT_RANGE_SIZE=9, // (VK_IMAGE_LAYOUT_PREINITIALIZED-VK_IMAGE_LAYOUT_UNDEFINED)+1
       VK_IMAGE_LAYOUT_PRESENT_SRC_KHR=1000001002,
       VK_IMAGE_LAYOUT_MAX_ENUM=$7fffffff
      );
     PVkAttachmentLoadOp=^TVkAttachmentLoadOp;
     TVkAttachmentLoadOp=
      (
       VK_ATTACHMENT_LOAD_OP_LOAD=0,
       VK_ATTACHMENT_LOAD_OP_BEGIN_RANGE=0, // VK_ATTACHMENT_LOAD_OP_LOAD
       VK_ATTACHMENT_LOAD_OP_CLEAR=1,
       VK_ATTACHMENT_LOAD_OP_DONT_CARE=2,
       VK_ATTACHMENT_LOAD_OP_END_RANGE=2, // VK_ATTACHMENT_LOAD_OP_DONT_CARE
       VK_ATTACHMENT_LOAD_OP_RANGE_SIZE=3, // (VK_ATTACHMENT_LOAD_OP_DONT_CARE-VK_ATTACHMENT_LOAD_OP_LOAD)+1
       VK_ATTACHMENT_LOAD_OP_MAX_ENUM=$7fffffff
      );
     PVkAttachmentStoreOp=^TVkAttachmentStoreOp;
     TVkAttachmentStoreOp=
      (
       VK_ATTACHMENT_STORE_OP_STORE=0,
       VK_ATTACHMENT_STORE_OP_BEGIN_RANGE=0, // VK_ATTACHMENT_STORE_OP_STORE
       VK_ATTACHMENT_STORE_OP_DONT_CARE=1,
       VK_ATTACHMENT_STORE_OP_END_RANGE=1, // VK_ATTACHMENT_STORE_OP_DONT_CARE
       VK_ATTACHMENT_STORE_OP_RANGE_SIZE=2, // (VK_ATTACHMENT_STORE_OP_DONT_CARE-VK_ATTACHMENT_STORE_OP_STORE)+1
       VK_ATTACHMENT_STORE_OP_MAX_ENUM=$7fffffff
      );
     PVkImageType=^TVkImageType;
     TVkImageType=
      (
       VK_IMAGE_TYPE_1D=0,
       VK_IMAGE_TYPE_BEGIN_RANGE=0, // VK_IMAGE_TYPE_1D
       VK_IMAGE_TYPE_2D=1,
       VK_IMAGE_TYPE_3D=2,
       VK_IMAGE_TYPE_END_RANGE=2, // VK_IMAGE_TYPE_3D
       VK_IMAGE_TYPE_RANGE_SIZE=3, // (VK_IMAGE_TYPE_3D-VK_IMAGE_TYPE_1D)+1
       VK_IMAGE_TYPE_MAX_ENUM=$7fffffff
      );
     PVkImageTiling=^TVkImageTiling;
     TVkImageTiling=
      (
       VK_IMAGE_TILING_OPTIMAL=0,
       VK_IMAGE_TILING_BEGIN_RANGE=0, // VK_IMAGE_TILING_OPTIMAL
       VK_IMAGE_TILING_LINEAR=1,
       VK_IMAGE_TILING_END_RANGE=1, // VK_IMAGE_TILING_LINEAR
       VK_IMAGE_TILING_RANGE_SIZE=2, // (VK_IMAGE_TILING_LINEAR-VK_IMAGE_TILING_OPTIMAL)+1
       VK_IMAGE_TILING_MAX_ENUM=$7fffffff
      );
     PVkImageViewType=^TVkImageViewType;
     TVkImageViewType=
      (
       VK_IMAGE_VIEW_TYPE_1D=0,
       VK_IMAGE_VIEW_TYPE_BEGIN_RANGE=0, // VK_IMAGE_VIEW_TYPE_1D
       VK_IMAGE_VIEW_TYPE_2D=1,
       VK_IMAGE_VIEW_TYPE_3D=2,
       VK_IMAGE_VIEW_TYPE_CUBE=3,
       VK_IMAGE_VIEW_TYPE_1D_ARRAY=4,
       VK_IMAGE_VIEW_TYPE_2D_ARRAY=5,
       VK_IMAGE_VIEW_TYPE_CUBE_ARRAY=6,
       VK_IMAGE_VIEW_TYPE_END_RANGE=6, // VK_IMAGE_VIEW_TYPE_CUBE_ARRAY
       VK_IMAGE_VIEW_TYPE_RANGE_SIZE=7, // (VK_IMAGE_VIEW_TYPE_CUBE_ARRAY-VK_IMAGE_VIEW_TYPE_1D)+1
       VK_IMAGE_VIEW_TYPE_MAX_ENUM=$7fffffff
      );
     PVkCommandBufferLevel=^TVkCommandBufferLevel;
     TVkCommandBufferLevel=
      (
       VK_COMMAND_BUFFER_LEVEL_PRIMARY=0,
       VK_COMMAND_BUFFER_LEVEL_BEGIN_RANGE=0, // VK_COMMAND_BUFFER_LEVEL_PRIMARY
       VK_COMMAND_BUFFER_LEVEL_SECONDARY=1,
       VK_COMMAND_BUFFER_LEVEL_END_RANGE=1, // VK_COMMAND_BUFFER_LEVEL_SECONDARY
       VK_COMMAND_BUFFER_LEVEL_RANGE_SIZE=2, // (VK_COMMAND_BUFFER_LEVEL_SECONDARY-VK_COMMAND_BUFFER_LEVEL_PRIMARY)+1
       VK_COMMAND_BUFFER_LEVEL_MAX_ENUM=$7fffffff
      );
     PVkComponentSwizzle=^TVkComponentSwizzle;
     TVkComponentSwizzle=
      (
       VK_COMPONENT_SWIZZLE_IDENTITY=0,
       VK_COMPONENT_SWIZZLE_BEGIN_RANGE=0, // VK_COMPONENT_SWIZZLE_IDENTITY
       VK_COMPONENT_SWIZZLE_ZERO=1,
       VK_COMPONENT_SWIZZLE_ONE=2,
       VK_COMPONENT_SWIZZLE_R=3,
       VK_COMPONENT_SWIZZLE_G=4,
       VK_COMPONENT_SWIZZLE_B=5,
       VK_COMPONENT_SWIZZLE_A=6,
       VK_COMPONENT_SWIZZLE_END_RANGE=6, // VK_COMPONENT_SWIZZLE_A
       VK_COMPONENT_SWIZZLE_RANGE_SIZE=7, // (VK_COMPONENT_SWIZZLE_A-VK_COMPONENT_SWIZZLE_IDENTITY)+1
       VK_COMPONENT_SWIZZLE_MAX_ENUM=$7fffffff
      );
     PVkDescriptorType=^TVkDescriptorType;
     TVkDescriptorType=
      (
       VK_DESCRIPTOR_TYPE_SAMPLER=0,
       VK_DESCRIPTOR_TYPE_BEGIN_RANGE=0, // VK_DESCRIPTOR_TYPE_SAMPLER
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
       VK_DESCRIPTOR_TYPE_END_RANGE=10, // VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT
       VK_DESCRIPTOR_TYPE_RANGE_SIZE=11, // (VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT-VK_DESCRIPTOR_TYPE_SAMPLER)+1
       VK_DESCRIPTOR_TYPE_MAX_ENUM=$7fffffff
      );
     PVkQueryType=^TVkQueryType;
     TVkQueryType=
      (
       VK_QUERY_TYPE_OCCLUSION=0,
       VK_QUERY_TYPE_BEGIN_RANGE=0, // VK_QUERY_TYPE_OCCLUSION
       VK_QUERY_TYPE_PIPELINE_STATISTICS=1, // Optional
       VK_QUERY_TYPE_TIMESTAMP=2,
       VK_QUERY_TYPE_END_RANGE=2, // VK_QUERY_TYPE_TIMESTAMP
       VK_QUERY_TYPE_RANGE_SIZE=3, // (VK_QUERY_TYPE_TIMESTAMP-VK_QUERY_TYPE_OCCLUSION)+1
       VK_QUERY_TYPE_MAX_ENUM=$7fffffff
      );
     PVkBorderColor=^TVkBorderColor;
     TVkBorderColor=
      (
       VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK=0,
       VK_BORDER_COLOR_BEGIN_RANGE=0, // VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK
       VK_BORDER_COLOR_INT_TRANSPARENT_BLACK=1,
       VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK=2,
       VK_BORDER_COLOR_INT_OPAQUE_BLACK=3,
       VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE=4,
       VK_BORDER_COLOR_INT_OPAQUE_WHITE=5,
       VK_BORDER_COLOR_END_RANGE=5, // VK_BORDER_COLOR_INT_OPAQUE_WHITE
       VK_BORDER_COLOR_RANGE_SIZE=6, // (VK_BORDER_COLOR_INT_OPAQUE_WHITE-VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK)+1
       VK_BORDER_COLOR_MAX_ENUM=$7fffffff
      );
     PVkPipelineBindPoint=^TVkPipelineBindPoint;
     TVkPipelineBindPoint=
      (
       VK_PIPELINE_BIND_POINT_GRAPHICS=0,
       VK_PIPELINE_BIND_POINT_BEGIN_RANGE=0, // VK_PIPELINE_BIND_POINT_GRAPHICS
       VK_PIPELINE_BIND_POINT_COMPUTE=1,
       VK_PIPELINE_BIND_POINT_END_RANGE=1, // VK_PIPELINE_BIND_POINT_COMPUTE
       VK_PIPELINE_BIND_POINT_RANGE_SIZE=2, // (VK_PIPELINE_BIND_POINT_COMPUTE-VK_PIPELINE_BIND_POINT_GRAPHICS)+1
       VK_PIPELINE_BIND_POINT_MAX_ENUM=$7fffffff
      );
     PVkPipelineCacheHeaderVersion=^TVkPipelineCacheHeaderVersion;
     TVkPipelineCacheHeaderVersion=
      (
       VK_PIPELINE_CACHE_HEADER_VERSION_ONE=1,
       VK_PIPELINE_CACHE_HEADER_VERSION_BEGIN_RANGE=1, // VK_PIPELINE_CACHE_HEADER_VERSION_ONE
       VK_PIPELINE_CACHE_HEADER_VERSION_END_RANGE=1, // VK_PIPELINE_CACHE_HEADER_VERSION_ONE
       VK_PIPELINE_CACHE_HEADER_VERSION_RANGE_SIZE=1, // (VK_PIPELINE_CACHE_HEADER_VERSION_ONE-VK_PIPELINE_CACHE_HEADER_VERSION_ONE)+1
       VK_PIPELINE_CACHE_HEADER_VERSION_MAX_ENUM=$7fffffff
      );
     PVkPrimitiveTopology=^TVkPrimitiveTopology;
     TVkPrimitiveTopology=
      (
       VK_PRIMITIVE_TOPOLOGY_POINT_LIST=0,
       VK_PRIMITIVE_TOPOLOGY_BEGIN_RANGE=0, // VK_PRIMITIVE_TOPOLOGY_POINT_LIST
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
       VK_PRIMITIVE_TOPOLOGY_END_RANGE=10, // VK_PRIMITIVE_TOPOLOGY_PATCH_LIST
       VK_PRIMITIVE_TOPOLOGY_RANGE_SIZE=11, // (VK_PRIMITIVE_TOPOLOGY_PATCH_LIST-VK_PRIMITIVE_TOPOLOGY_POINT_LIST)+1
       VK_PRIMITIVE_TOPOLOGY_MAX_ENUM=$7fffffff
      );
     PVkSharingMode=^TVkSharingMode;
     TVkSharingMode=
      (
       VK_SHARING_MODE_EXCLUSIVE=0,
       VK_SHARING_MODE_BEGIN_RANGE=0, // VK_SHARING_MODE_EXCLUSIVE
       VK_SHARING_MODE_CONCURRENT=1,
       VK_SHARING_MODE_END_RANGE=1, // VK_SHARING_MODE_CONCURRENT
       VK_SHARING_MODE_RANGE_SIZE=2, // (VK_SHARING_MODE_CONCURRENT-VK_SHARING_MODE_EXCLUSIVE)+1
       VK_SHARING_MODE_MAX_ENUM=$7fffffff
      );
     PVkIndexType=^TVkIndexType;
     TVkIndexType=
      (
       VK_INDEX_TYPE_UINT16=0,
       VK_INDEX_TYPE_BEGIN_RANGE=0, // VK_INDEX_TYPE_UINT16
       VK_INDEX_TYPE_UINT32=1,
       VK_INDEX_TYPE_END_RANGE=1, // VK_INDEX_TYPE_UINT32
       VK_INDEX_TYPE_RANGE_SIZE=2, // (VK_INDEX_TYPE_UINT32-VK_INDEX_TYPE_UINT16)+1
       VK_INDEX_TYPE_MAX_ENUM=$7fffffff
      );
     PVkFilter=^TVkFilter;
     TVkFilter=
      (
       VK_FILTER_NEAREST=0,
       VK_FILTER_BEGIN_RANGE=0, // VK_FILTER_NEAREST
       VK_FILTER_LINEAR=1,
       VK_FILTER_END_RANGE=1, // VK_FILTER_LINEAR
       VK_FILTER_RANGE_SIZE=2, // (VK_FILTER_LINEAR-VK_FILTER_NEAREST)+1
       VK_FILTER_MAX_ENUM=$7fffffff
      );
     PVkSamplerMipmapMode=^TVkSamplerMipmapMode;
     TVkSamplerMipmapMode=
      (
       VK_SAMPLER_MIPMAP_MODE_NEAREST=0, // Choose nearest mip level
       VK_SAMPLER_MIPMAP_MODE_BEGIN_RANGE=0, // VK_SAMPLER_MIPMAP_MODE_NEAREST
       VK_SAMPLER_MIPMAP_MODE_LINEAR=1, // Linear filter between mip levels
       VK_SAMPLER_MIPMAP_MODE_END_RANGE=1, // VK_SAMPLER_MIPMAP_MODE_LINEAR
       VK_SAMPLER_MIPMAP_MODE_RANGE_SIZE=2, // (VK_SAMPLER_MIPMAP_MODE_LINEAR-VK_SAMPLER_MIPMAP_MODE_NEAREST)+1
       VK_SAMPLER_MIPMAP_MODE_MAX_ENUM=$7fffffff
      );
     PVkSamplerAddressMode=^TVkSamplerAddressMode;
     TVkSamplerAddressMode=
      (
       VK_SAMPLER_ADDRESS_MODE_REPEAT=0,
       VK_SAMPLER_ADDRESS_MODE_BEGIN_RANGE=0, // VK_SAMPLER_ADDRESS_MODE_REPEAT
       VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT=1,
       VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE=2,
       VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER=3,
       VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE=4,
       VK_SAMPLER_ADDRESS_MODE_END_RANGE=4, // VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE
       VK_SAMPLER_ADDRESS_MODE_RANGE_SIZE=5, // (VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE-VK_SAMPLER_ADDRESS_MODE_REPEAT)+1
       VK_SAMPLER_ADDRESS_MODE_MAX_ENUM=$7fffffff
      );
     PVkCompareOp=^TVkCompareOp;
     TVkCompareOp=
      (
       VK_COMPARE_OP_NEVER=0,
       VK_COMPARE_OP_BEGIN_RANGE=0, // VK_COMPARE_OP_NEVER
       VK_COMPARE_OP_LESS=1,
       VK_COMPARE_OP_EQUAL=2,
       VK_COMPARE_OP_LESS_OR_EQUAL=3,
       VK_COMPARE_OP_GREATER=4,
       VK_COMPARE_OP_NOT_EQUAL=5,
       VK_COMPARE_OP_GREATER_OR_EQUAL=6,
       VK_COMPARE_OP_ALWAYS=7,
       VK_COMPARE_OP_END_RANGE=7, // VK_COMPARE_OP_ALWAYS
       VK_COMPARE_OP_RANGE_SIZE=8, // (VK_COMPARE_OP_ALWAYS-VK_COMPARE_OP_NEVER)+1
       VK_COMPARE_OP_MAX_ENUM=$7fffffff
      );
     PVkPolygonMode=^TVkPolygonMode;
     TVkPolygonMode=
      (
       VK_POLYGON_MODE_FILL=0,
       VK_POLYGON_MODE_BEGIN_RANGE=0, // VK_POLYGON_MODE_FILL
       VK_POLYGON_MODE_LINE=1,
       VK_POLYGON_MODE_POINT=2,
       VK_POLYGON_MODE_END_RANGE=2, // VK_POLYGON_MODE_POINT
       VK_POLYGON_MODE_RANGE_SIZE=3, // (VK_POLYGON_MODE_POINT-VK_POLYGON_MODE_FILL)+1
       VK_POLYGON_MODE_MAX_ENUM=$7fffffff
      );
     PVkCullModeFlagBits=^TVkCullModeFlagBits;
     TVkCullModeFlagBits=
      (
       VK_CULL_MODE_NONE=$00000001,
       VK_CULL_MODE_FRONT_BIT=$00000001,
       VK_CULL_MODE_FRONT_AND_BACK=$00000001,
       VK_CULL_MODE_BACK_BIT=$00000002
      );
     PVkFrontFace=^TVkFrontFace;
     TVkFrontFace=
      (
       VK_FRONT_FACE_COUNTER_CLOCKWISE=0,
       VK_FRONT_FACE_BEGIN_RANGE=0, // VK_FRONT_FACE_COUNTER_CLOCKWISE
       VK_FRONT_FACE_CLOCKWISE=1,
       VK_FRONT_FACE_END_RANGE=1, // VK_FRONT_FACE_CLOCKWISE
       VK_FRONT_FACE_RANGE_SIZE=2, // (VK_FRONT_FACE_CLOCKWISE-VK_FRONT_FACE_COUNTER_CLOCKWISE)+1
       VK_FRONT_FACE_MAX_ENUM=$7fffffff
      );
     PVkBlendFactor=^TVkBlendFactor;
     TVkBlendFactor=
      (
       VK_BLEND_FACTOR_ZERO=0,
       VK_BLEND_FACTOR_BEGIN_RANGE=0, // VK_BLEND_FACTOR_ZERO
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
       VK_BLEND_FACTOR_END_RANGE=18, // VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA
       VK_BLEND_FACTOR_RANGE_SIZE=19, // (VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA-VK_BLEND_FACTOR_ZERO)+1
       VK_BLEND_FACTOR_MAX_ENUM=$7fffffff
      );
     PVkBlendOp=^TVkBlendOp;
     TVkBlendOp=
      (
       VK_BLEND_OP_ADD=0,
       VK_BLEND_OP_BEGIN_RANGE=0, // VK_BLEND_OP_ADD
       VK_BLEND_OP_SUBTRACT=1,
       VK_BLEND_OP_REVERSE_SUBTRACT=2,
       VK_BLEND_OP_MIN=3,
       VK_BLEND_OP_MAX=4,
       VK_BLEND_OP_END_RANGE=4, // VK_BLEND_OP_MAX
       VK_BLEND_OP_RANGE_SIZE=5, // (VK_BLEND_OP_MAX-VK_BLEND_OP_ADD)+1
       VK_BLEND_OP_MAX_ENUM=$7fffffff
      );
     PVkStencilOp=^TVkStencilOp;
     TVkStencilOp=
      (
       VK_STENCIL_OP_KEEP=0,
       VK_STENCIL_OP_BEGIN_RANGE=0, // VK_STENCIL_OP_KEEP
       VK_STENCIL_OP_ZERO=1,
       VK_STENCIL_OP_REPLACE=2,
       VK_STENCIL_OP_INCREMENT_AND_CLAMP=3,
       VK_STENCIL_OP_DECREMENT_AND_CLAMP=4,
       VK_STENCIL_OP_INVERT=5,
       VK_STENCIL_OP_INCREMENT_AND_WRAP=6,
       VK_STENCIL_OP_DECREMENT_AND_WRAP=7,
       VK_STENCIL_OP_END_RANGE=7, // VK_STENCIL_OP_DECREMENT_AND_WRAP
       VK_STENCIL_OP_RANGE_SIZE=8, // (VK_STENCIL_OP_DECREMENT_AND_WRAP-VK_STENCIL_OP_KEEP)+1
       VK_STENCIL_OP_MAX_ENUM=$7fffffff
      );
     PVkLogicOp=^TVkLogicOp;
     TVkLogicOp=
      (
       VK_LOGIC_OP_CLEAR=0,
       VK_LOGIC_OP_BEGIN_RANGE=0, // VK_LOGIC_OP_CLEAR
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
       VK_LOGIC_OP_END_RANGE=15, // VK_LOGIC_OP_SET
       VK_LOGIC_OP_RANGE_SIZE=16, // (VK_LOGIC_OP_SET-VK_LOGIC_OP_CLEAR)+1
       VK_LOGIC_OP_MAX_ENUM=$7fffffff
      );
     PVkInternalAllocationType=^TVkInternalAllocationType;
     TVkInternalAllocationType=
      (
       VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE=0,
       VK_INTERNAL_ALLOCATION_TYPE_BEGIN_RANGE=0, // VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE
       VK_INTERNAL_ALLOCATION_TYPE_END_RANGE=0, // VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE
       VK_INTERNAL_ALLOCATION_TYPE_RANGE_SIZE=1, // (VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE-VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE)+1
       VK_INTERNAL_ALLOCATION_TYPE_MAX_ENUM=$7fffffff
      );
     PVkSystemAllocationScope=^TVkSystemAllocationScope;
     TVkSystemAllocationScope=
      (
       VK_SYSTEM_ALLOCATION_SCOPE_COMMAND=0,
       VK_SYSTEM_ALLOCATION_SCOPE_BEGIN_RANGE=0, // VK_SYSTEM_ALLOCATION_SCOPE_COMMAND
       VK_SYSTEM_ALLOCATION_SCOPE_OBJECT=1,
       VK_SYSTEM_ALLOCATION_SCOPE_CACHE=2,
       VK_SYSTEM_ALLOCATION_SCOPE_DEVICE=3,
       VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE=4,
       VK_SYSTEM_ALLOCATION_SCOPE_END_RANGE=4, // VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE
       VK_SYSTEM_ALLOCATION_SCOPE_RANGE_SIZE=5, // (VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE-VK_SYSTEM_ALLOCATION_SCOPE_COMMAND)+1
       VK_SYSTEM_ALLOCATION_SCOPE_MAX_ENUM=$7fffffff
      );
     PVkPhysicalDeviceType=^TVkPhysicalDeviceType;
     TVkPhysicalDeviceType=
      (
       VK_PHYSICAL_DEVICE_TYPE_OTHER=0,
       VK_PHYSICAL_DEVICE_TYPE_BEGIN_RANGE=0, // VK_PHYSICAL_DEVICE_TYPE_OTHER
       VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU=1,
       VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU=2,
       VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU=3,
       VK_PHYSICAL_DEVICE_TYPE_CPU=4,
       VK_PHYSICAL_DEVICE_TYPE_END_RANGE=4, // VK_PHYSICAL_DEVICE_TYPE_CPU
       VK_PHYSICAL_DEVICE_TYPE_RANGE_SIZE=5, // (VK_PHYSICAL_DEVICE_TYPE_CPU-VK_PHYSICAL_DEVICE_TYPE_OTHER)+1
       VK_PHYSICAL_DEVICE_TYPE_MAX_ENUM=$7fffffff
      );
     PVkVertexInputRate=^TVkVertexInputRate;
     TVkVertexInputRate=
      (
       VK_VERTEX_INPUT_RATE_VERTEX=0,
       VK_VERTEX_INPUT_RATE_BEGIN_RANGE=0, // VK_VERTEX_INPUT_RATE_VERTEX
       VK_VERTEX_INPUT_RATE_INSTANCE=1,
       VK_VERTEX_INPUT_RATE_END_RANGE=1, // VK_VERTEX_INPUT_RATE_INSTANCE
       VK_VERTEX_INPUT_RATE_RANGE_SIZE=2, // (VK_VERTEX_INPUT_RATE_INSTANCE-VK_VERTEX_INPUT_RATE_VERTEX)+1
       VK_VERTEX_INPUT_RATE_MAX_ENUM=$7fffffff
      );
     PVkFormat=^TVkFormat;
     TVkFormat=
      (
       VK_FORMAT_UNDEFINED=0,
       VK_FORMAT_BEGIN_RANGE=0, // VK_FORMAT_UNDEFINED
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
       VK_FORMAT_END_RANGE=184, // VK_FORMAT_ASTC_12x12_SRGB_BLOCK
       VK_FORMAT_RANGE_SIZE=185, // (VK_FORMAT_ASTC_12x12_SRGB_BLOCK-VK_FORMAT_UNDEFINED)+1
       VK_FORMAT_MAX_ENUM=$7fffffff
      );
     PVkStructureType=^TVkStructureType;
     TVkStructureType=
      (
       VK_STRUCTURE_TYPE_APPLICATION_INFO=0,
       VK_STRUCTURE_TYPE_BEGIN_RANGE=0, // VK_STRUCTURE_TYPE_APPLICATION_INFO
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
       VK_STRUCTURE_TYPE_END_RANGE=48, // VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO
       VK_STRUCTURE_TYPE_RANGE_SIZE=49, // (VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO-VK_STRUCTURE_TYPE_APPLICATION_INFO)+1
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
     PVkSubpassContents=^TVkSubpassContents;
     TVkSubpassContents=
      (
       VK_SUBPASS_CONTENTS_INLINE=0,
       VK_SUBPASS_CONTENTS_BEGIN_RANGE=0, // VK_SUBPASS_CONTENTS_INLINE
       VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS=1,
       VK_SUBPASS_CONTENTS_END_RANGE=1, // VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS
       VK_SUBPASS_CONTENTS_RANGE_SIZE=2, // (VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS-VK_SUBPASS_CONTENTS_INLINE)+1
       VK_SUBPASS_CONTENTS_MAX_ENUM=$7fffffff
      );
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
       VK_RESULT_BEGIN_RANGE=-12, // VK_RESULT_UNUSED_START
       VK_ERROR_FORMAT_NOT_SUPPORTED=-11, // Requested format is not supported on this device
       VK_ERROR_TOO_MANY_OBJECTS=-10, // Too many objects of the type have already been created
       VK_ERROR_INCOMPATIBLE_DRIVER=-9, // Unable to find a Vulkan driver
       VK_ERROR_FEATURE_NOT_PRESENT=-8, // Requested feature is not available on this device
       VK_ERROR_EXTENSION_NOT_PRESENT=-7, // Extension specified does not exist
       VK_ERROR_LAYER_NOT_PRESENT=-6, // Layer specified does not exist
       VK_ERROR_MEMORY_MAP_FAILED=-5, // Mapping of a memory object has failed
       VK_ERROR_DEVICE_LOST=-4, // Initialization of a object has failed
       VK_ERROR_INITIALIZATION_FAILED=-3, // The logical device has been lost. See <<devsandqueues-lost-device>>
       VK_ERROR_OUT_OF_DEVICE_MEMORY=-2, // A device memory allocation has failed
       VK_ERROR_OUT_OF_HOST_MEMORY=-1, // A host memory allocation has failed
       VK_SUCCESS=0, // Command completed successfully
       VK_NOT_READY=1, // A fence or query has not yet completed
       VK_TIMEOUT=2, // A wait operation has not completed in the specified time
       VK_EVENT_SET=3, // An event is signaled
       VK_EVENT_RESET=4, // An event is unsignalled
       VK_INCOMPLETE=5, // A return array was too small for the resul
       VK_RESULT_END_RANGE=5, // VK_INCOMPLETE
       VK_RESULT_RANGE_SIZE=18, // (VK_INCOMPLETE-VK_RESULT_UNUSED_START)+1
       VK_SUBOPTIMAL_KHR=1000001003,
       VK_RESULT_MAX_ENUM=$7fffffff
      );
     PVkDynamicState=^TVkDynamicState;
     TVkDynamicState=
      (
       VK_DYNAMIC_STATE_VIEWPORT=0,
       VK_DYNAMIC_STATE_BEGIN_RANGE=0, // VK_DYNAMIC_STATE_VIEWPORT
       VK_DYNAMIC_STATE_SCISSOR=1,
       VK_DYNAMIC_STATE_LINE_WIDTH=2,
       VK_DYNAMIC_STATE_DEPTH_BIAS=3,
       VK_DYNAMIC_STATE_BLEND_CONSTANTS=4,
       VK_DYNAMIC_STATE_DEPTH_BOUNDS=5,
       VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK=6,
       VK_DYNAMIC_STATE_STENCIL_WRITE_MASK=7,
       VK_DYNAMIC_STATE_STENCIL_REFERENCE=8,
       VK_DYNAMIC_STATE_END_RANGE=8, // VK_DYNAMIC_STATE_STENCIL_REFERENCE
       VK_DYNAMIC_STATE_RANGE_SIZE=9, // (VK_DYNAMIC_STATE_STENCIL_REFERENCE-VK_DYNAMIC_STATE_VIEWPORT)+1
       VK_DYNAMIC_STATE_MAX_ENUM=$7fffffff
      );
     PVkQueueFlagBits=^TVkQueueFlagBits;
     TVkQueueFlagBits=
      (
       VK_QUEUE_GRAPHICS_BIT=$00000001, // Queue supports graphics operations
       VK_QUEUE_COMPUTE_BIT=$00000002, // Queue supports compute operations
       VK_QUEUE_TRANSFER_BIT=$00000004, // Queue supports transfer operations
       VK_QUEUE_SPARSE_BINDING_BIT=$00000008 // Queue supports sparse resource memory management operations
      );
     PVkMemoryPropertyFlagBits=^TVkMemoryPropertyFlagBits;
     TVkMemoryPropertyFlagBits=
      (
       VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT=$00000001, // If otherwise stated, then allocate memory on device
       VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT=$00000002, // Memory is mappable by host
       VK_MEMORY_PROPERTY_HOST_COHERENT_BIT=$00000004, // Memory will have i/o coherency. If not set, application may need to use vkFlushMappedMemoryRanges and vkInvalidateMappedMemoryRanges to flush/invalidate host cache
       VK_MEMORY_PROPERTY_HOST_CACHED_BIT=$00000008, // Memory will be cached by the host
       VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT=$00000010 // Memory may be allocated by the driver when it is required
      );
     PVkMemoryHeapFlagBits=^TVkMemoryHeapFlagBits;
     TVkMemoryHeapFlagBits=
      (
       VK_MEMORY_HEAP_DEVICE_LOCAL_BIT=$00000001 // If set, heap represents device memory
      );
     PVkAccessFlagBits=^TVkAccessFlagBits;
     TVkAccessFlagBits=
      (
       VK_ACCESS_INDIRECT_COMMAND_READ_BIT=$00000001, // Controls coherency of indirect command reads
       VK_ACCESS_INDEX_READ_BIT=$00000002, // Controls coherency of index reads
       VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT=$00000004, // Controls coherency of vertex attribute reads
       VK_ACCESS_UNIFORM_READ_BIT=$00000008, // Controls coherency of uniform buffer reads
       VK_ACCESS_INPUT_ATTACHMENT_READ_BIT=$00000010, // Controls coherency of input attachment reads
       VK_ACCESS_SHADER_READ_BIT=$00000020, // Controls coherency of shader reads
       VK_ACCESS_SHADER_WRITE_BIT=$00000040, // Controls coherency of shader writes
       VK_ACCESS_COLOR_ATTACHMENT_READ_BIT=$00000080, // Controls coherency of color attachment reads
       VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT=$00000100, // Controls coherency of color attachment writes
       VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT=$00000200, // Controls coherency of depth/stencil attachment reads
       VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT=$00000400, // Controls coherency of depth/stencil attachment writes
       VK_ACCESS_TRANSFER_READ_BIT=$00000800, // Controls coherency of transfer reads
       VK_ACCESS_TRANSFER_WRITE_BIT=$00001000, // Controls coherency of transfer writes
       VK_ACCESS_HOST_READ_BIT=$00002000, // Controls coherency of host reads
       VK_ACCESS_HOST_WRITE_BIT=$00004000, // Controls coherency of host writes
       VK_ACCESS_MEMORY_READ_BIT=$00008000, // Controls coherency of memory reads
       VK_ACCESS_MEMORY_WRITE_BIT=$00010000 // Controls coherency of memory writes
      );
     PVkBufferUsageFlagBits=^TVkBufferUsageFlagBits;
     TVkBufferUsageFlagBits=
      (
       VK_BUFFER_USAGE_TRANSFER_SRC_BIT=$00000001, // Can be used as a source of transfer operations
       VK_BUFFER_USAGE_TRANSFER_DST_BIT=$00000002, // Can be used as a destination of transfer operations
       VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT=$00000004, // Can be used as TBO
       VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT=$00000008, // Can be used as IBO
       VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT=$00000010, // Can be used as UBO
       VK_BUFFER_USAGE_STORAGE_BUFFER_BIT=$00000020, // Can be used as SSBO
       VK_BUFFER_USAGE_INDEX_BUFFER_BIT=$00000040, // Can be used as source of fixed-function index fetch (index buffer)
       VK_BUFFER_USAGE_VERTEX_BUFFER_BIT=$00000080, // Can be used as source of fixed-function vertex fetch (VBO)
       VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT=$00000100 // Can be the source of indirect parameters (e.g. indirect buffer, parameter buffer)
      );
     PVkBufferCreateFlagBits=^TVkBufferCreateFlagBits;
     TVkBufferCreateFlagBits=
      (
       VK_BUFFER_CREATE_SPARSE_BINDING_BIT=$00000001, // Buffer should support sparse backing
       VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT=$00000002, // Buffer should support sparse backing with partial residency
       VK_BUFFER_CREATE_SPARSE_ALIASED_BIT=$00000004 // Buffer should support constent data access to physical memory blocks mapped into multiple locations of sparse buffers
      );
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
     PVkImageUsageFlagBits=^TVkImageUsageFlagBits;
     TVkImageUsageFlagBits=
      (
       VK_IMAGE_USAGE_TRANSFER_SRC_BIT=$00000001, // Can be used as a source of transfer operations
       VK_IMAGE_USAGE_TRANSFER_DST_BIT=$00000002, // Can be used as a destination of transfer operations
       VK_IMAGE_USAGE_SAMPLED_BIT=$00000004, // Can be sampled from (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
       VK_IMAGE_USAGE_STORAGE_BIT=$00000008, // Can be used as storage image (STORAGE_IMAGE descriptor type)
       VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT=$00000010, // Can be used as framebuffer color attachment
       VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT=$00000020, // Can be used as framebuffer depth/stencil attachment
       VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT=$00000040, // Image data not needed outside of rendering
       VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT=$00000080 // Can be used as framebuffer input attachment
      );
     PVkImageCreateFlagBits=^TVkImageCreateFlagBits;
     TVkImageCreateFlagBits=
      (
       VK_IMAGE_CREATE_SPARSE_BINDING_BIT=$00000001, // Image should support sparse backing
       VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT=$00000002, // Image should support sparse backing with partial residency
       VK_IMAGE_CREATE_SPARSE_ALIASED_BIT=$00000004, // Image should support constent data access to physical memory blocks mapped into multiple locations of sparse images
       VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT=$00000008, // Allows image views to have different format than the base image
       VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT=$00000010 // Allows creating image views with cube type from the created image
      );
     PVkPipelineCreateFlagBits=^TVkPipelineCreateFlagBits;
     TVkPipelineCreateFlagBits=
      (
       VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT=$00000001,
       VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT=$00000002,
       VK_PIPELINE_CREATE_DERIVATIVE_BIT=$00000004
      );
     PVkColorComponentFlagBits=^TVkColorComponentFlagBits;
     TVkColorComponentFlagBits=
      (
       VK_COLOR_COMPONENT_R_BIT=$00000001,
       VK_COLOR_COMPONENT_G_BIT=$00000002,
       VK_COLOR_COMPONENT_B_BIT=$00000004,
       VK_COLOR_COMPONENT_A_BIT=$00000008
      );
     PVkFenceCreateFlagBits=^TVkFenceCreateFlagBits;
     TVkFenceCreateFlagBits=
      (
       VK_FENCE_CREATE_SIGNALED_BIT=$00000001
      );
     PVkFormatFeatureFlagBits=^TVkFormatFeatureFlagBits;
     TVkFormatFeatureFlagBits=
      (
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT=$00000001, // Format can be used for sampled images (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
       VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT=$00000002, // Format can be used for storage images (STORAGE_IMAGE descriptor type)
       VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT=$00000004, // Format supports atomic operations in case it's used for storage images
       VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT=$00000008, // Format can be used for uniform texel buffers (TBOs)
       VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT=$00000010, // Format can be used for storage texel buffers (IBOs)
       VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT=$00000020, // Format supports atomic operations in case it's used for storage texel buffers
       VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT=$00000040, // Format can be used for vertex buffers (VBOs)
       VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT=$00000080, // Format can be used for color attachment images
       VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT=$00000100, // Format supports blending in case it's used for color attachment images
       VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT=$00000200, // Format can be used for depth/stencil attachment images
       VK_FORMAT_FEATURE_BLIT_SRC_BIT=$00000400, // Format can be used as the source image of blits with vkCmdBlitImage
       VK_FORMAT_FEATURE_BLIT_DST_BIT=$00000800, // Format can be used as the destination image of blits with vkCmdBlitImage
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT=$00001000 // Format can be filtered with VK_FILTER_LINEAR when being sampled
      );
     PVkQueryControlFlagBits=^TVkQueryControlFlagBits;
     TVkQueryControlFlagBits=
      (
       VK_QUERY_CONTROL_PRECISE_BIT=$00000001 // Require precise results to be collected by the query
      );
     PVkQueryResultFlagBits=^TVkQueryResultFlagBits;
     TVkQueryResultFlagBits=
      (
       VK_QUERY_RESULT_64_BIT=$00000001, // Results of the queries are written to the destination buffer as 64-bit values
       VK_QUERY_RESULT_WAIT_BIT=$00000002, // Results of the queries are waited on before proceeding with the result copy
       VK_QUERY_RESULT_WITH_AVAILABILITY_BIT=$00000004, // Besides the results of the query, the availability of the results is also written
       VK_QUERY_RESULT_PARTIAL_BIT=$00000008 // Copy the partial results of the query even if the final results aren't available
      );
     PVkCommandBufferUsageFlagBits=^TVkCommandBufferUsageFlagBits;
     TVkCommandBufferUsageFlagBits=
      (
       VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT=$00000001,
       VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT=$00000002,
       VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT=$00000004 // Command buffer may be submitted/executed more than once simultaneously
      );
     PVkQueryPipelineStatisticFlagBits=^TVkQueryPipelineStatisticFlagBits;
     TVkQueryPipelineStatisticFlagBits=
      (
       VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT=$00000001, // Optional
       VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT=$00000002, // Optional
       VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT=$00000004, // Optional
       VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT=$00000008, // Optional
       VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT=$00000010, // Optional
       VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT=$00000020, // Optional
       VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT=$00000040, // Optional
       VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT=$00000080, // Optional
       VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT=$00000100, // Optional
       VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT=$00000200, // Optional
       VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT=$00000400 // Optional
      );
     PVkImageAspectFlagBits=^TVkImageAspectFlagBits;
     TVkImageAspectFlagBits=
      (
       VK_IMAGE_ASPECT_COLOR_BIT=$00000001,
       VK_IMAGE_ASPECT_DEPTH_BIT=$00000002,
       VK_IMAGE_ASPECT_STENCIL_BIT=$00000004,
       VK_IMAGE_ASPECT_METADATA_BIT=$00000008
      );
     PVkSparseImageFormatFlagBits=^TVkSparseImageFormatFlagBits;
     TVkSparseImageFormatFlagBits=
      (
       VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT=$00000001, // Image uses a single miptail region for all array layers
       VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT=$00000002, // Image requires mip levels to be an exact multiple of the sparse image block size for non-miptail levels.
       VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT=$00000004 // Image uses a non-standard sparse block size
      );
     PVkSparseMemoryBindFlagBits=^TVkSparseMemoryBindFlagBits;
     TVkSparseMemoryBindFlagBits=
      (
       VK_SPARSE_MEMORY_BIND_METADATA_BIT=$00000001 // Operation binds resource metadata to memory
      );
     PVkPipelineStageFlagBits=^TVkPipelineStageFlagBits;
     TVkPipelineStageFlagBits=
      (
       VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT=$00000001, // Before subsequent commands are processed
       VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT=$00000002, // Draw/DispatchIndirect command fetch
       VK_PIPELINE_STAGE_VERTEX_INPUT_BIT=$00000004, // Vertex/index fetch
       VK_PIPELINE_STAGE_VERTEX_SHADER_BIT=$00000008, // Vertex shading
       VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT=$00000010, // Tessellation control shading
       VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT=$00000020, // Tessellation evaluation shading
       VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT=$00000040, // Geometry shading
       VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT=$00000080, // Fragment shading
       VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT=$00000100, // Early fragment (depth and stencil) tests
       VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT=$00000200, // Late fragment (depth and stencil) tests
       VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT=$00000400, // Color attachment writes
       VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT=$00000800, // Compute shading
       VK_PIPELINE_STAGE_TRANSFER_BIT=$00001000, // Transfer/copy operations
       VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT=$00002000, // After previous commands have completed
       VK_PIPELINE_STAGE_HOST_BIT=$00004000, // Indicates host (CPU) is a source/sink of the dependency
       VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT=$00008000, // All stages of the graphics pipeline
       VK_PIPELINE_STAGE_ALL_COMMANDS_BIT=$00010000 // All stages supported on the queue
      );
     PVkCommandPoolCreateFlagBits=^TVkCommandPoolCreateFlagBits;
     TVkCommandPoolCreateFlagBits=
      (
       VK_COMMAND_POOL_CREATE_TRANSIENT_BIT=$00000001, // Command buffers have a short lifetime
       VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT=$00000002 // Command buffers may release their memory individually
      );
     PVkCommandPoolResetFlagBits=^TVkCommandPoolResetFlagBits;
     TVkCommandPoolResetFlagBits=
      (
       VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT=$00000001 // Release resources owned by the pool
      );
     PVkCommandBufferResetFlagBits=^TVkCommandBufferResetFlagBits;
     TVkCommandBufferResetFlagBits=
      (
       VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT=$00000001 // Release resources owned by the buffer
      );
     PVkSampleCountFlagBits=^TVkSampleCountFlagBits;
     TVkSampleCountFlagBits=
      (
       VK_SAMPLE_COUNT_1_BIT=$00000001, // Sample count 1 supported
       VK_SAMPLE_COUNT_2_BIT=$00000002, // Sample count 2 supported
       VK_SAMPLE_COUNT_4_BIT=$00000004, // Sample count 4 supported
       VK_SAMPLE_COUNT_8_BIT=$00000008, // Sample count 8 supported
       VK_SAMPLE_COUNT_16_BIT=$00000010, // Sample count 16 supported
       VK_SAMPLE_COUNT_32_BIT=$00000020, // Sample count 32 supported
       VK_SAMPLE_COUNT_64_BIT=$00000040 // Sample count 64 supported
      );
     PVkAttachmentDescriptionFlagBits=^TVkAttachmentDescriptionFlagBits;
     TVkAttachmentDescriptionFlagBits=
      (
       VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT=$00000001 // The attachment may alias physical memory of another attachment in the same render pass
      );
     PVkStencilFaceFlagBits=^TVkStencilFaceFlagBits;
     TVkStencilFaceFlagBits=
      (
       VK_STENCIL_FACE_FRONT_BIT=$00000001, // Front face
       VK_STENCIL_FRONT_AND_BACK=$00000001, // Front and back faces
       VK_STENCIL_FACE_BACK_BIT=$00000002 // Back face
      );
     PVkDescriptorPoolCreateFlagBits=^TVkDescriptorPoolCreateFlagBits;
     TVkDescriptorPoolCreateFlagBits=
      (
       VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT=$00000001 // Descriptor sets may be freed individually
      );
     PVkDependencyFlagBits=^TVkDependencyFlagBits;
     TVkDependencyFlagBits=
      (
       VK_DEPENDENCY_BY_REGION_BIT=$00000001 // Dependency is per pixel region 
      );
     PVkPresentModeKHR=^TVkPresentModeKHR;
     TVkPresentModeKHR=
      (
       VK_PRESENT_MODE_IMMEDIATE_KHR=0,
       VK_PRESENT_MODE_BEGIN_RANGE=0, // VK_PRESENT_MODE_IMMEDIATE_KHR
       VK_PRESENT_MODE_MAILBOX_KHR=1,
       VK_PRESENT_MODE_FIFO_KHR=2,
       VK_PRESENT_MODE_FIFO_RELAXED_KHR=3,
       VK_PRESENT_MODE_END_RANGE=3, // VK_PRESENT_MODE_FIFO_RELAXED_KHR
       VK_PRESENT_MODE_RANGE_SIZE=4, // (VK_PRESENT_MODE_FIFO_RELAXED_KHR-VK_PRESENT_MODE_IMMEDIATE_KHR)+1
       VK_PRESENT_MODE_MAX_ENUM=$7fffffff
      );
     PVkColorSpaceKHR=^TVkColorSpaceKHR;
     TVkColorSpaceKHR=
      (
       VK_COLORSPACE_SRGB_NONLINEAR_KHR=0,
       VK_COLORSPACE_BEGIN_RANGE=0, // VK_COLORSPACE_SRGB_NONLINEAR_KHR
       VK_COLORSPACE_END_RANGE=0, // VK_COLORSPACE_SRGB_NONLINEAR_KHR
       VK_COLORSPACE_RANGE_SIZE=1, // (VK_COLORSPACE_SRGB_NONLINEAR_KHR-VK_COLORSPACE_SRGB_NONLINEAR_KHR)+1
       VK_COLORSPACE_MAX_ENUM=$7fffffff
      );
     PVkDisplayPlaneAlphaFlagBitsKHR=^TVkDisplayPlaneAlphaFlagBitsKHR;
     TVkDisplayPlaneAlphaFlagBitsKHR=
      (
       VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR=$00000001,
       VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR=$00000002,
       VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR=$00000004,
       VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR=$00000008
      );
     PVkCompositeAlphaFlagBitsKHR=^TVkCompositeAlphaFlagBitsKHR;
     TVkCompositeAlphaFlagBitsKHR=
      (
       VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR=$00000001,
       VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR=$00000002,
       VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR=$00000004,
       VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR=$00000008
      );
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
     PVkDebugReportFlagBitsEXT=^TVkDebugReportFlagBitsEXT;
     TVkDebugReportFlagBitsEXT=
      (
       VK_DEBUG_REPORT_INFORMATION_BIT_EXT=$00000001,
       VK_DEBUG_REPORT_WARNING_BIT_EXT=$00000002,
       VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT=$00000004,
       VK_DEBUG_REPORT_ERROR_BIT_EXT=$00000008,
       VK_DEBUG_REPORT_DEBUG_BIT_EXT=$00000010
      );
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
     PVkDebugReportErrorEXT=^TVkDebugReportErrorEXT;
     TVkDebugReportErrorEXT=
      (
       VK_DEBUG_REPORT_ERROR_NONE_EXT=0,
       VK_DEBUG_REPORT_ERROR_CALLBACK_REF_EXT=1
      );
     PPFN_vkInternalAllocationNotification=^TPFN_vkInternalAllocationNotification;
     PPFN_vkInternalFreeNotification=^TPFN_vkInternalFreeNotification;
     PPFN_vkReallocationFunction=^TPFN_vkReallocationFunction;
     PPFN_vkAllocationFunction=^TPFN_vkAllocationFunction;
     PPFN_vkFreeFunction=^TPFN_vkFreeFunction;
     PPFN_vkVoidFunction=^TPFN_vkVoidFunction;
     PPFN_vkDebugReportCallbackEXT=^TPFN_vkDebugReportCallbackEXT;
     PVkOffset2D=^TVkOffset2D;
     PVkOffset3D=^TVkOffset3D;
     PVkExtent2D=^TVkExtent2D;
     PVkExtent3D=^TVkExtent3D;
     PVkViewport=^TVkViewport;
     PVkRect2D=^TVkRect2D;
     PVkRect3D=^TVkRect3D;
     PVkClearRect=^TVkClearRect;
     PVkComponentMapping=^TVkComponentMapping;
     PVkPhysicalDeviceSparseProperties=^TVkPhysicalDeviceSparseProperties;
     PVkExtensionProperties=^TVkExtensionProperties;
     PVkLayerProperties=^TVkLayerProperties;
     PVkApplicationInfo=^TVkApplicationInfo;
     PVkAllocationCallbacks=^TVkAllocationCallbacks;
     PVkDeviceQueueCreateInfo=^TVkDeviceQueueCreateInfo;
     PVkDeviceCreateInfo=^TVkDeviceCreateInfo;
     PVkInstanceCreateInfo=^TVkInstanceCreateInfo;
     PVkQueueFamilyProperties=^TVkQueueFamilyProperties;
     PVkMemoryType=^TVkMemoryType;
     PVkMemoryAllocateInfo=^TVkMemoryAllocateInfo;
     PVkMemoryRequirements=^TVkMemoryRequirements;
     PVkSparseImageFormatProperties=^TVkSparseImageFormatProperties;
     PVkSparseImageMemoryRequirements=^TVkSparseImageMemoryRequirements;
     PVkMemoryHeap=^TVkMemoryHeap;
     PVkPhysicalDeviceMemoryProperties=^TVkPhysicalDeviceMemoryProperties;
     PVkMappedMemoryRange=^TVkMappedMemoryRange;
     PVkFormatProperties=^TVkFormatProperties;
     PVkImageFormatProperties=^TVkImageFormatProperties;
     PVkDescriptorBufferInfo=^TVkDescriptorBufferInfo;
     PVkDescriptorImageInfo=^TVkDescriptorImageInfo;
     PVkWriteDescriptorSet=^TVkWriteDescriptorSet;
     PVkCopyDescriptorSet=^TVkCopyDescriptorSet;
     PVkBufferCreateInfo=^TVkBufferCreateInfo;
     PVkBufferViewCreateInfo=^TVkBufferViewCreateInfo;
     PVkImageSubresource=^TVkImageSubresource;
     PVkImageSubresourceLayers=^TVkImageSubresourceLayers;
     PVkImageSubresourceRange=^TVkImageSubresourceRange;
     PVkMemoryBarrier=^TVkMemoryBarrier;
     PVkBufferMemoryBarrier=^TVkBufferMemoryBarrier;
     PVkImageMemoryBarrier=^TVkImageMemoryBarrier;
     PVkImageCreateInfo=^TVkImageCreateInfo;
     PVkSubresourceLayout=^TVkSubresourceLayout;
     PVkImageViewCreateInfo=^TVkImageViewCreateInfo;
     PVkBufferCopy=^TVkBufferCopy;
     PVkSparseMemoryBind=^TVkSparseMemoryBind;
     PVkSparseImageMemoryBind=^TVkSparseImageMemoryBind;
     PVkSparseBufferMemoryBindInfo=^TVkSparseBufferMemoryBindInfo;
     PVkSparseImageOpaqueMemoryBindInfo=^TVkSparseImageOpaqueMemoryBindInfo;
     PVkSparseImageMemoryBindInfo=^TVkSparseImageMemoryBindInfo;
     PVkBindSparseInfo=^TVkBindSparseInfo;
     PVkImageCopy=^TVkImageCopy;
     PVkImageBlit=^TVkImageBlit;
     PVkBufferImageCopy=^TVkBufferImageCopy;
     PVkImageResolve=^TVkImageResolve;
     PVkShaderModuleCreateInfo=^TVkShaderModuleCreateInfo;
     PVkDescriptorSetLayoutBinding=^TVkDescriptorSetLayoutBinding;
     PVkDescriptorSetLayoutCreateInfo=^TVkDescriptorSetLayoutCreateInfo;
     PVkDescriptorPoolSize=^TVkDescriptorPoolSize;
     PVkDescriptorPoolCreateInfo=^TVkDescriptorPoolCreateInfo;
     PVkDescriptorSetAllocateInfo=^TVkDescriptorSetAllocateInfo;
     PVkSpecializationMapEntry=^TVkSpecializationMapEntry;
     PVkSpecializationInfo=^TVkSpecializationInfo;
     PVkPipelineShaderStageCreateInfo=^TVkPipelineShaderStageCreateInfo;
     PVkComputePipelineCreateInfo=^TVkComputePipelineCreateInfo;
     PVkVertexInputBindingDescription=^TVkVertexInputBindingDescription;
     PVkVertexInputAttributeDescription=^TVkVertexInputAttributeDescription;
     PVkPipelineVertexInputStateCreateInfo=^TVkPipelineVertexInputStateCreateInfo;
     PVkPipelineInputAssemblyStateCreateInfo=^TVkPipelineInputAssemblyStateCreateInfo;
     PVkPipelineTessellationStateCreateInfo=^TVkPipelineTessellationStateCreateInfo;
     PVkPipelineViewportStateCreateInfo=^TVkPipelineViewportStateCreateInfo;
     PVkPipelineRasterizationStateCreateInfo=^TVkPipelineRasterizationStateCreateInfo;
     PVkPipelineMultisampleStateCreateInfo=^TVkPipelineMultisampleStateCreateInfo;
     PVkPipelineColorBlendAttachmentState=^TVkPipelineColorBlendAttachmentState;
     PVkPipelineColorBlendStateCreateInfo=^TVkPipelineColorBlendStateCreateInfo;
     PVkPipelineDynamicStateCreateInfo=^TVkPipelineDynamicStateCreateInfo;
     PVkStencilOpState=^TVkStencilOpState;
     PVkPipelineDepthStencilStateCreateInfo=^TVkPipelineDepthStencilStateCreateInfo;
     PVkGraphicsPipelineCreateInfo=^TVkGraphicsPipelineCreateInfo;
     PVkPipelineCacheCreateInfo=^TVkPipelineCacheCreateInfo;
     PVkPushConstantRange=^TVkPushConstantRange;
     PVkPipelineLayoutCreateInfo=^TVkPipelineLayoutCreateInfo;
     PVkSamplerCreateInfo=^TVkSamplerCreateInfo;
     PVkCommandPoolCreateInfo=^TVkCommandPoolCreateInfo;
     PVkCommandBufferAllocateInfo=^TVkCommandBufferAllocateInfo;
     PVkCommandBufferInheritanceInfo=^TVkCommandBufferInheritanceInfo;
     PVkCommandBufferBeginInfo=^TVkCommandBufferBeginInfo;
     PVkRenderPassBeginInfo=^TVkRenderPassBeginInfo;
     PVkClearColorValue=^TVkClearColorValue;
     PVkClearDepthStencilValue=^TVkClearDepthStencilValue;
     PVkClearValue=^TVkClearValue;
     PVkClearAttachment=^TVkClearAttachment;
     PVkAttachmentDescription=^TVkAttachmentDescription;
     PVkAttachmentReference=^TVkAttachmentReference;
     PVkSubpassDescription=^TVkSubpassDescription;
     PVkSubpassDependency=^TVkSubpassDependency;
     PVkRenderPassCreateInfo=^TVkRenderPassCreateInfo;
     PVkEventCreateInfo=^TVkEventCreateInfo;
     PVkFenceCreateInfo=^TVkFenceCreateInfo;
     PVkPhysicalDeviceFeatures=^TVkPhysicalDeviceFeatures;
     PVkPhysicalDeviceLimits=^TVkPhysicalDeviceLimits;
     PVkPhysicalDeviceProperties=^TVkPhysicalDeviceProperties;
     PVkSemaphoreCreateInfo=^TVkSemaphoreCreateInfo;
     PVkQueryPoolCreateInfo=^TVkQueryPoolCreateInfo;
     PVkFramebufferCreateInfo=^TVkFramebufferCreateInfo;
     PVkDrawIndirectCommand=^TVkDrawIndirectCommand;
     PVkDrawIndexedIndirectCommand=^TVkDrawIndexedIndirectCommand;
     PVkDispatchIndirectCommand=^TVkDispatchIndirectCommand;
     PVkSubmitInfo=^TVkSubmitInfo;
     PVkDisplayPropertiesKHR=^TVkDisplayPropertiesKHR;
     PVkDisplayPlanePropertiesKHR=^TVkDisplayPlanePropertiesKHR;
     PVkDisplayModeParametersKHR=^TVkDisplayModeParametersKHR;
     PVkDisplayModePropertiesKHR=^TVkDisplayModePropertiesKHR;
     PVkDisplayModeCreateInfoKHR=^TVkDisplayModeCreateInfoKHR;
     PVkDisplayPlaneCapabilitiesKHR=^TVkDisplayPlaneCapabilitiesKHR;
     PVkDisplaySurfaceCreateInfoKHR=^TVkDisplaySurfaceCreateInfoKHR;
     PVkDisplayPresentInfoKHR=^TVkDisplayPresentInfoKHR;
     PVkSurfaceCapabilitiesKHR=^TVkSurfaceCapabilitiesKHR;
{$ifdef Android}
     PVkAndroidSurfaceCreateInfoKHR=^TVkAndroidSurfaceCreateInfoKHR;
{$endif}
{$ifdef Mir}
     PVkMirSurfaceCreateInfoKHR=^TVkMirSurfaceCreateInfoKHR;
{$endif}
{$ifdef Wayland}
     PVkWaylandSurfaceCreateInfoKHR=^TVkWaylandSurfaceCreateInfoKHR;
{$endif}
{$ifdef Windows}
     PVkWin32SurfaceCreateInfoKHR=^TVkWin32SurfaceCreateInfoKHR;
{$endif}
{$ifdef X11}
     PVkXlibSurfaceCreateInfoKHR=^TVkXlibSurfaceCreateInfoKHR;
{$endif}
{$ifdef XCB}
     PVkXcbSurfaceCreateInfoKHR=^TVkXcbSurfaceCreateInfoKHR;
{$endif}
     PVkSurfaceFormatKHR=^TVkSurfaceFormatKHR;
     PVkSwapchainCreateInfoKHR=^TVkSwapchainCreateInfoKHR;
     PVkPresentInfoKHR=^TVkPresentInfoKHR;
     PVkDebugReportCallbackCreateInfoEXT=^TVkDebugReportCallbackCreateInfoEXT;
     TPFN_vkInternalAllocationNotification=procedure(pUserData:TVkPointer;size:TVkPtrInt;allocationType:TVkInternalAllocationType;allocationScope:TVkSystemAllocationScope); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TPFN_vkInternalFreeNotification=procedure(pUserData:TVkPointer;size:TVkPtrInt;allocationType:TVkInternalAllocationType;allocationScope:TVkSystemAllocationScope); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TPFN_vkReallocationFunction=function(pUserData:TVkPointer;pOriginal:TVkPointer;size:TVkPtrInt;alignment:TVkPtrInt;allocationScope:TVkSystemAllocationScope):TVkPointer; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TPFN_vkAllocationFunction=function(pUserData:TVkPointer;size:TVkPtrInt;alignment:TVkPtrInt;allocationScope:TVkSystemAllocationScope):TVkPointer; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TPFN_vkFreeFunction=procedure(pUserData:TVkPointer;pMemory:TVkPointer); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TPFN_vkVoidFunction=procedure(); {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TPFN_vkDebugReportCallbackEXT=function(flags:TVkDebugReportFlagsEXT;objectType:TVkDebugReportObjectTypeEXT;object_:TVkUInt64;location:TVkPtrInt;messageCode:TVkInt32;const pLayerPrefix:PVkChar;const pMessage:PVkChar;pUserData:TVkPointer):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}
     TVkOffset2D=record
      x:TVkInt32;
      y:TVkInt32;
     end;
     TVkOffset3D=record
      x:TVkInt32;
      y:TVkInt32;
      z:TVkInt32;
     end;
     TVkExtent2D=record
      width:TVkUInt32;
      height:TVkUInt32;
     end;
     TVkExtent3D=record
      width:TVkUInt32;
      height:TVkUInt32;
      depth:TVkUInt32;
     end;
     TVkViewport=record
      x:TVkFloat;
      y:TVkFloat;
      width:TVkFloat;
      height:TVkFloat;
      minDepth:TVkFloat;
      maxDepth:TVkFloat;
     end;
     TVkRect2D=record
      offset:TVkOffset2D;
      extent:TVkExtent2D;
     end;
     TVkRect3D=record
      offset:TVkOffset3D;
      extent:TVkExtent3D;
     end;
     TVkClearRect=record
      rect:TVkRect2D;
      baseArrayLayer:TVkUInt32;
      layerCount:TVkUInt32;
     end;
     TVkComponentMapping=record
      r:TVkComponentSwizzle;
      g:TVkComponentSwizzle;
      b:TVkComponentSwizzle;
      a:TVkComponentSwizzle;
     end;
     TVkPhysicalDeviceSparseProperties=record
      residencyStandard2DBlockShape:TVkBool32;
      residencyStandard2DMultisampleBlockShape:TVkBool32;
      residencyStandard3DBlockShape:TVkBool32;
      residencyAlignedMipSize:TVkBool32;
      residencyNonResidentStrict:TVkBool32;
     end;
     TVkExtensionProperties=record
      extensionName:array[0..VK_MAX_EXTENSION_NAME_SIZE-1] of TVkChar;
      specVersion:TVkUInt32;
     end;
     TVkLayerProperties=record
      layerName:array[0..VK_MAX_EXTENSION_NAME_SIZE-1] of TVkChar;
      specVersion:TVkUInt32;
      implementationVersion:TVkUInt32;
      description:array[0..VK_MAX_DESCRIPTION_SIZE-1] of TVkChar;
     end;
     TVkApplicationInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      pApplicationName:PVkChar;
      applicationVersion:TVkUInt32;
      pEngineName:PVkChar;
      engineVersion:TVkUInt32;
      apiVersion:TVkUInt32;
     end;
     TVkAllocationCallbacks=record
      pUserData:TVkPointer;
      pfnAllocation:TPFN_vkAllocationFunction;
      pfnReallocation:TPFN_vkReallocationFunction;
      pfnFree:TPFN_vkFreeFunction;
      pfnInternalAllocation:TPFN_vkInternalAllocationNotification;
      pfnInternalFree:TPFN_vkInternalFreeNotification;
     end;
     TVkDeviceQueueCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkDeviceQueueCreateFlags;
      queueFamilyIndex:TVkUInt32;
      queueCount:TVkUInt32;
      pQueuePriorities:PVkFloat;
     end;
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
     TVkQueueFamilyProperties=record
      queueFlags:TVkQueueFlags;
      queueCount:TVkUInt32;
      timestampValidBits:TVkUInt32;
      minImageTransferGranularity:TVkExtent3D;
     end;
     TVkMemoryType=record
      propertyFlags:TVkMemoryPropertyFlags;
      heapIndex:TVkUInt32;
     end;
     TVkMemoryAllocateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      allocationSize:TVkDeviceSize;
      memoryTypeIndex:TVkUInt32;
     end;
     TVkMemoryRequirements=record
      size:TVkDeviceSize;
      alignment:TVkDeviceSize;
      memoryTypeBits:TVkUInt32;
     end;
     TVkSparseImageFormatProperties=record
      aspectMask:TVkImageAspectFlags;
      imageGranularity:TVkExtent3D;
      flags:TVkSparseImageFormatFlags;
     end;
     TVkSparseImageMemoryRequirements=record
      formatProperties:TVkSparseImageFormatProperties;
      imageMipTailFirstLod:TVkUInt32;
      imageMipTailSize:TVkDeviceSize;
      imageMipTailOffset:TVkDeviceSize;
      imageMipTailStride:TVkDeviceSize;
     end;
     TVkMemoryHeap=record
      size:TVkDeviceSize;
      flags:TVkMemoryHeapFlags;
     end;
     TVkPhysicalDeviceMemoryProperties=record
      memoryTypeCount:TVkUInt32;
      memoryTypes:array[0..VK_MAX_MEMORY_TYPES-1] of TVkMemoryType;
      memoryHeapCount:TVkUInt32;
      memoryHeaps:array[0..VK_MAX_MEMORY_HEAPS-1] of TVkMemoryHeap;
     end;
     TVkMappedMemoryRange=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      memory:TVkDeviceMemory;
      offset:TVkDeviceSize;
      size:TVkDeviceSize;
     end;
     TVkFormatProperties=record
      linearTilingFeatures:TVkFormatFeatureFlags;
      optimalTilingFeatures:TVkFormatFeatureFlags;
      bufferFeatures:TVkFormatFeatureFlags;
     end;
     TVkImageFormatProperties=record
      maxExtent:TVkExtent3D;
      maxMipLevels:TVkUInt32;
      maxArrayLayers:TVkUInt32;
      sampleCounts:TVkSampleCountFlags;
      maxResourceSize:TVkDeviceSize;
     end;
     TVkDescriptorBufferInfo=record
      buffer:TVkBuffer;
      offset:TVkDeviceSize;
      range:TVkDeviceSize;
     end;
     TVkDescriptorImageInfo=record
      sampler:TVkSampler;
      imageView:TVkImageView;
      imageLayout:TVkImageLayout;
     end;
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
     TVkBufferViewCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkBufferViewCreateFlags;
      buffer:TVkBuffer;
      format:TVkFormat;
      offset:TVkDeviceSize;
      range:TVkDeviceSize;
     end;
     TVkImageSubresource=record
      aspectMask:TVkImageAspectFlags;
      mipLevel:TVkUInt32;
      arrayLayer:TVkUInt32;
     end;
     TVkImageSubresourceLayers=record
      aspectMask:TVkImageAspectFlags;
      mipLevel:TVkUInt32;
      baseArrayLayer:TVkUInt32;
      layerCount:TVkUInt32;
     end;
     TVkImageSubresourceRange=record
      aspectMask:TVkImageAspectFlags;
      baseMipLevel:TVkUInt32;
      levelCount:TVkUInt32;
      baseArrayLayer:TVkUInt32;
      layerCount:TVkUInt32;
     end;
     TVkMemoryBarrier=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      srcAccessMask:TVkAccessFlags;
      dstAccessMask:TVkAccessFlags;
     end;
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
     TVkSubresourceLayout=record
      offset:TVkDeviceSize;
      size:TVkDeviceSize;
      rowPitch:TVkDeviceSize;
      arrayPitch:TVkDeviceSize;
      depthPitch:TVkDeviceSize;
     end;
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
     TVkBufferCopy=record
      srcOffset:TVkDeviceSize;
      dstOffset:TVkDeviceSize;
      size:TVkDeviceSize;
     end;
     TVkSparseMemoryBind=record
      resourceOffset:TVkDeviceSize;
      size:TVkDeviceSize;
      memory:TVkDeviceMemory;
      memoryOffset:TVkDeviceSize;
      flags:TVkSparseMemoryBindFlags;
     end;
     TVkSparseImageMemoryBind=record
      subresource:TVkImageSubresource;
      offset:TVkOffset3D;
      extent:TVkExtent3D;
      memory:TVkDeviceMemory;
      memoryOffset:TVkDeviceSize;
      flags:TVkSparseMemoryBindFlags;
     end;
     TVkSparseBufferMemoryBindInfo=record
      buffer:TVkBuffer;
      bindCount:TVkUInt32;
      pBinds:PVkSparseMemoryBind;
     end;
     TVkSparseImageOpaqueMemoryBindInfo=record
      image:TVkImage;
      bindCount:TVkUInt32;
      pBinds:PVkSparseMemoryBind;
     end;
     TVkSparseImageMemoryBindInfo=record
      image:TVkImage;
      bindCount:TVkUInt32;
      pBinds:PVkSparseImageMemoryBind;
     end;
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
     TVkImageCopy=record
      srcSubresource:TVkImageSubresourceLayers;
      srcOffset:TVkOffset3D;
      dstSubresource:TVkImageSubresourceLayers;
      dstOffset:TVkOffset3D;
      extent:TVkExtent3D;
     end;
     TVkImageBlit=record
      srcSubresource:TVkImageSubresourceLayers;
      srcOffsets:array[0..1] of TVkOffset3D;
      dstSubresource:TVkImageSubresourceLayers;
      dstOffsets:array[0..1] of TVkOffset3D;
     end;
     TVkBufferImageCopy=record
      bufferOffset:TVkDeviceSize;
      bufferRowLength:TVkUInt32;
      bufferImageHeight:TVkUInt32;
      imageSubresource:TVkImageSubresourceLayers;
      imageOffset:TVkOffset3D;
      imageExtent:TVkExtent3D;
     end;
     TVkImageResolve=record
      srcSubresource:TVkImageSubresourceLayers;
      srcOffset:TVkOffset3D;
      dstSubresource:TVkImageSubresourceLayers;
      dstOffset:TVkOffset3D;
      extent:TVkExtent3D;
     end;
     TVkShaderModuleCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkShaderModuleCreateFlags;
      codeSize:TVkPtrInt;
      pCode:PVkUInt32;
     end;
     TVkDescriptorSetLayoutBinding=record
      binding:TVkUInt32;
      descriptorType:TVkDescriptorType;
      descriptorCount:TVkUInt32;
      stageFlags:TVkShaderStageFlags;
      pImmutableSamplers:PVkSampler;
     end;
     TVkDescriptorSetLayoutCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkDescriptorSetLayoutCreateFlags;
      bindingCount:TVkUInt32;
      pBindings:PVkDescriptorSetLayoutBinding;
     end;
     TVkDescriptorPoolSize=record
      type_:TVkDescriptorType;
      descriptorCount:TVkUInt32;
     end;
     TVkDescriptorPoolCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkDescriptorPoolCreateFlags;
      maxSets:TVkUInt32;
      poolSizeCount:TVkUInt32;
      pPoolSizes:PVkDescriptorPoolSize;
     end;
     TVkDescriptorSetAllocateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      descriptorPool:TVkDescriptorPool;
      descriptorSetCount:TVkUInt32;
      pSetLayouts:PVkDescriptorSetLayout;
     end;
     TVkSpecializationMapEntry=record
      constantID:TVkUInt32;
      offset:TVkUInt32;
      size:TVkPtrInt;
     end;
     TVkSpecializationInfo=record
      mapEntryCount:TVkUInt32;
      pMapEntries:PVkSpecializationMapEntry;
      dataSize:TVkPtrInt;
      pData:TVkPointer;
     end;
     TVkPipelineShaderStageCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineShaderStageCreateFlags;
      stage:TVkShaderStageFlagBits;
      module:TVkShaderModule;
      pName:PVkChar;
      pSpecializationInfo:PVkSpecializationInfo;
     end;
     TVkComputePipelineCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineCreateFlags;
      stage:TVkPipelineShaderStageCreateInfo;
      layout:TVkPipelineLayout;
      basePipelineHandle:TVkPipeline;
      basePipelineIndex:TVkInt32;
     end;
     TVkVertexInputBindingDescription=record
      binding:TVkUInt32;
      stride:TVkUInt32;
      inputRate:TVkVertexInputRate;
     end;
     TVkVertexInputAttributeDescription=record
      location:TVkUInt32;
      binding:TVkUInt32;
      format:TVkFormat;
      offset:TVkUInt32;
     end;
     TVkPipelineVertexInputStateCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineVertexInputStateCreateFlags;
      vertexBindingDescriptionCount:TVkUInt32;
      pVertexBindingDescriptions:PVkVertexInputBindingDescription;
      vertexAttributeDescriptionCount:TVkUInt32;
      pVertexAttributeDescriptions:PVkVertexInputAttributeDescription;
     end;
     TVkPipelineInputAssemblyStateCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineInputAssemblyStateCreateFlags;
      topology:TVkPrimitiveTopology;
      primitiveRestartEnable:TVkBool32;
     end;
     TVkPipelineTessellationStateCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineTessellationStateCreateFlags;
      patchControlPoints:TVkUInt32;
     end;
     TVkPipelineViewportStateCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineViewportStateCreateFlags;
      viewportCount:TVkUInt32;
      pViewports:PVkViewport;
      scissorCount:TVkUInt32;
      pScissors:PVkRect2D;
     end;
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
     TVkPipelineColorBlendStateCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineColorBlendStateCreateFlags;
      logicOpEnable:TVkBool32;
      logicOp:TVkLogicOp;
      attachmentCount:TVkUInt32;
      pAttachments:PVkPipelineColorBlendAttachmentState;
      blendConstants:TVkFloat;
     end;
     TVkPipelineDynamicStateCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineDynamicStateCreateFlags;
      dynamicStateCount:TVkUInt32;
      pDynamicStates:PVkDynamicState;
     end;
     TVkStencilOpState=record
      failOp:TVkStencilOp;
      passOp:TVkStencilOp;
      depthFailOp:TVkStencilOp;
      compareOp:TVkCompareOp;
      compareMask:TVkUInt32;
      writeMask:TVkUInt32;
      reference:TVkUInt32;
     end;
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
     TVkPipelineCacheCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineCacheCreateFlags;
      initialDataSize:TVkPtrInt;
      pInitialData:TVkPointer;
     end;
     TVkPushConstantRange=record
      stageFlags:TVkShaderStageFlags;
      offset:TVkUInt32;
      size:TVkUInt32;
     end;
     TVkPipelineLayoutCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkPipelineLayoutCreateFlags;
      setLayoutCount:TVkUInt32;
      pSetLayouts:PVkDescriptorSetLayout;
      pushConstantRangeCount:TVkUInt32;
      pPushConstantRanges:PVkPushConstantRange;
     end;
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
     TVkCommandPoolCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkCommandPoolCreateFlags;
      queueFamilyIndex:TVkUInt32;
     end;
     TVkCommandBufferAllocateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      commandPool:TVkCommandPool;
      level:TVkCommandBufferLevel;
      commandBufferCount:TVkUInt32;
     end;
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
     TVkCommandBufferBeginInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkCommandBufferUsageFlags;
      pInheritanceInfo:PVkCommandBufferInheritanceInfo;
     end;
     TVkRenderPassBeginInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      renderPass:TVkRenderPass;
      framebuffer:TVkFramebuffer;
      renderArea:TVkRect2D;
      clearValueCount:TVkUInt32;
      pClearValues:PVkClearValue;
     end;
     TVkClearColorValue=record
      case longint of
       0:(
        float32:TVkFloat;
       );
       1:(
        int32:TVkInt32;
       );
       2:(
        uint32:TVkUInt32;
       );
     end;
     TVkClearDepthStencilValue=record
      depth:TVkFloat;
      stencil:TVkUInt32;
     end;
     TVkClearValue=record
      case longint of
       0:(
        color:TVkClearColorValue;
       );
       1:(
        depthStencil:TVkClearDepthStencilValue;
       );
     end;
     TVkClearAttachment=record
      aspectMask:TVkImageAspectFlags;
      colorAttachment:TVkUInt32;
      clearValue:TVkClearValue;
     end;
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
     TVkAttachmentReference=record
      attachment:TVkUInt32;
      layout:TVkImageLayout;
     end;
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
     TVkSubpassDependency=record
      srcSubpass:TVkUInt32;
      dstSubpass:TVkUInt32;
      srcStageMask:TVkPipelineStageFlags;
      dstStageMask:TVkPipelineStageFlags;
      srcAccessMask:TVkAccessFlags;
      dstAccessMask:TVkAccessFlags;
      dependencyFlags:TVkDependencyFlags;
     end;
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
     TVkEventCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkEventCreateFlags;
     end;
     TVkFenceCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkFenceCreateFlags;
     end;
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
      maxComputeWorkGroupCount:TVkUInt32;
      maxComputeWorkGroupInvocations:TVkUInt32;
      maxComputeWorkGroupSize:TVkUInt32;
      subPixelPrecisionBits:TVkUInt32;
      subTexelPrecisionBits:TVkUInt32;
      mipmapPrecisionBits:TVkUInt32;
      maxDrawIndexedIndexValue:TVkUInt32;
      maxDrawIndirectCount:TVkUInt32;
      maxSamplerLodBias:TVkFloat;
      maxSamplerAnisotropy:TVkFloat;
      maxViewports:TVkUInt32;
      maxViewportDimensions:TVkUInt32;
      viewportBoundsRange:TVkFloat;
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
      pointSizeRange:TVkFloat;
      lineWidthRange:TVkFloat;
      pointSizeGranularity:TVkFloat;
      lineWidthGranularity:TVkFloat;
      strictLines:TVkBool32;
      standardSampleLocations:TVkBool32;
      optimalBufferCopyOffsetAlignment:TVkDeviceSize;
      optimalBufferCopyRowPitchAlignment:TVkDeviceSize;
      nonCoherentAtomSize:TVkDeviceSize;
     end;
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
     TVkSemaphoreCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkSemaphoreCreateFlags;
     end;
     TVkQueryPoolCreateInfo=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkQueryPoolCreateFlags;
      queryType:TVkQueryType;
      queryCount:TVkUInt32;
      pipelineStatistics:TVkQueryPipelineStatisticFlags;
     end;
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
     TVkDrawIndirectCommand=record
      vertexCount:TVkUInt32;
      instanceCount:TVkUInt32;
      firstVertex:TVkUInt32;
      firstInstance:TVkUInt32;
     end;
     TVkDrawIndexedIndirectCommand=record
      indexCount:TVkUInt32;
      instanceCount:TVkUInt32;
      firstIndex:TVkUInt32;
      vertexOffset:TVkInt32;
      firstInstance:TVkUInt32;
     end;
     TVkDispatchIndirectCommand=record
      x:TVkUInt32;
      y:TVkUInt32;
      z:TVkUInt32;
     end;
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
     TVkDisplayPropertiesKHR=record
      display:TVkDisplayKHR;
      displayName:PVkChar;
      physicalDimensions:TVkExtent2D;
      physicalResolution:TVkExtent2D;
      supportedTransforms:TVkSurfaceTransformFlagsKHR;
      planeReorderPossible:TVkBool32;
      persistentContent:TVkBool32;
     end;
     TVkDisplayPlanePropertiesKHR=record
      currentDisplay:TVkDisplayKHR;
      currentStackIndex:TVkUInt32;
     end;
     TVkDisplayModeParametersKHR=record
      visibleRegion:TVkExtent2D;
      refreshRate:TVkUInt32;
     end;
     TVkDisplayModePropertiesKHR=record
      displayMode:TVkDisplayModeKHR;
      parameters:TVkDisplayModeParametersKHR;
     end;
     TVkDisplayModeCreateInfoKHR=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkDisplayModeCreateFlagsKHR;
      parameters:TVkDisplayModeParametersKHR;
     end;
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
     TVkDisplayPresentInfoKHR=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      srcRect:TVkRect2D;
      dstRect:TVkRect2D;
      persistent:TVkBool32;
     end;
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
     TVkAndroidSurfaceCreateInfoKHR=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkAndroidSurfaceCreateFlagsKHR;
      window:PANativeWindow;
     end;
{$endif}
{$ifdef Mir}
     TVkMirSurfaceCreateInfoKHR=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkMirSurfaceCreateFlagsKHR;
      connection:PMirConnection;
      mirSurface:PMirSurface;
     end;
{$endif}
{$ifdef Wayland}
     TVkWaylandSurfaceCreateInfoKHR=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkWaylandSurfaceCreateFlagsKHR;
      display:Pwl_display;
      surface:Pwl_surface;
     end;
{$endif}
{$ifdef Windows}
     TVkWin32SurfaceCreateInfoKHR=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkWin32SurfaceCreateFlagsKHR;
      hinstance_:TVkHINSTANCE;
      hwnd_:TVkHWND;
     end;
{$endif}
{$ifdef X11}
     TVkXlibSurfaceCreateInfoKHR=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkXlibSurfaceCreateFlagsKHR;
      dpy:PDisplay;
      window:TWindow;
     end;
{$endif}
{$ifdef XCB}
     TVkXcbSurfaceCreateInfoKHR=record
      sType:TVkStructureType;
      pNext:TVkPointer;
      flags:TVkXcbSurfaceCreateFlagsKHR;
      connection:Pxcb_connection;
      window:Txcb_window;
     end;
{$endif}
     TVkSurfaceFormatKHR=record
      format:TVkFormat;
      colorSpace:TVkColorSpaceKHR;
     end;
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
     PVulkan=^TVulkan;
     TVulkan=record
      vkCreateInstance:TvkCreateInstance;
      vkDestroyInstance:TvkDestroyInstance;
      vkEnumeratePhysicalDevices:TvkEnumeratePhysicalDevices;
      vkGetDeviceProcAddr:TvkGetDeviceProcAddr;
      vkGetInstanceProcAddr:TvkGetInstanceProcAddr;
      vkGetPhysicalDeviceProperties:TvkGetPhysicalDeviceProperties;
      vkGetPhysicalDeviceQueueFamilyProperties:TvkGetPhysicalDeviceQueueFamilyProperties;
      vkGetPhysicalDeviceMemoryProperties:TvkGetPhysicalDeviceMemoryProperties;
      vkGetPhysicalDeviceFeatures:TvkGetPhysicalDeviceFeatures;
      vkGetPhysicalDeviceFormatProperties:TvkGetPhysicalDeviceFormatProperties;
      vkGetPhysicalDeviceImageFormatProperties:TvkGetPhysicalDeviceImageFormatProperties;
      vkCreateDevice:TvkCreateDevice;
      vkDestroyDevice:TvkDestroyDevice;
      vkEnumerateInstanceLayerProperties:TvkEnumerateInstanceLayerProperties;
      vkEnumerateInstanceExtensionProperties:TvkEnumerateInstanceExtensionProperties;
      vkEnumerateDeviceLayerProperties:TvkEnumerateDeviceLayerProperties;
      vkEnumerateDeviceExtensionProperties:TvkEnumerateDeviceExtensionProperties;
      vkGetDeviceQueue:TvkGetDeviceQueue;
      vkQueueSubmit:TvkQueueSubmit;
      vkQueueWaitIdle:TvkQueueWaitIdle;
      vkDeviceWaitIdle:TvkDeviceWaitIdle;
      vkAllocateMemory:TvkAllocateMemory;
      vkFreeMemory:TvkFreeMemory;
      vkMapMemory:TvkMapMemory;
      vkUnmapMemory:TvkUnmapMemory;
      vkFlushMappedMemoryRanges:TvkFlushMappedMemoryRanges;
      vkInvalidateMappedMemoryRanges:TvkInvalidateMappedMemoryRanges;
      vkGetDeviceMemoryCommitment:TvkGetDeviceMemoryCommitment;
      vkGetBufferMemoryRequirements:TvkGetBufferMemoryRequirements;
      vkBindBufferMemory:TvkBindBufferMemory;
      vkGetImageMemoryRequirements:TvkGetImageMemoryRequirements;
      vkBindImageMemory:TvkBindImageMemory;
      vkGetImageSparseMemoryRequirements:TvkGetImageSparseMemoryRequirements;
      vkGetPhysicalDeviceSparseImageFormatProperties:TvkGetPhysicalDeviceSparseImageFormatProperties;
      vkQueueBindSparse:TvkQueueBindSparse;
      vkCreateFence:TvkCreateFence;
      vkDestroyFence:TvkDestroyFence;
      vkResetFences:TvkResetFences;
      vkGetFenceStatus:TvkGetFenceStatus;
      vkWaitForFences:TvkWaitForFences;
      vkCreateSemaphore:TvkCreateSemaphore;
      vkDestroySemaphore:TvkDestroySemaphore;
      vkCreateEvent:TvkCreateEvent;
      vkDestroyEvent:TvkDestroyEvent;
      vkGetEventStatus:TvkGetEventStatus;
      vkSetEvent:TvkSetEvent;
      vkResetEvent:TvkResetEvent;
      vkCreateQueryPool:TvkCreateQueryPool;
      vkDestroyQueryPool:TvkDestroyQueryPool;
      vkGetQueryPoolResults:TvkGetQueryPoolResults;
      vkCreateBuffer:TvkCreateBuffer;
      vkDestroyBuffer:TvkDestroyBuffer;
      vkCreateBufferView:TvkCreateBufferView;
      vkDestroyBufferView:TvkDestroyBufferView;
      vkCreateImage:TvkCreateImage;
      vkDestroyImage:TvkDestroyImage;
      vkGetImageSubresourceLayout:TvkGetImageSubresourceLayout;
      vkCreateImageView:TvkCreateImageView;
      vkDestroyImageView:TvkDestroyImageView;
      vkCreateShaderModule:TvkCreateShaderModule;
      vkDestroyShaderModule:TvkDestroyShaderModule;
      vkCreatePipelineCache:TvkCreatePipelineCache;
      vkDestroyPipelineCache:TvkDestroyPipelineCache;
      vkGetPipelineCacheData:TvkGetPipelineCacheData;
      vkMergePipelineCaches:TvkMergePipelineCaches;
      vkCreateGraphicsPipelines:TvkCreateGraphicsPipelines;
      vkCreateComputePipelines:TvkCreateComputePipelines;
      vkDestroyPipeline:TvkDestroyPipeline;
      vkCreatePipelineLayout:TvkCreatePipelineLayout;
      vkDestroyPipelineLayout:TvkDestroyPipelineLayout;
      vkCreateSampler:TvkCreateSampler;
      vkDestroySampler:TvkDestroySampler;
      vkCreateDescriptorSetLayout:TvkCreateDescriptorSetLayout;
      vkDestroyDescriptorSetLayout:TvkDestroyDescriptorSetLayout;
      vkCreateDescriptorPool:TvkCreateDescriptorPool;
      vkDestroyDescriptorPool:TvkDestroyDescriptorPool;
      vkResetDescriptorPool:TvkResetDescriptorPool;
      vkAllocateDescriptorSets:TvkAllocateDescriptorSets;
      vkFreeDescriptorSets:TvkFreeDescriptorSets;
      vkUpdateDescriptorSets:TvkUpdateDescriptorSets;
      vkCreateFramebuffer:TvkCreateFramebuffer;
      vkDestroyFramebuffer:TvkDestroyFramebuffer;
      vkCreateRenderPass:TvkCreateRenderPass;
      vkDestroyRenderPass:TvkDestroyRenderPass;
      vkGetRenderAreaGranularity:TvkGetRenderAreaGranularity;
      vkCreateCommandPool:TvkCreateCommandPool;
      vkDestroyCommandPool:TvkDestroyCommandPool;
      vkResetCommandPool:TvkResetCommandPool;
      vkAllocateCommandBuffers:TvkAllocateCommandBuffers;
      vkFreeCommandBuffers:TvkFreeCommandBuffers;
      vkBeginCommandBuffer:TvkBeginCommandBuffer;
      vkEndCommandBuffer:TvkEndCommandBuffer;
      vkResetCommandBuffer:TvkResetCommandBuffer;
      vkCmdBindPipeline:TvkCmdBindPipeline;
      vkCmdSetViewport:TvkCmdSetViewport;
      vkCmdSetScissor:TvkCmdSetScissor;
      vkCmdSetLineWidth:TvkCmdSetLineWidth;
      vkCmdSetDepthBias:TvkCmdSetDepthBias;
      vkCmdSetBlendConstants:TvkCmdSetBlendConstants;
      vkCmdSetDepthBounds:TvkCmdSetDepthBounds;
      vkCmdSetStencilCompareMask:TvkCmdSetStencilCompareMask;
      vkCmdSetStencilWriteMask:TvkCmdSetStencilWriteMask;
      vkCmdSetStencilReference:TvkCmdSetStencilReference;
      vkCmdBindDescriptorSets:TvkCmdBindDescriptorSets;
      vkCmdBindIndexBuffer:TvkCmdBindIndexBuffer;
      vkCmdBindVertexBuffers:TvkCmdBindVertexBuffers;
      vkCmdDraw:TvkCmdDraw;
      vkCmdDrawIndexed:TvkCmdDrawIndexed;
      vkCmdDrawIndirect:TvkCmdDrawIndirect;
      vkCmdDrawIndexedIndirect:TvkCmdDrawIndexedIndirect;
      vkCmdDispatch:TvkCmdDispatch;
      vkCmdDispatchIndirect:TvkCmdDispatchIndirect;
      vkCmdCopyBuffer:TvkCmdCopyBuffer;
      vkCmdCopyImage:TvkCmdCopyImage;
      vkCmdBlitImage:TvkCmdBlitImage;
      vkCmdCopyBufferToImage:TvkCmdCopyBufferToImage;
      vkCmdCopyImageToBuffer:TvkCmdCopyImageToBuffer;
      vkCmdUpdateBuffer:TvkCmdUpdateBuffer;
      vkCmdFillBuffer:TvkCmdFillBuffer;
      vkCmdClearColorImage:TvkCmdClearColorImage;
      vkCmdClearDepthStencilImage:TvkCmdClearDepthStencilImage;
      vkCmdClearAttachments:TvkCmdClearAttachments;
      vkCmdResolveImage:TvkCmdResolveImage;
      vkCmdSetEvent:TvkCmdSetEvent;
      vkCmdResetEvent:TvkCmdResetEvent;
      vkCmdWaitEvents:TvkCmdWaitEvents;
      vkCmdPipelineBarrier:TvkCmdPipelineBarrier;
      vkCmdBeginQuery:TvkCmdBeginQuery;
      vkCmdEndQuery:TvkCmdEndQuery;
      vkCmdResetQueryPool:TvkCmdResetQueryPool;
      vkCmdWriteTimestamp:TvkCmdWriteTimestamp;
      vkCmdCopyQueryPoolResults:TvkCmdCopyQueryPoolResults;
      vkCmdPushConstants:TvkCmdPushConstants;
      vkCmdBeginRenderPass:TvkCmdBeginRenderPass;
      vkCmdNextSubpass:TvkCmdNextSubpass;
      vkCmdEndRenderPass:TvkCmdEndRenderPass;
      vkCmdExecuteCommands:TvkCmdExecuteCommands;
{$ifdef Android}
      vkCreateAndroidSurfaceKHR:TvkCreateAndroidSurfaceKHR;
{$endif}
      vkGetPhysicalDeviceDisplayPropertiesKHR:TvkGetPhysicalDeviceDisplayPropertiesKHR;
      vkGetPhysicalDeviceDisplayPlanePropertiesKHR:TvkGetPhysicalDeviceDisplayPlanePropertiesKHR;
      vkGetDisplayPlaneSupportedDisplaysKHR:TvkGetDisplayPlaneSupportedDisplaysKHR;
      vkGetDisplayModePropertiesKHR:TvkGetDisplayModePropertiesKHR;
      vkCreateDisplayModeKHR:TvkCreateDisplayModeKHR;
      vkGetDisplayPlaneCapabilitiesKHR:TvkGetDisplayPlaneCapabilitiesKHR;
      vkCreateDisplayPlaneSurfaceKHR:TvkCreateDisplayPlaneSurfaceKHR;
      vkCreateSharedSwapchainsKHR:TvkCreateSharedSwapchainsKHR;
{$ifdef Mir}
      vkCreateMirSurfaceKHR:TvkCreateMirSurfaceKHR;
{$endif}
{$ifdef Mir}
      vkGetPhysicalDeviceMirPresentationSupportKHR:TvkGetPhysicalDeviceMirPresentationSupportKHR;
{$endif}
      vkDestroySurfaceKHR:TvkDestroySurfaceKHR;
      vkGetPhysicalDeviceSurfaceSupportKHR:TvkGetPhysicalDeviceSurfaceSupportKHR;
      vkGetPhysicalDeviceSurfaceCapabilitiesKHR:TvkGetPhysicalDeviceSurfaceCapabilitiesKHR;
      vkGetPhysicalDeviceSurfaceFormatsKHR:TvkGetPhysicalDeviceSurfaceFormatsKHR;
      vkGetPhysicalDeviceSurfacePresentModesKHR:TvkGetPhysicalDeviceSurfacePresentModesKHR;
      vkCreateSwapchainKHR:TvkCreateSwapchainKHR;
      vkDestroySwapchainKHR:TvkDestroySwapchainKHR;
      vkGetSwapchainImagesKHR:TvkGetSwapchainImagesKHR;
      vkAcquireNextImageKHR:TvkAcquireNextImageKHR;
      vkQueuePresentKHR:TvkQueuePresentKHR;
{$ifdef Wayland}
      vkCreateWaylandSurfaceKHR:TvkCreateWaylandSurfaceKHR;
{$endif}
{$ifdef Wayland}
      vkGetPhysicalDeviceWaylandPresentationSupportKHR:TvkGetPhysicalDeviceWaylandPresentationSupportKHR;
{$endif}
      vkCreateWin32SurfaceKHR:TvkCreateWin32SurfaceKHR;
      vkGetPhysicalDeviceWin32PresentationSupportKHR:TvkGetPhysicalDeviceWin32PresentationSupportKHR;
{$ifdef X11}
      vkCreateXlibSurfaceKHR:TvkCreateXlibSurfaceKHR;
{$endif}
{$ifdef X11}
      vkGetPhysicalDeviceXlibPresentationSupportKHR:TvkGetPhysicalDeviceXlibPresentationSupportKHR;
{$endif}
{$ifdef XCB}
      vkCreateXcbSurfaceKHR:TvkCreateXcbSurfaceKHR;
{$endif}
{$ifdef XCB}
      vkGetPhysicalDeviceXcbPresentationSupportKHR:TvkGetPhysicalDeviceXcbPresentationSupportKHR;
{$endif}
      vkCreateDebugReportCallbackEXT:TvkCreateDebugReportCallbackEXT;
      vkDestroyDebugReportCallbackEXT:TvkDestroyDebugReportCallbackEXT;
      vkDebugReportMessageEXT:TvkDebugReportMessageEXT;
     end;
var LibVulkan:pointer=nil;
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
function LoadVulkanLibrary:boolean;
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
function LoadVulkanLibrary:boolean;
begin
{$ifdef Windows}
 LibVulkan:=vkLoadLibrary('vulkan.dll');
{$else}
{$ifdef Unix}
 LibVulkan:=vkLoadLibrary('libvulkan.so');
{$else}
 LibVulkan:=nil;
{$endif}
{$endif}
 result:=assigned(LibVulkan);
 if result then begin
  vkEnumerateInstanceExtensionProperties:=vkGetProcAddress(LibVulkan,'vkEnumerateInstanceExtensionProperties');
  vkEnumerateInstanceLayerProperties:=vkGetProcAddress(LibVulkan,'vkEnumerateInstanceLayerProperties');
  vkCreateInstance:=vkGetProcAddress(LibVulkan,'vkCreateInstance');
  vkGetInstanceProcAddr:=vkGetProcAddress(LibVulkan,'vkGetInstanceProcAddr');
  vkGetDeviceProcAddr:=vkGetProcAddress(LibVulkan,'vkGetDeviceProcAddr');
  result:=assigned(vkEnumerateInstanceExtensionProperties) and
          assigned(vkEnumerateInstanceLayerProperties) and
          assigned(vkCreateInstance) and
          assigned(vkGetInstanceProcAddr) and
          assigned(vkGetDeviceProcAddr);
 end;
end;
end.
