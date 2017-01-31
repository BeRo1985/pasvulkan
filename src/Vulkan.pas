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
unit Vulkan;
{$ifdef fpc}
 {$mode delphi}
 {$z4}
 {$packrecords c}
 {$define CAN_INLINE}
 {$define HAS_ADVANCED_RECORDS}
 {$notes off}
{$else}
 {$z4}
 {$undef CAN_INLINE}
 {$undef HAS_ADVANCED_RECORDS}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
  {$if CompilerVersion>=18.0}
   {$define CAN_INLINE}
   {$define HAS_ADVANCED_RECORDS}
  {$ifend}
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

const VK_DEFAULT_LIB_NAME={$ifdef Windows}'vulkan-1.dll'{$else}{$ifdef Unix}'libvulkan.so'{$else}'libvulkan'{$endif}{$endif};

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

     PPVkVoid=^PVkVoid;
     PVkVoid=pointer;

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

     PPVkSizeUInt=^PVkSizeUInt;
     PVkSizeUInt=^TVkSizeUInt;
     TVkSizeUInt=TVkPtrUInt;

     PPVkSizeInt=^PVkSizeInt;
     PVkSizeInt=^TVkSizeInt;
     TVkSizeInt=TVkPtrInt;

     PPVkSize=^PVkSizeUInt;
     PVkSize=^TVkSizeUInt;
     TVkSize=TVkPtrUInt;

     PPVkPtrDiff=^PVkPtrDiff;
     PVkPtrDiff=^TVkPtrDiff;
     TVkPtrDiff=TVkPtrInt;

     PPVkCharString=^PVkCharString;
     PVkCharString=^TVkCharString;
     TVkCharString=AnsiString;

const VK_NULL_HANDLE=0;

      VK_NULL_INSTANCE=0;

      VK_API_VERSION=(1 shl 22) or (0 shl 12) or (0 shl 0);

      VK_API_VERSION_1_0=(1 shl 22) or (0 shl 12) or (0 shl 0);

      VK_HEADER_VERSION=39;

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
      VK_KHR_SURFACE_SPEC_VERSION=25;
      VK_KHR_SURFACE_EXTENSION_NAME='VK_KHR_surface';
      VK_KHR_SWAPCHAIN_SPEC_VERSION=68;
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
      VK_EXT_DEBUG_REPORT_SPEC_VERSION=4;
      VK_EXT_DEBUG_REPORT_EXTENSION_NAME='VK_EXT_debug_report';
      VK_NV_GLSL_SHADER_SPEC_VERSION=1;
      VK_NV_GLSL_SHADER_EXTENSION_NAME='VK_NV_glsl_shader';
      VK_NV_EXTENSION_1_SPEC_VERSION=0;
      VK_NV_EXTENSION_1_EXTENSION_NAME='VK_NV_extension_1';
      VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION=1;
      VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME='VK_KHR_sampler_mirror_clamp_to_edge';
      VK_IMG_FILTER_CUBIC_SPEC_VERSION=1;
      VK_IMG_FILTER_CUBIC_EXTENSION_NAME='VK_IMG_filter_cubic';
      VK_AMD_EXTENSION_17_SPEC_VERSION=0;
      VK_AMD_EXTENSION_17_EXTENSION_NAME='VK_AMD_extension_17';
      VK_AMD_EXTENSION_18_SPEC_VERSION=0;
      VK_AMD_EXTENSION_18_EXTENSION_NAME='VK_AMD_extension_18';
      VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION=1;
      VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME='VK_AMD_rasterization_order';
      VK_AMD_EXTENSION_20_SPEC_VERSION=0;
      VK_AMD_EXTENSION_20_EXTENSION_NAME='VK_AMD_extension_20';
      VK_AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION=1;
      VK_AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME='VK_AMD_shader_trinary_minmax';
      VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_SPEC_VERSION=1;
      VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME='VK_AMD_shader_explicit_vertex_parameter';
      VK_EXT_DEBUG_MARKER_SPEC_VERSION=3;
      VK_EXT_DEBUG_MARKER_EXTENSION_NAME='VK_EXT_debug_marker';
      VK_AMD_EXTENSION_24_SPEC_VERSION=0;
      VK_AMD_EXTENSION_24_EXTENSION_NAME='VK_AMD_extension_24';
      VK_AMD_EXTENSION_25_SPEC_VERSION=0;
      VK_AMD_EXTENSION_25_EXTENSION_NAME='VK_AMD_extension_25';
      VK_AMD_GCN_SHADER_SPEC_VERSION=1;
      VK_AMD_GCN_SHADER_EXTENSION_NAME='VK_AMD_gcn_shader';
      VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION=1;
      VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME='VK_NV_dedicated_allocation';
      VK_EXT_EXTENSION_28_SPEC_VERSION=0;
      VK_EXT_EXTENSION_28_EXTENSION_NAME='VK_NV_extension_28';
      VK_NVX_EXTENSION_29_SPEC_VERSION=0;
      VK_NVX_EXTENSION_29_EXTENSION_NAME='VK_NVX_extension_29';
      VK_NVX_EXTENSION_30_SPEC_VERSION=0;
      VK_NVX_EXTENSION_30_EXTENSION_NAME='VK_NVX_extension_30';
      VK_NVX_EXTENSION_31_SPEC_VERSION=0;
      VK_NVX_EXTENSION_31_EXTENSION_NAME='VK_NVX_extension_31';
      VK_AMD_EXTENSION_32_SPEC_VERSION=0;
      VK_AMD_EXTENSION_32_EXTENSION_NAME='VK_AMD_extension_32';
      VK_AMD_EXTENSION_33_SPEC_VERSION=0;
      VK_AMD_EXTENSION_33_EXTENSION_NAME='VK_AMD_extension_33';
      VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION=1;
      VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME='VK_AMD_draw_indirect_count';
      VK_AMD_EXTENSION_35_SPEC_VERSION=0;
      VK_AMD_EXTENSION_35_EXTENSION_NAME='VK_AMD_extension_35';
      VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION=1;
      VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME='VK_AMD_negative_viewport_height';
      VK_AMD_GPU_SHADER_HALF_FLOAT_SPEC_VERSION=1;
      VK_AMD_GPU_SHADER_HALF_FLOAT_EXTENSION_NAME='VK_AMD_gpu_shader_half_float';
      VK_AMD_SHADER_BALLOT_SPEC_VERSION=1;
      VK_AMD_SHADER_BALLOT_EXTENSION_NAME='VK_AMD_shader_ballot';
      VK_AMD_EXTENSION_39_SPEC_VERSION=0;
      VK_AMD_EXTENSION_39_EXTENSION_NAME='VK_AMD_extension_39';
      VK_AMD_EXTENSION_40_SPEC_VERSION=0;
      VK_AMD_EXTENSION_40_EXTENSION_NAME='VK_AMD_extension_40';
      VK_AMD_EXTENSION_41_SPEC_VERSION=0;
      VK_AMD_EXTENSION_41_EXTENSION_NAME='VK_AMD_extension_41';
      VK_AMD_EXTENSION_42_SPEC_VERSION=0;
      VK_AMD_EXTENSION_42_EXTENSION_NAME='VK_AMD_extension_42';
      VK_AMD_EXTENSION_43_SPEC_VERSION=0;
      VK_AMD_EXTENSION_43_EXTENSION_NAME='VK_AMD_extension_43';
      VK_AMD_EXTENSION_44_SPEC_VERSION=0;
      VK_AMD_EXTENSION_44_EXTENSION_NAME='VK_AMD_extension_44';
      VK_AMD_EXTENSION_45_SPEC_VERSION=0;
      VK_AMD_EXTENSION_45_EXTENSION_NAME='VK_AMD_extension_45';
      VK_AMD_EXTENSION_46_SPEC_VERSION=0;
      VK_AMD_EXTENSION_46_EXTENSION_NAME='VK_AMD_extension_46';
      VK_AMD_EXTENSION_47_SPEC_VERSION=0;
      VK_AMD_EXTENSION_47_EXTENSION_NAME='VK_AMD_extension_47';
      VK_NVX_EXTENSION_48_SPEC_VERSION=0;
      VK_NVX_EXTENSION_48_EXTENSION_NAME='VK_NVX_extension_48';
      VK_GOOGLE_EXTENSION_49_SPEC_VERSION=0;
      VK_GOOGLE_EXTENSION_49_EXTENSION_NAME='VK_GOOGLE_extension_49';
      VK_GOOGLE_EXTENSION_50_SPEC_VERSION=0;
      VK_GOOGLE_EXTENSION_50_EXTENSION_NAME='VK_GOOGLE_extension_50';
      VK_NVX_EXTENSION_51_SPEC_VERSION=0;
      VK_NVX_EXTENSION_51_EXTENSION_NAME='VK_NVX_extension_51';
      VK_NVX_EXTENSION_52_SPEC_VERSION=0;
      VK_NVX_EXTENSION_52_EXTENSION_NAME='VK_NVX_extension_52';
      VK_NV_EXTENSION_53_SPEC_VERSION=0;
      VK_NV_EXTENSION_53_EXTENSION_NAME='VK_NV_extension_53';
      VK_NV_EXTENSION_54_SPEC_VERSION=0;
      VK_NV_EXTENSION_54_EXTENSION_NAME='VK_NV_extension_54';
      VK_IMG_FORMAT_PVRTC_SPEC_VERSION=1;
      VK_IMG_FORMAT_PVRTC_EXTENSION_NAME='VK_IMG_format_pvrtc';
      VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION=1;
      VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME='VK_NV_external_memory_capabilities';
      VK_NV_EXTERNAL_MEMORY_SPEC_VERSION=1;
      VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME='VK_NV_external_memory';
      VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION=1;
      VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME='VK_NV_external_memory_win32';
      VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION=1;
      VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME='VK_NV_win32_keyed_mutex';
      VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION=1;
      VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME='VK_KHR_get_physical_device_properties2';
      VK_KHR_EXTENSION_61_SPEC_VERSION=0;
      VK_KHR_EXTENSION_61_EXTENSION_NAME='VK_KHR_extension_61';
      VK_EXT_VALIDATION_FLAGS_SPEC_VERSION=1;
      VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME='VK_EXT_validation_flags';
      VK_NN_VI_SURFACE_SPEC_VERSION=1;
      VK_NN_VI_SURFACE_EXTENSION_NAME='VK_NN_vi_surface';
      VK_KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION=1;
      VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME='VK_KHR_shader_draw_parameters';
      VK_EXT_SHADER_SUBGROUP_BALLOT_SPEC_VERSION=1;
      VK_EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME='VK_EXT_shader_subgroup_ballot';
      VK_EXT_SHADER_SUBGROUP_VOTE_SPEC_VERSION=1;
      VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME='VK_EXT_shader_subgroup_vote';
      VK_ARM_EXTENSION_01_SPEC_VERSION=0;
      VK_ARM_EXTENSION_01_EXTENSION_NAME='VK_ARM_extension_01';
      VK_ARM_EXTENSION_02_SPEC_VERSION=0;
      VK_ARM_EXTENSION_02_EXTENSION_NAME='VK_ARM_extension_02';
      VK_IMG_EXTENSION_69_SPEC_VERSION=0;
      VK_IMG_EXTENSION_69_EXTENSION_NAME='VK_IMG_extension_69';
      VK_KHR_MAINTENANCE1_SPEC_VERSION=1;
      VK_KHR_MAINTENANCE1_EXTENSION_NAME='VK_KHR_maintenance1';
      VK_KHR_EXTENSION_71_SPEC_VERSION=0;
      VK_KHR_EXTENSION_71_EXTENSION_NAME='VK_KHR_extension_71';
      VK_KHR_EXTENSION_72_SPEC_VERSION=0;
      VK_KHR_EXTENSION_72_EXTENSION_NAME='VK_KHR_extension_72';
      VK_KHR_EXTENSION_73_SPEC_VERSION=0;
      VK_KHR_EXTENSION_73_EXTENSION_NAME='VK_KHR_extension_73';
      VK_KHR_EXTENSION_74_SPEC_VERSION=0;
      VK_KHR_EXTENSION_74_EXTENSION_NAME='VK_KHR_extension_74';
      VK_KHR_EXTENSION_75_SPEC_VERSION=0;
      VK_KHR_EXTENSION_75_EXTENSION_NAME='VK_KHR_extension_75';
      VK_KHR_EXTENSION_76_SPEC_VERSION=0;
      VK_KHR_EXTENSION_76_EXTENSION_NAME='VK_KHR_extension_76';
      VK_KHR_EXTENSION_77_SPEC_VERSION=0;
      VK_KHR_EXTENSION_77_EXTENSION_NAME='VK_KHR_extension_77';
      VK_KHR_EXTENSION_78_SPEC_VERSION=0;
      VK_KHR_EXTENSION_78_EXTENSION_NAME='VK_KHR_extension_78';
      VK_KHR_EXTENSION_79_SPEC_VERSION=0;
      VK_KHR_EXTENSION_79_EXTENSION_NAME='VK_KHR_extension_79';
      VK_KHR_EXTENSION_80_SPEC_VERSION=0;
      VK_KHR_EXTENSION_80_EXTENSION_NAME='VK_KHR_extension_80';
      VK_KHR_EXTENSION_81_SPEC_VERSION=0;
      VK_KHR_EXTENSION_81_EXTENSION_NAME='VK_KHR_extension_81';
      VK_KHR_EXTENSION_82_SPEC_VERSION=0;
      VK_KHR_EXTENSION_82_EXTENSION_NAME='VK_KHR_extension_82';
      VK_KHR_EXTENSION_83_SPEC_VERSION=0;
      VK_KHR_EXTENSION_83_EXTENSION_NAME='VK_KHR_extension_83';
      VK_KHR_EXTENSION_84_SPEC_VERSION=0;
      VK_KHR_EXTENSION_84_EXTENSION_NAME='VK_KHR_extension_84';
      VK_KHR_EXTENSION_85_SPEC_VERSION=0;
      VK_KHR_EXTENSION_85_EXTENSION_NAME='VK_KHR_extension_85';
      VK_KHR_EXTENSION_86_SPEC_VERSION=0;
      VK_KHR_EXTENSION_86_EXTENSION_NAME='VK_KHR_extension_86';
      VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION=1;
      VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME='VK_NVX_device_generated_commands';
      VK_KHR_EXTENSION_88_SPEC_VERSION=0;
      VK_KHR_EXTENSION_88_EXTENSION_NAME='VK_KHR_extension_88';
      VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION=1;
      VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME='VK_EXT_direct_mode_display';
      VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION=1;
      VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME='VK_EXT_acquire_xlib_display';
      VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION=1;
      VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME='VK_EXT_display_surface_counter';
      VK_EXT_DISPLAY_CONTROL_SPEC_VERSION=1;
      VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME='VK_EXT_display_control';
      VK_GOOGLE_EXTENSION_93_SPEC_VERSION=0;
      VK_GOOGLE_EXTENSION_93_EXTENSION_NAME='VK_GOOGLE_extension_93';
      VK_KHR_EXTENSION_94_SPEC_VERSION=0;
      VK_KHR_EXTENSION_94_EXTENSION_NAME='VK_KHR_extension_94';
      VK_NV_EXTENSION_95_SPEC_VERSION=0;
      VK_NV_EXTENSION_95_EXTENSION_NAME='VK_NV_extension_95';
      VK_NV_EXTENSION_96_SPEC_VERSION=0;
      VK_NV_EXTENSION_96_EXTENSION_NAME='VK_NV_extension_96';
      VK_NV_EXTENSION_97_SPEC_VERSION=0;
      VK_NV_EXTENSION_97_EXTENSION_NAME='VK_NV_extension_97';
      VK_NV_EXTENSION_98_SPEC_VERSION=0;
      VK_NV_EXTENSION_98_EXTENSION_NAME='VK_NV_extension_98';
      VK_NV_EXTENSION_99_SPEC_VERSION=0;
      VK_NV_EXTENSION_99_EXTENSION_NAME='VK_NV_extension_99';
      VK_NV_EXTENSION_100_SPEC_VERSION=0;
      VK_NV_EXTENSION_100_EXTENSION_NAME='VK_NV_extension_100';
      VK_NV_EXTENSION_101_SPEC_VERSION=0;
      VK_NV_EXTENSION_101_EXTENSION_NAME='VK_NV_extension_101';
      VK_NV_EXTENSION_102_SPEC_VERSION=0;
      VK_NV_EXTENSION_102_EXTENSION_NAME='VK_NV_extension_102';
      VK_NV_EXTENSION_103_SPEC_VERSION=0;
      VK_NV_EXTENSION_103_EXTENSION_NAME='VK_NV_extension_103';
      VK_NV_EXTENSION_104_SPEC_VERSION=0;
      VK_NV_EXTENSION_104_EXTENSION_NAME='VK_NV_extension_104';
      VK_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION=1;
      VK_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME='VK_EXT_swapchain_colorspace';
      VK_KHR_EXTENSION_106_SPEC_VERSION=0;
      VK_KHR_EXTENSION_106_EXTENSION_NAME='VK_EXT_extension_106';
      VK_IMG_EXTENSION_107_SPEC_VERSION=0;
      VK_IMG_EXTENSION_107_EXTENSION_NAME='VK_IMG_extension_107';
      VK_IMG_EXTENSION_108_SPEC_VERSION=0;
      VK_IMG_EXTENSION_108_EXTENSION_NAME='VK_IMG_extension_108';
      VK_IMG_EXTENSION_109_SPEC_VERSION=0;
      VK_IMG_EXTENSION_109_EXTENSION_NAME='VK_IMG_extension_109';
      VK_IMG_EXTENSION_110_SPEC_VERSION=0;
      VK_IMG_EXTENSION_110_EXTENSION_NAME='VK_IMG_extension_110';
      VK_IMG_EXTENSION_111_SPEC_VERSION=0;
      VK_IMG_EXTENSION_111_EXTENSION_NAME='VK_IMG_extension_111';
      VK_KHR_EXTENSION_112_SPEC_VERSION=0;
      VK_KHR_EXTENSION_112_EXTENSION_NAME='VK_KHR_extension_112';
      VK_KHR_EXTENSION_113_SPEC_VERSION=0;
      VK_KHR_EXTENSION_113_EXTENSION_NAME='VK_KHR_extension_113';
      VK_KHR_EXTENSION_114_SPEC_VERSION=0;
      VK_KHR_EXTENSION_114_EXTENSION_NAME='VK_KHR_extension_114';
      VK_KHR_EXTENSION_115_SPEC_VERSION=0;
      VK_KHR_EXTENSION_115_EXTENSION_NAME='VK_KHR_extension_115';
      VK_KHR_EXTENSION_116_SPEC_VERSION=0;
      VK_KHR_EXTENSION_116_EXTENSION_NAME='VK_KHR_extension_116';
      VK_KHR_EXTENSION_117_SPEC_VERSION=0;
      VK_KHR_EXTENSION_117_EXTENSION_NAME='VK_KHR_extension_117';

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

     PPVkIndirectCommandsLayoutUsageFlagsNVX=^PVkIndirectCommandsLayoutUsageFlagsNVX;
     PVkIndirectCommandsLayoutUsageFlagsNVX=^TVkIndirectCommandsLayoutUsageFlagsNVX;
     TVkIndirectCommandsLayoutUsageFlagsNVX=TVkFlags;

     PPVkObjectEntryUsageFlagsNVX=^PVkObjectEntryUsageFlagsNVX;
     PVkObjectEntryUsageFlagsNVX=^TVkObjectEntryUsageFlagsNVX;
     TVkObjectEntryUsageFlagsNVX=TVkFlags;

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

     PPVkViSurfaceCreateFlagsNN=^PVkViSurfaceCreateFlagsNN;
     PVkViSurfaceCreateFlagsNN=^TVkViSurfaceCreateFlagsNN;
     TVkViSurfaceCreateFlagsNN=TVkFlags;

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

     PPVkCommandPoolTrimFlagsKHR=^PVkCommandPoolTrimFlagsKHR;
     PVkCommandPoolTrimFlagsKHR=^TVkCommandPoolTrimFlagsKHR;
     TVkCommandPoolTrimFlagsKHR=TVkFlags;

     PPVkExternalMemoryHandleTypeFlagsNV=^PVkExternalMemoryHandleTypeFlagsNV;
     PVkExternalMemoryHandleTypeFlagsNV=^TVkExternalMemoryHandleTypeFlagsNV;
     TVkExternalMemoryHandleTypeFlagsNV=TVkFlags;

     PPVkExternalMemoryFeatureFlagsNV=^PVkExternalMemoryFeatureFlagsNV;
     PVkExternalMemoryFeatureFlagsNV=^TVkExternalMemoryFeatureFlagsNV;
     TVkExternalMemoryFeatureFlagsNV=TVkFlags;

     PPVkSurfaceCounterFlagsEXT=^PVkSurfaceCounterFlagsEXT;
     PVkSurfaceCounterFlagsEXT=^TVkSurfaceCounterFlagsEXT;
     TVkSurfaceCounterFlagsEXT=TVkFlags;

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

     PPVkObjectTableNVX=^PVkObjectTableNVX;
     PVkObjectTableNVX=^TVkObjectTableNVX;
     TVkObjectTableNVX=TVkNonDispatchableHandle;

     PPVkIndirectCommandsLayoutNVX=^PVkIndirectCommandsLayoutNVX;
     PVkIndirectCommandsLayoutNVX=^TVkIndirectCommandsLayoutNVX;
     TVkIndirectCommandsLayoutNVX=TVkNonDispatchableHandle;

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
       VK_IMAGE_LAYOUT_UNDEFINED=0,                                              //< Implicit layout an image is when its contents are undefined due to various reasons (e.g. right after creation)
       VK_IMAGE_LAYOUT_GENERAL=1,                                                //< General layout when image can be used for any kind of access
       VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL=2,                               //< Optimal layout when image is only used for color attachment read/write
       VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL=3,                       //< Optimal layout when image is only used for depth/stencil attachment read/write
       VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL=4,                        //< Optimal layout when image is used for read only depth/stencil attachment and shader access
       VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL=5,                               //< Optimal layout when image is used for read only shader access
       VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL=6,                                   //< Optimal layout when image is used only as source of transfer operations
       VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL=7,                                   //< Optimal layout when image is used only as destination of transfer operations
       VK_IMAGE_LAYOUT_PREINITIALIZED=8,                                         //< Initial layout used when the data is populated by the CPU
       VK_IMAGE_LAYOUT_PRESENT_SRC_KHR=1000001002
      );

     PPVkAttachmentLoadOp=^PVkAttachmentLoadOp;
     PVkAttachmentLoadOp=^TVkAttachmentLoadOp;
     TVkAttachmentLoadOp=
      (
       VK_ATTACHMENT_LOAD_OP_LOAD=0,
       VK_ATTACHMENT_LOAD_OP_CLEAR=1,
       VK_ATTACHMENT_LOAD_OP_DONT_CARE=2
      );

     PPVkAttachmentStoreOp=^PVkAttachmentStoreOp;
     PVkAttachmentStoreOp=^TVkAttachmentStoreOp;
     TVkAttachmentStoreOp=
      (
       VK_ATTACHMENT_STORE_OP_STORE=0,
       VK_ATTACHMENT_STORE_OP_DONT_CARE=1
      );

     PPVkImageType=^PVkImageType;
     PVkImageType=^TVkImageType;
     TVkImageType=
      (
       VK_IMAGE_TYPE_1D=0,
       VK_IMAGE_TYPE_2D=1,
       VK_IMAGE_TYPE_3D=2
      );

     PPVkImageTiling=^PVkImageTiling;
     PVkImageTiling=^TVkImageTiling;
     TVkImageTiling=
      (
       VK_IMAGE_TILING_OPTIMAL=0,
       VK_IMAGE_TILING_LINEAR=1
      );

     PPVkImageViewType=^PVkImageViewType;
     PVkImageViewType=^TVkImageViewType;
     TVkImageViewType=
      (
       VK_IMAGE_VIEW_TYPE_1D=0,
       VK_IMAGE_VIEW_TYPE_2D=1,
       VK_IMAGE_VIEW_TYPE_3D=2,
       VK_IMAGE_VIEW_TYPE_CUBE=3,
       VK_IMAGE_VIEW_TYPE_1D_ARRAY=4,
       VK_IMAGE_VIEW_TYPE_2D_ARRAY=5,
       VK_IMAGE_VIEW_TYPE_CUBE_ARRAY=6
      );

     PPVkCommandBufferLevel=^PVkCommandBufferLevel;
     PVkCommandBufferLevel=^TVkCommandBufferLevel;
     TVkCommandBufferLevel=
      (
       VK_COMMAND_BUFFER_LEVEL_PRIMARY=0,
       VK_COMMAND_BUFFER_LEVEL_SECONDARY=1
      );

     PPVkComponentSwizzle=^PVkComponentSwizzle;
     PVkComponentSwizzle=^TVkComponentSwizzle;
     TVkComponentSwizzle=
      (
       VK_COMPONENT_SWIZZLE_IDENTITY=0,
       VK_COMPONENT_SWIZZLE_ZERO=1,
       VK_COMPONENT_SWIZZLE_ONE=2,
       VK_COMPONENT_SWIZZLE_R=3,
       VK_COMPONENT_SWIZZLE_G=4,
       VK_COMPONENT_SWIZZLE_B=5,
       VK_COMPONENT_SWIZZLE_A=6
      );

     PPVkDescriptorType=^PVkDescriptorType;
     PVkDescriptorType=^TVkDescriptorType;
     TVkDescriptorType=
      (
       VK_DESCRIPTOR_TYPE_SAMPLER=0,
       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER=1,
       VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE=2,
       VK_DESCRIPTOR_TYPE_STORAGE_IMAGE=3,
       VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER=4,
       VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER=5,
       VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER=6,
       VK_DESCRIPTOR_TYPE_STORAGE_BUFFER=7,
       VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC=8,
       VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC=9,
       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT=10
      );

     PPVkQueryType=^PVkQueryType;
     PVkQueryType=^TVkQueryType;
     TVkQueryType=
      (
       VK_QUERY_TYPE_OCCLUSION=0,
       VK_QUERY_TYPE_PIPELINE_STATISTICS=1,                                      //< Optional
       VK_QUERY_TYPE_TIMESTAMP=2
      );

     PPVkBorderColor=^PVkBorderColor;
     PVkBorderColor=^TVkBorderColor;
     TVkBorderColor=
      (
       VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK=0,
       VK_BORDER_COLOR_INT_TRANSPARENT_BLACK=1,
       VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK=2,
       VK_BORDER_COLOR_INT_OPAQUE_BLACK=3,
       VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE=4,
       VK_BORDER_COLOR_INT_OPAQUE_WHITE=5
      );

     PPVkPipelineBindPoint=^PVkPipelineBindPoint;
     PVkPipelineBindPoint=^TVkPipelineBindPoint;
     TVkPipelineBindPoint=
      (
       VK_PIPELINE_BIND_POINT_GRAPHICS=0,
       VK_PIPELINE_BIND_POINT_COMPUTE=1
      );

     PPVkPipelineCacheHeaderVersion=^PVkPipelineCacheHeaderVersion;
     PVkPipelineCacheHeaderVersion=^TVkPipelineCacheHeaderVersion;
     TVkPipelineCacheHeaderVersion=
      (
       VK_PIPELINE_CACHE_HEADER_VERSION_ONE=1
      );

     PPVkPrimitiveTopology=^PVkPrimitiveTopology;
     PVkPrimitiveTopology=^TVkPrimitiveTopology;
     TVkPrimitiveTopology=
      (
       VK_PRIMITIVE_TOPOLOGY_POINT_LIST=0,
       VK_PRIMITIVE_TOPOLOGY_LINE_LIST=1,
       VK_PRIMITIVE_TOPOLOGY_LINE_STRIP=2,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST=3,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP=4,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN=5,
       VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY=6,
       VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY=7,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY=8,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY=9,
       VK_PRIMITIVE_TOPOLOGY_PATCH_LIST=10
      );

     PPVkSharingMode=^PVkSharingMode;
     PVkSharingMode=^TVkSharingMode;
     TVkSharingMode=
      (
       VK_SHARING_MODE_EXCLUSIVE=0,
       VK_SHARING_MODE_CONCURRENT=1
      );

     PPVkIndexType=^PVkIndexType;
     PVkIndexType=^TVkIndexType;
     TVkIndexType=
      (
       VK_INDEX_TYPE_UINT16=0,
       VK_INDEX_TYPE_UINT32=1
      );

     PPVkFilter=^PVkFilter;
     PVkFilter=^TVkFilter;
     TVkFilter=
      (
       VK_FILTER_NEAREST=0,
       VK_FILTER_LINEAR=1,
       VK_FILTER_CUBIC_IMG=1000015000
      );

     PPVkSamplerMipmapMode=^PVkSamplerMipmapMode;
     PVkSamplerMipmapMode=^TVkSamplerMipmapMode;
     TVkSamplerMipmapMode=
      (
       VK_SAMPLER_MIPMAP_MODE_NEAREST=0,                                         //< Choose nearest mip level
       VK_SAMPLER_MIPMAP_MODE_LINEAR=1                                           //< Linear filter between mip levels
      );

     PPVkSamplerAddressMode=^PVkSamplerAddressMode;
     PVkSamplerAddressMode=^TVkSamplerAddressMode;
     TVkSamplerAddressMode=
      (
       VK_SAMPLER_ADDRESS_MODE_REPEAT=0,
       VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT=1,
       VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE=2,
       VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER=3,
       VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE=4
      );

     PPVkCompareOp=^PVkCompareOp;
     PVkCompareOp=^TVkCompareOp;
     TVkCompareOp=
      (
       VK_COMPARE_OP_NEVER=0,
       VK_COMPARE_OP_LESS=1,
       VK_COMPARE_OP_EQUAL=2,
       VK_COMPARE_OP_LESS_OR_EQUAL=3,
       VK_COMPARE_OP_GREATER=4,
       VK_COMPARE_OP_NOT_EQUAL=5,
       VK_COMPARE_OP_GREATER_OR_EQUAL=6,
       VK_COMPARE_OP_ALWAYS=7
      );

     PPVkPolygonMode=^PVkPolygonMode;
     PVkPolygonMode=^TVkPolygonMode;
     TVkPolygonMode=
      (
       VK_POLYGON_MODE_FILL=0,
       VK_POLYGON_MODE_LINE=1,
       VK_POLYGON_MODE_POINT=2
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
       VK_FRONT_FACE_CLOCKWISE=1
      );

     PPVkBlendFactor=^PVkBlendFactor;
     PVkBlendFactor=^TVkBlendFactor;
     TVkBlendFactor=
      (
       VK_BLEND_FACTOR_ZERO=0,
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
       VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA=18
      );

     PPVkBlendOp=^PVkBlendOp;
     PVkBlendOp=^TVkBlendOp;
     TVkBlendOp=
      (
       VK_BLEND_OP_ADD=0,
       VK_BLEND_OP_SUBTRACT=1,
       VK_BLEND_OP_REVERSE_SUBTRACT=2,
       VK_BLEND_OP_MIN=3,
       VK_BLEND_OP_MAX=4
      );

     PPVkStencilOp=^PVkStencilOp;
     PVkStencilOp=^TVkStencilOp;
     TVkStencilOp=
      (
       VK_STENCIL_OP_KEEP=0,
       VK_STENCIL_OP_ZERO=1,
       VK_STENCIL_OP_REPLACE=2,
       VK_STENCIL_OP_INCREMENT_AND_CLAMP=3,
       VK_STENCIL_OP_DECREMENT_AND_CLAMP=4,
       VK_STENCIL_OP_INVERT=5,
       VK_STENCIL_OP_INCREMENT_AND_WRAP=6,
       VK_STENCIL_OP_DECREMENT_AND_WRAP=7
      );

     PPVkLogicOp=^PVkLogicOp;
     PVkLogicOp=^TVkLogicOp;
     TVkLogicOp=
      (
       VK_LOGIC_OP_CLEAR=0,
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
       VK_LOGIC_OP_SET=15
      );

     PPVkInternalAllocationType=^PVkInternalAllocationType;
     PVkInternalAllocationType=^TVkInternalAllocationType;
     TVkInternalAllocationType=
      (
       VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE=0
      );

     PPVkSystemAllocationScope=^PVkSystemAllocationScope;
     PVkSystemAllocationScope=^TVkSystemAllocationScope;
     TVkSystemAllocationScope=
      (
       VK_SYSTEM_ALLOCATION_SCOPE_COMMAND=0,
       VK_SYSTEM_ALLOCATION_SCOPE_OBJECT=1,
       VK_SYSTEM_ALLOCATION_SCOPE_CACHE=2,
       VK_SYSTEM_ALLOCATION_SCOPE_DEVICE=3,
       VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE=4
      );

     PPVkPhysicalDeviceType=^PVkPhysicalDeviceType;
     PVkPhysicalDeviceType=^TVkPhysicalDeviceType;
     TVkPhysicalDeviceType=
      (
       VK_PHYSICAL_DEVICE_TYPE_OTHER=0,
       VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU=1,
       VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU=2,
       VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU=3,
       VK_PHYSICAL_DEVICE_TYPE_CPU=4
      );

     PPVkVertexInputRate=^PVkVertexInputRate;
     PVkVertexInputRate=^TVkVertexInputRate;
     TVkVertexInputRate=
      (
       VK_VERTEX_INPUT_RATE_VERTEX=0,
       VK_VERTEX_INPUT_RATE_INSTANCE=1
      );

     PPVkFormat=^PVkFormat;
     PVkFormat=^TVkFormat;
     TVkFormat=
      (
       VK_FORMAT_UNDEFINED=0,
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
       VK_FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG=1000054000,
       VK_FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG=1000054001,
       VK_FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG=1000054002,
       VK_FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG=1000054003,
       VK_FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG=1000054004,
       VK_FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG=1000054005,
       VK_FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG=1000054006,
       VK_FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG=1000054007
      );

     PPVkStructureType=^PVkStructureType;
     PVkStructureType=^TVkStructureType;
     TVkStructureType=
      (
       VK_STRUCTURE_TYPE_APPLICATION_INFO=0,
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
       VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT=1000011000,
       VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT=1000011000,
       VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD=1000018000,
       VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT=1000022000,
       VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT=1000022001,
       VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT=1000022002,
       VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV=1000026000,
       VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV=1000026001,
       VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV=1000026002,
       VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV=1000056000,
       VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV=1000056001,
       VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV=1000057000,
       VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV=1000057001,
       VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV=1000058000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR=1000059000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR=1000059001,
       VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR=1000059002,
       VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR=1000059003,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR=1000059004,
       VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR=1000059005,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR=1000059006,
       VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR=1000059007,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR=1000059008,
       VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT=1000061000,
       VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN=1000062000,
       VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX=1000086000,
       VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX=1000086001,
       VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX=1000086002,
       VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX=1000086003,
       VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX=1000086004,
       VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX=1000086005,
       VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT=1000090000,
       VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT=1000091000,
       VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT=1000091001,
       VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT=1000091002,
       VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT=1000091003
      );

     PPVkSubpassContents=^PVkSubpassContents;
     PVkSubpassContents=^TVkSubpassContents;
     TVkSubpassContents=
      (
       VK_SUBPASS_CONTENTS_INLINE=0,
       VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS=1
      );

     PPVkResult=^PVkResult;
     PVkResult=^TVkResult;
     TVkResult=
      (
       VK_ERROR_OUT_OF_POOL_MEMORY_KHR=-1000069000,
       VK_NV_EXTENSION_1_ERROR=-1000013000,
       VK_ERROR_INVALID_SHADER_NV=-1000012000,
       VK_ERROR_VALIDATION_FAILED_EXT=-1000011001,
       VK_ERROR_INCOMPATIBLE_DISPLAY_KHR=-1000003001,
       VK_ERROR_OUT_OF_DATE_KHR=-1000001004,
       VK_ERROR_NATIVE_WINDOW_IN_USE_KHR=-1000000001,
       VK_ERROR_SURFACE_LOST_KHR=-1000000000,
       VK_ERROR_FRAGMENTED_POOL=-12,                                             //< A requested pool allocation has failed due to fragmentation of the pool's memory
       _UNUSED_START=-12,
       VK_ERROR_FORMAT_NOT_SUPPORTED=-11,                                        //< Requested format is not supported on this device
       VK_ERROR_TOO_MANY_OBJECTS=-10,                                            //< Too many objects of the type have already been created
       VK_ERROR_INCOMPATIBLE_DRIVER=-9,                                          //< Unable to find a Vulkan driver
       VK_ERROR_FEATURE_NOT_PRESENT=-8,                                          //< Requested feature is not available on this device
       VK_ERROR_EXTENSION_NOT_PRESENT=-7,                                        //< Extension specified does not exist
       VK_ERROR_LAYER_NOT_PRESENT=-6,                                            //< Layer specified does not exist
       VK_ERROR_MEMORY_MAP_FAILED=-5,                                            //< Mapping of a memory object has failed
       VK_ERROR_DEVICE_LOST=-4,                                                  //< The logical device has been lost. See <<devsandqueues-lost-device>>
       VK_ERROR_INITIALIZATION_FAILED=-3,                                        //< Initialization of a object has failed
       VK_ERROR_OUT_OF_DEVICE_MEMORY=-2,                                         //< A device memory allocation has failed
       VK_ERROR_OUT_OF_HOST_MEMORY=-1,                                           //< A host memory allocation has failed
       VK_SUCCESS=0,                                                             //< Command completed successfully
       VK_NOT_READY=1,                                                           //< A fence or query has not yet completed
       VK_TIMEOUT=2,                                                             //< A wait operation has not completed in the specified time
       VK_EVENT_SET=3,                                                           //< An event is signaled
       VK_EVENT_RESET=4,                                                         //< An event is unsignaled
       VK_INCOMPLETE=5,                                                          //< A return array was too small for the result
       VK_SUBOPTIMAL_KHR=1000001003
      );

     PPVkDynamicState=^PVkDynamicState;
     PVkDynamicState=^TVkDynamicState;
     TVkDynamicState=
      (
       VK_DYNAMIC_STATE_VIEWPORT=0,
       VK_DYNAMIC_STATE_SCISSOR=1,
       VK_DYNAMIC_STATE_LINE_WIDTH=2,
       VK_DYNAMIC_STATE_DEPTH_BIAS=3,
       VK_DYNAMIC_STATE_BLEND_CONSTANTS=4,
       VK_DYNAMIC_STATE_DEPTH_BOUNDS=5,
       VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK=6,
       VK_DYNAMIC_STATE_STENCIL_WRITE_MASK=7,
       VK_DYNAMIC_STATE_STENCIL_REFERENCE=8
      );

     PPVkQueueFlagBits=^PVkQueueFlagBits;
     PVkQueueFlagBits=^TVkQueueFlagBits;
     TVkQueueFlagBits=
      (
       VK_QUEUE_GRAPHICS_BIT=$00000001,                                          //< Queue supports graphics operations
       VK_QUEUE_COMPUTE_BIT=$00000002,                                           //< Queue supports compute operations
       VK_QUEUE_TRANSFER_BIT=$00000004,                                          //< Queue supports transfer operations
       VK_QUEUE_SPARSE_BINDING_BIT=$00000008                                     //< Queue supports sparse resource memory management operations
      );

     PPVkMemoryPropertyFlagBits=^PVkMemoryPropertyFlagBits;
     PVkMemoryPropertyFlagBits=^TVkMemoryPropertyFlagBits;
     TVkMemoryPropertyFlagBits=
      (
       VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT=$00000001,                            //< If otherwise stated, then allocate memory on device
       VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT=$00000002,                            //< Memory is mappable by host
       VK_MEMORY_PROPERTY_HOST_COHERENT_BIT=$00000004,                           //< Memory will have i/o coherency. If not set, application may need to use vkFlushMappedMemoryRanges and vkInvalidateMappedMemoryRanges to flush/invalidate host cache
       VK_MEMORY_PROPERTY_HOST_CACHED_BIT=$00000008,                             //< Memory will be cached by the host
       VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT=$00000010                         //< Memory may be allocated by the driver when it is required
      );

     PPVkMemoryHeapFlagBits=^PVkMemoryHeapFlagBits;
     PVkMemoryHeapFlagBits=^TVkMemoryHeapFlagBits;
     TVkMemoryHeapFlagBits=
      (
       VK_MEMORY_HEAP_DEVICE_LOCAL_BIT=$00000001                                 //< If set, heap represents device memory
      );

     PPVkAccessFlagBits=^PVkAccessFlagBits;
     PVkAccessFlagBits=^TVkAccessFlagBits;
     TVkAccessFlagBits=
      (
       VK_ACCESS_INDIRECT_COMMAND_READ_BIT=$00000001,                            //< Controls coherency of indirect command reads
       VK_ACCESS_INDEX_READ_BIT=$00000002,                                       //< Controls coherency of index reads
       VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT=$00000004,                            //< Controls coherency of vertex attribute reads
       VK_ACCESS_UNIFORM_READ_BIT=$00000008,                                     //< Controls coherency of uniform buffer reads
       VK_ACCESS_INPUT_ATTACHMENT_READ_BIT=$00000010,                            //< Controls coherency of input attachment reads
       VK_ACCESS_SHADER_READ_BIT=$00000020,                                      //< Controls coherency of shader reads
       VK_ACCESS_SHADER_WRITE_BIT=$00000040,                                     //< Controls coherency of shader writes
       VK_ACCESS_COLOR_ATTACHMENT_READ_BIT=$00000080,                            //< Controls coherency of color attachment reads
       VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT=$00000100,                           //< Controls coherency of color attachment writes
       VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT=$00000200,                    //< Controls coherency of depth/stencil attachment reads
       VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT=$00000400,                   //< Controls coherency of depth/stencil attachment writes
       VK_ACCESS_TRANSFER_READ_BIT=$00000800,                                    //< Controls coherency of transfer reads
       VK_ACCESS_TRANSFER_WRITE_BIT=$00001000,                                   //< Controls coherency of transfer writes
       VK_ACCESS_HOST_READ_BIT=$00002000,                                        //< Controls coherency of host reads
       VK_ACCESS_HOST_WRITE_BIT=$00004000,                                       //< Controls coherency of host writes
       VK_ACCESS_MEMORY_READ_BIT=$00008000,                                      //< Controls coherency of memory reads
       VK_ACCESS_MEMORY_WRITE_BIT=$00010000,                                     //< Controls coherency of memory writes
       VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX=$00020000,
       VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX=$00040000
      );

     PPVkBufferUsageFlagBits=^PVkBufferUsageFlagBits;
     PVkBufferUsageFlagBits=^TVkBufferUsageFlagBits;
     TVkBufferUsageFlagBits=
      (
       VK_BUFFER_USAGE_TRANSFER_SRC_BIT=$00000001,                               //< Can be used as a source of transfer operations
       VK_BUFFER_USAGE_TRANSFER_DST_BIT=$00000002,                               //< Can be used as a destination of transfer operations
       VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT=$00000004,                       //< Can be used as TBO
       VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT=$00000008,                       //< Can be used as IBO
       VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT=$00000010,                             //< Can be used as UBO
       VK_BUFFER_USAGE_STORAGE_BUFFER_BIT=$00000020,                             //< Can be used as SSBO
       VK_BUFFER_USAGE_INDEX_BUFFER_BIT=$00000040,                               //< Can be used as source of fixed-function index fetch (index buffer)
       VK_BUFFER_USAGE_VERTEX_BUFFER_BIT=$00000080,                              //< Can be used as source of fixed-function vertex fetch (VBO)
       VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT=$00000100                             //< Can be the source of indirect parameters (e.g. indirect buffer, parameter buffer)
      );

     PPVkBufferCreateFlagBits=^PVkBufferCreateFlagBits;
     PVkBufferCreateFlagBits=^TVkBufferCreateFlagBits;
     TVkBufferCreateFlagBits=
      (
       VK_BUFFER_CREATE_SPARSE_BINDING_BIT=$00000001,                            //< Buffer should support sparse backing
       VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT=$00000002,                          //< Buffer should support sparse backing with partial residency
       VK_BUFFER_CREATE_SPARSE_ALIASED_BIT=$00000004                             //< Buffer should support constent data access to physical memory ranges mapped into multiple locations of sparse buffers
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
       VK_IMAGE_USAGE_TRANSFER_SRC_BIT=$00000001,                                //< Can be used as a source of transfer operations
       VK_IMAGE_USAGE_TRANSFER_DST_BIT=$00000002,                                //< Can be used as a destination of transfer operations
       VK_IMAGE_USAGE_SAMPLED_BIT=$00000004,                                     //< Can be sampled from (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
       VK_IMAGE_USAGE_STORAGE_BIT=$00000008,                                     //< Can be used as storage image (STORAGE_IMAGE descriptor type)
       VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT=$00000010,                            //< Can be used as framebuffer color attachment
       VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT=$00000020,                    //< Can be used as framebuffer depth/stencil attachment
       VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT=$00000040,                        //< Image data not needed outside of rendering
       VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT=$00000080                             //< Can be used as framebuffer input attachment
      );

     PPVkImageCreateFlagBits=^PVkImageCreateFlagBits;
     PVkImageCreateFlagBits=^TVkImageCreateFlagBits;
     TVkImageCreateFlagBits=
      (
       VK_IMAGE_CREATE_SPARSE_BINDING_BIT=$00000001,                             //< Image should support sparse backing
       VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT=$00000002,                           //< Image should support sparse backing with partial residency
       VK_IMAGE_CREATE_SPARSE_ALIASED_BIT=$00000004,                             //< Image should support constent data access to physical memory ranges mapped into multiple locations of sparse images
       VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT=$00000008,                             //< Allows image views to have different format than the base image
       VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT=$00000010,                            //< Allows creating image views with cube type from the created image
       VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR=$00000020
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
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT=$00000001,                            //< Format can be used for sampled images (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
       VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT=$00000002,                            //< Format can be used for storage images (STORAGE_IMAGE descriptor type)
       VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT=$00000004,                     //< Format supports atomic operations in case it is used for storage images
       VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT=$00000008,                     //< Format can be used for uniform texel buffers (TBOs)
       VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT=$00000010,                     //< Format can be used for storage texel buffers (IBOs)
       VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT=$00000020,              //< Format supports atomic operations in case it is used for storage texel buffers
       VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT=$00000040,                            //< Format can be used for vertex buffers (VBOs)
       VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT=$00000080,                         //< Format can be used for color attachment images
       VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT=$00000100,                   //< Format supports blending in case it is used for color attachment images
       VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT=$00000200,                 //< Format can be used for depth/stencil attachment images
       VK_FORMAT_FEATURE_BLIT_SRC_BIT=$00000400,                                 //< Format can be used as the source image of blits with vkCmdBlitImage
       VK_FORMAT_FEATURE_BLIT_DST_BIT=$00000800,                                 //< Format can be used as the destination image of blits with vkCmdBlitImage
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT=$00001000,              //< Format can be filtered with VK_FILTER_LINEAR when being sampled
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG=$00002000,
       VK_FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR=$00004000,
       VK_FORMAT_FEATURE_TRANSFER_DST_BIT_KHR=$00008000
      );

     PPVkQueryControlFlagBits=^PVkQueryControlFlagBits;
     PVkQueryControlFlagBits=^TVkQueryControlFlagBits;
     TVkQueryControlFlagBits=
      (
       VK_QUERY_CONTROL_PRECISE_BIT=$00000001                                    //< Require precise results to be collected by the query
      );

     PPVkQueryResultFlagBits=^PVkQueryResultFlagBits;
     PVkQueryResultFlagBits=^TVkQueryResultFlagBits;
     TVkQueryResultFlagBits=
      (
       VK_QUERY_RESULT_64_BIT=$00000001,                                         //< Results of the queries are written to the destination buffer as 64-bit values
       VK_QUERY_RESULT_WAIT_BIT=$00000002,                                       //< Results of the queries are waited on before proceeding with the result copy
       VK_QUERY_RESULT_WITH_AVAILABILITY_BIT=$00000004,                          //< Besides the results of the query, the availability of the results is also written
       VK_QUERY_RESULT_PARTIAL_BIT=$00000008                                     //< Copy the partial results of the query even if the final results are not available
      );

     PPVkCommandBufferUsageFlagBits=^PVkCommandBufferUsageFlagBits;
     PVkCommandBufferUsageFlagBits=^TVkCommandBufferUsageFlagBits;
     TVkCommandBufferUsageFlagBits=
      (
       VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT=$00000001,
       VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT=$00000002,
       VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT=$00000004                    //< Command buffer may be submitted/executed more than once simultaneously
      );

     PPVkQueryPipelineStatisticFlagBits=^PVkQueryPipelineStatisticFlagBits;
     PVkQueryPipelineStatisticFlagBits=^TVkQueryPipelineStatisticFlagBits;
     TVkQueryPipelineStatisticFlagBits=
      (
       VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT=$00000001,        //< Optional
       VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT=$00000002,      //< Optional
       VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT=$00000004,      //< Optional
       VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT=$00000008,    //< Optional
       VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT=$00000010,     //< Optional
       VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT=$00000020,           //< Optional
       VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT=$00000040,            //< Optional
       VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT=$00000080,    //< Optional
       VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT=$00000100, //< Optional
       VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT=$00000200, //< Optional
       VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT=$00000400      //< Optional
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
       VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT=$00000001,                      //< Image uses a single mip tail region for all array layers
       VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT=$00000002,                    //< Image requires mip level dimensions to be an integer multiple of the sparse image block dimensions for non-tail mip levels.
       VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT=$00000004               //< Image uses a non-standard sparse image block dimensions
      );

     PPVkSparseMemoryBindFlagBits=^PVkSparseMemoryBindFlagBits;
     PVkSparseMemoryBindFlagBits=^TVkSparseMemoryBindFlagBits;
     TVkSparseMemoryBindFlagBits=
      (
       VK_SPARSE_MEMORY_BIND_METADATA_BIT=$00000001                              //< Operation binds resource metadata to memory
      );

     PPVkPipelineStageFlagBits=^PVkPipelineStageFlagBits;
     PVkPipelineStageFlagBits=^TVkPipelineStageFlagBits;
     TVkPipelineStageFlagBits=
      (
       VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT=$00000001,                              //< Before subsequent commands are processed
       VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT=$00000002,                            //< Draw/DispatchIndirect command fetch
       VK_PIPELINE_STAGE_VERTEX_INPUT_BIT=$00000004,                             //< Vertex/index fetch
       VK_PIPELINE_STAGE_VERTEX_SHADER_BIT=$00000008,                            //< Vertex shading
       VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT=$00000010,              //< Tessellation control shading
       VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT=$00000020,           //< Tessellation evaluation shading
       VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT=$00000040,                          //< Geometry shading
       VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT=$00000080,                          //< Fragment shading
       VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT=$00000100,                     //< Early fragment (depth and stencil) tests
       VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT=$00000200,                      //< Late fragment (depth and stencil) tests
       VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT=$00000400,                  //< Color attachment writes
       VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT=$00000800,                           //< Compute shading
       VK_PIPELINE_STAGE_TRANSFER_BIT=$00001000,                                 //< Transfer/copy operations
       VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT=$00002000,                           //< After previous commands have completed
       VK_PIPELINE_STAGE_HOST_BIT=$00004000,                                     //< Indicates host (CPU) is a source/sink of the dependency
       VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT=$00008000,                             //< All stages of the graphics pipeline
       VK_PIPELINE_STAGE_ALL_COMMANDS_BIT=$00010000,                             //< All stages supported on the queue
       VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX=$00020000
      );

     PPVkCommandPoolCreateFlagBits=^PVkCommandPoolCreateFlagBits;
     PVkCommandPoolCreateFlagBits=^TVkCommandPoolCreateFlagBits;
     TVkCommandPoolCreateFlagBits=
      (
       VK_COMMAND_POOL_CREATE_TRANSIENT_BIT=$00000001,                           //< Command buffers have a short lifetime
       VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT=$00000002                 //< Command buffers may release their memory individually
      );

     PPVkCommandPoolResetFlagBits=^PVkCommandPoolResetFlagBits;
     PVkCommandPoolResetFlagBits=^TVkCommandPoolResetFlagBits;
     TVkCommandPoolResetFlagBits=
      (
       VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT=$00000001                     //< Release resources owned by the pool
      );

     PPVkCommandBufferResetFlagBits=^PVkCommandBufferResetFlagBits;
     PVkCommandBufferResetFlagBits=^TVkCommandBufferResetFlagBits;
     TVkCommandBufferResetFlagBits=
      (
       VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT=$00000001                   //< Release resources owned by the buffer
      );

     PPVkSampleCountFlagBits=^PVkSampleCountFlagBits;
     PVkSampleCountFlagBits=^TVkSampleCountFlagBits;
     TVkSampleCountFlagBits=
      (
       VK_SAMPLE_COUNT_1_BIT=$00000001,                                          //< Sample count 1 supported
       VK_SAMPLE_COUNT_2_BIT=$00000002,                                          //< Sample count 2 supported
       VK_SAMPLE_COUNT_4_BIT=$00000004,                                          //< Sample count 4 supported
       VK_SAMPLE_COUNT_8_BIT=$00000008,                                          //< Sample count 8 supported
       VK_SAMPLE_COUNT_16_BIT=$00000010,                                         //< Sample count 16 supported
       VK_SAMPLE_COUNT_32_BIT=$00000020,                                         //< Sample count 32 supported
       VK_SAMPLE_COUNT_64_BIT=$00000040                                          //< Sample count 64 supported
      );

     PPVkAttachmentDescriptionFlagBits=^PVkAttachmentDescriptionFlagBits;
     PVkAttachmentDescriptionFlagBits=^TVkAttachmentDescriptionFlagBits;
     TVkAttachmentDescriptionFlagBits=
      (
       VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT=$00000001                         //< The attachment may alias physical memory of another attachment in the same render pass
      );

     PPVkStencilFaceFlagBits=^PVkStencilFaceFlagBits;
     PVkStencilFaceFlagBits=^TVkStencilFaceFlagBits;
     TVkStencilFaceFlagBits=
      (
       VK_STENCIL_FACE_FRONT_BIT=$00000001,                                      //< Front face
       VK_STENCIL_FRONT_AND_BACK=$00000001,                                      //< Front and back faces
       VK_STENCIL_FACE_BACK_BIT=$00000002                                        //< Back face
      );

     PPVkDescriptorPoolCreateFlagBits=^PVkDescriptorPoolCreateFlagBits;
     PVkDescriptorPoolCreateFlagBits=^TVkDescriptorPoolCreateFlagBits;
     TVkDescriptorPoolCreateFlagBits=
      (
       VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT=$00000001               //< Descriptor sets may be freed individually
      );

     PPVkDependencyFlagBits=^PVkDependencyFlagBits;
     PVkDependencyFlagBits=^TVkDependencyFlagBits;
     TVkDependencyFlagBits=
      (
       VK_DEPENDENCY_BY_REGION_BIT=$00000001                                     //< Dependency is per pixel region 
      );

     PPVkPresentModeKHR=^PVkPresentModeKHR;
     PVkPresentModeKHR=^TVkPresentModeKHR;
     TVkPresentModeKHR=
      (
       VK_PRESENT_MODE_IMMEDIATE_KHR=0,
       VK_PRESENT_MODE_MAILBOX_KHR=1,
       VK_PRESENT_MODE_FIFO_KHR=2,
       VK_PRESENT_MODE_FIFO_RELAXED_KHR=3
      );

     PPVkColorSpaceKHR=^PVkColorSpaceKHR;
     PVkColorSpaceKHR=^TVkColorSpaceKHR;
     TVkColorSpaceKHR=
      (
       VK_COLOR_SPACE_SRGB_NONLINEAR_KHR=0,
       VK_COLORSPACE_SRGB_NONLINEAR_KHR=0,
       VK_COLOR_SPACE_DISPLAY_P3_LINEAR_EXT=1000104001,
       VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT=1000104002,
       VK_COLOR_SPACE_SCRGB_LINEAR_EXT=1000104003,
       VK_COLOR_SPACE_SCRGB_NONLINEAR_EXT=1000104004,
       VK_COLOR_SPACE_DCI_P3_LINEAR_EXT=1000104005,
       VK_COLOR_SPACE_DCI_P3_NONLINEAR_EXT=1000104006,
       VK_COLOR_SPACE_BT709_LINEAR_EXT=1000104007,
       VK_COLOR_SPACE_BT709_NONLINEAR_EXT=1000104008,
       VK_COLOR_SPACE_BT2020_LINEAR_EXT=1000104009,
       VK_COLOR_SPACE_BT2020_NONLINEAR_EXT=1000104010,
       VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT=1000104011,
       VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT=1000104012
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
       VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT=28,
       VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT=29,
       VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT=30,
       VK_DEBUG_REPORT_OBJECT_TYPE_OBJECT_TABLE_NVX_EXT=31,
       VK_DEBUG_REPORT_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX_EXT=32
      );

     PPVkDebugReportErrorEXT=^PVkDebugReportErrorEXT;
     PVkDebugReportErrorEXT=^TVkDebugReportErrorEXT;
     TVkDebugReportErrorEXT=
      (
       VK_DEBUG_REPORT_ERROR_NONE_EXT=0,
       VK_DEBUG_REPORT_ERROR_CALLBACK_REF_EXT=1
      );

     PPVkRasterizationOrderAMD=^PVkRasterizationOrderAMD;
     PVkRasterizationOrderAMD=^TVkRasterizationOrderAMD;
     TVkRasterizationOrderAMD=
      (
       VK_RASTERIZATION_ORDER_STRICT_AMD=0,
       VK_RASTERIZATION_ORDER_RELAXED_AMD=1
      );

     PPVkExternalMemoryHandleTypeFlagBitsNV=^PVkExternalMemoryHandleTypeFlagBitsNV;
     PVkExternalMemoryHandleTypeFlagBitsNV=^TVkExternalMemoryHandleTypeFlagBitsNV;
     TVkExternalMemoryHandleTypeFlagBitsNV=
      (
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV=$00000001,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV=$00000002,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV=$00000004,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV=$00000008
      );

     PPVkExternalMemoryFeatureFlagBitsNV=^PVkExternalMemoryFeatureFlagBitsNV;
     PVkExternalMemoryFeatureFlagBitsNV=^TVkExternalMemoryFeatureFlagBitsNV;
     TVkExternalMemoryFeatureFlagBitsNV=
      (
       VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV=$00000001,
       VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV=$00000002,
       VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV=$00000004
      );

     PPVkValidationCheckEXT=^PVkValidationCheckEXT;
     PVkValidationCheckEXT=^TVkValidationCheckEXT;
     TVkValidationCheckEXT=
      (
       VK_VALIDATION_CHECK_ALL_EXT=0
      );

     PPVkIndirectCommandsLayoutUsageFlagBitsNVX=^PVkIndirectCommandsLayoutUsageFlagBitsNVX;
     PVkIndirectCommandsLayoutUsageFlagBitsNVX=^TVkIndirectCommandsLayoutUsageFlagBitsNVX;
     TVkIndirectCommandsLayoutUsageFlagBitsNVX=
      (
       VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX=$00000001,
       VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX=$00000002,
       VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX=$00000004,
       VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX=$00000008
      );

     PPVkObjectEntryUsageFlagBitsNVX=^PVkObjectEntryUsageFlagBitsNVX;
     PVkObjectEntryUsageFlagBitsNVX=^TVkObjectEntryUsageFlagBitsNVX;
     TVkObjectEntryUsageFlagBitsNVX=
      (
       VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX=$00000001,
       VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX=$00000002
      );

     PPVkIndirectCommandsTokenTypeNVX=^PVkIndirectCommandsTokenTypeNVX;
     PVkIndirectCommandsTokenTypeNVX=^TVkIndirectCommandsTokenTypeNVX;
     TVkIndirectCommandsTokenTypeNVX=
      (
       VK_INDIRECT_COMMANDS_TOKEN_PIPELINE_NVX=0,
       VK_INDIRECT_COMMANDS_TOKEN_DESCRIPTOR_SET_NVX=1,
       VK_INDIRECT_COMMANDS_TOKEN_INDEX_BUFFER_NVX=2,
       VK_INDIRECT_COMMANDS_TOKEN_VERTEX_BUFFER_NVX=3,
       VK_INDIRECT_COMMANDS_TOKEN_PUSH_CONSTANT_NVX=4,
       VK_INDIRECT_COMMANDS_TOKEN_DRAW_INDEXED_NVX=5,
       VK_INDIRECT_COMMANDS_TOKEN_DRAW_NVX=6,
       VK_INDIRECT_COMMANDS_TOKEN_DISPATCH_NVX=7
      );

     PPVkObjectEntryTypeNVX=^PVkObjectEntryTypeNVX;
     PVkObjectEntryTypeNVX=^TVkObjectEntryTypeNVX;
     TVkObjectEntryTypeNVX=
      (
       VK_OBJECT_ENTRY_DESCRIPTOR_SET_NVX=0,
       VK_OBJECT_ENTRY_PIPELINE_NVX=1,
       VK_OBJECT_ENTRY_INDEX_BUFFER_NVX=2,
       VK_OBJECT_ENTRY_VERTEX_BUFFER_NVX=3,
       VK_OBJECT_ENTRY_PUSH_CONSTANT_NVX=4
      );

     PPVkSurfaceCounterFlagBitsEXT=^PVkSurfaceCounterFlagBitsEXT;
     PVkSurfaceCounterFlagBitsEXT=^TVkSurfaceCounterFlagBitsEXT;
     TVkSurfaceCounterFlagBitsEXT=
      (
       VK_SURFACE_COUNTER_VBLANK_EXT=$00000001
      );

     PPVkDisplayPowerStateEXT=^PVkDisplayPowerStateEXT;
     PVkDisplayPowerStateEXT=^TVkDisplayPowerStateEXT;
     TVkDisplayPowerStateEXT=
      (
       VK_DISPLAY_POWER_STATE_OFF_EXT=0,
       VK_DISPLAY_POWER_STATE_SUSPEND_EXT=1,
       VK_DISPLAY_POWER_STATE_ON_EXT=2
      );

     PPVkDeviceEventTypeEXT=^PVkDeviceEventTypeEXT;
     PVkDeviceEventTypeEXT=^TVkDeviceEventTypeEXT;
     TVkDeviceEventTypeEXT=
      (
       VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT=0
      );

     PPVkDisplayEventTypeEXT=^PVkDisplayEventTypeEXT;
     PVkDisplayEventTypeEXT=^TVkDisplayEventTypeEXT;
     TVkDisplayEventTypeEXT=
      (
       VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT=0
      );

     PPPFN_vkInternalAllocationNotification=^PPFN_vkInternalAllocationNotification;
     PPFN_vkInternalAllocationNotification=^TPFN_vkInternalAllocationNotification;
     TPFN_vkInternalAllocationNotification=procedure(pUserData:PVkVoid;size:TVkSize;allocationType:TVkInternalAllocationType;allocationScope:TVkSystemAllocationScope); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     PPPFN_vkInternalFreeNotification=^PPFN_vkInternalFreeNotification;
     PPFN_vkInternalFreeNotification=^TPFN_vkInternalFreeNotification;
     TPFN_vkInternalFreeNotification=procedure(pUserData:PVkVoid;size:TVkSize;allocationType:TVkInternalAllocationType;allocationScope:TVkSystemAllocationScope); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     PPPFN_vkReallocationFunction=^PPFN_vkReallocationFunction;
     PPFN_vkReallocationFunction=^TPFN_vkReallocationFunction;
     TPFN_vkReallocationFunction=function(pUserData:PVkVoid;pOriginal:PVkVoid;size:TVkSize;alignment:TVkSize;allocationScope:TVkSystemAllocationScope):PVkVoid; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     PPPFN_vkAllocationFunction=^PPFN_vkAllocationFunction;
     PPFN_vkAllocationFunction=^TPFN_vkAllocationFunction;
     TPFN_vkAllocationFunction=function(pUserData:PVkVoid;size:TVkSize;alignment:TVkSize;allocationScope:TVkSystemAllocationScope):PVkVoid; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     PPPFN_vkFreeFunction=^PPFN_vkFreeFunction;
     PPFN_vkFreeFunction=^TPFN_vkFreeFunction;
     TPFN_vkFreeFunction=procedure(pUserData:PVkVoid;pMemory:PVkVoid); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     PPPFN_vkVoidFunction=^PPFN_vkVoidFunction;
     PPFN_vkVoidFunction=^TPFN_vkVoidFunction;
     TPFN_vkVoidFunction=procedure(); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     PPPFN_vkDebugReportCallbackEXT=^PPFN_vkDebugReportCallbackEXT;
     PPFN_vkDebugReportCallbackEXT=^TPFN_vkDebugReportCallbackEXT;
     TPFN_vkDebugReportCallbackEXT=function(flags:TVkDebugReportFlagsEXT;objectType:TVkDebugReportObjectTypeEXT;object_:TVkUInt64;location:TVkSize;messageCode:TVkInt32;const pLayerPrefix:PVkChar;const pMessage:PVkChar;pUserData:PVkVoid):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     PPVkOffset2D=^PVkOffset2D;
     PVkOffset2D=^TVkOffset2D;
     TVkOffset2D=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       x:TVkInt32;
       y:TVkInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pX:TVkInt32;
                          const pY:TVkInt32);
{$endif}
     end;

     PPVkOffset3D=^PVkOffset3D;
     PVkOffset3D=^TVkOffset3D;
     TVkOffset3D=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       x:TVkInt32;
       y:TVkInt32;
       z:TVkInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pX:TVkInt32;
                          const pY:TVkInt32;
                          const pZ:TVkInt32);
{$endif}
     end;

     PPVkExtent2D=^PVkExtent2D;
     PVkExtent2D=^TVkExtent2D;
     TVkExtent2D=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       width:TVkUInt32;
       height:TVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pWidth:TVkUInt32;
                          const pHeight:TVkUInt32);
{$endif}
     end;

     PPVkExtent3D=^PVkExtent3D;
     PVkExtent3D=^TVkExtent3D;
     TVkExtent3D=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       width:TVkUInt32;
       height:TVkUInt32;
       depth:TVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pWidth:TVkUInt32;
                          const pHeight:TVkUInt32;
                          const pDepth:TVkUInt32);
{$endif}
     end;

     PPVkViewport=^PVkViewport;
     PVkViewport=^TVkViewport;
     TVkViewport=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       x:TVkFloat;
       y:TVkFloat;
       width:TVkFloat;
       height:TVkFloat;
       minDepth:TVkFloat;
       maxDepth:TVkFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pX:TVkFloat;
                          const pY:TVkFloat;
                          const pWidth:TVkFloat;
                          const pHeight:TVkFloat;
                          const pMinDepth:TVkFloat;
                          const pMaxDepth:TVkFloat);
{$endif}
     end;

     PPVkRect2D=^PVkRect2D;
     PVkRect2D=^TVkRect2D;
     TVkRect2D=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       offset:TVkOffset2D;
       extent:TVkExtent2D;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pOffset:TVkOffset2D;
                          const pExtent:TVkExtent2D);
{$endif}
     end;

     PPVkRect3D=^PVkRect3D;
     PVkRect3D=^TVkRect3D;
     TVkRect3D=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       offset:TVkOffset3D;
       extent:TVkExtent3D;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pOffset:TVkOffset3D;
                          const pExtent:TVkExtent3D);
{$endif}
     end;

     PPVkClearRect=^PVkClearRect;
     PVkClearRect=^TVkClearRect;
     TVkClearRect=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       rect:TVkRect2D;
       baseArrayLayer:TVkUInt32;
       layerCount:TVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pRect:TVkRect2D;
                          const pBaseArrayLayer:TVkUInt32;
                          const pLayerCount:TVkUInt32);
{$endif}
     end;

     PPVkComponentMapping=^PVkComponentMapping;
     PVkComponentMapping=^TVkComponentMapping;
     TVkComponentMapping=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       r:TVkComponentSwizzle;
       g:TVkComponentSwizzle;
       b:TVkComponentSwizzle;
       a:TVkComponentSwizzle;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pR:TVkComponentSwizzle;
                          const pG:TVkComponentSwizzle;
                          const pB:TVkComponentSwizzle;
                          const pA:TVkComponentSwizzle);
{$endif}
     end;

     PPVkPhysicalDeviceSparseProperties=^PVkPhysicalDeviceSparseProperties;
     PVkPhysicalDeviceSparseProperties=^TVkPhysicalDeviceSparseProperties;
     TVkPhysicalDeviceSparseProperties=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       residencyStandard2DBlockShape:TVkBool32; //< Sparse resources support: GPU will access all 2D (single sample) sparse resources using the standard sparse image block shapes (based on pixel format)
       residencyStandard2DMultisampleBlockShape:TVkBool32; //< Sparse resources support: GPU will access all 2D (multisample) sparse resources using the standard sparse image block shapes (based on pixel format)
       residencyStandard3DBlockShape:TVkBool32; //< Sparse resources support: GPU will access all 3D sparse resources using the standard sparse image block shapes (based on pixel format)
       residencyAlignedMipSize:TVkBool32; //< Sparse resources support: Images with mip level dimensions that are NOT a multiple of the sparse image block dimensions will be placed in the mip tail
       residencyNonResidentStrict:TVkBool32; //< Sparse resources support: GPU can consistently access non-resident regions of a resource, all reads return as if data is 0, writes are discarded
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pResidencyStandard2DBlockShape:TVkBool32; //< Sparse resources support: GPU will access all 2D (single sample) sparse resources using the standard sparse image block shapes (based on pixel format)
                          const pResidencyStandard2DMultisampleBlockShape:TVkBool32; //< Sparse resources support: GPU will access all 2D (multisample) sparse resources using the standard sparse image block shapes (based on pixel format)
                          const pResidencyStandard3DBlockShape:TVkBool32; //< Sparse resources support: GPU will access all 3D sparse resources using the standard sparse image block shapes (based on pixel format)
                          const pResidencyAlignedMipSize:TVkBool32; //< Sparse resources support: Images with mip level dimensions that are NOT a multiple of the sparse image block dimensions will be placed in the mip tail
                          const pResidencyNonResidentStrict:TVkBool32); //< Sparse resources support: GPU can consistently access non-resident regions of a resource, all reads return as if data is 0, writes are discarded
{$endif}
     end;

     PPVkExtensionProperties=^PVkExtensionProperties;
     PVkExtensionProperties=^TVkExtensionProperties;
     TVkExtensionProperties=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       extensionName:array[0..VK_MAX_EXTENSION_NAME_SIZE-1] of TVkChar; //< extension name
       specVersion:TVkUInt32; //< version of the extension specification implemented
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pExtensionName:TVkCharString; //< extension name
                          const pSpecVersion:TVkUInt32); //< version of the extension specification implemented
{$endif}
     end;

     PPVkLayerProperties=^PVkLayerProperties;
     PVkLayerProperties=^TVkLayerProperties;
     TVkLayerProperties=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       layerName:array[0..VK_MAX_EXTENSION_NAME_SIZE-1] of TVkChar; //< layer name
       specVersion:TVkUInt32; //< version of the layer specification implemented
       implementationVersion:TVkUInt32; //< build or release version of the layer's library
       description:array[0..VK_MAX_DESCRIPTION_SIZE-1] of TVkChar; //< Free-form description of the layer
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pLayerName:TVkCharString; //< layer name
                          const pSpecVersion:TVkUInt32; //< version of the layer specification implemented
                          const pImplementationVersion:TVkUInt32; //< build or release version of the layer's library
                          const pDescription:TVkCharString); //< Free-form description of the layer
{$endif}
     end;

     PPVkApplicationInfo=^PVkApplicationInfo;
     PVkApplicationInfo=^TVkApplicationInfo;
     TVkApplicationInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_APPLICATION_INFO
       pNext:PVkVoid; //< Pointer to next structure
       pApplicationName:PVkChar;
       applicationVersion:TVkUInt32;
       pEngineName:PVkChar;
       engineVersion:TVkUInt32;
       apiVersion:TVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pPApplicationName:PVkChar;
                          const pApplicationVersion:TVkUInt32;
                          const pPEngineName:PVkChar;
                          const pEngineVersion:TVkUInt32;
                          const pApiVersion:TVkUInt32);
{$endif}
     end;

     PPVkAllocationCallbacks=^PVkAllocationCallbacks;
     PVkAllocationCallbacks=^TVkAllocationCallbacks;
     TVkAllocationCallbacks=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       pUserData:PVkVoid;
       pfnAllocation:TPFN_vkAllocationFunction;
       pfnReallocation:TPFN_vkReallocationFunction;
       pfnFree:TPFN_vkFreeFunction;
       pfnInternalAllocation:TPFN_vkInternalAllocationNotification;
       pfnInternalFree:TPFN_vkInternalFreeNotification;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pPUserData:PVkVoid;
                          const pPfnAllocation:TPFN_vkAllocationFunction;
                          const pPfnReallocation:TPFN_vkReallocationFunction;
                          const pPfnFree:TPFN_vkFreeFunction;
                          const pPfnInternalAllocation:TPFN_vkInternalAllocationNotification;
                          const pPfnInternalFree:TPFN_vkInternalFreeNotification);
{$endif}
     end;

     PPVkDeviceQueueCreateInfo=^PVkDeviceQueueCreateInfo;
     PVkDeviceQueueCreateInfo=^TVkDeviceQueueCreateInfo;
     TVkDeviceQueueCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkDeviceQueueCreateFlags; //< Reserved
       queueFamilyIndex:TVkUInt32;
       queueCount:TVkUInt32;
       pQueuePriorities:PVkFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkDeviceQueueCreateFlags; //< Reserved
                          const pQueueFamilyIndex:TVkUInt32;
                          const pQueueCount:TVkUInt32;
                          const pPQueuePriorities:PVkFloat);
{$endif}
     end;

     PPVkPhysicalDeviceFeatures=^PVkPhysicalDeviceFeatures;
     PVkPhysicalDeviceFeatures=^TVkPhysicalDeviceFeatures;
     TVkPhysicalDeviceFeatures=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       robustBufferAccess:TVkBool32; //< out of bounds buffer accesses are well defined
       fullDrawIndexUint32:TVkBool32; //< full 32-bit range of indices for indexed draw calls
       imageCubeArray:TVkBool32; //< image views which are arrays of cube maps
       independentBlend:TVkBool32; //< blending operations are controlled per-attachment
       geometryShader:TVkBool32; //< geometry stage
       tessellationShader:TVkBool32; //< tessellation control and evaluation stage
       sampleRateShading:TVkBool32; //< per-sample shading and interpolation
       dualSrcBlend:TVkBool32; //< blend operations which take two sources
       logicOp:TVkBool32; //< logic operations
       multiDrawIndirect:TVkBool32; //< multi draw indirect
       drawIndirectFirstInstance:TVkBool32; //< indirect draws can use non-zero firstInstance
       depthClamp:TVkBool32; //< depth clamping
       depthBiasClamp:TVkBool32; //< depth bias clamping
       fillModeNonSolid:TVkBool32; //< point and wireframe fill modes
       depthBounds:TVkBool32; //< depth bounds test
       wideLines:TVkBool32; //< lines with width greater than 1
       largePoints:TVkBool32; //< points with size greater than 1
       alphaToOne:TVkBool32; //< the fragment alpha component can be forced to maximum representable alpha value
       multiViewport:TVkBool32; //< viewport arrays
       samplerAnisotropy:TVkBool32; //< anisotropic sampler filtering
       textureCompressionETC2:TVkBool32; //< ETC texture compression formats
       textureCompressionASTC_LDR:TVkBool32; //< ASTC LDR texture compression formats
       textureCompressionBC:TVkBool32; //< BC1-7 texture compressed formats
       occlusionQueryPrecise:TVkBool32; //< precise occlusion queries returning actual sample counts
       pipelineStatisticsQuery:TVkBool32; //< pipeline statistics query
       vertexPipelineStoresAndAtomics:TVkBool32; //< stores and atomic ops on storage buffers and images are supported in vertex, tessellation, and geometry stages
       fragmentStoresAndAtomics:TVkBool32; //< stores and atomic ops on storage buffers and images are supported in the fragment stage
       shaderTessellationAndGeometryPointSize:TVkBool32; //< tessellation and geometry stages can export point size
       shaderImageGatherExtended:TVkBool32; //< image gather with run-time values and independent offsets
       shaderStorageImageExtendedFormats:TVkBool32; //< the extended set of formats can be used for storage images
       shaderStorageImageMultisample:TVkBool32; //< multisample images can be used for storage images
       shaderStorageImageReadWithoutFormat:TVkBool32; //< read from storage image does not require format qualifier
       shaderStorageImageWriteWithoutFormat:TVkBool32; //< write to storage image does not require format qualifier
       shaderUniformBufferArrayDynamicIndexing:TVkBool32; //< arrays of uniform buffers can be accessed with dynamically uniform indices
       shaderSampledImageArrayDynamicIndexing:TVkBool32; //< arrays of sampled images can be accessed with dynamically uniform indices
       shaderStorageBufferArrayDynamicIndexing:TVkBool32; //< arrays of storage buffers can be accessed with dynamically uniform indices
       shaderStorageImageArrayDynamicIndexing:TVkBool32; //< arrays of storage images can be accessed with dynamically uniform indices
       shaderClipDistance:TVkBool32; //< clip distance in shaders
       shaderCullDistance:TVkBool32; //< cull distance in shaders
       shaderFloat64:TVkBool32; //< 64-bit floats (doubles) in shaders
       shaderInt64:TVkBool32; //< 64-bit integers in shaders
       shaderInt16:TVkBool32; //< 16-bit integers in shaders
       shaderResourceResidency:TVkBool32; //< shader can use texture operations that return resource residency information (requires sparseNonResident support)
       shaderResourceMinLod:TVkBool32; //< shader can use texture operations that specify minimum resource level of detail
       sparseBinding:TVkBool32; //< Sparse resources support: Resource memory can be managed at opaque page level rather than object level
       sparseResidencyBuffer:TVkBool32; //< Sparse resources support: GPU can access partially resident buffers
       sparseResidencyImage2D:TVkBool32; //< Sparse resources support: GPU can access partially resident 2D (non-MSAA non-depth/stencil) images
       sparseResidencyImage3D:TVkBool32; //< Sparse resources support: GPU can access partially resident 3D images
       sparseResidency2Samples:TVkBool32; //< Sparse resources support: GPU can access partially resident MSAA 2D images with 2 samples
       sparseResidency4Samples:TVkBool32; //< Sparse resources support: GPU can access partially resident MSAA 2D images with 4 samples
       sparseResidency8Samples:TVkBool32; //< Sparse resources support: GPU can access partially resident MSAA 2D images with 8 samples
       sparseResidency16Samples:TVkBool32; //< Sparse resources support: GPU can access partially resident MSAA 2D images with 16 samples
       sparseResidencyAliased:TVkBool32; //< Sparse resources support: GPU can correctly access data aliased into multiple locations (opt-in)
       variableMultisampleRate:TVkBool32; //< multisample rate must be the same for all pipelines in a subpass
       inheritedQueries:TVkBool32; //< Queries may be inherited from primary to secondary command buffers
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pRobustBufferAccess:TVkBool32; //< out of bounds buffer accesses are well defined
                          const pFullDrawIndexUint32:TVkBool32; //< full 32-bit range of indices for indexed draw calls
                          const pImageCubeArray:TVkBool32; //< image views which are arrays of cube maps
                          const pIndependentBlend:TVkBool32; //< blending operations are controlled per-attachment
                          const pGeometryShader:TVkBool32; //< geometry stage
                          const pTessellationShader:TVkBool32; //< tessellation control and evaluation stage
                          const pSampleRateShading:TVkBool32; //< per-sample shading and interpolation
                          const pDualSrcBlend:TVkBool32; //< blend operations which take two sources
                          const pLogicOp:TVkBool32; //< logic operations
                          const pMultiDrawIndirect:TVkBool32; //< multi draw indirect
                          const pDrawIndirectFirstInstance:TVkBool32; //< indirect draws can use non-zero firstInstance
                          const pDepthClamp:TVkBool32; //< depth clamping
                          const pDepthBiasClamp:TVkBool32; //< depth bias clamping
                          const pFillModeNonSolid:TVkBool32; //< point and wireframe fill modes
                          const pDepthBounds:TVkBool32; //< depth bounds test
                          const pWideLines:TVkBool32; //< lines with width greater than 1
                          const pLargePoints:TVkBool32; //< points with size greater than 1
                          const pAlphaToOne:TVkBool32; //< the fragment alpha component can be forced to maximum representable alpha value
                          const pMultiViewport:TVkBool32; //< viewport arrays
                          const pSamplerAnisotropy:TVkBool32; //< anisotropic sampler filtering
                          const pTextureCompressionETC2:TVkBool32; //< ETC texture compression formats
                          const pTextureCompressionASTC_LDR:TVkBool32; //< ASTC LDR texture compression formats
                          const pTextureCompressionBC:TVkBool32; //< BC1-7 texture compressed formats
                          const pOcclusionQueryPrecise:TVkBool32; //< precise occlusion queries returning actual sample counts
                          const pPipelineStatisticsQuery:TVkBool32; //< pipeline statistics query
                          const pVertexPipelineStoresAndAtomics:TVkBool32; //< stores and atomic ops on storage buffers and images are supported in vertex, tessellation, and geometry stages
                          const pFragmentStoresAndAtomics:TVkBool32; //< stores and atomic ops on storage buffers and images are supported in the fragment stage
                          const pShaderTessellationAndGeometryPointSize:TVkBool32; //< tessellation and geometry stages can export point size
                          const pShaderImageGatherExtended:TVkBool32; //< image gather with run-time values and independent offsets
                          const pShaderStorageImageExtendedFormats:TVkBool32; //< the extended set of formats can be used for storage images
                          const pShaderStorageImageMultisample:TVkBool32; //< multisample images can be used for storage images
                          const pShaderStorageImageReadWithoutFormat:TVkBool32; //< read from storage image does not require format qualifier
                          const pShaderStorageImageWriteWithoutFormat:TVkBool32; //< write to storage image does not require format qualifier
                          const pShaderUniformBufferArrayDynamicIndexing:TVkBool32; //< arrays of uniform buffers can be accessed with dynamically uniform indices
                          const pShaderSampledImageArrayDynamicIndexing:TVkBool32; //< arrays of sampled images can be accessed with dynamically uniform indices
                          const pShaderStorageBufferArrayDynamicIndexing:TVkBool32; //< arrays of storage buffers can be accessed with dynamically uniform indices
                          const pShaderStorageImageArrayDynamicIndexing:TVkBool32; //< arrays of storage images can be accessed with dynamically uniform indices
                          const pShaderClipDistance:TVkBool32; //< clip distance in shaders
                          const pShaderCullDistance:TVkBool32; //< cull distance in shaders
                          const pShaderFloat64:TVkBool32; //< 64-bit floats (doubles) in shaders
                          const pShaderInt64:TVkBool32; //< 64-bit integers in shaders
                          const pShaderInt16:TVkBool32; //< 16-bit integers in shaders
                          const pShaderResourceResidency:TVkBool32; //< shader can use texture operations that return resource residency information (requires sparseNonResident support)
                          const pShaderResourceMinLod:TVkBool32; //< shader can use texture operations that specify minimum resource level of detail
                          const pSparseBinding:TVkBool32; //< Sparse resources support: Resource memory can be managed at opaque page level rather than object level
                          const pSparseResidencyBuffer:TVkBool32; //< Sparse resources support: GPU can access partially resident buffers
                          const pSparseResidencyImage2D:TVkBool32; //< Sparse resources support: GPU can access partially resident 2D (non-MSAA non-depth/stencil) images
                          const pSparseResidencyImage3D:TVkBool32; //< Sparse resources support: GPU can access partially resident 3D images
                          const pSparseResidency2Samples:TVkBool32; //< Sparse resources support: GPU can access partially resident MSAA 2D images with 2 samples
                          const pSparseResidency4Samples:TVkBool32; //< Sparse resources support: GPU can access partially resident MSAA 2D images with 4 samples
                          const pSparseResidency8Samples:TVkBool32; //< Sparse resources support: GPU can access partially resident MSAA 2D images with 8 samples
                          const pSparseResidency16Samples:TVkBool32; //< Sparse resources support: GPU can access partially resident MSAA 2D images with 16 samples
                          const pSparseResidencyAliased:TVkBool32; //< Sparse resources support: GPU can correctly access data aliased into multiple locations (opt-in)
                          const pVariableMultisampleRate:TVkBool32; //< multisample rate must be the same for all pipelines in a subpass
                          const pInheritedQueries:TVkBool32); //< Queries may be inherited from primary to secondary command buffers
{$endif}
     end;

     PPVkInstanceCreateInfo=^PVkInstanceCreateInfo;
     PVkInstanceCreateInfo=^TVkInstanceCreateInfo;
     TVkInstanceCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkInstanceCreateFlags; //< Reserved
       pApplicationInfo:PVkApplicationInfo;
       enabledLayerCount:TVkUInt32;
       ppEnabledLayerNames:PPVkChar; //< Ordered list of layer names to be enabled
       enabledExtensionCount:TVkUInt32;
       ppEnabledExtensionNames:PPVkChar; //< Extension names to be enabled
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkInstanceCreateFlags; //< Reserved
                          const pPApplicationInfo:PVkApplicationInfo;
                          const pEnabledLayerCount:TVkUInt32;
                          const pPpEnabledLayerNames:PPVkChar; //< Ordered list of layer names to be enabled
                          const pEnabledExtensionCount:TVkUInt32;
                          const pPpEnabledExtensionNames:PPVkChar); //< Extension names to be enabled
{$endif}
     end;

     PPVkQueueFamilyProperties=^PVkQueueFamilyProperties;
     PVkQueueFamilyProperties=^TVkQueueFamilyProperties;
     TVkQueueFamilyProperties=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       queueFlags:TVkQueueFlags; //< Queue flags
       queueCount:TVkUInt32;
       timestampValidBits:TVkUInt32;
       minImageTransferGranularity:TVkExtent3D; //< Minimum alignment requirement for image transfers
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pQueueFlags:TVkQueueFlags; //< Queue flags
                          const pQueueCount:TVkUInt32;
                          const pTimestampValidBits:TVkUInt32;
                          const pMinImageTransferGranularity:TVkExtent3D); //< Minimum alignment requirement for image transfers
{$endif}
     end;

     PPVkMemoryType=^PVkMemoryType;
     PVkMemoryType=^TVkMemoryType;
     TVkMemoryType=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       propertyFlags:TVkMemoryPropertyFlags; //< Memory properties of this memory type
       heapIndex:TVkUInt32; //< Index of the memory heap allocations of this memory type are taken from
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pPropertyFlags:TVkMemoryPropertyFlags; //< Memory properties of this memory type
                          const pHeapIndex:TVkUInt32); //< Index of the memory heap allocations of this memory type are taken from
{$endif}
     end;

     PPVkMemoryAllocateInfo=^PVkMemoryAllocateInfo;
     PVkMemoryAllocateInfo=^TVkMemoryAllocateInfo;
     TVkMemoryAllocateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       allocationSize:TVkDeviceSize; //< Size of memory allocation
       memoryTypeIndex:TVkUInt32; //< Index of the of the memory type to allocate from
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pAllocationSize:TVkDeviceSize; //< Size of memory allocation
                          const pMemoryTypeIndex:TVkUInt32); //< Index of the of the memory type to allocate from
{$endif}
     end;

     PPVkMemoryRequirements=^PVkMemoryRequirements;
     PVkMemoryRequirements=^TVkMemoryRequirements;
     TVkMemoryRequirements=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       size:TVkDeviceSize; //< Specified in bytes
       alignment:TVkDeviceSize; //< Specified in bytes
       memoryTypeBits:TVkUInt32; //< Bitmask of the allowed memory type indices into memoryTypes[] for this object
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pSize:TVkDeviceSize; //< Specified in bytes
                          const pAlignment:TVkDeviceSize; //< Specified in bytes
                          const pMemoryTypeBits:TVkUInt32); //< Bitmask of the allowed memory type indices into memoryTypes[] for this object
{$endif}
     end;

     PPVkSparseImageFormatProperties=^PVkSparseImageFormatProperties;
     PVkSparseImageFormatProperties=^TVkSparseImageFormatProperties;
     TVkSparseImageFormatProperties=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       aspectMask:TVkImageAspectFlags;
       imageGranularity:TVkExtent3D;
       flags:TVkSparseImageFormatFlags;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pAspectMask:TVkImageAspectFlags;
                          const pImageGranularity:TVkExtent3D;
                          const pFlags:TVkSparseImageFormatFlags);
{$endif}
     end;

     PPVkSparseImageMemoryRequirements=^PVkSparseImageMemoryRequirements;
     PVkSparseImageMemoryRequirements=^TVkSparseImageMemoryRequirements;
     TVkSparseImageMemoryRequirements=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       formatProperties:TVkSparseImageFormatProperties;
       imageMipTailFirstLod:TVkUInt32;
       imageMipTailSize:TVkDeviceSize; //< Specified in bytes, must be a multiple of sparse block size in bytes / alignment
       imageMipTailOffset:TVkDeviceSize; //< Specified in bytes, must be a multiple of sparse block size in bytes / alignment
       imageMipTailStride:TVkDeviceSize; //< Specified in bytes, must be a multiple of sparse block size in bytes / alignment
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFormatProperties:TVkSparseImageFormatProperties;
                          const pImageMipTailFirstLod:TVkUInt32;
                          const pImageMipTailSize:TVkDeviceSize; //< Specified in bytes, must be a multiple of sparse block size in bytes / alignment
                          const pImageMipTailOffset:TVkDeviceSize; //< Specified in bytes, must be a multiple of sparse block size in bytes / alignment
                          const pImageMipTailStride:TVkDeviceSize); //< Specified in bytes, must be a multiple of sparse block size in bytes / alignment
{$endif}
     end;

     PPVkMemoryHeap=^PVkMemoryHeap;
     PVkMemoryHeap=^TVkMemoryHeap;
     TVkMemoryHeap=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       size:TVkDeviceSize; //< Available memory in the heap
       flags:TVkMemoryHeapFlags; //< Flags for the heap
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pSize:TVkDeviceSize; //< Available memory in the heap
                          const pFlags:TVkMemoryHeapFlags); //< Flags for the heap
{$endif}
     end;

     PPVkPhysicalDeviceMemoryProperties=^PVkPhysicalDeviceMemoryProperties;
     PVkPhysicalDeviceMemoryProperties=^TVkPhysicalDeviceMemoryProperties;
     TVkPhysicalDeviceMemoryProperties=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       memoryTypeCount:TVkUInt32;
       memoryTypes:array[0..VK_MAX_MEMORY_TYPES-1] of TVkMemoryType;
       memoryHeapCount:TVkUInt32;
       memoryHeaps:array[0..VK_MAX_MEMORY_HEAPS-1] of TVkMemoryHeap;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pMemoryTypeCount:TVkUInt32;
                          const pMemoryTypes:array of TVkMemoryType;
                          const pMemoryHeapCount:TVkUInt32;
                          const pMemoryHeaps:array of TVkMemoryHeap);
{$endif}
     end;

     PPVkMappedMemoryRange=^PVkMappedMemoryRange;
     PVkMappedMemoryRange=^TVkMappedMemoryRange;
     TVkMappedMemoryRange=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
       pNext:PVkVoid; //< Pointer to next structure
       memory:TVkDeviceMemory; //< Mapped memory object
       offset:TVkDeviceSize; //< Offset within the memory object where the range starts
       size:TVkDeviceSize; //< Size of the range within the memory object
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pMemory:TVkDeviceMemory; //< Mapped memory object
                          const pOffset:TVkDeviceSize; //< Offset within the memory object where the range starts
                          const pSize:TVkDeviceSize); //< Size of the range within the memory object
{$endif}
     end;

     PPVkFormatProperties=^PVkFormatProperties;
     PVkFormatProperties=^TVkFormatProperties;
     TVkFormatProperties=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       linearTilingFeatures:TVkFormatFeatureFlags; //< Format features in case of linear tiling
       optimalTilingFeatures:TVkFormatFeatureFlags; //< Format features in case of optimal tiling
       bufferFeatures:TVkFormatFeatureFlags; //< Format features supported by buffers
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pLinearTilingFeatures:TVkFormatFeatureFlags; //< Format features in case of linear tiling
                          const pOptimalTilingFeatures:TVkFormatFeatureFlags; //< Format features in case of optimal tiling
                          const pBufferFeatures:TVkFormatFeatureFlags); //< Format features supported by buffers
{$endif}
     end;

     PPVkImageFormatProperties=^PVkImageFormatProperties;
     PVkImageFormatProperties=^TVkImageFormatProperties;
     TVkImageFormatProperties=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       maxExtent:TVkExtent3D; //< max image dimensions for this resource type
       maxMipLevels:TVkUInt32; //< max number of mipmap levels for this resource type
       maxArrayLayers:TVkUInt32; //< max array size for this resource type
       sampleCounts:TVkSampleCountFlags; //< supported sample counts for this resource type
       maxResourceSize:TVkDeviceSize; //< max size (in bytes) of this resource type
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pMaxExtent:TVkExtent3D; //< max image dimensions for this resource type
                          const pMaxMipLevels:TVkUInt32; //< max number of mipmap levels for this resource type
                          const pMaxArrayLayers:TVkUInt32; //< max array size for this resource type
                          const pSampleCounts:TVkSampleCountFlags; //< supported sample counts for this resource type
                          const pMaxResourceSize:TVkDeviceSize); //< max size (in bytes) of this resource type
{$endif}
     end;

     PPVkDescriptorBufferInfo=^PVkDescriptorBufferInfo;
     PVkDescriptorBufferInfo=^TVkDescriptorBufferInfo;
     TVkDescriptorBufferInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       buffer:TVkBuffer; //< Buffer used for this descriptor slot when the descriptor is UNIFORM_BUFFER[_DYNAMIC] or STORAGE_BUFFER[_DYNAMIC]. VK_NULL_HANDLE otherwise.
       offset:TVkDeviceSize; //< Base offset from buffer start in bytes to update in the descriptor set.
       range:TVkDeviceSize; //< Size in bytes of the buffer resource for this descriptor update.
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pBuffer:TVkBuffer; //< Buffer used for this descriptor slot when the descriptor is UNIFORM_BUFFER[_DYNAMIC] or STORAGE_BUFFER[_DYNAMIC]. VK_NULL_HANDLE otherwise.
                          const pOffset:TVkDeviceSize; //< Base offset from buffer start in bytes to update in the descriptor set.
                          const pRange:TVkDeviceSize); //< Size in bytes of the buffer resource for this descriptor update.
{$endif}
     end;

     PPVkDescriptorImageInfo=^PVkDescriptorImageInfo;
     PVkDescriptorImageInfo=^TVkDescriptorImageInfo;
     TVkDescriptorImageInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sampler:TVkSampler; //< Sampler to write to the descriptor in case it is a SAMPLER or COMBINED_IMAGE_SAMPLER descriptor. Ignored otherwise.
       imageView:TVkImageView; //< Image view to write to the descriptor in case it is a SAMPLED_IMAGE, STORAGE_IMAGE, COMBINED_IMAGE_SAMPLER, or INPUT_ATTACHMENT descriptor. Ignored otherwise.
       imageLayout:TVkImageLayout; //< Layout the image is expected to be in when accessed using this descriptor (only used if imageView is not VK_NULL_HANDLE).
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pSampler:TVkSampler; //< Sampler to write to the descriptor in case it is a SAMPLER or COMBINED_IMAGE_SAMPLER descriptor. Ignored otherwise.
                          const pImageView:TVkImageView; //< Image view to write to the descriptor in case it is a SAMPLED_IMAGE, STORAGE_IMAGE, COMBINED_IMAGE_SAMPLER, or INPUT_ATTACHMENT descriptor. Ignored otherwise.
                          const pImageLayout:TVkImageLayout); //< Layout the image is expected to be in when accessed using this descriptor (only used if imageView is not VK_NULL_HANDLE).
{$endif}
     end;

     PPVkWriteDescriptorSet=^PVkWriteDescriptorSet;
     PVkWriteDescriptorSet=^TVkWriteDescriptorSet;
     TVkWriteDescriptorSet=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
       pNext:PVkVoid; //< Pointer to next structure
       dstSet:TVkDescriptorSet; //< Destination descriptor set
       dstBinding:TVkUInt32; //< Binding within the destination descriptor set to write
       dstArrayElement:TVkUInt32; //< Array element within the destination binding to write
       descriptorCount:TVkUInt32; //< Number of descriptors to write (determines the size of the array pointed by pDescriptors)
       descriptorType:TVkDescriptorType; //< Descriptor type to write (determines which members of the array pointed by pDescriptors are going to be used)
       pImageInfo:PVkDescriptorImageInfo; //< Sampler, image view, and layout for SAMPLER, COMBINED_IMAGE_SAMPLER, {SAMPLED,STORAGE}_IMAGE, and INPUT_ATTACHMENT descriptor types.
       pBufferInfo:PVkDescriptorBufferInfo; //< Raw buffer, size, and offset for {UNIFORM,STORAGE}_BUFFER[_DYNAMIC] descriptor types.
       pTexelBufferView:PVkBufferView; //< Buffer view to write to the descriptor for {UNIFORM,STORAGE}_TEXEL_BUFFER descriptor types.
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pDstSet:TVkDescriptorSet; //< Destination descriptor set
                          const pDstBinding:TVkUInt32; //< Binding within the destination descriptor set to write
                          const pDstArrayElement:TVkUInt32; //< Array element within the destination binding to write
                          const pDescriptorCount:TVkUInt32; //< Number of descriptors to write (determines the size of the array pointed by pDescriptors)
                          const pDescriptorType:TVkDescriptorType; //< Descriptor type to write (determines which members of the array pointed by pDescriptors are going to be used)
                          const pPImageInfo:PVkDescriptorImageInfo; //< Sampler, image view, and layout for SAMPLER, COMBINED_IMAGE_SAMPLER, {SAMPLED,STORAGE}_IMAGE, and INPUT_ATTACHMENT descriptor types.
                          const pPBufferInfo:PVkDescriptorBufferInfo; //< Raw buffer, size, and offset for {UNIFORM,STORAGE}_BUFFER[_DYNAMIC] descriptor types.
                          const pPTexelBufferView:PVkBufferView); //< Buffer view to write to the descriptor for {UNIFORM,STORAGE}_TEXEL_BUFFER descriptor types.
{$endif}
     end;

     PPVkCopyDescriptorSet=^PVkCopyDescriptorSet;
     PVkCopyDescriptorSet=^TVkCopyDescriptorSet;
     TVkCopyDescriptorSet=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET
       pNext:PVkVoid; //< Pointer to next structure
       srcSet:TVkDescriptorSet; //< Source descriptor set
       srcBinding:TVkUInt32; //< Binding within the source descriptor set to copy from
       srcArrayElement:TVkUInt32; //< Array element within the source binding to copy from
       dstSet:TVkDescriptorSet; //< Destination descriptor set
       dstBinding:TVkUInt32; //< Binding within the destination descriptor set to copy to
       dstArrayElement:TVkUInt32; //< Array element within the destination binding to copy to
       descriptorCount:TVkUInt32; //< Number of descriptors to write (determines the size of the array pointed by pDescriptors)
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pSrcSet:TVkDescriptorSet; //< Source descriptor set
                          const pSrcBinding:TVkUInt32; //< Binding within the source descriptor set to copy from
                          const pSrcArrayElement:TVkUInt32; //< Array element within the source binding to copy from
                          const pDstSet:TVkDescriptorSet; //< Destination descriptor set
                          const pDstBinding:TVkUInt32; //< Binding within the destination descriptor set to copy to
                          const pDstArrayElement:TVkUInt32; //< Array element within the destination binding to copy to
                          const pDescriptorCount:TVkUInt32); //< Number of descriptors to write (determines the size of the array pointed by pDescriptors)
{$endif}
     end;

     PPVkBufferCreateInfo=^PVkBufferCreateInfo;
     PVkBufferCreateInfo=^TVkBufferCreateInfo;
     TVkBufferCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure.
       flags:TVkBufferCreateFlags; //< Buffer creation flags
       size:TVkDeviceSize; //< Specified in bytes
       usage:TVkBufferUsageFlags; //< Buffer usage flags
       sharingMode:TVkSharingMode;
       queueFamilyIndexCount:TVkUInt32;
       pQueueFamilyIndices:PVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkBufferCreateFlags; //< Buffer creation flags
                          const pSize:TVkDeviceSize; //< Specified in bytes
                          const pUsage:TVkBufferUsageFlags; //< Buffer usage flags
                          const pSharingMode:TVkSharingMode;
                          const pQueueFamilyIndexCount:TVkUInt32;
                          const pPQueueFamilyIndices:PVkUInt32);
{$endif}
     end;

     PPVkBufferViewCreateInfo=^PVkBufferViewCreateInfo;
     PVkBufferViewCreateInfo=^TVkBufferViewCreateInfo;
     TVkBufferViewCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure.
       flags:TVkBufferViewCreateFlags; //< Reserved
       buffer:TVkBuffer;
       format:TVkFormat; //< Optionally specifies format of elements
       offset:TVkDeviceSize; //< Specified in bytes
       range:TVkDeviceSize; //< View size specified in bytes
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkBufferViewCreateFlags; //< Reserved
                          const pBuffer:TVkBuffer;
                          const pFormat:TVkFormat; //< Optionally specifies format of elements
                          const pOffset:TVkDeviceSize; //< Specified in bytes
                          const pRange:TVkDeviceSize); //< View size specified in bytes
{$endif}
     end;

     PPVkImageSubresource=^PVkImageSubresource;
     PVkImageSubresource=^TVkImageSubresource;
     TVkImageSubresource=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       aspectMask:TVkImageAspectFlags;
       mipLevel:TVkUInt32;
       arrayLayer:TVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pAspectMask:TVkImageAspectFlags;
                          const pMipLevel:TVkUInt32;
                          const pArrayLayer:TVkUInt32);
{$endif}
     end;

     PPVkImageSubresourceLayers=^PVkImageSubresourceLayers;
     PVkImageSubresourceLayers=^TVkImageSubresourceLayers;
     TVkImageSubresourceLayers=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       aspectMask:TVkImageAspectFlags;
       mipLevel:TVkUInt32;
       baseArrayLayer:TVkUInt32;
       layerCount:TVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pAspectMask:TVkImageAspectFlags;
                          const pMipLevel:TVkUInt32;
                          const pBaseArrayLayer:TVkUInt32;
                          const pLayerCount:TVkUInt32);
{$endif}
     end;

     PPVkImageSubresourceRange=^PVkImageSubresourceRange;
     PVkImageSubresourceRange=^TVkImageSubresourceRange;
     TVkImageSubresourceRange=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       aspectMask:TVkImageAspectFlags;
       baseMipLevel:TVkUInt32;
       levelCount:TVkUInt32;
       baseArrayLayer:TVkUInt32;
       layerCount:TVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pAspectMask:TVkImageAspectFlags;
                          const pBaseMipLevel:TVkUInt32;
                          const pLevelCount:TVkUInt32;
                          const pBaseArrayLayer:TVkUInt32;
                          const pLayerCount:TVkUInt32);
{$endif}
     end;

     PPVkMemoryBarrier=^PVkMemoryBarrier;
     PVkMemoryBarrier=^TVkMemoryBarrier;
     TVkMemoryBarrier=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_MEMORY_BARRIER
       pNext:PVkVoid; //< Pointer to next structure.
       srcAccessMask:TVkAccessFlags; //< Memory accesses from the source of the dependency to synchronize
       dstAccessMask:TVkAccessFlags; //< Memory accesses from the destination of the dependency to synchronize
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pSrcAccessMask:TVkAccessFlags; //< Memory accesses from the source of the dependency to synchronize
                          const pDstAccessMask:TVkAccessFlags); //< Memory accesses from the destination of the dependency to synchronize
{$endif}
     end;

     PPVkBufferMemoryBarrier=^PVkBufferMemoryBarrier;
     PVkBufferMemoryBarrier=^TVkBufferMemoryBarrier;
     TVkBufferMemoryBarrier=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER
       pNext:PVkVoid; //< Pointer to next structure.
       srcAccessMask:TVkAccessFlags; //< Memory accesses from the source of the dependency to synchronize
       dstAccessMask:TVkAccessFlags; //< Memory accesses from the destination of the dependency to synchronize
       srcQueueFamilyIndex:TVkUInt32; //< Queue family to transition ownership from
       dstQueueFamilyIndex:TVkUInt32; //< Queue family to transition ownership to
       buffer:TVkBuffer; //< Buffer to sync
       offset:TVkDeviceSize; //< Offset within the buffer to sync
       size:TVkDeviceSize; //< Amount of bytes to sync
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pSrcAccessMask:TVkAccessFlags; //< Memory accesses from the source of the dependency to synchronize
                          const pDstAccessMask:TVkAccessFlags; //< Memory accesses from the destination of the dependency to synchronize
                          const pSrcQueueFamilyIndex:TVkUInt32; //< Queue family to transition ownership from
                          const pDstQueueFamilyIndex:TVkUInt32; //< Queue family to transition ownership to
                          const pBuffer:TVkBuffer; //< Buffer to sync
                          const pOffset:TVkDeviceSize; //< Offset within the buffer to sync
                          const pSize:TVkDeviceSize); //< Amount of bytes to sync
{$endif}
     end;

     PPVkImageMemoryBarrier=^PVkImageMemoryBarrier;
     PVkImageMemoryBarrier=^TVkImageMemoryBarrier;
     TVkImageMemoryBarrier=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
       pNext:PVkVoid; //< Pointer to next structure.
       srcAccessMask:TVkAccessFlags; //< Memory accesses from the source of the dependency to synchronize
       dstAccessMask:TVkAccessFlags; //< Memory accesses from the destination of the dependency to synchronize
       oldLayout:TVkImageLayout; //< Current layout of the image
       newLayout:TVkImageLayout; //< New layout to transition the image to
       srcQueueFamilyIndex:TVkUInt32; //< Queue family to transition ownership from
       dstQueueFamilyIndex:TVkUInt32; //< Queue family to transition ownership to
       image:TVkImage; //< Image to sync
       subresourceRange:TVkImageSubresourceRange; //< Subresource range to sync
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pSrcAccessMask:TVkAccessFlags; //< Memory accesses from the source of the dependency to synchronize
                          const pDstAccessMask:TVkAccessFlags; //< Memory accesses from the destination of the dependency to synchronize
                          const pOldLayout:TVkImageLayout; //< Current layout of the image
                          const pNewLayout:TVkImageLayout; //< New layout to transition the image to
                          const pSrcQueueFamilyIndex:TVkUInt32; //< Queue family to transition ownership from
                          const pDstQueueFamilyIndex:TVkUInt32; //< Queue family to transition ownership to
                          const pImage:TVkImage; //< Image to sync
                          const pSubresourceRange:TVkImageSubresourceRange); //< Subresource range to sync
{$endif}
     end;

     PPVkImageCreateInfo=^PVkImageCreateInfo;
     PVkImageCreateInfo=^TVkImageCreateInfo;
     TVkImageCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure.
       flags:TVkImageCreateFlags; //< Image creation flags
       imageType:TVkImageType;
       format:TVkFormat;
       extent:TVkExtent3D;
       mipLevels:TVkUInt32;
       arrayLayers:TVkUInt32;
       samples:TVkSampleCountFlagBits;
       tiling:TVkImageTiling;
       usage:TVkImageUsageFlags; //< Image usage flags
       sharingMode:TVkSharingMode; //< Cross-queue-family sharing mode
       queueFamilyIndexCount:TVkUInt32; //< Number of queue families to share across
       pQueueFamilyIndices:PVkUInt32; //< Array of queue family indices to share across
       initialLayout:TVkImageLayout; //< Initial image layout for all subresources
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkImageCreateFlags; //< Image creation flags
                          const pImageType:TVkImageType;
                          const pFormat:TVkFormat;
                          const pExtent:TVkExtent3D;
                          const pMipLevels:TVkUInt32;
                          const pArrayLayers:TVkUInt32;
                          const pSamples:TVkSampleCountFlagBits;
                          const pTiling:TVkImageTiling;
                          const pUsage:TVkImageUsageFlags; //< Image usage flags
                          const pSharingMode:TVkSharingMode; //< Cross-queue-family sharing mode
                          const pQueueFamilyIndexCount:TVkUInt32; //< Number of queue families to share across
                          const pPQueueFamilyIndices:PVkUInt32; //< Array of queue family indices to share across
                          const pInitialLayout:TVkImageLayout); //< Initial image layout for all subresources
{$endif}
     end;

     PPVkSubresourceLayout=^PVkSubresourceLayout;
     PVkSubresourceLayout=^TVkSubresourceLayout;
     TVkSubresourceLayout=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       offset:TVkDeviceSize; //< Specified in bytes
       size:TVkDeviceSize; //< Specified in bytes
       rowPitch:TVkDeviceSize; //< Specified in bytes
       arrayPitch:TVkDeviceSize; //< Specified in bytes
       depthPitch:TVkDeviceSize; //< Specified in bytes
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pOffset:TVkDeviceSize; //< Specified in bytes
                          const pSize:TVkDeviceSize; //< Specified in bytes
                          const pRowPitch:TVkDeviceSize; //< Specified in bytes
                          const pArrayPitch:TVkDeviceSize; //< Specified in bytes
                          const pDepthPitch:TVkDeviceSize); //< Specified in bytes
{$endif}
     end;

     PPVkImageViewCreateInfo=^PVkImageViewCreateInfo;
     PVkImageViewCreateInfo=^TVkImageViewCreateInfo;
     TVkImageViewCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkImageViewCreateFlags; //< Reserved
       image:TVkImage;
       viewType:TVkImageViewType;
       format:TVkFormat;
       components:TVkComponentMapping;
       subresourceRange:TVkImageSubresourceRange;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkImageViewCreateFlags; //< Reserved
                          const pImage:TVkImage;
                          const pViewType:TVkImageViewType;
                          const pFormat:TVkFormat;
                          const pComponents:TVkComponentMapping;
                          const pSubresourceRange:TVkImageSubresourceRange);
{$endif}
     end;

     PPVkBufferCopy=^PVkBufferCopy;
     PVkBufferCopy=^TVkBufferCopy;
     TVkBufferCopy=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       srcOffset:TVkDeviceSize; //< Specified in bytes
       dstOffset:TVkDeviceSize; //< Specified in bytes
       size:TVkDeviceSize; //< Specified in bytes
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pSrcOffset:TVkDeviceSize; //< Specified in bytes
                          const pDstOffset:TVkDeviceSize; //< Specified in bytes
                          const pSize:TVkDeviceSize); //< Specified in bytes
{$endif}
     end;

     PPVkSparseMemoryBind=^PVkSparseMemoryBind;
     PVkSparseMemoryBind=^TVkSparseMemoryBind;
     TVkSparseMemoryBind=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       resourceOffset:TVkDeviceSize; //< Specified in bytes
       size:TVkDeviceSize; //< Specified in bytes
       memory:TVkDeviceMemory;
       memoryOffset:TVkDeviceSize; //< Specified in bytes
       flags:TVkSparseMemoryBindFlags; //< Reserved for future
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pResourceOffset:TVkDeviceSize; //< Specified in bytes
                          const pSize:TVkDeviceSize; //< Specified in bytes
                          const pMemory:TVkDeviceMemory;
                          const pMemoryOffset:TVkDeviceSize; //< Specified in bytes
                          const pFlags:TVkSparseMemoryBindFlags); //< Reserved for future
{$endif}
     end;

     PPVkSparseImageMemoryBind=^PVkSparseImageMemoryBind;
     PVkSparseImageMemoryBind=^TVkSparseImageMemoryBind;
     TVkSparseImageMemoryBind=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       subresource:TVkImageSubresource;
       offset:TVkOffset3D;
       extent:TVkExtent3D;
       memory:TVkDeviceMemory;
       memoryOffset:TVkDeviceSize; //< Specified in bytes
       flags:TVkSparseMemoryBindFlags; //< Reserved for future
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pSubresource:TVkImageSubresource;
                          const pOffset:TVkOffset3D;
                          const pExtent:TVkExtent3D;
                          const pMemory:TVkDeviceMemory;
                          const pMemoryOffset:TVkDeviceSize; //< Specified in bytes
                          const pFlags:TVkSparseMemoryBindFlags); //< Reserved for future
{$endif}
     end;

     PPVkSparseBufferMemoryBindInfo=^PVkSparseBufferMemoryBindInfo;
     PVkSparseBufferMemoryBindInfo=^TVkSparseBufferMemoryBindInfo;
     TVkSparseBufferMemoryBindInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       buffer:TVkBuffer;
       bindCount:TVkUInt32;
       pBinds:PVkSparseMemoryBind;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pBuffer:TVkBuffer;
                          const pBindCount:TVkUInt32;
                          const pPBinds:PVkSparseMemoryBind);
{$endif}
     end;

     PPVkSparseImageOpaqueMemoryBindInfo=^PVkSparseImageOpaqueMemoryBindInfo;
     PVkSparseImageOpaqueMemoryBindInfo=^TVkSparseImageOpaqueMemoryBindInfo;
     TVkSparseImageOpaqueMemoryBindInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       image:TVkImage;
       bindCount:TVkUInt32;
       pBinds:PVkSparseMemoryBind;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pImage:TVkImage;
                          const pBindCount:TVkUInt32;
                          const pPBinds:PVkSparseMemoryBind);
{$endif}
     end;

     PPVkSparseImageMemoryBindInfo=^PVkSparseImageMemoryBindInfo;
     PVkSparseImageMemoryBindInfo=^TVkSparseImageMemoryBindInfo;
     TVkSparseImageMemoryBindInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       image:TVkImage;
       bindCount:TVkUInt32;
       pBinds:PVkSparseImageMemoryBind;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pImage:TVkImage;
                          const pBindCount:TVkUInt32;
                          const pPBinds:PVkSparseImageMemoryBind);
{$endif}
     end;

     PPVkBindSparseInfo=^PVkBindSparseInfo;
     PVkBindSparseInfo=^TVkBindSparseInfo;
     TVkBindSparseInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_BIND_SPARSE_INFO
       pNext:PVkVoid; //< Pointer to next structure.
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
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pWaitSemaphoreCount:TVkUInt32;
                          const pPWaitSemaphores:PVkSemaphore;
                          const pBufferBindCount:TVkUInt32;
                          const pPBufferBinds:PVkSparseBufferMemoryBindInfo;
                          const pImageOpaqueBindCount:TVkUInt32;
                          const pPImageOpaqueBinds:PVkSparseImageOpaqueMemoryBindInfo;
                          const pImageBindCount:TVkUInt32;
                          const pPImageBinds:PVkSparseImageMemoryBindInfo;
                          const pSignalSemaphoreCount:TVkUInt32;
                          const pPSignalSemaphores:PVkSemaphore);
{$endif}
     end;

     PPVkImageCopy=^PVkImageCopy;
     PVkImageCopy=^TVkImageCopy;
     TVkImageCopy=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       srcSubresource:TVkImageSubresourceLayers;
       srcOffset:TVkOffset3D; //< Specified in pixels for both compressed and uncompressed images
       dstSubresource:TVkImageSubresourceLayers;
       dstOffset:TVkOffset3D; //< Specified in pixels for both compressed and uncompressed images
       extent:TVkExtent3D; //< Specified in pixels for both compressed and uncompressed images
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pSrcSubresource:TVkImageSubresourceLayers;
                          const pSrcOffset:TVkOffset3D; //< Specified in pixels for both compressed and uncompressed images
                          const pDstSubresource:TVkImageSubresourceLayers;
                          const pDstOffset:TVkOffset3D; //< Specified in pixels for both compressed and uncompressed images
                          const pExtent:TVkExtent3D); //< Specified in pixels for both compressed and uncompressed images
{$endif}
     end;

     PPVkImageBlit=^PVkImageBlit;
     PVkImageBlit=^TVkImageBlit;
     TVkImageBlit=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       srcSubresource:TVkImageSubresourceLayers;
       srcOffsets:array[0..1] of TVkOffset3D; //< Specified in pixels for both compressed and uncompressed images
       dstSubresource:TVkImageSubresourceLayers;
       dstOffsets:array[0..1] of TVkOffset3D; //< Specified in pixels for both compressed and uncompressed images
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pSrcSubresource:TVkImageSubresourceLayers;
                          const pSrcOffsets:array of TVkOffset3D; //< Specified in pixels for both compressed and uncompressed images
                          const pDstSubresource:TVkImageSubresourceLayers;
                          const pDstOffsets:array of TVkOffset3D); //< Specified in pixels for both compressed and uncompressed images
{$endif}
     end;

     PPVkBufferImageCopy=^PVkBufferImageCopy;
     PVkBufferImageCopy=^TVkBufferImageCopy;
     TVkBufferImageCopy=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       bufferOffset:TVkDeviceSize; //< Specified in bytes
       bufferRowLength:TVkUInt32; //< Specified in texels
       bufferImageHeight:TVkUInt32;
       imageSubresource:TVkImageSubresourceLayers;
       imageOffset:TVkOffset3D; //< Specified in pixels for both compressed and uncompressed images
       imageExtent:TVkExtent3D; //< Specified in pixels for both compressed and uncompressed images
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pBufferOffset:TVkDeviceSize; //< Specified in bytes
                          const pBufferRowLength:TVkUInt32; //< Specified in texels
                          const pBufferImageHeight:TVkUInt32;
                          const pImageSubresource:TVkImageSubresourceLayers;
                          const pImageOffset:TVkOffset3D; //< Specified in pixels for both compressed and uncompressed images
                          const pImageExtent:TVkExtent3D); //< Specified in pixels for both compressed and uncompressed images
{$endif}
     end;

     PPVkImageResolve=^PVkImageResolve;
     PVkImageResolve=^TVkImageResolve;
     TVkImageResolve=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       srcSubresource:TVkImageSubresourceLayers;
       srcOffset:TVkOffset3D;
       dstSubresource:TVkImageSubresourceLayers;
       dstOffset:TVkOffset3D;
       extent:TVkExtent3D;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pSrcSubresource:TVkImageSubresourceLayers;
                          const pSrcOffset:TVkOffset3D;
                          const pDstSubresource:TVkImageSubresourceLayers;
                          const pDstOffset:TVkOffset3D;
                          const pExtent:TVkExtent3D);
{$endif}
     end;

     PPVkShaderModuleCreateInfo=^PVkShaderModuleCreateInfo;
     PVkShaderModuleCreateInfo=^TVkShaderModuleCreateInfo;
     TVkShaderModuleCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkShaderModuleCreateFlags; //< Reserved
       codeSize:TVkSize; //< Specified in bytes
       pCode:PVkUInt32; //< Binary code of size codeSize
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkShaderModuleCreateFlags; //< Reserved
                          const pCodeSize:TVkSize; //< Specified in bytes
                          const pPCode:PVkUInt32); //< Binary code of size codeSize
{$endif}
     end;

     PPVkDescriptorSetLayoutBinding=^PVkDescriptorSetLayoutBinding;
     PVkDescriptorSetLayoutBinding=^TVkDescriptorSetLayoutBinding;
     TVkDescriptorSetLayoutBinding=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       binding:TVkUInt32; //< Binding number for this entry
       descriptorType:TVkDescriptorType; //< Type of the descriptors in this binding
       descriptorCount:TVkUInt32; //< Number of descriptors in this binding
       stageFlags:TVkShaderStageFlags; //< Shader stages this binding is visible to
       pImmutableSamplers:PVkSampler; //< Immutable samplers (used if descriptor type is SAMPLER or COMBINED_IMAGE_SAMPLER, is either NULL or contains count number of elements)
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pBinding:TVkUInt32; //< Binding number for this entry
                          const pDescriptorType:TVkDescriptorType; //< Type of the descriptors in this binding
                          const pDescriptorCount:TVkUInt32; //< Number of descriptors in this binding
                          const pStageFlags:TVkShaderStageFlags; //< Shader stages this binding is visible to
                          const pPImmutableSamplers:PVkSampler); //< Immutable samplers (used if descriptor type is SAMPLER or COMBINED_IMAGE_SAMPLER, is either NULL or contains count number of elements)
{$endif}
     end;

     PPVkDescriptorSetLayoutCreateInfo=^PVkDescriptorSetLayoutCreateInfo;
     PVkDescriptorSetLayoutCreateInfo=^TVkDescriptorSetLayoutCreateInfo;
     TVkDescriptorSetLayoutCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkDescriptorSetLayoutCreateFlags; //< Reserved
       bindingCount:TVkUInt32; //< Number of bindings in the descriptor set layout
       pBindings:PVkDescriptorSetLayoutBinding; //< Array of descriptor set layout bindings
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkDescriptorSetLayoutCreateFlags; //< Reserved
                          const pBindingCount:TVkUInt32; //< Number of bindings in the descriptor set layout
                          const pPBindings:PVkDescriptorSetLayoutBinding); //< Array of descriptor set layout bindings
{$endif}
     end;

     PPVkDescriptorPoolSize=^PVkDescriptorPoolSize;
     PVkDescriptorPoolSize=^TVkDescriptorPoolSize;
     TVkDescriptorPoolSize=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TVkDescriptorType;
       descriptorCount:TVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pType_:TVkDescriptorType;
                          const pDescriptorCount:TVkUInt32);
{$endif}
     end;

     PPVkDescriptorPoolCreateInfo=^PVkDescriptorPoolCreateInfo;
     PVkDescriptorPoolCreateInfo=^TVkDescriptorPoolCreateInfo;
     TVkDescriptorPoolCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkDescriptorPoolCreateFlags;
       maxSets:TVkUInt32;
       poolSizeCount:TVkUInt32;
       pPoolSizes:PVkDescriptorPoolSize;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkDescriptorPoolCreateFlags;
                          const pMaxSets:TVkUInt32;
                          const pPoolSizeCount:TVkUInt32;
                          const pPPoolSizes:PVkDescriptorPoolSize);
{$endif}
     end;

     PPVkDescriptorSetAllocateInfo=^PVkDescriptorSetAllocateInfo;
     PVkDescriptorSetAllocateInfo=^TVkDescriptorSetAllocateInfo;
     TVkDescriptorSetAllocateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       descriptorPool:TVkDescriptorPool;
       descriptorSetCount:TVkUInt32;
       pSetLayouts:PVkDescriptorSetLayout;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pDescriptorPool:TVkDescriptorPool;
                          const pDescriptorSetCount:TVkUInt32;
                          const pPSetLayouts:PVkDescriptorSetLayout);
{$endif}
     end;

     PPVkSpecializationMapEntry=^PVkSpecializationMapEntry;
     PVkSpecializationMapEntry=^TVkSpecializationMapEntry;
     TVkSpecializationMapEntry=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       constantID:TVkUInt32; //< The SpecConstant ID specified in the BIL
       offset:TVkUInt32; //< Offset of the value in the data block
       size:TVkSize; //< Size in bytes of the SpecConstant
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pConstantID:TVkUInt32; //< The SpecConstant ID specified in the BIL
                          const pOffset:TVkUInt32; //< Offset of the value in the data block
                          const pSize:TVkSize); //< Size in bytes of the SpecConstant
{$endif}
     end;

     PPVkSpecializationInfo=^PVkSpecializationInfo;
     PVkSpecializationInfo=^TVkSpecializationInfo;
     TVkSpecializationInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       mapEntryCount:TVkUInt32; //< Number of entries in the map
       pMapEntries:PVkSpecializationMapEntry; //< Array of map entries
       dataSize:TVkSize; //< Size in bytes of pData
       pData:PVkVoid; //< Pointer to SpecConstant data
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pMapEntryCount:TVkUInt32; //< Number of entries in the map
                          const pPMapEntries:PVkSpecializationMapEntry; //< Array of map entries
                          const pDataSize:TVkSize; //< Size in bytes of pData
                          const pPData:PVkVoid); //< Pointer to SpecConstant data
{$endif}
     end;

     PPVkPipelineShaderStageCreateInfo=^PVkPipelineShaderStageCreateInfo;
     PVkPipelineShaderStageCreateInfo=^TVkPipelineShaderStageCreateInfo;
     TVkPipelineShaderStageCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkPipelineShaderStageCreateFlags; //< Reserved
       stage:TVkShaderStageFlagBits; //< Shader stage
       module:TVkShaderModule; //< Module containing entry point
       pName:PVkChar; //< Null-terminated entry point name
       pSpecializationInfo:PVkSpecializationInfo;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkPipelineShaderStageCreateFlags; //< Reserved
                          const pStage:TVkShaderStageFlagBits; //< Shader stage
                          const pModule:TVkShaderModule; //< Module containing entry point
                          const pPName:PVkChar; //< Null-terminated entry point name
                          const pPSpecializationInfo:PVkSpecializationInfo);
{$endif}
     end;

     PPVkComputePipelineCreateInfo=^PVkComputePipelineCreateInfo;
     PVkComputePipelineCreateInfo=^TVkComputePipelineCreateInfo;
     TVkComputePipelineCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkPipelineCreateFlags; //< Pipeline creation flags
       stage:TVkPipelineShaderStageCreateInfo;
       layout:TVkPipelineLayout; //< Interface layout of the pipeline
       basePipelineHandle:TVkPipeline; //< If VK_PIPELINE_CREATE_DERIVATIVE_BIT is set and this value is nonzero, it specifies the handle of the base pipeline this is a derivative of
       basePipelineIndex:TVkInt32; //< If VK_PIPELINE_CREATE_DERIVATIVE_BIT is set and this value is not -1, it specifies an index into pCreateInfos of the base pipeline this is a derivative of
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkPipelineCreateFlags; //< Pipeline creation flags
                          const pStage:TVkPipelineShaderStageCreateInfo;
                          const pLayout:TVkPipelineLayout; //< Interface layout of the pipeline
                          const pBasePipelineHandle:TVkPipeline; //< If VK_PIPELINE_CREATE_DERIVATIVE_BIT is set and this value is nonzero, it specifies the handle of the base pipeline this is a derivative of
                          const pBasePipelineIndex:TVkInt32); //< If VK_PIPELINE_CREATE_DERIVATIVE_BIT is set and this value is not -1, it specifies an index into pCreateInfos of the base pipeline this is a derivative of
{$endif}
     end;

     PPVkVertexInputBindingDescription=^PVkVertexInputBindingDescription;
     PVkVertexInputBindingDescription=^TVkVertexInputBindingDescription;
     TVkVertexInputBindingDescription=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       binding:TVkUInt32; //< Vertex buffer binding id
       stride:TVkUInt32; //< Distance between vertices in bytes (0 = no advancement)
       inputRate:TVkVertexInputRate; //< The rate at which the vertex data is consumed
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pBinding:TVkUInt32; //< Vertex buffer binding id
                          const pStride:TVkUInt32; //< Distance between vertices in bytes (0 = no advancement)
                          const pInputRate:TVkVertexInputRate); //< The rate at which the vertex data is consumed
{$endif}
     end;

     PPVkVertexInputAttributeDescription=^PVkVertexInputAttributeDescription;
     PVkVertexInputAttributeDescription=^TVkVertexInputAttributeDescription;
     TVkVertexInputAttributeDescription=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       location:TVkUInt32; //< location of the shader vertex attrib
       binding:TVkUInt32; //< Vertex buffer binding id
       format:TVkFormat; //< format of source data
       offset:TVkUInt32; //< Offset of first element in bytes from base of vertex
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pLocation:TVkUInt32; //< location of the shader vertex attrib
                          const pBinding:TVkUInt32; //< Vertex buffer binding id
                          const pFormat:TVkFormat; //< format of source data
                          const pOffset:TVkUInt32); //< Offset of first element in bytes from base of vertex
{$endif}
     end;

     PPVkPipelineVertexInputStateCreateInfo=^PVkPipelineVertexInputStateCreateInfo;
     PVkPipelineVertexInputStateCreateInfo=^TVkPipelineVertexInputStateCreateInfo;
     TVkPipelineVertexInputStateCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkPipelineVertexInputStateCreateFlags; //< Reserved
       vertexBindingDescriptionCount:TVkUInt32; //< number of bindings
       pVertexBindingDescriptions:PVkVertexInputBindingDescription;
       vertexAttributeDescriptionCount:TVkUInt32; //< number of attributes
       pVertexAttributeDescriptions:PVkVertexInputAttributeDescription;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkPipelineVertexInputStateCreateFlags; //< Reserved
                          const pVertexBindingDescriptionCount:TVkUInt32; //< number of bindings
                          const pPVertexBindingDescriptions:PVkVertexInputBindingDescription;
                          const pVertexAttributeDescriptionCount:TVkUInt32; //< number of attributes
                          const pPVertexAttributeDescriptions:PVkVertexInputAttributeDescription);
{$endif}
     end;

     PPVkPipelineInputAssemblyStateCreateInfo=^PVkPipelineInputAssemblyStateCreateInfo;
     PVkPipelineInputAssemblyStateCreateInfo=^TVkPipelineInputAssemblyStateCreateInfo;
     TVkPipelineInputAssemblyStateCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkPipelineInputAssemblyStateCreateFlags; //< Reserved
       topology:TVkPrimitiveTopology;
       primitiveRestartEnable:TVkBool32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkPipelineInputAssemblyStateCreateFlags; //< Reserved
                          const pTopology:TVkPrimitiveTopology;
                          const pPrimitiveRestartEnable:TVkBool32);
{$endif}
     end;

     PPVkPipelineTessellationStateCreateInfo=^PVkPipelineTessellationStateCreateInfo;
     PVkPipelineTessellationStateCreateInfo=^TVkPipelineTessellationStateCreateInfo;
     TVkPipelineTessellationStateCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkPipelineTessellationStateCreateFlags; //< Reserved
       patchControlPoints:TVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkPipelineTessellationStateCreateFlags; //< Reserved
                          const pPatchControlPoints:TVkUInt32);
{$endif}
     end;

     PPVkPipelineViewportStateCreateInfo=^PVkPipelineViewportStateCreateInfo;
     PVkPipelineViewportStateCreateInfo=^TVkPipelineViewportStateCreateInfo;
     TVkPipelineViewportStateCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkPipelineViewportStateCreateFlags; //< Reserved
       viewportCount:TVkUInt32;
       pViewports:PVkViewport;
       scissorCount:TVkUInt32;
       pScissors:PVkRect2D;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkPipelineViewportStateCreateFlags; //< Reserved
                          const pViewportCount:TVkUInt32;
                          const pPViewports:PVkViewport;
                          const pScissorCount:TVkUInt32;
                          const pPScissors:PVkRect2D);
{$endif}
     end;

     PPVkPipelineRasterizationStateCreateInfo=^PVkPipelineRasterizationStateCreateInfo;
     PVkPipelineRasterizationStateCreateInfo=^TVkPipelineRasterizationStateCreateInfo;
     TVkPipelineRasterizationStateCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkPipelineRasterizationStateCreateFlags; //< Reserved
       depthClampEnable:TVkBool32;
       rasterizerDiscardEnable:TVkBool32;
       polygonMode:TVkPolygonMode; //< optional (GL45)
       cullMode:TVkCullModeFlags;
       frontFace:TVkFrontFace;
       depthBiasEnable:TVkBool32;
       depthBiasConstantFactor:TVkFloat;
       depthBiasClamp:TVkFloat;
       depthBiasSlopeFactor:TVkFloat;
       lineWidth:TVkFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkPipelineRasterizationStateCreateFlags; //< Reserved
                          const pDepthClampEnable:TVkBool32;
                          const pRasterizerDiscardEnable:TVkBool32;
                          const pPolygonMode:TVkPolygonMode; //< optional (GL45)
                          const pCullMode:TVkCullModeFlags;
                          const pFrontFace:TVkFrontFace;
                          const pDepthBiasEnable:TVkBool32;
                          const pDepthBiasConstantFactor:TVkFloat;
                          const pDepthBiasClamp:TVkFloat;
                          const pDepthBiasSlopeFactor:TVkFloat;
                          const pLineWidth:TVkFloat);
{$endif}
     end;

     PPVkPipelineMultisampleStateCreateInfo=^PVkPipelineMultisampleStateCreateInfo;
     PVkPipelineMultisampleStateCreateInfo=^TVkPipelineMultisampleStateCreateInfo;
     TVkPipelineMultisampleStateCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkPipelineMultisampleStateCreateFlags; //< Reserved
       rasterizationSamples:TVkSampleCountFlagBits; //< Number of samples used for rasterization
       sampleShadingEnable:TVkBool32; //< optional (GL45)
       minSampleShading:TVkFloat; //< optional (GL45)
       pSampleMask:PVkSampleMask; //< Array of sampleMask words
       alphaToCoverageEnable:TVkBool32;
       alphaToOneEnable:TVkBool32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkPipelineMultisampleStateCreateFlags; //< Reserved
                          const pRasterizationSamples:TVkSampleCountFlagBits; //< Number of samples used for rasterization
                          const pSampleShadingEnable:TVkBool32; //< optional (GL45)
                          const pMinSampleShading:TVkFloat; //< optional (GL45)
                          const pPSampleMask:PVkSampleMask; //< Array of sampleMask words
                          const pAlphaToCoverageEnable:TVkBool32;
                          const pAlphaToOneEnable:TVkBool32);
{$endif}
     end;

     PPVkPipelineColorBlendAttachmentState=^PVkPipelineColorBlendAttachmentState;
     PVkPipelineColorBlendAttachmentState=^TVkPipelineColorBlendAttachmentState;
     TVkPipelineColorBlendAttachmentState=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       blendEnable:TVkBool32;
       srcColorBlendFactor:TVkBlendFactor;
       dstColorBlendFactor:TVkBlendFactor;
       colorBlendOp:TVkBlendOp;
       srcAlphaBlendFactor:TVkBlendFactor;
       dstAlphaBlendFactor:TVkBlendFactor;
       alphaBlendOp:TVkBlendOp;
       colorWriteMask:TVkColorComponentFlags;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pBlendEnable:TVkBool32;
                          const pSrcColorBlendFactor:TVkBlendFactor;
                          const pDstColorBlendFactor:TVkBlendFactor;
                          const pColorBlendOp:TVkBlendOp;
                          const pSrcAlphaBlendFactor:TVkBlendFactor;
                          const pDstAlphaBlendFactor:TVkBlendFactor;
                          const pAlphaBlendOp:TVkBlendOp;
                          const pColorWriteMask:TVkColorComponentFlags);
{$endif}
     end;

     PPVkPipelineColorBlendStateCreateInfo=^PVkPipelineColorBlendStateCreateInfo;
     PVkPipelineColorBlendStateCreateInfo=^TVkPipelineColorBlendStateCreateInfo;
     TVkPipelineColorBlendStateCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkPipelineColorBlendStateCreateFlags; //< Reserved
       logicOpEnable:TVkBool32;
       logicOp:TVkLogicOp;
       attachmentCount:TVkUInt32; //< # of pAttachments
       pAttachments:PVkPipelineColorBlendAttachmentState;
       blendConstants:array[0..3] of TVkFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkPipelineColorBlendStateCreateFlags; //< Reserved
                          const pLogicOpEnable:TVkBool32;
                          const pLogicOp:TVkLogicOp;
                          const pAttachmentCount:TVkUInt32; //< # of pAttachments
                          const pPAttachments:PVkPipelineColorBlendAttachmentState;
                          const pBlendConstants:array of TVkFloat);
{$endif}
     end;

     PPVkPipelineDynamicStateCreateInfo=^PVkPipelineDynamicStateCreateInfo;
     PVkPipelineDynamicStateCreateInfo=^TVkPipelineDynamicStateCreateInfo;
     TVkPipelineDynamicStateCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkPipelineDynamicStateCreateFlags; //< Reserved
       dynamicStateCount:TVkUInt32;
       pDynamicStates:PVkDynamicState;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkPipelineDynamicStateCreateFlags; //< Reserved
                          const pDynamicStateCount:TVkUInt32;
                          const pPDynamicStates:PVkDynamicState);
{$endif}
     end;

     PPVkStencilOpState=^PVkStencilOpState;
     PVkStencilOpState=^TVkStencilOpState;
     TVkStencilOpState=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       failOp:TVkStencilOp;
       passOp:TVkStencilOp;
       depthFailOp:TVkStencilOp;
       compareOp:TVkCompareOp;
       compareMask:TVkUInt32;
       writeMask:TVkUInt32;
       reference:TVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFailOp:TVkStencilOp;
                          const pPassOp:TVkStencilOp;
                          const pDepthFailOp:TVkStencilOp;
                          const pCompareOp:TVkCompareOp;
                          const pCompareMask:TVkUInt32;
                          const pWriteMask:TVkUInt32;
                          const pReference:TVkUInt32);
{$endif}
     end;

     PPVkPipelineDepthStencilStateCreateInfo=^PVkPipelineDepthStencilStateCreateInfo;
     PVkPipelineDepthStencilStateCreateInfo=^TVkPipelineDepthStencilStateCreateInfo;
     TVkPipelineDepthStencilStateCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkPipelineDepthStencilStateCreateFlags; //< Reserved
       depthTestEnable:TVkBool32;
       depthWriteEnable:TVkBool32;
       depthCompareOp:TVkCompareOp;
       depthBoundsTestEnable:TVkBool32; //< optional (depth_bounds_test)
       stencilTestEnable:TVkBool32;
       front:TVkStencilOpState;
       back:TVkStencilOpState;
       minDepthBounds:TVkFloat;
       maxDepthBounds:TVkFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkPipelineDepthStencilStateCreateFlags; //< Reserved
                          const pDepthTestEnable:TVkBool32;
                          const pDepthWriteEnable:TVkBool32;
                          const pDepthCompareOp:TVkCompareOp;
                          const pDepthBoundsTestEnable:TVkBool32; //< optional (depth_bounds_test)
                          const pStencilTestEnable:TVkBool32;
                          const pFront:TVkStencilOpState;
                          const pBack:TVkStencilOpState;
                          const pMinDepthBounds:TVkFloat;
                          const pMaxDepthBounds:TVkFloat);
{$endif}
     end;

     PPVkGraphicsPipelineCreateInfo=^PVkGraphicsPipelineCreateInfo;
     PVkGraphicsPipelineCreateInfo=^TVkGraphicsPipelineCreateInfo;
     TVkGraphicsPipelineCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkPipelineCreateFlags; //< Pipeline creation flags
       stageCount:TVkUInt32;
       pStages:PVkPipelineShaderStageCreateInfo; //< One entry for each active shader stage
       pVertexInputState:PVkPipelineVertexInputStateCreateInfo;
       pInputAssemblyState:PVkPipelineInputAssemblyStateCreateInfo;
       pTessellationState:PVkPipelineTessellationStateCreateInfo;
       pViewportState:PVkPipelineViewportStateCreateInfo;
       pRasterizationState:PVkPipelineRasterizationStateCreateInfo;
       pMultisampleState:PVkPipelineMultisampleStateCreateInfo;
       pDepthStencilState:PVkPipelineDepthStencilStateCreateInfo;
       pColorBlendState:PVkPipelineColorBlendStateCreateInfo;
       pDynamicState:PVkPipelineDynamicStateCreateInfo;
       layout:TVkPipelineLayout; //< Interface layout of the pipeline
       renderPass:TVkRenderPass;
       subpass:TVkUInt32;
       basePipelineHandle:TVkPipeline; //< If VK_PIPELINE_CREATE_DERIVATIVE_BIT is set and this value is nonzero, it specifies the handle of the base pipeline this is a derivative of
       basePipelineIndex:TVkInt32; //< If VK_PIPELINE_CREATE_DERIVATIVE_BIT is set and this value is not -1, it specifies an index into pCreateInfos of the base pipeline this is a derivative of
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkPipelineCreateFlags; //< Pipeline creation flags
                          const pStageCount:TVkUInt32;
                          const pPStages:PVkPipelineShaderStageCreateInfo; //< One entry for each active shader stage
                          const pPVertexInputState:PVkPipelineVertexInputStateCreateInfo;
                          const pPInputAssemblyState:PVkPipelineInputAssemblyStateCreateInfo;
                          const pPTessellationState:PVkPipelineTessellationStateCreateInfo;
                          const pPViewportState:PVkPipelineViewportStateCreateInfo;
                          const pPRasterizationState:PVkPipelineRasterizationStateCreateInfo;
                          const pPMultisampleState:PVkPipelineMultisampleStateCreateInfo;
                          const pPDepthStencilState:PVkPipelineDepthStencilStateCreateInfo;
                          const pPColorBlendState:PVkPipelineColorBlendStateCreateInfo;
                          const pPDynamicState:PVkPipelineDynamicStateCreateInfo;
                          const pLayout:TVkPipelineLayout; //< Interface layout of the pipeline
                          const pRenderPass:TVkRenderPass;
                          const pSubpass:TVkUInt32;
                          const pBasePipelineHandle:TVkPipeline; //< If VK_PIPELINE_CREATE_DERIVATIVE_BIT is set and this value is nonzero, it specifies the handle of the base pipeline this is a derivative of
                          const pBasePipelineIndex:TVkInt32); //< If VK_PIPELINE_CREATE_DERIVATIVE_BIT is set and this value is not -1, it specifies an index into pCreateInfos of the base pipeline this is a derivative of
{$endif}
     end;

     PPVkPipelineCacheCreateInfo=^PVkPipelineCacheCreateInfo;
     PVkPipelineCacheCreateInfo=^TVkPipelineCacheCreateInfo;
     TVkPipelineCacheCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkPipelineCacheCreateFlags; //< Reserved
       initialDataSize:TVkSize; //< Size of initial data to populate cache, in bytes
       pInitialData:PVkVoid; //< Initial data to populate cache
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkPipelineCacheCreateFlags; //< Reserved
                          const pInitialDataSize:TVkSize; //< Size of initial data to populate cache, in bytes
                          const pPInitialData:PVkVoid); //< Initial data to populate cache
{$endif}
     end;

     PPVkPushConstantRange=^PVkPushConstantRange;
     PVkPushConstantRange=^TVkPushConstantRange;
     TVkPushConstantRange=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       stageFlags:TVkShaderStageFlags; //< Which stages use the range
       offset:TVkUInt32; //< Start of the range, in bytes
       size:TVkUInt32; //< Size of the range, in bytes
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pStageFlags:TVkShaderStageFlags; //< Which stages use the range
                          const pOffset:TVkUInt32; //< Start of the range, in bytes
                          const pSize:TVkUInt32); //< Size of the range, in bytes
{$endif}
     end;

     PPVkPipelineLayoutCreateInfo=^PVkPipelineLayoutCreateInfo;
     PVkPipelineLayoutCreateInfo=^TVkPipelineLayoutCreateInfo;
     TVkPipelineLayoutCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkPipelineLayoutCreateFlags; //< Reserved
       setLayoutCount:TVkUInt32; //< Number of descriptor sets interfaced by the pipeline
       pSetLayouts:PVkDescriptorSetLayout; //< Array of setCount number of descriptor set layout objects defining the layout of the
       pushConstantRangeCount:TVkUInt32; //< Number of push-constant ranges used by the pipeline
       pPushConstantRanges:PVkPushConstantRange; //< Array of pushConstantRangeCount number of ranges used by various shader stages
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkPipelineLayoutCreateFlags; //< Reserved
                          const pSetLayoutCount:TVkUInt32; //< Number of descriptor sets interfaced by the pipeline
                          const pPSetLayouts:PVkDescriptorSetLayout; //< Array of setCount number of descriptor set layout objects defining the layout of the
                          const pPushConstantRangeCount:TVkUInt32; //< Number of push-constant ranges used by the pipeline
                          const pPPushConstantRanges:PVkPushConstantRange); //< Array of pushConstantRangeCount number of ranges used by various shader stages
{$endif}
     end;

     PPVkSamplerCreateInfo=^PVkSamplerCreateInfo;
     PVkSamplerCreateInfo=^TVkSamplerCreateInfo;
     TVkSamplerCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkSamplerCreateFlags; //< Reserved
       magFilter:TVkFilter; //< Filter mode for magnification
       minFilter:TVkFilter; //< Filter mode for minifiation
       mipmapMode:TVkSamplerMipmapMode; //< Mipmap selection mode
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
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkSamplerCreateFlags; //< Reserved
                          const pMagFilter:TVkFilter; //< Filter mode for magnification
                          const pMinFilter:TVkFilter; //< Filter mode for minifiation
                          const pMipmapMode:TVkSamplerMipmapMode; //< Mipmap selection mode
                          const pAddressModeU:TVkSamplerAddressMode;
                          const pAddressModeV:TVkSamplerAddressMode;
                          const pAddressModeW:TVkSamplerAddressMode;
                          const pMipLodBias:TVkFloat;
                          const pAnisotropyEnable:TVkBool32;
                          const pMaxAnisotropy:TVkFloat;
                          const pCompareEnable:TVkBool32;
                          const pCompareOp:TVkCompareOp;
                          const pMinLod:TVkFloat;
                          const pMaxLod:TVkFloat;
                          const pBorderColor:TVkBorderColor;
                          const pUnnormalizedCoordinates:TVkBool32);
{$endif}
     end;

     PPVkCommandPoolCreateInfo=^PVkCommandPoolCreateInfo;
     PVkCommandPoolCreateInfo=^TVkCommandPoolCreateInfo;
     TVkCommandPoolCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkCommandPoolCreateFlags; //< Command pool creation flags
       queueFamilyIndex:TVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkCommandPoolCreateFlags; //< Command pool creation flags
                          const pQueueFamilyIndex:TVkUInt32);
{$endif}
     end;

     PPVkCommandBufferAllocateInfo=^PVkCommandBufferAllocateInfo;
     PVkCommandBufferAllocateInfo=^TVkCommandBufferAllocateInfo;
     TVkCommandBufferAllocateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       commandPool:TVkCommandPool;
       level:TVkCommandBufferLevel;
       commandBufferCount:TVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pCommandPool:TVkCommandPool;
                          const pLevel:TVkCommandBufferLevel;
                          const pCommandBufferCount:TVkUInt32);
{$endif}
     end;

     PPVkCommandBufferInheritanceInfo=^PVkCommandBufferInheritanceInfo;
     PVkCommandBufferInheritanceInfo=^TVkCommandBufferInheritanceInfo;
     TVkCommandBufferInheritanceInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       renderPass:TVkRenderPass; //< Render pass for secondary command buffers
       subpass:TVkUInt32;
       framebuffer:TVkFramebuffer; //< Framebuffer for secondary command buffers
       occlusionQueryEnable:TVkBool32; //< Whether this secondary command buffer may be executed during an occlusion query
       queryFlags:TVkQueryControlFlags; //< Query flags used by this secondary command buffer, if executed during an occlusion query
       pipelineStatistics:TVkQueryPipelineStatisticFlags; //< Pipeline statistics that may be counted for this secondary command buffer
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pRenderPass:TVkRenderPass; //< Render pass for secondary command buffers
                          const pSubpass:TVkUInt32;
                          const pFramebuffer:TVkFramebuffer; //< Framebuffer for secondary command buffers
                          const pOcclusionQueryEnable:TVkBool32; //< Whether this secondary command buffer may be executed during an occlusion query
                          const pQueryFlags:TVkQueryControlFlags; //< Query flags used by this secondary command buffer, if executed during an occlusion query
                          const pPipelineStatistics:TVkQueryPipelineStatisticFlags); //< Pipeline statistics that may be counted for this secondary command buffer
{$endif}
     end;

     PPVkCommandBufferBeginInfo=^PVkCommandBufferBeginInfo;
     PVkCommandBufferBeginInfo=^TVkCommandBufferBeginInfo;
     TVkCommandBufferBeginInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkCommandBufferUsageFlags; //< Command buffer usage flags
       pInheritanceInfo:PVkCommandBufferInheritanceInfo; //< Pointer to inheritance info for secondary command buffers
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkCommandBufferUsageFlags; //< Command buffer usage flags
                          const pPInheritanceInfo:PVkCommandBufferInheritanceInfo); //< Pointer to inheritance info for secondary command buffers
{$endif}
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
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       depth:TVkFloat;
       stencil:TVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pDepth:TVkFloat;
                          const pStencil:TVkUInt32);
{$endif}
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
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
       pNext:PVkVoid; //< Pointer to next structure
       renderPass:TVkRenderPass;
       framebuffer:TVkFramebuffer;
       renderArea:TVkRect2D;
       clearValueCount:TVkUInt32;
       pClearValues:PVkClearValue;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pRenderPass:TVkRenderPass;
                          const pFramebuffer:TVkFramebuffer;
                          const pRenderArea:TVkRect2D;
                          const pClearValueCount:TVkUInt32;
                          const pPClearValues:PVkClearValue);
{$endif}
     end;

     PPVkClearAttachment=^PVkClearAttachment;
     PVkClearAttachment=^TVkClearAttachment;
     TVkClearAttachment=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       aspectMask:TVkImageAspectFlags;
       colorAttachment:TVkUInt32;
       clearValue:TVkClearValue;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pAspectMask:TVkImageAspectFlags;
                          const pColorAttachment:TVkUInt32;
                          const pClearValue:TVkClearValue);
{$endif}
     end;

     PPVkAttachmentDescription=^PVkAttachmentDescription;
     PVkAttachmentDescription=^TVkAttachmentDescription;
     TVkAttachmentDescription=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       flags:TVkAttachmentDescriptionFlags;
       format:TVkFormat;
       samples:TVkSampleCountFlagBits;
       loadOp:TVkAttachmentLoadOp; //< Load operation for color or depth data
       storeOp:TVkAttachmentStoreOp; //< Store operation for color or depth data
       stencilLoadOp:TVkAttachmentLoadOp; //< Load operation for stencil data
       stencilStoreOp:TVkAttachmentStoreOp; //< Store operation for stencil data
       initialLayout:TVkImageLayout;
       finalLayout:TVkImageLayout;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkAttachmentDescriptionFlags;
                          const pFormat:TVkFormat;
                          const pSamples:TVkSampleCountFlagBits;
                          const pLoadOp:TVkAttachmentLoadOp; //< Load operation for color or depth data
                          const pStoreOp:TVkAttachmentStoreOp; //< Store operation for color or depth data
                          const pStencilLoadOp:TVkAttachmentLoadOp; //< Load operation for stencil data
                          const pStencilStoreOp:TVkAttachmentStoreOp; //< Store operation for stencil data
                          const pInitialLayout:TVkImageLayout;
                          const pFinalLayout:TVkImageLayout);
{$endif}
     end;

     PPVkAttachmentReference=^PVkAttachmentReference;
     PVkAttachmentReference=^TVkAttachmentReference;
     TVkAttachmentReference=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       attachment:TVkUInt32;
       layout:TVkImageLayout;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pAttachment:TVkUInt32;
                          const pLayout:TVkImageLayout);
{$endif}
     end;

     PPVkSubpassDescription=^PVkSubpassDescription;
     PVkSubpassDescription=^TVkSubpassDescription;
     TVkSubpassDescription=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       flags:TVkSubpassDescriptionFlags;
       pipelineBindPoint:TVkPipelineBindPoint; //< Must be VK_PIPELINE_BIND_POINT_GRAPHICS for now
       inputAttachmentCount:TVkUInt32;
       pInputAttachments:PVkAttachmentReference;
       colorAttachmentCount:TVkUInt32;
       pColorAttachments:PVkAttachmentReference;
       pResolveAttachments:PVkAttachmentReference;
       pDepthStencilAttachment:PVkAttachmentReference;
       preserveAttachmentCount:TVkUInt32;
       pPreserveAttachments:PVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkSubpassDescriptionFlags;
                          const pPipelineBindPoint:TVkPipelineBindPoint; //< Must be VK_PIPELINE_BIND_POINT_GRAPHICS for now
                          const pInputAttachmentCount:TVkUInt32;
                          const pPInputAttachments:PVkAttachmentReference;
                          const pColorAttachmentCount:TVkUInt32;
                          const pPColorAttachments:PVkAttachmentReference;
                          const pPResolveAttachments:PVkAttachmentReference;
                          const pPDepthStencilAttachment:PVkAttachmentReference;
                          const pPreserveAttachmentCount:TVkUInt32;
                          const pPPreserveAttachments:PVkUInt32);
{$endif}
     end;

     PPVkSubpassDependency=^PVkSubpassDependency;
     PVkSubpassDependency=^TVkSubpassDependency;
     TVkSubpassDependency=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       srcSubpass:TVkUInt32;
       dstSubpass:TVkUInt32;
       srcStageMask:TVkPipelineStageFlags;
       dstStageMask:TVkPipelineStageFlags;
       srcAccessMask:TVkAccessFlags; //< Memory accesses from the source of the dependency to synchronize
       dstAccessMask:TVkAccessFlags; //< Memory accesses from the destination of the dependency to synchronize
       dependencyFlags:TVkDependencyFlags;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pSrcSubpass:TVkUInt32;
                          const pDstSubpass:TVkUInt32;
                          const pSrcStageMask:TVkPipelineStageFlags;
                          const pDstStageMask:TVkPipelineStageFlags;
                          const pSrcAccessMask:TVkAccessFlags; //< Memory accesses from the source of the dependency to synchronize
                          const pDstAccessMask:TVkAccessFlags; //< Memory accesses from the destination of the dependency to synchronize
                          const pDependencyFlags:TVkDependencyFlags);
{$endif}
     end;

     PPVkRenderPassCreateInfo=^PVkRenderPassCreateInfo;
     PVkRenderPassCreateInfo=^TVkRenderPassCreateInfo;
     TVkRenderPassCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkRenderPassCreateFlags; //< Reserved
       attachmentCount:TVkUInt32;
       pAttachments:PVkAttachmentDescription;
       subpassCount:TVkUInt32;
       pSubpasses:PVkSubpassDescription;
       dependencyCount:TVkUInt32;
       pDependencies:PVkSubpassDependency;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkRenderPassCreateFlags; //< Reserved
                          const pAttachmentCount:TVkUInt32;
                          const pPAttachments:PVkAttachmentDescription;
                          const pSubpassCount:TVkUInt32;
                          const pPSubpasses:PVkSubpassDescription;
                          const pDependencyCount:TVkUInt32;
                          const pPDependencies:PVkSubpassDependency);
{$endif}
     end;

     PPVkEventCreateInfo=^PVkEventCreateInfo;
     PVkEventCreateInfo=^TVkEventCreateInfo;
     TVkEventCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_EVENT_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkEventCreateFlags; //< Event creation flags
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkEventCreateFlags); //< Event creation flags
{$endif}
     end;

     PPVkFenceCreateInfo=^PVkFenceCreateInfo;
     PVkFenceCreateInfo=^TVkFenceCreateInfo;
     TVkFenceCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkFenceCreateFlags; //< Fence creation flags
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkFenceCreateFlags); //< Fence creation flags
{$endif}
     end;

     PPVkDeviceCreateInfo=^PVkDeviceCreateInfo;
     PVkDeviceCreateInfo=^TVkDeviceCreateInfo;
     TVkDeviceCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkDeviceCreateFlags; //< Reserved
       queueCreateInfoCount:TVkUInt32;
       pQueueCreateInfos:PVkDeviceQueueCreateInfo;
       enabledLayerCount:TVkUInt32;
       ppEnabledLayerNames:PPVkChar; //< Ordered list of layer names to be enabled
       enabledExtensionCount:TVkUInt32;
       ppEnabledExtensionNames:PPVkChar;
       pEnabledFeatures:PVkPhysicalDeviceFeatures;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkDeviceCreateFlags; //< Reserved
                          const pQueueCreateInfoCount:TVkUInt32;
                          const pPQueueCreateInfos:PVkDeviceQueueCreateInfo;
                          const pEnabledLayerCount:TVkUInt32;
                          const pPpEnabledLayerNames:PPVkChar; //< Ordered list of layer names to be enabled
                          const pEnabledExtensionCount:TVkUInt32;
                          const pPpEnabledExtensionNames:PPVkChar;
                          const pPEnabledFeatures:PVkPhysicalDeviceFeatures);
{$endif}
     end;

     PPVkPhysicalDeviceLimits=^PVkPhysicalDeviceLimits;
     PVkPhysicalDeviceLimits=^TVkPhysicalDeviceLimits;
     TVkPhysicalDeviceLimits=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       maxImageDimension1D:TVkUInt32; //< max 1D image dimension
       maxImageDimension2D:TVkUInt32; //< max 2D image dimension
       maxImageDimension3D:TVkUInt32; //< max 3D image dimension
       maxImageDimensionCube:TVkUInt32; //< max cubemap image dimension
       maxImageArrayLayers:TVkUInt32; //< max layers for image arrays
       maxTexelBufferElements:TVkUInt32; //< max texel buffer size (fstexels)
       maxUniformBufferRange:TVkUInt32; //< max uniform buffer range (bytes)
       maxStorageBufferRange:TVkUInt32; //< max storage buffer range (bytes)
       maxPushConstantsSize:TVkUInt32; //< max size of the push constants pool (bytes)
       maxMemoryAllocationCount:TVkUInt32; //< max number of device memory allocations supported
       maxSamplerAllocationCount:TVkUInt32; //< max number of samplers that can be allocated on a device
       bufferImageGranularity:TVkDeviceSize; //< Granularity (in bytes) at which buffers and images can be bound to adjacent memory for simultaneous usage
       sparseAddressSpaceSize:TVkDeviceSize; //< Total address space available for sparse allocations (bytes)
       maxBoundDescriptorSets:TVkUInt32; //< max number of descriptors sets that can be bound to a pipeline
       maxPerStageDescriptorSamplers:TVkUInt32; //< max number of samplers allowed per-stage in a descriptor set
       maxPerStageDescriptorUniformBuffers:TVkUInt32; //< max number of uniform buffers allowed per-stage in a descriptor set
       maxPerStageDescriptorStorageBuffers:TVkUInt32; //< max number of storage buffers allowed per-stage in a descriptor set
       maxPerStageDescriptorSampledImages:TVkUInt32; //< max number of sampled images allowed per-stage in a descriptor set
       maxPerStageDescriptorStorageImages:TVkUInt32; //< max number of storage images allowed per-stage in a descriptor set
       maxPerStageDescriptorInputAttachments:TVkUInt32; //< max number of input attachments allowed per-stage in a descriptor set
       maxPerStageResources:TVkUInt32; //< max number of resources allowed by a single stage
       maxDescriptorSetSamplers:TVkUInt32; //< max number of samplers allowed in all stages in a descriptor set
       maxDescriptorSetUniformBuffers:TVkUInt32; //< max number of uniform buffers allowed in all stages in a descriptor set
       maxDescriptorSetUniformBuffersDynamic:TVkUInt32; //< max number of dynamic uniform buffers allowed in all stages in a descriptor set
       maxDescriptorSetStorageBuffers:TVkUInt32; //< max number of storage buffers allowed in all stages in a descriptor set
       maxDescriptorSetStorageBuffersDynamic:TVkUInt32; //< max number of dynamic storage buffers allowed in all stages in a descriptor set
       maxDescriptorSetSampledImages:TVkUInt32; //< max number of sampled images allowed in all stages in a descriptor set
       maxDescriptorSetStorageImages:TVkUInt32; //< max number of storage images allowed in all stages in a descriptor set
       maxDescriptorSetInputAttachments:TVkUInt32; //< max number of input attachments allowed in all stages in a descriptor set
       maxVertexInputAttributes:TVkUInt32; //< max number of vertex input attribute slots
       maxVertexInputBindings:TVkUInt32; //< max number of vertex input binding slots
       maxVertexInputAttributeOffset:TVkUInt32; //< max vertex input attribute offset added to vertex buffer offset
       maxVertexInputBindingStride:TVkUInt32; //< max vertex input binding stride
       maxVertexOutputComponents:TVkUInt32; //< max number of output components written by vertex shader
       maxTessellationGenerationLevel:TVkUInt32; //< max level supported by tessellation primitive generator
       maxTessellationPatchSize:TVkUInt32; //< max patch size (vertices)
       maxTessellationControlPerVertexInputComponents:TVkUInt32; //< max number of input components per-vertex in TCS
       maxTessellationControlPerVertexOutputComponents:TVkUInt32; //< max number of output components per-vertex in TCS
       maxTessellationControlPerPatchOutputComponents:TVkUInt32; //< max number of output components per-patch in TCS
       maxTessellationControlTotalOutputComponents:TVkUInt32; //< max total number of per-vertex and per-patch output components in TCS
       maxTessellationEvaluationInputComponents:TVkUInt32; //< max number of input components per vertex in TES
       maxTessellationEvaluationOutputComponents:TVkUInt32; //< max number of output components per vertex in TES
       maxGeometryShaderInvocations:TVkUInt32; //< max invocation count supported in geometry shader
       maxGeometryInputComponents:TVkUInt32; //< max number of input components read in geometry stage
       maxGeometryOutputComponents:TVkUInt32; //< max number of output components written in geometry stage
       maxGeometryOutputVertices:TVkUInt32; //< max number of vertices that can be emitted in geometry stage
       maxGeometryTotalOutputComponents:TVkUInt32; //< max total number of components (all vertices) written in geometry stage
       maxFragmentInputComponents:TVkUInt32; //< max number of input compontents read in fragment stage
       maxFragmentOutputAttachments:TVkUInt32; //< max number of output attachments written in fragment stage
       maxFragmentDualSrcAttachments:TVkUInt32; //< max number of output attachments written when using dual source blending
       maxFragmentCombinedOutputResources:TVkUInt32; //< max total number of storage buffers, storage images and output buffers
       maxComputeSharedMemorySize:TVkUInt32; //< max total storage size of work group local storage (bytes)
       maxComputeWorkGroupCount:array[0..2] of TVkUInt32; //< max num of compute work groups that may be dispatched by a single command (x,y,z)
       maxComputeWorkGroupInvocations:TVkUInt32; //< max total compute invocations in a single local work group
       maxComputeWorkGroupSize:array[0..2] of TVkUInt32; //< max local size of a compute work group (x,y,z)
       subPixelPrecisionBits:TVkUInt32; //< number bits of subpixel precision in screen x and y
       subTexelPrecisionBits:TVkUInt32; //< number bits of precision for selecting texel weights
       mipmapPrecisionBits:TVkUInt32; //< number bits of precision for selecting mipmap weights
       maxDrawIndexedIndexValue:TVkUInt32; //< max index value for indexed draw calls (for 32-bit indices)
       maxDrawIndirectCount:TVkUInt32; //< max draw count for indirect draw calls
       maxSamplerLodBias:TVkFloat; //< max absolute sampler level of detail bias
       maxSamplerAnisotropy:TVkFloat; //< max degree of sampler anisotropy
       maxViewports:TVkUInt32; //< max number of active viewports
       maxViewportDimensions:array[0..1] of TVkUInt32; //< max viewport dimensions (x,y)
       viewportBoundsRange:array[0..1] of TVkFloat; //< viewport bounds range (min,max)
       viewportSubPixelBits:TVkUInt32; //< number bits of subpixel precision for viewport
       minMemoryMapAlignment:TVkSize; //< min required alignment of pointers returned by MapMemory (bytes)
       minTexelBufferOffsetAlignment:TVkDeviceSize; //< min required alignment for texel buffer offsets (bytes)
       minUniformBufferOffsetAlignment:TVkDeviceSize; //< min required alignment for uniform buffer sizes and offsets (bytes)
       minStorageBufferOffsetAlignment:TVkDeviceSize; //< min required alignment for storage buffer offsets (bytes)
       minTexelOffset:TVkInt32; //< min texel offset for OpTextureSampleOffset
       maxTexelOffset:TVkUInt32; //< max texel offset for OpTextureSampleOffset
       minTexelGatherOffset:TVkInt32; //< min texel offset for OpTextureGatherOffset
       maxTexelGatherOffset:TVkUInt32; //< max texel offset for OpTextureGatherOffset
       minInterpolationOffset:TVkFloat; //< furthest negative offset for interpolateAtOffset
       maxInterpolationOffset:TVkFloat; //< furthest positive offset for interpolateAtOffset
       subPixelInterpolationOffsetBits:TVkUInt32; //< number of subpixel bits for interpolateAtOffset
       maxFramebufferWidth:TVkUInt32; //< max width for a framebuffer
       maxFramebufferHeight:TVkUInt32; //< max height for a framebuffer
       maxFramebufferLayers:TVkUInt32; //< max layer count for a layered framebuffer
       framebufferColorSampleCounts:TVkSampleCountFlags; //< supported color sample counts for a framebuffer
       framebufferDepthSampleCounts:TVkSampleCountFlags; //< supported depth sample counts for a framebuffer
       framebufferStencilSampleCounts:TVkSampleCountFlags; //< supported stencil sample counts for a framebuffer
       framebufferNoAttachmentsSampleCounts:TVkSampleCountFlags; //< supported sample counts for a framebuffer with no attachments
       maxColorAttachments:TVkUInt32; //< max number of color attachments per subpass
       sampledImageColorSampleCounts:TVkSampleCountFlags; //< supported color sample counts for a non-integer sampled image
       sampledImageIntegerSampleCounts:TVkSampleCountFlags; //< supported sample counts for an integer image
       sampledImageDepthSampleCounts:TVkSampleCountFlags; //< supported depth sample counts for a sampled image
       sampledImageStencilSampleCounts:TVkSampleCountFlags; //< supported stencil sample counts for a sampled image
       storageImageSampleCounts:TVkSampleCountFlags; //< supported sample counts for a storage image
       maxSampleMaskWords:TVkUInt32; //< max number of sample mask words
       timestampComputeAndGraphics:TVkBool32; //< timestamps on graphics and compute queues
       timestampPeriod:TVkFloat; //< number of nanoseconds it takes for timestamp query value to increment by 1
       maxClipDistances:TVkUInt32; //< max number of clip distances
       maxCullDistances:TVkUInt32; //< max number of cull distances
       maxCombinedClipAndCullDistances:TVkUInt32; //< max combined number of user clipping
       discreteQueuePriorities:TVkUInt32; //< distinct queue priorities available
       pointSizeRange:array[0..1] of TVkFloat; //< range (min,max) of supported point sizes
       lineWidthRange:array[0..1] of TVkFloat; //< range (min,max) of supported line widths
       pointSizeGranularity:TVkFloat; //< granularity of supported point sizes
       lineWidthGranularity:TVkFloat; //< granularity of supported line widths
       strictLines:TVkBool32; //< line rasterization follows preferred rules
       standardSampleLocations:TVkBool32; //< supports standard sample locations for all supported sample counts
       optimalBufferCopyOffsetAlignment:TVkDeviceSize; //< optimal offset of buffer copies
       optimalBufferCopyRowPitchAlignment:TVkDeviceSize; //< optimal pitch of buffer copies
       nonCoherentAtomSize:TVkDeviceSize; //< minimum size and alignment for non-coherent host-mapped device memory access
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pMaxImageDimension1D:TVkUInt32; //< max 1D image dimension
                          const pMaxImageDimension2D:TVkUInt32; //< max 2D image dimension
                          const pMaxImageDimension3D:TVkUInt32; //< max 3D image dimension
                          const pMaxImageDimensionCube:TVkUInt32; //< max cubemap image dimension
                          const pMaxImageArrayLayers:TVkUInt32; //< max layers for image arrays
                          const pMaxTexelBufferElements:TVkUInt32; //< max texel buffer size (fstexels)
                          const pMaxUniformBufferRange:TVkUInt32; //< max uniform buffer range (bytes)
                          const pMaxStorageBufferRange:TVkUInt32; //< max storage buffer range (bytes)
                          const pMaxPushConstantsSize:TVkUInt32; //< max size of the push constants pool (bytes)
                          const pMaxMemoryAllocationCount:TVkUInt32; //< max number of device memory allocations supported
                          const pMaxSamplerAllocationCount:TVkUInt32; //< max number of samplers that can be allocated on a device
                          const pBufferImageGranularity:TVkDeviceSize; //< Granularity (in bytes) at which buffers and images can be bound to adjacent memory for simultaneous usage
                          const pSparseAddressSpaceSize:TVkDeviceSize; //< Total address space available for sparse allocations (bytes)
                          const pMaxBoundDescriptorSets:TVkUInt32; //< max number of descriptors sets that can be bound to a pipeline
                          const pMaxPerStageDescriptorSamplers:TVkUInt32; //< max number of samplers allowed per-stage in a descriptor set
                          const pMaxPerStageDescriptorUniformBuffers:TVkUInt32; //< max number of uniform buffers allowed per-stage in a descriptor set
                          const pMaxPerStageDescriptorStorageBuffers:TVkUInt32; //< max number of storage buffers allowed per-stage in a descriptor set
                          const pMaxPerStageDescriptorSampledImages:TVkUInt32; //< max number of sampled images allowed per-stage in a descriptor set
                          const pMaxPerStageDescriptorStorageImages:TVkUInt32; //< max number of storage images allowed per-stage in a descriptor set
                          const pMaxPerStageDescriptorInputAttachments:TVkUInt32; //< max number of input attachments allowed per-stage in a descriptor set
                          const pMaxPerStageResources:TVkUInt32; //< max number of resources allowed by a single stage
                          const pMaxDescriptorSetSamplers:TVkUInt32; //< max number of samplers allowed in all stages in a descriptor set
                          const pMaxDescriptorSetUniformBuffers:TVkUInt32; //< max number of uniform buffers allowed in all stages in a descriptor set
                          const pMaxDescriptorSetUniformBuffersDynamic:TVkUInt32; //< max number of dynamic uniform buffers allowed in all stages in a descriptor set
                          const pMaxDescriptorSetStorageBuffers:TVkUInt32; //< max number of storage buffers allowed in all stages in a descriptor set
                          const pMaxDescriptorSetStorageBuffersDynamic:TVkUInt32; //< max number of dynamic storage buffers allowed in all stages in a descriptor set
                          const pMaxDescriptorSetSampledImages:TVkUInt32; //< max number of sampled images allowed in all stages in a descriptor set
                          const pMaxDescriptorSetStorageImages:TVkUInt32; //< max number of storage images allowed in all stages in a descriptor set
                          const pMaxDescriptorSetInputAttachments:TVkUInt32; //< max number of input attachments allowed in all stages in a descriptor set
                          const pMaxVertexInputAttributes:TVkUInt32; //< max number of vertex input attribute slots
                          const pMaxVertexInputBindings:TVkUInt32; //< max number of vertex input binding slots
                          const pMaxVertexInputAttributeOffset:TVkUInt32; //< max vertex input attribute offset added to vertex buffer offset
                          const pMaxVertexInputBindingStride:TVkUInt32; //< max vertex input binding stride
                          const pMaxVertexOutputComponents:TVkUInt32; //< max number of output components written by vertex shader
                          const pMaxTessellationGenerationLevel:TVkUInt32; //< max level supported by tessellation primitive generator
                          const pMaxTessellationPatchSize:TVkUInt32; //< max patch size (vertices)
                          const pMaxTessellationControlPerVertexInputComponents:TVkUInt32; //< max number of input components per-vertex in TCS
                          const pMaxTessellationControlPerVertexOutputComponents:TVkUInt32; //< max number of output components per-vertex in TCS
                          const pMaxTessellationControlPerPatchOutputComponents:TVkUInt32; //< max number of output components per-patch in TCS
                          const pMaxTessellationControlTotalOutputComponents:TVkUInt32; //< max total number of per-vertex and per-patch output components in TCS
                          const pMaxTessellationEvaluationInputComponents:TVkUInt32; //< max number of input components per vertex in TES
                          const pMaxTessellationEvaluationOutputComponents:TVkUInt32; //< max number of output components per vertex in TES
                          const pMaxGeometryShaderInvocations:TVkUInt32; //< max invocation count supported in geometry shader
                          const pMaxGeometryInputComponents:TVkUInt32; //< max number of input components read in geometry stage
                          const pMaxGeometryOutputComponents:TVkUInt32; //< max number of output components written in geometry stage
                          const pMaxGeometryOutputVertices:TVkUInt32; //< max number of vertices that can be emitted in geometry stage
                          const pMaxGeometryTotalOutputComponents:TVkUInt32; //< max total number of components (all vertices) written in geometry stage
                          const pMaxFragmentInputComponents:TVkUInt32; //< max number of input compontents read in fragment stage
                          const pMaxFragmentOutputAttachments:TVkUInt32; //< max number of output attachments written in fragment stage
                          const pMaxFragmentDualSrcAttachments:TVkUInt32; //< max number of output attachments written when using dual source blending
                          const pMaxFragmentCombinedOutputResources:TVkUInt32; //< max total number of storage buffers, storage images and output buffers
                          const pMaxComputeSharedMemorySize:TVkUInt32; //< max total storage size of work group local storage (bytes)
                          const pMaxComputeWorkGroupCount:array of TVkUInt32; //< max num of compute work groups that may be dispatched by a single command (x,y,z)
                          const pMaxComputeWorkGroupInvocations:TVkUInt32; //< max total compute invocations in a single local work group
                          const pMaxComputeWorkGroupSize:array of TVkUInt32; //< max local size of a compute work group (x,y,z)
                          const pSubPixelPrecisionBits:TVkUInt32; //< number bits of subpixel precision in screen x and y
                          const pSubTexelPrecisionBits:TVkUInt32; //< number bits of precision for selecting texel weights
                          const pMipmapPrecisionBits:TVkUInt32; //< number bits of precision for selecting mipmap weights
                          const pMaxDrawIndexedIndexValue:TVkUInt32; //< max index value for indexed draw calls (for 32-bit indices)
                          const pMaxDrawIndirectCount:TVkUInt32; //< max draw count for indirect draw calls
                          const pMaxSamplerLodBias:TVkFloat; //< max absolute sampler level of detail bias
                          const pMaxSamplerAnisotropy:TVkFloat; //< max degree of sampler anisotropy
                          const pMaxViewports:TVkUInt32; //< max number of active viewports
                          const pMaxViewportDimensions:array of TVkUInt32; //< max viewport dimensions (x,y)
                          const pViewportBoundsRange:array of TVkFloat; //< viewport bounds range (min,max)
                          const pViewportSubPixelBits:TVkUInt32; //< number bits of subpixel precision for viewport
                          const pMinMemoryMapAlignment:TVkSize; //< min required alignment of pointers returned by MapMemory (bytes)
                          const pMinTexelBufferOffsetAlignment:TVkDeviceSize; //< min required alignment for texel buffer offsets (bytes)
                          const pMinUniformBufferOffsetAlignment:TVkDeviceSize; //< min required alignment for uniform buffer sizes and offsets (bytes)
                          const pMinStorageBufferOffsetAlignment:TVkDeviceSize; //< min required alignment for storage buffer offsets (bytes)
                          const pMinTexelOffset:TVkInt32; //< min texel offset for OpTextureSampleOffset
                          const pMaxTexelOffset:TVkUInt32; //< max texel offset for OpTextureSampleOffset
                          const pMinTexelGatherOffset:TVkInt32; //< min texel offset for OpTextureGatherOffset
                          const pMaxTexelGatherOffset:TVkUInt32; //< max texel offset for OpTextureGatherOffset
                          const pMinInterpolationOffset:TVkFloat; //< furthest negative offset for interpolateAtOffset
                          const pMaxInterpolationOffset:TVkFloat; //< furthest positive offset for interpolateAtOffset
                          const pSubPixelInterpolationOffsetBits:TVkUInt32; //< number of subpixel bits for interpolateAtOffset
                          const pMaxFramebufferWidth:TVkUInt32; //< max width for a framebuffer
                          const pMaxFramebufferHeight:TVkUInt32; //< max height for a framebuffer
                          const pMaxFramebufferLayers:TVkUInt32; //< max layer count for a layered framebuffer
                          const pFramebufferColorSampleCounts:TVkSampleCountFlags; //< supported color sample counts for a framebuffer
                          const pFramebufferDepthSampleCounts:TVkSampleCountFlags; //< supported depth sample counts for a framebuffer
                          const pFramebufferStencilSampleCounts:TVkSampleCountFlags; //< supported stencil sample counts for a framebuffer
                          const pFramebufferNoAttachmentsSampleCounts:TVkSampleCountFlags; //< supported sample counts for a framebuffer with no attachments
                          const pMaxColorAttachments:TVkUInt32; //< max number of color attachments per subpass
                          const pSampledImageColorSampleCounts:TVkSampleCountFlags; //< supported color sample counts for a non-integer sampled image
                          const pSampledImageIntegerSampleCounts:TVkSampleCountFlags; //< supported sample counts for an integer image
                          const pSampledImageDepthSampleCounts:TVkSampleCountFlags; //< supported depth sample counts for a sampled image
                          const pSampledImageStencilSampleCounts:TVkSampleCountFlags; //< supported stencil sample counts for a sampled image
                          const pStorageImageSampleCounts:TVkSampleCountFlags; //< supported sample counts for a storage image
                          const pMaxSampleMaskWords:TVkUInt32; //< max number of sample mask words
                          const pTimestampComputeAndGraphics:TVkBool32; //< timestamps on graphics and compute queues
                          const pTimestampPeriod:TVkFloat; //< number of nanoseconds it takes for timestamp query value to increment by 1
                          const pMaxClipDistances:TVkUInt32; //< max number of clip distances
                          const pMaxCullDistances:TVkUInt32; //< max number of cull distances
                          const pMaxCombinedClipAndCullDistances:TVkUInt32; //< max combined number of user clipping
                          const pDiscreteQueuePriorities:TVkUInt32; //< distinct queue priorities available
                          const pPointSizeRange:array of TVkFloat; //< range (min,max) of supported point sizes
                          const pLineWidthRange:array of TVkFloat; //< range (min,max) of supported line widths
                          const pPointSizeGranularity:TVkFloat; //< granularity of supported point sizes
                          const pLineWidthGranularity:TVkFloat; //< granularity of supported line widths
                          const pStrictLines:TVkBool32; //< line rasterization follows preferred rules
                          const pStandardSampleLocations:TVkBool32; //< supports standard sample locations for all supported sample counts
                          const pOptimalBufferCopyOffsetAlignment:TVkDeviceSize; //< optimal offset of buffer copies
                          const pOptimalBufferCopyRowPitchAlignment:TVkDeviceSize; //< optimal pitch of buffer copies
                          const pNonCoherentAtomSize:TVkDeviceSize); //< minimum size and alignment for non-coherent host-mapped device memory access
{$endif}
     end;

     PPVkPhysicalDeviceProperties=^PVkPhysicalDeviceProperties;
     PVkPhysicalDeviceProperties=^TVkPhysicalDeviceProperties;
     TVkPhysicalDeviceProperties=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       apiVersion:TVkUInt32;
       driverVersion:TVkUInt32;
       vendorID:TVkUInt32;
       deviceID:TVkUInt32;
       deviceType:TVkPhysicalDeviceType;
       deviceName:array[0..VK_MAX_PHYSICAL_DEVICE_NAME_SIZE-1] of TVkChar;
       pipelineCacheUUID:array[0..VK_UUID_SIZE-1] of TVkUInt8;
       limits:TVkPhysicalDeviceLimits;
       sparseProperties:TVkPhysicalDeviceSparseProperties;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pApiVersion:TVkUInt32;
                          const pDriverVersion:TVkUInt32;
                          const pVendorID:TVkUInt32;
                          const pDeviceID:TVkUInt32;
                          const pDeviceType:TVkPhysicalDeviceType;
                          const pDeviceName:TVkCharString;
                          const pPipelineCacheUUID:array of TVkUInt8;
                          const pLimits:TVkPhysicalDeviceLimits;
                          const pSparseProperties:TVkPhysicalDeviceSparseProperties);
{$endif}
     end;

     PPVkSemaphoreCreateInfo=^PVkSemaphoreCreateInfo;
     PVkSemaphoreCreateInfo=^TVkSemaphoreCreateInfo;
     TVkSemaphoreCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkSemaphoreCreateFlags; //< Semaphore creation flags
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkSemaphoreCreateFlags); //< Semaphore creation flags
{$endif}
     end;

     PPVkQueryPoolCreateInfo=^PVkQueryPoolCreateInfo;
     PVkQueryPoolCreateInfo=^TVkQueryPoolCreateInfo;
     TVkQueryPoolCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkQueryPoolCreateFlags; //< Reserved
       queryType:TVkQueryType;
       queryCount:TVkUInt32;
       pipelineStatistics:TVkQueryPipelineStatisticFlags; //< Optional
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkQueryPoolCreateFlags; //< Reserved
                          const pQueryType:TVkQueryType;
                          const pQueryCount:TVkUInt32;
                          const pPipelineStatistics:TVkQueryPipelineStatisticFlags); //< Optional
{$endif}
     end;

     PPVkFramebufferCreateInfo=^PVkFramebufferCreateInfo;
     PVkFramebufferCreateInfo=^TVkFramebufferCreateInfo;
     TVkFramebufferCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkFramebufferCreateFlags; //< Reserved
       renderPass:TVkRenderPass;
       attachmentCount:TVkUInt32;
       pAttachments:PVkImageView;
       width:TVkUInt32;
       height:TVkUInt32;
       layers:TVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkFramebufferCreateFlags; //< Reserved
                          const pRenderPass:TVkRenderPass;
                          const pAttachmentCount:TVkUInt32;
                          const pPAttachments:PVkImageView;
                          const pWidth:TVkUInt32;
                          const pHeight:TVkUInt32;
                          const pLayers:TVkUInt32);
{$endif}
     end;

     PPVkDrawIndirectCommand=^PVkDrawIndirectCommand;
     PVkDrawIndirectCommand=^TVkDrawIndirectCommand;
     TVkDrawIndirectCommand=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       vertexCount:TVkUInt32;
       instanceCount:TVkUInt32;
       firstVertex:TVkUInt32;
       firstInstance:TVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pVertexCount:TVkUInt32;
                          const pInstanceCount:TVkUInt32;
                          const pFirstVertex:TVkUInt32;
                          const pFirstInstance:TVkUInt32);
{$endif}
     end;

     PPVkDrawIndexedIndirectCommand=^PVkDrawIndexedIndirectCommand;
     PVkDrawIndexedIndirectCommand=^TVkDrawIndexedIndirectCommand;
     TVkDrawIndexedIndirectCommand=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       indexCount:TVkUInt32;
       instanceCount:TVkUInt32;
       firstIndex:TVkUInt32;
       vertexOffset:TVkInt32;
       firstInstance:TVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pIndexCount:TVkUInt32;
                          const pInstanceCount:TVkUInt32;
                          const pFirstIndex:TVkUInt32;
                          const pVertexOffset:TVkInt32;
                          const pFirstInstance:TVkUInt32);
{$endif}
     end;

     PPVkDispatchIndirectCommand=^PVkDispatchIndirectCommand;
     PVkDispatchIndirectCommand=^TVkDispatchIndirectCommand;
     TVkDispatchIndirectCommand=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       x:TVkUInt32;
       y:TVkUInt32;
       z:TVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pX:TVkUInt32;
                          const pY:TVkUInt32;
                          const pZ:TVkUInt32);
{$endif}
     end;

     PPVkSubmitInfo=^PVkSubmitInfo;
     PVkSubmitInfo=^TVkSubmitInfo;
     TVkSubmitInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SUBMIT_INFO
       pNext:PVkVoid; //< Pointer to next structure
       waitSemaphoreCount:TVkUInt32;
       pWaitSemaphores:PVkSemaphore;
       pWaitDstStageMask:PVkPipelineStageFlags;
       commandBufferCount:TVkUInt32;
       pCommandBuffers:PVkCommandBuffer;
       signalSemaphoreCount:TVkUInt32;
       pSignalSemaphores:PVkSemaphore;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pWaitSemaphoreCount:TVkUInt32;
                          const pPWaitSemaphores:PVkSemaphore;
                          const pPWaitDstStageMask:PVkPipelineStageFlags;
                          const pCommandBufferCount:TVkUInt32;
                          const pPCommandBuffers:PVkCommandBuffer;
                          const pSignalSemaphoreCount:TVkUInt32;
                          const pPSignalSemaphores:PVkSemaphore);
{$endif}
     end;

     PPVkDisplayPropertiesKHR=^PVkDisplayPropertiesKHR;
     PVkDisplayPropertiesKHR=^TVkDisplayPropertiesKHR;
     TVkDisplayPropertiesKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       display:TVkDisplayKHR; //< Handle of the display object
       displayName:PVkChar; //< Name of the display
       physicalDimensions:TVkExtent2D; //< In millimeters?
       physicalResolution:TVkExtent2D; //< Max resolution for CRT?
       supportedTransforms:TVkSurfaceTransformFlagsKHR; //< one or more bits from VkSurfaceTransformFlagsKHR
       planeReorderPossible:TVkBool32; //< VK_TRUE if the overlay plane's z-order can be changed on this display.
       persistentContent:TVkBool32; //< VK_TRUE if this is a "smart" display that supports self-refresh/internal buffering.
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pDisplay:TVkDisplayKHR; //< Handle of the display object
                          const pDisplayName:PVkChar; //< Name of the display
                          const pPhysicalDimensions:TVkExtent2D; //< In millimeters?
                          const pPhysicalResolution:TVkExtent2D; //< Max resolution for CRT?
                          const pSupportedTransforms:TVkSurfaceTransformFlagsKHR; //< one or more bits from VkSurfaceTransformFlagsKHR
                          const pPlaneReorderPossible:TVkBool32; //< VK_TRUE if the overlay plane's z-order can be changed on this display.
                          const pPersistentContent:TVkBool32); //< VK_TRUE if this is a "smart" display that supports self-refresh/internal buffering.
{$endif}
     end;

     PPVkDisplayPlanePropertiesKHR=^PVkDisplayPlanePropertiesKHR;
     PVkDisplayPlanePropertiesKHR=^TVkDisplayPlanePropertiesKHR;
     TVkDisplayPlanePropertiesKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       currentDisplay:TVkDisplayKHR; //< Display the plane is currently associated with. Will be VK_NULL_HANDLE if the plane is not in use.
       currentStackIndex:TVkUInt32; //< Current z-order of the plane.
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pCurrentDisplay:TVkDisplayKHR; //< Display the plane is currently associated with. Will be VK_NULL_HANDLE if the plane is not in use.
                          const pCurrentStackIndex:TVkUInt32); //< Current z-order of the plane.
{$endif}
     end;

     PPVkDisplayModeParametersKHR=^PVkDisplayModeParametersKHR;
     PVkDisplayModeParametersKHR=^TVkDisplayModeParametersKHR;
     TVkDisplayModeParametersKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       visibleRegion:TVkExtent2D; //< Visible scanout region.
       refreshRate:TVkUInt32; //< Number of times per second the display is updated.
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pVisibleRegion:TVkExtent2D; //< Visible scanout region.
                          const pRefreshRate:TVkUInt32); //< Number of times per second the display is updated.
{$endif}
     end;

     PPVkDisplayModePropertiesKHR=^PVkDisplayModePropertiesKHR;
     PVkDisplayModePropertiesKHR=^TVkDisplayModePropertiesKHR;
     TVkDisplayModePropertiesKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       displayMode:TVkDisplayModeKHR; //< Handle of this display mode.
       parameters:TVkDisplayModeParametersKHR; //< The parameters this mode uses.
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pDisplayMode:TVkDisplayModeKHR; //< Handle of this display mode.
                          const pParameters:TVkDisplayModeParametersKHR); //< The parameters this mode uses.
{$endif}
     end;

     PPVkDisplayModeCreateInfoKHR=^PVkDisplayModeCreateInfoKHR;
     PVkDisplayModeCreateInfoKHR=^TVkDisplayModeCreateInfoKHR;
     TVkDisplayModeCreateInfoKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkDisplayModeCreateFlagsKHR; //< Reserved
       parameters:TVkDisplayModeParametersKHR; //< The parameters this mode uses.
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkDisplayModeCreateFlagsKHR; //< Reserved
                          const pParameters:TVkDisplayModeParametersKHR); //< The parameters this mode uses.
{$endif}
     end;

     PPVkDisplayPlaneCapabilitiesKHR=^PVkDisplayPlaneCapabilitiesKHR;
     PVkDisplayPlaneCapabilitiesKHR=^TVkDisplayPlaneCapabilitiesKHR;
     TVkDisplayPlaneCapabilitiesKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       supportedAlpha:TVkDisplayPlaneAlphaFlagsKHR; //< Types of alpha blending supported, if any.
       minSrcPosition:TVkOffset2D; //< Does the plane have any position and extent restrictions?
       maxSrcPosition:TVkOffset2D;
       minSrcExtent:TVkExtent2D;
       maxSrcExtent:TVkExtent2D;
       minDstPosition:TVkOffset2D;
       maxDstPosition:TVkOffset2D;
       minDstExtent:TVkExtent2D;
       maxDstExtent:TVkExtent2D;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pSupportedAlpha:TVkDisplayPlaneAlphaFlagsKHR; //< Types of alpha blending supported, if any.
                          const pMinSrcPosition:TVkOffset2D; //< Does the plane have any position and extent restrictions?
                          const pMaxSrcPosition:TVkOffset2D;
                          const pMinSrcExtent:TVkExtent2D;
                          const pMaxSrcExtent:TVkExtent2D;
                          const pMinDstPosition:TVkOffset2D;
                          const pMaxDstPosition:TVkOffset2D;
                          const pMinDstExtent:TVkExtent2D;
                          const pMaxDstExtent:TVkExtent2D);
{$endif}
     end;

     PPVkDisplaySurfaceCreateInfoKHR=^PVkDisplaySurfaceCreateInfoKHR;
     PVkDisplaySurfaceCreateInfoKHR=^TVkDisplaySurfaceCreateInfoKHR;
     TVkDisplaySurfaceCreateInfoKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkDisplaySurfaceCreateFlagsKHR; //< Reserved
       displayMode:TVkDisplayModeKHR; //< The mode to use when displaying this surface
       planeIndex:TVkUInt32; //< The plane on which this surface appears. Must be between 0 and the value returned by vkGetPhysicalDeviceDisplayPlanePropertiesKHR() in pPropertyCount.
       planeStackIndex:TVkUInt32; //< The z-order of the plane.
       transform:TVkSurfaceTransformFlagBitsKHR; //< Transform to apply to the images as part of the scanout operation
       globalAlpha:TVkFloat; //< Global alpha value. Must be between 0 and 1, inclusive. Ignored if alphaMode is not VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR
       alphaMode:TVkDisplayPlaneAlphaFlagBitsKHR; //< What type of alpha blending to use. Must be a bit from vkGetDisplayPlanePropertiesKHR::supportedAlpha.
       imageExtent:TVkExtent2D; //< size of the images to use with this surface
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkDisplaySurfaceCreateFlagsKHR; //< Reserved
                          const pDisplayMode:TVkDisplayModeKHR; //< The mode to use when displaying this surface
                          const pPlaneIndex:TVkUInt32; //< The plane on which this surface appears. Must be between 0 and the value returned by vkGetPhysicalDeviceDisplayPlanePropertiesKHR() in pPropertyCount.
                          const pPlaneStackIndex:TVkUInt32; //< The z-order of the plane.
                          const pTransform:TVkSurfaceTransformFlagBitsKHR; //< Transform to apply to the images as part of the scanout operation
                          const pGlobalAlpha:TVkFloat; //< Global alpha value. Must be between 0 and 1, inclusive. Ignored if alphaMode is not VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR
                          const pAlphaMode:TVkDisplayPlaneAlphaFlagBitsKHR; //< What type of alpha blending to use. Must be a bit from vkGetDisplayPlanePropertiesKHR::supportedAlpha.
                          const pImageExtent:TVkExtent2D); //< size of the images to use with this surface
{$endif}
     end;

     PPVkDisplayPresentInfoKHR=^PVkDisplayPresentInfoKHR;
     PVkDisplayPresentInfoKHR=^TVkDisplayPresentInfoKHR;
     TVkDisplayPresentInfoKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR
       pNext:PVkVoid; //< Pointer to next structure
       srcRect:TVkRect2D; //< Rectangle within the presentable image to read pixel data from when presenting to the display.
       dstRect:TVkRect2D; //< Rectangle within the current display mode's visible region to display srcRectangle in.
       persistent:TVkBool32; //< For smart displays, use buffered mode. If the display properties member "persistentMode" is VK_FALSE, this member must always be VK_FALSE.
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pSrcRect:TVkRect2D; //< Rectangle within the presentable image to read pixel data from when presenting to the display.
                          const pDstRect:TVkRect2D; //< Rectangle within the current display mode's visible region to display srcRectangle in.
                          const pPersistent:TVkBool32); //< For smart displays, use buffered mode. If the display properties member "persistentMode" is VK_FALSE, this member must always be VK_FALSE.
{$endif}
     end;

     PPVkSurfaceCapabilitiesKHR=^PVkSurfaceCapabilitiesKHR;
     PVkSurfaceCapabilitiesKHR=^TVkSurfaceCapabilitiesKHR;
     TVkSurfaceCapabilitiesKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       minImageCount:TVkUInt32; //< Supported minimum number of images for the surface
       maxImageCount:TVkUInt32; //< Supported maximum number of images for the surface, 0 for unlimited
       currentExtent:TVkExtent2D; //< Current image width and height for the surface, (0, 0) if undefined
       minImageExtent:TVkExtent2D; //< Supported minimum image width and height for the surface
       maxImageExtent:TVkExtent2D; //< Supported maximum image width and height for the surface
       maxImageArrayLayers:TVkUInt32; //< Supported maximum number of image layers for the surface
       supportedTransforms:TVkSurfaceTransformFlagsKHR; //< 1 or more bits representing the transforms supported
       currentTransform:TVkSurfaceTransformFlagBitsKHR; //< The surface's current transform relative to the device's natural orientation
       supportedCompositeAlpha:TVkCompositeAlphaFlagsKHR; //< 1 or more bits representing the alpha compositing modes supported
       supportedUsageFlags:TVkImageUsageFlags; //< Supported image usage flags for the surface
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pMinImageCount:TVkUInt32; //< Supported minimum number of images for the surface
                          const pMaxImageCount:TVkUInt32; //< Supported maximum number of images for the surface, 0 for unlimited
                          const pCurrentExtent:TVkExtent2D; //< Current image width and height for the surface, (0, 0) if undefined
                          const pMinImageExtent:TVkExtent2D; //< Supported minimum image width and height for the surface
                          const pMaxImageExtent:TVkExtent2D; //< Supported maximum image width and height for the surface
                          const pMaxImageArrayLayers:TVkUInt32; //< Supported maximum number of image layers for the surface
                          const pSupportedTransforms:TVkSurfaceTransformFlagsKHR; //< 1 or more bits representing the transforms supported
                          const pCurrentTransform:TVkSurfaceTransformFlagBitsKHR; //< The surface's current transform relative to the device's natural orientation
                          const pSupportedCompositeAlpha:TVkCompositeAlphaFlagsKHR; //< 1 or more bits representing the alpha compositing modes supported
                          const pSupportedUsageFlags:TVkImageUsageFlags); //< Supported image usage flags for the surface
{$endif}
     end;

{$ifdef Android}
     PPVkAndroidSurfaceCreateInfoKHR=^PVkAndroidSurfaceCreateInfoKHR;
     PVkAndroidSurfaceCreateInfoKHR=^TVkAndroidSurfaceCreateInfoKHR;
     TVkAndroidSurfaceCreateInfoKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkAndroidSurfaceCreateFlagsKHR; //< Reserved
       window:PANativeWindow;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkAndroidSurfaceCreateFlagsKHR; //< Reserved
                          const pWindow:PANativeWindow);
{$endif}
     end;
{$endif}

{$ifdef Mir}
     PPVkMirSurfaceCreateInfoKHR=^PVkMirSurfaceCreateInfoKHR;
     PVkMirSurfaceCreateInfoKHR=^TVkMirSurfaceCreateInfoKHR;
     TVkMirSurfaceCreateInfoKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkMirSurfaceCreateFlagsKHR; //< Reserved
       connection:PMirConnection;
       mirSurface:PMirSurface;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkMirSurfaceCreateFlagsKHR; //< Reserved
                          const pConnection:PMirConnection;
                          const pMirSurface:PMirSurface);
{$endif}
     end;
{$endif}

     PPVkViSurfaceCreateInfoNN=^PVkViSurfaceCreateInfoNN;
     PVkViSurfaceCreateInfoNN=^TVkViSurfaceCreateInfoNN;
     TVkViSurfaceCreateInfoNN=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkViSurfaceCreateFlagsNN; //< Reserved
       window:PVkVoid;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkViSurfaceCreateFlagsNN; //< Reserved
                          const pWindow:PVkVoid);
{$endif}
     end;

{$ifdef Wayland}
     PPVkWaylandSurfaceCreateInfoKHR=^PVkWaylandSurfaceCreateInfoKHR;
     PVkWaylandSurfaceCreateInfoKHR=^TVkWaylandSurfaceCreateInfoKHR;
     TVkWaylandSurfaceCreateInfoKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkWaylandSurfaceCreateFlagsKHR; //< Reserved
       display:Pwl_display;
       surface:Pwl_surface;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkWaylandSurfaceCreateFlagsKHR; //< Reserved
                          const pDisplay:Pwl_display;
                          const pSurface:Pwl_surface);
{$endif}
     end;
{$endif}

{$ifdef Windows}
     PPVkWin32SurfaceCreateInfoKHR=^PVkWin32SurfaceCreateInfoKHR;
     PVkWin32SurfaceCreateInfoKHR=^TVkWin32SurfaceCreateInfoKHR;
     TVkWin32SurfaceCreateInfoKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkWin32SurfaceCreateFlagsKHR; //< Reserved
       hinstance_:TVkHINSTANCE;
       hwnd_:TVkHWND;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkWin32SurfaceCreateFlagsKHR; //< Reserved
                          const pHinstance_:TVkHINSTANCE;
                          const pHwnd_:TVkHWND);
{$endif}
     end;
{$endif}

{$ifdef X11}
     PPVkXlibSurfaceCreateInfoKHR=^PVkXlibSurfaceCreateInfoKHR;
     PVkXlibSurfaceCreateInfoKHR=^TVkXlibSurfaceCreateInfoKHR;
     TVkXlibSurfaceCreateInfoKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkXlibSurfaceCreateFlagsKHR; //< Reserved
       dpy:PDisplay;
       window:TWindow;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkXlibSurfaceCreateFlagsKHR; //< Reserved
                          const pDpy:PDisplay;
                          const pWindow:TWindow);
{$endif}
     end;
{$endif}

{$ifdef XCB}
     PPVkXcbSurfaceCreateInfoKHR=^PVkXcbSurfaceCreateInfoKHR;
     PVkXcbSurfaceCreateInfoKHR=^TVkXcbSurfaceCreateInfoKHR;
     TVkXcbSurfaceCreateInfoKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkXcbSurfaceCreateFlagsKHR; //< Reserved
       connection:Pxcb_connection;
       window:Txcb_window;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkXcbSurfaceCreateFlagsKHR; //< Reserved
                          const pConnection:Pxcb_connection;
                          const pWindow:Txcb_window);
{$endif}
     end;
{$endif}

     PPVkSurfaceFormatKHR=^PVkSurfaceFormatKHR;
     PVkSurfaceFormatKHR=^TVkSurfaceFormatKHR;
     TVkSurfaceFormatKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       format:TVkFormat; //< Supported pair of rendering format
       colorSpace:TVkColorSpaceKHR; //< and color space for the surface
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFormat:TVkFormat; //< Supported pair of rendering format
                          const pColorSpace:TVkColorSpaceKHR); //< and color space for the surface
{$endif}
     end;

     PPVkSwapchainCreateInfoKHR=^PVkSwapchainCreateInfoKHR;
     PVkSwapchainCreateInfoKHR=^TVkSwapchainCreateInfoKHR;
     TVkSwapchainCreateInfoKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkSwapchainCreateFlagsKHR; //< Reserved
       surface:TVkSurfaceKHR; //< The swapchain's target surface
       minImageCount:TVkUInt32; //< Minimum number of presentation images the application needs
       imageFormat:TVkFormat; //< Format of the presentation images
       imageColorSpace:TVkColorSpaceKHR; //< Colorspace of the presentation images
       imageExtent:TVkExtent2D; //< Dimensions of the presentation images
       imageArrayLayers:TVkUInt32; //< Determines the number of views for multiview/stereo presentation
       imageUsage:TVkImageUsageFlags; //< Bits indicating how the presentation images will be used
       imageSharingMode:TVkSharingMode; //< Sharing mode used for the presentation images
       queueFamilyIndexCount:TVkUInt32; //< Number of queue families having access to the images in case of concurrent sharing mode
       pQueueFamilyIndices:PVkUInt32; //< Array of queue family indices having access to the images in case of concurrent sharing mode
       preTransform:TVkSurfaceTransformFlagBitsKHR; //< The transform, relative to the device's natural orientation, applied to the image content prior to presentation
       compositeAlpha:TVkCompositeAlphaFlagBitsKHR; //< The alpha blending mode used when compositing this surface with other surfaces in the window system
       presentMode:TVkPresentModeKHR; //< Which presentation mode to use for presents on this swap chain
       clipped:TVkBool32; //< Specifies whether presentable images may be affected by window clip regions
       oldSwapchain:TVkSwapchainKHR; //< Existing swap chain to replace, if any
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkSwapchainCreateFlagsKHR; //< Reserved
                          const pSurface:TVkSurfaceKHR; //< The swapchain's target surface
                          const pMinImageCount:TVkUInt32; //< Minimum number of presentation images the application needs
                          const pImageFormat:TVkFormat; //< Format of the presentation images
                          const pImageColorSpace:TVkColorSpaceKHR; //< Colorspace of the presentation images
                          const pImageExtent:TVkExtent2D; //< Dimensions of the presentation images
                          const pImageArrayLayers:TVkUInt32; //< Determines the number of views for multiview/stereo presentation
                          const pImageUsage:TVkImageUsageFlags; //< Bits indicating how the presentation images will be used
                          const pImageSharingMode:TVkSharingMode; //< Sharing mode used for the presentation images
                          const pQueueFamilyIndexCount:TVkUInt32; //< Number of queue families having access to the images in case of concurrent sharing mode
                          const pPQueueFamilyIndices:PVkUInt32; //< Array of queue family indices having access to the images in case of concurrent sharing mode
                          const pPreTransform:TVkSurfaceTransformFlagBitsKHR; //< The transform, relative to the device's natural orientation, applied to the image content prior to presentation
                          const pCompositeAlpha:TVkCompositeAlphaFlagBitsKHR; //< The alpha blending mode used when compositing this surface with other surfaces in the window system
                          const pPresentMode:TVkPresentModeKHR; //< Which presentation mode to use for presents on this swap chain
                          const pClipped:TVkBool32; //< Specifies whether presentable images may be affected by window clip regions
                          const pOldSwapchain:TVkSwapchainKHR); //< Existing swap chain to replace, if any
{$endif}
     end;

     PPVkPresentInfoKHR=^PVkPresentInfoKHR;
     PVkPresentInfoKHR=^TVkPresentInfoKHR;
     TVkPresentInfoKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
       pNext:PVkVoid; //< Pointer to next structure
       waitSemaphoreCount:TVkUInt32; //< Number of semaphores to wait for before presenting
       pWaitSemaphores:PVkSemaphore; //< Semaphores to wait for before presenting
       swapchainCount:TVkUInt32; //< Number of swap chains to present in this call
       pSwapchains:PVkSwapchainKHR; //< Swapchains to present an image from
       pImageIndices:PVkUInt32; //< Indices of which swapchain images to present
       pResults:PVkResult; //< Optional (i.e. if non-NULL) VkResult for each swapchain
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pWaitSemaphoreCount:TVkUInt32; //< Number of semaphores to wait for before presenting
                          const pPWaitSemaphores:PVkSemaphore; //< Semaphores to wait for before presenting
                          const pSwapchainCount:TVkUInt32; //< Number of swap chains to present in this call
                          const pPSwapchains:PVkSwapchainKHR; //< Swapchains to present an image from
                          const pPImageIndices:PVkUInt32; //< Indices of which swapchain images to present
                          const pPResults:PVkResult); //< Optional (i.e. if non-NULL) VkResult for each swapchain
{$endif}
     end;

     PPVkDebugReportCallbackCreateInfoEXT=^PVkDebugReportCallbackCreateInfoEXT;
     PVkDebugReportCallbackCreateInfoEXT=^TVkDebugReportCallbackCreateInfoEXT;
     TVkDebugReportCallbackCreateInfoEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
       pNext:PVkVoid; //< Pointer to next structure
       flags:TVkDebugReportFlagsEXT; //< Indicates which events call this callback
       pfnCallback:TPFN_vkDebugReportCallbackEXT; //< Function pointer of a callback function
       pUserData:PVkVoid; //< User data provided to callback function
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFlags:TVkDebugReportFlagsEXT; //< Indicates which events call this callback
                          const pPfnCallback:TPFN_vkDebugReportCallbackEXT; //< Function pointer of a callback function
                          const pPUserData:PVkVoid); //< User data provided to callback function
{$endif}
     end;

     PPVkValidationFlagsEXT=^PVkValidationFlagsEXT;
     PVkValidationFlagsEXT=^TVkValidationFlagsEXT;
     TVkValidationFlagsEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT
       pNext:PVkVoid; //< Pointer to next structure
       disabledValidationCheckCount:TVkUInt32; //< Number of validation checks to disable
       pDisabledValidationChecks:PVkValidationCheckEXT; //< Validation checks to disable
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pDisabledValidationCheckCount:TVkUInt32; //< Number of validation checks to disable
                          const pPDisabledValidationChecks:PVkValidationCheckEXT); //< Validation checks to disable
{$endif}
     end;

     PPVkPipelineRasterizationStateRasterizationOrderAMD=^PVkPipelineRasterizationStateRasterizationOrderAMD;
     PVkPipelineRasterizationStateRasterizationOrderAMD=^TVkPipelineRasterizationStateRasterizationOrderAMD;
     TVkPipelineRasterizationStateRasterizationOrderAMD=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD
       pNext:PVkVoid; //< Pointer to next structure
       rasterizationOrder:TVkRasterizationOrderAMD; //< Rasterization order to use for the pipeline
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pRasterizationOrder:TVkRasterizationOrderAMD); //< Rasterization order to use for the pipeline
{$endif}
     end;

     PPVkDebugMarkerObjectNameInfoEXT=^PVkDebugMarkerObjectNameInfoEXT;
     PVkDebugMarkerObjectNameInfoEXT=^TVkDebugMarkerObjectNameInfoEXT;
     TVkDebugMarkerObjectNameInfoEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT
       pNext:PVkVoid; //< Pointer to next structure
       objectType:TVkDebugReportObjectTypeEXT; //< The type of the object
       object_:TVkUInt64; //< The handle of the object, cast to uint64_t
       pObjectName:PVkChar; //< Name to apply to the object
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pObjectType:TVkDebugReportObjectTypeEXT; //< The type of the object
                          const pObject_:TVkUInt64; //< The handle of the object, cast to uint64_t
                          const pPObjectName:PVkChar); //< Name to apply to the object
{$endif}
     end;

     PPVkDebugMarkerObjectTagInfoEXT=^PVkDebugMarkerObjectTagInfoEXT;
     PVkDebugMarkerObjectTagInfoEXT=^TVkDebugMarkerObjectTagInfoEXT;
     TVkDebugMarkerObjectTagInfoEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT
       pNext:PVkVoid; //< Pointer to next structure
       objectType:TVkDebugReportObjectTypeEXT; //< The type of the object
       object_:TVkUInt64; //< The handle of the object, cast to uint64_t
       tagName:TVkUInt64; //< The name of the tag to set on the object
       tagSize:TVkSize; //< The length in bytes of the tag data
       pTag:PVkVoid; //< Tag data to attach to the object
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pObjectType:TVkDebugReportObjectTypeEXT; //< The type of the object
                          const pObject_:TVkUInt64; //< The handle of the object, cast to uint64_t
                          const pTagName:TVkUInt64; //< The name of the tag to set on the object
                          const pTagSize:TVkSize; //< The length in bytes of the tag data
                          const pPTag:PVkVoid); //< Tag data to attach to the object
{$endif}
     end;

     PPVkDebugMarkerMarkerInfoEXT=^PVkDebugMarkerMarkerInfoEXT;
     PVkDebugMarkerMarkerInfoEXT=^TVkDebugMarkerMarkerInfoEXT;
     TVkDebugMarkerMarkerInfoEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT
       pNext:PVkVoid; //< Pointer to next structure
       pMarkerName:PVkChar; //< Name of the debug marker
       color:array[0..3] of TVkFloat; //< Optional color for debug marker
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pPMarkerName:PVkChar; //< Name of the debug marker
                          const pColor:array of TVkFloat); //< Optional color for debug marker
{$endif}
     end;

     PPVkDedicatedAllocationImageCreateInfoNV=^PVkDedicatedAllocationImageCreateInfoNV;
     PVkDedicatedAllocationImageCreateInfoNV=^TVkDedicatedAllocationImageCreateInfoNV;
     TVkDedicatedAllocationImageCreateInfoNV=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
       pNext:PVkVoid; //< Pointer to next structure
       dedicatedAllocation:TVkBool32; //< Whether this image uses a dedicated allocation
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pDedicatedAllocation:TVkBool32); //< Whether this image uses a dedicated allocation
{$endif}
     end;

     PPVkDedicatedAllocationBufferCreateInfoNV=^PVkDedicatedAllocationBufferCreateInfoNV;
     PVkDedicatedAllocationBufferCreateInfoNV=^TVkDedicatedAllocationBufferCreateInfoNV;
     TVkDedicatedAllocationBufferCreateInfoNV=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
       pNext:PVkVoid; //< Pointer to next structure
       dedicatedAllocation:TVkBool32; //< Whether this buffer uses a dedicated allocation
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pDedicatedAllocation:TVkBool32); //< Whether this buffer uses a dedicated allocation
{$endif}
     end;

     PPVkDedicatedAllocationMemoryAllocateInfoNV=^PVkDedicatedAllocationMemoryAllocateInfoNV;
     PVkDedicatedAllocationMemoryAllocateInfoNV=^TVkDedicatedAllocationMemoryAllocateInfoNV;
     TVkDedicatedAllocationMemoryAllocateInfoNV=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
       pNext:PVkVoid; //< Pointer to next structure
       image:TVkImage; //< Image that this allocation will be bound to
       buffer:TVkBuffer; //< Buffer that this allocation will be bound to
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pImage:TVkImage; //< Image that this allocation will be bound to
                          const pBuffer:TVkBuffer); //< Buffer that this allocation will be bound to
{$endif}
     end;

     PPVkExternalImageFormatPropertiesNV=^PVkExternalImageFormatPropertiesNV;
     PVkExternalImageFormatPropertiesNV=^TVkExternalImageFormatPropertiesNV;
     TVkExternalImageFormatPropertiesNV=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       imageFormatProperties:TVkImageFormatProperties;
       externalMemoryFeatures:TVkExternalMemoryFeatureFlagsNV;
       exportFromImportedHandleTypes:TVkExternalMemoryHandleTypeFlagsNV;
       compatibleHandleTypes:TVkExternalMemoryHandleTypeFlagsNV;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pImageFormatProperties:TVkImageFormatProperties;
                          const pExternalMemoryFeatures:TVkExternalMemoryFeatureFlagsNV;
                          const pExportFromImportedHandleTypes:TVkExternalMemoryHandleTypeFlagsNV;
                          const pCompatibleHandleTypes:TVkExternalMemoryHandleTypeFlagsNV);
{$endif}
     end;

     PPVkExternalMemoryImageCreateInfoNV=^PVkExternalMemoryImageCreateInfoNV;
     PVkExternalMemoryImageCreateInfoNV=^TVkExternalMemoryImageCreateInfoNV;
     TVkExternalMemoryImageCreateInfoNV=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV
       pNext:PVkVoid;
       handleTypes:TVkExternalMemoryHandleTypeFlagsNV;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pHandleTypes:TVkExternalMemoryHandleTypeFlagsNV);
{$endif}
     end;

     PPVkExportMemoryAllocateInfoNV=^PVkExportMemoryAllocateInfoNV;
     PVkExportMemoryAllocateInfoNV=^TVkExportMemoryAllocateInfoNV;
     TVkExportMemoryAllocateInfoNV=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV
       pNext:PVkVoid;
       handleTypes:TVkExternalMemoryHandleTypeFlagsNV;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pHandleTypes:TVkExternalMemoryHandleTypeFlagsNV);
{$endif}
     end;

     PPVkImportMemoryWin32HandleInfoNV=^PVkImportMemoryWin32HandleInfoNV;
     PVkImportMemoryWin32HandleInfoNV=^TVkImportMemoryWin32HandleInfoNV;
     TVkImportMemoryWin32HandleInfoNV=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV
       pNext:PVkVoid;
       handleType:TVkExternalMemoryHandleTypeFlagsNV;
       handle:THANDLE;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pHandleType:TVkExternalMemoryHandleTypeFlagsNV;
                          const pHandle:THANDLE);
{$endif}
     end;

{$ifdef Windows}
     PPVkExportMemoryWin32HandleInfoNV=^PVkExportMemoryWin32HandleInfoNV;
     PVkExportMemoryWin32HandleInfoNV=^TVkExportMemoryWin32HandleInfoNV;
     TVkExportMemoryWin32HandleInfoNV=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV
       pNext:PVkVoid;
       pAttributes:PSecurityAttributes;
       dwAccess:TVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pPAttributes:PSecurityAttributes;
                          const pDwAccess:TVkUInt32);
{$endif}
     end;
{$endif}

     PPVkWin32KeyedMutexAcquireReleaseInfoNV=^PVkWin32KeyedMutexAcquireReleaseInfoNV;
     PVkWin32KeyedMutexAcquireReleaseInfoNV=^TVkWin32KeyedMutexAcquireReleaseInfoNV;
     TVkWin32KeyedMutexAcquireReleaseInfoNV=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV
       pNext:PVkVoid;
       acquireCount:TVkUInt32;
       pAcquireSyncs:PVkDeviceMemory;
       pAcquireKeys:PVkUInt64;
       pAcquireTimeoutMilliseconds:PVkUInt32;
       releaseCount:TVkUInt32;
       pReleaseSyncs:PVkDeviceMemory;
       pReleaseKeys:PVkUInt64;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pAcquireCount:TVkUInt32;
                          const pPAcquireSyncs:PVkDeviceMemory;
                          const pPAcquireKeys:PVkUInt64;
                          const pPAcquireTimeoutMilliseconds:PVkUInt32;
                          const pReleaseCount:TVkUInt32;
                          const pPReleaseSyncs:PVkDeviceMemory;
                          const pPReleaseKeys:PVkUInt64);
{$endif}
     end;

     PPVkDeviceGeneratedCommandsFeaturesNVX=^PVkDeviceGeneratedCommandsFeaturesNVX;
     PVkDeviceGeneratedCommandsFeaturesNVX=^TVkDeviceGeneratedCommandsFeaturesNVX;
     TVkDeviceGeneratedCommandsFeaturesNVX=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX
       pNext:PVkVoid;
       computeBindingPointSupport:TVkBool32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pComputeBindingPointSupport:TVkBool32);
{$endif}
     end;

     PPVkDeviceGeneratedCommandsLimitsNVX=^PVkDeviceGeneratedCommandsLimitsNVX;
     PVkDeviceGeneratedCommandsLimitsNVX=^TVkDeviceGeneratedCommandsLimitsNVX;
     TVkDeviceGeneratedCommandsLimitsNVX=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX
       pNext:PVkVoid;
       maxIndirectCommandsLayoutTokenCount:TVkUInt32;
       maxObjectEntryCounts:TVkUInt32;
       minSequenceCountBufferOffsetAlignment:TVkUInt32;
       minSequenceIndexBufferOffsetAlignment:TVkUInt32;
       minCommandsTokenBufferOffsetAlignment:TVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pMaxIndirectCommandsLayoutTokenCount:TVkUInt32;
                          const pMaxObjectEntryCounts:TVkUInt32;
                          const pMinSequenceCountBufferOffsetAlignment:TVkUInt32;
                          const pMinSequenceIndexBufferOffsetAlignment:TVkUInt32;
                          const pMinCommandsTokenBufferOffsetAlignment:TVkUInt32);
{$endif}
     end;

     PPVkIndirectCommandsTokenNVX=^PVkIndirectCommandsTokenNVX;
     PVkIndirectCommandsTokenNVX=^TVkIndirectCommandsTokenNVX;
     TVkIndirectCommandsTokenNVX=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       tokenType:TVkIndirectCommandsTokenTypeNVX;
       buffer:TVkBuffer; //< buffer containing tableEntries and additional data for indirectCommands
       offset:TVkDeviceSize; //< offset from the base address of the buffer
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pTokenType:TVkIndirectCommandsTokenTypeNVX;
                          const pBuffer:TVkBuffer; //< buffer containing tableEntries and additional data for indirectCommands
                          const pOffset:TVkDeviceSize); //< offset from the base address of the buffer
{$endif}
     end;

     PPVkIndirectCommandsLayoutTokenNVX=^PVkIndirectCommandsLayoutTokenNVX;
     PVkIndirectCommandsLayoutTokenNVX=^TVkIndirectCommandsLayoutTokenNVX;
     TVkIndirectCommandsLayoutTokenNVX=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       tokenType:TVkIndirectCommandsTokenTypeNVX;
       bindingUnit:TVkUInt32; //< Binding unit for vertex attribute / descriptor set, offset for pushconstants
       dynamicCount:TVkUInt32; //< Number of variable dynamic values for descriptor set / push constants
       divisor:TVkUInt32; //< Rate the which the array is advanced per element (must be power of 2, minimum 1)
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pTokenType:TVkIndirectCommandsTokenTypeNVX;
                          const pBindingUnit:TVkUInt32; //< Binding unit for vertex attribute / descriptor set, offset for pushconstants
                          const pDynamicCount:TVkUInt32; //< Number of variable dynamic values for descriptor set / push constants
                          const pDivisor:TVkUInt32); //< Rate the which the array is advanced per element (must be power of 2, minimum 1)
{$endif}
     end;

     PPVkIndirectCommandsLayoutCreateInfoNVX=^PVkIndirectCommandsLayoutCreateInfoNVX;
     PVkIndirectCommandsLayoutCreateInfoNVX=^TVkIndirectCommandsLayoutCreateInfoNVX;
     TVkIndirectCommandsLayoutCreateInfoNVX=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX
       pNext:PVkVoid;
       pipelineBindPoint:TVkPipelineBindPoint;
       flags:TVkIndirectCommandsLayoutUsageFlagsNVX;
       tokenCount:TVkUInt32;
       pTokens:PVkIndirectCommandsLayoutTokenNVX;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pPipelineBindPoint:TVkPipelineBindPoint;
                          const pFlags:TVkIndirectCommandsLayoutUsageFlagsNVX;
                          const pTokenCount:TVkUInt32;
                          const pPTokens:PVkIndirectCommandsLayoutTokenNVX);
{$endif}
     end;

     PPVkCmdProcessCommandsInfoNVX=^PVkCmdProcessCommandsInfoNVX;
     PVkCmdProcessCommandsInfoNVX=^TVkCmdProcessCommandsInfoNVX;
     TVkCmdProcessCommandsInfoNVX=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX
       pNext:PVkVoid;
       objectTable:TVkObjectTableNVX;
       indirectCommandsLayout:TVkIndirectCommandsLayoutNVX;
       indirectCommandsTokenCount:TVkUInt32;
       pIndirectCommandsTokens:PVkIndirectCommandsTokenNVX;
       maxSequencesCount:TVkUInt32;
       targetCommandBuffer:TVkCommandBuffer;
       sequencesCountBuffer:TVkBuffer;
       sequencesCountOffset:TVkDeviceSize;
       sequencesIndexBuffer:TVkBuffer;
       sequencesIndexOffset:TVkDeviceSize;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pObjectTable:TVkObjectTableNVX;
                          const pIndirectCommandsLayout:TVkIndirectCommandsLayoutNVX;
                          const pIndirectCommandsTokenCount:TVkUInt32;
                          const pPIndirectCommandsTokens:PVkIndirectCommandsTokenNVX;
                          const pMaxSequencesCount:TVkUInt32;
                          const pTargetCommandBuffer:TVkCommandBuffer;
                          const pSequencesCountBuffer:TVkBuffer;
                          const pSequencesCountOffset:TVkDeviceSize;
                          const pSequencesIndexBuffer:TVkBuffer;
                          const pSequencesIndexOffset:TVkDeviceSize);
{$endif}
     end;

     PPVkCmdReserveSpaceForCommandsInfoNVX=^PVkCmdReserveSpaceForCommandsInfoNVX;
     PVkCmdReserveSpaceForCommandsInfoNVX=^TVkCmdReserveSpaceForCommandsInfoNVX;
     TVkCmdReserveSpaceForCommandsInfoNVX=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX
       pNext:PVkVoid;
       objectTable:TVkObjectTableNVX;
       indirectCommandsLayout:TVkIndirectCommandsLayoutNVX;
       maxSequencesCount:TVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pObjectTable:TVkObjectTableNVX;
                          const pIndirectCommandsLayout:TVkIndirectCommandsLayoutNVX;
                          const pMaxSequencesCount:TVkUInt32);
{$endif}
     end;

     PPVkObjectTableCreateInfoNVX=^PVkObjectTableCreateInfoNVX;
     PVkObjectTableCreateInfoNVX=^TVkObjectTableCreateInfoNVX;
     TVkObjectTableCreateInfoNVX=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX
       pNext:PVkVoid;
       objectCount:TVkUInt32;
       pObjectEntryTypes:PVkObjectEntryTypeNVX;
       pObjectEntryCounts:PVkUInt32;
       pObjectEntryUsageFlags:PVkObjectEntryUsageFlagsNVX;
       maxUniformBuffersPerDescriptor:TVkUInt32;
       maxStorageBuffersPerDescriptor:TVkUInt32;
       maxStorageImagesPerDescriptor:TVkUInt32;
       maxSampledImagesPerDescriptor:TVkUInt32;
       maxPipelineLayouts:TVkUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pObjectCount:TVkUInt32;
                          const pPObjectEntryTypes:PVkObjectEntryTypeNVX;
                          const pPObjectEntryCounts:PVkUInt32;
                          const pPObjectEntryUsageFlags:PVkObjectEntryUsageFlagsNVX;
                          const pMaxUniformBuffersPerDescriptor:TVkUInt32;
                          const pMaxStorageBuffersPerDescriptor:TVkUInt32;
                          const pMaxStorageImagesPerDescriptor:TVkUInt32;
                          const pMaxSampledImagesPerDescriptor:TVkUInt32;
                          const pMaxPipelineLayouts:TVkUInt32);
{$endif}
     end;

     PPVkObjectTableEntryNVX=^PVkObjectTableEntryNVX;
     PVkObjectTableEntryNVX=^TVkObjectTableEntryNVX;
     TVkObjectTableEntryNVX=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TVkObjectEntryTypeNVX;
       flags:TVkObjectEntryUsageFlagsNVX;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pType_:TVkObjectEntryTypeNVX;
                          const pFlags:TVkObjectEntryUsageFlagsNVX);
{$endif}
     end;

     PPVkObjectTablePipelineEntryNVX=^PVkObjectTablePipelineEntryNVX;
     PVkObjectTablePipelineEntryNVX=^TVkObjectTablePipelineEntryNVX;
     TVkObjectTablePipelineEntryNVX=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TVkObjectEntryTypeNVX;
       flags:TVkObjectEntryUsageFlagsNVX;
       pipeline:TVkPipeline;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pType_:TVkObjectEntryTypeNVX;
                          const pFlags:TVkObjectEntryUsageFlagsNVX;
                          const pPipeline:TVkPipeline);
{$endif}
     end;

     PPVkObjectTableDescriptorSetEntryNVX=^PVkObjectTableDescriptorSetEntryNVX;
     PVkObjectTableDescriptorSetEntryNVX=^TVkObjectTableDescriptorSetEntryNVX;
     TVkObjectTableDescriptorSetEntryNVX=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TVkObjectEntryTypeNVX;
       flags:TVkObjectEntryUsageFlagsNVX;
       pipelineLayout:TVkPipelineLayout;
       descriptorSet:TVkDescriptorSet;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pType_:TVkObjectEntryTypeNVX;
                          const pFlags:TVkObjectEntryUsageFlagsNVX;
                          const pPipelineLayout:TVkPipelineLayout;
                          const pDescriptorSet:TVkDescriptorSet);
{$endif}
     end;

     PPVkObjectTableVertexBufferEntryNVX=^PVkObjectTableVertexBufferEntryNVX;
     PVkObjectTableVertexBufferEntryNVX=^TVkObjectTableVertexBufferEntryNVX;
     TVkObjectTableVertexBufferEntryNVX=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TVkObjectEntryTypeNVX;
       flags:TVkObjectEntryUsageFlagsNVX;
       buffer:TVkBuffer;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pType_:TVkObjectEntryTypeNVX;
                          const pFlags:TVkObjectEntryUsageFlagsNVX;
                          const pBuffer:TVkBuffer);
{$endif}
     end;

     PPVkObjectTableIndexBufferEntryNVX=^PVkObjectTableIndexBufferEntryNVX;
     PVkObjectTableIndexBufferEntryNVX=^TVkObjectTableIndexBufferEntryNVX;
     TVkObjectTableIndexBufferEntryNVX=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TVkObjectEntryTypeNVX;
       flags:TVkObjectEntryUsageFlagsNVX;
       buffer:TVkBuffer;
       indexType:TVkIndexType;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pType_:TVkObjectEntryTypeNVX;
                          const pFlags:TVkObjectEntryUsageFlagsNVX;
                          const pBuffer:TVkBuffer;
                          const pIndexType:TVkIndexType);
{$endif}
     end;

     PPVkObjectTablePushConstantEntryNVX=^PVkObjectTablePushConstantEntryNVX;
     PVkObjectTablePushConstantEntryNVX=^TVkObjectTablePushConstantEntryNVX;
     TVkObjectTablePushConstantEntryNVX=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TVkObjectEntryTypeNVX;
       flags:TVkObjectEntryUsageFlagsNVX;
       pipelineLayout:TVkPipelineLayout;
       stageFlags:TVkShaderStageFlags;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pType_:TVkObjectEntryTypeNVX;
                          const pFlags:TVkObjectEntryUsageFlagsNVX;
                          const pPipelineLayout:TVkPipelineLayout;
                          const pStageFlags:TVkShaderStageFlags);
{$endif}
     end;

     PPVkPhysicalDeviceFeatures2KHR=^PVkPhysicalDeviceFeatures2KHR;
     PVkPhysicalDeviceFeatures2KHR=^TVkPhysicalDeviceFeatures2KHR;
     TVkPhysicalDeviceFeatures2KHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR
       pNext:PVkVoid; //< Pointer to next structure
       features:TVkPhysicalDeviceFeatures;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFeatures:TVkPhysicalDeviceFeatures);
{$endif}
     end;

     PPVkPhysicalDeviceProperties2KHR=^PVkPhysicalDeviceProperties2KHR;
     PVkPhysicalDeviceProperties2KHR=^TVkPhysicalDeviceProperties2KHR;
     TVkPhysicalDeviceProperties2KHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR
       pNext:PVkVoid; //< Pointer to next structure
       properties:TVkPhysicalDeviceProperties;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pProperties:TVkPhysicalDeviceProperties);
{$endif}
     end;

     PPVkFormatProperties2KHR=^PVkFormatProperties2KHR;
     PVkFormatProperties2KHR=^TVkFormatProperties2KHR;
     TVkFormatProperties2KHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR
       pNext:PVkVoid; //< Pointer to next structure
       formatProperties:TVkFormatProperties;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFormatProperties:TVkFormatProperties);
{$endif}
     end;

     PPVkImageFormatProperties2KHR=^PVkImageFormatProperties2KHR;
     PVkImageFormatProperties2KHR=^TVkImageFormatProperties2KHR;
     TVkImageFormatProperties2KHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR
       pNext:PVkVoid; //< Pointer to next structure
       imageFormatProperties:TVkImageFormatProperties;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pImageFormatProperties:TVkImageFormatProperties);
{$endif}
     end;

     PPVkPhysicalDeviceImageFormatInfo2KHR=^PVkPhysicalDeviceImageFormatInfo2KHR;
     PVkPhysicalDeviceImageFormatInfo2KHR=^TVkPhysicalDeviceImageFormatInfo2KHR;
     TVkPhysicalDeviceImageFormatInfo2KHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR
       pNext:PVkVoid; //< Pointer to next structure
       format:TVkFormat;
       type_:TVkImageType;
       tiling:TVkImageTiling;
       usage:TVkImageUsageFlags;
       flags:TVkImageCreateFlags;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFormat:TVkFormat;
                          const pType_:TVkImageType;
                          const pTiling:TVkImageTiling;
                          const pUsage:TVkImageUsageFlags;
                          const pFlags:TVkImageCreateFlags);
{$endif}
     end;

     PPVkQueueFamilyProperties2KHR=^PVkQueueFamilyProperties2KHR;
     PVkQueueFamilyProperties2KHR=^TVkQueueFamilyProperties2KHR;
     TVkQueueFamilyProperties2KHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR
       pNext:PVkVoid; //< Pointer to next structure
       queueFamilyProperties:TVkQueueFamilyProperties;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pQueueFamilyProperties:TVkQueueFamilyProperties);
{$endif}
     end;

     PPVkPhysicalDeviceMemoryProperties2KHR=^PVkPhysicalDeviceMemoryProperties2KHR;
     PVkPhysicalDeviceMemoryProperties2KHR=^TVkPhysicalDeviceMemoryProperties2KHR;
     TVkPhysicalDeviceMemoryProperties2KHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR
       pNext:PVkVoid; //< Pointer to next structure
       memoryProperties:TVkPhysicalDeviceMemoryProperties;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pMemoryProperties:TVkPhysicalDeviceMemoryProperties);
{$endif}
     end;

     PPVkSparseImageFormatProperties2KHR=^PVkSparseImageFormatProperties2KHR;
     PVkSparseImageFormatProperties2KHR=^TVkSparseImageFormatProperties2KHR;
     TVkSparseImageFormatProperties2KHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR
       pNext:PVkVoid; //< Pointer to next structure
       properties:TVkSparseImageFormatProperties;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pProperties:TVkSparseImageFormatProperties);
{$endif}
     end;

     PPVkPhysicalDeviceSparseImageFormatInfo2KHR=^PVkPhysicalDeviceSparseImageFormatInfo2KHR;
     PVkPhysicalDeviceSparseImageFormatInfo2KHR=^TVkPhysicalDeviceSparseImageFormatInfo2KHR;
     TVkPhysicalDeviceSparseImageFormatInfo2KHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR
       pNext:PVkVoid; //< Pointer to next structure
       format:TVkFormat;
       type_:TVkImageType;
       samples:TVkSampleCountFlagBits;
       usage:TVkImageUsageFlags;
       tiling:TVkImageTiling;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pFormat:TVkFormat;
                          const pType_:TVkImageType;
                          const pSamples:TVkSampleCountFlagBits;
                          const pUsage:TVkImageUsageFlags;
                          const pTiling:TVkImageTiling);
{$endif}
     end;

     PPVkSurfaceCapabilities2EXT=^PVkSurfaceCapabilities2EXT;
     PVkSurfaceCapabilities2EXT=^TVkSurfaceCapabilities2EXT;
     TVkSurfaceCapabilities2EXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT
       pNext:PVkVoid;
       minImageCount:TVkUInt32; //< Supported minimum number of images for the surface
       maxImageCount:TVkUInt32; //< Supported maximum number of images for the surface, 0 for unlimited
       currentExtent:TVkExtent2D; //< Current image width and height for the surface, (0, 0) if undefined
       minImageExtent:TVkExtent2D; //< Supported minimum image width and height for the surface
       maxImageExtent:TVkExtent2D; //< Supported maximum image width and height for the surface
       maxImageArrayLayers:TVkUInt32; //< Supported maximum number of image layers for the surface
       supportedTransforms:TVkSurfaceTransformFlagsKHR; //< 1 or more bits representing the transforms supported
       currentTransform:TVkSurfaceTransformFlagBitsKHR; //< The surface's current transform relative to the device's natural orientation
       supportedCompositeAlpha:TVkCompositeAlphaFlagsKHR; //< 1 or more bits representing the alpha compositing modes supported
       supportedUsageFlags:TVkImageUsageFlags; //< Supported image usage flags for the surface
       supportedSurfaceCounters:TVkSurfaceCounterFlagsEXT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pMinImageCount:TVkUInt32; //< Supported minimum number of images for the surface
                          const pMaxImageCount:TVkUInt32; //< Supported maximum number of images for the surface, 0 for unlimited
                          const pCurrentExtent:TVkExtent2D; //< Current image width and height for the surface, (0, 0) if undefined
                          const pMinImageExtent:TVkExtent2D; //< Supported minimum image width and height for the surface
                          const pMaxImageExtent:TVkExtent2D; //< Supported maximum image width and height for the surface
                          const pMaxImageArrayLayers:TVkUInt32; //< Supported maximum number of image layers for the surface
                          const pSupportedTransforms:TVkSurfaceTransformFlagsKHR; //< 1 or more bits representing the transforms supported
                          const pCurrentTransform:TVkSurfaceTransformFlagBitsKHR; //< The surface's current transform relative to the device's natural orientation
                          const pSupportedCompositeAlpha:TVkCompositeAlphaFlagsKHR; //< 1 or more bits representing the alpha compositing modes supported
                          const pSupportedUsageFlags:TVkImageUsageFlags; //< Supported image usage flags for the surface
                          const pSupportedSurfaceCounters:TVkSurfaceCounterFlagsEXT);
{$endif}
     end;

     PPVkDisplayPowerInfoEXT=^PVkDisplayPowerInfoEXT;
     PVkDisplayPowerInfoEXT=^TVkDisplayPowerInfoEXT;
     TVkDisplayPowerInfoEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT
       pNext:PVkVoid;
       powerState:TVkDisplayPowerStateEXT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pPowerState:TVkDisplayPowerStateEXT);
{$endif}
     end;

     PPVkDeviceEventInfoEXT=^PVkDeviceEventInfoEXT;
     PVkDeviceEventInfoEXT=^TVkDeviceEventInfoEXT;
     TVkDeviceEventInfoEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT
       pNext:PVkVoid;
       deviceEvent:TVkDeviceEventTypeEXT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pDeviceEvent:TVkDeviceEventTypeEXT);
{$endif}
     end;

     PPVkDisplayEventInfoEXT=^PVkDisplayEventInfoEXT;
     PVkDisplayEventInfoEXT=^TVkDisplayEventInfoEXT;
     TVkDisplayEventInfoEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT
       pNext:PVkVoid;
       displayEvent:TVkDisplayEventTypeEXT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pDisplayEvent:TVkDisplayEventTypeEXT);
{$endif}
     end;

     PPVkSwapchainCounterCreateInfoEXT=^PVkSwapchainCounterCreateInfoEXT;
     PVkSwapchainCounterCreateInfoEXT=^TVkSwapchainCounterCreateInfoEXT;
     TVkSwapchainCounterCreateInfoEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       sType:TVkStructureType; //< Must be VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT
       pNext:PVkVoid;
       surfaceCounters:TVkSurfaceCounterFlagsEXT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const pSurfaceCounters:TVkSurfaceCounterFlagsEXT);
{$endif}
     end;

     TvkCreateInstance=function(const pCreateInfo:PVkInstanceCreateInfo;const pAllocator:PVkAllocationCallbacks;pInstance:PVkInstance):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroyInstance=procedure(instance:TVkInstance;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkEnumeratePhysicalDevices=function(instance:TVkInstance;pPhysicalDeviceCount:PVkUInt32;pPhysicalDevices:PVkPhysicalDevice):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetDeviceProcAddr=function(device:TVkDevice;const pName:PVkChar):TPFN_vkVoidFunction; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetInstanceProcAddr=function(instance:TVkInstance;const pName:PVkChar):TPFN_vkVoidFunction; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceProperties=procedure(physicalDevice:TVkPhysicalDevice;pProperties:PVkPhysicalDeviceProperties); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceQueueFamilyProperties=procedure(physicalDevice:TVkPhysicalDevice;pQueueFamilyPropertyCount:PVkUInt32;pQueueFamilyProperties:PVkQueueFamilyProperties); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceMemoryProperties=procedure(physicalDevice:TVkPhysicalDevice;pMemoryProperties:PVkPhysicalDeviceMemoryProperties); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceFeatures=procedure(physicalDevice:TVkPhysicalDevice;pFeatures:PVkPhysicalDeviceFeatures); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceFormatProperties=procedure(physicalDevice:TVkPhysicalDevice;format:TVkFormat;pFormatProperties:PVkFormatProperties); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceImageFormatProperties=function(physicalDevice:TVkPhysicalDevice;format:TVkFormat;type_:TVkImageType;tiling:TVkImageTiling;usage:TVkImageUsageFlags;flags:TVkImageCreateFlags;pImageFormatProperties:PVkImageFormatProperties):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateDevice=function(physicalDevice:TVkPhysicalDevice;const pCreateInfo:PVkDeviceCreateInfo;const pAllocator:PVkAllocationCallbacks;pDevice:PVkDevice):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroyDevice=procedure(device:TVkDevice;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkEnumerateInstanceLayerProperties=function(pPropertyCount:PVkUInt32;pProperties:PVkLayerProperties):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkEnumerateInstanceExtensionProperties=function(const pLayerName:PVkChar;pPropertyCount:PVkUInt32;pProperties:PVkExtensionProperties):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkEnumerateDeviceLayerProperties=function(physicalDevice:TVkPhysicalDevice;pPropertyCount:PVkUInt32;pProperties:PVkLayerProperties):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkEnumerateDeviceExtensionProperties=function(physicalDevice:TVkPhysicalDevice;const pLayerName:PVkChar;pPropertyCount:PVkUInt32;pProperties:PVkExtensionProperties):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetDeviceQueue=procedure(device:TVkDevice;queueFamilyIndex:TVkUInt32;queueIndex:TVkUInt32;pQueue:PVkQueue); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkQueueSubmit=function(queue:TVkQueue;submitCount:TVkUInt32;const pSubmits:PVkSubmitInfo;fence:TVkFence):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkQueueWaitIdle=function(queue:TVkQueue):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDeviceWaitIdle=function(device:TVkDevice):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkAllocateMemory=function(device:TVkDevice;const pAllocateInfo:PVkMemoryAllocateInfo;const pAllocator:PVkAllocationCallbacks;pMemory:PVkDeviceMemory):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkFreeMemory=procedure(device:TVkDevice;memory:TVkDeviceMemory;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkMapMemory=function(device:TVkDevice;memory:TVkDeviceMemory;offset:TVkDeviceSize;size:TVkDeviceSize;flags:TVkMemoryMapFlags;ppData:PPVkVoid):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkUnmapMemory=procedure(device:TVkDevice;memory:TVkDeviceMemory); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkFlushMappedMemoryRanges=function(device:TVkDevice;memoryRangeCount:TVkUInt32;const pMemoryRanges:PVkMappedMemoryRange):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkInvalidateMappedMemoryRanges=function(device:TVkDevice;memoryRangeCount:TVkUInt32;const pMemoryRanges:PVkMappedMemoryRange):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetDeviceMemoryCommitment=procedure(device:TVkDevice;memory:TVkDeviceMemory;pCommittedMemoryInBytes:PVkDeviceSize); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetBufferMemoryRequirements=procedure(device:TVkDevice;buffer:TVkBuffer;pMemoryRequirements:PVkMemoryRequirements); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkBindBufferMemory=function(device:TVkDevice;buffer:TVkBuffer;memory:TVkDeviceMemory;memoryOffset:TVkDeviceSize):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetImageMemoryRequirements=procedure(device:TVkDevice;image:TVkImage;pMemoryRequirements:PVkMemoryRequirements); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkBindImageMemory=function(device:TVkDevice;image:TVkImage;memory:TVkDeviceMemory;memoryOffset:TVkDeviceSize):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetImageSparseMemoryRequirements=procedure(device:TVkDevice;image:TVkImage;pSparseMemoryRequirementCount:PVkUInt32;pSparseMemoryRequirements:PVkSparseImageMemoryRequirements); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceSparseImageFormatProperties=procedure(physicalDevice:TVkPhysicalDevice;format:TVkFormat;type_:TVkImageType;samples:TVkSampleCountFlagBits;usage:TVkImageUsageFlags;tiling:TVkImageTiling;pPropertyCount:PVkUInt32;pProperties:PVkSparseImageFormatProperties); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkQueueBindSparse=function(queue:TVkQueue;bindInfoCount:TVkUInt32;const pBindInfo:PVkBindSparseInfo;fence:TVkFence):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateFence=function(device:TVkDevice;const pCreateInfo:PVkFenceCreateInfo;const pAllocator:PVkAllocationCallbacks;pFence:PVkFence):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroyFence=procedure(device:TVkDevice;fence:TVkFence;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkResetFences=function(device:TVkDevice;fenceCount:TVkUInt32;const pFences:PVkFence):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetFenceStatus=function(device:TVkDevice;fence:TVkFence):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkWaitForFences=function(device:TVkDevice;fenceCount:TVkUInt32;const pFences:PVkFence;waitAll:TVkBool32;timeout:TVkUInt64):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateSemaphore=function(device:TVkDevice;const pCreateInfo:PVkSemaphoreCreateInfo;const pAllocator:PVkAllocationCallbacks;pSemaphore:PVkSemaphore):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroySemaphore=procedure(device:TVkDevice;semaphore:TVkSemaphore;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateEvent=function(device:TVkDevice;const pCreateInfo:PVkEventCreateInfo;const pAllocator:PVkAllocationCallbacks;pEvent:PVkEvent):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroyEvent=procedure(device:TVkDevice;event:TVkEvent;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetEventStatus=function(device:TVkDevice;event:TVkEvent):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkSetEvent=function(device:TVkDevice;event:TVkEvent):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkResetEvent=function(device:TVkDevice;event:TVkEvent):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateQueryPool=function(device:TVkDevice;const pCreateInfo:PVkQueryPoolCreateInfo;const pAllocator:PVkAllocationCallbacks;pQueryPool:PVkQueryPool):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroyQueryPool=procedure(device:TVkDevice;queryPool:TVkQueryPool;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetQueryPoolResults=function(device:TVkDevice;queryPool:TVkQueryPool;firstQuery:TVkUInt32;queryCount:TVkUInt32;dataSize:TVkSize;pData:PVkVoid;stride:TVkDeviceSize;flags:TVkQueryResultFlags):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateBuffer=function(device:TVkDevice;const pCreateInfo:PVkBufferCreateInfo;const pAllocator:PVkAllocationCallbacks;pBuffer:PVkBuffer):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroyBuffer=procedure(device:TVkDevice;buffer:TVkBuffer;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateBufferView=function(device:TVkDevice;const pCreateInfo:PVkBufferViewCreateInfo;const pAllocator:PVkAllocationCallbacks;pView:PVkBufferView):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroyBufferView=procedure(device:TVkDevice;bufferView:TVkBufferView;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateImage=function(device:TVkDevice;const pCreateInfo:PVkImageCreateInfo;const pAllocator:PVkAllocationCallbacks;pImage:PVkImage):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroyImage=procedure(device:TVkDevice;image:TVkImage;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetImageSubresourceLayout=procedure(device:TVkDevice;image:TVkImage;const pSubresource:PVkImageSubresource;pLayout:PVkSubresourceLayout); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateImageView=function(device:TVkDevice;const pCreateInfo:PVkImageViewCreateInfo;const pAllocator:PVkAllocationCallbacks;pView:PVkImageView):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroyImageView=procedure(device:TVkDevice;imageView:TVkImageView;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateShaderModule=function(device:TVkDevice;const pCreateInfo:PVkShaderModuleCreateInfo;const pAllocator:PVkAllocationCallbacks;pShaderModule:PVkShaderModule):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroyShaderModule=procedure(device:TVkDevice;shaderModule:TVkShaderModule;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreatePipelineCache=function(device:TVkDevice;const pCreateInfo:PVkPipelineCacheCreateInfo;const pAllocator:PVkAllocationCallbacks;pPipelineCache:PVkPipelineCache):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroyPipelineCache=procedure(device:TVkDevice;pipelineCache:TVkPipelineCache;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPipelineCacheData=function(device:TVkDevice;pipelineCache:TVkPipelineCache;pDataSize:PVkSize;pData:PVkVoid):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkMergePipelineCaches=function(device:TVkDevice;dstCache:TVkPipelineCache;srcCacheCount:TVkUInt32;const pSrcCaches:PVkPipelineCache):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateGraphicsPipelines=function(device:TVkDevice;pipelineCache:TVkPipelineCache;createInfoCount:TVkUInt32;const pCreateInfos:PVkGraphicsPipelineCreateInfo;const pAllocator:PVkAllocationCallbacks;pPipelines:PVkPipeline):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateComputePipelines=function(device:TVkDevice;pipelineCache:TVkPipelineCache;createInfoCount:TVkUInt32;const pCreateInfos:PVkComputePipelineCreateInfo;const pAllocator:PVkAllocationCallbacks;pPipelines:PVkPipeline):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroyPipeline=procedure(device:TVkDevice;pipeline:TVkPipeline;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreatePipelineLayout=function(device:TVkDevice;const pCreateInfo:PVkPipelineLayoutCreateInfo;const pAllocator:PVkAllocationCallbacks;pPipelineLayout:PVkPipelineLayout):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroyPipelineLayout=procedure(device:TVkDevice;pipelineLayout:TVkPipelineLayout;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateSampler=function(device:TVkDevice;const pCreateInfo:PVkSamplerCreateInfo;const pAllocator:PVkAllocationCallbacks;pSampler:PVkSampler):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroySampler=procedure(device:TVkDevice;sampler:TVkSampler;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateDescriptorSetLayout=function(device:TVkDevice;const pCreateInfo:PVkDescriptorSetLayoutCreateInfo;const pAllocator:PVkAllocationCallbacks;pSetLayout:PVkDescriptorSetLayout):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroyDescriptorSetLayout=procedure(device:TVkDevice;descriptorSetLayout:TVkDescriptorSetLayout;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateDescriptorPool=function(device:TVkDevice;const pCreateInfo:PVkDescriptorPoolCreateInfo;const pAllocator:PVkAllocationCallbacks;pDescriptorPool:PVkDescriptorPool):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroyDescriptorPool=procedure(device:TVkDevice;descriptorPool:TVkDescriptorPool;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkResetDescriptorPool=function(device:TVkDevice;descriptorPool:TVkDescriptorPool;flags:TVkDescriptorPoolResetFlags):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkAllocateDescriptorSets=function(device:TVkDevice;const pAllocateInfo:PVkDescriptorSetAllocateInfo;pDescriptorSets:PVkDescriptorSet):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkFreeDescriptorSets=function(device:TVkDevice;descriptorPool:TVkDescriptorPool;descriptorSetCount:TVkUInt32;const pDescriptorSets:PVkDescriptorSet):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkUpdateDescriptorSets=procedure(device:TVkDevice;descriptorWriteCount:TVkUInt32;const pDescriptorWrites:PVkWriteDescriptorSet;descriptorCopyCount:TVkUInt32;const pDescriptorCopies:PVkCopyDescriptorSet); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateFramebuffer=function(device:TVkDevice;const pCreateInfo:PVkFramebufferCreateInfo;const pAllocator:PVkAllocationCallbacks;pFramebuffer:PVkFramebuffer):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroyFramebuffer=procedure(device:TVkDevice;framebuffer:TVkFramebuffer;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateRenderPass=function(device:TVkDevice;const pCreateInfo:PVkRenderPassCreateInfo;const pAllocator:PVkAllocationCallbacks;pRenderPass:PVkRenderPass):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroyRenderPass=procedure(device:TVkDevice;renderPass:TVkRenderPass;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetRenderAreaGranularity=procedure(device:TVkDevice;renderPass:TVkRenderPass;pGranularity:PVkExtent2D); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateCommandPool=function(device:TVkDevice;const pCreateInfo:PVkCommandPoolCreateInfo;const pAllocator:PVkAllocationCallbacks;pCommandPool:PVkCommandPool):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroyCommandPool=procedure(device:TVkDevice;commandPool:TVkCommandPool;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkResetCommandPool=function(device:TVkDevice;commandPool:TVkCommandPool;flags:TVkCommandPoolResetFlags):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkAllocateCommandBuffers=function(device:TVkDevice;const pAllocateInfo:PVkCommandBufferAllocateInfo;pCommandBuffers:PVkCommandBuffer):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkFreeCommandBuffers=procedure(device:TVkDevice;commandPool:TVkCommandPool;commandBufferCount:TVkUInt32;const pCommandBuffers:PVkCommandBuffer); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkBeginCommandBuffer=function(commandBuffer:TVkCommandBuffer;const pBeginInfo:PVkCommandBufferBeginInfo):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkEndCommandBuffer=function(commandBuffer:TVkCommandBuffer):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkResetCommandBuffer=function(commandBuffer:TVkCommandBuffer;flags:TVkCommandBufferResetFlags):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdBindPipeline=procedure(commandBuffer:TVkCommandBuffer;pipelineBindPoint:TVkPipelineBindPoint;pipeline:TVkPipeline); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdSetViewport=procedure(commandBuffer:TVkCommandBuffer;firstViewport:TVkUInt32;viewportCount:TVkUInt32;const pViewports:PVkViewport); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdSetScissor=procedure(commandBuffer:TVkCommandBuffer;firstScissor:TVkUInt32;scissorCount:TVkUInt32;const pScissors:PVkRect2D); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdSetLineWidth=procedure(commandBuffer:TVkCommandBuffer;lineWidth:TVkFloat); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdSetDepthBias=procedure(commandBuffer:TVkCommandBuffer;depthBiasConstantFactor:TVkFloat;depthBiasClamp:TVkFloat;depthBiasSlopeFactor:TVkFloat); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdSetBlendConstants=procedure(commandBuffer:TVkCommandBuffer;const blendConstants:TVkFloat); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdSetDepthBounds=procedure(commandBuffer:TVkCommandBuffer;minDepthBounds:TVkFloat;maxDepthBounds:TVkFloat); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdSetStencilCompareMask=procedure(commandBuffer:TVkCommandBuffer;faceMask:TVkStencilFaceFlags;compareMask:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdSetStencilWriteMask=procedure(commandBuffer:TVkCommandBuffer;faceMask:TVkStencilFaceFlags;writeMask:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdSetStencilReference=procedure(commandBuffer:TVkCommandBuffer;faceMask:TVkStencilFaceFlags;reference:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdBindDescriptorSets=procedure(commandBuffer:TVkCommandBuffer;pipelineBindPoint:TVkPipelineBindPoint;layout:TVkPipelineLayout;firstSet:TVkUInt32;descriptorSetCount:TVkUInt32;const pDescriptorSets:PVkDescriptorSet;dynamicOffsetCount:TVkUInt32;const pDynamicOffsets:PVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdBindIndexBuffer=procedure(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;indexType:TVkIndexType); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdBindVertexBuffers=procedure(commandBuffer:TVkCommandBuffer;firstBinding:TVkUInt32;bindingCount:TVkUInt32;const pBuffers:PVkBuffer;const pOffsets:PVkDeviceSize); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdDraw=procedure(commandBuffer:TVkCommandBuffer;vertexCount:TVkUInt32;instanceCount:TVkUInt32;firstVertex:TVkUInt32;firstInstance:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdDrawIndexed=procedure(commandBuffer:TVkCommandBuffer;indexCount:TVkUInt32;instanceCount:TVkUInt32;firstIndex:TVkUInt32;vertexOffset:TVkInt32;firstInstance:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdDrawIndirect=procedure(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;drawCount:TVkUInt32;stride:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdDrawIndexedIndirect=procedure(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;drawCount:TVkUInt32;stride:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdDispatch=procedure(commandBuffer:TVkCommandBuffer;x:TVkUInt32;y:TVkUInt32;z:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdDispatchIndirect=procedure(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdCopyBuffer=procedure(commandBuffer:TVkCommandBuffer;srcBuffer:TVkBuffer;dstBuffer:TVkBuffer;regionCount:TVkUInt32;const pRegions:PVkBufferCopy); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdCopyImage=procedure(commandBuffer:TVkCommandBuffer;srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkImageCopy); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdBlitImage=procedure(commandBuffer:TVkCommandBuffer;srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkImageBlit;filter:TVkFilter); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdCopyBufferToImage=procedure(commandBuffer:TVkCommandBuffer;srcBuffer:TVkBuffer;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkBufferImageCopy); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdCopyImageToBuffer=procedure(commandBuffer:TVkCommandBuffer;srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstBuffer:TVkBuffer;regionCount:TVkUInt32;const pRegions:PVkBufferImageCopy); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdUpdateBuffer=procedure(commandBuffer:TVkCommandBuffer;dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;dataSize:TVkDeviceSize;const pData:PVkVoid); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdFillBuffer=procedure(commandBuffer:TVkCommandBuffer;dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;size:TVkDeviceSize;data:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdClearColorImage=procedure(commandBuffer:TVkCommandBuffer;image:TVkImage;imageLayout:TVkImageLayout;const pColor:PVkClearColorValue;rangeCount:TVkUInt32;const pRanges:PVkImageSubresourceRange); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdClearDepthStencilImage=procedure(commandBuffer:TVkCommandBuffer;image:TVkImage;imageLayout:TVkImageLayout;const pDepthStencil:PVkClearDepthStencilValue;rangeCount:TVkUInt32;const pRanges:PVkImageSubresourceRange); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdClearAttachments=procedure(commandBuffer:TVkCommandBuffer;attachmentCount:TVkUInt32;const pAttachments:PVkClearAttachment;rectCount:TVkUInt32;const pRects:PVkClearRect); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdResolveImage=procedure(commandBuffer:TVkCommandBuffer;srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkImageResolve); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdSetEvent=procedure(commandBuffer:TVkCommandBuffer;event:TVkEvent;stageMask:TVkPipelineStageFlags); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdResetEvent=procedure(commandBuffer:TVkCommandBuffer;event:TVkEvent;stageMask:TVkPipelineStageFlags); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdWaitEvents=procedure(commandBuffer:TVkCommandBuffer;eventCount:TVkUInt32;const pEvents:PVkEvent;srcStageMask:TVkPipelineStageFlags;dstStageMask:TVkPipelineStageFlags;memoryBarrierCount:TVkUInt32;const pMemoryBarriers:PVkMemoryBarrier;bufferMemoryBarrierCount:TVkUInt32;const pBufferMemoryBarriers:PVkBufferMemoryBarrier;imageMemoryBarrierCount:TVkUInt32;const pImageMemoryBarriers:PVkImageMemoryBarrier); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdPipelineBarrier=procedure(commandBuffer:TVkCommandBuffer;srcStageMask:TVkPipelineStageFlags;dstStageMask:TVkPipelineStageFlags;dependencyFlags:TVkDependencyFlags;memoryBarrierCount:TVkUInt32;const pMemoryBarriers:PVkMemoryBarrier;bufferMemoryBarrierCount:TVkUInt32;const pBufferMemoryBarriers:PVkBufferMemoryBarrier;imageMemoryBarrierCount:TVkUInt32;const pImageMemoryBarriers:PVkImageMemoryBarrier); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdBeginQuery=procedure(commandBuffer:TVkCommandBuffer;queryPool:TVkQueryPool;query:TVkUInt32;flags:TVkQueryControlFlags); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdEndQuery=procedure(commandBuffer:TVkCommandBuffer;queryPool:TVkQueryPool;query:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdResetQueryPool=procedure(commandBuffer:TVkCommandBuffer;queryPool:TVkQueryPool;firstQuery:TVkUInt32;queryCount:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdWriteTimestamp=procedure(commandBuffer:TVkCommandBuffer;pipelineStage:TVkPipelineStageFlagBits;queryPool:TVkQueryPool;query:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdCopyQueryPoolResults=procedure(commandBuffer:TVkCommandBuffer;queryPool:TVkQueryPool;firstQuery:TVkUInt32;queryCount:TVkUInt32;dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;stride:TVkDeviceSize;flags:TVkQueryResultFlags); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdPushConstants=procedure(commandBuffer:TVkCommandBuffer;layout:TVkPipelineLayout;stageFlags:TVkShaderStageFlags;offset:TVkUInt32;size:TVkUInt32;const pValues:PVkVoid); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdBeginRenderPass=procedure(commandBuffer:TVkCommandBuffer;const pRenderPassBegin:PVkRenderPassBeginInfo;contents:TVkSubpassContents); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdNextSubpass=procedure(commandBuffer:TVkCommandBuffer;contents:TVkSubpassContents); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdEndRenderPass=procedure(commandBuffer:TVkCommandBuffer); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdExecuteCommands=procedure(commandBuffer:TVkCommandBuffer;commandBufferCount:TVkUInt32;const pCommandBuffers:PVkCommandBuffer); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

{$ifdef Android}
     TvkCreateAndroidSurfaceKHR=function(instance:TVkInstance;const pCreateInfo:PVkAndroidSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}

     TvkGetPhysicalDeviceDisplayPropertiesKHR=function(physicalDevice:TVkPhysicalDevice;pPropertyCount:PVkUInt32;pProperties:PVkDisplayPropertiesKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceDisplayPlanePropertiesKHR=function(physicalDevice:TVkPhysicalDevice;pPropertyCount:PVkUInt32;pProperties:PVkDisplayPlanePropertiesKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetDisplayPlaneSupportedDisplaysKHR=function(physicalDevice:TVkPhysicalDevice;planeIndex:TVkUInt32;pDisplayCount:PVkUInt32;pDisplays:PVkDisplayKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetDisplayModePropertiesKHR=function(physicalDevice:TVkPhysicalDevice;display:TVkDisplayKHR;pPropertyCount:PVkUInt32;pProperties:PVkDisplayModePropertiesKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateDisplayModeKHR=function(physicalDevice:TVkPhysicalDevice;display:TVkDisplayKHR;const pCreateInfo:PVkDisplayModeCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pMode:PVkDisplayModeKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetDisplayPlaneCapabilitiesKHR=function(physicalDevice:TVkPhysicalDevice;mode:TVkDisplayModeKHR;planeIndex:TVkUInt32;pCapabilities:PVkDisplayPlaneCapabilitiesKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateDisplayPlaneSurfaceKHR=function(instance:TVkInstance;const pCreateInfo:PVkDisplaySurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateSharedSwapchainsKHR=function(device:TVkDevice;swapchainCount:TVkUInt32;const pCreateInfos:PVkSwapchainCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSwapchains:PVkSwapchainKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

{$ifdef Mir}
     TvkCreateMirSurfaceKHR=function(instance:TVkInstance;const pCreateInfo:PVkMirSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}

{$ifdef Mir}
     TvkGetPhysicalDeviceMirPresentationSupportKHR=function(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;connection:PMirConnection):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}

     TvkDestroySurfaceKHR=procedure(instance:TVkInstance;surface:TVkSurfaceKHR;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceSurfaceSupportKHR=function(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;surface:TVkSurfaceKHR;pSupported:PVkBool32):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceSurfaceCapabilitiesKHR=function(physicalDevice:TVkPhysicalDevice;surface:TVkSurfaceKHR;pSurfaceCapabilities:PVkSurfaceCapabilitiesKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceSurfaceFormatsKHR=function(physicalDevice:TVkPhysicalDevice;surface:TVkSurfaceKHR;pSurfaceFormatCount:PVkUInt32;pSurfaceFormats:PVkSurfaceFormatKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceSurfacePresentModesKHR=function(physicalDevice:TVkPhysicalDevice;surface:TVkSurfaceKHR;pPresentModeCount:PVkUInt32;pPresentModes:PVkPresentModeKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateSwapchainKHR=function(device:TVkDevice;const pCreateInfo:PVkSwapchainCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSwapchain:PVkSwapchainKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroySwapchainKHR=procedure(device:TVkDevice;swapchain:TVkSwapchainKHR;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetSwapchainImagesKHR=function(device:TVkDevice;swapchain:TVkSwapchainKHR;pSwapchainImageCount:PVkUInt32;pSwapchainImages:PVkImage):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkAcquireNextImageKHR=function(device:TVkDevice;swapchain:TVkSwapchainKHR;timeout:TVkUInt64;semaphore:TVkSemaphore;fence:TVkFence;pImageIndex:PVkUInt32):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkQueuePresentKHR=function(queue:TVkQueue;const pPresentInfo:PVkPresentInfoKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateViSurfaceNN=function(instance:TVkInstance;const pCreateInfo:PVkViSurfaceCreateInfoNN;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

{$ifdef Wayland}
     TvkCreateWaylandSurfaceKHR=function(instance:TVkInstance;const pCreateInfo:PVkWaylandSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}

{$ifdef Wayland}
     TvkGetPhysicalDeviceWaylandPresentationSupportKHR=function(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;display:Pwl_display):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}

{$ifdef Windows}
     TvkCreateWin32SurfaceKHR=function(instance:TVkInstance;const pCreateInfo:PVkWin32SurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}

{$ifdef Windows}
     TvkGetPhysicalDeviceWin32PresentationSupportKHR=function(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}

{$ifdef X11}
     TvkCreateXlibSurfaceKHR=function(instance:TVkInstance;const pCreateInfo:PVkXlibSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}

{$ifdef X11}
     TvkGetPhysicalDeviceXlibPresentationSupportKHR=function(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;dpy:PDisplay;visualID:TVisualID):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}

{$ifdef XCB}
     TvkCreateXcbSurfaceKHR=function(instance:TVkInstance;const pCreateInfo:PVkXcbSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}

{$ifdef XCB}
     TvkGetPhysicalDeviceXcbPresentationSupportKHR=function(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;connection:Pxcb_connection;visual_id:Txcb_visualid):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}

     TvkCreateDebugReportCallbackEXT=function(instance:TVkInstance;const pCreateInfo:PVkDebugReportCallbackCreateInfoEXT;const pAllocator:PVkAllocationCallbacks;pCallback:PVkDebugReportCallbackEXT):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroyDebugReportCallbackEXT=procedure(instance:TVkInstance;callback:TVkDebugReportCallbackEXT;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDebugReportMessageEXT=procedure(instance:TVkInstance;flags:TVkDebugReportFlagsEXT;objectType:TVkDebugReportObjectTypeEXT;object_:TVkUInt64;location:TVkSize;messageCode:TVkInt32;const pLayerPrefix:PVkChar;const pMessage:PVkChar); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDebugMarkerSetObjectNameEXT=function(device:TVkDevice;pNameInfo:PVkDebugMarkerObjectNameInfoEXT):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDebugMarkerSetObjectTagEXT=function(device:TVkDevice;pTagInfo:PVkDebugMarkerObjectTagInfoEXT):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdDebugMarkerBeginEXT=procedure(commandBuffer:TVkCommandBuffer;pMarkerInfo:PVkDebugMarkerMarkerInfoEXT); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdDebugMarkerEndEXT=procedure(commandBuffer:TVkCommandBuffer); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdDebugMarkerInsertEXT=procedure(commandBuffer:TVkCommandBuffer;pMarkerInfo:PVkDebugMarkerMarkerInfoEXT); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceExternalImageFormatPropertiesNV=function(physicalDevice:TVkPhysicalDevice;format:TVkFormat;type_:TVkImageType;tiling:TVkImageTiling;usage:TVkImageUsageFlags;flags:TVkImageCreateFlags;externalHandleType:TVkExternalMemoryHandleTypeFlagsNV;pExternalImageFormatProperties:PVkExternalImageFormatPropertiesNV):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

{$ifdef Windows}
     TvkGetMemoryWin32HandleNV=function(device:TVkDevice;memory:TVkDeviceMemory;handleType:TVkExternalMemoryHandleTypeFlagsNV;pHandle:PHANDLE):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}

     TvkCmdDrawIndirectCountAMD=procedure(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;countBuffer:TVkBuffer;countBufferOffset:TVkDeviceSize;maxDrawCount:TVkUInt32;stride:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdDrawIndexedIndirectCountAMD=procedure(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;countBuffer:TVkBuffer;countBufferOffset:TVkDeviceSize;maxDrawCount:TVkUInt32;stride:TVkUInt32); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdProcessCommandsNVX=procedure(commandBuffer:TVkCommandBuffer;const pProcessCommandsInfo:PVkCmdProcessCommandsInfoNVX); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCmdReserveSpaceForCommandsNVX=procedure(commandBuffer:TVkCommandBuffer;const pReserveSpaceInfo:PVkCmdReserveSpaceForCommandsInfoNVX); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateIndirectCommandsLayoutNVX=function(device:TVkDevice;const pCreateInfo:PVkIndirectCommandsLayoutCreateInfoNVX;const pAllocator:PVkAllocationCallbacks;pIndirectCommandsLayout:PVkIndirectCommandsLayoutNVX):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroyIndirectCommandsLayoutNVX=procedure(device:TVkDevice;indirectCommandsLayout:TVkIndirectCommandsLayoutNVX;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkCreateObjectTableNVX=function(device:TVkDevice;const pCreateInfo:PVkObjectTableCreateInfoNVX;const pAllocator:PVkAllocationCallbacks;pObjectTable:PVkObjectTableNVX):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkDestroyObjectTableNVX=procedure(device:TVkDevice;objectTable:TVkObjectTableNVX;const pAllocator:PVkAllocationCallbacks); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkRegisterObjectsNVX=function(device:TVkDevice;objectTable:TVkObjectTableNVX;objectCount:TVkUInt32;const ppObjectTableEntries:PPVkObjectTableEntryNVX;const pObjectIndices:PVkUInt32):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkUnregisterObjectsNVX=function(device:TVkDevice;objectTable:TVkObjectTableNVX;objectCount:TVkUInt32;const pObjectEntryTypes:PVkObjectEntryTypeNVX;const pObjectIndices:PVkUInt32):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceGeneratedCommandsPropertiesNVX=procedure(physicalDevice:TVkPhysicalDevice;pFeatures:PVkDeviceGeneratedCommandsFeaturesNVX;pLimits:PVkDeviceGeneratedCommandsLimitsNVX); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceFeatures2KHR=procedure(physicalDevice:TVkPhysicalDevice;pFeatures:PVkPhysicalDeviceFeatures2KHR); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceProperties2KHR=procedure(physicalDevice:TVkPhysicalDevice;pProperties:PVkPhysicalDeviceProperties2KHR); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceFormatProperties2KHR=procedure(physicalDevice:TVkPhysicalDevice;format:TVkFormat;pFormatProperties:PVkFormatProperties2KHR); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceImageFormatProperties2KHR=function(physicalDevice:TVkPhysicalDevice;const pImageFormatInfo:PVkPhysicalDeviceImageFormatInfo2KHR;pImageFormatProperties:PVkImageFormatProperties2KHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceQueueFamilyProperties2KHR=procedure(physicalDevice:TVkPhysicalDevice;pQueueFamilyPropertyCount:PVkUInt32;pQueueFamilyProperties:PVkQueueFamilyProperties2KHR); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceMemoryProperties2KHR=procedure(physicalDevice:TVkPhysicalDevice;pMemoryProperties:PVkPhysicalDeviceMemoryProperties2KHR); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceSparseImageFormatProperties2KHR=procedure(physicalDevice:TVkPhysicalDevice;const pFormatInfo:PVkPhysicalDeviceSparseImageFormatInfo2KHR;pPropertyCount:PVkUInt32;pProperties:PVkSparseImageFormatProperties2KHR); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkTrimCommandPoolKHR=procedure(device:TVkDevice;commandPool:TVkCommandPool;flags:TVkCommandPoolTrimFlagsKHR); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkReleaseDisplayEXT=function(physicalDevice:TVkPhysicalDevice;display:TVkDisplayKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

{$ifdef X11}
     TvkAcquireXlibDisplayEXT=function(physicalDevice:TVkPhysicalDevice;dpy:PDisplay;display:TVkDisplayKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}

{$ifdef X11}
     TvkGetRandROutputDisplayEXT=function(physicalDevice:TVkPhysicalDevice;dpy:PDisplay;rrOutput:TRROutput;pDisplay:PVkDisplayKHR):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}

     TvkDisplayPowerControlEXT=function(device:TVkDevice;display:TVkDisplayKHR;const pDisplayPowerInfo:PVkDisplayPowerInfoEXT):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkRegisterDeviceEventEXT=function(device:TVkDevice;const pDeviceEventInfo:PVkDeviceEventInfoEXT;const pAllocator:PVkAllocationCallbacks;pFence:PVkFence):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkRegisterDisplayEventEXT=function(device:TVkDevice;display:TVkDisplayKHR;const pDisplayEventInfo:PVkDisplayEventInfoEXT;const pAllocator:PVkAllocationCallbacks;pFence:PVkFence):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetSwapchainCounterEXT=function(device:TVkDevice;swapchain:TVkSwapchainKHR;counter:TVkSurfaceCounterFlagBitsEXT;pCounterValue:PVkUInt64):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TvkGetPhysicalDeviceSurfaceCapabilities2EXT=function(physicalDevice:TVkPhysicalDevice;surface:TVkSurfaceKHR;pSurfaceCapabilities:PVkSurfaceCapabilities2EXT):TVkResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}


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

      CreateViSurfaceNN:TvkCreateViSurfaceNN;

{$ifdef Wayland}
      CreateWaylandSurfaceKHR:TvkCreateWaylandSurfaceKHR;
{$endif}

{$ifdef Wayland}
      GetPhysicalDeviceWaylandPresentationSupportKHR:TvkGetPhysicalDeviceWaylandPresentationSupportKHR;
{$endif}

{$ifdef Windows}
      CreateWin32SurfaceKHR:TvkCreateWin32SurfaceKHR;
{$endif}

{$ifdef Windows}
      GetPhysicalDeviceWin32PresentationSupportKHR:TvkGetPhysicalDeviceWin32PresentationSupportKHR;
{$endif}

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

      DebugMarkerSetObjectNameEXT:TvkDebugMarkerSetObjectNameEXT;

      DebugMarkerSetObjectTagEXT:TvkDebugMarkerSetObjectTagEXT;

      CmdDebugMarkerBeginEXT:TvkCmdDebugMarkerBeginEXT;

      CmdDebugMarkerEndEXT:TvkCmdDebugMarkerEndEXT;

      CmdDebugMarkerInsertEXT:TvkCmdDebugMarkerInsertEXT;

      GetPhysicalDeviceExternalImageFormatPropertiesNV:TvkGetPhysicalDeviceExternalImageFormatPropertiesNV;

{$ifdef Windows}
      GetMemoryWin32HandleNV:TvkGetMemoryWin32HandleNV;
{$endif}

      CmdDrawIndirectCountAMD:TvkCmdDrawIndirectCountAMD;

      CmdDrawIndexedIndirectCountAMD:TvkCmdDrawIndexedIndirectCountAMD;

      CmdProcessCommandsNVX:TvkCmdProcessCommandsNVX;

      CmdReserveSpaceForCommandsNVX:TvkCmdReserveSpaceForCommandsNVX;

      CreateIndirectCommandsLayoutNVX:TvkCreateIndirectCommandsLayoutNVX;

      DestroyIndirectCommandsLayoutNVX:TvkDestroyIndirectCommandsLayoutNVX;

      CreateObjectTableNVX:TvkCreateObjectTableNVX;

      DestroyObjectTableNVX:TvkDestroyObjectTableNVX;

      RegisterObjectsNVX:TvkRegisterObjectsNVX;

      UnregisterObjectsNVX:TvkUnregisterObjectsNVX;

      GetPhysicalDeviceGeneratedCommandsPropertiesNVX:TvkGetPhysicalDeviceGeneratedCommandsPropertiesNVX;

      GetPhysicalDeviceFeatures2KHR:TvkGetPhysicalDeviceFeatures2KHR;

      GetPhysicalDeviceProperties2KHR:TvkGetPhysicalDeviceProperties2KHR;

      GetPhysicalDeviceFormatProperties2KHR:TvkGetPhysicalDeviceFormatProperties2KHR;

      GetPhysicalDeviceImageFormatProperties2KHR:TvkGetPhysicalDeviceImageFormatProperties2KHR;

      GetPhysicalDeviceQueueFamilyProperties2KHR:TvkGetPhysicalDeviceQueueFamilyProperties2KHR;

      GetPhysicalDeviceMemoryProperties2KHR:TvkGetPhysicalDeviceMemoryProperties2KHR;

      GetPhysicalDeviceSparseImageFormatProperties2KHR:TvkGetPhysicalDeviceSparseImageFormatProperties2KHR;

      TrimCommandPoolKHR:TvkTrimCommandPoolKHR;

      ReleaseDisplayEXT:TvkReleaseDisplayEXT;

{$ifdef X11}
      AcquireXlibDisplayEXT:TvkAcquireXlibDisplayEXT;
{$endif}

{$ifdef X11}
      GetRandROutputDisplayEXT:TvkGetRandROutputDisplayEXT;
{$endif}

      DisplayPowerControlEXT:TvkDisplayPowerControlEXT;

      RegisterDeviceEventEXT:TvkRegisterDeviceEventEXT;

      RegisterDisplayEventEXT:TvkRegisterDisplayEventEXT;

      GetSwapchainCounterEXT:TvkGetSwapchainCounterEXT;

      GetPhysicalDeviceSurfaceCapabilities2EXT:TvkGetPhysicalDeviceSurfaceCapabilities2EXT;

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

       function MapMemory(device:TVkDevice;memory:TVkDeviceMemory;offset:TVkDeviceSize;size:TVkDeviceSize;flags:TVkMemoryMapFlags;ppData:PPVkVoid):TVkResult; virtual;

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

       function GetQueryPoolResults(device:TVkDevice;queryPool:TVkQueryPool;firstQuery:TVkUInt32;queryCount:TVkUInt32;dataSize:TVkSize;pData:PVkVoid;stride:TVkDeviceSize;flags:TVkQueryResultFlags):TVkResult; virtual;

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

       function GetPipelineCacheData(device:TVkDevice;pipelineCache:TVkPipelineCache;pDataSize:PVkSize;pData:PVkVoid):TVkResult; virtual;

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

       procedure CmdUpdateBuffer(commandBuffer:TVkCommandBuffer;dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;dataSize:TVkDeviceSize;const pData:PVkVoid); virtual;

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

       procedure CmdPushConstants(commandBuffer:TVkCommandBuffer;layout:TVkPipelineLayout;stageFlags:TVkShaderStageFlags;offset:TVkUInt32;size:TVkUInt32;const pValues:PVkVoid); virtual;

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

       function CreateViSurfaceNN(instance:TVkInstance;const pCreateInfo:PVkViSurfaceCreateInfoNN;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; virtual;

{$ifdef Wayland}
       function CreateWaylandSurfaceKHR(instance:TVkInstance;const pCreateInfo:PVkWaylandSurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; virtual;
{$endif}

{$ifdef Wayland}
       function GetPhysicalDeviceWaylandPresentationSupportKHR(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32;display:Pwl_display):TVkBool32; virtual;
{$endif}

{$ifdef Windows}
       function CreateWin32SurfaceKHR(instance:TVkInstance;const pCreateInfo:PVkWin32SurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult; virtual;
{$endif}

{$ifdef Windows}
       function GetPhysicalDeviceWin32PresentationSupportKHR(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32):TVkBool32; virtual;
{$endif}

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

       procedure DebugReportMessageEXT(instance:TVkInstance;flags:TVkDebugReportFlagsEXT;objectType:TVkDebugReportObjectTypeEXT;object_:TVkUInt64;location:TVkSize;messageCode:TVkInt32;const pLayerPrefix:PVkChar;const pMessage:PVkChar); virtual;

       function DebugMarkerSetObjectNameEXT(device:TVkDevice;pNameInfo:PVkDebugMarkerObjectNameInfoEXT):TVkResult; virtual;

       function DebugMarkerSetObjectTagEXT(device:TVkDevice;pTagInfo:PVkDebugMarkerObjectTagInfoEXT):TVkResult; virtual;

       procedure CmdDebugMarkerBeginEXT(commandBuffer:TVkCommandBuffer;pMarkerInfo:PVkDebugMarkerMarkerInfoEXT); virtual;

       procedure CmdDebugMarkerEndEXT(commandBuffer:TVkCommandBuffer); virtual;

       procedure CmdDebugMarkerInsertEXT(commandBuffer:TVkCommandBuffer;pMarkerInfo:PVkDebugMarkerMarkerInfoEXT); virtual;

       function GetPhysicalDeviceExternalImageFormatPropertiesNV(physicalDevice:TVkPhysicalDevice;format:TVkFormat;type_:TVkImageType;tiling:TVkImageTiling;usage:TVkImageUsageFlags;flags:TVkImageCreateFlags;externalHandleType:TVkExternalMemoryHandleTypeFlagsNV;pExternalImageFormatProperties:PVkExternalImageFormatPropertiesNV):TVkResult; virtual;

{$ifdef Windows}
       function GetMemoryWin32HandleNV(device:TVkDevice;memory:TVkDeviceMemory;handleType:TVkExternalMemoryHandleTypeFlagsNV;pHandle:PHANDLE):TVkResult; virtual;
{$endif}

       procedure CmdDrawIndirectCountAMD(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;countBuffer:TVkBuffer;countBufferOffset:TVkDeviceSize;maxDrawCount:TVkUInt32;stride:TVkUInt32); virtual;

       procedure CmdDrawIndexedIndirectCountAMD(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;countBuffer:TVkBuffer;countBufferOffset:TVkDeviceSize;maxDrawCount:TVkUInt32;stride:TVkUInt32); virtual;

       procedure CmdProcessCommandsNVX(commandBuffer:TVkCommandBuffer;const pProcessCommandsInfo:PVkCmdProcessCommandsInfoNVX); virtual;

       procedure CmdReserveSpaceForCommandsNVX(commandBuffer:TVkCommandBuffer;const pReserveSpaceInfo:PVkCmdReserveSpaceForCommandsInfoNVX); virtual;

       function CreateIndirectCommandsLayoutNVX(device:TVkDevice;const pCreateInfo:PVkIndirectCommandsLayoutCreateInfoNVX;const pAllocator:PVkAllocationCallbacks;pIndirectCommandsLayout:PVkIndirectCommandsLayoutNVX):TVkResult; virtual;

       procedure DestroyIndirectCommandsLayoutNVX(device:TVkDevice;indirectCommandsLayout:TVkIndirectCommandsLayoutNVX;const pAllocator:PVkAllocationCallbacks); virtual;

       function CreateObjectTableNVX(device:TVkDevice;const pCreateInfo:PVkObjectTableCreateInfoNVX;const pAllocator:PVkAllocationCallbacks;pObjectTable:PVkObjectTableNVX):TVkResult; virtual;

       procedure DestroyObjectTableNVX(device:TVkDevice;objectTable:TVkObjectTableNVX;const pAllocator:PVkAllocationCallbacks); virtual;

       function RegisterObjectsNVX(device:TVkDevice;objectTable:TVkObjectTableNVX;objectCount:TVkUInt32;const ppObjectTableEntries:PPVkObjectTableEntryNVX;const pObjectIndices:PVkUInt32):TVkResult; virtual;

       function UnregisterObjectsNVX(device:TVkDevice;objectTable:TVkObjectTableNVX;objectCount:TVkUInt32;const pObjectEntryTypes:PVkObjectEntryTypeNVX;const pObjectIndices:PVkUInt32):TVkResult; virtual;

       procedure GetPhysicalDeviceGeneratedCommandsPropertiesNVX(physicalDevice:TVkPhysicalDevice;pFeatures:PVkDeviceGeneratedCommandsFeaturesNVX;pLimits:PVkDeviceGeneratedCommandsLimitsNVX); virtual;

       procedure GetPhysicalDeviceFeatures2KHR(physicalDevice:TVkPhysicalDevice;pFeatures:PVkPhysicalDeviceFeatures2KHR); virtual;

       procedure GetPhysicalDeviceProperties2KHR(physicalDevice:TVkPhysicalDevice;pProperties:PVkPhysicalDeviceProperties2KHR); virtual;

       procedure GetPhysicalDeviceFormatProperties2KHR(physicalDevice:TVkPhysicalDevice;format:TVkFormat;pFormatProperties:PVkFormatProperties2KHR); virtual;

       function GetPhysicalDeviceImageFormatProperties2KHR(physicalDevice:TVkPhysicalDevice;const pImageFormatInfo:PVkPhysicalDeviceImageFormatInfo2KHR;pImageFormatProperties:PVkImageFormatProperties2KHR):TVkResult; virtual;

       procedure GetPhysicalDeviceQueueFamilyProperties2KHR(physicalDevice:TVkPhysicalDevice;pQueueFamilyPropertyCount:PVkUInt32;pQueueFamilyProperties:PVkQueueFamilyProperties2KHR); virtual;

       procedure GetPhysicalDeviceMemoryProperties2KHR(physicalDevice:TVkPhysicalDevice;pMemoryProperties:PVkPhysicalDeviceMemoryProperties2KHR); virtual;

       procedure GetPhysicalDeviceSparseImageFormatProperties2KHR(physicalDevice:TVkPhysicalDevice;const pFormatInfo:PVkPhysicalDeviceSparseImageFormatInfo2KHR;pPropertyCount:PVkUInt32;pProperties:PVkSparseImageFormatProperties2KHR); virtual;

       procedure TrimCommandPoolKHR(device:TVkDevice;commandPool:TVkCommandPool;flags:TVkCommandPoolTrimFlagsKHR); virtual;

       function ReleaseDisplayEXT(physicalDevice:TVkPhysicalDevice;display:TVkDisplayKHR):TVkResult; virtual;

{$ifdef X11}
       function AcquireXlibDisplayEXT(physicalDevice:TVkPhysicalDevice;dpy:PDisplay;display:TVkDisplayKHR):TVkResult; virtual;
{$endif}

{$ifdef X11}
       function GetRandROutputDisplayEXT(physicalDevice:TVkPhysicalDevice;dpy:PDisplay;rrOutput:TRROutput;pDisplay:PVkDisplayKHR):TVkResult; virtual;
{$endif}

       function DisplayPowerControlEXT(device:TVkDevice;display:TVkDisplayKHR;const pDisplayPowerInfo:PVkDisplayPowerInfoEXT):TVkResult; virtual;

       function RegisterDeviceEventEXT(device:TVkDevice;const pDeviceEventInfo:PVkDeviceEventInfoEXT;const pAllocator:PVkAllocationCallbacks;pFence:PVkFence):TVkResult; virtual;

       function RegisterDisplayEventEXT(device:TVkDevice;display:TVkDisplayKHR;const pDisplayEventInfo:PVkDisplayEventInfoEXT;const pAllocator:PVkAllocationCallbacks;pFence:PVkFence):TVkResult; virtual;

       function GetSwapchainCounterEXT(device:TVkDevice;swapchain:TVkSwapchainKHR;counter:TVkSurfaceCounterFlagBitsEXT;pCounterValue:PVkUInt64):TVkResult; virtual;

       function GetPhysicalDeviceSurfaceCapabilities2EXT(physicalDevice:TVkPhysicalDevice;surface:TVkSurfaceKHR;pSurfaceCapabilities:PVkSurfaceCapabilities2EXT):TVkResult; virtual;

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

    vkCreateViSurfaceNN:TvkCreateViSurfaceNN=nil;

{$ifdef Wayland}
    vkCreateWaylandSurfaceKHR:TvkCreateWaylandSurfaceKHR=nil;
{$endif}

{$ifdef Wayland}
    vkGetPhysicalDeviceWaylandPresentationSupportKHR:TvkGetPhysicalDeviceWaylandPresentationSupportKHR=nil;
{$endif}

{$ifdef Windows}
    vkCreateWin32SurfaceKHR:TvkCreateWin32SurfaceKHR=nil;
{$endif}

{$ifdef Windows}
    vkGetPhysicalDeviceWin32PresentationSupportKHR:TvkGetPhysicalDeviceWin32PresentationSupportKHR=nil;
{$endif}

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

    vkDebugMarkerSetObjectNameEXT:TvkDebugMarkerSetObjectNameEXT=nil;

    vkDebugMarkerSetObjectTagEXT:TvkDebugMarkerSetObjectTagEXT=nil;

    vkCmdDebugMarkerBeginEXT:TvkCmdDebugMarkerBeginEXT=nil;

    vkCmdDebugMarkerEndEXT:TvkCmdDebugMarkerEndEXT=nil;

    vkCmdDebugMarkerInsertEXT:TvkCmdDebugMarkerInsertEXT=nil;

    vkGetPhysicalDeviceExternalImageFormatPropertiesNV:TvkGetPhysicalDeviceExternalImageFormatPropertiesNV=nil;

{$ifdef Windows}
    vkGetMemoryWin32HandleNV:TvkGetMemoryWin32HandleNV=nil;
{$endif}

    vkCmdDrawIndirectCountAMD:TvkCmdDrawIndirectCountAMD=nil;

    vkCmdDrawIndexedIndirectCountAMD:TvkCmdDrawIndexedIndirectCountAMD=nil;

    vkCmdProcessCommandsNVX:TvkCmdProcessCommandsNVX=nil;

    vkCmdReserveSpaceForCommandsNVX:TvkCmdReserveSpaceForCommandsNVX=nil;

    vkCreateIndirectCommandsLayoutNVX:TvkCreateIndirectCommandsLayoutNVX=nil;

    vkDestroyIndirectCommandsLayoutNVX:TvkDestroyIndirectCommandsLayoutNVX=nil;

    vkCreateObjectTableNVX:TvkCreateObjectTableNVX=nil;

    vkDestroyObjectTableNVX:TvkDestroyObjectTableNVX=nil;

    vkRegisterObjectsNVX:TvkRegisterObjectsNVX=nil;

    vkUnregisterObjectsNVX:TvkUnregisterObjectsNVX=nil;

    vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX:TvkGetPhysicalDeviceGeneratedCommandsPropertiesNVX=nil;

    vkGetPhysicalDeviceFeatures2KHR:TvkGetPhysicalDeviceFeatures2KHR=nil;

    vkGetPhysicalDeviceProperties2KHR:TvkGetPhysicalDeviceProperties2KHR=nil;

    vkGetPhysicalDeviceFormatProperties2KHR:TvkGetPhysicalDeviceFormatProperties2KHR=nil;

    vkGetPhysicalDeviceImageFormatProperties2KHR:TvkGetPhysicalDeviceImageFormatProperties2KHR=nil;

    vkGetPhysicalDeviceQueueFamilyProperties2KHR:TvkGetPhysicalDeviceQueueFamilyProperties2KHR=nil;

    vkGetPhysicalDeviceMemoryProperties2KHR:TvkGetPhysicalDeviceMemoryProperties2KHR=nil;

    vkGetPhysicalDeviceSparseImageFormatProperties2KHR:TvkGetPhysicalDeviceSparseImageFormatProperties2KHR=nil;

    vkTrimCommandPoolKHR:TvkTrimCommandPoolKHR=nil;

    vkReleaseDisplayEXT:TvkReleaseDisplayEXT=nil;

{$ifdef X11}
    vkAcquireXlibDisplayEXT:TvkAcquireXlibDisplayEXT=nil;
{$endif}

{$ifdef X11}
    vkGetRandROutputDisplayEXT:TvkGetRandROutputDisplayEXT=nil;
{$endif}

    vkDisplayPowerControlEXT:TvkDisplayPowerControlEXT=nil;

    vkRegisterDeviceEventEXT:TvkRegisterDeviceEventEXT=nil;

    vkRegisterDisplayEventEXT:TvkRegisterDisplayEventEXT=nil;

    vkGetSwapchainCounterEXT:TvkGetSwapchainCounterEXT=nil;

    vkGetPhysicalDeviceSurfaceCapabilities2EXT:TvkGetPhysicalDeviceSurfaceCapabilities2EXT=nil;


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
function LoadVulkanDeviceCommands(const GetDeviceProcAddr:TvkGetDeviceProcAddr;const Device:TVkDevice;out DeviceCommands:TVulkanCommands):boolean;

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
  if result then begin
   vkEnumerateInstanceExtensionProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkEnumerateInstanceExtensionProperties')));
   @vk.fCommands.EnumerateInstanceExtensionProperties:=addr(vkEnumerateInstanceExtensionProperties);
   vkEnumerateInstanceLayerProperties:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkEnumerateInstanceLayerProperties')));
   @vk.fCommands.EnumerateInstanceLayerProperties:=addr(vkEnumerateInstanceLayerProperties);
   vkCreateInstance:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(VK_NULL_INSTANCE,PVkChar('vkCreateInstance')));
   @vk.fCommands.CreateInstance:=addr(vkCreateInstance);
   result:=assigned(vkEnumerateInstanceExtensionProperties) and
           assigned(vkEnumerateInstanceLayerProperties) and 
           assigned(vkCreateInstance);
  end;
 end;
end;

function LoadVulkanGlobalCommands:boolean;
begin
 result:=assigned(vkGetInstanceProcAddr);
 if result then begin
  if not assigned(vkCreateInstance) then begin
   @vkCreateInstance:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateInstance'));
   @vk.fCommands.CreateInstance:=addr(vkCreateInstance);
  end;
  if not assigned(vkDestroyInstance) then begin
   @vkDestroyInstance:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroyInstance'));
   @vk.fCommands.DestroyInstance:=addr(vkDestroyInstance);
  end;
  if not assigned(vkEnumeratePhysicalDevices) then begin
   @vkEnumeratePhysicalDevices:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkEnumeratePhysicalDevices'));
   @vk.fCommands.EnumeratePhysicalDevices:=addr(vkEnumeratePhysicalDevices);
  end;
  if not assigned(vkGetDeviceProcAddr) then begin
   @vkGetDeviceProcAddr:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetDeviceProcAddr'));
   @vk.fCommands.GetDeviceProcAddr:=addr(vkGetDeviceProcAddr);
  end;
  if not assigned(vkGetInstanceProcAddr) then begin
   @vkGetInstanceProcAddr:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetInstanceProcAddr'));
   @vk.fCommands.GetInstanceProcAddr:=addr(vkGetInstanceProcAddr);
  end;
  if not assigned(vkGetPhysicalDeviceProperties) then begin
   @vkGetPhysicalDeviceProperties:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceProperties'));
   @vk.fCommands.GetPhysicalDeviceProperties:=addr(vkGetPhysicalDeviceProperties);
  end;
  if not assigned(vkGetPhysicalDeviceQueueFamilyProperties) then begin
   @vkGetPhysicalDeviceQueueFamilyProperties:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceQueueFamilyProperties'));
   @vk.fCommands.GetPhysicalDeviceQueueFamilyProperties:=addr(vkGetPhysicalDeviceQueueFamilyProperties);
  end;
  if not assigned(vkGetPhysicalDeviceMemoryProperties) then begin
   @vkGetPhysicalDeviceMemoryProperties:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceMemoryProperties'));
   @vk.fCommands.GetPhysicalDeviceMemoryProperties:=addr(vkGetPhysicalDeviceMemoryProperties);
  end;
  if not assigned(vkGetPhysicalDeviceFeatures) then begin
   @vkGetPhysicalDeviceFeatures:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceFeatures'));
   @vk.fCommands.GetPhysicalDeviceFeatures:=addr(vkGetPhysicalDeviceFeatures);
  end;
  if not assigned(vkGetPhysicalDeviceFormatProperties) then begin
   @vkGetPhysicalDeviceFormatProperties:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceFormatProperties'));
   @vk.fCommands.GetPhysicalDeviceFormatProperties:=addr(vkGetPhysicalDeviceFormatProperties);
  end;
  if not assigned(vkGetPhysicalDeviceImageFormatProperties) then begin
   @vkGetPhysicalDeviceImageFormatProperties:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceImageFormatProperties'));
   @vk.fCommands.GetPhysicalDeviceImageFormatProperties:=addr(vkGetPhysicalDeviceImageFormatProperties);
  end;
  if not assigned(vkCreateDevice) then begin
   @vkCreateDevice:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateDevice'));
   @vk.fCommands.CreateDevice:=addr(vkCreateDevice);
  end;
  if not assigned(vkDestroyDevice) then begin
   @vkDestroyDevice:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroyDevice'));
   @vk.fCommands.DestroyDevice:=addr(vkDestroyDevice);
  end;
  if not assigned(vkEnumerateInstanceLayerProperties) then begin
   @vkEnumerateInstanceLayerProperties:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkEnumerateInstanceLayerProperties'));
   @vk.fCommands.EnumerateInstanceLayerProperties:=addr(vkEnumerateInstanceLayerProperties);
  end;
  if not assigned(vkEnumerateInstanceExtensionProperties) then begin
   @vkEnumerateInstanceExtensionProperties:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkEnumerateInstanceExtensionProperties'));
   @vk.fCommands.EnumerateInstanceExtensionProperties:=addr(vkEnumerateInstanceExtensionProperties);
  end;
  if not assigned(vkEnumerateDeviceLayerProperties) then begin
   @vkEnumerateDeviceLayerProperties:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkEnumerateDeviceLayerProperties'));
   @vk.fCommands.EnumerateDeviceLayerProperties:=addr(vkEnumerateDeviceLayerProperties);
  end;
  if not assigned(vkEnumerateDeviceExtensionProperties) then begin
   @vkEnumerateDeviceExtensionProperties:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkEnumerateDeviceExtensionProperties'));
   @vk.fCommands.EnumerateDeviceExtensionProperties:=addr(vkEnumerateDeviceExtensionProperties);
  end;
  if not assigned(vkGetDeviceQueue) then begin
   @vkGetDeviceQueue:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetDeviceQueue'));
   @vk.fCommands.GetDeviceQueue:=addr(vkGetDeviceQueue);
  end;
  if not assigned(vkQueueSubmit) then begin
   @vkQueueSubmit:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkQueueSubmit'));
   @vk.fCommands.QueueSubmit:=addr(vkQueueSubmit);
  end;
  if not assigned(vkQueueWaitIdle) then begin
   @vkQueueWaitIdle:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkQueueWaitIdle'));
   @vk.fCommands.QueueWaitIdle:=addr(vkQueueWaitIdle);
  end;
  if not assigned(vkDeviceWaitIdle) then begin
   @vkDeviceWaitIdle:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDeviceWaitIdle'));
   @vk.fCommands.DeviceWaitIdle:=addr(vkDeviceWaitIdle);
  end;
  if not assigned(vkAllocateMemory) then begin
   @vkAllocateMemory:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkAllocateMemory'));
   @vk.fCommands.AllocateMemory:=addr(vkAllocateMemory);
  end;
  if not assigned(vkFreeMemory) then begin
   @vkFreeMemory:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkFreeMemory'));
   @vk.fCommands.FreeMemory:=addr(vkFreeMemory);
  end;
  if not assigned(vkMapMemory) then begin
   @vkMapMemory:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkMapMemory'));
   @vk.fCommands.MapMemory:=addr(vkMapMemory);
  end;
  if not assigned(vkUnmapMemory) then begin
   @vkUnmapMemory:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkUnmapMemory'));
   @vk.fCommands.UnmapMemory:=addr(vkUnmapMemory);
  end;
  if not assigned(vkFlushMappedMemoryRanges) then begin
   @vkFlushMappedMemoryRanges:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkFlushMappedMemoryRanges'));
   @vk.fCommands.FlushMappedMemoryRanges:=addr(vkFlushMappedMemoryRanges);
  end;
  if not assigned(vkInvalidateMappedMemoryRanges) then begin
   @vkInvalidateMappedMemoryRanges:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkInvalidateMappedMemoryRanges'));
   @vk.fCommands.InvalidateMappedMemoryRanges:=addr(vkInvalidateMappedMemoryRanges);
  end;
  if not assigned(vkGetDeviceMemoryCommitment) then begin
   @vkGetDeviceMemoryCommitment:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetDeviceMemoryCommitment'));
   @vk.fCommands.GetDeviceMemoryCommitment:=addr(vkGetDeviceMemoryCommitment);
  end;
  if not assigned(vkGetBufferMemoryRequirements) then begin
   @vkGetBufferMemoryRequirements:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetBufferMemoryRequirements'));
   @vk.fCommands.GetBufferMemoryRequirements:=addr(vkGetBufferMemoryRequirements);
  end;
  if not assigned(vkBindBufferMemory) then begin
   @vkBindBufferMemory:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkBindBufferMemory'));
   @vk.fCommands.BindBufferMemory:=addr(vkBindBufferMemory);
  end;
  if not assigned(vkGetImageMemoryRequirements) then begin
   @vkGetImageMemoryRequirements:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetImageMemoryRequirements'));
   @vk.fCommands.GetImageMemoryRequirements:=addr(vkGetImageMemoryRequirements);
  end;
  if not assigned(vkBindImageMemory) then begin
   @vkBindImageMemory:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkBindImageMemory'));
   @vk.fCommands.BindImageMemory:=addr(vkBindImageMemory);
  end;
  if not assigned(vkGetImageSparseMemoryRequirements) then begin
   @vkGetImageSparseMemoryRequirements:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetImageSparseMemoryRequirements'));
   @vk.fCommands.GetImageSparseMemoryRequirements:=addr(vkGetImageSparseMemoryRequirements);
  end;
  if not assigned(vkGetPhysicalDeviceSparseImageFormatProperties) then begin
   @vkGetPhysicalDeviceSparseImageFormatProperties:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceSparseImageFormatProperties'));
   @vk.fCommands.GetPhysicalDeviceSparseImageFormatProperties:=addr(vkGetPhysicalDeviceSparseImageFormatProperties);
  end;
  if not assigned(vkQueueBindSparse) then begin
   @vkQueueBindSparse:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkQueueBindSparse'));
   @vk.fCommands.QueueBindSparse:=addr(vkQueueBindSparse);
  end;
  if not assigned(vkCreateFence) then begin
   @vkCreateFence:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateFence'));
   @vk.fCommands.CreateFence:=addr(vkCreateFence);
  end;
  if not assigned(vkDestroyFence) then begin
   @vkDestroyFence:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroyFence'));
   @vk.fCommands.DestroyFence:=addr(vkDestroyFence);
  end;
  if not assigned(vkResetFences) then begin
   @vkResetFences:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkResetFences'));
   @vk.fCommands.ResetFences:=addr(vkResetFences);
  end;
  if not assigned(vkGetFenceStatus) then begin
   @vkGetFenceStatus:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetFenceStatus'));
   @vk.fCommands.GetFenceStatus:=addr(vkGetFenceStatus);
  end;
  if not assigned(vkWaitForFences) then begin
   @vkWaitForFences:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkWaitForFences'));
   @vk.fCommands.WaitForFences:=addr(vkWaitForFences);
  end;
  if not assigned(vkCreateSemaphore) then begin
   @vkCreateSemaphore:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateSemaphore'));
   @vk.fCommands.CreateSemaphore:=addr(vkCreateSemaphore);
  end;
  if not assigned(vkDestroySemaphore) then begin
   @vkDestroySemaphore:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroySemaphore'));
   @vk.fCommands.DestroySemaphore:=addr(vkDestroySemaphore);
  end;
  if not assigned(vkCreateEvent) then begin
   @vkCreateEvent:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateEvent'));
   @vk.fCommands.CreateEvent:=addr(vkCreateEvent);
  end;
  if not assigned(vkDestroyEvent) then begin
   @vkDestroyEvent:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroyEvent'));
   @vk.fCommands.DestroyEvent:=addr(vkDestroyEvent);
  end;
  if not assigned(vkGetEventStatus) then begin
   @vkGetEventStatus:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetEventStatus'));
   @vk.fCommands.GetEventStatus:=addr(vkGetEventStatus);
  end;
  if not assigned(vkSetEvent) then begin
   @vkSetEvent:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkSetEvent'));
   @vk.fCommands.SetEvent:=addr(vkSetEvent);
  end;
  if not assigned(vkResetEvent) then begin
   @vkResetEvent:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkResetEvent'));
   @vk.fCommands.ResetEvent:=addr(vkResetEvent);
  end;
  if not assigned(vkCreateQueryPool) then begin
   @vkCreateQueryPool:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateQueryPool'));
   @vk.fCommands.CreateQueryPool:=addr(vkCreateQueryPool);
  end;
  if not assigned(vkDestroyQueryPool) then begin
   @vkDestroyQueryPool:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroyQueryPool'));
   @vk.fCommands.DestroyQueryPool:=addr(vkDestroyQueryPool);
  end;
  if not assigned(vkGetQueryPoolResults) then begin
   @vkGetQueryPoolResults:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetQueryPoolResults'));
   @vk.fCommands.GetQueryPoolResults:=addr(vkGetQueryPoolResults);
  end;
  if not assigned(vkCreateBuffer) then begin
   @vkCreateBuffer:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateBuffer'));
   @vk.fCommands.CreateBuffer:=addr(vkCreateBuffer);
  end;
  if not assigned(vkDestroyBuffer) then begin
   @vkDestroyBuffer:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroyBuffer'));
   @vk.fCommands.DestroyBuffer:=addr(vkDestroyBuffer);
  end;
  if not assigned(vkCreateBufferView) then begin
   @vkCreateBufferView:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateBufferView'));
   @vk.fCommands.CreateBufferView:=addr(vkCreateBufferView);
  end;
  if not assigned(vkDestroyBufferView) then begin
   @vkDestroyBufferView:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroyBufferView'));
   @vk.fCommands.DestroyBufferView:=addr(vkDestroyBufferView);
  end;
  if not assigned(vkCreateImage) then begin
   @vkCreateImage:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateImage'));
   @vk.fCommands.CreateImage:=addr(vkCreateImage);
  end;
  if not assigned(vkDestroyImage) then begin
   @vkDestroyImage:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroyImage'));
   @vk.fCommands.DestroyImage:=addr(vkDestroyImage);
  end;
  if not assigned(vkGetImageSubresourceLayout) then begin
   @vkGetImageSubresourceLayout:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetImageSubresourceLayout'));
   @vk.fCommands.GetImageSubresourceLayout:=addr(vkGetImageSubresourceLayout);
  end;
  if not assigned(vkCreateImageView) then begin
   @vkCreateImageView:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateImageView'));
   @vk.fCommands.CreateImageView:=addr(vkCreateImageView);
  end;
  if not assigned(vkDestroyImageView) then begin
   @vkDestroyImageView:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroyImageView'));
   @vk.fCommands.DestroyImageView:=addr(vkDestroyImageView);
  end;
  if not assigned(vkCreateShaderModule) then begin
   @vkCreateShaderModule:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateShaderModule'));
   @vk.fCommands.CreateShaderModule:=addr(vkCreateShaderModule);
  end;
  if not assigned(vkDestroyShaderModule) then begin
   @vkDestroyShaderModule:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroyShaderModule'));
   @vk.fCommands.DestroyShaderModule:=addr(vkDestroyShaderModule);
  end;
  if not assigned(vkCreatePipelineCache) then begin
   @vkCreatePipelineCache:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreatePipelineCache'));
   @vk.fCommands.CreatePipelineCache:=addr(vkCreatePipelineCache);
  end;
  if not assigned(vkDestroyPipelineCache) then begin
   @vkDestroyPipelineCache:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroyPipelineCache'));
   @vk.fCommands.DestroyPipelineCache:=addr(vkDestroyPipelineCache);
  end;
  if not assigned(vkGetPipelineCacheData) then begin
   @vkGetPipelineCacheData:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPipelineCacheData'));
   @vk.fCommands.GetPipelineCacheData:=addr(vkGetPipelineCacheData);
  end;
  if not assigned(vkMergePipelineCaches) then begin
   @vkMergePipelineCaches:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkMergePipelineCaches'));
   @vk.fCommands.MergePipelineCaches:=addr(vkMergePipelineCaches);
  end;
  if not assigned(vkCreateGraphicsPipelines) then begin
   @vkCreateGraphicsPipelines:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateGraphicsPipelines'));
   @vk.fCommands.CreateGraphicsPipelines:=addr(vkCreateGraphicsPipelines);
  end;
  if not assigned(vkCreateComputePipelines) then begin
   @vkCreateComputePipelines:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateComputePipelines'));
   @vk.fCommands.CreateComputePipelines:=addr(vkCreateComputePipelines);
  end;
  if not assigned(vkDestroyPipeline) then begin
   @vkDestroyPipeline:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroyPipeline'));
   @vk.fCommands.DestroyPipeline:=addr(vkDestroyPipeline);
  end;
  if not assigned(vkCreatePipelineLayout) then begin
   @vkCreatePipelineLayout:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreatePipelineLayout'));
   @vk.fCommands.CreatePipelineLayout:=addr(vkCreatePipelineLayout);
  end;
  if not assigned(vkDestroyPipelineLayout) then begin
   @vkDestroyPipelineLayout:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroyPipelineLayout'));
   @vk.fCommands.DestroyPipelineLayout:=addr(vkDestroyPipelineLayout);
  end;
  if not assigned(vkCreateSampler) then begin
   @vkCreateSampler:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateSampler'));
   @vk.fCommands.CreateSampler:=addr(vkCreateSampler);
  end;
  if not assigned(vkDestroySampler) then begin
   @vkDestroySampler:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroySampler'));
   @vk.fCommands.DestroySampler:=addr(vkDestroySampler);
  end;
  if not assigned(vkCreateDescriptorSetLayout) then begin
   @vkCreateDescriptorSetLayout:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateDescriptorSetLayout'));
   @vk.fCommands.CreateDescriptorSetLayout:=addr(vkCreateDescriptorSetLayout);
  end;
  if not assigned(vkDestroyDescriptorSetLayout) then begin
   @vkDestroyDescriptorSetLayout:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroyDescriptorSetLayout'));
   @vk.fCommands.DestroyDescriptorSetLayout:=addr(vkDestroyDescriptorSetLayout);
  end;
  if not assigned(vkCreateDescriptorPool) then begin
   @vkCreateDescriptorPool:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateDescriptorPool'));
   @vk.fCommands.CreateDescriptorPool:=addr(vkCreateDescriptorPool);
  end;
  if not assigned(vkDestroyDescriptorPool) then begin
   @vkDestroyDescriptorPool:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroyDescriptorPool'));
   @vk.fCommands.DestroyDescriptorPool:=addr(vkDestroyDescriptorPool);
  end;
  if not assigned(vkResetDescriptorPool) then begin
   @vkResetDescriptorPool:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkResetDescriptorPool'));
   @vk.fCommands.ResetDescriptorPool:=addr(vkResetDescriptorPool);
  end;
  if not assigned(vkAllocateDescriptorSets) then begin
   @vkAllocateDescriptorSets:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkAllocateDescriptorSets'));
   @vk.fCommands.AllocateDescriptorSets:=addr(vkAllocateDescriptorSets);
  end;
  if not assigned(vkFreeDescriptorSets) then begin
   @vkFreeDescriptorSets:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkFreeDescriptorSets'));
   @vk.fCommands.FreeDescriptorSets:=addr(vkFreeDescriptorSets);
  end;
  if not assigned(vkUpdateDescriptorSets) then begin
   @vkUpdateDescriptorSets:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkUpdateDescriptorSets'));
   @vk.fCommands.UpdateDescriptorSets:=addr(vkUpdateDescriptorSets);
  end;
  if not assigned(vkCreateFramebuffer) then begin
   @vkCreateFramebuffer:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateFramebuffer'));
   @vk.fCommands.CreateFramebuffer:=addr(vkCreateFramebuffer);
  end;
  if not assigned(vkDestroyFramebuffer) then begin
   @vkDestroyFramebuffer:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroyFramebuffer'));
   @vk.fCommands.DestroyFramebuffer:=addr(vkDestroyFramebuffer);
  end;
  if not assigned(vkCreateRenderPass) then begin
   @vkCreateRenderPass:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateRenderPass'));
   @vk.fCommands.CreateRenderPass:=addr(vkCreateRenderPass);
  end;
  if not assigned(vkDestroyRenderPass) then begin
   @vkDestroyRenderPass:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroyRenderPass'));
   @vk.fCommands.DestroyRenderPass:=addr(vkDestroyRenderPass);
  end;
  if not assigned(vkGetRenderAreaGranularity) then begin
   @vkGetRenderAreaGranularity:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetRenderAreaGranularity'));
   @vk.fCommands.GetRenderAreaGranularity:=addr(vkGetRenderAreaGranularity);
  end;
  if not assigned(vkCreateCommandPool) then begin
   @vkCreateCommandPool:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateCommandPool'));
   @vk.fCommands.CreateCommandPool:=addr(vkCreateCommandPool);
  end;
  if not assigned(vkDestroyCommandPool) then begin
   @vkDestroyCommandPool:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroyCommandPool'));
   @vk.fCommands.DestroyCommandPool:=addr(vkDestroyCommandPool);
  end;
  if not assigned(vkResetCommandPool) then begin
   @vkResetCommandPool:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkResetCommandPool'));
   @vk.fCommands.ResetCommandPool:=addr(vkResetCommandPool);
  end;
  if not assigned(vkAllocateCommandBuffers) then begin
   @vkAllocateCommandBuffers:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkAllocateCommandBuffers'));
   @vk.fCommands.AllocateCommandBuffers:=addr(vkAllocateCommandBuffers);
  end;
  if not assigned(vkFreeCommandBuffers) then begin
   @vkFreeCommandBuffers:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkFreeCommandBuffers'));
   @vk.fCommands.FreeCommandBuffers:=addr(vkFreeCommandBuffers);
  end;
  if not assigned(vkBeginCommandBuffer) then begin
   @vkBeginCommandBuffer:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkBeginCommandBuffer'));
   @vk.fCommands.BeginCommandBuffer:=addr(vkBeginCommandBuffer);
  end;
  if not assigned(vkEndCommandBuffer) then begin
   @vkEndCommandBuffer:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkEndCommandBuffer'));
   @vk.fCommands.EndCommandBuffer:=addr(vkEndCommandBuffer);
  end;
  if not assigned(vkResetCommandBuffer) then begin
   @vkResetCommandBuffer:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkResetCommandBuffer'));
   @vk.fCommands.ResetCommandBuffer:=addr(vkResetCommandBuffer);
  end;
  if not assigned(vkCmdBindPipeline) then begin
   @vkCmdBindPipeline:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdBindPipeline'));
   @vk.fCommands.CmdBindPipeline:=addr(vkCmdBindPipeline);
  end;
  if not assigned(vkCmdSetViewport) then begin
   @vkCmdSetViewport:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdSetViewport'));
   @vk.fCommands.CmdSetViewport:=addr(vkCmdSetViewport);
  end;
  if not assigned(vkCmdSetScissor) then begin
   @vkCmdSetScissor:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdSetScissor'));
   @vk.fCommands.CmdSetScissor:=addr(vkCmdSetScissor);
  end;
  if not assigned(vkCmdSetLineWidth) then begin
   @vkCmdSetLineWidth:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdSetLineWidth'));
   @vk.fCommands.CmdSetLineWidth:=addr(vkCmdSetLineWidth);
  end;
  if not assigned(vkCmdSetDepthBias) then begin
   @vkCmdSetDepthBias:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdSetDepthBias'));
   @vk.fCommands.CmdSetDepthBias:=addr(vkCmdSetDepthBias);
  end;
  if not assigned(vkCmdSetBlendConstants) then begin
   @vkCmdSetBlendConstants:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdSetBlendConstants'));
   @vk.fCommands.CmdSetBlendConstants:=addr(vkCmdSetBlendConstants);
  end;
  if not assigned(vkCmdSetDepthBounds) then begin
   @vkCmdSetDepthBounds:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdSetDepthBounds'));
   @vk.fCommands.CmdSetDepthBounds:=addr(vkCmdSetDepthBounds);
  end;
  if not assigned(vkCmdSetStencilCompareMask) then begin
   @vkCmdSetStencilCompareMask:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdSetStencilCompareMask'));
   @vk.fCommands.CmdSetStencilCompareMask:=addr(vkCmdSetStencilCompareMask);
  end;
  if not assigned(vkCmdSetStencilWriteMask) then begin
   @vkCmdSetStencilWriteMask:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdSetStencilWriteMask'));
   @vk.fCommands.CmdSetStencilWriteMask:=addr(vkCmdSetStencilWriteMask);
  end;
  if not assigned(vkCmdSetStencilReference) then begin
   @vkCmdSetStencilReference:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdSetStencilReference'));
   @vk.fCommands.CmdSetStencilReference:=addr(vkCmdSetStencilReference);
  end;
  if not assigned(vkCmdBindDescriptorSets) then begin
   @vkCmdBindDescriptorSets:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdBindDescriptorSets'));
   @vk.fCommands.CmdBindDescriptorSets:=addr(vkCmdBindDescriptorSets);
  end;
  if not assigned(vkCmdBindIndexBuffer) then begin
   @vkCmdBindIndexBuffer:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdBindIndexBuffer'));
   @vk.fCommands.CmdBindIndexBuffer:=addr(vkCmdBindIndexBuffer);
  end;
  if not assigned(vkCmdBindVertexBuffers) then begin
   @vkCmdBindVertexBuffers:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdBindVertexBuffers'));
   @vk.fCommands.CmdBindVertexBuffers:=addr(vkCmdBindVertexBuffers);
  end;
  if not assigned(vkCmdDraw) then begin
   @vkCmdDraw:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdDraw'));
   @vk.fCommands.CmdDraw:=addr(vkCmdDraw);
  end;
  if not assigned(vkCmdDrawIndexed) then begin
   @vkCmdDrawIndexed:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdDrawIndexed'));
   @vk.fCommands.CmdDrawIndexed:=addr(vkCmdDrawIndexed);
  end;
  if not assigned(vkCmdDrawIndirect) then begin
   @vkCmdDrawIndirect:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdDrawIndirect'));
   @vk.fCommands.CmdDrawIndirect:=addr(vkCmdDrawIndirect);
  end;
  if not assigned(vkCmdDrawIndexedIndirect) then begin
   @vkCmdDrawIndexedIndirect:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdDrawIndexedIndirect'));
   @vk.fCommands.CmdDrawIndexedIndirect:=addr(vkCmdDrawIndexedIndirect);
  end;
  if not assigned(vkCmdDispatch) then begin
   @vkCmdDispatch:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdDispatch'));
   @vk.fCommands.CmdDispatch:=addr(vkCmdDispatch);
  end;
  if not assigned(vkCmdDispatchIndirect) then begin
   @vkCmdDispatchIndirect:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdDispatchIndirect'));
   @vk.fCommands.CmdDispatchIndirect:=addr(vkCmdDispatchIndirect);
  end;
  if not assigned(vkCmdCopyBuffer) then begin
   @vkCmdCopyBuffer:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdCopyBuffer'));
   @vk.fCommands.CmdCopyBuffer:=addr(vkCmdCopyBuffer);
  end;
  if not assigned(vkCmdCopyImage) then begin
   @vkCmdCopyImage:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdCopyImage'));
   @vk.fCommands.CmdCopyImage:=addr(vkCmdCopyImage);
  end;
  if not assigned(vkCmdBlitImage) then begin
   @vkCmdBlitImage:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdBlitImage'));
   @vk.fCommands.CmdBlitImage:=addr(vkCmdBlitImage);
  end;
  if not assigned(vkCmdCopyBufferToImage) then begin
   @vkCmdCopyBufferToImage:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdCopyBufferToImage'));
   @vk.fCommands.CmdCopyBufferToImage:=addr(vkCmdCopyBufferToImage);
  end;
  if not assigned(vkCmdCopyImageToBuffer) then begin
   @vkCmdCopyImageToBuffer:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdCopyImageToBuffer'));
   @vk.fCommands.CmdCopyImageToBuffer:=addr(vkCmdCopyImageToBuffer);
  end;
  if not assigned(vkCmdUpdateBuffer) then begin
   @vkCmdUpdateBuffer:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdUpdateBuffer'));
   @vk.fCommands.CmdUpdateBuffer:=addr(vkCmdUpdateBuffer);
  end;
  if not assigned(vkCmdFillBuffer) then begin
   @vkCmdFillBuffer:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdFillBuffer'));
   @vk.fCommands.CmdFillBuffer:=addr(vkCmdFillBuffer);
  end;
  if not assigned(vkCmdClearColorImage) then begin
   @vkCmdClearColorImage:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdClearColorImage'));
   @vk.fCommands.CmdClearColorImage:=addr(vkCmdClearColorImage);
  end;
  if not assigned(vkCmdClearDepthStencilImage) then begin
   @vkCmdClearDepthStencilImage:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdClearDepthStencilImage'));
   @vk.fCommands.CmdClearDepthStencilImage:=addr(vkCmdClearDepthStencilImage);
  end;
  if not assigned(vkCmdClearAttachments) then begin
   @vkCmdClearAttachments:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdClearAttachments'));
   @vk.fCommands.CmdClearAttachments:=addr(vkCmdClearAttachments);
  end;
  if not assigned(vkCmdResolveImage) then begin
   @vkCmdResolveImage:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdResolveImage'));
   @vk.fCommands.CmdResolveImage:=addr(vkCmdResolveImage);
  end;
  if not assigned(vkCmdSetEvent) then begin
   @vkCmdSetEvent:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdSetEvent'));
   @vk.fCommands.CmdSetEvent:=addr(vkCmdSetEvent);
  end;
  if not assigned(vkCmdResetEvent) then begin
   @vkCmdResetEvent:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdResetEvent'));
   @vk.fCommands.CmdResetEvent:=addr(vkCmdResetEvent);
  end;
  if not assigned(vkCmdWaitEvents) then begin
   @vkCmdWaitEvents:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdWaitEvents'));
   @vk.fCommands.CmdWaitEvents:=addr(vkCmdWaitEvents);
  end;
  if not assigned(vkCmdPipelineBarrier) then begin
   @vkCmdPipelineBarrier:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdPipelineBarrier'));
   @vk.fCommands.CmdPipelineBarrier:=addr(vkCmdPipelineBarrier);
  end;
  if not assigned(vkCmdBeginQuery) then begin
   @vkCmdBeginQuery:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdBeginQuery'));
   @vk.fCommands.CmdBeginQuery:=addr(vkCmdBeginQuery);
  end;
  if not assigned(vkCmdEndQuery) then begin
   @vkCmdEndQuery:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdEndQuery'));
   @vk.fCommands.CmdEndQuery:=addr(vkCmdEndQuery);
  end;
  if not assigned(vkCmdResetQueryPool) then begin
   @vkCmdResetQueryPool:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdResetQueryPool'));
   @vk.fCommands.CmdResetQueryPool:=addr(vkCmdResetQueryPool);
  end;
  if not assigned(vkCmdWriteTimestamp) then begin
   @vkCmdWriteTimestamp:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdWriteTimestamp'));
   @vk.fCommands.CmdWriteTimestamp:=addr(vkCmdWriteTimestamp);
  end;
  if not assigned(vkCmdCopyQueryPoolResults) then begin
   @vkCmdCopyQueryPoolResults:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdCopyQueryPoolResults'));
   @vk.fCommands.CmdCopyQueryPoolResults:=addr(vkCmdCopyQueryPoolResults);
  end;
  if not assigned(vkCmdPushConstants) then begin
   @vkCmdPushConstants:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdPushConstants'));
   @vk.fCommands.CmdPushConstants:=addr(vkCmdPushConstants);
  end;
  if not assigned(vkCmdBeginRenderPass) then begin
   @vkCmdBeginRenderPass:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdBeginRenderPass'));
   @vk.fCommands.CmdBeginRenderPass:=addr(vkCmdBeginRenderPass);
  end;
  if not assigned(vkCmdNextSubpass) then begin
   @vkCmdNextSubpass:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdNextSubpass'));
   @vk.fCommands.CmdNextSubpass:=addr(vkCmdNextSubpass);
  end;
  if not assigned(vkCmdEndRenderPass) then begin
   @vkCmdEndRenderPass:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdEndRenderPass'));
   @vk.fCommands.CmdEndRenderPass:=addr(vkCmdEndRenderPass);
  end;
  if not assigned(vkCmdExecuteCommands) then begin
   @vkCmdExecuteCommands:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdExecuteCommands'));
   @vk.fCommands.CmdExecuteCommands:=addr(vkCmdExecuteCommands);
  end;
{$ifdef Android}
  if not assigned(vkCreateAndroidSurfaceKHR) then begin
   @vkCreateAndroidSurfaceKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateAndroidSurfaceKHR'));
   @vk.fCommands.CreateAndroidSurfaceKHR:=addr(vkCreateAndroidSurfaceKHR);
  end;
{$endif}
  if not assigned(vkGetPhysicalDeviceDisplayPropertiesKHR) then begin
   @vkGetPhysicalDeviceDisplayPropertiesKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceDisplayPropertiesKHR'));
   @vk.fCommands.GetPhysicalDeviceDisplayPropertiesKHR:=addr(vkGetPhysicalDeviceDisplayPropertiesKHR);
  end;
  if not assigned(vkGetPhysicalDeviceDisplayPlanePropertiesKHR) then begin
   @vkGetPhysicalDeviceDisplayPlanePropertiesKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceDisplayPlanePropertiesKHR'));
   @vk.fCommands.GetPhysicalDeviceDisplayPlanePropertiesKHR:=addr(vkGetPhysicalDeviceDisplayPlanePropertiesKHR);
  end;
  if not assigned(vkGetDisplayPlaneSupportedDisplaysKHR) then begin
   @vkGetDisplayPlaneSupportedDisplaysKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetDisplayPlaneSupportedDisplaysKHR'));
   @vk.fCommands.GetDisplayPlaneSupportedDisplaysKHR:=addr(vkGetDisplayPlaneSupportedDisplaysKHR);
  end;
  if not assigned(vkGetDisplayModePropertiesKHR) then begin
   @vkGetDisplayModePropertiesKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetDisplayModePropertiesKHR'));
   @vk.fCommands.GetDisplayModePropertiesKHR:=addr(vkGetDisplayModePropertiesKHR);
  end;
  if not assigned(vkCreateDisplayModeKHR) then begin
   @vkCreateDisplayModeKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateDisplayModeKHR'));
   @vk.fCommands.CreateDisplayModeKHR:=addr(vkCreateDisplayModeKHR);
  end;
  if not assigned(vkGetDisplayPlaneCapabilitiesKHR) then begin
   @vkGetDisplayPlaneCapabilitiesKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetDisplayPlaneCapabilitiesKHR'));
   @vk.fCommands.GetDisplayPlaneCapabilitiesKHR:=addr(vkGetDisplayPlaneCapabilitiesKHR);
  end;
  if not assigned(vkCreateDisplayPlaneSurfaceKHR) then begin
   @vkCreateDisplayPlaneSurfaceKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateDisplayPlaneSurfaceKHR'));
   @vk.fCommands.CreateDisplayPlaneSurfaceKHR:=addr(vkCreateDisplayPlaneSurfaceKHR);
  end;
  if not assigned(vkCreateSharedSwapchainsKHR) then begin
   @vkCreateSharedSwapchainsKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateSharedSwapchainsKHR'));
   @vk.fCommands.CreateSharedSwapchainsKHR:=addr(vkCreateSharedSwapchainsKHR);
  end;
{$ifdef Mir}
  if not assigned(vkCreateMirSurfaceKHR) then begin
   @vkCreateMirSurfaceKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateMirSurfaceKHR'));
   @vk.fCommands.CreateMirSurfaceKHR:=addr(vkCreateMirSurfaceKHR);
  end;
{$endif}
{$ifdef Mir}
  if not assigned(vkGetPhysicalDeviceMirPresentationSupportKHR) then begin
   @vkGetPhysicalDeviceMirPresentationSupportKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceMirPresentationSupportKHR'));
   @vk.fCommands.GetPhysicalDeviceMirPresentationSupportKHR:=addr(vkGetPhysicalDeviceMirPresentationSupportKHR);
  end;
{$endif}
  if not assigned(vkDestroySurfaceKHR) then begin
   @vkDestroySurfaceKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroySurfaceKHR'));
   @vk.fCommands.DestroySurfaceKHR:=addr(vkDestroySurfaceKHR);
  end;
  if not assigned(vkGetPhysicalDeviceSurfaceSupportKHR) then begin
   @vkGetPhysicalDeviceSurfaceSupportKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceSurfaceSupportKHR'));
   @vk.fCommands.GetPhysicalDeviceSurfaceSupportKHR:=addr(vkGetPhysicalDeviceSurfaceSupportKHR);
  end;
  if not assigned(vkGetPhysicalDeviceSurfaceCapabilitiesKHR) then begin
   @vkGetPhysicalDeviceSurfaceCapabilitiesKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceSurfaceCapabilitiesKHR'));
   @vk.fCommands.GetPhysicalDeviceSurfaceCapabilitiesKHR:=addr(vkGetPhysicalDeviceSurfaceCapabilitiesKHR);
  end;
  if not assigned(vkGetPhysicalDeviceSurfaceFormatsKHR) then begin
   @vkGetPhysicalDeviceSurfaceFormatsKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceSurfaceFormatsKHR'));
   @vk.fCommands.GetPhysicalDeviceSurfaceFormatsKHR:=addr(vkGetPhysicalDeviceSurfaceFormatsKHR);
  end;
  if not assigned(vkGetPhysicalDeviceSurfacePresentModesKHR) then begin
   @vkGetPhysicalDeviceSurfacePresentModesKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceSurfacePresentModesKHR'));
   @vk.fCommands.GetPhysicalDeviceSurfacePresentModesKHR:=addr(vkGetPhysicalDeviceSurfacePresentModesKHR);
  end;
  if not assigned(vkCreateSwapchainKHR) then begin
   @vkCreateSwapchainKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateSwapchainKHR'));
   @vk.fCommands.CreateSwapchainKHR:=addr(vkCreateSwapchainKHR);
  end;
  if not assigned(vkDestroySwapchainKHR) then begin
   @vkDestroySwapchainKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroySwapchainKHR'));
   @vk.fCommands.DestroySwapchainKHR:=addr(vkDestroySwapchainKHR);
  end;
  if not assigned(vkGetSwapchainImagesKHR) then begin
   @vkGetSwapchainImagesKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetSwapchainImagesKHR'));
   @vk.fCommands.GetSwapchainImagesKHR:=addr(vkGetSwapchainImagesKHR);
  end;
  if not assigned(vkAcquireNextImageKHR) then begin
   @vkAcquireNextImageKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkAcquireNextImageKHR'));
   @vk.fCommands.AcquireNextImageKHR:=addr(vkAcquireNextImageKHR);
  end;
  if not assigned(vkQueuePresentKHR) then begin
   @vkQueuePresentKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkQueuePresentKHR'));
   @vk.fCommands.QueuePresentKHR:=addr(vkQueuePresentKHR);
  end;
  if not assigned(vkCreateViSurfaceNN) then begin
   @vkCreateViSurfaceNN:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateViSurfaceNN'));
   @vk.fCommands.CreateViSurfaceNN:=addr(vkCreateViSurfaceNN);
  end;
{$ifdef Wayland}
  if not assigned(vkCreateWaylandSurfaceKHR) then begin
   @vkCreateWaylandSurfaceKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateWaylandSurfaceKHR'));
   @vk.fCommands.CreateWaylandSurfaceKHR:=addr(vkCreateWaylandSurfaceKHR);
  end;
{$endif}
{$ifdef Wayland}
  if not assigned(vkGetPhysicalDeviceWaylandPresentationSupportKHR) then begin
   @vkGetPhysicalDeviceWaylandPresentationSupportKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceWaylandPresentationSupportKHR'));
   @vk.fCommands.GetPhysicalDeviceWaylandPresentationSupportKHR:=addr(vkGetPhysicalDeviceWaylandPresentationSupportKHR);
  end;
{$endif}
{$ifdef Windows}
  if not assigned(vkCreateWin32SurfaceKHR) then begin
   @vkCreateWin32SurfaceKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateWin32SurfaceKHR'));
   @vk.fCommands.CreateWin32SurfaceKHR:=addr(vkCreateWin32SurfaceKHR);
  end;
{$endif}
{$ifdef Windows}
  if not assigned(vkGetPhysicalDeviceWin32PresentationSupportKHR) then begin
   @vkGetPhysicalDeviceWin32PresentationSupportKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceWin32PresentationSupportKHR'));
   @vk.fCommands.GetPhysicalDeviceWin32PresentationSupportKHR:=addr(vkGetPhysicalDeviceWin32PresentationSupportKHR);
  end;
{$endif}
{$ifdef X11}
  if not assigned(vkCreateXlibSurfaceKHR) then begin
   @vkCreateXlibSurfaceKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateXlibSurfaceKHR'));
   @vk.fCommands.CreateXlibSurfaceKHR:=addr(vkCreateXlibSurfaceKHR);
  end;
{$endif}
{$ifdef X11}
  if not assigned(vkGetPhysicalDeviceXlibPresentationSupportKHR) then begin
   @vkGetPhysicalDeviceXlibPresentationSupportKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceXlibPresentationSupportKHR'));
   @vk.fCommands.GetPhysicalDeviceXlibPresentationSupportKHR:=addr(vkGetPhysicalDeviceXlibPresentationSupportKHR);
  end;
{$endif}
{$ifdef XCB}
  if not assigned(vkCreateXcbSurfaceKHR) then begin
   @vkCreateXcbSurfaceKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateXcbSurfaceKHR'));
   @vk.fCommands.CreateXcbSurfaceKHR:=addr(vkCreateXcbSurfaceKHR);
  end;
{$endif}
{$ifdef XCB}
  if not assigned(vkGetPhysicalDeviceXcbPresentationSupportKHR) then begin
   @vkGetPhysicalDeviceXcbPresentationSupportKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceXcbPresentationSupportKHR'));
   @vk.fCommands.GetPhysicalDeviceXcbPresentationSupportKHR:=addr(vkGetPhysicalDeviceXcbPresentationSupportKHR);
  end;
{$endif}
  if not assigned(vkCreateDebugReportCallbackEXT) then begin
   @vkCreateDebugReportCallbackEXT:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateDebugReportCallbackEXT'));
   @vk.fCommands.CreateDebugReportCallbackEXT:=addr(vkCreateDebugReportCallbackEXT);
  end;
  if not assigned(vkDestroyDebugReportCallbackEXT) then begin
   @vkDestroyDebugReportCallbackEXT:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroyDebugReportCallbackEXT'));
   @vk.fCommands.DestroyDebugReportCallbackEXT:=addr(vkDestroyDebugReportCallbackEXT);
  end;
  if not assigned(vkDebugReportMessageEXT) then begin
   @vkDebugReportMessageEXT:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDebugReportMessageEXT'));
   @vk.fCommands.DebugReportMessageEXT:=addr(vkDebugReportMessageEXT);
  end;
  if not assigned(vkDebugMarkerSetObjectNameEXT) then begin
   @vkDebugMarkerSetObjectNameEXT:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDebugMarkerSetObjectNameEXT'));
   @vk.fCommands.DebugMarkerSetObjectNameEXT:=addr(vkDebugMarkerSetObjectNameEXT);
  end;
  if not assigned(vkDebugMarkerSetObjectTagEXT) then begin
   @vkDebugMarkerSetObjectTagEXT:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDebugMarkerSetObjectTagEXT'));
   @vk.fCommands.DebugMarkerSetObjectTagEXT:=addr(vkDebugMarkerSetObjectTagEXT);
  end;
  if not assigned(vkCmdDebugMarkerBeginEXT) then begin
   @vkCmdDebugMarkerBeginEXT:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdDebugMarkerBeginEXT'));
   @vk.fCommands.CmdDebugMarkerBeginEXT:=addr(vkCmdDebugMarkerBeginEXT);
  end;
  if not assigned(vkCmdDebugMarkerEndEXT) then begin
   @vkCmdDebugMarkerEndEXT:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdDebugMarkerEndEXT'));
   @vk.fCommands.CmdDebugMarkerEndEXT:=addr(vkCmdDebugMarkerEndEXT);
  end;
  if not assigned(vkCmdDebugMarkerInsertEXT) then begin
   @vkCmdDebugMarkerInsertEXT:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdDebugMarkerInsertEXT'));
   @vk.fCommands.CmdDebugMarkerInsertEXT:=addr(vkCmdDebugMarkerInsertEXT);
  end;
  if not assigned(vkGetPhysicalDeviceExternalImageFormatPropertiesNV) then begin
   @vkGetPhysicalDeviceExternalImageFormatPropertiesNV:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceExternalImageFormatPropertiesNV'));
   @vk.fCommands.GetPhysicalDeviceExternalImageFormatPropertiesNV:=addr(vkGetPhysicalDeviceExternalImageFormatPropertiesNV);
  end;
{$ifdef Windows}
  if not assigned(vkGetMemoryWin32HandleNV) then begin
   @vkGetMemoryWin32HandleNV:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetMemoryWin32HandleNV'));
   @vk.fCommands.GetMemoryWin32HandleNV:=addr(vkGetMemoryWin32HandleNV);
  end;
{$endif}
  if not assigned(vkCmdDrawIndirectCountAMD) then begin
   @vkCmdDrawIndirectCountAMD:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdDrawIndirectCountAMD'));
   @vk.fCommands.CmdDrawIndirectCountAMD:=addr(vkCmdDrawIndirectCountAMD);
  end;
  if not assigned(vkCmdDrawIndexedIndirectCountAMD) then begin
   @vkCmdDrawIndexedIndirectCountAMD:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdDrawIndexedIndirectCountAMD'));
   @vk.fCommands.CmdDrawIndexedIndirectCountAMD:=addr(vkCmdDrawIndexedIndirectCountAMD);
  end;
  if not assigned(vkCmdProcessCommandsNVX) then begin
   @vkCmdProcessCommandsNVX:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdProcessCommandsNVX'));
   @vk.fCommands.CmdProcessCommandsNVX:=addr(vkCmdProcessCommandsNVX);
  end;
  if not assigned(vkCmdReserveSpaceForCommandsNVX) then begin
   @vkCmdReserveSpaceForCommandsNVX:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCmdReserveSpaceForCommandsNVX'));
   @vk.fCommands.CmdReserveSpaceForCommandsNVX:=addr(vkCmdReserveSpaceForCommandsNVX);
  end;
  if not assigned(vkCreateIndirectCommandsLayoutNVX) then begin
   @vkCreateIndirectCommandsLayoutNVX:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateIndirectCommandsLayoutNVX'));
   @vk.fCommands.CreateIndirectCommandsLayoutNVX:=addr(vkCreateIndirectCommandsLayoutNVX);
  end;
  if not assigned(vkDestroyIndirectCommandsLayoutNVX) then begin
   @vkDestroyIndirectCommandsLayoutNVX:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroyIndirectCommandsLayoutNVX'));
   @vk.fCommands.DestroyIndirectCommandsLayoutNVX:=addr(vkDestroyIndirectCommandsLayoutNVX);
  end;
  if not assigned(vkCreateObjectTableNVX) then begin
   @vkCreateObjectTableNVX:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkCreateObjectTableNVX'));
   @vk.fCommands.CreateObjectTableNVX:=addr(vkCreateObjectTableNVX);
  end;
  if not assigned(vkDestroyObjectTableNVX) then begin
   @vkDestroyObjectTableNVX:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDestroyObjectTableNVX'));
   @vk.fCommands.DestroyObjectTableNVX:=addr(vkDestroyObjectTableNVX);
  end;
  if not assigned(vkRegisterObjectsNVX) then begin
   @vkRegisterObjectsNVX:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkRegisterObjectsNVX'));
   @vk.fCommands.RegisterObjectsNVX:=addr(vkRegisterObjectsNVX);
  end;
  if not assigned(vkUnregisterObjectsNVX) then begin
   @vkUnregisterObjectsNVX:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkUnregisterObjectsNVX'));
   @vk.fCommands.UnregisterObjectsNVX:=addr(vkUnregisterObjectsNVX);
  end;
  if not assigned(vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX) then begin
   @vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX'));
   @vk.fCommands.GetPhysicalDeviceGeneratedCommandsPropertiesNVX:=addr(vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX);
  end;
  if not assigned(vkGetPhysicalDeviceFeatures2KHR) then begin
   @vkGetPhysicalDeviceFeatures2KHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceFeatures2KHR'));
   @vk.fCommands.GetPhysicalDeviceFeatures2KHR:=addr(vkGetPhysicalDeviceFeatures2KHR);
  end;
  if not assigned(vkGetPhysicalDeviceProperties2KHR) then begin
   @vkGetPhysicalDeviceProperties2KHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceProperties2KHR'));
   @vk.fCommands.GetPhysicalDeviceProperties2KHR:=addr(vkGetPhysicalDeviceProperties2KHR);
  end;
  if not assigned(vkGetPhysicalDeviceFormatProperties2KHR) then begin
   @vkGetPhysicalDeviceFormatProperties2KHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceFormatProperties2KHR'));
   @vk.fCommands.GetPhysicalDeviceFormatProperties2KHR:=addr(vkGetPhysicalDeviceFormatProperties2KHR);
  end;
  if not assigned(vkGetPhysicalDeviceImageFormatProperties2KHR) then begin
   @vkGetPhysicalDeviceImageFormatProperties2KHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceImageFormatProperties2KHR'));
   @vk.fCommands.GetPhysicalDeviceImageFormatProperties2KHR:=addr(vkGetPhysicalDeviceImageFormatProperties2KHR);
  end;
  if not assigned(vkGetPhysicalDeviceQueueFamilyProperties2KHR) then begin
   @vkGetPhysicalDeviceQueueFamilyProperties2KHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceQueueFamilyProperties2KHR'));
   @vk.fCommands.GetPhysicalDeviceQueueFamilyProperties2KHR:=addr(vkGetPhysicalDeviceQueueFamilyProperties2KHR);
  end;
  if not assigned(vkGetPhysicalDeviceMemoryProperties2KHR) then begin
   @vkGetPhysicalDeviceMemoryProperties2KHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceMemoryProperties2KHR'));
   @vk.fCommands.GetPhysicalDeviceMemoryProperties2KHR:=addr(vkGetPhysicalDeviceMemoryProperties2KHR);
  end;
  if not assigned(vkGetPhysicalDeviceSparseImageFormatProperties2KHR) then begin
   @vkGetPhysicalDeviceSparseImageFormatProperties2KHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceSparseImageFormatProperties2KHR'));
   @vk.fCommands.GetPhysicalDeviceSparseImageFormatProperties2KHR:=addr(vkGetPhysicalDeviceSparseImageFormatProperties2KHR);
  end;
  if not assigned(vkTrimCommandPoolKHR) then begin
   @vkTrimCommandPoolKHR:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkTrimCommandPoolKHR'));
   @vk.fCommands.TrimCommandPoolKHR:=addr(vkTrimCommandPoolKHR);
  end;
  if not assigned(vkReleaseDisplayEXT) then begin
   @vkReleaseDisplayEXT:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkReleaseDisplayEXT'));
   @vk.fCommands.ReleaseDisplayEXT:=addr(vkReleaseDisplayEXT);
  end;
{$ifdef X11}
  if not assigned(vkAcquireXlibDisplayEXT) then begin
   @vkAcquireXlibDisplayEXT:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkAcquireXlibDisplayEXT'));
   @vk.fCommands.AcquireXlibDisplayEXT:=addr(vkAcquireXlibDisplayEXT);
  end;
{$endif}
{$ifdef X11}
  if not assigned(vkGetRandROutputDisplayEXT) then begin
   @vkGetRandROutputDisplayEXT:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetRandROutputDisplayEXT'));
   @vk.fCommands.GetRandROutputDisplayEXT:=addr(vkGetRandROutputDisplayEXT);
  end;
{$endif}
  if not assigned(vkDisplayPowerControlEXT) then begin
   @vkDisplayPowerControlEXT:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkDisplayPowerControlEXT'));
   @vk.fCommands.DisplayPowerControlEXT:=addr(vkDisplayPowerControlEXT);
  end;
  if not assigned(vkRegisterDeviceEventEXT) then begin
   @vkRegisterDeviceEventEXT:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkRegisterDeviceEventEXT'));
   @vk.fCommands.RegisterDeviceEventEXT:=addr(vkRegisterDeviceEventEXT);
  end;
  if not assigned(vkRegisterDisplayEventEXT) then begin
   @vkRegisterDisplayEventEXT:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkRegisterDisplayEventEXT'));
   @vk.fCommands.RegisterDisplayEventEXT:=addr(vkRegisterDisplayEventEXT);
  end;
  if not assigned(vkGetSwapchainCounterEXT) then begin
   @vkGetSwapchainCounterEXT:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetSwapchainCounterEXT'));
   @vk.fCommands.GetSwapchainCounterEXT:=addr(vkGetSwapchainCounterEXT);
  end;
  if not assigned(vkGetPhysicalDeviceSurfaceCapabilities2EXT) then begin
   @vkGetPhysicalDeviceSurfaceCapabilities2EXT:=vkVoidFunctionToPointer(vkGetProcAddress(LibVulkan,'vkGetPhysicalDeviceSurfaceCapabilities2EXT'));
   @vk.fCommands.GetPhysicalDeviceSurfaceCapabilities2EXT:=addr(vkGetPhysicalDeviceSurfaceCapabilities2EXT);
  end;
  result:=assigned(vkCreateInstance);
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
  @InstanceCommands.CreateViSurfaceNN:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateViSurfaceNN')));
{$ifdef Wayland}
  @InstanceCommands.CreateWaylandSurfaceKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateWaylandSurfaceKHR')));
{$endif}
{$ifdef Wayland}
  @InstanceCommands.GetPhysicalDeviceWaylandPresentationSupportKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceWaylandPresentationSupportKHR')));
{$endif}
{$ifdef Windows}
  @InstanceCommands.CreateWin32SurfaceKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateWin32SurfaceKHR')));
{$endif}
{$ifdef Windows}
  @InstanceCommands.GetPhysicalDeviceWin32PresentationSupportKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceWin32PresentationSupportKHR')));
{$endif}
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
  @InstanceCommands.DebugMarkerSetObjectNameEXT:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDebugMarkerSetObjectNameEXT')));
  @InstanceCommands.DebugMarkerSetObjectTagEXT:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDebugMarkerSetObjectTagEXT')));
  @InstanceCommands.CmdDebugMarkerBeginEXT:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdDebugMarkerBeginEXT')));
  @InstanceCommands.CmdDebugMarkerEndEXT:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdDebugMarkerEndEXT')));
  @InstanceCommands.CmdDebugMarkerInsertEXT:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdDebugMarkerInsertEXT')));
  @InstanceCommands.GetPhysicalDeviceExternalImageFormatPropertiesNV:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceExternalImageFormatPropertiesNV')));
{$ifdef Windows}
  @InstanceCommands.GetMemoryWin32HandleNV:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetMemoryWin32HandleNV')));
{$endif}
  @InstanceCommands.CmdDrawIndirectCountAMD:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdDrawIndirectCountAMD')));
  @InstanceCommands.CmdDrawIndexedIndirectCountAMD:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdDrawIndexedIndirectCountAMD')));
  @InstanceCommands.CmdProcessCommandsNVX:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdProcessCommandsNVX')));
  @InstanceCommands.CmdReserveSpaceForCommandsNVX:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCmdReserveSpaceForCommandsNVX')));
  @InstanceCommands.CreateIndirectCommandsLayoutNVX:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateIndirectCommandsLayoutNVX')));
  @InstanceCommands.DestroyIndirectCommandsLayoutNVX:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroyIndirectCommandsLayoutNVX')));
  @InstanceCommands.CreateObjectTableNVX:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkCreateObjectTableNVX')));
  @InstanceCommands.DestroyObjectTableNVX:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDestroyObjectTableNVX')));
  @InstanceCommands.RegisterObjectsNVX:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkRegisterObjectsNVX')));
  @InstanceCommands.UnregisterObjectsNVX:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkUnregisterObjectsNVX')));
  @InstanceCommands.GetPhysicalDeviceGeneratedCommandsPropertiesNVX:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX')));
  @InstanceCommands.GetPhysicalDeviceFeatures2KHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceFeatures2KHR')));
  @InstanceCommands.GetPhysicalDeviceProperties2KHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceProperties2KHR')));
  @InstanceCommands.GetPhysicalDeviceFormatProperties2KHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceFormatProperties2KHR')));
  @InstanceCommands.GetPhysicalDeviceImageFormatProperties2KHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceImageFormatProperties2KHR')));
  @InstanceCommands.GetPhysicalDeviceQueueFamilyProperties2KHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceQueueFamilyProperties2KHR')));
  @InstanceCommands.GetPhysicalDeviceMemoryProperties2KHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceMemoryProperties2KHR')));
  @InstanceCommands.GetPhysicalDeviceSparseImageFormatProperties2KHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceSparseImageFormatProperties2KHR')));
  @InstanceCommands.TrimCommandPoolKHR:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkTrimCommandPoolKHR')));
  @InstanceCommands.ReleaseDisplayEXT:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkReleaseDisplayEXT')));
{$ifdef X11}
  @InstanceCommands.AcquireXlibDisplayEXT:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkAcquireXlibDisplayEXT')));
{$endif}
{$ifdef X11}
  @InstanceCommands.GetRandROutputDisplayEXT:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetRandROutputDisplayEXT')));
{$endif}
  @InstanceCommands.DisplayPowerControlEXT:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkDisplayPowerControlEXT')));
  @InstanceCommands.RegisterDeviceEventEXT:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkRegisterDeviceEventEXT')));
  @InstanceCommands.RegisterDisplayEventEXT:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkRegisterDisplayEventEXT')));
  @InstanceCommands.GetSwapchainCounterEXT:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetSwapchainCounterEXT')));
  @InstanceCommands.GetPhysicalDeviceSurfaceCapabilities2EXT:=vkVoidFunctionToPointer(vkGetInstanceProcAddr(Instance,PVkChar('vkGetPhysicalDeviceSurfaceCapabilities2EXT')));
  if not assigned(InstanceCommands.EnumerateInstanceExtensionProperties) then begin
   InstanceCommands.EnumerateInstanceExtensionProperties:=addr(vkEnumerateInstanceExtensionProperties);
  end;
  if not assigned(InstanceCommands.EnumerateInstanceLayerProperties) then begin
   InstanceCommands.EnumerateInstanceLayerProperties:=addr(vkEnumerateInstanceLayerProperties);
  end;
  if not assigned(InstanceCommands.CreateInstance) then begin
   InstanceCommands.CreateInstance:=addr(vkCreateInstance);
  end;
  result:=assigned(InstanceCommands.DestroyInstance);
 end;
end;

function LoadVulkanDeviceCommands(const GetDeviceProcAddr:TvkGetDeviceProcAddr;const Device:TVkDevice;out DeviceCommands:TVulkanCommands):boolean;
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
  @DeviceCommands.DebugMarkerSetObjectNameEXT:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDebugMarkerSetObjectNameEXT')));
  @DeviceCommands.DebugMarkerSetObjectTagEXT:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDebugMarkerSetObjectTagEXT')));
  @DeviceCommands.CmdDebugMarkerBeginEXT:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdDebugMarkerBeginEXT')));
  @DeviceCommands.CmdDebugMarkerEndEXT:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdDebugMarkerEndEXT')));
  @DeviceCommands.CmdDebugMarkerInsertEXT:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdDebugMarkerInsertEXT')));
{$ifdef Windows}
  @DeviceCommands.GetMemoryWin32HandleNV:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetMemoryWin32HandleNV')));
{$endif}
  @DeviceCommands.CmdDrawIndirectCountAMD:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdDrawIndirectCountAMD')));
  @DeviceCommands.CmdDrawIndexedIndirectCountAMD:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdDrawIndexedIndirectCountAMD')));
  @DeviceCommands.CmdProcessCommandsNVX:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdProcessCommandsNVX')));
  @DeviceCommands.CmdReserveSpaceForCommandsNVX:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCmdReserveSpaceForCommandsNVX')));
  @DeviceCommands.CreateIndirectCommandsLayoutNVX:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateIndirectCommandsLayoutNVX')));
  @DeviceCommands.DestroyIndirectCommandsLayoutNVX:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyIndirectCommandsLayoutNVX')));
  @DeviceCommands.CreateObjectTableNVX:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkCreateObjectTableNVX')));
  @DeviceCommands.DestroyObjectTableNVX:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDestroyObjectTableNVX')));
  @DeviceCommands.RegisterObjectsNVX:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkRegisterObjectsNVX')));
  @DeviceCommands.UnregisterObjectsNVX:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkUnregisterObjectsNVX')));
  @DeviceCommands.TrimCommandPoolKHR:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkTrimCommandPoolKHR')));
  @DeviceCommands.DisplayPowerControlEXT:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkDisplayPowerControlEXT')));
  @DeviceCommands.RegisterDeviceEventEXT:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkRegisterDeviceEventEXT')));
  @DeviceCommands.RegisterDisplayEventEXT:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkRegisterDisplayEventEXT')));
  @DeviceCommands.GetSwapchainCounterEXT:=vkVoidFunctionToPointer(vkGetDeviceProcAddr(Device,PVkChar('vkGetSwapchainCounterEXT')));
  result:=assigned(DeviceCommands.DestroyDevice);
 end;
end;

{$ifdef HAS_ADVANCED_RECORDS}
constructor TVkOffset2D.Create(const pX:TVkInt32;
                               const pY:TVkInt32);
begin
 x:=pX;
 y:=pY;
end;

constructor TVkOffset3D.Create(const pX:TVkInt32;
                               const pY:TVkInt32;
                               const pZ:TVkInt32);
begin
 x:=pX;
 y:=pY;
 z:=pZ;
end;

constructor TVkExtent2D.Create(const pWidth:TVkUInt32;
                               const pHeight:TVkUInt32);
begin
 width:=pWidth;
 height:=pHeight;
end;

constructor TVkExtent3D.Create(const pWidth:TVkUInt32;
                               const pHeight:TVkUInt32;
                               const pDepth:TVkUInt32);
begin
 width:=pWidth;
 height:=pHeight;
 depth:=pDepth;
end;

constructor TVkViewport.Create(const pX:TVkFloat;
                               const pY:TVkFloat;
                               const pWidth:TVkFloat;
                               const pHeight:TVkFloat;
                               const pMinDepth:TVkFloat;
                               const pMaxDepth:TVkFloat);
begin
 x:=pX;
 y:=pY;
 width:=pWidth;
 height:=pHeight;
 minDepth:=pMinDepth;
 maxDepth:=pMaxDepth;
end;

constructor TVkRect2D.Create(const pOffset:TVkOffset2D;
                             const pExtent:TVkExtent2D);
begin
 offset:=pOffset;
 extent:=pExtent;
end;

constructor TVkRect3D.Create(const pOffset:TVkOffset3D;
                             const pExtent:TVkExtent3D);
begin
 offset:=pOffset;
 extent:=pExtent;
end;

constructor TVkClearRect.Create(const pRect:TVkRect2D;
                                const pBaseArrayLayer:TVkUInt32;
                                const pLayerCount:TVkUInt32);
begin
 rect:=pRect;
 baseArrayLayer:=pBaseArrayLayer;
 layerCount:=pLayerCount;
end;

constructor TVkComponentMapping.Create(const pR:TVkComponentSwizzle;
                                       const pG:TVkComponentSwizzle;
                                       const pB:TVkComponentSwizzle;
                                       const pA:TVkComponentSwizzle);
begin
 r:=pR;
 g:=pG;
 b:=pB;
 a:=pA;
end;

constructor TVkPhysicalDeviceSparseProperties.Create(const pResidencyStandard2DBlockShape:TVkBool32;
                                                     const pResidencyStandard2DMultisampleBlockShape:TVkBool32;
                                                     const pResidencyStandard3DBlockShape:TVkBool32;
                                                     const pResidencyAlignedMipSize:TVkBool32;
                                                     const pResidencyNonResidentStrict:TVkBool32);
begin
 residencyStandard2DBlockShape:=pResidencyStandard2DBlockShape;
 residencyStandard2DMultisampleBlockShape:=pResidencyStandard2DMultisampleBlockShape;
 residencyStandard3DBlockShape:=pResidencyStandard3DBlockShape;
 residencyAlignedMipSize:=pResidencyAlignedMipSize;
 residencyNonResidentStrict:=pResidencyNonResidentStrict;
end;

constructor TVkExtensionProperties.Create(const pExtensionName:TVkCharString;
                                          const pSpecVersion:TVkUInt32);
var ArrayItemCount:TVkInt32;
begin
 FillChar(self,SizeOf(TVkExtensionProperties),#0);
 ArrayItemCount:=length(pExtensionName);
 if ArrayItemCount>length(extensionName) then begin
  ArrayItemCount:=length(extensionName);
 end;
 if ArrayItemCount>0 then begin
  Move(pExtensionName[1],extensionName[0],ArrayItemCount*SizeOf(TVkChar));
 end;
 specVersion:=pSpecVersion;
end;

constructor TVkLayerProperties.Create(const pLayerName:TVkCharString;
                                      const pSpecVersion:TVkUInt32;
                                      const pImplementationVersion:TVkUInt32;
                                      const pDescription:TVkCharString);
var ArrayItemCount:TVkInt32;
begin
 FillChar(self,SizeOf(TVkLayerProperties),#0);
 ArrayItemCount:=length(pLayerName);
 if ArrayItemCount>length(layerName) then begin
  ArrayItemCount:=length(layerName);
 end;
 if ArrayItemCount>0 then begin
  Move(pLayerName[1],layerName[0],ArrayItemCount*SizeOf(TVkChar));
 end;
 specVersion:=pSpecVersion;
 implementationVersion:=pImplementationVersion;
 ArrayItemCount:=length(pDescription);
 if ArrayItemCount>length(description) then begin
  ArrayItemCount:=length(description);
 end;
 if ArrayItemCount>0 then begin
  Move(pDescription[1],description[0],ArrayItemCount*SizeOf(TVkChar));
 end;
end;

constructor TVkApplicationInfo.Create(const pPApplicationName:PVkChar;
                                      const pApplicationVersion:TVkUInt32;
                                      const pPEngineName:PVkChar;
                                      const pEngineVersion:TVkUInt32;
                                      const pApiVersion:TVkUInt32);
begin
 sType:=VK_STRUCTURE_TYPE_APPLICATION_INFO;
 pNext:=nil;
 pApplicationName:=pPApplicationName;
 applicationVersion:=pApplicationVersion;
 pEngineName:=pPEngineName;
 engineVersion:=pEngineVersion;
 apiVersion:=pApiVersion;
end;

constructor TVkAllocationCallbacks.Create(const pPUserData:PVkVoid;
                                          const pPfnAllocation:TPFN_vkAllocationFunction;
                                          const pPfnReallocation:TPFN_vkReallocationFunction;
                                          const pPfnFree:TPFN_vkFreeFunction;
                                          const pPfnInternalAllocation:TPFN_vkInternalAllocationNotification;
                                          const pPfnInternalFree:TPFN_vkInternalFreeNotification);
begin
 pUserData:=pPUserData;
 pfnAllocation:=pPfnAllocation;
 pfnReallocation:=pPfnReallocation;
 pfnFree:=pPfnFree;
 pfnInternalAllocation:=pPfnInternalAllocation;
 pfnInternalFree:=pPfnInternalFree;
end;

constructor TVkDeviceQueueCreateInfo.Create(const pFlags:TVkDeviceQueueCreateFlags;
                                            const pQueueFamilyIndex:TVkUInt32;
                                            const pQueueCount:TVkUInt32;
                                            const pPQueuePriorities:PVkFloat);
begin
 sType:=VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 queueFamilyIndex:=pQueueFamilyIndex;
 queueCount:=pQueueCount;
 pQueuePriorities:=pPQueuePriorities;
end;

constructor TVkPhysicalDeviceFeatures.Create(const pRobustBufferAccess:TVkBool32;
                                             const pFullDrawIndexUint32:TVkBool32;
                                             const pImageCubeArray:TVkBool32;
                                             const pIndependentBlend:TVkBool32;
                                             const pGeometryShader:TVkBool32;
                                             const pTessellationShader:TVkBool32;
                                             const pSampleRateShading:TVkBool32;
                                             const pDualSrcBlend:TVkBool32;
                                             const pLogicOp:TVkBool32;
                                             const pMultiDrawIndirect:TVkBool32;
                                             const pDrawIndirectFirstInstance:TVkBool32;
                                             const pDepthClamp:TVkBool32;
                                             const pDepthBiasClamp:TVkBool32;
                                             const pFillModeNonSolid:TVkBool32;
                                             const pDepthBounds:TVkBool32;
                                             const pWideLines:TVkBool32;
                                             const pLargePoints:TVkBool32;
                                             const pAlphaToOne:TVkBool32;
                                             const pMultiViewport:TVkBool32;
                                             const pSamplerAnisotropy:TVkBool32;
                                             const pTextureCompressionETC2:TVkBool32;
                                             const pTextureCompressionASTC_LDR:TVkBool32;
                                             const pTextureCompressionBC:TVkBool32;
                                             const pOcclusionQueryPrecise:TVkBool32;
                                             const pPipelineStatisticsQuery:TVkBool32;
                                             const pVertexPipelineStoresAndAtomics:TVkBool32;
                                             const pFragmentStoresAndAtomics:TVkBool32;
                                             const pShaderTessellationAndGeometryPointSize:TVkBool32;
                                             const pShaderImageGatherExtended:TVkBool32;
                                             const pShaderStorageImageExtendedFormats:TVkBool32;
                                             const pShaderStorageImageMultisample:TVkBool32;
                                             const pShaderStorageImageReadWithoutFormat:TVkBool32;
                                             const pShaderStorageImageWriteWithoutFormat:TVkBool32;
                                             const pShaderUniformBufferArrayDynamicIndexing:TVkBool32;
                                             const pShaderSampledImageArrayDynamicIndexing:TVkBool32;
                                             const pShaderStorageBufferArrayDynamicIndexing:TVkBool32;
                                             const pShaderStorageImageArrayDynamicIndexing:TVkBool32;
                                             const pShaderClipDistance:TVkBool32;
                                             const pShaderCullDistance:TVkBool32;
                                             const pShaderFloat64:TVkBool32;
                                             const pShaderInt64:TVkBool32;
                                             const pShaderInt16:TVkBool32;
                                             const pShaderResourceResidency:TVkBool32;
                                             const pShaderResourceMinLod:TVkBool32;
                                             const pSparseBinding:TVkBool32;
                                             const pSparseResidencyBuffer:TVkBool32;
                                             const pSparseResidencyImage2D:TVkBool32;
                                             const pSparseResidencyImage3D:TVkBool32;
                                             const pSparseResidency2Samples:TVkBool32;
                                             const pSparseResidency4Samples:TVkBool32;
                                             const pSparseResidency8Samples:TVkBool32;
                                             const pSparseResidency16Samples:TVkBool32;
                                             const pSparseResidencyAliased:TVkBool32;
                                             const pVariableMultisampleRate:TVkBool32;
                                             const pInheritedQueries:TVkBool32);
begin
 robustBufferAccess:=pRobustBufferAccess;
 fullDrawIndexUint32:=pFullDrawIndexUint32;
 imageCubeArray:=pImageCubeArray;
 independentBlend:=pIndependentBlend;
 geometryShader:=pGeometryShader;
 tessellationShader:=pTessellationShader;
 sampleRateShading:=pSampleRateShading;
 dualSrcBlend:=pDualSrcBlend;
 logicOp:=pLogicOp;
 multiDrawIndirect:=pMultiDrawIndirect;
 drawIndirectFirstInstance:=pDrawIndirectFirstInstance;
 depthClamp:=pDepthClamp;
 depthBiasClamp:=pDepthBiasClamp;
 fillModeNonSolid:=pFillModeNonSolid;
 depthBounds:=pDepthBounds;
 wideLines:=pWideLines;
 largePoints:=pLargePoints;
 alphaToOne:=pAlphaToOne;
 multiViewport:=pMultiViewport;
 samplerAnisotropy:=pSamplerAnisotropy;
 textureCompressionETC2:=pTextureCompressionETC2;
 textureCompressionASTC_LDR:=pTextureCompressionASTC_LDR;
 textureCompressionBC:=pTextureCompressionBC;
 occlusionQueryPrecise:=pOcclusionQueryPrecise;
 pipelineStatisticsQuery:=pPipelineStatisticsQuery;
 vertexPipelineStoresAndAtomics:=pVertexPipelineStoresAndAtomics;
 fragmentStoresAndAtomics:=pFragmentStoresAndAtomics;
 shaderTessellationAndGeometryPointSize:=pShaderTessellationAndGeometryPointSize;
 shaderImageGatherExtended:=pShaderImageGatherExtended;
 shaderStorageImageExtendedFormats:=pShaderStorageImageExtendedFormats;
 shaderStorageImageMultisample:=pShaderStorageImageMultisample;
 shaderStorageImageReadWithoutFormat:=pShaderStorageImageReadWithoutFormat;
 shaderStorageImageWriteWithoutFormat:=pShaderStorageImageWriteWithoutFormat;
 shaderUniformBufferArrayDynamicIndexing:=pShaderUniformBufferArrayDynamicIndexing;
 shaderSampledImageArrayDynamicIndexing:=pShaderSampledImageArrayDynamicIndexing;
 shaderStorageBufferArrayDynamicIndexing:=pShaderStorageBufferArrayDynamicIndexing;
 shaderStorageImageArrayDynamicIndexing:=pShaderStorageImageArrayDynamicIndexing;
 shaderClipDistance:=pShaderClipDistance;
 shaderCullDistance:=pShaderCullDistance;
 shaderFloat64:=pShaderFloat64;
 shaderInt64:=pShaderInt64;
 shaderInt16:=pShaderInt16;
 shaderResourceResidency:=pShaderResourceResidency;
 shaderResourceMinLod:=pShaderResourceMinLod;
 sparseBinding:=pSparseBinding;
 sparseResidencyBuffer:=pSparseResidencyBuffer;
 sparseResidencyImage2D:=pSparseResidencyImage2D;
 sparseResidencyImage3D:=pSparseResidencyImage3D;
 sparseResidency2Samples:=pSparseResidency2Samples;
 sparseResidency4Samples:=pSparseResidency4Samples;
 sparseResidency8Samples:=pSparseResidency8Samples;
 sparseResidency16Samples:=pSparseResidency16Samples;
 sparseResidencyAliased:=pSparseResidencyAliased;
 variableMultisampleRate:=pVariableMultisampleRate;
 inheritedQueries:=pInheritedQueries;
end;

constructor TVkInstanceCreateInfo.Create(const pFlags:TVkInstanceCreateFlags;
                                         const pPApplicationInfo:PVkApplicationInfo;
                                         const pEnabledLayerCount:TVkUInt32;
                                         const pPpEnabledLayerNames:PPVkChar;
                                         const pEnabledExtensionCount:TVkUInt32;
                                         const pPpEnabledExtensionNames:PPVkChar);
begin
 sType:=VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 pApplicationInfo:=pPApplicationInfo;
 enabledLayerCount:=pEnabledLayerCount;
 ppEnabledLayerNames:=pPpEnabledLayerNames;
 enabledExtensionCount:=pEnabledExtensionCount;
 ppEnabledExtensionNames:=pPpEnabledExtensionNames;
end;

constructor TVkQueueFamilyProperties.Create(const pQueueFlags:TVkQueueFlags;
                                            const pQueueCount:TVkUInt32;
                                            const pTimestampValidBits:TVkUInt32;
                                            const pMinImageTransferGranularity:TVkExtent3D);
begin
 queueFlags:=pQueueFlags;
 queueCount:=pQueueCount;
 timestampValidBits:=pTimestampValidBits;
 minImageTransferGranularity:=pMinImageTransferGranularity;
end;

constructor TVkMemoryType.Create(const pPropertyFlags:TVkMemoryPropertyFlags;
                                 const pHeapIndex:TVkUInt32);
begin
 propertyFlags:=pPropertyFlags;
 heapIndex:=pHeapIndex;
end;

constructor TVkMemoryAllocateInfo.Create(const pAllocationSize:TVkDeviceSize;
                                         const pMemoryTypeIndex:TVkUInt32);
begin
 sType:=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
 pNext:=nil;
 allocationSize:=pAllocationSize;
 memoryTypeIndex:=pMemoryTypeIndex;
end;

constructor TVkMemoryRequirements.Create(const pSize:TVkDeviceSize;
                                         const pAlignment:TVkDeviceSize;
                                         const pMemoryTypeBits:TVkUInt32);
begin
 size:=pSize;
 alignment:=pAlignment;
 memoryTypeBits:=pMemoryTypeBits;
end;

constructor TVkSparseImageFormatProperties.Create(const pAspectMask:TVkImageAspectFlags;
                                                  const pImageGranularity:TVkExtent3D;
                                                  const pFlags:TVkSparseImageFormatFlags);
begin
 aspectMask:=pAspectMask;
 imageGranularity:=pImageGranularity;
 flags:=pFlags;
end;

constructor TVkSparseImageMemoryRequirements.Create(const pFormatProperties:TVkSparseImageFormatProperties;
                                                    const pImageMipTailFirstLod:TVkUInt32;
                                                    const pImageMipTailSize:TVkDeviceSize;
                                                    const pImageMipTailOffset:TVkDeviceSize;
                                                    const pImageMipTailStride:TVkDeviceSize);
begin
 formatProperties:=pFormatProperties;
 imageMipTailFirstLod:=pImageMipTailFirstLod;
 imageMipTailSize:=pImageMipTailSize;
 imageMipTailOffset:=pImageMipTailOffset;
 imageMipTailStride:=pImageMipTailStride;
end;

constructor TVkMemoryHeap.Create(const pSize:TVkDeviceSize;
                                 const pFlags:TVkMemoryHeapFlags);
begin
 size:=pSize;
 flags:=pFlags;
end;

constructor TVkPhysicalDeviceMemoryProperties.Create(const pMemoryTypeCount:TVkUInt32;
                                                     const pMemoryTypes:array of TVkMemoryType;
                                                     const pMemoryHeapCount:TVkUInt32;
                                                     const pMemoryHeaps:array of TVkMemoryHeap);
var ArrayItemCount:TVkInt32;
begin
 FillChar(self,SizeOf(TVkPhysicalDeviceMemoryProperties),#0);
 memoryTypeCount:=pMemoryTypeCount;
 ArrayItemCount:=length(pMemoryTypes);
 if ArrayItemCount>length(memoryTypes) then begin
  ArrayItemCount:=length(memoryTypes);
 end;
 if ArrayItemCount>0 then begin
  Move(pMemoryTypes[0],memoryTypes[0],ArrayItemCount*SizeOf(TVkMemoryType));
 end;
 memoryHeapCount:=pMemoryHeapCount;
 ArrayItemCount:=length(pMemoryHeaps);
 if ArrayItemCount>length(memoryHeaps) then begin
  ArrayItemCount:=length(memoryHeaps);
 end;
 if ArrayItemCount>0 then begin
  Move(pMemoryHeaps[0],memoryHeaps[0],ArrayItemCount*SizeOf(TVkMemoryHeap));
 end;
end;

constructor TVkMappedMemoryRange.Create(const pMemory:TVkDeviceMemory;
                                        const pOffset:TVkDeviceSize;
                                        const pSize:TVkDeviceSize);
begin
 sType:=VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE;
 pNext:=nil;
 memory:=pMemory;
 offset:=pOffset;
 size:=pSize;
end;

constructor TVkFormatProperties.Create(const pLinearTilingFeatures:TVkFormatFeatureFlags;
                                       const pOptimalTilingFeatures:TVkFormatFeatureFlags;
                                       const pBufferFeatures:TVkFormatFeatureFlags);
begin
 linearTilingFeatures:=pLinearTilingFeatures;
 optimalTilingFeatures:=pOptimalTilingFeatures;
 bufferFeatures:=pBufferFeatures;
end;

constructor TVkImageFormatProperties.Create(const pMaxExtent:TVkExtent3D;
                                            const pMaxMipLevels:TVkUInt32;
                                            const pMaxArrayLayers:TVkUInt32;
                                            const pSampleCounts:TVkSampleCountFlags;
                                            const pMaxResourceSize:TVkDeviceSize);
begin
 maxExtent:=pMaxExtent;
 maxMipLevels:=pMaxMipLevels;
 maxArrayLayers:=pMaxArrayLayers;
 sampleCounts:=pSampleCounts;
 maxResourceSize:=pMaxResourceSize;
end;

constructor TVkDescriptorBufferInfo.Create(const pBuffer:TVkBuffer;
                                           const pOffset:TVkDeviceSize;
                                           const pRange:TVkDeviceSize);
begin
 buffer:=pBuffer;
 offset:=pOffset;
 range:=pRange;
end;

constructor TVkDescriptorImageInfo.Create(const pSampler:TVkSampler;
                                          const pImageView:TVkImageView;
                                          const pImageLayout:TVkImageLayout);
begin
 sampler:=pSampler;
 imageView:=pImageView;
 imageLayout:=pImageLayout;
end;

constructor TVkWriteDescriptorSet.Create(const pDstSet:TVkDescriptorSet;
                                         const pDstBinding:TVkUInt32;
                                         const pDstArrayElement:TVkUInt32;
                                         const pDescriptorCount:TVkUInt32;
                                         const pDescriptorType:TVkDescriptorType;
                                         const pPImageInfo:PVkDescriptorImageInfo;
                                         const pPBufferInfo:PVkDescriptorBufferInfo;
                                         const pPTexelBufferView:PVkBufferView);
begin
 sType:=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
 pNext:=nil;
 dstSet:=pDstSet;
 dstBinding:=pDstBinding;
 dstArrayElement:=pDstArrayElement;
 descriptorCount:=pDescriptorCount;
 descriptorType:=pDescriptorType;
 pImageInfo:=pPImageInfo;
 pBufferInfo:=pPBufferInfo;
 pTexelBufferView:=pPTexelBufferView;
end;

constructor TVkCopyDescriptorSet.Create(const pSrcSet:TVkDescriptorSet;
                                        const pSrcBinding:TVkUInt32;
                                        const pSrcArrayElement:TVkUInt32;
                                        const pDstSet:TVkDescriptorSet;
                                        const pDstBinding:TVkUInt32;
                                        const pDstArrayElement:TVkUInt32;
                                        const pDescriptorCount:TVkUInt32);
begin
 sType:=VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET;
 pNext:=nil;
 srcSet:=pSrcSet;
 srcBinding:=pSrcBinding;
 srcArrayElement:=pSrcArrayElement;
 dstSet:=pDstSet;
 dstBinding:=pDstBinding;
 dstArrayElement:=pDstArrayElement;
 descriptorCount:=pDescriptorCount;
end;

constructor TVkBufferCreateInfo.Create(const pFlags:TVkBufferCreateFlags;
                                       const pSize:TVkDeviceSize;
                                       const pUsage:TVkBufferUsageFlags;
                                       const pSharingMode:TVkSharingMode;
                                       const pQueueFamilyIndexCount:TVkUInt32;
                                       const pPQueueFamilyIndices:PVkUInt32);
begin
 sType:=VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 size:=pSize;
 usage:=pUsage;
 sharingMode:=pSharingMode;
 queueFamilyIndexCount:=pQueueFamilyIndexCount;
 pQueueFamilyIndices:=pPQueueFamilyIndices;
end;

constructor TVkBufferViewCreateInfo.Create(const pFlags:TVkBufferViewCreateFlags;
                                           const pBuffer:TVkBuffer;
                                           const pFormat:TVkFormat;
                                           const pOffset:TVkDeviceSize;
                                           const pRange:TVkDeviceSize);
begin
 sType:=VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 buffer:=pBuffer;
 format:=pFormat;
 offset:=pOffset;
 range:=pRange;
end;

constructor TVkImageSubresource.Create(const pAspectMask:TVkImageAspectFlags;
                                       const pMipLevel:TVkUInt32;
                                       const pArrayLayer:TVkUInt32);
begin
 aspectMask:=pAspectMask;
 mipLevel:=pMipLevel;
 arrayLayer:=pArrayLayer;
end;

constructor TVkImageSubresourceLayers.Create(const pAspectMask:TVkImageAspectFlags;
                                             const pMipLevel:TVkUInt32;
                                             const pBaseArrayLayer:TVkUInt32;
                                             const pLayerCount:TVkUInt32);
begin
 aspectMask:=pAspectMask;
 mipLevel:=pMipLevel;
 baseArrayLayer:=pBaseArrayLayer;
 layerCount:=pLayerCount;
end;

constructor TVkImageSubresourceRange.Create(const pAspectMask:TVkImageAspectFlags;
                                            const pBaseMipLevel:TVkUInt32;
                                            const pLevelCount:TVkUInt32;
                                            const pBaseArrayLayer:TVkUInt32;
                                            const pLayerCount:TVkUInt32);
begin
 aspectMask:=pAspectMask;
 baseMipLevel:=pBaseMipLevel;
 levelCount:=pLevelCount;
 baseArrayLayer:=pBaseArrayLayer;
 layerCount:=pLayerCount;
end;

constructor TVkMemoryBarrier.Create(const pSrcAccessMask:TVkAccessFlags;
                                    const pDstAccessMask:TVkAccessFlags);
begin
 sType:=VK_STRUCTURE_TYPE_MEMORY_BARRIER;
 pNext:=nil;
 srcAccessMask:=pSrcAccessMask;
 dstAccessMask:=pDstAccessMask;
end;

constructor TVkBufferMemoryBarrier.Create(const pSrcAccessMask:TVkAccessFlags;
                                          const pDstAccessMask:TVkAccessFlags;
                                          const pSrcQueueFamilyIndex:TVkUInt32;
                                          const pDstQueueFamilyIndex:TVkUInt32;
                                          const pBuffer:TVkBuffer;
                                          const pOffset:TVkDeviceSize;
                                          const pSize:TVkDeviceSize);
begin
 sType:=VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER;
 pNext:=nil;
 srcAccessMask:=pSrcAccessMask;
 dstAccessMask:=pDstAccessMask;
 srcQueueFamilyIndex:=pSrcQueueFamilyIndex;
 dstQueueFamilyIndex:=pDstQueueFamilyIndex;
 buffer:=pBuffer;
 offset:=pOffset;
 size:=pSize;
end;

constructor TVkImageMemoryBarrier.Create(const pSrcAccessMask:TVkAccessFlags;
                                         const pDstAccessMask:TVkAccessFlags;
                                         const pOldLayout:TVkImageLayout;
                                         const pNewLayout:TVkImageLayout;
                                         const pSrcQueueFamilyIndex:TVkUInt32;
                                         const pDstQueueFamilyIndex:TVkUInt32;
                                         const pImage:TVkImage;
                                         const pSubresourceRange:TVkImageSubresourceRange);
begin
 sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
 pNext:=nil;
 srcAccessMask:=pSrcAccessMask;
 dstAccessMask:=pDstAccessMask;
 oldLayout:=pOldLayout;
 newLayout:=pNewLayout;
 srcQueueFamilyIndex:=pSrcQueueFamilyIndex;
 dstQueueFamilyIndex:=pDstQueueFamilyIndex;
 image:=pImage;
 subresourceRange:=pSubresourceRange;
end;

constructor TVkImageCreateInfo.Create(const pFlags:TVkImageCreateFlags;
                                      const pImageType:TVkImageType;
                                      const pFormat:TVkFormat;
                                      const pExtent:TVkExtent3D;
                                      const pMipLevels:TVkUInt32;
                                      const pArrayLayers:TVkUInt32;
                                      const pSamples:TVkSampleCountFlagBits;
                                      const pTiling:TVkImageTiling;
                                      const pUsage:TVkImageUsageFlags;
                                      const pSharingMode:TVkSharingMode;
                                      const pQueueFamilyIndexCount:TVkUInt32;
                                      const pPQueueFamilyIndices:PVkUInt32;
                                      const pInitialLayout:TVkImageLayout);
begin
 sType:=VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 imageType:=pImageType;
 format:=pFormat;
 extent:=pExtent;
 mipLevels:=pMipLevels;
 arrayLayers:=pArrayLayers;
 samples:=pSamples;
 tiling:=pTiling;
 usage:=pUsage;
 sharingMode:=pSharingMode;
 queueFamilyIndexCount:=pQueueFamilyIndexCount;
 pQueueFamilyIndices:=pPQueueFamilyIndices;
 initialLayout:=pInitialLayout;
end;

constructor TVkSubresourceLayout.Create(const pOffset:TVkDeviceSize;
                                        const pSize:TVkDeviceSize;
                                        const pRowPitch:TVkDeviceSize;
                                        const pArrayPitch:TVkDeviceSize;
                                        const pDepthPitch:TVkDeviceSize);
begin
 offset:=pOffset;
 size:=pSize;
 rowPitch:=pRowPitch;
 arrayPitch:=pArrayPitch;
 depthPitch:=pDepthPitch;
end;

constructor TVkImageViewCreateInfo.Create(const pFlags:TVkImageViewCreateFlags;
                                          const pImage:TVkImage;
                                          const pViewType:TVkImageViewType;
                                          const pFormat:TVkFormat;
                                          const pComponents:TVkComponentMapping;
                                          const pSubresourceRange:TVkImageSubresourceRange);
begin
 sType:=VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 image:=pImage;
 viewType:=pViewType;
 format:=pFormat;
 components:=pComponents;
 subresourceRange:=pSubresourceRange;
end;

constructor TVkBufferCopy.Create(const pSrcOffset:TVkDeviceSize;
                                 const pDstOffset:TVkDeviceSize;
                                 const pSize:TVkDeviceSize);
begin
 srcOffset:=pSrcOffset;
 dstOffset:=pDstOffset;
 size:=pSize;
end;

constructor TVkSparseMemoryBind.Create(const pResourceOffset:TVkDeviceSize;
                                       const pSize:TVkDeviceSize;
                                       const pMemory:TVkDeviceMemory;
                                       const pMemoryOffset:TVkDeviceSize;
                                       const pFlags:TVkSparseMemoryBindFlags);
begin
 resourceOffset:=pResourceOffset;
 size:=pSize;
 memory:=pMemory;
 memoryOffset:=pMemoryOffset;
 flags:=pFlags;
end;

constructor TVkSparseImageMemoryBind.Create(const pSubresource:TVkImageSubresource;
                                            const pOffset:TVkOffset3D;
                                            const pExtent:TVkExtent3D;
                                            const pMemory:TVkDeviceMemory;
                                            const pMemoryOffset:TVkDeviceSize;
                                            const pFlags:TVkSparseMemoryBindFlags);
begin
 subresource:=pSubresource;
 offset:=pOffset;
 extent:=pExtent;
 memory:=pMemory;
 memoryOffset:=pMemoryOffset;
 flags:=pFlags;
end;

constructor TVkSparseBufferMemoryBindInfo.Create(const pBuffer:TVkBuffer;
                                                 const pBindCount:TVkUInt32;
                                                 const pPBinds:PVkSparseMemoryBind);
begin
 buffer:=pBuffer;
 bindCount:=pBindCount;
 pBinds:=pPBinds;
end;

constructor TVkSparseImageOpaqueMemoryBindInfo.Create(const pImage:TVkImage;
                                                      const pBindCount:TVkUInt32;
                                                      const pPBinds:PVkSparseMemoryBind);
begin
 image:=pImage;
 bindCount:=pBindCount;
 pBinds:=pPBinds;
end;

constructor TVkSparseImageMemoryBindInfo.Create(const pImage:TVkImage;
                                                const pBindCount:TVkUInt32;
                                                const pPBinds:PVkSparseImageMemoryBind);
begin
 image:=pImage;
 bindCount:=pBindCount;
 pBinds:=pPBinds;
end;

constructor TVkBindSparseInfo.Create(const pWaitSemaphoreCount:TVkUInt32;
                                     const pPWaitSemaphores:PVkSemaphore;
                                     const pBufferBindCount:TVkUInt32;
                                     const pPBufferBinds:PVkSparseBufferMemoryBindInfo;
                                     const pImageOpaqueBindCount:TVkUInt32;
                                     const pPImageOpaqueBinds:PVkSparseImageOpaqueMemoryBindInfo;
                                     const pImageBindCount:TVkUInt32;
                                     const pPImageBinds:PVkSparseImageMemoryBindInfo;
                                     const pSignalSemaphoreCount:TVkUInt32;
                                     const pPSignalSemaphores:PVkSemaphore);
begin
 sType:=VK_STRUCTURE_TYPE_BIND_SPARSE_INFO;
 pNext:=nil;
 waitSemaphoreCount:=pWaitSemaphoreCount;
 pWaitSemaphores:=pPWaitSemaphores;
 bufferBindCount:=pBufferBindCount;
 pBufferBinds:=pPBufferBinds;
 imageOpaqueBindCount:=pImageOpaqueBindCount;
 pImageOpaqueBinds:=pPImageOpaqueBinds;
 imageBindCount:=pImageBindCount;
 pImageBinds:=pPImageBinds;
 signalSemaphoreCount:=pSignalSemaphoreCount;
 pSignalSemaphores:=pPSignalSemaphores;
end;

constructor TVkImageCopy.Create(const pSrcSubresource:TVkImageSubresourceLayers;
                                const pSrcOffset:TVkOffset3D;
                                const pDstSubresource:TVkImageSubresourceLayers;
                                const pDstOffset:TVkOffset3D;
                                const pExtent:TVkExtent3D);
begin
 srcSubresource:=pSrcSubresource;
 srcOffset:=pSrcOffset;
 dstSubresource:=pDstSubresource;
 dstOffset:=pDstOffset;
 extent:=pExtent;
end;

constructor TVkImageBlit.Create(const pSrcSubresource:TVkImageSubresourceLayers;
                                const pSrcOffsets:array of TVkOffset3D;
                                const pDstSubresource:TVkImageSubresourceLayers;
                                const pDstOffsets:array of TVkOffset3D);
var ArrayItemCount:TVkInt32;
begin
 FillChar(self,SizeOf(TVkImageBlit),#0);
 srcSubresource:=pSrcSubresource;
 ArrayItemCount:=length(pSrcOffsets);
 if ArrayItemCount>length(srcOffsets) then begin
  ArrayItemCount:=length(srcOffsets);
 end;
 if ArrayItemCount>0 then begin
  Move(pSrcOffsets[0],srcOffsets[0],ArrayItemCount*SizeOf(TVkOffset3D));
 end;
 dstSubresource:=pDstSubresource;
 ArrayItemCount:=length(pDstOffsets);
 if ArrayItemCount>length(dstOffsets) then begin
  ArrayItemCount:=length(dstOffsets);
 end;
 if ArrayItemCount>0 then begin
  Move(pDstOffsets[0],dstOffsets[0],ArrayItemCount*SizeOf(TVkOffset3D));
 end;
end;

constructor TVkBufferImageCopy.Create(const pBufferOffset:TVkDeviceSize;
                                      const pBufferRowLength:TVkUInt32;
                                      const pBufferImageHeight:TVkUInt32;
                                      const pImageSubresource:TVkImageSubresourceLayers;
                                      const pImageOffset:TVkOffset3D;
                                      const pImageExtent:TVkExtent3D);
begin
 bufferOffset:=pBufferOffset;
 bufferRowLength:=pBufferRowLength;
 bufferImageHeight:=pBufferImageHeight;
 imageSubresource:=pImageSubresource;
 imageOffset:=pImageOffset;
 imageExtent:=pImageExtent;
end;

constructor TVkImageResolve.Create(const pSrcSubresource:TVkImageSubresourceLayers;
                                   const pSrcOffset:TVkOffset3D;
                                   const pDstSubresource:TVkImageSubresourceLayers;
                                   const pDstOffset:TVkOffset3D;
                                   const pExtent:TVkExtent3D);
begin
 srcSubresource:=pSrcSubresource;
 srcOffset:=pSrcOffset;
 dstSubresource:=pDstSubresource;
 dstOffset:=pDstOffset;
 extent:=pExtent;
end;

constructor TVkShaderModuleCreateInfo.Create(const pFlags:TVkShaderModuleCreateFlags;
                                             const pCodeSize:TVkSize;
                                             const pPCode:PVkUInt32);
begin
 sType:=VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 codeSize:=pCodeSize;
 pCode:=pPCode;
end;

constructor TVkDescriptorSetLayoutBinding.Create(const pBinding:TVkUInt32;
                                                 const pDescriptorType:TVkDescriptorType;
                                                 const pDescriptorCount:TVkUInt32;
                                                 const pStageFlags:TVkShaderStageFlags;
                                                 const pPImmutableSamplers:PVkSampler);
begin
 binding:=pBinding;
 descriptorType:=pDescriptorType;
 descriptorCount:=pDescriptorCount;
 stageFlags:=pStageFlags;
 pImmutableSamplers:=pPImmutableSamplers;
end;

constructor TVkDescriptorSetLayoutCreateInfo.Create(const pFlags:TVkDescriptorSetLayoutCreateFlags;
                                                    const pBindingCount:TVkUInt32;
                                                    const pPBindings:PVkDescriptorSetLayoutBinding);
begin
 sType:=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 bindingCount:=pBindingCount;
 pBindings:=pPBindings;
end;

constructor TVkDescriptorPoolSize.Create(const pType_:TVkDescriptorType;
                                         const pDescriptorCount:TVkUInt32);
begin
 type_:=pType_;
 descriptorCount:=pDescriptorCount;
end;

constructor TVkDescriptorPoolCreateInfo.Create(const pFlags:TVkDescriptorPoolCreateFlags;
                                               const pMaxSets:TVkUInt32;
                                               const pPoolSizeCount:TVkUInt32;
                                               const pPPoolSizes:PVkDescriptorPoolSize);
begin
 sType:=VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 maxSets:=pMaxSets;
 poolSizeCount:=pPoolSizeCount;
 pPoolSizes:=pPPoolSizes;
end;

constructor TVkDescriptorSetAllocateInfo.Create(const pDescriptorPool:TVkDescriptorPool;
                                                const pDescriptorSetCount:TVkUInt32;
                                                const pPSetLayouts:PVkDescriptorSetLayout);
begin
 sType:=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
 pNext:=nil;
 descriptorPool:=pDescriptorPool;
 descriptorSetCount:=pDescriptorSetCount;
 pSetLayouts:=pPSetLayouts;
end;

constructor TVkSpecializationMapEntry.Create(const pConstantID:TVkUInt32;
                                             const pOffset:TVkUInt32;
                                             const pSize:TVkSize);
begin
 constantID:=pConstantID;
 offset:=pOffset;
 size:=pSize;
end;

constructor TVkSpecializationInfo.Create(const pMapEntryCount:TVkUInt32;
                                         const pPMapEntries:PVkSpecializationMapEntry;
                                         const pDataSize:TVkSize;
                                         const pPData:PVkVoid);
begin
 mapEntryCount:=pMapEntryCount;
 pMapEntries:=pPMapEntries;
 dataSize:=pDataSize;
 pData:=pPData;
end;

constructor TVkPipelineShaderStageCreateInfo.Create(const pFlags:TVkPipelineShaderStageCreateFlags;
                                                    const pStage:TVkShaderStageFlagBits;
                                                    const pModule:TVkShaderModule;
                                                    const pPName:PVkChar;
                                                    const pPSpecializationInfo:PVkSpecializationInfo);
begin
 sType:=VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 stage:=pStage;
 module:=pModule;
 pName:=pPName;
 pSpecializationInfo:=pPSpecializationInfo;
end;

constructor TVkComputePipelineCreateInfo.Create(const pFlags:TVkPipelineCreateFlags;
                                                const pStage:TVkPipelineShaderStageCreateInfo;
                                                const pLayout:TVkPipelineLayout;
                                                const pBasePipelineHandle:TVkPipeline;
                                                const pBasePipelineIndex:TVkInt32);
begin
 sType:=VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 stage:=pStage;
 layout:=pLayout;
 basePipelineHandle:=pBasePipelineHandle;
 basePipelineIndex:=pBasePipelineIndex;
end;

constructor TVkVertexInputBindingDescription.Create(const pBinding:TVkUInt32;
                                                    const pStride:TVkUInt32;
                                                    const pInputRate:TVkVertexInputRate);
begin
 binding:=pBinding;
 stride:=pStride;
 inputRate:=pInputRate;
end;

constructor TVkVertexInputAttributeDescription.Create(const pLocation:TVkUInt32;
                                                      const pBinding:TVkUInt32;
                                                      const pFormat:TVkFormat;
                                                      const pOffset:TVkUInt32);
begin
 location:=pLocation;
 binding:=pBinding;
 format:=pFormat;
 offset:=pOffset;
end;

constructor TVkPipelineVertexInputStateCreateInfo.Create(const pFlags:TVkPipelineVertexInputStateCreateFlags;
                                                         const pVertexBindingDescriptionCount:TVkUInt32;
                                                         const pPVertexBindingDescriptions:PVkVertexInputBindingDescription;
                                                         const pVertexAttributeDescriptionCount:TVkUInt32;
                                                         const pPVertexAttributeDescriptions:PVkVertexInputAttributeDescription);
begin
 sType:=VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 vertexBindingDescriptionCount:=pVertexBindingDescriptionCount;
 pVertexBindingDescriptions:=pPVertexBindingDescriptions;
 vertexAttributeDescriptionCount:=pVertexAttributeDescriptionCount;
 pVertexAttributeDescriptions:=pPVertexAttributeDescriptions;
end;

constructor TVkPipelineInputAssemblyStateCreateInfo.Create(const pFlags:TVkPipelineInputAssemblyStateCreateFlags;
                                                           const pTopology:TVkPrimitiveTopology;
                                                           const pPrimitiveRestartEnable:TVkBool32);
begin
 sType:=VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 topology:=pTopology;
 primitiveRestartEnable:=pPrimitiveRestartEnable;
end;

constructor TVkPipelineTessellationStateCreateInfo.Create(const pFlags:TVkPipelineTessellationStateCreateFlags;
                                                          const pPatchControlPoints:TVkUInt32);
begin
 sType:=VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 patchControlPoints:=pPatchControlPoints;
end;

constructor TVkPipelineViewportStateCreateInfo.Create(const pFlags:TVkPipelineViewportStateCreateFlags;
                                                      const pViewportCount:TVkUInt32;
                                                      const pPViewports:PVkViewport;
                                                      const pScissorCount:TVkUInt32;
                                                      const pPScissors:PVkRect2D);
begin
 sType:=VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 viewportCount:=pViewportCount;
 pViewports:=pPViewports;
 scissorCount:=pScissorCount;
 pScissors:=pPScissors;
end;

constructor TVkPipelineRasterizationStateCreateInfo.Create(const pFlags:TVkPipelineRasterizationStateCreateFlags;
                                                           const pDepthClampEnable:TVkBool32;
                                                           const pRasterizerDiscardEnable:TVkBool32;
                                                           const pPolygonMode:TVkPolygonMode;
                                                           const pCullMode:TVkCullModeFlags;
                                                           const pFrontFace:TVkFrontFace;
                                                           const pDepthBiasEnable:TVkBool32;
                                                           const pDepthBiasConstantFactor:TVkFloat;
                                                           const pDepthBiasClamp:TVkFloat;
                                                           const pDepthBiasSlopeFactor:TVkFloat;
                                                           const pLineWidth:TVkFloat);
begin
 sType:=VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 depthClampEnable:=pDepthClampEnable;
 rasterizerDiscardEnable:=pRasterizerDiscardEnable;
 polygonMode:=pPolygonMode;
 cullMode:=pCullMode;
 frontFace:=pFrontFace;
 depthBiasEnable:=pDepthBiasEnable;
 depthBiasConstantFactor:=pDepthBiasConstantFactor;
 depthBiasClamp:=pDepthBiasClamp;
 depthBiasSlopeFactor:=pDepthBiasSlopeFactor;
 lineWidth:=pLineWidth;
end;

constructor TVkPipelineMultisampleStateCreateInfo.Create(const pFlags:TVkPipelineMultisampleStateCreateFlags;
                                                         const pRasterizationSamples:TVkSampleCountFlagBits;
                                                         const pSampleShadingEnable:TVkBool32;
                                                         const pMinSampleShading:TVkFloat;
                                                         const pPSampleMask:PVkSampleMask;
                                                         const pAlphaToCoverageEnable:TVkBool32;
                                                         const pAlphaToOneEnable:TVkBool32);
begin
 sType:=VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 rasterizationSamples:=pRasterizationSamples;
 sampleShadingEnable:=pSampleShadingEnable;
 minSampleShading:=pMinSampleShading;
 pSampleMask:=pPSampleMask;
 alphaToCoverageEnable:=pAlphaToCoverageEnable;
 alphaToOneEnable:=pAlphaToOneEnable;
end;

constructor TVkPipelineColorBlendAttachmentState.Create(const pBlendEnable:TVkBool32;
                                                        const pSrcColorBlendFactor:TVkBlendFactor;
                                                        const pDstColorBlendFactor:TVkBlendFactor;
                                                        const pColorBlendOp:TVkBlendOp;
                                                        const pSrcAlphaBlendFactor:TVkBlendFactor;
                                                        const pDstAlphaBlendFactor:TVkBlendFactor;
                                                        const pAlphaBlendOp:TVkBlendOp;
                                                        const pColorWriteMask:TVkColorComponentFlags);
begin
 blendEnable:=pBlendEnable;
 srcColorBlendFactor:=pSrcColorBlendFactor;
 dstColorBlendFactor:=pDstColorBlendFactor;
 colorBlendOp:=pColorBlendOp;
 srcAlphaBlendFactor:=pSrcAlphaBlendFactor;
 dstAlphaBlendFactor:=pDstAlphaBlendFactor;
 alphaBlendOp:=pAlphaBlendOp;
 colorWriteMask:=pColorWriteMask;
end;

constructor TVkPipelineColorBlendStateCreateInfo.Create(const pFlags:TVkPipelineColorBlendStateCreateFlags;
                                                        const pLogicOpEnable:TVkBool32;
                                                        const pLogicOp:TVkLogicOp;
                                                        const pAttachmentCount:TVkUInt32;
                                                        const pPAttachments:PVkPipelineColorBlendAttachmentState;
                                                        const pBlendConstants:array of TVkFloat);
var ArrayItemCount:TVkInt32;
begin
 FillChar(self,SizeOf(TVkPipelineColorBlendStateCreateInfo),#0);
 sType:=VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 logicOpEnable:=pLogicOpEnable;
 logicOp:=pLogicOp;
 attachmentCount:=pAttachmentCount;
 pAttachments:=pPAttachments;
 ArrayItemCount:=length(pBlendConstants);
 if ArrayItemCount>length(blendConstants) then begin
  ArrayItemCount:=length(blendConstants);
 end;
 if ArrayItemCount>0 then begin
  Move(pBlendConstants[0],blendConstants[0],ArrayItemCount*SizeOf(TVkFloat));
 end;
end;

constructor TVkPipelineDynamicStateCreateInfo.Create(const pFlags:TVkPipelineDynamicStateCreateFlags;
                                                     const pDynamicStateCount:TVkUInt32;
                                                     const pPDynamicStates:PVkDynamicState);
begin
 sType:=VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 dynamicStateCount:=pDynamicStateCount;
 pDynamicStates:=pPDynamicStates;
end;

constructor TVkStencilOpState.Create(const pFailOp:TVkStencilOp;
                                     const pPassOp:TVkStencilOp;
                                     const pDepthFailOp:TVkStencilOp;
                                     const pCompareOp:TVkCompareOp;
                                     const pCompareMask:TVkUInt32;
                                     const pWriteMask:TVkUInt32;
                                     const pReference:TVkUInt32);
begin
 failOp:=pFailOp;
 passOp:=pPassOp;
 depthFailOp:=pDepthFailOp;
 compareOp:=pCompareOp;
 compareMask:=pCompareMask;
 writeMask:=pWriteMask;
 reference:=pReference;
end;

constructor TVkPipelineDepthStencilStateCreateInfo.Create(const pFlags:TVkPipelineDepthStencilStateCreateFlags;
                                                          const pDepthTestEnable:TVkBool32;
                                                          const pDepthWriteEnable:TVkBool32;
                                                          const pDepthCompareOp:TVkCompareOp;
                                                          const pDepthBoundsTestEnable:TVkBool32;
                                                          const pStencilTestEnable:TVkBool32;
                                                          const pFront:TVkStencilOpState;
                                                          const pBack:TVkStencilOpState;
                                                          const pMinDepthBounds:TVkFloat;
                                                          const pMaxDepthBounds:TVkFloat);
begin
 sType:=VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 depthTestEnable:=pDepthTestEnable;
 depthWriteEnable:=pDepthWriteEnable;
 depthCompareOp:=pDepthCompareOp;
 depthBoundsTestEnable:=pDepthBoundsTestEnable;
 stencilTestEnable:=pStencilTestEnable;
 front:=pFront;
 back:=pBack;
 minDepthBounds:=pMinDepthBounds;
 maxDepthBounds:=pMaxDepthBounds;
end;

constructor TVkGraphicsPipelineCreateInfo.Create(const pFlags:TVkPipelineCreateFlags;
                                                 const pStageCount:TVkUInt32;
                                                 const pPStages:PVkPipelineShaderStageCreateInfo;
                                                 const pPVertexInputState:PVkPipelineVertexInputStateCreateInfo;
                                                 const pPInputAssemblyState:PVkPipelineInputAssemblyStateCreateInfo;
                                                 const pPTessellationState:PVkPipelineTessellationStateCreateInfo;
                                                 const pPViewportState:PVkPipelineViewportStateCreateInfo;
                                                 const pPRasterizationState:PVkPipelineRasterizationStateCreateInfo;
                                                 const pPMultisampleState:PVkPipelineMultisampleStateCreateInfo;
                                                 const pPDepthStencilState:PVkPipelineDepthStencilStateCreateInfo;
                                                 const pPColorBlendState:PVkPipelineColorBlendStateCreateInfo;
                                                 const pPDynamicState:PVkPipelineDynamicStateCreateInfo;
                                                 const pLayout:TVkPipelineLayout;
                                                 const pRenderPass:TVkRenderPass;
                                                 const pSubpass:TVkUInt32;
                                                 const pBasePipelineHandle:TVkPipeline;
                                                 const pBasePipelineIndex:TVkInt32);
begin
 sType:=VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 stageCount:=pStageCount;
 pStages:=pPStages;
 pVertexInputState:=pPVertexInputState;
 pInputAssemblyState:=pPInputAssemblyState;
 pTessellationState:=pPTessellationState;
 pViewportState:=pPViewportState;
 pRasterizationState:=pPRasterizationState;
 pMultisampleState:=pPMultisampleState;
 pDepthStencilState:=pPDepthStencilState;
 pColorBlendState:=pPColorBlendState;
 pDynamicState:=pPDynamicState;
 layout:=pLayout;
 renderPass:=pRenderPass;
 subpass:=pSubpass;
 basePipelineHandle:=pBasePipelineHandle;
 basePipelineIndex:=pBasePipelineIndex;
end;

constructor TVkPipelineCacheCreateInfo.Create(const pFlags:TVkPipelineCacheCreateFlags;
                                              const pInitialDataSize:TVkSize;
                                              const pPInitialData:PVkVoid);
begin
 sType:=VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 initialDataSize:=pInitialDataSize;
 pInitialData:=pPInitialData;
end;

constructor TVkPushConstantRange.Create(const pStageFlags:TVkShaderStageFlags;
                                        const pOffset:TVkUInt32;
                                        const pSize:TVkUInt32);
begin
 stageFlags:=pStageFlags;
 offset:=pOffset;
 size:=pSize;
end;

constructor TVkPipelineLayoutCreateInfo.Create(const pFlags:TVkPipelineLayoutCreateFlags;
                                               const pSetLayoutCount:TVkUInt32;
                                               const pPSetLayouts:PVkDescriptorSetLayout;
                                               const pPushConstantRangeCount:TVkUInt32;
                                               const pPPushConstantRanges:PVkPushConstantRange);
begin
 sType:=VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 setLayoutCount:=pSetLayoutCount;
 pSetLayouts:=pPSetLayouts;
 pushConstantRangeCount:=pPushConstantRangeCount;
 pPushConstantRanges:=pPPushConstantRanges;
end;

constructor TVkSamplerCreateInfo.Create(const pFlags:TVkSamplerCreateFlags;
                                        const pMagFilter:TVkFilter;
                                        const pMinFilter:TVkFilter;
                                        const pMipmapMode:TVkSamplerMipmapMode;
                                        const pAddressModeU:TVkSamplerAddressMode;
                                        const pAddressModeV:TVkSamplerAddressMode;
                                        const pAddressModeW:TVkSamplerAddressMode;
                                        const pMipLodBias:TVkFloat;
                                        const pAnisotropyEnable:TVkBool32;
                                        const pMaxAnisotropy:TVkFloat;
                                        const pCompareEnable:TVkBool32;
                                        const pCompareOp:TVkCompareOp;
                                        const pMinLod:TVkFloat;
                                        const pMaxLod:TVkFloat;
                                        const pBorderColor:TVkBorderColor;
                                        const pUnnormalizedCoordinates:TVkBool32);
begin
 sType:=VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 magFilter:=pMagFilter;
 minFilter:=pMinFilter;
 mipmapMode:=pMipmapMode;
 addressModeU:=pAddressModeU;
 addressModeV:=pAddressModeV;
 addressModeW:=pAddressModeW;
 mipLodBias:=pMipLodBias;
 anisotropyEnable:=pAnisotropyEnable;
 maxAnisotropy:=pMaxAnisotropy;
 compareEnable:=pCompareEnable;
 compareOp:=pCompareOp;
 minLod:=pMinLod;
 maxLod:=pMaxLod;
 borderColor:=pBorderColor;
 unnormalizedCoordinates:=pUnnormalizedCoordinates;
end;

constructor TVkCommandPoolCreateInfo.Create(const pFlags:TVkCommandPoolCreateFlags;
                                            const pQueueFamilyIndex:TVkUInt32);
begin
 sType:=VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 queueFamilyIndex:=pQueueFamilyIndex;
end;

constructor TVkCommandBufferAllocateInfo.Create(const pCommandPool:TVkCommandPool;
                                                const pLevel:TVkCommandBufferLevel;
                                                const pCommandBufferCount:TVkUInt32);
begin
 sType:=VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
 pNext:=nil;
 commandPool:=pCommandPool;
 level:=pLevel;
 commandBufferCount:=pCommandBufferCount;
end;

constructor TVkCommandBufferInheritanceInfo.Create(const pRenderPass:TVkRenderPass;
                                                   const pSubpass:TVkUInt32;
                                                   const pFramebuffer:TVkFramebuffer;
                                                   const pOcclusionQueryEnable:TVkBool32;
                                                   const pQueryFlags:TVkQueryControlFlags;
                                                   const pPipelineStatistics:TVkQueryPipelineStatisticFlags);
begin
 sType:=VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO;
 pNext:=nil;
 renderPass:=pRenderPass;
 subpass:=pSubpass;
 framebuffer:=pFramebuffer;
 occlusionQueryEnable:=pOcclusionQueryEnable;
 queryFlags:=pQueryFlags;
 pipelineStatistics:=pPipelineStatistics;
end;

constructor TVkCommandBufferBeginInfo.Create(const pFlags:TVkCommandBufferUsageFlags;
                                             const pPInheritanceInfo:PVkCommandBufferInheritanceInfo);
begin
 sType:=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
 pNext:=nil;
 flags:=pFlags;
 pInheritanceInfo:=pPInheritanceInfo;
end;

constructor TVkClearDepthStencilValue.Create(const pDepth:TVkFloat;
                                             const pStencil:TVkUInt32);
begin
 depth:=pDepth;
 stencil:=pStencil;
end;

constructor TVkRenderPassBeginInfo.Create(const pRenderPass:TVkRenderPass;
                                          const pFramebuffer:TVkFramebuffer;
                                          const pRenderArea:TVkRect2D;
                                          const pClearValueCount:TVkUInt32;
                                          const pPClearValues:PVkClearValue);
begin
 sType:=VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
 pNext:=nil;
 renderPass:=pRenderPass;
 framebuffer:=pFramebuffer;
 renderArea:=pRenderArea;
 clearValueCount:=pClearValueCount;
 pClearValues:=pPClearValues;
end;

constructor TVkClearAttachment.Create(const pAspectMask:TVkImageAspectFlags;
                                      const pColorAttachment:TVkUInt32;
                                      const pClearValue:TVkClearValue);
begin
 aspectMask:=pAspectMask;
 colorAttachment:=pColorAttachment;
 clearValue:=pClearValue;
end;

constructor TVkAttachmentDescription.Create(const pFlags:TVkAttachmentDescriptionFlags;
                                            const pFormat:TVkFormat;
                                            const pSamples:TVkSampleCountFlagBits;
                                            const pLoadOp:TVkAttachmentLoadOp;
                                            const pStoreOp:TVkAttachmentStoreOp;
                                            const pStencilLoadOp:TVkAttachmentLoadOp;
                                            const pStencilStoreOp:TVkAttachmentStoreOp;
                                            const pInitialLayout:TVkImageLayout;
                                            const pFinalLayout:TVkImageLayout);
begin
 flags:=pFlags;
 format:=pFormat;
 samples:=pSamples;
 loadOp:=pLoadOp;
 storeOp:=pStoreOp;
 stencilLoadOp:=pStencilLoadOp;
 stencilStoreOp:=pStencilStoreOp;
 initialLayout:=pInitialLayout;
 finalLayout:=pFinalLayout;
end;

constructor TVkAttachmentReference.Create(const pAttachment:TVkUInt32;
                                          const pLayout:TVkImageLayout);
begin
 attachment:=pAttachment;
 layout:=pLayout;
end;

constructor TVkSubpassDescription.Create(const pFlags:TVkSubpassDescriptionFlags;
                                         const pPipelineBindPoint:TVkPipelineBindPoint;
                                         const pInputAttachmentCount:TVkUInt32;
                                         const pPInputAttachments:PVkAttachmentReference;
                                         const pColorAttachmentCount:TVkUInt32;
                                         const pPColorAttachments:PVkAttachmentReference;
                                         const pPResolveAttachments:PVkAttachmentReference;
                                         const pPDepthStencilAttachment:PVkAttachmentReference;
                                         const pPreserveAttachmentCount:TVkUInt32;
                                         const pPPreserveAttachments:PVkUInt32);
begin
 flags:=pFlags;
 pipelineBindPoint:=pPipelineBindPoint;
 inputAttachmentCount:=pInputAttachmentCount;
 pInputAttachments:=pPInputAttachments;
 colorAttachmentCount:=pColorAttachmentCount;
 pColorAttachments:=pPColorAttachments;
 pResolveAttachments:=pPResolveAttachments;
 pDepthStencilAttachment:=pPDepthStencilAttachment;
 preserveAttachmentCount:=pPreserveAttachmentCount;
 pPreserveAttachments:=pPPreserveAttachments;
end;

constructor TVkSubpassDependency.Create(const pSrcSubpass:TVkUInt32;
                                        const pDstSubpass:TVkUInt32;
                                        const pSrcStageMask:TVkPipelineStageFlags;
                                        const pDstStageMask:TVkPipelineStageFlags;
                                        const pSrcAccessMask:TVkAccessFlags;
                                        const pDstAccessMask:TVkAccessFlags;
                                        const pDependencyFlags:TVkDependencyFlags);
begin
 srcSubpass:=pSrcSubpass;
 dstSubpass:=pDstSubpass;
 srcStageMask:=pSrcStageMask;
 dstStageMask:=pDstStageMask;
 srcAccessMask:=pSrcAccessMask;
 dstAccessMask:=pDstAccessMask;
 dependencyFlags:=pDependencyFlags;
end;

constructor TVkRenderPassCreateInfo.Create(const pFlags:TVkRenderPassCreateFlags;
                                           const pAttachmentCount:TVkUInt32;
                                           const pPAttachments:PVkAttachmentDescription;
                                           const pSubpassCount:TVkUInt32;
                                           const pPSubpasses:PVkSubpassDescription;
                                           const pDependencyCount:TVkUInt32;
                                           const pPDependencies:PVkSubpassDependency);
begin
 sType:=VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 attachmentCount:=pAttachmentCount;
 pAttachments:=pPAttachments;
 subpassCount:=pSubpassCount;
 pSubpasses:=pPSubpasses;
 dependencyCount:=pDependencyCount;
 pDependencies:=pPDependencies;
end;

constructor TVkEventCreateInfo.Create(const pFlags:TVkEventCreateFlags);
begin
 sType:=VK_STRUCTURE_TYPE_EVENT_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
end;

constructor TVkFenceCreateInfo.Create(const pFlags:TVkFenceCreateFlags);
begin
 sType:=VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
end;

constructor TVkDeviceCreateInfo.Create(const pFlags:TVkDeviceCreateFlags;
                                       const pQueueCreateInfoCount:TVkUInt32;
                                       const pPQueueCreateInfos:PVkDeviceQueueCreateInfo;
                                       const pEnabledLayerCount:TVkUInt32;
                                       const pPpEnabledLayerNames:PPVkChar;
                                       const pEnabledExtensionCount:TVkUInt32;
                                       const pPpEnabledExtensionNames:PPVkChar;
                                       const pPEnabledFeatures:PVkPhysicalDeviceFeatures);
begin
 sType:=VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 queueCreateInfoCount:=pQueueCreateInfoCount;
 pQueueCreateInfos:=pPQueueCreateInfos;
 enabledLayerCount:=pEnabledLayerCount;
 ppEnabledLayerNames:=pPpEnabledLayerNames;
 enabledExtensionCount:=pEnabledExtensionCount;
 ppEnabledExtensionNames:=pPpEnabledExtensionNames;
 pEnabledFeatures:=pPEnabledFeatures;
end;

constructor TVkPhysicalDeviceLimits.Create(const pMaxImageDimension1D:TVkUInt32;
                                           const pMaxImageDimension2D:TVkUInt32;
                                           const pMaxImageDimension3D:TVkUInt32;
                                           const pMaxImageDimensionCube:TVkUInt32;
                                           const pMaxImageArrayLayers:TVkUInt32;
                                           const pMaxTexelBufferElements:TVkUInt32;
                                           const pMaxUniformBufferRange:TVkUInt32;
                                           const pMaxStorageBufferRange:TVkUInt32;
                                           const pMaxPushConstantsSize:TVkUInt32;
                                           const pMaxMemoryAllocationCount:TVkUInt32;
                                           const pMaxSamplerAllocationCount:TVkUInt32;
                                           const pBufferImageGranularity:TVkDeviceSize;
                                           const pSparseAddressSpaceSize:TVkDeviceSize;
                                           const pMaxBoundDescriptorSets:TVkUInt32;
                                           const pMaxPerStageDescriptorSamplers:TVkUInt32;
                                           const pMaxPerStageDescriptorUniformBuffers:TVkUInt32;
                                           const pMaxPerStageDescriptorStorageBuffers:TVkUInt32;
                                           const pMaxPerStageDescriptorSampledImages:TVkUInt32;
                                           const pMaxPerStageDescriptorStorageImages:TVkUInt32;
                                           const pMaxPerStageDescriptorInputAttachments:TVkUInt32;
                                           const pMaxPerStageResources:TVkUInt32;
                                           const pMaxDescriptorSetSamplers:TVkUInt32;
                                           const pMaxDescriptorSetUniformBuffers:TVkUInt32;
                                           const pMaxDescriptorSetUniformBuffersDynamic:TVkUInt32;
                                           const pMaxDescriptorSetStorageBuffers:TVkUInt32;
                                           const pMaxDescriptorSetStorageBuffersDynamic:TVkUInt32;
                                           const pMaxDescriptorSetSampledImages:TVkUInt32;
                                           const pMaxDescriptorSetStorageImages:TVkUInt32;
                                           const pMaxDescriptorSetInputAttachments:TVkUInt32;
                                           const pMaxVertexInputAttributes:TVkUInt32;
                                           const pMaxVertexInputBindings:TVkUInt32;
                                           const pMaxVertexInputAttributeOffset:TVkUInt32;
                                           const pMaxVertexInputBindingStride:TVkUInt32;
                                           const pMaxVertexOutputComponents:TVkUInt32;
                                           const pMaxTessellationGenerationLevel:TVkUInt32;
                                           const pMaxTessellationPatchSize:TVkUInt32;
                                           const pMaxTessellationControlPerVertexInputComponents:TVkUInt32;
                                           const pMaxTessellationControlPerVertexOutputComponents:TVkUInt32;
                                           const pMaxTessellationControlPerPatchOutputComponents:TVkUInt32;
                                           const pMaxTessellationControlTotalOutputComponents:TVkUInt32;
                                           const pMaxTessellationEvaluationInputComponents:TVkUInt32;
                                           const pMaxTessellationEvaluationOutputComponents:TVkUInt32;
                                           const pMaxGeometryShaderInvocations:TVkUInt32;
                                           const pMaxGeometryInputComponents:TVkUInt32;
                                           const pMaxGeometryOutputComponents:TVkUInt32;
                                           const pMaxGeometryOutputVertices:TVkUInt32;
                                           const pMaxGeometryTotalOutputComponents:TVkUInt32;
                                           const pMaxFragmentInputComponents:TVkUInt32;
                                           const pMaxFragmentOutputAttachments:TVkUInt32;
                                           const pMaxFragmentDualSrcAttachments:TVkUInt32;
                                           const pMaxFragmentCombinedOutputResources:TVkUInt32;
                                           const pMaxComputeSharedMemorySize:TVkUInt32;
                                           const pMaxComputeWorkGroupCount:array of TVkUInt32;
                                           const pMaxComputeWorkGroupInvocations:TVkUInt32;
                                           const pMaxComputeWorkGroupSize:array of TVkUInt32;
                                           const pSubPixelPrecisionBits:TVkUInt32;
                                           const pSubTexelPrecisionBits:TVkUInt32;
                                           const pMipmapPrecisionBits:TVkUInt32;
                                           const pMaxDrawIndexedIndexValue:TVkUInt32;
                                           const pMaxDrawIndirectCount:TVkUInt32;
                                           const pMaxSamplerLodBias:TVkFloat;
                                           const pMaxSamplerAnisotropy:TVkFloat;
                                           const pMaxViewports:TVkUInt32;
                                           const pMaxViewportDimensions:array of TVkUInt32;
                                           const pViewportBoundsRange:array of TVkFloat;
                                           const pViewportSubPixelBits:TVkUInt32;
                                           const pMinMemoryMapAlignment:TVkSize;
                                           const pMinTexelBufferOffsetAlignment:TVkDeviceSize;
                                           const pMinUniformBufferOffsetAlignment:TVkDeviceSize;
                                           const pMinStorageBufferOffsetAlignment:TVkDeviceSize;
                                           const pMinTexelOffset:TVkInt32;
                                           const pMaxTexelOffset:TVkUInt32;
                                           const pMinTexelGatherOffset:TVkInt32;
                                           const pMaxTexelGatherOffset:TVkUInt32;
                                           const pMinInterpolationOffset:TVkFloat;
                                           const pMaxInterpolationOffset:TVkFloat;
                                           const pSubPixelInterpolationOffsetBits:TVkUInt32;
                                           const pMaxFramebufferWidth:TVkUInt32;
                                           const pMaxFramebufferHeight:TVkUInt32;
                                           const pMaxFramebufferLayers:TVkUInt32;
                                           const pFramebufferColorSampleCounts:TVkSampleCountFlags;
                                           const pFramebufferDepthSampleCounts:TVkSampleCountFlags;
                                           const pFramebufferStencilSampleCounts:TVkSampleCountFlags;
                                           const pFramebufferNoAttachmentsSampleCounts:TVkSampleCountFlags;
                                           const pMaxColorAttachments:TVkUInt32;
                                           const pSampledImageColorSampleCounts:TVkSampleCountFlags;
                                           const pSampledImageIntegerSampleCounts:TVkSampleCountFlags;
                                           const pSampledImageDepthSampleCounts:TVkSampleCountFlags;
                                           const pSampledImageStencilSampleCounts:TVkSampleCountFlags;
                                           const pStorageImageSampleCounts:TVkSampleCountFlags;
                                           const pMaxSampleMaskWords:TVkUInt32;
                                           const pTimestampComputeAndGraphics:TVkBool32;
                                           const pTimestampPeriod:TVkFloat;
                                           const pMaxClipDistances:TVkUInt32;
                                           const pMaxCullDistances:TVkUInt32;
                                           const pMaxCombinedClipAndCullDistances:TVkUInt32;
                                           const pDiscreteQueuePriorities:TVkUInt32;
                                           const pPointSizeRange:array of TVkFloat;
                                           const pLineWidthRange:array of TVkFloat;
                                           const pPointSizeGranularity:TVkFloat;
                                           const pLineWidthGranularity:TVkFloat;
                                           const pStrictLines:TVkBool32;
                                           const pStandardSampleLocations:TVkBool32;
                                           const pOptimalBufferCopyOffsetAlignment:TVkDeviceSize;
                                           const pOptimalBufferCopyRowPitchAlignment:TVkDeviceSize;
                                           const pNonCoherentAtomSize:TVkDeviceSize);
var ArrayItemCount:TVkInt32;
begin
 FillChar(self,SizeOf(TVkPhysicalDeviceLimits),#0);
 maxImageDimension1D:=pMaxImageDimension1D;
 maxImageDimension2D:=pMaxImageDimension2D;
 maxImageDimension3D:=pMaxImageDimension3D;
 maxImageDimensionCube:=pMaxImageDimensionCube;
 maxImageArrayLayers:=pMaxImageArrayLayers;
 maxTexelBufferElements:=pMaxTexelBufferElements;
 maxUniformBufferRange:=pMaxUniformBufferRange;
 maxStorageBufferRange:=pMaxStorageBufferRange;
 maxPushConstantsSize:=pMaxPushConstantsSize;
 maxMemoryAllocationCount:=pMaxMemoryAllocationCount;
 maxSamplerAllocationCount:=pMaxSamplerAllocationCount;
 bufferImageGranularity:=pBufferImageGranularity;
 sparseAddressSpaceSize:=pSparseAddressSpaceSize;
 maxBoundDescriptorSets:=pMaxBoundDescriptorSets;
 maxPerStageDescriptorSamplers:=pMaxPerStageDescriptorSamplers;
 maxPerStageDescriptorUniformBuffers:=pMaxPerStageDescriptorUniformBuffers;
 maxPerStageDescriptorStorageBuffers:=pMaxPerStageDescriptorStorageBuffers;
 maxPerStageDescriptorSampledImages:=pMaxPerStageDescriptorSampledImages;
 maxPerStageDescriptorStorageImages:=pMaxPerStageDescriptorStorageImages;
 maxPerStageDescriptorInputAttachments:=pMaxPerStageDescriptorInputAttachments;
 maxPerStageResources:=pMaxPerStageResources;
 maxDescriptorSetSamplers:=pMaxDescriptorSetSamplers;
 maxDescriptorSetUniformBuffers:=pMaxDescriptorSetUniformBuffers;
 maxDescriptorSetUniformBuffersDynamic:=pMaxDescriptorSetUniformBuffersDynamic;
 maxDescriptorSetStorageBuffers:=pMaxDescriptorSetStorageBuffers;
 maxDescriptorSetStorageBuffersDynamic:=pMaxDescriptorSetStorageBuffersDynamic;
 maxDescriptorSetSampledImages:=pMaxDescriptorSetSampledImages;
 maxDescriptorSetStorageImages:=pMaxDescriptorSetStorageImages;
 maxDescriptorSetInputAttachments:=pMaxDescriptorSetInputAttachments;
 maxVertexInputAttributes:=pMaxVertexInputAttributes;
 maxVertexInputBindings:=pMaxVertexInputBindings;
 maxVertexInputAttributeOffset:=pMaxVertexInputAttributeOffset;
 maxVertexInputBindingStride:=pMaxVertexInputBindingStride;
 maxVertexOutputComponents:=pMaxVertexOutputComponents;
 maxTessellationGenerationLevel:=pMaxTessellationGenerationLevel;
 maxTessellationPatchSize:=pMaxTessellationPatchSize;
 maxTessellationControlPerVertexInputComponents:=pMaxTessellationControlPerVertexInputComponents;
 maxTessellationControlPerVertexOutputComponents:=pMaxTessellationControlPerVertexOutputComponents;
 maxTessellationControlPerPatchOutputComponents:=pMaxTessellationControlPerPatchOutputComponents;
 maxTessellationControlTotalOutputComponents:=pMaxTessellationControlTotalOutputComponents;
 maxTessellationEvaluationInputComponents:=pMaxTessellationEvaluationInputComponents;
 maxTessellationEvaluationOutputComponents:=pMaxTessellationEvaluationOutputComponents;
 maxGeometryShaderInvocations:=pMaxGeometryShaderInvocations;
 maxGeometryInputComponents:=pMaxGeometryInputComponents;
 maxGeometryOutputComponents:=pMaxGeometryOutputComponents;
 maxGeometryOutputVertices:=pMaxGeometryOutputVertices;
 maxGeometryTotalOutputComponents:=pMaxGeometryTotalOutputComponents;
 maxFragmentInputComponents:=pMaxFragmentInputComponents;
 maxFragmentOutputAttachments:=pMaxFragmentOutputAttachments;
 maxFragmentDualSrcAttachments:=pMaxFragmentDualSrcAttachments;
 maxFragmentCombinedOutputResources:=pMaxFragmentCombinedOutputResources;
 maxComputeSharedMemorySize:=pMaxComputeSharedMemorySize;
 ArrayItemCount:=length(pMaxComputeWorkGroupCount);
 if ArrayItemCount>length(maxComputeWorkGroupCount) then begin
  ArrayItemCount:=length(maxComputeWorkGroupCount);
 end;
 if ArrayItemCount>0 then begin
  Move(pMaxComputeWorkGroupCount[0],maxComputeWorkGroupCount[0],ArrayItemCount*SizeOf(TVkUInt32));
 end;
 maxComputeWorkGroupInvocations:=pMaxComputeWorkGroupInvocations;
 ArrayItemCount:=length(pMaxComputeWorkGroupSize);
 if ArrayItemCount>length(maxComputeWorkGroupSize) then begin
  ArrayItemCount:=length(maxComputeWorkGroupSize);
 end;
 if ArrayItemCount>0 then begin
  Move(pMaxComputeWorkGroupSize[0],maxComputeWorkGroupSize[0],ArrayItemCount*SizeOf(TVkUInt32));
 end;
 subPixelPrecisionBits:=pSubPixelPrecisionBits;
 subTexelPrecisionBits:=pSubTexelPrecisionBits;
 mipmapPrecisionBits:=pMipmapPrecisionBits;
 maxDrawIndexedIndexValue:=pMaxDrawIndexedIndexValue;
 maxDrawIndirectCount:=pMaxDrawIndirectCount;
 maxSamplerLodBias:=pMaxSamplerLodBias;
 maxSamplerAnisotropy:=pMaxSamplerAnisotropy;
 maxViewports:=pMaxViewports;
 ArrayItemCount:=length(pMaxViewportDimensions);
 if ArrayItemCount>length(maxViewportDimensions) then begin
  ArrayItemCount:=length(maxViewportDimensions);
 end;
 if ArrayItemCount>0 then begin
  Move(pMaxViewportDimensions[0],maxViewportDimensions[0],ArrayItemCount*SizeOf(TVkUInt32));
 end;
 ArrayItemCount:=length(pViewportBoundsRange);
 if ArrayItemCount>length(viewportBoundsRange) then begin
  ArrayItemCount:=length(viewportBoundsRange);
 end;
 if ArrayItemCount>0 then begin
  Move(pViewportBoundsRange[0],viewportBoundsRange[0],ArrayItemCount*SizeOf(TVkFloat));
 end;
 viewportSubPixelBits:=pViewportSubPixelBits;
 minMemoryMapAlignment:=pMinMemoryMapAlignment;
 minTexelBufferOffsetAlignment:=pMinTexelBufferOffsetAlignment;
 minUniformBufferOffsetAlignment:=pMinUniformBufferOffsetAlignment;
 minStorageBufferOffsetAlignment:=pMinStorageBufferOffsetAlignment;
 minTexelOffset:=pMinTexelOffset;
 maxTexelOffset:=pMaxTexelOffset;
 minTexelGatherOffset:=pMinTexelGatherOffset;
 maxTexelGatherOffset:=pMaxTexelGatherOffset;
 minInterpolationOffset:=pMinInterpolationOffset;
 maxInterpolationOffset:=pMaxInterpolationOffset;
 subPixelInterpolationOffsetBits:=pSubPixelInterpolationOffsetBits;
 maxFramebufferWidth:=pMaxFramebufferWidth;
 maxFramebufferHeight:=pMaxFramebufferHeight;
 maxFramebufferLayers:=pMaxFramebufferLayers;
 framebufferColorSampleCounts:=pFramebufferColorSampleCounts;
 framebufferDepthSampleCounts:=pFramebufferDepthSampleCounts;
 framebufferStencilSampleCounts:=pFramebufferStencilSampleCounts;
 framebufferNoAttachmentsSampleCounts:=pFramebufferNoAttachmentsSampleCounts;
 maxColorAttachments:=pMaxColorAttachments;
 sampledImageColorSampleCounts:=pSampledImageColorSampleCounts;
 sampledImageIntegerSampleCounts:=pSampledImageIntegerSampleCounts;
 sampledImageDepthSampleCounts:=pSampledImageDepthSampleCounts;
 sampledImageStencilSampleCounts:=pSampledImageStencilSampleCounts;
 storageImageSampleCounts:=pStorageImageSampleCounts;
 maxSampleMaskWords:=pMaxSampleMaskWords;
 timestampComputeAndGraphics:=pTimestampComputeAndGraphics;
 timestampPeriod:=pTimestampPeriod;
 maxClipDistances:=pMaxClipDistances;
 maxCullDistances:=pMaxCullDistances;
 maxCombinedClipAndCullDistances:=pMaxCombinedClipAndCullDistances;
 discreteQueuePriorities:=pDiscreteQueuePriorities;
 ArrayItemCount:=length(pPointSizeRange);
 if ArrayItemCount>length(pointSizeRange) then begin
  ArrayItemCount:=length(pointSizeRange);
 end;
 if ArrayItemCount>0 then begin
  Move(pPointSizeRange[0],pointSizeRange[0],ArrayItemCount*SizeOf(TVkFloat));
 end;
 ArrayItemCount:=length(pLineWidthRange);
 if ArrayItemCount>length(lineWidthRange) then begin
  ArrayItemCount:=length(lineWidthRange);
 end;
 if ArrayItemCount>0 then begin
  Move(pLineWidthRange[0],lineWidthRange[0],ArrayItemCount*SizeOf(TVkFloat));
 end;
 pointSizeGranularity:=pPointSizeGranularity;
 lineWidthGranularity:=pLineWidthGranularity;
 strictLines:=pStrictLines;
 standardSampleLocations:=pStandardSampleLocations;
 optimalBufferCopyOffsetAlignment:=pOptimalBufferCopyOffsetAlignment;
 optimalBufferCopyRowPitchAlignment:=pOptimalBufferCopyRowPitchAlignment;
 nonCoherentAtomSize:=pNonCoherentAtomSize;
end;

constructor TVkPhysicalDeviceProperties.Create(const pApiVersion:TVkUInt32;
                                               const pDriverVersion:TVkUInt32;
                                               const pVendorID:TVkUInt32;
                                               const pDeviceID:TVkUInt32;
                                               const pDeviceType:TVkPhysicalDeviceType;
                                               const pDeviceName:TVkCharString;
                                               const pPipelineCacheUUID:array of TVkUInt8;
                                               const pLimits:TVkPhysicalDeviceLimits;
                                               const pSparseProperties:TVkPhysicalDeviceSparseProperties);
var ArrayItemCount:TVkInt32;
begin
 FillChar(self,SizeOf(TVkPhysicalDeviceProperties),#0);
 apiVersion:=pApiVersion;
 driverVersion:=pDriverVersion;
 vendorID:=pVendorID;
 deviceID:=pDeviceID;
 deviceType:=pDeviceType;
 ArrayItemCount:=length(pDeviceName);
 if ArrayItemCount>length(deviceName) then begin
  ArrayItemCount:=length(deviceName);
 end;
 if ArrayItemCount>0 then begin
  Move(pDeviceName[1],deviceName[0],ArrayItemCount*SizeOf(TVkChar));
 end;
 ArrayItemCount:=length(pPipelineCacheUUID);
 if ArrayItemCount>length(pipelineCacheUUID) then begin
  ArrayItemCount:=length(pipelineCacheUUID);
 end;
 if ArrayItemCount>0 then begin
  Move(pPipelineCacheUUID[0],pipelineCacheUUID[0],ArrayItemCount*SizeOf(TVkUInt8));
 end;
 limits:=pLimits;
 sparseProperties:=pSparseProperties;
end;

constructor TVkSemaphoreCreateInfo.Create(const pFlags:TVkSemaphoreCreateFlags);
begin
 sType:=VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
end;

constructor TVkQueryPoolCreateInfo.Create(const pFlags:TVkQueryPoolCreateFlags;
                                          const pQueryType:TVkQueryType;
                                          const pQueryCount:TVkUInt32;
                                          const pPipelineStatistics:TVkQueryPipelineStatisticFlags);
begin
 sType:=VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 queryType:=pQueryType;
 queryCount:=pQueryCount;
 pipelineStatistics:=pPipelineStatistics;
end;

constructor TVkFramebufferCreateInfo.Create(const pFlags:TVkFramebufferCreateFlags;
                                            const pRenderPass:TVkRenderPass;
                                            const pAttachmentCount:TVkUInt32;
                                            const pPAttachments:PVkImageView;
                                            const pWidth:TVkUInt32;
                                            const pHeight:TVkUInt32;
                                            const pLayers:TVkUInt32);
begin
 sType:=VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
 pNext:=nil;
 flags:=pFlags;
 renderPass:=pRenderPass;
 attachmentCount:=pAttachmentCount;
 pAttachments:=pPAttachments;
 width:=pWidth;
 height:=pHeight;
 layers:=pLayers;
end;

constructor TVkDrawIndirectCommand.Create(const pVertexCount:TVkUInt32;
                                          const pInstanceCount:TVkUInt32;
                                          const pFirstVertex:TVkUInt32;
                                          const pFirstInstance:TVkUInt32);
begin
 vertexCount:=pVertexCount;
 instanceCount:=pInstanceCount;
 firstVertex:=pFirstVertex;
 firstInstance:=pFirstInstance;
end;

constructor TVkDrawIndexedIndirectCommand.Create(const pIndexCount:TVkUInt32;
                                                 const pInstanceCount:TVkUInt32;
                                                 const pFirstIndex:TVkUInt32;
                                                 const pVertexOffset:TVkInt32;
                                                 const pFirstInstance:TVkUInt32);
begin
 indexCount:=pIndexCount;
 instanceCount:=pInstanceCount;
 firstIndex:=pFirstIndex;
 vertexOffset:=pVertexOffset;
 firstInstance:=pFirstInstance;
end;

constructor TVkDispatchIndirectCommand.Create(const pX:TVkUInt32;
                                              const pY:TVkUInt32;
                                              const pZ:TVkUInt32);
begin
 x:=pX;
 y:=pY;
 z:=pZ;
end;

constructor TVkSubmitInfo.Create(const pWaitSemaphoreCount:TVkUInt32;
                                 const pPWaitSemaphores:PVkSemaphore;
                                 const pPWaitDstStageMask:PVkPipelineStageFlags;
                                 const pCommandBufferCount:TVkUInt32;
                                 const pPCommandBuffers:PVkCommandBuffer;
                                 const pSignalSemaphoreCount:TVkUInt32;
                                 const pPSignalSemaphores:PVkSemaphore);
begin
 sType:=VK_STRUCTURE_TYPE_SUBMIT_INFO;
 pNext:=nil;
 waitSemaphoreCount:=pWaitSemaphoreCount;
 pWaitSemaphores:=pPWaitSemaphores;
 pWaitDstStageMask:=pPWaitDstStageMask;
 commandBufferCount:=pCommandBufferCount;
 pCommandBuffers:=pPCommandBuffers;
 signalSemaphoreCount:=pSignalSemaphoreCount;
 pSignalSemaphores:=pPSignalSemaphores;
end;

constructor TVkDisplayPropertiesKHR.Create(const pDisplay:TVkDisplayKHR;
                                           const pDisplayName:PVkChar;
                                           const pPhysicalDimensions:TVkExtent2D;
                                           const pPhysicalResolution:TVkExtent2D;
                                           const pSupportedTransforms:TVkSurfaceTransformFlagsKHR;
                                           const pPlaneReorderPossible:TVkBool32;
                                           const pPersistentContent:TVkBool32);
begin
 display:=pDisplay;
 displayName:=pDisplayName;
 physicalDimensions:=pPhysicalDimensions;
 physicalResolution:=pPhysicalResolution;
 supportedTransforms:=pSupportedTransforms;
 planeReorderPossible:=pPlaneReorderPossible;
 persistentContent:=pPersistentContent;
end;

constructor TVkDisplayPlanePropertiesKHR.Create(const pCurrentDisplay:TVkDisplayKHR;
                                                const pCurrentStackIndex:TVkUInt32);
begin
 currentDisplay:=pCurrentDisplay;
 currentStackIndex:=pCurrentStackIndex;
end;

constructor TVkDisplayModeParametersKHR.Create(const pVisibleRegion:TVkExtent2D;
                                               const pRefreshRate:TVkUInt32);
begin
 visibleRegion:=pVisibleRegion;
 refreshRate:=pRefreshRate;
end;

constructor TVkDisplayModePropertiesKHR.Create(const pDisplayMode:TVkDisplayModeKHR;
                                               const pParameters:TVkDisplayModeParametersKHR);
begin
 displayMode:=pDisplayMode;
 parameters:=pParameters;
end;

constructor TVkDisplayModeCreateInfoKHR.Create(const pFlags:TVkDisplayModeCreateFlagsKHR;
                                               const pParameters:TVkDisplayModeParametersKHR);
begin
 sType:=VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR;
 pNext:=nil;
 flags:=pFlags;
 parameters:=pParameters;
end;

constructor TVkDisplayPlaneCapabilitiesKHR.Create(const pSupportedAlpha:TVkDisplayPlaneAlphaFlagsKHR;
                                                  const pMinSrcPosition:TVkOffset2D;
                                                  const pMaxSrcPosition:TVkOffset2D;
                                                  const pMinSrcExtent:TVkExtent2D;
                                                  const pMaxSrcExtent:TVkExtent2D;
                                                  const pMinDstPosition:TVkOffset2D;
                                                  const pMaxDstPosition:TVkOffset2D;
                                                  const pMinDstExtent:TVkExtent2D;
                                                  const pMaxDstExtent:TVkExtent2D);
begin
 supportedAlpha:=pSupportedAlpha;
 minSrcPosition:=pMinSrcPosition;
 maxSrcPosition:=pMaxSrcPosition;
 minSrcExtent:=pMinSrcExtent;
 maxSrcExtent:=pMaxSrcExtent;
 minDstPosition:=pMinDstPosition;
 maxDstPosition:=pMaxDstPosition;
 minDstExtent:=pMinDstExtent;
 maxDstExtent:=pMaxDstExtent;
end;

constructor TVkDisplaySurfaceCreateInfoKHR.Create(const pFlags:TVkDisplaySurfaceCreateFlagsKHR;
                                                  const pDisplayMode:TVkDisplayModeKHR;
                                                  const pPlaneIndex:TVkUInt32;
                                                  const pPlaneStackIndex:TVkUInt32;
                                                  const pTransform:TVkSurfaceTransformFlagBitsKHR;
                                                  const pGlobalAlpha:TVkFloat;
                                                  const pAlphaMode:TVkDisplayPlaneAlphaFlagBitsKHR;
                                                  const pImageExtent:TVkExtent2D);
begin
 sType:=VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR;
 pNext:=nil;
 flags:=pFlags;
 displayMode:=pDisplayMode;
 planeIndex:=pPlaneIndex;
 planeStackIndex:=pPlaneStackIndex;
 transform:=pTransform;
 globalAlpha:=pGlobalAlpha;
 alphaMode:=pAlphaMode;
 imageExtent:=pImageExtent;
end;

constructor TVkDisplayPresentInfoKHR.Create(const pSrcRect:TVkRect2D;
                                            const pDstRect:TVkRect2D;
                                            const pPersistent:TVkBool32);
begin
 sType:=VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR;
 pNext:=nil;
 srcRect:=pSrcRect;
 dstRect:=pDstRect;
 persistent:=pPersistent;
end;

constructor TVkSurfaceCapabilitiesKHR.Create(const pMinImageCount:TVkUInt32;
                                             const pMaxImageCount:TVkUInt32;
                                             const pCurrentExtent:TVkExtent2D;
                                             const pMinImageExtent:TVkExtent2D;
                                             const pMaxImageExtent:TVkExtent2D;
                                             const pMaxImageArrayLayers:TVkUInt32;
                                             const pSupportedTransforms:TVkSurfaceTransformFlagsKHR;
                                             const pCurrentTransform:TVkSurfaceTransformFlagBitsKHR;
                                             const pSupportedCompositeAlpha:TVkCompositeAlphaFlagsKHR;
                                             const pSupportedUsageFlags:TVkImageUsageFlags);
begin
 minImageCount:=pMinImageCount;
 maxImageCount:=pMaxImageCount;
 currentExtent:=pCurrentExtent;
 minImageExtent:=pMinImageExtent;
 maxImageExtent:=pMaxImageExtent;
 maxImageArrayLayers:=pMaxImageArrayLayers;
 supportedTransforms:=pSupportedTransforms;
 currentTransform:=pCurrentTransform;
 supportedCompositeAlpha:=pSupportedCompositeAlpha;
 supportedUsageFlags:=pSupportedUsageFlags;
end;

{$ifdef Android}
constructor TVkAndroidSurfaceCreateInfoKHR.Create(const pFlags:TVkAndroidSurfaceCreateFlagsKHR;
                                                  const pWindow:PANativeWindow);
begin
 sType:=VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR;
 pNext:=nil;
 flags:=pFlags;
 window:=pWindow;
end;
{$endif}

{$ifdef Mir}
constructor TVkMirSurfaceCreateInfoKHR.Create(const pFlags:TVkMirSurfaceCreateFlagsKHR;
                                              const pConnection:PMirConnection;
                                              const pMirSurface:PMirSurface);
begin
 sType:=VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR;
 pNext:=nil;
 flags:=pFlags;
 connection:=pConnection;
 mirSurface:=pMirSurface;
end;
{$endif}

constructor TVkViSurfaceCreateInfoNN.Create(const pFlags:TVkViSurfaceCreateFlagsNN;
                                            const pWindow:PVkVoid);
begin
 sType:=VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN;
 pNext:=nil;
 flags:=pFlags;
 window:=pWindow;
end;

{$ifdef Wayland}
constructor TVkWaylandSurfaceCreateInfoKHR.Create(const pFlags:TVkWaylandSurfaceCreateFlagsKHR;
                                                  const pDisplay:Pwl_display;
                                                  const pSurface:Pwl_surface);
begin
 sType:=VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR;
 pNext:=nil;
 flags:=pFlags;
 display:=pDisplay;
 surface:=pSurface;
end;
{$endif}

{$ifdef Windows}
constructor TVkWin32SurfaceCreateInfoKHR.Create(const pFlags:TVkWin32SurfaceCreateFlagsKHR;
                                                const pHinstance_:TVkHINSTANCE;
                                                const pHwnd_:TVkHWND);
begin
 sType:=VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
 pNext:=nil;
 flags:=pFlags;
 hinstance_:=pHinstance_;
 hwnd_:=pHwnd_;
end;
{$endif}

{$ifdef X11}
constructor TVkXlibSurfaceCreateInfoKHR.Create(const pFlags:TVkXlibSurfaceCreateFlagsKHR;
                                               const pDpy:PDisplay;
                                               const pWindow:TWindow);
begin
 sType:=VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR;
 pNext:=nil;
 flags:=pFlags;
 dpy:=pDpy;
 window:=pWindow;
end;
{$endif}

{$ifdef XCB}
constructor TVkXcbSurfaceCreateInfoKHR.Create(const pFlags:TVkXcbSurfaceCreateFlagsKHR;
                                              const pConnection:Pxcb_connection;
                                              const pWindow:Txcb_window);
begin
 sType:=VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR;
 pNext:=nil;
 flags:=pFlags;
 connection:=pConnection;
 window:=pWindow;
end;
{$endif}

constructor TVkSurfaceFormatKHR.Create(const pFormat:TVkFormat;
                                       const pColorSpace:TVkColorSpaceKHR);
begin
 format:=pFormat;
 colorSpace:=pColorSpace;
end;

constructor TVkSwapchainCreateInfoKHR.Create(const pFlags:TVkSwapchainCreateFlagsKHR;
                                             const pSurface:TVkSurfaceKHR;
                                             const pMinImageCount:TVkUInt32;
                                             const pImageFormat:TVkFormat;
                                             const pImageColorSpace:TVkColorSpaceKHR;
                                             const pImageExtent:TVkExtent2D;
                                             const pImageArrayLayers:TVkUInt32;
                                             const pImageUsage:TVkImageUsageFlags;
                                             const pImageSharingMode:TVkSharingMode;
                                             const pQueueFamilyIndexCount:TVkUInt32;
                                             const pPQueueFamilyIndices:PVkUInt32;
                                             const pPreTransform:TVkSurfaceTransformFlagBitsKHR;
                                             const pCompositeAlpha:TVkCompositeAlphaFlagBitsKHR;
                                             const pPresentMode:TVkPresentModeKHR;
                                             const pClipped:TVkBool32;
                                             const pOldSwapchain:TVkSwapchainKHR);
begin
 sType:=VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
 pNext:=nil;
 flags:=pFlags;
 surface:=pSurface;
 minImageCount:=pMinImageCount;
 imageFormat:=pImageFormat;
 imageColorSpace:=pImageColorSpace;
 imageExtent:=pImageExtent;
 imageArrayLayers:=pImageArrayLayers;
 imageUsage:=pImageUsage;
 imageSharingMode:=pImageSharingMode;
 queueFamilyIndexCount:=pQueueFamilyIndexCount;
 pQueueFamilyIndices:=pPQueueFamilyIndices;
 preTransform:=pPreTransform;
 compositeAlpha:=pCompositeAlpha;
 presentMode:=pPresentMode;
 clipped:=pClipped;
 oldSwapchain:=pOldSwapchain;
end;

constructor TVkPresentInfoKHR.Create(const pWaitSemaphoreCount:TVkUInt32;
                                     const pPWaitSemaphores:PVkSemaphore;
                                     const pSwapchainCount:TVkUInt32;
                                     const pPSwapchains:PVkSwapchainKHR;
                                     const pPImageIndices:PVkUInt32;
                                     const pPResults:PVkResult);
begin
 sType:=VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
 pNext:=nil;
 waitSemaphoreCount:=pWaitSemaphoreCount;
 pWaitSemaphores:=pPWaitSemaphores;
 swapchainCount:=pSwapchainCount;
 pSwapchains:=pPSwapchains;
 pImageIndices:=pPImageIndices;
 pResults:=pPResults;
end;

constructor TVkDebugReportCallbackCreateInfoEXT.Create(const pFlags:TVkDebugReportFlagsEXT;
                                                       const pPfnCallback:TPFN_vkDebugReportCallbackEXT;
                                                       const pPUserData:PVkVoid);
begin
 sType:=VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT;
 pNext:=nil;
 flags:=pFlags;
 pfnCallback:=pPfnCallback;
 pUserData:=pPUserData;
end;

constructor TVkValidationFlagsEXT.Create(const pDisabledValidationCheckCount:TVkUInt32;
                                         const pPDisabledValidationChecks:PVkValidationCheckEXT);
begin
 sType:=VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT;
 pNext:=nil;
 disabledValidationCheckCount:=pDisabledValidationCheckCount;
 pDisabledValidationChecks:=pPDisabledValidationChecks;
end;

constructor TVkPipelineRasterizationStateRasterizationOrderAMD.Create(const pRasterizationOrder:TVkRasterizationOrderAMD);
begin
 sType:=VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD;
 pNext:=nil;
 rasterizationOrder:=pRasterizationOrder;
end;

constructor TVkDebugMarkerObjectNameInfoEXT.Create(const pObjectType:TVkDebugReportObjectTypeEXT;
                                                   const pObject_:TVkUInt64;
                                                   const pPObjectName:PVkChar);
begin
 sType:=VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT;
 pNext:=nil;
 objectType:=pObjectType;
 object_:=pObject_;
 pObjectName:=pPObjectName;
end;

constructor TVkDebugMarkerObjectTagInfoEXT.Create(const pObjectType:TVkDebugReportObjectTypeEXT;
                                                  const pObject_:TVkUInt64;
                                                  const pTagName:TVkUInt64;
                                                  const pTagSize:TVkSize;
                                                  const pPTag:PVkVoid);
begin
 sType:=VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT;
 pNext:=nil;
 objectType:=pObjectType;
 object_:=pObject_;
 tagName:=pTagName;
 tagSize:=pTagSize;
 pTag:=pPTag;
end;

constructor TVkDebugMarkerMarkerInfoEXT.Create(const pPMarkerName:PVkChar;
                                               const pColor:array of TVkFloat);
var ArrayItemCount:TVkInt32;
begin
 FillChar(self,SizeOf(TVkDebugMarkerMarkerInfoEXT),#0);
 sType:=VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT;
 pNext:=nil;
 pMarkerName:=pPMarkerName;
 ArrayItemCount:=length(pColor);
 if ArrayItemCount>length(color) then begin
  ArrayItemCount:=length(color);
 end;
 if ArrayItemCount>0 then begin
  Move(pColor[0],color[0],ArrayItemCount*SizeOf(TVkFloat));
 end;
end;

constructor TVkDedicatedAllocationImageCreateInfoNV.Create(const pDedicatedAllocation:TVkBool32);
begin
 sType:=VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV;
 pNext:=nil;
 dedicatedAllocation:=pDedicatedAllocation;
end;

constructor TVkDedicatedAllocationBufferCreateInfoNV.Create(const pDedicatedAllocation:TVkBool32);
begin
 sType:=VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV;
 pNext:=nil;
 dedicatedAllocation:=pDedicatedAllocation;
end;

constructor TVkDedicatedAllocationMemoryAllocateInfoNV.Create(const pImage:TVkImage;
                                                              const pBuffer:TVkBuffer);
begin
 sType:=VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV;
 pNext:=nil;
 image:=pImage;
 buffer:=pBuffer;
end;

constructor TVkExternalImageFormatPropertiesNV.Create(const pImageFormatProperties:TVkImageFormatProperties;
                                                      const pExternalMemoryFeatures:TVkExternalMemoryFeatureFlagsNV;
                                                      const pExportFromImportedHandleTypes:TVkExternalMemoryHandleTypeFlagsNV;
                                                      const pCompatibleHandleTypes:TVkExternalMemoryHandleTypeFlagsNV);
begin
 imageFormatProperties:=pImageFormatProperties;
 externalMemoryFeatures:=pExternalMemoryFeatures;
 exportFromImportedHandleTypes:=pExportFromImportedHandleTypes;
 compatibleHandleTypes:=pCompatibleHandleTypes;
end;

constructor TVkExternalMemoryImageCreateInfoNV.Create(const pHandleTypes:TVkExternalMemoryHandleTypeFlagsNV);
begin
 sType:=VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV;
 pNext:=nil;
 handleTypes:=pHandleTypes;
end;

constructor TVkExportMemoryAllocateInfoNV.Create(const pHandleTypes:TVkExternalMemoryHandleTypeFlagsNV);
begin
 sType:=VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV;
 pNext:=nil;
 handleTypes:=pHandleTypes;
end;

constructor TVkImportMemoryWin32HandleInfoNV.Create(const pHandleType:TVkExternalMemoryHandleTypeFlagsNV;
                                                    const pHandle:THANDLE);
begin
 sType:=VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV;
 pNext:=nil;
 handleType:=pHandleType;
 handle:=pHandle;
end;

{$ifdef Windows}
constructor TVkExportMemoryWin32HandleInfoNV.Create(const pPAttributes:PSecurityAttributes;
                                                    const pDwAccess:TVkUInt32);
begin
 sType:=VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV;
 pNext:=nil;
 pAttributes:=pPAttributes;
 dwAccess:=pDwAccess;
end;
{$endif}

constructor TVkWin32KeyedMutexAcquireReleaseInfoNV.Create(const pAcquireCount:TVkUInt32;
                                                          const pPAcquireSyncs:PVkDeviceMemory;
                                                          const pPAcquireKeys:PVkUInt64;
                                                          const pPAcquireTimeoutMilliseconds:PVkUInt32;
                                                          const pReleaseCount:TVkUInt32;
                                                          const pPReleaseSyncs:PVkDeviceMemory;
                                                          const pPReleaseKeys:PVkUInt64);
begin
 sType:=VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV;
 pNext:=nil;
 acquireCount:=pAcquireCount;
 pAcquireSyncs:=pPAcquireSyncs;
 pAcquireKeys:=pPAcquireKeys;
 pAcquireTimeoutMilliseconds:=pPAcquireTimeoutMilliseconds;
 releaseCount:=pReleaseCount;
 pReleaseSyncs:=pPReleaseSyncs;
 pReleaseKeys:=pPReleaseKeys;
end;

constructor TVkDeviceGeneratedCommandsFeaturesNVX.Create(const pComputeBindingPointSupport:TVkBool32);
begin
 sType:=VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX;
 pNext:=nil;
 computeBindingPointSupport:=pComputeBindingPointSupport;
end;

constructor TVkDeviceGeneratedCommandsLimitsNVX.Create(const pMaxIndirectCommandsLayoutTokenCount:TVkUInt32;
                                                       const pMaxObjectEntryCounts:TVkUInt32;
                                                       const pMinSequenceCountBufferOffsetAlignment:TVkUInt32;
                                                       const pMinSequenceIndexBufferOffsetAlignment:TVkUInt32;
                                                       const pMinCommandsTokenBufferOffsetAlignment:TVkUInt32);
begin
 sType:=VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX;
 pNext:=nil;
 maxIndirectCommandsLayoutTokenCount:=pMaxIndirectCommandsLayoutTokenCount;
 maxObjectEntryCounts:=pMaxObjectEntryCounts;
 minSequenceCountBufferOffsetAlignment:=pMinSequenceCountBufferOffsetAlignment;
 minSequenceIndexBufferOffsetAlignment:=pMinSequenceIndexBufferOffsetAlignment;
 minCommandsTokenBufferOffsetAlignment:=pMinCommandsTokenBufferOffsetAlignment;
end;

constructor TVkIndirectCommandsTokenNVX.Create(const pTokenType:TVkIndirectCommandsTokenTypeNVX;
                                               const pBuffer:TVkBuffer;
                                               const pOffset:TVkDeviceSize);
begin
 tokenType:=pTokenType;
 buffer:=pBuffer;
 offset:=pOffset;
end;

constructor TVkIndirectCommandsLayoutTokenNVX.Create(const pTokenType:TVkIndirectCommandsTokenTypeNVX;
                                                     const pBindingUnit:TVkUInt32;
                                                     const pDynamicCount:TVkUInt32;
                                                     const pDivisor:TVkUInt32);
begin
 tokenType:=pTokenType;
 bindingUnit:=pBindingUnit;
 dynamicCount:=pDynamicCount;
 divisor:=pDivisor;
end;

constructor TVkIndirectCommandsLayoutCreateInfoNVX.Create(const pPipelineBindPoint:TVkPipelineBindPoint;
                                                          const pFlags:TVkIndirectCommandsLayoutUsageFlagsNVX;
                                                          const pTokenCount:TVkUInt32;
                                                          const pPTokens:PVkIndirectCommandsLayoutTokenNVX);
begin
 sType:=VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX;
 pNext:=nil;
 pipelineBindPoint:=pPipelineBindPoint;
 flags:=pFlags;
 tokenCount:=pTokenCount;
 pTokens:=pPTokens;
end;

constructor TVkCmdProcessCommandsInfoNVX.Create(const pObjectTable:TVkObjectTableNVX;
                                                const pIndirectCommandsLayout:TVkIndirectCommandsLayoutNVX;
                                                const pIndirectCommandsTokenCount:TVkUInt32;
                                                const pPIndirectCommandsTokens:PVkIndirectCommandsTokenNVX;
                                                const pMaxSequencesCount:TVkUInt32;
                                                const pTargetCommandBuffer:TVkCommandBuffer;
                                                const pSequencesCountBuffer:TVkBuffer;
                                                const pSequencesCountOffset:TVkDeviceSize;
                                                const pSequencesIndexBuffer:TVkBuffer;
                                                const pSequencesIndexOffset:TVkDeviceSize);
begin
 sType:=VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX;
 pNext:=nil;
 objectTable:=pObjectTable;
 indirectCommandsLayout:=pIndirectCommandsLayout;
 indirectCommandsTokenCount:=pIndirectCommandsTokenCount;
 pIndirectCommandsTokens:=pPIndirectCommandsTokens;
 maxSequencesCount:=pMaxSequencesCount;
 targetCommandBuffer:=pTargetCommandBuffer;
 sequencesCountBuffer:=pSequencesCountBuffer;
 sequencesCountOffset:=pSequencesCountOffset;
 sequencesIndexBuffer:=pSequencesIndexBuffer;
 sequencesIndexOffset:=pSequencesIndexOffset;
end;

constructor TVkCmdReserveSpaceForCommandsInfoNVX.Create(const pObjectTable:TVkObjectTableNVX;
                                                        const pIndirectCommandsLayout:TVkIndirectCommandsLayoutNVX;
                                                        const pMaxSequencesCount:TVkUInt32);
begin
 sType:=VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX;
 pNext:=nil;
 objectTable:=pObjectTable;
 indirectCommandsLayout:=pIndirectCommandsLayout;
 maxSequencesCount:=pMaxSequencesCount;
end;

constructor TVkObjectTableCreateInfoNVX.Create(const pObjectCount:TVkUInt32;
                                               const pPObjectEntryTypes:PVkObjectEntryTypeNVX;
                                               const pPObjectEntryCounts:PVkUInt32;
                                               const pPObjectEntryUsageFlags:PVkObjectEntryUsageFlagsNVX;
                                               const pMaxUniformBuffersPerDescriptor:TVkUInt32;
                                               const pMaxStorageBuffersPerDescriptor:TVkUInt32;
                                               const pMaxStorageImagesPerDescriptor:TVkUInt32;
                                               const pMaxSampledImagesPerDescriptor:TVkUInt32;
                                               const pMaxPipelineLayouts:TVkUInt32);
begin
 sType:=VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX;
 pNext:=nil;
 objectCount:=pObjectCount;
 pObjectEntryTypes:=pPObjectEntryTypes;
 pObjectEntryCounts:=pPObjectEntryCounts;
 pObjectEntryUsageFlags:=pPObjectEntryUsageFlags;
 maxUniformBuffersPerDescriptor:=pMaxUniformBuffersPerDescriptor;
 maxStorageBuffersPerDescriptor:=pMaxStorageBuffersPerDescriptor;
 maxStorageImagesPerDescriptor:=pMaxStorageImagesPerDescriptor;
 maxSampledImagesPerDescriptor:=pMaxSampledImagesPerDescriptor;
 maxPipelineLayouts:=pMaxPipelineLayouts;
end;

constructor TVkObjectTableEntryNVX.Create(const pType_:TVkObjectEntryTypeNVX;
                                          const pFlags:TVkObjectEntryUsageFlagsNVX);
begin
 type_:=pType_;
 flags:=pFlags;
end;

constructor TVkObjectTablePipelineEntryNVX.Create(const pType_:TVkObjectEntryTypeNVX;
                                                  const pFlags:TVkObjectEntryUsageFlagsNVX;
                                                  const pPipeline:TVkPipeline);
begin
 type_:=pType_;
 flags:=pFlags;
 pipeline:=pPipeline;
end;

constructor TVkObjectTableDescriptorSetEntryNVX.Create(const pType_:TVkObjectEntryTypeNVX;
                                                       const pFlags:TVkObjectEntryUsageFlagsNVX;
                                                       const pPipelineLayout:TVkPipelineLayout;
                                                       const pDescriptorSet:TVkDescriptorSet);
begin
 type_:=pType_;
 flags:=pFlags;
 pipelineLayout:=pPipelineLayout;
 descriptorSet:=pDescriptorSet;
end;

constructor TVkObjectTableVertexBufferEntryNVX.Create(const pType_:TVkObjectEntryTypeNVX;
                                                      const pFlags:TVkObjectEntryUsageFlagsNVX;
                                                      const pBuffer:TVkBuffer);
begin
 type_:=pType_;
 flags:=pFlags;
 buffer:=pBuffer;
end;

constructor TVkObjectTableIndexBufferEntryNVX.Create(const pType_:TVkObjectEntryTypeNVX;
                                                     const pFlags:TVkObjectEntryUsageFlagsNVX;
                                                     const pBuffer:TVkBuffer;
                                                     const pIndexType:TVkIndexType);
begin
 type_:=pType_;
 flags:=pFlags;
 buffer:=pBuffer;
 indexType:=pIndexType;
end;

constructor TVkObjectTablePushConstantEntryNVX.Create(const pType_:TVkObjectEntryTypeNVX;
                                                      const pFlags:TVkObjectEntryUsageFlagsNVX;
                                                      const pPipelineLayout:TVkPipelineLayout;
                                                      const pStageFlags:TVkShaderStageFlags);
begin
 type_:=pType_;
 flags:=pFlags;
 pipelineLayout:=pPipelineLayout;
 stageFlags:=pStageFlags;
end;

constructor TVkPhysicalDeviceFeatures2KHR.Create(const pFeatures:TVkPhysicalDeviceFeatures);
begin
 sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR;
 pNext:=nil;
 features:=pFeatures;
end;

constructor TVkPhysicalDeviceProperties2KHR.Create(const pProperties:TVkPhysicalDeviceProperties);
begin
 sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR;
 pNext:=nil;
 properties:=pProperties;
end;

constructor TVkFormatProperties2KHR.Create(const pFormatProperties:TVkFormatProperties);
begin
 sType:=VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR;
 pNext:=nil;
 formatProperties:=pFormatProperties;
end;

constructor TVkImageFormatProperties2KHR.Create(const pImageFormatProperties:TVkImageFormatProperties);
begin
 sType:=VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR;
 pNext:=nil;
 imageFormatProperties:=pImageFormatProperties;
end;

constructor TVkPhysicalDeviceImageFormatInfo2KHR.Create(const pFormat:TVkFormat;
                                                        const pType_:TVkImageType;
                                                        const pTiling:TVkImageTiling;
                                                        const pUsage:TVkImageUsageFlags;
                                                        const pFlags:TVkImageCreateFlags);
begin
 sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR;
 pNext:=nil;
 format:=pFormat;
 type_:=pType_;
 tiling:=pTiling;
 usage:=pUsage;
 flags:=pFlags;
end;

constructor TVkQueueFamilyProperties2KHR.Create(const pQueueFamilyProperties:TVkQueueFamilyProperties);
begin
 sType:=VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR;
 pNext:=nil;
 queueFamilyProperties:=pQueueFamilyProperties;
end;

constructor TVkPhysicalDeviceMemoryProperties2KHR.Create(const pMemoryProperties:TVkPhysicalDeviceMemoryProperties);
begin
 sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR;
 pNext:=nil;
 memoryProperties:=pMemoryProperties;
end;

constructor TVkSparseImageFormatProperties2KHR.Create(const pProperties:TVkSparseImageFormatProperties);
begin
 sType:=VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR;
 pNext:=nil;
 properties:=pProperties;
end;

constructor TVkPhysicalDeviceSparseImageFormatInfo2KHR.Create(const pFormat:TVkFormat;
                                                              const pType_:TVkImageType;
                                                              const pSamples:TVkSampleCountFlagBits;
                                                              const pUsage:TVkImageUsageFlags;
                                                              const pTiling:TVkImageTiling);
begin
 sType:=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR;
 pNext:=nil;
 format:=pFormat;
 type_:=pType_;
 samples:=pSamples;
 usage:=pUsage;
 tiling:=pTiling;
end;

constructor TVkSurfaceCapabilities2EXT.Create(const pMinImageCount:TVkUInt32;
                                              const pMaxImageCount:TVkUInt32;
                                              const pCurrentExtent:TVkExtent2D;
                                              const pMinImageExtent:TVkExtent2D;
                                              const pMaxImageExtent:TVkExtent2D;
                                              const pMaxImageArrayLayers:TVkUInt32;
                                              const pSupportedTransforms:TVkSurfaceTransformFlagsKHR;
                                              const pCurrentTransform:TVkSurfaceTransformFlagBitsKHR;
                                              const pSupportedCompositeAlpha:TVkCompositeAlphaFlagsKHR;
                                              const pSupportedUsageFlags:TVkImageUsageFlags;
                                              const pSupportedSurfaceCounters:TVkSurfaceCounterFlagsEXT);
begin
 sType:=VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT;
 pNext:=nil;
 minImageCount:=pMinImageCount;
 maxImageCount:=pMaxImageCount;
 currentExtent:=pCurrentExtent;
 minImageExtent:=pMinImageExtent;
 maxImageExtent:=pMaxImageExtent;
 maxImageArrayLayers:=pMaxImageArrayLayers;
 supportedTransforms:=pSupportedTransforms;
 currentTransform:=pCurrentTransform;
 supportedCompositeAlpha:=pSupportedCompositeAlpha;
 supportedUsageFlags:=pSupportedUsageFlags;
 supportedSurfaceCounters:=pSupportedSurfaceCounters;
end;

constructor TVkDisplayPowerInfoEXT.Create(const pPowerState:TVkDisplayPowerStateEXT);
begin
 sType:=VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT;
 pNext:=nil;
 powerState:=pPowerState;
end;

constructor TVkDeviceEventInfoEXT.Create(const pDeviceEvent:TVkDeviceEventTypeEXT);
begin
 sType:=VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT;
 pNext:=nil;
 deviceEvent:=pDeviceEvent;
end;

constructor TVkDisplayEventInfoEXT.Create(const pDisplayEvent:TVkDisplayEventTypeEXT);
begin
 sType:=VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT;
 pNext:=nil;
 displayEvent:=pDisplayEvent;
end;

constructor TVkSwapchainCounterCreateInfoEXT.Create(const pSurfaceCounters:TVkSurfaceCounterFlagsEXT);
begin
 sType:=VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT;
 pNext:=nil;
 surfaceCounters:=pSurfaceCounters;
end;
{$endif}

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

function TVulkan.MapMemory(device:TVkDevice;memory:TVkDeviceMemory;offset:TVkDeviceSize;size:TVkDeviceSize;flags:TVkMemoryMapFlags;ppData:PPVkVoid):TVkResult;
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

function TVulkan.GetQueryPoolResults(device:TVkDevice;queryPool:TVkQueryPool;firstQuery:TVkUInt32;queryCount:TVkUInt32;dataSize:TVkSize;pData:PVkVoid;stride:TVkDeviceSize;flags:TVkQueryResultFlags):TVkResult;
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

function TVulkan.GetPipelineCacheData(device:TVkDevice;pipelineCache:TVkPipelineCache;pDataSize:PVkSize;pData:PVkVoid):TVkResult;
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

procedure TVulkan.CmdUpdateBuffer(commandBuffer:TVkCommandBuffer;dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;dataSize:TVkDeviceSize;const pData:PVkVoid);
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

procedure TVulkan.CmdPushConstants(commandBuffer:TVkCommandBuffer;layout:TVkPipelineLayout;stageFlags:TVkShaderStageFlags;offset:TVkUInt32;size:TVkUInt32;const pValues:PVkVoid);
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

function TVulkan.CreateViSurfaceNN(instance:TVkInstance;const pCreateInfo:PVkViSurfaceCreateInfoNN;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult;
begin
 result:=fCommands.CreateViSurfaceNN(instance,pCreateInfo,pAllocator,pSurface);
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

{$ifdef Windows}
function TVulkan.CreateWin32SurfaceKHR(instance:TVkInstance;const pCreateInfo:PVkWin32SurfaceCreateInfoKHR;const pAllocator:PVkAllocationCallbacks;pSurface:PVkSurfaceKHR):TVkResult;
begin
 result:=fCommands.CreateWin32SurfaceKHR(instance,pCreateInfo,pAllocator,pSurface);
end;
{$endif}

{$ifdef Windows}
function TVulkan.GetPhysicalDeviceWin32PresentationSupportKHR(physicalDevice:TVkPhysicalDevice;queueFamilyIndex:TVkUInt32):TVkBool32;
begin
 result:=fCommands.GetPhysicalDeviceWin32PresentationSupportKHR(physicalDevice,queueFamilyIndex);
end;
{$endif}

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

procedure TVulkan.DebugReportMessageEXT(instance:TVkInstance;flags:TVkDebugReportFlagsEXT;objectType:TVkDebugReportObjectTypeEXT;object_:TVkUInt64;location:TVkSize;messageCode:TVkInt32;const pLayerPrefix:PVkChar;const pMessage:PVkChar);
begin
 fCommands.DebugReportMessageEXT(instance,flags,objectType,object_,location,messageCode,pLayerPrefix,pMessage);
end;

function TVulkan.DebugMarkerSetObjectNameEXT(device:TVkDevice;pNameInfo:PVkDebugMarkerObjectNameInfoEXT):TVkResult;
begin
 result:=fCommands.DebugMarkerSetObjectNameEXT(device,pNameInfo);
end;

function TVulkan.DebugMarkerSetObjectTagEXT(device:TVkDevice;pTagInfo:PVkDebugMarkerObjectTagInfoEXT):TVkResult;
begin
 result:=fCommands.DebugMarkerSetObjectTagEXT(device,pTagInfo);
end;

procedure TVulkan.CmdDebugMarkerBeginEXT(commandBuffer:TVkCommandBuffer;pMarkerInfo:PVkDebugMarkerMarkerInfoEXT);
begin
 fCommands.CmdDebugMarkerBeginEXT(commandBuffer,pMarkerInfo);
end;

procedure TVulkan.CmdDebugMarkerEndEXT(commandBuffer:TVkCommandBuffer);
begin
 fCommands.CmdDebugMarkerEndEXT(commandBuffer);
end;

procedure TVulkan.CmdDebugMarkerInsertEXT(commandBuffer:TVkCommandBuffer;pMarkerInfo:PVkDebugMarkerMarkerInfoEXT);
begin
 fCommands.CmdDebugMarkerInsertEXT(commandBuffer,pMarkerInfo);
end;

function TVulkan.GetPhysicalDeviceExternalImageFormatPropertiesNV(physicalDevice:TVkPhysicalDevice;format:TVkFormat;type_:TVkImageType;tiling:TVkImageTiling;usage:TVkImageUsageFlags;flags:TVkImageCreateFlags;externalHandleType:TVkExternalMemoryHandleTypeFlagsNV;pExternalImageFormatProperties:PVkExternalImageFormatPropertiesNV):TVkResult;
begin
 result:=fCommands.GetPhysicalDeviceExternalImageFormatPropertiesNV(physicalDevice,format,type_,tiling,usage,flags,externalHandleType,pExternalImageFormatProperties);
end;

{$ifdef Windows}
function TVulkan.GetMemoryWin32HandleNV(device:TVkDevice;memory:TVkDeviceMemory;handleType:TVkExternalMemoryHandleTypeFlagsNV;pHandle:PHANDLE):TVkResult;
begin
 result:=fCommands.GetMemoryWin32HandleNV(device,memory,handleType,pHandle);
end;
{$endif}

procedure TVulkan.CmdDrawIndirectCountAMD(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;countBuffer:TVkBuffer;countBufferOffset:TVkDeviceSize;maxDrawCount:TVkUInt32;stride:TVkUInt32);
begin
 fCommands.CmdDrawIndirectCountAMD(commandBuffer,buffer,offset,countBuffer,countBufferOffset,maxDrawCount,stride);
end;

procedure TVulkan.CmdDrawIndexedIndirectCountAMD(commandBuffer:TVkCommandBuffer;buffer:TVkBuffer;offset:TVkDeviceSize;countBuffer:TVkBuffer;countBufferOffset:TVkDeviceSize;maxDrawCount:TVkUInt32;stride:TVkUInt32);
begin
 fCommands.CmdDrawIndexedIndirectCountAMD(commandBuffer,buffer,offset,countBuffer,countBufferOffset,maxDrawCount,stride);
end;

procedure TVulkan.CmdProcessCommandsNVX(commandBuffer:TVkCommandBuffer;const pProcessCommandsInfo:PVkCmdProcessCommandsInfoNVX);
begin
 fCommands.CmdProcessCommandsNVX(commandBuffer,pProcessCommandsInfo);
end;

procedure TVulkan.CmdReserveSpaceForCommandsNVX(commandBuffer:TVkCommandBuffer;const pReserveSpaceInfo:PVkCmdReserveSpaceForCommandsInfoNVX);
begin
 fCommands.CmdReserveSpaceForCommandsNVX(commandBuffer,pReserveSpaceInfo);
end;

function TVulkan.CreateIndirectCommandsLayoutNVX(device:TVkDevice;const pCreateInfo:PVkIndirectCommandsLayoutCreateInfoNVX;const pAllocator:PVkAllocationCallbacks;pIndirectCommandsLayout:PVkIndirectCommandsLayoutNVX):TVkResult;
begin
 result:=fCommands.CreateIndirectCommandsLayoutNVX(device,pCreateInfo,pAllocator,pIndirectCommandsLayout);
end;

procedure TVulkan.DestroyIndirectCommandsLayoutNVX(device:TVkDevice;indirectCommandsLayout:TVkIndirectCommandsLayoutNVX;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroyIndirectCommandsLayoutNVX(device,indirectCommandsLayout,pAllocator);
end;

function TVulkan.CreateObjectTableNVX(device:TVkDevice;const pCreateInfo:PVkObjectTableCreateInfoNVX;const pAllocator:PVkAllocationCallbacks;pObjectTable:PVkObjectTableNVX):TVkResult;
begin
 result:=fCommands.CreateObjectTableNVX(device,pCreateInfo,pAllocator,pObjectTable);
end;

procedure TVulkan.DestroyObjectTableNVX(device:TVkDevice;objectTable:TVkObjectTableNVX;const pAllocator:PVkAllocationCallbacks);
begin
 fCommands.DestroyObjectTableNVX(device,objectTable,pAllocator);
end;

function TVulkan.RegisterObjectsNVX(device:TVkDevice;objectTable:TVkObjectTableNVX;objectCount:TVkUInt32;const ppObjectTableEntries:PPVkObjectTableEntryNVX;const pObjectIndices:PVkUInt32):TVkResult;
begin
 result:=fCommands.RegisterObjectsNVX(device,objectTable,objectCount,ppObjectTableEntries,pObjectIndices);
end;

function TVulkan.UnregisterObjectsNVX(device:TVkDevice;objectTable:TVkObjectTableNVX;objectCount:TVkUInt32;const pObjectEntryTypes:PVkObjectEntryTypeNVX;const pObjectIndices:PVkUInt32):TVkResult;
begin
 result:=fCommands.UnregisterObjectsNVX(device,objectTable,objectCount,pObjectEntryTypes,pObjectIndices);
end;

procedure TVulkan.GetPhysicalDeviceGeneratedCommandsPropertiesNVX(physicalDevice:TVkPhysicalDevice;pFeatures:PVkDeviceGeneratedCommandsFeaturesNVX;pLimits:PVkDeviceGeneratedCommandsLimitsNVX);
begin
 fCommands.GetPhysicalDeviceGeneratedCommandsPropertiesNVX(physicalDevice,pFeatures,pLimits);
end;

procedure TVulkan.GetPhysicalDeviceFeatures2KHR(physicalDevice:TVkPhysicalDevice;pFeatures:PVkPhysicalDeviceFeatures2KHR);
begin
 fCommands.GetPhysicalDeviceFeatures2KHR(physicalDevice,pFeatures);
end;

procedure TVulkan.GetPhysicalDeviceProperties2KHR(physicalDevice:TVkPhysicalDevice;pProperties:PVkPhysicalDeviceProperties2KHR);
begin
 fCommands.GetPhysicalDeviceProperties2KHR(physicalDevice,pProperties);
end;

procedure TVulkan.GetPhysicalDeviceFormatProperties2KHR(physicalDevice:TVkPhysicalDevice;format:TVkFormat;pFormatProperties:PVkFormatProperties2KHR);
begin
 fCommands.GetPhysicalDeviceFormatProperties2KHR(physicalDevice,format,pFormatProperties);
end;

function TVulkan.GetPhysicalDeviceImageFormatProperties2KHR(physicalDevice:TVkPhysicalDevice;const pImageFormatInfo:PVkPhysicalDeviceImageFormatInfo2KHR;pImageFormatProperties:PVkImageFormatProperties2KHR):TVkResult;
begin
 result:=fCommands.GetPhysicalDeviceImageFormatProperties2KHR(physicalDevice,pImageFormatInfo,pImageFormatProperties);
end;

procedure TVulkan.GetPhysicalDeviceQueueFamilyProperties2KHR(physicalDevice:TVkPhysicalDevice;pQueueFamilyPropertyCount:PVkUInt32;pQueueFamilyProperties:PVkQueueFamilyProperties2KHR);
begin
 fCommands.GetPhysicalDeviceQueueFamilyProperties2KHR(physicalDevice,pQueueFamilyPropertyCount,pQueueFamilyProperties);
end;

procedure TVulkan.GetPhysicalDeviceMemoryProperties2KHR(physicalDevice:TVkPhysicalDevice;pMemoryProperties:PVkPhysicalDeviceMemoryProperties2KHR);
begin
 fCommands.GetPhysicalDeviceMemoryProperties2KHR(physicalDevice,pMemoryProperties);
end;

procedure TVulkan.GetPhysicalDeviceSparseImageFormatProperties2KHR(physicalDevice:TVkPhysicalDevice;const pFormatInfo:PVkPhysicalDeviceSparseImageFormatInfo2KHR;pPropertyCount:PVkUInt32;pProperties:PVkSparseImageFormatProperties2KHR);
begin
 fCommands.GetPhysicalDeviceSparseImageFormatProperties2KHR(physicalDevice,pFormatInfo,pPropertyCount,pProperties);
end;

procedure TVulkan.TrimCommandPoolKHR(device:TVkDevice;commandPool:TVkCommandPool;flags:TVkCommandPoolTrimFlagsKHR);
begin
 fCommands.TrimCommandPoolKHR(device,commandPool,flags);
end;

function TVulkan.ReleaseDisplayEXT(physicalDevice:TVkPhysicalDevice;display:TVkDisplayKHR):TVkResult;
begin
 result:=fCommands.ReleaseDisplayEXT(physicalDevice,display);
end;

{$ifdef X11}
function TVulkan.AcquireXlibDisplayEXT(physicalDevice:TVkPhysicalDevice;dpy:PDisplay;display:TVkDisplayKHR):TVkResult;
begin
 result:=fCommands.AcquireXlibDisplayEXT(physicalDevice,dpy,display);
end;
{$endif}

{$ifdef X11}
function TVulkan.GetRandROutputDisplayEXT(physicalDevice:TVkPhysicalDevice;dpy:PDisplay;rrOutput:TRROutput;pDisplay:PVkDisplayKHR):TVkResult;
begin
 result:=fCommands.GetRandROutputDisplayEXT(physicalDevice,dpy,rrOutput,pDisplay);
end;
{$endif}

function TVulkan.DisplayPowerControlEXT(device:TVkDevice;display:TVkDisplayKHR;const pDisplayPowerInfo:PVkDisplayPowerInfoEXT):TVkResult;
begin
 result:=fCommands.DisplayPowerControlEXT(device,display,pDisplayPowerInfo);
end;

function TVulkan.RegisterDeviceEventEXT(device:TVkDevice;const pDeviceEventInfo:PVkDeviceEventInfoEXT;const pAllocator:PVkAllocationCallbacks;pFence:PVkFence):TVkResult;
begin
 result:=fCommands.RegisterDeviceEventEXT(device,pDeviceEventInfo,pAllocator,pFence);
end;

function TVulkan.RegisterDisplayEventEXT(device:TVkDevice;display:TVkDisplayKHR;const pDisplayEventInfo:PVkDisplayEventInfoEXT;const pAllocator:PVkAllocationCallbacks;pFence:PVkFence):TVkResult;
begin
 result:=fCommands.RegisterDisplayEventEXT(device,display,pDisplayEventInfo,pAllocator,pFence);
end;

function TVulkan.GetSwapchainCounterEXT(device:TVkDevice;swapchain:TVkSwapchainKHR;counter:TVkSurfaceCounterFlagBitsEXT;pCounterValue:PVkUInt64):TVkResult;
begin
 result:=fCommands.GetSwapchainCounterEXT(device,swapchain,counter,pCounterValue);
end;

function TVulkan.GetPhysicalDeviceSurfaceCapabilities2EXT(physicalDevice:TVkPhysicalDevice;surface:TVkSurfaceKHR;pSurfaceCapabilities:PVkSurfaceCapabilities2EXT):TVkResult;
begin
 result:=fCommands.GetPhysicalDeviceSurfaceCapabilities2EXT(physicalDevice,surface,pSurfaceCapabilities);
end;

initialization
 vk:=TVulkan.Create;
finalization
 vk.Free;
 if assigned(LibVulkan) then begin
  vkFreeLibrary(LibVulkan);
 end;
end.
