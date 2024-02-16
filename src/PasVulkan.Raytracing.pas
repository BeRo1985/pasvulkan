(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2024, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
 * 5. Write code which's compatible with Delphi >= 2009 and FreePascal >=     *
 *    3.1.1                                                                   *
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
unit PasVulkan.Raytracing;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework;

type EpvRaytracing=class(Exception);

     { TpvRaytracingProperties }
     TpvRaytracingProperties=class
      private
       fDevice:TpvVulkanDevice;
       fAccelerationStructureProperties:PVkPhysicalDeviceAccelerationStructurePropertiesKHR;
       fRayTracingPipelineProperties:PVkPhysicalDeviceRayTracingPipelinePropertiesKHR;
       function GetMaxDescriptorSetAccelerationStructures:TVkUInt32;
       function GetMaxGeometryCount:TVkUInt64;
       function GetMaxInstanceCount:TVkUInt64;
       function GetMaxPrimitiveCount:TVkUInt64;
       function GetMaxRayRecursionDepth:TVkUInt32;
       function GetMaxShaderGroupStride:TVkUInt32;
       function GetMinAccelerationStructureScratchOffsetAlignment:TVkUInt32;
       function GetShaderGroupBaseAlignment:TVkUInt32;
       function GetShaderGroupHandleCaptureReplaySize:TVkUInt32;
       function GetShaderGroupHandleSize:TVkUInt32;
      public
       constructor Create(const aDevice:TpvVulkanDevice);
       destructor Destroy; override;
      published
       property Device:TpvVulkanDevice read fDevice;
      public 
       property AccelerationStructureProperties:PVkPhysicalDeviceAccelerationStructurePropertiesKHR read fAccelerationStructureProperties;
       property RayTracingPipelineProperties:PVkPhysicalDeviceRayTracingPipelinePropertiesKHR read fRayTracingPipelineProperties;
      published
       property MaxDescriptorSetAccelerationStructures:TVkUInt32 read GetMaxDescriptorSetAccelerationStructures;
       property MaxGeometryCount:TVkUInt64 read GetMaxGeometryCount;
       property MaxInstanceCount:TVkUInt64 read GetMaxInstanceCount;
       property MaxPrimitiveCount:TVkUInt64 read GetMaxPrimitiveCount;
       property MaxRayRecursionDepth:TVkUInt32 read GetMaxRayRecursionDepth;
       property MaxShaderGroupStride:TVkUInt32 read GetMaxShaderGroupStride;
       property MinAccelerationStructureScratchOffsetAlignment:TVkUInt32 read GetMinAccelerationStructureScratchOffsetAlignment;
       property ShaderGroupBaseAlignment:TVkUInt32 read GetShaderGroupBaseAlignment;
       property ShaderGroupHandleCaptureReplaySize:TVkUInt32 read GetShaderGroupHandleCaptureReplaySize;
       property ShaderGroupHandleSize:TVkUInt32 read GetShaderGroupHandleSize; 
     end;

     { TpvRaytracingAccelerationStructure }
     TpvRaytracingAccelerationStructure=class
      private
       fDevice:TpvVulkanDevice;
       fRaytracingProperties:TpvRaytracingProperties;
       fFlags:TVkBuildAccelerationStructureFlagsKHR;
       fBuildGeometryInfo:TVkAccelerationStructureBuildGeometryInfoKHR;
       fBuildSizesInfo:TVkAccelerationStructureBuildSizesInfoKHR;
       fType:TVkAccelerationStructureTypeKHR;
       fHandle:TVkAccelerationStructureKHR;
      public
       
     end;

implementation

{ TpvRaytracingProperties }

constructor TpvRaytracingProperties.Create(const aDevice:TpvVulkanDevice);
begin
 inherited Create;
 fDevice:=aDevice;
 fAccelerationStructureProperties:=@fDevice.PhysicalDevice.AccelerationStructurePropertiesKHR;
 fRayTracingPipelineProperties:=@fDevice.PhysicalDevice.RayTracingPipelinePropertiesKHR;
end;

destructor TpvRaytracingProperties.Destroy;
begin
 inherited Destroy;
end;

function TpvRaytracingProperties.GetMaxDescriptorSetAccelerationStructures:TVkUInt32;
begin
 result:=fAccelerationStructureProperties^.maxDescriptorSetAccelerationStructures;
end;

function TpvRaytracingProperties.GetMaxGeometryCount:TVkUInt64;
begin
 result:=fAccelerationStructureProperties^.maxGeometryCount;
end;

function TpvRaytracingProperties.GetMaxInstanceCount:TVkUInt64;
begin
 result:=fAccelerationStructureProperties^.maxInstanceCount;
end;

function TpvRaytracingProperties.GetMaxPrimitiveCount:TVkUInt64;
begin
 result:=fAccelerationStructureProperties^.maxPrimitiveCount;
end;

function TpvRaytracingProperties.GetMaxRayRecursionDepth:TVkUInt32;
begin
 result:=fRayTracingPipelineProperties^.maxRayRecursionDepth;
end;

function TpvRaytracingProperties.GetMaxShaderGroupStride:TVkUInt32;
begin
 result:=fRayTracingPipelineProperties^.maxShaderGroupStride;
end;

function TpvRaytracingProperties.GetMinAccelerationStructureScratchOffsetAlignment:TVkUInt32;
begin
 result:=fAccelerationStructureProperties^.minAccelerationStructureScratchOffsetAlignment;
end;

function TpvRaytracingProperties.GetShaderGroupBaseAlignment:TVkUInt32;
begin
 result:=fRayTracingPipelineProperties^.shaderGroupBaseAlignment;
end;

function TpvRaytracingProperties.GetShaderGroupHandleCaptureReplaySize:TVkUInt32;
begin
 result:=fRayTracingPipelineProperties^.shaderGroupHandleCaptureReplaySize;
end;

function TpvRaytracingProperties.GetShaderGroupHandleSize:TVkUInt32;
begin
 result:=fRayTracingPipelineProperties^.shaderGroupHandleSize;
end;

end.
