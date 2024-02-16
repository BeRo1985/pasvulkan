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

     { TpvRaytracingAccelerationStructure }
     TpvRaytracingAccelerationStructure=class
      private
       fDevice:TpvVulkanDevice;
       fFlags:TVkBuildAccelerationStructureFlagsKHR;
       fBuildGeometryInfo:TVkAccelerationStructureBuildGeometryInfoKHR;
       fBuildSizesInfo:TVkAccelerationStructureBuildSizesInfoKHR;
       fType:TVkAccelerationStructureTypeKHR;
       fHandle:TVkAccelerationStructureKHR;
      public
       constructor Create(const aDevice:TpvVulkanDevice;
                          const aFlags:TVkBuildAccelerationStructureFlagsKHR=TVkBuildAccelerationStructureFlagsKHR(VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR);                          
                          const aType:TVkAccelerationStructureTypeKHR=TVkAccelerationStructureTypeKHR(VK_ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR)); overload;
       constructor Create(const aFrom:TpvRaytracingAccelerationStructure); overload;
       destructor Destroy; override;
       function GetBuildSizes(const aMaxPrimitiveCounts:PVkUInt32):TVkAccelerationStructureBuildSizesInfoKHR; overload;
       function GetBuildSizes(const aMaxPrimitiveCounts:array of TVkUInt32):TVkAccelerationStructureBuildSizesInfoKHR; overload;
       procedure CreateAccelerationStructure(const aResultBuffer:TpvVulkanBuffer;const aResultOffset:TVkDeviceSize);
       procedure MemoryBarrier(const aCommandBuffer:TpvVulkanCommandBuffer);    
     end;

implementation

{ TpvRaytracingAccelerationStructure }

constructor TpvRaytracingAccelerationStructure.Create(const aDevice:TpvVulkanDevice;
                                                      const aFlags:TVkBuildAccelerationStructureFlagsKHR;
                                                      const aType:TVkAccelerationStructureTypeKHR);
begin

 inherited Create;

 fDevice:=aDevice;

 fFlags:=aFlags;

 fType:=aType;

 FillChar(fBuildGeometryInfo,SizeOf(TVkAccelerationStructureBuildGeometryInfoKHR),#0);

 fBuildGeometryInfo.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR;
 fBuildGeometryInfo.pNext:=nil;
 fBuildGeometryInfo.type_:=fType;
 fBuildGeometryInfo.flags:=fFlags;
 fBuildGeometryInfo.mode:=VK_BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR;
 fBuildGeometryInfo.srcAccelerationStructure:=VK_NULL_HANDLE;
 fBuildGeometryInfo.dstAccelerationStructure:=VK_NULL_HANDLE;
 fBuildGeometryInfo.geometryCount:=0;
 fBuildGeometryInfo.pGeometries:=nil;
 fBuildGeometryInfo.ppGeometries:=nil;
 fBuildGeometryInfo.scratchData.deviceAddress:=0;

 FillChar(fBuildSizesInfo,SizeOf(TVkAccelerationStructureBuildSizesInfoKHR),#0);

 fHandle:=VK_NULL_HANDLE;

end;

constructor TpvRaytracingAccelerationStructure.Create(const aFrom:TpvRaytracingAccelerationStructure);
begin
 inherited Create;
 fDevice:=aFrom.fDevice;
 fFlags:=aFrom.fFlags;
 fBuildGeometryInfo:=aFrom.fBuildGeometryInfo;
 fBuildSizesInfo:=aFrom.fBuildSizesInfo;
 fType:=aFrom.fType;
 fHandle:=aFrom.fHandle;
end;

destructor TpvRaytracingAccelerationStructure.Destroy;
begin
 if fHandle<>VK_NULL_HANDLE then begin
  fDevice.Commands.DestroyAccelerationStructureKHR(fDevice.Handle,fHandle,nil);
  fHandle:=VK_NULL_HANDLE;
 end;
 inherited Destroy;
end;

function TpvRaytracingAccelerationStructure.GetBuildSizes(const aMaxPrimitiveCounts:PVkUInt32):TVkAccelerationStructureBuildSizesInfoKHR;
const AccelerationStructureAlignment=TpvUInt64(256);
var ScratchAlignment:TpvUInt64;
begin

 FillChar(result,SizeOf(TVkAccelerationStructureBuildSizesInfoKHR),#0);
 result.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR;

 fDevice.Commands.GetAccelerationStructureBuildSizesKHR(fDevice.Handle,
                                                        VK_ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR,
                                                        @fBuildGeometryInfo,
                                                        Pointer(aMaxPrimitiveCounts),
                                                        @result);

 ScratchAlignment:=fDevice.PhysicalDevice.AccelerationStructurePropertiesKHR.minAccelerationStructureScratchOffsetAlignment;

 result.accelerationStructureSize:=(result.accelerationStructureSize+(AccelerationStructureAlignment-1)) and not (AccelerationStructureAlignment-1);
 result.buildScratchSize:=(result.buildScratchSize+(ScratchAlignment-1)) and not (ScratchAlignment-1);

end;

function TpvRaytracingAccelerationStructure.GetBuildSizes(const aMaxPrimitiveCounts:array of TVkUInt32):TVkAccelerationStructureBuildSizesInfoKHR;
begin
 result:=GetBuildSizes(@aMaxPrimitiveCounts[0]);
end;

procedure TpvRaytracingAccelerationStructure.CreateAccelerationStructure(const aResultBuffer:TpvVulkanBuffer;const aResultOffset:TVkDeviceSize);
var CreateInfo:TVkAccelerationStructureCreateInfoKHR;
begin

 FillChar(CreateInfo,SizeOf(TVkAccelerationStructureCreateInfoKHR),#0);
 CreateInfo.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR;
 CreateInfo.pNext:=nil;
 CreateInfo.type_:=fBuildGeometryInfo.type_;
 CreateInfo.deviceAddress:=0;
 CreateInfo.size:=fBuildSizesInfo.accelerationStructureSize;
 CreateInfo.buffer:=aResultBuffer.Handle;
 CreateInfo.offset:=aResultOffset;

 VulkanCheckResult(fDevice.Commands.CreateAccelerationStructureKHR(fDevice.Handle,@CreateInfo,nil,@fHandle));

end;

procedure TpvRaytracingAccelerationStructure.MemoryBarrier(const aCommandBuffer:TpvVulkanCommandBuffer);
var MemoryBarrier:TVkMemoryBarrier;
begin

 FillChar(MemoryBarrier,SizeOf(TVkMemoryBarrier),#0);
 MemoryBarrier.sType:=VK_STRUCTURE_TYPE_MEMORY_BARRIER;
 MemoryBarrier.pNext:=nil;
 MemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR) or TVkAccessFlags(VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR);
 MemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR) or TVkAccessFlags(VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR);

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR),
                                   0,
                                   1,@MemoryBarrier,
                                   0,nil,
                                   0,nil);

end;

end.
