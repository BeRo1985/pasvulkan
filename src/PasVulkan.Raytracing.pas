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
       fAccelerationStructure:TVkAccelerationStructureKHR;
       fAccelerationStructureBuffer:TpvVulkanBuffer;
      public
       constructor Create(const aDevice:TpvVulkanDevice;const aAccelerationStructureSize:TVkDeviceSize;const aAccelerationStructureType:TVkAccelerationStructureTypeKHR); reintroduce;
       destructor Destroy; override;
       procedure Clone(const aCommandBuffer:TpvVulkanCommandBuffer;const aSourceAccelerationStructure:TpvRaytracingAccelerationStructure);
      published
       property Device:TpvVulkanDevice read fDevice;
       property AccelerationStructure:TVkAccelerationStructureKHR read fAccelerationStructure;
     end;

     { TpvRaytracingAccelerationStructureScratch }
     TpvRaytracingAccelerationStructureScratch=class
      private
       fDevice:TpvVulkanDevice;
       fAccelerationStructureScratchBuffer:TpvVulkanBuffer;
       fDeviceAddress:TVkDeviceAddress;
      public
       constructor Create(const aDevice:TpvVulkanDevice;const aAccelerationStructureScratchSize:TVkDeviceSize); reintroduce;
       destructor Destroy; override;
      published
       property Device:TpvVulkanDevice read fDevice;
       property AccelerationStructureScratchBuffer:TpvVulkanBuffer read fAccelerationStructureScratchBuffer;
       property DeviceAddress:TVkDeviceAddress read fDeviceAddress;
     end;
     
implementation

{ TpvRaytracingAccelerationStructure }

constructor TpvRaytracingAccelerationStructure.Create(const aDevice:TpvVulkanDevice;const aAccelerationStructureSize:TVkDeviceSize;const aAccelerationStructureType:TVkAccelerationStructureTypeKHR);
var AccelerationStructureCreateInfo:TVkAccelerationStructureCreateInfoKHR;
    AccelerationStructure:TVkAccelerationStructureKHR;
    AccelerationStructureBuffer:TpvVulkanBuffer;
    ResultValue:TVkResult;
begin

 inherited Create;

 fDevice:=aDevice;

 fAccelerationStructure:=VK_NULL_HANDLE;

 fAccelerationStructureBuffer:=nil;

 AccelerationStructureBuffer:=TpvVulkanBuffer.Create(fDevice,
                                                     aAccelerationStructureSize,
                                                     TVkBufferUsageFlags(VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR),
                                                     TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                     [],
                                                     0,
                                                     TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     [],
                                                     256);

 AccelerationStructureCreateInfo:=TVkAccelerationStructureCreateInfoKHR.Create(0,
                                                                               fAccelerationStructureBuffer.Handle,
                                                                               0,
                                                                               aAccelerationStructureSize,
                                                                               aAccelerationStructureType,
                                                                               0);

 ResultValue:=fDevice.Commands.Commands.CreateAccelerationStructureKHR(fDevice.Handle,@AccelerationStructureCreateInfo,nil,@AccelerationStructure);

 if ResultValue=VK_SUCCESS then begin

  FreeAndNil(fAccelerationStructureBuffer);

  fAccelerationStructure:=AccelerationStructure;

  fAccelerationStructureBuffer:=AccelerationStructureBuffer;

 end else begin

  FreeAndNil(fAccelerationStructureBuffer);

  VulkanCheckResult(ResultValue);

  raise EpvRaytracing.Create('TpvRaytracingAccelerationStructure.Create');

 end;

end;

destructor TpvRaytracingAccelerationStructure.Destroy;
begin
 if fAccelerationStructure<>VK_NULL_HANDLE then begin
  fDevice.Commands.Commands.DestroyAccelerationStructureKHR(fDevice.Handle,fAccelerationStructure,nil);
  fAccelerationStructure:=VK_NULL_HANDLE;
 end;
 FreeAndNil(fAccelerationStructureBuffer);
 inherited Destroy;
end;

procedure TpvRaytracingAccelerationStructure.Clone(const aCommandBuffer:TpvVulkanCommandBuffer;const aSourceAccelerationStructure:TpvRaytracingAccelerationStructure);
var CopyAccelerationStructureInfo:TVkCopyAccelerationStructureInfoKHR;
begin

 Assert(aCommandBuffer<>nil);
 Assert(aSourceAccelerationStructure<>nil);
 Assert(aSourceAccelerationStructure.fDevice=aCommandBuffer.Device);
 Assert(fDevice=aCommandBuffer.Device);
 Assert(aSourceAccelerationStructure.fAccelerationStructure<>VK_NULL_HANDLE);
 Assert(fAccelerationStructure<>VK_NULL_HANDLE);

 FillChar(CopyAccelerationStructureInfo,SizeOf(TVkCopyAccelerationStructureInfoKHR),#0);
 CopyAccelerationStructureInfo.sType:=VK_STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR;
 CopyAccelerationStructureInfo.pNext:=nil;
 CopyAccelerationStructureInfo.src:=aSourceAccelerationStructure.AccelerationStructure;
 CopyAccelerationStructureInfo.dst:=fAccelerationStructure;
 CopyAccelerationStructureInfo.mode:=VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR;

 fDevice.Commands.Commands.CmdCopyAccelerationStructureKHR(aCommandBuffer.Handle,@CopyAccelerationStructureInfo);
 
end;

{ TpvRaytracingAccelerationStructureScratch }

constructor TpvRaytracingAccelerationStructureScratch.Create(const aDevice:TpvVulkanDevice;const aAccelerationStructureScratchSize:TVkDeviceSize);
var BufferDeviceAddressInfo:TVkBufferDeviceAddressInfoKHR;
begin

 inherited Create;

 fDevice:=aDevice;

 fAccelerationStructureScratchBuffer:=nil;

 fDeviceAddress:=0;

 fAccelerationStructureScratchBuffer:=TpvVulkanBuffer.Create(fDevice,
                                                             aAccelerationStructureScratchSize,
                                                             TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT),
                                                             TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                             [],
                                                             0,
                                                             TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                             0,
                                                             0,
                                                             0,
                                                             0,
                                                             0,
                                                             0,
                                                             [],
                                                             256);

 BufferDeviceAddressInfo:=TVkBufferDeviceAddressInfoKHR.Create(fAccelerationStructureScratchBuffer.Handle);

 fDeviceAddress:=fDevice.Commands.Commands.GetBufferDeviceAddressKHR(fDevice.Handle,
                                                                     @BufferDeviceAddressInfo);

end;

destructor TpvRaytracingAccelerationStructureScratch.Destroy;
begin
 FreeAndNil(fAccelerationStructureScratchBuffer);
 fDeviceAddress:=0;
 inherited Destroy;
end;

end.
