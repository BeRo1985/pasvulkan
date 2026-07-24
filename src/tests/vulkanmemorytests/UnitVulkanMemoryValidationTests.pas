(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2026, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
 * 4. After a pull request, check the status of your pull request at          *
 *    http://github.com/BeRo1985/pasvulkan                                    *
 * 5. Write code which's compatible with Delphi >= 2009 and FreePascal >=     *
 *    3.1.1                                                                   *
 * 6. Don't use Delphi-only, FreePascal-only or Lazarus-only libraries/units, *
 *    but if needed, make it out-ifdef-able.                                  *
 * 7. No use of third-party libraries/units as possible, but if needed, make  *
 *    it out-ifdef-able.                                                       *
 * 8. Try to use const when possible.                                         *
 * 9. Make sure to comment out writeln, used while debugging.                 *
 * 10. Make sure the code compiles on 32-bit and 64-bit platforms (x86-32,    *
 *     x86-64, ARM, ARM64, etc.).                                             *
 * 11. Make sure the code runs on all platforms with Vulkan support           *
 *                                                                            *
 ******************************************************************************)
unit UnitVulkanMemoryValidationTests;
{$i ../../PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

procedure RunVulkanMemoryValidationTests;

implementation

uses SysUtils,
     Classes,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Framework,
     UnitVulkanMemoryTestUtils;

type TValidationMessageCollector=class
      private
       fMessages:TStringList;
      public
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       function Count:TpvSizeInt;
       function Text:TpvUTF8String;
       function DebugUtilsMessengerCallback(const aMessageSeverity:TVkDebugUtilsMessageSeverityFlagsEXT;
                                            const aMessageTypes:TVkDebugUtilsMessageTypeFlagsEXT;
                                            const aCallbackData:PVkDebugUtilsMessengerCallbackDataEXT;
                                            const aUserData:pointer):TVkBool32;
     end;

constructor TValidationMessageCollector.Create;
begin
 inherited Create;
 fMessages:=TStringList.Create;
end;

destructor TValidationMessageCollector.Destroy;
begin
 FreeAndNil(fMessages);
 inherited Destroy;
end;

procedure TValidationMessageCollector.Clear;
begin
 fMessages.Clear;
end;

function TValidationMessageCollector.Count:TpvSizeInt;
begin
 result:=fMessages.Count;
end;

function TValidationMessageCollector.Text:TpvUTF8String;
begin
 result:=TpvUTF8String(fMessages.Text);
end;

function TValidationMessageCollector.DebugUtilsMessengerCallback(const aMessageSeverity:TVkDebugUtilsMessageSeverityFlagsEXT;
                                                                  const aMessageTypes:TVkDebugUtilsMessageTypeFlagsEXT;
                                                                  const aCallbackData:PVkDebugUtilsMessengerCallbackDataEXT;
                                                                  const aUserData:pointer):TVkBool32;
begin

 result:=VK_FALSE;

 // Only warnings and errors produced by the validation layer are test failures.
 if assigned(aCallbackData) and
    ((aMessageSeverity and
      (TVkDebugUtilsMessageSeverityFlagsEXT(VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT) or
       TVkDebugUtilsMessageSeverityFlagsEXT(VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT)))<>0) and
    ((aMessageTypes and TVkDebugUtilsMessageTypeFlagsEXT(VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT))<>0) then begin
  fMessages.Add(String(aCallbackData^.pMessage));
 end;

 if assigned(aUserData) then begin
  // Keep the callback signature fully exercised without depending on user data.
 end;

end;

procedure CheckNoValidationMessages(const aCollector:TValidationMessageCollector;
                                    const aTestName:TpvUTF8String);
begin
 if aCollector.Count>0 then begin
  raise EpvVulkanMemoryTestException.Create(String(aTestName+': '+aCollector.Text));
 end;
end;

function HasNonCoherentHostVisibleMemory(const aPhysicalDevice:TpvVulkanPhysicalDevice):boolean;
var Index:TpvSizeInt;
    PropertyFlags:TVkMemoryPropertyFlags;
begin
 result:=false;
 for Index:=0 to aPhysicalDevice.MemoryProperties.memoryTypeCount-1 do begin
  PropertyFlags:=aPhysicalDevice.MemoryProperties.memoryTypes[Index].propertyFlags;
  if ((PropertyFlags and TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT))<>0) and
     ((PropertyFlags and TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))=0) then begin
   result:=true;
   break;
  end;
 end;
end;

function HasBufferDeviceAddressSupport(const aInstance:TpvVulkanInstance;
                                       const aPhysicalDevice:TpvVulkanPhysicalDevice):boolean;
begin
 if (aInstance.APIVersion and VK_API_VERSION_WITHOUT_PATCH_MASK)>=VK_API_VERSION_1_2 then begin
  result:=aPhysicalDevice.Vulkan12Features.bufferDeviceAddress<>VK_FALSE;
 end else begin
  result:=aPhysicalDevice.BufferDeviceAddressFeaturesKHR.bufferDeviceAddress<>VK_FALSE;
 end;
end;

procedure TestNonCoherentMapping(const aDevice:TpvVulkanDevice;
                                 const aCollector:TValidationMessageCollector);
var Buffer:TpvVulkanBuffer;
    MappedMemory:PVkVoid;
    AtomSize,BufferSize,MapOffset,MapSize:TVkDeviceSize;
begin

 if not HasNonCoherentHostVisibleMemory(aDevice.PhysicalDevice) then begin
  VulkanMemoryTestSkipped('Validation noncoherent mapping',
                          'physical device has no host-visible noncoherent memory type');
  exit;
 end;

 AtomSize:=aDevice.PhysicalDevice.Properties.limits.nonCoherentAtomSize;
 if AtomSize<1 then begin
  AtomSize:=1;
 end;

 BufferSize:=(AtomSize*4)+64;
 MapOffset:=AtomSize+3;
 MapSize:=AtomSize+19;

 Buffer:=nil;
 MappedMemory:=nil;
 try

  try
   Buffer:=TpvVulkanBuffer.Create(aDevice,
                                  BufferSize,
                                  TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or
                                  TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT),
                                  VK_SHARING_MODE_EXCLUSIVE,
                                  [],
                                  TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                  TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_CACHED_BIT),
                                  TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                  0,
                                  0,
                                  0,
                                  0,
                                  0,
                                  [TpvVulkanBufferFlag.OwnSingleMemoryChunk],
                                  1,
                                  0,
                                  'Vulkan memory noncoherent mapping validation test');
  except
   on E:EpvVulkanMemoryAllocationException do begin
    VulkanMemoryTestSkipped('Validation noncoherent mapping',
                            'buffer requirements expose no host-visible noncoherent memory type');
    exit;
   end;
  end;

  aCollector.Clear;

  MappedMemory:=Buffer.Memory.MapMemory(MapOffset,MapSize);
  VulkanMemoryTestCheck(assigned(MappedMemory),'Noncoherent mapping returned nil');

  // Use deliberately unaligned subranges; the allocator must clamp and atom-align them.
  FillChar(MappedMemory^,13,$a5);
  Buffer.Memory.FlushMappedMemoryRange(TpvPointer(TpvPtrUInt(MappedMemory)+3),17);
  Buffer.Memory.InvalidateMappedMemoryRange(TpvPointer(TpvPtrUInt(MappedMemory)+5),13);

  Buffer.Memory.UnmapMemory;
  MappedMemory:=nil;

  FreeAndNil(Buffer);

  CheckNoValidationMessages(aCollector,'Noncoherent mapping validation');

 finally
  if assigned(MappedMemory) and assigned(Buffer) then begin
   Buffer.Memory.UnmapMemory;
  end;
  FreeAndNil(Buffer);
 end;

 VulkanMemoryTestPassed('Validation noncoherent mapping');

end;

procedure TestDedicatedBufferDeviceAddress(const aInstance:TpvVulkanInstance;
                                           const aDevice:TpvVulkanDevice;
                                           const aCollector:TValidationMessageCollector);
var Buffer:TpvVulkanBuffer;
begin

 if not HasBufferDeviceAddressSupport(aInstance,aDevice.PhysicalDevice) then begin
  VulkanMemoryTestSkipped('Validation dedicated+BDA',
                          'physical device has no buffer-device-address support');
  exit;
 end;

 Buffer:=nil;
 try

  aCollector.Clear;

  Buffer:=TpvVulkanBuffer.Create(aDevice,
                                 4096,
                                 TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or
                                 TVkBufferUsageFlags(VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_KHR),
                                 VK_SHARING_MODE_EXCLUSIVE,
                                 [],
                                 0,
                                 TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                 0,
                                 0,
                                 0,
                                 0,
                                 0,
                                 0,
                                 [TpvVulkanBufferFlag.OwnSingleMemoryChunk,
                                  TpvVulkanBufferFlag.DedicatedAllocation,
                                  TpvVulkanBufferFlag.BufferDeviceAddress],
                                 1,
                                 0,
                                 'Vulkan memory dedicated+BDA validation test');

  VulkanMemoryTestCheck(TpvVulkanBufferFlag.DedicatedAllocation in Buffer.Flags,
                        'Dedicated buffer flag is missing');
  VulkanMemoryTestCheck(TpvVulkanBufferFlag.BufferDeviceAddress in Buffer.Flags,
                        'Buffer-device-address buffer flag is missing');
  VulkanMemoryTestCheck(TpvVulkanDeviceMemoryChunkFlag.DedicatedAllocation in Buffer.Memory.MemoryChunk.MemoryChunkFlags,
                        'Dedicated memory chunk flag is missing');
  VulkanMemoryTestCheck(TpvVulkanDeviceMemoryChunkFlag.BufferDeviceAddress in Buffer.Memory.MemoryChunk.MemoryChunkFlags,
                        'Buffer-device-address memory chunk flag is missing');

  FreeAndNil(Buffer);

  CheckNoValidationMessages(aCollector,'Dedicated+BDA validation');

 finally
  FreeAndNil(Buffer);
 end;

 VulkanMemoryTestPassed('Validation dedicated+BDA');

end;

procedure RunVulkanMemoryValidationTests;
var VulkanInstance:TpvVulkanInstance;
    VulkanDevice:TpvVulkanDevice;
    Collector:TValidationMessageCollector;
    Reason:TpvUTF8String;
begin

 VulkanInstance:=nil;
 VulkanDevice:=nil;
 Collector:=nil;

 try

  try
   VulkanInstance:=TpvVulkanInstance.Create('PasVulkan memory tests',
                                             1,
                                             'PasVulkan',
                                             1,
                                             0,
                                             true);
  except
   on E:Exception do begin
    if VulkanMemoryTestStrictValidation then begin
     raise;
    end;
    Reason:=TpvUTF8String(E.Message);
    VulkanMemoryTestSkipped('Validation noncoherent mapping',Reason);
    VulkanMemoryTestSkipped('Validation dedicated+BDA',Reason);
    exit;
   end;
  end;

  if VulkanInstance.AvailableLayerNames.IndexOf('VK_LAYER_KHRONOS_validation')<0 then begin
   if VulkanMemoryTestStrictValidation then begin
    VulkanMemoryTestCheck(false,'VK_LAYER_KHRONOS_validation is unavailable');
   end;
   VulkanMemoryTestSkipped('Validation noncoherent mapping',
                           'VK_LAYER_KHRONOS_validation is unavailable');
   VulkanMemoryTestSkipped('Validation dedicated+BDA',
                           'VK_LAYER_KHRONOS_validation is unavailable');
   exit;
  end;

  if VulkanInstance.AvailableExtensionNames.IndexOf(VK_EXT_DEBUG_UTILS_EXTENSION_NAME)<0 then begin
   if VulkanMemoryTestStrictValidation then begin
    VulkanMemoryTestCheck(false,'VK_EXT_debug_utils is unavailable');
   end;
   VulkanMemoryTestSkipped('Validation noncoherent mapping',
                           'VK_EXT_debug_utils is unavailable');
   VulkanMemoryTestSkipped('Validation dedicated+BDA',
                           'VK_EXT_debug_utils is unavailable');
   exit;
  end;

  VulkanInstance.EnabledLayerNames.Add('VK_LAYER_KHRONOS_validation');
  VulkanInstance.EnabledExtensionNames.Add(VK_EXT_DEBUG_UTILS_EXTENSION_NAME);

  try

   // Driver and logical-device initialization is a capability precondition, not a test result.
   VulkanInstance.Initialize;

   Collector:=TValidationMessageCollector.Create;
   VulkanInstance.OnInstanceDebugUtilsMessengerCallback:=Collector.DebugUtilsMessengerCallback;
   VulkanInstance.InstallDebugUtilsMessengerCallback;

   VulkanDevice:=TpvVulkanDevice.Create(VulkanInstance,nil,nil,nil,false);
   VulkanDevice.EnabledLayerNames.Add('VK_LAYER_KHRONOS_validation');

   if ((VulkanInstance.APIVersion and VK_API_VERSION_WITHOUT_PATCH_MASK)<VK_API_VERSION_1_2) and
      (VulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME)>=0) then begin
    VulkanDevice.EnabledExtensionNames.Add(VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME);
   end;

   VulkanDevice.AddQueues(nil,false,false);
   VulkanDevice.Initialize;

  except
   on E:EpvVulkanException do begin
    if VulkanMemoryTestStrictValidation then begin
     raise;
    end;
    Reason:=TpvUTF8String(E.Message);
    VulkanMemoryTestSkipped('Validation noncoherent mapping',Reason);
    VulkanMemoryTestSkipped('Validation dedicated+BDA',Reason);
    exit;
   end;
  end;

  Collector.Clear;

  TestNonCoherentMapping(VulkanDevice,Collector);
  TestDedicatedBufferDeviceAddress(VulkanInstance,VulkanDevice,Collector);

  VulkanDevice.WaitIdle;

 finally
  FreeAndNil(VulkanDevice);
  FreeAndNil(VulkanInstance);
  FreeAndNil(Collector);
 end;

end;

end.
