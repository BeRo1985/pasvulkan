(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2020, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.Scene3D.Renderer;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

interface

uses Classes,
     SysUtils,
     PasMP,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Resources,
     PasVulkan.FrameGraph,
     PasVulkan.TimerQuery,
     PasVulkan.Collections,
     PasVulkan.CircularDoublyLinkedList,
     PasVulkan.Scene3D,
     PasVulkan.Scene3D.Renderer.SMAAData,
     PasVulkan.Scene3D.Renderer.SkyCubeMap,
     PasVulkan.Scene3D.Renderer.SkyBox,
     PasVulkan.Scene3D.Renderer.OrderIndependentTransparencyBuffer,
     PasVulkan.Scene3D.Renderer.OrderIndependentTransparencyImage,
     PasVulkan.Scene3D.Renderer.MipmappedArray2DImage,
     PasVulkan.Scene3D.Renderer.Lambertian.EnvMapCubeMap,
     PasVulkan.Scene3D.Renderer.Charlie.BRDF,
     PasVulkan.Scene3D.Renderer.Charlie.EnvMapCubeMap,
     PasVulkan.Scene3D.Renderer.GGX.BRDF,
     PasVulkan.Scene3D.Renderer.GGX.EnvMapCubeMap;

type TpvScene3DRenderer=class;

     TpvScene3DRendererBaseObject=class;

     TpvScene3DRendererBaseObjects=class(TpvObjectGenericList<TpvScene3DRendererBaseObject>);

     TpvScene3DRendererBaseObjectCircularDoublyLinkedListNode=class(TpvCircularDoublyLinkedListNode<TpvScene3DRendererBaseObject>);

     { TpvScene3DRendererBaseObject }
     TpvScene3DRendererBaseObject=class
      private
       fParent:TpvScene3DRendererBaseObject;
       fRenderer:TpvScene3DRenderer;
       fChildrenLock:TPasMPCriticalSection;
       fChildren:TpvScene3DRendererBaseObjectCircularDoublyLinkedListNode;
       fOwnCircularDoublyLinkedListNode:TpvScene3DRendererBaseObjectCircularDoublyLinkedListNode;
      public
       constructor Create(const aParent:TpvScene3DRendererBaseObject); reintroduce;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
      published
       property Parent:TpvScene3DRendererBaseObject read fParent;
       property Renderer:TpvScene3DRenderer read fRenderer;
     end;

     { TpvScene3DRenderer }
     TpvScene3DRenderer=class(TpvScene3DRendererBaseObject)
      private
       fScene3D:TpvScene3D;
       fVulkanDevice:TpvVulkanDevice;
       fCountInFlightFrames:TpvSizeInt;
      public
       constructor Create(const aScene3D:TpvScene3D;const aVulkanDevice:TpvVulkanDevice=nil;const aCountInFlightFrames:TpvSizeInt=MaxInFlightFrames); reintroduce;
       destructor Destroy; override;
       class procedure SetupVulkanDevice(const aVulkanDevice:TpvVulkanDevice); static;
      published
       property Scene3D:TpvScene3D read fScene3D;
       property VulkanDevice:TpvVulkanDevice read fVulkanDevice;
       property CountInFlightFrames:TpvSizeInt read fCountInFlightFrames;
     end;


implementation

uses PasVulkan.Scene3D.Renderer.Instance;

{ TpvScene3DRendererBaseObject }

constructor TpvScene3DRendererBaseObject.Create(const aParent:TpvScene3DRendererBaseObject);
begin
 inherited Create;

 fParent:=aParent;
 if assigned(fParent) then begin
  if fParent is TpvScene3DRenderer then begin
   fRenderer:=TpvScene3DRenderer(fParent);
  end else begin
   fRenderer:=fParent.fRenderer;
  end;
 end else begin
  fRenderer:=nil;
 end;

 fOwnCircularDoublyLinkedListNode:=TpvScene3DRendererBaseObjectCircularDoublyLinkedListNode.Create;
 fOwnCircularDoublyLinkedListNode.Value:=self;

 fChildrenLock:=TPasMPCriticalSection.Create;
 fChildren:=TpvScene3DRendererBaseObjectCircularDoublyLinkedListNode.Create;

end;

destructor TpvScene3DRendererBaseObject.Destroy;
var Child:TpvScene3DRendererBaseObject;
begin
 fChildrenLock.Acquire;
 try
  while fChildren.PopFromBack(Child) do begin
   FreeAndNil(Child);
  end;
 finally
  fChildrenLock.Release;
 end;
 FreeAndNil(fChildren);
 FreeAndNil(fChildrenLock);
 FreeAndNil(fOwnCircularDoublyLinkedListNode);
 inherited Destroy;
end;

procedure TpvScene3DRendererBaseObject.AfterConstruction;
begin
 inherited AfterConstruction;
 if assigned(fParent) then begin
  fParent.fChildrenLock.Acquire;
  try
   fParent.fChildren.Add(fOwnCircularDoublyLinkedListNode);
  finally
   fParent.fChildrenLock.Release;
  end;
 end;
end;

procedure TpvScene3DRendererBaseObject.BeforeDestruction;
begin
 if assigned(fParent) and not fOwnCircularDoublyLinkedListNode.IsEmpty then begin
  try
   fParent.fChildrenLock.Acquire;
   try
    if not fOwnCircularDoublyLinkedListNode.IsEmpty then begin
     fOwnCircularDoublyLinkedListNode.Remove;
    end;
   finally
    fParent.fChildrenLock.Release;
   end;
  finally
   fParent:=nil;
  end;
 end;
 inherited BeforeDestruction;
end;

{ TpvScene3DRenderer }

constructor TpvScene3DRenderer.Create(const aScene3D:TpvScene3D;const aVulkanDevice:TpvVulkanDevice;const aCountInFlightFrames:TpvSizeInt);
begin
 inherited Create(nil);

 fScene3D:=aScene3D;

 if assigned(aVulkanDevice) then begin
  fVulkanDevice:=aVulkanDevice;
 end else begin
  fVulkanDevice:=pvApplication.VulkanDevice;
 end;

 fCountInFlightFrames:=aCountInFlightFrames;

end;

destructor TpvScene3DRenderer.Destroy;
begin
 inherited Destroy;
end;

class procedure TpvScene3DRenderer.SetupVulkanDevice(const aVulkanDevice:TpvVulkanDevice);
begin
 if (aVulkanDevice.PhysicalDevice.DescriptorIndexingFeaturesEXT.descriptorBindingPartiallyBound=VK_FALSE) or
    (aVulkanDevice.PhysicalDevice.DescriptorIndexingFeaturesEXT.runtimeDescriptorArray=VK_FALSE) or
    (aVulkanDevice.PhysicalDevice.DescriptorIndexingFeaturesEXT.shaderSampledImageArrayNonUniformIndexing=VK_FALSE) then begin
  raise EpvApplication.Create('Application','Support for VK_EXT_DESCRIPTOR_INDEXING (descriptorBindingPartiallyBound + runtimeDescriptorArray + shaderSampledImageArrayNonUniformIndexing) is needed',LOG_ERROR);
 end;
{if aVulkanDevice.PhysicalDevice.BufferDeviceAddressFeaturesKHR.bufferDeviceAddress=VK_FALSE then begin
  raise EpvApplication.Create('Application','Support for VK_KHR_buffer_device_address (bufferDeviceAddress) is needed',LOG_ERROR);
 end;}
 if aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME)>=0 then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME);
 end;
 if aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_KHR_MAINTENANCE1_EXTENSION_NAME)>=0 then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_KHR_MAINTENANCE1_EXTENSION_NAME);
 end;
 if aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_KHR_MAINTENANCE2_EXTENSION_NAME)>=0 then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_KHR_MAINTENANCE2_EXTENSION_NAME);
 end;
 if aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_KHR_MAINTENANCE3_EXTENSION_NAME)>=0 then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_KHR_MAINTENANCE3_EXTENSION_NAME);
 end;
 if aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME)>=0 then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME);
 end;
 if aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME)>=0 then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME);
 end;
 if aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME)>=0 then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME);
 end;
 if aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME)>=0 then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME);
 end;
 if aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME)>=0 then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME);
 end;
 if aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME)>=0 then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME);
 end;
 if ((aVulkanDevice.Instance.APIVersion and VK_API_VERSION_WITHOUT_PATCH_MASK)<VK_API_VERSION_1_2) and
    (aVulkanDevice.PhysicalDevice.AvailableExtensionNames.IndexOf(VK_KHR_SPIRV_1_4_EXTENSION_NAME)>=0) then begin
  aVulkanDevice.EnabledExtensionNames.Add(VK_KHR_SPIRV_1_4_EXTENSION_NAME);
 end;
end;

end.

