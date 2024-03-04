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
unit PasVulkan.Scene3D.Renderer.OrderIndependentTransparencyBuffer;
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

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application;

type { TpvScene3DRendererOrderIndependentTransparencyBuffer }
     TpvScene3DRendererOrderIndependentTransparencyBuffer=class
      private
       fVulkanBuffer:TpvVulkanBuffer;
       fVulkanBufferView:TpvVulkanBufferView;
      public

       constructor Create(const aDevice:TpvVulkanDevice;const aSize:TpvInt32;const aFormat:TVkFormat;const aBufferUsage:TVkBufferUsageFlags=TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT));

       destructor Destroy; override;

      published

       property VulkanBuffer:TpvVulkanBuffer read fVulkanBuffer;

       property VulkanBufferView:TpvVulkanBufferView read fVulkanBufferView;

      end;

implementation

{ TpvScene3DRendererOrderIndependentTransparencyBuffer }

constructor TpvScene3DRendererOrderIndependentTransparencyBuffer.Create(const aDevice:TpvVulkanDevice;const aSize:TpvInt32;const aFormat:TVkFormat;const aBufferUsage:TVkBufferUsageFlags);
begin

 inherited Create;

 fVulkanBuffer:=TpvVulkanBuffer.Create(aDevice,
                                       aSize,
                                       aBufferUsage,
                                       TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                       [],
                                       TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                       0,
                                       0,
                                       0,
                                       0,
                                       0,
                                       0,
                                       0,
                                       [],
                                       0,
                                       pvAllocationGroupIDScene3DSurface);

 aDevice.DebugUtils.SetObjectName(fVulkanBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.OITBuffer');

 if (aBufferUsage and (TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT) or
                       TVkBufferUsageFlags(VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT)))<>0 then begin
  fVulkanBufferView:=TpvVulkanBufferView.Create(aDevice,
                                                fVulkanBuffer,aFormat,
                                                0,
                                                TVkDeviceSize(VK_WHOLE_SIZE));
 end else begin
  fVulkanBufferView:=nil;
 end;

end;

destructor TpvScene3DRendererOrderIndependentTransparencyBuffer.Destroy;
begin
 FreeAndNil(fVulkanBufferView);
 FreeAndNil(fVulkanBuffer);
 inherited Destroy;
end;

end.
