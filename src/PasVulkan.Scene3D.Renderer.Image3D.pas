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
unit PasVulkan.Scene3D.Renderer.Image3D;
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

type { TpvScene3DRendererImage3D }
     TpvScene3DRendererImage3D=class
      private
       fVulkanImage:TpvVulkanImage;
       fVulkanImageView:TpvVulkanImageView;
       fMemoryBlock:TpvVulkanDeviceMemoryBlock;
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fDepth:TpvInt32;
       fFormat:TVkFormat;
      public

       constructor Create(const aDevice:TpvVulkanDevice;const aWidth,aHeight,aDepth:TpvInt32;const aFormat:TVkFormat;const aSampleBits:TVkSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT);const aImageLayout:TVkImageLayout=TVkImageLayout(VK_IMAGE_LAYOUT_GENERAL);const aAllocationGroupID:TpvUInt64=0);

       destructor Destroy; override;

      published

       property VulkanImage:TpvVulkanImage read fVulkanImage;

       property VulkanImageView:TpvVulkanImageView read fVulkanImageView;

       property Width:TpvInt32 read fWidth;

       property Height:TpvInt32 read fHeight;

       property Depth:TpvInt32 read fDepth;

       property Format:TVkFormat read fFormat;

     end;

implementation

{ TpvScene3DRendererImage3D }

constructor TpvScene3DRendererImage3D.Create(const aDevice:TpvVulkanDevice;const aWidth,aHeight,aDepth:TpvInt32;const aFormat:TVkFormat;const aSampleBits:TVkSampleCountFlagBits;const aImageLayout:TVkImageLayout;const aAllocationGroupID:TpvUInt64);
var MemoryRequirements:TVkMemoryRequirements;
    RequiresDedicatedAllocation,
    PrefersDedicatedAllocation:boolean;
    MemoryBlockFlags:TpvVulkanDeviceMemoryBlockFlags;
    ImageSubresourceRange:TVkImageSubresourceRange;
    Queue:TpvVulkanQueue;
    CommandPool:TpvVulkanCommandPool;
    CommandBuffer:TpvVulkanCommandBuffer;
    Fence:TpvVulkanFence;
    ImageViewType:TVkImageViewType;
    ImageAspectMask:TVkImageAspectFlags;
begin
 inherited Create;

 fWidth:=aWidth;

 fHeight:=aHeight;

 fDepth:=aDepth;

 fFormat:=aFormat;

 case aFormat of
  VK_FORMAT_D16_UNORM,
  VK_FORMAT_D16_UNORM_S8_UINT,
  VK_FORMAT_D24_UNORM_S8_UINT,
  VK_FORMAT_D32_SFLOAT,
  VK_FORMAT_D32_SFLOAT_S8_UINT:begin
   ImageAspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_DEPTH_BIT);
  end;
  else begin
   ImageAspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
  end;
 end;

 ImageViewType:=TVkImageViewType(VK_IMAGE_VIEW_TYPE_3D);

 fVulkanImage:=TpvVulkanImage.Create(aDevice,
                                     0, //TVkImageCreateFlags(VK_IMAGE_CREATE_3D_ARRAY_COMPATIBLE_BIT),
                                     VK_IMAGE_TYPE_3D,
                                     aFormat,
                                     aWidth,
                                     aHeight,
                                     aDepth,
                                     1,
                                     1,
                                     aSampleBits,
                                     VK_IMAGE_TILING_OPTIMAL,
                                     TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT) or
                                     TVkImageUsageFlags(VK_IMAGE_USAGE_STORAGE_BIT) or
                                     TVkImageUsageFlags(VK_IMAGE_USAGE_TRANSFER_DST_BIT),
                                     VK_SHARING_MODE_EXCLUSIVE,
                                     0,
                                     nil,
                                     VK_IMAGE_LAYOUT_UNDEFINED
                                    );

 MemoryRequirements:=aDevice.MemoryManager.GetImageMemoryRequirements(fVulkanImage.Handle,
                                                                      RequiresDedicatedAllocation,
                                                                      PrefersDedicatedAllocation);

 MemoryBlockFlags:=[];

 if RequiresDedicatedAllocation or PrefersDedicatedAllocation then begin
  Include(MemoryBlockFlags,TpvVulkanDeviceMemoryBlockFlag.DedicatedAllocation);
 end;

 fMemoryBlock:=aDevice.MemoryManager.AllocateMemoryBlock(MemoryBlockFlags,
                                                         MemoryRequirements.size,
                                                         MemoryRequirements.alignment,
                                                         MemoryRequirements.memoryTypeBits,
                                                         TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                         0,
                                                         0,
                                                         0,
                                                         0,
                                                         0,
                                                         0,
                                                         0,
                                                         TpvVulkanDeviceMemoryAllocationType.ImageOptimal,
                                                         @fVulkanImage.Handle,
                                                         aAllocationGroupID);
 if not assigned(fMemoryBlock) then begin
  raise EpvVulkanMemoryAllocationException.Create('Memory for texture couldn''t be allocated!');
 end;

 fMemoryBlock.AssociatedObject:=self;

 VulkanCheckResult(aDevice.Commands.BindImageMemory(aDevice.Handle,
                                                                       fVulkanImage.Handle,
                                                                       fMemoryBlock.MemoryChunk.Handle,
                                                                       fMemoryBlock.Offset));

 Queue:=aDevice.GraphicsQueue;

 CommandPool:=TpvVulkanCommandPool.Create(aDevice,
                                          aDevice.GraphicsQueueFamilyIndex,
                                          TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
 try

  CommandBuffer:=TpvVulkanCommandBuffer.Create(CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  try

   Fence:=TpvVulkanFence.Create(aDevice);
   try

    FillChar(ImageSubresourceRange,SizeOf(TVkImageSubresourceRange),#0);
    ImageSubresourceRange.aspectMask:=ImageAspectMask;
    ImageSubresourceRange.baseMipLevel:=0;
    ImageSubresourceRange.levelCount:=1;
    ImageSubresourceRange.baseArrayLayer:=0;
    ImageSubresourceRange.layerCount:=1;
    fVulkanImage.SetLayout(ImageAspectMask,
                           TVkImageLayout(VK_IMAGE_LAYOUT_UNDEFINED),
                           aImageLayout,
                           @ImageSubresourceRange,
                           CommandBuffer,
                           Queue,
                           Fence,
                           true);

    fVulkanImageView:=TpvVulkanImageView.Create(aDevice,
                                                fVulkanImage,
                                                ImageViewType,
                                                aFormat,
                                                TVkComponentSwizzle(VK_COMPONENT_SWIZZLE_IDENTITY),
                                                TVkComponentSwizzle(VK_COMPONENT_SWIZZLE_IDENTITY),
                                                TVkComponentSwizzle(VK_COMPONENT_SWIZZLE_IDENTITY),
                                                TVkComponentSwizzle(VK_COMPONENT_SWIZZLE_IDENTITY),
                                                ImageAspectMask,
                                                0,
                                                1,
                                                0,
                                                1);

   finally
    FreeAndNil(Fence);
   end;

  finally
   FreeAndNil(CommandBuffer);
  end;

 finally
  FreeAndNil(CommandPool);
 end;

end;

destructor TpvScene3DRendererImage3D.Destroy;
begin
 FreeAndNil(fVulkanImageView);
 FreeAndNil(fVulkanImage);
 FreeAndNil(fMemoryBlock);
 inherited Destroy;
end;

end.
