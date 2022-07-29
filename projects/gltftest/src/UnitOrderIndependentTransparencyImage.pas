unit UnitOrderIndependentTransparencyImage;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$if defined(Win32) or defined(Win64)}
 {$define Windows}
{$ifend}

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application;

type { TOrderIndependentTransparencyImage }
     TOrderIndependentTransparencyImage=class
      private
       fVulkanImage:TpvVulkanImage;
       fVulkanSampler:TpvVulkanSampler;
       fVulkanImageView:TpvVulkanImageView;
       fMemoryBlock:TpvVulkanDeviceMemoryBlock;
       fDescriptorImageInfo:TVkDescriptorImageInfo;
      public

       constructor Create(const aWidth,aHeight,aLayers:TpvInt32;const aFormat:TVkFormat;const aSampleBits:TVkSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT));

       destructor Destroy; override;

      published

       property VulkanImage:TpvVulkanImage read fVulkanImage;

       property VulkanSampler:TpvVulkanSampler read fVulkanSampler;

       property VulkanImageView:TpvVulkanImageView read fVulkanImageView;

      public

       property DescriptorImageInfo:TVkDescriptorImageInfo read fDescriptorImageInfo;

     end;

implementation

{ TOrderIndependentTransparencyImage }

constructor TOrderIndependentTransparencyImage.Create(const aWidth,aHeight,aLayers:TpvInt32;const aFormat:TVkFormat;const aSampleBits:TVkSampleCountFlagBits);
var MemoryRequirements:TVkMemoryRequirements;
    RequiresDedicatedAllocation,
    PrefersDedicatedAllocation:boolean;
    MemoryBlockFlags:TpvVulkanDeviceMemoryBlockFlags;
    ImageSubresourceRange:TVkImageSubresourceRange;
    Queue:TpvVulkanQueue;
    CommandPool:TpvVulkanCommandPool;
    CommandBuffer:TpvVulkanCommandBuffer;
    Fence:TpvVulkanFence;
begin
 inherited Create;

 fVulkanImage:=TpvVulkanImage.Create(pvApplication.VulkanDevice,
                                     0, //TVkImageCreateFlags(VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT),
                                     VK_IMAGE_TYPE_2D,
                                     aFormat,
                                     aWidth,
                                     aHeight,
                                     1,
                                     1,
                                     aLayers,
                                     aSampleBits,
                                     VK_IMAGE_TILING_OPTIMAL,
                                     TVkImageUsageFlags(VK_IMAGE_USAGE_STORAGE_BIT) or
                                     TVkImageUsageFlags(VK_IMAGE_USAGE_TRANSFER_DST_BIT),
                                     VK_SHARING_MODE_EXCLUSIVE,
                                     0,
                                     nil,
                                     VK_IMAGE_LAYOUT_UNDEFINED
                                    );

 MemoryRequirements:=pvApplication.VulkanDevice.MemoryManager.GetImageMemoryRequirements(fVulkanImage.Handle,
                                                                                         RequiresDedicatedAllocation,
                                                                                         PrefersDedicatedAllocation);

 MemoryBlockFlags:=[];

 if RequiresDedicatedAllocation or PrefersDedicatedAllocation then begin
  Include(MemoryBlockFlags,TpvVulkanDeviceMemoryBlockFlag.DedicatedAllocation);
 end;

 fMemoryBlock:=pvApplication.VulkanDevice.MemoryManager.AllocateMemoryBlock(MemoryBlockFlags,
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
                                                                            @fVulkanImage.Handle);
 if not assigned(fMemoryBlock) then begin
  raise EpvVulkanMemoryAllocationException.Create('Memory for texture couldn''t be allocated!');
 end;

 fMemoryBlock.AssociatedObject:=self;

 VulkanCheckResult(pvApplication.VulkanDevice.Commands.BindImageMemory(pvApplication.VulkanDevice.Handle,
                                                                       fVulkanImage.Handle,
                                                                       fMemoryBlock.MemoryChunk.Handle,
                                                                       fMemoryBlock.Offset));

 Queue:=pvApplication.VulkanDevice.GraphicsQueue;

 CommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                          pvApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                          TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
 try

  CommandBuffer:=TpvVulkanCommandBuffer.Create(CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  try

   Fence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);
   try

    FillChar(ImageSubresourceRange,SizeOf(TVkImageSubresourceRange),#0);
    ImageSubresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
    ImageSubresourceRange.baseMipLevel:=0;
    ImageSubresourceRange.levelCount:=1;
    ImageSubresourceRange.baseArrayLayer:=0;
    ImageSubresourceRange.layerCount:=aLayers;
    fVulkanImage.SetLayout(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                           TVkImageLayout(VK_IMAGE_LAYOUT_UNDEFINED),
                           TVkImageLayout(VK_IMAGE_LAYOUT_GENERAL),
                           @ImageSubresourceRange,
                           CommandBuffer,
                           Queue,
                           Fence,
                           true);

    fVulkanSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                            TVkFilter(VK_FILTER_LINEAR),
                                            TVkFilter(VK_FILTER_LINEAR),
                                            TVkSamplerMipmapMode(VK_SAMPLER_MIPMAP_MODE_LINEAR),
                                            TVkSamplerAddressMode(VK_SAMPLER_ADDRESS_MODE_REPEAT),
                                            TVkSamplerAddressMode(VK_SAMPLER_ADDRESS_MODE_REPEAT),
                                            TVkSamplerAddressMode(VK_SAMPLER_ADDRESS_MODE_REPEAT),
                                            0.0,
                                            false,
                                            1.0,
                                            false,
                                            TVkCompareOp(VK_COMPARE_OP_NEVER),
                                            0.0,
                                            1,
                                            TVkBorderColor(VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK),
                                            false);

    fVulkanImageView:=TpvVulkanImageView.Create(pvApplication.VulkanDevice,
                                                fVulkanImage,
                                                TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY),
                                                aFormat,
                                                TVkComponentSwizzle(VK_COMPONENT_SWIZZLE_IDENTITY),
                                                TVkComponentSwizzle(VK_COMPONENT_SWIZZLE_IDENTITY),
                                                TVkComponentSwizzle(VK_COMPONENT_SWIZZLE_IDENTITY),
                                                TVkComponentSwizzle(VK_COMPONENT_SWIZZLE_IDENTITY),
                                                TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                0,
                                                1,
                                                0,
                                                aLayers);

    fDescriptorImageInfo:=TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                        fVulkanImageView.Handle,
                                                        VK_IMAGE_LAYOUT_GENERAL);


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

destructor TOrderIndependentTransparencyImage.Destroy;
begin
 FreeAndNil(fMemoryBlock);
 FreeAndNil(fVulkanImageView);
 FreeAndNil(fVulkanSampler);
 FreeAndNil(fVulkanImage);
 inherited Destroy;
end;

end.
