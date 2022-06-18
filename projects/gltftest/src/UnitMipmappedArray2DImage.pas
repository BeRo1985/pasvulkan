unit UnitMipmappedArray2DImage;
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

type { TMipmappedArray2DImage }
     TMipmappedArray2DImage=class
      private
       fVulkanImage:TpvVulkanImage;
       fVulkanSampler:TpvVulkanSampler;
       fVulkanImageView:TpvVulkanImageView;
       fMemoryBlock:TpvVulkanDeviceMemoryBlock;
       fDescriptorImageInfo:TVkDescriptorImageInfo;
       fMipMapLevels:TpvInt32;
      public

       VulkanImageViews:array of array of TpvVulkanImageView;

       DescriptorImageInfos:array of array of TVkDescriptorImageInfo;

       constructor Create(const aWidth,aHeight,aLayers:TpvInt32;const aFormat:TVkFormat;const aSampleBits:TVkSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT);const aImageLayout:TVkImageLayout=TVkImageLayout(VK_IMAGE_LAYOUT_GENERAL));

       destructor Destroy; override;

      published

       property VulkanImage:TpvVulkanImage read fVulkanImage;

       property VulkanSampler:TpvVulkanSampler read fVulkanSampler;

       property VulkanImageView:TpvVulkanImageView read fVulkanImageView;

      public

       property DescriptorImageInfo:TVkDescriptorImageInfo read fDescriptorImageInfo;

       property MipMapLevels:TpvInt32 read fMipMapLevels;

     end;

implementation

{ TMipmappedArray2DImage }

constructor TMipmappedArray2DImage.Create(const aWidth,aHeight,aLayers:TpvInt32;const aFormat:TVkFormat;const aSampleBits:TVkSampleCountFlagBits;const aImageLayout:TVkImageLayout);
var LayerIndex,MipMapLevelIndex:TpvInt32;
    MemoryRequirements:TVkMemoryRequirements;
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

 VulkanImageViews:=nil;

 DescriptorImageInfos:=nil;

 fMipMapLevels:=Max(1,IntLog2(Min(aWidth,aHeight)+1));

 fVulkanImage:=TpvVulkanImage.Create(pvApplication.VulkanDevice,
                                     0, //TVkImageCreateFlags(VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT),
                                     VK_IMAGE_TYPE_2D,
                                     aFormat,
                                     aWidth,
                                     aHeight,
                                     1,
                                     MipMapLevels,
                                     aLayers,
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
    ImageSubresourceRange.levelCount:=fMipMapLevels;
    ImageSubresourceRange.baseArrayLayer:=0;
    ImageSubresourceRange.layerCount:=aLayers;
    fVulkanImage.SetLayout(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                           TVkImageLayout(VK_IMAGE_LAYOUT_UNDEFINED),
                           aImageLayout,
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
                                                fMipMapLevels,
                                                0,
                                                aLayers);

    fDescriptorImageInfo:=TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                        fVulkanImageView.Handle,
                                                        aImageLayout);

    SetLength(VulkanImageViews,aLayers,fMipMapLevels);

    SetLength(DescriptorImageInfos,aLayers,fMipMapLevels);

    for LayerIndex:=0 to length(VulkanImageViews)-1 do begin
     for MipMapLevelIndex:=0 to length(VulkanImageViews[LayerIndex])-1 do begin
      VulkanImageViews[LayerIndex,MipMapLevelIndex]:=TpvVulkanImageView.Create(pvApplication.VulkanDevice,
                                                                               fVulkanImage,
                                                                               TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D),
                                                                               aFormat,
                                                                               TVkComponentSwizzle(VK_COMPONENT_SWIZZLE_IDENTITY),
                                                                               TVkComponentSwizzle(VK_COMPONENT_SWIZZLE_IDENTITY),
                                                                               TVkComponentSwizzle(VK_COMPONENT_SWIZZLE_IDENTITY),
                                                                               TVkComponentSwizzle(VK_COMPONENT_SWIZZLE_IDENTITY),
                                                                               TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                               MipMapLevelIndex,
                                                                               1,
                                                                               LayerIndex,
                                                                               1);
      DescriptorImageInfos[LayerIndex,MipMapLevelIndex]:=TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                       VulkanImageViews[LayerIndex,MipMapLevelIndex].Handle,
                                                                                       aImageLayout);
     end;
    end;


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

destructor TMipmappedArray2DImage.Destroy;
var LayerIndex,MipMapLevelIndex:TpvInt32;
begin
 FreeAndNil(fMemoryBlock);
 for LayerIndex:=0 to length(VulkanImageViews)-1 do begin
  for MipMapLevelIndex:=0 to length(VulkanImageViews[LayerIndex])-1 do begin
   FreeAndNil(VulkanImageViews[LayerIndex,MipMapLevelIndex]);
  end;
 end;
 VulkanImageViews:=nil;
 DescriptorImageInfos:=nil;
 FreeAndNil(fVulkanImageView);
 FreeAndNil(fVulkanSampler);
 FreeAndNil(fVulkanImage);
 inherited Destroy;
end;

end.
