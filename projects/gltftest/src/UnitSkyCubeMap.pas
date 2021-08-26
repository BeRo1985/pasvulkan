unit UnitSkyCubeMap;
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
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application;

type { TSkyCubeMap }
     TSkyCubeMap=class
      public
       const Width=1024;
             Height=1024;
      private
       fVertexShaderModule:TpvVulkanShaderModule;
       fFragmentShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
       fVulkanImage:TpvVulkanImage;
       fVulkanSampler:TpvVulkanSampler;
       fVulkanImageView:TpvVulkanImageView;
       fMemoryBlock:TpvVulkanDeviceMemoryBlock;
      public

       constructor Create;

       destructor Destroy; override;

      published

       property VulkanImage:TpvVulkanImage read fVulkanImage;

       property VulkanSampler:TpvVulkanSampler read fVulkanSampler;

       property VulkanImageView:TpvVulkanImageView read fVulkanImageView;

     end;

implementation

{ TSkyCubeMap }

constructor TSkyCubeMap.Create;
var Index:TpvSizeInt;
    Stream:TStream;
    MemoryRequirements:TVkMemoryRequirements;
    RequiresDedicatedAllocation,
    PrefersDedicatedAllocation:boolean;
    MemoryBlockFlags:TpvVulkanDeviceMemoryBlockFlags;
    ImageSubresourceRange:TVkImageSubresourceRange;
    Queue:TpvVulkanQueue;
    CommandPool:TpvVulkanCommandPool;
    CommandBuffer:TpvVulkanCommandBuffer;
    Fence:TpvVulkanFence;
    ImageView:TpvVulkanImageView;
    FrameBuffer:TpvVulkanFrameBuffer;
    RenderPass:TpvVulkanRenderPass;
    FrameBufferColorAttachment:TpvVulkanFrameBufferAttachment;
begin
 inherited Create;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/cubemap_vert.spv');
 try
  fVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/cubemap_sky_frag.spv');
 try
  fFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fFragmentShaderModule,'main');

 fVulkanImage:=TpvVulkanImage.Create(pvApplication.VulkanDevice,
                                     TVkImageCreateFlags(VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT),
                                     VK_IMAGE_TYPE_2D,
                                     VK_FORMAT_R16G16B16A16_SFLOAT,
                                     Width,
                                     Height,
                                     1,
                                     1,
                                     6,
                                     VK_SAMPLE_COUNT_1_BIT,
                                     VK_IMAGE_TILING_OPTIMAL,
                                     TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
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
    ImageSubresourceRange.levelCount:=1;
    ImageSubresourceRange.baseArrayLayer:=0;
    ImageSubresourceRange.layerCount:=6;
    fVulkanImage.SetLayout(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                           TVkImageLayout(VK_IMAGE_LAYOUT_UNDEFINED),
                           TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL),
                           @ImageSubresourceRange,
                           CommandBuffer,
                           Queue,
                           Fence,
                           true);

    fVulkanSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                            TVkFilter(VK_FILTER_LINEAR),
                                            TVkFilter(VK_FILTER_LINEAR),
                                            TVkSamplerMipmapMode(VK_SAMPLER_MIPMAP_MODE_LINEAR),
                                            TVkSamplerAddressMode(VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER),
                                            TVkSamplerAddressMode(VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER),
                                            TVkSamplerAddressMode(VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER),
                                            0.0,
                                            false,
                                            1.0,
                                            false,
                                            TVkCompareOp(VK_COMPARE_OP_NEVER),
                                            0.0,
                                            1.0,
                                            TVkBorderColor(VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK),
                                            false);

    fVulkanImageView:=TpvVulkanImageView.Create(pvApplication.VulkanDevice,
                                                fVulkanImage,
                                                TVkImageViewType(VK_IMAGE_VIEW_TYPE_CUBE),
                                                TVkFormat(VK_FORMAT_R16G16B16A16_SFLOAT),
                                                TVkComponentSwizzle(VK_COMPONENT_SWIZZLE_IDENTITY),
                                                TVkComponentSwizzle(VK_COMPONENT_SWIZZLE_IDENTITY),
                                                TVkComponentSwizzle(VK_COMPONENT_SWIZZLE_IDENTITY),
                                                TVkComponentSwizzle(VK_COMPONENT_SWIZZLE_IDENTITY),
                                                TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                0,
                                                1,
                                                0,
                                                6);

    ImageView:=TpvVulkanImageView.Create(pvApplication.VulkanDevice,
                                         fVulkanImage,
                                         TVkImageViewType(VK_IMAGE_VIEW_TYPE_CUBE),
                                         TVkFormat(VK_FORMAT_R16G16B16A16_SFLOAT),
                                         TVkComponentSwizzle(VK_COMPONENT_SWIZZLE_IDENTITY),
                                         TVkComponentSwizzle(VK_COMPONENT_SWIZZLE_IDENTITY),
                                         TVkComponentSwizzle(VK_COMPONENT_SWIZZLE_IDENTITY),
                                         TVkComponentSwizzle(VK_COMPONENT_SWIZZLE_IDENTITY),
                                         TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                         0,
                                         1,
                                         0,
                                         6);
    try

     RenderPass:=TpvVulkanRenderPass.Create(pvApplication.VulkanDevice);
     try

      RenderPass.AddSubpassDescription(0,
                                       VK_PIPELINE_BIND_POINT_GRAPHICS,
                                       [],
                                       [RenderPass.AddAttachmentReference(RenderPass.AddAttachmentDescription(0,
                                                                                                              TVkFormat(VK_FORMAT_R16G16B16A16_SFLOAT),
                                                                                                              VK_SAMPLE_COUNT_1_BIT,
                                                                                                              VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                                                                              VK_ATTACHMENT_STORE_OP_STORE,
                                                                                                              VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                                                              VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                                                                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                                                                                                             ),
                                                                           VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                                                                          )],
                                       [],
                                       VK_NULL_HANDLE,
                                       []
                                      );
      RenderPass.Initialize;

      RenderPass.ClearValues[0].color.float32[0]:=0.0;
      RenderPass.ClearValues[0].color.float32[1]:=0.0;
      RenderPass.ClearValues[0].color.float32[2]:=0.0;
      RenderPass.ClearValues[0].color.float32[3]:=0.0;

      FrameBufferColorAttachment:=TpvVulkanFrameBufferAttachment.Create(pvApplication.VulkanDevice,
                                                                        fVulkanImage,
                                                                        ImageView,
                                                                        Width,
                                                                        Height,
                                                                        TVkFormat(VK_FORMAT_R16G16B16A16_SFLOAT),
                                                                        false);
      try

       FrameBuffer:=TpvVulkanFrameBuffer.Create(pvApplication.VulkanDevice,
                                                RenderPass,
                                                Width,
                                                Height,
                                                1,
                                                [FrameBufferColorAttachment],
                                                false);
       try



       finally
        FreeAndNil(FrameBuffer);
       end;

      finally
       FreeAndNil(FrameBufferColorAttachment);
      end;

     finally
      FreeAndNil(RenderPass);
     end;

    finally
     FreeAndNil(ImageView);
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

destructor TSkyCubeMap.Destroy;
begin
 FreeAndNil(fMemoryBlock);
 FreeAndNil(fVulkanImageView);
 FreeAndNil(fVulkanSampler);
 FreeAndNil(fVulkanImage);
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVertexShaderModule);
 FreeAndNil(fFragmentShaderModule);
 inherited Destroy;
end;

end.
