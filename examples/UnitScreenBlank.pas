unit UnitScreenBlank;
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

uses SysUtils,Classes,Vulkan,PasVulkan,PasVulkanAndroid,PasVulkanApplication;

type TScreenBlank=class(TVulkanApplicationScreen)
      private
       fTriangleVertexShaderModule:TVulkanShaderModule;
       fTriangleFragmentShaderModule:TVulkanShaderModule;
       fVulkanPipelineShaderStageTriangleVertex:TVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageTriangleFragment:TVulkanPipelineShaderStage;
       fVulkanRenderPass:TVulkanRenderPass;
       fVulkanCommandPool:TVulkanCommandPool;
       fVulkanRenderCommandBuffers:array[0..MaxSwapChainImages-1] of TVulkanCommandBuffer;
       fVulkanRenderSemaphores:array[0..MaxSwapChainImages-1] of TVulkanSemaphore;
      public

       constructor Create; override;

       destructor Destroy; override;

       procedure Show; override;

       procedure Hide; override;

       procedure Resume; override;

       procedure Pause; override;

       procedure Resize(const aWidth,aHeight:TVkInt32); override;

       procedure AfterCreateSwapChain; override;

       procedure BeforeDestroySwapChain; override;

       procedure Update(const aDeltaTime:double); override;

       procedure Draw(const aSwapChainImageIndex:TVkInt32;var aWaitSemaphore:TVulkanSemaphore;const aWaitFence:TVulkanFence=nil); override;

     end;

implementation

constructor TScreenBlank.Create;
begin
 inherited Create;
end;

destructor TScreenBlank.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenBlank.Show;
var Stream:TStream;
    Index:TVkInt32;
begin
 inherited Show;

 fVulkanCommandPool:=TVulkanCommandPool.Create(VulkanApplication.VulkanDevice,
                                               VulkanApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                               TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
 for Index:=0 to MaxSwapChainImages-1 do begin
  fVulkanRenderCommandBuffers[Index]:=TVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  fVulkanRenderSemaphores[Index]:=TVulkanSemaphore.Create(VulkanApplication.VulkanDevice);
 end;

 Stream:=VulkanApplication.Assets.GetAssetStream('shaders/triangle/triangle_vert.spv');
 try
  fTriangleVertexShaderModule:=TVulkanShaderModule.Create(VulkanApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=VulkanApplication.Assets.GetAssetStream('shaders/triangle/triangle_frag.spv');
 try
  fTriangleFragmentShaderModule:=TVulkanShaderModule.Create(VulkanApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageTriangleVertex:=TVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fTriangleVertexShaderModule,'main');

 fVulkanPipelineShaderStageTriangleFragment:=TVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fTriangleFragmentShaderModule,'main');

 fVulkanRenderPass:=nil;

end;

procedure TScreenBlank.Hide;
var Index:TVkInt32;
begin
 FreeAndNil(fVulkanRenderPass);
 FreeAndNil(fVulkanPipelineShaderStageTriangleVertex);
 FreeAndNil(fVulkanPipelineShaderStageTriangleFragment);
 FreeAndNil(fTriangleFragmentShaderModule);
 FreeAndNil(fTriangleVertexShaderModule);
 for Index:=0 to MaxSwapChainImages-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[Index]);
  FreeAndNil(fVulkanRenderSemaphores[Index]);
 end;
 FreeAndNil(fVulkanCommandPool);
 inherited Hide;
end;

procedure TScreenBlank.Resume;
begin
 inherited Resume;
end;

procedure TScreenBlank.Pause;
begin
 inherited Pause;
end;

procedure TScreenBlank.Resize(const aWidth,aHeight:TVkInt32);
begin
 inherited Resize(aWidth,aHeight);
end;

procedure TScreenBlank.AfterCreateSwapChain;
var SwapChainImageIndex:TVkInt32;
    VulkanCommandBuffer:TVulkanCommandBuffer;
begin
 inherited AfterCreateSwapChain;

 FreeAndNil(fVulkanRenderPass);

 fVulkanRenderPass:=TVulkanRenderPass.Create(VulkanApplication.VulkanDevice);

 fVulkanRenderPass.AddSubpassDescription(0,
                                         VK_PIPELINE_BIND_POINT_GRAPHICS,
                                         [],
                                         [fVulkanRenderPass.AddAttachmentReference(fVulkanRenderPass.AddAttachmentDescription(0,
                                                                                                                              VulkanApplication.VulkanSwapChain.ImageFormat,
                                                                                                                              VK_SAMPLE_COUNT_1_BIT,
                                                                                                                              VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                                                                                              VK_ATTACHMENT_STORE_OP_STORE,
                                                                                                                              VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                                                                              VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                              VK_IMAGE_LAYOUT_UNDEFINED, //VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, //VK_IMAGE_LAYOUT_UNDEFINED, // VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                                                                                                              VK_IMAGE_LAYOUT_PRESENT_SRC_KHR //VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL //VK_IMAGE_LAYOUT_PRESENT_SRC_KHR  // VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                                                                                                                             ),
                                                                             VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                                                                            )],
                                         [],
                                         fVulkanRenderPass.AddAttachmentReference(fVulkanRenderPass.AddAttachmentDescription(0,
                                                                                                                             VulkanApplication.VulkanDepthImageFormat,
                                                                                                                             VK_SAMPLE_COUNT_1_BIT,
                                                                                                                             VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                                                                                             VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                             VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                                                                             VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                             VK_IMAGE_LAYOUT_UNDEFINED, //VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, // VK_IMAGE_LAYOUT_UNDEFINED, // VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                                                                                                             VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                                                                                            ),
                                                                                  VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                                                 ),
                                         []);
 fVulkanRenderPass.AddSubpassDependency(VK_SUBPASS_EXTERNAL,
                                        0,
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                        TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT),
                                        TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
                                        TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT));
 fVulkanRenderPass.AddSubpassDependency(0,
                                        VK_SUBPASS_EXTERNAL,
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                        TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
                                        TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT),
                                        TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT));
 fVulkanRenderPass.Initialize;

 fVulkanRenderPass.ClearValues[0].color.float32[0]:=0.0;
 fVulkanRenderPass.ClearValues[0].color.float32[1]:=0.0;
 fVulkanRenderPass.ClearValues[0].color.float32[2]:=0.0;
 fVulkanRenderPass.ClearValues[0].color.float32[3]:=1.0;

 for SwapChainImageIndex:=0 to length(fVulkanRenderCommandBuffers)-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[SwapChainImageIndex]);
 end;

 for SwapChainImageIndex:=0 to VulkanApplication.CountSwapChainImages-1 do begin

  fVulkanRenderCommandBuffers[SwapChainImageIndex]:=TVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

  VulkanCommandBuffer:=fVulkanRenderCommandBuffers[SwapChainImageIndex];

  VulkanCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT));

  fVulkanRenderPass.BeginRenderPass(VulkanCommandBuffer,
                                    VulkanApplication.VulkanFrameBuffers[SwapChainImageIndex],
                                    VK_SUBPASS_CONTENTS_INLINE,
                                    0,
                                    0,
                                    VulkanApplication.VulkanSwapChain.Width,
                                    VulkanApplication.VulkanSwapChain.Height);

  fVulkanRenderPass.EndRenderPass(VulkanCommandBuffer);

  VulkanCommandBuffer.EndRecording;

 end;

end;

procedure TScreenBlank.BeforeDestroySwapChain;
begin
 FreeAndNil(fVulkanRenderPass);
 inherited BeforeDestroySwapChain;
end;

procedure TScreenBlank.Update(const aDeltaTime:double);
begin
 inherited Update(aDeltaTime);
end;

procedure TScreenBlank.Draw(const aSwapChainImageIndex:TVkInt32;var aWaitSemaphore:TVulkanSemaphore;const aWaitFence:TVulkanFence=nil);
begin
 inherited Draw(aSwapChainImageIndex,aWaitSemaphore,nil);
 if assigned(fVulkanRenderPass) then begin

  fVulkanRenderCommandBuffers[aSwapChainImageIndex].Execute(VulkanApplication.VulkanDevice.GraphicsQueue,
                                                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                                            aWaitSemaphore,
                                                            fVulkanRenderSemaphores[aSwapChainImageIndex],
                                                            aWaitFence,
                                                            false);

  aWaitSemaphore:=fVulkanRenderSemaphores[aSwapChainImageIndex];

 end;
end;

end.
