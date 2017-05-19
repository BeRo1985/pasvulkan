unit UnitScreenExampleDragon;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
{$endif}
{$if defined(Win32) or defined(Win64)}
 {$define Windows}
{$ifend}

interface

uses SysUtils,Classes,Vulkan,PasVulkan,PasVulkanSDL2,PasVulkanApplication,UnitRegisteredExamplesList,UnitMath3D,UnitModel;

type PScreenExampleDragonUniformBuffer=^TScreenExampleDragonUniformBuffer;
     TScreenExampleDragonUniformBuffer=record
      ModelViewMatrix:TMatrix4x4;
      ModelViewProjectionMatrix:TMatrix4x4;
      ModelViewNormalMatrix:TMatrix4x4; // actually TMatrix3x3, but it would have then a TMatrix3x4 alignment, according to https://www.khronos.org/registry/vulkan/specs/1.0/html/vkspec.html#interfaces-resources-layout
     end;

     PScreenExampleDragonState=^TScreenExampleDragonState;
     TScreenExampleDragonState=record
      Time:double;
      AnglePhases:array[0..1] of single;
     end;

     PScreenExampleDragonStates=^TScreenExampleDragonStates;
     TScreenExampleDragonStates=array[0..MaxSwapChainImages-1] of TScreenExampleDragonState;

     TScreenExampleDragon=class(TVulkanScreen)
      private
       fDragonVertexShaderModule:TVulkanShaderModule;
       fDragonFragmentShaderModule:TVulkanShaderModule;
       fVulkanPipelineShaderStageDragonVertex:TVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageDragonFragment:TVulkanPipelineShaderStage;
       fVulkanGraphicsPipeline:TVulkanGraphicsPipeline;
       fVulkanRenderPass:TVulkanRenderPass;
       fVulkanModel:TModel;
       fVulkanUniformBuffer:TVulkanBuffer;
       fVulkanDescriptorPool:TVulkanDescriptorPool;
       fVulkanDescriptorSetLayout:TVulkanDescriptorSetLayout;
       fVulkanDescriptorSet:TVulkanDescriptorSet;
       fVulkanPipelineLayout:TVulkanPipelineLayout;
       fVulkanCommandPool:TVulkanCommandPool;
       fVulkanRenderCommandBuffers:array[0..MaxSwapChainImages-1] of TVulkanCommandBuffer;
       fVulkanRenderSemaphores:array[0..MaxSwapChainImages-1] of TVulkanSemaphore;
       fUniformBuffer:TScreenExampleDragonUniformBuffer;
       fBoxAlbedoTexture:TVulkanTexture;
       fReady:boolean;
       fSelectedIndex:TVkInt32;
       fStartY:single;
       fState:TScreenExampleDragonState;
       fStates:TScreenExampleDragonStates;
      public

       constructor Create; override;

       destructor Destroy; override;

       procedure Show; override;

       procedure Hide; override;

       procedure Resume; override;

       procedure Pause; override;

       procedure Resize(const pWidth,pHeight:TSDLInt32); override;

       procedure AfterCreateSwapChain; override;

       procedure BeforeDestroySwapChain; override;

       function HandleEvent(const pEvent:TSDL_Event):boolean; override;

       function KeyDown(const pKeyCode,pKeyModifier:TVkInt32):boolean; override;

       function KeyUp(const pKeyCode,pKeyModifier:TVkInt32):boolean; override;

       function KeyTyped(const pKeyCode,pKeyModifier:TVkInt32):boolean; override;

       function TouchDown(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean; override;

       function TouchUp(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean; override;

       function TouchDragged(const pScreenX,pScreenY,pPressure:single;const pPointerID:TVkInt32):boolean; override;

       function MouseMoved(const pScreenX,pScreenY:TVkInt32):boolean; override;

       function Scrolled(const pAmount:TVkInt32):boolean; override;

       procedure Update(const pDeltaTime:double); override;

       procedure Draw(const pSwapChainImageIndex:TVkInt32;var pWaitSemaphore:TVulkanSemaphore;const pWaitFence:TVulkanFence=nil); override;

     end;

implementation

uses UnitExampleVulkanApplication,UnitTextOverlay,UnitScreenMainMenu;

const Offsets:array[0..0] of TVkDeviceSize=(0);

      FontSize=3.0;

constructor TScreenExampleDragon.Create;
begin
 inherited Create;
 fSelectedIndex:=-1;
 FillChar(fState,SizeOf(TScreenExampleDragonState),#0);
 FillChar(fStates,SizeOf(TScreenExampleDragonStates),#0);
 fReady:=false;
end;

destructor TScreenExampleDragon.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenExampleDragon.Show;
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

 Stream:=VulkanApplication.Assets.GetAssetStream('shaders/dragon/dragon_vert.spv');
 try
  fDragonVertexShaderModule:=TVulkanShaderModule.Create(VulkanApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=VulkanApplication.Assets.GetAssetStream('shaders/dragon/dragon_frag.spv');
 try
  fDragonFragmentShaderModule:=TVulkanShaderModule.Create(VulkanApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fBoxAlbedoTexture:=TVulkanTexture.CreateDefault(VulkanApplication.VulkanDevice,
                                                 VulkanApplication.VulkanDevice.GraphicsQueue,
                                                 VulkanApplication.VulkanGraphicsCommandBuffers[0,0],
                                                 VulkanApplication.VulkanGraphicsCommandBufferFences[0,0],
                                                 VulkanApplication.VulkanDevice.TransferQueue,
                                                 VulkanApplication.VulkanTransferCommandBuffers[0,0],
                                                 VulkanApplication.VulkanTransferCommandBufferFences[0,0],
                                                 vtdtCheckerboard,
                                                 512,
                                                 512,
                                                 1,
                                                 1,
                                                 1,
                                                 true,
                                                 false);{}
 fBoxAlbedoTexture.WrapModeU:=vtwmRepeat;
 fBoxAlbedoTexture.WrapModeV:=vtwmRepeat;
 fBoxAlbedoTexture.WrapModeW:=vtwmRepeat;
 fBoxAlbedoTexture.BorderColor:=VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE;
 fBoxAlbedoTexture.UpdateSampler;

 fVulkanPipelineShaderStageDragonVertex:=TVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fDragonVertexShaderModule,'main');

 fVulkanPipelineShaderStageDragonFragment:=TVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fDragonFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

 fVulkanRenderPass:=nil;

 fVulkanModel:=TModel.Create;

 Stream:=VulkanApplication.Assets.GetAssetStream('models/dragon.mdl');
 try
  fVulkanModel.LoadFromStream(Stream);
  fVulkanModel.Upload(VulkanApplication.VulkanDevice,
                      VulkanApplication.VulkanDevice.TransferQueue,
                      VulkanApplication.VulkanTransferCommandBuffers[0,0],
                      VulkanApplication.VulkanTransferCommandBufferFences[0,0]);
 finally
  Stream.Free;
 end;

 fVulkanUniformBuffer:=TVulkanBuffer.Create(VulkanApplication.VulkanDevice,
                                            SizeOf(TScreenExampleDragonUniformBuffer),
                                            TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT),
                                            TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                            nil,
                                            TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
                                           );
 fVulkanUniformBuffer.UploadData(VulkanApplication.VulkanDevice.TransferQueue,
                                 VulkanApplication.VulkanTransferCommandBuffers[0,0],
                                 VulkanApplication.VulkanTransferCommandBufferFences[0,0],
                                 fUniformBuffer,
                                 0,
                                 SizeOf(TScreenExampleDragonUniformBuffer),
                                 vbutsbmYes);

 fVulkanDescriptorPool:=TVulkanDescriptorPool.Create(VulkanApplication.VulkanDevice,
                                                     TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                     1);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,1);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,1);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TVulkanDescriptorSetLayout.Create(VulkanApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(1,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 fVulkanDescriptorSet:=TVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                   fVulkanDescriptorSetLayout);
 fVulkanDescriptorSet.WriteToDescriptorSet(0,
                                           0,
                                           1,
                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                           [],
                                           [fVulkanUniformBuffer.DescriptorBufferInfo],
                                           [],
                                           false
                                          );
 fVulkanDescriptorSet.WriteToDescriptorSet(1,
                                           0,
                                           1,
                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                           [fBoxAlbedoTexture.DescriptorImageInfo],
                                           [],
                                           [],
                                           false
                                          );
 fVulkanDescriptorSet.Flush;

 fVulkanPipelineLayout:=TVulkanPipelineLayout.Create(VulkanApplication.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

end;

procedure TScreenExampleDragon.Hide;
var Index:TVkInt32;
begin
 fVulkanModel.Unload;
 FreeAndNil(fVulkanPipelineLayout);
 FreeAndNil(fVulkanDescriptorSet);
 FreeAndNil(fVulkanDescriptorSetLayout);
 FreeAndNil(fVulkanDescriptorPool);
 FreeAndNil(fVulkanUniformBuffer);
 FreeAndNil(fVulkanModel);
 FreeAndNil(fVulkanRenderPass);
 FreeAndNil(fVulkanGraphicsPipeline);
 FreeAndNil(fVulkanPipelineShaderStageDragonVertex);
 FreeAndNil(fVulkanPipelineShaderStageDragonFragment);
 FreeAndNil(fDragonFragmentShaderModule);
 FreeAndNil(fDragonVertexShaderModule);
 FreeAndNil(fBoxAlbedoTexture);
 for Index:=0 to MaxSwapChainImages-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[Index]);
  FreeAndNil(fVulkanRenderSemaphores[Index]);
 end;
 FreeAndNil(fVulkanCommandPool);
 inherited Hide;
end;

procedure TScreenExampleDragon.Resume;
begin
 inherited Resume;
end;

procedure TScreenExampleDragon.Pause;
begin
 inherited Pause;
end;

procedure TScreenExampleDragon.Resize(const pWidth,pHeight:TSDLInt32);
begin
 inherited Resize(pWidth,pHeight);
end;

procedure TScreenExampleDragon.AfterCreateSwapChain;
var SwapChainImageIndex:TVkInt32;
    VulkanCommandBuffer:TVulkanCommandBuffer;
begin
 inherited AfterCreateSwapChain;

 FreeAndNil(fVulkanRenderPass);
 FreeAndNil(fVulkanGraphicsPipeline);

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

 fVulkanGraphicsPipeline:=TVulkanGraphicsPipeline.Create(VulkanApplication.VulkanDevice,
                                                         VulkanApplication.VulkanPipelineCache,
                                                         0,
                                                         [],
                                                         fVulkanPipelineLayout,
                                                         fVulkanRenderPass,
                                                         0,
                                                         nil,
                                                         0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageDragonVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageDragonFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputBindingDescription(0,SizeOf(TModelVertex),VK_VERTEX_INPUT_RATE_VERTEX);
 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(0,0,VK_FORMAT_R32G32B32_SFLOAT,TVkPtrUInt(pointer(@PModelVertex(nil)^.Position)));
 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(1,0,VK_FORMAT_R16G16B16A16_SNORM,TVkPtrUInt(pointer(@PModelVertex(nil)^.QTangent)));
 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(2,0,VK_FORMAT_R32G32_SFLOAT,TVkPtrUInt(pointer(@PModelVertex(nil)^.TexCoord)));
 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(3,0,VK_FORMAT_R32_UINT,TVkPtrUInt(pointer(@PModelVertex(nil)^.Material)));

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,VulkanApplication.VulkanSwapChain.Width,VulkanApplication.VulkanSwapChain.Height,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,VulkanApplication.VulkanSwapChain.Width,VulkanApplication.VulkanSwapChain.Height);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
 fVulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_CLOCKWISE;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
 fVulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

 fVulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=VK_SAMPLE_COUNT_1_BIT;
 fVulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=false;
 fVulkanGraphicsPipeline.MultisampleState.MinSampleShading:=0.0;
 fVulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
 fVulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=false;
 fVulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;

 fVulkanGraphicsPipeline.ColorBlendState.LogicOpEnable:=false;
 fVulkanGraphicsPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
 fVulkanGraphicsPipeline.ColorBlendState.BlendConstants[0]:=0.0;
 fVulkanGraphicsPipeline.ColorBlendState.BlendConstants[1]:=0.0;
 fVulkanGraphicsPipeline.ColorBlendState.BlendConstants[2]:=0.0;
 fVulkanGraphicsPipeline.ColorBlendState.BlendConstants[3]:=0.0;
 fVulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_OP_ADD,
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

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

  VulkanCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanPipelineLayout.Handle,0,1,@fVulkanDescriptorSet.Handle,0,nil);
  VulkanCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
  VulkanCommandBuffer.CmdBindVertexBuffers(0,1,@fVulkanModel.VertexBuffer.Handle,@Offsets);
  VulkanCommandBuffer.CmdBindVertexBuffers(1,1,@fVulkanModel.VertexBuffer.Handle,@Offsets);
  VulkanCommandBuffer.CmdBindVertexBuffers(2,1,@fVulkanModel.VertexBuffer.Handle,@Offsets);
  VulkanCommandBuffer.CmdBindVertexBuffers(3,1,@fVulkanModel.VertexBuffer.Handle,@Offsets);
  VulkanCommandBuffer.CmdBindIndexBuffer(fVulkanModel.IndexBuffer.Handle,0,VK_INDEX_TYPE_UINT32);
  VulkanCommandBuffer.CmdDrawIndexed(fVulkanModel.CountIndices,1,0,0,1);

  fVulkanRenderPass.EndRenderPass(VulkanCommandBuffer);

  VulkanCommandBuffer.EndRecording;

 end;

end;

procedure TScreenExampleDragon.BeforeDestroySwapChain;
begin
 FreeAndNil(fVulkanRenderPass);
 FreeAndNil(fVulkanGraphicsPipeline);
 inherited BeforeDestroySwapChain;
end;

function TScreenExampleDragon.HandleEvent(const pEvent:TSDL_Event):boolean;
begin
 result:=inherited HandleEvent(pEvent);
end;

function TScreenExampleDragon.KeyDown(const pKeyCode,pKeyModifier:TVkInt32):boolean;
begin
 result:=false;
 if fReady then begin
  case pKeyCode of
   KEYCODE_AC_BACK,KEYCODE_ESCAPE:begin
    VulkanApplication.NextScreen:=TScreenMainMenu.Create;
   end;
   KEYCODE_UP:begin
    if fSelectedIndex<=0 then begin
     fSelectedIndex:=0;
    end else begin
     dec(fSelectedIndex);
    end;
   end;
   KEYCODE_DOWN:begin
    if fSelectedIndex>=0 then begin
     fSelectedIndex:=0;
    end else begin
     inc(fSelectedIndex);
    end;
   end;
   KEYCODE_PAGEUP:begin
    if fSelectedIndex<0 then begin
     fSelectedIndex:=0;
    end;
   end;
   KEYCODE_PAGEDOWN:begin
    if fSelectedIndex<0 then begin
     fSelectedIndex:=0;
    end;
   end;
   KEYCODE_HOME:begin
    fSelectedIndex:=0;
   end;
   KEYCODE_END:begin
    fSelectedIndex:=0;
   end;
   KEYCODE_RETURN,KEYCODE_SPACE:begin
    if fSelectedIndex=0 then begin
     VulkanApplication.NextScreen:=TScreenMainMenu.Create;
    end;
   end;
  end;
 end;
end;

function TScreenExampleDragon.KeyUp(const pKeyCode,pKeyModifier:TVkInt32):boolean;
begin
 result:=false;
end;

function TScreenExampleDragon.KeyTyped(const pKeyCode,pKeyModifier:TVkInt32):boolean;
begin
 result:=false;
end;

function TScreenExampleDragon.TouchDown(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean;
var Index:TVkInt32;
    cy:single;
begin
 result:=false;
 if fReady then begin
  fSelectedIndex:=-1;
  cy:=fStartY;
  for Index:=0 to 0 do begin
   if (pScreenY>=cy) and (pScreenY<=(cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*FontSize))) then begin
    fSelectedIndex:=Index;
    if fSelectedIndex=0 then begin
     VulkanApplication.NextScreen:=TScreenMainMenu.Create;
    end;
   end;
   cy:=cy+((ExampleVulkanApplication.TextOverlay.FontCharHeight+4)*FontSize);
  end;
 end;
end;

function TScreenExampleDragon.TouchUp(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean;
begin
 result:=false;
end;

function TScreenExampleDragon.TouchDragged(const pScreenX,pScreenY,pPressure:single;const pPointerID:TVkInt32):boolean;
var Index:TVkInt32;
    cy:single;
begin
 result:=false;
 if fReady then begin
  fSelectedIndex:=-1;
  cy:=fStartY;
  for Index:=0 to 0 do begin
   if (pScreenY>=cy) and (pScreenY<=(cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*FontSize))) then begin
    fSelectedIndex:=Index;
   end;
   cy:=cy+((ExampleVulkanApplication.TextOverlay.FontCharHeight+4)*FontSize);
  end;
 end;
end;

function TScreenExampleDragon.MouseMoved(const pScreenX,pScreenY:TVkInt32):boolean;
var Index:TVkInt32;
    cy:single;
begin
 result:=false;
 if fReady then begin
  fSelectedIndex:=-1;
  cy:=fStartY;
  for Index:=0 to 0 do begin
   if (pScreenY>=cy) and (pScreenY<=(cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*FontSize))) then begin
    fSelectedIndex:=Index;
   end;
   cy:=cy+((ExampleVulkanApplication.TextOverlay.FontCharHeight+4)*FontSize);
  end;
 end;
end;

function TScreenExampleDragon.Scrolled(const pAmount:TVkInt32):boolean;
begin
 result:=false;
end;

procedure TScreenExampleDragon.Update(const pDeltaTime:double);
const BoolToInt:array[boolean] of TVkInt32=(0,1);
      Options:array[0..0] of string=('Back');
      f0=1.0/(2.0*pi);
      f1=0.5/(2.0*pi);
var Index:TVkInt32;
    cy:single;
    s:string;
    IsSelected:boolean;
begin
 inherited Update(pDeltaTime);
 fState.Time:=fState.Time+pDeltaTime;
 fState.AnglePhases[0]:=frac(fState.AnglePhases[0]+(pDeltaTime*f0));
 fState.AnglePhases[1]:=frac(fState.AnglePhases[1]+(pDeltaTime*f1));
 fStates[VulkanApplication.UpdateFrameCounter and 1]:=fState;
 ExampleVulkanApplication.TextOverlay.AddText(VulkanApplication.Width*0.5,ExampleVulkanApplication.TextOverlay.FontCharHeight*1.0,2.0,toaCenter,'Dragon');
 fStartY:=VulkanApplication.Height-((((ExampleVulkanApplication.TextOverlay.FontCharHeight+4)*FontSize)*1.25)-(4*FontSize));
 cy:=fStartY;
 for Index:=0 to 0 do begin
  IsSelected:=fSelectedIndex=Index;
  s:=' '+Options[Index]+' ';
  if IsSelected then begin
   s:='>'+s+'<';
  end;
  ExampleVulkanApplication.TextOverlay.AddText(VulkanApplication.Width*0.5,cy,FontSize,toaCenter,s,MenuColors[IsSelected,0,0],MenuColors[IsSelected,0,1],MenuColors[IsSelected,0,2],MenuColors[IsSelected,0,3],MenuColors[IsSelected,1,0],MenuColors[IsSelected,1,1],MenuColors[IsSelected,1,2],MenuColors[IsSelected,1,3]);
  cy:=cy+((ExampleVulkanApplication.TextOverlay.FontCharHeight+4)*FontSize);
 end;
 fReady:=true;
end;

procedure TScreenExampleDragon.Draw(const pSwapChainImageIndex:TVkInt32;var pWaitSemaphore:TVulkanSemaphore;const pWaitFence:TVulkanFence=nil);
const TwoPI=2.0*pi;
var p:pointer;
    ModelMatrix:TMatrix4x4;
    ViewMatrix:TMatrix4x4;
    ProjectionMatrix:TMatrix4x4;
    State:PScreenExampleDragonState;
begin
 inherited Draw(pSwapChainImageIndex,pWaitSemaphore,nil);
 if assigned(fVulkanGraphicsPipeline) then begin

  State:=@fStates[VulkanApplication.DrawFrameCounter and 1];

  ModelMatrix:=Matrix4x4TermMul(Matrix4x4Rotate(State^.AnglePhases[0]*TwoPI,Vector3(0.0,0.0,1.0)),
                                Matrix4x4Rotate(State^.AnglePhases[1]*TwoPI,Vector3(0.0,1.0,0.0)));
  ViewMatrix:=Matrix4x4Translate(0.0,0.0,-32.0);
  ProjectionMatrix:=Matrix4x4Perspective(45.0,VulkanApplication.Width/VulkanApplication.Height,1.0,1024.0);

  fUniformBuffer.ModelViewMatrix:=Matrix4x4TermMul(ModelMatrix,ViewMatrix);
  fUniformBuffer.ModelViewProjectionMatrix:=Matrix4x4TermMul(fUniformBuffer.ModelViewMatrix,ProjectionMatrix);
  fUniformBuffer.ModelViewNormalMatrix:=Matrix4x4(Matrix3x3TermTranspose(Matrix3x3TermInverse(Matrix3x3(fUniformBuffer.ModelViewMatrix))));

  p:=fVulkanUniformBuffer.Memory.MapMemory(0,SizeOf(TScreenExampleDragonUniformBuffer));
  if assigned(p) then begin
   try
    Move(fUniformBuffer,p^,SizeOf(TScreenExampleDragonUniformBuffer));
   finally
    fVulkanUniformBuffer.Memory.UnmapMemory;
   end;
  end;

  fVulkanRenderCommandBuffers[pSwapChainImageIndex].Execute(VulkanApplication.VulkanDevice.GraphicsQueue,
                                                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                                            pWaitSemaphore,
                                                            fVulkanRenderSemaphores[pSwapChainImageIndex],
                                                            pWaitFence,
                                                            false);

  pWaitSemaphore:=fVulkanRenderSemaphores[pSwapChainImageIndex];

 end;
end;

initialization
 RegisterExample('Dragon',TScreenExampleDragon);
end.
