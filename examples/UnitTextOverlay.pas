unit UnitTextOverlay;
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
{$m+}

interface

uses SysUtils,
     Classes,
     UnitRegisteredExamplesList,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application;

const TextOverlayBufferCharSize=65536;

type PTextOverlayBufferCharVertex=^TTextOverlayBufferCharVertex;
     TTextOverlayBufferCharVertex=packed record
      x,y:TpvFloat;
      u,v,w:TpvFloat;
      br,bg,bb,ba:TpvFloat;
      fr,fg,fb,fa:TpvFloat;
     end;

     PTextOverlayBufferCharVertices=^TTextOverlayBufferCharVertices;
     TTextOverlayBufferCharVertices=array[0..3] of TTextOverlayBufferCharVertex;

     PTextOverlayBufferChar=^TTextOverlayBufferChar;
     TTextOverlayBufferChar=packed record
      Vertices:TTextOverlayBufferCharVertices;
     end;

     PTextOverlayBufferChars=^TTextOverlayBufferChars;
     TTextOverlayBufferChars=array[0..TextOverlayBufferCharSize-1] of TTextOverlayBufferChar;

     PTextOverlayBufferCharsBuffers=^TTextOverlayBufferCharsBuffers;
     TTextOverlayBufferCharsBuffers=array[0..MaxSwapChainImages-1] of TTextOverlayBufferChars;

     PTextOverlayIndices=^TTextOverlayIndices;
     TTextOverlayIndices=array[0..(TextOverlayBufferCharSize*6)-1] of TpvInt32;

     TTextOverlayAlignment=
      (
       toaLeft,
       toaCenter,
       toaRight
      );

     PTextOverlayUniformBuffer=^TTextOverlayUniformBuffer;
     TTextOverlayUniformBuffer=record
      uThreshold:TpvFloat;
     end;

     TTextOverlay=class
      private
       fLoaded:boolean;
       fUpdateBufferIndex:TpvInt32;
       fBufferChars:PTextOverlayBufferChars;
       fBufferCharsBuffers:TTextOverlayBufferCharsBuffers;
       fCountBufferChars:TpvInt32;
       fCountBufferCharsBuffers:array[0..MaxSwapChainImages-1] of TpvInt32;
       fIndices:TTextOverlayIndices;
       fTextOverlayVertexShaderModule:TVulkanShaderModule;
       fTextOverlayFragmentShaderModule:TVulkanShaderModule;
       fVulkanPipelineShaderStageTriangleVertex:TVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageTriangleFragment:TVulkanPipelineShaderStage;
       fVulkanGraphicsPipeline:TVulkanGraphicsPipeline;
       fVulkanRenderPass:TVulkanRenderPass;
       fVulkanVertexBuffers:array[0..MaxSwapChainImages-1] of TVulkanBuffer;
       fVulkanIndexBuffer:TVulkanBuffer;
       fVulkanUniformBuffer:TVulkanBuffer;
       fVulkanDescriptorPool:TVulkanDescriptorPool;
       fVulkanDescriptorSetLayout:TVulkanDescriptorSetLayout;
       fVulkanDescriptorSet:TVulkanDescriptorSet;
       fVulkanPipelineLayout:TVulkanPipelineLayout;
       fVulkanCommandPool:TVulkanCommandPool;
       fVulkanRenderCommandBuffers:array[0..MaxSwapChainImages-1] of TVulkanCommandBuffer;
       fVulkanRenderSemaphores:array[0..MaxSwapChainImages-1] of TVulkanSemaphore;
       fUniformBuffer:TTextOverlayUniformBuffer;
       fFontTexture:TVulkanTexture;
       fFontCharWidth:TpvFloat;
       fFontCharHeight:TpvFloat;
       fInvWidth:TpvFloat;
       fInvHeight:TpvFloat;
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       procedure Load;
       procedure Unload;
       procedure AfterCreateSwapChain;
       procedure BeforeDestroySwapChain;
       procedure Reset;
       procedure AddText(const pX,pY,aSize:TpvFloat;const aAlignment:TTextOverlayAlignment;const aText:TVulkanApplicationRawByteString;const pBR:TpvFloat=1.0;const pBG:TpvFloat=1.0;const pBB:TpvFloat=1.0;const pBA:TpvFloat=0.0;const pFR:TpvFloat=1.0;const pFG:TpvFloat=1.0;const pFB:TpvFloat=1.0;const pFA:TpvFloat=1.0);
       procedure PreUpdate(const aDeltaTime:double);
       procedure PostUpdate(const aDeltaTime:double);
       procedure Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TVulkanSemaphore;const aWaitFence:TVulkanFence=nil);
      published
       property FontCharWidth:TpvFloat read fFontCharWidth;
       property FontCharHeight:TpvFloat read fFontCharHeight;
     end;

implementation

uses UnitSDFFont;

constructor TTextOverlay.Create;
var Index:TpvInt32;
begin
 inherited Create;

 fLoaded:=false;

 for Index:=0 to TextOverlayBufferCharSize-1 do begin
  fIndices[(Index*6)+0]:=(Index*4)+0;
  fIndices[(Index*6)+1]:=(Index*4)+1;
  fIndices[(Index*6)+2]:=(Index*4)+2;
  fIndices[(Index*6)+3]:=(Index*4)+1;
  fIndices[(Index*6)+4]:=(Index*4)+3;
  fIndices[(Index*6)+5]:=(Index*4)+2;
 end;

 fCountBufferChars:=0;

 for Index:=low(fCountBufferCharsBuffers) to high(fCountBufferCharsBuffers) do begin
  fCountBufferCharsBuffers[Index]:=0;
 end;

end;

destructor TTextOverlay.Destroy;
begin
 inherited Destroy;
end;

procedure TTextOverlay.Load;
var Stream:TStream;
    Index:TpvInt32;
begin

 if not fLoaded then begin

  fLoaded:=true;

  fVulkanCommandPool:=TVulkanCommandPool.Create(VulkanApplication.VulkanDevice,
                                                VulkanApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                                TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
  for Index:=0 to MaxSwapChainImages-1 do begin
   fVulkanRenderCommandBuffers[Index]:=TVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
   fVulkanRenderSemaphores[Index]:=TVulkanSemaphore.Create(VulkanApplication.VulkanDevice);
  end;

  Stream:=VulkanApplication.Assets.GetAssetStream('shaders/textoverlay/textoverlay_vert.spv');
  try
   fTextOverlayVertexShaderModule:=TVulkanShaderModule.Create(VulkanApplication.VulkanDevice,Stream);
  finally
   Stream.Free;
  end;

  Stream:=VulkanApplication.Assets.GetAssetStream('shaders/textoverlay/textoverlay_frag.spv');
  try
   fTextOverlayFragmentShaderModule:=TVulkanShaderModule.Create(VulkanApplication.VulkanDevice,Stream);
  finally
   Stream.Free;
  end;

  fVulkanPipelineShaderStageTriangleVertex:=TVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fTextOverlayVertexShaderModule,'main');

  fVulkanPipelineShaderStageTriangleFragment:=TVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fTextOverlayFragmentShaderModule,'main');

  fVulkanGraphicsPipeline:=nil;

  fVulkanRenderPass:=nil;

  fFontTexture:=TVulkanTexture.CreateFromMemory(VulkanApplication.VulkanDevice,
                                                VulkanApplication.VulkanDevice.GraphicsQueue,
                                                VulkanApplication.VulkanGraphicsCommandBuffers[0,0],
                                                VulkanApplication.VulkanGraphicsCommandBufferFences[0,0],
                                                VulkanApplication.VulkanDevice.TransferQueue,
                                                VulkanApplication.VulkanTransferCommandBuffers[0,0],
                                                VulkanApplication.VulkanTransferCommandBufferFences[0,0],
                                                VK_FORMAT_R8_UNORM,
                                                VK_SAMPLE_COUNT_1_BIT,
                                                SDFFontWidth,
                                                SDFFontHeight,
                                                0,
                                                SDFFontDepth,
                                                1,
                                                1,
                                                [vtufTransferDst,vtufSampled],
                                                @SDFFontData,
                                                SizeOf(SDFFontData),
                                                false,
                                                false,
                                                0,
                                                false
                                               );
  fFontTexture.WrapModeU:=vtwmClampToBorder;
  fFontTexture.WrapModeV:=vtwmClampToBorder;
  fFontTexture.WrapModeW:=vtwmClampToBorder;
  fFontTexture.BorderColor:=VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE;
  fFontTexture.UpdateSampler;

  for Index:=0 to MaxSwapChainImages-1 do begin
   fVulkanVertexBuffers[Index]:=TVulkanBuffer.Create(VulkanApplication.VulkanDevice,
                                                     SizeOf(TTextOverlayBufferChars),
                                                     TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT),
                                                     TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                     nil,
                                                     TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) {or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)},
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     [vbfPersistentMapped]
                                                    );
   fVulkanVertexBuffers[Index].UploadData(VulkanApplication.VulkanDevice.TransferQueue,
                                          VulkanApplication.VulkanTransferCommandBuffers[0,0],
                                          VulkanApplication.VulkanTransferCommandBufferFences[0,0],
                                          fBufferCharsBuffers[0],
                                          0,
                                          SizeOf(TTextOverlayBufferChars),
                                          vbutsbmNo);
  end;
  
  fVulkanIndexBuffer:=TVulkanBuffer.Create(VulkanApplication.VulkanDevice,
                                           SizeOf(TTextOverlayIndices),
                                           TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT),
                                           TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                           nil,
                                           TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
                                          );
  fVulkanIndexBuffer.UploadData(VulkanApplication.VulkanDevice.TransferQueue,
                                VulkanApplication.VulkanTransferCommandBuffers[0,0],
                                VulkanApplication.VulkanTransferCommandBufferFences[0,0],
                                fIndices,
                                0,
                                SizeOf(TTextOverlayIndices),
                                vbutsbmYes);

  fVulkanUniformBuffer:=TVulkanBuffer.Create(VulkanApplication.VulkanDevice,
                                             SizeOf(TTextOverlayUniformBuffer),
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
                                  SizeOf(TTextOverlayUniformBuffer),
                                  vbutsbmNo);

  fVulkanDescriptorPool:=TVulkanDescriptorPool.Create(VulkanApplication.VulkanDevice,
                                                      TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                      2);
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
                                            [fFontTexture.DescriptorImageInfo],
                                            [],
                                            [],
                                            false
                                           );
  fVulkanDescriptorSet.Flush;

  fVulkanPipelineLayout:=TVulkanPipelineLayout.Create(VulkanApplication.VulkanDevice);
  fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
  fVulkanPipelineLayout.Initialize;

 end;

end;

procedure TTextOverlay.Unload;
var Index:TpvInt32;
begin

 if fLoaded then begin

  fLoaded:=false;

  FreeAndNil(fVulkanRenderPass);
  FreeAndNil(fVulkanGraphicsPipeline);
  FreeAndNil(fVulkanPipelineLayout);
  FreeAndNil(fVulkanDescriptorSet);
  FreeAndNil(fVulkanDescriptorSetLayout);
  FreeAndNil(fVulkanDescriptorPool);
  FreeAndNil(fVulkanUniformBuffer);
  FreeAndNil(fVulkanIndexBuffer);
  for Index:=0 to MaxSwapChainImages-1 do begin
   FreeAndNil(fVulkanVertexBuffers[Index]);
  end;
  FreeAndNil(fVulkanPipelineShaderStageTriangleVertex);
  FreeAndNil(fVulkanPipelineShaderStageTriangleFragment);
  FreeAndNil(fTextOverlayFragmentShaderModule);
  FreeAndNil(fTextOverlayVertexShaderModule);
  FreeAndNil(fFontTexture);

  for Index:=0 to MaxSwapChainImages-1 do begin
   FreeAndNil(fVulkanRenderCommandBuffers[Index]);
   FreeAndNil(fVulkanRenderSemaphores[Index]);
  end;
  FreeAndNil(fVulkanCommandPool);

 end;
end;

procedure TTextOverlay.AfterCreateSwapChain;
begin

 FreeAndNil(fVulkanRenderPass);
 FreeAndNil(fVulkanGraphicsPipeline);

 fVulkanRenderPass:=TVulkanRenderPass.Create(VulkanApplication.VulkanDevice);

 fVulkanRenderPass.AddSubpassDescription(0,
                                         VK_PIPELINE_BIND_POINT_GRAPHICS,
                                         [],
                                         [fVulkanRenderPass.AddAttachmentReference(fVulkanRenderPass.AddAttachmentDescription(0,
                                                                                                                              VulkanApplication.VulkanSwapChain.ImageFormat,
                                                                                                                              VK_SAMPLE_COUNT_1_BIT,
                                                                                                                              VK_ATTACHMENT_LOAD_OP_LOAD,
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
                                                                                                                             VK_ATTACHMENT_LOAD_OP_DONT_CARE,
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

 fVulkanGraphicsPipeline:=TVulkanGraphicsPipeline.Create(VulkanApplication.VulkanDevice,
                                                         VulkanApplication.VulkanPipelineCache,
                                                         0,
                                                         [],
                                                         fVulkanPipelineLayout,
                                                         fVulkanRenderPass,
                                                         0,
                                                         nil,
                                                         0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageTriangleVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageTriangleFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputBindingDescription(0,SizeOf(TVkFloat)*(2+3+4+4),VK_VERTEX_INPUT_RATE_VERTEX);
 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(0,0,VK_FORMAT_R32G32_SFLOAT,SizeOf(TVkFloat)*0);
 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(1,0,VK_FORMAT_R32G32B32_SFLOAT,SizeOf(TVkFloat)*2);
 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(2,0,VK_FORMAT_R32G32B32A32_SFLOAT,SizeOf(TVkFloat)*(2+3));
 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(3,0,VK_FORMAT_R32G32B32A32_SFLOAT,SizeOf(TVkFloat)*(2+3+4));

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
 fVulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(true,
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

 fFontCharWidth:=VulkanApplication.Width/160.0;
 fFontCharHeight:=fFontCharWidth*2.0;

 fInvWidth:=1.0/VulkanApplication.Width;
 fInvHeight:=1.0/VulkanApplication.Height;

 if assigned(fVulkanUniformBuffer) then begin
  fUniformBuffer.uThreshold:=(SDFFontSpreadScale/sqrt(sqr(fFontCharWidth)+sqr(fFontCharHeight)))*1.0;
  fVulkanUniformBuffer.UploadData(VulkanApplication.VulkanDevice.TransferQueue,
                                  VulkanApplication.VulkanTransferCommandBuffers[0,0],
                                  VulkanApplication.VulkanTransferCommandBufferFences[0,0],
                                  fUniformBuffer,
                                  0,
                                  SizeOf(TTextOverlayUniformBuffer),
                                  vbutsbmNo);

 end;

end;

procedure TTextOverlay.BeforeDestroySwapChain;
begin
 FreeAndNil(fVulkanRenderPass);
 FreeAndNil(fVulkanGraphicsPipeline);
end;

procedure TTextOverlay.Reset;
begin
 fCountBufferChars:=0;
end;

procedure TTextOverlay.AddText(const pX,pY,aSize:TpvFloat;const aAlignment:TTextOverlayAlignment;const aText:TVulkanApplicationRawByteString;const pBR:TpvFloat=1.0;const pBG:TpvFloat=1.0;const pBB:TpvFloat=1.0;const pBA:TpvFloat=0.0;const pFR:TpvFloat=1.0;const pFG:TpvFloat=1.0;const pFB:TpvFloat=1.0;const pFA:TpvFloat=1.0);
var Index,EdgeIndex:TpvInt32;
    BufferChar:PTextOverlayBufferChar;
    CurrentChar:TpvUInt8;
    cX:TpvFloat;
begin
 if (pBA>0.0) or (pFA>0.0) then begin
  case aAlignment of
   toaLeft:begin
    cX:=pX;
   end;
   toaCenter:begin
    cX:=pX-((length(aText)*fFontCharWidth*aSize)*0.5);
   end;
   else {toaRight:}begin
    cX:=pX-(length(aText)*fFontCharWidth*aSize);
   end;
  end;
  for Index:=1 to length(aText) do begin
   CurrentChar:=TpvUInt8(AnsiChar(aText[Index]));
   if (CurrentChar<>32) or (pBA>0.0) then begin
    if fCountBufferChars<TextOverlayBufferCharSize then begin
     BufferChar:=@fBufferChars^[fCountBufferChars];
     inc(fCountBufferChars);
     for EdgeIndex:=0 to 3 do begin
      BufferChar^.Vertices[EdgeIndex].x:=(((cX+((EdgeIndex and 1)*fFontCharWidth*aSize))*fInvWidth)*2.0)-1.0;
      BufferChar^.Vertices[EdgeIndex].y:=(((pY+((EdgeIndex shr 1)*fFontCharHeight*aSize))*fInvHeight)*2.0)-1.0;
      BufferChar^.Vertices[EdgeIndex].u:=EdgeIndex and 1;
      BufferChar^.Vertices[EdgeIndex].v:=EdgeIndex shr 1;
      BufferChar^.Vertices[EdgeIndex].w:=CurrentChar;
      BufferChar^.Vertices[EdgeIndex].br:=pBR;
      BufferChar^.Vertices[EdgeIndex].bg:=pBG;
      BufferChar^.Vertices[EdgeIndex].bb:=pBB;
      BufferChar^.Vertices[EdgeIndex].ba:=pBA;
      BufferChar^.Vertices[EdgeIndex].fr:=pFR;
      BufferChar^.Vertices[EdgeIndex].fg:=pFG;
      BufferChar^.Vertices[EdgeIndex].fb:=pFB;
      BufferChar^.Vertices[EdgeIndex].fa:=pFA;
     end;
    end;
   end;
   cX:=cX+(fFontCharWidth*aSize);
  end;
 end;
end;

procedure TTextOverlay.PreUpdate(const aDeltaTime:double);
var FPS,ms:string;
begin
 fUpdateBufferIndex:=VulkanApplication.UpdateSwapChainImageIndex;
 fBufferChars:=@fBufferCharsBuffers[fUpdateBufferIndex];
 begin
  Reset;
  AddText(0.0,fFontCharHeight*0.0,1.0,toaLeft,'Device: '+VulkanApplication.VulkanDevice.PhysicalDevice.DeviceName{$ifdef Android}+' ('+TVulkanApplicationRawByteString(AndroidDeviceName)+')'{$endif});
  AddText(0.0,fFontCharHeight*1.0,1.0,toaLeft,'Vulkan API version: '+IntToStr(VulkanApplication.VulkanDevice.PhysicalDevice.Properties.apiVersion shr 22)+'.'+IntToStr((VulkanApplication.VulkanDevice.PhysicalDevice.Properties.apiVersion shr 12) and $3ff)+'.'+IntToStr((VulkanApplication.VulkanDevice.PhysicalDevice.Properties.apiVersion shr 0) and $fff));
  Str(VulkanApplication.FramesPerSecond:1:1,FPS);
  Str(VulkanApplication.DeltaTime*1000.0:1:2,ms);
  AddText(0.0,fFontCharHeight*2.0,1.0,toaLeft,'Frame rate: '+FPS+' FPS');
  AddText(0.0,fFontCharHeight*3.0,1.0,toaLeft,'Frame time: '+MS+' ms');
 end;
end;

procedure TTextOverlay.PostUpdate(const aDeltaTime:double);
begin
 fCountBufferCharsBuffers[fUpdateBufferIndex]:=fCountBufferChars;
end;

procedure TTextOverlay.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TVulkanSemaphore;const aWaitFence:TVulkanFence=nil);
const Offsets:array[0..0] of TVkDeviceSize=(0);
var BufferIndex,Size:TpvInt32;
    VulkanVertexBuffer:TVulkanBuffer;
    VulkanCommandBuffer:TVulkanCommandBuffer;
    VulkanSwapChain:TVulkanSwapChain;
    p:TpvPointer;
begin

 BufferIndex:=VulkanApplication.DrawSwapChainImageIndex;
 if fCountBufferCharsBuffers[BufferIndex]=0 then begin
  fCountBufferCharsBuffers[BufferIndex]:=1;
  FillChar(fBufferCharsBuffers[BufferIndex],SizeOf(TTextOverlayBufferChar),#0);
 end;

 begin

  VulkanVertexBuffer:=fVulkanVertexBuffers[aSwapChainImageIndex];

  Size:=SizeOf(TTextOverlayBufferChar)*fCountBufferCharsBuffers[BufferIndex];
  p:=VulkanVertexBuffer.Memory.MapMemory(0,Size);
  if assigned(p) then begin
   try
    Move(fBufferCharsBuffers[BufferIndex],p^,Size);
    VulkanVertexBuffer.Memory.FlushMappedMemoryRange(p,Size);
   finally
    VulkanVertexBuffer.Memory.UnmapMemory;
   end;
  end;

  if assigned(fVulkanGraphicsPipeline) then begin

   VulkanCommandBuffer:=fVulkanRenderCommandBuffers[aSwapChainImageIndex];
   VulkanSwapChain:=VulkanApplication.VulkanSwapChain;

   VulkanCommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));

   VulkanCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));

  {VulkanCommandBuffer.MetaCmdMemoryBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
                                            TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT),
                                            TVkAccessFlags(VK_ACCESS_UNIFORM_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT));}

   fVulkanRenderPass.BeginRenderPass(VulkanCommandBuffer,
                                     VulkanApplication.VulkanFrameBuffers[aSwapChainImageIndex],
                                     VK_SUBPASS_CONTENTS_INLINE,
                                     0,
                                     0,
                                     VulkanSwapChain.Width,
                                     VulkanSwapChain.Height);

   VulkanCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanPipelineLayout.Handle,0,1,@fVulkanDescriptorSet.Handle,0,nil);
   VulkanCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
   VulkanCommandBuffer.CmdBindVertexBuffers(0,1,@VulkanVertexBuffer.Handle,@Offsets);
   VulkanCommandBuffer.CmdBindIndexBuffer(fVulkanIndexBuffer.Handle,0,VK_INDEX_TYPE_UINT32);
   VulkanCommandBuffer.CmdDrawIndexed(fCountBufferCharsBuffers[BufferIndex]*6,1,0,0,0);

   fVulkanRenderPass.EndRenderPass(VulkanCommandBuffer);

   VulkanCommandBuffer.EndRecording;

   VulkanCommandBuffer.Execute(VulkanApplication.VulkanDevice.GraphicsQueue,
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                               aWaitSemaphore,
                               fVulkanRenderSemaphores[aSwapChainImageIndex],
                               aWaitFence,
                               false);

   aWaitSemaphore:=fVulkanRenderSemaphores[aSwapChainImageIndex];

  end;
 end;
end;

end.
