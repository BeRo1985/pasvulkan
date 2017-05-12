unit UnitTextOverlay;
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

uses SysUtils,Classes,Vulkan,PasVulkan,PasVulkanSDL2,PasVulkanApplication;

const TextOverlayBufferCharSize=65536;

type PTextOverlayBufferCharVertex=^TTextOverlayBufferCharVertex;
     TTextOverlayBufferCharVertex=packed record
      x,y:single;
      u,v,w:single;
      r,g,b:single;
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
     TTextOverlayBufferCharsBuffers=array[0..1] of TTextOverlayBufferChars;

     PTextOverlayIndices=^TTextOverlayIndices;
     TTextOverlayIndices=array[0..(TextOverlayBufferCharSize*6)-1] of TVkInt32;

     TTextOverlayAlignment=
      (
       toaLeft,
       toaCenter,
       toaRight
      );

     PTextOverlayUniformBuffer=^TTextOverlayUniformBuffer;
     TTextOverlayUniformBuffer=record
      uThreshold:single;
     end;

     TTextOverlay=class
      private
       fLoaded:boolean;
       fUpdateBufferIndex:TVkInt32;
       fBufferChars:PTextOverlayBufferChars;
       fBufferCharsBuffers:TTextOverlayBufferCharsBuffers;
       fCountBufferChars:TVkInt32;
       fCountBufferCharsBuffers:array[0..1] of TVkInt32;
       fIndices:TTextOverlayIndices;
       fTextOverlayVertexShaderModule:TVulkanShaderModule;
       fTextOverlayFragmentShaderModule:TVulkanShaderModule;
       fVulkanPipelineShaderStageTriangleVertex:TVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageTriangleFragment:TVulkanPipelineShaderStage;
       fVulkanPipelineCache:TVulkanPipelineCache;
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
       fFontCharWidth:single;
       fFontCharHeight:single;
       fInvWidth:single;
       fInvHeight:single;
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       procedure Load;
       procedure Unload;
       procedure AfterCreateSwapChain;
       procedure BeforeDestroySwapChain;
       procedure Reset;
       procedure AddText(const pX,pY,pSize:single;const pAlignment:TTextOverlayAlignment;const pText:AnsiString;const pR:single=1.0;const pG:single=1.0;const pB:single=1.0);
       procedure PreUpdate(const pDeltaTime:double);
       procedure PostUpdate(const pDeltaTime:double);
       procedure Draw;
     end;

implementation

uses UnitSDFFont;

constructor TTextOverlay.Create;
var Index:TVkInt32;
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

 fCountBufferCharsBuffers[0]:=0;
 fCountBufferCharsBuffers[1]:=0;

end;

destructor TTextOverlay.Destroy;
begin
 inherited Destroy;
end;

procedure TTextOverlay.Load;
var Stream:TStream;
    Index:TVkInt32;
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

  fVulkanPipelineCache:=TVulkanPipelineCache.Create(VulkanApplication.VulkanDevice);

  fVulkanGraphicsPipeline:=nil;

  fVulkanRenderPass:=nil;

  fFontTexture:=TVulkanTexture.CreateFromMemory(VulkanApplication.VulkanDevice,
                                                VulkanApplication.VulkanGraphicsCommandBuffers[0,0],
                                                VulkanApplication.VulkanGraphicsCommandBufferFences[0,0],
                                                VulkanApplication.VulkanTransferCommandBuffers[0,0],
                                                VulkanApplication.VulkanTransferCommandBufferFences[0,0],
                                                VK_FORMAT_R8_UNORM,
                                                VK_SAMPLE_COUNT_1_BIT,
                                                SDFFontWidth,
                                                SDFFontHeight,
                                                1,
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
                                                     TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
                                                    );
   fVulkanVertexBuffers[Index].UploadData(VulkanApplication.VulkanTransferCommandBuffers[0,0],
                                          VulkanApplication.VulkanTransferCommandBufferFences[0,0],
                                          fBufferCharsBuffers[0],
                                          0,
                                          SizeOf(TTextOverlayBufferChars),
                                          false);
  end;
  
  fVulkanIndexBuffer:=TVulkanBuffer.Create(VulkanApplication.VulkanDevice,
                                           SizeOf(TTextOverlayIndices),
                                           TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT),
                                           TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                           nil,
                                           TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
                                          );
  fVulkanIndexBuffer.UploadData(VulkanApplication.VulkanTransferCommandBuffers[0,0],
                                VulkanApplication.VulkanTransferCommandBufferFences[0,0],
                                fIndices,
                                0,
                                SizeOf(TTextOverlayIndices),
                                true);

  fVulkanUniformBuffer:=TVulkanBuffer.Create(VulkanApplication.VulkanDevice,
                                             SizeOf(TTextOverlayUniformBuffer),
                                             TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT),
                                             TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                             nil,
                                             TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
                                            );
  fVulkanUniformBuffer.UploadData(VulkanApplication.VulkanTransferCommandBuffers[0,0],
                                  VulkanApplication.VulkanTransferCommandBufferFences[0,0],
                                  fUniformBuffer,
                                  0,
                                  SizeOf(TTextOverlayUniformBuffer),
                                  false);

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
var Index:TVkInt32;
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
  FreeAndNil(fVulkanPipelineCache);
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

 fVulkanGraphicsPipeline:=TVulkanGraphicsPipeline.Create(VulkanApplication.VulkanDevice,
                                                         fVulkanPipelineCache,
                                                         0,
                                                         [],
                                                         fVulkanPipelineLayout,
                                                         VulkanApplication.VulkanPresentationSurface.VulkanSwapChainSimpleDirectRenderTarget.RenderPass,
                                                         0,
                                                         nil,
                                                         0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageTriangleVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageTriangleFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputBindingDescription(0,SizeOf(TVkFloat)*(2+3+3),VK_VERTEX_INPUT_RATE_VERTEX);
 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(0,0,VK_FORMAT_R32G32_SFLOAT,SizeOf(TVkFloat)*0);
 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(1,0,VK_FORMAT_R32G32B32_SFLOAT,SizeOf(TVkFloat)*2);
 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(2,0,VK_FORMAT_R32G32B32_SFLOAT,SizeOf(TVkFloat)*(2+3));

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,VulkanApplication.VulkanPresentationSurface.Width,VulkanApplication.VulkanPresentationSurface.Height,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,VulkanApplication.VulkanPresentationSurface.Width,VulkanApplication.VulkanPresentationSurface.Height);

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
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_OP_ADD,
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

{fVulkanSwapChainSimpleDirectRenderTarget:=TVulkanSwapChainSimpleDirectRenderTarget.Create(VulkanApplication.VulkanDevice,
                                                                                           VulkanApplication.VulkanPresentationSurface.VulkanSwapChain,
                                                                                           VulkanApplication.VulkanPresentCommandBuffers[0,0],
                                                                                           VulkanApplication.VulkanPresentCommandBufferFences[0,0],
                                                                                           VulkanApplication.VulkanGraphicsCommandBuffers[0,0],
                                                                                           VulkanApplication.VulkanGraphicsCommandBufferFences[0,0],
                                                                                           VK_FORMAT_UNDEFINED,
                                                                                           false,
                                                                                           false);}

 fVulkanRenderPass:=TVulkanRenderPass.Create(VulkanApplication.VulkanDevice);

 fVulkanRenderPass.AddSubpassDescription(0,
                                         VK_PIPELINE_BIND_POINT_GRAPHICS,
                                         [],
                                         [fVulkanRenderPass.AddAttachmentReference(fVulkanRenderPass.AddAttachmentDescription(0,
                                                                                                                              VulkanApplication.VulkanPresentationSurface.VulkanSwapChain.ImageFormat,
                                                                                                                              VK_SAMPLE_COUNT_1_BIT,
                                                                                                                              VK_ATTACHMENT_LOAD_OP_LOAD,
                                                                                                                              VK_ATTACHMENT_STORE_OP_STORE,
                                                                                                                              VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                                                                              VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                              VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, //VK_IMAGE_LAYOUT_UNDEFINED, // VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                                                                                                              VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL //VK_IMAGE_LAYOUT_PRESENT_SRC_KHR  // VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                                                                                                                             ),
                                                                             VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                                                                            )],
                                         [],
                                         fVulkanRenderPass.AddAttachmentReference(fVulkanRenderPass.AddAttachmentDescription(0,
                                                                                                                             VulkanApplication.VulkanPresentationSurface.VulkanSwapChainSimpleDirectRenderTarget.DepthImageFormat,
                                                                                                                             VK_SAMPLE_COUNT_1_BIT,
                                                                                                                             VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                                                                             VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                             VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                                                                             VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                             VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, // VK_IMAGE_LAYOUT_UNDEFINED, // VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                                                                                                             VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                                                                                            ),
                                                                                  VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                                                 ),
                                         []);

 fVulkanRenderPass.Initialize;

 fFontCharWidth:=VulkanApplication.Width/160.0;
 fFontCharHeight:=fFontCharWidth*2.0;

 fInvWidth:=1.0/VulkanApplication.Width;
 fInvHeight:=1.0/VulkanApplication.Height;

 if assigned(fVulkanUniformBuffer) then begin
  fUniformBuffer.uThreshold:=(SDFFontSpreadScale/sqrt(sqr(fFontCharWidth)+sqr(fFontCharHeight)))*1.0;
  fVulkanUniformBuffer.UploadData(VulkanApplication.VulkanTransferCommandBuffers[0,0],
                                  VulkanApplication.VulkanTransferCommandBufferFences[0,0],
                                  fUniformBuffer,
                                  0,
                                  SizeOf(TTextOverlayUniformBuffer),
                                  false);

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

procedure TTextOverlay.AddText(const pX,pY,pSize:single;const pAlignment:TTextOverlayAlignment;const pText:AnsiString;const pR:single=1.0;const pG:single=1.0;const pB:single=1.0);
var Index,EdgeIndex:TVkInt32;
    BufferChar:PTextOverlayBufferChar;
    CurrentChar:byte;
    cX:single;
begin
 case pAlignment of
  toaLeft:begin
   cX:=pX;
  end;
  toaCenter:begin
   cX:=pX-((length(pText)*fFontCharWidth*pSize)*0.5);
  end;
  else {toaRight:}begin
   cX:=pX-(length(pText)*fFontCharWidth*pSize);
  end;
 end;
 for Index:=1 to length(pText) do begin
  CurrentChar:=Byte(AnsiChar(pText[Index]));
  if CurrentChar<>32 then begin
   if fCountBufferChars<TextOverlayBufferCharSize then begin
    BufferChar:=@fBufferChars^[fCountBufferChars];
    inc(fCountBufferChars);
    for EdgeIndex:=0 to 3 do begin
     BufferChar^.Vertices[EdgeIndex].x:=(((cX+((EdgeIndex and 1)*fFontCharWidth*pSize))*fInvWidth)*2.0)-1.0;
     BufferChar^.Vertices[EdgeIndex].y:=(((pY+((EdgeIndex shr 1)*fFontCharHeight*pSize))*fInvHeight)*2.0)-1.0;
     BufferChar^.Vertices[EdgeIndex].u:=EdgeIndex and 1;
     BufferChar^.Vertices[EdgeIndex].v:=EdgeIndex shr 1;
     BufferChar^.Vertices[EdgeIndex].w:=CurrentChar;
     BufferChar^.Vertices[EdgeIndex].r:=pR;
     BufferChar^.Vertices[EdgeIndex].g:=pG;
     BufferChar^.Vertices[EdgeIndex].b:=pB;
    end;
   end;
  end;
  cX:=cX+(fFontCharWidth*pSize);
 end;
end;

procedure TTextOverlay.PreUpdate(const pDeltaTime:double);
var s:string;
begin
 fUpdateBufferIndex:=VulkanApplication.FrameCounter and 1;
 fBufferChars:=@fBufferCharsBuffers[fUpdateBufferIndex];
 begin
  Reset;
  AddText(0.0,fFontCharHeight*0.0,1.0,toaLeft,'Device: '+VulkanApplication.VulkanDevice.PhysicalDevice.DeviceName);
  AddText(0.0,fFontCharHeight*1.0,1.0,toaLeft,'Vulkan API version: '+IntToStr(VulkanApplication.VulkanDevice.PhysicalDevice.Properties.apiVersion shr 22)+'.'+IntToStr((VulkanApplication.VulkanDevice.PhysicalDevice.Properties.apiVersion shr 12) and $3ff)+'.'+IntToStr((VulkanApplication.VulkanDevice.PhysicalDevice.Properties.apiVersion shr 0) and $fff));
  Str(VulkanApplication.FramesPerSecond:1:1,s);
  AddText(0.0,fFontCharHeight*2.0,1.0,toaLeft,'Frame rate: '+s+' FPS');
 end;
end;

procedure TTextOverlay.PostUpdate(const pDeltaTime:double);
begin
 fCountBufferCharsBuffers[fUpdateBufferIndex]:=fCountBufferChars;
end;

procedure TTextOverlay.Draw;
const Offsets:array[0..0] of TVkDeviceSize=(0);
var BufferIndex,CurrentImageIndex:TVkInt32;
    VulkanVertexBuffer:TVulkanBuffer;
    VulkanCommandBuffer:TVulkanCommandBuffer;
    VulkanSwapChain:TVulkanSwapChain;
    ImageMemoryBarrier:TVkImageMemoryBarrier;
begin

 BufferIndex:=(VulkanApplication.FrameCounter+1) and 1;
 if fCountBufferCharsBuffers[BufferIndex]>0 then begin

  CurrentImageIndex:=VulkanApplication.VulkanPresentationSurface.CurrentImageIndex;

  VulkanVertexBuffer:=fVulkanVertexBuffers[CurrentImageIndex];

  VulkanVertexBuffer.UploadData(VulkanApplication.VulkanTransferCommandBuffers[0,0],
                                VulkanApplication.VulkanTransferCommandBufferFences[0,0],
                                fBufferCharsBuffers[BufferIndex],
                                0,
                                SizeOf(TTextOverlayBufferChar)*fCountBufferCharsBuffers[BufferIndex],
                                false);

  if assigned(fVulkanGraphicsPipeline) then begin

   VulkanCommandBuffer:=fVulkanRenderCommandBuffers[CurrentImageIndex];
   VulkanSwapChain:=VulkanApplication.VulkanPresentationSurface.VulkanSwapChain;

   VulkanCommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));

   VulkanCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));

{  // A non-layout-change image memory barrier, otherwise the text overlay can be flicker at least on some GPUs
   FillChar(ImageMemoryBarrier,SizeOf(TVkImageMemoryBarrier),#0);
   ImageMemoryBarrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
   ImageMemoryBarrier.oldLayout:=VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
   ImageMemoryBarrier.newLayout:=VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
   ImageMemoryBarrier.image:=VulkanApplication.VulkanPresentationSurface.VulkanSwapChain.Images[CurrentImageIndex].Handle;
   ImageMemoryBarrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
   ImageMemoryBarrier.subresourceRange.baseMipLevel:=0;
   ImageMemoryBarrier.subresourceRange.levelCount:=1;
   ImageMemoryBarrier.subresourceRange.baseArrayLayer:=0;
   ImageMemoryBarrier.subresourceRange.layerCount:=1;
   ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
   ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT);
   VulkanCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                          TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                          0,
                                          0,
                                          nil,
                                          0,
                                          nil,
                                          1,
                                          @ImageMemoryBarrier);}

   fVulkanRenderPass.BeginRenderPass(VulkanCommandBuffer,
                                     VulkanApplication.VulkanPresentationSurface.VulkanSwapChainSimpleDirectRenderTarget.FrameBuffer,
                                     VK_SUBPASS_CONTENTS_INLINE,
                                     0,
                                     0,
                                     VulkanSwapChain.Width,
                                     VulkanSwapChain.Height);

   VulkanCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanPipelineLayout.Handle,0,1,@fVulkanDescriptorSet.Handle,0,nil);
   VulkanCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
   VulkanCommandBuffer.CmdBindVertexBuffers(0,1,@VulkanVertexBuffer.Handle,@Offsets);
   VulkanCommandBuffer.CmdBindVertexBuffers(1,1,@VulkanVertexBuffer.Handle,@Offsets);
   VulkanCommandBuffer.CmdBindVertexBuffers(2,1,@VulkanVertexBuffer.Handle,@Offsets);
   VulkanCommandBuffer.CmdBindIndexBuffer(fVulkanIndexBuffer.Handle,0,VK_INDEX_TYPE_UINT32);
   VulkanCommandBuffer.CmdDrawIndexed(fCountBufferCharsBuffers[BufferIndex]*6,1,0,0,1);

   fVulkanRenderPass.EndRenderPass(VulkanCommandBuffer);

   VulkanCommandBuffer.EndRecording;

   VulkanCommandBuffer.Execute(VulkanApplication.VulkanDevice.GraphicsQueue,
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
                               VulkanApplication.VulkanPresentationSurface.VulkanLastWaitSemaphore,
                               fVulkanRenderSemaphores[CurrentImageIndex],
                               nil,
                               false);
   VulkanApplication.VulkanPresentationSurface.VulkanLastWaitSemaphore:=fVulkanRenderSemaphores[CurrentImageIndex];

  end;
 end;
end;

end.
