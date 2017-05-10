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

     PTextOverlayIndices=^TTextOverlayIndices;
     TTextOverlayIndices=array[0..(TextOverlayBufferCharSize*6)-1] of TVkInt32;

     PTextOverlayUniformBuffer=^TTextOverlayUniformBuffer;
     TTextOverlayUniformBuffer=record
      Dummy:TVkInt32;
     end;

     TTextOverlayAlignment=
      (
       toaLeft,
       toaCenter,
       toaRight
      );

     TTextOverlay=class
      private
       fLoaded:boolean;
       fBufferChars:TTextOverlayBufferChars;
       fCountBufferChars:TVkInt32;
       fIndices:TTextOverlayIndices;
       fTextOverlayVertexShaderModule:TVulkanShaderModule;
       fTextOverlayFragmentShaderModule:TVulkanShaderModule;
       fVulkanPipelineShaderStageTriangleVertex:TVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageTriangleFragment:TVulkanPipelineShaderStage;
       fVulkanPipelineCache:TVulkanPipelineCache;
       fVulkanGraphicsPipeline:TVulkanGraphicsPipeline;
       fVulkanSwapChainSimpleDirectRenderTarget:TVulkanSwapChainSimpleDirectRenderTarget;
       fVulkanVertexBuffer:TVulkanBuffer;
       fVulkanIndexBuffer:TVulkanBuffer;
       fVulkanUniformBuffer:TVulkanBuffer;
       fVulkanDescriptorPool:TVulkanDescriptorPool;
       fVulkanDescriptorSetLayout:TVulkanDescriptorSetLayout;
       fVulkanDescriptorSet:TVulkanDescriptorSet;
       fVulkanPipelineLayout:TVulkanPipelineLayout;
       fUniformBuffer:TTextOverlayUniformBuffer;
       fFontTexture:TVulkanTexture;
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       procedure Load;
       procedure Unload;
       procedure AfterCreateSwapChain;
       procedure BeforeDestroySwapChain;
       procedure Reset;
       procedure AddText(const pX,pY:single;const pAlignment:TTextOverlayAlignment;const pText:AnsiString;const pR:single=1.0;const pG:single=1.0;const pB:single=1.0);
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
  fIndices[(Index*6)+3]:=(Index*4)+2;
  fIndices[(Index*6)+4]:=(Index*4)+3;
  fIndices[(Index*6)+5]:=(Index*4)+0;
 end;

 fCountBufferChars:=0;

end;

destructor TTextOverlay.Destroy;
begin
 inherited Destroy;
end;

procedure TTextOverlay.Load;
var Stream:TStream;
begin

 if not fLoaded then begin

  fLoaded:=true;

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

  fVulkanSwapChainSimpleDirectRenderTarget:=nil;

  fFontTexture:=TVulkanTexture.CreateFromMemory(VulkanApplication.VulkanDevice,
                                                VulkanApplication.VulkanGraphicsCommandBuffers[0,0],
                                                VulkanApplication.VulkanGraphicsCommandBufferFences[0,0],
                                                VulkanApplication.VulkanTransferCommandBuffers[0,0],
                                                VulkanApplication.VulkanTransferCommandBufferFences[0,0],
                                                VK_FORMAT_R16_UNORM,
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
  fFontTexture.UpdateSampler;                                             

  fVulkanVertexBuffer:=TVulkanBuffer.Create(VulkanApplication.VulkanDevice,
                                            SizeOf(TTextOverlayBufferChars),
                                            TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT),
                                            TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                            nil,
                                            TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
                                           );
  fVulkanVertexBuffer.UploadData(VulkanApplication.VulkanTransferCommandBuffers[0,0],
                                 VulkanApplication.VulkanTransferCommandBufferFences[0,0],
                                 fBufferChars,
                                 0,
                                 SizeOf(TTextOverlayBufferChars),
                                 false);

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
                                             TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
                                            );
  fVulkanUniformBuffer.UploadData(VulkanApplication.VulkanTransferCommandBuffers[0,0],
                                  VulkanApplication.VulkanTransferCommandBufferFences[0,0],
                                  fUniformBuffer,
                                  0,
                                  SizeOf(TTextOverlayUniformBuffer),
                                  true);

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
                                        TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT),
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
begin

 if fLoaded then begin

  fLoaded:=false;

  FreeAndNil(fVulkanPipelineLayout);
  FreeAndNil(fVulkanDescriptorSet);
  FreeAndNil(fVulkanDescriptorSetLayout);
  FreeAndNil(fVulkanDescriptorPool);
  FreeAndNil(fVulkanUniformBuffer);
  FreeAndNil(fVulkanIndexBuffer);
  FreeAndNil(fVulkanVertexBuffer);
  FreeAndNil(fVulkanSwapChainSimpleDirectRenderTarget);
  FreeAndNil(fVulkanGraphicsPipeline);
  FreeAndNil(fVulkanPipelineCache);
  FreeAndNil(fVulkanPipelineShaderStageTriangleVertex);
  FreeAndNil(fVulkanPipelineShaderStageTriangleFragment);
  FreeAndNil(fTextOverlayFragmentShaderModule);
  FreeAndNil(fTextOverlayVertexShaderModule);
  FreeAndNil(fFontTexture);

 end;
end;

procedure TTextOverlay.AfterCreateSwapChain;
begin

 FreeAndNil(fVulkanSwapChainSimpleDirectRenderTarget);
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

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

 fVulkanSwapChainSimpleDirectRenderTarget:=TVulkanSwapChainSimpleDirectRenderTarget.Create(VulkanApplication.VulkanDevice,
                                                                                           VulkanApplication.VulkanPresentationSurface.VulkanSwapChain,
                                                                                           VulkanApplication.VulkanPresentCommandBuffers[0,0],
                                                                                           VulkanApplication.VulkanPresentCommandBufferFences[0,0],
                                                                                           VulkanApplication.VulkanGraphicsCommandBuffers[0,0],
                                                                                           VulkanApplication.VulkanGraphicsCommandBufferFences[0,0],
                                                                                           VK_FORMAT_UNDEFINED,
                                                                                           false);

 fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.ClearValues[0].color.float32[0]:=0.0;
 fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.ClearValues[0].color.float32[1]:=0.0;
 fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.ClearValues[0].color.float32[2]:=0.0;

end;

procedure TTextOverlay.BeforeDestroySwapChain;
begin
 FreeAndNil(fVulkanSwapChainSimpleDirectRenderTarget);
 FreeAndNil(fVulkanGraphicsPipeline);
end;

procedure TTextOverlay.Reset;
begin
 fCountBufferChars:=0;
end;

procedure TTextOverlay.AddText(const pX,pY:single;const pAlignment:TTextOverlayAlignment;const pText:AnsiString;const pR:single=1.0;const pG:single=1.0;const pB:single=1.0);
var Index,EdgeIndex:TVkInt32;
    BufferChar:PTextOverlayBufferChar;
    CurrentChar:byte;
    FontCharWidth,FontCharHeight,InvWidth,InvHeight,cX:single;
begin
 InvWidth:=1.0/VulkanApplication.Width;
 InvHeight:=1.0/VulkanApplication.Height;
 FontCharWidth:=VulkanApplication.Width/24.0;
 FontCharHeight:=VulkanApplication.Height/80.0;
 case pAlignment of
  toaLeft:begin
   cX:=pX;
  end;
  toaCenter:begin
   cX:=pX-((length(pText)*FontCharWidth)*0.5);
  end;
  else {toaRight:}begin
   cX:=pX-(length(pText)*FontCharWidth);
  end;
 end;
 for Index:=1 to length(pText) do begin
  CurrentChar:=Byte(AnsiChar(pText[Index]));
  if CurrentChar<>32 then begin
   if fCountBufferChars<TextOverlayBufferCharSize then begin
    BufferChar:=@fBufferChars[fCountBufferChars];
    inc(fCountBufferChars);
    for EdgeIndex:=0 to 3 do begin
     BufferChar^.Vertices[EdgeIndex].x:=(cX+((EdgeIndex and 1)*FontCharWidth))*InvWidth;
     BufferChar^.Vertices[EdgeIndex].y:=(pY+((EdgeIndex shr 1)*FontCharHeight))*InvHeight;
     BufferChar^.Vertices[EdgeIndex].u:=EdgeIndex and 1;
     BufferChar^.Vertices[EdgeIndex].v:=EdgeIndex shr 1;
     BufferChar^.Vertices[EdgeIndex].w:=CurrentChar;
     BufferChar^.Vertices[EdgeIndex].r:=pR;
     BufferChar^.Vertices[EdgeIndex].g:=pG;
     BufferChar^.Vertices[EdgeIndex].b:=pB;
    end;
   end;
  end;
  cX:=cX+FontCharWidth;
 end;
end;

procedure TTextOverlay.Draw;
begin
 if fCountBufferChars>0 then begin
  fVulkanVertexBuffer.UploadData(VulkanApplication.VulkanTransferCommandBuffers[0,0],
                                 VulkanApplication.VulkanTransferCommandBufferFences[0,0],
                                 fBufferChars,
                                 0,
                                 SizeOf(TTextOverlayBufferChar)*fCountBufferChars,
                                 false);
 end;
end;

end.
