unit UnitScreenExampleTriangle;
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

type TScreenExampleTriangle=class(TVulkanScreen)
      private
       fTriangleVertexShaderModule:TVulkanShaderModule;
       fTriangleFragmentShaderModule:TVulkanShaderModule;
       fVulkanPipelineShaderStageTriangleVertex:TVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageTriangleFragment:TVulkanPipelineShaderStage;
       fVulkanPipelineCache:TVulkanPipelineCache;
       fVulkanPipelineLayout:TVulkanPipelineLayout;
       fVulkanGraphicsPipeline:TVulkanGraphicsPipeline;
       fVulkanSwapChainSimpleDirectRenderTarget:TVulkanSwapChainSimpleDirectRenderTarget;
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

       procedure Update(const pDeltaTime:double); override;

       procedure Draw; override;

     end;

implementation

constructor TScreenExampleTriangle.Create;
var Stream:TStream;
begin
 inherited Create;

 Stream:=VulkanApplication.Assets.GetAsset('shaders/triangle_vert.spv');
 try
  fTriangleVertexShaderModule:=TVulkanShaderModule.Create(VulkanApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=VulkanApplication.Assets.GetAsset('shaders/triangle_frag.spv');
 try
  fTriangleFragmentShaderModule:=TVulkanShaderModule.Create(VulkanApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageTriangleVertex:=TVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fTriangleVertexShaderModule,'main');

 fVulkanPipelineShaderStageTriangleFragment:=TVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fTriangleFragmentShaderModule,'main');

 fVulkanPipelineCache:=TVulkanPipelineCache.Create(VulkanApplication.VulkanDevice);

 fVulkanPipelineLayout:=TVulkanPipelineLayout.Create(VulkanApplication.VulkanDevice);
 fVulkanPipelineLayout.Initialize;

 fVulkanGraphicsPipeline:=nil;

 fVulkanSwapChainSimpleDirectRenderTarget:=nil;

end;

destructor TScreenExampleTriangle.Destroy;
begin
 FreeAndNil(fVulkanSwapChainSimpleDirectRenderTarget);
 FreeAndNil(fVulkanGraphicsPipeline);
 FreeAndNil(fVulkanPipelineLayout);
 FreeAndNil(fVulkanPipelineCache);
 FreeAndNil(fVulkanPipelineShaderStageTriangleVertex);
 FreeAndNil(fVulkanPipelineShaderStageTriangleFragment);
 FreeAndNil(fTriangleFragmentShaderModule);
 FreeAndNil(fTriangleVertexShaderModule);
 inherited Destroy;
end;

procedure TScreenExampleTriangle.Show;
begin
 inherited Show;
end;

procedure TScreenExampleTriangle.Hide;
begin
 inherited Hide;
end;

procedure TScreenExampleTriangle.Resume;
begin
 inherited Resume;
end;

procedure TScreenExampleTriangle.Pause;
begin
 inherited Pause;
end;

procedure TScreenExampleTriangle.Resize(const pWidth,pHeight:TSDLInt32);
begin
 inherited Resize(pWidth,pHeight);
end;

procedure TScreenExampleTriangle.AfterCreateSwapChain;
begin
 inherited AfterCreateSwapChain;

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
                                                                                           VulkanApplication.VulkanPresentationSurface.VulkanInitializationCommandBuffer,
                                                                                           VulkanApplication.VulkanPresentationSurface.VulkanInitializationCommandBufferFence,
                                                                                           VK_FORMAT_UNDEFINED,
                                                                                           false);

 fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.ClearValues[0].color.float32[0]:=0.0;
 fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.ClearValues[0].color.float32[1]:=0.0;
 fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.ClearValues[0].color.float32[2]:=0.0;

end;

procedure TScreenExampleTriangle.BeforeDestroySwapChain;
begin
 FreeAndNil(fVulkanSwapChainSimpleDirectRenderTarget);
 FreeAndNil(fVulkanGraphicsPipeline);
 inherited BeforeDestroySwapChain;
end;

function TScreenExampleTriangle.HandleEvent(const pEvent:TSDL_Event):boolean;
begin
 result:=inherited HandleEvent(pEvent);
end;

procedure TScreenExampleTriangle.Update(const pDeltaTime:double);
begin
 inherited Update(pDeltaTime);
end;

procedure TScreenExampleTriangle.Draw;
var VulkanCommandBuffer:TVulkanCommandBuffer;
    VulkanSwapChain:TVulkanSwapChain;
begin
 inherited Draw;
 if assigned(fVulkanGraphicsPipeline) then begin

  VulkanCommandBuffer:=VulkanApplication.VulkanPresentationSurface.VulkanCommandBuffer;
  VulkanSwapChain:=VulkanApplication.VulkanPresentationSurface.VulkanSwapChain;

  fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.BeginRenderPass(VulkanCommandBuffer,
                                                                      fVulkanSwapChainSimpleDirectRenderTarget.FrameBuffer,
                                                                      VK_SUBPASS_CONTENTS_INLINE,
                                                                      0,
                                                                      0,
                                                                      VulkanSwapChain.Width,
                                                                      VulkanSwapChain.Height);

  VulkanCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
  VulkanCommandBuffer.CmdDraw(3,1,0,0);

  fVulkanSwapChainSimpleDirectRenderTarget.RenderPass.EndRenderPass(VulkanCommandBuffer);

 end;
end;

end.
