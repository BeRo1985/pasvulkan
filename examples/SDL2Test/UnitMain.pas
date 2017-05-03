unit UnitMain;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$if defined(Win32) or defined(Win64)}
 {$define Windows}
{$ifend}

interface

uses {$if defined(Unix)}
      BaseUnix,Unix,UnixType,ctypes,
     {$ifend}
     SysUtils,Classes,Math,
     Vulkan,
     PasVulkan,
     UnitSDL2,
     UnitVulkanPresentationSurface,
     UnitGlobals;

type TMain=class
      private
       fTriangleVertexShaderModule:TVulkanShaderModule;
       fTriangleFragmentShaderModule:TVulkanShaderModule;
       fVulkanPipelineShaderStageTriangleVertex:TVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageTriangleFragment:TVulkanPipelineShaderStage;
       fVulkanPipelineCache:TVulkanPipelineCache;
       fVulkanPipelineLayout:TVulkanPipelineLayout;
       fVulkanGraphicsPipeline:TVulkanGraphicsPipeline;
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       procedure OnAfterCreateSwapChain(const pSurface:TVulkanPresentationSurface);
       procedure OnBeforeDestroySwapChain(const pSurface:TVulkanPresentationSurface);
       procedure StartGraphics;
       procedure StopGraphics;
       procedure ResizeGraphics(NewWidth,NewHeight:TSDLInt32);
       procedure DrawGraphics(const pVulkanCommandBuffer:TVulkanCommandBuffer);
     end;

var Main:TMain=nil;

implementation

constructor TMain.Create;
begin
 inherited Create;

 fTriangleVertexShaderModule:=TVulkanShaderModule.Create(VulkanDevice,'triangle_vert.spv');

 fTriangleFragmentShaderModule:=TVulkanShaderModule.Create(VulkanDevice,'triangle_frag.spv');

 fVulkanPipelineShaderStageTriangleVertex:=TVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fTriangleVertexShaderModule,'main');

 fVulkanPipelineShaderStageTriangleFragment:=TVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fTriangleFragmentShaderModule,'main');

 fVulkanPipelineCache:=TVulkanPipelineCache.Create(VulkanDevice);

 fVulkanPipelineLayout:=TVulkanPipelineLayout.Create(VulkanDevice);
 fVulkanPipelineLayout.Initialize;

end;

destructor TMain.Destroy;
begin
 FreeAndNil(fVulkanGraphicsPipeline);
 FreeAndNil(fVulkanPipelineLayout);
 FreeAndNil(fVulkanPipelineCache);
 FreeAndNil(fVulkanPipelineShaderStageTriangleVertex);
 FreeAndNil(fVulkanPipelineShaderStageTriangleFragment);
 FreeAndNil(fTriangleFragmentShaderModule);
 FreeAndNil(fTriangleVertexShaderModule);
 inherited Destroy;
end;

procedure TMain.OnAfterCreateSwapChain(const pSurface:TVulkanPresentationSurface);
begin
 FreeAndNil(fVulkanGraphicsPipeline);

 fVulkanGraphicsPipeline:=TVulkanGraphicsPipeline.Create(VulkanDevice,
                                                         fVulkanPipelineCache,
                                                         0,
                                                         [],
                                                         fVulkanPipelineLayout,
                                                         VulkanPresentationSurface.VulkanSwapChainSimpleDirectRenderTarget.RenderPass,
                                                         0,
                                                         nil,
                                                         0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageTriangleVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageTriangleFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,VulkanPresentationSurface.Width,VulkanPresentationSurface.Height,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,VulkanPresentationSurface.Width,VulkanPresentationSurface.Height);

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

end;

procedure TMain.OnBeforeDestroySwapChain(const pSurface:TVulkanPresentationSurface);
begin
 FreeAndNil(fVulkanGraphicsPipeline);
end;

procedure TMain.StartGraphics;
begin
end;

procedure TMain.StopGraphics;
begin
end;

procedure TMain.ResizeGraphics(NewWidth,NewHeight:TSDLInt32);
begin
end;

procedure TMain.DrawGraphics(const pVulkanCommandBuffer:TVulkanCommandBuffer);
begin
 if assigned(fVulkanGraphicsPipeline) then begin
  pVulkanCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
  pVulkanCommandBuffer.CmdDraw(3,1,0,0);
 end;
end;

end.
