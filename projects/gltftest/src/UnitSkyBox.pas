unit UnitSkyBox;
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
     PasVulkan.Application,
     PasVulkan.Scene3D;

type { TSkyBox }
     TSkyBox=class
      private
       fScene3D:TpvScene3D;
       fVertexShaderModule:TpvVulkanShaderModule;
       fFragmentShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
       fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
       fVulkanPipelineLayout:TpvVulkanPipelineLayout;
       fVulkanPipeline:TpvVulkanGraphicsPipeline;
      public

       constructor Create(const aScene3D:TpvScene3D;const aSkyCubeMap:TVkDescriptorImageInfo);

       destructor Destroy; override;

       procedure AfterCreateSwapChain(const aRenderPass:TpvVulkanRenderPass;
                                      const aWidth:TpvInt32;
                                      const aHeight:TpvInt32;
                                      const aVulkanSampleCountFlagBits:TVkSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT));

       procedure BeforeDestroySwapChain;

       procedure Draw(const aInFlightFrameIndex,aViewBaseIndex,aCountViews:TpvSizeInt;const aCommandBuffer:TpvVulkanCommandBuffer);

     end;

implementation

{ TSkyBox }

constructor TSkyBox.Create(const aScene3D:TpvScene3D;const aSkyCubeMap:TVkDescriptorImageInfo);
var Index:TpvSizeInt;
    Stream:TStream;
begin
 inherited Create;

 fScene3D:=aScene3D;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/skybox_vert.spv');
 try
  fVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/skybox_frag.spv');
 try
  fFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fFragmentShaderModule,'main');

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
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

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),length(fVulkanDescriptorSets));
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,length(fVulkanDescriptorSets));
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,length(fVulkanDescriptorSets));
 fVulkanDescriptorPool.Initialize;

 for Index:=0 to length(fVulkanDescriptorSets)-1 do begin
  fVulkanDescriptorSets[Index]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                              fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[Index].WriteToDescriptorSet(0,
                                                    0,
                                                    1,
                                                    TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                    [],
                                                    [fScene3D.GlobalVulkanViewUniformBuffers[Index].DescriptorBufferInfo],
                                                    [],
                                                    false);
  fVulkanDescriptorSets[Index].WriteToDescriptorSet(1,
                                                    0,
                                                    1,
                                                    TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                    [aSkyCubeMap],
                                                    [],
                                                    [],
                                                    false);
  fVulkanDescriptorSets[Index].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddPushConstantRange(TVkPipelineStageFlags(VK_SHADER_STAGE_VERTEX_BIT),0,SizeOf(TpvScene3D.TVertexStagePushConstants));
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

end;

destructor TSkyBox.Destroy;
var Index:TpvSizeInt;
begin
 FreeAndNil(fVulkanPipelineLayout);
 for Index:=0 to length(fVulkanDescriptorSets)-1 do begin
  FreeAndNil(fVulkanDescriptorSets[Index]);
 end;
 FreeAndNil(fVulkanDescriptorPool);
 FreeAndNil(fVulkanDescriptorSetLayout);
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVertexShaderModule);
 FreeAndNil(fFragmentShaderModule);
 inherited Destroy;
end;

procedure TSkyBox.AfterCreateSwapChain(const aRenderPass:TpvVulkanRenderPass;
                                       const aWidth:TpvInt32;
                                       const aHeight:TpvInt32;
                                       const aVulkanSampleCountFlagBits:TVkSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT));
begin

 fVulkanPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                   pvApplication.VulkanPipelineCache,
                                                   0,
                                                   [],
                                                   fVulkanPipelineLayout,
                                                   aRenderPass,
                                                   0,
                                                   nil,
                                                   0);

 fVulkanPipeline.AddStage(fVulkanPipelineShaderStageVertex);
 fVulkanPipeline.AddStage(fVulkanPipelineShaderStageFragment);

 fVulkanPipeline.InputAssemblyState.Topology:=TVkPrimitiveTopology(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
 fVulkanPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanPipeline.ViewPortState.AddViewPort(0.0,0.0,aWidth,aHeight,0.0,1.0);
 fVulkanPipeline.ViewPortState.AddScissor(0,0,aWidth,aHeight);

 fVulkanPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
 fVulkanPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_CLOCKWISE;
 fVulkanPipeline.RasterizationState.DepthBiasEnable:=false;
 fVulkanPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
 fVulkanPipeline.RasterizationState.DepthBiasClamp:=0.0;
 fVulkanPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
 fVulkanPipeline.RasterizationState.LineWidth:=1.0;

 fVulkanPipeline.MultisampleState.RasterizationSamples:=aVulkanSampleCountFlagBits;
 fVulkanPipeline.MultisampleState.SampleShadingEnable:=false;
 fVulkanPipeline.MultisampleState.MinSampleShading:=0.0;
 fVulkanPipeline.MultisampleState.CountSampleMasks:=0;
 fVulkanPipeline.MultisampleState.AlphaToCoverageEnable:=false;
 fVulkanPipeline.MultisampleState.AlphaToOneEnable:=false;

 fVulkanPipeline.ColorBlendState.LogicOpEnable:=false;
 fVulkanPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
 fVulkanPipeline.ColorBlendState.BlendConstants[0]:=0.0;
 fVulkanPipeline.ColorBlendState.BlendConstants[1]:=0.0;
 fVulkanPipeline.ColorBlendState.BlendConstants[2]:=0.0;
 fVulkanPipeline.ColorBlendState.BlendConstants[3]:=0.0;
 fVulkanPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
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

 fVulkanPipeline.DepthStencilState.DepthTestEnable:=false;
 fVulkanPipeline.DepthStencilState.DepthWriteEnable:=false;
 fVulkanPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fVulkanPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanPipeline.Initialize;

 fVulkanPipeline.FreeMemory;

end;

procedure TSkyBox.BeforeDestroySwapChain;
begin
 FreeAndNil(fVulkanPipeline);
end;

procedure TSkyBox.Draw(const aInFlightFrameIndex,aViewBaseIndex,aCountViews:TpvSizeInt;const aCommandBuffer:TpvVulkanCommandBuffer);
var VertexStagePushConstants:TpvScene3D.TVertexStagePushConstants;
begin
 VertexStagePushConstants.ViewBaseIndex:=aViewBaseIndex;
 VertexStagePushConstants.CountViews:=aCountViews;
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanPipeline.Handle);
 aCommandBuffer.CmdPushConstants(fVulkanPipelineLayout.Handle,
                                 TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_VERTEX_BIT),
                                 0,
                                 SizeOf(TpvScene3D.TVertexStagePushConstants),
                                 @VertexStagePushConstants);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      fVulkanPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,
                                      0,
                                      nil);
 aCommandBuffer.CmdDraw(36,1,0,0);
end;

end.
