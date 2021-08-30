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
     PasVulkan.Application;

type { TSkyBox }
     TSkyBox=class
      private
       fVertexShaderModule:TpvVulkanShaderModule;
       fFragmentShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
       fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fVulkanDescriptorSet:TpvVulkanDescriptorSet;
       fVulkanPipelineLayout:TpvVulkanPipelineLayout;
       fVulkanPipeline:TpvVulkanGraphicsPipeline;
      public

       constructor Create(const aSkyCubeMap:TVkDescriptorImageInfo;const aRenderPass:TpvVulkanRenderPass;const aWidth,aHeight:TpvInt32;const aVulkanSampleCountFlagBits:TVkSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT));

       destructor Destroy; override;

       procedure Draw(const aCommandBuffer:TpvVulkanCommandBuffer;const aViewMatrix,aProjectionMatrix:TpvMatrix4x4);

     end;

implementation

{ TSkyBox }

constructor TSkyBox.Create(const aSkyCubeMap:TVkDescriptorImageInfo;const aRenderPass:TpvVulkanRenderPass;const aWidth,aHeight:TpvInt32;const aVulkanSampleCountFlagBits:TVkSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT));
var Stream:TStream;
begin
 inherited Create;

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
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),1);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,1);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSet:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                       fVulkanDescriptorSetLayout);
 fVulkanDescriptorSet.WriteToDescriptorSet(0,
                                           0,
                                           1,
                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                           [aSkyCubeMap],
                                           [],
                                           [],
                                           false);
 fVulkanDescriptorSet.Flush;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddPushConstantRange(TVkPipelineStageFlags(VK_SHADER_STAGE_VERTEX_BIT),0,SizeOf(TpvMatrix4x4));
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

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

destructor TSkyBox.Destroy;
begin
 FreeAndNil(fVulkanPipeline);
 FreeAndNil(fVulkanPipelineLayout);
 FreeAndNil(fVulkanDescriptorSet);
 FreeAndNil(fVulkanDescriptorPool);
 FreeAndNil(fVulkanDescriptorSetLayout);
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVertexShaderModule);
 FreeAndNil(fFragmentShaderModule);
 inherited Destroy;
end;

procedure TSkyBox.Draw(const aCommandBuffer:TpvVulkanCommandBuffer;const aViewMatrix,aProjectionMatrix:TpvMatrix4x4);
var ViewMatrix,ViewProjectionMatrix:TpvMatrix4x4;
begin
 ViewMatrix:=aViewMatrix;
 ViewMatrix.RawComponents[0,3]:=0.0;
 ViewMatrix.RawComponents[1,3]:=0.0;
 ViewMatrix.RawComponents[2,3]:=0.0;
 ViewMatrix.RawComponents[3,0]:=0.0;
 ViewMatrix.RawComponents[3,1]:=0.0;
 ViewMatrix.RawComponents[3,2]:=0.0;
 ViewProjectionMatrix:=ViewMatrix*aProjectionMatrix;
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanPipeline.Handle);
 aCommandBuffer.CmdPushConstants(fVulkanPipelineLayout.Handle,
                                 TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_VERTEX_BIT),
                                 0,
                                 SizeOf(TpvMatrix4x4),@ViewProjectionMatrix);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      fVulkanPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSet.Handle,
                                      0,
                                      nil);
 aCommandBuffer.CmdDraw(36,1,0,0);
end;

end.
