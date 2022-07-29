unit UnitScreenMain;
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

{$scopedenums on}

{$define UseMomentBasedOrderIndependentTransparency}

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasMP,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Resources,
     PasVulkan.FrameGraph,
     PasVulkan.Scene3D,
     UnitGlobals,
     UnitOrderIndependentTransparencyBuffer,
     UnitOrderIndependentTransparencyImage,
     UnitMipmappedArray2DImage,
     UnitSkyCubeMap,
     UnitGGXBRDF,
     UnitGGXEnvMapCubeMap,
     UnitCharlieBRDF,
     UnitCharlieEnvMapCubeMap,
     UnitLambertianEnvMapCubeMap,
     UnitSkyBox,
     UnitSMAAData;

type { TScreenMain }
     TScreenMain=class(TpvApplicationScreen)
      public
        const CountCascadedShadowMapCascades=4;
              CountOrderIndependentTransparencyLayers=8;
        type TCameraMode=(
              Orbit,
              FirstPerson
             );
             PCameraMode=^TCameraMode;
             { TCascadedShadowMap }
             TCascadedShadowMap=record
              public
               View:TpvScene3D.TView;
               CombinedMatrix:TpvMatrix4x4;
               SplitDepths:TpvVector3;
               Scales:TpvVector2;
             end;
             { TLockOrderIndependentTransparentViewPort }
             TLockOrderIndependentTransparentViewPort=packed record
              x:TpvInt32;
              y:TpvInt32;
              z:TpvInt32;
              w:TpvInt32;
             end;
             { TLockOrderIndependentTransparentUniformBuffer }
             TLockOrderIndependentTransparentUniformBuffer=packed record
              ViewPort:TLockOrderIndependentTransparentViewPort;
             end;
             { TLoopOrderIndependentTransparentViewPort }
             TLoopOrderIndependentTransparentViewPort=packed record
              x:TpvInt32;
              y:TpvInt32;
              z:TpvInt32;
              w:TpvInt32;
             end;
             { TLoopOrderIndependentTransparentUniformBuffer }
             TLoopOrderIndependentTransparentUniformBuffer=packed record
              ViewPort:TLoopOrderIndependentTransparentViewPort;
             end;
             { TApproximationOrderIndependentTransparentUniformBuffer }
             TApproximationOrderIndependentTransparentUniformBuffer=packed record
              ZNearZFar:TpvVector4;
             end;
             PCascadedShadowMap=^TCascadedShadowMap;
             TCascadedShadowMaps=array[0..CountCascadedShadowMapCascades-1] of TCascadedShadowMap;
             PCascadedShadowMaps=^TCascadedShadowMaps;
             TInFlightFrameCascadedShadowMaps=array[0..MaxInFlightFrames-1] of TCascadedShadowMaps;
             TCascadedShadowMapUniformBuffer=packed record
              Matrices:array[0..CountCascadedShadowMapCascades-1] of TpvMatrix4x4;
              SplitDepths:array[0..CountCascadedShadowMapCascades-1] of TpvVector4; // actually TpvVector2 but because of alignment it is a TpvVector4 here
             end;
             PCascadedShadowMapUniformBuffer=^TCascadedShadowMapUniformBuffer;
             TCascadedShadowMapUniformBuffers=array[0..MaxInFlightFrames-1] of TCascadedShadowMapUniformBuffer;
             TCascadedShadowMapVulkanUniformBuffers=array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
             { TMeshComputePass }
             TMeshComputePass=class(TpvFrameGraph.TComputePass)
              private
               fParent:TScreenMain;
               fResourceInput:TpvFrameGraph.TPass.TUsedImageResource;
               fComputeShaderModule:TpvVulkanShaderModule;
               fVulkanPipelineShaderStageCompute:TpvVulkanPipelineShaderStage;
               fPipelineLayout:TpvVulkanPipelineLayout;
               fPipeline:TpvVulkanComputePipeline;
               fEvents:array[0..MaxInFlightFrames-1] of TpvVulkanEvent;
               fEventReady:array[0..MaxInFlightFrames-1] of boolean;
              public
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TDepthVelocityNormalsRenderPass }
             TDepthVelocityNormalsRenderPass=class(TpvFrameGraph.TRenderPass)
              private
               fOnSetRenderPassResourcesDone:boolean;
               procedure OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                  const aPipelineLayout:TpvVulkanPipelineLayout;
                                                  const aRenderPassIndex:TpvSizeInt;
                                                  const aPreviousInFlightFrameIndex:TpvSizeInt;
                                                  const aInFlightFrameIndex:TpvSizeInt);
              public
               fVulkanRenderPass:TpvVulkanRenderPass;
               fParent:TScreenMain;
               fResourceVelocity:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceNormals:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceDepth:TpvFrameGraph.TPass.TUsedImageResource;
               fVulkanGraphicsCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanGraphicsCommandBufferFence:TpvVulkanFence;
               fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanTransferCommandBufferFence:TpvVulkanFence;
               fMeshVertexShaderModule:TpvVulkanShaderModule;
               fMeshDepthFragmentShaderModule:TpvVulkanShaderModule;
               fMeshDepthMaskedFragmentShaderModule:TpvVulkanShaderModule;
               fVulkanPipelineShaderStageMeshVertex:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshDepthFragment:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshDepthMaskedFragment:TpvVulkanPipelineShaderStage;
               fVulkanGraphicsPipelines:array[TpvScene3D.TMaterial.TAlphaMode] of TpvScene3D.TGraphicsPipelines;
               fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TDepthMipMapComputePass }
             TDepthMipMapComputePass=class(TpvFrameGraph.TComputePass)
              private
               fParent:TScreenMain;
               fResourceInput:TpvFrameGraph.TPass.TUsedImageResource;
               fDownsampleLevel0ComputeShaderModule:TpvVulkanShaderModule;
               fDownsampleLevel1ComputeShaderModule:TpvVulkanShaderModule;
               fVulkanSampler:TpvVulkanSampler;
               fVulkanImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
               fVulkanPipelineShaderStageDownsampleLevel0Compute:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageDownsampleLevel1Compute:TpvVulkanPipelineShaderStage;
               fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
               fVulkanDescriptorPool:TpvVulkanDescriptorPool;
               fVulkanDescriptorSets:array[0..MaxInFlightFrames-1,0..15] of TpvVulkanDescriptorSet;
               fPipelineLayout:TpvVulkanPipelineLayout;
               fPipelineLevel0:TpvVulkanComputePipeline;
               fPipelineLevel1:TpvVulkanComputePipeline;
              public
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TCascadedShadowMapRenderPass }
             TCascadedShadowMapRenderPass=class(TpvFrameGraph.TRenderPass)
              public
               fVulkanRenderPass:TpvVulkanRenderPass;
               fParent:TScreenMain;
               fResourceDepth:TpvFrameGraph.TPass.TUsedImageResource;
               fVulkanGraphicsCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanGraphicsCommandBufferFence:TpvVulkanFence;
               fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanTransferCommandBufferFence:TpvVulkanFence;
               fMeshVertexShaderModule:TpvVulkanShaderModule;
               fMeshFragmentShaderModule:TpvVulkanShaderModule;
               fMeshMaskedFragmentShaderModule:TpvVulkanShaderModule;
               fVulkanPipelineShaderStageMeshVertex:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshFragment:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshMaskedFragment:TpvVulkanPipelineShaderStage;
               fVulkanGraphicsPipelines:array[TpvScene3D.TMaterial.TAlphaMode] of TpvScene3D.TGraphicsPipelines;
               fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TCascadedShadowMapResolveRenderPass }
             TCascadedShadowMapResolveRenderPass=class(TpvFrameGraph.TRenderPass)
              private
               type TFragmentStagePushConstants=record
                     CountSamples:Int32;
                    end;
              private
               fParent:TScreenMain;
               fVulkanRenderPass:TpvVulkanRenderPass;
               fResourceInput:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceOutput:TpvFrameGraph.TPass.TUsedImageResource;
               fVulkanSampler:TpvVulkanSampler;
               fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanTransferCommandBufferFence:TpvVulkanFence;
               fVulkanVertexShaderModule:TpvVulkanShaderModule;
               fVulkanFragmentShaderModule:TpvVulkanShaderModule;
               fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
               fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
               fVulkanDescriptorPool:TpvVulkanDescriptorPool;
               fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
               fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
               fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               fFragmentStagePushConstants:TFragmentStagePushConstants;
              public
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TCascadedShadowMapBlurRenderPass }
             TCascadedShadowMapBlurRenderPass=class(TpvFrameGraph.TRenderPass)
              private
               type TFragmentStagePushConstants=record
                     Direction:TpvVector4;
                    end;
              private
               fParent:TScreenMain;
               fVulkanRenderPass:TpvVulkanRenderPass;
               fResourceInput:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceOutput:TpvFrameGraph.TPass.TUsedImageResource;
               fVulkanSampler:TpvVulkanSampler;
               fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanTransferCommandBufferFence:TpvVulkanFence;
               fVulkanVertexShaderModule:TpvVulkanShaderModule;
               fVulkanFragmentShaderModule:TpvVulkanShaderModule;
               fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
               fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
               fVulkanDescriptorPool:TpvVulkanDescriptorPool;
               fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
               fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
               fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               fFragmentStagePushConstants:TFragmentStagePushConstants;
              public
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain;const aHorziontal:boolean); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TSSAORenderPass }
             TSSAORenderPass=class(TpvFrameGraph.TRenderPass)
              public
               type TSSAOPushConstants=record
                     ViewBaseIndex:UInt32;
                     CountViews:UInt32;
                     FrameIndex:UInt32;
                    end;
              private
               fParent:TScreenMain;
               fVulkanRenderPass:TpvVulkanRenderPass;
               fResourceNormals:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceOutput:TpvFrameGraph.TPass.TUsedImageResource;
               fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanTransferCommandBufferFence:TpvVulkanFence;
               fVulkanVertexShaderModule:TpvVulkanShaderModule;
               fVulkanFragmentShaderModule:TpvVulkanShaderModule;
               fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
               fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
               fVulkanSampler:TpvVulkanSampler;
               fVulkanDescriptorPool:TpvVulkanDescriptorPool;
               fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
               fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
               fVulkanPipelineLayout:TpvVulkanPipelineLayout;
              public
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TSSAOBlurRenderPass }
             TSSAOBlurRenderPass=class(TpvFrameGraph.TRenderPass)
              private
               type TFragmentStagePushConstants=record
                     Direction:TpvVector4;
                    end;
              private
               fParent:TScreenMain;
               fVulkanRenderPass:TpvVulkanRenderPass;
               fResourceInput:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceOutput:TpvFrameGraph.TPass.TUsedImageResource;
               fVulkanSampler:TpvVulkanSampler;
               fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanTransferCommandBufferFence:TpvVulkanFence;
               fVulkanVertexShaderModule:TpvVulkanShaderModule;
               fVulkanFragmentShaderModule:TpvVulkanShaderModule;
               fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
               fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
               fVulkanDescriptorPool:TpvVulkanDescriptorPool;
               fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
               fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
               fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               fFragmentStagePushConstants:TFragmentStagePushConstants;
              public
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain;const aHorziontal:boolean); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TForwardRenderPass }
             TForwardRenderPass=class(TpvFrameGraph.TRenderPass)
              private
               fOnSetRenderPassResourcesDone:boolean;
               procedure OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                  const aPipelineLayout:TpvVulkanPipelineLayout;
                                                  const aRenderPassIndex:TpvSizeInt;
                                                  const aPreviousInFlightFrameIndex:TpvSizeInt;
                                                  const aInFlightFrameIndex:TpvSizeInt);
              public
               fVulkanRenderPass:TpvVulkanRenderPass;
               fParent:TScreenMain;
               fResourceCascadedShadowMap:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceSSAO:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceDepth:TpvFrameGraph.TPass.TUsedImageResource;
               fVulkanGraphicsCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanGraphicsCommandBufferFence:TpvVulkanFence;
               fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanTransferCommandBufferFence:TpvVulkanFence;
               fMeshVertexShaderModule:TpvVulkanShaderModule;
               fMeshFragmentShaderModule:TpvVulkanShaderModule;
               fMeshMaskedFragmentShaderModule:TpvVulkanShaderModule;
               fMeshDepthFragmentShaderModule:TpvVulkanShaderModule;
               fMeshDepthMaskedFragmentShaderModule:TpvVulkanShaderModule;
               fGlobalVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
               fGlobalVulkanDescriptorPool:TpvVulkanDescriptorPool;
               fGlobalVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
               fVulkanCascadedShadowMapSampler:TpvVulkanSampler;
               fVulkanSSAOSampler:TpvVulkanSampler;
               fVulkanPipelineShaderStageMeshVertex:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshFragment:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshMaskedFragment:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshDepthFragment:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshDepthMaskedFragment:TpvVulkanPipelineShaderStage;
               fVulkanGraphicsPipelines:array[boolean,TpvScene3D.TMaterial.TAlphaMode] of TpvScene3D.TGraphicsPipelines;
               fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               fSkyBox:TSkyBox;
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TForwardRenderMipMapComputePass }
             TForwardRenderMipMapComputePass=class(TpvFrameGraph.TComputePass)
              private
               fParent:TScreenMain;
               fResourceInput:TpvFrameGraph.TPass.TUsedImageResource;
               fDownsampleLevel0ComputeShaderModule:TpvVulkanShaderModule;
               fDownsampleLevel1ComputeShaderModule:TpvVulkanShaderModule;
               fDownsampleLevel2ComputeShaderModule:TpvVulkanShaderModule;
               fVulkanSampler:TpvVulkanSampler;
               fVulkanImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
               fVulkanPipelineShaderStageDownsampleLevel0Compute:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageDownsampleLevel1Compute:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageDownsampleLevel2Compute:TpvVulkanPipelineShaderStage;
               fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
               fVulkanDescriptorPool:TpvVulkanDescriptorPool;
               fVulkanDescriptorSets:array[0..MaxInFlightFrames-1,0..15] of TpvVulkanDescriptorSet;
               fPipelineLayout:TpvVulkanPipelineLayout;
               fPipelineLevel0:TpvVulkanComputePipeline;
               fPipelineLevel1:TpvVulkanComputePipeline;
               fPipelineLevel2:TpvVulkanComputePipeline;
              public
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TDirectTransparencyRenderPass }
             TDirectTransparencyRenderPass=class(TpvFrameGraph.TRenderPass)
              private
               fOnSetRenderPassResourcesDone:boolean;
               procedure OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                  const aPipelineLayout:TpvVulkanPipelineLayout;
                                                  const aRenderPassIndex:TpvSizeInt;
                                                  const aPreviousInFlightFrameIndex:TpvSizeInt;
                                                  const aInFlightFrameIndex:TpvSizeInt);
              private
               fVulkanRenderPass:TpvVulkanRenderPass;
               fParent:TScreenMain;
               fResourceCascadedShadowMap:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceSSAO:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceDepth:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
               fVulkanGraphicsCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanGraphicsCommandBufferFence:TpvVulkanFence;
               fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanTransferCommandBufferFence:TpvVulkanFence;
               fMeshVertexShaderModule:TpvVulkanShaderModule;
               fMeshFragmentShaderModule:TpvVulkanShaderModule;
               fMeshMaskedFragmentShaderModule:TpvVulkanShaderModule;
               fGlobalVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
               fGlobalVulkanDescriptorPool:TpvVulkanDescriptorPool;
               fGlobalVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
               fVulkanCascadedShadowMapSampler:TpvVulkanSampler;
               fVulkanSSAOSampler:TpvVulkanSampler;
               fVulkanPipelineShaderStageMeshVertex:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshFragment:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshMaskedFragment:TpvVulkanPipelineShaderStage;
               fVulkanGraphicsPipelines:array[TpvScene3D.TMaterial.TAlphaMode] of TpvScene3D.TGraphicsPipelines;
               fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TDirectTransparencyResolveRenderPass }
             TDirectTransparencyResolveRenderPass=class(TpvFrameGraph.TRenderPass)
               private
                fParent:TScreenMain;
                fVulkanRenderPass:TpvVulkanRenderPass;
                fResourceOpaque:TpvFrameGraph.TPass.TUsedImageResource;
                fResourceTransparent:TpvFrameGraph.TPass.TUsedImageResource;
                fResourceSurface:TpvFrameGraph.TPass.TUsedImageResource;
                fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
                fVulkanTransferCommandBufferFence:TpvVulkanFence;
                fVulkanVertexShaderModule:TpvVulkanShaderModule;
                fVulkanFragmentShaderModule:TpvVulkanShaderModule;
                fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
                fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
                fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
                fVulkanDescriptorPool:TpvVulkanDescriptorPool;
                fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
                fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
                fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               public
                constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
                destructor Destroy; override;
                procedure Show; override;
                procedure Hide; override;
                procedure AfterCreateSwapChain; override;
                procedure BeforeDestroySwapChain; override;
                procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
                procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TLockOrderIndependentTransparencyClearCustomPass }
             TLockOrderIndependentTransparencyClearCustomPass=class(TpvFrameGraph.TCustomPass)
              private
               fParent:TScreenMain;
              public
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TLockOrderIndependentTransparencyRenderPass }
             TLockOrderIndependentTransparencyRenderPass=class(TpvFrameGraph.TRenderPass)
              private
               fOnSetRenderPassResourcesDone:boolean;
               procedure OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                  const aPipelineLayout:TpvVulkanPipelineLayout;
                                                  const aRenderPassIndex:TpvSizeInt;
                                                  const aPreviousInFlightFrameIndex:TpvSizeInt;
                                                  const aInFlightFrameIndex:TpvSizeInt);
              private
               fVulkanRenderPass:TpvVulkanRenderPass;
               fParent:TScreenMain;
               fResourceCascadedShadowMap:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceSSAO:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceDepth:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
               fVulkanGraphicsCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanGraphicsCommandBufferFence:TpvVulkanFence;
               fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanTransferCommandBufferFence:TpvVulkanFence;
               fMeshVertexShaderModule:TpvVulkanShaderModule;
               fMeshFragmentShaderModule:TpvVulkanShaderModule;
               fMeshMaskedFragmentShaderModule:TpvVulkanShaderModule;
               fGlobalVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
               fGlobalVulkanDescriptorPool:TpvVulkanDescriptorPool;
               fGlobalVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
               fVulkanCascadedShadowMapSampler:TpvVulkanSampler;
               fVulkanSSAOSampler:TpvVulkanSampler;
               fVulkanPipelineShaderStageMeshVertex:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshFragment:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshMaskedFragment:TpvVulkanPipelineShaderStage;
               fVulkanGraphicsPipelines:array[TpvScene3D.TMaterial.TAlphaMode] of TpvScene3D.TGraphicsPipelines;
               fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TLockOrderIndependentTransparencyBarrierCustomPass }
             TLockOrderIndependentTransparencyBarrierCustomPass=class(TpvFrameGraph.TCustomPass)
              private
               fParent:TScreenMain;
              public
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TLockOrderIndependentTransparencyResolveRenderPass }
             TLockOrderIndependentTransparencyResolveRenderPass=class(TpvFrameGraph.TRenderPass)
               private
                fParent:TScreenMain;
                fVulkanRenderPass:TpvVulkanRenderPass;
                fResourceOpaque:TpvFrameGraph.TPass.TUsedImageResource;
                fResourceTransparent:TpvFrameGraph.TPass.TUsedImageResource;
                fResourceSurface:TpvFrameGraph.TPass.TUsedImageResource;
                fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
                fVulkanTransferCommandBufferFence:TpvVulkanFence;
                fVulkanVertexShaderModule:TpvVulkanShaderModule;
                fVulkanFragmentShaderModule:TpvVulkanShaderModule;
                fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
                fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
                fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
                fVulkanDescriptorPool:TpvVulkanDescriptorPool;
                fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
                fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
                fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               public
                constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
                destructor Destroy; override;
                procedure Show; override;
                procedure Hide; override;
                procedure AfterCreateSwapChain; override;
                procedure BeforeDestroySwapChain; override;
                procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
                procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TLoopOrderIndependentTransparencyClearCustomPass }
             TLoopOrderIndependentTransparencyClearCustomPass=class(TpvFrameGraph.TCustomPass)
              private
               fParent:TScreenMain;
              public
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TLoopOrderIndependentTransparencyPass1RenderPass }
             TLoopOrderIndependentTransparencyPass1RenderPass=class(TpvFrameGraph.TRenderPass)
              private
               fOnSetRenderPassResourcesDone:boolean;
               procedure OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                  const aPipelineLayout:TpvVulkanPipelineLayout;
                                                  const aRenderPassIndex:TpvSizeInt;
                                                  const aPreviousInFlightFrameIndex:TpvSizeInt;
                                                  const aInFlightFrameIndex:TpvSizeInt);
              private
               fVulkanRenderPass:TpvVulkanRenderPass;
               fParent:TScreenMain;
               fResourceCascadedShadowMap:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceSSAO:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceDepth:TpvFrameGraph.TPass.TUsedImageResource;
//             fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
               fVulkanGraphicsCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanGraphicsCommandBufferFence:TpvVulkanFence;
               fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanTransferCommandBufferFence:TpvVulkanFence;
               fMeshVertexShaderModule:TpvVulkanShaderModule;
               fMeshFragmentShaderModule:TpvVulkanShaderModule;
               fMeshMaskedFragmentShaderModule:TpvVulkanShaderModule;
               fGlobalVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
               fGlobalVulkanDescriptorPool:TpvVulkanDescriptorPool;
               fGlobalVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
               fVulkanCascadedShadowMapSampler:TpvVulkanSampler;
               fVulkanSSAOSampler:TpvVulkanSampler;
               fVulkanPipelineShaderStageMeshVertex:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshFragment:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshMaskedFragment:TpvVulkanPipelineShaderStage;
               fVulkanGraphicsPipelines:array[TpvScene3D.TMaterial.TAlphaMode] of TpvScene3D.TGraphicsPipelines;
               fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TLoopOrderIndependentTransparencyPass1BarrierCustomPass }
             TLoopOrderIndependentTransparencyPass1BarrierCustomPass=class(TpvFrameGraph.TCustomPass)
              private
               fParent:TScreenMain;
              public
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TLoopOrderIndependentTransparencyPass2RenderPass }
             TLoopOrderIndependentTransparencyPass2RenderPass=class(TpvFrameGraph.TRenderPass)
              private
               fOnSetRenderPassResourcesDone:boolean;
               procedure OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                  const aPipelineLayout:TpvVulkanPipelineLayout;
                                                  const aRenderPassIndex:TpvSizeInt;
                                                  const aPreviousInFlightFrameIndex:TpvSizeInt;
                                                  const aInFlightFrameIndex:TpvSizeInt);
              private
               fVulkanRenderPass:TpvVulkanRenderPass;
               fParent:TScreenMain;
               fResourceCascadedShadowMap:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceSSAO:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceDepth:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
               fVulkanGraphicsCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanGraphicsCommandBufferFence:TpvVulkanFence;
               fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanTransferCommandBufferFence:TpvVulkanFence;
               fMeshVertexShaderModule:TpvVulkanShaderModule;
               fMeshFragmentShaderModule:TpvVulkanShaderModule;
               fMeshMaskedFragmentShaderModule:TpvVulkanShaderModule;
               fGlobalVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
               fGlobalVulkanDescriptorPool:TpvVulkanDescriptorPool;
               fGlobalVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
               fVulkanCascadedShadowMapSampler:TpvVulkanSampler;
               fVulkanSSAOSampler:TpvVulkanSampler;
               fVulkanPipelineShaderStageMeshVertex:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshFragment:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshMaskedFragment:TpvVulkanPipelineShaderStage;
               fVulkanGraphicsPipelines:array[TpvScene3D.TMaterial.TAlphaMode] of TpvScene3D.TGraphicsPipelines;
               fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TLoopOrderIndependentTransparencyPass2BarrierCustomPass }
             TLoopOrderIndependentTransparencyPass2BarrierCustomPass=class(TpvFrameGraph.TCustomPass)
              private
               fParent:TScreenMain;
              public
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TLoopOrderIndependentTransparencyResolveRenderPass }
             TLoopOrderIndependentTransparencyResolveRenderPass=class(TpvFrameGraph.TRenderPass)
               private
                fParent:TScreenMain;
                fVulkanRenderPass:TpvVulkanRenderPass;
                fResourceOpaque:TpvFrameGraph.TPass.TUsedImageResource;
                fResourceTransparent:TpvFrameGraph.TPass.TUsedImageResource;
                fResourceSurface:TpvFrameGraph.TPass.TUsedImageResource;
                fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
                fVulkanTransferCommandBufferFence:TpvVulkanFence;
                fVulkanVertexShaderModule:TpvVulkanShaderModule;
                fVulkanFragmentShaderModule:TpvVulkanShaderModule;
                fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
                fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
                fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
                fVulkanDescriptorPool:TpvVulkanDescriptorPool;
                fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
                fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
                fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               public
                constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
                destructor Destroy; override;
                procedure Show; override;
                procedure Hide; override;
                procedure AfterCreateSwapChain; override;
                procedure BeforeDestroySwapChain; override;
                procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
                procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass }
             TMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass=class(TpvFrameGraph.TRenderPass)
              private
               fOnSetRenderPassResourcesDone:boolean;
               procedure OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                  const aPipelineLayout:TpvVulkanPipelineLayout;
                                                  const aRenderPassIndex:TpvSizeInt;
                                                  const aPreviousInFlightFrameIndex:TpvSizeInt;
                                                  const aInFlightFrameIndex:TpvSizeInt);
              private
               fVulkanRenderPass:TpvVulkanRenderPass;
               fParent:TScreenMain;
               fResourceCascadedShadowMap:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceSSAO:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceDepth:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceMoments0:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceMoments1:TpvFrameGraph.TPass.TUsedImageResource;
               fVulkanGraphicsCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanGraphicsCommandBufferFence:TpvVulkanFence;
               fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanTransferCommandBufferFence:TpvVulkanFence;
               fMeshVertexShaderModule:TpvVulkanShaderModule;
               fMeshFragmentShaderModule:TpvVulkanShaderModule;
               fMeshMaskedFragmentShaderModule:TpvVulkanShaderModule;
               fGlobalVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
               fGlobalVulkanDescriptorPool:TpvVulkanDescriptorPool;
               fGlobalVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
               fVulkanCascadedShadowMapSampler:TpvVulkanSampler;
               fVulkanSSAOSampler:TpvVulkanSampler;
               fVulkanPipelineShaderStageMeshVertex:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshFragment:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshMaskedFragment:TpvVulkanPipelineShaderStage;
               fVulkanGraphicsPipelines:array[TpvScene3D.TMaterial.TAlphaMode] of TpvScene3D.TGraphicsPipelines;
               fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TMomentBasedOrderIndependentTransparencyTransmittanceRenderPass }
             TMomentBasedOrderIndependentTransparencyTransmittanceRenderPass=class(TpvFrameGraph.TRenderPass)
              private
               fOnSetRenderPassResourcesDone:boolean;
               procedure OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                  const aPipelineLayout:TpvVulkanPipelineLayout;
                                                  const aRenderPassIndex:TpvSizeInt;
                                                  const aPreviousInFlightFrameIndex:TpvSizeInt;
                                                  const aInFlightFrameIndex:TpvSizeInt);
              private
               fVulkanRenderPass:TpvVulkanRenderPass;
               fParent:TScreenMain;
               fResourceCascadedShadowMap:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceSSAO:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceDepth:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceMoments0:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceMoments1:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
               fVulkanGraphicsCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanGraphicsCommandBufferFence:TpvVulkanFence;
               fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanTransferCommandBufferFence:TpvVulkanFence;
               fMeshVertexShaderModule:TpvVulkanShaderModule;
               fMeshFragmentShaderModule:TpvVulkanShaderModule;
               fMeshMaskedFragmentShaderModule:TpvVulkanShaderModule;
               fGlobalVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
               fGlobalVulkanDescriptorPool:TpvVulkanDescriptorPool;
               fGlobalVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
               fVulkanCascadedShadowMapSampler:TpvVulkanSampler;
               fVulkanSSAOSampler:TpvVulkanSampler;
               fVulkanPipelineShaderStageMeshVertex:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshFragment:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshMaskedFragment:TpvVulkanPipelineShaderStage;
               fVulkanGraphicsPipelines:array[TpvScene3D.TMaterial.TAlphaMode] of TpvScene3D.TGraphicsPipelines;
               fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TMomentBasedOrderIndependentTransparencyResolveRenderPass }
             TMomentBasedOrderIndependentTransparencyResolveRenderPass=class(TpvFrameGraph.TRenderPass)
               private
                fParent:TScreenMain;
                fVulkanRenderPass:TpvVulkanRenderPass;
                fResourceOpaque:TpvFrameGraph.TPass.TUsedImageResource;
                fResourceTransparent:TpvFrameGraph.TPass.TUsedImageResource;
                fResourceMoments0:TpvFrameGraph.TPass.TUsedImageResource;
                fResourceSurface:TpvFrameGraph.TPass.TUsedImageResource;
                fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
                fVulkanTransferCommandBufferFence:TpvVulkanFence;
                fVulkanVertexShaderModule:TpvVulkanShaderModule;
                fVulkanFragmentShaderModule:TpvVulkanShaderModule;
                fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
                fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
                fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
                fVulkanDescriptorPool:TpvVulkanDescriptorPool;
                fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
                fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
                fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               public
                constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
                destructor Destroy; override;
                procedure Show; override;
                procedure Hide; override;
                procedure AfterCreateSwapChain; override;
                procedure BeforeDestroySwapChain; override;
                procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
                procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TWeightBlendedOrderIndependentTransparencyRenderPass }
             TWeightBlendedOrderIndependentTransparencyRenderPass=class(TpvFrameGraph.TRenderPass)
              private
               fOnSetRenderPassResourcesDone:boolean;
               procedure OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                  const aPipelineLayout:TpvVulkanPipelineLayout;
                                                  const aRenderPassIndex:TpvSizeInt;
                                                  const aPreviousInFlightFrameIndex:TpvSizeInt;
                                                  const aInFlightFrameIndex:TpvSizeInt);
              private
               fVulkanRenderPass:TpvVulkanRenderPass;
               fParent:TScreenMain;
               fResourceCascadedShadowMap:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceSSAO:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceDepth:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceAccumulation:TpvFrameGraph.TPass.TUsedImageResource;
               fResourceRevealage:TpvFrameGraph.TPass.TUsedImageResource;
               fVulkanGraphicsCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanGraphicsCommandBufferFence:TpvVulkanFence;
               fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
               fVulkanTransferCommandBufferFence:TpvVulkanFence;
               fMeshVertexShaderModule:TpvVulkanShaderModule;
               fMeshFragmentShaderModule:TpvVulkanShaderModule;
               fMeshMaskedFragmentShaderModule:TpvVulkanShaderModule;
               fGlobalVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
               fGlobalVulkanDescriptorPool:TpvVulkanDescriptorPool;
               fGlobalVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
               fVulkanCascadedShadowMapSampler:TpvVulkanSampler;
               fVulkanSSAOSampler:TpvVulkanSampler;
               fVulkanPipelineShaderStageMeshVertex:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshFragment:TpvVulkanPipelineShaderStage;
               fVulkanPipelineShaderStageMeshMaskedFragment:TpvVulkanPipelineShaderStage;
               fVulkanGraphicsPipelines:array[TpvScene3D.TMaterial.TAlphaMode] of TpvScene3D.TGraphicsPipelines;
               fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
               destructor Destroy; override;
               procedure Show; override;
               procedure Hide; override;
               procedure AfterCreateSwapChain; override;
               procedure BeforeDestroySwapChain; override;
               procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TWeightBlendedOrderIndependentTransparencyResolveRenderPass }
             TWeightBlendedOrderIndependentTransparencyResolveRenderPass=class(TpvFrameGraph.TRenderPass)
               private
                fParent:TScreenMain;
                fVulkanRenderPass:TpvVulkanRenderPass;
                fResourceOpaque:TpvFrameGraph.TPass.TUsedImageResource;
                fResourceAccumlation:TpvFrameGraph.TPass.TUsedImageResource;
                fResourceRevealage:TpvFrameGraph.TPass.TUsedImageResource;
                fResourceSurface:TpvFrameGraph.TPass.TUsedImageResource;
                fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
                fVulkanTransferCommandBufferFence:TpvVulkanFence;
                fVulkanVertexShaderModule:TpvVulkanShaderModule;
                fVulkanFragmentShaderModule:TpvVulkanShaderModule;
                fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
                fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
                fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
                fVulkanDescriptorPool:TpvVulkanDescriptorPool;
                fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
                fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
                fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               public
                constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
                destructor Destroy; override;
                procedure Show; override;
                procedure Hide; override;
                procedure AfterCreateSwapChain; override;
                procedure BeforeDestroySwapChain; override;
                procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
                procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
             end;
             { TTonemappingRenderPass }
             TTonemappingRenderPass=class(TpvFrameGraph.TRenderPass)
               private
                fParent:TScreenMain;
                fVulkanRenderPass:TpvVulkanRenderPass;
                fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
                fResourceSurface:TpvFrameGraph.TPass.TUsedImageResource;
                fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
                fVulkanTransferCommandBufferFence:TpvVulkanFence;
                fVulkanVertexShaderModule:TpvVulkanShaderModule;
                fVulkanFragmentShaderModule:TpvVulkanShaderModule;
                fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
                fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
                fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
                fVulkanDescriptorPool:TpvVulkanDescriptorPool;
                fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
                fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
                fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               public
                constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
                destructor Destroy; override;
                procedure Show; override;
                procedure Hide; override;
                procedure AfterCreateSwapChain; override;
                procedure BeforeDestroySwapChain; override;
                procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
                procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
              end;
              { TAntialiasingNoneRenderPass }
              TAntialiasingNoneRenderPass=class(TpvFrameGraph.TRenderPass)
               private
                fParent:TScreenMain;
                fVulkanRenderPass:TpvVulkanRenderPass;
                fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
                fResourceSurface:TpvFrameGraph.TPass.TUsedImageResource;
                fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
                fVulkanTransferCommandBufferFence:TpvVulkanFence;
                fVulkanVertexShaderModule:TpvVulkanShaderModule;
                fVulkanFragmentShaderModule:TpvVulkanShaderModule;
                fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
                fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
                fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
                fVulkanSampler:TpvVulkanSampler;
                fVulkanDescriptorPool:TpvVulkanDescriptorPool;
                fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
                fVulkanImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
                fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
                fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               public
                constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
                destructor Destroy; override;
                procedure Show; override;
                procedure Hide; override;
                procedure AfterCreateSwapChain; override;
                procedure BeforeDestroySwapChain; override;
                procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
                procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
              end;
              { TAntialiasingDSAARenderPass }
              TAntialiasingDSAARenderPass=class(TpvFrameGraph.TRenderPass)
               private
                fParent:TScreenMain;
                fVulkanRenderPass:TpvVulkanRenderPass;
                fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
                fResourceSurface:TpvFrameGraph.TPass.TUsedImageResource;
                fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
                fVulkanTransferCommandBufferFence:TpvVulkanFence;
                fVulkanVertexShaderModule:TpvVulkanShaderModule;
                fVulkanFragmentShaderModule:TpvVulkanShaderModule;
                fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
                fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
                fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
                fVulkanSampler:TpvVulkanSampler;
                fVulkanDescriptorPool:TpvVulkanDescriptorPool;
                fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
                fVulkanImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
                fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
                fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               public
                constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
                destructor Destroy; override;
                procedure Show; override;
                procedure Hide; override;
                procedure AfterCreateSwapChain; override;
                procedure BeforeDestroySwapChain; override;
                procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
                procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
              end;
              { TAntialiasingFXAARenderPass }
              TAntialiasingFXAARenderPass=class(TpvFrameGraph.TRenderPass)
               private
                fParent:TScreenMain;
                fVulkanRenderPass:TpvVulkanRenderPass;
                fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
                fResourceSurface:TpvFrameGraph.TPass.TUsedImageResource;
                fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
                fVulkanTransferCommandBufferFence:TpvVulkanFence;
                fVulkanVertexShaderModule:TpvVulkanShaderModule;
                fVulkanFragmentShaderModule:TpvVulkanShaderModule;
                fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
                fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
                fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
                fVulkanSampler:TpvVulkanSampler;
                fVulkanDescriptorPool:TpvVulkanDescriptorPool;
                fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
                fVulkanImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
                fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
                fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               public
                constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
                destructor Destroy; override;
                procedure Show; override;
                procedure Hide; override;
                procedure AfterCreateSwapChain; override;
                procedure BeforeDestroySwapChain; override;
                procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
                procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
              end;
              { TAntialiasingSMAAEdgesRenderPass }
              TAntialiasingSMAAEdgesRenderPass=class(TpvFrameGraph.TRenderPass)
               private
                fParent:TScreenMain;
                fVulkanRenderPass:TpvVulkanRenderPass;
                fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
                fResourceSurface:TpvFrameGraph.TPass.TUsedImageResource;
                fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
                fVulkanTransferCommandBufferFence:TpvVulkanFence;
                fVulkanVertexShaderModule:TpvVulkanShaderModule;
                fVulkanFragmentShaderModule:TpvVulkanShaderModule;
                fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
                fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
                fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
                fVulkanSampler:TpvVulkanSampler;
                fVulkanDescriptorPool:TpvVulkanDescriptorPool;
                fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
                fVulkanImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
                fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
                fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               public
                constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
                destructor Destroy; override;
                procedure Show; override;
                procedure Hide; override;
                procedure AfterCreateSwapChain; override;
                procedure BeforeDestroySwapChain; override;
                procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
                procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
              end;
              { TAntialiasingSMAAWeightsRenderPass }
              TAntialiasingSMAAWeightsRenderPass=class(TpvFrameGraph.TRenderPass)
               private
                fParent:TScreenMain;
                fVulkanRenderPass:TpvVulkanRenderPass;
                fResourceEdges:TpvFrameGraph.TPass.TUsedImageResource;
                fResourceSurface:TpvFrameGraph.TPass.TUsedImageResource;
                fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
                fVulkanTransferCommandBufferFence:TpvVulkanFence;
                fVulkanVertexShaderModule:TpvVulkanShaderModule;
                fVulkanFragmentShaderModule:TpvVulkanShaderModule;
                fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
                fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
                fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
                fVulkanSampler:TpvVulkanSampler;
                fVulkanDescriptorPool:TpvVulkanDescriptorPool;
                fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
                fVulkanImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
                fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
                fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               public
                constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
                destructor Destroy; override;
                procedure Show; override;
                procedure Hide; override;
                procedure AfterCreateSwapChain; override;
                procedure BeforeDestroySwapChain; override;
                procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
                procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
              end;
              { TAntialiasingSMAABlendRenderPass }
              TAntialiasingSMAABlendRenderPass=class(TpvFrameGraph.TRenderPass)
               private
                fParent:TScreenMain;
                fVulkanRenderPass:TpvVulkanRenderPass;
                fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
                fResourceWeights:TpvFrameGraph.TPass.TUsedImageResource;
                fResourceSurface:TpvFrameGraph.TPass.TUsedImageResource;
                fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
                fVulkanTransferCommandBufferFence:TpvVulkanFence;
                fVulkanVertexShaderModule:TpvVulkanShaderModule;
                fVulkanFragmentShaderModule:TpvVulkanShaderModule;
                fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
                fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
                fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
                fVulkanSampler:TpvVulkanSampler;
                fVulkanDescriptorPool:TpvVulkanDescriptorPool;
                fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
                fVulkanImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
                fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
                fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               public
                constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
                destructor Destroy; override;
                procedure Show; override;
                procedure Hide; override;
                procedure AfterCreateSwapChain; override;
                procedure BeforeDestroySwapChain; override;
                procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
                procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
              end;
              { TDitheringRenderPass }
              TDitheringRenderPass=class(TpvFrameGraph.TRenderPass)
               private
                fParent:TScreenMain;
                fVulkanRenderPass:TpvVulkanRenderPass;
                fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
                fResourceSurface:TpvFrameGraph.TPass.TUsedImageResource;
                fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
                fVulkanTransferCommandBufferFence:TpvVulkanFence;
                fVulkanVertexShaderModule:TpvVulkanShaderModule;
                fVulkanFragmentShaderModule:TpvVulkanShaderModule;
                fVulkanPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
                fVulkanPipelineShaderStageFragment:TpvVulkanPipelineShaderStage;
                fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
                fVulkanSampler:TpvVulkanSampler;
                fVulkanDescriptorPool:TpvVulkanDescriptorPool;
                fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
                fVulkanImageViews:array[0..MaxInFlightFrames-1] of TpvVulkanImageView;
                fVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
                fVulkanPipelineLayout:TpvVulkanPipelineLayout;
               public
                constructor Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain); reintroduce;
                destructor Destroy; override;
                procedure Show; override;
                procedure Hide; override;
                procedure AfterCreateSwapChain; override;
                procedure BeforeDestroySwapChain; override;
                procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
                procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
              end;
      public
       type TInFlightFrameState=record
             Ready:TPasMPBool32;
             FinalViewIndex:TpvSizeInt;
             CountViews:TpvSizeInt;
             CascadedShadowMapViewIndex:TpvSizeInt;
             CountCascadedShadowMapViews:TpvSizeInt;
            end;
            PInFlightFrameState=^TInFlightFrameState;
            TInFlightFrameStates=array[0..MaxInFlightFrames+1] of TInFlightFrameState;
      private
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fBufferDeviceAddress:boolean;
       fCountInFlightFrames:TpvSizeInt;
       fMeshFragTypeName:TpvUTF8String;
       fSurfaceMultiviewMask:TpvUInt32;
       fCountSurfaceViews:TpvInt32;
       fTransparencyMode:TTransparencyMode;
       fAntialiasingMode:TAntialiasingMode;
       fVulkanSampleCountFlagBits:TVkSampleCountFlagBits;
       fVulkanShadowMapSampleCountFlagBits:TVkSampleCountFlagBits;
       fCountSurfaceMSAASamples:Int32;
       fCountCascadedShadowMapMSAASamples:Int32;
       fVulkanGraphicsCommandPool:TpvVulkanCommandPool;
       fVulkanGraphicsCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanGraphicsCommandBufferFence:TpvVulkanFence;
       fVulkanTransferCommandPool:TpvVulkanCommandPool;
       fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanTransferCommandBufferFence:TpvVulkanFence;
       fVulkanFlushQueue:TpvVulkanQueue;
       fVulkanFlushCommandPool:TpvVulkanCommandPool;
       fVulkanFlushCommandBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanCommandBuffer;
       fVulkanFlushCommandBufferFences:array[0..MaxInFlightFrames-1] of TpvVulkanFence;
       fVulkanFlushSemaphores:array[0..MaxInFlightFrames-1] of TpvVulkanSemaphore;
       fVulkanRenderSemaphores:array[0..MaxInFlightFrames-1] of TpvVulkanSemaphore;
       fSkyCubeMap:TSkyCubeMap;
       fGGXBRDF:TGGXBRDF;
       fGGXEnvMapCubeMap:TGGXEnvMapCubeMap;
       fCharlieBRDF:TCharlieBRDF;
       fCharlieEnvMapCubeMap:TCharlieEnvMapCubeMap;
       fLambertianEnvMapCubeMap:TLambertianEnvMapCubeMap;
       fSheenELUT:TpvVulkanTexture;
       fScene3D:TpvScene3D;
       fPrimaryDirectionalLight:TpvScene3D.TLight;
       fFrameGraph:TpvFrameGraph;
       fExternalOutputImageData:TpvFrameGraph.TExternalImageData;
       fSMAAAreaTexture:TpvVulkanTexture;
       fSMAASearchTexture:TpvVulkanTexture;
       fMeshComputePass:TMeshComputePass;
       fDepthVelocityNormalsRenderPass:TDepthVelocityNormalsRenderPass;
       fDepthMipmappedArray2DImages:array[0..MaxInFlightFrames-1] of TMipmappedArray2DImage;
       fDepthMipMapComputePass:TDepthMipMapComputePass;
       fCascadedShadowMapRenderPass:TCascadedShadowMapRenderPass;
       fCascadedShadowMapResolveRenderPass:TCascadedShadowMapResolveRenderPass;
       fCascadedShadowMapBlurRenderPasses:array[0..1] of TCascadedShadowMapBlurRenderPass;
       fSSAORenderPass:TSSAORenderPass;
       fSSAOBlurRenderPasses:array[0..1] of TSSAOBlurRenderPass;
       fForwardRenderPass:TForwardRenderPass;
       fForwardMipmappedArray2DImages:array[0..MaxInFlightFrames-1] of TMipmappedArray2DImage;
       fForwardRenderMipMapComputePass:TForwardRenderMipMapComputePass;
       fDirectTransparencyRenderPass:TDirectTransparencyRenderPass;
       fDirectTransparencyResolveRenderPass:TDirectTransparencyResolveRenderPass;
       fLockOrderIndependentTransparencyClearCustomPass:TLockOrderIndependentTransparencyClearCustomPass;
       fLockOrderIndependentTransparencyRenderPass:TLockOrderIndependentTransparencyRenderPass;
       fLockOrderIndependentTransparencyBarrierCustomPass:TLockOrderIndependentTransparencyBarrierCustomPass;
       fLockOrderIndependentTransparencyResolveRenderPass:TLockOrderIndependentTransparencyResolveRenderPass;
       fLoopOrderIndependentTransparencyClearCustomPass:TLoopOrderIndependentTransparencyClearCustomPass;
       fLoopOrderIndependentTransparencyPass1RenderPass:TLoopOrderIndependentTransparencyPass1RenderPass;
       fLoopOrderIndependentTransparencyPass1BarrierCustomPass:TLoopOrderIndependentTransparencyPass1BarrierCustomPass;
       fLoopOrderIndependentTransparencyPass2RenderPass:TLoopOrderIndependentTransparencyPass2RenderPass;
       fLoopOrderIndependentTransparencyPass2BarrierCustomPass:TLoopOrderIndependentTransparencyPass2BarrierCustomPass;
       fLoopOrderIndependentTransparencyResolveRenderPass:TLoopOrderIndependentTransparencyResolveRenderPass;
       fWeightBlendedOrderIndependentTransparencyRenderPass:TWeightBlendedOrderIndependentTransparencyRenderPass;
       fWeightBlendedOrderIndependentTransparencyResolveRenderPass:TWeightBlendedOrderIndependentTransparencyResolveRenderPass;
       fMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass:TMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass;
       fMomentBasedOrderIndependentTransparencyTransmittanceRenderPass:TMomentBasedOrderIndependentTransparencyTransmittanceRenderPass;
       fMomentBasedOrderIndependentTransparencyResolveRenderPass:TMomentBasedOrderIndependentTransparencyResolveRenderPass;
       fTonemappingRenderPass:TTonemappingRenderPass;
       fAntialiasingNoneRenderPass:TAntialiasingNoneRenderPass;
       fAntialiasingDSAARenderPass:TAntialiasingDSAARenderPass;
       fAntialiasingFXAARenderPass:TAntialiasingFXAARenderPass;
       fAntialiasingSMAAEdgesRenderPass:TAntialiasingSMAAEdgesRenderPass;
       fAntialiasingSMAAWeightsRenderPass:TAntialiasingSMAAWeightsRenderPass;
       fAntialiasingSMAABlendRenderPass:TAntialiasingSMAABlendRenderPass;
       fDitheringRenderPass:TDitheringRenderPass;
       fGroup:TpvScene3D.TGroup;
       fGroupInstance:TpvScene3D.TGroup.TInstance;
       fInFlightFrameCascadedShadowMaps:TInFlightFrameCascadedShadowMaps;
       fCascadedShadowMapUniformBuffers:TCascadedShadowMapUniformBuffers;
       fCascadedShadowMapVulkanUniformBuffers:TCascadedShadowMapVulkanUniformBuffers;
       fTime:Double;
       fCameraMode:TCameraMode;
       fCameraRotationX:TpvScalar;
       fCameraRotationY:TpvScalar;
       fZoom:TpvScalar;
       fCameraMatrix:TpvMatrix4x4;
       fCameraSpeed:TpvScalar;
       fInFlightFrameStates:TInFlightFrameStates;
       fUpdateLock:TPasMPCriticalSection;
       fAnimationIndex:TpvInt32;
       fUseOITAlphaTest:boolean;
       fUseDemote:boolean;
       fUseNoDiscard:boolean;
       fUseDepthPrepass:boolean;
       fFOV:TpvFloat;
       fZNear:TpvFloat;
       fZFar:TpvFloat;
       fCountLockOrderIndependentTransparencyLayers:TpvInt32;
       fLockOrderIndependentTransparentUniformBuffer:TLockOrderIndependentTransparentUniformBuffer;
       fLockOrderIndependentTransparentUniformVulkanBuffer:TpvVulkanBuffer;
       fLockOrderIndependentTransparencyABufferBuffers:array[0..MaxInFlightFrames-1] of TOrderIndependentTransparencyBuffer;
       fLockOrderIndependentTransparencyAuxImages:array[0..MaxInFlightFrames-1] of TOrderIndependentTransparencyImage;
       fLockOrderIndependentTransparencySpinLockImages:array[0..MaxInFlightFrames-1] of TOrderIndependentTransparencyImage;
       fCountLoopOrderIndependentTransparencyLayers:TpvInt32;
       fLoopOrderIndependentTransparentUniformBuffer:TLoopOrderIndependentTransparentUniformBuffer;
       fLoopOrderIndependentTransparentUniformVulkanBuffer:TpvVulkanBuffer;
       fLoopOrderIndependentTransparencyABufferBuffers:array[0..MaxInFlightFrames-1] of TOrderIndependentTransparencyBuffer;
       fLoopOrderIndependentTransparencyZBufferBuffers:array[0..MaxInFlightFrames-1] of TOrderIndependentTransparencyBuffer;
       fLoopOrderIndependentTransparencySBufferBuffers:array[0..MaxInFlightFrames-1] of TOrderIndependentTransparencyBuffer;
       fApproximationOrderIndependentTransparentUniformBuffer:TApproximationOrderIndependentTransparentUniformBuffer;
       fApproximationOrderIndependentTransparentUniformVulkanBuffer:TpvVulkanBuffer;
       fKeyLeft:boolean;
       fKeyRight:boolean;
       fKeyForwards:boolean;
       fKeyBackwards:boolean;
       fKeyUp:boolean;
       fKeyDown:boolean;
       fKeyPitchInc:boolean;
       fKeyPitchDec:boolean;
       fKeyYawInc:boolean;
       fKeyYawDec:boolean;
       fKeyRollInc:boolean;
       fKeyRollDec:boolean;
       fCascadedShadowMapSize:TpvInt32;
       fOptimizedNonAlphaFormat:TVkFormat;
       fOldFPS:TpvInt32;
       fFPSTimeAccumulator:TpvDouble;
       procedure CalculateCascadedShadowMaps(const aInFlightFrameIndex:Int32;const aViewLeft,aViewRight:TpvScene3D.TView);
      public

       constructor Create; override;

       destructor Destroy; override;

       procedure Show; override;

       procedure Hide; override;

       procedure Resume; override;

       procedure Pause; override;

       procedure Resize(const aWidth,aHeight:TpvInt32); override;

       procedure AfterCreateSwapChain; override;

       procedure BeforeDestroySwapChain; override;

       function CanBeParallelProcessed:boolean; override;

       procedure Update(const aDeltaTime:TpvDouble); override;

       function IsReadyForDrawOfInFlightFrameIndex(const aInFlightFrameIndex:TpvInt32):boolean; override;

       procedure DrawUpdate(const aInFlightFrameIndex:TpvInt32;const aDeltaTime:TpvDouble);

       procedure Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil); override;

       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;

       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;

       function Scrolled(const aRelativeAmount:TpvVector2):boolean; override;

      published
       property CascadedShadowMapWidth:TpvInt32 read fCascadedShadowMapSize;
       property CascadedShadowMapHeight:TpvInt32 read fCascadedShadowMapSize;
     end;

implementation

uses PasGLTF,
     UnitApplication,
     PasVulkan.Frustum;

{ TScreenMain.TMeshComputePass }

constructor TScreenMain.TMeshComputePass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin
 inherited Create(aFrameGraph);
 fParent:=aParent;
 Name:='MeshComputePass';
end;

destructor TScreenMain.TMeshComputePass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TMeshComputePass.Show;
var Stream:TStream;
begin

 inherited Show;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_comp.spv');
 try
  fComputeShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageCompute:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

end;

procedure TScreenMain.TMeshComputePass.Hide;
begin
 FreeAndNil(fVulkanPipelineShaderStageCompute);
 FreeAndNil(fComputeShaderModule);
 inherited Hide;
end;

procedure TScreenMain.TMeshComputePass.AfterCreateSwapChain;
var Index:TpvSizeInt;
begin

 inherited AfterCreateSwapChain;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TpvScene3D.TMeshComputeStagePushConstants));
 fPipelineLayout.AddDescriptorSetLayout(fParent.fScene3D.MeshComputeVulkanDescriptorSetLayout);
 fPipelineLayout.Initialize;

 fPipeline:=TpvVulkanComputePipeline.Create(pvApplication.VulkanDevice,
                                            pvApplication.VulkanPipelineCache,
                                            0,
                                            fVulkanPipelineShaderStageCompute,
                                            fPipelineLayout,
                                            nil,
                                            0);

 for Index:=0 to fParent.fCountInFlightFrames-1 do begin
  fEvents[Index]:=TpvVulkanEvent.Create(pvApplication.VulkanDevice);
  fEventReady[Index]:=false;
 end;

end;

procedure TScreenMain.TMeshComputePass.BeforeDestroySwapChain;
var Index:TpvSizeInt;
begin
 FreeAndNil(fPipeline);
 FreeAndNil(fPipelineLayout);
 for Index:=0 to fParent.fCountInFlightFrames-1 do begin
  FreeAndNil(fEvents[Index]);
 end;
 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TMeshComputePass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TMeshComputePass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var PreviousInFlightFrameIndex:TpvSizeInt;
    MemoryBarrier:TVkMemoryBarrier;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);
 fParent.fScene3D.UpdateCachedVertices(fPipeline,
                                       aInFlightFrameIndex,
                                       aCommandBuffer,
                                       fPipelineLayout);
 PreviousInFlightFrameIndex:=fFrameGraph.DrawPreviousInFlightFrameIndex;

 if fEventReady[aInFlightFrameIndex] then begin
  Assert(false);
 end;
 aCommandBuffer.CmdSetEvent(fEvents[aInFlightFrameIndex].Handle,
                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_INPUT_BIT) or
                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or
                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT));
 fEventReady[aInFlightFrameIndex]:=true;

 if (aInFlightFrameIndex<>PreviousInFlightFrameIndex) and fEventReady[PreviousInFlightFrameIndex] then begin
  fEventReady[PreviousInFlightFrameIndex]:=false;
  FillChar(MemoryBarrier,SizeOf(TVkMemoryBarrier),#0);
  MemoryBarrier.sType:=VK_STRUCTURE_TYPE_MEMORY_BARRIER;
  MemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
  MemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
  aCommandBuffer.CmdWaitEvents(1,
                               @fEvents[PreviousInFlightFrameIndex].Handle,
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_INPUT_BIT) or
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_INPUT_BIT) or
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                               1,@MemoryBarrier,
                               0,nil,
                               0,nil);
  aCommandBuffer.CmdResetEvent(fEvents[PreviousInFlightFrameIndex].Handle,
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_INPUT_BIT) or
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT));
 end else begin
  FillChar(MemoryBarrier,SizeOf(TVkMemoryBarrier),#0);
  MemoryBarrier.sType:=VK_STRUCTURE_TYPE_MEMORY_BARRIER;
  MemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
  MemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_INPUT_BIT) or
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    1,@MemoryBarrier,
                                    0,nil,
                                    0,nil);
 end;

end;

{ TScreenMain.TDepthVelocityNormalsRenderPass }

constructor TScreenMain.TDepthVelocityNormalsRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin
inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='DepthVelocityNormalsRendering';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin

  fResourceVelocity:=AddImageOutput('resourcetype_velocity',
                                    'resource_velocity_data',
                                    VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                    TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                 TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                    [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                   );

  fResourceNormals:=AddImageOutput('resourcetype_normals',
                                   'resource_normals_data',
                                   VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                   TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                   [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                  );

  fResourceDepth:=AddImageDepthOutput('resourcetype_depth',
                                      'resource_depth_data', // _temporary',
                                      VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                      TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                   TpvVector4.InlineableCreate(IfThen(fParent.fZFar<0.0,0.0,1.0),0.0,0.0,0.0)),
                                      [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                     );

 end else begin

  fResourceVelocity:=AddImageOutput('resourcetype_msaa_velocity',
                                    'resource_forwardrendering_msaa_velocity',
                                    VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                    TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                 TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                    [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                   );

  fResourceVelocity:=AddImageResolveOutput('resourcetype_velocity',
                                           'resource_velocity_data',
                                           'resource_forwardrendering_msaa_velocity',
                                           VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                           TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare,
                                                                        TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                           [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                          );

  fResourceNormals:=AddImageOutput('resourcetype_msaa_normals',
                                   'resource_normals_data_msaa',
                                   VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                   TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                   [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                  );

  fResourceNormals:=AddImageResolveOutput('resourcetype_normals',
                                          'resource_normals_data',
                                          'resource_normals_data_msaa',
                                          VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                          TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare,
                                                                       TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                          [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                         );

  fResourceDepth:=AddImageDepthOutput('resourcetype_msaa_depth',
                                      'resource_msaa_depth_data', //'_temporary',
                                      VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                      TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                   TpvVector4.InlineableCreate(IfThen(fParent.fZFar<0.0,0.0,1.0),0.0,0.0,0.0)),
                                      [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                     );

 end;

end;

destructor TScreenMain.TDepthVelocityNormalsRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TDepthVelocityNormalsRenderPass.Show;
var Index:TpvSizeInt;
    Stream:TStream;
begin
 inherited Show;

 fVulkanGraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.GraphicsQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanGraphicsCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_velocity_vert.spv');
 try
  fMeshVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_depth_velocity_frag.spv');
 try
  fMeshDepthFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fParent.fUseDemote then begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_depth_velocity_masked_demote_frag.spv');
 end else if fParent.fUseNoDiscard then begin
  if fParent.fZFar<0.0 then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_depth_velocity_masked_nodiscard_reversedz_frag.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_depth_velocity_masked_nodiscard_frag.spv');
  end;
 end else begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_depth_velocity_masked_frag.spv');
 end;
 try
  fMeshDepthMaskedFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageMeshVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fMeshVertexShaderModule,'main');

 fVulkanPipelineShaderStageMeshDepthFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshDepthFragmentShaderModule,'main');

 fVulkanPipelineShaderStageMeshDepthMaskedFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshDepthMaskedFragmentShaderModule,'main');

end;

procedure TScreenMain.TDepthVelocityNormalsRenderPass.Hide;
begin

 FreeAndNil(fVulkanPipelineShaderStageMeshVertex);

 FreeAndNil(fVulkanPipelineShaderStageMeshDepthFragment);

 FreeAndNil(fVulkanPipelineShaderStageMeshDepthMaskedFragment);

 FreeAndNil(fMeshVertexShaderModule);

 FreeAndNil(fMeshDepthFragmentShaderModule);

 FreeAndNil(fMeshDepthMaskedFragmentShaderModule);

 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 FreeAndNil(fVulkanGraphicsCommandBufferFence);
 FreeAndNil(fVulkanGraphicsCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TDepthVelocityNormalsRenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
    VulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
begin

 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT),0,SizeOf(TpvScene3D.TVertexStagePushConstants));
 fVulkanPipelineLayout.AddDescriptorSetLayout(fParent.fScene3D.GlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin

  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin

   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin

    VulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                             pvApplication.VulkanPipelineCache,
                                                             0,
                                                             [],
                                                             fVulkanPipelineLayout,
                                                             fVulkanRenderPass,
                                                             VulkanRenderPassSubpassIndex,
                                                             nil,
                                                             0);

    try

     VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshVertex);
     if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask then begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshDepthMaskedFragment);
     end else begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshDepthFragment);
     end;

     VulkanGraphicsPipeline.InputAssemblyState.Topology:=TVkPrimitiveTopology(PrimitiveTopology);
     VulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

     fParent.fScene3D.InitializeGraphicsPipeline(VulkanGraphicsPipeline,true);

     VulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
     VulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

     VulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
     if DoubleSided then begin
      VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
     end else begin
      VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
     end;
     VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

     VulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=fParent.fVulkanSampleCountFlagBits;
     begin
      VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=false;
      VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=0.0;
      VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
      VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=false;
      VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;
     end;

     VulkanGraphicsPipeline.ColorBlendState.LogicOpEnable:=false;
     VulkanGraphicsPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[0]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[1]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[2]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[3]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
                                                                         VK_BLEND_FACTOR_ZERO,
                                                                         VK_BLEND_FACTOR_ZERO,
                                                                         VK_BLEND_OP_ADD,
                                                                         VK_BLEND_FACTOR_ZERO,
                                                                         VK_BLEND_FACTOR_ZERO,
                                                                         VK_BLEND_OP_ADD,
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT));
     VulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
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

     VulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
     VulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=AlphaMode<>TpvScene3D.TMaterial.TAlphaMode.Blend;
     if fParent.fZFar<0.0 then begin
      VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_GREATER_OR_EQUAL;
      end else begin
      VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
     end;
     VulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
     VulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

     VulkanGraphicsPipeline.Initialize;

     VulkanGraphicsPipeline.FreeMemory;

    finally
     fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]:=VulkanGraphicsPipeline;
    end;

   end;

  end;

 end;

end;

procedure TScreenMain.TDepthVelocityNormalsRenderPass.BeforeDestroySwapChain;
var Index:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
begin
 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;
 FreeAndNil(fVulkanPipelineLayout);
 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TDepthVelocityNormalsRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TDepthVelocityNormalsRenderPass.OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                               const aPipelineLayout:TpvVulkanPipelineLayout;
                                                                               const aRenderPassIndex:TpvSizeInt;
                                                                               const aPreviousInFlightFrameIndex:TpvSizeInt;
                                                                               const aInFlightFrameIndex:TpvSizeInt);
begin
 if not fOnSetRenderPassResourcesDone then begin
  fOnSetRenderPassResourcesDone:=true;
 end;
end;

procedure TScreenMain.TDepthVelocityNormalsRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                            const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var InFlightFrameState:TScreenMain.PInFlightFrameState;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 InFlightFrameState:=@fParent.fInFlightFrameStates[aInFlightFrameIndex];

 if InFlightFrameState^.Ready then begin

  fOnSetRenderPassResourcesDone:=false;

  fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Opaque],
                        IfThen(aFrameIndex=0,aInFlightFrameIndex,fFrameGraph.DrawPreviousInFlightFrameIndex),
                        aInFlightFrameIndex,
                        0,
                        InFlightFrameState^.FinalViewIndex,
                        InFlightFrameState^.CountViews,
                        aCommandBuffer,
                        fVulkanPipelineLayout,
                        OnSetRenderPassResources,
                        [TpvScene3D.TMaterial.TAlphaMode.Opaque]);

{ if (fParent.fTransparencyMode=TTransparencyMode.Direct) or not fParent.fUseOITAlphaTest then begin
   fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Mask],
                         IfThen(aFrameIndex=0,aInFlightFrameIndex,fFrameGraph.DrawPreviousInFlightFrameIndex),
                         aInFlightFrameIndex,
                         0,
                         InFlightFrameState^.FinalViewIndex,
                         InFlightFrameState^.CountViews,
                         aCommandBuffer,
                         fVulkanPipelineLayout,
                         OnSetRenderPassResources,
                         [TpvScene3D.TMaterial.TAlphaMode.Mask]);
  end;}

 end;

end;

{ TScreenMain.TDepthMipMapComputePass  }

constructor TScreenMain.TDepthMipMapComputePass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin
 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='DepthMipMapComputePass';

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin

  fResourceInput:=AddImageInput('resourcetype_depth',
                                'resource_depth_data',
                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                               );

 end else begin

  fResourceInput:=AddImageInput('resourcetype_msaa_depth',
                                'resource_msaa_depth_data',
                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                               );

 end;

end;

destructor TScreenMain.TDepthMipMapComputePass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TDepthMipMapComputePass.Show;
var Stream:TStream;
begin

 inherited Show;

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  if fParent.fZFar<0.0 then begin
   if fParent.fCountSurfaceViews>1 then begin
    Stream:=pvApplication.Assets.GetAssetStream('shaders/downsample_depth_multiview_reversedz_level0_comp.spv');
   end else begin
    Stream:=pvApplication.Assets.GetAssetStream('shaders/downsample_depth_reversedz_level0_comp.spv');
   end;
  end else begin
   if fParent.fCountSurfaceViews>1 then begin
    Stream:=pvApplication.Assets.GetAssetStream('shaders/downsample_depth_multiview_level0_comp.spv');
   end else begin
    Stream:=pvApplication.Assets.GetAssetStream('shaders/downsample_depth_level0_comp.spv');
   end;
  end;
 end else begin
  if fParent.fZFar<0.0 then begin
   if fParent.fCountSurfaceViews>1 then begin
    Stream:=pvApplication.Assets.GetAssetStream('shaders/downsample_depth_multiview_msaa_reversedz_level0_comp.spv');
   end else begin
    Stream:=pvApplication.Assets.GetAssetStream('shaders/downsample_depth_msaa_reversedz_level0_comp.spv');
   end;
  end else begin
   if fParent.fCountSurfaceViews>1 then begin
    Stream:=pvApplication.Assets.GetAssetStream('shaders/downsample_depth_multiview_msaa_level0_comp.spv');
   end else begin
    Stream:=pvApplication.Assets.GetAssetStream('shaders/downsample_depth_msaa_level0_comp.spv');
   end;
  end;
 end;
 try
  fDownsampleLevel0ComputeShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fParent.fZFar<0.0 then begin
  if fParent.fCountSurfaceViews>1 then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/downsample_depth_multiview_reversedz_level1_comp.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/downsample_depth_reversedz_level1_comp.spv');
  end;
 end else begin
  if fParent.fCountSurfaceViews>1 then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/downsample_multiview_level1_comp.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/downsample_level1_comp.spv');
  end;
 end;
 try
  fDownsampleLevel1ComputeShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageDownsampleLevel0Compute:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fDownsampleLevel0ComputeShaderModule,'main');

 fVulkanPipelineShaderStageDownsampleLevel1Compute:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fDownsampleLevel1ComputeShaderModule,'main');

end;

procedure TScreenMain.TDepthMipMapComputePass.Hide;
begin
 FreeAndNil(fVulkanPipelineShaderStageDownsampleLevel1Compute);
 FreeAndNil(fVulkanPipelineShaderStageDownsampleLevel0Compute);
 FreeAndNil(fDownsampleLevel1ComputeShaderModule);
 FreeAndNil(fDownsampleLevel0ComputeShaderModule);
 inherited Hide;
end;

procedure TScreenMain.TDepthMipMapComputePass.AfterCreateSwapChain;
var InFlightFrameIndex,MipMapLevelIndex:TpvInt32;
    ImageViewType:TVkImageViewType;
begin

 inherited AfterCreateSwapChain;

 fVulkanSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                         TVkFilter.VK_FILTER_NEAREST,
                                         TVkFilter.VK_FILTER_NEAREST,
                                         TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_NEAREST,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         0.0,
                                         false,
                                         0.0,
                                         false,
                                         VK_COMPARE_OP_ALWAYS,
                                         0.0,
                                         0.0,
                                         VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK,
                                         false);

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fParent.fCountInFlightFrames*fParent.fDepthMipmappedArray2DImages[0].MipMapLevels);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fParent.fCountInFlightFrames*fParent.fDepthMipmappedArray2DImages[0].MipMapLevels);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,fParent.fCountInFlightFrames*fParent.fDepthMipmappedArray2DImages[0].MipMapLevels);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(1,
                                       VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TpvUInt32));
 fPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fPipelineLayout.Initialize;

 fPipelineLevel0:=TpvVulkanComputePipeline.Create(pvApplication.VulkanDevice,
                                                  pvApplication.VulkanPipelineCache,
                                                  0,
                                                  fVulkanPipelineShaderStageDownsampleLevel0Compute,
                                                  fPipelineLayout,
                                                  nil,
                                                  0);

 fPipelineLevel1:=TpvVulkanComputePipeline.Create(pvApplication.VulkanDevice,
                                                  pvApplication.VulkanPipelineCache,
                                                  0,
                                                  fVulkanPipelineShaderStageDownsampleLevel1Compute,
                                                  fPipelineLayout,
                                                  nil,
                                                  0);

 if fParent.fCountSurfaceViews>1 then begin
  ImageViewType:=TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY);
 end else begin
  ImageViewType:=TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D);
 end;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(pvApplication.VulkanDevice,
                                                                   fResourceInput.VulkanImages[InFlightFrameIndex],
                                                                   ImageViewType,
                                                                   TpvFrameGraph.TImageResourceType(fResourceInput.ResourceType).Format,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   TVkImageAspectFlags(VK_IMAGE_ASPECT_DEPTH_BIT),
                                                                   0,
                                                                   1,
                                                                   0,
                                                                   fParent.fCountSurfaceViews
                                                                  );
  for MipMapLevelIndex:=0 to fParent.fDepthMipmappedArray2DImages[InFlightFrameIndex].MipMapLevels-1 do begin
   fVulkanDescriptorSets[InFlightFrameIndex,MipMapLevelIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                                             fVulkanDescriptorSetLayout);
   if MipMapLevelIndex=0 then begin
    fVulkanDescriptorSets[InFlightFrameIndex,MipMapLevelIndex].WriteToDescriptorSet(0,
                                                                                    0,
                                                                                    1,
                                                                                    TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                                    [TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                                                   fVulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                                   fResourceInput.ResourceTransition.Layout)],
                                                                                    [],
                                                                                    [],
                                                                                    false
                                                                                   );
   end else begin
    fVulkanDescriptorSets[InFlightFrameIndex,MipMapLevelIndex].WriteToDescriptorSet(0,
                                                                                    0,
                                                                                    1,
                                                                                    TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                                    [TVkDescriptorImageInfo.Create(fParent.fDepthMipmappedArray2DImages[InFlightFrameIndex].VulkanMipMapSampler.Handle,
                                                                                                                   fParent.fDepthMipmappedArray2DImages[InFlightFrameIndex].DescriptorImageInfos[MipMapLevelIndex-1].imageView,
                                                                                                                   VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                                    [],
                                                                                    [],
                                                                                    false
                                                                                   );
   end;
   fVulkanDescriptorSets[InFlightFrameIndex,MipMapLevelIndex].WriteToDescriptorSet(1,
                                                                                   0,
                                                                                   1,
                                                                                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                                   [TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                                                  fParent.fDepthMipmappedArray2DImages[InFlightFrameIndex].DescriptorImageInfos[MipMapLevelIndex].imageView,
                                                                                                                  VK_IMAGE_LAYOUT_GENERAL)],
                                                                                   [],
                                                                                   [],
                                                                                   false
                                                                                  );
   fVulkanDescriptorSets[InFlightFrameIndex,MipMapLevelIndex].Flush;
  end;
 end;

end;

procedure TScreenMain.TDepthMipMapComputePass.BeforeDestroySwapChain;
var InFlightFrameIndex,MipMapLevelIndex:TpvInt32;
begin
 FreeAndNil(fPipelineLevel1);
 FreeAndNil(fPipelineLevel0);
 FreeAndNil(fPipelineLayout);
 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  for MipMapLevelIndex:=0 to fParent.fDepthMipmappedArray2DImages[InFlightFrameIndex].MipMapLevels-1 do begin
   FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex,MipMapLevelIndex]);
  end;
  FreeAndNil(fVulkanImageViews[InFlightFrameIndex]);
 end;
 FreeAndNil(fVulkanDescriptorSetLayout);
 FreeAndNil(fVulkanDescriptorPool);
 FreeAndNil(fVulkanSampler);
 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TDepthMipMapComputePass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TDepthMipMapComputePass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var InFlightFrameIndex,MipMapLevelIndex:TpvInt32;
    Pipeline:TpvVulkanComputePipeline;
    ImageMemoryBarrier:TVkImageMemoryBarrier;
    CountSamples:TpvUInt32;
begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 InFlightFrameIndex:=aInFlightFrameIndex;

 FillChar(ImageMemoryBarrier,SizeOf(TVkImageMemoryBarrier),#0);
 ImageMemoryBarrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
 ImageMemoryBarrier.pNext:=nil;
 ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT);
 ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
 ImageMemoryBarrier.oldLayout:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
 ImageMemoryBarrier.newLayout:=VK_IMAGE_LAYOUT_GENERAL;
 ImageMemoryBarrier.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 ImageMemoryBarrier.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 ImageMemoryBarrier.image:=fParent.fDepthMipmappedArray2DImages[InFlightFrameIndex].VulkanImage.Handle;
 ImageMemoryBarrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 ImageMemoryBarrier.subresourceRange.baseMipLevel:=0;
 ImageMemoryBarrier.subresourceRange.levelCount:=fParent.fDepthMipmappedArray2DImages[InFlightFrameIndex].MipMapLevels;
 ImageMemoryBarrier.subresourceRange.baseArrayLayer:=0;
 ImageMemoryBarrier.subresourceRange.layerCount:=fParent.fCountSurfaceViews;
 aCommandBuffer.CmdPipelineBarrier(fFrameGraph.VulkanDevice.PhysicalDevice.PipelineStageAllShaderBits,
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@ImageMemoryBarrier);

 for MipMapLevelIndex:=0 to fParent.fDepthMipmappedArray2DImages[InFlightFrameIndex].MipMapLevels-1 do begin

  case MipMapLevelIndex of
   0:begin
    Pipeline:=fPipelineLevel0;
   end;
   else begin
    Pipeline:=fPipelineLevel1;
   end;
  end;

  if MipMapLevelIndex<3 then begin
   aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,Pipeline.Handle);
  end;

  CountSamples:=fParent.fCountSurfaceMSAASamples;

  aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                  TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_COMPUTE_BIT),
                                  0,
                                  SizeOf(TpvUInt32),
                                  @CountSamples);

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       fPipelineLayout.Handle,
                                       0,
                                       1,
                                       @fVulkanDescriptorSets[InFlightFrameIndex,MipMapLevelIndex].Handle,
                                       0,
                                       nil);

  aCommandBuffer.CmdDispatch(Max(1,(fParent.fWidth+((1 shl (4+MipMapLevelIndex))-1)) shr (4+MipMapLevelIndex)),
                             Max(1,(fParent.fHeight+((1 shl (4+MipMapLevelIndex))-1)) shr (4+MipMapLevelIndex)),
                             fParent.fCountSurfaceViews);

  FillChar(ImageMemoryBarrier,SizeOf(TVkImageMemoryBarrier),#0);
  ImageMemoryBarrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
  ImageMemoryBarrier.pNext:=nil;
  ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
  ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT);
  ImageMemoryBarrier.oldLayout:=VK_IMAGE_LAYOUT_GENERAL;
  ImageMemoryBarrier.newLayout:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
  ImageMemoryBarrier.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  ImageMemoryBarrier.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  ImageMemoryBarrier.image:=fParent.fDepthMipmappedArray2DImages[InFlightFrameIndex].VulkanImage.Handle;
  ImageMemoryBarrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
  ImageMemoryBarrier.subresourceRange.baseMipLevel:=MipMapLevelIndex;
  ImageMemoryBarrier.subresourceRange.levelCount:=1;
  ImageMemoryBarrier.subresourceRange.baseArrayLayer:=0;
  ImageMemoryBarrier.subresourceRange.layerCount:=fParent.fCountSurfaceViews;
  if (MipMapLevelIndex+1)<fParent.fDepthMipmappedArray2DImages[InFlightFrameIndex].MipMapLevels then begin
   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                     0,
                                     0,nil,
                                     0,nil,
                                     1,@ImageMemoryBarrier);
  end else begin
   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                     fFrameGraph.VulkanDevice.PhysicalDevice.PipelineStageAllShaderBits,
                                     0,
                                     0,nil,
                                     0,nil,
                                     1,@ImageMemoryBarrier);
  end;

 end;

end;

{ TScreenMain.TCascadedShadowMapRenderPass }

constructor TScreenMain.TCascadedShadowMapRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
var Index:TpvSizeInt;
begin
inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='CascadedShadowMap';

 MultiviewMask:=0;
 for Index:=0 to CountCascadedShadowMapCascades-1 do begin
  MultiviewMask:=MultiviewMask or (1 shl Index);
 end;

 Queue:=aFrameGraph.UniversalQueue;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.Absolute,
                                       fParent.CascadedShadowMapWidth,
                                       fParent.CascadedShadowMapHeight,
                                       1.0,
                                       CountCascadedShadowMapCascades);

 if fParent.fVulkanShadowMapSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  fResourceDepth:=AddImageDepthOutput('resourcetype_cascadedshadowmap_depth',
                                      'resource_cascadedshadowmap_single_depth',
                                      VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                      TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                   TpvVector4.InlineableCreate(1.0,1.0,1.0,1.0)),
                                      [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                     );
 end else begin
  fResourceDepth:=AddImageDepthOutput('resourcetype_cascadedshadowmap_msaa_depth',
                                      'resource_cascadedshadowmap_msaa_depth',
                                      VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                      TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                   TpvVector4.InlineableCreate(1.0,1.0,1.0,1.0)),
                                      [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                     );
 end;

end;

destructor TScreenMain.TCascadedShadowMapRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TCascadedShadowMapRenderPass.Show;
var Index:TpvSizeInt;
    Stream:TStream;
begin
 inherited Show;

 fVulkanGraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.GraphicsQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanGraphicsCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_vert.spv');
 try
  fMeshVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_depth_frag.spv');
 try
  fMeshFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fParent.fUseDemote then begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_depth_masked_demote_frag.spv');
 end else if fParent.fUseNoDiscard then begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_depth_masked_nodiscard_frag.spv');
 end else begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_depth_masked_frag.spv');
 end;
 try
  fMeshMaskedFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageMeshVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fMeshVertexShaderModule,'main');

 fVulkanPipelineShaderStageMeshFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshFragmentShaderModule,'main');

 fVulkanPipelineShaderStageMeshMaskedFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshMaskedFragmentShaderModule,'main');

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT),0,SizeOf(TpvScene3D.TVertexStagePushConstants));
 fVulkanPipelineLayout.AddDescriptorSetLayout(fParent.fScene3D.GlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

end;

procedure TScreenMain.TCascadedShadowMapRenderPass.Hide;
begin

 FreeAndNil(fVulkanPipelineLayout);

 FreeAndNil(fVulkanPipelineShaderStageMeshVertex);

 FreeAndNil(fVulkanPipelineShaderStageMeshFragment);

 FreeAndNil(fVulkanPipelineShaderStageMeshMaskedFragment);

 FreeAndNil(fMeshVertexShaderModule);

 FreeAndNil(fMeshFragmentShaderModule);

 FreeAndNil(fMeshMaskedFragmentShaderModule);

 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 FreeAndNil(fVulkanGraphicsCommandBufferFence);
 FreeAndNil(fVulkanGraphicsCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TCascadedShadowMapRenderPass.AfterCreateSwapChain;
var AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
    VulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
begin

 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin

  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin

   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin

    VulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                             pvApplication.VulkanPipelineCache,
                                                             0,
                                                             [],
                                                             fVulkanPipelineLayout,
                                                             fVulkanRenderPass,
                                                             VulkanRenderPassSubpassIndex,
                                                             nil,
                                                             0);

    try

     VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshVertex);
     if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask then begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshMaskedFragment);
     end else begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshFragment);
     end;

     VulkanGraphicsPipeline.InputAssemblyState.Topology:=TVkPrimitiveTopology(PrimitiveTopology);
     VulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

     fParent.fScene3D.InitializeGraphicsPipeline(VulkanGraphicsPipeline);

     VulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.CascadedShadowMapWidth,fParent.CascadedShadowMapHeight,0.0,1.0);
     VulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.CascadedShadowMapWidth,fParent.CascadedShadowMapHeight);

     VulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
     if DoubleSided then begin
      VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
     end else begin
      VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
     end;
     VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_CLOCKWISE;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=true;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=1.25;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=1.75;
     VulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

     VulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=fParent.fVulkanShadowMapSampleCountFlagBits;
     VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=false;
     VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=0.0;
     VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
     VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=false;
     VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;

     VulkanGraphicsPipeline.ColorBlendState.LogicOpEnable:=false;
     VulkanGraphicsPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[0]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[1]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[2]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[3]:=0.0;
     if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Blend then begin
      VulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(true,
                                                                          VK_BLEND_FACTOR_SRC_ALPHA,
                                                                          VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                          VK_BLEND_OP_ADD,
                                                                          VK_BLEND_FACTOR_ONE,
                                                                          VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                          VK_BLEND_OP_ADD,
                                                                          TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                          TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                          TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                          TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));
     end else begin
      VulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
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
     end;

     VulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
     VulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true; //AlphaMode<>TpvScene3D.TMaterial.TAlphaMode.Blend;
     VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
     VulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
     VulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

     VulkanGraphicsPipeline.Initialize;

     VulkanGraphicsPipeline.FreeMemory;

    finally
     fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]:=VulkanGraphicsPipeline;
    end;

   end;

  end;

 end;

end;

procedure TScreenMain.TCascadedShadowMapRenderPass.BeforeDestroySwapChain;
var AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
begin
 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;
 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TCascadedShadowMapRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TCascadedShadowMapRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                           const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var InFlightFrameState:TScreenMain.PInFlightFrameState;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 InFlightFrameState:=@fParent.fInFlightFrameStates[aInFlightFrameIndex];

 if InFlightFrameState^.Ready then begin

  fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Opaque],
                        -1,
                        aInFlightFrameIndex,
                        1,
                        InFlightFrameState^.CascadedShadowMapViewIndex,
                        InFlightFrameState^.CountCascadedShadowMapViews,
                        aCommandBuffer,
                        fVulkanPipelineLayout,
                        nil,
                        [TpvScene3D.TMaterial.TAlphaMode.Opaque]);

  fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Mask],
                        -1,
                        aInFlightFrameIndex,
                        1,
                        InFlightFrameState^.CascadedShadowMapViewIndex,
                        InFlightFrameState^.CountCascadedShadowMapViews,
                        aCommandBuffer,
                        fVulkanPipelineLayout,
                        nil,
                        [TpvScene3D.TMaterial.TAlphaMode.Mask]);

{ fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Blend],
                        -1,
                        aInFlightFrameIndex,
                        0,
                        InFlightFrameState^.FinalViewIndex,
                        InFlightFrameState^.CountViews,
                        aCommandBuffer,
                        fVulkanPipelineLayout,
                        [TpvScene3D.TMaterial.TAlphaMode.Blend]);}

 end;

end;

{ TScreenMain.TCascadedShadowMapResolveRenderPass }

constructor TScreenMain.TCascadedShadowMapResolveRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
var Index:TpvSizeInt;
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='CascadedShadowMapResolveRenderPass';

 MultiviewMask:=0;
{for Index:=0 to CountCascadedShadowMapCascades-1 do begin
  MultiviewMask:=MultiviewMask or (1 shl Index);
 end; }

 Queue:=aFrameGraph.UniversalQueue;

//SeparatePhysicalPass:=true;

//SeparateCommandBuffer:=true;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.Absolute,
                                       fParent.CascadedShadowMapWidth,
                                       fParent.CascadedShadowMapHeight,
                                       1.0,
                                       CountCascadedShadowMapCascades);

 if fParent.fVulkanShadowMapSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  fResourceInput:=AddImageInput('resourcetype_cascadedshadowmap_depth',
                                'resource_cascadedshadowmap_single_depth',
                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                []//TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                               );
 end else begin
  fResourceInput:=AddImageInput('resourcetype_cascadedshadowmap_msaa_depth',
                                'resource_cascadedshadowmap_msaa_depth',
                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                []//TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                               );
 end;

 fResourceOutput:=AddImageOutput('resourcetype_cascadedshadowmap_data',
                                 'resource_cascadedshadowmap_data',
                                 VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                              TpvVector4.InlineableCreate(1.0,1.0,1.0,1.0)),
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

end;

destructor TScreenMain.TCascadedShadowMapResolveRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TCascadedShadowMapResolveRenderPass.Show;
var Stream:TStream;
begin

 inherited Show;

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/msm_resolve_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fParent.fVulkanShadowMapSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/msm_resolve_frag.spv');
 end else begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/msm_resolve_msaa_frag.spv');
 end;
 try
  fVulkanFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVulkanVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fVulkanFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

 fVulkanSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         0.0,
                                         false,
                                         0.0,
                                         false,
                                         VK_COMPARE_OP_ALWAYS,
                                         0.0,
                                         0.0,
                                         VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK,
                                         false);

end;

procedure TScreenMain.TCascadedShadowMapResolveRenderPass.Hide;
begin
 FreeAndNil(fVulkanSampler);
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TCascadedShadowMapResolveRenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin
 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                                fResourceInput.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceInput.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),0,SizeOf(TScreenMain.TCascadedShadowMapResolveRenderPass.TFragmentStagePushConstants));
 fVulkanPipelineLayout.Initialize;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                           pvApplication.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           fVulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           VulkanRenderPassSubpassIndex,
                                                           nil,
                                                           0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.CascadedShadowMapWidth,fParent.CascadedShadowMapHeight,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.CascadedShadowMapWidth,fParent.CascadedShadowMapHeight);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
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
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_DST_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_ONE,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_OP_ADD,
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

end;

procedure TScreenMain.TCascadedShadowMapResolveRenderPass.BeforeDestroySwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 fVulkanRenderPass:=nil;

 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TCascadedShadowMapResolveRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TCascadedShadowMapResolveRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 fFragmentStagePushConstants.CountSamples:=fParent.fCountCascadedShadowMapMSAASamples;
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      fVulkanPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,0,nil);
 aCommandBuffer.CmdPushConstants(fVulkanPipelineLayout.Handle,
                                 TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_FRAGMENT_BIT),
                                 0,
                                 SizeOf(TScreenMain.TCascadedShadowMapResolveRenderPass.TFragmentStagePushConstants),
                                 @fFragmentStagePushConstants);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
 aCommandBuffer.CmdDraw(3*CountCascadedShadowMapCascades,1,0,0);
end;

{ TScreenMain.TCascadedShadowMapBlurRenderPass }

constructor TScreenMain.TCascadedShadowMapBlurRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain;const aHorziontal:boolean);
var Index:TpvSizeInt;
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 if aHorziontal then begin
  fFragmentStagePushConstants.Direction:=TpvVector4.InlineableCreate(1.0,0.0,0.0,0.0);
  Name:='CascadedShadowMapBlurRenderPass(Horziontal)';
 end else begin
  fFragmentStagePushConstants.Direction:=TpvVector4.InlineableCreate(0.0,1.0,0.0);
  Name:='CascadedShadowMapBlurRenderPass(Vertical)';
 end;

 MultiviewMask:=0;
{for Index:=0 to CountCascadedShadowMapCascades-1 do begin
  MultiviewMask:=MultiviewMask or (1 shl Index);
 end; }

 Queue:=aFrameGraph.UniversalQueue;

//SeparatePhysicalPass:=true;

//SeparateCommandBuffer:=true;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.Absolute,
                                       fParent.CascadedShadowMapWidth,
                                       fParent.CascadedShadowMapHeight,
                                       1.0,
                                       CountCascadedShadowMapCascades);

 if aHorziontal then begin

  fResourceInput:=AddImageInput('resourcetype_cascadedshadowmap_data',
                                'resource_cascadedshadowmap_data',
                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                []//TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                               );

  fResourceOutput:=AddImageOutput('resourcetype_cascadedshadowmap_data',
                                  'resource_cascadedshadowmap_data_temporary_blurred',
                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                  TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                               TpvVector4.InlineableCreate(1.0,1.0,1.0,1.0)),
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );


 end else begin

  fResourceInput:=AddImageInput('resourcetype_cascadedshadowmap_data',
                                'resource_cascadedshadowmap_data_temporary_blurred',
                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                []//TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                               );

  fResourceOutput:=AddImageOutput('resourcetype_cascadedshadowmap_data',
                                  'resource_cascadedshadowmap_data_final',
                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                  TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                               TpvVector4.InlineableCreate(1.0,1.0,1.0,1.0)),
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );

 end;

end;

destructor TScreenMain.TCascadedShadowMapBlurRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TCascadedShadowMapBlurRenderPass.Show;
var Stream:TStream;
begin

 inherited Show;

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/msm_blur_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/msm_blur_frag.spv');
 try
  fVulkanFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVulkanVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fVulkanFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

 fVulkanSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         0.0,
                                         false,
                                         0.0,
                                         false,
                                         VK_COMPARE_OP_ALWAYS,
                                         0.0,
                                         0.0,
                                         VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK,
                                         false);

end;

procedure TScreenMain.TCascadedShadowMapBlurRenderPass.Hide;
begin
 FreeAndNil(fVulkanSampler);
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TCascadedShadowMapBlurRenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin
 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                                fResourceInput.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceInput.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),0,SizeOf(TScreenMain.TCascadedShadowMapBlurRenderPass.TFragmentStagePushConstants));
 fVulkanPipelineLayout.Initialize;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                           pvApplication.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           fVulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           VulkanRenderPassSubpassIndex,
                                                           nil,
                                                           0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.CascadedShadowMapWidth,fParent.CascadedShadowMapHeight,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.CascadedShadowMapWidth,fParent.CascadedShadowMapHeight);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
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
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_DST_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_ONE,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_OP_ADD,
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

end;

procedure TScreenMain.TCascadedShadowMapBlurRenderPass.BeforeDestroySwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 fVulkanRenderPass:=nil;

 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TCascadedShadowMapBlurRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TCascadedShadowMapBlurRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      fVulkanPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,0,nil);
 aCommandBuffer.CmdPushConstants(fVulkanPipelineLayout.Handle,
                                 TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_FRAGMENT_BIT),
                                 0,
                                 SizeOf(TScreenMain.TCascadedShadowMapBlurRenderPass.TFragmentStagePushConstants),
                                 @fFragmentStagePushConstants);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
 aCommandBuffer.CmdDraw(3*CountCascadedShadowMapCascades,1,0,0);
end;

{ TScreenMain.TSSAORenderPass }

constructor TScreenMain.TSSAORenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='SSAO';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

//SeparatePhysicalPass:=true;

//SeparateCommandBuffer:=true;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 fResourceNormals:=AddImageInput('resourcetype_normals',
                                 'resource_normals_data',
                                 VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                 []//TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

 fResourceOutput:=AddImageOutput('resourcetype_ssao',
                                 'resource_ssao_data_temporary',
                                 VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                              TpvVector4.InlineableCreate(1.0,1.0,1.0,1.0)),
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

end;

destructor TScreenMain.TSSAORenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TSSAORenderPass.Show;
var Stream:TStream;
begin

 inherited Show;

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/fullscreen_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fParent.fCountSurfaceViews>1 then begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/ssao_multiview_frag.spv');
 end else begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/ssao_frag.spv');
 end;
 try
  fVulkanFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVulkanVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fVulkanFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

end;

procedure TScreenMain.TSSAORenderPass.Hide;
begin
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TSSAORenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin
 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         0.0,
                                         false,
                                         0.0,
                                         false,
                                         VK_COMPARE_OP_ALWAYS,
                                         0.0,
                                         0.0,
                                         VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK,
                                         false);

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fParent.fCountInFlightFrames*4);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fParent.fCountInFlightFrames*2);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(1,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(2,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                 [],
                                                                 [fParent.fScene3D.GlobalVulkanViewUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                 [],
                                                                 false);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                                fParent.fDepthMipmappedArray2DImages[InFlightFrameIndex].DescriptorImageInfo.imageView,
                                                                                                TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                                fResourceNormals.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceNormals.ResourceTransition.Layout)],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),0,SizeOf(TSSAOPushConstants));
 fVulkanPipelineLayout.Initialize;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                           pvApplication.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           fVulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           VulkanRenderPassSubpassIndex,
                                                           nil,
                                                           0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
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
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_DST_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_ONE,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_OP_ADD,
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT));

 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

end;

procedure TScreenMain.TSSAORenderPass.BeforeDestroySwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 FreeAndNil(fVulkanSampler);

 fVulkanRenderPass:=nil;

 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TSSAORenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TSSAORenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var InFlightFrameState:TScreenMain.PInFlightFrameState;
    SSAOPushConstants:TSSAOPushConstants;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 InFlightFrameState:=@fParent.fInFlightFrameStates[aInFlightFrameIndex];
 SSAOPushConstants.ViewBaseIndex:=InFlightFrameState^.FinalViewIndex;
 SSAOPushConstants.CountViews:=InFlightFrameState^.CountViews;
 SSAOPushConstants.FrameIndex:=fFrameGraph.DrawFrameIndex;
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      fVulkanPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,0,nil);
 aCommandBuffer.CmdPushConstants(fVulkanPipelineLayout.Handle,
                                 TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_FRAGMENT_BIT),
                                 0,
                                 SizeOf(TSSAOPushConstants),
                                 @SSAOPushConstants);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
 aCommandBuffer.CmdDraw(3,1,0,0);
end;

{ TScreenMain.TSSAOBlurRenderPass }

constructor TScreenMain.TSSAOBlurRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain;const aHorziontal:boolean);
var Index:TpvSizeInt;
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 if aHorziontal then begin
  fFragmentStagePushConstants.Direction:=TpvVector4.InlineableCreate(1.0,0.0,0.0,0.0);
  Name:='SSAOBlurRenderPass(Horziontal)';
 end else begin
  fFragmentStagePushConstants.Direction:=TpvVector4.InlineableCreate(0.0,1.0,0.0,0.0);
  Name:='SSAOBlurRenderPass(Vertical)';
 end;

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

//SeparatePhysicalPass:=true;

//SeparateCommandBuffer:=true;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 if aHorziontal then begin

  fResourceInput:=AddImageInput('resourcetype_ssao',
                                'resource_ssao_data_temporary',
                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                []//TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                               );

  fResourceOutput:=AddImageOutput('resourcetype_ssao',
                                  'resource_ssao_data_temporary_blurred',
                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                  TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                               TpvVector4.InlineableCreate(1.0,1.0,1.0,1.0)),
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );


 end else begin

  fResourceInput:=AddImageInput('resourcetype_ssao',
                                'resource_ssao_data_temporary_blurred',
                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                []//TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                               );

  fResourceOutput:=AddImageOutput('resourcetype_ssao_final',
                                  'resource_ssao_data_final',
                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                  TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                               TpvVector4.InlineableCreate(1.0,1.0,1.0,1.0)),
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );

 end;

end;

destructor TScreenMain.TSSAOBlurRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TSSAOBlurRenderPass.Show;
var Stream:TStream;
begin

 inherited Show;

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/fullscreen_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/ssao_blur_frag.spv');
 try
  fVulkanFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVulkanVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fVulkanFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

 fVulkanSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         0.0,
                                         false,
                                         0.0,
                                         false,
                                         VK_COMPARE_OP_ALWAYS,
                                         0.0,
                                         0.0,
                                         VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK,
                                         false);

end;

procedure TScreenMain.TSSAOBlurRenderPass.Hide;
begin
 FreeAndNil(fVulkanSampler);
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TSSAOBlurRenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin
 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                                fResourceInput.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceInput.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),0,SizeOf(TScreenMain.TSSAOBlurRenderPass.TFragmentStagePushConstants));
 fVulkanPipelineLayout.Initialize;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                           pvApplication.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           fVulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           VulkanRenderPassSubpassIndex,
                                                           nil,
                                                           0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
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
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_DST_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_ONE,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_OP_ADD,
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT));

 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

end;

procedure TScreenMain.TSSAOBlurRenderPass.BeforeDestroySwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 fVulkanRenderPass:=nil;

 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TSSAOBlurRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TSSAOBlurRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      fVulkanPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,0,nil);
 aCommandBuffer.CmdPushConstants(fVulkanPipelineLayout.Handle,
                                 TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_FRAGMENT_BIT),
                                 0,
                                 SizeOf(TScreenMain.TSSAOBlurRenderPass.TFragmentStagePushConstants),
                                 @fFragmentStagePushConstants);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
 aCommandBuffer.CmdDraw(3,1,0,0);
end;

{ TScreenMain.TForwardRenderPass }

constructor TScreenMain.TForwardRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin
inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='ForwardRendering';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 fResourceCascadedShadowMap:=AddImageInput('resourcetype_cascadedshadowmap_data',
                                           'resource_cascadedshadowmap_data_final',
                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                           []
                                          );

 fResourceSSAO:=AddImageInput('resourcetype_ssao_final',
                              'resource_ssao_data_final',
                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                              []
                             );

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin

  fResourceColor:=AddImageOutput('resourcetype_color_optimized_non_alpha',
                                 'resource_forwardrendering_color',
                                 VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                              TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

 fResourceDepth:=AddImageDepthInput('resourcetype_depth',
                                    'resource_depth_data',
                                    VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL,//VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,//VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                    [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                   );{}

{ fResourceDepth:=AddImageDepthOutput('resourcetype_depth',
                                      'resource_depth_data',
                                      VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                      TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                   TpvVector4.InlineableCreate(IfThen(fParent.fZFar<0.0,0.0,1.0),0.0,0.0,0.0)),
                                      [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                     ); {}

 end else begin

  fResourceColor:=AddImageOutput('resourcetype_msaa_color_optimized_non_alpha',
                                 'resource_forwardrendering_msaa_color',
                                 VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                              TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

  fResourceColor:=AddImageResolveOutput('resourcetype_color_optimized_non_alpha',
                                        'resource_forwardrendering_color',
                                        'resource_forwardrendering_msaa_color',
                                        VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                        TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare,
                                                                     TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                        [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                       );

  fResourceDepth:=AddImageDepthInput('resourcetype_msaa_depth',
                                     'resource_msaa_depth_data',
                                     VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL,//VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,//VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

{fResourceDepth:=AddImageDepthOutput('resourcetype_msaa_depth',
                                     'resource_msaa_depth_data',
                                     VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                     TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                  TpvVector4.InlineableCreate(IfThen(fParent.fZFar<0.0,0.0,1.0),0.0,0.0,0.0)),
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );{}

 end;

end;

destructor TScreenMain.TForwardRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TForwardRenderPass.Show;
var Index:TpvSizeInt;
    Stream:TStream;
begin
 inherited Show;

 fVulkanGraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.GraphicsQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanGraphicsCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_vert.spv');
 try
  fMeshVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_frag.spv');
 try
  fMeshFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fParent.fUseDemote then begin
  if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_masked_demote_frag.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_masked_demote_msaa_frag.spv');
  end;
 end else if fParent.fUseNoDiscard then begin
  if fParent.fZFar<0.0 then begin
   if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
    Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_masked_nodiscard_reversedz_frag.spv');
   end else begin
    Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_masked_nodiscard_reversedz_msaa_frag.spv');
   end;
  end else begin
   if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
    Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_masked_nodiscard_frag.spv');
   end else begin
    Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_masked_nodiscard_msaa_frag.spv');
   end;
  end;
 end else begin
  if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_masked_frag.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_masked_msaa_frag.spv');
  end;
 end;
 try
  fMeshMaskedFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fParent.fUseDepthPrepass then begin

  Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_depth_frag.spv');
  try
   fMeshDepthFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
  finally
   Stream.Free;
  end;

  if fParent.fUseDemote then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_depth_masked_demote_frag.spv');
  end else if fParent.fUseNoDiscard then begin
   if fParent.fZFar<0.0 then begin
    Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_depth_masked_nodiscard_reversedz_frag.spv');
   end else begin
    Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_depth_masked_nodiscard_frag.spv');
   end;
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_depth_masked_frag.spv');
  end;
  try
   fMeshDepthMaskedFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
  finally
   Stream.Free;
  end;

 end;

 fVulkanPipelineShaderStageMeshVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fMeshVertexShaderModule,'main');

 fVulkanPipelineShaderStageMeshFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshFragmentShaderModule,'main');

 fVulkanPipelineShaderStageMeshMaskedFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshMaskedFragmentShaderModule,'main');

 if fParent.fUseDepthPrepass then begin

  fVulkanPipelineShaderStageMeshDepthFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshDepthFragmentShaderModule,'main');

  fVulkanPipelineShaderStageMeshDepthMaskedFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshDepthMaskedFragmentShaderModule,'main');

 end;

 fVulkanCascadedShadowMapSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                                          TVkFilter.VK_FILTER_LINEAR,
                                                          TVkFilter.VK_FILTER_LINEAR,
                                                          TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          0.0,
                                                          false,
                                                          0.0,
                                                          false,
                                                          VK_COMPARE_OP_ALWAYS,
                                                          0.0,
                                                          0.0,
                                                          VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE,
                                                          false);

 fVulkanSSAOSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                             TVkFilter.VK_FILTER_LINEAR,
                                             TVkFilter.VK_FILTER_LINEAR,
                                             TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             0.0,
                                             false,
                                             0.0,
                                             false,
                                             VK_COMPARE_OP_ALWAYS,
                                             0.0,
                                             0.0,
                                             VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE,
                                             false);

 fSkyBox:=TSkyBox.Create(fParent.fScene3D,
                         fParent.fSkyCubeMap.DescriptorImageInfo);

end;

procedure TScreenMain.TForwardRenderPass.Hide;
begin

 FreeAndNil(fSkyBox);

 FreeAndNil(fVulkanCascadedShadowMapSampler);

 FreeAndNil(fVulkanSSAOSampler);

 FreeAndNil(fVulkanPipelineShaderStageMeshVertex);

 FreeAndNil(fVulkanPipelineShaderStageMeshFragment);

 FreeAndNil(fVulkanPipelineShaderStageMeshMaskedFragment);

 if fParent.fUseDepthPrepass then begin

  FreeAndNil(fVulkanPipelineShaderStageMeshDepthFragment);

  FreeAndNil(fVulkanPipelineShaderStageMeshDepthMaskedFragment);

 end;

 FreeAndNil(fMeshVertexShaderModule);

 FreeAndNil(fMeshFragmentShaderModule);

 FreeAndNil(fMeshMaskedFragmentShaderModule);

 if fParent.fUseDepthPrepass then begin

  FreeAndNil(fMeshDepthFragmentShaderModule);

  FreeAndNil(fMeshDepthMaskedFragmentShaderModule);

 end;

 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 FreeAndNil(fVulkanGraphicsCommandBufferFence);
 FreeAndNil(fVulkanGraphicsCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TForwardRenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
    DepthPrePass:boolean;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
    VulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
begin

 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fGlobalVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fGlobalVulkanDescriptorSetLayout.AddBinding(0,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             3,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(1,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             3,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(2,
                                             VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(3,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(4,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             2,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.Initialize;

 fGlobalVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,9*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,1*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fGlobalVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fGlobalVulkanDescriptorPool,
                                                                                 fGlobalVulkanDescriptorSetLayout);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                       0,
                                                                       3,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [fParent.fGGXBRDF.DescriptorImageInfo,
                                                                        fParent.fCharlieBRDF.DescriptorImageInfo,
                                                                        fParent.fSheenELUT.DescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                       0,
                                                                       3,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [fParent.fGGXEnvMapCubeMap.DescriptorImageInfo,
                                                                        fParent.fCharlieEnvMapCubeMap.DescriptorImageInfo,
                                                                        fParent.fLambertianEnvMapCubeMap.DescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                       [],
                                                                       [fParent.fCascadedShadowMapVulkanUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [TVkDescriptorImageInfo.Create(fVulkanCascadedShadowMapSampler.Handle,
                                                                                                      fResourceCascadedShadowMap.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceCascadedShadowMap.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                                       0,
                                                                       2,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [TVkDescriptorImageInfo.Create(fVulkanSSAOSampler.Handle,
                                                                                                      fResourceSSAO.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceSSAO.ResourceTransition.Layout),// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                        // Duplicate as dummy really non-used opaque texture
                                                                        TVkDescriptorImageInfo.Create(fVulkanSSAOSampler.Handle,
                                                                                                      fResourceSSAO.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceSSAO.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT),0,SizeOf(TpvScene3D.TVertexStagePushConstants));
 fVulkanPipelineLayout.AddDescriptorSetLayout(fParent.fScene3D.GlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 for DepthPrePass:=false to fParent.fUseDepthPrepass do begin
  for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
   for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
    for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
     FreeAndNil(fVulkanGraphicsPipelines[DepthPrePass,AlphaMode,PrimitiveTopology,DoubleSided]);
    end;
   end;
  end;
 end;

 for DepthPrePass:=false to fParent.fUseDepthPrepass do begin

  for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin

   for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin

    for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin

     VulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                              pvApplication.VulkanPipelineCache,
                                                              0,
                                                              [],
                                                              fVulkanPipelineLayout,
                                                              fVulkanRenderPass,
                                                              VulkanRenderPassSubpassIndex,
                                                              nil,
                                                              0);

     try

      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshVertex);
      if DepthPrePass then begin
       if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask then begin
        VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshDepthMaskedFragment);
       end else begin
        VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshDepthFragment);
       end;
      end else begin
       if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask then begin
        VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshMaskedFragment);
       end else begin
        VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshFragment);
       end;
      end;

      VulkanGraphicsPipeline.InputAssemblyState.Topology:=TVkPrimitiveTopology(PrimitiveTopology);
      VulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

      fParent.fScene3D.InitializeGraphicsPipeline(VulkanGraphicsPipeline);

      VulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
      VulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

      VulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
      VulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
      VulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
      if DoubleSided then begin
       VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
      end else begin
       VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
      end;
      VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
      VulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
      VulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
      VulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
      VulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
      VulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

      VulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=fParent.fVulkanSampleCountFlagBits;
      if (not DepthPrePass) and
         (AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask) and
         (VulkanGraphicsPipeline.MultisampleState.RasterizationSamples<>VK_SAMPLE_COUNT_1_BIT) then begin
       VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=true;
       VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=1.0;
       VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
       VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=true;
       VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;
       VulkanGraphicsPipeline.MultisampleState.AddSampleMask((1 shl fParent.fCountSurfaceMSAASamples)-1);
      end else begin
       VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=false;
       VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=0.0;
       VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
       VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=false;
       VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;
      end;

      VulkanGraphicsPipeline.ColorBlendState.LogicOpEnable:=false;
      VulkanGraphicsPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
      VulkanGraphicsPipeline.ColorBlendState.BlendConstants[0]:=0.0;
      VulkanGraphicsPipeline.ColorBlendState.BlendConstants[1]:=0.0;
      VulkanGraphicsPipeline.ColorBlendState.BlendConstants[2]:=0.0;
      VulkanGraphicsPipeline.ColorBlendState.BlendConstants[3]:=0.0;
      if DepthPrePass then begin
       VulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
                                                                           VK_BLEND_FACTOR_ZERO,
                                                                           VK_BLEND_FACTOR_ZERO,
                                                                           VK_BLEND_OP_ADD,
                                                                           VK_BLEND_FACTOR_ZERO,
                                                                           VK_BLEND_FACTOR_ZERO,
                                                                           VK_BLEND_OP_ADD,
                                                                           0);
      end else begin
       if ((VulkanGraphicsPipeline.MultisampleState.RasterizationSamples<>VK_SAMPLE_COUNT_1_BIT) and
           (AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask)) or
          (AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Blend) then begin
        VulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(true,
                                                                            VK_BLEND_FACTOR_SRC_ALPHA,
                                                                            VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                            VK_BLEND_OP_ADD,
                                                                            VK_BLEND_FACTOR_ONE,
                                                                            VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                            VK_BLEND_OP_ADD,
                                                                            TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                            TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                            TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                            TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));
       end else begin
        VulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
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
       end;
      end;

      VulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
      VulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=AlphaMode<>TpvScene3D.TMaterial.TAlphaMode.Blend;
      if fParent.fZFar<0.0 then begin
       VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_GREATER_OR_EQUAL;
       end else begin
       VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
      end;
      VulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
      VulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

      VulkanGraphicsPipeline.Initialize;

      VulkanGraphicsPipeline.FreeMemory;

     finally
      fVulkanGraphicsPipelines[DepthPrePass,AlphaMode,PrimitiveTopology,DoubleSided]:=VulkanGraphicsPipeline;
     end;

    end;

   end;

  end;

 end;

 fSkyBox.AfterCreateSwapChain(fVulkanRenderPass,
                              fParent.fWidth,
                              fParent.fHeight,
                              fParent.fVulkanSampleCountFlagBits);

end;

procedure TScreenMain.TForwardRenderPass.BeforeDestroySwapChain;
var Index:TpvSizeInt;
    DepthPrePass:boolean;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
begin
 fSkyBox.BeforeDestroySwapChain;
 for DepthPrePass:=false to fParent.fUseDepthPrepass do begin
  for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
   for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
    for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
     FreeAndNil(fVulkanGraphicsPipelines[DepthPrePass,AlphaMode,PrimitiveTopology,DoubleSided]);
    end;
   end;
  end;
 end;
 FreeAndNil(fVulkanPipelineLayout);
 for Index:=0 to fParent.fCountInFlightFrames-1 do begin
  FreeAndNil(fGlobalVulkanDescriptorSets[Index]);
 end;
 FreeAndNil(fGlobalVulkanDescriptorPool);
 FreeAndNil(fGlobalVulkanDescriptorSetLayout);
 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TForwardRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TForwardRenderPass.OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                  const aPipelineLayout:TpvVulkanPipelineLayout;
                                                                  const aRenderPassIndex:TpvSizeInt;
                                                                  const aPreviousInFlightFrameIndex:TpvSizeInt;
                                                                  const aInFlightFrameIndex:TpvSizeInt);
begin
 if not fOnSetRenderPassResourcesDone then begin
  fOnSetRenderPassResourcesDone:=true;
  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                       fVulkanPipelineLayout.Handle,
                                       1,
                                       1,
                                       @fGlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil);
 end;
end;

procedure TScreenMain.TForwardRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                 const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var InFlightFrameState:TScreenMain.PInFlightFrameState;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 InFlightFrameState:=@fParent.fInFlightFrameStates[aInFlightFrameIndex];

 if InFlightFrameState^.Ready then begin

{}fSkyBox.Draw(aInFlightFrameIndex,
               InFlightFrameState^.FinalViewIndex,
               InFlightFrameState^.CountViews,
               aCommandBuffer);//{}

  if true then begin

   fOnSetRenderPassResourcesDone:=false;

(* if fParent.fUseDepthPrepass then begin

    fParent.fScene3D.Draw(fVulkanGraphicsPipelines[true,TpvScene3D.TMaterial.TAlphaMode.Opaque],
                          -1,
                          aInFlightFrameIndex,
                          0,
                          InFlightFrameState^.FinalViewIndex,
                          InFlightFrameState^.CountViews,
                          aCommandBuffer,
                          fVulkanPipelineLayout,
                          OnSetRenderPassResources,
                          [TpvScene3D.TMaterial.TAlphaMode.Opaque]);

 {  if fParent.fVulkanSampleCountFlagBits=VK_SAMPLE_COUNT_1_BIT then begin
     fParent.fScene3D.Draw(fVulkanGraphicsPipelines[true,TpvScene3D.TMaterial.TAlphaMode.Mask],
                           aInFlightFrameIndex,
                           0,
                           InFlightFrameState^.FinalViewIndex,
                           InFlightFrameState^.CountViews,
                           aCommandBuffer,
                           fVulkanPipelineLayout,
                           OnSetRenderPassResources,
                           [TpvScene3D.TMaterial.TAlphaMode.Mask]);
    end;}

   end;   *)

   fParent.fScene3D.Draw(fVulkanGraphicsPipelines[false,TpvScene3D.TMaterial.TAlphaMode.Opaque],
                         -1,
                         aInFlightFrameIndex,
                         0,
                         InFlightFrameState^.FinalViewIndex,
                         InFlightFrameState^.CountViews,
                         aCommandBuffer,
                         fVulkanPipelineLayout,
                         OnSetRenderPassResources,
                         [TpvScene3D.TMaterial.TAlphaMode.Opaque]);

  if ((fParent.fTransparencyMode=TTransparencyMode.Direct) and not fParent.fScene3D.HasTransmission) or not fParent.fUseOITAlphaTest then begin
   fParent.fScene3D.Draw(fVulkanGraphicsPipelines[false,TpvScene3D.TMaterial.TAlphaMode.Mask],
                         -1,
                         aInFlightFrameIndex,
                         0,
                         InFlightFrameState^.FinalViewIndex,
                         InFlightFrameState^.CountViews,
                         aCommandBuffer,
                         fVulkanPipelineLayout,
                         OnSetRenderPassResources,
                         [TpvScene3D.TMaterial.TAlphaMode.Mask]);
  end;

 { if fParent.fUseDepthPrepass then begin

    fParent.fScene3D.Draw(fVulkanGraphicsPipelines[true,TpvScene3D.TMaterial.TAlphaMode.Mask],
                          -1,
                          aInFlightFrameIndex,
                          0,
                          InFlightFrameState^.FinalViewIndex,
                          InFlightFrameState^.CountViews,
                          aCommandBuffer,
                          fVulkanPipelineLayout,
                          OnSetRenderPassResources,
                          [TpvScene3D.TMaterial.TAlphaMode.Mask]);

   end;}

  end;

 end;

end;

{ TScreenMain.TForwardRenderMipMapComputePass }

constructor TScreenMain.TForwardRenderMipMapComputePass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin
 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='ForwardRenderMipMapComputePass';

 fResourceInput:=AddImageInput('resourcetype_color_optimized_non_alpha',
                               'resource_forwardrendering_color',
                               VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                               [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                              );

end;

destructor TScreenMain.TForwardRenderMipMapComputePass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TForwardRenderMipMapComputePass.Show;
var Stream:TStream;
    Format:string;
begin

 inherited Show;

 case fParent.fOptimizedNonAlphaFormat of
  VK_FORMAT_B10G11R11_UFLOAT_PACK32:begin
   Format:='r11g11b10f';
  end;
  VK_FORMAT_R16G16B16A16_SFLOAT:begin
   Format:='rgba16f';
  end;
  else begin
   Assert(false);
   Format:='';
  end;
 end;

 if fParent.fCountSurfaceViews>1 then begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/downsample_'+Format+'_multiview_level0_comp.spv');
 end else begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/downsample_'+Format+'_level0_comp.spv');
 end;
 try
  fDownsampleLevel0ComputeShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fParent.fCountSurfaceViews>1 then begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/downsample_'+Format+'_multiview_level1_comp.spv');
 end else begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/downsample_'+Format+'_level1_comp.spv');
 end;
 try
  fDownsampleLevel1ComputeShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fParent.fCountSurfaceViews>1 then begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/downsample_'+Format+'_multiview_level2_comp.spv');
 end else begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/downsample_'+Format+'_level2_comp.spv');
 end;
 try
  fDownsampleLevel2ComputeShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageDownsampleLevel0Compute:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fDownsampleLevel0ComputeShaderModule,'main');

 fVulkanPipelineShaderStageDownsampleLevel1Compute:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fDownsampleLevel1ComputeShaderModule,'main');

 fVulkanPipelineShaderStageDownsampleLevel2Compute:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fDownsampleLevel2ComputeShaderModule,'main');

end;

procedure TScreenMain.TForwardRenderMipMapComputePass.Hide;
begin
 FreeAndNil(fVulkanPipelineShaderStageDownsampleLevel2Compute);
 FreeAndNil(fVulkanPipelineShaderStageDownsampleLevel1Compute);
 FreeAndNil(fVulkanPipelineShaderStageDownsampleLevel0Compute);
 FreeAndNil(fDownsampleLevel2ComputeShaderModule);
 FreeAndNil(fDownsampleLevel1ComputeShaderModule);
 FreeAndNil(fDownsampleLevel0ComputeShaderModule);
 inherited Hide;
end;

procedure TScreenMain.TForwardRenderMipMapComputePass.AfterCreateSwapChain;
var InFlightFrameIndex,MipMapLevelIndex:TpvInt32;
    ImageViewType:TVkImageViewType;
begin

 inherited AfterCreateSwapChain;

 fVulkanSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         0.0,
                                         false,
                                         0.0,
                                         false,
                                         VK_COMPARE_OP_ALWAYS,
                                         0.0,
                                         0.0,
                                         VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK,
                                         false);

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fParent.fCountInFlightFrames*fParent.fForwardMipmappedArray2DImages[0].MipMapLevels);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fParent.fCountInFlightFrames*fParent.fForwardMipmappedArray2DImages[0].MipMapLevels);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,fParent.fCountInFlightFrames*fParent.fForwardMipmappedArray2DImages[0].MipMapLevels);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(1,
                                       VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fPipelineLayout.Initialize;

 fPipelineLevel0:=TpvVulkanComputePipeline.Create(pvApplication.VulkanDevice,
                                                  pvApplication.VulkanPipelineCache,
                                                  0,
                                                  fVulkanPipelineShaderStageDownsampleLevel0Compute,
                                                  fPipelineLayout,
                                                  nil,
                                                  0);

 fPipelineLevel1:=TpvVulkanComputePipeline.Create(pvApplication.VulkanDevice,
                                                  pvApplication.VulkanPipelineCache,
                                                  0,
                                                  fVulkanPipelineShaderStageDownsampleLevel1Compute,
                                                  fPipelineLayout,
                                                  nil,
                                                  0);

 fPipelineLevel2:=TpvVulkanComputePipeline.Create(pvApplication.VulkanDevice,
                                                  pvApplication.VulkanPipelineCache,
                                                  0,
                                                  fVulkanPipelineShaderStageDownsampleLevel2Compute,
                                                  fPipelineLayout,
                                                  nil,
                                                  0);

 if fParent.fCountSurfaceViews>1 then begin
  ImageViewType:=TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D_ARRAY);
 end else begin
  ImageViewType:=TVkImageViewType(VK_IMAGE_VIEW_TYPE_2D);
 end;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(pvApplication.VulkanDevice,
                                                                   fResourceInput.VulkanImages[InFlightFrameIndex],
                                                                   ImageViewType,
                                                                   TpvFrameGraph.TImageResourceType(fResourceInput.ResourceType).Format,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                   0,
                                                                   1,
                                                                   0,
                                                                   fParent.fCountSurfaceViews
                                                                  );
  for MipMapLevelIndex:=0 to fParent.fForwardMipmappedArray2DImages[InFlightFrameIndex].MipMapLevels-1 do begin
   fVulkanDescriptorSets[InFlightFrameIndex,MipMapLevelIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                                             fVulkanDescriptorSetLayout);
   if MipMapLevelIndex=0 then begin
    fVulkanDescriptorSets[InFlightFrameIndex,MipMapLevelIndex].WriteToDescriptorSet(0,
                                                                                    0,
                                                                                    1,
                                                                                    TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                                    [TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                                                   fVulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                                   fResourceInput.ResourceTransition.Layout)],
                                                                                    [],
                                                                                    [],
                                                                                    false
                                                                                   );
   end else begin
    fVulkanDescriptorSets[InFlightFrameIndex,MipMapLevelIndex].WriteToDescriptorSet(0,
                                                                                    0,
                                                                                    1,
                                                                                    TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                                    [TVkDescriptorImageInfo.Create(fParent.fForwardMipmappedArray2DImages[InFlightFrameIndex].VulkanMipMapSampler.Handle,
                                                                                                                   fParent.fForwardMipmappedArray2DImages[InFlightFrameIndex].DescriptorImageInfos[MipMapLevelIndex-1].imageView,
                                                                                                                   VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                                    [],
                                                                                    [],
                                                                                    false
                                                                                   );
   end;
   fVulkanDescriptorSets[InFlightFrameIndex,MipMapLevelIndex].WriteToDescriptorSet(1,
                                                                                   0,
                                                                                   1,
                                                                                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                                   [TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                                                  fParent.fForwardMipmappedArray2DImages[InFlightFrameIndex].DescriptorImageInfos[MipMapLevelIndex].imageView,
                                                                                                                  VK_IMAGE_LAYOUT_GENERAL)],
                                                                                   [],
                                                                                   [],
                                                                                   false
                                                                                  );
   fVulkanDescriptorSets[InFlightFrameIndex,MipMapLevelIndex].Flush;
  end;
 end;

end;

procedure TScreenMain.TForwardRenderMipMapComputePass.BeforeDestroySwapChain;
var InFlightFrameIndex,MipMapLevelIndex:TpvInt32;
begin
 FreeAndNil(fPipelineLevel2);
 FreeAndNil(fPipelineLevel1);
 FreeAndNil(fPipelineLevel0);
 FreeAndNil(fPipelineLayout);
 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  for MipMapLevelIndex:=0 to fParent.fForwardMipmappedArray2DImages[InFlightFrameIndex].MipMapLevels-1 do begin
   FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex,MipMapLevelIndex]);
  end;
  FreeAndNil(fVulkanImageViews[InFlightFrameIndex]);
 end;
 FreeAndNil(fVulkanDescriptorSetLayout);
 FreeAndNil(fVulkanDescriptorPool);
 FreeAndNil(fVulkanSampler);
 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TForwardRenderMipMapComputePass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TForwardRenderMipMapComputePass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var InFlightFrameIndex,MipMapLevelIndex:TpvInt32;
    Pipeline:TpvVulkanComputePipeline;
    ImageMemoryBarrier:TVkImageMemoryBarrier;
begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 InFlightFrameIndex:=aInFlightFrameIndex;

 FillChar(ImageMemoryBarrier,SizeOf(TVkImageMemoryBarrier),#0);
 ImageMemoryBarrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
 ImageMemoryBarrier.pNext:=nil;
 ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT);
 ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
 ImageMemoryBarrier.oldLayout:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
 ImageMemoryBarrier.newLayout:=VK_IMAGE_LAYOUT_GENERAL;
 ImageMemoryBarrier.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 ImageMemoryBarrier.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 ImageMemoryBarrier.image:=fParent.fForwardMipmappedArray2DImages[InFlightFrameIndex].VulkanImage.Handle;
 ImageMemoryBarrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 ImageMemoryBarrier.subresourceRange.baseMipLevel:=0;
 ImageMemoryBarrier.subresourceRange.levelCount:=fParent.fForwardMipmappedArray2DImages[InFlightFrameIndex].MipMapLevels;
 ImageMemoryBarrier.subresourceRange.baseArrayLayer:=0;
 ImageMemoryBarrier.subresourceRange.layerCount:=fParent.fCountSurfaceViews;
 aCommandBuffer.CmdPipelineBarrier(fFrameGraph.VulkanDevice.PhysicalDevice.PipelineStageAllShaderBits,
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@ImageMemoryBarrier);

 for MipMapLevelIndex:=0 to fParent.fForwardMipmappedArray2DImages[InFlightFrameIndex].MipMapLevels-1 do begin

  case MipMapLevelIndex of
   0:begin
    Pipeline:=fPipelineLevel0;
   end;
   1:begin
    Pipeline:=fPipelineLevel1;
   end;
   else begin
    Pipeline:=fPipelineLevel2;
   end;
  end;

  if MipMapLevelIndex<3 then begin
   aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,Pipeline.Handle);
  end;

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       fPipelineLayout.Handle,
                                       0,
                                       1,
                                       @fVulkanDescriptorSets[InFlightFrameIndex,MipMapLevelIndex].Handle,
                                       0,
                                       nil);

  aCommandBuffer.CmdDispatch(Max(1,(fParent.fWidth+((1 shl (4+MipMapLevelIndex))-1)) shr (4+MipMapLevelIndex)),
                             Max(1,(fParent.fHeight+((1 shl (4+MipMapLevelIndex))-1)) shr (4+MipMapLevelIndex)),
                             fParent.fCountSurfaceViews);

  FillChar(ImageMemoryBarrier,SizeOf(TVkImageMemoryBarrier),#0);
  ImageMemoryBarrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
  ImageMemoryBarrier.pNext:=nil;
  ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
  ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT);
  ImageMemoryBarrier.oldLayout:=VK_IMAGE_LAYOUT_GENERAL;
  ImageMemoryBarrier.newLayout:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
  ImageMemoryBarrier.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  ImageMemoryBarrier.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  ImageMemoryBarrier.image:=fParent.fForwardMipmappedArray2DImages[InFlightFrameIndex].VulkanImage.Handle;
  ImageMemoryBarrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
  ImageMemoryBarrier.subresourceRange.baseMipLevel:=MipMapLevelIndex;
  ImageMemoryBarrier.subresourceRange.levelCount:=1;
  ImageMemoryBarrier.subresourceRange.baseArrayLayer:=0;
  ImageMemoryBarrier.subresourceRange.layerCount:=fParent.fCountSurfaceViews;
  if (MipMapLevelIndex+1)<fParent.fForwardMipmappedArray2DImages[InFlightFrameIndex].MipMapLevels then begin
   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                     0,
                                     0,nil,
                                     0,nil,
                                     1,@ImageMemoryBarrier);
  end else begin
   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                     fFrameGraph.VulkanDevice.PhysicalDevice.PipelineStageAllShaderBits,
                                     0,
                                     0,nil,
                                     0,nil,
                                     1,@ImageMemoryBarrier);
  end;

 end;

end;

{ TScreenMain.TDirectTransparencyRenderPass }

constructor TScreenMain.TDirectTransparencyRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin
inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='DirectTransparencyRendering';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 fResourceCascadedShadowMap:=AddImageInput('resourcetype_cascadedshadowmap_data',
                                           'resource_cascadedshadowmap_data_final',
                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                           []
                                          );

 fResourceSSAO:=AddImageInput('resourcetype_ssao_final',
                              'resource_ssao_data_final',
                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                              []
                             );

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin

  fResourceDepth:=AddImageDepthInput('resourcetype_depth',
                                     'resource_depth_data',
                                     VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,//VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

  fResourceColor:=AddImageOutput('resourcetype_color',
                                 'resource_orderindependenttransparency_tailblending_color',
                                 VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                              TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

 end else begin

  fResourceDepth:=AddImageDepthInput('resourcetype_msaa_depth',
                                     'resource_msaa_depth_data',
                                     VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,//VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

  fResourceColor:=AddImageOutput('resourcetype_msaa_color',
                                 'resource_orderindependenttransparency_tailblending_msaa_color',
                                 VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                              TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

  fResourceColor:=AddImageResolveOutput('resourcetype_color',
                                        'resource_orderindependenttransparency_tailblending_color',
                                        'resource_orderindependenttransparency_tailblending_msaa_color',
                                        VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                        TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare,
                                                                     TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                                        [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                       );

 end;

end;

destructor TScreenMain.TDirectTransparencyRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TDirectTransparencyRenderPass.Show;
var Index:TpvSizeInt;
    Stream:TStream;
begin
 inherited Show;

 fVulkanGraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.GraphicsQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanGraphicsCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_vert.spv');
 try
  fMeshVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
//fFrameGraph.VulkanDevice.DebugMarker.SetObjectName(fMeshVertexShaderModule.Handle,TVkDebugReportObjectTypeEXT.VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT,'fMeshVertexShaderModule');
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_frag.spv');
 try
  fMeshFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fParent.fUseDemote then begin
  if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_blend_masked_demote_frag.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_blend_masked_demote_msaa_frag.spv');
  end;
 end else if fParent.fUseNoDiscard then begin
  if fParent.fZFar<0.0 then begin
   if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
    Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_blend_masked_nodiscard_reversedz_frag.spv');
   end else begin
    Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_blend_masked_nodiscard_reversedz_msaa_frag.spv');
   end;
  end else begin
   if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
    Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_blend_masked_nodiscard_frag.spv');
   end else begin
    Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_blend_masked_nodiscard_msaa_frag.spv');
   end;
  end;
 end else begin
  if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_blend_masked_frag.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_blend_masked_msaa_frag.spv');
  end;
 end;
 try
  fMeshMaskedFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageMeshVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fMeshVertexShaderModule,'main');

 fVulkanPipelineShaderStageMeshFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshFragmentShaderModule,'main');

 fVulkanPipelineShaderStageMeshMaskedFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshMaskedFragmentShaderModule,'main');

 fVulkanCascadedShadowMapSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                                          TVkFilter.VK_FILTER_LINEAR,
                                                          TVkFilter.VK_FILTER_LINEAR,
                                                          TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          0.0,
                                                          false,
                                                          0.0,
                                                          false,
                                                          VK_COMPARE_OP_ALWAYS,
                                                          0.0,
                                                          0.0,
                                                          VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE,
                                                          false);

 fVulkanSSAOSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                             TVkFilter.VK_FILTER_LINEAR,
                                             TVkFilter.VK_FILTER_LINEAR,
                                             TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             0.0,
                                             false,
                                             0.0,
                                             false,
                                             VK_COMPARE_OP_ALWAYS,
                                             0.0,
                                             0.0,
                                             VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE,
                                             false);

end;

procedure TScreenMain.TDirectTransparencyRenderPass.Hide;
begin

 FreeAndNil(fVulkanCascadedShadowMapSampler);

 FreeAndNil(fVulkanSSAOSampler);

 FreeAndNil(fVulkanPipelineShaderStageMeshVertex);

 FreeAndNil(fVulkanPipelineShaderStageMeshFragment);

 FreeAndNil(fVulkanPipelineShaderStageMeshMaskedFragment);

 FreeAndNil(fMeshVertexShaderModule);

 FreeAndNil(fMeshFragmentShaderModule);

 FreeAndNil(fMeshMaskedFragmentShaderModule);

 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 FreeAndNil(fVulkanGraphicsCommandBufferFence);
 FreeAndNil(fVulkanGraphicsCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TDirectTransparencyRenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
    VulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
begin

 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fGlobalVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fGlobalVulkanDescriptorSetLayout.AddBinding(0,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             3,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(1,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             3,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(2,
                                             VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(3,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(4,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             2,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(5,
                                             VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.Initialize;

 fGlobalVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,9*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,2*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,1*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,2*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fGlobalVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fGlobalVulkanDescriptorPool,
                                                                                 fGlobalVulkanDescriptorSetLayout);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                       0,
                                                                       3,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [fParent.fGGXBRDF.DescriptorImageInfo,
                                                                        fParent.fCharlieBRDF.DescriptorImageInfo,
                                                                        fParent.fSheenELUT.DescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                       0,
                                                                       3,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [fParent.fGGXEnvMapCubeMap.DescriptorImageInfo,
                                                                        fParent.fCharlieEnvMapCubeMap.DescriptorImageInfo,
                                                                        fParent.fLambertianEnvMapCubeMap.DescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                       [],
                                                                       [fParent.fCascadedShadowMapVulkanUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [TVkDescriptorImageInfo.Create(fVulkanCascadedShadowMapSampler.Handle,
                                                                                                      fResourceCascadedShadowMap.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceCascadedShadowMap.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                                       0,
                                                                       2,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [TVkDescriptorImageInfo.Create(fVulkanSSAOSampler.Handle,
                                                                                                      fResourceSSAO.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceSSAO.ResourceTransition.Layout), // TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                        fParent.fForwardMipmappedArray2DImages[InFlightFrameIndex].ArrayDescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(5,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                       [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                      fResourceDepth.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceDepth.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT),0,SizeOf(TpvScene3D.TVertexStagePushConstants));
 fVulkanPipelineLayout.AddDescriptorSetLayout(fParent.fScene3D.GlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin

  if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Opaque then begin
   continue;
  end;

  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin

   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin

    VulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                             pvApplication.VulkanPipelineCache,
                                                             0,
                                                             [],
                                                             fVulkanPipelineLayout,
                                                             fVulkanRenderPass,
                                                             VulkanRenderPassSubpassIndex,
                                                             nil,
                                                             0);

    try

     VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshVertex);
     if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask then begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshMaskedFragment);
     end else begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshFragment);
     end;

     VulkanGraphicsPipeline.InputAssemblyState.Topology:=TVkPrimitiveTopology(PrimitiveTopology);
     VulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

     fParent.fScene3D.InitializeGraphicsPipeline(VulkanGraphicsPipeline);

     VulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
     VulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

     VulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
     if DoubleSided then begin
      VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
     end else begin
      VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
     end;
     VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

     VulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=fParent.fVulkanSampleCountFlagBits;
     if (AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask) and (VulkanGraphicsPipeline.MultisampleState.RasterizationSamples<>VK_SAMPLE_COUNT_1_BIT) then begin
      VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=true;
      VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=1.0;
      VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
      VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=true;
      VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;
     end else begin
      VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=false;
      VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=0.0;
      VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
      VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=false;
      VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;
     end;

     VulkanGraphicsPipeline.ColorBlendState.LogicOpEnable:=false;
     VulkanGraphicsPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[0]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[1]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[2]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[3]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(true,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                         VK_BLEND_OP_ADD,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                         VK_BLEND_OP_ADD,
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

     VulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
     VulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=false;
     if fParent.fZFar<0.0 then begin
      VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_GREATER_OR_EQUAL;
     end else begin
      VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
     end;
     VulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
     VulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

     VulkanGraphicsPipeline.Initialize;

     VulkanGraphicsPipeline.FreeMemory;

    finally
     fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]:=VulkanGraphicsPipeline;
    end;

   end;

  end;

 end;

end;

procedure TScreenMain.TDirectTransparencyRenderPass.BeforeDestroySwapChain;
var Index:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
begin
 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;
 FreeAndNil(fVulkanPipelineLayout);
 for Index:=0 to fParent.fCountInFlightFrames-1 do begin
  FreeAndNil(fGlobalVulkanDescriptorSets[Index]);
 end;
 FreeAndNil(fGlobalVulkanDescriptorPool);
 FreeAndNil(fGlobalVulkanDescriptorSetLayout);
 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TDirectTransparencyRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TDirectTransparencyRenderPass.OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                             const aPipelineLayout:TpvVulkanPipelineLayout;
                                                                             const aRenderPassIndex:TpvSizeInt;
                                                                             const aPreviousInFlightFrameIndex:TpvSizeInt;
                                                                             const aInFlightFrameIndex:TpvSizeInt);
begin
 if not fOnSetRenderPassResourcesDone then begin
  fOnSetRenderPassResourcesDone:=true;
  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                       fVulkanPipelineLayout.Handle,
                                       1,
                                       1,
                                       @fGlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil);
 end;
end;

procedure TScreenMain.TDirectTransparencyRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                            const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var InFlightFrameState:TScreenMain.PInFlightFrameState;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 InFlightFrameState:=@fParent.fInFlightFrameStates[aInFlightFrameIndex];

 if InFlightFrameState^.Ready then begin

  fOnSetRenderPassResourcesDone:=false;

  if fParent.fScene3D.HasTransmission {or fParent.fUseOITAlphaTest} then begin
   fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Mask],
                         -1,
                         aInFlightFrameIndex,
                         0,
                         InFlightFrameState^.FinalViewIndex,
                         InFlightFrameState^.CountViews,
                         aCommandBuffer,
                         fVulkanPipelineLayout,
                         OnSetRenderPassResources,
                         [TpvScene3D.TMaterial.TAlphaMode.Mask]);
  end;

  fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Blend],
                        -1,
                        aInFlightFrameIndex,
                        0,
                        InFlightFrameState^.FinalViewIndex,
                        InFlightFrameState^.CountViews,
                        aCommandBuffer,
                        fVulkanPipelineLayout,
                        OnSetRenderPassResources,
                        [TpvScene3D.TMaterial.TAlphaMode.Blend]);

 end;

end;

{ TScreenMain.TDirectTransparencyResolveRenderPass }

constructor TScreenMain.TDirectTransparencyResolveRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='DirectTransparencyResolve';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

//SeparatePhysicalPass:=true;

//SeparateCommandBuffer:=true;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 fResourceOpaque:=AddImageInput('resourcetype_color_optimized_non_alpha',
                                'resource_forwardrendering_color',
                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                               );

 fResourceTransparent:=AddImageInput('resourcetype_color',
                                     'resource_orderindependenttransparency_tailblending_color',
                                     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

 fResourceSurface:=AddImageOutput('resourcetype_color_optimized_non_alpha',
                                  'resource_combinedopaquetransparency_final_color',
                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                  TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                               TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );

end;

destructor TScreenMain.TDirectTransparencyResolveRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TDirectTransparencyResolveRenderPass.Show;
var Stream:TStream;
begin

 inherited Show;

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/fullscreen_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/blend_resolve_frag.spv');
 end else begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/blend_resolve_msaa_frag.spv');
 end;
 try
  fVulkanFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVulkanVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fVulkanFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

end;

procedure TScreenMain.TDirectTransparencyResolveRenderPass.Hide;
begin
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TDirectTransparencyResolveRenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin
 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,2*fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,1*fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,1*fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,1*fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(1,
                                       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(2,
                                       VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(3,
                                       VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(4,
                                       VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                fResourceOpaque.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceOpaque.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                fResourceTransparent.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceTransparent.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                           pvApplication.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           fVulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           VulkanRenderPassSubpassIndex,
                                                           nil,
                                                           0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
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
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_DST_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_ONE,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_OP_ADD,
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

end;

procedure TScreenMain.TDirectTransparencyResolveRenderPass.BeforeDestroySwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 fVulkanRenderPass:=nil;

 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TDirectTransparencyResolveRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TDirectTransparencyResolveRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      fVulkanPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,0,nil);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
 aCommandBuffer.CmdDraw(3,1,0,0);
end;

{ TScreenMain.TLockOrderIndependentTransparencyClearCustomPass }

constructor TScreenMain.TLockOrderIndependentTransparencyClearCustomPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin
 inherited Create(aFrameGraph);
 fParent:=aParent;
 Name:='LockOrderIndependentTransparencyClearCustomPass';
end;

destructor TScreenMain.TLockOrderIndependentTransparencyClearCustomPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TLockOrderIndependentTransparencyClearCustomPass.Show;
begin
 inherited Show;
end;

procedure TScreenMain.TLockOrderIndependentTransparencyClearCustomPass.Hide;
begin
 inherited Hide;
end;

procedure TScreenMain.TLockOrderIndependentTransparencyClearCustomPass.AfterCreateSwapChain;
begin
 inherited AfterCreateSwapChain;
end;

procedure TScreenMain.TLockOrderIndependentTransparencyClearCustomPass.BeforeDestroySwapChain;
begin
 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TLockOrderIndependentTransparencyClearCustomPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TLockOrderIndependentTransparencyClearCustomPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var ClearValue:TVkClearColorValue;
    ImageSubresourceRange:TVkImageSubresourceRange;
    BufferMemoryBarrier:TVkBufferMemoryBarrier;
    ImageMemoryBarriers:array[0..1] of TVkImageMemoryBarrier;
    CountImageMemoryBarriers:TpvInt32;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 ClearValue.uint32[0]:=0;
 ClearValue.uint32[1]:=0;
 ClearValue.uint32[2]:=0;
 ClearValue.uint32[3]:=0;

 ImageSubresourceRange:=TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),0,1,0,fParent.fCountSurfaceViews);

 aCommandBuffer.CmdClearColorImage(fParent.fLockOrderIndependentTransparencyAuxImages[aInFlightFrameIndex].VulkanImage.Handle,
                                   VK_IMAGE_LAYOUT_GENERAL,
                                   @ClearValue,
                                   1,
                                   @ImageSubresourceRange);

 if fParent.fTransparencyMode=TTransparencyMode.SPINLOCKOIT then begin
  aCommandBuffer.CmdClearColorImage(fParent.fLockOrderIndependentTransparencySpinLockImages[aInFlightFrameIndex].VulkanImage.Handle,
                                    VK_IMAGE_LAYOUT_GENERAL,
                                    @ClearValue,
                                    1,
                                    @ImageSubresourceRange);
 end;

 BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                    TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    0,
                                                    0,
                                                    fParent.fLockOrderIndependentTransparencyABufferBuffers[aInFlightFrameIndex].VulkanBuffer.Handle,
                                                    0,
                                                    VK_WHOLE_SIZE);

 ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                      TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      TVkImageLayout(VK_IMAGE_LAYOUT_GENERAL),
                                                      TVkImageLayout(VK_IMAGE_LAYOUT_GENERAL),
                                                      0,
                                                      0,
                                                      fParent.fLockOrderIndependentTransparencyAuxImages[aInFlightFrameIndex].VulkanImage.Handle,
                                                      ImageSubresourceRange);

 if fParent.fTransparencyMode=TTransparencyMode.SPINLOCKOIT then begin
  ImageMemoryBarriers[1]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                       TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                       TVkImageLayout(VK_IMAGE_LAYOUT_GENERAL),
                                                       TVkImageLayout(VK_IMAGE_LAYOUT_GENERAL),
                                                       0,
                                                       0,
                                                       fParent.fLockOrderIndependentTransparencySpinLockImages[aInFlightFrameIndex].VulkanImage.Handle,
                                                       ImageSubresourceRange);
  CountImageMemoryBarriers:=2;
 end else begin
  CountImageMemoryBarriers:=1;
 end;

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                   TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT),
                                   0,
                                   nil,
                                   1,
                                   @BufferMemoryBarrier,
                                   CountImageMemoryBarriers,
                                   @ImageMemoryBarriers[0]);

end;

{ TScreenMain.TLockOrderIndependentTransparencyRenderPass }

constructor TScreenMain.TLockOrderIndependentTransparencyRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin
inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='LockOrderIndependentTransparencyRendering';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 fResourceCascadedShadowMap:=AddImageInput('resourcetype_cascadedshadowmap_data',
                                           'resource_cascadedshadowmap_data_final',
                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                           []
                                          );

 fResourceSSAO:=AddImageInput('resourcetype_ssao_final',
                              'resource_ssao_data_final',
                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                              []
                             );

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin

  fResourceDepth:=AddImageDepthInput('resourcetype_depth',
                                     'resource_depth_data',
                                     VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,//VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

  fResourceColor:=AddImageOutput('resourcetype_color',
                                 'resource_orderindependenttransparency_tailblending_color',
                                 VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                              TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

 end else begin

  fResourceDepth:=AddImageDepthInput('resourcetype_msaa_depth',
                                     'resource_msaa_depth_data',
                                     VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,//VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

  fResourceColor:=AddImageOutput('resourcetype_msaa_color',
                                 'resource_orderindependenttransparency_tailblending_msaa_color',
                                 VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                              TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

  fResourceColor:=AddImageResolveOutput('resourcetype_color',
                                        'resource_orderindependenttransparency_tailblending_color',
                                        'resource_orderindependenttransparency_tailblending_msaa_color',
                                        VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                        TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare,
                                                                     TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                                        [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                       );

 end;

end;

destructor TScreenMain.TLockOrderIndependentTransparencyRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TLockOrderIndependentTransparencyRenderPass.Show;
var Index:TpvSizeInt;
    Stream:TStream;
    OITVariant:TpvUTF8String;
begin
 inherited Show;

 fVulkanGraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.GraphicsQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanGraphicsCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_vert.spv');
 try
  fMeshVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
//fFrameGraph.VulkanDevice.DebugMarker.SetObjectName(fMeshVertexShaderModule.Handle,TVkDebugReportObjectTypeEXT.VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT,'fMeshVertexShaderModule');
 finally
  Stream.Free;
 end;

 case fParent.fTransparencyMode of
  TTransparencyMode.SPINLOCKOIT:begin
   OITVariant:='spinlock';
  end;
  TTransparencyMode.INTERLOCKOIT:begin
   OITVariant:='interlock';
  end;
  else begin
   Assert(false);
   OITVariant:='';
  end;
 end;

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  if fParent.fZFar<0.0 then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_lockoit_'+OITVariant+'_reversedz_frag.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_lockoit_'+OITVariant+'_frag.spv');
  end;
 end else begin
  if fParent.fZFar<0.0 then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_lockoit_'+OITVariant+'_reversedz_msaa_frag.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_lockoit_'+OITVariant+'_msaa_frag.spv');
  end;
 end;
 try
  fMeshFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
//fFrameGraph.VulkanDevice.DebugMarker.SetObjectName(fMeshFragmentShaderModule.Handle,TVkDebugReportObjectTypeEXT.VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT,'fMeshFragmentShaderModule');
 finally
  Stream.Free;
 end;

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  if fParent.fZFar<0.0 then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_lockoit_'+OITVariant+'_reversedz_masked_frag.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_lockoit_'+OITVariant+'_masked_frag.spv');
  end;
 end else begin
  if fParent.fZFar<0.0 then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_lockoit_'+OITVariant+'_reversedz_masked_msaa_frag.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_lockoit_'+OITVariant+'_masked_msaa_frag.spv');
  end;
 end;
 try
  fMeshMaskedFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
//fFrameGraph.VulkanDevice.DebugMarker.SetObjectName(fMeshMaskedFragmentShaderModule.Handle,TVkDebugReportObjectTypeEXT.VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT,'fMeshMaskedFragmentShaderModule');
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageMeshVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fMeshVertexShaderModule,'main');

 fVulkanPipelineShaderStageMeshFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshFragmentShaderModule,'main');

 fVulkanPipelineShaderStageMeshMaskedFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshMaskedFragmentShaderModule,'main');

 fVulkanCascadedShadowMapSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                                          TVkFilter.VK_FILTER_LINEAR,
                                                          TVkFilter.VK_FILTER_LINEAR,
                                                          TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          0.0,
                                                          false,
                                                          0.0,
                                                          false,
                                                          VK_COMPARE_OP_ALWAYS,
                                                          0.0,
                                                          0.0,
                                                          VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE,
                                                          false);

 fVulkanSSAOSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                             TVkFilter.VK_FILTER_LINEAR,
                                             TVkFilter.VK_FILTER_LINEAR,
                                             TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             0.0,
                                             false,
                                             0.0,
                                             false,
                                             VK_COMPARE_OP_ALWAYS,
                                             0.0,
                                             0.0,
                                             VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE,
                                             false);


end;

procedure TScreenMain.TLockOrderIndependentTransparencyRenderPass.Hide;
begin

 FreeAndNil(fVulkanCascadedShadowMapSampler);

 FreeAndNil(fVulkanSSAOSampler);

 FreeAndNil(fVulkanPipelineShaderStageMeshVertex);

 FreeAndNil(fVulkanPipelineShaderStageMeshFragment);

 FreeAndNil(fVulkanPipelineShaderStageMeshMaskedFragment);

 FreeAndNil(fMeshVertexShaderModule);

 FreeAndNil(fMeshFragmentShaderModule);

 FreeAndNil(fMeshMaskedFragmentShaderModule);

 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 FreeAndNil(fVulkanGraphicsCommandBufferFence);
 FreeAndNil(fVulkanGraphicsCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TLockOrderIndependentTransparencyRenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
    VulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
begin

 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fGlobalVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fGlobalVulkanDescriptorSetLayout.AddBinding(0,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             3,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(1,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             3,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(2,
                                             VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(3,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(4,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             2,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(5,
                                             VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(6,
                                             VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(7,
                                             VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 case fParent.fTransparencyMode of
  TTransparencyMode.SPINLOCKOIT:begin
   fGlobalVulkanDescriptorSetLayout.AddBinding(8,
                                               VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                               1,
                                               TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                               []);
   fGlobalVulkanDescriptorSetLayout.AddBinding(9,
                                               VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                               1,
                                               TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                               []);
  end;
  TTransparencyMode.INTERLOCKOIT:begin
   fGlobalVulkanDescriptorSetLayout.AddBinding(8,
                                               VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                               1,
                                               TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                               []);
  end;
  else begin
  end;
 end;
 fGlobalVulkanDescriptorSetLayout.Initialize;

 fGlobalVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,9*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,2*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,1*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,2*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fGlobalVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fGlobalVulkanDescriptorPool,
                                                                                 fGlobalVulkanDescriptorSetLayout);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                       0,
                                                                       3,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [fParent.fGGXBRDF.DescriptorImageInfo,
                                                                        fParent.fCharlieBRDF.DescriptorImageInfo,
                                                                        fParent.fSheenELUT.DescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                       0,
                                                                       3,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [fParent.fGGXEnvMapCubeMap.DescriptorImageInfo,
                                                                        fParent.fCharlieEnvMapCubeMap.DescriptorImageInfo,
                                                                        fParent.fLambertianEnvMapCubeMap.DescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                       [],
                                                                       [fParent.fCascadedShadowMapVulkanUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [TVkDescriptorImageInfo.Create(fVulkanCascadedShadowMapSampler.Handle,
                                                                                                      fResourceCascadedShadowMap.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceCascadedShadowMap.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                                       0,
                                                                       2,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [TVkDescriptorImageInfo.Create(fVulkanSSAOSampler.Handle,
                                                                                                      fResourceSSAO.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceSSAO.ResourceTransition.Layout), // TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                        fParent.fForwardMipmappedArray2DImages[InFlightFrameIndex].ArrayDescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(5,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                       [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                      fResourceDepth.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceDepth.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(6,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER),
                                                                       [],
                                                                       [],
                                                                       [fParent.fLockOrderIndependentTransparencyABufferBuffers[InFlightFrameIndex].VulkanBufferView.Handle],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(7,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                       [fParent.fLockOrderIndependentTransparencyAuxImages[InFlightFrameIndex].DescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  case fParent.fTransparencyMode of
   TTransparencyMode.SPINLOCKOIT:begin
    fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(8,
                                                                         0,
                                                                         1,
                                                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                         [fParent.fLockOrderIndependentTransparencySpinLockImages[InFlightFrameIndex].DescriptorImageInfo],
                                                                         [],
                                                                         [],
                                                                         false);
    fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(9,
                                                                         0,
                                                                         1,
                                                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                         [],
                                                                         [fParent.fLockOrderIndependentTransparentUniformVulkanBuffer.DescriptorBufferInfo],
                                                                         [],
                                                                         false);
   end;
   TTransparencyMode.INTERLOCKOIT:begin
    fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(8,
                                                                         0,
                                                                         1,
                                                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                         [],
                                                                         [fParent.fLockOrderIndependentTransparentUniformVulkanBuffer.DescriptorBufferInfo],
                                                                         [],
                                                                         false);
   end;
   else begin
   end;
  end;
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT),0,SizeOf(TpvScene3D.TVertexStagePushConstants));
 fVulkanPipelineLayout.AddDescriptorSetLayout(fParent.fScene3D.GlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin

  if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Opaque then begin
   continue;
  end;

  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin

   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin

    VulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                             pvApplication.VulkanPipelineCache,
                                                             0,
                                                             [],
                                                             fVulkanPipelineLayout,
                                                             fVulkanRenderPass,
                                                             VulkanRenderPassSubpassIndex,
                                                             nil,
                                                             0);

    try

     VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshVertex);
     if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask then begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshMaskedFragment);
     end else begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshFragment);
     end;

     VulkanGraphicsPipeline.InputAssemblyState.Topology:=TVkPrimitiveTopology(PrimitiveTopology);
     VulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

     fParent.fScene3D.InitializeGraphicsPipeline(VulkanGraphicsPipeline);

     VulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
     VulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

     VulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
     if DoubleSided then begin
      VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
     end else begin
      VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
     end;
     VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

     VulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=fParent.fVulkanSampleCountFlagBits;
     if (AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask) and (VulkanGraphicsPipeline.MultisampleState.RasterizationSamples<>VK_SAMPLE_COUNT_1_BIT) then begin
      VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=true;
      VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=1.0;
      VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
      VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=true;
      VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;
     end else begin
      VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=false;
      VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=0.0;
      VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
      VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=false;
      VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;
     end;

     VulkanGraphicsPipeline.ColorBlendState.LogicOpEnable:=false;
     VulkanGraphicsPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[0]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[1]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[2]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[3]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(true,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                         VK_BLEND_OP_ADD,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                         VK_BLEND_OP_ADD,
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

     VulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
     VulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=false;
     if fParent.fZFar<0.0 then begin
      VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_GREATER_OR_EQUAL;
     end else begin
      VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
     end;
     VulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
     VulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

     VulkanGraphicsPipeline.Initialize;

     VulkanGraphicsPipeline.FreeMemory;

    finally
     fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]:=VulkanGraphicsPipeline;
    end;

   end;

  end;

 end;

end;

procedure TScreenMain.TLockOrderIndependentTransparencyRenderPass.BeforeDestroySwapChain;
var Index:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
begin
 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;
 FreeAndNil(fVulkanPipelineLayout);
 for Index:=0 to fParent.fCountInFlightFrames-1 do begin
  FreeAndNil(fGlobalVulkanDescriptorSets[Index]);
 end;
 FreeAndNil(fGlobalVulkanDescriptorPool);
 FreeAndNil(fGlobalVulkanDescriptorSetLayout);
 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TLockOrderIndependentTransparencyRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TLockOrderIndependentTransparencyRenderPass.OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                                           const aPipelineLayout:TpvVulkanPipelineLayout;
                                                                                           const aRenderPassIndex:TpvSizeInt;
                                                                                           const aPreviousInFlightFrameIndex:TpvSizeInt;
                                                                                           const aInFlightFrameIndex:TpvSizeInt);
begin
 if not fOnSetRenderPassResourcesDone then begin
  fOnSetRenderPassResourcesDone:=true;
  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                       fVulkanPipelineLayout.Handle,
                                       1,
                                       1,
                                       @fGlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil);
 end;
end;

procedure TScreenMain.TLockOrderIndependentTransparencyRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                          const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var InFlightFrameState:TScreenMain.PInFlightFrameState;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 InFlightFrameState:=@fParent.fInFlightFrameStates[aInFlightFrameIndex];

 if InFlightFrameState^.Ready then begin

  fOnSetRenderPassResourcesDone:=false;

  if fParent.fUseOITAlphaTest then begin
   fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Mask],
                         -1,
                         aInFlightFrameIndex,
                         0,
                         InFlightFrameState^.FinalViewIndex,
                         InFlightFrameState^.CountViews,
                         aCommandBuffer,
                         fVulkanPipelineLayout,
                         OnSetRenderPassResources,
                         [TpvScene3D.TMaterial.TAlphaMode.Mask]);
  end;

  fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Blend],
                        -1,
                        aInFlightFrameIndex,
                        0,
                        InFlightFrameState^.FinalViewIndex,
                        InFlightFrameState^.CountViews,
                        aCommandBuffer,
                        fVulkanPipelineLayout,
                        OnSetRenderPassResources,
                        [TpvScene3D.TMaterial.TAlphaMode.Blend]);

 end;

end;

{ TScreenMain.TLockOrderIndependentTransparencyBarrierCustomPass }

constructor TScreenMain.TLockOrderIndependentTransparencyBarrierCustomPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin
 inherited Create(aFrameGraph);
 fParent:=aParent;
 Name:='LockOrderIndependentTransparencyBarrierCustomPass';
end;

destructor TScreenMain.TLockOrderIndependentTransparencyBarrierCustomPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TLockOrderIndependentTransparencyBarrierCustomPass.Show;
begin
 inherited Show;
end;

procedure TScreenMain.TLockOrderIndependentTransparencyBarrierCustomPass.Hide;
begin
 inherited Hide;
end;

procedure TScreenMain.TLockOrderIndependentTransparencyBarrierCustomPass.AfterCreateSwapChain;
begin
 inherited AfterCreateSwapChain;
end;

procedure TScreenMain.TLockOrderIndependentTransparencyBarrierCustomPass.BeforeDestroySwapChain;
begin
 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TLockOrderIndependentTransparencyBarrierCustomPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TLockOrderIndependentTransparencyBarrierCustomPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var BufferMemoryBarrier:TVkBufferMemoryBarrier;
    ImageSubresourceRange:TVkImageSubresourceRange;
    ImageMemoryBarriers:array[0..1] of TVkImageMemoryBarrier;
    CountImageMemoryBarriers:TpvInt32;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    0,
                                                    0,
                                                    fParent.fLockOrderIndependentTransparencyABufferBuffers[aInFlightFrameIndex].VulkanBuffer.Handle,
                                                    0,
                                                    VK_WHOLE_SIZE);

 ImageSubresourceRange:=TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),0,1,0,fParent.fCountSurfaceViews);

 ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      TVkImageLayout(VK_IMAGE_LAYOUT_GENERAL),
                                                      TVkImageLayout(VK_IMAGE_LAYOUT_GENERAL),
                                                      0,
                                                      0,
                                                      fParent.fLockOrderIndependentTransparencyAuxImages[aInFlightFrameIndex].VulkanImage.Handle,
                                                      ImageSubresourceRange);

 if fParent.fTransparencyMode=TTransparencyMode.SPINLOCKOIT then begin
  ImageMemoryBarriers[1]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                       TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                       TVkImageLayout(VK_IMAGE_LAYOUT_GENERAL),
                                                       TVkImageLayout(VK_IMAGE_LAYOUT_GENERAL),
                                                       0,
                                                       0,
                                                       fParent.fLockOrderIndependentTransparencySpinLockImages[aInFlightFrameIndex].VulkanImage.Handle,
                                                       ImageSubresourceRange);
  CountImageMemoryBarriers:=2;
 end else begin
  CountImageMemoryBarriers:=1;
 end;

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                   TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT),
                                   0,
                                   nil,
                                   1,
                                   @BufferMemoryBarrier,
                                   CountImageMemoryBarriers,
                                   @ImageMemoryBarriers[0]);

end;

{ TScreenMain.TLockOrderIndependentTransparencyResolveRenderPass }

constructor TScreenMain.TLockOrderIndependentTransparencyResolveRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='LockOrderIndependentTransparencyResolve';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

//SeparatePhysicalPass:=true;

//SeparateCommandBuffer:=true;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 fResourceOpaque:=AddImageInput('resourcetype_color_optimized_non_alpha',
                                'resource_forwardrendering_color',
                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                               );

 fResourceTransparent:=AddImageInput('resourcetype_color',
                                     'resource_orderindependenttransparency_tailblending_color',
                                     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

 fResourceSurface:=AddImageOutput('resourcetype_color_optimized_non_alpha',
                                  'resource_combinedopaquetransparency_final_color',
                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                  TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                               TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );

end;

destructor TScreenMain.TLockOrderIndependentTransparencyResolveRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TLockOrderIndependentTransparencyResolveRenderPass.Show;
var Stream:TStream;
begin

 inherited Show;

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/fullscreen_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  if fParent.fZFar<0.0 then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/lockoit_resolve_reversedz_frag.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/lockoit_resolve_frag.spv');
  end;
 end else begin
  if fParent.fZFar<0.0 then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/lockoit_resolve_reversedz_msaa_frag.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/lockoit_resolve_msaa_frag.spv');
  end;
 end;
 try
  fVulkanFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVulkanVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fVulkanFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

end;

procedure TScreenMain.TLockOrderIndependentTransparencyResolveRenderPass.Hide;
begin
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TLockOrderIndependentTransparencyResolveRenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin
 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,2*fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,1*fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,1*fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,1*fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(1,
                                       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(2,
                                       VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(3,
                                       VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(4,
                                       VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                fResourceOpaque.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceOpaque.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                fResourceTransparent.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceTransparent.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER),
                                                                 [],
                                                                 [],
                                                                 [fParent.fLockOrderIndependentTransparencyABufferBuffers[InFlightFrameIndex].VulkanBufferView.Handle],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                 [fParent.fLockOrderIndependentTransparencyAuxImages[InFlightFrameIndex].DescriptorImageInfo],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                 [],
                                                                 [fParent.fLockOrderIndependentTransparentUniformVulkanBuffer.DescriptorBufferInfo],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                           pvApplication.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           fVulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           VulkanRenderPassSubpassIndex,
                                                           nil,
                                                           0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
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
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_DST_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_ONE,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_OP_ADD,
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

end;

procedure TScreenMain.TLockOrderIndependentTransparencyResolveRenderPass.BeforeDestroySwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 fVulkanRenderPass:=nil;

 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TLockOrderIndependentTransparencyResolveRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TLockOrderIndependentTransparencyResolveRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      fVulkanPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,0,nil);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
 aCommandBuffer.CmdDraw(3,1,0,0);
end;

{ TScreenMain.TLoopOrderIndependentTransparencyClearCustomPass }

constructor TScreenMain.TLoopOrderIndependentTransparencyClearCustomPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin
 inherited Create(aFrameGraph);
 fParent:=aParent;
 Name:='LoopOrderIndependentTransparencyClearCustomPass';
end;

destructor TScreenMain.TLoopOrderIndependentTransparencyClearCustomPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyClearCustomPass.Show;
begin
 inherited Show;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyClearCustomPass.Hide;
begin
 inherited Hide;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyClearCustomPass.AfterCreateSwapChain;
begin
 inherited AfterCreateSwapChain;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyClearCustomPass.BeforeDestroySwapChain;
begin
 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyClearCustomPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyClearCustomPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var BufferMemoryBarriers:array[0..2] of TVkBufferMemoryBarrier;
    CountBufferMemoryBarriers:TpvInt32;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 aCommandBuffer.CmdFillBuffer(fParent.fLoopOrderIndependentTransparencyABufferBuffers[aInFlightFrameIndex].VulkanBuffer.Handle,
                              0,
                              VK_WHOLE_SIZE,
                              0);

 if fParent.fZFar<0 then begin
  aCommandBuffer.CmdFillBuffer(fParent.fLoopOrderIndependentTransparencyZBufferBuffers[aInFlightFrameIndex].VulkanBuffer.Handle,
                               0,
                               VK_WHOLE_SIZE,
                               0);
 end else begin
  aCommandBuffer.CmdFillBuffer(fParent.fLoopOrderIndependentTransparencyZBufferBuffers[aInFlightFrameIndex].VulkanBuffer.Handle,
                               0,
                               VK_WHOLE_SIZE,
                               $ffffffff);
 end;

 if fParent.fVulkanSampleCountFlagBits<>TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  aCommandBuffer.CmdFillBuffer(fParent.fLoopOrderIndependentTransparencySBufferBuffers[aInFlightFrameIndex].VulkanBuffer.Handle,
                               0,
                               VK_WHOLE_SIZE,
                               0);
 end;

 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        0,
                                                        0,
                                                        fParent.fLoopOrderIndependentTransparencyABufferBuffers[aInFlightFrameIndex].VulkanBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        0,
                                                        0,
                                                        fParent.fLoopOrderIndependentTransparencyZBufferBuffers[aInFlightFrameIndex].VulkanBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 if fParent.fVulkanSampleCountFlagBits<>TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  BufferMemoryBarriers[2]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         0,
                                                         0,
                                                         fParent.fLoopOrderIndependentTransparencySBufferBuffers[aInFlightFrameIndex].VulkanBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);
  CountBufferMemoryBarriers:=3;
 end else begin
  CountBufferMemoryBarriers:=2;
 end;

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                   TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT),
                                   0,
                                   nil,
                                   CountBufferMemoryBarriers,
                                   @BufferMemoryBarriers[0],
                                   0,
                                   nil);

end;

{ TScreenMain.TLoopOrderIndependentTransparencyPass1RenderPass }

constructor TScreenMain.TLoopOrderIndependentTransparencyPass1RenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin
inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='LoopOrderIndependentTransparencyPass1Rendering';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 fResourceCascadedShadowMap:=AddImageInput('resourcetype_cascadedshadowmap_data',
                                           'resource_cascadedshadowmap_data_final',
                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                           []
                                          );

 fResourceSSAO:=AddImageInput('resourcetype_ssao_final',
                              'resource_ssao_data_final',
                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                              []
                             );

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin

  fResourceDepth:=AddImageDepthInput('resourcetype_depth',
                                     'resource_depth_data',
                                     VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,//VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

{ fResourceColor:=AddImageOutput('resourcetype_color',
                                 'resource_orderindependenttransparency_dummy_color',
                                 VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                              TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );
 }
 end else begin

  fResourceDepth:=AddImageDepthInput('resourcetype_msaa_depth',
                                     'resource_msaa_depth_data',
                                     VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,//VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

{ fResourceColor:=AddImageOutput('resourcetype_msaa_color',
                                 'resource_orderindependenttransparency_dummy_msaa_color',
                                 VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                              TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

  fResourceColor:=AddImageResolveOutput('resourcetype_color',
                                        'resource_orderindependenttransparency_dummy_color',
                                        'resource_orderindependenttransparency_dummy_msaa_color',
                                        VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                        TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare,
                                                                     TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                                        [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                       );
 }
 end;

end;

destructor TScreenMain.TLoopOrderIndependentTransparencyPass1RenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass1RenderPass.Show;
var Index:TpvSizeInt;
    Stream:TStream;
begin
 inherited Show;

 fVulkanGraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.GraphicsQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanGraphicsCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_vert.spv');
 try
  fMeshVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
//fFrameGraph.VulkanDevice.DebugMarker.SetObjectName(fMeshVertexShaderModule.Handle,TVkDebugReportObjectTypeEXT.VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT,'fMeshVertexShaderModule');
 finally
  Stream.Free;
 end;

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  if fParent.fZFar<0.0 then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_loopoit_reversedz_pass1_frag.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_loopoit_pass1_frag.spv');
  end;
 end else begin
  if fParent.fZFar<0.0 then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_loopoit_msaa_reversedz_pass1_frag.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_loopoit_msaa_frag.spv');
  end;
 end;
 try
  fMeshFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
//fFrameGraph.VulkanDevice.DebugMarker.SetObjectName(fMeshFragmentShaderModule.Handle,TVkDebugReportObjectTypeEXT.VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT,'fMeshFragmentShaderModule');
 finally
  Stream.Free;
 end;

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  if fParent.fZFar<0.0 then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_loopoit_masked_reversedz_pass1_frag.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_loopoit_masked_pass1_frag.spv');
  end;
 end else begin
  if fParent.fZFar<0.0 then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_loopoit_msaa_masked_reversedz_pass1_frag.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_loopoit_msaa_masked_pass1_frag.spv');
  end;
 end;
 try
  fMeshMaskedFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
//fFrameGraph.VulkanDevice.DebugMarker.SetObjectName(fMeshMaskedFragmentShaderModule.Handle,TVkDebugReportObjectTypeEXT.VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT,'fMeshMaskedFragmentShaderModule');
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageMeshVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fMeshVertexShaderModule,'main');

 fVulkanPipelineShaderStageMeshFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshFragmentShaderModule,'main');

 fVulkanPipelineShaderStageMeshMaskedFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshMaskedFragmentShaderModule,'main');

 fVulkanCascadedShadowMapSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                                          TVkFilter.VK_FILTER_LINEAR,
                                                          TVkFilter.VK_FILTER_LINEAR,
                                                          TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          0.0,
                                                          false,
                                                          0.0,
                                                          false,
                                                          VK_COMPARE_OP_ALWAYS,
                                                          0.0,
                                                          0.0,
                                                          VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE,
                                                          false);

 fVulkanSSAOSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                             TVkFilter.VK_FILTER_LINEAR,
                                             TVkFilter.VK_FILTER_LINEAR,
                                             TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             0.0,
                                             false,
                                             0.0,
                                             false,
                                             VK_COMPARE_OP_ALWAYS,
                                             0.0,
                                             0.0,
                                             VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE,
                                             false);


end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass1RenderPass.Hide;
begin

 FreeAndNil(fVulkanCascadedShadowMapSampler);

 FreeAndNil(fVulkanSSAOSampler);

 FreeAndNil(fVulkanPipelineShaderStageMeshVertex);

 FreeAndNil(fVulkanPipelineShaderStageMeshFragment);

 FreeAndNil(fVulkanPipelineShaderStageMeshMaskedFragment);

 FreeAndNil(fMeshVertexShaderModule);

 FreeAndNil(fMeshFragmentShaderModule);

 FreeAndNil(fMeshMaskedFragmentShaderModule);

 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 FreeAndNil(fVulkanGraphicsCommandBufferFence);
 FreeAndNil(fVulkanGraphicsCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass1RenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
    VulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
begin

 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fGlobalVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fGlobalVulkanDescriptorSetLayout.AddBinding(0,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             3,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(1,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             3,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(2,
                                             VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(3,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(4,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             2,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(5,
                                             VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(6,
                                             VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(7,
                                             VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.Initialize;

 fGlobalVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,9*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,2*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,1*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fGlobalVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fGlobalVulkanDescriptorPool,
                                                                                 fGlobalVulkanDescriptorSetLayout);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                       0,
                                                                       3,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [fParent.fGGXBRDF.DescriptorImageInfo,
                                                                        fParent.fCharlieBRDF.DescriptorImageInfo,
                                                                        fParent.fSheenELUT.DescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                       0,
                                                                       3,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [fParent.fGGXEnvMapCubeMap.DescriptorImageInfo,
                                                                        fParent.fCharlieEnvMapCubeMap.DescriptorImageInfo,
                                                                        fParent.fLambertianEnvMapCubeMap.DescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                       [],
                                                                       [fParent.fCascadedShadowMapVulkanUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [TVkDescriptorImageInfo.Create(fVulkanCascadedShadowMapSampler.Handle,
                                                                                                      fResourceCascadedShadowMap.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceCascadedShadowMap.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                                       0,
                                                                       2,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [TVkDescriptorImageInfo.Create(fVulkanSSAOSampler.Handle,
                                                                                                      fResourceSSAO.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceSSAO.ResourceTransition.Layout), // TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                        fParent.fForwardMipmappedArray2DImages[InFlightFrameIndex].ArrayDescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(5,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                       [],
                                                                       [fParent.fLoopOrderIndependentTransparentUniformVulkanBuffer.DescriptorBufferInfo],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(6,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                       [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                      fResourceDepth.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceDepth.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(7,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER),
                                                                       [],
                                                                       [],
                                                                       [fParent.fLoopOrderIndependentTransparencyZBufferBuffers[InFlightFrameIndex].VulkanBufferView.Handle],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT),0,SizeOf(TpvScene3D.TVertexStagePushConstants));
 fVulkanPipelineLayout.AddDescriptorSetLayout(fParent.fScene3D.GlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin

  if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Opaque then begin
   continue;
  end;

  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin

   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin

    VulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                             pvApplication.VulkanPipelineCache,
                                                             0,
                                                             [],
                                                             fVulkanPipelineLayout,
                                                             fVulkanRenderPass,
                                                             VulkanRenderPassSubpassIndex,
                                                             nil,
                                                             0);

    try

     VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshVertex);
     if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask then begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshMaskedFragment);
     end else begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshFragment);
     end;

     VulkanGraphicsPipeline.InputAssemblyState.Topology:=TVkPrimitiveTopology(PrimitiveTopology);
     VulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

     fParent.fScene3D.InitializeGraphicsPipeline(VulkanGraphicsPipeline);

     VulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
     VulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

     VulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
     if DoubleSided then begin
      VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
     end else begin
      VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
     end;
     VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

     VulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=fParent.fVulkanSampleCountFlagBits;
{    if (AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask) and (VulkanGraphicsPipeline.MultisampleState.RasterizationSamples<>VK_SAMPLE_COUNT_1_BIT) then begin
      VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=true;
      VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=1.0;
      VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
      VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=true;
      VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;
     end else}begin
      VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=false;
      VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=0.0;
      VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
      VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=false;
      VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;
     end;

     VulkanGraphicsPipeline.ColorBlendState.LogicOpEnable:=false;
     VulkanGraphicsPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[0]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[1]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[2]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[3]:=0.0;
{    VulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(true,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                         VK_BLEND_OP_ADD,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                         VK_BLEND_OP_ADD,
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT)); }

     VulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
     VulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=false;
     if fParent.fZFar<0.0 then begin
      VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_GREATER_OR_EQUAL;
     end else begin
      VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
     end;
     VulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
     VulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

     VulkanGraphicsPipeline.Initialize;

     VulkanGraphicsPipeline.FreeMemory;

    finally
     fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]:=VulkanGraphicsPipeline;
    end;

   end;

  end;

 end;

end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass1RenderPass.BeforeDestroySwapChain;
var Index:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
begin
 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;
 FreeAndNil(fVulkanPipelineLayout);
 for Index:=0 to fParent.fCountInFlightFrames-1 do begin
  FreeAndNil(fGlobalVulkanDescriptorSets[Index]);
 end;
 FreeAndNil(fGlobalVulkanDescriptorPool);
 FreeAndNil(fGlobalVulkanDescriptorSetLayout);
 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass1RenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass1RenderPass.OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                                                const aPipelineLayout:TpvVulkanPipelineLayout;
                                                                                                const aRenderPassIndex:TpvSizeInt;
                                                                                                const aPreviousInFlightFrameIndex:TpvSizeInt;
                                                                                                const aInFlightFrameIndex:TpvSizeInt);
begin
 if not fOnSetRenderPassResourcesDone then begin
  fOnSetRenderPassResourcesDone:=true;
  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                       fVulkanPipelineLayout.Handle,
                                       1,
                                       1,
                                       @fGlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil);
 end;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass1RenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                               const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var InFlightFrameState:TScreenMain.PInFlightFrameState;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 InFlightFrameState:=@fParent.fInFlightFrameStates[aInFlightFrameIndex];

 if InFlightFrameState^.Ready then begin

  fOnSetRenderPassResourcesDone:=false;

  if fParent.fUseOITAlphaTest then begin
   fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Mask],
                         -1,
                         aInFlightFrameIndex,
                         0,
                         InFlightFrameState^.FinalViewIndex,
                         InFlightFrameState^.CountViews,
                         aCommandBuffer,
                         fVulkanPipelineLayout,
                         OnSetRenderPassResources,
                         [TpvScene3D.TMaterial.TAlphaMode.Mask]);
  end;

  fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Blend],
                        -1,
                        aInFlightFrameIndex,
                        0,
                        InFlightFrameState^.FinalViewIndex,
                        InFlightFrameState^.CountViews,
                        aCommandBuffer,
                        fVulkanPipelineLayout,
                        OnSetRenderPassResources,
                        [TpvScene3D.TMaterial.TAlphaMode.Blend]);

 end;

end;

{ TScreenMain.TLoopOrderIndependentTransparencyPass1BarrierCustomPass }

constructor TScreenMain.TLoopOrderIndependentTransparencyPass1BarrierCustomPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin
 inherited Create(aFrameGraph);
 fParent:=aParent;
 Name:='LoopOrderIndependentTransparencyPass1BarrierCustomPass';
end;

destructor TScreenMain.TLoopOrderIndependentTransparencyPass1BarrierCustomPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass1BarrierCustomPass.Show;
begin
 inherited Show;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass1BarrierCustomPass.Hide;
begin
 inherited Hide;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass1BarrierCustomPass.AfterCreateSwapChain;
begin
 inherited AfterCreateSwapChain;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass1BarrierCustomPass.BeforeDestroySwapChain;
begin
 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass1BarrierCustomPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass1BarrierCustomPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var BufferMemoryBarrier:TVkBufferMemoryBarrier;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    0,
                                                    0,
                                                    fParent.fLoopOrderIndependentTransparencyZBufferBuffers[aInFlightFrameIndex].VulkanBuffer.Handle,
                                                    0,
                                                    VK_WHOLE_SIZE);

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                   TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT),
                                   0,
                                   nil,
                                   1,
                                   @BufferMemoryBarrier,
                                   0,
                                   nil);

end;

{ TScreenMain.TLoopOrderIndependentTransparencyPass2RenderPass }

constructor TScreenMain.TLoopOrderIndependentTransparencyPass2RenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin
inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='LoopOrderIndependentTransparencyPass2Rendering';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 fResourceCascadedShadowMap:=AddImageInput('resourcetype_cascadedshadowmap_data',
                                           'resource_cascadedshadowmap_data_final',
                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                           []
                                          );

 fResourceSSAO:=AddImageInput('resourcetype_ssao_final',
                              'resource_ssao_data_final',
                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                              []
                             );

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin

  fResourceDepth:=AddImageDepthInput('resourcetype_depth',
                                     'resource_depth_data',
                                     VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,//VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

  fResourceColor:=AddImageOutput('resourcetype_color',
                                 'resource_orderindependenttransparency_tailblending_color',
                                 VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                              TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

 end else begin

  fResourceDepth:=AddImageDepthInput('resourcetype_msaa_depth',
                                     'resource_msaa_depth_data',
                                     VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,//VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

  fResourceColor:=AddImageOutput('resourcetype_msaa_color',
                                 'resource_orderindependenttransparency_tailblending_msaa_color',
                                 VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                              TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

  fResourceColor:=AddImageResolveOutput('resourcetype_color',
                                        'resource_orderindependenttransparency_tailblending_color',
                                        'resource_orderindependenttransparency_tailblending_msaa_color',
                                        VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                        TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare,
                                                                     TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                                        [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                       );

 end;

end;

destructor TScreenMain.TLoopOrderIndependentTransparencyPass2RenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass2RenderPass.Show;
var Index:TpvSizeInt;
    Stream:TStream;
begin
 inherited Show;

 fVulkanGraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.GraphicsQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanGraphicsCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_vert.spv');
 try
  fMeshVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
//fFrameGraph.VulkanDevice.DebugMarker.SetObjectName(fMeshVertexShaderModule.Handle,TVkDebugReportObjectTypeEXT.VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT,'fMeshVertexShaderModule');
 finally
  Stream.Free;
 end;

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  if fParent.fZFar<0.0 then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_loopoit_reversedz_pass2_frag.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_loopoit_pass2_frag.spv');
  end;
 end else begin
  if fParent.fZFar<0.0 then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_loopoit_msaa_reversedz_pass2_frag.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_loopoit_msaa_frag.spv');
  end;
 end;
 try
  fMeshFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
//fFrameGraph.VulkanDevice.DebugMarker.SetObjectName(fMeshFragmentShaderModule.Handle,TVkDebugReportObjectTypeEXT.VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT,'fMeshFragmentShaderModule');
 finally
  Stream.Free;
 end;

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  if fParent.fZFar<0.0 then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_loopoit_masked_reversedz_pass2_frag.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_loopoit_masked_pass2_frag.spv');
  end;
 end else begin
  if fParent.fZFar<0.0 then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_loopoit_msaa_masked_reversedz_pass2_frag.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_loopoit_msaa_masked_pass2_frag.spv');
  end;
 end;
 try
  fMeshMaskedFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
//fFrameGraph.VulkanDevice.DebugMarker.SetObjectName(fMeshMaskedFragmentShaderModule.Handle,TVkDebugReportObjectTypeEXT.VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT,'fMeshMaskedFragmentShaderModule');
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageMeshVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fMeshVertexShaderModule,'main');

 fVulkanPipelineShaderStageMeshFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshFragmentShaderModule,'main');

 fVulkanPipelineShaderStageMeshMaskedFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshMaskedFragmentShaderModule,'main');

 fVulkanCascadedShadowMapSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                                          TVkFilter.VK_FILTER_LINEAR,
                                                          TVkFilter.VK_FILTER_LINEAR,
                                                          TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          0.0,
                                                          false,
                                                          0.0,
                                                          false,
                                                          VK_COMPARE_OP_ALWAYS,
                                                          0.0,
                                                          0.0,
                                                          VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE,
                                                          false);

 fVulkanSSAOSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                             TVkFilter.VK_FILTER_LINEAR,
                                             TVkFilter.VK_FILTER_LINEAR,
                                             TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             0.0,
                                             false,
                                             0.0,
                                             false,
                                             VK_COMPARE_OP_ALWAYS,
                                             0.0,
                                             0.0,
                                             VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE,
                                             false);


end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass2RenderPass.Hide;
begin

 FreeAndNil(fVulkanCascadedShadowMapSampler);

 FreeAndNil(fVulkanSSAOSampler);

 FreeAndNil(fVulkanPipelineShaderStageMeshVertex);

 FreeAndNil(fVulkanPipelineShaderStageMeshFragment);

 FreeAndNil(fVulkanPipelineShaderStageMeshMaskedFragment);

 FreeAndNil(fMeshVertexShaderModule);

 FreeAndNil(fMeshFragmentShaderModule);

 FreeAndNil(fMeshMaskedFragmentShaderModule);

 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 FreeAndNil(fVulkanGraphicsCommandBufferFence);
 FreeAndNil(fVulkanGraphicsCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass2RenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
    VulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
begin

 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fGlobalVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fGlobalVulkanDescriptorSetLayout.AddBinding(0,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             3,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(1,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             3,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(2,
                                             VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(3,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(4,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             2,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(5,
                                             VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(6,
                                             VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(7,
                                             VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(8,
                                             VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 if fParent.fVulkanSampleCountFlagBits<>TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  fGlobalVulkanDescriptorSetLayout.AddBinding(9,
                                              VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,
                                              1,
                                              TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                              []);
 end;
 fGlobalVulkanDescriptorSetLayout.Initialize;

 fGlobalVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,9*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,2*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,3*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fGlobalVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fGlobalVulkanDescriptorPool,
                                                                                 fGlobalVulkanDescriptorSetLayout);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                       0,
                                                                       3,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [fParent.fGGXBRDF.DescriptorImageInfo,
                                                                        fParent.fCharlieBRDF.DescriptorImageInfo,
                                                                        fParent.fSheenELUT.DescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                       0,
                                                                       3,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [fParent.fGGXEnvMapCubeMap.DescriptorImageInfo,
                                                                        fParent.fCharlieEnvMapCubeMap.DescriptorImageInfo,
                                                                        fParent.fLambertianEnvMapCubeMap.DescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                       [],
                                                                       [fParent.fCascadedShadowMapVulkanUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [TVkDescriptorImageInfo.Create(fVulkanCascadedShadowMapSampler.Handle,
                                                                                                      fResourceCascadedShadowMap.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceCascadedShadowMap.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                                       0,
                                                                       2,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [TVkDescriptorImageInfo.Create(fVulkanSSAOSampler.Handle,
                                                                                                      fResourceSSAO.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceSSAO.ResourceTransition.Layout), // TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                        fParent.fForwardMipmappedArray2DImages[InFlightFrameIndex].ArrayDescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(5,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                       [],
                                                                       [fParent.fLoopOrderIndependentTransparentUniformVulkanBuffer.DescriptorBufferInfo],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(6,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                       [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                      fResourceDepth.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceDepth.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(7,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER),
                                                                       [],
                                                                       [],
                                                                       [fParent.fLoopOrderIndependentTransparencyZBufferBuffers[InFlightFrameIndex].VulkanBufferView.Handle],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(8,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER),
                                                                       [],
                                                                       [],
                                                                       [fParent.fLoopOrderIndependentTransparencyABufferBuffers[InFlightFrameIndex].VulkanBufferView.Handle],
                                                                       false);
  if fParent.fVulkanSampleCountFlagBits<>TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
   fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(9,
                                                                        0,
                                                                        1,
                                                                        TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER),
                                                                        [],
                                                                        [],
                                                                        [fParent.fLoopOrderIndependentTransparencySBufferBuffers[InFlightFrameIndex].VulkanBufferView.Handle],
                                                                        false);
  end;
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT),0,SizeOf(TpvScene3D.TVertexStagePushConstants));
 fVulkanPipelineLayout.AddDescriptorSetLayout(fParent.fScene3D.GlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin

  if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Opaque then begin
   continue;
  end;

  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin

   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin

    VulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                             pvApplication.VulkanPipelineCache,
                                                             0,
                                                             [],
                                                             fVulkanPipelineLayout,
                                                             fVulkanRenderPass,
                                                             VulkanRenderPassSubpassIndex,
                                                             nil,
                                                             0);

    try

     VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshVertex);
     if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask then begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshMaskedFragment);
     end else begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshFragment);
     end;

     VulkanGraphicsPipeline.InputAssemblyState.Topology:=TVkPrimitiveTopology(PrimitiveTopology);
     VulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

     fParent.fScene3D.InitializeGraphicsPipeline(VulkanGraphicsPipeline);

     VulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
     VulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

     VulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
     if DoubleSided then begin
      VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
     end else begin
      VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
     end;
     VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

     VulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=fParent.fVulkanSampleCountFlagBits;
     if (AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask) and (VulkanGraphicsPipeline.MultisampleState.RasterizationSamples<>VK_SAMPLE_COUNT_1_BIT) then begin
      VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=true;
      VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=1.0;
      VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
      VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=true;
      VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;
     end else begin
      VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=false;
      VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=0.0;
      VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
      VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=false;
      VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;
     end;

     VulkanGraphicsPipeline.ColorBlendState.LogicOpEnable:=false;
     VulkanGraphicsPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[0]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[1]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[2]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[3]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(true,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                         VK_BLEND_OP_ADD,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                         VK_BLEND_OP_ADD,
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

     VulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
     VulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=false;
     if fParent.fZFar<0.0 then begin
      VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_GREATER_OR_EQUAL;
     end else begin
      VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
     end;
     VulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
     VulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

     VulkanGraphicsPipeline.Initialize;

     VulkanGraphicsPipeline.FreeMemory;

    finally
     fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]:=VulkanGraphicsPipeline;
    end;

   end;

  end;

 end;

end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass2RenderPass.BeforeDestroySwapChain;
var Index:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
begin
 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;
 FreeAndNil(fVulkanPipelineLayout);
 for Index:=0 to fParent.fCountInFlightFrames-1 do begin
  FreeAndNil(fGlobalVulkanDescriptorSets[Index]);
 end;
 FreeAndNil(fGlobalVulkanDescriptorPool);
 FreeAndNil(fGlobalVulkanDescriptorSetLayout);
 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass2RenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass2RenderPass.OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                                                const aPipelineLayout:TpvVulkanPipelineLayout;
                                                                                                const aRenderPassIndex:TpvSizeInt;
                                                                                                const aPreviousInFlightFrameIndex:TpvSizeInt;
                                                                                                const aInFlightFrameIndex:TpvSizeInt);
begin
 if not fOnSetRenderPassResourcesDone then begin
  fOnSetRenderPassResourcesDone:=true;
  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                       fVulkanPipelineLayout.Handle,
                                       1,
                                       1,
                                       @fGlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil);
 end;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass2RenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                               const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var InFlightFrameState:TScreenMain.PInFlightFrameState;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 InFlightFrameState:=@fParent.fInFlightFrameStates[aInFlightFrameIndex];

 if InFlightFrameState^.Ready then begin

  fOnSetRenderPassResourcesDone:=false;

  if fParent.fUseOITAlphaTest then begin
   fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Mask],
                         -1,
                         aInFlightFrameIndex,
                         0,
                         InFlightFrameState^.FinalViewIndex,
                         InFlightFrameState^.CountViews,
                         aCommandBuffer,
                         fVulkanPipelineLayout,
                         OnSetRenderPassResources,
                         [TpvScene3D.TMaterial.TAlphaMode.Mask]);
  end;

  fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Blend],
                        -1,
                        aInFlightFrameIndex,
                        0,
                        InFlightFrameState^.FinalViewIndex,
                        InFlightFrameState^.CountViews,
                        aCommandBuffer,
                        fVulkanPipelineLayout,
                        OnSetRenderPassResources,
                        [TpvScene3D.TMaterial.TAlphaMode.Blend]);

 end;

end;

{ TScreenMain.TLoopOrderIndependentTransparencyPass2BarrierCustomPass }

constructor TScreenMain.TLoopOrderIndependentTransparencyPass2BarrierCustomPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin
 inherited Create(aFrameGraph);
 fParent:=aParent;
 Name:='LoopOrderIndependentTransparencyPass2BarrierCustomPass';
end;

destructor TScreenMain.TLoopOrderIndependentTransparencyPass2BarrierCustomPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass2BarrierCustomPass.Show;
begin
 inherited Show;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass2BarrierCustomPass.Hide;
begin
 inherited Hide;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass2BarrierCustomPass.AfterCreateSwapChain;
begin
 inherited AfterCreateSwapChain;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass2BarrierCustomPass.BeforeDestroySwapChain;
begin
 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass2BarrierCustomPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyPass2BarrierCustomPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var BufferMemoryBarriers:array[0..1] of TVkBufferMemoryBarrier;
    CountBufferMemoryBarriers:TpvInt32;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        0,
                                                        0,
                                                        fParent.fLoopOrderIndependentTransparencyABufferBuffers[aInFlightFrameIndex].VulkanBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 if fParent.fVulkanSampleCountFlagBits<>TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         0,
                                                         0,
                                                         fParent.fLoopOrderIndependentTransparencySBufferBuffers[aInFlightFrameIndex].VulkanBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  CountBufferMemoryBarriers:=2;
 end else begin
  CountBufferMemoryBarriers:=1;
 end;

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                   TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT),
                                   0,
                                   nil,
                                   CountBufferMemoryBarriers,
                                   @BufferMemoryBarriers[0],
                                   0,
                                   nil);

end;

{ TScreenMain.TLoopOrderIndependentTransparencyResolveRenderPass }

constructor TScreenMain.TLoopOrderIndependentTransparencyResolveRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='LoopOrderIndependentTransparencyResolve';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

//SeparatePhysicalPass:=true;

//SeparateCommandBuffer:=true;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 fResourceOpaque:=AddImageInput('resourcetype_color_optimized_non_alpha',
                                'resource_forwardrendering_color',
                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                               );

 fResourceTransparent:=AddImageInput('resourcetype_color',
                                     'resource_orderindependenttransparency_tailblending_color',
                                     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

 fResourceSurface:=AddImageOutput('resourcetype_color_optimized_non_alpha',
                                  'resource_combinedopaquetransparency_final_color',
                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                  TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                               TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );

end;

destructor TScreenMain.TLoopOrderIndependentTransparencyResolveRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyResolveRenderPass.Show;
var Stream:TStream;
begin

 inherited Show;

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/fullscreen_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  if fParent.fZFar<0.0 then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/loopoit_resolve_reversedz_frag.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/loopoit_resolve_frag.spv');
  end;
 end else begin
  if fParent.fZFar<0.0 then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/loopoit_resolve_reversedz_msaa_frag.spv');
  end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/loopoit_resolve_msaa_frag.spv');
  end;
 end;
 try
  fVulkanFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVulkanVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fVulkanFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

end;

procedure TScreenMain.TLoopOrderIndependentTransparencyResolveRenderPass.Hide;
begin
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyResolveRenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin
 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,2*fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,2*fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,1*fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(1,
                                       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(2,
                                       VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(3,
                                       VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 if fParent.fVulkanSampleCountFlagBits<>TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  fVulkanDescriptorSetLayout.AddBinding(4,
                                        VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,
                                        1,
                                        TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                        []);
 end;
 fVulkanDescriptorSetLayout.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                fResourceOpaque.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceOpaque.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                fResourceTransparent.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceTransparent.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                 [],
                                                                 [fParent.fLoopOrderIndependentTransparentUniformVulkanBuffer.DescriptorBufferInfo],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER),
                                                                 [],
                                                                 [],
                                                                 [fParent.fLoopOrderIndependentTransparencyABufferBuffers[InFlightFrameIndex].VulkanBufferView.Handle],
                                                                 false
                                                                );
  if fParent.fVulkanSampleCountFlagBits<>TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
   fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                                  0,
                                                                  1,
                                                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER),
                                                                  [],
                                                                  [],
                                                                  [fParent.fLoopOrderIndependentTransparencySBufferBuffers[InFlightFrameIndex].VulkanBufferView.Handle],
                                                                  false
                                                                 );
  end;
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                           pvApplication.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           fVulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           VulkanRenderPassSubpassIndex,
                                                           nil,
                                                           0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
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
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_DST_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_ONE,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_OP_ADD,
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

end;

procedure TScreenMain.TLoopOrderIndependentTransparencyResolveRenderPass.BeforeDestroySwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 fVulkanRenderPass:=nil;

 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyResolveRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TLoopOrderIndependentTransparencyResolveRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      fVulkanPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,0,nil);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
 aCommandBuffer.CmdDraw(3,1,0,0);
end;

{ TScreenMain.TMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass }

constructor TScreenMain.TMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin
inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='MomentBasedOrderIndependentTransparencyAbsorbance';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 fResourceCascadedShadowMap:=AddImageInput('resourcetype_cascadedshadowmap_data',
                                           'resource_cascadedshadowmap_data_final',
                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                           []
                                          );

 fResourceSSAO:=AddImageInput('resourcetype_ssao_final',
                              'resource_ssao_data_final',
                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                              []
                             );

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin

  fResourceDepth:=AddImageDepthInput('resourcetype_depth',
                                     'resource_depth_data',
                                     VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,//VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

 end else begin

  fResourceDepth:=AddImageDepthInput('resourcetype_msaa_depth',
                                     'resource_msaa_depth_data',
                                     VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,//VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

 end;

 fResourceMoments0:=AddImageOutput('resourcetype_mboit_data',
                                   'resource_momentbasedorderindependenttransparency_moments0',
                                   VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                   TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                 TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                                   [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                  );

 fResourceMoments1:=AddImageOutput('resourcetype_mboit_data',
                                   'resource_momentbasedorderindependenttransparency_moments1',
                                   VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                   TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                 TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                                   [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                  );


end;

destructor TScreenMain.TMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass.Show;
var Index:TpvSizeInt;
    Stream:TStream;
begin
 inherited Show;

 fVulkanGraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.GraphicsQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanGraphicsCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_vert.spv');
 try
  fMeshVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_mboit_pass1_frag.spv');
 try
  fMeshFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_mboit_masked_pass1_frag.spv');
 try
  fMeshMaskedFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageMeshVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fMeshVertexShaderModule,'main');

 fVulkanPipelineShaderStageMeshFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshFragmentShaderModule,'main');

 fVulkanPipelineShaderStageMeshMaskedFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshMaskedFragmentShaderModule,'main');

 fVulkanCascadedShadowMapSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                                          TVkFilter.VK_FILTER_LINEAR,
                                                          TVkFilter.VK_FILTER_LINEAR,
                                                          TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          0.0,
                                                          false,
                                                          0.0,
                                                          false,
                                                          VK_COMPARE_OP_ALWAYS,
                                                          0.0,
                                                          0.0,
                                                          VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE,
                                                          false);

 fVulkanSSAOSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                             TVkFilter.VK_FILTER_LINEAR,
                                             TVkFilter.VK_FILTER_LINEAR,
                                             TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             0.0,
                                             false,
                                             0.0,
                                             false,
                                             VK_COMPARE_OP_ALWAYS,
                                             0.0,
                                             0.0,
                                             VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE,
                                             false);

end;

procedure TScreenMain.TMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass.Hide;
begin

 FreeAndNil(fVulkanCascadedShadowMapSampler);

 FreeAndNil(fVulkanSSAOSampler);

 FreeAndNil(fVulkanPipelineShaderStageMeshVertex);

 FreeAndNil(fVulkanPipelineShaderStageMeshFragment);

 FreeAndNil(fVulkanPipelineShaderStageMeshMaskedFragment);

 FreeAndNil(fMeshVertexShaderModule);

 FreeAndNil(fMeshFragmentShaderModule);

 FreeAndNil(fMeshMaskedFragmentShaderModule);

 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 FreeAndNil(fVulkanGraphicsCommandBufferFence);
 FreeAndNil(fVulkanGraphicsCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
    VulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
begin

 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fGlobalVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fGlobalVulkanDescriptorSetLayout.AddBinding(0,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             3,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(1,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             3,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(2,
                                             VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(3,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(4,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             2,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(5,
                                             VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.Initialize;

 fGlobalVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,9*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,2*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fGlobalVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fGlobalVulkanDescriptorPool,
                                                                                 fGlobalVulkanDescriptorSetLayout);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                       0,
                                                                       3,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [fParent.fGGXBRDF.DescriptorImageInfo,
                                                                        fParent.fCharlieBRDF.DescriptorImageInfo,
                                                                        fParent.fSheenELUT.DescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                       0,
                                                                       3,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [fParent.fGGXEnvMapCubeMap.DescriptorImageInfo,
                                                                        fParent.fCharlieEnvMapCubeMap.DescriptorImageInfo,
                                                                        fParent.fLambertianEnvMapCubeMap.DescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                       [],
                                                                       [fParent.fCascadedShadowMapVulkanUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [TVkDescriptorImageInfo.Create(fVulkanCascadedShadowMapSampler.Handle,
                                                                                                      fResourceCascadedShadowMap.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceCascadedShadowMap.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                                       0,
                                                                       2,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [TVkDescriptorImageInfo.Create(fVulkanSSAOSampler.Handle,
                                                                                                      fResourceSSAO.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceSSAO.ResourceTransition.Layout), // TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                        fParent.fForwardMipmappedArray2DImages[InFlightFrameIndex].ArrayDescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(5,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                       [],
                                                                       [fParent.fApproximationOrderIndependentTransparentUniformVulkanBuffer.DescriptorBufferInfo],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT),0,SizeOf(TpvScene3D.TVertexStagePushConstants));
 fVulkanPipelineLayout.AddDescriptorSetLayout(fParent.fScene3D.GlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin

  if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Opaque then begin
   continue;
  end;

  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin

   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin

    VulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                             pvApplication.VulkanPipelineCache,
                                                             0,
                                                             [],
                                                             fVulkanPipelineLayout,
                                                             fVulkanRenderPass,
                                                             VulkanRenderPassSubpassIndex,
                                                             nil,
                                                             0);

    try

     VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshVertex);
     if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask then begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshMaskedFragment);
     end else begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshFragment);
     end;

     VulkanGraphicsPipeline.InputAssemblyState.Topology:=TVkPrimitiveTopology(PrimitiveTopology);
     VulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

     fParent.fScene3D.InitializeGraphicsPipeline(VulkanGraphicsPipeline);

     VulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
     VulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

     VulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
     if DoubleSided then begin
      VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
     end else begin
      VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
     end;
     VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

     VulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=fParent.fVulkanSampleCountFlagBits;
     if (AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask) and (VulkanGraphicsPipeline.MultisampleState.RasterizationSamples<>VK_SAMPLE_COUNT_1_BIT) then begin
      VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=true;
      VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=1.0;
      VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
      VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=true;
      VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;
     end else begin
      VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=false;
      VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=0.0;
      VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
      VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=false;
      VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;
     end;

     VulkanGraphicsPipeline.ColorBlendState.LogicOpEnable:=false;
     VulkanGraphicsPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[0]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[1]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[2]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[3]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(true,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_OP_ADD,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_OP_ADD,
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));
     VulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(true,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_OP_ADD,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_OP_ADD,
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));
     VulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
     VulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=false;
     if fParent.fZFar<0.0 then begin
      VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_GREATER_OR_EQUAL;
     end else begin
      VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
     end;
     VulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
     VulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

     VulkanGraphicsPipeline.Initialize;

     VulkanGraphicsPipeline.FreeMemory;

    finally
     fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]:=VulkanGraphicsPipeline;
    end;

   end;

  end;

 end;

end;

procedure TScreenMain.TMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass.BeforeDestroySwapChain;
var Index:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
begin
 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;
 FreeAndNil(fVulkanPipelineLayout);
 for Index:=0 to fParent.fCountInFlightFrames-1 do begin
  FreeAndNil(fGlobalVulkanDescriptorSets[Index]);
 end;
 FreeAndNil(fGlobalVulkanDescriptorPool);
 FreeAndNil(fGlobalVulkanDescriptorSetLayout);
 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass.OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                                                            const aPipelineLayout:TpvVulkanPipelineLayout;
                                                                                                            const aRenderPassIndex:TpvSizeInt;
                                                                                                            const aPreviousInFlightFrameIndex:TpvSizeInt;
                                                                                                            const aInFlightFrameIndex:TpvSizeInt);
begin
 if not fOnSetRenderPassResourcesDone then begin
  fOnSetRenderPassResourcesDone:=true;
  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                       fVulkanPipelineLayout.Handle,
                                       1,
                                       1,
                                       @fGlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil);
 end;
end;

procedure TScreenMain.TMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                                           const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var InFlightFrameState:TScreenMain.PInFlightFrameState;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 InFlightFrameState:=@fParent.fInFlightFrameStates[aInFlightFrameIndex];

 if InFlightFrameState^.Ready then begin

  fOnSetRenderPassResourcesDone:=false;

  if fParent.fUseOITAlphaTest then begin
   fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Mask],
                         -1,
                         aInFlightFrameIndex,
                         0,
                         InFlightFrameState^.FinalViewIndex,
                         InFlightFrameState^.CountViews,
                         aCommandBuffer,
                         fVulkanPipelineLayout,
                         OnSetRenderPassResources,
                         [TpvScene3D.TMaterial.TAlphaMode.Mask]);
  end;

  fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Blend],
                        -1,
                        aInFlightFrameIndex,
                        0,
                        InFlightFrameState^.FinalViewIndex,
                        InFlightFrameState^.CountViews,
                        aCommandBuffer,
                        fVulkanPipelineLayout,
                        OnSetRenderPassResources,
                        [TpvScene3D.TMaterial.TAlphaMode.Blend]);

 end;

end;

{ TScreenMain.TMomentBasedOrderIndependentTransparencyTransmittanceRenderPass }

constructor TScreenMain.TMomentBasedOrderIndependentTransparencyTransmittanceRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin
inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='MomentBasedOrderIndependentTransparencyTransmittance';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 fResourceCascadedShadowMap:=AddImageInput('resourcetype_cascadedshadowmap_data',
                                           'resource_cascadedshadowmap_data_final',
                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                           []
                                          );

 fResourceSSAO:=AddImageInput('resourcetype_ssao_final',
                              'resource_ssao_data_final',
                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                              []
                             );

 fResourceMoments0:=AddImageInput('resourcetype_mboit_data',
                                  'resource_momentbasedorderindependenttransparency_moments0',
                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );

 fResourceMoments1:=AddImageInput('resourcetype_mboit_data',
                                  'resource_momentbasedorderindependenttransparency_moments1',
                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin

  fResourceDepth:=AddImageDepthInput('resourcetype_depth',
                                     'resource_depth_data',
                                     VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,//VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

  fResourceColor:=AddImageOutput('resourcetype_color',
                                 'resource_momentbasedorderindependenttransparency_transmittance',
                                 VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                              TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );
 end else begin

  fResourceDepth:=AddImageDepthInput('resourcetype_msaa_depth',
                                     'resource_msaa_depth_data',
                                     VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,//VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

  fResourceColor:=AddImageOutput('resourcetype_msaa_color',
                                 'resource_momentbasedorderindependenttransparency_msaa_transmittance',
                                 VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                              TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

 end;

end;

destructor TScreenMain.TMomentBasedOrderIndependentTransparencyTransmittanceRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TMomentBasedOrderIndependentTransparencyTransmittanceRenderPass.Show;
var Index:TpvSizeInt;
    Stream:TStream;
begin
 inherited Show;

 fVulkanGraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.GraphicsQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanGraphicsCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_vert.spv');
 try
  fMeshVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_mboit_pass2_frag.spv');
 end else begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_mboit_msaa_pass2_frag.spv');
 end;
 try
  fMeshFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_mboit_masked_pass2_frag.spv');
 end else begin
  Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_mboit_masked_msaa_pass2_frag.spv');
 end;
 try
  fMeshMaskedFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageMeshVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fMeshVertexShaderModule,'main');

 fVulkanPipelineShaderStageMeshFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshFragmentShaderModule,'main');

 fVulkanPipelineShaderStageMeshMaskedFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshMaskedFragmentShaderModule,'main');

 fVulkanCascadedShadowMapSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                                          TVkFilter.VK_FILTER_LINEAR,
                                                          TVkFilter.VK_FILTER_LINEAR,
                                                          TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          0.0,
                                                          false,
                                                          0.0,
                                                          false,
                                                          VK_COMPARE_OP_ALWAYS,
                                                          0.0,
                                                          0.0,
                                                          VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE,
                                                          false);

 fVulkanSSAOSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                             TVkFilter.VK_FILTER_LINEAR,
                                             TVkFilter.VK_FILTER_LINEAR,
                                             TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             0.0,
                                             false,
                                             0.0,
                                             false,
                                             VK_COMPARE_OP_ALWAYS,
                                             0.0,
                                             0.0,
                                             VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE,
                                             false);

end;

procedure TScreenMain.TMomentBasedOrderIndependentTransparencyTransmittanceRenderPass.Hide;
begin

 FreeAndNil(fVulkanCascadedShadowMapSampler);

 FreeAndNil(fVulkanSSAOSampler);

 FreeAndNil(fVulkanPipelineShaderStageMeshVertex);

 FreeAndNil(fVulkanPipelineShaderStageMeshFragment);

 FreeAndNil(fVulkanPipelineShaderStageMeshMaskedFragment);

 FreeAndNil(fMeshVertexShaderModule);

 FreeAndNil(fMeshFragmentShaderModule);

 FreeAndNil(fMeshMaskedFragmentShaderModule);

 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 FreeAndNil(fVulkanGraphicsCommandBufferFence);
 FreeAndNil(fVulkanGraphicsCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TMomentBasedOrderIndependentTransparencyTransmittanceRenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
    VulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
begin

 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fGlobalVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fGlobalVulkanDescriptorSetLayout.AddBinding(0,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             3,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(1,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             3,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(2,
                                             VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(3,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(4,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             2,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(5,
                                             VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(6,
                                             VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(7,
                                             VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.Initialize;

 fGlobalVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,9*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,2*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,2*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fGlobalVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fGlobalVulkanDescriptorPool,
                                                                                  fGlobalVulkanDescriptorSetLayout);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                       0,
                                                                       3,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [fParent.fGGXBRDF.DescriptorImageInfo,
                                                                        fParent.fCharlieBRDF.DescriptorImageInfo,
                                                                        fParent.fSheenELUT.DescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                       0,
                                                                       3,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [fParent.fGGXEnvMapCubeMap.DescriptorImageInfo,
                                                                        fParent.fCharlieEnvMapCubeMap.DescriptorImageInfo,
                                                                        fParent.fLambertianEnvMapCubeMap.DescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                       [],
                                                                       [fParent.fCascadedShadowMapVulkanUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [TVkDescriptorImageInfo.Create(fVulkanCascadedShadowMapSampler.Handle,
                                                                                                      fResourceCascadedShadowMap.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceCascadedShadowMap.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                                       0,
                                                                       2,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [TVkDescriptorImageInfo.Create(fVulkanSSAOSampler.Handle,
                                                                                                      fResourceSSAO.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceSSAO.ResourceTransition.Layout), // TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                        fParent.fForwardMipmappedArray2DImages[InFlightFrameIndex].ArrayDescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(5,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                       [],
                                                                       [fParent.fApproximationOrderIndependentTransparentUniformVulkanBuffer.DescriptorBufferInfo],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(6,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                       [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                      fResourceMoments0.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceMoments0.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(7,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                       [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                      fResourceMoments1.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceMoments1.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT),0,SizeOf(TpvScene3D.TVertexStagePushConstants));
 fVulkanPipelineLayout.AddDescriptorSetLayout(fParent.fScene3D.GlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin

  if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Opaque then begin
   continue;
  end;

  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin

   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin

    VulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                             pvApplication.VulkanPipelineCache,
                                                             0,
                                                             [],
                                                             fVulkanPipelineLayout,
                                                             fVulkanRenderPass,
                                                             VulkanRenderPassSubpassIndex,
                                                             nil,
                                                             0);

    try

     VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshVertex);
     if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask then begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshMaskedFragment);
     end else begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshFragment);
     end;

     VulkanGraphicsPipeline.InputAssemblyState.Topology:=TVkPrimitiveTopology(PrimitiveTopology);
     VulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

     fParent.fScene3D.InitializeGraphicsPipeline(VulkanGraphicsPipeline);

     VulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
     VulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

     VulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
     if DoubleSided then begin
      VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
     end else begin
      VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
     end;
     VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

     VulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=fParent.fVulkanSampleCountFlagBits;
     if (AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask) and (VulkanGraphicsPipeline.MultisampleState.RasterizationSamples<>VK_SAMPLE_COUNT_1_BIT) then begin
      VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=true;
      VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=1.0;
      VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
      VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=true;
      VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;
     end else begin
      VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=false;
      VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=0.0;
      VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
      VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=false;
      VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;
     end;

     VulkanGraphicsPipeline.ColorBlendState.LogicOpEnable:=false;
     VulkanGraphicsPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[0]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[1]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[2]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[3]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(true,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_OP_ADD,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_OP_ADD,
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));
     VulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
     VulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=false;
     if fParent.fZFar<0.0 then begin
      VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_GREATER_OR_EQUAL;
     end else begin
      VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
     end;
     VulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
     VulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

     VulkanGraphicsPipeline.Initialize;

     VulkanGraphicsPipeline.FreeMemory;

    finally
     fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]:=VulkanGraphicsPipeline;
    end;

   end;

  end;

 end;

end;

procedure TScreenMain.TMomentBasedOrderIndependentTransparencyTransmittanceRenderPass.BeforeDestroySwapChain;
var Index:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
begin
 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;
 FreeAndNil(fVulkanPipelineLayout);
 for Index:=0 to fParent.fCountInFlightFrames-1 do begin
  FreeAndNil(fGlobalVulkanDescriptorSets[Index]);
 end;
 FreeAndNil(fGlobalVulkanDescriptorPool);
 FreeAndNil(fGlobalVulkanDescriptorSetLayout);
 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TMomentBasedOrderIndependentTransparencyTransmittanceRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TMomentBasedOrderIndependentTransparencyTransmittanceRenderPass.OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                                                               const aPipelineLayout:TpvVulkanPipelineLayout;
                                                                                                               const aRenderPassIndex:TpvSizeInt;
                                                                                                               const aPreviousInFlightFrameIndex:TpvSizeInt;
                                                                                                               const aInFlightFrameIndex:TpvSizeInt);
begin
 if not fOnSetRenderPassResourcesDone then begin
  fOnSetRenderPassResourcesDone:=true;
  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                       fVulkanPipelineLayout.Handle,
                                       1,
                                       1,
                                       @fGlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil);
 end;
end;

procedure TScreenMain.TMomentBasedOrderIndependentTransparencyTransmittanceRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                                              const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var InFlightFrameState:TScreenMain.PInFlightFrameState;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 InFlightFrameState:=@fParent.fInFlightFrameStates[aInFlightFrameIndex];

 if InFlightFrameState^.Ready then begin

  fOnSetRenderPassResourcesDone:=false;

  if fParent.fUseOITAlphaTest then begin
   fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Mask],
                         -1,
                         aInFlightFrameIndex,
                         0,
                         InFlightFrameState^.FinalViewIndex,
                         InFlightFrameState^.CountViews,
                         aCommandBuffer,
                         fVulkanPipelineLayout,
                         OnSetRenderPassResources,
                         [TpvScene3D.TMaterial.TAlphaMode.Mask]);
  end;

  fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Blend],
                        -1,
                        aInFlightFrameIndex,
                        0,
                        InFlightFrameState^.FinalViewIndex,
                        InFlightFrameState^.CountViews,
                        aCommandBuffer,
                        fVulkanPipelineLayout,
                        OnSetRenderPassResources,
                        [TpvScene3D.TMaterial.TAlphaMode.Blend]);

 end;

end;

{ TScreenMain.TMomentBasedOrderIndependentTransparencyResolveRenderPass }

constructor TScreenMain.TMomentBasedOrderIndependentTransparencyResolveRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='MomentBasedOrderIndependentTransparencyResolve';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

//SeparatePhysicalPass:=true;

//SeparateCommandBuffer:=true;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin

  fResourceOpaque:=AddImageInput('resourcetype_color_optimized_non_alpha',
                                 'resource_forwardrendering_color',
                                 VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

  fResourceTransparent:=AddImageInput('resourcetype_color',
                                      'resource_momentbasedorderindependenttransparency_transmittance',
                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                      [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                     );

 end else begin

  fResourceOpaque:=AddImageInput('resourcetype_msaa_color',
                                 'resource_forwardrendering_msaa_color',
                                 VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

  fResourceTransparent:=AddImageInput('resourcetype_msaa_color',
                                      'resource_momentbasedorderindependenttransparency_msaa_transmittance',
                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                      [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                     );

 end;

 fResourceMoments0:=AddImageInput('resourcetype_mboit_data',
                                  'resource_momentbasedorderindependenttransparency_moments0',
                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin

  fResourceSurface:=AddImageOutput('resourcetype_color_optimized_non_alpha',
                                   'resource_combinedopaquetransparency_final_color',
                                   VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                   TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                   [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                  );

 end else begin

  fResourceSurface:=AddImageOutput('resourcetype_msaa_color_optimized_non_alpha',
                                   'resource_combinedopaquetransparency_final_msaa_color',
                                   VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                   TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                   [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                  );

  fResourceSurface:=AddImageResolveOutput('resourcetype_color_optimized_non_alpha',
                                          'resource_combinedopaquetransparency_final_color',
                                          'resource_combinedopaquetransparency_final_msaa_color',
                                          VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                          TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare,
                                                                       TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                          [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                         );

 end;

end;

destructor TScreenMain.TMomentBasedOrderIndependentTransparencyResolveRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TMomentBasedOrderIndependentTransparencyResolveRenderPass.Show;
var Stream:TStream;
begin

 inherited Show;

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/fullscreen_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mboit_resolve_frag.spv');
 end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/mboit_resolve_msaa_frag.spv');
 end;
 try
  fVulkanFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVulkanVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fVulkanFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

end;

procedure TScreenMain.TMomentBasedOrderIndependentTransparencyResolveRenderPass.Hide;
begin
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TMomentBasedOrderIndependentTransparencyResolveRenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin
 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,3*fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(1,
                                       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(2,
                                       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                fResourceOpaque.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceOpaque.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                fResourceTransparent.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceTransparent.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                  0,
                                                                  1,
                                                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                  [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                 fResourceMoments0.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                 fResourceMoments0.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                  [],
                                                                  [],
                                                                  false
                                                                 );
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                           pvApplication.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           fVulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           VulkanRenderPassSubpassIndex,
                                                           nil,
                                                           0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
 fVulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_CLOCKWISE;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
 fVulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

 fVulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=fParent.fVulkanSampleCountFlagBits;
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
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_DST_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_ONE,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_OP_ADD,
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

end;

procedure TScreenMain.TMomentBasedOrderIndependentTransparencyResolveRenderPass.BeforeDestroySwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 fVulkanRenderPass:=nil;

 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TMomentBasedOrderIndependentTransparencyResolveRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TMomentBasedOrderIndependentTransparencyResolveRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      fVulkanPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,0,nil);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
 aCommandBuffer.CmdDraw(3,1,0,0);
end;

{ TScreenMain.TWeightBlendedOrderIndependentTransparencyRenderPass }

constructor TScreenMain.TWeightBlendedOrderIndependentTransparencyRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin
inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='WeightBlendedOrderIndependentTransparency';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 fResourceCascadedShadowMap:=AddImageInput('resourcetype_cascadedshadowmap_data',
                                           'resource_cascadedshadowmap_data_final',
                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                           []
                                          );

 fResourceSSAO:=AddImageInput('resourcetype_ssao_final',
                              'resource_ssao_data_final',
                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                              []
                             );

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin

  fResourceDepth:=AddImageDepthInput('resourcetype_depth',
                                     'resource_depth_data',
                                     VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,//VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

 end else begin

  fResourceDepth:=AddImageDepthInput('resourcetype_msaa_depth',
                                     'resource_msaa_depth_data',
                                     VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,//VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

 end;

 fResourceAccumulation:=AddImageOutput('resourcetype_wboit_accumulation',
                                       'resource_weightblendedorderindependenttransparency_accumulation',
                                       VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                       TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                     TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                                       [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                      );

 fResourceRevealage:=AddImageOutput('resourcetype_wboit_revealage',
                                    'resource_weightblendedorderindependenttransparency_revealage',
                                    VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                    TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                 TpvVector4.InlineableCreate(1.0,1.0,1.0,1.0)),
                                    [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                   );


end;

destructor TScreenMain.TWeightBlendedOrderIndependentTransparencyRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TWeightBlendedOrderIndependentTransparencyRenderPass.Show;
var Index:TpvSizeInt;
    Stream:TStream;
begin
 inherited Show;

 fVulkanGraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.GraphicsQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanGraphicsCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_vert.spv');
 try
  fMeshVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_wboit_frag.spv');
 try
  fMeshFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/mesh_'+fParent.fMeshFragTypeName+'_wboit_masked_frag.spv');
 try
  fMeshMaskedFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageMeshVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fMeshVertexShaderModule,'main');

 fVulkanPipelineShaderStageMeshFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshFragmentShaderModule,'main');

 fVulkanPipelineShaderStageMeshMaskedFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fMeshMaskedFragmentShaderModule,'main');

 fVulkanCascadedShadowMapSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                                          TVkFilter.VK_FILTER_LINEAR,
                                                          TVkFilter.VK_FILTER_LINEAR,
                                                          TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                                          0.0,
                                                          false,
                                                          0.0,
                                                          false,
                                                          VK_COMPARE_OP_ALWAYS,
                                                          0.0,
                                                          0.0,
                                                          VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE,
                                                          false);

 fVulkanSSAOSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                             TVkFilter.VK_FILTER_LINEAR,
                                             TVkFilter.VK_FILTER_LINEAR,
                                             TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
                                             0.0,
                                             false,
                                             0.0,
                                             false,
                                             VK_COMPARE_OP_ALWAYS,
                                             0.0,
                                             0.0,
                                             VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE,
                                             false);
end;

procedure TScreenMain.TWeightBlendedOrderIndependentTransparencyRenderPass.Hide;
begin

 FreeAndNil(fVulkanCascadedShadowMapSampler);

 FreeAndNil(fVulkanSSAOSampler);

 FreeAndNil(fVulkanPipelineShaderStageMeshVertex);

 FreeAndNil(fVulkanPipelineShaderStageMeshFragment);

 FreeAndNil(fVulkanPipelineShaderStageMeshMaskedFragment);

 FreeAndNil(fMeshVertexShaderModule);

 FreeAndNil(fMeshFragmentShaderModule);

 FreeAndNil(fMeshMaskedFragmentShaderModule);

 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 FreeAndNil(fVulkanGraphicsCommandBufferFence);
 FreeAndNil(fVulkanGraphicsCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TWeightBlendedOrderIndependentTransparencyRenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
    VulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
begin

 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fGlobalVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fGlobalVulkanDescriptorSetLayout.AddBinding(0,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             3,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(1,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             3,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(2,
                                             VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(3,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(4,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             2,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(5,
                                             VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.Initialize;

 fGlobalVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,9*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,2*fParent.fCountInFlightFrames);
 fGlobalVulkanDescriptorPool.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fGlobalVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fGlobalVulkanDescriptorPool,
                                                                                 fGlobalVulkanDescriptorSetLayout);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                       0,
                                                                       3,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [fParent.fGGXBRDF.DescriptorImageInfo,
                                                                        fParent.fCharlieBRDF.DescriptorImageInfo,
                                                                        fParent.fSheenELUT.DescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                       0,
                                                                       3,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [fParent.fGGXEnvMapCubeMap.DescriptorImageInfo,
                                                                        fParent.fCharlieEnvMapCubeMap.DescriptorImageInfo,
                                                                        fParent.fLambertianEnvMapCubeMap.DescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                       [],
                                                                       [fParent.fCascadedShadowMapVulkanUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [TVkDescriptorImageInfo.Create(fVulkanCascadedShadowMapSampler.Handle,
                                                                                                      fResourceCascadedShadowMap.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceCascadedShadowMap.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                                       0,
                                                                       2,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [TVkDescriptorImageInfo.Create(fVulkanSSAOSampler.Handle,
                                                                                                      fResourceSSAO.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                      fResourceSSAO.ResourceTransition.Layout), // TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                        fParent.fForwardMipmappedArray2DImages[InFlightFrameIndex].ArrayDescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(5,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                       [],
                                                                       [fParent.fApproximationOrderIndependentTransparentUniformVulkanBuffer.DescriptorBufferInfo],
                                                                       [],
                                                                       false);
  fGlobalVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT),0,SizeOf(TpvScene3D.TVertexStagePushConstants));
 fVulkanPipelineLayout.AddDescriptorSetLayout(fParent.fScene3D.GlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;

 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin

  if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Opaque then begin
   continue;
  end;

  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin

   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin

    VulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                             pvApplication.VulkanPipelineCache,
                                                             0,
                                                             [],
                                                             fVulkanPipelineLayout,
                                                             fVulkanRenderPass,
                                                             VulkanRenderPassSubpassIndex,
                                                             nil,
                                                             0);

    try

     VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshVertex);
     if AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask then begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshMaskedFragment);
     end else begin
      VulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageMeshFragment);
     end;

     VulkanGraphicsPipeline.InputAssemblyState.Topology:=TVkPrimitiveTopology(PrimitiveTopology);
     VulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

     fParent.fScene3D.InitializeGraphicsPipeline(VulkanGraphicsPipeline);

     VulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
     VulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

     VulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
     if DoubleSided then begin
      VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
     end else begin
      VulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
     end;
     VulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
     VulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
     VulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

     VulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=fParent.fVulkanSampleCountFlagBits;
     if (AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Mask) and (VulkanGraphicsPipeline.MultisampleState.RasterizationSamples<>VK_SAMPLE_COUNT_1_BIT) then begin
      VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=true;
      VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=1.0;
      VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
      VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=true;
      VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;
     end else begin
      VulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=false;
      VulkanGraphicsPipeline.MultisampleState.MinSampleShading:=0.0;
      VulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
      VulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=false;
      VulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;
     end;

     VulkanGraphicsPipeline.ColorBlendState.LogicOpEnable:=false;
     VulkanGraphicsPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[0]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[1]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[2]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.BlendConstants[3]:=0.0;
     VulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(true,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_OP_ADD,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_FACTOR_ONE,
                                                                         VK_BLEND_OP_ADD,
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));
     VulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(true,
                                                                         VK_BLEND_FACTOR_ZERO,
                                                                         VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                         VK_BLEND_OP_ADD,
                                                                         VK_BLEND_FACTOR_ZERO,
                                                                         VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                         VK_BLEND_OP_ADD,
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));
     VulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
     VulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=false;
     if fParent.fZFar<0.0 then begin
      VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_GREATER_OR_EQUAL;
     end else begin
      VulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
     end;
     VulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
     VulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

     VulkanGraphicsPipeline.Initialize;

     VulkanGraphicsPipeline.FreeMemory;

    finally
     fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]:=VulkanGraphicsPipeline;
    end;

   end;

  end;

 end;

end;

procedure TScreenMain.TWeightBlendedOrderIndependentTransparencyRenderPass.BeforeDestroySwapChain;
var Index:TpvSizeInt;
    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided:TpvScene3D.TDoubleSided;
begin
 for AlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to High(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
   for DoubleSided:=Low(TpvScene3D.TDoubleSided) to High(TpvScene3D.TDoubleSided) do begin
    FreeAndNil(fVulkanGraphicsPipelines[AlphaMode,PrimitiveTopology,DoubleSided]);
   end;
  end;
 end;
 FreeAndNil(fVulkanPipelineLayout);
 for Index:=0 to fParent.fCountInFlightFrames-1 do begin
  FreeAndNil(fGlobalVulkanDescriptorSets[Index]);
 end;
 FreeAndNil(fGlobalVulkanDescriptorPool);
 FreeAndNil(fGlobalVulkanDescriptorSetLayout);
 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TWeightBlendedOrderIndependentTransparencyRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TWeightBlendedOrderIndependentTransparencyRenderPass.OnSetRenderPassResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                                                    const aPipelineLayout:TpvVulkanPipelineLayout;
                                                                                                    const aRenderPassIndex:TpvSizeInt;
                                                                                                    const aPreviousInFlightFrameIndex:TpvSizeInt;
                                                                                                    const aInFlightFrameIndex:TpvSizeInt);
begin
 if not fOnSetRenderPassResourcesDone then begin
  fOnSetRenderPassResourcesDone:=true;
  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                       fVulkanPipelineLayout.Handle,
                                       1,
                                       1,
                                       @fGlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil);
 end;
end;

procedure TScreenMain.TWeightBlendedOrderIndependentTransparencyRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                                                   const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var InFlightFrameState:TScreenMain.PInFlightFrameState;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 InFlightFrameState:=@fParent.fInFlightFrameStates[aInFlightFrameIndex];

 if InFlightFrameState^.Ready then begin

  fOnSetRenderPassResourcesDone:=false;

  if fParent.fUseOITAlphaTest then begin
   fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Mask],
                         -1,
                         aInFlightFrameIndex,
                         0,
                         InFlightFrameState^.FinalViewIndex,
                         InFlightFrameState^.CountViews,
                         aCommandBuffer,
                         fVulkanPipelineLayout,
                         OnSetRenderPassResources,
                         [TpvScene3D.TMaterial.TAlphaMode.Mask]);
  end;

  fParent.fScene3D.Draw(fVulkanGraphicsPipelines[TpvScene3D.TMaterial.TAlphaMode.Blend],
                        -1,
                        aInFlightFrameIndex,
                        0,
                        InFlightFrameState^.FinalViewIndex,
                        InFlightFrameState^.CountViews,
                        aCommandBuffer,
                        fVulkanPipelineLayout,
                        OnSetRenderPassResources,
                        [TpvScene3D.TMaterial.TAlphaMode.Blend]);

 end;

end;

{ TScreenMain.TWeightBlendedOrderIndependentTransparencyResolveRenderPass }

constructor TScreenMain.TWeightBlendedOrderIndependentTransparencyResolveRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='WeightBlendedOrderIndependentTransparencyResolve';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

//SeparatePhysicalPass:=true;

//SeparateCommandBuffer:=true;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin

  fResourceOpaque:=AddImageInput('resourcetype_color_optimized_non_alpha',
                                 'resource_forwardrendering_color',
                                 VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

 end else begin

  fResourceOpaque:=AddImageInput('resourcetype_msaa_color',
                                 'resource_forwardrendering_msaa_color',
                                 VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                 [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                );

 end;

 fResourceAccumlation:=AddImageInput('resourcetype_wboit_accumulation',
                                     'resource_weightblendedorderindependenttransparency_accumulation',
                                     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

 fResourceRevealage:=AddImageInput('resourcetype_wboit_revealage',
                                   'resource_weightblendedorderindependenttransparency_revealage',
                                   VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                   [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                  );

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin

  fResourceSurface:=AddImageOutput('resourcetype_color_optimized_non_alpha',
                                   'resource_combinedopaquetransparency_final_color',
                                   VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                   TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                   [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                  );

 end else begin

  fResourceSurface:=AddImageOutput('resourcetype_msaa_color_optimized_non_alpha',
                                   'resource_combinedopaquetransparency_final_msaa_color',
                                   VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                   TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                   [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                  );

  fResourceSurface:=AddImageResolveOutput('resourcetype_color_optimized_non_alpha',
                                          'resource_combinedopaquetransparency_final_color',
                                          'resource_combinedopaquetransparency_final_msaa_color',
                                          VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                          TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare,
                                                                       TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                          [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                         );

 end;

end;

destructor TScreenMain.TWeightBlendedOrderIndependentTransparencyResolveRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TWeightBlendedOrderIndependentTransparencyResolveRenderPass.Show;
var Stream:TStream;
begin

 inherited Show;

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/fullscreen_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 if fParent.fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/wboit_resolve_frag.spv');
 end else begin
   Stream:=pvApplication.Assets.GetAssetStream('shaders/wboit_resolve_msaa_frag.spv');
 end;
 try
  fVulkanFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVulkanVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fVulkanFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

end;

procedure TScreenMain.TWeightBlendedOrderIndependentTransparencyResolveRenderPass.Hide;
begin
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TWeightBlendedOrderIndependentTransparencyResolveRenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin
 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,3*fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(1,
                                       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(2,
                                       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                fResourceOpaque.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceOpaque.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                fResourceAccumlation.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceAccumlation.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                fResourceRevealage.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceRevealage.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                           pvApplication.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           fVulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           VulkanRenderPassSubpassIndex,
                                                           nil,
                                                           0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
 fVulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_CLOCKWISE;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
 fVulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

 fVulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=fParent.fVulkanSampleCountFlagBits;
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
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_DST_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_ONE,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_OP_ADD,
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

end;

procedure TScreenMain.TWeightBlendedOrderIndependentTransparencyResolveRenderPass.BeforeDestroySwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 fVulkanRenderPass:=nil;

 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TWeightBlendedOrderIndependentTransparencyResolveRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TWeightBlendedOrderIndependentTransparencyResolveRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      fVulkanPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,0,nil);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
 aCommandBuffer.CmdDraw(3,1,0,0);
end;

{ TScreenMain.TTonemappingRenderPass }

constructor TScreenMain.TTonemappingRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='Tonemapping';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

//SeparatePhysicalPass:=true;

//SeparateCommandBuffer:=true;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 if fParent.fTransparencyMode in [TTransparencyMode.DIRECT,
                                  TTransparencyMode.SPINLOCKOIT,
                                  TTransparencyMode.INTERLOCKOIT,
                                  TTransparencyMode.LOOPOIT,
                                  TTransparencyMode.WBOIT,
                                  TTransparencyMode.MBOIT] then begin
  fResourceColor:=AddImageInput('resourcetype_color_optimized_non_alpha',
                                'resource_combinedopaquetransparency_final_color',
                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                               );
 end else begin
  fResourceColor:=AddImageInput('resourcetype_color_optimized_non_alpha',
                                'resource_forwardrendering_color',
                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                               );
 end;

 fResourceSurface:=AddImageOutput('resourcetype_color_tonemapping',
                                  'resource_tonemapping_color',
                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                  TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                               TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );

end;

destructor TScreenMain.TTonemappingRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TTonemappingRenderPass.Show;
var Stream:TStream;
begin

 inherited Show;

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/fullscreen_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/tonemapping_frag.spv');
 try
  fVulkanFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVulkanVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fVulkanFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

end;

procedure TScreenMain.TTonemappingRenderPass.Hide;
begin
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TTonemappingRenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin
 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                fResourceColor.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceColor.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                           pvApplication.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           fVulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           VulkanRenderPassSubpassIndex,
                                                           nil,
                                                           0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
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
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_DST_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_ONE,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_OP_ADD,
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

end;

procedure TScreenMain.TTonemappingRenderPass.BeforeDestroySwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 fVulkanRenderPass:=nil;

 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TTonemappingRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TTonemappingRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      fVulkanPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,0,nil);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
 aCommandBuffer.CmdDraw(3,1,0,0);
end;

{ TScreenMain.TAntialiasingNoneRenderPass }

constructor TScreenMain.TAntialiasingNoneRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='AntialiasingNone';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

//SeparatePhysicalPass:=true;

//SeparateCommandBuffer:=true;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 fResourceColor:=AddImageInput('resourcetype_color_tonemapping',
                               'resource_tonemapping_color',
                               VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                               [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                              );

 fResourceSurface:=AddImageOutput('resourcetype_color_antialiasing',
                                  'resource_antialiasing_color',
                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                  TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                               TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );

end;

destructor TScreenMain.TAntialiasingNoneRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TAntialiasingNoneRenderPass.Show;
var Stream:TStream;
begin

 inherited Show;

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/fullscreen_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/antialiasing_none_frag.spv');
 try
  fVulkanFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVulkanVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fVulkanFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

 fVulkanSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         0.0,
                                         false,
                                         0.0,
                                         false,
                                         VK_COMPARE_OP_ALWAYS,
                                         0.0,
                                         0.0,
                                         VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK,
                                         false);

end;

procedure TScreenMain.TAntialiasingNoneRenderPass.Hide;
begin
 FreeAndNil(fVulkanSampler);
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TAntialiasingNoneRenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin
 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(pvApplication.VulkanDevice,
                                                                   fResourceColor.VulkanImages[InFlightFrameIndex],
                                                                   VK_IMAGE_VIEW_TYPE_2D_ARRAY,
                                                                   TpvFrameGraph.TImageResourceType(fResourceColor.ResourceType).Format,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                   0,
                                                                   1,
                                                                   0,
                                                                   fParent.fCountSurfaceViews
                                                                  );
  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                fResourceColor.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceColor.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                           pvApplication.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           fVulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           VulkanRenderPassSubpassIndex,
                                                           nil,
                                                           0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
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
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_DST_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_ONE,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_OP_ADD,
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

end;

procedure TScreenMain.TAntialiasingNoneRenderPass.BeforeDestroySwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fVulkanImageViews[InFlightFrameIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 fVulkanRenderPass:=nil;

 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TAntialiasingNoneRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TAntialiasingNoneRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      fVulkanPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,0,nil);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
 aCommandBuffer.CmdDraw(3,1,0,0);
end;

{ TScreenMain.TAntialiasingDSAARenderPass }

constructor TScreenMain.TAntialiasingDSAARenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='AntialiasingDSAA';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

//SeparatePhysicalPass:=true;

//SeparateCommandBuffer:=true;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 fResourceColor:=AddImageInput('resourcetype_color_tonemapping',
                               'resource_tonemapping_color',
                               VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                               []
                              );

 fResourceSurface:=AddImageOutput('resourcetype_color_antialiasing',
                                  'resource_antialiasing_color',
                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                  TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                               TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );

end;

destructor TScreenMain.TAntialiasingDSAARenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TAntialiasingDSAARenderPass.Show;
var Stream:TStream;
begin

 inherited Show;

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/fullscreen_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/antialiasing_dsaa_frag.spv');
 try
  fVulkanFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVulkanVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fVulkanFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

 fVulkanSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         0.0,
                                         false,
                                         0.0,
                                         false,
                                         VK_COMPARE_OP_ALWAYS,
                                         0.0,
                                         0.0,
                                         VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK,
                                         false);

end;

procedure TScreenMain.TAntialiasingDSAARenderPass.Hide;
begin
 FreeAndNil(fVulkanSampler);
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TAntialiasingDSAARenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin
 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(pvApplication.VulkanDevice,
                                                                   fResourceColor.VulkanImages[InFlightFrameIndex],
                                                                   VK_IMAGE_VIEW_TYPE_2D_ARRAY,
                                                                   TpvFrameGraph.TImageResourceType(fResourceColor.ResourceType).Format,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                   0,
                                                                   1,
                                                                   0,
                                                                   fParent.fCountSurfaceViews
                                                                  );
  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                                fResourceColor.VulkanAdditionalFormatImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceColor.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                           pvApplication.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           fVulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           VulkanRenderPassSubpassIndex,
                                                           nil,
                                                           0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
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
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_DST_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_ONE,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_OP_ADD,
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

end;

procedure TScreenMain.TAntialiasingDSAARenderPass.BeforeDestroySwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fVulkanImageViews[InFlightFrameIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 fVulkanRenderPass:=nil;

 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TAntialiasingDSAARenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TAntialiasingDSAARenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      fVulkanPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,0,nil);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
 aCommandBuffer.CmdDraw(3,1,0,0);
end;

{ TScreenMain.TAntialiasingFXAARenderPass }

constructor TScreenMain.TAntialiasingFXAARenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='AntialiasingFXAA';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

//SeparatePhysicalPass:=true;

//SeparateCommandBuffer:=true;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 fResourceColor:=AddImageInput('resourcetype_color_tonemapping',
                               'resource_tonemapping_color',
                               VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                               []
                              );

 fResourceSurface:=AddImageOutput('resourcetype_color_antialiasing',
                                  'resource_antialiasing_color',
                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                  TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                               TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );

end;

destructor TScreenMain.TAntialiasingFXAARenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TAntialiasingFXAARenderPass.Show;
var Stream:TStream;
begin

 inherited Show;

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/fullscreen_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/antialiasing_fxaa_frag.spv');
 try
  fVulkanFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVulkanVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fVulkanFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

 fVulkanSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         0.0,
                                         false,
                                         0.0,
                                         false,
                                         VK_COMPARE_OP_ALWAYS,
                                         0.0,
                                         0.0,
                                         VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK,
                                         false);

end;

procedure TScreenMain.TAntialiasingFXAARenderPass.Hide;
begin
 FreeAndNil(fVulkanSampler);
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TAntialiasingFXAARenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin
 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(pvApplication.VulkanDevice,
                                                                   fResourceColor.VulkanImages[InFlightFrameIndex],
                                                                   VK_IMAGE_VIEW_TYPE_2D_ARRAY,
                                                                   TpvFrameGraph.TImageResourceType(fResourceColor.ResourceType).Format,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                   0,
                                                                   1,
                                                                   0,
                                                                   fParent.fCountSurfaceViews
                                                                  );
  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                                fResourceColor.VulkanAdditionalFormatImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceColor.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                           pvApplication.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           fVulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           VulkanRenderPassSubpassIndex,
                                                           nil,
                                                           0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
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
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_DST_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_ONE,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_OP_ADD,
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

end;

procedure TScreenMain.TAntialiasingFXAARenderPass.BeforeDestroySwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fVulkanImageViews[InFlightFrameIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 fVulkanRenderPass:=nil;

 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TAntialiasingFXAARenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TAntialiasingFXAARenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      fVulkanPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,0,nil);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
 aCommandBuffer.CmdDraw(3,1,0,0);
end;

{ TScreenMain.TAntialiasingSMAAEdgesRenderPass }

constructor TScreenMain.TAntialiasingSMAAEdgesRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='AntialiasingSMAAEdges';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

//SeparatePhysicalPass:=true;

//SeparateCommandBuffer:=true;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 fResourceColor:=AddImageInput('resourcetype_color_tonemapping',
                               'resource_tonemapping_color',
                               VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                               []
                              );

 fResourceSurface:=AddImageOutput('resourcetype_smaa_edges',
                                  'resource_smaa_edges',
                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                  TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                               TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );

end;

destructor TScreenMain.TAntialiasingSMAAEdgesRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TAntialiasingSMAAEdgesRenderPass.Show;
var Stream:TStream;
begin

 inherited Show;

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/antialiasing_smaa_edges_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/antialiasing_smaa_edges_color_frag.spv');
 try
  fVulkanFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVulkanVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fVulkanFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

 fVulkanSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         0.0,
                                         false,
                                         0.0,
                                         false,
                                         VK_COMPARE_OP_ALWAYS,
                                         0.0,
                                         0.0,
                                         VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK,
                                         false);

end;

procedure TScreenMain.TAntialiasingSMAAEdgesRenderPass.Hide;
begin
 FreeAndNil(fVulkanSampler);
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TAntialiasingSMAAEdgesRenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin
 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(pvApplication.VulkanDevice,
                                                                   fResourceColor.VulkanImages[InFlightFrameIndex],
                                                                   VK_IMAGE_VIEW_TYPE_2D_ARRAY,
                                                                   TpvFrameGraph.TImageResourceType(fResourceColor.ResourceType).Format,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                   0,
                                                                   1,
                                                                   0,
                                                                   fParent.fCountSurfaceViews
                                                                  );
  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                                fResourceColor.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceColor.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT),0,SizeOf(TpvVector4));
 fVulkanPipelineLayout.Initialize;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                           pvApplication.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           fVulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           VulkanRenderPassSubpassIndex,
                                                           nil,
                                                           0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
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
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_DST_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_ONE,
                                                                      VK_BLEND_FACTOR_ZERO,
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

end;

procedure TScreenMain.TAntialiasingSMAAEdgesRenderPass.BeforeDestroySwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fVulkanImageViews[InFlightFrameIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 fVulkanRenderPass:=nil;

 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TAntialiasingSMAAEdgesRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TAntialiasingSMAAEdgesRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var Metrices:TpvVector4;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 Metrices:=TpvVector4.Create(1.0/fParent.fWidth,1.0/fParent.fHeight,fParent.fWidth,fParent.fHeight);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      fVulkanPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,0,nil);
 aCommandBuffer.CmdPushConstants(fVulkanPipelineLayout.Handle,TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_VERTEX_BIT),0,SizeOf(TpvVector4),@Metrices);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
 aCommandBuffer.CmdDraw(3,1,0,0);
end;

{ TScreenMain.TAntialiasingSMAAWeightsRenderPass }

constructor TScreenMain.TAntialiasingSMAAWeightsRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='AntialiasingSMAAWeights';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

//SeparatePhysicalPass:=true;

//SeparateCommandBuffer:=true;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 fResourceEdges:=AddImageInput('resourcetype_smaa_edges',
                               'resource_smaa_edges',
                               VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                               []
                              );

 fResourceSurface:=AddImageOutput('resourcetype_smaa_weights',
                                  'resource_smaa_weights',
                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                  TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                               TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );

end;

destructor TScreenMain.TAntialiasingSMAAWeightsRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TAntialiasingSMAAWeightsRenderPass.Show;
var Stream:TStream;
begin

 inherited Show;

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/antialiasing_smaa_weights_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/antialiasing_smaa_weights_frag.spv');
 try
  fVulkanFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVulkanVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fVulkanFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

 fVulkanSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         0.0,
                                         false,
                                         0.0,
                                         false,
                                         VK_COMPARE_OP_ALWAYS,
                                         0.0,
                                         0.0,
                                         VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK,
                                         false);

end;

procedure TScreenMain.TAntialiasingSMAAWeightsRenderPass.Hide;
begin
 FreeAndNil(fVulkanSampler);
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TAntialiasingSMAAWeightsRenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin
 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fParent.fCountInFlightFrames*3);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fParent.fCountInFlightFrames*3);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(1,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(2,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(pvApplication.VulkanDevice,
                                                                   fResourceEdges.VulkanImages[InFlightFrameIndex],
                                                                   VK_IMAGE_VIEW_TYPE_2D_ARRAY,
                                                                   TpvFrameGraph.TImageResourceType(fResourceEdges.ResourceType).Format,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                   0,
                                                                   1,
                                                                   0,
                                                                   fParent.fCountSurfaceViews
                                                                  );
  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                                fResourceEdges.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceEdges.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                                fParent.fSMAAAreaTexture.ImageView.Handle,
                                                                                                TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                                fParent.fSMAASearchTexture.ImageView.Handle,
                                                                                                TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),0,SizeOf(TpvVector4));
 fVulkanPipelineLayout.Initialize;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                           pvApplication.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           fVulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           VulkanRenderPassSubpassIndex,
                                                           nil,
                                                           0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
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
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_DST_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_ONE,
                                                                      VK_BLEND_FACTOR_ZERO,
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

end;

procedure TScreenMain.TAntialiasingSMAAWeightsRenderPass.BeforeDestroySwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fVulkanImageViews[InFlightFrameIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 fVulkanRenderPass:=nil;

 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TAntialiasingSMAAWeightsRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TAntialiasingSMAAWeightsRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var Metrices:TpvVector4;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 Metrices:=TpvVector4.Create(1.0/fParent.fWidth,1.0/fParent.fHeight,fParent.fWidth,fParent.fHeight);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      fVulkanPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,0,nil);
 aCommandBuffer.CmdPushConstants(fVulkanPipelineLayout.Handle,TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_VERTEX_BIT) or TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_FRAGMENT_BIT),0,SizeOf(TpvVector4),@Metrices);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
 aCommandBuffer.CmdDraw(3,1,0,0);
end;

{ TScreenMain.TAntialiasingSMAABlendRenderPass }

constructor TScreenMain.TAntialiasingSMAABlendRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='AntialiasingSMAABlend';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

//SeparatePhysicalPass:=true;

//SeparateCommandBuffer:=true;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 fResourceColor:=AddImageInput('resourcetype_color_tonemapping',
                               'resource_tonemapping_color',
                               VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                               []
                              );

 fResourceWeights:=AddImageInput('resourcetype_smaa_weights',
                                 'resource_smaa_weights',
                                 VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                 []
                                );

 fResourceSurface:=AddImageOutput('resourcetype_color_antialiasing',
                                  'resource_antialiasing_color',
                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                  TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                               TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                  [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                 );

end;

destructor TScreenMain.TAntialiasingSMAABlendRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TAntialiasingSMAABlendRenderPass.Show;
var Stream:TStream;
begin

 inherited Show;

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/antialiasing_smaa_blend_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/antialiasing_smaa_blend_frag.spv');
 try
  fVulkanFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVulkanVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fVulkanFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

 fVulkanSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         0.0,
                                         false,
                                         0.0,
                                         false,
                                         VK_COMPARE_OP_ALWAYS,
                                         0.0,
                                         0.0,
                                         VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK,
                                         false);

end;

procedure TScreenMain.TAntialiasingSMAABlendRenderPass.Hide;
begin
 FreeAndNil(fVulkanSampler);
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TAntialiasingSMAABlendRenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin
 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fParent.fCountInFlightFrames*2);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,fParent.fCountInFlightFrames*2);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(1,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(pvApplication.VulkanDevice,
                                                                   fResourceColor.VulkanImages[InFlightFrameIndex],
                                                                   VK_IMAGE_VIEW_TYPE_2D_ARRAY,
                                                                   TpvFrameGraph.TImageResourceType(fResourceColor.ResourceType).Format,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                   0,
                                                                   1,
                                                                   0,
                                                                   fParent.fCountSurfaceViews
                                                                  );
  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                                fResourceColor.VulkanAdditionalFormatImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceColor.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                 [TVkDescriptorImageInfo.Create(fVulkanSampler.Handle,
                                                                                                fResourceWeights.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceWeights.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),0,SizeOf(TpvVector4));
 fVulkanPipelineLayout.Initialize;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                           pvApplication.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           fVulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           VulkanRenderPassSubpassIndex,
                                                           nil,
                                                           0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
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
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_DST_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_ONE,
                                                                      VK_BLEND_FACTOR_ZERO,
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

end;

procedure TScreenMain.TAntialiasingSMAABlendRenderPass.BeforeDestroySwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fVulkanImageViews[InFlightFrameIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 fVulkanRenderPass:=nil;

 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TAntialiasingSMAABlendRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TAntialiasingSMAABlendRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var Metrices:TpvVector4;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 Metrices:=TpvVector4.Create(1.0/fParent.fWidth,1.0/fParent.fHeight,fParent.fWidth,fParent.fHeight);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      fVulkanPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,0,nil);
 aCommandBuffer.CmdPushConstants(fVulkanPipelineLayout.Handle,TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_VERTEX_BIT) or TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_FRAGMENT_BIT),0,SizeOf(TpvVector4),@Metrices);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
 aCommandBuffer.CmdDraw(3,1,0,0);
end;

{ TScreenMain.TDitheringRenderPass }

constructor TScreenMain.TDitheringRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aParent:TScreenMain);
begin

 inherited Create(aFrameGraph);

 fParent:=aParent;

 Name:='Dithering';

 MultiviewMask:=fParent.fSurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

//SeparatePhysicalPass:=true;

//SeparateCommandBuffer:=true;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       1.0,
                                       1.0,
                                       1.0,
                                       fParent.fCountSurfaceViews);

 fResourceColor:=AddImageInput('resourcetype_color_antialiasing',
                               'resource_antialiasing_color',
                               VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                               [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                              );

 if assigned(UnitApplication.Application.VirtualReality) then begin
  fResourceSurface:=AddImageOutput('resourcetype_output_color',
                                   'resource_output',
                                   VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                   TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                   [TpvFrameGraph.TResourceTransition.TFlag.Attachment],
                                   TpvFrameGraph.TResourceInstanceType.Default,
                                   fParent.fExternalOutputImageData
                                  );
 end else begin
  fResourceSurface:=AddImageOutput('resourcetype_output_color',
                                   'resource_output',
                                   VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                   TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)),
                                   [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                  );
 end;

end;

destructor TScreenMain.TDitheringRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.TDitheringRenderPass.Show;
var Stream:TStream;
begin

 inherited Show;

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(FrameGraph.TransferQueue.CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 Stream:=pvApplication.Assets.GetAssetStream('shaders/fullscreen_vert.spv');
 try
  fVulkanVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/dithering_frag.spv');
 try
  fVulkanFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVulkanVertexShaderModule,'main');

 fVulkanPipelineShaderStageFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fVulkanFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

 fVulkanSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkFilter.VK_FILTER_LINEAR,
                                         TVkSamplerMipmapMode.VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                         0.0,
                                         false,
                                         0.0,
                                         false,
                                         VK_COMPARE_OP_ALWAYS,
                                         0.0,
                                         0.0,
                                         VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK,
                                         false);

end;

procedure TScreenMain.TDitheringRenderPass.Hide;
begin
 FreeAndNil(fVulkanSampler);
 FreeAndNil(fVulkanPipelineShaderStageVertex);
 FreeAndNil(fVulkanPipelineShaderStageFragment);
 FreeAndNil(fVulkanFragmentShaderModule);
 FreeAndNil(fVulkanVertexShaderModule);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 inherited Hide;
end;

procedure TScreenMain.TDitheringRenderPass.AfterCreateSwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin
 inherited AfterCreateSwapChain;

 fVulkanRenderPass:=VulkanRenderPass;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,fParent.fCountInFlightFrames);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  fVulkanImageViews[InFlightFrameIndex]:=TpvVulkanImageView.Create(pvApplication.VulkanDevice,
                                                                   fResourceColor.VulkanImages[InFlightFrameIndex],
                                                                   VK_IMAGE_VIEW_TYPE_2D_ARRAY,
                                                                   TpvFrameGraph.TImageResourceType(fResourceColor.ResourceType).Format,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                                                   TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                   0,
                                                                   1,
                                                                   0,
                                                                   fParent.fCountSurfaceViews
                                                                  );
  fVulkanDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                           fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                fResourceColor.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                                fResourceColor.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                                 [],
                                                                 [],
                                                                 false
                                                                );
  fVulkanDescriptorSets[InFlightFrameIndex].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),0,SizeOf(TpvInt32));
 fVulkanPipelineLayout.Initialize;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                           pvApplication.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           fVulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           VulkanRenderPassSubpassIndex,
                                                           nil,
                                                           0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,fParent.fWidth,fParent.fHeight,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,fParent.fWidth,fParent.fHeight);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
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
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_DST_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_ONE,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_OP_ADD,
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fVulkanGraphicsPipeline.Initialize;

 fVulkanGraphicsPipeline.FreeMemory;

end;

procedure TScreenMain.TDitheringRenderPass.BeforeDestroySwapChain;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fVulkanGraphicsPipeline);

 FreeAndNil(fVulkanPipelineLayout);

 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fVulkanImageViews[InFlightFrameIndex]);
 end;

 FreeAndNil(fVulkanDescriptorSetLayout);

 FreeAndNil(fVulkanDescriptorPool);

 fVulkanRenderPass:=nil;

 inherited BeforeDestroySwapChain;
end;

procedure TScreenMain.TDitheringRenderPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TScreenMain.TDitheringRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var FrameCounter:TpvInt32;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 FrameCounter:=TpvInt32(FrameGraph.DrawFrameIndex);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      fVulkanPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fVulkanDescriptorSets[aInFlightFrameIndex].Handle,0,nil);
 aCommandBuffer.CmdPushConstants(fVulkanPipelineLayout.Handle,TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_FRAGMENT_BIT),0,SizeOf(TpvInt32),@FrameCounter);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
 aCommandBuffer.CmdDraw(3,1,0,0);
end;

{ TScreenMain }

constructor TScreenMain.Create;
var GLTF:TPasGLTF.TDocument;
    AssetStream:TStream;
    SampleCounts:TVkSampleCountFlags;
    Center,Bounds:TpvVector3;
    CameraRotationX,CameraRotationY:TpvScalar;
    FormatProperties:TVkFormatProperties;
    MaxShadowMSAA,MaxMSAA:TpvInt32;
begin
 inherited Create;

 fCountInFlightFrames:=pvApplication.CountInFlightFrames;

 fOldFPS:=-1;

 fFPSTimeAccumulator:=0;

 fCascadedShadowMapSize:=Max(16,UnitApplication.Application.ShadowMapSize);

 fBufferDeviceAddress:=(pvApplication.VulkanDevice.PhysicalDevice.BufferDeviceAddressFeaturesKHR.bufferDeviceAddress<>VK_FALSE) and
                       (pvApplication.VulkanDevice.PhysicalDevice.BufferDeviceAddressFeaturesKHR.bufferDeviceAddressCaptureReplay<>VK_FALSE);
 if fBufferDeviceAddress then begin
  fMeshFragTypeName:='matbufref';
 end else begin
  fMeshFragTypeName:='matssbo';
 end;

 FormatProperties:=pvApplication.VulkanDevice.PhysicalDevice.GetFormatProperties(VK_FORMAT_B10G11R11_UFLOAT_PACK32);
 if //(pvApplication.VulkanDevice.PhysicalDevice.Properties.deviceType=VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU) and
    ((FormatProperties.linearTilingFeatures and (TVkFormatFeatureFlags(VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT) or
                                                 TVkFormatFeatureFlags(VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT) or
                                                 TVkFormatFeatureFlags(VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT) or
                                                 TVkFormatFeatureFlags(VK_FORMAT_FEATURE_TRANSFER_DST_BIT) or
                                                 TVkFormatFeatureFlags(VK_FORMAT_FEATURE_TRANSFER_SRC_BIT)))=(TVkFormatFeatureFlags(VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT) or
                                                                                                              TVkFormatFeatureFlags(VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT) or
                                                                                                              TVkFormatFeatureFlags(VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT) or
                                                                                                              TVkFormatFeatureFlags(VK_FORMAT_FEATURE_TRANSFER_DST_BIT) or
                                                                                                              TVkFormatFeatureFlags(VK_FORMAT_FEATURE_TRANSFER_SRC_BIT))) and
    ((FormatProperties.optimalTilingFeatures and (TVkFormatFeatureFlags(VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT) or
                                                  TVkFormatFeatureFlags(VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT) or
                                                  TVkFormatFeatureFlags(VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT) or
                                                  TVkFormatFeatureFlags(VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT) or
                                                  TVkFormatFeatureFlags(VK_FORMAT_FEATURE_TRANSFER_DST_BIT) or
                                                  TVkFormatFeatureFlags(VK_FORMAT_FEATURE_TRANSFER_SRC_BIT)))=(TVkFormatFeatureFlags(VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT) or
                                                                                                               TVkFormatFeatureFlags(VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT) or
                                                                                                               TVkFormatFeatureFlags(VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT) or
                                                                                                               TVkFormatFeatureFlags(VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT) or
                                                                                                               TVkFormatFeatureFlags(VK_FORMAT_FEATURE_TRANSFER_DST_BIT) or
                                                                                                               TVkFormatFeatureFlags(VK_FORMAT_FEATURE_TRANSFER_SRC_BIT))) then begin
  fOptimizedNonAlphaFormat:=VK_FORMAT_B10G11R11_UFLOAT_PACK32;
 end else begin
  fOptimizedNonAlphaFormat:=VK_FORMAT_R16G16B16A16_SFLOAT;
 end;

 case TpvVulkanVendorID(pvApplication.VulkanDevice.PhysicalDevice.Properties.vendorID) of
  TpvVulkanVendorID.ImgTec,
  TpvVulkanVendorID.ARM,
  TpvVulkanVendorID.Qualcomm,
  TpvVulkanVendorID.Vivante:begin
   // Tile-based GPUs => Use no depth prepass, as it can be counterproductive for those
   fUseDepthPrepass:=false;
  end;
  else begin
   // Immediate-based GPUs => Use depth prepass, as for which it can bring an advantage
   fUseDepthPrepass:=true;
  end;
 end;

 fUseDemote:=pvApplication.VulkanDevice.PhysicalDevice.ShaderDemoteToHelperInvocation;

 case TpvVulkanVendorID(pvApplication.VulkanDevice.PhysicalDevice.Properties.vendorID) of
  TpvVulkanVendorID.Intel:begin
   // Workaround for Intel (i)GPUs, which've problems with discarding fragments in 2x2 fragment blocks at alpha-test usage
   fUseNoDiscard:=not fUseDemote;
   fUseOITAlphaTest:=true;
  end;
  else begin
   fUseNoDiscard:=false;
   fUseOITAlphaTest:=false;
  end;
 end;

 fAnimationIndex:=0;

 fCameraMode:=TCameraMode.Orbit;

 fKeyLeft:=false;
 fKeyRight:=false;
 fKeyForwards:=false;
 fKeyBackwards:=false;
 fKeyUp:=false;
 fKeyDown:=false;
 fKeyPitchInc:=false;
 fKeyPitchDec:=false;
 fKeyYawInc:=false;
 fKeyYawDec:=false;
 fKeyRollInc:=false;
 fKeyRollDec:=false;

 FillChar(fCascadedShadowMapVulkanUniformBuffers,SizeOf(TCascadedShadowMapVulkanUniformBuffers),#0);

 fUpdateLock:=TPasMPCriticalSection.Create;

 fScene3D:=TpvScene3D.Create(pvApplication.ResourceManager,nil,fBufferDeviceAddress,fCountInFlightFrames);

 fPrimaryDirectionalLight:=TpvScene3D.TLight.Create(fScene3D);
 fPrimaryDirectionalLight.Type_:=TpvScene3D.TLightData.TType.PrimaryDirectional;
 fPrimaryDirectionalLight.Color:=TpvVector3.InlineableCreate(1.7,1.15,0.70);
 fPrimaryDirectionalLight.Matrix:=TpvMatrix4x4.CreateConstructZ(-TSkyCubeMap.LightDirection.xyz);
 fPrimaryDirectionalLight.Intensity:=1.0;
 fPrimaryDirectionalLight.Range:=0.0;
 fPrimaryDirectionalLight.CastShadows:=true;
 fPrimaryDirectionalLight.Data.Visible:=true;
 fPrimaryDirectionalLight.Visible:=true;
 fPrimaryDirectionalLight.Update;

 fGroup:=TpvScene3D.TGroup.Create(pvApplication.ResourceManager,fScene3D);
 try
  fGroup.Culling:=false; // true for GLTFs with large scenes like landscapes, cities, etc.
  GLTF:=TPasGLTF.TDocument.Create;
  try
   if FileExists(GLTFFileName) then begin
    GLTF.RootPath:=ExtractFilePath(ExpandFileName(GLTFFileName));
    AssetStream:=TFileStream.Create(GLTFFileName,fmOpenRead or fmShareDenyWrite);
   end else begin
    AssetStream:=pvApplication.Assets.GetAssetStream(GLTFFileName);
   end;
   if assigned(AssetStream) then begin
    try
     GLTF.LoadFromStream(AssetStream);
    finally
     FreeAndNil(AssetStream);
    end;
   end;
   fGroup.AssignFromGLTF(GLTF);
  finally
   FreeAndNil(GLTF);
  end;
 finally
 end;

 fGroupInstance:=fGroup.CreateInstance;

 if fScene3D.HasTransmission then begin
  fUseOITAlphaTest:=true;
 end;

 fFrameGraph:=TpvFrameGraph.Create(pvApplication.VulkanDevice,fCountInFlightFrames);

 fFrameGraph.SurfaceIsSwapchain:=true;

 fFrameGraph.DefaultResourceInstanceType:=TpvFrameGraph.TResourceInstanceType.InstancePerInFlightFrame;

 if assigned(UnitApplication.Application.VirtualReality) then begin

  fFOV:=UnitApplication.Application.VirtualReality.FOV;

  fZNear:=UnitApplication.Application.VirtualReality.ZNear;

  fZFar:=UnitApplication.Application.VirtualReality.ZFar;

  fCountSurfaceViews:=UnitApplication.Application.VirtualReality.CountImages;

  fSurfaceMultiviewMask:=UnitApplication.Application.VirtualReality.MultiviewMask;

 end else begin

  fFOV:=53.13010235415598;

  fZNear:=-0.01;

  fZFar:=-Infinity;

  fCountSurfaceViews:=1;

  fSurfaceMultiviewMask:=1 shl 0;

 end;

 fAntialiasingMode:=UnitApplication.Application.AntialiasingMode;

 if fAntialiasingMode=TAntialiasingMode.Auto then begin
  case TpvVulkanVendorID(pvApplication.VulkanDevice.PhysicalDevice.Properties.vendorID) of
   TpvVulkanVendorID.AMD:begin
    if pvApplication.VulkanDevice.PhysicalDevice.Properties.deviceType=VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU then begin
     fAntialiasingMode:=TAntialiasingMode.FXAA;
    end else begin
     fAntialiasingMode:=TAntialiasingMode.SMAA;
    end;
   end;
   TpvVulkanVendorID.NVIDIA:begin
    if pvApplication.VulkanDevice.PhysicalDevice.Properties.deviceType=VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU then begin
     fAntialiasingMode:=TAntialiasingMode.FXAA;
    end else begin
     fAntialiasingMode:=TAntialiasingMode.SMAA;
    end;
   end;
   TpvVulkanVendorID.Intel:begin
    if pvApplication.VulkanDevice.PhysicalDevice.Properties.deviceType=VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU then begin
     fAntialiasingMode:=TAntialiasingMode.FXAA;
    end else begin
     fAntialiasingMode:=TAntialiasingMode.SMAA;
    end;
   end;
   else begin
    if pvApplication.VulkanDevice.PhysicalDevice.Properties.deviceType=VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU then begin
     fAntialiasingMode:=TAntialiasingMode.DSAA;
    end else begin
     fAntialiasingMode:=TAntialiasingMode.FXAA;
    end;
   end;
  end;
 end;

 SampleCounts:=pvApplication.VulkanDevice.PhysicalDevice.Properties.limits.framebufferColorSampleCounts and
               pvApplication.VulkanDevice.PhysicalDevice.Properties.limits.framebufferDepthSampleCounts and
               pvApplication.VulkanDevice.PhysicalDevice.Properties.limits.framebufferStencilSampleCounts;

 if UnitApplication.Application.MaxShadowMSAA=0 then begin
  case TpvVulkanVendorID(pvApplication.VulkanDevice.PhysicalDevice.Properties.vendorID) of
   TpvVulkanVendorID.AMD:begin
    MaxShadowMSAA:=1;
   end;
   TpvVulkanVendorID.NVIDIA:begin
    if pvApplication.VulkanDevice.PhysicalDevice.Properties.deviceType=VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU then begin
     MaxShadowMSAA:=8;
    end else begin
     MaxShadowMSAA:=1;
    end;
   end;
   TpvVulkanVendorID.Intel:begin
    MaxShadowMSAA:=1;
   end;
   else begin
    MaxShadowMSAA:=1;
   end;
  end;
 end else begin
  MaxShadowMSAA:=UnitApplication.Application.MaxShadowMSAA;
 end;

 if UnitApplication.Application.MaxMSAA=0 then begin
  case TpvVulkanVendorID(pvApplication.VulkanDevice.PhysicalDevice.Properties.vendorID) of
   TpvVulkanVendorID.AMD:begin
    MaxMSAA:=2;
   end;
   TpvVulkanVendorID.NVIDIA:begin
    if pvApplication.VulkanDevice.PhysicalDevice.Properties.deviceType=VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU then begin
     MaxMSAA:=8;
    end else begin
     MaxMSAA:=2;
    end;
   end;
   TpvVulkanVendorID.Intel:begin
    MaxMSAA:=2;
   end;
   else begin
    MaxMSAA:=2;
   end;
  end;
 end else begin
  MaxMSAA:=UnitApplication.Application.MaxMSAA;
 end;

 if (MaxShadowMSAA>=64) and ((SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_64_BIT))<>0) then begin
  fVulkanShadowMapSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_64_BIT);
  fCountCascadedShadowMapMSAASamples:=64;
 end else if (MaxShadowMSAA>=32) and ((SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_32_BIT))<>0) then begin
  fVulkanShadowMapSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_32_BIT);
  fCountCascadedShadowMapMSAASamples:=32;
 end else if (MaxShadowMSAA>=16) and ((SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_16_BIT))<>0) then begin
  fVulkanShadowMapSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_16_BIT);
  fCountCascadedShadowMapMSAASamples:=16;
 end else if (MaxShadowMSAA>=8) and ((SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_8_BIT))<>0) then begin
  fVulkanShadowMapSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_8_BIT);
  fCountCascadedShadowMapMSAASamples:=8;
 end else if (MaxShadowMSAA>=4) and ((SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_4_BIT))<>0) then begin
  fVulkanShadowMapSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_4_BIT);
  fCountCascadedShadowMapMSAASamples:=4;
 end else if (MaxShadowMSAA>=2) and ((SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_2_BIT))<>0) then begin
  fVulkanShadowMapSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_2_BIT);
  fCountCascadedShadowMapMSAASamples:=2;
 end else begin
  fVulkanShadowMapSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT);
  fCountCascadedShadowMapMSAASamples:=1;
 end;

 if fAntialiasingMode=TAntialiasingMode.MSAA then begin
  if (MaxMSAA>=64) and ((SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_64_BIT))<>0) then begin
   fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_64_BIT);
   fCountSurfaceMSAASamples:=64;
  end else if (MaxMSAA>=32) and ((SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_32_BIT))<>0) then begin
   fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_32_BIT);
   fCountSurfaceMSAASamples:=32;
  end else if (MaxMSAA>=16) and ((SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_16_BIT))<>0) then begin
   fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_16_BIT);
   fCountSurfaceMSAASamples:=16;
  end else if (MaxMSAA>=8) and ((SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_8_BIT))<>0) then begin
   fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_8_BIT);
   fCountSurfaceMSAASamples:=8;
  end else if (MaxMSAA>=4) and ((SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_4_BIT))<>0) then begin
   fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_4_BIT);
   fCountSurfaceMSAASamples:=4;
  end else if (MaxMSAA>=2) and ((SampleCounts and TVkSampleCountFlags(VK_SAMPLE_COUNT_2_BIT))<>0) then begin
   fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_2_BIT);
   fCountSurfaceMSAASamples:=2;
  end else begin
   fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT);
   fCountSurfaceMSAASamples:=1;
   fAntialiasingMode:=TAntialiasingMode.FXAA;
  end;
 end else begin
  fVulkanSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT);
  fCountSurfaceMSAASamples:=1;
 end;

 //fVulkanShadowMapSampleCountFlagBits:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT);

{fFrameGraph.AddImageResourceType('resourcetype_surface',
                                  true,
                                  TVkFormat(VK_FORMAT_UNDEFINED),
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Surface,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT),
                                  1
                                 );}

 fTransparencyMode:=UnitApplication.Application.TransparencyMode;

 if fTransparencyMode=TTransparencyMode.Auto then begin
  case TpvVulkanVendorID(pvApplication.VulkanDevice.PhysicalDevice.Properties.vendorID) of
   TpvVulkanVendorID.AMD:begin
    if (fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT)) and
       (pvApplication.VulkanDevice.EnabledExtensionNames.IndexOf(VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME)>0) then begin
     // >= RDNA, since VK_EXT_post_depth_coverage exists just from RDNA on.
     fTransparencyMode:=TTransparencyMode.SPINLOCKOIT;
    end else begin
     if pvApplication.VulkanDevice.PhysicalDevice.Properties.deviceType=VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU then begin
      fTransparencyMode:=TTransparencyMode.WBOIT;
     end else begin
      fTransparencyMode:=TTransparencyMode.LOOPOIT;
     end;
    end;
   end;
   TpvVulkanVendorID.NVIDIA:begin
    if pvApplication.VulkanDevice.EnabledExtensionNames.IndexOf(VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME)>0 then begin
     if (pvApplication.VulkanDevice.EnabledExtensionNames.IndexOf(VK_EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME)>0) and
        pvApplication.VulkanDevice.PhysicalDevice.FragmentShaderPixelInterlock then begin
      fTransparencyMode:=TTransparencyMode.INTERLOCKOIT;
     end else begin
      fTransparencyMode:=TTransparencyMode.SPINLOCKOIT;
     end;
    end else begin
     if pvApplication.VulkanDevice.PhysicalDevice.Properties.deviceType=VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU then begin
      fTransparencyMode:=TTransparencyMode.WBOIT;
     end else begin
      fTransparencyMode:=TTransparencyMode.MBOIT;
     end;
    end;
   end;
   TpvVulkanVendorID.Intel:begin
    if (fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT)) and
       (pvApplication.VulkanDevice.EnabledExtensionNames.IndexOf(VK_EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME)>0) and
       pvApplication.VulkanDevice.PhysicalDevice.FragmentShaderPixelInterlock then begin
     fTransparencyMode:=TTransparencyMode.INTERLOCKOIT;
    end else begin
     if pvApplication.VulkanDevice.PhysicalDevice.Properties.deviceType=VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU then begin
      fTransparencyMode:=TTransparencyMode.WBOIT;
     end else begin
      fTransparencyMode:=TTransparencyMode.MBOIT;
     end;
    end;
   end;
   else begin
    if pvApplication.VulkanDevice.PhysicalDevice.Properties.deviceType=VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU then begin
     fTransparencyMode:=TTransparencyMode.Direct;
    end else begin
     fTransparencyMode:=TTransparencyMode.WBOIT;
    end;
   end;
  end;
 end;

 if assigned(UnitApplication.Application.VirtualReality) then begin

  fExternalOutputImageData:=TpvFrameGraph.TExternalImageData.Create(fFrameGraph);

  fFrameGraph.AddImageResourceType('resourcetype_output_color',
                                   true,
                                   UnitApplication.Application.VirtualReality.ImageFormat,
                                   TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                   TpvFrameGraph.TImageType.Color,
                                   TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                   TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT),
                                   1
                                  );
 end else begin

  fExternalOutputImageData:=nil;

  fFrameGraph.AddImageResourceType('resourcetype_output_color',
                                   true,
                                   TVkFormat(VK_FORMAT_UNDEFINED),
                                   TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                   TpvFrameGraph.TImageType.Surface,
                                   TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                   TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT),
                                   1
                                  );
 end;

 fFrameGraph.AddImageResourceType('resourcetype_msaa_color',
                                  false,
                                  VK_FORMAT_R16G16B16A16_SFLOAT,
                                  fVulkanSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_msaa_color_optimized_non_alpha',
                                  false,
                                  fOptimizedNonAlphaFormat,
                                  fVulkanSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_msaa_depth',
                                  false,
                                  VK_FORMAT_D32_SFLOAT{pvApplication.VulkanDepthImageFormat},
                                  fVulkanSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.From(VK_FORMAT_D32_SFLOAT{pvApplication.VulkanDepthImageFormat}),
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_msaa_predepth',
                                  false,
                                  VK_FORMAT_R32_SFLOAT,
                                  fVulkanSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_msaa_velocity',
                                  false,
                                  VK_FORMAT_R32G32_SFLOAT,
                                  fVulkanSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_msaa_normals',
                                  false,
                                  VK_FORMAT_A2B10G10R10_UNORM_PACK32,
                                  fVulkanSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_mboit_data',
                                  false,
                                  VK_FORMAT_R32G32B32A32_SFLOAT,
                                  fVulkanSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_wboit_accumulation',
                                  false,
                                  VK_FORMAT_R16G16B16A16_SFLOAT,
                                  fVulkanSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_wboit_revealage',
                                  false,
                                  VK_FORMAT_R32_SFLOAT,
                                  fVulkanSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_color_optimized_non_alpha',
                                  true,
                                  fOptimizedNonAlphaFormat,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_color_tonemapping',
                                  true,
                                  VK_FORMAT_R8G8B8A8_SRGB,//TVkFormat(TpvInt32(IfThen(fVulkanSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),TpvInt32(VK_FORMAT_R8G8B8A8_SRGB),TpvInt32(VK_FORMAT_R8G8B8A8_UNORM)))),
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1,
                                  VK_IMAGE_LAYOUT_UNDEFINED,
                                  VK_IMAGE_LAYOUT_UNDEFINED,
                                  VK_FORMAT_R8G8B8A8_UNORM
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_color',
                                  true,
                                  VK_FORMAT_R16G16B16A16_SFLOAT,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_color_antialiasing',
                                  true,
                                  VK_FORMAT_R8G8B8A8_SRGB,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_depth',
                                  true,
                                  VK_FORMAT_D32_SFLOAT{pvApplication.VulkanDepthImageFormat},
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.From(VK_FORMAT_D32_SFLOAT{pvApplication.VulkanDepthImageFormat}),
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_predepth',
                                  true,
                                  VK_FORMAT_R32_SFLOAT,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_velocity',
                                  true,
                                  VK_FORMAT_R32G32_SFLOAT,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_normals',
                                  true,
                                  VK_FORMAT_A2B10G10R10_UNORM_PACK32,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_ssao',
                                  true,
                                  VK_FORMAT_R32G32_SFLOAT,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_ssao_final',
                                  true,
                                  VK_FORMAT_R8_UNORM,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_cascadedshadowmap_msaa_data',
                                  false,
                                  VK_FORMAT_R16G16B16A16_UNORM,
//                                VK_FORMAT_R32G32B32A32_SFLOAT,
                                  fVulkanShadowMapSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.Absolute,CascadedShadowMapWidth,CascadedShadowMapHeight,1.0,CountCascadedShadowMapCascades),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_cascadedshadowmap_msaa_depth',
                                  false,
                                  VK_FORMAT_D32_SFLOAT,
                                  fVulkanShadowMapSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.From(VK_FORMAT_D32_SFLOAT),
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.Absolute,CascadedShadowMapWidth,CascadedShadowMapHeight,1.0,CountCascadedShadowMapCascades),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_cascadedshadowmap_data',
                                  false,
                                  VK_FORMAT_R16G16B16A16_UNORM,
//                                VK_FORMAT_R32G32B32A32_SFLOAT,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.Absolute,CascadedShadowMapWidth,CascadedShadowMapHeight,1.0,CountCascadedShadowMapCascades),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_cascadedshadowmap_depth',
                                  false,
                                  VK_FORMAT_D32_SFLOAT,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.From(VK_FORMAT_D32_SFLOAT),
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.Absolute,CascadedShadowMapWidth,CascadedShadowMapHeight,1.0,CountCascadedShadowMapCascades),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_smaa_edges',
                                  false,
                                  VK_FORMAT_R8G8_UNORM,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_smaa_weights',
                                  false,
                                  VK_FORMAT_R8G8B8A8_UNORM,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fMeshComputePass:=TMeshComputePass.Create(fFrameGraph,self);

 fDepthVelocityNormalsRenderPass:=TDepthVelocityNormalsRenderPass.Create(fFrameGraph,self);
 fDepthVelocityNormalsRenderPass.AddExplicitPassDependency(fMeshComputePass);

 fDepthMipMapComputePass:=TDepthMipMapComputePass.Create(fFrameGraph,self);
 fDepthMipMapComputePass.AddExplicitPassDependency(fDepthVelocityNormalsRenderPass);

 fCascadedShadowMapRenderPass:=TCascadedShadowMapRenderPass.Create(fFrameGraph,self);
 fCascadedShadowMapRenderPass.AddExplicitPassDependency(fMeshComputePass);
 fCascadedShadowMapRenderPass.AddExplicitPassDependency(fDepthVelocityNormalsRenderPass);
 fCascadedShadowMapRenderPass.AddExplicitPassDependency(fDepthMipMapComputePass);

 fCascadedShadowMapResolveRenderPass:=TCascadedShadowMapResolveRenderPass.Create(fFrameGraph,self);

 fCascadedShadowMapBlurRenderPasses[0]:=TCascadedShadowMapBlurRenderPass.Create(fFrameGraph,self,true);

 fCascadedShadowMapBlurRenderPasses[1]:=TCascadedShadowMapBlurRenderPass.Create(fFrameGraph,self,false);

 fSSAORenderPass:=TSSAORenderPass.Create(fFrameGraph,self);
 fSSAORenderPass.AddExplicitPassDependency(fDepthMipMapComputePass);

 fSSAOBlurRenderPasses[0]:=TSSAOBlurRenderPass.Create(fFrameGraph,self,true);

 fSSAOBlurRenderPasses[1]:=TSSAOBlurRenderPass.Create(fFrameGraph,self,false);

 fForwardRenderPass:=TForwardRenderPass.Create(fFrameGraph,self);
 fForwardRenderPass.AddExplicitPassDependency(fMeshComputePass);
 fForwardRenderPass.AddExplicitPassDependency(fDepthMipMapComputePass);
 fForwardRenderPass.AddExplicitPassDependency(fSSAOBlurRenderPasses[1]);

 fForwardRenderMipMapComputePass:=TForwardRenderMipMapComputePass.Create(fFrameGraph,self);

 case fTransparencyMode of

  TTransparencyMode.Direct:begin

   fDirectTransparencyRenderPass:=TDirectTransparencyRenderPass.Create(fFrameGraph,self);
   fDirectTransparencyRenderPass.AddExplicitPassDependency(fMeshComputePass);
   fDirectTransparencyRenderPass.AddExplicitPassDependency(fDepthMipMapComputePass);
   fDirectTransparencyRenderPass.AddExplicitPassDependency(fForwardRenderMipMapComputePass);

   fDirectTransparencyResolveRenderPass:=TDirectTransparencyResolveRenderPass.Create(fFrameGraph,self);

  end;

  TTransparencyMode.SPINLOCKOIT,
  TTransparencyMode.INTERLOCKOIT:begin

   fLockOrderIndependentTransparencyClearCustomPass:=TLockOrderIndependentTransparencyClearCustomPass.Create(fFrameGraph,self);

   fLockOrderIndependentTransparencyRenderPass:=TLockOrderIndependentTransparencyRenderPass.Create(fFrameGraph,self);
   fLockOrderIndependentTransparencyRenderPass.AddExplicitPassDependency(fMeshComputePass);
   fLockOrderIndependentTransparencyRenderPass.AddExplicitPassDependency(fLockOrderIndependentTransparencyClearCustomPass);
   fLockOrderIndependentTransparencyRenderPass.AddExplicitPassDependency(fDepthMipMapComputePass);
   fLockOrderIndependentTransparencyRenderPass.AddExplicitPassDependency(fForwardRenderMipMapComputePass);

   fLockOrderIndependentTransparencyBarrierCustomPass:=TLockOrderIndependentTransparencyBarrierCustomPass.Create(fFrameGraph,self);
   fLockOrderIndependentTransparencyBarrierCustomPass.AddExplicitPassDependency(fLockOrderIndependentTransparencyRenderPass);

   fLockOrderIndependentTransparencyResolveRenderPass:=TLockOrderIndependentTransparencyResolveRenderPass.Create(fFrameGraph,self);
   fLockOrderIndependentTransparencyResolveRenderPass.AddExplicitPassDependency(fLockOrderIndependentTransparencyBarrierCustomPass);

  end;

  TTransparencyMode.LOOPOIT:begin

   fLoopOrderIndependentTransparencyClearCustomPass:=TLoopOrderIndependentTransparencyClearCustomPass.Create(fFrameGraph,self);

   fLoopOrderIndependentTransparencyPass1RenderPass:=TLoopOrderIndependentTransparencyPass1RenderPass.Create(fFrameGraph,self);
   fLoopOrderIndependentTransparencyPass1RenderPass.AddExplicitPassDependency(fMeshComputePass);
   fLoopOrderIndependentTransparencyPass1RenderPass.AddExplicitPassDependency(fLoopOrderIndependentTransparencyClearCustomPass);
   fLoopOrderIndependentTransparencyPass1RenderPass.AddExplicitPassDependency(fDepthMipMapComputePass);
   fLoopOrderIndependentTransparencyPass1RenderPass.AddExplicitPassDependency(fForwardRenderMipMapComputePass);

   fLoopOrderIndependentTransparencyPass1BarrierCustomPass:=TLoopOrderIndependentTransparencyPass1BarrierCustomPass.Create(fFrameGraph,self);
   fLoopOrderIndependentTransparencyPass1BarrierCustomPass.AddExplicitPassDependency(fLoopOrderIndependentTransparencyPass1RenderPass);

   fLoopOrderIndependentTransparencyPass2RenderPass:=TLoopOrderIndependentTransparencyPass2RenderPass.Create(fFrameGraph,self);
   fLoopOrderIndependentTransparencyPass2RenderPass.AddExplicitPassDependency(fLoopOrderIndependentTransparencyPass1BarrierCustomPass);

   fLoopOrderIndependentTransparencyPass2BarrierCustomPass:=TLoopOrderIndependentTransparencyPass2BarrierCustomPass.Create(fFrameGraph,self);
   fLoopOrderIndependentTransparencyPass2BarrierCustomPass.AddExplicitPassDependency(fLoopOrderIndependentTransparencyPass2RenderPass);

   fLoopOrderIndependentTransparencyResolveRenderPass:=TLoopOrderIndependentTransparencyResolveRenderPass.Create(fFrameGraph,self);
   fLoopOrderIndependentTransparencyResolveRenderPass.AddExplicitPassDependency(fLoopOrderIndependentTransparencyPass2BarrierCustomPass);

  end;

  TTransparencyMode.WBOIT:begin

   fWeightBlendedOrderIndependentTransparencyRenderPass:=TWeightBlendedOrderIndependentTransparencyRenderPass.Create(fFrameGraph,self);
   fWeightBlendedOrderIndependentTransparencyRenderPass.AddExplicitPassDependency(fMeshComputePass);
   fWeightBlendedOrderIndependentTransparencyRenderPass.AddExplicitPassDependency(fDepthMipMapComputePass);
   fWeightBlendedOrderIndependentTransparencyRenderPass.AddExplicitPassDependency(fForwardRenderMipMapComputePass);

   fWeightBlendedOrderIndependentTransparencyResolveRenderPass:=TWeightBlendedOrderIndependentTransparencyResolveRenderPass.Create(fFrameGraph,self);

  end;

  TTransparencyMode.MBOIT:begin

   fMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass:=TMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass.Create(fFrameGraph,self);
   fMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass.AddExplicitPassDependency(fMeshComputePass);
   fMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass.AddExplicitPassDependency(fDepthMipMapComputePass);
   fMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass.AddExplicitPassDependency(fForwardRenderMipMapComputePass);

   fMomentBasedOrderIndependentTransparencyTransmittanceRenderPass:=TMomentBasedOrderIndependentTransparencyTransmittanceRenderPass.Create(fFrameGraph,self);

   fMomentBasedOrderIndependentTransparencyResolveRenderPass:=TMomentBasedOrderIndependentTransparencyResolveRenderPass.Create(fFrameGraph,self);

  end;

  else begin
  end;

 end;

 fTonemappingRenderPass:=TTonemappingRenderPass.Create(fFrameGraph,self);

 case fAntialiasingMode of
  TAntialiasingMode.DSAA:begin
   fAntialiasingDSAARenderPass:=TAntialiasingDSAARenderPass.Create(fFrameGraph,self);
  end;
  TAntialiasingMode.FXAA:begin
   fAntialiasingFXAARenderPass:=TAntialiasingFXAARenderPass.Create(fFrameGraph,self);
  end;
  TAntialiasingMode.SMAA:begin
   fAntialiasingSMAAEdgesRenderPass:=TAntialiasingSMAAEdgesRenderPass.Create(fFrameGraph,self);
   fAntialiasingSMAAWeightsRenderPass:=TAntialiasingSMAAWeightsRenderPass.Create(fFrameGraph,self);
   fAntialiasingSMAABlendRenderPass:=TAntialiasingSMAABlendRenderPass.Create(fFrameGraph,self);
  end;
  else begin
   fAntialiasingNoneRenderPass:=TAntialiasingNoneRenderPass.Create(fFrameGraph,self);
  end;
 end;

 fDitheringRenderPass:=TDitheringRenderPass.Create(fFrameGraph,self);

 fFrameGraph.RootPass:=fDitheringRenderPass;

 fFrameGraph.DoWaitOnSemaphore:=true;

 fFrameGraph.DoSignalSemaphore:=true;

 fFrameGraph.Compile;

// fFrameGraph.Dump;

 Center:=(fGroup.BoundingBox.Min+fGroup.BoundingBox.Max)*0.5;

 Bounds:=(fGroup.BoundingBox.Max-fGroup.BoundingBox.Min)*0.5;

 CameraRotationX:=0.0;
 CameraRotationY:=0.0;

 fCameraMatrix:=TpvMatrix4x4.CreateLookAt(Center+(TpvVector3.Create(sin(CameraRotationX*PI*2.0)*cos(-CameraRotationY*PI*2.0),
                                                                     sin(-CameraRotationY*PI*2.0),
                                                                     cos(CameraRotationX*PI*2.0)*cos(-CameraRotationY*PI*2.0)).Normalize*
                                                           (Max(Max(Bounds[0],Bounds[1]),Bounds[2])*2.0*1.0)),
                                           Center,
                                           TpvVector3.Create(0.0,1.0,0.0)).SimpleInverse;

 fCameraSpeed:=Max(1.0,fGroup.BoundingBox.Radius)*0.1;

end;

destructor TScreenMain.Destroy;
var Index:TpvSizeInt;
begin
 for Index:=0 to fCountInFlightFrames-1 do begin
  FreeAndNil(fCascadedShadowMapVulkanUniformBuffers[Index]);
 end;
 FreeAndNil(fSMAAAreaTexture);
 FreeAndNil(fSMAASearchTexture);
 FreeAndNil(fFrameGraph);
 FreeAndNil(fGroupInstance);
 FreeAndNil(fGroup);
 FreeAndNil(fPrimaryDirectionalLight);
 FreeAndNil(fScene3D);
 FreeAndNil(fUpdateLock);
 inherited Destroy;
end;

procedure TScreenMain.Show;
var Index:TpvSizeInt;
    Stream:TStream;
    GraphicsQueue:TpvVulkanQueue;
    GraphicsCommandPool:TpvVulkanCommandPool;
    GraphicsCommandBuffer:TpvVulkanCommandBuffer;
    GraphicsFence:TpvVulkanFence;
begin

 inherited Show;

 fSkyCubeMap:=TSkyCubeMap.Create(fOptimizedNonAlphaFormat);

 fGGXBRDF:=TGGXBRDF.Create;

 fGGXEnvMapCubeMap:=TGGXEnvMapCubeMap.Create(fSkyCubeMap.DescriptorImageInfo,fOptimizedNonAlphaFormat);

 fCharlieBRDF:=TCharlieBRDF.Create;

 fCharlieEnvMapCubeMap:=TCharlieEnvMapCubeMap.Create(fSkyCubeMap.DescriptorImageInfo,fOptimizedNonAlphaFormat);

 fLambertianEnvMapCubeMap:=TLambertianEnvMapCubeMap.Create(fSkyCubeMap.DescriptorImageInfo,fOptimizedNonAlphaFormat);

 fTime:=0.0;

 fCameraRotationX:=0.0;//frac(fTime*0.03125);

 fCameraRotationY:=0.0;

 fZoom:=1.0;

 fVulkanGraphicsCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                         pvApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                                         TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

 fVulkanGraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(fVulkanGraphicsCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanGraphicsCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fVulkanTransferCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                         pvApplication.VulkanDevice.TransferQueueFamilyIndex,
                                                         TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(fVulkanTransferCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fScene3D.Upload;

 Stream:=pvApplication.Assets.GetAssetStream('textures/sheenelut.png');
 try
  fSheenELUT:=TpvVulkanTexture.CreateFromImage(pvApplication.VulkanDevice,
                                                pvApplication.VulkanDevice.GraphicsQueue,
                                                fVulkanGraphicsCommandBuffer,
                                                fVulkanGraphicsCommandBufferFence,
                                                pvApplication.VulkanDevice.TransferQueue,
                                                fVulkanTransferCommandBuffer,
                                                fVulkanTransferCommandBufferFence,
                                                Stream,
                                                false,
                                                false);
  fSheenELUT.UpdateSampler;
 finally
  FreeAndNil(Stream);
 end;

 FillChar(fInFlightFrameStates,SizeOf(TInFlightFrameStates),#0);

 fVulkanFlushQueue:=pvApplication.VulkanDevice.UniversalQueue;

 fVulkanFlushCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                      pvApplication.VulkanDevice.UniversalQueueFamilyIndex,
                                                      TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

 for Index:=0 to fCountInFlightFrames-1 do begin

  fVulkanFlushCommandBuffers[Index]:=TpvVulkanCommandBuffer.Create(fVulkanFlushCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

  fVulkanFlushCommandBufferFences[Index]:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

  fVulkanFlushSemaphores[Index]:=TpvVulkanSemaphore.Create(pvApplication.VulkanDevice);

  fVulkanRenderSemaphores[Index]:=TpvVulkanSemaphore.Create(pvApplication.VulkanDevice);

 end;

 for Index:=0 to fCountInFlightFrames-1 do begin
  fCascadedShadowMapVulkanUniformBuffers[Index]:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                                        SizeOf(TCascadedShadowMapUniformBuffer),
                                                                        TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT),
                                                                        TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                        [],
                                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                        0,
                                                                        0,
                                                                        0,
                                                                        0,
                                                                        0,
                                                                        0,
                                                                        []);
 end;

 case fTransparencyMode of
  TTransparencyMode.SPINLOCKOIT,
  TTransparencyMode.INTERLOCKOIT:begin
   fLockOrderIndependentTransparentUniformVulkanBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                                               SizeOf(TLockOrderIndependentTransparentUniformBuffer),
                                                                               TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT),
                                                                               TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                               [],
                                                                               0,
                                                                               TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                               0,
                                                                               TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                               0,
                                                                               0,
                                                                               0,
                                                                               0,
                                                                               []);

  end;
  TTransparencyMode.LOOPOIT:begin
   fLoopOrderIndependentTransparentUniformVulkanBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                                               SizeOf(TLoopOrderIndependentTransparentUniformBuffer),
                                                                               TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT),
                                                                               TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                               [],
                                                                               0,
                                                                               TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                               0,
                                                                               TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                               0,
                                                                               0,
                                                                               0,
                                                                               0,
                                                                               []);

  end;
  TTransparencyMode.WBOIT,
  TTransparencyMode.MBOIT:begin
   fApproximationOrderIndependentTransparentUniformVulkanBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                                                        SizeOf(TApproximationOrderIndependentTransparentUniformBuffer),
                                                                                        TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT),
                                                                                        TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                                        [],
                                                                                        0,
                                                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                                        0,
                                                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                                        0,
                                                                                        0,
                                                                                        0,
                                                                                        0,
                                                                                        []);
  end;
  else begin
  end;
 end;

 case fAntialiasingMode of
  TAntialiasingMode.SMAA:begin
   GraphicsQueue:=pvApplication.VulkanDevice.GraphicsQueue;
   try
    GraphicsCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                     pvApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                                     TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
    try
     GraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(GraphicsCommandPool,
                                                          VK_COMMAND_BUFFER_LEVEL_PRIMARY);
     try
      GraphicsFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);
      try
       fSMAAAreaTexture:=TpvVulkanTexture.CreateFromMemory(pvApplication.VulkanDevice,
                                                           GraphicsQueue,
                                                           GraphicsCommandBuffer,
                                                           GraphicsFence,
                                                           GraphicsQueue,
                                                           GraphicsCommandBuffer,
                                                           GraphicsFence,
                                                           VK_FORMAT_R8G8_UNORM,
                                                           VK_SAMPLE_COUNT_1_BIT,
                                                           UnitSMAAData.AREATEX_WIDTH,
                                                           UnitSMAAData.AREATEX_HEIGHT,
                                                           0,
                                                           0,
                                                           1,
                                                           0,
                                                           [TpvVulkanTextureUsageFlag.General,
                                                            TpvVulkanTextureUsageFlag.TransferDst,
                                                            TpvVulkanTextureUsageFlag.TransferSrc,
                                                            TpvVulkanTextureUsageFlag.Sampled],
                                                           @UnitSMAAData.AreaTexBytes[0],
                                                           UnitSMAAData.AREATEX_SIZE,
                                                           false,
                                                           false,
                                                           0,
                                                           true,
                                                           false);
       fSMAASearchTexture:=TpvVulkanTexture.CreateFromMemory(pvApplication.VulkanDevice,
                                                             GraphicsQueue,
                                                             GraphicsCommandBuffer,
                                                             GraphicsFence,
                                                             GraphicsQueue,
                                                             GraphicsCommandBuffer,
                                                             GraphicsFence,
                                                             VK_FORMAT_R8_UNORM,
                                                             VK_SAMPLE_COUNT_1_BIT,
                                                             UnitSMAAData.SEARCHTEX_WIDTH,
                                                             UnitSMAAData.SEARCHTEX_HEIGHT,
                                                             0,
                                                             0,
                                                             1,
                                                             0,
                                                             [TpvVulkanTextureUsageFlag.General,
                                                              TpvVulkanTextureUsageFlag.TransferDst,
                                                              TpvVulkanTextureUsageFlag.TransferSrc,
                                                              TpvVulkanTextureUsageFlag.Sampled],
                                                             @UnitSMAAData.SearchTexBytes[0],
                                                             UnitSMAAData.SEARCHTEX_SIZE,
                                                             false,
                                                             false,
                                                             0,
                                                             true,
                                                             false);
      finally
       FreeAndNil(GraphicsFence);
      end;
     finally
      FreeAndNil(GraphicsCommandBuffer);
     end;
    finally
     FreeAndNil(GraphicsCommandPool);
    end;
   finally
    GraphicsQueue:=nil;
   end;
  end;
  else begin
  end;
 end;

 fFrameGraph.Show;

 pvApplication.SkipNextDrawFrame:=true;

end;

procedure TScreenMain.Hide;
var Index:TpvSizeInt;
begin

 fFrameGraph.Hide;

 for Index:=0 to fCountInFlightFrames-1 do begin

  FreeAndNil(fVulkanRenderSemaphores[Index]);

  FreeAndNil(fVulkanFlushCommandBuffers[Index]);

  FreeAndNil(fVulkanFlushCommandBufferFences[Index]);

  FreeAndNil(fVulkanFlushSemaphores[Index]);

 end;

 FreeAndNil(fVulkanFlushCommandPool);

 for Index:=0 to fCountInFlightFrames-1 do begin
  FreeAndNil(fCascadedShadowMapVulkanUniformBuffers[Index]);
 end;

 fScene3D.Unload;

 case fTransparencyMode of
  TTransparencyMode.SPINLOCKOIT,
  TTransparencyMode.INTERLOCKOIT:begin
   FreeAndNil(fLockOrderIndependentTransparentUniformVulkanBuffer);
  end;
  TTransparencyMode.LOOPOIT:begin
   FreeAndNil(fLoopOrderIndependentTransparentUniformVulkanBuffer);
  end;
  TTransparencyMode.WBOIT,
  TTransparencyMode.MBOIT:begin
   FreeAndNil(fApproximationOrderIndependentTransparentUniformVulkanBuffer);
  end;
  else begin
  end;
 end;

 case fAntialiasingMode of
  TAntialiasingMode.SMAA:begin
   FreeAndNil(fSMAAAreaTexture);
   FreeAndNil(fSMAASearchTexture);
  end;
  else begin
  end;
 end;


 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 FreeAndNil(fVulkanTransferCommandPool);
 FreeAndNil(fVulkanGraphicsCommandBufferFence);
 FreeAndNil(fVulkanGraphicsCommandBuffer);
 FreeAndNil(fVulkanGraphicsCommandPool);

 FreeAndNil(fSheenELUT);

 FreeAndNil(fCharlieEnvMapCubeMap);

 FreeAndNil(fCharlieBRDF);

 FreeAndNil(fGGXEnvMapCubeMap);

 FreeAndNil(fGGXBRDF);

 FreeAndNil(fLambertianEnvMapCubeMap);

 FreeAndNil(fSkyCubeMap);

 inherited Hide;
end;

procedure TScreenMain.Resume;
begin
 inherited Resume;
 pvApplication.SkipNextDrawFrame:=true;
end;

procedure TScreenMain.Pause;
begin
 inherited Pause;
end;

procedure TScreenMain.Resize(const aWidth,aHeight:TpvInt32);
begin
 inherited Resize(aWidth,aHeight);
 pvApplication.SkipNextDrawFrame:=true;
end;

procedure TScreenMain.AfterCreateSwapChain;
var Index:TpvSizeInt;
begin
 inherited AfterCreateSwapChain;

 if assigned(UnitApplication.Application.VirtualReality) then begin

  fWidth:=UnitApplication.Application.VirtualReality.Width;

  fHeight:=UnitApplication.Application.VirtualReality.Height;

 end else begin

  fWidth:=pvApplication.VulkanSwapChain.Width;

  fHeight:=pvApplication.VulkanSwapChain.Height;

 end;

 FillChar(fInFlightFrameStates,SizeOf(TInFlightFrameStates),#0);

 fFrameGraph.SetSwapChain(pvApplication.VulkanSwapChain,
                          pvApplication.VulkanDepthImageFormat);

 if assigned(UnitApplication.Application.VirtualReality) then begin

  fFrameGraph.SurfaceWidth:=fWidth;
  fFrameGraph.SurfaceHeight:=fHeight;

  fExternalOutputImageData.VulkanImages.Clear;
  for Index:=0 to UnitApplication.Application.VirtualReality.VulkanImages.Count-1 do begin
   fExternalOutputImageData.VulkanImages.Add(UnitApplication.Application.VirtualReality.VulkanImages[Index]);
  end;

  (fFrameGraph.ResourceTypeByName['resourcetype_output_color'] as TpvFrameGraph.TImageResourceType).Format:=UnitApplication.Application.VirtualReality.ImageFormat;

 end;

 for Index:=0 to fFrameGraph.CountInFlightFrames-1 do begin
  fDepthMipmappedArray2DImages[Index]:=TMipmappedArray2DImage.Create(fWidth,fHeight,fCountSurfaceViews,VK_FORMAT_R32_SFLOAT,false,VK_SAMPLE_COUNT_1_BIT,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
  fForwardMipmappedArray2DImages[Index]:=TMipmappedArray2DImage.Create(fWidth,fHeight,fCountSurfaceViews,fOptimizedNonAlphaFormat,true,VK_SAMPLE_COUNT_1_BIT,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
 end;

 case fTransparencyMode of

  TTransparencyMode.SPINLOCKOIT,
  TTransparencyMode.INTERLOCKOIT:begin

   fCountLockOrderIndependentTransparencyLayers:=CountOrderIndependentTransparencyLayers;//Min(Max(CountOrderIndependentTransparencyLayers,fCountSurfaceMSAASamples),16);

   fLockOrderIndependentTransparentUniformBuffer.ViewPort.x:=fWidth;
   fLockOrderIndependentTransparentUniformBuffer.ViewPort.y:=fHeight;
   fLockOrderIndependentTransparentUniformBuffer.ViewPort.z:=fLockOrderIndependentTransparentUniformBuffer.ViewPort.x*fLockOrderIndependentTransparentUniformBuffer.ViewPort.y;
   fLockOrderIndependentTransparentUniformBuffer.ViewPort.w:=(fCountLockOrderIndependentTransparencyLayers and $ffff) or ((fCountSurfaceMSAASamples and $ffff) shl 16);

   fLockOrderIndependentTransparentUniformVulkanBuffer.UploadData(pvApplication.VulkanDevice.TransferQueue,
                                                                  fVulkanTransferCommandBuffer,
                                                                  fVulkanTransferCommandBufferFence,
                                                                  fLockOrderIndependentTransparentUniformBuffer,
                                                                  0,
                                                                  SizeOf(TLockOrderIndependentTransparentUniformBuffer));

   for Index:=0 to fFrameGraph.CountInFlightFrames-1 do begin

    fLockOrderIndependentTransparencyABufferBuffers[Index]:=TOrderIndependentTransparencyBuffer.Create(fWidth*fHeight*fCountLockOrderIndependentTransparencyLayers*fCountSurfaceViews*(SizeOf(UInt32)*4),
                                                                                                       VK_FORMAT_R32G32B32A32_UINT,
                                                                                                       TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT));

    fLockOrderIndependentTransparencyAuxImages[Index]:=TOrderIndependentTransparencyImage.Create(fWidth,
                                                                                                 fHeight,
                                                                                                 fCountSurfaceViews,
                                                                                                 VK_FORMAT_R32_UINT,
                                                                                                 VK_SAMPLE_COUNT_1_BIT);

    if fTransparencyMode=TTransparencyMode.SPINLOCKOIT then begin
     fLockOrderIndependentTransparencySpinLockImages[Index]:=TOrderIndependentTransparencyImage.Create(fWidth,
                                                                                                       fHeight,
                                                                                                       fCountSurfaceViews,
                                                                                                       VK_FORMAT_R32_UINT,
                                                                                                       VK_SAMPLE_COUNT_1_BIT);
    end;

   end;

  end;

  TTransparencyMode.LOOPOIT:begin

   fCountLoopOrderIndependentTransparencyLayers:=CountOrderIndependentTransparencyLayers;//Min(Max(CountOrderIndependentTransparencyLayers,fCountSurfaceMSAASamples),16);

   fLoopOrderIndependentTransparentUniformBuffer.ViewPort.x:=fWidth;
   fLoopOrderIndependentTransparentUniformBuffer.ViewPort.y:=fHeight;
   fLoopOrderIndependentTransparentUniformBuffer.ViewPort.z:=fLoopOrderIndependentTransparentUniformBuffer.ViewPort.x*fLoopOrderIndependentTransparentUniformBuffer.ViewPort.y;
   fLoopOrderIndependentTransparentUniformBuffer.ViewPort.w:=(fCountLoopOrderIndependentTransparencyLayers and $ffff) or ((fCountSurfaceMSAASamples and $ffff) shl 16);

   fLoopOrderIndependentTransparentUniformVulkanBuffer.UploadData(pvApplication.VulkanDevice.TransferQueue,
                                                                  fVulkanTransferCommandBuffer,
                                                                  fVulkanTransferCommandBufferFence,
                                                                  fLoopOrderIndependentTransparentUniformBuffer,
                                                                  0,
                                                                  SizeOf(TLoopOrderIndependentTransparentUniformBuffer));

   for Index:=0 to fFrameGraph.CountInFlightFrames-1 do begin

    fLoopOrderIndependentTransparencyABufferBuffers[Index]:=TOrderIndependentTransparencyBuffer.Create(fWidth*fHeight*fCountLoopOrderIndependentTransparencyLayers*fCountSurfaceViews*(SizeOf(UInt32)*2),
                                                                                                       VK_FORMAT_R32G32_UINT,
                                                                                                       TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT));

    fLoopOrderIndependentTransparencyZBufferBuffers[Index]:=TOrderIndependentTransparencyBuffer.Create(fWidth*fHeight*fCountLoopOrderIndependentTransparencyLayers*fCountSurfaceViews*(SizeOf(UInt32)*1),
                                                                                                       VK_FORMAT_R32_UINT,
                                                                                                       TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT));

    if fVulkanSampleCountFlagBits<>TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
     fLoopOrderIndependentTransparencySBufferBuffers[Index]:=TOrderIndependentTransparencyBuffer.Create(fWidth*fHeight*fCountLoopOrderIndependentTransparencyLayers*fCountSurfaceViews*(SizeOf(UInt32)*1),
                                                                                                        VK_FORMAT_R32_UINT,
                                                                                                        TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT));
    end else begin
     fLoopOrderIndependentTransparencySBufferBuffers[Index]:=nil;
    end;

   end;

  end;

  TTransparencyMode.MBOIT,
  TTransparencyMode.WBOIT:begin

   fApproximationOrderIndependentTransparentUniformBuffer.ZNearZFar.x:=abs(fZNear);
   fApproximationOrderIndependentTransparentUniformBuffer.ZNearZFar.y:=IfThen(IsInfinite(fZFar),4096.0,abs(fZFar));
   fApproximationOrderIndependentTransparentUniformBuffer.ZNearZFar.z:=ln(fApproximationOrderIndependentTransparentUniformBuffer.ZNearZFar.x);
   fApproximationOrderIndependentTransparentUniformBuffer.ZNearZFar.w:=ln(fApproximationOrderIndependentTransparentUniformBuffer.ZNearZFar.y);

   fApproximationOrderIndependentTransparentUniformVulkanBuffer.UploadData(pvApplication.VulkanDevice.TransferQueue,
                                                                           fVulkanTransferCommandBuffer,
                                                                           fVulkanTransferCommandBufferFence,
                                                                           fApproximationOrderIndependentTransparentUniformBuffer,
                                                                           0,
                                                                           SizeOf(TApproximationOrderIndependentTransparentUniformBuffer));

  end;

  else begin
  end;

 end;

 fFrameGraph.AfterCreateSwapChain;

 pvApplication.SkipNextDrawFrame:=true;

end;

procedure TScreenMain.BeforeDestroySwapChain;
var Index:TpvSizeInt;
begin
 fFrameGraph.BeforeDestroySwapChain;
 if assigned(UnitApplication.Application.VirtualReality) then begin
  fExternalOutputImageData.VulkanImages.Clear;
 end;
 for Index:=0 to fFrameGraph.CountInFlightFrames-1 do begin
  FreeAndNil(fDepthMipmappedArray2DImages[Index]);
  FreeAndNil(fForwardMipmappedArray2DImages[Index]);
 end;
 case fTransparencyMode of
  TTransparencyMode.SPINLOCKOIT,
  TTransparencyMode.INTERLOCKOIT:begin
   for Index:=0 to fCountInFlightFrames-1 do begin
    FreeAndNil(fLockOrderIndependentTransparencyABufferBuffers[Index]);
    FreeAndNil(fLockOrderIndependentTransparencyAuxImages[Index]);
    if fTransparencyMode=TTransparencyMode.SPINLOCKOIT then begin
     FreeAndNil(fLockOrderIndependentTransparencySpinLockImages[Index]);
    end;
   end;
  end;
  TTransparencyMode.LOOPOIT:begin
   for Index:=0 to fCountInFlightFrames-1 do begin
    FreeAndNil(fLoopOrderIndependentTransparencyABufferBuffers[Index]);
    FreeAndNil(fLoopOrderIndependentTransparencyZBufferBuffers[Index]);
    FreeAndNil(fLoopOrderIndependentTransparencySBufferBuffers[Index]);
   end;
  end;
  else begin
  end;
 end;
 inherited BeforeDestroySwapChain;
end;

function TScreenMain.CanBeParallelProcessed:boolean;
begin
 result:=false;
end;

procedure TScreenMain.CalculateCascadedShadowMaps(const aInFlightFrameIndex:Int32;const aViewLeft,aViewRight:TpvScene3D.TView);
{$undef UseSphereBasedCascadedShadowMaps}
const FrustumCorners:array[0..7] of TpvVector3=
       (
        (x:-1.0;y:-1.0;z:0.0),
        (x:1.0;y:-1.0;z:0.0),
        (x:-1.0;y:1.0;z:0.0),
        (x:1.0;y:1.0;z:0.0),
        (x:-1.0;y:-1.0;z:1.0),
        (x:1.0;y:-1.0;z:1.0),
        (x:-1.0;y:1.0;z:1.0),
        (x:1.0;y:1.0;z:1.0)
       );
var CascadedShadowMapIndex,Index:TpvSizeInt;
    CascadedShadowMaps:TScreenMain.PCascadedShadowMaps;
    CascadedShadowMap:TScreenMain.PCascadedShadowMap;
    SceneWorldSpaceBoundingBox,
    SceneLightSpaceBoundingBox,
    LightSpaceAABB:TpvAABB;
    SceneWorldSpaceSphere,
    LightSpaceSphere:TpvSphere;
    SceneClipWorldSpaceSphere:TpvSphere;
    LightForwardVector,LightSideVector,
    LightUpVector,LightSpaceCorner:TpvVector3;
{$ifdef UseSphereBasedCascadedShadowMaps}
    {SplitCenter,SplitBounds,}SplitOffset,SplitScale:TpvVector3;
    Offset,Step:TpvVector2;
{$else}
    UnitsPerTexel:TpvVector2;
    ShadowOrigin,RoundedOrigin,RoundOffset:TpvVector2;
{$endif}
    ProjectionMatrix,
    LightViewMatrix,
    LightProjectionMatrix,
    LightViewProjectionMatrix,
    FromViewSpaceToLightSpaceMatrixLeft,
    FromViewSpaceToLightSpaceMatrixRight,
    InverseProjectionMatrixLeft,
    InverseProjectionMatrixRight,
    ViewMatrix:TpvMatrix4x4;
    CascadedShadowMapSplitLambda,
    CascadedShadowMapSplitOverlap,
    MinZ,MaxZ,MinZExtents,MaxZExtents,ZMargin,
    Ratio,SplitValue,UniformSplitValue,LogSplitValue,
    FadeStartValue,LastValue,Value,
{$ifdef UseSphereBasedCascadedShadowMaps}
    Border,RoundedUpLightSpaceSphereRadius,
{$endif}
    zNear,zFar,RealZNear,RealZFar:TpvScalar;
    DoNeedRefitNearFarPlanes:boolean;
    ViewSpaceFrustumCornersLeft,
    ViewSpaceFrustumCornersRight:array[0..7] of TpvVector3;
    InFlightFrameState:PInFlightFrameState;
begin

 SceneWorldSpaceBoundingBox:=fScene3D.BoundingBox;

 SceneWorldSpaceSphere:=TpvSphere.CreateFromAABB(SceneWorldSpaceBoundingBox);

 if IsInfinite(fZFar) then begin
  RealZNear:=0.1;
  RealZFar:=1.0;
  for Index:=0 to 1 do begin
   if Index=0 then begin
    ViewMatrix:=aViewLeft.ViewMatrix;
   end else begin
    ViewMatrix:=aViewRight.ViewMatrix;
   end;
   ViewMatrix:=ViewMatrix.SimpleInverse;
   if SceneWorldSpaceSphere.Contains(ViewMatrix.Translation.xyz) then begin
    if not SceneWorldSpaceSphere.RayIntersection(ViewMatrix.Translation.xyz,-ViewMatrix.Forwards.xyz,Value) then begin
     Value:=SceneWorldSpaceSphere.Radius;
    end;
   end else begin
    Value:=SceneWorldSpaceSphere.Center.DistanceTo(ViewMatrix.Translation.xyz)+SceneWorldSpaceSphere.Radius;
   end;
   RealZFar:=Max(RealZFar,Value);
  end;
  zNear:=RealZNear;
  zFar:=RealZFar;
  DoNeedRefitNearFarPlanes:=true;
 end else begin
  zNear:=abs(fZNear);
  zFar:=abs(fZFar);
  RealZNear:=zNear;
  RealZFar:=zFar;
  DoNeedRefitNearFarPlanes:=fZFar<0.0;
 end;

 CascadedShadowMapSplitLambda:=0.5;

 CascadedShadowMapSplitOverlap:=0.1;

 SceneClipWorldSpaceSphere:=TpvSphere.Create(SceneWorldSpaceSphere.Center,Max(SceneWorldSpaceSphere.Radius,RealZFar*0.5));

 ProjectionMatrix:=aViewLeft.ProjectionMatrix;
 if DoNeedRefitNearFarPlanes then begin
  ProjectionMatrix[2,2]:=RealZFar/(RealZNear-RealZFar);
  ProjectionMatrix[3,2]:=(-(RealZNear*RealZFar))/(RealZFar-RealZNear);
 end;
 InverseProjectionMatrixLeft:=ProjectionMatrix.Inverse;

 ProjectionMatrix:=aViewRight.ProjectionMatrix;
 if DoNeedRefitNearFarPlanes then begin
  ProjectionMatrix[2,2]:=RealZFar/(RealZNear-RealZFar);
  ProjectionMatrix[3,2]:=(-(RealZNear*RealZFar))/(RealZFar-RealZNear);
 end;
 InverseProjectionMatrixRight:=ProjectionMatrix.Inverse;

 LightForwardVector:=-TSkyCubeMap.LightDirection.xyz.Normalize;
 LightSideVector:=LightForwardVector.Perpendicular;
{LightSideVector:=TpvVector3.InlineableCreate(-aViewLeft.ViewMatrix.RawComponents[0,2],
                                              -aViewLeft.ViewMatrix.RawComponents[1,2],
                                              -aViewLeft.ViewMatrix.RawComponents[2,2]).Normalize;
 if abs(LightForwardVector.Dot(LightSideVector))>0.5 then begin
  if abs(LightForwardVector.Dot(TpvVector3.YAxis))<0.9 then begin
   LightSideVector:=TpvVector3.YAxis;
  end else begin
   LightSideVector:=TpvVector3.ZAxis;
  end;
 end;}
 LightUpVector:=(LightForwardVector.Cross(LightSideVector)).Normalize;
 LightSideVector:=(LightUpVector.Cross(LightForwardVector)).Normalize;
 LightViewMatrix.RawComponents[0,0]:=LightSideVector.x;
 LightViewMatrix.RawComponents[0,1]:=LightUpVector.x;
 LightViewMatrix.RawComponents[0,2]:=LightForwardVector.x;
 LightViewMatrix.RawComponents[0,3]:=0.0;
 LightViewMatrix.RawComponents[1,0]:=LightSideVector.y;
 LightViewMatrix.RawComponents[1,1]:=LightUpVector.y;
 LightViewMatrix.RawComponents[1,2]:=LightForwardVector.y;
 LightViewMatrix.RawComponents[1,3]:=0.0;
 LightViewMatrix.RawComponents[2,0]:=LightSideVector.z;
 LightViewMatrix.RawComponents[2,1]:=LightUpVector.z;
 LightViewMatrix.RawComponents[2,2]:=LightForwardVector.z;
 LightViewMatrix.RawComponents[2,3]:=0.0;
 LightViewMatrix.RawComponents[3,0]:=0.0;
 LightViewMatrix.RawComponents[3,1]:=0.0;
 LightViewMatrix.RawComponents[3,2]:=0.0;
 LightViewMatrix.RawComponents[3,3]:=1.0;

 for Index:=0 to 1 do begin
  if Index=0 then begin
   ViewMatrix:=aViewLeft.ViewMatrix;
  end else begin
   ViewMatrix:=aViewRight.ViewMatrix;
  end;
  ViewMatrix:=ViewMatrix.SimpleInverse;
  if not SceneClipWorldSpaceSphere.Contains(ViewMatrix.Translation.xyz) then begin
   ViewMatrix.Translation.xyz:=SceneClipWorldSpaceSphere.Center+((ViewMatrix.Translation.xyz-SceneClipWorldSpaceSphere.Center).Normalize*SceneClipWorldSpaceSphere.Radius);
  end;
  ViewMatrix:=ViewMatrix*LightViewMatrix;
  if Index=0 then begin
   FromViewSpaceToLightSpaceMatrixLeft:=ViewMatrix;
  end else begin
   FromViewSpaceToLightSpaceMatrixRight:=ViewMatrix;
  end;
 end;

 SceneLightSpaceBoundingBox:=SceneWorldSpaceBoundingBox.Transform(LightViewMatrix);

 MinZExtents:=SceneLightSpaceBoundingBox.Min.z-16;
 MaxZExtents:=SceneLightSpaceBoundingBox.Max.z+16;
 ZMargin:=(MaxZExtents-MinZExtents)*0.25;
 MinZExtents:=MinZExtents-ZMargin;
 MaxZExtents:=MaxZExtents+ZMargin;

 for Index:=0 to 7 do begin
  ViewSpaceFrustumCornersLeft[Index]:=InverseProjectionMatrixLeft.MulHomogen(TpvVector4.InlineableCreate(FrustumCorners[Index],1.0)).xyz;
  ViewSpaceFrustumCornersRight[Index]:=InverseProjectionMatrixRight.MulHomogen(TpvVector4.InlineableCreate(FrustumCorners[Index],1.0)).xyz;
 end;

 CascadedShadowMaps:=@fInFlightFrameCascadedShadowMaps[aInFlightFrameIndex];

 CascadedShadowMaps^[0].SplitDepths.x:=Min(zNear,RealZNear);
 Ratio:=zFar/zNear;
 LastValue:=0.0;
 for CascadedShadowMapIndex:=1 to CountCascadedShadowMapCascades-1 do begin
  SplitValue:=CascadedShadowMapIndex/CountCascadedShadowMapCascades;
  UniformSplitValue:=((1.0-SplitValue)*zNear)+(SplitValue*zFar);
  LogSplitValue:=zNear*power(Ratio,SplitValue);
  Value:=((1.0-CascadedShadowMapSplitLambda)*UniformSplitValue)+(CascadedShadowMapSplitLambda*LogSplitValue);
  FadeStartValue:=Min(Max((Value*(1.0-CascadedShadowMapSplitOverlap))+(LastValue*CascadedShadowMapSplitOverlap),Min(zNear,RealZNear)),Max(zFar,RealZFar));
  LastValue:=Value;
  CascadedShadowMaps^[CascadedShadowMapIndex].SplitDepths.x:=Min(Max(FadeStartValue,Min(zNear,RealZNear)),Max(zFar,RealZFar));
  CascadedShadowMaps^[CascadedShadowMapIndex-1].SplitDepths.y:=Min(Max(Value,Min(zNear,RealZNear)),Max(zFar,RealZFar));
 end;
 CascadedShadowMaps^[CountCascadedShadowMapCascades-1].SplitDepths.y:=Max(ZFar,RealZFar);

 for CascadedShadowMapIndex:=0 to CountCascadedShadowMapCascades-1 do begin

  CascadedShadowMap:=@CascadedShadowMaps^[CascadedShadowMapIndex];

  MinZ:=CascadedShadowMap^.SplitDepths.x;
  MaxZ:=CascadedShadowMap^.SplitDepths.y;

  for Index:=0 to 7 do begin
   case Index of
    0..3:begin
     LightSpaceCorner:=ViewSpaceFrustumCornersLeft[Index].Lerp(ViewSpaceFrustumCornersLeft[Index+4],(MinZ-RealZNear)/(RealZFar-RealZNear));
    end;
    else {4..7:}begin
     LightSpaceCorner:=ViewSpaceFrustumCornersLeft[Index-4].Lerp(ViewSpaceFrustumCornersLeft[Index],(MaxZ-RealZNear)/(RealZFar-RealZNear));
    end;
   end;
   LightSpaceCorner:=FromViewSpaceToLightSpaceMatrixLeft*LightSpaceCorner;
   if Index=0 then begin
    LightSpaceAABB.Min:=LightSpaceCorner;
    LightSpaceAABB.Max:=LightSpaceCorner;
   end else begin
    LightSpaceAABB:=LightSpaceAABB.CombineVector3(LightSpaceCorner);
   end;
  end;
  for Index:=0 to 7 do begin
   case Index of
    0..3:begin
     LightSpaceCorner:=ViewSpaceFrustumCornersRight[Index].Lerp(ViewSpaceFrustumCornersRight[Index+4],(MinZ-RealZNear)/(RealZFar-RealZNear));
    end;
    else {4..7:}begin
     LightSpaceCorner:=ViewSpaceFrustumCornersRight[Index-4].Lerp(ViewSpaceFrustumCornersRight[Index],(MaxZ-RealZNear)/(RealZFar-RealZNear));
    end;
   end;
   LightSpaceAABB:=LightSpaceAABB.CombineVector3(FromViewSpaceToLightSpaceMatrixRight*LightSpaceCorner);
  end;

  if LightSpaceAABB.Intersect(SceneLightSpaceBoundingBox) then begin
   LightSpaceAABB:=LightSpaceAABB.GetIntersection(SceneLightSpaceBoundingBox);
  end;

  //LightSpaceAABB:=SceneLightSpaceBoundingBox;

//UnitsPerTexel:=(LightSpaceAABB.Max.xy-LightSpaceAABB.Min.xy)/TpvVector2.InlineableCreate(CascadedShadowMapWidth,CascadedShadowMapHeight);

  LightSpaceSphere:=TpvSphere.CreateFromAABB(LightSpaceAABB);
  LightSpaceSphere.Radius:=ceil(LightSpaceSphere.Radius*16)/16;
  LightSpaceAABB:=LightSpaceSphere.ToAABB;

{$ifdef UseSphereBasedCascadedShadowMaps}
  LightSpaceSphere:=TpvSphere.CreateFromAABB(LightSpaceAABB);

  Border:=4;

  RoundedUpLightSpaceSphereRadius:=ceil(LightSpaceSphere.Radius);

  Step.x:=(2.0*RoundedUpLightSpaceSphereRadius)/(CascadedShadowMapWidth-(2.0*Border));
  Step.y:=(2.0*RoundedUpLightSpaceSphereRadius)/(CascadedShadowMapHeight-(2.0*Border));

  Offset.x:=floor((LightSpaceSphere.Center.x-RoundedUpLightSpaceSphereRadius)/Step.x);
  Offset.y:=floor((LightSpaceSphere.Center.y-RoundedUpLightSpaceSphereRadius)/Step.y);

{ SplitCenter.x:=(Offset.x*Step.x)+RoundedUpLightSpaceSphereRadius;
  SplitCenter.y:=(Offset.y*Step.y)+RoundedUpLightSpaceSphereRadius;
  SplitCenter.z:=-0.5*(MinZExtents+MaxZExtents);

  SplitBounds.x:=RoundedUpLightSpaceSphereRadius;
  SplitBounds.y:=RoundedUpLightSpaceSphereRadius;
  SplitBounds.z:=0.5*(MaxZExtents-MinZExtents);}

  SplitScale.x:=1.0/Step.x;
  SplitScale.y:=1.0/Step.y;
  SplitScale.z:=(-1.0)/(MaxZExtents-MinZExtents);

  SplitOffset.x:=Border-Offset.x;
  SplitOffset.y:=Border-Offset.y;
  SplitOffset.z:=(-MinZExtents)/(MaxZExtents-MinZExtents);

  LightProjectionMatrix[0,0]:=2.0*(SplitScale.x/CascadedShadowMapWidth);
  LightProjectionMatrix[0,1]:=0.0;
  LightProjectionMatrix[0,2]:=0.0;
  LightProjectionMatrix[0,3]:=0.0;
  LightProjectionMatrix[1,0]:=0.0;
  LightProjectionMatrix[1,1]:=2.0*(SplitScale.y/CascadedShadowMapHeight);
  LightProjectionMatrix[1,2]:=0.0;
  LightProjectionMatrix[1,3]:=0.0;
  LightProjectionMatrix[2,0]:=0.0;
  LightProjectionMatrix[2,1]:=0.0;
  LightProjectionMatrix[2,2]:=SplitScale.z;//2.0*SplitScale.z;
  LightProjectionMatrix[2,3]:=0.0;
  LightProjectionMatrix[3,0]:=(2.0*(SplitOffset.x/CascadedShadowMapWidth))-1.0;
  LightProjectionMatrix[3,1]:=(2.0*(SplitOffset.y/CascadedShadowMapHeight))-1.0;
  LightProjectionMatrix[3,2]:=SplitOffset.z;//(2.0*SplitOffset.z)-1.0;
  LightProjectionMatrix[3,3]:=1.0;

{$else}

{ UnitsPerTexel:=(LightSpaceAABB.Max.xy-LightSpaceAABB.Min.xy)/TpvVector2.InlineableCreate(CascadedShadowMapWidth,CascadedShadowMapHeight);

  LightSpaceAABB.Min.x:=floor(LightSpaceAABB.Min.x/UnitsPerTexel.x)*UnitsPerTexel.x;
  LightSpaceAABB.Min.y:=floor(LightSpaceAABB.Min.y/UnitsPerTexel.y)*UnitsPerTexel.y;

  LightSpaceAABB.Max.x:=ceil(LightSpaceAABB.Max.x/UnitsPerTexel.x)*UnitsPerTexel.x;
  LightSpaceAABB.Max.y:=ceil(LightSpaceAABB.Max.y/UnitsPerTexel.y)*UnitsPerTexel.y;{}

  LightProjectionMatrix:=TpvMatrix4x4.CreateOrthoRightHandedZeroToOne(LightSpaceAABB.Min.x,
                                                                      LightSpaceAABB.Max.x,
                                                                      LightSpaceAABB.Min.y,
                                                                      LightSpaceAABB.Max.y,
                                                                      MinZExtents,
                                                                      MaxZExtents);

  LightViewProjectionMatrix:=LightViewMatrix*LightProjectionMatrix;

  ShadowOrigin:=(LightViewProjectionMatrix.MulHomogen(TpvVector3.Origin)).xy*TpvVector2.InlineableCreate(CascadedShadowMapWidth*0.5,CascadedShadowMapHeight*0.5);
  RoundedOrigin.x:=round(ShadowOrigin.x);
  RoundedOrigin.y:=round(ShadowOrigin.y);
  RoundOffset:=(RoundedOrigin-ShadowOrigin)*TpvVector2.InlineableCreate(2.0/CascadedShadowMapWidth,2.0/CascadedShadowMapHeight);
  LightProjectionMatrix[3,0]:=LightProjectionMatrix[3,0]+RoundOffset.x;
  LightProjectionMatrix[3,1]:=LightProjectionMatrix[3,1]+RoundOffset.y;

{$endif}

  LightViewProjectionMatrix:=LightViewMatrix*LightProjectionMatrix;

  CascadedShadowMap.View.ViewMatrix:=LightViewMatrix;
  CascadedShadowMap.View.ProjectionMatrix:=LightProjectionMatrix;
  CascadedShadowMap.View.InverseViewMatrix:=LightViewMatrix.Inverse;
  CascadedShadowMap.View.InverseProjectionMatrix:=LightProjectionMatrix.Inverse;
  CascadedShadowMap.CombinedMatrix:=LightViewProjectionMatrix;

  fCascadedShadowMapUniformBuffers[aInFlightFrameIndex].Matrices[CascadedShadowMapIndex]:=LightViewProjectionMatrix;
  fCascadedShadowMapUniformBuffers[aInFlightFrameIndex].SplitDepths[CascadedShadowMapIndex]:=TpvVector4.Create(CascadedShadowMap^.SplitDepths,0.0);

 end;

 InFlightFrameState:=@fInFlightFrameStates[aInFlightFrameIndex];

 InFlightFrameState^.CascadedShadowMapViewIndex:=fScene3D.AddView(CascadedShadowMaps^[0].View);
 for CascadedShadowMapIndex:=1 to CountCascadedShadowMapCascades-1 do begin
  fScene3D.AddView(CascadedShadowMaps^[CascadedShadowMapIndex].View);
 end;

 InFlightFrameState^.CountCascadedShadowMapViews:=CountCascadedShadowMapCascades;

end;

procedure TScreenMain.Update(const aDeltaTime:TpvDouble);
const Directions:array[boolean,boolean] of TpvScalar=
       (
        (0,1),
        (-1,0)
       );
var RotationSpeed,MovementSpeed:TpvDouble;
    FPS:TpvInt32;
    FPSString:string;
begin

 RotationSpeed:=aDeltaTime*1.0;
 MovementSpeed:=aDeltaTime*1.0*fCameraSpeed;

 if fKeyPitchInc or fKeyPitchDec or fKeyYawInc or fKeyYawDec or fKeyRollInc or fKeyRollDec then begin
  fCameraMatrix:=(TpvMatrix4x4.CreateFromQuaternion(TpvQuaternion.CreateFromEuler(TpvVector3.Create(Directions[fKeyPitchInc,fKeyPitchDec],
                                                                                                    Directions[fKeyYawDec,fKeyYawInc],
                                                                                                    Directions[fKeyRollInc,fKeyRollDec])*-RotationSpeed).Normalize)*fCameraMatrix).OrthoNormalize;
 end;
 if fKeyLeft or fKeyRight or fKeyForwards or fKeyBackwards or fKeyUp or fKeyDown then begin
  fCameraMatrix:=fCameraMatrix*
                 TpvMatrix4x4.CreateTranslation((fCameraMatrix.ToMatrix3x3*
                                                 TpvVector3.InlineableCreate(Directions[fKeyLeft,fKeyRight],
                                                                             Directions[fKeyDown,fKeyUp],
                                                                             Directions[fKeyForwards,fKeyBackwards]))*MovementSpeed);
 end;

 inherited Update(aDeltaTime);

 fFrameGraph.Update(pvApplication.UpdateInFlightFrameIndex,pvApplication.UpdateFrameCounter);

 FPS:=round(pvApplication.FramesPerSecond*100.0);
 fFPSTimeAccumulator:=fFPSTimeAccumulator+aDeltaTime;
 if fFPSTimeAccumulator>=0.25 then begin
  fFPSTimeAccumulator:=frac(fFPSTimeAccumulator*4)*0.25;
  fOldFPS:=Low(Int32);
 end;
 if abs(fOldFPS-FPS)>=100 then begin
  fOldFPS:=FPS;
  str((FPS*0.01):4:2,FPSString);
  pvApplication.WindowTitle:=pvApplication.Title+' ['+FPSString+' FPS]';
 end;

//DrawUpdate(pvApplication.UpdateInFlightFrameIndex,pvApplication.DeltaTime);

end;

function TScreenMain.IsReadyForDrawOfInFlightFrameIndex(const aInFlightFrameIndex:TpvInt32):boolean;
begin
 result:=TPasMPInterlocked.Read(fInFlightFrameStates[aInFlightFrameIndex].Ready);
end;

procedure TScreenMain.DrawUpdate(const aInFlightFrameIndex:TpvInt32;const aDeltaTime:TpvDouble);
var Index:TpvSizeInt;
    ModelMatrix,ViewMatrix:TpvMatrix4x4;
    Center,Bounds:TpvVector3;
    t0,t1:Double;
    ViewLeft,ViewRight:TpvScene3D.TView;
    InFlightFrameState:PInFlightFrameState;
    BlendFactor,Factor:single;
begin

 InFlightFrameState:=@fInFlightFrameStates[aInFlightFrameIndex];

 begin

  fUpdateLock.Acquire;
  try

   ModelMatrix:=TpvMatrix4x4.Identity; // TpvMatrix4x4.CreateRotate(State^.AnglePhases[0]*TwoPI,TpvVector3.Create(0.0,0.0,1.0))*TpvMatrix4x4.CreateRotate(State^.AnglePhases[1]*TwoPI,TpvVector3.Create(0.0,1.0,0.0));

   fGroupInstance.ModelMatrix:=ModelMatrix;

   begin
    BlendFactor:=1.0-exp(-(pvApplication.DeltaTime*4.0));
    for Index:=-1 to fGroupInstance.Group.Animations.Count-1 do begin
     Factor:=fGroupInstance.Automations[Index].Factor;
     if Index=fAnimationIndex then begin
      if Factor<0.0 then begin
       Factor:=0.0;
       fGroupInstance.Automations[Index].ShadowTime:=0.0;
      end;
      Factor:=(Factor*(1.0-BlendFactor))+(1.0*BlendFactor);
     end else if Factor>0.0 then begin
      Factor:=Factor*(1.0-BlendFactor);
      if Factor<1e-5 then begin
       Factor:=-1.0;
      end;
     end;
     if Factor>0.0 then begin
      if Index>=0 then begin
       t0:=fGroupInstance.Group.Animations[Index].GetAnimationBeginTime;
       t1:=fGroupInstance.Group.Animations[Index].GetAnimationEndTime;
       fGroupInstance.Automations[Index].Time:=fGroupInstance.Automations[Index].ShadowTime+t0;
       fGroupInstance.Automations[Index].ShadowTime:=ModuloPos(fGroupInstance.Automations[Index].ShadowTime+pvApplication.DeltaTime,t1-t0);
       fGroupInstance.Automations[Index].Complete:=true;
      end else begin
       fGroupInstance.Automations[Index].Time:=0.0;
       fGroupInstance.Automations[Index].Complete:=false;
      end;
     end else begin
      fGroupInstance.Automations[Index].Time:=0.0;
     end;
     fGroupInstance.Automations[Index].Factor:=Factor;
    end;
   end;

   fScene3D.Update(aInFlightFrameIndex);

   fScene3D.TransferViewsToPreviousViews;

   fScene3D.ClearViews;

   Center:=(fScene3D.BoundingBox.Min+fScene3D.BoundingBox.Max)*0.5;

   Bounds:=(fScene3D.BoundingBox.Max-fScene3D.BoundingBox.Min)*0.5;

   case fCameraMode of
    TCameraMode.FirstPerson:begin
     ViewMatrix:=fCameraMatrix.SimpleInverse;//TpvMatrix4x4.CreateTranslation(-fCameraPosition)*TpvMatrix4x4.CreateFromQuaternion(fCameraOrientation);
    end;
    else begin
     ViewMatrix:=TpvMatrix4x4.CreateLookAt(Center+(TpvVector3.Create(sin(fCameraRotationX*PI*2.0)*cos(-fCameraRotationY*PI*2.0),
                                                                     sin(-fCameraRotationY*PI*2.0),
                                                                     cos(fCameraRotationX*PI*2.0)*cos(-fCameraRotationY*PI*2.0)).Normalize*
                                                           (Max(Max(Bounds[0],Bounds[1]),Bounds[2])*2.0*fZoom)),
                                           Center,
                                           TpvVector3.Create(0.0,1.0,0.0));//*TpvMatrix4x4.FlipYClipSpace;
     fCameraMatrix:=ViewMatrix.SimpleInverse;
    end;
   end;

   if assigned(UnitApplication.Application.VirtualReality) then begin

    ViewLeft.ViewMatrix:=ViewMatrix*UnitApplication.Application.VirtualReality.GetPositionMatrix(0);
    ViewLeft.ProjectionMatrix:=UnitApplication.Application.VirtualReality.GetProjectionMatrix(0);
    ViewLeft.InverseViewMatrix:=ViewLeft.ViewMatrix.Inverse;
    ViewLeft.InverseProjectionMatrix:=ViewLeft.ProjectionMatrix.Inverse;

    ViewRight.ViewMatrix:=ViewMatrix*UnitApplication.Application.VirtualReality.GetPositionMatrix(1);
    ViewRight.ProjectionMatrix:=UnitApplication.Application.VirtualReality.GetProjectionMatrix(1);
    ViewRight.InverseViewMatrix:=ViewRight.ViewMatrix.Inverse;
    ViewRight.InverseProjectionMatrix:=ViewRight.ProjectionMatrix.Inverse;

   end else begin

    ViewLeft.ViewMatrix:=ViewMatrix;

    if fZFar>0.0 then begin
     ViewLeft.ProjectionMatrix:=TpvMatrix4x4.CreatePerspectiveRightHandedZeroToOne(fFOV,
                                                                                   fWidth/fHeight,
                                                                                   abs(fZNear),
                                                                                   IfThen(IsInfinite(fZFar),1024.0,abs(fZFar)));
    end else begin
     ViewLeft.ProjectionMatrix:=TpvMatrix4x4.CreatePerspectiveRightHandedOneToZero(fFOV,
                                                                                   fWidth/fHeight,
                                                                                   abs(fZNear),
                                                                                   IfThen(IsInfinite(fZFar),1024.0,abs(fZFar)));
    end;
    if fZFar<0.0 then begin
     if IsInfinite(fZFar) then begin
      // Convert to reversed infinite Z
      ViewLeft.ProjectionMatrix.RawComponents[2,2]:=0.0;
      ViewLeft.ProjectionMatrix.RawComponents[2,3]:=-1.0;
      ViewLeft.ProjectionMatrix.RawComponents[3,2]:=abs(fZNear);
     end else begin
      // Convert to reversed non-infinite Z
      ViewLeft.ProjectionMatrix.RawComponents[2,2]:=abs(fZNear)/(abs(fZFar)-abs(fZNear));
      ViewLeft.ProjectionMatrix.RawComponents[2,3]:=-1.0;
      ViewLeft.ProjectionMatrix.RawComponents[3,2]:=(abs(fZNear)*abs(fZFar))/(abs(fZFar)-abs(fZNear));
     end;
    end;
    ViewLeft.ProjectionMatrix:=ViewLeft.ProjectionMatrix*TpvMatrix4x4.FlipYClipSpace;
    ViewLeft.InverseViewMatrix:=ViewLeft.ViewMatrix.Inverse;
    ViewLeft.InverseProjectionMatrix:=ViewLeft.ProjectionMatrix.Inverse;

    ViewRight.ViewMatrix:=ViewLeft.ViewMatrix;
    ViewRight.ProjectionMatrix:=ViewLeft.ProjectionMatrix;
    ViewRight.InverseViewMatrix:=ViewLeft.InverseViewMatrix;
    ViewRight.InverseProjectionMatrix:=ViewLeft.InverseProjectionMatrix;

   end;

   InFlightFrameState^.FinalViewIndex:=fScene3D.AddViews([ViewLeft,ViewRight]);

   InFlightFrameState^.CountViews:=2;

   CalculateCascadedShadowMaps(aInFlightFrameIndex,
                               ViewLeft,
                               ViewRight);

   fCascadedShadowMapVulkanUniformBuffers[aInFlightFrameIndex].UpdateData(fCascadedShadowMapUniformBuffers[aInFlightFrameIndex],
                                                                          0,
                                                                          SizeOf(TCascadedShadowMapUniformBuffer));

   fScene3D.UpdateViews(aInFlightFrameIndex);

   // Main viewport(s)
   fScene3D.Prepare(aInFlightFrameIndex,
                    0,
                    InFlightFrameState^.FinalViewIndex,
                    InFlightFrameState^.CountViews,
                    fWidth,
                    fHeight,
                    true,
                    true);

   // Cascaded shadow map viewport(s)
   fScene3D.Prepare(aInFlightFrameIndex,
                    1,
                    InFlightFrameState^.CascadedShadowMapViewIndex,
                    InFlightFrameState^.CountCascadedShadowMapViews,
                    CascadedShadowMapWidth,
                    CascadedShadowMapHeight,
                    false,
                    true);

   TPasMPInterlocked.Write(InFlightFrameState^.Ready,true);

   fTime:=fTime+pvApplication.DeltaTime;

  finally
   fUpdateLock.Release;
  end;

 end;

end;

procedure TScreenMain.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
var InFlightFrameState:TScreenMain.PInFlightFrameState;
    InFlightFrameIndex:Int32;
begin

 inherited Draw(aSwapChainImageIndex,aWaitSemaphore,nil);

 InFlightFrameIndex:=pvApplication.DrawInFlightFrameIndex;

 InFlightFrameState:=@fInFlightFrameStates[InFlightFrameIndex];

 DrawUpdate(InFlightFrameIndex,pvApplication.DeltaTime);

 if fScene3D.NeedFlush(pvApplication.DrawInFlightFrameIndex) then begin
  fVulkanFlushCommandBuffers[InFlightFrameIndex].Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));
  fVulkanFlushCommandBuffers[InFlightFrameIndex].BeginRecording;
  fScene3D.Flush(InFlightFrameIndex,fVulkanFlushCommandBuffers[InFlightFrameIndex]);
  fVulkanFlushCommandBuffers[InFlightFrameIndex].EndRecording;
  fVulkanFlushCommandBuffers[InFlightFrameIndex].Execute(fVulkanFlushQueue,
                                                         TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or pvApplication.VulkanDevice.PhysicalDevice.PipelineStageAllShaderBits,
                                                         aWaitSemaphore,
                                                         fVulkanFlushSemaphores[InFlightFrameIndex],
                                                         nil,
                                                         false);
  aWaitSemaphore:=fVulkanFlushSemaphores[InFlightFrameIndex];
 end;

 fFrameGraph.Draw(pvApplication.SwapChainImageIndex,
                  pvApplication.DrawInFlightFrameIndex,
                  pvApplication.DrawFrameCounter,
                  aWaitSemaphore,
                  fVulkanRenderSemaphores[InFlightFrameIndex],
                  aWaitFence);

 TPasMPInterlocked.Write(InFlightFrameState^.Ready,false);

 aWaitSemaphore:=fVulkanRenderSemaphores[InFlightFrameIndex];

end;

function TScreenMain.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=inherited KeyEvent(aKeyEvent);
 if aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down then begin
  case aKeyEvent.KeyCode of
   KEYCODE_ESCAPE:begin
    pvApplication.Terminate;
   end;
   KEYCODE_U:begin
    fCameraSpeed:=fCameraSpeed*0.5;
   end;
   KEYCODE_I:begin
    fCameraSpeed:=fCameraSpeed*2.0;
   end;
   KEYCODE_O:begin
    fCameraMode:=TCameraMode.Orbit;
   end;
   KEYCODE_P:begin
    fCameraMode:=TCameraMode.FirstPerson;
   end;
   KEYCODE_L:begin
    pvApplication.CatchMouse:=not pvApplication.CatchMouse;
   end;
   KEYCODE_V,KEYCODE_B:begin
    if fAnimationIndex<0 then begin
     fAnimationIndex:=fGroupInstance.Group.Animations.Count-1;
    end else begin
     dec(fAnimationIndex);
    end;
   end;
   KEYCODE_N,KEYCODE_M:begin
    inc(fAnimationIndex);
    if fAnimationIndex>=fGroupInstance.Group.Animations.Count then begin
     fAnimationIndex:=-1;
    end;
   end;
   KEYCODE_0..KEYCODE_9:begin
    if ((aKeyEvent.KeyCode-(KEYCODE_0+1))>=-1) and ((aKeyEvent.KeyCode-(KEYCODE_0+1))<fGroupInstance.Group.Animations.Count) then begin
     fAnimationIndex:=aKeyEvent.KeyCode-(KEYCODE_0+1);
    end;
   end;
   KEYCODE_BACKSPACE:begin
    if abs(PpvVector3(pointer(@fCameraMatrix.RawComponents[2,0]))^.y*PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^.y)<0.5 then begin
     PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^.x:=0.0;
     PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^.y:=1.0;
     PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^.z:=0.0;
    end else begin
     PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^:=PpvVector3(pointer(@fCameraMatrix.RawComponents[2,0]))^.Cross(PpvVector3(pointer(@fCameraMatrix.RawComponents[0,0]))^).Normalize;
    end;
    PpvVector3(pointer(@fCameraMatrix.RawComponents[0,0]))^:=PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^.Cross(PpvVector3(pointer(@fCameraMatrix.RawComponents[2,0]))^).Normalize;
    PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^:=PpvVector3(pointer(@fCameraMatrix.RawComponents[2,0]))^.Cross(PpvVector3(pointer(@fCameraMatrix.RawComponents[0,0]))^).Normalize;
    PpvVector3(pointer(@fCameraMatrix.RawComponents[2,0]))^:=PpvVector3(pointer(@fCameraMatrix.RawComponents[0,0]))^.Cross(PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^).Normalize;
    fCameraMatrix:=fCameraMatrix.RobustOrthoNormalize;
   end;
  end;
 end;
 if aKeyEvent.KeyEventType in [TpvApplicationInputKeyEventType.Down,TpvApplicationInputKeyEventType.Up] then begin
  case aKeyEvent.KeyCode of
   KEYCODE_LEFT,KEYCODE_A:begin
    fKeyLeft:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_RIGHT,KEYCODE_S:begin
    fKeyRight:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_UP,KEYCODE_W:begin
    fKeyForwards:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_DOWN,KEYCODE_D:begin
    fKeyBackwards:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_PAGEUP,KEYCODE_R:begin
    fKeyUp:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_PAGEDOWN,KEYCODE_F:begin
    fKeyDown:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_T:begin
    fKeyPitchInc:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_G:begin
    fKeyPitchDec:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_E:begin
    fKeyYawInc:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_Q:begin
    fKeyYawDec:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_X:begin
    fKeyRollInc:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_C:begin
    fKeyRollDec:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
  end;
 end;
end;

function TScreenMain.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
begin
 result:=inherited PointerEvent(aPointerEvent);
 if not result then begin
  if (aPointerEvent.PointerEventType=TpvApplicationInputPointerEventType.Motion) and
     (pvApplication.CatchMouse or (TpvApplicationInputPointerButton.Left in aPointerEvent.Buttons)) then begin
   fUpdateLock.Acquire;
   try
    case fCameraMode of
     TCameraMode.FirstPerson:begin
      fCameraMatrix:=TpvMatrix4x4.CreateFromQuaternion(TpvQuaternion.CreateFromEuler(TpvVector3.InlineableCreate(aPointerEvent.RelativePosition.y,aPointerEvent.RelativePosition.x,0.0)*0.002)).Transpose*fCameraMatrix;
      if abs(PpvVector3(pointer(@fCameraMatrix.RawComponents[2,0]))^.y*PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^.y)<0.5 then begin
       PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^.x:=0.0;
       PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^.y:=1.0;
       PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^.z:=0.0;
      end else begin
       PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^:=PpvVector3(pointer(@fCameraMatrix.RawComponents[2,0]))^.Cross(PpvVector3(pointer(@fCameraMatrix.RawComponents[0,0]))^).Normalize;
      end;
      PpvVector3(pointer(@fCameraMatrix.RawComponents[0,0]))^:=PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^.Cross(PpvVector3(pointer(@fCameraMatrix.RawComponents[2,0]))^).Normalize;
      PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^:=PpvVector3(pointer(@fCameraMatrix.RawComponents[2,0]))^.Cross(PpvVector3(pointer(@fCameraMatrix.RawComponents[0,0]))^).Normalize;
      PpvVector3(pointer(@fCameraMatrix.RawComponents[2,0]))^:=PpvVector3(pointer(@fCameraMatrix.RawComponents[0,0]))^.Cross(PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^).Normalize;
      fCameraMatrix:=fCameraMatrix.RobustOrthoNormalize;
     end;
     else begin
      fCameraRotationX:=frac(fCameraRotationX+(1.0-(aPointerEvent.RelativePosition.x*(1.0/pvApplication.VulkanSwapChain.Width))));
      fCameraRotationY:=frac(fCameraRotationY+(1.0-(aPointerEvent.RelativePosition.y*(1.0/pvApplication.VulkanSwapChain.Height))));
     end;
    end;
   finally
    fUpdateLock.Release;
   end;
   result:=true;
  end;
 end;
end;

function TScreenMain.Scrolled(const aRelativeAmount:TpvVector2):boolean;
begin
 result:=inherited Scrolled(aRelativeAmount);
 if not result then begin
  fUpdateLock.Acquire;
  try
   case fCameraMode of
    TCameraMode.FirstPerson:begin
     fCameraMatrix:=fCameraMatrix*TpvMatrix4x4.CreateTranslation((fCameraMatrix.ToMatrix3x3*TpvVector3.ZAxis).Normalize*(aRelativeAmount.x+aRelativeAmount.y)*fCameraSpeed);
    end;
    else begin
     fZoom:=Max(1e-4,fZoom+((aRelativeAmount.x+aRelativeAmount.y)*0.1));
    end;
   end;
  finally
   fUpdateLock.Release;
  end;
  result:=true;
 end;
end;

end.
