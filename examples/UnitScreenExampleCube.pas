unit UnitScreenExampleCube;
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
     UnitRegisteredExamplesList,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application;

type PScreenExampleCubeUniformBuffer=^TScreenExampleCubeUniformBuffer;
     TScreenExampleCubeUniformBuffer=record
      ModelViewProjectionMatrix:TpvMatrix4x4;
      ModelViewNormalMatrix:TpvMatrix4x4; // actually TpvMatrix3x3, but it would have then a TMatrix3x4 alignment, according to https://www.khronos.org/registry/vulkan/specs/1.0/html/vkspec.html#interfaces-resources-layout
     end;

     PScreenExampleCubeState=^TScreenExampleCubeState;
     TScreenExampleCubeState=record
      Time:TpvDouble;
      AnglePhases:array[0..1] of TpvFloat;
     end;

     PScreenExampleCubeStates=^TScreenExampleCubeStates;
     TScreenExampleCubeStates=array[0..MaxSwapChainImages-1] of TScreenExampleCubeState;

     TScreenExampleCube=class(TpvApplicationScreen)
      private
       fCubeVertexShaderModule:TpvVulkanShaderModule;
       fCubeFragmentShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStageCubeVertex:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageCubeFragment:TpvVulkanPipelineShaderStage;
       fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
       fVulkanRenderPass:TpvVulkanRenderPass;
       fVulkanVertexBuffer:TpvVulkanBuffer;
       fVulkanIndexBuffer:TpvVulkanBuffer;
       fVulkanUniformBuffers:array[0..MaxSwapChainImages-1] of TpvVulkanBuffer;
       fVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fVulkanDescriptorSets:array[0..MaxSwapChainImages-1] of TpvVulkanDescriptorSet;
       fVulkanPipelineLayout:TpvVulkanPipelineLayout;
       fVulkanCommandPool:TpvVulkanCommandPool;
       fVulkanRenderCommandBuffers:array[0..MaxSwapChainImages-1] of TpvVulkanCommandBuffer;
       fVulkanRenderSemaphores:array[0..MaxSwapChainImages-1] of TpvVulkanSemaphore;
       fUniformBuffer:TScreenExampleCubeUniformBuffer;
       fBoxAlbedoTexture:TpvVulkanTexture;
       fReady:boolean;
       fSelectedIndex:TpvInt32;
       fStartY:TpvFloat;
       fState:TScreenExampleCubeState;
       fStates:TScreenExampleCubeStates;
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

       function KeyDown(const aKeyCode:TpvInt32;const aKeyModifiers:TpvApplicationInputKeyModifiers):boolean; override;

       function KeyUp(const aKeyCode:TpvInt32;const aKeyModifiers:TpvApplicationInputKeyModifiers):boolean; override;

       function KeyTyped(const aKeyCode:TpvInt32;const aKeyModifiers:TpvApplicationInputKeyModifiers):boolean; override;

       function PointerDown(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID:TpvInt32;const aButton:TpvApplicationInputPointerButton;const aButtons:TpvApplicationInputPointerButtons;const aKeyModifiers:TpvApplicationInputKeyModifiers):boolean; override;

       function PointerUp(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID:TpvInt32;const aButton:TpvApplicationInputPointerButton;const aButtons:TpvApplicationInputPointerButtons;const aKeyModifiers:TpvApplicationInputKeyModifiers):boolean; override;

       function PointerMotion(const aPosition,aRelativePosition:TpvVector2;const aPressure:TpvFloat;const aPointerID:TpvInt32;const aButtons:TpvApplicationInputPointerButtons;const aKeyModifiers:TpvApplicationInputKeyModifiers):boolean; override;

       function Scrolled(const aRelativeAmount:TpvVector2):boolean; override;

       function CanBeParallelProcessed:boolean; override;

       procedure Update(const aDeltaTime:TpvDouble); override;

       procedure Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil); override;

     end;

implementation

uses UnitExampleApplication,UnitTextOverlay,UnitScreenMainMenu;

type PVertex=^TVertex;
     TVertex=record
      Position:TpvVector3;
      Tangent:TpvVector3;
      Bitangent:TpvVector3;
      Normal:TpvVector3;
      TexCoord:TpvVector2;
     end;

const CubeVertices:array[0..23] of TVertex=
       (// Left
        (Position:(x:-1;y:-1;z:-1;);Tangent:(x:0;y:0;z:1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:-1;y:0;z:0;);TexCoord:(u:0;v:0)),
        (Position:(x:-1;y: 1;z:-1;);Tangent:(x:0;y:0;z:1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:-1;y:0;z:0;);TexCoord:(u:0;v:1)),
        (Position:(x:-1;y: 1;z: 1;);Tangent:(x:0;y:0;z:1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:-1;y:0;z:0;);TexCoord:(u:1;v:1)),
        (Position:(x:-1;y:-1;z: 1;);Tangent:(x:0;y:0;z:1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:-1;y:0;z:0;);TexCoord:(u:1;v:0)),

        // Right
        (Position:(x: 1;y:-1;z: 1;);Tangent:(x:0;y:0;z:-1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:1;y:0;z:0;);TexCoord:(u:0;v:0)),
        (Position:(x: 1;y: 1;z: 1;);Tangent:(x:0;y:0;z:-1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:1;y:0;z:0;);TexCoord:(u:0;v:1)),
        (Position:(x: 1;y: 1;z:-1;);Tangent:(x:0;y:0;z:-1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:1;y:0;z:0;);TexCoord:(u:1;v:1)),
        (Position:(x: 1;y:-1;z:-1;);Tangent:(x:0;y:0;z:-1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:1;y:0;z:0;);TexCoord:(u:1;v:0)),

        // Bottom
        (Position:(x:-1;y:-1;z:-1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:1;);Normal:(x:0;y:-1;z:0;);TexCoord:(u:0;v:0)),
        (Position:(x:-1;y:-1;z: 1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:1;);Normal:(x:0;y:-1;z:0;);TexCoord:(u:0;v:1)),
        (Position:(x: 1;y:-1;z: 1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:1;);Normal:(x:0;y:-1;z:0;);TexCoord:(u:1;v:1)),
        (Position:(x: 1;y:-1;z:-1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:1;);Normal:(x:0;y:-1;z:0;);TexCoord:(u:1;v:0)),

        // Top
        (Position:(x:-1;y: 1;z:-1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:-1;);Normal:(x:0;y:1;z:0;);TexCoord:(u:0;v:0)),
        (Position:(x: 1;y: 1;z:-1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:-1;);Normal:(x:0;y:1;z:0;);TexCoord:(u:0;v:1)),
        (Position:(x: 1;y: 1;z: 1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:-1;);Normal:(x:0;y:1;z:0;);TexCoord:(u:1;v:1)),
        (Position:(x:-1;y: 1;z: 1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:-1;);Normal:(x:0;y:1;z:0;);TexCoord:(u:1;v:0)),

        // Back
        (Position:(x: 1;y:-1;z:-1;);Tangent:(x:-1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:-1;);TexCoord:(u:0;v:0)),
        (Position:(x: 1;y: 1;z:-1;);Tangent:(x:-1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:-1;);TexCoord:(u:0;v:1)),
        (Position:(x:-1;y: 1;z:-1;);Tangent:(x:-1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:-1;);TexCoord:(u:1;v:1)),
        (Position:(x:-1;y:-1;z:-1;);Tangent:(x:-1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:-1;);TexCoord:(u:1;v:0)),

        // Front
        (Position:(x:-1;y:-1;z:1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:1;);TexCoord:(u:0;v:0)),
        (Position:(x:-1;y: 1;z:1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:1;);TexCoord:(u:0;v:1)),
        (Position:(x: 1;y: 1;z:1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:1;);TexCoord:(u:1;v:1)),
        (Position:(x: 1;y:-1;z:1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:1;);TexCoord:(u:1;v:0))

       );

      CubeIndices:array[0..35] of TpvInt32=
       ( 0, 1, 2,
         0, 2, 3,

         // Right
         4, 5, 6,
         4, 6, 7,

         // Bottom
         8, 9, 10,
         8, 10, 11,

         // Top
         12, 13, 14,
         12, 14, 15,

         // Back
         16, 17, 18,
         16, 18, 19,

         // Front
         20, 21, 22,
         20, 22, 23);

      Offsets:array[0..0] of TVkDeviceSize=(0);

      FontSize=3.0;

constructor TScreenExampleCube.Create;
begin
 inherited Create;
 fSelectedIndex:=-1;
 FillChar(fState,SizeOf(TScreenExampleCubeState),#0);
 FillChar(fStates,SizeOf(TScreenExampleCubeStates),#0);
 fReady:=false;
end;

destructor TScreenExampleCube.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenExampleCube.Show;
var Stream:TStream;
    Index:TpvInt32;
begin
 inherited Show;

 fVulkanCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                 pvApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                                 TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
 for Index:=0 to MaxSwapChainImages-1 do begin
  fVulkanRenderCommandBuffers[Index]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  fVulkanRenderSemaphores[Index]:=TpvVulkanSemaphore.Create(pvApplication.VulkanDevice);
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/cube/cube_vert.spv');
 try
  fCubeVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/cube/cube_frag.spv');
 try
  fCubeFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fBoxAlbedoTexture:=TpvVulkanTexture.CreateDefault(pvApplication.VulkanDevice,
                                                   pvApplication.VulkanDevice.GraphicsQueue,
                                                   pvApplication.VulkanGraphicsCommandBuffers[0,0],
                                                   pvApplication.VulkanGraphicsCommandBufferFences[0,0],
                                                   pvApplication.VulkanDevice.TransferQueue,
                                                   pvApplication.VulkanTransferCommandBuffers[0,0],
                                                   pvApplication.VulkanTransferCommandBufferFences[0,0],
                                                   vtdtCheckerboard,
                                                   512,
                                                   512,
                                                   0,
                                                   0,
                                                   1,
                                                   true,
                                                   true);{}

{Stream:=pvApplication.Assets.GetAssetStream('textures/box_albedo.png');
 try
  fBoxAlbedoTexture:=TpvVulkanTexture.CreateFromPNG(pvApplication.VulkanDevice,
                                                    pvApplication.VulkanGraphicsCommandBuffers[0,0],
                                                    pvApplication.VulkanGraphicsCommandBufferFences[0,0],
                                                    pvApplication.VulkanTransferCommandBuffers[0,0],
                                                    pvApplication.VulkanTransferCommandBufferFences[0,0],
                                                    Stream,
                                                    true);
 finally
  Stream.Free;
 end;{}

{Stream:=pvApplication.Assets.GetAssetStream('textures/box_albedo.jpg');
 try
  fBoxAlbedoTexture:=TpvVulkanTexture.CreateFromJPEG(pvApplication.VulkanDevice,
                                                     pvApplication.VulkanGraphicsCommandBuffers[0,0],
                                                     pvApplication.VulkanGraphicsCommandBufferFences[0,0],
                                                     pvApplication.VulkanTransferCommandBuffers[0,0],
                                                     pvApplication.VulkanTransferCommandBufferFences[0,0],
                                                     Stream,
                                                     true);
 finally
  Stream.Free;
 end;{}

 fBoxAlbedoTexture.WrapModeU:=vtwmClampToEdge;
 fBoxAlbedoTexture.WrapModeV:=vtwmClampToEdge;
 fBoxAlbedoTexture.WrapModeW:=vtwmClampToEdge;
 fBoxAlbedoTexture.BorderColor:=VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK;
 fBoxAlbedoTexture.UpdateSampler;

 fVulkanPipelineShaderStageCubeVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fCubeVertexShaderModule,'main');

 fVulkanPipelineShaderStageCubeFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fCubeFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

 fVulkanRenderPass:=nil;

 fVulkanVertexBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                             SizeOf(CubeVertices),
                                             TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT),
                                             TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                             nil,
                                             TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
                                            );
 fVulkanVertexBuffer.UploadData(pvApplication.VulkanDevice.TransferQueue,
                                pvApplication.VulkanTransferCommandBuffers[0,0],
                                pvApplication.VulkanTransferCommandBufferFences[0,0],
                                CubeVertices,
                                0,
                                SizeOf(CubeVertices),
                                vbutsbmYes);

 fVulkanIndexBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                            SizeOf(CubeIndices),
                                            TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT),
                                            TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                            nil,
                                            TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
                                           );
 fVulkanIndexBuffer.UploadData(pvApplication.VulkanDevice.TransferQueue,
                               pvApplication.VulkanTransferCommandBuffers[0,0],
                               pvApplication.VulkanTransferCommandBufferFences[0,0],
                               CubeIndices,
                               0,
                               SizeOf(CubeIndices),
                               vbutsbmYes);

 for Index:=0 to MaxSwapChainImages-1 do begin
  fVulkanUniformBuffers[Index]:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                       SizeOf(TScreenExampleCubeUniformBuffer),
                                                       TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT),
                                                       TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                       nil,
                                                       TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                       0,
                                                       0,
                                                       0,
                                                       0,
                                                       0,
                                                       [vbfPersistentMapped]
                                                      );
  fVulkanUniformBuffers[Index].UploadData(pvApplication.VulkanDevice.TransferQueue,
                                          pvApplication.VulkanTransferCommandBuffers[0,0],
                                          pvApplication.VulkanTransferCommandBufferFences[0,0],
                                          fUniformBuffer,
                                          0,
                                          SizeOf(TScreenExampleCubeUniformBuffer),
                                          vbutsbmYes);
 end;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       MaxSwapChainImages);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,MaxSwapChainImages);
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,MaxSwapChainImages);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
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

 for Index:=0 to MaxSwapChainImages-1 do begin
  fVulkanDescriptorSets[Index]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                              fVulkanDescriptorSetLayout);
  fVulkanDescriptorSets[Index].WriteToDescriptorSet(0,
                                                    0,
                                                    1,
                                                    TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                    [],
                                                    [fVulkanUniformBuffers[Index].DescriptorBufferInfo],
                                                    [],
                                                    false
                                                   );
  fVulkanDescriptorSets[Index].WriteToDescriptorSet(1,
                                                    0,
                                                    1,
                                                    TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                    [fBoxAlbedoTexture.DescriptorImageInfo],
                                                    [],
                                                    [],
                                                    false
                                                   );
  fVulkanDescriptorSets[Index].Flush;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 fVulkanPipelineLayout.Initialize;

end;

procedure TScreenExampleCube.Hide;
var Index:TpvInt32;
begin
 FreeAndNil(fVulkanPipelineLayout);
 for Index:=0 to MaxSwapChainImages-1 do begin
  FreeAndNil(fVulkanDescriptorSets[Index]);
 end;
 FreeAndNil(fVulkanDescriptorSetLayout);
 FreeAndNil(fVulkanDescriptorPool);
 for Index:=0 to MaxSwapChainImages-1 do begin
  FreeAndNil(fVulkanUniformBuffers[Index]);
 end;
 FreeAndNil(fVulkanIndexBuffer);
 FreeAndNil(fVulkanVertexBuffer);
 FreeAndNil(fVulkanRenderPass);
 FreeAndNil(fVulkanGraphicsPipeline);
 FreeAndNil(fVulkanPipelineShaderStageCubeVertex);
 FreeAndNil(fVulkanPipelineShaderStageCubeFragment);
 FreeAndNil(fCubeFragmentShaderModule);
 FreeAndNil(fCubeVertexShaderModule);
 FreeAndNil(fBoxAlbedoTexture);
 for Index:=0 to MaxSwapChainImages-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[Index]);
  FreeAndNil(fVulkanRenderSemaphores[Index]);
 end;
 FreeAndNil(fVulkanCommandPool);
 inherited Hide;
end;

procedure TScreenExampleCube.Resume;
begin
 inherited Resume;
end;

procedure TScreenExampleCube.Pause;
begin
 inherited Pause;
end;

procedure TScreenExampleCube.Resize(const aWidth,aHeight:TpvInt32);
begin
 inherited Resize(aWidth,aHeight);
end;

procedure TScreenExampleCube.AfterCreateSwapChain;
var SwapChainImageIndex:TpvInt32;
    VulkanCommandBuffer:TpvVulkanCommandBuffer;
begin
 inherited AfterCreateSwapChain;

 FreeAndNil(fVulkanRenderPass);
 FreeAndNil(fVulkanGraphicsPipeline);

 fVulkanRenderPass:=TpvVulkanRenderPass.Create(pvApplication.VulkanDevice);

 fVulkanRenderPass.AddSubpassDescription(0,
                                         VK_PIPELINE_BIND_POINT_GRAPHICS,
                                         [],
                                         [fVulkanRenderPass.AddAttachmentReference(fVulkanRenderPass.AddAttachmentDescription(0,
                                                                                                                              pvApplication.VulkanSwapChain.ImageFormat,
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
                                                                                                                             pvApplication.VulkanDepthImageFormat,
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

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                           pvApplication.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           fVulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           0,
                                                           nil,
                                                           0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageCubeVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageCubeFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputBindingDescription(0,SizeOf(TVertex),VK_VERTEX_INPUT_RATE_VERTEX);
 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(0,0,VK_FORMAT_R32G32B32_SFLOAT,TVkPtrUInt(pointer(@PVertex(nil)^.Position)));
 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(1,0,VK_FORMAT_R32G32B32_SFLOAT,TVkPtrUInt(pointer(@PVertex(nil)^.Tangent)));
 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(2,0,VK_FORMAT_R32G32B32_SFLOAT,TVkPtrUInt(pointer(@PVertex(nil)^.Bitangent)));
 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(3,0,VK_FORMAT_R32G32B32_SFLOAT,TVkPtrUInt(pointer(@PVertex(nil)^.Normal)));
 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(4,0,VK_FORMAT_R32G32_SFLOAT,TVkPtrUInt(pointer(@PVertex(nil)^.TexCoord)));

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,pvApplication.VulkanSwapChain.Width,pvApplication.VulkanSwapChain.Height,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,pvApplication.VulkanSwapChain.Width,pvApplication.VulkanSwapChain.Height);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
 fVulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
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

 for SwapChainImageIndex:=0 to pvApplication.CountSwapChainImages-1 do begin

  fVulkanRenderCommandBuffers[SwapChainImageIndex]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

  VulkanCommandBuffer:=fVulkanRenderCommandBuffers[SwapChainImageIndex];

  VulkanCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT));

 {VulkanCommandBuffer.MetaCmdMemoryBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                           TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
                                           TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT),
                                           TVkAccessFlags(VK_ACCESS_UNIFORM_READ_BIT));}

  fVulkanRenderPass.BeginRenderPass(VulkanCommandBuffer,
                                    pvApplication.VulkanFrameBuffers[SwapChainImageIndex],
                                    VK_SUBPASS_CONTENTS_INLINE,
                                    0,
                                    0,
                                    pvApplication.VulkanSwapChain.Width,
                                    pvApplication.VulkanSwapChain.Height);

  VulkanCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanPipelineLayout.Handle,0,1,@fVulkanDescriptorSets[SwapChainImageIndex].Handle,0,nil);
  VulkanCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
  VulkanCommandBuffer.CmdBindVertexBuffers(0,1,@fVulkanVertexBuffer.Handle,@Offsets);
  VulkanCommandBuffer.CmdBindIndexBuffer(fVulkanIndexBuffer.Handle,0,VK_INDEX_TYPE_UINT32);
  VulkanCommandBuffer.CmdDrawIndexed(length(CubeIndices),1,0,0,0);

  fVulkanRenderPass.EndRenderPass(VulkanCommandBuffer);

  VulkanCommandBuffer.EndRecording;

 end;

end;

procedure TScreenExampleCube.BeforeDestroySwapChain;
begin
 FreeAndNil(fVulkanRenderPass);
 FreeAndNil(fVulkanGraphicsPipeline);
 inherited BeforeDestroySwapChain;
end;

function TScreenExampleCube.KeyDown(const aKeyCode:TpvInt32;const aKeyModifiers:TpvApplicationInputKeyModifiers):boolean;
begin
 result:=false;
 if fReady then begin
  case aKeyCode of
   KEYCODE_AC_BACK,KEYCODE_ESCAPE:begin
    pvApplication.NextScreen:=TScreenMainMenu.Create;
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
     pvApplication.NextScreen:=TScreenMainMenu.Create;
    end;
   end;
  end;
 end;
end;

function TScreenExampleCube.KeyUp(const aKeyCode:TpvInt32;const aKeyModifiers:TpvApplicationInputKeyModifiers):boolean;
begin
 result:=false;
end;

function TScreenExampleCube.KeyTyped(const aKeyCode:TpvInt32;const aKeyModifiers:TpvApplicationInputKeyModifiers):boolean;
begin
 result:=false;
end;

function TScreenExampleCube.PointerDown(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID:TpvInt32;const aButton:TpvApplicationInputPointerButton;const aButtons:TpvApplicationInputPointerButtons;const aKeyModifiers:TpvApplicationInputKeyModifiers):boolean;
var Index:TpvInt32;
    cy:TpvFloat;
begin
 result:=false;
 if fReady then begin
  fSelectedIndex:=-1;
  cy:=fStartY;
  for Index:=0 to 0 do begin
   if (aPosition.y>=cy) and (aPosition.y<(cy+(ExampleApplication.TextOverlay.FontCharHeight*FontSize))) then begin
    fSelectedIndex:=Index;
    if fSelectedIndex=0 then begin
     pvApplication.NextScreen:=TScreenMainMenu.Create;
    end;
   end;
   cy:=cy+((ExampleApplication.TextOverlay.FontCharHeight+4)*FontSize);
  end;
 end;
end;

function TScreenExampleCube.PointerUp(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID:TpvInt32;const aButton:TpvApplicationInputPointerButton;const aButtons:TpvApplicationInputPointerButtons;const aKeyModifiers:TpvApplicationInputKeyModifiers):boolean;
begin
 result:=false;
end;

function TScreenExampleCube.PointerMotion(const aPosition,aRelativePosition:TpvVector2;const aPressure:TpvFloat;const aPointerID:TpvInt32;const aButtons:TpvApplicationInputPointerButtons;const aKeyModifiers:TpvApplicationInputKeyModifiers):boolean;
var Index:TpvInt32;
    cy:TpvFloat;
begin
 result:=false;
 if fReady then begin
  fSelectedIndex:=-1;
  cy:=fStartY;
  for Index:=0 to 0 do begin
   if (aPosition.y>=cy) and (aPosition.y<(cy+(ExampleApplication.TextOverlay.FontCharHeight*FontSize))) then begin
    fSelectedIndex:=Index;
   end;
   cy:=cy+((ExampleApplication.TextOverlay.FontCharHeight+4)*FontSize);
  end;
 end;
end;

function TScreenExampleCube.Scrolled(const aRelativeAmount:TpvVector2):boolean;
begin
 result:=false;
end;

function TScreenExampleCube.CanBeParallelProcessed:boolean;
begin
 result:=true;
end;

procedure TScreenExampleCube.Update(const aDeltaTime:TpvDouble);
const BoolToInt:array[boolean] of TpvInt32=(0,1);
      Options:array[0..0] of string=('Back');
      f0=1.0/(2.0*pi);
      f1=0.5/(2.0*pi);
var Index:TpvInt32;
    cy:TpvFloat;
    s:string;
    IsSelected:boolean;
begin
 inherited Update(aDeltaTime);
 fState.Time:=fState.Time+aDeltaTime;
 fState.AnglePhases[0]:=frac(fState.AnglePhases[0]+(aDeltaTime*f0));
 fState.AnglePhases[1]:=frac(fState.AnglePhases[1]+(aDeltaTime*f1));
 fStates[pvApplication.UpdateSwapChainImageIndex]:=fState;
 ExampleApplication.TextOverlay.AddText(pvApplication.Width*0.5,ExampleApplication.TextOverlay.FontCharHeight*1.0,2.0,toaCenter,'Cube');
 fStartY:=pvApplication.Height-((((ExampleApplication.TextOverlay.FontCharHeight+4)*FontSize)*1.25)-(4*FontSize));
 cy:=fStartY;
 for Index:=0 to 0 do begin
  IsSelected:=fSelectedIndex=Index;
  s:=' '+Options[Index]+' ';
  if IsSelected then begin
   s:='>'+s+'<';
  end;
  ExampleApplication.TextOverlay.AddText(pvApplication.Width*0.5,cy,FontSize,toaCenter,TpvRawByteString(s),MenuColors[IsSelected,0,0],MenuColors[IsSelected,0,1],MenuColors[IsSelected,0,2],MenuColors[IsSelected,0,3],MenuColors[IsSelected,1,0],MenuColors[IsSelected,1,1],MenuColors[IsSelected,1,2],MenuColors[IsSelected,1,3]);
  cy:=cy+((ExampleApplication.TextOverlay.FontCharHeight+4)*FontSize);
 end;
 fReady:=true;
end;

procedure TScreenExampleCube.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
const TwoPI=2.0*pi;
var p:pointer;
    ModelMatrix:TpvMatrix4x4;
    ViewMatrix:TpvMatrix4x4;
    ProjectionMatrix:TpvMatrix4x4;
    State:PScreenExampleCubeState;
begin
 inherited Draw(aSwapChainImageIndex,aWaitSemaphore,nil);
 if assigned(fVulkanGraphicsPipeline) then begin

  State:=@fStates[pvApplication.DrawSwapChainImageIndex];

  ModelMatrix:=TpvMatrix4x4.CreateRotate(State^.AnglePhases[0]*TwoPI,TpvVector3.Create(0.0,0.0,1.0))*
               TpvMatrix4x4.CreateRotate(State^.AnglePhases[1]*TwoPI,TpvVector3.Create(0.0,1.0,0.0));
  ViewMatrix:=TpvMatrix4x4.CreateTranslation(0.0,0.0,-6.0);
  ProjectionMatrix:=TpvMatrix4x4.CreatePerspective(45.0,pvApplication.Width/pvApplication.Height,1.0,128.0);

  fUniformBuffer.ModelViewProjectionMatrix:=(ModelMatrix*ViewMatrix)*ProjectionMatrix;
  fUniformBuffer.ModelViewNormalMatrix:=TpvMatrix4x4.Create((ModelMatrix*ViewMatrix).ToMatrix3x3.Inverse.Transpose);

  p:=fVulkanUniformBuffers[pvApplication.DrawSwapChainImageIndex].Memory.MapMemory(0,SizeOf(TScreenExampleCubeUniformBuffer));
  if assigned(p) then begin
   try
    Move(fUniformBuffer,p^,SizeOf(TScreenExampleCubeUniformBuffer));
   finally
    fVulkanUniformBuffers[pvApplication.DrawSwapChainImageIndex].Memory.UnmapMemory;
   end;
  end;

  fVulkanRenderCommandBuffers[aSwapChainImageIndex].Execute(pvApplication.VulkanDevice.GraphicsQueue,
                                                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                                            aWaitSemaphore,
                                                            fVulkanRenderSemaphores[aSwapChainImageIndex],
                                                            aWaitFence,
                                                            false);

  aWaitSemaphore:=fVulkanRenderSemaphores[aSwapChainImageIndex];

 end;
end;

initialization
 RegisterExample('Cube',TScreenExampleCube);
end.
