(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2026, Benjamin Rosseaux (benjamin@rosseaux.de)          *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 *                                                                            *
 ******************************************************************************
 *                  General guidelines for code contributors                  *
 *============================================================================*
 *                                                                            *
 * 1. Make sure you are legally allowed to make a contribution under the zlib *
 *    license.                                                                *
 * 2. The zlib license header goes at the top of each source file, with       *
 *    appropriate copyright notice.                                           *
 * 3. This PasVulkan wrapper may be used only with the PasVulkan-own Vulkan   *
 *    Pascal header.                                                          *
 * 4. After a pull request, check the status of your pull request on          *
      http://github.com/BeRo1985/pasvulkan                                    *
 * 5. Write code which's compatible with Delphi >= 2009 and FreePascal >=     *
 *    3.1.1                                                                   *
 * 6. Don't use Delphi-only, FreePascal-only or Lazarus-only libraries/units, *
 *    but if needed, make it out-ifdef-able.                                  *
 * 7. No use of third-party libraries/units as possible, but if needed, make  *
 *    it out-ifdef-able.                                                      *
 * 8. Try to use const when possible.                                         *
 * 9. Make sure to comment out writeln, used while debugging.                 *
 * 10. Make sure the code compiles on 32-bit and 64-bit platforms (x86-32,    *
 *     x86-64, ARM, ARM64, etc.).                                             *
 * 11. Make sure the code runs on all platforms with Vulkan support           *
 *                                                                            *
 ******************************************************************************)
unit PasVulkan.Scene3D.Renderer.FrameGenerationBackend.Blend;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

// The built-in frame generation for testing, TpvScene3DRendererFrameGeneration.Blend: a plain cross-fade
// between the two rendered frames around the generated one, see framegen_blend.comp. Anything that moves
// shows up twice, but it needs no depth and no motion vectors and runs everywhere, so that the
// presentation of generated frames - their order, their timing, the HUD on top - can be checked apart from
// any real frame generation.

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Scene3D.Renderer.Globals,
     PasVulkan.Scene3D.Renderer.FrameGenerationBackend;

type { TpvScene3DRendererFrameGenerationBackendBlend }
     TpvScene3DRendererFrameGenerationBackendBlend=class(TpvScene3DRendererFrameGenerationBackend)
      public
       const MaximumCountGeneratedFrames=3;
       type TPushConstants=packed record
             Factor:TpvFloat;
            end;
            PPushConstants=^TPushConstants;
      private
       fShaderModule:TpvVulkanShaderModule;
       fShaderStage:TpvVulkanPipelineShaderStage;
       fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fDescriptorPool:TpvVulkanDescriptorPool;
       fDescriptorSets:array[0..MaxInFlightFrames-1,0..MaximumCountGeneratedFrames-1] of TpvVulkanDescriptorSet;
       fPipelineLayout:TpvVulkanPipelineLayout;
       fPipeline:TpvVulkanComputePipeline;
      public
       class function IsSupported(const aDevice:TpvVulkanDevice):boolean; override;
       class function GetMaximumCountGeneratedFrames(const aDevice:TpvVulkanDevice):TpvInt32; override;
       class function NeedsDepthAndMotionVectors:boolean; override;
       constructor Create(const aDevice:TpvVulkanDevice;
                          const aPipelineCache:TpvVulkanPipelineCache;
                          const aCountInFlightFrames:TpvSizeInt;
                          const aWidth:TpvInt32;
                          const aHeight:TpvInt32;
                          const aRenderWidth:TpvInt32;
                          const aRenderHeight:TpvInt32;
                          const aColorFormat:TVkFormat;
                          const aInvertedDepth:boolean;
                          const aName:TpvUTF8String); override;
       destructor Destroy; override;
       function Generate(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex:TpvSizeInt;const aDescription:TGenerateDescription):boolean; override;
     end;

implementation

{ TpvScene3DRendererFrameGenerationBackendBlend }

class function TpvScene3DRendererFrameGenerationBackendBlend.IsSupported(const aDevice:TpvVulkanDevice):boolean;
begin
 result:=true;
end;

class function TpvScene3DRendererFrameGenerationBackendBlend.GetMaximumCountGeneratedFrames(const aDevice:TpvVulkanDevice):TpvInt32;
begin
 result:=MaximumCountGeneratedFrames;
end;

class function TpvScene3DRendererFrameGenerationBackendBlend.NeedsDepthAndMotionVectors:boolean;
begin
 result:=false;
end;

constructor TpvScene3DRendererFrameGenerationBackendBlend.Create(const aDevice:TpvVulkanDevice;
                                                                 const aPipelineCache:TpvVulkanPipelineCache;
                                                                 const aCountInFlightFrames:TpvSizeInt;
                                                                 const aWidth:TpvInt32;
                                                                 const aHeight:TpvInt32;
                                                                 const aRenderWidth:TpvInt32;
                                                                 const aRenderHeight:TpvInt32;
                                                                 const aColorFormat:TVkFormat;
                                                                 const aInvertedDepth:boolean;
                                                                 const aName:TpvUTF8String);
var Stream:TStream;
    InFlightFrameIndex,Index:TpvSizeInt;
begin

 inherited Create(aDevice,aPipelineCache,aCountInFlightFrames,aWidth,aHeight,aRenderWidth,aRenderHeight,aColorFormat,aInvertedDepth,aName);

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('framegen_blend_comp.spv');
 try
  fShaderModule:=TpvVulkanShaderModule.Create(fDevice,Stream);
 finally
  Stream.Free;
 end;
 fDevice.DebugUtils.SetObjectName(fShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,fName+'.ShaderModule');

 fShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fShaderModule,'main');

 fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fDevice);
 fDescriptorSetLayout.AddBinding(0,VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);
 fDescriptorSetLayout.AddBinding(1,VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);
 fDescriptorSetLayout.AddBinding(2,VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,1,TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),[]);
 fDescriptorSetLayout.Initialize;
 fDevice.DebugUtils.SetObjectName(fDescriptorSetLayout.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,fName+'.DescriptorSetLayout');

 // One set per in-flight frame and generated frame, written while the command buffer of that in-flight
 // frame is recorded, which is after its previous use has finished.
 fDescriptorPool:=TpvVulkanDescriptorPool.Create(fDevice,
                                                 TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                 MaxInFlightFrames*MaximumCountGeneratedFrames);
 fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE,MaxInFlightFrames*MaximumCountGeneratedFrames*2);
 fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,MaxInFlightFrames*MaximumCountGeneratedFrames);
 fDescriptorPool.Initialize;
 fDevice.DebugUtils.SetObjectName(fDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,fName+'.DescriptorPool');

 for InFlightFrameIndex:=0 to MaxInFlightFrames-1 do begin
  for Index:=0 to MaximumCountGeneratedFrames-1 do begin
   fDescriptorSets[InFlightFrameIndex,Index]:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
   fDevice.DebugUtils.SetObjectName(fDescriptorSets[InFlightFrameIndex,Index].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,fName+'.DescriptorSet['+IntToStr(InFlightFrameIndex)+','+IntToStr(Index)+']');
  end;
 end;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(fDevice);
 fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
 fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
 fPipelineLayout.Initialize;
 fDevice.DebugUtils.SetObjectName(fPipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,fName+'.PipelineLayout');

 fPipeline:=TpvVulkanComputePipeline.Create(fDevice,
                                            fPipelineCache,
                                            0,
                                            fShaderStage,
                                            fPipelineLayout,
                                            nil,
                                            0);
 fDevice.DebugUtils.SetObjectName(fPipeline.Handle,VK_OBJECT_TYPE_PIPELINE,fName+'.Pipeline');

end;

destructor TpvScene3DRendererFrameGenerationBackendBlend.Destroy;
var InFlightFrameIndex,Index:TpvSizeInt;
begin
 FreeAndNil(fPipeline);
 FreeAndNil(fPipelineLayout);
 for InFlightFrameIndex:=0 to MaxInFlightFrames-1 do begin
  for Index:=0 to MaximumCountGeneratedFrames-1 do begin
   FreeAndNil(fDescriptorSets[InFlightFrameIndex,Index]);
  end;
 end;
 FreeAndNil(fDescriptorPool);
 FreeAndNil(fDescriptorSetLayout);
 FreeAndNil(fShaderStage);
 FreeAndNil(fShaderModule);
 inherited Destroy;
end;

function TpvScene3DRendererFrameGenerationBackendBlend.Generate(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex:TpvSizeInt;const aDescription:TGenerateDescription):boolean;
var DescriptorSet:TpvVulkanDescriptorSet;
    PushConstants:TPushConstants;
begin

 result:=false;

 if (aDescription.Index<1) or (aDescription.Index>MaximumCountGeneratedFrames) then begin
  exit;
 end;

 DescriptorSet:=fDescriptorSets[aInFlightFrameIndex,aDescription.Index-1];
 DescriptorSet.WriteToDescriptorSet(0,
                                    0,
                                    1,
                                    TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                                    [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                   aDescription.Previous.Color.ImageView,
                                                                   VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                    [],
                                    [],
                                    false);
 DescriptorSet.WriteToDescriptorSet(1,
                                    0,
                                    1,
                                    TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                                    [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                   aDescription.Current.Color.ImageView,
                                                                   VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                    [],
                                    [],
                                    false);
 DescriptorSet.WriteToDescriptorSet(2,
                                    0,
                                    1,
                                    TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                    [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                   aDescription.Output.ImageView,
                                                                   VK_IMAGE_LAYOUT_GENERAL)],
                                    [],
                                    [],
                                    false);
 DescriptorSet.Flush;

 if aDescription.Reset then begin
  // Without a usable previous frame there is nothing to fade from, so the generated frame is the
  // current one.
  PushConstants.Factor:=1.0;
 end else begin
  PushConstants.Factor:=aDescription.Index/(aDescription.Count+1);
 end;

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);
 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                      fPipelineLayout.Handle,
                                      0,
                                      1,
                                      @DescriptorSet.Handle,
                                      0,
                                      nil);
 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 0,
                                 SizeOf(TPushConstants),
                                 @PushConstants);
 aCommandBuffer.CmdDispatch((fWidth+15) shr 4,(fHeight+15) shr 4,1);

 result:=true;

end;

initialization
 TpvScene3DRendererFrameGenerationBackendBlend.RegisterBackend(TpvScene3DRendererFrameGeneration.Blend);
end.
