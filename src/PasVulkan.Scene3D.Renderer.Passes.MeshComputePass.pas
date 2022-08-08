(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2020, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.Scene3D.Renderer.Passes.MeshComputePass;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.FrameGraph,
     PasVulkan.Scene3D,
     PasVulkan.Scene3D.Renderer.Globals,
     PasVulkan.Scene3D.Renderer,
     PasVulkan.Scene3D.Renderer.Instance;

type { TpvScene3DRendererPassesMeshComputePass }
     TpvScene3DRendererPassesMeshComputePass=class(TpvFrameGraph.TComputePass)
      private
       fInstance:TpvScene3DRendererInstance;
       fResourceInput:TpvFrameGraph.TPass.TUsedImageResource;
       fComputeShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStageCompute:TpvVulkanPipelineShaderStage;
       fPipelineLayout:TpvVulkanPipelineLayout;
       fPipeline:TpvVulkanComputePipeline;
       fEvents:array[0..MaxInFlightFrames-1] of TpvVulkanEvent;
       fEventReady:array[0..MaxInFlightFrames-1] of boolean;
      public
       constructor Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance); reintroduce;
       destructor Destroy; override;
       procedure AcquirePersistentResources; override;
       procedure ReleasePersistentResources; override;
       procedure AcquireVolatileResources; override;
       procedure ReleaseVolatileResources; override;
       procedure Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt); override;
       procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
     end;

implementation

{ TpvScene3DRendererPassesMeshComputePass }

constructor TpvScene3DRendererPassesMeshComputePass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin
 inherited Create(aFrameGraph);
 fInstance:=aInstance;
 Name:='MeshComputePass';
end;

destructor TpvScene3DRendererPassesMeshComputePass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesMeshComputePass.AcquirePersistentResources;
var Stream:TStream;
begin

 inherited AcquirePersistentResources;

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('mesh_comp.spv');
 try
  fComputeShaderModule:=TpvVulkanShaderModule.Create(fInstance.Renderer.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageCompute:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

end;

procedure TpvScene3DRendererPassesMeshComputePass.ReleasePersistentResources;
begin
 FreeAndNil(fVulkanPipelineShaderStageCompute);
 FreeAndNil(fComputeShaderModule);
 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesMeshComputePass.AcquireVolatileResources;
var Index:TpvSizeInt;
begin

 inherited AcquireVolatileResources;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(fInstance.Renderer.VulkanDevice);
 fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TpvScene3D.TMeshComputeStagePushConstants));
 fPipelineLayout.AddDescriptorSetLayout(fInstance.Renderer.Scene3D.MeshComputeVulkanDescriptorSetLayout);
 fPipelineLayout.Initialize;

 fPipeline:=TpvVulkanComputePipeline.Create(fInstance.Renderer.VulkanDevice,
                                            fInstance.Renderer.VulkanPipelineCache,
                                            0,
                                            fVulkanPipelineShaderStageCompute,
                                            fPipelineLayout,
                                            nil,
                                            0);

 for Index:=0 to fInstance.Renderer.CountInFlightFrames-1 do begin
  fEvents[Index]:=TpvVulkanEvent.Create(fInstance.Renderer.VulkanDevice);
  fEventReady[Index]:=false;
 end;

end;

procedure TpvScene3DRendererPassesMeshComputePass.ReleaseVolatileResources;
var Index:TpvSizeInt;
begin
 FreeAndNil(fPipeline);
 FreeAndNil(fPipelineLayout);
 for Index:=0 to fInstance.Renderer.CountInFlightFrames-1 do begin
  FreeAndNil(fEvents[Index]);
 end;
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesMeshComputePass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvScene3DRendererPassesMeshComputePass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var PreviousInFlightFrameIndex:TpvSizeInt;
    MemoryBarrier:TVkMemoryBarrier;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);
 fInstance.Renderer.Scene3D.UpdateCachedVertices(fPipeline,
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


end.
