(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2024, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.Scene3D.Renderer.Passes.GlobalIlluminationCascadedRadianceHintsInjectFinalizationCustomPass;
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
     PasVulkan.Scene3D.Renderer.Instance,
     PasVulkan.Scene3D.Renderer.SkyBox;

type { TpvScene3DRendererPassesGlobalIlluminationCascadedRadianceHintsInjectFinalizationCustomPass }
     TpvScene3DRendererPassesGlobalIlluminationCascadedRadianceHintsInjectFinalizationCustomPass=class(TpvFrameGraph.TCustomPass)
      private
       fInstance:TpvScene3DRendererInstance;
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

{ TpvScene3DRendererPassesGlobalIlluminationCascadedRadianceHintsInjectFinalizationCustomPass }

constructor TpvScene3DRendererPassesGlobalIlluminationCascadedRadianceHintsInjectFinalizationCustomPass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin
 inherited Create(aFrameGraph);
 fInstance:=aInstance;
 Name:='GlobalIlluminationCascadedRadianceHintsInjectFinalizationCustomPass';
end;

destructor TpvScene3DRendererPassesGlobalIlluminationCascadedRadianceHintsInjectFinalizationCustomPass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationCascadedRadianceHintsInjectFinalizationCustomPass.AcquirePersistentResources;
begin
 inherited AcquirePersistentResources;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationCascadedRadianceHintsInjectFinalizationCustomPass.ReleasePersistentResources;
begin
 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationCascadedRadianceHintsInjectFinalizationCustomPass.AcquireVolatileResources;
begin
 inherited AcquireVolatileResources;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationCascadedRadianceHintsInjectFinalizationCustomPass.ReleaseVolatileResources;
begin
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesGlobalIlluminationCascadedRadianceHintsInjectFinalizationCustomPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvScene3DRendererPassesGlobalIlluminationCascadedRadianceHintsInjectFinalizationCustomPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var Index,CascadeIndex,VolumeIndex:TpvInt32;
    ImageSubresourceRange:TVkImageSubresourceRange;
    ImageMemoryBarriers:array[0..(TpvScene3DRendererInstance.CountGlobalIlluminationRadiantHintCascades*TpvScene3DRendererInstance.CountGlobalIlluminationRadiantHintVolumeImages)-1] of TVkImageMemoryBarrier;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 ImageSubresourceRange:=TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                        0,
                                                        1,
                                                        0,
                                                        1);

 Index:=0;
 for CascadeIndex:=0 to TpvScene3DRendererInstance.CountGlobalIlluminationRadiantHintCascades-1 do begin
  for VolumeIndex:=0 to TpvScene3DRendererInstance.CountGlobalIlluminationRadiantHintVolumeImages-1 do begin
   ImageMemoryBarriers[Index]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                            TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                                            VK_IMAGE_LAYOUT_GENERAL,
                                                            VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                            VK_QUEUE_FAMILY_IGNORED,
                                                            VK_QUEUE_FAMILY_IGNORED,
                                                            fInstance.InFlightFrameCascadedRadianceHintVolumeImages[aInFlightFrameIndex,CascadeIndex,VolumeIndex].VulkanImage.Handle,
                                                            ImageSubresourceRange);
   inc(Index);
  end;
 end;
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   length(ImageMemoryBarriers),@ImageMemoryBarriers[0]);

{if fInstance.fGlobalIlluminationRadianceHintsEventReady[aInFlightFrameIndex] then begin
  Assert(false);
 end;}
 aCommandBuffer.CmdSetEvent(fInstance.fGlobalIlluminationRadianceHintsEvents[aInFlightFrameIndex].Handle,
                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_ALL_COMMANDS_BIT){
                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT) or
                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) or
                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) or
                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT) or
                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT)});
 fInstance.fGlobalIlluminationRadianceHintsEventReady[aInFlightFrameIndex]:=true;

end;

end.
