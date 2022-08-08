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
unit PasVulkan.Scene3D.Renderer.Passes.AntialiasingTAAWaitEventCustomPass;
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

type { TpvScene3DRendererPassesAntialiasingTAAWaitEventCustomPass }
     TpvScene3DRendererPassesAntialiasingTAAWaitEventCustomPass=class(TpvFrameGraph.TCustomPass)
      private
       fInstance:TpvScene3DRendererInstance;
       fResourceHistoryColor:TpvFrameGraph.TPass.TUsedImageResource;
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

{ TpvScene3DRendererPassesAntialiasingTAAWaitEventCustomPass }

constructor TpvScene3DRendererPassesAntialiasingTAAWaitEventCustomPass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin
 inherited Create(aFrameGraph);
 fInstance:=aInstance;
 Name:='AntialiasingTAAWaitEventCustomPass';

 fResourceHistoryColor:=AddImageInput('resourcetype_color_temporal_antialiasing',
                                      'resource_temporal_antialiasing_history_color',
                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                      [TpvFrameGraph.TResourceTransition.TFlag.Attachment,
                                       TpvFrameGraph.TResourceTransition.TFlag.PreviousFrameInput]
                                     );
end;

destructor TpvScene3DRendererPassesAntialiasingTAAWaitEventCustomPass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesAntialiasingTAAWaitEventCustomPass.AcquirePersistentResources;
begin
 inherited AcquirePersistentResources;
end;

procedure TpvScene3DRendererPassesAntialiasingTAAWaitEventCustomPass.ReleasePersistentResources;
begin
 inherited ReleasePersistentResources;
end;

procedure TpvScene3DRendererPassesAntialiasingTAAWaitEventCustomPass.AcquireVolatileResources;
begin
 inherited AcquireVolatileResources;
end;

procedure TpvScene3DRendererPassesAntialiasingTAAWaitEventCustomPass.ReleaseVolatileResources;
begin
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesAntialiasingTAAWaitEventCustomPass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvScene3DRendererPassesAntialiasingTAAWaitEventCustomPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var ImageMemoryBarrier:TVkImageMemoryBarrier;
    ImageSubresourceRange:TVkImageSubresourceRange;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 ImageSubresourceRange:=TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),0,1,0,fInstance.CountSurfaceViews);

 ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or
                                                  TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT) or
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or
                                                  TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                                  TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL),
                                                  TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL),
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  fResourceHistoryColor.VulkanImages[FrameGraph.ConvertRelativeToAbsoluteInFlightFrameIndex(aInFlightFrameIndex,-1)].Handle,
                                                  ImageSubresourceRange);

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT) or
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) or
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT) or
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) or
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                   TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT),
                                   0,
                                   nil,
                                   0,
                                   nil,
                                   1,
                                   @ImageMemoryBarrier);

end;

end.
