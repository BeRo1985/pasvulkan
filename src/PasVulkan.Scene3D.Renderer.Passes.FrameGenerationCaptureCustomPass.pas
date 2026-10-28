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
unit PasVulkan.Scene3D.Renderer.Passes.FrameGenerationCaptureCustomPass;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

// Captures the rendered frame for frame generation, see TpvScene3DRendererFrameGenerationCompositor: the
// picture as it goes into the HUD composition, so with everything drawn into the scene but without the HUD,
// and the HUD itself where the instance has one. Where the backend reads them, the resolved depth and the
// motion vectors with the camera motion filled in, as the temporal upscaler reads them, as well. Only in the
// frame graph with frame generation.

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

type { TpvScene3DRendererPassesFrameGenerationCaptureCustomPass }
     TpvScene3DRendererPassesFrameGenerationCaptureCustomPass=class(TpvFrameGraph.TCustomPass)
      private
       fInstance:TpvScene3DRendererInstance;
       fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceHUD:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceMotionVectors:TpvFrameGraph.TPass.TUsedImageResource;
      public
       constructor Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance;const aWithHUD:boolean); reintroduce;
       destructor Destroy; override;
       procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
     end;

implementation

{ TpvScene3DRendererPassesFrameGenerationCaptureCustomPass }

constructor TpvScene3DRendererPassesFrameGenerationCaptureCustomPass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance;const aWithHUD:boolean);
begin
 inherited Create(aFrameGraph);

 fInstance:=aInstance;

 Name:='FrameGenerationCaptureCustomPass';

 fResourceColor:=AddImageInput(fInstance.LastOutputResource.ResourceType.Name,
                               fInstance.LastOutputResource.Resource.Name,
                               VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                               []
                              );

 if aWithHUD then begin
  fResourceHUD:=AddImageInput('resourcetype_hud_color',
                              'resource_hud_color',
                              VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                              []
                             );
 end else begin
  fResourceHUD:=nil;
 end;

 // The depth is not taken from the graph: it comes from FinalDepthArray2DImage, which the depth resolve
 // leaves behind, and which the instance makes this pass wait for.
 if fInstance.FrameGenerationNeedsDepthAndMotionVectors then begin
  fResourceMotionVectors:=AddImageInput('resourcetype_temporal_upscaler_velocity',
                                        'resource_temporal_upscaler_velocity',
                                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                        []
                                       );
 end else begin
  fResourceMotionVectors:=nil;
 end;

end;

destructor TpvScene3DRendererPassesFrameGenerationCaptureCustomPass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesFrameGenerationCaptureCustomPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var HUDImage,DepthImage,MotionVectorsImage:TVkImage;
begin
 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);
 if assigned(fInstance.FrameGenerationCompositor) then begin
  if assigned(fResourceHUD) then begin
   HUDImage:=fResourceHUD.VulkanImages[aInFlightFrameIndex].Handle;
  end else begin
   HUDImage:=VK_NULL_HANDLE;
  end;
  if assigned(fResourceMotionVectors) and assigned(fInstance.FinalDepthArray2DImage) then begin
   DepthImage:=fInstance.FinalDepthArray2DImage.VulkanImage.Handle;
   MotionVectorsImage:=fResourceMotionVectors.VulkanImages[aInFlightFrameIndex].Handle;
  end else begin
   DepthImage:=VK_NULL_HANDLE;
   MotionVectorsImage:=VK_NULL_HANDLE;
  end;
  fInstance.FrameGenerationCompositor.Capture(aCommandBuffer,
                                              fResourceColor.VulkanImages[aInFlightFrameIndex].Handle,
                                              0,
                                              HUDImage,
                                              DepthImage,
                                              0,
                                              MotionVectorsImage,
                                              0);
 end;
end;

end.
