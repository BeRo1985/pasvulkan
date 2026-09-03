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
 ******************************************************************************)
unit PasVulkan.Scene3D.Renderer.Passes.PickRenderPass;
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

type { TpvScene3DRendererPassesPickRenderPass }
     // Object picking: the target a later pick shader writes mesh object ids into, so that what is
     // under the cursor can be read back from one pixel instead of being guessed from collision
     // shapes. Exists only when the renderer instance was asked for it (TpvScene3DRendererInstance.
     // Picking, to be set between Create and Prepare) - every other project keeps the frame graph it
     // has today.
     //
     // First step on purpose: the pass draws NOTHING. It only declares its attachments and is always
     // active, so what an added pass costs by merely existing - the render pass switch and the load
     // ops - can be measured before any of the drawing machinery is written. Everything else (the
     // ray filtered indirect draw list, the id writing shader, the one pixel readback) comes on top
     // of this and can be compared against it.
     TpvScene3DRendererPassesPickRenderPass=class(TpvFrameGraph.TRenderPass)
      private
       fInstance:TpvScene3DRendererInstance;
       fResourceID:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceDepth:TpvFrameGraph.TPass.TUsedImageResource;
      public
       constructor Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance); reintroduce;
       destructor Destroy; override;
       procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt); override;
     end;

implementation

{ TpvScene3DRendererPassesPickRenderPass }

constructor TpvScene3DRendererPassesPickRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin

 inherited Create(aFrameGraph);

 fInstance:=aInstance;

 Name:='PickRenderPass';

 MultiviewMask:=fInstance.SurfaceMultiviewMask;

 Queue:=aFrameGraph.UniversalQueue;

 Size:=TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,
                                       fInstance.SizeFactor,
                                       fInstance.SizeFactor,
                                       1.0,
                                       fInstance.CountSurfaceViews);

 // Cleared to zero, which is the "nothing here" id: mesh object ids are handed out from one upwards,
 // so a read back zero says the cursor was over nothing without needing a second flag.
 fResourceID:=AddImageOutput('resourcetype_pick_id',
                             'resource_pick_id',
                             VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                             TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                          TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0)),
                             [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                            );

 fResourceDepth:=AddImageDepthOutput('resourcetype_pick_id_depth',
                                     'resource_pick_id_depth',
                                     VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                     TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.Clear,
                                                                  // reverse-Z: clear depth to the far value (0.0 when ZFar<0)
                                                                  TpvVector4.InlineableCreate(IfThen(fInstance.ZFar<0.0,0.0,1.0),0.0,0.0,0.0)),
                                     [TpvFrameGraph.TResourceTransition.TFlag.Attachment]
                                    );

end;

destructor TpvScene3DRendererPassesPickRenderPass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesPickRenderPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 // Nothing yet - see the class comment. The attachments are cleared by their load ops, so the target
 // already holds a defined "nothing under the cursor" while the drawing side does not exist.

end;

end.
