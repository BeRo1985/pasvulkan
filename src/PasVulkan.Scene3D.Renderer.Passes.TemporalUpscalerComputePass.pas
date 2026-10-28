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
unit PasVulkan.Scene3D.Renderer.Passes.TemporalUpscalerComputePass;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

// The temporal upscaler in the frame graph. It takes the place of the antialiasing and of the resampling
// at once: the jittered HDR picture at render resolution goes in, together with the motion vectors and
// the resolved depth, and the picture at output resolution comes out, still HDR, as the new
// LastOutputResource for the lens and the tone mapping behind it.
//
// The work itself is the backend's, one per view, so that every view - every eye - keeps a history of its
// own. The backends see single 2D layers, and this pass makes the layer views of the frame graph's array
// images for them.

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
     PasVulkan.Scene3D.Renderer.TemporalUpscalerBackend;

type { TpvScene3DRendererPassesTemporalUpscalerComputePass }
     TpvScene3DRendererPassesTemporalUpscalerComputePass=class(TpvFrameGraph.TComputePass)
      private
       type TLayerImageViews=record
             Color:TpvVulkanImageView;
             MotionVectors:TpvVulkanImageView;
             Reactive:TpvVulkanImageView;
             Output:TpvVulkanImageView;
            end;
            TLayerImageViewsArray=array of TLayerImageViews;
      private
       fInstance:TpvScene3DRendererInstance;
       fResourceColor:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceMotionVectors:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceReactive:TpvFrameGraph.TPass.TUsedImageResource;
       fResourceOutput:TpvFrameGraph.TPass.TUsedImageResource;
       fBackends:array of TpvScene3DRendererTemporalUpscalerBackend;
       fDepthImageViews:array of TpvVulkanImageView;
       fLayerImageViews:array[0..MaxInFlightFrames-1] of TLayerImageViewsArray;
       function CreateLayerImageView(const aImage:TpvVulkanImage;const aFormat:TVkFormat;const aLayer:TpvSizeInt;const aName:TpvUTF8String):TpvVulkanImageView;
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

{ TpvScene3DRendererPassesTemporalUpscalerComputePass }

constructor TpvScene3DRendererPassesTemporalUpscalerComputePass.Create(const aFrameGraph:TpvFrameGraph;const aInstance:TpvScene3DRendererInstance);
begin

 inherited Create(aFrameGraph);

 fInstance:=aInstance;

 Name:='TemporalUpscalerComputePass';

 fResourceColor:=AddImageInput(fInstance.LastOutputResource.ResourceType.Name,
                               fInstance.LastOutputResource.Resource.Name,
                               VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                               []
                              );

 // The velocity with the camera motion filled in where nothing was drawn, which the velocity pass before
 // this one makes.
 fResourceMotionVectors:=AddImageInput('resourcetype_temporal_upscaler_velocity',
                                       'resource_temporal_upscaler_velocity',
                                       VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                       []
                                      );

 // How much each pixel should follow the current frame, from the transparency and the depth of field,
 // which the reactive pass before this one makes.
 fResourceReactive:=AddImageInput('resourcetype_temporal_upscaler_reactive',
                                  'resource_temporal_upscaler_reactive',
                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                  []
                                 );

 // The depth is not taken from the graph: it comes from FinalDepthArray2DImage, which the depth resolve
 // leaves behind, and which the instance makes this pass wait for.

 fResourceOutput:=AddImageOutput('resourcetype_color_fullres_optimized_non_alpha',
                                 'resource_temporal_upscaled_color',
                                 VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                 TpvFrameGraph.TLoadOp.Create(TpvFrameGraph.TLoadOp.TKind.DontCare),
                                 []
                                );

 fInstance.LastOutputResource:=fResourceOutput;

end;

destructor TpvScene3DRendererPassesTemporalUpscalerComputePass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererPassesTemporalUpscalerComputePass.AcquirePersistentResources;
begin
 inherited AcquirePersistentResources;
end;

procedure TpvScene3DRendererPassesTemporalUpscalerComputePass.ReleasePersistentResources;
begin
 inherited ReleasePersistentResources;
end;

function TpvScene3DRendererPassesTemporalUpscalerComputePass.CreateLayerImageView(const aImage:TpvVulkanImage;const aFormat:TVkFormat;const aLayer:TpvSizeInt;const aName:TpvUTF8String):TpvVulkanImageView;
begin
 result:=TpvVulkanImageView.Create(fInstance.Renderer.VulkanDevice,
                                   aImage,
                                   VK_IMAGE_VIEW_TYPE_2D,
                                   aFormat,
                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                   VK_COMPONENT_SWIZZLE_IDENTITY,
                                   TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                   0,
                                   1,
                                   aLayer,
                                   1);
 fInstance.Renderer.VulkanDevice.DebugUtils.SetObjectName(result.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,aName);
end;

procedure TpvScene3DRendererPassesTemporalUpscalerComputePass.AcquireVolatileResources;
var InFlightFrameIndex,ViewIndex,CountViews:TpvSizeInt;
    Inputs:TpvScene3DRendererTemporalUpscalerBackend.TInputs;
    ColorFormat,MotionVectorsFormat,ReactiveFormat,OutputFormat:TVkFormat;
    InvertedDepth,InfiniteDepth:boolean;
    BackendClass:TpvScene3DRendererTemporalUpscalerBackendClass;
begin

 inherited AcquireVolatileResources;

 CountViews:=Max(1,fInstance.CountSurfaceViews);

 ColorFormat:=TpvFrameGraph.TImageResourceType(fResourceColor.ResourceType).Format;
 MotionVectorsFormat:=TpvFrameGraph.TImageResourceType(fResourceMotionVectors.ResourceType).Format;
 ReactiveFormat:=TpvFrameGraph.TImageResourceType(fResourceReactive.ResourceType).Format;
 OutputFormat:=TpvFrameGraph.TImageResourceType(fResourceOutput.ResourceType).Format;

 // A negative far plane is the renderer's reversed depth, and an infinite one is the reversed infinite
 // depth; a positive far plane is the ordinary depth, made finite at 1024 if it was asked for infinite.
 InvertedDepth:=fInstance.ZFar<0.0;
 InfiniteDepth:=InvertedDepth and IsInfinite(fInstance.ZFar);

 SetLength(fBackends,CountViews);
 SetLength(fDepthImageViews,CountViews);
 for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin
  SetLength(fLayerImageViews[InFlightFrameIndex],CountViews);
 end;

 // The backend is the one registered for the active temporal upscaler, see
 // TpvScene3DRendererTemporalUpscalerBackend.RegisterBackend.
 BackendClass:=TpvScene3DRendererTemporalUpscalerBackend.FindBackend(fInstance.Renderer.ActiveTemporalUpscaler);

 for ViewIndex:=0 to CountViews-1 do begin

  if assigned(BackendClass) then begin
   fBackends[ViewIndex]:=BackendClass.Create(fInstance.Renderer.VulkanDevice,
                                             fInstance.Renderer.VulkanPipelineCache,
                                             FrameGraph.CountInFlightFrames,
                                             fInstance.ScaledWidth,
                                             fInstance.ScaledHeight,
                                             fInstance.Width,
                                             fInstance.Height,
                                             InvertedDepth,
                                             InfiniteDepth,
                                             'TemporalUpscaler.'+BackendClass.ClassName+'['+IntToStr(ViewIndex)+']');
   fBackends[ViewIndex].Quality:=fInstance.Renderer.TemporalUpscalerQuality;
   // The probe looks at the first view only.
   fBackends[ViewIndex].FrameGenerationProbe:=fInstance.Renderer.FrameGenerationProbe and (ViewIndex=0);
  end else begin
   fBackends[ViewIndex]:=nil;
  end;

  fDepthImageViews[ViewIndex]:=CreateLayerImageView(fInstance.FinalDepthArray2DImage.VulkanImage,
                                                    VK_FORMAT_R32_SFLOAT,
                                                    ViewIndex,
                                                    'TemporalUpscaler.Depth['+IntToStr(ViewIndex)+']');

  for InFlightFrameIndex:=0 to FrameGraph.CountInFlightFrames-1 do begin

   fLayerImageViews[InFlightFrameIndex,ViewIndex].Color:=CreateLayerImageView(fResourceColor.VulkanImages[InFlightFrameIndex],
                                                                              ColorFormat,
                                                                              ViewIndex,
                                                                              'TemporalUpscaler.Color['+IntToStr(InFlightFrameIndex)+','+IntToStr(ViewIndex)+']');

   fLayerImageViews[InFlightFrameIndex,ViewIndex].MotionVectors:=CreateLayerImageView(fResourceMotionVectors.VulkanImages[InFlightFrameIndex],
                                                                                      MotionVectorsFormat,
                                                                                      ViewIndex,
                                                                                      'TemporalUpscaler.MotionVectors['+IntToStr(InFlightFrameIndex)+','+IntToStr(ViewIndex)+']');

   fLayerImageViews[InFlightFrameIndex,ViewIndex].Reactive:=CreateLayerImageView(fResourceReactive.VulkanImages[InFlightFrameIndex],
                                                                                 ReactiveFormat,
                                                                                 ViewIndex,
                                                                                 'TemporalUpscaler.Reactive['+IntToStr(InFlightFrameIndex)+','+IntToStr(ViewIndex)+']');

   fLayerImageViews[InFlightFrameIndex,ViewIndex].Output:=CreateLayerImageView(fResourceOutput.VulkanImages[InFlightFrameIndex],
                                                                               OutputFormat,
                                                                               ViewIndex,
                                                                               'TemporalUpscaler.Output['+IntToStr(InFlightFrameIndex)+','+IntToStr(ViewIndex)+']');

   if assigned(fBackends[ViewIndex]) then begin

    FillChar(Inputs,SizeOf(TpvScene3DRendererTemporalUpscalerBackend.TInputs),#0);

    Inputs.Color.Image:=fResourceColor.VulkanImages[InFlightFrameIndex].Handle;
    Inputs.Color.ImageView:=fLayerImageViews[InFlightFrameIndex,ViewIndex].Color.Handle;
    Inputs.Color.Format:=ColorFormat;
    Inputs.Color.Width:=fInstance.ScaledWidth;
    Inputs.Color.Height:=fInstance.ScaledHeight;
    Inputs.Color.BaseArrayLayer:=ViewIndex;

    Inputs.Depth.Image:=fInstance.FinalDepthArray2DImage.VulkanImage.Handle;
    Inputs.Depth.ImageView:=fDepthImageViews[ViewIndex].Handle;
    Inputs.Depth.Format:=VK_FORMAT_R32_SFLOAT;
    Inputs.Depth.Width:=fInstance.ScaledWidth;
    Inputs.Depth.Height:=fInstance.ScaledHeight;
    Inputs.Depth.BaseArrayLayer:=ViewIndex;

    Inputs.MotionVectors.Image:=fResourceMotionVectors.VulkanImages[InFlightFrameIndex].Handle;
    Inputs.MotionVectors.ImageView:=fLayerImageViews[InFlightFrameIndex,ViewIndex].MotionVectors.Handle;
    Inputs.MotionVectors.Format:=MotionVectorsFormat;
    Inputs.MotionVectors.Width:=fInstance.ScaledWidth;
    Inputs.MotionVectors.Height:=fInstance.ScaledHeight;
    Inputs.MotionVectors.BaseArrayLayer:=ViewIndex;

    Inputs.Reactive.Image:=fResourceReactive.VulkanImages[InFlightFrameIndex].Handle;
    Inputs.Reactive.ImageView:=fLayerImageViews[InFlightFrameIndex,ViewIndex].Reactive.Handle;
    Inputs.Reactive.Format:=ReactiveFormat;
    Inputs.Reactive.Width:=fInstance.ScaledWidth;
    Inputs.Reactive.Height:=fInstance.ScaledHeight;
    Inputs.Reactive.BaseArrayLayer:=ViewIndex;

    Inputs.Output.Image:=fResourceOutput.VulkanImages[InFlightFrameIndex].Handle;
    Inputs.Output.ImageView:=fLayerImageViews[InFlightFrameIndex,ViewIndex].Output.Handle;
    Inputs.Output.Format:=OutputFormat;
    Inputs.Output.Width:=fInstance.Width;
    Inputs.Output.Height:=fInstance.Height;
    Inputs.Output.BaseArrayLayer:=ViewIndex;

    fBackends[ViewIndex].SetInputs(InFlightFrameIndex,Inputs);

   end;

  end;

 end;

end;

procedure TpvScene3DRendererPassesTemporalUpscalerComputePass.ReleaseVolatileResources;
var InFlightFrameIndex,ViewIndex:TpvSizeInt;
begin
 for ViewIndex:=0 to length(fBackends)-1 do begin
  FreeAndNil(fBackends[ViewIndex]);
 end;
 fBackends:=nil;
 for InFlightFrameIndex:=0 to MaxInFlightFrames-1 do begin
  for ViewIndex:=0 to length(fLayerImageViews[InFlightFrameIndex])-1 do begin
   FreeAndNil(fLayerImageViews[InFlightFrameIndex,ViewIndex].Output);
   FreeAndNil(fLayerImageViews[InFlightFrameIndex,ViewIndex].Reactive);
   FreeAndNil(fLayerImageViews[InFlightFrameIndex,ViewIndex].MotionVectors);
   FreeAndNil(fLayerImageViews[InFlightFrameIndex,ViewIndex].Color);
  end;
  fLayerImageViews[InFlightFrameIndex]:=nil;
 end;
 for ViewIndex:=0 to length(fDepthImageViews)-1 do begin
  FreeAndNil(fDepthImageViews[ViewIndex]);
 end;
 fDepthImageViews:=nil;
 inherited ReleaseVolatileResources;
end;

procedure TpvScene3DRendererPassesTemporalUpscalerComputePass.Update(const aUpdateInFlightFrameIndex,aUpdateFrameIndex:TpvSizeInt);
begin
 inherited Update(aUpdateInFlightFrameIndex,aUpdateFrameIndex);
end;

procedure TpvScene3DRendererPassesTemporalUpscalerComputePass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex,aFrameIndex:TpvSizeInt);
var ViewIndex,CountViews:TpvSizeInt;
    InFlightFrameState:TpvScene3DRendererInstance.PInFlightFrameState;
    Description:TpvScene3DRendererTemporalUpscalerBackend.TDispatchDescription;
    ImageMemoryBarrier:TVkImageMemoryBarrier;
    View:TpvScene3D.TView;
begin

 inherited Execute(aCommandBuffer,aInFlightFrameIndex,aFrameIndex);

 CountViews:=length(fBackends);

 InFlightFrameState:=@fInstance.InFlightFrameStates^[aInFlightFrameIndex];

 // The output goes into GENERAL for the backends to write it as a storage image, and back afterwards.
 // Some backends also clear it, such as DLSS does on a reset, so transfer writes are covered too.
 FillChar(ImageMemoryBarrier,SizeOf(TVkImageMemoryBarrier),#0);
 ImageMemoryBarrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
 ImageMemoryBarrier.srcAccessMask:=0;
 ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT);
 ImageMemoryBarrier.oldLayout:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
 ImageMemoryBarrier.newLayout:=VK_IMAGE_LAYOUT_GENERAL;
 ImageMemoryBarrier.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 ImageMemoryBarrier.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 ImageMemoryBarrier.image:=fResourceOutput.VulkanImages[aInFlightFrameIndex].Handle;
 ImageMemoryBarrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 ImageMemoryBarrier.subresourceRange.baseMipLevel:=0;
 ImageMemoryBarrier.subresourceRange.levelCount:=1;
 ImageMemoryBarrier.subresourceRange.baseArrayLayer:=0;
 ImageMemoryBarrier.subresourceRange.layerCount:=Max(1,CountViews);
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@ImageMemoryBarrier);

 if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
  fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.BeginBreadcrumb(aCommandBuffer.Handle,TpvVulkanBreadcrumbType.Dispatch,'TemporalUpscaler');
 end;

 for ViewIndex:=0 to CountViews-1 do begin

  if assigned(fBackends[ViewIndex]) then begin

   FillChar(Description,SizeOf(TpvScene3DRendererTemporalUpscalerBackend.TDispatchDescription),#0);

   // The jitter as the clip space offset the projection got, back into pixels of the render resolution.
   // The renderer applies it after the flip into Vulkan's clip space, so it is the shift of the picture
   // to the right and downwards, as the backends take it.
   Description.JitterOffset.x:=InFlightFrameState^.Jitter.x*fInstance.ScaledWidth*0.5;
   Description.JitterOffset.y:=InFlightFrameState^.Jitter.y*fInstance.ScaledHeight*0.5;

   // The velocity buffer holds the motion from the previous position to the current one in texture
   // coordinates, the backends want it the other way round and in pixels.
   Description.MotionVectorScale.x:=-fInstance.ScaledWidth;
   Description.MotionVectorScale.y:=-fInstance.ScaledHeight;

   Description.RenderWidth:=fInstance.ScaledWidth;
   Description.RenderHeight:=fInstance.ScaledHeight;

   View:=fInstance.Views[aInFlightFrameIndex].Items[InFlightFrameState^.FinalViewIndex+ViewIndex];
   Description.ProjectionMatrix:=View.ProjectionMatrix;
   Description.ViewMatrix:=View.ViewMatrix;

   Description.ZNear:=abs(fInstance.ZNear);
   Description.ZFar:=abs(fInstance.ZFar);

   if assigned(pvApplication) then begin
    Description.FrameTimeDelta:=pvApplication.DeltaTime;
   end else begin
    Description.FrameTimeDelta:=1.0/60.0;
   end;

   Description.PreExposure:=1.0;

   Description.Sharpness:=fInstance.Renderer.TemporalUpscalerSharpness;

   Description.Reset:=InFlightFrameState^.CameraReset;

   Description.DebugView:=fInstance.Renderer.TemporalUpscalerDebugView;

   fBackends[ViewIndex].Dispatch(aCommandBuffer,aInFlightFrameIndex,Description);

  end;

 end;

 if assigned(fInstance.Renderer.VulkanDevice.BreadcrumbBuffer) then begin
  fInstance.Renderer.VulkanDevice.BreadcrumbBuffer.EndBreadcrumb(aCommandBuffer.Handle);
 end;

 FillChar(ImageMemoryBarrier,SizeOf(TVkImageMemoryBarrier),#0);
 ImageMemoryBarrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
 ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT);
 ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT);
 ImageMemoryBarrier.oldLayout:=VK_IMAGE_LAYOUT_GENERAL;
 ImageMemoryBarrier.newLayout:=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
 ImageMemoryBarrier.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 ImageMemoryBarrier.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 ImageMemoryBarrier.image:=fResourceOutput.VulkanImages[aInFlightFrameIndex].Handle;
 ImageMemoryBarrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 ImageMemoryBarrier.subresourceRange.baseMipLevel:=0;
 ImageMemoryBarrier.subresourceRange.levelCount:=1;
 ImageMemoryBarrier.subresourceRange.baseArrayLayer:=0;
 ImageMemoryBarrier.subresourceRange.layerCount:=Max(1,CountViews);
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@ImageMemoryBarrier);

end;

end.
