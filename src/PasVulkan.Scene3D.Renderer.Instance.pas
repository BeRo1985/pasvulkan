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
unit PasVulkan.Scene3D.Renderer.Instance;
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

uses Classes,
     SysUtils,
     Math,
     PasMP,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Resources,
     PasVulkan.FrameGraph,
     PasVulkan.TimerQuery,
     PasVulkan.Collections,
     PasVulkan.CircularDoublyLinkedList,
     PasVulkan.VirtualReality,
     PasVulkan.Scene3D,
     PasVulkan.Scene3D.Renderer.Globals,
     PasVulkan.Scene3D.Renderer,
     PasVulkan.Scene3D.Renderer.MipmappedArray2DImage,
     PasVulkan.Scene3D.Renderer.OrderIndependentTransparencyBuffer,
     PasVulkan.Scene3D.Renderer.OrderIndependentTransparencyImage;

type { TpvScene3DRendererInstance }
     TpvScene3DRendererInstance=class(TpvScene3DRendererBaseObject)
      public
       const CountCascadedShadowMapCascades=4;
             CountOrderIndependentTransparencyLayers=8;
       type { TInFlightFrameState }
            TInFlightFrameState=record
             Ready:TPasMPBool32;
             FinalViewIndex:TpvSizeInt;
             CountViews:TpvSizeInt;
             CascadedShadowMapViewIndex:TpvSizeInt;
             CountCascadedShadowMapViews:TpvSizeInt;
            end;
            PInFlightFrameState=^TInFlightFrameState;
            TInFlightFrameStates=array[0..MaxInFlightFrames+1] of TInFlightFrameState;
            PInFlightFrameStates=^TInFlightFrameStates;
            { TCascadedShadowMap }
            TCascadedShadowMap=record
             public
              View:TpvScene3D.TView;
              CombinedMatrix:TpvMatrix4x4;
              SplitDepths:TpvVector2;
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
             SplitDepthsScales:array[0..CountCascadedShadowMapCascades-1] of TpvVector4;
             ConstantBiasNormalBiasSlopeBiasClamp:array[0..CountCascadedShadowMapCascades-1] of TpvVector4;
             MetaData:array[0..3] of TpvUInt32;
            end;
            PCascadedShadowMapUniformBuffer=^TCascadedShadowMapUniformBuffer;
            TCascadedShadowMapUniformBuffers=array[0..MaxInFlightFrames-1] of TCascadedShadowMapUniformBuffer;
            TCascadedShadowMapVulkanUniformBuffers=array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
      private
       fFrameGraph:TpvFrameGraph;
       fVirtualReality:TpvVirtualReality;
       fExternalOutputImageData:TpvFrameGraph.TExternalImageData;
       fCascadedShadowMapWidth:TpvInt32;
       fCascadedShadowMapHeight:TpvInt32;
       fCountSurfaceViews:TpvInt32;
       fSurfaceMultiviewMask:TpvUInt32;
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fFOV:TpvFloat;
       fZNear:TpvFloat;
       fZFar:TpvFloat;
       fCameraMatrix:TpvMatrix4x4;
       fPointerToCameraMatrix:PpvMatrix4x4;
       fInFlightFrameStates:TInFlightFrameStates;
       fPointerToInFlightFrameStates:PInFlightFrameStates;
      private
       fVulkanFlushQueue:TpvVulkanQueue;
       fVulkanFlushCommandPool:TpvVulkanCommandPool;
       fVulkanFlushCommandBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanCommandBuffer;
       fVulkanFlushCommandBufferFences:array[0..MaxInFlightFrames-1] of TpvVulkanFence;
       fVulkanFlushSemaphores:array[0..MaxInFlightFrames-1] of TpvVulkanSemaphore;
       fVulkanRenderSemaphores:array[0..MaxInFlightFrames-1] of TpvVulkanSemaphore;
      private
       fInFlightFrameCascadedShadowMaps:TInFlightFrameCascadedShadowMaps;
       fCascadedShadowMapUniformBuffers:TCascadedShadowMapUniformBuffers;
       fCascadedShadowMapVulkanUniformBuffers:TCascadedShadowMapVulkanUniformBuffers;
      private
       fCountLockOrderIndependentTransparencyLayers:TpvInt32;
       fLockOrderIndependentTransparentUniformBuffer:TLockOrderIndependentTransparentUniformBuffer;
       fLockOrderIndependentTransparentUniformVulkanBuffer:TpvVulkanBuffer;
       fLockOrderIndependentTransparencyABufferBuffers:array[0..MaxInFlightFrames-1] of TpvScene3DRendererOrderIndependentTransparencyBuffer;
       fLockOrderIndependentTransparencyAuxImages:array[0..MaxInFlightFrames-1] of TpvScene3DRendererOrderIndependentTransparencyImage;
       fLockOrderIndependentTransparencySpinLockImages:array[0..MaxInFlightFrames-1] of TpvScene3DRendererOrderIndependentTransparencyImage;
      private
       fCountLoopOrderIndependentTransparencyLayers:TpvInt32;
       fLoopOrderIndependentTransparentUniformBuffer:TLoopOrderIndependentTransparentUniformBuffer;
       fLoopOrderIndependentTransparentUniformVulkanBuffer:TpvVulkanBuffer;
       fLoopOrderIndependentTransparencyABufferBuffers:array[0..MaxInFlightFrames-1] of TpvScene3DRendererOrderIndependentTransparencyBuffer;
       fLoopOrderIndependentTransparencyZBufferBuffers:array[0..MaxInFlightFrames-1] of TpvScene3DRendererOrderIndependentTransparencyBuffer;
       fLoopOrderIndependentTransparencySBufferBuffers:array[0..MaxInFlightFrames-1] of TpvScene3DRendererOrderIndependentTransparencyBuffer;
      private
       fApproximationOrderIndependentTransparentUniformBuffer:TApproximationOrderIndependentTransparentUniformBuffer;
       fApproximationOrderIndependentTransparentUniformVulkanBuffer:TpvVulkanBuffer;
      private
       fDepthMipmappedArray2DImages:array[0..MaxInFlightFrames-1] of TpvScene3DRendererMipmappedArray2DImage;
       fForwardMipmappedArray2DImages:array[0..MaxInFlightFrames-1] of TpvScene3DRendererMipmappedArray2DImage;
      private
       fCascadedShadowMapInverseProjectionMatrices:array[0..7] of TpvMatrix4x4;
       fCascadedShadowMapViewSpaceFrustumCorners:array[0..7,0..7] of TpvVector3;
       procedure CalculateCascadedShadowMaps(const aInFlightFrameIndex:TpvInt32);
      public
       constructor Create(const aParent:TpvScene3DRendererBaseObject;const aVirtualReality:TpvVirtualReality=nil); reintroduce;
       destructor Destroy; override;
       procedure Prepare;
       procedure AllocateResources;
       procedure ReleaseResources;
       procedure Update(const aInFlightFrameIndex:TpvInt32);
      public
       property CameraMatrix:PpvMatrix4x4 read fPointerToCameraMatrix;
       property InFlightFrameStates:PInFlightFrameStates read fPointerToInFlightFrameStates;
      published
       property FrameGraph:TpvFrameGraph read fFrameGraph;
       property VirtualReality:TpvVirtualReality read fVirtualReality;
       property ExternalOutputImageData:TpvFrameGraph.TExternalImageData read fExternalOutputImageData;
       property CascadedShadowMapWidth:TpvInt32 read fCascadedShadowMapWidth write fCascadedShadowMapWidth;
       property CascadedShadowMapHeight:TpvInt32 read fCascadedShadowMapHeight write fCascadedShadowMapHeight;
       property Width:TpvInt32 read fWidth write fWidth;
       property Height:TpvInt32 read fHeight write fHeight;
       property CountSurfaceViews:TpvInt32 read fCountSurfaceViews write fCountSurfaceViews;
       property SurfaceMultiviewMask:TpvUInt32 read fSurfaceMultiviewMask write fSurfaceMultiviewMask;
       property FOV:TpvFloat read fFOV write fFOV;
       property ZNear:TpvFloat read fZNear write fZNear;
       property ZFar:TpvFloat read fZFar write fZFar;
     end;

implementation

{ TpvScene3DRendererInstance }

constructor TpvScene3DRendererInstance.Create(const aParent:TpvScene3DRendererBaseObject;const aVirtualReality:TpvVirtualReality=nil);
var InFlightFrameIndex:TpvSizeInt;
begin
 inherited Create(aParent);

 fVirtualReality:=aVirtualReality;

 if assigned(fVirtualReality) then begin

  fFOV:=fVirtualReality.FOV;

  fZNear:=fVirtualReality.ZNear;

  fZFar:=fVirtualReality.ZFar;

  fCountSurfaceViews:=fVirtualReality.CountImages;

  fSurfaceMultiviewMask:=fVirtualReality.MultiviewMask;

 end else begin

  fFOV:=53.13010235415598;

  fZNear:=-0.01;

  fZFar:=-Infinity;

  fCountSurfaceViews:=1;

  fSurfaceMultiviewMask:=1 shl 0;

 end;

 fCascadedShadowMapWidth:=Renderer.ShadowMapSize;

 fCascadedShadowMapHeight:=Renderer.ShadowMapSize;

 fCameraMatrix:=TpvMatrix4x4.Identity;

 fPointerToCameraMatrix:=@fCameraMatrix;

 fPointerToInFlightFrameStates:=@fInFlightFrameStates;

 fFrameGraph:=TpvFrameGraph.Create(Renderer.VulkanDevice,Renderer.CountInFlightFrames);

 FillChar(fInFlightFrameStates,SizeOf(TInFlightFrameStates),#0);

 fVulkanFlushQueue:=Renderer.VulkanDevice.UniversalQueue;

 fVulkanFlushCommandPool:=TpvVulkanCommandPool.Create(Renderer.VulkanDevice,
                                                      Renderer.VulkanDevice.UniversalQueueFamilyIndex,
                                                      TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

 for InFlightFrameIndex:=0 to Renderer.CountInFlightFrames-1 do begin

  fVulkanFlushCommandBuffers[InFlightFrameIndex]:=TpvVulkanCommandBuffer.Create(fVulkanFlushCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

  fVulkanFlushCommandBufferFences[InFlightFrameIndex]:=TpvVulkanFence.Create(Renderer.VulkanDevice);

  fVulkanFlushSemaphores[InFlightFrameIndex]:=TpvVulkanSemaphore.Create(Renderer.VulkanDevice);

  fVulkanRenderSemaphores[InFlightFrameIndex]:=TpvVulkanSemaphore.Create(Renderer.VulkanDevice);

 end;

 FillChar(fCascadedShadowMapVulkanUniformBuffers,SizeOf(TCascadedShadowMapVulkanUniformBuffers),#0);

 for InFlightFrameIndex:=0 to Renderer.CountInFlightFrames-1 do begin
  fCascadedShadowMapVulkanUniformBuffers[InFlightFrameIndex]:=TpvVulkanBuffer.Create(Renderer.VulkanDevice,
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
                                                                                     [TpvVulkanBufferFlag.PersistentMapped]);
 end;

 case Renderer.TransparencyMode of
  TpvScene3DRendererTransparencyMode.SPINLOCKOIT,
  TpvScene3DRendererTransparencyMode.INTERLOCKOIT:begin
   fLockOrderIndependentTransparentUniformVulkanBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                                               SizeOf(TLockOrderIndependentTransparentUniformBuffer),
                                                                               TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT),
                                                                               TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                               [],
                                                                               TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                               0,
                                                                               0,
                                                                               TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                               0,
                                                                               0,
                                                                               0,
                                                                               0,
                                                                               []);

  end;
  TpvScene3DRendererTransparencyMode.LOOPOIT:begin
   fLoopOrderIndependentTransparentUniformVulkanBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                                               SizeOf(TLoopOrderIndependentTransparentUniformBuffer),
                                                                               TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT),
                                                                               TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                               [],
                                                                               TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                               0,
                                                                               0,
                                                                               TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                               0,
                                                                               0,
                                                                               0,
                                                                               0,
                                                                               []);

  end;
  TpvScene3DRendererTransparencyMode.WBOIT,
  TpvScene3DRendererTransparencyMode.MBOIT:begin
   fApproximationOrderIndependentTransparentUniformVulkanBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                                                        SizeOf(TApproximationOrderIndependentTransparentUniformBuffer),
                                                                                        TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT),
                                                                                        TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                                        [],
                                                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                                        0,
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

end;

destructor TpvScene3DRendererInstance.Destroy;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fFrameGraph);

 for InFlightFrameIndex:=0 to Renderer.CountInFlightFrames-1 do begin

  FreeAndNil(fVulkanRenderSemaphores[InFlightFrameIndex]);

  FreeAndNil(fVulkanFlushCommandBuffers[InFlightFrameIndex]);

  FreeAndNil(fVulkanFlushCommandBufferFences[InFlightFrameIndex]);

  FreeAndNil(fVulkanFlushSemaphores[InFlightFrameIndex]);

 end;

 FreeAndNil(fVulkanFlushCommandPool);

 for InFlightFrameIndex:=0 to Renderer.CountInFlightFrames-1 do begin
  FreeAndNil(fCascadedShadowMapVulkanUniformBuffers[InFlightFrameIndex]);
 end;

 case Renderer.TransparencyMode of
  TpvScene3DRendererTransparencyMode.SPINLOCKOIT,
  TpvScene3DRendererTransparencyMode.INTERLOCKOIT:begin
   FreeAndNil(fLockOrderIndependentTransparentUniformVulkanBuffer);
  end;
  TpvScene3DRendererTransparencyMode.LOOPOIT:begin
   FreeAndNil(fLoopOrderIndependentTransparentUniformVulkanBuffer);
  end;
  TpvScene3DRendererTransparencyMode.WBOIT,
  TpvScene3DRendererTransparencyMode.MBOIT:begin
   FreeAndNil(fApproximationOrderIndependentTransparentUniformVulkanBuffer);
  end;
  else begin
  end;
 end;

 inherited Destroy;
end;

procedure TpvScene3DRendererInstance.Prepare;
begin

 if assigned(fVirtualReality) then begin

  fExternalOutputImageData:=TpvFrameGraph.TExternalImageData.Create(fFrameGraph);

  fFrameGraph.AddImageResourceType('resourcetype_output_color',
                                   true,
                                   fVirtualReality.ImageFormat,
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
                                  Renderer.SurfaceSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_msaa_color_optimized_non_alpha',
                                  false,
                                  Renderer.OptimizedNonAlphaFormat,
                                  Renderer.SurfaceSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_msaa_depth',
                                  false,
                                  VK_FORMAT_D32_SFLOAT{pvApplication.VulkanDepthImageFormat},
                                  Renderer.SurfaceSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.From(VK_FORMAT_D32_SFLOAT{pvApplication.VulkanDepthImageFormat}),
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_msaa_predepth',
                                  false,
                                  VK_FORMAT_R32_SFLOAT,
                                  Renderer.SurfaceSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_msaa_velocity',
                                  false,
                                  VK_FORMAT_R32G32_SFLOAT,
                                  Renderer.SurfaceSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_msaa_normals',
                                  false,
                                  VK_FORMAT_A2B10G10R10_UNORM_PACK32,
                                  Renderer.SurfaceSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_mboit_data',
                                  false,
                                  VK_FORMAT_R32G32B32A32_SFLOAT,
                                  Renderer.SurfaceSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_wboit_accumulation',
                                  false,
                                  VK_FORMAT_R16G16B16A16_SFLOAT,
                                  Renderer.SurfaceSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_wboit_revealage',
                                  false,
                                  VK_FORMAT_R32_SFLOAT,
                                  Renderer.SurfaceSampleCountFlagBits,
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_color_optimized_non_alpha',
                                  true,
                                  Renderer.OptimizedNonAlphaFormat,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_color_tonemapping',
                                  true,
                                  VK_FORMAT_R8G8B8A8_SRGB,//TVkFormat(TpvInt32(IfThen(Renderer.SurfaceSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),TpvInt32(VK_FORMAT_R8G8B8A8_SRGB),TpvInt32(VK_FORMAT_R8G8B8A8_UNORM)))),
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
 case Renderer.ShadowMode of

  TpvScene3DRendererShadowMode.MSM:begin

   fFrameGraph.AddImageResourceType('resourcetype_cascadedshadowmap_msaa_data',
                                    false,
                                    VK_FORMAT_R16G16B16A16_UNORM,
  //                                VK_FORMAT_R32G32B32A32_SFLOAT,
                                    Renderer.ShadowMapSampleCountFlagBits,
                                    TpvFrameGraph.TImageType.Color,
                                    TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.Absolute,fCascadedShadowMapWidth,fCascadedShadowMapHeight,1.0,CountCascadedShadowMapCascades),
                                    TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                    1
                                   );

   fFrameGraph.AddImageResourceType('resourcetype_cascadedshadowmap_msaa_depth',
                                    false,
                                    VK_FORMAT_D32_SFLOAT,
                                    Renderer.ShadowMapSampleCountFlagBits,
                                    TpvFrameGraph.TImageType.From(VK_FORMAT_D32_SFLOAT),
                                    TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.Absolute,fCascadedShadowMapWidth,fCascadedShadowMapHeight,1.0,CountCascadedShadowMapCascades),
                                    TVkImageUsageFlags(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                    1
                                   );

   fFrameGraph.AddImageResourceType('resourcetype_cascadedshadowmap_data',
                                    false,
                                    VK_FORMAT_R16G16B16A16_UNORM,
  //                                VK_FORMAT_R32G32B32A32_SFLOAT,
                                    TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                    TpvFrameGraph.TImageType.Color,
                                    TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.Absolute,fCascadedShadowMapWidth,fCascadedShadowMapHeight,1.0,CountCascadedShadowMapCascades),
                                    TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                    1
                                   );

   fFrameGraph.AddImageResourceType('resourcetype_cascadedshadowmap_depth',
                                    false,
                                    VK_FORMAT_D32_SFLOAT,
                                    TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                    TpvFrameGraph.TImageType.From(VK_FORMAT_D32_SFLOAT),
                                    TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.Absolute,fCascadedShadowMapWidth,fCascadedShadowMapHeight,1.0,CountCascadedShadowMapCascades),
                                    TVkImageUsageFlags(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                    1
                                   );

  end;

  else begin

   fFrameGraph.AddImageResourceType('resourcetype_cascadedshadowmap_data',
                                    false,
                                    VK_FORMAT_D32_SFLOAT,
                                    TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                    TpvFrameGraph.TImageType.From(VK_FORMAT_D32_SFLOAT),
                                    TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.Absolute,fCascadedShadowMapWidth,fCascadedShadowMapHeight,1.0,CountCascadedShadowMapCascades),
                                    TVkImageUsageFlags(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                    1
                                   );

  end;

 end;

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
end;

procedure TpvScene3DRendererInstance.AllocateResources;
var InFlightFrameIndex,Index:TpvSizeInt;
    UniversalQueue:TpvVulkanQueue;
    UniversalCommandPool:TpvVulkanCommandPool;
    UniversalCommandBuffer:TpvVulkanCommandBuffer;
    UniversalFence:TpvVulkanFence;
begin

 if assigned(fVirtualReality) then begin

  fWidth:=fVirtualReality.Width;

  fHeight:=fVirtualReality.Height;

 end else begin

  fWidth:=pvApplication.VulkanSwapChain.Width;

  fHeight:=pvApplication.VulkanSwapChain.Height;

 end;

 FillChar(fInFlightFrameStates,SizeOf(TInFlightFrameStates),#0);

 fFrameGraph.SetSwapChain(pvApplication.VulkanSwapChain,
                          pvApplication.VulkanDepthImageFormat);

 if assigned(fVirtualReality) then begin

  fFrameGraph.SurfaceWidth:=fWidth;
  fFrameGraph.SurfaceHeight:=fHeight;

  fExternalOutputImageData.VulkanImages.Clear;
  for Index:=0 to fVirtualReality.VulkanImages.Count-1 do begin
   fExternalOutputImageData.VulkanImages.Add(fVirtualReality.VulkanImages[Index]);
  end;

  (fFrameGraph.ResourceTypeByName['resourcetype_output_color'] as TpvFrameGraph.TImageResourceType).Format:=fVirtualReality.ImageFormat;

 end;

 UniversalQueue:=Renderer.VulkanDevice.UniversalQueue;
 try

  UniversalCommandPool:=TpvVulkanCommandPool.Create(Renderer.VulkanDevice,
                                                    Renderer.VulkanDevice.UniversalQueueFamilyIndex,
                                                    TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
  try

   UniversalCommandBuffer:=TpvVulkanCommandBuffer.Create(UniversalCommandPool,
                                                         VK_COMMAND_BUFFER_LEVEL_PRIMARY);
   try

    UniversalFence:=TpvVulkanFence.Create(Renderer.VulkanDevice);
    try

     for InFlightFrameIndex:=0 to Renderer.CountInFlightFrames-1 do begin
      fDepthMipmappedArray2DImages[InFlightFrameIndex]:=TpvScene3DRendererMipmappedArray2DImage.Create(fWidth,fHeight,fCountSurfaceViews,VK_FORMAT_R32_SFLOAT,false,VK_SAMPLE_COUNT_1_BIT,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
      fForwardMipmappedArray2DImages[InFlightFrameIndex]:=TpvScene3DRendererMipmappedArray2DImage.Create(fWidth,fHeight,fCountSurfaceViews,Renderer.OptimizedNonAlphaFormat,true,VK_SAMPLE_COUNT_1_BIT,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
     end;

     case Renderer.TransparencyMode of

      TpvScene3DRendererTransparencyMode.SPINLOCKOIT,
      TpvScene3DRendererTransparencyMode.INTERLOCKOIT:begin

       fCountLockOrderIndependentTransparencyLayers:=CountOrderIndependentTransparencyLayers;//Min(Max(CountOrderIndependentTransparencyLayers,fCountSurfaceMSAASamples),16);

       fLockOrderIndependentTransparentUniformBuffer.ViewPort.x:=fWidth;
       fLockOrderIndependentTransparentUniformBuffer.ViewPort.y:=fHeight;
       fLockOrderIndependentTransparentUniformBuffer.ViewPort.z:=fLockOrderIndependentTransparentUniformBuffer.ViewPort.x*fLockOrderIndependentTransparentUniformBuffer.ViewPort.y;
       fLockOrderIndependentTransparentUniformBuffer.ViewPort.w:=(fCountLockOrderIndependentTransparencyLayers and $ffff) or ((Renderer.CountSurfaceMSAASamples and $ffff) shl 16);

       fLockOrderIndependentTransparentUniformVulkanBuffer.UploadData(pvApplication.VulkanDevice.TransferQueue,
                                                                      UniversalCommandBuffer,
                                                                      UniversalFence,
                                                                      fLockOrderIndependentTransparentUniformBuffer,
                                                                      0,
                                                                      SizeOf(TLockOrderIndependentTransparentUniformBuffer));

       for InFlightFrameIndex:=0 to fFrameGraph.CountInFlightFrames-1 do begin

        fLockOrderIndependentTransparencyABufferBuffers[InFlightFrameIndex]:=TpvScene3DRendererOrderIndependentTransparencyBuffer.Create(fWidth*fHeight*fCountLockOrderIndependentTransparencyLayers*fCountSurfaceViews*(SizeOf(UInt32)*4),
                                                                                                                                         VK_FORMAT_R32G32B32A32_UINT,
                                                                                                                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT));

        fLockOrderIndependentTransparencyAuxImages[InFlightFrameIndex]:=TpvScene3DRendererOrderIndependentTransparencyImage.Create(fWidth,
                                                                                                                                   fHeight,
                                                                                                                                   fCountSurfaceViews,
                                                                                                                                   VK_FORMAT_R32_UINT,
                                                                                                                                   VK_SAMPLE_COUNT_1_BIT);

        if Renderer.TransparencyMode=TpvScene3DRendererTransparencyMode.SPINLOCKOIT then begin
         fLockOrderIndependentTransparencySpinLockImages[InFlightFrameIndex]:=TpvScene3DRendererOrderIndependentTransparencyImage.Create(fWidth,
                                                                                                                                         fHeight,
                                                                                                                                         fCountSurfaceViews,
                                                                                                                                         VK_FORMAT_R32_UINT,
                                                                                                                                         VK_SAMPLE_COUNT_1_BIT);
        end;

       end;

      end;

      TpvScene3DRendererTransparencyMode.LOOPOIT:begin

       fCountLoopOrderIndependentTransparencyLayers:=CountOrderIndependentTransparencyLayers;//Min(Max(CountOrderIndependentTransparencyLayers,fCountSurfaceMSAASamples),16);

       fLoopOrderIndependentTransparentUniformBuffer.ViewPort.x:=fWidth;
       fLoopOrderIndependentTransparentUniformBuffer.ViewPort.y:=fHeight;
       fLoopOrderIndependentTransparentUniformBuffer.ViewPort.z:=fLoopOrderIndependentTransparentUniformBuffer.ViewPort.x*fLoopOrderIndependentTransparentUniformBuffer.ViewPort.y;
       fLoopOrderIndependentTransparentUniformBuffer.ViewPort.w:=(fCountLoopOrderIndependentTransparencyLayers and $ffff) or ((Renderer.CountSurfaceMSAASamples and $ffff) shl 16);

       fLoopOrderIndependentTransparentUniformVulkanBuffer.UploadData(pvApplication.VulkanDevice.TransferQueue,
                                                                      UniversalCommandBuffer,
                                                                      UniversalFence,
                                                                      fLoopOrderIndependentTransparentUniformBuffer,
                                                                      0,
                                                                      SizeOf(TLoopOrderIndependentTransparentUniformBuffer));

       for InFlightFrameIndex:=0 to fFrameGraph.CountInFlightFrames-1 do begin

        fLoopOrderIndependentTransparencyABufferBuffers[InFlightFrameIndex]:=TpvScene3DRendererOrderIndependentTransparencyBuffer.Create(fWidth*fHeight*fCountLoopOrderIndependentTransparencyLayers*fCountSurfaceViews*(SizeOf(UInt32)*2),
                                                                                                                                         VK_FORMAT_R32G32_UINT,
                                                                                                                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT));

        fLoopOrderIndependentTransparencyZBufferBuffers[InFlightFrameIndex]:=TpvScene3DRendererOrderIndependentTransparencyBuffer.Create(fWidth*fHeight*fCountLoopOrderIndependentTransparencyLayers*fCountSurfaceViews*(SizeOf(UInt32)*1),
                                                                                                                                         VK_FORMAT_R32_UINT,
                                                                                                                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT));

        if Renderer.SurfaceSampleCountFlagBits<>TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
         fLoopOrderIndependentTransparencySBufferBuffers[InFlightFrameIndex]:=TpvScene3DRendererOrderIndependentTransparencyBuffer.Create(fWidth*fHeight*fCountLoopOrderIndependentTransparencyLayers*fCountSurfaceViews*(SizeOf(UInt32)*1),
                                                                                                                                          VK_FORMAT_R32_UINT,
                                                                                                                                          TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT));
        end else begin
         fLoopOrderIndependentTransparencySBufferBuffers[InFlightFrameIndex]:=nil;
        end;

       end;

      end;

      TpvScene3DRendererTransparencyMode.MBOIT,
      TpvScene3DRendererTransparencyMode.WBOIT:begin

       fApproximationOrderIndependentTransparentUniformBuffer.ZNearZFar.x:=abs(fZNear);
       fApproximationOrderIndependentTransparentUniformBuffer.ZNearZFar.y:=IfThen(IsInfinite(fZFar),4096.0,abs(fZFar));
       fApproximationOrderIndependentTransparentUniformBuffer.ZNearZFar.z:=ln(fApproximationOrderIndependentTransparentUniformBuffer.ZNearZFar.x);
       fApproximationOrderIndependentTransparentUniformBuffer.ZNearZFar.w:=ln(fApproximationOrderIndependentTransparentUniformBuffer.ZNearZFar.y);

       fApproximationOrderIndependentTransparentUniformVulkanBuffer.UploadData(pvApplication.VulkanDevice.TransferQueue,
                                                                               UniversalCommandBuffer,
                                                                               UniversalFence,
                                                                               fApproximationOrderIndependentTransparentUniformBuffer,
                                                                               0,
                                                                               SizeOf(TApproximationOrderIndependentTransparentUniformBuffer));

      end;

      else begin
      end;

     end;

    finally
     FreeAndNil(UniversalFence);
    end;

   finally
    FreeAndNil(UniversalCommandBuffer);
   end;

  finally
   FreeAndNil(UniversalCommandPool);
  end;

 finally
  UniversalQueue:=nil;
 end;

 fFrameGraph.AfterCreateSwapChain;

end;

procedure TpvScene3DRendererInstance.ReleaseResources;
var InFlightFrameIndex:TpvSizeInt;
begin

 fFrameGraph.BeforeDestroySwapChain;

 if assigned(fVirtualReality) then begin
  fExternalOutputImageData.VulkanImages.Clear;
 end;

 for InFlightFrameIndex:=0 to Renderer.CountInFlightFrames-1 do begin
  FreeAndNil(fDepthMipmappedArray2DImages[InFlightFrameIndex]);
  FreeAndNil(fForwardMipmappedArray2DImages[InFlightFrameIndex]);
 end;

 case Renderer.TransparencyMode of

  TpvScene3DRendererTransparencyMode.SPINLOCKOIT,
  TpvScene3DRendererTransparencyMode.INTERLOCKOIT:begin
   for InFlightFrameIndex:=0 to Renderer.CountInFlightFrames-1 do begin
    FreeAndNil(fLockOrderIndependentTransparencyABufferBuffers[InFlightFrameIndex]);
    FreeAndNil(fLockOrderIndependentTransparencyAuxImages[InFlightFrameIndex]);
    if Renderer.TransparencyMode=TpvScene3DRendererTransparencyMode.SPINLOCKOIT then begin
     FreeAndNil(fLockOrderIndependentTransparencySpinLockImages[InFlightFrameIndex]);
    end;
   end;
  end;

  TpvScene3DRendererTransparencyMode.LOOPOIT:begin
   for InFlightFrameIndex:=0 to Renderer.CountInFlightFrames-1 do begin
    FreeAndNil(fLoopOrderIndependentTransparencyABufferBuffers[InFlightFrameIndex]);
    FreeAndNil(fLoopOrderIndependentTransparencyZBufferBuffers[InFlightFrameIndex]);
    FreeAndNil(fLoopOrderIndependentTransparencySBufferBuffers[InFlightFrameIndex]);
   end;
  end;

  else begin
  end;

 end;

end;

procedure TpvScene3DRendererInstance.CalculateCascadedShadowMaps(const aInFlightFrameIndex:TpvInt32);
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
var CascadedShadowMapIndex,Index,ViewIndex:TpvSizeInt;
    CascadedShadowMaps:PCascadedShadowMaps;
    CascadedShadowMap:PCascadedShadowMap;
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
    InverseLightViewProjectionMatrix,
    ViewMatrix:TpvMatrix4x4;
    CascadedShadowMapSplitLambda,
    CascadedShadowMapSplitOverlap,
    MinZ,MaxZ,MinZExtents,MaxZExtents,ZMargin,
    Ratio,SplitValue,UniformSplitValue,LogSplitValue,
    FadeStartValue,LastValue,Value,TexelSizeAtOneMeter,
{$ifdef UseSphereBasedCascadedShadowMaps}
    Border,RoundedUpLightSpaceSphereRadius,
{$endif}
    zNear,zFar,RealZNear,RealZFar:TpvScalar;
    DoNeedRefitNearFarPlanes:boolean;
    InFlightFrameState:PInFlightFrameState;
begin

 SceneWorldSpaceBoundingBox:=Renderer.Scene3D.BoundingBox;

 SceneWorldSpaceSphere:=TpvSphere.CreateFromAABB(SceneWorldSpaceBoundingBox);

 InFlightFrameState:=@fInFlightFrameStates[aInFlightFrameIndex];

 if IsInfinite(fZFar) then begin
  RealZNear:=0.1;
  RealZFar:=1.0;
  for Index:=0 to InFlightFrameState^.CountViews-1 do begin
   ViewMatrix:=Renderer.Scene3D.Views.Items[InFlightFrameState^.FinalViewIndex+Index].ViewMatrix.SimpleInverse;
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

 for Index:=0 to InFlightFrameState^.CountViews-1 do begin
  ProjectionMatrix:=Renderer.Scene3D.Views.Items[InFlightFrameState^.FinalViewIndex+Index].ProjectionMatrix;
  if DoNeedRefitNearFarPlanes then begin
   ProjectionMatrix[2,2]:=RealZFar/(RealZNear-RealZFar);
   ProjectionMatrix[3,2]:=(-(RealZNear*RealZFar))/(RealZFar-RealZNear);
  end;
  fCascadedShadowMapInverseProjectionMatrices[Index]:=ProjectionMatrix.Inverse;
 end;

 LightForwardVector:=-Renderer.SkyCubeMap.LightDirection.xyz.Normalize;
 LightSideVector:=LightForwardVector.Perpendicular;
{LightSideVector:=TpvVector3.InlineableCreate(-Renderer.Scene3D.Views.Items[InFlightFrameState^.FinalViewIndex].ViewMatrix.RawComponents[0,2],
                                              -Renderer.Scene3D.Views.Items[InFlightFrameState^.FinalViewIndex].ViewMatrix.RawComponents[1,2],
                                              -Renderer.Scene3D.Views.Items[InFlightFrameState^.FinalViewIndex].ViewMatrix.RawComponents[2,2]).Normalize;
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

 for Index:=0 to InFlightFrameState^.CountViews-1 do begin
  ViewMatrix:=Renderer.Scene3D.Views.Items[InFlightFrameState^.FinalViewIndex+Index].ViewMatrix.SimpleInverse;
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

 for ViewIndex:=0 to InFlightFrameState^.CountViews-1 do begin
  for Index:=0 to 7 do begin
   fCascadedShadowMapViewSpaceFrustumCorners[ViewIndex,Index]:=fCascadedShadowMapInverseProjectionMatrices[ViewIndex].MulHomogen(TpvVector4.InlineableCreate(FrustumCorners[Index],1.0)).xyz;
  end;
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

  for ViewIndex:=0 to InFlightFrameState^.CountViews-1 do begin
   for Index:=0 to 7 do begin
    case Index of
     0..3:begin
      LightSpaceCorner:=fCascadedShadowMapViewSpaceFrustumCorners[ViewIndex,Index].Lerp(fCascadedShadowMapViewSpaceFrustumCorners[ViewIndex,Index+4],(MinZ-RealZNear)/(RealZFar-RealZNear));
     end;
     else {4..7:}begin
      LightSpaceCorner:=fCascadedShadowMapViewSpaceFrustumCorners[ViewIndex,Index-4].Lerp(fCascadedShadowMapViewSpaceFrustumCorners[ViewIndex,Index],(MaxZ-RealZNear)/(RealZFar-RealZNear));
     end;
    end;
    LightSpaceCorner:=FromViewSpaceToLightSpaceMatrixLeft*LightSpaceCorner;
    if (ViewIndex=0) and (Index=0) then begin
     LightSpaceAABB.Min:=LightSpaceCorner;
     LightSpaceAABB.Max:=LightSpaceCorner;
    end else begin
     LightSpaceAABB:=LightSpaceAABB.CombineVector3(LightSpaceCorner);
    end;
   end;
  end;

  if LightSpaceAABB.Intersect(SceneLightSpaceBoundingBox) then begin
   LightSpaceAABB:=LightSpaceAABB.GetIntersection(SceneLightSpaceBoundingBox);
  end;

  //LightSpaceAABB:=SceneLightSpaceBoundingBox;

  LightSpaceSphere:=TpvSphere.CreateFromAABB(LightSpaceAABB);
  LightSpaceSphere.Radius:=ceil(LightSpaceSphere.Radius*16)/16;
  LightSpaceAABB:=LightSpaceSphere.ToAABB;

  UnitsPerTexel:=(LightSpaceAABB.Max.xy-LightSpaceAABB.Min.xy)/TpvVector2.InlineableCreate(CascadedShadowMapWidth,CascadedShadowMapHeight);

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

  CascadedShadowMap^.View.ViewMatrix:=LightViewMatrix;
  CascadedShadowMap^.View.ProjectionMatrix:=LightProjectionMatrix;
  CascadedShadowMap^.View.InverseViewMatrix:=LightViewMatrix.Inverse;
  CascadedShadowMap^.View.InverseProjectionMatrix:=LightProjectionMatrix.Inverse;
  CascadedShadowMap^.CombinedMatrix:=LightViewProjectionMatrix;

  InverseLightViewProjectionMatrix:=LightViewProjectionMatrix.Inverse;

{ TexelSizeAtOneMeter:=Max(TpvVector3.InlineableCreate(InverseLightViewProjectionMatrix[0,0],InverseLightViewProjectionMatrix[0,1],InverseLightViewProjectionMatrix[0,2]).Length/CascadedShadowMapWidth,
                                   TpvVector3.InlineableCreate(InverseLightViewProjectionMatrix[1,0],InverseLightViewProjectionMatrix[1,1],InverseLightViewProjectionMatrix[1,2]).Length/CascadedShadowMapHeight);}
  TexelSizeAtOneMeter:=UnitsPerTexel.Length*SQRT_0_DOT_5*0.5;

  CascadedShadowMap^.Scales.x:=TexelSizeAtOneMeter;
  CascadedShadowMap^.Scales.y:=Max(4.0,(1.0*0.02)/TexelSizeAtOneMeter);

  fCascadedShadowMapUniformBuffers[aInFlightFrameIndex].Matrices[CascadedShadowMapIndex]:=LightViewProjectionMatrix;
  fCascadedShadowMapUniformBuffers[aInFlightFrameIndex].SplitDepthsScales[CascadedShadowMapIndex]:=TpvVector4.Create(CascadedShadowMap^.SplitDepths,CascadedShadowMap^.Scales.x,CascadedShadowMap^.Scales.y);
  fCascadedShadowMapUniformBuffers[aInFlightFrameIndex].ConstantBiasNormalBiasSlopeBiasClamp[CascadedShadowMapIndex]:=TpvVector4.Create(1e-3,1.0*TexelSizeAtOneMeter,5.0*TexelSizeAtOneMeter,0.0);
  fCascadedShadowMapUniformBuffers[aInFlightFrameIndex].MetaData[0]:=TpvUInt32(Renderer.ShadowMode);
  fCascadedShadowMapUniformBuffers[aInFlightFrameIndex].MetaData[1]:=0;
  fCascadedShadowMapUniformBuffers[aInFlightFrameIndex].MetaData[2]:=0;
  fCascadedShadowMapUniformBuffers[aInFlightFrameIndex].MetaData[3]:=0;

 end;

 InFlightFrameState^.CascadedShadowMapViewIndex:=Renderer.Scene3D.AddView(CascadedShadowMaps^[0].View);
 for CascadedShadowMapIndex:=1 to CountCascadedShadowMapCascades-1 do begin
  Renderer.Scene3D.AddView(CascadedShadowMaps^[CascadedShadowMapIndex].View);
 end;

 InFlightFrameState^.CountCascadedShadowMapViews:=CountCascadedShadowMapCascades;

end;

procedure TpvScene3DRendererInstance.Update(const aInFlightFrameIndex:TpvInt32);
var InFlightFrameState:PInFlightFrameState;
    ViewLeft,ViewRight:TpvScene3D.TView;
    ViewMatrix:TpvMatrix4x4;
begin

 InFlightFrameState:=@fInFlightFrameStates[aInFlightFrameIndex];

 ViewMatrix:=fCameraMatrix.SimpleInverse;

 if assigned(fVirtualReality) then begin

  ViewLeft.ViewMatrix:=ViewMatrix*fVirtualReality.GetPositionMatrix(0);
  ViewLeft.ProjectionMatrix:=fVirtualReality.GetProjectionMatrix(0);
  ViewLeft.InverseViewMatrix:=ViewLeft.ViewMatrix.Inverse;
  ViewLeft.InverseProjectionMatrix:=ViewLeft.ProjectionMatrix.Inverse;

  ViewRight.ViewMatrix:=ViewMatrix*fVirtualReality.GetPositionMatrix(1);
  ViewRight.ProjectionMatrix:=fVirtualReality.GetProjectionMatrix(1);
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

 case CountSurfaceViews of
  1:begin
   InFlightFrameState^.FinalViewIndex:=Renderer.Scene3D.AddView(ViewLeft);
   InFlightFrameState^.CountViews:=1;
  end;
  2:begin
   InFlightFrameState^.FinalViewIndex:=Renderer.Scene3D.AddViews([ViewLeft,ViewRight]);
   InFlightFrameState^.CountViews:=2;
  end;
{ 6:begin
   // TODO: Cube map
   InFlightFrameState^.CountViews:=6;
  end;}
  else begin
   Assert(false);
   InFlightFrameState^.CountViews:=0;
  end;
 end;

 CalculateCascadedShadowMaps(aInFlightFrameIndex);

 fCascadedShadowMapVulkanUniformBuffers[aInFlightFrameIndex].UpdateData(fCascadedShadowMapUniformBuffers[aInFlightFrameIndex],
                                                                        0,
                                                                        SizeOf(TCascadedShadowMapUniformBuffer));

end;

end.

