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
     PasVulkan.Scene3D.Renderer.CameraPreset,
     PasVulkan.Scene3D.Renderer,
     PasVulkan.Scene3D.Renderer.Array2DImage,
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
             ViewRenderPassIndex:TpvSizeInt;
             CascadedShadowMapRenderPassIndex:TpvSizeInt;
             ZNear:TpvFloat;
             ZFar:TpvFloat;
             Jitter:TpvVector4;
            end;
            PInFlightFrameState=^TInFlightFrameState;
            TInFlightFrameStates=array[0..MaxInFlightFrames+1] of TInFlightFrameState;
            PInFlightFrameStates=^TInFlightFrameStates;
            TLightGridPushConstants=packed record
             public
              TileSizeX:TpvUInt32;
              TileSizeY:TpvUInt32;
              ZNear:TpvFloat;
              ZFar:TpvFloat;
              ////
              ViewRect:TpvVector4;
              ////
              CountLights:TpvUInt32;
              ViewIndex:TpvUInt32;
              Size:TpvUInt32;
              OffsetedViewIndex:TpvUInt32;
              ////
              ClusterSizeX:TpvUInt32;
              ClusterSizeY:TpvUInt32;
              ClusterSizeZ:TpvUInt32;
              Reversed0:TpvUInt32;
              //
              ZScale:TpvFloat;
              ZBias:TpvFloat;
              ZMax:TpvFloat;
              Reversed1:TpvUInt32;
            end;
            PLightGridPushConstants=^TLightGridPushConstants;
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
            TVulkanBuffers=array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
            TArray2DImages=array[0..MaxInFlightFrames-1] of TpvScene3DRendererArray2DImage;
            TMipmappedArray2DImages=array[0..MaxInFlightFrames-1] of TpvScene3DRendererMipmappedArray2DImage;
            TOrderIndependentTransparencyBuffers=array[0..MaxInFlightFrames-1] of TpvScene3DRendererOrderIndependentTransparencyBuffer;
            TOrderIndependentTransparencyImages=array[0..MaxInFlightFrames-1] of TpvScene3DRendererOrderIndependentTransparencyImage;
            { TMeshFragmentSpecializationConstants }
            TMeshFragmentSpecializationConstants=record
             public
              UseReversedZ:TVkBool32;
              procedure SetPipelineShaderStage(const aVulkanPipelineShaderStage:TpvVulkanPipelineShaderStage);
            end;
      private
       fFrameGraph:TpvFrameGraph;
       fVirtualReality:TpvVirtualReality;
       fExternalImageFormat:TVkFormat;
       fExternalOutputImageData:TpvFrameGraph.TExternalImageData;
       fHasExternalOutputImage:boolean;
       fCascadedShadowMapWidth:TpvInt32;
       fCascadedShadowMapHeight:TpvInt32;
       fCountSurfaceViews:TpvInt32;
       fSurfaceMultiviewMask:TpvUInt32;
       fLeft:TpvInt32;
       fTop:TpvInt32;
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fLightGridSizeX:TpvInt32;
       fLightGridSizeY:TpvInt32;
       fLightGridSizeZ:TpvInt32;
       fLightGridTileSizeX:TpvInt32;
       fLightGridTileSizeY:TpvInt32;
       fFOV:TpvFloat;
       fZNear:TpvFloat;
       fZFar:TpvFloat;
       fCameraMatrix:TpvMatrix4x4;
       fPointerToCameraMatrix:PpvMatrix4x4;
       fInFlightFrameStates:TInFlightFrameStates;
       fPointerToInFlightFrameStates:PInFlightFrameStates;
       fMeshFragmentSpecializationConstants:TMeshFragmentSpecializationConstants;
       fCameraPreset:TpvScene3DRendererCameraPreset;
      private
       fViews:TpvScene3D.TViews;
      private
       fVulkanRenderSemaphores:array[0..MaxInFlightFrames-1] of TpvVulkanSemaphore;
      private
       fNearestFarthestDepthVulkanBuffers:TVulkanBuffers;
       fDepthOfFieldAutoFocusVulkanBuffers:TVulkanBuffers;
       fDepthOfFieldBokenShapeTapVulkanBuffers:TVulkanBuffers;
      private
       fLightGridPushConstants:TpvScene3DRendererInstance.TLightGridPushConstants;
       fLightGridGlobalsVulkanBuffers:TVulkanBuffers;
       fLightGridClusterAABBVulkanBuffers:TVulkanBuffers;
       fLightGridIndexListCounterVulkanBuffers:TVulkanBuffers;
       fLightGridIndexListVulkanBuffers:TVulkanBuffers;
       fLightGridClustersVulkanBuffers:TVulkanBuffers;
      private
       fInFlightFrameCascadedShadowMaps:TInFlightFrameCascadedShadowMaps;
       fCascadedShadowMapUniformBuffers:TCascadedShadowMapUniformBuffers;
       fCascadedShadowMapVulkanUniformBuffers:TCascadedShadowMapVulkanUniformBuffers;
      private
       fCountLockOrderIndependentTransparencyLayers:TpvInt32;
       fLockOrderIndependentTransparentUniformBuffer:TLockOrderIndependentTransparentUniformBuffer;
       fLockOrderIndependentTransparentUniformVulkanBuffer:TpvVulkanBuffer;
       fLockOrderIndependentTransparencyABufferBuffers:TOrderIndependentTransparencyBuffers;
       fLockOrderIndependentTransparencyAuxImages:TOrderIndependentTransparencyImages;
       fLockOrderIndependentTransparencySpinLockImages:TOrderIndependentTransparencyImages;
      private
       fCountLoopOrderIndependentTransparencyLayers:TpvInt32;
       fLoopOrderIndependentTransparentUniformBuffer:TLoopOrderIndependentTransparentUniformBuffer;
       fLoopOrderIndependentTransparentUniformVulkanBuffer:TpvVulkanBuffer;
       fLoopOrderIndependentTransparencyABufferBuffers:TOrderIndependentTransparencyBuffers;
       fLoopOrderIndependentTransparencyZBufferBuffers:TOrderIndependentTransparencyBuffers;
       fLoopOrderIndependentTransparencySBufferBuffers:TOrderIndependentTransparencyBuffers;
      private
       fApproximationOrderIndependentTransparentUniformBuffer:TApproximationOrderIndependentTransparentUniformBuffer;
       fApproximationOrderIndependentTransparentUniformVulkanBuffer:TpvVulkanBuffer;
      private
       fDepthMipmappedArray2DImages:TMipmappedArray2DImages;
       fSceneMipmappedArray2DImages:TMipmappedArray2DImages;
      private
       fCascadedShadowMapInverseProjectionMatrices:array[0..7] of TpvMatrix4x4;
       fCascadedShadowMapViewSpaceFrustumCorners:array[0..7,0..7] of TpvVector3;
      private
       fTAAHistoryColorImages:TArray2DImages;
       fTAAHistoryDepthImages:TArray2DImages;
      public
       fTAAEvents:array[0..MaxInFlightFrames-1] of TpvVulkanEvent;
       fTAAEventReady:array[0..MaxInFlightFrames-1] of boolean;
      private
       fPasses:TObject;
       fLastOutputResource:TpvFrameGraph.TPass.TUsedImageResource;
       procedure CalculateCascadedShadowMaps(const aInFlightFrameIndex:TpvInt32);
      public
       constructor Create(const aParent:TpvScene3DRendererBaseObject;const aVirtualReality:TpvVirtualReality=nil;const aExternalImageFormat:TVkFormat=VK_FORMAT_UNDEFINED); reintroduce;
       destructor Destroy; override;
       procedure Prepare;
       procedure AcquirePersistentResources;
       procedure ReleasePersistentResources;
       procedure AcquireVolatileResources;
       procedure ReleaseVolatileResources;
       procedure Update(const aInFlightFrameIndex:TpvInt32;const aFrameCounter:TpvInt64);
       procedure Reset;
       procedure AddView(const aView:TpvScene3D.TView);
       procedure AddViews(const aViews:array of TpvScene3D.TView);
       function GetJitterOffset(const aFrameCounter:TpvInt64):TpvVector2;
       function AddTemporalAntialiasingJitter(const aProjectionMatrix:TpvMatrix4x4;const aFrameCounter:TpvInt64):TpvMatrix4x4;
       procedure DrawUpdate(const aInFlightFrameIndex:TpvInt32;const aFrameCounter:TpvInt64);
       procedure Draw(const aSwapChainImageIndex,aInFlightFrameIndex:TpvInt32;const aFrameCounter:TpvInt64;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
      public
       property CameraMatrix:TpvMatrix4x4 read fCameraMatrix write fCameraMatrix;
       property PointerToCameraMatrix:PpvMatrix4x4 read fPointerToCameraMatrix;
       property InFlightFrameStates:PInFlightFrameStates read fPointerToInFlightFrameStates;
       property Views:TpvScene3D.TViews read fViews;
       property MeshFragmentSpecializationConstants:TMeshFragmentSpecializationConstants read fMeshFragmentSpecializationConstants;
      published
       property CameraPreset:TpvScene3DRendererCameraPreset read fCameraPreset;
      public
       property NearestFarthestDepthVulkanBuffers:TVulkanBuffers read fNearestFarthestDepthVulkanBuffers;
       property DepthOfFieldAutoFocusVulkanBuffers:TVulkanBuffers read fDepthOfFieldAutoFocusVulkanBuffers;
       property DepthOfFieldBokenShapeTapVulkanBuffers:TVulkanBuffers read fDepthOfFieldBokenShapeTapVulkanBuffers;
      public
       property LightGridSizeX:TpvInt32 read fLightGridSizeX;
       property LightGridSizeY:TpvInt32 read fLightGridSizeY;
       property LightGridSizeZ:TpvInt32 read fLightGridSizeZ;
       property LightGridTileSizeX:TpvInt32 read fLightGridTileSizeX;
       property LightGridTileSizeY:TpvInt32 read fLightGridTileSizeY;
       property LightGridPushConstants:TpvScene3DRendererInstance.TLightGridPushConstants read fLightGridPushConstants;
       property LightGridGlobalsVulkanBuffers:TVulkanBuffers read fLightGridGlobalsVulkanBuffers;
       property LightGridClusterAABBVulkanBuffers:TVulkanBuffers read fLightGridClusterAABBVulkanBuffers;
       property LightGridIndexListCounterVulkanBuffers:TVulkanBuffers read fLightGridIndexListCounterVulkanBuffers;
       property LightGridIndexListVulkanBuffers:TVulkanBuffers read fLightGridIndexListVulkanBuffers;
       property LightGridClustersVulkanBuffers:TVulkanBuffers read fLightGridClustersVulkanBuffers;
      public
       property CascadedShadowMapUniformBuffers:TCascadedShadowMapUniformBuffers read fCascadedShadowMapUniformBuffers;
       property CascadedShadowMapVulkanUniformBuffers:TCascadedShadowMapVulkanUniformBuffers read fCascadedShadowMapVulkanUniformBuffers;
      public
       property CountLockOrderIndependentTransparencyLayers:TpvInt32 read fCountLockOrderIndependentTransparencyLayers;
       property LockOrderIndependentTransparentUniformBuffer:TLockOrderIndependentTransparentUniformBuffer read fLockOrderIndependentTransparentUniformBuffer;
       property LockOrderIndependentTransparentUniformVulkanBuffer:TpvVulkanBuffer read fLockOrderIndependentTransparentUniformVulkanBuffer;
       property LockOrderIndependentTransparencyABufferBuffers:TOrderIndependentTransparencyBuffers read fLockOrderIndependentTransparencyABufferBuffers;
       property LockOrderIndependentTransparencyAuxImages:TOrderIndependentTransparencyImages read fLockOrderIndependentTransparencyAuxImages;
       property LockOrderIndependentTransparencySpinLockImages:TOrderIndependentTransparencyImages read fLockOrderIndependentTransparencySpinLockImages;
      public
       property CountLoopOrderIndependentTransparencyLayers:TpvInt32 read fCountLoopOrderIndependentTransparencyLayers;
       property LoopOrderIndependentTransparentUniformBuffer:TLoopOrderIndependentTransparentUniformBuffer read fLoopOrderIndependentTransparentUniformBuffer;
       property LoopOrderIndependentTransparentUniformVulkanBuffer:TpvVulkanBuffer read fLoopOrderIndependentTransparentUniformVulkanBuffer;
       property LoopOrderIndependentTransparencyABufferBuffers:TOrderIndependentTransparencyBuffers read fLoopOrderIndependentTransparencyABufferBuffers;
       property LoopOrderIndependentTransparencyZBufferBuffers:TOrderIndependentTransparencyBuffers read fLoopOrderIndependentTransparencyZBufferBuffers;
       property LoopOrderIndependentTransparencySBufferBuffers:TOrderIndependentTransparencyBuffers read fLoopOrderIndependentTransparencySBufferBuffers;
      public
       property ApproximationOrderIndependentTransparentUniformBuffer:TApproximationOrderIndependentTransparentUniformBuffer read fApproximationOrderIndependentTransparentUniformBuffer;
       property ApproximationOrderIndependentTransparentUniformVulkanBuffer:TpvVulkanBuffer read fApproximationOrderIndependentTransparentUniformVulkanBuffer;
      public
       property DepthMipmappedArray2DImages:TMipmappedArray2DImages read fDepthMipmappedArray2DImages;
       property SceneMipmappedArray2DImages:TMipmappedArray2DImages read fSceneMipmappedArray2DImages;
      public
       property TAAHistoryColorImages:TArray2DImages read fTAAHistoryColorImages;
       property TAAHistoryDepthImages:TArray2DImages read fTAAHistoryDepthImages;
      public
       property LastOutputResource:TpvFrameGraph.TPass.TUsedImageResource read fLastOutputResource write fLastOutputResource;
      published
       property FrameGraph:TpvFrameGraph read fFrameGraph;
       property VirtualReality:TpvVirtualReality read fVirtualReality;
       property ExternalImageFormat:TVkFormat read fExternalImageFormat write fExternalImageFormat;
       property ExternalOutputImageData:TpvFrameGraph.TExternalImageData read fExternalOutputImageData;
       property HasExternalOutputImage:boolean read fHasExternalOutputImage;
       property CascadedShadowMapWidth:TpvInt32 read fCascadedShadowMapWidth write fCascadedShadowMapWidth;
       property CascadedShadowMapHeight:TpvInt32 read fCascadedShadowMapHeight write fCascadedShadowMapHeight;
       property Left:TpvInt32 read fLeft write fLeft;
       property Top:TpvInt32 read fTop write fTop;
       property Width:TpvInt32 read fWidth write fWidth;
       property Height:TpvInt32 read fHeight write fHeight;
       property CountSurfaceViews:TpvInt32 read fCountSurfaceViews write fCountSurfaceViews;
       property SurfaceMultiviewMask:TpvUInt32 read fSurfaceMultiviewMask write fSurfaceMultiviewMask;
       property FOV:TpvFloat read fFOV write fFOV;
       property ZNear:TpvFloat read fZNear write fZNear;
       property ZFar:TpvFloat read fZFar write fZFar;
     end;

implementation

uses PasVulkan.Scene3D.Renderer.Passes.MeshComputePass,
     PasVulkan.Scene3D.Renderer.Passes.DepthVelocityNormalsRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.DepthMipMapComputePass,
     PasVulkan.Scene3D.Renderer.Passes.DepthOfFieldAutoFocusComputePass,
     PasVulkan.Scene3D.Renderer.Passes.LightClusterGridBuildComputePass,
     PasVulkan.Scene3D.Renderer.Passes.LightClusterGridAssignComputePass,
     PasVulkan.Scene3D.Renderer.Passes.CascadedShadowMapRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.CascadedShadowMapResolveRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.CascadedShadowMapBlurRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.SSAORenderPass,
     PasVulkan.Scene3D.Renderer.Passes.SSAOBlurRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.ForwardRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.ForwardResolveRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.ForwardRenderMipMapComputePass,
     PasVulkan.Scene3D.Renderer.Passes.DirectTransparencyRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.DirectTransparencyResolveRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.LockOrderIndependentTransparencyClearCustomPass,
     PasVulkan.Scene3D.Renderer.Passes.LockOrderIndependentTransparencyRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.LockOrderIndependentTransparencyBarrierCustomPass,
     PasVulkan.Scene3D.Renderer.Passes.LockOrderIndependentTransparencyResolveRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.LoopOrderIndependentTransparencyClearCustomPass,
     PasVulkan.Scene3D.Renderer.Passes.LoopOrderIndependentTransparencyPass1RenderPass,
     PasVulkan.Scene3D.Renderer.Passes.LoopOrderIndependentTransparencyPass1BarrierCustomPass,
     PasVulkan.Scene3D.Renderer.Passes.LoopOrderIndependentTransparencyPass2RenderPass,
     PasVulkan.Scene3D.Renderer.Passes.LoopOrderIndependentTransparencyPass2BarrierCustomPass,
     PasVulkan.Scene3D.Renderer.Passes.LoopOrderIndependentTransparencyResolveRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.MomentBasedOrderIndependentTransparencyAbsorbanceRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.MomentBasedOrderIndependentTransparencyTransmittanceRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.MomentBasedOrderIndependentTransparencyResolveRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.WeightBlendedOrderIndependentTransparencyRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.WeightBlendedOrderIndependentTransparencyResolveRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.AntialiasingTAAPreCustomPass,
     PasVulkan.Scene3D.Renderer.Passes.AntialiasingTAARenderPass,
     PasVulkan.Scene3D.Renderer.Passes.AntialiasingTAAPostCustomPass,
     PasVulkan.Scene3D.Renderer.Passes.DepthOfFieldPrepareRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.DepthOfFieldBokehComputePass,
     PasVulkan.Scene3D.Renderer.Passes.DepthOfFieldPrefilterRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.DepthOfFieldBlurRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.DepthOfFieldBruteforceRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.DepthOfFieldPostBlurRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.DepthOfFieldCombineRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.DepthOfFieldGatherPass1RenderPass,
     PasVulkan.Scene3D.Renderer.Passes.DepthOfFieldGatherPass2RenderPass,
     PasVulkan.Scene3D.Renderer.Passes.DepthOfFieldResolveRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.LensDownsampleComputePass,
     PasVulkan.Scene3D.Renderer.Passes.LensUpsampleComputePass,
     PasVulkan.Scene3D.Renderer.Passes.LensResolveRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.TonemappingRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.AntialiasingNoneRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.AntialiasingDSAARenderPass,
     PasVulkan.Scene3D.Renderer.Passes.AntialiasingFXAARenderPass,
     PasVulkan.Scene3D.Renderer.Passes.AntialiasingSMAAEdgesRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.AntialiasingSMAAWeightsRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.AntialiasingSMAABlendRenderPass,
     PasVulkan.Scene3D.Renderer.Passes.DitheringRenderPass;

type TpvScene3DRendererInstancePasses=class
      private
       fMeshComputePass:TpvScene3DRendererPassesMeshComputePass;
       fDepthVelocityNormalsRenderPass:TpvScene3DRendererPassesDepthVelocityNormalsRenderPass;
       fDepthMipMapComputePass:TpvScene3DRendererPassesDepthMipMapComputePass;
       fDepthOfFieldAutoFocusComputePass:TpvScene3DRendererPassesDepthOfFieldAutoFocusComputePass;
       fLightClusterGridBuildComputePass:TpvScene3DRendererPassesLightClusterGridBuildComputePass;
       fLightClusterGridAssignComputePass:TpvScene3DRendererPassesLightClusterGridAssignComputePass;
       fCascadedShadowMapRenderPass:TpvScene3DRendererPassesCascadedShadowMapRenderPass;
       fCascadedShadowMapResolveRenderPass:TpvScene3DRendererPassesCascadedShadowMapResolveRenderPass;
       fCascadedShadowMapBlurRenderPasses:array[0..1] of TpvScene3DRendererPassesCascadedShadowMapBlurRenderPass;
       fSSAORenderPass:TpvScene3DRendererPassesSSAORenderPass;
       fSSAOBlurRenderPasses:array[0..1] of TpvScene3DRendererPassesSSAOBlurRenderPass;
       fForwardRenderPass:TpvScene3DRendererPassesForwardRenderPass;
       fForwardResolveRenderPass:TpvScene3DRendererPassesForwardResolveRenderPass;
       fForwardRenderMipMapComputePass:TpvScene3DRendererPassesForwardRenderMipMapComputePass;
       fDirectTransparencyRenderPass:TpvScene3DRendererPassesDirectTransparencyRenderPass;
       fDirectTransparencyResolveRenderPass:TpvScene3DRendererPassesDirectTransparencyResolveRenderPass;
       fLockOrderIndependentTransparencyClearCustomPass:TpvScene3DRendererPassesLockOrderIndependentTransparencyClearCustomPass;
       fLockOrderIndependentTransparencyRenderPass:TpvScene3DRendererPassesLockOrderIndependentTransparencyRenderPass;
       fLockOrderIndependentTransparencyBarrierCustomPass:TpvScene3DRendererPassesLockOrderIndependentTransparencyBarrierCustomPass;
       fLockOrderIndependentTransparencyResolveRenderPass:TpvScene3DRendererPassesLockOrderIndependentTransparencyResolveRenderPass;
       fLoopOrderIndependentTransparencyClearCustomPass:TpvScene3DRendererPassesLoopOrderIndependentTransparencyClearCustomPass;
       fLoopOrderIndependentTransparencyPass1RenderPass:TpvScene3DRendererPassesLoopOrderIndependentTransparencyPass1RenderPass;
       fLoopOrderIndependentTransparencyPass1BarrierCustomPass:TpvScene3DRendererPassesLoopOrderIndependentTransparencyPass1BarrierCustomPass;
       fLoopOrderIndependentTransparencyPass2RenderPass:TpvScene3DRendererPassesLoopOrderIndependentTransparencyPass2RenderPass;
       fLoopOrderIndependentTransparencyPass2BarrierCustomPass:TpvScene3DRendererPassesLoopOrderIndependentTransparencyPass2BarrierCustomPass;
       fLoopOrderIndependentTransparencyResolveRenderPass:TpvScene3DRendererPassesLoopOrderIndependentTransparencyResolveRenderPass;
       fWeightBlendedOrderIndependentTransparencyRenderPass:TpvScene3DRendererPassesWeightBlendedOrderIndependentTransparencyRenderPass;
       fWeightBlendedOrderIndependentTransparencyResolveRenderPass:TpvScene3DRendererPassesWeightBlendedOrderIndependentTransparencyResolveRenderPass;
       fMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass:TpvScene3DRendererPassesMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass;
       fMomentBasedOrderIndependentTransparencyTransmittanceRenderPass:TpvScene3DRendererPassesMomentBasedOrderIndependentTransparencyTransmittanceRenderPass;
       fMomentBasedOrderIndependentTransparencyResolveRenderPass:TpvScene3DRendererPassesMomentBasedOrderIndependentTransparencyResolveRenderPass;
       fAntialiasingTAAPreCustomPass:TpvScene3DRendererPassesAntialiasingTAAPreCustomPass;
       fAntialiasingTAARenderPass:TpvScene3DRendererPassesAntialiasingTAARenderPass;
       fAntialiasingTAAPostCustomPass:TpvScene3DRendererPassesAntialiasingTAAPostCustomPass;
       fDepthOfFieldPrepareRenderPass:TpvScene3DRendererPassesDepthOfFieldPrepareRenderPass;
       fDepthOfFieldBokehComputePass:TpvScene3DRendererPassesDepthOfFieldBokehComputePass;
       fDepthOfFieldPrefilterRenderPass:TpvScene3DRendererPassesDepthOfFieldPrefilterRenderPass;
       fDepthOfFieldBlurRenderPass:TpvScene3DRendererPassesDepthOfFieldBlurRenderPass;
       fDepthOfFieldBruteforceRenderPass:TpvScene3DRendererPassesDepthOfFieldBruteforceRenderPass;
       fDepthOfFieldPostBlurRenderPass:TpvScene3DRendererPassesDepthOfFieldPostBlurRenderPass;
       fDepthOfFieldCombineRenderPass:TpvScene3DRendererPassesDepthOfFieldCombineRenderPass;
       fDepthOfFieldGatherPass1RenderPass:TpvScene3DRendererPassesDepthOfFieldGatherPass1RenderPass;
       fDepthOfFieldGatherPass2RenderPass:TpvScene3DRendererPassesDepthOfFieldGatherPass2RenderPass;
       fDepthOfFieldResolveRenderPass:TpvScene3DRendererPassesDepthOfFieldResolveRenderPass;
       fLensDownsampleComputePass:TpvScene3DRendererPassesLensDownsampleComputePass;
       fLensUpsampleComputePass:TpvScene3DRendererPassesLensUpsampleComputePass;
       fLensResolveRenderPass:TpvScene3DRendererPassesLensResolveRenderPass;
       fTonemappingRenderPass:TpvScene3DRendererPassesTonemappingRenderPass;
       fAntialiasingNoneRenderPass:TpvScene3DRendererPassesAntialiasingNoneRenderPass;
       fAntialiasingDSAARenderPass:TpvScene3DRendererPassesAntialiasingDSAARenderPass;
       fAntialiasingFXAARenderPass:TpvScene3DRendererPassesAntialiasingFXAARenderPass;
       fAntialiasingSMAAEdgesRenderPass:TpvScene3DRendererPassesAntialiasingSMAAEdgesRenderPass;
       fAntialiasingSMAAWeightsRenderPass:TpvScene3DRendererPassesAntialiasingSMAAWeightsRenderPass;
       fAntialiasingSMAABlendRenderPass:TpvScene3DRendererPassesAntialiasingSMAABlendRenderPass;
       fDitheringRenderPass:TpvScene3DRendererPassesDitheringRenderPass;
     end;

const CountJitterOffsets=128;
      JitterOffsetMask=CountJitterOffsets-1;

var JitterOffsets:array[0..CountJitterOffsets-1] of TpvVector2;

{ TpvScene3DRendererInstance.TMeshFragmentSpecializationConstants }

procedure TpvScene3DRendererInstance.TMeshFragmentSpecializationConstants.SetPipelineShaderStage(const aVulkanPipelineShaderStage:TpvVulkanPipelineShaderStage);
begin
{aVulkanPipelineShaderStage.AddSpecializationMapEntry(0,TVkPtrUInt(pointer(@UseReversedZ))-TVkPtrUInt(pointer(@self)),SizeOf(TVkBool32));
 aVulkanPipelineShaderStage.AddSpecializationDataFromMemory(@self,SizeOf(TpvScene3DRendererInstance.TMeshFragmentSpecializationConstants),true);//}
end;

{ TpvScene3DRendererInstance }

constructor TpvScene3DRendererInstance.Create(const aParent:TpvScene3DRendererBaseObject;const aVirtualReality:TpvVirtualReality;const aExternalImageFormat:TVkFormat);
var InFlightFrameIndex:TpvSizeInt;
begin
 inherited Create(aParent);

 fPasses:=TpvScene3DRendererInstancePasses.Create;

 fExternalImageFormat:=aExternalImageFormat;

 fVirtualReality:=aVirtualReality;

 fCameraPreset:=TpvScene3DRendererCameraPreset.Create;

 fLightGridSizeX:=16;
 fLightGridSizeY:=16;
 fLightGridSizeZ:=16;

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

 fFrameGraph.SurfaceIsSwapchain:=(fExternalImageFormat=VK_FORMAT_UNDEFINED) and not assigned(fVirtualReality);

 if fFrameGraph.SurfaceIsSwapchain then begin
  fExternalOutputImageData:=nil;
 end else begin
  fExternalOutputImageData:=TpvFrameGraph.TExternalImageData.Create(fFrameGraph);
 end;

 fHasExternalOutputImage:=(fExternalImageFormat<>VK_FORMAT_UNDEFINED) and not assigned(fVirtualReality);

 fFrameGraph.DefaultResourceInstanceType:=TpvFrameGraph.TResourceInstanceType.InstancePerInFlightFrame;

 FillChar(fInFlightFrameStates,SizeOf(TInFlightFrameStates),#0);

 for InFlightFrameIndex:=0 to Renderer.CountInFlightFrames-1 do begin

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

 fLeft:=0;
 fTop:=0;
 fWidth:=1024;
 fHeight:=768;

 fMeshFragmentSpecializationConstants.UseReversedZ:=IfThen(fZFar<0.0,VK_TRUE,VK_FALSE);

end;

destructor TpvScene3DRendererInstance.Destroy;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fFrameGraph);

 for InFlightFrameIndex:=0 to Renderer.CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanRenderSemaphores[InFlightFrameIndex]);
 end;

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

 FreeAndNil(fPasses);

 FreeAndNil(fCameraPreset);

 inherited Destroy;
end;

procedure TpvScene3DRendererInstance.Prepare;
begin

 if assigned(fVirtualReality) then begin

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
                                  false,
                                  Renderer.OptimizedNonAlphaFormat,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_color_temporal_antialiasing',
                                  false,
                                  Renderer.OptimizedNonAlphaFormat,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_TRANSFER_SRC_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_color_tonemapping',
                                  false,
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
                                  false,
                                  VK_FORMAT_R16G16B16A16_SFLOAT,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_color_halfres',
                                  false,
                                  VK_FORMAT_R16G16B16A16_SFLOAT,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,0.5,0.5,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

{fFrameGraph.AddImageResourceType('resourcetype_color_posteffect',
                                  false,
                                  VK_FORMAT_R16G16B16A16_SFLOAT,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_color_posteffect_halfres',
                                  false,
                                  VK_FORMAT_R16G16B16A16_SFLOAT,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,0.5,0.5,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );}

 fFrameGraph.AddImageResourceType('resourcetype_color_antialiasing',
                                  false,
                                  VK_FORMAT_R8G8B8A8_SRGB,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_depth',
                                  false,
                                  VK_FORMAT_D32_SFLOAT{pvApplication.VulkanDepthImageFormat},
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.From(VK_FORMAT_D32_SFLOAT{pvApplication.VulkanDepthImageFormat}),
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT) or (IfThen(Renderer.AntialiasingMode=TpvScene3DRendererAntialiasingMode.TAA,TVkImageUsageFlags(VK_IMAGE_USAGE_TRANSFER_SRC_BIT),0)),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_predepth',
                                  false,
                                  VK_FORMAT_R32_SFLOAT,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_velocity',
                                  false,
                                  VK_FORMAT_R32G32_SFLOAT,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_normals',
                                  false,
                                  VK_FORMAT_A2B10G10R10_UNORM_PACK32,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_ssao',
                                  false,
                                  VK_FORMAT_R32G32_SFLOAT,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );

 fFrameGraph.AddImageResourceType('resourcetype_ssao_final',
                                  false,
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

{fFrameGraph.AddImageResourceType('resourcetype_depthoffield',
                                  false,
                                  VK_FORMAT_R16G16B16A16_SFLOAT,
                                  TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                  TpvFrameGraph.TImageType.Color,
                                  TpvFrameGraph.TImageSize.Create(TpvFrameGraph.TImageSize.TKind.SurfaceDependent,1.0,1.0,1.0,fCountSurfaceViews),
                                  TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT),
                                  1
                                 );}

 TpvScene3DRendererInstancePasses(fPasses).fMeshComputePass:=TpvScene3DRendererPassesMeshComputePass.Create(fFrameGraph,self);

 TpvScene3DRendererInstancePasses(fPasses).fDepthVelocityNormalsRenderPass:=TpvScene3DRendererPassesDepthVelocityNormalsRenderPass.Create(fFrameGraph,self);
 TpvScene3DRendererInstancePasses(fPasses).fDepthVelocityNormalsRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fMeshComputePass);

 TpvScene3DRendererInstancePasses(fPasses).fDepthMipMapComputePass:=TpvScene3DRendererPassesDepthMipMapComputePass.Create(fFrameGraph,self);
 TpvScene3DRendererInstancePasses(fPasses).fDepthMipMapComputePass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fDepthVelocityNormalsRenderPass);

 TpvScene3DRendererInstancePasses(fPasses).fLightClusterGridBuildComputePass:=TpvScene3DRendererPassesLightClusterGridBuildComputePass.Create(fFrameGraph,self);
 TpvScene3DRendererInstancePasses(fPasses).fLightClusterGridBuildComputePass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fDepthMipMapComputePass);

 TpvScene3DRendererInstancePasses(fPasses).fLightClusterGridAssignComputePass:=TpvScene3DRendererPassesLightClusterGridAssignComputePass.Create(fFrameGraph,self);
 TpvScene3DRendererInstancePasses(fPasses).fLightClusterGridAssignComputePass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fLightClusterGridBuildComputePass);

 case Renderer.ShadowMode of

  TpvScene3DRendererShadowMode.PCF,TpvScene3DRendererShadowMode.DPCF,TpvScene3DRendererShadowMode.PCSS:begin

   TpvScene3DRendererInstancePasses(fPasses).fCascadedShadowMapRenderPass:=TpvScene3DRendererPassesCascadedShadowMapRenderPass.Create(fFrameGraph,self);
   TpvScene3DRendererInstancePasses(fPasses).fCascadedShadowMapRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fMeshComputePass);
   TpvScene3DRendererInstancePasses(fPasses).fCascadedShadowMapRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fDepthVelocityNormalsRenderPass);
   TpvScene3DRendererInstancePasses(fPasses).fCascadedShadowMapRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fDepthMipMapComputePass);

  end;

  TpvScene3DRendererShadowMode.MSM:begin

   TpvScene3DRendererInstancePasses(fPasses).fCascadedShadowMapRenderPass:=TpvScene3DRendererPassesCascadedShadowMapRenderPass.Create(fFrameGraph,self);
   TpvScene3DRendererInstancePasses(fPasses).fCascadedShadowMapRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fMeshComputePass);
   TpvScene3DRendererInstancePasses(fPasses).fCascadedShadowMapRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fDepthVelocityNormalsRenderPass);
   TpvScene3DRendererInstancePasses(fPasses).fCascadedShadowMapRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fDepthMipMapComputePass);

   TpvScene3DRendererInstancePasses(fPasses).fCascadedShadowMapResolveRenderPass:=TpvScene3DRendererPassesCascadedShadowMapResolveRenderPass.Create(fFrameGraph,self);

   TpvScene3DRendererInstancePasses(fPasses).fCascadedShadowMapBlurRenderPasses[0]:=TpvScene3DRendererPassesCascadedShadowMapBlurRenderPass.Create(fFrameGraph,self,true);

   TpvScene3DRendererInstancePasses(fPasses).fCascadedShadowMapBlurRenderPasses[1]:=TpvScene3DRendererPassesCascadedShadowMapBlurRenderPass.Create(fFrameGraph,self,false);

  end;

  else begin

   Assert(false);

  end;

 end;

 TpvScene3DRendererInstancePasses(fPasses).fSSAORenderPass:=TpvScene3DRendererPassesSSAORenderPass.Create(fFrameGraph,self);
 TpvScene3DRendererInstancePasses(fPasses).fSSAORenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fDepthMipMapComputePass);

 TpvScene3DRendererInstancePasses(fPasses).fSSAOBlurRenderPasses[0]:=TpvScene3DRendererPassesSSAOBlurRenderPass.Create(fFrameGraph,self,true);

 TpvScene3DRendererInstancePasses(fPasses).fSSAOBlurRenderPasses[1]:=TpvScene3DRendererPassesSSAOBlurRenderPass.Create(fFrameGraph,self,false);

 TpvScene3DRendererInstancePasses(fPasses).fForwardRenderPass:=TpvScene3DRendererPassesForwardRenderPass.Create(fFrameGraph,self);
 TpvScene3DRendererInstancePasses(fPasses).fForwardRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fLightClusterGridAssignComputePass);
 TpvScene3DRendererInstancePasses(fPasses).fForwardRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fMeshComputePass);
 TpvScene3DRendererInstancePasses(fPasses).fForwardRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fDepthMipMapComputePass);
 TpvScene3DRendererInstancePasses(fPasses).fForwardRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fSSAOBlurRenderPasses[1]);

 if Renderer.SurfaceSampleCountFlagBits<>TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  TpvScene3DRendererInstancePasses(fPasses).fForwardResolveRenderPass:=TpvScene3DRendererPassesForwardResolveRenderPass.Create(fFrameGraph,self);
 end;

 TpvScene3DRendererInstancePasses(fPasses).fForwardRenderMipMapComputePass:=TpvScene3DRendererPassesForwardRenderMipMapComputePass.Create(fFrameGraph,self);

 case Renderer.TransparencyMode of

  TpvScene3DRendererTransparencyMode.Direct:begin

   TpvScene3DRendererInstancePasses(fPasses).fDirectTransparencyRenderPass:=TpvScene3DRendererPassesDirectTransparencyRenderPass.Create(fFrameGraph,self);
   TpvScene3DRendererInstancePasses(fPasses).fDirectTransparencyRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fMeshComputePass);
   TpvScene3DRendererInstancePasses(fPasses).fDirectTransparencyRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fDepthMipMapComputePass);
   TpvScene3DRendererInstancePasses(fPasses).fDirectTransparencyRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fForwardRenderMipMapComputePass);

   TpvScene3DRendererInstancePasses(fPasses).fDirectTransparencyResolveRenderPass:=TpvScene3DRendererPassesDirectTransparencyResolveRenderPass.Create(fFrameGraph,self);

  end;

  TpvScene3DRendererTransparencyMode.SPINLOCKOIT,
  TpvScene3DRendererTransparencyMode.INTERLOCKOIT:begin

   TpvScene3DRendererInstancePasses(fPasses).fLockOrderIndependentTransparencyClearCustomPass:=TpvScene3DRendererPassesLockOrderIndependentTransparencyClearCustomPass.Create(fFrameGraph,self);

   TpvScene3DRendererInstancePasses(fPasses).fLockOrderIndependentTransparencyRenderPass:=TpvScene3DRendererPassesLockOrderIndependentTransparencyRenderPass.Create(fFrameGraph,self);
   TpvScene3DRendererInstancePasses(fPasses).fLockOrderIndependentTransparencyRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fMeshComputePass);
   TpvScene3DRendererInstancePasses(fPasses).fLockOrderIndependentTransparencyRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fLockOrderIndependentTransparencyClearCustomPass);
   TpvScene3DRendererInstancePasses(fPasses).fLockOrderIndependentTransparencyRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fDepthMipMapComputePass);
   TpvScene3DRendererInstancePasses(fPasses).fLockOrderIndependentTransparencyRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fForwardRenderMipMapComputePass);

   TpvScene3DRendererInstancePasses(fPasses).fLockOrderIndependentTransparencyBarrierCustomPass:=TpvScene3DRendererPassesLockOrderIndependentTransparencyBarrierCustomPass.Create(fFrameGraph,self);
   TpvScene3DRendererInstancePasses(fPasses).fLockOrderIndependentTransparencyBarrierCustomPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fLockOrderIndependentTransparencyRenderPass);

   TpvScene3DRendererInstancePasses(fPasses).fLockOrderIndependentTransparencyResolveRenderPass:=TpvScene3DRendererPassesLockOrderIndependentTransparencyResolveRenderPass.Create(fFrameGraph,self);
   TpvScene3DRendererInstancePasses(fPasses).fLockOrderIndependentTransparencyResolveRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fLockOrderIndependentTransparencyBarrierCustomPass);

  end;

  TpvScene3DRendererTransparencyMode.LOOPOIT:begin

   TpvScene3DRendererInstancePasses(fPasses).fLoopOrderIndependentTransparencyClearCustomPass:=TpvScene3DRendererPassesLoopOrderIndependentTransparencyClearCustomPass.Create(fFrameGraph,self);

   TpvScene3DRendererInstancePasses(fPasses).fLoopOrderIndependentTransparencyPass1RenderPass:=TpvScene3DRendererPassesLoopOrderIndependentTransparencyPass1RenderPass.Create(fFrameGraph,self);
   TpvScene3DRendererInstancePasses(fPasses).fLoopOrderIndependentTransparencyPass1RenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fMeshComputePass);
   TpvScene3DRendererInstancePasses(fPasses).fLoopOrderIndependentTransparencyPass1RenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fLoopOrderIndependentTransparencyClearCustomPass);
   TpvScene3DRendererInstancePasses(fPasses).fLoopOrderIndependentTransparencyPass1RenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fDepthMipMapComputePass);
   TpvScene3DRendererInstancePasses(fPasses).fLoopOrderIndependentTransparencyPass1RenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fForwardRenderMipMapComputePass);

   TpvScene3DRendererInstancePasses(fPasses).fLoopOrderIndependentTransparencyPass1BarrierCustomPass:=TpvScene3DRendererPassesLoopOrderIndependentTransparencyPass1BarrierCustomPass.Create(fFrameGraph,self);
   TpvScene3DRendererInstancePasses(fPasses).fLoopOrderIndependentTransparencyPass1BarrierCustomPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fLoopOrderIndependentTransparencyPass1RenderPass);

   TpvScene3DRendererInstancePasses(fPasses).fLoopOrderIndependentTransparencyPass2RenderPass:=TpvScene3DRendererPassesLoopOrderIndependentTransparencyPass2RenderPass.Create(fFrameGraph,self);
   TpvScene3DRendererInstancePasses(fPasses).fLoopOrderIndependentTransparencyPass2RenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fLoopOrderIndependentTransparencyPass1BarrierCustomPass);

   TpvScene3DRendererInstancePasses(fPasses).fLoopOrderIndependentTransparencyPass2BarrierCustomPass:=TpvScene3DRendererPassesLoopOrderIndependentTransparencyPass2BarrierCustomPass.Create(fFrameGraph,self);
   TpvScene3DRendererInstancePasses(fPasses).fLoopOrderIndependentTransparencyPass2BarrierCustomPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fLoopOrderIndependentTransparencyPass2RenderPass);

   TpvScene3DRendererInstancePasses(fPasses).fLoopOrderIndependentTransparencyResolveRenderPass:=TpvScene3DRendererPassesLoopOrderIndependentTransparencyResolveRenderPass.Create(fFrameGraph,self);
   TpvScene3DRendererInstancePasses(fPasses).fLoopOrderIndependentTransparencyResolveRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fLoopOrderIndependentTransparencyPass2BarrierCustomPass);

  end;

  TpvScene3DRendererTransparencyMode.WBOIT:begin

   TpvScene3DRendererInstancePasses(fPasses).fWeightBlendedOrderIndependentTransparencyRenderPass:=TpvScene3DRendererPassesWeightBlendedOrderIndependentTransparencyRenderPass.Create(fFrameGraph,self);
   TpvScene3DRendererInstancePasses(fPasses).fWeightBlendedOrderIndependentTransparencyRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fMeshComputePass);
   TpvScene3DRendererInstancePasses(fPasses).fWeightBlendedOrderIndependentTransparencyRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fDepthMipMapComputePass);
   TpvScene3DRendererInstancePasses(fPasses).fWeightBlendedOrderIndependentTransparencyRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fForwardRenderMipMapComputePass);

   TpvScene3DRendererInstancePasses(fPasses).fWeightBlendedOrderIndependentTransparencyResolveRenderPass:=TpvScene3DRendererPassesWeightBlendedOrderIndependentTransparencyResolveRenderPass.Create(fFrameGraph,self);

  end;

  TpvScene3DRendererTransparencyMode.MBOIT:begin

   TpvScene3DRendererInstancePasses(fPasses).fMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass:=TpvScene3DRendererPassesMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass.Create(fFrameGraph,self);
   TpvScene3DRendererInstancePasses(fPasses).fMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fMeshComputePass);
   TpvScene3DRendererInstancePasses(fPasses).fMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fDepthMipMapComputePass);
   TpvScene3DRendererInstancePasses(fPasses).fMomentBasedOrderIndependentTransparencyAbsorbanceRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fForwardRenderMipMapComputePass);

   TpvScene3DRendererInstancePasses(fPasses).fMomentBasedOrderIndependentTransparencyTransmittanceRenderPass:=TpvScene3DRendererPassesMomentBasedOrderIndependentTransparencyTransmittanceRenderPass.Create(fFrameGraph,self);

   TpvScene3DRendererInstancePasses(fPasses).fMomentBasedOrderIndependentTransparencyResolveRenderPass:=TpvScene3DRendererPassesMomentBasedOrderIndependentTransparencyResolveRenderPass.Create(fFrameGraph,self);

  end;

  else begin
  end;

 end;

 if Renderer.AntialiasingMode=TpvScene3DRendererAntialiasingMode.TAA then begin

  TpvScene3DRendererInstancePasses(fPasses).fAntialiasingTAAPreCustomPass:=TpvScene3DRendererPassesAntialiasingTAAPreCustomPass.Create(fFrameGraph,self);
  case Renderer.TransparencyMode of
   TpvScene3DRendererTransparencyMode.Direct:begin
    TpvScene3DRendererInstancePasses(fPasses).fAntialiasingTAAPreCustomPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fDirectTransparencyResolveRenderPass);
   end;
   TpvScene3DRendererTransparencyMode.SPINLOCKOIT,
   TpvScene3DRendererTransparencyMode.INTERLOCKOIT:begin
    TpvScene3DRendererInstancePasses(fPasses).fAntialiasingTAAPreCustomPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fLockOrderIndependentTransparencyResolveRenderPass);
   end;
   TpvScene3DRendererTransparencyMode.LOOPOIT:begin
    TpvScene3DRendererInstancePasses(fPasses).fAntialiasingTAAPreCustomPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fLoopOrderIndependentTransparencyResolveRenderPass);
   end;
   TpvScene3DRendererTransparencyMode.WBOIT:begin
    TpvScene3DRendererInstancePasses(fPasses).fAntialiasingTAAPreCustomPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fWeightBlendedOrderIndependentTransparencyResolveRenderPass);
   end;
   TpvScene3DRendererTransparencyMode.MBOIT:begin
    TpvScene3DRendererInstancePasses(fPasses).fAntialiasingTAAPreCustomPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fMomentBasedOrderIndependentTransparencyResolveRenderPass);
   end;
   else begin
    TpvScene3DRendererInstancePasses(fPasses).fAntialiasingTAAPreCustomPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fForwardRenderPass);
   end;
  end;

  TpvScene3DRendererInstancePasses(fPasses).fAntialiasingTAARenderPass:=TpvScene3DRendererPassesAntialiasingTAARenderPass.Create(fFrameGraph,self);
  TpvScene3DRendererInstancePasses(fPasses).fAntialiasingTAARenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fAntialiasingTAAPreCustomPass);

  TpvScene3DRendererInstancePasses(fPasses).fAntialiasingTAAPostCustomPass:=TpvScene3DRendererPassesAntialiasingTAAPostCustomPass.Create(fFrameGraph,self);
  TpvScene3DRendererInstancePasses(fPasses).fAntialiasingTAAPostCustomPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fAntialiasingTAARenderPass);

 end;

(**)
 if (Renderer.DepthOfFieldMode<>TpvScene3DRendererDepthOfFieldMode.None) and not assigned(VirtualReality) then begin

  TpvScene3DRendererInstancePasses(fPasses).fDepthOfFieldAutoFocusComputePass:=TpvScene3DRendererPassesDepthOfFieldAutoFocusComputePass.Create(fFrameGraph,self);
  TpvScene3DRendererInstancePasses(fPasses).fDepthOfFieldAutoFocusComputePass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fDepthMipMapComputePass);

  TpvScene3DRendererInstancePasses(fPasses).fDepthOfFieldPrepareRenderPass:=TpvScene3DRendererPassesDepthOfFieldPrepareRenderPass.Create(fFrameGraph,self);
  TpvScene3DRendererInstancePasses(fPasses).fDepthOfFieldPrepareRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fDepthOfFieldAutoFocusComputePass);

  case Renderer.DepthOfFieldMode of

   TpvScene3DRendererDepthOfFieldMode.HalfResSeparateNearFar,
   TpvScene3DRendererDepthOfFieldMode.HalfResBruteforce:begin

    TpvScene3DRendererInstancePasses(fPasses).fDepthOfFieldBokehComputePass:=TpvScene3DRendererPassesDepthOfFieldBokehComputePass.Create(fFrameGraph,self);

    TpvScene3DRendererInstancePasses(fPasses).fDepthOfFieldPrefilterRenderPass:=TpvScene3DRendererPassesDepthOfFieldPrefilterRenderPass.Create(fFrameGraph,self);

    TpvScene3DRendererInstancePasses(fPasses).fDepthOfFieldBlurRenderPass:=TpvScene3DRendererPassesDepthOfFieldBlurRenderPass.Create(fFrameGraph,self);
    TpvScene3DRendererInstancePasses(fPasses).fDepthOfFieldBlurRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fDepthOfFieldBokehComputePass);

    TpvScene3DRendererInstancePasses(fPasses).fDepthOfFieldPostBlurRenderPass:=TpvScene3DRendererPassesDepthOfFieldPostBlurRenderPass.Create(fFrameGraph,self);

    TpvScene3DRendererInstancePasses(fPasses).fDepthOfFieldCombineRenderPass:=TpvScene3DRendererPassesDepthOfFieldCombineRenderPass.Create(fFrameGraph,self);

   end;

   TpvScene3DRendererDepthOfFieldMode.FullResBruteforce:begin

    TpvScene3DRendererInstancePasses(fPasses).fDepthOfFieldBokehComputePass:=TpvScene3DRendererPassesDepthOfFieldBokehComputePass.Create(fFrameGraph,self);

    TpvScene3DRendererInstancePasses(fPasses).fDepthOfFieldBruteforceRenderPass:=TpvScene3DRendererPassesDepthOfFieldBruteforceRenderPass.Create(fFrameGraph,self);
    TpvScene3DRendererInstancePasses(fPasses).fDepthOfFieldBruteforceRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fDepthOfFieldBokehComputePass);

   end;

   else {TpvScene3DRendererDepthOfFieldMode.FullResHexagon:}begin

    TpvScene3DRendererInstancePasses(fPasses).fDepthOfFieldGatherPass1RenderPass:=TpvScene3DRendererPassesDepthOfFieldGatherPass1RenderPass.Create(fFrameGraph,self);

    TpvScene3DRendererInstancePasses(fPasses).fDepthOfFieldGatherPass2RenderPass:=TpvScene3DRendererPassesDepthOfFieldGatherPass2RenderPass.Create(fFrameGraph,self);

   end;

  end;

  TpvScene3DRendererInstancePasses(fPasses).fDepthOfFieldResolveRenderPass:=TpvScene3DRendererPassesDepthOfFieldResolveRenderPass.Create(fFrameGraph,self);

 end; //*)

 if not assigned(VirtualReality) then begin

  case Renderer.LensMode of

   TpvScene3DRendererLensMode.DownUpsample:begin

    TpvScene3DRendererInstancePasses(fPasses).fLensDownsampleComputePass:=TpvScene3DRendererPassesLensDownsampleComputePass.Create(fFrameGraph,self);

    TpvScene3DRendererInstancePasses(fPasses).fLensUpsampleComputePass:=TpvScene3DRendererPassesLensUpsampleComputePass.Create(fFrameGraph,self);
    TpvScene3DRendererInstancePasses(fPasses).fLensUpsampleComputePass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fLensDownsampleComputePass);

    TpvScene3DRendererInstancePasses(fPasses).fLensResolveRenderPass:=TpvScene3DRendererPassesLensResolveRenderPass.Create(fFrameGraph,self);
    TpvScene3DRendererInstancePasses(fPasses).fLensResolveRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fLensUpsampleComputePass);

   end;

   else begin
   end;

  end;

 end;

 TpvScene3DRendererInstancePasses(fPasses).fTonemappingRenderPass:=TpvScene3DRendererPassesTonemappingRenderPass.Create(fFrameGraph,self);

 if Renderer.AntialiasingMode=TpvScene3DRendererAntialiasingMode.TAA then begin
  TpvScene3DRendererInstancePasses(fPasses).fTonemappingRenderPass.AddExplicitPassDependency(TpvScene3DRendererInstancePasses(fPasses).fAntialiasingTAAPostCustomPass);
 end;

 case Renderer.AntialiasingMode of
  TpvScene3DRendererAntialiasingMode.DSAA:begin
   TpvScene3DRendererInstancePasses(fPasses).fAntialiasingDSAARenderPass:=TpvScene3DRendererPassesAntialiasingDSAARenderPass.Create(fFrameGraph,self);
  end;
  TpvScene3DRendererAntialiasingMode.FXAA:begin
   TpvScene3DRendererInstancePasses(fPasses).fAntialiasingFXAARenderPass:=TpvScene3DRendererPassesAntialiasingFXAARenderPass.Create(fFrameGraph,self);
  end;
  TpvScene3DRendererAntialiasingMode.SMAA:begin
   TpvScene3DRendererInstancePasses(fPasses).fAntialiasingSMAAEdgesRenderPass:=TpvScene3DRendererPassesAntialiasingSMAAEdgesRenderPass.Create(fFrameGraph,self);
   TpvScene3DRendererInstancePasses(fPasses).fAntialiasingSMAAWeightsRenderPass:=TpvScene3DRendererPassesAntialiasingSMAAWeightsRenderPass.Create(fFrameGraph,self);
   TpvScene3DRendererInstancePasses(fPasses).fAntialiasingSMAABlendRenderPass:=TpvScene3DRendererPassesAntialiasingSMAABlendRenderPass.Create(fFrameGraph,self);
  end;
  else begin
   TpvScene3DRendererInstancePasses(fPasses).fAntialiasingNoneRenderPass:=TpvScene3DRendererPassesAntialiasingNoneRenderPass.Create(fFrameGraph,self);
  end;
 end;

 TpvScene3DRendererInstancePasses(fPasses).fDitheringRenderPass:=TpvScene3DRendererPassesDitheringRenderPass.Create(fFrameGraph,self);

 fFrameGraph.RootPass:=TpvScene3DRendererInstancePasses(fPasses).fDitheringRenderPass;

 fFrameGraph.DoWaitOnSemaphore:=true;

 fFrameGraph.DoSignalSemaphore:=true;

 fFrameGraph.Compile;

end;

procedure TpvScene3DRendererInstance.AcquirePersistentResources;
begin
 fFrameGraph.AcquirePersistentResources;
end;

procedure TpvScene3DRendererInstance.ReleasePersistentResources;
begin
 fFrameGraph.ReleasePersistentResources;
end;

procedure TpvScene3DRendererInstance.AcquireVolatileResources;
var InFlightFrameIndex,Index:TpvSizeInt;
    UniversalQueue:TpvVulkanQueue;
    UniversalCommandPool:TpvVulkanCommandPool;
    UniversalCommandBuffer:TpvVulkanCommandBuffer;
    UniversalFence:TpvVulkanFence;
begin

 if assigned(fVirtualReality) then begin

  fWidth:=fVirtualReality.Width;

  fHeight:=fVirtualReality.Height;

 end else if fHasExternalOutputImage then begin

  // Nothing

 end else begin

  fWidth:=pvApplication.VulkanSwapChain.Width;

  fHeight:=pvApplication.VulkanSwapChain.Height;

 end;

 fCameraPreset.MaxCoC:=((fCameraPreset.BlurKernelSize*4.0)+6.0)/fHeight;

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

 end else if fHasExternalOutputImage then begin

  (fFrameGraph.ResourceTypeByName['resourcetype_output_color'] as TpvFrameGraph.TImageResourceType).Format:=fExternalImageFormat;

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
      fNearestFarthestDepthVulkanBuffers[InFlightFrameIndex]:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                                                     SizeOf(TpvVector4),
                                                                                     TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT),
                                                                                     TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                                     [],
                                                                                     TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                                     0,
                                                                                     0,
                                                                                     TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                                     0,
                                                                                     0,
                                                                                     0,
                                                                                     0,
                                                                                     []);
      fDepthOfFieldAutoFocusVulkanBuffers[InFlightFrameIndex]:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                                                      SizeOf(TpvVector4),
                                                                                      TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT),
                                                                                      TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                                      [],
                                                                                      TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                                      0,
                                                                                      0,
                                                                                      TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                                      0,
                                                                                      0,
                                                                                      0,
                                                                                      0,
                                                                                      []);
      fDepthOfFieldBokenShapeTapVulkanBuffers[InFlightFrameIndex]:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                                                          (SizeOf(TpvVector2)*4096)+SizeOf(TpvVector4),
                                                                                          TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT),
                                                                                          TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                                          [],
                                                                                          TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                                          0,
                                                                                          0,
                                                                                          TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                                          0,
                                                                                          0,
                                                                                          0,
                                                                                          0,
                                                                                          []);
     end;

     fLightGridTileSizeX:=(fWidth+(fLightGridSizeX-1)) div fLightGridSizeX;
     fLightGridTileSizeY:=(fHeight+(fLightGridSizeY-1)) div fLightGridSizeY;

     for InFlightFrameIndex:=0 to Renderer.CountInFlightFrames-1 do begin
      fLightGridGlobalsVulkanBuffers[InFlightFrameIndex]:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                                                 SizeOf(TLightGridPushConstants),
                                                                                 TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT),
                                                                                 TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                                 [],
                                                                                 TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                                 TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                                 0,
                                                                                 0,
                                                                                 0,
                                                                                 0,
                                                                                 0,
                                                                                 0,
                                                                                 [TpvVulkanBufferFlag.PersistentMapped]);
      fLightGridClusterAABBVulkanBuffers[InFlightFrameIndex]:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                                                     fLightGridSizeX*fLightGridSizeY*fLightGridSizeZ*SizeOf(TpvVector4)*4*fCountSurfaceViews,
                                                                                     TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                                                     TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                                     [],
                                                                                     TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                                     0,
                                                                                     0,
                                                                                     TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                                     0,
                                                                                     0,
                                                                                     0,
                                                                                     0,
                                                                                     []);
      fLightGridIndexListCounterVulkanBuffers[InFlightFrameIndex]:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                                                          SizeOf(TpvUInt32),
                                                                                          TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                                                          TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                                          [],
                                                                                          TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                                          0,
                                                                                          0,
                                                                                          TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                                          0,
                                                                                          0,
                                                                                          0,
                                                                                          0,
                                                                                          []);
      fLightGridIndexListVulkanBuffers[InFlightFrameIndex]:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                                                   fLightGridSizeX*fLightGridSizeY*fLightGridSizeZ*SizeOf(TpvUInt32)*128*fCountSurfaceViews,
                                                                                   TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                                                   TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                                   [],
                                                                                   TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                                   0,
                                                                                   0,
                                                                                   TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                                   0,
                                                                                   0,
                                                                                   0,
                                                                                   0,
                                                                                   []);
      fLightGridClustersVulkanBuffers[InFlightFrameIndex]:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                                                  fLightGridSizeX*fLightGridSizeY*fLightGridSizeZ*SizeOf(TpvUInt32)*2*fCountSurfaceViews,
                                                                                  TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                                                  TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                                  [],
                                                                                  TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                                  0,
                                                                                  0,
                                                                                  TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                                  0,
                                                                                  0,
                                                                                  0,
                                                                                  0,
                                                                                  []);
     end;

     for InFlightFrameIndex:=0 to Renderer.CountInFlightFrames-1 do begin
      fDepthMipmappedArray2DImages[InFlightFrameIndex]:=TpvScene3DRendererMipmappedArray2DImage.Create(fWidth,fHeight,fCountSurfaceViews,VK_FORMAT_R32_SFLOAT,false,VK_SAMPLE_COUNT_1_BIT,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
      fSceneMipmappedArray2DImages[InFlightFrameIndex]:=TpvScene3DRendererMipmappedArray2DImage.Create(fWidth,fHeight,fCountSurfaceViews,Renderer.OptimizedNonAlphaFormat,true,VK_SAMPLE_COUNT_1_BIT,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
     end;

     case Renderer.TransparencyMode of

      TpvScene3DRendererTransparencyMode.SPINLOCKOIT,
      TpvScene3DRendererTransparencyMode.INTERLOCKOIT:begin

       fCountLockOrderIndependentTransparencyLayers:=CountOrderIndependentTransparencyLayers;//Min(Max(CountOrderIndependentTransparencyLayers,fCountSurfaceMSAASamples),16);

       fLockOrderIndependentTransparentUniformBuffer.ViewPort.x:=fWidth;
       fLockOrderIndependentTransparentUniformBuffer.ViewPort.y:=fHeight;
       fLockOrderIndependentTransparentUniformBuffer.ViewPort.z:=fLockOrderIndependentTransparentUniformBuffer.ViewPort.x*fLockOrderIndependentTransparentUniformBuffer.ViewPort.y;
       fLockOrderIndependentTransparentUniformBuffer.ViewPort.w:=(fCountLockOrderIndependentTransparencyLayers and $ffff) or ((Renderer.CountSurfaceMSAASamples and $ffff) shl 16);

       fLockOrderIndependentTransparentUniformVulkanBuffer.UploadData(pvApplication.VulkanDevice.UniversalQueue,
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

       fLoopOrderIndependentTransparentUniformVulkanBuffer.UploadData(pvApplication.VulkanDevice.UniversalQueue,
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

       fApproximationOrderIndependentTransparentUniformVulkanBuffer.UploadData(pvApplication.VulkanDevice.UniversalQueue,
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

 if Renderer.AntialiasingMode=TpvScene3DRendererAntialiasingMode.TAA then begin
  for InFlightFrameIndex:=0 to Renderer.CountInFlightFrames-1 do begin
   fTAAHistoryColorImages[InFlightFrameIndex]:=TpvScene3DRendererArray2DImage.Create(fWidth,
                                                                                     fHeight,
                                                                                     fCountSurfaceViews,
                                                                                     Renderer.OptimizedNonAlphaFormat,
                                                                                     TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                                                                     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
   fTAAHistoryDepthImages[InFlightFrameIndex]:=TpvScene3DRendererArray2DImage.Create(fWidth,
                                                                                     fHeight,
                                                                                     fCountSurfaceViews,
                                                                                     VK_FORMAT_D32_SFLOAT,
                                                                                     TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                                                                     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
   fTAAEvents[InFlightFrameIndex]:=TpvVulkanEvent.Create(Renderer.VulkanDevice);
   fTAAEventReady[InFlightFrameIndex]:=false;
  end;
 end;

 fFrameGraph.AcquireVolatileResources;

end;

procedure TpvScene3DRendererInstance.ReleaseVolatileResources;
var InFlightFrameIndex:TpvSizeInt;
begin

 fFrameGraph.ReleaseVolatileResources;

 if Renderer.AntialiasingMode=TpvScene3DRendererAntialiasingMode.TAA then begin
  for InFlightFrameIndex:=0 to Renderer.CountInFlightFrames-1 do begin
   FreeAndNil(fTAAHistoryColorImages[InFlightFrameIndex]);
   FreeAndNil(fTAAHistoryDepthImages[InFlightFrameIndex]);
   FreeAndNil(fTAAEvents[InFlightFrameIndex]);
  end;
 end;

 if assigned(fExternalOutputImageData) then begin
  fExternalOutputImageData.VulkanImages.Clear;
 end;

 for InFlightFrameIndex:=0 to Renderer.CountInFlightFrames-1 do begin
  FreeAndNil(fDepthMipmappedArray2DImages[InFlightFrameIndex]);
  FreeAndNil(fSceneMipmappedArray2DImages[InFlightFrameIndex]);
 end;

 for InFlightFrameIndex:=0 to Renderer.CountInFlightFrames-1 do begin
  FreeAndNil(fNearestFarthestDepthVulkanBuffers[InFlightFrameIndex]);
  FreeAndNil(fDepthOfFieldAutoFocusVulkanBuffers[InFlightFrameIndex]);
  FreeAndNil(fDepthOfFieldBokenShapeTapVulkanBuffers[InFlightFrameIndex]);
 end;

 for InFlightFrameIndex:=0 to Renderer.CountInFlightFrames-1 do begin
  FreeAndNil(fLightGridGlobalsVulkanBuffers[InFlightFrameIndex]);
  FreeAndNil(fLightGridClusterAABBVulkanBuffers[InFlightFrameIndex]);
  FreeAndNil(fLightGridIndexListCounterVulkanBuffers[InFlightFrameIndex]);
  FreeAndNil(fLightGridIndexListVulkanBuffers[InFlightFrameIndex]);
  FreeAndNil(fLightGridClustersVulkanBuffers[InFlightFrameIndex]);
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

procedure TpvScene3DRendererInstance.Update(const aInFlightFrameIndex:TpvInt32;const aFrameCounter:TpvInt64);
begin
 fFrameGraph.Update(aInFlightFrameIndex,aFrameCounter);
end;

procedure TpvScene3DRendererInstance.Reset;
begin
 fViews.Count:=0;
end;

procedure TpvScene3DRendererInstance.AddView(const aView:TpvScene3D.TView);
begin
 fViews.Add(aView);
end;

procedure TpvScene3DRendererInstance.AddViews(const aViews:array of TpvScene3D.TView);
begin
 fViews.Add(aViews);
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
  for Index:=0 to fViews.Count-1 do begin
   ViewMatrix:=fViews.Items[Index].ViewMatrix.SimpleInverse;
   if SceneWorldSpaceSphere.Contains(ViewMatrix.Translation.xyz) then begin
    if SceneWorldSpaceSphere.RayIntersection(ViewMatrix.Translation.xyz,-ViewMatrix.Forwards.xyz,Value) then begin
     Value:=Value*2.0;
    end else begin
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

 InFlightFrameState^.ZNear:=Min(RealZNear,1e-4);
 InFlightFrameState^.ZFar:=RealZFar;

 CascadedShadowMapSplitLambda:=0.5;

 CascadedShadowMapSplitOverlap:=0.1;

 SceneClipWorldSpaceSphere:=TpvSphere.Create(SceneWorldSpaceSphere.Center,Max(SceneWorldSpaceSphere.Radius,RealZFar*0.5));

 for Index:=0 to fViews.Count-1 do begin
  ProjectionMatrix:=fViews.Items[Index].ProjectionMatrix;
  if DoNeedRefitNearFarPlanes then begin
   ProjectionMatrix[2,2]:=RealZFar/(RealZNear-RealZFar);
   ProjectionMatrix[3,2]:=(-(RealZNear*RealZFar))/(RealZFar-RealZNear);
  end;
  fCascadedShadowMapInverseProjectionMatrices[Index]:=ProjectionMatrix.Inverse;
 end;

 LightForwardVector:=-Renderer.SkyCubeMap.LightDirection.xyz.Normalize;
 LightSideVector:=LightForwardVector.Perpendicular;
{LightSideVector:=TpvVector3.InlineableCreate(-fViews.Items[0].ViewMatrix.RawComponents[0,2],
                                              -fViews.Items[0].ViewMatrix.RawComponents[1,2],
                                              -fViews.Items[0].ViewMatrix.RawComponents[2,2]).Normalize;
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

 for Index:=0 to fViews.Count-1 do begin
  ViewMatrix:=fViews.Items[Index].ViewMatrix.SimpleInverse;
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

 for ViewIndex:=0 to fViews.Count-1 do begin
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

  for ViewIndex:=0 to fViews.Count-1 do begin
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
  LightSpaceAABB.Max.y:=ceil(LightSpaceAABB.Max.y/UnitsPerTexel.y)*UnitsPerTexel.y;//}

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

function TpvScene3DRendererInstance.GetJitterOffset(const aFrameCounter:TpvInt64):TpvVector2;
begin
 if (Renderer.AntialiasingMode=TpvScene3DRendererAntialiasingMode.TAA) and (aFrameCounter>=0) then begin
  result:=((JitterOffsets[aFrameCounter and JitterOffsetMask]-TpvVector2.InlineableCreate(0.5,0.5))*1.0)/TpvVector2.InlineableCreate(fWidth,fHeight);
 end else begin
  result.x:=0.0;
  result.y:=0.0;
 end;
end;

function TpvScene3DRendererInstance.AddTemporalAntialiasingJitter(const aProjectionMatrix:TpvMatrix4x4;const aFrameCounter:TpvInt64):TpvMatrix4x4;
begin
 if Renderer.AntialiasingMode=TpvScene3DRendererAntialiasingMode.TAA then begin
  result:=aProjectionMatrix*TpvMatrix4x4.CreateTranslation(GetJitterOffset(aFrameCounter));
 end else begin
  result:=aProjectionMatrix;
 end;
end;

procedure TpvScene3DRendererInstance.DrawUpdate(const aInFlightFrameIndex:TpvInt32;const aFrameCounter:TpvInt64);
var Index:TpvSizeInt;
    InFlightFrameState:PInFlightFrameState;
    ViewLeft,ViewRight:TpvScene3D.TView;
    ViewMatrix:TpvMatrix4x4;
begin

 InFlightFrameState:=@fInFlightFrameStates[aInFlightFrameIndex];

 if fViews.Count=0 then begin

  ViewMatrix:=fCameraMatrix.SimpleInverse;

  if assigned(fVirtualReality) then begin

   ViewLeft.ViewMatrix:=ViewMatrix*fVirtualReality.GetPositionMatrix(0);
   ViewLeft.ProjectionMatrix:=AddTemporalAntialiasingJitter(fVirtualReality.GetProjectionMatrix(0),aFrameCounter);
   ViewLeft.InverseViewMatrix:=ViewLeft.ViewMatrix.Inverse;
   ViewLeft.InverseProjectionMatrix:=ViewLeft.ProjectionMatrix.Inverse;

   ViewRight.ViewMatrix:=ViewMatrix*fVirtualReality.GetPositionMatrix(1);
   ViewRight.ProjectionMatrix:=AddTemporalAntialiasingJitter(fVirtualReality.GetProjectionMatrix(1),aFrameCounter);
   ViewRight.InverseViewMatrix:=ViewRight.ViewMatrix.Inverse;
   ViewRight.InverseProjectionMatrix:=ViewRight.ProjectionMatrix.Inverse;

   fViews.Add([ViewLeft,ViewRight]);

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
   ViewLeft.ProjectionMatrix:=AddTemporalAntialiasingJitter(ViewLeft.ProjectionMatrix*TpvMatrix4x4.FlipYClipSpace,aFrameCounter);
   ViewLeft.InverseViewMatrix:=ViewLeft.ViewMatrix.Inverse;
   ViewLeft.InverseProjectionMatrix:=ViewLeft.ProjectionMatrix.Inverse;

   fViews.Add(ViewLeft);

  end;

 end;

 if fViews.Count>0 then begin
  InFlightFrameState^.FinalViewIndex:=Renderer.Scene3D.AddView(fViews.Items[0]);
  for Index:=1 to fViews.Count-1 do begin
   Renderer.Scene3D.AddView(fViews.Items[Index]);
  end;
 end;
 InFlightFrameState^.CountViews:=fViews.Count;

 CalculateCascadedShadowMaps(aInFlightFrameIndex);

 fCascadedShadowMapVulkanUniformBuffers[aInFlightFrameIndex].UpdateData(fCascadedShadowMapUniformBuffers[aInFlightFrameIndex],
                                                                        0,
                                                                        SizeOf(TCascadedShadowMapUniformBuffer));

 InFlightFrameState^.ViewRenderPassIndex:=Renderer.Scene3D.AcquireRenderPassIndex;

 InFlightFrameState^.CascadedShadowMapRenderPassIndex:=Renderer.Scene3D.AcquireRenderPassIndex;

 InFlightFrameState^.Jitter.xy:=GetJitterOffset(aFrameCounter);
 InFlightFrameState^.Jitter.zw:=GetJitterOffset(aFrameCounter-1);

 // Main viewport(s)
 Renderer.Scene3D.Prepare(aInFlightFrameIndex,
                          InFlightFrameState^.ViewRenderPassIndex,
                          InFlightFrameState^.FinalViewIndex,
                          InFlightFrameState^.CountViews,
                          fWidth,
                          fHeight,
                          true,
                          true);

 // Cascaded shadow map viewport(s)
 Renderer.Scene3D.Prepare(aInFlightFrameIndex,
                          InFlightFrameState^.CascadedShadowMapRenderPassIndex,
                          InFlightFrameState^.CascadedShadowMapViewIndex,
                          InFlightFrameState^.CountCascadedShadowMapViews,
                          CascadedShadowMapWidth,
                          CascadedShadowMapHeight,
                          false,
                          true);

 TPasMPInterlocked.Write(InFlightFrameState^.Ready,true);

end;

procedure TpvScene3DRendererInstance.Draw(const aSwapChainImageIndex,aInFlightFrameIndex:TpvInt32;const aFrameCounter:TpvInt64;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
begin

 FillChar(fLightGridPushConstants,SizeOf(TpvScene3DRendererInstance.TLightGridPushConstants),#0);
 fLightGridPushConstants.TileSizeX:=fLightGridTileSizeX;
 fLightGridPushConstants.TileSizeY:=fLightGridTileSizeY;
 fLightGridPushConstants.ZNear:=InFlightFrameStates[aInFlightFrameIndex].ZNear;
 fLightGridPushConstants.ZFar:=InFlightFrameStates[aInFlightFrameIndex].ZFar;
 fLightGridPushConstants.ViewRect:=TpvVector4.InlineableCreate(0.0,0.0,fWidth,fHeight);
 fLightGridPushConstants.CountLights:=Renderer.Scene3D.LightBuffers[aInFlightFrameIndex].LightItems.Count;
 fLightGridPushConstants.Size:=fLightGridSizeX*fLightGridSizeY*fLightGridSizeZ;
 fLightGridPushConstants.OffsetedViewIndex:=fInFlightFrameStates[aInFlightFrameIndex].FinalViewIndex;
 fLightGridPushConstants.ClusterSizeX:=fLightGridSizeX;
 fLightGridPushConstants.ClusterSizeY:=fLightGridSizeY;
 fLightGridPushConstants.ClusterSizeZ:=fLightGridSizeZ;
 fLightGridPushConstants.ZScale:=fLightGridSizeZ/Log2(fLightGridPushConstants.ZFar/fLightGridPushConstants.ZNear);
 fLightGridPushConstants.ZBias:=-((fLightGridSizeZ*Log2(fLightGridPushConstants.ZNear))/Log2(fLightGridPushConstants.ZFar/fLightGridPushConstants.ZNear));
 fLightGridPushConstants.ZMax:=fLightGridSizeZ-1;

 fLightGridGlobalsVulkanBuffers[aInFlightFrameIndex].UpdateData(fLightGridPushConstants,0,SizeOf(TpvScene3DRendererInstance.TLightGridPushConstants));

 fFrameGraph.Draw(aSwapChainImageIndex,
                  aInFlightFrameIndex,
                  aFrameCounter,
                  aWaitSemaphore,
                  fVulkanRenderSemaphores[aInFlightFrameIndex],
                  aWaitFence);

 aWaitSemaphore:=fVulkanRenderSemaphores[aInFlightFrameIndex];

 TPasMPInterlocked.Write(fInFlightFrameStates[aInFlightFrameIndex].Ready,false);

end;

procedure InitializeJitterOffsets;
var Index:TpvSizeInt;
begin
 for Index:=0 to CountJitterOffsets-1 do begin
  JitterOffsets[Index]:=TpvVector2.InlineableCreate(GetHaltonSequence(Index+1,2),GetHaltonSequence(Index+1,3));
 end;
end;

initialization
 InitializeJitterOffsets;
end.

