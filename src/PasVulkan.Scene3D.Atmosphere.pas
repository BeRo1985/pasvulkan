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
unit PasVulkan.Scene3D.Atmosphere;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasMP,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Application,
     PasVulkan.Framework,
     PasVulkan.Collections,
     PasVulkan.Scene3D.Renderer.Array2DImage;

type TpvScene3DAtmosphere=class;

     { TpvScene3DAtmosphere } 
     TpvScene3DAtmosphere=class
      public
       const TRANSMITTANCE_TEXTURE_WIDTH=256;
             TRANSMITTANCE_TEXTURE_HEIGHT=64;
             SCATTERING_TEXTURE_R_SIZE=32;
             SCATTERING_TEXTURE_MU_SIZE=128;
             SCATTERING_TEXTURE_MU_S_SIZE=32;
             SCATTERING_TEXTURE_NU_SIZE=8;
             SCATTERING_TEXTURE_WIDTH=SCATTERING_TEXTURE_NU_SIZE*SCATTERING_TEXTURE_MU_S_SIZE;
             SCATTERING_TEXTURE_HEIGHT=SCATTERING_TEXTURE_MU_SIZE;
             SCATTERING_TEXTURE_DEPTH=SCATTERING_TEXTURE_R_SIZE;             
             IRRADIANCE_TEXTURE_WIDTH=64;
             IRRADIANCE_TEXTURE_HEIGHT=16;
             SkyViewLUTTextureWidth=192;
             SkyViewLUTTextureHeight=108;     
             CameraVolumeTextureWidth=32;
             CameraVolumeTextureHeight=32;
             CameraVolumeTextureDepth=32;
             MultiScatteringLUTRes=32;
             ScatteringOrders=4; 
       type { TDensityProfileLayer }
            TDensityProfileLayer=packed record
             public
              Width:TpvFloat;
              ExpTerm:TpvFloat;
              ExpScale:TpvFloat;
              LinearTerm:TpvFloat;
              ConstantTerm:TpvFloat;
              Unused0:TpvFloat;
              Unused1:TpvFloat;
              Unused2:TpvFloat;
              constructor Create(const aWidth,aExpTerm,aExpScale,aLinearTerm,aConstantTerm:TpvFloat);
            end;
            PDensityProfileLayer=^TDensityProfileLayer;
            { TDensityProfile }
            TDensityProfile=packed record
             public
              Layers:array[0..1] of TDensityProfileLayer;
            end; 
            { TAtmosphereParameters }
            TAtmosphereParameters=packed record             
             public
              Transform:TpvMatrix4x4; // Transform of the atmosphere for the case that the atmosphere is not centered at the origin (e.g. multiple planets)
              RayleighDensity:TDensityProfile;
              MieDensity:TDensityProfile;
              AbsorptionDensity:TDensityProfile;
              Center:TpvVector4; // w is unused, for alignment
              SolarIrradiance:TpvVector4; // w is unused, for alignment
              RayleighScattering:TpvVector4; // w is unused, for alignment
              MieScattering:TpvVector4; // w is unused, for alignment
              MieExtinction:TpvVector4; // w is unused, for alignment
              AbsorptionExtinction:TpvVector4; // w is unused, for alignment
              GroundAlbedo:TpvVector4; // w is unused, for alignment
              MiePhaseFunctionG:TpvFloat;
              SunAngularRadius:TpvFloat;
              BottomRadius:TpvFloat;
              TopRadius:TpvFloat;
              MuSMin:TpvFloat;
              procedure InitializeEarthAtmosphere;
            end;
            PAtmosphereParameters=^TAtmosphereParameters;
            { TRendererInstance }
            TRendererInstance=class
             public
              type { TKey }
                   TKey=record
                    public
                     fRendererInstance:TObject;
                    public
                     constructor Create(const aRendererInstance:TObject);
                   end;
                   PKey=^TKey;
             private
              fAtmosphere:TpvScene3DAtmosphere;
              fRendererInstance:TObject;
              fKey:TKey;
              fTransmittanceTexture:TpvScene3DRendererArray2DImage;
              fMultiScatteringTexture:TpvScene3DRendererArray2DImage;
              fSkyViewLUTTexture:TpvScene3DRendererArray2DImage;
              fCameraVolumeTexture:TpvScene3DRendererArray2DImage;
              fTransmittanceLUTPassDescriptorPool:TpvVulkanDescriptorPool;
              fTransmittanceLUTPassDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fMultiScatteringLUTPassDescriptorPool:TpvVulkanDescriptorPool;
              fMultiScatteringLUTPassDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fSkyViewLUTPassDescriptorPool:TpvVulkanDescriptorPool;
              fSkyViewLUTPassDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fCameraVolumePassDescriptorPool:TpvVulkanDescriptorPool;
              fCameraVolumePassDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fRaymarchingPassDescriptorPool:TpvVulkanDescriptorPool;
              fRaymarchingPassDepthImageViews:array[0..MaxInFlightFrames-1] of TVkImageView;
              fRaymarchingPassDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fGlobalDescriptorPool:TpvVulkanDescriptorPool;
              fGlobalDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
             public
              constructor Create(const aAtmosphere:TpvScene3DAtmosphere;const aRendererInstance:TObject);
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure Execute(const aInFlightFrameIndex:TpvSizeInt;
                                const aCommandBuffer:TpvVulkanCommandBuffer);
             published
              property Atmosphere:TpvScene3DAtmosphere read fAtmosphere;
              property RendererInstance:TObject read fRendererInstance;
              property TransmittanceTexture:TpvScene3DRendererArray2DImage read fTransmittanceTexture;
              property MultiScatteringTexture:TpvScene3DRendererArray2DImage read fMultiScatteringTexture;
              property SkyViewLUTTexture:TpvScene3DRendererArray2DImage read fSkyViewLUTTexture;
              property CameraVolumeTexture:TpvScene3DRendererArray2DImage read fCameraVolumeTexture; 
            end;
            { TRendererInstances }
            TRendererInstances=TpvObjectGenericList<TRendererInstance>;
            { TRendererInstanceHashMap }
            TRendererInstanceHashMap=TpvHashMap<TRendererInstance.TKey,TRendererInstance>;
      private
       fScene3D:TObject;
       fAtmosphereParameters:TAtmosphereParameters;
       fPointerToAtmosphereParameters:PAtmosphereParameters;
       fAtmosphereParametersBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fRendererInstances:TRendererInstances;
       fRendererInstanceHashMap:TRendererInstanceHashMap;
       fRendererInstanceListLock:TPasMPSlimReaderWriterLock;
       fToDestroy:boolean;
       fReleaseFrameCounter:TpvInt32;
       fReady:Boolean;
       fUploaded:LongBool;
       fVisible:Boolean;
       fInFlightFrameVisible:array[0..MaxInFlightFrames-1] of boolean;
      public
       constructor Create(const aScene3D:TObject);
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       procedure Release;
       function HandleRelease:boolean;
       procedure Upload;
       procedure Unload;
       procedure Update(const aInFlightFrameIndex:TpvSizeInt;
                        const aTransferQueue:TpvVulkanQueue;
                        const aTransferCommandBuffer:TpvVulkanCommandBuffer;
                        const aTransferFence:TpvVulkanFence);
       procedure Execute(const aInFlightFrameIndex:TpvSizeInt;
                         const aCommandBuffer:TpvVulkanCommandBuffer;
                         const aRendererInstance:TObject);
      public
       property AtmosphereParameters:PAtmosphereParameters read fPointerToAtmosphereParameters;
       property Ready:Boolean read fReady;
       property Uploaded:LongBool read fUploaded;
       property Visible:Boolean read fVisible;
     end; 

     { TpvScene3DAtmospheres }
     TpvScene3DAtmospheres=class(TpvObjectGenericList<TpvScene3DAtmosphere>)
      private
       fScene3D:TObject;
       fLock:TPasMPMultipleReaderSingleWriterLock;
      public
       constructor Create(const aScene3D:TObject); reintroduce;
       destructor Destroy; override;
       procedure ProcessReleases;
      published
       property Scene3D:TObject read fScene3D;
       property Lock:TPasMPMultipleReaderSingleWriterLock read fLock;
     end;

     { TpvScene3DAtmosphereGlobals }
     TpvScene3DAtmosphereGlobals=class
      public
       type TTransmittanceLUTPushConstants=packed record
             BaseViewIndex:TpvInt32;
             CountViews:TpvInt32;
             Dummy0:TpvInt32;
             Dummy1:TpvInt32;
             SunDirection:TpvVector4;
            end;
            PTransmittanceLUTPushConstants=^TTransmittanceLUTPushConstants;
            TMultipleScatteringLUTPushConstants=packed record
             BaseViewIndex:TpvInt32;
             CountViews:TpvInt32;
             ViewIndex:TpvInt32;
             MultipleScatteringFactor:TpvFloat;
            end;            
            PMultipleScatteringLUTPushConstants=^TMultipleScatteringLUTPushConstants;
            TSkyViewLUTPushConstants=packed record
             BaseViewIndex:TpvInt32;
             CountViews:TpvInt32;
             Dummy0:TpvInt32;
             Dummy1:TpvInt32;
             SunDirection:TpvVector4;
            end;
            PSkyViewLUTPushConstants=^TSkyViewLUTPushConstants;
            TCameraVolumePushConstants=packed record
             BaseViewIndex:TpvInt32;
             CountViews:TpvInt32;
             Dummy0:TpvInt32;
             Dummy1:TpvInt32;
             SunDirection:TpvVector4;
            end;
      private
       fScene3D:TObject;
       fAtmospheres:TpvScene3DAtmospheres;
       fTransmittanceLUTPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fMultiScatteringLUTPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fSkyViewLUTPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fCameraVolumePassDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fRaymarchingPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fGlobalVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fTransmittanceLUTComputeShaderModule:TpvVulkanShaderModule;
       fTransmittanceLUTComputeShaderStage:TpvVulkanPipelineShaderStage;
       fTransmittanceLUTComputePipelineLayout:TpvVulkanPipelineLayout;
       fTransmittanceLUTComputePipeline:TpvVulkanComputePipeline;
       fMultiScatteringLUTComputeShaderModule:TpvVulkanShaderModule;
       fMultiScatteringLUTComputeShaderStage:TpvVulkanPipelineShaderStage;
       fMultiScatteringLUTComputePipelineLayout:TpvVulkanPipelineLayout;
       fMultiScatteringLUTComputePipeline:TpvVulkanComputePipeline;
       fSkyViewLUTComputeShaderModule:TpvVulkanShaderModule;
       fSkyViewLUTComputeShaderStage:TpvVulkanPipelineShaderStage;
       fSkyViewLUTComputePipelineLayout:TpvVulkanPipelineLayout;
       fSkyViewLUTComputePipeline:TpvVulkanComputePipeline;
       fCameraVolumeComputeShaderModule:TpvVulkanShaderModule;
       fCameraVolumeComputeShaderStage:TpvVulkanPipelineShaderStage;
       fCameraVolumeComputePipelineLayout:TpvVulkanPipelineLayout;
       fCameraVolumeComputePipeline:TpvVulkanComputePipeline;
      public
       constructor Create(const aScene3D:TObject);
       destructor Destroy; override;
       procedure AllocateResources;
       procedure DeallocateResources;
      published
       property Scene3D:TObject read fScene3D;
       property Atmospheres:TpvScene3DAtmospheres read fAtmospheres;
     end;

     { TpvScene3DAtmosphereRendererInstance }
     TpvScene3DAtmosphereRendererInstance=class
      public
       // The passes are for all frustum visible atmospheres in the scene in a row for a renderer instance
       type TTransmittanceLUTPass=class 
             private
              fAtmosphereRendererInstance:TpvScene3DAtmosphereRendererInstance;
              fPipeline:TpvVulkanComputePipeline;
             public
              constructor Create(const aAtmosphereRendererInstance:TpvScene3DAtmosphereRendererInstance);
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure Execute(const aVulkanCommandBuffer:TpvVulkanCommandBuffer);
            end; 
      private
       fScene3D:TObject;
       fRenderer:TObject;
       fRendererInstance:TObject;
       fVulkanComputeQueue:TpvVulkanQueue;
       fVulkanComputeCommandPool:TpvVulkanCommandPool;
       fVulkanComputeCommandBuffer:TpvVulkanCommandBuffer;       
      public
       constructor Create(const aScene3D,aRenderer,aRendererInstance:TObject);
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       procedure AllocateResources;
       procedure DeallocateResources;
      published
     end;

implementation

uses PasVulkan.Scene3D,
     PasVulkan.Scene3D.Assets,
     PasVulkan.Scene3D.Renderer,
     PasVulkan.Scene3D.Renderer.Globals,
     PasVulkan.Scene3D.Renderer.Instance;

{ TpvScene3DAtmosphere.TDensityProfileLayer }

constructor TpvScene3DAtmosphere.TDensityProfileLayer.Create(const aWidth,aExpTerm,aExpScale,aLinearTerm,aConstantTerm:TpvFloat);
begin
 Width:=aWidth;
 ExpTerm:=aExpTerm;
 ExpScale:=aExpScale;
 LinearTerm:=aLinearTerm;
 ConstantTerm:=aConstantTerm;
end;

{ TpvScene3DAtmosphere.TAtmosphereParameters }

procedure TpvScene3DAtmosphere.TAtmosphereParameters.InitializeEarthAtmosphere;
const EarthBottomRadius=6360.0;
      EarthTopRadius=6460.0;
      EarthRayleighScaleHeight=8.0;
      EarthMieScaleHeight=1.2;
begin
 
 // Transform
 Transform:=TpvMatrix4x4.Identity;

 // Sun
 SolarIrradiance:=TpvVector4.InlineableCreate(1.0,1.0,1.0,0.0);
 SunAngularRadius:=0.004675;

 // Planet
 BottomRadius:=EarthBottomRadius;
 TopRadius:=EarthTopRadius;
 GroundAlbedo:=TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0);

 // Rayleigh scattering
 RayleighDensity.Layers[0]:=TDensityProfileLayer.Create(0.0,0.0,0.0,0.0,0.0);
 RayleighDensity.Layers[1]:=TDensityProfileLayer.Create(0.0,1.0,-1.0/EarthRayleighScaleHeight,0.0,0.0);
 RayleighScattering:=TpvVector4.InlineableCreate(0.005802,0.013558,0.033100,0.0);

 // Mie scattering
 MieDensity.Layers[0]:=TDensityProfileLayer.Create(0.0,0.0,0.0,0.0,0.0);
 MieDensity.Layers[1]:=TDensityProfileLayer.Create(0.0,1.0,-1.0/EarthMieScaleHeight,0.0,0.0);
 MieScattering:=TpvVector4.InlineableCreate(0.003996,0.003996,0.003996,0.0);
 MieExtinction:=TpvVector4.InlineableCreate(0.004440,0.004440,0.004440,0.0);
 MiePhaseFunctionG:=0.8;

 // Absorption extinction / Ozone layer
 AbsorptionDensity.Layers[0]:=TDensityProfileLayer.Create(25.0,0.0,0.0,1.0/15.0,-2.0/3.0);
 AbsorptionDensity.Layers[1]:=TDensityProfileLayer.Create(0.0,0.0,0.0,-1.0/15.0,8.0/3.0);
 AbsorptionExtinction:=TpvVector4.InlineableCreate(0.000650,0.001881,0.000085,0.0);

 MuSMin:=cos(PI*120.0/180.0);

end;

{ TpvScene3DAtmosphere.TRendererInstance.TKey }

constructor TpvScene3DAtmosphere.TRendererInstance.TKey.Create(const aRendererInstance:TObject);
begin
 fRendererInstance:=aRendererInstance;
end;

{ TpvScene3DAtmosphere.TRendererInstance }

constructor TpvScene3DAtmosphere.TRendererInstance.Create(const aAtmosphere:TpvScene3DAtmosphere;const aRendererInstance:TObject);
var InFlightFrameIndex:TpvSizeInt;
begin

 inherited Create;

 fAtmosphere:=aAtmosphere;

 fRendererInstance:=aRendererInstance;

 fKey:=TKey.Create(fRendererInstance);

 fTransmittanceTexture:=TpvScene3DRendererArray2DImage.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                              TRANSMITTANCE_TEXTURE_WIDTH,
                                                              TRANSMITTANCE_TEXTURE_HEIGHT,
                                                              TpvScene3DRendererInstance(aRendererInstance).CountSurfaceViews,
                                                              VK_FORMAT_R32G32B32A32_SFLOAT,
                                                              VK_SAMPLE_COUNT_1_BIT,
                                                              VK_IMAGE_LAYOUT_GENERAL,
                                                              true,
                                                              0);

 fMultiScatteringTexture:=TpvScene3DRendererArray2DImage.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                                MultiScatteringLUTRes,
                                                                MultiScatteringLUTRes,
                                                                TpvScene3DRendererInstance(aRendererInstance).CountSurfaceViews,
                                                                VK_FORMAT_R32G32B32A32_SFLOAT,
                                                                VK_SAMPLE_COUNT_1_BIT,
                                                                VK_IMAGE_LAYOUT_GENERAL,
                                                                true,
                                                                0);  

 fSkyViewLUTTexture:=TpvScene3DRendererArray2DImage.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                           SkyViewLUTTextureWidth,
                                                           SkyViewLUTTextureHeight, 
                                                           TpvScene3DRendererInstance(aRendererInstance).CountSurfaceViews,
                                                           VK_FORMAT_R32G32B32A32_SFLOAT,
                                                           VK_SAMPLE_COUNT_1_BIT,
                                                           VK_IMAGE_LAYOUT_GENERAL,
                                                           true,
                                                           0);                                                            

 fCameraVolumeTexture:=TpvScene3DRendererArray2DImage.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                            CameraVolumeTextureWidth,
                                                            CameraVolumeTextureHeight,
                                                            CameraVolumeTextureDepth*TpvScene3DRendererInstance(aRendererInstance).CountSurfaceViews,
                                                            VK_FORMAT_R32G32B32A32_SFLOAT,
                                                            VK_SAMPLE_COUNT_1_BIT,
                                                            VK_IMAGE_LAYOUT_GENERAL,
                                                            true,
                                                            0);

 fTransmittanceLUTPassDescriptorPool:=TpvVulkanDescriptorPool.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                                     TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT) or
                                                                     TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT),
                                                                     TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fTransmittanceLUTPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fTransmittanceLUTPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fTransmittanceLUTPassDescriptorPool.Initialize;
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fTransmittanceLUTPassDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'TransmittanceLUTPassDescriptorPool');

 for InFlightFrameIndex:=0 to TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames-1 do begin

  fTransmittanceLUTPassDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fTransmittanceLUTPassDescriptorPool,
                                                                                         TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).Atmospheres).fTransmittanceLUTPassDescriptorSetLayout);

  fTransmittanceLUTPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                               0,
                                                                               1,
                                                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                               [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                              fTransmittanceTexture.VulkanImage.Handle,
                                                                                                              VK_IMAGE_LAYOUT_GENERAL)],
                                                                               [],
                                                                               [],
                                                                               false);

  fTransmittanceLUTPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                               0,
                                                                               1,
                                                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                               [],
                                                                               [fAtmosphere.fAtmosphereParametersBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                               [],
                                                                               false);                                                             

  fTransmittanceLUTPassDescriptorSets[InFlightFrameIndex].Flush;

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fTransmittanceLUTPassDescriptorSets[InFlightFrameIndex].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'TransmittanceLUTPassDescriptorSets['+IntToStr(InFlightFrameIndex)+']');

 end; 

 fMultiScatteringLUTPassDescriptorPool:=TpvVulkanDescriptorPool.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                                      TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT) or
                                                                      TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT),
                                                                      TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fMultiScatteringLUTPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fMultiScatteringLUTPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fMultiScatteringLUTPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fMultiScatteringLUTPassDescriptorPool.Initialize;
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fMultiScatteringLUTPassDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'MultiScatteringLUTPassDescriptorPool');

 for InFlightFrameIndex:=0 to TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames-1 do begin

  fMultiScatteringLUTPassDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fMultiScatteringLUTPassDescriptorPool,
                                                                                         TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).Atmospheres).fMultiScatteringLUTPassDescriptorSetLayout);

  fMultiScatteringLUTPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                                 0,
                                                                                 1,
                                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                                fMultiScatteringTexture.VulkanImage.Handle,
                                                                                                                VK_IMAGE_LAYOUT_GENERAL)],
                                                                                 [],
                                                                                 [],
                                                                                 false);

  fMultiScatteringLUTPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                                 0,
                                                                                 1,
                                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                                 [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                                fTransmittanceTexture.VulkanImageView.Handle,
                                                                                                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                                 [],
                                                                                 [],
                                                                                 false);

  fMultiScatteringLUTPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                                 0,
                                                                                 1,
                                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                                 [],
                                                                                 [fAtmosphere.fAtmosphereParametersBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                                 [],
                                                                                 false);                                                             

  fMultiScatteringLUTPassDescriptorSets[InFlightFrameIndex].Flush;

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fMultiScatteringLUTPassDescriptorSets[InFlightFrameIndex].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'MultiScatteringLUTPassDescriptorSets['+IntToStr(InFlightFrameIndex)+']');

 end;

 fSkyViewLUTPassDescriptorPool:=TpvVulkanDescriptorPool.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                              TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT) or
                                                              TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT),
                                                              TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fSkyViewLUTPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fSkyViewLUTPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*2);
 fSkyViewLUTPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fSkyViewLUTPassDescriptorPool.Initialize;
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fSkyViewLUTPassDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'SkyViewLUTPassDescriptorPool');

 for InFlightFrameIndex:=0 to TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames-1 do begin

  fSkyViewLUTPassDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fSkyViewLUTPassDescriptorPool,
                                                                                   TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).Atmospheres).fSkyViewLUTPassDescriptorSetLayout);

  fSkyViewLUTPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                         0,
                                                                         1,
                                                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                         [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                        fSkyViewLUTTexture.VulkanImage.Handle,
                                                                                                        VK_IMAGE_LAYOUT_GENERAL)],
                                                                         [],
                                                                         [],
                                                                         false);

  fSkyViewLUTPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                         0,
                                                                         1,
                                                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                         [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                        fTransmittanceTexture.VulkanImageView.Handle,
                                                                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                         [],
                                                                         [],
                                                                         false);

  fSkyViewLUTPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                         0,
                                                                         1,
                                                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                         [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                        fMultiScatteringTexture.VulkanImageView.Handle,
                                                                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                         [],
                                                                         [],
                                                                         false);

  fSkyViewLUTPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                         0,
                                                                         1,
                                                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                         [],
                                                                         [fAtmosphere.fAtmosphereParametersBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                         [],
                                                                         false);                                                             

  fSkyViewLUTPassDescriptorSets[InFlightFrameIndex].Flush;

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fSkyViewLUTPassDescriptorSets[InFlightFrameIndex].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'SkyViewLUTPassDescriptorSets['+IntToStr(InFlightFrameIndex)+']');

 end; 
 
 fCameraVolumePassDescriptorPool:=TpvVulkanDescriptorPool.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                               TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT) or
                                                               TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT),
                                                               TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fCameraVolumePassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fCameraVolumePassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*2);
 fCameraVolumePassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fCameraVolumePassDescriptorPool.Initialize;
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCameraVolumePassDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'CameraVolumePassDescriptorPool');

 for InFlightFrameIndex:=0 to TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames-1 do begin

  fCameraVolumePassDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fCameraVolumePassDescriptorPool,
                                                                                      TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).Atmospheres).fCameraVolumePassDescriptorSetLayout);

  fCameraVolumePassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                           0,
                                                                           1,
                                                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                           [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                          fCameraVolumeTexture.VulkanImage.Handle,
                                                                                                          VK_IMAGE_LAYOUT_GENERAL)],
                                                                           [],
                                                                           [],
                                                                           false);

  fCameraVolumePassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                           0,
                                                                           1,
                                                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                           [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                          fTransmittanceTexture.VulkanImageView.Handle,
                                                                                                          VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                           [],
                                                                           [],
                                                                           false);

  fCameraVolumePassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                           0,
                                                                           1,
                                                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                           [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                          fMultiScatteringTexture.VulkanImageView.Handle,
                                                                                                          VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                           [],
                                                                           [],
                                                                           false);

  fCameraVolumePassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                           0,
                                                                           1,
                                                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                           [],
                                                                           [fAtmosphere.fAtmosphereParametersBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                           [],
                                                                           false);                                                             

  fCameraVolumePassDescriptorSets[InFlightFrameIndex].Flush;

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCameraVolumePassDescriptorSets[InFlightFrameIndex].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'CameraVolumePassDescriptorSets['+IntToStr(InFlightFrameIndex)+']');

 end; 

 fRaymarchingPassDescriptorPool:=TpvVulkanDescriptorPool.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                               TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT) or
                                                               TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT),
                                                               TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fRaymarchingPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fRaymarchingPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*2);
 fRaymarchingPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fRaymarchingPassDescriptorPool.Initialize;
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fRaymarchingPassDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'RaymarchingPassDescriptorPool');

 for InFlightFrameIndex:=0 to TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames-1 do begin

  fRaymarchingPassDepthImageViews[InFlightFrameIndex]:=VK_NULL_HANDLE;

  fRaymarchingPassDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fRaymarchingPassDescriptorPool,
                                                                                     TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).Atmospheres).fRaymarchingPassDescriptorSetLayout);

  fRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                          0,
                                                                          1,
                                                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT),
                                                                          [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                         fCameraVolumeTexture.VulkanImage.Handle, // Dummy, will be replaced with the actual depth texture attachment
                                                                                                         VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                          [],
                                                                          [],
                                                                          false);

  fRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                          0,
                                                                          1,
                                                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                          [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                         fSkyViewLUTTexture.VulkanImageView.Handle,
                                                                                                         VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                          [],
                                                                          [],
                                                                          false);

  fRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                          0,
                                                                          1,
                                                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                          [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                         fCameraVolumeTexture.VulkanImageView.Handle,
                                                                                                         VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                          [],
                                                                          [],
                                                                          false);

  fRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                          0,
                                                                          1,
                                                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                          [],
                                                                          [fAtmosphere.fAtmosphereParametersBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                          [],
                                                                          false);                                                             

  fRaymarchingPassDescriptorSets[InFlightFrameIndex].Flush;

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fRaymarchingPassDescriptorSets[InFlightFrameIndex].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'RaymarchingPassDescriptorSets['+IntToStr(InFlightFrameIndex)+']');

 end;

 fGlobalDescriptorPool:=TpvVulkanDescriptorPool.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT) or
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT),
                                                       TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fGlobalDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fGlobalDescriptorPool.Initialize;

 for InFlightFrameIndex:=0 to TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames-1 do begin

  fGlobalDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fGlobalDescriptorPool,
                                                                           TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).Atmospheres).fGlobalVulkanDescriptorSetLayout);

  fGlobalDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                 [],
                                                                 [TpvScene3DRendererInstance(fRendererInstance).VulkanViewUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                 [],
                                                                 false);

  fGlobalDescriptorSets[InFlightFrameIndex].Flush;

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fGlobalDescriptorSets[InFlightFrameIndex].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'GlobalDescriptorSets['+IntToStr(InFlightFrameIndex)+']');

 end;

end;

destructor TpvScene3DAtmosphere.TRendererInstance.Destroy;
var InFlightFrameIndex:TpvSizeInt;
begin
 
 for InFlightFrameIndex:=0 to TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames-1 do begin
  FreeAndNil(fTransmittanceLUTPassDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fMultiScatteringLUTPassDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fSkyViewLUTPassDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fCameraVolumePassDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fRaymarchingPassDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fGlobalDescriptorSets[InFlightFrameIndex]);
 end;

 FreeAndNil(fTransmittanceLUTPassDescriptorPool);
 FreeAndNil(fMultiScatteringLUTPassDescriptorPool);
 FreeAndNil(fSkyViewLUTPassDescriptorPool);
 FreeAndNil(fCameraVolumePassDescriptorPool);
 FreeAndNil(fRaymarchingPassDescriptorPool);
 FreeAndNil(fGlobalDescriptorPool);

 FreeAndNil(fTransmittanceTexture);
 FreeAndNil(fMultiScatteringTexture);
 FreeAndNil(fSkyViewLUTTexture);
 FreeAndNil(fCameraVolumeTexture);

 inherited Destroy;

end;

procedure TpvScene3DAtmosphere.TRendererInstance.AfterConstruction;
begin
 inherited AfterConstruction;
 if assigned(fAtmosphere) and assigned(fAtmosphere.fRendererInstanceListLock) then begin
  fAtmosphere.fRendererInstanceListLock.Acquire;
  try
   fAtmosphere.fRendererInstances.Add(self);
   fAtmosphere.fRendererInstanceHashMap.Add(fKey,self);
  finally
   fAtmosphere.fRendererInstanceListLock.Release;
  end;
 end;
end;

procedure TpvScene3DAtmosphere.TRendererInstance.BeforeDestruction;
begin
 if assigned(fAtmosphere) and assigned(fAtmosphere.fRendererInstanceListLock) then begin
  fAtmosphere.fRendererInstanceListLock.Acquire;
  try
   fAtmosphere.fRendererInstanceHashMap.Delete(fKey);
   fAtmosphere.fRendererInstances.RemoveWithoutFree(self);
  finally
   fAtmosphere.fRendererInstanceListLock.Release;
  end;
 end;
 inherited BeforeDestruction;
end;

procedure TpvScene3DAtmosphere.TRendererInstance.Execute(const aInFlightFrameIndex:TpvSizeInt;
                                                         const aCommandBuffer:TpvVulkanCommandBuffer);
var AtmosphereGlobals:TpvScene3DAtmosphereGlobals;
    ImageSubresourceRange:TVkImageSubresourceRange;
    ImageMemoryBarriers:array[0..3] of TVkImageMemoryBarrier;
    TransmittanceLUTPushConstants:TpvScene3DAtmosphereGlobals.TTransmittanceLUTPushConstants;
begin

 AtmosphereGlobals:=TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).AtmosphereGlobals);

 begin

  // Transmittance LUT

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DAtmosphere.TransmittanceLUT',[1.0,0.0,0.0,1.0]);

  ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(0,
                                                       TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                       VK_IMAGE_LAYOUT_UNDEFINED,
                                                       VK_IMAGE_LAYOUT_GENERAL,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       fTransmittanceTexture.VulkanImage.Handle,
                                                       TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                       0,
                                                                                       1,
                                                                                       0,
                                                                                       TpvScene3DRendererInstance(fRendererInstance).CountSurfaceViews));

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    1,@ImageMemoryBarriers[0]);
  
  aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,AtmosphereGlobals.fTransmittanceLUTComputePipeline.Handle);

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       AtmosphereGlobals.fTransmittanceLUTComputePipelineLayout.Handle,
                                       0,
                                       1,
                                       @fTransmittanceLUTPassDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil);

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       AtmosphereGlobals.fTransmittanceLUTComputePipelineLayout.Handle,
                                       1,
                                       1,
                                       @fGlobalDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil);

  TransmittanceLUTPushConstants.BaseViewIndex:=0;
  TransmittanceLUTPushConstants.CountViews:=1;
  TransmittanceLUTPushConstants.Dummy0:=1;
  TransmittanceLUTPushConstants.Dummy1:=1;
  TransmittanceLUTPushConstants.SunDirection:=TpvVector4.InlineableCreate(TpvVector3.Origin,1.0);

  aCommandBuffer.CmdPushConstants(AtmosphereGlobals.fTransmittanceLUTComputePipelineLayout.Handle,
                                  TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_COMPUTE_BIT),
                                  0,
                                  SizeOf(TpvScene3DAtmosphereGlobals.TTransmittanceLUTPushConstants),
                                  @TransmittanceLUTPushConstants);

  aCommandBuffer.CmdDispatch((fTransmittanceTexture.Width+15) shr 4,
                             (fTransmittanceTexture.Height+15) shr 4,
                             TpvScene3DRendererInstance(fRendererInstance).CountSurfaceViews);


  ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                       TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                                       VK_IMAGE_LAYOUT_GENERAL,
                                                       VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       fTransmittanceTexture.VulkanImage.Handle,
                                                       TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                       0,
                                                                                       1,
                                                                                       0,
                                                                                       TpvScene3DRendererInstance(fRendererInstance).CountSurfaceViews));

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    1,@ImageMemoryBarriers[0]);

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

 end;

end;

{ TpvScene3DAtmosphere }

constructor TpvScene3DAtmosphere.Create(const aScene3D:TObject);
begin
 
 inherited Create;

 fScene3D:=aScene3D;
 
 fAtmosphereParameters.InitializeEarthAtmosphere;
 fPointerToAtmosphereParameters:=@fAtmosphereParameters;
 
 FillChar(fAtmosphereParametersBuffers,SizeOf(fAtmosphereParametersBuffers),#0);

 fRendererInstances:=TRendererInstances.Create(true);
 fRendererInstanceHashMap:=TRendererInstanceHashMap.Create(nil);
 fRendererInstanceListLock:=TPasMPSlimReaderWriterLock.Create;
 
 fReady:=false;

 fUploaded:=false;

 fVisible:=true;

 FillChar(fInFlightFrameVisible,SizeOf(fInFlightFrameVisible),#0);

end;

destructor TpvScene3DAtmosphere.Destroy;
begin
 FreeAndNil(fRendererInstances);
 FreeAndNil(fRendererInstanceHashMap);
 FreeAndNil(fRendererInstanceListLock);
 inherited Destroy;
end;

procedure TpvScene3DAtmosphere.AfterConstruction;
begin
 inherited AfterConstruction;
 if assigned(fScene3D) then begin
  TpvScene3DAtmospheres(TpvScene3D(fScene3D).Atmospheres).Lock.AcquireWrite;
  try
   TpvScene3DAtmospheres(TpvScene3D(fScene3D).Atmospheres).Add(self);
  finally
   TpvScene3DAtmospheres(TpvScene3D(fScene3D).Atmospheres).Lock.ReleaseWrite;
  end;
 end;
end;

procedure TpvScene3DAtmosphere.BeforeDestruction;
var Index:TpvSizeInt;
begin
 if assigned(fScene3D) then begin
  TpvScene3DAtmospheres(TpvScene3D(fScene3D).Atmospheres).Lock.AcquireWrite;
  try
   Index:=TpvScene3DAtmospheres(TpvScene3D(fScene3D).Atmospheres).IndexOf(self);
   if Index>=0 then begin
    TpvScene3DAtmospheres(TpvScene3D(fScene3D).Atmospheres).Extract(Index); // not delete or remove, since we don't want to free ourself here already.
   end;
  finally
   TpvScene3DAtmospheres(TpvScene3D(fScene3D).Atmospheres).Lock.ReleaseWrite;
  end;
 end;
 inherited BeforeDestruction;
end;

procedure TpvScene3DAtmosphere.Release;
begin
 if fReleaseFrameCounter<0 then begin
  fReleaseFrameCounter:=TpvScene3D(fScene3D).CountInFlightFrames;
  fReady:=false;
 end;
end;

function TpvScene3DAtmosphere.HandleRelease:boolean;
begin
 if fReleaseFrameCounter>0 then begin
  result:=TPasMPInterlocked.Decrement(fReleaseFrameCounter)=0;
  if result then begin
   Free;
  end;
 end else begin
  result:=false;
 end;
end;

procedure TpvScene3DAtmosphere.Upload;
var InFlightFrameIndex:TpvSizeInt;
begin
 
 if assigned(TpvScene3D(fScene3D).VulkanDevice) and not fUploaded then begin
 
  for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin

   fAtmosphereParametersBuffers[InFlightFrameIndex]:=TpvVulkanBuffer.Create(TpvScene3D(fScene3D).VulkanDevice,
                                                                            SizeOf(TAtmosphereParameters),
                                                                            TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or
                                                                            TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or
                                                                            TVkBufferUsageFlags(VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_KHR),
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
                                                                            [TpvVulkanBufferFlag.PersistentMappedIfPossible,TpvVulkanBufferFlag.OwnSingleMemoryChunk,TpvVulkanBufferFlag.DedicatedAllocation],
                                                                            0,
                                                                            pvAllocationGroupIDScene3DDynamic);
   TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fAtmosphereParametersBuffers[InFlightFrameIndex].Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DAtmosphere.fAtmosphereParametersBuffers['+IntToStr(InFlightFrameIndex)+']');
 
  end;

  fUploaded:=true;
 
 end;

end;

procedure TpvScene3DAtmosphere.Unload;
var InFlightFrameIndex:TpvSizeInt;
begin
 if fUploaded then begin
  for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
   FreeAndNil(fAtmosphereParametersBuffers[InFlightFrameIndex]);
  end;
  fUploaded:=false;
 end;
end;

procedure TpvScene3DAtmosphere.Update(const aInFlightFrameIndex:TpvSizeInt;
                                      const aTransferQueue:TpvVulkanQueue;
                                      const aTransferCommandBuffer:TpvVulkanCommandBuffer;
                                      const aTransferFence:TpvVulkanFence);
var IsVisible:boolean;
begin

 IsVisible:=fReady and fUploaded;

 if IsVisible then begin

  if assigned(fAtmosphereParametersBuffers[aInFlightFrameIndex]) then begin
   TpvScene3D(fScene3D).VulkanDevice.MemoryStaging.Upload(aTransferQueue,
                                                          aTransferCommandBuffer,
                                                          aTransferFence,
                                                          fAtmosphereParameters,
                                                          fAtmosphereParametersBuffers[aInFlightFrameIndex],
                                                          0,
                                                          SizeOf(TAtmosphereParameters));
  end;

 end;

 fInFlightFrameVisible[aInFlightFrameIndex]:=IsVisible;
 
end;

procedure TpvScene3DAtmosphere.Execute(const aInFlightFrameIndex:TpvSizeInt;
                                       const aCommandBuffer:TpvVulkanCommandBuffer;
                                       const aRendererInstance:TObject);
var AtmosphereRendererInstance:TpvScene3DAtmosphere.TRendererInstance;
    AtmosphereRendererInstanceKey:TpvScene3DAtmosphere.TRendererInstance.TKey;
begin

 if fInFlightFrameVisible[aInFlightFrameIndex] then begin

  AtmosphereRendererInstanceKey:=TpvScene3DAtmosphere.TRendererInstance.TKey.Create(aRendererInstance);
  AtmosphereRendererInstance:=fRendererInstanceHashMap[AtmosphereRendererInstanceKey];
  if not assigned(AtmosphereRendererInstance) then begin
   AtmosphereRendererInstance:=TpvScene3DAtmosphere.TRendererInstance.Create(self,aRendererInstance);
  end;

  if assigned(AtmosphereRendererInstance) then begin
   AtmosphereRendererInstance.Execute(aInFlightFrameIndex,aCommandBuffer);
  end;
   
 end;

end;

{ TpvScene3DAtmospheres }

constructor TpvScene3DAtmospheres.Create(const aScene3D:TObject);
begin
 inherited Create(true);
 fScene3D:=aScene3D;
 fLock:=TPasMPMultipleReaderSingleWriterLock.Create;
end;

destructor TpvScene3DAtmospheres.Destroy;
begin
 FreeAndNil(fLock);
 inherited Destroy;
end;

procedure TpvScene3DAtmospheres.ProcessReleases;
var Index:TpvInt32;
    Atmosphere:TpvScene3DAtmosphere;
begin
 // Going backwards through the list, because we will remove items from the list
 fLock.AcquireRead;
 try
  Index:=Count;
  while Index>0 do begin
   dec(Index);
   Atmosphere:=Items[Index];
   if assigned(Atmosphere) then begin
    fLock.ReleaseRead;
    try
     Atmosphere.HandleRelease;
    finally
     fLock.AcquireRead;
    end;
   end;
  end;
 finally
  fLock.ReleaseRead;
 end; 
end;

{ TpvScene3DAtmosphereGlobals }

constructor TpvScene3DAtmosphereGlobals.Create(const aScene3D:TObject);
begin
 inherited Create;
 fScene3D:=aScene3D;
 fAtmospheres:=TpvScene3DAtmospheres(TpvScene3D(fScene3D).Atmospheres);
 fTransmittanceLUTPassDescriptorSetLayout:=nil;
 fMultiScatteringLUTPassDescriptorSetLayout:=nil;
 fSkyViewLUTPassDescriptorSetLayout:=nil;
 fCameraVolumePassDescriptorSetLayout:=nil;
 fRaymarchingPassDescriptorSetLayout:=nil;
 fGlobalVulkanDescriptorSetLayout:=nil;
end;

destructor TpvScene3DAtmosphereGlobals.Destroy;
begin
 DeallocateResources;
 inherited Destroy;
end;

procedure TpvScene3DAtmosphereGlobals.AllocateResources;
var Stream:TStream;
begin

 // Transmittance LUT pass descriptor set layout 
 begin

  fTransmittanceLUTPassDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(TpvScene3D(fScene3D).VulkanDevice);

  // Destination texture
  fTransmittanceLUTPassDescriptorSetLayout.AddBinding(0,
                                                      VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                                      1,
                                                      TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                      []);

  // Atmosphere parameters
  fTransmittanceLUTPassDescriptorSetLayout.AddBinding(1,
                                                      VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                      1,
                                                      TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                      []);

  fTransmittanceLUTPassDescriptorSetLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fTransmittanceLUTPassDescriptorSetLayout.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,'TpvScene3DAtmosphereGlobals.fTransmittanceLUTPassDescriptorSetLayout');

 end;

 // Multi scattering LUT pass descriptor set layout
 begin

  fMultiScatteringLUTPassDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(TpvScene3D(fScene3D).VulkanDevice);

  // Destination texture
  fMultiScatteringLUTPassDescriptorSetLayout.AddBinding(0,
                                                        VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                                        1,
                                                        TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                        []);

  // Transmittance LUT texture (previous)
  fMultiScatteringLUTPassDescriptorSetLayout.AddBinding(1,
                                                        VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                        1,
                                                        TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                        []);

  // Atmosphere parameters
  fMultiScatteringLUTPassDescriptorSetLayout.AddBinding(2,
                                                        VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                        1,
                                                        TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                        []);

  fMultiScatteringLUTPassDescriptorSetLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fMultiScatteringLUTPassDescriptorSetLayout.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,'TpvScene3DAtmosphereGlobals.fMultiScatteringLUTPassDescriptorSetLayout');

 end;
 
 // Sky view LUT pass descriptor set layout
 begin

  fSkyViewLUTPassDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(TpvScene3D(fScene3D).VulkanDevice);

  // Destination texture
  fSkyViewLUTPassDescriptorSetLayout.AddBinding(0,
                                                VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                                1,
                                                TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                []);

  // Transmittance LUT texture (previous)
  fSkyViewLUTPassDescriptorSetLayout.AddBinding(1,
                                                VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                1,
                                                TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                []);

  // Multi scattering LUT texture (previous)
  fSkyViewLUTPassDescriptorSetLayout.AddBinding(2,
                                                VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                1,
                                                TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                []);

  // Atmosphere parameters
  fSkyViewLUTPassDescriptorSetLayout.AddBinding(3,
                                                VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                1,
                                                TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                []);

  fSkyViewLUTPassDescriptorSetLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fSkyViewLUTPassDescriptorSetLayout.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,'TpvScene3DAtmosphereGlobals.fSkyViewLUTPassDescriptorSetLayout');

 end;

 // Camera volume pass descriptor set layout
 begin

  fCameraVolumePassDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(TpvScene3D(fScene3D).VulkanDevice);

  // Destination texture
  fCameraVolumePassDescriptorSetLayout.AddBinding(0,
                                                  VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                                  1,
                                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                  []);

  // Transmittance LUT texture (previous)
  fCameraVolumePassDescriptorSetLayout.AddBinding(1,
                                                  VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                  1,
                                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                  []);

  // Multi scattering LUT texture (previous)
  fCameraVolumePassDescriptorSetLayout.AddBinding(2,
                                                  VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                  1,
                                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                  []);

  // Atmosphere parameters
  fCameraVolumePassDescriptorSetLayout.AddBinding(3,
                                                  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                  1,
                                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                  []);

  fCameraVolumePassDescriptorSetLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCameraVolumePassDescriptorSetLayout.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,'TpvScene3DAtmosphereGlobals.fCameraVolumePassDescriptorSetLayout');

 end;

 // Raymarching pass descriptor set layout
 begin
  
  fRaymarchingPassDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(TpvScene3D(fScene3D).VulkanDevice);

  // Subpass depth
  fRaymarchingPassDescriptorSetLayout.AddBinding(0,
                                                 VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
                                                 1,
                                                 TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                 []);

  // Sky view LUT texture
  fRaymarchingPassDescriptorSetLayout.AddBinding(1,
                                                 VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                 1,
                                                 TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                 []);

  // Camera volume texture
  fRaymarchingPassDescriptorSetLayout.AddBinding(2,
                                                 VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                 1,
                                                 TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                 []);

  // Atmosphere parameters
  fRaymarchingPassDescriptorSetLayout.AddBinding(3,
                                                 VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                 1,
                                                 TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                 []);

  fRaymarchingPassDescriptorSetLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fRaymarchingPassDescriptorSetLayout.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,'TpvScene3DAtmosphereGlobals.fRaymarchingPassDescriptorSetLayout');

 end; 

 // Global Vulkan descriptor set layout
 begin

  fGlobalVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(TpvScene3D(fScene3D).VulkanDevice);

  // Views
  fGlobalVulkanDescriptorSetLayout.AddBinding(0,
                                              VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                              1,
                                              TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT) or
                                              TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                              []);

  fGlobalVulkanDescriptorSetLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fGlobalVulkanDescriptorSetLayout.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,'TpvScene3DAtmosphereGlobals.fGlobalVulkanDescriptorSetLayout');

 end;
 
 begin

  // Transmittance LUT compute pipeline

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('atmosphere_transmittancelut_comp.spv');
  try
   fTransmittanceLUTComputeShaderModule:=TpvVulkanShaderModule.Create(TpvScene3D(fScene3D).VulkanDevice,Stream);
  finally
   Stream.Free;
  end;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fTransmittanceLUTComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DAtmosphereGlobals.fTransmittanceLUTComputeShaderModule');

  fTransmittanceLUTComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fTransmittanceLUTComputeShaderModule,'main');

  fTransmittanceLUTComputePipelineLayout:=TpvVulkanPipelineLayout.Create(TpvScene3D(fScene3D).VulkanDevice);
  fTransmittanceLUTComputePipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TTransmittanceLUTPushConstants));
  fTransmittanceLUTComputePipelineLayout.AddDescriptorSetLayout(fTransmittanceLUTPassDescriptorSetLayout);
  fTransmittanceLUTComputePipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
  fTransmittanceLUTComputePipelineLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fTransmittanceLUTComputePipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DAtmosphereGlobals.fTransmittanceLUTComputePipelineLayout');

  fTransmittanceLUTComputePipeline:=TpvVulkanComputePipeline.Create(TpvScene3D(fScene3D).VulkanDevice,
                                                                    TpvScene3D(fScene3D).VulkanPipelineCache,
                                                                    0,
                                                                    fTransmittanceLUTComputeShaderStage,
                                                                    fTransmittanceLUTComputePipelineLayout,
                                                                    nil,
                                                                    0);
 end;

 begin

  // Multi scattering LUT compute pipeline

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('atmosphere_multiscattering_comp.spv');
  try
   fMultiScatteringLUTComputeShaderModule:=TpvVulkanShaderModule.Create(TpvScene3D(fScene3D).VulkanDevice,Stream);
  finally
   Stream.Free;
  end;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fMultiScatteringLUTComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DAtmosphereGlobals.fMultiScatteringLUTComputeShaderModule');

  fMultiScatteringLUTComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fMultiScatteringLUTComputeShaderModule,'main');

  fMultiScatteringLUTComputePipelineLayout:=TpvVulkanPipelineLayout.Create(TpvScene3D(fScene3D).VulkanDevice);
  fMultiScatteringLUTComputePipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TMultipleScatteringLUTPushConstants));
  fMultiScatteringLUTComputePipelineLayout.AddDescriptorSetLayout(fMultiScatteringLUTPassDescriptorSetLayout);
  fMultiScatteringLUTComputePipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
  fMultiScatteringLUTComputePipelineLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fMultiScatteringLUTComputePipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DAtmosphereGlobals.fMultiScatteringLUTComputePipelineLayout');

  fMultiScatteringLUTComputePipeline:=TpvVulkanComputePipeline.Create(TpvScene3D(fScene3D).VulkanDevice,
                                                                      TpvScene3D(fScene3D).VulkanPipelineCache,
                                                                      0,
                                                                      fMultiScatteringLUTComputeShaderStage,
                                                                      fMultiScatteringLUTComputePipelineLayout,
                                                                      nil,
                                                                      0);
 end;

 begin

  // Sky view LUT compute pipeline

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('atmosphere_skyviewlut_comp.spv');
  try
   fSkyViewLUTComputeShaderModule:=TpvVulkanShaderModule.Create(TpvScene3D(fScene3D).VulkanDevice,Stream);
  finally
   Stream.Free;
  end;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fSkyViewLUTComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DAtmosphereGlobals.fSkyViewLUTComputeShaderModule');

  fSkyViewLUTComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fSkyViewLUTComputeShaderModule,'main');

  fSkyViewLUTComputePipelineLayout:=TpvVulkanPipelineLayout.Create(TpvScene3D(fScene3D).VulkanDevice);
  fSkyViewLUTComputePipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TSkyViewLUTPushConstants));
  fSkyViewLUTComputePipelineLayout.AddDescriptorSetLayout(fSkyViewLUTPassDescriptorSetLayout);
  fSkyViewLUTComputePipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
  fSkyViewLUTComputePipelineLayout.Initialize;

  fSkyViewLUTComputePipeline:=TpvVulkanComputePipeline.Create(TpvScene3D(fScene3D).VulkanDevice,
                                                              TpvScene3D(fScene3D).VulkanPipelineCache,
                                                              0,
                                                              fSkyViewLUTComputeShaderStage,
                                                              fSkyViewLUTComputePipelineLayout,
                                                              nil,
                                                              0);
 end;

 begin

  // Camera volume compute pipeline

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('atmosphere_cameravolume_comp.spv');
  try
   fCameraVolumeComputeShaderModule:=TpvVulkanShaderModule.Create(TpvScene3D(fScene3D).VulkanDevice,Stream);
  finally
   Stream.Free;
  end;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCameraVolumeComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DAtmosphereGlobals.fCameraVolumeComputeShaderModule');

  fCameraVolumeComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fCameraVolumeComputeShaderModule,'main');
  
  fCameraVolumeComputePipelineLayout:=TpvVulkanPipelineLayout.Create(TpvScene3D(fScene3D).VulkanDevice);
  fCameraVolumeComputePipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TCameraVolumePushConstants));
  fCameraVolumeComputePipelineLayout.AddDescriptorSetLayout(fCameraVolumePassDescriptorSetLayout);
  fCameraVolumeComputePipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
  fCameraVolumeComputePipelineLayout.Initialize;

  fCameraVolumeComputePipeline:=TpvVulkanComputePipeline.Create(TpvScene3D(fScene3D).VulkanDevice,
                                                                TpvScene3D(fScene3D).VulkanPipelineCache,
                                                                0,
                                                                fCameraVolumeComputeShaderStage,
                                                                fCameraVolumeComputePipelineLayout,
                                                                nil,
                                                                0);
 end;


end;

procedure TpvScene3DAtmosphereGlobals.DeallocateResources;
begin
 FreeAndNil(fTransmittanceLUTComputePipeline);
 FreeAndNil(fTransmittanceLUTComputePipelineLayout);
 FreeAndNil(fTransmittanceLUTComputeShaderStage);
 FreeAndNil(fTransmittanceLUTComputeShaderModule);
 FreeAndNil(fMultiScatteringLUTComputePipeline);
 FreeAndNil(fMultiScatteringLUTComputePipelineLayout);
 FreeAndNil(fMultiScatteringLUTComputeShaderStage);
 FreeAndNil(fMultiScatteringLUTComputeShaderModule);
 FreeAndNil(fSkyViewLUTComputePipeline);
 FreeAndNil(fSkyViewLUTComputePipelineLayout);
 FreeAndNil(fSkyViewLUTComputeShaderStage);
 FreeAndNil(fSkyViewLUTComputeShaderModule);
 FreeAndNil(fCameraVolumeComputePipeline);
 FreeAndNil(fCameraVolumeComputePipelineLayout);
 FreeAndNil(fCameraVolumeComputeShaderStage);
 FreeAndNil(fCameraVolumeComputeShaderModule);
 FreeAndNil(fGlobalVulkanDescriptorSetLayout);
 FreeAndNil(fRaymarchingPassDescriptorSetLayout);
 FreeAndNil(fCameraVolumePassDescriptorSetLayout);
 FreeAndNil(fSkyViewLUTPassDescriptorSetLayout);
 FreeAndNil(fMultiScatteringLUTPassDescriptorSetLayout);
 FreeAndNil(fTransmittanceLUTPassDescriptorSetLayout); 
end;

{ TpvScene3DAtmosphereRendererInstance.TTransmittanceLUTPass }

constructor TpvScene3DAtmosphereRendererInstance.TTransmittanceLUTPass.Create(const aAtmosphereRendererInstance:TpvScene3DAtmosphereRendererInstance);
begin

 inherited Create;

 fAtmosphereRendererInstance:=aAtmosphereRendererInstance;
 
 fPipeline:=nil;

end;

destructor TpvScene3DAtmosphereRendererInstance.TTransmittanceLUTPass.Destroy;
begin
 FreeAndNil(fPipeline);
 inherited Destroy;
end;

procedure TpvScene3DAtmosphereRendererInstance.TTransmittanceLUTPass.AfterConstruction;
begin
 inherited AfterConstruction;
end;

procedure TpvScene3DAtmosphereRendererInstance.TTransmittanceLUTPass.BeforeDestruction;
begin
 inherited BeforeDestruction;
end;

procedure TpvScene3DAtmosphereRendererInstance.TTransmittanceLUTPass.Execute(const aVulkanCommandBuffer:TpvVulkanCommandBuffer);
begin  
end;

{ TpvScene3DAtmosphereRendererInstance }

constructor TpvScene3DAtmosphereRendererInstance.Create(const aScene3D,aRenderer,aRendererInstance:TObject);
begin
 inherited Create;
 fScene3D:=aScene3D;
 fRenderer:=aRenderer;
 fRendererInstance:=aRendererInstance;
end;

destructor TpvScene3DAtmosphereRendererInstance.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DAtmosphereRendererInstance.AfterConstruction;
begin
 inherited AfterConstruction;
end;

procedure TpvScene3DAtmosphereRendererInstance.BeforeDestruction;
begin
 inherited BeforeDestruction;
end;

procedure TpvScene3DAtmosphereRendererInstance.AllocateResources;
begin
end;

procedure TpvScene3DAtmosphereRendererInstance.DeallocateResources;
begin
end;

end.
