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
     POCA,
     PUCU,
     PasMP,
     PasJSON,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Application,
     PasVulkan.Framework,
     PasVulkan.Collections,
     PasVulkan.Scene3D.Renderer.Image2D,
     PasVulkan.Scene3D.Renderer.Array2DImage,
     PasVulkan.Scene3D.Renderer.MipmapImageCubeMap,
     PasVulkan.Scene3D.Renderer.CubeMapMipMapGenerator;

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
             SkyViewLUTTextureWidth=256;
             SkyViewLUTTextureHeight=128;
             CameraVolumeTextureWidth=32;
             CameraVolumeTextureHeight=32;
             CameraVolumeTextureDepth=32;
             CubeMapTextureSize=32;
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
              SunDirection:TpvVector4; // w is unused, for alignment
              SolarIrradiance:TpvVector4; // w is unused, for alignment
              RayleighScattering:TpvVector4; // w is unused, for alignment
              MieScattering:TpvVector4; // w is unused, for alignment
              MieExtinction:TpvVector4; // w is unused, for alignment
              AbsorptionExtinction:TpvVector4; // w is unused, for alignment
              GroundAlbedo:TpvVector4; // w is unused, for alignment
              Intensity:TpvFloat;
              MiePhaseFunctionG:TpvFloat;
              SunAngularRadius:TpvFloat;
              BottomRadius:TpvFloat;
              TopRadius:TpvFloat;
              MuSMin:TpvFloat;
              RaymarchingMinSteps:TpvInt32;
              RaymarchingMaxSteps:TpvInt32;
              procedure InitializeEarthAtmosphere(const aEarthBottomRadius:TpvFloat=6360.0;
                                                  const aEarthTopRadius:TpvFloat=6460.0;
                                                  const aEarthRayleighScaleHeight:TpvFloat=8.0;
                                                  const aEarthMieScaleHeight:TpvFloat=1.2);
              procedure LoadFromJSON(const aJSON:TPasJSONItem);
              procedure LoadFromJSONStream(const aStream:TStream);
              procedure LoadFromJSONFile(const aFileName:string);
              function SaveToJSON:TPasJSONItemObject;
              procedure SaveToJSONStream(const aStream:TStream);
              procedure SaveToJSONFile(const aFileName:string);
              procedure LoadFromPOCA(const aPOCACode:TpvUTF8String);
              procedure LoadFromPOCAStream(const aStream:TStream);
              procedure LoadFromPOCAFile(const aFileName:string);
            end;
            PAtmosphereParameters=^TAtmosphereParameters;
            { TGPUAtmosphereParameters }
            TGPUAtmosphereParameters=packed record
             public
              Transform:TpvMatrix4x4; // Transform of the atmosphere for the case that the atmosphere is not centered at the origin (e.g. multiple planets)
              InverseTransform:TpvMatrix4x4; // Transform of the atmosphere for the case that the atmosphere is not centered at the origin (e.g. multiple planets)
              RayleighScattering:TpvVector4; // w = Mu_S_min
              MieScattering:TpvVector4; // w = sun direction X
              MieExtinction:TpvVector4; // w = sun direction Y
              MieAbsorption:TpvVector4; // w = sun direction Z
              AbsorptionExtinction:TpvVector4;
              GroundAlbedo:TpvVector4; // w = intensity
              SolarIrradiance:TpvVector4;
              BottomRadius:TpvFloat;
              TopRadius:TpvFloat;
              RayleighDensityExpScale:TpvFloat;
              MieDensityExpScale:TpvFloat;
              MiePhaseG:TpvFloat;
              AbsorptionDensity0LayerWidth:TpvFloat;
              AbsorptionDensity0ConstantTerm:TpvFloat;
              AbsorptionDensity0LinearTerm:TpvFloat;
              AbsorptionDensity1ConstantTerm:TpvFloat;
              AbsorptionDensity1LinearTerm:TpvFloat;              
              RaymarchingMinSteps:TpvInt32;
              RaymarchingMaxSteps:TpvInt32;
              procedure Assign(const aAtmosphereParameters:TAtmosphereParameters);
            end;
            PGPUAtmosphereParameters=^TGPUAtmosphereParameters;
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
              fTransmittanceTexture:TpvScene3DRendererImage2D;
              fMultiScatteringTexture:TpvScene3DRendererArray2DImage;
              fSkyViewLUTTexture:TpvScene3DRendererArray2DImage;
              fCameraVolumeTexture:TpvScene3DRendererArray2DImage;
              fCubeMapTexture:TpvScene3DRendererMipmapImageCubeMap;
              fGGXCubeMapTexture:TpvScene3DRendererMipmapImageCubeMap;
              fCharlieCubeMapTexture:TpvScene3DRendererMipmapImageCubeMap;
              fLambertianCubeMapTexture:TpvScene3DRendererMipmapImageCubeMap;
              fTransmittanceLUTPassDescriptorPool:TpvVulkanDescriptorPool;
              fTransmittanceLUTPassDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fMultiScatteringLUTPassDescriptorPool:TpvVulkanDescriptorPool;
              fMultiScatteringLUTPassDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fSkyViewLUTPassDescriptorPool:TpvVulkanDescriptorPool;
              fSkyViewLUTPassDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fCameraVolumePassDescriptorPool:TpvVulkanDescriptorPool;
              fCameraVolumePassDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fCubeMapPassDescriptorPool:TpvVulkanDescriptorPool;
              fCubeMapPassDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fRaymarchingPassDescriptorPool:TpvVulkanDescriptorPool;
              fRaymarchingPassDepthImageViews:array[0..MaxInFlightFrames-1] of TVkImageView;
              fRaymarchingPassDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fRaymarchingPassDescriptorSetFirsts:array[0..MaxInFlightFrames-1] of boolean;
              fGlobalDescriptorPool:TpvVulkanDescriptorPool;
              fGlobalDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fRaymarchingGraphicsPipeline:TpvVulkanGraphicsPipeline;
             public
              constructor Create(const aAtmosphere:TpvScene3DAtmosphere;const aRendererInstance:TObject);
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure SetDepthImageView(const aInFlightFrameIndex:TpvSizeInt;
                                          const aDepthImageView:TVkImageView);
              procedure Setup(const aRenderPass:TpvVulkanRenderPass;
                              const aRenderPassSubpassIndex:TpvSizeInt;
                              const aSampleCount:TVkSampleCountFlagBits;
                              const aWidth:TpvSizeInt;
                              const aHeight:TpvSizeInt);
              procedure ReleaseGraphicsPipeline;
              procedure Execute(const aInFlightFrameIndex:TpvSizeInt;
                                const aCommandBuffer:TpvVulkanCommandBuffer);
              procedure Draw(const aInFlightFrameIndex:TpvSizeInt;
                             const aCommandBuffer:TpvVulkanCommandBuffer;
                             const aDepthImageView:TVkImageView);
             published
              property Atmosphere:TpvScene3DAtmosphere read fAtmosphere;
              property RendererInstance:TObject read fRendererInstance;
              property TransmittanceTexture:TpvScene3DRendererImage2D read fTransmittanceTexture;
              property MultiScatteringTexture:TpvScene3DRendererArray2DImage read fMultiScatteringTexture;
              property SkyViewLUTTexture:TpvScene3DRendererArray2DImage read fSkyViewLUTTexture;
              property CameraVolumeTexture:TpvScene3DRendererArray2DImage read fCameraVolumeTexture; 
              property CubeMapTexture:TpvScene3DRendererMipmapImageCubeMap read fCubeMapTexture;
              property GGXCubeMapTexture:TpvScene3DRendererMipmapImageCubeMap read fGGXCubeMapTexture;
              property CharlieCubeMapTexture:TpvScene3DRendererMipmapImageCubeMap read fCharlieCubeMapTexture;
              property LambertianCubeMapTexture:TpvScene3DRendererMipmapImageCubeMap read fLambertianCubeMapTexture;
            end;
            { TRendererInstances }
            TRendererInstances=TpvObjectGenericList<TRendererInstance>;
            { TRendererInstanceHashMap }
            TRendererInstanceHashMap=TpvHashMap<TRendererInstance.TKey,TRendererInstance>;
      private
       fScene3D:TObject;
       fAtmosphereParameters:TAtmosphereParameters;
       fPointerToAtmosphereParameters:PAtmosphereParameters;
       fGPUAtmosphereParameters:TGPUAtmosphereParameters;
       fAtmosphereParametersBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fRendererInstances:TRendererInstances;
       fRendererInstanceHashMap:TRendererInstanceHashMap;
       fRendererInstanceListLock:TPasMPSlimReaderWriterLock;
       fToDestroy:boolean;
       fReleaseFrameCounter:TpvInt32;
       fReady:TPasMPBool32;
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
       procedure Draw(const aInFlightFrameIndex:TpvSizeInt;
                      const aCommandBuffer:TpvVulkanCommandBuffer;
                      const aDepthImageView:TVkImageView; 
                      const aRendererInstance:TObject);
      public
       property AtmosphereParameters:PAtmosphereParameters read fPointerToAtmosphereParameters;
       property Ready:TPasMPBool32 read fReady;
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
       procedure Draw(const aInFlightFrameIndex:TpvSizeInt;
                      const aCommandBuffer:TpvVulkanCommandBuffer;
                      const aDepthImageView:TVkImageView;
                      const aRendererInstance:TObject);
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
            end;
            PTransmittanceLUTPushConstants=^TTransmittanceLUTPushConstants;
            TMultiScatteringLUTPushConstants=packed record
             BaseViewIndex:TpvInt32;
             CountViews:TpvInt32;
             MultipleScatteringFactor:TpvFloat;
             FrameIndex:TpvUInt32;
            end;
            PMultiScatteringLUTPushConstants=^TMultiScatteringLUTPushConstants;
            TSkyViewLUTPushConstants=packed record
             BaseViewIndex:TpvInt32;
             CountViews:TpvInt32;
             FrameIndex:TpvUInt32;
             Dummy1:TpvInt32;
            end;
            PSkyViewLUTPushConstants=^TSkyViewLUTPushConstants;
            TCameraVolumePushConstants=packed record
             BaseViewIndex:TpvInt32;
             CountViews:TpvInt32;
             FrameIndex:TpvUInt32;
             Dummy1:TpvInt32;
            end;
            TCubeMapPushConstants=packed record
             CameraPosition:TpvVector4; // w = unused, for alignment
             UpVector:TpvVector4; // w = unused, for alignment
            end;
            PCubeMapPushConstants=^TCubeMapPushConstants;
            TRaymarchingPushConstants=packed record
             BaseViewIndex:TpvInt32;
             CountViews:TpvInt32;
             FrameIndex:TpvUInt32;
             Flags:TpvUInt32;
            end;
            PRaymarchingPushConstants=^TRaymarchingPushConstants;
      private
       fScene3D:TObject;
       fAtmospheres:TpvScene3DAtmospheres;
       fTransmittanceLUTPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fMultiScatteringLUTPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fSkyViewLUTPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fCameraVolumePassDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fCubeMapPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
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
       fCubeMapComputeShaderModule:TpvVulkanShaderModule;
       fCubeMapComputeShaderStage:TpvVulkanPipelineShaderStage;
       fCubeMapComputePipelineLayout:TpvVulkanPipelineLayout;
       fCubeMapComputePipeline:TpvVulkanComputePipeline;
       fRaymarchingVertexShaderModule:TpvVulkanShaderModule;
       fRaymarchingFragmentShaderModules:array[boolean] of TpvVulkanShaderModule; // false = Non-MSAA, true = MSAA
       fRaymarchingPipelineShaderStageVertex:TpvVulkanPipelineShaderStage;
       fRaymarchingPipelineShaderStageFragments:array[boolean] of TpvVulkanPipelineShaderStage; // false = Non-MSAA, true = MSAA
       fRaymarchingPipelineLayout:TpvVulkanPipelineLayout;
      public
       constructor Create(const aScene3D:TObject);
       destructor Destroy; override;
       procedure AllocateResources;
       procedure DeallocateResources;
      published
       property Scene3D:TObject read fScene3D;
       property Atmospheres:TpvScene3DAtmospheres read fAtmospheres;
      public
       property TransmittanceLUTPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fTransmittanceLUTPassDescriptorSetLayout;
       property MultiScatteringLUTPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fMultiScatteringLUTPassDescriptorSetLayout;
       property SkyViewLUTPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fSkyViewLUTPassDescriptorSetLayout;
       property CameraVolumePassDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fCameraVolumePassDescriptorSetLayout;
       property CubeMapPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fCubeMapPassDescriptorSetLayout;
       property RaymarchingPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fRaymarchingPassDescriptorSetLayout;
       property GlobalVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fGlobalVulkanDescriptorSetLayout;
       property RaymarchingPipelineLayout:TpvVulkanPipelineLayout read fRaymarchingPipelineLayout;
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
     PasVulkan.Scene3D.Renderer.Instance,
     PasVulkan.JSON;

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

procedure TpvScene3DAtmosphere.TAtmosphereParameters.InitializeEarthAtmosphere(const aEarthBottomRadius:TpvFloat;
                                                                               const aEarthTopRadius:TpvFloat;
                                                                               const aEarthRayleighScaleHeight:TpvFloat;
                                                                               const aEarthMieScaleHeight:TpvFloat);
begin
 
 // Transform
 Transform:=TpvMatrix4x4.Identity;

 // Center
 Center:=TpvVector4.Origin;

 // Sun direction
 SunDirection:=TpvVector4.InlineableCreate(0.0,0.90045,0.43497,0.0).Normalize;

 // Sun
 SolarIrradiance:=TpvVector4.InlineableCreate(1.0,1.0,1.0,1.0);
 SunAngularRadius:=0.004675;

 // Planet
 BottomRadius:=aEarthBottomRadius;
 TopRadius:=aEarthTopRadius;
 GroundAlbedo:=TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0);

 // Intensity 
 Intensity:=1.0;

 // Rayleigh scattering
 RayleighDensity.Layers[0]:=TDensityProfileLayer.Create(0.0,0.0,0.0,0.0,0.0);
 RayleighDensity.Layers[1]:=TDensityProfileLayer.Create(0.0,1.0,-1.0/aEarthRayleighScaleHeight,0.0,0.0);
 RayleighScattering:=TpvVector4.InlineableCreate(0.005802,0.013558,0.033100,0.0);

 // Mie scattering
 MieDensity.Layers[0]:=TDensityProfileLayer.Create(0.0,0.0,0.0,0.0,0.0);
 MieDensity.Layers[1]:=TDensityProfileLayer.Create(0.0,1.0,-1.0/aEarthMieScaleHeight,0.0,0.0);
 MieScattering:=TpvVector4.InlineableCreate(0.003996,0.003996,0.003996,0.0);
 MieExtinction:=TpvVector4.InlineableCreate(0.004440,0.004440,0.004440,0.0);
 MiePhaseFunctionG:=0.8;

 // Absorption extinction / Ozone layer
 AbsorptionDensity.Layers[0]:=TDensityProfileLayer.Create(25.0,0.0,0.0,1.0/15.0,-2.0/3.0);
 AbsorptionDensity.Layers[1]:=TDensityProfileLayer.Create(0.0,0.0,0.0,-1.0/15.0,8.0/3.0);
 AbsorptionExtinction:=TpvVector4.InlineableCreate(0.000650,0.001881,0.000085,0.0);

 // MuSMin
 MuSMin:=cos(PI*120.0/180.0);

 // Raymarching min/max steps
 RaymarchingMinSteps:=4;
 RaymarchingMaxSteps:=14;

end;

procedure TpvScene3DAtmosphere.TAtmosphereParameters.LoadFromJSON(const aJSON:TPasJSONItem);
var JSONRootObject:TPasJSONItemObject;
    JSON:TPasJSONItem;
    Factor:TpvFloat;
 procedure LoadDensityProfileLayer(const aJSON:TPasJSONItem;var aLayer:TDensityProfileLayer);
 var JSONLayer:TPasJSONItemObject;
 begin
  if assigned(aJSON) and (aJSON is TPasJSONItemObject) then begin
   JSONLayer:=TPasJSONItemObject(aJSON);
   aLayer.Width:=TPasJSON.GetNumber(JSONLayer.Properties['width'],aLayer.Width);
   aLayer.ExpTerm:=TPasJSON.GetNumber(JSONLayer.Properties['expterm'],aLayer.ExpTerm);
   aLayer.ExpScale:=TPasJSON.GetNumber(JSONLayer.Properties['expscale'],aLayer.ExpScale);
   aLayer.LinearTerm:=TPasJSON.GetNumber(JSONLayer.Properties['linearterm'],aLayer.LinearTerm);
   aLayer.ConstantTerm:=TPasJSON.GetNumber(JSONLayer.Properties['constantterm'],aLayer.ConstantTerm);
  end;  
 end;
 procedure LoadRaymarching(const aJSON:TPasJSONItem);
 var JSONObject:TPasJSONItemObject;
 begin
  if assigned(aJSON) and (aJSON is TPasJSONItemObject) then begin
   JSONObject:=TPasJSONItemObject(aJSON);
   RaymarchingMinSteps:=Min(Max(TPasJSON.GetInt64(JSONObject.Properties['minsteps'],RaymarchingMinSteps),1),256);
   RaymarchingMaxSteps:=Min(Max(TPasJSON.GetInt64(JSONObject.Properties['maxsteps'],RaymarchingMaxSteps),1),256);
  end;
 end; 
begin
 
 if assigned(aJSON) and (aJSON is TPasJSONItemObject) then begin
  
  JSONRootObject:=TPasJSONItemObject(aJSON);

  Transform:=JSONToMatrix4x4(JSONRootObject.Properties['transform'],Transform);

  Factor:=TPasJSON.GetNumber(JSONRootObject.Properties['scatteringcoefficientscale'],1.0);

  SolarIrradiance:=JSONToVector4(JSONRootObject.Properties['solarirradiance'],SolarIrradiance);
  SunAngularRadius:=TPasJSON.GetNumber(JSONRootObject.Properties['sunangularradius'],SunAngularRadius);
  
  BottomRadius:=TPasJSON.GetNumber(JSONRootObject.Properties['bottomradius'],BottomRadius);
  TopRadius:=TPasJSON.GetNumber(JSONRootObject.Properties['topradius'],TopRadius);
  GroundAlbedo.xyz:=JSONToVector3(JSONRootObject.Properties['groundalbedo'],GroundAlbedo.xyz);
  Intensity:=TPasJSON.GetNumber(JSONRootObject.Properties['intensity'],Intensity);
  
  LoadDensityProfileLayer(JSONRootObject.Properties['rayleighdensity0'],RayleighDensity.Layers[0]);
  LoadDensityProfileLayer(JSONRootObject.Properties['rayleighdensity1'],RayleighDensity.Layers[1]);
  RayleighScattering.xyz:=JSONToVector3(JSONRootObject.Properties['rayleighscattering'],RayleighScattering.xyz/Factor)*Factor;
  
  LoadDensityProfileLayer(JSONRootObject.Properties['miedensity0'],MieDensity.Layers[0]);
  LoadDensityProfileLayer(JSONRootObject.Properties['miedensity1'],MieDensity.Layers[1]);
  MieScattering.xyz:=JSONToVector3(JSONRootObject.Properties['miescattering'],MieScattering.xyz/Factor)*Factor;
  MieExtinction.xyz:=JSONToVector3(JSONRootObject.Properties['mieextinction'],MieExtinction.xyz/Factor)*Factor;
  MiePhaseFunctionG:=TPasJSON.GetNumber(JSONRootObject.Properties['miephasefunctiong'],MiePhaseFunctionG);

  LoadDensityProfileLayer(JSONRootObject.Properties['absorptiondensity0'],AbsorptionDensity.Layers[0]);
  LoadDensityProfileLayer(JSONRootObject.Properties['absorptiondensity1'],AbsorptionDensity.Layers[1]);
  AbsorptionExtinction.xyz:=JSONToVector3(JSONRootObject.Properties['absorptionextinction'],AbsorptionExtinction.xyz/Factor)*Factor;

  //SunDirection.xyz:=JSONToVector3(JSONRootObject.Properties['sundirection'],SunDirection.xyz);

  //MuSMin:=TPasJSON.GetNumber(JSONRootObject.Properties['musmin'],MuSMin);

  LoadRaymarching(JSONRootObject.Properties['raymarching']);

 end;

end;

procedure TpvScene3DAtmosphere.TAtmosphereParameters.LoadFromJSONStream(const aStream:TStream);
var JSON:TPasJSONItem;
begin
 JSON:=TPasJSON.Parse(aStream);
 if assigned(JSON) then begin
  try
   LoadFromJSON(JSON);
  finally
   FreeAndNil(JSON);
  end;
 end;
end;

procedure TpvScene3DAtmosphere.TAtmosphereParameters.LoadFromJSONFile(const aFileName:string);
var Stream:TMemoryStream;
begin
 Stream:=TMemoryStream.Create;
 try
  Stream.LoadFromFile(aFileName);
  LoadFromJSONStream(Stream);
 finally
  FreeAndNil(Stream);
 end;
end;

function TpvScene3DAtmosphere.TAtmosphereParameters.SaveToJSON:TPasJSONItemObject;
 function SaveDensityLayer(const aLayer:TDensityProfileLayer):TPasJSONItemObject;
 begin
  result:=TPasJSONItemObject.Create;
  result.Add('width',TPasJSONItemNumber.Create(aLayer.Width));
  result.Add('expterm',TPasJSONItemNumber.Create(aLayer.ExpTerm));
  result.Add('expscale',TPasJSONItemNumber.Create(aLayer.ExpScale));
  result.Add('linearterm',TPasJSONItemNumber.Create(aLayer.LinearTerm));
  result.Add('constantterm',TPasJSONItemNumber.Create(aLayer.ConstantTerm));
 end;
 function SaveRaymarching:TPasJSONItemObject;
 begin
  result:=TPasJSONItemObject.Create;
  result.Add('minsteps',TPasJSONItemNumber.Create(RaymarchingMinSteps));
  result.Add('maxsteps',TPasJSONItemNumber.Create(RaymarchingMaxSteps));
 end;
begin
 result:=TPasJSONItemObject.Create;
 result.Add('transform',Matrix4x4ToJSON(Transform));
 result.Add('scatteringcoefficientscale',TPasJSONItemNumber.Create(1.0));
 result.Add('solarirradiance',Vector4ToJSON(SolarIrradiance));
 result.Add('sunangularradius',TPasJSONItemNumber.Create(SunAngularRadius));
 result.Add('bottomradius',TPasJSONItemNumber.Create(BottomRadius));
 result.Add('topradius',TPasJSONItemNumber.Create(TopRadius));
 result.Add('groundalbedo',Vector3ToJSON(GroundAlbedo.xyz));
 result.Add('intensity',TPasJSONItemNumber.Create(Intensity));
 result.Add('rayleighdensity0',SaveDensityLayer(RayleighDensity.Layers[0]));
 result.Add('rayleighdensity1',SaveDensityLayer(RayleighDensity.Layers[1]));
 result.Add('rayleighscattering',Vector3ToJSON(RayleighScattering.xyz));
 result.Add('miedensity0',SaveDensityLayer(MieDensity.Layers[0]));
 result.Add('miedensity1',SaveDensityLayer(MieDensity.Layers[1]));
 result.Add('miescattering',Vector3ToJSON(MieScattering.xyz));
 result.Add('mieextinction',Vector3ToJSON(MieExtinction.xyz));
 result.Add('miephasefunctiong',TPasJSONItemNumber.Create(MiePhaseFunctionG));
 result.Add('absorptiondensity0',SaveDensityLayer(AbsorptionDensity.Layers[0]));
 result.Add('absorptiondensity1',SaveDensityLayer(AbsorptionDensity.Layers[1]));
 result.Add('absorptionextinction',Vector3ToJSON(AbsorptionExtinction.xyz));
 //result.Add('sundirection',Vector3ToJSON(SunDirection.xyz));
 //result.Add('musmin',TPasJSONItemNumber.Create(MuSMin));
 result.Add('raymarching',SaveRaymarching); 
end;

procedure TpvScene3DAtmosphere.TAtmosphereParameters.SaveToJSONStream(const aStream:TStream);
var JSON:TPasJSONItem;
begin
 JSON:=SaveToJSON;
 if assigned(JSON) then begin
  try
   TPasJSON.StringifyToStream(aStream,JSON,true);
  finally
   FreeAndNil(JSON);
  end;
 end;
end;

procedure TpvScene3DAtmosphere.TAtmosphereParameters.SaveToJSONFile(const aFileName:string);
var Stream:TMemoryStream;
begin
 Stream:=TMemoryStream.Create;
 try
  SaveToJSONStream(Stream);
  Stream.SaveToFile(aFileName);
 finally
  FreeAndNil(Stream);
 end;
end;

function TpvScene3DAtmosphere_TAtmosphereParameters_POCASetAtmosphere(Context:PPOCAContext;const This:TPOCAValue;const Arguments:PPOCAValues;const CountArguments:longint;const UserData:pointer):TPOCAValue;
var JSONString:TpvUTF8String;
    JSON:TPasJSONItem;
begin
 if CountArguments>0 then begin
  JSONString:=POCAStringDump(Context,Arguments^[0]);
  JSON:=TPasJSON.Parse(JSONString);
  if assigned(JSON) then begin
   try
    TpvScene3DAtmosphere.PAtmosphereParameters(UserData)^.LoadFromJSON(JSON);
   finally
    FreeAndNil(JSON);
   end;
  end;
 end;
 result:=POCAValueNull;
end;

procedure TpvScene3DAtmosphere.TAtmosphereParameters.LoadFromPOCA(const aPOCACode:TpvUTF8String);
var POCAInstance:PPOCAInstance;
    POCAContext:PPOCAContext;
    POCACode:TPOCAValue;
    Code:TpvUTF8String;
begin
 if length(aPOCACode)>0 then begin
  Code:=PUCUUTF8Trim(PUCUUTF8Correct(aPOCACode));
  if length(Code)>0 then begin
   POCAInstance:=POCAInstanceCreate;
   try
    POCAContext:=POCAContextCreate(POCAInstance);
    try
     try
      POCAAddNativeFunction(POCAContext,POCAInstance.Globals.Namespace,'setAtmosphere',@TpvScene3DAtmosphere_TAtmosphereParameters_POCASetAtmosphere,nil,@self);
      POCACode:=POCACompile(POCAInstance,POCAContext,Code,'<CODE>');
      POCACall(POCAContext,POCACode,nil,0,POCAValueNull,POCAInstance^.Globals.Namespace);
     except
      on e:EPOCASyntaxError do begin
       // Ignore
      end;
      on e:EPOCARuntimeError do begin
       // Ignore
      end;
      on e:EPOCAScriptError do begin
       // Ignore
      end;
      on e:Exception do begin
       raise;
      end;
     end;
    finally
     POCAContextDestroy(POCAContext);
    end;
   finally
    POCAInstanceDestroy(POCAInstance);
   end;
  end;
 end;
end;

procedure TpvScene3DAtmosphere.TAtmosphereParameters.LoadFromPOCAStream(const aStream:TStream);
var POCACode:TpvUTF8String;
begin
 if assigned(aStream) and (aStream.Size>0) then begin
  POCACode:='';
  try
   SetLength(POCACode,aStream.Size);
   aStream.Seek(0,soBeginning);
   aStream.ReadBuffer(POCACode[1],aStream.Size);
   LoadFromPOCA(POCACode);
  finally
   POCACode:='';
  end;
 end; 
end;

procedure TpvScene3DAtmosphere.TAtmosphereParameters.LoadFromPOCAFile(const aFileName:string);
var Stream:TMemoryStream;
begin
 Stream:=TMemoryStream.Create;
 try
  Stream.LoadFromFile(aFileName);
  LoadFromPOCAStream(Stream);
 finally
  FreeAndNil(Stream);
 end;
end;

{ TpvScene3DAtmosphere.TGPUAtmosphereParameters }

procedure TpvScene3DAtmosphere.TGPUAtmosphereParameters.Assign(const aAtmosphereParameters:TAtmosphereParameters);
begin

 Transform:=aAtmosphereParameters.Transform;
 InverseTransform:=Transform.Inverse;

 SolarIrradiance:=aAtmosphereParameters.SolarIrradiance;

 BottomRadius:=aAtmosphereParameters.BottomRadius;
 TopRadius:=aAtmosphereParameters.TopRadius;
 RayleighDensityExpScale:=aAtmosphereParameters.RayleighDensity.Layers[1].ExpScale;
 RayleighScattering:=TpvVector4.InlineableCreate(aAtmosphereParameters.RayleighScattering.xyz,aAtmosphereParameters.MuSMin);

 MieDensityExpScale:=aAtmosphereParameters.MieDensity.Layers[1].ExpScale;
 MieScattering:=TpvVector4.InlineableCreate(aAtmosphereParameters.MieScattering.xyz,aAtmosphereParameters.SunDirection.x);
 MieExtinction:=TpvVector4.InlineableCreate(aAtmosphereParameters.MieExtinction.xyz,aAtmosphereParameters.SunDirection.y);
 MieAbsorption:=TpvVector4.InlineableCreate(aAtmosphereParameters.AbsorptionExtinction.xyz,aAtmosphereParameters.SunDirection.z);
 MiePhaseG:=aAtmosphereParameters.MiePhaseFunctionG;

 AbsorptionDensity0LayerWidth:=aAtmosphereParameters.AbsorptionDensity.Layers[0].Width;
 AbsorptionDensity0ConstantTerm:=aAtmosphereParameters.AbsorptionDensity.Layers[0].ConstantTerm;
 AbsorptionDensity0LinearTerm:=aAtmosphereParameters.AbsorptionDensity.Layers[0].LinearTerm;
 AbsorptionDensity1ConstantTerm:=aAtmosphereParameters.AbsorptionDensity.Layers[1].ConstantTerm;
 AbsorptionDensity1LinearTerm:=aAtmosphereParameters.AbsorptionDensity.Layers[1].LinearTerm;
 AbsorptionExtinction:=aAtmosphereParameters.AbsorptionExtinction;

 GroundAlbedo:=TpvVector4.InlineableCreate(aAtmosphereParameters.GroundAlbedo.xyz,aAtmosphereParameters.Intensity);

 RaymarchingMinSteps:=aAtmosphereParameters.RaymarchingMinSteps;
 RaymarchingMaxSteps:=aAtmosphereParameters.RaymarchingMaxSteps;

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

 fTransmittanceTexture:=TpvScene3DRendererImage2D.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                         TRANSMITTANCE_TEXTURE_WIDTH,
                                                         TRANSMITTANCE_TEXTURE_HEIGHT,
                                                         VK_FORMAT_R32G32B32A32_SFLOAT,
                                                         true,
                                                         VK_SAMPLE_COUNT_1_BIT,
                                                         VK_IMAGE_LAYOUT_GENERAL);
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fTransmittanceTexture.VulkanImage.Handle,VK_OBJECT_TYPE_IMAGE,'TransmittanceTexture');
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fTransmittanceTexture.VulkanImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'TransmittanceTexture');

 fMultiScatteringTexture:=TpvScene3DRendererArray2DImage.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                                MultiScatteringLUTRes,
                                                                MultiScatteringLUTRes,
                                                                TpvScene3DRendererInstance(aRendererInstance).CountSurfaceViews,
                                                                VK_FORMAT_R32G32B32A32_SFLOAT,
                                                                VK_SAMPLE_COUNT_1_BIT,
                                                                VK_IMAGE_LAYOUT_GENERAL,
                                                                true,
                                                                0);  
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fMultiScatteringTexture.VulkanImage.Handle,VK_OBJECT_TYPE_IMAGE,'MultiScatteringTexture');
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fMultiScatteringTexture.VulkanImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'MultiScatteringTexture');
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fMultiScatteringTexture.VulkanArrayImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'MultiScatteringTexture');
 if assigned(fMultiScatteringTexture.VulkanOtherArrayImageView) then begin
  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fMultiScatteringTexture.VulkanOtherArrayImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'MultiScatteringTexture');
 end;

 fSkyViewLUTTexture:=TpvScene3DRendererArray2DImage.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                           SkyViewLUTTextureWidth,
                                                           SkyViewLUTTextureHeight, 
                                                           TpvScene3DRendererInstance(aRendererInstance).CountSurfaceViews*2,
                                                           VK_FORMAT_R32G32B32A32_SFLOAT,
                                                           VK_SAMPLE_COUNT_1_BIT,
                                                           VK_IMAGE_LAYOUT_GENERAL,
                                                           true,
                                                           0);           
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fSkyViewLUTTexture.VulkanImage.Handle,VK_OBJECT_TYPE_IMAGE,'SkyViewLUTTexture');
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fSkyViewLUTTexture.VulkanImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'SkyViewLUTTexture');
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fSkyViewLUTTexture.VulkanArrayImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'SkyViewLUTTexture');
 if assigned(fSkyViewLUTTexture.VulkanOtherArrayImageView) then begin
  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fSkyViewLUTTexture.VulkanOtherArrayImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'SkyViewLUTTexture');
 end;                                                                                                           

 fCameraVolumeTexture:=TpvScene3DRendererArray2DImage.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                             CameraVolumeTextureWidth,
                                                             CameraVolumeTextureHeight,
                                                             CameraVolumeTextureDepth*TpvScene3DRendererInstance(aRendererInstance).CountSurfaceViews*2,
                                                             VK_FORMAT_R32G32B32A32_SFLOAT,
                                                             VK_SAMPLE_COUNT_1_BIT,
                                                             VK_IMAGE_LAYOUT_GENERAL,
                                                             true,
                                                             0);
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCameraVolumeTexture.VulkanImage.Handle,VK_OBJECT_TYPE_IMAGE,'CameraVolumeTexture');
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCameraVolumeTexture.VulkanImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'CameraVolumeTexture');
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCameraVolumeTexture.VulkanArrayImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'CameraVolumeTexture');
 if assigned(fCameraVolumeTexture.VulkanOtherArrayImageView) then begin
  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCameraVolumeTexture.VulkanOtherArrayImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'CameraVolumeTexture');
 end;                                                            

 fCubeMapTexture:=TpvScene3DRendererMipmapImageCubeMap.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                              CubeMapTextureSize,
                                                              CubeMapTextureSize,
                                                              VK_FORMAT_R16G16B16A16_SFLOAT,
                                                              true,                                                              
                                                              VK_SAMPLE_COUNT_1_BIT,                                                              
                                                              VK_IMAGE_LAYOUT_GENERAL,
                                                              TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                              nil,
                                                              0);
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCubeMapTexture.VulkanImage.Handle,VK_OBJECT_TYPE_IMAGE,'AtmosphereCubeMapTexture');
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCubeMapTexture.VulkanImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'AtmosphereCubeMapTexture');
  
 fGGXCubeMapTexture:=TpvScene3DRendererMipmapImageCubeMap.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                                 CubeMapTextureSize,
                                                                 CubeMapTextureSize,
                                                                 VK_FORMAT_R16G16B16A16_SFLOAT,
                                                                 true,                                                              
                                                                 VK_SAMPLE_COUNT_1_BIT,                                                              
                                                                 VK_IMAGE_LAYOUT_GENERAL,
                                                                 TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                 nil,
                                                                 0);
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fGGXCubeMapTexture.VulkanImage.Handle,VK_OBJECT_TYPE_IMAGE,'AtmosphereGGXCubeMapTexture');
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fGGXCubeMapTexture.VulkanImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'AtmosphereGGXCubeMapTexture');
 
 fCharlieCubeMapTexture:=TpvScene3DRendererMipmapImageCubeMap.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                                     CubeMapTextureSize,
                                                                     CubeMapTextureSize,
                                                                     VK_FORMAT_R16G16B16A16_SFLOAT,
                                                                     true,                                                              
                                                                     VK_SAMPLE_COUNT_1_BIT,                                                              
                                                                     VK_IMAGE_LAYOUT_GENERAL,
                                                                     TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                     nil,
                                                                     0);
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCharlieCubeMapTexture.VulkanImage.Handle,VK_OBJECT_TYPE_IMAGE,'AtmosphereCharlieCubeMapTexture');
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCharlieCubeMapTexture.VulkanImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'AtmosphereCharlieCubeMapTexture');
   
 fLambertianCubeMapTexture:=TpvScene3DRendererMipmapImageCubeMap.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                                        CubeMapTextureSize,
                                                                        CubeMapTextureSize,
                                                                        VK_FORMAT_R16G16B16A16_SFLOAT,
                                                                        true,                                                              
                                                                        VK_SAMPLE_COUNT_1_BIT,                                                              
                                                                        VK_IMAGE_LAYOUT_GENERAL,
                                                                        TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                        nil,
                                                                        0);
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fLambertianCubeMapTexture.VulkanImage.Handle,VK_OBJECT_TYPE_IMAGE,'AtmosphereLambertianCubeMapTexture');
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fLambertianCubeMapTexture.VulkanImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'AtmosphereLambertianCubeMapTexture');

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
                                                                                         TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).AtmosphereGlobals).fTransmittanceLUTPassDescriptorSetLayout);

  fTransmittanceLUTPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                               0,
                                                                               1,
                                                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                               [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                              fTransmittanceTexture.VulkanImageView.Handle,
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
 fMultiScatteringLUTPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*2);
 fMultiScatteringLUTPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fMultiScatteringLUTPassDescriptorPool.Initialize;
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fMultiScatteringLUTPassDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'MultiScatteringLUTPassDescriptorPool');

 for InFlightFrameIndex:=0 to TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames-1 do begin

  fMultiScatteringLUTPassDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fMultiScatteringLUTPassDescriptorPool,
                                                                                         TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).AtmosphereGlobals).fMultiScatteringLUTPassDescriptorSetLayout);

  fMultiScatteringLUTPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                                 0,
                                                                                 1,
                                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                                fMultiScatteringTexture.VulkanArrayImageView.Handle,
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

  fMultiScatteringLUTPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                                 0,
                                                                                 1,
                                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                                 [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                                TpvScene3D(fAtmosphere.fScene3D).BlueNoise2DTexture.ImageView.Handle,
                                                                                                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                                 [],
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
 fSkyViewLUTPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*3);
 fSkyViewLUTPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fSkyViewLUTPassDescriptorPool.Initialize;
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fSkyViewLUTPassDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'SkyViewLUTPassDescriptorPool');

 for InFlightFrameIndex:=0 to TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames-1 do begin

  fSkyViewLUTPassDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fSkyViewLUTPassDescriptorPool,
                                                                                   TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).AtmosphereGlobals).fSkyViewLUTPassDescriptorSetLayout);

  fSkyViewLUTPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                         0,
                                                                         1,
                                                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                         [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                        fSkyViewLUTTexture.VulkanArrayImageView.Handle,
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
                                                                                                        fMultiScatteringTexture.VulkanArrayImageView.Handle,
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

  fSkyViewLUTPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                                         0,
                                                                         1,
                                                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                         [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                        TpvScene3D(fAtmosphere.fScene3D).BlueNoise2DTexture.ImageView.Handle,
                                                                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                         [],
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
 fCameraVolumePassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*3);
 fCameraVolumePassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fCameraVolumePassDescriptorPool.Initialize;
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCameraVolumePassDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'CameraVolumePassDescriptorPool');

 for InFlightFrameIndex:=0 to TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames-1 do begin

  fCameraVolumePassDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fCameraVolumePassDescriptorPool,
                                                                                      TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).AtmosphereGlobals).fCameraVolumePassDescriptorSetLayout);

  fCameraVolumePassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                           0,
                                                                           1,
                                                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                           [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                          fCameraVolumeTexture.VulkanArrayImageView.Handle,
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
                                                                                                          fMultiScatteringTexture.VulkanArrayImageView.Handle,
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

  fCameraVolumePassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                                           0,
                                                                           1,
                                                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                           [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                          TpvScene3D(fAtmosphere.fScene3D).BlueNoise2DTexture.ImageView.Handle,
                                                                                                          VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                           [],
                                                                           [],
                                                                           false);

  fCameraVolumePassDescriptorSets[InFlightFrameIndex].Flush;

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCameraVolumePassDescriptorSets[InFlightFrameIndex].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'CameraVolumePassDescriptorSets['+IntToStr(InFlightFrameIndex)+']');

 end; 

 fCubeMapPassDescriptorPool:=TpvVulkanDescriptorPool.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                            TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT) or
                                                            TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT),
                                                            TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fCubeMapPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fCubeMapPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*2);
 fCubeMapPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fCubeMapPassDescriptorPool.Initialize;
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCubeMapPassDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'CubeMapPassDescriptorPool');

 for InFlightFrameIndex:=0 to TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames-1 do begin

  fCubeMapPassDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fCubeMapPassDescriptorPool,
                                                                                TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).AtmosphereGlobals).fCubeMapPassDescriptorSetLayout);

  fCubeMapPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                      0,
                                                                      1,
                                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                      [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                     fCubeMapTexture.VulkanImageViews[0].Handle,
                                                                                                     VK_IMAGE_LAYOUT_GENERAL)],
                                                                      [],
                                                                      [],
                                                                      false);

  fCubeMapPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                      0,
                                                                      1,
                                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                      [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                     fTransmittanceTexture.VulkanImageView.Handle,
                                                                                                     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                      [],
                                                                      [],
                                                                      false);

  fCubeMapPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                      0,
                                                                      1,
                                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                      [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                     fMultiScatteringTexture.VulkanArrayImageView.Handle,
                                                                                                     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                      [],
                                                                      [],
                                                                      false);

  fCubeMapPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                      0,
                                                                      1,
                                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                      [],
                                                                      [fAtmosphere.fAtmosphereParametersBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                      [],
                                                                      false);                                                             

  fCubeMapPassDescriptorSets[InFlightFrameIndex].Flush;

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCubeMapPassDescriptorSets[InFlightFrameIndex].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'CubeMapPassDescriptorSets['+IntToStr(InFlightFrameIndex)+']');

 end;

 fRaymarchingPassDescriptorPool:=TpvVulkanDescriptorPool.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                               TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT) or
                                                               TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT),
                                                               TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fRaymarchingPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fRaymarchingPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*5);
 fRaymarchingPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fRaymarchingPassDescriptorPool.Initialize;
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fRaymarchingPassDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'RaymarchingPassDescriptorPool');

 for InFlightFrameIndex:=0 to TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames-1 do begin

  fRaymarchingPassDepthImageViews[InFlightFrameIndex]:=VK_NULL_HANDLE;

  fRaymarchingPassDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fRaymarchingPassDescriptorPool,
                                                                                     TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).AtmosphereGlobals).fRaymarchingPassDescriptorSetLayout);

{ fRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                          0,
                                                                          1,
                                                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                                                                          [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                         VK_NULL_HANDLE, //fCameraVolumeTexture.VulkanArrayImageView.Handle, // Dummy, will be replaced with the actual depth texture attachment
                                                                                                         VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                          [],
                                                                          [],
                                                                          false);}

  fRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                          0,
                                                                          1,
                                                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                          [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                         fTransmittanceTexture.VulkanImageView.Handle,
                                                                                                         VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                          [],
                                                                          [],
                                                                          false);

  fRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                          0,
                                                                          1,
                                                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                          [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                         fMultiScatteringTexture.VulkanArrayImageView.Handle,
                                                                                                         VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                          [],
                                                                          [],
                                                                          false);

  fRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                          0,
                                                                          1,
                                                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                          [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                         fSkyViewLUTTexture.VulkanArrayImageView.Handle,
                                                                                                         VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                          [],
                                                                          [],
                                                                          false);

  fRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                                          0,
                                                                          1,
                                                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                          [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                         fCameraVolumeTexture.VulkanArrayImageView.Handle,
                                                                                                         VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                          [],
                                                                          [],
                                                                          false);

  fRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(5,
                                                                          0,
                                                                          1,
                                                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                          [],
                                                                          [fAtmosphere.fAtmosphereParametersBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                          [],
                                                                          false);                                                             

  fRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(6,
                                                                          0,
                                                                          1,
                                                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                          [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                         TpvScene3D(fAtmosphere.fScene3D).BlueNoise2DTexture.ImageView.Handle,
                                                                                                         VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                          [],
                                                                          [],
                                                                          false);

//fRaymarchingPassDescriptorSets[InFlightFrameIndex].Flush; // Will be flushed later

  fRaymarchingPassDescriptorSetFirsts[InFlightFrameIndex]:=true; // Will be set to false after the first flush, so that the further descriptor set updates will be updated directly instead of creating new descriptor sets

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
                                                                           TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).AtmosphereGlobals).fGlobalVulkanDescriptorSetLayout);

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

 fRaymarchingGraphicsPipeline:=nil;

end;

destructor TpvScene3DAtmosphere.TRendererInstance.Destroy;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fRaymarchingGraphicsPipeline);

 for InFlightFrameIndex:=0 to TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames-1 do begin
  FreeAndNil(fTransmittanceLUTPassDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fMultiScatteringLUTPassDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fSkyViewLUTPassDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fCubeMapPassDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fCameraVolumePassDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fRaymarchingPassDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fGlobalDescriptorSets[InFlightFrameIndex]);
 end;

 FreeAndNil(fTransmittanceLUTPassDescriptorPool);
 FreeAndNil(fMultiScatteringLUTPassDescriptorPool);
 FreeAndNil(fSkyViewLUTPassDescriptorPool);
 FreeAndNil(fCubeMapPassDescriptorPool);
 FreeAndNil(fCameraVolumePassDescriptorPool);
 FreeAndNil(fRaymarchingPassDescriptorPool);
 FreeAndNil(fGlobalDescriptorPool);

 FreeAndNil(fCubeMapTexture);
 FreeAndNil(fGGXCubeMapTexture);
 FreeAndNil(fCharlieCubeMapTexture);
 FreeAndNil(fLambertianCubeMapTexture);

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

procedure TpvScene3DAtmosphere.TRendererInstance.SetDepthImageView(const aInFlightFrameIndex:TpvSizeInt;
                                                                   const aDepthImageView:TVkImageView);
begin

 if fRaymarchingPassDepthImageViews[aInFlightFrameIndex]<>aDepthImageView then begin

  fRaymarchingPassDepthImageViews[aInFlightFrameIndex]:=aDepthImageView;

  fRaymarchingPassDescriptorSets[aInFlightFrameIndex].WriteToDescriptorSet(0,
                                                                           0,
                                                                           1,
                                                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                                                                           [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                          aDepthImageView,
                                                                                                          VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                           [],
                                                                           [],
                                                                           not fRaymarchingPassDescriptorSetFirsts[aInFlightFrameIndex]);

  if fRaymarchingPassDescriptorSetFirsts[aInFlightFrameIndex] then begin 
   fRaymarchingPassDescriptorSets[aInFlightFrameIndex].Flush;
  end;

  fRaymarchingPassDescriptorSetFirsts[aInFlightFrameIndex]:=false;

 end;

end;

procedure TpvScene3DAtmosphere.TRendererInstance.Setup(const aRenderPass:TpvVulkanRenderPass;
                                                       const aRenderPassSubpassIndex:TpvSizeInt;
                                                       const aSampleCount:TVkSampleCountFlagBits;
                                                       const aWidth:TpvSizeInt;
                                                       const aHeight:TpvSizeInt);
begin

 // Free the old graphics pipeline, if it exists 
 if assigned(fRaymarchingGraphicsPipeline) then begin
  FreeAndNil(fRaymarchingGraphicsPipeline);
 end;

 // Create the new graphics pipeline
 fRaymarchingGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                                TpvScene3D(fAtmosphere.fScene3D).VulkanPipelineCache,
                                                                0,
                                                                [],
                                                                TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).AtmosphereGlobals).fRaymarchingPipelineLayout,
                                                                aRenderPass,
                                                                aRenderPassSubpassIndex,
                                                                nil,
                                                                0);

 fRaymarchingGraphicsPipeline.AddStage(TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).AtmosphereGlobals).fRaymarchingPipelineShaderStageVertex);
 fRaymarchingGraphicsPipeline.AddStage(TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).AtmosphereGlobals).fRaymarchingPipelineShaderStageFragments[TVkSampleCountFlags(aSampleCount)<>TVkSampleCountFlags(VK_SAMPLE_COUNT_1_BIT)]);

 fRaymarchingGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fRaymarchingGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fRaymarchingGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,aWidth,aHeight,0.0,1.0);
 fRaymarchingGraphicsPipeline.ViewPortState.AddScissor(0,0,aWidth,aHeight);

 fRaymarchingGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fRaymarchingGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fRaymarchingGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fRaymarchingGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
 fRaymarchingGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_CLOCKWISE;
 fRaymarchingGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
 fRaymarchingGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
 fRaymarchingGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
 fRaymarchingGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
 fRaymarchingGraphicsPipeline.RasterizationState.LineWidth:=1.0;

 fRaymarchingGraphicsPipeline.MultisampleState.RasterizationSamples:=aSampleCount;
 fRaymarchingGraphicsPipeline.MultisampleState.SampleShadingEnable:=false;
 fRaymarchingGraphicsPipeline.MultisampleState.MinSampleShading:=0.0;
 fRaymarchingGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
 fRaymarchingGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=false;
 fRaymarchingGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;

 fRaymarchingGraphicsPipeline.ColorBlendState.LogicOpEnable:=false;
 fRaymarchingGraphicsPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
 fRaymarchingGraphicsPipeline.ColorBlendState.BlendConstants[0]:=0.0;
 fRaymarchingGraphicsPipeline.ColorBlendState.BlendConstants[1]:=0.0;
 fRaymarchingGraphicsPipeline.ColorBlendState.BlendConstants[2]:=0.0;
 fRaymarchingGraphicsPipeline.ColorBlendState.BlendConstants[3]:=0.0;
 fRaymarchingGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(true, // Enable alpha blending
                                                                           VK_BLEND_FACTOR_SRC_ALPHA,
                                                                           VK_BLEND_FACTOR_DST_ALPHA,
                                                                           VK_BLEND_OP_ADD,
                                                                           VK_BLEND_FACTOR_ONE,
                                                                           VK_BLEND_FACTOR_ZERO,
                                                                           VK_BLEND_OP_ADD,
                                                                           TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                           TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                           TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                           TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

 fRaymarchingGraphicsPipeline.DepthStencilState.DepthTestEnable:=false;
 fRaymarchingGraphicsPipeline.DepthStencilState.DepthWriteEnable:=false;
 fRaymarchingGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_ALWAYS;
 fRaymarchingGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fRaymarchingGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

 fRaymarchingGraphicsPipeline.Initialize;

end;

procedure TpvScene3DAtmosphere.TRendererInstance.ReleaseGraphicsPipeline;
begin
 FreeAndNil(fRaymarchingGraphicsPipeline);
end;

procedure TpvScene3DAtmosphere.TRendererInstance.Execute(const aInFlightFrameIndex:TpvSizeInt;
                                                         const aCommandBuffer:TpvVulkanCommandBuffer);
var BaseViewIndex,CountViews:TpvSizeInt;
    InFlightFrameState:TpvScene3DRendererInstance.PInFlightFrameState;
    AtmosphereGlobals:TpvScene3DAtmosphereGlobals;
    //ImageSubresourceRange:TVkImageSubresourceRange;
    ImageMemoryBarriers:array[0..3] of TVkImageMemoryBarrier;
    TransmittanceLUTPushConstants:TpvScene3DAtmosphereGlobals.TTransmittanceLUTPushConstants;
    MultiScatteringLUTPushConstants:TpvScene3DAtmosphereGlobals.TMultiScatteringLUTPushConstants;
    SkyViewLUTPushConstants:TpvScene3DAtmosphereGlobals.TSkyViewLUTPushConstants;
    CameraVolumePushConstants:TpvScene3DAtmosphereGlobals.TCameraVolumePushConstants;
    CubeMapPushConstants:TpvScene3DAtmosphereGlobals.TCubeMapPushConstants;
begin

 AtmosphereGlobals:=TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).AtmosphereGlobals);

 InFlightFrameState:=@TpvScene3DRendererInstance(fRendererInstance).InFlightFrameStates[aInFlightFrameIndex];

 BaseViewIndex:=InFlightFrameState^.FinalViewIndex;
 CountViews:=InFlightFrameState^.CountFinalViews;

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
                                                                                       1));

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
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

  TransmittanceLUTPushConstants.BaseViewIndex:=BaseViewIndex;
  TransmittanceLUTPushConstants.CountViews:=CountViews;
  TransmittanceLUTPushConstants.Dummy0:=1;
  TransmittanceLUTPushConstants.Dummy1:=1;

  aCommandBuffer.CmdPushConstants(AtmosphereGlobals.fTransmittanceLUTComputePipelineLayout.Handle,
                                  TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_COMPUTE_BIT),
                                  0,
                                  SizeOf(TpvScene3DAtmosphereGlobals.TTransmittanceLUTPushConstants),
                                  @TransmittanceLUTPushConstants);

  aCommandBuffer.CmdDispatch((fTransmittanceTexture.Width+15) shr 4,
                             (fTransmittanceTexture.Height+15) shr 4,
                             1);


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
                                                                                       1));

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    1,@ImageMemoryBarriers[0]);

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

 end;

 begin

  // Multi scattering LUT

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DAtmosphere.MultiScatteringLUT',[0.0,1.0,0.0,1.0]);

  ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(0,
                                                       TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                       VK_IMAGE_LAYOUT_UNDEFINED,
                                                       VK_IMAGE_LAYOUT_GENERAL,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       fMultiScatteringTexture.VulkanImage.Handle,
                                                       TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                       0,
                                                                                       1,
                                                                                       0,
                                                                                       TpvScene3DRendererInstance(fRendererInstance).CountSurfaceViews));

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    1,@ImageMemoryBarriers[0]);      

  aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,AtmosphereGlobals.fMultiScatteringLUTComputePipeline.Handle);

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       AtmosphereGlobals.fMultiScatteringLUTComputePipelineLayout.Handle,
                                       0,
                                       1,
                                       @fMultiScatteringLUTPassDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil); 

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       AtmosphereGlobals.fMultiScatteringLUTComputePipelineLayout.Handle,
                                       1,
                                       1,
                                       @fGlobalDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil);    

  MultiScatteringLUTPushConstants.BaseViewIndex:=BaseViewIndex;
  MultiScatteringLUTPushConstants.CountViews:=CountViews;
  MultiScatteringLUTPushConstants.MultipleScatteringFactor:=1;
  MultiScatteringLUTPushConstants.FrameIndex:=pvApplication.DrawFrameCounter;

  aCommandBuffer.CmdPushConstants(AtmosphereGlobals.fMultiScatteringLUTComputePipelineLayout.Handle,
                                  TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_COMPUTE_BIT),
                                  0,
                                  SizeOf(TpvScene3DAtmosphereGlobals.TMultiScatteringLUTPushConstants),
                                  @MultiScatteringLUTPushConstants);

  aCommandBuffer.CmdDispatch(fMultiScatteringTexture.Width,
                             fMultiScatteringTexture.Height,
                             TpvScene3DRendererInstance(fRendererInstance).CountSurfaceViews);   

  ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                       TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                                       VK_IMAGE_LAYOUT_GENERAL,
                                                       VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       fMultiScatteringTexture.VulkanImage.Handle,
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

 if TpvScene3DRenderer(TpvScene3DRendererInstance(fRendererInstance).Renderer).FastSky then begin

  // Sky view LUT

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DAtmosphere.SkyViewLUT',[0.0,0.0,1.0,1.0]);

  ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(0,
                                                       TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                       VK_IMAGE_LAYOUT_UNDEFINED,
                                                       VK_IMAGE_LAYOUT_GENERAL,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       fSkyViewLUTTexture.VulkanImage.Handle,
                                                       TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                       0,
                                                                                       1,
                                                                                       0,
                                                                                       fSkyViewLUTTexture.Layers));

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    1,@ImageMemoryBarriers[0]);      

  aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,AtmosphereGlobals.fSkyViewLUTComputePipeline.Handle);

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       AtmosphereGlobals.fSkyViewLUTComputePipelineLayout.Handle,
                                       0,
                                       1,
                                       @fSkyViewLUTPassDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil); 

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       AtmosphereGlobals.fSkyViewLUTComputePipelineLayout.Handle,
                                       1,
                                       1,
                                       @fGlobalDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil);    

  SkyViewLUTPushConstants.BaseViewIndex:=BaseViewIndex;
  SkyViewLUTPushConstants.CountViews:=CountViews;
  SkyViewLUTPushConstants.FrameIndex:=pvApplication.DrawFrameCounter;
  SkyViewLUTPushConstants.Dummy1:=0;

  aCommandBuffer.CmdPushConstants(AtmosphereGlobals.fSkyViewLUTComputePipelineLayout.Handle,
                                  TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_COMPUTE_BIT),
                                  0,
                                  SizeOf(TpvScene3DAtmosphereGlobals.TSkyViewLUTPushConstants),
                                  @SkyViewLUTPushConstants);

  aCommandBuffer.CmdDispatch((fSkyViewLUTTexture.Width+15) shr 4,
                             (fSkyViewLUTTexture.Height+15) shr 4,
                             TpvScene3DRendererInstance(fRendererInstance).CountSurfaceViews);    

  ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                       TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                                       VK_IMAGE_LAYOUT_GENERAL,
                                                       VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       fSkyViewLUTTexture.VulkanImage.Handle,
                                                       TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                       0,
                                                                                       1,
                                                                                       0,
                                                                                       fSkyViewLUTTexture.Layers));

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    1,@ImageMemoryBarriers[0]);   

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

 end; 

 if TpvScene3DRenderer(TpvScene3DRendererInstance(fRendererInstance).Renderer).FastAerialPerspective then begin

  // Camera volume

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DAtmosphere.CameraVolume',[1.0,1.0,0.0,1.0]);

  ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(0,
                                                       TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                       VK_IMAGE_LAYOUT_UNDEFINED,
                                                       VK_IMAGE_LAYOUT_GENERAL,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       fCameraVolumeTexture.VulkanImage.Handle,
                                                       TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                       0,
                                                                                       1,
                                                                                       0,
                                                                                       fCameraVolumeTexture.Layers));

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    1,@ImageMemoryBarriers[0]);      

  aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,AtmosphereGlobals.fCameraVolumeComputePipeline.Handle);

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       AtmosphereGlobals.fCameraVolumeComputePipelineLayout.Handle,
                                       0,
                                       1,
                                       @fCameraVolumePassDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil); 

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       AtmosphereGlobals.fCameraVolumeComputePipelineLayout.Handle,
                                       1,
                                       1,
                                       @fGlobalDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil);    

  CameraVolumePushConstants.BaseViewIndex:=BaseViewIndex;
  CameraVolumePushConstants.CountViews:=CountViews;
  CameraVolumePushConstants.FrameIndex:=pvApplication.DrawFrameCounter;
  CameraVolumePushConstants.Dummy1:=0;

  aCommandBuffer.CmdPushConstants(AtmosphereGlobals.fCameraVolumeComputePipelineLayout.Handle,
                                  TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_COMPUTE_BIT),
                                  0,
                                  SizeOf(TpvScene3DAtmosphereGlobals.TCameraVolumePushConstants),
                                  @CameraVolumePushConstants);

  aCommandBuffer.CmdDispatch((fCameraVolumeTexture.Width+15) shr 4,
                             (fCameraVolumeTexture.Height+15) shr 4,
                             fCameraVolumeTexture.Layers); // includes CountSurfaceViews already    

  ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                       TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                                       VK_IMAGE_LAYOUT_GENERAL,
                                                       VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       fCameraVolumeTexture.VulkanImage.Handle,
                                                       TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                       0,
                                                                                       1,
                                                                                       0,
                                                                                       fCameraVolumeTexture.Layers));

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    1,@ImageMemoryBarriers[0]);    

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

 end; 

 begin

  // Cube map pass

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DAtmosphere.CubeMapPass',[1.0,0.0,1.0,1.0]);

  ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(0,
                                                       TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                       VK_IMAGE_LAYOUT_UNDEFINED,
                                                       VK_IMAGE_LAYOUT_GENERAL,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       fCubeMapTexture.VulkanImage.Handle,
                                                       TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                       0,
                                                                                       1,
                                                                                       0,
                                                                                       6));

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    1,@ImageMemoryBarriers[0]);

  aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,AtmosphereGlobals.fCubeMapComputePipeline.Handle);

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       AtmosphereGlobals.fCubeMapComputePipelineLayout.Handle,
                                       0,
                                       1,
                                       @fCubeMapPassDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil);

  CubeMapPushConstants.CameraPosition:=TpvVector4.InlineableCreate(TpvScene3DRendererInstance(fRendererInstance).InFlightFrameStates^[aInFlightFrameIndex].MainCameraPosition,1.0);
  CubeMapPushConstants.UpVector:=TpvVector4.AllAxis;

  aCommandBuffer.CmdPushConstants(AtmosphereGlobals.fCubeMapComputePipelineLayout.Handle,
                                  TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_COMPUTE_BIT),
                                  0,
                                  SizeOf(TpvScene3DAtmosphereGlobals.TCubeMapPushConstants),
                                  @CubeMapPushConstants);

  aCommandBuffer.CmdDispatch((fCubeMapTexture.Width+15) shr 4,
                             (fCubeMapTexture.Height+15) shr 4,
                             6);

  ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                       TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                                       VK_IMAGE_LAYOUT_GENERAL,
                                                       VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       fCubeMapTexture.VulkanImage.Handle,
                                                       TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                       0,
                                                                                       1,
                                                                                       0,
                                                                                       6));

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    1,@ImageMemoryBarriers[0]);  

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

 end;

end;

procedure TpvScene3DAtmosphere.TRendererInstance.Draw(const aInFlightFrameIndex:TpvSizeInt;
                                                      const aCommandBuffer:TpvVulkanCommandBuffer;
                                                      const aDepthImageView:TVkImageView);
var DescriptorSets:array[0..1] of TVkDescriptorSet;
begin

 SetDepthImageView(aInFlightFrameIndex,aDepthImageView);

 DescriptorSets[0]:=fRaymarchingPassDescriptorSets[aInFlightFrameIndex].Handle;
 DescriptorSets[1]:=fGlobalDescriptorSets[aInFlightFrameIndex].Handle;

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS, 
                                      TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).AtmosphereGlobals).fRaymarchingPipelineLayout.Handle,
                                      0,
                                      2,
                                      @DescriptorSets,
                                      0,
                                      nil);

 aCommandBuffer.CmdDraw(3,1,0,0);

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
 
 fUploaded:=false;

 fVisible:=true;

 FillChar(fInFlightFrameVisible,SizeOf(fInFlightFrameVisible),#0);

 fReady:=true;

end;

destructor TpvScene3DAtmosphere.Destroy;
begin

 Unload;

 while fRendererInstances.Count>0 do begin
  fRendererInstances.Items[fRendererInstances.Count-1].Free;
 end;
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
 if assigned(fScene3D) and assigned(TpvScene3D(fScene3D).Atmospheres) then begin
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
                                                                            SizeOf(TGPUAtmosphereParameters),
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
   
   fGPUAtmosphereParameters.Assign(fAtmosphereParameters);

   TpvScene3D(fScene3D).VulkanDevice.MemoryStaging.Upload(aTransferQueue,
                                                          aTransferCommandBuffer,
                                                          aTransferFence,
                                                          fGPUAtmosphereParameters,
                                                          fAtmosphereParametersBuffers[aInFlightFrameIndex],
                                                          0,
                                                          SizeOf(TGPUAtmosphereParameters));

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

procedure TpvScene3DAtmosphere.Draw(const aInFlightFrameIndex:TpvSizeInt;
                                    const aCommandBuffer:TpvVulkanCommandBuffer;
                                    const aDepthImageView:TVkImageView;
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
   AtmosphereRendererInstance.Draw(aInFlightFrameIndex,aCommandBuffer,aDepthImageView);
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

procedure TpvScene3DAtmospheres.Draw(const aInFlightFrameIndex:TpvSizeInt;
                                     const aCommandBuffer:TpvVulkanCommandBuffer;
                                     const aDepthImageView:TVkImageView;
                                     const aRendererInstance:TObject);
var Index:TpvSizeInt;
    Atmosphere:TpvScene3DAtmosphere;
begin
 fLock.AcquireRead;
 try

  if Count>0 then begin

   for Index:=0 to Count-1 do begin
    Atmosphere:=Items[Index];
    if assigned(Atmosphere) then begin
     Atmosphere.Draw(aInFlightFrameIndex,aCommandBuffer,aDepthImageView,aRendererInstance);
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
 fCubeMapPassDescriptorSetLayout:=nil;
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

  // Blue noise texture
  fMultiScatteringLUTPassDescriptorSetLayout.AddBinding(3,
                                                        VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
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

  // Transmittance LUT texture 
  fSkyViewLUTPassDescriptorSetLayout.AddBinding(1,
                                                VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                1,
                                                TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                []);

  // Multi scattering LUT texture
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

  // Blue noise texture 
  fSkyViewLUTPassDescriptorSetLayout.AddBinding(4,
                                                VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
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

  // Blue noise texture 
  fCameraVolumePassDescriptorSetLayout.AddBinding(4,
                                                  VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                  1,
                                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                  []);

  fCameraVolumePassDescriptorSetLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCameraVolumePassDescriptorSetLayout.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,'TpvScene3DAtmosphereGlobals.fCameraVolumePassDescriptorSetLayout');

 end;

 // Cube map pass descriptor set layout
 begin

  fCubeMapPassDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(TpvScene3D(fScene3D).VulkanDevice);

  // Destination texture
  fCubeMapPassDescriptorSetLayout.AddBinding(0,
                                             VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                             []);

  // Transmittance LUT texture
  fCubeMapPassDescriptorSetLayout.AddBinding(1,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                             []);

  // Multi scattering LUT texture
  fCubeMapPassDescriptorSetLayout.AddBinding(2,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                             []);

  // Atmosphere parameters
  fCubeMapPassDescriptorSetLayout.AddBinding(3,
                                             VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                             []);

  fCubeMapPassDescriptorSetLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCubeMapPassDescriptorSetLayout.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,'TpvScene3DAtmosphereGlobals.fCubeMapPassDescriptorSetLayout');

 end;

 // Raymarching pass descriptor set layout
 begin
  
  fRaymarchingPassDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(TpvScene3D(fScene3D).VulkanDevice);

  // Subpass depth
  fRaymarchingPassDescriptorSetLayout.AddBinding(0,
                                                 VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE,
                                                 1,
                                                 TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                 []);

  // Transmittance LUT texture
  fRaymarchingPassDescriptorSetLayout.AddBinding(1,
                                                 VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                 1,
                                                 TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                 []);

  // Multi scattering LUT texture
  fRaymarchingPassDescriptorSetLayout.AddBinding(2,
                                                 VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                 1,
                                                 TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                 []);

  // Sky view LUT texture
  fRaymarchingPassDescriptorSetLayout.AddBinding(3,
                                                 VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                 1,
                                                 TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                 []);

  // Camera volume texture
  fRaymarchingPassDescriptorSetLayout.AddBinding(4,
                                                 VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                 1,
                                                 TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                 []);

  // Atmosphere parameters
  fRaymarchingPassDescriptorSetLayout.AddBinding(5,
                                                 VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                 1,
                                                 TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                 []);

  // Blue noise texture
  fRaymarchingPassDescriptorSetLayout.AddBinding(6,
                                                 VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
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
  fMultiScatteringLUTComputePipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TMultiScatteringLUTPushConstants));
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

 begin

  // Cube map compute pipeline

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('atmosphere_cubemap_comp.spv');
  try
   fCubeMapComputeShaderModule:=TpvVulkanShaderModule.Create(TpvScene3D(fScene3D).VulkanDevice,Stream);
  finally
   Stream.Free;
  end;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCubeMapComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DAtmosphereGlobals.fCubeMapComputeShaderModule'); 

  fCubeMapComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fCubeMapComputeShaderModule,'main');

  fCubeMapComputePipelineLayout:=TpvVulkanPipelineLayout.Create(TpvScene3D(fScene3D).VulkanDevice);
  fCubeMapComputePipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TCubeMapPushConstants));
  fCubeMapComputePipelineLayout.AddDescriptorSetLayout(fCubeMapPassDescriptorSetLayout);
  fCubeMapComputePipelineLayout.Initialize;

  fCubeMapComputePipeline:=TpvVulkanComputePipeline.Create(TpvScene3D(fScene3D).VulkanDevice,
                                                           TpvScene3D(fScene3D).VulkanPipelineCache,
                                                           0,
                                                           fCubeMapComputeShaderStage,
                                                           fCubeMapComputePipelineLayout,
                                                           nil,
                                                           0);

 end;

 begin
   
  // Raymarching graphics pipeline

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('fullscreen_vert.spv');
  try
   fRaymarchingVertexShaderModule:=TpvVulkanShaderModule.Create(TpvScene3D(fScene3D).VulkanDevice,Stream);
  finally
   Stream.Free;
  end;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fRaymarchingVertexShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DAtmosphereGlobals.fRaymarchingVertexShaderModule');

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('atmosphere_raymarch_frag.spv');
  try
   fRaymarchingFragmentShaderModules[false]:=TpvVulkanShaderModule.Create(TpvScene3D(fScene3D).VulkanDevice,Stream);
  finally
   Stream.Free;
  end;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fRaymarchingFragmentShaderModules[false].Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DAtmosphereGlobals.fRaymarchingFragmentShaderModules[false]');

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('atmosphere_raymarch_msaa_frag.spv');
  try
   fRaymarchingFragmentShaderModules[true]:=TpvVulkanShaderModule.Create(TpvScene3D(fScene3D).VulkanDevice,Stream);
  finally
   Stream.Free;
  end;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fRaymarchingFragmentShaderModules[true].Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DAtmosphereGlobals.fRaymarchingFragmentShaderModules[true]');

  fRaymarchingPipelineShaderStageVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fRaymarchingVertexShaderModule,'main');

  fRaymarchingPipelineShaderStageFragments[false]:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fRaymarchingFragmentShaderModules[false],'main');

  fRaymarchingPipelineShaderStageFragments[true]:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fRaymarchingFragmentShaderModules[true],'main');

  fRaymarchingPipelineLayout:=TpvVulkanPipelineLayout.Create(TpvScene3D(fScene3D).VulkanDevice);
  fRaymarchingPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),0,SizeOf(TRaymarchingPushConstants));
  fRaymarchingPipelineLayout.AddDescriptorSetLayout(fRaymarchingPassDescriptorSetLayout);
  fRaymarchingPipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
  fRaymarchingPipelineLayout.Initialize;

  // fRaymarchingGraphicsPipeline will be created by the renderer instances when needed, because it depends on the render pass and so on.

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

 FreeAndNil(fCubeMapComputePipeline);
 FreeAndNil(fCubeMapComputePipelineLayout);
 FreeAndNil(fCubeMapComputeShaderStage);
 FreeAndNil(fCubeMapComputeShaderModule);
 
 FreeAndNil(fRaymarchingPipelineLayout);
 FreeAndNil(fRaymarchingPipelineShaderStageVertex);
 FreeAndNil(fRaymarchingPipelineShaderStageFragments[false]);
 FreeAndNil(fRaymarchingPipelineShaderStageFragments[true]);
 FreeAndNil(fRaymarchingVertexShaderModule);
 FreeAndNil(fRaymarchingFragmentShaderModules[false]);
 FreeAndNil(fRaymarchingFragmentShaderModules[true]);

 FreeAndNil(fGlobalVulkanDescriptorSetLayout);
 FreeAndNil(fRaymarchingPassDescriptorSetLayout);
 FreeAndNil(fCubeMapPassDescriptorSetLayout);
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
