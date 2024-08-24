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
     PasVulkan.Scene3D.Renderer.ImageCubeMap,
     PasVulkan.Scene3D.Renderer.MipmapImageCubeMap,
     PasVulkan.Scene3D.Renderer.MipmapImage3D,
     PasVulkan.Scene3D.Renderer.CubeMapMipMapGenerator,
     PasVulkan.Scene3D.Renderer.CubeMapIBLFilter;

type TpvScene3DAtmosphere=class;

     TpvScene3DAtmospheres=class;

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
            TSkyLuminanceLUTPushConstants=packed record
             BaseViewIndex:TpvInt32;
             CountViews:TpvInt32;
             FrameIndex:TpvUInt32;
            end;
            PSkyLuminanceLUTPushConstants=^TSkyLuminanceLUTPushConstants;
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
            TCloudRaymarchingPushConstants=packed record
             BaseViewIndex:TpvInt32;
             CountViews:TpvInt32;
             FrameIndex:TpvUInt32;
             Flags:TpvUInt32;
             CountSamples:TpvUInt32;
            end;
            PCloudRaymarchingPushConstants=^TCloudRaymarchingPushConstants;
            TRaymarchingPushConstants=packed record
             BaseViewIndex:TpvInt32;
             CountViews:TpvInt32;
             FrameIndex:TpvUInt32;
             Flags:TpvUInt32;
             CountSamples:TpvUInt32;
            end;
            PRaymarchingPushConstants=^TRaymarchingPushConstants;
            TCloudWeatherMapPushConstants=packed record
             CoverageRotation:TpvVector4;
             TypeRotation:TpvVector4;
             WetnessRotation:TpvVector4;
             TopRotation:TpvVector4;
             CoveragePerlinWorleyDifference:TpvFloat;
             TotalSize:TpvFloat;
             WorleySeed:TpvFloat;
            end;
            PCloudWeatherMapPushConstants=^TCloudWeatherMapPushConstants;
      private
       fScene3D:TObject;
       fAtmospheres:TpvScene3DAtmospheres;
       fTransmittanceLUTPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fMultiScatteringLUTPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fSkyLuminanceLUTPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fSkyViewLUTPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fCameraVolumePassDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fCubeMapPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fCloudRaymarchingPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fRaymarchingPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fGlobalVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fWeatherMapTextureDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fTransmittanceLUTComputeShaderModule:TpvVulkanShaderModule;
       fTransmittanceLUTComputeShaderStage:TpvVulkanPipelineShaderStage;
       fTransmittanceLUTComputePipelineLayout:TpvVulkanPipelineLayout;
       fTransmittanceLUTComputePipeline:TpvVulkanComputePipeline;
       fMultiScatteringLUTComputeShaderModule:TpvVulkanShaderModule;
       fMultiScatteringLUTComputeShaderStage:TpvVulkanPipelineShaderStage;
       fMultiScatteringLUTComputePipelineLayout:TpvVulkanPipelineLayout;
       fMultiScatteringLUTComputePipeline:TpvVulkanComputePipeline;
       fSkyLuminanceLUTComputeShaderModule:TpvVulkanShaderModule;
       fSkyLuminanceLUTComputeShaderStage:TpvVulkanPipelineShaderStage;
       fSkyLuminanceLUTComputePipelineLayout:TpvVulkanPipelineLayout;
       fSkyLuminanceLUTComputePipeline:TpvVulkanComputePipeline; 
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
       fCloudRaymarchingPipelineLayout:TpvVulkanPipelineLayout;
       fRaymarchingPipelineLayout:TpvVulkanPipelineLayout;
       fCloudWeatherMapComputeShaderModule:TpvVulkanShaderModule;
       fCloudWeatherMapComputeShaderStage:TpvVulkanPipelineShaderStage;
       fCloudWeatherMapComputePipelineLayout:TpvVulkanPipelineLayout;
       fCloudWeatherMapComputePipeline:TpvVulkanComputePipeline;
       fCloudCurlTexture:TpvScene3DRendererMipmapImage3D;
       fCloudDetailTexture:TpvScene3DRendererMipmapImage3D;
       fCloudShapeTexture:TpvScene3DRendererMipmapImage3D;
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
       property SkyLuminanceLUTPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fSkyLuminanceLUTPassDescriptorSetLayout;
       property SkyViewLUTPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fSkyViewLUTPassDescriptorSetLayout;
       property CameraVolumePassDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fCameraVolumePassDescriptorSetLayout;
       property CubeMapPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fCubeMapPassDescriptorSetLayout;
       property CloudRaymarchingPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fCloudRaymarchingPassDescriptorSetLayout;
       property RaymarchingPassDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fRaymarchingPassDescriptorSetLayout;
       property GlobalVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fGlobalVulkanDescriptorSetLayout;
       property WeatherMapTextureDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fWeatherMapTextureDescriptorSetLayout;
       property CloudRaymarchingPipelineLayout:TpvVulkanPipelineLayout read fCloudRaymarchingPipelineLayout;
       property RaymarchingPipelineLayout:TpvVulkanPipelineLayout read fRaymarchingPipelineLayout;
     end;

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
             SkyLuminanceLUTRes=8;
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
            { TVolumetricCloudLayer }
            TVolumetricCloudLayer=packed record
             public
              Albedo:TpvVector4; // w = unused
              ExtinctionCoefficient:TpvVector4; // w = CoverageWindAngle
              ///////
              SkewAlongWindDirection:TpvFloat;
              TotalNoiseScale:TpvFloat;
              CurlScale:TpvFloat;
              CurlNoiseHeightFraction:TpvFloat;
              ///////
              CurlNoiseModifier:TpvFloat;
              DetailScale:TpvFloat;
              DetailNoiseHeightFraction:TpvFloat;
              DetailNoiseModifier:TpvFloat;
              ///////
              SkewAlongCoverageWindDirection:TpvFloat;
              WeatherScale:TpvFloat;
              CoverageAmount:TpvFloat;
              CoverageMinimum:TpvFloat;
              ///////
              TypeAmount:TpvFloat;
              TypeMinimum:TpvFloat;
              RainAmount:TpvFloat;
              RainMinimum:TpvFloat;
              ///////
              GradientSmall:TpvVector4;
              GradientMedium:TpvVector4;
              GradientLarge:TpvVector4;
              AnvilDeformationSmall:TpvVector4;
              AnvilDeformationMedium:TpvVector4;
              AnvilDeformationLarge:TpvVector4;
              ///////
              WindSpeed:TpvFloat;
              WindAngle:TpvFloat;
              WindUpAmount:TpvFloat;
              CoverageWindSpeed:TpvFloat;
              function GetCoverageWindAngle:TpvFloat;
              procedure SetCoverageWindAngle(const aCoverageWindAngle:TpvFloat);
              procedure LoadFromJSON(const aJSON:TPasJSONItem);
              procedure LoadFromJSONStream(const aStream:TStream);
              procedure LoadFromJSONFile(const aFileName:string);
              function SaveToJSON:TPasJSONItemObject;
              procedure SaveToJSONStream(const aStream:TStream);
              procedure SaveToJSONFile(const aFileName:string);
              property CoverageWindAngle:TpvFloat read GetCoverageWindAngle write SetCoverageWindAngle;
            end;
            PVolumetricCloudLayer=^TVolumetricCloudLayer;
            { TVolumetricCloudParametersOld }
            TVolumetricCloudParametersOld=packed record
             public
              BeerPowder:TpvFloat;
              BeerPowderPower:TpvFloat;
              AmbientGroundMultiplier:TpvFloat;
              PhaseG:TpvFloat;
              ///////
              PhaseG2:TpvFloat;
              PhaseBlend:TpvFloat;
              MultiScatteringScattering:TpvFloat;
              MultiScatteringExtinction:TpvFloat;
              ///////
              MultiScatteringEccentricity:TpvFloat;
              ShadowStepLength:TpvFloat;
              HorizonBlendAmount:TpvFloat;
              HorizonBlendPower:TpvFloat;
              ///////
              CloudStartHeight:TpvFloat;
              CloudThickness:TpvFloat;
              AnimationMultiplier:TpvFloat;
              Padding0:TpvFloat;
              ///////
              MaxStepCount:TpvInt32;
              MaxMarchingDistance:TpvFloat;
              InverseDistanceStepCount:TpvFloat;
              RenderDistance:TpvFloat;
              ///////
              LODDistance:TpvFloat;
              LODMin:TpvFloat;
              BigStepMarch:TpvFloat;
              TransmittanceThreshold:TpvFloat;            
              ///////
              ShadowSampleCount:TpvFloat;
              GroundContributionSampleCount:TpvFloat;
              Padding1:TpvFloat;
              Padding2:TpvFloat;
              ///////
              Layers:array[0..1] of TVolumetricCloudLayer;
              procedure Initialize;
              procedure LoadFromJSON(const aJSON:TPasJSONItem);
              procedure LoadFromJSONStream(const aStream:TStream);
              procedure LoadFromJSONFile(const aFileName:string);
              function SaveToJSON:TPasJSONItemObject;
              procedure SaveToJSONStream(const aStream:TStream);
              procedure SaveToJSONFile(const aFileName:string);
            end;
            PVolumetricCloudParametersOld=^TVolumetricCloudParametersOld;
            { TVolumetricCloudLayerLow }
            TVolumetricCloudLayerLow=packed record
             public
          
              Orientation:TpvQuaternion;

              StartHeight:TpvFloat;
              EndHeight:TpvFloat;
              PositionScale:TpvFloat;
              ShapeNoiseScale:TpvFloat;
          
              DetailNoiseScale:TpvFloat;
              CurlScale:TpvFloat;
              AdvanceCurlScale:TpvFloat;
              AdvanceCurlAmplitude:TpvFloat;
          
              HeightGradients:array[0..2] of TpvVector4; // mat3x4
              AnvilDeformations:array[0..2] of TpvVector4; // mat3x4 unused for now
          
              procedure Initialize;
              procedure LoadFromJSON(const aJSON:TPasJSONItem);
              procedure LoadFromJSONStream(const aStream:TStream);
              procedure LoadFromJSONFile(const aFileName:string);
              function SaveToJSON:TPasJSONItemObject;
              procedure SaveToJSONStream(const aStream:TStream);
              procedure SaveToJSONFile(const aFileName:string);
            end;
            PVolumetricCloudLayerLow=^TVolumetricCloudLayerLow;
            { TVolumetricCloudLayerHigh }
            TVolumetricCloudLayerHigh=packed record
             public

              Orientation:TpvQuaternion;
          
              StartHeight:TpvFloat;
              EndHeight:TpvFloat;
              PositionScale:TpvFloat;
              Density:TpvFloat;
          
              CoverMin:TpvFloat;
              CoverMax:TpvFloat;
              FadeMin:TpvFloat;
              FadeMax:TpvFloat;
          
              Speed:TpvFloat;
              Padding0:TpvFloat;
              Padding1:TpvFloat;
              Padding2:TpvFloat;
          
              RotationBase:TpvVector4;
          
              RotationOctave1:TpvVector4;
          
              RotationOctave2:TpvVector4;
          
              RotationOctave3:TpvVector4;
          
              OctaveScales:TpvVector4;
          
              OctaveFactors:TpvVector4;
          
              procedure Initialize;
              procedure LoadFromJSON(const aJSON:TPasJSONItem);
              procedure LoadFromJSONStream(const aStream:TStream);
              procedure LoadFromJSONFile(const aFileName:string);
              function SaveToJSON:TPasJSONItemObject;
              procedure SaveToJSONStream(const aStream:TStream);
              procedure SaveToJSONFile(const aFileName:string);

            end;
            PVolumetricCloudLayerHigh=^TVolumetricCloudLayerHigh;
            { TVolumetricCloudParameters }
            TVolumetricCloudParameters=packed record
             public
          
              CoverageTypeWetnessTopFactors:TpvVector4; // x = Coverage, y = Type, z = Wetness, w = Top
          
              CoverageTypeWetnessTopOffsets:TpvVector4; // x = Coverage, y = Type, z = Wetness, w = Top
          
              Scattering:TpvVector4; // w = unused
          
              Absorption:TpvVector4; // w = unused
          
              LightingDensity:TpvFloat;
              ShadowDensity:TpvFloat;
              ViewDensity:TpvFloat;
              DensityScale:TpvFloat;
          
              Scale:TpvFloat;
              ForwardScatteringG:TpvFloat;
              BackwardScatteringG:TpvFloat;
              ShadowRayLength:TpvFloat;
          
              DensityAlongConeLength:TpvFloat;
              DensityAlongConeLengthFarMultiplier:TpvFloat;
              RayMinSteps:TpvUInt32;
              RayMaxSteps:TpvUInt32;

              OuterSpaceRayMinSteps:TpvUInt32;
              OuterSpaceRayMaxSteps:TpvUInt32;

              DirectScatteringIntensity:TpvFloat;
              IndirectScatteringIntensity:TpvFloat;

              AmbientLightIntensity:TpvFloat;
              Padding0:TpvFloat;
              Padding1:TpvFloat;
              Padding2:TpvFloat;

              LayerLow:TVolumetricCloudLayerLow;
          
              LayerHigh:TVolumetricCloudLayerHigh;

              procedure Initialize;
              procedure LoadFromJSON(const aJSON:TPasJSONItem);
              procedure LoadFromJSONStream(const aStream:TStream);
              procedure LoadFromJSONFile(const aFileName:string);
              function SaveToJSON:TPasJSONItemObject;
              procedure SaveToJSONStream(const aStream:TStream);
              procedure SaveToJSONFile(const aFileName:string);

            end;
            PVolumetricCloudParameters=^TVolumetricCloudParameters;
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
              VolumetricClouds:TVolumetricCloudParameters;
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
            { TGPUVolumetricCloudLayerLow }
            TGPUVolumetricCloudLayerLow=packed record
             public

              Orientation:TpvQuaternion;

              StartHeight:TpvFloat;
              EndHeight:TpvFloat;
              PositionScale:TpvFloat;
              ShapeNoiseScale:TpvFloat;
             
              DetailNoiseScale:TpvFloat;
              CurlScale:TpvFloat;
              AdvanceCurlScale:TpvFloat;
              AdvanceCurlAmplitude:TpvFloat;
             
              HeightGradients:array[0..2] of TpvVector4; // mat3x4
              
              AnvilDeformations:array[0..2] of TpvVector4; // mat3x4 unused for now

              procedure Assign(const aVolumetricCloudLayerLow:TVolumetricCloudLayerLow);
            end;
            PGPUVolumetricCloudLayerLow=^TGPUVolumetricCloudLayerLow;
            { TGPUVolumetricCloudLayerHigh }
            TGPUVolumetricCloudLayerHigh=packed record
             public
              
              Orientation:TpvQuaternion;

              StartHeight:TpvFloat;
              EndHeight:TpvFloat;              
              PositionScale:TpvFloat;
              Density:TpvFloat;

              CoverMin:TpvFloat;
              CoverMax:TpvFloat;
              FadeMin:TpvFloat;
              FadeMax:TpvFloat;
              
              Speed:TpvFloat;
              Padding0:TpvFloat;
              Padding1:TpvFloat;
              Padding2:TpvFloat;

              RotationBase:TpvVector4;
              
              RotationOctave1:TpvVector4;
              
              RotationOctave2:TpvVector4;
              
              RotationOctave3:TpvVector4;
              
              OctaveScales:TpvVector4;
              
              OctaveFactors:TpvVector4;
              
              procedure Assign(const aVolumetricCloudLayerHigh:TVolumetricCloudLayerHigh);
            end;
            PGPUVolumetricCloudLayerHigh=^TGPUVolumetricCloudLayerHigh;
            { TGPUVolumetricCloudParameters }
            TGPUVolumetricCloudParameters=packed record
             public
             
              CoverageTypeWetnessTopFactors:TpvVector4; // x = Coverage, y = Type, z = Wetness, w = Top
             
              CoverageTypeWetnessTopOffsets:TpvVector4; // x = Coverage, y = Type, z = Wetness, w = Top
             
              Scattering:TpvVector4; // w = unused
             
              Absorption:TpvVector4; // w = unused
             
              LightingDensity:TpvFloat;
              ShadowDensity:TpvFloat;
              ViewDensity:TpvFloat;
              DensityScale:TpvFloat;
             
              Scale:TpvFloat;
              ForwardScatteringG:TpvFloat;
              BackwardScatteringG:TpvFloat;
              ShadowRayLength:TpvFloat;
             
              DensityAlongConeLength:TpvFloat;
              DensityAlongConeLengthFarMultiplier:TpvFloat;
              RayMinSteps:TpvUInt32;
              RayMaxSteps:TpvUInt32;
             
              OuterSpaceRayMinSteps:TpvUInt32;
              OuterSpaceRayMaxSteps:TpvUInt32;
              DirectScatteringIntensity:TpvFloat;
              IndirectScatteringIntensity:TpvFloat;

              AmbientLightIntensity:TpvFloat;
              Padding0:TpvFloat;
              Padding1:TpvFloat;
              Padding2:TpvFloat;

              LayerLow:TGPUVolumetricCloudLayerLow;
              LayerHigh:TGPUVolumetricCloudLayerHigh;
             
              procedure Assign(const aVolumetricCloudParameters:TVolumetricCloudParameters);
            end;
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
              VolumetricClouds:TGPUVolumetricCloudParameters;
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
              fMultiScatteringTexture:TpvScene3DRendererImage2D;
              fSkyLuminanceLUTTexture:TpvScene3DRendererImageCubeMap;
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
              fSkyLuminanceLUTPassDescriptorPool:TpvVulkanDescriptorPool;
              fSkyLuminanceLUTPassDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fSkyViewLUTPassDescriptorPool:TpvVulkanDescriptorPool;
              fSkyViewLUTPassDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fCameraVolumePassDescriptorPool:TpvVulkanDescriptorPool;
              fCameraVolumePassDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fCubeMapPassDescriptorPool:TpvVulkanDescriptorPool;
              fCubeMapPassDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fCloudRaymarchingPassDescriptorPool:TpvVulkanDescriptorPool;
              fCloudRaymarchingPassDepthImageViews:array[0..MaxInFlightFrames-1] of TVkImageView;
              fCloudRaymarchingPassCascadedShadowMapImageViews:array[0..MaxInFlightFrames-1] of TVkImageView;
              fCloudRaymarchingPassDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fCloudRaymarchingPassDescriptorSetFirsts:array[0..MaxInFlightFrames-1] of boolean;
              fRaymarchingPassDescriptorPool:TpvVulkanDescriptorPool;
              fRaymarchingPassDepthImageViews:array[0..MaxInFlightFrames-1] of TVkImageView;
              fRaymarchingPassCascadedShadowMapImageViews:array[0..MaxInFlightFrames-1] of TVkImageView;
              fRaymarchingPassCloudsInscatteringImageViews:array[0..MaxInFlightFrames-1] of TVkImageView;
              fRaymarchingPassCloudsTransmittanceImageViews:array[0..MaxInFlightFrames-1] of TVkImageView;
              fRaymarchingPassCloudsDepthImageViews:array[0..MaxInFlightFrames-1] of TVkImageView;
              fRaymarchingPassDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fRaymarchingPassDescriptorSetFirsts:array[0..MaxInFlightFrames-1] of boolean;
              fGlobalDescriptorPool:TpvVulkanDescriptorPool;
              fGlobalDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fCubeMapMipMapGenerator:TpvScene3DRendererCubeMapMipMapGenerator;
              fGGXCubeMapIBLFilter:TpvScene3DRendererCubeMapIBLFilter;
              fCharlieCubeMapIBLFilter:TpvScene3DRendererCubeMapIBLFilter;
              fLambertianCubeMapIBLFilter:TpvScene3DRendererCubeMapIBLFilter;
             public
              constructor Create(const aAtmosphere:TpvScene3DAtmosphere;const aRendererInstance:TObject);
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure SetCloudsImageViews(const aInFlightFrameIndex:TpvSizeInt;
                                            const aDepthImageView:TVkImageView;
                                            const aCascadedShadowMapImageView:TVkImageView);
              procedure SetImageViews(const aInFlightFrameIndex:TpvSizeInt;
                                      const aDepthImageView:TVkImageView;
                                      const aCascadedShadowMapImageView:TVkImageView;
                                      const aCloudsInscatteringImageView:TVkImageView;
                                      const aCloudsTransmittanceImageView:TVkImageView;
                                      const aCloudsDepthImageView:TVkImageView);
              procedure Setup(const aRenderPass:TpvVulkanRenderPass;
                              const aRenderPassSubpassIndex:TpvSizeInt;
                              const aSampleCount:TVkSampleCountFlagBits;
                              const aWidth:TpvSizeInt;
                              const aHeight:TpvSizeInt);
              procedure ReleaseGraphicsPipeline;
              procedure Execute(const aInFlightFrameIndex:TpvSizeInt;
                                const aCommandBuffer:TpvVulkanCommandBuffer);
              procedure DrawClouds(const aInFlightFrameIndex:TpvSizeInt;
                                   const aCommandBuffer:TpvVulkanCommandBuffer;
                                   const aDepthImageView:TVkImageView;
                                   const aCascadedShadowMapImageView:TVkImageView);
              procedure Draw(const aInFlightFrameIndex:TpvSizeInt;
                             const aCommandBuffer:TpvVulkanCommandBuffer;
                             const aDepthImageView:TVkImageView;
                             const aCascadedShadowMapImageView:TVkImageView;
                             const aCloudsInscatteringImageView:TVkImageView;
                             const aCloudsTransmittanceImageView:TVkImageView;
                             const aCloudsDepthImageView:TVkImageView);
             published
              property Atmosphere:TpvScene3DAtmosphere read fAtmosphere;
              property RendererInstance:TObject read fRendererInstance;
              property TransmittanceTexture:TpvScene3DRendererImage2D read fTransmittanceTexture;
              property MultiScatteringTexture:TpvScene3DRendererImage2D read fMultiScatteringTexture;
              property SkyLuminanceLUTTexture:TpvScene3DRendererImageCubeMap read fSkyLuminanceLUTTexture;
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
       fWeatherMapTexture:TpvScene3DRendererImageCubeMap;
       fWeatherMapTextureDescriptorPool:TpvVulkanDescriptorPool;
       fWeatherMapTextureDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
       fWeatherMapTextureGeneration:TpvUInt64;
       fWeatherMapTextureLastGeneration:TpvUInt64;
       fCloudWeatherMapPushConstants:TpvScene3DAtmosphereGlobals.TCloudWeatherMapPushConstants;
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
       function IsInFlightFrameVisible(const aInFlightFrameIndex:TpvSizeInt):Boolean;
       function GetRenderInstance(const aRendererInstance:TObject):TpvScene3DAtmosphere.TRendererInstance;
       procedure ProcessSimulation(const aCommandBuffer:TpvVulkanCommandBuffer;
                                   const aInFlightFrameIndex:TpvSizeInt);
       procedure Execute(const aInFlightFrameIndex:TpvSizeInt;
                         const aCommandBuffer:TpvVulkanCommandBuffer;
                         const aRendererInstance:TObject);
       procedure DrawClouds(const aInFlightFrameIndex:TpvSizeInt;
                            const aCommandBuffer:TpvVulkanCommandBuffer;
                            const aDepthImageView:TVkImageView;
                            const aCascadedShadowMapImageView:TVkImageView;
                            const aRendererInstance:TObject);
       procedure Draw(const aInFlightFrameIndex:TpvSizeInt;
                      const aCommandBuffer:TpvVulkanCommandBuffer;
                      const aDepthImageView:TVkImageView;
                      const aCascadedShadowMapImageView:TVkImageView;
                      const aCloudsInscatteringImageView:TVkImageView;
                      const aCloudsTransmittanceImageView:TVkImageView;
                      const aCloudsDepthImageView:TVkImageView;
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
       procedure DrawClouds(const aInFlightFrameIndex:TpvSizeInt;
                            const aCommandBuffer:TpvVulkanCommandBuffer;
                            const aDepthImageView:TVkImageView;
                            const aCascadedShadowMapImageView:TVkImageView;
                            const aRendererInstance:TObject);
       procedure Draw(const aInFlightFrameIndex:TpvSizeInt;
                      const aCommandBuffer:TpvVulkanCommandBuffer;
                      const aDepthImageView:TVkImageView;
                      const aCascadedShadowMapImageView:TVkImageView;
                      const aCloudsInscatteringImageView:TVkImageView;
                      const aCloudsTransmittanceImageView:TVkImageView;
                      const aCloudsDepthImageView:TVkImageView;
                      const aRendererInstance:TObject);
      published
       property Scene3D:TObject read fScene3D;
       property Lock:TPasMPMultipleReaderSingleWriterLock read fLock;
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

{ TpvScene3DAtmosphere.TVolumetricCloudLayer }

function TpvScene3DAtmosphere.TVolumetricCloudLayer.GetCoverageWindAngle:TpvFloat;
begin
 result:=ExtinctionCoefficient.w;
end;

procedure TpvScene3DAtmosphere.TVolumetricCloudLayer.SetCoverageWindAngle(const aCoverageWindAngle:TpvFloat);
begin
 ExtinctionCoefficient.w:=aCoverageWindAngle;
end;

procedure TpvScene3DAtmosphere.TVolumetricCloudLayer.LoadFromJSON(const aJSON:TPasJSONItem);
var JSONObject:TPasJSONItemObject;
    JSONValue:TPasJSONItem;
begin
 if assigned(aJSON) and (aJSON is TPasJSONItemObject) then begin
  JSONObject:=TPasJSONItemObject(aJSON);
  Albedo:=TpvVector4.InlineableCreate(JSONToVector3(JSONObject.Properties['albedo'],Albedo.xyz),0.0);
  ExtinctionCoefficient:=TpvVector4.InlineableCreate(JSONToVector3(JSONObject.Properties['extinctioncoefficient'],ExtinctionCoefficient.xyz),TPasJSON.GetNumber(JSONObject.Properties['coveragewindangle'],CoverageWindAngle));
  SkewAlongWindDirection:=TPasJSON.GetNumber(JSONObject.Properties['skewalongwinddirection'],SkewAlongWindDirection);
  TotalNoiseScale:=TPasJSON.GetNumber(JSONObject.Properties['totalnoisescale'],TotalNoiseScale);
  CurlScale:=TPasJSON.GetNumber(JSONObject.Properties['curlscale'],CurlScale);
  CurlNoiseHeightFraction:=TPasJSON.GetNumber(JSONObject.Properties['curlnoiseheightfraction'],CurlNoiseHeightFraction);
  CurlNoiseModifier:=TPasJSON.GetNumber(JSONObject.Properties['curlnoisemodifier'],CurlNoiseModifier);
  DetailScale:=TPasJSON.GetNumber(JSONObject.Properties['detailscale'],DetailScale);
  DetailNoiseHeightFraction:=TPasJSON.GetNumber(JSONObject.Properties['detailnoiseheightfraction'],DetailNoiseHeightFraction);
  DetailNoiseModifier:=TPasJSON.GetNumber(JSONObject.Properties['detailnoisemodifier'],DetailNoiseModifier);
  SkewAlongCoverageWindDirection:=TPasJSON.GetNumber(JSONObject.Properties['skewalongcoveragewinddirection'],SkewAlongCoverageWindDirection);
  WeatherScale:=TPasJSON.GetNumber(JSONObject.Properties['weatherscale'],WeatherScale);
  CoverageAmount:=TPasJSON.GetNumber(JSONObject.Properties['coverageamount'],CoverageAmount);
  CoverageMinimum:=TPasJSON.GetNumber(JSONObject.Properties['coverageminimum'],CoverageMinimum);
  TypeAmount:=TPasJSON.GetNumber(JSONObject.Properties['typeamount'],TypeAmount);
  TypeMinimum:=TPasJSON.GetNumber(JSONObject.Properties['typeminimum'],TypeMinimum);
  RainAmount:=TPasJSON.GetNumber(JSONObject.Properties['rainamount'],RainAmount);
  RainMinimum:=TPasJSON.GetNumber(JSONObject.Properties['rainminimum'],RainMinimum);
  GradientSmall:=JSONToVector4(JSONObject.Properties['gradientsmall'],GradientSmall);
  GradientMedium:=JSONToVector4(JSONObject.Properties['gradientmedium'],GradientMedium);
  GradientLarge:=JSONToVector4(JSONObject.Properties['gradientlarge'],GradientLarge);
  AnvilDeformationSmall:=JSONToVector4(JSONObject.Properties['anvildeformationsmall'],AnvilDeformationSmall);
  AnvilDeformationMedium:=JSONToVector4(JSONObject.Properties['anvildeformationmedium'],AnvilDeformationMedium);
  AnvilDeformationLarge:=JSONToVector4(JSONObject.Properties['anvildeformationlarge'],AnvilDeformationLarge);
  WindSpeed:=TPasJSON.GetNumber(JSONObject.Properties['windspeed'],WindSpeed);
  WindAngle:=TPasJSON.GetNumber(JSONObject.Properties['windangle'],WindAngle);
  WindUpAmount:=TPasJSON.GetNumber(JSONObject.Properties['windupamount'],WindUpAmount);
  CoverageWindSpeed:=TPasJSON.GetNumber(JSONObject.Properties['coveragewindspeed'],CoverageWindSpeed);
  //CoverageWindAngle:=TPasJSON.GetNumber(JSONObject.Properties['coveragewindangle'],CoverageWindAngle); // Already done above
 end;  
end;  

procedure TpvScene3DAtmosphere.TVolumetricCloudLayer.LoadFromJSONStream(const aStream:TStream);
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

procedure TpvScene3DAtmosphere.TVolumetricCloudLayer.LoadFromJSONFile(const aFileName:string);
var Stream:TMemoryStream;
begin
 Stream:=TMemoryStream.Create;
 try
  Stream.LoadFromFile(aFileName);
  Stream.Seek(0,soBeginning);
  LoadFromJSONStream(Stream);
 finally
  Stream.Free;
 end;
end;

function TpvScene3DAtmosphere.TVolumetricCloudLayer.SaveToJSON:TPasJSONItemObject;
begin
 result:=TPasJSONItemObject.Create;
 result.Add('albedo',Vector3ToJSON(Albedo.xyz));
 result.Add('extinctioncoefficient',Vector3ToJSON(ExtinctionCoefficient.xyz));
 result.Add('skewalongwinddirection',TPasJSONItemNumber.Create(SkewAlongWindDirection));
 result.Add('totalnoisescale',TPasJSONItemNumber.Create(TotalNoiseScale));
 result.Add('curlscale',TPasJSONItemNumber.Create(CurlScale));
 result.Add('curlnoiseheightfraction',TPasJSONItemNumber.Create(CurlNoiseHeightFraction));
 result.Add('curlnoisemodifier',TPasJSONItemNumber.Create(CurlNoiseModifier));
 result.Add('detailscale',TPasJSONItemNumber.Create(DetailScale));
 result.Add('detailnoiseheightfraction',TPasJSONItemNumber.Create(DetailNoiseHeightFraction));
 result.Add('detailnoisemodifier',TPasJSONItemNumber.Create(DetailNoiseModifier));
 result.Add('skewalongcoveragewinddirection',TPasJSONItemNumber.Create(SkewAlongCoverageWindDirection));
 result.Add('weatherscale',TPasJSONItemNumber.Create(WeatherScale));
 result.Add('coverageamount',TPasJSONItemNumber.Create(CoverageAmount));
 result.Add('coverageminimum',TPasJSONItemNumber.Create(CoverageMinimum));
 result.Add('typeamount',TPasJSONItemNumber.Create(TypeAmount));
 result.Add('typeminimum',TPasJSONItemNumber.Create(TypeMinimum));
 result.Add('rainamount',TPasJSONItemNumber.Create(RainAmount));
 result.Add('rainminimum',TPasJSONItemNumber.Create(RainMinimum));
 result.Add('gradientsmall',Vector4ToJSON(GradientSmall));
 result.Add('gradientmedium',Vector4ToJSON(GradientMedium));
 result.Add('gradientlarge',Vector4ToJSON(GradientLarge));
 result.Add('anvildeformationsmall',Vector4ToJSON(AnvilDeformationSmall));
 result.Add('anvildeformationmedium',Vector4ToJSON(AnvilDeformationMedium));
 result.Add('anvildeformationlarge',Vector4ToJSON(AnvilDeformationLarge));
 result.Add('windspeed',TPasJSONItemNumber.Create(WindSpeed));
 result.Add('windangle',TPasJSONItemNumber.Create(WindAngle));
 result.Add('windupamount',TPasJSONItemNumber.Create(WindUpAmount));
 result.Add('coveragewindspeed',TPasJSONItemNumber.Create(CoverageWindSpeed));
 result.Add('coveragewindangle',TPasJSONItemNumber.Create(CoverageWindAngle));
end;

procedure TpvScene3DAtmosphere.TVolumetricCloudLayer.SaveToJSONStream(const aStream:TStream);
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

procedure TpvScene3DAtmosphere.TVolumetricCloudLayer.SaveToJSONFile(const aFileName:string);
var Stream:TMemoryStream;
begin
 Stream:=TMemoryStream.Create;
 try
  SaveToJSONStream(Stream);
  Stream.Seek(0,soBeginning);
  Stream.SaveToFile(aFileName);
 finally
  Stream.Free;
 end;
end;

{ TpvScene3DAtmosphere.TVolumetricCloudParametersOld }

procedure TpvScene3DAtmosphere.TVolumetricCloudParametersOld.Initialize;
begin

 BeerPowder:=20.0;
 BeerPowderPower:=0.5;
 AmbientGroundMultiplier:=0.75;
 PhaseG:=0.5; // [-0.999; 0.999]
 PhaseG2:=-0.5; // [-0.999; 0.999]
 PhaseBlend:=0.2; // [0; 1]
 MultiScatteringScattering:=1.0;
 MultiScatteringExtinction:=0.1;
 MultiScatteringEccentricity:=0.2;
 ShadowStepLength:=3000.0;
 HorizonBlendAmount:=0.0000125;
 HorizonBlendPower:=2.0;

 CloudStartHeight:=1500.0;
 CloudThickness:=5000.0;
 
 begin
 
  Layers[0].Albedo:=TpvVector4.InlineableCreate(0.9,0.9,0.9,0.0);
  Layers[0].ExtinctionCoefficient:=TpvVector4.InlineableCreate(0.71*0.1,0.86*0.1,1.0*0.1,0.0);
  Layers[0].SkewAlongWindDirection:=700.0;

  Layers[0].TotalNoiseScale:=0.0006;
  Layers[0].CurlScale:=0.3;
  Layers[0].CurlNoiseHeightFraction:=5.0;
  Layers[0].CurlNoiseModifier:=500.0;
  Layers[0].DetailScale:=4.0;
  Layers[0].DetailNoiseHeightFraction:=10.0;
  Layers[0].DetailNoiseModifier:=0.3;
  Layers[0].SkewAlongCoverageWindDirection:=2500.0;
  Layers[0].WeatherScale:=0.00002;
  Layers[0].CoverageAmount:=1.0;
  Layers[0].CoverageMinimum:=0.0;
  Layers[0].TypeAmount:=1.0;
  Layers[0].TypeMinimum:=0.0;
  Layers[0].RainAmount:=0.0;
  Layers[0].RainMinimum:=0.0;

  Layers[0].GradientSmall:=TpvVector4.InlineableCreate(0.01,0.1,0.11,0.2);
  Layers[0].GradientMedium:=TpvVector4.InlineableCreate(0.01,0.08,0.3,0.4);
  Layers[0].GradientLarge:=TpvVector4.InlineableCreate(0.01,0.06,0.75,0.95);

  Layers[0].AnvilDeformationSmall:=TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0);
  Layers[0].AnvilDeformationMedium:=TpvVector4.InlineableCreate(15.0,0.1,15.0,0.1);
  Layers[0].AnvilDeformationLarge:=TpvVector4.InlineableCreate(5.0,0.25,5.0,0.15);

  Layers[0].WindSpeed:=15.0;
  Layers[0].WindAngle:=0.75;
  Layers[0].WindUpAmount:=0.5;
  Layers[0].CoverageWindSpeed:=30.0;
  Layers[0].CoverageWindAngle:=0.0;

 end; 

 begin
 
  Layers[1].Albedo:=TpvVector4.InlineableCreate(0.9,0.9,0.9,0.0);
  Layers[1].ExtinctionCoefficient:=TpvVector4.InlineableCreate(0.71*0.01,0.86*0.01,1.0*0.01,0.0);
  Layers[1].SkewAlongWindDirection:=400.0;

  Layers[1].TotalNoiseScale:=0.0006;
  Layers[1].CurlScale:=0.1;
  Layers[1].CurlNoiseHeightFraction:=500.0;
  Layers[1].CurlNoiseModifier:=250.0;
  Layers[1].DetailScale:=2.0;
  Layers[1].DetailNoiseHeightFraction:=0.0;
  Layers[1].DetailNoiseModifier:=1.0;
  Layers[1].SkewAlongCoverageWindDirection:=0.0;
  Layers[1].WeatherScale:=0.000025;
  Layers[1].CoverageAmount:=0.0;
  Layers[1].CoverageMinimum:=0.0;
  Layers[1].TypeAmount:=1.0;
  Layers[1].TypeMinimum:=0.0;
  Layers[1].RainAmount:=0.0;
  Layers[1].RainMinimum:=0.0;

  Layers[1].GradientSmall:=TpvVector4.InlineableCreate(0.6,0.62,0.63,0.65);
  Layers[1].GradientMedium:=TpvVector4.InlineableCreate(0.6,0.64,0.66,0.7);
  Layers[1].GradientLarge:=TpvVector4.InlineableCreate(0.6,0.66,0.69,0.75);

  Layers[1].AnvilDeformationSmall:=TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0);
  Layers[1].AnvilDeformationMedium:=TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0);
  Layers[1].AnvilDeformationLarge:=TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0);

  Layers[1].WindSpeed:=10.0;
  Layers[1].WindAngle:=1.0;
  Layers[1].WindUpAmount:=0.1;
  Layers[1].CoverageWindSpeed:=50.0;
  Layers[1].CoverageWindAngle:=1.0;

 end;

 AnimationMultiplier:=2.0;
 
 MaxStepCount:=96;
 MaxMarchingDistance:=30000.0;
 InverseDistanceStepCount:=15000.0;
 RenderDistance:=70000.0;
 LODDistance:=30000.0;
 LODMin:=0;
 BigStepMarch:=2.0;
 TransmittanceThreshold:=0.005;
 ShadowSampleCount:=5.0;
 GroundContributionSampleCount:=3.0;

end;

procedure TpvScene3DAtmosphere.TVolumetricCloudParametersOld.LoadFromJSON(const aJSON:TPasJSONItem);
var JSONRootObject:TPasJSONItemObject;
    JSON:TPasJSONItem;
    Index:TpvInt32;
begin

 if assigned(aJSON) and (aJSON is TPasJSONItemObject) then begin

  JSONRootObject:=TPasJSONItemObject(aJSON);

  BeerPowder:=TPasJSON.GetNumber(JSONRootObject.Properties['beerpowder'],BeerPowder);
  BeerPowderPower:=TPasJSON.GetNumber(JSONRootObject.Properties['beerpowderpower'],BeerPowderPower);
  AmbientGroundMultiplier:=TPasJSON.GetNumber(JSONRootObject.Properties['ambientgroundmultiplier'],AmbientGroundMultiplier);
  PhaseG:=TPasJSON.GetNumber(JSONRootObject.Properties['phaseg'],PhaseG);
  PhaseG2:=TPasJSON.GetNumber(JSONRootObject.Properties['phaseg2'],PhaseG2);
  PhaseBlend:=TPasJSON.GetNumber(JSONRootObject.Properties['phaseblend'],PhaseBlend);
  MultiScatteringScattering:=TPasJSON.GetNumber(JSONRootObject.Properties['multiscatteringscattering'],MultiScatteringScattering);
  MultiScatteringExtinction:=TPasJSON.GetNumber(JSONRootObject.Properties['multiscatteringextinction'],MultiScatteringExtinction);
  MultiScatteringEccentricity:=TPasJSON.GetNumber(JSONRootObject.Properties['multiScatteringeccentricity'],MultiScatteringEccentricity);
  ShadowStepLength:=TPasJSON.GetNumber(JSONRootObject.Properties['shadowsteplength'],ShadowStepLength);
  HorizonBlendAmount:=TPasJSON.GetNumber(JSONRootObject.Properties['horizonblendamount'],HorizonBlendAmount);
  HorizonBlendPower:=TPasJSON.GetNumber(JSONRootObject.Properties['horizonblendpower'],HorizonBlendPower);

  CloudStartHeight:=TPasJSON.GetNumber(JSONRootObject.Properties['cloudstartheight'],CloudStartHeight);
  CloudThickness:=TPasJSON.GetNumber(JSONRootObject.Properties['cloudthickness'],CloudThickness);

  JSON:=JSONRootObject.Properties['layers'];
  if assigned(JSON) and (JSON is TPasJSONItemArray) then begin
   for Index:=0 to Min(TPasJSONItemArray(JSON).Count-1,Length(Layers)-1) do begin
    Layers[Index].LoadFromJSON(TPasJSONItemArray(JSON).Items[Index]);
   end;
  end;

  AnimationMultiplier:=TPasJSON.GetNumber(JSONRootObject.Properties['animationmultiplier'],AnimationMultiplier);

  MaxStepCount:=TPasJSON.GetInt64(JSONRootObject.Properties['maxstepcount'],MaxStepCount);
  MaxMarchingDistance:=TPasJSON.GetNumber(JSONRootObject.Properties['maxmarchingdistance'],MaxMarchingDistance);
  InverseDistanceStepCount:=TPasJSON.GetNumber(JSONRootObject.Properties['inversedistancestepcount'],InverseDistanceStepCount);
  RenderDistance:=TPasJSON.GetNumber(JSONRootObject.Properties['renderdistance'],RenderDistance);
  LODDistance:=TPasJSON.GetNumber(JSONRootObject.Properties['loddistance'],LODDistance);
  LODMin:=TPasJSON.GetNumber(JSONRootObject.Properties['lodmin'],LODMin);
  BigStepMarch:=TPasJSON.GetNumber(JSONRootObject.Properties['bigstepmarch'],BigStepMarch);
  TransmittanceThreshold:=TPasJSON.GetNumber(JSONRootObject.Properties['transmittancethreshold'],TransmittanceThreshold);
  ShadowSampleCount:=TPasJSON.GetNumber(JSONRootObject.Properties['shadowsamplecount'],ShadowSampleCount);
  GroundContributionSampleCount:=TPasJSON.GetNumber(JSONRootObject.Properties['groundcontributionsamplecount'],GroundContributionSampleCount);

 end;

end;

procedure TpvScene3DAtmosphere.TVolumetricCloudParametersOld.LoadFromJSONStream(const aStream:TStream);
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

procedure TpvScene3DAtmosphere.TVolumetricCloudParametersOld.LoadFromJSONFile(const aFileName:string);
var Stream:TMemoryStream;
begin
 Stream:=TMemoryStream.Create;
 try
  Stream.LoadFromFile(aFileName);
  Stream.Seek(0,soBeginning);
  LoadFromJSONStream(Stream);
 finally
  Stream.Free;
 end;
end;

function TpvScene3DAtmosphere.TVolumetricCloudParametersOld.SaveToJSON:TPasJSONItemObject;
var JSONArray:TPasJSONItemArray;
    Index:TpvInt32;
begin
 result:=TPasJSONItemObject.Create;
 result.Add('beerpowder',TPasJSONItemNumber.Create(BeerPowder));
 result.Add('beerpowderpower',TPasJSONItemNumber.Create(BeerPowderPower));
 result.Add('ambientgroundmultiplier',TPasJSONItemNumber.Create(AmbientGroundMultiplier));
 result.Add('phaseg',TPasJSONItemNumber.Create(PhaseG));
 result.Add('phaseg2',TPasJSONItemNumber.Create(PhaseG2));
 result.Add('phaseblend',TPasJSONItemNumber.Create(PhaseBlend));
 result.Add('multiscatteringscattering',TPasJSONItemNumber.Create(MultiScatteringScattering));
 result.Add('multiscatteringextinction',TPasJSONItemNumber.Create(MultiScatteringExtinction));
 result.Add('multiscatteringeccentricity',TPasJSONItemNumber.Create(MultiScatteringEccentricity));
 result.Add('shadowsteplength',TPasJSONItemNumber.Create(ShadowStepLength));
 result.Add('horizonblendamount',TPasJSONItemNumber.Create(HorizonBlendAmount));
 result.Add('horizonblendpower',TPasJSONItemNumber.Create(HorizonBlendPower));
 result.Add('cloudstartheight',TPasJSONItemNumber.Create(CloudStartHeight));
 result.Add('cloudthickness',TPasJSONItemNumber.Create(CloudThickness));
 JSONArray:=TPasJSONItemArray.Create;
 try
  for Index:=0 to Length(Layers)-1 do begin
   JSONArray.Add(Layers[Index].SaveToJSON);
  end;
 finally
  result.Add('layers',JSONArray);
 end; 
 result.Add('animationmultiplier',TPasJSONItemNumber.Create(AnimationMultiplier));
 result.Add('maxstepcount',TPasJSONItemNumber.Create(MaxStepCount));
 result.Add('maxmarchingdistance',TPasJSONItemNumber.Create(MaxMarchingDistance));
 result.Add('inversedistancestepcount',TPasJSONItemNumber.Create(InverseDistanceStepCount));
 result.Add('renderdistance',TPasJSONItemNumber.Create(RenderDistance));
 result.Add('loddistance',TPasJSONItemNumber.Create(LODDistance));
 result.Add('lodmin',TPasJSONItemNumber.Create(LODMin));
 result.Add('bigstepmarch',TPasJSONItemNumber.Create(BigStepMarch));
 result.Add('transmittancethreshold',TPasJSONItemNumber.Create(TransmittanceThreshold));
 result.Add('shadowsamplecount',TPasJSONItemNumber.Create(ShadowSampleCount));
 result.Add('groundcontributionsamplecount',TPasJSONItemNumber.Create(GroundContributionSampleCount));
end;

procedure TpvScene3DAtmosphere.TVolumetricCloudParametersOld.SaveToJSONStream(const aStream:TStream);
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

procedure TpvScene3DAtmosphere.TVolumetricCloudParametersOld.SaveToJSONFile(const aFileName:string);
var Stream:TMemoryStream;
begin
 Stream:=TMemoryStream.Create;
 try
  SaveToJSONStream(Stream);
  Stream.Seek(0,soBeginning);
  Stream.SaveToFile(aFileName);
 finally
  Stream.Free;
 end;
end;

{ TpvScene3DAtmosphere.TVolumetricCloudLayerLow }

procedure TpvScene3DAtmosphere.TVolumetricCloudLayerLow.Initialize;
begin
 Orientation:=TpvQuaternion.Identity;
 StartHeight:=6380.0;
 EndHeight:=6400.0;
 PositionScale:=0.0005;
 ShapeNoiseScale:=1.0;
 DetailNoiseScale:=1.0;
 CurlScale:=1.0;
 AdvanceCurlScale:=0.25;
 AdvanceCurlAmplitude:=0.25;
 HeightGradients[0]:=TpvVector4.InlineableCreate(0.0200,0.0500,0.0900,0.1100);  // Stratus
 HeightGradients[1]:=TpvVector4.InlineableCreate(0.0199,0.2000,0.4800,0.6250);  // Cumulus
 HeightGradients[2]:=TpvVector4.InlineableCreate(0.0100,0.0625,0.7500,1.0000); // Cumulonimbus
 AnvilDeformations[0]:=TpvVector4.InlineableCreate(0.0,1.0,1.0,1.0);
 AnvilDeformations[1]:=TpvVector4.InlineableCreate(0.0,1.0,1.0,1.0);
 AnvilDeformations[2]:=TpvVector4.InlineableCreate(0.0,1.0,1.0,1.0);
end;

procedure TpvScene3DAtmosphere.TVolumetricCloudLayerLow.LoadFromJSON(const aJSON:TPasJSONItem);
var JSONRootObject:TPasJSONItemObject;
    JSONArray:TPasJSONItemArray;
    JSONItem:TPasJSONItem;
    Index:TpvSizeInt;
begin

 if assigned(aJSON) and (aJSON is TPasJSONItemObject) then begin
  
  JSONRootObject:=TPasJSONItemObject(aJSON);
  
  Orientation.Vector:=JSONToVector4(JSONRootObject.Properties['orientation'],Orientation.Vector);
  StartHeight:=TPasJSON.GetNumber(JSONRootObject.Properties['startheight'],StartHeight);
  EndHeight:=TPasJSON.GetNumber(JSONRootObject.Properties['endheight'],EndHeight);
  PositionScale:=TPasJSON.GetNumber(JSONRootObject.Properties['positionscale'],PositionScale);
  ShapeNoiseScale:=TPasJSON.GetNumber(JSONRootObject.Properties['shapenoisescale'],ShapeNoiseScale);
  DetailNoiseScale:=TPasJSON.GetNumber(JSONRootObject.Properties['detailnoisescale'],DetailNoiseScale);
  CurlScale:=TPasJSON.GetNumber(JSONRootObject.Properties['curlscale'],CurlScale);
  AdvanceCurlScale:=TPasJSON.GetNumber(JSONRootObject.Properties['advancecurlscale'],AdvanceCurlScale);
  AdvanceCurlAmplitude:=TPasJSON.GetNumber(JSONRootObject.Properties['advancecurlamplitude'],AdvanceCurlAmplitude);
  
  JSONItem:=JSONRootObject.Properties['heightgradients'];
  if assigned(JSONItem) and (JSONItem is TPasJSONItemArray) then begin
   JSONArray:=TPasJSONItemArray(JSONItem);
   for Index:=0 to Min(JSONArray.Count,3)-1 do begin
    HeightGradients[Index]:=JSONToVector4(JSONArray.Items[Index],HeightGradients[Index]);
   end;
  end;

  JSONItem:=JSONRootObject.Properties['anvildeformations'];
  if assigned(JSONItem) and (JSONItem is TPasJSONItemArray) then begin
   JSONArray:=TPasJSONItemArray(JSONItem);
   for Index:=0 to Min(JSONArray.Count,3)-1 do begin
    AnvilDeformations[Index]:=JSONToVector4(JSONArray.Items[Index],AnvilDeformations[Index]);
   end;
  end;
  
 end;
 
end;

procedure TpvScene3DAtmosphere.TVolumetricCloudLayerLow.LoadFromJSONStream(const aStream:TStream);
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

procedure TpvScene3DAtmosphere.TVolumetricCloudLayerLow.LoadFromJSONFile(const aFileName:string);
var Stream:TMemoryStream;
begin
 Stream:=TMemoryStream.Create;
 try
  Stream.LoadFromFile(aFileName);
  Stream.Seek(0,soBeginning);
  LoadFromJSONStream(Stream);
 finally
  Stream.Free;
 end;
end;

function TpvScene3DAtmosphere.TVolumetricCloudLayerLow.SaveToJSON:TPasJSONItemObject;
var JSONArray:TPasJSONItemArray;
    Index:TpvInt32;
begin

 result:=TPasJSONItemObject.Create;
 result.Add('orientation',Vector4ToJSON(Orientation.Vector));
 result.Add('startheight',TPasJSONItemNumber.Create(StartHeight));
 result.Add('endheight',TPasJSONItemNumber.Create(EndHeight));
 result.Add('positionscale',TPasJSONItemNumber.Create(PositionScale));
 result.Add('shapenoisescale',TPasJSONItemNumber.Create(ShapeNoiseScale));
 result.Add('detailnoisescale',TPasJSONItemNumber.Create(DetailNoiseScale));
 result.Add('curlscale',TPasJSONItemNumber.Create(CurlScale));
 result.Add('advancecurlscale',TPasJSONItemNumber.Create(AdvanceCurlScale));
 result.Add('advancecurlamplitude',TPasJSONItemNumber.Create(AdvanceCurlAmplitude));

 JSONArray:=TPasJSONItemArray.Create;
 try
  for Index:=0 to 2 do begin
   JSONArray.Add(Vector4ToJSON(HeightGradients[Index]));
  end;
 finally
  result.Add('heightgradients',JSONArray);
 end;

 JSONArray:=TPasJSONItemArray.Create;
 try
  for Index:=0 to 2 do begin
   JSONArray.Add(Vector4ToJSON(AnvilDeformations[Index]));
  end;
 finally
  result.Add('anvildeformations',JSONArray);
 end;

end;

procedure TpvScene3DAtmosphere.TVolumetricCloudLayerLow.SaveToJSONStream(const aStream:TStream);
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

procedure TpvScene3DAtmosphere.TVolumetricCloudLayerLow.SaveToJSONFile(const aFileName:string);
var Stream:TMemoryStream;
begin
 Stream:=TMemoryStream.Create;
 try
  SaveToJSONStream(Stream);
  Stream.Seek(0,soBeginning);
  Stream.SaveToFile(aFileName);
 finally
  Stream.Free;
 end;
end;

{ TpvScene3DAtmosphere.TVolumetricCloudLayerHigh }

procedure TpvScene3DAtmosphere.TVolumetricCloudLayerHigh.Initialize;
begin
 Orientation:=TpvQuaternion.Identity;
 StartHeight:=6420.0;
 EndHeight:=6440.0;
 PositionScale:=0.1;
 Density:=0.015625;
 CoverMin:=0.5;
 CoverMax:=0.7;
 FadeMin:=0.01;
 FadeMax:=0.01;
 Speed:=0.0;
 RotationBase:=TpvVector4.InlineableCreate(-1.0,0.0,1.0,1.0);
 RotationOctave1:=TpvVector4.InlineableCreate(-1.0,0.0,1.0,0.125);
 RotationOctave2:=TpvVector4.InlineableCreate(-1.0,0.0,1.0,-0.125);
 RotationOctave3:=TpvVector4.InlineableCreate(-1.0,0.0,1.0,0.0625);
 OctaveScales:=TpvVector4.InlineableCreate(1.0,2.0,7.0,16.0);
 OctaveFactors:=TpvVector4.InlineableCreate(0.5,0.25,0.125,0.0625);
end;

procedure TpvScene3DAtmosphere.TVolumetricCloudLayerHigh.LoadFromJSON(const aJSON:TPasJSONItem);
var JSONRootObject:TPasJSONItemObject;
begin
 if assigned(aJSON) and (aJSON is TPasJSONItemObject) then begin
  JSONRootObject:=TPasJSONItemObject(aJSON);
  Orientation.Vector:=JSONToVector4(JSONRootObject.Properties['orientation'],Orientation.Vector);
  StartHeight:=TPasJSON.GetNumber(JSONRootObject.Properties['startheight'],StartHeight);
  EndHeight:=TPasJSON.GetNumber(JSONRootObject.Properties['endheight'],EndHeight);
  PositionScale:=TPasJSON.GetNumber(JSONRootObject.Properties['positionscale'],PositionScale);
  Density:=TPasJSON.GetNumber(JSONRootObject.Properties['density'],Density);
  CoverMin:=TPasJSON.GetNumber(JSONRootObject.Properties['covermin'],CoverMin);
  CoverMax:=TPasJSON.GetNumber(JSONRootObject.Properties['covermax'],CoverMax);
  FadeMin:=TPasJSON.GetNumber(JSONRootObject.Properties['fademin'],FadeMin);
  FadeMax:=TPasJSON.GetNumber(JSONRootObject.Properties['fademax'],FadeMax);
  Speed:=TPasJSON.GetNumber(JSONRootObject.Properties['speed'],Speed);
  RotationBase:=JSONToVector4(JSONRootObject.Properties['rotationbase'],RotationBase);
  RotationOctave1:=JSONToVector4(JSONRootObject.Properties['rotationoctave1'],RotationOctave1);
  RotationOctave2:=JSONToVector4(JSONRootObject.Properties['rotationoctave2'],RotationOctave2);
  RotationOctave3:=JSONToVector4(JSONRootObject.Properties['rotationoctave3'],RotationOctave3);
  OctaveScales:=JSONToVector4(JSONRootObject.Properties['octavescales'],OctaveScales);
  OctaveFactors:=JSONToVector4(JSONRootObject.Properties['octavefactors'],OctaveFactors);
 end;
end;

procedure TpvScene3DAtmosphere.TVolumetricCloudLayerHigh.LoadFromJSONStream(const aStream:TStream);
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

procedure TpvScene3DAtmosphere.TVolumetricCloudLayerHigh.LoadFromJSONFile(const aFileName:string);
var Stream:TMemoryStream;
begin
 Stream:=TMemoryStream.Create;
 try
  Stream.LoadFromFile(aFileName);
  Stream.Seek(0,soBeginning);
  LoadFromJSONStream(Stream);
 finally
  Stream.Free;
 end;
end;

function TpvScene3DAtmosphere.TVolumetricCloudLayerHigh.SaveToJSON:TPasJSONItemObject;
begin
 result:=TPasJSONItemObject.Create;
 result.Add('orientation',Vector4ToJSON(Orientation.Vector));
 result.Add('startheight',TPasJSONItemNumber.Create(StartHeight));
 result.Add('endheight',TPasJSONItemNumber.Create(EndHeight));
 result.Add('positionscale',TPasJSONItemNumber.Create(PositionScale));
 result.Add('density',TPasJSONItemNumber.Create(Density));
 result.Add('covermin',TPasJSONItemNumber.Create(CoverMin));
 result.Add('covermax',TPasJSONItemNumber.Create(CoverMax));
 result.Add('fademin',TPasJSONItemNumber.Create(FadeMin));
 result.Add('fademax',TPasJSONItemNumber.Create(FadeMax));
 result.Add('speed',TPasJSONItemNumber.Create(Speed));
 result.Add('rotationbase',Vector4ToJSON(RotationBase));
 result.Add('rotationoctave1',Vector4ToJSON(RotationOctave1));
 result.Add('rotationoctave2',Vector4ToJSON(RotationOctave2));
 result.Add('rotationoctave3',Vector4ToJSON(RotationOctave3));
 result.Add('octavescales',Vector4ToJSON(OctaveScales));
 result.Add('octavefactors',Vector4ToJSON(OctaveFactors));
end;

procedure TpvScene3DAtmosphere.TVolumetricCloudLayerHigh.SaveToJSONStream(const aStream:TStream);
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

procedure TpvScene3DAtmosphere.TVolumetricCloudLayerHigh.SaveToJSONFile(const aFileName:string);
var Stream:TMemoryStream;
begin
 Stream:=TMemoryStream.Create;
 try
  SaveToJSONStream(Stream);
  Stream.Seek(0,soBeginning);
  Stream.SaveToFile(aFileName);
 finally
  Stream.Free;
 end;
end;

{ TpvScene3DAtmosphere.TVolumetricCloudParameters }

procedure TpvScene3DAtmosphere.TVolumetricCloudParameters.Initialize;
begin
 CoverageTypeWetnessTopFactors:=TpvVector4.InlineableCreate(1.0,1.0,1.0,1.0);
 CoverageTypeWetnessTopOffsets:=TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0);
 Scattering:=TpvVector4.InlineableCreate(1.0,1.0,1.0,0.0);
 Absorption:=TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0);
 LightingDensity:=1.0;
 ShadowDensity:=1.0;
 ViewDensity:=1.0;
 DensityScale:=1.0;
 Scale:=1.0;
 ForwardScatteringG:=0.5;
 BackwardScatteringG:=-0.8;
 ShadowRayLength:=1.0;
 DensityAlongConeLength:=1.0;
 DensityAlongConeLengthFarMultiplier:=3.0;
 RayMinSteps:=64;
 RayMaxSteps:=128;
 OuterSpaceRayMinSteps:=64;
 OuterSpaceRayMaxSteps:=256;
 DirectScatteringIntensity:=1.0;
 IndirectScatteringIntensity:=1.0;
 AmbientLightIntensity:=1.0;
 LayerLow.Initialize;
 LayerHigh.Initialize;
end;

procedure TpvScene3DAtmosphere.TVolumetricCloudParameters.LoadFromJSON(const aJSON:TPasJSONItem);
var JSONRootObject:TPasJSONItemObject;
begin
 if assigned(aJSON) and (aJSON is TPasJSONItemObject) then begin
  JSONRootObject:=TPasJSONItemObject(aJSON);
  CoverageTypeWetnessTopFactors:=JSONToVector4(JSONRootObject.Properties['coveragetypewetnesstopfactors'],CoverageTypeWetnessTopFactors);
  CoverageTypeWetnessTopOffsets:=JSONToVector4(JSONRootObject.Properties['coveragetypewetnesstopoffsets'],CoverageTypeWetnessTopOffsets);
  Scattering.xyz:=JSONToVector3(JSONRootObject.Properties['scattering'],Scattering.xyz);
  Absorption.xyz:=JSONToVector3(JSONRootObject.Properties['absorption'],Absorption.xyz);
  LightingDensity:=TPasJSON.GetNumber(JSONRootObject.Properties['lightingdensity'],LightingDensity);
  ShadowDensity:=TPasJSON.GetNumber(JSONRootObject.Properties['shadowdensity'],ShadowDensity);
  ViewDensity:=TPasJSON.GetNumber(JSONRootObject.Properties['viewdensity'],ViewDensity);
  DensityScale:=TPasJSON.GetNumber(JSONRootObject.Properties['densityscale'],DensityScale);
  Scale:=TPasJSON.GetNumber(JSONRootObject.Properties['scale'],Scale);
  ForwardScatteringG:=TPasJSON.GetNumber(JSONRootObject.Properties['forwardscatteringg'],ForwardScatteringG);
  BackwardScatteringG:=TPasJSON.GetNumber(JSONRootObject.Properties['backwardscatteringg'],BackwardScatteringG);
  ShadowRayLength:=TPasJSON.GetNumber(JSONRootObject.Properties['shadowraylength'],ShadowRayLength);
  DensityAlongConeLength:=TPasJSON.GetNumber(JSONRootObject.Properties['densityalongconelength'],DensityAlongConeLength);
  DensityAlongConeLengthFarMultiplier:=TPasJSON.GetNumber(JSONRootObject.Properties['densityalongconelengthfarmultiplier'],DensityAlongConeLengthFarMultiplier);
  RayMinSteps:=TPasJSON.GetInt64(JSONRootObject.Properties['rayminsteps'],RayMinSteps);
  RayMaxSteps:=TPasJSON.GetInt64(JSONRootObject.Properties['raymaxsteps'],RayMaxSteps);
  OuterSpaceRayMinSteps:=TPasJSON.GetInt64(JSONRootObject.Properties['outerspacerayminsteps'],OuterSpaceRayMinSteps);
  OuterSpaceRayMaxSteps:=TPasJSON.GetInt64(JSONRootObject.Properties['outerspaceraymaxsteps'],OuterSpaceRayMaxSteps);
  DirectScatteringIntensity:=TPasJSON.GetNumber(JSONRootObject.Properties['directscatteringintensity'],DirectScatteringIntensity);
  IndirectScatteringIntensity:=TPasJSON.GetNumber(JSONRootObject.Properties['indirectscatteringintensity'],IndirectScatteringIntensity);
  AmbientLightIntensity:=TPasJSON.GetNumber(JSONRootObject.Properties['ambientlightintensity'],AmbientLightIntensity);
  LayerLow.LoadFromJSON(JSONRootObject.Properties['layerlow']);
  LayerHigh.LoadFromJSON(JSONRootObject.Properties['layerhigh']);
 end;
end;

procedure TpvScene3DAtmosphere.TVolumetricCloudParameters.LoadFromJSONStream(const aStream:TStream);
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

procedure TpvScene3DAtmosphere.TVolumetricCloudParameters.LoadFromJSONFile(const aFileName:string);
var Stream:TMemoryStream;
begin
 Stream:=TMemoryStream.Create;
 try
  Stream.LoadFromFile(aFileName);
  Stream.Seek(0,soBeginning);
  LoadFromJSONStream(Stream);
 finally
  Stream.Free;
 end;
end;

function TpvScene3DAtmosphere.TVolumetricCloudParameters.SaveToJSON:TPasJSONItemObject;
begin
 result:=TPasJSONItemObject.Create;
 result.Add('coveragetypewetnesstopfactors',Vector4ToJSON(CoverageTypeWetnessTopFactors));
 result.Add('coveragetypewetnesstopoffsets',Vector4ToJSON(CoverageTypeWetnessTopOffsets));
 result.Add('scattering',Vector3ToJSON(Scattering.xyz));
 result.Add('absorption',Vector3ToJSON(Absorption.xyz));
 result.Add('lightingdensity',TPasJSONItemNumber.Create(LightingDensity));
 result.Add('shadowdensity',TPasJSONItemNumber.Create(ShadowDensity));
 result.Add('viewdensity',TPasJSONItemNumber.Create(ViewDensity));
 result.Add('densityscale',TPasJSONItemNumber.Create(DensityScale));
 result.Add('scale',TPasJSONItemNumber.Create(Scale));
 result.Add('forwardscatteringg',TPasJSONItemNumber.Create(ForwardScatteringG));
 result.Add('backwardscatteringg',TPasJSONItemNumber.Create(BackwardScatteringG));
 result.Add('shadowraylength',TPasJSONItemNumber.Create(ShadowRayLength));
 result.Add('densityalongconelength',TPasJSONItemNumber.Create(DensityAlongConeLength));
 result.Add('densityalongconelengthfarmultiplier',TPasJSONItemNumber.Create(DensityAlongConeLengthFarMultiplier));
 result.Add('rayminsteps',TPasJSONItemNumber.Create(RayMinSteps));
 result.Add('raymaxsteps',TPasJSONItemNumber.Create(RayMaxSteps));
 result.Add('outerspacerayminsteps',TPasJSONItemNumber.Create(OuterSpaceRayMinSteps));
 result.Add('outerspaceraymaxsteps',TPasJSONItemNumber.Create(OuterSpaceRayMaxSteps));
 result.Add('directscatteringintensity',TPasJSONItemNumber.Create(DirectScatteringIntensity));
 result.Add('indirectscatteringintensity',TPasJSONItemNumber.Create(IndirectScatteringIntensity));
 result.Add('ambientlightintensity',TPasJSONItemNumber.Create(AmbientLightIntensity));
 result.Add('layerlow',LayerLow.SaveToJSON);
 result.Add('layerhigh',LayerHigh.SaveToJSON);
end;

procedure TpvScene3DAtmosphere.TVolumetricCloudParameters.SaveToJSONStream(const aStream:TStream);
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

procedure TpvScene3DAtmosphere.TVolumetricCloudParameters.SaveToJSONFile(const aFileName:string);
var Stream:TMemoryStream;
begin
 Stream:=TMemoryStream.Create;
 try
  SaveToJSONStream(Stream);
  Stream.Seek(0,soBeginning);
  Stream.SaveToFile(aFileName);
 finally
  Stream.Free;
 end;
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

 // Volumetric clouds
 VolumetricClouds.Initialize;

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

  VolumetricClouds.LoadFromJSON(JSONRootObject.Properties['volumetricclouds']);

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
 result.Add('volumetricclouds',VolumetricClouds.SaveToJSON);
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

{ TpvScene3DAtmosphere.TGPUVolumetricCloudLayerLow }

procedure TpvScene3DAtmosphere.TGPUVolumetricCloudLayerLow.Assign(const aVolumetricCloudLayerLow:TVolumetricCloudLayerLow);
begin

 Orientation:=aVolumetricCloudLayerLow.Orientation;
 
 StartHeight:=aVolumetricCloudLayerLow.StartHeight;
 EndHeight:=aVolumetricCloudLayerLow.EndHeight;

 PositionScale:=aVolumetricCloudLayerLow.PositionScale;

 ShapeNoiseScale:=aVolumetricCloudLayerLow.ShapeNoiseScale;
 DetailNoiseScale:=aVolumetricCloudLayerLow.DetailNoiseScale;
 CurlScale:=aVolumetricCloudLayerLow.CurlScale;

 AdvanceCurlScale:=aVolumetricCloudLayerLow.AdvanceCurlScale;
 AdvanceCurlAmplitude:=aVolumetricCloudLayerLow.AdvanceCurlAmplitude;

 HeightGradients[0]:=aVolumetricCloudLayerLow.HeightGradients[0];
 HeightGradients[1]:=aVolumetricCloudLayerLow.HeightGradients[1];
 HeightGradients[2]:=aVolumetricCloudLayerLow.HeightGradients[2];

 AnvilDeformations[0]:=aVolumetricCloudLayerLow.AnvilDeformations[0];
 AnvilDeformations[1]:=aVolumetricCloudLayerLow.AnvilDeformations[1]; 
 AnvilDeformations[2]:=aVolumetricCloudLayerLow.AnvilDeformations[2];

end;

{ TpvScene3DAtmosphere.TGPUVolumetricCloudLayerHigh }

procedure TpvScene3DAtmosphere.TGPUVolumetricCloudLayerHigh.Assign(const aVolumetricCloudLayerHigh:TVolumetricCloudLayerHigh);
begin
 
 Orientation:=aVolumetricCloudLayerHigh.Orientation;

 StartHeight:=aVolumetricCloudLayerHigh.StartHeight;
 EndHeight:=aVolumetricCloudLayerHigh.EndHeight;
 
 PositionScale:=aVolumetricCloudLayerHigh.PositionScale;
 
 Density:=aVolumetricCloudLayerHigh.Density;
 
 CoverMin:=aVolumetricCloudLayerHigh.CoverMin;
 CoverMax:=aVolumetricCloudLayerHigh.CoverMax;
 
 FadeMin:=aVolumetricCloudLayerHigh.FadeMin;
 FadeMax:=aVolumetricCloudLayerHigh.FadeMax;
 
 Speed:=aVolumetricCloudLayerHigh.Speed;
 
 RotationBase:=aVolumetricCloudLayerHigh.RotationBase;
 RotationOctave1:=aVolumetricCloudLayerHigh.RotationOctave1;
 RotationOctave2:=aVolumetricCloudLayerHigh.RotationOctave2;
 RotationOctave3:=aVolumetricCloudLayerHigh.RotationOctave3;
 
 OctaveScales:=aVolumetricCloudLayerHigh.OctaveScales;
 OctaveFactors:=aVolumetricCloudLayerHigh.OctaveFactors;

end;

{ TpvScene3DAtmosphere.TGPUVolumetricCloudParameters }

procedure TpvScene3DAtmosphere.TGPUVolumetricCloudParameters.Assign(const aVolumetricCloudParameters:TVolumetricCloudParameters);
begin
 
 CoverageTypeWetnessTopFactors:=aVolumetricCloudParameters.CoverageTypeWetnessTopFactors; 
 CoverageTypeWetnessTopOffsets:=aVolumetricCloudParameters.CoverageTypeWetnessTopOffsets;
 
 Scattering:=aVolumetricCloudParameters.Scattering; 
 Absorption:=aVolumetricCloudParameters.Absorption;
 
 LightingDensity:=aVolumetricCloudParameters.LightingDensity; 
 ShadowDensity:=aVolumetricCloudParameters.ShadowDensity;
 ViewDensity:=aVolumetricCloudParameters.ViewDensity;
 
 DensityScale:=aVolumetricCloudParameters.DensityScale;

 Scale:=aVolumetricCloudParameters.Scale;

 ForwardScatteringG:=aVolumetricCloudParameters.ForwardScatteringG;
 BackwardScatteringG:=aVolumetricCloudParameters.BackwardScatteringG;
 
 ShadowRayLength:=aVolumetricCloudParameters.ShadowRayLength;
 
 DensityAlongConeLength:=aVolumetricCloudParameters.DensityAlongConeLength;
 DensityAlongConeLengthFarMultiplier:=aVolumetricCloudParameters.DensityAlongConeLengthFarMultiplier;

 RayMinSteps:=aVolumetricCloudParameters.RayMinSteps;
 RayMaxSteps:=aVolumetricCloudParameters.RayMaxSteps;

 OuterSpaceRayMinSteps:=aVolumetricCloudParameters.OuterSpaceRayMinSteps;
 OuterSpaceRayMaxSteps:=aVolumetricCloudParameters.OuterSpaceRayMaxSteps;

 DirectScatteringIntensity:=aVolumetricCloudParameters.DirectScatteringIntensity;
 IndirectScatteringIntensity:=aVolumetricCloudParameters.IndirectScatteringIntensity;
 AmbientLightIntensity:=aVolumetricCloudParameters.AmbientLightIntensity;

 LayerLow.Assign(aVolumetricCloudParameters.LayerLow); 
 LayerHigh.Assign(aVolumetricCloudParameters.LayerHigh);

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

 VolumetricClouds.Assign(aAtmosphereParameters.VolumetricClouds);

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

 fMultiScatteringTexture:=TpvScene3DRendererImage2D.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                           MultiScatteringLUTRes,
                                                           MultiScatteringLUTRes,
                                                           VK_FORMAT_R32G32B32A32_SFLOAT,
                                                           true,
                                                           VK_SAMPLE_COUNT_1_BIT,
                                                           VK_IMAGE_LAYOUT_GENERAL,
                                                           VK_SHARING_MODE_EXCLUSIVE,
                                                           [],
                                                           0);
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fMultiScatteringTexture.VulkanImage.Handle,VK_OBJECT_TYPE_IMAGE,'MultiScatteringTexture');
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fMultiScatteringTexture.VulkanImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'MultiScatteringTexture');

 fSkyLuminanceLUTTexture:=TpvScene3DRendererImageCubeMap.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                                SkyLuminanceLUTRes,
                                                                SkyLuminanceLUTRes,
                                                                VK_FORMAT_R32G32B32A32_SFLOAT,
                                                                true,
                                                                VK_SAMPLE_COUNT_1_BIT,
                                                                VK_IMAGE_LAYOUT_GENERAL,
                                                                VK_SHARING_MODE_EXCLUSIVE,
                                                                [],
                                                                0,
                                                                'SkyLuminanceLUTTexture');

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
                                                              0,
                                                              'AtmosphereCubeMap');

 fGGXCubeMapTexture:=TpvScene3DRendererMipmapImageCubeMap.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                                 CubeMapTextureSize,
                                                                 CubeMapTextureSize,
                                                                 VK_FORMAT_R16G16B16A16_SFLOAT,
                                                                 true,                                                              
                                                                 VK_SAMPLE_COUNT_1_BIT,                                                              
                                                                 VK_IMAGE_LAYOUT_GENERAL,
                                                                 TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                 nil,
                                                                 0,
                                                                 'GGXCubeMap');

 fCharlieCubeMapTexture:=TpvScene3DRendererMipmapImageCubeMap.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                                     CubeMapTextureSize,
                                                                     CubeMapTextureSize,
                                                                     VK_FORMAT_R16G16B16A16_SFLOAT,
                                                                     true,                                                              
                                                                     VK_SAMPLE_COUNT_1_BIT,                                                              
                                                                     VK_IMAGE_LAYOUT_GENERAL,
                                                                     TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                     nil,
                                                                     0,
                                                                     'CharlieCubeMap');

 fLambertianCubeMapTexture:=TpvScene3DRendererMipmapImageCubeMap.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                                        CubeMapTextureSize,
                                                                        CubeMapTextureSize,
                                                                        VK_FORMAT_R16G16B16A16_SFLOAT,
                                                                        true,                                                              
                                                                        VK_SAMPLE_COUNT_1_BIT,                                                              
                                                                        VK_IMAGE_LAYOUT_GENERAL,
                                                                        TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                        nil,
                                                                        0,
                                                                        'LambertianCubeMap');

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
                                                                                                                fMultiScatteringTexture.VulkanImageView.Handle,
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
                                                                                 [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.RepeatedSampler.Handle,
                                                                                                                TpvScene3D(fAtmosphere.fScene3D).BlueNoise2DTexture.ImageView.Handle,
                                                                                                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                                 [],
                                                                                 [],
                                                                                 false);                                                                                                                                          

  fMultiScatteringLUTPassDescriptorSets[InFlightFrameIndex].Flush;

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fMultiScatteringLUTPassDescriptorSets[InFlightFrameIndex].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'MultiScatteringLUTPassDescriptorSets['+IntToStr(InFlightFrameIndex)+']');

 end;

 fSkyLuminanceLUTPassDescriptorPool:=TpvVulkanDescriptorPool.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                                 TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT) or
                                                                 TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT),
                                                                 TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fSkyLuminanceLUTPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fSkyLuminanceLUTPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*3);                                                                
 fSkyLuminanceLUTPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fSkyLuminanceLUTPassDescriptorPool.Initialize;

 for InFlightFrameIndex:=0 to TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames-1 do begin

  fSkyLuminanceLUTPassDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fSkyLuminanceLUTPassDescriptorPool,
                                                                                         TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).AtmosphereGlobals).fSkyLuminanceLUTPassDescriptorSetLayout);

  fSkyLuminanceLUTPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                              0,
                                                                              1,
                                                                              TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                              [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                             fSkyLuminanceLUTTexture.VulkanImageView.Handle,
                                                                                                             VK_IMAGE_LAYOUT_GENERAL)],
                                                                              [],
                                                                              [],
                                                                              false);

  fSkyLuminanceLUTPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                              0,
                                                                              1,
                                                                              TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                              [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                             fTransmittanceTexture.VulkanImageView.Handle,
                                                                                                             VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                              [],
                                                                              [],
                                                                              false);

  fSkyLuminanceLUTPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                              0,
                                                                              1,
                                                                              TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                              [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                             fMultiScatteringTexture.VulkanImageView.Handle,
                                                                                                             VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                              [],
                                                                              [],
                                                                              false);

  fSkyLuminanceLUTPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                              0,
                                                                              1,
                                                                              TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                              [],
                                                                              [fAtmosphere.fAtmosphereParametersBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                              [],
                                                                              false);                                                             

  fSkyLuminanceLUTPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                                              0,
                                                                              1,
                                                                              TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                              [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.RepeatedSampler.Handle,
                                                                                                             TpvScene3D(fAtmosphere.fScene3D).BlueNoise2DTexture.ImageView.Handle,
                                                                                                             VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                              [],
                                                                              [],
                                                                              false);

  fSkyLuminanceLUTPassDescriptorSets[InFlightFrameIndex].Flush;

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fSkyLuminanceLUTPassDescriptorSets[InFlightFrameIndex].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'SkyLuminanceLUTPassDescriptorSets['+IntToStr(InFlightFrameIndex)+']');

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

  fSkyViewLUTPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                                         0,
                                                                         1,
                                                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                         [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.RepeatedSampler.Handle,
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

  fCameraVolumePassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                                           0,
                                                                           1,
                                                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                           [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.RepeatedSampler.Handle,
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
 fCubeMapPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*3);
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
                                                                                                     fMultiScatteringTexture.VulkanImageView.Handle,
                                                                                                     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                      [],
                                                                      [],
                                                                      false);

  fCubeMapPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                      0,
                                                                      1,
                                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                      [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                     fSkyLuminanceLUTTexture.VulkanImageView.Handle,
                                                                                                     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                      [],
                                                                      [],
                                                                      false);

  fCubeMapPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
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

 fCloudRaymarchingPassDescriptorPool:=TpvVulkanDescriptorPool.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                                    TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT) or
                                                                    TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT),
                                                                    TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fCloudRaymarchingPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fCloudRaymarchingPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*9);                                                                    
 fCloudRaymarchingPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fCloudRaymarchingPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*2);
 fCloudRaymarchingPassDescriptorPool.Initialize;
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCloudRaymarchingPassDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'CloudRaymarchingPassDescriptorPool');

 for InFlightFrameIndex:=0 to TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames-1 do begin

  fCloudRaymarchingPassDepthImageViews[InFlightFrameIndex]:=VK_NULL_HANDLE;

  fCloudRaymarchingPassCascadedShadowMapImageViews[InFlightFrameIndex]:=VK_NULL_HANDLE;
  
  fCloudRaymarchingPassDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fCloudRaymarchingPassDescriptorPool,
                                                                                         TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).AtmosphereGlobals).fCloudRaymarchingPassDescriptorSetLayout);

  // Depth texture
 {fCloudRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                               0,
                                                                               1,
                                                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_SAMPLED_IMAGE),
                                                                               [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                              VK_NULL_HANDLE, // will be replaced with the actual depth texture attachment
                                                                                                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                               [],
                                                                               [],
                                                                               false);}

  // Atmosphere parameters
  fCloudRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                               0,
                                                                               1,
                                                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
//                                                                             TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                               [],
                                                                               [fAtmosphere.fAtmosphereParametersBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                               [],
                                                                               false);

  // Blue noise 
  fCloudRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                               0,
                                                                               1,
                                                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                               [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.RepeatedSampler.Handle,
                                                                                                              TpvScene3D(fAtmosphere.fScene3D).BlueNoise2DTexture.ImageView.Handle,
                                                                                                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                               [],
                                                                               [],
                                                                               false);

  // Sky luminance
  fCloudRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                               0,
                                                                               1,
                                                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                               [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                              fSkyLuminanceLUTTexture.VulkanImageView.Handle,
                                                                                                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                               [],
                                                                               [],
                                                                               false);

  // Transmittance LUT
  fCloudRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                                               0,
                                                                               1,
                                                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                               [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                              fTransmittanceTexture.VulkanImageView.Handle,
                                                                                                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                               [],
                                                                               [],
                                                                               false);

  // Shape noise
  fCloudRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(5,
                                                                               0,
                                                                               1,
                                                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                               [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.MirrorRepeatedSampler.Handle,
                                                                                                              TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).AtmosphereGlobals).fCloudShapeTexture.VulkanImageView.Handle,
                                                                                                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                               [],
                                                                               [],
                                                                               false);

  // Detail noise
  fCloudRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(6,
                                                                               0,
                                                                               1,
                                                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                               [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.MirrorRepeatedSampler.Handle,
                                                                                                              TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).AtmosphereGlobals).fCloudDetailTexture.VulkanImageView.Handle,
                                                                                                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                               [],
                                                                               [],
                                                                               false);

  // Curl noise
  fCloudRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(7,
                                                                               0,
                                                                               1,
                                                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                               [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.MirrorRepeatedSampler.Handle,
                                                                                                              TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).AtmosphereGlobals).fCloudCurlTexture.VulkanImageView.Handle,
                                                                                                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                               [],
                                                                               [],
                                                                               false);

  // Sky luminance LUT
  fCloudRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(8,
                                                                               0,
                                                                               1,
                                                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                               [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                              fSkyLuminanceLUTTexture.VulkanImageView.Handle,
                                                                                                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                               [],
                                                                               [],
                                                                               false);

  // Weather map
  fCloudRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(9,
                                                                               0,
                                                                               1,
                                                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                               [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.ClampedSampler.Handle,
                                                                                                              fAtmosphere.fWeatherMapTexture.VulkanImageView.Handle,
                                                                                                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                               [],
                                                                               [],
                                                                               false);

  //fCloudRaymarchingPassDescriptorSets[InFlightFrameIndex].Flush; // Not needed, because the descriptor set will be flushed when it is bound

  fCloudRaymarchingPassDescriptorSetFirsts[InFlightFrameIndex]:=true; // Will be set to false after the first flush, so that the further descriptor set updates will be updated directly instead of creating new descriptor sets

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCloudRaymarchingPassDescriptorSets[InFlightFrameIndex].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'CloudRaymarchingPassDescriptorSets['+IntToStr(InFlightFrameIndex)+']');

 end;

 fRaymarchingPassDescriptorPool:=TpvVulkanDescriptorPool.Create(TpvScene3D(fAtmosphere.fScene3D).VulkanDevice,
                                                               TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT) or
                                                               TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT),
                                                               TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fRaymarchingPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*4);
 fRaymarchingPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*6);
 fRaymarchingPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fRaymarchingPassDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames*1);
 fRaymarchingPassDescriptorPool.Initialize;
 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.SetObjectName(fRaymarchingPassDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'RaymarchingPassDescriptorPool');

 for InFlightFrameIndex:=0 to TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames-1 do begin

  fRaymarchingPassDepthImageViews[InFlightFrameIndex]:=VK_NULL_HANDLE;

  fRaymarchingPassCascadedShadowMapImageViews[InFlightFrameIndex]:=VK_NULL_HANDLE;

  fRaymarchingPassCloudsInscatteringImageViews[InFlightFrameIndex]:=VK_NULL_HANDLE;

  fRaymarchingPassCloudsTransmittanceImageViews[InFlightFrameIndex]:=VK_NULL_HANDLE;

  fRaymarchingPassCloudsDepthImageViews[InFlightFrameIndex]:=VK_NULL_HANDLE;

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
                                                                                                         fMultiScatteringTexture.VulkanImageView.Handle,
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
                                                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                          [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRendererInstance).Renderer.RepeatedSampler.Handle,
                                                                                                         TpvScene3D(fAtmosphere.fScene3D).BlueNoise2DTexture.ImageView.Handle,
                                                                                                         VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                          [],
                                                                          [],
                                                                          false);

  fRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(6,
                                                                          0,
                                                                          1,
                                                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                          [],
                                                                          [fAtmosphere.fAtmosphereParametersBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                          [],
                                                                          false);

  fRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(7,
                                                                          0,
                                                                          1,
                                                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                          [],
                                                                          [TpvScene3DRendererInstance(fRendererInstance).CascadedShadowMapVulkanUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                          [],
                                                                          false);

{ fRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(8,
                                                                         0,
                                                                          1,
                                                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                          [TpvScene3DRendererInstance(fRendererInstance).CascadedShadowMapVulkanUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                          [],
                                                                          [],
                                                                          false);}

 {fRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(9,
                                                                          0,
                                                                          1,
                                                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                                                                          [...],
                                                                          [],
                                                                          [],
                                                                          false);}

 {fRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(10,
                                                                          0,
                                                                          1,
                                                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                                                                          [...],
                                                                          [],
                                                                          [],
                                                                          false);}

 {fRaymarchingPassDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(11,
                                                                          0,
                                                                          1,
                                                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                                                                          [...],
                                                                          [],
                                                                          [],
                                                                          false);}

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

 fCubeMapMipMapGenerator:=TpvScene3DRendererCubeMapMipMapGenerator.Create(TpvScene3D(fAtmosphere.fScene3D),fCubeMapTexture);
 fCubeMapMipMapGenerator.AcquirePersistentResources;
 fCubeMapMipMapGenerator.AcquireVolatileResources;

 fGGXCubeMapIBLFilter:=TpvScene3DRendererCubeMapIBLFilter.Create(TpvScene3D(fAtmosphere.fScene3D),TpvScene3DRendererInstance(fRendererInstance).Renderer,fCubeMapTexture,fGGXCubeMapTexture,TpvScene3DRendererCubeMapIBLFilter.GGX);
 fGGXCubeMapIBLFilter.AcquirePersistentResources;
 fGGXCubeMapIBLFilter.AcquireVolatileResources;

 fCharlieCubeMapIBLFilter:=TpvScene3DRendererCubeMapIBLFilter.Create(TpvScene3D(fAtmosphere.fScene3D),TpvScene3DRendererInstance(fRendererInstance).Renderer,fCubeMapTexture,fCharlieCubeMapTexture,TpvScene3DRendererCubeMapIBLFilter.Charlie);
 fCharlieCubeMapIBLFilter.AcquirePersistentResources;
 fCharlieCubeMapIBLFilter.AcquireVolatileResources;

 fLambertianCubeMapIBLFilter:=TpvScene3DRendererCubeMapIBLFilter.Create(TpvScene3D(fAtmosphere.fScene3D),TpvScene3DRendererInstance(fRendererInstance).Renderer,fCubeMapTexture,fLambertianCubeMapTexture,TpvScene3DRendererCubeMapIBLFilter.Lambertian);
 fLambertianCubeMapIBLFilter.AcquirePersistentResources;
 fLambertianCubeMapIBLFilter.AcquireVolatileResources;
 
end;

destructor TpvScene3DAtmosphere.TRendererInstance.Destroy;
var InFlightFrameIndex:TpvSizeInt;
begin

 fLambertianCubeMapIBLFilter.ReleaseVolatileResources;
 fLambertianCubeMapIBLFilter.ReleasePersistentResources;
 FreeAndNil(fLambertianCubeMapIBLFilter);

 fCharlieCubeMapIBLFilter.ReleaseVolatileResources;
 fCharlieCubeMapIBLFilter.ReleasePersistentResources;
 FreeAndNil(fCharlieCubeMapIBLFilter);

 fGGXCubeMapIBLFilter.ReleaseVolatileResources;
 fGGXCubeMapIBLFilter.ReleasePersistentResources;
 FreeAndNil(fGGXCubeMapIBLFilter);

 fCubeMapMipMapGenerator.ReleaseVolatileResources;
 fCubeMapMipMapGenerator.ReleasePersistentResources;
 FreeAndNil(fCubeMapMipMapGenerator);

 for InFlightFrameIndex:=0 to TpvScene3D(fAtmosphere.fScene3D).CountInFlightFrames-1 do begin
  FreeAndNil(fTransmittanceLUTPassDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fMultiScatteringLUTPassDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fSkyLuminanceLUTPassDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fSkyViewLUTPassDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fCubeMapPassDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fCameraVolumePassDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fCloudRaymarchingPassDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fRaymarchingPassDescriptorSets[InFlightFrameIndex]);
  FreeAndNil(fGlobalDescriptorSets[InFlightFrameIndex]);
 end;

 FreeAndNil(fTransmittanceLUTPassDescriptorPool);
 FreeAndNil(fMultiScatteringLUTPassDescriptorPool);
 FreeAndNil(fSkyLuminanceLUTPassDescriptorPool);
 FreeAndNil(fSkyViewLUTPassDescriptorPool);
 FreeAndNil(fCubeMapPassDescriptorPool);
 FreeAndNil(fCameraVolumePassDescriptorPool);
 FreeAndNil(fCloudRaymarchingPassDescriptorPool);
 FreeAndNil(fRaymarchingPassDescriptorPool);
 FreeAndNil(fGlobalDescriptorPool);

 FreeAndNil(fCubeMapTexture);
 FreeAndNil(fGGXCubeMapTexture);
 FreeAndNil(fCharlieCubeMapTexture);
 FreeAndNil(fLambertianCubeMapTexture);

 FreeAndNil(fTransmittanceTexture);
 FreeAndNil(fMultiScatteringTexture);
 FreeAndNil(fSkyLuminanceLUTTexture);
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

procedure TpvScene3DAtmosphere.TRendererInstance.SetCloudsImageViews(const aInFlightFrameIndex:TpvSizeInt;
                                                                     const aDepthImageView:TVkImageView;
                                                                     const aCascadedShadowMapImageView:TVkImageView);
begin

 if (fCloudRaymarchingPassDepthImageViews[aInFlightFrameIndex]<>aDepthImageView) or
    (fCloudRaymarchingPassCascadedShadowMapImageViews[aInFlightFrameIndex]<>aCascadedShadowMapImageView) then begin

  if fCloudRaymarchingPassDepthImageViews[aInFlightFrameIndex]<>aDepthImageView then begin

   fCloudRaymarchingPassDepthImageViews[aInFlightFrameIndex]:=aDepthImageView;

   fCloudRaymarchingPassDescriptorSets[aInFlightFrameIndex].WriteToDescriptorSet(0,
                                                                                 0,
                                                                                 1,
                                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                                                                                 [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                                aDepthImageView,
                                                                                                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                                 [],
                                                                                 [],
                                                                                 not fCloudRaymarchingPassDescriptorSetFirsts[aInFlightFrameIndex]);

  end;

  if fCloudRaymarchingPassCascadedShadowMapImageViews[aInFlightFrameIndex]<>aCascadedShadowMapImageView then begin

   fCloudRaymarchingPassCascadedShadowMapImageViews[aInFlightFrameIndex]:=aCascadedShadowMapImageView;

{  fCloudRaymarchingPassDescriptorSets[aInFlightFrameIndex].WriteToDescriptorSet(10,
                                                                                 0,
                                                                                 1,
                                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                                 [TVkDescriptorImageInfo.Create(TpvScene3DRendererInstance(fRendererInstance).Renderer.ShadowMapSampler.Handle,
                                                                                                                aCascadedShadowMapImageView,
                                                                                                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                                 [],
                                                                                 [],
                                                                                 not fCloudRaymarchingPassDescriptorSetFirsts[aInFlightFrameIndex]);}
  end;

  if fCloudRaymarchingPassDescriptorSetFirsts[aInFlightFrameIndex] then begin
   fCloudRaymarchingPassDescriptorSets[aInFlightFrameIndex].Flush;
  end;

  fCloudRaymarchingPassDescriptorSetFirsts[aInFlightFrameIndex]:=false;

 end;

end;

procedure TpvScene3DAtmosphere.TRendererInstance.SetImageViews(const aInFlightFrameIndex:TpvSizeInt;
                                                               const aDepthImageView:TVkImageView;
                                                               const aCascadedShadowMapImageView:TVkImageView;
                                                               const aCloudsInscatteringImageView:TVkImageView;
                                                               const aCloudsTransmittanceImageView:TVkImageView;
                                                               const aCloudsDepthImageView:TVkImageView);
begin

 if (fRaymarchingPassDepthImageViews[aInFlightFrameIndex]<>aDepthImageView) or
    (fRaymarchingPassCascadedShadowMapImageViews[aInFlightFrameIndex]<>aCascadedShadowMapImageView) or
    (fRaymarchingPassCloudsInscatteringImageViews[aInFlightFrameIndex]<>aCloudsInscatteringImageView) or
    (fRaymarchingPassCloudsTransmittanceImageViews[aInFlightFrameIndex]<>aCloudsTransmittanceImageView) or
    (fRaymarchingPassCloudsDepthImageViews[aInFlightFrameIndex]<>aCloudsDepthImageView) then begin

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

  end;

  if fRaymarchingPassCascadedShadowMapImageViews[aInFlightFrameIndex]<>aCascadedShadowMapImageView then begin

   fRaymarchingPassCascadedShadowMapImageViews[aInFlightFrameIndex]:=aCascadedShadowMapImageView;

   fRaymarchingPassDescriptorSets[aInFlightFrameIndex].WriteToDescriptorSet(8,
                                                                            0,
                                                                            1,
                                                                            TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                            [TVkDescriptorImageInfo.Create(TpvScene3DRendererInstance(fRendererInstance).Renderer.ShadowMapSampler.Handle,
                                                                                                           aCascadedShadowMapImageView,
                                                                                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                            [],
                                                                            [],
                                                                            not fRaymarchingPassDescriptorSetFirsts[aInFlightFrameIndex]);

  end;

  if fRaymarchingPassCloudsInscatteringImageViews[aInFlightFrameIndex]<>aCloudsInscatteringImageView then begin

   fRaymarchingPassCloudsInscatteringImageViews[aInFlightFrameIndex]:=aCloudsInscatteringImageView;

   fRaymarchingPassDescriptorSets[aInFlightFrameIndex].WriteToDescriptorSet(9,
                                                                            0,
                                                                            1,
                                                                            TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                                                                            [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                           aCloudsInscatteringImageView,
                                                                                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                            [],
                                                                            [],
                                                                            not fRaymarchingPassDescriptorSetFirsts[aInFlightFrameIndex]);

  end;

  if fRaymarchingPassCloudsTransmittanceImageViews[aInFlightFrameIndex]<>aCloudsTransmittanceImageView then begin

   fRaymarchingPassCloudsTransmittanceImageViews[aInFlightFrameIndex]:=aCloudsTransmittanceImageView;

   fRaymarchingPassDescriptorSets[aInFlightFrameIndex].WriteToDescriptorSet(10,
                                                                            0,
                                                                            1,
                                                                            TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                                                                            [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                           aCloudsTransmittanceImageView,
                                                                                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                            [],
                                                                            [],
                                                                            not fRaymarchingPassDescriptorSetFirsts[aInFlightFrameIndex]);

  end;

  if fRaymarchingPassCloudsDepthImageViews[aInFlightFrameIndex]<>aCloudsDepthImageView then begin

   fRaymarchingPassCloudsDepthImageViews[aInFlightFrameIndex]:=aCloudsDepthImageView;

   fRaymarchingPassDescriptorSets[aInFlightFrameIndex].WriteToDescriptorSet(11,
                                                                            0,
                                                                            1,
                                                                            TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                                                                            [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                           aCloudsDepthImageView,
                                                                                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                            [],
                                                                            [],
                                                                            not fRaymarchingPassDescriptorSetFirsts[aInFlightFrameIndex]);

  end;

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
end;

procedure TpvScene3DAtmosphere.TRendererInstance.ReleaseGraphicsPipeline;
begin
end;

procedure TpvScene3DAtmosphere.TRendererInstance.Execute(const aInFlightFrameIndex:TpvSizeInt;
                                                         const aCommandBuffer:TpvVulkanCommandBuffer);
var BaseViewIndex,UnjitteredBaseViewIndex,CountViews:TpvSizeInt;
    InFlightFrameState:TpvScene3DRendererInstance.PInFlightFrameState;
    AtmosphereGlobals:TpvScene3DAtmosphereGlobals;
    //ImageSubresourceRange:TVkImageSubresourceRange;
    ImageMemoryBarriers:array[0..3] of TVkImageMemoryBarrier;
    TransmittanceLUTPushConstants:TpvScene3DAtmosphereGlobals.TTransmittanceLUTPushConstants;
    MultiScatteringLUTPushConstants:TpvScene3DAtmosphereGlobals.TMultiScatteringLUTPushConstants;
    SkyLuminanceLUTPushConstants:TpvScene3DAtmosphereGlobals.TSkyLuminanceLUTPushConstants;
    SkyViewLUTPushConstants:TpvScene3DAtmosphereGlobals.TSkyViewLUTPushConstants;
    CameraVolumePushConstants:TpvScene3DAtmosphereGlobals.TCameraVolumePushConstants;
    CubeMapPushConstants:TpvScene3DAtmosphereGlobals.TCubeMapPushConstants;
    DescriptorSets:array[0..2] of TVkDescriptorSet;
begin

 AtmosphereGlobals:=TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).AtmosphereGlobals);

 InFlightFrameState:=@TpvScene3DRendererInstance(fRendererInstance).InFlightFrameStates[aInFlightFrameIndex];

 BaseViewIndex:=InFlightFrameState^.FinalViewIndex;
 UnjitteredBaseViewIndex:=InFlightFrameState^.FinalUnjitteredViewIndex;
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

  DescriptorSets[0]:=TpvScene3D(fAtmosphere.fScene3D).GlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle;
  DescriptorSets[1]:=fGlobalDescriptorSets[aInFlightFrameIndex].Handle;
  DescriptorSets[2]:=fTransmittanceLUTPassDescriptorSets[aInFlightFrameIndex].Handle;

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       AtmosphereGlobals.fTransmittanceLUTComputePipelineLayout.Handle,
                                       0,
                                       3,
                                       @DescriptorSets[0],
                                       0,
                                       nil);

  TransmittanceLUTPushConstants.BaseViewIndex:=UnjitteredBaseViewIndex;
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
                                                                                       1{TpvScene3DRendererInstance(fRendererInstance).CountSurfaceViews}));

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    1,@ImageMemoryBarriers[0]);      

  aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,AtmosphereGlobals.fMultiScatteringLUTComputePipeline.Handle);

  DescriptorSets[0]:=TpvScene3D(fAtmosphere.fScene3D).GlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle;
  DescriptorSets[1]:=fGlobalDescriptorSets[aInFlightFrameIndex].Handle;
  DescriptorSets[2]:=fMultiScatteringLUTPassDescriptorSets[aInFlightFrameIndex].Handle;

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       AtmosphereGlobals.fMultiScatteringLUTComputePipelineLayout.Handle,
                                       0,
                                       3,
                                       @DescriptorSets[0],
                                       0,
                                       nil);

  MultiScatteringLUTPushConstants.BaseViewIndex:=UnjitteredBaseViewIndex;
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
                                                                                       1{TpvScene3DRendererInstance(fRendererInstance).CountSurfaceViews}));

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    0,nil,
                                    1,@ImageMemoryBarriers[0]);      

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

 end; 

 begin

  // Sky luminance LUT

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DAtmosphere.SkyLuminanceLUT',[1.0,0.8,0.4,1.0]);

  ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(0,
                                                       TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                       VK_IMAGE_LAYOUT_UNDEFINED,
                                                       VK_IMAGE_LAYOUT_GENERAL,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       fSkyLuminanceLUTTexture.VulkanImage.Handle,
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

  aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,AtmosphereGlobals.fSkyLuminanceLUTComputePipeline.Handle);

  DescriptorSets[0]:=TpvScene3D(fAtmosphere.fScene3D).GlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle;
  DescriptorSets[1]:=fGlobalDescriptorSets[aInFlightFrameIndex].Handle;
  DescriptorSets[2]:=fSkyLuminanceLUTPassDescriptorSets[aInFlightFrameIndex].Handle;

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       AtmosphereGlobals.fSkyLuminanceLUTComputePipelineLayout.Handle,
                                       0,
                                       3,
                                       @DescriptorSets[0],
                                       0,
                                       nil);

  SkyLuminanceLUTPushConstants.BaseViewIndex:=UnjitteredBaseViewIndex;
  SkyLuminanceLUTPushConstants.CountViews:=CountViews;
  SkyLuminanceLUTPushConstants.FrameIndex:=pvApplication.DrawFrameCounter;
 
  aCommandBuffer.CmdPushConstants(AtmosphereGlobals.fSkyLuminanceLUTComputePipelineLayout.Handle,
                                  TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_COMPUTE_BIT),
                                  0,
                                  SizeOf(TpvScene3DAtmosphereGlobals.TSkyLuminanceLUTPushConstants),
                                  @SkyLuminanceLUTPushConstants);    

  aCommandBuffer.CmdDispatch(fSkyLuminanceLUTTexture.Width, 
                             fSkyLuminanceLUTTexture.Height,
                             6);    

  ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                       TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                                       VK_IMAGE_LAYOUT_GENERAL,
                                                       VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       VK_QUEUE_FAMILY_IGNORED,
                                                       fSkyLuminanceLUTTexture.VulkanImage.Handle,
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

  DescriptorSets[0]:=TpvScene3D(fAtmosphere.fScene3D).GlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle;
  DescriptorSets[1]:=fGlobalDescriptorSets[aInFlightFrameIndex].Handle;
  DescriptorSets[2]:=fSkyViewLUTPassDescriptorSets[aInFlightFrameIndex].Handle;

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       AtmosphereGlobals.fSkyViewLUTComputePipelineLayout.Handle,
                                       0,
                                       3,
                                       @DescriptorSets[0],
                                       0,
                                       nil);

  SkyViewLUTPushConstants.BaseViewIndex:=UnjitteredBaseViewIndex;
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

  DescriptorSets[0]:=TpvScene3D(fAtmosphere.fScene3D).GlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle;
  DescriptorSets[1]:=fGlobalDescriptorSets[aInFlightFrameIndex].Handle;
  DescriptorSets[2]:=fCameraVolumePassDescriptorSets[aInFlightFrameIndex].Handle;

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       AtmosphereGlobals.fCameraVolumeComputePipelineLayout.Handle,
                                       0,
                                       3,
                                       @DescriptorSets[0],
                                       0,
                                       nil);

  CameraVolumePushConstants.BaseViewIndex:=UnjitteredBaseViewIndex;
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

  DescriptorSets[0]:=TpvScene3D(fAtmosphere.fScene3D).GlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle;
  DescriptorSets[1]:=fGlobalDescriptorSets[aInFlightFrameIndex].Handle;
  DescriptorSets[2]:=fCubeMapPassDescriptorSets[aInFlightFrameIndex].Handle;

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       AtmosphereGlobals.fCubeMapComputePipelineLayout.Handle,
                                       0,
                                       3,
                                       @DescriptorSets[0],
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

 begin

  // Cube map mip map generation pass

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DAtmosphere.CubeMapMipMapPass',[1.0,0.5,0.75,1.0]);

  fCubeMapMipMapGenerator.Execute(aCommandBuffer);

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

 end;

 begin
  
  // GGX cube map IBL filter pass 

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DAtmosphere.GGXCubeMapIBLFilterPass',[0.5,1.0,0.75,1.0]);

  fGGXCubeMapIBLFilter.Execute(aCommandBuffer);

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

 end;

 begin

  // Charlie cube map IBL filter pass 

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DAtmosphere.CharlieCubeMapIBLFilterPass',[0.5,0.75,1.0,1.0]);

  fCharlieCubeMapIBLFilter.Execute(aCommandBuffer);

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

 end;

 begin

  // Lambertian cube map IBL filter pass 

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DAtmosphere.LambertianCubeMapIBLFilterPass',[0.75,1.0,0.5,1.0]);

  fLambertianCubeMapIBLFilter.Execute(aCommandBuffer);

  TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

 end;

end;

procedure TpvScene3DAtmosphere.TRendererInstance.DrawClouds(const aInFlightFrameIndex:TpvSizeInt;
                                                            const aCommandBuffer:TpvVulkanCommandBuffer;
                                                            const aDepthImageView:TVkImageView;
                                                            const aCascadedShadowMapImageView:TVkImageView);
var DescriptorSets:array[0..2] of TVkDescriptorSet;
begin

 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Atmosphere.Clouds.Draw',[1.0,0.0,0.0,1.0]);

 SetCloudsImageViews(aInFlightFrameIndex,aDepthImageView,aCascadedShadowMapImageView);

 DescriptorSets[0]:=TpvScene3D(fAtmosphere.fScene3D).GlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle;
 DescriptorSets[1]:=fGlobalDescriptorSets[aInFlightFrameIndex].Handle;
 DescriptorSets[2]:=fCloudRaymarchingPassDescriptorSets[aInFlightFrameIndex].Handle;

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).AtmosphereGlobals).fCloudRaymarchingPipelineLayout.Handle,
                                      0,
                                      3,
                                      @DescriptorSets,
                                      0,
                                      nil);

 aCommandBuffer.CmdDraw(3,1,0,0);

 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

end;

procedure TpvScene3DAtmosphere.TRendererInstance.Draw(const aInFlightFrameIndex:TpvSizeInt;
                                                      const aCommandBuffer:TpvVulkanCommandBuffer;
                                                      const aDepthImageView:TVkImageView;
                                                      const aCascadedShadowMapImageView:TVkImageView;
                                                      const aCloudsInscatteringImageView:TVkImageView;
                                                      const aCloudsTransmittanceImageView:TVkImageView;
                                                      const aCloudsDepthImageView:TVkImageView);
var DescriptorSets:array[0..2] of TVkDescriptorSet;
begin

 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Atmosphere.Draw',[1.0,0.0,0.0,1.0]);

 SetImageViews(aInFlightFrameIndex,aDepthImageView,aCascadedShadowMapImageView,aCloudsInscatteringImageView,aCloudsTransmittanceImageView,aCloudsDepthImageView);

 DescriptorSets[0]:=TpvScene3D(fAtmosphere.fScene3D).GlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle;
 DescriptorSets[1]:=fGlobalDescriptorSets[aInFlightFrameIndex].Handle;
 DescriptorSets[2]:=fRaymarchingPassDescriptorSets[aInFlightFrameIndex].Handle;

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      TpvScene3DAtmosphereGlobals(TpvScene3D(fAtmosphere.fScene3D).AtmosphereGlobals).fRaymarchingPipelineLayout.Handle,
                                      0,
                                      3,
                                      @DescriptorSets,
                                      0,
                                      nil);

 aCommandBuffer.CmdDraw(3,1,0,0);

 TpvScene3D(fAtmosphere.fScene3D).VulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

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
var InFlightFrameIndex,Index:TpvSizeInt;
begin
 
 if assigned(TpvScene3D(fScene3D).VulkanDevice) and not fUploaded then begin
 
  for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin

   fAtmosphereParametersBuffers[InFlightFrameIndex]:=TpvVulkanBuffer.Create(TpvScene3D(fScene3D).VulkanDevice,
                                                                            SizeOf(TGPUAtmosphereParameters),
                                                                            TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or
                                                                            TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or
                                                                            TVkBufferUsageFlags(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT) or
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

  fWeatherMapTexture:=TpvScene3DRendererImageCubeMap.Create(TpvScene3D(fScene3D).VulkanDevice,
                                                            1024,
                                                            1024,
                                                            VK_FORMAT_R8G8B8A8_UNORM,
                                                            true,
                                                            VK_SAMPLE_COUNT_1_BIT,
                                                            VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                            VK_SHARING_MODE_EXCLUSIVE,
                                                            nil,
                                                            0,
                                                            'WeatherMap');

  fWeatherMapTextureDescriptorPool:=TpvVulkanDescriptorPool.Create(TpvScene3D(fScene3D).VulkanDevice,
                                                                   TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                                   TpvScene3D(fScene3D).CountInFlightFrames*2);
  fWeatherMapTextureDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,TpvScene3D(fScene3D).CountInFlightFrames*2);
  fWeatherMapTextureDescriptorPool.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fWeatherMapTextureDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'WeatherMapTextureDescriptorPool');

  begin

   for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin

    fWeatherMapTextureDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fWeatherMapTextureDescriptorPool,
                                                                                        TpvScene3DAtmosphereGlobals(TpvScene3D(fScene3D).AtmosphereGlobals).fWeatherMapTextureDescriptorSetLayout);

    fWeatherMapTextureDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                              0,
                                                                              1,
                                                                              TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                                              [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                                             fWeatherMapTexture.VulkanImageView.Handle,
                                                                                                             VK_IMAGE_LAYOUT_GENERAL)],
                                                                              [],
                                                                              [],
                                                                              false);

    fWeatherMapTextureDescriptorSets[InFlightFrameIndex].Flush;

    TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fWeatherMapTextureDescriptorSets[InFlightFrameIndex].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'WeatherMapTextureDescriptorSets['+IntToStr(Index)+']['+IntToStr(InFlightFrameIndex)+']');

   end;

   fWeatherMapTextureGeneration:=1;

   fWeatherMapTextureLastGeneration:=0;

   FillChar(fCloudWeatherMapPushConstants,SizeOf(TpvScene3DAtmosphereGlobals.TCloudWeatherMapPushConstants),#0);

  end;

  fUploaded:=true;
 
 end;

end;

procedure TpvScene3DAtmosphere.Unload;
var InFlightFrameIndex,Index:TpvSizeInt;
begin
 if fUploaded then begin
  for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
   FreeAndNil(fAtmosphereParametersBuffers[InFlightFrameIndex]);
  end;
  for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
   FreeAndNil(fWeatherMapTextureDescriptorSets[InFlightFrameIndex]);
  end;
  FreeAndNil(fWeatherMapTextureDescriptorPool);
  FreeAndNil(fWeatherMapTexture);
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

function TpvScene3DAtmosphere.IsInFlightFrameVisible(const aInFlightFrameIndex:TpvSizeInt):Boolean;
begin
 result:=fInFlightFrameVisible[aInFlightFrameIndex];
end;

function TpvScene3DAtmosphere.GetRenderInstance(const aRendererInstance:TObject):TpvScene3DAtmosphere.TRendererInstance;
var AtmosphereRendererInstanceKey:TpvScene3DAtmosphere.TRendererInstance.TKey;
begin
 AtmosphereRendererInstanceKey:=TpvScene3DAtmosphere.TRendererInstance.TKey.Create(aRendererInstance);
 result:=fRendererInstanceHashMap[AtmosphereRendererInstanceKey];
 if not assigned(result) then begin
  result:=TpvScene3DAtmosphere.TRendererInstance.Create(self,aRendererInstance);
 end;
end;

procedure TpvScene3DAtmosphere.ProcessSimulation(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                 const aInFlightFrameIndex:TpvSizeInt);
var Index:TpvSizeInt;
    AtmosphereGlobals:TpvScene3DAtmosphereGlobals;
    PushConstants:TpvScene3DAtmosphereGlobals.TCloudWeatherMapPushConstants;
    ImageMemoryBarrier:TVkImageMemoryBarrier;
begin

 if fInFlightFrameVisible[aInFlightFrameIndex] then begin

  begin

   FillChar(PushConstants,SizeOf(TpvScene3DAtmosphereGlobals.TCloudWeatherMapPushConstants),#0);
   PushConstants.CoverageRotation:=TpvVector4.InlineableCreate(1.0,0.0,0.0,1.0*(PI*0.25));
   PushConstants.TypeRotation:=TpvVector4.InlineableCreate(1.0,0.0,0.0,1.0*(PI*0.125));
   PushConstants.WetnessRotation:=TpvVector4.InlineableCreate(1.0,0.0,0.0,1.0*(PI*0.5));
   PushConstants.TopRotation:=TpvVector4.InlineableCreate(1.0,0.0,0.0,1.0*(PI*0.75));
   PushConstants.CoveragePerlinWorleyDifference:=0.5;
   PushConstants.TotalSize:=4.0;
   PushConstants.WorleySeed:=10.0;

   if (fWeatherMapTextureLastGeneration=fWeatherMapTextureGeneration) and not CompareMem(@PushConstants,@fCloudWeatherMapPushConstants,SizeOf(TpvScene3DAtmosphereGlobals.TCloudWeatherMapPushConstants)) then begin
    inc(fWeatherMapTextureGeneration);
   end;

   fCloudWeatherMapPushConstants:=PushConstants;

   if fWeatherMapTextureLastGeneration<>fWeatherMapTextureGeneration then begin

    fWeatherMapTextureLastGeneration:=fWeatherMapTextureGeneration;

    AtmosphereGlobals:=TpvScene3DAtmosphereGlobals(TpvScene3D(fScene3D).AtmosphereGlobals);

    ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(0,
                                                     TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                     VK_IMAGE_LAYOUT_UNDEFINED,
                                                     VK_IMAGE_LAYOUT_GENERAL,
                                                     VK_QUEUE_FAMILY_IGNORED,
                                                     VK_QUEUE_FAMILY_IGNORED,
                                                     fWeatherMapTexture.VulkanImage.Handle,
                                                     TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                     0,
                                                                                     1,
                                                                                     0,
                                                                                     6));

    aCommandBuffer.CmdPipelineBarrier(TpvScene3D(fScene3D).VulkanDevice.PhysicalDevice.PipelineStageAllShaderBits,
                                      TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                      0,
                                      0,nil,
                                      0,nil,
                                      1,@ImageMemoryBarrier);

    aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,AtmosphereGlobals.fCloudWeatherMapComputePipeline.Handle);

    aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                         AtmosphereGlobals.fCloudWeatherMapComputePipelineLayout.Handle,
                                         0,
                                         1,
                                         @fWeatherMapTextureDescriptorSets[aInFlightFrameIndex].Handle,
                                         0,
                                         nil);

    aCommandBuffer.CmdPushConstants(AtmosphereGlobals.fCloudWeatherMapComputePipelineLayout.Handle,
                                    TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_COMPUTE_BIT),
                                    0,
                                    SizeOf(TpvScene3DAtmosphereGlobals.TCloudWeatherMapPushConstants),
                                    @PushConstants);

    aCommandBuffer.CmdDispatch((fWeatherMapTexture.Width+15) shr 4,
                               (fWeatherMapTexture.Height+15) shr 4,
                               6);

    ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                     TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                                     VK_IMAGE_LAYOUT_GENERAL,
                                                     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                     VK_QUEUE_FAMILY_IGNORED,
                                                     VK_QUEUE_FAMILY_IGNORED,
                                                     fWeatherMapTexture.VulkanImage.Handle,
                                                     TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                     0,
                                                                                     1,
                                                                                     0,
                                                                                     6));

    aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                      TpvScene3D(fScene3D).VulkanDevice.PhysicalDevice.PipelineStageAllShaderBits,
                                      0,
                                      0,nil,
                                      0,nil,
                                      1,@ImageMemoryBarrier);

   end;

  end;

 end;

end;

procedure TpvScene3DAtmosphere.Execute(const aInFlightFrameIndex:TpvSizeInt;
                                       const aCommandBuffer:TpvVulkanCommandBuffer;
                                       const aRendererInstance:TObject);
var AtmosphereRendererInstance:TpvScene3DAtmosphere.TRendererInstance;
begin

 if fInFlightFrameVisible[aInFlightFrameIndex] then begin

  AtmosphereRendererInstance:=GetRenderInstance(aRendererInstance);

  if assigned(AtmosphereRendererInstance) then begin
   AtmosphereRendererInstance.Execute(aInFlightFrameIndex,aCommandBuffer);
  end;
   
 end;

end;

procedure TpvScene3DAtmosphere.DrawClouds(const aInFlightFrameIndex:TpvSizeInt;
                                          const aCommandBuffer:TpvVulkanCommandBuffer;
                                          const aDepthImageView:TVkImageView;
                                          const aCascadedShadowMapImageView:TVkImageView;
                                          const aRendererInstance:TObject);
var AtmosphereRendererInstance:TpvScene3DAtmosphere.TRendererInstance;
begin

 if fInFlightFrameVisible[aInFlightFrameIndex] then begin

  AtmosphereRendererInstance:=GetRenderInstance(aRendererInstance);

  if assigned(AtmosphereRendererInstance) then begin
   AtmosphereRendererInstance.DrawClouds(aInFlightFrameIndex,aCommandBuffer,aDepthImageView,aCascadedShadowMapImageView);
  end;

 end;

end;

procedure TpvScene3DAtmosphere.Draw(const aInFlightFrameIndex:TpvSizeInt;
                                    const aCommandBuffer:TpvVulkanCommandBuffer;
                                    const aDepthImageView:TVkImageView;
                                    const aCascadedShadowMapImageView:TVkImageView;
                                    const aCloudsInscatteringImageView:TVkImageView;
                                    const aCloudsTransmittanceImageView:TVkImageView;
                                    const aCloudsDepthImageView:TVkImageView;
                                    const aRendererInstance:TObject);
var AtmosphereRendererInstance:TpvScene3DAtmosphere.TRendererInstance;
begin

 if fInFlightFrameVisible[aInFlightFrameIndex] then begin

  AtmosphereRendererInstance:=GetRenderInstance(aRendererInstance);

  if assigned(AtmosphereRendererInstance) then begin
   AtmosphereRendererInstance.Draw(aInFlightFrameIndex,aCommandBuffer,aDepthImageView,aCascadedShadowMapImageView,aCloudsInscatteringImageView,aCloudsTransmittanceImageView,aCloudsDepthImageView);
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

procedure TpvScene3DAtmospheres.DrawClouds(const aInFlightFrameIndex:TpvSizeInt;
                                           const aCommandBuffer:TpvVulkanCommandBuffer;
                                           const aDepthImageView:TVkImageView;
                                           const aCascadedShadowMapImageView:TVkImageView;
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
     Atmosphere.DrawClouds(aInFlightFrameIndex,aCommandBuffer,aDepthImageView,aCascadedShadowMapImageView,aRendererInstance);
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
                                     const aCascadedShadowMapImageView:TVkImageView;
                                     const aCloudsInscatteringImageView:TVkImageView;
                                     const aCloudsTransmittanceImageView:TVkImageView;
                                     const aCloudsDepthImageView:TVkImageView;
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
     Atmosphere.Draw(aInFlightFrameIndex,aCommandBuffer,aDepthImageView,aCascadedShadowMapImageView,aCloudsInscatteringImageView,aCloudsTransmittanceImageView,aCloudsDepthImageView,aRendererInstance);
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
 fSkyLuminanceLUTPassDescriptorSetLayout:=nil;
 fSkyViewLUTPassDescriptorSetLayout:=nil;
 fCameraVolumePassDescriptorSetLayout:=nil;
 fCubeMapPassDescriptorSetLayout:=nil;
 fCloudRaymarchingPassDescriptorSetLayout:=nil;
 fRaymarchingPassDescriptorSetLayout:=nil;
 fGlobalVulkanDescriptorSetLayout:=nil;
 fWeatherMapTextureDescriptorSetLayout:=nil;
end;

destructor TpvScene3DAtmosphereGlobals.Destroy;
begin
 DeallocateResources;
 inherited Destroy;
end;

procedure TpvScene3DAtmosphereGlobals.AllocateResources;
var Stream:TStream;
    Queue:TpvVulkanQueue;
    CommandPool:TpvVulkanCommandPool;
    CommandBuffer:TpvVulkanCommandBuffer;
    Fence:TpvVulkanFence;
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

 // Sky luminance LUT pass descriptor set layout
 begin

  fSkyLuminanceLUTPassDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(TpvScene3D(fScene3D).VulkanDevice);

  // Destination texture
  fSkyLuminanceLUTPassDescriptorSetLayout.AddBinding(0,
                                                     VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                                     1,
                                                     TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                     []);

  // Transmittance LUT texture
  fSkyLuminanceLUTPassDescriptorSetLayout.AddBinding(1,
                                                     VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                     1,
                                                     TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                     []);

  // Multi scattering LUT texture
  fSkyLuminanceLUTPassDescriptorSetLayout.AddBinding(2,
                                                     VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                     1,
                                                     TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                     []);

  // Atmosphere parameters
  fSkyLuminanceLUTPassDescriptorSetLayout.AddBinding(3,
                                                     VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                     1,
                                                     TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                     []);

  // Blue noise texture
  fSkyLuminanceLUTPassDescriptorSetLayout.AddBinding(4,
                                                     VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                     1,
                                                     TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                     []);

  fSkyLuminanceLUTPassDescriptorSetLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fSkyLuminanceLUTPassDescriptorSetLayout.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,'TpvScene3DAtmosphereGlobals.fSkyLuminanceLUTPassDescriptorSetLayout');

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

  // Sky luminance LUT texture
  fCubeMapPassDescriptorSetLayout.AddBinding(3,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                             []);

  // Atmosphere parameters
  fCubeMapPassDescriptorSetLayout.AddBinding(4,
                                             VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                             []);

  fCubeMapPassDescriptorSetLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCubeMapPassDescriptorSetLayout.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,'TpvScene3DAtmosphereGlobals.fCubeMapPassDescriptorSetLayout');

 end;

 // Cloud raymarching pass descriptor set layout
 begin

  fCloudRaymarchingPassDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(TpvScene3D(fScene3D).VulkanDevice);

  // Depth texture
  fCloudRaymarchingPassDescriptorSetLayout.AddBinding(0,
                                                      VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE,
                                                      1,
                                                      TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                      []);

  // Atmosphere parameters
  fCloudRaymarchingPassDescriptorSetLayout.AddBinding(1,
                                                      //VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                      VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                                      1,
                                                      TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                      []);

  // Blue noise texture
  fCloudRaymarchingPassDescriptorSetLayout.AddBinding(2,
                                                      VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                      1,
                                                      TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                      []);

  // Sky luminance LUT texture
  fCloudRaymarchingPassDescriptorSetLayout.AddBinding(3,
                                                      VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                      1,
                                                      TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                      []);

  // Transmittance LUT texture
  fCloudRaymarchingPassDescriptorSetLayout.AddBinding(4,
                                                      VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                      1,
                                                      TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                      []);  

  // Shape noise texture
  fCloudRaymarchingPassDescriptorSetLayout.AddBinding(5,
                                                      VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                      1,
                                                      TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                      []);

  // Detail noise texture
  fCloudRaymarchingPassDescriptorSetLayout.AddBinding(6,
                                                      VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                      1,
                                                      TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                      []);

  // Curl noise texture
  fCloudRaymarchingPassDescriptorSetLayout.AddBinding(7,
                                                      VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                      1,
                                                      TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                      []);

  // Sky luminance LUT texture
  fCloudRaymarchingPassDescriptorSetLayout.AddBinding(8,
                                                      VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                      1,
                                                      TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                      []);

  // Weather map texture
  fCloudRaymarchingPassDescriptorSetLayout.AddBinding(9,
                                                      VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                      1,
                                                      TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                      []);

  fCloudRaymarchingPassDescriptorSetLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCloudRaymarchingPassDescriptorSetLayout.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,'TpvScene3DAtmosphereGlobals.fCloudRaymarchingPassDescriptorSetLayout');

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

  // Blue noise texture
  fRaymarchingPassDescriptorSetLayout.AddBinding(5,
                                                 VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                 1,
                                                 TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                 []);

  // Atmosphere parameters
  fRaymarchingPassDescriptorSetLayout.AddBinding(6,
                                                 VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                 1,
                                                 TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                 []);

  // Cascaded shadow map UBO
  fRaymarchingPassDescriptorSetLayout.AddBinding(7,
                                                 VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                                 1,
                                                 TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                 []);

  // Cascaded shadow map textures
  fRaymarchingPassDescriptorSetLayout.AddBinding(8,
                                                 VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                 1,
                                                 TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                 []);

  // Clouds inscattering texture
  fRaymarchingPassDescriptorSetLayout.AddBinding(9,
                                                 VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE,
                                                 1,
                                                 TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                 []); 

  // Clouds transmittance texture
  fRaymarchingPassDescriptorSetLayout.AddBinding(10,
                                                 VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE,
                                                 1,
                                                 TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                 []);

  // Clouds depth texture
  fRaymarchingPassDescriptorSetLayout.AddBinding(11,
                                                 VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE,
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

  // Weather map texture descriptor set layout
  
  fWeatherMapTextureDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(TpvScene3D(fScene3D).VulkanDevice);

  // Weather map texture
  fWeatherMapTextureDescriptorSetLayout.AddBinding(0,
                                                   VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
                                                   1,
                                                   TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                   []);

  fWeatherMapTextureDescriptorSetLayout.Initialize;

  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fWeatherMapTextureDescriptorSetLayout.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,'TpvScene3DAtmosphereGlobals.fWeatherMapTextureDescriptorSetLayout');

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
  fTransmittanceLUTComputePipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).GlobalVulkanDescriptorSetLayout);
  fTransmittanceLUTComputePipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
  fTransmittanceLUTComputePipelineLayout.AddDescriptorSetLayout(fTransmittanceLUTPassDescriptorSetLayout);
  fTransmittanceLUTComputePipelineLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fTransmittanceLUTComputePipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DAtmosphereGlobals.fTransmittanceLUTComputePipelineLayout');

  fTransmittanceLUTComputePipeline:=TpvVulkanComputePipeline.Create(TpvScene3D(fScene3D).VulkanDevice,
                                                                    TpvScene3D(fScene3D).VulkanPipelineCache,
                                                                    0,
                                                                    fTransmittanceLUTComputeShaderStage,
                                                                    fTransmittanceLUTComputePipelineLayout,
                                                                    nil,
                                                                    0);
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fTransmittanceLUTComputePipeline.Handle,VK_OBJECT_TYPE_PIPELINE,'TpvScene3DAtmosphereGlobals.fTransmittanceLUTComputePipeline'); 
 
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
  fMultiScatteringLUTComputePipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).GlobalVulkanDescriptorSetLayout);
  fMultiScatteringLUTComputePipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
  fMultiScatteringLUTComputePipelineLayout.AddDescriptorSetLayout(fMultiScatteringLUTPassDescriptorSetLayout);
  fMultiScatteringLUTComputePipelineLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fMultiScatteringLUTComputePipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DAtmosphereGlobals.fMultiScatteringLUTComputePipelineLayout');

  fMultiScatteringLUTComputePipeline:=TpvVulkanComputePipeline.Create(TpvScene3D(fScene3D).VulkanDevice,
                                                                      TpvScene3D(fScene3D).VulkanPipelineCache,
                                                                      0,
                                                                      fMultiScatteringLUTComputeShaderStage,
                                                                      fMultiScatteringLUTComputePipelineLayout,
                                                                      nil,  
                                                                      0);
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fMultiScatteringLUTComputePipeline.Handle,VK_OBJECT_TYPE_PIPELINE,'TpvScene3DAtmosphereGlobals.fMultiScatteringLUTComputePipeline');

 end;

 begin
   
  // Sky luminance LUT compute pipeline

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('atmosphere_skyluminancelut_comp.spv');
  try
   fSkyLuminanceLUTComputeShaderModule:=TpvVulkanShaderModule.Create(TpvScene3D(fScene3D).VulkanDevice,Stream);
  finally
   Stream.Free;
  end;
  
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fSkyLuminanceLUTComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DAtmosphereGlobals.fSkyLuminanceLUTComputeShaderModule');

  fSkyLuminanceLUTComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fSkyLuminanceLUTComputeShaderModule,'main');

  fSkyLuminanceLUTComputePipelineLayout:=TpvVulkanPipelineLayout.Create(TpvScene3D(fScene3D).VulkanDevice);
  fSkyLuminanceLUTComputePipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TSkyLuminanceLUTPushConstants));
  fSkyLuminanceLUTComputePipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).GlobalVulkanDescriptorSetLayout);
  fSkyLuminanceLUTComputePipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
  fSkyLuminanceLUTComputePipelineLayout.AddDescriptorSetLayout(fSkyLuminanceLUTPassDescriptorSetLayout);
  fSkyLuminanceLUTComputePipelineLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fSkyLuminanceLUTComputePipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DAtmosphereGlobals.fSkyLuminanceLUTComputePipelineLayout');

  fSkyLuminanceLUTComputePipeline:=TpvVulkanComputePipeline.Create(TpvScene3D(fScene3D).VulkanDevice,
                                                                   TpvScene3D(fScene3D).VulkanPipelineCache,
                                                                   0,
                                                                   fSkyLuminanceLUTComputeShaderStage,
                                                                   fSkyLuminanceLUTComputePipelineLayout,
                                                                   nil,
                                                                   0);
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fSkyLuminanceLUTComputePipeline.Handle,VK_OBJECT_TYPE_PIPELINE,'TpvScene3DAtmosphereGlobals.fSkyLuminanceLUTComputePipeline');

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
  fSkyViewLUTComputePipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).GlobalVulkanDescriptorSetLayout);
  fSkyViewLUTComputePipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
  fSkyViewLUTComputePipelineLayout.AddDescriptorSetLayout(fSkyViewLUTPassDescriptorSetLayout);
  fSkyViewLUTComputePipelineLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fSkyViewLUTComputePipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DAtmosphereGlobals.fSkyViewLUTComputePipelineLayout');

  fSkyViewLUTComputePipeline:=TpvVulkanComputePipeline.Create(TpvScene3D(fScene3D).VulkanDevice,
                                                              TpvScene3D(fScene3D).VulkanPipelineCache,
                                                              0,
                                                              fSkyViewLUTComputeShaderStage,
                                                              fSkyViewLUTComputePipelineLayout,
                                                              nil,
                                                              0);
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fSkyViewLUTComputePipeline.Handle,VK_OBJECT_TYPE_PIPELINE,'TpvScene3DAtmosphereGlobals.fSkyViewLUTComputePipeline');                                                            

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
  fCameraVolumeComputePipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).GlobalVulkanDescriptorSetLayout);
  fCameraVolumeComputePipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
  fCameraVolumeComputePipelineLayout.AddDescriptorSetLayout(fCameraVolumePassDescriptorSetLayout);
  fCameraVolumeComputePipelineLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCameraVolumeComputePipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DAtmosphereGlobals.fCameraVolumeComputePipelineLayout');

  fCameraVolumeComputePipeline:=TpvVulkanComputePipeline.Create(TpvScene3D(fScene3D).VulkanDevice,
                                                                TpvScene3D(fScene3D).VulkanPipelineCache,
                                                                0,
                                                                fCameraVolumeComputeShaderStage,
                                                                fCameraVolumeComputePipelineLayout,
                                                                nil,
                                                                0);
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCameraVolumeComputePipeline.Handle,VK_OBJECT_TYPE_PIPELINE,'TpvScene3DAtmosphereGlobals.fCameraVolumeComputePipeline');

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
  fCubeMapComputePipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).GlobalVulkanDescriptorSetLayout);
  fCubeMapComputePipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
  fCubeMapComputePipelineLayout.AddDescriptorSetLayout(fCubeMapPassDescriptorSetLayout);
  fCubeMapComputePipelineLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCubeMapComputePipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DAtmosphereGlobals.fCubeMapComputePipelineLayout');

  fCubeMapComputePipeline:=TpvVulkanComputePipeline.Create(TpvScene3D(fScene3D).VulkanDevice,
                                                           TpvScene3D(fScene3D).VulkanPipelineCache,
                                                           0,
                                                           fCubeMapComputeShaderStage,
                                                           fCubeMapComputePipelineLayout,
                                                           nil,                                                           
                                                           0);
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCubeMapComputePipeline.Handle,VK_OBJECT_TYPE_PIPELINE,'TpvScene3DAtmosphereGlobals.fCubeMapComputePipeline');                                                         

 end;

 begin

  // Cloud raymarching compute pipeline

  fCloudRaymarchingPipelineLayout:=TpvVulkanPipelineLayout.Create(TpvScene3D(fScene3D).VulkanDevice);
  fCloudRaymarchingPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),0,SizeOf(TCloudRaymarchingPushConstants));
  fCloudRaymarchingPipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).GlobalVulkanDescriptorSetLayout);
  fCloudRaymarchingPipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
  fCloudRaymarchingPipelineLayout.AddDescriptorSetLayout(fCloudRaymarchingPassDescriptorSetLayout);
  fCloudRaymarchingPipelineLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCloudRaymarchingPipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DAtmosphereGlobals.fCloudRaymarchingPipelineLayout');

 end;

 begin
   
  // Raymarching graphics pipeline

  fRaymarchingPipelineLayout:=TpvVulkanPipelineLayout.Create(TpvScene3D(fScene3D).VulkanDevice);
  fRaymarchingPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),0,SizeOf(TRaymarchingPushConstants));
  fRaymarchingPipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).GlobalVulkanDescriptorSetLayout);
  fRaymarchingPipelineLayout.AddDescriptorSetLayout(fGlobalVulkanDescriptorSetLayout);
  fRaymarchingPipelineLayout.AddDescriptorSetLayout(fRaymarchingPassDescriptorSetLayout);
  fRaymarchingPipelineLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fRaymarchingPipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DAtmosphereGlobals.fRaymarchingPipelineLayout');

 end;

 begin

  // Cloud weather map compute pipeline
 
  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('atmosphere_clouds_weathermap_comp.spv');
  try
   fCloudWeatherMapComputeShaderModule:=TpvVulkanShaderModule.Create(TpvScene3D(fScene3D).VulkanDevice,Stream);
  finally
   Stream.Free;
  end;

  fCloudWeatherMapComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fCloudWeatherMapComputeShaderModule,'main');
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCloudWeatherMapComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DAtmosphereGlobals.fCloudWeatherMapComputeShaderModule');

  fCloudWeatherMapComputePipelineLayout:=TpvVulkanPipelineLayout.Create(TpvScene3D(fScene3D).VulkanDevice);
  fCloudWeatherMapComputePipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TCloudWeatherMapPushConstants));
  fCloudWeatherMapComputePipelineLayout.AddDescriptorSetLayout(fWeatherMapTextureDescriptorSetLayout);
  fCloudWeatherMapComputePipelineLayout.Initialize;
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCloudWeatherMapComputePipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DAtmosphereGlobals.fCloudWeatherMapComputePipelineLayout');

  fCloudWeatherMapComputePipeline:=TpvVulkanComputePipeline.Create(TpvScene3D(fScene3D).VulkanDevice,
                                                                   TpvScene3D(fScene3D).VulkanPipelineCache,
                                                                   0,
                                                                   fCloudWeatherMapComputeShaderStage,
                                                                   fCloudWeatherMapComputePipelineLayout,
                                                                   nil,
                                                                   0);
  TpvScene3D(fScene3D).VulkanDevice.DebugUtils.SetObjectName(fCloudWeatherMapComputePipeline.Handle,VK_OBJECT_TYPE_PIPELINE,'TpvScene3DAtmosphereGlobals.fCloudWeatherMapComputePipeline');

 end;

 begin

  // Allocate resources for cloud textures

  fCloudCurlTexture:=TpvScene3DRendererMipmapImage3D.Create(TpvScene3D(fScene3D).VulkanDevice,
                                                            128,
                                                            128,
                                                            128,
                                                            VK_FORMAT_R8G8B8A8_UNORM,
                                                            true,
                                                            VK_SAMPLE_COUNT_1_BIT,
                                                            VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                            VK_SHARING_MODE_EXCLUSIVE,
                                                            [],
                                                            0,
                                                            'CloudCurlTexture');

  fCloudDetailTexture:=TpvScene3DRendererMipmapImage3D.Create(TpvScene3D(fScene3D).VulkanDevice,
                                                              32,
                                                              32,
                                                              32,
                                                              VK_FORMAT_R8G8B8A8_UNORM,
                                                              true,
                                                              VK_SAMPLE_COUNT_1_BIT,
                                                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                              VK_SHARING_MODE_EXCLUSIVE,
                                                              [],
                                                              0,
                                                              'CloudDetailTexture');

  fCloudShapeTexture:=TpvScene3DRendererMipmapImage3D.Create(TpvScene3D(fScene3D).VulkanDevice,
                                                             64,
                                                             64,
                                                             64,
                                                             VK_FORMAT_R8G8B8A8_UNORM,
                                                             true,
                                                             VK_SAMPLE_COUNT_1_BIT,
                                                             VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                             VK_SHARING_MODE_EXCLUSIVE,
                                                             [],
                                                             0,
                                                             'CloudShapeTexture');

  Queue:=TpvScene3D(fScene3D).VulkanDevice.UniversalQueue;

  CommandPool:=TpvVulkanCommandPool.Create(TpvScene3D(fScene3D).VulkanDevice,
                                           TpvScene3D(fScene3D).VulkanDevice.UniversalQueueFamilyIndex,
                                           TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
  try                                         

   CommandBuffer:=TpvVulkanCommandBuffer.Create(CommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
   try
  
    Fence:=TpvVulkanFence.Create(TpvScene3D(fScene3D).VulkanDevice);                                                           
    try

     fCloudCurlTexture.Generate(Queue,
                                CommandBuffer,
                                Fence,
                                'atmosphere_clouds_noise_curl_comp.spv',
                                8,
                                8,
                                8);

     fCloudDetailTexture.Generate(Queue,
                                  CommandBuffer,
                                  Fence,
                                  'atmosphere_clouds_noise_detail_comp.spv',
                                  8,
                                  8,
                                  8);

     fCloudShapeTexture.Generate(Queue,
                                 CommandBuffer,
                                 Fence,
                                 'atmosphere_clouds_noise_shape_comp.spv',
                                 8,
                                 8,
                                 8);      

     fCloudCurlTexture.GenerateMipMaps(Queue,
                                       CommandBuffer,
                                       Fence);

     fCloudDetailTexture.GenerateMipMaps(Queue,
                                         CommandBuffer,
                                         Fence);

     fCloudShapeTexture.GenerateMipMaps(Queue,
                                        CommandBuffer,
                                        Fence);  

    finally
     FreeAndNil(Fence);
    end;

   finally
    FreeAndNil(CommandBuffer);
   end;

  finally 
   FreeAndNil(CommandPool);
  end;

 end;

end;

procedure TpvScene3DAtmosphereGlobals.DeallocateResources;
begin

 FreeAndNil(fCloudCurlTexture);
 FreeAndNil(fCloudDetailTexture);
 FreeAndNil(fCloudShapeTexture);
  
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

 FreeAndNil(fSkyLuminanceLUTComputePipeline);
 FreeAndNil(fSkyLuminanceLUTComputePipelineLayout);
 FreeAndNil(fSkyLuminanceLUTComputeShaderStage);
 FreeAndNil(fSkyLuminanceLUTComputeShaderModule);
  
 FreeAndNil(fCameraVolumeComputePipeline);
 FreeAndNil(fCameraVolumeComputePipelineLayout);
 FreeAndNil(fCameraVolumeComputeShaderStage);
 FreeAndNil(fCameraVolumeComputeShaderModule);

 FreeAndNil(fCubeMapComputePipeline);
 FreeAndNil(fCubeMapComputePipelineLayout);
 FreeAndNil(fCubeMapComputeShaderStage);
 FreeAndNil(fCubeMapComputeShaderModule);
 
 FreeAndNil(fRaymarchingPipelineLayout);

 FreeAndNil(fCloudRaymarchingPipelineLayout);

 FreeAndNil(fCloudWeatherMapComputePipeline);
 FreeAndNil(fCloudWeatherMapComputePipelineLayout);
 FreeAndNil(fCloudWeatherMapComputeShaderStage);
 FreeAndNil(fCloudWeatherMapComputeShaderModule);

 FreeAndNil(fWeatherMapTextureDescriptorSetLayout);
 FreeAndNil(fGlobalVulkanDescriptorSetLayout);
 FreeAndNil(fRaymarchingPassDescriptorSetLayout);
 FreeAndNil(fCloudRaymarchingPassDescriptorSetLayout);
 FreeAndNil(fCubeMapPassDescriptorSetLayout);
 FreeAndNil(fCameraVolumePassDescriptorSetLayout);
 FreeAndNil(fSkyViewLUTPassDescriptorSetLayout);
 FreeAndNil(fSkyLuminanceLUTPassDescriptorSetLayout);
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
