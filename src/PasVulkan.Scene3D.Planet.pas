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
unit PasVulkan.Scene3D.Planet;
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
     PasDblStrUtils,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Utils,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Resources,
     PasVulkan.FrameGraph,
     PasVulkan.TimerQuery,
     PasVulkan.Collections,
     PasVulkan.CircularDoublyLinkedList,
     PasVulkan.VirtualReality,
     PasVulkan.Scene3D.Renderer.Globals,
     PasVulkan.Scene3D.Renderer.Image2D,
     PasVulkan.Scene3D.Renderer.MipmapImage2D;

type TpvScene3DPlanets=class;

     { TpvScene3DPlanet }
     TpvScene3DPlanet=class
      public
       type THeightValue=TpvFloat;
            PHeightValue=^THeightValue;
            THeightMap=array of THeightValue;
            TSourcePrimitiveMode=
             (
              NormalizedCubeTriangles,
              NormalizedCubeQuads,
              OctasphereTriangles,
              OctasphereQuads,
              Icosphere,
              VisualMeshTriangles,
              PhysicsMeshTriangles
             );
            PSourcePrimitiveMode=^TSourcePrimitiveMode;
            TPlanetData=packed record

             ModelMatrix:TpvMatrix4x4;

             NormalMatrix:TpvMatrix4x4;

             BottomRadius:TpvFloat;
             TopRadius:TpvFloat;
             HeightMapScale:TpvFloat;
             Reserved0:TpvFloat;

             Flags:TpvUInt32;
             Resolutions:TpvUInt32;
             Reserved1:TpvUInt32;
             Reserved2:TpvUInt32;

             Selected:TpvVector4;

             Textures:array[0..15,0..3] of TpvUInt32;

            end;
            PPlanetData=^TPlanetData;
            TMaterial=record
             AlbedoTexture:TpvUInt32;
             NormalHeightTexture:TpvUInt32;
             OcclusionRoughnessMetallicTexture:TpvUInt32;
             Scale:TpvFloat;
            end;
            PMaterial=^TMaterial;
            TMaterials=array[0..15] of TMaterial;
            PMaterials=^TMaterials;
            TSizeIntArray=array of TpvSizeInt;
       const SourcePrimitiveMode:TpvScene3DPlanet.TSourcePrimitiveMode=TpvScene3DPlanet.TSourcePrimitiveMode.OctasphereQuads;
             Direct:Boolean=false;
       type TMeshVertex=record
             PositionAbsoluteHeight:TpvVector4;
             NormalRelativeHeight:TpvVector4;
            end;
            PMeshVertex=^TMeshVertex;
            TMeshVertices=TpvDynamicArrayList<TMeshVertex>;
            TMeshIndex=TpvUInt32;
            PMeshIndex=^TMeshIndex;
            TMeshIndices=TpvDynamicArrayList<TMeshIndex>;
            TTileDirtyQueueItem=TpvUInt32;
            PTileDirtyQueueItem=^TTileDirtyQueueItem;
            TTileDirtyQueueItems=TpvDynamicArrayList<TTileDirtyQueueItem>;
            TTileGeneration=TpvUInt64;
            PTileGeneration=^TTileGeneration;
            TTileGenerations=array of TTileGeneration;
            { TData }
            TData=class // one ground truth instance and one or more in-flight instances for flawlessly parallel rendering
             public
              type TOwnershipHolderState=
                    (
                     Uninitialized=0, 
                     AcquiredOnUniversalQueue=1,
                     ReleasedOnUniversalQueue=2,
                     AcquiredOnComputeQueue=3,
                     ReleasedOnComputeQueue=4
                    );
                   POwnershipHolderState=^TOwnershipHolderState;
                   TTileDirtyMap=array of TpvUInt32;
                   TTiledMeshBoundingBox=packed record
                    Min:TpvVector4;
                    Max:TpvVector4;
                   end;
                   PTiledMeshBoundingBox=^TTiledMeshBoundingBox;
                   TTiledMeshBoundingBoxes=TpvDynamicArrayList<TTiledMeshBoundingBox>;
                   TTiledMeshBoundingSphere=TpvVector4;
                   PTiledMeshBoundingSphere=^TTiledMeshBoundingSphere;
                   TTiledMeshBoundingSpheres=TpvDynamicArrayList<TTiledMeshBoundingSphere>;
             private    // All 2D maps are octahedral projected maps in this implementation (not equirectangular projected maps or cube maps)
              fPlanet:TpvScene3DPlanet;
              fInFlightFrameIndex:TpvInt32; // -1 is the ground truth instance, >=0 are the in-flight frame instances
              fHeightMap:THeightMap; // only on the ground truth instance, otherwise nil
              fHeightMapImage:TpvScene3DRendererMipmapImage2D; // R32_SFLOAT (at least for now, just for the sake of simplicity, later maybe R16_UNORM or R16_SNORM)
              fNormalMapImage:TpvScene3DRendererMipmapImage2D; // R16G16B16A16_SNORM (at least for now, just for the sake of simplicity, later maybe RGBA8_SNORM)
              fTangentBitangentMapImage:TpvScene3DRendererImage2D; // R16RG16B16A16_SFLOAT (octahedral-wise)
              fTileDirtyMap:TpvScene3DPlanet.TData.TTileDirtyMap;
              fTileExpandedDirtyMap:TpvScene3DPlanet.TData.TTileDirtyMap;
              fTileDirtyMapBuffer:TpvVulkanBuffer;
              fTileExpandedDirtyMapBuffer:TpvVulkanBuffer;
              fTileDirtyQueueBuffer:TpvVulkanBuffer;
              fTiledMeshBoundingBoxesBuffer:TpvVulkanBuffer;
              fTiledMeshBoundingSpheresBuffer:TpvVulkanBuffer;
              fVisualMeshIndexBuffer:TpvVulkanBuffer;
              fVisualMeshVertexBuffer:TpvVulkanBuffer;
              fPhysicsMeshIndexBuffer:TpvVulkanBuffer;
              fPhysicsMeshVertexBuffer:TpvVulkanBuffer;
              fRayIntersectionResultBuffer:TpvVulkanBuffer;
              fCountTriangleIndices:TVkUInt32;
              fCountDirtyTiles:TVkUInt32;
              fModelMatrix:TpvMatrix4x4;
              fReady:TPasMPBool32;
              fInitialized:TPasMPBool32;
              fHeightMapGeneration:TpvUInt64;
              fHeightMapProcessedGeneration:TpvUInt64;
//            fVisualMeshGeneration:TpvUInt64;
              fOwnershipHolderState:TpvScene3DPlanet.TData.TOwnershipHolderState;
              fSelectedRegion:TpvVector4;
              fSelectedRegionProperty:TpvVector4Property;
              fModifyHeightMapActive:Boolean;
              fModifyHeightMapBorderRadius:TpvScalar;
              fModifyHeightMapFactor:TpvScalar;
              fWireframeActive:Boolean;
              fDisplacementMappingActive:Boolean;
              fParallaxMappingActive:Boolean;
              fMeshVertices:TMeshVertices;
              fMeshIndices:TMeshIndices;
              fTileDirtyQueueItems:TTileDirtyQueueItems;
              fTileGenerations:TTileGenerations;
              fTiledMeshBoundingBoxes:TTiledMeshBoundingBoxes;
              fTiledMeshBoundingSpheres:TTiledMeshBoundingSpheres;
             public
              constructor Create(const aPlanet:TpvScene3DPlanet;const aInFlightFrameIndex:TpvInt32); reintroduce;
              destructor Destroy; override; 
              procedure AcquireOnUniversalQueue(const aCommandBuffer:TpvVulkanCommandBuffer);
              procedure ReleaseOnUniversalQueue(const aCommandBuffer:TpvVulkanCommandBuffer);
              procedure AcquireOnComputeQueue(const aCommandBuffer:TpvVulkanCommandBuffer);
              procedure ReleaseOnComputeQueue(const aCommandBuffer:TpvVulkanCommandBuffer);
              procedure CheckDirtyMap;
              procedure TransferTo(const aCommandBuffer:TpvVulkanCommandBuffer;
                                   const aInFlightFrameData:TData);
              procedure Assign(const aData:TData);
             published
              property Planet:TpvScene3DPlanet read fPlanet;
              property InFlightFrameIndex:TpvInt32 read fInFlightFrameIndex;
              property HeightMap:THeightMap read fHeightMap;              
              property HeightMapImage:TpvScene3DRendererMipmapImage2D read fHeightMapImage;
              property NormalMapImage:TpvScene3DRendererMipmapImage2D read fNormalMapImage;
              property TangentBitangentMapImage:TpvScene3DRendererImage2D read fTangentBitangentMapImage; 
              property TileDirtyMapBuffer:TpvVulkanBuffer read fTileDirtyMapBuffer;
              property TileExpandedDirtyMapBuffer:TpvVulkanBuffer read fTileExpandedDirtyMapBuffer;
              property TileDirtyQueueBuffer:TpvVulkanBuffer read fTileDirtyQueueBuffer;
              property TiledMeshBoundingBoxesBuffer:TpvVulkanBuffer read fTiledMeshBoundingBoxesBuffer;
              property TiledMeshBoundingSpheresBuffer:TpvVulkanBuffer read fTiledMeshBoundingSpheresBuffer;
              property VisualMeshVertexBuffer:TpvVulkanBuffer read fVisualMeshVertexBuffer;
              property VisualMeshIndexBuffer:TpvVulkanBuffer read fVisualMeshIndexBuffer;
              property PhysicsMeshVertexBuffer:TpvVulkanBuffer read fPhysicsMeshVertexBuffer;
              property PhysicsMeshIndexBuffer:TpvVulkanBuffer read fPhysicsMeshIndexBuffer;
              property RayIntersectionResultBuffer:TpvVulkanBuffer read fRayIntersectionResultBuffer;
              property MeshVertices:TMeshVertices read fMeshVertices;
              property MeshIndices:TMeshIndices read fMeshIndices;
              property TileDirtyQueueItems:TTileDirtyQueueItems read fTileDirtyQueueItems;
              property CountDirtyTiles:TpvUInt32 read fCountDirtyTiles;
             public
              property TileGenerations:TTileGenerations read fTileGenerations;
              property TiledMeshBoundingBoxes:TTiledMeshBoundingBoxes read fTiledMeshBoundingBoxes;
              property TiledMeshBoundingSpheres:TTiledMeshBoundingSpheres read fTiledMeshBoundingSpheres;
              property ModelMatrix:TpvMatrix4x4 read fModelMatrix write fModelMatrix;
              property Ready:TPasMPBool32 read fReady write fReady;
              property SelectedRegion:TpvVector4Property read fSelectedRegionProperty;
              property ModifyHeightMapActive:Boolean read fModifyHeightMapActive write fModifyHeightMapActive;
              property ModifyHeightMapBorderRadius:TpvScalar read fModifyHeightMapBorderRadius write fModifyHeightMapBorderRadius;
              property ModifyHeightMapFactor:TpvScalar read fModifyHeightMapFactor write fModifyHeightMapFactor;
              property WireframeActive:Boolean read fWireframeActive write fWireframeActive;
              property DisplacementMappingActive:Boolean read fDisplacementMappingActive write fDisplacementMappingActive;
              property ParallaxMappingActive:Boolean read fParallaxMappingActive write fParallaxMappingActive;
            end;
            TInFlightFrameDataList=TpvObjectGenericList<TData>;
            { THeightMapRandomInitialization }
            THeightMapRandomInitialization=class
             public 
              type TPushConstants=packed record
                    Octaves:TpvInt32;
                    Scale:TpvFloat;
                    Amplitude:TpvFloat;
                    Lacunarity:TpvFloat;
                    Gain:TpvFloat;
                    Factor:TpvFloat;
                    MinHeight:TpvFloat;
                    MaxHeight:TpvFloat;
                    BottomRadius:TpvFloat;
                    TopRadius:TpvFloat;
                    TileMapResolution:TpvUInt32;
                    TileMapShift:TpvUInt32;
                   end;
                   PPushConstants=^TPushConstants;
             private
              fPlanet:TpvScene3DPlanet;
              fVulkanDevice:TpvVulkanDevice;
              fComputeShaderModule:TpvVulkanShaderModule;
              fComputeShaderStage:TpvVulkanPipelineShaderStage;
              fPipeline:TpvVulkanComputePipeline;
              fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
              fDescriptorPool:TpvVulkanDescriptorPool;
              fDescriptorSet:TpvVulkanDescriptorSet;
              fPipelineLayout:TpvVulkanPipelineLayout;
              fPushConstants:TPushConstants;
             public 
              constructor Create(const aPlanet:TpvScene3DPlanet); reintroduce;
              destructor Destroy; override;
              procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
             public
              property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end; 
            { THeightMapModification }
            THeightMapModification=class
             public
              type TPushConstants=packed record
                    PositionRadius:TpvVector4; // xyz = position, w = radius
                    InnerRadiusValueMinMax:TpvVector4; // x = inner radius, y = value, z = min, w = max
                    TileMapResolution:TpvUInt32;
                    TileMapShift:TpvUInt32;
                    Reserved0:TpvUInt32;
                    Reserved1:TpvUInt32;
                   end;
                   PPushConstants=^TPushConstants;
             private
              fPlanet:TpvScene3DPlanet;
              fVulkanDevice:TpvVulkanDevice;
              fComputeShaderModule:TpvVulkanShaderModule;
              fComputeShaderStage:TpvVulkanPipelineShaderStage;
              fPipeline:TpvVulkanComputePipeline;
              fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
              fDescriptorPool:TpvVulkanDescriptorPool;
              fDescriptorSet:TpvVulkanDescriptorSet;
              fPipelineLayout:TpvVulkanPipelineLayout;
              fPushConstants:TPushConstants;
             public
              constructor Create(const aPlanet:TpvScene3DPlanet); reintroduce;
              destructor Destroy; override;
              procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
             public
              property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end;
            { THeightMapFlatten }
            THeightMapFlatten=class
             public
              type TPushConstants=packed record
                    Vector:TpvVector4; // xyz = vector, w=unused
                    TileMapResolution:TpvUInt32;
                    TileMapShift:TpvUInt32;
                    Reserved0:TpvUInt32;
                    Reserved1:TpvUInt32;
                    InnerRadius:TpvFloat;
                    OuterRadius:TpvFloat;
                    MinHeight:TpvFloat;
                    MaxHeight:TpvFloat;
                    BottomRadius:TpvFloat;
                    TopRadius:TpvFloat;
                    TargetHeight:TpvFloat;                    
                   end;
                   PPushConstants=^TPushConstants;
             private
              fPlanet:TpvScene3DPlanet;
              fVulkanDevice:TpvVulkanDevice;
              fComputeShaderModule:TpvVulkanShaderModule;
              fComputeShaderStage:TpvVulkanPipelineShaderStage;
              fPipeline:TpvVulkanComputePipeline;
              fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
              fDescriptorPool:TpvVulkanDescriptorPool;
              fDescriptorSet:TpvVulkanDescriptorSet;
              fPipelineLayout:TpvVulkanPipelineLayout;
              fPushConstants:TPushConstants;
             public
              constructor Create(const aPlanet:TpvScene3DPlanet); reintroduce;
              destructor Destroy; override;
              procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
             public
              property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end;
            { TTiledMeshBoundingVolumesGeneration }
            TTiledMeshBoundingVolumesGeneration=class
             public
              type TPushConstants=packed record
                    BottomRadius:TpvFloat;
                    TopRadius:TpvFloat;
                    TileMapResolution:TpvUInt32;
                   end;
                   PPushConstants=^TPushConstants;
             private
              fPlanet:TpvScene3DPlanet;
              fVulkanDevice:TpvVulkanDevice;
              fComputeShaderModule:TpvVulkanShaderModule;
              fComputeShaderStage:TpvVulkanPipelineShaderStage;
              fPipeline:TpvVulkanComputePipeline;
              fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
              fDescriptorPool:TpvVulkanDescriptorPool;
              fDescriptorSet:TpvVulkanDescriptorSet;
              fPipelineLayout:TpvVulkanPipelineLayout;
              fPushConstants:TPushConstants;
             public
              constructor Create(const aPlanet:TpvScene3DPlanet); reintroduce;
              destructor Destroy; override;
              procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
             public
              property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end;
            { TTileDirtyExpansion }
            TTileDirtyExpansion=class
             public
              type TPushConstants=packed record
                    TileMapResolution:TpvUInt32;
                   end;
                   PPushConstants=^TPushConstants;
             private
              fPlanet:TpvScene3DPlanet;
              fVulkanDevice:TpvVulkanDevice;
              fComputeShaderModule:TpvVulkanShaderModule;
              fComputeShaderStage:TpvVulkanPipelineShaderStage;
              fPipeline:TpvVulkanComputePipeline;
              fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
              fDescriptorPool:TpvVulkanDescriptorPool;
              fDescriptorSet:TpvVulkanDescriptorSet;
              fPipelineLayout:TpvVulkanPipelineLayout;
              fPushConstants:TPushConstants;
             public
              constructor Create(const aPlanet:TpvScene3DPlanet); reintroduce;
              destructor Destroy; override;
              procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
             public
              property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end;
            { TTileDirtyQueueGeneration }
            TTileDirtyQueueGeneration=class
             public
              type TPushConstants=packed record
                    TileMapResolution:TpvUInt32;
                    VisualTileResolution:TpvUInt32;
                    PhysicsTileResolution:TpvUInt32;
                   end;
                   PPushConstants=^TPushConstants;
             private
              fPlanet:TpvScene3DPlanet;
              fVulkanDevice:TpvVulkanDevice;
              fComputeShaderModule:TpvVulkanShaderModule;
              fComputeShaderStage:TpvVulkanPipelineShaderStage;
              fPipeline:TpvVulkanComputePipeline;
              fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
              fDescriptorPool:TpvVulkanDescriptorPool;
              fDescriptorSet:TpvVulkanDescriptorSet;
              fPipelineLayout:TpvVulkanPipelineLayout;
              fPushConstants:TPushConstants;
             public
              constructor Create(const aPlanet:TpvScene3DPlanet); reintroduce;
              destructor Destroy; override;
              procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
             public
              property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end;
            { TNormalMapGeneration }
            TNormalMapGeneration=class
             public              
              type TPushConstants=packed record
                    PlanetGroundRadius:TpvFloat; // planet ground radius
                    HeightMapScale:TpvFloat; // scale of height map
                   end;
                   PPushConstants=^TPushConstants;
             private
              fPlanet:TpvScene3DPlanet;
              fVulkanDevice:TpvVulkanDevice;
              fComputeShaderModule:TpvVulkanShaderModule;
              fComputeShaderStage:TpvVulkanPipelineShaderStage;
              fPipeline:TpvVulkanComputePipeline;
              fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
              fDescriptorPool:TpvVulkanDescriptorPool;
              fDescriptorSet:TpvVulkanDescriptorSet;
              fPipelineLayout:TpvVulkanPipelineLayout;
              fPushConstants:TPushConstants;
             public
              constructor Create(const aPlanet:TpvScene3DPlanet); reintroduce;
              destructor Destroy; override;
              procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
             public
              property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end;
            { THeightMapMipMapGeneration }
            THeightMapMipMapGeneration=class
             public              
              type TPushConstants=packed record
                    CountMipMapLevels:TpvInt32; // remaining count of mip map levels but maximum 4 at once
                   end;
                   PPushConstants=^TPushConstants;
             private
              fPlanet:TpvScene3DPlanet;
              fVulkanDevice:TpvVulkanDevice;
              fComputeShaderModule:TpvVulkanShaderModule;
              fComputeShaderStage:TpvVulkanPipelineShaderStage;
              fPipeline:TpvVulkanComputePipeline;
              fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
              fDescriptorPool:TpvVulkanDescriptorPool;
              fDescriptorSets:array[0..7] of TpvVulkanDescriptorSet; // 8*4 = max. 32 mip map levels, more than enough
              fPipelineLayout:TpvVulkanPipelineLayout;
              fPushConstants:TPushConstants;
              fCountMipMapLevelSets:TpvSizeInt;
             public
              constructor Create(const aPlanet:TpvScene3DPlanet); reintroduce;
              destructor Destroy; override;
              procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
             public
              property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end;
            { TNormalMapMipMapGeneration }
            TNormalMapMipMapGeneration=class
             public
              type TPushConstants=packed record
                    CountMipMapLevels:TpvInt32; // remaining count of mip map levels but maximum 4 at once
                   end;
                   PPushConstants=^TPushConstants;
             private
              fPlanet:TpvScene3DPlanet;
              fVulkanDevice:TpvVulkanDevice;
              fComputeShaderModule:TpvVulkanShaderModule;
              fComputeShaderStage:TpvVulkanPipelineShaderStage;
              fPipeline:TpvVulkanComputePipeline;
              fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
              fDescriptorPool:TpvVulkanDescriptorPool;
              fDescriptorSets:array[0..7] of TpvVulkanDescriptorSet; // 8*4 = max. 32 mip map levels, more than enough
              fPipelineLayout:TpvVulkanPipelineLayout;
              fPushConstants:TPushConstants;
              fCountMipMapLevelSets:TpvSizeInt;
             public
              constructor Create(const aPlanet:TpvScene3DPlanet); reintroduce;
              destructor Destroy; override;
              procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
             public
              property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end;
            { TMeshIndexGeneration }
            TMeshIndexGeneration=class
             public
              type TPushConstants=packed record
                    TileMapResolution:TpvUInt32;
                    TileResolution:TpvUInt32;
                   end;
                   PPushConstants=^TPushConstants;
             private
              fPlanet:TpvScene3DPlanet;
              fPhysics:Boolean;
              fVulkanDevice:TpvVulkanDevice;
              fComputeShaderModule:TpvVulkanShaderModule;
              fComputeShaderStage:TpvVulkanPipelineShaderStage;
              fPipeline:TpvVulkanComputePipeline;
              fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
              fDescriptorPool:TpvVulkanDescriptorPool;
              fDescriptorSet:TpvVulkanDescriptorSet;
              fPipelineLayout:TpvVulkanPipelineLayout;
              fPushConstants:TPushConstants;
             public
              constructor Create(const aPlanet:TpvScene3DPlanet;const aPhysics:Boolean); reintroduce;
              destructor Destroy; override;
              procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
             public
              property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end;
            { TMeshVertexGeneration }
            TMeshVertexGeneration=class
             public
              type TPushConstants=packed record
                    //ModelMatrix:TpvMatrix4x4;
                    BottomRadius:TpvFloat;
                    TopRadius:TpvFloat;                    
                    TileMapResolution:TpvUInt32;
                    TileResolution:TpvUInt32;
                    LOD:TpvInt32;
                   end;
                   PPushConstants=^TPushConstants;
             private
              fPlanet:TpvScene3DPlanet;
              fPhysics:Boolean;
              fVulkanDevice:TpvVulkanDevice;
              fComputeShaderModule:TpvVulkanShaderModule;
              fComputeShaderStage:TpvVulkanPipelineShaderStage;
              fPipeline:TpvVulkanComputePipeline;
              fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
              fDescriptorPool:TpvVulkanDescriptorPool;
              fDescriptorSet:TpvVulkanDescriptorSet;
              fPipelineLayout:TpvVulkanPipelineLayout;
              fPushConstants:TPushConstants;
             public
              constructor Create(const aPlanet:TpvScene3DPlanet;const aPhysics:Boolean); reintroduce;
              destructor Destroy; override;
              procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
             public
              property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end;
(*          { TBaseMeshVertexGeneration }
            TBaseMeshVertexGeneration=class
             public
              type TPushConstants=packed record
                    CountPoints:TpvUInt32;
                   end;
                   PPushConstants=^TPushConstants;
              private
               fPlanet:TpvScene3DPlanet;
               fPhysics:Boolean;
               fVulkanDevice:TpvVulkanDevice;
               fComputeShaderModule:TpvVulkanShaderModule;
               fComputeShaderStage:TpvVulkanPipelineShaderStage;
               fPipeline:TpvVulkanComputePipeline;
               fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
               fDescriptorPool:TpvVulkanDescriptorPool;
               fDescriptorSet:TpvVulkanDescriptorSet;
               fPipelineLayout:TpvVulkanPipelineLayout;
               fPushConstants:TPushConstants;
              public
               constructor Create(const aPlanet:TpvScene3DPlanet;const aPhysics:Boolean); reintroduce;
               destructor Destroy; override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
              public
               property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end;
            { TBaseMeshIndexGeneration }
            TBaseMeshIndexGeneration=class
             public
              type TPushConstants=packed record
                    CountPoints:TpvUInt32;
                   end;
                   PPushConstants=^TPushConstants;
              private
               fPlanet:TpvScene3DPlanet;
               fPhysics:Boolean;
               fVulkanDevice:TpvVulkanDevice;
               fComputeShaderModule:TpvVulkanShaderModule;
               fComputeShaderStage:TpvVulkanPipelineShaderStage;
               fPipeline:TpvVulkanComputePipeline;
               fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
               fDescriptorPool:TpvVulkanDescriptorPool;
               fDescriptorSet:TpvVulkanDescriptorSet;
               fPipelineLayout:TpvVulkanPipelineLayout;
               fPushConstants:TPushConstants;
              public
               constructor Create(const aPlanet:TpvScene3DPlanet;const aPhysics:Boolean); reintroduce;
               destructor Destroy; override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
              public
               property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end;
            { TMeshVertexGeneration }
            TMeshVertexGeneration=class
             public
              type TPushConstants=packed record
                    ModelMatrix:TpvMatrix4x4;
                    CountPoints:TpvUInt32;
                    PlanetGroundRadius:TpvFloat;
                    HeightMapScale:TpvFloat;
                    Dummy:TpvUInt32;
                   end;
                   PPushConstants=^TPushConstants;
              private
               fPlanet:TpvScene3DPlanet;
               fPhysics:Boolean;
               fVulkanDevice:TpvVulkanDevice;
               fComputeShaderModule:TpvVulkanShaderModule;
               fComputeShaderStage:TpvVulkanPipelineShaderStage;
               fPipeline:TpvVulkanComputePipeline;
               fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
               fDescriptorPool:TpvVulkanDescriptorPool;
               fDescriptorSet:TpvVulkanDescriptorSet;
               fPipelineLayout:TpvVulkanPipelineLayout;
               fPushConstants:TPushConstants;
              public
               constructor Create(const aPlanet:TpvScene3DPlanet;const aPhysics:Boolean); reintroduce;
               destructor Destroy; override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
              public
               property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end;*)
            { TRayIntersection }
            TRayIntersection=class
             public 
              type TPushConstants=packed record  
                    RayOriginPlanetBottomRadius:TpvVector4; // xyz = ray origin, w = planet bottom radius
                    RayDirectionPlanetTopRadius:TpvVector4; // xyz = ray direction, w = planet top radius
                    PlanetCenter:TpvVector4; // xyz = planet center, w = unused
                   end;                    
                   PPushConstants=^TPushConstants;
              private
               fPlanet:TpvScene3DPlanet;
               fVulkanDevice:TpvVulkanDevice;
               fComputeShaderModule:TpvVulkanShaderModule;
               fComputeShaderStage:TpvVulkanPipelineShaderStage;
               fPipeline:TpvVulkanComputePipeline;
               fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
               fDescriptorPool:TpvVulkanDescriptorPool;
               fDescriptorSet:TpvVulkanDescriptorSet;
               fPipelineLayout:TpvVulkanPipelineLayout;
               fPushConstants:TPushConstants;
              public
               constructor Create(const aPlanet:TpvScene3DPlanet); reintroduce;
               destructor Destroy; override;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aRayOrigin,aRayDirection:TpvVector3);
              public
               property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end;
            { TRenderPass } // Used by multiple TpvScene3DPlanet instances inside the TpvScene3D render passes per renderer instance 
            TRenderPass=class
             public
              type TMode=
                    (
                     ShadowMap,
                     ReflectiveShadowMap,
                     DepthPrepass,
                     Opaque
                    );
                   PMode=^TMode; 
                   TPushConstants=packed record
                    ViewBaseIndex:TpvUInt32;
                    CountViews:TpvUInt32;
                    CountQuadPointsInOneDirection:TpvUInt32;
                    CountAllViews:TpvUInt32;
                    ResolutionXY:TpvUInt32;
                    TessellationFactor:TpvFloat;
                    Jitter:TpvVector2;
                   end;
                   PPushConstants=^TPushConstants;
             private
              fRenderer:TObject;
              fRendererInstance:TObject;
              fScene3D:TObject; 
              fMode:TpvScene3DPlanet.TRenderPass.TMode;
              fVulkanDevice:TpvVulkanDevice;              
              fRenderPass:TpvVulkanRenderPass;
              fVertexShaderModule:TpvVulkanShaderModule;
              fTessellationControlShaderModule:TpvVulkanShaderModule;
              fTessellationEvaluationShaderModule:TpvVulkanShaderModule;
              fFragmentShaderModule:TpvVulkanShaderModule;
              fVertexShaderStage:TpvVulkanPipelineShaderStage;
              fTessellationControlShaderStage:TpvVulkanPipelineShaderStage;
              fTessellationEvaluationShaderStage:TpvVulkanPipelineShaderStage;
              fFragmentShaderStage:TpvVulkanPipelineShaderStage;
              fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
              fDescriptorPool:TpvVulkanDescriptorPool;
              fDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fPipelineLayout:TpvVulkanPipelineLayout;
              fPipeline:TpvVulkanGraphicsPipeline;
              fPushConstants:TPushConstants;
              fWidth:TpvInt32;
              fHeight:TpvInt32;
             public
              constructor Create(const aRenderer:TObject;const aRendererInstance:TObject;const aScene3D:TObject;const aMode:TpvScene3DPlanet.TRenderPass.TMode); reintroduce;
              destructor Destroy; override;
              procedure AllocateResources(const aRenderPass:TpvVulkanRenderPass;
                                          const aWidth:TpvInt32;
                                          const aHeight:TpvInt32;
                                          const aVulkanSampleCountFlagBits:TVkSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT));
              procedure ReleaseResources;
              procedure Draw(const aInFlightFrameIndex,aViewBaseIndex,aCountViews:TpvSizeInt;const aCommandBuffer:TpvVulkanCommandBuffer);
             public
              property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end;
      private
       fScene3D:TObject;
       fVulkanDevice:TpvVulkanDevice;
       fVulkanMemoryStagingQueue:TpvVulkanDeviceMemoryStagingQueue;
       fVulkanComputeQueue:TpvVulkanQueue;
       fVulkanComputeFence:TpvVulkanFence;
       fVulkanComputeCommandPool:TpvVulkanCommandPool;
       fVulkanComputeCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanUniversalQueue:TpvVulkanQueue;
       fVulkanUniversalFence:TpvVulkanFence;
       fVulkanUniversalCommandPool:TpvVulkanCommandPool;
       fVulkanUniversalCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanUniversalAcquireReleaseCommandPool:TpvVulkanCommandPool;
       fVulkanUniversalAcquireCommandBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanCommandBuffer;
       fVulkanUniversalReleaseCommandBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanCommandBuffer;
       fVulkanUniversalAcquireSemaphores:array[0..MaxInFlightFrames-1] of TpvVulkanSemaphore;
       fVulkanUniversalReleaseSemaphores:array[0..MaxInFlightFrames-1] of TpvVulkanSemaphore;
       fHeightMapResolution:TpvInt32;
       fTileMapResolution:TpvInt32;
       fTileMapShift:TpvInt32;
       fVisualTileResolution:TpvInt32;
       fPhysicsTileResolution:TpvInt32;
       fVisualResolution:TpvSizeInt;
       fPhysicsResolution:TpvSizeInt;
       fBottomRadius:TpvFloat; // Start of the lowest planet ground
       fTopRadius:TpvFloat; // End of the atmosphere
       fHeightMapScale:TpvFloat; // Scale factor for the height map
       fCountVisualMeshIndices:TpvSizeInt; 
       fCountVisualMeshLODLevels:TpvSizeInt;
       fCountPhysicsMeshIndices:TpvSizeInt; 
       fCountPhysicsMeshLODLevels:TpvSizeInt;
       fVisualMeshLODOffsets:TSizeIntArray;
       fVisualMeshLODCounts:TSizeIntArray;
       fPhysicsMeshLODOffsets:TSizeIntArray;
       fPhysicsMeshLODCounts:TSizeIntArray;
       fData:TData;
       fInFlightFrameDataList:TInFlightFrameDataList;
       fReleaseFrameCounter:TpvInt32;
       fReady:TPasMPBool32;
       fInFlightFrameReady:array[0..MaxInFlightFrames-1] of TPasMPBool32;
       fHeightMapRandomInitialization:THeightMapRandomInitialization;
       fHeightMapModification:THeightMapModification;
       fHeightMapFlatten:THeightMapFlatten;
       fTiledMeshBoundingVolumesGeneration:TTiledMeshBoundingVolumesGeneration;
       fTileDirtyExpansion:TTileDirtyExpansion;
       fTileDirtyQueueGeneration:TTileDirtyQueueGeneration;
       fNormalMapGeneration:TNormalMapGeneration;
       fHeightMapMipMapGeneration:THeightMapMipMapGeneration;
       fNormalMapMipMapGeneration:TNormalMapMipMapGeneration;
       fVisualMeshIndexGeneration:TMeshIndexGeneration;
       fPhysicsMeshIndexGeneration:TMeshIndexGeneration;
       fVisualMeshVertexGeneration:TMeshVertexGeneration;
       fPhysicsMeshVertexGeneration:TMeshVertexGeneration;
{      fVisualBaseMeshVertexGeneration:TBaseMeshVertexGeneration;
       fVisualBaseMeshIndexGeneration:TBaseMeshIndexGeneration;
       fVisualMeshVertexGeneration:TMeshVertexGeneration;
       fPhysicsBaseMeshVertexGeneration:TBaseMeshVertexGeneration;
       fPhysicsBaseMeshIndexGeneration:TBaseMeshIndexGeneration;
       fPhysicsMeshVertexGeneration:TMeshVertexGeneration;}
       fRayIntersection:TRayIntersection;
       fCommandBufferLevel:TpvInt32;
       fCommandBufferLock:TPasMPInt32;
       fComputeQueueLock:TPasMPInt32;
       fGlobalBufferSharingMode:TVkSharingMode;
       fGlobalBufferQueueFamilyIndices:TpvVulkanQueueFamilyIndices;
       fInFlightFrameSharingMode:TVkSharingMode;
       fInFlightFrameQueueFamilyIndices:TpvVulkanQueueFamilyIndices;
       fPlanetData:TPlanetData;
       fPointerToPlanetData:PPlanetData;
       fPlanetDataVulkanBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fMaterials:TMaterials;
       fPointerToMaterials:PMaterials;
       fDescriptorPool:TpvVulkanDescriptorPool;
       fDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
      public
       constructor Create(const aScene3D:TObject;
                          const aHeightMapResolution:TpvInt32=4096;
                          const aVisualResolution:TpvSizeInt=4096;
                          const aPhysicsResolution:TpvSizeInt=1024;
                          const aBottomRadius:TpvFloat=70.0;
                          const aTopRadius:TpvFloat=100.0); reintroduce;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       procedure Release;
       function HandleRelease:boolean;
       class function CreatePlanetDescriptorSetLayout(const aVulkanDevice:TpvVulkanDevice):TpvVulkanDescriptorSetLayout; static;
       class function CreatePlanetDescriptorPool(const aVulkanDevice:TpvVulkanDevice;const aCountInFlightFrames:TpvSizeInt):TpvVulkanDescriptorPool; static;
       procedure BeginUpdate;
       procedure EndUpdate;
       procedure FlushUpdate;
       procedure Initialize(const aPasMPInstance:TPasMP=nil);
       procedure Flatten(const aVector:TpvVector3;const aInnerRadius,aOuterRadius,aTargetHeight:TpvFloat);
       function RayIntersection(const aRayOrigin,aRayDirection:TpvVector3;out aHitNormal:TpvVector3;out aHitTime:TpvScalar):boolean;
       procedure Update(const aInFlightFrameIndex:TpvSizeInt);
       procedure FrameUpdate(const aInFlightFrameIndex:TpvSizeInt);
       procedure UploadFrame(const aInFlightFrameIndex:TpvSizeInt);
       procedure BeginFrame(const aInFlightFrameIndex:TpvSizeInt;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
       procedure EndFrame(const aInFlightFrameIndex:TpvSizeInt;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
       procedure ExportPhysicsMeshToOBJ(const aStream:TStream); overload;
       procedure ExportPhysicsMeshToOBJ(const aFileName:TpvUTF8String); overload;
      published
       property Scene3D:TObject read fScene3D;
       property HeightMapResolution:TpvInt32 read fHeightMapResolution;
       property TileMapResolution:TpvInt32 read fTileMapResolution;
       property VisualTileResolution:TpvInt32 read fVisualTileResolution;
       property PhysicsTileResolution:TpvInt32 read fPhysicsTileResolution;
       property VisualResolution:TpvSizeInt read fVisualResolution;
       property PhysicsResolution:TpvSizeInt read fPhysicsResolution;
       property CountVisualMeshIndices:TpvSizeInt read fCountVisualMeshIndices;
       property CountVisualMeshLODLevels:TpvSizeInt read fCountVisualMeshLODLevels;
       property CountPhysicsMeshIndices:TpvSizeInt read fCountPhysicsMeshIndices;
       property CountPhysicsMeshLODLevels:TpvSizeInt read fCountPhysicsMeshLODLevels;
       property BottomRadius:TpvFloat read fBottomRadius;
       property TopRadius:TpvFloat read fTopRadius;
       property Ready:TPasMPBool32 read fReady;
       property Data:TData read fData;
       property InFlightFrameDataList:TInFlightFrameDataList read fInFlightFrameDataList;
      public
       property PlanetData:PPlanetData read fPointerToPlanetData;
       property Materials:PMaterials read fPointerToMaterials; 
       property VisualMeshLODOffsets:TSizeIntArray read fVisualMeshLODOffsets;
       property VisualMeshLODCounts:TSizeIntArray read fVisualMeshLODCounts;
       property PhysicsMeshLODOffsets:TSizeIntArray read fPhysicsMeshLODOffsets;
       property PhysicsMeshLODCounts:TSizeIntArray read fPhysicsMeshLODCounts;
     end;

     TpvScene3DPlanets=class(TpvObjectGenericList<TpvScene3DPlanet>)
      private
       fScene3D:TObject;
       fLock:TPasMPCriticalSection;
      public
       constructor Create(const aScene3D:TObject); reintroduce;
       destructor Destroy; override;
      published
       property Scene3D:TObject read fScene3D;
       property Lock:TPasMPCriticalSection read fLock;
     end;

implementation

uses PasVulkan.Scene3D,
     PasVulkan.Scene3D.Renderer,
     PasVulkan.Scene3D.Renderer.Instance,
     PasVulkan.Geometry.FibonacciSphere,
     PasVulkan.VirtualFileSystem;

type TVector3Array=TpvDynamicArray<TpvVector3>;
     TIndexArray=TpvDynamicArray<TpvUInt32>;

procedure Subdivide(var aVertices:TVector3Array;var aIndices:TIndexArray;const aSubdivisions:TpvSizeInt=2);
type TVectorHashMap=TpvHashMap<TpvVector3,TpvSizeInt>;
var SubdivisionIndex,IndexIndex,VertexIndex:TpvSizeInt;
    NewIndices:TIndexArray;
    v0,v1,v2,va,vb,vc:TpvVector3;
    i0,i1,i2,ia,ib,ic:TpvUInt32;
    VectorHashMap:TVectorHashMap;
begin

 NewIndices.Initialize;
 try

  VectorHashMap:=TVectorHashMap.Create(-1);
  try

   for VertexIndex:=0 to aVertices.Count-1 do begin
    VectorHashMap.Add(aVertices.Items[VertexIndex],VertexIndex);
   end;

   for SubdivisionIndex:=1 to aSubdivisions do begin

    NewIndices.Count:=0;

    IndexIndex:=0;
    while (IndexIndex+2)<aIndices.Count do begin

     i0:=aIndices.Items[IndexIndex+0];
     i1:=aIndices.Items[IndexIndex+1];
     i2:=aIndices.Items[IndexIndex+2];

     v0:=aVertices.Items[i0];
     v1:=aVertices.Items[i1];
     v2:=aVertices.Items[i2];

     va:=Mix(v0,v1,0.5);
     vb:=Mix(v1,v2,0.5);
     vc:=Mix(v2,v0,0.5);

     VertexIndex:=VectorHashMap[va];
     if VertexIndex<0 then begin
      VertexIndex:=aVertices.Add(va);
      VectorHashMap.Add(va,VertexIndex);
     end;
     ia:=VertexIndex;

     VertexIndex:=VectorHashMap[vb];
     if VertexIndex<0 then begin
      VertexIndex:=aVertices.Add(vb);
      VectorHashMap.Add(vb,VertexIndex);
     end;
     ib:=VertexIndex;

     VertexIndex:=VectorHashMap[vc];
     if VertexIndex<0 then begin
      VertexIndex:=aVertices.Add(vc);
      VectorHashMap.Add(vc,VertexIndex);
     end;
     ic:=VertexIndex;

     NewIndices.Add([i0,ia,ic]);
     NewIndices.Add([i1,ib,ia]);
     NewIndices.Add([i2,ic,ib]);
     NewIndices.Add([ia,ib,ic]);

     inc(IndexIndex,3);

    end;

    aIndices.Assign(NewIndices);

   end;

  finally
   FreeAndNil(VectorHashMap);
  end;

 finally
  NewIndices.Finalize;
 end;

end;

procedure NormalizeVertices(var aVertices:TVector3Array);
var VertexIndex:TpvSizeInt;
begin
 for VertexIndex:=0 to aVertices.Count-1 do begin
  aVertices.Items[VertexIndex]:=aVertices.Items[VertexIndex].Normalize;
 end;
end;

procedure CreateIcosahedronSphere(var aVertices:TVector3Array;var aIndices:TIndexArray;const aCountMinimumVertices:TpvSizeInt=4096);
const GoldenRatio=1.61803398874989485; // (1.0+sqrt(5.0))/2.0 (golden ratio)
      IcosahedronLength=1.902113032590307; // sqrt(sqr(1)+sqr(GoldenRatio))
      IcosahedronNorm=0.5257311121191336; // 1.0 / IcosahedronLength
      IcosahedronNormGoldenRatio=0.85065080835204; // GoldenRatio / IcosahedronLength
      IcosaheronVertices:array[0..11] of TpvVector3=
       (
        (x:0.0;y:IcosahedronNorm;z:IcosahedronNormGoldenRatio),
        (x:0.0;y:-IcosahedronNorm;z:IcosahedronNormGoldenRatio),
        (x:IcosahedronNorm;y:IcosahedronNormGoldenRatio;z:0.0),
        (x:-IcosahedronNorm;y:IcosahedronNormGoldenRatio;z:0.0),
        (x:IcosahedronNormGoldenRatio;y:0.0;z:IcosahedronNorm),
        (x:-IcosahedronNormGoldenRatio;y:0.0;z:IcosahedronNorm),
        (x:0.0;y:-IcosahedronNorm;z:-IcosahedronNormGoldenRatio),
        (x:0.0;y:IcosahedronNorm;z:-IcosahedronNormGoldenRatio),
        (x:-IcosahedronNorm;y:-IcosahedronNormGoldenRatio;z:0.0),
        (x:IcosahedronNorm;y:-IcosahedronNormGoldenRatio;z:0.0),
        (x:-IcosahedronNormGoldenRatio;y:0.0;z:-IcosahedronNorm),
        (x:IcosahedronNormGoldenRatio;y:0.0;z:-IcosahedronNorm)
       );
      IcosahedronIndices:array[0..(20*3)-1] of TpvUInt32=
       (
        0,5,1,0,3,5,0,2,3,0,4,2,0,1,4,
        1,5,8,5,3,10,3,2,7,2,4,11,4,1,9,
        7,11,6,11,9,6,9,8,6,8,10,6,10,7,6,
        2,11,7,4,9,11,1,8,9,5,10,8,3,7,10
       );
var SubdivisionLevel,Count:TpvSizeInt;
begin

 Count:=12;
 SubdivisionLevel:=0;
 while Count<aCountMinimumVertices do begin
  Count:=12+((10*((2 shl SubdivisionLevel)+1))*((2 shl SubdivisionLevel)-1));
  inc(SubdivisionLevel);
 end;

 aVertices.Assign(IcosaheronVertices);

 aIndices.Assign(IcosahedronIndices);

 Subdivide(aVertices,aIndices,SubdivisionLevel);

 NormalizeVertices(aVertices);

end;

{ TpvScene3DPlanet.TData }

constructor TpvScene3DPlanet.TData.Create(const aPlanet:TpvScene3DPlanet;const aInFlightFrameIndex:TpvInt32);
var ImageSharingMode,BufferSharingMode:TVkSharingMode;
    ImageQueueFamilyIndices,BufferQueueFamilyIndices:TpvVulkanQueueFamilyIndices;
begin
  
 inherited Create;

 fPlanet:=aPlanet;

 if fInFlightFrameIndex<0 then begin
  ImageSharingMode:=TVkSharingMode.VK_SHARING_MODE_EXCLUSIVE;
  ImageQueueFamilyIndices:=[];
  BufferSharingMode:=fPlanet.fGlobalBufferSharingMode;
  BufferQueueFamilyIndices:=fPlanet.fGlobalBufferQueueFamilyIndices;
 end else begin
  ImageSharingMode:=fPlanet.fInFlightFrameSharingMode;
  ImageQueueFamilyIndices:=fPlanet.fInFlightFrameQueueFamilyIndices;
  BufferSharingMode:=fPlanet.fInFlightFrameSharingMode;
  BufferQueueFamilyIndices:=fPlanet.fInFlightFrameQueueFamilyIndices;
 end;

 fInFlightFrameIndex:=aInFlightFrameIndex;

 if fInFlightFrameIndex<0 then begin
  fHeightMap:=nil;
  SetLength(fHeightMap,fPlanet.fHeightMapResolution*fPlanet.fHeightMapResolution);
 end else begin
  fHeightMap:=nil;
 end;

 fInitialized:=false;

 if fInFlightFrameIndex<0 then begin
  fHeightMapGeneration:=0;
 end else begin
  fHeightMapGeneration:=High(TpvUInt64);
 end;

 fHeightMapProcessedGeneration:=High(TpvUInt64);

 fModelMatrix:=TpvMatrix4x4.Identity;

 fHeightMapImage:=nil;

 fNormalMapImage:=nil;

 fTangentBitangentMapImage:=nil;

 fTileDirtyMap:=nil;

 fTileExpandedDirtyMap:=nil;

 fTileDirtyMapBuffer:=nil;

 fTileExpandedDirtyMapBuffer:=nil;

 fTileDirtyQueueBuffer:=nil;

 fTiledMeshBoundingBoxesBuffer:=nil;

 fTiledMeshBoundingSpheresBuffer:=nil;

 fVisualMeshVertexBuffer:=nil;

 fVisualMeshIndexBuffer:=nil;

 fPhysicsMeshVertexBuffer:=nil;

 fPhysicsMeshIndexBuffer:=nil;

 fRayIntersectionResultBuffer:=nil;

 fOwnershipHolderState:=TpvScene3DPlanet.TData.TOwnershipHolderState.Uninitialized;

 if assigned(fPlanet.fVulkanDevice) then begin

  fHeightMapImage:=TpvScene3DRendererMipmapImage2D.Create(fPlanet.fVulkanDevice,
                                                          fPlanet.fHeightMapResolution,
                                                          fPlanet.fHeightMapResolution,
                                                          VK_FORMAT_R32_SFLOAT,
                                                          true,
                                                          VK_SAMPLE_COUNT_1_BIT,
                                                          VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                          ImageSharingMode,
                                                          ImageQueueFamilyIndices);
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fHeightMapImage.VulkanImage.Handle,VK_OBJECT_TYPE_IMAGE,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fHeightMapImage.Image');
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fHeightMapImage.VulkanImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fHeightMapImage.ImageView');

  fNormalMapImage:=TpvScene3DRendererMipmapImage2D.Create(fPlanet.fVulkanDevice,
                                                          fPlanet.fHeightMapResolution,
                                                          fPlanet.fHeightMapResolution,
                                                          VK_FORMAT_R16G16B16A16_SFLOAT,
                                                          true,
                                                          VK_SAMPLE_COUNT_1_BIT,
                                                          VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                          ImageSharingMode,
                                                          ImageQueueFamilyIndices);
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fNormalMapImage.VulkanImage.Handle,VK_OBJECT_TYPE_IMAGE,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fNormalMapImage.Image');
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fNormalMapImage.VulkanImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fNormalMapImage.ImageView');

  fTangentBitangentMapImage:=TpvScene3DRendererImage2D.Create(fPlanet.fVulkanDevice,
                                                              fPlanet.fHeightMapResolution,
                                                              fPlanet.fHeightMapResolution,
                                                              VK_FORMAT_R16G16B16A16_SFLOAT,
                                                              true,
                                                              VK_SAMPLE_COUNT_1_BIT,
                                                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                              ImageSharingMode,
                                                              ImageQueueFamilyIndices);
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fTangentBitangentMapImage.VulkanImage.Handle,VK_OBJECT_TYPE_IMAGE,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fTangentBitangentMapImage.Image');
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fTangentBitangentMapImage.VulkanImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fTangentBitangentMapImage.ImageView');

  if fInFlightFrameIndex<0 then begin

   SetLength(fTileDirtyMap,((fPlanet.fTileMapResolution*fPlanet.fTileMapResolution)+31) shr 5);
   if length(fTileDirtyMap)>0 then begin
    FillChar(fTileDirtyMap[0],length(fTileDirtyMap)*SizeOf(TpvUInt32),#0);
   end;

   SetLength(fTileExpandedDirtyMap,fPlanet.fTileMapResolution*fPlanet.fTileMapResolution);
   if length(fTileExpandedDirtyMap)>0 then begin
    FillChar(fTileExpandedDirtyMap[0],length(fTileExpandedDirtyMap)*SizeOf(TpvUInt32),#0);
   end;

   fTileDirtyMapBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                               (((fPlanet.fTileMapResolution*fPlanet.fTileMapResolution)+31) shr 5)*SizeOf(TpvUInt32),
                                               TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                               VK_SHARING_MODE_EXCLUSIVE,
                                               [],
                                               TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                               TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                               0,
                                               0,
                                               0,
                                               0,
                                               0,
                                               0,
                                               [TpvVulkanBufferFlag.PersistentMappedIfPossibe]
                                              );
   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fTileDirtyMapBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fTileDirtyMapBuffer');
   fPlanet.fVulkanDevice.MemoryStaging.Zero(fPlanet.fVulkanComputeQueue,
                                            fPlanet.fVulkanComputeCommandBuffer,
                                            fPlanet.fVulkanComputeFence,
                                            fTileDirtyMapBuffer,
                                            0,
                                            fTileDirtyMapBuffer.Size);

   fTileExpandedDirtyMapBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                       (((fPlanet.fTileMapResolution*fPlanet.fTileMapResolution)+31) shr 5)*SizeOf(TpvUInt32),
                                                       TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                       VK_SHARING_MODE_EXCLUSIVE,
                                                       [],
                                                       TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                       TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                       0,
                                                       0,
                                                       0,
                                                       0,
                                                       0,
                                                       0,
                                                       [TpvVulkanBufferFlag.PersistentMappedIfPossibe]
                                                      );
   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fTileExpandedDirtyMapBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fTileExpandedDirtyMapBuffer');
   fPlanet.fVulkanDevice.MemoryStaging.Zero(fPlanet.fVulkanComputeQueue,
                                            fPlanet.fVulkanComputeCommandBuffer,
                                            fPlanet.fVulkanComputeFence,
                                            fTileExpandedDirtyMapBuffer,
                                            0,
                                            fTileExpandedDirtyMapBuffer.Size);

   fTileDirtyQueueBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                 ((fPlanet.fTileMapResolution*fPlanet.fTileMapResolution)+6)*SizeOf(TpvUInt32),
                                                 TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT),
                                                 VK_SHARING_MODE_EXCLUSIVE,
                                                 [],
                                                 TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                 TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                 0,
                                                 0,
                                                 0,
                                                 0,
                                                 0,
                                                 0,
                                                 [TpvVulkanBufferFlag.PersistentMappedIfPossibe]
                                                );
   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fTileDirtyQueueBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fTileDirtyQueueBuffer');
   fPlanet.fVulkanDevice.MemoryStaging.Zero(fPlanet.fVulkanComputeQueue,
                                            fPlanet.fVulkanComputeCommandBuffer,
                                            fPlanet.fVulkanComputeFence,
                                            fTileDirtyQueueBuffer,
                                            0,
                                            fTileDirtyQueueBuffer.Size);

   fTiledMeshBoundingBoxesBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                         fPlanet.fTileMapResolution*fPlanet.fTileMapResolution*SizeOf(TpvScene3DPlanet.TData.TTiledMeshBoundingBox),
                                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                         VK_SHARING_MODE_EXCLUSIVE,
                                                         [],
                                                         0,
                                                         TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                         0,
                                                         0,
                                                         0,
                                                         0,
                                                         0,
                                                         0,
                                                         []
                                                        );
   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fTiledMeshBoundingBoxesBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fTiledMeshBoundingBoxesBuffer');

   fTiledMeshBoundingSpheresBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                           fPlanet.fTileMapResolution*fPlanet.fTileMapResolution*SizeOf(TpvScene3DPlanet.TData.TTiledMeshBoundingSphere),
                                                           TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                           VK_SHARING_MODE_EXCLUSIVE,
                                                           [],
                                                           0,
                                                           TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                           0,
                                                           0,
                                                           0,
                                                           0,
                                                           0,
                                                           0,
                                                           []
                                                          );
   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fTiledMeshBoundingSpheresBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fTiledMeshBoundingSpheresBuffer');

  end;

  begin

   // All only-visual buffers doesn't need to be accessible from the CPU, just from the GPU itself
    
  {if fInFlightFrameIndex<0 then}begin

    fVisualMeshVertexBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                    (fPlanet.fTileMapResolution*fPlanet.fTileMapResolution*fPlanet.fVisualTileResolution*fPlanet.fVisualTileResolution)*2*SizeOf(TpvVector4),
                                                    TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                    BufferSharingMode,
                                                    BufferQueueFamilyIndices,
                                                    0,
                                                    TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                    0,
                                                    TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                    0,
                                                    0,
                                                    0,
                                                    0,
                                                    []
                                                   );
    fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVisualMeshVertexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fVisualMeshVertexBuffer');
           
    fVisualMeshIndexBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                   fPlanet.fCountVisualMeshIndices*SizeOf(TpvUInt32),
                                                   TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                   BufferSharingMode,
                                                   BufferQueueFamilyIndices,
                                                   0,
                                                   TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                   0,
                                                   TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                   0,
                                                   0,
                                                   0,
                                                   0,
                                                   []
                                                  );
    fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVisualMeshIndexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fVisualMeshIndexBuffer');

   end;
   
  end;

  if fInFlightFrameIndex<0 then begin

   fPhysicsMeshVertexBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                    fPlanet.fTileMapResolution*fPlanet.fTileMapResolution*fPlanet.fPhysicsTileResolution*fPlanet.fPhysicsTileResolution*2*SizeOf(TpvVector4),
                                                    TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                    BufferSharingMode,
                                                    BufferQueueFamilyIndices,
                                                    TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                    TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                    0,
                                                    TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                    0,
                                                    0,
                                                    0,
                                                    0,
                                                    [TpvVulkanBufferFlag.PersistentMappedIfPossibe]
                                                   );
   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fPhysicsMeshVertexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fPhysicsMeshVertexBuffer');

   fPhysicsMeshIndexBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                   fPlanet.fCountPhysicsMeshIndices*SizeOf(TpvUInt32),
                                                   TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                   BufferSharingMode,
                                                   BufferQueueFamilyIndices,
                                                   0,
                                                   TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                   0,
                                                   0,
                                                   0,
                                                   0,
                                                   0,
                                                   0,
                                                   []
                                                  );
   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fPhysicsMeshIndexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fPhysicsMeshIndexBuffer');

  end;

  if fInFlightFrameIndex<0 then begin

   fRayIntersectionResultBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                        SizeOf(TpvVector4),
                                                        TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                        BufferSharingMode,
                                                        BufferQueueFamilyIndices,
                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                        0,
                                                        0,
                                                        0,
                                                        0,
                                                        0,
                                                        0,
                                                        [TpvVulkanBufferFlag.PersistentMappedIfPossibe]
                                                       );
   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fRayIntersectionResultBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fRayIntersectionResultBuffer');

  end;

 end;

 if aInFlightFrameIndex<0 then begin

  fMeshVertices:=TMeshVertices.Create;
  fMeshVertices.Resize(fPlanet.fTileMapResolution*fPlanet.fTileMapResolution*fPlanet.fPhysicsTileResolution*fPlanet.fPhysicsTileResolution);

  fMeshIndices:=TMeshIndices.Create;
  fMeshIndices.Resize(fPlanet.fTileMapResolution*fPlanet.fTileMapResolution*fPlanet.fPhysicsTileResolution*fPlanet.fPhysicsTileResolution*6);

  fTileDirtyQueueItems:=TTileDirtyQueueItems.Create;
  fTileDirtyQueueItems.Resize((fPlanet.fTileMapResolution*fPlanet.fTileMapResolution)+6);

  fTileGenerations:=nil;
  SetLength(fTileGenerations,fPlanet.fTileMapResolution*fPlanet.fTileMapResolution);
  FillChar(fTileGenerations[0],Length(fTileGenerations)*SizeOf(TpvUInt64),#$ff);

  fTiledMeshBoundingBoxes:=TTiledMeshBoundingBoxes.Create;
  fTiledMeshBoundingBoxes.Resize(fPlanet.fTileMapResolution*fPlanet.fTileMapResolution);

  fTiledMeshBoundingSpheres:=TTiledMeshBoundingSpheres.Create;
  fTiledMeshBoundingSpheres.Resize(fPlanet.fTileMapResolution*fPlanet.fTileMapResolution);

 end else begin

  fMeshVertices:=nil;

  fMeshIndices:=nil;

  fTileDirtyQueueItems:=nil;

  fTileGenerations:=nil;

  fTiledMeshBoundingBoxes:=nil;

  fTiledMeshBoundingSpheres:=nil;

 end;

 fSelectedRegion:=TpvVector4.Null;

 fSelectedRegionProperty:=TpvVector4Property.Create(@fSelectedRegion);

 fModifyHeightMapActive:=false;

 fModifyHeightMapBorderRadius:=0.0;

 fModifyHeightMapFactor:=0.0;

 fWireframeActive:=false;

 fDisplacementMappingActive:=false;

 fParallaxMappingActive:=false;

end;

destructor TpvScene3DPlanet.TData.Destroy;
begin

 fHeightMap:=nil;

 FreeAndNil(fHeightMapImage);

 FreeAndNil(fNormalMapImage);

 FreeAndNil(fTangentBitangentMapImage);

 FreeAndNil(fMeshVertices);

 FreeAndNil(fMeshIndices);

 fTileGenerations:=nil;

 FreeAndNil(fTiledMeshBoundingBoxes);

 FreeAndNil(fTiledMeshBoundingSpheres);

 FreeAndNil(fTileDirtyQueueItems);

 fTileDirtyMap:=nil;

 fTileExpandedDirtyMap:=nil;

 FreeAndNil(fTileDirtyMapBuffer);

 FreeAndNil(fTileExpandedDirtyMapBuffer);

 FreeAndNil(fTileDirtyQueueBuffer);

 FreeAndNil(fTiledMeshBoundingBoxesBuffer);

 FreeAndNil(fTiledMeshBoundingSpheresBuffer);

 FreeAndNil(fVisualMeshVertexBuffer);

 FreeAndNil(fVisualMeshIndexBuffer);

 FreeAndNil(fPhysicsMeshVertexBuffer);

 FreeAndNil(fPhysicsMeshIndexBuffer);

 FreeAndNil(fRayIntersectionResultBuffer);

 FreeAndNil(fSelectedRegionProperty);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.TData.AcquireOnUniversalQueue(const aCommandBuffer:TpvVulkanCommandBuffer);
var ImageSubresourceRange:TVkImageSubresourceRange;
    ImageMemoryBarriers:array[0..2] of TVkImageMemoryBarrier;
    BufferMemoryBarriers:array[0..1] of TVkBufferMemoryBarrier;
begin

 if fOwnershipHolderState=TpvScene3DPlanet.TData.TOwnershipHolderState.ReleasedOnComputeQueue then begin

  if assigned(fPlanet.fVulkanDevice) and
     (fPlanet.fVulkanDevice.ComputeQueueFamilyIndex<>fPlanet.fVulkanDevice.UniversalQueueFamilyIndex) and
     (fPlanet.fInFlightFrameSharingMode=TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE)) then begin

   ImageSubresourceRange:=TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                          0,
                                                          1,
                                                          0,
                                                          1);

   ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(0,
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                        fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                        fHeightMapImage.VulkanImage.Handle,
                                                        TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                        0,
                                                                                        fHeightMapImage.MipMapLevels,
                                                                                        0,
                                                                                        1));

   ImageMemoryBarriers[1]:=TVkImageMemoryBarrier.Create(0,
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                        fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                        fNormalMapImage.VulkanImage.Handle,
                                                        TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                        0,
                                                                                        fNormalMapImage.MipMapLevels,
                                                                                        0,
                                                                                        1));

   ImageMemoryBarriers[2]:=TVkImageMemoryBarrier.Create(0,
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                        fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                        fTangentBitangentMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(0,
                                                          TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                          fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                          fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                          fVisualMeshVertexBuffer.Handle,
                                                          0,
                                                          VK_WHOLE_SIZE);

   BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(0,
                                                          TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                          fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                          fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                          fVisualMeshIndexBuffer.Handle,
                                                          0,
                                                          VK_WHOLE_SIZE);
   
   fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].AcquireOnUniversalQueue',[0.5,0.25,0.25,1.0]);
    
   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or 
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or 
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT) or 
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or 
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_INPUT_BIT),
                                     0,
                                     0,nil,
                                     2,@BufferMemoryBarriers[0],
                                     3,@ImageMemoryBarriers[0]);

   fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

  end;

  fOwnershipHolderState:=TpvScene3DPlanet.TData.TOwnershipHolderState.AcquiredOnUniversalQueue;

 end;

end;

procedure TpvScene3DPlanet.TData.ReleaseOnUniversalQueue(const aCommandBuffer:TpvVulkanCommandBuffer);
var ImageSubresourceRange:TVkImageSubresourceRange;
    ImageMemoryBarriers:array[0..2] of TVkImageMemoryBarrier;
    BufferMemoryBarriers:array[0..1] of TVkBufferMemoryBarrier;
begin

 if fOwnershipHolderState in [TpvScene3DPlanet.TData.TOwnershipHolderState.Uninitialized,TpvScene3DPlanet.TData.TOwnershipHolderState.AcquiredOnUniversalQueue] then begin

  if assigned(fPlanet.fVulkanDevice) and
     (fPlanet.fVulkanDevice.ComputeQueueFamilyIndex<>fPlanet.fVulkanDevice.UniversalQueueFamilyIndex) and
     (fPlanet.fInFlightFrameSharingMode=TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE)) then begin

   ImageSubresourceRange:=TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                          0,
                                                          1,
                                                          0,
                                                          1);

   ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        0,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                        fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                        fHeightMapImage.VulkanImage.Handle,
                                                        TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                        0,
                                                                                        fHeightMapImage.MipMapLevels,
                                                                                        0,
                                                                                        1));

   ImageMemoryBarriers[1]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        0,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                        fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                        fNormalMapImage.VulkanImage.Handle,
                                                        TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                        0,
                                                                                        fNormalMapImage.MipMapLevels,
                                                                                        0,
                                                                                        1));

   ImageMemoryBarriers[2]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        0,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                        fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                        fTangentBitangentMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);      

   BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                          0,
                                                          fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                          fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                          fVisualMeshVertexBuffer.Handle,
                                                          0,
                                                          VK_WHOLE_SIZE);

   BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                          0,
                                                          fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                          fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                          fVisualMeshIndexBuffer.Handle,
                                                          0,
                                                          VK_WHOLE_SIZE); 

   fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].ReleaseOnUniversalQueue',[0.5,0.25,0.25,1.0]);

   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or 
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT) or 
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or 
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_INPUT_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                     0,
                                     0,nil,
                                     2,@BufferMemoryBarriers[0],
                                     3,@ImageMemoryBarriers[0]);    

   fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);       

  end;

  fOwnershipHolderState:=TpvScene3DPlanet.TData.TOwnershipHolderState.ReleasedOnUniversalQueue;

 end;

end;

procedure TpvScene3DPlanet.TData.AcquireOnComputeQueue(const aCommandBuffer:TpvVulkanCommandBuffer);
var ImageSubresourceRange:TVkImageSubresourceRange;
    ImageMemoryBarriers:array[0..2] of TVkImageMemoryBarrier;
    BufferMemoryBarriers:array[0..1] of TVkBufferMemoryBarrier;
begin

 if fOwnershipHolderState=TpvScene3DPlanet.TData.TOwnershipHolderState.ReleasedOnUniversalQueue then begin

  if assigned(fPlanet.fVulkanDevice) and
     (fPlanet.fVulkanDevice.ComputeQueueFamilyIndex<>fPlanet.fVulkanDevice.UniversalQueueFamilyIndex) and
     (fPlanet.fInFlightFrameSharingMode=TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE)) then begin

   ImageSubresourceRange:=TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                          0,
                                                          1,
                                                          0,
                                                          1);

   ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(0,
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                        fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                        fHeightMapImage.VulkanImage.Handle,
                                                        TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                        0,
                                                                                        fHeightMapImage.MipMapLevels,
                                                                                        0,
                                                                                        1));

   ImageMemoryBarriers[1]:=TVkImageMemoryBarrier.Create(0,
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                        fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                        fNormalMapImage.VulkanImage.Handle,
                                                        TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                        0,
                                                                                        fNormalMapImage.MipMapLevels,
                                                                                        0,
                                                                                        1));

   ImageMemoryBarriers[2]:=TVkImageMemoryBarrier.Create(0,
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                        fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                        fTangentBitangentMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(0,
                                                          TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                          fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                          fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                          fVisualMeshVertexBuffer.Handle,
                                                          0,
                                                          VK_WHOLE_SIZE);

   BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(0,
                                                          TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                          fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                          fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                          fVisualMeshIndexBuffer.Handle,
                                                          0,
                                                          VK_WHOLE_SIZE);

   fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].AcquireOnComputeQueue',[0.5,0.25,0.25,1.0]);

   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                     0,
                                     0,nil,
                                     2,@BufferMemoryBarriers[0],
                                     3,@ImageMemoryBarriers[0]);    

   fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);  

  end;

  fOwnershipHolderState:=TpvScene3DPlanet.TData.TOwnershipHolderState.AcquiredOnComputeQueue;

 end;

end;

procedure TpvScene3DPlanet.TData.ReleaseOnComputeQueue(const aCommandBuffer:TpvVulkanCommandBuffer);
var ImageSubresourceRange:TVkImageSubresourceRange;
    ImageMemoryBarriers:array[0..2] of TVkImageMemoryBarrier;
    BufferMemoryBarriers:array[0..1] of TVkBufferMemoryBarrier;
begin

 if fOwnershipHolderState in [TpvScene3DPlanet.TData.TOwnershipHolderState.Uninitialized,TpvScene3DPlanet.TData.TOwnershipHolderState.AcquiredOnComputeQueue] then begin

  if assigned(fPlanet.fVulkanDevice) and
     (fPlanet.fVulkanDevice.ComputeQueueFamilyIndex<>fPlanet.fVulkanDevice.UniversalQueueFamilyIndex) and
     (fPlanet.fInFlightFrameSharingMode=TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE)) then begin

   ImageSubresourceRange:=TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                          0,
                                                          1,
                                                          0,
                                                          1);

   ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        0,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                        fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                        fHeightMapImage.VulkanImage.Handle,
                                                        TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                        0,
                                                                                        fHeightMapImage.MipMapLevels,
                                                                                        0,
                                                                                        1));

   ImageMemoryBarriers[1]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        0,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                        fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                        fNormalMapImage.VulkanImage.Handle,
                                                        TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                        0,
                                                                                        fNormalMapImage.MipMapLevels,
                                                                                        0,
                                                                                        1));

   ImageMemoryBarriers[2]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        0,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                        fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                        fTangentBitangentMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);      

   BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                          0,
                                                          fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                          fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                          fVisualMeshVertexBuffer.Handle,
                                                          0,
                                                          VK_WHOLE_SIZE);

   BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                          0,
                                                          fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                          fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                          fVisualMeshIndexBuffer.Handle,
                                                          0,
                                                          VK_WHOLE_SIZE);   

   fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].ReleaseOnComputeQueue',[0.5,0.25,0.25,1.0]);

   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                     0,
                                     0,nil,
                                     2,@BufferMemoryBarriers[0],
                                     3,@ImageMemoryBarriers[0]);   

   fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

  end;

  fOwnershipHolderState:=TpvScene3DPlanet.TData.TOwnershipHolderState.ReleasedOnComputeQueue;

 end;

end;

procedure TpvScene3DPlanet.TData.CheckDirtyMap;
var Index,OtherIndex,Mask,x,y,ox,oy,ix,iy:TpvInt32;
begin

 if assigned(fPlanet.fVulkanDevice) and assigned(fTileDirtyMapBuffer) and (length(fTileDirtyMap)>0) then begin

  fPlanet.fVulkanDevice.MemoryStaging.Download(fPlanet.fVulkanComputeQueue,
                                               fPlanet.fVulkanComputeCommandBuffer,
                                               fPlanet.fVulkanComputeFence,
                                               fTileDirtyMapBuffer,
                                               0,
                                               fTileDirtyMap[0],
                                               fTileDirtyMapBuffer.Size);

  fPlanet.fVulkanDevice.MemoryStaging.Zero(fPlanet.fVulkanComputeQueue,
                                           fPlanet.fVulkanComputeCommandBuffer,
                                           fPlanet.fVulkanComputeFence,
                                           fTileDirtyMapBuffer,
                                           0,
                                           fTileDirtyMapBuffer.Size);

  // Expand dirty map, so that also adjacent tiles are marked as dirty, since tile edges are shared.
  // Indeed, it can be done more efficiently, but it's not worth the effort, because it's only done at
  // changing the height map, which is not done very often and maximal once per frame ir time step. 
  // The most important thing is that the mesh for the physics engine is updated correctly, because 
  // it's used for collision detection and so on. An expanded dirty map is used here, because it
  // would otherwise be self-overlapping and thus not work correctly, when it would update the dirty
  // map in-place. In other words, it would mark too much tiles as dirty then, which would result in
  // unnecessary work for updating the physics mesh and so on.
  Mask:=fPlanet.fTileMapResolution-1; // Resolution is always power of two here, so we can convert it to a mask easily
  FillChar(fTileExpandedDirtyMap[0],length(fTileExpandedDirtyMap)*SizeOf(TpvUInt32),#0); // Clear expanded dirty map
  for y:=0 to fPlanet.fTileMapResolution-1 do begin
   for x:=0 to fPlanet.fTileMapResolution-1 do begin
    Index:=(y*fPlanet.fTileMapResolution)+x;
    if (fTileDirtyMap[Index shr 5] and (TpvUInt32(1) shl (Index and 31)))<>0 then begin
     for oy:=-1 to 1 do begin
      for ox:=-1 to 1 do begin
       ix:=x+ox;
       iy:=y+oy;
       if (((abs(ix)+(TpvInt32(TpvUInt32(ix) shr 31) and 1)) and 1) xor ((abs(iy)+(TpvInt32(TpvUInt32(iy) shr 31) and 1)) and 1))<>0 then begin
        // Octahedral wrap, here the coordinates must be mirrored in a checkerboard pattern at overflows
        ix:=fPlanet.fTileMapResolution-(((ix+fPlanet.fTileMapResolution) and Mask)+1);
        iy:=fPlanet.fTileMapResolution-(((iy+fPlanet.fTileMapResolution) and Mask)+1);
       end else begin 
        ix:=(ix+fPlanet.fTileMapResolution) and Mask;
        iy:=(iy+fPlanet.fTileMapResolution) and Mask;
       end;                 
       OtherIndex:=(iy*fPlanet.fTileMapResolution)+ix;
       fTileExpandedDirtyMap[OtherIndex shr 5]:=fTileExpandedDirtyMap[OtherIndex shr 5] or (TpvUInt32(1) shl (OtherIndex and 31));
      end;
     end;     
    end; 
   end;
  end;

 {// Dump dirty map 
  for Index:=0 to length(fTileExpandedDirtyMap)-1 do begin
   if fTileExpandedDirtyMap[Index]<>0 then begin
    writeln('DirtyMap['+IntToStr(Index)+']='+IntToHex(fTileExpandedDirtyMap[Index],8));
   end;
  end;//}

 end;

end;

procedure TpvScene3DPlanet.TData.TransferTo(const aCommandBuffer:TpvVulkanCommandBuffer;
                                            const aInFlightFrameData:TData);
var MipMapIndex:TpvSizeInt;
    ImageSubresourceRange:TVkImageSubresourceRange;
    ImageMemoryBarriers:array[0..5] of TVkImageMemoryBarrier;
    BufferMemoryBarriers:array[0..3] of TVkBufferMemoryBarrier;
    ImageCopies:array[0..31] of TVkImageCopy;
    ImageCopy:PVkImageCopy;
    BufferCopy:TVkBufferCopy;
begin
  
 if assigned(fPlanet.fVulkanDevice) then begin

  fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet TransferTo',[0.25,0.25,0.5,1.0]);

  ////////////////////////////

  ImageSubresourceRange:=TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                         0,
                                                         1,
                                                         0,
                                                         1);

  ////////////////////////////
 
  begin                                                      

   ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fHeightMapImage.VulkanImage.Handle,
                                                        TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                        0,
                                                                                        fHeightMapImage.MipMapLevels,
                                                                                        0,
                                                                                        1));

   ImageMemoryBarriers[1]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fNormalMapImage.VulkanImage.Handle,
                                                        TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                        0,
                                                                                        fNormalMapImage.MipMapLevels,
                                                                                        0,
                                                                                        1));

   ImageMemoryBarriers[2]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fTangentBitangentMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange); 

   ImageMemoryBarriers[3]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        aInFlightFrameData.fHeightMapImage.VulkanImage.Handle,
                                                        TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                        0,
                                                                                        aInFlightFrameData.fHeightMapImage.MipMapLevels,
                                                                                        0,
                                                                                        1));

   ImageMemoryBarriers[4]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        aInFlightFrameData.fNormalMapImage.VulkanImage.Handle,
                                                        TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                        0,
                                                                                        aInFlightFrameData.fNormalMapImage.MipMapLevels,
                                                                                        0,
                                                                                        1));

   ImageMemoryBarriers[5]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        aInFlightFrameData.fTangentBitangentMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);          

   BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                          TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          fVisualMeshVertexBuffer.Handle,
                                                          0,
                                                          VK_WHOLE_SIZE);

   BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                          TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          fVisualMeshIndexBuffer.Handle,
                                                          0,
                                                          VK_WHOLE_SIZE);

   BufferMemoryBarriers[2]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                          TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          aInFlightFrameData.fVisualMeshVertexBuffer.Handle,
                                                          0,
                                                          VK_WHOLE_SIZE);

   BufferMemoryBarriers[3]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                          TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          aInFlightFrameData.fVisualMeshIndexBuffer.Handle,
                                                          0,
                                                          VK_WHOLE_SIZE);    

   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                     0,
                                     0,nil,
                                     4,@BufferMemoryBarriers[0],
                                     6,@ImageMemoryBarriers[0]);                                                 

  end;   
 
  //////////////////////////// 

  begin
    
   FillChar(ImageCopies,length(ImageCopies)*SizeOf(TVkImageCopy),#0);
   for MipMapIndex:=0 to fHeightMapImage.MipMapLevels-1 do begin
    ImageCopy:=@ImageCopies[MipMapIndex];
    ImageCopy^.srcSubresource.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
    ImageCopy^.srcSubresource.mipLevel:=MipMapIndex;
    ImageCopy^.srcSubresource.baseArrayLayer:=0;
    ImageCopy^.srcSubresource.layerCount:=1;
    ImageCopy^.srcOffset.x:=0;
    ImageCopy^.srcOffset.y:=0;
    ImageCopy^.srcOffset.z:=0;
    ImageCopy^.dstSubresource.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
    ImageCopy^.dstSubresource.mipLevel:=MipMapIndex;
    ImageCopy^.dstSubresource.baseArrayLayer:=0;
    ImageCopy^.dstSubresource.layerCount:=1;
    ImageCopy^.dstOffset.x:=0;
    ImageCopy^.dstOffset.y:=0;
    ImageCopy^.dstOffset.z:=0;
    ImageCopy^.extent.width:=fPlanet.fHeightMapResolution shr MipMapIndex;
    ImageCopy^.extent.height:=fPlanet.fHeightMapResolution shr MipMapIndex;
    ImageCopy^.extent.depth:=1;
   end;

   aCommandBuffer.CmdCopyImage(fHeightMapImage.VulkanImage.Handle,
                               VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                               aInFlightFrameData.fHeightMapImage.VulkanImage.Handle,
                               VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                               fHeightMapImage.MipMapLevels,@ImageCopies[0]);

   aCommandBuffer.CmdCopyImage(fNormalMapImage.VulkanImage.Handle,
                               VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                               aInFlightFrameData.fNormalMapImage.VulkanImage.Handle,
                               VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                               fNormalMapImage.MipMapLevels,@ImageCopies[0]);

   aCommandBuffer.CmdCopyImage(fTangentBitangentMapImage.VulkanImage.Handle,
                               VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                               aInFlightFrameData.fTangentBitangentMapImage.VulkanImage.Handle,
                               VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                               1,@ImageCopies[0]);

   BufferCopy.srcOffset:=0;
   BufferCopy.dstOffset:=0;
   BufferCopy.size:=fVisualMeshVertexBuffer.Size;
   aCommandBuffer.CmdCopyBuffer(fVisualMeshVertexBuffer.Handle,
                                aInFlightFrameData.fVisualMeshVertexBuffer.Handle,
                                1,
                                @BufferCopy);

   BufferCopy.srcOffset:=0;
   BufferCopy.dstOffset:=0;
   BufferCopy.size:=fVisualMeshIndexBuffer.Size;
   aCommandBuffer.CmdCopyBuffer(fVisualMeshIndexBuffer.Handle,
                                aInFlightFrameData.fVisualMeshIndexBuffer.Handle,
                                1,
                                @BufferCopy);

  end;

  //////////////////////////// 

  begin

   ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fHeightMapImage.VulkanImage.Handle,
                                                        TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                        0,
                                                                                        fHeightMapImage.MipMapLevels,
                                                                                        0,
                                                                                        1));

   ImageMemoryBarriers[1]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fNormalMapImage.VulkanImage.Handle,
                                                        TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                        0,
                                                                                        fNormalMapImage.MipMapLevels,
                                                                                        0,
                                                                                        1));

   ImageMemoryBarriers[2]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fTangentBitangentMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[3]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        aInFlightFrameData.fHeightMapImage.VulkanImage.Handle,
                                                        TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                        0,
                                                                                        aInFlightFrameData.fHeightMapImage.MipMapLevels,
                                                                                        0,
                                                                                        1));

   ImageMemoryBarriers[4]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        aInFlightFrameData.fNormalMapImage.VulkanImage.Handle,
                                                        TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                        0,
                                                                                        aInFlightFrameData.fNormalMapImage.MipMapLevels,
                                                                                        0,
                                                                                        1));

   ImageMemoryBarriers[5]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        aInFlightFrameData.fTangentBitangentMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                          TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          fVisualMeshVertexBuffer.Handle,
                                                          0,
                                                          VK_WHOLE_SIZE);

   BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                          TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          fVisualMeshIndexBuffer.Handle,
                                                          0,
                                                          VK_WHOLE_SIZE);

   BufferMemoryBarriers[2]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                          TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          aInFlightFrameData.fVisualMeshVertexBuffer.Handle,
                                                          0,
                                                          VK_WHOLE_SIZE);                                                       

   BufferMemoryBarriers[3]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                          TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          aInFlightFrameData.fVisualMeshIndexBuffer.Handle,
                                                          0,
                                                          VK_WHOLE_SIZE);  

   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                     0,
                                     0,
                                     nil,
                                     4,@BufferMemoryBarriers[0],
                                     6,@ImageMemoryBarriers[0]);
      
  end;

  ////////////////////////////

  fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

 end;

 aInFlightFrameData.fModelMatrix:=fModelMatrix;

 aInFlightFrameData.fReady:=fReady;

end;

procedure TpvScene3DPlanet.TData.Assign(const aData:TData);
begin
 fSelectedRegion:=aData.fSelectedRegion;
 fWireframeActive:=aData.fWireframeActive;
 fDisplacementMappingActive:=aData.fDisplacementMappingActive;
 fParallaxMappingActive:=aData.fParallaxMappingActive;
end;

{ TpvScene3DPlanet.THeightMapRandomInitialization }

constructor TpvScene3DPlanet.THeightMapRandomInitialization.Create(const aPlanet:TpvScene3DPlanet);
var Stream:TStream;
begin
  
 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=fPlanet.fVulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_heightmap_random_initialization_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.THeightMapRandomInitialization.fComputeShaderModule');

  fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.AddBinding(1,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.Initialize;

  fPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
  fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
  fPipelineLayout.Initialize;

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),1);
  fDescriptorPool.Initialize;

  fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  fDescriptorSet.WriteToDescriptorSet(0,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                      [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                     fPlanet.fData.fHeightMapImage.VulkanImageViews[0].Handle,
                                                                     VK_IMAGE_LAYOUT_GENERAL)],
                                      [],
                                      [],
                                      false);
  fDescriptorSet.WriteToDescriptorSet(1,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                      [],
                                      [TVkDescriptorBufferInfo.Create(fPlanet.fData.fTileDirtyMapBuffer.Handle,
                                                                      0,
                                                                      VK_WHOLE_SIZE)],
                                      [],
                                      false);
  fDescriptorSet.Flush;

  fPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                             pvApplication.VulkanPipelineCache,
                                             TVkPipelineCreateFlags(0),
                                             fComputeShaderStage,
                                             fPipelineLayout,
                                             nil,
                                             0);

  fPushConstants.Octaves:=4;
  fPushConstants.Scale:=4.0;
  fPushConstants.Amplitude:=1.0;
  fPushConstants.Lacunarity:=2.0;
  fPushConstants.Gain:=0.5;
  fPushConstants.Factor:=0.5;
  fPushConstants.MinHeight:=0.0;
  fPushConstants.MaxHeight:=1.0;
  fPushConstants.BottomRadius:=fPlanet.BottomRadius;
  fPushConstants.TopRadius:=fPlanet.TopRadius;
  fPushConstants.TileMapResolution:=fPlanet.fTileMapResolution;
  fPushConstants.TileMapShift:=fPlanet.fTileMapShift;

 end;

end;

destructor TpvScene3DPlanet.THeightMapRandomInitialization.Destroy;
begin
 
 FreeAndNil(fPipeline);

 FreeAndNil(fDescriptorSet);

 FreeAndNil(fDescriptorPool);

 FreeAndNil(fPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);
 
 FreeAndNil(fComputeShaderStage);

 FreeAndNil(fComputeShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.THeightMapRandomInitialization.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var ImageMemoryBarrier:TVkImageMemoryBarrier;
    BufferMemoryBarrier:TVkBufferMemoryBarrier; 
begin

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet HeightMapRandomInitialization',[0.25,0.5,0.5,1.0]);

 ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_IMAGE_LAYOUT_GENERAL,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  fPlanet.fData.fHeightMapImage.VulkanImage.Handle,
                                                  TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                  0,
                                                                                  1,
                                                                                  0,
                                                                                  1));

 BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_HOST_READ_BIT) or TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT),
                                                    TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    fPlanet.fData.fTileDirtyMapBuffer.Handle,
                                                    0,
                                                    VK_WHOLE_SIZE);

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   1,@BufferMemoryBarrier,
                                   1,@ImageMemoryBarrier); 

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                      fPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fDescriptorSet.Handle,
                                      0,
                                      nil);

 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 0,
                                 SizeOf(TPushConstants),
                                 @fPushConstants);

 aCommandBuffer.CmdDispatch((fPlanet.fHeightMapResolution+15) shr 4,
                            (fPlanet.fHeightMapResolution+15) shr 4,
                            1);                            

 ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  VK_IMAGE_LAYOUT_GENERAL,
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  fPlanet.fData.fHeightMapImage.VulkanImage.Handle,
                                                  TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                 0,
                                                                                 1,
                                                                                 0,
                                                                                 1));

 BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_HOST_READ_BIT) or TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT),
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    fPlanet.fData.fTileDirtyMapBuffer.Handle,
                                                    0,
                                                    VK_WHOLE_SIZE);

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                   0,
                                   0,nil,
                                   1,@BufferMemoryBarrier,
                                   1,@ImageMemoryBarrier);                                                                                                                                                                                                 

 inc(fPlanet.fData.fHeightMapGeneration);

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

end;

{ TpvScene3DPlanet.THeightMapModification }

constructor TpvScene3DPlanet.THeightMapModification.Create(const aPlanet:TpvScene3DPlanet);
var Stream:TStream;
begin
  
 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=fPlanet.fVulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_heightmap_modification_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.THeightMapModification.fComputeShaderModule');

  fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.AddBinding(1,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.Initialize;

  fPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
  fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
  fPipelineLayout.Initialize;

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),1);
  fDescriptorPool.Initialize;

  fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  fDescriptorSet.WriteToDescriptorSet(0,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                      [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                     fPlanet.fData.fHeightMapImage.VulkanImageViews[0].Handle,
                                                                     VK_IMAGE_LAYOUT_GENERAL)],
                                      [],
                                      [],
                                      false);
  fDescriptorSet.WriteToDescriptorSet(1,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                      [],
                                      [TVkDescriptorBufferInfo.Create(fPlanet.fData.fTileDirtyMapBuffer.Handle,
                                                                      0,
                                                                      VK_WHOLE_SIZE)],
                                      [],
                                      false);                                    
  fDescriptorSet.Flush;

  fPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                             pvApplication.VulkanPipelineCache,
                                             TVkPipelineCreateFlags(0),
                                             fComputeShaderStage,
                                             fPipelineLayout,
                                             nil,
                                             0);

  fPushConstants.PositionRadius:=TpvVector4.Create(0.0,0.0,0.0,0.0);
  fPushConstants.InnerRadiusValueMinMax:=TpvVector4.Create(0.0,0.0,0.0,0.0);
  
  fPushConstants.TileMapResolution:=fPlanet.fTileMapResolution;
  fPushConstants.TileMapShift:=fPlanet.fTileMapShift;

 end;

end;

destructor TpvScene3DPlanet.THeightMapModification.Destroy;
begin
 
 FreeAndNil(fPipeline);

 FreeAndNil(fDescriptorSet);

 FreeAndNil(fDescriptorPool);

 FreeAndNil(fPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);
 
 FreeAndNil(fComputeShaderStage);

 FreeAndNil(fComputeShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.THeightMapModification.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var ImageMemoryBarrier:TVkImageMemoryBarrier;
    BufferMemoryBarrier:TVkBufferMemoryBarrier;
begin

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet HeightMapModification',[0.5,0.5,0.5,1.0]);

 begin

  BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_HOST_READ_BIT) or TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT),
                                                     TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                     VK_QUEUE_FAMILY_IGNORED,
                                                     VK_QUEUE_FAMILY_IGNORED,
                                                     fPlanet.fData.fTileDirtyMapBuffer.Handle,
                                                     0,
                                                     VK_WHOLE_SIZE);

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                    0,
                                    0,nil,
                                    1,@BufferMemoryBarrier,
                                    0,nil);

  aCommandBuffer.CmdFillBuffer(fPlanet.fData.fTileDirtyMapBuffer.Handle,
                               0,
                               fPlanet.fData.fTileDirtyMapBuffer.Size,
                               0);

  BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                     TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_HOST_READ_BIT) or TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT),
                                                     VK_QUEUE_FAMILY_IGNORED,
                                                     VK_QUEUE_FAMILY_IGNORED,
                                                     fPlanet.fData.fTileDirtyMapBuffer.Handle,
                                                     0,
                                                     VK_WHOLE_SIZE);

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                    0,
                                    0,nil,
                                    1,@BufferMemoryBarrier,
                                    0,nil);

 end;

 ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_IMAGE_LAYOUT_GENERAL,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  fPlanet.fData.fHeightMapImage.VulkanImage.Handle,
                                                  TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                  0,
                                                                                  1,
                                                                                  0,
                                                                                  1));

 BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_HOST_READ_BIT) or TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT),
                                                    TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    fPlanet.fData.fTileDirtyMapBuffer.Handle,
                                                    0,
                                                    VK_WHOLE_SIZE);

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   1,@BufferMemoryBarrier,
                                   1,@ImageMemoryBarrier);

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                      fPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fDescriptorSet.Handle,
                                      0,
                                      nil);

 fPushConstants.InnerRadiusValueMinMax:=TpvVector4.InlineableCreate(Max(0.0,fPlanet.fData.fSelectedRegion.w-fPlanet.fData.fModifyHeightMapBorderRadius),
                                                                    fPlanet.fData.fModifyHeightMapFactor,
                                                                    0.0,
                                                                    1.0);

 fPushConstants.PositionRadius:=fPlanet.fData.fSelectedRegion;

 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 0,
                                 SizeOf(TPushConstants),
                                 @fPushConstants);

 aCommandBuffer.CmdDispatch((fPlanet.fHeightMapResolution+15) shr 4,
                            (fPlanet.fHeightMapResolution+15) shr 4,
                            1);

 ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  VK_IMAGE_LAYOUT_GENERAL,
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  fPlanet.fData.fHeightMapImage.VulkanImage.Handle,
                                                  TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                  0,
                                                                                  1,
                                                                                  0,
                                                                                  1));

 BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_HOST_READ_BIT) or TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT),
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    fPlanet.fData.fTileDirtyMapBuffer.Handle,
                                                    0,
                                                    VK_WHOLE_SIZE);                                                                                 

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                   0,
                                   0,nil,
                                   1,@BufferMemoryBarrier,
                                   1,@ImageMemoryBarrier);

 inc(fPlanet.fData.fHeightMapGeneration);

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

end;

{ TpvScene3DPlanet.THeightMapFlatten }

constructor TpvScene3DPlanet.THeightMapFlatten.Create(const aPlanet:TpvScene3DPlanet);
var Stream:TStream;
begin
  
 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=fPlanet.fVulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_heightmap_flatten_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.THeightMapFlatten.fComputeShaderModule');

  fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.AddBinding(1,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.Initialize;

  fPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
  fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
  fPipelineLayout.Initialize;

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),1);
  fDescriptorPool.Initialize;

  fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  fDescriptorSet.WriteToDescriptorSet(0,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                      [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                     fPlanet.fData.fHeightMapImage.VulkanImageViews[0].Handle,
                                                                     VK_IMAGE_LAYOUT_GENERAL)],
                                      [],
                                      [],
                                      false);
  fDescriptorSet.WriteToDescriptorSet(1,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                      [],
                                      [TVkDescriptorBufferInfo.Create(fPlanet.fData.fTileDirtyMapBuffer.Handle,
                                                                      0,
                                                                      VK_WHOLE_SIZE)],
                                      [],
                                      false);
  fDescriptorSet.Flush;

  fPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                             pvApplication.VulkanPipelineCache,
                                             TVkPipelineCreateFlags(0),
                                             fComputeShaderStage,
                                             fPipelineLayout,
                                             nil,
                                             0);

  fPushConstants.Vector:=TpvVector4.Create(0.0,1.0,0.0,0.0);

  fPushConstants.TileMapResolution:=fPlanet.fTileMapResolution;
  fPushConstants.TileMapShift:=fPlanet.fTileMapShift;

 end;

end;

destructor TpvScene3DPlanet.THeightMapFlatten.Destroy;
begin
 
 FreeAndNil(fPipeline);

 FreeAndNil(fDescriptorSet);

 FreeAndNil(fDescriptorPool);

 FreeAndNil(fPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);
 
 FreeAndNil(fComputeShaderStage);

 FreeAndNil(fComputeShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.THeightMapFlatten.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var ImageMemoryBarrier:TVkImageMemoryBarrier;
    BufferMemoryBarrier:TVkBufferMemoryBarrier;
begin

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet HeightMapFlatten',[0.5,0.25,0.5,1.0]);

 ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_IMAGE_LAYOUT_GENERAL,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  fPlanet.fData.fHeightMapImage.VulkanImage.Handle,
                                                  TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                  0,
                                                                                  1,
                                                                                  0,
                                                                                  1));

 BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_HOST_READ_BIT) or TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT),
                                                    TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    fPlanet.fData.fTileDirtyMapBuffer.Handle,
                                                    0,
                                                    VK_WHOLE_SIZE);    

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   1,@BufferMemoryBarrier,
                                   1,@ImageMemoryBarrier); 

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                      fPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fDescriptorSet.Handle,
                                      0,
                                      nil);

 fPushConstants.MinHeight:=0.0;
 fPushConstants.MaxHeight:=1.0;
 fPushConstants.BottomRadius:=fPlanet.fBottomRadius;
 fPushConstants.TopRadius:=fPlanet.fTopRadius;

 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 0,
                                 SizeOf(TPushConstants),
                                 @fPushConstants);

 aCommandBuffer.CmdDispatch((fPlanet.fHeightMapResolution+15) shr 4,
                            (fPlanet.fHeightMapResolution+15) shr 4,
                            1);

 ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  VK_IMAGE_LAYOUT_GENERAL,
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  fPlanet.fData.fHeightMapImage.VulkanImage.Handle,
                                                  TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                  0,
                                                                                  1,
                                                                                  0,
                                                                                  1));

 BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_HOST_READ_BIT) or TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT),
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    fPlanet.fData.fTileDirtyMapBuffer.Handle,
                                                    0,
                                                    VK_WHOLE_SIZE);                                                                                 

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                   0,
                                   0,nil,
                                   1,@BufferMemoryBarrier,
                                   1,@ImageMemoryBarrier);

 inc(fPlanet.fData.fHeightMapGeneration);

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

end;

{ TpvScene3DPlanet.TTiledMeshBoundingVolumesGeneration }

constructor TpvScene3DPlanet.TTiledMeshBoundingVolumesGeneration.Create(const aPlanet:TpvScene3DPlanet);
var Stream:TStream;
begin

 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=fPlanet.fVulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_tiled_mesh_boundingvolumes_generation_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TTiledMeshBoundingVolumesGeneration.fComputeShaderModule');

  fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.AddBinding(1,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.Initialize;

  fPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
  fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
  fPipelineLayout.Initialize;

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),2);
  fDescriptorPool.Initialize;

  fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  fDescriptorSet.WriteToDescriptorSet(0,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                      [],
                                      [TVkDescriptorBufferInfo.Create(fPlanet.fData.fTiledMeshBoundingBoxesBuffer.Handle,
                                                                      0,
                                                                      VK_WHOLE_SIZE)],
                                      [],
                                      false);
  fDescriptorSet.WriteToDescriptorSet(1,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                      [],
                                      [TVkDescriptorBufferInfo.Create(fPlanet.fData.fTiledMeshBoundingSpheresBuffer.Handle,
                                                                      0,
                                                                      VK_WHOLE_SIZE)],
                                      [],
                                      false);
  fDescriptorSet.Flush;

  fPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                             pvApplication.VulkanPipelineCache,
                                             TVkPipelineCreateFlags(0),
                                             fComputeShaderStage,
                                             fPipelineLayout,
                                             nil,
                                             0);

  fPushConstants.BottomRadius:=fPlanet.fBottomRadius;
  fPushConstants.TopRadius:=fPlanet.fTopRadius;
  fPushConstants.TileMapResolution:=fPlanet.fTileMapResolution;

 end;

end;

destructor TpvScene3DPlanet.TTiledMeshBoundingVolumesGeneration.Destroy;
begin

 FreeAndNil(fPipeline);

 FreeAndNil(fDescriptorSet);

 FreeAndNil(fDescriptorPool);

 FreeAndNil(fPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);

 FreeAndNil(fComputeShaderStage);

 FreeAndNil(fComputeShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.TTiledMeshBoundingVolumesGeneration.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var BufferMemoryBarriers:array[0..1] of TVkBufferMemoryBarrier;
begin

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet TiledMeshBoundingSpheresGeneration',[0.5,0.5,0.5,1.0]);

 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fTiledMeshBoundingBoxesBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fTiledMeshBoundingSpheresBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   2,@BufferMemoryBarriers[0],
                                   0,nil);

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                      fPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fDescriptorSet.Handle,
                                      0,
                                      nil);

 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 0,
                                 SizeOf(TPushConstants),
                                 @fPushConstants);

 aCommandBuffer.CmdDispatch(((fPlanet.fTileMapResolution*fPlanet.fTileMapResolution)+255) shr 8,
                            1,
                            1);

 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_HOST_READ_BIT) or TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fTiledMeshBoundingBoxesBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_HOST_READ_BIT) or TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fTiledMeshBoundingSpheresBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                   0,
                                   0,nil,
                                   2,@BufferMemoryBarriers[0],
                                   0,nil);                                      

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

end;

{ TpvScene3DPlanet.TTileDirtyExpansion }

constructor TpvScene3DPlanet.TTileDirtyExpansion.Create(const aPlanet:TpvScene3DPlanet);
var Stream:TStream;
begin

 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=fPlanet.fVulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_tiles_dirty_expansion_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TTileDirtyExpansion.fComputeShaderModule');

  fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.AddBinding(1,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.Initialize;

  fPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
  fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
  fPipelineLayout.Initialize;

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),2);
  fDescriptorPool.Initialize;

  fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  fDescriptorSet.WriteToDescriptorSet(0,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                      [],
                                      [TVkDescriptorBufferInfo.Create(fPlanet.fData.fTileDirtyMapBuffer.Handle,
                                                                      0,
                                                                      VK_WHOLE_SIZE)],
                                      [],
                                      false);
  fDescriptorSet.WriteToDescriptorSet(1,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                      [],
                                      [TVkDescriptorBufferInfo.Create(fPlanet.fData.fTileExpandedDirtyMapBuffer.Handle,
                                                                      0,
                                                                      VK_WHOLE_SIZE)],
                                      [],
                                      false);
  fDescriptorSet.Flush;

  fPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                             pvApplication.VulkanPipelineCache,
                                             TVkPipelineCreateFlags(0),
                                             fComputeShaderStage,
                                             fPipelineLayout,
                                             nil,
                                             0);

  fPushConstants.TileMapResolution:=fPlanet.fTileMapResolution;

 end;

end;

destructor TpvScene3DPlanet.TTileDirtyExpansion.Destroy;
begin

 FreeAndNil(fPipeline);

 FreeAndNil(fDescriptorSet);

 FreeAndNil(fDescriptorPool);

 FreeAndNil(fPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);

 FreeAndNil(fComputeShaderStage);

 FreeAndNil(fComputeShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.TTileDirtyExpansion.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var BufferMemoryBarriers:array[0..1] of TVkBufferMemoryBarrier;
    BufferCopy:TVkBufferCopy;
begin

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet TileDirtyExpansion',[0.5,0.5,0.25,1.0]);

 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fTileDirtyMapBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fTileExpandedDirtyMapBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   0,
                                   0,nil,
                                   2,@BufferMemoryBarriers[0],
                                   0,nil);

 BufferCopy:=TVkBufferCopy.Create(0,0,fPlanet.fData.fTileDirtyMapBuffer.Size);

 aCommandBuffer.CmdCopyBuffer(fPlanet.fData.fTileDirtyMapBuffer.Handle,
                              fPlanet.fData.fTileExpandedDirtyMapBuffer.Handle,
                              1,@BufferCopy);

 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fTileDirtyMapBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fTileExpandedDirtyMapBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   2,@BufferMemoryBarriers[0],
                                   0,nil);

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                      fPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fDescriptorSet.Handle,
                                      0,
                                      nil);

 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 0,
                                 SizeOf(TPushConstants),
                                 @fPushConstants);

 aCommandBuffer.CmdDispatch((fPlanet.fTileMapResolution+15) shr 4,
                            (fPlanet.fTileMapResolution+15) shr 4,
                            1);

 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fTileDirtyMapBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fTileExpandedDirtyMapBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   0,
                                   0,nil,
                                   2,@BufferMemoryBarriers[0],
                                   0,nil);

 aCommandBuffer.CmdFillBuffer(fPlanet.fData.fTileDirtyMapBuffer.Handle,
                              0,
                              VK_WHOLE_SIZE,
                              0);

 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fTileDirtyMapBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   1,@BufferMemoryBarriers[0],
                                   0,nil);

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

end;

{ TpvScene3DPlanet.TTileDirtyQueueGeneration }

constructor TpvScene3DPlanet.TTileDirtyQueueGeneration.Create(const aPlanet:TpvScene3DPlanet);
var Stream:TStream;
begin

 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=fPlanet.fVulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_tiles_dirty_queue_generation_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TTileDirtyQueueGeneration.fComputeShaderModule');

  fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.AddBinding(1,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.Initialize;

  fPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
  fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
  fPipelineLayout.Initialize;

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),2);
  fDescriptorPool.Initialize;

  fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  fDescriptorSet.WriteToDescriptorSet(0,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                      [],
                                      [TVkDescriptorBufferInfo.Create(fPlanet.fData.fTileExpandedDirtyMapBuffer.Handle,
                                                                      0,
                                                                      VK_WHOLE_SIZE)],
                                      [],
                                      false);
  fDescriptorSet.WriteToDescriptorSet(1,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                      [],
                                      [TVkDescriptorBufferInfo.Create(fPlanet.fData.fTileDirtyQueueBuffer.Handle,
                                                                      0,
                                                                      VK_WHOLE_SIZE)],
                                      [],
                                      false);
  fDescriptorSet.Flush;

  fPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                             pvApplication.VulkanPipelineCache,
                                             TVkPipelineCreateFlags(0),
                                             fComputeShaderStage,
                                             fPipelineLayout,
                                             nil,
                                             0);

  fPushConstants.TileMapResolution:=fPlanet.fTileMapResolution;
  fPushConstants.VisualTileResolution:=fPlanet.fVisualTileResolution;
  fPushConstants.PhysicsTileResolution:=fPlanet.fPhysicsTileResolution;

 end;

end; 

destructor TpvScene3DPlanet.TTileDirtyQueueGeneration.Destroy;
begin

 FreeAndNil(fPipeline);

 FreeAndNil(fDescriptorSet);

 FreeAndNil(fDescriptorPool);

 FreeAndNil(fPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);

 FreeAndNil(fComputeShaderStage);

 FreeAndNil(fComputeShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.TTileDirtyQueueGeneration.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var BufferMemoryBarriers:array[0..1] of TVkBufferMemoryBarrier;
    BufferCopy:TVkBufferCopy;
begin

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet TileDirtyQueueGeneration',[0.25,0.5,0.25,1.0]);

 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fTileExpandedDirtyMapBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fTileDirtyQueueBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   0,
                                   0,nil,
                                   2,@BufferMemoryBarriers[0],
                                   0,nil);

 // Not the whole buffer, but only the first two VkDispatchIndirectCommand's without the payload
 // data after the first two VkDispatchIndirectCommand's
 aCommandBuffer.CmdFillBuffer(fPlanet.fData.fTileDirtyQueueBuffer.Handle,
                              0,
                              SizeOf(TVkDispatchIndirectCommand)*2,
                              0);

 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fTileDirtyQueueBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   1,@BufferMemoryBarriers[0],
                                   0,nil);

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                      fPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fDescriptorSet.Handle,
                                      0,
                                      nil);

 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 0,
                                 SizeOf(TPushConstants),
                                 @fPushConstants);     

 aCommandBuffer.CmdDispatch((fPlanet.fTileMapResolution+15) shr 4,
                            (fPlanet.fTileMapResolution+15) shr 4,
                            1); 

 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fTileExpandedDirtyMapBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fTileDirtyQueueBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);       

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   0,
                                   0,nil,
                                   2,@BufferMemoryBarriers[0],
                                   0,nil);

 aCommandBuffer.CmdFillBuffer(fPlanet.fData.fTileExpandedDirtyMapBuffer.Handle,
                              0,
                              VK_WHOLE_SIZE,
                              0); 

 BufferCopy:=TVkBufferCopy.Create(SizeOf(TpvUInt32)*1,SizeOf(TpvUInt32)*4,SizeOf(TpvUInt32));

 aCommandBuffer.CmdCopyBuffer(fPlanet.fData.fTileDirtyQueueBuffer.Handle,
                              fPlanet.fData.fTileDirtyQueueBuffer.Handle,
                              1,@BufferCopy);

 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fTileExpandedDirtyMapBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 if fVulkanDevice.PhysicalDevice.RenderDocDetected then begin

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT) or TVkAccessFlags(VK_ACCESS_HOST_READ_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fTileDirtyQueueBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                    0,
                                    0,nil,
                                    2,@BufferMemoryBarriers[0],
                                    0,nil);

 end else begin

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fTileDirtyQueueBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT),
                                    0,
                                    0,nil,
                                    2,@BufferMemoryBarriers[0],
                                    0,nil);

 end;

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

end;

{ TpvScene3DPlanet.TNormalMapGeneration }

constructor TpvScene3DPlanet.TNormalMapGeneration.Create(const aPlanet:TpvScene3DPlanet);
var Stream:TStream;
begin
  
 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=fPlanet.fVulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_normalmap_generation_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TNormalMapGeneration.fComputeShaderModule');

  fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.AddBinding(1,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.Initialize;

  fPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
  fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
  fPipelineLayout.Initialize;

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),2);
  fDescriptorPool.Initialize;

  fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  fDescriptorSet.WriteToDescriptorSet(0,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                      [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                     fPlanet.fData.fHeightMapImage.VulkanImageViews[0].Handle,
                                                                     VK_IMAGE_LAYOUT_GENERAL)],
                                      [],
                                      [],
                                      false);
  fDescriptorSet.WriteToDescriptorSet(1,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                      [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                     fPlanet.fData.fNormalMapImage.VulkanImageViews[0].Handle,
                                                                     VK_IMAGE_LAYOUT_GENERAL)],
                                      [],
                                      [],
                                      false);
  fDescriptorSet.Flush;

  fPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                             pvApplication.VulkanPipelineCache,
                                             TVkPipelineCreateFlags(0),
                                             fComputeShaderStage,
                                             fPipelineLayout,
                                             nil,
                                             0);

  fPushConstants.PlanetGroundRadius:=fPlanet.fBottomRadius;
  fPushConstants.HeightMapScale:=fPlanet.fHeightMapScale;

 end;

end;

destructor TpvScene3DPlanet.TNormalMapGeneration.Destroy;
begin
 
 FreeAndNil(fPipeline);

 FreeAndNil(fDescriptorSet);

 FreeAndNil(fDescriptorPool);

 FreeAndNil(fPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);
 
 FreeAndNil(fComputeShaderStage);

 FreeAndNil(fComputeShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.TNormalMapGeneration.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var ImageMemoryBarriers:array[0..1] of TVkImageMemoryBarrier;
begin

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet NormalMapGeneration',[0.5,0.75,0.25,1.0]);

 ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                      VK_IMAGE_LAYOUT_GENERAL,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      fPlanet.fData.fHeightMapImage.VulkanImage.Handle,
                                                      TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                      0,
                                                                                      1,
                                                                                      0,
                                                                                      1));

 ImageMemoryBarriers[1]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                      VK_IMAGE_LAYOUT_GENERAL,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      fPlanet.fData.fNormalMapImage.VulkanImage.Handle,
                                                      TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                      0,
                                                                                      1,
                                                                                      0,
                                                                                      1));

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   2,@ImageMemoryBarriers[0]);

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                      fPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fDescriptorSet.Handle,
                                      0,
                                      nil);

 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 0,
                                 SizeOf(TPushConstants),
                                 @fPushConstants);          

 aCommandBuffer.CmdDispatch((fPlanet.fHeightMapResolution+15) shr 4,
                            (fPlanet.fHeightMapResolution+15) shr 4,
                            1);   

 ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      VK_IMAGE_LAYOUT_GENERAL,
                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      fPlanet.fData.fHeightMapImage.VulkanImage.Handle,
                                                      TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                      0,
                                                                                      1,
                                                                                      0,
                                                                                      1));

 ImageMemoryBarriers[1]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT), 
                                                      TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      VK_IMAGE_LAYOUT_GENERAL,
                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      fPlanet.fData.fNormalMapImage.VulkanImage.Handle,
                                                      TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                     0,
                                                                                     1,
                                                                                     0,
                                                                                     1));     

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   2,@ImageMemoryBarriers[0]);

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

end;  

{ TpvScene3DPlanet.THeightMapMipMapGeneration }

constructor TpvScene3DPlanet.THeightMapMipMapGeneration.Create(const aPlanet:TpvScene3DPlanet);
var MipMapLevelSetIndex:TpvSizeInt;
    Stream:TStream;    
begin
  
 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=fPlanet.fVulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('downsample_heightmap_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.THeightMapMipMapGeneration.fComputeShaderModule');

  fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.AddBinding(1,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                  4,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.Initialize;

  fPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
  fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
  fPipelineLayout.Initialize;

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  8);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),8*8*2);
  fDescriptorPool.Initialize;

  fCountMipMapLevelSets:=Min(((fPlanet.fData.fHeightMapImage.MipMapLevels-1)+3) shr 2,8);

  for MipMapLevelSetIndex:=0 to fCountMipMapLevelSets-1 do begin

   fDescriptorSets[MipMapLevelSetIndex]:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
   fDescriptorSets[MipMapLevelSetIndex].WriteToDescriptorSet(0,
                                                             0,
                                                             1,
                                                             TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                             [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                            fPlanet.fData.fHeightMapImage.VulkanImageViews[Min(MipMapLevelSetIndex shl 2,fPlanet.fData.fHeightMapImage.MipMapLevels-1)].Handle,
                                                                                            VK_IMAGE_LAYOUT_GENERAL)],
                                                             [],
                                                             [],
                                                             false);
   fDescriptorSets[MipMapLevelSetIndex].WriteToDescriptorSet(1,
                                                             0,
                                                             4,
                                                             TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                             [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                            fPlanet.fData.fHeightMapImage.VulkanImageViews[Min(((MipMapLevelSetIndex shl 2)+1),fPlanet.fData.fHeightMapImage.MipMapLevels-1)].Handle,
                                                                                            VK_IMAGE_LAYOUT_GENERAL),
                                                              TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                            fPlanet.fData.fHeightMapImage.VulkanImageViews[Min(((MipMapLevelSetIndex shl 2)+2),fPlanet.fData.fHeightMapImage.MipMapLevels-1)].Handle,
                                                                                            VK_IMAGE_LAYOUT_GENERAL),
                                                              TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                            fPlanet.fData.fHeightMapImage.VulkanImageViews[Min(((MipMapLevelSetIndex shl 2)+3),fPlanet.fData.fHeightMapImage.MipMapLevels-1)].Handle,
                                                                                            VK_IMAGE_LAYOUT_GENERAL),
                                                              TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                            fPlanet.fData.fHeightMapImage.VulkanImageViews[Min(((MipMapLevelSetIndex shl 2)+4),fPlanet.fData.fHeightMapImage.MipMapLevels-1)].Handle,
                                                                                            VK_IMAGE_LAYOUT_GENERAL)],

                                                             [],
                                                             [],
                                                             false);
   fDescriptorSets[MipMapLevelSetIndex].Flush;
   fVulkanDevice.DebugUtils.SetObjectName(fDescriptorSets[MipMapLevelSetIndex].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'TpvScene3DPlanet.THeightMapMipMapGeneration.fDescriptorSets['+IntToStr(MipMapLevelSetIndex)+']');

  end;

  fPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                             pvApplication.VulkanPipelineCache,
                                             TVkPipelineCreateFlags(0),
                                             fComputeShaderStage,
                                             fPipelineLayout,
                                             nil,
                                             0);

 end;

end;

destructor TpvScene3DPlanet.THeightMapMipMapGeneration.Destroy;
var MipMapLevelSetIndex:TpvSizeInt;
begin
 
 FreeAndNil(fPipeline);

 for MipMapLevelSetIndex:=0 to fCountMipMapLevelSets-1 do begin
  FreeAndNil(fDescriptorSets[MipMapLevelSetIndex]);
 end;

 FreeAndNil(fDescriptorPool);

 FreeAndNil(fPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);
 
 FreeAndNil(fComputeShaderStage);

 FreeAndNil(fComputeShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.THeightMapMipMapGeneration.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var MipMapLevelSetIndex:TpvSizeInt;
    ImageMemoryBarrier:TVkImageMemoryBarrier;
begin

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet HeightMapMipMapGeneration',[0.25,0.75,0.5,1.0]);

 ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_IMAGE_LAYOUT_GENERAL,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  fPlanet.fData.fHeightMapImage.VulkanImage.Handle,
                                                  TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                  0,
                                                                                  fPlanet.fData.fHeightMapImage.MipMapLevels,
                                                                                  0,
                                                                                  1));

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@ImageMemoryBarrier);

 begin
   
  aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

  for MipMapLevelSetIndex:=0 to fCountMipMapLevelSets-1 do begin

   fPushConstants.CountMipMapLevels:=Min(4,fPlanet.fData.fHeightMapImage.MipMapLevels-((MipMapLevelSetIndex shl 2) or 1));

   if fPushConstants.CountMipMapLevels<=0 then begin
    break;
   end;

   aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                        fPipelineLayout.Handle,
                                        0,
                                        1,
                                        @fDescriptorSets[MipMapLevelSetIndex].Handle,
                                        0,
                                        nil);

   aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                   TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                   0,
                                   SizeOf(TPushConstants),
                                   @fPushConstants);

   aCommandBuffer.CmdDispatch(Max(1,(fPlanet.fHeightMapResolution+((1 shl (3+(MipMapLevelSetIndex shl 2)))-1)) shr (3+(MipMapLevelSetIndex shl 2))),
                              Max(1,(fPlanet.fHeightMapResolution+((1 shl (3+(MipMapLevelSetIndex shl 2)))-1)) shr (3+(MipMapLevelSetIndex shl 2))),
                              1);

   ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    VK_IMAGE_LAYOUT_GENERAL,
                                                    VK_IMAGE_LAYOUT_GENERAL,
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    fPlanet.fData.fHeightMapImage.VulkanImage.Handle,
                                                    TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                    Min((MipMapLevelSetIndex shl 2)+1,fPlanet.fData.fHeightMapImage.MipMapLevels-1),
                                                                                    PushConstants.CountMipMapLevels,
                                                                                    0,
                                                                                    1));

   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                     0,
                                     0,nil,
                                     0,nil,
                                     1,@ImageMemoryBarrier);  
                                     
  end;

 end;

 ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                  VK_IMAGE_LAYOUT_GENERAL,
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  fPlanet.fData.fHeightMapImage.VulkanImage.Handle,
                                                  TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                  0,
                                                                                  fPlanet.fData.fHeightMapImage.MipMapLevels,
                                                                                  0,
                                                                                  1));

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@ImageMemoryBarrier);        

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);                                                                          

end;

{ TpvScene3DPlanet.TNormalMapMipMapGeneration }

constructor TpvScene3DPlanet.TNormalMapMipMapGeneration.Create(const aPlanet:TpvScene3DPlanet);
var MipMapLevelSetIndex:TpvSizeInt;
    Stream:TStream;
begin

 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=fPlanet.fVulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('downsample_normalmap_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TNormalMapMipMapGeneration.fComputeShaderModule');

  fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.AddBinding(1,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                  4,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.Initialize;

  fPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
  fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
  fPipelineLayout.Initialize;

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  8);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),8*8*2);
  fDescriptorPool.Initialize;

  fCountMipMapLevelSets:=Min(((fPlanet.fData.fNormalMapImage.MipMapLevels-1)+3) shr 2,8);

  for MipMapLevelSetIndex:=0 to fCountMipMapLevelSets-1 do begin

   fDescriptorSets[MipMapLevelSetIndex]:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
   fDescriptorSets[MipMapLevelSetIndex].WriteToDescriptorSet(0,
                                                             0,
                                                             1,
                                                             TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                             [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                            fPlanet.fData.fNormalMapImage.VulkanImageViews[Min(MipMapLevelSetIndex shl 2,fPlanet.fData.fNormalMapImage.MipMapLevels-1)].Handle,
                                                                                            VK_IMAGE_LAYOUT_GENERAL)],
                                                             [],
                                                             [],
                                                             false);
   fDescriptorSets[MipMapLevelSetIndex].WriteToDescriptorSet(1,
                                                             0,
                                                             4,
                                                             TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                             [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                            fPlanet.fData.fNormalMapImage.VulkanImageViews[Min(((MipMapLevelSetIndex shl 2)+1),fPlanet.fData.fNormalMapImage.MipMapLevels-1)].Handle,
                                                                                            VK_IMAGE_LAYOUT_GENERAL),
                                                              TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                            fPlanet.fData.fNormalMapImage.VulkanImageViews[Min(((MipMapLevelSetIndex shl 2)+2),fPlanet.fData.fNormalMapImage.MipMapLevels-1)].Handle,
                                                                                            VK_IMAGE_LAYOUT_GENERAL),
                                                              TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                            fPlanet.fData.fNormalMapImage.VulkanImageViews[Min(((MipMapLevelSetIndex shl 2)+3),fPlanet.fData.fNormalMapImage.MipMapLevels-1)].Handle,
                                                                                            VK_IMAGE_LAYOUT_GENERAL),
                                                              TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                            fPlanet.fData.fNormalMapImage.VulkanImageViews[Min(((MipMapLevelSetIndex shl 2)+4),fPlanet.fData.fNormalMapImage.MipMapLevels-1)].Handle,
                                                                                            VK_IMAGE_LAYOUT_GENERAL)],

                                                             [],
                                                             [],
                                                             false);   
   fDescriptorSets[MipMapLevelSetIndex].Flush;
   fVulkanDevice.DebugUtils.SetObjectName(fDescriptorSets[MipMapLevelSetIndex].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'TpvScene3DPlanet.TNormalMapMipMapGeneration.fDescriptorSets['+IntToStr(MipMapLevelSetIndex)+']');

  end;

  fPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                             pvApplication.VulkanPipelineCache,
                                             TVkPipelineCreateFlags(0),
                                             fComputeShaderStage,
                                             fPipelineLayout,
                                             nil,
                                             0);

 end;

end;

destructor TpvScene3DPlanet.TNormalMapMipMapGeneration.Destroy;
var MipMapLevelSetIndex:TpvSizeInt;
begin

 FreeAndNil(fPipeline);

 for MipMapLevelSetIndex:=0 to fCountMipMapLevelSets-1 do begin
  FreeAndNil(fDescriptorSets[MipMapLevelSetIndex]);
 end;

 FreeAndNil(fDescriptorPool);

 FreeAndNil(fPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);
 
 FreeAndNil(fComputeShaderStage);

 FreeAndNil(fComputeShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.TNormalMapMipMapGeneration.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var MipMapLevelSetIndex:TpvSizeInt;
    ImageMemoryBarrier:TVkImageMemoryBarrier;
begin

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet NormalMapMipMapGeneration',[0.75,0.25,0.5,1.0]);
 
 ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_IMAGE_LAYOUT_GENERAL,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  fPlanet.fData.fNormalMapImage.VulkanImage.Handle,
                                                  TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                  0,
                                                                                  fPlanet.fData.fNormalMapImage.MipMapLevels,
                                                                                  0,
                                                                                  1));

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@ImageMemoryBarrier);

 begin

  aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

  for MipMapLevelSetIndex:=0 to fCountMipMapLevelSets-1 do begin

   fPushConstants.CountMipMapLevels:=Min(4,fPlanet.fData.fNormalMapImage.MipMapLevels-((MipMapLevelSetIndex shl 2) or 1));

   if fPushConstants.CountMipMapLevels<=0 then begin
    break;
   end;

   aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                        fPipelineLayout.Handle,
                                        0,
                                        1,
                                        @fDescriptorSets[MipMapLevelSetIndex].Handle,
                                        0,
                                        nil);

   aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                   TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                   0,
                                   SizeOf(TPushConstants),
                                   @fPushConstants);

   aCommandBuffer.CmdDispatch(Max(1,(fPlanet.fHeightMapResolution+((1 shl (3+(MipMapLevelSetIndex shl 2)))-1)) shr (3+(MipMapLevelSetIndex shl 2))),
                              Max(1,(fPlanet.fHeightMapResolution+((1 shl (3+(MipMapLevelSetIndex shl 2)))-1)) shr (3+(MipMapLevelSetIndex shl 2))),
                              1);

   ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                                    VK_IMAGE_LAYOUT_GENERAL,
                                                    VK_IMAGE_LAYOUT_GENERAL,
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    fPlanet.fData.fNormalMapImage.VulkanImage.Handle,
                                                    TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                    Min(((MipMapLevelSetIndex shl 2)+1),fPlanet.fData.fNormalMapImage.MipMapLevels-1),
                                                                                    PushConstants.CountMipMapLevels,
                                                                                    0,
                                                                                    1));

   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                     0,
                                     0,nil,
                                     0,nil,
                                     1,@ImageMemoryBarrier);

  end;

 end;

 ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                  VK_IMAGE_LAYOUT_GENERAL,
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  fPlanet.fData.fNormalMapImage.VulkanImage.Handle,
                                                  TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                  0,
                                                                                  fPlanet.fData.fNormalMapImage.MipMapLevels,
                                                                                  0,
                                                                                  1));

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@ImageMemoryBarrier); 

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

end;

{ TpvScene3DPlanet.TMeshIndexGeneration }

constructor TpvScene3DPlanet.TMeshIndexGeneration.Create(const aPlanet:TpvScene3DPlanet;const aPhysics:Boolean);
var Stream:TStream;
begin
  
 inherited Create;

 fPlanet:=aPlanet;

 fPhysics:=aPhysics;

 fVulkanDevice:=fPlanet.fVulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_tiled_mesh_index_generation_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TMeshIndexGeneration.fComputeShaderModule');

  fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.Initialize;

  fPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
  fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
  fPipelineLayout.Initialize;

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),1);
  fDescriptorPool.Initialize;

  fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  if fPhysics then begin
   fDescriptorSet.WriteToDescriptorSet(0,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                       [],
                                       [TVkDescriptorBufferInfo.Create(fPlanet.fData.fPhysicsMeshIndexBuffer.Handle,
                                                                       0,
                                                                       VK_WHOLE_SIZE)],
                                       [],
                                       false);
  end else begin
   fDescriptorSet.WriteToDescriptorSet(0,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                       [],
                                       [TVkDescriptorBufferInfo.Create(fPlanet.fData.fVisualMeshIndexBuffer.Handle,
                                                                       0,
                                                                       VK_WHOLE_SIZE)],
                                       [],
                                       false);
  end;
  fDescriptorSet.Flush;

  fPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                             pvApplication.VulkanPipelineCache,
                                             TVkPipelineCreateFlags(0),
                                             fComputeShaderStage,
                                             fPipelineLayout,
                                             nil,
                                             0);

  fPushConstants.TileMapResolution:=fPlanet.fTileMapResolution;
  if fPhysics then begin
   fPushConstants.TileResolution:=fPlanet.fPhysicsTileResolution;
  end else begin
   fPushConstants.TileResolution:=fPlanet.fVisualTileResolution;
  end;

 end;

end; 

destructor TpvScene3DPlanet.TMeshIndexGeneration.Destroy;
begin
 
 FreeAndNil(fPipeline);

 FreeAndNil(fDescriptorSet);

 FreeAndNil(fDescriptorPool);

 FreeAndNil(fPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);
 
 FreeAndNil(fComputeShaderStage);

 FreeAndNil(fComputeShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.TMeshIndexGeneration.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var BufferMemoryBarrier:TVkBufferMemoryBarrier;
begin

 if fPhysics then begin
  fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet PhysicsMeshIndexGeneration',[0.5,0.75,0.75,1.0]);
 end else begin
  fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet VisualMeshIndexGeneration',[0.75,0.5,0.75,1.0]);
 end;

 if fPhysics then begin

  BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                     TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                     VK_QUEUE_FAMILY_IGNORED,
                                                     VK_QUEUE_FAMILY_IGNORED,
                                                     fPlanet.fData.fPhysicsMeshIndexBuffer.Handle,
                                                     0,
                                                     VK_WHOLE_SIZE);

 end else begin

  BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                     TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                     VK_QUEUE_FAMILY_IGNORED,
                                                     VK_QUEUE_FAMILY_IGNORED,
                                                     fPlanet.fData.fVisualMeshIndexBuffer.Handle,
                                                     0,
                                                     VK_WHOLE_SIZE);

 end;

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   1,@BufferMemoryBarrier,
                                   0,nil);

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                      fPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fDescriptorSet.Handle,
                                      0,
                                      nil);

 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 0,
                                 SizeOf(TPushConstants),
                                 @fPushConstants);

 if fPhysics then begin
  aCommandBuffer.CmdDispatch((fPlanet.fCountPhysicsMeshIndices+255) shr 8,
                             1,
                             1);
 end else begin
  aCommandBuffer.CmdDispatch((fPlanet.fCountVisualMeshIndices+255) shr 8,
                             1,
                             1);
 end;

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   1,@BufferMemoryBarrier,
                                   0,nil);

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

end;

{ TpvScene3DPlanet.TMeshVertexGeneration }

constructor TpvScene3DPlanet.TMeshVertexGeneration.Create(const aPlanet:TpvScene3DPlanet;const aPhysics:Boolean);
var Stream:TStream;
begin
  
 inherited Create;

 fPlanet:=aPlanet;

 fPhysics:=aPhysics;

 fVulkanDevice:=fPlanet.fVulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_tiled_mesh_vertex_generation_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TMeshVertexGeneration.fComputeShaderModule');

  fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0, // Vertices 
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.AddBinding(1, // QueuedTiles
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.AddBinding(2, // HeightMap
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.AddBinding(3, // NormalMap
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);                                
  fDescriptorSetLayout.Initialize;

  fPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
  fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
  fPipelineLayout.Initialize;

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  4);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),2);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),2);
  fDescriptorPool.Initialize;

  fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  if fPhysics then begin
   fDescriptorSet.WriteToDescriptorSet(0,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                       [],
                                       [TVkDescriptorBufferInfo.Create(fPlanet.fData.fPhysicsMeshVertexBuffer.Handle,
                                                                       0,
                                                                       VK_WHOLE_SIZE)],
                                       [],
                                       false);
  end else begin
   fDescriptorSet.WriteToDescriptorSet(0,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                       [],
                                       [TVkDescriptorBufferInfo.Create(fPlanet.fData.fVisualMeshVertexBuffer.Handle,
                                                                       0,
                                                                       VK_WHOLE_SIZE)],
                                       [],
                                       false);
  end;
  fDescriptorSet.WriteToDescriptorSet(1,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                      [],
                                      [TVkDescriptorBufferInfo.Create(fPlanet.fData.fTileDirtyQueueBuffer.Handle,
                                                                      0,
                                                                      VK_WHOLE_SIZE)],
                                      [],
                                      false);
  fDescriptorSet.WriteToDescriptorSet(2,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                                      [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,//TpvScene3D(fPlanet.fScene3D).GeneralComputeSampler.Handle,
                                                                     fPlanet.fData.fHeightMapImage.VulkanImageView.Handle,
                                                                     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                      [],
                                      [],
                                      false);
  fDescriptorSet.WriteToDescriptorSet(3,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                                      [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,//TpvScene3D(fPlanet.fScene3D).GeneralComputeSampler.Handle,
                                                                     fPlanet.fData.fNormalMapImage.VulkanImageView.Handle,
                                                                     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                      [],
                                      [],
                                      false);
  fDescriptorSet.Flush;

  fPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                             pvApplication.VulkanPipelineCache,
                                             TVkPipelineCreateFlags(0),
                                             fComputeShaderStage,
                                             fPipelineLayout,
                                             nil,
                                             0);

  fPushConstants.BottomRadius:=fPlanet.fBottomRadius;
  fPushConstants.TopRadius:=fPlanet.fTopRadius;
  fPushConstants.TileMapResolution:=fPlanet.fTileMapResolution;
  if fPhysics then begin
   fPushConstants.TileResolution:=fPlanet.fPhysicsTileResolution;
   fPushConstants.LOD:=Max(0,IntLog2(fPlanet.fHeightMapResolution)-IntLog2(fPlanet.fPhysicsResolution));
  end else begin
   fPushConstants.TileResolution:=fPlanet.fVisualTileResolution;
   fPushConstants.LOD:=Max(0,IntLog2(fPlanet.fHeightMapResolution)-IntLog2(fPlanet.fVisualResolution));
  end;

 end;

end;

destructor TpvScene3DPlanet.TMeshVertexGeneration.Destroy;
begin
 
 FreeAndNil(fPipeline);

 FreeAndNil(fDescriptorSet);

 FreeAndNil(fDescriptorPool);

 FreeAndNil(fPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);
 
 FreeAndNil(fComputeShaderStage);

 FreeAndNil(fComputeShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.TMeshVertexGeneration.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var BufferMemoryBarriers:array[0..1] of TVkBufferMemoryBarrier;
    ImageMemoryBarriers:array[0..1] of TVkImageMemoryBarrier;
begin

 if fPhysics then begin
  fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet PhysicsMeshVertexGeneration',[0.5,0.75,0.75,1.0]);
 end else begin
  fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet VisualMeshVertexGeneration',[0.75,0.5,0.75,1.0]);
 end;

 if fPhysics then begin

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fPhysicsMeshVertexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

 end else begin

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualMeshVertexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

 end;

 BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fTileDirtyQueueBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      fPlanet.fData.fHeightMapImage.VulkanImage.Handle,
                                                      TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                      0,
                                                                                      fPlanet.fData.fHeightMapImage.MipMapLevels,
                                                                                      0,
                                                                                      1));

 ImageMemoryBarriers[1]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      fPlanet.fData.fNormalMapImage.VulkanImage.Handle,
                                                      TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                      0,
                                                                                      fPlanet.fData.fNormalMapImage.MipMapLevels,
                                                                                      0,
                                                                                      1));  

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT),
                                   0,
                                   0,nil,
                                   2,@BufferMemoryBarriers[0],
                                   2,@ImageMemoryBarriers[0]); 

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                      fPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fDescriptorSet.Handle,
                                      0,
                                      nil);

 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 0,
                                 SizeOf(TPushConstants),
                                 @fPushConstants);

 if fVulkanDevice.PhysicalDevice.RenderDocDetected then begin

  if fPlanet.fData.fCountDirtyTiles>0 then begin

   if fPhysics then begin
    aCommandBuffer.CmdDispatch(((fPlanet.fPhysicsTileResolution*fPlanet.fPhysicsTileResolution)+255) shr 8,
                               fPlanet.fData.fCountDirtyTiles,
                               1);
   end else begin
    aCommandBuffer.CmdDispatch(((fPlanet.fVisualTileResolution*fPlanet.fVisualTileResolution)+255) shr 8,
                               fPlanet.fData.fCountDirtyTiles,
                               1);
   end;

  end;

 end else begin

  if fPhysics then begin
   aCommandBuffer.CmdDispatchIndirect(fPlanet.fData.fTileDirtyQueueBuffer.Handle,SizeOf(TVkDispatchIndirectCommand));
  end else begin
   aCommandBuffer.CmdDispatchIndirect(fPlanet.fData.fTileDirtyQueueBuffer.Handle,0);
  end;

 end;

 if fPhysics then begin

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_HOST_READ_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fPhysicsMeshVertexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                    0,
                                    0,nil,
                                    1,@BufferMemoryBarriers[0],
                                    0,nil);

 end else begin

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualMeshVertexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    1,@BufferMemoryBarriers[0],
                                    0,nil);

 end; 

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

end;

(*
{ TpvScene3DPlanet.TBaseMeshVertexGeneration }

constructor TpvScene3DPlanet.TBaseMeshVertexGeneration.Create(const aPlanet:TpvScene3DPlanet;const aPhysics:Boolean);
var Stream:TStream;
begin
  
 inherited Create;

 fPlanet:=aPlanet;

 fPhysics:=aPhysics;

 fVulkanDevice:=fPlanet.fVulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_base_mesh_vertex_generation_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TBaseMeshVertexGeneration.fComputeShaderModule');

  fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.Initialize;

  fPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
  fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
  fPipelineLayout.Initialize;

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),1);
  fDescriptorPool.Initialize;

  fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  if fPhysics then begin
   fDescriptorSet.WriteToDescriptorSet(0,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                       [],
                                       [TVkDescriptorBufferInfo.Create(fPlanet.fData.fPhysicsBaseMeshVertexBuffer.Handle,
                                                                       0,
                                                                       VK_WHOLE_SIZE)],
                                       [],
                                       false);
  end else begin
   fDescriptorSet.WriteToDescriptorSet(0,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                       [],
                                       [TVkDescriptorBufferInfo.Create(fPlanet.fData.fVisualBaseMeshVertexBuffer.Handle,
                                                                       0,
                                                                       VK_WHOLE_SIZE)],
                                       [],
                                       false);
  end;
  fDescriptorSet.Flush;

  fPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                             pvApplication.VulkanPipelineCache,
                                             TVkPipelineCreateFlags(0),
                                             fComputeShaderStage,
                                             fPipelineLayout,
                                             nil,
                                             0);

  if fPhysics then begin
   fPushConstants.CountPoints:=fPlanet.fCountPhysicsSpherePoints;
  end else begin
   fPushConstants.CountPoints:=fPlanet.fCountVisualSpherePoints;
  end;

 end;

end;

destructor TpvScene3DPlanet.TBaseMeshVertexGeneration.Destroy;
begin
 
 FreeAndNil(fPipeline);

 FreeAndNil(fDescriptorSet);

 FreeAndNil(fDescriptorPool);

 FreeAndNil(fPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);
 
 FreeAndNil(fComputeShaderStage);

 FreeAndNil(fComputeShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.TBaseMeshVertexGeneration.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var BufferMemoryBarrier:TVkBufferMemoryBarrier;
begin

 if fPhysics then begin

  BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                     TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                     VK_QUEUE_FAMILY_IGNORED,
                                                     VK_QUEUE_FAMILY_IGNORED,
                                                     fPlanet.fData.fPhysicsBaseMeshVertexBuffer.Handle,
                                                     0,
                                                     VK_WHOLE_SIZE);

 end else begin

  BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                     TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                     VK_QUEUE_FAMILY_IGNORED,
                                                     VK_QUEUE_FAMILY_IGNORED,
                                                     fPlanet.fData.fVisualBaseMeshVertexBuffer.Handle,
                                                     0,
                                                     VK_WHOLE_SIZE);

 end;

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   1,@BufferMemoryBarrier,
                                   0,nil);

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                      fPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fDescriptorSet.Handle,
                                      0,
                                      nil);

 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 0,
                                 SizeOf(TPushConstants),
                                 @fPushConstants);

 if fPhysics then begin

  aCommandBuffer.CmdDispatch((fPlanet.fCountPhysicsSpherePoints+255) shr 8,
                             1,
                             1);
  
 end else begin
 
  aCommandBuffer.CmdDispatch((fPlanet.fCountVisualSpherePoints+255) shr 8,
                             1,
                             1);

 end;                            

 if fPhysics then begin

  BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                     TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                     VK_QUEUE_FAMILY_IGNORED,
                                                     VK_QUEUE_FAMILY_IGNORED,
                                                     fPlanet.fData.fPhysicsBaseMeshVertexBuffer.Handle,
                                                     0,
                                                     VK_WHOLE_SIZE);

 end else begin

  BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                     TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                     VK_QUEUE_FAMILY_IGNORED,
                                                     VK_QUEUE_FAMILY_IGNORED,
                                                     fPlanet.fData.fVisualBaseMeshVertexBuffer.Handle,
                                                     0,
                                                     VK_WHOLE_SIZE);

 end;

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   1,@BufferMemoryBarrier,
                                   0,nil);

end;

{ TpvScene3DPlanet.TBaseMeshIndexGeneration }
                                            
constructor TpvScene3DPlanet.TBaseMeshIndexGeneration.Create(const aPlanet:TpvScene3DPlanet;const aPhysics:Boolean);
var Stream:TStream;
begin
  
 inherited Create;

 fPlanet:=aPlanet;

 fPhysics:=aPhysics;

 fVulkanDevice:=fPlanet.fVulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_base_mesh_index_generation_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TBaseMeshIndexGeneration.fComputeShaderModule');

  fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.AddBinding(1,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.Initialize;

  fPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
  fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
  fPipelineLayout.Initialize;

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),2);
  fDescriptorPool.Initialize;

  fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  if fPhysics then begin
   fDescriptorSet.WriteToDescriptorSet(0,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                       [],
                                       [TVkDescriptorBufferInfo.Create(fPlanet.fData.fPhysicsBaseMeshVertexBuffer.Handle,
                                                                       0,
                                                                       VK_WHOLE_SIZE)],
                                       [],
                                       false);
   fDescriptorSet.WriteToDescriptorSet(1,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                       [],
                                       [TVkDescriptorBufferInfo.Create(fPlanet.fData.fPhysicsBaseMeshTriangleIndexBuffer.Handle,
                                                                       0,
                                                                       VK_WHOLE_SIZE)],
                                       [],
                                       false);
  end else begin
   fDescriptorSet.WriteToDescriptorSet(0,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                       [],
                                       [TVkDescriptorBufferInfo.Create(fPlanet.fData.fVisualBaseMeshVertexBuffer.Handle,
                                                                       0,
                                                                       VK_WHOLE_SIZE)],
                                       [],
                                       false);
   fDescriptorSet.WriteToDescriptorSet(1,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                       [],
                                       [TVkDescriptorBufferInfo.Create(fPlanet.fData.fVisualBaseMeshTriangleIndexBuffer.Handle,
                                                                       0,
                                                                       VK_WHOLE_SIZE)],
                                       [],
                                       false);
  end;
  fDescriptorSet.Flush;

  fPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                             pvApplication.VulkanPipelineCache,
                                             TVkPipelineCreateFlags(0),
                                             fComputeShaderStage,
                                             fPipelineLayout,
                                             nil,
                                             0);

  if fPhysics then begin
   fPushConstants.CountPoints:=fPlanet.fCountPhysicsSpherePoints;
  end else begin
   fPushConstants.CountPoints:=fPlanet.fCountVisualSpherePoints;
  end;

 end;

end;

destructor TpvScene3DPlanet.TBaseMeshIndexGeneration.Destroy;
begin
 
 FreeAndNil(fPipeline);

 FreeAndNil(fDescriptorSet);

 FreeAndNil(fDescriptorPool);

 FreeAndNil(fPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);
 
 FreeAndNil(fComputeShaderStage);

 FreeAndNil(fComputeShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.TBaseMeshIndexGeneration.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var BufferMemoryBarriers:array[0..1] of TVkBufferMemoryBarrier;
begin

 if fPhysics then begin

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fPhysicsBaseMeshVertexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fPhysicsBaseMeshTriangleIndexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

 end else begin

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualBaseMeshVertexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualBaseMeshTriangleIndexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

 end;

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   0,
                                   0,nil,
                                   2,@BufferMemoryBarriers[0],
                                   0,nil);

 if fPhysics then begin

  aCommandBuffer.CmdFillBuffer(fPlanet.fData.fPhysicsBaseMeshTriangleIndexBuffer.Handle,
                               0,
                               SizeOf(TpvUInt32), // only the first uint32 needs to be cleared, since it is the count of indices
                               0);

 end else begin

  aCommandBuffer.CmdFillBuffer(fPlanet.fData.fVisualBaseMeshTriangleIndexBuffer.Handle,
                               0,
                               SizeOf(TpvUInt32), // only the first uint32 needs to be cleared, since it is the count of indices
                               0);

 end;
  
 if fPhysics then begin

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fPhysicsBaseMeshTriangleIndexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

 end else begin

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualBaseMeshTriangleIndexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

 end;

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   1,@BufferMemoryBarriers[0],
                                   0,nil);                                                       

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                      fPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fDescriptorSet.Handle,
                                      0,
                                      nil);

 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 0,
                                 SizeOf(TPushConstants),
                                 @fPushConstants);

 if fPhysics then begin

  aCommandBuffer.CmdDispatch((fPlanet.fCountPhysicsSpherePoints+255) shr 8,
                             1,
                             1);

 end else begin

  aCommandBuffer.CmdDispatch((fPlanet.fCountVisualSpherePoints+255) shr 8,
                             1,
                             1);

 end;

 if fPhysics then begin

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT), 
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fPhysicsBaseMeshVertexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fPhysicsBaseMeshTriangleIndexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    2,@BufferMemoryBarriers[0],
                                    0,nil);

 end else begin
 
  // Only fBaseMeshTriangleIndexBuffer is modified, so only these that buffer need to be barriered
  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualBaseMeshTriangleIndexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    1,@BufferMemoryBarriers[0],
                                    0,nil);

 end;


end;

{ TpvScene3DPlanet.TMeshVertexGeneration } 

constructor TpvScene3DPlanet.TMeshVertexGeneration.Create(const aPlanet:TpvScene3DPlanet;const aPhysics:Boolean);
var Stream:TStream;
begin
  
 inherited Create;

 fPlanet:=aPlanet;

 fPhysics:=aPhysics;

 fVulkanDevice:=fPlanet.fVulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_mesh_vertex_generation_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TMeshVertexGeneration.fComputeShaderModule');

  fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.AddBinding(1,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.AddBinding(2,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.AddBinding(3,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.AddBinding(4,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.Initialize;

  fPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
  fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
  fPipelineLayout.Initialize;

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),2);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),3);
  fDescriptorPool.Initialize;

  fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  if fPhysics then begin
   fDescriptorSet.WriteToDescriptorSet(0,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                       [],
                                       [TVkDescriptorBufferInfo.Create(fPlanet.fData.fPhysicsBaseMeshVertexBuffer.Handle,
                                                                       0,
                                                                       VK_WHOLE_SIZE)],
                                       [],
                                       false);
   fDescriptorSet.WriteToDescriptorSet(1,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                       [],
                                       [TVkDescriptorBufferInfo.Create(fPlanet.fData.fPhysicsMeshVertexBuffer.Handle,
                                                                       0,
                                                                       VK_WHOLE_SIZE)],
                                       [],
                                       false); 
  end else begin
   fDescriptorSet.WriteToDescriptorSet(0,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                       [],
                                       [TVkDescriptorBufferInfo.Create(fPlanet.fData.fVisualBaseMeshVertexBuffer.Handle,
                                                                       0,
                                                                       VK_WHOLE_SIZE)],
                                       [],
                                       false);
   fDescriptorSet.WriteToDescriptorSet(1,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                       [],
                                       [TVkDescriptorBufferInfo.Create(fPlanet.fData.fVisualMeshVertexBuffer.Handle,
                                                                       0,
                                                                       VK_WHOLE_SIZE)],
                                       [],
                                       false); 
  end;
  fDescriptorSet.WriteToDescriptorSet(2,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                      [TVkDescriptorImageInfo.Create(TpvScene3D(fPlanet.fScene3D).GeneralComputeSampler.Handle,
                                                                     fPlanet.fData.fHeightMapImage.VulkanImageView.Handle,
                                                                     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                      [],
                                      [],
                                      false);
  fDescriptorSet.WriteToDescriptorSet(3,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                      [TVkDescriptorImageInfo.Create(TpvScene3D(fPlanet.fScene3D).GeneralComputeSampler.Handle,
                                                                     fPlanet.fData.fNormalMapImage.VulkanImageView.Handle,
                                                                     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                      [],
                                      [],
                                      false);  
  fDescriptorSet.WriteToDescriptorSet(4,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                      [TVkDescriptorImageInfo.Create(TpvScene3D(fPlanet.fScene3D).GeneralComputeSampler.Handle,
                                                                     fPlanet.fData.fTangentBitangentMapImage.VulkanImageView.Handle,
                                                                     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                      [],
                                      [],
                                      false);  
  fDescriptorSet.Flush;

  fPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                             pvApplication.VulkanPipelineCache,
                                             TVkPipelineCreateFlags(0),
                                             fComputeShaderStage,
                                             fPipelineLayout,
                                             nil,
                                             0);

  if fPhysics then begin
   fPushConstants.CountPoints:=fPlanet.fCountPhysicsSpherePoints;
  end else begin
   fPushConstants.CountPoints:=fPlanet.fCountVisualSpherePoints;
  end;
  fPushConstants.PlanetGroundRadius:=fPlanet.fBottomRadius;
  fPushConstants.HeightMapScale:=fPlanet.fHeightMapScale;

 end;

end;

destructor TpvScene3DPlanet.TMeshVertexGeneration.Destroy;
begin
 
 FreeAndNil(fPipeline);

 FreeAndNil(fDescriptorSet);

 FreeAndNil(fDescriptorPool);

 FreeAndNil(fPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);
 
 FreeAndNil(fComputeShaderStage);

 FreeAndNil(fComputeShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.TMeshVertexGeneration.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var BufferMemoryBarriers:array[0..1] of TVkBufferMemoryBarrier;
    ImageMemoryBarriers:array[0..2] of TVkImageMemoryBarrier;
begin

 if fPhysics then begin

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT), 
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fPhysicsBaseMeshVertexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT), 
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fPhysicsMeshVertexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE); 

 end else begin

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT), 
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualBaseMeshVertexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT), 
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualMeshVertexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE); 

 end;
 
 ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      fPlanet.fData.fHeightMapImage.VulkanImage.Handle,
                                                      TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                      0,
                                                                                      1,
                                                                                      0,
                                                                                      1));

 ImageMemoryBarriers[1]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      fPlanet.fData.fNormalMapImage.VulkanImage.Handle,
                                                      TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                     0,
                                                                                     1,
                                                                                     0,
                                                                                     1));

 ImageMemoryBarriers[2]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      VK_QUEUE_FAMILY_IGNORED,
                                                      fPlanet.fData.fTangentBitangentMapImage.VulkanImage.Handle,
                                                      TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                     0,
                                                                                     1,
                                                                                     0,
                                                                                     1));

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   2,@BufferMemoryBarriers[0],
                                   3,@ImageMemoryBarriers[0]);

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                      fPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fDescriptorSet.Handle,
                                      0,
                                      nil);

 fPushConstants.ModelMatrix:=fPlanet.fData.fModelMatrix;

 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 0,
                                 SizeOf(TPushConstants),
                                 @fPushConstants);

 if fPhysics then begin
 
  aCommandBuffer.CmdDispatch((fPlanet.fCountPhysicsSpherePoints+255) shr 8,
                             1,
                             1);

 end else begin

  aCommandBuffer.CmdDispatch((fPlanet.fCountVisualSpherePoints+255) shr 8,
                             1,
                             1);
 end;

 if fPhysics then begin

  // Just one buffer memory barrier is needed here, since only fMeshVertexDataBuffer was written to, here in this compute shader.
  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fPhysicsMeshVertexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

 end else begin

  // Just one buffer memory barrier is needed here, since only fMeshVertexDataBuffer was written to, here in this compute shader.  
  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualMeshVertexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

 end;

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   1,@BufferMemoryBarriers[0],
                                   0,nil);

end;
  *)

{ TpvScene3DPlanet.TRayIntersection }

constructor TpvScene3DPlanet.TRayIntersection.Create(const aPlanet:TpvScene3DPlanet);
var Stream:TStream;
begin

 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=fPlanet.fVulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_ray_intersection_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TRayIntersection.fComputeShaderModule');

  fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.AddBinding(1,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.Initialize;

  fPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
  fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
  fPipelineLayout.Initialize;

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),1);
  fDescriptorPool.Initialize;

  fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  fDescriptorSet.WriteToDescriptorSet(0,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                      [TVkDescriptorImageInfo.Create(TpvScene3D(fPlanet.fScene3D).GeneralComputeSampler.Handle,
                                                                     fPlanet.fData.fHeightMapImage.VulkanImageView.Handle,
                                                                     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                      [],
                                      [],
                                      false);
  fDescriptorSet.WriteToDescriptorSet(1,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                      [],
                                      [TVkDescriptorBufferInfo.Create(fPlanet.fData.fRayIntersectionResultBuffer.Handle,
                                                                      0,
                                                                      VK_WHOLE_SIZE)],
                                      [],
                                      false);
  fDescriptorSet.Flush;

  fPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                             pvApplication.VulkanPipelineCache,
                                             TVkPipelineCreateFlags(0),
                                             fComputeShaderStage,
                                             fPipelineLayout,
                                             nil,
                                             0);

 end;

end;

destructor TpvScene3DPlanet.TRayIntersection.Destroy;
begin

 FreeAndNil(fPipeline);

 FreeAndNil(fDescriptorSet);

 FreeAndNil(fDescriptorPool);

 FreeAndNil(fPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);

 FreeAndNil(fComputeShaderStage);

 FreeAndNil(fComputeShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.TRayIntersection.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aRayOrigin,aRayDirection:TpvVector3);
var BufferMemoryBarrier:TVkBufferMemoryBarrier;
    ImageMemoryBarrier:TVkImageMemoryBarrier;
begin

 BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT) or TVkAccessFlags(VK_ACCESS_HOST_READ_BIT),
                                                    TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    fPlanet.fData.fRayIntersectionResultBuffer.Handle,
                                                    0,
                                                    VK_WHOLE_SIZE);

 ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  fPlanet.fData.fHeightMapImage.VulkanImage.Handle,
                                                  TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                  0,
                                                                                  fPlanet.fData.fHeightMapImage.MipMapLevels,
                                                                                  0,
                                                                                  1));

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   1,@BufferMemoryBarrier,
                                   1,@ImageMemoryBarrier);

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                      fPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fDescriptorSet.Handle,
                                      0,
                                      nil);

 fPushConstants.RayOriginPlanetBottomRadius:=TpvVector4.InlineableCreate(aRayOrigin,fPlanet.fBottomRadius);
 fPushConstants.RayDirectionPlanetTopRadius:=TpvVector4.InlineableCreate(aRayDirection,fPlanet.fTopRadius);
 fPushConstants.PlanetCenter:=TpvVector4.InlineableCreate(fPlanet.fData.fModelMatrix.MulHomogen(TpvVector3.Origin),0.0);

 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 0,
                                 SizeOf(TPushConstants),
                                 @fPushConstants);

 aCommandBuffer.CmdDispatch(1,1,1);

 BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT) or TVkAccessFlags(VK_ACCESS_HOST_READ_BIT),
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    fPlanet.fData.fRayIntersectionResultBuffer.Handle,
                                                    0,
                                                    VK_WHOLE_SIZE);

 ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  fPlanet.fData.fHeightMapImage.VulkanImage.Handle,
                                                  TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                  0,
                                                                                  fPlanet.fData.fHeightMapImage.MipMapLevels,
                                                                                  0,
                                                                                  1));

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   1,@BufferMemoryBarrier,
                                   1,@ImageMemoryBarrier);

end;

{ TpvScene3DPlanet.TRenderPass }

constructor TpvScene3DPlanet.TRenderPass.Create(const aRenderer:TObject;const aRendererInstance:TObject;const aScene3D:TObject;const aMode:TpvScene3DPlanet.TRenderPass.TMode);
var InFlightFrameIndex:TpvSizeInt;
    Stream:TStream;
    Kind:TpvUTF8String;
begin

 inherited Create;

 fRenderer:=aRenderer;

 fRendererInstance:=aRendererInstance;

 fScene3D:=aScene3D;

 fMode:=aMode;

 fVulkanDevice:=TpvScene3D(fScene3D).VulkanDevice;

 if assigned(fVulkanDevice) then begin

  case TpvScene3DPlanet.SourcePrimitiveMode of
   TpvScene3DPlanet.TSourcePrimitiveMode.NormalizedCubeTriangles:begin
    Kind:='triangles_';
   end;
   TpvScene3DPlanet.TSourcePrimitiveMode.OctasphereTriangles:begin
    Kind:='octahedral_triangles_';
   end;
   TpvScene3DPlanet.TSourcePrimitiveMode.OctasphereQuads:begin
    Kind:='octahedral_';
   end;
   TpvScene3DPlanet.TSourcePrimitiveMode.Icosphere:begin
    Kind:='icosahedral_triangles_';
   end;
   TpvScene3DPlanet.TSourcePrimitiveMode.VisualMeshTriangles,
   TpvScene3DPlanet.TSourcePrimitiveMode.PhysicsMeshTriangles:begin
    Kind:='external_';
   end;
   else begin
    Kind:='';
   end;
  end;

  if Direct then begin
   Kind:='direct_'+Kind;
  end;

  if (fMode in [TpvScene3DPlanet.TRenderPass.TMode.DepthPrepass,TpvScene3DPlanet.TRenderPass.TMode.Opaque]) and TpvScene3DRenderer(fRenderer).VelocityBufferNeeded then begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_renderpass_'+Kind+'velocity_vert.spv');
  end else begin 
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_renderpass_'+Kind+'vert.spv');
  end;
  try
   fVertexShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fVertexShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TRenderPass.fVertexShaderModule');

  case TpvScene3DPlanet.SourcePrimitiveMode of
   TpvScene3DPlanet.TSourcePrimitiveMode.NormalizedCubeTriangles,
   TpvScene3DPlanet.TSourcePrimitiveMode.OctasphereTriangles,
   TpvScene3DPlanet.TSourcePrimitiveMode.Icosphere,
   TpvScene3DPlanet.TSourcePrimitiveMode.VisualMeshTriangles,
   TpvScene3DPlanet.TSourcePrimitiveMode.PhysicsMeshTriangles:begin
    Kind:='triangles_';
   end;
   else begin
    Kind:='';
   end;
  end;

  if Direct then begin

   fTessellationControlShaderModule:=nil;

   fTessellationEvaluationShaderModule:=nil;

  end else begin

   if (fMode in [TpvScene3DPlanet.TRenderPass.TMode.DepthPrepass,TpvScene3DPlanet.TRenderPass.TMode.Opaque]) and TpvScene3DRenderer(fRenderer).VelocityBufferNeeded then begin
    Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_renderpass_'+Kind+'velocity_tesc.spv');
   end else begin
    Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_renderpass_'+Kind+'tesc.spv');
   end;
   try
    fTessellationControlShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
   finally
    FreeAndNil(Stream);
   end;

   fVulkanDevice.DebugUtils.SetObjectName(fTessellationControlShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TRenderPass.fTessellationControlShaderModule');

   if (fMode in [TpvScene3DPlanet.TRenderPass.TMode.DepthPrepass,TpvScene3DPlanet.TRenderPass.TMode.Opaque]) and TpvScene3DRenderer(fRenderer).VelocityBufferNeeded then begin
    Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_renderpass_'+Kind+'velocity_tese.spv');
   end else begin
    Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_renderpass_'+Kind+'tese.spv');
   end;
   try
    fTessellationEvaluationShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
   finally
    FreeAndNil(Stream);
   end;

   fVulkanDevice.DebugUtils.SetObjectName(fTessellationEvaluationShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TRenderPass.fTessellationEvaluationShaderModule');

  end;

  case fMode of
   
   TpvScene3DPlanet.TRenderPass.TMode.ShadowMap,
   TpvScene3DPlanet.TRenderPass.TMode.DepthPrepass:begin     
   
    fFragmentShaderModule:=nil; // No fragment shader, because we only need write to the depth buffer in these cases

   end; 
   
   TpvScene3DPlanet.TRenderPass.TMode.ReflectiveShadowMap:begin
   
    Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_renderpass_rsm_frag.spv');
    try
     fFragmentShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
    finally
     FreeAndNil(Stream);
    end;

   end; 

   else begin

    case TpvScene3DPlanet.SourcePrimitiveMode of
     TpvScene3DPlanet.TSourcePrimitiveMode.VisualMeshTriangles,
     TpvScene3DPlanet.TSourcePrimitiveMode.PhysicsMeshTriangles:begin
      Kind:='external_';
     end;
     else begin
      Kind:='';
     end;
    end;

    if fVulkanDevice.FragmentShaderBarycentricFeaturesKHR.fragmentShaderBarycentric<>VK_FALSE then begin
     if TpvScene3DRenderer(fRenderer).VelocityBufferNeeded then begin
      Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_renderpass_wireframe_velocity_'+Kind+'frag.spv');
     end else begin
      Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_renderpass_wireframe_'+Kind+'frag.spv');
     end;
    end else begin
     if TpvScene3DRenderer(fRenderer).VelocityBufferNeeded then begin
      Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_renderpass_velocity_'+Kind+'frag.spv');
     end else begin
      Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_renderpass_'+Kind+'frag.spv');
     end;
    end;
    try
     fFragmentShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
    finally
     FreeAndNil(Stream);
    end;

   end;
  end;

  if assigned(fFragmentShaderModule) then begin
   fVulkanDevice.DebugUtils.SetObjectName(fFragmentShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TRenderPass.fFragmentShaderModule');
  end;

  fVertexShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fVertexShaderModule,'main');

  if assigned(fTessellationControlShaderModule) then begin
   fTessellationControlShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT,fTessellationControlShaderModule,'main');
  end else begin
   fTessellationControlShaderStage:=nil;
  end;

  if assigned(fTessellationEvaluationShaderModule) then begin
   fTessellationEvaluationShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT,fTessellationEvaluationShaderModule,'main');
  end else begin
   fTessellationEvaluationShaderStage:=nil;
  end;

  if assigned(fFragmentShaderModule) then begin
   fFragmentShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fFragmentShaderModule,'main');
  end else begin
   fFragmentShaderStage:=nil;
  end;

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);

  // Uniform buffer with the views 
  fDescriptorSetLayout.AddBinding(0,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or
                                  TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT) or
                                  TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT) or
                                  TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                  [],
                                  0);

  fDescriptorSetLayout.AddBinding(1,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                  3,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or
                                  TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT) or
                                  TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT) or
                                  TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                  [],
                                  0);

  fDescriptorSetLayout.AddBinding(2,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                  3,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or
                                  TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT) or
                                  TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT) or
                                  TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                  [],
                                  0);

  fDescriptorSetLayout.AddBinding(3,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or
                                  TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT) or
                                  TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT) or
                                  TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                  [],
                                  0);

  // TODO: Add more bindings for other stuff like material textures, etc.

  fDescriptorSetLayout.Initialize;

  fPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or 
                                       TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT) or 
                                       TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT) or 
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       0,
                                       SizeOf(TPushConstants));
  fPipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).GlobalVulkanDescriptorSetLayout); // Global scene descriptor set
  fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout); // Global planet descriptor set
  fPipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).PlanetDescriptorSetLayout); // Per planet descriptor set
  fPipelineLayout.Initialize;

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  TpvScene3D(fScene3D).CountInFlightFrames);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),1*TpvScene3D(fScene3D).CountInFlightFrames);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),6*TpvScene3D(fScene3D).CountInFlightFrames);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),1*TpvScene3D(fScene3D).CountInFlightFrames);
  fDescriptorPool.Initialize;

  for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
   fDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
   fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                            0,
                                                            1,
                                                            TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                            [],
                                                            [TpvScene3DRendererInstance(fRendererInstance).VulkanViewUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                            [],
                                                            false);
   fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                            0,
                                                            3,
                                                            TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                            [TpvScene3DRendererInstance(fRendererInstance).Renderer.GGXBRDF.DescriptorImageInfo,
                                                             TpvScene3DRendererInstance(fRendererInstance).Renderer.CharlieBRDF.DescriptorImageInfo,
                                                             TpvScene3DRendererInstance(fRendererInstance).Renderer.SheenEBRDF.DescriptorImageInfo],
                                                            [],
                                                            [],
                                                            false);
   fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                            0,
                                                            3,
                                                            TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                            [TpvScene3DRendererInstance(fRendererInstance).Renderer.ImageBasedLightingEnvMapCubeMaps.GGXDescriptorImageInfo,
                                                             TpvScene3DRendererInstance(fRendererInstance).Renderer.ImageBasedLightingEnvMapCubeMaps.CharlieDescriptorImageInfo,
                                                             TpvScene3DRendererInstance(fRendererInstance).Renderer.ImageBasedLightingEnvMapCubeMaps.LambertianDescriptorImageInfo],
                                                            [],
                                                            [],
                                                            false);
   fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                            0,
                                                            1,
                                                            TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                            [],
                                                            [TpvScene3DRendererInstance(fRendererInstance).Renderer.SkySphericalHarmonicsMetaDataBuffer.DescriptorBufferInfo],
                                                            [],
                                                            false);
   fDescriptorSets[InFlightFrameIndex].Flush;
  end;

 end;

end;

destructor TpvScene3DPlanet.TRenderPass.Destroy;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fPipeline);

 for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
  FreeAndNil(fDescriptorSets[InFlightFrameIndex]);
 end;

 FreeAndNil(fDescriptorPool);

 FreeAndNil(fPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);

 FreeAndNil(fFragmentShaderStage);

 FreeAndNil(fTessellationEvaluationShaderStage);

 FreeAndNil(fTessellationControlShaderStage);

 FreeAndNil(fVertexShaderStage);

 FreeAndNil(fFragmentShaderModule);

 FreeAndNil(fTessellationEvaluationShaderModule);

 FreeAndNil(fTessellationControlShaderModule);

 FreeAndNil(fVertexShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.TRenderPass.AllocateResources(const aRenderPass:TpvVulkanRenderPass;
                                                         const aWidth:TpvInt32;
                                                         const aHeight:TpvInt32;
                                                         const aVulkanSampleCountFlagBits:TVkSampleCountFlagBits);
begin                                                         

 fWidth:=aWidth;
 fHeight:=aHeight;

 fPipeline:=TpvVulkanGraphicsPipeline.Create(fVulkanDevice,
                                             TpvScene3DRenderer(fRenderer).VulkanPipelineCache,
                                             0,
                                             [],
                                             fPipelineLayout,
                                             aRenderPass,
                                             0,
                                             nil,
                                             0);

 fPipeline.AddStage(fVertexShaderStage);
 if assigned(fTessellationControlShaderStage) then begin
  fPipeline.AddStage(fTessellationControlShaderStage);
 end;
 if assigned(fTessellationEvaluationShaderStage) then begin
  fPipeline.AddStage(fTessellationEvaluationShaderStage);
 end;
 if assigned(fFragmentShaderStage) then begin
  fPipeline.AddStage(fFragmentShaderStage);
 end;

 if Direct then begin
  fPipeline.InputAssemblyState.Topology:=TVkPrimitiveTopology(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
 end else begin
  fPipeline.InputAssemblyState.Topology:=TVkPrimitiveTopology(VK_PRIMITIVE_TOPOLOGY_PATCH_LIST);
 end;

 fPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 case TpvScene3DPlanet.SourcePrimitiveMode of
  TpvScene3DPlanet.TSourcePrimitiveMode.VisualMeshTriangles,
  TpvScene3DPlanet.TSourcePrimitiveMode.PhysicsMeshTriangles:begin
   fPipeline.VertexInputState.AddVertexInputBindingDescription(0,SizeOf(TpvVector4)*2,VK_VERTEX_INPUT_RATE_VERTEX);
   fPipeline.VertexInputState.AddVertexInputAttributeDescription(0,0,VK_FORMAT_R32G32B32_SFLOAT,0);
   fPipeline.VertexInputState.AddVertexInputAttributeDescription(1,0,VK_FORMAT_R32G32B32_SFLOAT,SizeOf(TpvVector4));
  end;
  else begin
  end;
 end;

 fPipeline.ViewPortState.AddViewPort(0.0,0.0,aWidth,aHeight,0.0,1.0);
 fPipeline.ViewPortState.AddScissor(0,0,aWidth,aHeight);

 fPipeline.RasterizationState.DepthClampEnable:=false;
 fPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
 fPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
 fPipeline.RasterizationState.DepthBiasEnable:=false;
 fPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
 fPipeline.RasterizationState.DepthBiasClamp:=0.0;
 fPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
 fPipeline.RasterizationState.LineWidth:=1.0;

 fPipeline.MultisampleState.RasterizationSamples:=aVulkanSampleCountFlagBits;
 fPipeline.MultisampleState.SampleShadingEnable:=false;
 fPipeline.MultisampleState.MinSampleShading:=0.0;
 fPipeline.MultisampleState.CountSampleMasks:=0;
 fPipeline.MultisampleState.AlphaToCoverageEnable:=false;
 fPipeline.MultisampleState.AlphaToOneEnable:=false;

 fPipeline.ColorBlendState.LogicOpEnable:=false;
 fPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
 fPipeline.ColorBlendState.BlendConstants[0]:=0.0;
 fPipeline.ColorBlendState.BlendConstants[1]:=0.0;
 fPipeline.ColorBlendState.BlendConstants[2]:=0.0;
 fPipeline.ColorBlendState.BlendConstants[3]:=0.0;
 fPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
                                                         VK_BLEND_FACTOR_ZERO,
                                                         VK_BLEND_FACTOR_ZERO,
                                                         VK_BLEND_OP_ADD,
                                                         VK_BLEND_FACTOR_ZERO,
                                                         VK_BLEND_FACTOR_ZERO,
                                                         VK_BLEND_OP_ADD,
                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                         TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));
 if (fMode=TpvScene3DPlanet.TRenderPass.TMode.Opaque) and TpvScene3DRenderer(fRenderer).VelocityBufferNeeded then begin
  fPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
                                                         VK_BLEND_FACTOR_ZERO,
                                                         VK_BLEND_FACTOR_ZERO,
                                                         VK_BLEND_OP_ADD,
                                                         VK_BLEND_FACTOR_ZERO,
                                                         VK_BLEND_FACTOR_ZERO,
                                                         VK_BLEND_OP_ADD,
                                                         0);
 end;

 fPipeline.DepthStencilState.DepthTestEnable:=true;
 fPipeline.DepthStencilState.DepthWriteEnable:=true;
 case fMode of
  TpvScene3DPlanet.TRenderPass.TMode.ShadowMap,
  TpvScene3DPlanet.TRenderPass.TMode.ReflectiveShadowMap:begin
   fPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
  end;
  else begin
   if TpvScene3DRendererInstance(fRendererInstance).ZFar<0.0 then begin
    fPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_GREATER_OR_EQUAL;
   end else begin
    fPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
   end;
  end;
 end;
 fPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fPipeline.DepthStencilState.StencilTestEnable:=false;

 if not Direct then begin
  case TpvScene3DPlanet.SourcePrimitiveMode of
   TpvScene3DPlanet.TSourcePrimitiveMode.NormalizedCubeTriangles,
   TpvScene3DPlanet.TSourcePrimitiveMode.OctasphereTriangles,
   TpvScene3DPlanet.TSourcePrimitiveMode.Icosphere,
   TpvScene3DPlanet.TSourcePrimitiveMode.VisualMeshTriangles,
   TpvScene3DPlanet.TSourcePrimitiveMode.PhysicsMeshTriangles:begin
    fPipeline.TessellationState.PatchControlPoints:=3;
   end;
   else begin
    fPipeline.TessellationState.PatchControlPoints:=4;
   end;
  end;
 end;

 fPipeline.Initialize;

end;

procedure TpvScene3DPlanet.TRenderPass.ReleaseResources;
begin

 FreeAndNil(fPipeline);

end;

procedure TpvScene3DPlanet.TRenderPass.Draw(const aInFlightFrameIndex,aViewBaseIndex,aCountViews:TpvSizeInt;const aCommandBuffer:TpvVulkanCommandBuffer);
const Offsets:array[0..0] of TVkUInt32=(0);
var PlanetIndex,Level:TpvSizeInt;
    Planet:TpvScene3DPlanet;
    First:Boolean;
    ViewMatrix,InverseViewMatrix,ProjectionMatrix:PpvMatrix4x4;
    ViewProjectionMatrix:TpvMatrix4x4;
    InverseViewProjectionMatrix:TpvMatrix4x4;
    Sphere:TpvSphere;
    Center,Right,Up:TpvVector3;
    TessellationFactor:TpvScalar;
    LeftAnchor,RightAnchor,DownAnchor,UpAnchor,MinXY,MaxXY:TpvVector2;
    Rect:TpvRect;
    DescriptorSets:array[0..1] of TVkDescriptorSet;
begin

 TpvScene3D(fScene3D).Planets.Lock.Acquire;
 try

  First:=true;

  for PlanetIndex:=0 to TpvScene3D(fScene3D).Planets.Count-1 do begin

   Planet:=TpvScene3D(fScene3D).Planets[PlanetIndex];

   if Planet.fReady and Planet.fInFlightFrameReady[aInFlightFrameIndex] then begin

    {if Planet.fData.fVisible then}begin

     if First then begin
       
      First:=false;

      aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fPipeline.Handle);

      DescriptorSets[0]:=TpvScene3D(fScene3D).GlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle;
      DescriptorSets[1]:=fDescriptorSets[aInFlightFrameIndex].Handle;

      aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                           fPipelineLayout.Handle,
                                           0,
                                           2,
                                           @DescriptorSets,
                                           0,
                                           nil);

     end; 

     aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                          fPipelineLayout.Handle,
                                          2,
                                          1,
                                          @Planet.fDescriptorSets[aInFlightFrameIndex].Handle,
                                          0,
                                          nil);

     ViewMatrix:=@TpvScene3DRendererInstance(fRendererInstance).Views[aInFlightFrameIndex].Items[aViewBaseIndex].ViewMatrix;
     InverseViewMatrix:=@TpvScene3DRendererInstance(fRendererInstance).Views[aInFlightFrameIndex].Items[aViewBaseIndex].InverseViewMatrix;
     ProjectionMatrix:=@TpvScene3DRendererInstance(fRendererInstance).Views[aInFlightFrameIndex].Items[aViewBaseIndex].ProjectionMatrix;

     Sphere.Center:=(Planet.fInFlightFrameDataList[aInFlightFrameIndex].fModelMatrix*TpvVector4.InlineableCreate(0.0,0.0,0.0,1.0)).xyz;
     Sphere.Radius:=Planet.fBottomRadius;

     Center:=ViewMatrix^.MulHomogen(Sphere.Center);
     Right:=InverseViewMatrix.Right.xyz*Sphere.Radius;
     Up:=InverseViewMatrix.Up.xyz*Sphere.Radius;

     LeftAnchor:=ProjectionMatrix^.MulHomogen(Center-Right).xy;
     RightAnchor:=ProjectionMatrix^.MulHomogen(Center+Right).xy;
     DownAnchor:=ProjectionMatrix^.MulHomogen(Center-Up).xy;
     UpAnchor:=ProjectionMatrix^.MulHomogen(Center+Up).xy;

     MinXY:=((TpvVector2.InlineableCreate(Min(LeftAnchor.x,Min(RightAnchor.x,Min(DownAnchor.x,UpAnchor.x))),Min(LeftAnchor.y,Min(RightAnchor.y,Min(DownAnchor.y,UpAnchor.y))))*0.5)+TpvVector2.InlineableCreate(0.5))*TpvVector2.InlineableCreate(fWidth,fHeight);

     MaxXY:=((TpvVector2.InlineableCreate(Max(LeftAnchor.x,Max(RightAnchor.x,Max(DownAnchor.x,UpAnchor.x))),Max(LeftAnchor.y,Max(RightAnchor.y,Max(DownAnchor.y,UpAnchor.y))))*0.5)+TpvVector2.InlineableCreate(0.5))*TpvVector2.InlineableCreate(fWidth,fHeight);

     Rect:=TpvRect.CreateAbsolute(MinXY,MaxXY);

     case fMode of
      TpvScene3DPlanet.TRenderPass.TMode.ShadowMap,
      TpvScene3DPlanet.TRenderPass.TMode.ReflectiveShadowMap:begin
       case TpvScene3DPlanet.SourcePrimitiveMode of
        TpvScene3DPlanet.TSourcePrimitiveMode.Icosphere:begin
         TessellationFactor:=1.0/256.0;
        end;
        else begin
         TessellationFactor:=1.0/16.0;
        end;
       end;
       Level:=-1;
      end;
      else begin
       case TpvScene3DPlanet.SourcePrimitiveMode of
        TpvScene3DPlanet.TSourcePrimitiveMode.Icosphere:begin
         TessellationFactor:=1.0/16.0;
        end;
        TpvScene3DPlanet.TSourcePrimitiveMode.OctasphereTriangles,
        TpvScene3DPlanet.TSourcePrimitiveMode.OctasphereQuads:begin
         TessellationFactor:=1.0/4.0;
        end;
        else begin
         TessellationFactor:=1.0/16.0;
        end;
       end;
       Level:=Min(Max(Round(Rect.Size.Length/Max(1,sqrt(sqr(fWidth)+sqr(fHeight))/4.0)),0),7);
      end;
     end;

//     writeln(Rect.Width:1:6,' ',Rect.Height:1:6,' ',Level:1:6);

     fPushConstants.ViewBaseIndex:=aViewBaseIndex;
     fPushConstants.CountViews:=aCountViews;
     case TpvScene3DPlanet.SourcePrimitiveMode of
      TpvScene3DPlanet.TSourcePrimitiveMode.Icosphere:begin
       if Level<0 then begin
        fPushConstants.CountQuadPointsInOneDirection:=32;
       end else begin
        fPushConstants.CountQuadPointsInOneDirection:=64;//Min(Max(1 shl Level,32),256);
       end;
      end;
      TpvScene3DPlanet.TSourcePrimitiveMode.OctasphereTriangles,
      TpvScene3DPlanet.TSourcePrimitiveMode.OctasphereQuads:begin
       if Level<0 then begin
        fPushConstants.CountQuadPointsInOneDirection:=32;
       end else begin
        fPushConstants.CountQuadPointsInOneDirection:=64;//Min(Max(1 shl Level,16),256);
       end;
      end;
      else begin
       if Level<0 then begin
        fPushConstants.CountQuadPointsInOneDirection:=32;
       end else begin
        fPushConstants.CountQuadPointsInOneDirection:=64;//Min(Max(16 shl Level,2),256);
       end;
      end;
     end;
     if Level>=0 then begin
      //writeln(fPushConstants.CountQuadPointsInOneDirection,' ',Level);
     end;
     fPushConstants.CountAllViews:=TpvScene3DRendererInstance(fRendererInstance).InFlightFrameStates[aInFlightFrameIndex].CountViews;
     fPushConstants.ResolutionXY:=(fWidth and $ffff) or ((fHeight and $ffff) shl 16);
     fPushConstants.TessellationFactor:=TessellationFactor;
     if fMode in [TpvScene3DPlanet.TRenderPass.TMode.DepthPrepass,TpvScene3DPlanet.TRenderPass.TMode.Opaque] then begin
      fPushConstants.Jitter:=TpvScene3DRendererInstance(fRendererInstance).InFlightFrameStates[aInFlightFrameIndex].Jitter.xy;
     end else begin
      fPushConstants.Jitter:=TpvVector2.Null;
     end;

     aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                     TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or 
                                     TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT) or 
                                     TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT) or 
                                     TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                     0,
                                     SizeOf(TPushConstants),
                                     @fPushConstants);

     case TpvScene3DPlanet.SourcePrimitiveMode of
      TpvScene3DPlanet.TSourcePrimitiveMode.NormalizedCubeTriangles:begin
       aCommandBuffer.CmdDraw(fPushConstants.CountQuadPointsInOneDirection*fPushConstants.CountQuadPointsInOneDirection*6*6,1,0,0);
      end;
      TpvScene3DPlanet.TSourcePrimitiveMode.NormalizedCubeQuads:begin
       aCommandBuffer.CmdDraw(fPushConstants.CountQuadPointsInOneDirection*fPushConstants.CountQuadPointsInOneDirection*6*4,1,0,0);
      end;
      TpvScene3DPlanet.TSourcePrimitiveMode.OctasphereTriangles:begin
       aCommandBuffer.CmdDraw(fPushConstants.CountQuadPointsInOneDirection*fPushConstants.CountQuadPointsInOneDirection*6,1,0,0);
      end;
      TpvScene3DPlanet.TSourcePrimitiveMode.OctasphereQuads:begin
       aCommandBuffer.CmdDraw(fPushConstants.CountQuadPointsInOneDirection*fPushConstants.CountQuadPointsInOneDirection*4,1,0,0);
      end;
      TpvScene3DPlanet.TSourcePrimitiveMode.Icosphere:begin
       aCommandBuffer.CmdDraw(fPushConstants.CountQuadPointsInOneDirection*fPushConstants.CountQuadPointsInOneDirection*3*20,1,0,0);
      end;
      TpvScene3DPlanet.TSourcePrimitiveMode.VisualMeshTriangles:begin
       aCommandBuffer.CmdBindIndexBuffer(Planet.fInFlightFrameDataList[aInFlightFrameIndex].fVisualMeshIndexBuffer.Handle,0,VK_INDEX_TYPE_UINT32);
       aCommandBuffer.CmdBindVertexBuffers(0,1,@Planet.fInFlightFrameDataList[aInFlightFrameIndex].fVisualMeshVertexBuffer.Handle,@Offsets);
       aCommandBuffer.CmdDrawIndexed(Planet.fTileMapResolution*
                                     Planet.fTileMapResolution*
                                     Planet.fVisualTileResolution*
                                     Planet.fVisualTileResolution*
                                     6,
                                     1,0,0,0);
{      aCommandBuffer.CmdDraw(Planet.fTileMapResolution*
                              Planet.fTileMapResolution*
                              Planet.fVisualTileResolution*
                              Planet.fVisualTileResolution,
                              1,0,0);}
      end;
      TpvScene3DPlanet.TSourcePrimitiveMode.PhysicsMeshTriangles:begin
       aCommandBuffer.CmdBindIndexBuffer(Planet.fData.fPhysicsMeshIndexBuffer.Handle,0,VK_INDEX_TYPE_UINT32);
       aCommandBuffer.CmdBindVertexBuffers(0,1,@Planet.fData.fPhysicsMeshVertexBuffer.Handle,@Offsets);
       aCommandBuffer.CmdDrawIndexed(Planet.fTileMapResolution*
                                     Planet.fTileMapResolution*
                                     Planet.fPhysicsTileResolution*
                                     Planet.fPhysicsTileResolution*
                                     6,
                                     1,0,0,0);
{      aCommandBuffer.CmdDraw(Planet.fTileMapResolution*
                              Planet.fTileMapResolution*
                              Planet.fPhysicsTileResolution*
                              Planet.fPhysicsTileResolution,
                              1,0,0);}
      end;
     end;

    end;

   end;

  end;    
   
 finally
  TpvScene3D(fScene3D).Planets.Lock.Release;
 end;

end;

{ TpvScene3DPlanet }

constructor TpvScene3DPlanet.Create(const aScene3D:TObject;
                                    const aHeightMapResolution:TpvInt32;
                                    const aVisualResolution:TpvSizeInt;
                                    const aPhysicsResolution:TpvSizeInt;
                                    const aBottomRadius:TpvFloat;
                                    const aTopRadius:TpvFloat);
var InFlightFrameIndex,Index,Resolution:TpvSizeInt;
begin

 inherited Create;

 fScene3D:=aScene3D;

 fVulkanDevice:=TpvScene3D(fScene3D).VulkanDevice;

 fHeightMapResolution:=RoundUpToPowerOfTwo(Min(Max(aHeightMapResolution,128),8192));

 fTileMapResolution:=Min(Max(fHeightMapResolution shr 8,32),fHeightMapResolution);

 fTileMapShift:=IntLog2(fHeightMapResolution)-IntLog2(fTileMapResolution);

 fVisualTileResolution:=Max(1,RoundUpToPowerOfTwo(Min(aVisualResolution,fHeightMapResolution)) div fTileMapResolution);

 fPhysicsTileResolution:=Max(1,RoundUpToPowerOfTwo(Min(aPhysicsResolution,fHeightMapResolution)) div fTileMapResolution);

 fVisualResolution:=fTileMapResolution*fVisualTileResolution;

 fPhysicsResolution:=fTileMapResolution*fPhysicsTileResolution;

 fCountVisualMeshIndices:=0;
 fCountVisualMeshLODLevels:=Max(1,IntLog2(fVisualTileResolution));
 fVisualMeshLODOffsets:=nil;
 fVisualMeshLODCounts:=nil;
 SetLength(fVisualMeshLODOffsets,fCountVisualMeshLODLevels);
 SetLength(fVisualMeshLODCounts,fCountVisualMeshLODLevels);
 for Index:=0 to fCountVisualMeshLODLevels-1 do begin
  Resolution:=fVisualTileResolution shr Index;
  fVisualMeshLODOffsets[Index]:=fCountVisualMeshIndices*((fTileMapResolution*fTileMapResolution)*6);
  fVisualMeshLODCounts[Index]:=(Resolution*Resolution)*((fTileMapResolution*fTileMapResolution)*6);
  inc(fCountVisualMeshIndices,Resolution*Resolution);
 end;
 fCountVisualMeshIndices:=fCountVisualMeshIndices*((fTileMapResolution*fTileMapResolution)*6);

 fCountPhysicsMeshIndices:=0;
 fCountPhysicsMeshLODLevels:=Max(1,IntLog2(fPhysicsTileResolution));
 fPhysicsMeshLODOffsets:=nil;
 fPhysicsMeshLODCounts:=nil;
 SetLength(fPhysicsMeshLODOffsets,fCountPhysicsMeshLODLevels);
 SetLength(fPhysicsMeshLODCounts,fCountPhysicsMeshLODLevels);
 for Index:=0 to fCountPhysicsMeshLODLevels-1 do begin
  Resolution:=fPhysicsTileResolution shr Index;
  fPhysicsMeshLODOffsets[Index]:=fCountPhysicsMeshIndices*((fTileMapResolution*fTileMapResolution)*6);
  fPhysicsMeshLODCounts[Index]:=(Resolution*Resolution)*((fTileMapResolution*fTileMapResolution)*6);
  inc(fCountPhysicsMeshIndices,Resolution*Resolution);
 end;
 fCountPhysicsMeshIndices:=fCountPhysicsMeshIndices*((fTileMapResolution*fTileMapResolution)*6);

 fBottomRadius:=aBottomRadius;

 fTopRadius:=aTopRadius;

 fHeightMapScale:=fTopRadius-fBottomRadius;

 if assigned(fVulkanDevice) then begin

  if fVulkanDevice.UniversalQueueFamilyIndex<>fVulkanDevice.ComputeQueueFamilyIndex then begin
   fGlobalBufferSharingMode:=TVkSharingMode(VK_SHARING_MODE_CONCURRENT);
   fGlobalBufferQueueFamilyIndices:=[fVulkanDevice.UniversalQueueFamilyIndex,fVulkanDevice.ComputeQueueFamilyIndex];
  end else begin
   fGlobalBufferSharingMode:=TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE);
   fGlobalBufferQueueFamilyIndices:=nil;
  end;

  if (fVulkanDevice.UniversalQueueFamilyIndex<>fVulkanDevice.ComputeQueueFamilyIndex) and
     fVulkanDevice.PhysicalDevice.RenderDocDetected then begin
   fInFlightFrameSharingMode:=TVkSharingMode(VK_SHARING_MODE_CONCURRENT);
   fInFlightFrameQueueFamilyIndices:=[fVulkanDevice.UniversalQueueFamilyIndex,fVulkanDevice.ComputeQueueFamilyIndex];
  end else begin
   fInFlightFrameSharingMode:=TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE);
   fInFlightFrameQueueFamilyIndices:=nil;
  end;

  fVulkanMemoryStagingQueue:=TpvVulkanDeviceMemoryStagingQueue.Create;

  fVulkanComputeQueue:=fVulkanDevice.ComputeQueue;

  fVulkanComputeCommandPool:=TpvVulkanCommandPool.Create(fVulkanDevice,
                                                  fVulkanDevice.ComputeQueueFamilyIndex,
                                                  TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

  fVulkanComputeCommandBuffer:=TpvVulkanCommandBuffer.Create(fVulkanComputeCommandPool);

  fVulkanComputeFence:=TpvVulkanFence.Create(fVulkanDevice);

  fVulkanUniversalQueue:=fVulkanDevice.UniversalQueue;

  fVulkanUniversalCommandPool:=TpvVulkanCommandPool.Create(fVulkanDevice,
                                                           fVulkanDevice.UniversalQueueFamilyIndex,
                                                           TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

  fVulkanUniversalCommandBuffer:=TpvVulkanCommandBuffer.Create(fVulkanUniversalCommandPool);

  fVulkanUniversalFence:=TpvVulkanFence.Create(fVulkanDevice);

  fVulkanUniversalAcquireReleaseCommandPool:=TpvVulkanCommandPool.Create(fVulkanDevice,
                                                                         fVulkanDevice.UniversalQueueFamilyIndex,
                                                                         TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));     

  for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
   fVulkanUniversalAcquireCommandBuffers[InFlightFrameIndex]:=TpvVulkanCommandBuffer.Create(fVulkanUniversalAcquireReleaseCommandPool);
   fVulkanUniversalReleaseCommandBuffers[InFlightFrameIndex]:=TpvVulkanCommandBuffer.Create(fVulkanUniversalAcquireReleaseCommandPool);
   fVulkanUniversalAcquireSemaphores[InFlightFrameIndex]:=TpvVulkanSemaphore.Create(fVulkanDevice);
   fVulkanUniversalReleaseSemaphores[InFlightFrameIndex]:=TpvVulkanSemaphore.Create(fVulkanDevice);
  end;

 end;

 fData:=TData.Create(self,-1);

 fInFlightFrameDataList:=TInFlightFrameDataList.Create(true);
 for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
  fInFlightFrameDataList.Add(TData.Create(self,InFlightFrameIndex));
 end;

 fReleaseFrameCounter:=-1;

 for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
  fInFlightFrameReady[InFlightFrameIndex]:=false;
 end; 
 
 fHeightMapRandomInitialization:=THeightMapRandomInitialization.Create(self);

 fHeightMapModification:=THeightMapModification.Create(self);

 fHeightMapFlatten:=THeightMapFlatten.Create(self);

 fTiledMeshBoundingVolumesGeneration:=TTiledMeshBoundingVolumesGeneration.Create(self);

 fTileDirtyExpansion:=TTileDirtyExpansion.Create(self);

 fTileDirtyQueueGeneration:=TTileDirtyQueueGeneration.Create(self);

 fNormalMapGeneration:=TNormalMapGeneration.Create(self);

 fHeightMapMipMapGeneration:=THeightMapMipMapGeneration.Create(self);

 fNormalMapMipMapGeneration:=TNormalMapMipMapGeneration.Create(self);

 fVisualMeshIndexGeneration:=TMeshIndexGeneration.Create(self,false);

 fPhysicsMeshIndexGeneration:=TMeshIndexGeneration.Create(self,true);

 fVisualMeshVertexGeneration:=TMeshVertexGeneration.Create(self,false);

 fPhysicsMeshVertexGeneration:=TMeshVertexGeneration.Create(self,true);

{fVisualBaseMeshVertexGeneration:=TBaseMeshVertexGeneration.Create(self,false);

 fVisualBaseMeshIndexGeneration:=TBaseMeshIndexGeneration.Create(self,false);

 fVisualMeshVertexGeneration:=TMeshVertexGeneration.Create(self,false);

 fPhysicsBaseMeshVertexGeneration:=TBaseMeshVertexGeneration.Create(self,true);

 fPhysicsBaseMeshIndexGeneration:=TBaseMeshIndexGeneration.Create(self,true);

 fPhysicsMeshVertexGeneration:=TMeshVertexGeneration.Create(self,true);}

 fRayIntersection:=TRayIntersection.Create(self);

 fCommandBufferLevel:=0;

 fCommandBufferLock:=0;

 fComputeQueueLock:=0;

 fPointerToPlanetData:=@fPlanetData;

 fPointerToMaterials:=@fMaterials;

 if assigned(fVulkanDevice) then begin

  for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
   fPlanetDataVulkanBuffers[InFlightFrameIndex]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                        SizeOf(TpvScene3DPlanet.TPlanetData),
                                                                        TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or
                                                                        TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                                        TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                        [],
                                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                        0,
                                                                        0,
                                                                        0,
                                                                        0,
                                                                        0,
                                                                        0,
                                                                        [TpvVulkanBufferFlag.PersistentMappedIfPossibe]
                                                                       );     
  end; 

  fDescriptorPool:=TpvScene3DPlanet.CreatePlanetDescriptorPool(fVulkanDevice,TpvScene3D(fScene3D).CountInFlightFrames);

  for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
   
   fDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fDescriptorPool,TpvScene3D(fScene3D).PlanetDescriptorSetLayout);
   fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                            0,
                                                            3,
                                                            TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                            [TVkDescriptorImageInfo.Create(TpvScene3D(fScene3D).GeneralComputeSampler.Handle,
                                                                                           fInFlightFrameDataList[InFlightFrameIndex].fHeightMapImage.VulkanImageView.Handle,
                                                                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL),
                                                             TVkDescriptorImageInfo.Create(TpvScene3D(fScene3D).GeneralComputeSampler.Handle,
                                                                                           fInFlightFrameDataList[InFlightFrameIndex].fNormalMapImage.VulkanImageView.Handle,
                                                                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL),
                                                             TVkDescriptorImageInfo.Create(TpvScene3D(fScene3D).GeneralComputeSampler.Handle,
                                                                                           fInFlightFrameDataList[InFlightFrameIndex].fTangentBitangentMapImage.VulkanImageView.Handle,
                                                                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                            [],
                                                            [],
                                                            false);
   fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                            0,
                                                            1,
                                                            TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                            [],
                                                            [fPlanetDataVulkanBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                            [],
                                                            false);
   fDescriptorSets[InFlightFrameIndex].Flush;

  end;

 end else begin
   
  for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
   fPlanetDataVulkanBuffers[InFlightFrameIndex]:=nil;
  end; 

  fDescriptorPool:=nil;

  for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
   fDescriptorSets[InFlightFrameIndex]:=nil;
  end;
 
 end;

 fReady:=true;

end;

destructor TpvScene3DPlanet.Destroy;
var InFlightFrameIndex:TpvSizeInt;
begin
 
 for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
  FreeAndNil(fDescriptorSets[InFlightFrameIndex]);
 end;

 FreeAndNil(fDescriptorPool);

 for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
  FreeAndNil(fPlanetDataVulkanBuffers[InFlightFrameIndex]);
 end;
  
 FreeAndNil(fRayIntersection);

 FreeAndNil(fVulkanMemoryStagingQueue);

{FreeAndNil(fPhysicsMeshVertexGeneration);

 FreeAndNil(fPhysicsBaseMeshIndexGeneration);

 FreeAndNil(fPhysicsBaseMeshVertexGeneration);
 
 FreeAndNil(fVisualMeshVertexGeneration);

 FreeAndNil(fVisualBaseMeshIndexGeneration);

 FreeAndNil(fVisualBaseMeshVertexGeneration);}

 FreeAndNil(fPhysicsMeshVertexGeneration);
 
 FreeAndNil(fVisualMeshVertexGeneration);

 FreeAndNil(fPhysicsMeshIndexGeneration);

 FreeAndNil(fVisualMeshIndexGeneration); 

 FreeAndNil(fNormalMapMipMapGeneration);

 FreeAndNil(fHeightMapMipMapGeneration);
 
 FreeAndNil(fNormalMapGeneration);

 FreeAndNil(fTiledMeshBoundingVolumesGeneration);

 FreeAndNil(fTileDirtyExpansion);

 FreeAndNil(fTileDirtyQueueGeneration);

 FreeAndNil(fHeightMapFlatten); 

 FreeAndNil(fHeightMapModification);

 FreeAndNil(fHeightMapRandomInitialization);

 FreeAndNil(fInFlightFrameDataList);

 fVisualMeshLODOffsets:=nil;

 fVisualMeshLODCounts:=nil;

 fPhysicsMeshLODOffsets:=nil;

 fPhysicsMeshLODCounts:=nil;

 FreeAndNil(fData);

 for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
  FreeAndNil(fVulkanUniversalReleaseSemaphores[InFlightFrameIndex]);
  FreeAndNil(fVulkanUniversalAcquireSemaphores[InFlightFrameIndex]);
  FreeAndNil(fVulkanUniversalReleaseCommandBuffers[InFlightFrameIndex]);
  FreeAndNil(fVulkanUniversalAcquireCommandBuffers[InFlightFrameIndex]);
 end;

 FreeAndNil(fVulkanUniversalAcquireReleaseCommandPool);

 FreeAndNil(fVulkanUniversalFence);

 FreeAndNil(fVulkanUniversalCommandBuffer);

 FreeAndNil(fVulkanUniversalCommandPool);

 FreeAndNil(fVulkanComputeFence);

 FreeAndNil(fVulkanComputeCommandBuffer);

 FreeAndNil(fVulkanComputeCommandPool);

 fGlobalBufferQueueFamilyIndices:=nil;

 fInFlightFrameQueueFamilyIndices:=nil;

 inherited Destroy;

end;

procedure TpvScene3DPlanet.AfterConstruction;
begin
 inherited AfterConstruction;
 if assigned(fScene3D) then begin
  TpvScene3D(fScene3D).Planets.Lock.Acquire;
  try  
   TpvScene3D(fScene3D).Planets.Add(self); 
  finally 
   TpvScene3D(fScene3D).Planets.Lock.Release;
  end; 
 end;
end;

procedure TpvScene3DPlanet.BeforeDestruction;
var Index:TpvSizeInt;
begin
 if assigned(fScene3D) then begin
  TpvScene3D(fScene3D).Planets.Lock.Acquire;
  try  
   Index:=TpvScene3D(fScene3D).Planets.IndexOf(self);
   if Index>=0 then begin
    TpvScene3D(fScene3D).Planets.Extract(Index); // not delete or remove, since we don't want to free ourself here already.
   end;
  finally 
   TpvScene3D(fScene3D).Planets.Lock.Release;
  end; 
 end; 
 inherited BeforeDestruction;
end;

procedure TpvScene3DPlanet.Release;
begin
 if fReleaseFrameCounter<0 then begin
  fReleaseFrameCounter:=TpvScene3D(fScene3D).CountInFlightFrames;
  fReady:=false;
 end;
end;

function TpvScene3DPlanet.HandleRelease:boolean;
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

class function TpvScene3DPlanet.CreatePlanetDescriptorSetLayout(const aVulkanDevice:TpvVulkanDevice):TpvVulkanDescriptorSetLayout;
begin

 result:=TpvVulkanDescriptorSetLayout.Create(aVulkanDevice);

 // Height map + normal map + tangent/bitangent map
 result.AddBinding(0,
                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                   3,
                   TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or
                   TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT) or
                   TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT) or
                   TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                   [],
                   0);

 // Planet data
 result.AddBinding(1,
                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                   1,
                   TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or
                   TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT) or
                   TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT) or
                   TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                   [],
                   0);

 result.Initialize;

 aVulkanDevice.DebugUtils.SetObjectName(result.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,'TpvScene3DPlanet.PlanetDescriptorSetLayout');
 
end;

class function TpvScene3DPlanet.CreatePlanetDescriptorPool(const aVulkanDevice:TpvVulkanDevice;const aCountInFlightFrames:TpvSizeInt):TpvVulkanDescriptorPool;
begin
 result:=TpvVulkanDescriptorPool.Create(aVulkanDevice,
                                        TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                        aCountInFlightFrames);
 result.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),3*aCountInFlightFrames);
 result.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),3*aCountInFlightFrames);
 result.Initialize;
 aVulkanDevice.DebugUtils.SetObjectName(result.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'TpvScene3DPlanet.PlanetDescriptorPool');
end;

procedure TpvScene3DPlanet.BeginUpdate;
begin
 TPasMPMultipleReaderSingleWriterSpinLock.AcquireWrite(fCommandBufferLock);
 try
  if fCommandBufferLevel=0 then begin
   fVulkanComputeCommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));
   fVulkanComputeCommandBuffer.BeginRecording;
  end;
  inc(fCommandBufferLevel);
 finally
  TPasMPMultipleReaderSingleWriterSpinLock.ReleaseWrite(fCommandBufferLock);
 end;
end;

procedure TpvScene3DPlanet.EndUpdate;
begin
 TPasMPMultipleReaderSingleWriterSpinLock.AcquireWrite(fCommandBufferLock);
 try
  if fCommandBufferLevel>0 then begin
   dec(fCommandBufferLevel);
   if fCommandBufferLevel=0 then begin
    fVulkanComputeCommandBuffer.EndRecording;
    fVulkanComputeCommandBuffer.Execute(fVulkanComputeQueue,
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                        nil,
                                        nil,
                                        fVulkanComputeFence,
                                        true);
   end;
  end;
 finally
  TPasMPMultipleReaderSingleWriterSpinLock.ReleaseWrite(fCommandBufferLock);
 end;
end;

procedure TpvScene3DPlanet.FlushUpdate;
begin
 TPasMPMultipleReaderSingleWriterSpinLock.AcquireWrite(fCommandBufferLock);
 try
  if fCommandBufferLevel=1 then begin
   fVulkanComputeCommandBuffer.EndRecording;
   fVulkanComputeCommandBuffer.Execute(fVulkanComputeQueue,
                                       TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                       nil,
                                       nil,
                                       fVulkanComputeFence,
                                       true);
   fVulkanComputeCommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));
   fVulkanComputeCommandBuffer.BeginRecording;
  end;
 finally
  TPasMPMultipleReaderSingleWriterSpinLock.ReleaseWrite(fCommandBufferLock);
 end;
end;

procedure TpvScene3DPlanet.Initialize(const aPasMPInstance:TPasMP);
//var ui32:TVkUInt32;
begin

 if not fData.fInitialized then begin

  if assigned(fVulkanDevice) then begin

   BeginUpdate;
   try

    fHeightMapRandomInitialization.Execute(fVulkanComputeCommandBuffer);

    fVisualMeshIndexGeneration.Execute(fVulkanComputeCommandBuffer);

    fPhysicsMeshIndexGeneration.Execute(fVulkanComputeCommandBuffer);

    fTiledMeshBoundingVolumesGeneration.Execute(fVulkanComputeCommandBuffer);

{   fVisualBaseMeshVertexGeneration.Execute(fVulkanComputeCommandBuffer);

    if fVisualResolution<=512 then begin
     fVisualBaseMeshIndexGeneration.Execute(fVulkanComputeCommandBuffer);
    end;

    fPhysicsBaseMeshVertexGeneration.Execute(fVulkanComputeCommandBuffer);

    if fPhysicsResolution<=512 then begin
     fPhysicsBaseMeshIndexGeneration.Execute(fVulkanComputeCommandBuffer);
    end;}

   finally
    EndUpdate;
   end;

   begin

    fVulkanDevice.MemoryStaging.Download(fVulkanComputeQueue,
                                         fVulkanComputeCommandBuffer,
                                         fVulkanComputeFence,
                                         fData.fPhysicsMeshIndexBuffer,
                                         0,
                                         fData.fMeshIndices.ItemArray[0],
                                         fTileMapResolution*fTileMapResolution*fPhysicsTileResolution*fPhysicsTileResolution*6*SizeOf(TpvUInt32));

    fVulkanDevice.MemoryStaging.Download(fVulkanComputeQueue,
                                         fVulkanComputeCommandBuffer,
                                         fVulkanComputeFence,
                                         fData.fTiledMeshBoundingBoxesBuffer,
                                         0,
                                         fData.fTiledMeshBoundingBoxes.ItemArray[0],
                                         fTileMapResolution*fTileMapResolution*SizeOf(TpvScene3DPlanet.TData.TTiledMeshBoundingBox));

    fVulkanDevice.MemoryStaging.Download(fVulkanComputeQueue,
                                         fVulkanComputeCommandBuffer,
                                         fVulkanComputeFence,
                                         fData.fTiledMeshBoundingSpheresBuffer,
                                         0,
                                         fData.fTiledMeshBoundingSpheres.ItemArray[0],
                                         fTileMapResolution*fTileMapResolution*SizeOf(TpvScene3DPlanet.TData.TTiledMeshBoundingSphere));

   end;

  end;

  fData.fInitialized:=true;

 end;

end;

procedure TpvScene3DPlanet.Flatten(const aVector:TpvVector3;const aInnerRadius,aOuterRadius,aTargetHeight:TpvFloat);
begin

 if assigned(fVulkanDevice) then begin

  BeginUpdate;
  try

   fHeightMapFlatten.fPushConstants.Vector.xyz:=aVector;

   fHeightMapFlatten.fPushConstants.InnerRadius:=aInnerRadius;

   fHeightMapFlatten.fPushConstants.OuterRadius:=aOuterRadius;

   fHeightMapFlatten.fPushConstants.TargetHeight:=aTargetHeight;

   fHeightMapFlatten.Execute(fVulkanComputeCommandBuffer);

  finally
   EndUpdate;
  end;

 end;

end;

function TpvScene3DPlanet.RayIntersection(const aRayOrigin,aRayDirection:TpvVector3;out aHitNormal:TpvVector3;out aHitTime:TpvScalar):boolean;
var Sphere:TpvSphere;
    HitNormalTime:TpvVector4;
begin

 result:=false;

 Sphere:=TpvSphere.Create(fData.fModelMatrix.MulHomogen(TpvVector3.Null),fTopRadius);

 // Pre-ray-intersection-check on the CPU, before we do the actual ray intersection on the GPU for to save unnecessary GPU interactions.aRayOrigin
 if Sphere.RayIntersection(aRayOrigin,aRayDirection,aHitTime) then begin

  if assigned(fVulkanDevice) then begin

   BeginUpdate;
   try

    fRayIntersection.Execute(fVulkanComputeCommandBuffer,aRayOrigin,aRayDirection);

   finally
    EndUpdate;
   end;

   fVulkanDevice.MemoryStaging.Download(fVulkanComputeQueue,
                                        fVulkanComputeCommandBuffer,
                                        fVulkanComputeFence,
                                        fData.fRayIntersectionResultBuffer,
                                        0,
                                        HitNormalTime,
                                        SizeOf(TpvVector4));

   if HitNormalTime.w>=0.0 then begin
    aHitNormal:=HitNormalTime.xyz;
    aHitTime:=HitNormalTime.w;
    result:=true;
   end;                                         

  end;

 end; 

end;

function TpvTypedSortCompareUInt32(const a,b:TpvUInt32):TpvInt32;
begin
 result:=Sign(TpvInt32(a)-TpvInt32(b));
end;

procedure TpvScene3DPlanet.Update(const aInFlightFrameIndex:TpvSizeInt);
var QueueTileIndex:TpvSizeInt;
    TileIndex:TpvUInt32;
    Source:Pointer;
begin

 fData.fCountDirtyTiles:=0;

 if (fData.fHeightMapProcessedGeneration<>fData.fHeightMapGeneration) or
    fData.fModifyHeightMapActive then begin

  if assigned(fVulkanDevice) then begin

   BeginUpdate;
   try

    if fData.fModifyHeightMapActive then begin
     fHeightMapModification.Execute(fVulkanComputeCommandBuffer);
    end;

    if fData.fHeightMapProcessedGeneration<>fData.fHeightMapGeneration then begin

     fData.fHeightMapProcessedGeneration:=fData.fHeightMapGeneration;

     fTileDirtyExpansion.Execute(fVulkanComputeCommandBuffer);

     fTileDirtyQueueGeneration.Execute(fVulkanComputeCommandBuffer);

     if fVulkanDevice.PhysicalDevice.RenderDocDetected then begin

      EndUpdate;
      try

       fVulkanDevice.MemoryStaging.Download(fVulkanComputeQueue,
                                            fVulkanComputeCommandBuffer,
                                            fVulkanComputeFence,
                                            fData.fTileDirtyQueueBuffer,
                                            SizeOf(TVkUInt32),
                                            fData.fCountDirtyTiles,
                                            SizeOf(TVkUInt32));

      finally
       BeginUpdate;
      end;

     end else begin

      fData.fCountDirtyTiles:=0;

     end;

     fHeightMapMipMapGeneration.Execute(fVulkanComputeCommandBuffer);

     fNormalMapGeneration.Execute(fVulkanComputeCommandBuffer);

     fNormalMapMipMapGeneration.Execute(fVulkanComputeCommandBuffer);

     fVisualMeshVertexGeneration.Execute(fVulkanComputeCommandBuffer);

     fPhysicsMeshVertexGeneration.Execute(fVulkanComputeCommandBuffer);

    end;

   finally
    EndUpdate;
   end;

   if not fVulkanDevice.PhysicalDevice.RenderDocDetected then begin
    fVulkanDevice.MemoryStaging.Download(fVulkanComputeQueue,
                                         fVulkanComputeCommandBuffer,
                                         fVulkanComputeFence,
                                         fData.fTileDirtyQueueBuffer,
                                         SizeOf(TVkUInt32),
                                         fData.fCountDirtyTiles,
                                         SizeOf(TVkUInt32));
   end;

   if fData.fCountDirtyTiles>0 then begin

    fVulkanDevice.MemoryStaging.Download(fVulkanComputeQueue,
                                         fVulkanComputeCommandBuffer,
                                         fVulkanComputeFence,
                                         fData.fTileDirtyQueueBuffer,
                                         SizeOf(TVkUInt32)*6,
                                         fData.fTileDirtyQueueItems.ItemArray[0],
                                         fData.fCountDirtyTiles*SizeOf(TVkUInt32));

    if fData.fCountDirtyTiles>0 then begin
     TpvTypedSort<TpvUInt32>.IntroSort(@fData.fTileDirtyQueueItems.ItemArray[0],0,fData.fCountDirtyTiles-1,TpvTypedSortCompareUInt32);
    end;

    for QueueTileIndex:=0 to TpvSizeInt(fData.fCountDirtyTiles)-1 do begin
     TileIndex:=fData.fTileDirtyQueueItems.ItemArray[QueueTileIndex];
     inc(fData.fTileGenerations[TileIndex]);
    end;

    if fData.fCountDirtyTiles=(fTileMapResolution*fTileMapResolution) then begin

     fVulkanDevice.MemoryStaging.Download(fVulkanComputeQueue,
                                          fVulkanComputeCommandBuffer,
                                          fVulkanComputeFence,
                                          fData.fPhysicsMeshVertexBuffer,
                                          0,
                                          fData.fMeshVertices.ItemArray[0],
                                          fTileMapResolution*fTileMapResolution*fPhysicsTileResolution*fPhysicsTileResolution*SizeOf(TMeshVertex));

    end else begin

     if (TpvVulkanBufferFlag.PersistentMapped in fData.fPhysicsMeshVertexBuffer.Flags) and
        ((fData.fPhysicsMeshVertexBuffer.MemoryPropertyFlags and TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT))<>0) then begin

      Source:=fData.fPhysicsMeshVertexBuffer.Memory.MapMemory;
      if assigned(Source) then begin
       try
        fData.fPhysicsMeshVertexBuffer.Flush(Source,0,fData.fPhysicsMeshVertexBuffer.Size);
        for QueueTileIndex:=0 to TpvSizeInt(fData.fCountDirtyTiles)-1 do begin
         TileIndex:=fData.fTileDirtyQueueItems.ItemArray[QueueTileIndex];
         Move(Pointer(TpvPtrUInt(TpvPtrUInt(Source)+TpvPtrUInt(TileIndex*fPhysicsTileResolution*fPhysicsTileResolution*SizeOf(TMeshVertex))))^,
              fData.fMeshVertices.ItemArray[TileIndex*fPhysicsTileResolution*fPhysicsTileResolution],
              fPhysicsTileResolution*fPhysicsTileResolution*SizeOf(TMeshVertex));
        end;
       finally
        fData.fPhysicsMeshVertexBuffer.Memory.UnmapMemory;
       end;
      end else begin
       raise EpvVulkanException.Create('Vulkan buffer memory block map failed');
      end;

     end else begin

      fVulkanMemoryStagingQueue.Clear;
      try
       for QueueTileIndex:=0 to TpvSizeInt(fData.fCountDirtyTiles)-1 do begin
        TileIndex:=fData.fTileDirtyQueueItems.ItemArray[QueueTileIndex];
        fVulkanMemoryStagingQueue.EnqueueDownload(fData.fPhysicsMeshVertexBuffer,
                                                  TileIndex*fPhysicsTileResolution*fPhysicsTileResolution*SizeOf(TMeshVertex),
                                                  fData.fMeshVertices.ItemArray[TileIndex*fPhysicsTileResolution*fPhysicsTileResolution],
                                                  fPhysicsTileResolution*fPhysicsTileResolution*SizeOf(TMeshVertex));
       end;
      finally
       fVulkanDevice.MemoryStaging.ProcessQueue(fVulkanComputeQueue,
                                                fVulkanComputeCommandBuffer,
                                                fVulkanComputeFence,
                                                fVulkanMemoryStagingQueue);
      end;

     end;

    end;

   end;

  end;

 end;

end;

procedure TpvScene3DPlanet.FrameUpdate(const aInFlightFrameIndex:TpvSizeInt);
var InFlightFrameData:TData;
begin

 if assigned(fVulkanDevice) then begin

  if aInFlightFrameIndex>=0 then begin
   InFlightFrameData:=fInFlightFrameDataList[aInFlightFrameIndex];
   InFlightFrameData.Assign(fData);
  end else begin
   InFlightFrameData:=nil;
  end;

  if ((fVulkanDevice.UniversalQueueFamilyIndex<>fVulkanDevice.ComputeQueueFamilyIndex) and
      (fInFlightFrameSharingMode=TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE))) or
//   true or
//   (fData.fVisualMeshGeneration<>fData.fHeightMapGeneration) or
     (assigned(InFlightFrameData) and (InFlightFrameData.fHeightMapGeneration<>fData.fHeightMapGeneration)) then begin

   BeginUpdate;
   try

{   if fData.fVisualMeshGeneration<>fData.fHeightMapGeneration then begin
     fData.fVisualMeshGeneration:=fData.fHeightMapGeneration;
     fVisualMeshVertexGeneration.Execute(fVulkanComputeCommandBuffer);
    end;}

    if assigned(InFlightFrameData) then begin

     InFlightFrameData.AcquireOnComputeQueue(fVulkanComputeCommandBuffer);

     if InFlightFrameData.fHeightMapGeneration<>fData.fHeightMapGeneration then begin
      InFlightFrameData.fHeightMapGeneration:=fData.fHeightMapGeneration;
      fData.TransferTo(fVulkanComputeCommandBuffer,InFlightFrameData);
     end;

     InFlightFrameData.ReleaseOnComputeQueue(fVulkanComputeCommandBuffer);

    end;

   finally
    EndUpdate;
   end;

   fInFlightFrameReady[aInFlightFrameIndex]:=true;

  end;

 end;

end;

procedure TpvScene3DPlanet.UploadFrame(const aInFlightFrameIndex:TpvSizeInt);
var MaterialIndex:TpvSizeInt;
    InFlightFrameData:TData;
    Material:TpvScene3DPlanet.PMaterial;
begin

 if assigned(fVulkanDevice) and (aInFlightFrameIndex>=0) then begin

  InFlightFrameData:=fInFlightFrameDataList[aInFlightFrameIndex];

  if assigned(InFlightFrameData) then begin

   fPlanetData.ModelMatrix:=InFlightFrameData.fModelMatrix;
   fPlanetData.NormalMatrix:=TpvMatrix4x4.Create(InFlightFrameData.fModelMatrix.ToMatrix3x3.Inverse.Transpose);
   fPlanetData.BottomRadius:=fBottomRadius;
   fPlanetData.TopRadius:=fTopRadius;
   fPlanetData.HeightMapScale:=fHeightMapScale;
   fPlanetData.Flags:=0;
   if InFlightFrameData.fWireframeActive then begin
    fPlanetData.Flags:=fPlanetData.Flags or (1 shl 0);
   end;
   if InFlightFrameData.fDisplacementMappingActive then begin
    fPlanetData.Flags:=fPlanetData.Flags or (1 shl 1);
   end;
   if InFlightFrameData.fParallaxMappingActive then begin
    fPlanetData.Flags:=fPlanetData.Flags or (1 shl 2);
   end;
   fPlanetData.Resolutions:=((fTileMapResolution and $ffff) shl 16) or (fVisualTileResolution and $ffff);
   fPlanetData.Selected:=InFlightFrameData.SelectedRegion.Vector;

   for MaterialIndex:=Low(TpvScene3DPlanet.TMaterials) to High(TpvScene3DPlanet.TMaterials) do begin
    Material:=@fMaterials[MaterialIndex];
    fPlanetData.Textures[MaterialIndex,0]:=Material^.AlbedoTexture;
    fPlanetData.Textures[MaterialIndex,1]:=Material^.NormalHeightTexture;
    fPlanetData.Textures[MaterialIndex,2]:=Material^.OcclusionRoughnessMetallicTexture;
    fPlanetData.Textures[MaterialIndex,3]:=TpvUInt32(pointer(@Material^.Scale)^);
   end;

   fVulkanDevice.MemoryStaging.Upload(fVulkanUniversalQueue,
                                      fVulkanUniversalCommandBuffer,
                                      fVulkanUniversalFence,
                                      fPlanetData,
                                      fPlanetDataVulkanBuffers[aInFlightFrameIndex],
                                      0,
                                      SizeOf(TpvScene3DPlanet.TPlanetData));

  end;

 end;

end;

procedure TpvScene3DPlanet.BeginFrame(const aInFlightFrameIndex:TpvSizeInt;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence);
var InFlightFrameData:TData;
    CommandBuffer:TpvVulkanCommandBuffer;
begin

 if assigned(fVulkanDevice) and (aInFlightFrameIndex>=0) then begin

  InFlightFrameData:=fInFlightFrameDataList[aInFlightFrameIndex];

  if assigned(InFlightFrameData) and
     (fVulkanDevice.UniversalQueueFamilyIndex<>fVulkanDevice.ComputeQueueFamilyIndex) and
     (fInFlightFrameSharingMode=TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE)) then begin

   CommandBuffer:=fVulkanUniversalAcquireCommandBuffers[aInFlightFrameIndex];
   
   CommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));

   CommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));

   InFlightFrameData.AcquireOnUniversalQueue(CommandBuffer);

   CommandBuffer.EndRecording;

   CommandBuffer.Execute(fVulkanUniversalQueue,
                         TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                         aWaitSemaphore,
                         fVulkanUniversalAcquireSemaphores[aInFlightFrameIndex],
                         aWaitFence,
                         assigned(aWaitFence));

   aWaitSemaphore:=fVulkanUniversalAcquireSemaphores[aInFlightFrameIndex];

  end;

 end;

end;

procedure TpvScene3DPlanet.EndFrame(const aInFlightFrameIndex:TpvSizeInt;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence);
var InFlightFrameData:TData;
    CommandBuffer:TpvVulkanCommandBuffer;
begin

 if assigned(fVulkanDevice) and (aInFlightFrameIndex>=0) then begin

  InFlightFrameData:=fInFlightFrameDataList[aInFlightFrameIndex];

  if assigned(InFlightFrameData) and
     (fVulkanDevice.UniversalQueueFamilyIndex<>fVulkanDevice.ComputeQueueFamilyIndex) and
     (fInFlightFrameSharingMode=TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE)) then begin

   CommandBuffer:=fVulkanUniversalReleaseCommandBuffers[aInFlightFrameIndex];

   CommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));

   CommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));

   InFlightFrameData.ReleaseOnUniversalQueue(CommandBuffer);

   CommandBuffer.EndRecording;

   CommandBuffer.Execute(fVulkanUniversalQueue,
                         TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                         aWaitSemaphore,
                         fVulkanUniversalReleaseSemaphores[aInFlightFrameIndex],
                         aWaitFence,
                         assigned(aWaitFence));

   aWaitSemaphore:=fVulkanUniversalReleaseSemaphores[aInFlightFrameIndex];

  end;

 end;

end;

procedure TpvScene3DPlanet.ExportPhysicsMeshToOBJ(const aStream:TStream);
 procedure WriteLine(const aString:TpvUTF8String);
 const NewLine:array[0..1] of AnsiChar=#13#10;
 begin
  if length(aString)>0 then begin
   aStream.WriteBuffer(aString[1],length(aString)*SizeOf(AnsiChar));
  end;
  aStream.WriteBuffer(NewLine[0],SizeOf(NewLine));
 end;
var Index:TpvSizeInt;
    Vertex:TpvScene3DPlanet.PMeshVertex;
begin
 WriteLine('# Exported physics mesh from TpvScene3DPlanet');
 WriteLine('o Planet');
 for Index:=0 to fData.fMeshVertices.Count-1 do begin
  Vertex:=@fData.fMeshVertices.ItemArray[Index];  
  WriteLine('v '+ConvertDoubleToString(Vertex^.PositionAbsoluteHeight.x)+' '+ConvertDoubleToString(Vertex^.PositionAbsoluteHeight.y)+' '+ConvertDoubleToString(Vertex^.PositionAbsoluteHeight.z));
  WriteLine('vn '+ConvertDoubleToString(Vertex^.NormalRelativeHeight.x)+' '+ConvertDoubleToString(Vertex^.NormalRelativeHeight.y)+' '+ConvertDoubleToString(Vertex^.NormalRelativeHeight.z));
 end;
 Index:=0;
 while (Index+2)<fData.fMeshIndices.Count do begin
  WriteLine('f '+IntToStr(fData.fMeshIndices.ItemArray[Index+0]+1)+'//'+IntToStr(fData.fMeshIndices.ItemArray[Index+0]+1)+' '+
                 IntToStr(fData.fMeshIndices.ItemArray[Index+1]+1)+'//'+IntToStr(fData.fMeshIndices.ItemArray[Index+1]+1)+' '+
                 IntToStr(fData.fMeshIndices.ItemArray[Index+2]+1)+'//'+IntToStr(fData.fMeshIndices.ItemArray[Index+2]+1));
  inc(Index,3);
 end;
end;

procedure TpvScene3DPlanet.ExportPhysicsMeshToOBJ(const aFileName:TpvUTF8String);
var FileStream:TFileStream;
begin
 FileStream:=TFileStream.Create(aFileName,fmCreate);
 try
  ExportPhysicsMeshToOBJ(FileStream);
 finally
  FreeAndNil(FileStream);
 end;
end;

{ TpvScene3DPlanets }

constructor TpvScene3DPlanets.Create(const aScene3D:TObject);
begin
 inherited Create(true);
 fScene3D:=aScene3D;
 fLock:=TPasMPCriticalSection.Create;
end;

destructor TpvScene3DPlanets.Destroy;
begin
 FreeAndNil(fLock);
 inherited Destroy;
end; 

end.

